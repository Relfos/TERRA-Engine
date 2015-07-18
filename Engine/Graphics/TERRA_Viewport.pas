{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_Viewport
 * Implements a viewport class
 ***********************************************************************************************************************
}
Unit TERRA_Viewport;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Camera, TERRA_Renderer, TERRA_Resource,
  TERRA_Ray, TERRA_Vector3D, TERRA_Matrix4x4, TERRA_Color, TERRA_Texture
  {$IFDEF POSTPROCESSING},TERRA_ScreenFX{$ENDIF};

Const
  vpPositionX   = 1;
  vpPositionY   = 2;

Type
  TERRAViewport = Class;

  RenderTargetSampler = Class(TERRAObject)
    Protected
      _Name:TERRAString;

	    _Targets:Array Of RenderTargetInterface;
      _Textures:Array Of TERRATexture;
      _TargetCount:Integer;

      _ResultIndex:Integer;

    	Procedure Init(Width, Height:Integer; PixelSize:PixelSizeType); Virtual; Abstract;

    	// Free memory
	    Procedure Clear();

    Public
	    Constructor Create(Const Name:TERRAString; Width, Height:Integer; PixelSize:PixelSizeType);

	    Procedure Release; Override;

	    Procedure Update(View:TERRAViewport; Source:TERRATexture; DownsamplerShader:ShaderInterface; First, Count:Integer); Virtual; Abstract;

	    // Number of render texture used
      Property TextureCount:Integer Read _TargetCount;

      // Get a downsampled render texture
      Function GetRenderTexture(Index:Integer):TERRATexture;

      Function GetResult():TERRATexture;
  End;


  TERRAViewport = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Visible:Boolean;

      _OfsX:Integer;
      _OfsY:Integer;

      _Width:Integer;
      _Height:Integer;
      _Scale:Single;

      _Camera:Camera;

      _BackgroundColor:Color;

      _Target:TERRAViewport;
      _TargetX1:Single;
      _TargetX2:Single;
      _TargetY1:Single;
      _TargetY2:Single;

      _ViewX:Integer;
      _ViewY:Integer;
      _ViewWidth:Integer;
      _ViewHeight:Integer;

      _ResolveBuffer:RenderTargetInterface;
      _ResolveTexture:TERRATexture;

      _RenderBuffers:Array[0..Pred(TotalCaptureTargets)] Of RenderTargetInterface;
      _RenderTextures:Array[0..Pred(TotalCaptureTargets)] Of TERRATexture;
      _RenderSamplers:Array[0..Pred(TotalCaptureTargets)] Of RenderTargetSampler;

      _VR:Boolean;
      _CurrentSubView:Integer;

      {$IFDEF POSTPROCESSING}
      _FXChain:ScreenFXChain;
      {$ENDIF}

      Function UnprojectVector(WX,WY,WZ:Single):Vector3D;

      {$IFDEF POSTPROCESSING}
      Function GetFXChain: ScreenFXChain;

      Procedure UpdateEffectTargets();
      {$ENDIF}

      Function GetResolveTexture: TERRATexture;

    Public
      AutoResolve:Boolean;

      Constructor Create(Name:TERRAString; Width,Height:Integer; Scale:Single = 1.0);

      Procedure Release; Override;

      Procedure Bind(SubView:Integer);
      Procedure Clear();
      Procedure Restore(Clear:Boolean);
      Procedure Resize(Width, Height:Integer);

      Procedure OnContextLost;

      Procedure SetRenderTargetState(TargetType:RenderTargetType; Enabled:Boolean);
      Function IsRenderTargetEnabled(TargetType:RenderTargetType):Boolean;
      Function IsDirectDrawing():Boolean;

      Function GetRenderTarget(TargetType:RenderTargetType):RenderTargetInterface;
      Function GetRenderTexture(TargetType:RenderTargetType):TERRATexture;

      Function ResolveToTexture():TERRATexture;

      Procedure SetViewArea(X,Y,Width,Height:Integer);

      Procedure SetPostProcessingState(Enabled:Boolean);

      Procedure SetTarget(Target:TERRAViewport; X1,Y1,X2,Y2:Single);
      Procedure SetTargetInPixels(Target:TERRAViewport; X1,Y1,X2,Y2:Integer);

      Function ProjectPoint(Pos:Vector3D):Vector3D;

      Function GetPickRay(TX,TY:Integer):Ray;

      Procedure SetBackgroundColor(BG:Color);

      Procedure EnableDefaultTargets();

      Function HasPostProcessing():Boolean;

      Procedure DrawToTarget(ProcessEffects:Boolean);

      Property BackgroundColor:Color Read _BackgroundColor Write SetBackgroundColor;

      Property Camera:TERRA_Camera.Camera Read _Camera;

      Property Visible:Boolean Read _Visible Write _Visible;

      {$IFDEF POSTPROCESSING}
      Property FXChain:ScreenFXChain Read GetFXChain;
      {$ENDIF}

      Property Name:TERRAString Read _Name Write _Name;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;


      Property OffsetX:Integer Read _OfsX Write _OfsX;
      Property OffsetY:Integer Read _OfsY Write _OfsY;

      Property Target:TERRAViewport Read _Target;
      Property TargetX1:Single Read _TargetX1;
      Property TargetX2:Single Read _TargetX2;
      Property TargetY1:Single Read _TargetY1;
      Property TargetY2:Single Read _TargetY2;

      Property VR:Boolean Read _VR Write _VR;

      Property ResolveTexture:TERRATexture Read GetResolveTexture;
  End;

Implementation
Uses TERRA_Error, TERRA_GraphicsManager, TERRA_Application, TERRA_Log, TERRA_OS, TERRA_Vector4D, TERRA_Downsampler;

{$IFDEF POSTPROCESSING}
Var
  _BlurShader:ShaderInterface;
  _EdgeShader:ShaderInterface;
  _DistanceFieldShader:ShaderInterface;
{$ENDIF}

Function GetShader_Blur():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec2 terra_UV0;');
	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');
	Line('void main()	{');
  Line('lowp vec2 st = texCoord.st;');

  // Apply 3x3 gaussian filter
	Line('lowp vec4 color	= texture2D(texture, st);');
  Line('lowp float alpha = color.a;');
  Line('color *= 4.0;');
  Line('lowp vec4 temp = texture2D(texture, st + vec2(+dx, 0.0));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, 0.0));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(0.0, +dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(0.0, -dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(+dx, +dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, +dy));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, -dy));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(+dx, -dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');

  Line('color = color / 16.0;');
  Line('color.a = alpha;');
  Line('gl_FragColor = color;');
  Line('}}');
  Result := S;
End;


(*	Line('float sobelFilter(vec2 texCoord){');
	Line('	vec3 s00 = texture2D(normal_texture, texCoord + vec2(-offX, -offY)).rgb;');
	Line('	vec3 s01 = texture2D(normal_texture, texCoord + vec2( 0,   -offY)).rgb;');
	Line('	vec3 s02 = texture2D(normal_texture, texCoord + vec2( offX, -offY)).rgb;');

	Line('	vec3 s10 = texture2D(normal_texture, texCoord + vec2(-offX,  0)).rgb;');
	Line('	vec3 s12 = texture2D(normal_texture, texCoord + vec2( offX,  0)).rgb;');

	Line('	vec3 s20 = texture2D(normal_texture, texCoord + vec2(-offX,  offY)).rgb;');
	Line('	vec3 s21 = texture2D(normal_texture, texCoord + vec2( 0,    offY)).rgb;');
	Line('	vec3 s22 = texture2D(normal_texture, texCoord + vec2( offX,  offY)).rgb;');

	Line('	vec3 sobelX = s00 + 2.0 * s10 + s20 - s02 - 2.0 * s12 - s22;');
	Line('	vec3 sobelY = s00 + 2.0 * s01 + s02 - s20 - 2.0 * s21 - s22;');

	Line('	vec3 edgeSqr = sobelX * sobelX + sobelY * sobelY;');
  Line('  vec4 px = texture2D(normal_texture, texCoord).rgba;');
	Line('	float p = dot(edgeSqr, edgeSqr);');


//  Line('	return px.a;	}');

	//Line('	return p * 0.3 + 0.7;	}');*)

Function GetShader_Edge():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec2 terra_UV0;');
	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Line('}');
  Line('fragment {');

  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');

	Line('void main()	{');
	Line('lowp float offX = dx;');
  Line('lowp float offY = dy;');

  // Apply sobel filter

  Line('mediump vec3 sample;');
  //Line('lowp float depth = texture2D(normal_texture, texCoord).a;');

  //Line(' depth = (1.0 - depth) * 0.5;');
  Line(' mediump float kox = offX; ');
  Line(' mediump float koy = offY; ');

// fetch the 3x3 neighbourhood and use the RGB vector's length as intensity value
  Line('  lowp float spv0;');
  Line('  lowp float spv1;');
  Line('  lowp float spv2;');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(-kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord + vec2(-kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(-kox, koy)).rgb);');
  Line(' lowp vec3 sample0 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(0.0, -koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(0.0, koy)).rgb);');
  Line(' lowp vec3 sample1 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord + vec2(kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(kox, koy)).rgb);');
  Line(' lowp vec3 sample2 = vec3(spv0, spv1, spv2);');

// calculate the convolution values for all the masks
	// calculate the convolution values for all the masks
  Line('lowp float conv0;');
  Line('lowp float conv1;');
  Line('mediump float dp3;');
  Line(' mediump vec3 gk0_0 = vec3(1.0, 2.0, 1.0); ');
  Line(' mediump vec3 gk0_2 = vec3(-1.0, -2.0, -1.0); ');
  Line(' mediump vec3 gk1_0 = vec3(1.0, 0.0, -1.0); ');
  Line(' mediump vec3 gk1_1 = vec3(2.0, 0.0, -2.0); ');
  Line(' mediump vec3 gk1_2 = vec3(1.0, 0.0, -1.0); ');
  Line(' dp3 =  dot(gk0_0, sample0) +  dot(gk0_2, sample2) ;');
  Line(' conv0 = dp3 * dp3;	');
  Line(' dp3 =  dot(gk1_0, sample0)  +  dot(gk1_1, sample1)  +  dot(gk1_2, sample2) ;');
  Line(' conv1 = dp3 * dp3;	');
  Line(' mediump float pp = sqrt(conv0*conv0+conv1*conv1);');

  Line('	pp = 1.0 - min(1.0, pp);');

//  Line('  pp = pp * 0.3 + 0.7;');
  //Line('	if (pp<0.99) return 0.0;  else return 1.0; 	}');

  Line('gl_FragColor = vec4(pp, pp, pp, 1.0);');

  //Line('gl_FragColor = texture2D(texture, texCoord);');
  Line('}}');
  Result := S;
End;

Function GetShader_DistanceField():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec2 terra_UV0;');
	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');

  Line('highp float minDist;');

  Line('void measurePixel(lowp ofst)	{');
  Line('lowp vec4 pB = texture2D(texture, st + vec2(dx, 0.0));');
  Line('}');

	Line('void main()	{');
  Line('lowp vec2 st = texCoord.st;');
  Line('  minDist = 9999.0;');

	Line('lowp vec4 pA	= texture2D(texture, st);');
  Line('lowp vec4 pB = texture2D(texture, st + vec2(dx, 0.0));');
  Line('lowp vec4 color = (pA + pB) * 0.5;');
  Line('gl_FragColor = color;');
  Line('}}');
  Result := S;
End;

{ RenderTargetSampler }
Constructor RenderTargetSampler.Create(Const Name:TERRAString; Width, Height:Integer; PixelSize:PixelSizeType);
Begin
  Self._Name := Name;
  Self.Init(Width, Height, PixelSize); // {$IFDEF FRAMEBUFFEROBJECTS}FBO_COLOR8{$ELSE}0{$ENDIF}); BIBI
End;

Function RenderTargetSampler.GetRenderTexture(Index:Integer):TERRATexture;
Begin
  If (Index<0) Or (Index>=_TargetCount) Then
    Result := Nil
  Else
  Begin
    If (_Textures[Index] = Nil) Then
    Begin
      _Textures[Index] := TERRATexture.Create(rtDynamic, Self._Name + '_rt'+IntToString(Index));
      _Textures[Index].InitFromSurface(_Targets[Index]);
      _Textures[Index].WrapMode := wrapNothing;
    End;

    Result := _Textures[Index];
  End;
End;

Procedure RenderTargetSampler.Release();
Begin
  Self.Clear();
End;


Procedure RenderTargetSampler.Clear();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TargetCount) Do
  Begin
    ReleaseObject(_Textures[I]);
    ReleaseObject(_Targets[I]);
  End;

  _TargetCount := 0;
  SetLength(_Targets, 0);
  SetLength(_Textures, 0);
End;

Function RenderTargetSampler.GetResult: TERRATexture;
Begin
  Result := Self.GetRenderTexture(_ResultIndex);
End;


{ TERRAViewport }
Constructor TERRAViewport.Create(Name:TERRAString; Width,Height:Integer; Scale:Single);
Begin
  _Name := Name;
  _Visible := True;

  _Width := Width;
  _Height := Height;
  _Scale := Scale;

  _OfsX := 0;
  _OfsY := 0;

  _CurrentSubView := 0;

  _BackgroundColor := ColorCreate(0, 0, 0, 255);

  _Camera := TERRA_Camera.Camera.Create(Name);

  Log(logDebug, 'Viewport', 'Created viewport '+Name+' with size '+IntToString(_Width) +' x '+IntToString(_Height)+' and scale = '+FloatToString(_Scale));
End;


Procedure TERRAViewport.Resize(Width, Height:Integer);
Begin
  Self._Width := Width;
  Self._Height := Height;

  Self.OnContextLost();

  If Assigned(Camera) Then
    Camera.Refresh();
End;

Procedure TERRAViewport.Restore(Clear:Boolean);
Begin
  Self.SetViewArea(_OfsX, _OfsY, _Width, _Height);

  If (Clear) Then
    Self.Clear();
End;

Procedure TERRAViewport.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(TotalCaptureTargets) Do
  Begin
    If I<RenderCaptureTargets Then
      ReleaseObject(_RenderTextures[I]);

    ReleaseObject(_RenderBuffers[I]);
    ReleaseObject(_RenderSamplers[I]);
  End;


  ReleaseObject(_ResolveBuffer);
  ReleaseObject(_ResolveTexture);

  ReleaseObject(_Camera);

  {$IFDEF POSTPROCESSING}
  ReleaseObject(_FXChain);
  {$ENDIF}
End;

Function TERRAViewport.ProjectPoint(Pos:Vector3D):Vector3D;
Var
  modelview:Matrix4x4;
  temp:Array[0..7] Of Single;
  Proj:Matrix4x4;
Begin
  If _Camera = Nil Then
  Begin
    Result := Pos;
    Exit;
  End;

  modelview := Camera.Transform;
  // Modelview transform
  Temp[0] := modelview.V[0] *Pos.x + modelview.V[4] * Pos.y + modelview.V[8] * Pos.z + modelview.V[12];  //w is always 1
  Temp[1] := modelview.V[1] *Pos.x + modelview.V[5] * Pos.y + modelview.V[9] * Pos.z + modelview.V[13];
  Temp[2] := modelview.V[2] *Pos.x + modelview.V[6] * Pos.y + modelview.V[10]* Pos.z + modelview.V[14];
  Temp[3] := modelview.V[3] *Pos.x + modelview.V[7] * Pos.y + modelview.V[11]* Pos.z + modelview.V[15];

  // Projection transform, the final row of projection matrix is always [0 0 -1 0] so we optimize for that.
  Proj := Camera.Projection;
  Temp[4] := Proj.V[0] * Temp[0] + Proj.V[4] * Temp[1] + Proj.V[8] * Temp[2] + Proj.V[12] * Temp[3];
  Temp[5] := Proj.V[1] * Temp[0] + Proj.V[5] * Temp[1] + Proj.V[9] * Temp[2] + Proj.V[13] * Temp[3];
  Temp[6] := Proj.V[2] * Temp[0] + Proj.V[6] * Temp[1] + Proj.V[10]* Temp[2] + Proj.V[14] * Temp[3];
  Temp[7] := -Temp[2];

  //The result normalizes between -1 and 1
  If (Temp[7]=0.0)	Then //The w value
  Begin
    Result := Pos;
    Exit;
  End;

  Temp[7] := 1.0 / Temp[7];
  If Not Camera.Ortho Then
  Begin  //Perspective division
    Temp[4] := Temp[4] * Temp[7];
    Temp[5] := Temp[5] * Temp[7];
    Temp[6] := Temp[6] * Temp[7];
  End;
  //Window coordinates
  //Map x, y to range 0-1
  Temp[4] := (Temp[4] *0.5 + 0.5);
  Temp[5] := (Temp[5] *0.5 + 0.5);

  //This is only correct when glDepthRange(0.0, 1.0)
  Result.Z := (1.0 + Temp[6]) * 0.5;	//Between 0 and 1

  Result.X := Temp[4] * _Width;
  Result.Y := Temp[5] * _Height;
  Result.Y := _Height - Result.Y;
End;


Function TERRAViewport.UnprojectVector(WX,WY,WZ:Single):Vector3D;
Var
  M:Matrix4x4;
  P:Vector4D;
Begin
  If _Camera = Nil Then
  Begin
    Result := VectorCreate(WX, WY, WZ);
    Exit;
  End;

  //Calculation for inverting a matrix, compute projection x modelview and store in A
  M := Matrix4x4Multiply4x4(Camera.Projection, Camera.Transform);
  //Now compute the inverse of matrix A
  M := Matrix4x4Inverse(M);

  //Transformation of normalized coordinates between -1 and 1
  P.X := ((WX/_Width) *2.0) - 1.0;
  P.Y := ((WY/_Height)*2.0) - 1.0;
  P.Z := (2.0 * WZ) - 1.0;
  P.W := 1.0;

  //Objects coordinates
  P.Transform(M);
  Result := VectorCreate(P.X/P.W, P.Y/P.W, P.Z/P.W);
End;

Function TERRAViewport.GetPickRay(TX, TY:Integer):Ray;
Var
  N,F:Vector3D;
  Px, Py:Single;
Begin
  TX := Trunc(TX);
  TY := Trunc(TY);

  {If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
  Begin
    TY := _Width - TY;
    TX := _Height - TX;

  	PY := TX * (GraphicsManager.Instance.Width/_Width);
	  PX := TY * (GraphicsManager.Instance.Height/_Height);
  End Else}
  Begin
    TY := _Height - TY;
  	Px := TX * (GraphicsManager.Instance.Width/_Width);
	  Py := TY * (GraphicsManager.Instance.Height/_Height);
  End;

  Px := TX;
  Py := TY;
  N := UnprojectVector(px, py, 0.0);
	F := UnprojectVector(px, py, 1.0);

  Result.Direction := VectorSubtract(F, N);
  Result.Direction.Normalize;
  Result.Origin := N;
End;


Procedure TERRAViewport.Bind(Subview:Integer);
Begin
	//glMatrixMode(GL_PROJECTION);
	//glLoadMatrixf(@_ProjectionMatrix);

  _CurrentSubView := SubView;

  If (Assigned(Camera)) Then
    Camera.Update(_Width, _Height, SubView);
End;


Procedure TERRAViewport.SetRenderTargetState(TargetType:RenderTargetType; Enabled: Boolean);
Var
  TargetValue:Integer;
  Graphics:GraphicsManager;
  DownSize:Integer;
Begin
  TargetValue := Integer(TargetType);

  If (TargetValue < 0) Or (TargetValue >= TotalCaptureTargets) Then
    Exit;

  Graphics := GraphicsManager.Instance;

  {If (TargetType = captureTargetEmission) And (Enabled) And (Not Graphics.Renderer.Features.FrameBufferObject.Avaliable) Then
    Enabled := False;}

  If (Self.IsRenderTargetEnabled(TargetType) =  Enabled) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetTargetType');{$ENDIF}

  If Enabled Then
  Begin
    Log(logDebug, 'GraphicsManager', 'Initializing '+IntToString(TargetValue)+' target for '+Self.Name);

    If (TargetValue<RenderCaptureTargets) Then
    Begin
      _RenderBuffers[TargetValue] := Graphics.Renderer.CreateRenderTarget();
      _RenderBuffers[TargetValue].Generate(Trunc(_Width * _Scale), Trunc(_Height * _Scale), False, pixelSizeByte, 1, True, True);
    End Else
    Begin

      If TargetType = effectTargetEdge Then
      Begin
        DownSize := 1024;
        _RenderSamplers[TargetValue] := RenderTargetBouncer.Create(_Name+'_edge', DownSize, DownSize, pixelSizeByte);
      End Else
      Begin
        DownSize := 512;
        _RenderSamplers[TargetValue] := RenderTargetDownSampler.Create(_Name+'_bloom', DownSize, DownSize, pixelSizeByte);
      End;

    End;

  End Else
  Begin
    Log(logDebug, 'GraphicsManager', 'Destroying '+IntToString(TargetValue)+' target for '+Self.Name);
    If TargetValue<RenderCaptureTargets Then
      ReleaseObject(_RenderTextures[TargetValue]);

    ReleaseObject(_RenderBuffers[TargetValue]);
    ReleaseObject(_RenderSamplers[TargetValue]);
  End;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Function TERRAViewport.IsRenderTargetEnabled(TargetType:RenderTargetType):Boolean;
Var
  TargetValue:Integer;
Begin
  TargetValue := Integer(TargetType);

  If (TargetValue<0) Or (TargetValue >= TotalCaptureTargets) Then
    Result := False
  Else
  If (TargetValue<RenderCaptureTargets) Then
    Result := Assigned(_RenderBuffers[TargetValue])
  Else
    Result := Assigned(_RenderSamplers[TargetValue]);
End;

Function TERRAViewport.GetRenderTarget(TargetType:RenderTargetType):RenderTargetInterface;
Var
  TargetValue:Integer;
Begin
  TargetValue := Integer(TargetType);

  If (TargetValue < 0) Or (TargetValue >= TotalCaptureTargets) Then
    Result := Nil
  Else
  Begin
    Result := _RenderBuffers[TargetValue];

    If (Assigned(Result)) And (Not Result.IsValid()) Then
    Begin
      Self.OnContextLost();
      Result := Nil;
    End;
  End;
End;

Function TERRAViewport.GetRenderTexture(TargetType:RenderTargetType):TERRATexture;
Var
  TargetValue:Integer;
Begin
  TargetValue := Integer(TargetType);

  If (TargetValue < 0) Or (TargetValue >= TotalCaptureTargets) Then
    Result := Nil
  Else
  Begin
    If (Not Self.IsRenderTargetEnabled(TargetType)) Then
      Self.SetRenderTargetState(TargetType, True);

    If _RenderTextures[TargetValue] = Nil Then
    Begin
      _RenderTextures[TargetValue] := TERRATexture.Create(rtDynamic, _Name+'_rt'+IntToString(TargetValue));
      _RenderTextures[TargetValue].InitFromSurface(Self.GetRenderTarget(TargetType));
    End;

    Result := _RenderTextures[TargetValue];
  End;
End;

Function TERRAViewport.ResolveToTexture():TERRATexture;
Var
  TempTarget:TERRAViewport;
Begin
  If (Not GraphicsManager.Instance.Renderer.Settings.PostProcessing.Enabled) Then
  Begin
    Result := Self.GetResolveTexture();
    Exit;
  End;

  If (_ResolveBuffer = Nil) Then
  Begin
    _ResolveBuffer := GraphicsManager.Instance.Renderer.CreateRenderTarget();
    _ResolveBuffer.Generate(_Width, _Height, False, pixelSizeByte, 1, False, False);
  End;

  If _ResolveTexture = Nil Then
  Begin
    _ResolveTexture := TERRATexture.Create(rtDynamic, _Name+'_resolve');
    _ResolveTexture.InitFromSurface(_ResolveBuffer);
  End;

  Self._Target := Self;
  Self._TargetX1 := 0;
  Self._TargetY1 := 0;
  Self._TargetX2 := 1.0;
  Self._TargetY2 := 1.0;


//  GraphicsManager.Instance.ShowDebugTarget := captureTargetColor;

  {$IFDEF POSTPROCESSING}
  If (Self.HasPostProcessing) Then
    UpdateEffectTargets();
	{$ENDIF}

  _ResolveBuffer.BackgroundColor := ColorNull;
  Self.SetViewArea(0, 0, _ResolveBuffer.Width, _ResolveBuffer.Height);
  _ResolveBuffer.BeginCapture();
  Self.DrawToTarget(False);
  _ResolveBuffer.EndCapture();

  GraphicsManager.Instance.ShowDebugTarget := captureTargetInvalid;

  Self._Target := Nil;

  Result := _ResolveTexture;
End;

Procedure TERRAViewport.DrawToTarget(ProcessEffects:Boolean);
Var
  MyShader:ShaderInterface;
  I:Integer;
  ShowID:RenderTargetType;
  TempColor:Color;
Begin
  If (Target = Nil) Then
  Begin
    If AutoResolve Then
    Begin
      Self.ResolveToTexture();
    End;

    Exit;
  End;

  {$IFDEF POSTPROCESSING}
  {$IFDEF FRAMEBUFFEROBJECTS}
  If (ProcessEffects) And (Self.HasPostProcessing) Then
    UpdateEffectTargets();
  {$ENDIF}
  {$ENDIF}

  TempColor := Target.BackgroundColor;
  Target.BackgroundColor := Self.BackgroundColor;
  Target.Restore(True);
  Target.BackgroundColor := TempColor;

  {$IFDEF POSTPROCESSING}
  ShowID := GraphicsManager.Instance.ShowDebugTarget;
  If (Integer(ShowID) >= TotalCaptureTargets) Then
    ShowID := captureTargetColor;

  If (Assigned(_FXChain)) And (ShowID = captureTargetInvalid) Then
  Begin
    If (Not GraphicsManager.Instance.Renderer.Settings.PostProcessing.Enabled) Then
    Begin
      For I:=0 To Pred(TotalCaptureTargets) Do
        Self.SetRenderTargetState(RenderTargetType(I), False);

      Self.EnableDefaultTargets();

      ReleaseObject(_FXChain);
    End;

    If (Assigned(_FXChain)) And (ShowID = captureTargetInvalid) Then
    Begin
      _FXChain.DrawScreen(_TargetX1, _TargetY1, _TargetX2, _TargetY2, Self);
      Exit;
    End;
  End;


  If (ShowID = captureTargetInvalid) Then
    ShowID := captureTargetColor;
  {$ELSE}
  ShowID := captureTargetColor;
  {$ENDIF}

  If (GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
  Begin
    MyShader := GraphicsManager.Instance.GetDefaultFullScreenShader();
    GraphicsManager.Instance.Renderer.BindShader(MyShader);
    MyShader.SetIntegerUniform('texture', 0);
    MyShader.SetColorUniform('color', ColorWhite); //BIBI
  End Else
    MyShader := Nil;

  If (_RenderBuffers[Integer(ShowID)] = Nil) Then
    Self.SetRenderTargetState(ShowID, True);

  Self.GetRenderTexture(ShowID).Bind(0);
  //GraphicsManager.Instance.Renderer.BindSurface(_RenderBuffers[Integer(ShowID)], 0);
  //_Buffers[ShowID].BilinearFilter := False;

  GraphicsManager.Instance.Renderer.SetBlendMode(blendNone);

  If (Integer(GraphicsManager.Instance.ShowDebugTarget) <=0) Then
    GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);

  GraphicsManager.Instance.DrawFullscreenQuad(MyShader, _TargetX1, _TargetY1, _TargetX2, _TargetY2);
End;

(*Procedure TERRAViewport.BindStageTexture(Stage:RenderTargetType; Slot:Integer);
Begin
{$IFDEF POSTPROCESSING}
  If Assigned(_Buffers[Integer(Stage)]) Then
    GraphicsManager.Instance.Renderer.BindSurface(_Buffers[Integer(Stage)], Slot)
  Else
{$ENDIF}
  TextureManager.Instance.BlackTexture.Bind(Slot);
End;*)

Procedure TERRAViewport.SetPostProcessingState(Enabled: Boolean);
Begin
  If (Not GraphicsManager.Instance.Renderer.Settings.PostProcessing.Enabled) Then
    Enabled := False;

  {$IFDEF POSTPROCESSING}
  If (Self.HasPostProcessing() = Enabled) Then
    Exit;

  If Enabled Then
    _FXChain := ScreenFXChain.Create()
  Else
    ReleaseObject(_FXChain);
  {$ENDIF}
End;

Function TERRAViewport.IsDirectDrawing: Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(TotalCaptureTargets) Do
  If Assigned(_RenderBuffers[I]) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;
End;

Procedure TERRAViewport.Clear;
Var
  UseScissors:Boolean;
  Flags:Integer;
Begin
  UseScissors := (Trunc(_Width*_Scale)<GraphicsManager.Instance.Width) Or (Trunc(_Height*_Scale)<GraphicsManager.Instance.Height);

  If (UseScissors) Then
  Begin
    GraphicsManager.Instance.Renderer.SetScissorArea(0, 0, Trunc(_Width*_Scale), Trunc(_Height*_Scale));
    GraphicsManager.Instance.Renderer.SetScissorState(True);
  End;

  If Self.IsDirectDrawing()  Then
    GraphicsManager.Instance.Renderer.SetClearColor(_BackgroundColor);

  GraphicsManager.Instance.Renderer.ClearBuffer((Not Self.IsDirectDrawing()) Or (_BackgroundColor.A>=255), True, True);

  If UseScissors Then
    GraphicsManager.Instance.Renderer.SetScissorState(False);
End;

Procedure TERRAViewport.SetBackgroundColor(BG: Color);
Begin
  _BackgroundColor := BG;
End;

Procedure TERRAViewport.EnableDefaultTargets();
Begin
  Self.SetRenderTargetState(captureTargetColor, True);
  {$IFDEF ADVANCED_ALPHA_BLEND}
  Self.SetRenderTargetState(captureTargetAlpha, True);
  {$ENDIF}
End;

Procedure TERRAViewport.SetTarget(Target: TERRAViewport; X1, Y1, X2, Y2: Single);
Begin
  If (Self._Target = Target) Then
    Exit;

  Self._Target := Target;
  Self._TargetX1 := X1;
  Self._TargetX2 := X2;
  Self._TargetY1 := Y1;
  Self._TargetY2 := Y2;
End;

Procedure TERRAViewport.SetTargetInPixels(Target: TERRAViewport; X1, Y1, X2, Y2:Integer);
Begin
  If (Self._Target = Target) Then
    Exit;

  If (Assigned(Target)) Then
    SetTarget(Target, SafeDiv(X1, Target.Width), SafeDiv(Y1, Target.Height), SafeDiv(X2, Target.Width), SafeDiv(Y2, Target.Height))
  Else
    SetTarget(Nil, 0.0, 0.0, 1.0, 1.0);
End;

Procedure TERRAViewport.SetViewArea(X, Y, Width, Height: Integer);
Begin
  _ViewX := X;
  _ViewY := Y;
  _ViewWidth := Width;
  _ViewHeight := Height;

  Y := Trunc(Y * _Scale);
  Height := Trunc(Height * _Scale);

  If _VR Then
  Begin
    X := Trunc(X * _Scale + Width * _Scale * 0.5 * _CurrentSubView);
    Width := Trunc(Width * _Scale * 0.5);
  End Else
  Begin
    X := Trunc(X * _Scale);
    Width := Trunc(Width * _Scale);
  End;

  GraphicsManager.Instance.Renderer.SetViewport(X, Y, Width, Height);
  If (_VR) Then
  Begin
    GraphicsManager.Instance.Renderer.SetScissorState(True);
    GraphicsManager.Instance.Renderer.SetScissorArea(X, Y, Width, Height);
  End Else
    GraphicsManager.Instance.Renderer.SetScissorState(False);
End;


Procedure TERRAViewport.OnContextLost;
Var
  I:Integer;
Begin
  Log(logDebug, 'Viewport', 'Context lost: '+Self.Name);

  For I:=0 To Pred(TotalCaptureTargets) Do
  Begin
    If (Assigned(_RenderBuffers[I])) Or (Assigned(_RenderTextures[I])) Then
    Begin
      Log(logDebug, 'Viewport', 'Reseting '+IntToString(I)+' target for '+Self.Name);
      Self.SetRenderTargetState(RenderTargetType(I), False);
      Self.SetRenderTargetState(RenderTargetType(I), True);
    End;
  End;

  ReleaseObject(_ResolveBuffer);
  ReleaseObject(_ResolveTexture);

  {$IFDEF POSTPROCESSING}
  If Assigned(_FXChain) Then
    _FXChain.OnContextLost();
  {$ENDIF}
End;

Function TERRAViewport.GetResolveTexture: TERRATexture;
Var
  ShowID:RenderTargetType;
Begin
  ShowID := GraphicsManager.Instance.ShowDebugTarget;
  If (ShowID = captureTargetInvalid) Then
  Begin
    If (Not GraphicsManager.Instance.Renderer.Settings.PostProcessing.Enabled) Then
      Result := Self.GetRenderTexture(captureTargetColor)
    Else
      Result := Self._ResolveTexture;

    Self.AutoResolve := True;
  End Else
  Begin
    Self.AutoResolve := False;
    Result := Self.GetRenderTexture(ShowID);
  End;
End;

Function TERRAViewport.HasPostProcessing: Boolean;
Begin
{$IFDEF POSTPROCESSING}
  Result := Assigned(_FXChain);
{$ELSE}
  Result := False;
{$ENDIF}
End;

{$IFDEF POSTPROCESSING}
Function TERRAViewport.GetFXChain: ScreenFXChain;
Begin
  If Not GraphicsManager.Instance.Renderer.Features.PostProcessing.Avaliable Then
  Begin
    Log(logError, 'Viewport', 'Postprocessing not supported in this device!');
    Result := Nil;
    Exit;
  End;

  If _FXChain = Nil Then
    _FXChain := ScreenFXChain.Create();

  Result := _FXChain;
End;

Procedure TERRAViewport.UpdateEffectTargets;
Var
  Sampler:RenderTargetSampler;
  I, Count:Integer;
  SrcType:RenderTargetType;
  Graphics:GraphicsManager;
Begin
  Graphics := GraphicsManager.Instance;

  For I:=RenderCaptureTargets To Pred(TotalCaptureTargets) Do
  Begin
    If (Self._RenderSamplers[I] = Nil) Then
      Continue;

    Sampler := _RenderSamplers[I];

    Case RenderTargetType(I) Of
    effectTargetBloom,
    effectTargetGlow:
      Begin
        Self.SetRenderTargetState(captureTargetEmission, True);

        If (Graphics.Renderer.Features.Shaders.Avaliable) And (Not Assigned(_BlurShader)) Then
        Begin
          _BlurShader := Graphics.Renderer.CreateShader();
          _BlurShader.Generate('blur', GetShader_Blur());
        End;

        If RenderTargetType(I) =  effectTargetBloom Then
        Begin
          SrcType := captureTargetColor;
          Count := 2;
        End Else
        Begin
          SrcType := captureTargetEmission;
          Count := 4;
        End;

        Sampler.Update(Self, Self.GetRenderTexture(SrcType), _BlurShader, 0, Count);
      End;

    effectTargetEdge:
      Begin
        Self.SetRenderTargetState(captureTargetNormal, True);

        If (Graphics.Renderer.Features.Shaders.Avaliable) And (Not Assigned(_EdgeShader)) Then
        Begin
          _EdgeShader := Graphics.Renderer.CreateShader();
          _EdgeShader.Generate('edge', GetShader_Edge());
        End;

        (*If (Graphics.Renderer.Features.Shaders.Avaliable) And (_DoPostProcessing) And (Not Assigned(_DistanceFieldShader)) Then
        Begin
          _DistanceFieldShader := Graphics.Renderer.CreateShader();
          _DistanceFieldShader.Generate('DistanceField', GetShader_DistanceField());
        End;*)

        Sampler.Update(Self, Self.GetRenderTexture(captureTargetNormal), _EdgeShader, 0, 1);
        //Sampler.Update(Sampler.GetResult(), _DistanceFieldShader, 0, 256);
      End;

    End;

    _RenderTextures[I] := Sampler.GetResult();
  End;
End;

{$ENDIF}


End.
