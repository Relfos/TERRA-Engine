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
  TERRA_Utils, TERRA_Camera, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF},
  TERRA_String, TERRA_Ray, TERRA_Vector3D, TERRA_Matrix4x4, 
  TERRA_Color, TERRA_RenderTarget, TERRA_Downsampler, TERRA_Shader
{$IFDEF POSTPROCESSING},TERRA_ScreenFX{$ENDIF};

Const
  vpPositionX   = 1;
  vpPositionY   = 2;

Type
  Viewport = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Active:Boolean;
      _Visible:Boolean;

      _OfsX:Integer;
      _OfsY:Integer;

      _Width:Integer;
      _Height:Integer;
      _Scale:Single;

      _Camera:Camera;

      _BackgroundColor:Color;

      _Target:Viewport;
      _TargetX1:Single;
      _TargetX2:Single;
      _TargetY1:Single;
      _TargetY2:Single;

      _ViewX:Integer;
      _ViewY:Integer;
      _ViewWidth:Integer;
      _ViewHeight:Integer;
      
      _ContextID:Integer;

      _Buffers:Array[0..Pred(MaxCaptureTargets)]  Of RenderTarget;
      _DoPostProcessing:Boolean;
      _Offscreen:Boolean;
      _DrawSky:Boolean;

      {$IFDEF POSTPROCESSING}
      _FXChain:ScreenFXChain;
      _Downsampler:RenderTargetDownsampler;
      _BloomID:Integer;
      {$ENDIF}

      Function UnprojectVector(WX,WY,WZ:Single):Vector3D;

      Procedure ClearDownSampler();

      {$IFDEF POSTPROCESSING}
      Function GetFXChain: ScreenFXChain;
      {$ENDIF}

    Public
      Constructor Create(Name:TERRAString; Width,Height:Integer; Scale:Single = 1.0);

      Procedure Release; Override;

      Procedure Bind();
      Procedure Clear();
      Procedure Restore(Clear:Boolean);
      Procedure Resize(Width, Height:Integer);

      Procedure OnContextLost;

      Procedure SetRenderTargetState(TargetType:Integer; Enabled:Boolean);
      Function GetRenderTarget(TargetType:Integer):RenderTarget;
      Function IsRenderTargetEnabled(TargetType:Integer):Boolean;
      Function IsDirectDrawing():Boolean;

      Procedure SetViewArea(X,Y,Width,Height:Integer);

      Procedure SetPostProcessingState(Enabled:Boolean);
      Function IsPostProcessingEnabled():Boolean; {$IFDEF FPC}Inline; {$ENDIF}

      Procedure SetOffScreenState(Enabled:Boolean);

      Procedure SetTarget(Target:Viewport; X1,Y1,X2,Y2:Single);
      Procedure SetTargetInPixels(Target:Viewport; X1,Y1,X2,Y2:Integer);


      Function ProjectPoint(Pos:Vector3D):Vector3D;

      Function GetPickRay(TX,TY:Integer):Ray;

      Procedure SetBackgroundColor(BG:Color);

      Procedure BindBloomTexture(Slot:Integer);
      Procedure BindStageTexture(Stage, Slot:Integer);

      Procedure DrawToTarget(AllowDebug:Boolean);

      Property BackgroundColor:Color Read _BackgroundColor Write SetBackgroundColor;

      Property Active:Boolean Read _Active Write _Active;
      Property Camera:TERRA_Camera.Camera Read _Camera;

      Property DrawSky:Boolean Read _DrawSky Write _DrawSky;
      Property OffScreen:Boolean Read _OffScreen Write SetOffScreenState;

      Property Visible:Boolean Read _Visible Write _Visible;

      {$IFDEF POSTPROCESSING}
      Property FXChain:ScreenFXChain Read GetFXChain;
      {$ENDIF}

      Property Name:TERRAString Read _Name Write _Name;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;


      Property OffsetX:Integer Read _OfsX Write _OfsX;
      Property OffsetY:Integer Read _OfsY Write _OfsY;

      Property Target:Viewport Read _Target;
      Property TargetX1:Single Read _TargetX1;
      Property TargetX2:Single Read _TargetX2;
      Property TargetY1:Single Read _TargetY1;
      Property TargetY2:Single Read _TargetY2;
  End;

Implementation
Uses TERRA_Error, TERRA_GraphicsManager, TERRA_Application, TERRA_Log, TERRA_OS, TERRA_Texture, TERRA_Vector4D
{$IFDEF POSTPROCESSING},TERRA_FrameBufferObject{$ENDIF};

{$IFDEF POSTPROCESSING}
Var
  _BlurShader:Shader;
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
  Line('uniform highp float width, height;');
  Line('uniform sampler2D texture;');
	Line('void main()	{');
	Line('lowp float dx = 1.0 / width;');
  Line('lowp float dy = 1.0 / height;');
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

Constructor Viewport.Create(Name:TERRAString; Width,Height:Integer; Scale:Single);
Begin
  _Name := Name;
  _Active := True;
  _DrawSky := False;
  _DoPostProcessing := False;

  _Width := Width;
  _Height := Height;
  _Scale := Scale;

  _OfsX := 0;
  _OfsY := 0;

  _ContextID := Application.Instance.ContextID;

  _BackgroundColor := ColorCreate(0, 0, 0, 255);

  _Camera := TERRA_Camera.Camera.Create(Name);

  SetOffScreenState(False);

  Log(logDebug, 'Viewport', 'Created viewport '+Name+' with size '+IntToString(_Width) +' x '+IntToString(_Height)+' and scale = '+FloatToString(_Scale));
End;


Procedure Viewport.Resize(Width, Height:Integer);
Begin
  Self._Width := Width;
  Self._Height := Height;

  Self.OnContextLost();

  If Assigned(Camera) Then
    Camera.Refresh();
End;

Procedure Viewport.Restore(Clear:Boolean);
Begin
  Self.SetViewArea(_OfsX, _OfsY, _Width, _Height);

  If (Clear) Then
    Self.Clear();
End;

Procedure Viewport.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxCaptureTargets) Do
    ReleaseObject(_Buffers[I]);

  ClearDownSampler();

  ReleaseObject(_Camera);

  {$IFDEF POSTPROCESSING}
  ReleaseObject(_FXChain);
  {$ENDIF}
End;

Function Viewport.ProjectPoint(Pos:Vector3D):Vector3D;
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


Function Viewport.UnprojectVector(WX,WY,WZ:Single):Vector3D;
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

Function Viewport.GetPickRay(TX, TY:Integer):Ray;
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


Procedure Viewport.Bind();
Begin
	//glMatrixMode(GL_PROJECTION);
	//glLoadMatrixf(@_ProjectionMatrix);

  If Assigned(Camera) Then
    Camera.Update(_Width, _Height);
End;


Procedure Viewport.SetRenderTargetState(TargetType: Integer; Enabled: Boolean);
Begin
  If (TargetType<0) Or (TargetType>=MaxCaptureTargets) Then
    Exit;

  If (Application.Instance.IsConsole) Then
    Exit;

  If (TargetType = captureTargetEmission) And (Enabled) And (Not GraphicsManager.Instance.Settings.FrameBufferObject.Avaliable) Then
    Enabled := False;

  If (Assigned(Self._Buffers[TargetType]) = Enabled) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetTargetType');{$ENDIF}

  If Enabled Then
  Begin
    Log(logDebug, 'GraphicsManager', 'Initializing '+TargetNames[TargetType]+' target for '+Self.Name);
    _Buffers[TargetType] := CreateRenderTarget(_Name+'_target'+IntToString(TargetType), Trunc(_Width * _Scale), Trunc(_Height * _Scale), True, True);

    {$IFDEF POSTPROCESSING}
    If (TargetType = captureTargetEmission) And (GraphicsManager.Instance.Settings.FrameBufferObject.Avaliable) Then
    Begin
      Log(logDebug, 'GraphicsManager', 'Initializing bloom downsampler');
      If (_Downsampler = Nil) Then
        _Downsampler := RenderTargetDownsampler.Create('bloom_downsampler', 512, 512);
    End;

    If (GraphicsManager.Instance.Settings.Shaders.Avaliable) And (_DoPostProcessing) And (Not Assigned(_BlurShader)) Then
    Begin
      _BlurShader := Shader.CreateFromString(GetShader_Blur(), 'blur');
      ShaderManager.Instance.AddShader(_BlurShader);
    End;
    {$ENDIF}
  End Else
  Begin
    Log(logDebug, 'GraphicsManager', 'Destroying '+TargetNames[TargetType]+' target for '+Self.Name);
    _Buffers[TargetType].Release();
    _Buffers[TargetType] := Nil;

    {$IFDEF POSTPROCESSING}
    If (TargetType = captureTargetEmission) Then
    Begin
      ClearDownSampler();
    End;
    {$ENDIF}
  End;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Function Viewport.IsRenderTargetEnabled(TargetType:Integer):Boolean;
Begin
  If (TargetType<0) Or (TargetType>=MaxCaptureTargets) Then
    Result := False
  Else
    Result := Assigned(_Buffers[TargetType]);
End;


Function Viewport.GetRenderTarget(TargetType: Integer): RenderTarget;
Begin
  If (_ContextID <> Application.Instance.ContextID) Then
  Begin
    Self.OnContextLost();
  End;

  If (TargetType<0) Or (TargetType>=MaxCaptureTargets) Then
    Result := Nil
  Else
    Result := _Buffers[TargetType];
End;

Procedure Viewport.DrawToTarget(AllowDebug:Boolean);
Var
  MyShader:Shader;
  I,ShowID:Integer;
Begin
  If (Target = Nil) Then
    Exit;

  {$IFDEF POSTPROCESSING}
  {$IFDEF FRAMEBUFFEROBJECTS}
  If (Self._DoPostProcessing) Then
  Begin
    //apply bloom
    If (_Buffers[captureTargetEmission] <> Nil) And (_Buffers[captureTargetEmission] Is FrameBufferObject)
    And (Assigned(_Downsampler)) And (Assigned(_BlurShader)) Then
    Begin
      _BloomID := _Downsampler.Update(FrameBufferObject(_Buffers[captureTargetEmission]), _BlurShader, 0, 3);
    End;
  End;
  {$ENDIF}
  {$ENDIF}

  Target.Restore(False);

  {$IFDEF POSTPROCESSING}
  ShowID := GraphicsManager.Instance.ShowDebugTarget;
  If (ShowID>=MaxCaptureTargets) Then
    ShowID := 0;

  If (Assigned(_FXChain)) Then
  Begin
    If (Not GraphicsManager.Instance.Settings.PostProcessing.Enabled) Then
    Begin
      For I:=0 To Pred(MaxCaptureTargets) Do
        Self.SetRenderTargetState(I, False);

      _FXChain.Release;
      _FXChain := Nil;
    End;

    If (Assigned(_FXChain)) And (ShowID<0) Then
    Begin
      _FXChain.DrawScreen(_TargetX1, _TargetY1, _TargetX2, _TargetY2);
      Exit;
    End;
  End;


  If (ShowID<0) Or (Not AllowDebug) Then
    ShowID := captureTargetColor;

  {$ELSE}
  ShowID := captureTargetColor;
  {$ENDIF}

  If (GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    MyShader := GetDefaultFullScreenShader();
    ShaderManager.Instance.Bind(MyShader);
    MyShader.SetUniform('texture', 0);
    MyShader.SetUniform('color', ColorWhite);
  End Else
    MyShader := Nil;

  If (_Buffers[ShowID] = Nil) Then
    Self.SetRenderTargetState(ShowID, True);

  _Buffers[ShowID].Bind(0);
  //_Buffers[ShowID].BilinearFilter := False;

  GraphicsManager.Instance.SetBlendMode(blendNone);

  If (GraphicsManager.Instance.ShowDebugTarget<=0) Then
    GraphicsManager.Instance.SetBlendMode(blendBlend);
    
  GraphicsManager.Instance.DrawFullscreenQuad(MyShader, _TargetX1, _TargetY1, _TargetX2, _TargetY2);
End;


Procedure Viewport.BindBloomTexture(Slot:Integer);
Begin
{$IFDEF POSTPROCESSING}
  If Assigned(_Downsampler) Then
    _Downsampler.GetRenderTexture(_BloomID).Bind(Slot)
  Else
{$ENDIF}
  TextureManager.Instance.BlackTexture.Bind(Slot);
End;

Procedure Viewport.BindStageTexture(Stage, Slot:Integer);
Begin
{$IFDEF POSTPROCESSING}
  If Assigned(_Buffers[Stage]) Then
    _Buffers[Stage].Bind(Slot)
  Else
{$ENDIF}
  TextureManager.Instance.BlackTexture.Bind(Slot);
End;

Procedure Viewport.SetPostProcessingState(Enabled: Boolean);
Begin
  If (Not GraphicsManager.Instance.Settings.PostProcessing.Enabled) Then
    Enabled := False;

  {$IFDEF POSTPROCESSING}
  If (Self._DoPostProcessing = Enabled) Then
    Exit;

  Self._DoPostProcessing := Enabled;
  If Enabled Then
    _FXChain := ScreenFXChain.Create()
  Else
  Begin
    _FXChain.Release();
    _FXChain := Nil;
  End;
  {$ELSE}
  Self._DoPostProcessing := False;
  {$ENDIF}
End;

Function Viewport.IsDirectDrawing: Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxCaptureTargets) Do
  If Assigned(_Buffers[I]) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;
End;

Function Viewport.IsPostProcessingEnabled: Boolean;
Begin
  Result := _DoPostProcessing;
End;

Procedure Viewport.Clear;
Var
  UseScissors:Boolean;
  Flags:Integer;
Begin
  UseScissors := (Trunc(_Width*_Scale)<GraphicsManager.Instance.Width) Or (Trunc(_Height*_Scale)<GraphicsManager.Instance.Height);

  If (UseScissors) Then
  Begin
    glScissor(0, 0, Trunc(_Width*_Scale), Trunc(_Height*_Scale));
    glEnable(GL_SCISSOR_TEST);
  End;

  glClearColor(_BackgroundColor.R/255, _BackgroundColor.G/255, _BackgroundColor.B/255, _BackgroundColor.A/255);

  Flags := GL_DEPTH_BUFFER_BIT Or GL_STENCIL_BUFFER_BIT;

  If (Not Self.IsDirectDrawing()) Or (_BackgroundColor.A>=255) Then
    Flags := Flags Or GL_COLOR_BUFFER_BIT;

  glClear(Flags);

  If UseScissors Then
    glDisable(GL_SCISSOR_TEST);
End;

Procedure Viewport.SetBackgroundColor(BG: Color);
Begin
  _BackgroundColor := BG;
End;

Procedure Viewport.SetOffScreenState(Enabled: Boolean);
Begin
  {$IFDEF IPHONE}
  Enabled := True;  // iOS does not support direct rendering
  {$ENDIF}

  _Offscreen := Enabled;

  If (Enabled) Then
    Self.SetRenderTargetState(captureTargetColor, True);
End;

Procedure Viewport.SetTarget(Target: Viewport; X1, Y1, X2, Y2: Single);
Begin
  If (Self._Target = Target) Then
    Exit;

  Self._Target := Target;
  Self._TargetX1 := X1;
  Self._TargetX2 := X2;
  Self._TargetY1 := Y1;
  Self._TargetY2 := Y2;
End;

Procedure Viewport.SetTargetInPixels(Target: Viewport; X1, Y1, X2, Y2:Integer);
Begin
  If (Self._Target = Target) Then
    Exit;

  If (Assigned(Target)) Then
    SetTarget(Target, SafeDiv(X1, Target.Width), SafeDiv(Y1, Target.Height), SafeDiv(X2, Target.Width), SafeDiv(Y2, Target.Height))
  Else
    SetTarget(Nil, 0.0, 0.0, 1.0, 1.0);
End;

Procedure Viewport.SetViewArea(X, Y, Width, Height: Integer);
Begin
  _ViewX := X;
  _ViewY := Y;
  _ViewWidth := Width;
  _ViewHeight := Height;

  glViewport(Trunc(X * _Scale), Trunc(Y * _Scale), Trunc(Width * _Scale), Trunc(Height * _Scale));
End;


Procedure Viewport.OnContextLost;
Var
  I:Integer;
  Temp:Boolean;
Begin
  Log(logDebug, 'Viewport', 'Context lost: '+Self.Name);

  _ContextID := Application.Instance.ContextID;

  ClearDownSampler();

  For I:=0 To Pred(MaxCaptureTargets) Do
  Begin
    Temp := Assigned(_Buffers[I]);
    If Temp Then
    Begin
      Log(logDebug, 'Viewport', 'Reseting '+TargetNames[I]+' target for '+Self.Name);
      Self.SetRenderTargetState(I, False);
      Self.SetRenderTargetState(I, True);
    End;
  End;


  {$IFDEF POSTPROCESSING}
  If Assigned(_FXChain) Then
    _FXChain.OnContextLost();
  {$ENDIF}
End;

Procedure Viewport.ClearDownSampler();
Begin
  {$IFDEF POSTPROCESSING}
  If Assigned(_DownSampler) Then
  Begin
    _DownSampler.Release;
    _DownSampler := Nil;
  End;
  {$ENDIF}
End;

{$IFDEF POSTPROCESSING}
Function Viewport.GetFXChain: ScreenFXChain;
Begin
  If Not GraphicsManager.Instance.Settings.PostProcessing.Avaliable Then
  Begin
    Log(logError, 'Viewport', 'Postprocessing not supported in this device!');
    Result := Nil;
    Exit;
  End;

  If _FXChain = Nil Then
    _FXChain := ScreenFXChain.Create();
    
  Result := _FXChain;
End;

{$ENDIF}

End.
