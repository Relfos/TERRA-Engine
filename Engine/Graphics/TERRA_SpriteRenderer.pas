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
 * TERRA_Sprite
 * Implements the global sprite manager
 ***********************************************************************************************************************
}
Unit TERRA_SpriteRenderer;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color, TERRA_Texture, TERRA_Renderer,
  TERRA_Application, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect,
  TERRA_VertexFormat, TERRA_Sprite;

Type
  TERRASpriteRenderer = Class;

  SpriteBatch = Class(TERRAObject)
    Protected
      _First:TERRASprite;
      _SpriteCount:Integer;
      _Texture:TERRATexture;

      _Manager:TERRASpriteRenderer;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _BlendMode:Integer;
      _Layer:Single;
      _Closed:Boolean;
      _Shader:ShaderInterface;
      _Saturation:Single;
      _Glow:ColorRGBA;
      _Outline:ColorRGBA;

      _Vertices:VertexData;
      _RenderCount:Integer;

      _Ready:Boolean;

      Procedure AddSprite(P:TERRASprite);

      Procedure Prepare();
      Procedure Render(Const ProjectionMatrix:Matrix4x4; Stage:RendererStage);
      Procedure Clear();

    Public
      Constructor Create(Manager:TERRASpriteRenderer);
      Procedure Release();
      //Procedure SetupSaturationCombiners(Var Slot:Integer);
  End;

  TERRASpriteRenderer = Class(TERRAObject)
    Protected
      _NullSprite:TERRASprite;

      _Index:Integer;
      _SpriteCount:Integer;
      _Sprites:Array Of TERRASprite;

      _Batches:Array Of SpriteBatch;
      _BatchCount:Integer;

      _CurrentShader:ShaderInterface;

      _SpriteShaderWithoutGrading:ShaderInterface;
      _SpriteShaderSolid:ShaderInterface;
      {$IFNDEF DISABLECOLORGRADING}
      _SpriteShaderWithGrading:ShaderInterface;
      {$ENDIF}

      _FontShader:ShaderInterface;

      Procedure SetShader(Const ProjectionMatrix:Matrix4x4; Shader:ShaderInterface);


   Public
      Constructor Create();
      Procedure Release; Override;

      Procedure Prepare;
      Procedure Render(Const ProjectionMatrix:Matrix4x4; Stage:RendererStage);
      Procedure Clear;


      Procedure QueueSprite(S:TERRASprite);

      Function DrawSprite(X,Y,Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend; Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):QuadSprite;
      Function DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:TERRATexture; Outline:ColorRGBA; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):QuadSprite;

      Property FontShader:ShaderInterface Read _FontShader;
  End;


Implementation
Uses TERRA_ResourceManager, TERRA_InputManager, TERRA_GraphicsManager, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  BatchSize = 128;

Var
  _TERRASpriteRenderer_Instance:ApplicationObject = Nil;

Function GetSaturationAndConstrast():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
	S := '';
	Line('  const lowp vec3 LumCoeff = vec3(0.2125, 0.7154, 0.0721);');
	Line('  const lowp float middleGray = 0.5;');
	Line('lowp vec3 AdjustSaturation(lowp vec3 color, lowp float saturation)	{');
  Line('  lowp vec3 AvgLumin = vec3(0.5);');
  Line('  lowp vec3 intensity = vec3(dot(color, LumCoeff));');
  Line('  lowp vec3 satColor = mix(intensity, color, saturation);');
  Line('return satColor;	}');
  Result := S;
End;

Function GetShader_Sprite(DoColorGrading, IsFont, IsSolid:Boolean):TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 fillColor;');
	Line('  varying lowp vec4 color;');
  Line('  attribute highp vec3 terra_position;');
  Line('  attribute mediump vec2 terra_UV0;');
  Line('  attribute mediump vec4 terra_UV1;');
  Line('  attribute mediump vec4 terra_UV2;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  uniform mat4 projectionMatrix;');
//  Line('  uniform mat4 modelMatrix;');

	Line('void main()	{');
  Line('  vec4 local_position = vec4(terra_position.x, terra_position.y, terra_position.z, 1.0);');
  Line('  screen_position = local_position.xy;');
  Line('  gl_Position =  projectionMatrix * local_position;');
  Line('  texCoord = terra_UV0;');
  Line('  clipRect = terra_UV1;');
  Line('  fillColor = terra_UV2;');
  Line('  color = terra_color;}');
  Line('}');

  Line('fragment {');

	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 fillColor;');
	Line('  varying lowp vec4 color;');

  If (Not IsSolid) Then
	  Line('  uniform sampler2D texture;');

  If IsFont Then
  Begin
    Line('  const float smoothing = 3.0/16.0;');
    Line('  const float outlineWidth = 5.0/16.0;');
    Line('  const float outerEdgeCenter = 0.5 - outlineWidth;');
    Line('  uniform lowp vec4 shadowOffset;');
    Line('  uniform lowp vec4 outlineColor;');
  End;

  Line(GetSaturationAndConstrast());

  {$IFNDEF DISABLECOLORGRADING}
  If (DoColorGrading) Then
    Line(GetColorTableShaderCode());
  {$ENDIF}

	Line('  void main()	{');

  Line('  if ( screen_position.x< clipRect.x) { discard;} ');
  Line('  if ( screen_position.x> clipRect.z) { discard;} ');

  Line('  if ( screen_position.y< clipRect.y) { discard;} ');
  Line('  if ( screen_position.y> clipRect.w) { discard;} ');

  If (IsSolid) Then
    Line('    lowp vec4 sourceColor = fillColor;')
  Else
    Line('    lowp vec4 sourceColor = texture2D(texture, texCoord.xy);');

  If IsFont Then
  Begin
  {$IFDEF DISTANCEFIELDFONTS}
  Line('    float colorDistance = sourceColor.a;');
  Line('    float alpha = smoothstep(outerEdgeCenter - smoothing, outerEdgeCenter + smoothing, colorDistance);');
  Line('    float border = smoothstep(0.5 - smoothing, 0.5 + smoothing, colorDistance);');
  Line('    vec4 baseColor = mix(outlineColor, color, border);');

(*  Line('    float shadowDistance = texture2D(texture, texCoord.xy + vec2(0.007, 0.005)).a;');
  Line('    float shadowAlpha = smoothstep(outerEdgeCenter - smoothing, outerEdgeCenter + smoothing, shadowDistance);');
  Line('    vec4 shadowColor = vec4(0.0, 0.0, 0.0, 1.0);');

  Line('    baseColor = mix(shadowColor, baseColor, shadowAlpha); ');*)

  Line('  alpha *=  color.a;');
  Line('    gl_FragColor = vec4( baseColor.rgb, alpha);');
  //Line('    gl_FragColor = vec4( color.rgb, border), alpha );');

  {$ELSE}

  Line('    lowp vec4 mask = sourceColor;');
  Line('    lowp float alpha;');
  Line('    if (mask.a<0.5) alpha = 0.0; else alpha = 1.0;');
  {$IFNDEF MOBILE}
  Line('    alpha *= smoothstep(0.25, 0.75, mask.a);');// anti-aliasing
  {$ENDIF}
  Line('    lowp vec4 baseColor;');
  Line('    baseColor = color; ');

  Line('    baseColor.rgb = AdjustSaturation(baseColor.rgb, fillColor.a); ');
//  Line('    baseColor.rgb = mix(baseColor.rgb, outlineColor.rgb, mask.r); ');
  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, baseColor.b, alpha * color.a);}');

  //Line('    gl_FragColor = vec4(mask.r);');

  {$ENDIF}
  End Else
  Begin
    Line('    vec4 c = sourceColor * color;');
    {$IFNDEF DISABLECOLORGRADING}
    If (DoColorGrading) Then
      Line('    c.rgb = ColorTableLookup(c.rgb);');
    {$ENDIF}
    Line('    c.rgb = AdjustSaturation(c.rgb, fillColor.a); ');

    //Line('    if (c.a<0.1) discard;');
    Line('    gl_FragColor = c;}');
  End;

  Line('}  ');
  Result := S;
End;

(*

Function GetShader_Font():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying highp vec4 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  varying lowp float saturation;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec3 terra_UV0;');
  Line('  attribute mediump vec4 terra_UV1;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  attribute lowp float terra_saturation;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  texCoord = vec4(terra_UV0.x, terra_UV0.y, terra_position.x, terra_position.y);');
  Line('  clipRect = terra_UV1;');
  Line('  saturation = terra_saturation;');
  Line('  color = terra_color;}');
  Line('}');
  Line('fragment {');
	Line('  varying highp vec4 clipRect;');
	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  varying lowp float saturation;');
	Line('  uniform sampler2D texture;');
  Line('  uniform lowp vec4 outline;');

  Line(GetSaturationAndConstrast());

	Line('  void main()	{');

  {$IFDEF DISTANCEFIELDFONTS}
  //Line('    if (mask>0.59) baseColor = color; else baseColor = mix(color, outline, outline.a);');
  //Line('    baseColor = mix(outline, color, alpha);');


  Line('    float distance = texture2D(texture, texCoord.xy).a;');
  Line('    float alpha = smoothstep(outerEdgeCenter - smoothing, outerEdgeCenter + smoothing, distance);');
  Line('    float border = smoothstep(0.5 - smoothing, 0.5 + smoothing, distance);');
  Line('    gl_FragColor = vec4( mix(outline.rgb, color.rgb, border), alpha );');

  {$ELSE}

  Line('    lowp float mask = texture2D(texture, texCoord.xy).a;');
  Line('    lowp float alpha;');
  Line('    if (mask<0.5) alpha = 0.0; else alpha = 1.0;');
  {$IFNDEF MOBILE}
  Line('    alpha *= smoothstep(0.25, 0.75, mask);');// anti-aliasing
  {$ENDIF}
  Line('    lowp vec4 baseColor;');
  Line('    baseColor = color; ');

  //Line('    baseColor.rgb = AdjustSaturation(c.rgb, saturation); ');
  Line('    baseColor.rgb = AdjustSaturation(baseColor.rgb, saturation); ');
  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, baseColor.b, alpha * color.a);}');
  {$ENDIF}
//  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, 1.0, 1.0);}');
  Line('}  ');
  Result := S;
End;*)


{ TERRASpriteRenderer }
Constructor TERRASpriteRenderer.Create();
Var
    I:Integer;
Begin
  _SpriteCount := 2000;
  SetLength(_Sprites, _SpriteCount);
  For I:=0 To Pred(_SpriteCount) Do
    _Sprites[I] := QuadSprite.Create;
  _Index := -1;

  _BatchCount := 50;
  SetLength(_Batches, _BatchCount);
  For I:=0 To Pred(_BatchCount) Do
    _Batches[I] := SpriteBatch.Create(Self);
End;

Procedure TERRASpriteRenderer.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SpriteCount) Do
    ReleaseObject(_Sprites[I]);
  _SpriteCount := 0;

  For I:=0 To Pred(_BatchCount) Do
    ReleaseObject(_Batches[I]);
  _BatchCount := 0;

  ReleaseObject(_NullSprite);

  ReleaseObject(_FontShader);
  ReleaseObject(_CurrentShader);
  ReleaseObject(_SpriteShaderWithoutGrading);
  ReleaseObject(_SpriteShaderSolid);
  {$IFNDEF DISABLECOLORGRADING}
  ReleaseObject(_SpriteShaderWithGrading);
  {$ENDIF}

  _TERRASpriteRenderer_Instance := Nil;
End;

Function TERRASpriteRenderer.DrawSprite(X,Y,Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; Shader:ShaderInterface): QuadSprite;
Begin
  Result := Self.DrawSpriteWithOutline(X,Y,Layer, SpriteTexture, ColorNull, ColorTable, BlendMode,  Saturation, Filter, Shader);
End;

Function TERRASpriteRenderer.DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:TERRATexture; Outline:ColorRGBA; ColorTable:TERRATexture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; Shader:ShaderInterface):QuadSprite;
Var
  I:Integer;
Begin
  If (Not Assigned(SpriteTexture)) Or (Not SpriteTexture.IsReady()) Then
  Begin
    If Not Assigned(_NullSprite) Then
      _NullSprite := QuadSprite.Create();

    Result := QuadSprite(_NullSprite);
    Exit;
  End;

  {$IFDEF WINDOWS}
  If (Layer<0) Then
    DebugBreak;
  {$ENDIF}

  X := Trunc(X);
  Y := Trunc(Y);

  Inc(_Index);
  If (_Index>=_SpriteCount) Then
  Begin
    _SpriteCount := _SpriteCount * 2;
    SetLength(_Sprites, _SpriteCount);
    For I:=_Index To Pred(_SpriteCount) Do
        _Sprites[I] := QuadSprite.Create;
  End;

  If (_Sprites[_Index] = Nil) Then
    _Sprites[_Index] := QuadSprite.Create;

  Result := QuadSprite(_Sprites[_Index]);
  Result.Position.X := X;
  Result.Position.Y := Y;
  Result.Layer := Layer;
  Result.Rect.Width := 0;
  Result.Rect.Height := 0;
  {$IFNDEF DISABLECOLORGRADING}
  Result.ColorTable := ColorTable;
  {$ENDIF}
  Result.Anchor := VectorCreate2D(0, 0);
  Result.Flip := False;
  Result.Mirror := False;
  Result.ClipRect.Style := clipNothing;
  Result.Saturation := Saturation;
  Result.Shader := Shader;
  Result.Outline := Outline;

  Result.SetTransform(MatrixIdentity3x3);

  SpriteTexture.Filter := Filter;
//  SpriteTexture.Wrap := True;
  SpriteTexture.MipMapped := False;

  Result.SetColor(ColorWhite);
  Result.Texture := SpriteTexture;
  Result.BlendMode := BlendMode;

  Result.Rect.U1 := 0.0;
  Result.Rect.V1 := 0.0;
  Result.Rect.U2 := 1.0;
  Result.Rect.V2 := 1.0;

  Result.ScrollU := 0.0;
  Result.ScrollV := 0.0;

  Self.QueueSprite(Result);
End;

Procedure TERRASpriteRenderer.QueueSprite(S:TERRASprite);
Var
  N, I:Integer;
  TargetLayer:Single;
  HasShaders, ResetBatch:Boolean;
Begin
  If (S = Nil) Then
    Exit;

  If (S.Texture = Nil) Then
    Exit;
    
  HasShaders := GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable;

  ResetBatch := True;

  TargetLayer := S.Layer * 0.01;

  N := -1;
  For I:=0 To Pred(_BatchCount) Do
  If ((_Batches[I]._Texture = S.Texture) And (_Batches[I]._BlendMode = S.BlendMode)
  {$IFNDEF DISABLECOLORGRADING}And (_Batches[I]._ColorTable = S.ColorTable){$ENDIF}
  And (_Batches[I]._Shader = S.Shader)
  And ( (HasShaders) Or (_Batches[I]._Saturation = S.Saturation))
  And (Cardinal(_Batches[I]._Outline) = Cardinal(S.Outline))
  And (_Batches[I]._SpriteCount<BatchSize)) And (_Batches[I]._Layer = TargetLayer)
  And (Not _Batches[I]._Closed) Then
  Begin
    N := I;
    ResetBatch := False;
    Break;
  End;
  
  If (N<0) Then
  Begin
    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._SpriteCount <=0) Then
    Begin
      N := I;
      Break;
    End;
  End;

  If (N<0) Then
  Begin
    N := _BatchCount;
    Inc(_BatchCount);
    SetLength(_Batches, _BatchCount);
  End;

  If ResetBatch Then
  Begin
    _Batches[N]._SpriteCount := 0;
    _Batches[N]._BlendMode := S.BlendMode;
    _Batches[N]._Texture := S.Texture;
    _Batches[N]._Closed := False;
    _Batches[N]._Layer := TargetLayer;
    {$IFNDEF DISABLECOLORGRADING}
    _Batches[N]._ColorTable := S.ColorTable;
    {$ENDIF}
    _Batches[N]._Saturation := S.Saturation;
    _Batches[N]._Glow := S.Glow;
    _Batches[N]._Shader := S.Shader;
    _Batches[N]._Outline := S.Outline;
    _Batches[N]._First := Nil;
    _Batches[N]._Manager := Self;
  End;

  _Batches[N].AddSprite(S);
End;

Procedure TERRASpriteRenderer.Prepare;
Var
  I:Integer;
  Graphics:GraphicsManager;
Begin
  Graphics := GraphicsManager.Instance;
  If (_SpriteShaderWithoutGrading = Nil) Then
  Begin
    _SpriteShaderWithoutGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithoutGrading.Generate('Sprite', GetShader_Sprite(False, False, False));
  End;

  If (_SpriteShaderSolid = Nil) Then
  Begin
    _SpriteShaderSolid := Graphics.Renderer.CreateShader();
    _SpriteShaderSolid.Generate('SpriteSolid', GetShader_Sprite(False, False, True));
  End;


  {$IFNDEF DISABLECOLORGRADING}
  If (_SpriteShaderWithGrading = Nil) Then
  Begin
    _SpriteShaderWithGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithGrading.Generate('SpriteGrading', GetShader_Sprite(True, False, False));
  End;
  {$ENDIF}

  If (_FontShader = Nil) Then
  Begin
    _FontShader := Graphics.Renderer.CreateShader();
    _FontShader.Generate('Font', GetShader_Sprite(False, True, False));
  End;

  For I:=0 To Pred(_BatchCount) Do
  Begin
    _Batches[I].Prepare();
  End;

  _Index := -1;
End;

Procedure TERRASpriteRenderer.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) Then
  Begin
    _Batches[I].Clear();
  End;

  _Index := -1;
End;

Procedure TERRASpriteRenderer.Render(Const ProjectionMatrix:Matrix4x4; Stage:RendererStage);
Var
  I,K:Integer;
  Min:Single;
  Total, Index, Count:Integer;
  M:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Render');{$ENDIF}

  Graphics := GraphicsManager.Instance;
  Graphics.Renderer.SetBlendMode(blendNone);

  //glEnable(GL_DEPTH_TEST); {FIXME}
  Graphics.Renderer.SetDepthFunction(compareLessOrEqual);

  Graphics.Renderer.SetCullMode(cullNone);
//  Graphics.Renderer.SetColorMask(True, True, True, False);

  (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    Projection := Graphics.ProjectionMatrix;

    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@Projection);

    M := Matrix4x4Identity;

    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@M);

    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@M);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  End;
  BIBI
  *)


  _CurrentShader := Nil;

  Total := 0;
  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) Then
    Inc(Total);

  Count := 0;
  While (Total>0) Do
  Begin
    Index := -1;
    Min := 9999;

    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._Ready) And (_Batches[I]._Layer<Min) Then
    Begin
      _Batches[I]._Ready := False;
      Min := _Batches[I]._Layer;
      Index := I;
    End;

    If (Index>=0) Then
    Begin
      _Batches[Index].Render(ProjectionMatrix, Stage);
      Dec(Total);

      Inc(Count); //If Count>1 Then break;
    End Else
    Break;
  End;


  Graphics.Renderer.SetDepthTest(True);    //BIBI

  (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;*)

  _CurrentShader := Nil;

  For I:=0 To Pred(_BatchCount) Do
    _Batches[I]._Ready := (_Batches[I]._SpriteCount>0);

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure TERRASpriteRenderer.SetShader(Const ProjectionMatrix:Matrix4x4; Shader:ShaderInterface);
Var
  Graphics:GraphicsManager;
Begin
  If (_CurrentShader = Shader) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetShader');{$ENDIF}

  Graphics := GraphicsManager.Instance;

  _CurrentShader := Shader;

  {If Not MyShader.IsReady() Then BIBI
    Exit;}

  Graphics.Renderer.BindShader(Shader);

  Shader.SetIntegerUniform('texture', 0);
  //Graphics.Renderer.SetModelMatrix(CameraMatrix);
  Graphics.Renderer.SetProjectionMatrix(ProjectionMatrix);

  {If (MyShader = _FontShader) Then
    IntToString(2);}

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

{ SpriteBatch }
Constructor SpriteBatch.Create(Manager: TERRASpriteRenderer);
Begin
  _Manager := Manager;
  _Vertices := CreateSpriteVertexData(6 * BatchSize);
End;

Procedure SpriteBatch.AddSprite(P:TERRASprite);
Var
  S, Prev:TERRASprite;
Begin
  Inc(_SpriteCount);
  P.Next := _First;
  _First := P;
End;

Procedure SpriteBatch.Release;
Begin
  ReleaseObject(_Vertices);
End;

Procedure SpriteBatch.Clear();
Begin
  _Closed := False;
  _SpriteCount := 0;
  _RenderCount := 0;
  _Texture := Nil;
  _First := Nil;
End;


Procedure SpriteBatch.Prepare();
Var
  I, J:Integer;
  S:TERRASprite;
  K:Single;
  MaxX,MinX, MaxY,MinY:Single;
  M:Matrix3x3;
  C:ColorRGBA;
  InIt, OutIt:VertexIterator;
  Src, Dest:SpriteVertex;
  Ofs:Integer;
  FullyClipped:Boolean;
  Ratio:Single;
  Pos:Vector3D;
  Graphics:GraphicsManager;
  CurrentClip:Vector4D;
Begin
  If (_SpriteCount<=0) Then
  Begin
    _First := Nil;
    Exit;
  End;

  Graphics := GraphicsManager.Instance;

  OutIt := _Vertices.GetIteratorForClass(SpriteVertex);

  Ofs := 0;
  S := _First;
  While Assigned(S) Do
  Begin
    If (S.ClipRect.Style = clipEverything) Or  (Not S.Rebuild()) Then
    Begin
      S := S.Next;
      Continue;
    End;

    If (S.ClipRect.Style = clipNothing) Then
      CurrentClip := VectorCreate4D(0, 0, 9999, 9999)
    Else
      CurrentClip := VectorCreate4D(S.ClipRect.X1, S.ClipRect.Y1, S.ClipRect.X2, S.ClipRect.Y2);

    MinX := 9999;
    MaxX := -9999;
    MinY := 9999;
    MaxY := -9999;

    If Not OutIt.Seek(Ofs) Then
      Break;

    InIt := S.Vertices.GetIteratorForClass(SpriteVertex);
    While (InIt.HasNext()) And (OutIt.HasNext()) Do
    Begin
      Src := SpriteVertex(InIt.Value);
      Dest := SpriteVertex(OutIt.Value);

      Pos := Src.Position;

      Pos := S.Transform.Transform(Pos);

      //Pos.Scale(5);
      (*Pos.X := Trunc(Pos.X);
      Pos.Y := Trunc(Pos.Y);
      Pos.Z := Pos.Z + S.Layer;*)

      Dest.Position := Pos;
      Dest.Saturation := S.Saturation;
      Dest.Glow := S.Glow;
      Dest.TexCoord := Src.TexCoord;
      Dest.Color := Src.Color;

      Dest.ClipRect := CurrentClip;

      MinX := FloatMin(MinX, Pos.X);
      MinY := FloatMin(MinY, Pos.Y);
      MaxX := FloatMax(MaxX, Pos.X);
      MaxY := FloatMax(MaxY, Pos.Y);
    End;
    ReleaseObject(InIt);

    FullyClipped := (S.ClipRect.Style = clipSomething) And ((Abs(MinX-MaxX)<Epsilon) Or (Abs(MinY-MaxY)<Epsilon));

    If Not FullyClipped Then
      Inc(Ofs, S.Vertices.Count);

    S := S.Next;
  End;
  ReleaseObject(OutIt);

  _RenderCount := Ofs;
  _Ready := True;
End;

Procedure SpriteBatch.Render(Const ProjectionMatrix:Matrix4x4; Stage:RendererStage);
Var
  Graphics:GraphicsManager;
Begin
  If (_SpriteCount<=0) Then
  Begin
    _First := Nil;
    Exit;
  End;

  Graphics := GraphicsManager.Instance;

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SpriteVertex));
  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D,  @_Vertices[0].Position);
  Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @_Vertices[0].TexCoord);
  Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor,  @_Vertices[0].Color);

  If (_Saturation<1.0) Then
    Graphics.Renderer.SetAttributeSource(TERRA_SATURATION_ATTRIBUTE, typeFloat, @_Vertices[0].Saturation);}

  If (Assigned(Self._Shader)) Then
  Begin
    _Manager.SetShader(ProjectionMatrix, Self._Shader);
    _Manager._FontShader.SetColorUniform('outlineColor', _Outline);
    _Manager._FontShader.SetVec2Uniform('shadowOffset', VectorCreate2D(0.1, 0.1));
  End Else
  If (Stage<>renderStageDiffuse) Then
    _Manager.SetShader( ProjectionMatrix, _Manager._SpriteShaderSolid)
  Else
  {$IFNDEF DISABLECOLORGRADING}
  If (Assigned(Self._ColorTable)) Then
    _Manager.SetShader(ProjectionMatrix, _Manager._SpriteShaderWithGrading)
  Else
  {$ENDIF}
    _Manager.SetShader(ProjectionMatrix, _Manager._SpriteShaderWithoutGrading);

//  _Texture := TextureManager.Instance.WhiteTexture;

  If Not _Texture.Bind(0) Then
    Exit;

  {$IFNDEF DISABLECOLORGRADING}
  If (Self._Shader = Nil) Then
    ColorTableBind(_ColorTable, 1);
  {$ENDIF}

  Graphics.Renderer.SetBlendMode(_BlendMode);
//  Ratio := UIManager.Instance.Ratio;

  Graphics.Renderer.SetVertexSource(_Vertices);
  Graphics.Renderer.DrawSource(renderTriangles, _RenderCount);

  (*
  If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    If (Slot>0) Then
    Begin
      Slot := Graphics.Renderer.Features.MaxTextureUnits;
      For I:=Pred(Slot) DownTo 1 Do
      Begin
        glActiveTexture(GL_TEXTURE0 + I);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        glDisable(GL_TEXTURE_2D);
      End;
    End;
  End;
  BIBI *)
End;

End.
