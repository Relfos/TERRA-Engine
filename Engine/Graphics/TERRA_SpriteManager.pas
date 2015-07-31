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
 * TERRA_SpriteManager
 * Implements the global sprite manager
 ***********************************************************************************************************************
}
Unit TERRA_SpriteManager;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color, TERRA_GraphicsManager, TERRA_Texture,
  TERRA_Application, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect,
  TERRA_Renderer, TERRA_InputManager, TERRA_VertexFormat;

Type
  SpriteManager = Class;

  SpriteVertex = Class(Vertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
      Position:Vector3D;
      Color:Color;
      TexCoord:Vector2D;
      ClipRect:Vector4D;
      Saturation:Single;
  End;

  TextureRect = Object
    Public
      Width:Integer;
      Height:Integer;
      U1,V1:Single;
      U2,V2:Single;

      Procedure ResizeWithWidth(W:Single);
      Procedure ResizeWithHeight(H:Single);

      Procedure TileRemap(X,Y, TilesPerX, TilesPerY:Integer; Tex:TERRATexture);
      Procedure TileRemapByID(TileID, TilesPerRow, TileSize:Integer; Tex:TERRATexture);
      Procedure PixelRemap(X1,Y1, X2, Y2:Integer; Tex:TERRATexture; W:Integer=0; H:Integer=0);
      Procedure UVRemap(_U1,_V1, _U2, _V2:Single);

      Procedure FullRemap();
  End;

  Sprite = Class(TERRAObject)
    Protected
      _Next:Sprite;

      _BlendMode:Integer;
      _Outline:Color;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _Transform:Matrix3x3;
      _Shader:ShaderInterface;

      _Saturation:Single;

      _Texture:TERRATexture;

      _Vertices:VertexData;

      Procedure Rebuild(); Virtual; Abstract;

    Public
      Layer:Single;
      ClipRect:ClipRect;

      Procedure Release; Override;

      Procedure SetColor(Const C:Color); Virtual; Abstract;

      Procedure SetTransform(Const Mat:Matrix3x3); Overload;
      Procedure SetTransform(Const Center:Vector2D; Const Mat:Matrix3x3); Overload;

      Procedure SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;

      Procedure SetScaleAndRotation(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Const Center:Vector2D; Scale:Single; Rotation:Single); Overload;

      Procedure ConcatTransform(Const Mat:Matrix3x3);

      Property Texture:TERRATexture Read _Texture Write _Texture;
  End;

  QuadSprite = Class(Sprite)
    Protected
      _ScrollU:Single;
      _ScrollV:Single;

      _A, _B, _C, _D:Color;

      Procedure Rebuild(); Override;

    Public
      Position:Vector2D;
      Anchor:Vector2D;

      Mirror:Boolean;
      Flip:Boolean;

      Rect:TextureRect;

      Procedure SetColor(Const C:Color); Override;
      Procedure SetColors(Const A, B, C, D:Color);
      Procedure SetAlpha(Alpha:Byte);

      Procedure SetScroll(U,V:Single);

      Procedure SetScale(ScaleX, ScaleY:Single); Overload;
      Procedure SetScale(Scale:Single); Overload;

      Procedure SetScaleRelative(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;
      Procedure SetScaleRelative(Const Center:Vector2D; Scale:Single); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single ); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; Scale:Single; Rotation:Single ); Overload;
      Procedure SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);

      Procedure SetScaleAndRotation(ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Scale:Single; Rotation:Single); Overload;

      Property Transform:Matrix3x3 Read _Transform;
  End;

  SpriteBatch = Object
    Protected
      _First:Sprite;
      _Count:Integer;
      _Texture:TERRATexture;

      _Manager:SpriteManager;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _BlendMode:Integer;
      _Layer:Single;
      _Closed:Boolean;
      _Shader:ShaderInterface;
      _Saturation:Single;
      _Outline:Color;

      Procedure AddSprite(P:Sprite);

      Procedure Flush(Const Projection:Matrix4x4);

      //Procedure SetupSaturationCombiners(Var Slot:Integer);
  End;

  SpriteManager = Class(ApplicationComponent)
    Protected
      _NullSprite:Sprite;
      
      _Index:Integer;
      _SpriteCount:Integer;
      _Sprites:Array Of Sprite;

      _Batches:Array Of SpriteBatch;
      _BatchCount:Integer;

      _CurrentShader:ShaderInterface;
      _SpriteShaderWithoutGrading:ShaderInterface;
      {$IFNDEF DISABLECOLORGRADING}
      _SpriteShaderWithGrading:ShaderInterface;
      {$ENDIF}
      _FontShader:ShaderInterface;

      _Vertices:VertexData;

      Procedure Clear;

      Procedure Init; Override;

      Procedure SetShader(Const Projection:Matrix4x4; MyShader:ShaderInterface);

      Procedure Flush(Const Projection:Matrix4x4);

   Public
      Class Function Instance:SpriteManager;

      Procedure Release; Override;

      Procedure Render(Const Projection:Matrix4x4);

      Procedure QueueSprite(S:Sprite);

      Function DrawSprite(X,Y,Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend; Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):QuadSprite;
      Function DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:TERRATexture; Outline:Color; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):QuadSprite;

      Property FontShader:ShaderInterface Read _FontShader;
  End;

Function CreateSpriteVertexData(Count:Integer):VertexData;

Implementation
Uses TERRA_ResourceManager, TERRA_UI, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  BatchSize = 128;

Var
  _SpriteManager_Instance:ApplicationObject = Nil;

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

Function GetShader_Sprite(DoColorGrading, IsFont:Boolean):TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying mediump float saturation;');
	Line('  varying lowp vec4 color;');
  Line('  attribute highp vec3 terra_position;');
  Line('  attribute mediump vec2 terra_UV0;');
  Line('  attribute mediump vec4 terra_UV1;');
  Line('  attribute mediump float terra_UV2;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  uniform mat4 projectionMatrix;');

	Line('void main()	{');
  Line('  vec4 local_position = vec4(terra_position.x, terra_position.y, terra_position.z, 1.0);');
  Line('  screen_position = local_position.xy;');
  Line('  gl_Position =  projectionMatrix * local_position;');
  Line('  texCoord = terra_UV0;');
  Line('  clipRect = terra_UV1;');
  Line('  saturation = terra_UV2;');
  Line('  color = terra_color;}');
  Line('}');

  Line('fragment {');
	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying mediump float saturation;');
	Line('  varying lowp vec4 color;');
	Line('  uniform sampler2D texture;');

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

  Line('    lowp vec4 c = color * texture2D(texture, texCoord.xy);');
(*  {$IFNDEF DISABLECOLORGRADING}
  If (DoColorGrading) Then
    Line('    c.rgb = ColorTableLookup(c.rgb);');
  {$ENDIF}
  Line('    c.rgb = AdjustSaturation(c.rgb, saturation); ');

 Line('    c.rgb = vec3(1.0, 0.0, 1.0);');
  //Line('    if (c.a<0.1) discard;');
 // Line('    c.rgb *= 0.0;');
//  Line('    c.rgb += vec3(1.0, 0.0, 0.0);');*)
  Line('    gl_FragColor = c;}');
  Line('}  ');
  Result := S;
End;

(*
(*  If IsFont Then
  Begin
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
  End Else

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

  {$IFDEF DISTANCEFIELDFONTS}
  Line('  const float smoothing = 1.0/16.0;');
  Line('  const float outlineWidth = 5.0/16.0;');
  Line('  const float outerEdgeCenter = 0.5 - outlineWidth;');
  {$ENDIF}

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

Function CreateSpriteVertexData(Count:Integer):VertexData;
Const
  SpriteVertexFormat = [vertexFormatPosition, vertexFormatColor, vertexFormatUV0, vertexFormatUV1, vertexFormatUV2];
Begin
  Result := VertexData.Create(SpriteVertexFormat, Count);
  Result.SetAttributeFormat(vertexUV1, typeVector4D);
  Result.SetAttributeFormat(vertexUV2, typeFloat);
End;


{ SpriteManager }
Class Function SpriteManager.Instance:SpriteManager;
Begin
  If Not Assigned(_SpriteManager_Instance) Then
    _SpriteManager_Instance := InitializeApplicationComponent(SpriteManager, GraphicsManager);

  Result := SpriteManager(_SpriteManager_Instance.Instance);
End;


Function SpriteManager.DrawSprite(X,Y,Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; Shader:ShaderInterface): QuadSprite;
Begin
  Result := Self.DrawSpriteWithOutline(X,Y,Layer, SpriteTexture, ColorNull, ColorTable, BlendMode,  Saturation, Filter, Shader);
End;

Function SpriteManager.DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:TERRATexture; Outline:Color; ColorTable:TERRATexture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; Shader:ShaderInterface):QuadSprite;
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
  Result._ColorTable := ColorTable;
  {$ENDIF}
  Result.Anchor := VectorCreate2D(0, 0);
  Result.Flip := False;
  Result.Mirror := False;
  Result.ClipRect.Style := clipNothing;
  Result._Saturation := Saturation;
  Result._Shader := Shader;
  Result._Outline := Outline;

  Result._Transform := MatrixIdentity3x3;

  SpriteTexture.Filter := Filter;
//  SpriteTexture.Wrap := True;
  SpriteTexture.MipMapped := False;

  Result._A := ColorWhite;
  Result._B := ColorWhite;
  Result._C := ColorWhite;
  Result._D := ColorWhite;

  Result.Texture := SpriteTexture;
  Result._BlendMode := BlendMode;

  Result.Rect.U1 := 0.0;
  Result.Rect.V1 := 0.0;
  Result.Rect.U2 := 1.0;
  Result.Rect.V2 := 1.0;

  Result._ScrollU := 0.0;
  Result._ScrollV := 0.0;

  Self.QueueSprite(Result);
End;

Procedure SpriteManager.QueueSprite(S:Sprite);
Var
  N, I:Integer;
  TargetLayer:Single;
  HasShaders, ResetBatch:Boolean;
Begin
  HasShaders := GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable;

  ResetBatch := True;

  TargetLayer := S.Layer * 0.01;

  N := -1;
  For I:=0 To Pred(_BatchCount) Do
  If ((_Batches[I]._Texture = S._Texture) And (_Batches[I]._BlendMode = S._BlendMode)
  {$IFNDEF DISABLECOLORGRADING}And (_Batches[I]._ColorTable = S._ColorTable){$ENDIF}
  And (_Batches[I]._Shader = S._Shader)
  And ( (HasShaders) Or (_Batches[I]._Saturation = S._Saturation))
  And (Cardinal(_Batches[I]._Outline) = Cardinal(S._Outline))
  And (_Batches[I]._Count<BatchSize)) And (_Batches[I]._Layer = TargetLayer)
  And (Not _Batches[I]._Closed) Then
  Begin
    N := I;
    ResetBatch := False;
    Break;
  End;

  If (N<0) Then
  Begin
    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._Count <=0) Then
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
    _Batches[N]._Count := 0;
    _Batches[N]._BlendMode := S._BlendMode;
    _Batches[N]._Texture := S._Texture;
    _Batches[N]._Closed := False;
    _Batches[N]._Layer := TargetLayer;
    {$IFNDEF DISABLECOLORGRADING}
    _Batches[N]._ColorTable := S._ColorTable;
    {$ENDIF}
    _Batches[N]._Saturation := S._Saturation;
    _Batches[N]._Shader := S._Shader;
    _Batches[N]._Outline := S._Outline;
    _Batches[N]._First := Nil;
    _Batches[N]._Manager := Self;
  End;

  _Batches[N].AddSprite(S);
End;

Procedure SpriteManager.Clear;
Begin
  _Index := -1;
End;

Procedure SpriteManager.Init;
Var
    I:Integer;
Begin
  _SpriteCount := 2000;
  SetLength(_Sprites, _SpriteCount);
  For I:=0 To Pred(_SpriteCount) Do
    _Sprites[I] := QuadSprite.Create;
  _Index := -1;
  _BatchCount := 50;

  _Vertices := CreateSpriteVertexData(6 * BatchSize);

  SetLength(_Batches, _BatchCount);
End;

Procedure SpriteManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._SpriteCount) Do
    ReleaseObject(_Sprites[I]);

  ReleaseObject(_Vertices);
  ReleaseObject(_NullSprite);

  ReleaseObject(_FontShader);
  ReleaseObject(_CurrentShader);
  ReleaseObject(_SpriteShaderWithoutGrading);
  {$IFNDEF DISABLECOLORGRADING}
  ReleaseObject(_SpriteShaderWithGrading);
  {$ENDIF}

  _SpriteManager_Instance := Nil;
End;

Function GreyTransform(S:Color):Color; CDecl;
Begin
  Result := ColorGrey(ColorLuminance(S));
End;

Procedure SpriteManager.Render;
Var
  I,K:Integer;
  Min:Single;
  Total, Index, Count:Integer;
  M:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  If InputManager.Instance.Keys.IsDown(keyF6) Then
    Exit;


  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Render');{$ENDIF}

  Graphics := GraphicsManager.Instance;
  Graphics.Renderer.SetBlendMode(blendNone);

  //glEnable(GL_DEPTH_TEST); {FIXME}
  Graphics.Renderer.SetDepthFunction(compareLessOrEqual);

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

  If (_SpriteShaderWithoutGrading = Nil) Then
  Begin
    _SpriteShaderWithoutGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithoutGrading.Generate('Sprite', GetShader_Sprite(False, False));
  End;

  {$IFNDEF DISABLECOLORGRADING}
  If (_SpriteShaderWithGrading = Nil) Then
  Begin
    _SpriteShaderWithGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithGrading.Generate('SpriteGrading', GetShader_Sprite(True, False));
  End;
  {$ENDIF}

  If (_FontShader = Nil) Then
  Begin
    _FontShader := Graphics.Renderer.CreateShader();
    _FontShader.Generate('Font', GetShader_Sprite(False, True));
  End;

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
    If (Assigned(_Batches[I]._First)) And (_Batches[I]._Layer<Min) Then
    Begin
      Min := _Batches[I]._Layer;
      Index := I;
    End;

    If (Index>=0) Then
    Begin
      _Batches[Index].Flush(Projection);
      Dec(Total);

      Inc(Count); //If Count>1 Then break;
    End Else
    Break;
  End;


  Graphics.Renderer.SetDepthTest(True);    //BIBI

  Self.Clear();

  (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;*)

  _CurrentShader := Nil;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure SpriteManager.Flush;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BatchCount) Do
    _Batches[I]._Closed := True;
End;

Procedure SpriteManager.SetShader(Const Projection:Matrix4x4; MyShader: ShaderInterface);
Var
  Graphics:GraphicsManager;
Begin
  If (_CurrentShader = MyShader) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetShader');{$ENDIF}

  Graphics := GraphicsManager.Instance;

  _CurrentShader := MyShader;

  {If Not MyShader.IsReady() Then BIBI
    Exit;}

  Graphics.Renderer.BindShader(MyShader);

  MyShader.SetIntegerUniform('texture', 0);
  Graphics.Renderer.SetModelMatrix(Matrix4x4Identity);
  Graphics.Renderer.SetProjectionMatrix(Projection);

  {If (MyShader = _FontShader) Then
    IntToString(2);}

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SpriteVertex));
  Graphics.Renderer.SetAttributeSource('terra_position', typeVector3D, @(_Vertices[0].Position));
  Graphics.Renderer.SetAttributeSource('terra_UV0', typeVector3D, @(_Vertices[0].TexCoord));
  Graphics.Renderer.SetAttributeSource('terra_color', typeColor, @(_Vertices[0].Color));
  Graphics.Renderer.SetAttributeSource('terra_saturation', typeFloat, @(_Vertices[0].Saturation));}

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

{ Sprite }
Procedure Sprite.Release();
Begin
  ReleaseObject(_Vertices);
End;

Procedure Sprite.SetTransform(Const Mat: Matrix3x3);
Begin
  _Transform := Mat;
End;

Procedure Sprite.SetTransform(Const Center:Vector2D; Const Mat: Matrix3x3);
Begin
  SetTransform(MatrixTransformAroundPoint2D(Center, Mat));
End;

Procedure Sprite.SetScaleAndRotation(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single);
Var
  Mat:Matrix3x3;
Begin
  Mat := MatrixRotationAndScale2D(Rotation, ScaleX, ScaleY);
  SetTransform(Center, Mat);
End;

Procedure Sprite.SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single);
Begin
  SetScaleAndRotation(Center, ScaleX, ScaleY, 0.0);
End;

Procedure Sprite.ConcatTransform(const Mat: Matrix3x3);
Begin
  Self._Transform := MatrixMultiply3x3(_Transform, Mat);
End;

Procedure Sprite.SetScaleAndRotation(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotation(Center, Scale, Scale, Rotation);
End;

{ QuadSprite }
Procedure QuadSprite.SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);
Var
  Dest:Vector2D;
  W,H:Single;
Begin
  If (Self.Texture = Nil) Then
  Begin
    SetTransform(Self.Position, Mat);
  End Else
  Begin
    If Rect.Width>0 Then
      W := Rect.Width
    Else
      W := Self.Texture.Width;

    If Rect.Height>0 Then
      H := Rect.Height
    Else
      H := Self.Texture.Height;

    Dest.X := Self.Position.X + Center.X * W;
    Dest.Y := Self.Position.Y + Center.Y * H;
    SetTransform(Dest, Mat)
  End;
End;

Procedure QuadSprite.SetScaleAndRotation(ScaleX, ScaleY, Rotation:Single);
Begin
  SetScaleAndRotation(Self.Position, ScaleX, ScaleY, Rotation);
End;

Procedure QuadSprite.SetScale(ScaleX, ScaleY:Single);
Begin
  SetScale(Self.Position, ScaleX, ScaleY);
End;

Procedure QuadSprite.SetScale(Scale: Single);
Begin
  SetScale(Scale, Scale);
End;

Procedure QuadSprite.SetScaleAndRotation(Scale, Rotation: Single);
Begin
  SetScaleAndRotation(Scale, Scale, Rotation);
End;

ProcedurE QuadSprite.SetScaleAndRotationRelative(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single);
Var
  Mat:Matrix3x3;
Begin
  Mat := MatrixRotationAndScale2D(Rotation, ScaleX, ScaleY);
  SetTransformRelative(Center, Mat);
End;

ProcedurE QuadSprite.SetScaleRelative(Const Center:Vector2D; ScaleX, ScaleY:Single);
Begin
  SetScaleAndRotationRelative(Center, ScaleX, ScaleY, 0.0);
End;

Procedure QuadSprite.SetScroll(U, V: Single);
Begin
  If (U>1) Or (U<-1) Then
    U := Frac(U);

  If (V>1) Or (V<-1) Then
    V := Frac(V);

  Self._ScrollU := U;
  Self._ScrollV := V;
End;

Procedure QuadSprite.SetColor(Const C:Color);
Begin
  _A := C;
  _B := C;
  _C := C;
  _D := C;
End;

Procedure QuadSprite.SetColors(Const A, B, C, D:Color);
Begin
  _A := A;
  _B := B;
  _C := C;
  _D := D;
End;

Procedure QuadSprite.SetAlpha(Alpha: Byte);
Begin
  _A.A := Alpha;
  _B.A := Alpha;
  _C.A := Alpha;
  _D.A := Alpha;
End;

Procedure QuadSprite.SetScaleAndRotationRelative(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotationRelative(Center, Scale, Scale, Rotation);
End;

Procedure QuadSprite.SetScaleRelative(const Center: Vector2D; Scale: Single);
Begin
  SetScaleRelative(Center, Scale, Scale);
End;


Procedure QuadSprite.Rebuild;
Var
  K:Single;
  Pos:Vector2D;
  Width, Height:Integer;
Begin
  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(6);

  Width := Self.Rect.Width;
  Height := Self.Rect.Height;

  If (Width<=0) Then
    Width := Trunc((Self.Rect.U2-Self.Rect.U1) * (_Texture.Width / _Texture.Ratio.X));
  If (Height<=0) Then
    Height := Trunc((Self.Rect.V2-Self.Rect.V1) * (_Texture.Height / _Texture.Ratio.Y));

  Pos.X := Position.X - Anchor.X * Width;
  Pos.Y := Position.Y - Anchor.Y * Height;

  If (Self.Mirror) Then
  Begin
    K := Self.Rect.U1;
    Self.Rect.U1 := Self.Rect.U2;
    Self.Rect.U2 := K;
  End;

  If (Self.Texture.Origin = surfaceBottomRight) Then
    Self.Flip := Not Self.Flip;

  If (Self.Flip) Then
  Begin
    K := Self.Rect.V1;
    Self.Rect.V1 := Self.Rect.V2;
    Self.Rect.V2 := K;
  End;

  Self.Rect.U1 := Self.Rect.U1 + Self._ScrollU;
  Self.Rect.U2 := Self.Rect.U2 + Self._ScrollU;
  Self.Rect.V1 := Self.Rect.V1 + Self._ScrollV;
  Self.Rect.V2 := Self.Rect.V2 + Self._Scrollv;

  _Vertices.SetColor(0, vertexColor, _C);
  _Vertices.SetColor(1, vertexColor, _D);
  _Vertices.SetColor(2, vertexColor, _B);
  _Vertices.SetColor(4, vertexColor, _A);

  _Vertices.SetVector3D(0, vertexPosition, VectorCreate(Pos.X, Pos.Y + Height, 0));
  _Vertices.SetVector2D(0, vertexUV0, VectorCreate2D(Self.Rect.U1, Self.Rect.V2));

  _Vertices.SetVector3D(1, vertexPosition, VectorCreate(Pos.X + Width, Pos.Y +Height, 0));
  _Vertices.SetVector2D(1, vertexUV0, VectorCreate2D(Self.Rect.U2, Self.Rect.V2));

  _Vertices.SetVector3D(2, vertexPosition, VectorCreate(Pos.X + Width, Pos.Y, 0));
  _Vertices.SetVector2D(2, vertexUV0, VectorCreate2D(Self.Rect.U2, Self.Rect.V1));

  _Vertices.SetVector3D(4, vertexPosition, VectorCreate(Pos.X, Pos.Y, 0));
  _Vertices.SetVector2D(4, vertexUV0, VectorCreate2D(Self.Rect.U1, Self.Rect.V1));

  _Vertices.CopyVertex(2, 3);
  _Vertices.CopyVertex(0, 5);
End;

{ SpriteBatch }
Procedure SpriteBatch.AddSprite(P: Sprite);
Var
  S, Prev:Sprite;
Begin
  Inc(_Count);
  P._Next := _First;
  _First := P;
End;


Procedure SpriteBatch.Flush(Const Projection:Matrix4x4);
Var
  I, J:Integer;
  S:Sprite;
  K:Single;
  MaxX,MinX, MaxY,MinY:Single;
  M:Matrix3x3;
  C:Color;
  InIt, OutIt:VertexIterator;
  Src, Dest:SpriteVertex;
  Ofs:Integer;
  FullyClipped:Boolean;
  Ratio:Single;
  Pos:Vector3D;
  Graphics:GraphicsManager;
  CurrentClip:Vector4D;
Begin
  _Closed := False;
  If (_Count<=0) Then
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
    _Manager.SetShader(Projection, Self._Shader);

    {$IFDEF DISTANCEFIELDFONTS}
    _Manager._FontShader.SetColorUniform('outline', _Outline);
    {$ENDIF}
  End Else
  {$IFNDEF DISABLECOLORGRADING}
  If (Assigned(Self._ColorTable)) Then
    _Manager.SetShader(Projection, _Manager._SpriteShaderWithGrading)
  Else
  {$ENDIF}
    _Manager.SetShader(Projection, _Manager._SpriteShaderWithoutGrading);

  If Not _Texture.Bind(0) Then
    Exit;

  {$IFNDEF DISABLECOLORGRADING}
  If (Self._Shader = Nil) Then
    ColorTableBind(_ColorTable, 1);
  {$ENDIF}

  Graphics.Renderer.SetBlendMode(_BlendMode);

//  Ratio := UIManager.Instance.Ratio;

  OutIt := SpriteManager.Instance._Vertices.GetIteratorForClass(SpriteVertex);

  Ofs := 0;
  S := _First;
  While Assigned(S) Do
  Begin
    If (S.ClipRect.Style = clipEverything) Then
      Continue;

    S.Rebuild();

    If (S.ClipRect.Style = clipNothing) Then
      CurrentClip := VectorCreate4D(0, 0, 9999, 9999)
    Else
      CurrentClip := VectorCreate4D(S.ClipRect.X, S.ClipRect.Y, S.ClipRect.X + S.ClipRect.Width, S.ClipRect.Y + S.ClipRect.Height);

    MinX := 9999;
    MaxX := -9999;
    MinY := 9999;
    MaxY := -9999;

    If Not OutIt.Seek(Ofs) Then
      Break;

    InIt := S._Vertices.GetIteratorForClass(SpriteVertex);
    While (InIt.HasNext()) And (OutIt.HasNext()) Do
    Begin
      Src := SpriteVertex(InIt.Value);
      Dest := SpriteVertex(OutIt.Value);

      Pos := Src.Position;

      Pos := S._Transform.Transform(Pos);
      Pos.Z := S.Layer;

      Dest.Position := Pos;
      Dest.Saturation := S._Saturation;
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
      Inc(Ofs, S._Vertices.Count);

    S := S._Next;
  End;
  ReleaseObject(OutIt);

  Graphics.Renderer.SetVertexSource(SpriteManager.Instance._Vertices);
  Graphics.Renderer.DrawSource(renderTriangles, Ofs);

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

  _Count := 0;
  _Texture := Nil;
  _First := Nil;
End;

{ TextureRect }
Procedure TextureRect.UVRemap(_U1, _V1, _U2, _V2:Single);
Begin
  Self.U1 := _U1;
  Self.V1 := _V1;
  Self.U2 := _U2;
  Self.V2 := _V2;
End;

Procedure TextureRect.FullRemap;
Begin
  Self.U1 := 0.0;
  Self.V1 := 0.0;
  Self.U2 := 1.0;
  Self.V2 := 1.0;
End;

Procedure TextureRect.PixelRemap(X1, Y1, X2, Y2:Integer; Tex:TERRATexture;  W, H: Integer);
Begin
  If (Tex = Nil) Then
    Exit;

  Tex.Prefetch();

  U1 := (X1/Tex.Width * Tex.Ratio.X);
  V1 := (Y1/Tex.Height * Tex.Ratio.Y);
  U2 := (X2/Tex.Width * Tex.Ratio.X);
  V2 := (Y2/Tex.Height * Tex.Ratio.Y);

  If (W>0) Then
    Self.Width := W
  Else
    Self.Width := IntMax(1, (Abs(X2-X1)));

  If (H>0) Then
    Self.Height := H
  Else
    Self.Height := IntMax(1, (Abs(Y2-Y1)));
End;

Procedure TextureRect.TileRemapByID(TileID, TilesPerRow, TileSize:Integer; Tex:TERRATexture);
Var
  TX, TY:Integer;
  PX, PY:Single;
Begin
  If (Tex = Nil) Then
    Exit;

  Tex.Prefetch();

  PX := (1/(Tex.Width / Tex.Ratio.X));
  PY := (1/(Tex.Height / Tex.Ratio.Y));

  TX := (TileID Mod TilesPerRow);
  TY := (TileID Div TilesPerRow);
  U1 := (TX/TilesPerRow + PX) * Tex.Ratio.X;
  U2 := (Succ(TX)/TilesPerRow - PX) * Tex.Ratio.X;
  V1 := (TY/TilesPerRow + PY) * Tex.Ratio.Y;
  V2 := (Succ(TY)/TilesPerRow - PY) * Tex.Ratio.Y;

  Width := TileSize;
  Height := TileSize;
End;

Procedure TextureRect.TileRemap(X, Y, TilesPerX, TilesPerY: Integer; Tex:TERRATexture);
Var
  SX, SY:Single;
  TX,TY:Single;
Begin
  If (Tex = Nil) Then
    Exit;

  Tex.Prefetch();

  SX := (Tex.Width / Tex.Ratio.X) / TilesPerX;
  SY := (Tex.Height / Tex.Ratio.Y) / TilesPerY;
  TX := SX*X;
  TY := SY*Y;
  U1 := (TX/Tex.Width * Tex.Ratio.X);
  V1 := (TY/Tex.Height * Tex.Ratio.Y);
  U2 := (((TX+SX)-1)/Tex.Width * Tex.Ratio.X);
  V2 := (((TY+SY)-1)/Tex.Height * Tex.Ratio.Y);
  Self.Width := Trunc(SX);
  Self.Height := Trunc(SY);
End;

Procedure TextureRect.ResizeWithWidth(W: Single);
Var
  N:Single;
Begin
  If (Height>0) Then
    N := Width/Height
  Else
    N := 1;

  Width := Trunc(W);
  Height := Trunc(Width * N);
End;

Procedure TextureRect.ResizeWithHeight(H: Single);
Var
  N:Single;
Begin
  If (Height>0) Then
    N := Width/Height
  Else
    N := 1;

  Height := Trunc(H);
  Width := Trunc(Height / N);
End;


{ SpriteVertex }
Procedure SpriteVertex.Load;
Begin
  Self.GetVector3D(vertexPosition, Position);
  Self.GetColor(vertexColor, Color);
  Self.GetVector2D(vertexUV0, TexCoord);
  Self.GetVector4D(vertexUV1, ClipRect);
  Self.GetFloat(vertexUV2, Saturation);
End;

Procedure SpriteVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Position);
  Self.SetColor(vertexColor, Color);
  Self.SetVector2D(vertexUV0, TexCoord);
  Self.SetVector4D(vertexUV1, ClipRect);
  Self.SetFloat(vertexUV2, Saturation);
  
End;

End.
