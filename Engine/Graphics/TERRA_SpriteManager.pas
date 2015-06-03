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
  TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_GraphicsManager, TERRA_Texture,
  TERRA_Application, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect,
  TERRA_Renderer, TERRA_InputManager, TERRA_VertexFormat;

Const
  vertexFormatSaturation = vertexFormatUV1;
  vertexSaturation = vertexUV1;

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
      Saturation:Single;
  End;

  TextureRect = Object
    Public
      Texture:Texture;
      Width:Integer;
      Height:Integer;
      U1,V1:Single;
      U2,V2:Single;

      Procedure ResizeWithWidth(W:Single);
      Procedure ResizeWithHeight(H:Single);

      Procedure TileRemap(X,Y, TilesPerX, TilesPerY:Integer);
      Procedure TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
      Procedure PixelRemap(X1,Y1, X2, Y2:Integer; W:Integer=0; H:Integer=0);
      Procedure UVRemap(_U1,_V1, _U2, _V2:Single);

      Procedure FullRemap();
  End;

  Sprite = Class(TERRAObject)
    Protected
      _Next:Sprite;

      _BlendMode:Integer;
      _Outline:Color;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:Texture;
      {$ENDIF}

      _Transform:Matrix3x3;
      _IsFont:Boolean;

      _Saturation:Single;

      _Texture:Texture;

      _Vertices:VertexData;

      _USize:Single;
      _VSize:Single;

      _Width:Single;
      _Height:Single;

      Procedure Rebuild(); Virtual; Abstract;

    Public
      Position:Vector2D;
      Anchor:Vector2D;

      Layer:Single;
      ClipRect:ClipRect;

      Procedure Release; Override;
      
      Procedure SetColor(C:Color); Virtual; Abstract;

      Procedure SetTransform(Const Mat:Matrix3x3); Overload;
      Procedure SetTransform(Const Center:Vector2D; Const Mat:Matrix3x3); Overload;

      Procedure SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;
      Procedure SetScale(ScaleX, ScaleY:Single); Overload;
      Procedure SetScale(Scale:Single); Overload;

      Procedure SetScaleAndRotation(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Const Center:Vector2D; Scale:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Scale:Single; Rotation:Single); Overload;

      Procedure ConcatTransform(Const Mat:Matrix3x3);

      Property Texture:TERRA_Texture.Texture Read _Texture;
  End;

  QuadSprite = Class(Sprite)
    Protected
      _ScrollU:Single;
      _ScrollV:Single;

      _A, _B, _C, _D:Color;

      Procedure Rebuild(); Override;

    Public
      Mirror:Boolean;
      Flip:Boolean;

      Skew:Single;

      Rect:TextureRect;

      Procedure SetColor(C:Color); Override;
      Procedure SetColors(A, B, C, D:Color);
      Procedure SetAlpha(Alpha:Byte);

      Procedure SetScroll(U,V:Single);

      Procedure SetScaleRelative(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;
      Procedure SetScaleRelative(Const Center:Vector2D; Scale:Single); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single ); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; Scale:Single; Rotation:Single ); Overload;
      Procedure SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);

      Property Transform:Matrix3x3 Read _Transform;
  End;

  SpriteBatch = Object
    Protected
      _First:Sprite;
      _Count:Integer;
      _Texture:Texture;

      _Manager:SpriteManager;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:Texture;
      {$ENDIF}

      _BlendMode:Integer;
      _Layer:Single;
      _Closed:Boolean;
      _IsFont:Boolean;
      _Saturation:Single;
      _Outline:Color;

      Procedure AddSprite(P:Sprite);

      Procedure Flush;

      //Procedure SetupSaturationCombiners(Var Slot:Integer);
  End;

  SpriteManager = Class(ApplicationComponent)
    Protected
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

      Procedure SetShader(MyShader:ShaderInterface);

   Public
      Class Function Instance:SpriteManager;

      Procedure Release; Override;

      Procedure Render;

      Procedure EnableSpriteShader(ColorGrading:Boolean);
      Procedure EnableFontShader();

      Procedure Flush;

      Function DrawSprite(X,Y,Layer:Single; SpriteTexture:Texture; ColorTable:Texture = Nil; BlendMode:Integer = blendBlend; Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; IsFont:Boolean = False):QuadSprite;
      Function DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:Texture; Outline:Color; ColorTable:Texture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; IsFont:Boolean = False):QuadSprite;
  End;


Implementation
Uses TERRA_ResourceManager, TERRA_UI, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  BatchSize = 128;

Var
  _SpriteManager_Instance:ApplicationObject = Nil;
  _NullSprite:Sprite;

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

Function GetShader_Sprite(DoColorGrading:Boolean):TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  varying lowp float saturation;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec3 terra_UV0;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  attribute lowp float terra_saturation;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  texCoord = terra_UV0.xy;');
  Line('  saturation = terra_saturation;');
  Line('  color = terra_color;}');
  Line('}');
  Line('fragment {');
	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  varying lowp float saturation;');
	Line('  uniform sampler2D texture;');

  Line(GetSaturationAndConstrast());

  {$IFNDEF DISABLECOLORGRADING}
  If (DoColorGrading) Then
    Line(GetColorTableShaderCode());
  {$ENDIF}

	Line('  void main()	{');
  Line('    lowp vec4 c = color * texture2D(texture, texCoord.st);');
  {$IFNDEF DISABLECOLORGRADING}
  If (DoColorGrading) Then
    Line('    c.rgb = ColorTableLookup(c.rgb);');
  {$ENDIF}
  Line('    c.rgb = AdjustSaturation(c.rgb, saturation); ');

//  Line('    c.rgb = vec3(1.0, 0.0, 1.0);');
  //Line('    if (c.a<0.1) discard;');
 // Line('    c.rgb *= 0.0;');
//  Line('    c.rgb += vec3(1.0, 0.0, 0.0);');
  Line('    gl_FragColor = c;}');
  Line('}  ');
  Result := S;
End;

Function GetShader_Font():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  varying lowp float saturation;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec3 terra_UV0;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  attribute lowp float terra_saturation;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  texCoord = terra_UV0.xy;');
  Line('  saturation = terra_saturation;');
  Line('  color = terra_color;}');
  Line('}');
  Line('fragment {');
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


  Line('    float distance = texture2D(texture, texCoord.st).a;');
  Line('    float alpha = smoothstep(outerEdgeCenter - smoothing, outerEdgeCenter + smoothing, distance);');
  Line('    float border = smoothstep(0.5 - smoothing, 0.5 + smoothing, distance);');
  Line('    gl_FragColor = vec4( mix(outline.rgb, color.rgb, border), alpha );');

  {$ELSE}

  Line('    lowp float mask = texture2D(texture, texCoord.st).a;');
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
End;

Procedure ClipVertex(V:SpriteVertex; Clip:ClipRect; Width, Height, USize, VSize:Single{; Landscape:Boolean});
Var
  X1,X2,Y1,Y2:Single;
  Dist, P:Single;
Begin
  If (Width=0) Or (Height=0) Then
    Exit;

  Clip.GetRealRect(X1, Y1, X2, Y2{, Landscape});

  If (Clip.Style = clipEverything) Then
  Begin
    V.Position.X := X1;
    V.Position.Y := Y1;
    Exit;
  End;

//  Y2 := 100;

  If (V.Position.X>X2) Then
  Begin
    Dist := (V.Position.X-X2)/Width;
    V.TexCoord.X := V.TexCoord.X - USize * Dist;
    V.Position.X := X2;
  End Else
  If (V.Position.X<X1) Then
  Begin
    Dist := (X1 - V.Position.X)/Width;
    V.TexCoord.X := V.TexCoord.X + USize * Dist;
    V.Position.X := X1;
  End;

  If (V.Position.Y>Y2) Then
  Begin
    Dist := (V.Position.Y-Y2)/Height;
    V.TexCoord.Y := V.TexCoord.Y - VSize * Dist;
    V.Position.Y := Y2;
  End Else
  If (V.Position.Y<Y1) Then
  Begin
    Dist := (Y1 - V.Position.Y)/Height;
    V.TexCoord.Y := V.TexCoord.Y + VSize * Dist;
    V.Position.Y := Y1;
  End;
End;

Function CreateSpriteVertexData(Count:Integer):VertexData;
Const
  SpriteVertexFormat = [vertexFormatPosition, vertexFormatColor, vertexFormatUV0, vertexFormatSaturation];
Begin
  Result := VertexData.Create(SpriteVertexFormat, Count);
  Result.SetAttributeName(vertexSaturation, 'terra_saturation');
  Result.SetAttributeFormat(vertexSaturation, typeFloat);
End;


{ SpriteManager }
Class Function SpriteManager.Instance:SpriteManager;
Begin
  If Not Assigned(_SpriteManager_Instance) Then
    _SpriteManager_Instance := InitializeApplicationComponent(SpriteManager, GraphicsManager);

  Result := SpriteManager(_SpriteManager_Instance.Instance);
End;


Function SpriteManager.DrawSprite(X,Y,Layer:Single; SpriteTexture:Texture; ColorTable:Texture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; IsFont:Boolean): QuadSprite;
Begin
  Result := Self.DrawSpriteWithOutline(X,Y,Layer, SpriteTexture, ColorNull, ColorTable, BlendMode,  Saturation, Filter, IsFont);
End;

Function SpriteManager.DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:Texture; Outline:Color; ColorTable:Texture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; IsFont:Boolean):QuadSprite;
Var
  N, I:Integer;
  HasShaders, ResetBatch:Boolean;
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

  Layer := Layer/100;

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
  Result.Skew := 0;
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
  Result._IsFont := IsFont;
  Result._Outline := Outline;

  Result._Transform := MatrixIdentity3x3;

  SpriteTexture.Filter := Filter;
//  SpriteTexture.Wrap := True;
  SpriteTexture.MipMapped := False;

  Result._A := ColorWhite;
  Result._B := ColorWhite;
  Result._C := ColorWhite;
  Result._D := ColorWhite;
  Result.Rect.Texture := SpriteTexture;
  Result._BlendMode := BlendMode;

  Result.Rect.U1 := 0.0;
  Result.Rect.V1 := 0.0;
  Result.Rect.U2 := 1.0;
  Result.Rect.V2 := 1.0;

  Result._ScrollU := 0.0;
  Result._ScrollV := 0.0;

  HasShaders := GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable;

  ResetBatch := True;

  N := -1;
  For I:=0 To Pred(_BatchCount) Do
  If ((_Batches[I]._Texture = SpriteTexture) And (_Batches[I]._BlendMode = BlendMode)
  {$IFNDEF DISABLECOLORGRADING}And (_Batches[I]._ColorTable = ColorTable){$ENDIF}
  And (_Batches[I]._IsFont = IsFont)
  And ( (HasShaders) Or (_Batches[I]._Saturation = Saturation))
  And (Cardinal(_Batches[I]._Outline) = Cardinal(Outline))
  And (_Batches[I]._Count<BatchSize)) And (_Batches[I]._Layer = Result.Layer)
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
    _Batches[N]._BlendMode := BlendMode;
    _Batches[N]._Texture := SpriteTexture;
    _Batches[N]._Closed := False;
    _Batches[N]._Layer := Result.Layer;
    {$IFNDEF DISABLECOLORGRADING}
    _Batches[N]._ColorTable := ColorTable;
    {$ENDIF}
    _Batches[N]._Saturation := Saturation;
    _Batches[N]._IsFont := IsFont;
    _Batches[N]._Outline := Outline;
    _Batches[N]._First := Nil;
    _Batches[N]._Manager := Self;
  End;

  _Batches[N].AddSprite(Result);
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
  Projection, M:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  If InputManager.Instance.Keys.IsDown(keyF6) Then
    Exit;


  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Render');{$ENDIF}

  Graphics := GraphicsManager.Instance;
  Graphics.Renderer.SetBlendMode(blendNone);

//  glDisable(GL_DEPTH_TEST); {FIXME}
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
    _SpriteShaderWithoutGrading.Generate('Sprite', GetShader_Sprite(False)); 
  End;

  {$IFNDEF DISABLECOLORGRADING}
  If (_SpriteShaderWithGrading = Nil) Then
  Begin
    _SpriteShaderWithGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithGrading.Generate('SpriteGrading', GetShader_Sprite(True));
  End;
  {$ENDIF}

  If (_FontShader = Nil) Then
  Begin
    _FontShader := Graphics.Renderer.CreateShader();
    _FontShader.Generate('Font', GetShader_Font());
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
      _Batches[Index].Flush();
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

Procedure SpriteManager.SetShader(MyShader: ShaderInterface);
Var
  Projection:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  If (_CurrentShader = MyShader) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetShader');{$ENDIF}

  Graphics := GraphicsManager.Instance;

  _CurrentShader := MyShader;

  Projection := Graphics.ProjectionMatrix;

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

Procedure SpriteManager.EnableFontShader;
Begin
  SetShader(_FontShader);
End;

Procedure SpriteManager.EnableSpriteShader(ColorGrading:Boolean);
Begin
  {$IFNDEF DISABLECOLORGRADING}
  If (ColorGrading) Then
    SetShader(_SpriteShaderWithGrading)
  Else
  {$ENDIF}
    SetShader(_SpriteShaderWithoutGrading);
End;

{ Sprite }
Procedure Sprite.Release;
Begin
  // do nothing
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

Procedure Sprite.SetScaleAndRotation(ScaleX, ScaleY, Rotation: Single);
Begin
  SetScaleAndRotation(Self.Position, ScaleX, ScaleY, Rotation);
End;

Procedure Sprite.SetScale(ScaleX, ScaleY:Single);
Begin
  SetScale(Self.Position, ScaleX, ScaleY);
End;

Procedure Sprite.ConcatTransform(const Mat: Matrix3x3);
Begin
  Self._Transform := MatrixMultiply3x3(_Transform, Mat);
End;

Procedure Sprite.SetScale(Scale: Single);
Begin
  SetScale(Scale, Scale);
End;

Procedure Sprite.SetScaleAndRotation(Scale, Rotation: Single);
Begin
  SetScaleAndRotation(Scale, Scale, Rotation);
End;

Procedure Sprite.SetScaleAndRotation(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotation(Center, Scale, Scale, Rotation);
End;

{ QuadSprite }
ProcedurE QuadSprite.SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);
Var
  Dest:Vector2D;
  W,H:Single;
Begin
  If (Rect.Texture = Nil) Then
  Begin
    SetTransform(Self.Position, Mat);
  End Else
  Begin
    If Rect.Width>0 Then
      W := Rect.Width
    Else
      W := Rect.Texture.Width;

    If Rect.Height>0 Then
      H := Rect.Height
    Else
      H := Rect.Texture.Height;

    Dest.X := Self.Position.X + Center.X * W;
    Dest.Y := Self.Position.Y + Center.Y * H;
    SetTransform(Dest, Mat)
  End;
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

Procedure QuadSprite.SetColor(C: Color);
Begin
  _A := C;
  _B := C;
  _C := C;
  _D := C;
End;

Procedure QuadSprite.SetColors(A, B, C, D:Color);
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
Begin
  _Texture := Self.Rect.Texture;

  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(6);

  _Width := Self.Rect.Width;
  _Height := Self.Rect.Height;

  If (_Width<=0) Then
    _Width := (Self.Rect.U2-Self.Rect.U1) * (_Texture.Width / _Texture.Ratio.X);
  If (_Height<=0) Then
    _Height := (Self.Rect.V2-Self.Rect.V1) * (_Texture.Height / _Texture.Ratio.Y);

  If (Self.Mirror) Then
  Begin
    K := Self.Rect.U1;
    Self.Rect.U1 := Self.Rect.U2;
    Self.Rect.U2 := K;
  End;

  If (Self.Rect.Texture.Origin = surfaceBottomRight) Then
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

  _USize := Self.Rect.U2 - Self.Rect.U1;
  _VSize := Self.Rect.V2 - Self.Rect.V1;

  _Vertices.SetColor(0, vertexColor, _C);
  _Vertices.SetColor(1, vertexColor, _D);
  _Vertices.SetColor(2, vertexColor, _B);
  _Vertices.SetColor(4, vertexColor, _A);

  _Vertices.SetVector3D(0, vertexPosition, VectorCreate(0, _Height, 0));
  _Vertices.SetVector2D(0, vertexUV0, VectorCreate2D(Self.Rect.U1, Self.Rect.V2));

  _Vertices.SetVector3D(1, vertexPosition, VectorCreate(_Width, _Height, 0));
  _Vertices.SetVector2D(1, vertexUV0, VectorCreate2D(Self.Rect.U2, Self.Rect.V2));

  _Vertices.SetVector3D(2, vertexPosition, VectorCreate(_Width + Self.Skew, 0, 0));
  _Vertices.SetVector2D(2, vertexUV0, VectorCreate2D(Self.Rect.U2, Self.Rect.V1));

  _Vertices.SetVector3D(4, vertexPosition, VectorCreate(0 + Self.Skew, 0, 0));
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


Procedure SpriteBatch.Flush;
Var
  I, J:Integer;
  S:Sprite;
  W,H, K:Single;
  MaxX,MinX, MaxY,MinY:Single;
  M:Matrix3x3;
  C:Color;
  InIt, OutIt:VertexIterator;
  Src, Dest:SpriteVertex;
  Ofs:Integer;
  Landscape, FullyClipped:Boolean;
  Ratio:Single;
  Pos:Vector3D;
  Graphics:GraphicsManager;
Begin
  _Closed := False;
  If (_Count<=0) Then
  Begin
    _First := Nil;
    Exit;
  End;

  Landscape := IsLandscapeOrientation(Application.Instance.Orientation);

  Graphics := GraphicsManager.Instance;

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SpriteVertex));
  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D,  @_Vertices[0].Position);
  Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @_Vertices[0].TexCoord);
  Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor,  @_Vertices[0].Color);

  If (_Saturation<1.0) Then
    Graphics.Renderer.SetAttributeSource(TERRA_SATURATION_ATTRIBUTE, typeFloat, @_Vertices[0].Saturation);}

  If (Self._IsFont) Then
  Begin
    _Manager.EnableFontShader();
    {$IFDEF DISTANCEFIELDFONTS}
    _Manager._FontShader.SetColorUniform('outline', _Outline);
    {$ENDIF}
  End Else
    _Manager.EnableSpriteShader({$IFDEF DISABLECOLORGRADING}False{$ELSE}Assigned(Self._ColorTable){$ENDIF});

  _Texture.Bind(0);

  {$IFNDEF DISABLECOLORGRADING}
  If (Not Self._IsFont) Then
    ColorTableBind(_ColorTable, 1);
  {$ENDIF}

  Graphics.Renderer.SetBlendMode(_BlendMode);

//  Ratio := UIManager.Instance.Ratio;

  OutIt := SpriteManager.Instance._Vertices.GetIterator(SpriteVertex);

  Ofs := 0;
  S := _First;
  While Assigned(S) Do
  Begin
    If (S.ClipRect.Style = clipEverything) Then
      Continue;

    S.Rebuild();

    MinX := 9999;
    MaxX := -9999;
    MinY := 9999;
    MaxY := -9999;

    W := S._Width;
    H := S._Height;

    OutIt.Seek(Ofs);
    InIt := S._Vertices.GetIterator(SpriteVertex);
    While (InIt.HasNext()) And (OutIt.HasNext()) Do
    Begin
      Src := SpriteVertex(InIt.Value);
      Dest := SpriteVertex(OutIt.Value);

      Pos := Src.Position;

      Pos.X := Pos.X + S.Position.X - S.Anchor.X * S._Width;
      Pos.Y := Pos.Y + S.Position.Y - S.Anchor.Y * S._Height;

      Pos := S._Transform.Transform(Pos);
      Pos.Z := S.Layer;

      Dest.Position := Pos;
      Dest.Saturation := S._Saturation;
      Dest.TexCoord := Src.TexCoord;
      Dest.Color := Src.Color;

      If (S.ClipRect.Style = clipSomething) Then
      Begin
        ClipVertex(Dest, S.ClipRect, W, H, S._USize, S._VSize{, Landscape});
      End;

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

Procedure TextureRect.PixelRemap(X1, Y1, X2, Y2, W, H: Integer);
Begin
  If (Texture = Nil) Then
    Exit;

  Texture.Prefetch();

  U1 := (X1/Texture.Width * Texture.Ratio.X);
  V1 := (Y1/Texture.Height * Texture.Ratio.Y);
  U2 := (X2/Texture.Width * Texture.Ratio.X);
  V2 := (Y2/Texture.Height * Texture.Ratio.Y);

  If (W>0) Then
    Self.Width := W
  Else
    Self.Width := IntMax(1, (Abs(X2-X1)));

  If (H>0) Then
    Self.Height := H
  Else
    Self.Height := IntMax(1, (Abs(Y2-Y1)));
End;

Procedure TextureRect.TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
Var
  TX, TY:Integer;
  PX, PY:Single;
Begin
  If (Texture = Nil) Then
    Exit;

  Texture.Prefetch();

  PX := (1/(Texture.Width / Texture.Ratio.X));
  PY := (1/(Texture.Height / Texture.Ratio.Y));

  TX := (TileID Mod TilesPerRow);
  TY := (TileID Div TilesPerRow);
  U1 := (TX/TilesPerRow + PX) * Texture.Ratio.X;
  U2 := (Succ(TX)/TilesPerRow - PX) * Texture.Ratio.X;
  V1 := (TY/TilesPerRow + PY) * Texture.Ratio.Y;
  V2 := (Succ(TY)/TilesPerRow - PY) * Texture.Ratio.Y;

  Width := TileSize;
  Height := TileSize;
End;

Procedure TextureRect.TileRemap(X, Y, TilesPerX, TilesPerY: Integer);
Var
  SX, SY:Single;
  TX,TY:Single;
Begin
  If (Texture = Nil) Then
    Exit;

  Texture.Prefetch();

  SX := (Texture.Width / Texture.Ratio.X) / TilesPerX;
  SY := (Texture.Height / Texture.Ratio.Y) / TilesPerY;
  TX := SX*X;
  TY := SY*Y;
  U1 := (TX/Texture.Width * Texture.Ratio.X);
  V1 := (TY/Texture.Height * Texture.Ratio.Y);
  U2 := (((TX+SX)-1)/Texture.Width * Texture.Ratio.X);
  V2 := (((TY+SY)-1)/Texture.Height * Texture.Ratio.Y);
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
  Self.GetFloat(vertexSaturation, Saturation);
End;

Procedure SpriteVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Position);
  Self.SetColor(vertexColor, Color);
  Self.SetVector2D(vertexUV0, TexCoord);
  Self.SetFloat(vertexSaturation, Saturation);
End;

Initialization
Finalization
  If Assigned(_NullSprite) Then
    _NullSprite.Release;
End.