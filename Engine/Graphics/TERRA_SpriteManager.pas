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
  TERRA_Application, TERRA_Shader, TERRA_Matrix3x3, TERRA_Matrix4x4;

Type
  SpriteManager = Class;

  PSpriteVertex = ^SpriteVertex;
  SpriteVertex = Packed Record
    Position:Vector3D;
    Color:Color;
    TexCoord:Vector2D;
    Saturation:Single;
  End;

  ClipRect = Class(TERRAObject)
    Protected
      _X, _Y:Single;
      _Width, _Height:Single;

      Procedure SetHeight(const Value: Single);
      Procedure SetWidth(const Value: Single);
      Procedure SetX(const Value: Single);
      Procedure SetY(const Value: Single);

    Public
      Procedure GetRealRect(Out X1, Y1, X2, Y2:Single{; Landscape:Boolean});

      Procedure Transform(Const M:Matrix3x3);

      Procedure Release; Override;

      Property X:Single Read _X Write SetX;
      Property Y:Single Read _Y Write SetY;

      Property Width:Single Read _Width Write SetWidth;
      Property Height:Single Read _Height Write SetHeight;
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
      _BlendMode:Integer;
      _Outline:Color;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:Texture;
      {$ENDIF}

      _Transform:Matrix3x3;
      _IsFont:Boolean;

      _Saturation:Single;

      _ScrollU:Single;
      _ScrollV:Single;

      _Next:Sprite;

      _A, _B, _C, _D:Color;

    Public
      Position:Vector2D;

      Layer:Single;

      Mirror:Boolean;
      Flip:Boolean;

      Anchor:Vector2D;
      Rect:TextureRect;
      ClipRect:ClipRect;

      Skew:Single;

      Procedure Release; Override;

      Procedure SetTransform(Const Mat:Matrix3x3); Overload;
      Procedure SetTransform(Const Center:Vector2D; Const Mat:Matrix3x3); Overload;

      Procedure SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;
      Procedure SetScale(ScaleX, ScaleY:Single); Overload;
      Procedure SetScale(Scale:Single); Overload;

      Procedure SetScaleAndRotation(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Const Center:Vector2D; Scale:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotation(Scale:Single; Rotation:Single); Overload;

      Procedure SetScaleRelative(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;
      Procedure SetScaleRelative(Const Center:Vector2D; Scale:Single); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single ); Overload;
      Procedure SetScaleAndRotationRelative(Const Center:Vector2D; Scale:Single; Rotation:Single ); Overload;
      Procedure SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);

      Procedure ConcatTransform(Const Mat:Matrix3x3);

      Procedure SetColors(A, B, C, D:Color);
      Procedure SetColor(C:Color);
      Procedure SetAlpha(Alpha:Byte);

      Procedure SetScroll(U,V:Single);

      Property Texture:TERRA_Texture.Texture Read Rect.Texture;

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

      Procedure SetupSaturationCombiners(Var Slot:Integer);
  End;

  SpriteManager = Class(ApplicationComponent)
    Protected
      _Index:Integer;
      _SpriteCount:Integer;
      _Sprites:Array Of Sprite;

      _Batches:Array Of SpriteBatch;
      _BatchCount:Integer;

      _CurrentShader:Shader;
      _SpriteShaderWithoutGrading:Shader;
      {$IFNDEF DISABLECOLORGRADING}
      _SpriteShaderWithGrading:Shader;
      {$ENDIF}
      _FontShader:Shader;

      Procedure Clear;


      Procedure SetShader(MyShader:Shader);

   Public
      Procedure Init; Override;

      Class Function Instance:SpriteManager;

      Procedure Release; Override;

      Procedure Render;

      Procedure EnableSpriteShader(ColorGrading:Boolean);
      Procedure EnableFontShader();

      Procedure Flush;

      Function DrawSprite(X,Y,Layer:Single; SpriteTexture:Texture; ColorTable:Texture = Nil; BlendMode:Integer = blendBlend; Saturation:Single = 1.0; BilinearFilter:Boolean=False; IsFont:Boolean = False):Sprite;
      Function DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:Texture; Outline:Color; ColorTable:Texture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; BilinearFilter:Boolean=False; IsFont:Boolean = False):Sprite;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF},
  TERRA_RenderTarget, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  BatchSize = 128 ;

Var
  _Vertices:Array[0..Pred(6*BatchSize)] Of SpriteVertex;
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

	Line('  void main()	{');
  Line('    lowp float mask = texture2D(texture, texCoord.st).a;');
  Line('    lowp float alpha;');
  Line('    if (mask<0.5) alpha = 0.0; else alpha = 1.0;');
  {$IFDEF ANDROID}
  {$IFDEF OUYA}
  Line('    alpha *= smoothstep(0.25, 0.75, mask);');// anti-aliasing
  {$ENDIF}
  {$ELSE}
  Line('    alpha *= smoothstep(0.25, 0.75, mask);');// anti-aliasing
  {$ENDIF}
  Line('    lowp vec4 baseColor;');
  {$IFDEF DISTANCEFIELDFONTS}
  Line('    if (mask>0.59) baseColor = color; else baseColor = mix(color, outline, outline.a);');
  {$ELSE}
  Line('    baseColor = color; ');
  {$ENDIF}

  //Line('    baseColor.rgb = AdjustSaturation(c.rgb, saturation); ');
  Line('    baseColor.rgb = AdjustSaturation(baseColor.rgb, saturation); ');
  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, baseColor.b, alpha * color.a);}');
//  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, 1.0, 1.0);}');
  Line('}  ');
  Result := S;
End;

Procedure ClipVertex(V:PSpriteVertex; Clip:ClipRect; Width, Height, USize, VSize:Single{; Landscape:Boolean});
Var
  X1,X2,Y1,Y2:Single;
  Dist:Single;
Begin
  If (Width=0) Or (Height=0) Then
    Exit;

  Clip.GetRealRect(X1, Y1, X2, Y2{, Landscape});

  If (V.Position.X<X1) Then
  Begin
    Dist := (X1 - V.Position.X)/Width;
    V.TexCoord.X := V.TexCoord.X + USize * Dist;
    V.Position.X := X1;
  End Else
  If (V.Position.X>X2) Then
  Begin
    Dist := (V.Position.X-X2)/Width;
    V.TexCoord.X := V.TexCoord.X - USize * Dist;
    V.Position.X := X2;
  End;

  If (V.Position.Y<Y1) Then
  Begin
    Dist := (Y1 - V.Position.Y)/Height;
    V.TexCoord.Y := V.TexCoord.Y + VSize * Dist;
    V.Position.Y := Y1;
  End Else
  If (V.Position.Y>Y2) Then
  Begin
    Dist := (V.Position.Y-Y2)/Height;
    V.TexCoord.Y := V.TexCoord.Y - VSize * Dist;
    V.Position.Y := Y2;
  End;
End;


{ SpriteManager }
Class Function SpriteManager.Instance:SpriteManager;
Begin
  If Not Assigned(_SpriteManager_Instance) Then
    _SpriteManager_Instance := InitializeApplicationComponent(SpriteManager, GraphicsManager);

  Result := SpriteManager(_SpriteManager_Instance.Instance);
End;


Function SpriteManager.DrawSprite(X,Y,Layer:Single; SpriteTexture:Texture; ColorTable:Texture; BlendMode:Integer;  Saturation:Single; BilinearFilter:Boolean; IsFont:Boolean): Sprite;
Begin
  Result := Self.DrawSpriteWithOutline(X,Y,Layer, SpriteTexture, ColorNull, ColorTable, BlendMode,  Saturation, BilinearFilter, IsFont);
End;

Function SpriteManager.DrawSpriteWithOutline(X,Y,Layer:Single; SpriteTexture:Texture; Outline:Color; ColorTable:Texture; BlendMode:Integer;  Saturation:Single; BilinearFilter:Boolean; IsFont:Boolean):Sprite;
Var
  N, I:Integer;
  HasShaders:Boolean;
Begin
  If (Not Assigned(SpriteTexture)) Or (Not SpriteTexture.IsReady()) Then
  Begin
    If Not Assigned(_NullSprite) Then
      _NullSprite := Sprite.Create;

    Result := _NullSprite;
    Exit;
  End;

  If (Layer>0) Then
    Layer := -Layer;
  X := Trunc(X);
  Y := Trunc(Y);

  Inc(_Index);
  If (_Index>=_SpriteCount) Then
  Begin
    _SpriteCount := _SpriteCount * 2;
    SetLength(_Sprites, _SpriteCount);
    For I:=_Index To Pred(_SpriteCount) Do
        _Sprites[I] := Sprite.Create;
  End;

  If (_Sprites[_Index] = Nil) Then
    _Sprites[_Index] := Sprite.Create;

  Result := _Sprites[_Index];
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
  Result.ClipRect := Nil;
  Result._Saturation := Saturation;
  Result._IsFont := IsFont;
  Result._Outline := Outline;

  Result._Transform := MatrixIdentity3x3;

  SpriteTexture.BilinearFilter := BilinearFilter;
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

  HasShaders := GraphicsManager.Instance.Settings.Shaders.Avaliable;

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
    Break;
  End;

  If (N<0) Then
  Begin
    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._Count <=0) Then
    Begin
      N := I;
      _Batches[N]._BlendMode := BlendMode;
      _Batches[N]._Texture := SpriteTexture;
      _Batches[N]._Closed := False;
      _Batches[N]._Layer := Result.Layer;
      {$IFNDEF DISABLECOLORGRADING}
      _Batches[N]._ColorTable := ColorTable;
      {$ENDIF}
      _Batches[N]._IsFont := IsFont;
      _Batches[N]._Saturation := Saturation;
      _Batches[N]._Outline := Outline;
      _Batches[N]._First := Nil;
      _Batches[N]._Manager := Self;
      Break;
    End;
  End;

  If (N<0) Then
  Begin
    N := _BatchCount;
    Inc(_BatchCount);
    SetLength(_Batches, _BatchCount);
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

  If (N<0) Or (N>=_BatchCount) Then
    Exit;

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
    _Sprites[I] := Sprite.Create;
  _Index := -1;
  _BatchCount := 50;
  SetLength(_Batches, _BatchCount);
End;

Procedure SpriteManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._SpriteCount) Do
  If Assigned(_Sprites[I]) Then
    _Sprites[I].Release;

  _SpriteManager_Instance := Nil;
End;

Function GreyTransform(S:Color):Color; CDecl;
Begin
  Result := ColorGrey(ColorLuminance(S));
End;

Procedure SpriteManager.Render;
Var
  I:Integer;
  Min:Single;
  Total, Index:Integer;
  //Count:Integer;
  Projection, M:Matrix4x4;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Render');{$ENDIF}

  GraphicsManager.Instance.SetBlendMode(blendNone);

//  glDisable(GL_DEPTH_TEST); {FIXME}
  glDepthFunc(GL_LEQUAL);

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    Projection := GraphicsManager.Instance.ProjectionMatrix;
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
  {$ENDIF}

  If (_SpriteShaderWithoutGrading = Nil) Then
  Begin
    _SpriteShaderWithoutGrading := TERRA_Shader.Shader.CreateFromString(GetShader_Sprite(False), 'Sprite');
    ShaderManager.Instance.AddShader(_SpriteShaderWithoutGrading);
  End;

  {$IFNDEF DISABLECOLORGRADING}
  If (_SpriteShaderWithGrading = Nil) Then
  Begin
    _SpriteShaderWithGrading := TERRA_Shader.Shader.CreateFromString(GetShader_Sprite(True), 'SpriteGrading');
    ShaderManager.Instance.AddShader(_SpriteShaderWithGrading);
  End;
  {$ENDIF}

  If (_FontShader = Nil) Then
  Begin
    _FontShader := TERRA_Shader.Shader.CreateFromString(GetShader_Font(), 'Font');
    ShaderManager.Instance.AddShader(_FontShader);
  End;

  _CurrentShader := Nil;

  Total := 0;
  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) Then
    Inc(Total);

  //Count := 0;
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

//      Inc(Count); //If Count>1 Then break;
    End Else
    Break;
  End;

  glEnable(GL_DEPTH_TEST);

  Self.Clear;

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;
  {$ENDIF}

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

Procedure SpriteManager.SetShader(MyShader: Shader);
Var
  PositionHandle, UVHandle, ColorHandle, SaturationHandle:Integer;
  Projection:Matrix4x4;
Begin
  If (_CurrentShader = MyShader) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'SetShader');{$ENDIF}

  _CurrentShader := MyShader;

  Projection := GraphicsManager.Instance.ProjectionMatrix;

  If Not MyShader.IsReady() Then
    Exit;

  ShaderManager.Instance.Bind(MyShader);

  MyShader.SetUniform('texture', 0);
  MyShader.SetUniform('projectionMatrix', Projection);

  {If (MyShader = _FontShader) Then
    IntToString(2);}

  {$IFDEF PC}
  If (GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  {$ENDIF}
  Begin
    PositionHandle := MyShader.GetAttribute('terra_position');
    UVHandle := MyShader.GetAttribute('terra_UV0');
    ColorHandle := MyShader.GetAttribute('terra_color');
    SaturationHandle := MyShader.GetAttribute('terra_saturation');

    glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(SpriteVertex), @(_Vertices[0].Position));
    glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(SpriteVertex), @(_Vertices[0].TexCoord));  
    glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(SpriteVertex), @(_Vertices[0].Color));         
    glVertexAttribPointer(SaturationHandle, 4, GL_FLOAT, False, SizeOf(SpriteVertex), @(_Vertices[0].Saturation));
  End;

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

Procedure Sprite.SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);
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

Procedure Sprite.SetScaleAndRotationRelative(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single);
Var
  Mat:Matrix3x3;
Begin
  Mat := MatrixRotationAndScale2D(Rotation, ScaleX, ScaleY);
  SetTransformRelative(Center, Mat);
End;

Procedure Sprite.SetScaleRelative(Const Center:Vector2D; ScaleX, ScaleY:Single);
Begin
  SetScaleAndRotationRelative(Center, ScaleX, ScaleY, 0.0);
End;

Procedure Sprite.SetScroll(U, V: Single);
Begin
  If (U>1) Or (U<-1) Then
    U := Frac(U);

  If (V>1) Or (V<-1) Then
    V := Frac(V);

  Self._ScrollU := U;
  Self._ScrollV := V;
End;

Procedure Sprite.SetColor(C: Color);
Begin
  _A := C;
  _B := C;
  _C := C;
  _D := C;
End;

Procedure Sprite.SetColors(A, B, C, D:Color);
Begin
  _A := A;
  _B := B;
  _C := C;
  _D := D;
End;

Procedure Sprite.SetAlpha(Alpha: Byte);
Begin
  _A.A := Alpha;
  _B.A := Alpha;
  _C.A := Alpha;
  _D.A := Alpha;
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

Procedure Sprite.SetScaleAndRotationRelative(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotationRelative(Center, Scale, Scale, Rotation);
End;

Procedure Sprite.SetScaleRelative(const Center: Vector2D; Scale: Single);
Begin
  SetScaleRelative(Center, Scale, Scale);
End;

Procedure Sprite.SetScaleAndRotation(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotation(Center, Scale, Scale, Rotation);
End;

Procedure Sprite.Release;
Begin
  // do nothing
End;

{ SpriteBatch }
Procedure SpriteBatch.AddSprite(P: Sprite);
Begin
  Inc(_Count);
  P._Next := _First;
  _First := P;
End;

Procedure SpriteBatch.SetupSaturationCombiners(Var Slot:Integer);
Var
  Values:Array[0..3] Of Single;
Begin
  Slot := 0;
{$IFDEF PC}
  Values[0] := 0.30;
  Values[1] := 0.59;
  Values[2] := 0.11;
  Values[3] := 1.0;

  glActiveTexture(GL_TEXTURE0);
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );
  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PRIMARY_COLOR);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR );
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_PRIMARY_COLOR);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA, GL_TEXTURE);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_ALPHA, GL_SRC_ALPHA);

  Inc(Slot);
  TextureManager.Instance.WhiteTexture.Bind(Slot);
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );
  glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_DOT3_RGB);
  glTexEnvi(GL_TEXTURE_ENV, GL_SRC0_RGB, GL_PREVIOUS);
  glTexEnvi(GL_TEXTURE_ENV, GL_SRC1_RGB, GL_CONSTANT);
  glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_ONE_MINUS_SRC_COLOR);
  glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @Values);

  If (_Saturation<=0) Then
    Exit;

  If (GraphicsManager.Instance.Settings.MaxTextureUnits>3) Then
  Begin
    Values[0] := 0.5;
    Values[1] := Values[0];
    Values[2] := Values[0];
    Values[3] := Values[0];
    Inc(Slot);
    TextureManager.Instance.WhiteTexture.Bind(Slot);
    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_ADD);
    glTexEnvi(GL_TEXTURE_ENV, GL_SRC0_RGB, GL_PREVIOUS);
    glTexEnvi(GL_TEXTURE_ENV, GL_SRC1_RGB, GL_CONSTANT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
    glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @Values);
  End;

  If (GraphicsManager.Instance.Settings.MaxTextureUnits>2) Then
  Begin
    Values[0] := _Saturation;
    Values[1] := Values[0];
    Values[2] := Values[0];
    Values[3] := Values[0];

    Inc(Slot);
    TextureManager.Instance.WhiteTexture.Bind(Slot);
    glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @Values);
    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_INTERPOLATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SRC0_RGB, GL_TEXTURE0);
    glTexEnvi(GL_TEXTURE_ENV, GL_SRC1_RGB, GL_PREVIOUS);
    glTexEnvi(GL_TEXTURE_ENV, GL_SRC2_RGB, GL_CONSTANT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB, GL_SRC_COLOR);
  End;
{$ENDIF}
End;

Procedure SpriteBatch.Flush;
Var
  I, J:Integer;
  Ofs:Integer;
  S:Sprite;
  W,H, K:Single;
  MaxX,MinX, MaxY,MinY:Single;
  V:PSpriteVertex;
  Landscape, FullyClipped:Boolean;
  USize, VSize:Single;
  Slot:Integer;
Begin
  _Closed := False;
  If (_Count<=0) Then
    Exit;

  Landscape := IsLandscapeOrientation(Application.Instance.Orientation);

  {$IFDEF PC}
  Slot := 0;
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    glVertexPointer(3, GL_FLOAT, SizeOf(SpriteVertex), @_Vertices[0].Position);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(SpriteVertex), @_Vertices[0].TexCoord);
    glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(SpriteVertex), @_Vertices[0].Color);

    If (_Saturation<1.0) Then
      SetupSaturationCombiners(Slot); //BIBI
  End;
  {$ENDIF}

  If (Self._IsFont) Then
  Begin
    _Manager.EnableFontShader();
    {$IFDEF DISTANCEFIELDFONTS}
    _Manager._FontShader.SetUniform('outline', _Outline);
    {$ENDIF}
  End Else
    _Manager.EnableSpriteShader({$IFDEF DISABLECOLORGRADING}False{$ELSE}Assigned(Self._ColorTable){$ENDIF});

  _Texture.Bind(0);

  {$IFNDEF DISABLECOLORGRADING}
  If (Not Self._IsFont) Then
    ColorTableBind(_ColorTable, 1);
  {$ENDIF}

  GraphicsManager.Instance.SetBlendMode(_BlendMode);
  Ofs := 0;

//  Ratio := UIManager.Instance.Ratio;

  S := _First;
  While Assigned(S) Do
  Begin
    W := S.Rect.Width;
    H := S.Rect.Height;

    If (W<=0) Then
      W := (S.Rect.U2-S.Rect.U1) * (S.Texture.Width / S.Texture.Ratio.X);
    If (H<=0) Then
      H := (S.Rect.V2-S.Rect.V1) * (S.Texture.Height /S.Texture.Ratio.Y);

    If (S.Mirror) Then
    Begin
      K := S.Rect.U1; S.Rect.U1 := S.Rect.U2; S.Rect.U2 := K;
    End;

    If (S.Rect.Texture Is RenderTarget) Then
      S.Flip := Not S.Flip;

    If (S.Flip) Then
    Begin
      K := S.Rect.V1; S.Rect.V1 := S.Rect.V2; S.Rect.V2 := K;
    End;

    S.Rect.U1 := S.Rect.U1 + S._ScrollU;
    S.Rect.U2 := S.Rect.U2 + S._ScrollU;
    S.Rect.V1 := S.Rect.V1 + S._ScrollV;
    S.Rect.V2 := S.Rect.V2 + S._Scrollv;

    USize := S.Rect.U2-S.Rect.U1;
    VSize := S.Rect.V2-S.Rect.V1;

    _Vertices[Ofs + 0].Color :=  S._C;
    _Vertices[Ofs + 1].Color :=  S._D;
    _Vertices[Ofs + 2].Color :=  S._B;
    _Vertices[Ofs + 4].Color :=  S._A;

    _Vertices[Ofs+0].Position := VectorCreate(0, H, 0);
    _Vertices[Ofs+0].TexCoord := VectorCreate2D(S.Rect.U1, S.Rect.V2);

    _Vertices[Ofs+1].Position := VectorCreate(W, H, 0);
    _Vertices[Ofs+1].TexCoord := VectorCreate2D(S.Rect.U2, S.Rect.V2);

    _Vertices[Ofs+2].Position := VectorCreate(W + S.Skew, 0, 0);
    _Vertices[Ofs+2].TexCoord := VectorCreate2D(S.Rect.U2, S.Rect.V1);

    _Vertices[Ofs+4].Position := VectorCreate(0 + S.Skew, 0, 0);
    _Vertices[Ofs+4].TexCoord := VectorCreate2D(S.Rect.U1, S.Rect.V1);

    _Vertices[Ofs+3] := _Vertices[Ofs+2];
    _Vertices[Ofs+5] := _Vertices[Ofs+0];

    For J:=0 To 5 Do
    Begin
      _Vertices[Ofs + J].Position.X := _Vertices[Ofs + J].Position.X + S.Position.X - S.Anchor.X *W;
      _Vertices[Ofs + J].Position.Y := _Vertices[Ofs + J].Position.Y + S.Position.Y - S.Anchor.Y *H;
    End;

    For J:=0 To 5 Do
      _Vertices[Ofs + J].Position := S._Transform.Transform(_Vertices[Ofs + J].Position);

    If Assigned(S.ClipRect) Then
    Begin
      MinX := 9999;
      MaxX := -9999;
      MinY := 9999;
      MaxY := -9999;

      For J:=0 To 5 Do
      Begin
        V := @_Vertices[Ofs + J];
        ClipVertex(V, S.ClipRect, W, H, USize, VSize{, Landscape});
        MinX := FloatMin(MinX, V.Position.X);
        MinY := FloatMin(MinY, V.Position.Y);
        MaxX := FloatMax(MaxX, V.Position.X);
        MaxY := FloatMax(MaxY, V.Position.Y);
      End;

      FullyClipped := (Abs(MinX-MaxX)<Epsilon) Or (Abs(MinY-MaxY)<Epsilon);
    End Else
      FullyClipped := False;

    If Not FullyClipped Then
    Begin
      For J:=0 To 5 Do
      Begin
        _Vertices[Ofs + J].Position.Z := S.Layer;
        _Vertices[Ofs + J].Saturation :=  S._Saturation;
      End;

      Inc(Ofs, 6);
    End;

    S := S._Next;
  End;

  glDrawArrays(GL_TRIANGLES, 0, Ofs);
  GraphicsManager.Instance.Internal(0 , Ofs Div 3);

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    If (Slot>0) Then
    Begin
      Slot := GraphicsManager.Instance.Settings.MaxTextureUnits;
      For I:=Pred(Slot) DownTo 1 Do
      Begin
        glActiveTexture(GL_TEXTURE0 + I);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        glDisable(GL_TEXTURE_2D);
      End;
    End;
  End;
  {$ENDIF}

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
  If (Texture = Nil) Or (Not Texture.IsReady()) Then
    Exit;

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
  If (Texture = Nil) Or (Not Texture.IsReady()) Then
    Exit;

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
  If (Texture = Nil) Or (Not Texture.IsReady()) Then
    Exit;

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

{ ClipRect }
Procedure ClipRect.Release;
Begin
  // do nothing
End;

Procedure ClipRect.GetRealRect(Out X1, Y1, X2, Y2: Single{; Landscape:Boolean});
Var
  UIWidth, UIHeight:Integer;
Begin
{  If (Landscape) Then
  Begin
    UIWidth := GraphicsManager.Instance.UIViewport.Width;
    UIHeight := GraphicsManager.Instance.UIViewport.Height;
    X2 := UIWidth - (Self.Y);
    X1 := UIWidth - (X2 + Self.Height);
    Y2 := UIHeight - (Self.X);
    Y1 := UIHeight - (Y2 + Self.Width);
  End Else}
  Begin
    X1 := Self.X;
    X2 := X1 + Self.Width;
    Y1 := Self.Y;
    Y2 := Y1 + Self.Height;
  End;
End;

Procedure ClipRect.SetHeight(const Value: Single);
Begin
  _Height := Value;
End;

Procedure ClipRect.SetWidth(const Value: Single);
Begin
  _Width := Value;
End;

Procedure ClipRect.SetX(const Value: Single);
Begin
  _X := Value;
End;

Procedure ClipRect.SetY(const Value: Single);
Begin
  _Y := Value;
End;

Procedure ClipRect.Transform(const M: Matrix3x3);
Var
  I:Integer;
  P:Array[0..3] Of Vector2D;
  T:Vector2D;
  MinX, MinY, MaxX, MaxY:Single;
Begin
  P[0].X := _X;
  P[0].Y := _Y;

  P[1].X := _X + _Width;
  P[1].Y := _Y;

  P[2].X := _X + _Width;
  P[2].Y := _Y + _Height;

  P[3].X := _X;
  P[3].Y := _Y + _Height;

  MaxX := -9999;
  MaxY := -9999;

  MinX := 9999;
  MinY := 9999;

  For I:=0 To 3 Do
  Begin
    T := M.Transform(P[I]);

    If (T.X>MaxX) Then
      MaxX := T.X;

    If (T.Y>MaxY) Then
      MaxY := T.Y;

    If (T.X<MinX) Then
      MinX := T.X;

    If (T.Y<MinY) Then
      MinY := T.Y;
  End;

  Self.X := MinX;
  Self.Y := MinY;

  Self.Width := MaxX - MinX;
  Self.Height := MaxY - MinY;
End;

Initialization
Finalization
  If Assigned(_NullSprite) Then
    _NullSprite.Release;
End.
