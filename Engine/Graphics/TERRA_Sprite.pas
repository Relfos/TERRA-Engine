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
Unit TERRA_Sprite;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color, TERRA_Texture,
  TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect, TERRA_Renderer, TERRA_VertexFormat;

Type
  SpriteVertex = Class(Vertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
      Position:Vector3D;
      Color:ColorRGBA;
      Glow:ColorRGBA;
      Saturation:Single;
      ClipRect:Vector4D;
      TexCoord:Vector2D;
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

  TERRASprite = Class(TERRAObject)
    Protected
      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _Transform:Matrix3x3;
      _Shader:ShaderInterface;

      _Saturation:Single;

      _Texture:TERRATexture;

      _Vertices:VertexData;

      _Offset:Integer;

      _Glow:ColorRGBA;

      Procedure MakeQuad(Const Pos:Vector2D; LayerOffset:Single; Const U1, V1, U2, V2:Single; Const Width, Height:Single; Const A,B,C,D:ColorRGBA; Const Skew:Single);

    Public
      Layer:Single;
      ClipRect:TERRAClipRect;
      Outline:ColorRGBA;
      BlendMode:Integer;
      Next:TERRASprite;

      Constructor Create();

      Procedure Release; Override;

      Function Rebuild():Boolean; Virtual;

      Procedure SetTexture(Value: TERRATexture); Virtual;

      Procedure SetTransform(Const Mat:Matrix3x3);
      Procedure SetTransformWithCenter(Const Center:Vector2D; Const Mat:Matrix3x3);

      Procedure SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single); Overload;

      Procedure SetScaleAndRotationWithCenter(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single); Overload;
      Procedure SetScaleAndRotationWithCenter(Const Center:Vector2D; Scale:Single; Rotation:Single); Overload;

      Procedure ConcatTransform(Const Mat:Matrix3x3);

      Property Texture:TERRATexture Read _Texture Write SetTexture;
      Property Shader:ShaderInterface Read _Shader Write _Shader;
      Property Saturation:Single Read _Saturation Write _Saturation;
      Property Glow:ColorRGBA Read _Glow Write _Glow;
      Property ColorTable:TERRATexture Read _ColorTable Write _ColorTable;

      Property Vertices:VertexData Read _Vertices;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;
  End;

  QuadSprite = Class(TERRASprite)
    Protected
      _A, _B, _C, _D:ColorRGBA;

    Public
      Position:Vector2D;
      Anchor:Vector2D;

      Mirror:Boolean;
      Flip:Boolean;

      ScrollU:Single;
      ScrollV:Single;

      Rect:TextureRect;

      Function Rebuild():Boolean; Override;

      Procedure SetColor(Const C:ColorRGBA);
      Procedure SetColors(Const A, B, C, D:ColorRGBA);
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

Function CreateSpriteVertexData(Count:Integer):VertexData;

Implementation
Uses TERRA_ResourceManager, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};


Function CreateSpriteVertexData(Count:Integer):VertexData;
Const
  SpriteVertexFormat = [vertexFormatPosition, vertexFormatColor, vertexFormatUV0, vertexFormatUV1, vertexFormatUV2];
Begin
  Result := VertexData.Create(SpriteVertexFormat, Count);
  Result.SetAttributeFormat(vertexUV1, typeVector4D);
  Result.SetAttributeFormat(vertexUV2, typeVector4D);
End;

{ SpriteVertex }
Procedure SpriteVertex.Load;
Var
  V:Vector4D;
Begin
  Self.GetVector3D(vertexPosition, Position);
  Self.GetColor(vertexColor, Color);
  Self.GetVector2D(vertexUV0, TexCoord);
  Self.GetVector4D(vertexUV1, ClipRect);
  Self.GetVector4D(vertexUV2, V);

  Self.Saturation := V.W;
  V.W := 1.0;
  Self.Glow := ColorCreateFromFloat(V.X, V.Y, V.Z);
End;

Procedure SpriteVertex.Save;
Var
  V:Vector4D;
Begin
  V.X := Glow.R / 255;
  V.Y := Glow.G / 255;
  V.Z := Glow.B / 255;
  V.W := Saturation;

  Self.SetVector3D(vertexPosition, Position);
  Self.SetColor(vertexColor, Color);
  Self.SetVector2D(vertexUV0, TexCoord);
  Self.SetVector4D(vertexUV1, ClipRect);
  Self.SetVector4D(vertexUV2, V);
End;

{ TERRASprite }
Constructor TERRASprite.Create;
Begin
  Self.Saturation := 1;
  Self.BlendMode := blendBlend;
  Self.Outline := ColorBlack;
  Self.Glow := ColorBlack;
End;

Procedure TERRASprite.Release();
Begin
  ReleaseObject(_Vertices);
End;

Procedure TERRASprite.SetTexture(Value:TERRATexture);
Begin
  _Texture := Value;
End;

Procedure TERRASprite.SetTransform(Const Mat: Matrix3x3);
Begin
  _Transform := Mat;
End;

Procedure TERRASprite.SetTransformWithCenter(Const Center:Vector2D; Const Mat: Matrix3x3);
Begin
  SetTransform(MatrixTransformAroundPoint2D(Center, Mat));
End;

Procedure TERRASprite.SetScaleAndRotationWithCenter(Const Center:Vector2D; ScaleX, ScaleY:Single; Rotation:Single);
Var
  Mat:Matrix3x3;
Begin
  Mat := MatrixRotationAndScale2D(Rotation, ScaleX, ScaleY);
  SetTransformWithCenter(Center, Mat);
End;

Procedure TERRASprite.SetScale(Const Center:Vector2D; ScaleX, ScaleY:Single);
Begin
  SetScaleAndRotationWithCenter(Center, ScaleX, ScaleY, 0.0);
End;

Procedure TERRASprite.ConcatTransform(const Mat: Matrix3x3);
Begin
  Self._Transform := MatrixMultiply3x3(_Transform, Mat);
End;

Procedure TERRASprite.SetScaleAndRotationWithCenter(const Center: Vector2D; Scale, Rotation: Single);
Begin
  SetScaleAndRotationWithCenter(Center, Scale, Scale, Rotation);
End;

Procedure TERRASprite.MakeQuad(Const Pos:Vector2D; LayerOffset:Single; Const U1, V1, U2, V2:Single; Const Width, Height:Single; Const A, B, C, D:ColorRGBA; Const Skew:Single);
Begin
  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_Offset + 6);

  If (_Offset >= _Vertices.Count) Then
    _Vertices.Resize(_Offset + 6);

  LayerOffset := LayerOffset + Self.Layer;

  _Vertices.SetColor(_Offset + 0, vertexColor, C);
  _Vertices.SetColor(_Offset + 1, vertexColor, D);
  _Vertices.SetColor(_Offset + 2, vertexColor, B);
  _Vertices.SetColor(_Offset + 4, vertexColor, A);

  _Vertices.SetVector3D(_Offset + 0, vertexPosition, VectorCreate(Pos.X, Pos.Y + Height, LayerOffset));
  _Vertices.SetVector2D(_Offset + 0, vertexUV0, VectorCreate2D(U1, V2));

  _Vertices.SetVector3D(_Offset + 1, vertexPosition, VectorCreate(Pos.X + Width, Pos.Y +Height, LayerOffset));
  _Vertices.SetVector2D(_Offset + 1, vertexUV0, VectorCreate2D(U2, V2));

  _Vertices.SetVector3D(_Offset + 2, vertexPosition, VectorCreate(Pos.X + Width + Skew, Pos.Y, LayerOffset));
  _Vertices.SetVector2D(_Offset + 2, vertexUV0, VectorCreate2D(U2, V1));

  _Vertices.SetVector3D(_Offset + 4, vertexPosition, VectorCreate(Pos.X + Skew, Pos.Y, LayerOffset));
  _Vertices.SetVector2D(_Offset + 4, vertexUV0, VectorCreate2D(U1, V1));

  _Vertices.CopyVertex(_Offset + 2, _Offset + 3);
  _Vertices.CopyVertex(_Offset + 0, _Offset + 5);

  Inc(_Offset, 6);
End;

Function TERRASprite.Rebuild():Boolean;
Begin
  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(6);

  _Offset := 0;

  Result := True;
End;

{ QuadSprite }
Procedure QuadSprite.SetTransformRelative(Const Center:Vector2D; Const Mat:Matrix3x3);
Var
  Dest:Vector2D;
  W,H:Single;
Begin
  If (Self.Texture = Nil) Then
  Begin
    SetTransformWithCenter(Self.Position, Mat);
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
    SetTransformWithCenter(Dest, Mat)
  End;
End;

Procedure QuadSprite.SetScaleAndRotation(ScaleX, ScaleY, Rotation:Single);
Begin
  SetScaleAndRotationWithCenter(Self.Position, ScaleX, ScaleY, Rotation);
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

  Self.ScrollU := U;
  Self.ScrollV := V;
End;

Procedure QuadSprite.SetColor(Const C:ColorRGBA);
Begin
  _A := C;
  _B := C;
  _C := C;
  _D := C;
End;

Procedure QuadSprite.SetColors(Const A, B, C, D:ColorRGBA);
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

Function QuadSprite.Rebuild():Boolean;
Var
  K:Single;
  Pos:Vector2D;
  Width, Height:Integer;
Begin
  Inherited Rebuild();

  If (Self._A.A=0) And (Self._B.A=0) And (Self._C.A=0) And (Self._D.A=0) Then
  Begin
    Result := False;
    Exit;
  End;

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

  Self.Rect.U1 := Self.Rect.U1 + Self.ScrollU;
  Self.Rect.U2 := Self.Rect.U2 + Self.ScrollU;
  Self.Rect.V1 := Self.Rect.V1 + Self.ScrollV;
  Self.Rect.V2 := Self.Rect.V2 + Self.Scrollv;

  MakeQuad(Pos, 0.0, Rect.U1, Rect.V1, Rect.U2, Rect.V2, Width, Height, _A, _B, _C, _D, 0.0);

  Result := True;
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



End.
