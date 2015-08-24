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

      _U1, _V1:Single;
      _U2, _V2:Single;

      _CA, _CB, _CC, _CD:ColorRGBA;

    Public
      Layer:Single;
      ClipRect:TERRAClipRect;
      Outline:ColorRGBA;
      BlendMode:Integer;
      Next:TERRASprite;

      Flip:Boolean;
      Mirror:Boolean;

      Constructor Create();

      Procedure Release; Override;

      Procedure Clear();

      Procedure SetUVs(Const U1, V1, U2, V2:Single);

      Procedure SetColor(Const Color:ColorRGBA);
      Procedure SetCornerColors(Const A, B, C, D:ColorRGBA);

      Procedure MakeQuad(Const Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single = 0.0);

      Procedure SetTexture(Value: TERRATexture); Virtual;

      Procedure SetTransform(Const Mat:Matrix3x3);

      Procedure ConcatTransform(Const Mat:Matrix3x3);
      Procedure Translate(Const X,Y:Single);
      Procedure Rotate(Angle:Single);
      Procedure Scale(Const X, Y:Single); Overload;
      Procedure Scale(Const Value:Single); Overload;


      Property Texture:TERRATexture Read _Texture Write SetTexture;
      Property Shader:ShaderInterface Read _Shader Write _Shader;
      Property Saturation:Single Read _Saturation Write _Saturation;
      Property Glow:ColorRGBA Read _Glow Write _Glow;
      Property ColorTable:TERRATexture Read _ColorTable Write _ColorTable;

      Property Vertices:VertexData Read _Vertices;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;
  End;

Function CreateSpriteVertexData(Count:Integer):VertexData;

Implementation
Uses TERRA_ResourceManager, TERRA_EngineManager, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math
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
  If (Not Assigned(Value)) Or (Not Value.IsReady()) Then
    Value := Engine.Textures.WhiteTexture;

  _Texture := Value;
End;

Procedure TERRASprite.SetTransform(Const Mat: Matrix3x3);
Begin
  _Transform := Mat;
End;

Procedure TERRASprite.ConcatTransform(const Mat: Matrix3x3);
Begin
  _Transform := MatrixMultiply3x3(Mat, _Transform);
End;

Procedure TERRASprite.Translate(const X, Y: Single);
Begin
  Self.ConcatTransform(MatrixTranslation2D(X, Y));
End;

procedure TERRASprite.Rotate(Angle: Single);
Begin
  Self.ConcatTransform(MatrixRotation2D(Angle));
End;

Procedure TERRASprite.Scale(const Value: Single);
Begin
  Self.ConcatTransform(MatrixScale2D(Value));
End;

Procedure TERRASprite.Scale(const X, Y: Single);
Begin
  Self.ConcatTransform(MatrixScale2D(X, Y));
End;

Procedure TERRASprite.MakeQuad(Const Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single);
Var
  U1, V1, U2, V2:Single;
Begin
  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_Offset + 6);

  If (_Offset >= _Vertices.Count) Then
    _Vertices.Resize(_Offset + 6);

  LayerOffset := LayerOffset + Self.Layer;

  If (Self.Mirror) Then
  Begin
    U1 := _U2;
    U2 := _U1;
  End Else
  Begin
    U1 := _U1;
    U2 := _U2;
  End;

  If ((Self.Texture.Origin = surfaceBottomRight) <> Self.Flip) Then
  Begin
    V1 := _V2;
    V2 := _V1;
  End Else
  Begin
    V1 := _V1;
    V2 := _V2;
  End;

  _Vertices.SetColor(_Offset + 0, vertexColor, _CC);
  _Vertices.SetColor(_Offset + 1, vertexColor, _CD);
  _Vertices.SetColor(_Offset + 2, vertexColor, _CB);
  _Vertices.SetColor(_Offset + 4, vertexColor, _CA);

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

Procedure TERRASprite.Clear;
Begin
  If Assigned(_Vertices) Then
    _Vertices.Resize(0);

  _Offset := 0;
End;

(*Function QuadSprite.Rebuild():Boolean;
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

  Pos.X := 0;
  Pos.Y := 0;

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
End;*)

Procedure TERRASprite.SetUVs(const U1, V1, U2, V2: Single);
Begin
  Self._U1 := U1;
  Self._U2 := U2;
  Self._V1 := V1;
  Self._V2 := V2;
End;

Procedure TERRASprite.SetColor(const Color: ColorRGBA);
Begin
  _CA := Color;
  _CB := Color;
  _CC := Color;
  _CD := Color;
End;

Procedure TERRASprite.SetCornerColors(Const A, B, C, D:ColorRGBA);
Begin
  _CA := A;
  _CB := B;
  _CC := C;
  _CD := D;
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
