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

Const
  Sprite_Font         = 1;
  Sprite_SolidColor   = 2;
  Sprite_ColorGrading = 4;
  Sprite_Dissolve     = 8;

Type
  SpriteAnchor = (
    spriteAnchor_TopLeft,
    spriteAnchor_Center,
    spriteAnchor_BottomMiddle
    );

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
  private
    procedure SetColorTable(const Value: TERRATexture);
    Protected
      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _Transform:Matrix3x3;

      _Saturation:Single;

      _Texture:TERRATexture;

      _DissolveTexture:TERRATexture;
      _DissolveValue:Single;

      _Vertices:VertexData;

      _VertexOffset:Integer;
      _IndexOffset:Integer;

      _Glow:ColorRGBA;

      _U1, _V1:Single;
      _U2, _V2:Single;

      _CA, _CB, _CC, _CD:ColorRGBA;

      _Indices:Array Of Word;

      _Flags:Cardinal;

    Public
      Layer:Single;
      ClipRect:TERRAClipRect;
      Outline:ColorRGBA;
      BlendMode:Integer;
      Next:TERRASprite;
      Smoothing:Single; // for fonts only

      Flip:Boolean;
      Mirror:Boolean;

      Constructor Create();

      Procedure Release; Override;

      Procedure Clear();

      Procedure SetUVs(Const U1, V1, U2, V2:Single);

      Procedure SetColor(Const Color:ColorRGBA);
      Procedure SetCornerColors(Const A, B, C, D:ColorRGBA);
      Procedure SetLineColor(Const StartColor, EndColor:ColorRGBA);

      Procedure AddQuad(Const Anchor:SpriteAnchor; Const Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single = 0.0);
      Procedure AddEllipse(Const Anchor:SpriteAnchor; Const Pos:Vector2D; LayerOffset:Single; Const RadiusX, RadiusY:Single);
      Procedure AddCircle(Const Anchor:SpriteAnchor; Const Pos:Vector2D; LayerOffset:Single; Const Radius:Single);
      Procedure AddPath(Const Positions:Array Of Vector2D; LayerOffset:Single; Width:Single);
      Procedure AddLine(Const StartPos, EndPos:Vector2D; LayerOffset:Single; Width:Single);
      Procedure AddTriangle(Const PosA, PosB, PosC:Vector2D; LayerOffset:Single);
      Procedure AddArrow(Const StartPos, EndPos:Vector2D; LayerOffset:Single; LineWidth, ArrowLength:Single);

      Procedure SetTexture(Value: TERRATexture);
      Procedure SetDissolve(Mask:TERRATexture; Const Value:Single);

      Procedure SetTransform(Const Mat:Matrix3x3);

      Function GetIndex(Index:Integer):Word;

      Procedure ConcatTransform(Const Mat:Matrix3x3);
      Procedure Translate(Const X,Y:Single);
      Procedure Rotate(Angle:Single);
      Procedure Scale(Const X, Y:Single); Overload;
      Procedure Scale(Const Value:Single); Overload;


      Property Texture:TERRATexture Read _Texture Write SetTexture;
      Property DissolveTexture:TERRATexture Read _DissolveTexture;
      Property DissolveValue:Single Read _DissolveValue;

      Property Saturation:Single Read _Saturation Write _Saturation;
      Property Glow:ColorRGBA Read _Glow Write _Glow;
      Property ColorTable:TERRATexture Read _ColorTable Write SetColorTable;

      Property Vertices:VertexData Read _Vertices;
      Property IndexCount:Integer Read _IndexOffset;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;

      Property Flags:Cardinal Read _Flags Write _Flags;
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
    Value := Engine.Textures.WhiteTexture
  Else
  Begin
    //Value.WrapMode := wrapNothing;
    Value.WrapMode := wrapAll;
    Value.Filter := filterLinear;
  End;

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

Procedure TERRASprite.AddTriangle(Const PosA, PosB, PosC:Vector2D; LayerOffset:Single);
Var
  U1, V1, U2, V2:Single;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_VertexOffset + 3)
  Else
  If (_VertexOffset >= _Vertices.Count) Then
    _Vertices.Resize(_VertexOffset + 3);

  If (Length(_Indices)< _IndexOffset + 3) Then
    SetLength(_Indices, _IndexOffset + 3);

  LayerOffset := LayerOffset + Self.Layer;

  _Vertices.SetColor(_VertexOffset + 0, vertexColor, _CA); //SampleColorAt(PX, PY)
  _Vertices.SetVector3D(_VertexOffset + 0, vertexPosition, VectorCreate(PosA.X, PosA.Y, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 0, vertexUV0, VectorCreate2D(0.5, 0.5));

  _Vertices.SetColor(_VertexOffset + 1, vertexColor, _CA); //SampleColorAt(PX, PY)
  _Vertices.SetVector3D(_VertexOffset + 1, vertexPosition, VectorCreate(PosB.X, PosB.Y, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 1, vertexUV0, VectorCreate2D(0.5, 0.5));

  _Vertices.SetColor(_VertexOffset + 2, vertexColor, _CA); //SampleColorAt(PX, PY)
  _Vertices.SetVector3D(_VertexOffset + 2, vertexPosition, VectorCreate(PosC.X, PosC.Y, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 2, vertexUV0, VectorCreate2D(0.5, 0.5));

  _Indices[_IndexOffset + 0] := _VertexOffset + 0;
  _Indices[_IndexOffset + 1] := _VertexOffset + 1;
  _Indices[_IndexOffset + 2] := _VertexOffset + 2;
  Inc(_IndexOffset, 3);
  Inc(_VertexOffset, 3);
End;

Procedure TERRASprite.AddEllipse(Const Anchor:SpriteAnchor; const Pos:Vector2D; LayerOffset:Single; Const RadiusX, RadiusY: Single);
Const
  SubDivs = 32;

Var
  I:Integer;
  U1, V1, U2, V2:Single;
  Delta, PX, PY:Single;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_VertexOffset + Succ(SubDivs))
  Else
  If (_VertexOffset >= _Vertices.Count) Then
    _Vertices.Resize(_VertexOffset + Succ(SubDivs));

  If (Length(_Indices)< _IndexOffset + 3 * SubDivs) Then
    SetLength(_Indices, _IndexOffset + 3 * SubDivs);

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

  Case Anchor Of
    spriteAnchor_Center:
      Pos.Subtract(VectorCreate2D(RadiusX * 0.5, RadiusY * 0.5));

    spriteAnchor_BottomMiddle:
      Pos.Subtract(VectorCreate2D(RadiusX * 0.5, -RadiusY * 0.5));
  End;

  For I:=0 To SubDivs Do
  Begin
    If (I>=SubDivs) Then
    Begin
      PX := 0.5;
      PY := 0.5;
      Delta := 0;
    End Else
    Begin
      Delta := (I / Pred(SubDivs)) * 360 * RAD;
      PX := 0.5 + Cos(Delta) * 0.5;
      PY := 0.5 + Sin(Delta) * 0.5;
    End;

    _Vertices.SetColor(_VertexOffset + I, vertexColor, _CA); //SampleColorAt(PX, PY)
    _Vertices.SetVector3D(_VertexOffset + I, vertexPosition, VectorCreate(Pos.X + RadiusX * PX, Pos.Y + + RadiusY * PY, LayerOffset));
    _Vertices.SetVector2D(_VertexOffset + I, vertexUV0, VectorCreate2D(PX, PY));
  End;

  For I:=0 To Pred(SubDivs) Do
  Begin
    _Indices[_IndexOffset + 0] := _VertexOffset + SubDivs;
    _Indices[_IndexOffset + 1] := _VertexOffset + I;
    _Indices[_IndexOffset + 2] := _VertexOffset + ((I + 1) Mod SubDivs);
    Inc(_IndexOffset, 3);
  End;

  Inc(_VertexOffset, Succ(SubDivs));
End;

Procedure TERRASprite.AddCircle(const Anchor: SpriteAnchor; Const Pos:Vector2D; LayerOffset:Single; Const Radius:Single);
Begin
  Self.AddEllipse(Anchor, Pos, LayerOffset, Radius, Radius);
End;


Procedure TERRASprite.AddQuad(Const Anchor:SpriteAnchor; Const Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single);
Var
  U1, V1, U2, V2:Single;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_VertexOffset + 4)
  Else
  If (_VertexOffset >= _Vertices.Count) Then
    _Vertices.Resize(_VertexOffset + 4);

  If (Length(_Indices)< _IndexOffset + 6) Then
    SetLength(_Indices, _IndexOffset + 6);

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

  Case Anchor Of
    spriteAnchor_Center:
      Pos.Subtract(VectorCreate2D(Width * 0.5, Height * 0.5));

    spriteAnchor_BottomMiddle:
      Pos.Subtract(VectorCreate2D(Width * 0.5, -Height * 0.5));
  End;

  _Vertices.SetColor(_VertexOffset + 0, vertexColor, _CC);
  _Vertices.SetColor(_VertexOffset + 1, vertexColor, _CD);
  _Vertices.SetColor(_VertexOffset + 2, vertexColor, _CB);
  _Vertices.SetColor(_VertexOffset + 3, vertexColor, _CA);

  _Vertices.SetVector3D(_VertexOffset + 0, vertexPosition, VectorCreate(Pos.X, Pos.Y + Height, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 0, vertexUV0, VectorCreate2D(U1, V2));

  _Vertices.SetVector3D(_VertexOffset + 1, vertexPosition, VectorCreate(Pos.X + Width, Pos.Y +Height, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 1, vertexUV0, VectorCreate2D(U2, V2));

  _Vertices.SetVector3D(_VertexOffset + 2, vertexPosition, VectorCreate(Pos.X + Width + Skew, Pos.Y, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 2, vertexUV0, VectorCreate2D(U2, V1));

  _Vertices.SetVector3D(_VertexOffset + 3, vertexPosition, VectorCreate(Pos.X + Skew, Pos.Y, LayerOffset));
  _Vertices.SetVector2D(_VertexOffset + 3, vertexUV0, VectorCreate2D(U1, V1));

  _Indices[_IndexOffset + 0] := _VertexOffset + 0;
  _Indices[_IndexOffset + 1] := _VertexOffset + 1;
  _Indices[_IndexOffset + 2] := _VertexOffset + 2;

  _Indices[_IndexOffset + 3] := _VertexOffset + 2;
  _Indices[_IndexOffset + 4] := _VertexOffset + 3;
  _Indices[_IndexOffset + 5] := _VertexOffset + 0;

  Inc(_VertexOffset, 4);
  Inc(_IndexOffset, 6);
End;

Procedure TERRASprite.AddPath(Const Positions:Array Of Vector2D; LayerOffset:Single; Width:Single);
Var
  Normal, Tangent, Pos:Vector2D;
  U1, V1, U2, V2, TU:Single;
  Len, Mult:Single;
  I, A, B:Integer;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  If (Length(Positions)<2) Then
    Exit;

  Width := Width * 0.5;

  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(_VertexOffset + 2 * Length(Positions))
  Else
  If (_VertexOffset >= _Vertices.Count) Then
    _Vertices.Resize(_VertexOffset + 2 * Length(Positions));

  If (Length(_Indices)< _IndexOffset + 6 * Length(Positions)) Then
    SetLength(_Indices, _IndexOffset + 6 * Length(Positions));

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

  For I:=Low(Positions) To High(Positions) Do
  Begin
    If (I=Low(Positions)) Then
    Begin
      A := I;
      B := Succ(I);
    End Else
    Begin
      A := Pred(I);
      B := I;
    End;

    Normal := Positions[B];
    Normal.Subtract(Positions[A]);
    Len := Normal.Length;
    Normal.Normalize();
    Tangent := VectorCreate2D(-Normal.Y, Normal.X);

    Pos := Positions[I];

    Mult := Len / (Width*2);

    _Vertices.SetColor(_VertexOffset + 0, vertexColor, _CC);
    _Vertices.SetColor(_VertexOffset + 1, vertexColor, _CC);

    If (Odd(I)) Then
      TU := U2
    Else
      TU := U1;

    _Vertices.SetVector3D(_VertexOffset + 0, vertexPosition, VectorCreate(Trunc(Pos.X + Tangent.X * Width), Trunc(Pos.Y + Tangent.Y * Width), LayerOffset));
    _Vertices.SetVector2D(_VertexOffset + 0, vertexUV0, VectorCreate2D(TU * Mult, V1));

    _Vertices.SetVector3D(_VertexOffset + 1, vertexPosition, VectorCreate(Trunc(Pos.X - Tangent.X * Width), Trunc(Pos.Y - Tangent.Y * Width), LayerOffset));
    _Vertices.SetVector2D(_VertexOffset + 1, vertexUV0, VectorCreate2D(TU * Mult, V2));

    If (I>Low(Positions)) Then
    Begin
      _Indices[_IndexOffset + 0] := _VertexOffset + 0;
      _Indices[_IndexOffset + 1] := _VertexOffset + 1;
      _Indices[_IndexOffset + 2] := _VertexOffset - 1;

      _Indices[_IndexOffset + 3] := _VertexOffset - 1;
      _Indices[_IndexOffset + 4] := _VertexOffset - 2;
      _Indices[_IndexOffset + 5] := _VertexOffset + 0;

      Inc(_IndexOffset, 6);
    End;

    Inc(_VertexOffset, 2);
  End;
End;

Procedure TERRASprite.AddLine(Const StartPos, EndPos:Vector2D; LayerOffset:Single; Width:Single);
Var
  Path:Array[0..1] Of Vector2D;
Begin
  Path[0] := StartPos;
  Path[1] := EndPos;
  Self.AddPath(Path, LayerOffset, Width);
End;

Procedure TERRASprite.AddArrow(const StartPos, EndPos: Vector2D; LayerOffset, LineWidth, ArrowLength: Single);
Var
  Normal, Tangent, SideA, SideB, ArrowCenter, ArrowPoint:Vector2D;
  Len, Delta:Single;
  ArrowWidth:Single;
Begin
  Normal := EndPos;
  Normal.Subtract(StartPos);

  Len := Normal.Length;
  Normal.Normalize();
  Tangent := VectorCreate2D(-Normal.Y, Normal.X);

  Delta := (ArrowLength) / Len;
  If (Delta>1) Then
    Delta := 1;

  ArrowWidth := ArrowLength * 0.3333;

  ArrowCenter := VectorCreate2D(StartPos.X * Delta + EndPos.X * (1.0-Delta), StartPos.Y * Delta + EndPos.Y * (1.0-Delta));
  ArrowPoint := EndPos;//VectorAdd2D(EndPos, VectorCreate2D(Normal.X * ArrowWidth * 0.5, Normal.Y * ArrowWidth * 0.5));

  SideA := VectorAdd2D(ArrowCenter, VectorCreate2D(Tangent.X * ArrowWidth, Tangent.Y * ArrowWidth));
  SideB := VectorAdd2D(ArrowCenter, VectorCreate2D(Tangent.X * -ArrowWidth, Tangent.Y * -ArrowWidth));

  Self.AddLine(StartPos, ArrowCenter, LayerOffset, LineWidth);
  Self.AddTriangle(ArrowPoint, SideA, SideB, LayerOffset);
End;

Procedure TERRASprite.Clear;
Begin
  If Assigned(_Vertices) Then
    _Vertices.Resize(0);

  _VertexOffset := 0;
  _IndexOffset := 0;
  _Flags := 0;
End;


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

Procedure TERRASprite.SetLineColor(Const StartColor, EndColor:ColorRGBA);
Begin
  _CA := EndColor;
  _CB := StartColor;
  _CC := EndColor;
  _CD := StartColor;
End;

Function TERRASprite.GetIndex(Index: Integer): Word;
Begin
  If (Index<0) Or (Index>=_IndexOffset) Then
    Result := 0
  Else
    Result := _Indices[Index];
End;

Procedure TERRASprite.SetDissolve(Mask:TERRATexture; Const Value:Single);
Begin
  Self._Flags := _Flags Or Sprite_Dissolve;
  Self._DissolveValue := Value;
  Self._DissolveTexture := Mask;
End;

Procedure TERRASprite.SetColorTable(const Value: TERRATexture);
Begin
  Self._ColorTable := Value;
  Self._Flags := _Flags Or Sprite_ColorGrading;
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
