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
 * Implements the sprite class
 ***********************************************************************************************************************
}
Unit TERRA_Sprite;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color, TERRA_Texture,
  TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect, TERRA_Renderer, TERRA_VertexFormat, TERRA_Renderable, TERRA_Geometry, TERRA_Viewport;

Const
  Sprite_Font         = 1;
  Sprite_SolidColor   = 2;
  Sprite_ColorGrading = 4;
  Sprite_Dissolve     = 8;
  Sprite_Pattern      = 16;
  Sprite_GUI          = 32;

  MaxSpriteShaders = 64;

Type
  SpriteAnchor = (
    spriteAnchor_TopLeft,
    spriteAnchor_Center,
    spriteAnchor_BottomMiddle
    );

  SpriteVertex = Class(TERRAVertex)
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

  TERRASprite = Class(TERRARenderable)
    Protected
      _Geometry:TERRAGeometry;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _Transform:Matrix3x3;

      _Saturation:Single;

      _Texture:TERRATexture;

      _DissolveTexture:TERRATexture;
      _DissolveValue:Single;

      _Pattern:TERRATexture;


      _Glow:ColorRGBA;

      _U1, _V1:Single;
      _U2, _V2:Single;

      _CA, _CB, _CC, _CD:ColorRGBA;


      _Flags:Cardinal;

      Procedure SetDefaultVertexValues(StartOfs:Integer);

      Function GetShader():ShaderInterface;

    Public
      Layer:Single;
      ClipRect:TERRAClipRect;
      Outline:ColorRGBA;
      BlendMode:Integer;
      Smoothing:Single; // for fonts only

      Flip:Boolean;
      Mirror:Boolean;

      Constructor Create();
      Procedure Release; Override;

      Procedure GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType); Override;
      Procedure GetMaterialDetails(Out Shader:ShaderInterface; Out Texture:TERRATexture); Override;
      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Override;

      Procedure Clear();
      Procedure Reset();

      Procedure SetUVs(Const U1, V1, U2, V2:Single);

      Procedure SetColor(Const Color:ColorRGBA);
      Procedure SetCornerColors(Const A, B, C, D:ColorRGBA);
      Procedure SetLineColor(Const StartColor, EndColor:ColorRGBA);

      Procedure SetColorTable(const Value: TERRATexture);
      Procedure SetPattern(const Value: TERRATexture);

      Procedure AddQuad(Const Anchor:SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single = 0.0; Const Angle:Single = 0.0; Const Scale:Single = 1.0);
      Procedure AddEllipse(Const Anchor:SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const RadiusX, RadiusY:Single);
      Procedure AddCircle(Const Anchor:SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const Radius:Single);
      Procedure AddPath(Const Positions:Array Of Vector2D; LayerOffset:Single; Width:Single);
      Procedure AddLine(Const StartPos, EndPos:Vector2D; LayerOffset:Single; Width:Single);
      Procedure AddTriangle(Const PosA, PosB, PosC:Vector2D; LayerOffset:Single);
      Procedure AddArrow(Const StartPos, EndPos:Vector2D; LayerOffset:Single; LineWidth, ArrowLength:Single);

      Procedure TileRemap(X,Y, TilesPerX, TilesPerY:Integer);
      Procedure TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
      Procedure PixelRemap(X1,Y1, X2, Y2:Integer; W:Integer=0; H:Integer=0);
      Procedure FullRemapUV();

      Procedure SetTexture(Value: TERRATexture);
      Procedure SetDissolve(Mask:TERRATexture; Const Value:Single);

      Procedure SetTransform(Const Mat:Matrix3x3);
      Procedure ResetTransform();
      Procedure ConcatTransform(Const Mat:Matrix3x3);
      Procedure Translate(Const X,Y:Single);
      Procedure Rotate(Angle:Single);
      Procedure Scale(Const X, Y:Single); Overload;
      Procedure Scale(Const Value:Single); Overload;

      Property Geometry:TERRAGeometry Read _Geometry;

      Property Texture:TERRATexture Read _Texture Write SetTexture;
      Property DissolveTexture:TERRATexture Read _DissolveTexture;
      Property DissolveValue:Single Read _DissolveValue;

      Property Saturation:Single Read _Saturation Write _Saturation;
      Property Glow:ColorRGBA Read _Glow Write _Glow;

      Property Pattern:TERRATexture Read _Pattern Write SetPattern;
      Property ColorTable:TERRATexture Read _ColorTable Write SetColorTable;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;

      Property Flags:Cardinal Read _Flags Write _Flags;

      Property Shader:ShaderInterface Read GetShader;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_ShaderManager, TERRA_Engine, TERRA_Log, TERRA_Image, TERRA_GraphicsManager, TERRA_OS, TERRA_Math
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  MaxVerticesPerBatch = 1024 * 32;

Var
  _SpriteShaders:Array[0..Pred(MaxSpriteShaders)] Of ShaderInterface;


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
  _Geometry := TERRAGeometry(Engine.Pool.Fetch(TERRAGeometry));
  If Assigned(_Geometry) Then
    _Geometry.Create() 
  Else
    _Geometry := TERRAGeometry.Create();

  Self.Clear();        
  Self.Reset();
End;

Procedure TERRASprite.Release();
Begin
  ReleaseObject(_Geometry);
End;

Procedure TERRASprite.GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType);
Begin
  Depth := Trunc(Self.Layer);
  AlphaType := Renderable_Blend;
  Layer := RenderableLayer_Default;
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
  _Transform := Matrix3x3_Multiply(Mat, _Transform);
End;

Procedure TERRASprite.ResetTransform();
Begin
  _Transform := Matrix3x3_Identity;
End;

Procedure TERRASprite.Translate(const X, Y: Single);
Begin
  Self.ConcatTransform(Matrix3x3_Translation(X, Y));
End;

procedure TERRASprite.Rotate(Angle: Single);
Begin
  Self.ConcatTransform(Matrix3x3_Rotation(Angle));
End;

Procedure TERRASprite.Scale(const Value: Single);
Begin
  Self.ConcatTransform(Matrix3x3_Scale(Value));
End;

Procedure TERRASprite.Scale(const X, Y: Single);
Begin
  Self.ConcatTransform(Matrix3x3_Scale(X, Y));
End;

Procedure TERRASprite.AddTriangle(Const PosA, PosB, PosC:Vector2D; LayerOffset:Single);
Var
  U1, V1, U2, V2:Single;
  IndexOffset, VertexOffset:Integer;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  VertexOffset := Geometry.Vertices.Count;
  IndexOffset := Geometry.Indices.Count;

  Geometry.Vertices.Resize(Geometry.Vertices.Count + 3);
  Geometry.Indices.Resize(Geometry.Indices.Count + 3);

  LayerOffset := LayerOffset + Self.Layer;

  Geometry.Vertices.SetColor(VertexOffset + 0, vertexColor, _CA); //SampleColorAt(PX, PY)
  Geometry.Vertices.SetVector3D(VertexOffset + 0, vertexPosition, Vector3D_Create(PosA.X, PosA.Y, LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 0, vertexUV0, Vector2D_Create(0.5, 0.5));

  Geometry.Vertices.SetColor(VertexOffset + 1, vertexColor, _CA); //SampleColorAt(PX, PY)
  Geometry.Vertices.SetVector3D(VertexOffset + 1, vertexPosition, Vector3D_Create(PosB.X, PosB.Y, LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 1, vertexUV0, Vector2D_Create(0.5, 0.5));

  Geometry.Vertices.SetColor(VertexOffset + 2, vertexColor, _CA); //SampleColorAt(PX, PY)
  Geometry.Vertices.SetVector3D(VertexOffset + 2, vertexPosition, Vector3D_Create(PosC.X, PosC.Y, LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 2, vertexUV0, Vector2D_Create(0.5, 0.5));

  Geometry.Indices.SetIndex(IndexOffset + 0, VertexOffset + 0);
  Geometry.Indices.SetIndex(IndexOffset + 1, VertexOffset + 1);
  Geometry.Indices.SetIndex(IndexOffset + 2, VertexOffset + 2);
End;

Procedure TERRASprite.AddEllipse(Const Anchor:SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const RadiusX, RadiusY: Single);
Const
  SubDivs = 32;

Var
  I:Integer;
  U1, V1, U2, V2:Single;
  Delta, PX, PY:Single;
  IndexOffset, VertexOffset:Integer;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  VertexOffset := Geometry.Vertices.Count;
  IndexOffset := Geometry.Indices.Count;

  Geometry.Vertices.Resize(Geometry.Vertices.Count + Succ(SubDivs));
  Geometry.Indices.Resize(Geometry.Indices.Count + 3 * SubDivs);

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
      Pos.Subtract(Vector2D_Create(RadiusX * 0.5, RadiusY * 0.5));

    spriteAnchor_BottomMiddle:
      Pos.Subtract(Vector2D_Create(RadiusX * 0.5, -RadiusY * 0.5));
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

    Geometry.Vertices.SetColor(VertexOffset + I, vertexColor, _CA); //SampleColorAt(PX, PY)
    Geometry.Vertices.SetVector3D(VertexOffset + I, vertexPosition, Vector3D_Create(Pos.X + RadiusX * PX, Pos.Y + + RadiusY * PY, LayerOffset));
    Geometry.Vertices.SetVector2D(VertexOffset + I, vertexUV0, Vector2D_Create(PX, PY));
  End;

  For I:=0 To Pred(SubDivs) Do
  Begin
    Geometry.Indices.SetIndex(IndexOffset + 0, VertexOffset + SubDivs);
    Geometry.Indices.SetIndex(IndexOffset + 1, VertexOffset + I);
    Geometry.Indices.SetIndex(IndexOffset + 2, VertexOffset + ((I + 1) Mod SubDivs));
    Inc(IndexOffset, 3);
  End;

  SetDefaultVertexValues(VertexOffset);
End;

Procedure TERRASprite.AddCircle(const Anchor: SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const Radius:Single);
Begin
  Self.AddEllipse(Anchor, Pos, LayerOffset, Radius, Radius);
End;

Procedure TERRASprite.AddQuad(Const Anchor:SpriteAnchor; Pos:Vector2D; LayerOffset:Single; Const Width, Height:Single; Const Skew:Single; Const Angle:Single; Const Scale:Single);
Var
  I:Integer;
  U1, V1, U2, V2:Single;
  PX, PY:Array[1..4] Of Single;
  TX, TY:Single;
  DX, DY:Single;
  IndexOffset, VertexOffset:Integer;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  VertexOffset := Geometry.Vertices.Count;
  IndexOffset := Geometry.Indices.Count;

  Geometry.Vertices.Resize(VertexOffset + 4);
  Geometry.Indices.Resize(IndexOffset + 6);

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

  PX[1] := 0;
  PY[1] := 1;

  PX[2] := 1;
  PY[2] := 1;

  PX[3] := 1;
  PY[3] := 0;

  PX[4] := 0;
  PY[4] := 0;

  Dx := Cos(Angle);
  Dy := Sin(Angle);

  For I:=1 To 4 Do
  Begin
    TX := PX[I];
    TY := PY[I];

    Case Anchor Of
      spriteAnchor_Center:
        Begin
          TX := TX - 0.5;
          TY := TY - 0.5;
        End;

      spriteAnchor_BottomMiddle:
        Begin
          TX := TX - 0.5;
        End;
    End;


    PX[I] := TX * Dx - TY * Dy;
    PY[I] := TX * Dy + TY * Dx;

    PX[I] := Pos.X + PX[I] * Width * Scale;
    PY[I] := Pos.Y + PY[I] * Height * Scale;
  End;

  PX[3] := PX[3] + Skew;
  PX[4] := PX[4] + Skew;

  Geometry.Vertices.SetColor(VertexOffset + 0, vertexColor, _CC);
  Geometry.Vertices.SetColor(VertexOffset + 1, vertexColor, _CD);
  Geometry.Vertices.SetColor(VertexOffset + 2, vertexColor, _CB);
  Geometry.Vertices.SetColor(VertexOffset + 3, vertexColor, _CA);

  Geometry.Vertices.SetVector3D(VertexOffset + 0, vertexPosition, Vector3D_Create(PX[1], PY[1], LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 0, vertexUV0, Vector2D_Create(U1, V2));

  Geometry.Vertices.SetVector3D(VertexOffset + 1, vertexPosition, Vector3D_Create(PX[2], PY[2], LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 1, vertexUV0, Vector2D_Create(U2, V2));

  Geometry.Vertices.SetVector3D(VertexOffset + 2, vertexPosition, Vector3D_Create(PX[3], PY[3], LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 2, vertexUV0, Vector2D_Create(U2, V1));

  Geometry.Vertices.SetVector3D(VertexOffset + 3, vertexPosition, Vector3D_Create(PX[4], PY[4], LayerOffset));
  Geometry.Vertices.SetVector2D(VertexOffset + 3, vertexUV0, Vector2D_Create(U1, V1));

  Geometry.Indices.SetIndex(IndexOffset + 0, VertexOffset + 0);
  Geometry.Indices.SetIndex(IndexOffset + 1, VertexOffset + 1);
  Geometry.Indices.SetIndex(IndexOffset + 2, VertexOffset + 2);

  Geometry.Indices.SetIndex(IndexOffset + 3, VertexOffset + 2);
  Geometry.Indices.SetIndex(IndexOffset + 4, VertexOffset + 3);
  Geometry.Indices.SetIndex(IndexOffset + 5, VertexOffset + 0);

  SetDefaultVertexValues(VertexOffset);
End;

Procedure TERRASprite.AddPath(Const Positions:Array Of Vector2D; LayerOffset:Single; Width:Single);
Var
  Normal, Tangent, Pos:Vector2D;
  U1, V1, U2, V2, TU:Single;
  Len, Mult:Single;
  I, A, B:Integer;
  IndexOffset, VertexOffset, BaseVertexOffset:Integer;
Begin
  If (Self._CA.A = 0) And (Self._CB.A = 0) And (Self._CC.A=0) And (Self._CD.A=0) Then
    Exit;

  If (Length(Positions)<2) Then
    Exit;

  Width := Width * 0.5;

  VertexOffset := Geometry.Vertices.Count;
  IndexOffset := Geometry.Indices.Count;
  BaseVertexOffset := VertexOffset;

  Geometry.Vertices.Resize(VertexOffset + 2 * Length(Positions));
  Geometry.Indices.Resize(IndexOffset + 6 * Length(Positions));

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
    Tangent := Vector2D_Create(-Normal.Y, Normal.X);

    Pos := Positions[I];

    Mult := Len / (Width*2);

    Geometry.Vertices.SetColor(VertexOffset + 0, vertexColor, _CC);
    Geometry.Vertices.SetColor(VertexOffset + 1, vertexColor, _CC);

    If (Odd(I)) Then
      TU := U2
    Else
      TU := U1;

    Geometry.Vertices.SetVector3D(VertexOffset + 0, vertexPosition, Vector3D_Create(Trunc(Pos.X + Tangent.X * Width), Trunc(Pos.Y + Tangent.Y * Width), LayerOffset));
    Geometry.Vertices.SetVector2D(VertexOffset + 0, vertexUV0, Vector2D_Create(TU * Mult, V1));

    Geometry.Vertices.SetVector3D(VertexOffset + 1, vertexPosition, Vector3D_Create(Trunc(Pos.X - Tangent.X * Width), Trunc(Pos.Y - Tangent.Y * Width), LayerOffset));
    Geometry.Vertices.SetVector2D(VertexOffset + 1, vertexUV0, Vector2D_Create(TU * Mult, V2));

    If (I>Low(Positions)) Then
    Begin
      Geometry.Indices.SetIndex(IndexOffset + 0, VertexOffset + 0);
      Geometry.Indices.SetIndex(IndexOffset + 1, VertexOffset + 1);
      Geometry.Indices.SetIndex(IndexOffset + 2, VertexOffset - 1);

      Geometry.Indices.SetIndex(IndexOffset + 3, VertexOffset - 1);
      Geometry.Indices.SetIndex(IndexOffset + 4, VertexOffset - 2);
      Geometry.Indices.SetIndex(IndexOffset + 5, VertexOffset + 0);

      Inc(IndexOffset, 6);
    End;

    Inc(VertexOffset, 2);
  End;

  SetDefaultVertexValues(BaseVertexOffset);
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
  Tangent := Vector2D_Create(-Normal.Y, Normal.X);

  Delta := (ArrowLength) / Len;
  If (Delta>1) Then
    Delta := 1;

  ArrowWidth := ArrowLength * 0.3333;

  ArrowCenter := Vector2D_Create(StartPos.X * Delta + EndPos.X * (1.0-Delta), StartPos.Y * Delta + EndPos.Y * (1.0-Delta));
  ArrowPoint := EndPos;//VectorAdd2D(EndPos, Vector2D_Create(Normal.X * ArrowWidth * 0.5, Normal.Y * ArrowWidth * 0.5));

  SideA := Vector2D_Add(ArrowCenter, Vector2D_Create(Tangent.X * ArrowWidth, Tangent.Y * ArrowWidth));
  SideB := Vector2D_Add(ArrowCenter, Vector2D_Create(Tangent.X * -ArrowWidth, Tangent.Y * -ArrowWidth));

  Self.AddLine(StartPos, ArrowCenter, LayerOffset, LineWidth);
  Self.AddTriangle(ArrowPoint, SideA, SideB, LayerOffset);
End;

Procedure TERRASprite.Clear;
Begin
  If Assigned(Geometry) Then
  Begin
    Geometry.Vertices.Resize(0);
    Geometry.Indices.Resize(0);
  End;

  _Flags := 0;
End;

Procedure TERRASprite.Reset();
Begin
  Self.ColorTable := Nil;
  Self.SetTexture(Nil);
  Self.ClipRect.Style := clipNothing;
  Self.Saturation := 1.0;
  Self.BlendMode := blendBlend;
  Self.Layer := 50.0;
  Self.Outline := ColorNull;
  Self.SetColor(ColorWhite);
  Self.Pattern := Nil;
  Self.SetUVs(0.0, 0.0, 1.0, 1.0);
  Self.Glow := ColorBlack;

  ResetTransform();
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

Procedure TERRASprite.SetDissolve(Mask:TERRATexture; Const Value:Single);
Begin
  Self._DissolveValue := Value;
  Self._DissolveTexture := Mask;

  If ((_Flags And Sprite_Dissolve<>0) <> Assigned(Mask)) Then
    Self._Flags := _Flags Xor Sprite_Dissolve;
End;

Procedure TERRASprite.SetColorTable(const Value: TERRATexture);
Begin
  Self._ColorTable := Value;

  If ((_Flags And Sprite_ColorGrading<>0) <> Assigned(Value)) Then
    Self._Flags := _Flags Xor Sprite_ColorGrading;
End;

Procedure TERRASprite.SetPattern(const Value: TERRATexture);
Begin
  _Pattern := Value;

  If ((_Flags And Sprite_Pattern<>0) <> Assigned(Value)) Then
    Self._Flags := _Flags Xor Sprite_Pattern;

  If Assigned(Value) Then
  Begin
    Value.WrapMode := wrapAll;
    Value.Filter := filterBilinear;
  End;
End;

Procedure TERRASprite.FullRemapUV();
Begin
  Self._U1 := 0.0;
  Self._V1 := 0.0;
  Self._U2 := 1.0;
  Self._V2 := 1.0;
End;

Procedure TERRASprite.PixelRemap(X1, Y1, X2, Y2:Integer; W, H: Integer);
Begin
  If (_Texture = Nil) Then
    Exit;

  _Texture.Prefetch();

  _U1 := (X1/_Texture.Width * _Texture.Ratio.X);
  _V1 := (Y1/_Texture.Height * _Texture.Ratio.Y);
  _U2 := (X2/_Texture.Width * _Texture.Ratio.X);
  _V2 := (Y2/_Texture.Height * _Texture.Ratio.Y);

(*  If (W>0) Then
    Self.Width := W
  Else
    Self.Width := IntMax(1, (Abs(X2-X1)));

  If (H>0) Then
    Self.Height := H
  Else
    Self.Height := IntMax(1, (Abs(Y2-Y1)));*)
End;

Procedure TERRASprite.TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
Var
  TX, TY:Integer;
  PX, PY:Single;
Begin
  If (_Texture = Nil) Then
    Exit;

  _Texture.Prefetch();

  PX := (1/(_Texture.Width / _Texture.Ratio.X));
  PY := (1/(_Texture.Height / _Texture.Ratio.Y));

  TX := (TileID Mod TilesPerRow);
  TY := (TileID Div TilesPerRow);
  _U1 := (TX/TilesPerRow + PX) * _Texture.Ratio.X;
  _U2 := (Succ(TX)/TilesPerRow - PX) * _Texture.Ratio.X;
  _V1 := (TY/TilesPerRow + PY) * _Texture.Ratio.Y;
  _V2 := (Succ(TY)/TilesPerRow - PY) * _Texture.Ratio.Y;

  (*Width := TileSize;
  Height := TileSize;*)
End;

Procedure TERRASprite.TileRemap(X, Y, TilesPerX, TilesPerY: Integer);
Var
  SX, SY:Single;
  TX,TY:Single;
Begin
  If (_Texture = Nil) Then
    Exit;

  SX := (_Texture.Width / _Texture.Ratio.X) / TilesPerX;
  SY := (_Texture.Height / _Texture.Ratio.Y) / TilesPerY;
  TX := SX*X;
  TY := SY*Y;

  _U1 := (TX/_Texture.Width * _Texture.Ratio.X);
  _V1 := (TY/_Texture.Height * _Texture.Ratio.Y);
  _U2 := (((TX+SX)-1)/_Texture.Width * _Texture.Ratio.X);
  _V2 := (((TY+SY)-1)/_Texture.Height * _Texture.Ratio.Y);
(*  Self.Width := Trunc(SX);
  Self.Height := Trunc(SY);*)
End;

Function TERRASprite.GetShader():ShaderInterface;
Begin
  If (_SpriteShaders[Self.Flags] = Nil) Then
  Begin
    _SpriteShaders[Self.Flags] := Engine.Graphics.Renderer.CreateShader();
    _SpriteShaders[Self.Flags].Generate('sprite_'+CardinalToString(Self.Flags), GetShader_Sprite(Self.Flags));
  End;

  Result := _SpriteShaders[Self.Flags];
End;

Procedure TERRASprite.Render(View:TERRAViewport; Const Stage:RendererStage);
Var
  Graphics:GraphicsManager;
  TargetShader:ShaderInterface;

  OutIt:VertexIterator;
  Dest:SpriteVertex;

  ModelTransform:Matrix4x4;
Begin
  If (Self.Geometry.Indices.Count <= 0) Or (Self.Texture = Nil) Then
    Exit;

(*  OutIt := Geometry.Vertices.GetIteratorForClass(SpriteVertex);
  While (OutIt.HasNext()) Do
  Begin
    Dest := SpriteVertex(OutIt.Value);
    Dest.Position := _Transform.Transform(Dest.Position);
  End;
  ReleaseObject(OutIt);*)

  ModelTransform := Matrix4x4_CreateFromMatrix3x3(Self.Transform);

  Graphics := Engine.Graphics;
  //glEnable(GL_DEPTH_TEST); {FIXME}
  //
  Graphics.Renderer.SetDepthTest(True);
  Graphics.Renderer.SetDepthFunction(compareLessOrEqual);

  Graphics.Renderer.SetCullMode(cullNone);

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SpriteVertex));
  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D,  @Geometry.Vertices[0].Position);
  Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @Geometry.Vertices[0].TexCoord);
  Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor,  @Geometry.Vertices[0].Color);

  If (_Saturation<1.0) Then
    Graphics.Renderer.SetAttributeSource(TERRA_SATURATION_ATTRIBUTE, typeFloat, @Geometry.Vertices[0].Saturation);}

  TargetShader := Self.Shader;
  Graphics.Renderer.BindShader(TargetShader);

  If (Self.Flags And Sprite_GUI = 0) Then
    ModelTransform := Matrix4x4_Multiply4x4(ModelTransform, View.Camera.Transform);

  Graphics.Renderer.SetModelMatrix(ModelTransform);

  Graphics.Renderer.SetProjectionMatrix(View.Camera.Projection);

  If (Self.Flags And Sprite_SolidColor = 0) Then
  Begin
    TargetShader.SetIntegerUniform('texture', 0);
    Self.Texture.Bind(0);
  End;

  If (Self.Flags And Sprite_Dissolve<>0) Then
  Begin
    TargetShader.SetIntegerUniform('dissolve_texture', 1);
    Self.DissolveTexture.WrapMode := wrapNothing;
    Self.DissolveTexture.Filter := filterLinear;
    Self.DissolveTexture.Bind(1);
    TargetShader.SetFloatUniform('dissolve_value', Self.DissolveValue);
  End;

  If (Self.Flags And Sprite_Pattern<>0) Then
  Begin
    TargetShader.SetIntegerUniform('pattern_texture', 1);
    Self.Pattern.Bind(1);
  End;

  If (Self.Flags And Sprite_Font<>0) Then
  Begin
    TargetShader.SetColorUniform('outlineColor', Self.Outline);
    TargetShader.SetVec2Uniform('shadowOffset', Vector2D_Create(0.1, 0.1));
    TargetShader.SetFloatUniform('smoothing', Self.Smoothing); //);
  End Else
  {$IFNDEF DISABLECOLORGRADING}
    ColorTableBind(Self.ColorTable, 1);
  {$ENDIF}

  Graphics.Renderer.SetBlendMode(Self.BlendMode);
//  Ratio := UIManager.Instance.Ratio;

  _Geometry.Render();

//  Graphics.Renderer.SetDepthTest(True);    //BIBI
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

{ TERRASpriteRenderer }
(*Procedure TERRASpriteRenderer.InitBatches;
Var
    I:Integer;
Begin
  _SpriteCount := 0;
  SetLength(_SpriteList, 64);
End;

Procedure TERRASpriteRenderer.Release;
Var
  I:Integer;
Begin
{  For I:=0 To Pred(_SpriteCount) Do
    ReleaseObject(_Sprites[I]);
    }
  _SpriteCount := 0;

  For I:=0 To Pred(MaxSpriteShaders) Do
    ReleaseObject(_SpriteShaders[I]);
End;

Procedure TERRASpriteRenderer.QueueSprite(S:TERRASprite);
Var
  N, I:Integer;
  TargetLayer:Single;
  HasShaders, ResetBatch:Boolean;
Begin
  If (S = Nil) Then
    Exit;

  If (S.Texture = Nil) Or (S.BatchID>0) Or (S.IndexCount<=0) Then
    Exit;

{  If (_BatchCount<=0) Then
    Self.InitBatches();

  HasShaders := Engine.Graphics.Renderer.Features.Shaders.Avaliable;

  ResetBatch := True;

  TargetLayer := S.Layer * 0.01;
  N := -1;

  For I:=0 To Pred(_BatchCount) Do
  If (_Batches[I]._Texture = S.Texture) And (_Batches[I]._Pattern = S.Pattern)
  And (_Batches[I]._DissolveTexture = S.DissolveTexture) And (_Batches[I]._DissolveValue = S.DissolveValue) And (_Batches[I]._BlendMode = S.BlendMode)
  {$IFNDEF DISABLECOLORGRADING}And (_Batches[I]._ColorTable = S.ColorTable){$ENDIF}
  And (_Batches[I]._ShaderID = S.Flags)
  And ( (HasShaders) Or (_Batches[I]._Saturation = S.Saturation))
  And (Cardinal(_Batches[I]._Outline) = Cardinal(S.Outline))
  And (_Batches[I]._RequiredVertices + S.Vertices.Count < MaxVerticesPerBatch)
  And (_Batches[I]._Smoothing = S.Smoothing) And (_Batches[I]._Layer = TargetLayer) Then
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
    _Batches[N]._Pattern  := S.Pattern;
    _Batches[N]._DissolveTexture  := S.DissolveTexture;
    _Batches[N]._DissolveValue  := S.DissolveValue;
    _Batches[N]._Layer := TargetLayer;
    {$IFNDEF DISABLECOLORGRADING}
    _Batches[N]._ColorTable := S.ColorTable;
    {$ENDIF}
    _Batches[N]._Saturation := S.Saturation;
    _Batches[N]._Glow := S.Glow;
    _Batches[N]._Smoothing := S.Smoothing;
    _Batches[N]._ShaderID := S.Flags;
    _Batches[N]._Outline := S.Outline;
    _Batches[N]._First := Nil;
    _Batches[N]._RequiredVertices := 0;
    _Batches[N]._RequiredIndices := 0;
  End;

  _Batches[N].AddSprite(S);}

  Inc(_SpriteCount);
  If (Length(_SpriteList) < _SpriteCount) Then
    SetLength(_SpriteList, _SpriteCount);

  _SpriteList[Pred(_SpriteCount)] := S;
End;

Procedure TERRASpriteRenderer.Prepare;
Var
  I:Integer;
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;

{  If (_SpriteShaderWithoutGrading = Nil) Then
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
  End;}

  {
  For I:=0 To Pred(_BatchCount) Do
  Begin
    _Batches[I].Prepare();
  End;

  _Index := -1;}
End;

Procedure TERRASpriteRenderer.Clear;
Var
  I:Integer;
Begin
{  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) Then
  Begin
    _Batches[I].Clear();
  End;}

  _SpriteCount := 0;
End;

Procedure TERRASpriteRenderer.Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
Var
  I,K:Integer;
  Min:Single;
  Total, Index:Integer;
  M:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;
  Graphics.Renderer.SetBlendMode(blendNone);

  //glEnable(GL_DEPTH_TEST); {FIXME}
  Graphics.Renderer.SetDepthFunction(compareLessOrEqual);

  Graphics.Renderer.SetCullMode(cullNone);
//  Graphics.Renderer.SetColorMask(True, True, True, False);

  {If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
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
  }


  Total := _SpriteCount;

  While (Total>0) Do
  Begin
    Index := -1;
    Min := 9999;

    For I:=0 To Pred(_SpriteCount) Do
    If (_SpriteList[I].Layer<Min) Then
    Begin
      Min := _SpriteList[I].Layer;
      Index := I;
    End;

    If (Index>=0) Then
    Begin
      RenderSprite(_SpriteList[Index], ProjectionMatrix, TransformMatrix, Stage);
      Dec(Total);
    End Else
      Break;
  End;


  Graphics.Renderer.SetDepthTest(True);    //BIBI

  {If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;}
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
*)

Procedure TERRASprite.SetDefaultVertexValues(StartOfs: Integer);
Var
  CurrentClip:Vector4D;
  OutIt:VertexIterator;
  Dest:SpriteVertex;
Begin
  If (Self.ClipRect.Style = clipNothing) Then
    CurrentClip := Vector4D_Create(0, 0, 9999, 9999)
  Else
    CurrentClip := Vector4D_Create(Self.ClipRect.X1, Self.ClipRect.Y1, Self.ClipRect.X2, Self.ClipRect.Y2);

  OutIt := Geometry.Vertices.GetIteratorForClass(SpriteVertex);
  OutIt.Seek(StartOfs);
  While (OutIt.HasNext()) Do
  Begin
    Dest := SpriteVertex(OutIt.Value);
    Dest.Saturation := Self.Saturation;
    Dest.Glow := Self.Glow;
    Dest.ClipRect := CurrentClip;
  End;
  ReleaseObject(OutIt);
End;

Procedure TERRASprite.GetMaterialDetails(out Shader: ShaderInterface; out Texture: TERRATexture);
Begin
  Texture := Self.Texture;
  Shader := Self.Shader;
End;

End.
