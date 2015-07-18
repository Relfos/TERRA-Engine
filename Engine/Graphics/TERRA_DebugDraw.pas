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
 * TERRA_DebugDraw
 * Implements debug drawing objects
 ***********************************************************************************************************************
}
Unit TERRA_DebugDraw;

{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_String, TERRA_GraphicsManager, TERRA_Renderer, TERRA_Color, TERRA_BoundingBox, TERRA_Frustum,
  TERRA_Ray, TERRA_Matrix4x4, TERRA_Vector3D, TERRA_Vector2D, TERRA_Utils, TERRA_SpriteManager,
  TERRA_MeshSkeleton, TERRA_MeshAnimationNodes, TERRA_Collision2D, TERRA_Splines, TERRA_ClipRect, TERRA_Viewport;

// 2d drawing
Procedure DrawPoint2D(View:TERRAViewport; Const P:Vector2D; FillColor:Color; Radius:Single = 2.0);
Procedure DrawLine2D(View:TERRAViewport; Const A,B:Vector2D; LineColor:Color; LineWidth:Single = 1.0);
Procedure DrawCircle(View:TERRAViewport; Const P:Vector2D; Radius:Single; LineColor:Color; LineWidth:Single = 1.0);
Procedure DrawRectangle(View:TERRAViewport; Const A,B:Vector2D; LineColor:Color; LineWidth:Single = 1.0);
Procedure DrawFilledRect(View:TERRAViewport; Const A,B:Vector2D; FillColor:Color);
Procedure DrawPolygon2D(View:TERRAViewport; Poly:Polygon2D; LineColor:Color; LineWidth:Single = 1.0);

// 3d drawing
Procedure DrawPoint3D(View:TERRAViewport; Const P:Vector3D; FillColor:Color; Radius:Single = 2.0);
Procedure DrawLine3D(View:TERRAViewport; Const A,B:Vector3D; LineColor:Color; LineWidth:Single = 1.0);
Procedure DrawRay(View:TERRAViewport; Const R:Ray; LineColor:Color; LineWidth:Single = 1.0; Length:Single =0);
Procedure DrawBoundingBox(View:TERRAViewport; Const MyBox:BoundingBox; LineColor:Color; LineWidth:Single = 1.0);
Procedure DrawSpline(View:TERRAViewport; S:Spline; LineColor:Color; LineWidth:Single = 1.0);

Procedure DrawFrustum(View:TERRAViewport; F:Frustum; LineColor:Color; LineWidth:Single = 1.0);

Procedure DrawBone(View:TERRAViewport; Bone:MeshBone; State:AnimationState; Const Transform:Matrix4x4; LineColor:Color; LineWidth:Single);
Procedure DrawSkeleton(View:TERRAViewport; Skeleton:MeshSkeleton; State:AnimationState; Const Transform:Matrix4x4; LineColor:Color; LineWidth:Single);

(*Procedure DrawFrustum(Const MyFrustum:Frustum; Color:TERRA_Color.Color);
Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);
Procedure DrawPointCloud(Cloud:PointCloud2D; MyColor:Color; Layer:Single);
*)

Implementation
Uses TERRA_OS, TERRA_Math, TERRA_Texture;

Const
  Layer = 99;

Function ConvertTo2D(View:TERRAViewport; P:Vector3D):Vector2D;
Begin
  P := View.ProjectPoint(P);
  Result := VectorCreate2D(P.X, P.Y);
End;

Procedure DrawPoint2D(View:TERRAViewport; Const P:Vector2D; FillColor:Color; Radius:Single = 2.0);
Var
  A,B:Vector2D;
Begin
  Radius := Radius * 0.5;
  A.X := P.X - Radius;
  A.Y := P.Y - Radius;
  B.X := P.X + Radius;
  B.Y := P.Y + Radius;

  DrawFilledRect(View, A, B, FillColor);
End;

Procedure DrawLine2D(View:TERRAViewport; Const A,B:Vector2D; LineColor:Color; LineWidth:Single);
Var
  Tex:TERRATexture;
  DX, DY, Angle, Len:Single;
  S:QuadSprite;
Begin
  Tex := TextureManager.Instance.WhiteTexture;
  If Tex = Nil Then
    Exit;

  DX := B.X - A.X;
  DY := A.Y - B.Y;
  Angle := Atan2(DY, DX);

  S := SpriteManager.Instance.DrawSprite(Trunc(A.X), Trunc(A.Y), Layer, Tex);
  S.SetColor(LineColor);
  S.Rect.Width := Trunc(A.Distance(B));
  S.Rect.Height := Trunc(LineWidth);
  S.SetScaleAndRotation(1.0, Angle);
End;

Procedure DrawFilledRect(View:TERRAViewport; Const A,B:Vector2D; FillColor:Color);
Var
  I:Integer;
  Tex:TERRATexture;
  MinX, MinY, MaxX, MaxY:Single;
  S:QuadSprite;
Begin
  Tex := TextureManager.Instance.WhiteTexture;
  If Tex = Nil Then
    Exit;

  Tex.WrapMode := wrapAll;
  MinX := Trunc(FloatMin(A.X, B.X));
  MinY := Trunc(FloatMin(A.Y, B.Y));
  MaxX := Trunc(FloatMax(A.X, B.X));
  MaxY := Trunc(FloatMax(A.Y, B.Y));

  S := SpriteManager.Instance.DrawSprite(MinX, MinY, Layer, Tex);
  S.SetColor(FillColor);
  S.Rect.Width := Trunc(MaxX-MinX);
  S.Rect.Height := Trunc(MaxY-MinY);
//  S.ClipRect := Clip;
End;

Procedure DrawRectangle(View:TERRAViewport; Const A,B:Vector2D; LineColor:Color; LineWidth:Single = 1.0);
Var
  I:Integer;
  Tex:TERRATexture;
  MinX, MinY, MaxX, MaxY:Single;
  S:QuadSprite;
Begin
  Tex := TextureManager.Instance.WhiteTexture;
  If Tex = Nil Then
    Exit;

  Tex.WrapMode := wrapAll;
  MinX := Trunc(FloatMin(A.X, B.X));
  MinY := Trunc(FloatMin(A.Y, B.Y));
  MaxX := Trunc(FloatMax(A.X, B.X));
  MaxY := Trunc(FloatMax(A.Y, B.Y));

  S := SpriteManager.Instance.DrawSprite(MinX, MinY, Layer, Tex);
  S.SetColor(LineColor);
  S.Rect.Width := Trunc(MaxX-MinX);
  S.Rect.Height := Trunc(LineWidth);
//  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MinX, MinY, Layer, Tex);
  S.SetColor(LineColor);
  S.Rect.Width := Trunc(LineWidth);
  S.Rect.Height := Trunc(MaxY-MinY);
//  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MinX, MaxY, Layer, Tex);
  S.SetColor(LineColor);
  S.Rect.Width := Trunc(MaxX-MinX);
  S.Rect.Height := Trunc(LineWidth);
//  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MaxX, MinY, Layer, Tex);
  S.SetColor(LineColor);
  S.Rect.Width := Trunc(LineWidth);
  S.Rect.Height := Trunc(MaxY-MinY);
//  S.ClipRect := Clip;
End;

Procedure DrawCircle(View:TERRAViewport; Const P:Vector2D; Radius:Single; LineColor:Color; LineWidth:Single = 1.0);
Const
  SubDivs = 32;
Var
  I:Integer;
  A, B:Vector2D;
  DX, DY, Angle:Single;
Begin
  DX := 1.0;
  DY := 0.0;

  For I:=1 To SubDivs Do
  Begin
    Angle := (I/SubDivs) * PI * 2;
    A := VectorCreate2D(P.X + DX * Radius, P.Y + DY * Radius);

    DX := Cos(Angle);
    DY := Sin(Angle);
    B := VectorCreate2D(P.X + DX * Radius, P.Y + DY * Radius);

    DrawLine2D(View, A, B, LineColor, LineWidth);
  End;
End;

Procedure DrawPolygon2D(View:TERRAViewport; Poly:Polygon2D; LineColor:Color; LineWidth:Single);
Var
  I:Integer;
Begin
  If (Poly.VertexCount<=0) Then
    Exit;

  For I:=0 To Pred(Poly.VertexCount) Do
    DrawLine2D(View, Poly.Vertices[I], Poly.Vertices[Succ(I) Mod Poly.VertexCount], LineColor, LineWidth);
End;

Procedure DrawPoint3D(View:TERRAViewport; Const P:Vector3D; FillColor:Color; Radius:Single);
Begin
  DrawPoint2D(View, ConvertTo2D(View, P), FillColor, Radius);
End;

Procedure DrawLine3D(View:TERRAViewport; Const A,B:Vector3D; LineColor:Color; LineWidth:Single);
Begin
  DrawLine2D(View, ConvertTo2D(View, A), ConvertTo2D(View, B), LineColor, LineWidth);
End;

Procedure DrawRay(View:TERRAViewport; Const R:Ray; LineColor:Color; LineWidth:Single = 1.0; Length:Single =0);
Var
  P:Vector3D;
Begin
  If Length<=0 Then
    Length := 9999;

  P := VectorAdd(R.Origin, VectorScale(R.Direction, Length));

  DrawLine3D(View, R.Origin, P, LineColor, LineWidth);
End;

Procedure DrawBoundingBox(View:TERRAViewport; Const MyBox:BoundingBox; LineColor:Color; LineWidth:Single);
Var
  Min, Max:Vector3D;
  Points:Array[0..7] Of Vector3D;
Begin
  Min := MyBox.StartVertex;
  Max := MyBox.EndVertex;

  Points[0] := VectorCreate(Min.X, Min.Y, Min.Z);
  Points[1] := VectorCreate(Max.X, Min.Y, Min.Z);
  Points[2] := VectorCreate(Min.X, Min.Y, Max.Z);
  Points[3] := VectorCreate(Max.X, Min.Y, Max.Z);

  Points[4] := VectorCreate(Min.X, Max.Y, Min.Z);
  Points[5] := VectorCreate(Max.X, Max.Y, Min.Z);
  Points[6] := VectorCreate(Min.X, Max.Y, Max.Z);
  Points[7] := VectorCreate(Max.X, Max.Y, Max.Z);

  DrawLine3D(View, Points[0], Points[1], LineColor, LineWidth);
  DrawLine3D(View, Points[0], Points[2], LineColor, LineWidth);
  DrawLine3D(View, Points[1], Points[3], LineColor, LineWidth);
  DrawLine3D(View, Points[2], Points[3], LineColor, LineWidth);
  DrawLine3D(View, Points[4], Points[5], LineColor, LineWidth);
  DrawLine3D(View, Points[4], Points[6], LineColor, LineWidth);
  DrawLine3D(View, Points[5], Points[7], LineColor, LineWidth);
  DrawLine3D(View, Points[6], Points[7], LineColor, LineWidth);
  DrawLine3D(View, Points[0], Points[4], LineColor, LineWidth);
  DrawLine3D(View, Points[1], Points[5], LineColor, LineWidth);
  DrawLine3D(View, Points[2], Points[6], LineColor, LineWidth);
  DrawLine3D(View, Points[3], Points[7], LineColor, LineWidth);
End;

Procedure DrawSpline(View:TERRAViewport; S:Spline; LineColor:Color; LineWidth:Single = 1.0);
Const
  SubDivs = 50;
Var
  I:Integer;
  A, B:Vector3D;
Begin
  If (S.PointCount<=1) Then
    Exit;

  S.Update();

  For I:=1 To SubDivs Do
  Begin
    A := S.GetPosition(((I-1)/SubDivs));
    B := S.GetPosition((I/SubDivs));

    DrawLine3D(View, A, B, LineColor, LineWidth);
  End;
End;

Procedure DrawBone(View:TERRAViewport; Bone:MeshBone; State:AnimationState; Const Transform:Matrix4x4; LineColor:Color; LineWidth:Single);
Var
  A,B:Vector3D;
Begin
  If (Bone = Nil) Or (Bone.Parent = Nil) Then
    Exit;

  A := State.Transforms[Bone.Index+1].Transform(VectorZero);
  B := State.Transforms[Bone.Parent.Index+1].Transform(VectorZero);

  DrawLine3D(View, A, B, LineColor, LineWidth);
End;

Procedure DrawSkeleton(View:TERRAViewport; Skeleton:MeshSkeleton; State:AnimationState; Const Transform:Matrix4x4; LineColor:Color; LineWidth:Single);
Var
  I:Integer;
Begin
  If (Skeleton = Nil) Then
    Exit;

  For I:=0 To Pred(Skeleton.BoneCount) Do
    DrawBone(View, Skeleton.GetBone(I), State, Transform, LineColor, LineWidth);
End;

Procedure DrawFrustum(View:TERRAViewport; F:Frustum; LineColor:Color; LineWidth:Single);
Var
  V:BoundingBoxVertices;
  P:Array[0..3] Of Vector3D;
  I:Integer;
Begin
  V := F.Vertices;

	P[0] := VectorCreate(V[2].X, V[2].Y, V[2].z);
	P[1] := VectorCreate(V[1].X, V[1].Y, V[1].z);
  P[2] := VectorCreate(V[3].X, V[3].Y, V[3].z);
  P[3] := VectorCreate(V[4].X, V[4].Y, V[4].z);
	For I:=1 To 3 Do
    DrawLine3D(View, P[I-1], P[I], LineColor, LineWidth);

	P[0] := VectorCreate(V[6].X, V[6].Y, V[6].z);
	P[1] := VectorCreate(V[5].X, V[5].Y, V[5].z);
  P[2] := VectorCreate(V[7].X, V[7].Y, V[7].z);
  P[3] := VectorCreate(V[8].X, V[8].Y, V[8].z);
	For I:=1 To 3 Do
    DrawLine3D(View, P[I-1], P[I], LineColor, LineWidth);

	P[0] := VectorCreate(V[1].X, V[1].Y, V[1].z);
	P[1] := VectorCreate(V[3].X, V[3].Y, V[3].z);
  P[2] := VectorCreate(V[7].X, V[7].Y, V[7].z);
  P[3] := VectorCreate(V[5].X, V[5].Y, V[5].z);
	For I:=1 To 3 Do
    DrawLine3D(View, P[I-1], P[I], LineColor, LineWidth);

	P[0] := VectorCreate(V[2].X, V[2].Y, V[2].z);
	P[1] := VectorCreate(V[4].X, V[4].Y, V[4].z);
  P[2] := VectorCreate(V[8].X, V[8].Y, V[8].z);
  P[3] := VectorCreate(V[6].X, V[6].Y, V[6].z);
	For I:=1 To 3 Do
    DrawLine3D(View, P[I-1], P[I], LineColor, LineWidth);
End;


(*
Procedure DrawPointCloud(Cloud:PointCloud2D; MyColor:Color; Layer:Single);
Var
  I:Integer;
Begin
  glPointSize(3.0);

  glBegin(GL_POINTS);
  glColor4ub(MyColor.R, MyColor.G, MyColor.B, MyColor.A);
  For I:=0 To Pred(Cloud.PointCount) Do
  Begin
    glVertex3f(Cloud.Points[I].X, Cloud.Points[I].Y, -Layer);
  End;
  glEnd();
End;

Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);
Var
  U,V:Vector3D;
  A,B,C,D:Vector3D;
Begin
  If (Abs(Normal.Y)>Abs(Normal.X)) And (Abs(Normal.Y)>Abs(Normal.Z)) Then
  Begin
    U := VectorCreate(Normal.X, Normal.Z, Normal.Y);
  End Else
  Begin
    U := VectorCreate(Normal.Z, Normal.Y, Normal.X);
  End;
  V := VectorCross(Normal, U);

  A := VectorAdd(VectorScale(U, Scale), VectorScale(V, Scale));
  B := VectorAdd(VectorScale(U, -Scale), VectorScale(V, Scale));
  C := VectorAdd(VectorScale(U, -Scale), VectorScale(V, -Scale));
  D := VectorAdd(VectorScale(U, Scale), VectorScale(V, -Scale));


  GraphicsManager.Instance.EnableColorShader(Color, Matrix4x4Translation(Position));

  	glBegin(GL_QUADS);
  	glVertex3f(A.x, A.y, A.z);
  	glVertex3f(B.x, B.y, B.z);
  	glVertex3f(C.x, C.y, C.z);
  	glVertex3f(D.x, D.y, D.z);
  	glEnd();
End;

*)


End.
