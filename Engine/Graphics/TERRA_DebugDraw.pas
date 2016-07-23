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
  TERRA_Ray, TERRA_Matrix4x4, TERRA_Vector3D, TERRA_Vector2D, TERRA_Utils, TERRA_Sprite,
  TERRA_MeshSkeleton, TERRA_MeshAnimationNodes, TERRA_Collision2D, TERRA_Splines, TERRA_ClipRect, TERRA_Viewport;

Var
  DrawLayer:Single = 99;
  DebugClipRect:TERRAClipRect;

// 2d drawing
Procedure DrawPoint2D(SourceView, TargetView:TERRAViewport; Const P:Vector2D; FillColor:ColorRGBA; Radius:Single = 2.0);
Procedure DrawLine2D(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawCircle(SourceView, TargetView:TERRAViewport; Const P:Vector2D; Radius:Single; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawRectangle(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawFilledRect(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; FillColor:ColorRGBA);
Procedure DrawPolygon2D(SourceView, TargetView:TERRAViewport; Poly:Polygon2D; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawClipRect(SourceView, TargetView:TERRAViewport; Const Rect:TERRAClipRect; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawPointCloud(SourceView, TargetView:TERRAViewport; Cloud:PointCloud2D; Color:ColorRGBA);

// 3d drawing
Procedure DrawPoint3D(SourceView, TargetView:TERRAViewport; Const P:Vector3D; FillColor:ColorRGBA; Radius:Single = 2.0);
Procedure DrawLine3D(SourceView, TargetView:TERRAViewport; Const A,B:Vector3D; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawRay(SourceView, TargetView:TERRAViewport; Const R:TERRARay; LineColor:ColorRGBA; LineWidth:Single = 1.0; Length:Single =0);
Procedure DrawBoundingBox(SourceView, TargetView:TERRAViewport; Const MyBox:BoundingBox; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawSpline(SourceView, TargetView:TERRAViewport; S:Spline; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Procedure DrawAxis(SourceView,  TargetView:TERRAViewport; Const Origin, Normal:Vector3D; LineWidth:Single = 1.0);

Procedure DrawFrustum(SourceView, TargetView:TERRAViewport; F:Frustum; LineColor:ColorRGBA; LineWidth:Single = 1.0);

Procedure DrawBone(SourceView, TargetView:TERRAViewport; Bone:MeshBone; State:AnimationState; Const Transform:Matrix4x4; LineColor:ColorRGBA; LineWidth:Single);
Procedure DrawSkeleton(SourceView, TargetView:TERRAViewport; Skeleton:MeshSkeleton; State:AnimationState; Const Transform:Matrix4x4; LineColor:ColorRGBA; LineWidth:Single);

(*Procedure DrawFrustum(Const MyFrustum:Frustum; Color:TERRA_Color.Color);
Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);
*)

Implementation
Uses TERRA_OS, TERRA_Engine, TERRA_Math, TERRA_Texture;

Function ConvertTo2D(SourceView:TERRAViewport; P:Vector3D):Vector2D;
Begin
  P := SourceView.ProjectPoint(P);
  Result := Vector2D_Create(P.X, P.Y);
End;

Procedure DrawPoint2D(SourceView, TargetView:TERRAViewport; Const P:Vector2D; FillColor:ColorRGBA; Radius:Single = 2.0);
Var
  A,B:Vector2D;
Begin
  Radius := Radius * 0.5;
  A.X := P.X - Radius;
  A.Y := P.Y - Radius;
  B.X := P.X + Radius;
  B.Y := P.Y + Radius;

  DrawFilledRect(SourceView, TargetView, A, B, FillColor);
End;

Procedure DrawLine2D(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; LineColor:ColorRGBA; LineWidth:Single);
Var
  Tex:TERRATexture;
  S:TERRASprite;
Begin
  Tex := Engine.Textures.WhiteTexture;
  If Tex = Nil Then
    Exit;

  S := Engine.FetchSprite();
  S.ClipRect := DebugClipRect;
  S.SetTexture(Tex);
  S.Layer := DrawLayer;
  S.SetColor(LineColor);
  S.AddLine(A, B, 0.0, LineWidth);
  Engine.Graphics.AddRenderable(TargetView, S);
End;


Procedure DrawFilledRect(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; FillColor:ColorRGBA);
Var
  I:Integer;
  Tex:TERRATexture;
  MinX, MinY, MaxX, MaxY:Single;
  S:TERRASprite;
Begin
  Tex := Engine.Textures.WhiteTexture;
  If Tex = Nil Then
    Exit;

  Tex.WrapMode := wrapAll;
  MinX := Trunc(FloatMin(A.X, B.X));
  MinY := Trunc(FloatMin(A.Y, B.Y));
  MaxX := Trunc(FloatMax(A.X, B.X));
  MaxY := Trunc(FloatMax(A.Y, B.Y));

  S := Engine.FetchSprite();
//  S.ClipRect := DebugClipRect;
  S.SetTexture(Tex);
  S.Layer := DrawLayer;
  S.SetColor(FillColor);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0,0), 0.0, Trunc(MaxX-MinX), Trunc(MaxY-MinY)); // colors
  S.Translate(MinX, MinY);

  Engine.Graphics.AddRenderable(TargetView, S);
End;

Procedure DrawRectangle(SourceView, TargetView:TERRAViewport; Const A,B:Vector2D; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Var
  I:Integer;
  Tex:TERRATexture;
  MinX, MinY, MaxX, MaxY:Single;
  S:TERRASprite;
Begin
  Tex := Engine.Textures.WhiteTexture;
  If Tex = Nil Then
    Exit;

  Tex.WrapMode := wrapAll;
  MinX := Trunc(FloatMin(A.X, B.X));
  MinY := Trunc(FloatMin(A.Y, B.Y));
  MaxX := Trunc(FloatMax(A.X, B.X));
  MaxY := Trunc(FloatMax(A.Y, B.Y));

  If (MinX = MaxX) Then
  Begin
    If (MinY = MaxY) Then
      DrawPoint2D(SourceView, TargetView, A, LineColor, LineWidth)
    Else
      DrawLine2D(SourceView, TargetView, A, B, LineColor, LineWidth);

    Exit;
  End Else
  If (MinY = MaxY) Then
  Begin
    DrawLine2D(SourceView, TargetView, A, B, LineColor, LineWidth);
    Exit;
  End;

  DrawLine2D(SourceView,  TargetView, Vector2D_Create(MinX, MinY), Vector2D_Create(MaxX, MinY), LineColor, LineWidth);
  DrawLine2D(SourceView,  TargetView, Vector2D_Create(MaxX, MinY), Vector2D_Create(MaxX, MaxY), LineColor, LineWidth);
  DrawLine2D(SourceView,  TargetView, Vector2D_Create(MaxX, MaxY), Vector2D_Create(MinX, MaxY), LineColor, LineWidth);
  DrawLine2D(SourceView,  TargetView, Vector2D_Create(MinX, MaxY), Vector2D_Create(MinX, MinY), LineColor, LineWidth);
End;

Procedure DrawClipRect(SourceView, TargetView:TERRAViewport; Const Rect:TERRAClipRect; LineColor:ColorRGBA; LineWidth:Single = 1.0);
Begin
  DrawRectangle(SourceView,  TargetView, Vector2D_Create(Rect.X1, Rect.Y1), Vector2D_Create(Rect.X2, Rect.Y2), LineColor, LineWidth);
End;

Procedure DrawCircle(SourceView, TargetView:TERRAViewport; Const P:Vector2D; Radius:Single; LineColor:ColorRGBA; LineWidth:Single = 1.0);
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
    A := Vector2D_Create(P.X + DX * Radius, P.Y + DY * Radius);

    DX := Cos(Angle);
    DY := Sin(Angle);
    B := Vector2D_Create(P.X + DX * Radius, P.Y + DY * Radius);

    DrawLine2D(SourceView, TargetView, A, B, LineColor, LineWidth);
  End;
End;

Procedure DrawPolygon2D(SourceView, TargetView:TERRAViewport; Poly:Polygon2D; LineColor:ColorRGBA; LineWidth:Single);
Var
  I:Integer;
Begin
  If (Poly.VertexCount<=0) Then
    Exit;

  For I:=0 To Pred(Poly.VertexCount) Do
    DrawLine2D(SourceView,  TargetView, Poly.Vertices[I], Poly.Vertices[Succ(I) Mod Poly.VertexCount], LineColor, LineWidth);
End;

Procedure DrawPoint3D(SourceView, TargetView:TERRAViewport; Const P:Vector3D; FillColor:ColorRGBA; Radius:Single);
Begin
  DrawPoint2D(SourceView,  TargetView, ConvertTo2D(SourceView, P), FillColor, Radius);
End;

Procedure DrawLine3D(SourceView, TargetView:TERRAViewport; Const A,B:Vector3D; LineColor:ColorRGBA; LineWidth:Single);
Begin
  DrawLine2D(SourceView,  TargetView, ConvertTo2D(SourceView, A), ConvertTo2D(SourceView, B), LineColor, LineWidth);
End;

Procedure DrawRay(SourceView, TargetView:TERRAViewport; Const R:TERRARay; LineColor:ColorRGBA; LineWidth:Single = 1.0; Length:Single =0);
Var
  P:Vector3D;
Begin
  If Length<=0 Then
    Length := 9999;

  P := Vector3D_Add(R.Origin, Vector3D_Scale(R.Direction, Length));

  DrawLine3D(SourceView,  TargetView, R.Origin, P, LineColor, LineWidth);
End;

Procedure DrawBoundingBox(SourceView, TargetView:TERRAViewport; Const MyBox:BoundingBox; LineColor:ColorRGBA; LineWidth:Single);
Var
  Min, Max:Vector3D;
  Points:Array[0..7] Of Vector3D;
Begin
  Min := MyBox.StartVertex;
  Max := MyBox.EndVertex;

  Points[0] := Vector3D_Create(Min.X, Min.Y, Min.Z);
  Points[1] := Vector3D_Create(Max.X, Min.Y, Min.Z);
  Points[2] := Vector3D_Create(Min.X, Min.Y, Max.Z);
  Points[3] := Vector3D_Create(Max.X, Min.Y, Max.Z);

  Points[4] := Vector3D_Create(Min.X, Max.Y, Min.Z);
  Points[5] := Vector3D_Create(Max.X, Max.Y, Min.Z);
  Points[6] := Vector3D_Create(Min.X, Max.Y, Max.Z);
  Points[7] := Vector3D_Create(Max.X, Max.Y, Max.Z);

  DrawLine3D(SourceView,  TargetView, Points[0], Points[1], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[0], Points[2], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[1], Points[3], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[2], Points[3], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[4], Points[5], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[4], Points[6], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[5], Points[7], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[6], Points[7], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[0], Points[4], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[1], Points[5], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[2], Points[6], LineColor, LineWidth);
  DrawLine3D(SourceView,  TargetView, Points[3], Points[7], LineColor, LineWidth);
End;

Procedure DrawSpline(SourceView, TargetView:TERRAViewport; S:Spline; LineColor:ColorRGBA; LineWidth:Single = 1.0);
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

    DrawLine3D(SourceView, TargetView, A, B, LineColor, LineWidth);
  End;
End;

Procedure DrawBone(SourceView, TargetView:TERRAViewport; Bone:MeshBone; State:AnimationState; Const Transform:Matrix4x4; LineColor:ColorRGBA; LineWidth:Single);
Var
  A,B:Vector3D;
  M:Matrix4x4;
Begin
  If (Bone = Nil) Or (Bone.Parent = Nil) Then
    Exit;

  If Assigned(State) Then
    M := Matrix4x4_Multiply4x3(Transform, State.GetAbsoluteMatrix(Bone.ID))
  Else
    M := Matrix4x4_Multiply4x3(Transform, Bone.AbsoluteMatrix);

  A := M.Transform(Vector3D_Zero);

  If Assigned(State) Then
    M := Matrix4x4_Multiply4x3(Transform, State.GetAbsoluteMatrix(Bone.Parent.ID))
  Else
    M := Matrix4x4_Multiply4x3(Transform, Bone.Parent.AbsoluteMatrix);
    
  B := M.Transform(Vector3D_Zero);

  DrawLine3D(SourceView, TargetView, A, B, LineColor, LineWidth);
End;

Procedure DrawSkeleton(SourceView, TargetView:TERRAViewport; Skeleton:MeshSkeleton; State:AnimationState; Const Transform:Matrix4x4; LineColor:ColorRGBA; LineWidth:Single);
Var
  I:Integer;
Begin
  If (Skeleton = Nil) Then
    Exit;

  For I:=1 To Pred(Skeleton.BoneCount) Do
    DrawBone(SourceView,  TargetView, Skeleton.GetBoneByIndex(I), State, Transform, LineColor, LineWidth);
End;

Procedure DrawFrustum(SourceView, TargetView:TERRAViewport; F:Frustum; LineColor:ColorRGBA; LineWidth:Single);
Var
  V:BoundingBoxVertices;
  P:Array[0..3] Of Vector3D;
  I:Integer;
Begin
  V := F.Vertices;

	P[0] := Vector3D_Create(V[2].X, V[2].Y, V[2].z);
	P[1] := Vector3D_Create(V[1].X, V[1].Y, V[1].z);
  P[2] := Vector3D_Create(V[3].X, V[3].Y, V[3].z);
  P[3] := Vector3D_Create(V[4].X, V[4].Y, V[4].z);
	For I:=1 To 3 Do
    DrawLine3D(SourceView,  TargetView, P[I-1], P[I], LineColor, LineWidth);

	P[0] := Vector3D_Create(V[6].X, V[6].Y, V[6].z);
	P[1] := Vector3D_Create(V[5].X, V[5].Y, V[5].z);
  P[2] := Vector3D_Create(V[7].X, V[7].Y, V[7].z);
  P[3] := Vector3D_Create(V[8].X, V[8].Y, V[8].z);
	For I:=1 To 3 Do
    DrawLine3D(SourceView,  TargetView, P[I-1], P[I], LineColor, LineWidth);

	P[0] := Vector3D_Create(V[1].X, V[1].Y, V[1].z);
	P[1] := Vector3D_Create(V[3].X, V[3].Y, V[3].z);
  P[2] := Vector3D_Create(V[7].X, V[7].Y, V[7].z);
  P[3] := Vector3D_Create(V[5].X, V[5].Y, V[5].z);
	For I:=1 To 3 Do
    DrawLine3D(SourceView,  TargetView, P[I-1], P[I], LineColor, LineWidth);

	P[0] := Vector3D_Create(V[2].X, V[2].Y, V[2].z);
	P[1] := Vector3D_Create(V[4].X, V[4].Y, V[4].z);
  P[2] := Vector3D_Create(V[8].X, V[8].Y, V[8].z);
  P[3] := Vector3D_Create(V[6].X, V[6].Y, V[6].z);
	For I:=1 To 3 Do
    DrawLine3D(SourceView,  TargetView, P[I-1], P[I], LineColor, LineWidth);
End;

Procedure DrawAxis(SourceView,  TargetView:TERRAViewport; Const Origin, Normal:Vector3D; LineWidth:Single);
Var
  Tangent, BiTangent:Vector3D;
  M:Matrix4x4;
Begin
  Tangent := Vector3D_Cross(Normal, Vector3D_Up);
  BiTangent := Vector3D_Cross(Normal, Tangent);

  DrawRay(SourceView,  TargetView, RayCreate(Origin, Normal), ColorRed, LineWidth, 5);
  DrawRay(SourceView,  TargetView, RayCreate(Origin, Tangent), ColorBlue, LineWidth, 5);
  DrawRay(SourceView,  TargetView, RayCreate(Origin, BiTangent), ColorGreen, LineWidth, 5);
End;

Procedure DrawPointCloud(SourceView, TargetView:TERRAViewport; Cloud:PointCloud2D; Color:ColorRGBA);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Cloud.PointCount) Do
  Begin
    DrawPoint2d(SourceView, TargetView, Cloud.Points[I], Color);
  End;
End;

(*
Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);
Var
  U,V:Vector3D;
  A,B,C,D:Vector3D;
Begin
  If (Abs(Normal.Y)>Abs(Normal.X)) And (Abs(Normal.Y)>Abs(Normal.Z)) Then
  Begin
    U := Vector3D_Create(Normal.X, Normal.Z, Normal.Y);
  End Else
  Begin
    U := Vector3D_Create(Normal.Z, Normal.Y, Normal.X);
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
