{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_Collision3D
 * Implements collision detection in 3D
 ***********************************************************************************************************************
}
Unit TERRA_Collision3D;

{$I terra.inc}
Interface
Uses TERRA_Utils, TERRA_Math, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color,
  TERRA_Mesh, TERRA_Ray, TERRA_Matrix, TERRA_Octree, TERRA_DebugDraw;

Type
  CollisionTriangle = Class(OctreeElement)
    V:Array[0..2] Of Vector3D;
    Normal:Vector3D;

    Function Intersect(Const R:Ray; Var T:Single):Boolean; Override;
  End;

  CollisionMesh = Class(TERRAObject)
    Protected
      _HorizontalOctree:Octree;
      _VerticalOctree:Octree;

    Public
      Constructor Create(Source:Mesh);
      Procedure Release;

      Function GetHeightAt(X:Single; Var Y:Single; Z:Single):Boolean;
      Function CheckMovement(Var Position:Vector3D; NewPos:Vector3D; Radius:Single):Boolean;

      Property HorizontalOctree:Octree Read _HorizontalOctree;
      Property VerticalOctree:Octree Read _VerticalOctree;
  End;

Function SphereIntersectTriangle(Center:Vector3D; Radius:Single; a, b, c:Vector3D; Var contactPoint:Vector3D):Boolean;
Function SphereIntersectMesh(Center:Vector3D; Radius:Single; MyMesh:Mesh; Transform:Matrix; Var contactPoint:Vector3D):Boolean;
Function SphereIntersectSphere(C1:Vector3D; R1:Single; C2:Vector3D; R2:Single): Boolean;

Implementation



{ CollisionMesh }
Constructor CollisionMesh.Create(Source: Mesh);
Var
  I,J,K:Integer;
  Group:MeshGroup;
  CT:CollisionTriangle;
  N:Vector3D;
Begin
  _HorizontalOctree := Octree.Create(Source.BoundingBox, 0);
  _VerticalOctree := Octree.Create(Source.BoundingBox, 0);

  For K:=0 To Pred(Source.GroupCount) Do
  Begin
    Group := Source.GetGroup(K);
    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      N := Group.GetTriangleNormal(I);
      If (N.Y<0) Then
        Continue;

      CT := CollisionTriangle.Create();
      CT.Box.Reset;
      For J:=0 To 2 Do
      Begin
        CT.V[J] := Group.Vertices[Group.Triangles[I].Indices[J]].Position;
        CT.Box.Add(CT.V[J]);
      End;
      CT.Normal := Group.GetTriangleNormal(I);


      If (N.Y>0.333) Then
        _HorizontalOctree.AddElement(CT)
      Else
      If (Abs(N.Y)<0.25) Then
        _VerticalOctree.AddElement(CT)
      Else
        CT.Release;
    End;
  End;
End;

Procedure CollisionMesh.Release;
Begin
  ReleaseObject(_HorizontalOctree);
  ReleaseObject(_VerticalOctree);
End;

Function CollisionMesh.GetHeightAt(X:Single; Var Y:Single; Z:Single): Boolean;
Var
  R:Ray;
  H:Single;
Begin
  H := 99999;
  R.Origin := VectorCreate(X, Y, Z);
  R.Direction := VectorCreate(0, -1, 0);
  //DrawRay(R, ColorWhite, 1000, True);
  Result := _HorizontalOctree.Intersect(R, H);
  If Result Then
    Y := R.IntersectionPoint(H).Y;
End;

Function CollisionMesh.CheckMovement(var Position: Vector3D;  NewPos: Vector3D; Radius: Single): Boolean;
Var
  R:Ray;
  H:Single;
Begin
  H := 99999;
  R.Origin := Position;
  R.Origin.Add(VectorCreate(0, 5, 0));
  R.Direction := NewPos;
  R.Direction.Subtract(Position);
  R.Direction.Normalize();
  //DrawRay(R, ColorWhite, 1000, True);
  If (_VerticalOctree.Intersect(R, H)) And (H<10) Then
  Begin
    Position := R.IntersectionPoint(H);
    Result := False;
  End Else
    Result := True;
End;



{ CollisionTriangle }
Function CollisionTriangle.Intersect(const R: Ray; var T: Single): Boolean;
Var
  X,Y:Single;
Begin
  Result := R.TriangleIntersect(V[0], V[1], V[2], T, X,Y);
End;

Function ClosestPointTriangle(p, a, b, c:Vector3D):Vector3D;
Var
  ab, ac, bc, n:Vector3D;
  snom, sdenom, tnom, tdenom:Single;
  va,vb, vc, unom,udenom:Single;
  u,v,w:Single;
Begin
	ab := VectorSubtract(b,a);
	ac := VectorSubtract(c,a);
	bc := VectorSubtract(c,b);

	// Compute parametric position s for projection P' of P on AB,
	// P' = A + s*AB, s = snom/(snom+sdenom)
	snom := VectorDot(VectorSubtract(p, a), ab);
  sdenom := VectorDot(VectorSubtract(p,b), VectorSubtract(a, b));

	// Compute parametric position t for projection P' of P on AC,
	// P' = A + t*AC, s = tnom/(tnom+tdenom)
	tnom := VectorDot(VectorSubtract(p,a), ac);
  tdenom := VectorDot(VectorSubtract(p,c), VectorSubtract(a, c));

	If (snom <= 0.0) And (tnom <= 0.0) Then // Vertex region early out
  Begin
    Result := A;
    Exit;
  End;

	// Compute parametric position u for projection P' of P on BC,
	// P' = B + u*BC, u = unom/(unom+udenom)
	unom := VectorDot(VectorSubtract(p, b), bc);
  udenom := VectorDot(VectorSubtract(p,c), VectorSubtract(b,c));

	If (sdenom <= 0.0) And (unom <= 0.0) Then // Vertex region early out
  Begin
    Result := B;
    Exit;
  End;

	If (tdenom <= 0.0) And (udenom <= 0.0) Then
  Begin
    Result := C; // Vertex region early out
    Exit;
  End;


	// P is outside (or on) AB if the triple scalar product [N PA PB] <= 0
	n := VectorCross(VectorSubtract(b,a), VectorSubtract(c,a));
	vc := VectorDot(n, VectorCross(VectorSubtract(a,p), VectorSubtract(b,p)));
	// If P outside AB and within feature region of AB,
	// return projection of P onto AB
	If (vc <= 0.0) And (snom >= 0.0) And (sdenom >= 0.0) Then
  Begin
		Result := A;
    Result.Add(VectorScale(ab,  snom / (snom + sdenom)));
    Exit;
  End;

	// P is outside (or on) BC if the triple scalar product [N PB PC] <= 0
	va := VectorDot(n, VectorCross(VectorSubtract(b,p), VectorSubtract(c,p)));
	// If P outside BC and within feature region of BC,
	// return projection of P onto BC
	If (va <= 0.0) And (unom >= 0.0) And (udenom >= 0.0) Then
  Begin
		Result := b;
    Result.Add(VectorScale(bc, unom / (unom + udenom)));
    Exit;
  End;

	// P is outside (or on) CA if the triple scalar product [N PC PA] <= 0
	vb := VectorDot(n, VectorCross(VectorSubtract(c,p), VectorSubtract(a,p)));
	// If P outside CA and within feature region of CA,
	// return projection of P onto CA
	If (vb <= 0.0) And (tnom >= 0.0) And (tdenom >= 0.0) Then
  Begin
    Result := A;
		Result.Add(VectorScale(Ac, tnom / (tnom + tdenom)));
    Exit;
  End;

	// P must project inside face region. Compute Q using barycentric coordinates
	u := va / (va + vb + vc);
	v := vb / (va + vb + vc);
	w := 1.0 - u - v; // = vc / (va + vb + vc)
	Result := VectorScale(a, u);
  Result.Add(VectorScale(b, v));
  Result.Add(VectorScale(c, w));
End;


// Returns true if sphere s intersects triangle ABC, false otherwise.
// The point p on abc closest to the sphere center is also returned
Function SphereIntersectTriangle(Center:Vector3D; Radius:Single; a, b, c:Vector3D; Var contactPoint:Vector3D):Boolean;
Var
  V:Vector3D;
Begin
	// Find point P on triangle ABC closest to sphere center
	contactPoint := ClosestPointTriangle(Center, a, b, c);

	// Sphere and triangle intersect if the (squared) distance from sphere
	// center to point p is less than the (squared) sphere radius
	v := VectorSubtract(contactPoint, Center);
	Result := VectorDot(v, v) <= Sqr(Radius);
End;

Function SphereIntersectMesh(Center:Vector3D; Radius:Single; MyMesh:Mesh; Transform:Matrix; Var contactPoint:Vector3D):Boolean;
Var
  I,J:Integer;
  Group:MeshGroup;
  T:Triangle;
  A,B,C:Vector3D;
Begin
  Result := False;
  If (MyMesh=Nil) Then
    Exit;

  For J:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    Group := MyMesh.GetGroup(J);
    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTriangle(I);
      A := Group.GetVertex(T.Indices[0]).Position;
      B := Group.GetVertex(T.Indices[1]).Position;
      C := Group.GetVertex(T.Indices[2]).Position;
      A := Transform.Transform(A);
      B := Transform.Transform(B);
      C := Transform.Transform(C);
      Result := SphereIntersectTriangle(Center, Radius, a, b, c, contactPoint);
      If Result Then
        Exit;
    End;
  End;
End;

Function SphereIntersectSphere(C1:Vector3D; R1:Single; C2:Vector3D; R2:Single): Boolean;
Var
  D:Vector3D;
  R:Single;
Begin
  //compare the distance to combined radii
  D := VectorSubtract(C1, C2);
  R := R1 + R2;
  Result := (Sqr(D.x) + Sqr(D.y) + Sqr(D.z) < Sqr(R));
End;

End.