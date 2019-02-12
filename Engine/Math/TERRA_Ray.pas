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
 * TERRA_RAY
 * Implements ray class (intersections, etc)
 ***********************************************************************************************************************
}
Unit TERRA_Ray;
{$I terra.inc}

Interface
Uses TERRA_Math, TERRA_BoundingBox, TERRA_Vector3D, TERRA_Plane;

Type
  TERRARay = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    Public
      Origin:Vector3D;
      Direction:Vector3D;

      Function Intersect(Const A:BoundingBox; Var T:Single; Out Normal:Vector3D):Boolean; Overload;
      Function Intersect(Const P:Plane; Var T:Single):Boolean; Overload;

      // Returns true if the ray intersects the triangle
      Function TriangleIntersect(Const V0,V1,V2:Vector3D; Var T,U,V:Single):Boolean;
      Function SphereIntersect(Const Position:Vector3D; Const Radius:Single; Var T:Single):Boolean;

      Function IntersectionPoint(Const T:Single):Vector3D;
  End;

Function RayCreate(Origin, Direction:Vector3D):TERRARay;

Implementation

Function RayCreate(Origin, Direction:Vector3D):TERRARay;
Begin
  Result.Origin := Origin;
  Result.Direction := Direction;
End;

Function TERRARay.TriangleIntersect(Const V0,V1,V2:Vector3D;  Var T,U,V:Single):Boolean;
Var
  Edge1,Edge2:Vector3D;
  PVec,TVec,QVec:Vector3D;
  Det,InvDet:Double;
Begin
  Result := False;
  // Find vectors for two edges sharing vert0
  Edge1.x := V1.x - V0.x;
  Edge1.y := V1.y - V0.y;
  Edge1.z := V1.z - V0.z;

  Edge2.x := V2.x - V0.x;
  Edge2.y := V2.y - V0.y;
  Edge2.z := V2.z - V0.z;

  // Begin calculating determinant - also used to calculate U parameter
  PVec := Vector3D_Cross(Direction, Edge2);

  // If determinant is near zero, ray lies in plane of triangle
  Det := Vector3D_Dot(PVec, Edge1);

  {If (det > -EPSILON) And (det < EPSILON) Then
    Exit;}
    If Det=0 Then
      Exit;

  InvDet := 1.0 / det;

  // calculate distance from vert0 to ray origin
  TVec := Vector3D_Subtract(Origin, V0);

  // calculate U parameter and test bounds
  U := Vector3D_Dot(tvec, pvec) * InvDet;
  If (U < 0.0) Or (U > 1.0) Then
    Exit;

  // prepare to test V parameter */
  QVec := Vector3D_Cross(Tvec,Edge1);

  // calculate V parameter and test bounds
  V := Vector3D_Dot(Direction,QVec) * InvDet;
  If (V<0.0) Or (U+V> 1.0) Then Exit;

  // calculate t, ray intersects triangle
  T := Vector3D_Dot(Edge2, qvec) * InvDet;
  Result := (T>=0.0);
End;

Function TERRARay.Intersect(Const P:Plane; Var T:Single):Boolean;
Begin
  T := -(P.A * Origin.X + P.B * Origin.Y + P.C * Origin.Z + P.D) / (P.A * Direction.X + P.B * Direction.Y + P.C * Direction.Z);

  If (T < 0.0) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;
End;

Function TERRARay.SphereIntersect(Const Position:Vector3D; Const Radius:Single; Var T:Single):Boolean;
Var
  Dest:Vector3D;
  B,C,D:Single;
Begin
	Dest := Vector3D_Subtract(Origin, Position);
	B := Vector3D_Dot(Dest, Direction);
	C := Vector3D_Dot(Dest, Dest) - Sqr(Radius);
	D := Sqr(B) - C;
	Result := (D > 0.0);
  If Result Then
  Begin
    T := -B - sqrt(D);
    Result := T>=0;
  End;
End;

Function TERRARay.Intersect(Const A:BoundingBox; Var T:Single; Out Normal:Vector3D):Boolean;
Const
  Normals:Array[0..5] Of Vector3D = ((X:1; Y:0; Z:0), (X:0; Y:1; Z:0), (X:0; Y:0; Z:1), (X:-1; Y:0; Z:0), (X:0; Y:-1; Z:0), (X:0; Y:0; Z:-1));
Var
  I:Integer;
  RC:Single;
  IP:Vector3D;
  Dist:Array[0..5] Of Single;
Begin
  Result := False;
  For i:=0 To 5 Do
    Dist[i] := -1;

  If (Direction.x<>0) Then
  Begin
    rc := 1.0 / Direction.X;
	  dist[0] := (A.StartVertex.x - Origin.x) * rc;
  	dist[3] := (A.EndVertex.x - Origin.x) * rc;
  End;
	If (Direction.y<>0) Then
  Begin
    rc := 1.0 / Direction.Y;
	  dist[1] := (A.StartVertex.y - Origin.y) * rc;
  	dist[4] := (A.EndVertex.y - Origin.y) * rc;
  End;
  If (Direction.z<>0) Then
	Begin
    rc := 1.0 / Direction.Z;
  	dist[2] := (A.StartVertex.z - Origin.z) * rc;
    dist[5] := (A.EndVertex.z - Origin.z) * rc;
  End;

  For I:=0 To 5 Do
  If (dist[i] > 0) Then
  Begin
    ip := Vector3D_Add(Origin, Vector3D_Scale(Direction, dist[i]));
		if ((ip.x > (A.StartVertex.x - EPSILON)) And (ip.x < (A.EndVertex.x + EPSILON)) And
  	    (ip.y > (A.StartVertex.y - EPSILON)) And (ip.y < (A.EndVertex.y + EPSILON)) And
	  		(ip.z > (A.StartVertex.z - EPSILON)) And (ip.z < (A.EndVertex.z + EPSILON))) Then
  		Begin
        If (dist[i] < T) Then
        Begin
          T := Dist[I];
  			  Result := True;
          Normal := Normals[I];
          //Exit;
        End;
  		End;
  End;
End;

(*
Function Ray.Intersect(Const A:BoundingBox; Var T:Single):Boolean;
Var
  Distance, lowT:Single;
  Hit:Boolean;
  HitPoint:Vector3D;
Begin
	lowt := 0.0;
	Hit := False;

	// Check origin inside first

	If (Self.Origin.X>A.StartVertex.X) And (Self.Origin.X<A.EndVertex.X)
  And (Self.Origin.Y>A.StartVertex.Y) And (Self.Origin.Y<A.EndVertex.Y)
  And (Self.Origin.Z>A.StartVertex.Z) And (Self.Origin.Z<A.EndVertex.Z) Then
  Begin
		T := 0;
		Result := True;
    Exit;
  End;

	// Check each face in turn, only check closest 3
	// Min x
	If (Origin.x <= A.StartVertex.x) And (Direction.x > 0) Then
	Begin
		Distance := (A.StartVertex.x - Origin.x) / Direction.x;
		If (Distance >= 0) Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(Origin, VectorScale(Direction, Distance));
			if (hitpoint.y >= A.StartVertex.y) And (hitpoint.y <= A.EndVertex.y) And
				(hitpoint.z >= A.StartVertex.z) And (hitpoint.z <= A.EndVertex.z) And
				((Not hit) Or (Distance < lowt)) Then
			Begin
				Hit := true;
				lowt := Distance;
			End;
		End;
	End;

	// Max x
	If (Origin.x >= A.EndVertex.x) And (Direction.x < 0) Then
	Begin
		Distance := (A.EndVertex.x - origin.x) / direction.x;
		If (Distance >= 0) Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(origin, VectorScale(Direction, Distance));
			If (hitpoint.y >= A.StartVertex.y) And (hitpoint.y <= A.EndVertex.y)
      And (hitpoint.z >= A.StartVertex.z) And (hitpoint.z <= A.EndVertex.z)
      And ((Not hit) Or (Distance < lowt)) Then
			Begin
				hit := true;
				lowt := Distance;
			End;
		End;
	End;

	// A.StartVertex y
	If (origin.y <= A.StartVertex.y ) And ( direction.y > 0) Then
	Begin
		Distance := (A.StartVertex.y - origin.y) / direction.y;
		If (Distance >= 0) Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(origin, VectorScale(direction, t));
			if (hitpoint.x >= A.StartVertex.x ) And ( hitpoint.x <= A.EndVertex.x ) And (
				hitpoint.z >= A.StartVertex.z ) And ( hitpoint.z <= A.EndVertex.z ) And 
				((Not hit) Or (Distance < lowt)) Then
			Begin
				Hit := True;
				lowt := Distance;
			End;
		End;
	End;
	// A.EndVertex y
	If (origin.y >= A.EndVertex.y ) And ( direction.y < 0) Then
	Begin
		Distance := ((A.EndVertex.y- origin.y)/direction.y);
		if (Distance >= 0) Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(origin, VectorScale(direction, t));
			if (hitpoint.x >= A.StartVertex.x ) And ( hitpoint.x <= A.EndVertex.x ) And
				(hitpoint.z >= A.StartVertex.z ) And ( hitpoint.z <= A.EndVertex.z ) And
				((Not hit) Or (Distance < lowt)) Then
			Begin
				hit := true;
				lowt := Distance;
			End;
		End;
	End;
	// A.StartVertex z
	if (origin.z <= A.StartVertex.z ) And ( direction.z > 0) Then
	Begin
		Distance := (A.StartVertex.z - origin.z) / direction.z;
		if (Distance >= 0) Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(origin, VectorScale(Direction, t));
			if (hitpoint.x >= A.StartVertex.x ) And ( hitpoint.x <= A.EndVertex.x ) And
      (hitpoint.y >= A.StartVertex.y ) And ( hitpoint.y <= A.EndVertex.y ) And
      ((Not hit) Or (Distance < lowt)) Then
			Begin
				hit := true;
				lowt := Distance;
			End;
		End;
	End;
	// A.EndVertex z
	if (origin.z >= A.EndVertex.z ) And ( direction.z < 0) Then
	Begin
		Distance := (A.EndVertex.z - origin.z) / direction.z;
		if (Distance >= 0)  Then
		Begin
			// Substitute t back into ray and check bounds and dist
			hitpoint := VectorAdd(origin, VectorScale(direction,t));
			if (hitpoint.x >= A.StartVertex.x ) And ( hitpoint.x <= A.EndVertex.x ) And
				(hitpoint.y >= A.StartVertex.y ) And ( hitpoint.y <= A.EndVertex.y ) And
				((Not hit) Or (Distance < lowt)) Then
			Begin
				hit := true;
				lowt := Distance;
			End;
		End;
	End;

	T := lowt;
	Result := Hit;
End;
*)

Function TERRARay.IntersectionPoint(Const T:Single):Vector3D;
Begin
  Result := Vector3D_Add(Origin, Vector3D_Scale(Direction, T));
End;


End.