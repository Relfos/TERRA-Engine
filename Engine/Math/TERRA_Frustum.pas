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
 * TERRA_Frustum
 * Implements a frustum class
 ***********************************************************************************************************************
}

Unit TERRA_Frustum;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Math, TERRA_BoundingBox, TERRA_Plane,
  TERRA_Vector3D, TERRA_Matrix4x4;

Type
  Frustum = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    Protected
        _Planes:Array[0..5]Of Plane;
        _Vertices:BoundingBoxVertices;

        Procedure Normalize;

	  Public
        Procedure Update(Const Projection, Camera:Matrix4x4);
        Function PointVisible(Point:Vector3D):Boolean;
        Function SphereVisible(Sphere:BoundingSphere):Boolean;
        Function BoxVisible(Box:BoundingBox):Boolean;

        Function GetVertices(CameraMatrix4x4, ProjectionMatrix4x4:Matrix4x4):BoundingBoxVertices; Overload;

        Property Vertices:BoundingBoxVertices Read _Vertices;
    End;

Implementation
Uses TERRA_Vector4D;

Function Frustum.GetVertices(CameraMatrix4x4, ProjectionMatrix4x4:Matrix4x4):BoundingBoxVertices;
Var
  I:Integer;
  Mat:Matrix4x4;
  P:Vector4D;
Begin
  Result[1] := VectorCreate(1,-1,-1);
  Result[2] := VectorCreate(-1,-1,-1);
  Result[3] := VectorCreate(1,1,-1);
  Result[4] := VectorCreate(-1,1,-1);
  Result[5] := VectorCreate(1,-1,1);
  Result[6] := VectorCreate(-1,-1,1);
  Result[7] := VectorCreate(1,1,1);
  Result[8] := VectorCreate(-1,1,1);

  Mat := Matrix4x4Multiply4x4(ProjectionMatrix4x4, CameraMatrix4x4);
  Mat := Matrix4x4Inverse(Mat);
  For I:=1 To 8 Do
  Begin
    P := Vector4DCreate(Result[I]);
    P.Transform(Mat);
    If (P.W = 0) Then
      P.W := 1.0;
    Result[I] := VectorCreate(P.X/P.W, P.Y/P.W, P.Z/P.W);
  End;
End;

Procedure Frustum.Update(Const Projection, Camera:Matrix4x4);
Var
  Clip:Matrix4x4;
Begin
  _Vertices := GetVertices(Camera, Projection);

  clip.V[0] := Camera.V[0] * Projection.V[0] + Camera.V[1] * Projection.V[4] + Camera.V[2] * Projection.V[8]  + Camera.V[3] * Projection.V[12];
	clip.V[1] := Camera.V[0] * Projection.V[1] + Camera.V[1] * Projection.V[5] + Camera.V[2] * Projection.V[9]  + Camera.V[3] * Projection.V[13];
	clip.V[2] := Camera.V[0] * Projection.V[2] + Camera.V[1] * Projection.V[6] + Camera.V[2] * Projection.V[10] + Camera.V[3] * Projection.V[14];
	clip.V[3] := Camera.V[0] * Projection.V[3] + Camera.V[1] * Projection.V[7] + Camera.V[2] * Projection.V[11] + Camera.V[3] * Projection.V[15];

	clip.V[4] := Camera.V[4] * Projection.V[0] + Camera.V[5] * Projection.V[4] + Camera.V[6] * Projection.V[8]  + Camera.V[7] * Projection.V[12];
	clip.V[5] := Camera.V[4] * Projection.V[1] + Camera.V[5] * Projection.V[5] + Camera.V[6] * Projection.V[9]  + Camera.V[7] * Projection.V[13];
	clip.V[6] := Camera.V[4] * Projection.V[2] + Camera.V[5] * Projection.V[6] + Camera.V[6] * Projection.V[10] + Camera.V[7] * Projection.V[14];
	clip.V[7] := Camera.V[4] * Projection.V[3] + Camera.V[5] * Projection.V[7] + Camera.V[6] * Projection.V[11] + Camera.V[7] * Projection.V[15];

	clip.V[8]  := Camera.V[8] * Projection.V[0] + Camera.V[9] * Projection.V[4] + Camera.V[10] * Projection.V[8]  + Camera.V[11] * Projection.V[12];
	clip.V[9]  := Camera.V[8] * Projection.V[1] + Camera.V[9] * Projection.V[5] + Camera.V[10] * Projection.V[9]  + Camera.V[11] * Projection.V[13];
	clip.V[10] := Camera.V[8] * Projection.V[2] + Camera.V[9] * Projection.V[6] + Camera.V[10] * Projection.V[10] + Camera.V[11] * Projection.V[14];
	clip.V[11] := Camera.V[8] * Projection.V[3] + Camera.V[9] * Projection.V[7] + Camera.V[10] * Projection.V[11] + Camera.V[11] * Projection.V[15];

	clip.V[12] := Camera.V[12] * Projection.V[0] + Camera.V[13] * Projection.V[4] + Camera.V[14] * Projection.V[8]  + Camera.V[15] * Projection.V[12];
	clip.V[13] := Camera.V[12] * Projection.V[1] + Camera.V[13] * Projection.V[5] + Camera.V[14] * Projection.V[9]  + Camera.V[15] * Projection.V[13];
	clip.V[14] := Camera.V[12] * Projection.V[2] + Camera.V[13] * Projection.V[6] + Camera.V[14] * Projection.V[10] + Camera.V[15] * Projection.V[14];
	clip.V[15] := Camera.V[12] * Projection.V[3] + Camera.V[13] * Projection.V[7] + Camera.V[14] * Projection.V[11] + Camera.V[15] * Projection.V[15];

	// Calculate the right side of the frustum.
  _Planes[0].a := clip.V[3]  - clip.V[0];
  _Planes[0].b := clip.V[7]  - clip.V[4];
	_Planes[0].c := clip.V[11] - clip.V[8];
	_Planes[0].d := clip.V[15] - clip.V[12];

	// Calculate the left side of the frustum.
	_Planes[1].a := clip.V[3]  + clip.V[0];
	_Planes[1].b := clip.V[7]  + clip.V[4];
	_Planes[1].c := clip.V[11] + clip.V[8];
	_Planes[1].d := clip.V[15] + clip.V[12];

	// Calculate the bottom side of the frustum.
	_Planes[2].a := clip.V[3]  + clip.V[1];
	_Planes[2].b := clip.V[7]  + clip.V[5];
	_Planes[2].c := clip.V[11] + clip.V[9];
	_Planes[2].d := clip.V[15] + clip.V[13];

	// Calculate the top side of the frustum.
	_Planes[3].a := clip.V[3]  - clip.V[1];
	_Planes[3].b := clip.V[7]  - clip.V[5];
	_Planes[3].c := clip.V[11] - clip.V[9];
	_Planes[3].d := clip.V[15] - clip.V[13];

	// Calculate the far side of the frustum.
	_Planes[4].a := clip.V[3]  - clip.V[2];
	_Planes[4].b := clip.V[7]  - clip.V[6];
	_Planes[4].c := clip.V[11] - clip.V[10];
	_Planes[4].d := clip.V[15] - clip.V[14];

	// Calculate the near side of the frustum.
	_Planes[5].a := clip.V[3]  + clip.V[2];
	_Planes[5].b := clip.V[7]  + clip.V[6];
	_Planes[5].c := clip.V[11] + clip.V[10];
	_Planes[5].d := clip.V[15] + clip.V[14];

	// Normalize the sides of the frustum.
   Normalize;
End;

Procedure Frustum.Normalize;
Var
  I:Integer;
  Magnitude:Single;
Begin
  // Loop through each side of the frustum and normalize it.
  For i:= 0 To 5 Do
  Begin
    Magnitude := InvSqrt(Sqr(_Planes[i].a) + Sqr(_Planes[i].b) + Sqr(_Planes[i].c));

    _Planes[i].a := _Planes[i].a * magnitude;
    _Planes[i].b := _Planes[i].b * magnitude;
    _Planes[i].c := _Planes[i].c * magnitude;
    _Planes[i].d := _Planes[i].d * magnitude;
  End;
End;


Function Frustum.PointVisible(Point:Vector3D):Boolean;
Var
  I:Integer;
Begin
  Result:=False;

  // Loop through each side of the frustum and test if the point lies outside any of them.
  For I:=0 To 5 Do
    If (_Planes[i].Distance(Point) < 0) Then
      Exit;

  Result:=True;
End;


Function Frustum.SphereVisible(Sphere:BoundingSphere):Boolean;
Var
  I:Integer;
  Distance:Single;
Begin
  Result:=False;
   // Loop through each side of the frustum and test if the sphere lies outside any of them.
  For i:=0 To 5 Do
  Begin
    Distance := _Planes[i].Distance(Sphere.Center);
    If (Distance < -Sphere.Radius) Then
      Exit;
  End;

  Result:=True;
End;

Function Frustum.BoxVisible(Box:BoundingBox):Boolean;
Var
  I:Integer;
Begin
  Result := False;
   // Loop through each side of the frustum and test if the box lies outside any of them.
  For I:=0 To 5 Do
  Begin
    If (_Planes[i].Distance(Box.StartVertex.X, Box.StartVertex.Y, Box.StartVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.EndVertex.X, Box.StartVertex.Y, Box.StartVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.StartVertex.X, Box.EndVertex.Y, Box.StartVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.EndVertex.X, Box.EndVertex.Y, Box.StartVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.StartVertex.X, Box.StartVertex.Y, Box.EndVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.EndVertex.X, Box.StartVertex.Y, Box.EndVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.StartVertex.X, Box.EndVertex.Y, Box.EndVertex.Z) >= 0) Then Continue;
    If (_Planes[i].Distance(Box.EndVertex.X, Box.EndVertex.Y, Box.EndVertex.Z) >= 0) Then Continue;
    Exit;
  End;

   Result:=True;
End;

End.

