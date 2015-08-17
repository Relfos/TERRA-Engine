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
 * TERRA_Matrix4x4
 * Implements a 4x4 matrix
 ***********************************************************************************************************************
}
Unit TERRA_Matrix4x4;
{$I terra.inc}

Interface
Uses TERRA_Math, TERRA_Vector3D, TERRA_Plane;

Type
  MatrixColumns = Array[0..2] Of Vector3D;

  PMatrix4x4 = ^Matrix4x4;
  Matrix4x4 = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    V:Array [0..15] Of Single;

    Function GetEulerAngles:Vector3D;

    Function Transform(Const P:Vector3D):Vector3D;
    Function TransformNormal(Const P:Vector3D):Vector3D;

    Procedure OrthoNormalize;

    Procedure SetValue(I,J:Integer; Value:Single);

    Function Get(I,J:Integer):Single;

    Procedure SetTranslation(Const P:Vector3D);
    Function GetTranslation:Vector3D;

    Function GetColumns:MatrixColumns;
  End;


  PMatrix4x4Array=^Matrix4x4Array;
  Matrix4x4Array=Array[0..255] Of Matrix4x4;

Const
 Matrix4x4Identity:Matrix4x4= (V:(1.0, 0.0, 0.0, 0.0,
                            0.0, 1.0, 0.0, 0.0,
                            0.0, 0.0, 1.0, 0.0,
                            0.0, 0.0, 0.0, 1.0));

// Returns a rotation Matrix4x4
Function Matrix4x4Rotation(Const Rotation:Vector3D):Matrix4x4; Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function Matrix4x4Rotation(Const X,Y,Z:Single):Matrix4x4; Overload; {$IFDEF FPC}Inline;{$ENDIF}

Function Matrix4x4Rotation(Const Axis:Vector3D; Const Angle:Single):Matrix4x4; Overload;

// Returns a translation Matrix4x4
Function Matrix4x4Translation(Const Translation:Vector3D):Matrix4x4;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function Matrix4x4Translation(Const X,Y,Z:Single):Matrix4x4;Overload; {$IFDEF FPC}Inline;{$ENDIF}

Function Matrix4x4Transform(Const Position,Rotation,Scale:Vector3D):Matrix4x4;

Function Matrix4x4Orientation(Const Position,Direction,Up,Scale:Vector3D):Matrix4x4;

Function Matrix4x4Lerp(Const A,B:Matrix4x4; Const S:Single):Matrix4x4;

// Inverts a Matrix4x4
Function Matrix4x4Inverse(Const A:Matrix4x4):Matrix4x4;

Function Matrix4x4Scale(Const Scale:Vector3D):Matrix4x4;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function Matrix4x4Scale(Const X,Y,Z:Single):Matrix4x4;Overload;   {$IFDEF FPC}Inline;{$ENDIF}

// Returns a reflection Matrix4x4
Function Matrix4x4Reflection(Const P:Plane):Matrix4x4;
Function Matrix4x4Mirror(Const Source,Normal:Vector3D):Matrix4x4;

Function Matrix4x4Transpose(Const Source:Matrix4x4):Matrix4x4;

// Multiplys two matrices
Function Matrix4x4Multiply4x3(Const A,B:Matrix4x4):Matrix4x4;
Function Matrix4x4Multiply4x4(Const A,B:Matrix4x4):Matrix4x4;

{$IFDEF BENCHMARK}
Function Matrix4x4Multiply4x3SSE(Const A,B:Matrix4x4):Matrix4x4;
Function Matrix4x4Multiply4x4SSE(Const A,B:Matrix4x4):Matrix4x4;
{$ENDIF}

Function Matrix4x4Perspective(FOV, AspectRatio, zNear, zFar:Single):Matrix4x4;
Function Matrix4x4LookAt(Eye, LookAt, Roll:Vector3D):Matrix4x4;
Function Matrix4x4Ortho(left, right,  bottom,  top,  nearVal,  farVal:Single):Matrix4x4;

//Function Matrix4x4Isometric(X,Y, Height:Single):Matrix4x4;

// modifies projection Matrix4x4 in place
// clipPlane is in camera space
Procedure CalculateObliqueMatrix4x4ClipPlane(Var projection:Matrix4x4; {Const Transform:Matrix4x4;} clipPlane:Plane);

Implementation
Uses TERRA_Vector4D, Math{$IFDEF NEON_FPU},TERRA_NEON{$ENDIF};

// modifies projection Matrix4x4 in place
// clipPlane is in camera space
Procedure CalculateObliqueMatrix4x4ClipPlane(Var projection:Matrix4x4; clipPlane:Plane);
Var
  Q,C:Vector4D;
  D:Single;
Begin
  C := VectorCreate4D(clipPlane.A, clipPlane.B, clipPlane.C, clipPlane.D);

    // Calculate the clip-space corner point opposite the clipping plane
    // as (sgn(clipPlane.x), sgn(clipPlane.y), 1, 1) and
    // transform it into camera space by multiplying it
    // by the inverse of the projection Matrix4x4

    q.x := (sgn(C.x)-Projection.V[8]) / Projection.V[0];
    q.y := (sgn(C.y)-Projection.V[9]) / Projection.V[5];
    q.z := -1.0;
    q.w := (1.0 + Projection.V[10]) / Projection.V[14];

    // Calculate the scaled plane vector
    D := 2.0 / Vector4DDot(C, Q);
    C := Vector4DScale(C, D);

    // Replace the third row of the projection Matrix4x4
    Projection.V[2] := c.x;
    Projection.V[6] := c.y;
    Projection.V[10] := c.z + 1.0;
    Projection.V[14] := c.w;

    (*q.x := (sgn(C.x)) / Projection.V[0];
    q.y := (sgn(C.y)) / Projection.V[5];
    q.z := 1.0;
    q.w := 1.0;

    // Calculate the scaled plane vector
    D := 2.0 / Vector4DDot(C, Q);
    C := Vector4DScale(C, D);

    // Replace the third row of the projection Matrix4x4
    Projection.V[2] := c.x - Projection.V[3];
    Projection.V[6] := c.y - Projection.V[7];
    Projection.V[10] := c.z - Projection.V[11];
    Projection.V[14] := c.w - Projection.V[15];*)
End;

{   
  0  1  2  3
  4  5  6  7
  8  9  10 11
  12 13 14 15

  0 4 8  12
  1 5 9  13
  2 6 10 14
  3 7 11 15
}

(*Var
  Q,C:Vector4D;
  M,Inverse:Matrix4x4;
  D:Single;
  I:Integer;
  Normal, P:Vector3D;
Begin
  Normal := VectorCreate(clipPlane.A, clipPlane.B, clipPlane.C);
  Normal.Normalize();
  P := VectorScale(Normal, clipPlane.D);

  M := Matrix4x4Multiply4x4(Projection, Transform);
  //M := Projection;
  //M := Transform;
  M := Matrix4x4Inverse(M);
  M := Matrix4x4Transpose(M);
  P := M.Transform(P);

  D := -VectorDot(P, Normal);

  C := Vector4DCreate(Normal.X, Normal.Y, Normal.Z, D);

  C := Vector4DCreate(clipPlane.A, clipPlane.B, clipPlane.C, clipPlane.D);

  C.Transform(M);

  {Inverse := Matrix4x4Inverse(Projection);
  C.Transform(Inverse);}


  C.Normalize();
  C.W := C.W - 1.0;

  If (C.Z < 0) Then
    C := Vector4DScale(C, -1);

  // third row = clip plane - fourth row
{  projection.V[8] := c.x;
  projection.V[9] := c.y;
  projection.V[10] := c.z;
  projection.V[11] := c.w;
  }

    Projection.V[2] := c.x;
    Projection.V[6] := c.y;
    Projection.V[10] := c.z;
    Projection.V[14] := c.w;
End;*)

Function Matrix4x4Reflection(Const P:Plane):Matrix4x4;
Begin
  P.Normalize();

  Result.V[0] := -2 * P.a * P.a + 1;
  Result.V[1] := -2 * P.b * P.a;
  Result.V[2] := -2 * P.c * P.a;
  Result.V[3] :=  0;

  Result.V[4] := -2 * P.a * P.b;
  Result.V[5] := -2 * P.b * P.b + 1;
  Result.V[6] := -2 * P.c * P.b;
  Result.V[7] := 0;

  Result.V[8] := -2 * P.a * P.c;
  Result.V[9] := -2 * P.b * P.c;
  Result.V[10] := -2 * P.c * P.c + 1;
  Result.V[11] := 0;

  Result.V[12] := -2 * P.a * P.d;
  Result.V[13] := -2 * P.b * P.d;
  Result.V[14] := -2 * P.c * P.d;
  Result.V[15] := 1;
End;

(*Function Matrix4x4Isometric(X,Y, Height:Single):Matrix4x4;
Begin
//http://toxicdump.org/blog/view/7/Haunted-Mansion-DevBlog-_02---Oblique-vs-Isometric-perspective-and-perfect-depth-sorting
//http://www.xnaresources.com/default.asp?page=Tutorial:TileEngineSeries:4
//http://www.gamedev.net/topic/226797-isometric-camera-problems/
//glRotatef(30.0, 1,0,0);
//glRotatef(-45.0, 0,1,0);
//glTranslatef(-x, -m_cameraheight, -z);
End;*)

Function Matrix4x4Transpose(Const Source:Matrix4x4):Matrix4x4;
Begin
  Result.V[0] := Source.V[0];
  Result.V[1] := Source.V[4];
  Result.V[2] := Source.V[8];
  Result.V[3] := Source.V[12];

  Result.V[4] := Source.V[1];
  Result.V[5] := Source.V[5];
  Result.V[6] := Source.V[9];
  Result.V[7] := Source.V[13];

  Result.V[8] := Source.V[2];
  Result.V[9] := Source.V[6];
  Result.V[10] := Source.V[10];
  Result.V[11] := Source.V[14];

  Result.V[12] := Source.V[3];
  Result.V[13] := Source.V[7];
  Result.V[14] := Source.V[11];
  Result.V[15] := Source.V[15];
End;

Function Matrix4x4Ortho(left, right,  bottom,  top,  nearVal,  farVal:Single):Matrix4x4;
Var
  N:Single;
  Tx, Ty, Tz:Single;
Begin
  TX := -(Right + Left)/(Right - Left);
  TY := -(Top + Bottom)/(Top - Bottom);
  TZ := -(farVal + nearVal)/(farVal - nearVal);


  N := (Right - Left);
  If (N<>0) Then
    Result.V[0] := 2 / N
  Else
    Result.V[0] := 0.0;

  Result.V[1] := 0;
  Result.V[2] := 0;
  Result.V[3] := 0;

  Result.V[4] := 0;
  N := (Top - Bottom);
  If (N<>0) Then
    Result.V[5] := 2 / N
  Else
    Result.V[5] := 0.0;
  Result.V[6] := 0;
  Result.V[7] := 0;

  Result.V[8] := 0;
  Result.V[9] := 0;
  N := (farVal - nearVal);
  If (N<>0) Then
    Result.V[10] := -2 / N
  Else
    Result.V[10] := 0.0;
  Result.V[11] := 0;

  Result.V[12] := tx;
  Result.V[13] := ty;
  Result.V[14] := tz;
  Result.V[15] := 1.0;
  // + 0.375
End;

//TESTME
Procedure Matrix4x4.SetValue(I,J:Integer; Value:Single);
Begin
  V[J*4+I] := Value;
End;

Function Matrix4x4.Get(I, J: Integer): Single;
Begin
  Result := V[J*4+I];
End;

Function Matrix4x4.GetColumns: MatrixColumns;
Begin
	Result[0] := VectorCreate(V[0], V[4], V[8]);
	Result[1] := VectorCreate(V[1], V[5], V[9]);
	Result[2] := VectorCreate(V[2], V[6], V[10]);
End;

Function Matrix4x4.GetEulerAngles:Vector3D;
Var
  sinPitch, cosPitch, sinRoll, cosRoll, sinYaw, cosYaw:Double;
  Inv:Double;
Begin
	sinPitch := -V[2]; // [2][0];
	cosPitch := Sqrt(1 - Sqr(sinPitch));

	If (Abs(cosPitch) > EPSILON ) Then
	Begin
    Inv := 1.0 / cosPitch;
    sinRoll := V[9] * Inv;
	  cosRoll := V[10] * Inv;
	  sinYaw := V[1] * Inv;
	  cosYaw := V[0] * Inv;
  End Else
  Begin
    sinRoll := -V[6];
	  cosRoll := V[5];
	  sinYaw := 0.0;
	  cosYaw := 1.0;
  End;

	Result.X  := atan2(sinYaw, cosYaw);
	Result.Y := atan2(sinPitch, cosPitch);
	Result.Z  := atan2(sinRoll, cosRoll);
End;

Procedure Matrix4x4.SetTranslation(Const P:Vector3D);
Begin
  V[12] := P.X;
  V[13] := P.Y;
  V[14] := P.Z;
End;

Function Matrix4x4.GetTranslation:Vector3D;
Begin
  Result.X := V[12];
  Result.Y := V[13];
  Result.Z := V[14];
End;

Procedure Matrix4x4.Orthonormalize;
Var
  fInvLength:Single;
  fDot0, fDot1:Single;
Function Index(I,J:Integer):Integer;
Begin
  Result := J*4+I;
End;
Begin
exit;
// Algorithm uses Gram-Schmidt orthogonalization. If 'this' Matrix4x4 is
// M = [m0|m1|m2], then orthonormal output Matrix4x4 is Q = [q0|q1|q2],
//
// q0 = m0/|m0|
// q1 = (m1-(q0*m1)q0)/|m1-(q0*m1)q0|
// q2 = (m2-(q0*m2)q0-(q1*m2)q1)/|m2-(q0*m2)q0-(q1*m2)q1|
//
// where |V| indicates length of vector V and A*B indicates dot
// product of vectors A and B.

// compute q0
  fInvLength := InvSqrt( Sqr(V[Index(0,0)]) + Sqr(V[Index(1,0)]) + Sqr(V[Index(2,0)]));

  V[Index(0,0)] := V[Index(0,0)] * fInvLength;
  V[Index(1,0)] := V[Index(1,0)] * fInvLength;
  V[Index(2,0)] := V[Index(2,0)] * fInvLength;

  // compute q1
  fDot0 := V[Index(0,0)]*V[Index(0,1)] + V[Index(1,0)]*V[Index(1,1)] + V[Index(2,0)]*V[Index(2,1)];

  V[Index(0,1)] := V[Index(0,1)] - fDot0 * V[Index(0,0)];
  V[Index(1,1)] := V[Index(1,1)] - fDot0 * V[Index(1,0)];
  V[Index(2,1)] := V[Index(2,1)] - fDot0 * V[Index(2,0)];

  fInvLength := InvSqrt(Sqr(V[Index(0,1)])+Sqr(V[Index(1,1)]) +Sqr(V[Index(2,1)]));

  V[Index(0,1)] := V[Index(0,1)] * fInvLength;
  V[Index(1,1)] := V[Index(1,1)] * fInvLength;
  V[Index(2,1)] := V[Index(2,1)] * fInvLength;

  // compute q2
  fDot1 := V[Index(0,1)]*V[Index(0,2)] + V[Index(1,1)]*V[Index(1,2)] + V[Index(2,1)]*V[Index(2,2)];

  fDot0 := V[Index(0,0)]*V[Index(0,2)] +  V[Index(1,0)]*V[Index(1,2)] + V[Index(2,0)]*V[Index(2,2)];

  V[Index(0,2)] := V[Index(0,2)] - fDot0*V[Index(0,0)] + fDot1*V[Index(0,1)];
  V[Index(1,2)] := V[Index(1,2)] - fDot0*V[Index(1,0)] + fDot1*V[Index(1,1)];
  V[Index(2,2)] := V[Index(2,2)] - fDot0*V[Index(2,0)] + fDot1*V[Index(2,1)];

  fInvLength := InvSqrt(Sqr(V[Index(0,2)]) + Sqr(V[Index(1,2)]) + Sqr(V[Index(2,2)]));

  V[Index(0,2)] := V[Index(0,2)] * fInvLength;
  V[Index(1,2)] := V[Index(1,2)] * fInvLength;
  V[Index(2,2)] := V[Index(2,2)] * fInvLength;
End;

Function Matrix4x4.Transform(Const P:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Begin
{$IFDEF NEON_FPU}
  matvec3_neon(@Self,@P,@Result);
{$ELSE}
  Result.X := P.X*V[0] + P.Y*V[4] + P.Z*V[8]  + V[12];
  Result.Y := P.X*V[1] + P.Y*V[5] + P.Z*V[9]  + V[13];
  Result.Z := P.X*V[2] + P.Y*V[6] + P.Z*V[10] + V[14];
{$ENDIF}
End;

Function Matrix4x4.TransformNormal(Const P:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.X := P.X*V[0] + P.Y*V[4] + P.Z*V[8];
  Result.Y := P.X*V[1] + P.Y*V[5] + P.Z*V[9];
  Result.Z := P.X*V[2] + P.Y*V[6] + P.Z*V[10];
End;

Function Matrix4x4Transform(Const Position,Rotation,Scale:Vector3D):Matrix4x4;
Var
  CosRx,CosRy,CosRz:Single;
  SinRx,SinRy,SinRz:Single;
Begin
  CosRx := Cos(Rotation.x); //Used 6x
  CosRy := Cos(Rotation.y); //Used 4x
  CosRz := Cos(Rotation.z); //Used 4x
  SinRx := Sin(Rotation.x); //Used 5x
  SinRy := Sin(Rotation.y); //Used 5x
  SinRz := Sin(Rotation.z); //Used 5x

  Result.V[0] := CosRy*CosRz*Scale.x;
  Result.V[1] := CosRy*SinRz*Scale.x;
  Result.V[2] := -SinRy*Scale.x;
  Result.V[3] := 0.0;

  Result.V[4] := (CosRz*SinRx*SinRy*Scale.y) - (CosRx*SinRz*Scale.y);
  Result.V[5] := (CosRx*CosRz*Scale.y) + (SinRx*SinRy*SinRz*Scale.y);
  Result.V[6] := CosRy*SinRx*Scale.y;
  Result.V[7] := 0.0;

  Result.V[8] := (CosRx*CosRz*SinRy*Scale.z) + (SinRx*SinRz*Scale.z);
  Result.V[9] := (-CosRz*SinRx*Scale.z) + (CosRx*SinRy*SinRz*Scale.z);
  Result.V[10] := CosRx*CosRy*Scale.z;
  Result.V[11] := 0.0;

  Result.V[12] := Position.x;
  Result.V[13] := Position.y;
  Result.V[14] := Position.z;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Orientation(Const Position,Direction,Up,Scale:Vector3D):Matrix4x4;
Var
  TX,TZ:Vector3D;
Begin
  TZ := VectorCross(Direction, Up);
  TZ.Normalize;
  TX := VectorCross(Up, TZ);
  TX.Normalize;

  Result.V[0] := TX.X * Scale.X;
  Result.V[1] := TX.Y * Scale.X;
  Result.V[2] := TX.Z * Scale.X;
  Result.V[3] := 0.0;

  Result.V[4] := Up.X * Scale.y;
  Result.V[5] := Up.Y * Scale.y;
  Result.V[6] := Up.Z * Scale.Y;
  Result.V[7] := 0.0;

  Result.V[8] := TZ.X * Scale.Z;
  Result.V[9] := TZ.Y * Scale.Z;
  Result.V[10] := TZ.Z * Scale.Z;
  Result.V[11] := 0.0;

  Result.V[12] := Position.x;
  Result.V[13] := Position.y;
  Result.V[14] := Position.z;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Rotation(Const Axis:Vector3D; Const Angle:Single):Matrix4x4;
Var
  C,S,T:Single;
  X,Y,Z:Single;
Begin
  C := Cos(Angle);
  S := Sin(Angle);
  T := 1-C;

  X := Axis.X;
  Y := Axis.Y;
  Z := Axis.Z;

	Result.V[0] := T * Sqr(X) + C;
	Result.V[1] := (T * X * Y) - (s *Z);
	Result.V[2] := (T * X * Z) + (s * Y);
  Result.V[3] := 0.0;

	Result.V[4] := (t * X * Y) + (s * Z);
	Result.V[5] := (t * Y * Y)+ C;
	Result.V[6] := (T * Y * Z) - (S * X);
  Result.V[7] := 0.0;

	Result.V[8] := (T * X * Z) - (S * Y);
	Result.V[9] := (T * Y * Z) + (S * X);
	Result.V[10] := (T * Z * Z) +  C;
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Rotation(Const Rotation:Vector3D):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := Matrix4x4Rotation(Rotation.X, Rotation.Y, Rotation.Z);
End;

Function Matrix4x4Rotation(Const X,Y,Z:Single):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Var
  Cr,Sr,Cp,Sp,Cy,Sy,Srsp,Crsp:Single;
Begin
  cr := Cos(X);
	sr := Sin(X);
	cp := Cos(Y);
	sp := Sin(Y);
	cy := Cos(Z);
	sy := Sin(Z);

	Result.V[0] := cp * cy;
	Result.V[1] := cp * sy;
	Result.V[2] := -sp;

  If Result.V[2] = -0 Then
    Result.V[2] := 0;
  Result.V[3] := 0.0;

	srsp := sr * sp;
	crsp := cr * sp;

	Result.V[4] := (srsp * cy) - (cr * sy);
	Result.V[5] := (srsp * sy) + (cr * cy);
	Result.V[6] := (sr * cp);
  Result.V[7] := 0.0;

	Result.V[8] := (crsp * cy) + (sr * sy);
	Result.V[9] := (crsp * sy) - (sr * cy);
	Result.V[10] := (cr * cp);
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Translation(Const Translation:Vector3D):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := Matrix4x4Translation(Translation.X,Translation.Y,Translation.Z);
End;

Function Matrix4x4Translation(Const X,Y,Z:Single):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.V[0] := 1.0;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := 1.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 0.0;
  Result.V[9] := 0.0;
  Result.V[10] := 1.0;
  Result.V[11] := 0.0;
  Result.V[12] := X;
  Result.V[13] := Y;
  Result.V[14] := Z;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Scale(Const Scale:Vector3D):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := Matrix4x4Scale(Scale.X,Scale.Y,Scale.Z);
End;

Function Matrix4x4Scale(Const X,Y,Z:Single):Matrix4x4;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.V[0] := X;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := Y;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 0.0;
  Result.V[9] := 0.0;
  Result.V[10] := Z;
  Result.V[11] := 0.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := 0.0;
  Result.V[15] := 1.0;
End;

Function Matrix4x4Perspective(FOV, aspectRatio, znear, zfar:Single):Matrix4x4;
Var
  left, right, bottom, top:Single;
  ymax, xmax:Single;
  temp, temp2, temp3, temp4:Single;
Begin
  ymax := znear * Tan(FOV * 0.5 * Rad);
  xmax := ymax * aspectRatio;

  left := -xmax;
  right := xmax;
  bottom := -ymax;
  top := ymax;

  temp := znear * 2.0;
  temp2 := (xmax * 2.0);
  temp3 :=  (top - bottom);
  temp4 := 1.0 / (zfar - znear);
  Result.V[0] := temp / temp2;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := 0.0;
  Result.V[5] := temp / temp3;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := (right + left) / temp2;
  Result.V[9] := (top + bottom) / temp3;
  Result.V[10] := (-zfar - znear) * temp4;
  Result.V[11] := -1.0;
  Result.V[12] := 0.0;
  Result.V[13] := 0.0;
  Result.V[14] := (-temp * zfar) * temp4;
  Result.V[15] := 0.0;
End;

Function Matrix4x4LookAt(Eye, LookAt, Roll:Vector3D):Matrix4x4;
Var
  xaxis, yaxis, zaxis:Vector3D;
Begin
  zaxis := VectorSubtract(Eye, lookAt);
  zaxis.Normalize();
  xaxis := VectorCross(Roll, zaxis);
  xaxis.Normalize();
  If (xaxis.LengthSquared()<=0) Then
  Begin
    Roll := VectorCreate(-Roll.Z, -Roll.X, -Roll.Y);
    xaxis := VectorCross(Roll, zaxis);
    xaxis.Normalize();
  End;

  yaxis := VectorCross(zaxis, xaxis);

  Result.V[0] := xaxis.x;
  Result.V[1] := yaxis.x;
  Result.V[2] := zaxis.x;
  Result.V[3] := 0.0;
  Result.V[4] := xaxis.y;
  Result.V[5] := yaxis.y;
  Result.V[6] := zaxis.y;
  Result.V[7] := 0.0;
  Result.V[8] := xaxis.z;
  Result.V[9] := yaxis.z;
  Result.V[10] := zaxis.z;
  Result.V[11] := 0.0;
  Result.V[12] := -xaxis.dot(eye);
  Result.V[13] := -yaxis.dot(eye);
  Result.V[14] := -zaxis.dot(eye);
  Result.V[15] := 1.0;
End;

Function Matrix4x4Mirror(Const Source,Normal:Vector3D):Matrix4x4;
Var
  Dot:Single;
Begin
  Dot := VectorDot(Source,Normal);

  Result.V[0] := 1.0 - (2.0 *Normal.X * Normal.X);
  Result.V[1] := - (2.0 * Normal.Y * Normal.X);
  Result.V[2] := - (2.0 * Normal.Z * Normal.X);
  Result.V[3] := 0.0;

  Result.V[4] := - (2.0 * Normal.X * Normal.Y);
  Result.V[5] := 1.0 - (2.0 * Normal.Y * Normal.Y);
  Result.V[6] := - (2.0 * Normal.Z * Normal.Y);
  Result.V[7] := 0.0;

  Result.V[8] := - (2.0 * Normal.X * Normal.Z);
  Result.V[9] := - (2.0 * Normal.Y * Normal.Z);
  Result.V[10] := 1.0 - (2.0 * Normal.Z * Normal.Z);
  Result.V[11] := 0.0;

  Result.V[12]:= 2.0 * Dot * Normal.X;
  Result.V[13]:= 2.0 * Dot * Normal.Y;
  Result.V[14]:= 2.0 * Dot * Normal.Z;
  Result.V[15]:= 1.0;
End;

Function Matrix4x4GetTranslation(Const A:Matrix4x4):Vector3D;
Begin
  Result.X := A.V[12];
  Result.Y := A.V[13];
  Result.Z := A.V[14];
End;

Function Matrix4x4GetScale(Const A:Matrix4x4):Vector3D;
Begin
  Result.X := A.V[0];
  Result.Y := A.V[5];
  Result.Z := A.V[10];
End;

// 4x4 Matrix4x4 inverse using Gauss-Jordan algorithm with row pivoting
// originally written by Nathan Reed, now released into the public domain.
Function Matrix4x4Inverse(Const A:Matrix4x4):Matrix4x4;
Var
  I:Integer;
  a0, a1, a2, a3, a4, a5: Single;
  b0, b1, b2, b3, b4, b5: Single;
  Det, invDet:Single;
Begin
  a0 := A.V[ 0] * A.V[ 5] - A.V[ 1] *A.V[ 4];
  a1 := A.V[ 0] * A.V[ 6] - A.V[ 2] *A.V[ 4];
  a2 := A.V[ 0] * A.V[ 7] - A.V[ 3] *A.V[ 4];
  a3 := A.V[ 1] * A.V[ 6] - A.V[ 2] *A.V[ 5];
  a4 := A.V[ 1] * A.V[ 7] - A.V[ 3] *A.V[ 5];
  a5 := A.V[ 2] * A.V[ 7] - A.V[ 3] *A.V[ 6];
  b0 := A.V[ 8] * A.V[13] - A.V[ 9] *A.V[12];
  b1 := A.V[ 8] * A.V[14] - A.V[10] *A.V[12];
  b2 := A.V[ 8] * A.V[15] - A.V[11] *A.V[12];
  b3 := A.V[ 9] * A.V[14] - A.V[10] *A.V[13];
  b4 := A.V[ 9] * A.V[15] - A.V[11] *A.V[13];
  b5 := A.V[10] * A.V[15] - A.V[11] *A.V[14];

  Det := a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0;
  If (Abs(Det) > Epsilon) Then
  Begin
    Result.V[ 0] := + A.V[ 5]*b5 - A.V[ 6]*b4 + A.V[ 7]*b3;
    Result.V[ 4] := - A.V[ 4]*b5 + A.V[ 6]*b2 - A.V[ 7]*b1;
    Result.V[ 8] := + A.V[ 4]*b4 - A.V[ 5]*b2 + A.V[ 7]*b0;
    Result.V[12] := - A.V[ 4]*b3 + A.V[ 5]*b1 - A.V[ 6]*b0;
    Result.V[ 1] := - A.V[ 1]*b5 + A.V[ 2]*b4 - A.V[ 3]*b3;
    Result.V[ 5] := + A.V[ 0]*b5 - A.V[ 2]*b2 + A.V[ 3]*b1;
    Result.V[ 9] := - A.V[ 0]*b4 + A.V[ 1]*b2 - A.V[ 3]*b0;
    Result.V[13] := + A.V[ 0]*b3 - A.V[ 1]*b1 + A.V[ 2]*b0;
    Result.V[ 2] := + A.V[13]*a5 - A.V[14]*a4 + A.V[15]*a3;
    Result.V[ 6] := - A.V[12]*a5 + A.V[14]*a2 - A.V[15]*a1;
    Result.V[10] := + A.V[12]*a4 - A.V[13]*a2 + A.V[15]*a0;
    Result.V[14] := - A.V[12]*a3 + A.V[13]*a1 - A.V[14]*a0;
    Result.V[ 3] := - A.V[ 9]*a5 + A.V[10]*a4 - A.V[11]*a3;
    Result.V[ 7] := + A.V[ 8]*a5 - A.V[10]*a2 + A.V[11]*a1;
    Result.V[11] := - A.V[ 8]*a4 + A.V[ 9]*a2 - A.V[11]*a0;
    Result.V[15] := + A.V[ 8]*a3 - A.V[ 9]*a1 + A.V[10]*a0;

    invDet := 1.0 / Det;
    For I:=0 To 15 Do
      Result.V[ I] := Result.V[ I] * invDet;
  End Else
    FillChar(Result, SizeOf(Result), 0);
End;

Function Matrix4x4Lerp(Const A,B:Matrix4x4; Const S:Single):Matrix4x4;
Var
  I:Integer;
Begin
  For I:=0 To 15 Do
   Result.V[I] := A.V[I] * (1.0 - S) + B.V[I] * S;
End;

{
    // According to fastcall convention in Delphi
    // eax = ths, edx = m1, ecx = m2
    // We do not need to reserve eax, ecx and edx, only reserve esi, edi, ebx

    push esi
    push edi

movss xmm0, dword ptr [eax]
movaps xmm1,  [edx]
shufps xmm0, xmm0, 0
movss xmm2, dword ptr [eax+4]
mulps xmm0, xmm1
shufps xmm2, xmm2, 0
movaps xmm3,  [edx+$10]
movss xmm7, dword ptr [eax+8]
mulps xmm2, xmm3
shufps xmm7, xmm7, 0
addps xmm0, xmm2
movaps xmm4, [edx+$20]
movss xmm2, dword ptr [eax+$0C]
mulps xmm7, xmm4
shufps xmm2, xmm2, 0
addps xmm0, xmm7
movaps xmm5, [edx+$30]
movss xmm6, dword ptr [eax+$10]
mulps xmm2, xmm5
movss xmm7, dword ptr [eax+$14]
shufps xmm6, xmm6, 0
addps xmm0, xmm2
shufps xmm7, xmm7, 0
movlps qword ptr [ecx], xmm0
movhps qword ptr [ecx+8], xmm0
mulps xmm7, xmm3
movss xmm0, dword ptr [eax+18h]
mulps xmm6, xmm1
shufps xmm0, xmm0, 0
addps xmm6, xmm7
mulps xmm0, xmm4
movss xmm2, dword ptr [eax+24h]
addps xmm6, xmm0
movss xmm0, dword ptr [eax+1Ch]
movss xmm7, dword ptr [eax+20h]
shufps xmm0, xmm0, 0
shufps xmm7, xmm7, 0
mulps xmm0, xmm5
mulps xmm7, xmm1
addps xmm6, xmm0
shufps xmm2, xmm2, 0
movlps qword ptr [ecx+10h], xmm6
movhps qword ptr [ecx+18h], xmm6
mulps xmm2, xmm3
movss xmm6, dword ptr [eax+28h]
addps xmm7, xmm2
shufps xmm6, xmm6, 0
movss xmm2, dword ptr [eax+2Ch]
mulps xmm6, xmm4
shufps xmm2, xmm2, 0
addps xmm7, xmm6
mulps xmm2, xmm5
movss xmm0, dword ptr [eax+34h]
addps xmm7, xmm2
shufps xmm0, xmm0, 0
movlps qword ptr [ecx+20h], xmm7
movss xmm2, dword ptr [eax+30h]
movhps qword ptr [ecx+28h], xmm7
mulps xmm0, xmm3
shufps xmm2, xmm2, 0
movss xmm6, dword ptr [eax+38h]
mulps xmm2, xmm1
shufps xmm6, xmm6, 0
addps xmm2, xmm0
mulps xmm6, xmm4
movss xmm7, dword ptr [eax+3Ch]
shufps xmm7, xmm7, 0
addps xmm2, xmm6
mulps xmm7, xmm5
addps xmm2, xmm7
movaps [ecx+$30], xmm2
    pop edi
    pop esi
}

{$IFDEF BENCHMARK}{$UNDEF SSE}{$ENDIF}
{$IFNDEF SSE}
Function Matrix4x4Multiply4x4(Const A,B:Matrix4x4):Matrix4x4;
Begin
{$IFDEF NEON_FPU}
  matmul4_neon(@A,@B,@Result);
{$ELSE}
	Result.V[0] := A.V[0]*B.V[0] + A.V[4]*B.V[1] + A.V[8]*B.V[2] + A.V[12]*B.V[3];
	Result.V[1] := A.V[1]*B.V[0] + A.V[5]*B.V[1] + A.V[9]*B.V[2] + A.V[13]*B.V[3];
	Result.V[2] := A.V[2]*B.V[0] + A.V[6]*B.V[1] + A.V[10]*B.V[2] + A.V[14]*B.V[3];
	Result.V[3] := A.V[3]*B.V[0] + A.V[7]*B.V[1] + A.V[11]*B.V[2] + A.V[15]*B.V[3];

	Result.V[4] := A.V[0]*B.V[4] + A.V[4]*B.V[5] + A.V[8]*B.V[6] + A.V[12]*B.V[7];
	Result.V[5] := A.V[1]*B.V[4] + A.V[5]*B.V[5] + A.V[9]*B.V[6] + A.V[13]*B.V[7];
	Result.V[6] := A.V[2]*B.V[4] + A.V[6]*B.V[5] + A.V[10]*B.V[6] + A.V[14]*B.V[7];
	Result.V[7] := A.V[3]*B.V[4] + A.V[7]*B.V[5] + A.V[11]*B.V[6] + A.V[15]*B.V[7];

	Result.V[8] := A.V[0]*B.V[8] + A.V[4]*B.V[9] + A.V[8]*B.V[10] + A.V[12]*B.V[11];
	Result.V[9] := A.V[1]*B.V[8] + A.V[5]*B.V[9] + A.V[9]*B.V[10] + A.V[13]*B.V[11];
	Result.V[10] := A.V[2]*B.V[8] + A.V[6]*B.V[9] + A.V[10]*B.V[10] + A.V[14]*B.V[11];
	Result.V[11] := A.V[3]*B.V[8] + A.V[7]*B.V[9] + A.V[11]*B.V[10] + A.V[15]*B.V[11];

	Result.V[12] := A.V[0]*B.V[12] + A.V[4]*B.V[13] + A.V[8]*B.V[14] + A.V[12]*B.V[15];
	Result.V[13] := A.V[1]*B.V[12] + A.V[5]*B.V[13] + A.V[9]*B.V[14] + A.V[13]*B.V[15];
	Result.V[14] := A.V[2]*B.V[12] + A.V[6]*B.V[13] + A.V[10]*B.V[14] + A.V[14]*B.V[15];
	Result.V[15] := A.V[3]*B.V[12] + A.V[7]*B.V[13] + A.V[11]*B.V[14] + A.V[15]*B.V[15];
{$ENDIF}
End;

Function Matrix4x4Multiply4x3(Const A,B:Matrix4x4):Matrix4x4;
Begin
{$IFDEF NEON_FPU}
  matmul4_neon(@A,@B,@Result);
{$ELSE}
	Result.V[0] := A.V[0]*B.V[0] + A.V[4]*B.V[1] + A.V[8]*B.V[2];
	Result.V[1] := A.V[1]*B.V[0] + A.V[5]*B.V[1] + A.V[9]*B.V[2];
	Result.V[2] := A.V[2]*B.V[0] + A.V[6]*B.V[1] + A.V[10]*B.V[2];
	Result.V[3] := 0.0;

	Result.V[4] := A.V[0]*B.V[4] + A.V[4]*B.V[5] + A.V[8]*B.V[6];
	Result.V[5] := A.V[1]*B.V[4] + A.V[5]*B.V[5] + A.V[9]*B.V[6];
	Result.V[6] := A.V[2]*B.V[4] + A.V[6]*B.V[5] + A.V[10]*B.V[6];
	Result.V[7] := 0.0;

	Result.V[8] := A.V[0]*B.V[8] + A.V[4]*B.V[9] + A.V[8]*B.V[10];
	Result.V[9] := A.V[1]*B.V[8] + A.V[5]*B.V[9] + A.V[9]*B.V[10];
	Result.V[10] := A.V[2]*B.V[8] + A.V[6]*B.V[9] + A.V[10]*B.V[10];
	Result.V[11] := 0.0;

	Result.V[12] := A.V[0]*B.V[12] + A.V[4]*B.V[13] + A.V[8]*B.V[14] + A.V[12];
	Result.V[13] := A.V[1]*B.V[12] + A.V[5]*B.V[13] + A.V[9]*B.V[14] + A.V[13];
	Result.V[14] := A.V[2]*B.V[12] + A.V[6]*B.V[13] + A.V[10]*B.V[14] + A.V[14];
	Result.V[15] := 1.0;
{$ENDIF}
End;
{$ENDIF}

{$IFDEF BENCHMARK}{$DEFINE SSE}{$ENDIF}

{$IFDEF SSE}
Procedure MultMatrix4x4_SSE(A, B, Dest: Pointer);Register;
Asm
    push esi
    push edi

    mov edi, A

    movaps xmm4, [edi]
    movaps xmm5, [edi+16]
    movaps xmm6, [edi+32]
    movaps xmm7, [edi+48]

    mov esi, B
    mov eax, 0

@@L1:
    movaps xmm0, [esi+eax]
    movaps xmm1, xmm0
    movaps xmm2, xmm0
    movaps xmm3, xmm0

    shufps xmm0, xmm2, 000h
    shufps xmm1, xmm2, 055h
    shufps xmm2, xmm2, 0AAh
    shufps xmm3, xmm3, 0FFh

    mulps xmm0, [edi]
    mulps xmm1, [edi+16]
    mulps xmm2, [edi+32]
    mulps xmm3, [edi+48]

    addps xmm0, xmm1
    addps xmm0, xmm2
    addps xmm0, xmm3

    mov edx, Dest
    add edx, eax
    movaps [edx], xmm0

    add eax, 16
    cmp eax, 48
    jle @@L1

    pop edi
    pop esi
End;

Var
  _AlignedA, _AlignedB, _AlignedC:PMatrix4x4;
  _P1, _P2, _P3: Pointer;

{  0 0
  1 4
  2 8
  3 12     X
  4 16
  5 20
  6 24
  7 28    X
  8 32
  9 36
  10 40
  11 44   X
  12 48
  13 52
  14 56
  15 60   X
}

{$IFDEF BENCHMARK}
Function Matrix4x4Multiply4x3SSE(Const A,B:Matrix4x4):Matrix4x4;
{$ELSE}
Function Matrix4x4Multiply4x3(Const A,B:Matrix4x4):Matrix4x4;
{$ENDIF}
Asm
	movss xmm0, dword ptr [edx]
	movups xmm1,  [eax]
	shufps xmm0, xmm0, 0
	movss xmm2, dword ptr [edx+4]
	mulps xmm0, xmm1
	shufps xmm2, xmm2, 0
	movups xmm3,  [eax+10h]
	movss xmm7, dword ptr [edx+8]
	mulps xmm2, xmm3
	shufps xmm7, xmm7, 0
	addps xmm0, xmm2
	movups xmm4,  [eax+20h]
//	movss xmm2, dword ptr [edx+12]
	mulps xmm7, xmm4
//	shufps xmm2, xmm2, 0
	addps xmm0, xmm7
	movups xmm5,  [eax+48]
	movss xmm6, dword ptr [edx+16]
//	mulps xmm2, xmm5
	movss xmm7, dword ptr [edx+20]
	shufps xmm6, xmm6, 0
//	addps xmm0, xmm2
	shufps xmm7, xmm7, 0
	movlps qword ptr [ecx], xmm0
	movhps qword ptr [ecx+8], xmm0
	mulps xmm7, xmm3
	movss xmm0, dword ptr [edx+24]
	mulps xmm6, xmm1
	shufps xmm0, xmm0, 0
	addps xmm6, xmm7
	mulps xmm0, xmm4
	movss xmm2, dword ptr [edx+36]
	addps xmm6, xmm0
//	movss xmm0, dword ptr [edx+28]
	movss xmm7, dword ptr [edx+32]
//	shufps xmm0, xmm0, 0
	shufps xmm7, xmm7, 0
//	mulps xmm0, xmm5
	mulps xmm7, xmm1
//	addps xmm6, xmm0
	shufps xmm2, xmm2, 0
	movlps qword ptr [ecx+10h], xmm6
	movhps qword ptr [ecx+18h], xmm6
	mulps xmm2, xmm3
	movss xmm6, dword ptr [edx+40]
	addps xmm7, xmm2
	shufps xmm6, xmm6, 0
//	movss xmm2, dword ptr [edx+44]
	mulps xmm6, xmm4
//	shufps xmm2, xmm2, 0
	addps xmm7, xmm6
//	mulps xmm2, xmm5
	movss xmm0, dword ptr [edx+52]
//	addps xmm7, xmm2
	shufps xmm0, xmm0, 0
	movlps qword ptr [ecx+20h], xmm7
	movss xmm2, dword ptr [edx+30h]
	movhps qword ptr [ecx+40], xmm7
	mulps xmm0, xmm3
	shufps xmm2, xmm2, 0
	movss xmm6, dword ptr [edx+56]
	mulps xmm2, xmm1
	shufps xmm6, xmm6, 0
	addps xmm2, xmm0
	mulps xmm6, xmm4
//	movss xmm7, dword ptr [edx+60]
//	shufps xmm7, xmm7, 0
	addps xmm2, xmm6
	//mulps xmm7, xmm5
	addps xmm2, xmm5 //xmm7
	movups  [ecx+48], xmm2
{Begin
  Move(A, _AlignedA^, SizeOf(Matrix4x4));
  Move(B, _AlignedB^, SizeOf(Matrix4x4));
  MultMatrix4x4_SSE(_AlignedA, _AlignedB, _AlignedC);
  Move(_AlignedC^, Result, SizeOf(Matrix4x4));}
End;

{$IFDEF BENCHMARK}
Function Matrix4x4Multiply4x4SSE(Const A,B:Matrix4x4):Matrix4x4;
{$ELSE}
Function Matrix4x4Multiply4x4(Const A,B:Matrix4x4):Matrix4x4;
{$ENDIF}
Asm
	movss xmm0, dword ptr [edx]
	movups xmm1,  [eax]
	shufps xmm0, xmm0, 0
	movss xmm2, dword ptr [edx+4]
	mulps xmm0, xmm1
	shufps xmm2, xmm2, 0
	movups xmm3,  [eax+10h]
	movss xmm7, dword ptr [edx+8]
	mulps xmm2, xmm3
	shufps xmm7, xmm7, 0
	addps xmm0, xmm2
	movups xmm4,  [eax+20h]
	movss xmm2, dword ptr [edx+0Ch]
	mulps xmm7, xmm4
	shufps xmm2, xmm2, 0
	addps xmm0, xmm7
	movups xmm5,  [eax+30h]
	movss xmm6, dword ptr [edx+10h]
	mulps xmm2, xmm5
	movss xmm7, dword ptr [edx+14h]
	shufps xmm6, xmm6, 0
	addps xmm0, xmm2
	shufps xmm7, xmm7, 0
	movlps qword ptr [ecx], xmm0
	movhps qword ptr [ecx+8], xmm0
	mulps xmm7, xmm3
	movss xmm0, dword ptr [edx+18h]
	mulps xmm6, xmm1
	shufps xmm0, xmm0, 0
	addps xmm6, xmm7
	mulps xmm0, xmm4
	movss xmm2, dword ptr [edx+24h]
	addps xmm6, xmm0
	movss xmm0, dword ptr [edx+1Ch]
	movss xmm7, dword ptr [edx+20h]
	shufps xmm0, xmm0, 0
	shufps xmm7, xmm7, 0
	mulps xmm0, xmm5
	mulps xmm7, xmm1
	addps xmm6, xmm0
	shufps xmm2, xmm2, 0
	movlps qword ptr [ecx+10h], xmm6
	movhps qword ptr [ecx+18h], xmm6
	mulps xmm2, xmm3
	movss xmm6, dword ptr [edx+28h]
	addps xmm7, xmm2
	shufps xmm6, xmm6, 0
	movss xmm2, dword ptr [edx+2Ch]
	mulps xmm6, xmm4
	shufps xmm2, xmm2, 0
	addps xmm7, xmm6
	mulps xmm2, xmm5
	movss xmm0, dword ptr [edx+34h]
	addps xmm7, xmm2
	shufps xmm0, xmm0, 0
	movlps qword ptr [ecx+20h], xmm7
	movss xmm2, dword ptr [edx+30h]
	movhps qword ptr [ecx+28h], xmm7
	mulps xmm0, xmm3
	shufps xmm2, xmm2, 0
	movss xmm6, dword ptr [edx+38h]
	mulps xmm2, xmm1
	shufps xmm6, xmm6, 0
	addps xmm2, xmm0
	mulps xmm6, xmm4
	movss xmm7, dword ptr [edx+3Ch]
	shufps xmm7, xmm7, 0
	addps xmm2, xmm6
	mulps xmm7, xmm5
	addps xmm2, xmm7
	movups  [ecx+30h], xmm2
End;
{$ENDIF}

Initialization
{$IFDEF SSE}
  GetMem(_P1, SizeOf(Matrix4x4) + 15);
  _AlignedA := PMatrix4x4((Cardinal(_P1) + $0F) and $FFFFFFF0);
  GetMem(_P2, SizeOf(Matrix4x4) + 15);
  _AlignedB := PMatrix4x4((Cardinal(_P2) + $0F) and $FFFFFFF0);
  GetMem(_P3, SizeOf(Matrix4x4) + 15);
  _AlignedC := PMatrix4x4((Cardinal(_P3) + $0F) and $FFFFFFF0);
Finalization
  FreeMem(_P1);
  FreeMem(_P2);
  FreeMem(_P3);
{$ENDIF}
End.
