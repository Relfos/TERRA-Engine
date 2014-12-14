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
 * TERRA_Vector3D
 * Implements a vector 3d class
 ***********************************************************************************************************************
}
{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
{$I terra.inc}
Unit TERRA_Vector3D;
{$ENDIF}

Interface
Uses TERRA_Math;

Type
  {$IFDEF OXYGENE}
  Vector3D = Class
  {$ELSE}
  PVector3D = ^Vector3D;
  Vector3D = Packed Object
  {$ENDIF}
    X:Single;
    Y:Single;
    Z:Single;

    {$IFDEF OXYGENE}
    Constructor Create(X,Y,Z:Single);
    {$ENDIF}

    Function Equals(Const B:Vector3D):Boolean;
    Function Dot(Const B:Vector3D):Single;

    Procedure Add(Const B:Vector3D);
    Procedure Subtract(Const B:Vector3D);

    Procedure Scale(Const S:Single); Overload;
    Procedure Scale(Const B:Vector3D); Overload;

    Function Get(Index:Integer):Single;
    Procedure SetValue(Index:Integer; Value:Single);

    // Normalizes the vector
    Procedure Normalize;
    Function Length:Single;
    Function LengthSquared:Single;
    Function Distance(Const N:Vector3D):Single;
    Function Distance2D(Const N:Vector3D):Single;

    {$IFDEF BENCHMARK}
    Procedure NormalizeSSE;
    Function LengthSSE:Single;
    Function DistanceSSE(Const N:Vector3D):Single;
    {$ENDIF}


    Procedure Rotate(Const Axis:Vector3D; Const Angle:Single);
  End;


Const
// Vector constants
{$IFDEF OXYGENE}
  VectorZero: Vector3D = new Vector3D(0.0, 0.0, 0.0);
  VectorOne: Vector3D = new Vector3D(1.0, 1.0, 1.0);
  VectorUp: Vector3D = new Vector3D(0.0, 1.0, 0.0);
{$ELSE}
  VectorZero:  Vector3D = (X:0.0; Y:0.0; Z:0.0);
  VectorOne:  Vector3D = (X:1.0; Y:1.0; Z:1.0);
  VectorUp:   Vector3D = (X:0.0; Y:1.0; Z:0.0);
{$ENDIF}

// Vector functions
Function VectorCreate(Const X,Y,Z:Single):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

Function VectorUniform(Const N:Single):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

Function VectorMax(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Function VectorMin(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

Function VectorAdd(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Function VectorSubtract(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Function VectorMultiply(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}
Function VectorCross(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

// Returns the dot product between two vectors
Function VectorDot(Const A,B:Vector3D):Single; {$IFDEF FPC}Inline;{$ENDIF}

// Scales a vector by S
Function VectorScale(Const A:Vector3D; S:Single):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

// Reflect two vectors
Function VectorReflect(Const Source,Normal:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

Function VectorInterpolate(Const A,B:Vector3D; Const S:Single):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

// Halve arc between unit vectors v0 and v1.
Function VectorBisect(Const A,B:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

Procedure VectorOrthoNormalize(Var A,B:Vector3D); {$IFDEF FPC}Inline;{$ENDIF}

// Returns a triangle normal
Function TriangleNormal(Const V0,V1,V2:Vector3D):Vector3D; {$IFDEF FPC}Inline;{$ENDIF}

//  Quad functions
Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single):Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single; Var Normal:Vector3D):Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}


Implementation
{$IFDEF NEON_FPU}Uses TERRA_NEON;{$ENDIF}


Function Vector3D.Get(Index:Integer):Single;
Begin
  Case Index Of
  0:  Result := X;
  1:  Result := Y;
  2:  Result := Z;
  Else
      Result := 0;
  End;
End;

Procedure Vector3D.SetValue(Index:Integer; Value:Single);
Begin
  Case Index Of
  0:  X := Value;
  1:  Y := Value;
  2:  Z := Value;
  End;
End;

Function VectorCreate(Const X,Y,Z:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
End;

Function VectorUniform(Const N:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := N;
  Result.Y := N;
  Result.Z := N;
End;

Function Vector3D.Equals(Const B:Vector3D):Boolean; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (Self.X=B.X) And (Self.Y=B.Y) And(Self.Z=B.Z);
End;

Function VectorMax(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A.X>B.X Then Result.X:=A.X Else Result.X:=B.X;
  If A.Y>B.Y Then Result.Y:=A.Y Else Result.Y:=B.Y;
  If A.Z>B.Z Then Result.Z:=A.Z Else Result.Z:=B.Z;
End;

Function VectorMin(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A.X<B.X Then Result.X:=A.X Else Result.X:=B.X;
  If A.Y<B.Y Then Result.Y:=A.Y Else Result.Y:=B.Y;
  If A.Z<B.Z Then Result.Z:=A.Z Else Result.Z:=B.Z;
End;

Procedure Vector3D.Rotate(Const Axis:Vector3D; Const Angle:Single); {$IFDEF FPC} Inline;{$ENDIF}
Var
  SX,SY,SZ:Single;
  C,S:Single;
Begin
	C := Cos(angle);
	S := Sin(angle);

  SX := X;
  SY := Y;
  SZ := Z;

	X  := (Axis.x*Axis.x*(1-c) + c)	* Sx + (Axis.x*Axis.y*(1-c) - Axis.z*s)	* Sy + (Axis.x*Axis.z*(1-c) + Axis.y*s)	* Sz;
  Y  := (Axis.y*Axis.x*(1-c) + Axis.z*s)	* Sx + (Axis.y*Axis.y*(1-c) + c)	* Sy + (Axis.y*Axis.z*(1-c) - Axis.x*s)	* Sz;
	Z  := (Axis.x*Axis.z*(1-c) - Axis.y*s)	* Sx + (Axis.y*Axis.z*(1-c) + Axis.x*s)	* Sy + (Axis.z*Axis.z*(1-c) + c)	* Sz;
End;

Procedure Vector3D.Add(Const B:Vector3D); {$IFDEF FPC} Inline; {$ENDIF}
Begin
  X := X + B.X;
  Y := Y + B.Y;
  Z := Z + B.Z;
End;

Procedure Vector3D.Subtract(Const B:Vector3D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X - B.X;
  Y := Y - B.Y;
  Z := Z - B.Z;
End;

Procedure Vector3D.Scale(Const S:Single); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * S;
  Y := Y * S;
  Z := Z * S;
End;

Procedure Vector3D.Scale(Const B:Vector3D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * B.X;
  Y := Y * B.Y;
  Z := Z * B.Z;
End;

Function Vector3D.Dot(Const B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  Result := dot3_neon_hfp(@Self,@B);
  {$ELSE}
  Result := (X*B.X)+(Y*B.Y)+(Z*B.Z);
  {$ENDIF}
End;

Function VectorDot(Const A,B:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  Result := dot3_neon_hfp(@A,@B);
  {$ELSE}
  Result := (A.X*B.X)+(A.Y*B.Y)+(A.Z*B.Z);
  {$ENDIF}
End;

// R = 2 * ( N dot V ) * V - V
// R =	V - 2 * ( N dot V ) * V
Function VectorReflect(Const Source,Normal:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Var
  N:Single;
Begin
  N := VectorDot(Normal,Source) * 2;
  Result := VectorScale(Source, N);
  Result := VectorSubtract(Source,Result);
End;

Procedure VectorOrthoNormalize(Var A,B:Vector3D); {$IFDEF FPC}Inline;{$ENDIF}
Var
  proj:Vector3D;
Begin
  A.Normalize();

  proj := VectorScale(A, VectorDot(B, A));
  B.Subtract(proj);
  B.Normalize();
End;

Function VectorCross(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  cross3_neon(@A, @B, @Result);
  {$ELSE}
  With Result Do
  Begin
    X := A.Y*B.Z - A.Z*B.Y;
    Y := A.Z*B.X - A.X*B.Z;
    Z := A.X*B.Y - A.Y*B.X;
  End;
  {$ENDIF}
End;

Function VectorSubtract(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=A.X-B.X;
    Y:=A.Y-B.Y;
    Z:=A.Z-B.Z;
  End;
End;

Function VectorScale(Const A:Vector3D; S:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X := A.X* S;
    Y := A.Y* S;
    Z := A.Z* S;
  End;
End;

Function VectorAdd(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=(A.X+B.X);
    Y:=(A.Y+B.Y);
    Z:=(A.Z+B.Z);
  End;
End;

Function VectorMultiply(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X:=(A.X*B.X);
    Y:=(A.Y*B.Y);
    Z:=(A.Z*B.Z);
  End;
End;

Function VectorInterpolate(Const A,B:Vector3D; Const S:Single):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  With Result Do
  Begin
    X := (B.X*S)+(A.X*(1-S));
    Y := (B.Y*S)+(A.Y*(1-S));
    Z := (B.Z*S)+(A.Z*(1-S));
  End;
End;

Function VectorBisect(Const A,B:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Var
  Len:Single;
Begin
  Result := VectorAdd(A,B);

  Len := Result.Length;
  If (Len<Epsilon) Then
    Result := VectorZero
  Else
    Result := VectorScale(Result, 1.0 / Len);
End;

Function VectorNullify(Const A:Vector3D):Vector3D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If Abs(A.X)<Epsilon Then Result.X:=0 Else Result.X:=A.X;
  If Abs(A.Y)<Epsilon Then Result.Y:=0 Else Result.Y:=A.Y;
  If Abs(A.Z)<Epsilon Then Result.Z:=0 Else Result.Z:=A.Z;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Procedure Vector3D.NormalizeSSE;
{$ELSE}
Procedure Vector3D.Normalize; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h  // broadcast length
  movlps xmm0, [eax]    // read(x,y)
  movaps xmm2, xmm0      // store a copy of the vector
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all
  shufps xmm0, xmm0, 0h  // broadcast length
  xorps xmm1, xmm1
  ucomiss xmm0, xmm1 // check for zero length
  je @@END
  rsqrtps xmm0, xmm0       // get reciprocal of length of vector
  mulps xmm2, xmm0      // normalize

  movlps [eax], xmm2   // store X and Y
  shufps xmm2, xmm2, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  movss [eax+8], xmm2   // store Z
@@END:
End;

{$IFDEF BENCHMARK}
Function Vector3D.LengthSSE:Single;
{$ELSE}
Function Vector3D.Length:Single; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h
  movlps xmm0, [eax]    // read(x,y)

  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all

  sqrtss xmm0, xmm0       // and finally, sqrt
  movss result, xmm0
End;

{$IFDEF BENCHMARK}
Function Vector3D.DistanceSSE(Const N:Vector3D):Single;
{$ELSE}
Function Vector3D.Distance(Const N:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
{$ENDIF}
Asm
  movss xmm0, [eax+8]    // read (z)
  shufps xmm0, xmm0, 0h
  movlps xmm0, [eax]    // read(x,y)

  movss xmm1, [edx+8]    // read (b.z)
  shufps xmm1, xmm1, 0h
  movlps xmm1, [edx]    // read(b.x,b.y)
  subps xmm0, xmm1

  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum all

  sqrtss xmm0, xmm0       // and finally, sqrt
  movss result, xmm0
End;
{$ENDIF}

{$IFDEF BENCHMARK} {$UNDEF SSE} {$ENDIF}

{$IFNDEF SSE}

Procedure Vector3D.Normalize; {$IFDEF FPC} Inline;{$ENDIF}
Var
  K:Single;
  {$IFDEF NEON_FPU}
  Temp:Vector3D;
  {$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  Temp := Self;
  normalize3_neon(@Temp, @Self);
  {$ELSE}
  K := (X*X) + (Y*Y) + (Z*Z);
  K := InvSqrt(K);
  X := X * K;
  Y := Y * K;
  Z := Z * K;
  {$ENDIF}
End;

Function Vector3D.Length:Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  {$IFDEF OXYGENE}
  Result := System.Math.Sqrt((X*X) + (Y*Y) + (Z*Z));
  {$ELSE}
  Result := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
  {$ENDIF}
End;

Function Vector3D.Distance(Const N:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
  A,B,c:Single;
Begin
  A := Self.X-N.X;
  B := Self.Y-N.Y;
  C := Self.Z-N.Z;
  Result := Sqrt((A*A) + (B*B) + (C*C));
End;
{$ENDIF}

Function Vector3D.LengthSquared:Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (X*X) + (Y*Y) + (Z*Z);
End;

Function Vector3D.Distance2D(Const N:Vector3D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
  A,B:Single;
Begin
  A := Self.X-N.X;
  B := Self.Z-N.Z;
  Result := Sqrt((A*A) + (B*B));
End;

Function TriangleNormal(Const V0,V1,V2:Vector3D):Vector3D;  {$IFDEF FPC} Inline;{$ENDIF}
Var
 A,B:Vector3D;
Begin
  A := VectorSubtract(V1,V0);
  B := VectorSubtract(V2,V0);
  Result := VectorCross(A,B);
  Result.Normalize();
End;

Function TriangleHeightNormal(Const V0,V1,V2:Single):Vector3D;
Begin
  Result:=TriangleNormal(VectorCreate(0.0, V0, 0.0),
                         VectorCreate(1.0, V1, 0.0),
                         VectorCreate(0.0, V2, 1.0));
End;

Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single):Single;
Var
  Normal:Vector3D;
Begin
  Normal := VectorUp;
  Result := GetTriangleHeight(H0, H1, H2, X, Y, Normal);
End;

Function GetTriangleHeight(H0,H1,H2:Single; X,Y:Single; Var Normal:Vector3D):Single;
Var
  D:Single;
  FloorNormal:Vector3D;
Begin
  FloorNormal := TriangleHeightNormal(H0, H1, H2);

  D := - (FloorNormal.Y * H0);
  Result := - ((FloorNormal.X * X) + (FloorNormal.Z * Y) + D) / FloorNormal.Y;

  Normal := FloorNormal;
End;

{$IFDEF OXYGENE}
Constructor Vector3D.Create(X,Y,Z:Single);
Begin
    Self.X := X;
    Self.Y := Y;
    Self.Z := Z;
End;
{$ENDIF}

End.
