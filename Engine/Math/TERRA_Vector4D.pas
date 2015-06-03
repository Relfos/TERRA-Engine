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
 * TERRA_
 * Implements a Vector4D class 
 ***********************************************************************************************************************
}
Unit TERRA_Vector4D;
{$I terra.inc}

Interface
Uses TERRA_Math, TERRA_Vector3D, TERRA_Matrix4x4;

Type
  PVector4D = ^Vector4D;
  Vector4D=Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    X:Single;
    Y:Single;
    Z:Single;
    W:Single;

    Function Equals(Const B:Vector4D):Boolean;

    Procedure Transform(Const M:Matrix4x4);
    // Returns a normalized Vector4D
    Procedure Normalize;

    {$IFDEF BENCHMARK}
    Procedure TransformSSE(Const M:Matrix4x4);
    Procedure NormalizeSSE;
    {$ENDIF}

    Procedure Add(Const B:Vector4D);
    Procedure Subtract(Const B:Vector4D);

    Procedure Scale(Const V:Single);

    Function Length:Single;
  End;

  Color4F = Vector4D;

Const
  Vector4DOne:Vector4D=(X:1.0; Y:1.0; Z:1.0; W:1.0);
  Vector4DZero:Vector4D=(X:0.0; Y:0.0; Z:0.0; W:0.0);

Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;Overload;
Function VectorCreate4D(Const V:Vector3D):Vector4D;Overload;

// Multiplies two Vector4Ds
Function Vector4DMultiply(Const A,B:Vector4D):Vector4D;

Function Vector4DAdd(Const A,B:Vector4D):Vector4D;

Function Vector4DScale(Const A:Vector4D; Const Value:Single):Vector4D;

// Returns the dot product between two vectors
Function Vector4DDot(Const A,B:Vector4D):Single; {$IFDEF FPC}Inline;{$ENDIF}

Implementation
Uses Math{$IFDEF SSE},TERRA_SSE{$ENDIF}{$IFDEF NEON_FPU},TERRA_NEON{$ENDIF};

Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
End;

Function VectorCreate4D(Const V:Vector3D):Vector4D;
Begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := 1.0;
End;

Function Vector4D.Length:Single;
Begin
  Result := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z) + Sqr(W));
End;

Function Vector4D.Equals(Const B:Vector4D):Boolean;
Begin
  Result := (Self.X=B.X) And (Self.Y=B.Y) And(Self.Z=B.Z) And(Self.W=B.W);
End;

Function Vector4DDot(Const A,B:Vector4D):Single; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  Result := dot4_neon_hfp(@A,@B);
  {$ELSE}
  Result := (A.X*B.X)+(A.Y*B.Y)+(A.Z*B.Z)+(A.W*B.W);
  {$ENDIF}
End;

Function Vector4DMultiply(Const A,B:Vector4D):Vector4D;
Begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Procedure Vector4D.NormalizeSSE; Register;
{$ELSE}
Procedure Vector4D.Normalize; Register;
{$ENDIF}
Asm
  movups xmm0, [eax]
  movaps xmm2, xmm0     // store copy of the vector
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y), sqr(z), sqr(w))
  movaps xmm1, xmm0
  movaps xmm3, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y), sqr(y), sqr(y))
  addps xmm1, xmm0        // add both
  shufps xmm0, xmm0, 0AAh  // xmm1 = (sqr(z), sqr(z), sqr(z), sqr(z))
  addps xmm0, xmm1        // sum x + y + z

  shufps xmm3, xmm3, 0FFh  // xmm2 = (sqr(w), sqr(w), sqr(w), sqr(w))
  addps xmm0, xmm3        // sum all

  shufps xmm0, xmm0, 0h  // broadcast length
  xorps xmm1, xmm1
  ucomiss xmm0, xmm1    // check for zero length
  je @@END
  rsqrtps xmm0, xmm0       // get reciprocal of length of vector
  mulps xmm2, xmm0      // normalize

  movups [eax], xmm2   // store normalized result
@@END:
End;

{$ENDIF}

{$IFDEF BENCHMARK}{$UNDEF SSE}{$ENDIF}

{$IFNDEF SSE}
Procedure Vector4D.Normalize;
Var
  Len:Single;
{$IFDEF NEON_FPU}
  Temp:Vector4D;
{$ENDIF}
Begin
{$IFDEF NEON_FPU}
  Temp := Self;
  normalize4_neon(@Temp, @Self);
{$ELSE}
  Len := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z) + Sqr(W));
  If (Len<=0) Then
    Exit;

  Len := 1.0 / Len;
  X := X * Len;
  Y := Y * Len;
  Z := Z * Len;
  W := W * Len;
{$ENDIF}
End;

{$IFDEF BENCHMARK}
{$ENDIF}
{$ENDIF}

{$IFDEF BENCHMARK}{$DEFINE SSE}{$ENDIF}

Procedure Vector4D.Add(Const B:Vector4D);
Begin
  X := X + B.X;
  Y := Y + B.Y;
  Z := Z + B.Z;
  W := W + B.W;
End;

Procedure Vector4D.Subtract(Const B:Vector4D);
Begin
  X := X - B.X;
  Y := Y - B.Y;
  Z := Z - B.Z;
  W := W - B.W;
End;

Procedure Vector4D.Scale(Const V:Single);
Begin
  X := X * V;
  Y := Y * V;
  Z := Z * V;
  W := W * V;
End;

Function Vector4DAdd(Const A,B:Vector4D):Vector4D;
Begin
  With Result Do
  Begin
    X := A.X + B.X;
    Y := A.Y + B.Y;
    Z := A.Z + B.Z;
    W := A.W + B.W;
  End;
End;

Function Vector4DScale(Const A:Vector4D; Const Value:Single):Vector4D;
Begin
  With Result Do
  Begin
    X := A.X * Value;
    Y := A.Y * Value;
    Z := A.Z * Value;
    W := A.W * Value;
  End;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Procedure Vector4D.TransformSSE(Const M:Matrix4x4); Register;
{$ELSE}
Procedure Vector4D.Transform(Const M:Matrix4x4); Register;
{$ENDIF}
Asm
  // copy vector, Matrix4x4 and result vector to registers
  mov ecx, eax
  mov edx, M

  movups xmm4, [edx]
  movups xmm5, [edx+$10]
  movups xmm6, [edx+$20]
  movups xmm7, [edx+$30]

  // calc x * m_1X
  movss xmm0, [ecx]
  shufps xmm0, xmm0, 0

  //mulps xmm0, [edx]
  mulps xmm0, xmm4

  // calc y * m_2X
  movss xmm1, [ecx+4]
  shufps xmm1, xmm1, 0

  //mulps xmm1, [edx+16]
  mulps xmm1, xmm5

  // calc z * m_3X
  movss xmm2, [ecx+8]
  shufps xmm2, xmm2, 0

  //mulps xmm2, [edx+32]
  mulps xmm2, xmm6

  // calc w * m_3X
  movss xmm3, [ecx+12]
  shufps xmm3, xmm3, 0

  //mulps xmm3, [edx+48]
  mulps xmm3, xmm7

  // calc final result
  addps xmm0, xmm1
  addps xmm2, xmm3
  addps xmm0, xmm2

  // save result
  movups [eax], xmm0
End;
{$ENDIF}

{$IFDEF BENCHMARK}{$UNDEF SSE}{$ENDIF}

{$IFNDEF SSE}
Procedure Vector4D.Transform(Const M:Matrix4x4); {$IFDEF FPC}Inline;{$ENDIF}
Var
  QX,QY,QZ,QW:Single;
{$IFDEF NEON_FPU}
  Temp:Vector4D;
{$ENDIF}
Begin
{$IFDEF NEON_FPU}
  Temp := Self;
  matvec4_neon(@M,@Temp,@Self);
{$ELSE}
  QX := X;
  QY := Y;
  QZ := Z;
  QW := W;

  X := QX*M.V[0] + QY*M.V[4] + QZ*M.V[8]  + QW*M.V[12];
  Y := QX*M.V[1] + QY*M.V[5] + QZ*M.V[9]  + QW*M.V[13];
  Z := QX*M.V[2] + QY*M.V[6] + QZ*M.V[10] + QW*M.V[14];
  W := QX*M.V[3] + QY*M.V[7] + QZ*M.V[11] + QW*M.V[15];
{$ENDIF}
End;
{$ENDIF}

End.
