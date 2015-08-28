{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by S?rgio Flores (relfos@gmail.com)
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
 * TERRA_Math
 * Implements math routines
 ***********************************************************************************************************************
}
{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_Math;
{$DEFINE OPERATOROVERLOAD}
{$I terra.inc}
{$ENDIF}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

Interface

Const
  Epsilon = 0.00001;

  Rad   = 0.017453292519;   // Pi/180
  Deg   = 57.29577951308;   // 180/Pi

  {$IFDEF OXYGENE}
  PI = 3.14159265359;
  MAXINT = 2147483647;
  {$ENDIF}

  M_SQRT2 = 1.41421356237309504880;

  RAND_MAX = (MAXINT-1);
  INV_RAND_MAX = 1.0 / (RAND_MAX + 1);

Type  
  Float = Double;

Function FloatMax(Const A,B:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Function FloatMin(Const A,B:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}

Function RandomFloat:Float; Overload;
Function RandomFloat(Const min,max:Float):Float; Overload;
Function RandomInt(Const min,max:Integer):Integer;

Function RealMod(Const n,d: Float): Float;

Function Atan2(Y,X :Float):Float;
Function Tan(X:Float):Float;

Function SmoothStep(A,B,X:Float):Float;

Function NearestPowerOfTwo(P:Cardinal):Cardinal;

Function LinearInterpolate(a,b, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicInterpolate(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Function HermiteInterpolate(pA, pB, vA, vB, u:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}

Function QuadraticBezierCurve(y0, y1, y2, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicBezierCurve(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}

Function InvSqrt(X:Single):Float; {$IFDEF FPC} Inline;{$ENDIF}


Function FloatMod(const x,y: Float):Float;

Function Sgn(Const X:Float):Float;

Function SmoothCurve(Delta:Float):Float;
Function SmoothCurveWithOffset(Delta, Offset:Float):Float;

Function Ln(Const X:Float):Float;
Function Log2(Const X:Float):Float;
//Function Log2(X:Integer):Float; Overload;
Function LNXP1(Const x:Float):Float;

Function float32_Unpack(Const x:Cardinal):Single;

Function ArcSin(Const X:Float):Float;
Function ArcCos(Const X:Float):Float;

Function Floor(Const X:Float):Integer;
Function Ceil(Const X:Float):Integer;

Function Power(Base:Float; Const Power: Integer): Float; Overload;
Function Power(Const Base, Power:Integer):Float; Overload;
Function Power(Const Base,Power:Float):Float; Overload;

Function Exp(Const X:Float): Float;

Function Clamp(Const X, Min, Max:Float):Float;

Implementation
Uses Math;

Const EulerNumber = 2.71828182846;

Function Power(Const Base, Power:Integer):Float;
Var
  I:Integer;
  Temp:Double;
Begin
  Temp := 1;

  For I:=0 To Pred(Power) Do
    Temp := Temp * Base;

  Result := Temp;
End;

Function Floor(Const X:Float):Integer;
Begin
  Result := Math.Floor(X);
End;

Function Ceil(Const X:Float):Integer;
Begin
  Result := Math.Ceil(X);
End;

Function ArcCos(Const X:Float):Float;
Begin
  Result := Math.ArcCos(X);
End;

Function ArcSin(Const X:Float):Float;
Begin
  Result := Math.ArcSin(X);
End;

Function Sgn(Const X:Float):Float;
Begin
  If (X<0) Then
    Result := -1
  Else
  If (X>0) Then
    Result := 1
  Else
    Result := 0
End;

Function Log2(Const X:Float):Float;
Begin
  Result := Ln(x) * 1.4426950408889634079;    // 1/ln(2)
End;

{Function Log2(X:Integer):Float;
Begin
  Result := 0;
  X := X Shr 1;
  While X<>0 Do
  Begin
    X := X Shr 1;
    Result := Result + 1;
  End;
End;}

(*{$IFDEF CPU386}
Function FloatMod(Const X,Y:Float):Float; Assembler; Register;
Asm
  fld dword ptr[y]
  fld dword ptr[x]
@r:
  fprem
  fstsw ax
  sahf
  jp @r
  fstp st(1)
End;
{$ELSE}*)

Function FloatMod(const x,y: Float):Float;
Var
  I:Integer;
Begin
  I := trunc(x / y);
  Result := x - y * i;
End;
//{$ENDIF}

{Function Atan2(Y,X :Float): Float;
Assembler;
asm
  fld [y]
  fld [x]
  fpatan
end;}

Function atan2 (y, x :Float):Float;
Begin
  If x > 0 Then
    Result := arctan (y/x)
  Else
  If x < 0 Then
    Result := arctan (y/x) + pi
  Else
    Result := pi/2 * sgn (y);
End;

Function Tan(X:Float):Float;
Begin
  Result := Math.Tan(X);
End;

Function RealMod(Const n,d: Float): Float;
Var
  i: integer;
Begin
  i := trunc(n / d);
  result := n - d * i;
End;

Function Power(Base:Float; Const Power: Integer): Float;
Var
  Y:Integer;
  Temp:Double;
begin
  Y := Abs(Power);
  Temp := 1.0;
  While Y > 0 do
  Begin
    While Not Odd(Y) do
    Begin
      Y := Y Shr 1;
      Base := Base * Base;
    End;
    Dec(Y);
    Temp := Temp * Base;
  End;

  If Power < 0.0 Then
    Temp := 1.0 / Temp;

  Result := Temp;
End;

Function Exp(Const X:Float): Float;
Var
  I, N:Integer;
  D:Double;
Begin
  If (X = 1.0) Then
    Result := EulerNumber
  Else
  If (x < 0) Then
    Result := 1.0 / Exp(-X)
  Else
  Begin
    N := 2;
    Result := 1.0 + X;
    Repeat
      D := X;
      For I:=2 To N Do
      Begin
        D := D * (X / I);
      End;

      Result := Result + D;
      Inc(N);
    Until (d <= Epsilon);
  End;
End;

Function Power(Const Base,Power:Float):Float; Overload;
Begin
  Result := Exp(Power * Ln(Base));
End;

// LN reimplemented here because FPC implementation of LN crashes on Android devices with Tegra 2 cpus..
Function Ln(Const X:Float):Float;
Var
  Lo, Hi, Mid, Val:Float;
Begin
  If(X<0) Then
  Begin
    Result := 0;
    Exit;
  End;

    // use recursion to get approx range
  If(X<1) Then
  Begin
    Result := - ln( 1 / X );
    Exit;
  End;

  If (X > EulerNumber) Then
  Begin
    Result := Ln(X / EulerNumber) + 1;
    Exit;
  End;

    // X is now between 1 and e
    // Y is between 0 and 1
  lo := 0.0;
  hi := 1.0;

  While(true) Do
  Begin
    mid := (lo+hi)/2;
    val := exp(mid);
    If(val > X) Then
      hi := mid;

    If (val < X) Then
      lo := mid;

    If (abs(val-X) < Epsilon) Then
    Begin
      Result := mid;
      Exit;
    End;
  End;
End;

// ldexp() multiplies x by 2**n.
Function ldexp( x: Real; N: Integer):Real;
Var
  r:Real;
Begin
  R := 1;
  If N>0 Then
  Begin
    While N>0 do
    Begin
      R:=R*2;
      Dec(N);
    End;
  End Else
  Begin
    While N<0 Do
    Begin
      R:=R/2;
      Inc(N);
    End;
  End;

  Result := x * R;
End;

Function LNXP1(Const x:Float):Float;
Var
  y:Float;
begin
  If (x>=4.0) Then
      Result := ln(1.0+x)
  Else
  Begin
    y:=1.0 + x;
    if (y=1.0) then
      Result :=x
    Else
    Begin
      Result := ln(y);     // lnxp1(-1) = ln(0) = -Inf

      If y>0.0 Then
        Result := Result + (x-(y-1.0))/y;
    End;
  End;
End;

{Function Pow(X, Y:Float):Float;
Begin
  If (X<=0) Then
    Result := 0
  Else
  If Y = 0.0 Then
    Result := 1.0
  Else if (X = 0.0) and (Y > 0.0) Then
    Result := 0.0
  Else if (Frac(Y) = 0.0) and (Abs(Y) <= MaxInt) then
    Result := IntPower(X, Integer(Trunc(Y)))
  Else
    Result := Exp(Y * Ln(X));
End;}

Function NearestPowerOfTwo(P:Cardinal):Cardinal;
Var
  I,N:Cardinal;
Begin
  Result := 0;
  For I:=14 DownTo 2 Do
  Begin
    N:=(1 Shl I);
    If N<P Then
     Break
    Else
      Result:=N;
  End;
End;

Function SmoothCurveWithOffset(Delta,Offset:Float):Float;
Begin
{  Offset := Offset * 2;
  Delta := SmoothCurve(Delta) * Offset;}

  If (Delta<Offset) Then
  Begin
    Delta := (Delta/Offset);
    Result := Abs(Sin(Delta*PI*0.5));
  End Else
  Begin
    Delta := Delta - Offset;
    Delta := (Delta/(1.0-Offset));
    Result := Abs(Cos(Delta*PI*0.5));
  End;
End;

Function SmoothCurve(Delta:Float):Float;
Begin
  Result := Abs(Sin(Delta*PI));
End;

Function LinearInterpolate(a,b, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (B * Mu) + A * (1.0 - Mu);
End;

Function CubicInterpolate(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Float;
Begin
   mu2 := (mu*mu);
   a0 := y3 - y2 - y0 + y1;
   a1 := y0 - y1 - a0;
   a2 := y2 - y0;
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Float;
Begin
   mu2 := (mu*mu);
   a0 := (-0.5 * y0) + (1.5 * y1) - (1.5 * y2) + (0.5 * y3);
   a1 := y0 - (2.5 * y1) + (2.0 * y2) - (0.5 * y3);
   a2 := (-0.5 * y0) + (0.5 * y2);
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function HermiteInterpolate(pA, pB, vA, vB, u:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Var
  u2, u3, B0,B1,B2,B3:Float;
Begin
  u2 := (u*u);
  u3 := u2*u;
  B0 := 2.0*u3 - 3.0*u2 + 1.0;
  B1 := -2.0*u3 + 3.0*u2;
  B2 := u3 - 2.0*u2 + u;
  B3 := u3 - u;
  Result := ( B0*pA + B1*pB + B2*vA + B3*vB );
End;

Function QuadraticBezierCurve(y0, y1, y2,  mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqr(1-mu) * y0 + 2 * (1-mu) * y1  + Sqr(mu) * y2;
End;

Function CubicBezierCurve(y0, y1, y2, y3, mu:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (1-mu) * Sqr(1-mu) * y0 + 3 * Sqr(1-mu) * y1  + 3 * (1-mu) * Sqr(mu) * y2 + Sqr(mu) * mu * y3;
End;

Function FloatMax(Const A,B:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function FloatMin(Const A,B:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

  {$IFDEF OXYGENE}
Var
    Rnd:System.Random;
    {$ENDIF}


Function RandomFloat:Float; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Begin
  {$IFDEF OXYGENE}
  If (Rnd = Nil) Then
    Rnd := new System.Random();
  Result := Rnd.Next(RAND_MAX) * INV_RAND_MAX;
  {$ELSE}
  Result := System.Random(RAND_MAX) * INV_RAND_MAX;
  {$ENDIF}
End;

Function RandomFloat(Const min,max:Float):Float; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Begin
  {$IFDEF OXYGENE}
    If (Rnd = Nil) Then
        Rnd := new System.Random();
	Result := Min + ((max - min) * (Rnd.Next(RAND_MAX) * INV_RAND_MAX));
  {$ELSE}
	Result := Min + ((max - min) * (System.Random(RAND_MAX) * INV_RAND_MAX));
  {$ENDIF}
End;

Function RandomInt(Const min,max:Integer):Integer;
Begin
  Result := Random(Succ(Max-Min)) + Min;
End;

Function SmoothStep(A, B, X:Float):Float; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (x < a) Then
    Result := 0.0
  Else
  If (x >= b) Then
    Result := 1.0
  Else
  Begin
    x := (x-a) / (b-a);
    Result := (x*x) * (3-2*x);
  End;
End;

{$IFDEF OXYGENE}
Function InvSqrt(X:Single):Float;
Begin
    Result := 1.0 / System.Math.Sqrt(X);
End;
{$ELSE}
{$OVERFLOWCHECKS OFF}

Function InvSqrt(X:Single):Float; {$IFDEF FPC} Inline;{$ENDIF}
Var
  I:Cardinal;
  xhalf:Single;
Begin
  xhalf := 0.5 * x;
  i := PCardinal(@x)^;         // get bits for floating value
  i := $5f3759df - (i Shr 1);   // give initial guess y0
  x := PSingle(@i)^;           // convert bits back to float
  x := X * (1.5 - xhalf*x*x);     // newton step, repeating this step
  x := X * (1.5 - xhalf*x*x); // increases accuracy
  Result := X;
End;

{$ENDIF}


Function float32_unpack(Const x:Cardinal):Single;
Var
  mantissa, sign, exp :Cardinal;
  res:extended;
Begin
   // from the specification
   mantissa := x and $1fffff;
   sign := x and $80000000;
   exp := (x and $7fe00000) shr 21;
   if sign=0 then res:=Integer(mantissa) else res:=-Integer(mantissa);
   Result := ldexp(res, exp-788);
End;

Function Clamp(Const X, Min, Max:Float):Float;
Begin
  If X<Min Then
    Result := Min
  Else
  If X>Max Then
    Result := Max
  Else
    Result := X;
End;

End.







