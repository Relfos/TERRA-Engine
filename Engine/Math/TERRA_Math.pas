{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_Math;
{$DEFINE OPERATOROVERLOAD}
{$I terra.inc}
{$ENDIF}

Interface

Const
  Epsilon = 0.00001;

  Rad   = 0.017453292519;   // Pi/180
  Deg   = 57.29577951308;   // 180/Pi

  {$IFDEF OXYGENE}
  PI = 3.14159265359;
  MAXINT = 2147483647;
  {$ENDIF}

  RAND_MAX = (MAXINT-1);
  INV_RAND_MAX = 1.0 / (RAND_MAX + 1);

Function FloatMax(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function FloatMin(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function RandomFloat:Single; Overload;
Function RandomFloat(Const min,max:Single):Single; Overload;
Function RandomInt(Const min,max:Integer):Integer;

Function RealMod(Const n,d: Single): Single;

Function Atan2(Y,X : extended): Extended;

Function SmoothStep(A,B,X:Single):Single;

Function NearestPowerOfTwo(P:Cardinal):Cardinal;

Function LinearInterpolate(a,b, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function HermiteInterpolate(pA, pB, vA, vB, u:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function QuadraticBezierCurve(y0, y1, y2, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Function CubicBezierCurve(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}

Function InvSqrt(X:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}


Function Pow(X, Y:Single):Single;

Function FloatMod(const x,y: Single):Single;

Function Sgn(Const X:Single):Single;

Function SmoothCurve(Delta:Single):Single;
Function SmoothCurveWithOffset(Delta, Offset:Single):Single;

Function Ln(X:Single):Single;
Function LNXP1(x:Single):Single;

Implementation

Function Sgn(Const X:Single):Single;
Begin
  If (X<0) Then
    Result := -1
  Else
  If (X>0) Then
    Result := 1
  Else
    Result := 0
End;

{$IFDEF CPU386}
Function FloatMod(Const X,Y:Single):Single; Assembler; Register;
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
{$ELSE}
Function FloatMod(const x,y: Single):Single;
Var
  I:Integer;
Begin
  I := trunc(x / y);
  Result := x - y * i;
End;
{$ENDIF}

{$IFDEF NOTUSED_WINDOWS}
Function Atan2(Y,X : extended): Extended;
Assembler;
asm
  fld [y]
  fld [x]
  fpatan
end;
{$ELSE}

Function atan2 (y, x : Extended) : Extended;
Begin
  if x > 0       then  result := arctan (y/x)
  else if x < 0  then  result := arctan (y/x) + pi
  else                 result := pi/2 * sgn (y);
End;
{$ENDIF}

Function RealMod(Const n,d: Single): Single;
Var
  i: integer;
Begin
  i := trunc(n / d);
  result := n - d * i;
End;

Function IntPower(X:Single; I: Integer): Single;
var
  Y: Integer;
begin
  Y := Abs(I);
  Result := 1.0;
  While Y > 0 do
  Begin
    While Not Odd(Y) do
    Begin
      Y := Y Shr 1;
      X := X * X;
    End;
    Dec(Y);
    Result := Result * X;
  End;
  if I < 0.0 Then
    Result := 1.0 / Result;
End;

// LN reimplemented here because FPC implementation of LN crashes on Android devices with Tegra 2 cpus.. 
Function ln(X:Single):Single;
Var
  Lo, Hi, Mid, Val, E:Single;
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

  E := exp(1);

  If (X>E) Then
  Begin
    Result := ln(X/E) + 1;
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

Function LNXP1(x:Single):Single;
Var
  y:Single;
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

Function Pow(X, Y:Single):Single;
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
End;

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

Function SmoothCurveWithOffset(Delta,Offset:Single):Single;
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

Function SmoothCurve(Delta:Single):Single;
Begin
  Result := Abs(Sin(Delta*PI));
End;

Function LinearInterpolate(a,b, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (B * Mu) + A * (1.0 - Mu);
End;

Function CubicInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Single;
Begin
   mu2 := (mu*mu);
   a0 := y3 - y2 - y0 + y1;
   a1 := y0 - y1 - a0;
   a2 := y2 - y0;
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function CatmullRomInterpolate(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
   a0,a1,a2,a3,mu2:Single;
Begin
   mu2 := (mu*mu);
   a0 := (-0.5 * y0) + (1.5 * y1) - (1.5 * y2) + (0.5 * y3);
   a1 := y0 - (2.5 * y1) + (2.0 * y2) - (0.5 * y3);
   a2 := (-0.5 * y0) + (0.5 * y2);
   a3 := y1;
   Result := (a0 * mu * mu2) + (a1 * mu2) + (a2 * mu) + a3;
End;

Function HermiteInterpolate(pA, pB, vA, vB, u:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
  u2, u3, B0,B1,B2,B3:Single;
Begin
  u2 := (u*u);
  u3 := u2*u;
  B0 := 2.0*u3 - 3.0*u2 + 1.0;
  B1 := -2.0*u3 + 3.0*u2;
  B2 := u3 - 2.0*u2 + u;
  B3 := u3 - u;
  Result := ( B0*pA + B1*pB + B2*vA + B3*vB );
End;

Function QuadraticBezierCurve(y0, y1, y2,  mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Sqr(1-mu) * y0 + 2 * (1-mu) * y1  + Sqr(mu) * y2;
End;

Function CubicBezierCurve(y0, y1, y2, y3, mu:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (1-mu) * Sqr(1-mu) * y0 + 3 * Sqr(1-mu) * y1  + 3 * (1-mu) * Sqr(mu) * y2 + Sqr(mu) * mu * y3;
End;

Function FloatMax(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function FloatMin(Const A,B:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

  {$IFDEF OXYGENE}
Var
    Rnd:System.Random;
    {$ENDIF}


Function RandomFloat:Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Begin
  {$IFDEF OXYGENE}
  If (Rnd = Nil) Then
    Rnd := new System.Random();
  Result := Rnd.Next(RAND_MAX) * INV_RAND_MAX;
  {$ELSE}
  Result := System.Random(RAND_MAX) * INV_RAND_MAX;
  {$ENDIF}
End;

Function RandomFloat(Const min,max:Single):Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}
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

Function SmoothStep(A, B, X:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
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
Function InvSqrt(X:Single):Single;
Begin
    Result := 1.0 / System.Math.Sqrt(X);
End;
{$ELSE}
{$OVERFLOWCHECKS OFF}
Function InvSqrt(X:Single):Single; {$IFDEF FPC} Inline;{$ENDIF}
Var
  I:Cardinal;
  xhalf:Single;
Begin
  xhalf := 0.5*x;
  i := PCardinal(@x)^;         // get bits for floating value
  i := $5f3759df - (i Shr 1);   // give initial guess y0
  x := PSingle(@i)^;           // convert bits back to float
  x := X * (1.5 - xhalf*x*x);     // newton step, repeating this step
  x := X * (1.5 - xhalf*x*x); // increases accuracy
  Result := X;
End;

{$ENDIF}
End.






