{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_Vector2D;
{$I terra.inc}
{$ENDIF}

Interface

Type
  {$IFDEF OXYGENE}
  Vector2D = Class
  {$ELSE}
  PVector2D=^Vector2D;
  Vector2D=Packed Object
  {$ENDIF}
    X,Y:Single;

    {$IFDEF OXYGENE}
    Constructor Create(X,Y:Single);
    {$ENDIF}

    Function Equals(Const B:Vector2D):Boolean;

    Procedure Rotate(Const Angle:Single); Overload;
    Procedure Rotate(Const Angle:Single; Const Center:Vector2D); Overload;

    Procedure Add(Const B:Vector2D);
    Procedure Subtract(Const B:Vector2D);

    Procedure Scale(Const S:Single);Overload;
    Procedure Scale(Const B:Vector2D);Overload;

    Procedure Project(Const V:Vector2D);

    Procedure Normalize;

    Function Length:Single;
    Function Distance(Const N:Vector2D):Single;

    Function Dot(B:Vector2D):Single;

    {$IFDEF BENCHMARK}
    Function LengthSSE:Single;
    Function DistanceSSE(Const N:Vector2D):Single;
    {$ENDIF}
  End;

Function VectorCreate2D(Const X,Y:Single):Vector2D; 
Function VectorCross2D(Const A,B:Vector2D):Single;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;

Implementation
{$IFDEF NEON_FPU}Uses TERRA_NEON;{$ENDIF}

Function VectorCreate2D(Const X,Y:Single):Vector2D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := X;
  Result.Y := Y;
End;

Function VectorCross2D(Const A,B:Vector2D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (A.X * B.Y) - (A.Y * B.X);
End;

Function Vector2D.Equals(Const B:Vector2D):Boolean; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := (Self.X = B.X) And (Self.Y = B.Y);
End;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
End;

Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
End;

Procedure Vector2D.Project(Const V:Vector2D);
Var
  thisDotV:Single;
Begin
  thisDotV := Self.Dot(V);
  Self.X := V.X * thisDotV;
  Self.Y := V.Y * thisDotV;
End;

Procedure Vector2D.Rotate(Const Angle:Single; Const Center:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Self.Subtract(Center);
  Self.Rotate(Angle);
  Self.Add(Center);
End;

Procedure Vector2D.Rotate(Const Angle:Single); {$IFDEF FPC} Inline;{$ENDIF}
Var
  SX,SY:Single;
  Sine,Cosine:Single;
Begin
  SX := Self.X;
  SY := Self.Y;
  Sine := Sin(Angle);
  Cosine := Cos(Angle);
  X := (Sx * Cosine) - (Sy * Sine);
  Y := (Sx * Sine) + (Sy * Cosine);
End;

Procedure Vector2D.Add(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X + B.X;
  Y := Y + B.Y;
End;

{Procedure Vector2D.AddSSE(Const B:Vector2D);
Asm
  movlps xmm0, [self]   // xmm0 = (x,y)
  movlps xmm1, [edx]   // xmm1 = (b.x,b.y)
  addps xmm0, xmm1
  movlps [self], xmm0    // store result
End;
}

Procedure Vector2D.Subtract(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X - B.X;
  Y := Y - B.Y;
End;

Procedure Vector2D.Scale(Const S:Single); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * S;
  Y := Y * S;
End;

Procedure Vector2D.Scale(Const B:Vector2D); {$IFDEF FPC} Inline;{$ENDIF}
Begin
  X := X * B.X;
  Y := Y * B.Y;
End;

{$IFDEF SSE}
{$IFDEF BENCHMARK}
Function Vector2D.LengthSSE:Single;Register;
{$ELSE}
Function Vector2D.Length:Single;Register;
{$ENDIF}
Asm
  movlps xmm0, [eax]   // xmm0 = (x,y)
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 85  // xmm1 = (sqr(y), sqr(y))
  addps xmm0, xmm1        // add both

  sqrtss xmm0, xmm0       // and finally, sqrt}
  movss result, xmm0
End;

{$IFDEF BENCHMARK}
Function Vector2D.DistanceSSE(Const N:Vector2D):Single;Register;
{$ELSE}
Function Vector2D.Distance(Const N:Vector2D):Single;Register;
{$ENDIF}
Asm
  movlps xmm0, [eax]   // xmm0 = (x,y)
  movlps xmm1, [edx]   // xmm0 = (b.x,b.y)
  subps xmm0, xmm1
  mulps xmm0, xmm0      // xmm0 = (sqr(x), sqr(y))
  movaps xmm1, xmm0
  shufps xmm1, xmm1, 55h  // xmm1 = (sqr(y), sqr(y))
  addps xmm0, xmm1        // add both
  sqrtss xmm1, xmm0       // and finally, sqrt
  movss result, xmm1
End;
{$ENDIF}

{$IFDEF BENCHMARK} {$UNDEF SSE} {$ENDIF}

{$IFNDEF SSE}
Function Vector2D.Length:Single;
Begin
    {$IFDEF OXYGENE}
  Result := System.Math.Sqrt((X*X)+(Y*Y));
    {$ELSE}
  Result := Sqrt(Sqr(X)+Sqr(Y));
    {$ENDIF}
End;

Function Vector2D.Distance(Const N:Vector2D):Single;
Var
  A,B:Single;
Begin
  A := Self.X - N.X;
  B := Self.Y - N.Y;
    {$IFDEF OXYGENE}
  Result := System.Math.Sqrt((A*A)+(B*B));
    {$ELSE}
  Result := Sqrt(Sqr(A)+Sqr(B));
    {$ENDIF}
End;
{$ENDIF}

Procedure Vector2D.Normalize;
Var
  K:Single;
Begin
  K := Length;
  If (K<=1.0) Then
    Exit;
    
  X := X / K;
  Y := Y / K;
End;

Function Vector2D.Dot(B:Vector2D):Single;
Begin
  {$IFDEF NEON_FPU}
  Result := dot2_neon_hfp(@Self,@B);
  {$ELSE}
  Result := (Self.X * B.X) + (Self.Y * B.Y);
  {$ENDIF}
End;

{$IFDEF OXYGENE}
Constructor Vector2D.Create(X,Y:Single);
Begin
    Self.X := X;
    Self.Y := Y;
End;
{$ENDIF}

End.