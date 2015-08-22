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
 * TERRA_Vector2D
 * Implements a 2D vector class
 ***********************************************************************************************************************
}
{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_Vector2D;
{$I terra.inc}
{$ENDIF}

Interface
Uses TERRA_Object, TERRA_Vector3D, TERRA_Math, TERRA_String, TERRA_Utils, TERRA_Tween;

Type
  {$IFDEF OXYGENE}
  Vector2D = Class
  {$ELSE}
  PVector2D=^Vector2D;
  Vector2D=Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
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

    Function Length():Single;
    Function LengthSquared():Single;
    Function Distance(Const N:Vector2D):Single;

    {$IFDEF BENCHMARK}
    Function LengthSSE:Single;
    Function DistanceSSE(Const N:Vector2D):Single;
    {$ENDIF}
  End;

  Vector2DProperty = Class(TweenableProperty)
    Protected
      _X:FloatProperty;
      _Y:FloatProperty;

      Function GetVectorValue:Vector2D;
      Procedure SetVectorValue(const NewValue:Vector2D);

      Procedure UpdateTweens(); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Vector2D);
      Procedure Release(); Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector2D; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Function Stringify(Const Val:Vector2D):TERRAString;

      Property X:FloatProperty Read _X;
      Property Y:FloatProperty Read _Y;

      Property Value:Vector2D Read GetVectorValue Write SetVectorValue;
  End;
  
Function StringToVector2D(S:TERRAString):Vector2D;
Function VectorCreate2D(Const X,Y:Single):Vector2D;

Function VectorDot2D(Const A,B:Vector2D):Single;  
Function VectorCross2D(Const A,B:Vector2D):Single;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;

Function VectorScale2D(Const A:Vector2D; Const S:Single):Vector2D;
Function VectorLerp2D(Const A, B:Vector2D; Const Delta:Single):Vector2D;

Function VectorAngle3D(Const A,B:Vector3D):Single;
Function VectorAngle2D(Const A,B:Vector2D):Single;

Implementation
{$IFDEF NEON_FPU}Uses TERRA_NEON;{$ENDIF}

Function StringToVector2D(S:TERRAString):Vector2D;
Begin
  Result.X := StringToFloat(StringGetNextSplit(S, Ord('/')));
  Result.Y := StringToFloat(StringGetNextSplit(S, Ord('/')));
End;

Function VectorCreate2D(Const X,Y:Single):Vector2D; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.X := X;
  Result.Y := Y;
End;

Function VectorDot2D(Const A,B:Vector2D):Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  {$IFDEF NEON_FPU}
  Result := dot2_neon_hfp(@A, @B);
  {$ELSE}
  Result := (A.X * B.X) + (A.Y * B.Y);
  {$ENDIF}
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

Function VectorScale2D(Const A:Vector2D; Const S:Single):Vector2D;
Begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
End;

Function VectorLerp2D(Const A, B:Vector2D; Const Delta:Single):Vector2D;
Begin
  Result.X := A.X + Delta * (B.X - A.X);
  Result.Y := A.Y + Delta * (B.Y - A.Y);
End;

Procedure Vector2D.Project(Const V:Vector2D);
Var
  thisDotV:Single;
Begin
  thisDotV := VectorDot2D(Self, V);
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
Function Vector2D.LengthSquared:Single;
Begin
  Result := Sqr(X)+Sqr(Y);
End;

Function Vector2D.Length:Single;
Begin
    {$IFDEF OXYGENE}
  Result := System.Math.Sqrt(Self.LengthSquared);
    {$ELSE}
  Result := Sqrt(Self.LengthSquared);
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

{$IFDEF OXYGENE}
Constructor Vector2D.Create(X,Y:Single);
Begin
    Self.X := X;
    Self.Y := Y;
End;
{$ENDIF}

Function VectorAngle2D(Const A,B:Vector2D):Single;
Var
  XDiff, YDiff: Single;
  fpAngle: Single;
  N:Vector2D;
begin
  N := B;
  N.Subtract(A);
  N.Normalize();

  fpAngle := Atan2(-N.Y, N.X);
  Result := fpAngle + 90*RAD;
End;

Function VectorAngle3D(Const A,B:Vector3D):Single;
Var
  PA, PB:Vector2D;
Begin
  PA := VectorCreate2D(A.X, A.Z);
  PB   := VectorCreate2D(B.X, B.Z);
  Result := VectorAngle2D(PA, PB);
End;



{ Vector2DProperty }
Constructor Vector2DProperty.Create(Const Name:TERRAString; const InitValue: Vector2D);
Begin
  _ObjectName := Name;
  _X := FloatProperty.Create('x', InitValue.X);
  _Y := FloatProperty.Create('y', InitValue.Y);
End;

Procedure Vector2DProperty.Release;
Begin
  ReleaseObject(_X);
  ReleaseObject(_Y);
End;

(*Function Vector2DProperty.GetBlob: TERRAString;
Begin
  Result := X.GetBlob() + '/'+ Y.GetBlob();
End;

Procedure Vector2DProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;
  X.SetBlob(StringGetNextSplit(S, Ord('/')));
  Y.SetBlob(StringGetNextSplit(S, Ord('/')));
End;*)

Function Vector2DProperty.GetVectorValue: Vector2D;
Begin
  Result.X := X.Value;
  Result.Y := Y.Value;
End;

Procedure Vector2DProperty.SetVectorValue(const NewValue: Vector2D);
Begin
  X.Value := NewValue.X;
  Y.Value := NewValue.Y;
End;

Function Vector2DProperty.GetObjectType: TERRAString;
Begin
  Result := 'vec2';
End;

Procedure Vector2DProperty.AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.AddTween(Ease, StringToVector2D(StartValue), StringToVector2D(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure Vector2DProperty.AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector2D; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.X.AddTween(Ease, StartValue.X, TargetValue.X, Duration, Delay, Callback, CallTarget);
  Self.Y.AddTween(Ease, StartValue.Y, TargetValue.Y, Duration, Delay, Nil);
End;

Function Vector2DProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := X;
  1:  Result := Y;
  Else
    Result := Nil;
  End;
End;

Procedure Vector2DProperty.UpdateTweens;
Begin
  X.UpdateTweens();
  Y.UpdateTweens();
End;

Function Vector2DProperty.Stringify(Const Val:Vector2D):TERRAString;
Begin
  Result := FloatProperty.Stringify(Val.X) + '/'+ FloatProperty.Stringify(Val.Y);
End;


End.