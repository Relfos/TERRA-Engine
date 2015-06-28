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
 * TERRA_Tween
 * Implements a huge number of tweening formulas 
 ***********************************************************************************************************************
}

Unit TERRA_Tween;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Math;

Type
  TweenEaseType = (
  	easeLinear = 0,
  	easeInQuad = 1,
  	easeOutQuad = 2,
  	easeInOutQuad = 3,
  	easeOutInQuad = 4,
  	easeInCubic = 5,
  	easeOutCubic = 6,
  	easeInOutCubic = 7,
  	easeOutInCubic = 8,
  	easeInQuart = 9,
  	easeOutQuart = 10,
  	easeInOutQuart = 11,
  	easeOutInQuart = 12,
  	easeInSine = 13,
  	easeOutSine = 14,
  	easeInOutSine = 15,
  	easeOutInSine = 16,
  	easeInExpo = 17,
  	easeOutExpo = 18,
  	easeInOutExpo = 19,
  	easeOutInExpo = 20,
  	easeInCirc = 21,
  	easeOutCirc = 22,
  	easeInOutCirc = 23,
  	easeOutInCirc = 24,
  	easeInElastic = 25,
  	easeOutElastic = 26,
  	easeInOutElastic = 27,
  	easeOutInElastic = 28,
  	easeInBack = 29,
  	easeOutBack = 30,
  	easeInOutBack = 31,
  	easeOutInBack = 32,
  	easeInBounce = 33,
  	easeOutBounce = 34,
  	easeInOutBounce = 35,
  	easeOutInBounce = 36,
    easeWiggle = 37
  );


(*Type
  Tween = Class(TERRAObject)
    Protected
      _Object:Pointer;
      _Data:Pointer;
      _DataType:Integer;

      _CallbackTarget:TERRAObject;

      Function Read:Single;
      Procedure Write(Value:Single);
      Procedure Update;

    Public
      MotionType:Integer;
      Time:Cardinal;
      Delay:Cardinal;
      OnFinished:TweenCallback;

      Constructor Create(Obj:Pointer; MyType:Integer; Data:Pointer; TargetValue:Single; CallbackTarget:TERRAObject);
      Procedure Release; Override;

      Property Finished:Boolean Read _Finished;
      Property EaseType:Cardinal Read _EaseType Write _EaseType;
  End;

  TweenManager = Class(ApplicationComponent)
    Protected
      _Tweens:Array Of Tween;
      _TweenCount:Integer;

      Procedure Add(T:Tween);

    Public
      Procedure Update; Override;
      Procedure Init; Override;

      Class Function Instance:TweenManager;

      Function HasTween(T:Tween):Boolean;

      Procedure RemoveTween(Obj:Pointer);

      Procedure Clear;

      Procedure Release; Override;
  End;*)

Function GetEaseInQuad(t, b,c,d:Single):Single;
Function GetEaseOutQuad(t, b,c,d:Single):Single;
Function GetEaseInOutQuad(t, b,c,d:Single):Single;
Function GetEaseOutInQuad(t, b,c,d:Single):Single;
Function GetEaseInCubic(t, b,c,d:Single):Single;
Function GetEaseOutCubic(t, b,c,d:Single):Single;
Function GetEaseInOutCubic(t, b,c,d:Single):Single;
Function GetEaseOutInCubic(t, b,c,d:Single):Single;
Function GetEaseInQuart(t, b,c,d:Single):Single;
Function GetEaseOutQuart(t, b,c,d:Single):Single;
Function GetEaseInOutQuart(t, b,c,d:Single):Single;
Function GetEaseOutInQuart(t, b,c,d:Single):Single;
Function GetEaseInSine(t, b,c,d:Single):Single;
Function GetEaseOutSine(t, b,c,d:Single):Single;
Function GetEaseInOutSine(t, b,c,d:Single):Single;
Function GetEaseOutInSine(t, b,c,d:Single):Single;
Function GetEaseInExpo(t, b,c,d:Single):Single;
Function GetEaseOutExpo(t, b,c,d:Single):Single;
Function GetEaseInOutExpo(t, b,c,d:Single):Single;
Function GetEaseOutInExpo(t, b,c,d:Single):Single;
Function GetEaseInCirc(t, b,c,d:Single):Single;
Function GetEaseOutCirc(t, b,c,d:Single):Single;
Function GetEaseInOutCirc(t, b,c,d:Single):Single;
Function GetEaseOutInCirc(t, b,c,d:Single):Single;
Function GetEaseInElastic(t, b,c,d:Single):Single;
Function GetEaseOutElastic(t, b,c,d:Single):Single;
Function GetEaseInOutElastic(t, b,c,d:Single):Single;
Function GetEaseOutInElastic(t, b,c,d:Single):Single;
Function GetEaseInBack(t, b,c,d:Single):Single;
Function GetEaseOutBack(t, b,c,d:Single):Single;
Function GetEaseInOutBack(t, b,c,d:Single):Single;
Function GetEaseOutInBack(t, b,c,d:Single):Single;
Function GetEaseOutBounce(t, b,c,d:Single):Single;
Function GetEaseInBounce(t, b,c,d:Single):Single;
Function GetEaseInOutBounce(t, b,c,d:Single):Single;
Function GetEaseOutInBounce(t, b,c,d:Single):Single;
Function GetEaseWiggle(t, b,c,d:Single):Single;

Function GetEase(Const Delta:Single; Const Ease:TweenEaseType):Single;

Implementation
Uses TERRA_OS, TERRA_GraphicsManager;

(*Var
  _TweenManager_Instance:ApplicationObject = Nil;*)
  
{     * @param t     Current time (in frames or seconds).
     * @param b     Starting value.
     * @param c     Change needed in value.
     * @param d     Expected easing duration (in frames or seconds).
     * @return      The correct value.}
Function GetEaseInQuad(t, b,c,d:Single):Single;
Begin
  T := T / D;
  Result := c * t * t + b;
End;

Function GetEaseOutQuad(t, b,c,d:Single):Single;
Begin
  T := T / D;
  Result := -c *t*(t-2) + b;
End;

// Easing equation float for a quadratic (t^2) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutQuad(t, b,c,d:Single):Single;
Begin
  T := T / (D/2);
  if (T < 1) Then
    Result := c/2*t*t + b
  Else
  Begin
    T := T - 1;
    Result := -c/2 * (T*(T-2) - 1) + b;
  End;
End;

// Easing equation float for a quadratic (t^2) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInQuad(t, b,c,d:Single):Single;
Begin
  if (t < d/2) Then
    Result := GetEaseOutQuad (t*2, b, c/2, d)
  Else
    Result := GetEaseInQuad((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a cubic (t^3) easing in: accelerating from zero velocity.
Function GetEaseInCubic(t, b,c,d:Single):Single;
Begin
  T := T/D;
  Result := c*t*t*t + b;
End;

// Easing equation float for a cubic (t^3) easing out: decelerating from zero velocity.
Function GetEaseOutCubic(t, b,c,d:Single):Single;
Begin
  T := T / D;
  T := T - 1;
  Result := c*(t*t*t + 1) + b;
End;


// Easing equation float for a cubic (t^3) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutCubic(t, b,c,d:Single):Single;
Begin
  T := T / (D*0.5);
  if (T < 1) Then
    Result := (c/2)*t*t*t + b
  Else
  Begin
    T := T - 2;
    Result := (c/2)*(t*t*t + 2) + b;
  End;
End;

// Easing equation float for a cubic (t^3) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInCubic(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutCubic (t*2, b, c/2, d)
  Else
    Result := GetEaseInCubic((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a quartic (t^4) easing in: accelerating from zero velocity.
Function GetEaseInQuart(t, b,c,d:Single):Single;
Begin
  T := T / D;
  Result := c*t*t*t*t + b;
End;

//Easing equation float for a quartic (t^4) easing out: decelerating from zero velocity.
Function GetEaseOutQuart(t, b,c,d:Single):Single;
Begin
  T := T / D;
  T := T - 1;
  Result := -c * (T*t*t*t - 1) + b;
End;

// Easing equation float for a quartic (t^4) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutQuart(t, b,c,d:Single):Single;
Begin
  T := T / (D*0.5);
  If (T < 1) Then
    Result := (c/2)*t*t*t*t + b
  Else
  Begin
    T := T - 2;
    Result := (-c/2) * (t*t*t*t - 2) + b;
  End;
End;

// Easing equation float for a quartic (t^4) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInQuart(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutQuart (t*2, b, c/2, d)
  Else
    Result := GetEaseInQuart((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a sinusoidal (sin(t)) easing in: accelerating from zero velocity.
Function GetEaseInSine(t, b,c,d:Single):Single;
Begin
  Result := -c * Cos(t/d * (PI/2)) + c + b;
End;

// Easing equation float for a sinusoidal (sin(t)) easing out: decelerating from zero velocity.
Function GetEaseOutSine(t, b,c,d:Single):Single;
Begin
  Result := c * Sin(t/d * (PI/2)) + b;
End;

// Easing equation float for a sinusoidal (sin(t)) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutSine(t, b,c,d:Single):Single;
Begin
  Result := -c/2 * (Cos(PI*t/d) - 1) + b;
End;

//Easing equation float for a sinusoidal (sin(t)) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInSine(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutSine (t*2, b, c/2, d)
  Else
    Result := GetEaseInSine((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for an exponential (2^t) easing in: accelerating from zero velocity.
Function GetEaseInExpo(t, b,c,d:Single):Single;
Begin
  If (t<=0) Then
    Result := B
  Else
    Result :=  c * Power(2, 10 * (t/d - 1)) + b - c * 0.001;
End;

// Easing equation float for an exponential (2^t) easing out: decelerating from zero velocity.
Function GetEaseOutExpo(t, b,c,d:Single):Single;
Begin
  If (t>=d) Then
    Result := b+c
  Else
    Result := c * 1.001 * (-Power(2, -10 * t/d) + 1) + b;
End;

// Easing equation float for an exponential (2^t) easing in/out: acceleration until halfway, then deceleration.

Function GetEaseInOutExpo(t, b,c,d:Single):Single;
Begin
  If (t<=0) Then
    Result := b
  Else
  If (t>=d) Then
    Result := b+c
  Else
  Begin
    T := T / D;
    If (T/2 < 1) Then
      Result := c/2 * Power(2, 10 * (t - 1)) + b - c * 0.0005
    Else
      Result := c/2 * 1.0005 * (-Power(2, -10 * --t) + 2) + b;
  End;
End;

// Easing equation float for an exponential (2^t) easing out/in: deceleration until halfway, then acceleration.

Function GetEaseOutInExpo(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutExpo (t*2, b, c/2, d)
  Else
    Result := GetEaseInExpo((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a circular (sqrt(1-t^2)) easing in: accelerating from zero velocity.

Function GetEaseInCirc(t, b,c,d:Single):Single;
Begin
  T := T / D;
  Result := -c * (Sqrt(1 - t*t) - 1) + b;
End;

// Easing equation float for a circular (sqrt(1-t^2)) easing out: decelerating from zero velocity.
Function GetEaseOutCirc(t, b,c,d:Single):Single;
Begin
  T := T / D;
  T := T - 1;
  Result := c * Sqrt(1 - T*t) + b;
End;

// Easing equation float for a circular (sqrt(1-t^2)) easing in/out: acceleration until halfway, then deceleration.

Function GetEaseInOutCirc(t, b,c,d:Single):Single;
Begin
  T := T / (D*0.5);
  If (t < 1) Then
    Result := (-c/2) * (Sqrt(1 - t*t) - 1) + b
  Else
  Begin
    T := T - 2;
    Result := (c/2) * (Sqrt(1 - t*t) + 1) + b;
  End;
End;

// Easing equation float for a circular (sqrt(1-t^2)) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInCirc(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutCirc (t*2, b, c/2, d)
  Else
    Result := GetEaseInCirc((t*2)-d, b+c/2, c/2, d);
End;


// Easing equation float for an elastic (exponentially decaying sine wave) easing in: accelerating from zero velocity.
Function GetEaseInElastic(t, b,c,d:Single):Single;
Var
  P,S,A,PostFix:Single;
Begin
  If (t<=0) Then
  Begin
    Result := b;
    Exit;
  End;

  T := T / D;
  If (T>=1) Then
  Begin
    Result := b+c;
    Exit;
  End;

  P :=  d *0.3;
  A := c;
  S := p/4;

  T := T - 1;
  PostFix := a*Power(2,10*T);
  Result := -(postFix * sin((t*d-s)*(2*PI)/p )) + b;
End;

// Easing equation float for an elastic (exponentially decaying sine wave) easing out: decelerating from zero velocity.
Function GetEaseOutElastic(t, b,c,d:Single):Single;
Var
  P,S,A:Single;
Begin
  If (t<=0) Then
  Begin
    Result := b;
    Exit;
  End;

  T := T / D;
  If (T>=1) Then
  Begin
    Result := b+c;
    Exit;
  End;

  p := d*0.3;
  s := 0;
  a := 0;
  If (a = 0) Or (a < Abs(c)) Then
  Begin
    a := c;
    s := p/4;
  End Else
  Begin
    s := p/(2*PI) * ArcSin(c/a);
  End;

  Result := (a*Power(2,-10*t) * Sin( (t*d-s)*(2*PI)/p ) + c + b);
End;

// Easing equation float for an elastic (exponentially decaying sine wave) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutElastic(t, b,c,d:Single):Single;
Var
  P,S,A,PostFix:Single;
Begin
  If (t=0) Then
  Begin
    Result := b;
    Exit;
  End;

  T := T / (D*0.5);
  If (T=2) Then
  Begin
    Result := b+c;
    Exit;
  End;

  p := d*(0.3*1.5);
  a := C;
  s := P/4;

  If (t < 1) Then
  Begin
    T := T - 1;
    postFix := a*Power(2,10*t); // postIncrement is evil
    Result := -0.5*(postFix* sin( (t*d-s)*(2*PI)/p )) + b;
  End Else
  Begin
    T := T - 1;
    postFix := a*Power(2,-10*t); // postIncrement is evil
    Result := postFix * sin( (t*d-s)*(2*PI)/p )*0.5 + c + b;
  End;
      {
  If (a = 0) Or (a < Abs(c)) Then
  Begin
    a := c;
    s := p/4;
  End Else
  Begin
    s := p/(2*PI) * Arcsin (c/a);
  End;

  T := T -1;
  If (t < 1) Then
    Result := -0.5*(a*Pow(2,10*T) * Sin( (t*d-s)*(2*PI)/p )) + b
  Else
    Result := a*Pow(2,-10*T) * Sin( (t*d-s)*(2*PI)/p )*0.5 + c + b;}
End;

// Easing equation float for an elastic (exponentially decaying sine wave) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInElastic(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutElastic (t*2, b, c/2, d)
  Else
    Result := GetEaseInElastic((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing in: accelerating from zero velocity.
Function GetEaseInBack(t, b,c,d:Single):Single;
Const
  s = 1.70158;
Begin
  T := T / D;
  Result := c*t*t*((s+1)*t - s) + b;
End;

// Easing equation float for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing out: decelerating from zero velocity.
Function GetEaseOutBack(t, b,c,d:Single):Single;
Const
  S = 1.70158;
Begin
  T := T / D;
  Result := c*((T-1)*t*((s+1)*t + s) + 1) + b;
End;

// Easing equation float for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutBack(t, b,c,d:Single):Single;
Const
  S = 1.70158 * 1.525;
Var
  PostFix:Single;
Begin
  T := T / (D*0.5);

  If (T < 1) Then
    Result := c/2*(t*t*((s+1)*t - s)) + b
  Else
  Begin
    postFix := T;
    T := T - 2;
    Result := c/2*((postFix)*t*((s+1)*t + s) + 2) + b;
  End;
End;

//Easing equation float for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInBack(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutBack(t*2, b, c/2, d)
  Else
    Result := GetEaseInBack((t*2)-d, b+c/2, c/2, d);
End;

// Easing equation float for a bounce (exponentially decaying parabolic bounce) easing out: decelerating from zero velocity.
Function GetEaseOutBounce(t, b,c,d:Single):Single;
Begin
  T := T / D;
  if (T < (1/2.75)) Then
    Result := c*(7.5625*t*t) + b
  Else
  If (t < (2/2.75)) Then
  Begin
    T := T - (1.5/2.75);
    Result := c*(7.5625*T*t + 0.75) + b;
  End Else
  If (t < (2.5/2.75)) Then
  Begin
    T := T - (2.25/2.75);
    Result := c*(7.5625*T*t + 0.9375) + b;
  End Else
  Begin
    T := T - (2.625/2.75);
    Result := c*(7.5625*T*t + 0.984375) + b;
  End;
End;

// Easing equation float for a bounce (exponentially decaying parabolic bounce) easing in: accelerating from zero velocity.
Function GetEaseInBounce(t, b,c,d:Single):Single;
Begin
  Result := c - GetEaseOutBounce (d-t, 0, c, d) + b;
End;

//Easing equation float for a bounce (exponentially decaying parabolic bounce) easing in/out: acceleration until halfway, then deceleration.
Function GetEaseInOutBounce(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseInBounce (t*2, 0, c, d) * 0.5 + b
  Else
    Result := GetEaseOutBounce (t*2-d, 0, c, d) * 0.5 + c * 0.5 + b;
End;

//Easing equation float for a bounce (exponentially decaying parabolic bounce) easing out/in: deceleration until halfway, then acceleration.
Function GetEaseOutInBounce(t, b,c,d:Single):Single;
Begin
  If (t < d/2) Then
    Result := GetEaseOutBounce (t*2, b, c/2, d)
  Else
    Result := GetEaseInBounce((t*2)-d, b+c/2, c/2, d);
End;

Function GetEaseWiggle(t, b,c,d:Single):Single;
Begin
  If (T>=D) Then
    Result := B
  Else
    Result := B + Cos(RealMod(Application.GetTime{*D}, 360)*RAD)*C;
End;

Function GetEase(Const Delta:Single; Const Ease:TweenEaseType):Single;
Begin
  Case Ease Of
	  easeInQuad: Result := GetEaseInQuad(Delta, 0.0, 1.0, 1.0);
    easeOutQuad: Result := GetEaseOutQuad(Delta, 0.0, 1.0, 1.0);
  	easeInOutQuad: Result := GetEaseInOutQuad(Delta, 0.0, 1.0, 1.0);
  	easeOutInQuad: Result := GetEaseOutInQuad(Delta, 0.0, 1.0, 1.0);
  	easeInCubic: Result := GetEaseInCubic(Delta, 0.0, 1.0, 1.0);
  	easeOutCubic: Result := GetEaseOutCubic(Delta, 0.0, 1.0, 1.0);
  	easeInOutCubic: Result := GetEaseInOutCubic(Delta, 0.0, 1.0, 1.0);
  	easeOutInCubic: Result := GetEaseOutInCubic(Delta, 0.0, 1.0, 1.0);
  	easeInQuart: Result := GetEaseInQuart(Delta, 0.0, 1.0, 1.0);
  	easeOutQuart: Result := GetEaseOutQuart(Delta, 0.0, 1.0, 1.0);
  	easeInOutQuart: Result := GetEaseInOutQuart(Delta, 0.0, 1.0, 1.0);
  	easeOutInQuart: Result := GetEaseOutInQuart(Delta, 0.0, 1.0, 1.0);
  	easeInSine: Result := GetEaseInSine(Delta, 0.0, 1.0, 1.0);
  	easeOutSine: Result := GetEaseOutSine(Delta, 0.0, 1.0, 1.0);
  	easeInOutSine: Result := GetEaseInOutSine(Delta, 0.0, 1.0, 1.0);
    easeOutInSine: Result := GetEaseOutInSine(Delta, 0.0, 1.0, 1.0);
  	easeInExpo: Result := GetEaseInExpo(Delta, 0.0, 1.0, 1.0);
  	easeOutExpo: Result := GetEaseOutExpo(Delta, 0.0, 1.0, 1.0);
  	easeInOutExpo: Result := GetEaseInOutExpo(Delta, 0.0, 1.0, 1.0);
  	easeOutInExpo: Result := GetEaseOutInExpo(Delta, 0.0, 1.0, 1.0);
  	easeInCirc : Result := GetEaseInCirc(Delta, 0.0, 1.0, 1.0);
  	easeOutCirc: Result := GetEaseOutCirc(Delta, 0.0, 1.0, 1.0);
  	easeInOutCirc: Result := GetEaseInOutCirc(Delta, 0.0, 1.0, 1.0);
  	easeOutInCirc: Result := GetEaseOutInCirc(Delta, 0.0, 1.0, 1.0);
  	easeInElastic: Result := GetEaseInElastic(Delta, 0.0, 1.0, 1.0);
  	easeOutElastic: Result := GetEaseOutElastic(Delta, 0.0, 1.0, 1.0);
  	easeInOutElastic: Result := GetEaseInOutElastic(Delta, 0.0, 1.0, 1.0);
  	easeOutInElastic: Result := GetEaseOutInElastic(Delta, 0.0, 1.0, 1.0);
  	easeInBack: Result := GetEaseInBack(Delta, 0.0, 1.0, 1.0);
  	easeOutBack: Result := GetEaseOutBack(Delta, 0.0, 1.0, 1.0);
  	easeInOutBack: Result := GetEaseInOutBack(Delta, 0.0, 1.0, 1.0);
  	easeOutInBack: Result := GetEaseOutInBack(Delta, 0.0, 1.0, 1.0);
  	easeInBounce: Result := GetEaseInBounce(Delta, 0.0, 1.0, 1.0);
  	easeOutBounce: Result := GetEaseOutBounce(Delta, 0.0, 1.0, 1.0);
  	easeInOutBounce: Result := GetEaseInOutBounce(Delta, 0.0, 1.0, 1.0);
  	easeOutInBounce: Result := GetEaseOutInBounce(Delta, 0.0, 1.0, 1.0);
  	easeWiggle: Result := GetEaseWiggle(Delta, 0.0, 1.0, 1.0);
    Else
      Result := Delta;
  End;
End;

(*{ Tween }
Constructor Tween.Create(Obj:Pointer; MyType:Integer; Data:Pointer; TargetValue:Single; CallbackTarget:TERRAObject);
Begin
  _Object := Obj;
  _DataType := MyType;
  _Data := Data;
  _StartTime :=  Application.GetTime;
  _Finished := False;
  _StartValue := Self.Read();
  _TargetValue := TargetValue;
  _CallbackTarget := CallbackTarget;

  TweenManager.Instance.Add(Self);
End;

Procedure Tween.Release;
Begin
  If (Not _Finished) Then
  Begin
    Self._StartTime := 0;
    Self.Update();
  End;
End;

Procedure Tween.Update;
Var
  T,P:Single;
  Delta, Value:Single;
  IsFinished:Boolean;
Begin
  If (_Data = Nil) Then
    _Finished := True;

  If (_Finished) Then
    Exit;

  T := Application.GetTime();
  P := (Self._StartTime + Self.Delay);
  Delta := (T - P);
  If (Delta<0.0) Then
    Exit;

  Delta := Delta / Self.Time;

  If (Delta>=1.0)  Then
  Begin
    Delta := 1.0;
    IsFinished := True;
  End Else
    IsFinished := False;

  {  * @param t     Current time (in frames or seconds).
     * @param b     Starting value.
     * @param c     Change needed in value.
     * @param d     Expected easing duration (in frames or seconds).
     * @return      The correct value.}

  Delta := GetEase(Delta, _EaseType);
  Value := Self._StartValue * (1.0 - Delta) + Self._TargetValue * Delta;

  If (_DataType = tweenByte) Then
  Begin
    If (Value<0) Then
      Value := 0
    Else
    If (Value>255) Then
      Value := 255;
  End;

  Self.Write(Value);

  If (IsFinished) And (Not _Finished) Then
  Begin
    _Finished := True;
    If Assigned(OnFinished) Then
      OnFinished(Self._CallbackTarget);
  End;
End;

Function Tween.Read:Single;
Begin
  If (_DataType = tweenFloat) Then
    Result := PSingle(_Data)^
  Else
    Result := (PByte(_Data)^) / 255.0;
End;

Procedure Tween.Write(Value:Single);
Begin
  If (_DataType = tweenFloat) Then
    PSingle(_Data)^ := Value
  Else
    PByte(_Data)^ := Trunc(Value * 255.0);
End;

{ TweenManager }
Procedure TweenManager.Init;
Begin
  _TweenCount := 0;
End;

Procedure TweenManager.Add(T: Tween);
Begin
  Inc(_TweenCount);
  If Length(_Tweens)<_TweenCount Then
    SetLength(_Tweens, _TweenCount);

  _Tweens[Pred(_TweenCount)] := T;
End;

{ TweenManager }
Class Function TweenManager.Instance:TweenManager;
Begin
  If _TweenManager_Instance = Nil Then
    _TweenManager_Instance := InitializeApplicationComponent(TweenManager, GraphicsManager);

  Result := TweenManager(_TweenManager_Instance.Instance);
End;

Procedure TweenManager.Update;
Var
  I:Integer;
Begin
  I:=0;
  While (I<_TweenCount) Do
  If (_Tweens[I]._Finished) Then
  Begin
    ReleaseObject(_Tweens[I]);
    _Tweens[I] := _Tweens[Pred(_TweenCount)];
    Dec(_TweenCount);
  End Else
  Begin
    _Tweens[I].Update;
    Inc(I);
  End;
End;

Procedure TweenManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TweenCount) Do
    ReleaseObject(_Tweens[I]);

  _TweenCount := 0;
  SetLength(_Tweens, 0);
  _TweenManager_Instance := Nil;
End;

Function TweenManager.HasTween(T: Tween): Boolean;
Var
  I:Integer;
Begin
  Result := False;
  For I:=0 To Pred(_TweenCount) Do
  If (_Tweens[I] = T) Then
  Begin
    Result := True;
    Exit;
  End;
End;

Procedure TweenManager.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TweenCount) Do
    ReleaseObject(_Tweens[I]);

  _TweenCount := 0;
End;

Procedure TweenManager.RemoveTween(Obj: Pointer);
Var
  I:Integer;
Begin
  I :=0 ;
  While (I<_TweenCount) Do
  If (_Tweens[I]._Object = Obj) Then
  Begin
    ReleaseObject(_Tweens[I]);
    _Tweens[I] := _Tweens[Pred(_TweenCount)];
    Dec(_TweenCount);
  End Else
    Inc(I);
End;
*)


End.

