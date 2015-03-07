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
 * TERRA_VerletParticle
 * Implements verlet particle system
 ***********************************************************************************************************************
}
Unit TERRA_VerletParticle;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_Vector2D, TERRA_Quaternion, TERRA_BoundingBox, TERRA_Particles;

// Some physics constants
Const
{$IFDEF FIXEDPOINT}
  DAMPING = 00;         // how much to damp the cloth simulation each frame
  TIME_STEPSIZE2 = 00;  // how large time step each particle takes each frame
{$ELSE}
  DAMPING = 0.01;         // how much to damp the cloth simulation each frame
  TIME_STEPSIZE2 = 0.25;  // how large time step each particle takes each frame
{$ENDIF}

  CONSTRAINT_ITERATIONS = 20; // how many iterations of constraint satisfaction each frame (more is rigid, less is soft)

Type
  PVerletParticle = ^VerletParticle;
  VerletParticle = Object
    Protected
	    _Fixed:Boolean; // can the particle move or not ? used to pin parts of the cloth
      _Mass:Single; // the mass of the particle (is always 1 in this example)
	    _Position:Vector3D; // the current position of the particle in 3D space
	    _OldPos:Vector3D; // the position of the particle in the previous time step, used as part of the verlet numerical integration scheme
	    _Acceleration:Vector3D; // a vector representing the current acceleration of the particle
      _CurrentPosition:Vector3D;

    Public
	    Constructor Create(Pos:Vector3D);

	   //Given the equation "force = mass * acceleration" the next position is found through verlet integration
	    Procedure TimeStep;

      Procedure AddForce(F:Vector3D);

	    Procedure OffsetPos(v:Vector3D);

      Property Position:Vector3D Read _CurrentPosition;
      Property Fixed:Boolean Read _Fixed Write _Fixed;
    End;

  VerletConstraint = Object
    Protected
	    _RestDistance:Single; // the length between particle p1 and p2 in rest configuration
	    P1,P2:PVerletParticle; // the two particles that are connected through this constraint

    Public

      Constructor Create(A,B:PVerletParticle);

	// This is one of the important methods, where a single constraint between two particles p1 and p2 is solved the method is called by Cloth.time_step() many times per frame*/
	    Procedure SatisfyConstraint;
    End;

  VerletSystem = Class(TERRAObject)
    Protected
      _Particles:Array Of VerletParticle;
      _ParticleCount:Integer;

      _Constraints:Array Of VerletConstraint;
      _ConstraintCount:Integer;

      _Box:BoundingBox;

      _Buffer:PParticleVertex;
      _OutputQuads:Boolean;

    Public
      Constructor Create(Buffer:PParticleVertex; ParticleCount:Integer; OutputQuads:Boolean);

	    Procedure AddConstraint(P1,P2:Integer);

	    // used to add gravity (or any other arbitrary vector) to all particles
	    Procedure AddForce(Direction:Vector3D);

      Function GetParticle(Index:Integer):PVerletParticle;

  	{ this is an important methods where the time is progressed one time step for the entire cloth.
	  This includes calling satisfyConstraint() for every constraint, and calling timeStep() for all particles}
      Procedure Update;

    	{ used to detect and resolve the collision of the cloth with the ball.
	    This is based on a very simples scheme where the position of each particle is simply compared to the sphere and corrected.
	    This also means that the sphere can "slip through" if the ball is small enough compared to the distance in the grid bewteen particles
	    }
	    Procedure BallCollision(center:Vector3D; Radius:Single);

      Property BoundingBox:TERRA_BoundingBox.BoundingBox Read _Box;
      Property ParticleCount:Integer Read _ParticleCount;
  End;


Implementation

Constructor VerletParticle.Create(Pos:Vector3D);
Begin
  _Position := Pos;
  _CurrentPosition := Pos;
  _OldPos := Pos;
  _Acceleration := VectorZero;
  _Mass := 1.0;
  _Fixed := False;
End;

Procedure VerletParticle.TimeStep;
Var
  Temp:Vector3D;
Begin
  If (_Fixed) Then
  Begin
    _CurrentPosition := _Position;
    Exit;
  End;

  Temp := _Position;
  _Position.Add(VectorScale(VectorSubtract(_Position, _OldPos), (1.0 - DAMPING)));
  _Position.Add(VectorScale(_Acceleration, TIME_STEPSIZE2));
  _OldPos := Temp;
  _Acceleration := VectorZero; // acceleration is reset since it HAS been translated into a change in position (and implicitely into velocity)
  _CurrentPosition := _Position;
End;

Procedure VerletParticle.OffsetPos(v:Vector3D);
Begin
  If Not _Fixed Then
    _Position.Add(V);
End;

Procedure VerletParticle.AddForce(F:Vector3D);
Begin
  F.Scale(1.0 / _Mass);
  _Acceleration.Add(F);
End;

Constructor VerletConstraint.Create(A,B:PVerletParticle);
Var
  P:Vector3D;
Begin
  Self.P1 := A;
  Self.P2 := B;
  P := VectorSubtract(P1._Position, P2._Position);
  _RestDistance := P.Length;
End;

Procedure VerletConstraint.SatisfyConstraint;
Var
  current_distance:Single;
  p1_to_p2:Vector3D;
  correctionVector, correctionVectorHalf:Vector3D;
Begin
  p1_to_p2 := VectorSubtract(p2._Position, p1._Position); // vector from p1 to p2
  current_distance := p1_to_p2.Length; // current distance between p1 and p2
  correctionVector := VectorScale(p1_to_p2, (1.0 - (_RestDistance / current_distance))); // The offset vector that could moves p1 into a distance of rest_distance to p2
	correctionVectorHalf := VectorScale(correctionVector, 0.5); // Lets make it half that length, so that we can move BOTH p1 and p2.
  p1.offsetPos(correctionVectorHalf); // correctionVectorHalf is pointing from p1 to p2, so the length should move p1 half the length needed to satisfy the constraint.
  correctionVectorHalf := VectorScale(correctionVectorHalf, -1.0);
  p2.offsetPos(correctionVectorHalf); // we must move p2 the negative direction of correctionVectorHalf since it points from p2 to p1, and not p1 to p2.
End;

Procedure VerletParticleTimeStepCallback(P:Pointer);
Const
{$IFDEF FIXEDPOINT}
  QuadOffsets:Array[0..3] Of Vector2D = (
    (X: (Value: -(1 Shl FixBase)); Y:(Value: 1 Shl FixBase)),
    (X: (Value: 1 Shl FixBase); Y:(Value: 1 Shl FixBase)),
    (X: (Value: 1 Shl FixBase); Y:(Value: -(1 Shl FixBase))),
    (X: (Value: -(1 Shl FixBase)); Y:(Value: -(1 Shl FixBase)))
	);
{$ELSE}
  QuadOffsets:Array[0..3] Of Vector2D = (
    (X:-1.0; Y:-1.0),
    (X: 1.0; Y:-1.0),
    (X: 1.0; Y:1.0),
    (X:-1.0; Y:1.0));
{$ENDIF}
Var
  I,J:Integer;
  S:VerletSystem;
  Dest:PParticleVertex;
Begin
  S := VerletSystem(P);
  With S Do
  Begin
    For I := 0 To Pred(CONSTRAINT_ITERATIONS) Do // iterate over all constraints several times
      For J :=0 To Pred(_ConstraintCount) Do
        _Constraints[J].SatisfyConstraint; // satisfy constraint.

    _Box.Reset;
    Dest := _Buffer;
    If (_OutputQuads) Then
    Begin
      For I := 0 To Pred(S._ParticleCount) Do
      Begin
        _Particles[I].TimeStep; // calculate the position of each particle at the next time step.
        _Box.Add(_Particles[I].Position);

        For J:=0 To 3 Do
        Begin
          Dest.Position := _Particles[I].Position;
          Dest.Normal.X := QuadOffsets[J].X;
          Dest.Normal.Y := QuadOffsets[J].Y;
          Dest.Normal.Z := 0.0;
          Inc(Dest);
        End;
      End;
    End Else
    Begin
      For I := 0 To Pred(S._ParticleCount) Do
      Begin
        _Particles[I].TimeStep; // calculate the position of each particle at the next time step.
        _Box.Add(_Particles[I].Position);

        Dest.Position := _Particles[I].Position;
        Dest.Normal := VectorZero;
        Inc(Dest);
      End;
    End;
  End;
End;

Procedure VerletSystem.Update;
Begin
  //ThreadPool.Instance.RunTask(VerletParticleTimeStepCallback, Self, 30);
  VerletParticleTimeStepCallback(Self);
End;

Procedure VerletSystem.AddConstraint(P1,P2:Integer);
Var
  I:Integer;
  A,B:PVerletParticle;
Begin
  If (P1<0) Or (P2<0) Or (P1>=_ParticleCount) Or (P2>=_ParticleCount) Then
    Exit;

  A := @(_Particles[P1]);
  B := @(_Particles[P2]);
{  For I:=0 To Pred(_ConstraintCount) Do
  If ((_Constraints[I].P1=A) Or (_Constraints[I].P1=B))
  And ((_Constraints[I].P2=A) Or (_Constraints[I].P2=B)) Then
    Exit;
 }
  Inc(_ConstraintCount);
  SetLength(_Constraints, _ConstraintCount);
  _Constraints[Pred(_ConstraintCount)].Create(A, B);
End;

Procedure VerletSystem.AddForce(Direction:Vector3D);
Var
  I:Integer;
  F:Vector3D;
Begin
  For I:=0 To Pred(_ParticleCount) Do
  Begin
    F := VectorScale(Direction, 1.0 / _Particles[I]._Mass);
    _Particles[I]._Acceleration.Add(F);
  End;
End;

Function VerletSystem.GetParticle(Index:Integer):PVerletParticle;
Begin
  If (Index<0) Or (Index>=_ParticleCount) Then
    Result := Nil
  Else
    Result := @(_Particles[Index]);
End;

Constructor VerletSystem.Create(Buffer:PParticleVertex; ParticleCount:Integer; OutputQuads:Boolean);
Var
  I:Integer;
  Src:PParticleVertex;
Begin
  _OutputQuads := OutputQuads;
  _ParticleCount := ParticleCount;
  SetLength(_Particles, _ParticleCount);
  _Buffer := Buffer;
  Src := Buffer;
  For I:=0 To Pred(ParticleCount) Do
  Begin
    _Particles[I].Create(Src.Position);
    If (OutputQuads) Then
      Inc(Src,4)
    Else
      Inc(Src);
  End;
End;

Procedure VerletSystem.BallCollision(center:Vector3D; Radius:Single);
Var
  I:Integer;
  V:Vector3D;
  N:Single;
Begin
  For I :=0 To Pred(_ParticleCount) Do
  Begin
    v := VectorSubtract(_Particles[I].Position, Center);
    N := V.Length;
    If (N < Radius) Then // if the particle is inside the ball
    Begin
      V.Normalize;
      _Particles[I].OffsetPos(VectorScale(V, (Radius-N))); // project the particle to the surface of the ball
    End;
  End;
End;

End.
