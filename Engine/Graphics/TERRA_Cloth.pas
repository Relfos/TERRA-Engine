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
 * TERRA_Cloth
 * Implements cloth mesh
 ***********************************************************************************************************************
}

Unit TERRA_Cloth;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color,
  TERRA_Resource, TERRA_Texture, TERRA_Mesh, TERRA_MeshFilter, TERRA_VertexFormat;

Const
  MinimumDelta = 10;
  SimScale = 1;

  DefaultGravity:Vector3d = (X:0.0; Y:-0.98 * SimScale; Z:0.0);


  // size of the cloth mesh
  ClothScale = 20.0; //10

//Values given to each spring
  StretchStiffness = 2.5 * ClothScale;
  BendStiffness = 1.0 * ClothScale;


//Values given to each ball
  mass = 0.01 * SimScale;

//Damping factor. Velocities are multiplied by this
  dampFactor=0.9;

//Grid complexity. This is the number of Particles across and down in the model
  gridSize = 13* SimScale; //13;
  
Type
  ClothSystem = Class;

  ClothSpring = Object
    Protected
    	//Indices of the Particles at either end of the spring
  	  P1, P2:Integer;

      _NaturalLength:Double;
    	_InverseLength:Double;

      _Stiffness:Double;
      
    Procedure Init(PID1, PID2: Integer; Len, Stiffness:Double);
  End;

  ClothSpringLink = Record
    SpringIndex:Integer;
    ParticleIndex:Integer;
  End;

  ClothParticle = Object
    Protected
	    CurrentPosition:Vector3D;
    	CurrentVelocity:Vector3D;

	    NextPosition:Vector3D;
  	  NextVelocity:Vector3D;

      _Tension:Vector3D;
	    _InverseMass:Single;

  	  //Is this ball held in position?
	    _Fixed:Boolean;
  End;

  ClothCollider = Record
    Position:Vector3D;
    Radius:Single;
  End;

  ClothSystem = Class(TERRAObject)
    Protected
      _SpringCount:Integer;
      _Springs:Array Of ClothSpring;
      _ParticleCount:Integer;
      _Particles:Array Of ClothParticle;

      _Mesh:TERRAMesh;
      _Group:MeshGroup;
      _VertexFormat:VertexFormat;

      _LastTime:Cardinal;
      _timeSinceLastUpdate:Cardinal;

      _Gravity:Vector3D;

      _Colliders:Array Of ClothCollider;
      _ColliderCount:Integer;

      Procedure InitMesh(Texture:TERRATexture);
      Procedure UpdateMesh();

    Public
      Constructor Create(Texture:TERRATexture);
      Procedure Release(); Override;
      Procedure Reset();
      Procedure Simulate();

      Procedure UnpinParticle(Index:Integer);

      Procedure SetCollider(Index:Integer; Const Pos:Vector3D; Const Radius:Single);

      Property Mesh:TERRAMesh Read _Mesh;
  End;

Implementation
Uses TERRA_OS;

{ ClothSpring }
Procedure ClothSpring.Init(PID1, PID2: Integer; Len, Stiffness:Double);
Begin
  Self.P1 := PID1;
  Self.P2 := PID2;
  Self._NaturalLength := Len;
  Self._InverseLength := 1.0 / Len;
  Self._Stiffness := Stiffness;
End;

{ ClothSystem }
Constructor ClothSystem.Create(Texture:TERRATexture);
Var
  I:Integer;
Begin
  _lastTime := 0;
  _timeSinceLastUpdate := 0;

  _Gravity := DefaultGravity;

	//Calculate number of Particles
	_ParticleCount := gridSize*gridSize;

	//Calculate number of springs
	//There is a spring pointing right for each ball which is not on the right edge,
	//and one pointing down for each ball not on the bottom edge
	_SpringCount := (gridSize-1)*gridSize*2;

	//There is a spring pointing down & right for each ball not on bottom or right,
	//and one pointing down & left for each ball not on bottom or left
	Inc(_SpringCount, (gridSize-1)*(gridSize-1)*2);

	//There is a spring pointing right (to the next but one ball)
	//for each ball which is not on or next to the right edge,
	//and one pointing down for each ball not on or next to the bottom edge
	Inc(_SpringCount, (gridSize-2)*gridSize*2);

	//Create space for Particles & springs
	SetLength(_Particles, _ParticleCount);
	SetLength(_Springs, _SpringCount);

  InitMesh(Texture);

	//Reset cloth
	Self.Reset();
End;

Procedure ClothSystem.InitMesh(Texture:TERRATexture);
Var
  BallID:Integer;
  I, J, K:Integer;
  i0, i1, i2, i3:Integer;
  T:Triangle;
Begin
  ReleaseObject(_Mesh);
  _Mesh := TERRAMesh.Create(rtDynamic, 'cloth');

  _VertexFormat := [vertexFormatPosition, vertexFormatNormal, vertexFormatColor, vertexFormatUV0];

  _Group := _Mesh.AddGroup(_VertexFormat);
  _Group.DiffuseMap := Texture;
  _Group.Flags := _Group.Flags Or meshGroupDoubleSided;

  _Group.TriangleCount := (gridSize * gridSize) * 2;
  _Group.VertexCount := (gridSize * gridSize);

  K := 0;
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize-1) Do
		Begin
      i0 := J*gridSize + I;
			i1 := J*gridSize + I + 1;
			i2 := (J+1)*gridSize + I;
			i3 := (J+1)*gridSize + I + 1;

      T.Indices[0] := i2;
      T.Indices[1] := i1;
      T.Indices[2] := i0;
      _Group.SetTriangle(T, K); Inc(K);

      T.Indices[0] := i2;
      T.Indices[1] := i3;
      T.Indices[2] := i1;
      _Group.SetTriangle(T, K); Inc(K);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      _Group.Vertices.SetVector2D(BallID, vertexUV0, VectorCreate2D(I/GridSize, J/GridSize));
      _Group.Vertices.SetColor(BallID, vertexColor, ColorWhite);
    End;
End;

Procedure ClothSystem.Reset();
Var
  I, J, K:Integer;
  BallID, X,Y:Integer;
  currentSpring:Integer;
  naturalLength:Single;

  U,V:Single;
Begin
	//Initialise the Particles in an evenly spaced grid in the x-z plane
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      U := (I/Pred(gridSize)) - 0.5;
      V := (J/Pred(gridSize)) - 0.5;

      BallID := J*gridSize+I;
			_Particles[BallID].CurrentPosition := VectorCreate(ClothScale * U, 8.5, ClothScale * V);
			_Particles[BallID].CurrentVelocity := VectorZero;

			_Particles[BallID]._InverseMass := 1 / Mass;
			_Particles[BallID]._Fixed := False;

      _Particles[BallID]._Tension := VectorZero;
		End;

  NaturalLength := _Particles[0].CurrentPosition.Distance(_Particles[1].CurrentPosition);

	//Fix the top left & top right Particles in place
	_Particles[0]._Fixed := true;
	_Particles[gridSize-1]._Fixed :=true;

	//Fix the bottom left & bottom right Particles
	_Particles[gridSize*(gridSize-1)]._Fixed := true;
	_Particles[gridSize*gridSize-1]._Fixed := true;

  //_Particles[(gridSize Shr 1) * GRidsize +  (gridSize Shr 1)]._Fixed :=true;

	//Initialise the springs
	currentSpring := 0;

	//The first (gridSize-1)*gridSize springs go from one ball to the next,
	//excluding those on the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+1, naturalLength, StretchStiffness);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*gridSize springs go from one ball to the one below,
	//excluding those on the bottom edge
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize) Do
    Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I, naturalLength, StretchStiffness);
			Inc(currentSpring);
    End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and right
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I+1, naturalLength*sqrt(2.0), BendStiffness);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and left
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I-1, naturalLength*sqrt(2.0), BendStiffness);
			Inc(currentSpring);
		End;

	//The first (gridSize-2)*gridSize springs go from one ball to the next but one,
	//excluding those on or next to the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-2) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+2, naturalLength*2, BendStiffness);
			Inc(currentSpring);
		End;


	//The next (gridSize-2)*gridSize springs go from one ball to the next but one below,
	//excluding those on or next to the bottom edge
  For J:=0 To Pred(gridSize-2) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+2)*gridSize+I, naturalLength*2, BendStiffness);
			Inc(currentSpring);
		End;


  UpdateMesh();
End;

Procedure ClothSystem.UpdateMesh();
Var
  I, J, BallID, Index, X, Y:Integer;
  normal:Vector3D;

  Vertices:VertexData;
Begin
  _Group.CalculateTriangleNormals();

  Vertices := _Group.LockVertices();

  //Calculate the normals on the current Particles
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
    Begin
      BallID := J*gridSize+I;
      Normal := VectorZero;
      For Y:=0 To 1 Do
        For X:=0 To 1 Do
        If (X+I<GridSize) And (Y+J<GridSize) Then
        Begin
          Index := (J+Y)* gridSize + (I + X) * 2;
          Normal.Add(_Group.GetTriangleNormal(Index));

          Inc(Index);
          Normal.Add(_Group.GetTriangleNormal(Index));
        End;

      //Normal.Normalize();
      Normal.Scale(0.25);
      Vertices.SetVector3D(BallID, vertexNormal, Normal);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      Vertices.SetVector3D(BallID, vertexPosition, _Particles[BallID].CurrentPosition);
    End;


  _Group.UnlockVertices();

  _Mesh.UpdateBoundingBox();
End;

Procedure ClothSystem.Simulate;
Var
  currentTime, timePassed:Cardinal;
  updateMade:Boolean;
  timePassedInSeconds:Single;
  I, J, K, N:Integer;

  springLength, Extension, Tension:Double;
  force, acceleration, tensionDirection:VECTOR3D;
  P:Vector3D;
Begin
	//set currentTime and timePassed
  If _lastTime =0 Then
  Begin
    _lastTime := Application.Instance.GetTime();
    Exit;
  End;

	currentTime := Application.Instance.GetTime();
	timePassed := currentTime - _lastTime;
	_lastTime := currentTime;

	//Update the physics in intervals of 10ms to prevent problems
	//with different frame rates causing different damping
	Inc(_timeSinceLastUpdate, timePassed);

	updateMade := false;	//did we update the positions etc this time?

	while (_timeSinceLastUpdate>MinimumDelta) Do
	Begin
		Dec(_timeSinceLastUpdate, MinimumDelta);
		timePassedInSeconds := 1.0/ (1000/MinimumDelta);
		updateMade := True;

		//Calculate the tensions in the springs
		For I:=0 To Pred(_SpringCount) Do
		Begin
      TensionDirection :=	VectorSubtract(_Particles[_Springs[i].P1].CurrentPosition, _Particles[_Springs[i].P2].CurrentPosition);

			springLength := TensionDirection.Length();
			extension := springLength - _Springs[i]._naturalLength;

			//_Springs[i].
      Tension := _Springs[i]._Stiffness * (Extension * _Springs[i]._InverseLength);

      TensionDirection.Scale(Tension  * (1 / springLength));

      _Particles[_Springs[i].P2]._Tension.Add(tensionDirection);
      tensionDirection.Scale(-1.0);
      _Particles[_Springs[i].P1]._Tension.Add(tensionDirection);
		End;


		//Calculate the nextParticles from the currentParticles
		For I:=0 To Pred(_ParticleCount) Do
		Begin
			//If the ball is fixed, transfer the position and zero the velocity, otherwise calculate
			//the new values
			If (_Particles[i]._Fixed) Then
			Begin
				_Particles[i].NextPosition := _Particles[i].CurrentPosition;
				_Particles[i].NextVelocity := VectorZero;
        (*If MoveCloth Then
          _Particles[i].NextPosition.Add(VectorCreate(0, 2 * timePassedInSeconds, 5 * timePassedInSeconds));*)
        Continue;
			End;

      //Calculate the force on this ball
      Force := VectorAdd(_Gravity, _Particles[I]._Tension);

			//Calculate the acceleration
			Acceleration := VectorScale(force, _Particles[i]._InverseMass);

			//Update velocity
			_Particles[i].NextVelocity := VectorAdd(_Particles[i].CurrentVelocity, VectorScale(acceleration, timePassedInSeconds));

			//Damp the velocity
			_Particles[i].NextVelocity.Scale(dampFactor);

			//Calculate new position
       //Forcse := VectorAdd(VectorScale(Particles[i].NextVelocity, 0.5), VectorScale(Particles[i].CurrentVelocity, 0.5));
       Force := _Particles[i].NextVelocity;

       Force.Scale(timePassedInSeconds);
			_Particles[i].NextPosition := VectorAdd(_Particles[i].CurrentPosition, Force);

			//Check against floor
(*			If(_Particles[i].NextPosition.y <= -sphereRadius * 0.92) Then
      Begin
			  _Particles[i].NextPosition.y := -sphereRadius * 0.92;
        _Particles[i].NextVelocity.y := 0;
         Continue;
      End;*)

			//Check against sphere (at origin)
      For J:=0 To Pred(_ColliderCount) Do
      Begin
        P := VectorSubtract(_Particles[i].NextPosition, _Colliders[J].Position);
  			If (P.LengthSquared < Sqr(_Colliders[J].Radius*1.08)) Then
        Begin
		  	  P.Normalize();
          P.Scale(_Colliders[J].Radius * 1.08);
          _Particles[i].NextPosition := VectorAdd(P, _Colliders[J].Position);
          _Particles[i].NextVelocity := VectorZero;
          Break;
        End;
      End;
		End;

		//Swap the currentParticles and newParticles pointers
		For I:=0 To Pred(_ParticleCount) Do
    Begin
      _Particles[i].CurrentPosition := _Particles[i].NextPosition;
			_Particles[i].CurrentVelocity := _Particles[i].NextVelocity;
      _Particles[i]._Tension := VectorZero;
    End;
	End;

	//Calculate the normals if we have updated the positions
	If(updateMade) Then
    Self.UpdateMesh();
End;

Procedure ClothSystem.Release;
Begin
  ReleaseObject(_Mesh);
End;

Procedure ClothSystem.SetCollider(Index: Integer; const Pos: Vector3D; const Radius: Single);
Begin
  If (Index>=Pred(_ColliderCount)) Then
  Begin
    _ColliderCount := Succ(Index);
    SetLength(_Colliders, _ColliderCount);
  End;

  _Colliders[Index].Position := Pos;
  _Colliders[Index].Radius := Radius;
End;

Procedure ClothSystem.UnpinParticle(Index: Integer);
Begin
  _Particles[Index]._Fixed := False;
End;

End.
