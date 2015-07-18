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
 * TERRA_PhysicsManager
 * Implements the Physics manager singleton
 ***********************************************************************************************************************
}
Unit TERRA_PhysicsManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Application, TERRA_GraphicsManager,
  TERRA_Matrix4x4, TERRA_MeshFilter, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Renderable;

Const
  PhysicsSpeed = 50;

Type
  PhysicsSystem = Class;

  PhysicsBody = Class(TERRAObject)
    Protected
      _Owner:PhysicsSystem;

      _Transform:Matrix4x4;
      _Mass:Single;

      Function GetTransform():Matrix4x4; Virtual; Abstract;
      Procedure SetTransform(Const Transform:Matrix4x4); Virtual; Abstract;

    Public
      Procedure Release; Override;

      Property Transform:Matrix4x4 Read GetTransform Write SetTransform;
      Property Owner:PhysicsSystem Read _Owner;
  End;

  PhysicsSystem = Class(Renderable)
    Protected
      _WorldBox:BoundingBox;

      _Bodies:Array Of PhysicsBody;
      _BodyCount:Integer;

      Procedure AddBody(Body:PhysicsBody);
      Procedure RemoveBody(Body:PhysicsBody);

      Procedure Init; Virtual; Abstract;

      Procedure SetWorldBox(Const Box:BoundingBox); Virtual; Abstract;

      Procedure Run(Delta:Single); Virtual; Abstract;

      //Function CreateConvexMeshRigidBody(Mesh:MeshFilter; Const Transform:Matrix4x4; Mass:Single):PhysicsBody; Virtual; Abstract;
      Function CreateBoxRigidBody(Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody; Virtual; Abstract;
      Function CreateSphereRigidBody(Radius:Single; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody; Virtual; Abstract;

      Procedure BeginTree(); Virtual; Abstract;
      Procedure AddMeshToTree(Mesh:MeshFilter; Const Transform:Matrix4x4); Virtual; Abstract;
      Function FinishTree():PhysicsBody; Virtual; Abstract;

    Public
      Constructor Create(Const WorldBox:BoundingBox);

      Procedure Release; Override;

      Property WorldBox:BoundingBox Read _WorldBox Write SetWorldBox;
  End;

  PhysicsManager = Class(ApplicationComponent)
  private
    procedure SetSystem(const Value: PhysicsSystem);
    Protected
      _System:PhysicsSystem;

      _AccTimeSlice:Single;
      _TimeLastFrame:Cardinal;

    Public
      Procedure Update; Override;
      Procedure Init; Override;

      Class Function Instance:PhysicsManager;

      Procedure Release; Override;

//      Function CreateConvexMeshRigidBody(Mesh:MeshFilter; Const Transform:Matrix4x4; Mass:Single):PhysicsBody;
      Function CreateBoxRigidBody(Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody;
      Function CreateSphereRigidBody(Radius:Single; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody;

      Procedure BeginTree();
      Procedure AddMeshToTree(Mesh:MeshFilter; Const Transform:Matrix4x4);
      Function FinishTree():PhysicsBody;

      Property System:PhysicsSystem Read _System Write SetSystem;
  End;

Implementation
Uses TERRA_OS;

Var
  _PhysicsManager_Instance:ApplicationObject = Nil;

{ PhysicsManager }
Class Function PhysicsManager.Instance: PhysicsManager;
Begin
  If _PhysicsManager_Instance = Nil Then
    _PhysicsManager_Instance := InitializeApplicationComponent(PhysicsManager, Nil);

  Result := PhysicsManager(_PhysicsManager_Instance.Instance);
End;

Procedure PhysicsManager.Init;
Begin
  _System := Nil;
  _TimeLastFrame := Application.GetTime();
End;

Procedure PhysicsManager.Release;
Begin
  ReleaseObject(_System);
End;

Procedure PhysicsManager.Update;
Var
  T:Cardinal;
  Delta:Single;
  I:Integer;
Begin
  If _System = Nil Then
    Exit;

  T := Application.Instance.GetElapsedTime();
  _AccTimeSlice  := _AccTimeSlice + (T - _TimeLastFrame);
  _TimeLastFrame := T;

  // Correct timing is crucial for physics calculations if they should run the same
  // speed, no matter what FPS. So we use a method called "accumulative timeslicing"
  // which will give us the same results across all framerates
  //NewtonBodyAddImpulse(_Stage.CurrentBall.Body, @VectorZero, @VectorZero);

  Delta := (12.0 / PhysicsSpeed);
  While _AccTimeSlice > 12 do
  Begin
    {For I:=0 To Pred(_ObjectCount) Do
    If (_Objects[I] Is StagePlatform) Then
      StagePlatform(_Objects[I]).UpdateElevator(Delta);}

    _System.Run(Delta);
    _AccTimeSlice := _AccTimeSlice - 12;
  End;

End;

Function PhysicsManager.CreateBoxRigidBody(Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody;
Begin
  Result := Nil;

  If Assigned(_System) Then
  Begin
    Result := _System.CreateBoxRigidBody(Size, Position, Rotation, Mass);
    System.AddBody(Result);
  End;
End;

Function PhysicsManager.CreateSphereRigidBody(Radius: Single; Const Position, Rotation:Vector3D; Mass: Single): PhysicsBody;
Begin
  Result := Nil;

  If Assigned(_System) Then
  Begin
    Result := _System.CreateSphereRigidBody(Radius, Position, Rotation, Mass);
    System.AddBody(Result);
  End;
End;

Procedure PhysicsManager.BeginTree;
Begin
  If Assigned(_System) Then
    _System.BeginTree();
End;

Procedure PhysicsManager.AddMeshToTree(Mesh: MeshFilter; Const Transform:Matrix4x4);
Begin
  If Assigned(_System) Then
    _System.AddMeshToTree(Mesh, Transform);
End;

Function PhysicsManager.FinishTree: PhysicsBody;
Begin
  If Assigned(_System) Then
    Result := _System.FinishTree()
  Else
    Result := Nil;
End;

Procedure PhysicsManager.SetSystem(const Value: PhysicsSystem);
Begin
  If (Value = _System) Then
    Exit;

  ReleaseObject(_System);

  _System := Value;
End;

{ PhysicsSystem }
Constructor PhysicsSystem.Create(const WorldBox: BoundingBox);
Begin
  Self.Init();
  Self.SetWorldBox(WorldBox);
End;

Procedure PhysicsSystem.AddBody(Body: PhysicsBody);
Begin
  If Body = Nil Then
    Exit;

  Body._Owner := Self;

  Inc(_BodyCount);
  SetLength(_Bodies, _BodyCount);
  _Bodies[Pred(_BodyCount)] := Body;
End;

Procedure PhysicsSystem.RemoveBody(Body: PhysicsBody);
Var
  I:Integer;
Begin
  I := 0;
  While I<_BodyCount Do
  If (_Bodies[I] = Body) Then
  Begin
    _Bodies[I] := _Bodies[Pred(_BodyCount)];
    Dec(_BodyCount);
  End Else
    Inc(I);
End;

Procedure PhysicsSystem.Release;
Var
  I:Integer;
Begin
  Inherited;

  _BodyCount := 0;
End;

{ PhysicsBody }
Procedure PhysicsBody.Release;
Begin
  If Assigned(_Owner) Then
    _Owner.RemoveBody(Self);
End;

End.