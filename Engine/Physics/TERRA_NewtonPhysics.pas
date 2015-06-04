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
 * TERRA_NewtonPhysics
 * Implements a physics frontend for Newton physics engine
 ***********************************************************************************************************************
}

Unit TERRA_NewtonPhysics;

{$I terra.inc}
Interface

Uses NewtonImport, TERRA_Utils, TERRA_MeshFilter, TERRA_Color, TERRA_BoundingBox, TERRA_Vector3D, TERRA_Matrix4x4,
  TERRA_GraphicsManager, TERRA_PhysicsManager;

Type
  NewtonPhysicsSystem = Class;

  NewtonPhysicsBody = Class(PhysicsBody)
    Protected
      _Body:PNewtonBody;

      Function GetTransform():Matrix4x4; Override;
      Procedure SetTransform(Const Transform:Matrix4x4); Override;

      Procedure InitBody(World:PNewtonWorld; Collision:PNewtonCollision; Const InertiaVector, Position, Rotation:Vector3D);

    Public
      Procedure Release; Override;
  End;

  NewtonTreeRigidBody = Class(NewtonPhysicsBody)
    Protected
    Public
      Constructor Create(World:PNewtonWorld; Collision:PNewtonCollision);
  End;


  NewtonBoxRigidBody = Class(NewtonPhysicsBody)
    Protected
    Public
      Constructor Create(World:PNewtonWorld; Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single);
  End;

  NewtonSphereRigidBody = Class(NewtonPhysicsBody)
    Protected
    Public
      Constructor Create(World:PNewtonWorld; Radius:Single; Const Position, Rotation:Vector3D; Mass:Single);
  End;

   NewtonPhysicsSystem = Class(PhysicsSystem)
    Protected
      _World:PNewtonWorld;

      _TreeCollision:PNewtonCollision;

      Procedure Init; Override;

      Procedure Run(Delta:Single); Override;

      Procedure SetWorldBox(Const Box:BoundingBox); Override;

//      Function CreateConvexMeshRigidBody(Mesh:MeshFilter):PhysicsBody; Override;
      Function CreateBoxRigidBody(Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody; Override;
      Function CreateSphereRigidBody(Radius:Single; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody; Override;

      Procedure BeginTree(); Override;
      Procedure AddMeshToTree(Mesh:MeshFilter; Const Transform:Matrix4x4); Override;
      Function FinishTree():PhysicsBody; Override;

      // renderable
      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render(TranslucentPass:Boolean); Override;

    Public
      Procedure Release; Override;
  End;


Implementation
Uses TERRA_Math;

Procedure Debug_ShowGeometryCollision(Const Body:PNewtonBody; VertexCount:Integer; Const FaceArray:PSingle; FaceId:Integer); CDecl;
Var
  i:Integer;
  v0,v1: array[0..2] of Single;
  vA:Array of Single;
Begin
  If VertexCount = 0 then
     exit;

{  SetLength(vA, VertexCount*3);
  Move(FaceArray^, vA[0], VertexCount*3*SizeOf(Single));
  v0[0] := vA[(VertexCount-1)*3];
  v0[1] := vA[(VertexCount-1)*3+1];
  v0[2] := vA[(VertexCount-1)*3+2];
  for i := 0 to VertexCount-1 do
  begin
   v1[0] := vA[i*3];
   v1[1] := vA[i*3+1];
   v1[2] := vA[i*3+2];
   glVertex3f(v0[0], v0[1], v0[2]);
   glVertex3f(v1[0], v1[1], v1[2]);
   v0 := v1;
  end;}
end;

Procedure Debug_ShowBodyCollision(Const Body:PNewtonBody); CDecl;
Var
  M:Matrix4x4;
Begin
  NewtonBodyGetMatrix(Body, @M);
  NewtonCollisionForEachPolygonDo(NewtonBodyGetCollision(Body), @M, @Debug_ShowGeometryCollision, Nil);
End;

Procedure ForceAndTorqueCallback(const Body : PNewtonBody; TimeStep:Single; ThreadIndex : int); cdecl;
Var
  UserData:Pointer;
  B:NewtonPhysicsBody;
  Force:Vector3D;
  D:Single;
Begin
  // User data was set to the class of the character controll, so we do a typecast
  UserData := NewtonBodyGetUserData(Body);
  If UserData = nil then
    exit;

  B := NewtonPhysicsBody(UserData);

  {If B._Position.Y<0 Then
  Begin
    Force := VectorCreate(0, -1, 0);
    D := B._Mass/NewtonConvexCollisionCalculateVolume(NewtonBodyGetCollision(B._Body));
    D := D * 10;
    NewtonBodyAddBuoyancyForce(B._Body, D, 0.5, 0.5, @Force, GetBuoyancyPlane, nil);
  End Else}
  Begin
    Force := VectorCreate(0, -9.8 * B._Mass, 0);
    NewtonBodyAddForce(Body, @Force);

{    If B._Direction.Length>0 Then
      NewtonBodyAddForce(Body, @B._Direction);

    If B._Jump.Length>0 Then
    Begin
      NewtonBodyAddForce(Body, @B._Jump);
      B._Jump := VectorZero;
    End;}
  End;

{  If B.ExternalForce.Length>0 Then
  Begin
    NewtonBodyAddForce(Body, @B.ExternalForce);
    B.ExternalForce := VectorZero;
  End;}
End;

{ NewtonPhysicsBody }
Function NewtonPhysicsBody.GetTransform: Matrix4x4;
Begin
  NewtonBodyGetMatrix(_Body, @_Transform);
  Result := _Transform;
End;

Procedure NewtonPhysicsBody.SetTransform(const Transform: Matrix4x4);
Begin
  NewtonBodySetMatrix(_Body, @_Transform);
  NewtonBodySetVelocity(_Body, @VectorZero);
  NewtonBodySetForce(_Body, @VectorZero);
End;

Procedure NewtonPhysicsBody.InitBody(World:PNewtonWorld; Collision:PNewtonCollision; Const InertiaVector,Position, Rotation:Vector3D);
Var
  M:Matrix4x4;
Begin
{  M := Matrix4x4Rotation(VectorCreate(0, 180*RAD, 0));
  M := Matrix4x4Multiply4x4(Transform, M);}
  M := Matrix4x4Transform(Position, Rotation, VectorOne);

  // Create the rigid body
  _Body := NewtonCreateBody(World, Collision, @M);

  // Set the bodies mass and moment of inertia
  NewtonBodySetMassMatrix(_Body, _Mass, InertiaVector.x, InertiaVector.y, InertiaVector.z);

  // Finally set the callback in which the forces on this body will be applied
  NewtonBodySetForceAndTorqueCallBack(_Body, ForceAndTorqueCallBack);
  NewtonBodySetUserData(_Body, Self);

  //NewtonBodySetMaterialGroupID(_Body, _MatPlayer);

  //P := VectorCreate(1, 0, 0);
  //NewtonConstraintCreateUpVector(_World, @P, _Body);

  // Remove the collider, we don't need it anymore
  NewtonReleaseCollision(World, Collision);
End;

Procedure NewtonPhysicsBody.Release;
Begin
  Inherited;

  If (Assigned(_Owner)) And (Assigned(_Body)) Then
    NewtonDestroyBody(NewtonPhysicsSystem(_Owner)._World, _Body);
End;

{ NewtonTreeRigidBody }
Constructor NewtonTreeRigidBody.Create(World: PNewtonWorld; Collision: PNewtonCollision);
Begin
  _Mass := 0.0;
  _Body := NewtonCreateBody(World, Collision, @Matrix4x4Identity);
  //NewtonBodySetMaterialGroupID(_Body, _MatStage);
End;

{ NewtonBoxRigidBody }
Constructor NewtonBoxRigidBody.Create(World:PNewtonWorld; Const Size:Vector3D; Const Position, Rotation:Vector3D;  Mass:Single);
Var
  Collision:PNewtonCollision;
  P:Vector3D;
  Inv:Single;
  M:Matrix4x4;
Begin
  _Owner := Owner;
  _Mass := Mass;

  // Set the bodies mass and moment of inertia
  Inv := 1.0 / 12.0;
  P.x := _Mass * (Sqr(Size.y) + Sqr(Size.z)) * Inv;
  P.y := _Mass * (Sqr(Size.x) + Sqr(Size.z)) * Inv;
  P.z := _Mass * (Sqr(Size.x) + Sqr(Size.y)) * Inv;

  M := Matrix4x4Transform(VectorCreate(0, Size.Y * 0.5, 0), VectorCreate(0, 0, 90*RAD), VectorOne);

  Collision := NewtonCreateBox(World, Size.X, Size.Y, Size.Z, 0, @M);

  Self.InitBody(World, Collision, P, Position, Rotation);
End;

{ NewtonSphereRigidBody }

Constructor NewtonSphereRigidBody.Create(World:PNewtonWorld; Radius: Single; Const Position, Rotation:Vector3D; Mass: Single);
Var
  Collision:PNewtonCollision;
  P:Vector3D;
  Inertia:Single;
Begin
  _Owner := Owner;
  _Mass := Mass;

  Inertia := (2 * _Mass * Sqr(Radius)) /5.0;
  P := VectorConstant(Inertia);

  Collision := NewtonCreateSphere(World, Radius, Radius, Radius, 0, @Matrix4x4Identity);

  Self.InitBody(World, Collision, P, Position, Rotation);
End;

{ NewtonPhysicsSystem }
Procedure NewtonPhysicsSystem.Init;
Begin
  _World := NewtonCreate(Nil, Nil);
End;

Procedure NewtonPhysicsSystem.Release;
Begin
  Inherited;
  
  If Assigned(_World) Then
  Begin
    NewtonDestroy(_World);
    _World := Nil;
  End;
End;

Procedure NewtonPhysicsSystem.Run(Delta:Single);
Begin
  NewtonUpdate(_World, Delta);
End;

Procedure NewtonPhysicsSystem.SetWorldBox(const Box: BoundingBox);
Begin
  If Assigned(_World) Then
  Begin
    _WorldBox := Box;
    NewtonSetWorldSize(_World, @Box.StartVertex.X, @Box.EndVertex.X);
  End;
End;

Function NewtonPhysicsSystem.CreateBoxRigidBody(Const Size:Vector3D; Const Position, Rotation:Vector3D; Mass:Single):PhysicsBody;
Begin
  Result := NewtonBoxRigidBody.Create(_World, Size, Position, Rotation, Mass);
End;

Function NewtonPhysicsSystem.CreateSphereRigidBody(Radius:Single; Const Position, Rotation:Vector3D; Mass:Single): PhysicsBody;
Begin
  Result := NewtonSphereRigidBody.Create(_World, Radius, Position, Rotation, Mass);
End;

Procedure NewtonPhysicsSystem.BeginTree;
Begin
  // now create and empty collision tree
	_TreeCollision := NewtonCreateTreeCollision(_World, 0);

	// start adding faces to the collision tree
	NewtonTreeCollisionBeginBuild(_TreeCollision);
End;

Procedure NewtonPhysicsSystem.AddMeshToTree(Mesh: MeshFilter; Const Transform:Matrix4x4);
Var
  I,J,K:Integer;
  GroupCount, TriangleCount:Integer;
  T:Triangle;
  P:Vector3D;
  PP:Array[0..2] Of Vector3D;
Begin
  If _TreeCollision = Nil Then
    Exit;

  If Mesh = Nil Then
    Exit;

  GroupCount := Mesh.GetGroupCount();

  For I:=0 To Pred(GroupCount) Do
  Begin
    TriangleCount := Mesh.GetTriangleCount(I);
    For J:=0 To Pred(TriangleCount) Do
    Begin
      T := Mesh.GetTriangle(I, J);
      For K:=0 To 2 Do
      Begin
        P := Mesh.GetVertexPosition(I, T.Indices[K]);
        PP[K] := Transform.Transform(P);
      End;

      NewtonTreeCollisionAddFace(_TreeCollision, 3, @(PP[0].X), 12, J);
    End;
  End;
End;

Function NewtonPhysicsSystem.FinishTree: PhysicsBody;
Begin
  If (_TreeCollision = Nil) Then
  Begin
    Result := Nil;
    Exit;
  End;

	// end adding faces to the collision tree, also optimize the mesh for best performance
	NewtonTreeCollisionEndBuild(_TreeCollision, 1);

  Result := NewtonTreeRigidBody.Create(_World, _TreeCollision);
  NewtonReleaseCollision(_World, _TreeCollision);
End;

Function NewtonPhysicsSystem.GetBoundingBox: BoundingBox;
Begin
  Result := Self._WorldBox;
End;

Procedure NewtonPhysicsSystem.Render(TranslucentPass:Boolean);
Begin
  If TranslucentPass Then
    Exit;

  GraphicsManager.Instance.EnableColorShader(ColorWhite, Matrix4x4Identity);
{  glLineWidth(3.0);

  glBegin(GL_LINES);
   NewtonWorldForEachBodyInAABBDo(_World, @_WorldBox.StartVertex, @_WorldBox.EndVertex, @Debug_ShowBodyCollision, nil);
  glEnd();}
End;


End.

