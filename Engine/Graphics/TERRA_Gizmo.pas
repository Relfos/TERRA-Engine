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
 * TERRA_Gizmo
 * Implements a 3D gizmo (for translation, rotation and scale)
 ***********************************************************************************************************************
}

Unit TERRA_Gizmo;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Ray, TERRA_Math, TERRA_GraphicsManager,
  TERRA_DebugDraw, TERRA_BoundingBox, TERRA_Color, TERRA_Mesh, TERRA_Application, TERRA_OS;

Type
  Gizmo = Class(Renderable)
    Protected
      _SmallCubes:Array Of MeshInstance;
      _Cubes:Array[0..2] Of MeshInstance;
      _Cones:Array[0..2] Of MeshInstance;
      _Planes:Array[0..5] Of MeshInstance;
      _Radius:Single;
      _PlaneScale:Single;

      _Locked:Boolean;
      _LockedAxis:Integer;
      _LockType:Integer;
      _LockPoint:Vector3D;
      _LockCube:Integer;
      _Initialized:Boolean;

    Public
      Position:Vector3D;

      Constructor Create;

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render(Pass:Boolean); Override;

      Procedure UpdateRadius(Box:BoundingBox);

      Function OnMouseDown(X,Y:Integer):Boolean;
      Function OnMouseMove(X,Y:Integer):Boolean;
      Function OnMouseUp(X,Y:Integer):Boolean;

      Procedure OnMove(Tx, Ty, Tz:Single); Virtual;
      Procedure OnScale(Sx, Sy, Sz:Single); Virtual;
      Procedure OnRotate(Rx, Ry, Rz:Single); Virtual;
  End;

Implementation

Uses TERRA_Solids;

Var
  _CubeMesh:Mesh;
  _ConeMesh:Mesh;
  _PlaneMesh:Mesh;

Const
  Segments = 32;
  CubeSegments = Segments;

  RotateThickness = 2.0;
  TranslateThickness = 4.0;

  CubeScale = 0.5;
  ConeScale = 0.5;

  lockTranslate = 0;
  lockRotate    = 1;
  lockScale     = 2;

{ Gizmo }
Constructor Gizmo.Create;
Var
  I:Integer;
  S:SolidMesh;
  R:Single;
Begin
  _Radius := 5;

  If (_CubeMesh=Nil) Then
  Begin
    S := CubeMesh.Create(1);
    _CubeMesh := CreateMeshFromSolid(S);
    S.Destroy;
  End;

  If (_ConeMesh=Nil) Then
  Begin
    S := ConeMesh.Create(1, 8, True);
    _ConeMesh := CreateMeshFromSolid(S);
    S.Destroy;
  End;

  If (_PlaneMesh=Nil) Then
  Begin
    S := PlaneMesh.Create(VectorUp, 2);
    _PlaneMesh := CreateMeshFromSolid(S);
    S.Destroy;
  End;

  For I:=0 To 2 Do
  Begin
    _Cubes[I] := MeshInstance.Create(_CubeMesh);
    _Cubes[I].SetScale(VectorUniform(CubeScale));

    _Cones[I] := MeshInstance.Create(_ConeMesh);
    _Cones[I].SetScale(VectorCreate(ConeScale, ConeScale*2.5, ConeScale));
  End;

  For I:=0 To 5 Do
  Begin
    _Planes[I] := MeshInstance.Create(_PlaneMesh);
  End;

  R := 90*RAD;
  _Cones[1].SetRotation(VectorCreate(0, 0, -R));
  _Cones[2].SetRotation(VectorCreate(R, 0, 0));

  _Planes[0].SetRotation(VectorCreate(0, 0, R));
  _Planes[1].SetRotation(VectorCreate(R, 0, 0));

  _Planes[2].SetRotation(VectorCreate(0, 90*RAD, 0));
  _Planes[3].SetRotation(VectorCreate(-90*RAD, 0, 0));

  _Planes[4].SetRotation(VectorCreate(0, 0, 0));
  _Planes[5].SetRotation(VectorCreate(0, 0, 90*RAD));

  SetLength(_SmallCubes, CubeSegments*3);
  For I:=0 To Pred(CubeSegments*3) Do
  Begin
    _SmallCubes[I] := MeshInstance.Create(_CubeMesh);
  End;
End;

Function Gizmo.GetBoundingBox: BoundingBox;
Begin
  Result.StartVertex := VectorAdd(Position, VectorCreate(-_Radius, -_Radius, -_Radius));
  Result.EndVertex := VectorAdd(Position, VectorCreate(_Radius, _Radius, _Radius));
End;

Function Gizmo.OnMouseDown(X, Y: Integer): Boolean;
Var
  T:Single;
  I:Integer;
  R:Ray;
Begin
  Result := False;
  R := GraphicsManager.Instance.ActiveViewport.GetPickRay(X, Y);

  //DrawRay(R, ColorWhite, 50, True);
  _Locked := False;

  For I:=0 To 2 Do
  Begin
    T := 9999;
    If (_Cubes[I].Geometry.Intersect(R, T, _Cubes[I].Transform)) Then
    Begin
      _LockType := lockScale;
      _Locked := True;
      _LockedAxis := I;
      _LockPoint := R.IntersectionPoint(T);
      Result := True;
      Exit;
    End;

    If (_Cones[I].Geometry.Intersect(R, T, _Cones[I].Transform)) Then
    Begin
      _LockType := lockTranslate;
      _Locked := True;
      _LockedAxis := I;
      _LockPoint := R.IntersectionPoint(T);
      Result := True;
      Exit;
    End;
  End;

  For I:=0 To Pred(CubeSegments*3) Do
  Begin
    T := 9999;
    If (_SmallCubes[I].Geometry.Intersect(R, T, _SmallCubes[I].Transform)) Then
    Begin
      _LockType := lockRotate;
      _Locked := True;
      _LockCube := I;
      _LockedAxis := I Div CubeSegments;
      _LockPoint := R.IntersectionPoint(T);
      Result := True;
      Exit;
    End;
  End;
End;

Function Gizmo.OnMouseMove(X, Y: Integer): Boolean;
Var
  T, D:Single;
  I:Integer;
  R:Ray;
  P:Vector3D;
  PP:Array[0..1] Of Integer;
Begin
  Result := False;
  R := GraphicsManager.Instance.ActiveViewport.GetPickRay(X, Y);

  If (_Locked) Then
  Begin
    T := 99;
    PP[0] := _LockedAxis*2;
    PP[1] := PP[0]+1;

    For I:=0 To 1 Do
    If (_Planes[PP[I]].Geometry.Intersect(R, T, _Planes[PP[I]].Transform)) Then
    Begin
       P := R.IntersectionPoint(T);
      Case _LockedAxis Of
      0:  D := P.Y - _LockPoint.Y;
      1:  D := P.X - _LockPoint.X;
      2:  D := P.Z - _LockPoint.Z;
      End;
      _LockPoint := P;

      Case _LockType Of
      lockTranslate:
        Case _LockedAxis Of
        0:Self.OnMove(0, D, 0);
        1:Self.OnMove(D, 0, 0);
        2:Self.OnMove(0, 0, D);
        End;
      lockScale:
        Begin
          If (D<0) Then D := -0.1
          Else D := 0.1;
        Case _LockedAxis Of
        0:Self.OnScale(0, D, 0);
        1:Self.OnScale(D, 0, 0);
        2:Self.OnScale(0, 0, D);
        End;
        End;
      lockRotate:
        Begin
          D := D * 0.25;
          Case _LockedAxis Of
          0:Self.OnRotate(0, D, 0);
          1:Self.OnRotate(D, 0, 0);
          2:Self.OnRotate(0, 0, D);
          End;
        End;
      End;
      Result := True;
      Exit;
    End;
  End;
End;

Function Gizmo.OnMouseUp(X, Y: Integer): Boolean;
Begin
  _Locked := False;
End;

Procedure Gizmo.OnMove(Tx, Ty, Tz: Single);
Begin
  // do nothing
End;

Procedure Gizmo.OnRotate(Rx, Ry, Rz: Single);
Begin
  // do nothing
End;

Procedure Gizmo.OnScale(Sx, Sy, Sz: Single);
Begin
  // do nothing
End;

Procedure Gizmo.Render;
Var
  I, J:Integer;
  Angle, Dx:Single;
  A,B:Array[0..2] Of Vector3D;
Begin
  Angle := 0.0;
  Dx := 1/Segments;
  For I:=0 To Pred(Segments) Do
  Begin
    Angle := (I/Segments) * 360 * RAD;
    A[0] := VectorCreate(Cos(Angle)*_Radius, 0, -Sin(Angle)*_Radius);
    A[1] := VectorCreate(0, Cos(Angle)*_Radius, -Sin(Angle)*_Radius);
    A[2] := VectorCreate(Cos(Angle)*_Radius, -Sin(Angle)*_Radius, 0);

    Angle := (Succ(I)/Segments) * 360 * RAD;
    B[0] := VectorCreate(Cos(Angle)*_Radius, 0, -Sin(Angle)*_Radius);
    B[1] := VectorCreate(0, Cos(Angle)*_Radius, -Sin(Angle)*_Radius);
    B[2] := VectorCreate(Cos(Angle)*_Radius, -Sin(Angle)*_Radius, 0);

    For J:=0 To 2 Do
    Begin
      A[J].Add(Position);
      B[J].Add(Position);
    End;

    DrawLine3D(A[0], B[0], ColorBlue, RotateThickness, False, True);
    DrawLine3D(A[1], B[1], ColorRed, RotateThickness, False, True);
    DrawLine3D(A[2], B[2], ColorGreen, RotateThickness, False, True);

    For J:=0 To 2 Do
      _SmallCubes[J*CubeSegments+I].SetPosition(A[J]);
  End;

  For I:=0 To 2 Do
    A[I] := VectorZero;

  B[0] := VectorCreate(0, _Radius * 1.5, 0);
  B[1] := VectorCreate(_Radius * 1.5, 0, 0);
  B[2] := VectorCreate(0, 0, _Radius * 1.5);

  For J:=0 To 2 Do
  Begin
    A[J].Add(Position);
    B[J].Add(Position);
  End;

  DrawLine3D(A[0], B[0], ColorBlue, TranslateThickness, False, True);
  DrawLine3D(A[1], B[1], ColorRed, TranslateThickness, False, True);
  DrawLine3D(A[2], B[2], ColorGreen, TranslateThickness, False, True);

  _Cubes[0].SetPosition(VectorCreate(Position.X, Position.Y + _Radius * 1.25, Position.Z));
  _Cubes[0].SetColor(0, ColorBlue);

  _Cubes[1].SetPosition(VectorCreate(Position.X+ _Radius * 1.25, Position.Y, Position.Z));
  _Cubes[1].SetColor(0, ColorRed);

  _Cubes[2].SetPosition(VectorCreate(Position.X, Position.Y, Position.Z + _Radius * 1.25));
  _Cubes[2].SetColor(0, ColorGreen);

  _Cones[0].SetPosition(VectorCreate(Position.X, Position.Y + _Radius * 1.5, Position.Z));
  _Cones[0].SetColor(0, ColorBlue);

  _Cones[1].SetPosition(VectorCreate(Position.X+ _Radius * 1.5, Position.Y, Position.Z));
  _Cones[1].SetColor(0, ColorRed);

  _Cones[2].SetPosition(VectorCreate(Position.X, Position.Y, Position.Z + _Radius * 1.5));
  _Cones[2].SetColor(0, ColorGreen);

  _PlaneScale := _Radius * 8;

  _Planes[0].SetPosition(VectorCreate(Position.X , Position.Y- _PlaneScale*0.5, Position.Z- _PlaneScale*0.5));
  _Planes[0].SetColor(0, ColorBlue);

  _Planes[1].SetPosition(VectorCreate(Position.X- _PlaneScale*0.5, Position.Y+ _PlaneScale*0.5, Position.Z));
  _Planes[1].SetColor(0, ColorBlue);

  _Planes[2].SetPosition(VectorCreate(Position.X- _PlaneScale*0.5, Position.Y, Position.Z+ _PlaneScale*0.5 ));
  _Planes[2].SetColor(0, ColorRed);

  _Planes[3].SetPosition(VectorCreate(Position.X- _PlaneScale*0.5, Position.Y- _PlaneScale*0.5, Position.Z ));
  _Planes[3].SetColor(0, ColorRed);

  _Planes[4].SetPosition(VectorCreate(Position.X- _PlaneScale*0.5, Position.Y, Position.Z - _PlaneScale*0.5));
  _Planes[4].SetColor(0, ColorGreen);

  _Planes[5].SetPosition(VectorCreate(Position.X, Position.Y- _PlaneScale*0.5, Position.Z - _PlaneScale*0.5));
  _Planes[5].SetColor(0, ColorGreen);

  For J:=0 To 5 Do
    _Planes[J].SetScale(VectorUniform(_PlaneScale));

  For J:=0 To 2 Do
  Begin
    _Cubes[J].AlwaysOnTop := True;
    _Cubes[J].SetScale(VectorUniform(CubeScale*_Radius/5));
    GraphicsManager.Instance.AddRenderable(_Cubes[J], renderFlagsSkipSorting);

    _Cones[J].AlwaysOnTop := True;
    _Cones[J].SetScale(VectorCreate(ConeScale*_Radius/5, ConeScale*_Radius/5*2.5, ConeScale*_Radius/5));
    GraphicsManager.Instance.AddRenderable(_Cones[J], renderFlagsSkipSorting);
  End;

  For J:=0 To 5 Do
    _Planes[J].UpdateTransform();

  If (Not _Initialized) Or (Application.Instance.Input.Keys[keyControl]) Then
  Begin
    _Initialized := True;
  End;

  For I:=0 To Pred(CubeSegments*3) Do
  Begin
    _SmallCubes[I].SetScale(VectorUniform(_Radius/8));
    _SmallCubes[I].UpdateTransform();
    //GraphicsManager.Instance.AddRenderable(_SmallCubes[I]);
  End;
End;

Procedure Gizmo.UpdateRadius(Box: BoundingBox);
Begin
  _Radius := Box.Radius();
End;

End.
