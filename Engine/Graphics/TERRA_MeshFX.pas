{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_MeshFX
 * Implements Mesh geometry effects (eg: explosions)
 ***********************************************************************************************************************
}
Unit TERRA_MeshFX;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Mesh, TERRA_MeshFilter, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Quaternion, TERRA_Matrix4x4, TERRA_Color, TERRA_VertexFormat;

Type
  MeshDebris = Record
    Center:Vector3D;
    Direction:Vector3D;
    Duration:Single;
    Strength:Single;
    Count:Integer;
    StartVertex:Integer;
    Rotation:Quaternion;
  End;

  MeshExplosion = Class(MeshFX)
    Protected
      _StartTime:Cardinal;
      _Duration:Integer;
      _Radius:Single;

      _Positions:Array Of Vector3D;
      _Debris:Array Of MeshDebris;
      _DebrisCount:Integer;
      _Indices:Array Of Integer;

      _Initialized:Boolean;

      Procedure InitTarget();
      Procedure InitDebris(StartVertex, Index:Integer; Normal:Vector3D);

    Public
      Constructor Create(Duration:Integer);

      Function Update():Boolean; Override;
  End;


Implementation
Uses TERRA_OS, TERRA_Math;


{ MeshExplosion }
Constructor MeshExplosion.Create(Duration:Integer);
Begin
  _Duration := Duration;
  _Initialized := False;
End;

Procedure MeshExplosion.InitTarget();
Var
  I,J,K, N:Integer;
  Group:MeshGroup;
  P, OP:Vector3D;
  V:VertexIterator;
  TempVertices:VertexData;
  T:PTriangle;
  CurrentDebris:Integer;
Begin
  If Target = Nil Then
    Exit;

  TempVertices := Nil;
  For I:=0 To Pred(Target.GroupCount) Do
  Begin
    Group := Target.GetGroup(I);
    Group.Flags := Group.Flags Or meshGroupDoubleSided;

    Group.CalculateTriangleNormals();

    _Radius := Group.GetBoundingBox.Radius * 2;

    TempVertices := Group.Vertices.Clone();
    TempVertices.CopyBuffer(Group.Vertices);

    _DebrisCount := Trunc(Group.TriangleCount * RandomFloat(0.2, 0.5));
    SetLength(_Debris, _DebrisCount);

    SetLength(_Indices, Group.TriangleCount);
    Group.VertexCount := Group.TriangleCount * 3;

    SetLength(_Positions, Group.VertexCount);

    CurrentDebris := 0;
    InitDebris(0, 0, Group.GetTriangleNormal(0));

    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTrianglePointer(J);

      If (_Debris[CurrentDebris].Count>2+Random(2)) And (CurrentDebris<Pred(_DebrisCount)) Then
      Begin
        _Debris[CurrentDebris].Center.Scale(1/_Debris[CurrentDebris].Count);

        For K:=_Debris[CurrentDebris].StartVertex To Pred(J*3) Do
          _Positions[K].Subtract(_Debris[CurrentDebris].Center);

        Inc(CurrentDebris);
        InitDebris(J*3, CurrentDebris, Group.GetTriangleNormal(J));
      End;

      Inc(_Debris[CurrentDebris].Count);

      _Indices[J] := CurrentDebris;


      For K:=0 To 2 Do
      Begin
        N := J*3 + K;
        TempVertices.GetVector3D(T.Indices[K], vertexPosition, P);
        Group.Vertices.SetVector3D(N, vertexPosition, P);
        _Positions[J*3 + K] := P;
        _Debris[CurrentDebris].Center.Add(P);
         T.Indices[K] := J*3 + K;
      End;

{      For K:=0 To 2 Do
      Begin
        P := Group.GetVertexPointer(J*3 + K);
        P^ := Vertices[T.Indices[K]];
        _Positions[J*3 + K] := P^.Position;
        _Debris[CurrentDebris].Center.Add(P^.Position);
          T.Indices[K] := J*3 + K;
      End;}

    End;

    ReleaseObject(TempVertices);
  End;

  _StartTime := Application.GetTime();
End;

Procedure MeshExplosion.InitDebris(StartVertex, Index: Integer; Normal:Vector3D);
Begin
  _Debris[Index].Center := VectorZero;
  _Debris[Index].StartVertex := StartVertex;

  _Debris[Index].Direction := VectorCreate(RandomFloat(-10, 10), RandomFloat(-10, 10), RandomFloat(-10, 10));
  _Debris[Index].Direction.Normalize();

  _Debris[Index].Direction := VectorInterpolate(Normal, _Debris[Index].Direction, RandomFloat(0.3, 0.8));
  _Debris[Index].Direction.Normalize();

  _Debris[Index].Duration := RandomFloat(0.7, 1);
  _Debris[Index].Strength := RandomFloat(0.3, 1.2);

  _Debris[Index].Rotation := QuaternionFromAxisAngle(Normal, 360*RAD);
End;

Function MeshExplosion.Update():Boolean;
Const
  FadeDelta = 0.8;
Var
  I,J,K:Integer;
  Group:MeshGroup;
  PP, Delta:Single;
  T:Triangle;
  FinalPos, CurrentPosition:Vector3D;
  Alpha:Single;
  DebrisIndex:Integer;
  Q:Quaternion;
  M:Matrix4x4;
  It:VertexIterator;
  Temp:VertexData;
  V:MeshVertex;
  P:PVector3D;
Begin
  If Target = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  If Not _Initialized Then
  Begin
    _Initialized := True;
    Self.InitTarget();
  End;

  Delta := Application.GetTime - _StartTime;
  Delta := Delta / _Duration;

  If (Delta<0) Then
    Delta := 0
  Else
  If (Delta>1) Then
    Delta := 1;

  For I:=0 To Pred(Target.GroupCount) Do
  Begin
    Group := Target.GetGroup(I);
    Temp := Group.LockVertices();
    It := Temp.GetIteratorForClass(MeshVertex);

    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTriangle(J);

      DebrisIndex := _Indices[J];

      If (Delta>_Debris[DebrisIndex].Duration) Then
        PP := 1.0
      Else
        PP := Delta/_Debris[DebrisIndex].Duration;

      FinalPos := VectorAdd(_Debris[DebrisIndex].Center, VectorScale(_Debris[DebrisIndex].Direction, _Radius * _Debris[DebrisIndex].Strength));
      FinalPos.Y := Abs(Sin(PP*180*RAD)) * _Radius * _Debris[DebrisIndex].Strength;
      CurrentPosition := VectorInterpolate(_Debris[DebrisIndex].Center, FinalPos, PP);

      Q := QuaternionSlerp(QuaternionZero, _Debris[DebrisIndex].Rotation, Delta);
      M := QuaternionMatrix4x4(Q);


      If (PP>FadeDelta) Then
        Alpha := 1.0 - ((PP-FadeDelta)/(1-FadeDelta))
      Else
        Alpha := 1.0;

      For K:=0 To 2 Do
      Begin
        If Not It.HasNext() Then
          Break;

        V := MeshVertex(It.Value);
        V.Position := M.Transform(_Positions[It.Position]);
        V.Position.Add(CurrentPosition);

        V.BoneIndex := 0;

        V.BaseColor.A := Trunc(255*Alpha);
      End;
    End;

    ReleaseObject(It);

    Group.UnlockVertices();
  End;

  Result := Delta<1.0;
End;

End.