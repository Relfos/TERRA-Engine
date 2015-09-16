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
 * TERRA_Solids
 * Implements generic platonic solid meshes
 ***********************************************************************************************************************
}

Unit TERRA_Solids;
{$I terra.inc}

//http://www.geometrictools.com/Documentation/PlatonicSolids.pdf

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_Math, TERRA_GraphicsManager, TERRA_Resource, TERRA_BoundingBox, TERRA_Vector3D,
  TERRA_Vector2D, TERRA_Vector4D, TERRA_Matrix4x4, TERRA_Color, TERRA_Mesh, TERRA_MeshFilter, TERRA_Texture,
  TERRA_VertexFormat;

Type
  SolidVertex = Class(TERRAVertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
  		Position:Vector3D;
	  	Normal:Vector3D;
      Tangent:Vector4D;
      TextureCoords:Vector2D;
      Color:ColorRGBA;
  End;

	InternalSolidVertex = Packed Record
		Position:Vector3D;
		Normal:Vector3D;
    TextureCoords:Vector2D;
	End;

  SolidMesh = Class(TERRAObject)
    Protected
      _VertexList:Array Of InternalSolidVertex;
      _VertexCount:Integer;
      _IndexList:Array Of Word;
      _IndexCount:Integer;
      _Box:BoundingBox;

      Procedure AddTriangle(A,B,C:Word);

      Procedure AddMesh(Mesh:SolidMesh);

      Function DuplicateVertex(Index:Integer):Integer;

      Procedure UpdateBoundingBox;

    Public
      Procedure Transform(MyMatrix:Matrix4x4);
      Procedure Weld(Radius:Single);

      Procedure Invert;

      Function GetVertex(Index:Integer):InternalSolidVertex;
      Function GetIndex(Index:Integer):Integer;
      Property VertexCount:Integer Read _VertexCount;
      Property IndexCount:Integer Read _IndexCount;

      Property Box:BoundingBox Read _Box;
  End;

  TetrahedronMesh = Class(SolidMesh)
    Public
      Constructor Create();
  End;

  OctahedronMesh = Class(SolidMesh)
    Public
      Constructor Create();
  End;

  IcosahedronMesh = Class(SolidMesh)
    Public
      Constructor Create();
  End;

  PlaneMesh = Class(SolidMesh)
    Public
      Constructor Create(Const Normal:Vector3D; SubDivisions:Cardinal);
  End;

  CubeMesh = Class(SolidMesh)
    Public
      Constructor Create(SubDivisions:Cardinal);
  End;

  SphereMesh = Class(SolidMesh)
    Public
      Constructor Create(SubDivisions:Cardinal);
  End;

  CylinderMesh = Class(SolidMesh)
    Public
      Constructor Create(Stacks, Slices:Cardinal; Capped:Boolean = True);
  End;

  ConeMesh = Class(SolidMesh)
    Public
      Constructor Create(Stacks, Slices:Cardinal; Inverted:Boolean; Capped:Boolean = True);
  End;

  Function MergeSolidToMesh(Source:SolidMesh; Dest:TERRAMesh):MeshGroup;
  Function CreateMeshFromSolid(Source:SolidMesh; Tex:TERRATexture = Nil):TERRAMesh;

Implementation
Uses TERRA_Error, TERRA_Engine;

Const
  Scale = 1.0;

{Procedure SolidMesh.AddPolygon(IndexList:Array Of Word; Count:Integer);
Var
  I,N:Integer;
Begin
  N := _IndexCount;
  Inc(_IndexCount, (Count - 2) * 3);
  SetLength(_IndexList, _IndexCount);
  For I := 1 To (Count - 2) Do
  Begin
    _IndexList[N] := IndexList[0]; Inc(N);
    _IndexList[N] := IndexList[I + 0]; Inc(N);
    _IndexList[N] := IndexList[I + 1]; Inc(N);
  End;
End;}

Procedure SolidMesh.UpdateBoundingBox;
Var
  I:Integer;
Begin
  _Box.Reset;
  For I:=0 To Pred(_VertexCount) Do
    _Box.Add(_VertexList[I].Position);
End;

Procedure SolidMesh.AddTriangle(A,B,C:Word);
Var
  N:Integer;
Begin
  N := _IndexCount;
  Inc(_IndexCount, 3);
  SetLength(_IndexList, _IndexCount);

  _IndexList[N+0] := A;
  _IndexList[N+1] := B;
  _IndexList[N+2] := C
End;

Function SolidMesh.DuplicateVertex(Index:Integer):Integer;
Begin
  Inc(_VertexCount);
  SetLength(_VertexList, _VertexCount);
  _VertexList[Pred(_VertexCount)] := _VertexList[Index];
  Result := Pred(_VertexCount);
End;

Procedure SolidMesh.Transform(MyMatrix:Matrix4x4);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_VertexCount) Do
  Begin
    _VertexList[I].Position := MyMatrix.Transform(_VertexList[I].Position);
    _VertexList[I].Normal := MyMatrix.TransformNormal(_VertexList[I].Normal);
  End;
  UpdateBoundingBox;
End;

Procedure SolidMesh.Weld(Radius:Single);
Var
  I,J,K:Integer;
Begin
  I := 0;
  While (I<_VertexCount) Do
  Begin
    J:=0;
    While (J<_VertexCount) Do
    Begin
      If (I=J) Then
      Begin
        Inc(J);
      End Else
      If (_VertexList[I].Position.Distance(_VertexList[J].Position)<=Radius) Then
      Begin
        For K:=0 To Pred(_IndexCount) Do
        If (_IndexList[K] = J) Then
        Begin
          _IndexList[K] := I;
        End Else
        If (_IndexList[K] = Pred(_VertexCount)) Then
          _IndexList[K] := J;

        _VertexList[J] := _VertexList[Pred(_VertexCount)];
        Dec(_VertexCount);
      End Else
        Inc(J);
    End;

    Inc(I);
  End;
End;

Procedure SolidMesh.Invert;
Var
  I, K:Integer;
Begin
  For I:=0 To Pred(_IndexCount) Do
  If (I Mod 3 = 2) Then
  Begin
    K := _IndexList[I];
    _IndexList[I] := _IndexList[I-2];
    _IndexList[I-2] := K;
  End;

End;

Procedure SolidMesh.AddMesh(Mesh:SolidMesh);
Var
  I:Cardinal;
  VO, IO:Cardinal;
Begin
  VO := _VertexCount;
  IO := _IndexCount;

  Inc(_VertexCount, Mesh.VertexCount);
  Inc(_IndexCount, Mesh.IndexCount);
  SetLength(_VertexList, _VertexCount);
  SetLength(_IndexList, _IndexCount);

  For I:=0 To Pred(Mesh.VertexCount) Do
    _VertexList[VO + I] := Mesh._VertexList[I];

  For I:=0 To Pred(Mesh.IndexCount) Do
    _IndexList[IO + I] := Mesh._IndexList[I] + VO;

  UpdateBoundingBox;
End;

Function SolidMesh.GetVertex(Index:Integer):InternalSolidVertex;
Begin
  If (Index>=0) And (Index<_VertexCount) Then
    Result := _VertexList[Index]
  Else
  Begin
  	Result.Position := Vector3D_Zero;
    Engine.RaiseError('Invalid vertex index');
  End;
End;

Function SolidMesh.GetIndex(Index:Integer):Integer;
Begin
  Result := _IndexList[Index];
End;

{Constructor SimpleCubeMesh.Create();
Begin
  _VertexCount := 8;
  SetLength(_VertexList, _VertexCount);
  _VertexList[0].Position := VectorCreate(-Scale, -Scale, -Scale);
  _VertexList[1].Position := VectorCreate(Scale, -Scale, -Scale);
  _VertexList[2].Position := VectorCreate(Scale, Scale, -Scale);
  _VertexList[3].Position := VectorCreate(-Scale, Scale, -Scale);
  _VertexList[4].Position := VectorCreate(-Scale, -Scale, Scale);
  _VertexList[5].Position := VectorCreate(Scale, -Scale, Scale);
  _VertexList[6].Position := VectorCreate(Scale, Scale, Scale);
  _VertexList[7].Position := VectorCreate(-Scale, Scale, Scale);

  _IndexCount := 0;
  AddTriangle(0,3,2);
  AddTriangle(0,2,1);
  AddTriangle(0,1,5);
  AddTriangle(0,5,4);
  AddTriangle(0,4,7);
  AddTriangle(0,7,3);
  AddTriangle(6,5,1);
  AddTriangle(6,1,2);
  AddTriangle(6,2,3);
  AddTriangle(6,3,7);
  AddTriangle(6,7,4);
  AddTriangle(6,4,5);

  Self.GenerateData;
End;}

{
v number of vertices A dihedral angle between adjacent faces
e number of edges R radius of circumscribed sphere
f number of faces r radius of inscribed sphere
p number of edges per face L edge length
q number of edges sharing a vertex S surface area
V volume
}

Constructor TetrahedronMesh.Create;
Begin
  _VertexCount := 4;
  SetLength(_VertexList, _VertexCount);
  _VertexList[0].Position := Vector3D_Create(0, 0, Scale);
  _VertexList[1].Position := Vector3D_Create((2 * Sqrt(2)/3) * Scale, 0.0, (-1/3)*Scale);
  _VertexList[2].Position := Vector3D_Create((-Sqrt(2)/3) * Scale, (Sqrt(6)/3) * Scale, (-1/3)*Scale);
  _VertexList[3].Position := Vector3D_Create((-Sqrt(2)/3) * Scale, (-Sqrt(6)/3) * Scale, (-1/3)*Scale);

  _IndexCount := 0;
  AddTriangle(0,0,1);
  AddTriangle(0,2,3);
  AddTriangle(0,3,1);
  AddTriangle(1,3,2);

  Self.UpdateBoundingBox();
End;

Constructor OctahedronMesh.Create;
Begin
  _VertexCount := 6;
  SetLength(_VertexList, _VertexCount);
  _VertexList[0].Position := Vector3D_Create(Scale, 0, 0);
  _VertexList[1].Position := Vector3D_Create(-Scale, 0, 0);
  _VertexList[2].Position := Vector3D_Create(0, Scale, 0);
  _VertexList[3].Position := Vector3D_Create(0, -Scale, 0);
  _VertexList[4].Position := Vector3D_Create(0, 0, Scale);
  _VertexList[5].Position := Vector3D_Create(0, 0, -Scale);

  _IndexCount := 0;
  AddTriangle(4,0,2);
  AddTriangle(4,2,1);
  AddTriangle(4,1,3);
  AddTriangle(4,3,0);
  AddTriangle(5,2,0);
  AddTriangle(5,1,2);
  AddTriangle(5,3,1);
  AddTriangle(5,0,3);

  Self.UpdateBoundingBox();
End;

Constructor IcosahedronMesh.Create();
Var
  T:Single;
Begin
  T := ((1+Sqrt(5))/2) * Scale;
//  S := (Sqrt(1 + Sqr(T))) * Scale;

  _VertexCount := 12;
  SetLength(_VertexList, _VertexCount);
  _VertexList[0].Position := Vector3D_Create(T,1,0);
  _VertexList[1].Position := Vector3D_Create(-T,1,0);
  _VertexList[2].Position := Vector3D_Create(T,-1,0);
  _VertexList[3].Position := Vector3D_Create(-T,-1,0);
  _VertexList[4].Position := Vector3D_Create(1,0,T);
  _VertexList[5].Position := Vector3D_Create(1,0,-T);
  _VertexList[6].Position := Vector3D_Create(-1,0,T);
  _VertexList[7].Position := Vector3D_Create(-1,0,-T);
  _VertexList[8].Position := Vector3D_Create(0,T,1);
  _VertexList[9].Position := Vector3D_Create(0,-T,1);
  _VertexList[10].Position := Vector3D_Create(0,T,-1);
  _VertexList[11].Position := Vector3D_Create(0,-T,-1);

  _IndexCount := 0;
  AddTriangle(0,8,4);
  AddTriangle(0,5,10);
  AddTriangle(2,4,9);
  AddTriangle(2,11,5);
  AddTriangle(1,6,8);
  AddTriangle(1,10,7);
  AddTriangle(3,9,6);
  AddTriangle(3,7,11);
  AddTriangle(0,10,8);
  AddTriangle(1,8,10);
  AddTriangle(2,9,11);
  AddTriangle(3,11,9);
  AddTriangle(4,2,0);
  AddTriangle(5,0,2);
  AddTriangle(6,1,3);
  AddTriangle(7,3,1);
  AddTriangle(8,6,4);
  AddTriangle(9,4,6);
  AddTriangle(10,5,7);
  AddTriangle(11,7,5);

  Self.UpdateBoundingBox();
End;


{ CylinderMesh }
Constructor CylinderMesh.Create(Stacks, Slices: Cardinal; Capped:Boolean);
Var
  I,J:Cardinal ;
  Dy, Angle:Single;
  A,B,C,D:Cardinal;
Begin
  _VertexCount := Succ(Stacks) * Succ(Slices);
  SetLength(_VertexList, _VertexCount);

  Dy := 1/Stacks;
  For J:=0 To Stacks Do
  Begin
    For I:=0 To (Slices) Do
    Begin
      Angle := (I/Slices)*360*RAD;
      _VertexList[J*Succ(Slices) + I].Position := Vector3D_Create(0.5 * Scale * Cos(Angle), Scale * ((Dy * J) - 0.5), -0.5 * Scale * Sin(Angle));
      _VertexList[J*Succ(Slices) + I].TextureCoords.X := (I/(Slices));
      _VertexList[J*Succ(Slices) + I].TextureCoords.Y := (J/Stacks);
      _VertexList[J*Succ(Slices) + I].Normal := Vector3D_Create(Cos(Angle), 0, -Sin(Angle));
    End;
  End;

  For J:=0 To Pred(Stacks) Do
  Begin
    For I:=0 To (Slices) Do
    Begin
      A := I;
      B := Succ(I) Mod Succ(Slices);
      C := A + Succ(Slices);
      D := B + Succ(Slices);
      AddTriangle(J*Succ(Slices) + A, J*Succ(Slices) + B, J*Succ(Slices) + C);
      AddTriangle(J*Succ(Slices) + C, J*Succ(Slices) + B, J*Succ(Slices) + D);
    End;
  End;

  If (Capped) Then
  Begin
    A := _VertexCount;
    B := A + 1;
    Inc(_VertexCount, 2);
    SetLength(_VertexList, _VertexCount);

    For J:=0 To 1 Do
    Begin
      If (J=0) Then
      Begin
        _VertexList[A+J].Position := Vector3D_Create(0, 0.5 * Scale, 0);
        _VertexList[A+J].Normal := Vector3D_Up;
      End Else
      Begin
        _VertexList[A+J].Position := Vector3D_Create(0, 0, 0);
        _VertexList[A+J].Normal := Vector3D_Create(0, -0.5 * Scale, 0);
      End;
      _VertexList[A+J].TextureCoords := Vector2D_Create(0.5, 0.5);
      For I:=0 To Pred(Slices) Do
      Begin
        C := I;
        D := Succ(I) Mod Succ(Slices);
        If J=0 Then
          AddTriangle(A+J, Stacks*Succ(Slices) + C, Stacks*Succ(Slices) + D)
        Else
          AddTriangle(D, C, A+J);
      End;
    End;

  End;
End;

{ ConeMesh }
Constructor ConeMesh.Create(Stacks, Slices: Cardinal; Inverted, Capped:Boolean);
Var
  I,J:Cardinal;
  Dy, Scale, Angle:Single;
  A,B,C,D:Cardinal;
Begin
  _VertexCount := Succ(Stacks) * Succ(Slices);
  SetLength(_VertexList, _VertexCount);

  Dy := 1/Stacks;
  For J:=0 To Stacks Do
  Begin
    Scale := (J/Stacks);
    If Inverted Then
      Scale := 1.0 - Scale;

    For I:=0 To (Slices) Do
    Begin
      Angle := (I/Slices)*360*RAD;
      _VertexList[J*Succ(Slices) + I].Position := Vector3D_Create(0.5 * Scale * Cos(Angle), Scale * ((Dy * J) - 0.5), -0.5 * Scale * Sin(Angle));
      _VertexList[J*Succ(Slices) + I].TextureCoords.X := (I/(Slices));
      _VertexList[J*Succ(Slices) + I].TextureCoords.Y := (J/Stacks);
      _VertexList[J*Succ(Slices) + I].Normal := Vector3D_Create(Cos(Angle), 0, -Sin(Angle));
    End;
  End;

  For J:=0 To Pred(Stacks) Do
  Begin
    For I:=0 To (Slices) Do
    Begin
      A := I;
      B := Succ(I) Mod Succ(Slices);
      C := A + Succ(Slices);
      D := B + Succ(Slices);
      AddTriangle(J*Succ(Slices) + A, J*Succ(Slices) + B, J*Succ(Slices) + C);
      AddTriangle(J*Succ(Slices) + C, J*Succ(Slices) + B, J*Succ(Slices) + D);
    End;
  End;

  If (Capped) Then
  Begin
    A := _VertexCount;
    Inc(_VertexCount, 1);
    SetLength(_VertexList, _VertexCount);

    If Inverted Then
      _VertexList[A].Position := Vector3D_Create(0, 0, 0)
    Else
      _VertexList[A].Position := Vector3D_Create(0, 1 * Scale, 0);
      
    _VertexList[A].Normal := Vector3D_Create(0, -1, 0);
    _VertexList[A].TextureCoords := Vector2D_Create(0.5, 0.5);
    For I:=0 To Pred(Slices) Do
    Begin
      C := I;
      D := Succ(I) Mod Succ(Slices);
      If Not Inverted Then
      Begin
        Inc(C, (Stacks) * Succ(Slices));
        Inc(D, (Stacks) * Succ(Slices));
        AddTriangle(A, C, D);
      End Else
        AddTriangle(D, C, A);
    End;
  End;
End;

Constructor PlaneMesh.Create(Const Normal:Vector3D; SubDivisions:Cardinal);
Var
  Index, I,J:Cardinal;
  U,V:Vector3D;
  S, SX,SY:Single;
Begin
  _VertexCount := Sqr(Succ(SubDivisions));
  SetLength(_VertexList, _VertexCount);

  Normal.Normalize();

  If (Abs(Normal.Y)>Abs(Normal.X)) And (Abs(Normal.Y)>Abs(Normal.Z)) Then
  Begin
    U := Vector3D_Create(Normal.X, Normal.Z, Normal.Y);
  End Else
  Begin
    U := Vector3D_Create(Normal.Z, Normal.Y, Normal.X);
  End;

  V := Vector3D_Cross(Normal, U) ;

  S := (1.0 / (SubDivisions)) * Scale;
  For J:=0 To SubDivisions Do
    For I:=0 To SubDivisions Do
    Begin
      Index := (J * Succ(SubDivisions)) + I;
      Sx := (S * I) - 0.5;
      Sy := (S * J) - 0.5;

      _VertexList[Index].Position := Vector3D_Add(Vector3D_Scale(U, Sx), Vector3D_Scale(V, Sy ));
      _VertexList[Index].TextureCoords.X := 1.0 - ((1.0/Succ(SubDivisions)) * J);
      _VertexList[Index].TextureCoords.Y := ((1.0/Succ(SubDivisions)) * I);
      _VertexList[Index].Normal := Normal;
    End;

  _IndexCount := 0;
  For J:=0 To Pred(SubDivisions) Do
    For I:=0 To Pred(SubDivisions) Do
    Begin
      Index := (J * Succ(SubDivisions));
      AddTriangle(Index + I, Index + Succ(I), Index + I + Succ(SubDivisions));
      AddTriangle(Index + Succ(I), Index + Succ(I) + Succ(SubDivisions) , Index + I + Succ(SubDivisions));
    End;

  Self.UpdateBoundingBox();
End;

Constructor CubeMesh.Create(SubDivisions:Cardinal);
Const
  Size = 1.0;
Var
  Mesh:SolidMesh;
  MyMatrix:Matrix4x4;
  I:Integer;
Begin
  Mesh := PlaneMesh.Create(Vector3D_Create(0, 0, 1.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(0, 0, Size * 0.5);
  Mesh.Transform(MyMatrix);
  For I:=0 To Pred(Mesh._VertexCount) Do
  Begin
    Mesh._VertexList[I].TextureCoords.X := 1.0 - Mesh._VertexList[I].TextureCoords.X;
    Mesh._VertexList[I].TextureCoords.Y := 1.0 - Mesh._VertexList[I].TextureCoords.Y;
  End;
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  Mesh := PlaneMesh.Create(Vector3D_Create(0,0, -1.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(0, 0, -Size * 0.5);
  Mesh.Transform(MyMatrix);
  For I:=0 To Pred(Mesh._VertexCount) Do
  Begin
    Mesh._VertexList[I].TextureCoords.X := 1.0 - Mesh._VertexList[I].TextureCoords.X;
    Mesh._VertexList[I].TextureCoords.Y := 1.0 - Mesh._VertexList[I].TextureCoords.Y;
  End;
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  Mesh := PlaneMesh.Create(Vector3D_Create(1.0,0,0.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(Size * 0.5, 0, 0);
  Mesh.Transform(MyMatrix);
  For I:=0 To Pred(Mesh._VertexCount) Do
  Begin
    Mesh._VertexList[I].TextureCoords.X := 1.0 - Mesh._VertexList[I].TextureCoords.X;
  End;
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  Mesh := PlaneMesh.Create(Vector3D_Create(-1.0,0,0.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(-Size * 0.5, 0, 0);
  Mesh.Transform(MyMatrix);
  For I:=0 To Pred(Mesh._VertexCount) Do
  Begin
    Mesh._VertexList[I].TextureCoords.X := 1.0 - Mesh._VertexList[I].TextureCoords.X;
  End;
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  Mesh := PlaneMesh.Create(Vector3D_Create(0.0,1.0,0.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(0, Size * 0.5, 0);
  Mesh.Transform(MyMatrix);
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  Mesh := PlaneMesh.Create(Vector3D_Create(0.0,-1.0,0.0), SubDivisions);
  MyMatrix := Matrix4x4_Translation(0, -Size * 0.5, 0);
  Mesh.Transform(MyMatrix);
  For I:=0 To Pred(Mesh._VertexCount) Do
  Begin
    Mesh._VertexList[I].TextureCoords.X := 1.0 - Mesh._VertexList[I].TextureCoords.X;
    Mesh._VertexList[I].TextureCoords.Y := 1.0 - Mesh._VertexList[I].TextureCoords.Y;
  End;
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);
End;

Constructor SphereMesh.Create(SubDivisions:Cardinal);
Var
  Mesh:SolidMesh;
  I,K:Integer;
  Normal:Vector3D;
  A,B:Integer;

Procedure EdgeFix(A,B:Integer);
Var
  Diff:Single;
Begin
  Diff := Abs(_VertexList[_IndexList[A]].TextureCoords.X - _VertexList[_IndexList[B]].TextureCoords.X);
  If (Diff>=0.75) Then
  Begin
    If (_VertexList[_IndexList[B]].TextureCoords.X>=1.0) Then
    Begin
      _IndexList[B] := DuplicateVertex(_IndexList[B]);
      _VertexList[_IndexList[B]].TextureCoords.X := _VertexList[_IndexList[B]].TextureCoords.X - 1.0;
    End Else
    Begin
      _IndexList[A] := DuplicateVertex(_IndexList[A]);
      _VertexList[_IndexList[A]].TextureCoords.X := _VertexList[_IndexList[A]].TextureCoords.X - 1.0;
    End;
  End;
End;

Function IsPole(Index:Integer):Boolean;
Begin
  Result := (_VertexList[Index].Normal.Y >=1.0) Or (_VertexList[Index].Normal.Y <=-1.0);
End;

Procedure FixPole(Pole, A,B:Integer);
Begin
  _IndexList[Pole] := DuplicateVertex(_IndexList[Pole]);
  _VertexList[_IndexList[Pole]].TextureCoords.X := 0.5 * (_VertexList[_IndexList[A]].TextureCoords.X + _VertexList[_IndexList[B]].TextureCoords.X);
End;

Begin
  Mesh := CubeMesh.Create(SubDivisions);
  Mesh.Weld(0.01);
  Self.AddMesh(Mesh);
  ReleaseObject(Mesh);

  For I:=0 To Pred(_VertexCount) Do
  Begin
    Normal := _VertexList[I].Position;
    Normal.Normalize();
    _VertexList[I].Position := Vector3D_Scale(Normal, Scale * 0.5);
    _VertexList[I].Normal := Normal;
    _VertexList[I].TextureCoords.X := ((Atan2(Normal.X, Normal.Z) / PI) * 0.5) + 0.5; //(Normal.X * 0.5) + 0.5;
    _VertexList[I].TextureCoords.Y := Arccos(Normal.Y) / PI;//(Normal.Y * 0.5) + 0.5;
  End;

  // fix all discontinuities in the U coord
  For I:=0 To Pred(_IndexCount) Do
  If (I Mod 3 = 0) Then
  Begin
    For K:=0 To 2 Do
      EdgeFix(I+K, I+((K+1) Mod 3));
  End;

  For I:=0 To Pred(_IndexCount) Do
  If (I Mod 3 = 0) Then
  Begin
    For K:=0 To 2 Do
    If (IsPole(_IndexList[I+K])) Then
    Begin
      A := I + ((K+1) Mod 3);
      B := I + ((K+2) Mod 3);
      FixPole(I+K, A,B);
    End;
  End;
End;

Function MergeSolidToMesh(Source:SolidMesh; Dest:TERRAMesh):MeshGroup;
Var
  Group:MeshGroup;
  I:Integer;
  It:VertexIterator;
  V:SolidVertex;
Begin
  Group := Dest.AddGroup([vertexFormatPosition, vertexFormatColor, vertexFormatNormal, vertexFormatTangent, vertexFormatUV0], '');
  Group.Flags := 0;
//  Group.AmbientColor := ColorWhite;
  Group.DiffuseColor := ColorWhite;
  Group.TriangleCount := Source._IndexCount Div 3;
  Group.VertexCount := Source._VertexCount;

  It := Group.Vertices.GetIteratorForClass(SolidVertex);
  While It.HasNext() Do
  Begin
    V := SolidVertex(It.Value);
    I := It.Position;

    V.Position := Source._VertexList[I].Position;
    V.Normal := Source._VertexList[I].Normal;
    V.TextureCoords := Source._VertexList[I].TextureCoords;
    V.Color := ColorWhite;
  End;
  ReleaseObject(It);

  For I:=0 To Pred(Group.TriangleCount) Do
  Begin
    Group.Triangles[I].Indices[0] := Source._IndexList[I*3+0];
    Group.Triangles[I].Indices[1] := Source._IndexList[I*3+1];
    Group.Triangles[I].Indices[2] := Source._IndexList[I*3+2];
  End;

  Group.CalculateTangents();
  Group.CalculateTriangleNormals();

  Result := Group;
End;

Function CreateMeshFromSolid(Source:SolidMesh; Tex:TERRATexture):TERRAMesh;
Var
  Group:MeshGroup;
Begin
  Result := TERRAMesh.Create(rtDynamic);

  Group := MergeSolidToMesh(Source, Result);
  Group.DiffuseMap := Tex;

  Result.UpdateBoundingBox();
End;

{ SolidVertex }
Procedure SolidVertex.Load;
Begin
  Self.GetVector3D(vertexPosition, Self.Position);
  Self.GetVector3D(vertexNormal, Self.Normal);
  Self.GetVector2D(vertexUV0, Self.TextureCoords);
  Self.GetColor(vertexColor, Self.Color);
End;

Procedure SolidVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Self.Position);
  Self.SetVector3D(vertexNormal, Self.Normal);
  Self.SetVector2D(vertexUV0, Self.TextureCoords);
  Self.SetColor(vertexColor, Self.Color);
End;

End.
