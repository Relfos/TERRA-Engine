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
 * TERRA_MeshTopology
 * Implements functions to detect and manipulate mesh topology
 ***********************************************************************************************************************
}
Unit TERRA_MeshTopology;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Resource, TERRA_Vector2D, TERRA_Vector3D, TERRA_Math,
  TERRA_Matrix4x4, TERRA_Color, TERRA_Quaternion, TERRA_Mesh, TERRA_VertexFormat, TERRA_Collections;

Type
  MeshHalfEdge = Record
    StartVertex:Integer; // vertex at the start of the half-edge
    EndVertex:Integer; // vertex at the end of the half-edge
    FaceIndex:Integer;  // face the half-edge borders

    PairEdge:Integer;   // oppositely oriented adjacent half-edge
    NextEdge:Integer;  // next half-edge around the face
  End;

  MeshEdgeAdjancency = Record
    StartVertex, EndVertex:Integer;
    FaceA, FaceB:Integer;
  End;

  MeshVertexAdjancency = Record
    TriangleList:IntegerArrayObject;
    EdgeList:IntegerArrayObject;
  End;

  MeshEdgeLoop = Class(TERRAObject)
    Protected
      _Target:MeshGroup;
      _Indices:Array Of Integer;
      _IndexCount:Integer;

      Procedure Add(VertexIndex:Integer);
      Procedure Clear();

    Public
      Constructor Create(Target:MeshGroup);
  End;

  MeshGroupTopology = Class(TERRAObject)
    Protected
      _Target:MeshGroup;
      {
      _HalfEdgeCount:Integer;
      _HalfEdges:Array Of MeshHalfEdge;}

      _EdgeAdjacencyList:Array Of MeshEdgeAdjancency;
      _EdgeAdjacencyCount:Integer;

      _VertexAdjancencyList:Array Of MeshVertexAdjancency;
      _VertexSpatialHashes:Array Of Cardinal;

      _EdgeLoops:Array Of MeshEdgeLoop;
      _EdgeLoopCount:Integer;

      Procedure CalculateTopology();
      Procedure CalculateAdjacency();

      Procedure FindEdgeLoops();

    Public
      Constructor Create(Target:MeshGroup);
      Procedure Release; Override;

      Procedure SubDivide3();
      Procedure SubDivide6();
      Procedure Smooth();

      Function GetEdge(Const StartVertex, EndVertex:Integer):Integer;
      Procedure GetVertexAdjancency(Const VertexIndex:Integer; Out Result:IntegerArrayObject);

      Function GetEdgeLoop(Index:Integer):MeshEdgeLoop;
      Property EdgeLoopCount:Integer Read _EdgeLoopCount;
  End;

  MeshTopology = Class(TERRAObject)
    Protected
      _Target:Mesh;

      _Topologies:Array Of MeshGroupTopology;

    Public
      Constructor Create(Target:Mesh);
      Procedure Release; Override;

      Procedure SubDivide3();
      Procedure SubDivide6();
      Procedure Smooth();

      Function GetGroupTopology(Index:Integer):MeshGroupTopology;
  End;

Implementation
Uses TERRA_CRC32;

{ MeshTopology }
Constructor MeshTopology.Create(Target: Mesh);
Var
  I:Integer;
Begin
  _Target := Target;

  If _Target = Nil Then
    Exit;

  SetLength(_Topologies, _Target.GroupCount);
  For I:=0 To Pred(_Target.GroupCount) Do
    _Topologies[I] := MeshGroupTopology.Create(Target.GetGroup(I));
End;

Procedure MeshTopology.Release;
Var
  I:Integer;
Begin
  If _Target = Nil Then
    Exit;

  For I:=0 To Pred(_Target.GroupCount) Do
    ReleaseObject(_Topologies[I]);

  _Target := Nil;
End;

Procedure MeshTopology.SubDivide3();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Target.GroupCount) Do
    _Topologies[I].SubDivide3();
End;

Procedure MeshTopology.SubDivide6();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Target.GroupCount) Do
    _Topologies[I].SubDivide6();
End;

Procedure MeshTopology.Smooth();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Target.GroupCount) Do
    _Topologies[I].Smooth();
End;

Function MeshTopology.GetGroupTopology(Index: Integer): MeshGroupTopology;
Begin
  If (_Target = Nil) Or (Index<0) Or (Index>=_Target.GroupCount) Then
    Result := Nil
  Else
    Result := _Topologies[Index];
End;

{ MeshGroupTopology }
Procedure MeshGroupTopology.CalculateAdjacency;
Const
  SnapFactor = 5;

Var
  I, J:Integer;
  P:Vector3D;
  T:Triangle;
  V:Array[0..2] Of Integer;

  Procedure MakeEdge(TriID, VA, VB:Integer);
  Var
    I:Integer;
  Begin
    For I:=0 To Pred(_EdgeAdjacencyCount) Do
    If ((_EdgeAdjacencyList[I].StartVertex = VA) And (_EdgeAdjacencyList[I].EndVertex = VB)
    Or (_EdgeAdjacencyList[I].StartVertex = VB) And (_EdgeAdjacencyList[I].EndVertex = VA)) Then
    Begin
      _EdgeAdjacencyList[I].FaceB := TriID;
      Exit;
    End;

    I := _EdgeAdjacencyCount;
    Inc(_EdgeAdjacencyCount);
    If Length(_EdgeAdjacencyList)<=_EdgeAdjacencyCount Then
      SetLength(_EdgeAdjacencyList, Length(_EdgeAdjacencyList) * 2);

    _EdgeAdjacencyList[I].StartVertex := VA;
    _EdgeAdjacencyList[I].EndVertex := VB;
    _EdgeAdjacencyList[I].FaceA := TriID;
  End;
Begin
  _EdgeAdjacencyCount := 0;
  SetLength(_EdgeAdjacencyList, 64);

  SetLength(_VertexAdjancencyList, _Target.VertexCount);
  SetLength(_VertexSpatialHashes, _Target.VertexCount);
  For I:=0 To Pred(_Target.VertexCount) Do
  Begin
    _VertexAdjancencyList[I].TriangleList.Clear();
    _VertexAdjancencyList[I].EdgeList.Clear();

    _Target.Vertices.GetVector3D(I, vertexPosition, P);
    V[0] := Trunc(P.X*SnapFactor);
    V[1] := Trunc(P.Y*SnapFactor);
    V[2] := Trunc(P.Z*SnapFactor);

    _VertexSpatialHashes[I] := GetCRC32(@V[0], SizeOf(Integer)*3);
  End;

  For I:=0 To Pred(_Target.TriangleCount) Do
    For J:=0 To 2 Do
    Begin
      T := _Target.GetTriangle(I);
      MakeEdge(I, T.Indices[J], T.Indices[(J+1) Mod 3]);

      _VertexAdjancencyList[T.Indices[J]].TriangleList.Add(I);
    End;

  For I:=0 To Pred(_EdgeAdjacencyCount) Do
  Begin
    _VertexAdjancencyList[_EdgeAdjacencyList[I].StartVertex].EdgeList.Add(I);
    _VertexAdjancencyList[_EdgeAdjacencyList[I].EndVertex].EdgeList.Add(I);
  End;
End;

Constructor MeshGroupTopology.Create(Target: MeshGroup);
Begin
  _Target := Target;
  Self.CalculateTopology();
  Self.FindEdgeLoops();
End;

Procedure MeshGroupTopology.CalculateTopology();
Begin
  Self.CalculateAdjacency();
End;

Function MeshGroupTopology.GetEdge(Const StartVertex, EndVertex:Integer):Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_EdgeAdjacencyCount) Do
  If ((_EdgeAdjacencyList[I].StartVertex = StartVertex) And (_EdgeAdjacencyList[I].EndVertex = EndVertex))
  Or ((_EdgeAdjacencyList[I].StartVertex = EndVertex) And (_EdgeAdjacencyList[I].EndVertex = StartVertex)) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := -1;
End;

Procedure MeshGroupTopology.GetVertexAdjancency(Const VertexIndex:Integer; Out Result:IntegerArrayObject);
Var
  J, I, Edge:Integer;
Begin
  FillChar(Result, SizeOf(Result), 0);

  For J:=0 To Pred(_Target.Vertices.Count) Do
  If (J = VertexIndex) Or (_VertexSpatialHashes[J] = _VertexSpatialHashes[VertexIndex]) Then
  Begin
    For I:=0 To Pred(_EdgeAdjacencyCount) Do
    If (_EdgeAdjacencyList[I].StartVertex = J) Then
    Begin
      Result.Add(_EdgeAdjacencyList[I].EndVertex);
    End Else
    If (_EdgeAdjacencyList[I].EndVertex = J) Then
    Begin
      Result.Add(_EdgeAdjacencyList[I].StartVertex);
    End;
  End;
End;

Procedure MeshGroupTopology.FindEdgeLoops;
Var
  I,J,K:Integer;
  Mark:Array Of Boolean;
  Group:MeshGroup;
  Current:MeshEdgeLoop;

(*  Function WalkEdgeLoop(EdgeIndex:Integer):Boolean;
  Var
    I, N, VertexIndex:Integer;
    SideA, SideB:Integer;
  Begin
    Result := False;
    Mark[EdgeIndex] := True;

    VertexIndex := _EdgeAdjacencyList[EdgeIndex].EndVertex;
    SideA := _EdgeAdjacencyList[EdgeIndex].FaceA;
    SideB := _EdgeAdjacencyList[EdgeIndex].FaceB;

    For I:=0 To Pred(_VertexAdjancencyList[VertexIndex].TriangleList.Count) Do
    Begin
      N := _VertexAdjancencyList[VertexIndex].TriangleList.Items[I];
      If (N = SideA) Or (N = SideB) Then
      Begin
        N := -1;
      End Else
        Break;
    End;

    If N<0 Then
      Exit;

    Current.Add(VertexIndex);

    If Not Mark[N] Then
      WalkEdgeLoop(N);

    Result := True;
  End;*)

Var
  Count:Integer;
Begin
(*  SetLength(Mark, _EdgeAdjacencyCount);
  For J:=0 To Pred(_EdgeAdjacencyCount) Do
    Mark[J] := False;

  Current := MeshEdgeLoop.Create(_Target);

  _EdgeLoopCount := 0;
  Repeat
    Count := _EdgeAdjacencyCount;

    For I:=0 To Pred(Count) Do
      If (Not Mark[I]) Then
      Begin
        If (WalkEdgeLoop(I)) Then
        Begin
          Inc(_EdgeLoopCount);
          SetLength(_EdgeLoops, _EdgeLoopCount);
          _EdgeLoops[Pred(_EdgeLoopCount)]:= Current;
          Current := MeshEdgeLoop.Create(_Target);
          Break;
        End Else
          Current.Clear();
      End Else
        Dec(Count);
  Until (Count<=0);*)

  IntToString(_EdgeLoopCount);
End;

Function MeshGroupTopology.GetEdgeLoop(Index: Integer): MeshEdgeLoop;
Begin
  If (Index<0) Or (Index>=_EdgeLoopCount) Then
    Result := Nil
  Else
    Result := _EdgeLoops[Index];
End;

Procedure MeshGroupTopology.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_EdgeLoopCount) Do
    ReleaseObject(_EdgeLoops[I]);
  _EdgeLoopCount := 0;
End;


{-$DEFINE LAPLACIAN_SMOOTH}
Procedure MeshGroupTopology.Smooth();
Var
  O_V, O_N:Array Of Vector3D;

Procedure SmoothPass();
Var
  I, J, Count:Integer;
  P, V, N, DV, DN:Vector3D;
  Beta, SourceWeight, AdjWeight:Single;
  VertexAdjancency:IntegerArrayObject;
Begin
  For I:=0 To Pred(_Target.Vertices.Count) Do
  Begin
    //Disps[I] := VectorZero;

    Self.GetVertexAdjancency(I, VertexAdjancency);
    DV := VectorZero;
    DN := VectorZero;

    _Target.Vertices.GetVector3D(I, vertexPosition, V);
    _Target.Vertices.GetVector3D(I, vertexNormal, N);

    {$IFDEF LAPLACIAN_SMOOTH}
    For J:=0 To Pred(VertexAdjancency.Count) Do
    Begin
      DV.Add(VectorSubtract(O_V[VertexAdjancency.Items[J]], V));
      DN.Add(VectorSubtract(O_N[VertexAdjancency.Items[J]], N));
    End;

    DV.Scale((1/VertexAdjancency.Count) * Delta);
    DN.Scale((1/VertexAdjancency.Count) * Delta);

    {$ELSE}
    If (VertexAdjancency.Count = 3) Then
      Beta := 3.0/16.0
    Else
      Beta := 3.0/(8.0*VertexAdjancency.Count);

    SourceWeight := 1.0 - VertexAdjancency.Count * Beta;
    AdjWeight := Beta;

    For J:=0 To Pred(VertexAdjancency.Count) Do
    Begin
      DV.Add(O_V[VertexAdjancency.Items[J]]);
      DN.Add(O_N[VertexAdjancency.Items[J]]);
    End;

    DV.Scale(AdjWeight);
    DN.Scale(AdjWeight);

    V.Scale(SourceWeight);
    N.Scale(SourceWeight);
    {$ENDIF}

    V.Add(DV);
    N.Add(DN);
    N.Normalize();
    _Target.Vertices.SetVector3D(I, vertexPosition, V);
    _Target.Vertices.SetVector3D(I, vertexNormal, N);
  End;
End;

Var
  I:Integer;
Begin
  If (_Target.TriangleCount<=0) Then
    Exit;

  SetLength(O_V, _Target.Vertices.Count);
  SetLength(O_N, _Target.Vertices.Count);
  For I:=0 To Pred(_Target.Vertices.Count) Do
  Begin
    _Target.Vertices.GetVector3D(I, vertexPosition, O_V[I]);
    _Target.Vertices.GetVector3D(I, vertexNormal, O_N[I]);
  End;

  SmoothPass();

  SetLength(O_V, 0);
  SetLength(O_N, 0);
End;

Procedure MeshGroupTopology.SubDivide6();
Var
  I, J, K, OldVertexCount:Integer;
  OldTriangles:Array Of Triangle;
  OldTriangleCount:Integer;

  EdgePoints:Array Of Integer;
  EdgeInsert:Integer;
  EdgeIndicesA, EdgeIndicesB, EdgeIndicesC:Array Of Integer;

  Indices:Array[0..6] Of Integer;

  T:Triangle;

  Function DivideEdge(TriIndex, EdgeIndex:Integer):Integer;
  Var
    Tri:Triangle;
    Edge:Integer;
    UseA, UseB, UseC:Boolean;
  Begin
    Tri := _Target.GetTriangle(TriIndex);
    Edge := Self.GetEdge(Tri.Indices[EdgeIndex], Tri.Indices[(EdgeIndex+1) Mod 3]);

    Result := EdgePoints[Edge];
    If Result>=0 Then
    Begin
      IntToString(2);
      Exit;
    End;

    Case EdgeIndex Of
    0:Begin
        UseA := True;
        UseB := True;
        UseC := False;
      End;

    1:Begin
        UseA := False;
        UseB := True;
        UseC := True;
      End;

    Else
      Begin
        UseA := True;
        UseB := False;
        UseC := True;
      End;
    End;

    Result := _Target.SubDivideVertexFromTriangle(I, OldVertexCount + _Target.TriangleCount + EdgeInsert, UseA, UseB, UseC);
    Inc(EdgeInsert);
    EdgePoints[Edge] := Result;
   End;
Begin
  If (_Target.TriangleCount<=0) Then
    Exit;

  OldVertexCount := Self._Target.Vertices.Count;
  Self._Target.Vertices.Resize(OldVertexCount + _Target.TriangleCount + _EdgeAdjacencyCount); // we need to add a new vertex for each triangle

  EdgeInsert := 0;
  SetLength(EdgePoints, _EdgeAdjacencyCount);
  For I:=0 To Pred(_EdgeAdjacencyCount) Do
    EdgePoints[I] := -1;

  SetLength(EdgeIndicesA, _Target.TriangleCount);
  SetLength(EdgeIndicesB, _Target.TriangleCount);
  SetLength(EdgeIndicesC, _Target.TriangleCount);
  For I:=0 To Pred(_Target.TriangleCount) Do
  Begin
    _Target.SubDivideVertexFromTriangle(I, OldVertexCount + I, True, True, True);
    EdgeIndicesA[I] := DivideEdge(I, 0);
    EdgeIndicesB[I] := DivideEdge(I, 1);
    EdgeIndicesC[I] := DivideEdge(I, 2);
  End;

  If (vertexFormatTangent In _Target.Vertices.Format) Then
    _Target.CalculateTangents();

  SetLength(OldTriangles, _Target.TriangleCount);
  For I:=0 To Pred(_Target.TriangleCount) Do
    OldTriangles[I] := _Target.GetTriangle(I);

  OldTriangleCount := _Target.TriangleCount;

  _Target.TriangleCount := _Target.TriangleCount * 6;

  I:=0;

  While (I<OldTriangleCount) Do
  Begin
    Indices[0] := OldTriangles[I].Indices[0];
    Indices[1] := EdgeIndicesA[I];
    Indices[2] := OldTriangles[I].Indices[1];
    Indices[3] := EdgeIndicesB[I];
    Indices[4] := OldTriangles[I].Indices[2];
    Indices[5] := EdgeIndicesC[I];
    Indices[6] := OldVertexCount + I ;

    For J:=0 To 5 Do
    Begin
      T.Indices[0] := Indices[J];
      T.Indices[1] := Indices[(J+1) Mod 6];
      T.Indices[2] := Indices[6];

      _Target.SetTriangle(T, I*6 + J);
    End;

    Inc(I);
  End;

  _Target.CalculateTriangleNormals();
  _Target.ReleaseBuffer();

  Self.CalculateTopology();
End;

Procedure MeshGroupTopology.SubDivide3();
Var
  I, J, K, OldVertexCount:Integer;
  OldTriangles:Array Of Triangle;
  OldTriangleCount:Integer;
  T:Triangle;
Begin
  If (_Target.TriangleCount<=0) Then
    Exit;

  OldVertexCount := Self._Target.Vertices.Count;
  Self._Target.Vertices.Resize(OldVertexCount + Self._Target.TriangleCount); // we need to add a new vertex for each triangle

  For I:=0 To Pred(_Target.TriangleCount) Do
  Begin
    _Target.SubDivideVertexFromTriangle(I, OldVertexCount + I, True, True, True);
  End;

  If (vertexFormatTangent In _Target.Vertices.Format) Then
    _Target.CalculateTangents();

  SetLength(OldTriangles, _Target.TriangleCount);
  For I:=0 To Pred(_Target.TriangleCount) Do
    OldTriangles[I] := _Target.GetTriangle(I);

  OldTriangleCount := _Target.TriangleCount;

  _Target.TriangleCount := _Target.TriangleCount * 3;

  I:=0;

  While (I<OldTriangleCount) Do
  Begin
    For J:=0 To 2 Do
    Begin
      T.Indices[0] := OldTriangles[I].Indices[J];
      T.Indices[1] := OldTriangles[I].Indices[(J+1)Mod 3];
      T.Indices[2] := OldVertexCount + I;

      _Target.SetTriangle(T, I*3 + J);
    End;

    Inc(I);
  End;

  _Target.CalculateTriangleNormals();
  _Target.ReleaseBuffer();

  Self.CalculateTopology();
End;

{ MeshEdgeLoop }
Constructor MeshEdgeLoop.Create(Target: MeshGroup);
Begin
  _Target := Target;
End;

Procedure MeshEdgeLoop.Add(VertexIndex: Integer);
Begin
  Inc(_IndexCount);
  
  If Length(_Indices) < _IndexCount Then
    SetLength(_Indices, _IndexCount);

  _Indices[Pred(_IndexCount)] := VertexIndex;
End;

Procedure MeshEdgeLoop.Clear;
Begin
  _IndexCount := 0;
End;

End.
