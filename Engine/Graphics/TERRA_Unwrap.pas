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
 * TERRA_Unwrap
 * Implements a mesh UV unwrapper
 ***********************************************************************************************************************
}
Unit TERRA_Unwrap;
{$I terra.inc}

{
  How to use the unwrapper:
  Do the following for each group in a mesh
  1 - Pass all triangles with AddTriangle
  2 - Pass adjacency information (optional)
  3 - Call Unwrap() method
  4 - The unwrapper may change the mesh topology, so you need to delete all triangles/vertices from the current mesh
  5 - Copy the new geometry from the unwrapper with GetTriangle()
}

Interface

Uses TERRA_Utils, TERRA_Collections, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color,
  TERRA_Image, TERRA_Log, TERRA_MeshFilter, TERRA_Mesh;

Const
  PLANE_XPOSITIVE = 0;
  PLANE_XNEGATIVE = 1;
  PLANE_YPOSITIVE = 2;
  PLANE_YNEGATIVE = 3;
  PLANE_ZPOSITIVE = 4;
  PLANE_ZNEGATIVE = 5;

Type
  PUnwrapCluster = ^UnwrapCluster;

  UnwrapVertex = Record
    Vertex:MeshVertex;
    Cluster:PUnwrapCluster;
  End;

  UnwrapEdge = Record
    VA,VB:Integer;
    TA,TB:Integer;
  End;

  UnwrapTriangle2D = Record
    Vertex:Array[0..2] Of Vector2D;
  End;

  UnwrapCluster = Object
    Public
      Index:Integer;
      U1,V1:Single;
      U2,V2:Single;
      GroupID:Integer;

    Protected
      TriangleList:IntegerArray;
      Orientation:Integer;
      Normal:Vector3D;

      _Triangles:Array Of UnwrapTriangle2D;
      _TriangleCount:Integer;
      _Min, _Max:Vector2D;
      Width,Height:Integer;
      X,Y:Integer;
  End;

  PUnwrapTriangle = ^UnwrapTriangle;
  UnwrapTriangle = Object
    Public
      Cluster:PUnwrapCluster;
    Protected
      Index:Array[0..2] Of Integer;
      Normal:Vector3D;
      Orientation:Integer;
      Adjacency:IntegerArray;
  End;

  UnwrapGroup = Record
    _TriangleList:Array Of UnwrapTriangle;
    _TriangleCount:Integer;

    _VertexList:Array Of UnwrapVertex;
    _VertexCount:Integer;

    _EdgeTable:Array Of IntegerArray;
    _EdgeList:Array Of UnwrapEdge;
    _EdgeCount:Integer;
  End;

  Unwrapper = Class(TERRAObject)
    Protected
      _Groups:Array Of UnwrapGroup;
      _GroupCount:Integer;

      _Clusters:Array Of UnwrapCluster;
      _ClusterCount:Integer;

      _HasAdjacency:Boolean;

      _Target:Mesh;

      Procedure AddTriangle(GroupID, A, B, C:Integer; Normal:Vector3D);
      Function GetTriangle2D(GroupID, Index:Integer; Scale:Single):UnwrapTriangle2D;

      Procedure AddEdge(GroupID, A,B,Face:Integer);
      Function GetEdge(GroupID, A,B:Integer):Integer;

      Procedure BuildEdgeList(GroupID:Integer);
      Procedure BuildAdjancencyLists(GroupID:Integer);

      Procedure DuplicateVertex(Cluster:PUnwrapCluster; Index:Integer);

    Public
      Function Unwrap(Target:Mesh; Callback:ProgressNotifier=Nil):Boolean;

      Function GetTemplate(Size:Cardinal = 1024):Image;

      Function GetCluster(Index:Integer):PUnwrapCluster;
  End;

Implementation
Uses TERRA_Packer, TERRA_Math, TERRA_Rasterizer;

Procedure Unwrapper.AddTriangle(GroupID, A, B, C:Integer; Normal:Vector3D);
Var
  G:^UnwrapGroup;
  Index, Orientation:Integer;
Begin
  If (Abs(Normal.X)>Abs(Normal.Y)) And (Abs(Normal.X)>Abs(Normal.Z)) Then
  Begin
    If (Normal.X>0) Then
      Orientation := PLANE_XPOSITIVE
    Else
      Orientation := PLANE_XNEGATIVE;
  End Else
  If (Abs(Normal.Y)>Abs(Normal.X)) And (Abs(Normal.Y)>Abs(Normal.Z)) Then
  Begin
    If (Normal.Y>0) Then
      Orientation := PLANE_YPOSITIVE
    Else
      Orientation := PLANE_YNEGATIVE;
  End Else
  Begin
    If (Normal.Z>0) Then
      Orientation := PLANE_ZPOSITIVE
    Else
      Orientation := PLANE_ZNEGATIVE;
  End;

  G := @_Groups[GroupID];
  Index := G._TriangleCount;
  Inc(G._TriangleCount);

  G._TriangleList[Index].Normal := Normal;
  G._TriangleList[Index].Index[0] := A;
  G._TriangleList[Index].Index[1] := B;
  G._TriangleList[Index].Index[2] := C;
  G._TriangleList[Index].Orientation := Orientation;
  G._TriangleList[Index].Cluster := Nil;
  G._TriangleList[Index].Adjacency.Count := 0;
End;

Function Unwrapper.GetTriangle2D(GroupID, Index:Integer; Scale:Single):UnwrapTriangle2D;
Var
  I:Integer;
  P:Vector3D;
Begin
  For I:=0 To 2 Do
  Begin
    P := _Groups[GroupID]._VertexList[_Groups[GroupID]._TriangleList[Index].Index[I]].Vertex.Position;
    P := VectorScale(P, Scale);
    Case _Groups[GroupID]._TriangleList[Index].Orientation Of
    PLANE_XPOSITIVE,
    PLANE_XNEGATIVE:
      Begin
        Result.Vertex[I] := VectorCreate2D(P.Z, P.Y);
      End;

    PLANE_YPOSITIVE,
    PLANE_YNEGATIVE:
      Begin
        Result.Vertex[I] := VectorCreate2D(P.X, P.Z);
      End;

    PLANE_ZPOSITIVE,
    PLANE_ZNEGATIVE:
      Begin
        Result.Vertex[I] := VectorCreate2D(P.X, P.Y);
      End;
    End;
  End;
End;

Function Unwrapper.GetCluster(Index:Integer):PUnwrapCluster;
Begin
  Result := @(_Clusters[Index]);
End;

Procedure Unwrapper.AddEdge(GroupID, A,B,Face:Integer);
Var
  G:^UnwrapGroup;
  I,J:Integer;
Begin
  G := @_Groups[GroupID];

  If (A>B) Then
  Begin
    I := A;
    A := B;
    B := I;
  End;

  For I:=0 To Pred(G._EdgeTable[A].Count) Do
  If (G._EdgeList[G._EdgeTable[A].Items[I]].VB = B) Then
  Begin
    G._EdgeList[G._EdgeTable[A].Items[I]].TB := Face;
    Exit;
  End;

  J := G._EdgeCount;
  Inc(G._EdgeCount);
  SetLength(G._EdgeList, G._EdgeCount);
  G._EdgeList[J].VA := A;
  G._EdgeList[J].VB := B;
  G._EdgeList[J].TA := Face;
  G._EdgeList[J].TB := -1;

  G._EdgeTable[A].Add(J);
End;

Function Unwrapper.GetEdge(GroupID, A,B:Integer):Integer;
Var
  I:Integer;
  G:^UnwrapGroup;
Begin
  G := @_Groups[GroupID];

  If (A>B) Then
  Begin
    I := A;
    A := B;
    B := I;
  End;

  For I:=0 To Pred(G._EdgeTable[A].Count) Do
  If (G._EdgeList[G._EdgeTable[A].Items[I]].VB = B) Then
  Begin
    Result := G._EdgeTable[A].Items[I];
    Exit;
  End;

  Result := -1;
  Log(logError,'Unwrap', 'edge not found!');
End;

Procedure Unwrapper.BuildEdgeList(GroupID:Integer);
Var
  I,J,K,W,Count:Integer;
  G:^UnwrapGroup;
Begin
  G := @_Groups[GroupID];
  SetLength(G._EdgeTable, G._VertexCount);
  G._EdgeCount := 0;

  For I:=0 To Pred(G._TriangleCount) Do
    For J:=0 To 2 Do
    Begin
      K := Succ(J) Mod 3;
      AddEdge(GroupID, G._TriangleList[I].Index[J], G._TriangleList[I].Index[K], I);
    End;

End;

Procedure Unwrapper.BuildAdjancencyLists(GroupID:Integer);
Var
  I,J,K,N:Integer;
  G:^UnwrapGroup;
  Adj:Integer;
Begin
  BuildEdgeList(GroupID);

  G := @_Groups[GroupID];
  For I:=0 To Pred(G._TriangleCount) Do
    For J:=0 To 2 Do
    Begin
      K := Succ(J) Mod 3;
      N := GetEdge(GroupID, G._TriangleList[I].Index[J], G._TriangleList[I].Index[K]);
      If (G._EdgeList[N].TA = I) Then
        Adj := G._EdgeList[N].TB
      Else
        Adj := G._EdgeList[N].TA;

      If (Adj>=0) Then
      Begin
        G._TriangleList[I].Adjacency.Add(Adj);
        G._TriangleList[Adj].Adjacency.Add(I);
      End;

    End;
End;

Function Unwrapper.Unwrap(Target:Mesh; Callback:ProgressNotifier=Nil):Boolean;
Var
  I,J,K,W,N:Integer;
  Queue:IntegerArray;
  Cluster:^UnwrapCluster;
  Found:Boolean;
  Count:Integer;

  Scale:Single;
  MW,MH:Integer;
  U,V:Single;
  Packer:RectanglePacker;

  Group:MeshGroup;
  G:^UnwrapGroup;
  T:Triangle;
  Normal:Vector3D;
  P1,P2,P3:Vector3D;
Begin
  _Target := Target;
  _ClusterCount := 0;

  _GroupCount := Target.GroupCount;
  SetLength(_Groups, _GroupCount);
  For N:=0 To Pred(_GroupCount) Do
  Begin
    Group := Target.GetGroup(N);

    _Groups[N]._VertexCount := Group.VertexCount;
    SetLength(_Groups[N]._VertexList, Group.VertexCount);
    For J:=0 To Pred(Group.VertexCount) Do
    Begin
      _Groups[N]._VertexList[J].Vertex := Group.GetVertex(J);
      _Groups[N]._VertexList[J].Cluster := Nil;
    End;

    _Groups[N]._TriangleCount := 0;
    SetLength(_Groups[N]._TriangleList, Group.TriangleCount);

    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTriangle(J);
      P1 := Group.GetVertex(T.Indices[0]).Position;
      P2 := Group.GetVertex(T.Indices[1]).Position;
      P3 := Group.GetVertex(T.Indices[2]).Position;
      Normal := Group.GetTriangleNormal(J);
      AddTriangle(N, T.Indices[0], T.Indices[1], T.Indices[2], Normal);
    End;

    BuildAdjancencyLists(N);

    Queue.Clear;
    For I:=0 To Pred(_Groups[N]._TriangleCount) Do
      Queue.Add(I, True);

    While Queue.Count > 0 Do
    Begin
      If Assigned(Callback) Then
        Callback.Notify(Queue.Count/_Groups[N]._TriangleCount);

      // get first triangle in queue
      K := Queue.Pop;

      // create a new cluster
      Inc(_ClusterCount);
      SetLength(_Clusters, _ClusterCount);
      Cluster := @(_Clusters[Pred(_ClusterCount)]);
      Cluster.TriangleList.Add(K);
      Cluster.GroupID := N;
      Cluster.Orientation := _Groups[N]._TriangleList[K].Orientation;
      Cluster.Normal := _Groups[N]._TriangleList[K].Normal;

      Repeat
        Count := 0;
        I:=0;
        // for every triangle still in queue
        While (I<Queue.Count) Do
        Begin
          Found := False;
          // test if it is adjacent with any triangle in current cluster
          // it also needs similar orientation
          For J:=0 To Pred(Cluster.TriangleList.Count) Do
            If (_Groups[N]._TriangleList[Queue.Items[I]].Orientation = Cluster.Orientation)
  //          Or (Abs(VectorDot(_TriangleList[Queue.Items[I]].Normal, Cluster.Normal))>=0.7))
            And (_Groups[N]._TriangleList[Queue.Items[I]].Adjacency.Contains(Cluster.TriangleList.Items[J])) Then
            Begin
              // is adjacent, move from queue to cluster
              Cluster.TriangleList.Add(Queue.Items[I]);
              Queue.Remove(Queue.Items[I]);
              Found := True;
              Break;
            End;

          If Found Then
            Inc(Count)
          Else
            Inc(I);
        End;
      Until (Count<=0);
    End;
  End;

  MW := 0;
  MH := 0;
  // convert cluster triangles to 2D space
  For I:=0 To Pred(_ClusterCount) Do
  Begin
    Cluster := @(_Clusters[I]);
    Cluster.Index := I;
    Cluster._TriangleCount := Cluster.TriangleList.Count;
    SetLength(Cluster._Triangles, Cluster._TriangleCount);

    Scale := 1.0;
    Repeat
      For J:=0 To Pred(Cluster.TriangleList.Count) Do
        Cluster._Triangles[J] := GetTriangle2D(Cluster.GroupID, Cluster.TriangleList.Items[J], Scale);

      Cluster._Min := VectorCreate2D(9999, 9999);
      Cluster._Max := VectorCreate2D(-9999, -9999);
      For J:=0 To Pred(Cluster.TriangleList.Count) Do
        For K := 0 To 2 Do
        Begin
          Cluster._Min.X := FloatMin(Cluster._Min.X, Cluster._Triangles[J].Vertex[K].X);
          Cluster._Max.X := FloatMax(Cluster._Max.X, Cluster._Triangles[J].Vertex[K].X);
          Cluster._Min.Y := FloatMin(Cluster._Min.Y, Cluster._Triangles[J].Vertex[K].Y);
          Cluster._Max.Y := FloatMax(Cluster._Max.Y, Cluster._Triangles[J].Vertex[K].Y);
        End;

      Cluster.Width := Round(Cluster._Max.X - Cluster._Min.X) + 1;
      Cluster.Height := Round(Cluster._Max.Y - Cluster._Min.Y) + 1;
      Scale := Scale * 2.0;
    Until (Cluster.Width>0) And (Cluster.Height>0);

    If (Cluster.Width>MW) Then
      MW := Cluster.Width;
    If (Cluster.Height>MH) Then
      MH := Cluster.Height;
  End;

  Found := False;
  Repeat
    Packer := RectanglePacker.Create;
    For I:=0 To Pred(_ClusterCount) Do
      Packer.AddRect(_Clusters[I].Width, _Clusters[I].Height, I);
    // try to pack all clusters
    Count := Packer.Pack(MW, MH, Callback);
    If (Count>0) Then
    Begin
      // failed, increase space, to try again
      If (MW<=MH) Then
        MW := Trunc(MW * 1.5)
      Else
        MH := Trunc(MH * 1.5);
      ReleaseObject(Packer);
    End Else
      Found := True;
  Until Found;

  // get packed position for each cluster
  For I:=0 To Pred(_ClusterCount) Do
    Packer.GetRect(I, _Clusters[I].X, _Clusters[I].Y);

  // calculate UVs
  For I:=0 To Pred(_ClusterCount) Do
  Begin
    Cluster := @(_Clusters[I]);
    G := @_Groups[Cluster.GroupID];

    Cluster.U1 := Cluster._Min.X / MW;
    Cluster.V1 := Cluster._Min.Y / MH;
    Cluster.U2 := Cluster._Max.X / MW;
    Cluster.V2 := Cluster._Max.Y / MH;

    For J:=0 To Pred(Cluster._TriangleCount) Do
    Begin
      G._TriangleList[Cluster.TriangleList.Items[J]].Cluster := Cluster;
      For K := 0 To 2 Do
      Begin
        // convert UVs to cluster space
        U := (Cluster._Triangles[J].Vertex[K].X - Cluster._Min.X) + Cluster.X;
        V := (Cluster._Triangles[J].Vertex[K].Y - Cluster._Min.Y) + Cluster.Y;

        // convert to texture space
        U := U / MW;
        V := V / MH;

        If (G._VertexList[G._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Cluster<>Cluster)
        And (G._VertexList[G._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Cluster<>Nil) Then
        Begin // this vertex is already being used in another cluster, duplicate it
          DuplicateVertex(Cluster, G._TriangleList[Cluster.TriangleList.Items[J]].Index[K]);
        End;

        // store UVs (packed in normal, because normals are not used during lightmapping)
        G._VertexList[G._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Vertex.Normal.X := U;
        G._VertexList[G._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Vertex.Normal.Y := V;
      End;
    End;
  End;
  ReleaseObject(Packer);

  // final step, copy geometry back to the mesh, because vertices might have been duplicated
  For K:=0 To Pred(Self._GroupCount) Do
  Begin
    Group := _Target.GetGroup(K);

    Group.VertexCount := _Groups[K]._VertexCount;
    Group.TriangleCount := _Groups[K]._TriangleCount;
    For J:=0 To Pred(_Groups[K]._VertexCount) Do
      Group.Vertices[J] := _Groups[K]._VertexList[J].Vertex;

    For J:=0 To Pred(_Groups[K]._TriangleCount) Do
    Begin
      For I:=0 To 2 Do
        T.Indices[I] := _Groups[K]._TriangleList[J].Index[I];
      Group.Triangles[J] := T;
    End;
  End;
End;

Procedure Unwrapper.DuplicateVertex(Cluster:PUnwrapCluster; Index:Integer);
Var
  N, I, J:Integer;
Begin
  N := _Groups[Cluster.GroupID]._VertexCount;
  Inc(_Groups[Cluster.GroupID]._VertexCount);
  SetLength(_Groups[Cluster.GroupID]._VertexList, _Groups[Cluster.GroupID]._VertexCount);
  _Groups[Cluster.GroupID]._VertexList[N].Vertex := _Groups[Cluster.GroupID]._VertexList[Index].Vertex;
  _Groups[Cluster.GroupID]._VertexList[N].Cluster := Cluster;
  For I:=0 To Pred(Cluster.TriangleList.Count) Do
    For J:=0 To 2 Do
    If (_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[I]].Index[J] = Index) Then
    Begin
      _Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[I]].Index[J] := N;
    End;
End;

Function Unwrapper.GetTemplate(Size:Cardinal): Image;
Var
  Cluster:^UnwrapCluster;
  Color:TERRA_Color.Color;
  I,J,K,W:Integer;
  Rasterizer:ColorRasterizer;
Begin
  Result := Image.Create(Size, Size);
  Rasterizer := ColorRasterizer.Create;
  Rasterizer.Target := Result;

  For I:=0 To Pred(_ClusterCount) Do
  Begin
    Cluster := @(_Clusters[I]);

    Case Cluster.Orientation Of
      PLANE_XPOSITIVE:  Color := ColorCreate(255,0,0);
      PLANE_XNEGATIVE:  Color := ColorCreate(255,0,255);
      PLANE_YPOSITIVE:  Color := ColorCreate(0,255,0);
      PLANE_YNEGATIVE:  Color := ColorCreate(255,255,0);
      PLANE_ZPOSITIVE:  Color := ColorCreate(0,0,255);
      PLANE_ZNEGATIVE:  Color := ColorCreate(0,255,255);
    End;

    Rasterizer.FillColor := Color;

    For J:=0 To Pred(Cluster._TriangleCount) Do
    Begin
        Rasterizer.SetInterpolatorValues(rasterX,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[0]].Vertex.Normal.X,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[1]].Vertex.Normal.X,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[2]].Vertex.Normal.X);

        Rasterizer.SetInterpolatorValues(rasterY,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[0]].Vertex.Normal.Y,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[1]].Vertex.Normal.Y,
            _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[2]].Vertex.Normal.Y);

        Rasterizer.Rasterize();

      For K := 0 To 2 Do
      Begin
        W := Succ(K) Mod 3;
        Result.LineByUV(
          _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Vertex.Normal.X,
          _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[K]].Vertex.Normal.Y,
          _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[W]].Vertex.Normal.X,
          _Groups[Cluster.GroupID]._VertexList[_Groups[Cluster.GroupID]._TriangleList[Cluster.TriangleList.Items[J]].Index[W]].Vertex.Normal.Y, ColorWhite);
        End;

      End;
  End;

  ReleaseObject(Rasterizer);
End;

End.