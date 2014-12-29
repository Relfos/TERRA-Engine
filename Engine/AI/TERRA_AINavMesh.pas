Unit TERRA_AINavMesh;

{$I terra.inc}

Interface
Uses TERRA_AIPath, TERRA_Utils, TERRA_Mesh, TERRA_Vector3D, TERRA_Vector2D, TERRA_Ray,
  TERRA_MeshFilter, TERRA_Math, TERRA_Matrix, TERRA_Color, TERRA_BoundingBox,
  TERRA_Collision2D, TERRA_Octree
  {$IFDEF PC}, TERRA_DebugDraw{$ENDIF};

Const
  navMeshGround     = 0;
  navMeshBoundaries = 1;
  DefaultWallOffset = 100;
  SprayDistance = 0.15;

Type
  VectorDequeue = Object
    Values:Array Of Vector3D;
    Size:Integer;

    Constructor Create;
    Procedure Push_Back(P:Vector3D);
    Function Back:Vector3D;
    Function Front:Vector3D;
  End;

  NavMeshGeometryBuffer = Object
    _Vertices:Array Of Vector3D;
    _VertexCount:Integer;
    _Triangles:Array Of Triangle;
    _TriangleCount:Integer;

    Function AddVertex(P:Vector3D):Integer;
    Procedure AddTriangle(A,B,C:Integer);
  End;

  NavMeshEdge = Record
    A,B:Integer;
    TA, TB:Integer;
    Count:Integer;
  End;

  NavMeshAdjacency = Record
    List:Array[0..2] Of Integer;
    Edges:Array[0..2] Of Integer;
  End;

  PNavMeshNodeInfo = ^NavMeshNodeInfo;
  NavMeshNodeInfo = Record
    Triangle:Integer;
    ID:Integer;
    Previous:PNavMeshNodeInfo;
    Cost:Integer;
    State:PathNodeState;
  End;

  NavMesh = Class;

  NavMeshOctreeElement = Class(OctreeElement)
    Public
      TriangleID:Integer;
      BufferID:Integer;
      Mesh:NavMesh;

      Function Intersect(Const R:Ray; Var T:Single):Boolean; Override;
  End;

  NavMesh = Class(MeshFilter)
    Protected
      _Buffers:Array[0..1] Of NavMeshGeometryBuffer;
      _BoundingBoxes:Array[0..1] Of BoundingBox;
      _Octrees:Array[0..1] Of Octree;
      _EdgeList:Array Of NavMeshEdge;
      _Visited:Array Of  Boolean;
      _EdgeCount:Integer;
      _Adjacency:Array Of NavMeshAdjacency;
      _Centers:Array Of Vector3D;
      _Walls:Array Of Line2D;
      _WallCount:Integer;

      _Rays:Array Of Ray;
      _RaysColor:Array Of Color;
      _RayCount:Integer;
      _RayActive:Boolean;

      _CurrentTriangle:Integer;
      _StartTriangle:Integer;
      _EndTriangle:Integer;

      _WallOffset:Single;

      _Left, _Right, _Apex:VectorDequeue;

      _LastPath:Path3D;

      Procedure BuildGeometry(SourceMesh:Mesh);
      Procedure AddWall(P1,P2:Vector3D);

      Function GetCost(Triangle:Integer):Integer; Virtual;
      Procedure VisitNode(Triangle:Integer); Virtual;

      Function GetTriangleHit(P:Vector3D):Integer;

      Procedure CreatePath(Node:PNavMeshNodeInfo; Var Path:Path3D; Start, Goal:Vector3D);

      Procedure AddAdjacency(Index:Integer);
      Procedure GetCommonEdge(A,B:Integer; Var PA, PB:Vector3D);

      Function CreateOctree(Index:Integer):Octree;

    Public

      Constructor Create(SourceMesh:Mesh; WallOffset:Single = DefaultWallOffset);
      Destructor Destroy;

      Function Search(StartPos, EndPos:Vector3D;
                      Var Path:Path3D;
                      Flags:Integer=sfSearchTimeOut):PathSearchResult;

      Function GetHeightAt(P:Vector3D; Var Height:Single):Boolean;

      Procedure Render;

      Function Intersect(R:Ray; Var T:Single; GroupID:Integer):Boolean;

      Procedure Steer(Var Source:Circle);

      Function GetGroupCount: Integer; Override;

      Function IsPathClear(Start,Goal:Vector3D):Boolean;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
  End;

Implementation
Uses TERRA_OS, TERRA_Application, TERRA_Log, TERRA_ResourceManager, TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

{ NavMesh }
Function NavMeshGeometryBuffer.AddVertex(P: Vector3D): Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_VertexCount) Do
  If (_Vertices[I].Distance(P)<0.001) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := _VertexCount;
  Inc(_VertexCount);
  SetLength(_Vertices, _VertexCount);
  _Vertices[Pred(_VertexCount)] := P;
End;

Procedure NavMeshGeometryBuffer.AddTriangle(A,B,C:Integer);
Begin
  Inc(_TriangleCount);
  SetLength(_Triangles, _TriangleCount);
  _Triangles[Pred(_TriangleCount)].Indices[0] := A;
  _Triangles[Pred(_TriangleCount)].Indices[1] := B;
  _Triangles[Pred(_TriangleCount)].Indices[2] := C;
End;

Procedure NavMesh.AddWall(P1,P2: Vector3D);
Var
  A,B,C,D, K:Integer;
  VA, VB, VC, VD:Vector3D;
  Ofs, Ofs2:Vector3D;
Begin
  Ofs := VectorCreate(0, _WallOffset, 0);
  Ofs2 := VectorCreate(0, 0, 0);
  VA := VectorAdd(P1, Ofs2);
  VB := VectorAdd(P2, Ofs2);
  VC := VectorAdd(P2, Ofs);
  VD := VectorAdd(P1, Ofs);

  A := _Buffers[1].AddVertex(VA);
  B := _Buffers[1].AddVertex(VB);
  C := _Buffers[1].AddVertex(VC);
  D := _Buffers[1].AddVertex(VD);

  _BoundingBoxes[1].Add(VA);
  _BoundingBoxes[1].Add(VB);
  _BoundingBoxes[1].Add(VC);
  _BoundingBoxes[1].Add(VD);

  _Buffers[1].AddTriangle(C, B, A);
  _Buffers[1].AddTriangle(A, D, C);

  Inc(_WallCount);
  SetLength(_Walls, _WallCount);
  _Walls[Pred(_WallCount)].P1.X := P1.X;
  _Walls[Pred(_WallCount)].P1.Y := P1.Z;
  _Walls[Pred(_WallCount)].P2.X := P2.X;
  _Walls[Pred(_WallCount)].P2.Y := P2.Z;
End;

Procedure NavMesh.AddAdjacency(Index:Integer);
Var
  AlreadyHas:Boolean;
  A,B:Integer;
  N, I:Integer;
Begin
  A := _EdgeList[Index].TA;
  B := _EdgeList[Index].TB;

  AlreadyHas := False;
  N := -1;
  For I:=0 To 2 Do
  If (_Adjacency[A].List[I] = B) Then
    AlreadyHas := True
  Else
  If (_Adjacency[A].List[I]<0) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) And (Not AlreadyHas) Then
    RaiseError('NavMesh: Cannot build adjacencies!');

  If (Not AlreadyHas) Then
  Begin
    _Adjacency[A].List[N] := B;
    _Adjacency[A].Edges[N] := Index;
  End;

  AlreadyHas := False;
  N := -1;
  For I:=0 To 2 Do
  If (_Adjacency[B].List[I] = A) Then
    AlreadyHas := True
  Else
  If (_Adjacency[B].List[I]<0) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) And (Not AlreadyHas) Then
    RaiseError('NavMesh: Cannot build adjacencies!');

  If (Not AlreadyHas) Then
  Begin
    _Adjacency[B].List[N] := A;
    _Adjacency[B].Edges[N] := Index;
  End;
End;


Procedure NavMesh.BuildGeometry(SourceMesh:Mesh);
Var
  A, B,C:Integer;
  VA, VB, VC:Vector3D;
  I,J,K:Integer;
  G:MeshGroup;
  T:Triangle;
  N:Vector3D;

  Procedure TestEdge(A,B, ID:Integer);
  Var
    I:Integer;
  Begin
    For I:=0 To Pred(_EdgeCount) Do
    If ((_EdgeList[I].A = A) And (_EdgeList[I].B = B))
    Or ((_EdgeList[I].A = B) And (_EdgeList[I].B = A)) Then
    Begin
      Inc(_EdgeList[I].Count);
      _EdgeList[I].TB := ID;
      Exit;
    End;

    Inc(_EdgeCount);
    SetLength(_EdgeList, _EdgeCount);
    _EdgeList[Pred(_EdgeCount)].A := A;
    _EdgeList[Pred(_EdgeCount)].B := B;
    _EdgeList[Pred(_EdgeCount)].TA := ID;
    _EdgeList[Pred(_EdgeCount)].TB := -1;
    _EdgeList[Pred(_EdgeCount)].Count := 1;
  End;
Begin
  _Buffers[0]._VertexCount := 0;
  _Buffers[0]._TriangleCount := 0;

  _Buffers[1]._VertexCount := 0;
  _Buffers[1]._TriangleCount := 0;

  _BoundingBoxes[0].Reset;
  _BoundingBoxes[1].Reset;

  For K:=0 To Pred(SourceMesh.GroupCount) Do
  Begin
    G := SourceMesh.GetGroup(K);
    For I:=0 To Pred(G.TriangleCount) Do
    Begin
      {N := G.GetTriangleNormal(I);
      If (Abs(N.Z)>Abs(N.Y)) Or (Abs(N.X)>Abs(N.Y)) Then
        Continue;         }

      T := G.GetTriangle(I);
      VA := G.GetVertex(T.Indices[0]).Position;
      VB := G.GetVertex(T.Indices[1]).Position;
      VC := G.GetVertex(T.Indices[2]).Position;

      A := _Buffers[0].AddVertex(VA);
      B := _Buffers[0].AddVertex(VB);
      C := _Buffers[0].AddVertex(VC);
      _Buffers[0].AddTriangle(A, B, C);

      _BoundingBoxes[0].Add(VA);
      _BoundingBoxes[0].Add(VB);
      _BoundingBoxes[0].Add(VC);
    End;
  End;

  SetLength(_Adjacency, _Buffers[0]._TriangleCount);
  SetLength(_Centers, _Buffers[0]._TriangleCount);
  SetLength(_Visited, _Buffers[0]._TriangleCount);
  _EdgeCount := 0;
  For I:=0 To Pred(_Buffers[0]._TriangleCount) Do
  Begin
    T := _Buffers[0]._Triangles[I];
    For J:=0 To 2 Do
      _Adjacency[I].List[J] := -1;

    TestEdge(T.Indices[0], T.Indices[1], I);
    TestEdge(T.Indices[1], T.Indices[2], I);
    TestEdge(T.Indices[2], T.Indices[0], I);

    _Centers[I] := VectorZero;
    _Centers[I].Add(_Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[0]]);
    _Centers[I].Add(_Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[1]]);
    _Centers[I].Add(_Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[2]]);
    _Centers[I].Scale(1/3);
  End;

  For I:=0 To Pred(_EdgeCount) Do
  Begin
    If (_EdgeList[I].Count=1) Then
      AddWall(_Buffers[0]._Vertices[_EdgeList[I].A], _Buffers[0]._Vertices[_EdgeList[I].B]);

    If (_EdgeList[I].TB<0) Then
      Continue;

    AddAdjacency(I);
  End;

  For I:=0 To 1 Do
    _Octrees[I] := Self.CreateOctree(I);
End;

Constructor NavMesh.Create(SourceMesh: Mesh; WallOffset:Single);
Begin
  _WallOffset := WallOffset;
  _Left.Size := 0;
  _Right.Size := 0;
  _Apex.Size := 0;
  _LastPath.Size := 0;
  MeshManager.Instance.PreFetch(SourceMesh);

  BuildGeometry(SourceMesh);
End;

Function NavMesh.GetGroupCount: Integer;
Begin
  Result := 2;
End;

Function NavMesh.GetHeightAt(P: Vector3D; Var Height:Single):Boolean;
Var
  R:Ray;
  T:Single;
Begin
  R.Origin := VectorCreate(P.X, 1000, P.Z);
  R.Direction := VectorCreate(0, -1, 0);
  If Self.Intersect(R, T, 0) Then
  Begin
    P := R.IntersectionPoint(T);
    Height := P.Y;
    Result := True;
  End Else
    Result := False;
End;

Function NavMesh.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Buffers[GroupID]._Triangles[Index];
End;

Function NavMesh.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _Buffers[GroupID]._TriangleCount;
End;

Function NavMesh.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Buffers[GroupID]._VertexCount;
End;


Function NavMesh.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Buffers[GroupID]._Vertices[Index];
End;

Function NavMesh.Intersect(R: Ray; var T: Single; GroupID: Integer): Boolean;
Var
  I:Integer;
  A,B,C:Vector3D;
  K, U, V:Single;
Begin
  If _RayActive Then
  Begin
    Inc(_RayCount);
    SetLength(_Rays, _RayCount);
    SetLength(_RaysColor, _RayCount);
    _Rays[Pred(_RayCount)] := R;
  End;
  T := 99999;

  Result := _Octrees[GroupID].Intersect(R, T);

  {Result := False;
  For I:=0 To Pred(_Buffers[GroupID]._TriangleCount) Do
  Begin
    A := _Buffers[GroupID]._Vertices[_Buffers[GroupID]._Triangles[I].A];
    B := _Buffers[GroupID]._Vertices[_Buffers[GroupID]._Triangles[I].B];
    C := _Buffers[GroupID]._Vertices[_Buffers[GroupID]._Triangles[I].C];
    If (R.TriangleIntersect(A, B, C, K, U, V)) And (K<T) Then
    Begin
      T := K;
      Result := True;
    End;
  End;                    }

  If (_RayActive) Then
  Begin
    If (Not Result) Then
      _RaysColor[Pred(_RayCount)] := ColorGreen
    Else
      _RaysColor[Pred(_RayCount)] := ColorRed;
  End;
End;

Procedure NavMesh.Render;
Var
  I,J,K:Integer;
  A,B,C:Vector3D;
Begin
  {$IFDEF PC}
  GraphicsManager.Instance.SetBlendMode(blendBlend);
  glDisable(GL_CULL_FACE);
  glLineWidth(3);
  glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
  For K:=0 To 1 Do
  Begin
    _Octrees[K].Render();
    
    For I:=0 To Pred(_Buffers[K]._TriangleCount) Do
    Begin
      If (K=0) Then
        GraphicsManager.Instance.EnableColorShader(ColorBlue, MatrixIdentity)
      Else
        GraphicsManager.Instance.EnableColorShader(ColorWhite, MatrixIdentity);

      glBegin(GL_TRIANGLES);

      A := _Buffers[K]._Vertices[_Buffers[K]._Triangles[I].Indices[0]];
      B := _Buffers[K]._Vertices[_Buffers[K]._Triangles[I].Indices[1]];
      C := _Buffers[K]._Vertices[_Buffers[K]._Triangles[I].Indices[2]];

      glVertex3f(A.X, A.Y, A.Z);
      glVertex3f(B.X, B.Y, B.Z);
      glVertex3f(C.X, C.Y, C.Z);
      glEnd();
    End;
  End;

  glPolygonMode( GL_FRONT_AND_BACK, GL_FILL);

  GraphicsManager.Instance.EnableColorShader(ColorYellow, MatrixIdentity);
  glPointSize(4);
  glBegin(GL_POINTS);
  For I:=0 To Pred(_Buffers[0]._TriangleCount) Do
  Begin
    A := _Centers[I];
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd();

  GraphicsManager.Instance.EnableColorShader(ColorWhite, MatrixIdentity);
  glBegin(GL_LINE_STRIP);
  For I:=0 To Pred(_LastPath.Size) Do
  Begin
    A := _LastPath.List[I];
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd();

  glLineWidth(3);

  GraphicsManager.Instance.EnableColorShader(ColorGreen, MatrixIdentity);
  glBegin(GL_LINE_STRIP);
  For I:=0 To Pred(_Right.Size) Do
  Begin
    A := _Right.Values[I];
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd();

  GraphicsManager.Instance.EnableColorShader(ColorYellow, MatrixIdentity);
  glBegin(GL_LINE_STRIP);
  For I:=0 To Pred(_Left.Size) Do
  Begin
    A := _Left.Values[I];
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd();

  glLineWidth(1);
  glEnable(GL_CULL_FACE);

  For I:=0 To Pred(_RayCount) Do
    DrawRay(_Rays[I], _RaysColor[I]);
  {$ENDIF}
End;

Function NavMesh.Search(StartPos, EndPos: Vector3D; var Path: Path3D; Flags: Integer): PathSearchResult;
Var
  NodeList:Array[0..Pred(MaxSearchNodes)] Of NavMeshNodeInfo;
  NodeCount:Integer;

Procedure AddNode(Triangle, Cost:Integer; Previous:PNavMeshNodeInfo);
Var
  I:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  If (NodeList[I].Triangle = Triangle) Then
    Exit;

  Inc(NodeCount);
  I := Pred(NodeCount);

  NodeList[I].Triangle := Triangle;
  NodeList[I].ID := I;
  NodeList[I].Cost := Cost;
  NodeList[I].Previous := Previous;
  NodeList[I].State := nsActive;

  VisitNode(Triangle);
End;

Procedure TestNode(Triangle:Integer; Previous:PNavMeshNodeInfo);
Var
  Cost:Integer;
Begin
  If Assigned(Previous) Then
    _CurrentTriangle := Previous.Triangle;

  Cost := GetCost(Triangle);
  If Cost<0 Then
    Exit;

  AddNode(Triangle, Cost, Previous);
End;

Var
  I,K,Min:Integer;
  StartTime:Cardinal;
  Node:PNavMeshNodeInfo;
Begin
  _RayCount := 0;
  _RayActive := True;

  If (IsPathClear(StartPos, EndPos)) Then
  Begin
    Path.Size := 2;
    SetLength(Path.List, 2);
    Path.List[0] := StartPos;
    Path.List[1] := EndPos;
    Result := srSearchComplete;
    _RayActive := False;
    Exit;
  End;

  _CurrentTriangle := GetTriangleHit(StartPos);
  _EndTriangle := GetTriangleHit(EndPos);

  If (_CurrentTriangle<0) Or (_EndTriangle<0) Then
  Begin
    Result := srSearchFailed;
    Exit;
  End;

  NodeCount := 0;

  _StartTriangle := _CurrentTriangle;
  AddNode(_StartTriangle, GetCost(_StartTriangle), Nil);
  StartTime := GetTime;

  Path.Size := 0;
  Path.Position := 0;
  SetLength(Path.List, 0);

  For I:=0 To Pred(_Buffers[0]._TriangleCount) Do
    _Visited[I] := False;

  Repeat
    K := -1;
    Min := 999999;
    For I:=0 To Pred(NodeCount) Do
    If (NodeList[I].State=nsActive)And(NodeList[I].Cost<Min) Then
    Begin
      K := I;
      Min:=NodeList[I].Cost;
    End;

    If (K=-1) Then
    Begin
      Result := srSearchFailed;
      Exit;
    End;

    If (NodeList[K].Triangle = _EndTriangle) Then
    Begin
      Result := srSearchComplete;
      CreatePath(@NodeList[K], Path, StartPos, EndPos);
      _RayActive := False;
      Exit;
    End;

    With NodeList[K] Do
    Begin
      Node:=@NodeList[K];

      For I:=0 To 2 Do
      If (_Adjacency[Node.Triangle].List[I]>=0) Then
        TestNode(_Adjacency[Node.Triangle].List[I], Node);

      Node.State:=nsDiscarded;
    End;

  Until (NodeCount>=MaxSearchNodes) Or ((Flags And sfSearchTimeout<>0) And (GetTime-StartTime >= PathSearchTimeOut));

  If (NodeCount>=MaxSearchNodes) Then
    Result := srSearchLimitReached
  Else
    Result := srSearchTimeOut;

  _RayActive := False;
End;

Function NavMesh.GetCost(Triangle:Integer): Integer;
Begin
  Result := Trunc(_Centers[_StartTriangle].Distance(_Centers[Triangle]));
End;

Function NavMesh.GetTriangleHit(P: Vector3D): Integer;
Var
  PP:Vector2D;
  I:Integer;
  Poly:Polygon2D;
Begin
  PP.X := P.X;
  PP.Y := P.Z;
  Poly.VertexCount := 3;
  SetLength(Poly.Vertices, 3);
  For I:=0 To Pred(_Buffers[0]._TriangleCount) Do
  Begin
    Poly.Vertices[0].X := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[0]].X;
    Poly.Vertices[0].Y := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[0]].Z;

    Poly.Vertices[1].X := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[1]].X;
    Poly.Vertices[1].Y := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[1]].Z;

    Poly.Vertices[2].X := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[2]].X;
    Poly.Vertices[2].Y := _Buffers[0]._Vertices[_Buffers[0]._Triangles[I].Indices[2]].Z;

    If (Poly.PointInside(PP)) Then
    Begin
      Result := I;
      Exit;
    End;
  End;

  Result := -1;
End;

Procedure NavMesh.VisitNode(Triangle:Integer);
Begin
  _Visited[Triangle] := True;
End;

Procedure NavMesh.GetCommonEdge(A,B:Integer; Var PA, PB:Vector3D);
Var
  I:Integer;
  FoundFirst:Boolean;
  P1, P2:Integer;
Begin
  For I:=0 To 2 Do
  If (_Adjacency[A].List[I] = B) Then
  Begin
    PA := _Buffers[0]._Vertices[_EdgeList[_Adjacency[A].Edges[I]].A];
    PB := _Buffers[0]._Vertices[_EdgeList[_Adjacency[A].Edges[I]].B];
    Exit;
  End;

  RaiseError('NavMesh: ERROR in adjacency!');
End;

{
1 x1 y1     a b c
1 x2 y2     d e f
1 x3 y3     g h i

	= aei+bfg+cdh-ceg-bdi-afh.
}
Function CCW(A,B,C:Vector3D):Boolean;
Var
  N:Single;
Begin
  N :=  (B.X*C.Y) + (A.X * B.Y) + (A.Y * C.X) - (A.Y*B.X) - (A.X*C.Y) - (B.Y*C.X);
  Result := N<0;
End;

Function CW(A,B,C:Vector3D):Boolean;
Begin
  Result := Not CCW(A,B,C);
End;

Constructor VectorDequeue.Create;
Begin
  Size := 0;
End;

Function VectorDequeue.Front:Vector3D;
Begin
  Result := Values[0];
End;

Function VectorDequeue.Back:Vector3D;
Begin
  Result := Values[Pred(Size)];
End;

Procedure VectorDequeue.Push_Back(P:Vector3D);
Begin
  Inc(Size);
  SetLength(Values, Size);
  Values[Pred(Size)] := P;
End;

Procedure NavMesh.CreatePath(Node: PNavMeshNodeInfo; Var Path: Path3D; Start, Goal:Vector3D);
Var
  I,J,K,N:Integer;
  fRight, fLeft:VectorDequeue;
  P1, P2, P:Vector3D;
  LeftLast, RightLast:Vector3D;
  LeftFirst, RightFirst:Vector3D;
{  Points:Array Of Integer;
  PointCount:Integer;}
Begin
  Path.Size := 0;
  Path.Position := 0;

  If (Node.Previous = Nil) Then
  Begin
    Path.Size := 1;
    SetLength(Path.List, 1);
    Path.List[0] := Goal;
    Exit;
  End;

  //PointCount :=0;
  Repeat
    Inc(Path.Size);
    SetLength(Path.List, Path.Size);
    Path.List[Pred(Path.Size)] := _Centers[Node.Triangle];

{    Inc(PointCount);
    SetLength(Points, PointCount);
    Points[Pred(PointCount)] := Node.Triangle;}
    Node := Node.Previous;
  Until Node=Nil;

  //Reverse the path list
  For I:=0 To Pred(Path.Size Div 2) Do
  Begin
    K := Path.Size-Succ(I);
    P := Path.List[I];
    Path.List[I] := Path.List[K];
    Path.List[K] := P;
  End;

  //Reverse the point list
{  For I:=0 To Pred(PointCount Div 2) Do
  Begin
    K := PointCount - Succ(I);
    N := Points[I];
    Points[I] := Points[K];
    Points[K] := N;
  End;}

  // smooth: simplify the path
  I := 0;
  While (I<Path.Size-2) Do
  Begin
    If (IsPathClear(Path.List[I], Path.List[I+2])) Then
    Begin
      For J:=Succ(I) To Path.Size-2 Do
        Path.List[J] := Path.List[J+1];
      Dec(Path.Size);
    End Else
      Inc(I);
  End;

  _LastPath.Size := Path.Size;
  SetLength(_LastPath.List, Path.Size);
  For I:=0 To Pred(Path.Size) Do
    _lastPath.List[I] := Path.List[I];

(*
	// get common edge between triangle[0] and triangle[1] -> the first two (adiacent) triangles in path.
	// result is stored in p1 & p2
	GetCommonEdge(Points[0], Points[1], p1, p2);

  _Left.Size := 0;
  _Right.Size := 0;
  _Apex.Size := 0;
  fRight.Size := 0;
  fLeft.Size := 0;

	// add goal node to both vector lists
	_Left.push_back(Goal); //+CVector3(0,-27.6f,0));
	_Right.push_back(Goal); //+CVector3(0,-27.6f,0));
	_Apex.push_back(Goal); //+CVector3(0,-27.6f,0));

	// push to left or right points if clockwise/counter clockwise in respect to goal.
	If (CCW(p1,p2,goal)) Then
	Begin
		_Left.push_back(p1);
		_Right.push_back(p2);
	End Else
	Begin
		_Left.push_back(p2);
		_Right.push_back(p1);
	End;


	fLeft.push_back(_Left.back());
	fRight.push_back(_Right.back());

	// loop thru every triangle in path starting with triangle 2
	For I:=2 To Pred(PointCount) Do
	Begin
		LeftLast := fLeft.back();
		RightLast := fRight.back();

		// get common edge between n1 & n2
		GetCommonEdge(Points[I-1], Points[I], p1, p2);

		//  check p1 & p2 points with left & right vector lists -> and push to opposite list
		If (P1.Equals(_Right.back())) Then
      _left.push_back(p2)
		Else
    If(p2.Equals(_right.back())) Then
      _left.push_back(p1)
		Else
    If(p1.Equals(_left.back())) Then
      _right.push_back(p2)
		Else
    if(p2.Equals(_left.back())) Then
      _right.push_back(p1);

		LeftFirst := _left.back();
		RightFirst := _right.back();

		// left wall (funnel)
		If (CCW(LeftFirst,LeftLast, _Apex.back())) Then
		Begin
			// adding "outside" point to left wall
			if(Not LeftFirst.Equals( fLeft.back())) Then
        fLeft.push_back(LeftFirst);
		End Else
		Begin
			// adding "inside" point to left wall
			fLeft.push_back(LeftFirst);
      J := 0;
      While (J<fLeft.Size-1) Do
			Begin
        If(CW(LeftFirst, fLeft.Values[J], _Apex.back())) Then
				Begin
          For K:=I To Pred(fLeft.Size) Do
            fLeft.Values[K] := fLeft.Values[K+1];

          Dec(fLeft.Size);
					J := 0;
				End Else
          Inc(J);
			End;
		End;

		// ----------------------------------------
		// right wall (funnel)
		// ----------------------------------------
		If (CW(RightFirst,RightLast, _Apex.back())) Then
		Begin
			// adding "outside" point to right wall
			if(Not RightFirst.Equals(fRight.back())) Then
        fRight.push_back(RightFirst);
		End Else
		Begin
			// adding "inside" point to right wall
			fRight.push_back(RightFirst);
      J := 0;
      While (J<fRight.Size-1) Do
			Begin
        If(CCW(RightFirst, fRight.Values[J], _Apex.back())) Then
				Begin
          For K:=I To Pred(fRight.Size) Do
            fRight.Values[K] := fRight.Values[K+1];

          Dec(fRight.Size);
					J := 0;
				End Else
          Inc(J);
			End;
    End;

		// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
		// processing left side apex-es first, else processing right side apex-es.
		// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

		If (CCW(RightFirst, fLeft.front(), _Apex.back())) Then
		Begin
      J := 0;
      While (J<fLeft.Size-1) Do
			Begin
        If(CCW(RightFirst, fLeft.Values[J], _Apex.back())) Then
				Begin
          For K:=I To Pred(fLeft.Size) Do
            fLeft.Values[K] := fLeft.Values[K+1];

          Dec(fLeft.Size);
					J := 0;
				End Else
          Inc(J);
			End;
		End Else
		If (CW(LeftFirst, fRight.front(), _Apex.back())) Then
		Begin
      J := 0;
      While (J<fRight.Size-1) Do
			Begin
        If(CW(LeftFirst, fRight.Values[J], _Apex.back())) Then
				Begin
          For K:=I To Pred(fRight.Size) Do
            fRight.Values[K] := fRight.Values[K+1];

          Dec(fRight.Size);
					J := 0;
				End Else
          Inc(J);
			End;
    End;
  End;

	// finnaly add also starting point to both vector lists.
	_left.push_back(start);//+CVector3(0,-27.6f,0));
	_right.push_back(start);//+CVector3(0,-27.6f,0));

  *)
End;

Function NavMesh.IsPathClear(Start, Goal: Vector3D): Boolean;
Var
  T, Dist:Single;
  R:Ray;
  D:Vector3D;
Begin
  Start.Add(VectorCreate(0, 5, 0));
  Goal.Add(VectorCreate(0, 5, 0));
  D := VectorSubtract(Goal, Start);
  Dist := D.Length;
  R.Origin := Start;
  R.Direction := D;
  R.Direction.Normalize;
  Result := Self.Intersect(R, T, navMeshBoundaries);
  Result := (Not Result) Or (T>Dist);
End;

Procedure NavMesh.Steer(var Source: Circle);
Var
  I:Integer;
  Hit, Dir:Vector2D;
Begin
  For I:=0 To Pred(_WallCount) Do
  If (_Walls[I].Intersect(Source, @Hit)) Then
  Begin
    Dir := Hit;
    Dir.Subtract(Source.Center);
    Dir.Normalize;
    Dir.Scale(-Source.Radius);
    Dir.Add(Hit);
    Source.Center := Dir;
    Exit;
  End;
End;

Function NavMesh.CreateOctree(Index: Integer): Octree;
Var
  I:Integer;
  Tri:Triangle;
  A,B,C:Vector3D;
  Element:NavMeshOctreeElement;
Begin
  Result := Octree.Create(_BoundingBoxes[Index]);
  For I:=0 To Pred(_Buffers[Index]._TriangleCount) Do
  Begin
    Element := NavMeshOctreeElement.Create;
    Element.TriangleID := I;
    Element.BufferID := Index;
    Element.Mesh := Self;

    Tri := _Buffers[Index]._Triangles[I];
    A := _Buffers[Index]._Vertices[Tri.Indices[0]];
    B := _Buffers[Index]._Vertices[Tri.Indices[1]];
    C := _Buffers[Index]._Vertices[Tri.Indices[2]];

    Element.Box.Reset;
    Element.Box.Add(A);
    Element.Box.Add(B);
    Element.Box.Add(C);

    Result.AddElement(Element);
  End;
End;

Destructor NavMesh.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To 1 Do
  If Assigned(_Octrees[I]) Then
    _Octrees[I].Destroy;
End;

{ NavMeshOctreeElement }
Function NavMeshOctreeElement.Intersect(const R: Ray; Var T: Single): Boolean;
Var
  Tri:Triangle;
  A,B,C:Vector3D;
  U,V:Single;
Begin
  Tri := Mesh._Buffers[Self.BufferID]._Triangles[Self.TriangleID];
  A := Mesh._Buffers[Self.BufferID]._Vertices[Tri.Indices[0]];
  B := Mesh._Buffers[Self.BufferID]._Vertices[Tri.Indices[1]];
  C := Mesh._Buffers[Self.BufferID]._Vertices[Tri.Indices[2]];
  Result := R.TriangleIntersect(A,B,C, T, U, V);
End;

End.
