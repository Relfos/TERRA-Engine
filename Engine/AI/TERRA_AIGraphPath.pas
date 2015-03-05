Unit TERRA_AIGraphPath;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Stream;

Const
  //  Search flags
  sfSearchTimeOut   = 1;
  sfSearchDiagonal  = 2;

  PathSearchTimeOut = 200;
  MaxSearchNodes = 1000;

Type
  //  Search results
  PathNodeState = (nsInactive,nsActive,nsDiscarded);


  GraphPathVertex = Class
    Protected
      _ID:Integer;
      _Position:Vector3D;
      _Adjacents:Array Of GraphPathVertex3D;
      _AdjacentCount:Integer;

    Public
      Property Position:Vector3D Read _Position;
  End;

  GraphPath = Class
    Protected
      _List:Array Of Vector3D;
      _Size:Integer;
      _Position:Integer;
    Public
  End;

  GraphPathNodeInfo = Record
    Vertex:GraphPathVertex;
    ID:Integer;
    Previous:GraphPathNodeInfo;
    Cost:Integer;
    State:PathNodeState;
  End;

  GraphPathfinder = Class
    Protected
      _VertexList:Array Of GraphPathVertex;
      _VertexCount:Integer;
      _CurrentVertex:GraphPathVertex;
      _EndVertex:GraphPathVertex;

      Function GetCost(Vertex:PPathVertex3D):Integer;Virtual;
      Procedure VisitNode(Vertex:PPathVertex3D);Virtual;

      Procedure CreatePath(Node:PPathNodeInfo3D; Var Path:Path3D);

    Public
      Constructor Create(VertexCount:Integer);
      Function GetVertex(Index:Integer):PPathVertex3D;
      Function GetNearestVertex(Pos:Vector3D):PPathVertex3D;
      Procedure AddEdge(A,B:PPathVertex3D);

      Procedure Load(FileName:TERRAString); Overload;
      Procedure Load(Source:Stream); Overload;
      Procedure Save(FileName:TERRAString); Overload;
      Procedure Save(Dest:Stream); Overload;

      Function Search(StartPos, EndPos:Vector3D;
                      Var Path:Path3D;
                      Flags:Integer=sfSearchTimeOut):PathSearchResult;Overload;
  End;

Function PathNode2DCreate(X,Y:Integer):PathNode2D;

Implementation
Uses TERRA_OS, TERRA_FileStream;

Function PathNode2DCreate(X,Y:Integer):PathNode2D;
Begin
  Result.X:=X;
  Result.Y:=Y;
End;

{
Start at the initial position (node) and place it on the Open list, along with its estimated cost to the destination, which is determined by a heuristic.
The heuristic is often just the geometric distance between two nodes. Then perform the following loop while the Open list is nonempty:

Pop the node off the Open list that has the lowest estimated cost to the destination.

If the node is the destination, we've successfully finished (quit).

Examine the node's eight neighboring nodes.

For each of the nodes which are not blocked, calculate the estimated cost to the goal of the path that
goes through that node. (This is the actual cost to reach that node from the origin, plus the heuristic cost to the destination.)

Push all those nonblocked surrounding nodes onto the Open list, and repeat loop.
}

// LPathfinder2D
Function Pathfinder2D.GetCost(X,Y:Integer):Integer;
Begin
  Result := Trunc(Sqrt(Sqr(X-EndX)+Sqr(Y-EndY)));
End;

Procedure Pathfinder2D.VisitNode(X,Y:Integer);
Begin
End;

Procedure Pathfinder2D.CreatePath(Node:PPathNodeInfo2D; Var Path:Path2D);
Var
  I,K:Integer;
  P:PathNode2D;
Begin
  // Retrieve the path list
  Repeat
    Inc(Path.Size);
    SetLength(Path.List,Path.Size);
    Path.List[Pred(Path.Size)]:=PathNode2DCreate(Node.X,Node.Y);
    Node:=Node.Previous;
  Until Node=Nil;

  //Reverse the path list
  For I:=0 To Pred(Path.Size Div 2) Do
  Begin
    K:=Path.Size-Succ(I);
    P:=Path.List[I];
    Path.List[I]:=Path.List[K];
    Path.List[K]:=P;
  End;
End;

Function Pathfinder2D.SearchWithBounds(StartX,StartY,EndX,EndY:Integer;
                            MinX,MinY,MaxX,MaxY:Integer;
                            Out Path:Path2D;
                            Flags:Integer=sfSearchTimeOut):PathSearchResult;
Var
  NodeList:Array Of PathNodeInfo2D;
  Width:Integer;
  MinNode,MaxNode:Integer;
  NodeCount:Integer;

Procedure AddNode(X,Y,Cost:Integer; Previous:PPathNodeInfo2D);
Var
  I:Integer;
Begin
  I := (X-MinX)+(Y-MinY)*Width;
  If (Length(NodeList)<=I) Or (NodeList[I].State<>nsInactive) Then
    Exit;

  MinNode := IntMin(MinNode,I);
  MaxNode := IntMax(MaxNode,I);

  NodeList[I].X := X;
  NodeList[I].Y := Y;
  NodeList[I].ID := I;
  NodeList[I].Cost := Cost;
  NodeList[I].Previous := Previous;
  NodeList[I].State := nsActive;

  VisitNode(X,Y);
End;

Procedure TestNode(X,Y:Integer; Previous:PPathNodeInfo2D);
Var
  Cost:Integer;
Begin
  If Assigned(Previous) Then
  Begin
    CurrentX := Previous.X;
    CurrentY := Previous.Y;
  End;

  Cost := GetCost(X,Y);
  If Cost<0 Then
    Exit;

  AddNode(X,Y,Cost,Previous);
End;

Var
  I,K,Min:Integer;
  StartTime:Cardinal;
  Node:PPathNodeInfo2D;
Begin
  Self.CurrentX:=StartX;
  Self.CurrentY:=StartY;
  Self.EndX:=EndX;
  Self.EndY:=EndY;

  Width:=MaxX-MinX;
  NodeCount:=(MaxY-MinY)*(MaxX-MinX);
  SetLength(NodeList,NodeCount);
  MinNode:=Succ(NodeCount);
  MaxNode:=-1;

  AddNode(StartX,StartY, GetCost(StartX,StartY), Nil);
  StartTime := GetTime;

  Path.Size := 0;
  Path.Position := 0;
  SetLength(Path.List,0);

  Repeat
    K:=-1;
    Min:=999999;
    For I:=MinNode To MaxNode Do
    If (NodeList[I].State=nsActive)And(NodeList[I].Cost<Min) Then
    Begin
      K:=I;
      Min:=NodeList[I].Cost;
    End;

    If (K=-1) Then
    Begin
      Result:=srSearchFailed;
      Exit;
    End;

    If (NodeList[K].X=EndX)And(NodeList[K].Y=EndY) Then
    Begin
      Result:=srSearchComplete;
      CreatePath(@NodeList[K],Path);
      Exit;
    End;

    With NodeList[K] Do
    Begin
      Node:=@NodeList[K];

      TestNode(X-1, Y, Node);
      TestNode(X+1, Y, Node);
      TestNode(X, Y-1, Node);
      TestNode(X, Y+1, Node);

      If (Flags And sfSearchDiagonal<>0) Then
      Begin
        TestNode(X-1, Y-1, Node);
        TestNode(X-1, Y+1, Node);
        TestNode(X+1, Y-1, Node);
        TestNode(X+1, Y+1, Node);
      End;

      Node.State:=nsDiscarded;
    End;

  Until (Flags And sfSearchTimeout<>0)And(GetTime-StartTime>=PathSearchTimeOut);

  Result := srSearchTimeOut;
End;

Function Pathfinder2D.Search(StartX,StartY,EndX,EndY:Integer;
                            Out Path:Path2D;
                            Flags:Integer=sfSearchTimeOut):PathSearchResult;
Var
  NodeList:Array[0..Pred(MaxSearchNodes)] Of PathNodeInfo2D;
  NodeCount:Integer;

Procedure AddNode(X,Y,Cost:Integer; Previous:PPathNodeInfo2D);
Var
  I:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  If (NodeList[I].X=X) And (NodeList[I].Y=Y) Then
    Exit;

  Inc(NodeCount);
  I:=Pred(NodeCount);

  NodeList[I].X:=X;
  NodeList[I].Y:=Y;
  NodeList[I].ID:=I;
  NodeList[I].Cost:=Cost;
  NodeList[I].Previous:=Previous;
  NodeList[I].State:=nsActive;

  VisitNode(X,Y);
End;

Procedure TestNode(X,Y:Integer; Previous:PPathNodeInfo2D);
Var
  Cost:Integer;
Begin
  If Assigned(Previous) Then
  Begin
    CurrentX:=Previous.X;
    CurrentY:=Previous.Y;
  End;

  Cost:=GetCost(X,Y);
  If Cost<0 Then
    Exit;

  AddNode(X,Y,Cost,Previous);
End;

Var
  I,K,Min:Integer;
  StartTime:Cardinal;
  Node:PPathNodeInfo2D;
Begin
  Self.CurrentX:=StartX;
  Self.CurrentY:=StartY;
  Self.EndX:=EndX;
  Self.EndY:=EndY;

  NodeCount:=0;

  AddNode(StartX,StartY, GetCost(StartX,StartY), Nil);
  StartTime:=GetTime;

  Path.Size:=0;
  Path.Position:=0;
  SetLength(Path.List,0);

  Repeat
    K:=-1;
    Min:=999999;
    For I:=0 To Pred(NodeCount) Do
    If (NodeList[I].State=nsActive)And(NodeList[I].Cost<Min) Then
    Begin
      K:=I;
      Min:=NodeList[I].Cost;
    End;

    If (K=-1) Then
    Begin
      Result:=srSearchFailed;
      Exit;
    End;

    If (NodeList[K].X=EndX)And(NodeList[K].Y=EndY) Then
    Begin
      Result:=srSearchComplete;
      CreatePath(@NodeList[K], Path);
      Exit;
    End;

    With NodeList[K] Do
    Begin
      Node:=@NodeList[K];

      TestNode(X-1, Y, Node);
      TestNode(X+1, Y, Node);
      TestNode(X, Y-1, Node);
      TestNode(X, Y+1, Node);

      If (Flags And sfSearchDiagonal<>0) Then
      Begin
        TestNode(X-1, Y-1, Node);
        TestNode(X-1, Y+1, Node);
        TestNode(X+1, Y-1, Node);
        TestNode(X+1, Y+1, Node);
      End;

      Node.State:=nsDiscarded;
    End;

  Until (NodeCount>=MaxSearchNodes)Or((Flags And sfSearchTimeout<>0)And(GetTime-StartTime>=PathSearchTimeOut));

  If (NodeCount>=MaxSearchNodes) Then
    Result:=srSearchLimitReached
  Else
    Result:=srSearchTimeOut;
End;

{ Pathfinder3D }

Constructor Pathfinder3D.Create(VertexCount:Integer);
Begin
  _VertexCount := VertexCount;
  SetLength(_VertexList, _VertexCount);
End;

Procedure Pathfinder3D.AddEdge(A,B:PPathVertex3D);
Var
  N, I:Integer;
Begin
  If (Not Assigned(A)) Or (Not Assigned(B)) Then
    Exit;

  N:=-1;
  For I:=0 To Pred(A.AdjacentCount) Do
  If (A.Adjacents[I]=B) Then
  Begin
    N := I;
    Break;
  End;

  If (N=-1) Then
  Begin
    Inc(A.AdjacentCount);
    SetLength(A.Adjacents, A.AdjacentCount);
    A.Adjacents[Pred(A.AdjacentCount)] := B;
  End;

  N:=-1;
  For I:=0 To Pred(B.AdjacentCount) Do
  If (B.Adjacents[I]=A) Then
  Begin
    N:=I;
    Break;
  End;

  If (N=-1) Then
  Begin
    Inc(B.AdjacentCount);
    SetLength(B.Adjacents, B.AdjacentCount);
    B.Adjacents[Pred(B.AdjacentCount)] := A;
  End;
End;

Function Pathfinder3D.GetVertex(Index:Integer):PPathVertex3D;
Begin
  If (Index>=0) And (Index<_VertexCount) Then
    Result := @(_VertexList[Index])
  Else
    Result := Nil;
End;

Procedure Pathfinder3D.CreatePath(Node:PPathNodeInfo3D; Var Path:Path3D);
Var
  I,K:Integer;
  P:Vector3D;
Begin
  Path.Position := 0;
  Path.Size := 0;
  // Retrieve the path list
  Repeat
    Inc(Path.Size);
    SetLength(Path.List,Path.Size);
    Path.List[Pred(Path.Size)] := Node.Vertex.Position;
    Node:=Node.Previous;
  Until Node=Nil;

  //Reverse the path list
  For I:=0 To Pred(Path.Size Div 2) Do
  Begin
    K := Path.Size-Succ(I);
    P := Path.List[I];
    Path.List[I] := Path.List[K];
    Path.List[K] := P;
  End;
End;

Function Pathfinder3D.GetCost(Vertex:PPathVertex3D):Integer;
Begin
  Result:=Trunc(Vertex.Position.Distance(_EndVertex.Position));
End;

Procedure Pathfinder3D.VisitNode(Vertex: PPathVertex3D);
Begin
End;

Function Pathfinder3D.GetNearestVertex(Pos:Vector3D):PPathVertex3D;
Var
  I:Integer;
  Dist, Min:Single;
Begin
  Min:=9999;
  Result:=Nil;
  For I:=0 To Pred(_VertexCount) Do
  Begin
    Dist := Pos.Distance(_VertexList[I].Position);
    If (Dist<Min) Then
    Begin
      Min := Dist;
      Result := @(_VertexList[I]);
    End;
  End;
End;

Function Pathfinder3D.Search(StartPos,EndPos:Vector3D;
                             Var Path:Path3D; Flags:Integer):PathSearchResult;
Var
  NodeList:Array[0..Pred(MaxSearchNodes)] Of PathNodeInfo3D;
  NodeCount:Integer;

Procedure AddNode(Vertex:PPathVertex3D; Cost:Integer; Previous:PPathNodeInfo3D);
Var
  I:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  If (NodeList[I].Vertex=Vertex) Then
    Exit;

  Inc(NodeCount);
  I:=Pred(NodeCount);

  NodeList[I].Vertex:=Vertex;
  NodeList[I].ID:=I;
  NodeList[I].Cost:=Cost;
  NodeList[I].Previous:=Previous;
  NodeList[I].State:=nsActive;

  VisitNode(Vertex);
End;

Procedure TestNode(Vertex:PPathVertex3D; Previous:PPathNodeInfo3D);
Var
  Cost:Integer;
Begin
  If Assigned(Previous) Then
    _CurrentVertex:=Previous.Vertex;

  Cost:=GetCost(Vertex);
  If Cost<0 Then
    Exit;

  AddNode(Vertex, Cost, Previous);
End;

Var
  I,K,Min:Integer;
  StartTime:Cardinal;
  Node:PPathNodeInfo3D;
  StartVertex:PPathVertex3D;
Begin
  Self._CurrentVertex := GetNearestVertex(StartPos);
  Self._EndVertex := GetNearestVertex(EndPos);

  NodeCount:=0;

  StartVertex := _CurrentVertex;
  AddNode(StartVertex, GetCost(StartVertex), Nil);
  StartTime:=GetTime;

  Path.Size:=0;
  Path.Position:=0;
  SetLength(Path.List,0);

  Repeat
    K:=-1;
    Min:=999999;
    For I:=0 To Pred(NodeCount) Do
    If (NodeList[I].State=nsActive)And(NodeList[I].Cost<Min) Then
    Begin
      K:=I;
      Min:=NodeList[I].Cost;
    End;

    If (K=-1) Then
    Begin
      Result:=srSearchFailed;
      Exit;
    End;

    If (NodeList[K].Vertex = _EndVertex) Then
    Begin
      Result:=srSearchComplete;
      CreatePath(@NodeList[K], Path);
      Exit;
    End;

    With NodeList[K] Do
    Begin
      Node:=@NodeList[K];

      For I:=0 To Pred(Node.Vertex.AdjacentCount) Do
        TestNode(Node.Vertex.Adjacents[I], Node);

      Node.State:=nsDiscarded;
    End;

  Until (NodeCount>=MaxSearchNodes)Or((Flags And sfSearchTimeout<>0)And(GetTime-StartTime>=PathSearchTimeOut));

  If (NodeCount>=MaxSearchNodes) Then
    Result:=srSearchLimitReached
  Else
    Result:=srSearchTimeOut;
End;

Procedure Pathfinder3D.Load(Source: Stream);
Var
  I,J,K:Integer;
Begin
  Source.Read(@_VertexCount, 4);
  SetLength(_VertexList, _VertexCount);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    Source.Read(@_VertexList[I].ID, 4);
    Source.Read(@_VertexList[I].Position, SizeOf(Vector3D));
    Source.Read(@_VertexList[I].AdjacentCount, 4);
    SetLength(_VertexList[I].Adjacents, _VertexList[I].AdjacentCount);
    For J := 0 To Pred(_VertexList[I].AdjacentCount) Do
    Begin
      Source.Read(@K, 4);
      _VertexList[I].Adjacents := @(_VertexList[K]);
    End;
  End;
End;

Procedure Pathfinder3D.Save(Dest: Stream);
Var
  I,J:Integer;
Begin
  Dest.Write(@_VertexCount, 4);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    Dest.Write(@_VertexList[I].ID, 4);
    Dest.Write(@_VertexList[I].Position, SizeOf(Vector3D));
    Dest.Write(@_VertexList[I].AdjacentCount, 4);
    For J := 0 To Pred(_VertexList[I].AdjacentCount) Do
      Dest.Write(@_VertexList[I].Adjacents[J].ID, 4);
  End;
End;

Procedure Pathfinder3D.Load(FileName:TERRAString);
Var
  Source:Stream;
Begin
  Source := FileStream.Open(FileName);
  Load(Source);
  Source.Destroy;
End;

Procedure Pathfinder3D.Save(FileName:TERRAString);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Save(Dest);
  Dest.Destroy;
End;

End.
