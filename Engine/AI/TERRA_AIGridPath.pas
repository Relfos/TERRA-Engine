Unit TERRA_AIGridPath;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Stream, TERRA_Collections;

Const
  //  Search flags
  sfSearchTimeOut   = 1;
  sfSearchDiagonal  = 2;

  PathSearchDefaultTimeOut = 200;
  MaxSearchNodes = 1000;

Type
  //  Search results
  PathNodeState = (nsInactive, nsActive, nsDiscarded);

  PathSearchError = (
    pathSearchInProgress = 0,
    pathSearchComplete = 1,
    pathSearchLimitReached = 2,
    pathSearchTimeOut = 3,
    pathSearchFailed = 4
  );

  GridPathNodeInfo = Class(TERRAObject)
    X:Integer;
    Y:Integer;
    ID:Integer;
    Previous:GridPathNodeInfo;
    Cost:Double;
    State:PathNodeState;
  End;

  GridPathNode = Packed Record
    X:Integer;
    Y:Integer;
  End;

  GridPath = Class(TERRAObject)
    Protected
      _List:Array Of GridPathNode;
      _Size:Integer;

    Public
      Constructor Create(Node:GridPathNodeInfo);
      Function GetNode(Index:Integer; Out Node:GridPathNode):Boolean;
      Property Size:Integer Read _Size;
  End;

  GridPathfinder = Class(TERRAObject)
    Protected
      _CurrentX:Integer;
      _CurrentY:Integer;

      _TargetX:Integer;
      _TargetY:Integer;

      _ErrorCode:PathSearchError;

      Function GetCost(X,Y:Integer):Double; Virtual;
      Procedure VisitNode(X,Y:Integer); Virtual;

    Public

      // Search a path in a limited area
      Function SearchWithBounds(StartX,StartY:Integer;
                      EndX,EndY:Integer;
                      MinX,MinY:Integer;
                      MaxX,MaxY:Integer;
                      Flags:Integer = sfSearchTimeOut):GridPath;

      // Search a path in a infinite area
(*      Function Search(StartX,StartY:Integer;
                      EndX,EndY:Integer;
                      Flags:Integer=sfSearchTimeOut):GridPath;*)

    Property ErrorCode:PathSearchError Read _ErrorCode;
  End;

Function GridPathNodeCreate(X,Y:Integer):GridPathNode;

Implementation
Uses TERRA_OS, TERRA_FileStream;

Function GridPathNodeCreate(X,Y:Integer):GridPathNode;
Begin
  Result.X := X;
  Result.Y := Y;
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

{ GridPathFinder }
Function GridPathFinder.GetCost(X,Y:Integer):Double;
Begin
  Result := Sqrt(Sqr(X- _TargetX) + Sqr(Y-_TargetY));
End;

Procedure GridPathFinder.VisitNode(X,Y:Integer);
Begin
End;

Function GridPathFinder.SearchWithBounds(StartX,StartY:Integer;
                      EndX,EndY:Integer;
                      MinX,MinY:Integer;
                      MaxX,MaxY:Integer;
                      Flags:Integer = sfSearchTimeOut):GridPath;
Var
  NodeList:Array Of GridPathNodeInfo;
  Width:Integer;
  MinNode,MaxNode:Integer;
  NodeCount:Integer;

Procedure AddNode(X,Y:Integer; Const Cost:Double; Previous:GridPathNodeInfo);
Var
  I:Integer;
Begin
  I := (X-MinX)+(Y-MinY)*Width;
  If (I<0) Or (Length(NodeList)<=I) Then
    Exit;

  If (Assigned(NodeList[I])) And (NodeList[I].State<>nsInactive) Then
    Exit;

  If (Not Assigned(NodeList[I])) Then
    NodeList[I] := GridPathNodeInfo.Create();

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

Procedure TestNode(X,Y:Integer; Previous:GridPathNodeInfo);
Var
  Cost:Double;
Begin
  If Assigned(Previous) Then
  Begin
    _CurrentX := Previous.X;
    _CurrentY := Previous.Y;
  End;

  Cost := GetCost(X,Y);
  If Cost<0 Then
    Exit;

  AddNode(X,Y,Cost,Previous);
End;

Var
  I, N:Integer;
  Min:Double;
  X,Y:Integer;
  StartTime:Cardinal;
  CurrentNode:GridPathNodeInfo;
Begin
  Self._CurrentX := StartX;
  Self._CurrentY := StartY;
  Self._TargetX := EndX;
  Self._TargetY := EndY;

  Width:=MaxX-MinX;
  NodeCount:=(MaxY-MinY)*(MaxX-MinX);
  SetLength(NodeList,NodeCount);
  MinNode:=Succ(NodeCount);
  MaxNode:=-1;

  AddNode(StartX,StartY, GetCost(StartX,StartY), Nil);
  StartTime := Application.GetTime;

  Result := Nil;
  _ErrorCode := pathSearchInProgress;

  Repeat
    CurrentNode := Nil;
    Min := 999999;
    For I:=MinNode To MaxNode Do
    If (Assigned(NodeList[I])) And (NodeList[I].State=nsActive) And (NodeList[I].Cost<Min) Then
    Begin
      CurrentNode := NodeList[I];
      Min := CurrentNode.Cost;
    End;

    If (CurrentNode = Nil) Then
    Begin
      _ErrorCode := pathSearchFailed;
      Break;
    End;

    If (CurrentNode.X = EndX) And (CurrentNode.Y = EndY) Then
    Begin
      Result := GridPath.Create(CurrentNode);
      _ErrorCode := pathSearchComplete;
      Break;
    End;

    X := CurrentNode.X;
    Y := CurrentNode.Y;
    
    TestNode(X-1, Y, CurrentNode);
    TestNode(X+1, Y, CurrentNode);
    TestNode(X, Y-1, CurrentNode);
    TestNode(X, Y+1, CurrentNode);

    If (Flags And sfSearchDiagonal<>0) Then
    Begin
      TestNode(X-1, Y-1, CurrentNode);
      TestNode(X-1, Y+1, CurrentNode);
      TestNode(X+1, Y-1, CurrentNode);
      TestNode(X+1, Y+1, CurrentNode);
    End;

    CurrentNode.State := nsDiscarded;

  Until (Flags And sfSearchTimeout<>0)And(Application.GetTime-StartTime>= PathSearchDefaultTimeOut);

  If (_ErrorCode = pathSearchInProgress) Then
    _ErrorCode := pathSearchTimeOut;

  N := Length(NodeList);
  For I:=0 To Pred(N) Do
    ReleaseObject(NodeList[I]);

  SetLength(NodeList, 0);
End;

(*Function GridPathFinder.Search(StartX,StartY,EndX,EndY:Integer;
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
      CreatePath(NodeList[K], Path);
      Exit;
    End;

    With NodeList[K] Do
    Begin
      Node:= NodeList[K];

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
End;*)

{ GridPath }
Constructor GridPath.Create(Node:GridPathNodeInfo);
Var
  I, N:Integer;
  P:GridPathNodeInfo;
Begin
  _Size := 0;

  P := Node;
  While Assigned(P) Do
  Begin
    P := P.Previous;
    Inc(_Size);
  End;

  SetLength(_List, _Size);

  //Reverse the path list
  P := Node;
  N := 0;
  While Assigned(P) Do
  Begin
    I := Pred(_Size - N);
    _List[I].X := P.X;
    _List[I].Y := P.Y;
    Inc(N);
    P := P.Previous;
  End;
End;

Function GridPath.GetNode(Index:Integer; Out Node:GridPathNode):Boolean;
Begin
  Result := (Index>=0) And (Index<_Size);

  If Result Then
  Begin
    Node := _List[Index];
  End;
End;

End.
