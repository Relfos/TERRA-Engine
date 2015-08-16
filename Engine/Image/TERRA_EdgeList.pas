Unit TERRA_EdgeList;

{$I terra.inc}

Interface
Uses TERRA_Object;

Type
  TStBttEdge = record
    x0,y0, x1,y1: Single;
    invert: Integer;
  end;
  PStBttEdge = ^TStBttEdge;

  EdgeList = Class(TERRAObject)
  Private
    _List: Array Of PStBttEdge;
    _Count: Integer;

  Protected
    function Get(Index: Integer): PStBttEdge;


  Public
    Constructor Create();
    Procedure Release; override;
    function Add(Item: PStBttEdge): Integer;

    procedure Sort();

    property Count: Integer read _Count;
    property Items[Index: Integer]: PStBttEdge read Get; default;
  end;

Implementation

Const
  MaxListSize = Maxint div 16;


{ EdgeList }
Constructor EdgeList.Create();
Begin
  SetLength(_List, 16);
  _Count := 0;
End;

Procedure EdgeList.Release;
Var
  I:Integer;
Begin
   For I:=0 to Pred(_Count) Do
   Begin
      FreeMem(_List[i]);
   End;

  SetLength(_List, 0);
  _Count := 0;
End;

function EdgeList.Add(Item: PStBttEdge): Integer;
begin
  Result := _Count;

  While Result >= Length(_List) Do
    SetLength(_List, Length(_List) * 2);

  _List[Result] := Item;
  Inc(_Count);
end;

function EdgeList.Get(Index: Integer): PStBttEdge;
begin
  If (Index < 0) or (Index >= _Count) then
  Begin
  	Result := Nil;
  	Exit;
  End;

  Result := _List[Index];
end;

function EdgeCompare(pa, pb: PStBttEdge): Integer;
begin
   if pa.y0 < pb.y0 then
      Result := -1
   else
   if pa.y0 > pb.y0 then
      Result := 1
   else
      Result := 0;
end;

procedure QuickSort(SorEdgeList: PPointerArray; L, R: Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SorEdgeList[(L + R) shr 1];
    repeat
      while EdgeCompare(SorEdgeList[I], P) < 0 do
        Inc(I);

      while EdgeCompare(SorEdgeList[J], P) > 0 do
        Dec(J);

      if I <= J then
      begin
        T := SorEdgeList[I];
        SorEdgeList[I] := SorEdgeList[J];
        SorEdgeList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SorEdgeList, L, J);
    L := I;
  until I >= R;
end;

procedure EdgeList.Sort();
begin
  if (_List <> nil) and (Count > 0) then
    QuickSort(@_List[0], 0, Count - 1);
end;

End.
