Unit TERRA_EdgeList;

{$I terra.inc}

Interface
Uses TERRA_Object;

Type
  EdgeListSortCompare = function (Item1, Item2: Pointer): Integer;

  EdgeList = Class(TERRAObject)
  Private
    _List: Array Of Pointer;
    _Count: Integer;

  Protected
    function Get(Index: Integer): Pointer;


  Public
    Constructor Create();
    Procedure Release; override;
    function Add(Item: Pointer): Integer;

    procedure Sort(Compare: EdgeListSortCompare);

    property Count: Integer read _Count;
    property Items[Index: Integer]: Pointer read Get; default;
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
Begin
  SetLength(_List, 0);
  _Count := 0;
End;

function EdgeList.Add(Item: Pointer): Integer;
begin
  Result := _Count;

  While Result >= Length(_List) Do
    SetLength(_List, Length(_List) * 2);

  _List[Result] := Item;
  Inc(_Count);
end;

function EdgeList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= _Count) then
  Begin
  	Result := Nil;
  	Exit;
  End;
  
  Result := _List[Index];
end;


procedure QuickSort(SorEdgeList: PPointerArray; L, R: Integer; SCompare: EdgeListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SorEdgeList^[(L + R) shr 1];
    repeat
      while SCompare(SorEdgeList^[I], P) < 0 do
        Inc(I);
      while SCompare(SorEdgeList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SorEdgeList^[I];
        SorEdgeList^[I] := SorEdgeList^[J];
        SorEdgeList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SorEdgeList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure EdgeList.Sort(Compare: EdgeListSortCompare);
begin
  if (_List <> nil) and (Count > 0) then
    QuickSort(@_List[0], 0, Count - 1, Compare);
end;

End.
