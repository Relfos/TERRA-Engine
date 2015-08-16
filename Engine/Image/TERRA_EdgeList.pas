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
    _List: Array Of TStBttEdge;
    _Count: Integer;

    procedure QuickSort(L, R: Integer);

    function Get(Index: Integer): PStBttEdge;


  Public
    Constructor Create();
    Procedure Release; override;
    function Add(Const Item: TStBttEdge): Integer;

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
//      FreeMem(_List[i]);
   End;

  SetLength(_List, 0);
  _Count := 0;
End;

function EdgeList.Add(Const Item: TStBttEdge): Integer;
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

  Result := @_List[Index];
end;

function EdgeCompare(Const pa, pb: TStBttEdge): Integer;
begin
   if pa.y0 < pb.y0 then
      Result := -1
   else
   if pa.y0 > pb.y0 then
      Result := 1
   else
      Result := 0;
end;

procedure EdgeList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P, T:TStBttEdge;
begin
  repeat
    I := L;
    J := R;
    P := _List[(L + R) shr 1];
    repeat
      while EdgeCompare(_List[I], P) < 0 do
        Inc(I);

      while EdgeCompare(_List[J], P) > 0 do
        Dec(J);

      if I <= J then
      begin
        T := _List[I];
        _List[I] := _List[J];
        _List[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort( L, J);
    L := I;
  until I >= R;
end;

Procedure EdgeList.Sort();
Begin
  If (Count > 0) then
    QuickSort(0, Count - 1);
End;

End.
