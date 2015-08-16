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


  Public
    Constructor Create();
    Procedure Release; override;

    Function Add(Const Item: TStBttEdge): Integer;
    Function Get(Index: Integer): PStBttEdge;

    Procedure Sort();

    Property Count: Integer read _Count;
  End;

Implementation


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

Function EdgeList.Add(Const Item: TStBttEdge): Integer;
Begin
  Result := _Count;

  While Result >= Length(_List) Do
    SetLength(_List, Length(_List) * 2);

  _List[Result] := Item;
  Inc(_Count);
End;

Function EdgeList.Get(Index: Integer): PStBttEdge;
Begin
  If (Index < 0) or (Index >= _Count) then
  Begin
  	Result := Nil;
  	Exit;
  End;

  Result := @_List[Index];
End;

Function EdgeCompare(Const pa, pb: TStBttEdge): Integer;
Begin
   if pa.y0 < pb.y0 then
      Result := -1
   else
   if pa.y0 > pb.y0 then
      Result := 1
   else
      Result := 0;
End;

Procedure EdgeList.QuickSort(L, R: Integer);
Var
  I, J: Integer;
  P, T:TStBttEdge;
Begin
  Repeat
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
  Until I >= R;
End;

Procedure EdgeList.Sort();
Begin
  If (Count > 0) then
    QuickSort(0, Count - 1);
End;

End.
