Unit TERRA_List;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Collections;

Type
  TERRAList = Class(TERRACollection)
    Protected
      _First:TERRACollectionObject;
      _Last:TERRACollectionObject;

      Procedure RemoveDiscardedItems(); Override;

    Public
      Constructor Create(SortOrder:CollectionSortOrder = collection_Unsorted; Options:Integer = 0);

      Procedure Clear(); Override;
      Function GetIterator:TERRAIterator; Override;

      Function GetItemByIndex(Index:Integer):TERRAObject; Override;

      Class Function GetObjectType:TERRAString; Override;

      // adds copies of items, not references!
      Function Merge(C:TERRACollection):TERRAList;

      // Returns true if insertion was sucessful
      Function Add(Item:TERRAObject):Boolean;Virtual;

      // Returns true if removal was sucessful
      Function Remove(Item:TERRAObject):Boolean; Override;

      //Function Contains(Item:TERRAObject):Boolean; Override;

      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Property First:TERRACollectionObject Read _First;
  End;

  ListIterator = Class(TERRAIterator)
    Protected
      _Current:TERRACollectionObject;

      Function ObtainNext:TERRACollectionObject; Override;
      Procedure Reset(); Override;

    Public
  End;

Implementation
Uses TERRA_Engine, TERRA_Debug, TERRA_Log;

{ List }
Constructor TERRAList.Create(SortOrder:CollectionSortOrder; Options:Integer);
Begin
  _SortOrder := SortOrder;
  _First := Nil;
  _ItemCount := 0;
  Self.Init(Options, Nil);
End;

Procedure TERRAList.RemoveDiscardedItems();
Var
  P, Prev:TERRACollectionObject;
Begin
  Prev := Nil;

  P := _First;
  While (Assigned(P)) Do
  Begin
    If P.Discarded Then
    Begin
      If Assigned(Prev) Then
      Begin
        Prev.Next := P.Next;
        ReleaseObject(P);
        Dec(_ItemCount);
        P := Prev.Next;
      End Else
      Begin
        _First := P.Next;
        ReleaseObject(P);
        P := _First;
      End;
    End Else
    Begin
      Prev := P;
      P := P.Next;
    End;
  End;
End;

Function TERRAList.GetItemByIndex(Index:Integer):TERRAObject;
Var
  I:Integer;
  Src:TERRACollectionObject;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Src := Self._First;
  Result := Src.Item;

  If (Index=0) Then
    Exit;

  Self.Lock();

  I := 0;
  While (Src<>Nil) Do
  Begin
    Src := Src.Next;
    Result := Src.Item;

    Inc(I);

    If (I = Index) Then
      Break;
  End;
  Self.Unlock();
End;

Function TERRAList.Merge(C:TERRACollection):TERRAList;
Var
  I:TERRAIterator;
  Temp, N: TERRAObject;
Begin
  Result := Self;

  If (C = Self) Then
    Exit;

  Self.Update();

  I := C.GetIterator();
  While I.HasNext Do
  Begin
    Temp := I.Value;
    N := TERRAObject(Temp.ClassType.Create());
    N.CopyProperties(Temp);
    Self.Add(N);
  End;
  ReleaseObject(C);
End;

Procedure TERRAList.Clear();
Var
  List,Next:TERRACollectionObject;
Begin
  Self.Lock();

  If _ItemCount>0 Then
  Begin
    {$IFNDEF DISABLEMEMORYPOOL}
    Engine.Pool.Grow(Engine.Pool.Count + Self.Count);
    {$ENDIF}

    List := _First;
    While Assigned(List)Do
    Begin
      Next := List.Next;

      {$IFDEF DISABLEMEMORYPOOL}
      ReleaseObject(List);
      {$ELSE}
      Engine.Pool.Recycle(List);
      {$ENDIF}

      List := Next;
    End;

    _First := Nil;
    _ItemCount := 0;
  End;
  
  Self.Unlock();
End;

Function TERRAList.Add(Item:TERRAObject):Boolean;
Var
  N:Integer;
  Obj, P, Prev:TERRACollectionObject;
  Inserted:Boolean;
Begin
  Result := False;

  If (Item = Nil) Or ((Options And coCheckReferencesOnAdd<>0) And (Self.Contains(Item))) Then
  Begin
    Engine.Log.Write(logWarning, Self.ClassName, 'Reference already inside collection: '+Item.GetBlob());
    Exit;
  End;

  Obj := Self.NewItem(Item);

  Result := True;

  Self.Update();

  Self.Lock();
  If Assigned(_First) Then
  Begin
    If (_SortOrder = collection_Unsorted) Then
    Begin
      _Last.Next := Obj;
      Obj.Next := Nil;
      _Last := Obj;
    End Else
    {If (_SortOrder = collection_Unsorted) Then
    Begin
      P := _First;
      _First := Obj;
      Obj._Next := P;
    End Else}
    Begin
      Inserted := False;
      P := _First;
      Prev := Nil;
      While Assigned(P) Do
      Begin
        N := Self.CompareItems(P, Obj);
        If ((_SortOrder = collection_Sorted_Ascending) And (N>=0)) Or ((_SortOrder = collection_Sorted_Descending) And (N<=0)) Then
        Begin
          If Assigned(Prev) Then
          Begin
            Prev.Next := Obj;
            Obj.Next := P;
          End Else
          Begin
            _First := Obj;
            Obj.Next := P;
          End;

          Inserted := True;
          Break;
        End;

        Prev := P;
        P := P.Next;
      End;

      If Not Inserted Then
      Begin
        Prev.Next := Obj;
        Obj.Next := Nil;
      End;
    End;

  End Else
  Begin
    _First := Obj;
    _Last := _First;
    Obj.Next := Nil;
  End;

  Inc(_ItemCount);

  Self.Unlock();
End;

Function TERRAList.Remove(Item:TERRAObject):Boolean;
Var
  List,Prev, Next:TERRACollectionObject;
Begin
  Result := False;

  Self.Lock();
  List := _First;
  Prev := Nil;
  {$IFDEF DEBUG}Engine.Log.Write(logDebug, 'List', 'Testing deletion...');{$ENDIF}

  While Assigned(List) Do
  Begin
    {$IFDEF DEBUG}Engine.Log.Write(logDebug, 'List', 'Testing key contains '+HexStr(Cardinal(List)));{$ENDIF}
    If List.Item = Item Then
    Begin
      {$IFDEF DEBUG}Engine.Log.Write(logDebug, 'List', 'Match found!');{$ENDIF}

      Next := List.Next;

      If Assigned(Prev) Then
        Prev.Next := Next
      Else
        _First := Next;

      Dec(_ItemCount);
      Result := True;
      Break;
    End;

    Prev := List;
    List := List.Next;
  End;

  Self.Unlock();
End;

(*Function TERRAList.Contains(Item:TERRAObject):Boolean;
Var
  P:TERRAObject;
Begin
  Result := False;

  Self.Lock();
  P := _First;
  While (P<>Nil) Do
  Begin
    If (P = Item) Then
    Begin
      Result := True;
      Break;
    End;

    P := P.Next;
  End;
  Self.Unlock();
End;*)

Function TERRAList.GetIterator:TERRAIterator;
Begin
  Result := ListIterator(Engine.Pool.Fetch(ListIterator));
  If Assigned(Result) Then
    Result.Create(Self)
  Else
    Result := ListIterator.Create(Self);
End;

Function TERRAList.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  Result := Inherited CreateProperty(KeyName, ObjectType);

  If (Assigned(Result)) Then
  Begin
    Self.Add(Result);
  End;
End;

{ ListIterator }
Procedure ListIterator.Reset;
Begin
  Inherited;

  _Current := TERRAList(Self.Collection)._First;
End;

Function ListIterator.ObtainNext:TERRACollectionObject;
Begin
  If Assigned(_Current) Then
  Begin
    Result := _Current;

    _Current := _Current.Next;
  End Else
    Result := Nil;
End;

Class Function TERRAList.GetObjectType: TERRAString;
Begin
  Result := 'list';
End;

End.
