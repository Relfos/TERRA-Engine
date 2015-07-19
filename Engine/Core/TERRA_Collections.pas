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
}

{ Implements generic collection classes (lists and iterators) }
Unit TERRA_Collections;

{$I terra.inc}

{-$DEFINE DEBUG}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils
{$IFNDEF DISABLETHREADS}, TERRA_Mutex{$ENDIF};

Function GetStringSort(Const A,B:TERRAString):Integer;

Const
  { Insert fails if object already added }
  coCheckReferencesOnAdd    = 2;
  coCheckReferencesOnDelete = 4;
  { The collection will be thread-safe, meaning adds, deletes and iterators will be protected by a critical section. }
  coThreadSafe      = 32;

Type
  Collection = Class;

  CollectionSortOrder = (
    { The collection is unsorted. Add() does a very fast insertion without sorting.}
    collection_Unsorted,

    { The collection is sorted}
    collection_Sorted_Ascending,

    { The collection is reversed sorted}
    collection_Sorted_Descending
  );

  // must implement at least copy value and sort
  CollectionObject = Class(TERRAObject)
    Protected
      _Collection:Collection;
      _Next:CollectionObject;
      _Discarded:Boolean;

      Function IsCompatible(Other:CollectionObject):Boolean;
      Procedure Internal_Copy(Other:CollectionObject);
      Function Internal_Sort(Other:CollectionObject):Integer;

    Public
      { Release this object }
      Procedure Release(); Override;

      { Mark this object for release. It will be auto-released as soon as possible.}
      Procedure Discard(); 

      { Links this object to a specific collection. Internal use. }
      Procedure Link(Col:Collection);

      { Clones this object. }
      Procedure CopyValue(Other:CollectionObject); Virtual;

      Function ToString():TERRAString; {$IFDEF FPC}Reintroduce;{$ENDIF}  Virtual;

      { Compares this object with a similar object. If not implemented, no sorting will happen. }
      Function Sort(Other:CollectionObject):Integer; Virtual;

      Property Collection:Collection Read _Collection;
      Property Next:CollectionObject Read _Next Write _Next; // FIXME should not have write acess
      Property Discarded:Boolean Read _Discarded;
  End;

  Iterator = Class(TERRAObject)
    Private
      _Value:CollectionObject;
      _Index:Integer;
      _Collection:Collection;
      _Finished:Boolean;

      {$IFNDEF DISABLEALLOCOPTIMIZATIONS}
      Class Function NewInstance:TObject; Override;
      Procedure FreeInstance; Override;
      {$ENDIF}

    Protected

      Function ObtainNext():CollectionObject; Virtual; Abstract;

      Procedure Reset(); Virtual;
      Procedure JumpToIndex(Position: Integer); Virtual;

      Procedure Release(); Override;

      Function GetPosition():Integer;

      Property Index:Integer Read _Index;

    Public
      Constructor Create(Col:Collection);

      Function HasNext():Boolean;

      Function Seek(Position:Integer):Boolean;

      Property Value:CollectionObject Read _Value;
      Property Position:Integer Read GetPosition;
      Property Collection:TERRA_Collections.Collection Read _Collection;
  End;

  CollectionVisitor = Function(Item:CollectionObject; UserData:Pointer):Boolean; CDecl;

  Collection = Class(TERRAObject)
    Protected
      _ItemCount:Integer;
      _Options:Cardinal;

      _SortOrder:CollectionSortOrder;
      _Share:Collection;

      {$IFNDEF DISABLETHREADS}
      _Mutex:CriticalSection;
      {$ENDIF}

      _HasDiscards:Boolean;

      Procedure Init(Options:Integer; Share:Collection);

      Procedure Update();
      Procedure RemoveDiscardedItems(); Virtual;

    Public
      Procedure Release(); Override;

      Function GetIterator:Iterator; Virtual;

      // removes all items
      Procedure Clear(); Virtual; 

      Procedure Lock;
      Procedure Unlock;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):CollectionObject; Virtual;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Virtual; 

      // Should return True if Item contains Key
      Function Contains(Item:CollectionObject):Boolean; Virtual;

      // Returns true if deletion was sucessful
      Function Delete(Item:CollectionObject):Boolean; 

      // Returns true if removal was sucessful
      Function Remove(Item:CollectionObject):Boolean; Virtual; Abstract;

      Function GetItemByIndex(Index:Integer):CollectionObject; Virtual;

//      Function FindByValue(Const Value:TERRAString):CollectionObject; Virtual;

      Property Objects[Index: Integer]:CollectionObject Read GetItemByIndex; Default;

      Property Count:Integer Read _ItemCount;
      Property Options:Cardinal Read _Options;

      Property SortOrder:CollectionSortOrder Read _SortOrder;
  End;

  List = Class(Collection)
    Protected
      _First:CollectionObject;
      _Last:CollectionObject;

      Procedure RemoveDiscardedItems(); Override;

    Public
      Constructor Create(SortOrder:CollectionSortOrder = collection_Unsorted; Options:Integer = 0);

      Procedure Clear(); Override;
      Function GetIterator:Iterator; Override;

      Function GetItemByIndex(Index:Integer):CollectionObject; Override;

      // adds copies of items, not references!
      Function Merge(C:Collection):List;

      // Returns true if insertion was sucessful
      Function Add(Item:CollectionObject):Boolean;Virtual;

      // Returns true if removal was sucessful
      Function Remove(Item:CollectionObject):Boolean; Override;

      Function Contains(Item:CollectionObject):Boolean; Override;

      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):CollectionObject; Override;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Override;

      Property First:CollectionObject Read _First;
  End;

  ListIterator = Class(Iterator)
    Protected
      _Current:CollectionObject;

      Function ObtainNext:CollectionObject; Override;
      Procedure Reset(); Override;

    Public
  End;

  IntegerArrayObject = Object
    Items:Array Of Integer;
    Count:Integer;

    Procedure Clear;
    Procedure Replace(A,B:Integer);
    Function Contains(N:Integer):Boolean;
    Procedure Add(N:Integer; AllowDuplicates:Boolean = False);
    Procedure Remove(N:Integer);

    Procedure Merge(A:IntegerArrayObject);

    Function Pop:Integer;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_FileStream, TERRA_Stream
{$IFNDEF DISABLEALLOCOPTIMIZATIONS}, TERRA_StackObject{$ENDIF};

{$IFDEF DEBUG_ITERATORS}
Var
  _Iterator_Count:Integer;
{$ENDIF}

Function GetStringSort(Const A,B:TERRAString):Integer;
Begin
  If (A<B) Then
    Result := 1
  Else
  If (A>B) Then
    Result := -1
  Else
    Result := 0;
End;

{ Collection }
Procedure Collection.Release();
Begin
  Self.Clear();

  {$IFNDEF DISABLETHREADS}
  ReleaseObject(_Mutex);
  {$ENDIF}
End;

Procedure Collection.Lock();
Begin
  {$IFNDEF DISABLETHREADS}
  If Assigned(_Share) Then
  Begin
    _Share.Lock();
  End Else
  If Assigned(_Mutex) Then
  Begin
    _Mutex.Lock();
  End;
  {$ENDIF}
End;

Procedure Collection.Unlock();
Begin
  {$IFNDEF DISABLETHREADS}
  If Assigned(_Share) Then
  Begin
    _Share.Unlock();
  End Else
  If Assigned(_Mutex) Then
    _Mutex.Unlock();
  {$ENDIF}
End;

Procedure Collection.Init(Options:Integer; Share:Collection);
Begin
  {$IFNDEF DISABLETHREADS}
  If (Assigned(Share)) Then
  Begin
    _Share := Share;
    _Mutex := Nil;
  End Else
  Begin
    _Share := Nil;
    If (Options And coThreadSafe<>0) Then
      _Mutex := CriticalSection.Create({Self.ClassName +CardinalToString(GetTime())})
    Else
      _Mutex := Nil;
  End;
  {$ENDIF}

  _Options := Options;
End;

Function Collection.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): CollectionObject;
Var
  It:Iterator;
  P:CollectionObject;
Begin
  Result := Nil;

  It := Self.GetIterator();
  While (It.HasNext()) Do
  Begin
    P := It.Value;
    If (Visitor(P, UserData)) Then
    Begin
      Result := P;
      Break;
    End;
  End;
  ReleaseObject(It);
End;

Procedure Collection.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  While (It.HasNext()) Do
  Begin
    Visitor(It.Value, UserData);
  End;
End;

Function Collection.Contains(Item:CollectionObject):Boolean;
Var
  P:CollectionObject;
  It:Iterator;
Begin
  Result := False;
  It := Self.GetIterator();
  While (It.HasNext()) Do
  Begin
    If (It.Value = Item) Then
    Begin
      Result := True;
      Break;
    End;
  End;
  ReleaseObject(It);
End;

Function Collection.Delete(Item:CollectionObject):Boolean;
Begin
  Result := Self.Remove(Item);
  If Result Then
    ReleaseObject(Item);
End;

{Function Collection.FindByValue(Const Value:TERRAString): CollectionObject;
Var
  It:Iterator;
  P:CollectionObject;
Begin
  Result := Nil;
  It := Self.GetIterator();
  While It.HasNext() Do
  Begin
    P := It.GetNext();
    If StringEquals(P.ToString(), Value) Then
    Begin
      Result := P;
      Break;
    End;
  End;
  ReleaseObject(It);
End;}

Procedure Collection.RemoveDiscardedItems;
Begin
  // dummy
End;

Procedure Collection.Update;
Begin
  If Not _HasDiscards Then
    Exit;

  _HasDiscards := False;
  Self.RemoveDiscardedItems();
End;

Procedure IntegerArrayObject.Clear;
Begin
  Count := 0;
  SetLength(Items, 0);
End;

Function IntegerArrayObject.Contains(N:Integer):Boolean;
Var
  I:Integer;
Begin
  Result := True;
  For I:=0 To Pred(Count) Do
  If (Items[I]=N) Then
    Exit;

  Result := False;
End;

Procedure IntegerArrayObject.Replace(A,B:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Count) Do
  If (Items[I]=A) Then
    Items[I] := B;
End;

Procedure IntegerArrayObject.Add(N:Integer; AllowDuplicates:Boolean = False);
Begin
  If (Not AllowDuplicates) And (Contains(N)) Then
    Exit;

  Inc(Count);
  SetLength(Items, Count);
  Items[Pred(Count)] := N;
End;

Procedure IntegerArrayObject.Remove(N:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Count) Do
  If (Items[I]=N) Then
  Begin
    Items[I] := Items[Pred(Count)];
    Dec(Count);
    SetLength(Items, Count);
    Exit;
  End;
End;

Procedure IntegerArrayObject.Merge(A:IntegerArrayObject);
Var
  I:Integer;
Begin
  For I:=0 To Pred(A.Count) Do
    Self.Add(A.Items[I]);
End;

Function IntegerArrayObject.Pop:Integer;
Begin
  Result := Items[0];
  Items[0] := Items[Pred(Count)];
  Dec(Count);
  SetLength(Items, Count);
End;

Function Collection.GetItemByIndex(Index: Integer): CollectionObject;
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  It.Seek(Index);
  Result := It.Value;
  ReleaseObject(It);
End;

Function Collection.GetIterator: Iterator;
Begin
  Result := Nil;
End;

Procedure Collection.Clear;
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  While It.HasNext() Do
  Begin
    It.Value.Discard();
  End;
  ReleaseObject(It);
End;

Function Collection.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If (Index<Self.Count) Then
    Result := Self.GetItemByIndex(Index)
  Else
    Result := Nil;
End;

{ Iterator }
Constructor Iterator.Create(Col: Collection);
Begin
  _Collection := Col;

  If Assigned(_Collection) Then
  Begin
    _Collection.Lock();
    _Collection.Update();
  End;

{$IFDEF DEBUG_ITERATORS}
  Inc(_Iterator_Count);

  If (_Iterator_Count>100) Then
  Begin
    DebugBreak;
  End;
{$ENDIF}

  Self.Seek(0);
End;

{$IFNDEF DISABLEALLOCOPTIMIZATIONS}
Class Function Iterator.NewInstance: TObject;
Var
  ObjSize, GlobalSize:Integer;
Begin
  ObjSize := InstanceSize();
  Result := StackAlloc(ObjSize);
  InitInstance(Result);
End;

Procedure Iterator.FreeInstance;
Begin
End;
{$ENDIF}

Procedure Iterator.Reset();
Begin
  // do nothing
End;

Procedure Iterator.Release();
Begin
  If _Finished Then
    Exit;

  _Finished := True;

{$IFDEF DEBUG_ITERATORS}
  Dec(_Iterator_Count);
{$ENDIF}

  If Assigned(_Collection) Then
  Begin
    _Collection.Update();
    _Collection.Unlock();
  End;
End;

Function Iterator.HasNext: Boolean;
Begin
  _Value := Self.ObtainNext();
  Result := Assigned(_Value);
  If (Result) Then
    Inc(_Index)
  Else
  If (Not _Finished) Then
    Self.Release();
End;

Function Iterator.GetPosition():Integer;
Begin
  Result := Pred(_Index);
End;

Function Iterator.Seek(Position: Integer):Boolean;
Begin
  _Finished := False;
  _Value := Nil;

  _Index := 0;
  Self.Reset();

  If Position>0 Then
  Begin
    JumpToIndex(Position);
    _Index := Position;
  End;

  If Assigned(_Collection) Then
    Result := (Position<Self._Collection.Count)
  Else
    Result := True;
End;

Procedure Iterator.JumpToIndex(Position: Integer);
Begin
  While (Self.Index<Position) Do
  Begin
    If Not Self.HasNext() Then
      Exit;
  End;
End;

{ CollectionObject }
Procedure CollectionObject.CopyValue(Other: CollectionObject);
Begin
  // do nothing
End;

Procedure CollectionObject.Release();
Begin
  // do nothing
End;

Procedure CollectionObject.Internal_Copy(Other: CollectionObject);
Begin
  If (IsCompatible(Other)) Then
    Self.CopyValue(Other);
End;

Function CollectionObject.Internal_Sort(Other: CollectionObject): Integer;
Begin
  If (IsCompatible(Other)) Then
    Result := Self.Sort(Other)
  Else
    Result := 0;
End;

Function CollectionObject.IsCompatible(Other: CollectionObject): Boolean;
Begin
  Result := (Self Is Other.ClassType);
  If Not Result Then
    RaiseError('Cannot copy list objects, '+Self.ClassName +' and '+ Other.ClassName+' are not compatible!');
End;

Function CollectionObject.Sort(Other: CollectionObject): Integer;
Begin
  If (Other<>Nil) Then
    Result := 0
  Else
    Result := 1;
End;

Function CollectionObject.ToString:TERRAString;
Begin
  Result := Self.ClassName+'@'+HexStr(Cardinal(Self));
End;

Procedure CollectionObject.Discard();
Begin
  Self._Discarded := True;
  If Assigned(Self._Collection) Then
    Self._Collection._HasDiscards := True
  Else
    DebugBreak;
End;


Procedure CollectionObject.Link(Col: Collection);
Begin
  _Collection := Col;
End;

{ List }
Constructor List.Create(SortOrder:CollectionSortOrder; Options:Integer);
Begin
  _SortOrder := SortOrder;
  _First := Nil;
  _ItemCount := 0;
  Self.Init(Options, Nil);
End;

Procedure List.RemoveDiscardedItems();
Var
  P, Prev:CollectionObject;
Begin
  Prev := Nil;

  P := _First;
  While (Assigned(P)) Do
  Begin
    If P._Discarded Then
    Begin
      If Assigned(Prev) Then
      Begin
        Prev._Next := P.Next;
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

Function List.GetItemByIndex(Index:Integer):CollectionObject;
Var
  I:Integer;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Index=0) Then
  Begin
    Result := _First;
    Exit;
  End;

  Self.Lock();
  Result := Self._First;
  I := 0;
  While (Result<>Nil) Do
  Begin
    Result := Result.Next;
    Inc(I);

    If (I = Index) Then
      Break;
  End;
  Self.Unlock();
End;

Function List.Merge(C:Collection):List;
Var
  I:Iterator;
  Temp, N:CollectionObject;
Begin
  Result := Self;

  If (C = Self) Then
    Exit;

  Self.Update();

  I := C.GetIterator();
  While I.HasNext Do
  Begin
    Temp := I.Value;
    N := CollectionObject(Temp.ClassType.Create());
    N.CopyValue(Temp);
    Self.Add(N);
  End;
  ReleaseObject(C);
End;

Function List.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): CollectionObject;
Var
  P:CollectionObject;
Begin
  Result := Nil;

  Self.Lock();
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Visitor(P, UserData)) Then
    Begin
      Result := P;
      Break;
    End;

    P := P.Next;
  End;

  Self.Unlock();
End;

Procedure List.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  P:CollectionObject;
Begin
  Self.Lock();
  P := _First;
  While (Assigned(P)) Do
  Begin
    Visitor(P, UserData);
    P := P.Next;
  End;
  Self.Unlock();
End;

Procedure List.Clear();
Var
  List,Next:CollectionObject;
Begin
  Self.Lock();

  List := _First;
  While Assigned(List)Do
  Begin
    Next := List.Next;
    ReleaseObject(List);
    List := Next;
  End;

  _First := Nil;
  _ItemCount := 0;

  Self.Unlock();
End;

Function List.Add(Item:CollectionObject):Boolean;
Var
  N:Integer;
  P, Prev:CollectionObject;
  Inserted:Boolean;
Begin
  Result := False;

  If (Item = Nil) Or ((Options And coCheckReferencesOnAdd<>0) And (Self.Contains(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Reference already inside collection: '+Item.ToString());
    Exit;
  End;

  If (Item.Collection<>Nil) Then
  Begin
    Log(logWarning, Self.ClassName, 'Item already belongs to a collection: '+Item.ToString());
    Exit;
  End;

  Result := True;
  Item._Collection := Self;

  Self.Update();

  Self.Lock();
  If Assigned(_First) Then
  Begin
    If (_SortOrder = collection_Unsorted) Then
    Begin
      _Last._Next := Item;
      Item._Next := Nil;
      _Last := Item;
    End Else
    {If (_SortOrder = collection_Unsorted) Then
    Begin
      P := _First;
      _First := Item;
      Item._Next := P;
    End Else}
    Begin
      Inserted := False;
      P := _First;
      Prev := Nil;
      While Assigned(P) Do
      Begin
        N := P.Sort(Item);
        If ((_SortOrder = collection_Sorted_Ascending) And (N>=0)) Or ((_SortOrder = collection_Sorted_Descending) And (N<=0)) Then
        Begin
          If Assigned(Prev) Then
          Begin
            Prev._Next := Item;
            Item._Next := P;
          End Else
          Begin
            _First := Item;
            Item._Next := P;
          End;

          Inserted := True;
          Break;
        End;

        Prev := P;
        P := P.Next;
      End;

      If Not Inserted Then
      Begin
        Prev._Next := Item;
        Item._Next := Nil;
      End;
    End;

  End Else
  Begin
    _First := Item;
    _Last := _First;
    Item._Next := Nil;
  End;

  Inc(_ItemCount);

  Self.Unlock();

  {P := _First;
  PRev := Nil;
  While P<>Nil Do
  Begin
    S := S + P.ToString() + ',';
    Prev := P;
    P := P.Next;
  End;
  IF (Prev<>_Last) Then
  Begin
    S := Item.ToString();
    IntToSTring(2+Length(S));
  End;}
End;

Function List.Remove(Item:CollectionObject):Boolean;
Var
  List,Prev, Next:CollectionObject;
Begin
  Result := False;

  Self.Lock();
  List := _First;
  Prev := Nil;
  {$IFDEF DEBUG}Log(logDebug, 'List', 'Testing deletion...');{$ENDIF}

  While Assigned(List) Do
  Begin
    {$IFDEF DEBUG}Log(logDebug, 'List', 'Testing key contains '+HexStr(Cardinal(List)));{$ENDIF}
    If List = Item Then
    Begin
      {$IFDEF DEBUG}Log(logDebug, 'List', 'Match found!');{$ENDIF}

      Next := Item.Next;

      If Assigned(Prev) Then
        Prev._Next := Next
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

Function List.Contains(Item:CollectionObject):Boolean;
Var
  P:CollectionObject;
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
End;

Function List.GetIterator:Iterator;
Begin
  Result := ListIterator.Create(Self);
End;

Procedure ListIterator.Reset;
Begin
  Inherited;

  _Current := List(_Collection)._First;
End;

Function ListIterator.ObtainNext:CollectionObject;
Begin
  If Assigned(_Current) Then
  Begin
    Result := _Current;

    _Current := _Current.Next;
  End Else
    Result := Nil;
End;


Function List.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  Result := Inherited CreateProperty(KeyName, ObjectType);

  If (Assigned(Result)) And (Result Is CollectionObject) Then
  Begin
    Self.Add(CollectionObject(Result));
  End;
End;

End.






