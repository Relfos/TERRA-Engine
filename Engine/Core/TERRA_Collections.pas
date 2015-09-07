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

Const
  { Insert fails if object already added }
  coCheckReferencesOnAdd    = 2;
  coCheckReferencesOnDelete = 4;

  { Items are not deleted from memory when Clear() or Release() is called on a collection }
  coShared = 8;

  { The collection will be thread-safe, meaning adds, deletes and iterators will be protected by a critical section. }
  coThreadSafe      = 32;

Type
  TERRACollection = Class;

  CollectionSortOrder = (
    { The collection is unsorted. Add() does a very fast insertion without sorting.}
    collection_Unsorted,

    { The collection is sorted}
    collection_Sorted_Ascending,

    { The collection is reversed sorted}
    collection_Sorted_Descending
  );

  // must implement at least copy value and sort
  TERRACollectionObject = Class(TERRAObject)
    Protected
      _Collection:TERRACollection;
      _Next:TERRACollectionObject;
      _Discarded:Boolean;

      _Item:TERRAObject;

    Public
      { Creates and links this object to a specific collection. }
      Constructor Create(Owner:TERRACollection; Item:TERRAObject);

      { Release this object }
      Procedure Release(); Override;

      { Mark this object for release. It will be auto-released as soon as possible.}
      Procedure Discard();

      Class Function CanBePooled:Boolean; Override;

      Property Collection:TERRACollection Read _Collection;
      Property Next:TERRACollectionObject Read _Next Write _Next; // FIXME should not have write acess
      Property Item:TERRAObject Read _Item;
      Property Discarded:Boolean Read _Discarded;
  End;

  Iterator = Class(TERRAObject)
    Private
      _Value:TERRACollectionObject;
      _Index:Integer;
      _Collection:TERRACollection;
      _Finished:Boolean;

      {$IFNDEF DISABLEALLOCOPTIMIZATIONS}
      Class Function NewInstance:TObject; Override;
      Procedure FreeInstance; Override;
      {$ENDIF}

    Protected

      Function ObtainNext():TERRACollectionObject; Virtual; Abstract;

      Procedure Reset(); Virtual;
      Procedure JumpToIndex(Position: Integer); Virtual;

      Procedure Release(); Override;

      Function GetPosition():Integer;

      Function GetValue():TERRAObject;

      Property Index:Integer Read _Index;

    Public
      Constructor Create(Collection:TERRACollection);

      Procedure Init(Collection:TERRACollection);

      Function HasNext():Boolean;

      Function Seek(Position:Integer):Boolean;

      Procedure Discard();

      Class Function CanBePooled:Boolean; Override;

      Property Value:TERRAObject Read GetValue;
      Property Position:Integer Read GetPosition;
      Property Collection:TERRACollection Read _Collection;
  End;

  TERRACollection = Class(TERRAObject)
    Protected
      _ItemCount:Integer;
      _Options:Cardinal;

      _SortOrder:CollectionSortOrder;
      _Share:TERRACollection;

      {$IFNDEF DISABLETHREADS}
      _Mutex:CriticalSection;
      {$ENDIF}

      _HasDiscards:Boolean;

      Procedure Init(Options:Integer; Share:TERRACollection);

      Procedure Update();
      Procedure RemoveDiscardedItems(); Virtual;

      Function NewItem(Content:TERRAObject):TERRACollectionObject;

      Function CompareItems(A,B:TERRACollectionObject):Integer;

    Public
      Procedure Release(); Override;

      Function GetIterator:Iterator; Virtual;

      // removes all items
      Procedure Clear(); Virtual;

      Procedure Lock;
      Procedure Unlock;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      // Should return True if Item contains Key
      Function Contains(Item:TERRAObject):Boolean; Virtual;

      // Returns true if deletion was sucessful
      Function Delete(Item:TERRAObject):Boolean;

      // Returns true if removal was sucessful
      Function Remove(Item:TERRAObject):Boolean; Virtual;

      Function GetItemByIndex(Index:Integer):TERRAObject; Virtual;

//      Function FindByValue(Const Value:TERRAString):CollectionObject; Virtual;

      Property Items[Index: Integer]:TERRAObject Read GetItemByIndex; Default;

      Property Count:Integer Read _ItemCount;
      Property Options:Cardinal Read _Options;

      Property SortOrder:CollectionSortOrder Read _SortOrder;
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
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Engine, TERRA_FileStream, TERRA_Stream
{$IFNDEF DISABLEALLOCOPTIMIZATIONS}, TERRA_StackObject{$ENDIF};

{$IFDEF DEBUG_ITERATORS}
Var
  _Iterator_Count:Integer;
{$ENDIF}

{ TERRACollectionObject }
Constructor TERRACollectionObject.Create(Owner:TERRACollection; Item:TERRAObject);
Begin
  Self._Collection := Owner;
  Self._Item := Item;
End;

Procedure TERRACollectionObject.Release();
Begin
  If (Assigned(_Collection)) And (_Collection.Options And coShared = 0) Then
    ReleaseObject(_Item);
End;

Procedure TERRACollectionObject.Discard();
Begin
  Self._Discarded := True;
  If Assigned(Self._Collection) Then
    Self._Collection._HasDiscards := True
  Else
    DebugBreak;
End;

Class Function TERRACollectionObject.CanBePooled: Boolean;
Begin
  Result := True;
End;

{ TERRACollection }
Procedure TERRACollection.Release();
Begin
  Self.Clear();

  {$IFNDEF DISABLETHREADS}
  ReleaseObject(_Mutex);
  {$ENDIF}
End;

Procedure TERRACollection.Lock();
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

Procedure TERRACollection.Unlock();
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

Procedure TERRACollection.Init(Options:Integer; Share:TERRACollection);
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

(*Function TERRACollection.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): CollectionObject;
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

Procedure TERRACollection.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  While (It.HasNext()) Do
  Begin
    Visitor(It.Value, UserData);
  End;
End;*)

Function TERRACollection.Contains(Item:TERRAObject):Boolean;
Var
  P:TERRAObject;
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

Function TERRACollection.Delete(Item:TERRAObject):Boolean;
Begin
  Result := Self.Remove(Item);
  
  If Result Then
    ReleaseObject(Item);
End;

{Function TERRACollection.FindByValue(Const Value:TERRAString): CollectionObject;
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

Procedure TERRACollection.RemoveDiscardedItems;
Begin
  // dummy
End;

Procedure TERRACollection.Update;
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

Function TERRACollection.GetItemByIndex(Index: Integer):TERRAObject;
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  It.Seek(Index);
  Result := It.Value;
  ReleaseObject(It);
End;

Function TERRACollection.GetIterator: Iterator;
Begin
  Result := Nil;
End;

Procedure TERRACollection.Clear;
Var
  It:Iterator;
Begin
  It := Self.GetIterator();
  While It.HasNext() Do
  Begin
    It.Discard();
  End;
  ReleaseObject(It);
End;

Function TERRACollection.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If (Index<Self.Count) Then
    Result := Self.GetItemByIndex(Index)
  Else
    Result := Nil;
End;

Function TERRACollection.CompareItems(A, B: TERRACollectionObject): Integer;
Var
  VA, VB:Integer;
Begin
  VA := A.Item.SortID();
  VB := B.Item.SortID();

  If (VA>VB) Then
    Result := 1
  Else
  If (VA<VB) Then
    Result := -1
  Else
    Result := 0;
End;

Function TERRACollection.Remove(Item: TERRAObject): Boolean;
Begin
  Result := False;
End;

Function TERRACollection.NewItem(Content:TERRAObject): TERRACollectionObject;
Begin
  Result := TERRACollectionObject(Engine.Pool.Fetch(TERRACollectionObject));
  If Assigned(Result) Then
  Begin
    Result._Item := Content;
    Result._Discarded := False;
    Exit;
  End;

  Result := TERRACollectionObject.Create(Self, Content);
End;

{ Iterator }
Constructor Iterator.Create(Collection:TERRACollection);
Begin
  Self.Init(Collection);
End;

Procedure Iterator.Init(Collection:TERRACollection);
Begin
  _Collection := Collection;

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

Function Iterator.GetValue():TERRAObject;
Begin
  Result := _Value.Item;
End;

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

Procedure Iterator.Discard;
Begin
  If Assigned(_Value) Then
    _Value.Discard();
End;

Class Function Iterator.CanBePooled: Boolean;
Begin
  Result := True;
End;

End.


