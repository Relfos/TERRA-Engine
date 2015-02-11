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
 * TERRA_Collections
 * Implements generic collection classes (lists, queues, stacks, etc)
 ***********************************************************************************************************************
}

Unit TERRA_Collections;

{$I terra.inc}

{-$DEFINE DEBUG}

Interface
Uses TERRA_String, TERRA_Utils;

Type
  HashKey = Word;

Function GetStringHashKey(Const S:TERRAString):HashKey;
Function GetStringSort(Const A,B:TERRAString):Integer;

Const
  coAppend          = 1;
  coNoDuplicates    = 2;
  coSorted          = 4;
  coSortedInverted  = 8;
  coCheckReferencesOnAdd    = 16;
  coCheckReferencesOnDelete = 32;
  coUnmanaged       = 64;
  DefaultCollectionOptions = coAppend;

Type
  Collection = Class;

  // must implement at least copy value and sort
  ListObject = Class(TERRAObject)
    Protected
      _Collection:Collection;
      _Next:ListObject;

      Procedure CopyValue(Other:ListObject); Virtual; 
      Function Sort(Other:ListObject):Integer; Virtual; // if not implemented, no sort will happen
      Function GetHashKey():HashKey; Virtual; // does not need to be unique

      Function IsCompatible(Other:ListObject):Boolean;
      Procedure Internal_Copy(Other:ListObject);
      Function Internal_Sort(Other:ListObject):Integer;

    Public
      Destructor Destroy; Override;

      Function ToString():TERRAString; {$IFDEF FPC} Override; {$ELSE} Virtual; {$ENDIF}

      Property Collection:Collection Read _Collection;
      Property  Next:ListObject Read _Next;
  End;


  Iterator = Class(TERRAObject)
    Public
      Function HasNext():Boolean;Virtual; Abstract;
      Function GetNext():ListObject;Virtual; Abstract;
      Function Discard():Boolean; Virtual;
      Destructor Destroy(); Override; 
  End;

  CollectionVisitor = Function(Item:ListObject; UserData:Pointer):Boolean; CDecl;

  Collection = Class(TERRAObject)
    Protected
      _ItemCount:Integer;
      _Options:Integer;

      Procedure SetOptions(Value:Integer);

    Public
      Destructor Destroy; Override;

      Function CreateIterator:Iterator; Virtual;Abstract;

      // removes all items
      Procedure Clear(); Virtual;Abstract;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject; Virtual; Abstract;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Virtual; Abstract;

      // Should return True if Item contains Key
      Function ContainsReference(Item:ListObject):Boolean; Virtual; Abstract;
      Function ContainsDuplicate(Item:ListObject):Boolean; Virtual; Abstract;

      Function GetItemByIndex(Index:Integer):ListObject; Virtual; Abstract;

      Function FindByKey(Const Key:TERRAString):ListObject; Virtual;

      Property Objects[Index: Integer]:ListObject Read GetItemByIndex; Default;

      Property Count:Integer Read _ItemCount;
      Property Options:Integer Read _Options;
  End;

  Queue = Class(Collection)
    Protected
      _First:ListObject;

    Public
      Constructor Create();

      Function CreateIterator:Iterator; Override;

      Procedure Clear(); Override;

      Function Push(Item:ListObject):Boolean; Virtual;
      Function Pop():ListObject; Virtual;

      Function GetItemByIndex(Index:Integer):ListObject; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject; Override;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Override;

      Function ContainsReference(Item:ListObject):Boolean; Override;
      Function ContainsDuplicate(Item:ListObject):Boolean; Override;

      Property First:ListObject Read _First;
  End;

  Stack = Class(Queue)
    Public
      Function Push(Item:ListObject):Boolean; Override;
      Function Pop():ListObject; Override;
  End;

  List = Class(Collection)
    Protected
      _First:ListObject;
      _Last:ListObject;

    Public
      Constructor Create(Options:Integer = DefaultCollectionOptions);

      Procedure Clear(); Override;
      Function CreateIterator:Iterator; Override;

      Function GetItemByIndex(Index:Integer):ListObject; Override;

      // adds copies of items, not references!
      Function Merge(C:Collection):List;

      // Returns true if insertion was sucessful
      Function Add(Item:ListObject; SkipSorting:Boolean = False):Boolean;Virtual;
      // Returns true if deletion was sucessful
      Function Delete(Item:ListObject):Boolean; Virtual;

      Function ContainsReference(Item:ListObject):Boolean; Override;
      Function ContainsDuplicate(Item:ListObject):Boolean; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject; Override;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Override;

      Property First:ListObject Read _First;
  End;

  PoolItem = Record
    Used:Boolean;
    Obj:ListObject;
  End;

  Pool = Class(List)
    Protected
      _Objects:Array Of PoolItem;
      _ObjectCount:Integer;

      Procedure InsertPoolObject(Obj:ListObject);

    Public
      Destructor Destroy(); Override;

      Procedure Clear(); Override;

      Function Recycle():ListObject;

      // Returns true if insertion was sucessful
      Function Add(Item:ListObject; SkipSorting:Boolean = False):Boolean; Override;
  End;

  ListIterator=Class(Iterator)
    Protected
      _List:List;
      _Item,_Prev,_AntPrev:ListObject;

    Public
      Constructor Create(Source:List);

      Function HasNext:Boolean;Override;
      Function GetNext:ListObject;Override;
      Function Discard():Boolean;Override;
    End;

  HashTable = Class(Collection)
    Protected
      _Table:Array Of List;
      _TableSize:Word;

    Public
      Constructor Create(TableSize:Word);

      // Returns true if insertion was sucessful
      Function Add(Item:ListObject):Boolean;Virtual;
      // Returns true if deletion was sucessful
      Function Delete(Item:ListObject):Boolean; Virtual;

      Function ContainsReference(Item:ListObject):Boolean; Override;
      Function ContainsDuplicate(Item:ListObject):Boolean; Override;

      Function GetItemByIndex(Index:Integer):ListObject; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject; Override;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Override;
      Function Filter(Visitor:CollectionVisitor; UserData:Pointer = Nil):HashTable;

      Function SearchWithHash(HashKey:Word; Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject;

      Procedure Clear(); Override;

      Function CreateIteratorWithHash(HashKey:Word):Iterator;
      Function CreateIterator:Iterator; Override;
  End;

  HashTableIterator = Class(Iterator)
    Protected
      _CurrentTable:Integer;
      _ItemIndex:Integer;
      _Item:ListObject;
      _HashTable:HashTable;

      Function FindNextItem():ListObject;


    Public
      Constructor Create(Table:HashTable);

      Function HasNext:Boolean; Override;
      Function GetNext:ListObject; Override;
  End;

  IntegerArray = Object
    Items:Array Of Integer;
    Count:Integer;

    Procedure Clear;
    Procedure Replace(A,B:Integer);
    Function Contains(N:Integer):Boolean;
    Procedure Add(N:Integer; AllowDuplicates:Boolean = False);
    Procedure Remove(N:Integer);

    Procedure Merge(A:IntegerArray);

    Function Pop:Integer;
  End;

  // sample list objects
  StringObject=Class(ListObject)
    Public
      Value:TERRAString;

      Constructor Create(S:TERRAString);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

  IntegerObject = Class(ListObject)
    Public
      Value:Integer;

      Constructor Create(S:Integer);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

  KeyPairObject=Class(ListObject)
    Public
      Key:TERRAString;
      Value:TERRAString;

      Constructor Create(Key, Value:TERRAString);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

Function LoadKeypairList(SourceFile:TERRAString):List;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_FileStream, TERRA_Stream;

Function GetStringHashKey(Const S:TERRAString):HashKey;
Var
  N, I:Integer;
Begin
  Result := 0;
  N := 1;
  For I:=1 To Length(S) Do
  Begin
    Inc(Result, (Byte(S[I]) Mod 8) Shl N);
    Inc(N);
    If (N>4) Then
      N:=1;
  End;
End;

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

//####################
//#    Collection   #
//####################

Destructor Collection.Destroy;
Begin
  Self.Clear();
End;


Procedure Collection.SetOptions(Value: Integer);
Var
  Append:Boolean;
  SortedAsc, SortedDesc:Boolean;
  Ref1,Ref2:Boolean;
Begin
  Append := (Value And coAppend)<>0;
  SortedAsc := (Value And coSorted)<>0;
  SortedDesc := (Value And coSortedInverted)<>0;
  Ref1 := (Value And coCheckReferencesOnAdd)<>0;
  Ref2 := (Value And coCheckReferencesOnDelete)<>0;

  If (Append) And (SortedAsc) Then
  Begin
    Log(logDebug, Self.ClassName, 'Cannot use append and sort mode simultaneously!');
    Value := Value Xor coAppend;
  End;

  If (SortedDesc) And (SortedAsc) Then
  Begin
    Log(logDebug, Self.ClassName, 'Cannot use sort and sort_inverted mode simultaneously!');
    Value := Value Xor coSortedInverted;
  End;

  If (Ref1) And (Ref2) Then
  Begin
    Log(logDebug, Self.ClassName, 'Cannot use check references on add and on delete simultaneously!');
    Value := Value Xor coCheckReferencesOnDelete;
  End;

  _Options := Value;
End;

Function Collection.FindByKey(Const Key:TERRAString): ListObject;
Var
  It:Iterator;
  P:ListObject;
Begin
  Result := Nil;
  It := Self.CreateIterator;
  While It.HasNext() Do
  Begin
    P := It.GetNext();
    If StringEquals(P.ToString(), Key) Then
    Begin
      Result := P;
      Break;
    End;
  End;
  It.Destroy;
End;

{ List }
Constructor List.Create(Options:Integer);
Begin
  SetOptions(Options);
  _First := Nil;
  _ItemCount := 0;
End;

Function List.GetItemByIndex(Index:Integer):ListObject;
Var
  I:Integer;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := _First;
  If (Index=0) Then
    Exit;

  I := 0;
  While (Result<>Nil) Do
  Begin
    Result := Result.Next;
    Inc(I);

    If (I = Index) Then
      Exit;
  End;

  Result := Nil;
End;

(*Function List.GetItemByIndex(Index:Integer):ListObject;
Var
  I:Iterator;
Begin
  Result:=Nil;
  If (Index<0) Or (Index>=Self.Count) Then
    Exit;

  I := Self.CreateIterator;
  While I.HasNext Do
  Begin
    Result := I.GetNext;
    If (Index=0) Then
      Break
    Else
      Dec(Index);
  End;
  I.Destroy();
End;*)

Function List.Merge(C:Collection):List;
Var
  I:Iterator;
  Temp, N:ListObject;
Begin

  I := C.CreateIterator();
  While I.HasNext Do
  Begin
    Temp := I.GetNext();
    N := ListObject(Temp.ClassType.Create());
    N.CopyValue(Temp);
    Self.Add(N);
  End;
  I.Destroy();

  C.Destroy();

  Result := Self;
End;

Function List.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): ListObject;
Var
  P:ListObject;
Begin
  Result := Nil;
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Visitor(P, UserData)) Then
    Begin
      Result := P;
      Exit;
    End;

    P := P.Next;
  End;
End;

Procedure List.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  P:ListObject;
Begin
  P := _First;
  While (Assigned(P)) Do
  Begin
    Visitor(P, UserData);
    P := P.Next;
  End;
End;

Procedure List.Clear();
Var
  List,Next:ListObject;
Begin
  If (Options And coUnmanaged=0) Then
  Begin
    List := _First;
    While Assigned(List)Do
    Begin
      Next := List.Next;
      List.Destroy;
      List := Next;
    End;
  End;

  _First := Nil;
  _ItemCount := 0;
End;

Function List.Add(Item:ListObject; SkipSorting:Boolean = False):Boolean;
Var
  N:Integer;
  P, Prev:ListObject;
  Inserted:Boolean;
Begin
  Result := False;

  If (Item = Nil) Or ((Options And coCheckReferencesOnAdd<>0) And (Self.ContainsReference(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Reference already inside collection: '+Item.ToString());
    Exit;
  End;

  If ((Options And coNoDuplicates<>0) And (Self.ContainsDuplicate(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Item duplicated in collection: '+Item.ToString());
    Exit;
  End;

  If (Options And coUnmanaged=0) And (Item.Collection<>Nil) Then
  Begin
    Log(logWarning, Self.ClassName, 'Item already belongs to a collection: '+Item.ToString());
    Exit;
  End;

  Result := True;
  Item._Collection := Self;

  If Assigned(_First) Then
  Begin
    If ((Options And coAppend)<>0) Or (SkipSorting) Then
    Begin
      _Last._Next := Item;
      Item._Next := Nil;
      _Last := Item;
    End Else
    If ((Options and coSorted)=0) And ((Options and coSortedInverted)=0) Then
    Begin
      P := _First;
      _First := Item;
      Item._Next := P;
    End Else
    Begin
      Inserted := False;
      P := _First;
      Prev := Nil;
      While Assigned(P) Do
      Begin
        N := P.Sort(Item);
        If (((Options and coSorted)<>0) And (N>=0)) Or (((Options and coSortedInverted)<>0) And (N<=0)) Then
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

Function List.Delete(Item:ListObject):Boolean;
Var
  List,Prev, Next:ListObject;
Begin
  Result := False;
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

      Item.Destroy;
      {$IFDEF DEBUG}Log(logDebug, 'List', 'Discarded item!');{$ENDIF}

      If Assigned(Prev) Then
        Prev._Next := Next
      Else
        _First := Next;

      Dec(_ItemCount);
      Result := True;
      Exit;
    End;

    Prev := List;
    List := List.Next;
  End;

  {$IFDEF DEBUG}Log(logDebug, 'List', 'No matches found...');{$ENDIF}
End;

Function List.ContainsReference(Item:ListObject):Boolean;
Var
  P:ListObject;
Begin
  Result := True;
  P := _First;
  While (P<>Nil) Do
  Begin
    If (P = Item) Then
      Exit;

    P := P.Next;
  End;
  Result := False;
End;

Function List.ContainsDuplicate(Item:ListObject):Boolean;
Var
  P:ListObject;
Begin
  Result := True;
  P := _First;
  While (P<>Nil) Do
  Begin
    If (P.Sort(Item) = 0) Then
      Exit;

    P := P.Next;
  End;
  Result := False;
End;


Function List.CreateIterator:Iterator;
Begin
  Result := ListIterator.Create(Self);
End;


Constructor ListIterator.Create(Source:List);
Begin
  _List := Source;
  _Item := Source._First;
  _Prev := Nil;
  _AntPrev := Nil;
End;

Function ListIterator.GetNext:ListObject;
Begin
  Result := _Item;
  _AntPrev := _Prev;
  _Prev := _Item;
  _Item := _Item.Next;
End;

Function ListIterator.HasNext:Boolean;
Begin
  Result := Assigned(_Item);
End;

Function ListIterator.Discard():Boolean;
Var
  Next:ListObject;
Begin
  Result := False;

  If Not Assigned(_Prev) Then
    Exit;

  _Item:=_Prev;
  _Prev:=_AntPrev;
  Next:=_Item.Next;
  If Assigned(_Prev)Then
    _Prev._Next:=_Item.Next
  Else
    _List._First := _Item.Next;

  _Item.Destroy;

  Dec(_List._ItemCount);
  _Item := Next;

  Result := True;
End;

//###############################
//#         HashTable           #
//###############################

Constructor HashTable.Create(TableSize:Word);
Begin
  _Options := coAppend;
  _ItemCount := 0;
  _TableSize := TableSize;
  SetLength(_Table, _TableSize);
End;

Function HashTable.SearchWithHash(HashKey:Word; Visitor:CollectionVisitor; UserData:Pointer = Nil):ListObject;
Var
  Key:Word;
Begin
  Result := Nil;

  Key := HashKey Mod _TableSize;

  If Assigned(_Table[Key]) Then
    Result := _Table[Key].Search(Visitor, UserData);
End;

Function HashTable.GetItemByIndex(Index: Integer): ListObject;
Var
  I, K, Count:Integer;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  K := 0;
  For I:=0 To Pred(_TableSize) Do
    If (Assigned(_Table[I])) Then
    Begin
      Count := _Table[I].Count;
      If (Index>=K) And (Index< K + Count) Then
      Begin
        Result := _Table[I].GetItemByIndex(Index - K);
        Exit;
      End;

      Inc(K, Count);
    End;

  Result := Nil;
End;

Function HashTable.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): ListObject;
Var
  I:Integer;
Begin
  Result := Nil;

  For I:=0 To Pred(_TableSize) Do
    If Assigned(_Table[I]) Then
    Begin
      Result := _Table[I].Search(Visitor, UserData);
      If Assigned(Result) Then
        Exit;
    End;
End;

Procedure HashTable.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TableSize) Do
  If Assigned(_Table[I]) Then
    _Table[I].Visit(Visitor, UserData);
End;

Function HashTable.Filter(Visitor: CollectionVisitor; UserData: Pointer): HashTable;
Var
  It:Iterator;
  A,B:ListObject;
Begin
  Result := HashTable.Create(Self._TableSize);
  It := Self.CreateIterator();
  While It.HasNext Do
  Begin
    A := It.GetNext();
    If (Visitor(A, UserData)) Then
    Begin
      B := ListObject(A.ClassType.Create());
      B.CopyValue(A);
      Result.Add(B);
    End;
  End;
  It.Destroy;
End;

Function HashTable.Add(Item:ListObject):Boolean;
Var
  Key:HashKey;
Begin
  If Item = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Item.GetHashKey();
  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
  Begin
    {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Allocating a new table...');{$ENDIF}
    _Table[Key] := List.Create(coAppend{ Or coNoDuplicates  Or coCheckReferencesOnAdd});
  End;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Adding item to table...');{$ENDIF}
  Result := _Table[Key].Add(Item);
  Inc(_ItemCount);

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Insertion was ok!');{$ENDIF}
End;

Function HashTable.Delete(Item:ListObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If (Item = Nil) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Item.GetHashKey();
  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Removing item from table...');{$ENDIF}
  Result := (_Table[Key].Delete(Item));
  If Result Then
    Dec(_ItemCount);
End;

Function HashTable.ContainsReference(Item:ListObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If (Item = Nil) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Item.GetHashKey();
  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Searching item in table...');{$ENDIF}
  Result := (_Table[Key].ContainsReference(Item));
End;

Function HashTable.ContainsDuplicate(Item:ListObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If (Item = Nil) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Item.GetHashKey();
  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashTable', 'Searching item in table...');{$ENDIF}
  Result := (_Table[Key].ContainsDuplicate(Item));
End;

Procedure HashTable.Clear();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TableSize) Do
    If Assigned(_Table[I]) Then
    Begin
      Dec(_ItemCount, _Table[I].Count);
      _Table[I].Destroy();
      _Table[I] := Nil;
    End;

  If (_ItemCount <> 0) Then
    RaiseError('Implementation error!');
End;

Function HashTable.CreateIterator:Iterator;
Var
  MyIterator:HashTableIterator;
Begin
  MyIterator := HashTableIterator.Create(Self);
  Result := MyIterator;
End;

Function HashTable.CreateIteratorWithHash(HashKey: Word): Iterator;
Var
  Key:Word;
Begin
  Result := Nil;

  Key := HashKey Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  Result := _Table[Key].CreateIterator();
End;

Constructor HashTableIterator.Create(Table: HashTable);
Begin
  _HashTable := Table;
  _CurrentTable := -1;
  _ItemIndex := 0;
  _Item := Nil;
End;

Function HashTableIterator.HasNext:Boolean;
Begin
  Result := (_ItemIndex<_HashTable._ItemCount);
End;

Function HashTableIterator.FindNextItem():ListObject;
Var
  I:Integer;
Begin
  For I:=Succ(_CurrentTable) To Pred(_HashTable._TableSize) Do
  If Assigned(_HashTable._Table[I]) Then
  Begin
    Result := _HashTable._Table[I]._First;
    _CurrentTable := I;
    Exit;
  End;

  Result := Nil;
End;

Function HashTableIterator.GetNext:ListObject;
Begin
  If (_Item=Nil) Then
  Begin
    _Item := FindNextItem();
  End Else
  Begin
    _Item := _Item.Next;
    If (_Item = Nil) Then
      _Item := FindNextItem();
  End;

  Result := _Item;
  If (Result <> Nil) Then
    Inc(_ItemIndex)
  Else
  Begin
    IntToSTring(_ItemIndex + _HashTable._ItemCount);
  End;
End;

//###############
//#   Queue     #
//###############

Function Queue.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): ListObject;
Var
  P:ListObject;
Begin
  Result := Nil;
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Visitor(P, UserData)) Then
    Begin
      Result := P;
      Exit;
    End;

    P := P.Next;
  End;
End;

Procedure Queue.Visit(Visitor: CollectionVisitor; UserData:Pointer = Nil);
Var
  P:ListObject;
Begin
  P := _First;
  While (Assigned(P)) Do
  Begin
    Visitor(P, UserData);
    P := P.Next;
  End;
End;

Function Queue.Push(Item: ListObject): Boolean;
Var
  P:ListObject;
Begin
  Result := False;

  If (Item = Nil) Or ((Options And coCheckReferencesOnAdd<>0) And (Self.ContainsReference(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Reference already inside collection: '+Item.ToString());
    Exit;
  End;

  If ((Options And coNoDuplicates<>0) And (Self.ContainsDuplicate(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Item duplicated in collection: '+Item.ToString());
    Exit;
  End;

  If (Item.Collection<>Nil) Then
  Begin
    Log(logWarning, Self.ClassName, 'Item already belongs to a collection: '+Item.ToString());
    Exit;
  End;

  Item._Collection := Self;
  Inc(_ItemCount);
  Result := True;

  If (Not Assigned(_First)) Then
  Begin
    _First := Item;
    _First._Next := Nil;
    Exit;
  End;

  P := _First;
  Repeat
    If (Not Assigned(P.Next)) Then
    Begin

      P._Next := Item;
      P := P.Next;
      P._Next := Nil;
      Exit;
    End;

    P := P.Next;
  Until (Not Assigned(P));
End;

Procedure Queue.Clear();
Var
  Temp:ListObject;
Begin
  While (Assigned(_First)) Do
  Begin
    Temp := Self.Pop();

    If (Options And coUnmanaged=0) And (Assigned(Temp)) Then
      Temp.Destroy();
  End;
End;

Function Queue.ContainsReference(Item: ListObject): Boolean;
Var
  P:ListObject;
Begin
  Result := True;
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Item = P) Then
      Exit;

    P := P.Next;
  End;
  Result := False;
End;

Function Queue.ContainsDuplicate(Item: ListObject): Boolean;
Var
  P:ListObject;
Begin
  Result := True;
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Item.Sort(P) = 0) Then
      Exit;

    P := P.Next;
  End;
  Result := False;
End;

Constructor Queue.Create;
Begin
  _Options := coAppend;
  _First := Nil;
End;

Function Queue.CreateIterator: Iterator;
Begin
  Result := Nil;
End;

Function Queue.GetItemByIndex(Index:Integer):ListObject;
Var
  I:Integer;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := _First;
  If (Index = 0) Then
    Exit;

  I := 0;
  While (Result<>Nil) Do
  Begin
    Result := Result.Next;
    Inc(I);

    If (I = Index) Then
      Exit;
  End;

  Result := Nil;
End;

Function Queue.Pop:ListObject;
Var
  P:ListObject;
Begin
  If (_First = Nil) Then
  Begin
    Result := Nil;
    Exit;
  End;

  P := _First;
  _First := _First.Next;
  Dec(_ItemCount);

  Result := P;
End;

{ Stack }
Function Stack.Pop:ListObject;
Begin
  Result := _First;
  If (_First <> Nil) Then
    _First := _First.Next;
End;

Function Stack.Push(Item: ListObject): Boolean;
Begin
  Item._Next := _First;
  _First := Item;

  Result := True;
End;

Procedure IntegerArray.Clear;
Begin
  Count := 0;
  SetLength(Items, 0);
End;

Function IntegerArray.Contains(N:Integer):Boolean;
Var
  I:Integer;
Begin
  Result := True;
  For I:=0 To Pred(Count) Do
  If (Items[I]=N) Then
    Exit;

  Result := False;
End;

Procedure IntegerArray.Replace(A,B:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Count) Do
  If (Items[I]=A) Then
    Items[I] := B;
End;

Procedure IntegerArray.Add(N:Integer; AllowDuplicates:Boolean = False);
Begin
  If (Not AllowDuplicates) And (Contains(N)) Then
    Exit;

  Inc(Count);
  SetLength(Items, Count);
  Items[Pred(Count)] := N;
End;

Procedure IntegerArray.Remove(N:Integer);
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

Procedure IntegerArray.Merge(A:IntegerArray);
Var
  I:Integer;
Begin
  For I:=0 To Pred(A.Count) Do
    Self.Add(A.Items[I]);
End;

Function IntegerArray.Pop:Integer;
Begin
  Result := Items[0];
  Items[0] := Items[Pred(Count)];
  Dec(Count);
  SetLength(Items, Count);
End;

{ Iterator }
Destructor Iterator.Destroy;
Begin

End;

Function Iterator.Discard: Boolean;
Begin
  Result := False;
End;

{ ListObject }
Procedure ListObject.CopyValue(Other: ListObject);
Begin
  // do nothing
End;

Destructor ListObject.Destroy;
Begin
  // do nothing
End;

Function ListObject.GetHashKey: HashKey;
Begin
  Result := 0;
End;

Procedure ListObject.Internal_Copy(Other: ListObject);
Begin
  If (IsCompatible(Other)) Then
    Self.CopyValue(Other);
End;

Function ListObject.Internal_Sort(Other: ListObject): Integer;
Begin
  If (IsCompatible(Other)) Then
    Result := Self.Sort(Other)
  Else
    Result := 0;
End;

Function ListObject.IsCompatible(Other: ListObject): Boolean;
Begin
  Result := (Self Is Other.ClassType);
  If Not Result Then
    RaiseError('Cannot copy list objects, '+Self.ClassName +' and '+ Other.ClassName+' are not compatible!');
End;

Function ListObject.Sort(Other: ListObject): Integer;
Begin
  If (Other<>Nil) Then
    Result := 0
  Else
    Result := 1;
End;

Function ListObject.ToString:TERRAString;
Begin
  Result := Self.ClassName+'@'+HexStr(Cardinal(Self));
End;

{ StringObject }
Procedure StringObject.CopyValue(Other: ListObject);
Begin
  Self.Value := StringObject(Other).Value;
End;

Constructor StringObject.Create(S:TERRAString);
Begin
  Self.Value := S;
End;

Function StringObject.GetHashKey: HashKey;
Begin
  Result := GetStringHashKey(Value);
End;

Function StringObject.Sort(Other: ListObject): Integer;
Var
  S:TERRAString;
Begin
  S := StringObject(Other).Value;
  Result := GetStringSort(Self.Value, S);
End;

Function StringObject.ToString:TERRAString;
Begin
  Result := Value;
End;


{ KeyPairObject }
Constructor KeyPairObject.Create(Key, Value:TERRAString);
Begin
  Self.Key := Key;
  Self.Value := Value;
End;

Procedure KeyPairObject.CopyValue(Other: ListObject);
Begin
  Self.Key := KeyPairObject(Other).Key;
  Self.Value := KeyPairObject(Other).Value;
End;

Function KeyPairObject.GetHashKey: HashKey;
Begin
  Result := GetStringHashKey(Key);
End;

Function KeyPairObject.Sort(Other: ListObject): Integer;
Var
  S:TERRAString;
Begin
  S := KeyPairObject(Other).Key;
  Result := GetStringSort(Self.Key, S);
End;

Function KeyPairObject.ToString:TERRAString;
Begin
  Result := Key;
End;

Function LoadKeypairList(SourceFile:TERRAString):List;
Var
  Source:Stream;
  S,S2:TERRAString;
Begin
  S := '';
  Result := List.Create;
  If  (SourceFile<>'') And (FileStream.Exists(SourceFile)) Then
  Begin
    Source :=  FileStream.Open(SourceFile);
    While Not Source.EOF Do
    Begin
      Source.ReadLine(S);
      S2 := StringGetNextSplit(S, Ord(','));
      Result.Add(KeyPairObject.Create(S2,S));
    End;
    Source.Destroy;
  End;
End;

{ IntegerObject }
Procedure IntegerObject.CopyValue(Other: ListObject);
Begin
  Self.Value := IntegerObject(Other).Value;
End;

Function IntegerObject.Sort(Other: ListObject): Integer;
Var
  S:Integer;
Begin
  S := IntegerObject(Other).Value;
  If (S<Value) Then
    Result := 1
  Else
  If (S>Value) Then
    Result := -1
  Else
    Result := 0;
End;

Function IntegerObject.GetHashKey: HashKey;
Begin
  Result := Value;
End;

Constructor IntegerObject.Create(S:Integer);
Begin
  Self.Value := S;
End;


Function IntegerObject.ToString:TERRAString;
Begin
  Result := IntToString(Value);
End;

{ Pool }
Procedure Pool.Clear;
Var
  I:Integer;
  List,Next:ListObject;
Begin
  List := _First;
  While Assigned(List)Do
  Begin
    Next := List.Next;

    If (Not (List Is ListObject)) Then
      List.Destroy;

    List := Next;
  End;

  _First := Nil;
  _ItemCount := 0;

  For I:=0 To Pred(_ObjectCount) Do
    _Objects[I].Used := False;
End;

Destructor Pool.Destroy;
Var
  I:Integer;
Begin
  Inherited;

  For I:=0 To Pred(_ObjectCount) Do
    FreeAndNil(_Objects[I].Obj);
End;

Procedure Pool.InsertPoolObject(Obj: ListObject);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
  If (_Objects[I].Obj = Obj) Then
  Begin
    _Objects[I].Used := True;
    Exit;
  End;


  For I:=0 To Pred(_ObjectCount) Do
  If (Not _Objects[I].Used) And (_Objects[I].Obj = Nil) Then
  Begin
    _Objects[I].Obj := Obj;
    Exit;
  End;

  If (_ObjectCount<=0) Then
    _ObjectCount := 1024
  Else
    _ObjectCount := _ObjectCount * 2;

  SetLength(_Objects, _ObjectCount);
  InsertPoolObject(Obj);
End;

Function Pool.Add(Item: ListObject; SkipSorting:Boolean = False): Boolean;
Begin
  Self._Options := Self._Options Or coCheckReferencesOnAdd;

  Result := Inherited Add(Item, SkipSorting);

  If Result Then
    InsertPoolObject(Item);
End;


Function Pool.Recycle:ListObject;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
  If (Not _Objects[I].Used) And (Assigned(_Objects[I].Obj)) Then
  Begin
    _Objects[I].Used := True;
    Result := _Objects[I].Obj;
    Result._Collection := Nil;
    Exit;
  End;

  Result := Nil;
End;

End.





