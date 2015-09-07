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
 * TERRA_Hashmap
 * Implements a generic thread safe hash map
 ***********************************************************************************************************************
}

Unit TERRA_Hashmap;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_List;

Type
  TERRAHashMap = Class(TERRACollection)
    Protected
      _Table:Array Of TERRAList;
      _TableSize:Word;

    Public
      Constructor Create(TableSize:Word = 1024);

      // Returns true if insertion was sucessful
      Function Add(Item:TERRAObject):Boolean;Virtual;

      // Returns true if deletion was sucessful
      Function Remove(Item:TERRAObject):Boolean; Override;

      Function Contains(Item:TERRAObject):Boolean; Override;

      Function GetItemByIndex(Index:Integer):TERRAObject; Override;

      Function GetItemByKey(Const Key:TERRAString):TERRAObject;

      Procedure Clear(); Override;

      Function GetIterator:Iterator; Override;

      Property Items[Const Key:TERRAString]:TERRAObject Read GetItemByKey; Default;
  End;

  HashMapIterator = Class(Iterator)
    Protected
      _CurrentTable:Integer;
      _Current:TERRACollectionObject;

      Function FindNextItem():TERRACollectionObject;
      Function ObtainNext:TERRACollectionObject; Override;

    Public
      Procedure Reset(); Override;
  End;

Function LoadKeypairList(SourceFile:TERRAString):TERRAHashMap;

Implementation
Uses TERRA_Engine, TERRA_Log, TERRA_Stream, TERRA_FileStream, TERRA_MurmurHash;

Function LoadKeypairList(SourceFile:TERRAString):TERRAHashMap;
Var
  Source:TERRAStream;
  S,S2:TERRAString;
Begin
  S := '';
  Result := TERRAHashMap.Create();
  If  (SourceFile<>'') And (FileStream.Exists(SourceFile)) Then
  Begin
    Source :=  FileStream.Open(SourceFile);
    While Not Source.EOF Do
    Begin
      Source.ReadLine(S);
      S2 := StringGetNextSplit(S, ',');
      Result.Add(StringProperty.Create(S2,S));
    End;
    ReleaseObject(Source);
  End;
End;

{ HashMap }
Constructor TERRAHashMap.Create(TableSize:Word);
Begin
  _SortOrder := collection_Unsorted;
  _ItemCount := 0;
  _TableSize := TableSize;
  SetLength(_Table, _TableSize);

  Self.Init(0, Nil);
End;

Function TERRAHashMap.GetItemByIndex(Index: Integer): TERRAObject;
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

(*Function HashMap.Search(Visitor:CollectionVisitor; Target:TERRAObject):TERRACollectionObject;
Var
  I:Integer;
Begin
  Result := Nil;

  For I:=0 To Pred(_TableSize) Do
    If Assigned(_Table[I]) Then
    Begin
      Result := _Table[I].Search(Visitor, Target);
      If Assigned(Result) Then
        Exit;
    End;
End;

Procedure HashMap.Visit(Visitor:CollectionVisitor; Target:TERRAObject);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TableSize) Do
  If Assigned(_Table[I]) Then
    _Table[I].Visit(Visitor, Target);
End;

Function HashMap.Filter(Visitor:CollectionVisitor; Target:TERRAObject):List;
Var
  It:Iterator;
  A,B:TERRAObject;
Begin
  Result := List.Create(collection_Unsorted);
  It := Self.GetIterator();
  While It.HasNext Do
  Begin
    A := It.Value;
    If (Visitor(A, Target)) Then
    Begin
      B := TERRAObject(A.ClassType.Create());
      B.CopyProperties(A);
      Result.Add(B);
    End;
  End;
End;*)

Function TERRAHashMap.Add(Item:TERRAObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If Item = Nil Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Murmur2(Item.Name);
  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
  Begin
    {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Allocating a new table...');{$ENDIF}
    _Table[Key] := TERRAList.Create(collection_Unsorted);
  End;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Adding item to table...');{$ENDIF}
  Result := _Table[Key].Add(Item);
  Inc(_ItemCount);

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Insertion was ok!');{$ENDIF}
End;

Function TERRAHashMap.Remove(Item:TERRAObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If (Item = Nil) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Murmur2(Item.Name);
  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Removing item from table...');{$ENDIF}
  Result := (_Table[Key].Remove(Item));
  If Result Then
    Dec(_ItemCount);
End;

Function TERRAHashMap.Contains(Item:TERRAObject):Boolean;
Var
  Key:HashKey;
Begin
  Result := False;

  If (Item = Nil) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Obtaining an hash for this item...');{$ENDIF}
  Key := Murmur2(Item.Name);
  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Got hash index: '+HexStr(Key));{$ENDIF}

  Key := Key Mod _TableSize;

  If Not Assigned(_Table[Key]) Then
    Exit;

  {$IFDEF DEBUG}Log(logDebug, 'HashMap', 'Searching item in table...');{$ENDIF}
  Result := (_Table[Key].Contains(Item));
End;

Procedure TERRAHashMap.Clear();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TableSize) Do
    If Assigned(_Table[I]) Then
    Begin
      Dec(_ItemCount, _Table[I].Count);
      ReleaseObject(_Table[I]);
    End;
End;

Function TERRAHashMap.GetItemByKey(Const Key: TERRAString):TERRAObject;
Var
  K:HashKey;
  Index:Integer;
  P:TERRACollectionObject;
Begin
  K := Murmur2(Key);
  Index := K Mod _TableSize;

  Result := Nil;

  Self.Lock();

  If Assigned(_Table[Index]) Then
  Begin
    P := _Table[Index].First;
    While Assigned(P) Do
    If (StringEquals(Key, P.Item.Name)) Then
    Begin
      Result := P.Item;
      Break;
    End Else
      P := P.Next;
  End;

  Self.Unlock();
End;

Function TERRAHashMap.GetIterator:Iterator;
Var
  MyIterator:HashMapIterator;
Begin
  MyIterator := HashMapIterator(Engine.Pool.Fetch(HashMapIterator));
  If Assigned(MyIterator) Then
  Begin
    MyIterator.Init(Self);
    MyIterator.Reset();
  End Else
    MyIterator := HashMapIterator.Create(Self);

  Result := MyIterator;
End;

{ HashMapIterator }
Procedure HashMapIterator.Reset();
Begin
  Inherited;
  _CurrentTable := -1;
  _Current := Nil;
End;

Function HashMapIterator.FindNextItem():TERRACollectionObject;
Var
  I:Integer;
  Table:TERRAHashMap;
Begin
  Table := TERRAHashMap(Self.Collection);
  For I := Succ(_CurrentTable) To Pred(Table._TableSize) Do
  If Assigned(Table._Table[I]) Then
  Begin
    Result := Table._Table[I].First;
    _CurrentTable := I;
    Exit;
  End;

  Result := Nil;
End;

Function HashMapIterator.ObtainNext:TERRACollectionObject;
Begin
  If (_Current = Nil) And (Self.Index<Self.Collection.Count) Then
  Begin
    _Current := FindNextItem();
  End;

  If Assigned(_Current) Then
  Begin
    Result := _Current;
    _Current := _Current.Next;
  End Else
    Result := Nil;
End;

End.
