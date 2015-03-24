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
 * TERRA_ObjectArray
 * Implements a generic thread safe array of objects
 ***********************************************************************************************************************
}

Unit TERRA_ObjectArray;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Collections;

Type
  ObjectArray = Class(Collection)
    Protected
      _Objects:Array Of CollectionObject;

      Procedure RemoveDiscardedItems(); Override;

    Public
      Constructor Create(Options:Cardinal; ShareItemsWith:ObjectArray);

      Function GetIterator:Iterator; Override;

      Procedure Clear(); Override;

      Function Add(Item:CollectionObject):Boolean;
      Function Delete(Item:CollectionObject):Boolean;

      Function GetItemByIndex(Index:Integer):CollectionObject; Override;
  End;


Implementation
Uses TERRA_Log;

Type
  ObjectArrayIterator = Class(Iterator)
    Protected
      Function ObtainNext:CollectionObject; Override;

    Public
      Procedure Reset(); Override;
  End;

{ ObjectArray }
Constructor ObjectArray.Create(Options:Cardinal; ShareItemsWith:ObjectArray);
Begin
  _SortOrder := collection_Unsorted;
  _ItemCount := 0;

  Self.Init(Options, ShareItemsWith);
End;

Function ObjectArray.Add(Item: CollectionObject): Boolean;
Begin
  If Item = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  Self.Lock();

  Item.Link(Self);

  Inc(_ItemCount);
  SetLength(_Objects, _ItemCount);
  _Objects[Pred(_ItemCount)] := Item;
  Self.Unlock();

  Result := True;
End;

Function ObjectArray.Delete(Item: CollectionObject): Boolean;
Var
  I:Integer;
Begin
  Result := False;
  If Item = Nil Then
    Exit;

  Self.Lock();
{  For I:=0 To Pred(_ItemCount) Do
  If (_Objects[I] = Item) Then
  Begin
    _Objects[I].Discard();
    Result := True;
    Break;
  End;}

  I := 0;
  While I<_ItemCount Do
  If (_Objects[I] = Item) Then
  Begin
    _Objects[I] := _Objects[Pred(_ItemCount)];
    Dec(_ItemCount);
  End Else
    Inc(I);

  Self.Unlock();
End;

Procedure ObjectArray.RemoveDiscardedItems;
Var
  I:Integer;
Begin
  I := 0;
  While I<_ItemCount Do
  If (_Objects[I].Discarded) Then
  Begin
    _Objects[I] := _Objects[Pred(_ItemCount)];
    Dec(_ItemCount);
  End Else
    Inc(I);
End;

Procedure ObjectArray.Clear;
Begin
  Self.Lock();
  _ItemCount := 0;
  Self.Unlock();
End;

Function ObjectArray.GetItemByIndex(Index: Integer): CollectionObject;
Begin
  If (Index<0) Or (Index>=_ItemCount) Then
    Result := Nil
  Else
    Result := _Objects[Index];
End;

Function ObjectArray.GetIterator: Iterator;
Begin
  Result := ObjectArrayIterator.Create(Self);
End;

{ ObjectArrayIterator }
Function ObjectArrayIterator.ObtainNext: CollectionObject;
Begin
  Result := ObjectArray(_Collection)._Objects[_Index];
End;

Procedure ObjectArrayIterator.Reset;
Begin
  Inherited Reset();
  
  If (ObjectArray(_Collection)._ItemCount>0) Then
    _Next := ObjectArray(_Collection)._Objects[0]
  Else
    _Next := Nil;
End;

End.