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
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Collections;

Type
  ObjectArray = Class(TERRACollection)
    Protected
      _Objects:Array Of TERRACollectionObject;

      Procedure RemoveDiscardedItems(); Override;

    Public
      Constructor Create(Options:Cardinal; ShareItemsWith:ObjectArray);

      Function GetIterator:TERRAIterator; Override;

      Procedure Clear(); Override;

      Function Add(Item:TERRAObject):Boolean;
      Function Delete(Item:TERRAObject):Boolean;

      Function GetItemByIndex(Index:Integer):TERRAObject; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_Engine;

Type
  ObjectArrayIterator = Class(TERRAIterator)
    Protected
      Function ObtainNext:TERRACollectionObject; Override;
  End;

{ ObjectArray }
Constructor ObjectArray.Create(Options:Cardinal; ShareItemsWith:ObjectArray);
Begin
  _SortOrder := collection_Unsorted;
  _ItemCount := 0;

  Self.Init(Options, ShareItemsWith);
End;

Function ObjectArray.Add(Item:TERRAObject): Boolean;
Var
  Obj:TERRACollectionObject;
Begin
  If Item = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  Obj := Self.NewItem(Item);

  Self.Lock();

  Inc(_ItemCount);
  SetLength(_Objects, _ItemCount);
  _Objects[Pred(_ItemCount)] := Obj;
  Self.Unlock();

  Result := True;
End;

Function ObjectArray.Delete(Item:TERRAObject): Boolean;
Var
  I:Integer;
Begin
  Result := False;
  If Item = Nil Then
    Exit;

  Self.Lock();
  I := 0;
  While I<_ItemCount Do
  If (_Objects[I].Item = Item) Then
  Begin
    ReleaseObject(_Objects[I]);
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

Function ObjectArray.GetItemByIndex(Index: Integer):TERRAObject;
Begin
  If (Index<0) Or (Index>=_ItemCount) Then
    Result := Nil
  Else
    Result := _Objects[Index].Item;
End;

Function ObjectArray.GetIterator: TERRAIterator;
Begin
  Result := ObjectArrayIterator(Engine.Pool.Fetch(ObjectArrayIterator));
  If Assigned(Result) Then
    Result.Create(Self)
  Else
    Result := ObjectArrayIterator.Create(Self);
End;

{ ObjectArrayIterator }
Function ObjectArrayIterator.ObtainNext:TERRACollectionObject;
Begin
  If (Self.Index< Self.Collection.Count) Then
    Result := ObjectArray(Self.Collection)._Objects[Self.Index]
  Else
    Result := Nil;
End;

End.
