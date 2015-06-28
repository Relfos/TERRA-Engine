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
 * TERRA_Queue
 * Implements a generic thread safe stack
 ***********************************************************************************************************************
}

Unit TERRA_Pool;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections;

Type
  PoolItem = Record
    Used:Boolean;
    Obj:CollectionObject;
  End;

  Pool = Class(List)
    Protected
      _Objects:Array Of PoolItem;
      _ObjectCount:Integer;

      Procedure InsertPoolObject(Obj:CollectionObject);

    Public
      Procedure Release(); Override;

      Procedure Clear(); Override;

      Function Recycle():CollectionObject;

      // Returns true if insertion was sucessful
      Function Add(Item:CollectionObject):Boolean; Override;
  End;

Implementation

{ Pool }
Procedure Pool.Clear;
Var
  I:Integer;
  List,Next:CollectionObject;
Begin
  List := _First;
  While Assigned(List)Do
  Begin
    Next := List.Next;

    If (Not (List Is CollectionObject)) Then
      ReleaseObject(List);

    List := Next;
  End;

  _First := Nil;
  _ItemCount := 0;

  For I:=0 To Pred(_ObjectCount) Do
    _Objects[I].Used := False;
End;

Procedure Pool.Release();
Var
  I:Integer;
Begin
  Inherited;

  For I:=0 To Pred(_ObjectCount) Do
    ReleaseObject(_Objects[I].Obj);
End;

Procedure Pool.InsertPoolObject(Obj: CollectionObject);
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

Function Pool.Add(Item: CollectionObject): Boolean;
Begin
  Self._Options := Self._Options Or coCheckReferencesOnAdd;

  Self.Lock();
  If Assigned(_First) Then
  Begin
    _Last.Next := Item;
    Item.Next := Nil;
    _Last := Item;
  End Else
  Begin
    _First := Item;
    _Last := _First;
    Item.Next := Nil;
  End;

  InsertPoolObject(Item);
  Self.Unlock();

  Result := True;
End;


Function Pool.Recycle:CollectionObject;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
  If (Not _Objects[I].Used) And (Assigned(_Objects[I].Obj)) Then
  Begin
    _Objects[I].Used := True;
    Result := _Objects[I].Obj;
    Result.Link(Nil);
    Exit;
  End;

  Result := Nil;
End;

End.