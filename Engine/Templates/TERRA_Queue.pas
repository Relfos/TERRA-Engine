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
 * Implements a generic thread safe queue 
 ***********************************************************************************************************************
}

Unit TERRA_Queue;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Collections;

Type
  Queue = Class(TERRACollection)
    Protected
      _First:TERRACollectionObject;

    Public
      Constructor Create();

      Procedure Clear(); Override;

      Function Push(Item:TERRAObject):Boolean; Virtual;
      Function Pop():TERRAObject; Virtual;

      Function GetItemByIndex(Index:Integer):TERRAObject; Override;

      Function Contains(Item:TERRAObject):Boolean; Override;

      Property First:TERRACollectionObject Read _First;
  End;


Implementation
Uses TERRA_Log;

{ Queue }
Constructor Queue.Create();
Begin
  _SortOrder := collection_Unsorted;
  _Options := 0;
  _First := Nil;
End;

Procedure Queue.Clear();
Var
  Temp, Next:TERRACollectionObject;
Begin
  Temp := _First;

  While (Assigned(Temp)) Do
  Begin
    Next := Temp.Next;
    ReleaseObject(Temp);

    Temp := Next;
  End;

  _First := Nil;
End;

Function Queue.Contains(Item:TERRAObject): Boolean;
Var
  P:TERRACollectionObject;
Begin
  Result := True;
  P := _First;
  While (Assigned(P)) Do
  Begin
    If (Item = P.Item) Then
      Exit;

    P := P.Next;
  End;
  Result := False;
End;

Function Queue.GetItemByIndex(Index:Integer):TERRAObject;
Var
  I:Integer;
  P:TERRACollectionObject;
Begin
  If (Index<0) Or (Index>=Self.Count) Then
  Begin
    Result := Nil;
    Exit;
  End;

  P := _First;
  If (Index > 0) Then
  Begin
    I := 0;
    While (Assigned(P)) Do
    Begin
      P := P.Next;
      Inc(I);

      If (I = Index) Then
        Break;
    End;
  End;

  If Assigned(P) Then
    Result := P.Item
  Else
    Result := Nil;
End;


Function Queue.Push(Item: TERRAObject): Boolean;
Var
  Obj, P:TERRACollectionObject;
Begin
  Result := False;

  If (Item = Nil) Or ((Options And coCheckReferencesOnAdd<>0) And (Self.Contains(Item))) Then
  Begin
    Log(logWarning, Self.ClassName, 'Reference already inside collection: '+Item.GetBlob());
    Exit;
  End;

  Obj := TERRACollectionObject.Create(Self, Item);
  Inc(_ItemCount);
  Result := True;

  If (Not Assigned(_First)) Then
  Begin
    _First := Obj;
    _First.Next := Nil;
    Exit;
  End;

  P := _First;
  Repeat
    If (Not Assigned(P.Next)) Then
    Begin

      P.Next := Obj;
      P := P.Next;
      P.Next := Nil;
      Exit;
    End;

    P := P.Next;
  Until (Not Assigned(P));
End;

Function Queue.Pop:TERRAObject;
Var
  P:TERRACollectionObject;
Begin
  If (_First = Nil) Then
  Begin
    Result := Nil;
    Exit;
  End;

  P := _First;
  _First := _First.Next;
  Dec(_ItemCount);

  Result := P.Item;
  ReleaseObject(P);
End;

End.