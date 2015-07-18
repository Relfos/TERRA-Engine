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
  Queue = Class(Collection)
    Protected
      _First:CollectionObject;

    Public
      Constructor Create();

      Function GetIterator:Iterator; Override;

      Procedure Clear(); Override;

      Function Push(Item:CollectionObject):Boolean; Virtual;
      Function Pop():CollectionObject; Virtual;

      Function GetItemByIndex(Index:Integer):CollectionObject; Override;

      Function Search(Visitor:CollectionVisitor; UserData:Pointer = Nil):CollectionObject; Override;
      Procedure Visit(Visitor:CollectionVisitor; UserData:Pointer = Nil); Override;

      Function Contains(Item:CollectionObject):Boolean; Override;

      Property First:CollectionObject Read _First;
  End;


Implementation
Uses TERRA_Log;

{ Queue }
Function Queue.Search(Visitor: CollectionVisitor; UserData:Pointer = Nil): CollectionObject;
Var
  P:CollectionObject;
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
  P:CollectionObject;
Begin
  P := _First;
  While (Assigned(P)) Do
  Begin
    Visitor(P, UserData);
    P := P.Next;
  End;
End;

Function Queue.Push(Item: CollectionObject): Boolean;
Var
  P:CollectionObject;
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

  Item.Link(Self);
  Inc(_ItemCount);
  Result := True;

  If (Not Assigned(_First)) Then
  Begin
    _First := Item;
    _First.Next := Nil;
    Exit;
  End;

  P := _First;
  Repeat
    If (Not Assigned(P.Next)) Then
    Begin

      P.Next := Item;
      P := P.Next;
      P.Next := Nil;
      Exit;
    End;

    P := P.Next;
  Until (Not Assigned(P));
End;

Procedure Queue.Clear();
Var
  Temp:CollectionObject;
Begin
  While (Assigned(_First)) Do
  Begin
    Temp := Self.Pop();

    ReleaseObject(Temp);
  End;
End;

Function Queue.Contains(Item: CollectionObject): Boolean;
Var
  P:CollectionObject;
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

Constructor Queue.Create;
Begin
  _SortOrder := collection_Unsorted;
  _Options := 0;
  _First := Nil;
End;

Function Queue.GetIterator: Iterator;
Begin
  Result := Nil;
End;

Function Queue.GetItemByIndex(Index:Integer):CollectionObject;
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

Function Queue.Pop:CollectionObject;
Var
  P:CollectionObject;
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

End.