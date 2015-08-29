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
Uses TERRA_String, TERRA_Utils, TERRA_Object, TERRA_List, TERRA_Collections;

Type
  TERRAQueue = Class(TERRAList)
    Public
      Constructor Create();

      Function Push(Item:TERRAObject):Boolean; Virtual;
      Function Pop():TERRAObject; Virtual;

      Function CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject; Override;
  End;


Implementation
Uses TERRA_Log;

{ TERRAQueue }
Constructor TERRAQueue.Create();
Begin
  _SortOrder := collection_Unsorted;
  _Options := 0;
  _First := Nil;
End;

Function TERRAQueue.Push(Item: TERRAObject): Boolean;
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

Function TERRAQueue.Pop:TERRAObject;
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

Function TERRAQueue.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  Result := Inherited CreateProperty(KeyName, ObjectType);

  If (Assigned(Result)) Then
  Begin
    Self.Push(Result);
  End;
End;

End.
