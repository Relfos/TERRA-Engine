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

Unit TERRA_Stack;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Queue;

Type
  TERRAStack = Class(TERRAQueue)
    Public
      Function Push(Item:TERRAObject):Boolean; Override;
      Function Pop():TERRAObject; Override;
  End;

Implementation

{ Stack }
Function TERRAStack.Pop:TERRAObject;
Var
  Next:TERRACollectionObject;
Begin
  If _First = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Next := _First.Next;
  Result := _First.Item;

  ReleaseObject(_First);
  _First := Next;
End;

Function TERRAStack.Push(Item:TERRAObject): Boolean;
Var
  Obj:TERRACollectionObject;
Begin
  Obj := TERRACollectionObject.Create(Self, Item);

  Obj.Next := _First;
  _First := Obj;

  Result := True;
End;

End.