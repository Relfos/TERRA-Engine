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
Uses TERRA_String, TERRA_Utils, TERRA_Queue;

Type
  Stack = Class(Queue)
    Public
      Function Push(Item:ListObject):Boolean; Override;
      Function Pop():ListObject; Override;
  End;

Implementation

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

End.