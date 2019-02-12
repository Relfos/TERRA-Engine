{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
Uses TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Queue;

Type
  Stack = Class(Queue)
    Public
      Function Push(Item:CollectionObject):Boolean; Override;
      Function Pop():CollectionObject; Override;
  End;

Implementation

{ Stack }
Function Stack.Pop:CollectionObject;
Begin
  Result := _First;
  If (_First <> Nil) Then
    _First := _First.Next;
End;

Function Stack.Push(Item: CollectionObject): Boolean;
Begin
  Item.Next := _First;
  _First := Item;

  Result := True;
End;

End.