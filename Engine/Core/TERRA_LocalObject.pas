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
 * TERRA_LocalObject
 * Implements pseudo-stack objects
 ***********************************************************************************************************************
}

Unit TERRA_LocalObject;

Interface

Type
  LocalObject = Class(TERRAObject)
    Private
      _LocalStackOffset:Integer;

      Class Function NewInstance:TObject; Override;
      Procedure FreeInstance; Override;
    Public
  End;

Implementation

Var
  _GlobalStack:Array Of Byte = Nil;
  _GlobalStackPosition:Cardinal = 0;

{ LocalObject }
Class Function LocalObject.NewInstance: TObject;
Var
  ObjSize, GlobalSize:Integer;
Begin
  ObjSize := InstanceSize;

  If (ObjSize<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

  GlobalSize := Length(_GlobalStack);
  If (GlobalSize<=0) Then
    SetLength(_GlobalStack, 1024)
  Else
    SetLength(_GlobalStack, GlobalSize * 2);

  _LocalStackOffset := _GlobalStackPosition;
  Inc(_GlobalStackPosition, ObjSize);

  Result := @_GlobalStack[_LocalStackOffset];

  InitInstance(); 
End;

Procedure LocalObject.FreeInstance;
Var
  ObjSize:Integer;
Begin
  ObjSize := InstanceSize;

  If (_GlobalStackPosition - ObjSize = _LocalStackOffset) Then
  Begin
    Dec(_GlobalStackPosition, ObjSize);
    Exit;
  End;

  s
  fsfds??
End;


End.
