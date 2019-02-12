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
 * TERRA_Error
 * Implements a generic engine error exception
 ***********************************************************************************************************************
}
Unit TERRA_Error;

{$I terra.inc}

Interface
Uses SysUtils, TERRA_String;

Var
  _FatalError:TERRAString = '';

Procedure RaiseError(Const Desc:TERRAString);

Implementation
Uses TERRA_Log;


Type
  TERRAException = Class(Exception)
  End;

Procedure RaiseError(Const Desc:TERRAString);
Var
  S:TERRAString;
  {$IFDEF CALLSTACKINFO}
  I:Integer;
  CallStack:TERRAString;
  {$ENDIF}
Begin
  If _FatalError<>'' Then
    Exit;

  _FatalError := Desc;

  ForceLogFlush := True;

  Log(logError, 'Application', Desc);

  Raise TERRAException.Create(Desc);
End;

//    DiscardWhiteSpace = WhiteSpace -> { };
  //  Comment = '{' A '}' -> { A.Discard(); };

End.