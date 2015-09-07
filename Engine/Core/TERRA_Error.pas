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
 * TERRA_Error
 * Implements a generic engine error exception
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;

{$ELSE}
Unit TERRA_Error;
{$I terra.inc}
{$ENDIF}


Interface
Uses SysUtils, TERRA_Object, TERRA_Callstack;

Type
  TERRAError = Class(Exception)
      _Desc:TERRAString;
      _CrashLog:TERRAString;
      _Callstack:TERRACallstack;

    Public
      Constructor Create(Const Desc:TERRAString; E:Exception);
      Destructor Destroy();

      Property Description:TERRAString Read _Desc;

      Property CrashLog:TERRAString Read _CrashLog;
      Property Callstack:TERRACallstack Read _Callstack;
  End;

Implementation

{$IFNDEF OXYGENE}
Uses TERRA_String, TERRA_Log;
{$ENDIF}

Constructor TERRAError.Create(Const Desc:TERRAString; E:Exception);
Var
  S:TERRAString;
  {$IFDEF CALLSTACKINFO}
  I:Integer;
  CallStack:TERRAString;
  {$ENDIF}
Begin
  _Desc := Desc;

    {$IFNDEF OXYGENE}
  ForceLogFlush := True;
  {$ENDIF}

  Log(logError, 'Engine', Desc);

  If E = Nil Then
    E := Self;

  _Callstack := TERRACallstack.Create();
  _Callstack.FillExceptionCallStack(E);
End;

Destructor TERRAError.Destroy;
Begin
  ReleaseObject(_Callstack);
End;

End.
