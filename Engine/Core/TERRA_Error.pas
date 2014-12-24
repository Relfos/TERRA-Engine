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
Unit TERRA_Error;

{$I terra.inc}

Interface

Var
  _FatalError:Boolean;

Procedure RaiseError(Const Desc:AnsiString);

Implementation
Uses SysUtils, TERRA_Log;


Function DumpCallstack:AnsiString;
{$IFDEF FPC}
Var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
Const
  MaxDepth = 20;
Begin
  Result := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Result := Result + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;

End;
{$ELSE}

{$IFDEF CALLSTACKINFO}
Begin
  Result := GetCallStack(1);
End;
{$ELSE}
Begin
  Result := 'No call stack info present.';
End;
{$ENDIF}
{$ENDIF}

Type
  TERRAException = Class(Exception)
  End;

Procedure RaiseError(Const Desc:AnsiString);
Var
  S:AnsiString;
  {$IFDEF CALLSTACKINFO}
  I:Integer;
  CallStack:AnsiString;
  {$ENDIF}
Begin
  If _FatalError Then
    Exit;

  _FatalError := True;

  ForceLogFlush := True;

  Log(logError, 'Application', Desc);
  Log(logDebug, 'Callstack', DumpCallstack());
  {$IFDEF CALLSTACKINFO}
  Log(logDebug,'System','Callstack'+crLf+CallStack);
  {$ENDIF}

  Raise TERRAException.Create(Desc);
End;


End.