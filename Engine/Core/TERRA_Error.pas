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