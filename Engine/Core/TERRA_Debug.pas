Unit TERRA_Debug;
                     
{$I terra.inc}
Interface

//Procedure DebugStack(S:AnsiString);
Procedure DebugOpenAL;

Procedure PushCallstack(ClassType:TClass; S:AnsiString);
Procedure PopCallstack();

Function GetCallstack:AnsiString;

Implementation

Uses {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF FPC}lineinfo, {$ENDIF}
  SysUtils, TERRA_Utils, TERRA_Classes, TERRA_Application, TERRA_OS, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_AL,
{$IFDEF ANDROID}
  android_log,
{$ENDIF}
  TERRA_Shader;

Var
  _Callstack:Stack;

Procedure PushCallstack(ClassType:TClass; S:AnsiString);
Begin
  If (_Callstack = Nil) Then
    _Callstack := Stack.Create();

  If (ClassType <> Nil) Then
    S := ClassType.ClassName + '.' + S;

  If Pos('(',S)>0 Then
    IntToString(2);

  _Callstack.Push(StringObject.Create(S));
End;

Procedure PopCallstack();
Begin
  _Callstack.Pop();
End;


{$IFDEF WINDOWS}
Type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord =
  record
    ExceptionCode        : LongWord;
    ExceptionFlags       : LongWord;
    OuterException       : PExceptionRecord;
    ExceptionAddress     : Pointer;
    NumberParameters     : Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of Longint);
    False: (ExceptAddr: Pointer; ExceptObject: Pointer);
  end;

Var
  oldRTLUnwindProc: procedure; stdcall;
  writeToFile : boolean = false;

(*procedure MyRtlUnwind; stdcall;
var
  PER : PExceptionRecord;

  procedure DoIt;
  var             // This is done in a sub-routine because string variable is used and we want it finalized
    E: Exception;
    S:AnsiString;
  begin
    s:='--------------------------------------------------------'#13#10;
    s:=s+'New exception:'#13#10;

    if PER^.ExceptionFlags and 1=1 then      // This seems to be an indication of internal Delphi exception,
    begin                                    // thus we can access 'Exception' class
      try
        E := Exception( PER^.ExceptObject);
        if (E is Exception) then
          s:=s+'Delphi exception, type '+E.ClassName+', message: '+E.Message+#13#10;
      except
      end;
    end;

    DebugStack(S);

    MemCheck.RaiseExceptionsOnEnd := False;
  End;
begin
  asm
    mov eax, dword ptr [EBP+8+13*4]         // magic numbers - works for Delphi 7
    mov PER, eax
  end;

  DoIt;

  asm
    mov esp, ebp
    pop ebp
    jmp oldRTLUnwindProc
  end;
End;*)

Procedure TERRADump(S:AnsiString);
Begin
  Raise Exception.Create(S);
End;

{$ELSE}
Procedure DebugStack(S:AnsiString);
Var
  i:Integer;
  Frames: PPointer;
Begin
  Frames:=ExceptFrames;
  For I:=0 To Pred(ExceptFrameCount) Do
    S := S + BackTraceStrFunc(Frames) + crLf;
  Log(logError,'Callstack', S);
End;

Procedure TERRADump(S:AnsiString);
Begin
  DebugStack('');
  RaiseError(S);
        While (True) Do;
End;
{$ENDIF}

Function GetCallstack:AnsiString;
Var
  P:StringObject;
Begin
  Result := '';
  If (_Callstack = Nil) Then
    Exit;

  P := StringObject(_Callstack.First);
  While P<>Nil Do
  Begin
    Result := Result + P.Value + crLf;
    P := StringObject(P.Next);
  End;
End;


Procedure DebugOpenAL;
Var
  ErrorCode:Cardinal;
  S:AnsiString;
Begin
  ErrorCode := alGetError;
  If ErrorCode = GL_NO_ERROR Then
    Exit;

  Case ErrorCode Of
  AL_INVALID_NAME: S := 'Invalid Name paramater passed to AL call.';
  AL_INVALID_ENUM: S := 'Invalid parameter passed to AL call.';
  AL_INVALID_VALUE: S := 'Invalid enum parameter value.';
  AL_INVALID_OPERATION: S:= 'Invalid operation';
  Else
    S := 'Unknown AL error.';
  End;

  S := 'OpenAL Error ['+S+']';
  TERRADump(S);
End;

{$IFDEF WINDOWS}

Procedure InitExceptionLogging;
Begin
  oldRTLUnwindProc := RTLUnwindProc;
//  RTLUnwindProc := @MyRtlUnwind;
End;

Initialization
  InitExceptionLogging;
{$ENDIF}
End.

