Unit TERRA_Process;

Interface

Uses TERRA_Utils, TERRA_String, {$IFDEF FPC}Process{$ELSE}Windows{$ENDIF};

Type
  Process = Class(TERRAObject)
    Protected
      {$IFDEF FPC}
      _Process:TProcess;
      {$ELSE}
      _Command:TERRAString;
      _Options:TERRAString;

      _Read: THandle;
      _Write: THandle;
      _Security: TSecurityAttributes;
      _Startup: TStartupInfo;
      _Process: TProcessInformation;

      _Failed:Boolean;
      {$ENDIF}

      Procedure ProcessLine(Const Text:TERRAString); Virtual;

    Public
      Procedure Release; Override;

      Procedure Execute;

      Procedure AddOption(Const Option:TERRAString);

      Property Executable:TERRAString Read _Command Write _Command;
  End;


Implementation

Type
  TMethod = procedure of object;

(*procedure WaitUntilSignaled(Handle: THandle; const ProcessMessages: TMethod);
begin
  if Assigned(ProcessMessages) then begin
    ProcessMessages;//in case there are any messages are already waiting in the queue
    while MsgWaitForMultipleObjects(1, Handle, False, INFINITE, QS_ALLEVENTS)=WAIT_OBJECT_0+1 do begin
      ProcessMessages;
    end;
  end else begin
    WaitForSingleObject(Handle, INFINITE);
  end;
end;

function DefaultShellExecuteInfo(const Action, Filename, Parameters, Directory: string): TShellExecuteInfo;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.cbSize := SizeOf(TShellExecuteInfo);
  Result.fMask := SEE_MASK_NOCLOSEPROCESS;
  if Assigned(Application.MainForm) then begin
    Result.Wnd := Application.MainFormHandle;
  end;
  Result.lpVerb := PChar(Action);
  Result.lpFile := PChar(Filename);
  Result.lpParameters := PChar(Parameters);
  Result.lpDirectory := PChar(Directory);
  Result.nShow := SW_SHOWNORMAL;
end;

function MyShellExecute(const ShellExecuteInfo: TShellExecuteInfo; out ExitCode: DWORD; Wait: Boolean; const ProcessMessages: TMethod): Boolean; overload;
begin
  Result := ShellExecuteEx(@ShellExecuteInfo);
  if Result and (ShellExecuteInfo.hProcess<>0) then begin
    Try
      if Wait then begin
        WaitUntilSignaled(ShellExecuteInfo.hProcess, ProcessMessages);
        GetExitCodeProcess(ShellExecuteInfo.hProcess, ExitCode);
      end;
    Finally
      CloseHandle(ShellExecuteInfo.hProcess);
    End;
  end;
end;

function MyShellExecute(const ShellExecuteInfo: TShellExecuteInfo; out ExitCode: DWORD; const ProcessMessages: TMethod): Boolean; overload;
begin
  Result := MyShellExecute(ShellExecuteInfo, ExitCode, True, ProcessMessages);
end;

function MyShellExecute(const ShellExecuteInfo: TShellExecuteInfo; Wait: Boolean; const ProcessMessages: TMethod): Boolean; overload;
var
  ExitCode: DWORD;
begin
  Result := MyShellExecute(ShellExecuteInfo, ExitCode, Wait, ProcessMessages);
end;

type
  TShellExecuteMessageHandler = record
  public
    procedure ProcessMessages;
  end;

procedure TShellExecuteMessageHandler.ProcessMessages;
begin
  Application.ProcessMessages;
  if Application.Terminated then begin
    Abort;
  end;
end;

function MyShellExecute(const Action, Filename, Parameters, Directory: string; Wait: Boolean): Boolean; overload;
var
  MessageHandler: TShellExecuteMessageHandler;
begin
  Try
    Result := MyShellExecute(
      DefaultShellExecuteInfo(Action, FileName, Parameters, Directory),
      Wait,
      MessageHandler.ProcessMessages
    );
  Except
    on EAbort do begin
      Result := False;//the wait has been terminated before the process signaled
    end;
  End;
end;*)

{ Process }
Procedure Process.AddOption(const Option: TERRAString);
Begin
  _Options := _Options + Option + ' ';
End;

Procedure Process.Release;
Begin
End;

Procedure Process.Execute;
Const
  ReadBufferSize = 2400;
Var
  Buffer: Array[0..ReadBufferSize] Of AnsiChar;
  Running:Cardinal;
  ReadCount:Cardinal;
Begin
  _Failed := False;
  _Security.nLength := SizeOf(TSecurityAttributes);
  _Security.bInheritHandle := True;
  _Security.lpSecurityDescriptor := nil;

  If CreatePipe(_Read, _Write, @_Security, 0) Then
  Begin
    FillChar(_Startup, SizeOf(TStartupInfo), #0);
    _Startup.cb := SizeOf(TStartupInfo);
    _Startup.hStdInput := _Read;
    _Startup.hStdOutput := _Write;
    _Startup.hStdError := _Write;
    _Startup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    _Startup.wShowWindow := SW_HIDE;

    If CreateProcess(nil, PChar(_Command + ' ' + _Options), @_Security, @_Security, True, NORMAL_PRIORITY_CLASS, nil, nil, _Startup, _Process) Then
    Begin
      Repeat
        Running := WaitForSingleObject(_Process.hProcess, 100);
        //Application.ProcessMessages();
        Repeat
          ReadCount := 0;
          ReadFile(_Read, Buffer[0], ReadBufferSize, ReadCount, Nil);
          Buffer[ReadCount] := #0;

          OemToAnsi(Buffer, Buffer);
          Self.ProcessLine(String(Buffer));
         Until (ReadCount < ReadBufferSize);
      Until (Running <> WAIT_TIMEOUT);

      CloseHandle(_Read);
      CloseHandle(_Write);
    End Else
      _Failed := True;
  End Else
    _Failed := True;
End;

Procedure Process.ProcessLine(const Text: TERRAString);
Begin
  // do nothing
End;

(*
http://wiki.freepascal.org/Executing_External_Programs#An_improved_example_.28but_not_correct_yet.29

  Process := TProcess.Create(nil);

  S := Self.Params;
  ReplaceText('$FILE', FileName, S);

  Process.Executable := Self.Command;
  Process.Parameters.Add(S);

  {$IFDEF FPC}
  Process.Options := [poUsePipes];
  {$ENDIF}

  Process.Execute();

  While True do
  Begin
    // try reading it
    NumBytes := Process.Output.Read(Buffer[0], READ_BYTES);
    If NumBytes > 0 Then
    Begin
      Inc(BytesRead, NumBytes);
    End Else
      Break;
  End;

  Process.Free;
*)
End.
