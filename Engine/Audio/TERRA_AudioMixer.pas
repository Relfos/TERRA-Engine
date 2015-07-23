Unit TERRA_AudioMixer;

{$I terra.inc}

Interface
Uses Windows, TERRA_Utils, TERRA_String;

Const
  DefaultAudioBufferSize:Integer = 8192;

Type
  AudioRenderBufferProc = Procedure(DestBuffer:PWord) Of Object;

  TERRAAudioDriver = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Frequency:Cardinal;
      _OutputBufferSize:Cardinal;

      _RenderProc:AudioRenderBufferProc;

    Public
      Function Reset(AFrequency, InitBufferSize:Cardinal; RenderProc:AudioRenderBufferProc):Boolean; Virtual; Abstract;
      Procedure Update(); Virtual; Abstract;
  End;


  TERRAAudioMixer = Class(TERRAObject)
    Protected
       _Frequency:Cardinal;
       _OutputBufferSize:Cardinal;

       _Buffer:Array Of Cardinal;

       _ThreadHandle:THandle;
       _ThreadID:THandle;
       _ThreadCriticalSection:TRTLCriticalSection;
       _ThreadTerminated:Boolean;

       _Driver:TERRAAudioDriver;

       Procedure Render(DestBuffer:PWord); Virtual;

       Procedure Update();

       Procedure Enter();
       Procedure Leave();

    Public
       Constructor Create(AFrequency, InitBufferSize:Cardinal);
       Procedure Release(); Override;

       Procedure Start();
       Procedure Stop();

       Property Frequency:Cardinal Read _Frequency;
  End;


Implementation

Uses TERRA_Error
{$IFDEF WINDOWS}
, TERRA_WinMMAudioDriver
{$ENDIF}

{$IFDEF OSX}
, TERRA_CoreAudioDriver
{$ENDIF}
;

Procedure AudioThreadProc(Mixer:TERRAAudioMixer);
Begin
  While Not Mixer._ThreadTerminated DO
  Begin
    Mixer.Update();
    Sleep(50);
  End;
{$IFDEF FPC}
 ExitThread(0);
{$ELSE}
 EndThread(0);
{$ENDIF}
End;

{ TERRAAudioMixer }
Constructor TERRAAudioMixer.Create(AFrequency, InitBufferSize:Cardinal);
Var
  I:Integer;
Begin
  _Frequency := AFrequency;
 _OutputBufferSize := InitBufferSize;

  SetLength(_Buffer, _OutputBufferSize * 2);

  _Driver := WindowsAudioDriver.Create();
  _Driver.Reset(AFrequency, InitBufferSize, Self.Render);

 _ThreadTerminated := False;
 InitializeCriticalSection(_ThreadCriticalSection);
{$IFDEF FPC}
 _ThreadHandle := CreateThread(NIL,0,@AudioThreadProc, Self, CREATE_SUSPENDED, _ThreadID);
{$ELSE}
 _ThreadHandle := BeginThread(NIL,0,@AudioThreadProc, Self, CREATE_SUSPENDED, _ThreadID);
{$ENDIF}
 SetThreadPriority(_ThreadHandle, THREAD_PRIORITY_TIME_CRITICAL);
End;

Procedure TERRAAudioMixer.Release();
Begin
  Self.Enter();
  _ThreadTerminated := True;
  Self.Leave();

  WaitForSingleObject(_ThreadHandle, 25);
  TerminateThread(_ThreadHandle,0);
  WaitForSingleObject(_ThreadHandle, 5000);
  IF _ThreadHandle<>0 Then
    CloseHandle(_ThreadHandle);
  DeleteCriticalSection(_ThreadCriticalSection);

  ReleaseObject(_Driver);

  SetLength(_Buffer, 0);
End;

Procedure TERRAAudioMixer.Enter;
Begin
  EnterCriticalSection(_ThreadCriticalSection);
End;

Procedure TERRAAudioMixer.Leave;
Begin
  LeaveCriticalSection(_ThreadCriticalSection);
End;

Procedure TERRAAudioMixer.Start;
Begin
 ResumeThread(_ThreadHandle);
End;

procedure TERRAAudioMixer.Stop;
begin
 SuspendThread(_ThreadHandle);
end;


Procedure TERRAAudioMixer.Render(DestBuffer: PWord);
Begin

End;

Procedure TERRAAudioMixer.Update();
Begin
  Self.Enter();
  Self._Driver.Update();
  Self.Leave();
End;

End.
