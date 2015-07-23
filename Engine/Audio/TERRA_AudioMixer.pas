Unit TERRA_AudioMixer;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_String, TERRA_Threads, TERRA_Mutex;

Const
  DefaultAudioBufferSize:Integer = 8192;

Type
  PAudioSample = ^AudioSample;
  AudioSample = Word;

  TERRAAudioMixer = Class;

  TERRAAudioDriver = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Frequency:Cardinal;
      _OutputBufferSize:Cardinal;

      _Mixer:TERRAAudioMixer;

    Public
      Function Reset(AFrequency, InitBufferSize:Cardinal; Mixer:TERRAAudioMixer):Boolean; Virtual; Abstract;
      Procedure Update(); Virtual; Abstract;
  End;


  TERRAAudioMixer = Class(TERRAObject)
    Protected
       _Frequency:Cardinal;
       _OutputBufferSize:Cardinal;

       _Buffer:Array Of Cardinal;

       _Thread:TERRAThread;
       _Mutex:CriticalSection;
       _ThreadTerminated:Boolean;

       _Driver:TERRAAudioDriver;

       _CurrentOffset:Integer;

       Procedure Update();

       Procedure Enter();
       Procedure Leave();

       Procedure Render(DestBuffer:PAudioSample; Offset, Samples:Integer); Virtual;

    Public
       Constructor Create(AFrequency, InitBufferSize:Cardinal);
       Procedure Release(); Override;

       Procedure Start();
       Procedure Stop();

       Procedure RequestSamples(DestBuffer:PAudioSample; Samples:Integer);

       Property Frequency:Cardinal Read _Frequency;
  End;

  AudioMixerThread = Class(TERRAThread)
    Protected
      _Mixer:TERRAAudioMixer;
    Public
      Constructor Create(Mixer:TERRAAudioMixer);
      Procedure Execute; Override;
  End;


Implementation

Uses TERRA_Error, TERRA_OS
{$IFDEF WINDOWS}
, TERRA_WinMMAudioDriver
{$ENDIF}

{$IFDEF OSX}
, TERRA_CoreAudioDriver
{$ENDIF}
;

{ TERRAAudioMixer }
Constructor TERRAAudioMixer.Create(AFrequency, InitBufferSize:Cardinal);
Var
  I:Integer;
Begin
  _Frequency := AFrequency;
 _OutputBufferSize := InitBufferSize;

  SetLength(_Buffer, _OutputBufferSize * 2);

  {$IFDEF WINDOWS}
  _Driver := WindowsAudioDriver.Create();
  {$ENDIF}

  {$IFDEF OSX}
  _Driver := CoreAudioDriver.Create();
  {$ENDIF}

  _Driver.Reset(AFrequency, InitBufferSize, Self);

 _ThreadTerminated := False;
 _Mutex := CriticalSection.Create();
 _Thread := AudioMixerThread.Create(Self);
// CREATE_SUSPENDED
// SetThreadPriority(_ThreadHandle, THREAD_PRIORITY_TIME_CRITICAL);
End;

Procedure TERRAAudioMixer.Release();
Begin
  Self.Enter();
  _ThreadTerminated := True;
  Self.Leave();

  _Thread.Terminate();
  ReleaseObject(_Thread);

  ReleaseObject(_Mutex);

  ReleaseObject(_Driver);

  SetLength(_Buffer, 0);
End;

Procedure TERRAAudioMixer.Enter;
Begin
  _Mutex.Lock();
End;

Procedure TERRAAudioMixer.Leave;
Begin
  _Mutex.Unlock();
End;

Procedure TERRAAudioMixer.Start;
Begin
  _Thread.Resume();
// ResumeThread(_ThreadHandle);
End;

procedure TERRAAudioMixer.Stop;
begin
  _Thread.Suspend();
// SuspendThread(_ThreadHandle);
end;


Procedure TERRAAudioMixer.Render(DestBuffer:PAudioSample; Offset, Samples:Integer);
Begin

End;

Procedure TERRAAudioMixer.Update();
Begin
  Self.Enter();
  Self._Driver.Update();
  Self.Leave();
End;

Procedure TERRAAudioMixer.RequestSamples(DestBuffer:PAudioSample; Samples:Integer);
Begin
     Self.Render(DestBuffer, _CurrentOffset, Samples);
     Inc(_CurrentOffset, Samples);
If (_CurrentOffset>_OutputBufferSize) Then
   _CurrentOffset := 0;
End;

{ AudioMixerThread }
Constructor AudioMixerThread.Create(Mixer: TERRAAudioMixer);
Begin
  _Mixer := Mixer;

  Inherited Create();
End;

Procedure AudioMixerThread.Execute;
Begin
  While Not _Mixer._ThreadTerminated DO
  Begin
    _Mixer.Update();
    Application.Sleep(50);
  End;
End;

End.
