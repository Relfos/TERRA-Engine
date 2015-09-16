Unit TERRA_WinMMAudioDriver;

Interface

Uses Windows, Messages, MMSystem, TERRA_Utils, TERRA_Sound, TERRA_AudioMixer, TERRA_AudioBuffer;

Const
  WaveBufferCount = 4;
  InternalBufferSampleCount = 1024;

Type
  WindowsAudioDriver = Class(TERRAAudioDriver)
    Protected
       _WaveFormat:TWaveFormatEx;
       _WaveHandle:Cardinal;
       _WaveOutHandle:Cardinal;
       _WaveHandler:Array[0..Pred(WaveBufferCount)] Of TWAVEHDR;
       _WaveBufferSize:Cardinal;

       Function QueueBuffer():Boolean;

    Public
      Function Reset(Mixer:TERRAAudioMixer):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;

Implementation

{ WindowsAudioDriver }
Function WindowsAudioDriver.Reset(Mixer:TERRAAudioMixer):Boolean;
Var
  I:Integer;
Begin
  Self._Mixer := Mixer;

  _WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  _WaveFormat.nChannels := 2;
  _WaveFormat.wBitsPerSample := 16;

  _WaveFormat.nBlockAlign := _WaveFormat.nChannels * _WaveFormat.wBitsPerSample DIV 8;
  _WaveFormat.nSamplesPerSec := Mixer.Buffer.Frequency;
  _WaveFormat.nAvgBytesPerSec := _WaveFormat.nSamplesPerSec * _WaveFormat.nBlockAlign;
  _WaveFormat.cbSize := 0;
  _WaveHandle := waveOutOpen(@_WaveOutHandle, WAVE_MAPPER, @_WaveFormat, {PtrUInt(@waveOutProc), PtrUInt(Self), CALLBACK_FUNCTION }0, 0, 0);

  _WaveBufferSize := InternalBufferSampleCount * _WaveFormat.nChannels * SizeOf(AudioSample);

  For I:=0 To Pred(WaveBufferCount) Do
  Begin
    _WaveHandler[I].dwFlags := WHDR_DONE;

    GetMem(_WaveHandler[I].lpData, _WaveBufferSize);

    _WaveHandler[I].dwBufferLength := _WaveBufferSize;
    _WaveHandler[I].dwBytesRecorded := 0;
    _WaveHandler[I].dwUser := 0;
    _WaveHandler[I].dwLoops := 0;


    waveOutPrepareHeader(_WaveOutHandle, @_WaveHandler[I], SizeOf(TWAVEHDR));
    _WaveHandler[I].dwFlags := _WaveHandler[I].dwFlags Or WHDR_DONE;
  End;

  For I:=1 To WaveBufferCount Div 2 Do
    Self.QueueBuffer();

  Result := True;
End;

Procedure WindowsAudioDriver.Release;
Var
  I:Integer;
Begin
  waveOutReset(_WaveOutHandle);
  For I:=0 To Pred(WaveBufferCount) Do
  Begin
    While waveOutUnprepareHeader(_WaveOutHandle, @_WaveHandler[I], SIZEOF(TWAVEHDR))=WAVERR_STILLPLAYING Do
    Begin
     Sleep(25);
    End;
  End;
  waveOutReset(_WaveOutHandle);
  waveOutClose(_WaveOutHandle);
  For I:=0 To Pred(WaveBufferCount) Do
  Begin
    FreeMem(_WaveHandler[I].lpData);
  End;
End;

Procedure WindowsAudioDriver.Update();
Begin
  If Not _Mixer.Active Then
    Exit;

  While Self.QueueBuffer() Do;
End;

Function WindowsAudioDriver.QueueBuffer():Boolean;
Var
  I, J:Integer;
  N:Single;

  OutBuffer:PSmallInt;
Begin
  For I:=0 To Pred(WaveBufferCount) Do
  If (_WaveHandler[I].dwFlags And WHDR_DONE)<>0 Then
  Begin
    //If waveOutUnprepareHeader(_WaveOutHandle, _WaveHandler[I], SizeOf(TWAVEHDR)) <> WAVERR_STILLPLAYING Then
    Begin
      _WaveHandler[I].dwFlags := _WaveHandler[I].dwFlags Xor WHDR_DONE;
      _Mixer.RequestSamples(PAudioSample(_WaveHandler[I].lpData), InternalBufferSampleCount);

      //waveOutPrepareHeader(_WaveOutHandle, _WaveHandler[I], SizeOf(TWAVEHDR));
      waveOutWrite(_WaveOutHandle, @_WaveHandler[I], SizeOf(TWAVEHDR));

      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

End.
