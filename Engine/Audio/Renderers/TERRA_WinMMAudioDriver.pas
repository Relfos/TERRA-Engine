Unit TERRA_WinMMAudioDriver;

Interface

Uses Windows, Messages, MMSystem, TERRA_AudioMixer;

Type
  TWAVEHDR = WAVEHDR;

  WindowsAudioDriver = Class(TERRAAudioDriver)
    Protected
       _CurrentBuffer:Cardinal;

       _WaveFormat:TWaveFormatEx;
       _WaveHandle:Cardinal;
       _WaveOutHandle:Cardinal;
       _WaveHandler:ARRAY[0..3] OF PWAVEHDR;

    Public
      Function Reset(AFrequency, MaxSamples:Cardinal; Mixer:TERRAAudioMixer):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;

Implementation

{ WindowsAudioDriver }
Function WindowsAudioDriver.Reset(AFrequency, MaxSamples:Cardinal; Mixer:TERRAAudioMixer):Boolean;
Var
  I:Integer;
Begin
  Self._Frequency := AFrequency;
  Self._OutputBufferSize := MaxSamples * 2 * 2; // 2 channels * 16 bits
  Self._Mixer := Mixer;

  _WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  _WaveFormat.nChannels := 2;
  _WaveFormat.wBitsPerSample := 16;

  _WaveFormat.nBlockAlign := _WaveFormat.nChannels * _WaveFormat.wBitsPerSample DIV 8;
  _WaveFormat.nSamplesPerSec := _Frequency;
  _WaveFormat.nAvgBytesPerSec := _WaveFormat.nSamplesPerSec * _WaveFormat.nBlockAlign;
  _WaveFormat.cbSize := 0;
  _WaveHandle := waveOutOpen(@_WaveOutHandle, WAVE_MAPPER, @_WaveFormat,0,0,0);

  For I:=0 TO 3 Do
  Begin
    GetMem(_WaveHandler[I], SizeOf(TWAVEHDR));
    _WaveHandler[I].dwFlags := WHDR_DONE;
    GetMem(_WaveHandler[I].lpData, _OutputBufferSize);
    FillChar(_WaveHandler[I].lpData^, _OutputBufferSize, #0);
    _WaveHandler[I].dwBufferLength := _OutputBufferSize;
    _WaveHandler[I].dwBytesRecorded := 0;
    _WaveHandler[I].dwUser := 0;
    _WaveHandler[I].dwLoops := 0;
  End;

  _CurrentBuffer := 0;

  Result := True;
End;

Procedure WindowsAudioDriver.Release;
Var
  I:Integer;
Begin
  waveOutReset(_WaveOutHandle);
  For I:=0 TO 3 Do
  Begin
    While waveOutUnprepareHeader(_WaveOutHandle, _WaveHandler[I], SIZEOF(TWAVEHDR))=WAVERR_STILLPLAYING Do
    Begin
     Sleep(25);
    End;
  End;
  waveOutReset(_WaveOutHandle);
  waveOutClose(_WaveOutHandle);
  For I:=0 TO 3 Do
  Begin
    FreeMem(_WaveHandler[I].lpData);
    FreeMem(_WaveHandler[I]);
  End;
End;

Procedure WindowsAudioDriver.Update();
Var
  I:Integer;
Begin
  For I:=1 To 4 Do
  Begin
    If (_WaveHandler[_CurrentBuffer].dwFlags AND WHDR_DONE)<>0 Then
    Begin
      If waveOutUnprepareHeader(_WaveOutHandle, _WaveHandler[_CurrentBuffer], SizeOf(TWAVEHDR)) <> WAVERR_STILLPLAYING Then
      Begin
        _WaveHandler[_CurrentBuffer].dwFlags := _WaveHandler[_CurrentBuffer].dwFlags And (Not WHDR_DONE);
        _Mixer.RequestSamples(PAudioSample(_WaveHandler[_CurrentBuffer].lpData), _Mixer.SampleBufferSize);
        waveOutPrepareHeader(_WaveOutHandle, _WaveHandler[_CurrentBuffer], SizeOf(TWAVEHDR));
        waveOutWrite(_WaveOutHandle, _WaveHandler[_CurrentBuffer], SizeOf(TWAVEHDR));
        _CurrentBuffer := (_CurrentBuffer+1) Mod 4;
      End;
    End;
  End;

End;

End.
