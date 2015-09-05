Unit TERRA_AlsaAudioDriver;

Interface

Uses TERRA_Error, TERRA_Utils, TERRA_String, TERRA_AudioMixer, TERRA_AudioBuffer, Alsa;

Const
  DefaultAlsaPeriods = 4;
  DefaultAlsaPeriodSize = 2048;
  InternalBufferSampleCount = 4096 * 2;

Type
  AlsaAudioDriver = Class(TERRAAudioDriver)
    Protected
      _Handle:Psnd_pcm_t;
      _Buffer:PAudioSample;
      _Callback:Psnd_async_handler_t;

      Procedure Fill();

    Public
      Function Reset(Mixer:TERRAAudioMixer):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;

Implementation
Uses TERRA_Log;

Var
   _Driver:AlsaAudioDriver;

Function CheckStatus(Status:Integer; ErrorMsg:TERRAString):Boolean;
Begin
     Result := (Status>=0);
     If Result Then
        Exit;

     ErrorMsg := ErrorMsg+ ' ' + snd_strerror(Status);
     Log(logError, 'ALSA', ErrorMsg);
End;


(*Procedure AlsaCallback(pcm_callback:Psnd_async_handler_t); CDecl;
Begin
     _Driver.Fill();
End;*)

{ AlsaAudioDriver }
Function AlsaAudioDriver.Reset(Mixer:TERRAAudioMixer):Boolean;
Var
  hw_params:Psnd_pcm_hw_params_t;
  sw_params:Psnd_pcm_sw_params_t;

  periods:Integer;
  buffer_size:snd_pcm_uframes_t;
  period_size:snd_pcm_uframes_t;

  Freq:Cardinal;
Begin
  Result := False;
  Self._Mixer := Mixer;
  _Driver := Self;

  If Not CheckStatus( snd_pcm_open(_Handle, 'default', SND_PCM_STREAM_PLAYBACK, SND_PCM_ASYNC), 'Cannot open sound device...') Then
    Exit;

  If Not CheckStatus( snd_pcm_hw_params_malloc(hw_params),  'Cannot allocate hardware parameter structure ...') Then
    Exit;

  If Not CheckStatus( snd_pcm_hw_params_any (_Handle, hw_params), 'Cannot initialize hardware parameter structure  ...') Then
    Exit;

  If Not CheckStatus( snd_pcm_hw_params_set_access (_Handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED),   'Cannot set access type...') Then
    Exit;

  If Not CheckStatus( snd_pcm_hw_params_set_format (_Handle, hw_params, SND_PCM_FORMAT_S16), 'Cannot set sample format ...') Then
    Exit;

  Freq := _Mixer.Buffer.Frequency;
  If Not CheckStatus( snd_pcm_hw_params_set_rate_near(_Handle, hw_params, @Freq, Nil), 'Cannot set sample rate ...') Then
    Exit;

  If Not CheckStatus( snd_pcm_hw_params_set_channels (_Handle, hw_params, 2),  'Cannot set channel count ...') Then
    Exit;

  periods := DefaultAlsaPeriods;
  period_size := DefaultAlsaPeriodSize;
  buffer_size := period_size * periods * 4;
  If Not CheckStatus( snd_pcm_hw_params_set_periods(_Handle, hw_params, periods, 0), 'Cannot set ALSA period') Then
     Exit;

  If Not CheckStatus( snd_pcm_hw_params_set_period_size_near(_Handle, hw_params, @period_size, Nil), 'Cannot set ALSA period size') Then
     Exit;

  If Not CheckStatus( snd_pcm_hw_params_set_buffer_size_near(_Handle, hw_params, @buffer_size), 'Cannot set ALSA buffer size ') Then
    Exit;


  If Not CheckStatus( snd_pcm_hw_params (_Handle, hw_params),  'Cannot set hardware parameters...') Then
    Exit;

  snd_pcm_hw_params_free (hw_params);


  // tell ALSA to wake us up whenever 4096 or more frames of playback data can be delivered.
  // Also, tell ALSA that we'll start the device ourselves.

  If Not CheckStatus(snd_pcm_sw_params_malloc(sw_params), 'cannot allocate software parameters structure') Then
    Exit;

  If Not CheckStatus(snd_pcm_sw_params_current(_Handle, sw_params), 'cannot initialize software parameters structure') Then
     Exit;

(*  If Not CheckStatus(snd_pcm_sw_params_set_avail_min(_Handle, sw_params, 1024), 'cannot set minimum available count') Then
    Exit;*)

  If Not CheckStatus(snd_pcm_sw_params_set_start_threshold(_Handle, sw_params, 0), 'cannot set start mode') Then
     Exit;

  If Not CheckStatus(snd_pcm_sw_params(_Handle, sw_params), 'cannot set software parameters') Then
    Exit;

(*  If Not CheckStatus(snd_async_add_pcm_handler(_callback, _Handle, AlsaCallback, Nil), 'failed registering alsa callback') Then
    Exit;*)

  If Not CheckStatus( snd_pcm_prepare (_Handle), 'Cannot prepare audio interface for use...') Then
    Exit;

  GetMem(_Buffer, InternalBufferSampleCount * 2 * SizeOf(AudioSample));

  Result := True;
End;

Procedure AlsaAudioDriver.Release;
Begin
//  snd_pcm_drain(_Handle);
  snd_pcm_drop(_Handle);
  snd_pcm_close(_Handle);
  FreeMem(_Buffer);
End;

Procedure AlsaAudioDriver.Update();
Begin
(*  If Not _Mixer.Active Then
     Exit;*)

  Self.Fill();
End;

Procedure AlsaAudioDriver.Fill();
Var
   Count, Written, avail:snd_pcm_sframes_t;
Begin
  //snd_pcm_wait(_Handle, 1000);

  avail := snd_pcm_avail_update(_Handle);
  While Avail>0 Do
  Begin
       Count := Avail;
       If (Count > InternalBufferSampleCount) Then
          Count := InternalBufferSampleCount;

       Written := _Mixer.RequestSamples(_Buffer, Count);
       If Written>0 Then
       Begin
          If snd_pcm_writei(_Handle, _Buffer, Written)<0 Then
                    snd_pcm_prepare (_Handle);
          Dec(Avail, Count);
       End;

  End;
End;

End.
