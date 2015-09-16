Unit TERRA_OpenSLAudioDriver;

Interface

Uses TERRA_Error, TERRA_Utils, TERRA_String, TERRA_AudioMixer, TERRA_AudioBuffer; //, OpenSL;

Const
  InternalBufferSampleCount = 1024 * 4;

  OpenSLWrapperLib = 'libopenslwrapper.so';

Type
  OPENSL_STREAM = Pointer;

//  Open the audio device with a given sampling rate (sr), input and output channels and IO buffer size  in frames. Returns a handle to the OpenSL stream
  Function android_OpenAudioDevice(sampleRate, inchannels, outchannels, bufferframes:Integer):OPENSL_STREAM; CDecl; External OpenSLWrapperLib;

// Close the audio device
  Procedure android_CloseAudioDevice(p:OPENSL_STREAM); CDecl; External OpenSLWrapperLib;

// Read a buffer from the OpenSL stream *p, of size samples. Returns the number of samples read.
  Function android_AudioIn(p:OPENSL_STREAM; buffer:Pointer; size:Integer):Integer; CDecl; External OpenSLWrapperLib;

//  Write a buffer to the OpenSL stream *p, of size samples. Returns the number of samples written.
  Function android_AudioOut(p:OPENSL_STREAM; buffer:Pointer; size:Integer):Integer; CDecl; External OpenSLWrapperLib;

//  Get the current IO block time in seconds
  Function android_GetTimestamp(p:OPENSL_STREAM):Double; CDecl; External OpenSLWrapperLib;

Type
  SLAudioDriver = Class(TERRAAudioDriver)
    Protected
      _Buffer:PAudioSample;
      _Handle:OPENSL_STREAM;

(*      _Engine:PSLEngineItf;
      _outputMix:PSLObjectItf;
      _PlayerObject:PSLObjectItf;
      _PlayerPlay:PSLPlayItf;
      _PlayerBufferQueue:PSLBufferQueueItf;*)

    Public

      Function Reset(Mixer:TERRAAudioMixer):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;




Implementation
Uses TERRA_Log;

(*Procedure SLPlayerCallback(caller:PSLBufferQueueItf; pContext:Pointer); CDecl;
Begin
End;

Function CheckStatus(Const Status:SLResult; Const ErrorStr:TERRAString):Boolean;
Begin
  Result := (Status = SL_RESULT_SUCCESS);
  If Result Then
    Exit;

  Log(logError, 'OpenSL', ErrorStr);
End;*)


{ SLAudioDriver }
Function SLAudioDriver.Reset(Mixer:TERRAAudioMixer):Boolean;
(*Var
  I:Integer;
  req:SLBoolean;
  loc_bufq:SLDataLocator_AndroidSimpleBufferQueue;
  format_pcm:SLDataFormat_PCM;
  audioSrc:SLDataSource;
  loc_outmix:SLDataLocator_OutputMix;
  audioSnk:SLDataSink;*)
Begin
  Self._Mixer := Mixer;

  GetMem(_Buffer, InternalBufferSampleCount * SizeOf(AudioSample) * 2);

  _Handle := android_OpenAudioDevice(Mixer.Buffer.Frequency, 0, 2, InternalBufferSampleCount);

(*  req := SL_BOOLEAN_TRUE;

  If Not CheckStatus(slCreateEngine(@_Engine, 0, Nil, 0, Nil, Nil), 'Failed creating SL engine') Then
    Exit;

  If Not CheckStatus(PSLObjectItf(_Engine).Realize(PSLObjectItf(_Engine), SL_BOOLEAN_FALSE), 'Failed realizing SL engine') Then
    Exit;

  If Not CheckStatus(PSLObjectItf(_Engine).GetInterface(PSLObjectItf(_Engine), @SL_IID_ENGINE, @_Engine), 'Failed getting SL engine interface') Then
    Exit;

  If Not CheckStatus(_Engine.CreateOutputMix(_Engine, _outputMix, 1, @SL_IID_VOLUME, @req), 'Failed creating SL output mix') Then
    Exit;

  If Not CheckStatus(_outputMix.Realize(_outputMix, SL_BOOLEAN_FALSE), 'Failed to realize SL outputmix') Then
    Exit;

  loc_bufq.locatorType := SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE;
  loc_bufq.numBuffers := 2;

  format_pcm.formatType := SL_DATAFORMAT_PCM;
  format_pcm.numChannels := 2;
  format_pcm.samplesPerSec := Mixer.Buffer.Frequency;
  format_pcm.bitsPerSample := SL_PCMSAMPLEFORMAT_FIXED_16;
  format_pcm.containerSize := SL_PCMSAMPLEFORMAT_FIXED_16;
  format_pcm.channelMask := SL_SPEAKER_FRONT_CENTER; // SL_SPEAKER_FRONT_LEFT Or SL_SPEAKER_FRONT_RIGHT
  format_pcm.endianness := SL_BYTEORDER_LITTLEENDIAN;

  audioSrc.pLocator := @loc_bufq;
  audioSrc.pFormat := @format_pcm;

  loc_outmix.locatorType := SL_DATALOCATOR_OUTPUTMIX;
  loc_outmix.outputMix := _outputMix;
  audioSnk.pLocator := @loc_outmix;
  audioSnk.pFormat := Nil;

  If Not CheckStatus(_engine.CreateAudioPlayer(_Engine, _PlayerObject, @audioSrc, @audioSnk, 1, @SL_IID_ANDROIDSIMPLEBUFFERQUEUE, @Req), 'Failed to create SL audio player') Then
    Exit;

  If Not CheckStatus(_PlayerObject.Realize(_PlayerObject, SL_BOOLEAN_FALSE), 'Failed to realize SL audio player') Then
    Exit;

  If Not CheckStatus(_PlayerObject.GetInterface(_PlayerObject, @SL_IID_PLAY, _PlayerPlay), 'Failed to get SL audio player interface') Then
    Exit;

  If Not CheckStatus(_PlayerObject.GetInterface(_PlayerObject, @SL_IID_ANDROIDSIMPLEBUFFERQUEUE, _PlayerBufferQueue), 'Failed to get SL audio queue interface') Then
    Exit;

  If Not CheckStatus(_PlayerBufferQueue.RegisterCallback(_PlayerBufferQueue, SLPlayerCallback, Self), 'Failed to register SL audio queue callback') Then
    Exit;

  If Not CheckStatus(_PlayerPlay.SetPlayState(_PlayerPlay, SL_PLAYSTATE_PLAYING), 'Failed to start SL playback') Then
    Exit;*)

  Result := True;
End;

Procedure SLAudioDriver.Release;
Begin
  android_CloseAudioDevice(_Handle);

  FreeMem(_Buffer);
End;

Procedure SLAudioDriver.Update();
Begin
  //_PlayerBufferQueue.Enqueue(_PlayerBufferQueue,  _Buffer.Samples, _Buffer.SizeInBytes);

  If Not _Mixer.Active Then
    Exit;

  _Mixer.RequestSamples(_Buffer, InternalBufferSampleCount);
  android_AudioOut(_Handle, _Buffer, InternalBufferSampleCount * 2);
End;

End.
