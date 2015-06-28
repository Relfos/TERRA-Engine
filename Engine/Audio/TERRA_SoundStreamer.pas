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
 * TERRA_SoundStreamer
 * Implements a generic Sound Streamer, that can be used to stream audio to a sound source
 ***********************************************************************************************************************
}

Unit TERRA_SoundStreamer;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_Math, TERRA_Collections, TERRA_Application, TERRA_Stream, TERRA_FileStream,
  TERRA_Sound, TERRA_AL;

Const
  StreamBufferSize = 1024 * 1024;   //16Kb for streaming buffers, change if necessary

Type
  SoundStreamClass = Class Of SoundStream;
  SoundStream = Class(TERRAObject)
    Protected
      _Source:Stream;
      _Handle:Cardinal;
      _Buffers:Array[0..1] Of Cardinal;
      _Channels:Cardinal;
      _Frequency:Cardinal;
      _BitsPerSample:Cardinal;
      _Data:Pointer;
      _BufferSize:Cardinal;

      _StartTime:Cardinal;
      _Volume:Single;
      _Status:Integer;

      Procedure SetVolume(Value:Single);

      Procedure UpdateSettings;

      Procedure AllocBuffer(Channels, BitsPerSample, Frequency:Cardinal);

      Procedure InitStream; Virtual; Abstract;
      Procedure Stream(Buffer:Cardinal); Virtual;
      Class Function Validate(Source:Stream):Boolean; Virtual; Abstract;

    Public
      Constructor Create(Source:Stream);
      Procedure Release; Override;

      Procedure Update;

      Procedure Play;
      Procedure Pause;
      Procedure Stop;

      Property StartTime:Cardinal Read _StartTime;
      Property Volume:Single Read _Volume Write SetVolume;
  End;

  NullSoundStreamer = Class(SoundStream)
	Protected
		Procedure InitStream; Override;
		Procedure Stream(Buffer:Cardinal); Override;
		Class Function Validate(Source:Stream):Boolean; Override;
	Public
  End;

Procedure RegisterSoundStreamFormat(MyClass:SoundStreamClass);

Function CreateSoundStream(Source:Stream):SoundStream;

Implementation
Uses TERRA_OS, TERRA_GraphicsManager, TERRA_SoundManager, TERRA_SoundAmbience, TERRA_Log;

Var
  _SoundStreamClassList:Array Of SoundStreamClass;
  _SoundStreamClassCount:Integer = 0;

Procedure RegisterSoundStreamFormat(MyClass:SoundStreamClass);
Begin
  Inc(_SoundStreamClassCount);
  SetLength(_SoundStreamClassList, _SoundStreamClassCount);
  _SoundStreamClassList[Pred(_SoundStreamClassCount)] := MyClass;
End;

Function CreateSoundStream(Source:Stream):SoundStream;
Var
  I:Integer;
  Ofs:Cardinal;
Begin
  Result := Nil;
  Ofs := Source.Position;
  For I:=0 To Pred(_SoundStreamClassCount) Do
  Begin
    If _SoundStreamClassList[I].Validate(Source) Then
    Begin
      Source.Seek(Ofs);
      Result := _SoundStreamClassList[I].Create(Source);
      Exit;
    End;

    Source.Seek(Ofs);
  End;
End;

Procedure NullSoundStreamer.InitStream; 
Begin
End;

Procedure NullSoundStreamer.Stream(Buffer:Cardinal); 
Begin
End;

Class Function NullSoundStreamer.Validate(Source:Stream):Boolean; 
Begin
	Result := True;
End;

//  SoundStreamer
Constructor SoundStream.Create(Source:Stream);
Begin
  _Handle := 0;
  _Buffers[0] := 0;
  _Buffers[1] := 0;
  _Volume := 1.0;
  _Status := sndStopped;
  _Source := Source;
  _BufferSize := StreamBufferSize;

  Self.InitStream;
End;

Procedure SoundStream.Release;
Var
  Buffer:Cardinal;
  Queued:Integer;
Begin
  Stop;

  If Assigned(_Data) Then
    FreeMem(_Data);

  If (_Handle<>0) Then
  Begin
    alGetSourcei(_Handle, AL_BUFFERS_QUEUED, @queued); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    While (Queued>0) Do
    Begin
      alSourceUnqueueBuffers(_Handle, 1, @buffer);   {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
      Dec(Queued);
    End;

    alDeleteBuffers(2, @_Buffers[0]);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alDeleteSources(1, @_Handle);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    _Handle := 0;
  End;

  ReleaseObject(_Source);
End;

Procedure SoundStream.AllocBuffer(Channels, BitsPerSample, Frequency:Cardinal);
Begin
  If Assigned(_Data) Then
    FreeMem(_Data);

  _Channels := Channels;
  _Frequency := Frequency;
  _BitsPerSample := BitsPerSample;

  GetMem(_Data, StreamBufferSize);
End;

Var
  F:Stream;
Procedure SoundStream.Stream(Buffer:Cardinal);
Begin
  alBufferData(Buffer, AL_FORMAT_STEREO16, _Data, _BufferSize, _Frequency);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

  {If F=Nil Then
    F := FileStream.Create('raw');

  F.Write(_Data^, _Buffersize)}
End;

Procedure SoundStream.UpdateSettings;
Begin
  If (_Handle<=0) Then
    Exit;

  alSourcei(_Handle, AL_LOOPING, 0);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alSourcef(_Handle, AL_PITCH, 1.0);         {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alSourcef(_Handle, AL_GAIN, _Volume);         {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
End;

Procedure SoundStream.SetVolume(Value:Single);
Begin
  If (_Volume = Value) Then
    Exit;

  _Volume := Value;
  UpdateSettings;
End;

Procedure SoundStream.Update;
Var
  Processed:Integer;
  State, Buffer:Cardinal;
Begin
  If _Status <> sndPlaying Then
    Exit;

  alGetSourcei(_Handle, AL_SOURCE_STATE, @state);
  If (State <> AL_PLAYING) Then
  Begin
    Stream(_Buffers[0]);
    Stream(_Buffers[1]);

    alSourceQueueBuffers(_Handle, 2, @(_Buffers[0]));
    alSourcePlay(_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    Exit;
  End;

  alGetSourcei(_Handle, AL_BUFFERS_PROCESSED, @Processed);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  If Processed>0 Then
  Repeat
    alSourceUnqueueBuffers(_Handle, 1, @Buffer);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    Stream(Buffer);
    alSourceQueueBuffers(_Handle, 1, @Buffer);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    Dec(Processed);
  Until Processed<=0;
End;

Procedure SoundStream.Play;
Begin
  If (_Status = sndStopped) Then
    _StartTime := Application.GetTime;

  _Status := sndPlaying;

  If (_Handle <=0) Then
  Begin
    alGenSources(1, @_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alGenBuffers(2, @_Buffers[0]);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  End;

  UpdateSettings;
End;

Procedure SoundStream.Pause;
Begin
  If _Status = sndStopped Then
    Exit;

  If _Status = sndPlaying Then
    _Status := sndPaused
  Else
    _Status := sndPlaying;

  If _Status = sndPaused Then
    alSourcePause(_Handle)
  Else
    Play;
End;

Procedure SoundStream.Stop;
Begin
  If (_Status = sndStopped) Then
    Exit;

  _Status := sndStopped;

  alSourceStop(_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alSource3i(_Handle, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, AL_FILTER_NULL);
End;

End.
