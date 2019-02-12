{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
  TERRA_Sound, TERRA_SoundSource, TERRA_AudioBuffer, TERRA_AudioMixer;

Const
  StreamingAudioSampleCount = DefaultAudioSampleCount * 16;

Type
  SoundStreamClass = Class Of SoundStream;

  SoundStream = Class(SoundSource)
    Protected
      _Source:TERRAStream;

      Procedure InitStream; Virtual; Abstract;
      Class Function Validate(Source:TERRAStream):Boolean; Virtual; Abstract;

    Public
      Constructor Create(Mode:SoundSourceMode; Source:TERRAStream);
      Procedure Release; Override;
  End;

  NullSoundStreamer = Class(SoundStream)
	  Protected
		  Procedure InitStream; Override;
	  	Class Function Validate(Source:TERRAStream):Boolean; Override;
  	Public
  End;

Procedure RegisterSoundStreamFormat(MyClass:SoundStreamClass);

Function CreateSoundStream(Mode:SoundSourceMode; Source:TERRAStream):SoundStream;

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

Function CreateSoundStream(Mode:SoundSourceMode; Source:TERRAStream):SoundStream;
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
      Result := _SoundStreamClassList[I].Create(Mode, Source);
      Exit;
    End;

    Source.Seek(Ofs);
  End;
End;

Procedure NullSoundStreamer.InitStream;
Begin
End;

Class Function NullSoundStreamer.Validate(Source:TERRAStream):Boolean;
Begin
	Result := True;
End;

//  SoundStreamer
Constructor SoundStream.Create(Mode:SoundSourceMode; Source:TERRAStream);
Begin
  Inherited Create();

  _Source := Source;

  Self._Status := soundSource_Playing;

  Self.InitStream;
  Self.RequestMoreSamples();
End;

Procedure SoundStream.Release;
Begin
  ReleaseObject(_Buffer);
  ReleaseObject(_Source);
End;


End.
