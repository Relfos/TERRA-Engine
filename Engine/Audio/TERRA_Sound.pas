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
 * TERRA_Sound
 * Implements a Sound resource
 ***********************************************************************************************************************
}

Unit TERRA_Sound;
{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Resource, TERRA_Collections, TERRA_AudioBuffer;

Const
  DefaultSampleRate = 44100;

Type
  TERRASound = Class(TERRAResource)
    Protected
      _Buffer:TERRAAudioBuffer;

    Public
      Function Load(Source:TERRAStream):Boolean; Override;
      Function Unload:Boolean; Override;

      Class Function GetManager:TERRAObject; Override;

      Procedure SamplesFromBuffer(Samples, Frequency:Cardinal; Stereo:Boolean; Data:Pointer);
      Procedure SamplesFromStream(Samples, Frequency:Cardinal; Stereo:Boolean; Source:TERRAStream);

      Property Buffer:TERRAAudioBuffer Read _Buffer;
  End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Log, TERRA_SoundManager, TERRA_SoundSource,
  TERRA_AudioConverter, TERRA_FileFormat, TERRA_Engine, TERRA_AudioMixer;

Function TERRASound.Unload:Boolean;
Begin
  ReleaseObject(_Buffer);
  Result := Inherited Unload();
End;

Procedure TERRASound.SamplesFromBuffer(Samples, Frequency:Cardinal; Stereo:Boolean; Data:Pointer);
Var
  Temp:TERRAAudioBuffer;
  Converter:AudioRateConverter;
Begin
  ReleaseObject(_Buffer);

  _Buffer := TERRAAudioBuffer.Create(Samples, Frequency, Stereo);
  Move(Data^, _Buffer.Samples^, _Buffer.SizeInBytes);

  If (Frequency <> DefaultSampleFrequency) Then
  Begin
    Converter := AudioRateConverter.Create(_Buffer);
    Temp := _Buffer;
    _Buffer := Converter.Convert(DefaultSampleFrequency);
    ReleaseObject(Converter);
    ReleaseObject(Temp);
  End;

  SetStatus(rsReady);
End;

Procedure TERRASound.SamplesFromStream(Samples, Frequency:Cardinal; Stereo:Boolean; Source:TERRAStream);
Var
  Temp:Array Of AudioSample;
  Len:Integer;
Begin
  Len := Samples;
  If Stereo Then
    Len := Len * 2;

  SetLength(Temp, Len);
  Source.Read(@Temp[0], Len * SizeOf(AudioSample));

  Self.SamplesFromBuffer(Samples, Frequency, Stereo, @Temp[0]);

  SetLength(Temp, 0);
End;

Function TERRASound.Load(Source:TERRAStream):Boolean;
Var
  Format:TERRAFileFormat;
Begin
  Format := Engine.Formats.FindFormatFromStream(Source, TERRASound);
  If Not Assigned(Format) Then
  Begin
    Result := False;

    Engine.Log.Write(logError, 'Sound', 'Unknown sound format. ['+Source.Name+']');
    SetStatus(rsInvalid);

    Exit;
  End;

  Engine.Log.Write(logDebug, 'Sound', 'Calling sound loader...');

  Result := Format.LoadFromStream(Self, Source);
  SetStatus(rsReady);
End;

Class Function TERRASound.GetManager:TERRAObject;
Begin
  Result := Engine.Audio;
End;


End.
