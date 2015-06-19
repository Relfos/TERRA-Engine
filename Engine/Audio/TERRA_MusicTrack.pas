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
 * TERRA_MusicTrack
 * Implements a generic music track, which can be inherited from.
 ***********************************************************************************************************************
}

Unit TERRA_MusicTrack;
{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_AL, TERRA_OGG, TERRA_SoundStreamer;

Type
  MusicTrackClass = Class Of MusicTrack;
  
  MusicTrack = Class(TERRAObject)
    Protected
      _FileName:TERRAString;
      _Volume:Single;

      Procedure ChangeVolume(Volume:Single); Virtual; Abstract;

    Public
      Constructor Create(FileName:TERRAString; Volume:Single);
      Procedure Release; Override;

      Procedure Init(); Virtual; Abstract;
      Procedure Play(); Virtual; Abstract;
      Procedure Update; Virtual;
      Procedure Stop; Virtual; Abstract;

      Procedure SetVolume(Volume:Single);

      Class Function Supports(Const Extension:TERRAString):Boolean; Virtual;

      Property FileName:TERRAString Read _FileName;
      Property Volume:Single Read _Volume Write SetVolume;
  End;

  StreamingMusicTrack = Class(MusicTrack)
    Protected
      _Stream:SoundStream;

      Procedure ChangeVolume(Volume:Single); Override;

    Public
      Procedure Release; Override;

      Procedure Init(); Override;
      Procedure Play(); Override;
      Procedure Update; Override;
      Procedure Stop; Override;

      Class Function Supports(Const Extension:TERRAString):Boolean; Override;
  End;

Implementation
Uses TERRA_FileManager, TERRA_Stream;

{ MusicTrack }
Constructor MusicTrack.Create(FileName: TERRAString; Volume:Single);
Begin
  _FileName := FileName;
  _Volume := Volume;
End;

Procedure MusicTrack.Release;
Begin
  // do nothing
End;

Procedure MusicTrack.SetVolume(Volume: Single);
Begin
  If (Volume<0) Then
    Volume := 0
  Else
  If (Volume>1) Then
    Volume := 1;

  If (Volume = _Volume) Then
    Exit;

  _Volume := Volume;
  Self.ChangeVolume(Volume);
End;

Class Function MusicTrack.Supports(const Extension: TERRAString): Boolean;
Begin
  Result := False;
End;

Procedure MusicTrack.Update;
Begin
  // do nothing
End;

{ StreamingMusicTrack }
Procedure StreamingMusicTrack.Init;
Var
  Source:Stream;
Begin
  If (OpenALHandle=0) Then
    Exit;

  Source := FileManager.Instance().OpenStream(FileName);
  _Stream := CreateSoundStream(Source);
End;

Procedure StreamingMusicTrack.Release;
Begin
  ReleaseObject(_Stream);
End;


Procedure StreamingMusicTrack.Play();
Begin
  If Assigned(_Stream) Then
  Begin
    SetVolume(_Volume);
    _Stream.Play;
  End;
End;

Procedure StreamingMusicTrack.Update;
Begin
  If Assigned(_Stream) Then
    _Stream.Update;
End;

Procedure StreamingMusicTrack.Stop;
Begin
  If Assigned(_Stream) Then
    _Stream.Stop;
End;

Procedure StreamingMusicTrack.ChangeVolume(Volume:Single);
Begin
  If Assigned(_Stream) Then
    _Stream.Volume := _Volume;
End;


Class Function StreamingMusicTrack.Supports(Const Extension: TERRAString):Boolean;
Begin
  Result := (Extension = 'ogg');
End;

End.
