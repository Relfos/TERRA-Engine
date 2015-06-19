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
 * TERRA_MusicManager
 * Implements the global music manager.
 ***********************************************************************************************************************
}

Unit TERRA_MusicManager;
{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack;

Const
  DefaultMusicCrossFadeDuration = 6000;

Type
  MusicManager = Class(ApplicationComponent)
    Protected
      _Enabled:Boolean;

      _Mute:Boolean;
      _OldVolume:Single;
      _Volume:Single;

      _CurrentTrack:MusicTrack;
      _PreviousTrackName:TERRAString;

      _CrossFadeDuration:Integer;
      _CrossFadeTime:Cardinal;
      _CrossVolume:Single;
      _CrossFadeState:Integer;
      _CrossFadeTrack:TERRAString;

      Procedure SetEnabled(const Value: Boolean);

      Procedure InitTrack(Const SourceName:TERRAString);

    Public
      Class Function Instance:MusicManager;

      Procedure Release; Override;


      Procedure Init; Override;
      Procedure Update; Override;

      Procedure Play(SourceName:TERRAString; CrossFadeDuration:Integer = DefaultMusicCrossFadeDuration);
      Procedure Stop;

      Procedure SetVolume(Volume:Single);
      Procedure SetMute(Value:Boolean);

      Property Mute:Boolean Read _Mute Write SetMute;

      Property CurrentTrack:MusicTrack Read _CurrentTrack;
      Property Enabled:Boolean Read _Enabled Write SetEnabled;
      Property PreviousTrack:TERRAString Read _PreviousTrackName;

      Property Volume:Single Read _Volume Write SetVolume;
  End;

Implementation
Uses TERRA_FileManager, TERRA_SoundManager, TERRA_Log, TERRA_OS, TERRA_Stream, TERRA_Math
{$IFDEF HAS_MIDI}, TERRA_MIDI{$ENDIF}
{$IFDEF HAS_AUDIOTRACK}, TERRA_AudioTrack{$ENDIF};

Var
  _MusicManager_Instance:ApplicationObject;

Class Function MusicManager.Instance:MusicManager;
Begin
  If Not Assigned(_MusicManager_Instance) Then
    _MusicManager_Instance := InitializeApplicationComponent(MusicManager, {$IFDEF USE_OPENAL}SoundManager{$ELSE}Nil{$ENDIF});

  Result := MusicManager(_MusicManager_Instance.Instance);
End;

Procedure MusicManager.Init;
Begin
  SoundManager.Instance(); // load open AL

  // set initial values
  _Volume := 0.8;
  _Enabled := True;
End;

Procedure MusicManager.Play(SourceName:TERRAString; CrossFadeDuration:Integer);
Begin
  {$IFDEF DISABLEMUSIC}
  Log(logDebug, 'Music', 'Cannot play '+SourceName+', music is disabled');
  Exit;
  {$ENDIF}

  SourceName := StringLower(SourceName);

  If (Assigned(_CurrentTrack)) And (SourceName = GetFileName(_CurrentTrack.FileName, True)) Then
    Exit;

  If (SourceName = _CrossFadeTrack) Then
    Exit;

  If (Not _Enabled) Then
  Begin
    Log(logDebug, 'Music', 'Cannot play '+SourceName+', music is disabled');
    Exit;
  End;

  If (CrossFadeDuration>0) And (Assigned(_CurrentTrack)) Then
  Begin
    _CrossFadeDuration := CrossFadeDuration;
    _CrossFadeTrack := SourceName;
    _CrossVolume := _Volume;
    _CrossFadeTime := Application.GetTime;
    _CrossFadeState := 1;
  End Else
  Begin
    _CrossFadeState := 0;
    _CrossFadeTrack := '';
    InitTrack(SourceName);
  End;
End;

Procedure MusicManager.InitTrack(Const SourceName:TERRAString);
Var
  S, Ext:TERRAString;
  Procedure TryExtension(Ext:TERRAString);
  Begin
    If S<>'' Then
      Exit;

    S := FileManager.Instance.SearchResourceFile(SourceName+'.'+Ext);
  End;

  Procedure TryClass(C:MusicTrackClass);
  Begin
    If (_CurrentTrack<>Nil) Then
      Exit;

    If (C.Supports(Ext)) Then
      _CurrentTrack := C.Create(S, Self._Volume);
  End;
Begin
  If (SourceName='') Then
  Begin
    Self.Stop();
    Exit;
  End;

  If (Assigned(_CurrentTrack)) Then
  Begin
    If (SourceName = GetFileName(_CurrentTrack.FileName, True)) Then
      Exit;

    _PreviousTrackName := _CurrentTrack.FileName;

    Self.Stop();
  End Else
  Begin
    _PreviousTrackName := SourceName;
  End;

  S := '';
  TryExtension('ogg');
  TryExtension('mid');
  TryExtension('mp3');
  TryExtension('mod');

  If (S='') Then
    Exit;

  Ext := GetFileExtension(S);

  _CurrentTrack := Nil;

{$IFDEF HAS_MIDI}  
  TryClass(MidiTrack);
{$ENDIF}
  
{$IFDEF HAS_AUDIOTRACK}
  TryClass(AudioMusicTrack);
{$ENDIF}
  
  TryClass(StreamingMusicTrack);

  If _CurrentTrack=Nil Then
  Begin
    Log(logWarning, 'MusicManager', 'Cannot play track: '+SourceName);
    Exit;
  End;

  _CurrentTrack.Init();
  _CurrentTrack.Play();

  //If (_CrossFadeState = 0) Then
  _CurrentTrack.SetVolume(_Volume);
End;

Procedure MusicManager.Release;
Begin
  If (Assigned(_CurrentTrack)) Then
  Begin
    _CurrentTrack.Stop();
    ReleaseObject(_CurrentTrack);
  End;

  _MusicManager_Instance := Nil;
End;

Procedure MusicManager.SetVolume(Volume:Single);
Begin
  _Volume := Volume;
  If (_Volume<0) Then
    _Volume := 0
  Else
  If (_Volume>1) Then
    _Volume := 1;

  If Assigned(_CurrentTrack) Then
    _CurrentTrack.SetVolume(_Volume);
End;


Procedure MusicManager.Update;
Var
  Delta:Single;
Begin
  If (_CrossFadeState>0) Then
  Begin
    Delta := Application.GetTime() - _CrossFadeTime;
    Delta := Delta / _CrossFadeDuration;

    If (Delta>=1) Then
    Begin
      Delta := 1.0;
      _CrossFadeDuration := 0;
      _CrossFadeState := 0;
      _CrossFadeTrack := '';
    End Else
    If (Delta>=0.5) Then
    Begin
      If (_CrossFadeState=1) Then
      Begin
        _CrossFadeState := 2;
        Self.InitTrack(_CrossfadeTrack);
      End;
    End;

    Delta := Abs(Cos(Delta*180*RAD));
    Self.SetVolume(_CrossVolume * Delta);
  End;

  If Assigned(_CurrentTrack) Then
    _CurrentTrack.Update();
End;

Procedure MusicManager.SetMute(Value: Boolean);
Begin
  If (_Mute = Value) Then
    Exit;

  _Mute := Value;
  If (_Mute) Then
  Begin
    _OldVolume := Self._Volume;
    Self.SetVolume(0.0);
  End Else
    Self.SetVolume(Self._OldVolume);
End;

Procedure MusicManager.SetEnabled(Const Value: Boolean);
Begin
  If (Value = _Enabled) Then
    Exit;
    
  Self._Enabled := Value;

  If Not Value Then
    Self.Stop();
End;

Procedure MusicManager.Stop();
Begin
  If _CurrentTrack = Nil Then
    Exit;

  _CurrentTrack.Stop();
  ReleaseObject(_CurrentTrack);  
End;

End.