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
 * TERRA_SoundSource
 * Implements a 3D sound source
 ***********************************************************************************************************************
}
Unit TERRA_SoundSource;

{$I terra.inc}

{$IFDEF ANDROID}
{$DEFINE USEJAVA}
{$ENDIF}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_Sound,
  TERRA_OS,TERRA_AL
  {$IFDEF USEJAVA},TERRA_Java{$ENDIF};

{$IFDEF USEJAVA}
Const
  AudioTrackJavaClass = 'com.pascal.terra.TERRAAudioTrack';
{$ENDIF}

Type
  SoundSource = Class;

  SoundSourceCallback = Procedure(MySource:SoundSource; UserData:Pointer); Cdecl;

  SoundSource = Class(TERRAObject)
    Protected
      _Handle:Integer;
      _Sound:Sound;
      _StartTime:Cardinal;

      _Pitch:Single;
      _Volume:Single;
      _Loop:Boolean;

      _Position:Vector3D;
      _Velocity:Vector3D;

      _Waiting:Boolean;

      {$IFDEF USEJAVA}
      _Track:JavaObject;
      {$ENDIF}

      _Callback:SoundSourceCallback;
      _UserData:Pointer;

      Function GetStatus:Integer;

      Procedure SetPitch(Value:Single);
      Procedure SetVolume(Value:Single);
      Procedure SetLoop(Value:Boolean);
      Procedure SetPosition(Position:Vector3D);
      Procedure SetVelocity(Velocity:Vector3D);

      Procedure UpdateSettings;
      Procedure Clear;

    Public
      Constructor Create;
      Procedure Release; Override;

      Procedure Bind(MySound:Sound);

      Procedure Start;
      Procedure Play;
      Procedure Pause;
      Procedure Stop;

      Procedure OnFinish();

      Procedure SetCallback(Callback:SoundSourceCallback; UserData:Pointer = Nil);

      Property Handle:Integer Read _Handle;
      Property Sound:TERRA_Sound.Sound Read _Sound;
      Property Status:Integer Read GetStatus;
      Property StartTime:Cardinal Read _StartTime;

      Property Pitch:Single Read _Pitch Write SetPitch;
      Property Volume:Single Read _Volume Write SetVolume;
      Property Loop:Boolean Read _Loop Write SetLoop;

      Property Waiting:Boolean Read _Waiting;

      Property Position:Vector3D Read _Position Write SetPosition;
      Property Velocity:Vector3D Read _Velocity Write SetVelocity;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_SoundManager, TERRA_Log;

// SoundSource
Constructor SoundSource.Create;
Begin
  _Handle := 0;
  _Sound := Nil;

  _Volume := 1.0;
  _Pitch := 1.0;

  _Position := VectorZero; //GraphicsManager.Instance().MainViewport.Camera.Position;
  _Velocity := VectorZero;
  _Loop := False;

  {$IFDEF USEJAVA}
  _Track := Nil;
  {$ENDIF}
End;

Procedure SoundSource.Release;
Begin
  Clear;
End;

Procedure SoundSource.Clear;
{$IFDEF USEJAVA}
Var
  Frame:JavaFrame;
{$ENDIF}
Begin
  {$IFDEF DISABLESOUND}
  Exit;
  {$ENDIF}

  Stop;

  If Assigned(_Sound) Then
    _Sound.RemoveSource(Self);

  {$IFDEF USEJAVA}
  If (_Track<>Nil) Then
  Begin
    Java_Begin(Frame);
    _Track.CallVoidMethod(Frame, 'release', Nil);
    ReleaseObject(_Track);
    _Track := Nil;
    Java_End(Frame);
  End;
  {$ELSE}
  If (_Handle<>0) Then
  Begin
    alDeleteSources(1,@_Handle);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    _Handle:=0;
  End;
  {$ENDIF}
End;

Procedure SoundSource.UpdateSettings;
{$IFDEF USEJAVA}
Var
  Params:JavaArguments;
  Frame:JavaFrame;
{$ENDIF}
Begin
  Exit;
  {$IFDEF USEJAVA}
  If _Track<>Nil Then
  Begin
    Log(logDebug, 'SoundSource', 'Setting volume');

    Java_Begin(Frame);

    Params := JavaArguments.Create(Frame);
    Params.AddFloat(_Volume);
    _Track.CallVoidMethod(Frame, 'setVolume', Params);
    ReleaseObject(Params);

    Log(logDebug, 'SoundSource', 'Setting loop');
    Params := JavaArguments.Create(Frame);
    Params.AddBoolean(_Loop);
    _Track.CallVoidMethod(Frame, 'setLoop', Params);
    ReleaseObject(Params);

    Java_End(Frame);
  End;

  {$ELSE}
  If (_Handle<=0) Then
    Exit;

  //alSourcef(_Handle, AL_PITCH, _Pitch);         {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  If (Self._Sound.Channels<=1) Then
  Begin
    alSourcef(_Handle, AL_GAIN, _Volume);         {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alSourcefv(_Handle, AL_POSITION, @_Position); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alSourcefv(_Handle, AL_VELOCITY, @_Velocity); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alSourcef(_Handle, AL_REFERENCE_DISTANCE, 30.0);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alSourcef(_Handle, AL_MAX_DISTANCE, 5000.0);
    alSourcef(_Handle, AL_ROLLOFF_FACTOR, 2.0);
  End;
  alSourcei(_Handle, AL_LOOPING, Integer(_Loop));
  {$ENDIF}
End;

Procedure SoundSource.SetPitch(Value: Single);
Begin
  If (Value=_Pitch) Then
    Exit;

  _Pitch := Value;
  UpdateSettings;
End;

Procedure SoundSource.SetVolume(Value:Single);
Begin
  If (_Volume=Value) Then
    Exit;

  _Volume := Value;
  UpdateSettings;
End;

Procedure SoundSource.SetLoop(Value:Boolean);
Begin
  _Loop := Value;
  UpdateSettings;
End;

Procedure SoundSource.SetPosition(Position: Vector3D);
Begin
  _Position := Position;
  UpdateSettings;
End;

Procedure SoundSource.SetVelocity(Velocity: Vector3D);
Begin
  _Velocity := Velocity;
  UpdateSettings;
End;

Procedure SoundSource.Bind(MySound:Sound);
Begin
  If (_Sound=MySound) Or (Not Assigned(MySound)) Then
    Exit;

  Log(logDebug, 'SoundSource', 'Binding '+MySound.Name);

  If Assigned(_Sound) Then
    Clear;

  _Sound := MySound;
  _Sound.AttachSource(Self);
  _Waiting := True;
End;

Procedure SoundSource.Start;
{$IFDEF USEJAVA}
Var
  Params:JavaArguments;
  Samples:Integer;
  Frame:JavaFrame;
{$ENDIF}
Begin
  _Waiting := False;

  Log(logDebug, 'SoundSource', 'Updating '+ Sound.Name);
  Sound.Update;

  {$IFDEF USEJAVA}
  Samples := (Sound.Size Div Sound.SampleSize);

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Sound.Channels);
  Params.AddInteger(Sound.Frequency);
  Params.AddInteger(Sound.Size);
  Params.AddWordArray(Sound.Data, Samples);
  _Track := JavaObject.Create(AudioTrackJavaClass, Params, Frame);
  ReleaseObject(Params);
  Java_End(Frame);

  Log(logDebug, 'SoundSource', 'Ready!');
  {$ELSE}
  If (_Handle=0) Then
  Begin
    alGenSources(1, @_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  End;

  alSourcei(_Handle, AL_BUFFER, Sound.Buffer);
  {$ENDIF}

  UpdateSettings;

  Self.Play;
End;

Procedure SoundSource.Play;
Var
  I:Integer;
{$IFDEF USEJAVA}
  Frame:JavaFrame;
{$ENDIF}
Begin
  //If (Status<>sndPlaying) Then
  Begin
    _StartTime := Application.GetTime;

    {$IFDEF USEJAVA}
    If (_Track<>Nil) Then
    Begin
      Log(logDebug, 'SoundSource', 'Playing track');

      Java_Begin(Frame);
      _Track.CallVoidMethod(Frame, 'play', Nil);
      Java_End(Frame);
    End;
    {$ELSE}
    If (Assigned(SoundManager.Instance().Ambience)) Then
      alSource3i(_Handle, AL_AUXILIARY_SEND_FILTER, SoundManager.Instance().Ambience.Handle, 0, AL_FILTER_NULL);

    alSourcei(_Handle, AL_LOOPING, Integer(_Loop)); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    alSourcePlay(_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    {$ENDIF}
  End;
End;

Procedure SoundSource.Pause;
Begin
  If Status=sndPlaying Then
  Begin
    {$IFDEF USEJAVA}
    Self.Stop();
    {$ELSE}
    alSourcePause(_Handle);
    {$ENDIF}
  End Else
    Play;
End;

Procedure SoundSource.Stop;
{$IFDEF USEJAVA}
Var
  Frame:JavaFrame;
{$ENDIF}
Begin
  If (Status<>sndStopped) Then
  Begin
    {$IFDEF USEJAVA}
    If _Track<>Nil Then
    Begin
      Log(logDebug, 'SoundSource', 'Stopping track');

      Java_Begin(Frame);
      _Track.CallVoidMethod(Frame, 'stop', Nil);

      ReleaseObject(_Track);
      _Track := Nil;
      Java_End(Frame);
    End;

    {$ELSE}
    alSourceStop(_Handle);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    If (Assigned(SoundManager.Instance().Ambience)) Then
    Begin
      alSource3i(_Handle, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, AL_FILTER_NULL);
      {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    End;
    {$ENDIF}
  End;
End;

Function SoundSource.GetStatus:Integer;
Var
  State:Integer;
{$IFDEF USEJAVA}
  Frame:JavaFrame;
{$ENDIF}
Begin
  {$IFDEF USEJAVA}
  If _Track<>Nil Then
  Begin
    Java_Begin(Frame);
    Result := _Track.CallIntMethod(Frame, 'getState', Nil);
    Java_End(Frame);
    //Log(logDebug, 'Sound', 'Result='+IntToString(Result));
  End Else
    Result := sndStopped;
  {$ELSE}
  alGetSourcei(_Handle, AL_SOURCE_STATE, @State);
  Case State Of
  AL_PLAYING: Result := sndPlaying;
  AL_PAUSED: Result := sndPaused;
  Else
    Result := sndStopped;
  End;
  {$ENDIF}
End;

Procedure SoundSource.SetCallback(Callback: SoundSourceCallback; UserData: Pointer);
Begin
  _Callback := Callback;
  _UserData := Userdata;
End;

Procedure SoundSource.OnFinish;
Begin
  If Assigned(_Callback) Then
  Begin
    _Callback(Self, _UserData);
    _Callback := Nil;
  End;
End;

End.
