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
 * TERRA_SoundSource
 * Implements a positional sound source
 ***********************************************************************************************************************
}
Unit TERRA_SoundSource;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_Sound,
  TERRA_OS, TERRA_Resource, TERRA_AudioBuffer, TERRA_AudioPanning, TERRA_SoundAmbience;

Type
  SoundSource = Class;

  SoundSourceCallback = Procedure(MySource:SoundSource) Of Object;

  SoundSourceMode = (
    soundSource_Static,
    soundSource_Dynamic
  );

  SoundSourceStatus = (
    soundSource_Playing,
    soundSource_Finished,
    soundSource_Paused
  );

  SoundSource = Class(TERRAObject)
    Protected
      _Mode:SoundSourceMode;
      _Status:SoundSourceStatus;

      _Buffer:TERRAAudioBuffer;
      _CurrentSample:Cardinal;

      _Pitch:Single;
      _Loop:Boolean;

      _Gain:Single;
      _Volume:MixingAudioSample;

      _Position:Vector3D;

      _Ambience:SoundAmbience;

      _Callback:SoundSourceCallback;

      Procedure SetPitch(Const Value:Single);
      Procedure SetVolume(Const Value:Single);
      Procedure SetLoop(Value:Boolean);
      Procedure SetPosition(Const Position:Vector3D);

      Procedure RequestMoreSamples(); Virtual;

      Procedure CalculatePositionalVolume();

    Public
      Constructor Create();
      Procedure Release; Override;

      Procedure SetCallback(Callback:SoundSourceCallback);

      Procedure RenderSamples(Dest:TERRAAudioMixingBuffer); Virtual;

      Property Status:SoundSourceStatus Read _Status;

      Property Pitch:Single Read _Pitch Write SetPitch;
      Property Volume:Single Read _Gain Write SetVolume;
      Property Loop:Boolean Read _Loop Write SetLoop;

      Property Position:Vector3D Read _Position Write SetPosition;

      Property Ambience:SoundAmbience Read _Ambience Write _Ambience;
  End;

  ResourceSoundSource = Class(SoundSource)
    Protected
      _Sound:TERRASound;

    Public
      Constructor Create(Sound:TERRASound);

      Procedure RenderSamples(Dest:TERRAAudioMixingBuffer); Override;

      Property Sound:TERRASound Read _Sound;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_SoundManager, TERRA_Engine, TERRA_Log;

{ SoundSource }
Constructor SoundSource.Create();
Begin
  _Gain := 1.0;
  _Pitch := 1.0;

  _Position := Vector3D_Zero; //GraphicsManager.Instance().MainViewport.Camera.Position;
  _Loop := False;

  _Status := soundSource_Finished;
  _Mode := soundSource_Static;

  Self.SetVolume(0.2);
End;

Procedure SoundSource.SetPitch(Const Value: Single);
Begin
  If (Value=_Pitch) Then
    Exit;

  _Pitch := Value;
End;

Procedure SoundSource.SetVolume(Const Value:Single);
Begin
  If (_Gain = Value) Then
    Exit;

  _Gain := Value;
End;

Procedure SoundSource.SetLoop(Value:Boolean);
Begin
  _Loop := Value;
End;

Procedure SoundSource.SetPosition(Const Position:Vector3D);
Begin
  _Position := Position;

  _Mode := soundSource_Dynamic;
End;


Procedure SoundSource.SetCallback(Callback: SoundSourceCallback);
Begin
  _Callback := Callback;
End;

Procedure SoundSource.Release;
Begin
  If Assigned(_Callback) Then
  Begin
    _Callback(Self);
    _Callback := Nil;
  End;
End;

Procedure SoundSource.RenderSamples(Dest:TERRAAudioMixingBuffer);
Var
  SampleCount, CopyTotal, Temp, Leftovers:Integer;
  FreqRate:Single;
Begin
  CalculatePositionalVolume();

  FreqRate := _Buffer.Frequency / Dest.Frequency;

  SampleCount := Dest.SampleCount;
  If (Trunc(_CurrentSample + SampleCount * FreqRate) > _Buffer.SampleCount) Then
  Begin
    Temp := SampleCount;
    SampleCount := Trunc((_Buffer.SampleCount - _CurrentSample) / FreqRate);
    LeftOvers := Temp - SampleCount;
  End Else
    Leftovers := 0;

  CopyTotal := Dest.MixSamples(0, _Buffer, _CurrentSample, SampleCount, _Volume);
  Inc(_CurrentSample, CopyTotal);

  If (Leftovers>0) Then
  Begin
    RequestMoreSamples();

    If (_Status = soundSource_Playing) Then
    Begin
      CopyTotal := Dest.MixSamples(SampleCount, _Buffer, _CurrentSample, Leftovers, _Volume);
      Inc(_CurrentSample, CopyTotal);
    End;
  End;

  If (_CurrentSample>=_Buffer.SampleCount)  Then
    RequestMoreSamples();
End;

Procedure SoundSource.RequestMoreSamples();
Begin
  If (Self._Loop) Then
    _CurrentSample := 0
  Else
    _Status := soundSource_Finished;
End;

Procedure SoundSource.CalculatePositionalVolume;
Begin
  If (_Mode = soundSource_Static) Then
  Begin
    _Volume.Left := _Gain;
    _Volume.Right := _Gain;
    Exit;
  End;

  ComputeDirectionalGains(_Position, Self._Gain, Self._Volume);

(*  If (_Position.X >= 0.0) And (_Position.X<=1.0) Then
  Begin
    _VolumeLeft := (1.0 - _Position.X);
    _VolumeRight := _Position.X;
  End Else
  If (_Position.X<0.0) Then
  Begin
    If _Position.X >-1 Then
      _VolumeLeft := (1.0 + _Position.X)
    Else
      _VolumeLeft := 0;

    _VolumeRight := 0.0;
  End Else
  If (_Position.X<0.0) Then
  Begin
    If _Position.X <2 Then
      _VolumeRight := 1.0 - (_Position.X - 1.0)
    Else
      _VolumeRight := 0;

    _VolumeLeft := 0.0;
  End;*)

End;

{ ResourceSoundSource }
Constructor ResourceSoundSource.Create(Sound:TERRASound);
Begin
  Inherited Create();

  If (Not Assigned(Sound)) Then
    Exit;

  Engine.Log.Write(logDebug, 'SoundSource', 'Binding '+ Sound.Name);

  _Sound := Sound;
  _Status := soundSource_Playing;
End;

Procedure ResourceSoundSource.RenderSamples(Dest:TERRAAudioMixingBuffer);
Begin
  If (Self._Sound = Nil) Or (Self._Sound.Status = rsInvalid) Then
  Begin
    Self._Status := soundSource_Finished;
    Exit;
  End;

  If (Not Self._Sound.IsReady()) Then
    Exit;

  Self._Buffer := _Sound.Buffer;
  Inherited RenderSamples(Dest);
End;


End.
