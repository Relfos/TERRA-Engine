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
 * TERRA_SoundAmbience
 * Implements support for 3D sound ambience reverb effects
 ***********************************************************************************************************************
}

Unit TERRA_SoundAmbience;
{$I terra.inc}

Interface

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Stream, TERRA_Vector3D, TERRA_AudioReverb;

Type
  SoundAmbience = Class(TERRAObject)
    Protected
      _Reverb:AudioReverbEffect;

	    _Density:FloatProperty;
	    _Diffusion:FloatProperty;
	    _Gain:FloatProperty;
	    _GainHF:FloatProperty;
	    _GainLF:FloatProperty;
	    _DecayTime:FloatProperty;
	    _DecayHFRatio:FloatProperty;
	    _DecayLFRatio:FloatProperty;
	    _ReflectionsGain:FloatProperty;
	    _ReflectionsDelay:FloatProperty;
	    _LateReverbGain:FloatProperty;
	    _LateReverbDelay:FloatProperty;
	    _EchoTime:FloatProperty;
	    _EchoDepth:FloatProperty;
	    _ModulationTime:FloatProperty;
	    _ModulationDepth:FloatProperty;
	    _AirAbsorptionGainHF:FloatProperty;
	    _HFReference:FloatProperty;
	    _LFReference:FloatProperty;
	    _RoomRolloffFactor:FloatProperty;
	    _DecayHFLimit:BooleanProperty;

	    _ReflectionsPan: Vector3D;
	    _LateReverbPan: Vector3D;

      Function GetAirAbsorptionGainHF: Single;
      Function GetDecayHFLimit: Boolean;
      Function GetDecayHFRatio: Single;
      Function GetDecayLFRatio: Single;
      Function GetDecayTime: Single;
      Function GetDensity: Single;
      Function GetDiffusion: Single;
      Function GetEchoDepth: Single;
      Function GetEchoTime: Single;
      Function GetGain: Single;
      Function GetGainHF: Single;
      Function GetGainLF: Single;
      Function GetHFReference: Single;
      Function GetLateReverbDelay: Single;
      Function GetLateReverbGain: Single;
      Function GetLateReverbPan: Vector3D;
      Function GetLFReference: Single;
      Function GetModulationDepth: Single;
      Function GetModulationTime: Single;
      Function GetReflectionsDelay: Single;
      Function GetReflectionsGain: Single;
      Function GetReflectionsPan: Vector3D;
      Function GetRoomRolloffFactor: Single;

      Procedure SetAirAbsorptionGainHF(const Value:Single);
      Procedure SetDecayHFLimit(const Value:Boolean);
      Procedure SetDecayHFRatio(const Value:Single);
      Procedure SetDecayLFRatio(const Value:Single);
      Procedure SetDecayTime(const Value:Single);
      Procedure SetDensity(const Value:Single);
      Procedure SetDiffusion(const Value:Single);
      Procedure SetEchoDepth(const Value:Single);
      Procedure SetEchoTime(const Value:Single);
      Procedure SetGain(const Value:Single);
      Procedure SetGainHF(const Value:Single);
      Procedure SetGainLF(const Value:Single);
      Procedure SetHFReference(const Value:Single);
      Procedure SetLateReverbDelay(const Value:Single);
      Procedure SetLateReverbGain(const Value:Single);
      Procedure SetLateReverbPan(const Value:Vector3D);
      Procedure SetLFReference(const Value:Single);
      Procedure SetModulationDepth(const Value:Single);
      Procedure SetModulationTime(const Value:Single);
      Procedure SetReflectionsDelay(const Value:Single);
      Procedure SetReflectionsGain(const Value:Single);
      Procedure SetReflectionsPan(const Value:Vector3D);
      Procedure SetRoomRolloffFactor(const Value:Single);

      Procedure UpdateReverb();

    Public
      Constructor Create(Const Name:TERRAString);
      Procedure Release(); Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Class Function GetObjectType:TERRAString; Override;

      Property Density:Single Read GetDensity Write SetDensity;
      Property Diffusion:Single Read GetDiffusion Write SetDiffusion;

      Property Gain:Single Read GetGain Write SetGain;
      Property GainHF:Single Read GetGainHF Write SetGainHF;
      Property GainLF:Single Read GetGainLF Write SetGainLF;

      Property DecayTime:Single Read GetDecayTime Write SetDecayTime;
      Property DecayHFRatio:Single Read GetDecayHFRatio Write SetDecayHFRatio;
      Property DecayLFRatio:Single Read GetDecayLFRatio Write SetDecayLFRatio;

      Property ReflectionsGain:Single Read GetReflectionsGain Write SetReflectionsGain;
      Property ReflectionsDelay:Single Read GetReflectionsDelay Write SetReflectionsDelay;
      Property ReflectionsPan:Vector3D Read GetReflectionsPan Write SetReflectionsPan;

      Property LateReverbGain:Single Read GetLateReverbGain Write SetLateReverbGain;
      Property LateReverbPan:Vector3D Read GetLateReverbPan Write SetLateReverbPan;
      Property LateReverbDelay:Single Read GetLateReverbDelay Write SetLateReverbDelay;

      Property AirAbsorptionGainHF:Single Read GetAirAbsorptionGainHF Write SetAirAbsorptionGainHF;

      Property EchoTime:Single Read GetEchoTime Write SetEchoTime;
      Property EchoDepth:Single Read GetEchoDepth Write SetEchoDepth;

      Property ModulationTime:Single Read GetModulationTime Write SetModulationTime;
      Property ModulationDepth:Single Read GetModulationDepth Write SetModulationDepth;

      Property HFReference:Single Read GetHFReference Write SetHFReference;
      Property LFReference:Single Read GetLFReference Write SetLFReference;

      Property RoomRolloffFactor:Single Read GetRoomRolloffFactor Write SetRoomRolloffFactor;
      Property DecayHFLimit:Boolean Read GetDecayHFLimit Write SetDecayHFLimit;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_FileManager, TERRA_EngineManager, TERRA_AudioMixer;

{ SoundAmbience }
Constructor SoundAmbience.Create(Const Name:TERRAString);
Begin
  Self._ObjectName := Name;

  _Density := FloatProperty.Create('Density', EAXREVERB_DEFAULT_DENSITY);
	_Diffusion := FloatProperty.Create('Diffusion', EAXREVERB_DEFAULT_DIFFUSION);
	_Gain := FloatProperty.Create('Gain', EAXREVERB_DEFAULT_GAIN);
	_GainHF := FloatProperty.Create('GainHF', EAXREVERB_DEFAULT_GainHF);
	_GainLF := FloatProperty.Create('GainLF', EAXREVERB_DEFAULT_GainLF);
	_DecayTime := FloatProperty.Create('DecayTime', EAXREVERB_DEFAULT_DECAY_TIME);
  _DecayHFRatio := FloatProperty.Create('DecayHFRatio', EAXREVERB_DEFAULT_DECAY_HFRATIO);
	_DecayLFRatio := FloatProperty.Create('DecayLFRatio', EAXREVERB_DEFAULT_DECAY_LFRATIO);
	_ReflectionsGain := FloatProperty.Create('ReflectionsGain', EAXREVERB_DEFAULT_REFLECTIONS_GAIN);
	_ReflectionsDelay := FloatProperty.Create('ReflectionsDelay', EAXREVERB_DEFAULT_REFLECTIONS_DELAY);
//	_ReflectionsPan :=  Vector3DProperty.Create('ReflectionsPan', Vector3D_Zero);
	_LateReverbGain := FloatProperty.Create('LateReverbGain', EAXREVERB_DEFAULT_LATE_REVERB_GAIN);
	_LateReverbDelay := FloatProperty.Create('LateReverbDelay', EAXREVERB_DEFAULT_LATE_REVERB_DELAY);
//	_LateReverbPan :=  Vector3DProperty.Create('LateReverbPan', Vector3D_Zero);
	_EchoTime := FloatProperty.Create('EchoTime', EAXREVERB_DEFAULT_ECHO_TIME);
	_EchoDepth := FloatProperty.Create('EchoDepth', EAXREVERB_DEFAULT_ECHO_DEPTH);
	_ModulationTime := FloatProperty.Create('ModulationTime', EAXREVERB_DEFAULT_MODULATION_TIME);
	_ModulationDepth := FloatProperty.Create('ModulationDepth', EAXREVERB_DEFAULT_MODULATION_DEPTH);
	_AirAbsorptionGainHF := FloatProperty.Create('AirAbsorptionGainHF', EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF);
	_HFReference := FloatProperty.Create('HFReference', EAXREVERB_DEFAULT_HFReference);
	_LFReference := FloatProperty.Create('LFReference', EAXREVERB_DEFAULT_LFReference);
	_RoomRolloffFactor := FloatProperty.Create('RoomRolloffFactor', EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR);
	_DecayHFLimit := BooleanProperty.Create('DecayHFLimit', EAXREVERB_DEFAULT_DECAY_HFLIMIT);

	(*Density := 1.0;
	Diffusion := 0.20999999344;
	Gain := 0.31622776389;
	GainHF := 0.10000000149;
	GainLF := 1.0;
	DecayTime := 1.4900000095;
	DecayHFRatio := 0.5;
	DecayLFRatio := 1.0;
	ReflectionsGain := 0.058479003608;
	ReflectionsDelay := 0.17900000513;
	ReflectionsPan := Vector3D_Zero;
	LateReverbGain := 0.10889300704;
	LateReverbDelay := 0.10000000149;
	LateReverbPan := Vector3D_Zero;
	EchoTime := 0.25;
	EchoDepth := 1.0;
	ModulationTime := 0.25;
	ModulationDepth := 0.0;
	AirAbsorptionGainHF := 0.99426007271;
	HFReference := 5000;
	LFReference := 250;
	RoomRolloffFactor := 0.0;
	DecayHFLimit := False;*)

  _Reverb := AudioReverbEffect.Create(DefaultSampleFrequency);
  Self.UpdateReverb();
End;

Function SoundAmbience.GetAirAbsorptionGainHF: Single;
Begin
  Result := _AirAbsorptionGainHF.Value;
End;

Function SoundAmbience.GetDecayHFLimit: Boolean;
Begin
  Result := _DecayHFLimit.Value;
End;

Function SoundAmbience.GetDecayHFRatio: Single;
Begin
  Result := _DecayHFRatio.Value;
End;

Function SoundAmbience.GetDecayLFRatio: Single;
Begin
  Result := _DecayLFRatio.Value;
End;

Function SoundAmbience.GetDecayTime: Single;
Begin
  Result := _DecayTime.Value;
End;

Function SoundAmbience.GetDensity: Single;
Begin
  Result := _Density.Value;
End;

Function SoundAmbience.GetDiffusion: Single;
Begin
  Result := _Diffusion.Value;
End;

Function SoundAmbience.GetEchoDepth: Single;
Begin
  Result := _EchoDepth.Value;
End;

Function SoundAmbience.GetEchoTime: Single;
Begin
  Result := _EchoTime.Value;
End;

Function SoundAmbience.GetGain: Single;
Begin
  Result := _Gain.Value;
End;

Function SoundAmbience.GetGainHF: Single;
Begin
  Result := _GainHF.Value;
End;

Function SoundAmbience.GetGainLF: Single;
Begin
  Result := _GainLF.Value;
End;

Function SoundAmbience.GetHFReference: Single;
Begin
  Result := _HFReference.Value;
End;

Function SoundAmbience.GetLateReverbDelay: Single;
Begin
  Result := _LateReverbDelay.Value;
End;

Function SoundAmbience.GetLateReverbGain: Single;
Begin
  Result := _LateReverbGain.Value;
End;

Function SoundAmbience.GetLateReverbPan: Vector3D;
Begin
  Result := _LateReverbPan{.Value};
End;

Function SoundAmbience.GetLFReference: Single;
Begin
  Result := _LFReference.Value;
End;

Function SoundAmbience.GetModulationDepth: Single;
Begin
  Result := _ModulationDepth.Value;
End;

Function SoundAmbience.GetModulationTime: Single;
Begin
  Result := _ModulationTime.Value;
End;

Function SoundAmbience.GetReflectionsDelay: Single;
Begin
  Result := _ReflectionsDelay.Value;
End;

Function SoundAmbience.GetReflectionsGain: Single;
Begin
  Result := _ReflectionsGain.Value;
End;

Function SoundAmbience.GetReflectionsPan: Vector3D;
Begin
  Result := _ReflectionsPan{.Value};
End;

Function SoundAmbience.GetRoomRolloffFactor: Single;
Begin
  Result := _RoomRolloffFactor.Value;
End;

Procedure SoundAmbience.SetAirAbsorptionGainHF(const Value: Single);
Begin
  _AirAbsorptionGainHF.Value := Value;
End;

Procedure SoundAmbience.SetDecayHFLimit(const Value: Boolean);
Begin
  _DecayHFLimit.Value := Value;
End;

Procedure SoundAmbience.SetDecayHFRatio(const Value: Single);
Begin
  _DecayHFRatio.Value := Value;
End;

Procedure SoundAmbience.SetDecayLFRatio(const Value: Single);
Begin
  _DecayLFRatio.Value := Value;
End;

Procedure SoundAmbience.SetDecayTime(const Value: Single);
Begin
  _DecayTime.Value := Value;
End;

Procedure SoundAmbience.SetDensity(const Value: Single);
Begin
  _Density.Value := Value;
End;

Procedure SoundAmbience.SetDiffusion(const Value: Single);
Begin
  _Diffusion.Value := Value;
End;

Procedure SoundAmbience.SetEchoDepth(const Value: Single);
Begin
  _EchoDepth.Value := Value;
End;

Procedure SoundAmbience.SetEchoTime(const Value: Single);
Begin
  _EchoTime.Value := Value;
End;

Procedure SoundAmbience.SetGain(const Value: Single);
Begin
  _Gain.Value := Value;
End;

Procedure SoundAmbience.SetGainHF(const Value: Single);
Begin
  _GainHF.Value := Value;
End;

Procedure SoundAmbience.SetGainLF(const Value: Single);
Begin
  _GainLF.Value := Value;
End;

Procedure SoundAmbience.SetHFReference(const Value: Single);
Begin
  _HFReference.Value := Value;
End;

Procedure SoundAmbience.SetLateReverbDelay(const Value: Single);
Begin
  _LateReverbDelay.Value := Value;
End;

Procedure SoundAmbience.SetLateReverbGain(const Value: Single);
Begin
  _LateReverbGain.Value := Value;
End;

Procedure SoundAmbience.SetLateReverbPan(const Value: Vector3D);
Begin
  _LateReverbPan{.Value} := Value;
End;

Procedure SoundAmbience.SetLFReference(const Value: Single);
Begin
  _LFReference.Value := Value;
End;

Procedure SoundAmbience.SetModulationDepth(const Value: Single);
Begin
  _ModulationDepth.Value := Value;
End;

Procedure SoundAmbience.SetModulationTime(const Value: Single);
Begin
  _ModulationTime.Value := Value;
End;

Procedure SoundAmbience.SetReflectionsDelay(const Value: Single);
Begin
  _ReflectionsDelay.Value := Value;
End;

Procedure SoundAmbience.SetReflectionsGain(const Value: Single);
Begin
  _ReflectionsGain.Value := Value;
End;

Procedure SoundAmbience.SetReflectionsPan(const Value: Vector3D);
Begin
  _ReflectionsPan{.Value} := Value;
End;

Procedure SoundAmbience.SetRoomRolloffFactor(const Value: Single);
Begin
  _RoomRolloffFactor.Value := Value;
End;

Class Function SoundAmbience.GetObjectType: TERRAString;
Begin
  Result := 'ambience';
End;

Function SoundAmbience.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
    0: Result := _Density;
    1: Result := _Diffusion;
    2: Result := _Gain;
    3: Result := _GainHF;
    4: Result := _GainLF;
    5: Result := _DecayTime;
    6: Result := _DecayHFRatio;
    7: Result := _DecayLFRatio;
    8: Result := _ReflectionsGain;
    9: Result := _ReflectionsDelay;
    //10: Result := _ReflectionsPan;
    10: Result := _LateReverbGain;
    11: Result := _LateReverbDelay;
    //13: Result := _LateReverbPan;
    12: Result := _EchoTime;
    13: Result := _EchoDepth;
    14: Result := _ModulationTime;
    15: Result := _ModulationDepth;
    16: Result := _AirAbsorptionGainHF;
    17: Result := _HFReference;
    18: Result := _LFReference;
    19: Result := _RoomRolloffFactor;
    20: Result := _DecayHFLimit;
    Else
      Result := Nil;
  End;
End;

Procedure SoundAmbience.Release;
Begin
  ReleaseObject(_Reverb);
End;

Procedure SoundAmbience.UpdateReverb;
Begin
  _Reverb.Density := Self.Density;
  _Reverb.Diffusion := Self.Diffusion;
  _Reverb.Gain := Self.Gain;
  _Reverb.GainHF := Self.GainHF;
  _Reverb.GainLF := Self.GainLF;
  _Reverb.GainHF := Self.DecayTime;
  _Reverb.DecayHFRatio := Self.DecayHFRatio;
  _Reverb.DecayLFRatio := Self.DecayLFRatio;
  _Reverb.ReflectionsGain := Self.ReflectionsGain;
  _Reverb.ReflectionsDelay := Self.ReflectionsDelay;
  _Reverb.ReflectionsPan := Self.ReflectionsPan;
  _Reverb.LateReverbGain := Self.LateReverbGain;
  _Reverb.LateReverbDelay := Self.LateReverbDelay;
  _Reverb.LateReverbPan := Self.LateReverbPan;
  _Reverb.EchoTime := Self.EchoTime;
  _Reverb.EchoDepth := Self.EchoDepth;
  _Reverb.ModulationTime := Self.ModulationTime;
  _Reverb.ModulationDepth := Self.ModulationDepth;
  _Reverb.AirAbsorptionGainHF := Self.AirAbsorptionGainHF;
  _Reverb.HFReference := Self.HFReference;
  _Reverb.LFReference := Self.LFReference;
  _Reverb.RoomRolloffFactor := Self.RoomRolloffFactor;
  _Reverb.DecayHFLimit := Self.DecayHFLimit;
End;

End.