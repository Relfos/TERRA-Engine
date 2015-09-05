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
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Stream, TERRA_SoundSource, TERRA_Vector3D;

Const
  // Effect parameter ranges and defaults.
  EAXREVERB_MIN_DENSITY                 = 0.0;
  EAXREVERB_MAX_DENSITY                 = 1.0;
  EAXREVERB_DEFAULT_DENSITY             = 1.0;

  EAXREVERB_MIN_DIFFUSION               = 0.0;
  EAXREVERB_MAX_DIFFUSION               = 1.0;
  EAXREVERB_DEFAULT_DIFFUSION           = 1.0;

  EAXREVERB_MIN_GAIN                    = 0.0;
  EAXREVERB_MAX_GAIN                    = 1.0;
  EAXREVERB_DEFAULT_GAIN                = 0.32;

  EAXREVERB_MIN_GAINHF                  = 0.0;
  EAXREVERB_MAX_GAINHF                  = 1.0;
  EAXREVERB_DEFAULT_GAINHF              = 0.89;

  EAXREVERB_MIN_GAINLF                  = 0.0;
  EAXREVERB_MAX_GAINLF                  = 1.0;
  EAXREVERB_DEFAULT_GAINLF              = 1.0;

  EAXREVERB_MIN_DECAY_TIME              = 0.1;
  EAXREVERB_MAX_DECAY_TIME              = 20.0;
  EAXREVERB_DEFAULT_DECAY_TIME          = 1.49;

  EAXREVERB_MIN_DECAY_HFRATIO           = 0.1;
  EAXREVERB_MAX_DECAY_HFRATIO           = 2.0;
  EAXREVERB_DEFAULT_DECAY_HFRATIO       = 0.83;

  EAXREVERB_MIN_DECAY_LFRATIO           = 0.1;
  EAXREVERB_MAX_DECAY_LFRATIO           = 2.0;
  EAXREVERB_DEFAULT_DECAY_LFRATIO       = 1.0;

  EAXREVERB_MIN_REFLECTIONS_GAIN        = 0.0;
  EAXREVERB_MAX_REFLECTIONS_GAIN        = 3.16;
  EAXREVERB_DEFAULT_REFLECTIONS_GAIN    = 0.05;

  EAXREVERB_MIN_REFLECTIONS_DELAY       = 0.0;
  EAXREVERB_MAX_REFLECTIONS_DELAY       = 0.3;
  EAXREVERB_DEFAULT_REFLECTIONS_DELAY   = 0.007;

  EAXREVERB_MIN_LATE_REVERB_GAIN        = 0.0;
  EAXREVERB_MAX_LATE_REVERB_GAIN        = 10.0;
  EAXREVERB_DEFAULT_LATE_REVERB_GAIN    = 1.26;

  EAXREVERB_MIN_LATE_REVERB_DELAY       = 0.0;
  EAXREVERB_MAX_LATE_REVERB_DELAY       = 0.1;
  EAXREVERB_DEFAULT_LATE_REVERB_DELAY   = 0.011;

  EAXREVERB_MIN_ECHO_TIME               = 0.075;
  EAXREVERB_MAX_ECHO_TIME               = 0.25;
  EAXREVERB_DEFAULT_ECHO_TIME           = 0.25;

  EAXREVERB_MIN_ECHO_DEPTH              = 0.0;
  EAXREVERB_MAX_ECHO_DEPTH              = 1.0;
  EAXREVERB_DEFAULT_ECHO_DEPTH          = 0.0;

  EAXREVERB_MIN_MODULATION_TIME         = 0.04;
  EAXREVERB_MAX_MODULATION_TIME         = 4.0;
  EAXREVERB_DEFAULT_MODULATION_TIME     = 0.25;

  EAXREVERB_MIN_MODULATION_DEPTH        = 0.0;
  EAXREVERB_MAX_MODULATION_DEPTH        = 1.0;
  EAXREVERB_DEFAULT_MODULATION_DEPTH    = 0.0;

  EAXREVERB_MIN_AIR_ABSORPTION_GAINHF   = 0.892;
  EAXREVERB_MAX_AIR_ABSORPTION_GAINHF   = 1.0;
  EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF = 0.994;

  EAXREVERB_MIN_HFREFERENCE             = 1000.0;
  EAXREVERB_MAX_HFREFERENCE             = 20000.0;
  EAXREVERB_DEFAULT_HFREFERENCE         = 5000.0;

  EAXREVERB_MIN_LFREFERENCE             = 20.0;
  EAXREVERB_MAX_LFREFERENCE             = 1000.0;
  EAXREVERB_DEFAULT_LFREFERENCE         = 250.0;

  EAXREVERB_MIN_ROOM_ROLLOFF_FACTOR     = 0.0;
  EAXREVERB_MAX_ROOM_ROLLOFF_FACTOR     = 10.0;
  EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR = 0.0;

  EAXREVERB_MIN_DECAY_HFLIMIT           = False;
  EAXREVERB_MAX_DECAY_HFLIMIT           = True;
  EAXREVERB_DEFAULT_DECAY_HFLIMIT       = True;

Type
  SoundAmbience = Class(TERRAObject)
    Protected
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
	    _ReflectionsPan: Vector3DProperty;
	    _LateReverbGain:FloatProperty;
	    _LateReverbDelay:FloatProperty;
	    _LateReverbPan: Vector3DProperty;
	    _EchoTime:FloatProperty;
	    _EchoDepth:FloatProperty;
	    _ModulationTime:FloatProperty;
	    _ModulationDepth:FloatProperty;
	    _AirAbsorptionGainHF:FloatProperty;
	    _HFReference:FloatProperty;
	    _LFReference:FloatProperty;
	    _RoomRolloffFactor:FloatProperty;
	    _DecayHFLimit:BooleanProperty;

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

    Public
      Constructor Create(Const Name:TERRAString);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function GetObjectType:TERRAString; Override;
      
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
Uses TERRA_ResourceManager, TERRA_FileManager, TERRA_SoundManager;

Const
// EAX Reverb effect parameters
  AL_EAXREVERB_DENSITY                     = $0001;
  AL_EAXREVERB_DIFFUSION                   = $0002;
  AL_EAXREVERB_GAIN                        = $0003;
  AL_EAXREVERB_GAINHF                      = $0004;
  AL_EAXREVERB_GAINLF                      = $0005;
  AL_EAXREVERB_DECAY_TIME                  = $0006;
  AL_EAXREVERB_DECAY_HFRATIO               = $0007;
  AL_EAXREVERB_DECAY_LFRATIO               = $0008;
  AL_EAXREVERB_REFLECTIONS_GAIN            = $0009;
  AL_EAXREVERB_REFLECTIONS_DELAY           = $000A;
  AL_EAXREVERB_REFLECTIONS_PAN             = $000B;
  AL_EAXREVERB_LATE_REVERB_GAIN            = $000C;
  AL_EAXREVERB_LATE_REVERB_DELAY           = $000D;
  AL_EAXREVERB_LATE_REVERB_PAN             = $000E;
  AL_EAXREVERB_ECHO_TIME                   = $000F;
  AL_EAXREVERB_ECHO_DEPTH                  = $0010;
  AL_EAXREVERB_MODULATION_TIME             = $0011;
  AL_EAXREVERB_MODULATION_DEPTH            = $0012;
  AL_EAXREVERB_AIR_ABSORPTION_GAINHF       = $0013;
  AL_EAXREVERB_HFREFERENCE                 = $0014;
  AL_EAXREVERB_LFREFERENCE                 = $0015;
  AL_EAXREVERB_ROOM_ROLLOFF_FACTOR         = $0016;
  AL_EAXREVERB_DECAY_HFLIMIT               = $0017;

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
	_ReflectionsPan :=  Vector3DProperty.Create('ReflectionsPan', Vector3D_Zero);
	_LateReverbGain := FloatProperty.Create('LateReverbGain', EAXREVERB_DEFAULT_LATE_REVERB_GAIN);
	_LateReverbDelay := FloatProperty.Create('LateReverbDelay', EAXREVERB_DEFAULT_LATE_REVERB_DELAY);
	_LateReverbPan :=  Vector3DProperty.Create('LateReverbPan', Vector3D_Zero);
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
  Result := _LateReverbPan.Value;
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
  Result := _ReflectionsPan.Value;
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
  _LateReverbPan.Value := Value;
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
  _ReflectionsPan.Value := Value;
End;

Procedure SoundAmbience.SetRoomRolloffFactor(const Value: Single);
Begin
  _RoomRolloffFactor.Value := Value;
End;

Function SoundAmbience.GetObjectType: TERRAString;
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
    10: Result := _ReflectionsPan;
    11: Result := _LateReverbGain;
    12: Result := _LateReverbDelay;
    13: Result := _LateReverbPan;
    14: Result := _EchoTime;
    15: Result := _EchoDepth;
    16: Result := _ModulationTime;
    17: Result := _ModulationDepth;
    18: Result := _AirAbsorptionGainHF;
    19: Result := _HFReference;
    20: Result := _LFReference;
    21: Result := _RoomRolloffFactor;
    22: Result := _DecayHFLimit;
    Else
      Result := Nil;
  End;
End;

End.