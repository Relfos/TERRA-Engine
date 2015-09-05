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
      Procedure Release; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function GetObjectType:TERRAString; Override;
      
      Function Load(Source:TERRAStream):Boolean; Overload;
      Function Load(Name:TERRAString):Boolean; Overload;
      Function Save(Dest:TERRAStream):Boolean;

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
Uses TERRA_ResourceManager, TERRA_FileManager, TERRA_SoundManager, TERRA_AL;

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

Function SoundAmbience.Load(Name:TERRAString):Boolean;
Var
  Source:Stream;
Begin
  If (Name='') Then
  Begin
    Result := False;
    Exit;
  End;

  If Pos('.', Name)<=0 Then
    Name := Name + '.afx';
  If (Name = _Name) Then
  Begin
    Result := True;
    Exit;
  End;

  Source := FileManager.Instance().OpenStream(Name);
  If Assigned(Source) Then
  Begin
    Result := Load(Source);
    ReleaseObject(Source);
  End Else
    Result := False;

  _Name := Name;
End;
              
Function SoundAmbience.Load(Source: Stream): Boolean;
Begin
  Source.Read(@_Density, 4);
	Source.Read(@_Diffusion, 4);
  Source.Read(@_Gain, 4);
  Source.Read(@_GainHF, 4);
  Source.Read(@_GainLF, 4);
  Source.Read(@_DecayTime, 4);
  Source.Read(@_DecayHFRatio, 4);
  Source.Read(@_DecayLFRatio, 4);
  Source.Read(@_ReflectionsGain, 4);
  Source.Read(@_ReflectionsDelay, 4);
  Source.Read(@_ReflectionsPan[0], 4);
  Source.Read(@_ReflectionsPan[1], 4);
  Source.Read(@_ReflectionsPan[2], 4);
  Source.Read(@_LateReverbGain, 4);
  Source.Read(@_LateReverbDelay, 4);
  Source.Read(@_LateReverbPan[0], 4);
  Source.Read(@_LateReverbPan[1], 4);
  Source.Read(@_LateReverbPan[2], 4);
  Source.Read(@_EchoTime, 4);
  Source.Read(@_EchoDepth, 4);
  Source.Read(@_ModulationTime, 4);
  Source.Read(@_ModulationDepth, 4);
  Source.Read(@_AirAbsorptionGainHF, 4);
  Source.Read(@_HFReference, 4);
  Source.Read(@_LFReference, 4);
  Source.Read(@_RoomRolloffFactor, 4);
  Source.Read(@_DecayHFLimit, 4);

  Self.Update;
  Result := True;
End;

Function SoundAmbience.Save(Dest: Stream): Boolean;
Begin
  Dest.Write(@_Density, 4);
	Dest.Write(@_Diffusion, 4);
  Dest.Write(@_Gain, 4);
  Dest.Write(@_GainHF, 4);
  Dest.Write(@_GainLF, 4);
  Dest.Write(@_DecayTime, 4);
  Dest.Write(@_DecayHFRatio, 4);
  Dest.Write(@_DecayLFRatio, 4);
  Dest.Write(@_ReflectionsGain, 4);
  Dest.Write(@_ReflectionsDelay, 4);
  Dest.Write(@_ReflectionsPan[0], 4);
  Dest.Write(@_ReflectionsPan[1], 4);
  Dest.Write(@_ReflectionsPan[2], 4);
  Dest.Write(@_LateReverbGain, 4);
  Dest.Write(@_LateReverbDelay, 4);
  Dest.Write(@_LateReverbPan[0], 4);
  Dest.Write(@_LateReverbPan[1], 4);
  Dest.Write(@_LateReverbPan[2], 4);
  Dest.Write(@_EchoTime, 4);
  Dest.Write(@_EchoDepth, 4);
  Dest.Write(@_ModulationTime, 4);
  Dest.Write(@_ModulationDepth, 4);
  Dest.Write(@_AirAbsorptionGainHF, 4);
  Dest.Write(@_HFReference, 4);
  Dest.Write(@_LFReference, 4);
  Dest.Write(@_RoomRolloffFactor, 4);
  Dest.Write(@_DecayHFLimit, 4);
  Result := False;
End;

Procedure SoundAmbience.Update;
Begin
  If Not Assigned(alGenEffects) Then
    Exit;

  alEffectf(_EffectHandle, AL_EAXREVERB_DENSITY, _Density);               {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_DIFFUSION, _Diffusion);           {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_GAIN, _Gain);                     {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_GAINHF, _GainHF);                 {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_GAINLF, _GainLF);                 {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_DECAY_TIME, _DecayTime);          {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_DECAY_HFRATIO, _DecayHFRatio);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_DECAY_LFRATIO, _DecayLFRatio);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_REFLECTIONS_GAIN, _ReflectionsGain);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_REFLECTIONS_DELAY, _ReflectionsDelay);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectfv(_EffectHandle, AL_EAXREVERB_REFLECTIONS_PAN, @_ReflectionsPan);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_LATE_REVERB_GAIN, _LateReverbGain);     {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_LATE_REVERB_DELAY, _LateReverbDelay);   {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectfv(_EffectHandle, AL_EAXREVERB_LATE_REVERB_PAN, @_LateReverbPan);     {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_ECHO_TIME, _EchoTime);                  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_ECHO_DEPTH, _EchoDepth);                {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_MODULATION_TIME, _ModulationTime);      {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_MODULATION_DEPTH, _ModulationDepth);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, _AirAbsorptionGainHF); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_HFREFERENCE, _HFReference);                   {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_LFREFERENCE, _LFReference);                   {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffectf(_EffectHandle, AL_EAXREVERB_ROOM_ROLLOFF_FACTOR, _RoomRolloffFactor);     {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  alEffecti(_EffectHandle, AL_EAXREVERB_DECAY_HFLIMIT, _DecayHFLimit);                {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

  // Load Effect into Auxiliary Effect Slot
  alAuxiliaryEffectSloti(_SlotHandle, AL_EFFECTSLOT_EFFECT, _EffectHandle); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
End;

Constructor SoundAmbience.Create(Const Name:TERRAString);
Begin
  Self._ObjectName := Name;
  
  If Assigned(alGenEffects) Then
  Begin
    // Generate an Auxiliary Effect Slot
    alGenAuxiliaryEffectSlots(1, @_SlotHandle);                              {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    // Generate an Effect
    alGenEffects(1, @_EffectHandle);                                          {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    // Set the Effect Type
    alEffecti(_EffectHandle, AL_EFFECT_TYPE, AL_EFFECT_EAXREVERB);            {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  End Else
  Begin
    _EffectHandle := 0;
    _SlotHandle := 0;
  End;

	_Density := 1.0;
	_Diffusion := 0.20999999344;
	_Gain := 0.31622776389;
	_GainHF := 0.10000000149;
	_GainLF := 1.0;
	_DecayTime := 1.4900000095;
	_DecayHFRatio := 0.5;
	_DecayLFRatio := 1.0;
	_ReflectionsGain := 0.058479003608;
	_ReflectionsDelay := 0.17900000513;
	_ReflectionsPan[0] := 0.0;
	_ReflectionsPan[1] := 0.0;
	_ReflectionsPan[2] := 0.0;
	_LateReverbGain := 0.10889300704;
	_LateReverbDelay := 0.10000000149;
	_LateReverbPan[0] := 0.0;
	_LateReverbPan[1] := 0.0;
	_LateReverbPan[2] := 0.0;
	_EchoTime := 0.25;
	_EchoDepth := 1.0;
	_ModulationTime := 0.25;
	_ModulationDepth := 0.0;
	_AirAbsorptionGainHF := 0.99426007271;
	_HFReference := 5000;
	_LFReference := 250;
	_RoomRolloffFactor := 0.0;
	_DecayHFLimit := 0;

  Self.Update;
End;

Procedure SoundAmbience.Release;
Var
  I:Integer;
Begin
  If (_SlotHandle<>0) Then
  Begin
    // Load NULL Effect into Effect Slot
    alAuxiliaryEffectSloti(_SlotHandle, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

    // Delete Auxiliary Effect Slot
    alDeleteAuxiliaryEffectSlots(1, @_SlotHandle);                              {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    _SlotHandle := 0;
  End;

  // Delete Effect
  If (_EffectHandle<>0) Then
  Begin
    alDeleteEffects(1, @_EffectHandle);                                         {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    _EffectHandle := 0;
  End;
End;

function SoundAmbience.GetAirAbsorptionGainHF: Single;
begin

end;

function SoundAmbience.GetDecayHFLimit: Boolean;
begin

end;

function SoundAmbience.GetDecayHFRatio: Single;
begin

end;

function SoundAmbience.GetDecayLFRatio: Single;
begin

end;

function SoundAmbience.GetDecayTime: Single;
begin

end;

function SoundAmbience.GetDensity: Single;
begin

end;

function SoundAmbience.GetDiffusion: Single;
begin

end;

function SoundAmbience.GetEchoDepth: Single;
begin

end;

function SoundAmbience.GetEchoTime: Single;
begin

end;

function SoundAmbience.GetGain: Single;
begin

end;

function SoundAmbience.GetGainHF: Single;
begin

end;

function SoundAmbience.GetGainLF: Single;
begin

end;

function SoundAmbience.GetHFReference: Single;
begin

end;

function SoundAmbience.GetLateReverbDelay: Single;
begin

end;

function SoundAmbience.GetLateReverbGain: Single;
begin

end;

function SoundAmbience.GetLateReverbPan: Vector3D;
begin

end;

function SoundAmbience.GetLFReference: Single;
begin

end;

function SoundAmbience.GetModulationDepth: Single;
begin

end;

function SoundAmbience.GetModulationTime: Single;
begin

end;

function SoundAmbience.GetObjectType: TERRAString;
begin

end;

function SoundAmbience.GetPropertyByIndex(Index: Integer): TERRAObject;
begin

end;

function SoundAmbience.GetReflectionsDelay: Single;
begin

end;

function SoundAmbience.GetReflectionsGain: Single;
begin

end;

function SoundAmbience.GetReflectionsPan: Vector3D;
begin

end;

function SoundAmbience.GetRoomRolloffFactor: Single;
begin

end;

function SoundAmbience.Load(Source: TERRAStream): Boolean;
begin

end;

procedure SoundAmbience.SetAirAbsorptionGainHF(const Value: Single);
begin

end;

procedure SoundAmbience.SetDecayHFLimit(const Value: Boolean);
begin

end;

procedure SoundAmbience.SetDecayHFRatio(const Value: Single);
begin

end;

procedure SoundAmbience.SetDecayLFRatio(const Value: Single);
begin

end;

procedure SoundAmbience.SetDecayTime(const Value: Single);
begin

end;

procedure SoundAmbience.SetDensity(const Value: Single);
begin

end;

procedure SoundAmbience.SetDiffusion(const Value: Single);
begin

end;

procedure SoundAmbience.SetEchoDepth(const Value: Single);
begin

end;

procedure SoundAmbience.SetEchoTime(const Value: Single);
begin

end;

procedure SoundAmbience.SetGain(const Value: Single);
begin

end;

procedure SoundAmbience.SetGainHF(const Value: Single);
begin

end;

procedure SoundAmbience.SetGainLF(const Value: Single);
begin

end;

procedure SoundAmbience.SetHFReference(const Value: Single);
begin

end;

procedure SoundAmbience.SetLateReverbDelay(const Value: Single);
begin

end;

procedure SoundAmbience.SetLateReverbGain(const Value: Single);
begin

end;

procedure SoundAmbience.SetLateReverbPan(const Value: Vector3D);
begin

end;

procedure SoundAmbience.SetLFReference(const Value: Single);
begin

end;

procedure SoundAmbience.SetModulationDepth(const Value: Single);
begin

end;

procedure SoundAmbience.SetModulationTime(const Value: Single);
begin

end;

procedure SoundAmbience.SetReflectionsDelay(const Value: Single);
begin

end;

procedure SoundAmbience.SetReflectionsGain(const Value: Single);
begin

end;

procedure SoundAmbience.SetReflectionsPan(const Value: Vector3D);
begin

end;

procedure SoundAmbience.SetRoomRolloffFactor(const Value: Single);
begin

end;

End.