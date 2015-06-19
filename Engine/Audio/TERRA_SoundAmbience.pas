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
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Stream, TERRA_SoundSource;

Type
  SoundAmbience = Class(TERRAObject)
    Protected
      _Name:TERRAString;
	    _Density: Single;
	    _Diffusion: Single;
	    _Gain: Single;
	    _GainHF: Single;
	    _GainLF: Single;
	    _DecayTime: Single;
	    _DecayHFRatio: Single;
	    _DecayLFRatio: Single;
	    _ReflectionsGain: Single;
	    _ReflectionsDelay: Single;
	    _ReflectionsPan: packed array[0..2] of Single;
	    _LateReverbGain: Single;
	    _LateReverbDelay: Single;
	    _LateReverbPan: packed array[0..2] of Single;
	    _EchoTime: Single;
	    _EchoDepth: Single;
	    _ModulationTime: Single;
	    _ModulationDepth: Single;
	    _AirAbsorptionGainHF: Single;
	    _HFReference: Single;
	    _LFReference: Single;
	    _RoomRolloffFactor: Single;
	    _DecayHFLimit: integer;

      _EffectHandle:Integer;
      _SlotHandle:Integer;

      Procedure Update;

    Public
      Constructor Create;
      Procedure Release; Override;

      Function Load(Source:Stream):Boolean; Overload;
      Function Load(Name:TERRAString):Boolean; Overload;
      Function Save(Dest:Stream):Boolean;

      Property Handle:Integer Read _SlotHandle;
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

Constructor SoundAmbience.Create;
Begin
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

End.