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
 * TERRA_ALStub
 * Implements a OpenAL stub for when the library is not installed on the user computer.
 ***********************************************************************************************************************
}

Unit TERRA_ALstub;
{$I terra.inc}
Interface

Procedure LoadALstubs;

Implementation
Uses TERRA_AL{$IFDEF WINDOWS},Windows{$ENDIF};

Var
  BufferCount:Integer=0;
  SourcesCount:Integer=0;

Procedure alGenBuffers(n:Cardinal; buffers: PCardinal); CDecl;
Begin
  While (N>0) Do
  Begin
    Inc(BufferCount);
    Buffers^ := BufferCount;
    Inc(Buffers);
    Dec(N);
  End;
End;

Procedure alDeleteBuffers(n:Cardinal; buffers: PCardinal); CDecl;
Begin
End;

Procedure alBufferData(buffer:Cardinal; format:Integer; data: Pointer; size, freq:Cardinal); CDecl;
Begin
End;

Procedure alGenSources(n:Cardinal; sources: PCardinal); CDecl;
Begin
  While (N>0) Do
  Begin
    Inc(SourcesCount);
    Sources^ := SourcesCount;
    Inc(Sources);
    Dec(N);
  End;
End;

Procedure alDeleteSources(n:Cardinal; sources: PCardinal); CDecl;
Begin
End;

Procedure alSourcei(source: Cardinal; param:Integer; value: Integer); CDecl;
Begin
End;

Procedure alSourcef(source: Cardinal; param:Integer; value: Single); CDecl;
Begin
End;

Procedure alSourcefv(source: Cardinal; param:Integer; values: PSingle); CDecl;
Begin
End;

Procedure alGetSourcei(source:Cardinal; param:Integer; value: PInteger); CDecl;
Begin
  Value^ := 0;
End;

Procedure alListenerfv(param:Integer; values: PSingle); CDecl;
Begin
End;

Procedure alSourceQueueBuffers(source:Cardinal; n:Cardinal; buffers: PCardinal); CDecl;
Begin
End;

Procedure alSourceUnqueueBuffers(source:Cardinal; n:Cardinal; buffers: PCardinal); CDecl;
Begin
End;

Procedure alSourcePlay(source:Cardinal); CDecl;
Begin
End;

Procedure alSourcePause(source:Cardinal); CDecl;
Begin
End;

Procedure alSourceStop(source:Cardinal); CDecl;
Begin
End;

Function alcGetString(device: PALCdevice; param: Integer): PAnsiChar; CDecl;
Begin
  Result := Nil;
End;

Function alcOpenDevice(deviceName: PAnsiChar): PALCdevice; CDecl;
Begin
  Result := Nil;
End;

Procedure alcCloseDevice(device: PALCdevice); CDecl;
Begin
End;

Function alcGetContextsDevice(context: PALCcontext): PALCdevice; CDecl;
Begin
  Result := Nil;
End;

Function alcCreateContext(device: PALCdevice; attrlist: PInteger): PALCcontext; CDecl;
Begin
  Result := Nil;
End;

Function alcMakeContextCurrent(context: PALCcontext): Integer; CDecl;
Begin
  Result := 0;
End;

Function alcGetCurrentContext: PALCcontext; CDecl;
Begin
  Result := Nil;
End;

Procedure alcDestroyContext(context: PALCcontext); CDecl;
Begin
End;

Function alGetError:Integer; CDecl;
Begin
  Result := AL_NO_ERROR;
End;

Procedure alGenEffects(n:Integer; effects:PCardinal); CDecl;
Begin
End;

Procedure alDeleteEffects(N:Integer; effects:PCardinal); CDecl;
Begin
End;

Function alIsEffect(effect:Cardinal):Boolean; CDecl;
Begin
  Result := True;
End;

Procedure alEffecti(effect:Cardinal; param:Cardinal; iValue:Integer); CDecl;
Begin
End;

Procedure alEffectiv(effect:Cardinal; param:Cardinal; piValues:Integer); CDecl;
Begin
End;

Procedure alEffectf(effect:Cardinal; param:Cardinal; flValue:Single); CDecl;
Begin
End;

Procedure alEffectfv(effect:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
Begin
End;

Procedure alGetEffecti(effect:Cardinal; param:Cardinal; piValue:PInteger); CDecl;
Begin
End;

Procedure alGetEffectiv(effect:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
Begin
End;

Procedure alGetEffectf(effect:Cardinal; param:Cardinal; pflValue:PSingle); CDecl;
Begin
End;

Procedure alGetEffectfv(effect:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
Begin
End;

Procedure alGenAuxiliaryEffectSlots(n:Integer; effectslots:PCardinal); CDecl;
Begin
End;

Procedure alDeleteAuxiliaryEffectSlots(N:Integer; effectslots:PCardinal); CDecl;
Begin
End;

Procedure alIsAuxiliaryEffectSlot(effectslot:Cardinal); CDecl;
Begin
End;

Procedure alAuxiliaryEffectSloti(effectslot:Cardinal; param:Cardinal; iValue:Integer); CDecl;
Begin
End;

Procedure alAuxiliaryEffectSlotiv(effectslot:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
Begin
End;

Procedure alAuxiliaryEffectSlotf(effectslot:Cardinal; param:Cardinal; flValue:Single); CDecl;
Begin
End;

Procedure alAuxiliaryEffectSlotfv(effectslot:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
Begin
End;

Procedure alGetAuxiliaryEffectSloti(effectslot:Cardinal; param:Cardinal; piValue:PInteger); CDecl;
Begin
End;

Procedure alGetAuxiliaryEffectSlotiv(effectslot:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
Begin
End;

Procedure alGetAuxiliaryEffectSlotf(effectslot:Cardinal; param:Cardinal; pflValue:PSingle); CDecl;
Begin
End;

Procedure alGetAuxiliaryEffectSlotfv(effectslot:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
Begin
End;

Procedure alSource3i(source: Cardinal; param:Integer; v1, v2, v3: Integer); CDecl;
Begin
End;

Procedure LoadALstubs;
Begin
  TERRA_AL.alGenBuffers := alGenBuffers;
  TERRA_AL.alDeleteBuffers := alDeleteBuffers;

  TERRA_AL.alBufferData := alBufferData;

  TERRA_AL.alGenSources := alGenSources;
  TERRA_AL.alDeleteSources := alDeleteSources;

  TERRA_AL.alSourcei := alSourcei;
  TERRA_AL.alSourcef := alSourcef;
  TERRA_AL.alSourcefv := alSourcefv;

  TERRA_AL.alGetSourcei := alGetSourcei;

  TERRA_AL.alListenerfv := alListenerfv;

  TERRA_AL.alSourceQueueBuffers := alSourceQueueBuffers;
  TERRA_AL.alSourceUnqueueBuffers := alSourceUnqueueBuffers;

  TERRA_AL.alSourcePlay := alSourcePlay;
  TERRA_AL.alSourcePause := alSourcePause;
  TERRA_AL.alSourceStop := alSourceStop;

  TERRA_AL.alcGetString := alcGetString;

  TERRA_AL.alcOpenDevice := alcOpenDevice;
  TERRA_AL.alcCloseDevice := alcCloseDevice;
  TERRA_AL.alcGetContextsDevice := alcGetContextsDevice;

  TERRA_AL.alcCreateContext := alcCreateContext;
  TERRA_AL.alcMakeContextCurrent := alcMakeContextCurrent;
  TERRA_AL.alcGetCurrentContext := alcGetCurrentContext;
  TERRA_AL.alcDestroyContext := alcDestroyContext;
  TERRA_AL.alGetError := alGetError;

  TERRA_AL.alGenEffects := alGenEffects;
  TERRA_AL.alDeleteEffects := alDeleteEffects;
  TERRA_AL.alIsEffect := alIsEffect;
  TERRA_AL.alEffecti := alEffecti;
  TERRA_AL.alEffectiv := alEffectiv;
  TERRA_AL.alEffectf := alEffectf;
  TERRA_AL.alEffectfv := alEffectfv;
  TERRA_AL.alGetEffecti := alGetEffecti;
  TERRA_AL.alGetEffectiv := alGetEffectiv;
  TERRA_AL.alGetEffectf := alGetEffectf;
  TERRA_AL.alGetEffectfv := alGetEffectfv;

  TERRA_AL.alGenAuxiliaryEffectSlots := alGenAuxiliaryEffectSlots;
  TERRA_AL.alDeleteAuxiliaryEffectSlots := alDeleteAuxiliaryEffectSlots;
  TERRA_AL.alIsAuxiliaryEffectSlot := alIsAuxiliaryEffectSlot;
  TERRA_AL.alAuxiliaryEffectSloti := alAuxiliaryEffectSloti;
  TERRA_AL.alAuxiliaryEffectSlotiv := alAuxiliaryEffectSlotiv;
  TERRA_AL.alAuxiliaryEffectSlotf := alAuxiliaryEffectSlotf;
  TERRA_AL.alAuxiliaryEffectSlotfv := alAuxiliaryEffectSlotfv;
  TERRA_AL.alGetAuxiliaryEffectSloti := alGetAuxiliaryEffectSloti;
  TERRA_AL.alGetAuxiliaryEffectSlotiv := alGetAuxiliaryEffectSlotiv;
  TERRA_AL.alGetAuxiliaryEffectSlotf := alGetAuxiliaryEffectSlotf;
  TERRA_AL.alGetAuxiliaryEffectSlotfv := alGetAuxiliaryEffectSlotfv;
  TERRA_AL.alSource3i := alSource3i;
End;

End.
