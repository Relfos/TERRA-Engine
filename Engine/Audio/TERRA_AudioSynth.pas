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
 * TERRA_AudioSynth
 * Implements a software sound synth
 ***********************************************************************************************************************
}
Unit TERRA_AudioSynth;
{$I Terra.inc}

(*
function get2dvol(chn,vol,posx,posy : longint) : word;
{  Get volume of a channnel in a 2D-field

          X-AXIS

            256



Y
|
A    L       0        R
X  -256              256
I
S


             S
           -256

Position 0 is the listener's position
}
var
  val : longint;

begin
  if (posx < -256) then posx := -256;
  if (posx >  256) then posx :=  256;

  if (posy < -256) then posy := -256;
  if (posy >  256) then posy :=  256;

  { If only stereo, don't allow S signal to disappear }
  if (data.flags and snd_prologic = 0) then
  begin
    if (posy < -128) then posy := -128;
  end;

  case chn of
    0 : val := (((256-posx))*(256-abs(posy))) shr 8;
    1 : val := (((256+posx))*(256-abs(posy))) shr 8;
    2 : begin
          if posy >= 0 then val := 0
          else val := abs(posy)-abs(posx) shr 3;
          if val < 0 then val := 0;
        end;
    else val := 0;
  end;
  if (val > 256) then val := 256;
  get2dvol := val*vol shr 8;
end;

*)

{$DEFINE EQUALIZER}
{$DEFINE REVERB}
Interface
Uses TERRA_Utils, TERRA_Instrument
{$IFDEF EQUALIZER}, TERRA_EQUALIZER{$ENDIF};

{$IFDEF REVERB}
Const
  ReverbBufferSize = 1024*10;
{$ENDIF}

Type
  SynthNote = Record
    Note:Byte;
    IgnorePitchChange:Boolean;
    Velocity:Byte;
    StartTime:Cardinal;
    EndTime:Cardinal;
  End;

  SynthChannel = Record
    Patch:Instrument;
    Volume:Single;
    Pan:Byte;
    Notes:Array Of SynthNote;
    NoteCount:Integer;
  End;

  Synthetizer = Class
    Protected
      _Channels:Array Of SynthChannel;
      _ChannelCount:Integer;
      _Frequency:Cardinal;
      {$IFDEF EQUALIZER}
      _Equalizer:Equalizer;
      {$ENDIF}
      {$IFDEF REVERB}
      _ReverbBuffer:PSmallIntArray;
      {$ENDIF}

    Public
      Constructor Create(ChannelCount, Frequency:Cardinal);
      Destructor Destroy;

//      Procedure AddInstrument(Inst:Instrument);

      Procedure SetInstrument(Channel:Cardinal; MyInstrument:Instrument);
      Procedure SetPan(Channel:Cardinal; Pan:Byte);
      Procedure SetVolume(Channel:Cardinal; Volume:Single);

      Procedure NoteOn(Note, Channel, Time, Velocity:Cardinal; IgnorePitchChange:Boolean);
      Procedure NoteOff(Note, Channel, Time, Velocity:Cardinal);

      Procedure Mix(Dest:PSmallInt; SampleCount, StartTime:Cardinal);
  End;

Implementation
Uses TERRA_Application;

{$IFDEF EQUALIZER}
Const
  EqSettings:EqualizerSettings = (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);
{$ENDIF}

{ Synthetizer }

Constructor Synthetizer.Create(ChannelCount, Frequency:Cardinal);
Var
  I:Integer;
Begin
  Self._ChannelCount := ChannelCount;
  SetLength(_Channels, ChannelCount);
  For I:=0 To Pred(ChannelCount) Do
  Begin
    _Channels[I].Patch := Nil;
    _Channels[I].NoteCount := 0;
    _Channels[I].Pan := 127;
    _Channels[I].Volume := 1.0;
  End;

  Self._Frequency := Frequency;
  {$IFDEF EQUALIZER}
    _Equalizer := Equalizer.Create(Frequency, @EqSettings);
  {$ENDIF};
  {$IFDEF REVERB}
  GetMem(_ReverbBuffer, ReverbBufferSize*4);
  FillChar(_ReverbBuffer^, ReverbBufferSize*4, 0);
  {$ENDIF}
End;


Destructor Synthetizer.Destroy;
Begin
  {$IFDEF EQUALIZER}
  If Assigned(_Equalizer) Then
    _Equalizer.Destroy;
  {$ENDIF};
  {$IFDEF REVERB}
  If Assigned(_ReverbBuffer) Then
    FreeMem(_ReverbBuffer);
  {$ENDIF}
End;

Procedure Synthetizer.NoteOff(Note, Channel, Time, Velocity:Cardinal);
Var
  I:Integer;
Begin
  Time := Trunc(Time * (_Frequency/1000));
  Time := Time + Random(30);
  For I:=0 To Pred(_Channels[Channel].NoteCount) Do
  If (_Channels[Channel].Notes[I].Note=Note) And (_Channels[Channel].Notes[I].EndTime=0) Then
  Begin
    _Channels[Channel].Notes[I].EndTime := Time;
    Exit;
  End;
End;

Procedure Synthetizer.NoteOn(Note, Channel, Time, Velocity: Cardinal; IgnorePitchChange:Boolean);
Var
  I:Integer;
Begin
  Time := Trunc(Time * (_Frequency/1000));
  Time := Time + Random(30);
  I := _Channels[Channel].NoteCount;
  Inc(_Channels[Channel].NoteCount);
  SetLength(_Channels[Channel].Notes, _Channels[Channel].NoteCount);

  If Not Assigned(_Channels[Channel].Patch) Then
    IntToString(2);
  _Channels[Channel].Notes[I].Note := Note;
  _Channels[Channel].Notes[I].Velocity := Velocity;
  _Channels[Channel].Notes[I].StartTime := Time;
  _Channels[Channel].Notes[I].EndTime := 0;
  _Channels[Channel].Notes[I].IgnorePitchChange := IgnorePitchChange;
End;

Procedure Synthetizer.SetInstrument(Channel: Cardinal; MyInstrument: Instrument);
Begin
  If Not Assigned(MyInstrument) Then
    Exit;

  If (Channel<0) Or (Channel>=_ChannelCount) Then
    RaiseError('Invalid synth channel!')
  Else
    _Channels[Channel].Patch := MyInstrument;
End;

Procedure Synthetizer.SetVolume(Channel:Cardinal; Volume:Single);
Begin
  If (Channel<0) Or (Channel>=_ChannelCount) Then
    RaiseError('Invalid synth channel!')
  Else
    _Channels[Channel].Volume := Volume;
End;

Procedure Synthetizer.SetPan(Channel:Cardinal; Pan:Byte);
Begin
  If (Channel<0) Or (Channel>=_ChannelCount) Then
    RaiseError('Invalid synth channel!')
  Else
    _Channels[Channel].Pan := Pan;
End;

Procedure Synthetizer.Mix(Dest: PSmallInt; SampleCount, StartTime:Cardinal);
Var
  I,J,K,N:Integer;
  Buf:PSmallInt;
  Value:SmallInt;
  T1,T2, Count:Integer;
  EndTime, Ofs, MaxSamples:Cardinal;
  NoiseReductionLeft,NoiseReductionRight:SmallInt;
{$IFDEF REVERB}
  RL,RR:SmallInt;
{$ENDIF}
Begin
  StartTime := Trunc(StartTime * (_Frequency/1000));
  EndTime := StartTime + SampleCount;
  FillChar(Dest^, SampleCount*4, 0);

  For I:=0 To Pred(_ChannelCount) Do
  If (Assigned(_Channels[I].Patch)) And (_Channels[I].Patch.IsReady) Then
    For J:=0 To Pred(_Channels[I].NoteCount) Do
    Begin
      T2 := _Channels[I].Notes[J].EndTime;
      If (T2=0) Then
        T2 := EndTime;
      T2 := Integer(T2 - StartTime);

      If (T2<0) Then
        Continue
      Else
      If (T2>EndTime) Then
        T2 := EndTime;

      T1 := Integer(_Channels[I].Notes[J].StartTime - StartTime);
      If (T1>=EndTime) Then
        Continue
      Else
      If (T1<0) Then
      Begin
        Ofs := -T1;
        T1 := 0;
      End Else
        Ofs := 0;

      Count := Integer(T2-T1);
      If Count<=0 Then
        Continue;

      MaxSamples := EndTime-T1;
      _Channels[I].Patch.Play(_Channels[I].Notes[J].Note, _Frequency, T1, Count, MaxSamples, Ofs, Dest, _Channels[I].Pan, Trunc(_Channels[I].Volume* _Channels[I].Notes[J].Velocity));
    End;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

{$IFDEF REVERB}
  J := Pred(SampleCount);
  While (J>=0) Do
  Begin
    If (J<ReverbBufferSize) Then
    Begin
      RL := _ReverbBuffer[J*2+0];
      RR := _ReverbBuffer[J*2+1];
    End Else
    Begin
      RL := PSmallIntArray(Dest)[(J-ReverbBufferSize)*2+0];
      RR := PSmallIntArray(Dest)[(J-ReverbBufferSize)*2+1];
    End;

    Buf := @(PSmallIntArray(Dest)[J*2+0]);
    Buf^ := Buf^ + (RL Shr 1);
    Buf := @(PSmallIntArray(Dest)[J*2+1]);
    Buf^ := Buf^ + (RR Shr 1);

    Dec(J);
  End;

//  Move(Pointer(Cardinal(Dest)+(SampleCount-ReverbBufferSize)*4)^, _ReverbBuffer^, ReverbBufferSize*4);
{$ENDIF}

{$IFDEF EQUALIZER}
  _Equalizer.Process(Dest, SampleCount, 2);
{$ELSE}
  // Lowpass Filter
  Buf:=Dest;
  J := SampleCount;
  WHILE J>0 DO
  BEGIN
   Value := Buf^ DIV 2;
   Buf^ := Value + NoiseReductionLeft;
   NoiseReductionLeft := Value;
   INC(Buf);
   Value := Buf^ DIV 2;
   Buf^ := Value + NoiseReductionRight;
   NoiseReductionRight := Value;
   INC(Buf);
   Dec(J);
  END;
{$ENDIF}
  // discard used notes
  For I:=0 To Pred(_ChannelCount) Do
  Begin
    J := 0;
    While (J<_Channels[I].NoteCount) Do
    If (_Channels[I].Notes[J].EndTime>0) And (_Channels[I].Notes[J].EndTime<=EndTime) Then
    Begin
      _Channels[I].Notes[J] := _Channels[I].Notes[Pred(_Channels[I].NoteCount)];
      Dec(_Channels[I].NoteCount);
    End Else
      Inc(J);
  End;
End;

End.