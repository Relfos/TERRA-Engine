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
 * TERRA_Instrument
 * Implements the Instrument resource class, used for synth playback
 ***********************************************************************************************************************
}

Unit TERRA_Instrument;
{$I Terra.inc}

{$DEFINE CUBIC_INTERPOLATION}
Interface
Uses TERRA_Utils, TERRA_Stream, TERRA_Resource, TERRA_Math, TERRA_ResourceManager,
  TERRA_FileManager, TERRA_Application, TERRA_Log;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

Const
  MaxNote = 100;
  InvalidSample = 255;
  InvalidNote = 255;

Type
  Sample = Class(TERRAObject)
    Protected
      _Channels:Array[0..1] Of PSmallIntArray;
      _Size:Cardinal;
      _BaseNote:Byte;
      _MinNote:Byte;
      _MaxNote:Byte;
      _Loop:Boolean;
      _LoopStart:Cardinal;
      _LoopEnd:Cardinal;
      _Frequency:Cardinal;
      _Stereo:Boolean;
      _Pan:Shortint;
      _MinVelocity:Byte;
      _MaxVelocity:Byte;
    Public
      Function Load(Source:Stream):Boolean;

      Function GetSampleAt(X:Single; Channel:Integer):SmallInt;
      Procedure Play(Note:Byte; Frequency, Start, Samples, MaxSamples, Offset:Cardinal; Dest:PSmallInt; Pan,Volume:Integer; IgnorePitchChange:Boolean; FadeOutMode:Byte);

      Procedure Release;

 {$IFDEF EDITOR}
      Constructor Import(FileName:TERRAString); // loads from WAV
      Function Save(Dest:Stream):Boolean;
      Procedure FindLoopShort;
      Procedure FindLoopWide;
      Function FindNote:Byte;

      Property BaseNote:Byte Read _BaseNote Write _BaseNote;
      Property MinNote:Byte Read _MinNote Write _MinNote;
      Property MaxNote:Byte Read _MaxNote Write _MaxNote;
      Property Pan:Shortint Read _Pan Write _Pan;
      Property MinVelocity:Byte Read _MinVelocity Write _MinVelocity;
      Property MaxVelocity:Byte Read _MaxVelocity Write _MaxVelocity;
      Property Loop:Boolean Read _Loop Write _Loop;
      Property LoopStart:Cardinal Read _LoopStart Write _LoopStart;
      Property LoopEnd:Cardinal Read _LoopEnd Write _LoopEnd;
      Property Stereo:Boolean Read _Stereo;
      Property Size:Cardinal Read _Size;
  {$ENDIF}
 End;

  Instrument = Class(Resource)
    Protected
      _midiID:Byte;
      _Samples: Array Of Sample;
      _SampleCount:Integer;
      _Mapping:Array[0..Pred(MaxNote)] Of Sample;
      _Percussion:Boolean;
      _FadeOutMode:Byte;

    Public
      {$IFDEF EDITOR}
      Constructor New;
      Function AddSample(S:Sample):Integer;
      Procedure DeleteSample(Index:Integer);
      Function GetSample(Index:Integer):Sample;
      Procedure Save(FileName:TERRAString);
      {$ENDIF}

      Procedure RemapSample(Index:Integer);
      Function GetSize:Cardinal;

      Class Function GetManager:Pointer; Override;

      Function Load(Source:Stream):Boolean; Override;
      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Procedure Play(Note:Byte; Frequency, Start, Samples, MaxSamples, Offset:Cardinal; Dest:PSmallInt; Pan,Volume:Integer);

      Property SampleCount:Integer Read _SampleCount;
      Property midiID:Byte Read _midiID Write _midiID;
      Property Percussion:Boolean Read _Percussion Write _Percussion;
      Property FadeOut:Byte Read _FadeOutMode Write _FadeOutMode;
  End;

  InstrumentManager = Class(ResourceManager)
    Protected

    Public
      Class Function Instance:InstrumentManager;
      Function GetInstrument(Name:TERRAString; ValidateError:Boolean = True):Instrument;
  End;

  Function GetNoteIndex(S:TERRAString):Integer;
  Function GetNoteName(N:Integer):TERRAString;

Implementation
Uses TERRA_OS {$IFDEF EDITOR},TERRA_FFT{$ENDIF};

Var
  _InstrumentManager:InstrumentManager;

Class Function InstrumentManager.Instance:InstrumentManager;
Begin
  If (_InstrumentManager = Nil) Then
    _InstrumentManager := InstrumentManager.Create;

  Result := _InstrumentManager;
End;

Function InstrumentManager.GetInstrument(Name:TERRAString; ValidateError:Boolean = True):Instrument;
Var
  S:TERRAString;
Begin
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := Instrument(GetResource('instrument',Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance().SearchResourceFile(Name+'.inst');

    If S<>'' Then
    Begin
      Result := Instrument.Create(S);
      Result.InBackground := False;
      Result.Priority := 30;
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      RaiseError('Could not find instrument resource. ['+Name +']');
  End;
End;

Const
  Notes:Array[0..11] Of TERRAString = ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');

Function GetNoteIndex(S:TERRAString):Integer;
Var
  I, N:Integer;
Begin
  N := StringToInt(S[Length(S)]);
  SetLength(S, Pred(Length(S)));
  For I:=0 To 11 Do
  If (S=Notes[I]) Then
  Begin
    Result := N * 12 + I;
    Exit;
  End;

  Result := InvalidNote;
End;

Function GetNoteName(N:Integer):TERRAString;
Var
  Octave, Note:Integer;
Begin
  Octave := N Div 12;
  Note := N Mod 12;
  Result := Notes[Note]+IntToString(Octave);
End;

Const
  Frequencies:Array[0..Pred(MaxNote)] Of Single = (
  16.35, // C0
  17.32, // C#0/Db0
  18.35, // D0
 	19.45, // D#0/Eb0
  20.60, // E0
  21.83, // F0
 	23.12, // F#0/Gb0
  24.50, // G0
 	25.96, // G#0/Ab0
  27.50, // A0
	29.14, // A#0/Bb0
  30.87, // B0
  32.70, // C1
  34.65, // C#1/Db1
  36.71, // D1
  38.89, // D#1/Eb1
  41.20, // E1
  43.65, // F1
	46.25, // F#1/Gb1
  49.00, // G1
  51.91, // G#1/Ab1
  55.00, // A1
  58.27, // A#1/Bb1
  61.74, // B1
  65.41, // C2
 	69.30, // C#2/Db2
  73.42, // D2
 	77.78, // D#2/Eb2
  82.41, // E2
  87.31, // F2
 	92.50, // F#2/Gb2
  98.00, // G2
	103.83, // G#2/Ab2
  110.00, // A2
 	116.54, // A#2/Bb2
  123.47, // B2
  130.81, // C3
 	138.59, // C#3/Db3
  146.83, // D3
  155.56, // D#3/Eb3
  164.81, // E3
  174.61, // F3
 	185.00, // F#3/Gb3
  196.00, // G3
  207.65, // G#3/Ab3
  220.00, // A3
  233.08, // A#3/Bb3
  246.94, // B3
  261.63, // C4
  277.18, // C#4/Db4
  293.66, // D4
  311.13, // D#4/Eb4
  329.63, // E4
  349.23, // F4
  369.99, // F#4/Gb4
  392.00, // G4
  415.30, // G#4/Ab4
  440.00, // A4
  466.16, // A#4/Bb4
  493.88, // B4
  523.25, // C5
  554.37, // C#5/Db5
  587.33, // D5
  622.25, // D#5/Eb5
  659.26, // E5
  698.46, // F5
  739.99, // F#5/Gb5
  783.99, // G5
  830.61, // G#5/Ab5
  880.00, // A5
  932.33, // A#5/Bb5
  987.77, // B5
  1046.50, // C6
  1108.73, // C#6/Db6
  1174.66, // D6
  1244.51, // D#6/Eb6
  1318.51, // E6
  1396.91, // F6
  1479.98, // F#6/Gb6
  1567.98, // G6
  1661.22, // G#6/Ab6
  1760.00, // A6
  1864.66, // A#6/Bb6
  1975.53, // B6
  2093.00, // C7
  2217.46, // C#7/Db7
  2349.32, // D7
  2489.02, // D#7/Eb7
  2637.02, // E7
  2793.83, // F7
  2959.96, // F#7/Gb7
  3135.96, // G7
  3322.44, // G#7/Ab7
  3520.00, // A7
  3729.31, // A#7/Bb7
  3951.07, // B7
  4186.01, // C8
  4434.92, // C#8/Db8
  4698.64, // D8
  4978.03 // D#8/Eb8
  );

{$IFDEF EDITOR}
Const
  PCM_FORMAT =  1;
  // Chunks IDs
  RIFF_ID    =  $46464952;   // "RIFF"
  WAVE_ID    =  $45564157;   // "WAVE"
  FMT_ID     =  $20746D66;   // " fmt"
  DATA_ID    =  $61746164;   // "data"

Type
  TWaveHeader=Packed Record
                Format:Word;
                Channels:Word;
                Frequency:Cardinal;
                AvgBytes:Cardinal;
                BlockAlign:Word;
                Bits:Word;
              End;

  TWaveChunk=Packed Record
                ID:Cardinal;
                Size:Cardinal;
              End;

Function FindChunk(Source:Stream; ID:Cardinal; Var Chunk:TWaveChunk):Boolean;
Var
  Pos:Cardinal;
Begin
  Result:=False;
  Pos:=Source.Position;
  Repeat
    Source.Read(Chunk, SizeOf(Chunk));
    If Chunk.ID<>ID Then
    Begin
       Inc(Pos,SizeOf(Chunk)+Chunk.Size);
       Source.Seek(Pos);
    End Else
      Result:=True;
  Until (Result) Or (Source.Position>=Source.Size);
End;

Constructor Sample.Import(FileName:TERRAString);
Var
  Chunk:TWaveChunk;
  Header:TWaveHeader;
  Pos,Id :Cardinal;
  Source:Stream;
  Samples:PSmallIntArray;
  I,Count:Cardinal;
Begin
  Source := MemoryStream.Open(FileName);
  Source.Read(Chunk, SizeOf(Chunk));
  Source.Read(Id,4);

  If (Chunk.ID<>RIFF_ID) Or (Id<> WAVE_ID) Then
  Begin
    RaiseError('Invalid wave file.');
    ReleaseObject(Source);
    Exit;
  End;

  If Not FindChunk(Source, FMT_ID, Chunk ) Then
  Begin
    RaiseError('Cannot find header chunk.');
    ReleaseObject(Source);
    Exit;
  End;

  Pos:=Source.Position;
  Source.Read(Header, SizeOf(Header));

  If Header.Format<>PCM_FORMAT Then
  Begin
    RaiseError('Wave format not supported.');
    ReleaseObject(Source);
    Exit;
  End;

  Source.Seek(Pos+Chunk.Size);

  If Not FindChunk(Source, DATA_ID, Chunk ) Then
  Begin
    RaiseError('Cannot find wave data chunk.');
    ReleaseObject(Source);
    Exit;
  End;

  If (Header.Bits<>16) Then
  Begin
    ReleaseObject(Source);
    RaiseError('Only 16bit samples supported');
    Exit;
  End;

  _Stereo := (Header.Channels=2);
  _Size := (Chunk.Size Div 2) Div Header.Channels;
  _Frequency     := Header.Frequency;
  GetMem(Samples, Chunk.Size);
  Source.Read(Samples^, Chunk.Size);

  GetMem(_Channels[0], _Size*2);
  If (_Stereo) Then
    GetMem(_Channels[1], _Size*2)
  Else
    _Channels[1] := Nil;

  Count := 0;
  I:=0;
  While (Count<_Size) Do
  Begin
    _Channels[0][Count] := Samples[I]; Inc(I);
    If (_Stereo) Then
    Begin
      _Channels[1][Count] := Samples[I]; Inc(I);
    End;

    Inc(Count);
  End;

  FreeMem(Samples);
  ReleaseObject(Source);
End;

Function Sample.FindNote:Byte;
Var
  S,R:PSingleArray;
  I,N,J:Integer;
  A,B:Single;
  Count:Integer;
  Down:Boolean;
  IP:Integer;
Begin
  N := NearestPowerOfTwo(_Size);
  If (N>_Size) Then
    N := N Div 2;
  GetMem(S, N*4);
  For I:=0 To Pred(N) Do
    S[I] := _Channels[0][I]/32767;
  GetMem(R, N * 2);
  PowerSpectrum(N, S, R);

  Down := False;
  A := R[0];
  For I:=1 To (N Div 2)-2 Do
  Begin
    B := R[I];
    If (B<A) Then
    Begin
      If Not Down Then
      Begin
        IP := I;
        Down := True;
        Count := 0;
      End Else
        Inc(Count);
    End Else
      Down := False;

    If (Count>30) Then
    Begin
      Result := 0;
      For J:=1 To Pred(MaxNote) Do
      If (Abs(IP-Frequencies[J])<Abs(IP-Frequencies[Result])) Then
        Result := J;
      Break;
    End;
  End;

  FreeMem(S);
  FreeMem(R);
End;

Procedure Sample.FindLoopShort;
Type
  LoopPair = Record
    A,B:Integer;
  End;
Var
  N:SmallInt;
  I,J:Integer;
  Pairs:Array Of LoopPair;
  PairCount:Integer;
Begin
  _Loop := False;
  PairCount := 0;
  I := Pred(_Size);
  Repeat
    N := _Channels[0][I];
    For J:=(I-2) DownTo 0 Do
    If (Abs(_Channels[0][J] - N)<=2) Then
    Begin
      Inc(PairCount);
      SetLength(Pairs, PairCount);
      Pairs[Pred(PairCount)].A := J;
      Pairs[Pred(PairCount)].B := I;
      Break;
    End;

    Dec(I);
  Until (I<2);

  J := 0;
  For I:=0 To Pred(PairCount) Do
  If (Pairs[I].B>J) Then
  Begin
    _loopStart := Pairs[I].A;
    _loopEnd := Pairs[I].B;
    _Loop := True;
    J := Pairs[I].B;
  End;
End;

Procedure Sample.FindLoopWide;
Type
  LoopPair = Record
    A,B:Integer;
  End;
Var
  N:SmallInt;
  I,J:Integer;
  Pairs:Array Of LoopPair;
  PairCount:Integer;
Begin
  _Loop := False;
  PairCount := 0;
  I := Pred(_Size);
  Repeat
    N := _Channels[0][I];
    For J:=(I-2) DownTo 0 Do
    If (Abs(_Channels[0][J] - N)<=2) Then
    Begin
      Inc(PairCount);
      SetLength(Pairs, PairCount);
      Pairs[Pred(PairCount)].A := J;
      Pairs[Pred(PairCount)].B := I;
      Break;
    End;

    Dec(I);
  Until (I<2);

  J := 0;
  For I:=1 To Pred(PairCount) Do
  If (Pairs[I].B-Pairs[I].A>Pairs[J].B-Pairs[J].A) Then
  Begin
    _loopStart := Pairs[I].A;
    _loopEnd := Pairs[I].B;
    _Loop := True;
    J := I;
  End;
End;

Function Sample.Save(Dest:Stream):Boolean;
Begin
  Dest.Write(_BaseNote,1);
  Dest.Write(_MinNote,1);
  Dest.Write(_MaxNote,1);
  Dest.Write(_MinVelocity, 1);
  Dest.Write(_MaxVelocity, 1);
  Dest.Write(_Pan,1);
  Dest.Write(_Size, 4);
  Dest.Write(_Stereo, 1);
  Dest.Write(_Loop, 1);
  Dest.Write(_LoopStart, 4);
  Dest.Write(_LoopEnd, 4);
  Dest.Write(_Frequency, 4);
  Dest.Write(_Channels[0]^, _Size*2);
  If (_Stereo) Then
    Dest.Write(_Channels[1]^, _Size*2);
  Result := True;
End;
{$ENDIF}

Function Sample.Load(Source: Stream): Boolean;
Begin
  Source.Read(_BaseNote,1);
  Source.Read(_MinNote,1);
  Source.Read(_MaxNote,1);
  Source.Read(_MinVelocity, 1);
  Source.Read(_MaxVelocity, 1);
  Source.Read(_Pan,1);
  Source.Read(_Size, 4);
  Source.Read(_Stereo, 1);
  Source.Read(_Loop, 1);
  Source.Read(_LoopStart, 4);
  Source.Read(_LoopEnd, 4);
  Source.Read(_Frequency, 4);
  GetMem(_Channels[0], _Size*2);
  Source.Read(_Channels[0]^, _Size*2);
  If (_Stereo) Then
  Begin
    GetMem(_Channels[1], _Size*2);
    Source.Read(_Channels[1]^, _Size*2);
  End Else
    _Channels[1] := Nil;

  If Not _Loop Then
    _LoopEnd := _Size;
  If _MaxVelocity = 0 Then
    _MaxVelocity := 127;
  Result := True;
End;

Procedure Sample.Release;
Var
  I:Integer;
Begin
  For I:=0 To 1 Do
  If Assigned(_Channels[I]) Then
    FreeMem(_Channels[I]);
End;

{$IFDEF CUBIC_INTERPOLATION}
Function Sample.GetSampleAt(X:Single; Channel:Integer):SmallInt;
Var
  Y0,Y1,Y2,Y3:SmallInt;
  A0,A1,A2,A3:SmallInt;
  N:Integer;
  Mu,Mu2:Single;
Begin
  Mu := Frac(X);
  Mu2 := Sqr(Mu);

  N := Trunc(X);
  If (N>0) Then
    Y0 := _Channels[Channel][N-1]
  Else
    Y0 := _Channels[Channel][0];

  Y1 := _Channels[Channel][N];
  N := N +1;
  If (N>=_LoopEnd) Then
    N := _LoopStart;
  Y2 := _Channels[Channel][N];
  N := N +1;
  If (N>=_LoopEnd) Then
    N := _LoopStart;
  Y3 := _Channels[Channel][N];

  a0 := y3 - y2 - y0 + y1;
  a1 := y0 - y1 - a0;
  a2 := y2 - y0;
  a3 := y1;

  Result := Integer(Trunc(a0*mu*mu2+a1*mu2+a2*mu+a3));
End;
{$ELSE}
Function Sample.GetSampleAt(X:Single; Channel:Integer):SmallInt;
Var
  A,B:SmallInt;
  N:Integer;
  Delta:Single;
Begin
  Delta := Frac(X);
  N := Trunc(X);
  A := _Channels[Channel][N];
  N := N +1;
  If (N>=_LoopEnd) Then
    N := _LoopStart;
  B := _Channels[Channel][N];
  Result := Trunc((A*(1.0-Delta)) + (B*Delta));
End;
{$ENDIF}

Procedure Sample.Play(Note:Byte; Frequency, Start, Samples, MaxSamples, Offset: Cardinal; Dest: PSmallInt; Pan,Volume:Integer; IgnorePitchChange:Boolean; FadeOutMode:Byte);
Var
  Dx, V, Rp, LP, S, Pos:Single;
  N:SmallInt;
  FadeOutSamples:Integer;
Begin
  If IgnorePitchChange Then
    Dx := 1.0
  Else
    Dx := Frequencies[Note] /Frequencies[_BaseNote];
  Dx := Dx * (Self._Frequency /Frequency);

  If (FadeOutMode And 1)<>0 Then
    FadeOutSamples := Trunc(Samples-_LoopEnd)
  Else
    FadeOutSamples := -1;

  Pan := Pan + Self._Pan;
  If (Pan>255) Then Pan := 255;
  If (Pan<0) Then Pan := 0;

  V := (Pan-127)/256;
  RP := 0.5 + V;
  LP := 1.0 - RP;

  V := Volume/127;
  V := V * 0.25;

  Pos := 0;
  Inc(Dest, Start*2);
  Dec(MaxSamples, Samples);
  While (Samples>0) Do
  Begin
    If (Offset>0) Then
    Begin
      Dec(Offset);
    End Else
    Begin
      If (Samples<FadeOutSamples) Then
        S := Samples/FadeOutSamples
      Else
        S := 1.0;
      S := S * V;

      N := GetSampleAt(Pos, 0);
      Dest^ := (Dest^ ) + (Trunc(N*S*LP)); Inc(Dest); // left channel

      If (_Stereo) Then
        N := GetSampleAt(Pos, 1);
      Dest^ := (Dest^) + (Trunc(N*S*RP)); Inc(Dest); // right channel
    End;

    Pos := Pos + Dx;
    If (Pos>=_LoopEnd) And (Pos+(Samples*Dx)>_Size) Then
    Begin
      If (_Loop) Then
        Pos := Pos - (_LoopEnd-_LoopStart)
      Else
        Exit;
    End;

    If (Offset=0) Then
      Dec(Samples);
  End;

  If (FadeOutMode And 2)=0 Then
    Exit;

{  While(Pos<_Size) And (MaxSamples>0) Do
  Begin
    N := GetSampleAt(Pos, 0);
    Dest^ := (Dest^ ) + (Trunc(N*V*LP)); Inc(Dest); // left channel

    If (_Stereo) Then
      N := GetSampleAt(Pos, 1);
    Dest^ := (Dest^) + (Trunc(N*V*RP)); Inc(Dest); // right channel

    Pos := Pos + Dx;
    Dec(MaxSamples);
  End;}
End;

Function Instrument.Load(Source:Stream):Boolean;
Var
  I:Integer;
Begin
  Source.Read(_midiID, 1);
  Source.Read(_Percussion, 1);
  Source.Read(_FadeOutMode, 1);
  Source.Read(_SampleCount, 4);
  SetLength(_Samples, _SampleCount);
  For I:=0 To Pred(_SampleCount) Do
  Begin
    _Samples[I] := Sample.Create;
    _Samples[I].Load(Source);
    RemapSample(I);
  End;
  Result := True;
End;

Procedure Instrument.Play(Note: Byte; Frequency, Start, Samples, MaxSamples, Offset: Cardinal; Dest: PSmallInt; Pan, Volume: Integer);
Var
  S:Sample;
Begin
  Self._Time := GetTime;
  S := _Mapping[Note];
  If Assigned(S) Then
    S.Play(Note, Frequency, Start, Samples, MaxSamples, Offset, Dest, Pan,Volume, _Percussion, _FadeOutMode)
  Else
  Begin
    If (_Percussion) Then
    Begin
      Log(logWarning, 'Instrument', IntToString(Note)+' not found in '+ Self.Name);
    End Else
      Log(logWarning, 'Instrument', GetNoteName(Note) + ' not found in ' + _Name);
  End;
End;

Procedure Instrument.RemapSample(Index:Integer);
Var
  I:Integer;
Begin
  For I:=_Samples[Index]._MinNote To _Samples[Index]._MaxNote Do
    _Mapping[I] := _Samples[Index];
End;

{$IFDEF EDITOR}
Function Instrument.AddSample(S:Sample):Integer;
Begin
  Result := _SampleCount;
  Inc(_SampleCount);
  SetLength(_Samples, _SampleCount);
  _Samples[Pred(_SampleCount)] := S;
End;

Procedure Instrument.DeleteSample(Index:Integer);
Begin
  ReleaseObject(_Samples[Index]);
  _Samples[Index] := _Samples[Pred(_SampleCount)];
  Dec(_SampleCount);
End;

Function Instrument.GetSample(Index:Integer):Sample;
Begin
  If (Index>=0) And (Index<_SampleCount) Then
    Result := _Samples[Index]
  Else
    Result := Nil;
End;

Constructor Instrument.New;
Var
  I:Integer;
Begin
  Self._SampleCount := 0;
  For I:=0 To Pred(MaxNote) Do
    _Mapping[I] := Nil;
End;

Procedure Instrument.Save(FileName:TERRAString);
Var
  I:Integer;
  Stream:FileStream;
Begin
  Stream := FileStream.Create(FileName);
  Stream.Write(_midiID, 1);
  Stream.Write(_Percussion, 1);
  Stream.Write(_FadeOutMode, 1);
  Stream.Write(_SampleCount, 4);
  For I:=0 To Pred(_SampleCount) Do
    _Samples[I].Save(Stream);
  ReleaseObject(Stream);
End;

{$ENDIF}

Function Instrument.Unload: Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SampleCount) Do
    ReleaseObject(_Samples[I]);
  _SampleCount := 0;
  Result := True;
End;

Function Instrument.Update:Boolean;
Begin
  Inherited;
  Result := True;
End;

Function Instrument.GetSize: Cardinal;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_SampleCount) Do
  Begin
    Inc(Result, _Samples[I]._Size * 2);
    If _Samples[I]._Stereo Then
      Inc(Result, _Samples[I]._Size * 2);
  End;
End;

Class Function Instrument.GetManager: Pointer;
Begin
  Result := InstrumentManager.Instance;
End;

End.
