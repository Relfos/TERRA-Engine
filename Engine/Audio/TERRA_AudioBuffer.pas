Unit TERRA_AudioBuffer;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils;

Type
  AudioSample = SmallInt;
  PAudioSample = ^AudioSample;

  MixingAudioSample = Object
    Left:Single;
    Right:Single;

    Procedure Add(Const Other:MixingAudioSample);
    Procedure Scale(Const Gain:MixingAudioSample);
    Procedure ScaleUniform(Const Gain:Single);
  End;
  PMixingAudioSample = ^MixingAudioSample;

  FloatAudioSampleArray = Array [0..64444] Of MixingAudioSample;
  PFloatAudioSampleArray = ^FloatAudioSampleArray;

Const
  MinSampleValue = -1.0;
  MaxSampleValue = 1.0;

  SampleConvertFactor = 1.0 / 32767.0;

Type
  StereoAudioSampleDouble = Record
    Left:Double;
    Right:Double;
  End;

  StereoAudioSample16 = packed record
    Left, Right : AudioSample;
  End;

  MonoAudioArray16 = Array[0..100000] Of AudioSample;
  PMonoAudioArray16 = ^MonoAudioArray16;

  StereoAudioArray16 = Array[0..100000] Of StereoAudioSample16;
  PStereoAudioArray16 = ^StereoAudioArray16;


  MonoAudioArrayDouble = Array[0..100000] Of Double;
  PMonoAudioArrayDouble = ^MonoAudioArrayDouble;

  StereoAudioArrayDouble = Array[0..100000] Of StereoAudioSampleDouble;
  PStereoAudioArrayDouble = ^StereoAudioArrayDouble;

  { TERRAAudioBuffer }
  TERRAAudioBuffer = Class(TERRAObject)
    Protected
      _Samples:Array Of AudioSample;
      _SampleCount:Cardinal;

      _Frequency:Cardinal;
      _Stereo:Boolean;

      _AllocatedSamples:Cardinal;
      _TotalSize:Cardinal;

      Function GetSamples: Pointer;

      Procedure AllocateSamples();

    Public
      Constructor Create(SampleCount, Frequency:Cardinal; Stereo:Boolean);
      Procedure Release(); Override;

      Function GetSampleAt(Offset, Channel:Cardinal):PAudioSample;

      // in milisseconds
      Function GetLength: Cardinal;

      Property Samples:Pointer Read GetSamples;
      Property SampleCount:Cardinal Read _SampleCount;
      Property Frequency:Cardinal Read _Frequency;
      Property Stereo:Boolean Read _Stereo;

      Property SizeInBytes:Cardinal Read _TotalSize;
  End;

  { TERRAAudioMixingBuffer }
  TERRAAudioMixingBuffer = Class(TERRAObject)
    Protected
      _Samples:Array Of MixingAudioSample;
      _SampleCount:Cardinal;

      _AllocatedSamples:Cardinal;

      _Frequency:Cardinal;

      _TotalSize:Cardinal;

      _NoiseReductionLeft:AudioSample;
      _NoiseReductionRight:AudioSample;

      Function GetSamples: Pointer;

      Procedure AllocateSamples();

      Procedure SetSampleCount(Const Count:Cardinal);

      Procedure MixMonoSamplesWithShifting(Const SrcOffset:Cardinal; Src:TERRAAudioBuffer; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const SampleIncr:Single; Const Volume:MixingAudioSample);
      Procedure MixStereoSamplesWithShifting(Const SrcOffset:Cardinal; Src:TERRAAudioBuffer; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const SampleIncr:Single; Const Volume:MixingAudioSample);

      Procedure MixSamplesDirectMonoToStereo(SrcBuffer:PAudioSample; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const Volume:MixingAudioSample);
      Procedure MixSamplesDirectStereoToStereo(SrcBuffer:PAudioSample; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const Volume:MixingAudioSample);

      Class Procedure MixIntoBuffer(Var DestBuffer:PMixingAudioSample; Const InSample:MixingAudioSample);

    Public
      Constructor Create(SampleCount, Frequency:Cardinal);
      Procedure Release(); Override;

      Function GetSampleAt(Offset:Cardinal):PMixingAudioSample;

      Procedure FillSamples(Offset, Count:Cardinal);
      Procedure ClearSamples();

      Function MixSamples(DestOffset:Cardinal; Src:TERRAAudioBuffer; SrcOffset, SampleTotalToCopy:Cardinal; Const Volume:MixingAudioSample):Cardinal;

      // in milisseconds
      Function GetLength: Cardinal;

      Property Samples:Pointer Read GetSamples;
      Property SampleCount:Cardinal Read _SampleCount Write SetSampleCount;
      Property Frequency:Cardinal Read _Frequency;

      Property SizeInBytes:Cardinal Read _TotalSize;
  End;

Implementation
Uses TERRA_Math;

{ MixingAudioSample }
Procedure MixingAudioSample.Add(const Other: MixingAudioSample);
Begin
  Self.Left := Self.Left + Other.Left;
  Self.Right := Self.Right + Other.Right;
End;

Procedure MixingAudioSample.Scale(const Gain: MixingAudioSample);
Begin
  Self.Left := Self.Left * Gain.Left;
  Self.Right := Self.Right * Gain.Right;
End;

Procedure MixingAudioSample.ScaleUniform(const Gain: Single);
Begin
  Self.Left := Self.Left * Gain;
  Self.Right := Self.Right * Gain;
End;

{ TERRAAudioBuffer }
Constructor TERRAAudioBuffer.Create(SampleCount, Frequency: Cardinal; Stereo: Boolean);
Begin
  Self._Frequency := Frequency;
  Self._Stereo := Stereo;

  _SampleCount := SampleCount;
  Self.AllocateSamples();
End;

Procedure TERRAAudioBuffer.AllocateSamples;
Var
  TargetLen:Integer;
Begin
  TargetLen := _SampleCount;
  If Self.Stereo Then
    TargetLen := TargetLen * 2;

  If _AllocatedSamples = 0 Then
    _AllocatedSamples := TargetLen;

  While _AllocatedSamples<TargetLen Do
    _AllocatedSamples := _AllocatedSamples * 2;

  _TotalSize := _SampleCount * SizeOf(AudioSample);
  If Self.Stereo Then
    _TotalSize := _TotalSize * 2;


  SetLength(_Samples, _AllocatedSamples);
End;

procedure TERRAAudioBuffer.Release;
Begin
  If Assigned(_Samples) Then
  Begin
    SetLength(_Samples, 0);
    _Samples := Nil;
  End;
End;

function TERRAAudioBuffer.GetLength: Cardinal;
Begin
  Result := Trunc((_SampleCount*1000)/ _Frequency);
End;

(*Function Sound.GetBufferSize(Length,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
Begin
  Result := Round((Length/1000)*Frequency*Self.SampleSize*Self.Channels);
End;*)

function TERRAAudioBuffer.GetSamples: Pointer;
Begin
  Result := @(_Samples[0]);
End;

function TERRAAudioBuffer.GetSampleAt(Offset, Channel: Cardinal): PAudioSample;
Begin
  If (Offset >= _AllocatedSamples) Then
    Self.AllocateSamples();

  If (Offset >= _SampleCount) Then
    _SampleCount := Succ(Offset);

  If _Stereo Then
    Offset := Offset * 2 + Channel;

   Result := @(_Samples[Offset]);
End;


{ TERRAAudioMixingBuffer }
Constructor TERRAAudioMixingBuffer.Create(SampleCount, Frequency: Cardinal);
Begin
  Self._Frequency := Frequency;

  _SampleCount := SampleCount;
  Self.AllocateSamples();

  Self.ClearSamples();
End;

procedure TERRAAudioMixingBuffer.AllocateSamples;
Var
  TargetLen:Integer;
Begin
  TargetLen := _SampleCount;

  If _AllocatedSamples = 0 Then
    _AllocatedSamples := TargetLen;

  While _AllocatedSamples<TargetLen Do
    _AllocatedSamples := _AllocatedSamples * 2;

  _TotalSize := _SampleCount * SizeOf(MixingAudioSample);

  SetLength(_Samples, _AllocatedSamples);
End;

Procedure TERRAAudioMixingBuffer.SetSampleCount(const Count: Cardinal);
Begin
  While (Count > Self._SampleCount) Do
    Self.AllocateSamples();

  _SampleCount := Count;
End;

Procedure TERRAAudioMixingBuffer.FillSamples(Offset, Count:Cardinal);
Var
  I:Integer;
  DestSample:PMixingAudioSample;
Begin
  DestSample := Self.GetSampleAt(Offset);
  While Count>0 Do
  Begin
    DestSample.Left := 0.0;
    DestSample.Right := 0.0;
    Inc(DestSample);
    Dec(Count);
  End;
End;

Procedure TERRAAudioMixingBuffer.ClearSamples;
Begin
  FillSamples(0, _SampleCount);
End;

procedure TERRAAudioMixingBuffer.Release;
Begin
  If Assigned(_Samples) Then
  Begin
    SetLength(_Samples, 0);
    _Samples := Nil;
  End;
End;

function TERRAAudioMixingBuffer.GetLength: Cardinal;
Begin
  Result := Trunc((_SampleCount*1000)/ _Frequency);
End;

(*Function Sound.GetBufferSize(Length,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
Begin
  Result := Round((Length/1000)*Frequency*Self.SampleSize*Self.Channels);
End;*)

function TERRAAudioMixingBuffer.GetSamples: Pointer;
Begin
  Result := @(_Samples[0]);
End;

Function TERRAAudioMixingBuffer.GetSampleAt(Offset: Cardinal):PMixingAudioSample;
Begin
  If (Offset >= _AllocatedSamples) Then
    Self.AllocateSamples();

  If (Offset >= _SampleCount) Then
    _SampleCount := Succ(Offset);

  Result := @(_Samples[Offset]);
End;

Function TERRAAudioMixingBuffer.MixSamples(DestOffset:Cardinal; Src:TERRAAudioBuffer; SrcOffset, SampleTotalToCopy: Cardinal; Const Volume:MixingAudioSample): Cardinal;
Var
  SrcBuffer:PAudioSample;
  DestBuffer:PMixingAudioSample;
  SampleIncr:Single;
Begin
  If (Volume.Left<0.0) And (Volume.Right<=0.0) Then
    Exit;

  If (Src.Frequency < Self.Frequency) Then
  Begin
    SampleIncr := Src.Frequency / Self.Frequency;
    Result := Trunc(SampleTotalToCopy * SampleIncr);
  End Else
  Begin
    Result := SampleTotalToCopy;
    SampleIncr := 1.0;
  End;

  If (SrcOffset + Result > Src.SampleCount) Then
    SampleTotalToCopy := Src.SampleCount  - SrcOffset;

  If (DestOffset + Result > Self.SampleCount) Then
    SampleTotalToCopy := Self.SampleCount - DestOffset;

  DestBuffer := Self.GetSampleAt(DestOffset);

  If SampleIncr < 1.0 Then
  Begin
    If Src.Stereo Then
      Self.MixStereoSamplesWithShifting(SrcOffset, Src, DestBuffer, SampleTotalToCopy, SampleIncr, Volume)
    Else
      Self.MixMonoSamplesWithShifting(SrcOffset, Src, DestBuffer, SampleTotalToCopy, SampleIncr, Volume);
  End Else
  Begin
    SrcBuffer := Src.GetSampleAt(SrcOffset, 0);

    If Src.Stereo Then
      Self.MixSamplesDirectStereoToStereo(SrcBuffer, DestBuffer, SampleTotalToCopy, Volume)
    Else
      Self.MixSamplesDirectMonoToStereo(SrcBuffer, DestBuffer, SampleTotalToCopy, Volume);
  End;
End;

Class Procedure TERRAAudioMixingBuffer.MixIntoBuffer(Var DestBuffer:PMixingAudioSample; Const InSample:MixingAudioSample);
Var
  OutSample:MixingAudioSample;
Begin
  // left side
  OutSample.Left := FloatClamp(DestBuffer.Left + InSample.Left, MinSampleValue, MaxSampleValue);
  OutSample.Right := FloatClamp(DestBuffer.Right + InSample.Right, MinSampleValue, MaxSampleValue);

  DestBuffer^ := OutSample;
  Inc(DestBuffer);
End;

Procedure TERRAAudioMixingBuffer.MixSamplesDirectMonoToStereo(SrcBuffer:PAudioSample; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const Volume:MixingAudioSample);
Var
  InputValue:AudioSample;
  InSample:MixingAudioSample;
Begin
  While SampleTotalToCopy>0 Do
  Begin
    // read source sample
    InputValue := SrcBuffer^;
    Inc(SrcBuffer);

    InSample.Left := (InputValue * SampleConvertFactor * Volume.Left);
    InSample.Right := (InputValue * SampleConvertFactor * Volume.Right);

    MixIntoBuffer(DestBuffer, InSample);

    Dec(SampleTotalToCopy);
  End;
End;

Procedure TERRAAudioMixingBuffer.MixSamplesDirectStereoToStereo(SrcBuffer:PAudioSample; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const Volume:MixingAudioSample);
Var
  InputValue:AudioSample;
  InSample:MixingAudioSample;
Begin
  While SampleTotalToCopy>0 Do
  Begin
    InputValue := SrcBuffer^;
    Inc(SrcBuffer);

    InSample.Left := (InputValue * SampleConvertFactor * Volume.Left);

    InputValue := SrcBuffer^;
    Inc(SrcBuffer);
    InSample.Right := (InputValue * SampleConvertFactor * Volume.Right);

    MixIntoBuffer(DestBuffer, InSample);

    Dec(SampleTotalToCopy);
  End;
End;

Procedure TERRAAudioMixingBuffer.MixMonoSamplesWithShifting(Const SrcOffset:Cardinal; Src:TERRAAudioBuffer; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const SampleIncr:Single; Const Volume:MixingAudioSample);
Var
  InputValue:AudioSample;
  InSample:MixingAudioSample;

  CurrentSample:Single;
  TargetOffset:Cardinal;

  SrcData:PAudioSample;
  Delta:Single;
  SampleA, SampleB, Value:AudioSample;
Begin
  CurrentSample := SrcOffset;

  While SampleTotalToCopy>0 Do
  Begin
    TargetOffset := Trunc(CurrentSample);
    Delta := Frac(CurrentSample);
    SrcData := Src.GetSampleAt(TargetOffset, 0);

    SampleA := SrcData^;
    Inc(SrcData);
    SampleB := SrcData^;

    InSample.Left := Trunc(SampleA * (1.0 - Delta) + SampleB * Delta);
    InSample.Right := InSample.Left;

    InSample.Left := Trunc(InSample.Left * Volume.Left);
    InSample.Right := Trunc(InSample.Right * Volume.Right);

    MixIntoBuffer(DestBuffer, InSample);

    CurrentSample := CurrentSample + SampleIncr;
    Dec(SampleTotalToCopy);
  End;
End;

Procedure TERRAAudioMixingBuffer.MixStereoSamplesWithShifting(Const SrcOffset:Cardinal; Src:TERRAAudioBuffer; DestBuffer:PMixingAudioSample; SampleTotalToCopy:Cardinal; Const SampleIncr:Single; Const Volume:MixingAudioSample);
Var
  InputValue:AudioSample;
  InSample:MixingAudioSample;

  CurrentSample:Single;
  TargetOffset:Cardinal;

  SrcData:PAudioSample;
  Delta:Single;
  SampleA, SampleB, Value:AudioSample;
Begin
  CurrentSample := SrcOffset;

  While SampleTotalToCopy>0 Do
  Begin
    TargetOffset := Trunc(CurrentSample);
    Delta := Frac(CurrentSample);
    SrcData := Src.GetSampleAt(TargetOffset, 0);

    SampleA := SrcData^;
    Inc(SrcData, 2);
    SampleB := SrcData^;

    InSample.Left := Trunc(SampleA * (1.0 - Delta) + SampleB * Delta);

    Dec(SrcData);
    SampleA := SrcData^;
    Inc(SrcData, 2);
    SampleB := SrcData^;
    InSample.Right := Trunc(SampleA * (1.0 - Delta) + SampleB * Delta);

    InSample.Left := Trunc(InSample.Left * Volume.Left);
    InSample.Right := Trunc(InSample.Right * Volume.Right);

    MixIntoBuffer(DestBuffer, InSample);

    CurrentSample := CurrentSample + SampleIncr;
    Dec(SampleTotalToCopy);
  End;
End;

(*Function TERRAAudioMixingBuffer.CopySamples(DestOffset: Cardinal; Src: TERRAAudioMixingBuffer; SrcOffset, SampleTotalToCopy: Cardinal): Cardinal;
Var
  SrcBuffer, DestBuffer:PAudioSample;
  SrcSampleLeft, SrcSampleRight:AudioSample;
  CurrentValue:Single;
  CurrentSample, SampleIncr:Single;
Begin
  If (Src.Frequency <> Self.Frequency) Or (Src.Stereo <> Self.Stereo) Then
  Begin
    Self.FillSamples(DestOffset, SampleTotalToCopy);
    Result := Self.MixSamples(DestOffset, Src, SrcOffset, SampleTotalToCopy, 1.0, 1.0);
    Exit;
  End;

  If (SrcOffset + SampleTotalToCopy > Src.SampleCount) Then
    SampleTotalToCopy := Src.SampleCount  - SrcOffset;

  If (DestOffset + SampleTotalToCopy > Self.SampleCount) Then
    SampleTotalToCopy := Self.SampleCount - DestOffset;


  DestBuffer := Self.GetSampleAt(DestOffset, 0);
  SrcBuffer := Src.GetSampleAt(SrcOffset, 0);

  Result := SampleTotalToCopy;

  If (Self.Stereo) Then
    SampleTotalToCopy := SampleTotalToCopy Shl 1;

  Move(SrcBuffer^, DestBuffer^, SampleTotalToCopy * SizeOf(AudioSample));
End;*)


End.
