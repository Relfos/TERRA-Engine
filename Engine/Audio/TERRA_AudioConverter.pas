Unit TERRA_AudioConverter;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_AudioBuffer;

Type
  FilterWindowType = (fwHamming, fwHann, fwBlackman);

  AudioRateConverter = Class(TERRAObject)
    Protected
    _TargetSampleRate:Integer;
    _Remainder:Integer;
    InBufM, OutBufM:PMonoAudioArray16;
    InBufS, OutBufS:PStereoAudioArray16;

    DAM:Array of Double;
    DAS:Array of StereoAudioSampleDouble;
    _Kernel:Array of Double;
    _KernelWidth:Integer;
    _FilterWindow:FilterWindowType;
    _Tail:Pointer;
    _LBS:StereoAudioSample16;

    _Input:TERRAAudioBuffer;

    Function ConvertFreqs16Mono(): Integer;
    Function ConvertFreqs16Stereo(): Integer;

  Public
    Constructor Create(Input:TERRAAudioBuffer);
    Procedure Release(); override;

    Function Convert(OutSampleRate:Cardinal):TERRAAudioBuffer;
  End;

Implementation

Const
  TwoPi = 6.28318530718;

Procedure HannWindow(OutData:PMonoAudioArrayDouble; Width:Integer; Symmetric:Boolean);
Var
  i, n:Integer;
Begin
  If Symmetric Then
    n := Width-1
  Else
    n := Width;

  For i := 0 to Width-1 do
    OutData[i] := (1-Cos(TwoPi*i/n))/2;
End;

Procedure HammingWindow(OutData:PMonoAudioArrayDouble; Width:Integer; Symmetric:Boolean);
Var
  i, n:Integer;
Begin
  If Symmetric Then
    n := Width-1
  Else
    n := Width;

  For i := 0 to Width-1 Do
    OutData[i] := 0.54-0.46*Cos(TwoPi*i/n);
End;

Procedure BlackmanWindow(OutData:PMonoAudioArrayDouble; Width:Integer; Symmetric:Boolean);
Var
  i, n:Integer;
Begin
  If Symmetric Then
    n := Width-1
  Else
    n := Width;

  For i := 0 to Width-1 Do
    OutData[i] := 0.42-0.5*Cos(TwoPi*i/n) + 0.08*Cos(2*TwoPi*i/n);
End;

Procedure CalculateSincKernel(OutData:PMonoAudioArrayDouble; CutOff:Double; Width:Integer; WType:FilterWindowType);
Var
  i:Integer;
  S:Double;
  Window:Array Of Double;
Begin
  SetLength(Window, Width);
  Case WType of
    fwHamming : HammingWindow(@Window[0], Width, False);
    fwHann : HannWindow(@Window[0], Width, False);
    fwBlackman : BlackmanWindow(@Window[0], Width, False);
  End;

  S := 0;
  For i := 0 to Width-1 do
  Begin
    If i-(Width shr 1) <> 0 then
      OutData[i] := Sin(TwoPi*CutOff*(i-(Width shr 1)))/(i-(Width shr 1))*Window[i]
    Else
      OutData[i] := TwoPi*CutOff*Window[i];

    S := S + OutData[i];
  End;

  For i := 0 to Width-1 Do
    OutData[i] := OutData[i]/S;
End;

Constructor AudioRateConverter.Create(Input:TERRAAudioBuffer);
Begin
  _Input := Input;

  If (Input.SampleCount>2000) Then
    _KernelWidth := 30
  Else
    _KernelWidth := 2;
    
  _FilterWindow := fwBlackman;
End;

Procedure AudioRateConverter.Release();
Begin
  _Kernel := nil;
  DAS := nil;
  DAM := nil;
End;

Function AudioRateConverter.Convert(OutSampleRate:Cardinal):TERRAAudioBuffer;
Const
  BUF_SIZE = $100000;
Var
  Ratio:Single;
  TailSize, NewSize:Integer;
  L:Integer;
Begin
  _TargetSampleRate := OutSampleRate;

  Ratio := _TargetSampleRate/_Input.Frequency;

  If _Input.Stereo Then
  Begin
    If Ratio < 1.0 then
      TailSize := (_KernelWidth-1)*4
    Else
    Begin
      SetLength(DAS, (BUF_SIZE div 4)+ _KernelWidth);
      TailSize := (_KernelWidth-1)*16;
    End;
  End Else
  Begin
    If Ratio < 1.0 then
      TailSize := (_KernelWidth-1)*2
    Else
    Begin
      SetLength(DAM, (BUF_SIZE div 2)+ _KernelWidth);
      TailSize := (_KernelWidth-1)*8;
    End;

    FillChar(DAM[0], Length(DAM)*Sizeof(DAM[0]), 0);
  End;

  GetMem(_Tail, TailSize);
  FillChar(_Tail^, TailSize, 0);

  NewSize := Round(_Input.SampleCount * Ratio);
  Result := TERRAAudioBuffer.Create(NewSize, Self._TargetSampleRate, _Input.Stereo);
  _Remainder := -1;

  If Ratio > 1.0 Then
    Ratio := 1/Ratio;

  Ratio := Ratio*0.4;

  SetLength(_Kernel, _KernelWidth);
  CalculateSincKernel(@_Kernel[0], Ratio, _KernelWidth, _FilterWindow);

  If _Input.Stereo then
  Begin
    InBufS := _Input.Samples;
    OutBufS := Result.Samples;
    ConvertFreqs16Stereo();
  End Else
  Begin
    InBufM := _Input.Samples ;
    OutBufM := Result.Samples;
    ConvertFreqs16Mono();
  End;

  // clean up
  FreeMem(_Tail);
End;

Function AudioRateConverter.ConvertFreqs16Mono():Integer;
Var
  i, step, j, k, s, m : Integer;
  D : Double;
  TailMono:PMonoAudioArray16;
  TailMonoD:PMonoAudioArrayDouble;
Begin
  TailMono := _Tail;
  s := _Input.SampleCount Shr 1;

  If _Input.Frequency > _TargetSampleRate Then
  Begin
    step := _Input.Frequency - _TargetSampleRate;
    j := 0;
    If _Remainder < 0 Then
      _Remainder := _TargetSampleRate;

    For I:=0 To s - 1 Do
    Begin
      If _Remainder > _TargetSampleRate Then
        Dec(_Remainder, _TargetSampleRate)
      Else
      Begin
        D := 0;
        For k := 0 to _KernelWidth - 1 Do
        If i-k >= 0 Then
          D := D + InBufM[i-k] * _Kernel[_KernelWidth - 1 - k]
        Else
          D := D + TailMono[_KernelWidth-1+i-k]*_Kernel[_KernelWidth - 1 - k];

        OutBufM[j] := Round(D);
        Inc(j);
        Inc(_Remainder, step);
      End;
    End;

    For i := 0 to _KernelWidth-2 Do
      TailMono[i] := InBufM[i+s-_KernelWidth+1]
  End Else
  Begin
    TailMonoD := _Tail;
    FillChar(DAM[0], Length(DAM)*8, 0);
    For i := 0 to _KernelWidth-2 do
    Begin
      DAM[i] := TailMonoD[i];
      TailMonoD[i] := 0;
    End;
    Step := _Input.Frequency;
    j := 0;
    If _Remainder < 0 Then
      _Remainder := 0;

    While _Remainder < _TargetSampleRate Do
    Begin
      m := Round(((_TargetSampleRate - _Remainder)*_LBS.Left +  _Remainder*InBufM[0])/_TargetSampleRate);
      for k := 0 to _KernelWidth-1 do
        DAM[j+k] := DAM[j+k] + m * _Kernel[k];

      Inc(j);
      Inc(_Remainder, step);
    End;

    Dec(_Remainder, _TargetSampleRate);
    For i := 0 to s - 2 Do
    Begin
      while _Remainder < _TargetSampleRate do
      Begin
        m := Round(((_TargetSampleRate - _Remainder)*InBufM[i] +  _Remainder*InBufM[i+1])/_TargetSampleRate);
        for k := 0 to _KernelWidth-1 do
          DAM[j+k] := DAM[j+k] + m * _Kernel[k];

        Inc(j);
        Inc(_Remainder, step);
      End;

      Dec(_Remainder, _TargetSampleRate);
    End;

    _LBS.Left := InBufM[s-1];
    For i := 0 to j-1 do
      OutBufM[i] := Round(DAM[i]);

    For i := 0 to _KernelWidth-2 Do
      TailMonoD[i] := DAM[i+j];
  End;

  Result := j shl 1;
End;

Function AudioRateConverter.ConvertFreqs16Stereo():Integer;
Var
  i, step, j, k, s, m1, m2 : Integer;
  D1, D2 : Double;
  TailStereo:PStereoAudioArray16;
  TailStereoD:PStereoAudioArrayDouble;
Begin
  TailStereo := _Tail;
  s := _Input.SampleCount Shr 1;
  If _Input.Frequency > _TargetSampleRate Then
  Begin
    step := _Input.Frequency - _TargetSampleRate;
    j := 0;
    If _Remainder < 0 Then
      _Remainder := _TargetSampleRate;

    For i := 0 to s - 1 do
    Begin
      If _Remainder > _TargetSampleRate then
        Dec(_Remainder, _TargetSampleRate)
      Else
      Begin
        D1 := 0;
        D2 := 0;
        For k := 0 to _KernelWidth - 1 do
        If i-k >= 0 then
        Begin
          D1 := D1 + InBufS[i-k].Left * _Kernel[_KernelWidth - 1 - k];
          D2 := D2 + InBufS[i-k].Right * _Kernel[_KernelWidth - 1 - k];
        End Else
        begin
          D1 := D1 + TailStereo[_KernelWidth-1+i-k].Left * _Kernel[_KernelWidth - 1 - k];
          D2 := D2 + TailStereo[_KernelWidth-1+i-k].Right * _Kernel[_KernelWidth - 1 - k];
        end;

        OutBufS[j].Left := Round(D1);
        OutBufS[j].Right := Round(D2);
        Inc(j);
        Inc(_Remainder, step);
      end;
    end;

    for i := 0 to _KernelWidth-2 do
      TailStereo[i] := InBufS[i+s-_KernelWidth+1];
  End Else
  Begin
    TailStereoD := _Tail;
    FillChar(DAS[0], Length(DAS)*16, 0);
    For i := 0 to _KernelWidth-2 do
    begin
      DAS[i] := TailStereoD[i];
      TailStereoD[i].Left := 0;
      TailStereoD[i].Right := 0;
    End;

    Step := _Input.Frequency;
    j := 0;
    If _Remainder < 0 then
      _Remainder := 0;

    While _Remainder < _TargetSampleRate Do
    Begin
      m1 := Round(((_TargetSampleRate - _Remainder)*_LBS.Left +  _Remainder*InBufS[0].Left)/_TargetSampleRate);
      m2 := Round(((_TargetSampleRate - _Remainder)*_LBS.Right +  _Remainder*InBufS[0].Right)/_TargetSampleRate);
      for k := 0 to _KernelWidth-1 do
      Begin
        DAS[j+k].Left := DAS[j+k].Left + m1 * _Kernel[k]; //InBufS[i].Left*Kernel[k];
        DAS[j+k].Right := DAS[j+k].Right + m2 * _Kernel[k]; //InBufS[i].Right*Kernel[k];
      End;

      Inc(j);
      Inc(_Remainder, step);
    End;

    Dec(_Remainder, _TargetSampleRate);
    For i := 0 to s - 2 do
    Begin
      While _Remainder < _TargetSampleRate do
      Begin
        m1 := Round(((_TargetSampleRate - _Remainder)*InBufS[i].Left +  _Remainder*InBufS[i+1].Left)/_TargetSampleRate);
        m2 := Round(((_TargetSampleRate - _Remainder)*InBufS[i].Right +  _Remainder*InBufS[i+1].Right)/_TargetSampleRate);
        For k := 0 to _KernelWidth-1 do
        Begin
          DAS[j+k].Left := DAS[j+k].Left + m1 * _Kernel[k]; //InBufS[i].Left*Kernel[k];
          DAS[j+k].Right := DAS[j+k].Right + m2 * _Kernel[k]; //InBufS[i].Right*Kernel[k];
        End;

        Inc(j);
        Inc(_Remainder, step);
      End;

      Dec(_Remainder, _TargetSampleRate);
    End;

    _LBS := InBufS[s-1];
    For i := 0 to j-1 do
    begin
      OutBufS[i].Left := Round(DAS[i].Left);
      OutBufS[i].Right := Round(DAS[i].Right);
    End;

    For i := 0 to _KernelWidth-2 do
      TailStereoD[i] := DAS[i+j];
  End;

  Result := j shl 2;
End;

End.
