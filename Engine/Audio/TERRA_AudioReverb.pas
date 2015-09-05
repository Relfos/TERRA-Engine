//https://github.com/mono/opentk/blob/master/Source/OpenTK/Audio/OpenAL/AL/EffectsExtensionPresets.cs

Unit TERRA_AudioReverb;

Interface
{$I terra.inc}

Uses TERRA_Utils, TERRA_Vector3D, TERRA_AudioBuffer, TERRA_AudioPanning, TERRA_SoundAmbience;

Const
  OUTPUT_CHANNELS  = 2;
  REVERB_BUFFERSIZE = 2048;

Type
  ALfilterType = (
    // EFX-style low-pass filter, specifying a gain and reference frequency.
    ALfilterType_HighShelf,
    // EFX-style high-pass filter, specifying a gain and reference frequency.
    ALfilterType_LowShelf
  );

  FilterState = Object
    x:Array[0..1] Of Single; // History of two last input samples
    y:Array[0..1] Of Single; // History of two last output samples
    a:Array[0..2] Of Single; // Transfer function coefficients "a"
    b:Array[0..2] Of Single; // Transfer function coefficients "b"

    Function Process(Const sample:Single):Single;
    Procedure SetParams(FilterType:ALfilterType; gain, freq_mult, bandwidth:Single);
    Procedure Clear();
  End;

  DelayLine = Record
    // The delay lines use sample lengths that are powers of 2 to allow the
    // use of bit-masking instead of a modulus for wrapping.
    Mask:Cardinal;
    Line:PSingleArray;
    Offset:Integer;
  End;

  Modulator = Record
    // Modulator delay line.
    Delay:DelayLine;

    // The vibrato time is tracked with an index over a modulus-wrapped range (in samples).
    Index:Integer;
    Range:Integer;

    // The depth of frequency change (also in samples) and its filter.
    Depth:Single;
    Coeff:Single;
    Filter:Single;
  End;

  AudioEarlyReflection = Record
        // Output gain for early reflections.
        Gain:Single;

        // Early reflections are done with 4 delay lines.
        Coeff:Array[0..3] Of Single;
        Delay:Array[0..3] Of DelayLine;
        Offset:Array[0..3] Of Integer;

        // The gain for each output channel based on 3D panning (only for the EAX path).
        PanGain:MixingAudioSample;
  End;


  AudioLateReverb = Record
        // Output gain for late reverb.
        Gain:Single;

        // Attenuation to compensate for the modal density and decay rate of
        // the late lines.
        DensityGain:Single;

        // The feed-back and feed-forward all-pass coefficient.
        ApFeedCoeff:Single;

        // Mixing matrix coefficient.
        MixCoeff:Single;

        // Late reverb has 4 parallel all-pass filters.
        ApCoeff:Array[0..3] Of Single;
        ApDelay:Array[0..3] Of DelayLine;
        ApOffset:Array[0..3] Of Integer;

        // In addition to 4 cyclical delay lines.
        Coeff:Array[0..3] Of Single;
        Delay:Array[0..3] Of DelayLine;
        Offset:Array[0..3] Of Integer;

        // The cyclical delay lines are 1-pole low-pass filtered.
        LpCoeff:Array[0..3] Of Single;
        LpSample:Array[0..3] Of Single;

        // The gain for each output channel based on 3D panning
        PanGain:MixingAudioSample;
  End;

  ReverbEcho = Record
        // Attenuation to compensate for the modal density and decay rate of
        // the echo line.
        DensityGain:Single;

        // Echo delay and all-pass lines.
        Delay:DelayLine;
        ApDelay:DelayLine;

        Coeff:Single;
        ApFeedCoeff:Single;
        ApCoeff:Single;

        Offset:Integer;
        ApOffset:Integer;

        // The echo line is 1-pole low-pass filtered.
        LpCoeff:Single;
        LpSample:Single;

        // Echo mixing coefficients.
        MixCoeff:Array[0..1] Of Single;
  End;

  ReverbTapSamples = Array[0..3] Of Single;
  //PReverbTapSamples = ^ReverbTapSamples;

  AudioReverbEffect = Class(TERRAObject)
    Protected
      // All delay lines are allocated as a single buffer to reduce memory
      // fragmentation and management code.
      _SampleBuffer:Array Of Single;
      _TotalSamples:Integer;

      // Master effect filters
      _LpFilter:FilterState;
      _HpFilter:FilterState;

      _Mod:Modulator;

      // Initial effect delay.
      _Delay:DelayLine;

      // The tap points for the initial delay.  First tap goes to early reflections, the last to late reverb.
      _DelayTap:Array[0..1] Of Integer;

      _Early:AudioEarlyReflection;

      // Decorrelator delay line.
      _Decorrelator:DelayLine;

      // There are actually 4 decorrelator taps, but the first occurs at the initial sample.
      _DecoTap:Array[0..2] Of Integer;

      _Late:AudioLateReverb;

      _Echo:ReverbEcho;

      // The current read offset for all delay lines.
      _Offset:Integer;

      // Temporary storage used when processing, before deinterlacing.
      _ReverbSamples:Array[0..Pred(REVERB_BUFFERSIZE)] Of ReverbTapSamples;
      _EarlySamples:Array[0..Pred(REVERB_BUFFERSIZE)] Of ReverbTapSamples;

      Procedure AllocLines(frequency:Integer);

      Function EAXModulation(Const input:Single):Single;

      Function EarlyDelayLineOut(index:Integer):Single;
      Procedure EarlyReflection(Const input:Single; Var output:ReverbTapSamples);

      Function LateAllPassInOut(index:Integer; Const input:Single):Single;
      Function LateDelayLineOut(index:Integer):Single;

      Function LateLowPassInOut(index:Integer; input:Single):Single;
      Procedure LateReverb(Const input:ReverbTapSamples; Var output:ReverbTapSamples);

      Procedure EAXEcho(Const input:Single; Var late:ReverbTapSamples);
      Procedure EAXVerbPass(input:Single; Var early, late:ReverbTapSamples);

      Procedure UpdateDelayLine(earlyDelay, lateDelay:Single; frequency:Integer);
      Procedure UpdateModulator(modTime, modDepth:Single; frequency:Integer);
      Procedure UpdateEarlyLines(reverbGain, earlyGain, lateDelay:Single);
      Procedure UpdateDecorrelator(density:Single; frequency:Integer);
      Procedure UpdateLateLines(reverbGain, lateGain, xMix, density, decayTime, diffusion, hfRatio, cw:Single; frequency:Integer);
      Procedure UpdateEchoLine(reverbGain, lateGain, echoTime, decayTime, diffusion, echoDepth, hfRatio, cw:Single; frequency:Integer);

      Procedure Update3DPanning(Const ReflectionsPan, LateReverbPan:Vector3D; Const Gain:Single);

    Public
        Constructor Create(frequency:Integer);
        Procedure Release(); Override;

        Procedure Process(SamplesToDo:Integer; SamplesIn:PAudioSample; SamplesOut:PFloatAudioSampleArray);
        Procedure Update(frequency:Integer);

        Procedure LoadPreset(Const environment:Integer; Const environmentSize, environmentDiffusion:Single; Const room, roomHF, roomLF:Integer;
                        Const decayTime, decayHFRatio, decayLFRatio:Single;
                        Const reflections:Integer; Const reflectionsDelay:Single; Const reflectionsPan:Vector3D;
                        Const reverb:Integer; Const reverbDelay:Single; Const reverbPan:Vector3D;
                        Const echoTime, echoDepth, modulationTime, modulationDepth, airAbsorptionHF:Single;
                        Const hfReference, lfReference, roomRolloffFactor:Single);

  End;

Implementation
Uses TERRA_Math;

Const
  F_2PI  = 6.28318530717958647692;
  FLT_EPSILON = 1.19209290E-07;

  SPEEDOFSOUNDMETRESPERSEC = 343.3;

  GAIN_SILENCE_THRESHOLD  = 0.00001;

// This is a user config option for modifying the overall output of the reverb effect.
  ReverbBoost = 1.0;

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


(* This coefficient is used to define the maximum frequency range controlled
 * by the modulation depth.  The current value of 0.1 will allow it to swing
 * from 0.9x to 1.1x.  This value must be below 1.  At 1 it will cause the
 * sampler to stall on the downswing, and above 1 it will cause it to sample
 * backwards.
 *)
  MODULATION_DEPTH_COEFF = 0.1;

(* A filter is used to avoid the terrible distortion caused by changing
 * modulation time and/or depth.  To be consistent across different sample
 * rates, the coefficient must be raised to a constant divided by the sample
 * rate:  coeff^(constant / rate).
 *)
  MODULATION_FILTER_COEFF = 0.048;
  MODULATION_FILTER_CONST = 100000.0;

// When diffusion is above 0, an all-pass filter is used to take the edge off
// the echo effect.  It uses the following line length (in seconds).
  ECHO_ALLPASS_LENGTH = 0.0133;

// Input into the late reverb is decorrelated between four channels.  Their
// timings are dependent on a fraction and multiplier.  See the
// UpdateDecorrelator() routine for the calculations involved.
  DECO_FRACTION = 0.15;
  DECO_MULTIPLIER = 2.0;

// All delay line lengths are specified in seconds.

// The lengths of the early delay lines.
  EARLY_LINE_LENGTH:Array[0..3] Of Single  = (0.0015, 0.0045, 0.0135, 0.0405);

// The lengths of the late all-pass delay lines.
  ALLPASS_LINE_LENGTH:Array[0..3] Of Single = (0.0151, 0.0167, 0.0183, 0.0200);

// The lengths of the late cyclical delay lines.
  LATE_LINE_LENGTH:Array[0..3] Of Single = (0.0211, 0.0311, 0.0461, 0.0680);

// The late cyclical delay lines have a variable length dependent on the
// effect's density parameter (inverted for some reason) and this multiplier.
  LATE_LINE_MULTIPLIER = 4.0;

Function lerp(Const val1, val2, mu:Single):Single;
Begin
  Result := val1 + (val2-val1) * mu;
End;

Function eaxDbToAmp(Const eaxDb:Single):Single;
Begin
  Result := Power(10.0,  eaxDb / 2000.0);
End;

Function FilterState.Process(Const sample:Single):Single;
Begin
  Result := b[0] * sample + b[1] * x[0] + b[2] * x[1] - a[1] * y[0] - a[2] * y[1];
  x[1] := x[0];
  x[0] := sample;
  y[1] := y[0];
  y[0] := Result;
End;

Procedure FilterState.Clear();
Begin
  x[0] := 0.0;
  x[1] := 0.0;
  y[0] := 0.0;
  y[1] := 0.0;
End;

Procedure FilterState.SetParams(FilterType:ALfilterType; gain, freq_mult, bandwidth:Single);
Var
  alpha, w0:Single;
Begin
  // Limit gain to -100dB
  gain := FloatMax(gain, 0.00001);

  w0 := F_2PI * freq_mult;

  // Calculate filter coefficients depending on filter type
  Case (FilterType) Of
    ALfilterType_HighShelf:
      Begin
        alpha := Sin(w0)/2.0 * Sqrt((gain + 1.0/gain)*(1.0/0.75 - 1.0) + 2.0);
        b[0] :=       gain*((gain+1.0) + (gain-1.0)*cos(w0) + 2.0*sqrt(gain)*alpha);
        b[1] := -2.0*gain*((gain-1.0) + (gain+1.0)*cos(w0)                         );
        b[2] :=       gain*((gain+1.0) + (gain-1.0)*cos(w0) - 2.0*sqrt(gain)*alpha);
        a[0] :=             (gain+1.0) - (gain-1.0)*cos(w0) + 2.0*sqrt(gain)*alpha;
        a[1] :=  2.0*     ((gain-1.0) - (gain+1.0)*cos(w0)                         );
        a[2] :=             (gain+1.0) - (gain-1.0)*cos(w0) - 2.0*sqrt(gain)*alpha;
      End;

    ALfilterType_LowShelf:
      Begin
        alpha := sin(w0)/2.0*sqrt((gain + 1.0/gain)*(1.0/0.75 - 1.0) + 2.0);
        b[0] :=       gain*((gain+1.0) - (gain-1.0)*cos(w0) + 2.0*sqrt(gain)*alpha);
        b[1] :=  2.0*gain*((gain-1.0) - (gain+1.0)*cos(w0)                         );
        b[2] :=       gain*((gain+1.0) - (gain-1.0)*cos(w0) - 2.0*sqrt(gain)*alpha);
        a[0] :=             (gain+1.0) + (gain-1.0)*cos(w0) + 2.0*sqrt(gain)*alpha;
        a[1] := -2.0*     ((gain-1.0) + (gain+1.0)*cos(w0)                         );
        a[2] :=             (gain+1.0) + (gain-1.0)*cos(w0) - 2.0*sqrt(gain)*alpha;
      End;
  End;

  b[2] := b[2] / a[0];
  b[1] := b[1] / a[0];
  b[0] := b[0] / a[0];
  a[2] := a[2] / a[0];
  a[1] := a[1] / a[0];
  a[0] := a[0] / a[0];
End;

Procedure CalcMatrixCoeffs(Const diffusion:Single; Out x, y:Single);
Var
  n, t:Single;
Begin
  // The matrix is of order 4, so n is sqrt (4 - 1).
  N := sqrt(3.0);
  t := diffusion * Arctan(n);

  // Calculate the first mixing matrix coefficient.
  x := cos(t);

  // Calculate the second mixing matrix coefficient.
  y := sin(t) / n;
End;

// Calculate a decay coefficient given the length of each cycle and the time until the decay reaches -60 dB.
Function CalcDecayCoeff(length, decayTime:Single):Single;
Begin
  Result := Power(0.001{-60 dB}, length/decayTime);
End;

// Calculate an attenuation to be applied to the input of any echo models to compensate for modal density and decay time.
Function CalcDensityGain(a:Single):Single;
Begin
    (* The energy of a signal can be obtained by finding the area under the
     * squared signal.  This takes the form of Sum(x_n^2), where x is the
     * amplitude for the sample n.
     *
     * Decaying feedback matches exponential decay of the form Sum(a^n),
     * where a is the attenuation coefficient, and n is the sample.  The area
     * under this decay curve can be calculated as:  1 / (1 - a).
     *
     * Modifying the above equation to find the squared area under the curve
     * (for energy) yields:  1 / (1 - a^2).  Input attenuation can then be
     * calculated by inverting the square root of this approximation,
     * yielding:  1 / sqrt(1 / (1 - a^2)), simplified to: sqrt(1 - a^2).
     *)
  Result := Sqrt(1.0 - (a * a));
End;

// Calculate the coefficient for a HF (and eventually LF) decay damping filter.
Function CalcDampingCoeff(Const hfRatio, length, decayTime, decayCoeff, cw:Single):Single;
Var
  coeff, g:Single;
Begin
  // Eventually this should boost the high frequencies when the ratio exceeds 1.
  Coeff := 0.0;
  If (hfRatio < 1.0) Then
  Begin
    // Calculate the low-pass coefficient by dividing the HF decay coefficient by the full decay coefficient.
    g := CalcDecayCoeff(length, decayTime * hfRatio) / decayCoeff;

    // Damping is done with a 1-pole filter, so g needs to be squared.
    g := G * G;
    If (g < 0.9999) Then  // 1-epsilon
    Begin
      // Be careful with gains < 0.001, as that causes the coefficient head towards 1, which will flatten the signal.
      g := FloatMax(g, 0.001);
      coeff := (1 - g*cw - Sqrt(2*g*(1-cw) - g*g*(1 - cw*cw))) / (1 - g);
    End;

    // Very low decay times will produce minimal output, so apply an upper bound to the coefficient.
    coeff := FloatMin(coeff, 0.98);
  End;

  Result := coeff;
End;

// Update the EAX modulation index, range, and depth.

// Calculate a decay length from a coefficient and the time until the decay reaches -60 dB.
Function CalcDecayLength(coeff, decayTime:Single):Single;
Begin
  Result := log10(coeff) * decayTime / log10(0.001){-60 dB};
End;

// Calculate the limited HF ratio for use with the late reverb low-pass filters.
Function CalcLimitedHfRatio(Const hfRatio, airAbsorptionGainHF, decayTime:Single):Single;
Var
  limitRatio:Single;
Begin
    (* Find the attenuation due to air absorption in dB (converting delay
     * time to meters using the speed of sound).  Then reversing the decay
     * equation, solve for HF ratio.  The delay length is cancelled out of
     * the equation, so it can be calculated once for all lines.
     *)
  limitRatio := 1.0 / (CalcDecayLength(airAbsorptionGainHF, decayTime) * SPEEDOFSOUNDMETRESPERSEC);
  (* Using the limit calculated above, apply the upper bound to the HF
   * ratio. Also need to limit the result to a minimum of 0.1, just like the
   * HF ratio parameter. *)
  Result := FloatClamp(limitRatio, 0.1, hfRatio);
End;

// Basic delay line input/output routines.
Function DelayLineOut(Const Delay:DelayLine; Const offset:Integer):Single;
Begin
//  WriteLn('out :', Offset, ' ',Result:5:6);
  Result := Delay.Line[Offset And Delay.Mask];
End;

Procedure DelayLineIn(Const Delay:DelayLine; Const offset:Integer; Const Input:Single);
Begin
  //WriteLn('in  :', Input:5:6);
  Delay.Line[Offset And Delay.Mask] := Input;
End;

// Attenuated delay line output routine.
Function AttenuatedDelayLineOut(Const Delay:DelayLine; Const offset:Integer; Const coeff:Single):Single;
Begin
  Result := coeff * Delay.Line[Offset And Delay.Mask];
End;

// Basic attenuated all-pass input/output routine.
Function AllpassInOut(Const Delay:DelayLine; Const outOffset, inOffset:Integer;  Const input, feedCoeff, coeff:Single):Single;
Var
  Output, Feed:Single;
Begin
  Output := DelayLineOut(Delay, outOffset);
  Feed := feedCoeff * input;
  DelayLineIn(Delay, inOffset, (feedCoeff * (output - feed)) + input);

  // The time-based attenuation is only applied to the delay output to keep it from affecting the feed-back path (which is already controlled by the all-pass feed coefficient).
  Result := (coeff * output) - feed;
End;

// Calculate the length of a delay line and store its mask and offset.
Function CalcLineLength(Const length:Single; offset:Integer; frequency:Integer; Var Delay:DelayLine):Integer;
Begin
  // All line lengths are powers of 2, calculated from their lengths, with an additional sample in case of rounding errors.
  Result := NextPowerOfTwo(Trunc(length * frequency) + 1);

  // All lines share a single sample buffer.
  Delay.Mask := Result - 1;
  Delay.Offset := offset;
End;

// Given the allocated sample buffer, this function updates each delay line offset.
Procedure RealizeLineOffset(sampleBuffer:PSingleArray; Var Delay:DelayLine);
Begin
  Delay.Line := @(sampleBuffer[Delay.Offset]);
End;

{ AudioReverbEffect }
Constructor AudioReverbEffect.Create(frequency: Integer);
Var
  index:Integer;
Begin
  _LpFilter.Clear();
  _HpFilter.Clear();

  _Mod.Delay.Mask := 0;
  _Mod.Index := 0;
  _Mod.Range := 1;
  _Mod.Depth := 0.0;
  _Mod.Coeff := 0.0;
  _Mod.Filter := 0.0;

  _Delay.Mask := 0;
  _DelayTap[0] := 0;
  _DelayTap[1] := 0;

  _Early.Gain := 0.0;
  For Index :=0 To 3 Do
  Begin
    _Early.Coeff[index] := 0.0;
    _Early.Delay[index].Mask := 0;
    _Early.Offset[index] := 0;
  End;

  _Decorrelator.Mask := 0;
  _DecoTap[0] := 0;
  _DecoTap[1] := 0;
  _DecoTap[2] := 0;

  _Late.Gain := 0.0;
  _Late.DensityGain := 0.0;
  _Late.ApFeedCoeff := 0.0;
  _Late.MixCoeff := 0.0;
  For Index := 0 To 3 Do
  Begin
    _Late.ApCoeff[index] := 0.0;
    _Late.ApDelay[index].Mask := 0;
    _Late.ApOffset[index] := 0;

    _Late.Coeff[index] := 0.0;
    _Late.Delay[index].Mask := 0;
    _Late.Offset[index] := 0;

    _Late.LpCoeff[index] := 0.0;
    _Late.LpSample[index] := 0.0;
  End;

  _Early.PanGain.Left := 0.0;
  _Early.PanGain.Right := 0.0;
  _Late.PanGain.Left := 0.0;
  _Late.PanGain.Right := 0.0;

  _Echo.DensityGain := 0.0;
  _Echo.Delay.Mask := 0;
  _Echo.ApDelay.Mask := 0;
  _Echo.Coeff := 0.0;
  _Echo.ApFeedCoeff := 0.0;
  _Echo.ApCoeff := 0.0;
  _Echo.Offset := 0;
  _Echo.ApOffset := 0;
  _Echo.LpCoeff := 0.0;
  _Echo.LpSample := 0.0;
  _Echo.MixCoeff[0] := 0.0;
  _Echo.MixCoeff[1] := 0.0;

  _Offset := 0;

  _settings.Density   := EAXREVERB_DEFAULT_DENSITY;
  _settings.Diffusion := EAXREVERB_DEFAULT_DIFFUSION;
  _settings.Gain   := EAXREVERB_DEFAULT_GAIN;
  _settings.GainHF := EAXREVERB_DEFAULT_GAINHF;
  _settings.GainLF := EAXREVERB_DEFAULT_GAINLF;
  _settings.DecayTime    := EAXREVERB_DEFAULT_DECAY_TIME;
  _settings.DecayHFRatio := EAXREVERB_DEFAULT_DECAY_HFRATIO;
  _settings.DecayLFRatio := EAXREVERB_DEFAULT_DECAY_LFRATIO;
  _settings.ReflectionsGain   := EAXREVERB_DEFAULT_REFLECTIONS_GAIN;
  _settings.ReflectionsDelay  := EAXREVERB_DEFAULT_REFLECTIONS_DELAY;
  _settings.ReflectionsPan := VectorZero;
  _settings.LateReverbGain   := EAXREVERB_DEFAULT_LATE_REVERB_GAIN;
  _settings.LateReverbDelay  := EAXREVERB_DEFAULT_LATE_REVERB_DELAY;
  _settings.LateReverbPan := VectorZero;
  _settings.EchoTime  := EAXREVERB_DEFAULT_ECHO_TIME;
  _settings.EchoDepth := EAXREVERB_DEFAULT_ECHO_DEPTH;
  _settings.ModulationTime  := EAXREVERB_DEFAULT_MODULATION_TIME;
  _settings.ModulationDepth := EAXREVERB_DEFAULT_MODULATION_DEPTH;
  _settings.AirAbsorptionGainHF := EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF;
  _settings.HFReference := EAXREVERB_DEFAULT_HFREFERENCE;
  _settings.LFReference := EAXREVERB_DEFAULT_LFREFERENCE;
  _settings.RoomRolloffFactor := EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR;
  _settings.DecayHFLimit := EAXREVERB_DEFAULT_DECAY_HFLIMIT;

  // Allocate the delay lines.
  AllocLines(frequency);

  // Calculate the modulation filter coefficient.
  // Notice that the exponent is calculated given the current sample rate.
  // This ensures that the resulting filter response over time is consistent across all sample rates.
  _Mod.Coeff := Power(MODULATION_FILTER_COEFF, MODULATION_FILTER_CONST / frequency);

  // The early reflection and late all-pass filter line lengths are static, so their offsets only need to be calculated once.
  For Index:=0 To 3 Do
  Begin
    _Early.Offset[index] := Trunc(EARLY_LINE_LENGTH[index] * frequency);
    _Late.ApOffset[index] := Trunc(ALLPASS_LINE_LENGTH[index] * frequency);
  End;

  // The echo all-pass filter line length is static, so its offset only needs to be calculated once.
  _Echo.ApOffset := Trunc(ECHO_ALLPASS_LENGTH * frequency);
End;

Procedure AudioReverbEffect.AllocLines(frequency: Integer);
Var
  totalSamples, index:Integer;
  length:Single;
Begin
  // All delay line lengths are calculated to accomodate the full range of lengths given their respective paramters.
  totalSamples := 0;

  // The modulator's line length is calculated from the maximum modulation time and depth coefficient, and halfed for the low-to-high frequency swing.
  // An additional sample is added to keep it stable when there is no modulation.

  length := (EAXREVERB_MAX_MODULATION_TIME*MODULATION_DEPTH_COEFF * 0.5) + (1.0 / frequency);
  Inc(totalSamples, CalcLineLength(length, totalSamples, Frequency, Self._Mod.Delay));

  // The initial delay is the sum of the reflections and late reverb delays.
  length := EAXREVERB_MAX_REFLECTIONS_DELAY + EAXREVERB_MAX_LATE_REVERB_DELAY;
  Inc(totalSamples, CalcLineLength(length, totalSamples, Frequency, Self._Delay));

  // The early reflection lines.
  For Index := 0 To 3 Do
    Inc(totalSamples, CalcLineLength(EARLY_LINE_LENGTH[index], totalSamples, frequency, Self._Early.Delay[index]));

  // The decorrelator line is calculated from the lowest reverb density (a parameter value of 1).
  length := (DECO_FRACTION * DECO_MULTIPLIER * DECO_MULTIPLIER) * LATE_LINE_LENGTH[0] * (1.0 + LATE_LINE_MULTIPLIER);
  Inc(totalSamples, CalcLineLength(length, totalSamples, frequency, _Decorrelator));

  // The late all-pass lines.
  For Index := 0 To 3 Do
    Inc(totalSamples, CalcLineLength(ALLPASS_LINE_LENGTH[index], totalSamples, frequency, _Late.ApDelay[index]));

  // The late delay lines are calculated from the lowest reverb density.
  For Index := 0 To 3 Do
  Begin
    length := LATE_LINE_LENGTH[index] * (1.0 + LATE_LINE_MULTIPLIER);
    Inc(totalSamples, CalcLineLength(length, totalSamples, frequency, _Late.Delay[index]));
  End;

  // The echo all-pass and delay lines.
  Inc(totalSamples, CalcLineLength(ECHO_ALLPASS_LENGTH, totalSamples, frequency, _Echo.ApDelay));
  Inc(totalSamples, CalcLineLength(EAXREVERB_MAX_ECHO_TIME, totalSamples, frequency, _Echo.Delay));

  SetLength(_SampleBuffer, totalSamples);
  _TotalSamples := totalSamples;

  // Update all delays to reflect the new sample buffer.
  RealizeLineOffset(@_SampleBuffer[0], _Delay);
  RealizeLineOffset(@_SampleBuffer[0], _Decorrelator);
  For Index := 0 To 3 Do
  Begin
    RealizeLineOffset(@_SampleBuffer[0], _Early.Delay[index]);
    RealizeLineOffset(@_SampleBuffer[0], _Late.ApDelay[index]);
    RealizeLineOffset(@_SampleBuffer[0], _Late.Delay[index]);
  End;
  RealizeLineOffset(@_SampleBuffer[0], _Mod.Delay);
  RealizeLineOffset(@_SampleBuffer[0], _Echo.ApDelay);
  RealizeLineOffset(@_SampleBuffer[0], _Echo.Delay);

  // Clear the sample buffer.
  For Index := 0 To Pred(TotalSamples) Do
    _SampleBuffer[index] := 0.0;
End;

Procedure AudioReverbEffect.Release;
Begin
  SetLength(_SampleBuffer, 0);
End;

Procedure AudioReverbEffect.LoadPreset(Const environment:Integer; Const environmentSize, environmentDiffusion:Single; Const room, roomHF, roomLF:Integer;
                        Const decayTime, decayHFRatio, decayLFRatio:Single;
                        Const reflections:Integer; Const reflectionsDelay:Single; Const reflectionsPan:Vector3D;
                        Const reverb:Integer; Const reverbDelay:Single; Const reverbPan:Vector3D;
                        Const echoTime, echoDepth, modulationTime, modulationDepth, airAbsorptionHF:Single;
                        Const hfReference, lfReference, roomRolloffFactor:Single);
Begin
  _Settings.Density := 1.0; // todo, currently default
  _Settings.Diffusion := EnvironmentDiffusion;
  _Settings.Gain :=  eaxDbToAmp(room); //0.32f;
  _Settings.GainHF := eaxDbToAmp(roomHF); //0.89f;
  _Settings.GainLF := eaxDbToAmp(roomLF); // 1.0f;
  _Settings.DecayTime := decayTime;
  _Settings.DecayHFRatio := decayHFRatio;
  _Settings.DecayLFRatio := decayLFRatio;
  _Settings.ReflectionsGain := eaxDbToAmp(reflections); // 0.05f;
  _Settings.ReflectionsDelay := reflectionsDelay;
  _Settings.ReflectionsPan := reflectionsPan;
  _Settings.LateReverbGain := eaxDbToAmp(reverb); //1.26f;
  _Settings.LateReverbDelay := reverbDelay;
  _Settings.LateReverbPan := reverbPan;
  _Settings.EchoTime := echoTime;
  _Settings.EchoDepth := echoDepth;
  _Settings.ModulationTime := modulationTime;
  _Settings.ModulationDepth := modulationDepth;
  _Settings.AirAbsorptionGainHF := eaxDbToAmp(airAbsorptionHF); //0.995f;
  _Settings.HFReference := hfReference;
  _Settings.LFReference := lfReference;
  _Settings.RoomRolloffFactor := roomRolloffFactor;
  _Settings.DecayHFLimit := True;
End;

Procedure AudioReverbEffect.Update(frequency: Integer);
Var
  lfscale, hfscale, hfRatio:Single;
  cw, x, y:Single;
Begin
  // Calculate the master low-pass filter (from the master effect HF gain).
  hfscale := _settings.HFReference / frequency;
  _LpFilter.setParams(ALfilterType_HighShelf, _settings.GainHF, hfscale, 0.0);

  lfscale := _settings.LFReference / frequency;
  _HpFilter.setParams(ALfilterType_LowShelf, _settings.GainLF, lfscale, 0.0);

  // Update the modulator line.
  UpdateModulator(_settings.ModulationTime, _settings.ModulationDepth, frequency);

  // Update the initial effect delay.
  UpdateDelayLine(_settings.ReflectionsDelay, _settings.LateReverbDelay, frequency);

  // Update the early lines.
  UpdateEarlyLines(_settings.Gain, _settings.ReflectionsGain, _settings.LateReverbDelay);

  // Update the decorrelator.
  UpdateDecorrelator(_settings.Density, frequency);

  // Get the mixing matrix coefficients (x and y).
  CalcMatrixCoeffs(_settings.Diffusion, x, y);

  // Then divide x into y to simplify the matrix calculation.
  _Late.MixCoeff := y / x;

  // If the HF limit parameter is flagged, calculate an appropriate limit based on the air absorption parameter.
  hfRatio := _settings.DecayHFRatio;

  If (_settings.DecayHFLimit) And (_settings.AirAbsorptionGainHF < 1.0) Then
    hfRatio := CalcLimitedHfRatio(hfRatio, _settings.AirAbsorptionGainHF, _settings.DecayTime);

  cw := Cos(F_2PI * hfscale);
  // Update the late lines.
  UpdateLateLines(_settings.Gain, _settings.LateReverbGain, x, _settings.Density, _settings.DecayTime, _settings.Diffusion, hfRatio, cw, frequency);

  // Update the echo line.
  UpdateEchoLine(_settings.Gain, _settings.LateReverbGain, _settings.EchoTime, _settings.DecayTime, _settings.Diffusion, _settings.EchoDepth, hfRatio, cw, frequency);

  // Update early and late 3D panning.
  Update3DPanning(_settings.ReflectionsPan, _settings.LateReverbPan, ReverbBoost);
End;

Procedure AudioReverbEffect.EAXVerbPass(input:Single; Var early, late:ReverbTapSamples);
Var
  feed:Single;
  taps:ReverbTapSamples;
Begin
  //WriteLn('VERB');

  // Low-pass filter the incoming sample.
  Input := _LpFilter.Process(Input);
  //WriteLn(Input:5:6);

  Input := _HpFilter.Process(Input);

  //WriteLn(Input:5:6);

  // Perform any modulation on the input.
  Input := EAXModulation(Input);

  //WriteLn(Input:5:6);

  // Feed the initial delay line.
  DelayLineIn(_Delay, _Offset, Input);

  //WriteLn(Input:5:6);

//  WriteLn('A ',_DelayTap[0]);
  // Calculate the early reflection from the first delay tap.
  Input := DelayLineOut(_Delay, _Offset - _DelayTap[0]);
  EarlyReflection(Input, Early);

  //WriteLn(Input:5:6);

  //WriteLn('B ',_DelayTap[1]);
  // Feed the decorrelator from the energy-attenuated output of the second delay tap.
  Input := DelayLineOut(_Delay, _Offset - _DelayTap[1]);
  Feed := Input * _Late.DensityGain;
  DelayLineIn(_Decorrelator, _Offset, Feed);

  //WriteLn(Input:5:6);

  // Calculate the late reverb from the decorrelator taps.
  taps[0] := Feed;
  taps[1] := DelayLineOut(_Decorrelator, _Offset - _DecoTap[0]);
  taps[2] := DelayLineOut(_Decorrelator, _Offset - _DecoTap[1]);
  taps[3] := DelayLineOut(_Decorrelator, _Offset - _DecoTap[2]);
  LateReverb(taps, late);

  //WriteLn(Input:5:6);

  // Calculate and mix in any echo.
  EAXEcho(Input, Late);

  //WriteLn('');
  // Step all delays forward one sample.
  Inc(_Offset);
End;

Procedure AudioReverbEffect.Process(SamplesToDo: Integer; SamplesIn:PAudioSample; SamplesOut:PFloatAudioSampleArray);
Var
  Value:Single;
  index:Integer;
  earlyGain, lateGain:Single;
Begin
  // Process reverb for these samples.
  For index := 0 To Pred(SamplesToDo) Do
  Begin
    Value := (SamplesIn^) * SampleConvertFactor;
    Inc(SamplesIn);
    EAXVerbPass(Value, _EarlySamples[Index], _ReverbSamples[Index]);
  End;

  earlyGain := _Early.PanGain.Left;
  If (Abs(earlyGain) > GAIN_SILENCE_THRESHOLD) Then
  Begin
    For Index := 0 To Pred(SamplesToDo) Do
      SamplesOut[Index].Left := earlyGain * _EarlySamples[Index][0];
  End;

  lateGain := _Late.PanGain.Left;
  If(Abs(lateGain) > GAIN_SILENCE_THRESHOLD) Then
  Begin
    For Index := 0 To Pred(SamplesToDo) Do
      SamplesOut[index].Left := lateGain * _ReverbSamples[Index][0];
  End;

  earlyGain := _Early.PanGain.Right;
  If (Abs(earlyGain) > GAIN_SILENCE_THRESHOLD) Then
  Begin
    For Index := 0 To Pred(SamplesToDo) Do
    Begin
      SamplesOut[Index].Right := earlyGain * _EarlySamples[Index][1];
    End;
  End;

  lateGain := _Late.PanGain.Right;
  If(Abs(lateGain) > GAIN_SILENCE_THRESHOLD) Then
  Begin
    For Index := 0 To Pred(SamplesToDo) Do
    Begin
      SamplesOut[index].Right := lateGain * _ReverbSamples[Index][1];
    End;
  End;

End;


Function AudioReverbEffect.EarlyDelayLineOut(index: Integer): Single;
Begin
  Result := AttenuatedDelayLineOut(_Early.Delay[index], _Offset - _Early.Offset[index], _Early.Coeff[index]);
End;

// Given an input sample, this function produces four-channel output for the  early reflections.
Procedure AudioReverbEffect.EarlyReflection(Const input:Single; Var output:ReverbTapSamples);
Var
  V:Single;
  D,F:ReverbTapSamples;
Begin
  // Obtain the decayed results of each early delay line.
  d[0] := EarlyDelayLineOut(0);
  d[1] := EarlyDelayLineOut(1);
  d[2] := EarlyDelayLineOut(2);
  d[3] := EarlyDelayLineOut(3);

    (* The following uses a lossless scattering junction from waveguide
     * theory.  It actually amounts to a householder mixing matrix, which
     * will produce a maximally diffuse response, and means this can probably
     * be considered a simple feed-back delay network (FDN).
     *          N
     *         ---
     *         \
     * v = 2/N /   d_i
     *         ---
     *         i=1
     *)
  v := (d[0] + d[1] + d[2] + d[3]) * 0.5;
  // The junction is loaded with the input here.
  v := V + Input;

  // Calculate the feed values for the delay lines.
  f[0] := v - d[0];
  f[1] := v - d[1];
  f[2] := v - d[2];
  f[3] := v - d[3];

  // Re-feed the delay lines.
  DelayLineIn(_Early.Delay[0], _Offset, f[0]);
  DelayLineIn(_Early.Delay[1], _Offset, f[1]);
  DelayLineIn(_Early.Delay[2], _Offset, f[2]);
  DelayLineIn(_Early.Delay[3], _Offset, f[3]);

  // Output the results of the junction for all four channels.
  output[0] := _Early.Gain * f[0];
  output[1] := _Early.Gain * f[1];
  output[2] := _Early.Gain * f[2];
  output[3] := _Early.Gain * f[3];
End;

Procedure AudioReverbEffect.EAXEcho(const input: Single; Var late:ReverbTapSamples);
Var
  output, feed:Single;
Begin
  // Get the latest attenuated echo sample for output.
  feed := AttenuatedDelayLineOut(_Echo.Delay, _Offset - _Echo.Offset, _Echo.Coeff);

  // Mix the output into the late reverb channels.
  output := _Echo.MixCoeff[0] * feed;
  late[0] := (_Echo.MixCoeff[1] * late[0]) + output;
  late[1] := (_Echo.MixCoeff[1] * late[1]) + output;
  late[2] := (_Echo.MixCoeff[1] * late[2]) + output;
  late[3] := (_Echo.MixCoeff[1] * late[3]) + output;

  // Mix the energy-attenuated input with the output and pass it through the echo low-pass filter.
  Feed := Feed + _Echo.DensityGain * input;
  Feed := Lerp(feed, _Echo.LpSample, _Echo.LpCoeff);
  _Echo.LpSample := feed;

  // Then the echo all-pass filter.
  feed := AllpassInOut(_Echo.ApDelay, _Offset - _Echo.ApOffset, _Offset, feed, _Echo.ApFeedCoeff, _Echo.ApCoeff);

  // Feed the delay with the mixed and filtered sample.
  DelayLineIn(_Echo.Delay, _Offset, feed);
End;

// Given an input sample, this function produces modulation for the late reverb.
Function AudioReverbEffect.EAXModulation(const input: Single): Single;
Var
  sinus, frac:Single;
  offset:Integer;
  out0, out1:Single;
Begin
  // Calculate the sinus rythm (dependent on modulation time and the
  // sampling rate).  The center of the sinus is moved to reduce the delay of the effect when the time or depth are low.
  Sinus := 1.0 - Cos(F_2PI * _Mod.Index / _Mod.Range);

  // The depth determines the range over which to read the input samples from, so it must be filtered to reduce the distortion caused by even small parameter changes.
  _Mod.Filter := lerp(_Mod.Filter, _Mod.Depth, _Mod.Coeff);

  // Calculate the read offset and fraction between it and the next sample.
  frac := (1.0 + (_Mod.Filter * sinus));
  offset := Trunc(frac);
  frac := Frac - offset;

  // Get the two samples crossed by the offset, and feed the delay line with the next input sample.
  out0 := DelayLineOut(_Mod.Delay, _Offset - offset);
  out1 := DelayLineOut(_Mod.Delay, _Offset - offset - 1);
  DelayLineIn(_Mod.Delay, _Offset, input);

  // Step the modulation index forward, keeping it bound to its range.
  _Mod.Index := (_Mod.Index + 1) Mod _Mod.Range;

  // The output is obtained by linearly interpolating the two samples that were acquired above.
  Result := lerp(out0, out1, frac);
End;

// All-pass input/output routine for late reverb.
Function AudioReverbEffect.LateAllPassInOut(index: Integer; const input: Single): Single;
Begin
  Result := AllpassInOut(_Late.ApDelay[index], _Offset - _Late.ApOffset[index], _Offset, input, _Late.ApFeedCoeff, _Late.ApCoeff[index]);
End;

// Delay line output routine for late reverb.
Function AudioReverbEffect.LateDelayLineOut(index: Integer): Single;
Begin
  Result := AttenuatedDelayLineOut(_Late.Delay[index], _Offset - _Late.Offset[index], _Late.Coeff[index]);
End;

// Low-pass filter input/output routine for late reverb.
Function AudioReverbEffect.LateLowPassInOut(index:Integer; input:Single):Single;
Begin
  Input := lerp(input, _Late.LpSample[index], _Late.LpCoeff[index]);
  _Late.LpSample[index] := input;
  Result := input;
End;

// Given four decorrelated input samples, this function produces four-channel output for the late reverb.
Procedure AudioReverbEffect.LateReverb(Const input:ReverbTapSamples; Var output:ReverbTapSamples);
Var
  d, f:ReverbTapSamples;
Begin
  // Obtain the decayed results of the cyclical delay lines, and add the corresponding input channels.
  // Then pass the results through the low-pass filters.

  // This is where the feed-back cycles from line 0 to 1 to 3 to 2 and back to 0.
  d[0] := LateLowPassInOut(2, input[2] + LateDelayLineOut(2));
  d[1] := LateLowPassInOut(0, input[0] + LateDelayLineOut(0));
  d[2] := LateLowPassInOut(3, input[3] + LateDelayLineOut(3));
  d[3] := LateLowPassInOut(1, input[1] + LateDelayLineOut(1));

  // To help increase diffusion, run each line through an all-pass filter.
  // When there is no diffusion, the shortest all-pass filter will feed the shortest delay line.
  d[0] := LateAllPassInOut(0, d[0]);
  d[1] := LateAllPassInOut(1, d[1]);
  d[2] := LateAllPassInOut(2, d[2]);
  d[3] := LateAllPassInOut(3, d[3]);

    (* Late reverb is done with a modified feed-back delay network (FDN)
     * topology.  Four input lines are each fed through their own all-pass
     * filter and then into the mixing matrix.  The four outputs of the
     * mixing matrix are then cycled back to the inputs.  Each output feeds
     * a different input to form a circlular feed cycle.
     *
     * The mixing matrix used is a 4D skew-symmetric rotation matrix derived
     * using a single unitary rotational parameter:
     *
     *  [  d,  a,  b,  c ]          1 = a^2 + b^2 + c^2 + d^2
     *  [ -a,  d,  c, -b ]
     *  [ -b, -c,  d,  a ]
     *  [ -c,  b, -a,  d ]
     *
     * The rotation is constructed from the effect's diffusion parameter,
     * yielding:  1 = x^2 + 3 y^2; where a, b, and c are the coefficient y
     * with differing signs, and d is the coefficient x.  The matrix is thus:
     *
     *  [  x,  y, -y,  y ]          n = sqrt(matrix_order - 1)
     *  [ -y,  x,  y,  y ]          t = diffusion_parameter * atan(n)
     *  [  y, -y,  x,  y ]          x = cos(t)
     *  [ -y, -y, -y,  x ]          y = sin(t) / n
     *
     * To reduce the number of multiplies, the x coefficient is applied with
     * the cyclical delay line coefficients.  Thus only the y coefficient is
     * applied when mixing, and is modified to be:  y / x.
     *)
  f[0] := d[0] + (_Late.MixCoeff * (         d[1] + -d[2] + d[3]));
  f[1] := d[1] + (_Late.MixCoeff * (-d[0]         +  d[2] + d[3]));
  f[2] := d[2] + (_Late.MixCoeff * ( d[0] + -d[1]         + d[3]));
  f[3] := d[3] + (_Late.MixCoeff * (-d[0] + -d[1] + -d[2]       ));

  // Output the results of the matrix for all four channels, attenuated by the late reverb gain (which is attenuated by the 'x' mix coefficient).
  output[0] := _Late.Gain * f[0];
  output[1] := _Late.Gain * f[1];
  output[2] := _Late.Gain * f[2];
  output[3] := _Late.Gain * f[3];

  // Re-feed the cyclical delay lines.
  DelayLineIn(_Late.Delay[0], _Offset, f[0]);
  DelayLineIn(_Late.Delay[1], _Offset, f[1]);
  DelayLineIn(_Late.Delay[2], _Offset, f[2]);
  DelayLineIn(_Late.Delay[3], _Offset, f[3]);
End;

Procedure AudioReverbEffect.Update3DPanning(Const ReflectionsPan, LateReverbPan:Vector3D; Const Gain:Single);
Var
  earlyPan, latePan:Vector3D;
  I:Integer;
  length, invlen:Single;
  AmbientGains, DirGains:MixingAudioSample;
Begin
  earlyPan.X := ReflectionsPan.X;
  earlyPan.Y := ReflectionsPan.Y;
  earlyPan.Z := -ReflectionsPan.Z;

  latePan.X := LateReverbPan.X;
  latePan.X := LateReverbPan.Y;
  latePan.X := -LateReverbPan.Z;

  ComputeAmbientGains(1.4142, AmbientGains);

  length := earlyPan.LengthSquared;
  If (length <= FLT_EPSILON) Then
  Begin
    _Early.PanGain.Left := AmbientGains.Left * Gain;
    _Early.PanGain.Right := AmbientGains.Right * Gain;
  End Else
  Begin
    invlen := 1.0 / Sqrt(length);
    earlyPan.Scale(invlen);

    length := FloatMin(length, 1.0);
    ComputeDirectionalGains(earlyPan, 1.4142, DirGains);

    _Early.PanGain.Left := lerp(AmbientGains.Left, DirGains.Left, length) * Gain;
    _Early.PanGain.Right := lerp(AmbientGains.Right, DirGains.Right, length) * Gain;
  End;

  length := latePan.LengthSquared;
  If (length <= FLT_EPSILON) Then
  Begin
    _Late.PanGain.Left := AmbientGains.Left * Gain;
    _Late.PanGain.Right := AmbientGains.Right * Gain;
  End Else
  Begin
    invlen := 1.0 / Sqrt(length);
    latePan.Scale(invlen);

    length := FloatMin(length, 1.0);
    ComputeDirectionalGains(latePan, 1.4142, DirGains);

    _Late.PanGain.Left := lerp(AmbientGains.Left, DirGains.Left, length) * Gain;
    _Late.PanGain.Right := lerp(AmbientGains.Right, DirGains.Right, length) * Gain;
  End;
End;

Procedure AudioReverbEffect.UpdateDecorrelator(density:Single; Frequency:Integer);
Var
  index:Integer;
  length:Single;
Begin
    (* The late reverb inputs are decorrelated to smooth the reverb tail and
     * reduce harsh echos.  The first tap occurs immediately, while the
     * remaining taps are delayed by multiples of a fraction of the smallest
     * cyclical delay time.
     *
     * offset[index] = (FRACTION (MULTIPLIER^index)) smallest_delay
     *)

  For Index:=0 To 3 Do
  Begin
    length := (DECO_FRACTION * Power(DECO_MULTIPLIER, Index)) * LATE_LINE_LENGTH[0] * (1.0 + (density * LATE_LINE_MULTIPLIER));
    _DecoTap[index] := Trunc(length * frequency);
  End;
End;

// Update the offsets for the initial effect delay line.
Procedure AudioReverbEffect.UpdateDelayLine(earlyDelay, lateDelay: Single; frequency: Integer);
Begin
  // Calculate the initial delay taps.
  _DelayTap[0] := Trunc(earlyDelay * frequency);
  _DelayTap[1] := Trunc((earlyDelay + lateDelay) * frequency);
End;

// Update the early reflections gain and line coefficients.
Procedure AudioReverbEffect.UpdateEarlyLines(reverbGain, earlyGain, lateDelay: Single);
Var
  index:Integer;
Begin
    // Calculate the early reflections gain (from the master effect gain, and
    // reflections gain parameters) with a constant attenuation of 0.5.
    _Early.Gain := 0.5 * reverbGain * earlyGain;

    // Calculate the gain (coefficient) for each early delay line using the
    // late delay time.  This expands the early reflections to the start of the late reverb.
    For Index := 0 To 3 Do
      _Early.Coeff[index] := CalcDecayCoeff(EARLY_LINE_LENGTH[index], lateDelay);
End;

// Update the echo gain, line offset, line coefficients, and mixing coefficients.
Procedure AudioReverbEffect.UpdateEchoLine(reverbGain, lateGain, echoTime, decayTime, diffusion, echoDepth, hfRatio, cw: Single;  frequency: Integer);
Begin
  // Update the offset and coefficient for the echo delay line.
  _Echo.Offset := Trunc(echoTime * frequency);

  // Calculate the decay coefficient for the echo line.
  _Echo.Coeff := CalcDecayCoeff(echoTime, decayTime);

  // Calculate the energy-based attenuation coefficient for the echo delay line.
  _Echo.DensityGain := CalcDensityGain(_Echo.Coeff);

  // Calculate the echo all-pass feed coefficient.
  _Echo.ApFeedCoeff := 0.5 * Power(diffusion, 2.0);

  // Calculate the echo all-pass attenuation coefficient.
  _Echo.ApCoeff := CalcDecayCoeff(ECHO_ALLPASS_LENGTH, decayTime);

  // Calculate the damping coefficient for each low-pass filter.
  _Echo.LpCoeff := CalcDampingCoeff(hfRatio, echoTime, decayTime, _Echo.Coeff, cw);

    (* Calculate the echo mixing coefficients.  The first is applied to the
     * echo itself.  The second is used to attenuate the late reverb when
     * echo depth is high and diffusion is low, so the echo is slightly
     * stronger than the decorrelated echos in the reverb tail.
     *)
  _Echo.MixCoeff[0] := reverbGain * lateGain * echoDepth;
  _Echo.MixCoeff[1] := 1.0 - (echoDepth * 0.5 * (1.0 - diffusion));
End;

// Update the late reverb gains, line lengths, and line coefficients.
Procedure AudioReverbEffect.UpdateLateLines(reverbGain, lateGain, xMix, density, decayTime, diffusion, hfRatio, cw: Single; frequency: Integer);
Var
  length:Single;
  index:Integer;
Begin
  (* Calculate the late reverb gain (from the master effect gain, and late
     * reverb gain parameters).  Since the output is tapped prior to the
     * application of the next delay line coefficients, this gain needs to be
     * attenuated by the 'x' mixing matrix coefficient as well.
     *)
  _Late.Gain := reverbGain * lateGain * xMix;

   (* To compensate for changes in modal density and decay time of the late
     * reverb signal, the input is attenuated based on the maximal energy of
     * the outgoing signal.  This approximation is used to keep the apparent
     * energy of the signal equal for all ranges of density and decay time.
     *
     * The average length of the cyclcical delay lines is used to calculate
     * the attenuation coefficient.
     *)
  length := (LATE_LINE_LENGTH[0] + LATE_LINE_LENGTH[1] + LATE_LINE_LENGTH[2] + LATE_LINE_LENGTH[3]) / 4.0;
  length := Length * (1.0 + (density * LATE_LINE_MULTIPLIER));
  _Late.DensityGain := CalcDensityGain(CalcDecayCoeff(length, decayTime));

  // Calculate the all-pass feed-back and feed-forward coefficient.
  _Late.ApFeedCoeff := 0.5 * Power(diffusion, 2.0);

  For Index:=0 To 3 Do
  Begin
    // Calculate the gain (coefficient) for each all-pass line.
    _Late.ApCoeff[index] := CalcDecayCoeff(ALLPASS_LINE_LENGTH[index], decayTime);

    // Calculate the length (in seconds) of each cyclical delay line.
    length := LATE_LINE_LENGTH[index] * (1.0 + (density * LATE_LINE_MULTIPLIER));

    // Calculate the delay offset for each cyclical delay line.
    _Late.Offset[index] := Trunc(length * frequency);

    // Calculate the gain (coefficient) for each cyclical line.
    _Late.Coeff[index] := CalcDecayCoeff(length, decayTime);

    // Calculate the damping coefficient for each low-pass filter.
    _Late.LpCoeff[index] := CalcDampingCoeff(hfRatio, length, decayTime, _Late.Coeff[index], cw);

    // Attenuate the cyclical line coefficients by the mixing coefficient (x).
    _Late.Coeff[index] := _Late.Coeff[index] * xMix;
  End;
end;

// Update the EAX modulation index, range, and depth.  
// Keep in mind that this kind of vibrato is additive and not multiplicative as one may expect.  
// The downswing will sound stronger than the upswing.
Procedure AudioReverbEffect.UpdateModulator(modTime, modDepth: Single; frequency: Integer);
Var
  range:Integer;
  Temp:Int64;
Begin
    (* Modulation is calculated in two parts.
     *
     * The modulation time effects the sinus applied to the change in
     * frequency.  An index out of the current time range (both in samples)
     * is incremented each sample.  The range is bound to a reasonable
     * minimum (1 sample) and when the timing changes, the index is rescaled
     * to the new range (to keep the sinus consistent).
     *)
  range := Trunc(modTime*frequency);
  If (Range>1) Then
    Range := 1;

  Temp := _Mod.Index * range;
  Temp := Temp Div _Mod.Range;
  _Mod.Index := Temp;
  _Mod.Range := range;

    (* The modulation depth effects the amount of frequency change over the
     * range of the sinus.  It needs to be scaled by the modulation time so
     * that a given depth produces a consistent change in frequency over all
     * ranges of time.  Since the depth is applied to a sinus value, it needs
     * to be halfed once for the sinus range and again for the sinus swing
     * in time (half of it is spent decreasing the frequency, half is spent
     * increasing it).
     *)
  _Mod.Depth := modDepth * MODULATION_DEPTH_COEFF * modTime / 2.0 / 2.0 * frequency;
End;

End.
