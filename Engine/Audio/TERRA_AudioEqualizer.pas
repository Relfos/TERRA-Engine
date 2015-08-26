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
 * TERRA_AudioEqualizer
 * Implements an AudioEqualizer
 ***********************************************************************************************************************
}

Unit TERRA_Equalizer;
{$I terra.inc}
Interface
Uses TERRA_Utils;

Const
  EqualizerBandCount = 10;

Type
  PEqualizerSettings = ^EqualizerSettings;
  EqualizerSettings = Array[0..Pred(EqualizerBandCount)] Of Single;

  PEqualizerCircularBuffer = ^EqualizerCircularBuffer;
  EqualizerCircularBuffer = Array[0..1] Of Single;
  Equalizer = Class(TERRAObject)
    Protected
      _a:Array[0..Pred(EqualizerBandCount)] Of EqualizerCircularBuffer;        // A weights
      _b:Array[0..Pred(EqualizerBandCount)] Of EqualizerCircularBuffer;	     	 // B weights
      _wq:Array[0..1, 0..Pred(EqualizerBandCount)] Of EqualizerCircularBuffer; // Circular buffer for W data
      _Gain:Array[0..1, 0..Pred(EqualizerBandCount)] Of Single;      	 // Gain factor for each channel and band
      _ActiveFilters:Cardinal;

    Public
      Constructor Create(Frequency:Cardinal; Settings:PEqualizerSettings = Nil);

      Procedure Process(Data:PSmallInt; Samples, Channels:Integer);
  End;

Implementation
Uses TERRA_Math, TERRA_Log;

{ Equalizer }

Const
  Q = 1.2247449; // Q value for band-pass filters 1.2247=(3/2)^(1/2) gives 4dB suppression @ Fc*2 and Fc/2 */

(* Center frequencies for band-pass filters
   The different frequency bands are:
   nr.    	center frequency
   0  	31.25 Hz
   1 	62.50 Hz
   2	125.0 Hz
   3	250.0 Hz
   4	500.0 Hz
   5	1.000 kHz
   6	2.000 kHz
   7	4.000 kHz
   8	8.000 kHz
   9 	16.00 kHz
*)
  CF : Array[0..9] Of Single = (31.25,62.5,125,250,500,1000,2000,4000,8000,16000);

// Maximum and minimum gain for the bands
  G_MAX = +12.0;
  G_MIN =	-12.0;


// 2nd order Band-pass Filter design
Procedure bp2(a,b:PEqualizerCircularBuffer; fc, q:Single);
Var
  Th,C:Double;
Begin
  th := 2.0 * PI * fc;
  C := (1.0 - tan(th*q/2.0))/(1.0 + tan(th*q/2.0));

  a[0] := (1.0 + C) * cos(th);
  a[1] := -1 * C;

  b[0] := (1.0 - C)/2.0;
  b[1] := -1.0050;
End;

Constructor Equalizer.Create(Frequency: Cardinal; Settings:PEqualizerSettings);
Var
  J, K:Integer;
  G:Single;
Begin
  // Calculate number of active filters
  _ActiveFilters := EqualizerBandCount;
  While (CF[Pred(_ActiveFilters)] > Frequency/2.2) Do
    Dec(_ActiveFilters);

  If(_ActiveFilters < EqualizerBandCount) Then
    Log(logDebug, 'Equalizer','Limiting the number of filters to ' + IntToString(_ActiveFilters) +' due to low sample rate.');

  // Generate filter taps
  For K:=0 To Pred(_ActiveFilters) Do
    bp2(@_a[k], @_b[k], CF[k]/Frequency, Q);

  // Calculate how much this plugin adds to the overall time delay
//    af->delay += 2000.0/((float)af->data->rate);

  // Set gain
  For K:=0 To Pred(EqualizerBandCount) Do
    For J:=0 To 1 Do
    Begin
      If Assigned(Settings) Then
        G := Settings[K]
      Else
        G := 6.0;
      G := FloatMax(G_MIN, FloatMin(G, G_MAX));
      _Gain[J, k] := power(10.0, G/20.0)-1.0; // clamp(gain[k],,G_MAX)
    End;
End;

Procedure Equalizer.Process(Data: PSmallInt; Samples, Channels: Integer);
Var
  CI,Count:Cardinal;
  _In,_Out:PSmallInt;
  K:Integer;
  W, Yt:Single;
  wq:^EqualizerCircularBuffer;
Begin
//  af_data_t*       c 	= data;			    	// Current working data
//  af_equalizer_t*  s 	= (af_equalizer_t*)af->setup; 	// Setup
  CI := 0;

  While (CI<Channels) Do
  Begin
    Count := Samples;
    _In  := Data;
    Inc(_In, CI); //((float*)c->audio)+ci;
    _Out := _In;

    While (Count>0) Do
    Begin
      Yt := _In^/32768; 	// Current input sample
      Inc(_In, Channels);

      K := 0;		// Frequency band index
      // Run the filters
      While (k<_ActiveFilters) Do
      Begin
 	      // Pointer to circular buffer wq
 	      wq := @(_wq[ci, k]);
 	      // Calculate output from AR part of current filter
 	      w := yt * _b[k,0] + wq[0] * _a[k,0] + wq[1] * _a[k,1];
 	      // Calculate output form MA part of current filter
 	      Yt := Yt + (w + wq[1] * _b[k,1]) *_Gain[ci,k];
 	      // Update circular buffer
 	      wq[1] := wq[0];
	      wq[0] := w;

        Inc(K);
      End;
      // Calculate output
      _Out ^ := Trunc(Yt * 32768);       //Yt//4.0*10)
      Inc(_Out, Channels);
      Dec(Count);
    End;

    Inc(CI);
  End;
End;

End.