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
 * TERRA_MOD
 * Implements a MOD file loader/decoder
 ***********************************************************************************************************************
}

Unit TERRA_MOD;
{$I terra.inc}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$DEFINE UNROLLED}

Interface

Implementation
Uses TERRA_Utils, TERRA_Sound, TERRA_SoundStreamer, TERRA_Stream, TERRA_Log;

Const
  CMODSampleMoreSize=128;

  CMODPositionShift=16;

  CMODMixerShift=4;

  CMODMinPeriod=56;
  CMODMaxPeriod=1023;

Type
//  PShortint=^Shortint;
  TMODBuffer=Array[0..(StreamBufferSize*2)-1] OF Integer;

  PMODSampleData=^TMODSampleData;
  TMODSampleData=Array[0..131072+128] Of SmallInt;//TEST ME

     TMODSample=Packed Record
      Name:Array[0..21] Of TERRAChar;
      LengthCounter:WORD;
      FineTune:Byte;
      Volume:Byte;
      LoopStart:WORD;
      LoopLength:WORD;
     End;

     TMODSampleExt=Record
      Data:PMODSampleData;
      Number:Cardinal;
      LengthCounter:Cardinal;
      LoopStart:Cardinal;
      LoopLength:Cardinal;
      LoopEnd:Cardinal;
      Loop:LONGBOOL;
      FineTune:Cardinal;
      Position:Cardinal;
      PositionExt:Cardinal;
     End;

     TMODHeader15=Packed Record
      Title:Array[0..19] Of TERRAChar;
      Samples:Array[1..15] OF TMODSample;
      TrackLength:Byte;
      RestartPosition:Byte;
      PatternOrder:Array[0..127] OF Byte;
     End;

     TMODHeader31=Packed Record
      Title:Array[0..19] Of TERRAChar;
      Samples:Array[1..31] OF TMODSample;
      TrackLength:Byte;
      RestartPosition:Byte;
      PatternOrder:Array[0..127] OF Byte;
      Tag:Array[0..3] Of TERRAChar;
     End;

     PMODPatternNote=^TMODPatternNote;
     TMODPatternNote=Record
      Note,Sample,Effect,EffectParameter:Byte;
     End;
     TMODPatternZeile=Array[0..31] OF TMODPatternNote;

     PMODPattern=^TMODPattern;
     TMODPattern=Array[0..63] OF TMODPatternZeile;

     PMODChannel=^TMODChannel;
     TMODChannel=Record
      Active:Boolean;
      StandardPanning:Cardinal;
      Panning:Cardinal;
      Volume:Cardinal;
      TempVolume:Cardinal;
      Sample:TMODSampleExt;
      Increment:Cardinal;
      Period:Cardinal;
      TempPeriod:Cardinal;
      DestPeriod:Cardinal;
      Effect:Cardinal;
      EffectParameter:Cardinal;
      EffectParameterHi:Cardinal;
      EffectParameterLo:Cardinal;
      NoteDelay:Cardinal;
      VolumeSlideHi:Cardinal;
      VolumeSlideLo:Cardinal;
      PortSpeed:Cardinal;
      ArpeggioPosition:Cardinal;
      Arpeggio0:Cardinal;
      Arpeggio1:Cardinal;
      Arpeggio2:Cardinal;
      VibratoParameterHi:Cardinal;
      VibratoParameterLo:Integer;
      VibratoTyp:Cardinal;
      VibratoPosition:Cardinal;
      TremoloParameterHi:Cardinal;
      TremoloParameterLo:Integer;
      TremoloTyp:Cardinal;
      TremoloPosition:Cardinal;
      RetrigCounter:Cardinal;
      PatternLoopStart:Cardinal;
      PatternLoopCounter:Cardinal;
      PatternLoop:Boolean;
      Surround:Boolean;
      Filter:Boolean;
      GlissanDo:Boolean;
     End;

     TMODTimingMode=(tmNTSC,tmPAL);

     MODStreamer=Class(SoundStream)
      Protected
       Frequency:Cardinal;

       BufferCounter:Cardinal;
       Start:BOOLEAN;
       BPMSamples:Cardinal;
       BPMSamplesZaehler:Cardinal;

       Header15:TMODHeader15;
       Header31:TMODHeader31;

       HowManyChannels:Byte;
       HowManySamples:Byte;
       HowManyPatterns:Byte;

       IsTrackActive:Boolean;
       Filter:Boolean;

       TrackLength:Byte;
       RestartPosition:Byte;

       PatternDelay:Byte;
       FrameDelay:Byte;

       Buffer:TMODBuffer;

       NoiseReductionLeft,NoiseReductionRight:Integer;

       Procedure ClearData;

       Function AmigaToPC(W:WORD):WORD;

       Function GetPeriod(Note,FineTune:Cardinal):Cardinal;
       Function GetNote(Period:Cardinal):Cardinal;

       Procedure SetTickVariables;
       Procedure DoTick;

       Procedure MixChannel(Channel:PMODChannel;StartPosition,LengthCounter:Cardinal);
       Function DoMix(StartPosition,LengthCounter:Cardinal;Var DoContinue:Boolean):Cardinal;
       Procedure MixBuffer(DestBuffer:POINTER);

        Class Function Validate(Source:Stream):Boolean; Override;
        Procedure Stream(Target:Cardinal); Override;

      Public
       Sample:Array[1..31] OF TMODSample;
       SampleExt:Array[1..31] OF TMODSampleExt;
       PatternOrder:Array[0..127] OF Byte;
       Pattern:Array[0..127] OF PMODPattern;
       Channel:Array[1..32] OF TMODChannel;

       TimingMode:TMODTimingMode;

       Speed:Byte;
       BPM:Byte;

       Tick:Byte;

       CurrentPattenOrder:Byte;
       CurrentRow:Byte;
       CurrentPatten:Byte;

       Title:TERRAString[20];

       Procedure Init; Override;
       Procedure Release; Override;

       Function Load(Source:Stream):Boolean;

       PROPERTY TrackActive:Boolean Read IsTrackActive;
     End;

Const PeriodTab:Array[0..15,1..60] OF WORD=((1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906,856,808,762,720,678,640,604,570,538,508,480,453,428,404,381,360,339,320,302,285,269,254,240,226,214,202,190,180,170,160,151,143,135,127,120,113,107,101,95,90,85,80,75,71,67,63,60,56),
                                            (1700,1604,1514,1430,1348,1274,1202,1134,1070,1010,954,900,850,802,757,715,674,637,601,567,535,505,477,450,425,401,379,357,337,318,300,284,268,253,239,225,213,201,189,179,169,159,150,142,134,126,119,113,106,100,94,89,84,79,75,71,67,63,59,56),
                                            (1688,1592,1504,1418,1340,1264,1194,1126,1064,1004,948,894,844,796,752,709,670,632,597,563,532,502,474,447,422,398,376,355,335,316,298,282,266,251,237,224,211,199,188,177,167,158,149,141,133,125,118,112,105,99,94,88,83,79,74,70,66,62,59,56),
                                            (1676,1582,1492,1408,1330,1256,1184,1118,1056,996,940,888,838,791,746,704,665,628,592,559,528,498,470,444,419,395,373,352,332,314,296,280,264,249,235,222,209,198,187,176,166,157,148,140,132,125,118,111,104,99,93,88,83,78,74,70,66,62,59,55),
                                            (1664,1570,1482,1398,1320,1246,1176,1110,1048,990,934,882,832,785,741,699,660,623,588,555,524,495,467,441,416,392,370,350,330,312,294,278,262,247,233,220,208,196,185,175,165,156,147,139,131,124,117,110,104,98,92,87,82,78,73,69,65,62,58,55),
                                            (1652,1558,1472,1388,1310,1238,1168,1102,1040,982,926,874,826,779,736,694,655,619,584,551,520,491,463,437,413,390,368,347,328,309,292,276,260,245,232,219,206,195,184,174,164,155,146,138,130,123,116,109,103,97,92,87,82,77,73,69,65,61,58,54),
                                            (1640,1548,1460,1378,1302,1228,1160,1094,1032,974,920,868,820,774,730,689,651,614,580,547,516,487,460,434,410,387,365,345,325,307,290,274,258,244,230,217,205,193,183,172,163,154,145,137,129,122,115,109,102,96,91,86,81,77,72,68,64,61,57,54),
                                            (1628,1536,1450,1368,1292,1220,1150,1086,1026,968,914,862,814,768,725,684,646,610,575,543,513,484,457,431,407,384,363,342,323,305,288,272,256,242,228,216,204,192,181,171,161,152,144,136,128,121,114,108,102,96,90,85,80,76,72,68,64,60,57,54),
                                            (1814,1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,907,856,808,762,720,678,640,604,570,538,508,480,453,428,404,381,360,339,320,302,285,269,254,240,226,214,202,190,180,170,160,151,143,135,127,120,113,107,101,95,90,85,80,75,71,67,63,60),
                                            (1800,1700,1604,1514,1430,1350,1272,1202,1134,1070,1010,954,900,850,802,757,715,675,636,601,567,535,505,477,450,425,401,379,357,337,318,300,284,268,253,238,225,212,200,189,179,169,159,150,142,134,126,119,112,106,100,94,89,84,79,75,71,67,63,59),
                                            (1788,1688,1592,1504,1418,1340,1264,1194,1126,1064,1004,948,894,844,796,752,709,670,632,597,563,532,502,474,447,422,398,376,355,335,316,298,282,266,251,237,223,211,199,188,177,167,158,149,141,133,125,118,111,105,99,94,88,83,79,74,70,66,62,59),
                                            (1774,1676,1582,1492,1408,1330,1256,1184,1118,1056,996,940,887,838,791,746,704,665,628,592,559,528,498,470,444,419,395,373,352,332,314,296,280,264,249,235,222,209,198,187,176,166,157,148,140,132,125,118,111,104,99,93,88,83,78,74,70,66,62,59),
                                            (1762,1664,1570,1482,1398,1320,1246,1176,1110,1048,988,934,881,832,785,741,699,660,623,588,555,524,494,467,441,416,392,370,350,330,312,294,278,262,247,233,220,208,196,185,175,165,156,147,139,131,123,117,110,104,98,92,87,82,78,73,69,65,61,58),
                                            (1750,1652,1558,1472,1388,1310,1238,1168,1102,1040,982,926,875,826,779,736,694,655,619,584,551,520,491,463,437,413,390,368,347,328,309,292,276,260,245,232,219,206,195,184,174,164,155,146,138,130,123,116,109,103,97,92,87,82,77,73,69,65,61,58),
                                            (1736,1640,1548,1460,1378,1302,1228,1160,1094,1032,974,920,868,820,774,730,689,651,614,580,547,516,487,460,434,410,387,365,345,325,307,290,274,258,244,230,217,205,193,183,172,163,154,145,137,129,122,115,108,102,96,91,86,81,77,72,68,64,61,57),
                                            (1724,1628,1536,1450,1368,1292,1220,1150,1086,1026,968,914,862,814,768,725,684,646,610,575,543,513,484,457,431,407,384,363,342,323,305,288,272,256,242,228,216,203,192,181,171,161,152,144,136,128,121,114,108,101,96,90,85,80,76,72,68,64,60,57));
                                            SinusTabelle:Array[0..64-1] OF Shortint=(0,12,25,37,49,60,71,81,90,98,106,112,117,122,125,126,127,126,125,122,117,112,106,98,90,81,71,60,49,37,25,12,0,-12,-25,-37,-49,-60,-71,-81,-90,-98,-106,-112,-117,-122,-125,-126,-127,-126,-125,-122,-117,-112,-106,-98,-90,-81,-71,-60,-49,-37,-25,-12);
                                            RampDownTabelle:Array[0..64-1] OF Shortint=(0,-4,-8,-12,-16,-20,-24,-28,-32,-36,-40,-44,-48,-52,-56,-60,-64,-68,-72,-76,-80,-84,-88,-92,-96,-100,-104,-108,-112,-116,-120,-124,127,123,119,115,111,107,103,99,95,91,87,83,79,75,71,67,63,59,55,51,47,43,39,35,31,27,23,19,15,11,7,3);
                                            SquareTabelle:Array[0..64-1] OF Shortint=(127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127);
                                            RanDomTabelle:Array[0..64-1] OF Shortint=(88,-127,-43,88,102,41,-65,-94,125,20,-71,-86,-70,-32,-16,-96,17,72,107,-5,116,-69,-62,-40,10,-61,65,109,-18,-38,-13,-76,-23,88,21,-94,8,106,21,-112,6,109,20,-88,-30,9,-127,118,42,-34,89,-4,-51,-72,21,-29,112,123,84,-101,-92,98,-54,-95);

Procedure MODStreamer.Init;
Var
  I:Integer;
Begin
  Self.AllocBuffer(2, 16, DefaultSampleRate);
  TimingMode:=tmNTSC;

  For I:=0 To 31 Do
  Begin
    FillChar(Sample[I],SizeOf(TMODSample),#0);
    FillChar(SampleExt[I],SizeOf(TMODSampleExt),#0);
  End;

  For I:=0 To 127 Do
    Pattern[I] := Nil;
  ClearData;

 Start := False;
 IsTrackActive := False;

 Self.Frequency := DefaultSampleRate;

  If Not Self.Load(_Source) Then
    Exit;

 For I:=1 To HowManyChannels Do
 Begin
  Channel[I].Panning:=Channel[I].StandardPanning;
  Channel[I].NoteDelay:=0;
  Channel[I].VibratoParameterHi:=1;
  Channel[I].VibratoParameterLo:=1;
  Channel[I].VibratoTyp:=0;
  Channel[I].VibratoPosition:=0;
  Channel[I].TremoloParameterHi:=1;
  Channel[I].TremoloParameterLo:=1;
  Channel[I].TremoloTyp:=0;
  Channel[I].TremoloPosition:=0;
  Channel[I].RetrigCounter:=0;
  Channel[I].PatternLoopStart:=0;
  Channel[I].PatternLoopCounter:=0;
  Channel[I].PatternLoop:=False;
  Channel[I].Surround:=False;
  Channel[I].Filter:=False;
  Channel[I].GlissanDo:=False;
 End;

 Speed:=6;
 BPM:=125;
 Tick:=0;
 SetTickVariables;
 BPMSamplesZaehler:=0;

 PatternDelay:=0;
 FrameDelay:=0;

 CurrentPattenOrder:=0;
 CurrentRow:=0;

 BufferCounter:=0;
 Start:=True;
End;

Procedure MODStreamer.Release;
Begin
  ClearData();
End;

Procedure MODStreamer.ClearData;
Var
  I:Integer;
Begin
 IsTrackActive := False;
 Filter := False;
 Title := '';
 For I:=1 To 32 Do
  FillChar(Channel[I], SizeOf(TMODChannel),#0);

 For I:=1 To 31 Do
 Begin
  If Assigned(SampleExt[I].Data) Then
  Begin
   FreeMem(SampleExt[I].Data);
   SampleExt[I].Data:=NIL;
  End;
  FillChar(Sample[I],SizeOf(TMODSample),#0);
  FillChar(SampleExt[I],SizeOf(TMODSampleExt),#0);
  SampleExt[I].Number:=I;
 End;
 FillChar(PatternOrder,256,#0);

 For I:=0 To 127 Do
 If Assigned(Pattern[I]) Then
 Begin
  FreeMem(Pattern[I]);
  Pattern[I]:=NIL;
 End;

 For I:=1 To 32 Do
  Channel[I].Active:=False;
End;

Function MODStreamer.AmigaToPC(W:WORD):WORD; {$IFDEF CPU386} ASSEMBLER;
ASM
 MOV AX,W
 XCHG AH,AL
End;
{$Else}
Begin
 Result:=((W And $FF) Shl 8) Or ((W Shr 8) And $FF);
End;
{$ENDIF}

Function MODStreamer.GetPeriod(Note,FineTune:Cardinal):Cardinal;
Begin
 If Note<60 Then Begin
  If FineTune<16 Then Begin
   Result:=PeriodTab[FineTune,Note];
  End Else Begin
   Result:=PeriodTab[0,Note];
  End;
 End Else Begin
  Result:=0;
 End;
End;

Function MODStreamer.GetNote(Period:Cardinal):Cardinal;
Var Note:Cardinal;
Begin
 Note:=0;
 If Period>0 Then Begin
  While True Do Begin
   Inc(Note);
   If Period>=PeriodTab[0,Note] Then Begin
    BREAK;
   End Else If (Note>60) Then Begin
    Note:=0;
    BREAK;
   End;
  End;
 End;
 Result:=Note;
End;

Function MODStreamer.Load(Source:Stream):BOOLEAN;
Var
  TotalSampleLength,PatternStartPosition,PatternA,PatternB:Cardinal;
  ASample:TMODSample;
  OldMODFormat:Boolean;
  I,J,K:INTEGER;
  Buffer:Array[1..4] OF Byte;
  S:Shortint;

Begin
 Result:=False;
 ClearData;
 OldMODFormat:=False;

 HowManyChannels:=0;
 HowManySamples:=31;

 Title:='';

 Source.Read(Header31,SizeOf(TMODHeader31));

 If Header31.Tag[0]='P' Then If Header31.Tag[1]='P' Then EXIT;
 If (Header31.Tag='M.K.') Or (Header31.Tag='M!K!') Or (Header31.Tag='M&K!') Or (Header31.Tag='N.T.') Then Begin
  HowManyChannels:=4;
 End Else If (Header31.Tag='OCTA') Or (Header31.Tag='OKTA') Or (Header31.Tag='CD81') Or (Header31.Tag='WOW!') Then Begin  HowManyChannels:=8;
  HowManyChannels:=8;
 End Else If (Header31.Tag[0]='T') And (Header31.Tag[1]='D') And (Header31.Tag[2]='Z') Then Begin
  HowManyChannels:=ORD(Header31.Tag[3])-48;
 End Else If (Header31.Tag[0]='F') And (Header31.Tag[1]='L') And (Header31.Tag[2]='T') Then Begin
  HowManyChannels:=ORD(Header31.Tag[3])-48;
  If Not HowManyChannels IN [1..8] Then HowManyChannels:=0;
 End Else If (Header31.Tag[0]='E') And (Header31.Tag[1]='X') And (Header31.Tag[2]='O') Then Begin
  HowManyChannels:=ORD(Header31.Tag[3])-48;
  If Not HowManyChannels IN [1..8] Then HowManyChannels:=0;
 End Else If (Header31.Tag[1]='C') And (Header31.Tag[2]='H') And (Header31.Tag[3]='N') Then Begin
  HowManyChannels:=ORD(Header31.Tag[0])-48;
 End Else If (Header31.Tag[2]='C') And (Header31.Tag[3]='N') Then Begin
  HowManyChannels:=((ORD(Header31.Tag[0])-48)*10)+(ORD(Header31.Tag[1])-48);
 End Else If (Header31.Tag[2]='C') And (Header31.Tag[3]='H') Then Begin
  HowManyChannels:=((ORD(Header31.Tag[0])-48)*10)+(ORD(Header31.Tag[1])-48);
 End Else If (Header31.Tag[0]='E') And (Header31.Tag[1]='X') Then Begin
  HowManyChannels:=((ORD(Header31.Tag[2])-48)*10)+(ORD(Header31.Tag[3])-48);
 End;
 If HowManyChannels=0 Then Begin
  HowManySamples:=15;
  HowManyChannels:=4;
  OldMODFormat:=True;

  Source.Seek(0);
  Source.Read(Header15,SizeOf(TMODHeader15));

  Title:=Header15.Title;

  TrackLength:=Header15.TrackLength;
  RestartPosition:=Header15.RestartPosition;

  MOVE(Header15.PatternOrder,PatternOrder,128);
 End Else Begin
  Title:=Header31.Title;

  TrackLength:=Header31.TrackLength;
  RestartPosition:=Header31.RestartPosition;

  MOVE(Header31.PatternOrder,PatternOrder,128);
 End;

 If RestartPosition>TrackLength Then
  RestartPosition:=0;

 TotalSampleLength:=0;
 For I:=1 To HowManySamples Do Begin
  If OldMODFormat Then Begin
   ASample:=Header15.Samples[I];
  End Else Begin
   ASample:=Header31.Samples[I];
  End;
  Sample[I]:=ASample;
  SampleExt[I].Number:=I;
  SampleExt[I].Data:=NIL;
  SampleExt[I].LengthCounter:=AmigaToPC(ASample.LengthCounter) Shl 1;
  SampleExt[I].LoopStart:=AmigaToPC(ASample.LoopStart) Shl 1;
  SampleExt[I].LoopLength:=AmigaToPC(ASample.LoopLength) Shl 1;
  SampleExt[I].Loop:=False;
  SampleExt[I].FineTune:=ASample.FineTune;
  SampleExt[I].Position:=0;
  SampleExt[I].PositionExt:=0;
  Inc(TotalSampleLength,SampleExt[I].LengthCounter);
 End;

 If OldMODFormat Then Begin
  PatternStartPosition:=SizeOf(TMODHeader15);
 End Else Begin
  PatternStartPosition:=SizeOf(TMODHeader31);
 End;

 PatternB:=0;
 For I:=0 To 127 Do If PatternOrder[I]<=127 Then Begin
  If PatternOrder[I]>PatternB Then PatternB:=PatternOrder[I];
 End;
 Inc(PatternB);

 If HowManyChannels=4 Then Begin
  If Cardinal(PatternStartPosition+(PatternB*8*4*64)+TotalSampleLength)=Source.Size Then Begin
   HowManyChannels:=8;
  End;
 End;

 PatternA:=(Source.Size-PatternStartPosition-TotalSampleLength) Div ((((1024 Div 4)*HowManyChannels) Div 64)*64);
 HowManyPatterns:=PatternA;

 If OldMODFormat Or (((PatternA>64) And (PatternB<=64))) Then Begin
  PatternA:=PatternB;
 End;
 If PatternA=PatternB Then Begin
  HowManyPatterns:=PatternA;
 End Else Begin
  If PatternB<PatternA Then Begin
   HowManyPatterns:=PatternB;
  End Else If ((PatternB>64) And ((Header31.Tag<>'M!K!') And Not OldMODFormat)) Then Begin
   HowManyPatterns:=PatternA;
  End Else Begin
   HowManyPatterns:=PatternB;
  End;
 End;
 If HowManyPatterns>128 Then HowManyPatterns:=128;

 If Cardinal(PatternStartPosition+(HowManyPatterns*HowManyChannels*4*64)+TotalSampleLength)<>Source.Size Then
 Begin
  ClearData;
  EXIT;
 End;

 For I:=0 To (HowManyChannels Shr 2)-1 Do
 Begin
  Channel[(I*4)+1].StandardPanning:=0;
  Channel[(I*4)+2].StandardPanning:=255;
  Channel[(I*4)+3].StandardPanning:=255;
  Channel[(I*4)+4].StandardPanning:=0;
 End;

 For I:=0 To HowManyPatterns-1 Do
 Begin
  GetMem(Pattern[I], SizeOf(TMODPattern));
  For J:=0 To 63 Do
  Begin
   For K:=0 To HowManyChannels-1 Do Begin
    Source.Read(Buffer,4);
    Pattern[I]^[J,K].Note:=GetNote(((Buffer[1] And $0F) Shl 8) Or Buffer[2]);
    Pattern[I]^[J,K].Sample:=(Buffer[1] And $F0) Or (Buffer[3] Shr 4);
    Pattern[I]^[J,K].Effect:=Buffer[3] And $F;
    Pattern[I]^[J,K].EffectParameter:=Buffer[4];
   End;
  End;
 End;

 For I:=1 To HowManySamples Do
 Begin
  If SampleExt[I].LengthCounter>1 Then Begin
   If SampleExt[I].LoopStart>SampleExt[I].LengthCounter Then Begin
    SampleExt[I].LoopStart:=0;
    SampleExt[I].LoopLength:=0;
   End;
   If (SampleExt[I].LoopStart+SampleExt[I].LoopLength)>SampleExt[I].LengthCounter Then Begin
    SampleExt[I].LoopLength:=SampleExt[I].LengthCounter-SampleExt[I].LoopStart;
    If SampleExt[I].LoopLength<2 Then SampleExt[I].LoopLength:=2;
   End;
   SampleExt[I].Loop:=(SampleExt[I].LoopLength>2);
   If SampleExt[I].LengthCounter>0 Then Begin
    GetMem(SampleExt[I].Data,(SampleExt[I].LengthCounter+CMODSampleMoreSize)*SizeOf(SmallInt));
    FillChar(SampleExt[I].Data^,(SampleExt[I].LengthCounter+CMODSampleMoreSize)*SizeOf(SmallInt),0);
    For J:=1 To SampleExt[I].LengthCounter Do Begin
     Source.Read(S,SizeOf(Shortint));
     SampleExt[I].Data^[J]:=S*256;
    End;
    For J:=SampleExt[I].LengthCounter+1 To SampleExt[I].LengthCounter+CMODSampleMoreSize-1 Do Begin
     SampleExt[I].Data^[J]:=SampleExt[I].Data^[SampleExt[I].LengthCounter-1];
    End;
   End;
   SampleExt[I].LoopEnd:=SampleExt[I].LoopStart+SampleExt[I].LoopLength;
  End;
 End;

 IsTrackActive:=True;
 Result:=True;
End;

Procedure MODStreamer.SetTickVariables;
Begin
 BPMSamples:=(Frequency*5*128) Div (BPM Shl 8);
 BPMSamplesZaehler:=BPMSamples;
End;

Procedure MODStreamer.DoTick;
Var PatternNote:PMODPatternNote;
    SampleNr,Period,Effect,EffectParameter,PatternChannel,ChannelNr,Note,
    TheNote,FineTune,NewCurrentPattern,NewCurrentRow,Hz:Cardinal;
    Value,Value1,Value2:Integer;
    Jump:Boolean;
Begin
 Inc(Tick);
 If Tick>=((Speed*(PatternDelay+1))+FrameDelay) Then Begin
  Tick:=0;
  PatternDelay:=0;
  FrameDelay:=0;

  NewCurrentPattern:=0;
  NewCurrentRow:=0;
  Jump:=False;

  If CurrentRow>=64 Then
  Begin
   Inc(CurrentPattenOrder);
   CurrentRow:=0;
  End;
  If CurrentPattenOrder>=TrackLength Then
  Begin
   CurrentPattenOrder:=RestartPosition;
   CurrentRow:=0;
  End;

  If CurrentPattenOrder<128 Then Begin
   CurrentPatten:=PatternOrder[CurrentPattenOrder];
   If CurrentPatten<128 Then Begin
    For PatternChannel:=1 To HowManyChannels Do Begin
     PatternNote:=@Pattern[CurrentPatten]^[CurrentRow,PatternChannel-1];
     Note:=PatternNote.Note;
     SampleNr:=PatternNote.Sample;
     Effect:=PatternNote.Effect;
     EffectParameter:=PatternNote.EffectParameter;

     Channel[PatternChannel].Effect:=Effect;
     Channel[PatternChannel].EffectParameter:=EffectParameter;
     Channel[PatternChannel].EffectParameterHi:=EffectParameter Shr 4;
     Channel[PatternChannel].EffectParameterLo:=EffectParameter And $F;
     Channel[PatternChannel].VolumeSlideHi:=0;
     Channel[PatternChannel].VolumeSlideLo:=0;

     If (SampleNr>0) And (SampleNr<=HowManySamples) Then Begin
      FineTune:=Sample[SampleNr].FineTune;
     End Else Begin
      FineTune:=0;
     End;
     If Note<>0 Then Begin
      Period:=GetPeriod(Note,FineTune);
     End Else Begin
      Period:=0;
     End;

     If (SampleNr>0) And (SampleNr<=HowManySamples) Then Begin
      If Assigned(SampleExt[SampleNr].Data) Then Begin
       Channel[PatternChannel].Active:=True;
       Channel[PatternChannel].Volume:=Sample[SampleNr].Volume;
       If Channel[PatternChannel].Volume=0 Then Channel[PatternChannel].Volume:=64;
       Channel[PatternChannel].Sample:=SampleExt[SampleNr];
       Channel[PatternChannel].Sample.Position:=0;
       Channel[PatternChannel].Sample.PositionExt:=0;
      End Else Begin
       Channel[PatternChannel].Active:=False;
      End;
     End;
     If Period<>0 Then Begin
      If (Effect<>$3) Or (Effect<>$5) Then Begin
       Channel[PatternChannel].Period:=Period;
       Channel[PatternChannel].TempPeriod:=Period;
      End;
      If Assigned(Channel[PatternChannel].Sample.Data) Then Begin
       Channel[PatternChannel].Active:=True;
       Channel[PatternChannel].Volume:=Sample[Channel[PatternChannel].Sample.Number].Volume;
       If Channel[PatternChannel].Volume=0 Then Channel[PatternChannel].Volume:=64;
       Channel[PatternChannel].Sample.Position:=0;
       Channel[PatternChannel].Sample.PositionExt:=0;
      End;
     End;

     Case Effect OF
      $0:Begin
       If EffectParameter<>0 Then Begin
        TheNote:=GetNote(Channel[PatternChannel].Period);
        Channel[PatternChannel].ArpeggioPosition:=0;
        Channel[PatternChannel].Arpeggio0:=Channel[PatternChannel].Period;
        If (TheNote+(EffectParameter Shr 4))<CMODMaxPeriod Then Begin
         Channel[PatternChannel].Arpeggio1:=GetPeriod(TheNote+(EffectParameter Shr 4),Channel[PatternChannel].Sample.FineTune);
        End Else Begin
         Channel[PatternChannel].Arpeggio1:=CMODMaxPeriod;
        End;
        If (TheNote+(EffectParameter And $F))<CMODMaxPeriod Then Begin
         Channel[PatternChannel].Arpeggio2:=GetPeriod(TheNote+(EffectParameter And $F),Channel[PatternChannel].Sample.FineTune);
        End Else Begin
         Channel[PatternChannel].Arpeggio2:=CMODMaxPeriod;
        End;
        If Channel[PatternChannel].Arpeggio0>CMODMaxPeriod Then Channel[PatternChannel].Arpeggio0:=CMODMaxPeriod;
        If Channel[PatternChannel].Arpeggio1>CMODMaxPeriod Then Channel[PatternChannel].Arpeggio1:=CMODMaxPeriod;
        If Channel[PatternChannel].Arpeggio2>CMODMaxPeriod Then Channel[PatternChannel].Arpeggio2:=CMODMaxPeriod;
       End;
      End;
      $1:Begin // Port Down
       Channel[PatternChannel].PortSpeed:=EffectParameter;
      End;
      $2:Begin // Port Up
       Channel[PatternChannel].PortSpeed:=EffectParameter;
      End;
      $3:Begin // Porta To Note
       If Period<>0 Then Channel[PatternChannel].DestPeriod:=Period;
       If Channel[PatternChannel].TempPeriod=0 Then Channel[PatternChannel].TempPeriod:=CMODMinPeriod;
       If EffectParameter<>0 Then Channel[PatternChannel].PortSpeed:=EffectParameter;
       If Channel[PatternChannel].DestPeriod<CMODMinPeriod Then Channel[PatternChannel].DestPeriod:=CMODMinPeriod;
       If Channel[PatternChannel].DestPeriod>CMODMaxPeriod Then Channel[PatternChannel].DestPeriod:=CMODMaxPeriod;
      End;
      $4:Begin // Vibrato
       If (EffectParameter Shr 4)<>0 Then Channel[PatternChannel].VibratoParameterHi:=EffectParameter Shr 4;
       If (EffectParameter And $F)<>0 Then Channel[PatternChannel].VibratoParameterLo:=EffectParameter And $F;
       If (Channel[PatternChannel].VibratoTyp And 4)=0 Then Begin
        Channel[PatternChannel].VibratoPosition:=0;
       End;
      End;
      $5, // Porta To Note und Volume Slide
      $6, // Vibrato And Volume Slide
      $A:Begin // Volume Slide
       If (EffectParameter And $F0)<>0 Then Begin
        Channel[PatternChannel].VolumeSlideHi:=EffectParameter Shr 4;
       End;
       If (EffectParameter And $F)<>0 Then Begin
        Channel[PatternChannel].VolumeSlideLo:=EffectParameter And $F;
       End;
      End;
      $7:Begin // Tremolo
       Channel[PatternChannel].TempVolume:=Channel[PatternChannel].Volume;
       If (EffectParameter Shr 4)<>0 Then Channel[PatternChannel].TremoloParameterHi:=EffectParameter Shr 4;
       If (EffectParameter And $F)<>0 Then Channel[PatternChannel].TremoloParameterLo:=EffectParameter And $F;
       If (Channel[PatternChannel].TremoloTyp And 4)=0 Then Begin
        Channel[PatternChannel].TremoloPosition:=0;
       End;
      End;
      $8:Begin // Set Panning
       If Channel[PatternChannel].EffectParameter<=$80 Then Begin
        Channel[PatternChannel].Panning:=Channel[PatternChannel].EffectParameter Shl 1;
        Channel[PatternChannel].Surround:=False;
       End Else If Channel[PatternChannel].EffectParameter=$A4 Then Begin
        Channel[PatternChannel].Surround:=True;
       End;
      End;
      $9:Begin // Set Sample Position
       Channel[PatternChannel].Sample.Position:=EffectParameter Shl 8;
       Channel[PatternChannel].Sample.PositionExt:=Channel[PatternChannel].Sample.Position Shl CMODPositionShift;
      End;
      $B:Begin // Pattern Jump
       NewCurrentPattern:=EffectParameter;
       If Not Jump Then NewCurrentRow:=0;
       Jump:=True;
      End;
      $C:Begin // Set Volume
       Channel[PatternChannel].Volume:=EffectParameter;
      End;
      $D:Begin // Next Pattern
       If Not Jump Then NewCurrentPattern:=CurrentPattenOrder+1;
       NewCurrentRow:=(10*(EffectParameter Shr 4))+(EffectParameter And $F);
       Jump:=True;
      End;
      $E:Begin // Extended Effects
       Case Channel[PatternChannel].EffectParameterHi OF
        $0:Begin // Filter On/Off
         Channel[PatternChannel].Filter:=(Channel[PatternChannel].EffectParameterLo And 1)<>0;
         Filter:=(Channel[PatternChannel].EffectParameterLo And 1)<>0;
        End;
        $1:Begin // Fine Period Slide Up
         If Channel[PatternChannel].TempPeriod>Channel[PatternChannel].EffectParameterLo Then Begin
          Dec(Channel[PatternChannel].TempPeriod,Channel[PatternChannel].EffectParameterLo);
          If Channel[PatternChannel].TempPeriod<CMODMinPeriod Then Channel[PatternChannel].TempPeriod:=CMODMinPeriod;
         End Else Begin
          Channel[PatternChannel].TempPeriod:=CMODMinPeriod;
         End;
        End;
        $2:Begin // Fine Period Slide Down
         Inc(Channel[PatternChannel].TempPeriod,Channel[PatternChannel].EffectParameterLo);
         If Channel[PatternChannel].TempPeriod>CMODMaxPeriod Then Channel[PatternChannel].TempPeriod:=CMODMaxPeriod;
        End;
        $3:Begin // GlissanDo On/Off
         Channel[PatternChannel].GlissanDo:=(Channel[PatternChannel].EffectParameterLo And 1)<>0;
        End;
        $4:Begin // Set Vibrato WaveForm
         Channel[PatternChannel].VibratoTyp:=Channel[PatternChannel].EffectParameterLo And 7;
        End;
        $5:Begin // Set FineTune
         Channel[PatternChannel].TempPeriod:=GetPeriod(Channel[PatternChannel].EffectParameterLo,GetNote(Channel[PatternChannel].Period));
        End;
        $6:Begin // Pattern Loop
         If Channel[PatternChannel].EffectParameterLo=0 Then Begin
          If Not Channel[PatternChannel].PatternLoop Then Begin
           Channel[PatternChannel].PatternLoopStart:=CurrentRow;
          End;
         End Else Begin
          If Channel[PatternChannel].PatternLoop Then Begin
           If Channel[PatternChannel].PatternLoopCounter=0 Then Begin
            Channel[PatternChannel].PatternLoop:=False;
           End Else Begin
            Dec(Channel[PatternChannel].PatternLoopCounter);
            NewCurrentPattern:=CurrentPattenOrder;
            NewCurrentRow:=Channel[PatternChannel].PatternLoopStart;
            Jump:=True;
           End;
          End Else Begin
           Channel[PatternChannel].PatternLoopCounter:=Channel[PatternChannel].EffectParameterLo;
           Channel[PatternChannel].PatternLoop:=True;
           NewCurrentPattern:=CurrentPattenOrder;
           NewCurrentRow:=Channel[PatternChannel].PatternLoopStart;
           Jump:=True;
          End;
         End;
        End;
        $7:Begin // Set Tremolo WaveForm
         Channel[PatternChannel].TremoloTyp:=Channel[PatternChannel].EffectParameterLo And 7;
        End;
        $8:Begin // Set Panning
         Channel[PatternChannel].Panning:=Channel[PatternChannel].EffectParameterLo Shl 4;
         Channel[PatternChannel].Surround:=False;
        End;
        $9:Begin // Retrig Note
         Channel[PatternChannel].RetrigCounter:=Channel[PatternChannel].EffectParameterLo;
        End;
        $A:Begin // Fine Volume Slide Up
         Inc(Channel[PatternChannel].Volume,Channel[PatternChannel].EffectParameterLo);
         If Channel[PatternChannel].Volume>64 Then Channel[PatternChannel].Volume:=64;
        End;
        $B:Begin // Fine Volume Slide Down
         If Channel[PatternChannel].Volume>Channel[PatternChannel].EffectParameterLo Then Begin
          Dec(Channel[PatternChannel].Volume,Channel[PatternChannel].EffectParameterLo);
         End Else Begin
          Channel[PatternChannel].Volume:=0;
         End;
        End;
        $C:Begin // Sample Cut
         If Channel[PatternChannel].EffectParameterLo=0 Then Channel[PatternChannel].Volume:=0;
        End;
        $D:Begin // Note Delay
         If (Channel[PatternChannel].Active) And (Channel[PatternChannel].EffectParameterLo<>0) Then Begin
          Channel[PatternChannel].NoteDelay:=Channel[PatternChannel].EffectParameterLo;
          Channel[PatternChannel].Active:=False;
         End Else Begin
          Channel[PatternChannel].NoteDelay:=0;
         End;
        End;
        $E:Begin // Pattern Delay
         PatternDelay:=Channel[PatternChannel].EffectParameterLo;
        End;
       End;
      End;
      $F:Begin // Set Speed Or BPM
       If EffectParameter<32 Then Begin
        Speed:=EffectParameter;
       End Else Begin
        BPM:=EffectParameter;
       End;
      End;
     End;
    End;
   End;
  End;

  If Jump Then Begin
   CurrentPattenOrder:=NewCurrentPattern;
   CurrentRow:=NewCurrentRow;
  End Else Begin
   Inc(CurrentRow);
  End;
 End;

 Case TimingMode OF
  tmNTSC:Hz:=3579545;
  tmPAL:Hz:=3546895;
  Else Hz:=0;
 End;

 // Process Channel values
 For ChannelNr:=1 To HowManyChannels Do
 Begin
  If Channel[ChannelNr].Active Then
  Begin
   Case Channel[ChannelNr].Effect OF
    $0:If Tick>0 Then
      Begin
     // Arpeggio
     If Channel[ChannelNr].EffectParameter<>0 Then Begin
      Case Channel[ChannelNr].ArpeggioPosition OF
       0:Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio0;
       1:Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio1;
       Else Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio2;
      End;
      Channel[ChannelNr].ArpeggioPosition:=(Channel[ChannelNr].ArpeggioPosition+1) MOD 3;
     End;
    End;
    $1:If ((Channel[ChannelNr].EffectParameterHi<14) And (Tick<>0)) Or ((Channel[ChannelNr].EffectParameterHi=15) And (Tick=0)) Then Begin
     // Port Up
     If Channel[ChannelNr].TempPeriod>Channel[ChannelNr].PortSpeed Then Begin
      Dec(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
      If Channel[ChannelNr].TempPeriod<CMODMinPeriod Then Channel[ChannelNr].TempPeriod:=CMODMinPeriod;
     End Else Begin
      Channel[ChannelNr].TempPeriod:=CMODMinPeriod;
      Channel[ChannelNr].PortSpeed:=0;
     End;
    End;
    $2:If ((Channel[ChannelNr].EffectParameterHi<14) And (Tick<>0)) Or ((Channel[ChannelNr].EffectParameterHi=15) And (Tick=0)) Then Begin
     // Port Down
     If (Channel[ChannelNr].TempPeriod+Channel[ChannelNr].PortSpeed)<CMODMaxPeriod Then Begin
      Inc(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
      If Channel[ChannelNr].TempPeriod>CMODMaxPeriod Then Channel[ChannelNr].TempPeriod:=CMODMaxPeriod;
     End Else Begin
      Channel[ChannelNr].TempPeriod:=CMODMaxPeriod;
      Channel[ChannelNr].PortSpeed:=0;
     End;
    End;
    $3,$4,$5,$6,$A:If Tick<>0 Then Begin
     If (Channel[ChannelNr].Effect=$3) Or (Channel[ChannelNr].Effect=$5) Then Begin
      // Porta To Note
      If Channel[ChannelNr].TempPeriod<Channel[ChannelNr].DestPeriod Then Begin
       Inc(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
       If Channel[ChannelNr].TempPeriod>Channel[ChannelNr].DestPeriod Then Begin
        Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].DestPeriod;
        Channel[ChannelNr].PortSpeed:=0;
       End;
       If (Channel[ChannelNr].TempPeriod<>Channel[ChannelNr].DestPeriod) And Channel[ChannelNr].GlissanDo Then Begin
        Value:=GetNote(Channel[ChannelNr].TempPeriod);
        If Value>1 Then Begin
         Value1:=Channel[ChannelNr].TempPeriod-GetPeriod(Value-1,Channel[ChannelNr].Sample.FineTune);
        End Else Begin
         Value1:=Channel[ChannelNr].TempPeriod-CMODMinPeriod;
        End;
        Value2:=GetPeriod(Value,Channel[ChannelNr].Sample.FineTune)-Channel[ChannelNr].TempPeriod;
        If Value2>Value1 Then Begin
         Value:=Integer(Channel[ChannelNr].TempPeriod)-Value2;
        End Else Begin
         Value:=Integer(Channel[ChannelNr].TempPeriod)+Value1;
        End;
        If Value<CMODMinPeriod Then Value:=CMODMinPeriod;
        If Value>CMODMaxPeriod Then Value:=CMODMaxPeriod;
        Channel[ChannelNr].TempPeriod:=Value;
       End;
      End Else If Channel[ChannelNr].TempPeriod>Channel[ChannelNr].DestPeriod Then Begin
       Dec(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
       If Channel[ChannelNr].TempPeriod<Channel[ChannelNr].DestPeriod Then Begin
        Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].DestPeriod;
        Channel[ChannelNr].PortSpeed:=0;
       End;
       If (Channel[ChannelNr].TempPeriod<>Channel[ChannelNr].DestPeriod) And Channel[ChannelNr].GlissanDo Then Begin
        Value:=GetNote(Channel[ChannelNr].TempPeriod);
        If Value>1 Then Begin
         Value1:=Channel[ChannelNr].TempPeriod-GetPeriod(Value-1,Channel[ChannelNr].Sample.FineTune);
        End Else Begin
         Value1:=Channel[ChannelNr].TempPeriod-CMODMinPeriod;
        End;
        Value2:=GetPeriod(Value,Channel[ChannelNr].Sample.FineTune)-Channel[ChannelNr].TempPeriod;
        If Value2>Value1 Then Begin
         Value:=Integer(Channel[ChannelNr].TempPeriod)+Value2;
        End Else Begin
         Value:=Integer(Channel[ChannelNr].TempPeriod)-Value1;
        End;
        If Value<CMODMinPeriod Then Value:=CMODMinPeriod;
        If Value>CMODMaxPeriod Then Value:=CMODMaxPeriod;
        Channel[ChannelNr].TempPeriod:=Value;
       End;
      End;
     End;
     If (Channel[ChannelNr].Effect=$4) Or (Channel[ChannelNr].Effect=$6) Then Begin
      // Vibrato
      Case Channel[ChannelNr].VibratoTyp And 3 OF
       0:Value:=RampDownTabelle[Channel[ChannelNr].VibratoPosition And $3F];
       1:Value:=SquareTabelle[Channel[ChannelNr].VibratoPosition And $3F];
       2:Value:=RanDomTabelle[Channel[ChannelNr].VibratoPosition And $3F];
       Else Value:=SinusTabelle[Channel[ChannelNr].VibratoPosition And $3F];
      End;
      Value:=Integer(Channel[ChannelNr].Period)+((Value*Channel[ChannelNr].VibratoParameterLo) Div 64);
      If Value<CMODMinPeriod Then Value:=CMODMinPeriod;
      If Value>CMODMaxPeriod Then Value:=CMODMaxPeriod;
      Channel[ChannelNr].TempPeriod:=Value;
      If Tick<>0 Then Begin
       Channel[ChannelNr].VibratoPosition:=(Channel[ChannelNr].VibratoPosition+Channel[ChannelNr].VibratoParameterHi) And $3F;
      End;
     End;
     If (Channel[ChannelNr].Effect<>$3) And (Channel[ChannelNr].Effect<>$4) Then Begin
      // Volume Slide
      If Channel[ChannelNr].VolumeSlideLo<>0 Then Begin
       If Channel[ChannelNr].Volume>Channel[ChannelNr].VolumeSlideLo Then Begin
        Dec(Channel[ChannelNr].Volume,Channel[ChannelNr].VolumeSlideLo);
       End Else Begin
        Channel[ChannelNr].Volume:=0;
       End;
      End;
      If Channel[ChannelNr].VolumeSlideHi<>0 Then Begin
       If (Channel[ChannelNr].Volume+Channel[ChannelNr].VolumeSlideHi)<64 Then Begin
        Inc(Channel[ChannelNr].Volume,Channel[ChannelNr].VolumeSlideHi);
       End Else Begin
        Channel[ChannelNr].Volume:=64;
       End;
      End;
     End;
    End;
    $7:Begin
     // Tremolo
     Case Channel[ChannelNr].TremoloTyp And 3 OF
      0:Value:=RampDownTabelle[Channel[ChannelNr].TremoloPosition And $3F];
      1:Value:=SquareTabelle[Channel[ChannelNr].TremoloPosition And $3F];
      2:Value:=RanDomTabelle[Channel[ChannelNr].TremoloPosition And $3F];
      Else Value:=SinusTabelle[Channel[ChannelNr].TremoloPosition And $3F];
     End;
     Value:=Integer(Channel[ChannelNr].TempVolume)+((Value*Channel[ChannelNr].TremoloParameterLo) Div 64);
     If Value<0 Then Value:=0;
     If Value>64 Then Value:=64;
     Channel[ChannelNr].Volume:=Value;
     If Tick<>0 Then Begin
      Channel[ChannelNr].TremoloPosition:=(Channel[ChannelNr].TremoloPosition+Channel[ChannelNr].TremoloParameterHi) And $3F;
     End;
    End;
    $9:Begin
     // Retrig Note
     If Channel[ChannelNr].RetrigCounter<>0 Then Begin
      Inc(Channel[ChannelNr].RetrigCounter);
      If Channel[ChannelNr].EffectParameterLo<>0 Then Begin
       If (Channel[ChannelNr].RetrigCounter MOD Channel[ChannelNr].EffectParameterLo)=0 Then Begin
        If Assigned(Channel[ChannelNr].Sample.Data) Then Begin
         Channel[ChannelNr].Active:=True;
         Channel[ChannelNr].Sample.Position:=0;
         Channel[ChannelNr].Sample.PositionExt:=0;
        End;
       End;
      End;
     End;
    End;
    $E:Begin
     // Extended Effekte
     Case Channel[ChannelNr].EffectParameterHi OF
      $C:Begin // Sample Cut
       If Channel[ChannelNr].EffectParameterLo=Tick Then Channel[ChannelNr].Volume:=0;
      End;
      $D:Begin // Note Delay
       If Channel[ChannelNr].NoteDelay<>0 Then Begin
        If Channel[ChannelNr].NoteDelay=Tick Then Begin
         Channel[ChannelNr].Active:=True;
         Channel[ChannelNr].NoteDelay:=0;
        End;
       End;
      End;
     End;
    End;
   End;
   If Channel[ChannelNr].TempPeriod>0 Then Begin
    Channel[ChannelNr].Increment:=((Hz Div Channel[ChannelNr].TempPeriod) Shl CMODPositionShift) Div Frequency;
   End Else Begin
    Channel[ChannelNr].Active:=False;
   End;
  End;
 End;
 SetTickVariables;
End;

Function DoMixSample(Const Data:PMODSampleData;Const RemainSamples:Cardinal;Const VolumeLeft,VolumeRight:Integer;Var Buffer:PInteger;Const StartPosition:Cardinal;Const IncrementValue:Cardinal):Cardinal; REGISTER;
Var Remain,RemainCounter,Position,Increment:Cardinal;
    Value,LeftVolume,RightVolume:Integer;
    SampleData:PMODSampleData;
    Buf:PInteger;
Begin
 // Copy parameter To local variables -> better code optimization
 SampleData:=Data;
 Remain:=RemainSamples;
 LeftVolume:=VolumeLeft;
 RightVolume:=VolumeRight;
 Buf:=Buffer;
 Position:=StartPosition;
 Increment:=IncrementValue;
{$IFDEF UNROLLED}
 // Eliminate conditionals within loops
 Case Remain And 7 OF
  1:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain);
  End;
  2:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,2);
  End;
  3:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,3);
  End;
  4:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,4);
  End;
  5:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,5);
  End;
  6:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,6);
  End;
  7:Begin
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Value:=SampleData[Position Shr CMODPositionShift];
   Inc(Buf^,Value*LeftVolume); Inc(Buf);
   Inc(Buf^,Value*RightVolume); Inc(Buf);
   Inc(Position,Increment);
   Dec(Remain,7);
  End;
 End;
 // Unroll loops with eliminate most all conditionals within these unrolled
 // loops
 Remain:=Remain Shr 3;
 For RemainCounter:=1 To Remain Do Begin
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
 End;
{$Else}
 For RemainCounter:=1 To Remain Do Begin
  Value:=SampleData[Position Shr CMODPositionShift];
  Inc(Buf^,Value*LeftVolume); Inc(Buf);
  Inc(Buf^,Value*RightVolume); Inc(Buf);
  Inc(Position,Increment);
 End;
{$ENDIF}
 Buffer:=Buf;
 Result:=Position;
End;

Procedure MODStreamer.MixChannel(Channel:PMODChannel;StartPosition,LengthCounter:Cardinal);
Var Counter,Panning,SmpPos,SmpPosExt,SmpLen,SmpLoopStart,SmpLoopEnd,SmpLenExt,
    SmpLoopStartExt,SmpLoopEndExt,SmpInc,SmpEnd,Remain:Cardinal;
    SmpLoop:BOOLEAN;
    VolumeLeft,VolumeRight:Integer;
    Buf:PInteger;
Begin
 If Channel^.Increment=0 Then Begin
  Channel^.Active:=False;
 End Else If Channel^.Active Then Begin
  If Channel^.Surround Then Begin
   VolumeLeft:=64*Channel^.Volume;
   VolumeRight:=-64*Channel^.Volume;
  End Else Begin
   Panning:=(Channel^.Panning Shl 6) Shr 8;
   If Panning>64 Then Panning:=64;
   VolumeLeft:=(64-Panning)*Channel^.Volume;
   VolumeRight:=(Panning)*Channel^.Volume;
  End;
  VolumeLeft:=VolumeLeft Div HowManyChannels;
  VolumeRight:=VolumeRight Div HowManyChannels;

  SmpPos:=Channel^.Sample.Position;
  SmpPosExt:=Channel^.Sample.PositionExt;
  SmpLen:=Channel^.Sample.LengthCounter;
  SmpLoopStart:=Channel^.Sample.LoopStart;
  SmpLoopEnd:=Channel^.Sample.LoopEnd;
  SmpLoop:=Channel^.Sample.Loop;
  SmpInc:=Channel^.Increment;
  SmpLenExt:=SmpLen Shl CMODPositionShift;
  SmpLoopStartExt:=SmpLoopStart Shl CMODPositionShift;
  SmpLoopEndExt:=SmpLoopEnd Shl CMODPositionShift;

  If SmpLoop Then Begin
   SmpEnd:=SmpLoopEndExt;
  End Else Begin
   SmpEnd:=SmpLenExt;
  End;

  Buf:=@Buffer[StartPosition*2];
  Counter:=LengthCounter;
  While Counter>0 Do Begin
   If SmpLoop Then Begin
    If SmpPosExt>=SmpLoopEndExt Then Begin
     SmpPosExt:=SmpPosExt-(SmpLoopEndExt-SmpLoopStartExt);
    End;
   End;
   If SmpPosExt>=SmpLenExt Then Begin
    Channel^.Active:=False;
    BREAK;
   End;
   Remain:=1+((SmpEnd-SmpPosExt) Div SmpInc);
   If Remain=0 Then BREAK;
   If Remain>Counter Then Remain:=Counter;
   SmpPosExt:=DoMixSample(Channel^.Sample.Data,Remain,VolumeLeft,VolumeRight,Buf,SmpPosExt,SmpInc);
   Dec(Counter,Remain);
  End;

  Channel^.Sample.Position:=SmpPos;
  Channel^.Sample.PositionExt:=SmpPosExt;
 End;
End;

Function MODStreamer.DoMix(StartPosition,LengthCounter:Cardinal;Var DoContinue:Boolean):Cardinal;
Var TheLength,ChannelCounter:Cardinal;
Begin
 If (StartPosition+LengthCounter)<=StreamBufferSize Then Begin
  TheLength:=LengthCounter;
 End Else If StartPosition<=StreamBufferSize Then Begin
  TheLength:=StreamBufferSize-StartPosition;
 End Else Begin
  TheLength:=0;
 End;
 If TheLength>0 Then Begin
  Dec(BPMSamplesZaehler,TheLength);
  ChannelCounter:=HowManyChannels;
  While ChannelCounter>0 Do Begin
   MixChannel(@Channel[ChannelCounter],StartPosition,TheLength);
   Dec(ChannelCounter);
  End;
 End Else Begin
  DoContinue:=False;
 End;
 Result:=TheLength;
End;

Procedure MODStreamer.MixBuffer(DestBuffer:POINTER);
Var
  Counter:Cardinal;
  DoContinue:Boolean;
  SourcePointer:^SmallInt;
  Value:Cardinal;
  Buf:PCardinal;
Begin
 FillChar(Buffer,SizeOf(TMODBuffer),#0);

 Counter:=0;
 DoContinue:=True;
 While Counter<StreamBufferSize Do
 Begin
  If BPMSamplesZaehler=0 Then DoTick;
  Inc(Counter,DoMix(Counter,BPMSamplesZaehler,DoContinue));
 End;

 If Filter Then
 Begin
  // Lowpass Filter
  Buf:=@Buffer;
  Counter:=0;
  While Counter<StreamBufferSize Do
  Begin
   Value:=Buf^ Div 2;
   Buf^:=Value+NoiseReductionLeft;
   NoiseReductionLeft:=Value;
   Inc(Buf);
   Value:=Buf^ Div 2;
   Buf^:=Value+NoiseReductionRight;
   NoiseReductionRight:=Value;
   Inc(Buf);
   Inc(Counter);
  End;
 End;

 SourcePointer:=DestBuffer;
 Buf:=@Buffer;

 For Counter:=1 To StreamBufferSize Do
 Begin
    SourcePointer^ := SmallInt(Buf^ Shr (16-CMODMixerShift));
    Inc(SourcePointer);
    Inc(Buf);
    SourcePointer^ := SmallInt(Buf^ Shr (16-CMODMixerShift));
    Inc(SourcePointer);
    Inc(Buf);
 End;
End;

Procedure ModStreamer.Stream(Target:SoundTarget);
Begin
  Self.MixBuffer(_Data);
  Inherited;
End;

Class Function MODStreamer.Validate(Source:Stream):Boolean;
Begin
  Result := StringContains('.mod', Source.Name);
End;

Initialization
  Log(logDebug, 'MOD', 'Initializing');

  RegisterSoundStreamFormat(MODStreamer);
End.


