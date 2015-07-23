// MiniMOD
// (C)'04-05 BeRo (Benjamin Rosseaux)
//
// The source is free for use, but COPYRIGHTED
//
// Designed for small graphic demos as small music playback engine
//
// And don't forget to write my name in your credits of your demo ;)
//
// http://www.bero.0ok.de/
//
UNIT MiniMOD;
{$IFDEF FPC}
{$MODE DELPHI}
{$HINTS OFF}
{$WARNINGS OFF}
{$ELSE}
{$OPTIMIZATION ON}
{$ENDIF}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$DEFINE UNROLLED}

INTERFACE

Uses Windows, TERRA_AudioMixer, TERRA_Utils;

CONST CMiniMODSampleMoreSize=128;

      CMiniMODBufferSize=8192;
      CMiniMODPositionShift=16;

      CMiniMODMixerShift=4;

      CMiniMODMinPeriod=56;
      CMiniMODMaxPeriod=1023;

TYPE PLONGINT=^LONGINT;
     PSHORTINT=^SHORTINT;

     TMiniMODBuffer=ARRAY[0..(CMiniMODBufferSize*2)-1] OF LONGINT;

     PMiniMODSampleData=^TMiniMODSampleData;
     TMiniMODSampleData=ARRAY[0..131072+128] OF INTEGER;

     TMiniMODSample=PACKED RECORD
      Name:ARRAY[0..21] OF CHAR;
      LengthCounter:WORD;
      FineTune:BYTE;
      Volume:BYTE;
      LoopStart:WORD;
      LoopLength:WORD;
     END;

     TMiniMODSampleExt=RECORD
      Data:PMiniMODSampleData;
      Number:LONGWORD;
      LengthCounter:LONGWORD;
      LoopStart:LONGWORD;
      LoopLength:LONGWORD;
      LoopEnd:LONGWORD;
      Loop:LONGBOOL;
      FineTune:LONGWORD;
      Position:LONGWORD;
      PositionExt:LONGWORD;
     END;

     TMiniMODHeader15=PACKED RECORD
      Title:ARRAY[0..19] OF CHAR;
      Samples:ARRAY[1..15] OF TMiniMODSample;
      TrackLength:BYTE;
      RestartPosition:BYTE;
      PatternOrder:ARRAY[0..127] OF BYTE;
     END;

     TMiniMODHeader31=PACKED RECORD
      Title:ARRAY[0..19] OF CHAR;
      Samples:ARRAY[1..31] OF TMiniMODSample;
      TrackLength:BYTE;
      RestartPosition:BYTE;
      PatternOrder:ARRAY[0..127] OF BYTE;
      Tag:ARRAY[0..3] OF CHAR;
     END;

     PMiniMODPatternNote=^TMiniMODPatternNote;
     TMiniMODPatternNote=RECORD
      Note,Sample,Effect,EffectParameter:BYTE;
     END;
     TMiniMODPatternZeile=ARRAY[0..31] OF TMiniMODPatternNote;

     PMiniMODPattern=^TMiniMODPattern;
     TMiniMODPattern=ARRAY[0..63] OF TMiniMODPatternZeile;

     PMiniMODChannel=^TMiniMODChannel;
     TMiniMODChannel=RECORD
      Active:LONGBOOL;
      StandardPanning:LONGWORD;
      Panning:LONGWORD;
      Volume:LONGWORD;
      TempVolume:LONGWORD;
      Sample:TMiniMODSampleExt;
      Increment:LONGWORD;
      Period:LONGWORD;
      TempPeriod:LONGWORD;
      DestPeriod:LONGWORD;
      Effect:LONGWORD;
      EffectParameter:LONGWORD;
      EffectParameterHi:LONGWORD;
      EffectParameterLo:LONGWORD;
      NoteDelay:LONGWORD;
      VolumeSlideHi:LONGWORD;
      VolumeSlideLo:LONGWORD;
      PortSpeed:LONGWORD;
      ArpeggioPosition:LONGWORD;
      Arpeggio0:LONGWORD;
      Arpeggio1:LONGWORD;
      Arpeggio2:LONGWORD;
      VibratoParameterHi:LONGWORD;
      VibratoParameterLo:LONGINT;
      VibratoTyp:LONGWORD;
      VibratoPosition:LONGWORD;
      TremoloParameterHi:LONGWORD;
      TremoloParameterLo:LONGINT;
      TremoloTyp:LONGWORD;
      TremoloPosition:LONGWORD;
      RetrigCounter:LONGWORD;
      PatternLoopStart:LONGWORD;
      PatternLoopCounter:LONGWORD;
      PatternLoop:LONGBOOL;
      Surround:LONGBOOL;
      Filter:LONGBOOL;
      Glissando:LONGBOOL;
     END;

     TMiniMODTimingMode=(tmNTSC,tmPAL);

     TMiniMOD=CLASS(TERRAAudioMixer)
      PRIVATE
       ShouldStart:BOOLEAN;
       BPMSamples:LONGWORD;
       BPMSamplesZaehler:LONGWORD;

       Header15:TMiniMODHeader15;
       Header31:TMiniMODHeader31;

       HowManyChannels:BYTE;
       HowManySamples:BYTE;
       HowManyPatterns:BYTE;

       IsTrackActive:LONGBOOL;
       Filter:LONGBOOL;

       TrackLength:BYTE;
       RestartPosition:BYTE;

       PatternDelay:BYTE;
       FrameDelay:BYTE;

       //Buffer:TMiniMODBuffer;

       NoiseReductionLeft,NoiseReductionRight:LONGINT;

       PROCEDURE ClearData;

       FUNCTION AmigaToPC(W:WORD):WORD;

       FUNCTION GetPeriod(Note,FineTune:LONGWORD):LONGWORD;
       FUNCTION GetNote(Period:LONGWORD):LONGWORD;

       PROCEDURE SetTickVariables;
       PROCEDURE DoTick;

       PROCEDURE MixChannel(Channel:PMiniMODChannel;StartPosition,LengthCounter:LONGWORD);
       FUNCTION DoMix(StartPosition,LengthCounter:LONGWORD;VAR DoContinue:LONGBOOL):LONGWORD;
       PROCEDURE Render(DestBuffer:PAudioSample); Override;
      PUBLIC
       Sample:ARRAY[1..31] OF TMiniMODSample;
       SampleExt:ARRAY[1..31] OF TMiniMODSampleExt;
       PatternOrder:ARRAY[0..127] OF BYTE;
       Pattern:ARRAY[0..127] OF PMiniMODPattern;
       Channel:ARRAY[1..32] OF TMiniMODChannel;

       TimingMode:TMiniMODTimingMode;

       Speed:BYTE;
       BPM:BYTE;

       Tick:BYTE;

       CurrentPattenOrder:BYTE;
       CurrentRow:BYTE;
       CurrentPatten:BYTE;

       Title:STRING[20];

       CONSTRUCTOR Create(AFrequency:LONGWORD;WithStereo,In16Bit:BOOLEAN);
       DESTRUCTOR Destroy; OVERRIDE;

       PROCEDURE Clear;
       FUNCTION Load(DataPointer:POINTER;DataSize:LONGWORD):BOOLEAN;
       FUNCTION LoadFile(FileName:STRING):BOOLEAN;

       FUNCTION Play:BOOLEAN;
       PROCEDURE Stop;

       PROPERTY TrackActive:LONGBOOL READ IsTrackActive;
     END;

IMPLEMENTATION

CONST PeriodTab:ARRAY[0..15,1..60] OF WORD=((1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906,856,808,762,720,678,640,604,570,538,508,480,453,428,404,381,360,339,320,302,285,269,254,240,226,214,202,190,180,170,160,151,143,135,127,120,113,107,101,95,90,85,80,75,71,67,63,60,56),
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
                                            SinusTabelle:ARRAY[0..64-1] OF SHORTINT=(0,12,25,37,49,60,71,81,90,98,106,112,117,122,125,126,127,126,125,122,117,112,106,98,90,81,71,60,49,37,25,12,0,-12,-25,-37,-49,-60,-71,-81,-90,-98,-106,-112,-117,-122,-125,-126,-127,-126,-125,-122,-117,-112,-106,-98,-90,-81,-71,-60,-49,-37,-25,-12);
                                            RampDownTabelle:ARRAY[0..64-1] OF SHORTINT=(0,-4,-8,-12,-16,-20,-24,-28,-32,-36,-40,-44,-48,-52,-56,-60,-64,-68,-72,-76,-80,-84,-88,-92,-96,-100,-104,-108,-112,-116,-120,-124,127,123,119,115,111,107,103,99,95,91,87,83,79,75,71,67,63,59,55,51,47,43,39,35,31,27,23,19,15,11,7,3);
                                            SquareTabelle:ARRAY[0..64-1] OF SHORTINT=(127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127);
                                            RandomTabelle:ARRAY[0..64-1] OF SHORTINT=(88,-127,-43,88,102,41,-65,-94,125,20,-71,-86,-70,-32,-16,-96,17,72,107,-5,116,-69,-62,-40,10,-61,65,109,-18,-38,-13,-76,-23,88,21,-94,8,106,21,-112,6,109,20,-88,-30,9,-127,118,42,-34,89,-4,-51,-72,21,-29,112,123,84,-101,-92,98,-54,-95);


CONSTRUCTOR TMiniMOD.Create(AFrequency:LONGWORD;WithStereo,In16Bit:BOOLEAN);
VAR I:INTEGER;
BEGIN
 TimingMode:=tmNTSC;

 FOR I:=0 TO 31 DO BEGIN
  FILLCHAR(Sample[I],SIZEOF(TMiniMODSample),#0);
  FILLCHAR(SampleExt[I],SIZEOF(TMiniMODSampleExt),#0);
 END;
 FOR I:=0 TO 127 DO Pattern[I]:=NIL;
 ClearData;

 ShouldStart:=FALSE;
 IsTrackActive:=FALSE;

 INHERITED Create(AFrequency, CMiniMODBufferSize);
END;

DESTRUCTOR TMiniMOD.Destroy;
VAR I:INTEGER;
BEGIN
  Self.Release();
 ClearData;

 INHERITED Destroy;
END;

PROCEDURE TMiniMOD.ClearData;
VAR I:INTEGER;
BEGIN
 IsTrackActive:=FALSE;
 Filter:=FALSE;
 Title:='';
 FOR I:=1 TO 32 DO FILLCHAR(Channel[I],SIZEOF(TMiniMODChannel),#0);
 FOR I:=1 TO 31 DO BEGIN
  IF ASSIGNED(SampleExt[I].Data) THEN BEGIN
   FREEMEM(SampleExt[I].Data);
   SampleExt[I].Data:=NIL;
  END;
  FILLCHAR(Sample[I],SIZEOF(TMiniMODSample),#0);
  FILLCHAR(SampleExt[I],SIZEOF(TMiniMODSampleExt),#0);
  SampleExt[I].Number:=I;
 END;
 FILLCHAR(PatternOrder,256,#0);
 FOR I:=0 TO 127 DO IF ASSIGNED(Pattern[I]) THEN BEGIN
  FREEMEM(Pattern[I]);
  Pattern[I]:=NIL;
 END;
 FOR I:=1 TO 32 DO Channel[I].Active:=FALSE;
END;

PROCEDURE TMiniMOD.Clear;
BEGIN
 Self.Enter;
 ClearData;
 Self.Leave;
END;

FUNCTION TMiniMOD.AmigaToPC(W:WORD):WORD; {$IFDEF CPU386} ASSEMBLER;
ASM
 MOV AX,W
 XCHG AH,AL
END;
{$ELSE}
BEGIN
 RESULT:=((W AND $FF) SHL 8) OR ((W SHR 8) AND $FF);
END;
{$ENDIF}

FUNCTION TMiniMOD.GetPeriod(Note,FineTune:LONGWORD):LONGWORD;
BEGIN
 IF Note<60 THEN bEGIN
  IF FineTune<16 THEN BEGIN
   RESULT:=PeriodTab[FineTune,Note];
  END ELSE BEGIN
   RESULT:=PeriodTab[0,Note];
  END;
 END ELSE BEGIN
  RESULT:=0;
 END;
END;

FUNCTION TMiniMOD.GetNote(Period:LONGWORD):LONGWORD;
VAR Note:LONGWORD;
BEGIN
 Note:=0;
 IF Period>0 THEN BEGIN
  WHILE TRUE DO BEGIN
   INC(Note);
   IF Period>=PeriodTab[0,Note] THEN BEGIN
    BREAK;
   END ELSE IF (Note>60) THEN BEGIN
    Note:=0;
    BREAK;
   END;
  END;
 END;
 RESULT:=Note;
END;

FUNCTION TMiniMOD.Load(DataPointer:POINTER;DataSize:LONGWORD):BOOLEAN;
VAR TotalSampleLength,PatternStartPosition,PatternA,PatternB:LONGWORD;
    ASample:TMiniMODSample;
    OldMODFormat:LONGBOOL;
    I,J,K:INTEGER;
    Buffer:ARRAY[1..4] OF BYTE;
    DataPosition:LONGWORD;
    S:SHORTINT;
 FUNCTION Read(VAR Buffer;LengthCounter:LONGWORD):LONGWORD;
 VAR SourcePointer,DestPointer:POINTER;
 BEGIN
  RESULT:=0;
  SourcePointer:=POINTER(LONGWORD(LONGWORD(DataPointer)+DataPosition));
  DestPointer:=@Buffer;
  WHILE (DataPosition<DataSize) AND (RESULT<LengthCounter) DO BEGIN
   BYTE(DestPointer^):=BYTE(SourcePointer^);
   INC(LONGWORD(SourcePointer));
   INC(LONGWORD(DestPointer));
   INC(DataPosition);
   INC(RESULT);
  END;
 END;
BEGIN
 RESULT:=FALSE;
 Self.Enter;
 ClearData;
 OldMODFormat:=FALSE;

 HowManyChannels:=0;
 HowManySamples:=31;

 Title:='';

 DataPosition:=0;
 Read(Header31,SIZEOF(TMiniMODHeader31));

 IF Header31.Tag[0]='P' THEN IF Header31.Tag[1]='P' THEN EXIT;
 IF (Header31.Tag='M.K.') OR (Header31.Tag='M!K!') OR (Header31.Tag='M&K!') OR (Header31.Tag='N.T.') THEN BEGIN
  HowManyChannels:=4;
 END ELSE IF (Header31.Tag='OCTA') OR (Header31.Tag='OKTA') OR (Header31.Tag='CD81') OR (Header31.Tag='WOW!') THEN BEGIN  HowManyChannels:=8;
  HowManyChannels:=8;
 END ELSE IF (Header31.Tag[0]='T') AND (Header31.Tag[1]='D') AND (Header31.Tag[2]='Z') THEN BEGIN
  HowManyChannels:=ORD(Header31.Tag[3])-48;
 END ELSE IF (Header31.Tag[0]='F') AND (Header31.Tag[1]='L') AND (Header31.Tag[2]='T') THEN BEGIN
  HowManyChannels:=ORD(Header31.Tag[3])-48;
  IF NOT HowManyChannels IN [1..8] THEN HowManyChannels:=0;
 END ELSE IF (Header31.Tag[0]='E') AND (Header31.Tag[1]='X') AND (Header31.Tag[2]='O') THEN BEGIN
  HowManyChannels:=ORD(Header31.Tag[3])-48;
  IF NOT HowManyChannels IN [1..8] THEN HowManyChannels:=0;
 END ELSE IF (Header31.Tag[1]='C') AND (Header31.Tag[2]='H') AND (Header31.Tag[3]='N') THEN BEGIN
  HowManyChannels:=ORD(Header31.Tag[0])-48;
 END ELSE IF (Header31.Tag[2]='C') AND (Header31.Tag[3]='N') THEN BEGIN
  HowManyChannels:=((ORD(Header31.Tag[0])-48)*10)+(ORD(Header31.Tag[1])-48);
 END ELSE IF (Header31.Tag[2]='C') AND (Header31.Tag[3]='H') THEN BEGIN
  HowManyChannels:=((ORD(Header31.Tag[0])-48)*10)+(ORD(Header31.Tag[1])-48);
 END ELSE IF (Header31.Tag[0]='E') AND (Header31.Tag[1]='X') THEN BEGIN
  HowManyChannels:=((ORD(Header31.Tag[2])-48)*10)+(ORD(Header31.Tag[3])-48);
 END;
 IF HowManyChannels=0 THEN BEGIN
  HowManySamples:=15;
  HowManyChannels:=4;
  OldMODFormat:=TRUE;

  DataPosition:=0;
  Read(Header15,SIZEOF(TMiniMODHeader15));

  Title:=Header15.Title;

  TrackLength:=Header15.TrackLength;
  RestartPosition:=Header15.RestartPosition;

  MOVE(Header15.PatternOrder,PatternOrder,128);
 END ELSE BEGiN
  Title:=Header31.Title;

  TrackLength:=Header31.TrackLength;
  RestartPosition:=Header31.RestartPosition;

  MOVE(Header31.PatternOrder,PatternOrder,128);
 END;

 IF RestartPosition>TrackLength THEN RestartPosition:=0;

 TotalSampleLength:=0;
 FOR I:=1 TO HowManySamples DO BEGIN
  IF OldMODFormat THEN BEGIN
   ASample:=Header15.Samples[I];
  END ELSE BEGIN
   ASample:=Header31.Samples[I];
  END;
  Sample[I]:=ASample;
  SampleExt[I].Number:=I;
  SampleExt[I].Data:=NIL;
  SampleExt[I].LengthCounter:=AmigaToPC(ASample.LengthCounter) SHL 1;
  SampleExt[I].LoopStart:=AmigaToPC(ASample.LoopStart) SHL 1;
  SampleExt[I].LoopLength:=AmigaToPC(ASample.LoopLength) SHL 1;
  SampleExt[I].Loop:=FALSE;
  SampleExt[I].FineTune:=ASample.FineTune;
  SampleExt[I].Position:=0;
  SampleExt[I].PositionExt:=0;
  INC(TotalSampleLength,SampleExt[I].LengthCounter);
 END;

 IF OldMODFormat THEN BEGIN
  PatternStartPosition:=SIZEOF(TMiniMODHeader15);
 END ELSE BEGIN
  PatternStartPosition:=SIZEOF(TMiniMODHeader31);
 END;

 PatternB:=0;
 FOR I:=0 to 127 do IF PatternOrder[I]<=127 THEN BEGIN
  IF PatternOrder[I]>PatternB THEN PatternB:=PatternOrder[I];
 END;
 INC(PatternB);

 IF HowManyChannels=4 THEN BEGIN
  IF LONGWORD(PatternStartPosition+(PatternB*8*4*64)+TotalSampleLength)=DataSize THEN BEGIN
   HowManyChannels:=8;
  END;
 END;

 PatternA:=(LONGWORD(DataSize)-PatternStartPosition-TotalSampleLength) DIV ((((1024 DIV 4)*HowManyChannels) DIV 64)*64);
 HowManyPatterns:=PatternA;

 IF OldMODFormat OR (((PatternA>64) AND (PatternB<=64))) THEN BEGIN
  PatternA:=PatternB;
 END;
 IF PatternA=PatternB THEN BEGIN
  HowManyPatterns:=PatternA;
 END ELSE BEGIN
  IF PatternB<PatternA THEN BEGIN
   HowManyPatterns:=PatternB;
  END ELSE IF ((PatternB>64) AND ((Header31.Tag<>'M!K!') AND NOT OldMODFormat)) THEN BEGIN
   HowManyPatterns:=PatternA;
  END ELSE BEGIN
   HowManyPatterns:=PatternB;
  END;
 END;
 IF HowManyPatterns>128 THEN HowManyPatterns:=128;

 IF LONGWORD(PatternStartPosition+(HowManyPatterns*HowManyChannels*4*64)+TotalSampleLength)<>DataSize THEN BEGIN
  ClearData;
  Self.Leave;
  EXIT;
 END;

 FOR I:=0 TO (HowManyChannels SHR 2)-1 DO BEGIN
  Channel[(I*4)+1].StandardPanning:=0;
  Channel[(I*4)+2].StandardPanning:=255;
  Channel[(I*4)+3].StandardPanning:=255;
  Channel[(I*4)+4].StandardPanning:=0;
 END;

 FOR I:=0 TO HowManyPatterns-1 DO BEGIN
  GETMEM(Pattern[I],SIZEOF(TMiniMODPattern));
  FOR J:=0 TO 63 DO BEGIN
   FOR K:=0 TO HowManyChannels-1 DO BEGIN
    Read(Buffer,4);
    Pattern[I]^[J,K].Note:=GetNote(((Buffer[1] AND $0F) SHL 8) OR Buffer[2]);
    Pattern[I]^[J,K].Sample:=(Buffer[1] AND $F0) OR (Buffer[3] SHR 4);
    Pattern[I]^[J,K].Effect:=Buffer[3] AND $F;
    Pattern[I]^[J,K].EffectParameter:=Buffer[4];
   END;
  END;
 END;

 FOR I:=1 TO HowManySamples DO BEGIN
  IF SampleExt[I].LengthCounter>1 THEN BEGIN
   IF SampleExt[I].LoopStart>SampleExt[I].LengthCounter THEN BEGIN
    SampleExt[I].LoopStart:=0;
    SampleExt[I].LoopLength:=0;
   END;
   IF (SampleExt[I].LoopStart+SampleExt[I].LoopLength)>SampleExt[I].LengthCounter THEN BEGIN
    SampleExt[I].LoopLength:=SampleExt[I].LengthCounter-SampleExt[I].LoopStart;
    IF SampleExt[I].LoopLength<2 THEN SampleExt[I].LoopLength:=2;
   END;
   SampleExt[I].Loop:=(SampleExt[I].LoopLength>2);
   IF SampleExt[I].LengthCounter>0 THEN BEGIN
    GETMEM(SampleExt[I].Data,(SampleExt[I].LengthCounter+CMiniMODSampleMoreSize)*SIZEOF(INTEGER));
    FILLCHAR(SampleExt[I].Data^,(SampleExt[I].LengthCounter+CMiniMODSampleMoreSize)*SIZEOF(INTEGER),0);
    FOR J:=1 TO SampleExt[I].LengthCounter DO BEGIN
     Read(S,SIZEOF(SHORTINT));
     SampleExt[I].Data^[J]:=S*256;
    END;
    FOR J:=SampleExt[I].LengthCounter+1 TO SampleExt[I].LengthCounter+CMiniMODSampleMoreSize-1 DO BEGIN
     SampleExt[I].Data^[J]:=SampleExt[I].Data^[SampleExt[I].LengthCounter-1];
    END;
   END;
   SampleExt[I].LoopEnd:=SampleExt[I].LoopStart+SampleExt[I].LoopLength;
  END;
 END;

 IsTrackActive:=TRUE;
 Self.Leave;
 RESULT:=TRUE;
END;

FUNCTION TMiniMOD.LoadFile(FileName:STRING):BOOLEAN;
VAR Datei:FILE;
    Data:POINTER;
    Size:LONGWORD;
BEGIN
 RESULT:=FALSE;
 ASSIGNFILE(Datei,FileName);
 {$I-}RESET(Datei,1);{$I+}
 IF IOResult<>0 THEN EXIT;
 Size:=FILESIZE(Datei);
 GETMEM(Data,Size);
 BLOCKREAD(Datei,Data^,Size);
 CLOSEFILE(Datei);
 RESULT:=Load(Data,Size);
 FREEMEM(Data);
END;

PROCEDURE TMiniMOD.SetTickVariables;
BEGIN
 BPMSamples:=(Self.Frequency*5*128) DIV (BPM SHL 8);
 BPMSamplesZaehler:=BPMSamples;
END;

PROCEDURE TMiniMOD.DoTick;
VAR PatternNote:PMiniMODPatternNote;
    SampleNr,Period,Effect,EffectParameter,PatternChannel,ChannelNr,Note,
    TheNote,FineTune,NewCurrentPattern,NewCurrentRow,Hz:LONGWORD;
    Value,Value1,Value2:LONGINT;
    Jump:LONGBOOL;
BEGIN
  IntToString(Self.BPM);
 INC(Tick);
 IF Tick>=((Speed*(PatternDelay+1))+FrameDelay) THEN BEGIN
  Tick:=0;
  PatternDelay:=0;
  FrameDelay:=0;

  NewCurrentPattern:=0;
  NewCurrentRow:=0;
  Jump:=FALSE;

  IF CurrentRow>=64 THEN BEGIN
   INC(CurrentPattenOrder);
   CurrentRow:=0;
  END;
  IF CurrentPattenOrder>=TrackLength THEN BEGIN
   CurrentPattenOrder:=RestartPosition;
   CurrentRow:=0;
  END;

  IF CurrentPattenOrder<128 THEN BEGIN
   CurrentPatten:=PatternOrder[CurrentPattenOrder];
   IF CurrentPatten<128 THEN BEGIN
    FOR PatternChannel:=1 TO HowManyChannels DO BEGIN
     PatternNote:=@Pattern[CurrentPatten]^[CurrentRow,PatternChannel-1];
     Note:=PatternNote.Note;
     SampleNr:=PatternNote.Sample;
     Effect:=PatternNote.Effect;
     EffectParameter:=PatternNote.EffectParameter;

     Channel[PatternChannel].Effect:=Effect;
     Channel[PatternChannel].EffectParameter:=EffectParameter;
     Channel[PatternChannel].EffectParameterHi:=EffectParameter SHR 4;
     Channel[PatternChannel].EffectParameterLo:=EffectParameter AND $F;
     Channel[PatternChannel].VolumeSlideHi:=0;
     Channel[PatternChannel].VolumeSlideLo:=0;

     IF (SampleNr>0) AND (SampleNr<=HowManySamples) THEN BEGIN
      FineTune:=Sample[SampleNr].FineTune;
     END ELSE BEGIN
      FineTune:=0;
     END;
     IF Note<>0 THEN BEGIN
      Period:=GetPeriod(Note,FineTune);
     END ELSE BEGIN
      Period:=0;
     END;

     IF (SampleNr>0) AND (SampleNr<=HowManySamples) THEN BEGIN
      IF ASSIGNED(SampleExt[SampleNr].Data) THEN BEGIN
       Channel[PatternChannel].Active:=TRUE;
       Channel[PatternChannel].Volume:=Sample[SampleNr].Volume;
       IF Channel[PatternChannel].Volume=0 THEN Channel[PatternChannel].Volume:=64;
       Channel[PatternChannel].Sample:=SampleExt[SampleNr];
       Channel[PatternChannel].Sample.Position:=0;
       Channel[PatternChannel].Sample.PositionExt:=0;
      END ELSE BEGIN
       Channel[PatternChannel].Active:=FALSE;
      END;
     END;
     IF Period<>0 THEN BEGIN
      IF (Effect<>$3) OR (Effect<>$5) THEN BEGIN
       Channel[PatternChannel].Period:=Period;
       Channel[PatternChannel].TempPeriod:=Period;
      END;
      IF ASSIGNED(Channel[PatternChannel].Sample.Data) THEN BEGIN
       Channel[PatternChannel].Active:=TRUE;
       Channel[PatternChannel].Volume:=Sample[Channel[PatternChannel].Sample.Number].Volume;
       IF Channel[PatternChannel].Volume=0 THEN Channel[PatternChannel].Volume:=64;
       Channel[PatternChannel].Sample.Position:=0;
       Channel[PatternChannel].Sample.PositionExt:=0;
      END;
     END;

     CASE Effect OF
      $0:BEGIN
       IF EffectParameter<>0 THEN BEGIN
        TheNote:=GetNote(Channel[PatternChannel].Period);
        Channel[PatternChannel].ArpeggioPosition:=0;
        Channel[PatternChannel].Arpeggio0:=Channel[PatternChannel].Period;
        IF (TheNote+(EffectParameter SHR 4))<CMiniMODMaxPeriod THEN BEGIN
         Channel[PatternChannel].Arpeggio1:=GetPeriod(TheNote+(EffectParameter SHR 4),Channel[PatternChannel].Sample.FineTune);
        END ELSE BEGIN
         Channel[PatternChannel].Arpeggio1:=CMiniMODMaxPeriod;
        END;
        IF (TheNote+(EffectParameter AND $F))<CMiniMODMaxPeriod THEN BEGIN
         Channel[PatternChannel].Arpeggio2:=GetPeriod(TheNote+(EffectParameter AND $F),Channel[PatternChannel].Sample.FineTune);
        END ELSE BEGIN
         Channel[PatternChannel].Arpeggio2:=CMiniMODMaxPeriod;
        END;
        IF Channel[PatternChannel].Arpeggio0>CMiniMODMaxPeriod THEN Channel[PatternChannel].Arpeggio0:=CMiniMODMaxPeriod;
        IF Channel[PatternChannel].Arpeggio1>CMiniMODMaxPeriod THEN Channel[PatternChannel].Arpeggio1:=CMiniMODMaxPeriod;
        IF Channel[PatternChannel].Arpeggio2>CMiniMODMaxPeriod THEN Channel[PatternChannel].Arpeggio2:=CMiniMODMaxPeriod;
       END;
      END;
      $1:BEGIN // Port Down
       Channel[PatternChannel].PortSpeed:=EffectParameter;
      END;
      $2:BEGIN // Port Up
       Channel[PatternChannel].PortSpeed:=EffectParameter;
      END;
      $3:BEGIN // Porta to Note
       IF Period<>0 THEN Channel[PatternChannel].DestPeriod:=Period;
       IF Channel[PatternChannel].TempPeriod=0 THEN Channel[PatternChannel].TempPeriod:=CMiniMODMinPeriod;
       IF EffectParameter<>0 THEN Channel[PatternChannel].PortSpeed:=EffectParameter;
       IF Channel[PatternChannel].DestPeriod<CMiniMODMinPeriod THEN Channel[PatternChannel].DestPeriod:=CMiniMODMinPeriod;
       IF Channel[PatternChannel].DestPeriod>CMiniMODMaxPeriod THEN Channel[PatternChannel].DestPeriod:=CMiniMODMaxPeriod;
      END;
      $4:BEGIN // Vibrato
       IF (EffectParameter SHR 4)<>0 THEN Channel[PatternChannel].VibratoParameterHi:=EffectParameter SHR 4;
       IF (EffectParameter AND $F)<>0 THEN Channel[PatternChannel].VibratoParameterLo:=EffectParameter AND $F;
       IF (Channel[PatternChannel].VibratoTyp AND 4)=0 THEN BEGIN
        Channel[PatternChannel].VibratoPosition:=0;
       END;
      END;
      $5, // Porta to Note und Volume Slide
      $6, // Vibrato and Volume Slide
      $A:BEGIN // Volume Slide
       IF (EffectParameter AND $F0)<>0 THEN BEGIN
        Channel[PatternChannel].VolumeSlideHi:=EffectParameter SHR 4;
       END;
       IF (EffectParameter AND $F)<>0 THEN BEGIN
        Channel[PatternChannel].VolumeSlideLo:=EffectParameter AND $F;
       END;
      END;
      $7:BEGIN // Tremolo
       Channel[PatternChannel].TempVolume:=Channel[PatternChannel].Volume;
       IF (EffectParameter SHR 4)<>0 THEN Channel[PatternChannel].TremoloParameterHi:=EffectParameter SHR 4;
       IF (EffectParameter AND $F)<>0 THEN Channel[PatternChannel].TremoloParameterLo:=EffectParameter AND $F;
       IF (Channel[PatternChannel].TremoloTyp AND 4)=0 THEN BEGIN
        Channel[PatternChannel].TremoloPosition:=0;
       END;
      END;
      $8:BEGIN // Set Panning
       IF Channel[PatternChannel].EffectParameter<=$80 THEN BEGIN
        Channel[PatternChannel].Panning:=Channel[PatternChannel].EffectParameter SHL 1;
        Channel[PatternChannel].Surround:=FALSE;
       END ELSE IF Channel[PatternChannel].EffectParameter=$A4 THEN BEGIN
        Channel[PatternChannel].Surround:=TRUE;
       END;
      END;
      $9:BEGIN // Set Sample Position
       Channel[PatternChannel].Sample.Position:=EffectParameter SHL 8;
       Channel[PatternChannel].Sample.PositionExt:=Channel[PatternChannel].Sample.Position SHL CMiniMODPositionShift;
      END;
      $B:BEGIN // Pattern Jump
       NewCurrentPattern:=EffectParameter;
       IF NOT Jump THEN NewCurrentRow:=0;
       Jump:=TRUE;
      END;
      $C:BEGIN // Set Volume
       Channel[PatternChannel].Volume:=EffectParameter;
      END;
      $D:BEGIN // Next Pattern
       IF NOT Jump THEN NewCurrentPattern:=CurrentPattenOrder+1;
       NewCurrentRow:=(10*(EffectParameter SHR 4))+(EffectParameter AND $F);
       Jump:=TRUE;
      END;
      $E:BEGIN // Extended Effects
       CASE Channel[PatternChannel].EffectParameterHi OF
        $0:BEGIN // Filter On/Off
         Channel[PatternChannel].Filter:=(Channel[PatternChannel].EffectParameterLo AND 1)<>0;
         Filter:=(Channel[PatternChannel].EffectParameterLo AND 1)<>0;
        END;
        $1:BEGIN // Fine Period Slide Up
         IF Channel[PatternChannel].TempPeriod>Channel[PatternChannel].EffectParameterLo THEN BEGIN
          DEC(Channel[PatternChannel].TempPeriod,Channel[PatternChannel].EffectParameterLo);
          IF Channel[PatternChannel].TempPeriod<CMiniMODMinPeriod THEN Channel[PatternChannel].TempPeriod:=CMiniMODMinPeriod;
         END ELSE BEGIN
          Channel[PatternChannel].TempPeriod:=CMiniMODMinPeriod;
         END;
        END;
        $2:BEGIN // Fine Period Slide Down
         INC(Channel[PatternChannel].TempPeriod,Channel[PatternChannel].EffectParameterLo);
         IF Channel[PatternChannel].TempPeriod>CMiniMODMaxPeriod THEN Channel[PatternChannel].TempPeriod:=CMiniMODMaxPeriod;
        END;
        $3:BEGIN // Glissando On/Off
         Channel[PatternChannel].Glissando:=(Channel[PatternChannel].EffectParameterLo AND 1)<>0;
        END;
        $4:BEGIN // Set Vibrato Waveform
         Channel[PatternChannel].VibratoTyp:=Channel[PatternChannel].EffectParameterLo AND 7;
        END;
        $5:BEGIN // Set FineTune
         Channel[PatternChannel].TempPeriod:=GetPeriod(Channel[PatternChannel].EffectParameterLo,GetNote(Channel[PatternChannel].Period));
        END;
        $6:BEGIN // Pattern Loop
         IF Channel[PatternChannel].EffectParameterLo=0 THEN BEGIN
          IF NOT Channel[PatternChannel].PatternLoop THEN BEGIN
           Channel[PatternChannel].PatternLoopStart:=CurrentRow;
          END;
         END ELSE BEGIN
          IF Channel[PatternChannel].PatternLoop THEN BEGIN
           IF Channel[PatternChannel].PatternLoopCounter=0 THEN BEGIN
            Channel[PatternChannel].PatternLoop:=FALSE;
           END ELSE BEGIN
            DEC(Channel[PatternChannel].PatternLoopCounter);
            NewCurrentPattern:=CurrentPattenOrder;
            NewCurrentRow:=Channel[PatternChannel].PatternLoopStart;
            Jump:=TRUE;
           END;
          END ELSE BEGIN
           Channel[PatternChannel].PatternLoopCounter:=Channel[PatternChannel].EffectParameterLo;
           Channel[PatternChannel].PatternLoop:=TRUE;
           NewCurrentPattern:=CurrentPattenOrder;
           NewCurrentRow:=Channel[PatternChannel].PatternLoopStart;
           Jump:=TRUE;
          END;
         END;
        END;
        $7:BEGIN // Set Tremolo Waveform
         Channel[PatternChannel].TremoloTyp:=Channel[PatternChannel].EffectParameterLo AND 7;
        END;
        $8:BEGIN // Set Panning
         Channel[PatternChannel].Panning:=Channel[PatternChannel].EffectParameterLo SHL 4;
         Channel[PatternChannel].Surround:=FALSE;
        END;
        $9:BEGIN // Retrig Note
         Channel[PatternChannel].RetrigCounter:=Channel[PatternChannel].EffectParameterLo;
        END;
        $A:BEGIN // Fine Volume Slide Up
         INC(Channel[PatternChannel].Volume,Channel[PatternChannel].EffectParameterLo);
         IF Channel[PatternChannel].Volume>64 THEN Channel[PatternChannel].Volume:=64;
        END;
        $B:BEGIN // Fine Volume Slide Down
         IF Channel[PatternChannel].Volume>Channel[PatternChannel].EffectParameterLo THEN BEGIN
          DEC(Channel[PatternChannel].Volume,Channel[PatternChannel].EffectParameterLo);
         END ELSE BEGIN
          Channel[PatternChannel].Volume:=0;
         END;
        END;
        $C:BEGIN // Sample Cut
         IF Channel[PatternChannel].EffectParameterLo=0 THEN Channel[PatternChannel].Volume:=0;
        END;
        $D:BEGIN // Note Delay
         IF (Channel[PatternChannel].Active) AND (Channel[PatternChannel].EffectParameterLo<>0) THEN BEGIN
          Channel[PatternChannel].NoteDelay:=Channel[PatternChannel].EffectParameterLo;
          Channel[PatternChannel].Active:=FALSE;
         END ELSE BEGIN
          Channel[PatternChannel].NoteDelay:=0;
         END;
        END;
        $E:BEGIN // Pattern Delay
         PatternDelay:=Channel[PatternChannel].EffectParameterLo;
        END;
       END;
      END;
      $F:BEGIN // Set Speed or BPM
       IF EffectParameter<32 THEN BEGIN
        Speed:=EffectParameter;
       END ELSE BEGIN
        BPM:=EffectParameter;
       END;
      END;
     END;
    END;
   END;
  END;

  IF Jump THEN BEGIN
   CurrentPattenOrder:=NewCurrentPattern;
   CurrentRow:=NewCurrentRow;
  END ELSE BEGIN
   INC(CurrentRow);
  END;
 END;

 CASE TimingMode OF
  tmNTSC:Hz:=3579545;
  tmPAL:Hz:=3546895;
  ELSE Hz:=0;
 END;

 // Process Channel values
 FOR ChannelNr:=1 TO HowManyChannels DO BEGIN
  IF Channel[ChannelNr].Active THEN BEGIN
   CASE Channel[ChannelNr].Effect OF
    $0:IF Tick>0 THEN BEGIN
     // Arpeggio
     IF Channel[ChannelNr].EffectParameter<>0 THEN BEGIN
      CASE Channel[ChannelNr].ArpeggioPosition OF
       0:Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio0;
       1:Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio1;
       ELSE Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].Arpeggio2;
      END;
      Channel[ChannelNr].ArpeggioPosition:=(Channel[ChannelNr].ArpeggioPosition+1) MOD 3;
     END;
    END;
    $1:IF ((Channel[ChannelNr].EffectParameterHi<14) AND (Tick<>0)) OR ((Channel[ChannelNr].EffectParameterHi=15) AND (Tick=0)) THEN BEGIN
     // Port Up
     IF Channel[ChannelNr].TempPeriod>Channel[ChannelNr].PortSpeed THEN BEGIN
      DEC(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
      IF Channel[ChannelNr].TempPeriod<CMiniMODMinPeriod THEN Channel[ChannelNr].TempPeriod:=CMiniMODMinPeriod;
     END ELSE BEGIN
      Channel[ChannelNr].TempPeriod:=CMiniMODMinPeriod;
      Channel[ChannelNr].PortSpeed:=0;
     END;
    END;
    $2:IF ((Channel[ChannelNr].EffectParameterHi<14) AND (Tick<>0)) OR ((Channel[ChannelNr].EffectParameterHi=15) AND (Tick=0)) THEN BEGIN
     // Port Down
     IF (Channel[ChannelNr].TempPeriod+Channel[ChannelNr].PortSpeed)<CMiniMODMaxPeriod THEN BEGIN
      INC(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
      IF Channel[ChannelNr].TempPeriod>CMiniMODMaxPeriod THEN Channel[ChannelNr].TempPeriod:=CMiniMODMaxPeriod;
     END ELSE BEGIN
      Channel[ChannelNr].TempPeriod:=CMiniMODMaxPeriod;
      Channel[ChannelNr].PortSpeed:=0;
     END;
    END;
    $3,$4,$5,$6,$A:IF Tick<>0 THEN BEGIN
     IF (Channel[ChannelNr].Effect=$3) OR (Channel[ChannelNr].Effect=$5) THEN BEGIN
      // Porta to Note
      IF Channel[ChannelNr].TempPeriod<Channel[ChannelNr].DestPeriod THEN BEGIN
       INC(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
       IF Channel[ChannelNr].TempPeriod>Channel[ChannelNr].DestPeriod THEN BEGIN
        Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].DestPeriod;
        Channel[ChannelNr].PortSpeed:=0;
       END;
       IF (Channel[ChannelNr].TempPeriod<>Channel[ChannelNr].DestPeriod) AND Channel[ChannelNr].Glissando THEN BEGIN
        Value:=GetNote(Channel[ChannelNr].TempPeriod);
        IF Value>1 THEN BEGIN
         Value1:=Channel[ChannelNr].TempPeriod-GetPeriod(Value-1,Channel[ChannelNr].Sample.FineTune);
        END ELSE BEGIN
         Value1:=Channel[ChannelNr].TempPeriod-CMiniMODMinPeriod;
        END;
        Value2:=GetPeriod(Value,Channel[ChannelNr].Sample.FineTune)-Channel[ChannelNr].TempPeriod;
        IF Value2>Value1 THEN BEGIN
         Value:=LONGINT(Channel[ChannelNr].TempPeriod)-Value2;
        END ELSE BEGIN
         Value:=LONGINT(Channel[ChannelNr].TempPeriod)+Value1;
        END;
        IF Value<CMiniMODMinPeriod THEN Value:=CMiniMODMinPeriod;
        IF Value>CMiniMODMaxPeriod THEN Value:=CMiniMODMaxPeriod;
        Channel[ChannelNr].TempPeriod:=Value;
       END;
      END ELSE IF Channel[ChannelNr].TempPeriod>Channel[ChannelNr].DestPeriod THEN BEGIN
       DEC(Channel[ChannelNr].TempPeriod,Channel[ChannelNr].PortSpeed);
       IF Channel[ChannelNr].TempPeriod<Channel[ChannelNr].DestPeriod THEN BEGIN
        Channel[ChannelNr].TempPeriod:=Channel[ChannelNr].DestPeriod;
        Channel[ChannelNr].PortSpeed:=0;
       END;
       IF (Channel[ChannelNr].TempPeriod<>Channel[ChannelNr].DestPeriod) AND Channel[ChannelNr].Glissando THEN BEGIN
        Value:=GetNote(Channel[ChannelNr].TempPeriod);
        IF Value>1 THEN BEGIN
         Value1:=Channel[ChannelNr].TempPeriod-GetPeriod(Value-1,Channel[ChannelNr].Sample.FineTune);
        END ELSE BEGIN
         Value1:=Channel[ChannelNr].TempPeriod-CMiniMODMinPeriod;
        END;
        Value2:=GetPeriod(Value,Channel[ChannelNr].Sample.FineTune)-Channel[ChannelNr].TempPeriod;
        IF Value2>Value1 THEN BEGIN
         Value:=LONGINT(Channel[ChannelNr].TempPeriod)+Value2;
        END ELSE BEGIN
         Value:=LONGINT(Channel[ChannelNr].TempPeriod)-Value1;
        END;
        IF Value<CMiniMODMinPeriod THEN Value:=CMiniMODMinPeriod;
        IF Value>CMiniMODMaxPeriod THEN Value:=CMiniMODMaxPeriod;
        Channel[ChannelNr].TempPeriod:=Value;
       END;
      END;
     END;
     IF (Channel[ChannelNr].Effect=$4) OR (Channel[ChannelNr].Effect=$6) THEN BEGIN
      // Vibrato
      CASE Channel[ChannelNr].VibratoTyp AND 3 OF
       0:Value:=RampDownTabelle[Channel[ChannelNr].VibratoPosition AND $3F];
       1:Value:=SquareTabelle[Channel[ChannelNr].VibratoPosition AND $3F];
       2:Value:=RandomTabelle[Channel[ChannelNr].VibratoPosition AND $3F];
       ELSE Value:=SinusTabelle[Channel[ChannelNr].VibratoPosition AND $3F];
      END;
      Value:=LONGINT(Channel[ChannelNr].Period)+((Value*Channel[ChannelNr].VibratoParameterLo) DIV 64);
      IF Value<CMiniMODMinPeriod THEN Value:=CMiniMODMinPeriod;
      IF Value>CMiniMODMaxPeriod THEN Value:=CMiniMODMaxPeriod;
      Channel[ChannelNr].TempPeriod:=Value;
      IF Tick<>0 THEN BEGIN
       Channel[ChannelNr].VibratoPosition:=(Channel[ChannelNr].VibratoPosition+Channel[ChannelNr].VibratoParameterHi) AND $3F;
      END;
     END;
     IF (Channel[ChannelNr].Effect<>$3) AND (Channel[ChannelNr].Effect<>$4) THEN BEGIN
      // Volume Slide
      IF Channel[ChannelNr].VolumeSlideLo<>0 THEN BEGIN
       IF Channel[ChannelNr].Volume>Channel[ChannelNr].VolumeSlideLo THEN BEGIN
        DEC(Channel[ChannelNr].Volume,Channel[ChannelNr].VolumeSlideLo);
       END ELSE BEGIN
        Channel[ChannelNr].Volume:=0;
       END;
      END;
      IF Channel[ChannelNr].VolumeSlideHi<>0 THEN BEGIN
       IF (Channel[ChannelNr].Volume+Channel[ChannelNr].VolumeSlideHi)<64 THEN BEGIN
        INC(Channel[ChannelNr].Volume,Channel[ChannelNr].VolumeSlideHi);
       END ELSE BEGIN
        Channel[ChannelNr].Volume:=64;
       END;
      END;
     END;
    END;
    $7:BEGIN
     // Tremolo
     CASE Channel[ChannelNr].TremoloTyp AND 3 OF
      0:Value:=RampDownTabelle[Channel[ChannelNr].TremoloPosition AND $3F];
      1:Value:=SquareTabelle[Channel[ChannelNr].TremoloPosition AND $3F];
      2:Value:=RandomTabelle[Channel[ChannelNr].TremoloPosition AND $3F];
      ELSE Value:=SinusTabelle[Channel[ChannelNr].TremoloPosition AND $3F];
     END;
     Value:=LONGINT(Channel[ChannelNr].TempVolume)+((Value*Channel[ChannelNr].TremoloParameterLo) DIV 64);
     IF Value<0 THEN Value:=0;
     IF Value>64 THEN Value:=64;
     Channel[ChannelNr].Volume:=Value;
     IF Tick<>0 THEN BEGIN
      Channel[ChannelNr].TremoloPosition:=(Channel[ChannelNr].TremoloPosition+Channel[ChannelNr].TremoloParameterHi) AND $3F;
     END;
    END;
    $9:BEGIN
     // Retrig Note
     IF Channel[ChannelNr].RetrigCounter<>0 THEN BEGIN
      INC(Channel[ChannelNr].RetrigCounter);
      IF Channel[ChannelNr].EffectParameterLo<>0 THEN BEGIN
       IF (Channel[ChannelNr].RetrigCounter MOD Channel[ChannelNr].EffectParameterLo)=0 THEN BEGIN
        IF ASSIGNED(Channel[ChannelNr].Sample.Data) THEN BEGIN
         Channel[ChannelNr].Active:=TRUE;
         Channel[ChannelNr].Sample.Position:=0;
         Channel[ChannelNr].Sample.PositionExt:=0;
        END;
       END;
      END;
     END;
    END;
    $E:BEGIN
     // Extended Effekte
     CASE Channel[ChannelNr].EffectParameterHi OF
      $C:BEGIN // Sample Cut
       IF Channel[ChannelNr].EffectParameterLo=Tick THEN Channel[ChannelNr].Volume:=0;
      END;
      $D:BEGIN // Note Delay
       IF Channel[ChannelNr].NoteDelay<>0 THEN BEGIN
        IF Channel[ChannelNr].NoteDelay=Tick THEN BEGIN
         Channel[ChannelNr].Active:=TRUE;
         Channel[ChannelNr].NoteDelay:=0;
        END;
       END;
      END;
     END;
    END;
   END;
   IF Channel[ChannelNr].TempPeriod>0 THEN BEGIN
    Channel[ChannelNr].Increment:=((Hz DIV Channel[ChannelNr].TempPeriod) SHL CMiniMODPositionShift) DIV Self.Frequency;
   END ELSE BEGIN
    Channel[ChannelNr].Active:=FALSE;
   END;
  END;
 END;
 SetTickVariables;
END;

FUNCTION TMiniMOD.Play:BOOLEAN;
VAR I:LONGWORD;
BEGIN
 FOR I:=1 TO HowManyChannels DO BEGIN
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
  Channel[I].PatternLoop:=FALSE;
  Channel[I].Surround:=FALSE;
  Channel[I].Filter:=FALSE;
  Channel[I].Glissando:=FALSE;
 END;

 Speed:=6;
 Self.BPM:=125;
 Tick:=0;
 SetTickVariables;
 BPMSamplesZaehler:=0;

 PatternDelay:=0;
 FrameDelay:=0;

 CurrentPattenOrder:=0;
 CurrentRow:=0;

 ShouldStart:=TRUE;
 RESULT:=IsTrackActive;
 IF RESULT THEN
  Self.Start();
END;

PROCEDURE TMiniMOD.Stop;
BEGIN
 Self.Stop();
END;

FUNCTION DoMixSample(CONST Data:PMiniMODSampleData;CONST RemainSamples:LONGWORD;CONST VolumeLeft,VolumeRight:LONGINT;VAR Buffer:PLONGINT;CONST StartPosition:LONGWORD;CONST IncrementValue:LONGWORD):LONGWORD; REGISTER;
VAR Remain,RemainCounter,Position,Increment:LONGWORD;
    Value,LeftVolume,RightVolume:LONGINT;
    SampleData:PMiniMODSampleData;
    Buf:PLONGINT;
BEGIN
 // Copy parameter to local variables -> better code optimization
 SampleData:=Data;
 Remain:=RemainSamples;
 LeftVolume:=VolumeLeft;
 RightVolume:=VolumeRight;
 Buf:=Buffer;
 Position:=StartPosition;
 Increment:=IncrementValue;
{$IFDEF UNROLLED}
 // Eliminate conditionals within loops
 CASE Remain AND 7 OF
  1:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain);
  END;
  2:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,2);
  END;
  3:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,3);
  END;
  4:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,4);
  END;
  5:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,5);
  END;
  6:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,6);
  END;
  7:BEGIN
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   Value:=SampleData[Position SHR CMiniMODPositionShift];
   INC(Buf^,Value*LeftVolume); INC(Buf);
   INC(Buf^,Value*RightVolume); INC(Buf);
   INC(Position,Increment);
   DEC(Remain,7);
  END;
 END;
 // Unroll loops with eliminate most all conditionals within these unrolled
 // loops
 Remain:=Remain SHR 3;
 FOR RemainCounter:=1 TO Remain DO BEGIN
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
 END;
{$ELSE}
 FOR RemainCounter:=1 TO Remain DO BEGIN
  Value:=SampleData[Position SHR CMiniMODPositionShift];
  INC(Buf^,Value*LeftVolume); INC(Buf);
  INC(Buf^,Value*RightVolume); INC(Buf);
  INC(Position,Increment);
 END;
{$ENDIF}
 Buffer:=Buf;
 RESULT:=Position;
END;

PROCEDURE TMiniMOD.MixChannel(Channel:PMiniMODChannel;StartPosition,LengthCounter:LONGWORD);
VAR Counter,Panning,SmpPos,SmpPosExt,SmpLen,SmpLoopStart,SmpLoopEnd,SmpLenExt,
    SmpLoopStartExt,SmpLoopEndExt,SmpInc,SmpEnd,Remain:LONGWORD;
    SmpLoop:BOOLEAN;
    VolumeLeft,VolumeRight:LONGINT;
    Buf:PLONGINT;
BEGIN
 IF Channel^.Increment=0 THEN BEGIN
  Channel^.Active:=FALSE;
 END ELSE IF Channel^.Active THEN BEGIN
  IF Channel^.Surround THEN BEGIN
   VolumeLeft:=64*Channel^.Volume;
   VolumeRight:=-64*Channel^.Volume;
  END ELSE BEGIN
   Panning:=(Channel^.Panning SHL 6) SHR 8;
   IF Panning>64 THEN Panning:=64;
   VolumeLeft:=(64-Panning)*Channel^.Volume;
   VolumeRight:=(Panning)*Channel^.Volume;
  END;
  VolumeLeft:=VolumeLeft DIV HowManyChannels;
  VolumeRight:=VolumeRight DIV HowManyChannels;

  SmpPos:=Channel^.Sample.Position;
  SmpPosExt:=Channel^.Sample.PositionExt;
  SmpLen:=Channel^.Sample.LengthCounter;
  SmpLoopStart:=Channel^.Sample.LoopStart;
  SmpLoopEnd:=Channel^.Sample.LoopEnd;
  SmpLoop:=Channel^.Sample.Loop;
  SmpInc:=Channel^.Increment;
  SmpLenExt:=SmpLen SHL CMiniMODPositionShift;
  SmpLoopStartExt:=SmpLoopStart SHL CMiniMODPositionShift;
  SmpLoopEndExt:=SmpLoopEnd SHL CMiniMODPositionShift;

  IF SmpLoop THEN BEGIN
   SmpEnd:=SmpLoopEndExt;
  END ELSE BEGIN
   SmpEnd:=SmpLenExt;
  END;

  Buf:= @_Buffer[StartPosition*2];
  Counter:=LengthCounter;
  WHILE Counter>0 DO BEGIN
   IF SmpLoop THEN BEGIN
    IF SmpPosExt>=SmpLoopEndExt THEN BEGIN
     SmpPosExt:=SmpPosExt-(SmpLoopEndExt-SmpLoopStartExt);
    END;
   END;
   IF SmpPosExt>=SmpLenExt THEN BEGIN
    Channel^.Active:=FALSE;
    BREAK;
   END;
   Remain:=1+((SmpEnd-SmpPosExt) DIV SmpInc);
   IF Remain=0 THEN BREAK;
   IF Remain>Counter THEN Remain:=Counter;
   SmpPosExt:=DoMixSample(Channel^.Sample.Data,Remain,VolumeLeft,VolumeRight,Buf,SmpPosExt,SmpInc);
   DEC(Counter,Remain);
  END;

  Channel^.Sample.Position:=SmpPos;
  Channel^.Sample.PositionExt:=SmpPosExt;
 END;
END;

FUNCTION TMiniMOD.DoMix(StartPosition,LengthCounter:LONGWORD;VAR DoContinue:LONGBOOL):LONGWORD;
VAR TheLength,ChannelCounter:LONGWORD;
BEGIN
 IF (StartPosition+LengthCounter)<=CMiniMODBufferSize THEN BEGIN
  TheLength:=LengthCounter;
 END ELSE IF StartPosition<=CMiniMODBufferSize THEN BEGIN
  TheLength:=CMiniMODBufferSize-StartPosition;
 END ELSE BEGIN
  TheLength:=0;
 END;
 IF TheLength>0 THEN BEGIN
  DEC(BPMSamplesZaehler,TheLength);
  ChannelCounter:=HowManyChannels;
  WHILE ChannelCounter>0 DO BEGIN
   MixChannel(@Channel[ChannelCounter],StartPosition,TheLength);
   DEC(ChannelCounter);
  END;
 END ELSE BEGIN
  DoContinue:=FALSE;
 END;
 RESULT:=TheLength;
END;

PROCEDURE TMiniMOD.Render(DestBuffer:PAudioSample);
VAR
  Counter:LONGWORD;
  DoContinue:LONGBOOL;
  DestPointer:PAudioSample;
  Value:LONGINT;
  Buf:PLONGINT;
BEGIN
 FILLCHAR(_Buffer[0], _OutputBufferSize * 2 * SizeOf(Cardinal), 0);

 Counter:=0;
 DoContinue:=TRUE;
 WHILE Counter<CMiniMODBufferSize DO
 BEGIN
  IF BPMSamplesZaehler=0 THEN
    DoTick;
  INC(Counter,DoMix(Counter,BPMSamplesZaehler,DoContinue));
 END;

 IF Filter THEN BEGIN
  // Lowpass Filter
  Buf:= @_Buffer[0];
  Counter:=0;
  WHILE Counter<CMiniMODBufferSize DO BEGIN
   Value:=Buf^ DIV 2;
   Buf^:=Value+NoiseReductionLeft;
   NoiseReductionLeft:=Value;
   INC(Buf);
   Value:=Buf^ DIV 2;
   Buf^:=Value+NoiseReductionRight;
   NoiseReductionRight:=Value;
   INC(Buf);
   INC(Counter);
  END;
 END;

 DestPointer:=DestBuffer;
 Buf:=@_Buffer[0];

   FOR Counter:=1 TO CMiniMODBufferSize DO
   BEGIN
    DestPointer^ := LONGWORD(Buf^ SHR (16-CMiniMODMixerShift));
    INC(DestPointer);
    INC(Buf);
    DestPointer^ := LONGWORD(Buf^ SHR (16-CMiniMODMixerShift));
    INC(DestPointer);
    INC(Buf);
  END;
END;


END.


