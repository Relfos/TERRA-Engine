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
 *}

Unit TERRA_MIDI;

{.$DEFINE USE_INTERNAL_SYNTH}

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_OS, TERRA_Application, TERRA_MusicTrack;

Type
  ChunkType = (MIDI_illegal, MIDI_header, MIDI_track);
  MidiFileFormat = (MIDI_single, MIDI_multi_synch, MIDI_multi_asynch);


Const
  MidiMessage_NoteOff = $80;
  MidiMessage_NoteOn  = $90;
  MidiMessage_NoteAftertouch = $A0;
  MidiMessage_ControlChange = $B0;
  MidiMessage_ProgramChange = $C0;
  MidiMessage_ChannelAftertouch = $D0;
  MidiMessage_PitchBend = $E0;
  MidiMessage_MetaEvent = $FF;
  MidiMessage_BankMSBControl  = $0;
  MidiMessage_BankLSBControl  = $32;

  MidiControl_Volume = $7;
  MidiControl_Pan    = $A;

  MaxMIDIChannels = 15;

Type
  PMidiEvent = ^ MidiEvent;
  MidiEvent = Record
    _Type: Byte;
    _Opcode:Byte;
    _Channel:Byte;
    _Data1: byte;
    _Data2: byte;
    _String:TERRAString;
    _dticks: integer;
    _Time: integer;
    _ms:Cardinal;
  End;

  MidiTrack = Class;

  MidiEventTrack = Class(TERRAObject)
  Protected
    _Owner:MidiTrack;
    _ID:Integer;
    _Events: Array Of MidiEvent;
    _EventCount:Integer;
    _LastTempoEventIndex:Integer;
    _Name:TERRAString;
    _Instrument:TERRAString;
    _CurrentTime:Integer;
    _CurrentPos:Integer;

    Function GetTrackDuration:Cardinal;

  Public
    Constructor Create;

    Procedure Rewind(time: integer);
    Procedure Update(Const LastTime, CurrentTime:Cardinal);
    Procedure PlayUntil(Const CurrentTime:Cardinal);

    Procedure putEvent(event: MidiEvent);
    Function getEvent(index: integer): PMidiEvent;

    Property Name:TERRAString Read _Name;
    Property Instrument:TERRAString Read _Name;
    Property EventCount:Integer Read _EventCount;
    Property Duration:Cardinal Read GetTrackDuration;
  End;

  MIDIChannel = Record
    Instrument:Byte;
    Volume:Byte;
    Panning:Byte;
  End;

  MidiTrack = Class(MusicTrack)
  Protected
    _Time:Cardinal;

    // Protected declarations
    _chunkType:ChunkType;
    _chunkLength:Integer;
    _chunkData:Array Of Byte;
    _chunkIndex:Integer;
    _chunkEnd:Integer;

    // midi file attributes
    FileFormat: MidiFileFormat;
    deltaTicks: integer;
    _Bpm: integer;
    _BeatsPerMeasure: integer;
    _usPerTick: double;

    _EventTracks:Array Of MidiEventTrack;
    _EventTrackCount:Integer;
    _CurrentChannel:MidiEventTrack;
    _LastTrackIndex:Integer;

    // playing attributes
    _Playing: boolean;
    _StartTime: Cardinal;
    _LastTime:Cardinal;
    _CurrentTime: Cardinal; // Current playtime in msec
    _CurrentPos: Double; // Current Position in ticks

    Procedure ProcessMidiEvent(Event:PMidiEvent; Channel:Integer);

    procedure ReadChunkHeader(Source:Stream);
    procedure ReadChunkContent(Source:Stream);
    procedure ReadChunk(Source:Stream);
    procedure ProcessHeaderChunk;
    procedure ProcessTrackChunk;
    function ReadVarLength: integer;
    function ReadString(l: integer):TERRAString;

    Procedure Clear();
    Procedure Reset();

    Procedure ChangeVolume(Volume:Single); Override;

  Public
    Procedure Init(); Override;
    Procedure Play(); Override;
    Procedure Update(); Override;
    Procedure Stop; Override;

    Procedure Release; override;

    Function GetTrack(index: integer): MidiEventTrack;
    Function AddTrack:MidiEventTrack;

    Function GetCurrentTime:Integer;
    Function  GetTrackDuration:Cardinal;

    Class Function Supports(Const Extension:TERRAString):Boolean; Override;

    Property TrackCount: integer read _EventTrackCount;
    Property TicksPerQuarter: integer read deltaTicks;
  End;

Type
  MidiPatchInfo=Record
    ID:Byte;
    Name:TERRAString;
  End;

Const
  PatchCount=128;

  // general MIDI instrument list
  instrumentAcousticGrand = 1;
  instrumentBrightAcoustic = 2;
  instrumentElectricGrand = 3;
  instrumentHonkyTonk = 4;
  instrumentElectricPiano = 5;
  instrumentElectricPianoB = 6;
  instrumentHarpsichord = 7;
  instrumentClav = 8;
  instrumentCelesta = 9;
  instrumentGlockenspiel = 10;
  instrumentMusicBox = 11;
  instrumentVibraphone = 12;
  instrumentMarimba = 13;
  instrumentXylophone = 14;
  instrumentTubularBells = 15;
  instrumentDulcimer = 16;
  instrumentDrawbarOrgan = 17;
  instrumentPercussiveOrgan = 18;
  instrumentRockOrgan = 19;
  instrumentChurchOrgan = 20;
  instrumentReedOrgan = 21;
  instrumentAccordian = 22;
  instrumentHarmonica = 23;
  instrumentTangoAccordian = 24;
  instrumentAcousticGuitarNylon = 25;
  instrumentAcousticGuitarSteel = 26;
  instrumentElectricGuitarJazz =  27;
  instrumentElectricGuitarClean = 28;
  instrumentElectricGuitarMuted = 29;
  instrumentOverdrivenGuitar = 30;
  instrumentDistortionGuitar = 31;
  instrumentGuitarHarmonics = 32;
  instrumentAcousticBass = 33;
  instrumentElectricBassFinger = 34;
  instrumentElectricBassPick = 35;
  instrumentFretlessBass = 36;
  instrumentSlapBass = 37;
  instrumentSlapBassB = 38;
  instrumentSynthBass = 39;
  instrumentSynthBassB = 40;
  instrumentViolin = 41;
  instrumentViola =  42;
  instrumentCello =  43;
  instrumentContrabass = 44;
  instrumentTremoloStrings = 45;
  instrumentPizzicatoStrings = 46;
  instrumentOrchestralStrings = 47;
  instrumentTimpani = 48;
  instrumentStringEnsemble = 49;
  instrumentStringEnsembleB = 50;
  instrumentSynthStrings = 51;
  instrumentSynthStringsB = 52;
  instrumentChoirAahs = 53;
  instrumentChoirOohs = 54;
  instrumentSynthVoice = 55;
  instrumentOrchestraHit = 56;
  instrumentTrumpet = 57;
  instrumentTrombone = 58;
  instrumentTuba = 59;
  instrumentMutedTrumpet = 60;
  instrumentFrenchHorn = 61;
  instrumentBrassSection = 62;
  instrumentSynthBrass = 63;
  instrumentSynthBrassB = 64;
  instrumentSopranoSax = 65;
  instrumentAltoSax = 66;
  instrumentTenorSax = 67;
  instrumentBaritoneSax = 68;
  instrumentOboe = 69;
  instrumentEnglishHorn = 70;
  instrumentBassoon = 71;
  instrumentClarinet = 72;
  instrumentPiccolo = 73;
  instrumentFlute = 74;
  instrumentRecorder = 75;
  instrumentPanFlute = 76;
  instrumentBlownBottle = 77;
  instrumentShakuhachi = 78;
  instrumentWhistle = 79;
  instrumentOcarina = 80;
  instrumentLeadSquare = 81;
  instrumentLeadSawtooth = 82;
  instrumentLeadCalliope = 83;
  instrumentLeadChiff = 84;
  instrumentLeadCharang = 85;
  instrumentLeadVoice = 86;
  instrumentLeadFifths = 88;
  instrumentLeadBassLead = 89;
  instrumentPadNewAge = 90;
  instrumentPadWarm = 91;
  instrumentPadPolysynth = 92;
  instrumentPadChoir = 93;
  instrumentPadBowed = 94;
  instrumentPadMetallic = 95;
  instrumentPadHalo = 96;
  instrumentPadSweep = 97;
  instrumentFXRain = 98;
  instrumentFXSoundtrack = 99;
  instrumentFXCrystal = 100;
  instrumentFXAtmosphere = 101;
  instrumentFXBrightness = 102;
  instrumentFXGoblins = 103;
  instrumentFXEchoes = 104;
  instrumentFXSciFi = 105;
  instrumentSitar = 106;
  instrumentBanjo = 107;
  instrumentShamisen = 108;
  instrumentKoto = 109;
  instrumentKalimba = 110;
  instrumentBagpipe = 111;
  instrumentFiddle =  112;
  instrumentShanai =  113;
  instrumentTinkleBell = 114;
  instrumentAgogo = 115;
  instrumentSteelDrums = 116;
  instrumentWoodblock = 117;
  instrumentTaikoDrum = 118;
  instrumentMelodicTom = 119;
  instrumentSynthDrum = 120;
  instrumentReverseCymbal = 121;
  instrumentGuitarFretNoise = 122;
  instrumentBreathNoise = 123;
  instrumentSeashore = 124;
  instrumentBirdTweet = 125;
  instrumentTelephoneRing = 126;
  instrumentHelicopter = 127;
  instrumentApplause = 128;
  instrumentGunshot = 129;

  PatchInfo:Array[1..PatchCount] Of MidiPatchInfo=
  (
  (ID:1; Name:'Acoustic Grand'),
  (ID:2; Name:'Bright Acoustic'),
  (ID:3; Name:'Electric Grand'),
  (ID:4; Name:'Honky-Tonk'),
  (ID:5; Name:'Electric Piano 1'),
  (ID:6; Name:'Electric Piano 2'),
  (ID:7; Name:'Harpsichord'),
  (ID:8; Name:'Clav'),
  (ID:9; Name:'Celesta'),
  (ID:10; Name:'Glockenspiel'),
  (ID:11; Name:'Music Box'),
  (ID:12; Name:'Vibraphone'),
  (ID:13; Name:'Marimba'),
  (ID:14; Name:'Xylophone'),
  (ID:15; Name:'Tubular Bells'),
  (ID:16; Name:'Dulcimer'),
  (ID:17; Name:'Drawbar Organ'),
  (ID:18; Name:'Percussive Organ'),
  (ID:19; Name:'Rock Organ'),
  (ID:20; Name:'Church Organ'),
  (ID:21; Name:'Reed Organ'),
  (ID:22; Name:'Accoridan'),
  (ID:23; Name:'Harmonica'),
  (ID:24; Name:'Tango Accordian'),
  (ID:25; Name:'Acoustic Guitar(nylon)'),
  (ID:26; Name:'Acoustic Guitar(steel)'),
  (ID:27; Name:'Electric Guitar(jazz)'),
  (ID:28; Name:'Electric Guitar(clean)'),
  (ID:29; Name:'Electric Guitar(muted)'),
  (ID:30; Name:'Overdriven Guitar'),
  (ID:31; Name:'Distortion Guitar'),
  (ID:32; Name:'Guitar Harmonics'),
  (ID:33; Name:'Acoustic Bass'),
  (ID:34; Name:'Electric Bass(finger)'),
  (ID:35; Name:'Electric Bass(pick)'),
  (ID:36; Name:'Fretless Bass'),
  (ID:37; Name:'Slap Bass 1'),
  (ID:38; Name:'Slap Bass 2'),
  (ID:39; Name:'Synth Bass 1'),
  (ID:40; Name:'Synth Bass 2'),
  (ID:41; Name:'Violin'),
  (ID:42; Name:'Viola'),
  (ID:43; Name:'Cello'),
  (ID:44; Name:'Contrabass'),
  (ID:45; Name:'Tremolo Strings'),
  (ID:46; Name:'Pizzicato Strings'),
  (ID:47; Name:'Orchestral Strings'),
  (ID:48; Name:'Timpani'),
  (ID:49; Name:'String Ensemble 1'),
  (ID:50; Name:'String Ensemble 2'),
  (ID:51; Name:'SynthStrings 1'),
  (ID:52; Name:'SynthStrings 2'),
  (ID:53; Name:'Choir Aahs'),
  (ID:54; Name:'Voice Oohs'),
  (ID:55; Name:'Synth Voice'),
  (ID:56; Name:'Orchestra Hit'),
  (ID:57; Name:'Trumpet'),
  (ID:58; Name:'Trombone'),
  (ID:59; Name:'Tuba'),
  (ID:60; Name:'Muted Trumpet'),
  (ID:61; Name:'French Horn'),
  (ID:62; Name:'Brass Section'),
  (ID:63; Name:'SynthBrass 1'),
  (ID:64; Name:'SynthBrass 2'),
  (ID:65; Name:'Soprano Sax'),
  (ID:66; Name:'Alto Sax'),
  (ID:67; Name:'Tenor Sax'),
  (ID:68; Name:'Baritone Sax'),
  (ID:69; Name:'Oboe'),
  (ID:70; Name:'English Horn'),
  (ID:71; Name:'Bassoon'),
  (ID:72; Name:'Clarinet'),
  (ID:73; Name:'Piccolo'),
  (ID:74; Name:'Flute'),
  (ID:75; Name:'Recorder'),
  (ID:76; Name:'Pan Flute'),
  (ID:77; Name:'Blown Bottle'),
  (ID:78; Name:'Shakuhachi'),
  (ID:79; Name:'Whistle'),
  (ID:80; Name:'Ocarina'),
  (ID:81; Name:'Lead 1 (square)'),
  (ID:82; Name:'Lead 2 (sawtooth)'),
  (ID:83; Name:'Lead 3 (calliope)'),
  (ID:84; Name:'Lead 4 (chiff)'),
  (ID:85; Name:'Lead 5 (charang)'),
  (ID:86; Name:'Lead 6 (voice)'),
  (ID:87; Name:'Lead 7 (fifths)'),
  (ID:88; Name:'Lead 8 (bass+lead)'),
  (ID:89; Name:'Pad 1 (new age)'),
  (ID:90; Name:'Pad 2 (warm)'),
  (ID:91; Name:'Pad 3 (polysynth)'),
  (ID:92; Name:'Pad 4 (choir)'),
  (ID:93; Name:'Pad 5 (bowed)'),
  (ID:94; Name:'Pad 6 (metallic)'),
  (ID:95; Name:'Pad 7 (halo)'),
  (ID:96; Name:'Pad 8 (sweep)'),
  (ID:97; Name:'FX 1 (rain)'),
  (ID:98; Name:'FX 2 (soundtrack)'),
  (ID:99; Name:'FX 3 (crystal)'),
  (ID:100; Name:'FX 4 (atmosphere)'),
  (ID:101; Name:'FX 5 (brightness)'),
  (ID:102; Name:'FX 6 (goblins)'),
  (ID:103; Name:'FX 7 (echoes)'),
  (ID:104; Name:'FX 8 (sci-fi)'),
  (ID:105; Name:'Sitar'),
  (ID:106; Name:'Banjo'),
  (ID:107; Name:'Shamisen'),
  (ID:108; Name:'Koto'),
  (ID:109; Name:'Kalimba'),
  (ID:110; Name:'Bagpipe'),
  (ID:111; Name:'Fiddle'),
  (ID:112; Name:'Shanai'),
  (ID:113; Name:'Tinkle Bell'),
  (ID:114; Name:'Agogo'),
  (ID:115; Name:'Steel Drums'),
  (ID:116; Name:'Woodblock'),
  (ID:117; Name:'Taiko Drum'),
  (ID:118; Name:'Melodic Tom'),
  (ID:119; Name:'Synth Drum'),
  (ID:120; Name:'Reverse Cymbal'),
  (ID:121; Name:'Guitar Fret Noise'),
  (ID:122; Name:'Breath Noise'),
  (ID:123; Name:'Seashore'),
  (ID:124; Name:'Bird Tweet'),
  (ID:125; Name:'Telephone Ring'),
  (ID:126; Name:'Helicopter'),
  (ID:127; Name:'Applause'),
  (ID:128; Name:'Gunshot')
  );

  PercussionCount=47;
  PercussionInfo:Array[1..PercussionCount] Of MidiPatchInfo=
  (
  (ID:35; Name:'Acoustic Bass Drum'),
  (ID:36; Name:'Bass Drum 1'),
  (ID:37; Name:'Side Stick'),
  (ID:38; Name:'Acoustic Snare'),
  (ID:39; Name:'Hand Clap'),
  (ID:40; Name:'Electric Snare'),
  (ID:41; Name:'Low Floor Tom'),
  (ID:42; Name:'Closed Hi-Hat'),
  (ID:43; Name:'High Floor Tom'),
  (ID:44; Name:'Pedal Hi-Hat'),
  (ID:45; Name:'Low Tom'),
  (ID:46; Name:'Open Hi-Hat'),
  (ID:47; Name:'Low-Mid Tom'),
  (ID:48; Name:'Hi-Mid Tom'),
  (ID:49; Name:'Crash Cymbal 1'),
  (ID:50; Name:'High Tom'),
  (ID:51; Name:'Ride Cymbal 1'),
  (ID:52; Name:'Chinese Cymbal'),
  (ID:53; Name:'Ride Bell'),
  (ID:54; Name:'Tambourine'),
  (ID:55; Name:'Splash Cymbal'),
  (ID:56; Name:'Cowbell'),
  (ID:57; Name:'Crash Cymbal 2'),
  (ID:58; Name:'Vibraslap'),
  (ID:59; Name:'Ride Cymbal 2'),
  (ID:60; Name:'Hi Bongo'),
  (ID:61; Name:'Low Bongo'),
  (ID:62; Name:'Mute Hi Conga'),
  (ID:63; Name:'Open Hi Conga'),
  (ID:64; Name:'Low Conga'),
  (ID:65; Name:'High Timbale'),
  (ID:66; Name:'Low Timbale'),
  (ID:67; Name:'High Agogo'),
  (ID:68; Name:'Low Agogo'),
  (ID:69; Name:'Cabasa'),
  (ID:70; Name:'Maracas'),
  (ID:71; Name:'Short Whistle'),
  (ID:72; Name:'Long Whistle'),
  (ID:73; Name:'Short Guiro'),
  (ID:74; Name:'Long Guiro'),
  (ID:75; Name:'Claves'),
  (ID:76; Name:'Hi Wood Block'),
  (ID:77; Name:'Low Wood Block'),
  (ID:78; Name:'Mute Cuica'),
  (ID:79; Name:'Open Cuica'),
  (ID:80; Name:'Mute Triangle'),
  (ID:81; Name:'Open Triangle')
  );


// helper functions (those do not generate any midi event!)
Function MIDIEvent_NoteOn(Channel, Note, Volume:Byte):Cardinal;
Function MIDIEvent_NoteOff(Channel, Note, Volume:Byte):Cardinal;
Function MIDIEvent_SetInstrument(Channel, Instrument:Byte):Cardinal;
Function MIDIEvent_SetVolume(Channel, Volume:Byte):Cardinal;
Function MIDIEvent_SetPanning(Channel, Pan:Byte):Cardinal;

Type
  MidiNoteState = (noteWaiting, notePlaying, noteFinished);
  
  MidiNoteEvent = Class(TERRAObject)
    Protected
      _Channel:Byte;
      _Note:Byte;
      _Volume:Byte;

      _StartTime:Cardinal;
      _EndTime:Cardinal;
      _Duration:Cardinal;

      _State:MidiNoteState;
      _Managed:Boolean;

    Public
      Procedure Init(Channel, Note,Volume: Byte; Duration, Delay: Cardinal);
      Function Start():Boolean;
      Procedure Stop();
  End;

  MidiManager = Class(ApplicationComponent)
    Protected
      _Notes:Array Of MidiNoteEvent;
      _NoteCount:Integer;

      _Channels:Array[0..MaxMIDIChannels] Of MIDIChannel;

      Function StartNote(Channel, Note, Volume:Byte):Boolean;
      Function StopNote(Channel, Note, Volume:Byte):Boolean;

      Function AddNote(Channel, Note, Volume: Byte; Duration, Delay: Cardinal):Boolean;

      Procedure FlushNotes(Channel, Note:Byte);

    Public
      Procedure Update; Override;

      Class Function Instance:MidiManager;

      // play a midi note with a certain duration and volume
      Function PlayNote(Channel, Note:Byte; Duration:Cardinal; Volume:Single = 0.8; Delay:Cardinal = 0):Boolean;

      Function SetInstrument(Channel, Instrument:Byte):Boolean;
      Function SetVolume(Channel, Volume:Byte):Boolean;
      Function SetPanning(Channel:Byte; Pan:Single):Boolean;

      Procedure Clear;

      Procedure Release; Override;
  End;

Implementation
Uses TERRA_FileManager, TERRA_MIDI_IO, TERRA_Log;

Const
  MIDIVolumeBoost = 1.0;
  MaxMIDIVolume = 127;
  DefaultMIDIVolume = 127;
  DefaultMIDIPanning = 127;

Var
  _MidiManager_Instance:ApplicationObject;

Function MIDIEvent_SetVolume(Channel, Volume:Byte):Cardinal;
Begin
  If (Channel>MaxMIDIChannels) Then
    Channel := MaxMIDIChannels;

  If (Volume > MaxMIDIVolume) Then
    Volume  := MaxMIDIVolume;

  Result := (MidiMessage_ControlChange + Channel) + (MidiControl_Volume) Shl 8 + (Volume Shl 16);
End;

Function MIDIEvent_SetPanning(Channel, Pan:Byte):Cardinal;
Begin
  If (Channel>MaxMIDIChannels) Then
    Channel := MaxMIDIChannels;

  Result := (MidiMessage_ControlChange + Channel) + (MidiControl_Pan) Shl 8 + (Pan Shl 16);
End;

Function MIDIEvent_SetInstrument(Channel, Instrument:Byte):Cardinal;
Begin
  If (Channel>MaxMIDIChannels) Then
    Channel := MaxMIDIChannels;

  If (Instrument > PatchCount) Then
    Instrument  := PatchCount;

  Result := (MidiMessage_ProgramChange + Channel) + Instrument Shl 8;
End;

Function MIDIEvent_NoteOn(Channel, Note, Volume:Byte):Cardinal;
Begin
  If (Channel>MaxMIDIChannels) Then
    Channel := MaxMIDIChannels;

  If (Volume > MaxMIDIVolume) Then
    Volume  := MaxMIDIVolume;

  Result := (MidiMessage_NoteOn + Channel) + Note Shl 8 + Volume Shl 16;
End;

Function MIDIEvent_NoteOff(Channel, Note, Volume:Byte):Cardinal;
Begin
  If (Channel>MaxMIDIChannels) Then
    Channel := MaxMIDIChannels;

  If (Volume > MaxMIDIVolume) Then
    Volume  := MaxMIDIVolume;

  Result := (MidiMessage_NoteOff + Channel) + Note Shl 8 + Volume Shl 16;
End;

Constructor MidiEventTrack.Create;
Begin
  Inherited Create;

  _EventCount := 0;
  _CurrentTime := 0;
  _CurrentPos := 0;
  _LastTempoEventIndex := -1;
End;

Procedure MidiEventTrack.putEvent(Event: MidiEvent);
Var
  us_per_quarter: integer;
  lastTempoEvent:MidiEvent;
Begin
  _currentTime := _currentTime + event._dticks;
  Event._Time := _currentTime;

  If (_LastTempoEventIndex>=0) And (_LastTempoEventIndex<_EventCount) Then
  Begin
    lastTempoEvent := _Events[_LastTempoEventIndex];
    Event._ms := lastTempoEvent._Time + Round(((_currentTime - LastTempoEvent._Time) * _Owner._usPerTick)/1000.0);
  End Else
    Event._ms := Round((_currentTime * _Owner._usPerTick)/1000.0);

  Inc(_EventCount);
  SetLength(_Events, _EventCount);
  _Events[Pred(_EventCount)] := Event;

  If (event._Opcode = MidiMessage_MetaEvent) Then
  Begin
    If (event._data1 = 3) Then
      _name := event._string
    Else
    If (event._data1 = 4) Then
    Begin
      _instrument := event._string;
    End Else
    If (event._data1 = $51) Then
    Begin
      us_per_quarter :=
                  (integer(byte(event._string[1])) shl 16 +
                  integer(byte(event._string[2])) shl 8 +
                  integer(byte(event._string[3])));
      _Owner._Bpm := 60000000 div us_per_quarter;
      _Owner._usPerTick := us_per_quarter / _Owner.deltaTicks;
      _LastTempoEventIndex := Pred(_EventCount);
    End;
  End;
End;

Function MidiEventTrack.getEvent(index: integer): PMidiEvent;
Begin
  If ((index < _eventCount) and (index >= 0)) then
    Result := @(_events[index])
  Else
    Result := Nil;
End;

Procedure MidiEventTrack.Rewind;
Begin
  _CurrentPos := 0;
End;

Procedure MidiEventTrack.PlayUntil(Const CurrentTime:Cardinal);
Var
   T:Cardinal;
Begin
  _CurrentPos := 0;
  While (_currentPos < _EventCount) Do
  Begin
    T := _Events[_currentPos]._ms;
    If (T <=CurrentTime) Then
    Begin
      _Owner.ProcessMidiEvent(@(_Events[_currentPos]), Self._ID);
    End Else
        Break;
    Inc(_currentPos);
  End;
End;

Procedure MidiEventTrack.Update(Const LastTime, CurrentTime:Cardinal);
Var
   T:Cardinal;
Begin
  _CurrentPos := 0;
  While (_currentPos < _EventCount) Do
  Begin
    T := _Events[_currentPos]._ms;
    If (T > LastTime) And (T <=CurrentTime) Then
    Begin
      _Owner.ProcessMidiEvent(@(_events[_currentPos]), Self._ID);
    End Else
    If (T>CurrentTime) Then
       Break;

    Inc(_currentPos);
  End;
End;

Function MidiEventTrack.GetTrackDuration:Cardinal;
Begin
  Result := _events[Pred(_eventcount)]._ms;
End;

Procedure MidiTrack.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_EventTrackCount) do
    ReleaseObject(_EventTracks[I]);
    
  SetLength(_chunkData, 0);    
End;

Procedure MidiTrack.Release;
Begin
  Clear;
End;

function MidiTrack.GetTrack(index: integer): MidiEventTrack;
Begin
  result := _EventTracks[index];
end;

Function MidiTrack.AddTrack:MidiEventTrack;
Begin
  Inc(_EventTrackCount);
  SetLength(_EventTracks, _EventTrackCount);
  _EventTracks[Pred(_EventTrackCount)] := MidiEventTrack.Create;
  Result := _EventTracks[Pred(_EventTrackCount)];
  Result._Owner := Self;
  Result._ID := Pred(_EventTrackCount);
End;

Procedure MidiTrack.Init();
Var
   Src:Stream;
Begin
  Clear();
  _ChunkType := midi_Illegal;
  _usPerTick := 1042;
  _BPM := 120;

  Src := FileManager.Instance.OpenStream(_FileName);
  If Src = Nil Then
     Exit;

  While (Assigned(Src)) And (Not Src.EOF) Do
        ReadChunk(Src);

  ReleaseObject(Src);
End;


Procedure MidiTrack.Play();
Begin
  _Playing := (_EventTrackCount>0);

  If Not _Playing Then
    Exit;

  Self.Reset();
End;

Procedure MidiTrack.Reset();
Var
  I:Integer;
  Manager:MidiManager;
Begin
  Manager := MidiManager.Instance();

  For I:=0 To MaxMIDIChannels Do
  Begin
    Manager.SetInstrument(I, instrumentAcousticGrand);
    Manager.SetVolume(I, Trunc(DefaultMIDIVolume * (_Volume * MIDIVolumeBoost)));
    Manager.SetPanning(I, 0.0);
  End;

  For I:=0 To Pred(_EventTrackCount) do
    _EventTracks[i].Rewind(0);

  _StartTime := Application.GetTime();
  _LastTime := 0;
  _CurrentPos := 0.0;
  _CurrentTime := 0;
End;

Procedure MidiTrack.Stop;
Var
  I:Integer;
  Current, Length:Cardinal;
Begin
  If Not _Playing Then
    Exit;

  Current := _LastTime - _StartTime;
  Length := Self.GetTrackDuration();

  _Playing := false;
End;

Function MidiTrack.GetCurrentTime: integer;
Begin
  Result := _currentTime;
end;

Procedure MidiTrack.Update();
Var
  I:Integer;
  Before, Current, Length:Cardinal;
Begin
  If Not _Playing Then
     Exit;

  _CurrentTime := Application.GetTime();

  Current := _CurrentTime - _StartTime;

  If (_LastTime = 0) Then
  Begin
    For I := 0 to Pred(_EventTrackCount) Do
        _EventTracks[i].PlayUntil(Current);
  End Else
  Begin
    Before := _LastTime - _StartTime;
    Length := Self.GetTrackDuration;

    If (Current > Length) Then
    Begin
      Reset();
      {For I := 0 to Pred(_EventTrackCount) Do
          _EventTracks[i].Update(Before, Length);

      Dec(Current, Length);
      _StartTime := GetTime() - Current;
      _LastTime := GetTime();
       _CurrentTime := _LastTime + Current;

        For I := 0 to Pred(_EventTrackCount) Do
            _EventTracks[i].PlayUntil(Current);}
    End Else
    Begin
      For I := 0 to Pred(_EventTrackCount) Do
        _EventTracks[i].Update(Before, Current);
    End;
  End;

  _LastTime := _CurrentTime;
End;

procedure MidiTrack.ReadChunkHeader(Source:Stream);
var
  data: array[0..7] of byte;
Begin
  Source.Read(@data[0], 8);
  If (data[0] = $4D) and (data[1] = $54) then
  Begin
    If (data[2] = $68) and (data[3] = $64) then
      _chunkType := midi_Header
    Else
    If (data[2] = $72) and (data[3] = $6B) then
      _chunkType := midi_Track
    Else
      _chunkType := midi_Illegal;
  End Else
  Begin
    _chunkType := midi_Illegal;
  End;

  _chunkLength := data[7] + data[6] * $100 + data[5] * $10000 + data[4] * $1000000;
End;

procedure MidiTrack.ReadChunkContent(Source:Stream);
Begin
  SetLength(_chunkData, _chunkLength + 10);
  Source.Read(@(_chunkData[0]), _chunkLength);
  _chunkIndex := 0;
  _chunkEnd := Pred(_chunkLength);
end;

procedure MidiTrack.ReadChunk(Source:Stream);
Begin
  ReadChunkHeader(Source);
  ReadChunkContent(Source);
  case _chunkType of
    midi_header:
      ProcessHeaderChunk;

    midi_track:
      ProcessTrackCHunk;
  End;
End;

Procedure MidiTrack.ProcessHeaderChunk;
Begin
  Inc(_chunkIndex);

  If _chunkType = midi_header then
  Begin
    Case _ChunkData[_chunkIndex] of
      0: fileFormat := midi_single;
      1: fileFormat := midi_multi_synch;
      2: fileFormat := midi_multi_asynch;
    End;

    inc(_chunkIndex);
    _EventTrackCount := _ChunkData[_chunkIndex] * $100;

    inc(_chunkIndex);
    _EventTrackCount := _EventTrackCount + _ChunkData[_chunkIndex];

    SetLength(_EventTracks, _EventTrackCount);
    _LastTrackIndex := 0;

    Inc(_chunkIndex);
    deltaTicks := _ChunkData[_chunkIndex] * $100;

    Inc(_chunkIndex);
    deltaTicks := deltaTicks + _ChunkData[_chunkIndex];
  End;
End;

Procedure MidiTrack.ProcessTrackChunk;
Var
  dTime: integer;
  event: integer;
  len: integer;
  str:TERRAString;
  midiEvent: TERRA_Midi.MidiEvent;
  i: integer;
Begin
  //inc(_chunkIndex);
  event := 0;
  if _chunkType = midi_track then
  Begin
    _CurrentChannel := MidiEventTrack.Create;
    _CurrentChannel._Owner := Self;
    _CurrentChannel._ID := _LastTrackIndex;
    _EventTracks[_LastTrackIndex] := _CurrentChannel;
    Inc(_LastTrackIndex);
    While _chunkIndex < _chunkEnd Do
    Begin
      // each event starts with var length delta time
      dTime := ReadVarLength;
      If _ChunkData[_chunkIndex] >= $80 Then
      Begin
        event := _ChunkData[_chunkIndex];
        inc(_chunkIndex);
      End;
      // else it is a running status event (just the same event as before)

      If Event = $FF then
      Begin
        midiEvent._Opcode := MidiMessage_MetaEvent;
        midiEvent._data1 := _ChunkData[_chunkIndex]; // type is stored in data1
        midiEvent._dticks := dtime;

        inc(_chunkIndex);
        len := ReadVarLength;
        midiEvent._string := ReadString(len);
        _CurrentChannel.putEvent(midiEvent);
      End Else
      Begin
      // these are all midi events
        midiEvent._dticks := dtime;
        case event of
          $80..$8F: // note off
            Begin
              midiEvent._Opcode := MidiMessage_NoteOff;
              midiEvent._Channel := Event - MidiMessage_NoteOff;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
              midiEvent._data2 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $90..$9F: // note on
            Begin
              midiEvent._Opcode := MidiMessage_NoteOn;
              midiEvent._Channel := Event - MidiMessage_NoteOn;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
              midiEvent._data2 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $A0..$AF: // key aftertouch
            Begin
              midiEvent._Opcode := MidiMessage_NoteAftertouch;
              midiEvent._Channel := Event - MidiMessage_NoteAftertouch;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
              midiEvent._data2 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $B0..$BF: // control change
            Begin
              midiEvent._Opcode := MidiMessage_ControlChange;
              midiEvent._Channel := Event - MidiMessage_ControlChange;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
              midiEvent._data2 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $E0..$EF: // pitch wheel change
            Begin
              midiEvent._Opcode := MidiMessage_PitchBend;
              midiEvent._Channel := Event - MidiMessage_PitchBend;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
              midiEvent._data2 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $C0..$CF: // program change
            Begin
              midiEvent._Opcode := MidiMessage_ProgramChange;
              midiEvent._Channel := Event - MidiMessage_ProgramChange;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
          $D0..$DF: // channel aftertouch
            Begin
              midiEvent._Opcode := MidiMessage_ChannelAftertouch;
              midiEvent._Channel := Event - MidiMessage_ChannelAftertouch;
              midiEvent._data1 := _ChunkData[_chunkIndex]; inc(_chunkIndex);
            End;
        Else
          Log(logWarning, 'MIDI', 'Unknown midi event: '+IntToString(event));
        End;

        _CurrentChannel.putEvent(midiEvent);
      End;
    End;
  End;
End;


function MidiTrack.ReadVarLength: integer;
var
  i: integer;
  b: byte;
Begin
  b := 128;
  i := 0;
  While b > 127 do
  Begin
    i := i shl 7;
    b := _ChunkData[_chunkIndex];
    i := i + b and $7F;
    inc(_chunkIndex);
  End;
  result := i;
end;

Function MidiTrack.ReadString(l: integer):TERRAString;
Var
  I:Integer;
Begin
  SetLength(Result, L);
  For I := 1 to L Do
  Begin
    Result[i] := AnsiChar(_ChunkData[_chunkIndex]);
    Inc(_chunkIndex);
  End;
End;

(*Procedure MidiTrack.InitStream;
Var
  I:Integer;
Begin
  Self.AllocBuffer(2, 16, DefaultSampleRate);
  _Synth := Synthetizer.Create(20, DefaultSampleRate);

  Clear;
  ChunkType := Illegal;
  _usPerTick := 1042;
  _BPM := 120;

  While (Assigned(_Source)) And (Not _Source.EOF) Do
    ReadChunk(_Source);
End;*)

Function  MidiTrack.GetTrackDuration():Cardinal;
Var
  I, Length :Integer;
  Time:Double;
Begin
  Length := 0;
  For i := 0 to Pred(_EventTrackCount) do
  If _EventTracks[i].GetTrackDuration > length Then
    Length := _EventTracks[i].GetTrackDuration();

  Time := Length * _usPerTick;
  Time := Time / 1000.0;
  Result := Round(time);
End;

Procedure MidiTrack.ProcessMidiEvent(Event:PMidiEvent; Channel:Integer);
Var
  I:Integer;
  S, S2:TERRAString;
  Velocity:Integer;
  Manager:MidiManager;
Begin
  Manager := MidiManager.Instance();

  Case Event._Opcode of
    MidiMessage_ProgramChange:
      Begin
        {$IFDEF DEBUG_MIDI}
        Write('Program change:', S, ' on track ',track,' channel ',Event._Channel,'...');
        {$ENDIF}

        Manager.SetInstrument(Event._Channel, Event._Data1);
      End;

    MidiMessage_ControlChange:
      If (Event._Data1 = MidiControl_Volume) Then
      Begin
        {$IFDEF DEBUG_MIDI}
        WriteLn('Volume:', Event._Data2, ' on track ',track,' channel ',Event._Channel);
        {$ENDIF}

        Velocity := Trunc(Event._Data2 * (_Volume * MIDIVolumeBoost));

        If (Velocity>MaxMIDIVolume) Then
          Velocity := MaxMIDIVolume;

        Manager._Channels[Event._Channel].Volume := Event._Data2;

        MIDI_Out(MIDIEvent_SetVolume(Event._Channel, Velocity));
      End Else
      If (Event._Data1 = MidiControl_Pan) Then
      Begin
        {$IFDEF DEBUG_MIDI}
        WriteLn('Pan:', Event._Data2, ' on track ',track,' channel ',Event._Channel);
        {$ENDIF}

        Manager._Channels[Event._Channel].Panning := Event._Data2;

        MIDI_Out(MIDIEvent_SetPanning(Event._Channel, Event._Data2));
      End;

    MidiMessage_NoteOn: // note on
    Begin
        {$IFDEF DEBUG_MIDI}
        WriteLn('On ',GetNoteName(Event._Data1));
        {$ENDIF}

        Manager.StartNote(Event._Channel, Event._Data1, Event._Data2);
    End;


    MidiMessage_NoteOff:   // note off
      Begin
        {$IFDEF DEBUG_MIDI}
        WriteLn('Off ',GetNoteName(Event._Data1));
        {$ENDIF}

        Manager.StopNote(Event._Channel, Event._Data1, Event._Data2);
      End;

    Else  // raw events
        Begin
          MIDI_Out(Event._Channel + Event._Opcode + Event._Data1 Shl 8 + Event._Data2 Shl 16);
        End;
  End;
End;

Procedure MidiTrack.ChangeVolume(Volume: Single);
Var
  I, Velocity:Integer;
  Manager:MidiManager;
Begin
  Manager := MidiManager.Instance;
  For I:=0 To MaxMIDIChannels Do
  Begin
    Velocity := Trunc(Manager._Channels[I].Volume * (_Volume * MIDIVolumeBoost));

    If (Velocity>MaxMIDIVolume) Then
      Velocity := MaxMIDIVolume;

    MIDI_Out(MIDIEvent_SetVolume(I, Velocity));
  End;
End;

Class Function MidiTrack.Supports(Const Extension: TERRAString): Boolean;
Begin
  Result := (Extension = 'mid');
End;

(*
Procedure MidiTrack.Stream(Buffer:Cardinal);
Var
  Samples, Length, Delta:Cardinal;
Begin
  Length := Self.GetTrackDuration;
  Samples := streamBufferSize Div 4;
  Delta := Trunc(Samples/(DefaultSampleRate/1000));

  Self.PlayToTime(_Time + Delta);
  //Samples := Trunc(Delta * (DefaultSampleRate/1000));
  Self.Synth.Mix(PSmallInt(_Data), Samples, _Time);
  Inc(_Time, Delta);
  If (_Time>=Length) Then
    _Time := 0;

  Inherited;
End;
*)

{ MidiManager }
Procedure MidiManager.Release;
Begin
  Self.Clear();
  Inherited;
End;

Class Function MidiManager.Instance:MidiManager;
Begin
  If _MidiManager_Instance = Nil Then
    _MidiManager_Instance := InitializeApplicationComponent(MidiManager, Nil);

  Result := MidiManager(_MidiManager_Instance.Instance);
End;

Procedure MidiManager.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_NoteCount) Do
  Begin
    _Notes[I].Stop();
    ReleaseObject(_Notes[I]);
  End;
End;

Function MidiManager.AddNote(Channel, Note, Volume: Byte; Duration, Delay: Cardinal):Boolean;
Var
  N,I, Len:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_NoteCount) Do
  If (_Notes[I]._State = noteFinished) Or
  ((_Notes[I]._Channel = Channel) And (_Notes[I]._Note = Note) And (_Notes[I]._State <> noteWaiting)) Then
  Begin
    N := I;
    Break;
  End;

  If N<0 Then
  Begin
    N := _NoteCount;
    Inc(_NoteCount);

    Len := Length(_Notes);
    If Len<_NoteCount Then
    Begin
      SetLength(_Notes, _NoteCount);
      _Notes[Pred(_NoteCount)] := MidiNoteEvent.Create();
    End;
  End;

  _Notes[N].Init(Channel, Note, Volume, Duration, Delay);

  If Delay<=0 Then
  Begin
    Self.FlushNotes(Channel, Note);
    _Notes[N].Start();
  End;

  Result := True;
End;

Function MidiManager.StartNote(Channel, Note, Volume: Byte): Boolean;
Begin
  Result := Self.AddNote(Channel, Note, Volume, 0, 0);
End;

Function MidiManager.StopNote(Channel, Note, Volume: Byte): Boolean;
Var
  I:Integer;
  T:Cardinal;
Begin
  I := 0;
  While I<_NoteCount Do
  If (_Notes[I]._State <> noteFinished) And (_Notes[I]._Channel = Channel) And (_Notes[I]._Note = Note) Then
  Begin
    _Notes[I]._Volume := Volume;
    _Notes[I].Stop();
    Result := True;
    Exit;
  End Else
    Inc(I);
  Result := False;
End;

Function MidiManager.PlayNote(Channel, Note: Byte; Duration: Cardinal; Volume:Single; Delay:Cardinal):Boolean;
Var
  VolData:Byte;
Begin
  VolData := Trunc(Volume*MaxMIDIVolume);
  Result := Self.AddNote(Channel, Note, VolData, Duration, Delay);
End;

Procedure MidiManager.Update;
Var
  I:Integer;
  T:Cardinal;
Begin
  I := 0;
  T := Application.GetTime();
  While I<_NoteCount Do
  If (_Notes[I]._State = notePlaying) And (_Notes[I]._Managed) And (_Notes[I]._EndTime<T) Then
  Begin
    _Notes[I].Stop();
  End Else
  Begin
    If (_Notes[I]._State = noteWaiting) And (T >= _Notes[I]._StartTime) Then
    Begin
      Self.FlushNotes(_Notes[I]._Channel, _Notes[I]._Note);
      _Notes[I].Start();
    End;

    Inc(I);
  End;
End;

Function MidiManager.SetPanning(Channel:Byte; Pan:Single): Boolean;
Var
  PanData:Byte;
Begin
  PanData := Byte(Trunc(Pan * DefaultMIDIPanning) + DefaultMIDIPanning);
  Result := MIDI_Out(MIDIEvent_SetPanning(Channel, PanData));
  If Result Then
    _Channels[Channel].Panning := PanData;
End;

Function MidiManager.SetVolume(Channel, Volume: Byte): Boolean;
Begin
  Result := MIDI_Out(MIDIEvent_SetVolume(Channel, Volume));
  If Result Then
    _Channels[Channel].Volume := Volume;
End;

Function MidiManager.SetInstrument(Channel, Instrument: Byte): Boolean;
Begin
  Result := MIDI_Out(MIDIEvent_SetInstrument(Channel, Instrument));
  If Result Then
    _Channels[Channel].Instrument := Instrument;
End;

Procedure MidiManager.FlushNotes(Channel, Note: Byte);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_NoteCount) Do
  If (_Notes[I]._State = notePlaying) And (_Notes[I]._Channel = Channel) And (_Notes[I]._Note = Note) Then
  Begin
    _Notes[I]._State := noteFinished;
  End;
End;

{ MidiNoteEvent }
Procedure MidiNoteEvent.Init(Channel, Note, Volume: Byte; Duration, Delay: Cardinal);
Var
  T:Cardinal;
Begin
  _Channel := Channel;
  _Note := Note;
  _Volume := Volume;

  _State := noteWaiting;

  _Managed := (Duration>0);
  _Duration := Duration;

  If _Managed Then
  Begin
    T := Application.GetTime();
    _StartTime := T + Delay;
    _EndTime := _StartTime + _Duration;
  End;
End;

Function MidiNoteEvent.Start(): Boolean;
Begin
  Result := (MIDI_Out(MIDIEvent_NoteOn(_Channel, _Note, _Volume)));

//  WriteLn('Starting Note '+IntToString(_Note));

  _State := notePlaying;
End;

Procedure MidiNoteEvent.Stop;
Var
  MuteEvent:Cardinal;
Begin
  If _State <> notePlaying Then
    Exit;

  MuteEvent := MIDIEvent_NoteOff(_Channel, _Note, _Volume);
  MIDI_Out(MuteEvent);

  //WriteLn('Stopping Note '+IntToString(_Note));

  _State := noteFinished;
End;

End.

