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
 * TERRA_MIDI_IO
 * Implements MIDI input/output in OSX
 ***********************************************************************************************************************
}

Unit TERRA_MIDI_IO;

// https://developer.apple.com/library/mac/samplecode/PlaySoftMIDI/Listings/main_cpp.html

{$I terra.inc}

{s$DEFINE USE_COREMIDI}

Interface

Uses TERRA_Object, TERRA_Utils
  {$IFDEF USE_COREMIDI} ,MIDIServices;
  {$ELSE};
  {$LINKFRAMEWORK AudioToolbox}
  {$LINKFRAMEWORK AudioUnit}

Const
   libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
   libAudioUnit = '/System/Library/Frameworks/AudioUnit.framework/AudioUnit';

  {$ENDIF}


Function MIDI_Out(Event:Cardinal):Boolean;

Implementation
Uses MacOSAll, TERRA_Log, TERRA_MIDI;

Var
   _midiInitialized:Boolean = False;

{$IFDEF USE_COREMIDI}
Var
  _midiClient:MIDIClientRef = Nil;
  _midiOut:MIDIPortRef;

  _midiDest:MIDIEndpointRef;

Function MIDI_Init():Boolean;
Var
  status:OSStatus;
  I, DestCount, DeviceCount:Integer;
Begin
     If (_midiClient<>Nil) Then
     Begin
          Result := True;
          Exit;
     End;

     Status := MIDIClientCreate(CFSTR('MIDI client'), Nil, Nil, @_midiClient);
     If (Status != noErr) Then
     Begin
          Log(logError, 'MIDI', 'Error creating MIDI client ' + IntToString(status));
          Result := False;
     End;

     Status := MIDIOutputPortCreate(_midiClient, CFSTR('MIDI Output'), _midiOut);
     If (Status != noErr) Then
     Begin
          Log(logError, 'MIDI', 'Error creating MIDI output port ' + IntToString(status));
          Result := False;
     End;

     DestCount := MIDIGetNumberOfDestinations();

     For I:=0 To Pred(DestCount) Do
     Begin
        src = MIDIGetSource(i);
       MIDIPortConnectSource(inputPort, src, NULL);
     End;
End;

Procedure MIDI_Out(MidiEvent:Cardinal);
Var
  Status:OSStatus;
Begin
     If (_midiClient = Nil) Then
     Begin
          If (Not MIDI_Init()) Then
          Begin
               Result := False;
               Exit;
          End;
     End;

     Status := MIDISend(_midiOut, _midiDest, packet);
End;
{$ELSE}

Type
 AUGraph = Pointer;
 AUNode = Integer;

const
 kAUGraphErr_NodeNotFound = -10860;
 kAUGraphErr_InvalidConnection = -10861;
 kAUGraphErr_OutputNodeErr = -10862;
 kAUGraphErr_CannotDoInCurrentContext = -10863;
 kAUGraphErr_InvalidAudioUnit = -10864;

Type
    AudioComponentDescriptionRef = ^AudioComponentDescription;

Function NewAUGraph(out  outGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name  'NewAUGraph';

Function AUGraphAddNode(inGraph: AUGraph;
		inDescription: AudioComponentDescriptionRef;
		out  outNode: AUNode):OSStatus; cdecl; external libAudioToolbox name 'AUGraphAddNode';
Function AUGraphRemoveNode(inGraph: AUGraph;
		inNode: AUNode):OSStatus; cdecl; external libAudioToolbox name 'AUGraphRemoveNode';
Function AUGraphGetNodeCount(inGraph: AUGraph;
		out  outNumberOfNodes: Cardinal):OSStatus; cdecl; external libAudioToolbox name 'AUGraphGetNodeCount';

Function AUGraphOpen(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphOpen';
Function AUGraphClose(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphClose';
Function AUGraphInitialize(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphInitialize';
Function AUGraphUninitialize(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphUninitialize';
Function AUGraphStart(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphStart';
Function AUGraphStop(inGraph: AUGraph):OSStatus; cdecl; external libAudioToolbox name 'AUGraphStop';

Function AUGraphNodeInfo(inGraph: AUGraph;
		inNode: AUNode;
		out  outDescription: AudioComponentDescription;
		out  outAudioUnit: AudioUnit):OSStatus; cdecl; external libAudioToolbox name  'AUGraphNodeInfo';

Function AUGraphConnectNodeInput(inGraph: AUGraph;
		inSourceNode: AUNode;
		inSourceOutputNumber: UInt32;
		inDestNode: AUNode;
		inDestInputNumber: UInt32):OSStatus; cdecl; external libAudioToolbox name 'AUGraphConnectNodeInput';

Function MusicDeviceMIDIEvent(inUnit: MusicDeviceComponent;
		inStatus: UInt32;
		inData1: UInt32;
		inData2: UInt32;
		inOffsetSampleFrame: UInt32):OSStatus; cdecl; external libAudioUnit name  'MusicDeviceMIDIEvent';

Var
   _outGraph:AUGraph;
   _outSynth:AudioUnit;

Function MIDI_Init():Boolean;
Var
  status:OSStatus;
  synthNode, limiterNode, outNode:AUNode;
  cd, outdesc:AudioComponentDescription;
Begin
     //create the nodes of the graph
     cd.componentManufacturer := kAudioUnitManufacturer_Apple;
     cd.componentFlags := 0;
     cd.componentFlagsMask := 0;

     Status := NewAUGraph(_outGraph);
     If (Status <> noErr) Then
     Begin
          Log(logError, 'MIDI', 'Error creating audio graph: ' + IntToString(status));
          Result := False;
          Exit;
     End;

     cd.componentType := kAudioUnitType_MusicDevice;
     cd.componentSubType := kAudioUnitSubType_DLSSynth;

     Status := AUGraphAddNode(_outGraph, @cd, synthNode);
     If (Status <> noErr) Then
     Begin
          Log(logError, 'MIDI', 'Error adding DLS node to audio graph: ' + IntToString(status));
          Result := False;
          Exit;
     End;

     cd.componentType := kAudioUnitType_Effect;
     cd.componentSubType := kAudioUnitSubType_PeakLimiter;

     Status := AUGraphAddNode(_outGraph, @cd, limiterNode);
     If (Status <> noErr) Then
     Begin
          Log(logError, 'MIDI', 'Error adding effect node to audio graph: ' + IntToString(status));
          Result := False;
          Exit;
     End;

    cd.componentType := kAudioUnitType_Output;
    cd.componentSubType := kAudioUnitSubType_DefaultOutput;
    Status := AUGraphAddNode(_outGraph, @cd, &outNode);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error adding output node to audio graph: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    Status := AUGraphOpen(_outGraph);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error opening audio graph: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    Status := AUGraphConnectNodeInput(_outGraph, synthNode, 0, limiterNode, 0);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error conneting synth graph node for input: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    Status := AUGraphConnectNodeInput(_outGraph, limiterNode, 0, outNode, 0);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error connecting effect graph node : ' + IntToString(status));
         Result := False;
         Exit;
    End;

    // ok we're good to go - get the Synth Unit...
    Status := AUGraphNodeInfo(_outGraph, synthNode, outDesc, _outSynth);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error getting graph node info: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    Status := AUGraphInitialize(_outGraph);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error initializting graph: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    //set our bank
  (*  Status := MusicDeviceMIDIEvent(_outSynth,
                                    MidiMessage_ControlChange,
                                    MidiMessage_BankMSBControl, 0,
                                    0); // sample offset

    Status := MusicDeviceMIDIEvent(_outSynth,
                                    MidiMessage_ProgramChange,
                                    0, 0,
                                    0); // sample offset
*)
    Status := AUGraphStart(_outGraph);
    If (Status <> noErr) Then
    Begin
         Log(logError, 'MIDI', 'Error starting graph: ' + IntToString(status));
         Result := False;
         Exit;
    End;

    Result := True;

    _midiInitialized := True;
End;

Function MIDI_Out(Event:Cardinal):Boolean;
Var
  Status:OSStatus;
  Opcode, Arg1, Arg2:Byte;
Begin
     If (Not _midiInitialized) Then
     Begin
          If (MIDI_Init()) Then
          Begin
               Result := False;
               Exit;
          End;
     End;

     Opcode := Event And $FF;
     Arg1 := (Event Shr  8) And $FF;
     Arg2 := (Event Shr  16) And $FF;

     Status := MusicDeviceMIDIEvent(_outSynth, Opcode, Arg1, Arg2, 0);

     Result := (Status = noErr);
End;

{$ENDIF}

End.

