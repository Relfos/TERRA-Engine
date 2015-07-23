Unit CoreAudio;

{$I terra.inc}

Interface

{$LINKFRAMEWORK AudioToolbox}
{$LINKFRAMEWORK AudioUnit}

Const
   libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
   libAudioUnit = '/System/Library/Frameworks/AudioUnit.framework/AudioUnit';

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

Implementation

End.
