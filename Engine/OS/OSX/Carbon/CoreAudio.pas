Unit CoreAudio;

{$I terra.inc}

Interface
Uses MacOSAll;

{$LINKFRAMEWORK AudioToolbox}
{$LINKFRAMEWORK AudioUnit}

Const
   libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
   libAudioUnit = '/System/Library/Frameworks/AudioUnit.framework/AudioUnit';

Type
 AUGraph = Pointer;
 AUNode = Integer;

 PAudioBufferList = ^AudioBufferList;
 PAudioSampleType = ^AudioSampleType;

 AudioStreamBasicDescription = Object
   mSampleRate:Double;
   mFormatID:Cardinal;
   mFormatFlags:Cardinal;
   mBytesPerPacket:Cardinal;
   mFramesPerPacket:Cardinal;
   mBytesPerFrame:Cardinal;
   mChannelsPerFrame:Cardinal;
   mBitsPerChannel:Cardinal;
   mReserved:Cardinal;


   Procedure SetAUCanonical(nChannels:Cardinal; interleaved:Boolean);
 End;

const
 kAUGraphErr_NodeNotFound = -10860;
 kAUGraphErr_InvalidConnection = -10861;
 kAUGraphErr_OutputNodeErr = -10862;
 kAUGraphErr_CannotDoInCurrentContext = -10863;
 kAUGraphErr_InvalidAudioUnit = -10864;

Type
    AudioComponentDescriptionRef = ^AudioComponentDescription;

    RenderCallbackShared = Function (inRefCon:Pointer; Var ioActionFlags:AudioUnitRenderActionFlags; Var inTimeStamp:AudioTimeStamp; inBusNumber, inNumberFrames:Cardinal; ioData:PAudioBufferList):OSStatus; CDecl;

    PAURenderCallbackStruct = ^AURenderCallbackStruct;
    AURenderCallbackStruct = Record
        inputProc:RenderCallbackShared;
    	inputProcRefCon:Pointer;
    End;

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


Function AUGraphSetNodeInputCallback (inGraph:AUGraph; inDestNode:Integer; inDestInputNumber:Cardinal;
                inInputCallback:PAURenderCallbackStruct):OSStatus; cdecl; external libAudioToolbox name 'AUGraphSetNodeInputCallback';

Function AUGraphNodeInfo(inGraph: AUGraph;
		inNode: AUNode;
		out  outDescription: AudioComponentDescription;
		out  outAudioUnit: AudioUnit):OSStatus; cdecl; external libAudioToolbox name  'AUGraphNodeInfo';

Function AUGraphConnectNodeInput(inGraph: AUGraph;
		inSourceNode: AUNode;
		inSourceOutputNumber: UInt32;
		inDestNode: AUNode;
		inDestInputNumber: UInt32):OSStatus; cdecl; external libAudioToolbox name 'AUGraphConnectNodeInput';

Function AUGraphIsRunning(inGraph:AUGraph; out outIsRunning:Boolean):OSStatus; cdecl; external libAudioToolbox name 'AUGraphIsRunning';

Function DisposeAUGraph(inGraph:AUGraph):OSStatus; cdecl; external libAudioToolbox name 'DisposeAUGraph';

Function MusicDeviceMIDIEvent(inUnit: MusicDeviceComponent;
		inStatus: UInt32;
		inData1: UInt32;
		inData2: UInt32;
		inOffsetSampleFrame: UInt32):OSStatus; cdecl; external libAudioUnit name  'MusicDeviceMIDIEvent';

Implementation

Procedure AudioStreamBasicDescription.SetAUCanonical(nChannels:Cardinal; interleaved:Boolean);
Begin
       mFormatID := kAudioFormatLinearPCM;
       mFormatFlags := kAudioFormatFlagsCanonical;

       mChannelsPerFrame := nChannels;
       mFramesPerPacket := 1;
       mBitsPerChannel := 8 * SizeOf(AudioUnitSampleType);
       if (interleaved) Then
       Begin
            mBytesPerFrame := nChannels * SizeOf(AudioUnitSampleType);
           mBytesPerPacket := mBytesPerFrame;
       End Else
       Begin
            mBytesPerFrame := SizeOf(AudioUnitSampleType);
           mBytesPerPacket := mBytesPerFrame;
           mFormatFlags := mFormatFlags Or kAudioFormatFlagIsNonInterleaved;
       End;
End;

End.
