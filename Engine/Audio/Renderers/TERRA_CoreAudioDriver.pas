Unit TERRA_CoreAudioDriver;

Interface

Uses TERRA_Error, TERRA_Utils, TERRA_String, TERRA_AudioMixer, TERRA_AudioBuffer, MacOSAll, CoreAudio;

Const
  kAudioUnitSubType_RemoteIO =  kAudioUnitSubType_DefaultOutput;

Type
  CoreAudioDriver = Class(TERRAAudioDriver)
    Protected
	   (* mGraph:AUGraph;
	    mMixer:AudioUnit;

      mixer_desc:AudioComponentDescription;
      output_desc:AudioComponentDescription; *)

      _OutputUnit:AudioUnit;

    Public

      Function Reset(Mixer:TERRAAudioMixer):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;

Implementation

// audio render procedure, don't allocate memory, don't take any locks, don't waste time
Function renderInput(inRefCon:Pointer; Var ioActionFlags:AudioUnitRenderActionFlags; Var inTimeStamp:AudioTimeStamp; inBusNumber, inNumberFrames:Cardinal; ioData:PAudioBufferList):OSStatus; CDecl;
Var
  Driver:CoreAudioDriver;
Begin
	// Get a reference to the object that was passed with the callback
	// In this case, the AudioController passed itself so that you can access its data.
	Driver := CoreAudioDriver(inRefCon);
  Driver._Mixer.RequestSamples(ioData.mBuffers[0].mData, inNumberFrames);
  Result := noErr;
End;

{ CoreAudioDriver }
Function CoreAudioDriver.Reset(Mixer:TERRAAudioMixer):Boolean;
Var
  I:Integer;
  Status:OSStatus;
  (*
  // AUNodes represent AudioUnits on the AUGraph and provide an easy means for connecting audioUnits together.
  outputNode:AUNode;
	mixerNode:AUNode;
  numbuses, Size:Cardinal;
  desc:AudioStreamBasicDescription;
  renderCallbackStruct:AURenderCallbackStruct;
  Temp:AudioComponentDescription;   *)

  outputcd:AudioComponentDescription;

  input:AURenderCallbackStruct;
  comp:AudioComponent;

  streamFormat:AudioStreamBasicDescription;
Begin
  Self._Mixer := Mixer;

  //  10.6 and later: generate description that will match out output device (speakers)
  FillChar(outputcd, SizeOf(Outputcd), 0); // 10.6 version
  outputcd.componentType := kAudioUnitType_Output;
  outputcd.componentSubType := kAudioUnitSubType_DefaultOutput;
  outputcd.componentManufacturer := kAudioUnitManufacturer_Apple;

  comp := AudioComponentFindNext(Nil, OutputCd);
  If (comp = Nil) Then
  Begin
    RaiseError('could not find audio component');
    Exit;
  End;

  Status := AudioComponentInstanceNew(comp, _OutputUnit);
  If Status <> noErr Then
  Begin
    RaiseError('Couldnt open component for outputUnit');
    Exit;
  End;

  // set audio stream format

  streamFormat.mSampleRate := _Mixer.Buffer.Frequency;
  streamFormat.mFormatID := kAudioFormatLinearPCM;
  streamFormat.mFormatFlags := kAudioFormatFlagIsSignedInteger Or kAudioFormatFlagIsPacked; //kAudioFormatFlagIsFloat;
  streamFormat.mFramesPerPacket := 1;
  streamFormat.mChannelsPerFrame := 2;
  streamFormat.mBitsPerChannel := 16;
  streamFormat.mBytesPerFrame := ( streamFormat.mBitsPerChannel Div 8 ) * streamFormat.mChannelsPerFrame;
  streamFormat.mBytesPerPacket := streamFormat.mBytesPerFrame * streamFormat.mFramesPerPacket;

  Status := AudioUnitSetProperty (_OutputUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, @streamFormat, sizeof(streamFormat));
  If Status<>NoErr Then
      RaiseError('Failed to set audio unit input property.');

  // register render callback
  input.inputProc := renderInput;
  input.inputProcRefCon := Self;
  Status := AudioUnitSetProperty(_OutputUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input,  0, @input, sizeof(input));

  If Status <> NoErr Then
     RaiseError('AudioUnitSetProperty failed');

  	// initialize unit
  Status := AudioUnitInitialize(_OutputUnit);
  If Status<>NoERr Then
     RAiseError('Couldnt initialize output unit');

  Status := AudioOutputUnitStart(_OutputUnit);
  If Status<>NoErr Then
     RaiseError('Couldnt start output unit');


(*	// Setup the AUGraph, add AUNodes, and make connections

	// create a new AUGraph
	Status := NewAUGraph(mGraph);
        WriteLn('newgraph ',Status);

  // Create AudioComponentDescriptions for the AUs we want in the graph mixer component
	mixer_desc.componentType := kAudioUnitType_Mixer;
	mixer_desc.componentSubType := kAudioUnitSubType_MultiChannelMixer;
	mixer_desc.componentFlags := 0;
	mixer_desc.componentFlagsMask := 0;
	mixer_desc.componentManufacturer := kAudioUnitManufacturer_Apple;

	//  output component
	output_desc.componentType := kAudioUnitType_Output;
	output_desc.componentSubType := kAudioUnitSubType_DefaultOutput;
	output_desc.componentFlags := 0;
	output_desc.componentFlagsMask := 0;
	output_desc.componentManufacturer := kAudioUnitManufacturer_Apple;

  // Add nodes to the graph to hold our AudioUnits,
	// You pass in a reference to the  AudioComponentDescription and get back an  AudioUnit
	Status := AUGraphAddNode(mGraph, @output_desc, outputNode);
        WriteLn('AUGraphAddNode output ',Status);

	Status := AUGraphAddNode(mGraph, @mixer_desc,  mixerNode);
          WriteLn('AUGraphAddNode mixer ',Status);

          // open the graph AudioUnits are open but not initialized (no resource allocation occurs here)
          Status := AUGraphOpen(mGraph);
           WriteLn('AUGraphOpen ',Status);


	// Now we can manage connections using nodes in the graph.
  // Connect the mixer node's output to the output node's input
	Status := AUGraphConnectNodeInput(mGraph, mixerNode, 0, outputNode, 0);
          WriteLn('AUGraphConnectNodeInput output ',Status);


	// Get a link to the mixer AU so we can talk to it later
	Status := AUGraphNodeInfo(mGraph, mixerNode, Temp, mMixer);
         WriteLn('AUGraphNodeInfo ',Status);


	//*** Make connections to the mixer unit's inputs ***
  // Set the number of input busses on the Mixer Unit
	// Right now we are only doing a single bus.
	numbuses := 1;
	size := SizeOf(numbuses);
  Status := AudioUnitSetProperty(mMixer, kAudioUnitProperty_ElementCount, kAudioUnitScope_Input, 0, @numbuses, size);
         WriteLn('AudioUnitSetProperty ',Status);

	// Loop through and setup a callback for each source you want to send to the mixer.
	// Right now we are only doing a single bus so we could do without the loop.
  For I:=0 To Pred(numbuses) Do
  Begin
		// Setup render callback struct
		// This struct describes the function that will be called
		// to provide a buffer of audio samples for the mixer unit.

		renderCallbackStruct.inputProc := @renderInput;
		renderCallbackStruct.inputProcRefCon := Self;

    // Set a callback for the specified node's specified input
    Status := AUGraphSetNodeInputCallback(mGraph, mixerNode, i, @renderCallbackStruct);

		// Get a CAStreamBasicDescription from the mixer bus.
                size := SizeOf(desc);
		Status := AudioUnitGetProperty(mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, i, @desc, size);

		// Initializes the structure to 0 to ensure there are no spurious values.
		FillChar(desc, SizeOf(Desc), 0);

		// Make modifications to the CAStreamBasicDescription
		// We're going to use 16 bit Signed Ints because they're easier to deal with
		// The Mixer unit will accept either 16 bit signed integers or
		// 32 bit 8.24 fixed point integers.
		desc.mSampleRate := Self._Frequency; // set sample rate
		desc.mFormatID := kAudioFormatLinearPCM;
		desc.mFormatFlags      := kAudioFormatFlagIsSignedInteger Or kAudioFormatFlagIsPacked;
		desc.mBitsPerChannel := 16; // AudioSampleType == 16 bit signed ints
		desc.mChannelsPerFrame := 1;
		desc.mFramesPerPacket := 1;
		desc.mBytesPerFrame := ( desc.mBitsPerChannel Div 8 ) * desc.mChannelsPerFrame;
		desc.mBytesPerPacket := desc.mBytesPerFrame * desc.mFramesPerPacket;


                WriteLn('frequency', desc.mSampleRate);
                WriteLn('bits per channel', desc.mBitsPerChannel);

		//printf("Mixer file format: "); desc.Print();

		// Apply the modified CAStreamBasicDescription to the mixer input bus
		Status := AudioUnitSetProperty(  mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, i, @desc, SizeOf(desc));
	End;

	// Apply the CAStreamBasicDescription to the mixer output bus
	Status := AudioUnitSetProperty(	 mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 0, @desc, SizeOf(desc));

	//*** Setup the audio output stream ***

	// Get a CAStreamBasicDescription from the output Audio Unit
  Status := AudioUnitGetProperty(  mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 0, 	@desc,	size);

	// Initializes the structure to 0 to ensure there are no spurious values.
	FillChar(desc, SizeOf(desc), 0);

	// Make modifications to the CAStreamBasicDescription
	// AUCanonical on the iPhone is the 8.24 integer format that is native to the iPhone.
	// The Mixer unit does the format shifting for you.
	//desc.SetAUCanonical(1, true);
	//desc.mSampleRate := Self._Frequency;

       // Apply the modified CAStreamBasicDescription to the output Audio Unit
	Status := AudioUnitSetProperty(  mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 0, @desc, SizeOf(desc));

        // Once everything is set up call initialize to validate connections
	Status := AUGraphInitialize(mGraph);

  // Start the AUGraph
	Status := AUGraphStart(mGraph);   *)

  Result := True;
End;

Procedure CoreAudioDriver.Release;
Begin
  AudioOutputUnitStop(_OutputUnit);
  AudioUnitUninitialize(_OutputUnit);
  AudioComponentInstanceDispose(_OutputUnit);
End;

Procedure CoreAudioDriver.Update();
Begin
End;

End.
