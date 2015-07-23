Unit TERRA_CoreAudioDriver;

Interface

Uses TERRA_String, TERRA_AudioMixer, MacOSAll, CoreAudio;

Const
  kAudioUnitSubType_RemoteIO =  kAudioUnitSubType_DefaultOutput;

Type
  CoreAudioDriver = Class(TERRAAudioDriver)
    Protected
	    mGraph:AUGraph;
	    mMixer:AudioUnit;

      mixer_desc:AudioComponentDescription;
      output_desc:AudioComponentDescription;

      sinPhase:Single;

    Public
      Function Reset(AFrequency, InitBufferSize:Cardinal; RenderProc:AudioRenderBufferProc):Boolean; Override;
      Procedure Release; Override;

      Procedure Update(); Override;

    End;

Implementation

// audio render procedure, don't allocate memory, don't take any locks, don't waste time
Function renderInput(inRefCon:Pointer; Var ioActionFlags:AudioUnitRenderActionFlags; Var inTimeStamp:AudioTimeStamp; inBusNumber, inNumberFrames:Cardinal; ioData:PAudioBufferList):OSStatus; CDecl;
Var
  Driver:CoreAudioDriver;
  outA:PAudioSampleType;
  Freq, Phase, phaseIncrement, sinSignal:Single;
  I:Integer;
Begin
	// Get a reference to the object that was passed with the callback
	// In this case, the AudioController passed itself so that you can access its data.
	Driver := CoreAudioDriver(inRefCon);

	// Get a pointer to the dataBuffer of the AudioBufferList
	outA := PAudioSampleType(ioData.mBuffers[0].mData);

	// Calculations to produce a 600 Hz sinewave
	// A constant frequency value, you can pass in a reference vary this.
	freq := 600;
	// The amount the phase changes in  single sample
	phaseIncrement := PI * freq / 44100.0;
	// Pass in a reference to the phase value, you have to keep track of this
	// so that the sin resumes right where the last call left off
	phase := Driver.sinPhase;

	// Loop through the callback buffer, generating samples
  For I:=0 To Pred(inNumberFrames) Do
  Begin
    // calculate the next sample
    sinSignal := sin(phase);
    // Put the sample into the buffer
    // Scale the -1 to 1 values float to
    // -32767 to 32767 and then cast to an integer
    outA^ := SmallInt(Trunc(sinSignal * 32767.0));
    Inc(outA);
    // calculate the phase for the next sample
    Phase := Phase + phaseIncrement;

    // Reset the phase value to prevent the float from overflowing
    If (phase >=  PI * freq) Then
		  phase := phase - PI * freq;
	End;

	// Store the phase for the next callback.
	Driver.sinPhase := phase;

	Result := noErr;
End;

{ CoreAudioDriver }
Function CoreAudioDriver.Reset(AFrequency, InitBufferSize:Cardinal; RenderProc:AudioRenderBufferProc):Boolean;
Var
  I:Integer;
  Status:OSStatus;
  // AUNodes represent AudioUnits on the AUGraph and provide an easy means for connecting audioUnits together.
  outputNode:AUNode;
	mixerNode:AUNode;
  numbuses, Size:Cardinal;
  desc:AudioStreamBasicDescription;
  renderCallbackStruct:AURenderCallbackStruct;
  Temp:AudioComponentDescription;
Begin
  Self._RenderProc := RenderProc;
  Self._Frequency := AFrequency;
  Self._OutputBufferSize := InitBufferSize;

	// Setup the AUGraph, add AUNodes, and make connections

	// create a new AUGraph
	Status := NewAUGraph(mGraph);

  // Create AudioComponentDescriptions for the AUs we want in the graph mixer component
	mixer_desc.componentType := kAudioUnitType_Mixer;
	mixer_desc.componentSubType := kAudioUnitSubType_MultiChannelMixer;
	mixer_desc.componentFlags := 0;
	mixer_desc.componentFlagsMask := 0;
	mixer_desc.componentManufacturer := kAudioUnitManufacturer_Apple;

	//  output component
	output_desc.componentType := kAudioUnitType_Output;
	output_desc.componentSubType := kAudioUnitSubType_RemoteIO;
	output_desc.componentFlags := 0;
	output_desc.componentFlagsMask := 0;
	output_desc.componentManufacturer := kAudioUnitManufacturer_Apple;

  // Add nodes to the graph to hold our AudioUnits,
	// You pass in a reference to the  AudioComponentDescription and get back an  AudioUnit
	Status := AUGraphAddNode(mGraph, @output_desc, outputNode);
	Status := AUGraphAddNode(mGraph, @mixer_desc,  mixerNode);

	// Now we can manage connections using nodes in the graph.
  // Connect the mixer node's output to the output node's input
	Status := AUGraphConnectNodeInput(mGraph, mixerNode, 0, outputNode, 0);

  // open the graph AudioUnits are open but not initialized (no resource allocation occurs here)
	Status := AUGraphOpen(mGraph);

	// Get a link to the mixer AU so we can talk to it later
	Status := AUGraphNodeInfo(mGraph, mixerNode, Temp, mMixer);

	//*** Make connections to the mixer unit's inputs ***
  // Set the number of input busses on the Mixer Unit
	// Right now we are only doing a single bus.
	numbuses := 1;
	size := SizeOf(numbuses);
  Status := AudioUnitSetProperty(mMixer, kAudioUnitProperty_ElementCount, kAudioUnitScope_Input, 0, @numbuses, size);

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
		desc.mBitsPerChannel := SizeOf(AudioSampleType) * 8; // AudioSampleType == 16 bit signed ints
		desc.mChannelsPerFrame := 1;
		desc.mFramesPerPacket := 1;
		desc.mBytesPerFrame := ( desc.mBitsPerChannel Div 8 ) * desc.mChannelsPerFrame;
		desc.mBytesPerPacket := desc.mBytesPerFrame * desc.mFramesPerPacket;

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
	desc.SetAUCanonical(1, true);
	desc.mSampleRate := Self._Frequency;

       // Apply the modified CAStreamBasicDescription to the output Audio Unit
	Status := AudioUnitSetProperty(  mMixer, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 0, @desc, SizeOf(desc));

        // Once everything is set up call initialize to validate connections
	Status := AUGraphInitialize(mGraph);

  // Start the AUGraph
	Status := AUGraphStart(mGraph);

  Result := True;
End;

Procedure CoreAudioDriver.Release;
Var
  I:Integer;
  isRunning:Boolean;
  Status:OSStatus;
Begin
  isRunning := False;

  // Check to see if the graph is running.
  Status := AUGraphIsRunning(mGraph, isRunning);
  // If the graph is running, stop it.
  If (isRunning) Then
    Status := AUGraphStop(mGraph);

  DisposeAUGraph(mGraph);
End;

Procedure CoreAudioDriver.Update();
Begin
End;

End.
