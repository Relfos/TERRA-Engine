#import <AudioToolbox/MusicPlayer.h>

#import "TERRA_MidiPlayer.h"

#include "PascalImports.h"

@implementation MidiPlayer


-(void) create
{
    OSStatus result = noErr;
	
	for (int i=0; i<MAX_PRESETS; i++)
	{
		samplerNode[i] = NULL;
		samplerUnit[i] = NULL;
		hasPreset[i] = false;
	}

	for (int i=0; i<MAX_TRACKS; i++)
	{
		tracks[i] = NULL;
	}

    [self createAUGraph];
}

-(void) loadMusicFile:(NSString*)fileName 
{
    // Initialise the music sequence
    NewMusicSequence(&sequence);
     
    // Create a new URL which points to the MIDI file
    NSURL * midiFileURL = [NSURL fileURLWithPath:fileName];
                            
    MusicSequenceFileLoad(sequence, (__bridge CFURLRef) midiFileURL, 0, 0);
   
	MusicSequenceSetAUGraph(sequence, processingGraph);
   
    // Initialise the music player
    NewMusicPlayer(&player);
}

-(void) loadPreset:(NSString*)fileName patchID:(int)ID
{
    NSURL * fileURL = [NSURL fileURLWithPath:fileName];
    [self loadSoundFont: fileURL withPatch:ID];
}

-(void) setChannelPreset:(int)channel presetID:(int)preset
{
	MusicSequenceGetIndTrack(sequence, 0, &tracks[channel]);
	MusicTrackSetDestNode(tracks[channel], samplerNode[preset]);
	MusicTrackLoopInfo loopInfo = { 4, 0 }; 
	OSStatus result = MusicTrackSetProperty(&tracks[channel], kSequenceTrackProperty_LoopInfo, &loopInfo, sizeof(MusicTrackLoopInfo));	
}

-(void) play
{
    // Load the sequence into the music player
    MusicPlayerSetSequence(player, sequence);
	
    // Called to do some MusicPlayer setup. This just reduces latency when MusicPlayerStart is called
    MusicPlayerPreroll(player);
	
    // Starts the music playing
    MusicPlayerStart(player);

    // Get length of track so that we know how long to kill time for
   /* UInt32 sz = sizeof(MusicTimeStamp);
    MusicSequenceGetIndTrack(sequence, 1, &t);
    MusicTrackGetProperty(t, kSequenceTrackProperty_TrackLength, &length, &sz);*/
}

-(void) stop
{
	MusicPlayerStop(player);
	DisposeMusicSequence(sequence);
	DisposeMusicPlayer(player);
	
	player = NULL;
	sequence = NULL;
}

- (BOOL) createAUGraph 
{    
    // Each core audio call returns an OSStatus. This means that we Can see if there have been any errors in the setup
	OSStatus result = noErr;
       
    // Specify the common portion of an audio unit's identify, used for both audio units
    // in the graph.
    // Setup the manufacturer - in this case Apple
	AudioComponentDescription cd = {};
	cd.componentManufacturer     = kAudioUnitManufacturer_Apple;
    
    // Instantiate an audio processing graph
	result = NewAUGraph (&processingGraph);
    NSCAssert (result == noErr, @"Unable to create an AUGraph object. Error code: %d '%.4s'", (int) result, (const char *)&result);
    
	//Specify the Sampler unit, to be used as the first node of the graph
	cd.componentType = kAudioUnitType_MusicDevice; // type - music device
	cd.componentSubType = kAudioUnitSubType_Sampler; // sub type - sampler to convert our MIDI

	busCount = 0;
	
    // Add the Sampler unit node to the graph
	for (int i=0; i<128;i++)
	if (hasPreset[i])
	{
		busCount++;
		result = AUGraphAddNode (processingGraph, &cd, &samplerNode[i]);
		NSCAssert (result == noErr, @"Unable to add the Sampler unit to the audio processing graph. Error code: %d '%.4s'", (int) result, (const char *)&result);
	}
    
    // Add a Mixer unit node to the graph
	cd.componentType          = kAudioUnitType_Mixer;
	cd.componentSubType       = kAudioUnitSubType_MultiChannelMixer;
	result = AUGraphAddNode (processingGraph, &cd, &mixerNode);
	NSCAssert (result == noErr, @"Unable to add the Mixer  unit to the audio processing graph. Error code: %d '%.4s'", (int) result, (const char *)&result);
	
    // Add the Output unit node to the graph
	// Specify the Output unit, to be used as the second and final node of the graph	
	cd.componentType = kAudioUnitType_Output;  // Output
	cd.componentSubType = kAudioUnitSubType_RemoteIO;  // Output to speakers
	result = AUGraphAddNode (processingGraph, &cd, &ioNode);
    NSCAssert (result == noErr, @"Unable to add the Output unit to the audio processing graph. Error code: %d '%.4s'", (int) result, (const char *)&result);
    
    // Open the graph
	result = AUGraphOpen (processingGraph);
    NSCAssert (result == noErr, @"Unable to open the audio processing graph. Error code: %d '%.4s'", (int) result, (const char *)&result);
    
    // Obtain the mixer unit instance from its corresponding node
	AUGraphNodeInfo(processingGraph, mixerNode, NULL, &mixerUnit);

	// Set the bus count for the mixer
	AudioUnitSetProperty(mixerUnit,
						kAudioUnitProperty_ElementCount,
                        kAudioUnitScope_Input,
                        0,
                        &busCount, sizeof(busCount));



	for (int i=0; i<128;i++)
	if (hasPreset[i])
	{
		AUGraphConnectNodeInput(processingGraph, samplerNode[i], 0, mixerNode, 0);
	}
	
	// Connect the mixer unit to the output unit
	AUGraphConnectNodeInput(processingGraph, mixerNode, 0, ioNode, 0);

	// Obtain references to all of the audio units from their nodes
	for (int i=0; i<128;i++)
	if (hasPreset[i])
	{
		AUGraphNodeInfo (processingGraph, samplerNode[i], 0, &samplerUnit[i]);
	}
	
	AUGraphNodeInfo (processingGraph, ioNode, 0, &ioUnit);

      
    // Initialize the audio processing graph.
    result = AUGraphInitialize (processingGraph);
    NSAssert (result == noErr, @"Unable to initialze AUGraph object. Error code: %d '%.4s'", (int) result, (const char *)&result);
        
    // Start the graph
    result = AUGraphStart (processingGraph);
    NSAssert (result == noErr, @"Unable to start audio processing graph. Error code: %d '%.4s'", (int) result, (const char *)&result);
        
    return YES;
}

-(OSStatus) loadSoundFont: (NSURL *)bankURL withPatch: (int)presetNumber {

    OSStatus result = noErr;

    // fill out a bank preset data structure
    AUSamplerBankPresetData bpdata;
    bpdata.bankURL  = (__bridge CFURLRef) bankURL;
    bpdata.bankMSB  = kAUSampler_DefaultMelodicBankMSB;
    bpdata.bankLSB  = kAUSampler_DefaultBankLSB;
    bpdata.presetID = (UInt8) presetNumber;

    // set the kAUSamplerProperty_LoadPresetFromBank property
    result = AudioUnitSetProperty(samplerUnit[presetNumber],
                              kAUSamplerProperty_LoadPresetFromBank,
                              kAudioUnitScope_Global,
                              0,
                              &bpdata,
                              sizeof(bpdata));

    // check for errors
    NSCAssert (result == noErr,
           @"Unable to set the preset property on the Sampler. Error code:%d '%.4s'",
           (int) result,
           (const char *)&result);

    return result;
}


@end
