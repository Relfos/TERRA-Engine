#import <AudioToolbox/MusicPlayer.h>

#define MAX_TRACKS 16
#define MAX_PRESETS 128

@interface MidiPlayer : NSObject 
{
	MusicSequence sequence;
	MusicPlayer player;	
	MusicTimeStamp length;
	
	MusicTrack tracks[MAX_TRACKS];
	
	int busCount;
	
	AUGraph processingGraph;

	AUNode ioNode;
	AudioUnit ioUnit;
	
	AUNode mixerNode;
	AudioUnit mixerUnit;
	
	AUNode samplerNode[MAX_PRESETS];
	AudioUnit samplerUnit[MAX_PRESETS];	
	bool hasPreset[MAX_PRESETS];
}

// public methods
- (void)loadStore;
- (BOOL)canMakePurchases;
- (void)purchaseProduct;
- (void)requestProductData:(char *)s;


@end 