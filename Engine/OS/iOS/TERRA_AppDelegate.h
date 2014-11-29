//
//  Created by Sergio Flores on 3/3/11.
//  Copyright Sergio Flores 2011. All rights reserved.
//

#import "TERRA_MidiPlayer.h"
#import "TERRA_AdMobViewController.h"
#import "TERRA_iAdViewController.h"
#import "iCade/iCadeReaderView.h"

#define keyGamepadLeft    200
#define keyGamepadUp      201
#define keyGamepadRight   202
#define keyGamepadDown    203
#define keyGamepadA       204
#define keyGamepadB       205
#define keyGamepadC       206
#define keyGamepadD       207
#define keyGamepadX       208
#define keyGamepadY       209
#define keyGamepadZ       210
#define keyGamepadR       211
#define keyGamepadL       212
#define keyGamepadMenu    213

@class EAGLView;
@class InputController;
@class InAppPurchaseManager;
@class ADBannerView;
@class iAdViewController;

EAGLView *openglView;


@interface TERRA_AppDelegate : NSObject <UIApplicationDelegate, UITextViewDelegate, iCadeEventDelegate>
{
    UIWindow *window;
    EAGLView *glView;
    InputController *viewController;
    InAppPurchaseManager *iAPManager;
	MidiPlayer *midiPlayer;
    
    iAdViewController *_iAdViewController;
    AdMobViewController *_adMobViewController;
    
	BOOL iADSupported;
    
    //KPViewPosition kpViewPosition;
    
}

+ (TERRA_AppDelegate*)getInstance;
- (void)showAdMobView;
- (void)hideAdMobView;
- (void)showIADView;

@property (nonatomic, retain) IBOutlet InputController *viewController;
@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet EAGLView *glView;
@property (nonatomic, retain) IBOutlet InAppPurchaseManager *iAPManager;
@property (nonatomic, retain) IBOutlet ADBannerView *adBannerView;
@property (nonatomic, retain) IBOutlet GADBannerView *adMobView;
@end

