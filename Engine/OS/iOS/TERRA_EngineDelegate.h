//
//  Created by Sergio Flores on 3/3/11.
//  Copyright Sergio Flores 2011. All rights reserved.
//

#import "TERRA_AdMobViewController.h"
#import "TERRA_iAdViewController.h"
#import "iCade/iCadeReaderView.h"
#import "TERRA_EAGLView.h"

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
@class EngineController;
@class ADBannerView;
@class iAdViewController;


@interface EngineDelegate : NSObject <UIApplicationDelegate, UITextViewDelegate, iCadeEventDelegate>
{
@public
    UIWindow *window;
    EngineController *viewController;
    
    
    iAdViewController *_iAdViewController;
    AdMobViewController *_adMobViewController;
        
    //KPViewPosition kpViewPosition;
    
}

+ (EngineDelegate*)getInstance;
- (void)showAdMobView;
- (void)hideAdMobView;
- (void)showIADView;

@end

