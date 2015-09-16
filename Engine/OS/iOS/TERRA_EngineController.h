#import <UIKit/UIKit.h>
#import <CoreMotion/CMMotionManager.h>
#import <CoreLocation/CLLocationManager.h>

#import "GADInterstitial.h"

#import <AdBuddiz/AdBuddiz.h>

#import <Chartboost/Chartboost.h>
#import <Chartboost/CBNewsfeed.h>

#import <VungleSDK/VungleSDK.h>


#import "TERRA_EAGLView.h"
#import "TERRA_MidiPlayer.h"


@class AccelerometerFilter;
@class InAppPurchaseManager;


EAGLView *openglView;

@interface EngineController : UIViewController<UIAccelerometerDelegate,
GADInterstitialDelegate, CLLocationManagerDelegate, AdBuddizDelegate,
ChartboostDelegate, VungleSDKDelegate>
{
@public
	int touchX;
	int touchY;
    
    int adCount;
    
    bool showedAdBuddiz;
    bool showedAdMob;
    bool showediAd;
    bool showedChartboost;
    bool showedVungle;
    
    bool iADSupported;
    

	AccelerometerFilter *filter;
	CMMotionManager *motionManager;
	CLLocationManager *locationManager;
    
    InAppPurchaseManager *iAPManager;
    MidiPlayer *midiPlayer;
    
    UIViewController *tempController;
    
    UIDeviceOrientation currentOrientation;
    
    GADInterstitial *adMobinterstitial;
    NSString* adMobinterstitialID;
    
    EAGLView *glView;
}

+ (EngineController*)getInstance;
- (GADInterstitial *)createAndLoadAdMobInterstitial;
-(void)showFullscreenAd;
-(void)startEngine;

@end