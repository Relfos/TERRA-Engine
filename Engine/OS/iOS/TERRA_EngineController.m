#import <CoreLocation/CoreLocation.h>
#import <CoreMotion/CoreMotion.h>
#import <UIKit/UIKit.h>

#import "Flurry.h"

#import <AdBuddiz/AdBuddiz.h>

#import <Chartboost/Chartboost.h>
#import <Chartboost/CBNewsfeed.h>

#import <CommonCrypto/CommonDigest.h>
#import <AdSupport/AdSupport.h>

#import "FBSettings.h"
#import "FBAppEvents.h"

#import <VungleSDK/VungleSDK.h>

#import "PascalImports.h"
#import "TERRA_Utils.h"

#import "TERRA_EngineController.h"
#import "TERRA_EngineDelegate.h"
#import "TERRA_EAGLView.h"
#import "TERRA_AccelerometerFilter.h"

@implementation EngineController 

bool _hasGyro = false;
bool _hasCompass = false;

bool hasFlurry = false;
bool hasAdBuddiz = false;
bool hasAdMobBanner = false;
bool hasAdMobInterstitial = false;
bool hasChartboost = false;
bool hasVungle = false;

EngineController *_controller = nil;


void AnalyticsLog(char *s)
{
    if (!hasFlurry) return;
    NSString *a = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    NSLog(@"Sending analytics event: %s", s);
    [Flurry logEvent:a];
}

void AnalyticsLogWithParams(char *s, char *s2)
{
    if (!hasFlurry) return;
    NSString *a = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    NSString *b = [NSString stringWithCString:s2 encoding:NSASCIIStringEncoding];
    NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys:b, a, nil];
    NSLog(@"Sending analytics event: %s - %s", s, s2);
    [Flurry logEvent:a withParameters:dictionary];
}

void showFullscreenAds()
{
    [_controller showFullscreenAd];
}

+ (EngineController*)getInstance
{
    return _controller;
}

bool InitAccelerometer()
{
	[_controller InitAccelerometer]; 
	return true;
}

bool InitGyroscope()
{
	if (_hasGyro)
	{
		[_controller InitGyroscope]; 
		return true;
	}
	else
		return false;
}

bool InitCompass()
{
	if (_hasCompass)
	{
		[_controller InitCompass]; 
		return true;
	}
	else
		return false;
}

bool StopAccelerometer()
{
	[_controller StopAccelerometer]; 
	return true;
}

bool StopGyroscope()
{
	if (_hasGyro)
	{
		[_controller StopGyroscope]; 
		return true;
	}
	else
		return false;
}

bool StopCompass()
{
	if (_hasCompass)
	{
		[_controller StopCompass]; 
		return true;
	}
	else
		return false;
}

- (void)InitAccelerometer
{
	float updateFrequency = 30.0f;
	
	filter = [[LowpassFilter alloc] initWithSampleRate:updateFrequency cutoffFrequency:5.0];
	filter.adaptive = YES;	
	
	[[UIAccelerometer sharedAccelerometer] setUpdateInterval:1.0 / updateFrequency];
	[[UIAccelerometer sharedAccelerometer] setDelegate:self];
	NSLog(@"Initialized Accelerometer");
}

- (void)StopAccelerometer
{
	[[UIAccelerometer sharedAccelerometer] setDelegate:nil];
	NSLog(@"Stopped Accelerometer");
}

- (void)InitGyroscope
{
	if (!_hasGyro)
		return;

	float updateFrequency = 30.0f;
	
	// Start the gyroscope if it is not active already 
	if([_controller->motionManager isGyroActive] == NO)
	{
		_hasGyro = true;
		[_controller->motionManager setGyroUpdateInterval:1.0f / updateFrequency];

		// Add on a handler block object 
		// Receive the gyroscope data on this block 
		[_controller->motionManager startGyroUpdatesToQueue:[NSOperationQueue mainQueue]
		 withHandler:^(CMGyroData *gyroData, NSError *error)
		{
			NSString *x = [[NSString alloc] initWithFormat:@"%.02f",gyroData.rotationRate.x];
			NSString *y = [[NSString alloc] initWithFormat:@"%.02f",gyroData.rotationRate.y];
			NSString *z = [[NSString alloc] initWithFormat:@"%.02f",gyroData.rotationRate.z];
			NSLog(x);
			NSLog(y);
			NSLog(z);
			ApplicationOnGyroscope(gyroData.rotationRate.x, gyroData.rotationRate.y, gyroData.rotationRate.z);
		}];
	}
	
	NSLog(@"Initialized Gyroscope");
}

- (void)StopGyroscope
{
	[_controller->motionManager stopGyroUpdates];
	NSLog(@"Stopped Gyroscope");
}

- (void)InitCompass
{
	if (!_hasCompass)
		return;

	float updateFrequency = 30.0f;
	
	locationManager.desiredAccuracy = kCLLocationAccuracyBest;
	locationManager.headingFilter = 1;
	locationManager.delegate = self;
	[locationManager startUpdatingHeading];
	
	NSLog(@"Initialized Compass");
}

- (void)StopCompass
{
	if (!_hasCompass)
		return;

	[locationManager stopUpdatingHeading];
	NSLog(@"Stopped Compass");
}
	
// Implement viewDidLoad to do additional setup after loading the view.
-(void)viewDidLoad
{
	[super viewDidLoad];
	_controller = self;

	motionManager = [[CMMotionManager alloc] init];
	locationManager=[[CLLocationManager alloc] init];
	
	_hasGyro =  ([motionManager isGyroAvailable]);
	_hasCompass = [locationManager headingAvailable];
    
    UIWindow* window = [UIApplication sharedApplication].keyWindow;
   
    //CGRect screen = GetScreenBounds();
    //CGRect screen =[[UIScreen mainScreen] bounds];

    CGRect screen = window.frame;
    
    UIView *mainView = [[UIView alloc] initWithFrame:screen];
    self.view = mainView;
    
    
    glView = [[EAGLView alloc] initWithFrame: screen];
    
    [window addSubview:mainView];
    //  [window bringSubviewToFront:glView];
    
    //[window insertSubview:glView  belowSubview:self.view];
    [mainView addSubview:glView];
    //[window addSubview:glView];
    

    [glView setAnimationInterval:60.0];
    [glView startAnimation];
    openglView = glView;
    
    /*iCadeReaderView *control = [[iCadeReaderView alloc] initWithFrame:screen];
    [glView addSubview:control];
    control.active = YES;
    control.delegate = [EngineDelegate getInstance];
    //[control release];
 */
    
    iAPManager = [InAppPurchaseManager alloc];
    [iAPManager loadStore];
    
    midiPlayer = [MidiPlayer alloc];
    [midiPlayer create];
}

-(void)viewDidUnload
{
	[super viewDidUnload];
}

- (GADInterstitial *)createAndLoadAdMobInterstitial {

    if (self->adMobinterstitialID == nil) {
        return nil;
    }
    
    GADInterstitial *interstitial = [[GADInterstitial alloc] init];
    interstitial.adUnitID = self->adMobinterstitialID;
    interstitial.delegate = self;

    GADRequest *request = [GADRequest request];        
    request.testDevices = @[ GAD_SIMULATOR_ID ]; // Requests test ads on simulators.
    [interstitial loadRequest:request];        

    return interstitial;
}

-(void)showNextAd {

    
    self->adCount++;
    if (self->adCount>40)
        return;
    
    NSInteger randomNumber = arc4random() % 3;
    
    randomNumber = 0;
    NSLog(@"Random ad: %d", randomNumber);
    
    switch (randomNumber) {
        case 0:
            if (!self->showedAdBuddiz && hasAdBuddiz) {
                self->showedAdBuddiz = true;
                
                NSLog(@"Calling AdBuddiz interstial");

                [AdBuddiz showAd];
                return;
            }
            break;
           
        case 1:
            if (!self->showedAdMob && hasAdMobInterstitial) {
                if ([self->adMobinterstitial isReady]) {
                    self->showedAdMob = true;
                    
                    NSLog(@"Calling adMob interstial");

                    
                    [self->adMobinterstitial presentFromRootViewController:self];
                    return;
                }
               return;
            }
            break;
            
        case 2:
            if (!self->showedChartboost && hasChartboost) {
                self->showedChartboost = true;
                
                NSLog(@"Calling Chartboost interstial");
                
                [Chartboost showInterstitial:CBLocationLevelComplete];
                return;
            }
            break;
            
        case 3:
            if (!self->showedVungle) {
                self->showedVungle = true;
                
                NSLog(@"Calling vungle interstial");
                
                UIWindow *window = [[UIApplication sharedApplication] keyWindow];
                self->tempController = window.rootViewController;
                
                VungleSDK* sdk = [VungleSDK sharedSDK];
                [sdk playAd:self];
                
                return;
            }
            break;
            
        case 4:
            if (!self->showediAd) {
                self->showediAd = true;
                NSLog(@"Calling iAd interstial");
                return;
            }
            break;

        default:
            break;
    }
    

    [self showNextAd];
}

-(void)showFullscreenAd {

    NSLog(@"Showing fullscreenad");
    
    self->adCount = 0;
    self->showedAdBuddiz = false;
    self->showedAdMob = false;
    self->showediAd = false;
    self->showedChartboost = false;
    self->showedVungle = false;

    [self showNextAd];
}

-(void)didFailToShowAd:(AdBuddizError) error
{
    [self showNextAd];
}

- (void)didHideAd
{
}

- (void)didDisplayInterstitial:(CBLocation)location
{
}

- (void)didFailToLoadInterstitial:(CBLocation)location
                        withError:(CBLoadError)error
{
    [self showNextAd];
}

- (void)vungleSDKwillShowAd {
    [glView stopAnimation];
//    [[UIApplication sharedApplication] setStatusBarOrientation:UIInterfaceOrientationLandscapeRight];
}

- (void)vungleSDKwillCloseAdWithViewInfo:(NSDictionary*)viewInfo willPresentProductSheet:(BOOL)willPresentProductSheet
{
//    if (!willPresentProductSheet)
    UIWindow *window = [[UIApplication sharedApplication] keyWindow];
    window.rootViewController = self->tempController;
  //  [[UIApplication sharedApplication] setStatusBarOrientation:UIInterfaceOrientationPortrait];
//    ApplicationOnContextLost();
    [glView startAnimation];
}

-(void)startEngine {
    char buf[1024];

    CGRect screen = GetScreenBounds();
    
    NSLog(@"Width: %f", screen.size.width);
    NSLog(@"Height: %f", screen.size.height);
    ApplicationSetScreenRegion(screen.size.width, screen.size.height);
        
    NSString *countryCode = [[NSLocale currentLocale]
                             objectForKey:NSLocaleCountryCode];
    strcpy(buf, [countryCode UTF8String]);	
    ApplicationSetCountry(buf); 

    strcpy(buf, [[[NSLocale preferredLanguages] objectAtIndex:0] UTF8String]);	
    ApplicationSetLanguage(buf); 

    strcpy(buf, [[[NSBundle mainBundle] resourcePath] UTF8String]);
    ApplicationSetResourcesPath(buf); 
    
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    strcpy(buf, [documentsDirectory UTF8String]);
    ApplicationDocumentPath(buf); 
    
    paths = NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES);
    NSString *tempDirectory = [paths objectAtIndex:0];
    strcpy(buf, [tempDirectory UTF8String]);
    ApplicationTempPath(buf);  

    /*
    // clear bad files from cache
    NSFileManager *fileMgr = [NSFileManager defaultManager];
    NSArray *fileArray = [fileMgr contentsOfDirectoryAtPath:tempDirectory error:nil];
    for (NSString *filename in fileArray)  
    {
        NSString *URL = [tempDirectory stringByAppendingPathComponent:filename];
        NSError *attributesError = nil;
        NSDictionary *fileAttributes = [[NSFileManager defaultManager] attributesOfItemAtPath:URL error:&attributesError];
        NSNumber *fileSizeNumber = [fileAttributes objectForKey:NSFileSize];
        long fileSize = [fileSizeNumber longLongValue];		
        NSLog(@"Found cache file: %@",URL);
        NSLog(@"Size: %i",fileSize);
        if (fileSize<=0) 
        {
            [fileMgr removeItemAtPath:URL error:NULL];
        }
    }	*/	

    ApplicationInit();

    /*char *s = ApplicationGetAppID();
    if (strlen(s)>0)// call the Appirater class
        [Appirater appLaunched];
    */
    
    char* _fbid = ApplicationGetFacebookID();
    if (strlen(_fbid)>0 && isSocialFrameworkAvailable())
    {
        NSString* myid = [NSString stringWithFormat:@"%s", _fbid];
        [FBSettings setDefaultAppID:myid];
        [FBAppEvents activateApp];
    }

    
    NSLog(@"Checking if iAd is supported");
    Class adClass = (NSClassFromString(@"ADBannerView"));
    if (adClass==nil)
    {
        iADSupported = NO;
        NSLog(@"iAd unavailable");
    }
    else
    {
        NSString   *countryCode = [[NSLocale currentLocale] objectForKey: NSLocaleCountryCode];
        NSArray *countryList = [[NSArray alloc] initWithObjects:@"US", @"FR",@"GB",@"IT",@"JP",@"ES",@"DE", nil];
        iADSupported = [countryList containsObject: countryCode];
    }
    
    iADSupported = false;
    
    NSLog(@"Initializing flurry analytics");
    char* _flurryId = ApplicationGetFlurryID();
    if (strlen(_flurryId)>0)
    {
        NSString* myid = [NSString stringWithFormat:@"%s", _flurryId];
        NSLog(@"Got ID %@",myid);
        [Flurry startSession:myid];
        hasFlurry = true;
    }
    else
        hasFlurry = false;

    NSLog(@"Initializing adBuddiiz");
    char* _adBuddizId = ApplicationGetAdBuddizID();
    if (strlen(_adBuddizId)>0)
    {
        NSString* myid = [NSString stringWithFormat:@"%s", _adBuddizId];
        NSLog(@"Got ID %@",myid);
        
        [AdBuddiz setPublisherKey:myid];
        [AdBuddiz setDelegate:self];
        
         [AdBuddiz setTestModeActive];
        [AdBuddiz cacheAds];
        hasAdBuddiz = true;
    }
    else
        hasAdBuddiz = false;
    
    NSLog(@"Initializing Vungle");
    char* _vungleId = ApplicationGetVungleID();
    if (strlen(_vungleId)>0)
    {
        NSString* myid = [NSString stringWithFormat:@"%s", _vungleId];
        NSLog(@"Got ID %@",myid);
        
        VungleSDK* sdk = [VungleSDK sharedSDK];
        [sdk setDelegate:self];
        // start vungle publisher library
        [sdk startWithAppId:myid];
        
        hasVungle = true;
    }
    else
        hasVungle = false;
    
    NSLog(@"Initializing chartboost");
    char* _chartboostId = ApplicationGetChartboostID();
    char* _chartboostSecret = ApplicationGetChartboostSecret();
    if (strlen(_chartboostId)>0 && strlen(_chartboostSecret)>0)
    {
        NSString* myid = [NSString stringWithFormat:@"%s", _chartboostId];
        NSLog(@"Got ID %@",myid);

        NSString* mysecret = [NSString stringWithFormat:@"%s", _chartboostSecret];
        NSLog(@"Got secret %@",mysecret);
        
        [Chartboost startWithAppId:myid
                      appSignature:mysecret
                          delegate:self];

        hasChartboost = true;
    }
    else
        hasChartboost = false;
    
    

    NSLog(@"Initializing AdMob Interstitial");
    char* _adMobInterstitialId = ApplicationGetAdMobInterstitialID();
    if (strlen(_adMobInterstitialId)>0)
    {
        adMobinterstitialID = [NSString stringWithFormat:@"%s", _adMobInterstitialId];
        NSLog(@"Got ID %@", adMobinterstitialID);

        self->adMobinterstitial = [self createAndLoadAdMobInterstitial];
    
        hasAdMobInterstitial = true;
    }
    else
        hasAdMobInterstitial = false;
        
    
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver: self selector: @selector(deviceOrientationDidChange:) name: UIDeviceOrientationDidChangeNotification object: nil];

    glView.frame = screen;
    currentOrientation = getCurrentOrientation();

}

- (void)interstitialDidDismissScreen:(GADInterstitial *)interstitial {
    self->adMobinterstitial = [self createAndLoadAdMobInterstitial];
}

// UIAccelerometerDelegate method, called when the device accelerates.
-(void)accelerometer:(UIAccelerometer *)accelerometer didAccelerate:(UIAcceleration *)acceleration
{
	[filter addAcceleration:acceleration];
	float x = filter.x;
 	float y = filter.y;
	float z = filter.z;
	ApplicationOnAccelerometer(x,y,z);
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
	// Detect touch anywhere
	UITouch *touch = [touches anyObject];
	CGPoint touchLocation = [touch locationInView:openglView];

	touchX = touchLocation.x;
	touchY = touchLocation.y;

    //CGRect screen = [[UIScreen mainScreen] applicationFrame];
    //NSLog(@"PX:%i PY:%i", touchX, touchY);
    //NSLog(@"WX:%f HY:%f", screen.size.width, screen.size.height);

	ApplicationBeginTouch(touchX, touchY);
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent*)event 
{
    UITouch *touch = [touches anyObject];
	CGPoint touchLocation = [touch locationInView:openglView];
    //NSLog(@"%f",touchLocation.x);

	ApplicationMoveTouch(touchLocation.x, touchLocation.y);
}


- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent*)event 
{
    UITouch *touch = [touches anyObject];
	CGPoint touchLocation = [touch locationInView:openglView];
	
	ApplicationEndTouch(touchLocation.x, touchLocation.y);
}

- (void)locationManager:(CLLocationManager *)manager didUpdateHeading:(CLHeading *) newHeading
{
	float angle =  -newHeading.trueHeading * M_PI / 180.0f;		
	NSLog(@"Compass heading: %f", angle);	
	
	ApplicationOnCompass(angle, 0, 0); // TODO
}

- (UIInterfaceOrientation)preferredInterfaceOrientationForPresentation
{
    //return [[UIDevice currentDevice] orientation];
   return UIInterfaceOrientationLandscapeRight;

}

- (BOOL)shouldAutorotateToInterfaceOrientation
{
    return NO;
}

- (NSUInteger) supportedInterfaceOrientations
{
    //Because your app is only landscape, your view controller for the view in your
    // popover needs to support only landscape
    return UIInterfaceOrientationMaskLandscape;
}

 - (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)orientation
 {
 /*if (orientation == UIInterfaceOrientationLandscapeRight)
 return YES;
 
 return NO;*/
     return NO;
 }

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration
{
     NSLog(@"Rotating to %i", toInterfaceOrientation);
}

-(BOOL)shouldAutorotate
{
	return NO;
}

-(void)updateOrientation
{
    int orientation = getCurrentOrientation();
    //Ignoring specific orientations
    if (orientation <0 || orientation == currentOrientation)
    {
        return;
    }
    currentOrientation = orientation;
    
    [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(relayoutLayers) object:nil];
    
    NSLog(@"Switching orientation %i", orientation);
    
    ApplicationSetOrientation(orientation);
}

- (void)deviceOrientationDidChange:(NSNotification *)notification {
    [self updateOrientation];
    
  /*   CGRect screen = GetScreenBounds();
    float rotation;
    if (self.interfaceOrientation == UIInterfaceOrientationPortraitUpsideDown)
        rotation = M_PI;
    else if (self.interfaceOrientation == UIInterfaceOrientationLandscapeLeft)
        rotation = M_PI / 2.0;
    else if (self.interfaceOrientation == UIInterfaceOrientationLandscapeRight)
        rotation = M_PI / (- 2.0);
    
    self->glView.center = CGPointMake(screen.size.width/2, screen.size.height/2);
    self->glView.transform = CGAffineTransformMakeRotation(rotation);
    */
   // [self performSelector:@selector(orientationChangedMethod) withObject:nil afterDelay:0];
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
    ApplicationMemoryWarning();
}

-(void)dealloc
{
	[super dealloc];
    
    [glView release];
    [iAPManager release];

    [[UIAccelerometer sharedAccelerometer] setDelegate:nil];
}

@end
