#import "TERRA_AdMobViewController.h"

#import "GADBannerView.h"
#include "GADAdSize.h"
#include "GADRequest.h"
#include "PascalImports.h"

@interface AdMobViewController () <GADBannerViewDelegate>

@end

@implementation AdMobViewController {
    GADBannerView *adBanner;
    UIViewController *_contentController;
    BOOL visible;
}

- (instancetype)initWithContentViewController:(UIViewController *)contentController
{
    // If contentController is nil, -loadView is going to throw an exception when it attempts to setup
    // containment of a nil view controller.  Instead, throw the exception here and make it obvious
    // what is wrong.
    NSAssert(contentController != nil, @"Attempting to initialize a BannerViewController with a nil contentController.");
    
    self = [super init];
    
    if (self != nil)
    {
        adBanner = [[[GADBannerView alloc]
                          initWithAdSize:kGADAdSizeSmartBannerLandscape] autorelease];
        
        // Need to set this to no since we're creating this custom view.
        adBanner.translatesAutoresizingMaskIntoConstraints = NO;
        
        char* _id = ApplicationGetAdMobID();
        if (strlen(_id)<=0)
        {
            NSLog(@"AdMob ID not defined!");
            return self;
        }
        NSString* myid = [NSString stringWithFormat:@"%s", _id];
        NSLog(@"Got ID %@",myid);
        
        _contentController = contentController;
        
        adBanner.adUnitID = myid;
        adBanner.delegate = self;
    }
    return self;
}

- (void)loadView
{
    UIView *contentView = [[UIView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    
    [contentView addSubview:adBanner];
    
    // Setup containment of the _contentController.
    [self addChildViewController:_contentController];
    [contentView addSubview:_contentController.view];
    [_contentController didMoveToParentViewController:self];
    
    self.view = contentView;
    
    visible = true;
    
    [adBanner setRootViewController:self];
}


#if __IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_6_0
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return [_contentController shouldAutorotateToInterfaceOrientation:interfaceOrientation];
}
#endif

- (UIInterfaceOrientation)preferredInterfaceOrientationForPresentation
{
    return [_contentController preferredInterfaceOrientationForPresentation];
}

- (NSUInteger)supportedInterfaceOrientations
{
    return [_contentController supportedInterfaceOrientations];
}



#pragma mark GADRequest generation

// Here we're creating a simple GADRequest and whitelisting the application
// for test ads. You should request test ads during development to avoid
// generating invalid impressions and clicks.
- (GADRequest *)createRequest {
    GADRequest *request = [GADRequest request];
    
    // Make the request for a test ad. Put in an identifier for the simulator as
    // well as any devices you want to receive test ads.
    request.testDevices = [NSArray arrayWithObjects:GAD_SIMULATOR_ID, nil];
    return request;
}

#pragma mark GADBannerViewDelegate callbacks

// We've received an ad successfully.
- (void)adViewDidReceiveAd:(GADBannerView *)adView
{
    NSLog(@"Received ad successfully");
    
    int height = CGSizeFromGADAdSize(kGADAdSizeSmartBannerLandscape).height;
    
    ApplicationSetViewport(0, 0, [[UIScreen mainScreen]bounds].size.width - height, [[UIScreen mainScreen]bounds].size.height);
    
}

- (void)adView:(GADBannerView *)view
didFailToReceiveAdWithError:(GADRequestError *)error {
    NSLog(@"Failed to receive ad with error: %@", [error localizedFailureReason]);
}

- (void)startRequest {
    NSLog(@"Requesting ad");
    [adBanner loadRequest:[self createRequest]];
}

- (void)show {
    if (visible)
    {
        return;
    }
    
    visible = true;
    [self.view addSubview:adBanner];
}

- (void)hide {

    if (!visible)
    {
        return;
    }
    
    visible = false;
    [adBanner removeFromSuperview];
}


@end
