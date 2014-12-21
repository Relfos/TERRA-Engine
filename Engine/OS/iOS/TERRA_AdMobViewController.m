#import "TERRA_AdMobViewController.h"

#import "GADBannerView.h"
#include "GADAdSize.h"
#include "GADRequest.h"

#include "TERRA_Utils.h"
#include "PascalImports.h"

@interface AdMobViewController () <GADBannerViewDelegate>

@end

@implementation AdMobViewController {
    GADBannerView *_bannerView;
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
        _bannerView = [[[GADBannerView alloc]
                          initWithAdSize:kGADAdSizeSmartBannerLandscape] autorelease];
        
        // Need to set this to no since we're creating this custom view.
        _bannerView.translatesAutoresizingMaskIntoConstraints = NO;
        
        char* _id = ApplicationGetAdMobBannerID();
        if (strlen(_id)<=0)
        {
            NSLog(@"AdMob ID not defined!");
            return self;
        }
        NSString* myid = [NSString stringWithFormat:@"%s", _id];
        NSLog(@"Got ID %@",myid);
        
        _contentController = contentController;
        
        _bannerView.adUnitID = myid;
        _bannerView.delegate = self;
    }
    return self;
}

- (void)loadView
{
    CGRect screen = GetScreenBounds();
    UIView *contentView = [[UIView alloc] initWithFrame:screen];
    
    [contentView addSubview:_bannerView];
    
    // Setup containment of the _contentController.
    [self addChildViewController:_contentController];
    
    
    [contentView addSubview:_contentController.view];
    [_contentController didMoveToParentViewController:self];
    
    self.view = contentView;
    
    _contentController.view.frame = screen;
    

    visible = true;
    
    [_bannerView setRootViewController:self];
    [self startRequest];
}


#if __IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_6_0
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return [_contentController shouldAutorotateToInterfaceOrientation:interfaceOrientation];
}
#endif

- (UIInterfaceOrientation)preferredInterfaceOrientationForPresentation
{
    return UIInterfaceOrientationLandscapeLeft;
//    return [_contentController preferredInterfaceOrientationForPresentation];
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
    
    CGRect contentFrame = self.view.bounds;
    
    contentFrame.size.height -= height;
    contentFrame.origin.y = height;
  
    _contentController.view.frame = contentFrame;

   // ApplicationSetViewport(0, 0, [[UIScreen mainScreen]bounds].size.width - height, [[UIScreen mainScreen]bounds].size.height);
    
}

- (void)adView:(GADBannerView *)view
didFailToReceiveAdWithError:(GADRequestError *)error {
    NSLog(@"Failed to receive ad with error: %@", [error localizedFailureReason]);
}

- (void)startRequest {
    NSLog(@"Requesting ad");
    [_bannerView loadRequest:[self createRequest]];
}

- (void)show {
    if (visible)
    {
        return;
    }
    
    visible = true;
    [self.view addSubview:_bannerView];
}

- (void)hide {

    if (!visible)
    {
        return;
    }
    
    visible = false;
    [_bannerView removeFromSuperview];
}


@end
