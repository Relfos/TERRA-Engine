#import "TERRA_iAdViewController.h"
#import "TERRA_EngineDelegate.h"

#include "TERRA_Utils.h"
#include "PascalImports.h"



NSString * const BannerViewActionWillBegin = @"BannerViewActionWillBegin";
NSString * const BannerViewActionDidFinish = @"BannerViewActionDidFinish";


@implementation iAdViewController {
    ADBannerView *_bannerView;
    UIViewController *_contentController;
}

- (instancetype)initWithContentViewController:(UIViewController *)contentController
{
    // If contentController is nil, -loadView is going to throw an exception when it attempts to setup
    // containment of a nil view controller.  Instead, throw the exception here and make it obvious
    // what is wrong.
    NSAssert(contentController != nil, @"Attempting to initialize a BannerViewController with a nil contentController.");
    
    self = [super init];
    if (self != nil) {
        // On iOS 6 ADBannerView introduces a new initializer, use it when available.
        if ([ADBannerView instancesRespondToSelector:@selector(initWithAdType:)]) {
            _bannerView = [[ADBannerView alloc] initWithAdType:ADAdTypeBanner];
        } else {
            _bannerView = [[ADBannerView alloc] init];
        }
        _contentController = contentController;
        _bannerView.delegate = self;
    }
    return self;
}

- (void)loadView
{
    CGRect screen = GetScreenBounds();
    // CGRect screen =[UIScreen mainScreen] bounds];

    UIView *contentView = [[UIView alloc] initWithFrame:screen];
    
    [contentView addSubview:_bannerView];
    
    // Setup containment of the _contentController.
    [self addChildViewController:_contentController];
    [contentView addSubview:_contentController.view];
    [_contentController didMoveToParentViewController:self];
    
    self.view = contentView;
    
    _contentController.view.frame = screen;
    
    return;
}


#if __IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_6_0
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return [_contentController shouldAutorotateToInterfaceOrientation:interfaceOrientation];
    //return YES;
}
#endif

- (UIInterfaceOrientation)preferredInterfaceOrientationForPresentation
{
    return UIInterfaceOrientationLandscapeLeft;
   //return [_contentController preferredInterfaceOrientationForPresentation];
}

- (NSUInteger)supportedInterfaceOrientations
{
   return [_contentController supportedInterfaceOrientations];
   // return  UIInterfaceOrientationMaskAll;
}

- (void)viewDidLayoutSubviews
{
    // This method will be called whenever we receive a delegate callback
    // from the banner view.
    // (See the comments in -bannerViewDidLoadAd: and -bannerView:didFailToReceiveAdWithError:)
    
    CGRect contentFrame = self.view.bounds, bannerFrame = CGRectZero;
#if __IPHONE_OS_VERSION_MIN_REQUIRED < __IPHONE_6_0
    // If configured to support iOS <6.0, then we need to set the currentContentSizeIdentifier in order to resize the banner properly.
    // This continues to work on iOS 6.0, so we won't need to do anything further to resize the banner.
    if (contentFrame.size.width < contentFrame.size.height) {
        _bannerView.currentContentSizeIdentifier = ADBannerContentSizeIdentifierPortrait;
    } else {
        _bannerView.currentContentSizeIdentifier = ADBannerContentSizeIdentifierLandscape;
    }
    bannerFrame = _bannerView.frame;
#else
    // If configured to support iOS >= 6.0 only, then we want to avoid currentContentSizeIdentifier as it is deprecated.
    // Fortunately all we need to do is ask the banner for a size that fits into the layout area we are using.
    // At this point in this method contentFrame=self.view.bounds, so we'll use that size for the layout.
    bannerFrame.size = [_bannerView sizeThatFits:contentFrame.size];
#endif

    CGRect screen = GetScreenBounds();

    // Check if the banner has an ad loaded and ready for display.  Move the banner off
    // screen if it does not have an ad.
    if (_bannerView.bannerLoaded) {
        
        //EngineDelegate* inst = [EngineDelegate getInstance];
        //if (inst!=NULL){            [inst hideAdMobView];}

        NSLog(@"iad status: loaded");

    //    ApplicationSetViewport(0, bannerFrame.size.height, screen.size.width, screen.size.height);

//                ApplicationSetViewport(0, 0, screen.size.width - bannerFrame.size.height, screen.size.height);
//        ApplicationSetViewport(0, 0, screen.size.width ,screen.size.height);

        
       contentFrame.size.height -= bannerFrame.size.height;
//        contentFrame.size.width -= bannerFrame.size.width;

     contentFrame.origin.y = bannerFrame.size.height;
     //   contentFrame.origin.x = bannerFrame.size.width;
    
        bannerFrame.origin.y = 0;
        //bannerFrame.origin.x = 0;
    } else {

        NSLog(@"iad status: failed");
        contentFrame.origin.y = 0;
        bannerFrame.origin.y = contentFrame.size.height;
        
    //   ApplicationSetViewport(0, 0, screen.size.width - 25,screen.size.height);
        
     //   ApplicationSetViewport(0, 0, screen.size.width,screen.size.height);
        
//        EngineDelegate* inst = [EngineDelegate getInstance]; if (inst!=NULL){[inst showAdMobView];}
        
    }

    _contentController.view.frame = contentFrame;
    _bannerView.frame = bannerFrame;
}

- (void)bannerViewDidLoadAd:(ADBannerView *)banner
{
    [UIView animateWithDuration:0.25 animations:^{
        // -viewDidLayoutSubviews will handle positioning the banner such that it is either visible
        // or hidden depending upon whether its bannerLoaded property is YES or NO (It will be
        // YES if -bannerViewDidLoadAd: was last called).  We just need our view
        // to (re)lay itself out so -viewDidLayoutSubviews will be called.
        // You must not call [self.view layoutSubviews] directly.  However, you can flag the view
        // as requiring layout...
        [self.view setNeedsLayout];
        // ...then ask it to lay itself out immediately if it is flagged as requiring layout...
        [self.view layoutIfNeeded];
        // ...which has the same effect.
    }];
}

- (void)bannerView:(ADBannerView *)banner didFailToReceiveAdWithError:(NSError *)error
{
    [UIView animateWithDuration:0.25 animations:^{
        // -viewDidLayoutSubviews will handle positioning the banner such that it is either visible
        // or hidden depending upon whether its bannerLoaded property is YES or NO (It will be
        // NO if -bannerView:didFailToReceiveAdWithError: was last called).  We just need our view
        // to (re)lay itself out so -viewDidLayoutSubviews will be called.
        // You must not call [self.view layoutSubviews] directly.  However, you can flag the view
        // as requiring layout...
        [self.view setNeedsLayout];
        // ...then ask it to lay itself out immediately if it is flagged as requiring layout...
        [self.view layoutIfNeeded];
        // ...which has the same effect.
    }];
}

- (BOOL)bannerViewActionShouldBegin:(ADBannerView *)banner willLeaveApplication:(BOOL)willLeave
{
    [[NSNotificationCenter defaultCenter] postNotificationName:BannerViewActionWillBegin object:self];
    return YES;
}

- (void)bannerViewActionDidFinish:(ADBannerView *)banner
{
    [[NSNotificationCenter defaultCenter] postNotificationName:BannerViewActionDidFinish object:self];
}

@end
