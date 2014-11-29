#import <UIKit/UIKit.h>
#import "GADBannerViewDelegate.h"

@class GADBannerView, GADRequest;

@interface AdMobViewController : UIViewController <GADBannerViewDelegate>

- (instancetype)initWithContentViewController:(UIViewController *)contentController;

- (GADRequest *)createRequest;
- (void)startRequest;
- (void)show;
- (void)hide;

@end
