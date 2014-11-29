#import <UIKit/UIKit.h>
#import <iAd/iAd.h>

extern NSString * const BannerViewActionWillBegin;
extern NSString * const BannerViewActionDidFinish;

@interface iAdViewController : UIViewController<ADBannerViewDelegate>


- (instancetype)initWithContentViewController:(UIViewController *)contentController;

@end
