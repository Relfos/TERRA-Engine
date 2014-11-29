#import <UIKit/UIKit.h>
#import <CoreMotion/CMMotionManager.h>
#import <CoreLocation/CLLocationManager.h>

@class AccelerometerFilter;

@interface InputController : UIViewController<UIAccelerometerDelegate>
{
	int touchX;
	int touchY;
	
	AccelerometerFilter *filter;
	CMMotionManager *motionManager;
	CLLocationManager *locationManager;
}

@end