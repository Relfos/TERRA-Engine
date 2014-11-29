#import <CoreLocation/CoreLocation.h>
#import <CoreMotion/CoreMotion.h>
#import <UIKit/UIKit.h>

#include "PascalImports.h"

#import "TERRA_InputController.h"
#import "TERRA_AccelerometerFilter.h"
#include "TERRA_AppDelegate.h"

@implementation InputController 

bool _hasGyro = false;
bool _hasCompass = false;
InputController *_input = nil;

bool InitAccelerometer()
{
	[_input InitAccelerometer]; 
	return true;
}

bool InitGyroscope()
{
	if (_hasGyro)
	{
		[_input InitGyroscope]; 
		return true;
	}
	else
		return false;
}

bool InitCompass()
{
	if (_hasCompass)
	{
		[_input InitCompass]; 
		return true;
	}
	else
		return false;
}

bool StopAccelerometer()
{
	[_input StopAccelerometer]; 
	return true;
}

bool StopGyroscope()
{
	if (_hasGyro)
	{
		[_input StopGyroscope]; 
		return true;
	}
	else
		return false;
}

bool StopCompass()
{
	if (_hasCompass)
	{
		[_input StopCompass]; 
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
	if([_input->motionManager isGyroActive] == NO)
	{
		_hasGyro = true;
		[_input->motionManager setGyroUpdateInterval:1.0f / updateFrequency];

		// Add on a handler block object 
		// Receive the gyroscope data on this block 
		[_input->motionManager startGyroUpdatesToQueue:[NSOperationQueue mainQueue]
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
	[_input->motionManager stopGyroUpdates];
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
	_input = self;

	motionManager = [[CMMotionManager alloc] init];
	locationManager=[[CLLocationManager alloc] init];
	
	_hasGyro =  ([motionManager isGyroAvailable]);
	_hasCompass = [locationManager headingAvailable];
}

-(void)viewDidUnload
{
	[super viewDidUnload];
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
    return UIInterfaceOrientationMaskLandscapeRight;
}

-(BOOL)shouldAutorotate
{
	return NO;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)orientation
{
   if (orientation == UIInterfaceOrientationLandscapeRight)
      return YES;

   return NO;
}

-(void)dealloc
{
	[super dealloc];
	[[UIAccelerometer sharedAccelerometer] setDelegate:nil];
}

@end
