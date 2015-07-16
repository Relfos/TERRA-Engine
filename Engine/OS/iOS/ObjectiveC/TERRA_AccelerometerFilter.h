#import <UIKit/UIKit.h>

// Basic filter object. 
@interface AccelerometerFilter : NSObject
{
	BOOL adaptive;
	UIAccelerationValue x, y, z;
}

// Add a UIAcceleration to the filter.
-(void)addAcceleration:(UIAcceleration*)accel;

@property(nonatomic, readonly) UIAccelerationValue x;
@property(nonatomic, readonly) UIAccelerationValue y;
@property(nonatomic, readonly) UIAccelerationValue z;

@property(nonatomic, getter=isAdaptive) BOOL adaptive;
@property(nonatomic, readonly) NSString *name;

@end

// A filter class to represent a lowpass filter
@interface LowpassFilter : AccelerometerFilter
{
	double filterConstant;
	UIAccelerationValue lastX, lastY, lastZ;
}

-(id)initWithSampleRate:(double)rate cutoffFrequency:(double)freq;

@end

// A filter class to represent a highpass filter.
@interface HighpassFilter : AccelerometerFilter
{
	double filterConstant;
	UIAccelerationValue lastX, lastY, lastZ;
}

-(id)initWithSampleRate:(double)rate cutoffFrequency:(double)freq;

@end