#import <UIKit/UIKit.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import <CoreVideo/CoreVideo.h>
#import <CoreMedia/CoreMedia.h>

/*!
 @class	AVController 
 @author Benjamin Loulier
 
 @brief    Controller to demonstrate how we can have a direct access to the camera using the iPhone SDK 4
 */
@interface WebcamController : UIViewController <AVCaptureVideoDataOutputSampleBufferDelegate> {
	AVCaptureSession *_captureSession;
	AVCaptureVideoPreviewLayer *_prevLayer;
}

/*!
 @brief	The capture session takes the input from the camera and capture it
 */
@property (nonatomic, retain) AVCaptureSession *captureSession;

/*!
 @brief	This method initializes the capture session
 */
- (void)initCapture;

@end