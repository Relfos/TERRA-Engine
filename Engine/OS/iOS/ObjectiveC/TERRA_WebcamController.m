#import "TERRA_WebcamController.h"


@implementation WebcamController

@synthesize captureSession = _captureSession;

#pragma mark -
#pragma mark Initialization

WebcamController *AV;

- (id)init {
    NSLog(@"IM INITY MY CAM!");
	self = [super init];
	AV = self;
	_captureSession  = nil;
	return self;
}

void startAVCapture()
{
    NSLog(@"STARTAVCAP!!!");

	[AV initCapture];
}

void stopAVCapture()
{
	[AV stopCapture];	
}


- (void)initCapture {
    NSLog(@"CAPTURING VIDEO!!!");

	[self stopCapture];
    

	/*We setup the input*/
	AVCaptureDeviceInput *captureInput = [AVCaptureDeviceInput 
										  deviceInputWithDevice:[AVCaptureDevice defaultDeviceWithMediaType:AVMediaTypeVideo] 
										  error:nil];
	/*We setupt the output*/
	AVCaptureVideoDataOutput *captureOutput = [[AVCaptureVideoDataOutput alloc] init];
	/*While a frame is processes in -captureOutput:didOutputSampleBuffer:fromConnection: delegate methods no other frames are added in the queue.
	 If you don't want this behaviour set the property to NO */
	captureOutput.alwaysDiscardsLateVideoFrames = YES; 
	/*We specify a minimum duration for each frame (play with this settings to avoid having too many frames waiting
	 in the queue because it can cause memory issues). It is similar to the inverse of the maximum framerate.
	 In this example we set a min frame duration of 1/10 seconds so a maximum framerate of 10fps. We say that
	 we are not able to process more than 10 frames per second.*/
	//captureOutput.minFrameDuration = CMTimeMake(1, 10);
	
	/*We create a serial queue to handle the processing of our frames*/
	dispatch_queue_t queue;
	queue = dispatch_queue_create("cameraQueue", NULL);
	[captureOutput setSampleBufferDelegate:self queue:queue];
	//dispatch_release(queue);
	// Set the video output to store frame in BGRA (It is supposed to be faster)
	NSString* key = (NSString*)kCVPixelBufferPixelFormatTypeKey; 
	NSNumber* value = [NSNumber numberWithUnsignedInt:kCVPixelFormatType_32BGRA]; 
	NSDictionary* videoSettings = [NSDictionary dictionaryWithObject:value forKey:key]; 
	[captureOutput setVideoSettings:videoSettings]; 
	/*And we create a capture session*/
	self.captureSession = [[AVCaptureSession alloc] init];
	/*We add input and output*/
	[self.captureSession addInput:captureInput];
	[self.captureSession addOutput:captureOutput];
    /*We use medium quality, ont the iPhone 4 this demo would be laging too much, the conversion in UIImage and CGImage demands too much ressources for a 720p resolution.*/
    [self.captureSession setSessionPreset:AVCaptureSessionPresetMedium];

	/*We start the capture*/
	[self.captureSession startRunning];
	
}
- (void)stopCapture {
	NSLog(@"STOPING WEBCAM!!!");

	if (_captureSession == nil)
		return;
		
	[self.captureSession stopRunning];
	//[self.captureSession release];
	_captureSession  = nil;
}

#pragma mark -
#pragma mark AVCaptureSession delegate
- (void)captureOutput:(AVCaptureOutput *)captureOutput 
didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer 
	   fromConnection:(AVCaptureConnection *)connection 
{ 
    
    NSLog(@"CFRAME!!!");    
	/*We create an autorelease pool because as we are not in the main_queue our code is
	 not executed in the main thread. So we have to create an autorelease pool for the thread we are in*/
	
//	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    CVImageBufferRef imageBuffer = CMSampleBufferGetImageBuffer(sampleBuffer);
    // Lock the image buffer
    CVPixelBufferLockBaseAddress(imageBuffer,0);
    // Get information of the image
    uint8_t *baseAddress = (uint8_t *)CVPixelBufferGetBaseAddressOfPlane(imageBuffer, 0);
    size_t bytesPerRow = CVPixelBufferGetBytesPerRow(imageBuffer);
    size_t width = CVPixelBufferGetWidth(imageBuffer);
    size_t height = CVPixelBufferGetHeight(imageBuffer);
    
    // Create Colorspace
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef newContext = CGBitmapContextCreate(baseAddress, width, height, 8, bytesPerRow, colorSpace, kCGBitmapByteOrder32Little | kCGImageAlphaPremultipliedFirst);
    CGImageRef newImage = CGBitmapContextCreateImage(newContext);
    
    // Release it
    CGContextRelease(newContext);
    CGColorSpaceRelease(colorSpace);
    
    // Copy image data
    CFDataRef dataref = CGDataProviderCopyData(CGImageGetDataProvider(newImage));
    //memcpy(mImageData, dataref, width * height * 4);
	ApplicationOnCamera(width, height, baseAddress);

	
    CFRelease(dataref);
    CGImageRelease(newImage);
    
    // Unlock the image buffer
    CVPixelBufferUnlockBaseAddress(imageBuffer,0);
    // CVBufferRelease(imageBuffer);
	
/*	
    CVImageBufferRef imageBuffer = CMSampleBufferGetImageBuffer(sampleBuffer); 
    //Lock the image buffer
    CVPixelBufferLockBaseAddress(imageBuffer,0); 
    //Get information about the image
    uint8_t *baseAddress = (uint8_t *)CVPixelBufferGetBaseAddress(imageBuffer); 
    size_t bytesPerRow = CVPixelBufferGetBytesPerRow(imageBuffer); 
    size_t width = CVPixelBufferGetWidth(imageBuffer); 
    size_t height = CVPixelBufferGetHeight(imageBuffer);  
    
    //Create a CGImageRef from the CVImageBufferRef
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB(); 
    CGContextRef newContext = CGBitmapContextCreate(baseAddress, width, height, 8, bytesPerRow, colorSpace, kCGBitmapByteOrder32Little | kCGImageAlphaPremultipliedFirst);
    CGImageRef newImage = CGBitmapContextCreateImage(newContext); 
	
    //We release some components
    CGContextRelease(newContext); 
    CGColorSpaceRelease(colorSpace);
    
    //We display the result on the custom layer. All the display stuff must be done in the main thread because
	 //UIKit is no thread safe, and as we are not in the main thread (remember we didn't use the main_queue)
	 //we use performSelectorOnMainThread to call our CALayer and tell it to display the CGImage.
	[self.customLayer performSelectorOnMainThread:@selector(setContents:) withObject: (id) newImage waitUntilDone:YES];
	
	//We display the result on the image view (We need to change the orientation of the image so that the video is displayed correctly).
	// Same thing as for the CALayer we are not in the main thread so ...
	UIImage *image= [UIImage imageWithCGImage:newImage scale:1.0 orientation:UIImageOrientationRight];
	
	//We relase the CGImageRef
	CGImageRelease(newImage);
	
	//We unlock the  image buffer
	CVPixelBufferUnlockBaseAddress(imageBuffer,0);
	*/
	
//	[pool drain];
} 

#pragma mark -
#pragma mark Memory management

- (void)viewDidUnload {
    [super viewDidUnload];
	//self.customLayer = nil;
}

- (void)dealloc {
	[self stopCapture];
  //  [super dealloc];
}


@end