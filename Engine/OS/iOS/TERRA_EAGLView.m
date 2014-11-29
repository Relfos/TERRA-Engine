//
//  EAGLView.m
//
//  Created by Sergio Flores on 3/3/11.

#import <QuartzCore/QuartzCore.h>
#import <OpenGLES/EAGLDrawable.h>

#import "UIKit/UIKit.h"

#include "PascalImports.h"

#import "TERRA_EAGLView.h"
#include "TERRA_Utils.h"
#include "TERRA_AccelerometerFilter.h"

#import "Flurry.h"

#import "FBSettings.h"
#import "FBAppEvents.h"

// A class extension to declare private methods
@interface EAGLView ()

@property (nonatomic, retain) EAGLContext *context;
@property (nonatomic, assign) NSTimer *animationTimer;

@end


@implementation EAGLView

@synthesize context;
@synthesize animationTimer;
@synthesize animationInterval;

bool hasFlurry = false;
bool hasShaders = false;

// You must implement this method
+ (Class)layerClass {
    return [CAEAGLLayer class];
}
	
//The GL view is stored in the nib file. When it's unarchived it's sent -initWithCoder:
- (id)initWithCoder:(NSCoder*)coder 
{

    started = false;
    
    if ((self = [super initWithCoder:coder])) {
        // Get the layer
        CAEAGLLayer *eaglLayer = (CAEAGLLayer *)self.layer;
        
        eaglLayer.opaque = YES;
        eaglLayer.drawableProperties = [NSDictionary dictionaryWithObjectsAndKeys:
                                        [NSNumber numberWithBool:NO], kEAGLDrawablePropertyRetainedBacking, kEAGLColorFormatRGBA8, kEAGLDrawablePropertyColorFormat, nil];
        
        hasShaders = true;
        context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
        
        if (!context || ![EAGLContext setCurrentContext:context]) 
		{
            NSLog(@"Falling back to OpenGLES1");
            hasShaders = false;
            context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];
            [EAGLContext setCurrentContext:context];
        }
        
        animationInterval = 1.0 / 60.0;
    }
	
    return self;
}

bool shadersAvailable()
{
    return hasShaders;
}

EAGLView *_myView;

void SetRenderbufferStorage()
{
    [_myView setRenderbufferStorage];
}

void PresentRenderBuffer()
{	
    [_myView presentRenderBuffer];
}

- (void) setRenderbufferStorage
{
	NSLog(@"Setting renderbuffer storage from drawable...");
	[context renderbufferStorage:GL_RENDERBUFFER fromDrawable:(CAEAGLLayer*)self.layer];
}

- (void) presentRenderBuffer
{
	/*if (hasMsaa)
	{
		GLenum attachments[] = {GL_DEPTH_ATTACHMENT_OES, GL_STENCIL_ATTACHMENT_OES};
		glDiscardFramebufferEXT(GL_READ_FRAMEBUFFER_APPLE, 2, attachments);
        
		glBindFramebufferOES(GL_READ_FRAMEBUFFER_APPLE, msaaFramebuffer);
		glBindFramebufferOES(GL_DRAW_FRAMEBUFFER_APPLE, viewFramebuffer);
        
		glResolveMultisampleFramebufferAPPLE();
	}
    glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);*/
    [context presentRenderbuffer:GL_RENDERBUFFER_OES];
}

- (void)drawView {
    
	_myView = self;
    
    if (!started)
    {       
		char buf[1024];

		CGRect screen = [[UIScreen mainScreen] applicationFrame];
		ApplicationSetScreenRegion(screen.size.width, screen.size.height);

        NSString *countryCode = [[NSLocale currentLocale] objectForKey:NSLocaleCountryCode];
		strcpy(buf, [countryCode UTF8String]);	
		ApplicationSetCountry(buf); 

		strcpy(buf, [[[NSLocale preferredLanguages] objectAtIndex:0] UTF8String]);	
		ApplicationSetLanguage(buf); 

		strcpy(buf, [[[NSBundle mainBundle] resourcePath] UTF8String]);
		ApplicationSetResourcesPath(buf); 
		
		NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
		NSString *documentsDirectory = [paths objectAtIndex:0];
		strcpy(buf, [documentsDirectory UTF8String]);
		ApplicationDocumentPath(buf); 
		
		paths = NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES);
		NSString *tempDirectory = [paths objectAtIndex:0];
		strcpy(buf, [tempDirectory UTF8String]);
		ApplicationTempPath(buf);  

        /*
		// clear bad files from cache
		NSFileManager *fileMgr = [NSFileManager defaultManager];
		NSArray *fileArray = [fileMgr contentsOfDirectoryAtPath:tempDirectory error:nil];
		for (NSString *filename in fileArray)  
		{
			NSString *URL = [tempDirectory stringByAppendingPathComponent:filename];
            NSError *attributesError = nil;
			NSDictionary *fileAttributes = [[NSFileManager defaultManager] attributesOfItemAtPath:URL error:&attributesError];
			NSNumber *fileSizeNumber = [fileAttributes objectForKey:NSFileSize];
			long fileSize = [fileSizeNumber longLongValue];		
			NSLog(@"Found cache file: %@",URL);
			NSLog(@"Size: %i",fileSize);
			if (fileSize<=0) 
			{
				[fileMgr removeItemAtPath:URL error:NULL];
			}
		}	*/	

        ApplicationInit();

        /*char *s = ApplicationGetAppID();
        if (strlen(s)>0)// call the Appirater class
            [Appirater appLaunched];
        */
        
        char* _fbid = ApplicationGetFacebookID();
        if (strlen(_fbid)>0 && isSocialFrameworkAvailable())
        {
            NSString* myid = [NSString stringWithFormat:@"%s", _fbid];
            [FBSettings setDefaultAppID:myid];
            [FBAppEvents activateApp];
        }
        
        
        NSLog(@"Initializing flurry analytics");
        char* _id = ApplicationGetFlurryID();
        if (strlen(_id)>0)
        {
            NSString* myid = [NSString stringWithFormat:@"%s", _id];
            NSLog(@"Got ID %@",myid);
            [Flurry startSession:myid];
            hasFlurry = true;
        }
        else
            hasFlurry = false;

		currentOrientation = [[UIDevice currentDevice] orientation];
			
		[[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
		//[[NSNotificationCenter defaultCenter] addObserver: self selector: @selector(deviceOrientationDidChange:) name: UIDeviceOrientationDidChangeNotification object: nil];			
		
        started = true;
    }
    else
	{
		ApplicationUpdate();
	}
}

void AnalyticsLog(char *s)
{
    if (!hasFlurry) return;
    NSString *a = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    NSLog(@"Sending analytics event: %s", s);
    [Flurry logEvent:a];
}

void AnalyticsLogWithParams(char *s, char *s2)
{
    if (!hasFlurry) return;
    NSString *a = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    NSString *b = [NSString stringWithCString:s2 encoding:NSASCIIStringEncoding];
    NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys:b, a, nil];
    NSLog(@"Sending analytics event: %s - %s", s, s2);
    [Flurry logEvent:a withParameters:dictionary];
}

- (void)layoutSubviews 
{
    [self drawView];
}

void initScale()
{
	//if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad)
    [_myView initScale];
}

- (void) initScale {
    
    [EAGLContext setCurrentContext:context];
    
	NSLog(@"Rescaling screen...");
	
    // Set the scale factor to be the same as the main screen
    if ([self respondsToSelector: NSSelectorFromString(@"contentScaleFactor")]) 
    {
        screenscale = [[UIScreen mainScreen] scale];
        [self setContentScaleFactor:screenscale];
        ApplicationSetScale(screenscale);
    }
    else
    {
        screenscale = 1.0;
    }
}


- (void)startAnimation {
    self.animationTimer = [NSTimer scheduledTimerWithTimeInterval:animationInterval target:self selector:@selector(drawView) userInfo:nil repeats:YES];
}


- (void)stopAnimation {
    self.animationTimer = nil;
}


- (void)setAnimationTimer:(NSTimer *)newTimer {
    [animationTimer invalidate];
    animationTimer = newTimer;
}


- (void)setAnimationInterval:(NSTimeInterval)interval {
    
    animationInterval = interval;
    if (animationTimer) {
        [self stopAnimation];
        [self startAnimation];
    }
}

 - (BOOL)prefersStatusBarHidden 
{
	return YES;
}


 
- (void)deviceOrientationDidChange:(NSNotification *)notification {
  //Obtaining the current device orientation
  UIDeviceOrientation orientation = [[UIDevice currentDevice] orientation];
 
  //Ignoring specific orientations
  if (orientation == UIDeviceOrientationUnknown || currentOrientation == orientation) 
  {
    return;
  }
  
  [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(relayoutLayers) object:nil];
  //Responding only to changes in landscape or portrait
  currentOrientation = orientation;

	NSLog(@"Switching orientation %i", orientation);
  
	switch (orientation)
	{
		case UIDeviceOrientationPortrait:	ApplicationSetOrientation(0); break;
		case UIDeviceOrientationLandscapeLeft:	ApplicationSetOrientation(1); break;
		case UIDeviceOrientationLandscapeRight:	ApplicationSetOrientation(2); break;
		case UIDeviceOrientationPortraitUpsideDown:	ApplicationSetOrientation(3); break;	
	}
  
	[self performSelector:@selector(orientationChangedMethod) withObject:nil afterDelay:0];
}

- (void)dealloc {
    
    [self stopAnimation];
    
    if ([EAGLContext currentContext] == context) {
        [EAGLContext setCurrentContext:nil];
    }
    
    [context release];  
    [super dealloc];
}

@end
