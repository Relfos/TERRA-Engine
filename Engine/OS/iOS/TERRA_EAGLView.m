//
//  EAGLView.m
//
//  Created by Sergio Flores on 3/3/11.

#import <QuartzCore/QuartzCore.h>
#import <OpenGLES/EAGLDrawable.h>

#import "UIKit/UIKit.h"

#include "PascalImports.h"
#include "TERRA_Utils.h"

#import "TERRA_EAGLView.h"
#import "TERRA_AccelerometerFilter.h"
#import "TERRA_EngineController.h"

#import "Flurry.h"

#import "FBSettings.h"
#import "FBAppEvents.h"



@implementation EAGLView


bool hasShaders = false;

// You must implement this method
+ (Class)layerClass {
    return [CAEAGLLayer class];
}

- (id)initWithFrame:(CGRect)frame {
    
    self = [super initWithFrame:frame];
    if (self) {
       NSLog(@"initialized glView");

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
	[context renderbufferStorage:GL_RENDERBUFFER_OES fromDrawable:(CAEAGLLayer*)self.layer];
    
    NSLog(@"View Width: %f", self.bounds.size.width);
    NSLog(@"View Height: %f", self.bounds.size.height);
    
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
    
   
    
    
   /*  CGRect screen = GetScreenBounds();
    EngineController *controller = [EngineController getInstance];
    controller->glView.autoresizingMask = 0;
    controller->glView.frame = screen;
    controller->glView.transform = CGAffineTransformInvert(controller->glView.superview.transform);
//    controller->glView.center = CGPointMake(screen.size.width/2, screen.size.height/2);
    */
    
	_myView = self;
    
    if (!started)
    {   
        [[EngineController getInstance] startEngine];
        started = true;
    }
    else
	{
		ApplicationUpdate();
	}
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
    
    NSLog(@"Screen Scale: %f", screenscale);
}


- (void)startAnimation {
    [self setAnimationTimer:[NSTimer scheduledTimerWithTimeInterval:animationInterval target:self selector:@selector(drawView) userInfo:nil repeats:YES]];
}


- (void)stopAnimation {
    [self setAnimationTimer: nil];
}


- (void)setAnimationTimer:(NSTimer *)newTimer {
    [animationTimer invalidate];
    self->animationTimer = newTimer;
}


- (void)setAnimationInterval:(float)interval {
    
    if (interval> 0.0f) {
        animationInterval = 1.0f / interval;
    }
    else {
        animationInterval = 0.0f;
    }
    
    if (animationTimer) {
        [self stopAnimation];
        [self startAnimation];
    }
}

 - (BOOL)prefersStatusBarHidden 
{
	return YES;
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
