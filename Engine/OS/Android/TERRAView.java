package com.pascal.terra;

import android.content.Context;
import android.graphics.PixelFormat;
//import android.opengl.GLSurfaceView;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.Surface;
import android.view.Display;
import android.view.WindowManager;

import android.os.SystemClock;
import android.os.Process;

import java.util.LinkedList;
import java.util.Queue;

import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLContext;
import javax.microedition.khronos.egl.EGLDisplay;
import javax.microedition.khronos.opengles.GL10;

class TERRAView extends GLSurfaceView {
    private static String TAG = "TERRAView";
    private static final boolean DEBUG = false;
	private static boolean initialized = false;
	private static boolean resized = false;
	public static boolean paused = false;
	
	public static boolean terminated = false;

	public static int glWidth;
	public static int glHeight;
	public static boolean hasShaders = false;
	
	public static String purchaseID;
	public static int purchaseCredits = 0;
	public static int purchaseCode = 0;
	
	private static int APIID = 0;
    private static int APICode = 0;

	public static boolean hasAccelEvent = false; 
	public static float accelX;
	public static float accelY;
	public static float accelZ;

	public static boolean hasGyroEvent = false; 
	public static float gyroX;
	public static float gyroY;
	public static float gyroZ;

	public static boolean hasCompassEvent = false; 
	public static float compassHeading;
	public static float compassPitch;
	public static float compassRoll;
	
	public static int stateChange = 0;
	private static int currentOrientation = -1;
	private static int targetOrientation = -1;
	public static int baseOrientation = 0;
	public static long rotationTime = 0;
	private static int orientationChange = 0;
	
    private static Queue<TERRAInputEvent> inputQueue = new LinkedList<TERRAInputEvent>();
    private static Object inputLock = new Object();
    
    public static void QueueInputEvent(TERRAInputEvent event) {        
    
        if (terminated) {
            return;
        }
    
        synchronized (inputLock) {
            inputQueue.add(event);
        }
    }
	
	public static boolean isInitialized()
	{
		return initialized;
	}
    
    public static void APIResult(int API, int code)
    {
        APIID = API;
        APICode = code;
    }
	
	public static void setTargetOrientation(int orientation)
	{
		int finalOrientation = orientation + baseOrientation;
		while (finalOrientation>=360)
		{
			finalOrientation -= 360;
		}
										
		switch (finalOrientation) 
		{
			case 0:		finalOrientation = 0; break;
			case 90:	finalOrientation = 2; break;
			case 180:	finalOrientation = 3; break;
			case 270: 	finalOrientation = 1; break;
			default:	finalOrientation = -1;
		}				
					
		if (finalOrientation>=0 && finalOrientation != targetOrientation)
		{
			rotationTime = SystemClock.uptimeMillis();
			targetOrientation = finalOrientation;
			orientationChange = 1;
		}
	}
	
    public TERRAView(Context context) 
	{
        super(context);
        init(16, 8);		
    }

    private void init(int depth, int stencil) 
	{

        /* Setup the context factory for 2.0 rendering.
         * See ContextFactory class definition below
         */
        setEGLContextFactory(new ContextFactory());

        /* We need to choose an EGLConfig that matches the format of
         * our surface exactly. This is going to be done in our
         * custom config chooser. See ConfigChooser class definition
         * below.
         */
        setEGLConfigChooser(new ConfigChooser(5, 6, 5, 0, depth, stencil) );

        /* Set the renderer responsible for frame rendering */
        setRenderer(new Renderer());
		this.setPreserveEGLContextOnPause(true);		
		
		WindowManager windowManager = (WindowManager) TERRAActivity.instance.getSystemService(Context.WINDOW_SERVICE);
		Display display = windowManager.getDefaultDisplay();		 
		switch (display.getOrientation())
		{		
			case Surface.ROTATION_0:	baseOrientation = 0; break;
			case Surface.ROTATION_90:	baseOrientation = 90; break;
			case Surface.ROTATION_180:	baseOrientation = 180; break;
			case Surface.ROTATION_270:	baseOrientation = 270; break;			
		}
    }

    private static class ContextFactory implements GLSurfaceView.EGLContextFactory {
        private static int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
        public EGLContext createContext(EGL10 egl, EGLDisplay display, EGLConfig eglConfig) {
			int glVersion;
			if (hasShaders)
			{
				glVersion = 2;
				Log.w(TAG, "creating OpenGL ES 2.0 context");
			}
			else
			{
				glVersion = 1;
				Log.w(TAG, "creating OpenGL ES 1.0 context");
			}
            checkEglError("Before eglCreateContext", egl);
            int[] attrib_list = {EGL_CONTEXT_CLIENT_VERSION, glVersion, EGL10.EGL_NONE };
            EGLContext context = egl.eglCreateContext(display, eglConfig, EGL10.EGL_NO_CONTEXT, attrib_list);
            checkEglError("After eglCreateContext", egl);
            return context;
        }

        public void destroyContext(EGL10 egl, EGLDisplay display, EGLContext context) {
            egl.eglDestroyContext(display, context);
        }
    }

    private static void checkEglError(String prompt, EGL10 egl) {
        int error;
        while ((error = egl.eglGetError()) != EGL10.EGL_SUCCESS) {
            Log.e(TAG, String.format("%s: EGL error: 0x%x", prompt, error));
        }
    }

	@Override
	public boolean onTouchEvent(MotionEvent event)
	{
		if (initialized) 
		{
			if (event != null)
			{
				float x = event.getX();
				float y = event.getY();
				
				//Log.d("App", "Event Thread ID: "+android.os.Process.myTid());

                switch (event.getAction())
                {
                case MotionEvent.ACTION_MOVE:
                    QueueInputEvent(new TERRAInputEvent((int)x, (int)y, TERRAInputEvent.InputType.TOUCH_MOVE));
                    break;
                    
                case MotionEvent.ACTION_DOWN:
                    QueueInputEvent(new TERRAInputEvent((int)x, (int)y, TERRAInputEvent.InputType.TOUCH_BEGIN));							
                    break;
                    
                case MotionEvent.ACTION_UP:
                    QueueInputEvent(new TERRAInputEvent((int)x, (int)y, TERRAInputEvent.InputType.TOUCH_END));
                    break;
                }
				return true;
			}
		}
		
		return super.onTouchEvent(event);
	}

    private static class ConfigChooser implements GLSurfaceView.EGLConfigChooser {

        public ConfigChooser(int r, int g, int b, int a, int depth, int stencil) {
            mRedSize = r;
            mGreenSize = g;
            mBlueSize = b;
            mAlphaSize = a;
            mDepthSize = depth;
            mStencilSize = stencil;
        }

        /* This EGL config specification is used to specify 2.0 rendering.
         * We use a minimum size of 4 bits for red/green/blue, but will
         * perform actual matching in chooseConfig() below.
         */
        private static int EGL_OPENGL_ES2_BIT = 4;
        private static int[] s_configAttribs1 =
        {
            EGL10.EGL_RED_SIZE, 4,
            EGL10.EGL_GREEN_SIZE, 4,
            EGL10.EGL_BLUE_SIZE, 4,
            EGL10.EGL_NONE
        };
        private static int[] s_configAttribs2 =
        {
            EGL10.EGL_RED_SIZE, 4,
            EGL10.EGL_GREEN_SIZE, 4,
            EGL10.EGL_BLUE_SIZE, 4,
            EGL10.EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
            EGL10.EGL_NONE
        };

        public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display) {

            /* Get the number of minimally matching EGL configurations
             */
            int[] num_config = new int[1];
            egl.eglChooseConfig(display, s_configAttribs2, null, 0, num_config);

            int numConfigs = num_config[0];
			hasShaders = true;

            if (numConfigs <= 0) 
			{
				egl.eglChooseConfig(display, s_configAttribs1, null, 0, num_config);
				hasShaders = false;
				numConfigs = num_config[0];

				if (numConfigs <= 0) 
				{
					throw new IllegalArgumentException("No configs match configSpec");
				}
            }

            /* Allocate then read the array of minimally matching EGL configs
             */
            EGLConfig[] configs = new EGLConfig[numConfigs];
			if (hasShaders)
				egl.eglChooseConfig(display, s_configAttribs2, configs, numConfigs, num_config);
			else
				egl.eglChooseConfig(display, s_configAttribs1, configs, numConfigs, num_config);
			
			/*
            if (DEBUG) {
                 printConfigs(egl, display, configs);
            }*/
            /* Now return the "best" one
             */
            return chooseConfig(egl, display, configs);
        }

        public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display,
                EGLConfig[] configs) {
            for(EGLConfig config : configs) {
                int d = findConfigAttrib(egl, display, config,
                        EGL10.EGL_DEPTH_SIZE, 0);
                int s = findConfigAttrib(egl, display, config,
                        EGL10.EGL_STENCIL_SIZE, 0);

                // We need at least mDepthSize and mStencilSize bits
                if (d < mDepthSize || s < mStencilSize)
                    continue;

                // We want an *exact* match for red/green/blue/alpha
                int r = findConfigAttrib(egl, display, config,
                        EGL10.EGL_RED_SIZE, 0);
                int g = findConfigAttrib(egl, display, config,
                            EGL10.EGL_GREEN_SIZE, 0);
                int b = findConfigAttrib(egl, display, config,
                            EGL10.EGL_BLUE_SIZE, 0);
                int a = findConfigAttrib(egl, display, config,
                        EGL10.EGL_ALPHA_SIZE, 0);

                if (r == mRedSize && g == mGreenSize && b == mBlueSize && a == mAlphaSize)
                    return config;
            }
            return null;
        }

        private int findConfigAttrib(EGL10 egl, EGLDisplay display,
                EGLConfig config, int attribute, int defaultValue) {

            if (egl.eglGetConfigAttrib(display, config, attribute, mValue)) {
                return mValue[0];
            }
            return defaultValue;
        }

        private void printConfigs(EGL10 egl, EGLDisplay display,
            EGLConfig[] configs) {
            int numConfigs = configs.length;
            Log.w(TAG, String.format("%d configurations", numConfigs));
            for (int i = 0; i < numConfigs; i++) {
                Log.w(TAG, String.format("Configuration %d:  \n", i));
                printConfig(egl, display, configs[i]);
            }
        }

        private void printConfig(EGL10 egl, EGLDisplay display,
                EGLConfig config) {
            int[] attributes = {
                    EGL10.EGL_BUFFER_SIZE,
                    EGL10.EGL_ALPHA_SIZE,
                    EGL10.EGL_BLUE_SIZE,
                    EGL10.EGL_GREEN_SIZE,
                    EGL10.EGL_RED_SIZE,
                    EGL10.EGL_DEPTH_SIZE,
                    EGL10.EGL_STENCIL_SIZE,
                    EGL10.EGL_CONFIG_CAVEAT,
                    EGL10.EGL_CONFIG_ID,
                    EGL10.EGL_LEVEL,
                    EGL10.EGL_MAX_PBUFFER_HEIGHT,
                    EGL10.EGL_MAX_PBUFFER_PIXELS,
                    EGL10.EGL_MAX_PBUFFER_WIDTH,
                    EGL10.EGL_NATIVE_RENDERABLE,
                    EGL10.EGL_NATIVE_VISUAL_ID,
                    EGL10.EGL_NATIVE_VISUAL_TYPE,
                    0x3030, // EGL10.EGL_PRESERVED_RESOURCES,
                    EGL10.EGL_SAMPLES,
                    EGL10.EGL_SAMPLE_BUFFERS,
                    EGL10.EGL_SURFACE_TYPE,
                    EGL10.EGL_TRANSPARENT_TYPE,
                    EGL10.EGL_TRANSPARENT_RED_VALUE,
                    EGL10.EGL_TRANSPARENT_GREEN_VALUE,
                    EGL10.EGL_TRANSPARENT_BLUE_VALUE,
                    0x3039, // EGL10.EGL_BIND_TO_TEXTURE_RGB,
                    0x303A, // EGL10.EGL_BIND_TO_TEXTURE_RGBA,
                    0x303B, // EGL10.EGL_MIN_SWAP_INTERVAL,
                    0x303C, // EGL10.EGL_MAX_SWAP_INTERVAL,
                    EGL10.EGL_LUMINANCE_SIZE,
                    EGL10.EGL_ALPHA_MASK_SIZE,
                    EGL10.EGL_COLOR_BUFFER_TYPE,
                    EGL10.EGL_RENDERABLE_TYPE,
                    0x3042 // EGL10.EGL_CONFORMANT
            };
            String[] names = {
                    "EGL_BUFFER_SIZE",
                    "EGL_ALPHA_SIZE",
                    "EGL_BLUE_SIZE",
                    "EGL_GREEN_SIZE",
                    "EGL_RED_SIZE",
                    "EGL_DEPTH_SIZE",
                    "EGL_STENCIL_SIZE",
                    "EGL_CONFIG_CAVEAT",
                    "EGL_CONFIG_ID",
                    "EGL_LEVEL",
                    "EGL_MAX_PBUFFER_HEIGHT",
                    "EGL_MAX_PBUFFER_PIXELS",
                    "EGL_MAX_PBUFFER_WIDTH",
                    "EGL_NATIVE_RENDERABLE",
                    "EGL_NATIVE_VISUAL_ID",
                    "EGL_NATIVE_VISUAL_TYPE",
                    "EGL_PRESERVED_RESOURCES",
                    "EGL_SAMPLES",
                    "EGL_SAMPLE_BUFFERS",
                    "EGL_SURFACE_TYPE",
                    "EGL_TRANSPARENT_TYPE",
                    "EGL_TRANSPARENT_RED_VALUE",
                    "EGL_TRANSPARENT_GREEN_VALUE",
                    "EGL_TRANSPARENT_BLUE_VALUE",
                    "EGL_BIND_TO_TEXTURE_RGB",
                    "EGL_BIND_TO_TEXTURE_RGBA",
                    "EGL_MIN_SWAP_INTERVAL",
                    "EGL_MAX_SWAP_INTERVAL",
                    "EGL_LUMINANCE_SIZE",
                    "EGL_ALPHA_MASK_SIZE",
                    "EGL_COLOR_BUFFER_TYPE",
                    "EGL_RENDERABLE_TYPE",
                    "EGL_CONFORMANT"
            };
            int[] value = new int[1];
            for (int i = 0; i < attributes.length; i++) {
                int attribute = attributes[i];
                String name = names[i];
                if ( egl.eglGetConfigAttrib(display, config, attribute, value)) {
                    Log.w(TAG, String.format("  %s: %d  \n", name, value[0]));
                } else {
                    // Log.w(TAG, String.format("  %s: failed  \n", name));
                    while (egl.eglGetError() != EGL10.EGL_SUCCESS);
                }
            }
        }

        // Subclasses can adjust these values:
        protected int mRedSize;
        protected int mGreenSize;
        protected int mBlueSize;
        protected int mAlphaSize;
        protected int mDepthSize;
        protected int mStencilSize;
        private int[] mValue = new int[1];
    }

    private static class Renderer implements GLSurfaceView.Renderer {
        public void onDrawFrame(GL10 gl) 
		{
			if (paused || terminated)
				return;
				
			synchronized (TERRAActivity.instance)
			{			
				//Log.d("App", "Engine Thread ID: "+android.os.Process.myTid());

                synchronized (inputLock)
                {
                    TERRAInputEvent touch;

                    long currentTime = SystemClock.uptimeMillis();
                    int eventsProcessed = 0;
                    
                    do {
                        touch = inputQueue.poll();
                    
                        if (touch!=null && ((currentTime - touch.time)<2000) ) {
                            eventsProcessed ++;
                            switch (touch.type)
                            {
                                case TOUCH_MOVE:
                                    TERRALibrary.ApplicationTouchMove(touch.x, touch.y);
                                    break;
                                
                                case TOUCH_BEGIN:
                                    TERRALibrary.ApplicationTouchBegin(touch.x, touch.y);
                                    break;
                                
                                case TOUCH_END:
                                    TERRALibrary.ApplicationTouchEnd(touch.x, touch.y);
                                    break;
                                    
                                case KEY_DOWN:
                                    TERRALibrary.ApplicationKeyDown(touch.x);
                                    break;

                                case KEY_UP:
                                    TERRALibrary.ApplicationKeyUp(touch.x);
                                    break;
                                    
                            }
                        }
                        
                    } while (touch!=null && eventsProcessed<8);
                }
				
				//Log.d("App", "Calling On App Update");
				if (!TERRALibrary.ApplicationUpdate())
				{
					Log.d("App", "Engine was terminated!");
					terminated = true;
					TERRAActivity.instance.finish();
					return;
				}
				//Log.d("App", "Called On App Update");
				
				long timeDelta = SystemClock.uptimeMillis() - rotationTime;
				if (targetOrientation!=currentOrientation &&  timeDelta>2000 && orientationChange==1)
				{					
					orientationChange = 2;					
					TERRALibrary.ApplicationSetOrientation(targetOrientation);
				}
									
				if (orientationChange == 2)
				{					
					orientationChange = 0;
					currentOrientation = TERRALibrary.ApplicationGetOrientation();						
				}

				if (hasAccelEvent)
				{
					hasAccelEvent = false;					
					TERRALibrary.ApplicationOnAccelerometer(accelX, accelY, accelZ);
				}

				if (hasGyroEvent)
				{
					hasGyroEvent = false;					
					TERRALibrary.ApplicationOnGyroscope(gyroX, gyroY, gyroZ);
				}

				if (hasCompassEvent)
				{
					hasCompassEvent = false;					
					TERRALibrary.ApplicationOnCompass(compassHeading, compassPitch, compassRoll);
				}
				
				if (stateChange!=0)
				{
					Log.d("App", "Invoking TERRA.OnChangeState");
					TERRALibrary.ApplicationSetState(stateChange);
					Log.d("App", "Finished TERRA.OnChangeState");
					stateChange = 0;
				}

			
				if (APICode!=0)
				{
					Log.d("App", "Invoking TERRA.OnAPIResult");
					TERRALibrary.ApplicationAPIResult(APIID, APICode);
					APICode = 0;
                    APIID = 0;
				}

				if (purchaseCode!=0)
				{
					TERRALibrary.ApplicationIAPError(purchaseCode);
					purchaseCode = 0;
				}
				else
				if (purchaseID!=null) 
				{
					TERRALibrary.ApplicationIAPConfirm(purchaseID);
					purchaseID = null;
				}
				else
				if (purchaseCredits>0)
				{
					TERRALibrary.ApplicationIAPCredits(purchaseCredits);
					purchaseCredits = 0;
				}
				
			}
        }
		
		public void onExit()
		{
			Log.d("App", "Exiting Renderer");
            TERRAUtils.shutdownAnalytics();				
		}

        public void onSurfaceChanged(GL10 gl, int width, int height) {
			Log.d("App", "Called OnSurfaceChanged");
			glWidth = width;
			glHeight = height;
			
			/*if (!TERRAView.resized)
			{
				TERRAView.resized = true;
				TERRALibrary.ApplicationResize(width, height);
			}*/
        }

        public void onSurfaceCreated(GL10 gl, EglConfigInfo config) {
			Log.d("App", "Called OnSurfaceCreated");
            if (!TERRAView.initialized)
			{
				TERRAView.initialized = true;				
			}
			else
			{
				synchronized (TERRAActivity.instance)
				{							
					TERRALibrary.ApplicationContextLost();					
				}
			}
        }
    }
}
