	

    /*
     * Copyright (C) 2008 The Android Open Source Project
     *
     * Licensed under the Apache License, Version 2.0 (the "License");
     * you may not use this file except in compliance with the License.
     * You may obtain a copy of the License at
     *
     *      http://www.apache.org/licenses/LICENSE-2.0
     *
     * Unless required by applicable law or agreed to in writing, software
     * distributed under the License is distributed on an "AS IS" BASIS,
     * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     * See the License for the specific language governing permissions and
     * limitations under the License.
     */
    // Based on Android-15 Source Code Version (from Android-SDK release) of GLSurfaceView
     
    package com.pascal.terra;
     
    import android.content.Context;
    import android.content.pm.ConfigurationInfo;
    import android.graphics.PixelFormat;
    import android.opengl.GLDebugHelper;
    import android.os.Build;
    import android.util.AttributeSet;
    import android.util.Log;
    import android.view.SurfaceHolder;
    import android.view.SurfaceView;
          
    import java.io.Writer;
    import java.util.ArrayList;
     
    import javax.microedition.khronos.egl.EGL10;
    import javax.microedition.khronos.egl.EGL11;
    import javax.microedition.khronos.egl.EGLConfig;
    import javax.microedition.khronos.egl.EGLContext;
    import javax.microedition.khronos.egl.EGLDisplay;
    import javax.microedition.khronos.egl.EGLSurface;
    import javax.microedition.khronos.opengles.GL;
    import javax.microedition.khronos.opengles.GL10;
     
    /**
     * An implementation of SurfaceView that uses the dedicated surface for
     * displaying OpenGL rendering.
     * <p>
     * A GLSurfaceView provides the following features:
     * <p>
     * <ul>
     * <li>Manages a surface, which is a special piece of memory that can be
     * composited into the Android view system.
     * <li>Manages an EGL display, which enables OpenGL to render into a surface.
     * <li>Accepts a user-provided Renderer object that does the actual rendering.
     * <li>Renders on a dedicated thread to decouple rendering performance from the
     * UI thread.
     * <li>Supports both on-demand and continuous rendering.
     * <li>Optionally wraps, traces, and/or error-checks the renderer's OpenGL
     * calls.
     * </ul>
     * <div class="special reference">
     * <h3>Developer Guides</h3>
     * <p>
     * For more information about how to use OpenGL, read the <a href="{@docRoot}
     * guide/topics/graphics/opengl.html">OpenGL</a> developer guide.
     * </p>
     * </div> <h3>Using GLSurfaceView</h3>
     * <p>
     * Typically you use GLSurfaceView by subclassing it and overriding one or more
     * of the View system input event methods. If your application does not need to
     * override event methods then GLSurfaceView can be used as-is. For the most
     * part GLSurfaceView behavior is customized by calling "set" methods rather
     * than by subclassing. For example, unlike a regular View, drawing is delegated
     * to a separate Renderer object which is registered with the GLSurfaceView
     * using the {@link #setRenderer(Renderer)} call.
     * <p>
     * <h3>Initializing GLSurfaceView</h3> All you have to do to initialize a
     * GLSurfaceView is call {@link #setRenderer(Renderer)}. However, if desired,
     * you can modify the default behavior of GLSurfaceView by calling one or more
     * of these methods before calling setRenderer:
     * <ul>
     * <li>{@link #setDebugFlags(int)}
     * <li>{@link #setEGLConfigChooser(boolean)}
     * <li>{@link #setEGLConfigChooser(EGLConfigChooser)}
     * <li>{@link #setEGLConfigChooserStrict(int, int, int, int, int, int)}
     * <li>{@link #setGLWrapper(GLWrapper)}
     * </ul>
     * <p>
     * <h4>Specifying the android.view.Surface</h4> By default GLSurfaceView will
     * create a PixelFormat.RGB_565 format surface. If a translucent surface is
     * required, call getHolder().setFormat(PixelFormat.TRANSLUCENT). The exact
     * format of a TRANSLUCENT surface is device dependent, but it will be a
     * 32-bit-per-pixel surface with 8 bits per component.
     * <p>
     * <h4>Choosing an EGL Configuration</h4> A given Android device may support
     * multiple EGLConfig rendering configurations. The available configurations may
     * differ in how may channels of data are present, as well as how many bits are
     * allocated to each channel. Therefore, the first thing GLSurfaceView has to do
     * when starting to render is choose what EGLConfig to use.
     * <p>
     * By default GLSurfaceView chooses a EGLConfig that has an RGB_565 pixel
     * format, with at least a 16-bit depth buffer and no stencil.
     * <p>
     * If you would prefer a different EGLConfig you can override the default
     * behavior by calling one of the setEGLConfigChooser methods.
     * <p>
     * It is best to use either 16bit or 32bit (not 24bits) because those two should
     * be supported by all android devices - According to Romain Guy@Google (see
     * google-groups '24 bit color'). Even if device supports 8880, it may run with
     * pixelflinger (Sony xPeria Play) or run slower due to conversions in the
     * hardware (untested hypothesis).
     * <p>
     * <h4>Debug Behavior</h4> You can optionally modify the behavior of
     * GLSurfaceView by calling one or more of the debugging methods
     * {@link #setDebugFlags(int)}, and {@link #setGLWrapper}. These methods may be
     * called before and/or after setRenderer, but typically they are called before
     * setRenderer so that they take effect immediately.
     * <p>
     * <h4>Setting a Renderer</h4> Finally, you must call {@link #setRenderer} to
     * register a {@link Renderer}. The renderer is responsible for doing the actual
     * OpenGL rendering.
     * <p>
     * <h3>Rendering Mode</h3> Once the renderer is set, you can control whether the
     * renderer draws continuously or on-demand by calling {@link #setRenderMode}.
     * The default is continuous rendering.
     * <p>
     * <h3>Activity Life-cycle</h3> A GLSurfaceView must be notified when the
     * activity is paused and resumed. GLSurfaceView clients are required to call
     * {@link #onPause()} when the activity pauses and {@link #onResume()} when the
     * activity resumes. These calls allow GLSurfaceView to pause and resume the
     * rendering thread, and also allow GLSurfaceView to release and recreate the
     * OpenGL display.
     * <p>
     * <h3>Handling events</h3>
     * <p>
     * To handle an event you will typically subclass GLSurfaceView and override the
     * appropriate method, just as you would with any other View. However, when
     * handling the event, you may need to communicate with the Renderer object
     * that's running in the rendering thread. You can do this using any standard
     * Java cross-thread communication mechanism. In addition, one relatively easy
     * way to communicate with your renderer is to call
     * {@link #queueEvent(Runnable)}. For example:
     *
     * <pre class="prettyprint">
     * class MyGLSurfaceView extends GLSurfaceView {
     *
     *     private MyRenderer mMyRenderer;
     *
     *     public void start() {
     *         mMyRenderer = ...;
     *         setRenderer(mMyRenderer);
     *     }
     *
     *     public boolean onKeyDown(int keyCode, KeyEvent event) {
     *         if (keyCode == KeyEvent.KEYCODE_DPAD_CENTER) {
     *             queueEvent(new Runnable() {
     *                 // This method will be called on the rendering
     *                 // thread:
     *                 public void run() {
     *                     mMyRenderer.handleDpadCenter();
     *                 }
     *             });
     *             return true;
     *         }
     *         return super.onKeyDown(keyCode, event);
     *     }
     * }
     *
     *
     *
     * </pre>
     */
    public class GLSurfaceView extends SurfaceView implements SurfaceHolder.Callback {
        private final static String TAG = "GlSurfaceView";
        private static final int ANDROID_SDK_VERSION = Integer.parseInt(Build.VERSION.SDK);
        private final static boolean LOG_PAUSE_RESUME = false;
        private final static boolean LOG_SWAP_TIME = false;
        private final static boolean LOG_ATTACH_DETACH = false;
        private final static boolean LOG_THREADS = false;
        private final static boolean LOG_SURFACE = false;
        private final static boolean LOG_RENDERER = false;
        private final static boolean LOG_RENDERER_DRAW_FRAME = false;
        private final static boolean LOG_EGL = false;
        // Work-around for bug 2263168
        private final static boolean DRAW_TWICE_AFTER_SIZE_CHANGED = true;
        /**
         * The renderer only renders when the surface is created, or when
         * {@link #requestRender} is called.
         *
         * @see #getRenderMode()
         * @see #setRenderMode(int)
         * @see #requestRender()
         */
        public final static int RENDERMODE_WHEN_DIRTY = 0;
        /**
         * The renderer is called continuously to re-render the scene.
         *
         * @see #getRenderMode()
         * @see #setRenderMode(int)
         */
        public final static int RENDERMODE_CONTINUOUSLY = 1;
     
        /**
         * Check glError() after every GL call and throw an exception if glError
         * indicates that an error has occurred. This can be used to help track down
         * which OpenGL ES call is causing an error.
         *
         * @see #getDebugFlags
         * @see #setDebugFlags
         */
        public final static int DEBUG_CHECK_GL_ERROR = 1;
     
        /**
         * Log GL calls to the system log at "verbose" level with tag
         * "GLSurfaceView".
         *
         * @see #getDebugFlags
         * @see #setDebugFlags
         */
        public final static int DEBUG_LOG_GL_CALLS = 2;
     
        private final Context context;
     
        /**
         * Standard View constructor. In order to render something, you must call
         * {@link #setRenderer} to register a renderer.
         */
        public GLSurfaceView(final Context context) {
            super(context);
            this.context = context;
            init();
        }
     
        /**
         * Standard View constructor. In order to render something, you must call
         * {@link #setRenderer} to register a renderer.
         */
        public GLSurfaceView(final Context context, final AttributeSet attrs) {
            super(context, attrs);
            this.context = context;
            init();
        }
     
        private void init() {
            // Install a SurfaceHolder.Callback so we get notified when the
            // underlying surface is created and destroyed
            final SurfaceHolder holder = getHolder();
            holder.addCallback(this);
            if (ANDROID_SDK_VERSION <= 4) {
                // setType is not needed for SDK 2.0 or newer.
                holder.setType(SurfaceHolder.SURFACE_TYPE_GPU);
            } else if (ANDROID_SDK_VERSION <= 8) {
                // setFormat is done by SurfaceView in SDK 2.3 and newer.
                holder.setFormat(PixelFormat.RGB_565);
            }
        }
     
        /**
         * Set the glWrapper. If the glWrapper is not null, its
         * {@link GLWrapper#wrap(GL)} method is called whenever a surface is
         * created. A GLWrapper can be used to wrap the GL object that's passed to
         * the renderer. Wrapping a GL object enables examining and modifying the
         * behavior of the GL calls made by the renderer.
         * <p>
         * Wrapping is typically used for debugging purposes.
         * <p>
         * The default value is null.
         *
         * @param glWrapper the new GLWrapper
         */
        public void setGLWrapper(final GLWrapper glWrapper) {
            mGLWrapper = glWrapper;
        }
     
        /**
         * Set the debug flags to a new value. The value is constructed by
         * OR-together zero or more of the DEBUG_CHECK_* constants. The debug flags
         * take effect whenever a surface is created. The default value is zero.
         *
         * @param debugFlags the new debug flags
         * @see #DEBUG_CHECK_GL_ERROR
         * @see #DEBUG_LOG_GL_CALLS
         */
        public void setDebugFlags(final int debugFlags) {
            mDebugFlags = debugFlags;
        }
     
        /**
         * Get the current value of the debug flags.
         *
         * @return the current value of the debug flags.
         */
        public int getDebugFlags() {
            return mDebugFlags;
        }
     
        /**
         * Control whether the EGL context is preserved when the GLSurfaceView is
         * paused and resumed.
         * <p>
         * If set to true, then the EGL context may be preserved when the
         * GLSurfaceView is paused. Whether the EGL context is actually preserved or
         * not depends upon whether the Android device that the program is running
         * on can support an arbitrary number of EGL contexts or not. Devices that
         * can only support a limited number of EGL contexts must release the EGL
         * context in order to allow multiple applications to share the GPU.
         * <p>
         * If set to false, the EGL context will be released when the GLSurfaceView
         * is paused, and recreated when the GLSurfaceView is resumed.
         * <p>
         * The default is false.
         *
         * @param preserveOnPause preserve the EGL context when paused
         */
        public void setPreserveEGLContextOnPause(final boolean preserveOnPause) {
            mPreserveEGLContextOnPause = preserveOnPause;
        }
     
        /**
         * @return true if the EGL context will be preserved when paused
         */
        public boolean getPreserveEGLContextOnPause() {
            return mPreserveEGLContextOnPause;
        }

		public boolean isOldDevice()
		{
			if (sGLThreadManager!=null)
				return sGLThreadManager.shouldReleaseEGLContextWhenPausing();
			else
				return false;
		}
     
        /**
         * Set the renderer associated with this view. Also starts the thread that
         * will call the renderer, which in turn causes the rendering to start.
         * <p>
         * This method should be called once and only once in the life-cycle of a
         * GLSurfaceView.
         * <p>
         * The following GLSurfaceView methods can only be called <em>before</em>
         * setRenderer is called:
         * <ul>
         * <li>{@link #setEGLConfigChooser(boolean)}
         * <li>{@link #setEGLConfigChooser(EGLConfigChooser)}
         * <li>{@link #setEGLConfigChooserStrict(int, int, int, int, int, int)}
         * </ul>
         * <p>
         * The following GLSurfaceView methods can only be called <em>after</em>
         * setRenderer is called:
         * <ul>
         * <li>{@link #getRenderMode()}
         * <li>{@link #onPause()}
         * <li>{@link #onResume()}
         * <li>{@link #queueEvent(Runnable)}
         * <li>{@link #requestRender()}
         * <li>{@link #setRenderMode(int)}
         * </ul>
         *
         * @param renderer the renderer to use to perform OpenGL drawing.
         */
        public void setRenderer(final Renderer renderer) {
            checkRenderThreadState();
            if (mEGLConfigChooser == null) {
                mEGLConfigChooser = new SimpleComponentEGLConfigChooser(5, 6, 5, 0, true);
            }
            if (mEGLContextFactory == null) {
                mEGLContextFactory = new DefaultContextFactory();
            }
            if (mEGLWindowSurfaceFactory == null) {
                mEGLWindowSurfaceFactory = new DefaultWindowSurfaceFactory();
            }
            mRenderer = renderer;
            mGLThread = new GLThread(renderer);
            mGLThread.start();
        }
     
        /**
         * Install a custom EGLContextFactory.
         * <p>
         * If this method is called, it must be called before
         * {@link #setRenderer(Renderer)} is called.
         * <p>
         * If this method is not called, then by default a context will be created
         * with no shared context and with a null attribute list.
         */
        public void setEGLContextFactory(final EGLContextFactory factory) {
            checkRenderThreadState();
            mEGLContextFactory = factory;
        }
     
        /**
         * Install a custom EGLWindowSurfaceFactory.
         * <p>
         * If this method is called, it must be called before
         * {@link #setRenderer(Renderer)} is called.
         * <p>
         * If this method is not called, then by default a window surface will be
         * created with a null attribute list.
         */
        public void setEGLWindowSurfaceFactory(final EGLWindowSurfaceFactory factory) {
            checkRenderThreadState();
            mEGLWindowSurfaceFactory = factory;
        }
     
        /**
         * Install a custom EGLConfigChooser.
         * <p>
         * If this method is called, it must be called before
         * {@link #setRenderer(Renderer)} is called.
         * <p>
         * If no setEGLConfigChooser method is called, then by default the view will
         * choose an EGLConfig that is compatible with the current
         * android.view.Surface, with a depth buffer depth of at least 16 bits.
         *
         * @param configChooser
         */
        public void setEGLConfigChooser(final EGLConfigChooser configChooser) {
            checkRenderThreadState();
            mEGLConfigChooser = configChooser;
        }
     
        /**
         * Install a config chooser which will choose a config as close to 16-bit
         * rgb as possible, with or without an optional depth buffer as close to
         * 16-bits as possible.
         * <p>
         * The matching alpha value is preferred over matching RGB.
         * <p>
         * If this method is called, it must be called before
         * {@link #setrenderer(renderer)} is called.
         * <p>
         * If no seteglconfigchooser method is called, then by default the view will
         * choose an rgb_565 surface with a depth buffer depth of at least 16 bits.
         *
         * @param needdepth
         */
        public void setEGLConfigChooser(final boolean needDepth) {
            setEGLConfigChooser(new SimpleComponentEGLConfigChooser(5, 6, 5, 0, needDepth));
        }
     
        /**
         * Install a config chooser which will choose a config as close to the
         * requested rgb bit-detph as possible, but at least rgba=4440, with or
         * without an optional depth buffer as close to 16-bits as possible.
         * <p>
         * The matching alpha value is preferred over matching RGB.
         * <p>
         * If this method is called, it must be called before
         * {@link #setrenderer(renderer)} is called.
         * <p>
         * If no seteglconfigchooser method is called, then by default the view will
         * choose an rgb_565 surface with a depth buffer depth of at least 16 bits.
         *
         * @param needdepth
         */
        public void setEGLConfigChooser(final int redSize, final int greenSize, final int blueSize,
                final int alphaSize, final boolean needDepth) {
            setEGLConfigChooser(new SimpleComponentEGLConfigChooser(redSize, greenSize, blueSize,
                    alphaSize, needDepth));
        }
     
        /**
         * Install a config chooser which will choose a config with at least the
         * specified depthSize and stencilSize, and exactly the specified redSize,
         * greenSize, blueSize and alphaSize. If no such config exists, creation of
         * the GL surface will fail with exception.
         * <p>
         * If this method is called, it must be called before
         * {@link #setRenderer(Renderer)} is called.
         * <p>
         * If no setEGLConfigChooser method is called, then by default the view will
         * choose an RGB_565 surface with a depth buffer depth of at least 16 bits.
         */
        public void setEGLConfigChooserStrict(final int redSize, final int greenSize,
                final int blueSize, final int alphaSize, final int depthSize,
                final int stencilSize) {
            setEGLConfigChooser(new ComponentSizeChooser(redSize, greenSize, blueSize, alphaSize,
                    depthSize, stencilSize));
        }
     
        /**
         * Inform the default EGLContextFactory and default EGLConfigChooser which
         * EGLContext client version to pick.
         * <p>
         * Use this method to create an OpenGL ES 2.0-compatible context. Example:
         *
         * <pre class="prettyprint">
         * public MyView(Context context) {
         *     super(context);
         *     setEGLContextClientVersion(2); // Pick an OpenGL ES 2.0 context.
         *     setRenderer(new MyRenderer());
         * }
         * </pre>
         * <p>
         * Note: Activities which require OpenGL ES 2.0 should indicate this by
         * setting @lt;uses-feature android:glEsVersion="0x00020000" /> in the
         * activity's AndroidManifest.xml file.
         * <p>
         * If this method is called, it must be called before
         * {@link #setRenderer(Renderer)} is called.
         * <p>
         * This method only affects the behavior of the default EGLContexFactory and
         * the default EGLConfigChooser. If
         * {@link #setEGLContextFactory(EGLContextFactory)} has been called, then
         * the supplied EGLContextFactory is responsible for creating an OpenGL ES
         * 2.0-compatible context. If {@link #setEGLConfigChooser(EGLConfigChooser)}
         * has been called, then the supplied EGLConfigChooser is responsible for
         * choosing an OpenGL ES 2.0-compatible config.
         *
         * @param version The EGLContext client version to choose. Use 2 for OpenGL
         *            ES 2.0
         */
        public void setEGLContextClientVersion(final int version) {
            checkRenderThreadState();
            mEGLContextClientVersion = version;
        }
     
        /**
         * Set the rendering mode. When renderMode is RENDERMODE_CONTINUOUSLY, the
         * renderer is called repeatedly to re-render the scene. When renderMode is
         * RENDERMODE_WHEN_DIRTY, the renderer only rendered when the surface is
         * created, or when {@link #requestRender} is called. Defaults to
         * RENDERMODE_CONTINUOUSLY.
         * <p>
         * Using RENDERMODE_WHEN_DIRTY can improve battery life and overall system
         * performance by allowing the GPU and CPU to idle when the view does not
         * need to be updated.
         * <p>
         * This method can only be called after {@link #setRenderer(Renderer)}
         *
         * @param renderMode one of the RENDERMODE_X constants
         * @see #RENDERMODE_CONTINUOUSLY
         * @see #RENDERMODE_WHEN_DIRTY
         */
        public void setRenderMode(final int renderMode) {
            mGLThread.setRenderMode(renderMode);
        }
     
        /**
         * Get the current rendering mode. May be called from any thread. Must not
         * be called before a renderer has been set.
         *
         * @return the current rendering mode.
         * @see #RENDERMODE_CONTINUOUSLY
         * @see #RENDERMODE_WHEN_DIRTY
         */
        public int getRenderMode() {
            return mGLThread.getRenderMode();
        }
     
        /**
         * Request that the renderer render a frame. This method is typically used
         * when the render mode has been set to {@link #RENDERMODE_WHEN_DIRTY}, so
         * that frames are only rendered on demand. May be called from any thread.
         * Must not be called before a renderer has been set.
         */
        public void requestRender() {
            mGLThread.requestRender();
        }
     
        /**
         * This method is part of the SurfaceHolder.Callback interface, and is not
         * normally called or subclassed by clients of GLSurfaceView.
         */
        @Override
        public void surfaceCreated(final SurfaceHolder holder) {
            mGLThread.surfaceCreated();
        }
     
        /**
         * This method is part of the SurfaceHolder.Callback interface, and is not
         * normally called or subclassed by clients of GLSurfaceView.
         */
        @Override
        public void surfaceDestroyed(final SurfaceHolder holder) {
            // Surface will be destroyed when we return
            mGLThread.surfaceDestroyed();
        }
     
        /**
         * This method is part of the SurfaceHolder.Callback interface, and is not
         * normally called or subclassed by clients of GLSurfaceView.
         */
        @Override
        public void surfaceChanged(final SurfaceHolder holder, final int format, final int w,
                final int h) {
            mGLThread.onWindowResize(w, h);
        }
     
        /**
         * Inform the view that the activity is paused. The owner of this view must
         * call this method when the activity is paused. Calling this method will
         * pause the rendering thread. Must not be called before a renderer has been
         * set.
         */
        public void onPause() {
            mGLThread.onPause();
        }
     
        /**
         * Inform the view that the activity is resumed. The owner of this view must
         * call this method when the activity is resumed. Calling this method will
         * recreate the OpenGL display and resume the rendering thread. Must not be
         * called before a renderer has been set.
         */
        public void onResume() {
            mGLThread.onResume();
        }
     
        /**
         * Queue a runnable to be run on the GL rendering thread. This can be used
         * to communicate with the Renderer on the rendering thread. Must not be
         * called before a renderer has been set.
         *
         * @param r the runnable to be run on the GL rendering thread.
         */
        public void queueEvent(final Runnable r) {
            mGLThread.queueEvent(r);
        }
     
        /**
         * Inform the view that the window focus has changed. Added to fix Android <
         * 5 G1 Clipping problem, see comments around if-block at line 1575.
         */
        @Override
        public void onWindowFocusChanged(final boolean hasFocus) {
            super.onWindowFocusChanged(hasFocus);
            mGLThread.onWindowFocusChanged(hasFocus);
        }
     
        /**
         * This method is used as part of the View class and is not normally called
         * or subclassed by clients of GLSurfaceView.
         */
        @Override
        protected void onAttachedToWindow() {
            super.onAttachedToWindow();
            if (LOG_ATTACH_DETACH) {
                Log.d(TAG, "onAttachedToWindow reattach =" + mDetached);
            }
            if (mDetached && mRenderer != null) {
                int renderMode = RENDERMODE_CONTINUOUSLY;
                if (mGLThread != null) {
                    renderMode = mGLThread.getRenderMode();
                }
                mGLThread = new GLThread(mRenderer);
                if (renderMode != RENDERMODE_CONTINUOUSLY) {
                    mGLThread.setRenderMode(renderMode);
                }
                mGLThread.start();
            }
            mDetached = false;
        }
     
        /**
         * This method is used as part of the View class and is not normally called
         * or subclassed by clients of GLSurfaceView. Must not be called before a
         * renderer has been set.
         */
        @Override
        protected void onDetachedFromWindow() {
            if (LOG_ATTACH_DETACH) {
                Log.d(TAG, "onDetachedFromWindow");
            }
            if (mGLThread != null) {
                mGLThread.requestExitAndWait();
            }
            mDetached = true;
            super.onDetachedFromWindow();
        }
     
        /**
         * Waits for the GLThread to exit if the thread has been created.
         */
        public void waitForGlThreadExit() {
            if (mGLThread != null)
                mGLThread.waitForExit();
        }
     
        /**
         * Returns true if the GLThread has been started and is still running.
         *
         * @return true if the GLThread has been started and is still running.
         */
        public boolean isAlive() {
            return mGLThread != null && mGLThread.isAlive();
        }
     
        // ----------------------------------------------------------------------
     
        /**
         * An interface used to wrap a GL interface.
         * <p>
         * Typically used for implementing debugging and tracing on top of the
         * default GL interface. You would typically use this by creating your own
         * class that implemented all the GL methods by delegating to another GL
         * instance. Then you could add your own behavior before or after calling
         * the delegate. All the GLWrapper would do was instantiate and return the
         * wrapper GL instance:
         *
         * <pre class="prettyprint">
         * class MyGLWrapper implements GLWrapper {
         *     GL wrap(GL gl) {
         *         return new MyGLImplementation(gl);
         *     }
         *     static class MyGLImplementation implements GL,GL10,GL11,... {
         *         ...
         *     }
         * }
         * </pre>
         *
         * @see #setGLWrapper(GLWrapper)
         */
        public interface GLWrapper {
            /**
             * Wraps a gl interface in another gl interface.
             *
             * @param gl a GL interface that is to be wrapped.
             * @return either the input argument or another GL object that wraps the
             *         input argument.
             */
            GL wrap(GL gl);
        }
     
        /**
         * A generic renderer interface.
         * <p>
         * The renderer is responsible for making OpenGL calls to render a frame.
         * <p>
         * GLSurfaceView clients typically create their own classes that implement
         * this interface, and then call {@link GLSurfaceView#setRenderer} to
         * register the renderer with the GLSurfaceView.
         * <p>
         * <div class="special reference">
         * <h3>Developer Guides</h3>
         * <p>
         * For more information about how to use OpenGL, read the <a
         * href="{@docRoot}guide/topics/graphics/opengl.html">OpenGL</a> developer
         * guide.
         * </p>
         * </div> <h3>Threading</h3> The renderer will be called on a separate
         * thread, so that rendering performance is decoupled from the UI thread.
         * Clients typically need to communicate with the renderer from the UI
         * thread, because that's where input events are received. Clients can
         * communicate using any of the standard Java techniques for cross-thread
         * communication, or they can use the
         * {@link GLSurfaceView#queueEvent(Runnable)} convenience method.
         * <p>
         * <h3>EGL Context Lost</h3> There are situations where the EGL rendering
         * context will be lost. This typically happens when device wakes up after
         * going to sleep. When the EGL context is lost, all OpenGL resources (such
         * as textures) that are associated with that context will be automatically
         * deleted. In order to keep rendering correctly, a renderer must recreate
         * any lost resources that it still needs. The
         * {@link #onSurfaceCreated(GL10, EGLConfig)} method is a convenient place
         * to do this.
         *
         * @see #setRenderer(Renderer)
         */
        public interface Renderer {
            /**
             * Called when the surface is created or recreated.
             * <p>
             * Called when the rendering thread starts and whenever the EGL context
             * is lost. The EGL context will typically be lost when the Android
             * device awakes after going to sleep.
             * <p>
             * Since this method is called at the beginning of rendering, as well as
             * every time the EGL context is lost, this method is a convenient place
             * to put code to create resources that need to be created when the
             * rendering starts, and that need to be recreated when the EGL context
             * is lost. Textures are an example of a resource that you might want to
             * create here.
             * <p>
             * Note that when the EGL context is lost, all OpenGL resources
             * associated with that context will be automatically deleted. You do
             * not need to call the corresponding "glDelete" methods such as
             * glDeleteTextures to manually delete these lost resources.
             * <p>
             *
             * @param gl the GL interface. Use <code>instanceof</code> to test if
             *            the interface supports GL11 or higher interfaces.
             * @param config the EGLConfig of the created surface. Can be used to
             *            create matching pbuffers.
             */
            void onSurfaceCreated(GL10 gl, EglConfigInfo config);
     
            /**
             * Called when the surface changed size.
             * <p>
             * Called after the surface is created and whenever the OpenGL ES
             * surface size changes.
             * <p>
             * Typically you will set your viewport here. If your camera is fixed
             * then you could also set your projection matrix here:
             *
             * <pre class="prettyprint">
             * void onSurfaceChanged(GL10 gl, int width, int height) {
             *     gl.glViewport(0, 0, width, height);
             *     // for a fixed camera, set the projection too
             *     float ratio = (float) width / height;
             *     gl.glMatrixMode(GL10.GL_PROJECTION);
             *     gl.glLoadIdentity();
             *     gl.glFrustumf(-ratio, ratio, -1, 1, 1, 10);
             * }
             * </pre>
             *
             * @param gl the GL interface. Use <code>instanceof</code> to test if
             *            the interface supports GL11 or higher interfaces.
             * @param width
             * @param height
             */
            void onSurfaceChanged(GL10 gl, int width, int height);
     
            /**
             * Called to draw the current frame.
             * <p>
             * This method is responsible for drawing the current frame.
             * <p>
             * The implementation of this method typically looks like this:
             *
             * <pre class="prettyprint">
             * void onDrawFrame(GL10 gl) {
             *     gl.glClear(GL10.GL_COLOR_BUFFER_BIT | GL10.GL_DEPTH_BUFFER_BIT);
             *     // ... other gl calls to render the scene ...
             * }
             * </pre>
             *
             * @param gl the GL interface. Use <code>instanceof</code> to test if
             *            the interface supports GL11 or higher interfaces.
             */
            void onDrawFrame(GL10 gl) throws InterruptedException;
          
            void onExit();
        }
     
        /**
         * An interface for customizing the eglCreateContext and eglDestroyContext
         * calls.
         * <p>
         * This interface must be implemented by clients wishing to call
         * {@link GLSurfaceView#setEGLContextFactory(EGLContextFactory)}
         */
        public interface EGLContextFactory {
            EGLContext createContext(EGL10 egl, EGLDisplay display, EGLConfig eglConfig);
     
            void destroyContext(EGL10 egl, EGLDisplay display, EGLContext context);
        }
     
        private class DefaultContextFactory implements EGLContextFactory {
            private final int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
     
            @Override
            public EGLContext createContext(final EGL10 egl, final EGLDisplay display,
                    final EGLConfig config) {
                final int[] attrib_list = {
                        EGL_CONTEXT_CLIENT_VERSION, mEGLContextClientVersion, EGL10.EGL_NONE
                };
     
                return egl.eglCreateContext(display, config, EGL10.EGL_NO_CONTEXT,
                        mEGLContextClientVersion != 0 ? attrib_list : null);
            }
     
            @Override
            public void destroyContext(final EGL10 egl, final EGLDisplay display,
                    final EGLContext context) {
                if (!egl.eglDestroyContext(display, context)) {
                    Log.e("DefaultContextFactory", "display:" + display + " context: " + context);
                    if (LOG_THREADS) {
                        Log.i("DefaultContextFactory", "tid=" + Thread.currentThread().getId());
                    }
                    throw new RuntimeException("eglDestroyContext failed: "
                            + egl.eglGetError());
                }
            }
        }
     
        /**
         * An interface for customizing the eglCreateWindowSurface and
         * eglDestroySurface calls.
         * <p>
         * This interface must be implemented by clients wishing to call
         * {@link GLSurfaceView#setEGLWindowSurfaceFactory(EGLWindowSurfaceFactory)}
         */
        public interface EGLWindowSurfaceFactory {
            /**
             * @return null if the surface cannot be constructed.
             */
            EGLSurface createWindowSurface(EGL10 egl, EGLDisplay display, EGLConfig config,
                    Object nativeWindow);
     
            void destroySurface(EGL10 egl, EGLDisplay display, EGLSurface surface);
        }
     
        private static class DefaultWindowSurfaceFactory implements EGLWindowSurfaceFactory {
     
            @Override
            public EGLSurface createWindowSurface(final EGL10 egl, final EGLDisplay display,
                    final EGLConfig config, final Object nativeWindow) {
                EGLSurface result = null;
                try {
                    result = egl.eglCreateWindowSurface(display, config, nativeWindow, null);
                } catch (final IllegalArgumentException e) {
                    // This exception indicates that the surface flinger surface
                    // is not valid. This can happen if the surface flinger surface
                    // has
                    // been torn down, but the application has not yet been
                    // notified via SurfaceHolder.Callback.surfaceDestroyed.
                    // In theory the application should be notified first,
                    // but in practice sometimes it is not. See b/4588890
                    Log.e(TAG, "eglCreateWindowSurface", e);
                }
                return result;
            }
     
            @Override
            public void destroySurface(final EGL10 egl, final EGLDisplay display,
                    final EGLSurface surface) {
                egl.eglDestroySurface(display, surface);
            }
        }
     
        /**
         * An interface for choosing an EGLConfig configuration from a list of
         * potential configurations.
         * <p>
         * This interface must be implemented by clients wishing to call
         * {@link GLSurfaceView#setEGLConfigChooser(EGLConfigChooser)}
         */
        public interface EGLConfigChooser {
            /**
             * Choose a configuration from the list. Implementors typically
             * implement this method by calling {@link EGL10#eglChooseConfig} and
             * iterating through the results. Please consult the EGL specification
             * available from The Khronos Group to learn how to call
             * eglChooseConfig.
             *
             * @param egl the EGL10 for the current display.
             * @param display the current display.
             * @return the chosen configuration.
             */
            EGLConfig chooseConfig(EGL10 egl, EGLDisplay display);
        }
     
        private abstract class BaseConfigChooser implements EGLConfigChooser {
            public BaseConfigChooser(final int[] configSpec) {
                mConfigSpec = filterConfigSpec(configSpec);
            }
     
            @Override
            public EGLConfig chooseConfig(final EGL10 egl, final EGLDisplay display) {
                final int[] num_config = new int[1];
                if (!egl.eglChooseConfig(display, mConfigSpec, null, 0, num_config)) {
                    throw new IllegalArgumentException("eglChooseConfig failed");
                }
     
                final int numConfigs = num_config[0];
     
                if (numConfigs <= 0) {
                    throw new IllegalArgumentException("No configs match configSpec");
                }
     
                final EGLConfig[] configs = new EGLConfig[numConfigs];
                if (!egl.eglChooseConfig(display, mConfigSpec, configs, numConfigs, num_config)) {
                    throw new IllegalArgumentException("eglChooseConfig#2 failed");
                }
                final EGLConfig config = chooseConfig(egl, display, configs);
                if (config == null) {
                    throw new IllegalArgumentException("No config chosen");
                }
                return config;
            }
     
            abstract EGLConfig chooseConfig(EGL10 egl, EGLDisplay display, EGLConfig[] configs);
     
            protected int[] mConfigSpec;
     
            private int[] filterConfigSpec(final int[] configSpec) {
                if (mEGLContextClientVersion != 2) {
                    return configSpec;
                }
                /*
                 * We know none of the subclasses define EGL_RENDERABLE_TYPE. And we
                 * know the configSpec is well formed.
                 */
                final int len = configSpec.length;
                final int[] newConfigSpec = new int[len + 2];
                System.arraycopy(configSpec, 0, newConfigSpec, 0, len - 1);
                newConfigSpec[len - 1] = EGL10.EGL_RENDERABLE_TYPE;
                newConfigSpec[len] = 4; /* EGL_OPENGL_ES2_BIT */
                newConfigSpec[len + 1] = EGL10.EGL_NONE;
                return newConfigSpec;
            }
        }
     
        /**
         * Choose a configuration with exactly the specified r,g,b,a sizes, and at
         * least the specified depth and stencil sizes.
         */
        private class ComponentSizeChooser extends BaseConfigChooser {
            public ComponentSizeChooser(final int redSize, final int greenSize, final int blueSize,
                    final int alphaSize, final int depthSize,
                    final int stencilSize) {
                super(new int[] {
                        EGL10.EGL_RED_SIZE, redSize, EGL10.EGL_GREEN_SIZE, greenSize,
                        EGL10.EGL_BLUE_SIZE, blueSize, EGL10.EGL_ALPHA_SIZE,
                        alphaSize, EGL10.EGL_DEPTH_SIZE, depthSize, EGL10.EGL_STENCIL_SIZE,
                        stencilSize, EGL10.EGL_NONE
                });
                mValue = new int[1];
                mRedSize = redSize;
                mGreenSize = greenSize;
                mBlueSize = blueSize;
                mAlphaSize = alphaSize;
                mDepthSize = depthSize;
                mStencilSize = stencilSize;
            }
     
            @Override
            public EGLConfig chooseConfig(final EGL10 egl, final EGLDisplay display,
                    final EGLConfig[] configs) {
                EGLConfig closestConfig = null;
                int closestDistance = Integer.MAX_VALUE;
                if (LOG_EGL)
                    Log.d(TAG, "Searching for Config with rgba=" + mRedSize + "," + mGreenSize + ","
                            + mBlueSize + "," + mAlphaSize + ", depth="
                            + mDepthSize + ",stencil=" + mStencilSize
                            + " (alpha distance weighted 100 as much as rgb distance)");
                for (final EGLConfig config : configs) {
                    // Depth refers to Depth Buffer Size (z-buffer).
                    final int d = findConfigAttrib(egl, display, config, EGL10.EGL_DEPTH_SIZE, 0);
                    final int s = findConfigAttrib(egl, display, config, EGL10.EGL_STENCIL_SIZE, 0);
                    if (d >= mDepthSize && s >= mStencilSize) {
                        final int r = findConfigAttrib(egl, display, config, EGL10.EGL_RED_SIZE, 0);
                        final int g = findConfigAttrib(egl, display, config, EGL10.EGL_GREEN_SIZE, 0);
                        final int b = findConfigAttrib(egl, display, config, EGL10.EGL_BLUE_SIZE, 0);
                        final int a = findConfigAttrib(egl, display, config, EGL10.EGL_ALPHA_SIZE, 0);
                        final int distance = Math.abs(r - mRedSize) + Math.abs(g - mGreenSize)
                                + Math.abs(b - mBlueSize) + Math.abs(a - mAlphaSize);
                        if (LOG_EGL)
                            Log.d(TAG, "Config with distance " + distance + ", rgba=" + r + "," + g
                                    + "," + b + "," + a + ", depth=" + d + ", stencil="
                                    + s);
                        if (distance < closestDistance) {
                            if (LOG_EGL)
                                Log.d(TAG, "Best Config so far: (distance " + distance + "), rgba=" + r
                                        + "," + g + "," + b + "," + a + ", depth=" + d
                                        + ", stencil=" + s);
                            closestDistance = distance;
                            closestConfig = config;
                        }
                    }
                }
                if (ANDROID_SDK_VERSION <= 8 && closestConfig != null) {
                    // setFormat is done by SurfaceView in SDK 2.3 and newer.
                    final int format;
                    final int r = findConfigAttrib(egl, display, closestConfig, EGL10.EGL_RED_SIZE, 0);
                    final int g = findConfigAttrib(egl, display, closestConfig, EGL10.EGL_GREEN_SIZE, 0);
                    final int b = findConfigAttrib(egl, display, closestConfig, EGL10.EGL_BLUE_SIZE, 0);
                    final int a = findConfigAttrib(egl, display, closestConfig, EGL10.EGL_ALPHA_SIZE, 0);
                    if (a == 0) {
                        if (r == 8 && g == 8 && b == 8) {
                            format = PixelFormat.RGB_888;
                        } else {
                            format = PixelFormat.RGB_565;
                        }
                    } else {
                        if (r == 8 && g == 8 && b == 8) {
                            format = PixelFormat.RGBA_8888;
                        } else if (r == 5 && g == 5 && b == 5) {
                            format = PixelFormat.RGBA_5551;
                        } else {
                            format = a > 1 ? PixelFormat.TRANSLUCENT : PixelFormat.TRANSPARENT;
                        }
                    }
                    getHolder().setFormat(format);
                }
                return closestConfig;
            }
     
            private int findConfigAttrib(final EGL10 egl, final EGLDisplay display,
                    final EGLConfig config, final int attribute, final int defaultValue) {
     
                if (egl.eglGetConfigAttrib(display, config, attribute, mValue)) {
                    return mValue[0];
                }
                return defaultValue;
            }
     
            private final int[] mValue;
            // Subclasses can adjust these values:
            protected int mRedSize;
            protected int mGreenSize;
            protected int mBlueSize;
            protected int mAlphaSize;
            protected int mDepthSize;
            protected int mStencilSize;
            protected EglConfigInfo mChosenEglConfigInfo;
        }
     
        /**
         * This class will choose a surface as close to RGB_565 as possible with or
         * without a depth buffer. If no RGB_565 surface is available it will fall
         * back on lower resolution surfaces like 555 or 4444.
         */
        public class SimpleComponentEGLConfigChooser extends ComponentSizeChooser {
            public SimpleComponentEGLConfigChooser(final int r, final int g, final int b, final int a,
                    final boolean withDepthBuffer) {
                super(4, 4, 4, a, withDepthBuffer ? 16 : 0, 0);
                /*
                 * The ComponentSizeChooser.chooseConfig() will choose the config
                 * closest to the values of mRedSize, mGreenSize, mBlueSize,
                 * mALphaSizeAdjust on the time of creation of the surface (which is
                 * later than this config instance is created). The component
                 * chooser chooses amongst the set of available configs on the
                 * device for which bitsizes are equal or larger than those
                 * specified in the super-call. This way we'll accept a 4444 or 555
                 * buffer if there's no 565 buffer available.
                 */
                mRedSize = r;
                mGreenSize = g;
                mBlueSize = b;
            }
        }
     
        /**
         * An EGL helper class.
         */
     
        private class EglHelper {
            public EglHelper() {
     
            }
     
            /**
             * Initialize EGL for a given configuration spec.
             *
             * @param configSpec
             * @throws StopGlThreadException
             */
            public void start() throws StopGlThreadException {
                if (LOG_EGL) {
                    Log.w("EglHelper", "start() tid=" + Thread.currentThread().getId());
                }
                /* Get an EGL instance */
                mEgl = (EGL10) EGLContext.getEGL();
                if (mEgl == null) {
                    throwEglException("egl was null");
                }
                /* Get to the default display. */
                mEglDisplay = mEgl.eglGetDisplay(EGL10.EGL_DEFAULT_DISPLAY);
                if (mEglDisplay == null) {
                    throwEglException("eglDisplay was null");
                }
     
                if (mEglDisplay == EGL10.EGL_NO_DISPLAY) {
                    throwEglException("eglGetDisplay failed");
                }
     
                /* We can now initialize EGL for that display */
                final int[] version = new int[2];
                if (!mEgl.eglInitialize(mEglDisplay, version)) {
                    throwEglException("eglInitialize failed");
                }
                mEglConfig = mEGLConfigChooser.chooseConfig(mEgl, mEglDisplay);
                if (mEglConfig == null) {
                    throwEglException("eglConfig was null");
                }
     
                /*
                 * Create an EGL context. We want to do this as rarely as we can,
                 * because an EGL context is a somewhat heavy object.
                 */
                mEglContext = mEGLContextFactory.createContext(mEgl, mEglDisplay, mEglConfig);
                if (mEglContext == null || mEglContext == EGL10.EGL_NO_CONTEXT) {
                    mEglContext = null;
                    throwEglException("createContext");
                }
                if (LOG_EGL) {
                    Log.w("EglHelper", "createContext " + mEglContext + " tid="
                            + Thread.currentThread().getId());
                }
     
                mEglSurface = null;
            }
     
            /*
             * React to the creation of a new surface by creating and returning an
             * OpenGL interface that renders to that surface.
             */
            public GL createSurface(final SurfaceHolder holder) throws StopGlThreadException {
                if (LOG_EGL) {
                    Log.w("EglHelper", "createSurface()  tid=" + Thread.currentThread().getId());
                }
                /* Check preconditions. */
                if (mEgl == null) {
                    throw new RuntimeException("egl not initialized");
                }
                if (mEglDisplay == null) {
                    throw new RuntimeException("eglDisplay not initialized");
                }
                if (mEglConfig == null) {
                    throw new RuntimeException("mEglConfig not initialized");
                }
                /* The window size has changed, so we need to create a new surface. */
                if (mEglSurface != null && mEglSurface != EGL10.EGL_NO_SURFACE) {
     
                    /* Unbind and destroy the old EGL surface, if there is one. */
                    mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
                            EGL10.EGL_NO_CONTEXT);
                    mEGLWindowSurfaceFactory.destroySurface(mEgl, mEglDisplay, mEglSurface);
                }
     
                /* Create an EGL surface we can render into. */
                mEglSurface = mEGLWindowSurfaceFactory.createWindowSurface(mEgl, mEglDisplay,
                        mEglConfig, holder);
     
                if (mEglSurface == null || mEglSurface == EGL10.EGL_NO_SURFACE) {
                    final int error = mEgl.eglGetError();
                    if (error == EGL10.EGL_BAD_NATIVE_WINDOW) {
                        reportEglException("createWindowSurface returned EGL_BAD_NATIVE_WINDOW.");
                    }
                    return null;
                }
     
                /*
                 * Before we can issue GL commands, we need to make sure the context
                 * is current and bound to a surface.
                 */
                if (!mEgl.eglMakeCurrent(mEglDisplay, mEglSurface, mEglSurface, mEglContext)) {
                    throwEglException("eglMakeCurrent");
                }
     
                GL gl = mEglContext.getGL();
                if (mGLWrapper != null) {
                    gl = mGLWrapper.wrap(gl);
                }
     
                if ((mDebugFlags & (DEBUG_CHECK_GL_ERROR | DEBUG_LOG_GL_CALLS)) != 0) {
                    int configFlags = 0;
                    Writer log = null;
                    if ((mDebugFlags & DEBUG_CHECK_GL_ERROR) != 0) {
                        configFlags |= GLDebugHelper.CONFIG_CHECK_GL_ERROR;
                    }
                    if ((mDebugFlags & DEBUG_LOG_GL_CALLS) != 0) {
                        log = new LogWriter();
                    }
                    gl = GLDebugHelper.wrap(gl, configFlags, log);
                }
                return gl;
            }
     
            public void purgeBuffers() {
                mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
                        EGL10.EGL_NO_CONTEXT);
                mEgl.eglMakeCurrent(mEglDisplay, mEglSurface, mEglSurface, mEglContext);
            }
     
            /**
             * Display the current render surface.
             *
             * @return false if the context has been lost.
             * @throws StopGlThreadException
             */
            public boolean swap() throws StopGlThreadException {
                if (!mEgl.eglSwapBuffers(mEglDisplay, mEglSurface)) {
     
                    /*
                     * Check for EGL_CONTEXT_LOST, which means the context and all
                     * associated data were lost (For instance because the device
                     * went to sleep). We need to sleep until we get a new surface.
                     */
                    final int error = mEgl.eglGetError();
                    switch (error) {
                        case EGL11.EGL_CONTEXT_LOST:
                            return false;
                        case EGL10.EGL_BAD_NATIVE_WINDOW:
                            // The native window is bad, probably because the
                            // window manager has closed it. Ignore this error,
                            // on the expectation that the application will be
                            // closed soon.
                            Log.e("EglHelper", "eglSwapBuffers returned EGL_BAD_NATIVE_WINDOW. tid="
                                    + Thread.currentThread().getId());
                            break;
                        default:
                            throwEglException("eglSwapBuffers", error);
                    }
                }
                return true;
            }
     
            public void destroySurface() {
                if (LOG_EGL) {
                    Log.w("EglHelper", "destroySurface()  tid=" + Thread.currentThread().getId());
                }
                if (mEglSurface != null && mEglSurface != EGL10.EGL_NO_SURFACE) {
                    mEgl.eglMakeCurrent(mEglDisplay, EGL10.EGL_NO_SURFACE, EGL10.EGL_NO_SURFACE,
                            EGL10.EGL_NO_CONTEXT);
                    mEGLWindowSurfaceFactory.destroySurface(mEgl, mEglDisplay, mEglSurface);
                    mEglSurface = null;
                }
            }
     
            public void finish() {
                if (LOG_EGL) {
                    Log.w("EglHelper", "finish() tid=" + Thread.currentThread().getId());
                }
                if (mEglContext != null) {
                    mEGLContextFactory.destroyContext(mEgl, mEglDisplay, mEglContext);
                    mEglContext = null;
                }
                if (mEglDisplay != null) {
                    // Disabled hack - so we always do terminate the display, just
                    // as in original version
     
                    // Removed the teminate display call due to bugreports on
                    // cupcake and donut
                    // with exception: eglMakeCurrent failed: 12294
                    // http://forum.xda-developers.com/archive/index.php/t-644710.html
                    // That was related to Windows mobile gl crashing, so it may
                    // have nothing
                    // to do with our problem, but it solved their no 12294 bug.
                    // if (ANDROID_SDK_VERSION >= 5) {
                    // On Tattoo we must terminate the display or the app sometimes
                    // hang it appears
                    mEgl.eglTerminate(mEglDisplay);
                    // }
                    mEglDisplay = null;
                }
            }
     
            private void throwEglException(final String function) throws StopGlThreadException {
                final String errorCode = mEgl != null ? "" + mEgl.eglGetError() : "(mEgl was null)";
                final boolean handled = handleSurfaceException(context, function, errorCode);
                if (handled) {
                    throw new StopGlThreadException(function + " failed: " + errorCode);
                } else {
                    throw new RuntimeException(function + " failed: " + errorCode);
                }
            }
     
            private void reportEglException(final String errorMsg) {                
                Log.e(TAG, errorMsg);
            }
     
            private void throwEglException(final String function, final int error)
                    throws StopGlThreadException {
                final String errorCode = "" + error;
                final boolean handled = handleSurfaceException(context, function, errorCode);
                if (handled) {
                    throw new StopGlThreadException(function + " failed: " + errorCode);
                } else {
                    throw new RuntimeException(function + " failed: " + errorCode);
                }
            }
     
            EGL10 mEgl;
            EGLDisplay mEglDisplay;
            EGLSurface mEglSurface;
            EGLConfig mEglConfig;
            EGLContext mEglContext;
     
        }
     
        /**
         * A generic GL Thread. Takes care of initializing EGL and GL. Delegates to
         * a Renderer instance to do the actual drawing. Can be configured to render
         * continuously or on request. All potentially blocking synchronization is
         * done through the sGLThreadManager object. This avoids multiple-lock
         * ordering issues.
         */
        private class GLThread extends Thread {
            public GLThread(final Renderer renderer) {
                super();
                mWidth = 0;
                mHeight = 0;
                mRequestRender = true;
                mRenderMode = RENDERMODE_CONTINUOUSLY;
                mRenderer = renderer;
            }
     
            @Override
            public void run() {
                setName("GLThread " + getId());
                if (LOG_THREADS)
                    Log.i(TAG, "GLThread.run(): starting tid=" + getId());
     
                try {
                    guardedRun();
                } catch (final InterruptedException e) {
                    // fall thru and exit normally
                } catch (final StopGlThreadException e) {
                    // fall thru and exit normally
                } finally {
                    sGLThreadManager.threadExiting(this);
                }
                if (LOG_THREADS)
                    Log.i(TAG, "GLThread.run(): GlThread stopped running");
            }
     
            /*
             * This private method should only be called inside a
             * synchronized(sGLThreadManager) block.
             */
            private void stopEglSurfaceLocked() {
                if (mHaveEglSurface) {
                    mHaveEglSurface = false;
                    mEglHelper.destroySurface();
                }
            }
     
            /*
             * This private method should only be called inside a
             * synchronized(sGLThreadManager) block.
             */
            private void stopEglContextLocked() {
                if (mHaveEglContext) {
                    mEglHelper.finish();
                    mHaveEglContext = false;
                    sGLThreadManager.releaseEglContextLocked(this);
                }
            }
     
            private void guardedRun() throws InterruptedException, StopGlThreadException {
                mEglHelper = new EglHelper();
                mHaveEglContext = false;
                mHaveEglSurface = false;
                try {
                    GL10 gl = null;
                    boolean createEglContext = false;
                    boolean createEglSurface = false;
                    boolean lostEglContext = false;
                    boolean sizeChanged = false;
                    boolean wantRenderNotification = false;
                    boolean doRenderNotification = false;
                    boolean askedToReleaseEglContext = false;
                    int w = 0;
                    int h = 0;
                    Runnable event = null;
     
                    while (true) {
                        synchronized (sGLThreadManager) {
                            while (true) {
                                if (mShouldExit) {
                                    return;
                                }
     
                                if (!mEventQueue.isEmpty()) {
                                    event = mEventQueue.remove(0);
                                    break;
                                }
     
                                // Update the pause state.
                                if (mPaused != mRequestPaused) {
                                    mPaused = mRequestPaused;
                                    sGLThreadManager.notifyAll();
                                    if (LOG_PAUSE_RESUME) {
                                        Log.i("GLThread", "mPaused is now " + mPaused + " tid="
                                                + getId());
                                    }
                                }
     
                                // Do we need to give up the EGL context?
                                if (mShouldReleaseEglContext) {
                                    if (LOG_SURFACE) {
                                        Log.i("GLThread", "releasing EGL context because asked to tid="
                                                + getId());
                                    }
                                    stopEglSurfaceLocked();
                                    stopEglContextLocked();
                                    mShouldReleaseEglContext = false;
                                    askedToReleaseEglContext = true;
                                }
     
                                // Have we lost the EGL context?
                                if (lostEglContext) {
                                    stopEglSurfaceLocked();
                                    stopEglContextLocked();
                                    lostEglContext = false;
                                }
     
                                // Do we need to release the EGL surface?
                                if (mHaveEglSurface && mPaused) {
                                    if (LOG_SURFACE)
                                        Log.i(TAG, "releasing EGL surface because paused tid="
                                                + getId());
                                    if (LOG_SURFACE) {
                                        Log.i("GLThread", "releasing EGL surface because paused tid="
                                                + getId());
                                    }
                                    stopEglSurfaceLocked();
                                    if (!mPreserveEGLContextOnPause
                                            || sGLThreadManager.shouldReleaseEGLContextWhenPausing()) {
                                        stopEglContextLocked();
                                        if (LOG_SURFACE) {
                                            Log.i("GLThread",
                                                    "releasing EGL context because paused tid="
                                                            + getId()
                                                            + ", device supports preserve: "
                                                            + !sGLThreadManager
                                                                    .shouldReleaseEGLContextWhenPausing());
                                        }
                                    }
                                    if (sGLThreadManager.shouldTerminateEGLWhenPausing()) {
                                        mEglHelper.finish();
                                        if (LOG_SURFACE) {
                                            Log.i("GLThread", "terminating EGL because paused tid="
                                                    + getId());
                                        }
                                    }
                                }
     
                                // Have we lost the surface view surface?
                                if (!mHasSurface && !mWaitingForSurface) {
                                    if (LOG_SURFACE)
                                        Log.i(TAG,
                                                "GLThread.guardedRun(): noticed surfaceView surface lost tid="
                                                        + getId());
                                    if (mHaveEglSurface) {
                                        stopEglSurfaceLocked();
                                    }
                                    mWaitingForSurface = true;
                                    sGLThreadManager.notifyAll();
                                }
     
                                // Have we acquired the surface view surface?
                                if (mHasSurface && mWaitingForSurface) {
                                    if (LOG_SURFACE)
                                        Log.i(TAG,
                                                "GLThread.guardedRun(): noticed surfaceView surface acquired tid="
                                                        + getId());
                                    mWaitingForSurface = false;
                                    sGLThreadManager.notifyAll();
                                }
     
                                if (doRenderNotification) {
                                    if (LOG_SURFACE) {
                                        Log.i("GLThread", "sending render notification tid=" + getId());
                                    }
                                    wantRenderNotification = false;
                                    doRenderNotification = false;
                                    mRenderComplete = true;
                                    sGLThreadManager.notifyAll();
                                }
     
                                // Ready to draw?
                                if (readyToDraw()) {
     
                                    // If we don't have an EGL context, try to
                                    // acquire one.
                                    if (!mHaveEglContext) {
                                        if (askedToReleaseEglContext) {
                                            askedToReleaseEglContext = false;
                                        } else if (sGLThreadManager.tryAcquireEglContextLocked(this)) {
                                            try {
                                                mEglHelper.start();
                                            } catch (final RuntimeException t) {
                                                sGLThreadManager.releaseEglContextLocked(this);
                                                throw t;
                                            }
                                            mHaveEglContext = true;
                                            createEglContext = true;
     
                                            sGLThreadManager.notifyAll();
                                            if (LOG_SURFACE) {
                                                Log.w("GLThread", "egl create context");
                                            }
                                        }
                                    }
     
                                    if (mHaveEglContext && !mHaveEglSurface) {
                                        mHaveEglSurface = true;
                                        createEglSurface = true;
                                        sizeChanged = true;
                                    }
     
                                    if (mHaveEglSurface) {
                                        if (mSizeChanged) {
                                            sizeChanged = true;
                                            w = mWidth;
                                            h = mHeight;
                                            wantRenderNotification = true;
                                            if (LOG_SURFACE) {
                                                Log.i("GLThread",
                                                        "noticing that we want render notification tid="
                                                                + getId());
                                            }
     
                                            if (DRAW_TWICE_AFTER_SIZE_CHANGED) {
                                                // We keep mRequestRender true so
                                                // that we draw twice after the size
                                                // changes.
                                                // (Once because of mSizeChanged,
                                                // the second time because of
                                                // mRequestRender.)
                                                // This forces the updated graphics
                                                // onto the screen.
                                            } else {
                                                mRequestRender = false;
                                            }
                                            mSizeChanged = false;
                                        } else {
                                            mRequestRender = false;
                                        }
                                        sGLThreadManager.notifyAll();
                                        break;
                                    }
                                }
     
                                // By design, this is the only place in a GLThread
                                // thread where we wait().
                                if (LOG_THREADS)
                                    Log.i(TAG, "GLThread.guardedRun(): waiting tid=" + getId()
                                            + ", rendermode continuous="
                                            + (mRenderMode == RENDERMODE_CONTINUOUSLY));
                                sGLThreadManager.wait();
                            }
                        } // end of synchronized(sGLThreadManager)
     
                        if (event != null) {
                            event.run();
                            event = null;
                            continue;
                        }
     
                        /*
                         * The mHasFocus is applied on two levels. It is used it
                         * readyToDraw to avoid continuous draws if mHasFocus=false,
                         * but to allow one draw even without focus (to get stuff
                         * drawn behind dialogs for instance, and perhaps also on
                         * resume). The mHasFocus is also applied here for small SDK
                         * versions to avoid or reduce incorrect portrait mode
                         * clipping. On G1 clipping will become wrong after screen
                         * has been off if we ommit this mHasFocus check, and on G1
                         * the problem will remain until user has gone to home
                         * screen and back (thus destroying and recreating the
                         * context and surface)
                         */
                        if (mHasFocus || ANDROID_SDK_VERSION >= 5) {
                            if (createEglSurface) {
                                if (LOG_SURFACE) {
                                    Log.w("GLThread", "GLThread.guardedRun(): egl createSurface");
                                }
                                gl = (GL10) mEglHelper.createSurface(getHolder());
                                if (gl == null) {
                                    // Couldn't create a surface. Quit quietly.
                                    break;
                                }
                                sGLThreadManager.checkGLDriver(gl);
                                createEglSurface = false;
                                // Here the previous GLSurfaceView always called
                                // mRenderer.onSurfaceCreated
                            }
     
                            if (createEglContext) {
                                if (LOG_RENDERER) {
                                    Log.w("GLThread", "GLThread.guardedRun(): onSurfaceCreated");
                                }
                                mRenderer.onSurfaceCreated(gl, new EglConfigInfo(mEglHelper));
                                createEglContext = false;
                            }
                            if (sizeChanged) {
                                if (LOG_RENDERER) {
                                    Log.w("GLThread", "GLThread.guardedRun(): onSurfaceChanged(" + w
                                            + ", " + h + ")");
                                }
                                mEglHelper.purgeBuffers();
                                mRenderer.onSurfaceChanged(gl, w, h);
                                sizeChanged = false;
                            }
                            if (LOG_RENDERER_DRAW_FRAME) {
                                Log.w(TAG, "GLThread.guardedRun(): onDrawFrame tid=" + getId());
                            }
                            mRenderer.onDrawFrame(gl);
                            long time;
                            if (LOG_SWAP_TIME)
                                time = System.nanoTime();
                            if (!mEglHelper.swap()) {
                                if (LOG_SURFACE)
                                    Log.i(TAG, "egl surface lost tid=" + getId());
                                lostEglContext = true;
                            }
                        }
                        if (wantRenderNotification) {
                            doRenderNotification = true;
                        }
                    }
                } finally {
                    mRenderer.onExit();
                    /* clean-up everything... */
                    synchronized (sGLThreadManager) {
                        stopEglSurfaceLocked();
                        stopEglContextLocked();
                    }
                }
            }
     
            public boolean ableToDraw() {
                return mHaveEglContext && mHaveEglSurface && readyToDraw();
            }
     
            private boolean readyToDraw() {
                // Added mHasFocus to fix Android < 5 G1 Clipping problem, see
                // comments around if-block at line 1575.
                return !mPaused
                        && mHasSurface
                        && mWidth > 0
                        && mHeight > 0
                        && (mRequestRender || mRenderMode == RENDERMODE_CONTINUOUSLY
                                && (mHasFocus || ANDROID_SDK_VERSION >= 5));
            }
     
            public void setRenderMode(final int renderMode) {
                if (!(RENDERMODE_WHEN_DIRTY <= renderMode && renderMode <= RENDERMODE_CONTINUOUSLY)) {
                    throw new IllegalArgumentException("renderMode");
                }
                synchronized (sGLThreadManager) {
                    mRenderMode = renderMode;
                    sGLThreadManager.notifyAll();
                }
            }
     
            public int getRenderMode() {
                synchronized (sGLThreadManager) {
                    return mRenderMode;
                }
            }
     
            public void requestRender() {
                synchronized (sGLThreadManager) {
                    mRequestRender = true;
                    sGLThreadManager.notifyAll();
                }
            }
     
            public void surfaceCreated() {
                synchronized (sGLThreadManager) {
                    if (LOG_THREADS) {
                        Log.i(TAG, "GLThread.surfaceCreated() tid=" + getId());
                    }
                    mHasSurface = true;
                    sGLThreadManager.notifyAll();
                    while (mWaitingForSurface && !mExited) {
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void surfaceDestroyed() {
                synchronized (sGLThreadManager) {
                    if (LOG_THREADS) {
                        Log.i(TAG, "GLThread.surfaceDestroyed() tid=" + getId());
                    }
                    mHasSurface = false;
                    sGLThreadManager.notifyAll();
                    while (!mWaitingForSurface && !mExited) {
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void onPause() {
                synchronized (sGLThreadManager) {
                    if (LOG_PAUSE_RESUME) {
                        Log.i("GLThread", "onPause tid=" + getId());
                    }
                    mRequestPaused = true;
                    sGLThreadManager.notifyAll();
                    while (!mExited && !mPaused) {
                        if (LOG_PAUSE_RESUME) {
                            Log.i("Main thread", "onPause waiting for mPaused.");
                        }
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException ex) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void onResume() {
                synchronized (sGLThreadManager) {
                    if (LOG_PAUSE_RESUME) {
                        Log.i("GLThread", "onResume tid=" + getId());
                    }
                    mRequestPaused = false;
                    mRequestRender = true;
                    mRenderComplete = false;
                    sGLThreadManager.notifyAll();
                    while (!mExited && mPaused && !mRenderComplete) {
                        if (LOG_PAUSE_RESUME) {
                            Log.i("Main thread", "onResume waiting for !mPaused.");
                        }
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException ex) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void onWindowResize(final int w, final int h) {
                synchronized (sGLThreadManager) {
                    mWidth = w;
                    mHeight = h;
                    mSizeChanged = true;
                    mRequestRender = true;
                    mRenderComplete = false;
                    sGLThreadManager.notifyAll();
     
                    // Wait for thread to react to resize and render a frame
                    while (!mExited && !mPaused && !mRenderComplete && mGLThread != null
                            && mGLThread.ableToDraw()) {
                        if (LOG_SURFACE) {
                            Log.i("GlSurface Main thread", "GLThread.onWindowResize(" + w + "," + h
                                    + ") waiting for render complete.");
                        }
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException ex) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            // On some Qualcomm devices (such as the HTC Magic running Android 1.6),
            // there's a bug in the graphics driver that will cause glViewport() to
            // do the wrong thing in a very specific situation. When the screen is
            // rotated, if a surface is created in one layout (say, portrait view)
            // and then rotated to another, subsequent calls to glViewport are
            // clipped.
            // So, if the window is, say, 320x480 when the surface is created, and
            // then the rotation occurs and glViewport() is called with the new
            // size of 480x320, devices with the buggy driver will clip the viewport
            // to the old width (which means 320x320...ugh!). This is fixed in
            // Android 2.1 Qualcomm devices (like Nexus One) and doesn't affect
            // non-Qualcomm devices (like the Motorola DROID).
            //
            // Unfortunately, under Android 1.6 this exact case occurs when the
            // screen is put to sleep and then wakes up again. The lock screen
            // comes up in portrait mode, but at the same time the window surface
            // is also created in the backgrounded game. When the lock screen is
            // closed
            // and the game comes forward, the window is fixed to the correct size
            // which causes the bug to occur.
     
            // The solution used here is to simply never render when the window
            // surface
            // does not have the focus. When the lock screen (or menu) is up,
            // rendering
            // will stop. This resolves the driver bug (as the egl surface won't be
            // created
            // until after the screen size has been fixed), and is generally good
            // practice
            // since you don't want to be doing a lot of CPU intensive work when the
            // lock
            // screen is up (to preserve battery life).
     
            // Added to fix Android < 5 G1 Clipping problem, see comments around
            // if-block at line 1575.
            public void onWindowFocusChanged(final boolean hasFocus) {
                synchronized (sGLThreadManager) {
                    mHasFocus = hasFocus;
                    // Request a render here, because mHasFocus=false now overrides
                    // continous mode drawing
                    mRequestRender = true;
                    sGLThreadManager.notifyAll();
                }
                if (LOG_SURFACE) {
                    Log.i(TAG, "GLThread.onWindowFocusChanged(): Focus "
                            + (mHasFocus ? "gained" : "lost"));
                }
     
            }
     
            public void requestExitAndWait() {
                // don't call this from GLThread thread or it is a guaranteed
                // deadlock!
                synchronized (sGLThreadManager) {
                    mShouldExit = true;
                    sGLThreadManager.notifyAll();
                    while (!mExited) {
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException ex) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void waitForExit() {
                // don't call this from GLThread thread or it is a guaranteed
                // deadlock!
                synchronized (sGLThreadManager) {
                    while (!mExited) {
                        try {
                            sGLThreadManager.wait();
                        } catch (final InterruptedException ex) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
            }
     
            public void requestReleaseEglContextLocked() {
                mShouldReleaseEglContext = true;
                sGLThreadManager.notifyAll();
            }
     
            /**
             * Queue an "event" to be run on the GL rendering thread.
             *
             * @param r the runnable to be run on the GL rendering thread.
             */
            public void queueEvent(final Runnable r) {
                if (r == null) {
                    throw new IllegalArgumentException("r must not be null");
                }
                synchronized (sGLThreadManager) {
                    mEventQueue.add(r);
                    sGLThreadManager.notifyAll();
                }
            }
     
            // Once the thread is started, all accesses to the following member
            // variables are protected by the sGLThreadManager monitor
            private boolean mShouldExit;
            private boolean mExited;
            private boolean mRequestPaused;
            private boolean mPaused;
            private boolean mHasSurface;
            private boolean mWaitingForSurface;
            private boolean mHaveEglContext;
            private boolean mHaveEglSurface;
            private boolean mShouldReleaseEglContext;
            private int mWidth;
            private int mHeight;
            private int mRenderMode;
            private boolean mRequestRender;
            private boolean mRenderComplete;
            private final ArrayList<Runnable> mEventQueue = new ArrayList<Runnable>();
            private boolean mHasFocus;
     
            // End of member variables protected by the sGLThreadManager monitor.
     
            private final Renderer mRenderer;
            private EglHelper mEglHelper;
        }
     
        static class LogWriter extends Writer {
     
            @Override
            public void close() {
                flushBuilder();
            }
     
            @Override
            public void flush() {
                flushBuilder();
            }
     
            @Override
            public void write(final char[] buf, final int offset, final int count) {
                for (int i = 0; i < count; i++) {
                    final char c = buf[offset + i];
                    if (c == '\n') {
                        flushBuilder();
                    } else {
                        mBuilder.append(c);
                    }
                }
            }
     
            private void flushBuilder() {
                if (mBuilder.length() > 0) {
                    Log.v(TAG, mBuilder.toString());
                    mBuilder.delete(0, mBuilder.length());
                }
            }
     
            private final StringBuilder mBuilder = new StringBuilder();
        }
     
        private void checkRenderThreadState() {
            if (mGLThread != null) {
                throw new IllegalStateException(
                        "setRenderer has already been called for this instance.");
            }
        }
     
        private static class GLThreadManager {
     
            public synchronized void threadExiting(final GLThread thread) {
                if (LOG_THREADS) {
                    Log.i(TAG, "GLThreadManager: exiting tid=" + thread.getId());
                }
                thread.mExited = true;
                if (mEglOwner == thread) {
                    mEglOwner = null;
                }
                notifyAll();
            }
     
            /*
             * Tries once to acquire the right to use an EGL context. Does not
             * block. Requires that we are already in the sGLThreadManager monitor
             * when this is called.
             * @return true if the right to use an EGL context was acquired.
             */
            public boolean tryAcquireEglContextLocked(final GLThread thread) {
                if (mEglOwner == thread || mEglOwner == null) {
                    mEglOwner = thread;
                    notifyAll();
                    return true;
                }
                checkGLESVersion();
                if (mMultipleGLESContextsAllowed) {
                    return true;
                }
                // Notify the owning thread that it should release the context.
                // TODO: implement a fairness policy. Currently
                // if the owning thread is drawing continuously it will just
                // reacquire the EGL context.
                if (mEglOwner != null) {
                    mEglOwner.requestReleaseEglContextLocked();
                }
                return false;
            }
     
            /*
             * Releases the EGL context. Requires that we are already in the
             * sGLThreadManager monitor when this is called.
             */
            public void releaseEglContextLocked(final GLThread thread) {
                if (mEglOwner == thread) {
                    mEglOwner = null;
                }
                notifyAll();
            }
     
            public synchronized boolean shouldReleaseEGLContextWhenPausing() {
                // Release the EGL context when pausing even if
                // the hardware supports multiple EGL contexts.
                // Otherwise the device could run out of EGL contexts.
                return mLimitedGLESContexts;
            }
     
            public synchronized boolean shouldTerminateEGLWhenPausing() {
                checkGLESVersion();
                return !mMultipleGLESContextsAllowed;
            }
     
            public synchronized void checkGLDriver(final GL10 gl) {
                if (!mGLESDriverCheckComplete) {
                    checkGLESVersion();
                    final String renderer = gl.glGetString(GL10.GL_RENDERER);
                    if (mGLESVersion < kGLES_20) {
                        mMultipleGLESContextsAllowed = !renderer.startsWith(kMSM7K_RENDERER_PREFIX);
                        notifyAll();
                    }
                    // Regarding adreno limited contexts:
                    // https://developer.qualcomm.com/forum/qdevnet-forums/mobile-gaming-graphics-optimization-adreno/12173
                    mLimitedGLESContexts = !mMultipleGLESContextsAllowed
                            || renderer.startsWith(kADRENO);
                    if (LOG_SURFACE) {
                        Log.w(TAG, "checkGLDriver renderer = \"" + renderer
                                + "\" multipleContextsAllowed = " + mMultipleGLESContextsAllowed
                                + " mLimitedGLESContexts = " + mLimitedGLESContexts);
                    }
                    mGLESDriverCheckComplete = true;
                }
            }
     
            private void checkGLESVersion() {
                if (!mGLESVersionCheckComplete) {
                    if (ANDROID_SDK_VERSION >= 4) {
                        try {
                            mGLESVersion = ConfigurationInfo.class.getField("GL_ES_VERSION_UNDEFINED")
                                    .getInt(null);
                        } catch (final Exception e) {
                               Log.e(TAG, "extracting GL_ES_VERSION_UNDEFINED failed", e);
                            mGLESVersion = 0; // The constant is zero
                        }
                        if (mGLESVersion >= kGLES_20) {
                            mMultipleGLESContextsAllowed = true;
                        }
                    } else {
                        mGLESVersion = 0;
                    }
                    if (LOG_SURFACE) {
                        Log.w(TAG, "checkGLESVersion mGLESVersion =" + " " + mGLESVersion
                                + " mMultipleGLESContextsAllowed = "
                                + mMultipleGLESContextsAllowed);
                    }
                    mGLESVersionCheckComplete = true;
                }
            }
     
            private boolean mGLESVersionCheckComplete;
            private int mGLESVersion;
            private boolean mGLESDriverCheckComplete;
            private boolean mMultipleGLESContextsAllowed;
            private boolean mLimitedGLESContexts;
            private static final int kGLES_20 = 0x20000;
            private static final String kMSM7K_RENDERER_PREFIX = "Q3Dimension MSM7500 ";
            private static final String kADRENO = "Adreno";
            private GLThread mEglOwner;
        }
     
        public boolean handleSurfaceException(final Context context, final String function,
                final String errorCode) {
            return false;
        }
     
        public static class StopGlThreadException extends Exception {
            private static final long serialVersionUID = 1L;
     
            public StopGlThreadException(final String detailMessage) {
                super(detailMessage);
            }
        }
     
        public class EglConfigInfo {
            public final int mRedSize;
            public final int mGreenSize;
            public final int mBlueSize;
            public final int mAlphaSize;
            public final int mDepthSize;
            public final int mStencilSize;
     
            private int findConfigAttrib(final EGL10 egl, final EGLDisplay display,
                    final EGLConfig config, final int attribute, final int defaultValue) {
                final int[] value = new int[1];
                if (egl.eglGetConfigAttrib(display, config, attribute, value)) {
                    return value[0];
                }
                return defaultValue;
            }
     
            private EglConfigInfo(final EglHelper eglHelper) {
                this(eglHelper.mEgl, eglHelper.mEglDisplay, eglHelper.mEglConfig);
            }
     
            private EglConfigInfo(final EGL10 egl, final EGLDisplay display, final EGLConfig config) {
                final int d = findConfigAttrib(egl, display, config, EGL10.EGL_DEPTH_SIZE, 0);
                final int s = findConfigAttrib(egl, display, config, EGL10.EGL_STENCIL_SIZE, 0);
                final int r = findConfigAttrib(egl, display, config, EGL10.EGL_RED_SIZE, 0);
                final int g = findConfigAttrib(egl, display, config, EGL10.EGL_GREEN_SIZE, 0);
                final int b = findConfigAttrib(egl, display, config, EGL10.EGL_BLUE_SIZE, 0);
                final int a = findConfigAttrib(egl, display, config, EGL10.EGL_ALPHA_SIZE, 0);
     
                this.mRedSize = r;
                this.mGreenSize = g;
                this.mBlueSize = b;
                this.mAlphaSize = a;
                this.mDepthSize = d;
                this.mStencilSize = s;
            }
     
            public boolean isRgb888() {
                return mRedSize == 8 && mGreenSize == 8 && mBlueSize == 8;
            }
     
            public boolean isRgb565() {
                return mRedSize == 5 && mGreenSize == 6 && mBlueSize == 5;
            }
     
            public boolean isAlpha8() {
                return mAlphaSize == 8;
            }
     
            public boolean isAlpha0() {
                return mAlphaSize == 0;
            }
     
            @Override
            public String toString() {
                return "RGBADS=" + mRedSize + mGreenSize + mBlueSize + mAlphaSize + mDepthSize
                        + mStencilSize;
            }
        }
     
        private static final GLThreadManager sGLThreadManager = new GLThreadManager();
        private boolean mSizeChanged = true;
     
        private GLThread mGLThread;
        private Renderer mRenderer;
        private boolean mDetached;
        private EGLConfigChooser mEGLConfigChooser;
        private EGLContextFactory mEGLContextFactory;
        private EGLWindowSurfaceFactory mEGLWindowSurfaceFactory;
        private GLWrapper mGLWrapper;
        private int mDebugFlags;
        private int mEGLContextClientVersion;
        private boolean mPreserveEGLContextOnPause;
    }

