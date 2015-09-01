{************************************************************************************

Filename    :   libovr.pas
Content     :   Object-Pascal Interface to Oculus tracking and rendering.
Created     :   March 3, 2015
Authors     :   Sergio Flores

Copyright   :   Copyright 2014 Oculus VR, LLC All Rights reserved.

Licensed under the Oculus VR Rift SDK License Version 3.2 (the "License");
you may not use the Oculus VR Rift SDK except in compliance with the License,
which is provided at the time of installation or download, or which
otherwise accompanies this software in either electronic or hard copy form.

You may obtain a copy of the License at

http://www.oculusvr.com/licenses/LICENSE-3.2

Unless required by applicable law or agreed to in writing, the Oculus VR SDK
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

************************************************************************************}

Unit libovr;

Interface

{$IFDEF MSWINDOWS}
Uses Windows;
{$ENDIF}

Const
{$IFDEF MSWINDOWS}
  OVR_LIB_NAME = 'libovr.dll';
{$ENDIF}  

  OVR_KEY_USER                        = 'User';              // string
  OVR_KEY_NAME                        = 'Name';              // string
  OVR_KEY_GENDER                      = 'Gender';            // string
  OVR_KEY_PLAYER_HEIGHT               = 'PlayerHeight';      // float
  OVR_KEY_EYE_HEIGHT                  = 'EyeHeight';         // float
  OVR_KEY_IPD                         = 'IPD';               // float
  OVR_KEY_NECK_TO_EYE_DISTANCE        = 'NeckEyeDistance';   // float[2]
  OVR_KEY_EYE_RELIEF_DIAL             = 'EyeReliefDial';     // int
  OVR_KEY_EYE_TO_NOSE_DISTANCE        = 'EyeToNoseDist';     // float[2]
  OVR_KEY_MAX_EYE_TO_PLATE_DISTANCE   = 'MaxEyeToPlateDist'; // float[2]
  OVR_KEY_EYE_CUP                     = 'EyeCup';            // char[16]
  OVR_KEY_CUSTOM_EYE_RENDER           = 'CustomEyeRender';   // bool
  OVR_KEY_CAMERA_POSITION				= 'CenteredFromWorld'; // double[7]

  // Default measurements empirically determined at Oculus to make us happy
  // The neck model numbers were derived as an average of the male and female averages from ANSUR-88
  // NECK_TO_EYE_HORIZONTAL = H22 - H43 = INFRAORBITALE_BACK_OF_HEAD - TRAGION_BACK_OF_HEAD
  // NECK_TO_EYE_VERTICAL = H21 - H15 = GONION_TOP_OF_HEAD - ECTOORBITALE_TOP_OF_HEAD
  // These were determined to be the best in a small user study, clearly beating out the previous default values
  OVR_DEFAULT_GENDER                  = 'Unknown';
  OVR_DEFAULT_PLAYER_HEIGHT           = 1.778;
  OVR_DEFAULT_EYE_HEIGHT              = 1.675;
  OVR_DEFAULT_IPD                     = 0.064;
  OVR_DEFAULT_NECK_TO_EYE_HORIZONTAL  = 0.0805;
  OVR_DEFAULT_NECK_TO_EYE_VERTICAL    = 0.075;
  OVR_DEFAULT_EYE_RELIEF_DIAL         = 3;
//  OVR_DEFAULT_CAMERA_POSITION			{0,0,0,1,0,0,0}

Type
  ovrBool = Boolean;

  WordArray = Array[0..99999] Of Word;
  PWordArray = ^WordArray;

  ovrRGB = Packed Record
    r,g, b:Byte;
  End;
  
  /// A 2D vector with integer components.
  ovrVector2i = Packed Record
    x, y:Integer;
  End;

/// A 2D size with integer components.
  ovrSizei = Packed Record
    w, h:Integer;
  End;

/// A 2D rectangle with a position and size.
/// All components are integers.
  PovrRecti = ^ovrRecti;
  ovrRecti  = Packed Record
    Pos:ovrVector2i;
    Size:ovrSizei;
  End;

/// A quaternion rotation.
  ovrQuatf  = Packed Record
    x, y, z, w:Single;
  End;

/// A 2D vector with float components.
  ovrVector2f  = Packed Record
    x, y:Single;
  End;
  ovrVector2fArray = Array[0..1] Of ovrVector2f;
  PovrVector2fArray = ^ovrVector2fArray;

/// A 3D vector with float components.
  ovrVector3f  = Packed Record
    x, y, z:Single;
  End;
  ovrVector3fArray = Array[0..1] Of ovrVector3f;
  PovrVector3fArray = ^ovrVector3fArray;

/// A 4x4 matrix with float elements.
  ovrMatrix4f  = Packed Record
    M:Array[0..15] Of Single;
  End;
  ovrMatrix4fArray = Array[0..1] Of ovrMatrix4f;
  PovrMatrix4fArray = ^ovrMatrix4fArray;

/// Position and orientation together.
  ovrPosef  = Packed Record
    Orientation:ovrQuatf;
    Position:ovrVector3f;
  End;
  ovrPosefArray = Array[0..1] Of ovrPosef;
  PovrPosefArray = ^ovrPosefArray;

/// A full pose (rigid body) configuration with first and second derivatives.
  ovrPoseStatef  = Packed Record
    ThePose:ovrPosef;               ///< The body's position and orientation.
    AngularVelocity:ovrVector3f;       ///< The body's angular velocity in radians per second.
    LinearVelocity:ovrVector3f;        ///< The body's velocity in meters per second.
    AngularAcceleration:ovrVector3f;   ///< The body's angular acceleration in radians per second per second.
    LinearAcceleration:ovrVector3f;    ///< The body's acceleration in meters per second per second.
    TimeInSeconds:Double;         ///< Absolute time of this state sample.
  End;

/// Field Of View (FOV) in tangent of the angle units.
/// As an example, for a standard 90 degree vertical FOV, we would
/// have: { UpTan = tan(90 degrees / 2), DownTan = tan(90 degrees / 2) }.
  ovrFovPort  = Packed Record
    /// The tangent of the angle between the viewing vector and the top edge of the field of view.
    UpTan:Single;
    /// The tangent of the angle between the viewing vector and the bottom edge of the field of view.
    DownTan:Single;
    /// The tangent of the angle between the viewing vector and the left edge of the field of view.
    LeftTan:Single;
    /// The tangent of the angle between the viewing vector and the right edge of the field of view.
    RightTan:Single;
  End;
  PovrFovPort = ^ovrFovPort;

  ovrFovPortArray = Array[0..1] Of ovrFovPort;
  PovrFovPortArray = ^ovrFovPortArray;

//-----------------------------------------------------------------------------------
// ***** HMD Types

/// Enumerates all HMD types that we support.

  ovrHmdType = Integer;

Const
    ovrHmd_None             = 0;
    ovrHmd_DK1              = 3;
    ovrHmd_DKHD             = 4;
    ovrHmd_DK2              = 6;

/// HMD capability bits reported by device.
Const
    // Read-only flags.
    ovrHmdCap_Present           = $0001;   /// The HMD is plugged in and detected by the system.
    ovrHmdCap_Available         = $0002;   /// The HMD and its sensor are available for ownership use.
											/// i.e. it is not already owned by another application.
    ovrHmdCap_Captured          = $0004;   /// Set to 'true' if we captured ownership of this HMD.

    // These flags are intended for use with the new driver display mode.
    ovrHmdCap_ExtendDesktop     = $0008;   /// (read only) Means the display driver is in compatibility mode.

    // Modifiable flags (through ovrHmd_SetEnabledCaps).
    ovrHmdCap_NoMirrorToWindow  = $2000;   /// Disables mirroring of HMD output to the window. This may improve
											/// rendering performance slightly (only if 'ExtendDesktop' is off).
    ovrHmdCap_DisplayOff        = $0040;   /// Turns off HMD screen and output (only if 'ExtendDesktop' is off).

    ovrHmdCap_LowPersistence    = $0080;   /// HMD supports low persistence mode.
    ovrHmdCap_DynamicPrediction = $0200;   /// Adjust prediction dynamically based on internally measured latency.
    ovrHmdCap_DirectPentile     = $0400;   /// Write directly in pentile color mapping format
    ovrHmdCap_NoVSync           = $1000;   /// Support rendering without VSync for debugging.

    // These bits can be modified by ovrHmd_SetEnabledCaps.
    ovrHmdCap_Writable_Mask     = $32F0;

    /// These flags are currently passed into the service. May change without notice.
    ovrHmdCap_Service_Mask      = $22F0;


/// Tracking capability bits reported by the device.
/// Used with ovrHmd_ConfigureTracking.
    ovrTrackingCap_Orientation      = $0010;   /// Supports orientation tracking (IMU).
    ovrTrackingCap_MagYawCorrection = $0020;   /// Supports yaw drift correction via a magnetometer or other means.
    ovrTrackingCap_Position         = $0040;   /// Supports positional tracking.
    /// Overrides the other flags. Indicates that the application
    /// doesn't care about tracking settings. This is the internal
    /// default before ovrHmd_ConfigureTracking is called.
    ovrTrackingCap_Idle             = $0100;

/// Distortion capability bits reported by device.
/// Used with ovrHmd_ConfigureRendering and ovrHmd_CreateDistortionMesh.
    ovrDistortionCap_Chromatic          =   $01;     /// Supports chromatic aberration correction.
    ovrDistortionCap_TimeWarp           =   $02;     /// Supports timewarp.
    // $04 unused
    ovrDistortionCap_Vignette           =   $08;     /// Supports vignetting around the edges of the view.
    ovrDistortionCap_NoRestore          =   $10;     /// Do not save and restore the graphics and compute state when rendering distortion.
    ovrDistortionCap_FlipInput          =   $20;     /// Flip the vertical texture coordinate of input images.
    ovrDistortionCap_SRGB               =   $40;     /// Assume input images are in sRGB gamma-corrected color space.
    ovrDistortionCap_Overdrive          =   $80;     /// Overdrive brightness transitions to reduce artifacts on DK2+ displays
    ovrDistortionCap_HqDistortion       =  $100;     /// High-quality sampling of distortion buffer for anti-aliasing
    ovrDistortionCap_LinuxDevFullscreen =  $200;     /// Indicates window is fullscreen on a device when set. The SDK will automatically apply distortion mesh rotation if needed.
    ovrDistortionCap_ComputeShader      =  $400;     /// Using compute shader (DX11+ only)
    ovrDistortionCap_ProfileNoTimewarpSpinWaits = $10000;  /// Use when profiling with timewarp to remove false positives

Type
/// Specifies which eye is being used for rendering.
/// This type explicitly does not include a third "NoStereo" option, as such is
/// not required for an HMD-centered API.

ovrEyeType = Integer;

Const
    ovrEye_Left  = 0;
    ovrEye_Right = 1;
    ovrEye_Count = 2;

Type
  ovrHmd = ^ovrHmdDesc;
/// This is a complete descriptor of the HMD.
  ovrHmdDesc = Record
    /// Internal handle of this HMD.
    Handle:Pointer;

    /// This HMD's type.
    hmdType:ovrHmdType;

    /// Name string describing the product: "Oculus Rift DK1", etc.
    ProductName:PAnsiChar;
    Manufacturer:PAnsiChar;

    /// HID Vendor and ProductId of the device.
    VendorId:Word;
    ProductId:Word;
    /// Sensor (and display) serial number.
    SerialNumber:Array[0..23] Of AnsiChar;
    /// Sensor firmware version.
    FirmwareMajor:Word;
    FirmwareMinor:Word;
    /// External tracking camera frustum dimensions (if present).
    CameraFrustumHFovInRadians:Single;
    CameraFrustumVFovInRadians:Single;
    CameraFrustumNearZInMeters:Single;
    CameraFrustumFarZInMeters:Single;

    /// Capability bits described by ovrHmdCaps.
    HmdCaps:Cardinal;
	/// Capability bits described by ovrTrackingCaps.
    TrackingCaps:Cardinal;
    /// Capability bits described by ovrDistortionCaps.
    DistortionCaps:Cardinal;

    /// These define the recommended and maximum optical FOVs for the HMD.
    DefaultEyeFov:Array[0..Pred(ovrEye_Count)] Of ovrFovPort;
    MaxEyeFov:Array[0..Pred(ovrEye_Count)] Of ovrFovPort;

    /// Preferred eye rendering order for best performance.
    /// Can help reduce latency on sideways-scanned screens.
    EyeRenderOrder:Array[0..Pred(ovrEye_Count)] Of ovrEyeType;

    /// Resolution of the full HMD screen (both eyes) in pixels.
    Resolution:ovrSizei;
    /// Location of the application window on the desktop (or 0,0).
    WindowsPos:ovrVector2i;

    /// Display that the HMD should present on.
    /// TBD: It may be good to remove this information relying on WindowPos instead.
    /// Ultimately, we may need to come up with a more convenient alternative,
    /// such as API-specific functions that return adapter, or something that will
    /// work with our monitor driver.
    /// Windows: (e.g. "\\\\.\\DISPLAY3", can be used in EnumDisplaySettings/CreateDC).
    DisplayDeviceName:PAnsiChar;
    /// MacOS:
    DisplayId:Integer;
End;

/// Simple type ovrHmd is used in ovrHmd_* calls.
//typedef const ovrHmdDesc * ovrHmd;

/// Bit flags describing the current status of sensor tracking.
Const
    ovrStatus_OrientationTracked    = $0001;   /// Orientation is currently tracked (connected and in use).
    ovrStatus_PositionTracked       = $0002;   /// Position is currently tracked (false if out of range).
    ovrStatus_CameraPoseTracked     = $0004;   /// Camera pose is currently tracked.
    ovrStatus_PositionConnected     = $0020;   /// Position tracking hardware is connected.
    ovrStatus_HmdConnected          = $0080;    /// HMD Display is available and connected.

/// Specifies a reading we can query from the sensor.
Type
  ovrSensorData = Packed Record
    Accelerometer:ovrVector3f;    /// Acceleration reading in m/s^2.
    Gyro:ovrVector3f;             /// Rotation rate in rad/s.
    Magnetometer:ovrVector3f;     /// Magnetic field in Gauss.
    Temperature:Single;      /// Temperature of the sensor in degrees Celsius.
    TimeInSeconds:Single;    /// Time when the reported IMU reading took place, in seconds.
  End;

/// Tracking state at a given absolute time (describes predicted HMD pose etc).
/// Returned by ovrHmd_GetTrackingState.
  PovrTrackingState = ^ovrTrackingState;
  ovrTrackingState = Record
    /// Predicted head pose (and derivatives) at the requested absolute time.
    /// The look-ahead interval is equal to (HeadPose.TimeInSeconds - RawSensorData.TimeInSeconds).
    HeadPose:ovrPoseStatef;

    /// Current pose of the external camera (if present).
    /// This pose includes camera tilt (roll and pitch). For a leveled coordinate
    /// system use LeveledCameraPose.
    CameraPose:ovrPosef;

    /// Camera frame aligned with gravity.
    /// This value includes position and yaw of the camera, but not roll and pitch.
    /// It can be used as a reference point to render real-world objects in the correct location.
    LeveledCameraPose:ovrPosef;

    /// The most recent sensor data received from the HMD.
    RawSensorData:ovrSensorData;

    /// Tracking status described by ovrStatusBits.
    StatusFlags:Cardinal;

    //// 0.4.1

    // Measures the time from receiving the camera frame until vision CPU processing completes.
    LastVisionProcessingTime:double;

    //// 0.4.3

    // Measures the time from exposure until the pose is available for the frame, including processing time.
    LastVisionFrameLatency:double;

    /// Tag the vision processing results to a certain frame counter number.
    LastCameraFrameCounter:Cardinal;
  End;

/// Frame timing data reported by ovrHmd_BeginFrameTiming() or ovrHmd_BeginFrame().
  ovrFrameTiming = Packed Record
    /// The amount of time that has passed since the previous frame's
	/// ThisFrameSeconds value (usable for movement scaling).
    /// This will be clamped to no more than 0.1 seconds to prevent
    /// excessive movement after pauses due to loading or initialization.
    DeltaSeconds:Single;

    /// It is generally expected that the following holds:
    /// ThisFrameSeconds < TimewarpPointSeconds < NextFrameSeconds <
    /// EyeScanoutSeconds[EyeOrder[0]] <= ScanoutMidpointSeconds <= EyeScanoutSeconds[EyeOrder[1]].

    /// Absolute time value when rendering of this frame began or is expected to
    /// begin. Generally equal to NextFrameSeconds of the previous frame. Can be used
    /// for animation timing.
    ThisFrameSeconds:double;
    /// Absolute point when IMU expects to be sampled for this frame.
    TimewarpPointSeconds:double;
    /// Absolute time when frame Present followed by GPU Flush will finish and the next frame begins.
    NextFrameSeconds:double;

    /// Time when half of the screen will be scanned out. Can be passed as an absolute time
	/// to ovrHmd_GetTrackingState() to get the predicted general orientation.
    ScanoutMidpointSeconds:double;
    /// Timing points when each eye will be scanned out to display. Used when rendering each eye.
    EyeScanoutSeconds:Array[0..1] Of Double;
  End;

/// Rendering information for each eye. Computed by either ovrHmd_ConfigureRendering()
/// or ovrHmd_GetRenderDesc() based on the specified FOV. Note that the rendering viewport
/// is not included here as it can be specified separately and modified per frame through:
///    (a) ovrHmd_GetRenderScaleAndOffset in the case of client rendered distortion,
/// or (b) passing different values via ovrTexture in the case of SDK rendered distortion.
  ovrEyeRenderDesc = Packed Record
    Eye:ovrEyeType;                        ///< The eye index this instance corresponds to.
    Fov:ovrFovPort;                        ///< The field of view.
	  DistortedViewport:ovrRecti;          ///< Distortion viewport.
    PixelsPerTanAngleAtCenter:ovrVector2f;  ///< How many display pixels will fit in tan(angle) = 1.
    HmdToEyeViewOffset:ovrVector3f;         ///< Translation to be applied to view matrix for each eye offset.
  End;

  ovrEyeRenderDescArray = Array[0..1] Of ovrEyeRenderDesc;
  PovrEyeRenderDescArray = ^ovrEyeRenderDescArray;

//-----------------------------------------------------------------------------------
// ***** Platform-independent Rendering Configuration

/// These types are used to hide platform-specific details when passing
/// render device, OS, and texture data to the API.
///
/// The benefit of having these wrappers versus platform-specific API functions is
/// that they allow game glue code to be portable. A typical example is an
/// engine that has multiple back ends, say GL and D3D. Portable code that calls
/// these back ends may also use LibOVR. To do this, back ends can be modified
/// to return portable types such as ovrTexture and ovrRenderAPIConfig.

ovrRenderAPIType = Cardinal;
Const
    ovrRenderAPI_None = 0;
    ovrRenderAPI_OpenGL = 1;
    ovrRenderAPI_Android_GLES = 2;  // May include extra native window pointers, etc.
    ovrRenderAPI_D3D9 = 3;
    ovrRenderAPI_D3D10 = 4;
    ovrRenderAPI_D3D11 = 5;
    ovrRenderAPI_Count = 6;

Type
/// Platform-independent part of rendering API-configuration data.
/// It is a part of ovrRenderAPIConfig, passed to ovrHmd_Configure.
  ovrRenderAPIConfigHeader = Packed Record
    API:Integer;
    BackBufferSize:ovrSizei;    // Previously named RTSize.
    Multisample:Integer;
  End;

/// Contains platform-specific information for rendering.
  PovrRenderAPIConfig = ^ovrRenderAPIConfig;
  ovrRenderAPIConfig = Packed Record
    Header:ovrRenderAPIConfigHeader;
    PlatformData:Array[0..7] Of Cardinal;
  End;

  ovrGLConfig = Packed Record
    Case Integer Of
    0: (
    /// General device settings.
      Header:ovrRenderAPIConfigHeader;

      {$IFDEF MSWINDOWS}
      /// The optional window handle. If unset, rendering will use the current window.
      Window:HWND;
      /// The optional device context. If unset, rendering will use a new context.
      DC:HDC;
      {$ENDIF}

    {$IFDEF LINUX}
      /// Optional display. If unset, will issue glXGetCurrentDisplay when context is current.
      Disp:PXDisplay;
    {$ENDIF}
    );
    1: (Generic:ovrRenderAPIConfig);
  End;

/// Platform-independent part of the eye texture descriptor.
/// It is a part of ovrTexture, passed to ovrHmd_EndFrame.
/// If RenderViewport is all zeros then the full texture will be used.
  ovrTextureHeader = Packed Record
    API:ovrRenderAPIType;
    TextureSize:ovrSizei;
    RenderViewport:ovrRecti;  // Pixel viewport in texture that holds eye image.
  End;

/// Contains platform-specific information about a texture.
  ovrTexture = Record
    Header:ovrTextureHeader;
    PlatformData:Array[0..7] Of Cardinal;
  End;

  ovrGLTexture = Record
    /// General device settings.
    Case Integer Of
    0: (Header:ovrTextureHeader;
        /// The OpenGL name for this texture.
        TexId:Cardinal);
    1:  (Generic:ovrTexture;)
  End;


  ovrTextureArray = Array[0..1] Of ovrTexture;
  PovrTextureArray = ^ovrTextureArray;

// -----------------------------------------------------------------------------------
// ***** API Interfaces

// Basic steps to use the API:
//
// Setup:
//  * ovrInitialize()
//  * ovrHMD hmd = ovrHmd_Create(0)
//  * Use hmd members and ovrHmd_GetFovTextureSize() to determine graphics configuration.
//  * Call ovrHmd_ConfigureTracking() to configure and initialize tracking.
//  * Call ovrHmd_ConfigureRendering() to setup graphics for SDK rendering,
//    which is the preferred approach.
//    Please refer to "Client Distorton Rendering" below if you prefer to do that instead.
//  * If the ovrHmdCap_ExtendDesktop flag is not set, then use ovrHmd_AttachToWindow to
//    associate the relevant application window with the hmd.
//  * Allocate render target textures as needed.
//
// Game Loop:
//  * Call ovrHmd_BeginFrame() to get the current frame timing information.
//  * Render each eye using ovrHmd_GetEyePoses or ovrHmd_GetHmdPosePerEye to get
//    the predicted hmd pose and each eye pose.
//  * Call ovrHmd_EndFrame() to render the distorted textures to the back buffer
//    and present them on the hmd.
//
// Shutdown:
//  * ovrHmd_Destroy(hmd)
//  * ovr_Shutdown()
//

// ovr_InitializeRenderingShim initializes the rendering shim appart from everything
// else in LibOVR. This may be helpful if the application prefers to avoid
// creating any OVR resources (allocations, service connections, etc) at this point.
// ovr_InitializeRenderingShim does not bring up anything within LibOVR except the
// necessary hooks to enable the Direct-to-Rift functionality.
//
// Either ovr_InitializeRenderingShim() or ovr_Initialize() must be called before any
// Direct3D or OpenGL initilization is done by applictaion (creation of devices, etc).
// ovr_Initialize() must still be called after to use the rest of LibOVR APIs.
Function ovr_InitializeRenderingShim():ovrBool; CDecl; External OVR_LIB_NAME;

// Library init/shutdown, must be called around all other OVR code.
// No other functions calls besides ovr_InitializeRenderingShim are allowed
// before ovr_Initialize succeeds or after ovr_Shutdown.
/// Initializes all Oculus functionality.
Function ovr_Initialize():ovrBool; CDecl; External OVR_LIB_NAME;

/// Shuts down all Oculus functionality.
Procedure ovr_Shutdown(); CDecl; External OVR_LIB_NAME;

/// Returns version string representing libOVR version. Static, so
/// string remains valid for app lifespan
Function ovr_GetVersionString():PAnsiChar; CDecl; External OVR_LIB_NAME;

/// Detects or re-detects HMDs and reports the total number detected.
/// Users can get information about each HMD by calling ovrHmd_Create with an index.
Function ovrHmd_Detect():Integer; CDecl; External OVR_LIB_NAME;

/// Creates a handle to an HMD which doubles as a description structure.
/// Index can [0 .. ovrHmd_Detect()-1]. Index mappings can cange after each ovrHmd_Detect call.
/// If not null, then the returned handle must be freed with ovrHmd_Destroy.
Function ovrHmd_Create(index:Integer):ovrHmd; CDecl; External OVR_LIB_NAME;
Procedure ovrHmd_Destroy(hmd:ovrHmd); CDecl; External OVR_LIB_NAME;

/// Creates a 'fake' HMD used for debugging only. This is not tied to specific hardware,
/// but may be used to debug some of the related rendering.
Function ovrHmd_CreateDebug(hmdtype:ovrHmdType):ovrHmd; CDecl; External OVR_LIB_NAME;

/// Returns last error for HMD state. Returns null for no error.
/// String is valid until next call or GetLastError or HMD is destroyed.
/// Pass null hmd to get global errors (during create etc).
Function ovrHmd_GetLastError(hmd:ovrHmd):PAnsiChar; CDecl; External OVR_LIB_NAME;

/// Platform specific function to specify the application window whose output will be
/// displayed on the HMD. Only used if the ovrHmdCap_ExtendDesktop flag is false.
///   Windows: SwapChain associated with this window will be displayed on the HMD.
///            Specify 'destMirrorRect' in window coordinates to indicate an area
///            of the render target output that will be mirrored from 'sourceRenderTargetRect'.
///            Null pointers mean "full size".
/// @note Source and dest mirror rects are not yet implemented.
Function ovrHmd_AttachToWindow(hmd:ovrHmd; window:Cardinal; destMirrorRect, sourceRenderTargetRect:PovrRecti):Boolean; CDecl; External OVR_LIB_NAME;

/// Returns capability bits that are enabled at this time as described by ovrHmdCaps.
/// Note that this value is different font ovrHmdDesc::HmdCaps, which describes what
/// capabilities are available for that HMD.
Function ovrHmd_GetEnabledCaps(hmd:ovrHmd):Cardinal; CDecl; External OVR_LIB_NAME;

/// Modifies capability bits described by ovrHmdCaps that can be modified,
/// such as ovrHmdCap_LowPersistance.
Procedure ovrHmd_SetEnabledCaps(hmd:ovrHmd; hmdCaps:Cardinal); CDecl; External OVR_LIB_NAME;

//-------------------------------------------------------------------------------------
// ***** Tracking Interface

/// All tracking interface functions are thread-safe, allowing tracking state to be sampled
/// from different threads.
/// ConfigureTracking starts sensor sampling, enabling specified capabilities,
///    described by ovrTrackingCaps.
///  - supportedTrackingCaps specifies support that is requested. The function will succeed
///   even if these caps are not available (i.e. sensor or camera is unplugged). Support
///    will automatically be enabled if such device is plugged in later. Software should
///    check ovrTrackingState.StatusFlags for real-time status.
///  - requiredTrackingCaps specify sensor capabilities required at the time of the call.
///    If they are not available, the function will fail. Pass 0 if only specifying
///    supportedTrackingCaps.
///  - Pass 0 for both supportedTrackingCaps and requiredTrackingCaps to disable tracking.
Function ovrHmd_ConfigureTracking(hmd:ovrHmd; supportedTrackingCaps, requiredTrackingCaps:Cardinal):ovrBool; CDecl; External OVR_LIB_NAME;

/// Re-centers the sensor orientation.
/// Normally this will recenter the (x,y,z) translational components and the yaw
/// component of orientation.
Procedure ovrHmd_RecenterPose(hmd:ovrHmd); CDecl; External OVR_LIB_NAME;

/// Returns tracking state reading based on the specified absolute system time.
/// Pass an absTime value of 0.0 to request the most recent sensor reading. In this case
/// both PredictedPose and SamplePose will have the same value.
/// ovrHmd_GetEyePoses relies on this function internally.
/// This may also be used for more refined timing of FrontBuffer rendering logic, etc.
Function ovrHmd_GetTrackingState(hmd:ovrHmd; absTime:Double):ovrTrackingState; CDecl; External OVR_LIB_NAME;

//-------------------------------------------------------------------------------------
// ***** Graphics Setup

/// Calculates the recommended texture size for rendering a given eye within the HMD
/// with a given FOV cone. Higher FOV will generally require larger textures to
/// maintain quality.
///  - pixelsPerDisplayPixel specifies the ratio of the number of render target pixels
///    to display pixels at the center of distortion. 1.0 is the default value. Lower
///    values can improve performance.
Function ovrHmd_GetFovTextureSize(hmd:ovrHmd; eye:ovrEyeType; fov:ovrFovPort; pixelsPerDisplayPixel:Single):Int64; CDecl; External OVR_LIB_NAME;

//-------------------------------------------------------------------------------------
// *****  Rendering API Thread Safety

//  All of rendering functions including the configure and frame functions
// are *NOT thread safe*. It is ok to use ConfigureRendering on one thread and handle
//  frames on another thread, but explicit synchronization must be done since
//  functions that depend on configured state are not reentrant.
//
//  As an extra requirement, any of the following calls must be done on
//  the render thread, which is the same thread that calls ovrHmd_BeginFrame
//  or ovrHmd_BeginFrameTiming.
//    - ovrHmd_EndFrame
//    - ovrHmd_GetEyeTimewarpMatrices

//-------------------------------------------------------------------------------------
// *****  SDK Distortion Rendering Functions

// These functions support rendering of distortion by the SDK through direct
// access to the underlying rendering API, such as D3D or GL.
// This is the recommended approach since it allows better support for future
// Oculus hardware, and enables a range of low-level optimizations.

/// Configures rendering and fills in computed render parameters.
/// This function can be called multiple times to change rendering settings.
/// eyeRenderDescOut is a pointer to an array of two ovrEyeRenderDesc structs
/// that are used to return complete rendering information for each eye.
///  - apiConfig provides D3D/OpenGL specific parameters. Pass null
///    to shutdown rendering and release all resources.
///  - distortionCaps describe desired distortion settings.
Function ovrHmd_ConfigureRendering(hmd:ovrHmd; apiConfig:PovrRenderAPIConfig; distortionCaps:Cardinal; eyeFovIn:PovrFovPortArray;
                                              eyeRenderDescOut:PovrEyeRenderDescArray):ovrBool; CDecl; External OVR_LIB_NAME;


/// Begins a frame, returning timing information.
/// This should be called at the beginning of the game rendering loop (on the render thread).
/// Pass 0 for the frame index if not using ovrHmd_GetFrameTiming.
Function ovrHmd_BeginFrame(hmd:ovrHmd; frameIndex:Cardinal):ovrFrameTiming; CDecl; External OVR_LIB_NAME;

/// Ends a frame, submitting the rendered textures to the frame buffer.
/// - RenderViewport within each eyeTexture can change per frame if necessary.
/// - 'renderPose' will typically be the value returned from ovrHmd_GetEyePoses,
///   ovrHmd_GetHmdPosePerEye but can be different if a different head pose was
///   used for rendering.
/// - This may perform distortion and scaling internally, assuming is it not
///   delegated to another thread.
/// - Must be called on the same thread as BeginFrame.
/// - *** This Function will call Present/SwapBuffers and potentially wait for GPU Sync ***.
Procedure ovrHmd_EndFrame(hmd:ovrHmd; renderPose:PovrPosefArray; eyeTexture:PovrTextureArray); CDecl; External OVR_LIB_NAME;

/// Returns predicted head pose in outHmdTrackingState and offset eye poses in outEyePoses
/// as an atomic operation. Caller need not worry about applying HmdToEyeViewOffset to the
/// returned outEyePoses variables.
/// - Thread-safe function where caller should increment frameIndex with every frame
///   and pass the index where applicable to functions called on the  rendering thread.
/// - hmdToEyeViewOffset[2] can be ovrEyeRenderDesc.HmdToEyeViewOffset returned from 
///   ovrHmd_ConfigureRendering or ovrHmd_GetRenderDesc. For monoscopic rendering,
///   use a vector that is the average of the two vectors for both eyes.
/// - If frameIndex is not being used, pass in 0.
/// - Assuming outEyePoses are used for rendering, it should be passed into ovrHmd_EndFrame.
/// - If called doesn't need outHmdTrackingState, it can be NULL
Procedure ovrHmd_GetEyePoses(hmd:ovrHmd; frameIndex:Cardinal; hmdToEyeViewOffset:PovrVector3fArray;
                                   outEyePoses:PovrPosefArray; outHmdTrackingState:PovrTrackingState); CDecl; External OVR_LIB_NAME;

/// Function was previously called ovrHmd_GetEyePose
/// Returns the predicted head pose to use when rendering the specified eye.
/// - Important: Caller must apply HmdToEyeViewOffset before using ovrPosef for rendering
/// - Must be called between ovrHmd_BeginFrameTiming and ovrHmd_EndFrameTiming.
/// - If the pose is used for rendering the eye, it should be passed to ovrHmd_EndFrame.
/// - Parameter 'eye' is used for prediction timing only
Function ovrHmd_GetHmdPosePerEye(hmd:ovrHmd; eye:ovrEyeType):ovrPosef; CDecl; External OVR_LIB_NAME;


//-------------------------------------------------------------------------------------
// *****  Client Distortion Rendering Functions

// These functions provide the distortion data and render timing support necessary to allow
// client rendering of distortion. Client-side rendering involves the following steps:
//
//  1. Setup ovrEyeDesc based on the desired texture size and FOV.
//     Call ovrHmd_GetRenderDesc to get the necessary rendering parameters for each eye.
//
//  2. Use ovrHmd_CreateDistortionMesh to generate the distortion mesh.
//
//  3. Use ovrHmd_BeginFrameTiming, ovrHmd_GetEyePoses, and ovrHmd_BeginFrameTiming in
//     the rendering loop to obtain timing and predicted head orientation when rendering each eye.
//      - When using timewarp, use ovr_WaitTillTime after the rendering and gpu flush, followed
//        by ovrHmd_GetEyeTimewarpMatrices to obtain the timewarp matrices used
//        by the distortion pixel shader. This will minimize latency.
//

/// Computes the distortion viewport, view adjust, and other rendering parameters for
/// the specified eye. This can be used instead of ovrHmd_ConfigureRendering to do
/// setup for client rendered distortion.
Function ovrHmd_GetRenderDesc(hmd:ovrHmd; eyeType:ovrEyeType; fov:ovrFovPort):ovrEyeRenderDesc; CDecl; External OVR_LIB_NAME;


/// Describes a vertex used by the distortion mesh. This is intended to be converted into
/// the engine-specific format. Some fields may be unused based on the ovrDistortionCaps
/// flags selected. TexG and TexB, for example, are not used if chromatic correction is
/// not requested.
Type
ovrDistortionVertex = Packed Record
    ScreenPosNDC:ovrVector2f ;    ///< [-1,+1],[-1,+1] over the entire framebuffer.
    TimeWarpFactor:Single;  ///< Lerp factor between time-warp matrices. Can be encoded in Pos.z.
    VignetteFactor:Single;  ///< Vignette fade factor. Can be encoded in Pos.w.
    TanEyeAnglesR:ovrVector2f ;   ///< The tangents of the horizontal and vertical eye angles for the red channel.
	  TanEyeAnglesG:ovrVector2f ;   ///< The tangents of the horizontal and vertical eye angles for the green channel.
	  TanEyeAnglesB:ovrVector2f ;   ///< The tangents of the horizontal and vertical eye angles for the blue channel.
End;

ovrDistortionVertexArray = Array[0..9999] Of ovrDistortionVertex;
PovrDistortionVertexArray = ^ovrDistortionVertexArray;

/// Describes a full set of distortion mesh data, filled in by ovrHmd_CreateDistortionMesh.
/// Contents of this data structure, if not null, should be freed by ovrHmd_DestroyDistortionMesh.
PovrDistortionMesh=^ovrDistortionMesh;
ovrDistortionMesh = Packed Record
    pVertexData:PovrDistortionVertexArray; ///< The distortion vertices representing each point in the mesh.
    pIndexData:PWordArray;  ///< Indices for connecting the mesh vertices into polygons.
    VertexCount:Cardinal; ///< The number of vertices in the mesh.
    IndexCount:Cardinal;  ///< The number of indices in the mesh.
End;

/// Generate distortion mesh per eye.
/// Distortion capabilities will depend on 'distortionCaps' flags. Users should
/// render using the appropriate shaders based on their settings.
/// Distortion mesh data will be allocated and written into the ovrDistortionMesh data structure,
/// which should be explicitly freed with ovrHmd_DestroyDistortionMesh.
/// Users should call ovrHmd_GetRenderScaleAndOffset to get uvScale and Offset values for rendering.
/// The function shouldn't fail unless theres is a configuration or memory error, in which case
/// ovrDistortionMesh values will be set to null.
/// This is the only function in the SDK reliant on eye relief, currently imported from profiles,
/// or overridden here.
Function ovrHmd_CreateDistortionMesh(hmd: ovrHmd; eyeType:ovrEyeType; fov:ovrFovPort;
                                                 distortionCaps:Cardinal;
                                                 meshData:ovrDistortionMesh):ovrBool; CDecl; External OVR_LIB_NAME;
                                                 
Function ovrHmd_CreateDistortionMeshDebug(hmddesc:ovrHmd;
                                                     eyeType:ovrEyeType; fov:ovrFovPort;
                                                     distortionCaps:Cardinal;
                                                     meshData:PovrDistortionMesh;
												     debugEyeReliefOverrideInMetres:Single):ovrBool; CDecl; External OVR_LIB_NAME;


/// Used to free the distortion mesh allocated by ovrHmd_GenerateDistortionMesh. meshData elements
/// are set to null and zeroes after the call.
Procedure ovrHmd_DestroyDistortionMesh(meshData:PovrDistortionMesh); CDecl; External OVR_LIB_NAME;

/// Computes updated 'uvScaleOffsetOut' to be used with a distortion if render target size or
/// viewport changes after the fact. This can be used to adjust render size every frame if desired.
Procedure ovrHmd_GetRenderScaleAndOffset(fov:ovrFovPort; textureSize:ovrSizei; renderViewport:ovrRecti; uvScaleOffsetOut:PovrVector2fArray); CDecl; External OVR_LIB_NAME;

/// Thread-safe timing function for the main thread. Caller should increment frameIndex
/// with every frame and pass the index where applicable to functions called on the
/// rendering thread.
Function ovrHmd_GetFrameTiming(hmd:ovrHmd; frameIndex:Cardinal):ovrFrameTiming; CDecl; External OVR_LIB_NAME;

/// Called at the beginning of the frame on the rendering thread.
/// Pass frameIndex == 0 if ovrHmd_GetFrameTiming isn't being used. Otherwise,
/// pass the same frame index as was used for GetFrameTiming on the main thread.
Function ovrHmd_BeginFrameTiming(hmd:ovrHmd; frameIndex:Cardinal):ovrFrameTiming; CDecl; External OVR_LIB_NAME;

/// Marks the end of client distortion rendered frame, tracking the necessary timing information.
/// This function must be called immediately after Present/SwapBuffers + GPU sync. GPU sync is
/// important before this call to reduce latency and ensure proper timing.
Procedure ovrHmd_EndFrameTiming(hmd:ovrHmd); CDecl; External OVR_LIB_NAME;

/// Initializes and resets frame time tracking. This is typically not necessary, but
/// is helpful if game changes vsync state or video mode. vsync is assumed to be on if this
/// isn't called. Resets internal frame index to the specified number.
Procedure ovrHmd_ResetFrameTiming(hmd:ovrHmd; frameIndex:Cardinal); CDecl; External OVR_LIB_NAME;

/// Computes timewarp matrices used by distortion mesh shader, these are used to adjust
/// for head orientation change since the last call to ovrHmd_GetEyePoses
/// when rendering this eye. The ovrDistortionVertex::TimeWarpFactor is used to blend between the
/// matrices, usually representing two different sides of the screen.
/// Must be called on the same thread as ovrHmd_BeginFrameTiming.
Procedure ovrHmd_GetEyeTimewarpMatrices(hmd:ovrHmd; eye:ovrEyeType; renderPose:ovrPosef; twmOut:PovrMatrix4fArray); CDecl; External OVR_LIB_NAME;

Procedure ovrHmd_GetEyeTimewarpMatricesDebug(hmd:ovrHmd; eye:ovrEyeType; renderPose:ovrPosef; twmOut:PovrMatrix4fArray;
													   debugTimingOffsetInSeconds:Double); CDecl; External OVR_LIB_NAME;


//-------------------------------------------------------------------------------------
// ***** Stateless math setup functions

/// Used to generate projection from ovrEyeDesc::Fov.
Function ovrMatrix4f_Projection(fov:ovrFovPort; znear, zfar:Single; rightHanded:ovrBool):ovrMatrix4f; CDecl; External OVR_LIB_NAME;

/// Used for 2D rendering, Y is down
/// orthoScale = 1.0f / pixelsPerTanAngleAtCenter
/// orthoDistance = distance from camera, such as 0.8m
Function ovrMatrix4f_OrthoSubProjection(projection:ovrMatrix4f; orthoScale:ovrVector2f; orthoDistance:Single; hmdToEyeViewOffsetX:Single):ovrMatrix4f; CDecl; External OVR_LIB_NAME;

/// Returns global, absolute high-resolution time in seconds. This is the same
/// value as used in sensor messages.
Function ovr_GetTimeInSeconds():Double; CDecl; External OVR_LIB_NAME;

/// Waits until the specified absolute time.
Function ovr_WaitTillTime(absTime:double):double; CDecl; External OVR_LIB_NAME;

// -----------------------------------------------------------------------------------
// ***** Latency Test interface

/// Does latency test processing and returns 'TRUE' if specified rgb color should
/// be used to clear the screen.
Function ovrHmd_ProcessLatencyTest(hmd:ovrHmd; rgbColorOut:ovrRGB):ovrBool; CDecl; External OVR_LIB_NAME;

/// Returns non-null string once with latency test result, when it is available.
/// Buffer is valid until next call.
Function ovrHmd_GetLatencyTestResult(hmd:ovrHmd):PAnsiChar; CDecl; External OVR_LIB_NAME;

/// Returns the latency testing color in rgbColorOut to render when using a DK2
/// Returns false if this feature is disabled or not-applicable (e.g. using a DK1)
Function ovrHmd_GetLatencyTest2DrawColor(hmddesc:ovrHmd; rgbColorOut:ovrRGB):ovrBool; CDecl; External OVR_LIB_NAME;

//-------------------------------------------------------------------------------------
// ***** Health and Safety Warning Display interface
//

/// Used by ovrhmd_GetHSWDisplayState to report the current display state.
Type
 ovrHSWDisplayState = Packed Record
    /// If true then the warning should be currently visible
    /// and the following variables have meaning. Else there is no
    /// warning being displayed for this application on the given HMD.
    Displayed:ovrBool;       ///< True if the Health&Safety Warning is currently displayed.
    StartTime:double;       ///< Absolute time when the warning was first displayed. See ovr_GetTimeInSeconds().
    DismissibleTime:double; ///< Earliest absolute time when the warning can be dismissed. May be a time in the past.
  End;
  PovrHSWDisplayState = ^ovrHSWDisplayState;

/// Returns the current state of the HSW display. If the application is doing the rendering of
/// the HSW display then this function serves to indicate that the warning should be
/// currently displayed. If the application is using SDK-based eye rendering then the SDK by
/// default automatically handles the drawing of the HSW display. An application that uses
/// application-based eye rendering should use this function to know when to start drawing the
/// HSW display itself and can optionally use it in conjunction with ovrhmd_DismissHSWDisplay
/// as described below.
///
/// Example usage for application-based rendering:
///    bool HSWDisplayCurrentlyDisplayed = false; // global or class member variable
///    ovrHSWDisplayState hswDisplayState;
///    ovrhmd_GetHSWDisplayState(Hmd, &hswDisplayState);
///
///    if (hswDisplayState.Displayed && !HSWDisplayCurrentlyDisplayed) {
///        <insert model into the scene that stays in front of the user>
///        HSWDisplayCurrentlyDisplayed = true;
///    }
Procedure ovrHmd_GetHSWDisplayState(hmd:ovrHmd; hasWarningState:PovrHSWDisplayState); CDecl; External OVR_LIB_NAME;

/// Dismisses the HSW display if the warning is dismissible and the earliest dismissal time
/// has occurred. Returns true if the display is valid and could be dismissed. The application
/// should recognize that the HSW display is being displayed (via ovrhmd_GetHSWDisplayState)
/// and if so then call this function when the appropriate user input to dismiss the warning
/// occurs.
///
/// Example usage :
///    void ProcessEvent(int key) {
///        if (key == escape) {
///            ovrHSWDisplayState hswDisplayState;
///            ovrhmd_GetHSWDisplayState(hmd, &hswDisplayState);
///
///            if (hswDisplayState.Displayed && ovrhmd_DismissHSWDisplay(hmd)) {
///                <remove model from the scene>
///                HSWDisplayCurrentlyDisplayed = false;
///            }
///        }
///    }
Function ovrHmd_DismissHSWDisplay(hmd:ovrHmd ):ovrBool ; CDecl; External OVR_LIB_NAME;

/// Get boolean property. Returns first element if property is a boolean array.
/// Returns defaultValue if property doesn't exist.
Function ovrHmd_GetBool(hmd:ovrHmd; propertyName:PAnsiChar; defaultVal:ovrBool):ovrBool; CDecl; External OVR_LIB_NAME;

/// Modify bool property; false if property doesn't exist or is readonly.
Function ovrHmd_SetBool(hmd:ovrHmd; propertyName:PAnsiChar; value:ovrBool ):ovrBool; CDecl; External OVR_LIB_NAME;

/// Get integer property. Returns first element if property is an integer array.
/// Returns defaultValue if property doesn't exist.
Function ovrHmd_GetInt(hmd:ovrHmd; propertyName:PAnsiChar; defaultVal:Integer):Integer; CDecl; External OVR_LIB_NAME;

/// Modify integer property; false if property doesn't exist or is readonly.
Function ovrHmd_SetInt(hmd:ovrHmd; propertyName:PAnsiChar; value:Integer):ovrBool; CDecl; External OVR_LIB_NAME;

/// Get float property. Returns first element if property is a float array.
/// Returns defaultValue if property doesn't exist.
Function ovrHmd_GetFloat(hmd:ovrHmd; propertyName:PAnsiChar; defaultVal:Single):Single; CDecl; External OVR_LIB_NAME;

/// Modify float property; false if property doesn't exist or is readonly.
Function ovrHmd_SetFloat(hmd:ovrHmd; propertyName:PAnsiChar; value:Single):ovrBool; CDecl; External OVR_LIB_NAME;

/// Get float[] property. Returns the number of elements filled in, 0 if property doesn't exist.
/// Maximum of arraySize elements will be written.
Function ovrHmd_GetFloatArray(hmd:ovrHmd; propertyName:PAnsiChar; values:PSingle; arraySize:Cardinal):Cardinal; CDecl; External OVR_LIB_NAME;

/// Modify float[] property; false if property doesn't exist or is readonly.
Function ovrHmd_SetFloatArray(hmd:ovrHmd; propertyName:PAnsiChar; values:PSingle; arraySize:Cardinal):ovrBool; CDecl; External OVR_LIB_NAME;

/// Get string property. Returns first element if property is a string array.
/// Returns defaultValue if property doesn't exist.
/// String memory is guaranteed to exist until next call to GetString or GetStringArray, or HMD is destroyed.
Function ovrHmd_GetString(hmd:ovrHmd; propertyName:PAnsiChar; defaultVal:PAnsiChar):PAnsiChar; CDecl; External OVR_LIB_NAME;

/// Set string property
Function ovrHmd_SetString(hmd:ovrHmddesc; propertyName:PAnsiChar; value:PAnsiChar):ovrBool; CDecl; External OVR_LIB_NAME;

// -----------------------------------------------------------------------------------
// ***** Logging

/// Start performance logging. guid is optional and if included is written with each file entry.
/// If called while logging is already active with the same filename, only the guid will be updated
/// If called while logging is already active with a different filename, ovrHmd_StopPerfLog() will be called, followed by ovrHmd_StartPerfLog()
Function ovrHmd_StartPerfLog(hmd:ovrHmd; fileName, userData1:PAnsiChar):ovrBool; CDecl; External OVR_LIB_NAME;
/// Stop performance logging.
Function ovrHmd_StopPerfLog(hmd:ovrHmd):ovrBool; CDecl; External OVR_LIB_NAME;

Implementation


End.