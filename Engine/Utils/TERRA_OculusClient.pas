Unit TERRA_OculusClient;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Client, TERRA_Vector3D, TERRA_Matrix4x4, TERRA_RenderTarget, libovr;

Type
  OculusClient = Class(AppClient)
    Protected
      _hmd:ovrHmd;
      _Initialized:Boolean;
      _Windowed:Boolean;

      _EyeBuffers:Array[0..1] Of RenderTarget;
      _EyeSize:Array[0..1] Of ovrSizei;
      _EyeViewport:Array[0..1] Of ovrRecti;
      _EyeRenderPose:Array[0..1] Of ovrPosef;

      _EyeRenderDesc:ovrEyeRenderDescArray;

      _useHmdToEyeViewOffset:Array[0..1] Of ovrVector3f;

      Procedure InitHMD();
      Procedure InitBuffers();

    Public
      Function GetWidth:Word; Override;
      Function GetHeight:Word; Override;
      Function GetFullScreen:Boolean; Override;

      Procedure OnFrameBegin(); Override;
      Procedure OnFrameEnd(); Override;

      Function GetVRProjectionMatrix(Eye:Integer; FOV, Ratio, zNear, zFar:Single):Matrix4x4; Override;

      Procedure Release; Override;
  End;

Implementation
Uses TERRA_Log, TERRA_Application, TERRA_OS, TERRA_GraphicsManager;


{ OculusClient }
Procedure OculusClient.InitHMD;
Begin
  ovr_Initialize();
  _HMD := ovrHmd_Create(0);

  If (_HMD = Nil) Then
  Begin
    Log(logError, 'VR', 'Oculus Rift not detected.');
    Exit;
  End;

  If (_HMD.ProductName = Nil) Or (_HMD.ProductName^=#0) Then
  Begin
    Log(logError, 'VR', 'Rift detected, but the display is not enabled.');
    _HMD := Nil;
    Exit;
  End Else
    Log(logDebug, 'VR', 'VR detected: '+_HMD.ProductName);

  // Setup Window and Graphics - use window frame if relying on Oculus driver
  _Windowed := ((_HMD.HmdCaps And ovrHmdCap_ExtendDesktop) = 0);
End;


Procedure OculusClient.InitBuffers;
Var
  cfg:ovrGLConfig;
  Caps:Cardinal;
  Eye:Integer;
  idealSize:ovrSizei;
  Handle:Cardinal;
Begin
  Handle := Application.Instance.Handle;
  ovrHmd_AttachToWindow(_HMD, Handle, Nil, Nil);
  ovrHmd_SetEnabledCaps(_HMD, ovrHmdCap_LowPersistence Or ovrHmdCap_DynamicPrediction);

  // Start the sensor which informs of the Rift's pose and motion
  ovrHmd_ConfigureTracking(_HMD, ovrTrackingCap_Orientation Or ovrTrackingCap_MagYawCorrection Or ovrTrackingCap_Position, 0);

  FillChar(cfg, SizeOf(Cfg), 0);
  cfg.Header.API := ovrRenderAPI_OpenGL;
  cfg.Header.BackBufferSize.w := _HMD.Resolution.w;
  cfg.Header.BackBufferSize.h := _HMD.Resolution.h;
  cfg.Header.Multisample := 0;
  cfg.Window := Handle;
  cfg.DC := WindowsApplication(Application.Instance).HDC;

  Caps := ovrDistortionCap_Chromatic Or ovrDistortionCap_Vignette Or ovrDistortionCap_TimeWarp Or ovrDistortionCap_Overdrive;

  If (Not ovrHmd_ConfigureRendering(_HMD, @cfg, Caps, @_HMD.DefaultEyeFov[0], @_EyeRenderDesc)) Then
  Begin
    Log(logError, 'VR', 'Error configuring rift rendering');
    _HMD := Nil;
    Exit;
  End;

  // Make the eye render buffers (caution if actual size < requested due to HW limits).
  For Eye:=0 To 1 Do
  Begin
    idealSize := ovrSizei(ovrHmd_GetFovTextureSize(_HMD, ovrEyeType(eye), _HMD.DefaultEyeFov[eye], 1.0));
    _EyeBuffers[Eye] := CreateRenderTarget('VR_Eye'+IntToString(Eye), idealSize.w, idealSize.h, True, True);
    _EyeSize[Eye] := IdealSize;
    _EyeViewport[Eye].Pos.x := 0;
    _EyeViewport[Eye].Pos.y := 0;
    _EyeViewport[Eye].Size.w := _EyeBuffers[Eye].Width;
    _EyeViewport[Eye].Size.h := _EyeBuffers[Eye].Height;
  End;
End;

Procedure OculusClient.Release;
Var
  I:Integer;
Begin
  If Assigned(_HMD) Then
  Begin
    ovrHmd_Destroy(_HMD);
    _HMD := Nil;

    For I:=0 To 1 Do
      ReleaseObject(_EyeBuffers[I]);
  End;

  ovr_Shutdown();
End;

Procedure OculusClient.OnFrameBegin();
Begin
  If _HMD = Nil Then
    Exit;

  GraphicsManager.Instance.MainViewport.VR := True;
 // GraphicsManager.Instance.MainViewport.Camera.Near := _HMD.CameraFrustumNearZInMeters * 10;
//  GraphicsManager.Instance.MainViewport.Camera.Far := _HMD.CameraFrustumFarZInMeters * 10;

  _useHmdToEyeViewOffset[0] := _EyeRenderDesc[0].HmdToEyeViewOffset;
  _useHmdToEyeViewOffset[1] := _EyeRenderDesc[1].HmdToEyeViewOffset;

  ovrHmd_BeginFrame(_HMD, 0);


  ovrHmd_GetEyePoses(_HMD, 0, @_useHmdToEyeViewOffset[0], @_EyeRenderPose[0], Nil);
End;

Procedure OculusClient.OnFrameEnd();
Var
  Eye:Integer;
  Textures:Array[0..1] Of ovrGLTexture;
Begin
  If _HMD = Nil Then
    Exit;

  If (_EyeBuffers[0] = Nil) Then
    Self.InitBuffers();

  For Eye:=0 To 1 Do
  Begin
    Textures[Eye].Header.API := ovrRenderAPI_OpenGL;
    Textures[Eye].Header.TextureSize := _EyeSize[Eye];
    Textures[Eye].Header.RenderViewport := _EyeViewport[Eye];
    Textures[Eye].TexId := _EyeBuffers[Eye].GetHandle(0);
  End;

  ovrHmd_EndFrame(_HMD, @_EyeRenderPose[0], @Textures[0]);
End;

Function CreateOculusProjection(Const tanHalfFov:ovrFovPort; zNear, zFar:Single; rightHanded:Boolean):Matrix4x4;
Var
  projXScale, projYScale:Single;
  projXOffset, projYOffset:Single;
  handednessScale:Single;
Begin
    projXScale := 2.0 / ( tanHalfFov.LeftTan + tanHalfFov.RightTan );
    projXOffset := ( tanHalfFov.LeftTan - tanHalfFov.RightTan ) * projXScale * 0.5;
    projYScale := 2.0/ ( tanHalfFov.UpTan + tanHalfFov.DownTan );
    projYOffset := ( tanHalfFov.UpTan - tanHalfFov.DownTan ) * projYScale * 0.5;

    // Hey - why is that Y.Offset negated?
    // It's because a projection matrix transforms from world coords with Y=up,
    // whereas this is from NDC which is Y=down.

    If (rightHanded) Then
      handednessScale := -1.0
    Else
      handednessScale := 1.0;

    // Produces X result, mapping clip edges to [-w,+w]
    Result.V[0] := projXScale;
    Result.V[4] := 0.0;
    Result.V[8] := handednessScale * projXOffset;
    Result.V[12] := 0.0;

    // Produces Y result, mapping clip edges to [-w,+w]
    // Hey - why is that YOffset negated?
    // It's because a projection matrix transforms from world coords with Y=up,
    // whereas this is derived from an NDC scaling, which is Y=down.
    Result.V[1] := 0.0;
    Result.V[5] := projYScale;
    Result.V[9] := handednessScale * -projYOffset;
    Result.V[13] := 0.0;

    // Produces Z-buffer result - app needs to fill this in with whatever Z range it wants.
    // We'll just use some defaults for now.
    Result.V[2] := 0.0;
    Result.V[6] := 0.0;
    Result.V[10] := -handednessScale * zFar / (zNear - zFar);
    Result.V[14] := (zFar * zNear) / (zNear - zFar);

    // Produces W result (= Z in)
    Result.V[3] := 0.0;
    Result.V[7] := 0.0;
    Result.V[11] := handednessScale;
    Result.V[15] := 0.0;
End;

Function OculusClient.GetVRProjectionMatrix(Eye: Integer; FOV, Ratio, zNear, zFar: Single): Matrix4x4;
Begin
  //Result := Matrix4x4(ovrMatrix4f_Projection({_EyeRenderDesc[Eye].Fov}_HMD.DefaultEyeFov[Eye], zNear, zFar, False));
  Result := CreateOculusProjection({_EyeRenderDesc[Eye].Fov}_HMD.DefaultEyeFov[Eye], zNear, zFar, True);
End;

Function OculusClient.GetFullScreen: Boolean;
Begin
  Result := Not Self._Windowed;
End;

Function OculusClient.GetWidth: Word;
Begin
  If _HMD = Nil Then
    Self.InitHMD();

  If Assigned(_HMD) Then
    Result := _HMD.Resolution.w
  Else
    Result := 640;
End;

Function OculusClient.GetHeight: Word;
Begin
  If Assigned(_HMD) Then
    Result := _HMD.Resolution.h
  Else
    Result := 480;
End;

End.
