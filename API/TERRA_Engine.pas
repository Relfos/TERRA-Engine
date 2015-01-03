{**************************************************
*
*   TERRA ENGINE - $BUILD
*   www.pascalgameengine.com
*   https://github.com/Relfos/TERRA-Engine
*   Sérgio Flores (relfos@gmail)
*
*   3/1/2015
**************************************************}

Unit TERRA_Engine;
{$I terra.inc}

Interface

{$IFDEF IPHONE}
{$DEFINE STATIC_LINKING}
{$LINKLIB terra_engine}
{$ENDIF}

{$IFNDEF STATIC_LINKING}
Const
{$IFDEF WINDOWS}
  TERRA_LIB = 'terra_engine.dll';
{$ENDIF}

{$IFDEF LINUX}
  TERRA_LIB = 'terra_engine.so';
{$ENDIF}

{$IFDEF OSX}
  TERRA_LIB = 'terra_engine.dylib';
{$ENDIF}

{$ENDIF}

{$I TERRA_types.inc}

Type
  WidgetEventHandler = Procedure (Widget:TERRAWidget); Cdecl;

  PColor = ^Color;
  Color=Packed Object
    R:Byte;
    G:Byte;
    B:Byte;
    A:Byte;
  End;

  PVector2D=^Vector2D;
  Vector2D=Packed Object
    X,Y:Single;
  End;

  PVector3D = ^Vector3D;
  Vector3D = Packed Object
    X:Single;
    Y:Single;
    Z:Single;
  End;

  PVector4D = ^Vector4D;
  Vector4D = Packed Object
    X:Single;
    Y:Single;
    Z:Single;
    W:Single;
  End;

  PMatrix3x3 = ^Matrix3x3;
  Matrix3x3=Packed Object
    V:Array [0..8] Of Single;
  End;

  PMatrix4x4 = ^Matrix4x4;
  Matrix4x4=Packed Object
    V:Array [0..15] Of Single;
  End;

  PRay=^Ray;
  Ray = Packed Object
    Origin:Vector3D;
    Direction:Vector3D;
  End;

  BoundingBox = Packed Object
    StartVertex:Vector3D;
    EndVertex:Vector3D;
  End;

  Plane = Packed Object
    A,B,C,D:Single;
  End;

  Frustum = Packed Object
    Planes:Array[0..5]Of Plane;
  End;

Const
  Epsilon = 0.00001;

  Rad   = 0.017453292519;   // Pi/180
  Deg   = 57.29577951308;   // 180/Pi

// Color constants
  ColorWhite:Color=(R:255; G:255; B:255; A:255);
  ColorBlack:Color=(R:0; G:0; B:0; A:255);
  ColorNull:Color=(R:0; G:0; B:0; A:0);

  ColorRed:Color=(R:255; G:0; B:0; A:255);
  ColorBlue:Color=(R:0; G:0; B:255; A:255);
  ColorGreen:Color=(R:0; G:255; B:0; A:255);
  ColorYellow:Color=(R:255; G:255; B:0; A:255);

  blendNone     = 0;
  blendBlend    = 1;
  blendAdd      = 2;         //GL_ONE  GL_ONE
  blendFilter   = 3;      //GL_DST_COLOR GL_ZERO
  blendModulate = 4;
  blendJoin     = 5;

  tweenByte     = 0;
  tweenFloat    = 1;

  shaderSkinning    = 1;
  shaderSpecular    = 2;
  shaderLightMap    = 4;
  shaderSpecularMap = 8;
  shaderNormalMap   = 16;
  shaderReflective  = 32;
  shaderColorRamp   = 64;
  shaderFog         = 128;
  shaderAlphaTest   = 256;
  shaderAddSigned   = 512;

  camDirForward   = 0;
  camDirBackward  = 1;
  camDirLeft      = 2;
  camDirRight     = 3;

  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;

  textureFilterPoint = 0;
  textureFilterBilinear = 1;

  widgetAnimateAlpha  = 1;
  widgetAnimatePosX   = 2;
  widgetAnimatePosY   = 4;
  widgetAnimateRotation = 8;
  widgetAnimateScale    = 16;
  widgetAnimateSaturation  = 32;
  widgetAnimatePosX_Bottom = 64;
  widgetAnimatePosY_Bottom = 128;

  // widget tween
  wtPositionX   = 1;
  wtPositionY   = 2;
  wtColorRed    = 3;
  wtColorGreen  = 4;

  wtColorBlue   = 5;
  wtColorAlpha  = 6;
  wtRotation    = 7;
  wtScale       = 8;
  wtSaturation  = 9;

  // custom tweens
  wtValue       = 100;

  waTopLeft     = 0;
  waTopCenter   = 1;
  waTopRight    = 2;
  waLeftCenter  = 3;
  waCenter      = 4;
  waRightCenter = 5;
  waBottomLeft     = 6;
  waBottomCenter   = 7;
  waBottomRight    = 8;

  //  Search flags
  searchCanTimeOut  = 1;
  searchDiagonal    = 2;

  //  Search results
  searchComplete      = 0;
  searchTimeOut       = 1;
  searchLimitReached  = 2;
  searchFailed        = 3;

Procedure Settings_SetWidth(Width:Integer); External TERRA_LIB;
Procedure Settings_SetHeight(Height:Integer); External TERRA_LIB;
Procedure Settings_SetTitle(Title:PAnsiChar); External TERRA_LIB;
Procedure Settings_SetHandle(Handle:Integer); External TERRA_LIB;
Procedure Settings_SetFullScreen(Fullscreen:Boolean); External TERRA_LIB;
Procedure Settings_SetVSync(Value:Boolean); External TERRA_LIB;
Procedure Settings_SetAntialias(Samples:Integer); External TERRA_LIB;
Procedure Settings_SetLogging(Value:Boolean); External TERRA_LIB;
Procedure Scene_SetOnRenderShadowCasters(Callback:TERRASceneCallback); External TERRA_LIB;
Procedure Scene_SetOnRenderViewport(Callback:TERRASceneCallback); External TERRA_LIB;
Procedure Scene_SetOnRenderSprites(Callback:TERRASceneCallback); External TERRA_LIB;
Procedure Scene_SetOnRenderSky(Callback:TERRASceneCallback); External TERRA_LIB;
Procedure TERRA_SetOnError(Callback:TERRAErrorCallback); External TERRA_LIB;
Procedure TERRA_SetOnCreate(Callback:TERRAEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnDestroy(Callback:TERRAEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnIdle(Callback:TERRAEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnMouseDown(Callback:TERRAMouseEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnMouseUp(Callback:TERRAMouseEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnMouseMove(Callback:TERRAMouseMoveCallback); External TERRA_LIB;
Procedure TERRA_SetOnMouseWheel(Callback:TERRAMouseWheelCallback); External TERRA_LIB;
Procedure TERRA_SetOnKeyDown(Callback:TERRAKeyEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnKeyUp(Callback:TERRAKeyEventCallback); External TERRA_LIB;
Procedure TERRA_SetOnAccelerometer(Callback:TERRAAccelerometerCallback); External TERRA_LIB;
Procedure TERRA_SetOnStateChange(Callback:TERRAStateChangeCallback); External TERRA_LIB;
Procedure TERRA_Run(); External TERRA_LIB;
Procedure TERRA_Init(); External TERRA_LIB;
Procedure TERRA_Shutdown(); External TERRA_LIB;
Procedure TERRA_Resize(W:Integer; H:Integer); External TERRA_LIB;
Procedure TERRA_Log(Desc:PAnsiChar; LogType:Integer); External TERRA_LIB;
Function TERRA_GetTime():Cardinal; External TERRA_LIB;
Function TERRA_GetWidth():Cardinal; External TERRA_LIB;
Function TERRA_GetHeight():Cardinal; External TERRA_LIB;
Procedure TERRA_Pause(Pause:Boolean); External TERRA_LIB;
Function TERRA_Paused():Boolean; External TERRA_LIB;
Procedure TERRA_EnableAds(); External TERRA_LIB;
Procedure TERRA_SendAnalytics(Event:PAnsiChar; Value:PAnsiChar); External TERRA_LIB;
Function Localization_GetLanguage():PAnsiChar; External TERRA_LIB;
Procedure Localization_SetLanguage(Lang:PAnsiChar); External TERRA_LIB;
Function Localization_GetString(ID:PAnsiChar):PAnsiChar; External TERRA_LIB;
Procedure Input_SetKeyState(Key:Integer; State:Boolean); External TERRA_LIB;
Function Input_KeyDown(Key:Integer):Boolean; External TERRA_LIB;
Function Input_KeyPressed(Key:Integer):Boolean; External TERRA_LIB;
Function Input_GetMousePosition():Vector2D; External TERRA_LIB;
Function Stream_CreateBuffer(Size:Integer; Ptr:Pointer):TERRAStream; External TERRA_LIB;
Function Stream_Create(Name:PAnsiChar):TERRAStream; External TERRA_LIB;
Function Stream_Open(Name:PAnsiChar):TERRAStream; External TERRA_LIB;
Function Stream_EOF(MyStream:TERRAStream):Boolean; External TERRA_LIB;
Procedure Stream_Destroy(MyStream:TERRAStream); External TERRA_LIB;
Function Stream_ReadLine(MyStream:TERRAStream):PAnsiChar; External TERRA_LIB;
Function Stream_Read(MyStream:TERRAStream; Data:Pointer; Size:Integer):Integer; External TERRA_LIB;
Function Stream_Write(MyStream:TERRAStream; Data:Pointer; Size:Integer):Integer; External TERRA_LIB;
Procedure Stream_Seek(MyStream:TERRAStream; Position:Integer); External TERRA_LIB;
Function Stream_GetSize(MyStream:TERRAStream):Integer; External TERRA_LIB;
Function Stream_GetPosition(MyStream:TERRAStream):Integer; External TERRA_LIB;
Function AssetManager_Search(FileName:PAnsiChar):PAnsiChar; External TERRA_LIB;
Procedure AssetManager_AddPath(Path:PAnsiChar); External TERRA_LIB;
Procedure AssetManager_RemovePath(Path:PAnsiChar); External TERRA_LIB;
Function AssetManager_GetTexture(Name:PAnsiChar):Pointer; External TERRA_LIB;
Function AssetManager_GetSound(Name:PAnsiChar):Pointer; External TERRA_LIB;
Function AssetManager_GetMesh(Name:PAnsiChar):Pointer; External TERRA_LIB;
Function AssetManager_GetFont(Name:PAnsiChar):Pointer; External TERRA_LIB;
Procedure AssetManager_Prefetch(MyResource:Pointer); External TERRA_LIB;
Procedure AssetManager_Unload(MyResource:Pointer); External TERRA_LIB;
Function Session_Create(FileName:PAnsiChar):TERRASession; External TERRA_LIB;
Procedure Session_Save(MySession:TERRASession); External TERRA_LIB;
Procedure Session_SetValue(MySession:TERRASession; Key:PAnsiChar; Value:PAnsiChar); External TERRA_LIB;
Function Session_GetValue(MySession:TERRASession; Key:PAnsiChar):PAnsiChar; External TERRA_LIB;
Function Font_GetTextWidth(MyFont:TERRAFont; Text:PAnsiChar):Single; External TERRA_LIB;
Function Font_GetTextHeight(MyFont:TERRAFont; Text:PAnsiChar):Single; External TERRA_LIB;
Function Font_GetTextRect(MyFont:TERRAFont; Text:PAnsiChar):Vector2D; External TERRA_LIB;
Procedure Font_DrawText(MyFont:TERRAFont; X:Single; Y:Single; Layer:Single; Text:PAnsiChar; MyColor:Color); External TERRA_LIB;
Function Shader_GetFromFactory(FxFlags:Cardinal; OutFlags:Cardinal; FogFlags:Cardinal; LightModel:Integer; DirLightCount:Integer; PointLightCount:Integer; SpotLightCount:Integer):TERRAShader; External TERRA_LIB;
Procedure Shader_Bind(MyShader:TERRAShader); External TERRA_LIB;
Procedure Shader_SetDefaultCameraUniforms(); External TERRA_LIB;
Function Shader_GetAttribute(MyShader:TERRAShader; AttribName:PAnsiChar):Integer; External TERRA_LIB;
Procedure Shader_SetFloat(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Single); External TERRA_LIB;
Procedure Shader_SetVector(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Vector3D); External TERRA_LIB;
Procedure Shader_SetColor(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Color); External TERRA_LIB;
Procedure Shader_SetMatrix(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Matrix4x4); External TERRA_LIB;
Procedure Texture_Bind(Tex:TERRATexture; Slot:Integer); External TERRA_LIB;
Function Texture_GetWidth(Tex:TERRAImage):Integer; External TERRA_LIB;
Function Texture_GetHeight(Tex:TERRAImage):Integer; External TERRA_LIB;
Function Texture_Create(Name:PAnsiChar; Width:Integer; Height:Integer):TERRATexture; External TERRA_LIB;
Procedure Texture_UpdateRect(Tex:TERRATexture; Source:TERRAImage; X:Integer; Y:Integer); External TERRA_LIB;
Function Texture_GetImage(Tex:TERRATexture):TERRAImage; External TERRA_LIB;
Procedure Texture_SetWrapMode(Tex:TERRATexture; Wrap:Boolean); External TERRA_LIB;
Procedure Texture_SetMipMapping(Tex:TERRATexture; UseMipmaps:Boolean); External TERRA_LIB;
Procedure Texture_SetFiltering(Tex:TERRATexture; Filtering:Integer); External TERRA_LIB;
Function Skybox_Create(Name:PAnsiChar):TERRASkybox; External TERRA_LIB;
Procedure Skybox_Render(Sky:TERRASkybox); External TERRA_LIB;
Procedure Skybox_Destroy(Sky:TERRASkybox); External TERRA_LIB;
Function MeshInstance_Create(MyMesh:TERRAMesh):TERRAMeshInstance; External TERRA_LIB;
Procedure MeshInstance_PlayAnimation(Instance:TERRAMeshInstance; AnimName:PAnsiChar); External TERRA_LIB;
Procedure MeshInstance_CrossFadeAnimation(Instance:TERRAMeshInstance; AnimName:PAnsiChar; Duration:Cardinal); External TERRA_LIB;
Procedure MeshInstance_SetDiffuseColor(Instance:TERRAMeshInstance; Index:Integer; MyColor:Color); External TERRA_LIB;
Procedure MeshInstance_SetDiffuseMap(Instance:TERRAMeshInstance; Index:Integer; Tex:TERRATexture); External TERRA_LIB;
Procedure MeshInstance_SetVisibility(Instance:TERRAMeshInstance; Index:Integer; Visible:Boolean); External TERRA_LIB;
Function MeshInstance_Intersect(Instance:TERRAMeshInstance; R:Ray; T:Single):Boolean; External TERRA_LIB;
Procedure MeshInstance_SetPosition(Instance:TERRAMeshInstance; Position:Vector3D); External TERRA_LIB;
Procedure MeshInstance_SetRotation(Instance:TERRAMeshInstance; Rotation:Vector3D); External TERRA_LIB;
Procedure MeshInstance_SetScale(Instance:TERRAMeshInstance; Scale:Vector3D); External TERRA_LIB;
Procedure MeshInstance_SetTransform(Instance:TERRAMeshInstance; Transform:Matrix4x4); External TERRA_LIB;
Procedure MeshInstance_Destroy(Instance:TERRAMeshInstance); External TERRA_LIB;
Function MeshInstance_GetGroup(Instance:TERRAMeshInstance; Name:PAnsiChar):TERRAMeshGroup; External TERRA_LIB;
Procedure MeshInstance_SetGroupLocalTransform(Instance:TERRAMeshInstance; Index:Integer; Transform:Matrix4x4); External TERRA_LIB;
Procedure MeshInstance_SetGroupGlobalTransform(Instance:TERRAMeshInstance; Index:Integer; Transform:Matrix4x4); External TERRA_LIB;
Function MeshInstance_GetBoundingBox(Instance:TERRAMeshInstance):BoundingBox; External TERRA_LIB;
Function MeshGroup_GetIndex(Group:TERRAMeshGroup):Integer; External TERRA_LIB;
Function MeshGroup_GetBoundingBox(Group:TERRAMeshGroup):BoundingBox; External TERRA_LIB;
Function Occluder_Create():TERRAOccluder; External TERRA_LIB;
Procedure Occluder_SetTransform(Occluder:TERRAOccluder; Transform:Matrix4x4; Width:Single; Height:Single); External TERRA_LIB;
Function Occluder_PointOccluded(Occluder:TERRAOccluder; P:Vector3D):Boolean; External TERRA_LIB;
Function Occluder_BoxOccluded(Occluder:TERRAOccluder; Box:BoundingBox):Boolean; External TERRA_LIB;
Procedure Occluder_Destroy(Occluder:TERRAOccluder); External TERRA_LIB;
Function Graphics_IsBoxVisible(Box:BoundingBox):Boolean; External TERRA_LIB;
Procedure Graphics_SetBackgroundColor(MyColor:Color); External TERRA_LIB;
Procedure Graphics_SetDynamicShadows(Enabled:Boolean); External TERRA_LIB;
Procedure Graphics_SetPostProcessing(Enabled:Boolean); External TERRA_LIB;
Procedure Graphics_SetNormalMapping(Enabled:Boolean); External TERRA_LIB;
Procedure Graphics_SetDepthOfField(Enabled:Boolean); External TERRA_LIB;
Procedure Graphics_SetSSAO(Enabled:Boolean); External TERRA_LIB;
Procedure Graphics_SetFogFlags(Flags:Cardinal); External TERRA_LIB;
Procedure Graphics_SetFogColor(MyColor:Color); External TERRA_LIB;
Procedure Graphics_SetFogDensity(Density:Single); External TERRA_LIB;
Function Graphics_StatsTriangleCount():Integer; External TERRA_LIB;
Function Graphics_StatsShaderSwitches():Integer; External TERRA_LIB;
Function Graphics_StatsDrawCalls():Integer; External TERRA_LIB;
Function Graphics_StatsLightCount():Integer; External TERRA_LIB;
Function Graphics_StatsOccluderCount():Integer; External TERRA_LIB;
Function Graphics_StatsRenderableCount():Integer; External TERRA_LIB;
Function Graphics_StatsFramesPerSecond():Integer; External TERRA_LIB;
Function Graphics_GetPickRay(View:TERRAViewport; X:Integer; Y:Integer):Ray; External TERRA_LIB;
Procedure Graphics_AddRenderable(MyRenderable:Pointer; Flags:Cardinal); External TERRA_LIB;
Procedure Graphics_AddOccluder(Occluder:TERRAOccluder); External TERRA_LIB;
Procedure Graphics_AddLight(Light:TERRALight); External TERRA_LIB;
Function Graphics_AddSprite(X:Single; Y:Single; Layer:Single; Tex:TERRATexture):TERRASprite; External TERRA_LIB;
Procedure Graphics_FlushSprites(); External TERRA_LIB;
Function Graphics_GetActiveCamera():TERRACamera; External TERRA_LIB;
Function Graphics_GetActiveViewport():TERRAViewport; External TERRA_LIB;
Function Graphics_ElapsedTime():Single; External TERRA_LIB;
Procedure Light_SetPriority(MyLight:TERRALight; Priority:Integer); External TERRA_LIB;
Procedure Light_SetColor(MyLight:TERRALight; MyColor:Color); External TERRA_LIB;
Procedure Light_SetStatic(MyLight:TERRALight; IsStatic:Boolean); External TERRA_LIB;
Function Light_CreateDirectional(Dir:Vector3D):TERRALight; External TERRA_LIB;
Function Light_CreatePoint(Position:Vector3D; Radius:Single):TERRALight; External TERRA_LIB;
Procedure Light_Destroy(P:TERRALight); External TERRA_LIB;
Procedure Debug_DrawBoundingBox(MyBox:BoundingBox; MyColor:Color); External TERRA_LIB;
Procedure Debug_DrawFrustum(MyFrustum:Frustum; MyColor:Color); External TERRA_LIB;
Procedure Debug_DrawRay(MyRay:Ray; MyColor:Color; Length:Single); External TERRA_LIB;
Procedure Debug_DrawPlane(Position:Vector3D; Normal:Vector3D; Scale:Single; MyColor:Color); External TERRA_LIB;
Function Camera_Create(Name:PAnsiChar):TERRACamera; External TERRA_LIB;
Procedure Camera_Destroy(Cam:TERRACamera); External TERRA_LIB;
Procedure Camera_Move(Cam:TERRACamera; Dir:Integer; Speed:Single); External TERRA_LIB;
Procedure Camera_Rotate(Cam:TERRACamera; X:Single; Y:Single); External TERRA_LIB;
Procedure Camera_Fly(Cam:TERRACamera); External TERRA_LIB;
Procedure Camera_SetPosition(Cam:TERRACamera; Position:Vector3D); External TERRA_LIB;
Procedure Camera_SetDirection(Cam:TERRACamera; Direction:Vector3D); External TERRA_LIB;
Procedure Camera_SetNearPlane(Cam:TERRACamera; Distance:Single); External TERRA_LIB;
Procedure Camera_SetFarPlane(Cam:TERRACamera; Distance:Single); External TERRA_LIB;
Procedure Camera_LookAt(Cam:TERRACamera; Position:Vector3D); External TERRA_LIB;
Function Camera_GetPosition(Cam:TERRACamera):Vector3D; External TERRA_LIB;
Function Camera_GetDirection(Cam:TERRACamera):Vector3D; External TERRA_LIB;
Function Camera_GetRight(Cam:TERRACamera):Vector3D; External TERRA_LIB;
Function Camera_GetProjection(Cam:TERRACamera):Matrix4x4; External TERRA_LIB;
Function Camera_GetTransform(Cam:TERRACamera):Matrix4x4; External TERRA_LIB;
Function Camera_GetUp(Cam:TERRACamera):Vector3D; External TERRA_LIB;
Function Image_Create(Width:Integer; Height:Integer):TERRAImage; External TERRA_LIB;
Function Image_Load(FileName:PAnsiChar):TERRAImage; External TERRA_LIB;
Procedure Image_Destroy(Img:TERRAImage); External TERRA_LIB;
Function Image_GetPixels(Image:TERRAImage):PColor; External TERRA_LIB;
Function Image_GetWidth(Image:TERRAImage):Integer; External TERRA_LIB;
Function Image_GetHeight(Image:TERRAImage):Integer; External TERRA_LIB;
Function Image_GetPixel(Image:TERRAImage; X:Integer; Y:Integer):Color; External TERRA_LIB;
Procedure Image_SetPixel(Image:TERRAImage; X:Integer; Y:Integer; Pixel:Color); External TERRA_LIB;
Function UI_Create():TERRAUI; External TERRA_LIB;
Procedure UI_Destroy(MyUI:TERRAUI); External TERRA_LIB;
Procedure UI_SetColor(MyUI:TERRAUI; MyColor:Color); External TERRA_LIB;
Procedure UI_SetFocus(MyUI:TERRAUI; W:TERRAWidget); External TERRA_LIB;
Procedure UI_Clear(MyUI:TERRAUI); External TERRA_LIB;
Function UI_GetLastWidget(MyUI:TERRAUI):TERRAWidget; External TERRA_LIB;
Function UI_GetWidget(MyUI:TERRAUI; Name:PAnsiChar):TERRAWidget; External TERRA_LIB;
Procedure UI_DeleteWidget(MyUI:TERRAUI; MyWidget:TERRAWidget); External TERRA_LIB;
Procedure UI_LoadCursor(MyUI:TERRAUI; Name:PAnsiChar); External TERRA_LIB;
Function UI_GetDefaultFont(MyUI:TERRAUI):TERRAFont; External TERRA_LIB;
Procedure UI_SetDefaultFont(MyUI:TERRAUI; MyFont:TERRAFont); External TERRA_LIB;
Function UI_HasTransition(MyUI:TERRAUI):Boolean; External TERRA_LIB;
Procedure UI_CreateSlideTransition(MyUI:TERRAUI; Direction:Vector2D; Duration:Cardinal; Delay:Cardinal); External TERRA_LIB;
Procedure UI_CreateFadeTransition(MyUI:TERRAUI; Tex:TERRATexture; Duration:Cardinal; Delay:Cardinal; Invert:Boolean); External TERRA_LIB;
Procedure UI_SetTransitionCallback(MyUI:TERRAUI; Callback:TERRAFadeCallback; UserData:Pointer; OnStart:Boolean); External TERRA_LIB;
Function UI_GetWidth():Integer; External TERRA_LIB;
Function UI_GetHeight():Integer; External TERRA_LIB;
Function UI_CreateWindow(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Width:Integer; Height:Integer):TERRAWidget; External TERRA_LIB;
Function UI_CreateButton(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar; CustomPNG:PAnsiChar):TERRAWidget; External TERRA_LIB;
Function UI_CreateEditText(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Width:Integer):TERRAWidget; External TERRA_LIB;
Function UI_CreateIcon(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Icon:PAnsiChar):TERRAWidget; External TERRA_LIB;
Function UI_CreateLabel(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; External TERRA_LIB;
Function UI_CreateCheckbox(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; External TERRA_LIB;
Function UI_CreateRadioButton(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; External TERRA_LIB;
Procedure UI_MessageBox(MyUI:TERRAUI; Msg:PAnsiChar; Callback:WidgetEventHandler); External TERRA_LIB;
Procedure UI_ChoiceBox(MyUI:TERRAUI; Msg:PAnsiChar; Btn1:PAnsiChar; Btn2:PAnsiChar; Callback1:WidgetEventHandler; Callback2:WidgetEventHandler); External TERRA_LIB;
Function Widget_AddTween(W:TERRAWidget; TweenType:Integer; TargetValue:Single; Time:Integer; Delay:Integer):TERRATween; External TERRA_LIB;
Procedure Widget_SetDragging(W:TERRAWidget; Value:Boolean); External TERRA_LIB;
Procedure Widget_SetScale(W:TERRAWidget; Scale:Single); External TERRA_LIB;
Procedure Widget_SetGroup(W:TERRAWidget; Group:Integer); External TERRA_LIB;
Procedure Widget_SetChecked(W:TERRAWidget; Value:Boolean); External TERRA_LIB;
Function Widget_IsChecked(W:TERRAWidget):Boolean; External TERRA_LIB;
Function Widget_GetFont(W:TERRAWidget):TERRAFont; External TERRA_LIB;
Procedure Widget_SetCaption(W:TERRAWidget; Caption:PAnsiChar); External TERRA_LIB;
Procedure Widget_SetText(W:TERRAWidget; Text:PAnsiChar); External TERRA_LIB;
Procedure Widget_SetOnClick(W:TERRAWidget; OnClick:WidgetEventHandler); External TERRA_LIB;
Procedure Widget_SetOnRelease(W:TERRAWidget; OnClick:WidgetEventHandler); External TERRA_LIB;
Procedure Widget_SetOnMouseOver(W:TERRAWidget; OnClick:WidgetEventHandler); External TERRA_LIB;
Procedure Widget_SetOnMouseOut(W:TERRAWidget; OnClick:WidgetEventHandler); External TERRA_LIB;
Procedure Widget_SetColor(W:TERRAWidget; MyColor:Color); External TERRA_LIB;
Procedure Widget_SetPosition(W:TERRAWidget; Pos:Vector2D); External TERRA_LIB;
Procedure Widget_SetVisible(W:TERRAWidget; Visible:Boolean); External TERRA_LIB;
Function Widget_ToggleVisibility(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal; EaseType:Integer; Duration:Integer):TERRATween; External TERRA_LIB;
Function Widget_Hide(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal; EaseType:Integer; Duration:Integer):TERRATween; External TERRA_LIB;
Function Widget_Show(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal; EaseType:Integer; Duration:Integer):TERRATween; External TERRA_LIB;
Function Widget_IsVisible(W:TERRAWidget):Boolean; External TERRA_LIB;
Function Widget_HasTweens(W:TERRAWidget):Boolean; External TERRA_LIB;
Function Widget_GetSize(W:TERRAWidget):Vector2D; External TERRA_LIB;
Function Widget_GetPosition(W:TERRAWidget):Vector2D; External TERRA_LIB;
Function Widget_GetColor(W:TERRAWidget):Color; External TERRA_LIB;
Function Widget_GetName(W:TERRAWidget):PAnsiChar; External TERRA_LIB;
Function Widget_GetCaption(W:TERRAWidget):PAnsiChar; External TERRA_LIB;
Function Widget_GetText(W:TERRAWidget):PAnsiChar; External TERRA_LIB;
Procedure Widget_SetAlign(W:TERRAWidget; AlignMode:Cardinal); External TERRA_LIB;
Procedure Widget_CenterOnPoint(W:TERRAWidget; X:Single; Y:Single); External TERRA_LIB;
Function Tween_Create(MyType:Integer; Data:Pointer; TargetValue:Single; UserData:Pointer):TERRATween; External TERRA_LIB;
Procedure Tween_SetEaseType(T:TERRATween; EaseType:Integer); External TERRA_LIB;
Procedure Tween_SetTime(T:TERRATween; Time:Cardinal); External TERRA_LIB;
Procedure Tween_SetDelay(T:TERRATween; Delay:Cardinal); External TERRA_LIB;
Procedure Tween_SetUserData(T:TERRATween; Data:Pointer); External TERRA_LIB;
Procedure Tween_SetCallback(T:TERRATween; Callback:TERRAFadeCallback); External TERRA_LIB;
Procedure Sprite_SetPosition(Spr:TERRASprite; X:Single; Y:Single); External TERRA_LIB;
Function Sprite_GetWidth(Spr:TERRASprite):Integer; External TERRA_LIB;
Function Sprite_GetHeight(Spr:TERRASprite):Integer; External TERRA_LIB;
Procedure Sprite_SetWidth(Spr:TERRASprite; Width:Integer); External TERRA_LIB;
Procedure Sprite_SetHeight(Spr:TERRASprite; Height:Integer); External TERRA_LIB;
Procedure Sprite_SetAnchor(Spr:TERRASprite; P:Vector2D); External TERRA_LIB;
Procedure Sprite_SetTransform(Spr:TERRASprite; Mat:Matrix3x3); External TERRA_LIB;
Procedure Sprite_SetTransformOnCenter(Spr:TERRASprite; Center:Vector2D; Mat:Matrix3x3); External TERRA_LIB;
Procedure Sprite_SetScaleOnCenter(Spr:TERRASprite; Center:Vector2D; ScaleX:Single; ScaleY:Single); External TERRA_LIB;
Procedure Sprite_SetScale(Spr:TERRASprite; ScaleX:Single; ScaleY:Single); External TERRA_LIB;
Procedure Sprite_SetUniformScale(Spr:TERRASprite; Scale:Single); External TERRA_LIB;
Procedure Sprite_SetRotation(Spr:TERRASprite; Angle:Single); External TERRA_LIB;
Procedure Sprite_SetScaleAndRotationOnCenter(Spr:TERRASprite; Center:Vector2D; ScaleX:Single; ScaleY:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetUniformScaleAndRotationOnCenter(Spr:TERRASprite; Center:Vector2D; Scale:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetScaleAndRotation(Spr:TERRASprite; ScaleX:Single; ScaleY:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetUniformScaleAndRotation(Spr:TERRASprite; Scale:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetScaleRelativeOnCenter(Spr:TERRASprite; Center:Vector2D; ScaleX:Single; ScaleY:Single); External TERRA_LIB;
Procedure Sprite_SetUniformScaleRelativeOnCenter(Spr:TERRASprite; Center:Vector2D; Scale:Single); External TERRA_LIB;
Procedure Sprite_SetScaleAndRotationRelativeOnCenter(Spr:TERRASprite; Center:Vector2D; ScaleX:Single; ScaleY:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetUniformScaleAndRotationRelativeOnCenter(Spr:TERRASprite; Center:Vector2D; Scale:Single; Rotation:Single); External TERRA_LIB;
Procedure Sprite_SetTransformRelative(Spr:TERRASprite; Center:Vector2D; Mat:Matrix3x3); External TERRA_LIB;
Procedure Sprite_SetColors(Spr:TERRASprite; A:Color; B:Color; C:Color; D:Color); External TERRA_LIB;
Procedure Sprite_SetColor(Spr:TERRASprite; C:Color); External TERRA_LIB;
Procedure Sprite_SetAlpha(Spr:TERRASprite; Alpha:Byte); External TERRA_LIB;
Procedure Sprite_SetScroll(Spr:TERRASprite; U:Single; V:Single); External TERRA_LIB;
Procedure Sprite_SetMirror(Spr:TERRASprite; Mirror:Boolean); External TERRA_LIB;
Procedure Sprite_SetFlip(Spr:TERRASprite; Flip:Boolean); External TERRA_LIB;
Procedure Sprite_TileRemapByID(Spr:TERRASprite; TileID:Integer; TilesPerRow:Integer; TileSize:Integer); External TERRA_LIB;
Procedure Sprite_TileRemap(Spr:TERRASprite; X:Integer; Y:Integer; TilesPerX:Integer; TilesPerY:Integer); External TERRA_LIB;
Procedure Sprite_PixelRemap(Spr:TERRASprite; X1:Integer; Y1:Integer; X2:Integer; Y2:Integer; W:Integer; H:Integer); External TERRA_LIB;
Function TileMap_Create(Source:PAnsiChar):TERRATileMap; External TERRA_LIB;
Procedure TileMap_Destroy(Map:TERRATileMap); External TERRA_LIB;
Procedure TileMap_Draw(Map:TERRATileMap; Depth:Single); External TERRA_LIB;
Procedure TileMap_GetPosition(Map:TERRATileMap; X:Single; Y:Single); External TERRA_LIB;
Procedure TileMap_SetPosition(Map:TERRATileMap; X:Single; Y:Single); External TERRA_LIB;
Function TileMap_GetObjectCount(Map:TERRATileMap):Integer; External TERRA_LIB;
Procedure TileMap_GetObjectPosition(Map:TERRATileMap; ID:Integer; X:Single; Y:Single); External TERRA_LIB;
Procedure TileMap_GetObjectSize(Map:TERRATileMap; ID:Integer; W:Single; H:Single); External TERRA_LIB;
Procedure TileMap_GetObjectTile(Map:TERRATileMap; ID:Integer; TX:Integer; TY:Integer); External TERRA_LIB;
Function TileMap_GetTileAt(Map:TERRATileMap; X:Integer; Y:Integer; Layer:Integer; UsePalette:Boolean):Integer; External TERRA_LIB;
Function TileMap_GetObjectProperty(Map:TERRATileMap; ID:Integer; Key:PAnsiChar):PAnsiChar; External TERRA_LIB;
Function TileMap_GetTileProperty(Map:TERRATileMap; ID:Integer; Key:PAnsiChar):PAnsiChar; External TERRA_LIB;
Procedure Music_Play(FileName:PAnsiChar); External TERRA_LIB;
Procedure Music_Stop(); External TERRA_LIB;
Procedure Music_SetVolume(Volume:Single); External TERRA_LIB;
Function Sound_Play(MySound:TERRASound):TERRASoundSource; External TERRA_LIB;
Function AI_PathGetSize(Path:TERRAPath):Integer; External TERRA_LIB;
Function AI_PathGetNextNode(Path:TERRAPath):TERRAPathNode; External TERRA_LIB;
Procedure AI_NodeGetCoords(Node:TERRAPathNode; X:Integer; Y:Integer); External TERRA_LIB;
Function AI_CreatePath(StartX:Integer; StartY:Integer; EndX:Integer; EndY:Integer; MinX:Integer; MinY:Integer; MaxX:Integer; MaxY:Integer; Path:TERRAPath; Flags:Integer; GetCostCallback:TERRAPathCostCallback; VisitCallback:TERRAVisitNodeCallback):Integer; External TERRA_LIB;
Procedure AI_DestroyPath(Path:TERRAPath); External TERRA_LIB;
Function XML_Open(Name:PAnsiChar):TERRAXML; External TERRA_LIB;
Function XML_GetRoot(XML:TERRAXML):TERRAXMLNode; External TERRA_LIB;
Function XML_GetNode(Node:TERRAXMLNode; Name:PAnsiChar):TERRAXMLNode; External TERRA_LIB;
Function XML_GetNodeByIndex(Node:TERRAXMLNode; ID:Integer):TERRAXMLNode; External TERRA_LIB;
Function XML_GetNodeCount(Node:TERRAXMLNode):Integer; External TERRA_LIB;
Function XML_GetNodeName(Node:TERRAXMLNode):PAnsiChar; External TERRA_LIB;
Function XML_GetNodeValue(Node:TERRAXMLNode):PAnsiChar; External TERRA_LIB;
Procedure XML_Destroy(XML:TERRAXML); External TERRA_LIB;
Procedure NetClient_SetOnConnectionStart(Handler:TERRANetworkCallback); External TERRA_LIB;
Procedure NetClient_SetOnConnectionEnd(Handler:TERRANetworkCallback); External TERRA_LIB;
Function NetClient_GetLocalId():Integer; External TERRA_LIB;
Function NetClient_IsConnected():Boolean; External TERRA_LIB;
Procedure NetClient_Connect(Address:PAnsiChar; Port:Word; Version:Word; Username:PAnsiChar; Password:PAnsiChar); External TERRA_LIB;
Procedure NetClient_Disconnect(); External TERRA_LIB;
Procedure NetClient_AddHandler(OpCode:Byte; Handler:TERRANetworkHandler); External TERRA_LIB;
Function NetClient_SendMessage(S:TERRANetMessage):Boolean; External TERRA_LIB;
Procedure Decal_Add(TextureName:PAnsiChar; Position:Vector3D; Normal:Vector3D; DecalColor:Color; Size:Single; Rotation:Single; Duration:Integer); External TERRA_LIB;
Function Billboard_Add(Position:Vector3D; Width:Single; Height:Single; MyTexture:TERRATexture):TERRABillboard; External TERRA_LIB;
Procedure Billboard_Remap(Billboard:TERRABillboard; U1:Single; V1:Single; U2:Single; V2:Single); External TERRA_LIB;
Function Particles_Spawn(FXName:PAnsiChar; Position:Vector3D; Loop:Boolean):TERRAParticles; External TERRA_LIB;



Const
  keyA  = Ord('A');
  keyB  = Ord('B');
  keyC  = Ord('C');
  keyD  = Ord('D');
  keyE  = Ord('E');
  keyF  = Ord('F');
  keyG  = Ord('G');
  keyH  = Ord('H');
  keyI  = Ord('I');
  keyJ  = Ord('J');
  keyK  = Ord('K');
  keyL  = Ord('L');
  keyM  = Ord('M');
  keyN  = Ord('N');
  keyO  = Ord('O');
  keyP  = Ord('P');
  keyQ  = Ord('Q');
  keyR  = Ord('R');
  keyS  = Ord('S');
  keyT  = Ord('T');
  keyU  = Ord('U');
  keyV  = Ord('V');
  keyW  = Ord('W');
  keyX  = Ord('X');
  keyY  = Ord('Y');
  keyZ  = Ord('Z');

{$IFDEF WINDOWS}
	keyBackspace  = 8;
	keyTab        = 9;
	keyEnter      = 13;
	keyShift      = 16;
	keyControl    = 17;
	keyAlt        = 18;
	keyPause      = 19;
	keyEscape     = 27;
	keySpace      = 32;
	keyPageUp     = 33;
	keyPageDown   = 34;
	keyEnd        = 35;
	keyHome       = 36;

	keyLeft       = 37;
	keyUp         = 38;
	keyRight      = 39;
	keyDown       = 40;

	keyInsert     = 45;
	keyDelete     = 46;
	keyF1         = 112;
	keyF2         = 113;
	keyF3         = 114;
	keyF4         = 115;
	keyF5         = 116;
	keyF6         = 117;
	keyF7         = 118;
	keyF8         = 119;
	keyF9         = 120;
	keyF10        = 121;
	keyF11        = 122;
	keyF12        = 123;
{$ENDIF}

{$IFDEF LINUX}
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 22;
	keyTab        = 23;
	keyEnter      = 36;
	keyShift      = 50;
	keyControl    = 37;
	keyAlt        = 64;
	keyPause      = 127;
	keyEscape     = 9;
	keySpace      = 65;
	keyPageUp     = 112;
	keyPageDown   = 117;
	keyEnd        = 115;
	keyHome       = 110;

	keyLeft       = 113;
	keyUp         = 111;
	keyRight      = 114;
	keyDown       = 116;

	keyInsert     = 118;
	keyDelete     = 119;

	keyF1         = 67;
	keyF2         = 68;
	keyF3         = 69;
	keyF4         = 70;
	keyF5         = 71;
	keyF6         = 72;
	keyF7         = 73;
	keyF8         = 74;
	keyF9         = 75;
	keyF10        = 76;
	keyF11        = 77;
	keyF12        = 78;
{$ENDIF}

{$IFDEF IPHONE}
Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = $33;
	keyTab        = $30;
	keyEnter      = $24;
	keyShift      = $38;
	keyControl    = $3B;
	keyAlt        = $3A;
	keyPause      = $71;
	keyEscape     = $35;
	keySpace      = $31;
	keyPageUp     = $74;
	keyPageDown   = $79;
	keyEnd        = $77;
	keyHome       = $73;

	keyLeft       = $7B;
	keyUp         = $7E;
	keyRight      = $7C;
	keyDown       = $7D;
{$ENDIF}

  //easing types
	easeLinear = 0;
	easeInQuad = 1;
	easeOutQuad = 2;
	easeInOutQuad = 3;
	easeOutInQuad = 4;
	easeInCubic = 5;
	easeOutCubic = 6;
	easeInOutCubic = 7;
	easeOutInCubic = 8;
	easeInQuart = 9;
	easeOutQuart = 10;
	easeInOutQuart = 11;
	easeOutInQuart = 12;
	easeInSine = 17;
	easeOutSine = 18;
	easeInOutSine = 19;
	easeOutInSine = 20;
	easeInExpo = 21;
	easeOutExpo = 22;
	easeInOutExpo = 23;
	easeOutInExpo = 24;
	easeInCirc = 25;
	easeOutCirc = 26;
	easeInOutCirc = 27;
	easeOutInCirc = 28;
	easeInElastic = 29;
	easeOutElastic = 30;
	easeInOutElastic = 31;
	easeOutInElastic = 32;
	easeInBack = 33;
	easeOutBack = 34;
	easeInOutBack = 35;
	easeOutInBack = 36;
	easeInBounce = 37;
	easeOutBounce = 38;
	easeInOutBounce = 39;
	easeOutInBounce = 40;

Function IntToString(Const N:Integer):AnsiString;
Function CardinalToString(Const N:Cardinal):AnsiString;
Function FloatToString(Const N:Single; DecimalPlaces:Integer = 4):AnsiString;
Function BoolToString(Const N:Boolean):AnsiString;

Function RandomFloat(Const min,max:Single):Single;
Function RandomInt(Const min,max:Integer):Integer;

Function ColorCreate(Const R,G,B:Byte; A:Byte=255):Color;
Function ColorMultiply(Const A,B:Color):Color;
Function ColorScale(Const A:Color; B:Single):Color;
Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;

Function VectorCreate2D(Const X,Y:Single):Vector2D;
Function VectorDot2D(Const A,B:Vector2D):Single;
Function VectorDistance2D(Const A,B:Vector2D):Single;
Function VectorLength2D(Const V:Vector2D):Single;
Procedure VectorNormalize2D(Var V:Vector2D);
Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Function VectorMultiply2D(Const A,B:Vector2D):Vector2D;
Function VectorScale2D(Const A:Vector2D; S:Single):Vector2D;

Function VectorCreate3D(Const X,Y,Z:Single):Vector3D;
Function VectorDistance3D(Const A,B:Vector3D):Single;
Function VectorDot3D(Const A,B:Vector3D):Single;
Function VectorLength3D(Const V:Vector3D):Single;
Procedure VectorNormalize3D(Var V:Vector3D);
Function VectorAdd3D(Const A,B:Vector3D):Vector3D;
Function VectorSubtract3D(Const A,B:Vector3D):Vector3D;
Function VectorMultiply3D(Const A,B:Vector3D):Vector3D;
Function VectorScale3D(Const A:Vector3D; S:Single):Vector3D;

Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;


Implementation
Uses Math;

Const
  RAND_MAX = (MAXINT-1);
  INV_RAND_MAX = 1.0 / (RAND_MAX + 1);


Function RandomFloat(Const min,max:Single):Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Begin
	Result := Min + ((max - min) * (System.Random(RAND_MAX) * INV_RAND_MAX));
End;

Function RandomInt(Const min,max:Integer):Integer;
Begin
  Result := Random(Succ(Max-Min)) + Min;
End;

Function IntToString(Const N:Integer):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result := S;
End;

Function BoolToString(Const N:Boolean):AnsiString;
Begin
  If N Then
    Result := 'true'
  Else
    Result := 'false';
End;

Function CardinalToString(Const N:Cardinal):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result:=S;
End;

Function FloatToString(Const N:Single; DecimalPlaces:Integer):AnsiString;
Var
  P,X:Single;
  A,B, I:Integer;
Begin
  P := N;
  A := Trunc(P);
  X := Abs(Frac(P));
  For I:=1 To DecimalPlaces Do
    X := X*10;
  B := Trunc(X);

  Result := IntToString(A)+'.'+IntToString(B);
End;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
End;

Function ColorMultiply(Const A,B:Color):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc((A.R/255)*(B.R/255)*255);
  Result.G:=Trunc((A.G/255)*(B.G/255)*255);
  Result.B:=Trunc((A.B/255)*(B.B/255)*255);
  Result.A:=Trunc((A.A/255)*(B.A/255)*255);
End;

Function ColorScale(Const A:Color; B:Single):Color;Overload;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  X,Y,Z:Single;
Begin
  X := A.R*B;
  Y := A.G*B;
  Z := A.B*B;
  If (X>255) Then X := 255;
  If (Y>255) Then Y := 255;
  If (Z>255) Then Z := 255;
  Result.R := Trunc(X);
  Result.G := Trunc(Y);
  Result.B := Trunc(Z);
  Result.A := A.A;
End;

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := GreyLevel;
  Result.G := GreyLevel;
  Result.B := GreyLevel;
  Result.A := Alpha;
End;

Function VectorCreate2D(Const X,Y:Single):Vector2D;
Begin
  Result.X := X;
  Result.Y := Y;
End;

Function VectorDistance2D(Const A,B:Vector2D):Single;
Begin
  Result := Sqrt(Sqr(A.X-B.X) + Sqr(A.Y-B.Y));
End;

Function VectorDot2D(Const A,B:Vector2D):Single;
Begin
  Result := (A.X * B.X) + (A.Y * B.Y);
End;

Function VectorLength2D(Const V:Vector2D):Single;
Begin
  Result := Sqrt(Sqr(V.X)+Sqr(V.Y));
End;

Procedure VectorNormalize2D(Var V:Vector2D);
Var
  K:Single;
Begin
  K := VectorLength2D(V);
  If (K<=1.0) Then
    Exit;

  V.X := V.X / K;
  V.Y := V.Y / K;
End;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
End;

Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
End;

Function VectorMultiply2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
End;

Function VectorScale2D(Const A:Vector2D; S:Single):Vector2D;
Begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
End;

// Vec3

Function VectorCreate3D(Const X,Y,Z:Single):Vector3D;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
End;

Function VectorDistance3D(Const A,B:Vector3D):Single;
Begin
  Result := Sqrt(Sqr(A.X-B.X) + Sqr(A.Y-B.Y) + Sqr(A.Z-B.Z));
End;

Function VectorDot3D(Const A,B:Vector3D):Single;
Begin
  Result := (A.X * B.X) + (A.Y * B.Y) + (A.Z * B.Z);
End;

Function VectorLength3D(Const V:Vector3D):Single;
Begin
  Result := Sqrt(Sqr(V.X)+Sqr(V.Y)+Sqr(V.Z));
End;

Procedure VectorNormalize3D(Var V:Vector3D);
Var
  K:Single;
Begin
  K := VectorLength3D(V);
  If (K<=1.0) Then
    Exit;

  V.X := V.X / K;
  V.Y := V.Y / K;
  V.Z := V.Z / K;
End;

Function VectorAdd3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
End;

Function VectorSubtract3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
End;

Function VectorMultiply3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
End;

Function VectorScale3D(Const A:Vector3D; S:Single):Vector3D;
Begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
  Result.Z := A.Z * S;
End;


Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
End;

Initialization
{$IFDEF FPC}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$ELSE}
  Set8087CW($133F);
{$ENDIF}
End.

