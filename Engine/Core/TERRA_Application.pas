{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_Application
 * Implements the engine Application class, which provides cross platform support
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;

{$ELSE}
Unit TERRA_Application;
{$I terra.inc}
{$ENDIF}

{$IFNDEF OSX}
{$DEFINE CATCHEXCEPTIONS}
{$ENDIF}

Interface
Uses {$IFNDEF DEBUG_LEAKS}TERRA_MemoryManager,{$ENDIF}SysUtils, TERRA_Client, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D
  {$IFNDEF DISABLEINPUTMUTEX},TERRA_Mutex{$ENDIF}
  ;

Const
	// Operating System Class
	osUnknown = 0;
	osWindows = 1;
	osLinux   = 2;
	osOSX     = 4;
  osiOS     = 8;
  osAndroid = 16;
  osOuya    = 32;
  osWindowsPhone     = 64;
  osWii         = 128;
  osNintendoDS  = 256;
  osPSVita      = 512;
  osHTML5       = 1024;
  osFlash       = 2048;

  osDesktop = osWindows Or osLinux or osOSX;
  osMobile = osAndroid Or osiOS Or osWindowsPhone;
  osConsole = osOUYA Or osWii Or osPSVita Or osNintendoDS;
  osWeb = osHTML5 Or osFlash;
  osEverything = $FFFFFFFF;

	MaxTimers = 16;

  TimerMultiplier = 4;

  OrientationAnimationDuration = 1000;

	// Window state
	wsNormal    = 0;
	wsMinimized = 1;
	wsMaximized = 2;

  orientationPortrait =  0;
  orientationLandscapeLeft =  1;
  orientationLandscapeRight =  2;
  orientationPortraitInverted =  3;

  eventMouseUp   = 0;
  eventMouseDown = 1;
  eventMouseMove = 2;
  eventMouseWheel = 3;
  eventKeyPress   = 4;
  eventKeyDown    = 5;
  eventKeyUp      = 6;
  eventWindowResize   = 7;
  eventAccelerometer  = 8;
  eventGyroscope      = 9;
  eventCompass        = 10;
  eventContextLost    = 11;
  eventOrientation    = 12;
  eventViewport       = 13;
  eventIAPPurchase    = 14;
  eventIAPCredits     = 15;
  eventIAPError       = 16;

  EventBufferSize = 512;

  keyGamepadIndex   = 255;
	keyGamepadLeft    = keyGamepadIndex + 0;
	keyGamepadUp      = keyGamepadIndex + 1;
	keyGamepadRight   = keyGamepadIndex + 2;
	keyGamepadDown    = keyGamepadIndex + 3;
	keyGamepadA       = keyGamepadIndex + 4;
	keyGamepadB       = keyGamepadIndex + 5;
	keyGamepadC       = keyGamepadIndex + 6;
	keyGamepadD       = keyGamepadIndex + 7;
	keyGamepadX       = keyGamepadIndex + 8;
	keyGamepadY       = keyGamepadIndex + 9;
	keyGamepadZ       = keyGamepadIndex + 10;
	keyGamepadR       = keyGamepadIndex + 11;
	keyGamepadL       = keyGamepadIndex + 12;
  keyGamepadMenu    = keyGamepadIndex + 13;
  keyGamepadCount   = 32;

	keyGamepadLeft2    = keyGamepadLeft + keyGamepadCount * 1;
	keyGamepadUp2      = keyGamepadUp + keyGamepadCount * 1;
	keyGamepadRight2   = keyGamepadRight + keyGamepadCount * 1;
	keyGamepadDown2    = keyGamepadDown + keyGamepadCount * 1;
	keyGamepadA2       = keyGamepadA + keyGamepadCount * 1;
	keyGamepadB2       = keyGamepadB + keyGamepadCount * 1;
	keyGamepadC2       = keyGamepadC + keyGamepadCount * 1;
	keyGamepadD2       = keyGamepadD + keyGamepadCount * 1;
	keyGamepadX2       = keyGamepadX + keyGamepadCount * 1;
	keyGamepadY2       = keyGamepadY + keyGamepadCount * 1;
	keyGamepadZ2       = keyGamepadZ + keyGamepadCount * 1;
	keyGamepadR2       = keyGamepadR + keyGamepadCount * 1;
	keyGamepadL2       = keyGamepadL + keyGamepadCount * 1;
  keyGamepadMenu2    = keyGamepadMenu + keyGamepadCount * 1;

	keyGamepadLeft3    = keyGamepadLeft + keyGamepadCount * 2;
	keyGamepadUp3      = keyGamepadUp + keyGamepadCount * 2;
	keyGamepadRight3   = keyGamepadRight + keyGamepadCount * 2;
	keyGamepadDown3    = keyGamepadDown + keyGamepadCount * 2;
	keyGamepadA3       = keyGamepadA + keyGamepadCount * 2;
	keyGamepadB3       = keyGamepadB + keyGamepadCount * 2;
	keyGamepadC3       = keyGamepadC + keyGamepadCount * 2;
	keyGamepadD3       = keyGamepadD + keyGamepadCount * 2;
	keyGamepadX3       = keyGamepadX + keyGamepadCount * 2;
	keyGamepadY3       = keyGamepadY + keyGamepadCount * 2;
	keyGamepadZ3       = keyGamepadZ + keyGamepadCount * 2;
	keyGamepadR3       = keyGamepadR + keyGamepadCount * 2;
	keyGamepadL3       = keyGamepadL + keyGamepadCount * 2;
  keyGamepadMenu3    = keyGamepadMenu + keyGamepadCount * 2;

	keyGamepadLeft4    = keyGamepadLeft + keyGamepadCount * 3;
	keyGamepadUp4      = keyGamepadUp + keyGamepadCount * 3;
	keyGamepadRight4   = keyGamepadRight + keyGamepadCount * 3;
	keyGamepadDown4    = keyGamepadDown + keyGamepadCount * 3;
	keyGamepadA4       = keyGamepadA + keyGamepadCount * 3;
	keyGamepadB4       = keyGamepadB + keyGamepadCount * 3;
	keyGamepadC4       = keyGamepadC + keyGamepadCount * 3;
	keyGamepadD4       = keyGamepadD + keyGamepadCount * 3;
	keyGamepadX4       = keyGamepadX + keyGamepadCount * 3;
	keyGamepadY4       = keyGamepadY + keyGamepadCount * 3;
	keyGamepadZ4       = keyGamepadZ + keyGamepadCount * 3;
	keyGamepadR4       = keyGamepadR + keyGamepadCount * 3;
	keyGamepadL4       = keyGamepadL + keyGamepadCount * 3;
  keyGamepadMenu4    = keyGamepadMenu + keyGamepadCount * 3;

  MaxGamePads = 4;

  keyMouseLeft   = keyGamepadIndex + 1 + keyGamepadCount * MaxGamePads;
  keyMouseRight  = keyGamepadIndex + 2 + keyGamepadCount * MaxGamePads;
  keyMouseMiddle = keyGamepadIndex + 3 + keyGamepadCount * MaxGamePads;

  settingsHintLow   = 0;
  settingsHintMedium = 1;
  settingsHintHigh   = 2;
  settingsHintVeryHigh   = 3;

  {$IFDEF SPLASHSCREEN}
  SplashScreenDuration = 4000;
  {$ENDIF}

Type
	PCursor=^MouseCursor;
	MouseCursor=Record
		X:SmallInt;
		Y:SmallInt;
	End;

  ApplicationEvent = Record
    X,Y,Z,W:Single;
    S:AnsiString;
    Value:Integer;
    Action:Integer;
    HasCoords:Boolean;
  End;


  ApplicationCallback = Procedure(P:Pointer); Cdecl;

  ApplicationCallbackEntry = Record
    Callback:ApplicationCallback;
    Time:Cardinal;
    AllowPause:Boolean;
    Arg:Pointer;
  End;

  ApplicationClass = Class Of Application;

  ApplicationComponentClass = Class Of ApplicationComponent;

  ApplicationComponent = Class(TERRAObject)
    Protected
      Procedure OnAppResize; Virtual;
      Procedure OnAppSplash; Virtual;
      Procedure Suspend; Virtual;
      Procedure Resume; Virtual;
      Procedure OnLanguageChange; Virtual;
      Procedure OnOrientationChange; Virtual;
      Procedure OnContextLost; Virtual;
      Procedure OnViewportChange(X1, Y1, X2, Y2:Integer); Virtual;

    Public
      Constructor Create();

      Procedure Init; Virtual;
      Procedure Update; Virtual;

      Destructor Destroy; Override;
  End;

  ApplicationObject = Class(TERRAObject)
    Protected
      _Component:ApplicationComponentClass;
      _Dependency:ApplicationComponentClass;
      _Instance:ApplicationComponent;
    Public
      Destructor Destroy(); Override;

      Property Component:ApplicationComponentClass Read _Component;
      Property Dependency:ApplicationComponentClass Read _Dependency;
      Property Instance:ApplicationComponent Read _Instance;
  End;

  ApplicationInput = Record
		Mouse:MouseCursor;
		Keys:InputState; //Keyboard state
    Accelerometer:Vector3D;
    Gyroscope:Vector3D;
    Compass:Vector3D; // (heading, pitch, roll)
  End;

  AssetWatchNotifier = Procedure(Const FileName:AnsiString); Cdecl;

  FolderManager = Class(ApplicationComponent)
    Protected
      _Notifiers:Array Of AssetWatchNotifier;
      _NotifierCount:Integer;

    Public
      Class Function Instance:FolderManager;

      Function WatchFolder(Const Path:AnsiString):Boolean; Virtual;

      Procedure NotifyFileChange(Const FileName:AnsiString);

      Procedure AddWatcher(Notifier:AssetWatchNotifier);
  End;

	Application = Class(TERRAObject)
		Protected
			_Handle:Cardinal;   // Global window handle
			_Running:Boolean;
      _Ready:Boolean;
      _Hidden:Boolean;
			_CanReceiveEvents:Boolean;
      _Suspended:Boolean;
			_Startup:Boolean;
      _StartTime:Cardinal;
			_Title:AnsiString;
			_State:Cardinal;
			_Path:AnsiString;
      _Managed:Boolean;

      _Language:AnsiString;
      _Country:AnsiString;

      _Events:Array[0..Pred(EventBufferSize)] Of ApplicationEvent;
      _EventCount:Integer;

      {$IFNDEF DISABLEINPUTMUTEX}
      _InputMutex:CriticalSection;
      {$ENDIF}

      _Client:AppClient;
      _ClientInit:Boolean;

			_Width:Integer;
			_Height:Integer;
      _AspectRatio:Single;
      _AntialiasSamples:Integer;
			_FullScreen:Boolean;
      _IgnoreCursor:Boolean;
      _IsConsole:Boolean;

      _PauseStart:Cardinal;
      _PauseCounter:Cardinal;
      _Paused:Boolean;

      _ContextWasLost:Boolean;
      _ContextCounter:Integer;

      _Callbacks:Array Of ApplicationCallbackEntry;
      _CallbackCount:Integer;

      _UIWidth:Integer;
      _UIHeight:Integer;

      _DeviceX1:Integer;
      _DeviceY1:Integer;
      _DeviceX2:Integer;
      _DeviceY2:Integer;
      _DeviceWidth:Integer;
      _DeviceHeight:Integer;
      _DeviceScaleX:Single;
      _DeviceScaleY:Single;

      _CurrentUser:AnsiString;

      _ChangeToFullScreen:Boolean;

      _Terminated:Boolean;

      _Orientation:Integer;
      _PreviousOrientation:Integer;
      _OrientationTime:Integer;

      _MouseOnAdArea:Boolean;

      _CPUCores:Integer;

      _TapjoyCredits:Integer;

      _DocumentPath:AnsiString;
      _StoragePath:AnsiString;
      _TempPath:AnsiString;
      _FontPath:AnsiString;

      _FrameStart:Cardinal;

      _FolderManager:FolderManager;

			{$IFDEF HASTHREADS}
      //_InputThread:Thread;
      {$ENDIF}

			Function InitWindow:Boolean; Virtual; Abstract;
			Procedure CloseWindow; Virtual; Abstract;

      Procedure OnShutdown; Virtual;

			Procedure InitSystem;
			Procedure ShutdownSystem;

      Procedure ProcessMessages; Virtual;

      Function InitSettings:Boolean; Virtual; 

			Function InitGraphics:Boolean; Virtual; Abstract;
      Procedure CloseGraphics; Virtual; Abstract;

      Procedure Finish;

      Procedure SetPause(Value:Boolean);

      Procedure UpdateContextLost;

      Procedure ConvertCoords(Var X,Y:Integer);

      Function GetAspectRatio: Single;
      
      //Procedure UpdateCallbacks;
      //Procedure SetProcessorAffinity;

      Function GetTempPath():AnsiString; Virtual;
      Function GetDocumentPath():AnsiString; Virtual;
      Function GetStoragePath():AnsiString; Virtual;

      Procedure AddEventToQueue(Action:Integer; X,Y,Z,W:Single; Value:Integer; S:AnsiString; HasCoords:Boolean);

    Public
      Input:ApplicationInput;

			Constructor Create(Client:AppClient);

      Destructor Destroy; Override;

			Function Run:Boolean; Virtual;

			Procedure Terminate(ForceClose:Boolean = False);Virtual;

      Procedure SetViewport(X1, Y1, X2, Y2:Integer);
                                              Procedure AddRectEvent(Action:Integer; X1,Y1,X2,Y2:Single); Overload;
      Procedure AddVectorEvent(Action:Integer; X,Y,Z:Single); Overload;
      Procedure AddCoordEvent(Action:Integer; X,Y, Value:Integer); Overload;
      Procedure AddValueEvent(Action:Integer; Value:Integer); Overload;
      Procedure AddStringEvent(Action:Integer; S:AnsiString); Overload;

      Function SetOrientation(Value:Integer):Boolean; Virtual;

			Procedure SetState(State:Cardinal); Virtual;
			Procedure SwapBuffers; Virtual;
      Procedure Yeld; Virtual;

      Procedure SetFullscreenMode(UseFullScreen:Boolean); Virtual;
      Procedure ToggleFullscreen;

      // ads
      Procedure EnableAds; Virtual;
      Procedure DisableAds; Virtual;
      Procedure ShowFullscreenAd; Virtual;

      Procedure OpenAppStore(AppID:AnsiString); Virtual;
      Procedure SendEmail(DestEmail, Subject, Body:AnsiString); Virtual;

      Function SaveToCloud():Boolean; Virtual;

      Function HasInternet:Boolean; Virtual;

      Function HasFatalError:Boolean;

      //analytics
      Procedure SendAnalytics(EventName:AnsiString); {$IFNDEF OXYGENE}Overload; {$ENDIF} Virtual;
      Procedure SendAnalytics(EventName:AnsiString; Parameters:AnsiString); {$IFNDEF OXYGENE}Overload;{$ENDIF} Virtual;

      // achievements
      Procedure UnlockAchievement(AchievementID:AnsiString); Virtual;

      // facebook
      Procedure PostToFacebook(msg, link, desc, imageURL:AnsiString); Virtual;
      Procedure LikeFacebookPage(page, url:AnsiString); Virtual;

      //tapjoy
      Procedure Tapjoy_ShowOfferWall(); Virtual;
      Procedure Tapjoy_ShowVideo(); Virtual;
      Procedure Tapjoy_SpendCredits(Ammount:Integer); Virtual;
      Procedure Tapjoy_Update(Credits: Integer);

      Procedure Resize(Width, Height:Integer);

      Procedure SetSuspend(Value:Boolean);

      Procedure SetTitle(Const Name:AnsiString); Virtual;

      Procedure SetLanguage(Language:AnsiString);

      Procedure ProcessEvents;
      Procedure RefreshComponents();

      //Procedure AddCallback(Callback:ApplicationCallback; Delay:Integer; Argument:Pointer = Nil; AllowPause:Boolean = True);

      Function KeyPressed(Key:Word):Boolean;
			Procedure SetKeyPress(ID:Integer; Value:Boolean);

      Function GetDeviceID():AnsiString; Virtual;

      Function GetElapsedTime:Cardinal;

      Function GetPlatform:Cardinal;

      Function CanHandleEvents:Boolean;

      Function InSplashScreen:Boolean;

      Function FrameTime:Cardinal;

      Function GetControllerCount:Integer; Virtual;

      Function GetRecommendedSettings():Integer; Virtual;

      Function InitAccelerometer():Boolean; Virtual;
      Function InitGyroscope():Boolean; Virtual;
      Function InitCompass():Boolean; Virtual;

      Procedure StopAccelerometer(); Virtual;
      Procedure StopGyroscope(); Virtual;
      Procedure StopCompass(); Virtual;

      Function IsDebuggerPresent:Boolean; Virtual;
      Function IsAppRunning(Name:AnsiString):Boolean; Virtual;
      Function IsAppInstalled(Name:AnsiString):Boolean; Virtual;
      Function IsDeviceRooted:Boolean; Virtual;

      Function GetOrientationDelta:Single;

      Class Function Instance:Application;

      Property Client:AppClient Read _Client;

      Property CPUCores:Integer Read _CPUCores;

			Property Handle:Cardinal Read _Handle;
			Property OS:Cardinal Read GetPlatform;
			Property CurrentPath:AnsiString Read _Path;
      Property TempPath:AnsiString Read GetTempPath;
      Property StoragePath:AnsiString Read GetStoragePath;
      Property DocumentPath:AnsiString Read GetDocumentPath;
      Property FontPath:AnsiString Read _FontPath;

			Property Title:AnsiString Read _Title;
			Property Width:Integer Read _Width;
			Property Height:Integer Read _Height;
			Property FullScreen:Boolean Read _Fullscreen;
      Property Language:AnsiString Read _Language Write SetLanguage;
      Property Country:AnsiString Read _Country;
      Property CurrentUser:AnsiString Read _CurrentUser;

      Property UI_Width:Integer Read _UIWidth;
      Property UI_Height:Integer Read _UIHeight;

      Property IsRunning:Boolean Read _Running;

      Property Paused:Boolean Read _Paused Write SetPause;
      Property CanReceiveEvents:Boolean Read _CanReceiveEvents;

      Property ContextID:Integer Read _ContextCounter;

      Property TapjoyCredits:Integer Read _TapjoyCredits;

      Property Orientation:Integer Read _Orientation;
      Property PreviousOrientation:Integer Read _PreviousOrientation;

      Property AspectRatio:Single Read GetAspectRatio;

      Property IsConsole:Boolean Read _IsConsole;
	End;

Var
  Prefetching:Boolean;
  DebuggerPresent:Boolean;

Procedure ApplicationStart(Client:AppClient);
Function InitializeApplicationComponent(TargetClass, DestroyBefore:ApplicationComponentClass):ApplicationObject;

Function Blink(Period:Cardinal):Boolean;
Procedure Sleep(Time:Cardinal);

Function GetKeyByName(KeyName:AnsiString):Integer;
Function GetKeyName(Key:Integer):AnsiString;

Function GetOSName(OS:Integer=0):AnsiString;
Function GetProgramName():AnsiString;

Function IsLandscapeOrientation(Orientation:Integer):Boolean;
Function IsPortraitOrientation(Orientation:Integer):Boolean;
Function IsInvalidOrientation(Orientation:Integer):Boolean;

Implementation
Uses TERRA_Error, {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  {TERRA_Callstack, }TERRA_Log, TERRA_OS, TERRA_IAP, TERRA_Localization, TERRA_FileUtils, TERRA_FileManager
  {$IFDEF STEAM},TERRA_Steam{$ENDIF};

Var
  _Application_Ready:Boolean;
  _Application_Instance:Application;

  _ApplicationComponents:Array Of ApplicationObject;
  _ApplicationComponentCount:Integer;

  _RefreshingComponents:Boolean = False;

Function IsInvalidOrientation(Orientation:Integer):Boolean;
Begin
    Result := (Orientation<0) Or (Orientation>=4);
End;

Function IsLandscapeOrientation(Orientation:Integer): Boolean;
Begin
  Result := (Orientation = orientationLandscapeLeft) Or (Orientation = orientationLandscapeRight);
End;

Function IsPortraitOrientation(Orientation:Integer): Boolean;
Begin
  Result := (Orientation = orientationPortrait) Or (Orientation = orientationPortraitInverted);
End;

Var
  _ShuttingDown:Boolean = False;

{ Component }
Function InitializeApplicationComponent(TargetClass, DestroyBefore:ApplicationComponentClass):ApplicationObject;
Var
  I:Integer;
  S:AnsiString;
Begin
  If TargetClass = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  For I:=0 To Pred(_ApplicationComponentCount) Do
  If (_ApplicationComponents[I]._Component = TargetClass) Then
  Begin
    Result := _ApplicationComponents[I];
    Exit;
  End;

{$IFDEF OXYGENE}
  S := Target.GetType().Name;
{$ELSE}
  S := TargetClass.ClassName;
{$ENDIF}
  Log(logDebug, 'Application', 'Registering component: '+ S);

  Result := ApplicationObject.Create();

  Result._Component := TargetClass;
  Result._Dependency := DestroyBefore;

  Inc(_ApplicationComponentCount);
  {$IFDEF OXYGENE}
  _ApplicationComponents += Entry;
  {$ELSE}
  SetLength(_ApplicationComponents, _ApplicationComponentCount);
  _ApplicationComponents[Pred(_ApplicationComponentCount)] := Result;
  {$ENDIF}

  Result._Instance := TargetClass.Create();

  If (_ShuttingDown) Then
  Begin
    Log(logDebug,'App', S+ ' has wrong dependencies...');
  End;

{  For I:=0 To Pred(_ApplicationComponentCount) Do
  Log(logDebug,'App', IntToString(I)+ ' '+_ApplicationComponents[I].Component.ClassName);}
End;

Procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report:AnsiString;
begin
  Report := 'Fatal Exception! ' + CrLf;
  If E <> nil Then
  Begin
    Report := Report + 'Exception class: ' + E.ClassName + CrLf;
    Report := Report + 'Message: ' + E.Message + CrLf;
  end;

  {$IFDEF FPC}
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  For I := 0 to Pred(ExceptFrameCount) Do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  {$ENDIF}

  Log(logError, 'App', Report);
  //Halt; // End of program execution
End;

{$IFDEF HASTHREADS}
(*Function InputThread(Arg:Pointer):Integer;
Var
  App:Application;
Begin
  App := Application(Arg);
  App.InitWindow;

  While (App._Ready) Do
  Begin
    App.ProcessMessages;
    App.Yeld;
  End;

  App.CloseWindow;
  Result := 0;
End;*)
{$ENDIF}

Class Function Application.Instance:Application;
Begin  {
  If (Not Assigned(_Application_Instance)) And (_Application_Ready) Then
  Begin
    _Application_Instance := TERRA_OS.GetApplicationClass.Create;
  End;}

  Result := _Application_Instance;
End;

Procedure ShutdownComponents;
Var
  I:Integer;
  S:AnsiString;

  Function GetDependencyCount(T:ApplicationComponentClass):Integer;
  Var
    J:Integer;
    S3,S2:AnsiString;
  Begin
    Result := 0;
    For J:=0 To (_ApplicationComponentCount-1) Do
    If (_ApplicationComponents[J].Dependency<>Nil) And (_ApplicationComponents[J].Dependency = T) Then
    Begin
      Inc(Result);
      S3 := T.ClassName;
      S2 := _ApplicationComponents[J].Component.ClassName;
      Log(logDebug, 'App', S2+' still has dependency on '+S3);
    End;
  End;
Begin
  _ShuttingDown := True;
  Repeat
    I:=0;
    While (I<_ApplicationComponentCount) Do
    Begin
      If (GetDependencyCount(_ApplicationComponents[I].Component)>0) Then
        Inc(I)
      Else
      Begin
        S := _ApplicationComponents[I].Component.ClassName;

        Log(logDebug, 'App', 'Shutting down '+S);
        If Assigned(_ApplicationComponents[I].Instance) Then
        Begin
          _ApplicationComponents[I].Instance.Destroy();
          _ApplicationComponents[I]._Instance := Nil;
        End;

        _ApplicationComponents[I].Destroy();
        _ApplicationComponents[I] := _ApplicationComponents[Pred(_ApplicationComponentCount)];
        Dec(_ApplicationComponentCount);
        Break;
      End;
    End;
  Until _ApplicationComponentCount<=0;
End;

Procedure Application.ShutdownSystem;
Begin
  ShutdownComponents;

  If (Not _IsConsole) Then
  Begin
    CloseGraphics();
  End;

  _Ready := False;
  _CanReceiveEvents := False;

  {$IFNDEF DISABLEINPUTMUTEX}
  If Assigned(_InputMutex) Then
  Begin
    _InputMutex.Destroy();
    _InputMutex := Nil;
  End;
  {$ENDIF}

  If (Not _Managed) And (Not _IsConsole) Then
  Begin
    CloseWindow;
  End;

  Self.OnShutdown;
End;


Procedure Application.InitSystem;
Var
  I:Integer;
  S:AnsiString;
Begin
  Log(logDebug, 'App', 'Initializing randomizer');

  {$IFNDEF OXYGENE}
  System.Randomize;
  {$ENDIF}

  {$IFDEF PC}
  //_TapjoyCredits := 250;
  {$ENDIF}

  Log(logDebug, 'App', 'Creating critical section for input');

  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex := CriticalSection.Create('app_input');
  {$ENDIF}


  Log(logDebug, 'App', 'Initializing window');

  _Orientation := orientationPortrait;
  _OrientationTime := 0;
  _PreviousOrientation := _Orientation;

  If Assigned(Client) Then
    _IsConsole := Client.IsConsole()
  Else
    _IsConsole := False;

  _Ready := False;
  {$IFNDEF MOBILE}
  If (Not _Managed) And (Not _IsConsole) Then
  {$ENDIF}
    InitWindow;

  Log(logDebug, 'App', 'Initializing settings');
  If (Not InitSettings()) Then
    Halt(0);

  {$IFDEF STEAM}
  If (IsSupportedLanguage(Steam.Instance.Language)) Then
    _Language := Steam.Instance.Language;
  {$ENDIF}

  If (Not _IsConsole) Then
  Begin
    Log(logDebug, 'App', 'Initializing graphics');
    If (Not InitGraphics) Then
      Halt(0);

    Self.SetViewport(0,0,_Width,_Height);

    If Assigned(Client) Then
    Begin
      _UIWidth := _Width;
      _UIHeight := _Height;
      Client.SelectResolution2D(_UIWidth, _UIHeight);
      Log(logDebug, 'App', 'Selected UI resolution: '+IntToString(_UIWidth)+' x ' +IntToString(_UIHeight));
    End;
  End;
  
  Log(logDebug, 'App', 'Found ' + IntToString(_ApplicationComponentCount)+' Components!');
  For I:=0 To Pred(_ApplicationComponentCount) Do
  Begin
    {$IFDEF OXYGENE}
      S := _ApplicationComponents[I].Component.GetType().Name;
    {$ELSE}
      S := _ApplicationComponents[I].Component.ClassName;
    {$ENDIF}
    Log(logDebug, 'App', 'Starting '+S);
  End;

  FileManager.Instance.AddPath(Application.Instance.DocumentPath);
End;

Constructor Application.Create(Client:AppClient);
Begin
  _Client := Client;

  _Startup := True;
  _CanReceiveEvents := False;
  _Title := Self.Client.GetTitle();
  _Width := Self.Client.GetWidth();
  _Height := Self.Client.GetHeight();
  _Fullscreen := Self.Client.GetFullscreen();
  _AntialiasSamples := Self.Client.GetAntialiasSamples();
  _Hidden := Self.Client.GetHidden();
  _Handle := Self.Client.GetHandle();
  _State := wsNormal;

  If Assigned(_Client) Then
    _Client.Keys := @(Input.Keys[0]);
  _IgnoreCursor := Self.Client.GetIgnoreCursor;

{  _UsesAccelerometer := ApplicationSettings.UsesAccelerometer;
  _UsesGyroscope := ApplicationSettings.UsesGyroscope;
  _UsesCompass := ApplicationSettings.UsesCompass;
  _UsesGameCenter := ApplicationSettings.UsesGameCenter;}

  {$IFDEF OXYGENE}
  _Managed := True;
  {$ELSE}
  {$IFDEF MOBILE}
  _Managed := True;
  {$ELSE}
  _Managed := (_Handle > 0);
  {$ENDIF}
  {$ENDIF}
End;

Procedure Application.Finish();
Begin
  _Running := False;

  If Assigned(_Client) Then
    Client.OnDestroy;

  Log(logWarning, 'App', 'Shutting down all subsystems.');
  ShutdownSystem;
  Log(logWarning, 'App', 'All subsystems destroyed.');

  {$IFNDEF OXYGENE}
  If (Not _Managed) Then
    Self.Destroy;
  {$ENDIF}

  _Application_Instance := Nil;

  Log(logWarning, 'App', 'Application has shutdown.')
End;

Procedure Application.Terminate(ForceClose:Boolean);
Begin
  If (Self = Nil) Then
    Halt;

  _Terminated := True;

  {$IFNDEF MOBILE}
  If ForceClose Then
  Begin
    Self.Finish();
    Halt;
  End;
  {$ENDIF}
End;

Procedure Application.Resize(Width, Height:Integer);
Var
  I:Integer;
Begin
  If (Width=0) Or (Height=0) Then
    Exit;

  _Width := Width;
  _Height := Height;

  Self.SetViewport(0, 0, _Width, _Height);

  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.OnAppResize();
  End;
End;


Procedure Application.SetViewport(X1, Y1, X2, Y2:Integer);
Var
  I:Integer;
Begin
  If (Width=0) Or (Height=0) Then
    Exit;

  _DeviceX1 := X1;
  _DeviceY1 := Y1;
  _DeviceX2 := X2;
  _DeviceY2 := Y2;

  _DeviceWidth := (X2-X1);
  _DeviceHeight := (Y2-Y1);

  _DeviceScaleX := 1 / _DeviceWidth;
  _DeviceScaleY := 1 / _DeviceHeight;

  For I:=0 To Pred(_ApplicationComponentCount) Do
  Begin
    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.OnViewportChange(X1, Y1, X2, Y2);
  End;

  Self.SwapBuffers();
End;

Procedure Application.SetKeyPress(ID:Integer; Value:Boolean);
Begin
  If (Input.Keys[ID]=Value) Then
    Exit;

  Input.Keys[ID] := Value;
  If (Assigned(_Client)) Then
  Begin
    If (Value) Then
      Client.OnKeyDown(ID)
    Else
      Client.OnKeyUp(ID);
  End;
End;

Function Application.Run:Boolean;
Begin
  If (_Terminated) Then
  Begin
    Self.Finish();
    Result := False;
    Exit;
  End;

  If (Not _Managed) And (_Running) Then
  Begin
    RaiseError('Application is already running.');
    Result := False;
    Exit;
  End;

  Result := True;
  If _Startup Then
  Begin
    Log(logDebug, 'App', 'Initializing system');

    // Create window
    InitSystem();

    _ClientInit := True;
    _StartTime := GetElapsedTime();

    If (_IsConsole) Then
      _Managed := True;

    _Startup := False;
    If (_Managed) Or (_Hidden) Then
      Exit;
  End;

  _Running := True;
  _FrameStart := GetTime();
  While (_Running) And (Not _Terminated) Do
  Begin
  Try

    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Processing messages');{$ENDIF}{$ENDIF}
    Self.ProcessMessages;
    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'All messages processed');{$ENDIF}{$ENDIF}

    If Assigned(Client) Then
    Begin
      If (_ClientInit) {$IFDEF SPLASHSCREEN} And ((DebuggerPresent) Or (GetCurrentTime-_StartTime>SplashScreenDuration)){$ENDIF} Then
      Begin
        {$IFDEF SPLASHSCREEN}
        For I:=Pred(_ApplicationComponentCount) DownTo 0 Do
          _ApplicationComponents[I].Component.OnAppSplash;
        {$ENDIF}

        Client.OnCreate;
        _ClientInit := False;
        _CanReceiveEvents := True;
      End Else
      If (Not _ClientInit) Then
      Begin
        Self.ProcessEvents();

        If _FatalError Then
        Begin
          {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logWarning, 'App', 'Fatal error!!!!');{$ENDIF}{$ENDIF}
          If (Self.Input.Keys[keyEscape]) Then
            Self.Terminate(False);
        End Else
        Begin
          {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Callind client.OnIdle()');{$ENDIF}{$ENDIF}
          Client.OnIdle;
        End;

        {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'client.OnIdle() finished');{$ENDIF}{$ENDIF}
      End;
    End;

    If (_ContextWasLost) Then
    Begin
      _ContextWasLost := False;
      Inc(_ContextCounter);
      Self.UpdateContextLost();
    End;

    {$IFDEF CATCHEXCEPTIONS}Try{$ENDIF}
      _FrameStart := GetTime();

    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Refreshing Components');{$ENDIF}{$ENDIF}
    If Not _Suspended Then
      Self.RefreshComponents();

    {$IFDEF CATCHEXCEPTIONS}
    Except
    On E: Exception Do
      Begin
        If Assigned(Client) Then
          Client.OnFatalError(E.Message);
        DumpExceptionCallStack(E);
      End;
    End;
    {$ENDIF}

    //Log(logDebug, 'App', 'Updating callbacks');
    //Self.UpdateCallbacks;

    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Swapping buffers');{$ENDIF}{$ENDIF}
    Self.SwapBuffers;

    If (_ChangeToFullScreen) Then
    Begin
      _ChangeToFullScreen := False;
      ToggleFullScreen;
    End;

    If (_Managed) Then
      Exit;

  Except
    On E : Exception do
    Begin
      //FillCallStack(St, 0);
      Log(logError, 'Application', 'Exception: '+E.ClassName +' '+E.Message);
      //Log(logError, 'Application', CallStackTextualRepresentation(St,' '));

       Break;
     End;
  End;
  End;

  Self.Finish();
  Result := False;
End;


Function GetOSName(OS:Integer=0):AnsiString;
Begin
  If (OS = 0) Then
    OS := Application.Instance.GetPlatform();

  Case OS Of
  osWindows:Result := 'Windows';
  osLinux:  Result := 'Linux';
  osOSX:  Result := 'OSX';
  osWindowsPhone:    Result := 'WP8';
  osiOS:    Result := 'iOS';
  osAndroid:Result := 'Android';
  osOuya:   Result := 'Ouya';
  Else
    Result := 'Unknown';
  End;
End;

Procedure Application.SetFullscreenMode(UseFullScreen: Boolean);
Begin
  Log(logError, 'App','ToggleFullscreen not implemented!');
End;

Procedure Application.ToggleFullscreen;
Begin
  SetFullscreenMode(Not Self._Fullscreen);
End;

Procedure Application.SwapBuffers;
Begin
 Log(logError, 'App','SwapBuffers not implemented!');
End;

Procedure Application.SetState(State:Cardinal);
Begin
 Log(logError, 'App','SetState not implemented!');
End;

Procedure Application.Yeld;
Begin
 Log(logError, 'App','Yeld not implemented!');
End;

Function Application.KeyPressed(Key:Word):Boolean;
Begin
  {If Key=Ord('P') Then
    IntToString(2);}

  Result := Input.Keys[Key];
  Input.Keys[Key] := False;
End;

Procedure ApplicationStart(Client:AppClient);
Begin
  _FatalError := False;
  LoggingDisabled := Not Client.GetLogging();

  Log(logDebug,'App', 'Loading application settings.');

  _Application_Ready := True;
  _Application_Instance := CreateApplicationClass(Client);

  DebuggerPresent := _Application_Instance.IsDebuggerPresent();

  Log(logDebug,'App', 'Starting engine.');

  {$IFNDEF ISLIBRARY}
  _Application_Instance.Run();
  {$ENDIF}
End;

Procedure Application.SetPause(Value: Boolean);
Var
  N:Cardinal;
Begin
  If (Value = _Paused) Then
    Exit;

  If Value Then
  Begin
    _PauseStart := Self.GetElapsedTime();
    _Paused := True;
  End Else
  Begin
    _Paused := False;
    N := Self.GetElapsedTime();
    Dec(N, _PauseStart);
    Inc(_PauseCounter, N);
  End;
End;

Function Application.GetElapsedTime(): Cardinal; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (Application.Instance.Paused) Then
    Result := _PauseStart
  Else
    Result := GetTime() - _PauseCounter;
End;

{Procedure Application.AddCallback(Callback:ApplicationCallback; Delay:Integer; Argument:Pointer = Nil; AllowPause:Boolean = True);
Var
  T:Cardinal;
Begin
  If AllowPause Then
    T := Self.GetTime
  Else
    T := TERRA_OS.GetTime;
  T := T + Delay;
  Inc(_CallbackCount);
  SetLength(_Callbacks, _CallbackCount);
  _Callbacks[Pred(_CallbackCount)].Callback := Callback;
  _Callbacks[Pred(_CallbackCount)].Time := T;
  _Callbacks[Pred(_CallbackCount)].Arg := Argument;
  _Callbacks[Pred(_CallbackCount)].AllowPause := AllowPause;
End;}

{Procedure Application.UpdateCallbacks;
Var
  I:Integer;
  Done:Boolean;
Begin
  I := 0;
  While (I<_CallbackCount) Do
  Begin
    Done := False;
    If (_Callbacks[I].AllowPause) And (_Callbacks[I].Time<Self.GetTime) Then
      Done := True
    Else
    If (Not _Callbacks[I].AllowPause) And (_Callbacks[I].Time<TERRA_OS.GetTime) Then
      Done := True;

    If (Done) Then
    Begin
      _Callbacks[I].Callback(_Callbacks[I].Arg);
      _Callbacks[I] := _Callbacks[Pred(_CallbackCount)];
      Dec(_CallbackCount);
    End Else
      Inc(I);
  End;
End;}

// do nothing
Procedure Application.EnableAds; Begin End;
Procedure Application.DisableAds; Begin End;

Procedure Application.OpenAppStore(AppID:AnsiString); Begin End;

{ ApplicationObject }
Destructor ApplicationObject.Destroy;
Begin
  DestroyObject(@_Instance);
End;

{ ApplicationComponent }
Constructor ApplicationComponent.Create;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ApplicationComponentCount) Do
  If (_ApplicationComponents[I].Component = Self.ClassType) Then
  Begin
    _ApplicationComponents[I]._Instance := Self;
  End;

  Self.Init();
End;

Procedure ApplicationComponent.Init; Begin End;
Procedure ApplicationComponent.OnAppResize;  Begin End;
Procedure ApplicationComponent.OnAppSplash;  Begin End;
Procedure ApplicationComponent.Update;  Begin End;
Procedure ApplicationComponent.Resume;  Begin End;
Procedure ApplicationComponent.Suspend; Begin End;
Procedure ApplicationComponent.OnLanguageChange; Begin End;
Procedure ApplicationComponent.OnContextLost; Begin End;
Procedure ApplicationComponent.OnOrientationChange; Begin End;
Procedure ApplicationComponent.OnViewportChange(X1, Y1, X2, Y2:Integer); Begin End;

Procedure Application.SendAnalytics(EventName, Parameters:AnsiString); Begin End;
Procedure Application.UnlockAchievement(AchievementID:AnsiString); Begin End;
Function Application.IsDebuggerPresent:Boolean; Begin Result := False; End;


Function Blink(Period:Cardinal):Boolean;
Begin
  Result := ((GetTime() Shr 4) Mod Period<(Period Shr 1));
End;

Function GetKeyName(Key:Integer):AnsiString;
Begin
  Case Key Of
    keyBackspace: Result:='Back';
    keyEnter:     Result:='Enter';
    keyLeft:      Result:='Left';
    keyUp:        Result:='Up';
    keyRight:     Result:='Right';
    keyDown:      Result:='Down';

    keyTab:       Result:='Tab';
    keyShift:     Result:='Shift';
    keyControl:   Result:='Ctrl';
    keyAlt:       Result:='Alt';
    keyPause:     Result:='Pause';
    keyEscape:    Result:='ESC';
    keySpace:     Result:='Space';
    keyPageUp:    Result:='PgUp';
    keyPageDown:  Result:='PgDn';
    keyEnd:       Result:='End';
    keyHome:      Result:='Home';
    keyInsert:    Result:='Ins';
    keyDelete:    Result:='Del';
    keyF1:        Result:='F1';
    keyF2:        Result:='F2';
    keyF3:        Result:='F3';
    keyF4:        Result:='F4';
    keyF5:        Result:='F5';
    keyF6:        Result:='F6';
    keyF7:        Result:='F7';
    keyF8:        Result:='F8';
    keyF9:        Result:='F9';
    keyF10:       Result:='F10';
    keyF11:       Result:='F11';
    keyF12:       Result:='F12';

    keyMouseLeft:       Result:='MouseLeft';
    keyMouseRight:      Result:='MouseRight';
    keyMouseMiddle:     Result:='MouseMiddle';

    keyGamepadLeft:     Result:='GamepadLeft';
    keyGamepadUp:       Result:='GamepadUp';
    keyGamepadRight:    Result:='GamepadRight';
    keyGamepadDown:     Result:='GamepadDown';
    keyGamepadA:        Result:='GamepadA';
    keyGamepadB:        Result:='GamepadB';
    keyGamepadC:        Result:='GamepadC';
    keyGamepadD:        Result:='GamepadD';
    keyGamepadX:        Result:='GamepadX';
    keyGamepadY:        Result:='GamepadY';
    keyGamepadZ:        Result:='GamepadZ';
    keyGamepadL:        Result:='GamepadL';
    keyGamepadR:        Result:='GamepadR';
    keyGamepadMenu:     Result:='GamepadMenu';

    keyGamepadLeft2:     Result:='GamepadLeft #2';
    keyGamepadUp2:       Result:='GamepadUp #2';
    keyGamepadRight2:    Result:='GamepadRight #2';
    keyGamepadDown2:     Result:='GamepadDown #2';
    keyGamepadA2:        Result:='GamepadA #2';
    keyGamepadB2:        Result:='GamepadB #2';
    keyGamepadC2:        Result:='GamepadC #2';
    keyGamepadD2:        Result:='GamepadD #2';
    keyGamepadX2:        Result:='GamepadX #2';
    keyGamepadY2:        Result:='GamepadY #2';
    keyGamepadZ2:        Result:='GamepadZ #2';
    keyGamepadL2:        Result:='GamepadL #2';
    keyGamepadR2:        Result:='GamepadR #2';
    keyGamepadMenu2:     Result:='GamepadMenu #2';

    keyGamepadLeft3:     Result:='GamepadLeft #3';
    keyGamepadUp3:       Result:='GamepadUp #3';
    keyGamepadRight3:    Result:='GamepadRight #3';
    keyGamepadDown3:     Result:='GamepadDown #3';
    keyGamepadA3:        Result:='GamepadA #3';
    keyGamepadB3:        Result:='GamepadB #3';
    keyGamepadC3:        Result:='GamepadC #3';
    keyGamepadD3:        Result:='GamepadD #3';
    keyGamepadX3:        Result:='GamepadX #3';
    keyGamepadY3:        Result:='GamepadY #3';
    keyGamepadZ3:        Result:='GamepadZ #3';
    keyGamepadL3:        Result:='GamepadL #3';
    keyGamepadR3:        Result:='GamepadR #3';
    keyGamepadMenu3:     Result:='GamepadMenu #3';

    keyGamepadLeft4:     Result:='GamepadLeft #4';
    keyGamepadUp4:       Result:='GamepadUp #4';
    keyGamepadRight4:    Result:='GamepadRight #4';
    keyGamepadDown4:     Result:='GamepadDown #4';
    keyGamepadA4:        Result:='GamepadA #4';
    keyGamepadB4:        Result:='GamepadB #4';
    keyGamepadC4:        Result:='GamepadC #4';
    keyGamepadD4:        Result:='GamepadD #4';
    keyGamepadX4:        Result:='GamepadX #4';
    keyGamepadY4:        Result:='GamepadY #4';
    keyGamepadZ4:        Result:='GamepadZ #4';
    keyGamepadL4:        Result:='GamepadL #4';
    keyGamepadR4:        Result:='GamepadR #4';
    keyGamepadMenu4:     Result:='GamepadMenu #4';
    Else
      If (Key>=33) And (Key<=126) Then
        Result := Char(Key)
      Else
        Result:= 'Key #'+IntToString(Key);
  End;
End;

Function GetKeyByName(KeyName:AnsiString):Integer;
Var
  I:Integer;
Begin
  Result:=0;
  If KeyName='' Then
    Exit;

  KeyName := UpStr(KeyName);
  For I:=1 To keyGamepadL4 Do
  If UpStr(GetKeyName(I)) = KeyName Then
  Begin
    Result:=I;
    Exit;
  End;

  Result := StringToInt(KeyName);
End;

Function GetProgramName:AnsiString;
Begin
    {$IFDEF OXYGENE}
    Result := 'TERRA';
    {$ELSE}
  Result := GetFileName(ParamStr(0), True);
  Result := CapStr(Result);
    {$ENDIF}
End;

{$IFNDEF OXYGENE}
Destructor ApplicationComponent.Destroy;
Begin
  // FPC hack
End;
{$ENDIF}

Procedure Application.OnShutdown;
Begin

End;

Procedure Application.PostToFacebook(msg, link, desc, imageURL:AnsiString);
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiFacebook, facebookConnectionError);
End;

Procedure Application.LikeFacebookPage(page, url:AnsiString);
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiFacebook, facebookLikeError);
End;

Procedure Application.SetTitle(Const Name:AnsiString); 
Begin
	If (Name = _Title) Or (Name='') Then
		Exit;
		
	_Title := Name;
End;

Procedure Application.SetSuspend(Value:Boolean);
Var
  I:Integer;
Begin
  If (Value = _Suspended) Then
    Exit;

  _Suspended := Value;
  Log(logDebug, 'App', 'Suspend state = '+BoolToString(Value));

  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    If Not Assigned(_ApplicationComponents[I].Instance) Then
      Continue;

    If (_Suspended) Then
      _ApplicationComponents[I].Instance.Suspend()
    Else
      _ApplicationComponents[I].Instance.Resume();
  End;
End;

Function Application.CanHandleEvents: Boolean;
Begin
  Result := _CanReceiveEvents;
End;

Procedure Application.RefreshComponents();
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RefreshComponents');{$ENDIF}

//  Log(logDebug, 'Application', 'Orientation: ' + IntToString(_Orientation));

  _RefreshingComponents := True;
  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'Running', 'Components: ' + IntToString(_ApplicationComponentCount));{$ENDIF}{$ENDIF}
  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'Running', _ApplicationComponents[I].Component.ClassName);{$ENDIF}{$ENDIF}

    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.Update();

    If Not _Running Then
      Break;
  End;
  _RefreshingComponents := False;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

function Application.InSplashScreen: Boolean;
begin
{$IFDEF SPLASHSCREEN}
  Result := GetTime-_StartTime<=SplashScreenDuration;
{$ELSE}
  Result := False;
{$ENDIF}
End;

Procedure Sleep(Time:Cardinal);
Var
  T, Delta:Cardinal;
Begin
  T := GetTime();
  Repeat
    Delta := GetTime() - T;
  Until (Delta >= Time);
End;

Procedure Application.ShowFullscreenAd;
Begin
  // do nothing
End;

Function Application.IsAppRunning(Name:AnsiString):Boolean;
Begin
  Result := False;
End;

Function Application.IsAppInstalled(Name:AnsiString):Boolean;
Begin
  Result := False;
End;

Function Application.GetPlatform: Cardinal;
Begin
	Result := osUnknown;
  
	{$IFDEF WINDOWS}
  Result := osWindows;
  {$ENDIF}

	{$IFDEF LINUX}
  Result := osLinux;
  {$ENDIF}

	{$IFDEF OSX}
  Result := osMacOS;
  {$ENDIF}

	{$IFDEF ANDROID}
  Result := osAndroid;
  {$ENDIF}

	{$IFDEF OUYA}
  Result := osOuya;
  {$ENDIF}

	{$IFDEF IPHONE}
  Result := osIOS;
  {$ENDIF}

	{$IFDEF WINDOWS_PHONE}
  Result := osWindowsPhone;
  {$ENDIF}
End;

Function Application.GetDeviceID:AnsiString;
Begin
  Result := '';
End;

Function Application.GetControllerCount:Integer;
Begin
  Result := 0;
End;

Procedure Application.SendEmail(DestEmail, Subject, Body:AnsiString);
Begin
End;

Function Application.IsDeviceRooted: Boolean;
Begin
  Result := False;
End;

Function Application.HasInternet: Boolean;
Begin
  Result := True;
End;


Procedure Application.SendAnalytics(EventName:AnsiString);
Begin
  Self.SendAnalytics(EventName, '');
End;

Function Application.InitAccelerometer: Boolean;
Begin
  Result := False;
End;

Function Application.InitCompass: Boolean;
Begin
  Result := False;
End;

Function Application.InitGyroscope: Boolean;
Begin
  Result := False;
End;

Procedure Application.StopAccelerometer;
Begin
  // do nothing
End;

Procedure Application.StopCompass;
Begin
  // do nothing
End;

Procedure Application.StopGyroscope;
Begin
  // do nothing
End;

Procedure Application.SetLanguage(Language:AnsiString);
Var
  I:Integer;
Begin
  If (Language = Self._Language) Then
    Exit;

  Self._Language := Language;
  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.OnLanguageChange();
  End;
End;

{$IFDEF FPC}
Procedure CatchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
Var
  Result:AnsiString;
  I:Integer;
Begin
  Result := 'An unhandled exception occurred at 0x'+ HexStr(PtrUInt(Addr))+ CrLf;
  If Obj is Exception then
  Begin
     Result := Result + Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
  End;
  Result := Result + BackTraceStrFunc(Addr) + CrLf;
  For I:=0 To Pred(FrameCount) Do
    Result := Result + BackTraceStrFunc(Frames[i]) + CrLf;

  Log(logError, 'App', Result);
  Halt();
End;
{$ENDIF}

{$IFNDEF OXYGENE}

Function Application.HasFatalError: Boolean;
Begin
  Result := _FatalError;
End;

Destructor Application.Destroy;
Begin
    If Assigned(_Client) Then
    Begin
      {$IFNDEF OXYGENE}
      Client.Destroy;
      {$ENDIF}
      _Client := Nil;
    End;
End;

Procedure Application.UpdateContextLost;
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'OnContextLost');{$ENDIF}

  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    Log(logDebug, 'App', 'Context lost: '+ _ApplicationComponents[I].Component.ClassName);
    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.OnContextLost();
  End;

  If Assigned(Client) Then
    Client.OnContextLost();

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Function Application.SetOrientation(Value: Integer):Boolean;
Var
  Delta:Single;
  I:Integer;
Begin
  Result := False;

  If (IsInvalidOrientation(Value)) Then
    Begin
        Log(logDebug, 'App', 'Invalid orientation change: '+IntToString(Value));
        Exit;
    End;

  Delta := GetOrientationDelta();
If (_Orientation = Value) {Or (Delta<1)} Then
  Begin
    Log(logDebug, 'App', 'Failed orientation change (delta='+FloatToString(Delta)+')');
    Exit;
  End;

  Log(logDebug, 'App', 'Changing orientation to '+ IntToString(Value));
  _PreviousOrientation := _Orientation;
  _OrientationTime := GetTime();
  _Orientation := Value;

    Case _Orientation Of
    orientationLandscapeLeft:
    Begin
        Log(logDebug, 'App', 'Changing orientation to landscape-left');
    End;

    orientationLandscapeRight:
    Begin
        Log(logDebug, 'App', 'Changing orientation to landscape-right');
    End;

    orientationPortrait:
    Begin
        Log(logDebug, 'App', 'Changing orientation to portrait');
    End;

    orientationPortraitInverted:
    Begin
        Log(logDebug, 'App', 'Changing orientation to portrait-inverted');
    End;
End;


  For I:=0 To (_ApplicationComponentCount-1) Do
  Begin
    Log(logDebug, 'App', 'OnOrientationChange: ' + _ApplicationComponents[I].Component.ClassName);

    If Assigned(_ApplicationComponents[I].Instance) Then
      _ApplicationComponents[I].Instance.OnOrientationChange();
  End;

  Result := True;
End;

Function Application.GetOrientationDelta: Single;
Begin
  Result := GetTime - _OrientationTime;
  Result := Result / OrientationAnimationDuration;
  If (Result>1) Then
    Result := 1
  Else
  If (Result<0) Then
    Result := 0;
End;

Procedure Application.ConvertCoords(var X, Y: Integer);
Var
  PX, PY:Single;
  SX, SY:Single;
  Temp:Integer;
Begin
//  Log(logDebug, 'App', 'PRE1 X'+IntToString(X)+' Y:'+IntToString(Y));

  X := (X - Self._DeviceX1);
  Y := (Y - Self._DeviceY1);

//  Log(logDebug, 'App', 'PRE2 X'+IntToString(X)+' Y:'+IntToString(Y));

  Case _Orientation Of
  orientationLandscapeLeft:
    Begin
      Temp := X;
      X := Y;
      Y := _DeviceWidth - Temp;
    End;

  orientationLandscapeRight:
    Begin
      Temp := X;
      X := _DeviceHeight - Y;
      Y := Temp;
    End;

  orientationPortrait:
    Begin
    End;

  orientationPortraitInverted:
    Begin
      X := _DeviceWidth - X;
      Y := _DeviceHeight - Y;
    End;
  End;

//  Log(logDebug, 'App', 'PRE3 X'+IntToString(X)+' Y:'+IntToString(Y));

  If (IsLandscapeOrientation(Self.Instance.Orientation)) Then
  Begin
    PX := (X * _DeviceScaleY);
    PY := (Y * _DeviceScaleX);
  End Else
  Begin
    PX := (X * _DeviceScaleX);
    PY := (Y * _DeviceScaleY);
  End;

  _MouseOnAdArea := (PX<0) Or (PX>1) Or (PY<0) Or (PY>1);

  X := Trunc( PX * _UIWidth );
  Y := Trunc( PY * _UIHeight);

//  Log(logDebug, 'App', 'PRE4 X'+IntToString(X)+' Y:'+IntToString(Y));
End;

Procedure Application.AddEventToQueue(Action:Integer; X,Y,Z,W:Single; Value:Integer; S:AnsiString; HasCoords:Boolean);
Var
  N:Integer;
Begin
  If Not _CanReceiveEvents Then
    Exit;

  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Locking event mutex');{$ENDIF}{$ENDIF}

  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex.Lock();
  {$ENDIF}


  N := _EventCount;
  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Adding event with index '+IntToString(N));{$ENDIF}{$ENDIF}

  Inc(_EventCount);
  _Events[N].X := X;
  _Events[N].Y := Y;
  _Events[N].Z := Z;
  _Events[N].W := W;
  _Events[N].S := S;
  _Events[N].HasCoords := HasCoords;
  _Events[N].Value := Value;
  _Events[N].Action := Action;

  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Unlocking event mutex');{$ENDIF}{$ENDIF}

  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex.Unlock();
  {$ENDIF}
End;


Procedure Application.AddRectEvent(Action: Integer; X1, Y1, X2, Y2: Single);
Begin
  Self.AddEventToQueue(Action, X1, Y1, X2, Y2, 0, '', True);
End;

Procedure Application.AddVectorEvent(Action:Integer; X,Y,Z:Single);
Begin
  Self.AddEventToQueue(Action, X, Y, Z, 0, 0,  '', True);
End;

Procedure Application.AddCoordEvent(Action:Integer; X,Y, Value:Integer);
Begin
  Self.AddEventToQueue(Action, X, Y, 0, 0, Value, '', True);
End;

Procedure Application.AddValueEvent(Action:Integer; Value:Integer);
Begin
  Self.AddEventToQueue(Action, 0, 0, 0, 0, Value, '', False);
End;

Procedure Application.AddStringEvent(Action:Integer; S:AnsiString);
Begin
  Self.AddEventToQueue(Action, 0, 0, 0, 0, 0, S, False);
End;

{$IFDEF DEBUG_CORE}
Function GetEventTypeName(N:Integer):AnsiString;
Begin
  Case N Of
  eventMouseUp   : Result := 'eventMouseUp';
  eventMouseDown : Result := 'eventMouseDown';
  eventMouseMove : Result := 'eventMouseMove';
  eventMouseWheel : Result := 'eventMouseWheel';
  eventKeyPress   : Result := 'eventKeyPress';
  eventKeyDown    : Result := 'eventKeyDown';
  eventKeyUp      : Result := 'eventKeyUp';
  eventWindowResize   : Result := 'eventWindowResize';
  eventAccelerometer  : Result := 'eventAccelerometer';
  eventGyroscope      : Result := 'eventGyroscope';
  eventCompass        : Result := 'eventCompass';
  eventContextLost    : Result := 'eventContextLost';
  eventOrientation    : Result := 'eventOrientation';
  eventViewport       : Result := 'eventViewport';
  eventIAPPurchase    : Result := 'eventIAPPurchase';
  eventIAPCredits     : Result := 'eventIAPCredits';
  eventIAPError       : Result := 'eventIAPError';
  Else
    Result := '#'+IntToString(N);
  End;
End;
{$ENDIF}

Procedure Application.ProcessEvents;
Var
  I:Integer;
  PX,PY:Integer;
Begin
  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex.Lock();
  {$ENDIF}

  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Processing '+IntToString(_EventCount)+ ' events.');{$ENDIF}{$ENDIF}
  For I:=0 To Pred(_EventCount) Do
  Begin
    If (_Events[I].HasCoords) Then
    Begin
      PX := Trunc(_Events[I].X);
      PY := Trunc(_Events[I].Y);

      Self.ConvertCoords(PX, PY);

      Input.Mouse.X := PX;
      Input.Mouse.Y := PY;
    End;

    {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Events type: '+GetEventTypeName(_Events[I].Action));{$ENDIF}

    If Assigned(Client) Then
    Case _Events[I].Action Of
    eventMouseDown:
      Begin
        {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Mouse down, X:'+IntToString(Input.Mouse.X)+ ' Y:'+IntToString(Input.Mouse.Y));{$ENDIF}
        Self.SetKeyPress(_Events[I].Value, True);

        {Log(logDebug, 'App', 'Mouse down, X:'+IntToString(Input.Mouse.X)+ ' Y:'+IntToString(Input.Mouse.Y));
        Log(logDebug, 'App', 'DeviceX1:'+IntToString(_DeviceX1)+ ' DeviceY1:'+IntToString(_DeviceY1));
        Log(logDebug, 'App', 'DeviceX2:'+IntToString(_DeviceX2)+ ' DeviceY2:'+IntToString(_DeviceY2));
        Log(logDebug, 'App', 'DeviceWidth:'+IntToString(_DeviceWidth)+ ' DeviceHeight:'+IntToString(_DeviceHeight));
        }

        {If (_MouseOnAdArea) Then
          Client.OnAdClick(Input.Mouse.X, Input.Mouse.Y)
        Else}
          Client.OnMouseDown(Input.Mouse.X, Input.Mouse.Y, _Events[I].Value);
      End;

    eventMouseUp:
      Begin
        {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Mouse up, X:'+IntToString(Input.Mouse.X)+ ' Y:'+IntToString(Input.Mouse.Y));{$ENDIF}

        Self.SetKeyPress(_Events[I].Value, False);
        If (Not _MouseOnAdArea) Then
          Client.OnMouseUp(Input.Mouse.X, Input.Mouse.Y, _Events[I].Value);
      End;

    eventMouseMove:
      Begin
        Client.OnMouseMove(Input.Mouse.X, Input.Mouse.Y);
      End;

    eventMouseWheel:
      Begin
        Client.OnMouseWheel(Input.Mouse.X, Input.Mouse.Y, _Events[I].Value);
      End;

    eventKeyPress:
      Begin
        Client.OnKeyPress(_Events[I].Value);
      End;

    eventKeyDown:
      Begin
        Self.SetKeyPress(_Events[I].Value, True);
      End;

    eventKeyUp:
      Begin
        Self.SetKeyPress(_Events[I].Value, False);
      End;

    eventWindowResize:
      Begin
        Log(logDebug, 'App', 'Resizing, W:'+IntToString(Trunc(_Events[I].X))+ ' H:'+IntToString(Trunc(_Events[I].Y)));
        If (_Events[I].X <> _Width) Or (_Events[I].Y <> _Height) Then
          Self.Resize(Trunc(_Events[I].X), Trunc(_Events[I].Y));
      End;

    eventViewport:
      Begin
        Log(logDebug, 'App', 'Device viewport, X1:'+IntToString(Trunc(_Events[I].X))+ ' Y1:'+IntToString(Trunc(_Events[I].Y))+
        ' X2:'+IntToString(Trunc(_Events[I].Z))+ ' Y2:'+IntToString(Trunc(_Events[I].W)));
        //Self.SetViewport(Trunc(_Events[I].X), Trunc(_Events[I].Y), Trunc(_Events[I].Z), Trunc(_Events[I].W));
      End;

    eventAccelerometer:
      Begin
        Input.Accelerometer.X := _Events[I].X;
        Input.Accelerometer.Y := _Events[I].Y;
        Input.Accelerometer.Z := _Events[I].Z;

        Client.OnAccelerometer(Input.Accelerometer.X, Input.Accelerometer.Y, Input.Accelerometer.Z);
      End;

    eventGyroscope:
      Begin
        Input.Gyroscope.X := _Events[I].X;
        Input.Gyroscope.Y := _Events[I].Y;
        Input.Gyroscope.Z := _Events[I].Z;

        Client.OnGyroscope(Input.Gyroscope.X, Input.Gyroscope.Y, Input.Gyroscope.Z);
      End;

    eventCompass:
      Begin
        Input.Compass.X := _Events[I].X;
        Input.Compass.Y := _Events[I].Y;
        Input.Compass.Z := _Events[I].Z;

        Client.OnCompass(Input.Compass.X, Input.Compass.Y, Input.Compass.Z);
      End;

    eventContextLost:
      Begin
        Log(logDebug, 'App', 'App context was lost...');
        _ContextWasLost := True;
      End;

    eventOrientation:
      Begin
        Log(logDebug, 'App', 'Orientation request: ' + IntToString(_Events[I].Value));
        Client.OnOrientation(_Events[I].Value);
      End;

    eventIAPPurchase:
      If _Events[I].S<>'' Then
      Begin
        Log(logDebug, 'App', 'In-app-purchase: ' + _Events[I].S);
        Client.OnIAP_Purchase(_Events[I].S);
      End Else
      Begin
        _Events[I].Value := IAP_PurchaseCanceled;
        Log(logDebug, 'App', 'In-app-purchase error: ' + IntToString(_Events[I].Value));
        Client.OnIAP_Error(_Events[I].Value);
      End;

    eventIAPCredits:
      If (_Events[I].Value>0) Then
      Begin
        Log(logDebug, 'App', 'In-app-purchase: ' + IntToString(_Events[I].Value) + ' credits');
        Client.OnIAP_Purchase(_Events[I].Value);
      End;

    eventIAPError:
      Begin
        Log(logDebug, 'App', 'In-app-purchase error: ' + IntToString(_Events[I].Value));
        Client.OnIAP_Error(_Events[I].Value);
      End;

    End;
  End;

  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Events processed!');{$ENDIF}{$ENDIF}
  _EventCount := 0;

  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex.Unlock();
  {$ENDIF}
End;

Procedure Application.ProcessMessages;
Begin
  // do nothing
End;

Function Application.SaveToCloud: Boolean;
Begin
  Result := False;
End;

Function Application.GetRecommendedSettings: Integer;
Begin
  If (Self.Width<480) Or (Self.Height<480) Then
    Result := settingsHintLow
  Else
    Result := settingsHintMedium;
End;

Procedure Application.Tapjoy_ShowOfferWall;
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
End;

Procedure Application.Tapjoy_ShowVideo;
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
End;

Procedure Application.Tapjoy_SpendCredits(Ammount: Integer);
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
End;

Procedure Application.Tapjoy_Update(Credits: Integer);
Begin
  _TapjoyCredits := IntMax(0, Credits);
End;

Function Application.GetDocumentPath: AnsiString;
Begin
  Result := _DocumentPath;
End;

Function Application.GetStoragePath: AnsiString;
Begin
  Result := _StoragePath;
End;

Function Application.GetTempPath: AnsiString;
Begin
  Result := _TempPath;
End;

Function Application.FrameTime: Cardinal;
Begin
  Result := GetTime() - _FrameStart;
End;

{ FolderManager }
Procedure FolderManager.AddWatcher(Notifier: AssetWatchNotifier);
Begin
  Inc(_NotifierCount);
  SetLength(_Notifiers, _NotifierCount);
  _Notifiers[Pred(_NotifierCount)] := Notifier;
End;

Class Function FolderManager.Instance: FolderManager;
Begin
  If Assigned(_Application_Instance) Then
    Result := _Application_Instance._FolderManager
  Else
    Result := Nil;
End;

Procedure FolderManager.NotifyFileChange(const FileName: AnsiString);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_NotifierCount) Do
    _Notifiers[I](FileName);
End;

Function FolderManager.WatchFolder(const Path: AnsiString):Boolean;
Begin
  // do nothing
  Result := False;
End;

Function Application.GetAspectRatio: Single;
Begin
  Result := SafeDiv(_Height, _Width, 1.0);
End;

Function Application.InitSettings: Boolean;
Begin
  Log(logDebug, 'App', 'Initializing app path');
  {$IFDEF OXYGENE}
  _Path := System.IO.Directory.GetCurrentDirectory();
  {$ELSE}
  GetDir(0, _Path);
  {$ENDIF}
  _Language := 'EN';
  _ContextCounter := 1;
  _CPUCores := 1;

  Result := True;
End;

Initialization
  {$IFDEF FPC}
  ExceptProc := CatchUnhandledException;
  {$ENDIF}

Finalization
  //ShutdownComponents;
{$ENDIF}
End.
