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

{$IFDEF WINDOWS}
{-$DEFINE DEBUG_TAPJOY}
{$ENDIF}

Interface
Uses TERRA_String, SysUtils, TERRA_Client, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D
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
  CallbackBufferSize = 64;

  settingsHintLow   = 0;
  settingsHintMedium = 1;
  settingsHintHigh   = 2;
  settingsHintVeryHigh   = 3;

Type
  ApplicationEvent = Record
    X,Y,Z,W:Single;
    S:TERRAString;
    Value:Integer;
    Action:Integer;
    HasCoords:Boolean;
  End;

  ApplicationScreenDimensions = Record
    Width:Integer;
    Height:Integer;
  End;

  ApplicationCallback = Function(P:Pointer):Boolean; Cdecl;

  ApplicationCallbackEntry = Record
    Callback:ApplicationCallback;
    Time:Cardinal;
    Delay:Cardinal;
    Canceled:Boolean;
    Arg:Pointer;
  End;

  ApplicationClass = Class Of Application;

  ApplicationComponentClass = Class Of ApplicationComponent;

  ApplicationComponent = Class(TERRAObject)
    Protected
      Procedure OnAppResize; Virtual;
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

      Procedure Release; Override;
  End;

  ApplicationObject = Class(TERRAObject)
    Protected
      _Component:ApplicationComponentClass;
      _Dependency:ApplicationComponentClass;
      _Instance:ApplicationComponent;

    Public
      Procedure Release(); Override;

      Property Component:ApplicationComponentClass Read _Component;
      Property Dependency:ApplicationComponentClass Read _Dependency;
      Property Instance:ApplicationComponent Read _Instance;
  End;

  AssetWatchNotifier = Procedure(Const FileName:TERRAString); Cdecl;

  FolderManager = Class(ApplicationComponent)
    Protected
      _Notifiers:Array Of AssetWatchNotifier;
      _NotifierCount:Integer;

    Public
      Class Function Instance:FolderManager;

      Function WatchFolder(Const Path:TERRAString):Boolean; Virtual;

      Procedure NotifyFileChange(Const FileName:TERRAString);

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
			_Title:TERRAString;
			_State:Cardinal;
			_Path:TERRAString;
      _Managed:Boolean;

      _Screen:ApplicationScreenDimensions;

      _Language:TERRAString;
      _Country:TERRAString;

      _Events:Array[0..Pred(EventBufferSize)] Of ApplicationEvent;
      _EventCount:Integer;

      {$IFNDEF DISABLEINPUTMUTEX}
      _InputMutex:CriticalSection;
      {$ENDIF}

      _Callbacks:Array[0..Pred(CallbackBufferSize)] Of ApplicationCallbackEntry;
      _CallbackCount:Integer;

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

      _UIWidth:Integer;
      _UIHeight:Integer;
      _UIScale:Single;

      _DeviceX1:Integer;
      _DeviceY1:Integer;
      _DeviceX2:Integer;
      _DeviceY2:Integer;
      _DeviceWidth:Integer;
      _DeviceHeight:Integer;
      _DeviceScaleX:Single;
      _DeviceScaleY:Single;

      _BundleVersion:TERRAString;

      _CurrentUser:TERRAString;

      _ChangeToFullScreen:Boolean;

      _Terminated:Boolean;

      _Orientation:Integer;
      _PreviousOrientation:Integer;
      _OrientationTime:Integer;

      _MouseOnAdArea:Boolean;

      _CPUCores:Integer;

      _TapjoyCredits:Integer;

      _DocumentPath:TERRAString;
      _StoragePath:TERRAString;
      _TempPath:TERRAString;
      _FontPath:TERRAString;

      _FrameStart:Cardinal;

      _FolderManager:FolderManager;

  		_DebuggerPresent:Boolean;

      Function IsDebuggerPresent:Boolean; Virtual;

			{$IFDEF HASTHREADS}
      //_InputThread:Thread;
      {$ENDIF}

			Function InitWindow:Boolean; Virtual; Abstract;
			Procedure CloseWindow; Virtual; Abstract;

      Procedure OnShutdown; Virtual;

			Procedure InitSystem;
			Procedure ShutdownSystem;

      Procedure ProcessMessages; Virtual;
      Procedure ProcessCallbacks;

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

      Function GetTempPath():TERRAString; Virtual;
      Function GetDocumentPath():TERRAString; Virtual;
      Function GetStoragePath():TERRAString; Virtual;

      Procedure AddEventToQueue(Action:Integer; X,Y,Z,W:Single; Value:Integer; S:TERRAString; HasCoords:Boolean);

    Public

			Constructor Create(Client:AppClient);

      Procedure Release; Override;

			Function Run:Boolean; Virtual;

			Procedure Terminate(ForceClose:Boolean = False);Virtual;

      Procedure SetViewport(X1, Y1, X2, Y2:Integer);
                                              Procedure AddRectEvent(Action:Integer; X1,Y1,X2,Y2:Single); Overload;
      Procedure AddVectorEvent(Action:Integer; X,Y,Z:Single); Overload;
      Procedure AddCoordEvent(Action:Integer; X,Y, Value:Integer); Overload;
      Procedure AddValueEvent(Action:Integer; Value:Integer); Overload;
      Procedure AddStringEvent(Action:Integer; S:TERRAString); Overload;

      Function SetOrientation(Value:Integer):Boolean; Virtual;

			Procedure SetState(State:Cardinal); Virtual;
			Procedure SwapBuffers; Virtual;
      Procedure Yeld; Virtual;

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Virtual;
      Procedure ToggleFullscreen;

      // ads
      Procedure EnableAds; Virtual;
      Procedure DisableAds; Virtual;
      Procedure ShowFullscreenAd; Virtual;

      Procedure OpenAppStore(AppID:TERRAString); Virtual;
      Procedure SendEmail(DestEmail, Subject, Body:TERRAString); Virtual;

      Function SaveToCloud():Boolean; Virtual;

      Function InputForceFeedback(ControllerID, PadID:Integer; Duration:Integer):Boolean; Virtual;

      Function HasInternet:Boolean; Virtual;

      Function HasFatalError:Boolean;

      Function ExecuteLater(Callback:ApplicationCallback; Const Delay:Cardinal; Arg:Pointer = Nil):Boolean;
      Procedure CancelCallback(Arg:Pointer);

      //analytics
      Procedure SendAnalytics(EventName:TERRAString); {$IFNDEF OXYGENE}Overload; {$ENDIF} Virtual;
      Procedure SendAnalytics(EventName:TERRAString; Parameters:TERRAString); {$IFNDEF OXYGENE}Overload;{$ENDIF} Virtual;

      // achievements
      Procedure UnlockAchievement(AchievementID:TERRAString); Virtual;

      // facebook
      Procedure PostToFacebook(msg, link, desc, imageURL:TERRAString); Virtual;
      Procedure LikeFacebookPage(page, url:TERRAString); Virtual;

      //tapjoy
      Procedure Tapjoy_ShowOfferWall(); Virtual;
      Procedure Tapjoy_ShowVideo(); Virtual;
      Procedure Tapjoy_SpendCredits(Ammount:Integer); Virtual;
      Procedure Tapjoy_Update(Credits: Integer);

      Procedure Resize(Width, Height:Integer);

      Procedure SetSuspend(Value:Boolean);

      Procedure SetTitle(Const Name:TERRAString); Virtual;

      Procedure SetLanguage(Language:TERRAString);

      Procedure ProcessEvents;
      Procedure RefreshComponents();

      Function GetDeviceID():TERRAString; Virtual;

      Function GetElapsedTime:Cardinal;

      Function GetPlatform:Cardinal;

      Function CanHandleEvents:Boolean;

      Function FrameTime:Cardinal;

      Function GetRecommendedSettings():Integer; Virtual;

      Function InitAccelerometer():Boolean; Virtual;
      Function InitGyroscope():Boolean; Virtual;
      Function InitCompass():Boolean; Virtual;

      Procedure StopAccelerometer(); Virtual;
      Procedure StopGyroscope(); Virtual;
      Procedure StopCompass(); Virtual;

      Function IsAppRunning(Name:TERRAString):Boolean; Virtual;
      Function IsAppInstalled(Name:TERRAString):Boolean; Virtual;
      Function IsDeviceRooted:Boolean; Virtual;

      Function GetOrientationDelta:Single;

      Class Function Instance:Application;

      Property Client:AppClient Read _Client;

      Property CPUCores:Integer Read _CPUCores;

			Property Handle:Cardinal Read _Handle;
			Property OS:Cardinal Read GetPlatform;
			Property CurrentPath:TERRAString Read _Path;
      Property TempPath:TERRAString Read GetTempPath;
      Property StoragePath:TERRAString Read GetStoragePath;
      Property DocumentPath:TERRAString Read GetDocumentPath;
      Property FontPath:TERRAString Read _FontPath;

			Property Title:TERRAString Read _Title;
			Property Width:Integer Read _Width;
			Property Height:Integer Read _Height;
			Property FullScreen:Boolean Read _Fullscreen;
      Property Language:TERRAString Read _Language Write SetLanguage;
      Property Country:TERRAString Read _Country;
      Property BundleVersion:TERRAString Read _BundleVersion;
      Property CurrentUser:TERRAString Read _CurrentUser;

      Property UI_Width:Integer Read _UIWidth;
      Property UI_Height:Integer Read _UIHeight;
      Property UI_Scale:Single Read _UIScale;

      Property IsRunning:Boolean Read _Running;

      Property Paused:Boolean Read _Paused Write SetPause;
      Property CanReceiveEvents:Boolean Read _CanReceiveEvents;

      Property ContextID:Integer Read _ContextCounter;

      Property TapjoyCredits:Integer Read _TapjoyCredits;

      Property Orientation:Integer Read _Orientation;
      Property PreviousOrientation:Integer Read _PreviousOrientation;

      Property AspectRatio:Single Read GetAspectRatio;

      Property IsConsole:Boolean Read _IsConsole;

      Property Screen:ApplicationScreenDimensions Read _Screen;

      Property DebuggerPresent:Boolean Read _DebuggerPresent;

	End;

Var
  _Prefetching:Boolean;

Procedure ApplicationStart(Client:AppClient);
Function InitializeApplicationComponent(TargetClass, DestroyBefore:ApplicationComponentClass):ApplicationObject;

Function Blink(Period:Cardinal):Boolean;
Procedure Sleep(Time:Cardinal);

Function GetOSName(OS:Integer=0):TERRAString;
Function GetProgramName():TERRAString;

Function IsLandscapeOrientation(Orientation:Integer):Boolean;
Function IsPortraitOrientation(Orientation:Integer):Boolean;
Function IsInvalidOrientation(Orientation:Integer):Boolean;

Implementation
Uses TERRA_Error, {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  {TERRA_Callstack, }TERRA_Log, TERRA_OS, TERRA_IAP, TERRA_Localization, TERRA_FileUtils, TERRA_FileManager, TERRA_InputManager 
  {$IFDEF PC}, TERRA_Steam{$ENDIF};

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
  S:TERRAString;
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
  Report:TERRAString;
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

class function Application.Instance: Application;
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
  S:TERRAString;

  Function GetDependencyCount(T:ApplicationComponentClass):Integer;
  Var
    J:Integer;
    S3,S2:TERRAString;
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
        ReleaseObject(_ApplicationComponents[I]);
        
        _ApplicationComponents[I] := _ApplicationComponents[Pred(_ApplicationComponentCount)];
        Dec(_ApplicationComponentCount);
        Break;
      End;
    End;
  Until _ApplicationComponentCount<=0;
End;

procedure Application.ShutdownSystem;
Begin
  ShutdownComponents;

  If (Not _IsConsole) Then
  Begin
    CloseGraphics();
  End;

  _Ready := False;
  _CanReceiveEvents := False;

  {$IFNDEF DISABLEINPUTMUTEX}
  ReleaseObject(_InputMutex);
  {$ENDIF}

  If (Not _Managed) And (Not _IsConsole) Then
  Begin
    CloseWindow;
  End;

  Self.OnShutdown;
End;


procedure Application.InitSystem;
Var
  I:Integer;
  S:TERRAString;
Begin
  Log(logDebug, 'App', 'Initializing randomizer');

  {$IFNDEF OXYGENE}
  System.Randomize;
  {$ENDIF}

  {$IFDEF DEBUG_TAPJOY}
  _TapjoyCredits := 250;
  {$ELSE}
  _TapjoyCredits := 0;
  {$ENDIF}

  Log(logDebug, 'App', 'Creating critical section for input');

  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex := CriticalSection.Create({'app_input'});
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

  _BundleVersion := '0.0';

  Log(logDebug, 'App', 'Initializing settings');
  If (Not InitSettings()) Then
    Halt(0);

  _Title := Self.Client.GetTitle();
  _Width := Self.Client.GetWidth();
  _Height := Self.Client.GetHeight();
  _Fullscreen := Self.Client.GetFullscreen();
  _AntialiasSamples := Self.Client.GetAntialiasSamples();

  {$IFDEF PC}
  If (Steam.Instance.Enabled) And (IsSupportedLanguage(Steam.Instance.Language)) Then
    _Language := Steam.Instance.Language;
  {$ENDIF}

  {$IFNDEF MOBILE}
  If (Not _Managed) And (Not _IsConsole) Then
  {$ENDIF}
    InitWindow;

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
      _UIScale := 1.0;
      Client.SelectResolution2D(_UIWidth, _UIHeight, _UIScale);
      Log(logDebug, 'App', 'Selected UI resolution: '+IntToString(_UIWidth)+' x ' +IntToString(_UIHeight));
      Log(logDebug, 'App', 'Selected UI scale: '+FloatToString(_UIScale));
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

constructor Application.Create(Client: AppClient);
Begin
  _Client := Client;

  _Startup := True;
  _CanReceiveEvents := False;
  _Hidden := Self.Client.GetHidden();
  _Handle := Self.Client.GetHandle();
  _State := wsNormal;

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

procedure Application.Finish;
Begin
  _Running := False;

  If Assigned(_Client) Then
    Client.OnDestroy;

  Log(logWarning, 'App', 'Shutting down all subsystems.');
  ShutdownSystem;
  Log(logWarning, 'App', 'All subsystems destroyed.');

  {$IFNDEF OXYGENE}
  If (Not _Managed) Then
    Self.Release;
  {$ENDIF}

  _Application_Instance := Nil;

  Log(logWarning, 'App', 'Application has shutdown.')
End;

procedure Application.Terminate(ForceClose: Boolean);
Begin
  If (Self = Nil) Then
    Halt;

  _Terminated := True;
  _CanReceiveEvents := False;

  {$IFNDEF MOBILE}
  If ForceClose Then
  Begin
    Self.Finish();
    Halt;
  End;
  {$ENDIF}
End;

procedure Application.Resize(Width, Height: Integer);
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


procedure Application.SetViewport(X1, Y1, X2, Y2: Integer);
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

Function Application.Run: Boolean;
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

    If (Not _IsConsole) Then
    Begin
    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Processing messages');{$ENDIF}{$ENDIF}
      Self.ProcessMessages();
    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'All messages processed');{$ENDIF}{$ENDIF}
    End;

    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Processing callbacks');{$ENDIF}{$ENDIF}
    Self.ProcessCallbacks();
    {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'All callbacks processed');{$ENDIF}{$ENDIF}

    If Assigned(Client) Then
    Begin
      If (_ClientInit) Then
      Begin
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
          If (InputManager.Instance.Keys.IsDown(keyEscape)) Then
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


    If (Not _IsConsole) Then
    Begin
      {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'App', 'Swapping buffers');{$ENDIF}{$ENDIF}
      Self.SwapBuffers;

      If (_ChangeToFullScreen) Then
      Begin
	_ChangeToFullScreen := False;
	ToggleFullScreen;
      End;
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

  Log(logDebug, 'App', 'Application is finishing...');

  Self.Finish();
  Result := False;
End;


Function GetOSName(OS:Integer=0):TERRAString;
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

function Application.SetFullscreenMode(UseFullScreen: Boolean): Boolean;
Begin
  Log(logError, 'App','ToggleFullscreen not implemented!');
  Result := False;
End;

procedure Application.ToggleFullscreen;
Var
   NewMode:Boolean;
Begin
     NewMode := Not Self._Fullscreen;
     If SetFullscreenMode(NewMode) Then
        Self._Fullscreen := NewMode;
End;

procedure Application.SwapBuffers;
Begin
 Log(logError, 'App','SwapBuffers not implemented!');
End;

procedure Application.SetState(State: Cardinal);
Begin
 Log(logError, 'App','SetState not implemented!');
End;

procedure Application.Yeld;
Begin
 Log(logError, 'App','Yeld not implemented!');
End;

Procedure ApplicationStart(Client:AppClient);
Begin
  _FatalError := False;
  LoggingDisabled := Not Client.GetLogging();

  Log(logDebug,'App', 'Loading application settings.');

  _Application_Ready := True;
  _Application_Instance := CreateApplicationClass(Client);

  Log(logDebug,'App', 'Starting engine.');

  {$IFNDEF ISLIBRARY}
  _Application_Instance.Run();
  {$ENDIF}
End;

procedure Application.SetPause(Value: Boolean);
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

function Application.GetElapsedTime: Cardinal;
Begin
  If (Application.Instance.Paused) Then
    Result := _PauseStart
  Else
    Result := GetTime() - _PauseCounter;
End;

// do nothing
procedure Application.EnableAds; Begin End;
procedure Application.DisableAds; Begin End;

procedure Application.OpenAppStore(AppID: TERRAString); Begin End;

{ ApplicationObject }
Procedure ApplicationObject.Release;
Var
  S:TERRAString;
Begin
  If Assigned(_Instance) Then
  Begin
    S := _Instance.ClassName;
    Log(logDebug, 'App', 'Shutting down '+S);
  End;

  ReleaseObject(_Instance);
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
Procedure ApplicationComponent.Update;  Begin End;
Procedure ApplicationComponent.Resume;  Begin End;
Procedure ApplicationComponent.Suspend; Begin End;
Procedure ApplicationComponent.OnLanguageChange; Begin End;
Procedure ApplicationComponent.OnContextLost; Begin End;
Procedure ApplicationComponent.OnOrientationChange; Begin End;
Procedure ApplicationComponent.OnViewportChange(X1, Y1, X2, Y2:Integer); Begin End;

procedure Application.SendAnalytics(EventName: TERRAString;
  Parameters: TERRAString); Begin End;
procedure Application.UnlockAchievement(AchievementID: TERRAString); Begin End;
function Application.IsDebuggerPresent: Boolean; Begin Result := False; End;


Function Blink(Period:Cardinal):Boolean;
Begin
  Result := ((GetTime() Shr 4) Mod Period<(Period Shr 1));
End;

Function GetProgramName:TERRAString;
Begin
    {$IFDEF OXYGENE}
    Result := 'TERRA';
    {$ELSE}
  Result := GetFileName(ParamStr(0), True);
  //Result := CapStr(Result);
    {$ENDIF}
End;

{$IFNDEF OXYGENE}
Procedure ApplicationComponent.Release;
Begin
  // FPC hack
End;
{$ENDIF}

procedure Application.OnShutdown;
Begin

End;

procedure Application.PostToFacebook(msg, link, desc, imageURL: TERRAString);
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiFacebook, facebookConnectionError);
End;

procedure Application.LikeFacebookPage(page, url: TERRAString);
Begin
  If Assigned(Client) Then
    Client.OnAPIResult(apiFacebook, facebookLikeError);
End;

procedure Application.SetTitle(const Name: TERRAString);
Begin
	If (Name = _Title) Or (Name='') Then
		Exit;
		
	_Title := Name;
End;

procedure Application.SetSuspend(Value: Boolean);
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

function Application.CanHandleEvents: Boolean;
Begin
  Result := _CanReceiveEvents;
End;

procedure Application.RefreshComponents;
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

  {$IFDEF DEBUG_CORE}{$IFDEF EXTENDED_DEBUG}Log(logDebug, 'Running', 'Finished refreshing components!');{$ENDIF}{$ENDIF}
  
  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
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

procedure Application.ShowFullscreenAd;
Begin
  // do nothing
End;

function Application.IsAppRunning(Name: TERRAString): Boolean;
Begin
  Result := False;
End;

function Application.IsAppInstalled(Name: TERRAString): Boolean;
Begin
  Result := False;
End;

function Application.GetPlatform: Cardinal;
Begin
	Result := osUnknown;
  
	{$IFDEF WINDOWS}
  Result := osWindows;
  {$ENDIF}

	{$IFDEF LINUX}
  Result := osLinux;
  {$ENDIF}

	{$IFDEF OSX}
  Result := osOSX;
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

function Application.GetDeviceID: TERRAString;
Begin
  Result := '';
End;

procedure Application.SendEmail(DestEmail, Subject, Body: TERRAString);
Begin
End;

function Application.IsDeviceRooted: Boolean;
Begin
  Result := False;
End;

function Application.HasInternet: Boolean;
Begin
  Result := True;
End;


procedure Application.SendAnalytics(EventName: TERRAString);
Begin
  Self.SendAnalytics(EventName, '');
End;

function Application.InitAccelerometer: Boolean;
Begin
  Result := False;
End;

function Application.InitCompass: Boolean;
Begin
  Result := False;
End;

function Application.InitGyroscope: Boolean;
Begin
  Result := False;
End;

procedure Application.StopAccelerometer;
Begin
  // do nothing
End;

procedure Application.StopCompass;
Begin
  // do nothing
End;

procedure Application.StopGyroscope;
Begin
  // do nothing
End;

procedure Application.SetLanguage(Language: TERRAString);
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
  Result:TERRAString;
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

Function Application.HasFatalError: Boolean;
Begin
  Result := _FatalError;
End;

Procedure Application.Release;
Begin
  ReleaseObject(_Client);
End;

procedure Application.UpdateContextLost;
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

function Application.SetOrientation(Value: Integer): Boolean;
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

function Application.GetOrientationDelta: Single;
Begin
  Result := GetTime - _OrientationTime;
  Result := Result / OrientationAnimationDuration;
  If (Result>1) Then
    Result := 1
  Else
  If (Result<0) Then
    Result := 0;
End;

procedure Application.ConvertCoords(var X, Y: Integer);
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

procedure Application.AddEventToQueue(Action: Integer; X, Y, Z, W: Single;
  Value: Integer; S: TERRAString; HasCoords: Boolean);
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


procedure Application.AddRectEvent(Action: Integer; X1, Y1, X2, Y2: Single);
Begin
  Self.AddEventToQueue(Action, X1, Y1, X2, Y2, 0, '', True);
End;

procedure Application.AddVectorEvent(Action: Integer; X, Y, Z: Single);
Begin
  Self.AddEventToQueue(Action, X, Y, Z, 0, 0,  '', True);
End;

procedure Application.AddCoordEvent(Action: Integer; X, Y, Value: Integer);
Begin
  Self.AddEventToQueue(Action, X, Y, 0, 0, Value, '', True);
End;

procedure Application.AddValueEvent(Action: Integer; Value: Integer);
Begin
  Self.AddEventToQueue(Action, 0, 0, 0, 0, Value, '', False);
End;

procedure Application.AddStringEvent(Action: Integer; S: TERRAString);
Begin
  Self.AddEventToQueue(Action, 0, 0, 0, 0, 0, S, False);
End;

{$IFDEF DEBUG_CORE}
Function GetEventTypeName(N:Integer):TERRAString;
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

procedure Application.ProcessEvents;
Var
  I:Integer;
  PX,PY:Integer;
  Input:InputManager;
Begin
  {$IFNDEF DISABLEINPUTMUTEX}
  _InputMutex.Lock();
  {$ENDIF}

  Input := InputManager.Instance;

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
        Input.Keys.SetState(_Events[I].Value, True);

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

        Input.Keys.SetState(_Events[I].Value, False);
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
        Input.Keys.SetState(_Events[I].Value, True);
      End;

    eventKeyUp:
      Begin
        Input.Keys.SetState(_Events[I].Value, False);
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

procedure Application.ProcessMessages;
Begin
  // do nothing
End;

function Application.SaveToCloud: Boolean;
Begin
  Result := False;
End;

Function Application.InputForceFeedback(ControllerID, PadID: Integer; Duration: Integer):Boolean;
Begin
     Result := False;
End;

function Application.GetRecommendedSettings: Integer;
Begin
  If (Self.Width<480) Or (Self.Height<480) Then
    Result := settingsHintLow
  Else
    Result := settingsHintMedium;
End;

procedure Application.Tapjoy_ShowOfferWall;
Begin
  If Assigned(Client) Then
  Begin
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
  End;
End;

procedure Application.Tapjoy_ShowVideo;
Begin
  If Assigned(Client) Then
  Begin
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
  End;
End;

procedure Application.Tapjoy_SpendCredits(Ammount: Integer);
Begin
  If Assigned(Client) Then
  Begin
    {$IFDEF DEBUG_TAPJOY}
    Client.OnAPIResult(apiTapJoy, tapjoySpendSuccess);
    {$ELSE}
    Client.OnAPIResult(apiTapJoy, tapjoyConnectionError);
    {$ENDIF}
  End;
End;

procedure Application.Tapjoy_Update(Credits: Integer);
Begin
  _TapjoyCredits := IntMax(0, Credits);
End;

function Application.GetDocumentPath: TERRAString;
Begin
  Result := _DocumentPath;
End;

function Application.GetStoragePath: TERRAString;
Begin
  Result := _StoragePath;
End;

function Application.GetTempPath: TERRAString;
Begin
  Result := _TempPath;
End;

function Application.FrameTime: Cardinal;
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

Procedure FolderManager.NotifyFileChange(const FileName: TERRAString);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_NotifierCount) Do
    _Notifiers[I](FileName);
End;

Function FolderManager.WatchFolder(const Path: TERRAString):Boolean;
Begin
  // do nothing
  Result := False;
End;

Function Application.GetAspectRatio: Single;
Begin
  Result := SafeDiv(_Height, _Width, 1.0);
End;

Function Application.ExecuteLater(Callback:ApplicationCallback; Const Delay:Cardinal; Arg:Pointer):Boolean;
Begin 
  If (_CallbackCount>=CallbackBufferSize) Then
  Begin
    Result := False;
    Exit;
  End;

  _Callbacks[_CallbackCount].Callback := Callback;
  _Callbacks[_CallbackCount].Time := GetTime() + Delay;
  _Callbacks[_CallbackCount].Delay := Delay;
  _Callbacks[_CallbackCount].Arg := Arg;
  _Callbacks[_CallbackCount].Canceled := False;

  Inc(_CallbackCount);

  Result := True;
End;

Procedure Application.ProcessCallbacks();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_CallbackCount) Do
  Begin
    If (_Callbacks[I].Canceled) Then
      Continue;

    If (_Callbacks[I].Time<=GetTime()) Then
    Begin
      Log(logDebug, 'Game','Executing callback...');
      
      If _Callbacks[I].Callback(_Callbacks[I].Arg) Then
      Begin
        _Callbacks[I].Time := GetTime() + _Callbacks[I].Delay;
      End Else
        _Callbacks[I].Canceled := True;
    End;
  End;

  While (I<_CallbackCount) Do
  If (_Callbacks[I].Canceled) Then
  Begin
    _Callbacks[Pred(_CallbackCount)] := _Callbacks[I];
    Dec(_CallbackCount);
  End Else
    Inc(I);
End;

Procedure Application.CancelCallback(Arg:Pointer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_CallbackCount) Do
  If (_Callbacks[I].Arg = Arg) Then
  Begin
    _Callbacks[I].Canceled := True;
  End;
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


  _DebuggerPresent := Self.IsDebuggerPresent();

  Result := True;
End;


Initialization
  {$IFDEF FPC}
  ExceptProc := CatchUnhandledException;
  {$ENDIF}

Finalization
  //ShutdownComponents;
End.