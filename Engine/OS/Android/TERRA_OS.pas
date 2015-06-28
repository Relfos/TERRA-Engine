// Android port
Unit TERRA_OS;
{$I terra.inc}


Interface
Uses cmem, TERRA_String, TERRA_Error, TERRA_Utils, TERRA_Application,
    TERRA_Vector3D, TERRA_Java, TERRA_Collections, TERRA_CollectionObjects,
    sysutils,dateutils,unix, jni, BaseUnix;

Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 8;
	keyTab        = 9;
	keyEnter      = 13;
	keyShift      = 1;
	keyControl    = 2;
	keyAlt        = 3;
	keyPause      = 17;
	keyEscape     = 4; // $35;
	keySpace      = $31;
	keyPageUp     = $74;
	keyPageDown   = $79;
	keyEnd        = $77;
	keyHome       = $73;

	keyLeft       = $7B;
	keyUp         = $7E;
	keyRight      = $7C;
	keyDown       = $7D;

	keyInsert     = 18;
	keyDelete     = 19;
	keyPlus       = 20;
	keyMinus      = 21;
	keyPeriod     = 22;

  keyF1        = 200;
  keyF2        = 201;
  keyF3        = 202;
  keyF4        = 203;
  keyF5        = 204;
  keyF6        = 205;
  keyF7        = 206;
  keyF8        = 207;
  keyF9        = 208;
  keyF10       = 209;
  keyF11       = 210;
  keyF12       = 211;

  keyA = Ord('A');
  keyB = Ord('B');
  keyC = Ord('C');
  keyD = Ord('D');
  keyE = Ord('E');
  keyF = Ord('F');
  keyG = Ord('G');
  keyH = Ord('H');
  keyI = Ord('I');
  keyJ = Ord('J');
  keyK = Ord('K');
  keyL = Ord('L');
  keyM = Ord('M');
  keyN = Ord('N');
  keyO = Ord('O');
  keyP = Ord('P');
  keyQ = Ord('Q');
  keyR = Ord('R');
  keyS = Ord('S');
  keyT = Ord('T');
  keyU = Ord('U');
  keyV = Ord('V');
  keyW = Ord('W');
  keyX = Ord('X');
  keyY = Ord('Y');
  keyZ = Ord('Z');

  UtilsClassPath = 'com.pascal.terra.TERRAUtils';
  ActivityClassPath = 'com.pascal.terra.TERRAActivity';

Type
  JavaString = JString;

Type
  AndroidApplication = Class(BaseApplication)
    Protected
      _Focus :Boolean;

      _Utils:JavaClass;

      _TestedDebug:Boolean;
      _DebuggerAttached:Boolean;

      Function InitSettings:Boolean; Override;
      Function InitWindow:Boolean; Override;
      Procedure CloseWindow; Override;

      Procedure GainFocus;
      Procedure LostFocus;

      Procedure OnShutdown; Override;

	Public
      Constructor Create();

      Procedure SetState(State:Cardinal); Override;

      Procedure EnableAds; Override;
      Procedure DisableAds; Override;
      Procedure ShowFullscreenAd; Override;
      Procedure OpenAppStore(AppID:TERRAString); Override;

      Procedure PostToFacebook(msg, link, desc, imageURL:TERRAString); Override;
      Procedure LikeFacebookPage(page, url:TERRAString); Override;

      Function InitAccelerometer():Boolean; Override;
      Function InitGyroscope():Boolean; Override;
      Function InitCompass():Boolean; Override;

      Procedure StopAccelerometer(); Override;
      Procedure StopGyroscope(); Override;
      Procedure StopCompass(); Override;

      Function SaveToCloud: Boolean; Override;

      Procedure ListFile(Name:Pointer; Size:Integer);

      Procedure SendEmail(DestEmail, Subject, Body:TERRAString); Override;

      Procedure Tapjoy_ShowOfferWall(); Override;
      Procedure Tapjoy_ShowVideo(); Override;
      Procedure Tapjoy_SpendCredits(Ammount:Integer); Override;

      Function IsAppRunning(Name:TERRAString):Boolean; Override;
      Function IsAppInstalled(Name:TERRAString):Boolean; Override;

      Function GetDeviceID():TERRAString; Override;

      Function IsDebuggerPresent:Boolean; Override;

      Procedure SendAnalytics(EventName:TERRAString; Values:TERRAString=''); Override;

      Procedure OnFatalError(Const ErrorMsg, CrashLog, Callstack:TERRAString); Override;

      Class Procedure DisplayMessage(S:TERRAString);
      Class Function GetCurrentTime:TERRATime;
      Class Function GetCurrentDate:TERRADate;
      Class Function GetTime:Cardinal;

      Class Function Instance:AndroidApplication;
  End;

  Application = AndroidApplication;

Procedure focusKeyboard(s:PAnsiChar);
Function StartupWithVM(VM:Pointer):Integer;

Procedure Cache_Java_Classes(Env:PJNIEnv);

Procedure ApplicationThreadExecute(P:Integer);

Implementation
Uses TERRA_Log, TERRA_IAP, TERRA_FileSearch, TERRA_Facebook, TERRA_SoundSource, TERRA_MusicManager,
  TERRA_Threads, TERRA_Callstack, TERRA_FileStream, TERRA_Renderer, TERRA_GLES2Renderer;

Var
	_ApplicationInstance:AndroidApplication = Nil;

Procedure Cache_Java_Classes(Env:PJNIEnv);
Begin
  Java_CacheClass(Env, ExceptionJavaClass);
  Java_CacheClass(Env, ActivityClassPath);
  Java_CacheClass(Env, UtilsClassPath);
  Java_CacheClass(Env, JavaMusicPlayerClassName);
  Java_CacheClass(Env, AudioTrackJavaClass);
  Java_CacheClass(Env, FacebookClassPath);
  Java_CacheClass(Env, FileIOClassPath);
End;

Function StartupWithVM(VM:Pointer):Integer;
Begin
  curVM := vm;
  Result := JNI_VERSION_1_6;
End;

Procedure ApplicationThreadExecute(P:Integer);
Begin
  InternalThreadDispatcher(Pointer(P));
End;

Procedure focusKeyboard(s:PAnsiChar);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  If (_ApplicationInstance=Nil) Or (_ApplicationInstance._Utils=Nil) Then
    Exit;


  Java_Begin(Frame);
  Params := JavaArguments.Create(Nil);
  Params.AddString(S);
  _ApplicationInstance._Utils.CallStaticVoidMethod(Frame, 'showKeyboard', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Var
  _Application_Instance:AndroidApplication;

Constructor AndroidApplication.Create();
Begin
  _Application_Instance := Self;
  Inherited Create();
End;

Class Function AndroidApplication.Instance:AndroidApplication;
Begin
  Result := _Application_Instance;
End;

Class Procedure AndroidApplication.DisplayMessage(S:TERRAString);
Begin
	S := S +#0;
//  Log(logWarning, 'App', S);
End;

Class Function AndroidApplication.GetCurrentTime:TERRATime;
Var
 Datetime:Tdatetime;
Begin
 datetime := Now();
 Result.Hour     := hourof( datetime );
 Result.minute   := minuteof( datetime );
 Result.second   := secondof( datetime );
 Result.MiliSecond  := millisecondof( datetime );
End;

Class Function AndroidApplication.GetCurrentDate:TERRADate;
Var
 Datetime:Tdatetime;
Begin
 datetime := Today();
  Result.Year := yearof(datetime);
  Result.Month := monthof(datetime);
  Result.Day := dayof(datetime);
  Result.WeekDay := dayoftheweek(datetime);
End;

Type
	PtTimeSpec = ^tTimeSpec;
Function clock_gettime(clkid:Integer; t:PtTimeSpec):Integer; cdecl; external;
Const CLOCK_MONOTONIC = 1;

Class Function AndroidApplication.GetTime:Cardinal;
Var
  ts: TTimeSpec;
  i: Int64;
Begin
  clock_gettime(CLOCK_MONOTONIC, @ts);
  i := ts.tv_sec;
  i := i*1000 + ts.tv_nsec div 1000000;
  Result := (I And $ffffffff); //cut to unsig 32bit

  {$IFDEF USE_TIME_MULTIPLIER}
  Result := Result * TimerMultiplier;
  {$ENDIF}
End;

Function AndroidApplication.InitWindow:Boolean;
Begin
  Result := True;
End;

Procedure AndroidApplication.PostToFacebook(msg, link, desc, imageURL:TERRAString);
Var
  FB:Facebook;
Begin
  If (Application.Instance.GetFacebookID()='') Then
  Begin
    Self.OnAPIResult(apiFacebook, facebookConnectionError);
    Exit;
  End;

  FB := Facebook.Create();
  FB.Post(Msg, Link, Desc, ImageURL);
  ReleaseObject(FB);
End;


Procedure AndroidApplication.LikeFacebookPage(page, url:TERRAString);
Var
  FB:Facebook;
Begin
  If (Application.Instance.GetFacebookID()='') Then
  Begin
    Self.OnAPIResult(apiFacebook, facebookConnectionError);
    Exit;
  End;

  FB := Facebook.Create();
  FB.LikePage(Page, url);
  ReleaseObject(FB);
End;

Procedure AndroidApplication.CloseWindow;
Begin
End;

Procedure AndroidApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure AndroidApplication.ShowFullscreenAd;
Var
  Frame:JavaFrame;
Begin
  {$IFDEF OUYA}
  Log(logDebug, 'App','Ads not supported in OUYA!');
  Exit;
  {$ELSE}

  Log(logDebug, 'App', 'Showing fullscreen ad..');
  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'showFullscreenAds', Nil);
  Java_End(Frame);
  {$ENDIF}
End;

Procedure AndroidApplication.EnableAds;
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  {$IFDEF OUYA}
  Log(logDebug, 'App','Ads not supported in OUYA!');
  Exit;
  {$ELSE}

  Log(logDebug, 'App', 'Caching fullscreen ads..');
  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Application.Instance.GetAdMobBannerID());
  _Utils.CallStaticVoidMethod(Frame, 'enableAds', Params);
  ReleaseObject(Params);
  Java_End(Frame);
  {$ENDIF}
End;

Procedure AndroidApplication.DisableAds;
Var
  Frame:JavaFrame;
Begin
  {$IFDEF OUYA}
  Log(logDebug, 'App','Ads not supported in OUYA!');
  Exit;
  {$ELSE}

  Log(logDebug, 'App', 'Caching fullscreen ads..');
  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'disableAds', Nil);
  Java_End(Frame);
  {$ENDIF}
End;

Procedure AndroidApplication.OpenAppStore(AppID:TERRAString);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  {$IFDEF OUYA}
  Log(logDebug, 'App','App store not supported in OUYA!');
  Exit;
  {$ELSE}

  If Pos('http', AppID)<=0 Then
    AppID := 'market://details?id='+AppID;

  Log(logDebug, 'App', 'Opening URL:' +AppID);
  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(AppID);
  _Utils.CallStaticVoidMethod(Frame, 'openURL', Params);
  ReleaseObject(Params);
  Java_End(Frame);
  {$ENDIF}
End;

Procedure AndroidApplication.SendAnalytics(EventName, Values:TERRAString);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  If Not Assigned(_Utils) Then
    Exit;

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(EventName);
  Params.AddString(Values);
  _Utils.CallStaticVoidMethod(Frame, 'sendAnalytics', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Procedure AndroidApplication.GainFocus;
Begin
End;

Procedure AndroidApplication.LostFocus;
Begin
  SetSuspend(True);
End;

Procedure AndroidApplication.OnShutdown;
Begin
  //_Utils := Nil;
End;

Function AndroidApplication.IsAppRunning(Name:TERRAString): Boolean;
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Result := False;

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Checking if app is running:' +Name);

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Name);
  Result := _Utils.CallStaticBoolMethod(Frame, 'isAppRunning', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Function AndroidApplication.IsAppInstalled(Name:TERRAString): Boolean;
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Result := False;

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Checking if app is installed:' +Name);

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Name);
  Result := _Utils.CallStaticBoolMethod(Frame, 'isAppInstalled', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Function AndroidApplication.GetDeviceID:TERRAString;
Var
  Frame:JavaFrame;
Begin
  Result := '';

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Getting device ID');

  Java_Begin(Frame);
  Result := _Utils.CallStaticStringMethod(Frame, 'getDeviceID', Nil);
  Java_End(Frame);
End;

Function AndroidApplication.IsDebuggerPresent: Boolean;
Var
  Frame:JavaFrame;
Begin
  If (_TestedDebug) Then
  Begin
    Result := _DebuggerAttached;
    Exit;
  End;

  If Not Assigned(_Utils) Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'App', 'Checking if in debugging mode...');

  _TestedDebug := True;
  Java_Begin(Frame);
  _DebuggerAttached := _Utils.CallStaticBoolMethod(Frame, 'isDebuggerAttached', Nil);
  Java_End(Frame);
  Result := _DebuggerAttached;
End;

Function AndroidApplication.InitAccelerometer: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing accelerometer');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod(Frame, 'initAccelerometer', Nil);
  Java_End(Frame);
End;

Function AndroidApplication.InitGyroscope: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing gyroscope');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod(Frame, 'initGyroscope', Nil);
  Java_End(Frame);
End;

Function AndroidApplication.InitCompass: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing compass');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod(Frame, 'initCompass', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopAccelerometer;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping accelerometer');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'stopAccelerometer', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopGyroscope;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping gyroscope');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'stopGyroscope', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopCompass;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping compass');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'stopCompass', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.ListFile(Name:Pointer; Size: Integer);
Var
  FileName:TERRAString;
  P:FileInfo;
Begin
  FileName := JavaToString(Name);

  If IsFolderMode Then
    CurrentFileDir.Add(StringObject.Create(FileName))
  Else
  Begin
    P := FileInfo.Create();
    P.Name := FileName;
    P.Size := Size;
    P.Path := CurrentPath;
    P.Level := CurrentLevel;
    CurrentFileDir.Add(P);
  End;
End;

Procedure AndroidApplication.SendEmail(DestEmail, Subject, Body:TERRAString);
Var
  Frame:JavaFrame;
  Params:JavaArguments;
Begin
  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Sending email...');

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(DestEmail);
  Params.AddString(Subject);
  Params.AddString(Body);
  _Utils.CallStaticVoidMethod(Frame, 'sendEmail', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Function AndroidApplication.SaveToCloud: Boolean;
Var
  Frame:JavaFrame;
  Params:JavaArguments;
Begin
  {$IFDEF GAMECONSOLE}
  Exit;
  {$ENDIF}

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Saving to cloud...');

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  _Utils.CallStaticVoidMethod(Frame, 'saveToCloud', Params);
  ReleaseObject(Params);
  Java_End(Frame);

  Result := True;
End;

Procedure AndroidApplication.Tapjoy_ShowOfferWall;
Var
  Frame:JavaFrame;
  AppClass:JavaClass;
Begin
  {$IFDEF GAMECONSOLE}
  Exit;
  {$ENDIF}

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Showing offer wall');

  Java_Begin(Frame);
  AppClass := JavaClass.Create(ActivityClassPath, Frame);
  AppClass.CallStaticVoidMethod(Frame, 'showTapJoyOfferWall', Nil);
  ReleaseObject(AppClass);
  Java_End(Frame);
End;

Procedure AndroidApplication.Tapjoy_ShowVideo;
Var
  Frame:JavaFrame;
  AppClass:JavaClass;
Begin
  {$IFDEF GAMECONSOLE}
  Exit;
  {$ENDIF}

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Showing video');

  Java_Begin(Frame);
  AppClass := JavaClass.Create(ActivityClassPath, Frame);
  AppClass.CallStaticVoidMethod(Frame, 'showTapJoyVideo', Nil);
  ReleaseObject(AppClass);
  Java_End(Frame);
End;

Procedure AndroidApplication.Tapjoy_SpendCredits(Ammount: Integer);
Var
  Frame:JavaFrame;
  AppClass:JavaClass;
  Params:JavaArguments;
Begin
  {$IFDEF GAMECONSOLE}
  Exit;
  {$ENDIF}

  If Not Assigned(_Utils) Then
    Exit;

  Log(logDebug, 'App', 'Showing video');

  Java_Begin(Frame);
  AppClass := JavaClass.Create(ActivityClassPath, Frame);

  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Ammount);
  AppClass.CallStaticVoidMethod(Frame, 'spendTapJoyPoints', Params);
  ReleaseObject(Params);

  ReleaseObject(AppClass);
  Java_End(Frame);
End;


Function AndroidApplication.InitSettings: Boolean;
Var
  Params:JavaArguments;
  S:JavaObject;
  Frame:JavaFrame;
Begin
  Inherited InitSettings;

  _ApplicationInstance := Self;

  Log(logDebug, 'App', 'Starting Android App!');
  Java_Begin(Frame);

 // Java_ClearClassLoader();

  Log(logDebug, 'App', 'Getting terra utils class');
  _Utils := JavaClass.Create(UtilsClassPath, Frame);

{  Log(logDebug, 'App', 'Getting class loader');
  ClassLoader := _Utils.CallStaticObjectMethod(Frame, 'getClassLoader', 'java/lang/ClassLoader', Nil);

  Log(logDebug, 'App', 'Preparing class loader');
  Java_LoadClassLoader(Frame, ClassLoader);}

  Log(logDebug, 'App', 'Getting internal write path');
  _Path := _Utils.CallStaticStringMethod(Frame, 'getInternalDir', Nil);
  _StoragePath := _Path;
  Log(logDebug, 'App', 'Found internal app path: '+_Path);

  Log(logDebug, 'App', 'Getting external write path');
  _Path := _Utils.CallStaticStringMethod(Frame, 'getExternalDir', Nil);
  _TempPath := _Path;
  _DocumentPath := _Path;
  Log(logDebug, 'App', 'Found external app path: '+_Path);


  Log(logDebug, 'App', 'Getting user country');
  _Country := StringUpper(_Utils.CallStaticStringMethod(Frame, 'getCountry', Nil));
  Log(logDebug, 'App', 'Found country '+_Country);

  Log(logDebug, 'App', 'Getting local language');
  _Language := StringUpper(_Utils.CallStaticStringMethod(Frame, 'getLanguage', Nil));
  Log(logDebug, 'App', 'Found language: '+_Language);

  Log(logDebug, 'App', 'Getting screen dimensions');
  _Width := _Utils.CallStaticIntMethod(Frame, 'getScreenWidth', Nil);
  _Height := _Utils.CallStaticIntMethod(Frame, 'getScreenHeight', Nil);
  _Screen.Width := _Width;
  _Screen.Height := _Height;
  Log(logDebug, 'App', 'Dimensions: '+IntToString(_Width)+'x'+IntToString(_Height));

  Log(logDebug, 'App', 'Getting cpu cores');
  _CPUCores := _Utils.CallStaticIntMethod(Frame, 'getCPUCores', Nil);
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  Log(logDebug, 'App', 'Getting package version');
  _BundleVersion := _Utils.CallStaticStringMethod(Frame, 'getBundleVersion', Nil);
  Log(logDebug, 'App', 'Found version '+_BundleVersion);

  Java_CacheClass(Frame, FileIOClassPath);
  Java_End(Frame);

  Renderers.Add(OpenGLES2Renderer.Create());

	Result := True;
End;

(*Procedure AndroidApplication.SpawnThread(Args: Pointer);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Spawning new thread '+IntToString(Integer(Args)));
  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Integer(Args));
  _Utils.CallStaticVoidMethod(Frame, 'spawnThread', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;*)

Procedure AndroidApplication.OnFatalError(Const ErrorMsg, CrashLog, Callstack: TERRAString);
Var
  Frame:JavaFrame;
Begin
  Java_Begin(Frame);
  Java_Exception(Frame, ErrorMsg + CrLf + CrashLog + CrLf + Callstack);
  Java_End(Frame);
//  _Running := False;
End;

End.
