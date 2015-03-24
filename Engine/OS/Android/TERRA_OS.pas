// Android port
Unit TERRA_OS;
{$I terra.inc}


Interface
Uses cmem, TERRA_String, TERRA_Error, TERRA_Utils, TERRA_Application, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF},
    TERRA_Vector3D, TERRA_Java, TERRA_Collections, TERRA_Client, TERRA_CollectionObjects,
    sysutils,dateutils,unix, jni;

Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 8;
	keyTab        = 9;
	keyEnter      = 13;
	keyShift      = 1;
	keyControl    = 2;
	keyAlt        = 3;
	keyPause      = 82;
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

	keyInsert     = $72;
	keyDelete     = $75;

  keyF1        = $7A;
  keyF2        = $78;
  keyF3        = $63;
  keyF4        = $76;
  keyF5        = $60;
  keyF6        = $61;
  keyF7        = $62;
  keyF8        = $64;
  keyF9        = $65;
  keyF10       = $6D;
  keyF11       = $67;
  keyF12       = $6F;

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

Procedure DisplayMessage(S:TERRAString);
Function GetCurrentTime:TERRATime;
Function GetCurrentDate:TERRADate;
Function GetTime:Cardinal; {$IFDEF FPC} Inline;{$ENDIF}
Function CreateApplicationClass(Client:AppClient):Application;

Type
  AndroidApplication = Class(Application)
    Protected
      _Focus :Boolean;

      _Utils:JavaClass;

      _TestedDebug:Boolean;
      _DebuggerAttached:Boolean;

      Function InitSettings:Boolean; Override;
      Function InitWindow:Boolean; Override;
      Function InitGraphics:Boolean; Override;
      Procedure CloseGraphics; Override;
      Procedure CloseWindow; Override;

      Procedure GainFocus;
      Procedure LostFocus;

      Procedure OnShutdown; Override;

	Public
      Procedure SwapBuffers; Override;
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

      Procedure SpawnThread(Args:Pointer);      
  End;

Procedure focusKeyboard(s:PAnsiChar);
Function StartupWithVM(VM:Pointer):Integer;

Procedure ApplicationThreadExecute(P:Integer);

Implementation
Uses TERRA_Log, TERRA_ResourceManager, TERRA_Shader, TERRA_Texture, TERRA_Mesh, TERRA_GraphicsManager,
  TERRA_UI, TERRA_IAP, TERRA_FileSearch, TERRA_Facebook, TERRA_Threads, TERRA_FileStream;

Var
	_ApplicationInstance:AndroidApplication = Nil;


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
Begin
  If (_ApplicationInstance=Nil) Or (_ApplicationInstance._Utils=Nil) Then
    Exit;

  Params := JavaArguments.Create(Nil);
  Params.AddString(S);
  _ApplicationInstance._Utils.CallStaticVoidMethod('showKeyboard', Params);
  Params.Release();
End;

Procedure DisplayMessage(S:TERRAString);
Begin
	S := S +#0;
//  Log(logWarning, 'App', S);
End;

Function GetCurrentTime:TERRATime;
Var
 Datetime:Tdatetime;
Begin
 datetime := Now();
 Result.Hour     := hourof( datetime );
 Result.minute   := minuteof( datetime );
 Result.second   := secondof( datetime );
 Result.MiliSecond  := millisecondof( datetime );
End;

Function GetCurrentDate:TERRADate;
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

Function GetTime:Cardinal;{$IFDEF FPC} Inline;{$ENDIF}
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


Function CreateApplicationClass(Client:AppClient):Application;
Begin
	Result := AndroidApplication.Create(Client);
End;

Function AndroidApplication.InitWindow:Boolean;
Begin
  Result := True;
End;

Procedure AndroidApplication.PostToFacebook(msg, link, desc, imageURL:TERRAString);
Var
  FB:Facebook;
Begin
  If (Application.Instance.Client.GetFacebookID()='') Then
  Begin
    If Assigned(Client) Then
      Client.OnAPIResult(apiFacebook, facebookConnectionError);
    Exit;
  End;

  FB := Facebook.Create();
  FB.Post(Msg, Link, Desc, ImageURL);
  FB.Release();
End;


Procedure AndroidApplication.LikeFacebookPage(page, url:TERRAString);
Var
  FB:Facebook;
Begin
  If (Application.Instance.Client.GetFacebookID()='') Then
  Begin
    If Assigned(Client) Then
      Client.OnAPIResult(apiFacebook, facebookConnectionError);
    Exit;
  End;

  FB := Facebook.Create();
  FB.LikePage(Page, url);
  FB.Release();
End;

Function AndroidApplication.InitGraphics:Boolean;
Begin
	Result := True;
End;

Procedure AndroidApplication.CloseGraphics;
Begin
End;

Procedure AndroidApplication.CloseWindow;
Begin
End;

Procedure AndroidApplication.SwapBuffers;
Begin
  // do nothing
End;

Procedure AndroidApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure AndroidApplication.ShowFullscreenAd;
Begin
  {$IFDEF OUYA}
  Log(logDebug, 'App','Ads not supported in OUYA!');
  Exit;
  {$ELSE}

  Log(logDebug, 'App', 'Showing fullscreen ad..');
  _Utils.CallStaticVoidMethod('showFullscreenAds', Nil);
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
  Params.AddString(Application.Instance.Client.GetAdMobBannerID());
  _Utils.CallStaticVoidMethod('enableAds', Params);
  Params.Release();
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
  _Utils.CallStaticVoidMethod('disableAds', Nil);
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
  _Utils.CallStaticVoidMethod('openURL', Params);
  Params.Release();
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
  _Utils.CallStaticVoidMethod('sendAnalytics', Params);
  Params.Release();
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
  Result := _Utils.CallStaticBoolMethod('isAppRunning', Params);
  Params.Release();
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
  Result := _Utils.CallStaticBoolMethod('isAppInstalled', Params);
  Params.Release();
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
  Result := _Utils.CallStaticStringMethod('getDeviceID', Nil);
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
  _DebuggerAttached := _Utils.CallStaticBoolMethod('isDebuggerAttached', Nil);
  Java_End(Frame);
  Result := _DebuggerAttached;
End;

Function AndroidApplication.InitAccelerometer: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing accelerometer');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod('initAccelerometer', Nil);
  Java_End(Frame);
End;

Function AndroidApplication.InitGyroscope: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing gyroscope');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod('initGyroscope', Nil);
  Java_End(Frame);
End;

Function AndroidApplication.InitCompass: Boolean;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Initializing compass');

  Java_Begin(Frame);
  Result := _Utils.CallStaticBoolMethod('initCompass', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopAccelerometer;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping accelerometer');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod('stopAccelerometer', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopGyroscope;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping gyroscope');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod('stopGyroscope', Nil);
  Java_End(Frame);
End;

Procedure AndroidApplication.StopCompass;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Stopping compass');

  Java_Begin(Frame);
  _Utils.CallStaticVoidMethod('stopCompass', Nil);
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
  _Utils.CallStaticVoidMethod('sendEmail', Params);
  Params.Release();
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
  _Utils.CallStaticVoidMethod('saveToCloud', Params);
  Params.Release();
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
  AppClass.CallStaticVoidMethod('showTapJoyOfferWall', Nil);
  AppClass.Release();
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
  AppClass.CallStaticVoidMethod('showTapJoyVideo', Nil);
  AppClass.Release();
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
  AppClass.CallStaticVoidMethod('spendTapJoyPoints', Params);
  Params.Release();

  AppClass.Release();
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
  ClassLoader := _Utils.CallStaticObjectMethod('getClassLoader', 'java/lang/ClassLoader', Nil);

  Log(logDebug, 'App', 'Preparing class loader');
  Java_LoadClassLoader(Frame, ClassLoader);}

  Log(logDebug, 'App', 'Getting internal write path');
  _Path := _Utils.CallStaticStringMethod('getInternalDir', Nil);
  _StoragePath := _Path;
  Log(logDebug, 'App', 'Found internal app path: '+_Path);

  Log(logDebug, 'App', 'Getting external write path');
  _Path := _Utils.CallStaticStringMethod('getExternalDir', Nil);
  _TempPath := _Path;
  _DocumentPath := _Path;
  Log(logDebug, 'App', 'Found external app path: '+_Path);


  Log(logDebug, 'App', 'Getting user country');
  _Country := StringUpper(_Utils.CallStaticStringMethod('getCountry', Nil));
  Log(logDebug, 'App', 'Found country '+_Country);

  Log(logDebug, 'App', 'Getting local language');
  _Language := StringUpper(_Utils.CallStaticStringMethod('getLanguage', Nil));
  Log(logDebug, 'App', 'Found language: '+_Language);

  Log(logDebug, 'App', 'Getting screen dimensions');
  _Width := _Utils.CallStaticIntMethod('getScreenWidth', Nil);
  _Height := _Utils.CallStaticIntMethod('getScreenHeight', Nil);
  _Screen.Width := _Width;
  _Screen.Height := _Height;
  Log(logDebug, 'App', 'Dimensions: '+IntToString(_Width)+'x'+IntToString(_Height));

  Log(logDebug, 'App', 'Getting cpu cores');
  _CPUCores := _Utils.CallStaticIntMethod('getCPUCores', Nil);
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  Log(logDebug, 'App', 'Getting package version');
  _BundleVersion := _Utils.CallStaticStringMethod('getBundleVersion', Nil);
  Log(logDebug, 'App', 'Found version '+_BundleVersion);

  Java_CacheClass(Frame, FileIOClassPath);

  Java_End(Frame);

	Result := True;
End;

Procedure AndroidApplication.SpawnThread(Args: Pointer);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Spawning new thread '+IntToString(Integer(Args)));
  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Integer(Args));
  _Utils.CallStaticVoidMethod('spawnThread', Params);
  Params.Release();
  Java_End(Frame);
End;

End.
