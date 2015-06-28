// iPhone port
Unit TERRA_OS;
{$I terra.inc}


Interface
Uses TERRA_String, TERRA_Utils, TERRA_Application, TERRA_Vector3D;

Const
	PathSeparator = '/';
	CrLf = #10;

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
keyPlus       = 107;
keyMinus      = 109;
keyPeriod     = 190;

keyLeft       = 28;
keyUp         = 30;
keyRight      = 29;
keyDown       = 31;

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



Type
  iOSApplication = Class(BaseApplication)
    Protected

      //Procedure ConvertCoords(Var X,Y:Integer);
      Procedure SetScale(Scale:Single);

      Function InitSettings:Boolean; Override;
      Function InitWindow:Boolean; Override;
      Procedure CloseWindow; Override;

  	Public
        Constructor Create();

      Procedure ProcessMessages; Override;
      Procedure SetState(State:Cardinal); Override;

      Procedure EnableAds; Override;
      Procedure ShowFullscreenAd; Override;

      Procedure OpenAppStore(AppID:TERRAString); Override;

      Function IsDeviceRooted: Boolean; Override;

      Function InitAccelerometer():Boolean; Override;
      Function InitGyroscope():Boolean; Override;
      Function InitCompass():Boolean; Override;

      Procedure StopAccelerometer(); Override;
      Procedure StopGyroscope(); Override;
      Procedure StopCompass(); Override;

      Procedure PostToFacebook(msg, link, desc, imageURL:TERRAString); Override;

      Function GetDeviceID():TERRAString; Override;

      Procedure SendAnalytics(EventName:TERRAString; Params:TERRAString=''); Override;


    Class Function Instance:iOSApplication;

    Class Function GetCurrentTime:TERRATime;
    Class Function GetCurrentDate:TERRADate;
    Class Function GetTime:Cardinal;

  End;

    Application = iOSApplication;

Function getUniqueDeviceID():PAnsiChar; Cdecl; external;
Procedure showAlert(message:PAnsiChar); cdecl; external;
Procedure vibrate(); cdecl; external;
Procedure setPrefString(key, data:PAnsiChar); cdecl; external;
Procedure getPrefString(key, dest:PAnsiChar); cdecl; external;
Procedure savePref(); cdecl; external;
Procedure showAds(); cdecl; external;
Procedure showFullscreenAds(); cdecl; external;
Procedure openAppStore(S:PAnsiChar); cdecl; external;
//Procedure initAppViews(); cdecl; external;

Function getCPUCores():Integer; cdecl; external;

Function InitAccelerometer():Boolean; Cdecl; external;
Function InitGyroscope():Boolean; Cdecl; external;
Function InitCompass():Boolean; Cdecl; external;

Function StopAccelerometer():Boolean; Cdecl; external;
Function StopGyroscope():Boolean; Cdecl; external;
Function StopCompass():Boolean; Cdecl; external;

(*
Procedure submitScore(score:Integer); cdecl; external;
Procedure showLeaderboard(); cdecl; external;
Procedure changeLeaderboard(board:PAnsiChar); cdecl; external;

Procedure AnalyticsLog(s:PAnsiChar); cdecl; external;
Procedure AnalyticsLogWithParams(s, s2:PAnsiChar); cdecl; external;

*)

Procedure iPhoneLog(s:PAnsiChar); cdecl; external;

//Procedure focusKeyboard(s:PAnsiChar); cdecl; external;
Function isDeviceJailbroken():Boolean; cdecl; external;

Procedure IAP_RequestProduct(ID:PAnsiChar) cdecl; external;
Function IAP_CanPurchase():Boolean; cdecl; external;
Procedure IAP_Purchase(s:PAnsiChar); cdecl; external;

Function isSimulator():Boolean; cdecl; external;

Function resolveHost(s:PAnsiChar):PAnsiChar; cdecl; external;

Function audioOpen (name:PAnsiChar):Pointer; cdecl; external;
Procedure audioPlay (player:Pointer); cdecl; external;
Procedure audioStop (player:Pointer); cdecl; external;
Procedure audioSetVolume(player:Pointer; volume:Single); cdecl; external;
Procedure audioClose (player:Pointer); cdecl; external;

//Procedure initScale(); cdecl; external;

//Procedure postToFacebook(msg, link, desc, image:PAnsiChar); cdecl; external;

//Function shadersAvailable():Boolean; cdecl; external;

Procedure AppUnlockAchievement(ID:PAnsiChar);cdecl; external;

//Procedure iCloudSynchronize; Cdecl; external;
//Procedure excludeFileFromCloud(fileName:PAnsiChar);Cdecl; external;

Function getCurrentOrientation():Integer; Cdecl; external;

Procedure getBundleVersion(dest:PAnsiChar); Cdecl; external;

Procedure ApplicationSendInput(s:TERRAChar); cdecl; export;
Procedure ApplicationSetScreenRegion(width, height:Integer); cdecl; export;
Procedure ApplicationSetOrigin(X,Y:Integer); cdecl; export;
Procedure ApplicationSetResourcesPath(Path:PAnsiChar); cdecl; export;
Procedure ApplicationTempPath(Path:PAnsiChar); cdecl; export;
Procedure ApplicationDocumentPath(Path:PAnsiChar); cdecl; export;
Procedure ApplicationSetLanguage(Lang:PAnsiChar); cdecl; export;
Procedure ApplicationUpdate; cdecl; export;
Procedure ApplicationShutdown; cdecl; export;
Procedure ApplicationSetScale(Value:Single); cdecl; export;
Function ApplicationIsHiRes():Boolean; cdecl; export;
Procedure ApplicationBeginTouch(x,y:Integer); cdecl; export;
Procedure ApplicationMoveTouch(x,y:Integer); cdecl; export;
Procedure ApplicationEndTouch(x,y:Integer); cdecl; export;
Procedure ApplicationOnKeyDown(key:Integer); cdecl; export;
Procedure ApplicationOnKeyUp(key:Integer); cdecl; export;
Procedure ApplicationEnterState(state:Integer); cdecl; export;
Procedure ApplicationOnAccelerometer(x, y, z:Single); cdecl; export;
Procedure ApplicationOnGyroscope(x, y, z:Single); cdecl; export;
Procedure ApplicationResize(Width,Height:Integer); cdecl; export;
Procedure ApplicationMemoryWarning(); cdecl; export;

Function ApplicationGetAppID():PAnsiChar; cdecl; export;
Function ApplicationGetTestFlightID():PAnsiChar; cdecl; export;
Function ApplicationGetFlurryID():PAnsiChar; cdecl; export;
Function ApplicationGetAdMobBannerID():PAnsiChar; cdecl; export;
Function ApplicationGetAdMobInterstitialID():PAnsiChar; cdecl; export;
Function ApplicationGetChartboostID():PAnsiChar; cdecl; export;
Function ApplicationGetChartboostSecret():PAnsiChar; cdecl; export;
Function ApplicationGetFacebookID():PAnsiChar; cdecl; export;
Function ApplicationGetAdBuddizID():PAnsiChar; cdecl; export;
Function ApplicationGetVungleID():PAnsiChar; cdecl; export;

Procedure ApplicationOnFacebookPost(); cdecl; export;
Procedure ApplicationOnFacebookError(); cdecl; export;


Var
  iOSScale:Single = 1.0;

Implementation
Uses TERRA_Log, TERRA_GraphicsManager, TERRA_ResourceManager, TERRA_Texture,
    TERRA_InputManager, TERRA_SoundManager, TERRA_Renderer, TERRA_GLES2Renderer,
    dateutils, sysutils{$IFDEF NEON_FPU},TERRA_NEON{$ENDIF};

Var
  _ScreenWidth:Integer;
  _ScreenHeight:Integer;
  _OriginX:Integer=0;
  _OriginY:Integer=0;
  _iOSTempPath:TERRAString;
  _iOSDocumentPath:TERRAString;
  _iOSLanguage:TERRAString;
  _iOSCountry:TERRAString;

Var
    _Application_Instance:iOSApplication;

(*
Procedure ApplicationSetViewport(x1, y1, x2, y2:Integer); cdecl; export;
Begin
  If (_Application_Instance=Nil) Then
    Exit;

	Log(logDebug, 'App', 'Viewport request x1:' + IntToString(x1)+' y1:' + IntToString(y1)+' x2:' + IntToString(x2)+' y2:' + IntToString(y2));

  //_Application_Instance.AddRectEvent(eventViewport, Trunc(x1*iOSScale), Trunc(y1*iOSScale), Trunc(x2*iOSScale), Trunc(y2*iOSScale));
End;
*)

Procedure ApplicationSetOrientation(orientation:Integer); cdecl; export;
Var
  I:Integer;
Begin
  If (_Application_Instance=Nil) Then
    Exit;

  _Application_Instance.AddValueEvent(eventOrientation, Orientation);
End;

Procedure ApplicationSendInput(s:TERRAChar); cdecl; export;
Var
  I:Integer;
Begin
  If (_Application_Instance=Nil) Then
    Exit;

  _Application_Instance.OnKeyPress(Ord(S));
End;

Procedure ApplicationSetScreenRegion(width, height:Integer); cdecl; export;
Begin
  _ScreenWidth := Width;
  _ScreenHeight := Height;
End;

Procedure ApplicationSetOrigin(X,Y:Integer); cdecl; export;
Begin
  _OriginX := X;
  _OriginY := Y;
End;

Procedure ApplicationSetResourcesPath(Path:PAnsiChar); cdecl; export;
Begin
  Log(logDebug, 'App', 'Resource Folder: '+ Path);
  ChDir(Path);
End;

Procedure ApplicationTempPath(Path:PAnsiChar); cdecl; export;
Begin
  _iOSTempPath := Path;
  Log(logDebug, 'App', 'Temp Folder: '+ _iOSTempPath);
End;

Procedure ApplicationDocumentPath(Path:PAnsiChar); cdecl; export;
Begin
  _iOSDocumentPath := Path;
  Log(logDebug, 'App', 'Document Folder: '+ _iOSDocumentPath);
End;

Procedure ApplicationSetLanguage(Lang:PAnsiChar); cdecl; export;
Begin
  _iOSLanguage := StringUpper(Lang);
  If (_iOSLanguage='') Then
    _iOSLanguage := 'EN'
  Else
    SetLength(_iOSLanguage, 2);
// special cases, IETF BCP 47
End;

Procedure ApplicationSetCountry(Country:PAnsiChar); cdecl; export;
Begin
  _iOSCountry := StringUpper(Country);
  If (_iOSCountry='') Then
    _iOSCountry := 'US'
  Else
    SetLength(_iOSCountry, 2);
End;

Class Function iOSApplication.GetCurrentTime:TERRATime;
Var
 Datetime:Tdatetime;
Begin
 datetime := Now();
 Result.Hour     := hourof( datetime );
 Result.minute   := minuteof( datetime );
 Result.second   := secondof( datetime );
 Result.MiliSecond  := millisecondof( datetime );
End;

Class Function iOSApplication.GetCurrentDate:TERRADate;
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
	machtimebaseinfo = Record
		numer:Cardinal;
		denom:Cardinal;
	End;

Var
	timeinfo:machtimebaseinfo;
	basetime:Int64;

Function mach_timebase_info(Var info:machtimebaseinfo):Integer; Cdecl; External;
Function mach_absolute_time:Int64; Cdecl; External;

Class Function Application.GetTime:Cardinal; {$IFDEF FPC} Inline;{$ENDIF}
Var
	t:Int64;
	f:Single;
Begin
	t := mach_absolute_time() - basetime;
	f := t / timeinfo.denom;
	f := f * timeinfo.numer;
	Result := Trunc(f / 1000000);
End;


Constructor iOSApplication.Create();
Begin
    _Application_Instance := Self;
    Inherited Create();
End;

Class Function iOSApplication.Instance:iOSApplication;
Begin
    Result := _Application_Instance;
End;

Function iOSApplication.InitWindow:Boolean;
Var
  buffer: Array[0..255] Of TERRAChar;
  I:Integer;

Begin
//  initAppViews();

  Log(logDebug,'App', 'Creating window');

  _Width := Trunc(_ScreenWidth * iOSScale);
  _Height := Trunc(_ScreenHeight * iOSScale);
	Log(logDebug, 'App', 'Resolution: ' + IntToString(_Width)+' x' +IntToString(_Height));

  _Screen.Width := _Width;
  _Screen.Height := _Height;

  Log(logDebug,'App', 'OK!');
  _Application_Instance := Self;

    _OrientationTime := 0;
    _Orientation := -1;

//    initScale();

    Result := True;
End;

Procedure iOSApplication.CloseWindow;
Begin
	Log(logDebug,'App', 'Destroying window');
	Log(logDebug,'App', 'Ok');
End;

(*Procedure iOSApplication.SwapBuffers;
Begin
    If (IsInvalidOrientation(_Orientation)) Then
    Begin
        _OrientationTime := 0;
        //SetOrientation(getCurrentOrientation());
        SetOrientation(orientationLandscapeLeft);
    End;
End;*)

Procedure iOSApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure iOSApplication.ProcessMessages;
Begin
End;

Procedure iOSApplication.PostToFacebook(msg, link, desc, imageURL:TERRAString);
Begin
  //TERRA_OS.PostToFacebook(PAnsiChar(Msg), PAnsiChar(Link), PAnsiChar(Desc), PAnsiChar(ImageURL));
End;

{Procedure iOSApplication.ConvertCoords(Var X,Y:Integer);
Begin
  If (_Width<_ScreenWidth) Then
    X := Trunc(X*(_Width/_ScreenWidth)) - _OriginX;

  If (_Height<_ScreenHeight) Then
    Y := Trunc(Y*(_Height/_ScreenHeight)) - _OriginY;
End;}

Procedure iOSApplication.SetScale(Scale:Single);
Begin
  _Width := Trunc(_ScreenWidth * iOSScale);
  _Height := Trunc(_ScreenHeight * iOSScale);
End;

Procedure ApplicationUpdate; cdecl; export;
Begin
    Application.Instance.Run;
End;

Procedure ApplicationShutdown; cdecl; export;
Begin
    Application.Instance.Terminate;
End;

Procedure ApplicationSetScale(Value:Single); cdecl; export;
Begin
  iOSScale := Value;
  iOSApplication(_Application_Instance).SetScale(Value);
End;

Function ApplicationIsHiRes():Boolean; cdecl; export;
Begin
    Result := True;
End;

Procedure ApplicationBeginTouch(x,y:Integer); cdecl; export;
Begin
    If (Not Assigned(_Application_Instance)) Then
        Exit;

//	Log(logDebug, 'App', 'ScreenWidth:' + IntToString(_ScreenWidth)+' ScreenHeight:' + IntToString(_ScreenHeight));
    Log(logDebug, 'game','got touch X:'+IntToString(X)+', Y:'+InTToString(Y));

    X := Trunc(X * iOSScale);
    Y := Trunc(Y * iOSScale);

    Log(logDebug, 'game','converted touch X:'+IntToString(X)+', Y:'+InTToString(Y));

    _Application_Instance.AddCoordEvent(eventMouseDown, X, Y, keyMouseLeft);
    Log(logDebug, 'game','final touch X:'+IntToString(X)+', Y:'+InTToString(Y));
End;

Procedure ApplicationMoveTouch(x,y:Integer); cdecl; export;
Begin
    If (Not Assigned(_Application_Instance)) Then
        Exit;

//	Log(logDebug, 'game','untouch move X:'+IntToString(X)+', Y:'+InTToString(Y));
//    iOSApplication(_Application_Instance).ConvertCoords(X,Y);
//	Log(logDebug, 'game','convert move X:'+IntToString(X)+', Y:'+InTToString(Y));

    X := Trunc(X * iOSScale);
    Y := Trunc(Y * iOSScale);

    _Application_Instance.AddCoordEvent(eventMouseMove, X, Y, 0);
//	Log(logDebug, 'game','touch move X:'+IntToString(X)+', Y:'+InTToString(Y));
End;

Procedure ApplicationEndTouch(x,y:Integer); cdecl; export;
Begin
    If (Not Assigned(_Application_Instance))  Then
        Exit;

    X := Trunc(X * iOSScale);
    Y := Trunc(Y * iOSScale);

//    iOSApplication(_Application_Instance).ConvertCoords(X,Y);
    _Application_Instance.AddCoordEvent(eventMouseUp, X, Y, keyMouseLeft);
//	Log(logDebug, 'game','touch up X:'+IntToString(X)+', Y:'+InTToString(Y));
End;

Procedure ApplicationOnKeyDown(Key:Integer); cdecl; export;
Begin
  _Application_Instance.AddValueEvent(eventKeyDown, Key);
End;

Procedure ApplicationOnKeyUp(Key:Integer); cdecl; export;
Begin
  _Application_Instance.AddValueEvent(eventKeyUp, Key);
End;

Procedure ApplicationOnContextLost(); cdecl; export;
Begin
  _Application_Instance.AddValueEvent(eventContextLost, 0);
End;

Procedure ApplicationEnterState(state:Integer); cdecl; export;
Begin
    If (Not Assigned(_Application_Instance))  Then
        Exit;

    _Application_Instance.OnStateChange(state);
End;

Procedure ApplicationOnAccelerometer(x, y, z:Single); cdecl; export;
Begin
    If Not Assigned(_Application_Instance) Then
        Exit;

  Application.Instance.AddVectorEvent(eventAccelerometer, x,y ,z);
//Log(logDebug, 'game','accel X:'+FloatToString(X)+', Y:'+FloatToString(Y)+', Z:'+FloatToString(Z));
End;

Procedure ApplicationOnGyroscope(x, y, z:Single); cdecl; export;
Begin
    If Not Assigned(_Application_Instance) Then
        Exit;

  Application.Instance.AddVectorEvent(eventGyroscope, x,y ,z);
//Log(logDebug, 'game','accel X:'+FloatToString(X)+', Y:'+FloatToString(Y)+', Z:'+FloatToString(Z));
End;

Procedure ApplicationOnCompass(Heading, Pitch, Roll: Single); cdecl; export;
Begin
    If Not Assigned(_Application_Instance) Then
        Exit;

  Application.Instance.AddVectorEvent(eventCompass, Heading, Pitch, Roll);
//Log(logDebug, 'game','accel X:'+FloatToString(X)+', Y:'+FloatToString(Y)+', Z:'+FloatToString(Z));
End;

Procedure ApplicationResize(Width,Height:Integer); cdecl; export;
Var
    App:iOSApplication;
Begin
  If Not Assigned(_Application_Instance) Then
    Exit;

  Log(logDebug, 'game','resize X:'+IntToString(Width)+', Y:'+IntToString(Height));
  _Application_Instance.AddCoordEvent(eventWindowResize, Width, Height, 0);
  Log(logDebug, 'game','resize X:'+IntToString(_Application_Instance.Width)+', Y:'+IntToString(_Application_Instance.Height));
End;

Procedure ApplicationOnFacebookPost(); cdecl; export;
Begin
  If (Not Assigned(_Application_Instance)) Then
    Exit;

  _Application_Instance.OnAPIResult(apiFacebook, facebookPostSucess);
End;

Procedure ApplicationOnFacebookError(); cdecl; export;
Begin
  If (Not Assigned(_Application_Instance)) Then
    Exit;

  _Application_Instance.OnAPIResult(apiFacebook, facebookConnectionError);
End;

Procedure ApplicationMemoryWarning(); cdecl; export;
Begin
  Log(logDebug, 'game','Received memory warning, purging resources...');
  TextureManager.Instance.PurgeResources();
  SoundManager.Instance.PurgeResources();
End;

Procedure iOSApplication.EnableAds;
Begin
  //Self.SetViewport(0, 50, Application.Instance.Width, Application.Instance.Height);

  Log(logDebug, 'game','Trying to enable ads...');
  TERRA_OS.showAds();
End;

Procedure iOSApplication.ShowFullscreenAd;
Begin
  Log(logDebug, 'game','Showing fullscreen ads...');
  TERRA_OS.showFullscreenAds();
End;

Function iOSApplication.GetDeviceID:TERRAString;
Begin
  Result := TERRA_OS.getUniqueDeviceID();
End;

Procedure iOSApplication.OpenAppStore(AppID:TERRAString);
Begin
  TERRA_OS.openAppStore(PAnsiChar(AppID));
End;

Procedure iOSApplication.SendAnalytics(EventName, Params:TERRAString);
Begin
(*  If (EventName='') Then
    Exit;

  If (Params<>'') Then
    AnalyticsLogWithParams(PAnsiChar(EventName), PAnsiChar(Params))
  Else
    AnalyticsLog(PAnsiChar(EventName));
*)
End;

Var
  Buffer:Array[0..255] Of Char;

Function GetStringBuffer(S:String):PAnsiChar;
Var
    I:Integer;
Begin
    S := S + #0;
    For I:=1 To Length(S) Do
        Buffer[I] := S[I];
    Result := @Buffer[1];
End;

Function ApplicationGetAppID():PAnsiChar; cdecl; export;
Begin
    Result := GetStringBuffer(Application.Instance.GetAppID());
End;

Function ApplicationGetTestFlightID():PAnsiChar; cdecl; export;
Begin
    Result := GetStringBuffer(Application.Instance.GetTestFlightID());
End;

Function ApplicationGetFlurryID():PAnsiChar; cdecl; export;
Begin
  Result := GetStringBuffer(Application.Instance.GetFlurryID());
End;

Function ApplicationGetAdMobBannerID():PAnsiChar; cdecl; export;
Begin
  Result := GetStringBuffer(Application.Instance.GetadMobBannerID());
End;

Function ApplicationGetAdMobInterstitialID():PAnsiChar; cdecl; export;
Begin
  Result := GetStringBuffer(Application.Instance.GetAdMobInterstitialID());
End;

Function ApplicationGetChartboostID():PAnsiChar; cdecl; export;
Begin
    Result := GetStringBuffer(Application.Instance.GetChartboostID());
End;

Function ApplicationGetChartboostSecret():PAnsiChar; cdecl; export;
Begin
    Result := GetStringBuffer(Application.Instance.GetChartboostSecret());
End;

Function ApplicationGetAdBuddizID():PAnsiChar; cdecl; export;
Begin
  Result := GetStringBuffer(Application.Instance.GetAdBuddizID());
End;

Function ApplicationGetVungleID():PAnsiChar; cdecl; export;
Begin
    Result := GetStringBuffer(Application.Instance.GetVungleID());
End;

Function ApplicationGetFacebookID():PAnsiChar; cdecl; export;
Begin
  Result := GetStringBuffer(Application.Instance.GetFacebookID());
End;

Var
  TestedJailbroke:Boolean;
  JailbrokeResult:Boolean;

Function iOSApplication.IsDeviceRooted: Boolean;
Begin
  If Not TestedJailbroke Then
  Begin
    JailbrokeResult := isDeviceJailbroken();
    TestedJailbroke := True;
  End;

  Result := JailbrokeResult;
End;

Function iOSApplication.InitAccelerometer: Boolean;
Begin
  Log(logDebug, 'App', 'Enabling accelerometer');
  Result := InitAccelerometer();
End;

Function iOSApplication.InitCompass: Boolean;
Begin
  Log(logDebug, 'App', 'Enabling compass');
  Result := InitCompass();
End;

Function iOSApplication.InitGyroscope: Boolean;
Begin
  Log(logDebug, 'App', 'Enabling gyroscope');
  Result := InitGyroscope();
End;

Procedure iOSApplication.StopAccelerometer;
Begin
  Log(logDebug, 'App', 'Disabling accelerometer');
    StopAccelerometer();
End;

Procedure iOSApplication.StopCompass;
Begin
  Log(logDebug, 'App', 'Disabling compass');
  StopCompass();
End;

Procedure iOSApplication.StopGyroscope;
Begin
  Log(logDebug, 'App', 'Disabling gyroscope');
  StopGyroscope();
End;

Function iOSApplication.InitSettings: Boolean;
Var
  Buffer:Array[0..1024] Of AnsiChar;
  Temp:PAnsiChar;
Begin
  Inherited InitSettings;

  Log(logDebug,'App', 'Setuping app time');
  mach_timebase_info(timeinfo);
  basetime := mach_absolute_time();

  Log(logDebug, 'App', 'Temp Folder: '+ _iOSTempPath);
  _TempPath := _iOSTempPath;

  Log(logDebug, 'App', 'Document Folder: '+ _iOSDocumentPath);
  _DocumentPath := _iOSDocumentPath;
  _StoragePath := _DocumentPath;

  _Country := _iOSCountry;
	Log(logDebug, 'App', 'Country = ' + _Country);

  _Language := _iOSLanguage;
	Log(logDebug, 'App', 'Lang = ' + _Language);

  Log(logDebug, 'App', 'Getting cpu cores');
  _CPUCores := getCPUCores();
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  {$IFDEF NEON_FPU}
  Log(logDebug, 'App', 'Enabling fast fpu mode');
  enable_runfast();
  {$ENDIF}

  Log(logDebug, 'App', 'Getting app version');
  Temp := @Buffer[0];
  getBundleVersion(temp);
  _BundleVersion := Temp;
  Log(logDebug, 'App', 'Found version '+_BundleVersion);

  Renderers.Add(OpenGLES2Renderer.Create());

  Result := True;
End;

End.
