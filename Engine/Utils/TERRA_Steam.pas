Unit TERRA_Steam;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Application, SteamAPI, SteamCallback;

Type
  SteamAchievement = Class
    Protected
      _ID:TERRAString;
      _Name:TERRAString;
      _Description:TERRAString;
      _Achieved:Boolean;

      Procedure Load();

    Public
      Procedure Unlock();

      Property ID:TERRAString Read _ID;
      Property Name:TERRAString Read _Name;
      Property Description:TERRAString Read _Description;
      Property Achieved:Boolean Read _Achieved;
  End;

  SteamManager = Class(ApplicationComponent)
    Protected
      _Loaded:Boolean;
      _Running:Boolean;
      _LoggedOn:Boolean;

      _StatsRequested:Boolean;
      _StoreStats:Boolean;

      _HasController:Boolean;

      _SteamID:TERRAString;
      _AppID:TERRAString;

      _UserName:TERRAString;
      _Language:TERRAString;

      _LicenseResult:SteamUserHasLicenseForAppResult;


      _Achievements:Array Of SteamAchievement;
      _AchievementCount:Integer;

      Procedure OnUserStats(P:Pointer);

    Public
      Class Function Instance:SteamManager;

      Procedure Update; Override;
      Procedure Init; Override;

      Function UnlockAchievement(AchID:TERRAString):Boolean;

      Procedure Release; Override;

      Property UserName:TERRAString Read _UserName;
      Property Language:TERRAString Read _Language;
      Property SteamID:TERRAString Read _SteamID;
      Property AppID:TERRAString Read _AppID;

      Property Loaded:Boolean Read _Loaded;
      Property Enabled:Boolean Read _Running;
  End;


Implementation
Uses TERRA_OS, TERRA_Log, TERRA_GraphicsManager, TERRA_FileManager;

Var
  _Steam_Instance:ApplicationObject = Nil;


{ Steam }
Class Function SteamManager.Instance:SteamManager;
Begin
  If _Steam_Instance = Nil Then
    _Steam_Instance := InitializeApplicationComponent(SteamManager, Nil);

  Result := SteamManager(_Steam_Instance.Instance);
End;

Procedure SteamManager.Init;
Var
   ControllerPath:TERRAString;
Begin
  _LoggedOn := False;

  Log(logDebug, 'Steam', 'Trying to load Steam library...');
  _Loaded := LoadSteamAPI();
  If Not _Loaded Then
  Begin
    Log(logWarning, 'Steam', 'Failed to hook into Steam...');
    _Running := False;
    Exit;
  End;

  _Running := SteamAPI_InitSafe();

  If Not _Running Then
    Exit;

  _AppID := CardinalToString(ISteamUtils_GetAppID());
  Log(logDebug, 'Steam', 'App ID: '+ _AppID);

  _SteamID := UInt64ToString(ISteamUser_GetSteamID());
  Log(logDebug, 'Steam', 'User ID: '+ _SteamID);

  _UserName := ISteamFriends_GetPersonaName();
  Log(logDebug, 'Steam', 'Username: '+ _UserName);

  _Language := ISteamApps_GetCurrentGameLanguage();
  If _Language = '' Then
    _Language := Application.Instance.Language;
  Log(logDebug, 'Steam', 'Language: '+ _Language);


  ControllerPath := FileManager.Instance.SearchResourceFile('controller.vdf');
  _HasController := (ControllerPath<>'');
  If (_HasController) Then
  Begin
       _HasController := ISteamController_Init(PAnsiChar(ControllerPath));
  End;

  //_LicenseResult := ISteamGameServer_UserHasLicenseForApp(steamID:SteamID; appID:SteamAppId):
End;

Procedure SteamManager.Release;
Var
  I:Integer;
Begin
  If _Running Then
  Begin
    If (_HasController) Then
    Begin
         ISteamController_Shutdown();
    End;

    _Running := False;
    SteamAPI_Shutdown();
  End;

  _Steam_Instance := Nil;
End;

Procedure SteamManager.Update;
Var
   I:Integer;
   controllerState:SteamControllerState;
Begin
  If Not _Running Then
    Exit;

  // Is the user logged on?  If not we can't get stats.
  If Not _StatsRequested Then
  Begin
    _LoggedOn := ISteamUser_BLoggedOn();

    SteamCallbackDispatcher.Create(k_iSteamUserStatsCallbacks  + 1 , Self.OnUserStats, SizeOf(Steam_UserStatsReceived));

    If (_LoggedOn) Then
    Begin
      _StatsRequested := ISteamUserStats_RequestCurrentStats();
    End;
  End;

  SteamAPI_RunCallbacks();


(*  For I:=0 To 3 Do
  Begin
       If ISteamController_GetControllerState(I, controllerState) Then
       Begin

       End;
  End;*)
End;

Function SteamManager.UnlockAchievement(AchID: TERRAString): Boolean;
Begin
  Result := False;

  If (Not _Running) Or (Not _LoggedOn) Then
    Exit;

  ISteamUserStats_SetAchievement(PAnsiChar(AchID));
  Result := ISteamUserStats_StoreStats();
End;

Procedure SteamManager.OnUserStats(P: Pointer);
Var
  Info:PSteam_UserStatsReceived;
  GameID:TERRAString;
  I:Integer;
Begin
  Info := P;
  GameID := UInt64ToString(Info.GameID);

  If GameID<> Self.AppID Then
    Exit;

  Log(logDebug, 'Steam', 'Received stats, with return code '+CardinalToString(Info.Result));

  // load achievements

  For I:=0 To Pred(_AchievementCount) Do
  Begin
    _Achievements[I].Load();
  End;

	// load stats
(*	SteamUserStats.GetStat("NumGames", out m_nTotalGamesPlayed);
	SteamUserStats.GetStat("NumWins", out m_nTotalNumWins);
				SteamUserStats.GetStat("NumLosses", out m_nTotalNumLosses);
				SteamUserStats.GetStat("FeetTraveled", out m_flTotalFeetTraveled);
				SteamUserStats.GetStat("MaxFeetTraveled", out m_flMaxFeetTraveled);
				SteamUserStats.GetStat("AverageSpeed", out m_flAverageSpeed);*)
End;

{ SteamAchievement }
Procedure SteamAchievement.Load;
Var
  Ret:Boolean;
Begin
  Ret := ISteamUserStats_GetAchievement(PAnsiChar(_Name), _Achieved);
  If Ret Then
  Begin
    _Name := ISteamUserStats_GetAchievementDisplayAttribute(PAnsiChar(_ID), 'name');
		_Description := ISteamUserStats_GetAchievementDisplayAttribute(PAnsiChar(_ID), 'desc');
  End Else
    Log(logWarning, 'Steam', 'GetAchievement failed for Achievement ' + _ID);
End;

Procedure SteamAchievement.Unlock;
Begin
  _Achieved := True;

  // the icon may change once it's unlocked
	//_IconImage := 0;

  // mark it down
  ISteamUserStats_SetAchievement(PAnsiChar(ID));

 // Store stats end of frame
  SteamManager.Instance._StoreStats := True;
End;

End.
