Unit TERRA_Steam;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Math, TERRA_Application, SteamAPI, SteamCallback;

Type
  SteamAchievement = Class(TERRAObject)
    Protected
      _ID:TERRAString;
      _Name:TERRAString;
      _Description:TERRAString;
      _Achieved:Boolean;

      Procedure Load();

    Public
      Constructor Create(Const ID:TERRAString);

      Function Unlock():Boolean;

      Property ID:TERRAString Read _ID;
      Property Name:TERRAString Read _Name;
      Property Description:TERRAString Read _Description;
      Property Achieved:Boolean Read _Achieved;
  End;

  SteamStat = Class(TERRAObject)
    Protected
      _ID:TERRAString;
      _Value:Integer;

      Procedure Load();

      Procedure SetValue(Const Val:Integer);

    Public
      Constructor Create(Const ID:TERRAString);

      Property Value:Integer Read _Value Write SetValue;
  End;

  SteamLeaderboard = Class(TERRAObject)
    Protected
      _ID:TERRAString;

    Public
      Constructor Create(Const ID:TERRAString);

      Procedure Post(Const Value:Integer);
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

      _Stats:Array Of SteamStat;
      _StatCount:Integer;

      Procedure OnUserStats(P:Pointer);

    Public
      Class Function Instance:SteamManager;

      Procedure Update; Override;
      Procedure Init; Override;

      Function GetAchievement(Const AchID:TERRAString):SteamAchievement;
      Function AddAchievement(Const AchID:TERRAString):SteamAchievement;
      Function UnlockAchievement(Const AchID:TERRAString):Boolean;

      Function GetStat(Const StatID:TERRAString):SteamStat;
      Function AddStat(Const StatID:TERRAString):SteamStat;

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
   CurrentAppID:Cardinal;
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

  CurrentAppID := ISteamUtils_GetAppID();
  If (SteamAPI_RestartAppIfNecessary(CurrentAppID)) Then
  Begin
    _Running := False;
    Application.Instance.Terminate(False);
    Exit;
  End;

  _AppID := CardinalToString(CurrentAppID);
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


  For I:=0 To Pred(_AchievementCount) Do
    ReleaseObject(_Achievements[I]);

  For I:=0 To Pred(_StatCount) Do
    ReleaseObject(_Stats[I]);

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
  If _StatsRequested Then
  Begin
    _LoggedOn := ISteamUser_BLoggedOn();

    If (_LoggedOn) Then
    Begin
      SteamCallbackDispatcher.Create(SteamStatsCallbackID , Self.OnUserStats, SizeOf(Steam_UserStatsReceived));
      If ISteamUserStats_RequestCurrentStats() Then
        _StatsRequested := False; 
    End;
  End;

  If _StoreStats Then
  Begin
    // If this failed, we never sent anything to the server, try again later.
    If ISteamUserStats_StoreStats() Then
			_StoreStats := False;
  End;

  SteamAPI_RunCallbacks();

(*  For I:=0 To 3 Do
  Begin
       If ISteamController_GetControllerState(I, controllerState) Then
       Begin

       End;
  End;*)
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
  For I:=0 To Pred(_StatCount) Do
  Begin
    _Stats[I].Load();
  End;
End;

{ SteamAchievement }
Constructor SteamAchievement.Create(const ID: TERRAString);
Begin
  Self._ID := ID;
End;

Procedure SteamAchievement.Load;
Var
  Ret:Boolean;
Begin
  Ret := ISteamUserStats_GetAchievement(PAnsiChar(_ID), _Achieved);
  If Ret Then
  Begin
    _Name := ISteamUserStats_GetAchievementDisplayAttribute(PAnsiChar(_ID), 'name');
		_Description := ISteamUserStats_GetAchievementDisplayAttribute(PAnsiChar(_ID), 'desc');
  End Else
    Log(logWarning, 'Steam', 'GetAchievement failed for Achievement ' + _ID);
End;

Function SteamAchievement.Unlock():Boolean;
Begin
  If (_Achieved) Or (Not SteamManager.Instance._LoggedOn) Then
  Begin
    Result := False;
    Exit;
  End;

  _Achieved := True;

  // the icon may change once it's unlocked
	//_IconImage := 0;

  // mark it down
  ISteamUserStats_SetAchievement(PAnsiChar(ID));

 // Store stats end of frame
  SteamManager.Instance._StoreStats := True;

  Result := True;
End;

Function SteamManager.AddAchievement(const AchID: TERRAString):SteamAchievement;
Begin
  Result := Self.GetAchievement(AchID);
  If Assigned(Result) Then
    Exit;

  Result := SteamAchievement.Create(AchID);
  Inc(_AchievementCount);
  SetLength(_Achievements, _AchievementCount);
  _Achievements[Pred(_AchievementCount)] := Result;

  _StatsRequested := True;
End;

Function SteamManager.GetAchievement(const AchID: TERRAString): SteamAchievement;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AchievementCount) Do
  If _Achievements[I]._ID = AchID Then
  Begin
    Result := _Achievements[I];
    Exit;
  End;

  Result := Nil;
End;

Function SteamManager.UnlockAchievement(const AchID: TERRAString): Boolean;
Var
  Ach:SteamAchievement;
Begin
  Ach := Self.GetAchievement(AchID);
  If Ach = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  Result := Ach.Unlock();
End;

Function SteamManager.AddStat(const StatID: TERRAString): SteamStat;
Begin
  Result := Self.GetStat(StatID);
  If Assigned(Result) Then
    Exit;

  Result := SteamStat.Create(StatID);
  Inc(_StatCount);
  SetLength(_Stats, _StatCount);
  _Stats[Pred(_StatCount)] := Result;

  _StatsRequested := True;
End;

Function SteamManager.GetStat(const StatID: TERRAString): SteamStat;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_StatCount) Do
  If _Stats[I]._ID = StatID Then
  Begin
    Result := _Stats[I];
    Exit;
  End;

  Result := Nil;
End;

{ SteamStat }
Constructor SteamStat.Create(const ID: TERRAString);
Begin
  Self._ID := ID;
End;

Procedure SteamStat.Load;
Begin
  ISteamUserStats_GetStatInt(PAnsiChar(_ID), _Value);
  Log(logDebug, 'Steam', 'Stat '+_ID + ' = ' + IntToString(_Value));
End;


Procedure SteamStat.SetValue(const Val: Integer);
Begin  
  _Value := Val;

  If (Not SteamManager.Instance._LoggedOn) Then
    Exit;

  ISteamUserStats_SetStatInt(PAnsiChar(_ID), _Value);
  SteamManager.Instance._StoreStats := True;
End;

{ SteamLeaderboard }
Constructor SteamLeaderboard.Create(const ID: TERRAString);
Begin
  Self._ID := ID;

(* SteamAPICall_t hSteamAPICall = SteamUserStats()->FindLeaderboard(pchLeaderboardName);
 m_callResultFindLeaderboard.Set(hSteamAPICall, this,
   &CSteamLeaderboards::OnFindLeaderboard);*)
End;

Procedure SteamLeaderboard.Post(const Value: Integer);
Begin

End;

End.

