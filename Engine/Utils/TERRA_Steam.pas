Unit TERRA_Steam;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Math, TERRA_Application, SteamAPI;

Type
  Steam = Class(ApplicationComponent)
    Protected
      _Enabled:Boolean;
      _Running:Boolean;
      _LoggedOn:Boolean;
      _StatsRequested:Boolean;

      _SteamID:AnsiString;
      _AppID:AnsiString;

      _UserName:AnsiString;
      _Language:AnsiString;

      _LicenseResult:SteamUserHasLicenseForAppResult;

      Procedure Update; Override;
      Procedure Init; Override;

    Public
      Class Function Instance:Steam;

      Function UnlockAchievement(AchID:AnsiString):Boolean;

      Destructor Destroy; Override;

      Property UserName:AnsiString Read _UserName;
      Property Language:AnsiString Read _Language;
      Property SteamID:AnsiString Read _SteamID;
      Property AppID:AnsiString Read _AppID;
      Property Enabled:Boolean Read _Enabled;
  End;


Implementation
Uses TERRA_OS, TERRA_Log, TERRA_GraphicsManager;

Var
  _Steam_Instance:ApplicationObject = Nil;


{ Steam }
Class Function Steam.Instance:Steam;
Begin
  If _Steam_Instance = Nil Then
    _Steam_Instance := InitializeApplicationComponent(Steam, Nil);

  Result := Steam(_Steam_Instance.Instance);
End;

Procedure Steam.Init;
Begin
  _LoggedOn := False;

  Log(logDebug, 'Steam', 'Trying to load Steam library...');
  _Enabled := LoadSteamAPI();
  If Not _Enabled Then
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

  //_LicenseResult := ISteamGameServer_UserHasLicenseForApp(steamID:SteamID; appID:SteamAppId):
End;

Destructor Steam.Destroy;
Var
  I:Integer;
Begin
  If _Running Then
  Begin
    _Running := False;
    SteamAPI_Shutdown();
  End;

  _Steam_Instance := Nil;
End;

Procedure Steam.Update;
Begin
  If Not _Running Then
    Exit;

  // Is the user logged on?  If not we can't get stats.
  If Not _StatsRequested Then
  Begin
    _LoggedOn := ISteamUser_BLoggedOn();

    If (_LoggedOn) Then
    Begin
      _StatsRequested := ISteamUserStats_RequestCurrentStats();
    End;
  End;

  SteamAPI_RunCallbacks();
End;

Function Steam.UnlockAchievement(AchID: AnsiString): Boolean;
Begin
  Result := False;

  If (Not _Running) Or (Not _LoggedOn) Then
    Exit;

  ISteamUserStats_SetAchievement(PAnsiChar(AchID));
  Result := ISteamUserStats_StoreStats();
End;

End.
