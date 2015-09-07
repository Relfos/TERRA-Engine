// windows port
Unit TERRA_OS;

{$I terra.inc}

{$RANGECHECKS OFF}

{-$DEFINE FOLDERWATCH}

{-$DEFINE TRUE_FULLSCREEN}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Error, TERRA_Utils, TERRA_Application, TERRA_Window, TERRA_InputManager, TERRA_Multimedia,
  Windows, Messages;

Const
	PathSeparator = '\';
	CrLf = #13#10;

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
  {$IFDEF FOLDERWATCH}
  FolderWatcher = Class;

  PFolderWatcherState = ^FolderWatcherState;
  FolderWatcherState = Packed Record
    Overlapped:TOverlapped;
    DirHandle:THandle;
  	Buffer:Array[0..32 * 1024] Of Byte;
  	Param:LPARAM ;
  	NotifyFilter:Cardinal;
  	StopNow:Boolean;
    Owner:FolderWatcher;
  End;

  FolderWatcher = Class(TERRAObject)
    Protected
      _State:FolderWatcherState;
	    _Path:TERRAString;

    Public
      Constructor Create(Const Path:TERRAString);
      Procedure Release; Override;

      Procedure Clear();

      Function Refresh():Boolean;
  End;

  WindowsFolderManager = Class(FolderManager)
    Protected
      _Watchers:Array Of FolderWatcher;
      _WatchCount:Integer;

    Public
      Procedure Update; Override;
      Procedure Release; Override;

      Function WatchFolder(Const Path:TERRAString):Boolean; Override;
  End;
  {$ENDIF}

  WindowsApplication = Class(BaseApplication)
    Protected

      Procedure InitBuildInfo();

      Function InitSettings:Boolean; Override;

      Function CreateWindow():TERRAWindow; Override;

      Function GetDocumentPath():TERRAString; Override;
      Function GetStoragePath():TERRAString; Override;

      Procedure SetProcessorAffinity;

      Procedure ProcessMessages; Override;

    Public
      Constructor Create();

      Function GetClipboard():TERRAString; Override;

      Procedure Yeld; Override;

      Function SetOrientation(Const Value:TERRAOrientation):Boolean; Override;

      Function GetRecommendedSettings: Integer; Override;

      Procedure OpenURL(Const URL:TERRAString); Override;

      Procedure EnableAds(); Override;
      Procedure DisableAds(); Override;

      Procedure SendEmail(DestEmail, Subject, Body:TERRAString); Override;

      Function IsDebuggerPresent:Boolean; Override;

      Function GetDeviceID():TERRAString; Override;

      Procedure OnFatalError(Error:TERRAError); Override;

      Class Function GetCurrentTime:TERRATime;
      Class Function GetCurrentDate:TERRADate;
      Class Function GetTime:Cardinal;

      Class Function Instance:WindowsApplication;
  End;

  Application = WindowsApplication;

Implementation
Uses SysUtils, TERRA_Renderer, TERRA_GLRenderer, TERRA_Engine,
  TERRA_GraphicsManager, TERRA_Log, TERRA_Stream, TERRA_FileUtils, TERRA_FileManager, TERRA_MemoryStream, TERRA_MusicManager,
  TERRA_Gamepad, TERRA_XInput, TERRA_Ethernet, TERRA_Timer, TERRA_Win32Window;

Const
  WHEEL_DELTA = 120;

  FILE_READ_DATA         = $0001; // file & pipe
  FILE_LIST_DIRECTORY    = $0001; // directory
  FILE_WRITE_DATA        = $0002; // file & pipe
  FILE_ADD_FILE          = $0002; // directory
  FILE_APPEND_DATA       = $0004; // file
  FILE_ADD_SUBDIRECTORY  = $0004; // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  FILE_READ_EA           = $0008; // file & directory
  FILE_WRITE_EA          = $0010; // file & directory
  FILE_EXECUTE           = $0020; // file
  FILE_TRAVERSE          = $0020; // directory
  FILE_DELETE_CHILD      = $0040; // directory
  FILE_READ_ATTRIBUTES   = $0080; // all
  FILE_WRITE_ATTRIBUTES  = $0100; // all
  FILE_ALL_ACCESS        = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  FILE_GENERIC_READ      = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE     = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or
    FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE   = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;

Function ChangeDisplaySettings(lpDevMode: PDeviceMode; dwFlags:Cardinal): Longint; stdcall; external 'user32.dll' name 'ChangeDisplaySettingsA';
Function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong:Cardinal): Longint; stdcall; external 'user32.dll' name 'SetWindowLongA';
Function GetProcessAffinityMask(hProcess: THandle; Var lpProcessAffinityMask, lpSystemAffinityMask: PtrUInt):Boolean; stdcall; external 'kernel32.dll';

Var
  _Application_Instance:WindowsApplication;

Constructor WindowsApplication.Create();
Begin
  _Application_Instance := Self;
  Inherited Create();
End;

Class Function WindowsApplication.Instance:WindowsApplication;
Begin
  Result := _Application_Instance;
End;

Class Function WindowsApplication.GetTime:Cardinal;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  //Result := timeGetTime();
  Result := Trunc(Timer.GetElapsedTime() * 1000.0);

  {If (Application.Instance<>Nil) And (Application.Instance.Input.Keys[keyShift]) Then
    Result := Result *4;}

  {$IFDEF USE_TIME_MULTIPLIER}
  Result := Result * TimerMultiplier;
  {$ENDIF}

  //Result := {$IFDEF FPC}GetTickCount(){$ELSE}timeGetTime(){$ENDIF};
End;

Class Function WindowsApplication.GetCurrentTime:TERRATime;
var
  SystemTime: TSystemTime;
Begin
  GetLocalTime(SystemTime);
  Result.Hour:=SystemTime.wHour;
  Result.Minute:=SystemTime.wMinute;
  Result.Second:=SystemTime.wSecond;
  Result.MiliSecond:=SystemTime.wMilliseconds;
End;

Class Function WindowsApplication.GetCurrentDate:TERRADate;
var
  SystemTime: TSystemTime;
Begin
  GetLocalTime(SystemTime);
  Result.Year:=SystemTime.wYear;
  Result.Month:=SystemTime.wMonth;
  Result.Day:=SystemTime.wDay;
  Result.WeekDay:=SystemTime.wDayOfWeek;
End;


Function WindowsApplication.GetClipboard():TERRAString;
Var
  pText:THandle;
  P:PWord;
  N:Word;
Begin
  OpenClipboard(0);
  pText := GetClipboardData(CF_UNICODETEXT);
  CloseClipboard();
  P := GlobalLock(pText);
  Result := '';
  If (P<>Nil) Then
  Begin
    Repeat
      N := P^;
      If (N=0) Then
        Break;

      StringAppendChar(Result, TERRAChar(N));
      Inc(P);
    Until False;

  End;

  GlobalUnlock(pText);
End;

Procedure WindowsErrorCallback(Msg:TERRAString); Cdecl;
Begin
  MessageBoxA(0, PAnsiChar(Msg), 'Fatal error!', MB_OK);
  Application.Instance.Terminate();
End;

//  Creates the window
Function WindowsApplication.CreateWindow():TERRAWindow;
Begin
  Result := Win32Window.Create(Self.GetTitle(), Self.GetWidth(), Self.GetHeight(), Self.GetFullscreen());
End;


// Assign the current thread to one processor. This ensures that timing
// code runs on only one processor, and will not suffer any ill effects
// from power management.

// Based on DXUTSetProcessorAffinity() function from the DXUT framework.
Procedure WindowsApplication.SetProcessorAffinity;
Var
  dwProcessAffinityMask:PtrUInt;
  dwSystemAffinityMask:PtrUInt;
  dwAffinityMask:PtrUInt;
  hCurrentProcess:THANDLE;
  hCurrentThread:THANDLE;
Begin
  dwProcessAffinityMask := 0;
  dwSystemAffinityMask := 0;
  hCurrentProcess := GetCurrentProcess();

  If (Not GetProcessAffinityMask(hCurrentProcess, dwProcessAffinityMask, dwSystemAffinityMask)) Then
    Exit;

  If (dwProcessAffinityMask<>0) Then
  Begin
    // Find the lowest processor that our process is allowed to run against.
    dwAffinityMask := (dwProcessAffinityMask And ((Not dwProcessAffinityMask) + 1));

    // Set this as the processor that our thread must always run against.
    // This must be a subset of the process affinity mask.
    hCurrentThread := GetCurrentThread();
    if (hCurrentThread <> INVALID_HANDLE_VALUE) Then
    Begin
      SetThreadAffinityMask(hCurrentThread, dwAffinityMask);
      //SetThreadIdealProcessor
      CloseHandle(hCurrentThread);
    End;
  End;

  CloseHandle(hCurrentProcess);
End;

Procedure WindowsApplication.ProcessMessages;
Var
  I, PlayerID:Integer;
  Msg:TMsg;
Begin
  Repeat

    If Not (PeekMessageW(Msg, 0 {_Handle}, 0, 0, PM_REMOVE)) Then // Check if there is a message for this window
      Break;

    If (Msg.Message=WM_QUIT) Then     // If WM_QUIT message received then we are done
    Begin
      _Running:=False;
      Exit;
    End;

    // translate and dispatch the message to this window
    TranslateMessage(msg);
    DispatchMessageW(msg);
  Until (False);
End;

Procedure WindowsApplication.Yeld;
Begin
  Sleep(0);
End;

Function ShellExecute(hWnd: HWND; Operation, FileName, Parameters, Directory: PAnsiChar; ShowCmd: Integer): HINST; stdcall; external 'shell32.dll' name 'ShellExecuteA';

Procedure WindowsApplication.OpenURL(Const URL:TERRAString);
Begin
  ShellExecute(Application.Instance.Window.Handle, 'open', PAnsiChar(URL), Nil, Nil, SW_SHOWNORMAL);
End;

Type
  TDebugProc = Function:Boolean; Stdcall;

Var
  Kernel32: HMODULE;
  DebugProc: TDebugProc;

Function WindowsApplication.IsDebuggerPresent: Boolean;
Begin
  If (Self = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Not Assigned(DebugProc)) Then
  Begin
    Kernel32 := GetModuleHandle('kernel32');
    If Kernel32<>0 then
    Begin
      DebugProc := TDebugProc(GetProcAddress(Kernel32, 'IsDebuggerPresent'));
    End;
  End;


  If Assigned(DebugProc) then
    Result := DebugProc()
  Else
    Result := False;
End;

(*Function GetMACAdress():TERRAString;
var
  NCB:TNCB;
  Adapter:TAdapterStatus;

  URetCode: PAnsiChar;
  RetCode: AnsiChar;
  I: integer;
  Lenum:TLanAEnum;
  _SystemID:TERRAString;
  TMPSTR:TERRAString;
begin
  Result    := '';
  _SystemID := '';

  Fillchar(NCB, SizeOf(TNCB), 0);
  Fillchar(Lenum, SizeOf(TLanaEnum), 0);
  Fillchar(Adapter, SizeOf(TAdapterStatus), 0);

  Lenum.Length    := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer  := @Lenum;
  NCB.ncb_length  := SizeOf(Lenum);
  RetCode         := Netbios(@NCB);

  i := 0;
  Repeat
    Fillchar(NCB, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBRESET);
    Ncb.ncb_lana_num := lenum.lana[I];
    RetCode          := Netbios(@Ncb);

    Fillchar(NCB, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBASTAT);
    Ncb.ncb_lana_num := lenum.lana[I];
    // Must be 16
    Ncb.ncb_callname := '*               ';

    Ncb.ncb_buffer := @Adapter;

    Ncb.ncb_length := SizeOf(TAdapterStatus);
    RetCode        := Netbios(@Ncb);
    //---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin
      _SystemId := HexStr(Ord(Adapter.adapter_address[0])) + '-' +
        HexStr(Ord(Adapter.adapter_address[1])) + '-' +
        HexStr(Ord(Adapter.adapter_address[2])) + '-' +
        HexStr(Ord(Adapter.adapter_address[3])) + '-' +
        HexStr(Ord(Adapter.adapter_address[4])) + '-' +
        HexStr(Ord(Adapter.adapter_address[5]));
    end;
    Inc(i);
  until (I >= Ord(Lenum.Length)) or (_SystemID <> '00-00-00-00-00-00');

  Result := _SystemID;
End;*)

Function WindowsApplication.GetDeviceID:TERRAString;
Begin
  Result := GetMACAdress();
End;

Procedure WindowsApplication.SendEmail(DestEmail, Subject, Body:TERRAString);
Begin

End;

Function WindowsApplication.SetOrientation(Const Value:TERRAOrientation):Boolean;
Var
  Temp:Boolean;
  Rect:TRect;
Begin
  Temp := (IsLandscapeOrientation(Self.Orientation));

  Result := Inherited SetOrientation(Value);

  If Not Result Then
    Exit;

  {If ((IsLandscapeOrientation(Self.Orientation)) <> Temp) Then
  Begin
    GetWindowRect(_Handle, Rect);
    SetWindowPos(_Handle, 0, Rect.Left, Rect.Top, _Height, _Width, $400);
  End;}
End;

Procedure WindowsApplication.DisableAds;
Begin
  {$IFDEF SHOWADS}
  Self.SetViewport(0, 0, Application.Instance.Width, Application.Instance.Height);
  {$ENDIF}
End;

Procedure WindowsApplication.EnableAds;
Begin
  {$IFDEF SHOWADS}
  Self.SetViewport(0, 50, Application.Instance.Width, Application.Instance.Height);
  //Self.SetViewport(0, 0, Application.Instance.Width, Application.Instance.Height-50);
  {$ENDIF}
End;

Function WindowsApplication.GetStoragePath: TERRAString;
Begin
  _StoragePath := Self.GetDocumentPath();
  Result := _StoragePath;
End;

Function WindowsApplication.GetDocumentPath:TERRAString;
Begin
  _DocumentPath := GetCurrentDir() + PathSeparator + 'Data';
  If Not DirectoryExists(_DocumentPath) Then
  Begin
    CreateDir(_DocumentPath);
  End;

  Result := _DocumentPath;
End;

Function WindowsApplication.GetRecommendedSettings: Integer;
Begin
  If (Not Engine.Graphics.Renderer.Features.Shaders.Avaliable) Then
    Result := settingsHintLow
  Else
    Result := settingsHintHigh;
End;

{$IFDEF FOLDERWATCH}
{ WindowsFolderManager }
Procedure WindowsFolderManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_WatchCount) Do
    ReleaseObject(_Watchers[I]);
End;

Procedure WindowsFolderManager.Update;
Begin
End;

Function WindowsFolderManager.WatchFolder(const Path: TERRAString): Boolean;
Begin
  Inc(_WatchCount);
  SetLength(_Watchers, _WatchCount);
  _Watchers[Pred(_WatchCount)]:= FolderWatcher.Create(Path);
End;

Procedure WatchCallback(dwErrorCode, dwNumberOfBytesTransfered:Cardinal; Overlapped:POverlapped); Stdcall;
Type
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset:Cardinal;
    Action:Cardinal;
    FileNameLength:Cardinal;
    FileName:PWideChar;
  End;
Var
  szFile:Array[0..Pred(MAX_PATH)] Of AnsiChar;
  pNotify:PFileNotifyInformation;
  count, offset, I:Integer;
  State:PFolderWatcherState;
  WC:PWideChar;
  FileName:TERRAString;
Begin
  If (dwNumberOfBytesTransfered <= 0) Then
    Exit;

  If (dwErrorCode <> ERROR_SUCCESS) Then
    Exit;

  State := PFolderWatcherState(Overlapped);
  Offset := 0;

  Repeat
    Move(State.Buffer[Offset], pNotify, SizeOf(PtrUInt));
    Inc(Offset, pNotify.NextEntryOffset);


    FileName := '';
    WC := pNotify.FileName;
    For I:=0 To Pred(pNotify.FileNameLength) Do
      FileName := FileName + AnsiChar(WC^);

    FolderManager.Instance.NotifyFileChange(FileName);

  Until (pNotify.NextEntryOffset = 0);

	If (Not State.StopNow) Then
  Begin
	  State.Owner.Refresh();
  End;
End;

{ FolderWatcher }
Constructor FolderWatcher.Create(const Path: TERRAString);
Begin
  _Path := Path;
  _State.Owner := Self;

  _State.DirHandle := CreateFile(PChar(Path), FILE_LIST_DIRECTORY,
			FILE_SHARE_READ Or FILE_SHARE_WRITE Or FILE_SHARE_DELETE, 0,
			OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS Or FILE_FLAG_OVERLAPPED, 0);

  If (_State.DirHandle <> INVALID_HANDLE_VALUE) Then
  Begin
    _State.Overlapped.hEvent := CreateEvent(0, True, False, 0);
    _State.NotifyFilter := FILE_NOTIFY_CHANGE_CREATION Or FILE_NOTIFY_CHANGE_SIZE Or FILE_NOTIFY_CHANGE_LAST_WRITE;

    If (Not Self.Refresh()) Then
    Begin
      Self.Clear();
    End;
  End;
End;

Procedure FolderWatcher.Release;
Begin
  _State.StopNow := True;

  CancelIo(_State.DirHandle);

  Self.Refresh();

  Sleep(5);
  Clear();
End;

Procedure FolderWatcher.Clear;
Begin
  CloseHandle(_State.Overlapped.hEvent);
	CloseHandle(_State.DirHandle);
End;


Function FolderWatcher.Refresh: Boolean;
Begin
  // _clear ? 0 :
  Result := ReadDirectoryChangesW(_State.DirHandle, @_State.Buffer[0], Sizeof(_State.Buffer), False, $FFFFFFFF{_State.NotifyFilter}, 0, @_State.Overlapped, @WatchCallback);
End;
{$ENDIF}

Procedure WindowsApplication.InitBuildInfo();
Var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  FileName:PAnsiChar;
Begin
  Log(logDebug, 'App', 'Getting build info');

  _BundleVersion := '';

  FileName := PAnsiChar(ParamStr(0));
  VerInfoSize := GetFileVersionInfoSize(FileName, Dummy);
  if VerInfoSize > 0 then
  begin
      GetMem(VerInfo, VerInfoSize);
      Try
        If GetFileVersionInfo(FileName, 0, VerInfoSize, VerInfo) then
        Begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          //V1 := dwFileVersionMS shr 16;
            _BundleVersion := CardinalToString(VerValue.dwFileVersionMS and $FFFF) + '.' + CardinalToString(VerValue.dwFileVersionLS shr 16) + '.'+ CardinalToString(VerValue.dwFileVersionLS and $FFFF);
            Log(logDebug, 'App', 'Found '+_BundleVersion);
        End;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
  End;
End;

Function WindowsApplication.InitSettings: Boolean;
Var
  I:Integer;
  Size:Cardinal;
  Buf:Array[0..1023] Of AnsiChar;
  SystemInfo:TSystemInfo;
  Mask, ProcessAffinityMask, SystemAffinityMask:PtrUInt;
Begin
  Inherited InitSettings;

  _Screen.Width := GetSystemMetrics(SM_CXSCREEN);
  _Screen.Height := GetSystemMetrics(SM_CYSCREEN);

  If (_TempPath = '') Then
  Begin
    GetTempPathA(1024, @(Buf[0]));
    _TempPath := Buf;
    SetLength(_TempPath, Pred(Length(_TempPath)));
  End;

  If (_FontPath = '') Then
  Begin
    GetWindowsDirectoryA(@(Buf[0]), 1024);
    _FontPath := Buf;
    _FontPath := _FontPath + PathSeparator + 'Fonts';
  End;

  If (_CurrentUser = '') Then
  Begin
    GetUserNameA(@(Buf[0]), Size);
    _CurrentUser := Buf;
  End;

  // Detect system language
  Size := GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SABBREVLANGNAME, Nil, 0);
  SetLength(_Language, Succ(Size));
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SABBREVLANGNAME, @(_Language[1]), Size);
  SetLength(_Language, 2);
  _Language := StringUpper(_Language);

  Size := GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, Nil, 0);
  SetLength(_Country, Succ(Size));
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, PChar(_Country), Size);
  SetLength(_Country, 2);
  _Country := StringUpper(_Country);

  GetSystemInfo(SystemInfo);

  Log(logDebug, 'App', 'Getting cpu cores');
  _CPUCores := 0;
  If GetProcessAffinityMask(GetCurrentProcess(), ProcessAffinityMask, SystemAffinityMask) Then
  Begin
    For I:=0 To 31 Do
    Begin
      Mask := DWord(1) shl i;
      If (ProcessAffinityMask and Mask)<>0 then
        Inc(_CPUCores);
    End;
  End;

  If (_CPUCores<=0) Then
    _CPUCores := SystemInfo.dwNumberOfProcessors;
  Log(logDebug, 'App', 'Found '+ IntegerProperty.Stringify(_CPUCores)+' cores');

  Self.SetProcessorAffinity();

  // Initialize xinput gamepads
  For I:=0 To 3 Do
    Engine.Input.AddGamePad(XInputGamePad.Create(I));

  // Initialize other joysticks/gamepads
  For I:=0 To 3 Do
    Engine.Input.AddGamePad(WindowsGamePad.Create(I));

  Self.InitBuildInfo();

  Engine.Renderers.Add(OpenGLRenderer.Create());

  Result := True;
End;


Procedure WindowsApplication.OnFatalError(Error:TERRAError);
Var
  S:TERRAString;
Begin
  _Running := False;

  S := 'A fatal error has occurred.' + CrLf + Error.Description + CrLf+ Error.CrashLog + CrLf+ Error.Callstack.GetDescription();
  Windows.MessageBoxA(0, PAnsiChar(S), PAnsiChar(GetProgramName()), MB_OK Or MB_ICONERROR);
End;

Initialization
  LoadMultimedia();
Finalization
  ReleaseObject(_Application_Instance);
End.
