// windows port
Unit TERRA_OS;

{$I terra.inc}

{$RANGECHECKS OFF}

{-$DEFINE FOLDERWATCH}

{-$DEFINE TRUE_FULLSCREEN}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_InputManager, TERRA_Multimedia,
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
      _Handle:Cardinal;
      _Icon:HICON;
      _MultisampleFormat:Cardinal;
      _MultiSampleInitialized:Boolean;
      _savedExStyle:Cardinal;
      _savedStyle:Cardinal;
      _rcSaved:TRect;
      _PosX:Integer;
      _PosY:Integer;
      _BorderWidth:Integer;
      _BorderHeight:Integer;
      _CursorVisible:Boolean;

      _OriginalWidth:Integer;
      _OriginalHeight:Integer;

      _FullscreenActive:Boolean;

      Procedure InitIcon();
      Procedure InitBuildInfo();

      Function InitSettings:Boolean; Override;
      Function InitWindow:Boolean; Override;

      Procedure CloseWindow; Override;

      Function GetClipboard():TERRAString;

      Function GetDocumentPath():TERRAString; Override;
      Function GetStoragePath():TERRAString; Override;

      Procedure SetProcessorAffinity;

      Procedure ProcessMessages; Override;

    Public
      Constructor Create();

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Override;
      Procedure SetState(State:Cardinal); Override;
      Procedure Yeld; Override;

      Function SetOrientation(Value:Integer):Boolean; Override;

      Function GetRecommendedSettings: Integer; Override;

      Procedure OpenAppStore(URL:TERRAString); Override;

      Procedure EnableAds(); Override;
      Procedure DisableAds(); Override;

      Procedure SetTitle(Const Name:TERRAString); Override;

      Procedure SendEmail(DestEmail, Subject, Body:TERRAString); Override;

      Function IsDebuggerPresent:Boolean; Override;

      Function GetDeviceID():TERRAString; Override;

      Procedure OnFatalError(Const ErrorMsg, CrashLog, Callstack:TERRAString); Override;

      Class Function GetCurrentTime:TERRATime;
      Class Function GetCurrentDate:TERRADate;
      Class Function GetTime:Cardinal;

      Class Function Instance:WindowsApplication;

      Property Handle:Cardinal Read _Handle;
  End;

  Application = WindowsApplication;

Implementation
Uses TERRA_Error, SysUtils, TERRA_Renderer, TERRA_GLRenderer,
  TERRA_GraphicsManager, TERRA_Log, TERRA_Stream, TERRA_FileUtils, TERRA_FileManager, TERRA_MemoryStream, TERRA_MusicManager,
  TERRA_Gamepad, TERRA_XInput, TERRA_Ethernet, TERRA_Timer;

Const
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

//WM_WINDOWPOSCHANGING
Function WndProc(hWnd:HWND;Msg:UINT;wParam:wPARAM;lParam:LPARAM):LRESULT; Stdcall;
Var
  Delta:Integer;
  P:MouseCursor;
  W,H, I:Integer;
  S:TERRAString;
  sz:TRECT;
  Temp:Boolean;
  App:WindowsApplication;
Begin
  Result:=0;

  App := WindowsApplication(Application.Instance);
  If (Not Assigned(App)) Then
  Begin
    Result := DefWindowProcW(hWnd,Msg,wParam,lParam);
    Exit;
  End;

  If (*((Msg=WM_SYSCOMMAND) And (wParam = SC_MAXIMIZE))
  Or *)((Msg=WM_SYSKEYDOWN) And (wParam = keyEnter)) Then
  Begin
    App._ChangeToFullScreen := True;
    Exit;
  End;

  If (Msg=WM_SYSKEYDOWN) And (wParam = (VK_MENU)) Then
  Begin
    Result := 0;
    Exit;
  End;

  Case (Msg) Of
    MM_MCINOTIFY:
      Begin
        // end of play has been reached
        If (wParam = mci_Notify_Successful) And (MusicManager.Instance.CurrentTrack<>Nil) Then
          MusicManager.Instance.CurrentTrack.Play();
      End;

    WM_SYSCOMMAND:
      Begin
        If (wParam = SC_MINIMIZE) Then
        Begin
          App._State := wsMinimized;
          App.OnStateChange(App._State);
        End Else
        If (wParam = SC_RESTORE) Then
        Begin
          App._State := wsNormal;
          App.OnStateChange(App._State);
        End;

        Result := DefWindowProcW(hWnd,Msg,wParam,lParam);
      End;

    WM_ACTIVATE,WM_ACTIVATEAPP:
      Begin
        {$IFDEF TRUE_FULLSCREEN}
        Temp := App._FullScreen;
        If (wParam = 0) Then
        Begin
          If (App.FullScreen) And (App._FullscreenActive) Then
            App.SetFullscreenMode(False);
        End Else
        Begin
          If (App.FullScreen)  And (Not App._FullscreenActive) Then
            App.SetFullscreenMode(True);
        End;

        App._FullScreen := Temp;
        {$ENDIF}
      End;

    (*WM_SIZING:Begin
                Move(Pointer(lparam)^, SZ, SizeOf(TRect));
                w := sz.right - sz.left;
                h := sz.bottom - sz.top;

                Case wParam Of
                  WMSZ_LEFT,WMSZ_RIGHT:
                    Begin
                      // Modify the Heigh of the window
                      sz.bottom := Trunc(W * App.AspectRatio) + sz.top;
                    End;

                  WMSZ_TOP, WMSZ_BOTTOM:
                    Begin
                      // Modify the Width of the window
                      sz.right := Trunc(H / App.AspectRatio) + sz.left;
                    End;

                  WMSZ_TOPRIGHT, WMSZ_TOPLEFT, WMSZ_BOTTOMRIGHT, WMSZ_BOTTOMLEFT:
                  Begin
                    // Adjust the width and height of the window to match aspect ratio
                    h := Trunc(w * App.aspectRatio);

                    // Adjust Height
                    If (wParam = WMSZ_TOPLEFT) Or (wParam = WMSZ_TOPRIGHT) Then
                      sz.top := sz.bottom - h
                    Else
                     sz.bottom := sz.top + h;

                    // Adjust Width
                    If (wParam = WMSZ_TOPLEFT) Or (wParam = WMSZ_BOTTOMLEFT) Then
                      sz.left := sz.right - w
                    Else
                      sz.right := sz.left + w;
                  End;
                End;

                Move(SZ, Pointer(lparam)^, SizeOf(Trect));
              End;*)

    WM_SIZE:  Begin
                P := PCursor(@lParam)^;
                App.AddCoordEvent(eventWindowResize, P.X, P.Y, 0);
              End;

    WM_CLOSE: Begin
                PostQuitMessage(0);
                App._Running := False;
                App._CanReceiveEvents := False;
              End;

    WM_KEYDOWN: If (App._CanReceiveEvents) Then
                Begin // Set the pressed key (wparam) to equal true so we can check if its pressed
                  App.AddValueEvent(eventKeyDown, wParam);
                End;

    WM_KEYUP: If (App._CanReceiveEvents) Then
              Begin // Set the released key (wparam) to equal false so we can check if its pressed
                App.AddValueEvent(eventKeyUp, wParam);
              End;

    WM_CHAR:  If (App._CanReceiveEvents) Then
              Begin
                If (wParam=22) And  ($8000 And GetKeyState(VK_CONTROL)<>0) Then
                Begin
                  S := WindowsApplication(App).GetClipboard();
                  For I:=1 To Length(S) Do
                    App.AddValueEvent(eventKeyPress, Ord(S[I]));
                End Else
                  App.AddValueEvent(eventKeyPress, wParam);
              End;

    WM_LBUTTONDOWN: If (App._CanReceiveEvents) Then
                    Begin
                      SetCapture(App._Handle);
                      App.AddValueEvent(eventMouseDown, keyMouseLeft);
                    End;

    WM_RBUTTONDOWN: If (App._CanReceiveEvents)  Then
                    Begin
                      App.AddValueEvent(eventMouseDown, keyMouseRight);
                    End;

    WM_MBUTTONDOWN: If (App._CanReceiveEvents) Then
                    Begin
                      App.AddValueEvent(eventMouseDown, keyMouseMiddle);
                    End;

    WM_RBUTTONUP: If (App._CanReceiveEvents) Then
                  Begin
                    App.AddValueEvent(eventMouseUp, keyMouseRight);
                  End;

    WM_LBUTTONUP: If (App._CanReceiveEvents) Then
                  Begin
                    ReleaseCapture;
                    App.AddValueEvent(eventMouseUp, keyMouseLeft);
                  End;

    WM_MBUTTONUP: If (App._CanReceiveEvents) Then
                  Begin
                    App.AddValueEvent(eventMouseUp, keyMouseMiddle);
                  End;

    WM_MOUSEMOVE: If (App._CanReceiveEvents) Then
                  Begin
                    P := PCursor(@lParam)^;
                    App.AddCoordEvent(eventMouseMove, P.X, P.Y, 0);

                    If (App._CursorVisible) And (Not App._IgnoreCursor) Then
                    Begin
                      App._CursorVisible := False;
                      ShowCursor(False);
                    End;

                  End;

    WM_NCMOUSEMOVE: Begin
                      If (Not App._CursorVisible) And (Not App._IgnoreCursor)  Then
                      Begin
                        App._CursorVisible := True;
                        ShowCursor(True);
                      End;
                    End;

    WM_MOUSEWHEEL:If (App._CanReceiveEvents) Then
                  Begin
                    Delta := Integer(wParam Div High(Word));
                    App.AddValueEvent(eventMouseWheel, Delta);
                  End;

    WM_KILLFOCUS: If (App._CanReceiveEvents) Then
                  Begin
                    InputManager.Instance.Keys.Reset();
                    //App.OnDeactivate;
                  End;

    Else
      Begin
        Result := DefWindowProcW(hWnd,Msg,wParam,lParam);  // Default result if nothing happens
      End;
  End;
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

      StringAppendChar(Result, N);
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
Function WindowsApplication.InitWindow:Boolean;
Var
  I:Integer;
  wndClass:TWndClassW;         // Window class
  dwStyle:Cardinal;            // Window styles
  dwExStyle:Cardinal;          // Extended window styles
  Inst:HINST;             // Current instance
  X,Y,BW,BH:Integer;
  TitleStr:WideString;
Begin
  Result := False;

  {$IFDEF FOLDERWATCH}
  If _FolderManager = Nil Then
  Begin
    _FolderManager := WindowsFolderManager.Create();
    _FolderManager.Init();
  End;
  {$ENDIF}

//  FatalErrorHandler := WindowsErrorCallback;

  Inst := GetModuleHandle(Nil);        // Grab an instance for our window

  If Not _MultiSampleInitialized Then
  Begin
    FillChar(WndClass,SizeOf(wndClass),0); // Clear the window class structure

    _Icon := LoadIcon(Inst,'MAIN_ICON');

    With wndClass Do                    // Set up the window class
    Begin
     Style:=CS_HREDRAW Or  // Redraws entire window if length changes
            CS_VREDRAW Or  // Redraws entire window if height changes
            CS_OWNDC;      // Unique device context for the window
     lpfnWndProc := @WndProc;  // Set the window procedure to our func WndProc
     hInstance := Inst;
     hCursor := LoadCursor(0,IDC_ARROW);
     hIcon := _Icon;
     lpszClassName:='TERRA';
    End;

    If (RegisterClassW(wndClass)=0) Then  // Attemp to register the window class
    Begin
      RaiseError('Failed to register the window class.');
      Exit;
    End;
  End;

  If (_FullScreen) Then
  Begin
    dwExStyle := WS_EX_APPWINDOW;
    dwStyle := WS_POPUP;
    X:=0;
    Y:=0;
    BW:=0;
    BH:=0;
  End Else
  Begin
    dwExStyle := WS_EX_OVERLAPPEDWINDOW{ Or WS_EX_COMPOSITED};
    dwStyle := WS_OVERLAPPED Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX Or WS_SIZEBOX Or WS_CAPTION Or WS_SYSMENU;
//    dwStyle := WS_POPUP Or WS_BORDER;
    X := (_Screen.Width - Width) Div 2;
    Y := (_Screen.Height - Height) Div 2;

    //BW := GetSystemMetrics(SM_CXFIXEDFRAME)+GetSystemMetrics(SM_CXEDGE)*2;
    BH := GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYSIZEFRAME)*2+GetSystemMetrics(SM_CYEDGE)*2;
    BW := GetSystemMetrics(SM_CYSIZEFRAME)*2+GetSystemMetrics(SM_CYEDGE)*2;
//    BH := GetSystemMetrics(SM_CYSIZEFRAME)*2;
  End;

  _BorderWidth := BW;
  _BorderHeight := BH;

  _OriginalWidth := _Width;
  _OriginalHeight := _Height;

  _PosX := X;
  _PosY := Y;

  _CanReceiveEvents := True;

  TitleStr := WideString(Self.Title);

  // Attempt to create the actual window
  _Handle := CreateWindowExW(dwExStyle,    // Extended window styles
                          'TERRA',       // Class name
                          @(TitleStr[1]), // Window title (caption)
                          dwStyle,      // Window styles
                          X,Y,          // Window position
                          Width+BW,     // Size of window
                          Height+BH,
                          HWND_DESKTOP, // No parent window
                          0,            // No menu
                          Inst ,    // Instance
                          Nil);         // Pass nothing to WM_CREATE

  Assert(_Handle<>0,'Unable to create window.');

  //_Width := 600;
  //_Height := 1136;
  If (_Width>Screen.Width) Or (_Height>Screen.Height) Then
    SetWindowPos(_Handle, Cardinal(00), Cardinal(00), 0, _Width, _Height, $400);

  If Not _Ready Then
  Begin
    _Ready := True;

    If (_Icon<>0) Then
    Begin
      SendMessage(_Handle, WM_SETICON, ICON_SMALL, _Icon);
      SendMessage(_Handle, WM_SETICON, ICON_BIG, _Icon);
    End Else
      InitIcon();

    If (Self.IsDebuggerPresent()) Then
      ForceLogFlush := True;
  End;

  If Not _Hidden Then
  Begin
    UpdateWindow(_Handle);
    ShowWindow(_Handle,SW_SHOW);
    SetForegroundWindow(_Handle);
    SetFocus(_Handle);

    If (Not _IgnoreCursor) Then
      ShowCursor(False);
  End;

  _CursorVisible := False;
  //ReleaseCapture;
  //SetCapture(_Handle);

  If (_FullScreen) Then
  Begin
    _FullScreen := False;
    ToggleFullScreen;
  End;
  
  Result := True;
End;

// Assign the current thread to one processor. This ensures that timing
// code runs on only one processor, and will not suffer any ill effects
// from power management.
//
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

Procedure WindowsApplication.CloseWindow;
Begin
	If (_Fullscreen)	Then
    ToggleFullScreen();

  If (Not _IgnoreCursor) Then
    ShowCursor(True);
  //ReleaseCapture;
    
  If ((_Handle <> 0)And(Not DestroyWindow(_Handle)))Then
  Begin
    RaiseError('Unable to destroy window.');
    _Handle:=0;
  End;

 // Attempts to unregister the window class
  If (Not UnRegisterClass('TERRA',hInstance))Then
  Begin
    RaiseError('Unable to unregister window class.');
  End;
End;

Procedure WindowsApplication.SetState(State:Cardinal);
Begin
  _State := State;
  Case State Of
  wsNormal:    ShowWindow(_Handle,SW_RESTORE);
  wsMinimized: ShowWindow(_Handle,SW_MINIMIZE);
  wsMaximized: ShowWindow(_Handle,SW_MAXIMIZE);
  End;
End;

Function WindowsApplication.SetFullscreenMode(UseFullScreen:Boolean):Boolean;
Var
  FullWidth, FullHeight:Integer;
  ScreenSettings:DevMode;
  Flags:Cardinal;
Begin
  _Fullscreen := UseFullScreen;
  _FullscreenActive := _FullScreen;

  If (_FullScreen) Then
  Begin
    // Moving to full screen mode.
    _savedExStyle := GetWindowLong(_Handle, GWL_EXSTYLE);
    _savedStyle := GetWindowLong(_Handle, GWL_STYLE);
    GetWindowRect(_Handle, _rcSaved);

    {$IFDEF TRUE_FULLSCREEN}
    FullWidth := _OriginalWidth;
    FullHeight := _OriginalHeight;

    ZeroMemory(@ScreenSettings,SizeOf(ScreenSettings));
    //ScreenSettings.dmDisplayFrequency := 60;

    EnumDisplaySettings(Nil, 0, ScreenSettings);

    With ScreenSettings Do
    Begin   // Set parameters for the screen setting
     dmSize:=SizeOf(ScreenSettings);
     dmPelsWidth:= FullWidth;     // Window width
     dmPelsHeight:= FullHeight;   // Window height
     dmBitsPerPel:= 32;      // Window color depth
     dmFields := DM_PELSWIDTH Or DM_PELSHEIGHT Or DM_BITSPERPEL Or DM_DISPLAYFREQUENCY;
    End;

    // Try to change screen mode to fullscreen
    If (ChangeDisplaySettings(@ScreenSettings, CDS_FULLSCREEN)=DISP_CHANGE_FAILED)Then
    Begin
      Log(logError, 'App', 'Unable to switch to fullscreen.');
     _Fullscreen := False;
      Exit;
    End;

    {$ELSE}
    FullWidth := _Screen.Width;
    FullHeight := _Screen.Height;
    {$ENDIF}

    Flags := WS_POPUP Or WS_CLIPCHILDREN Or WS_CLIPSIBLINGS;
    SetWindowLong(_Handle, GWL_EXSTYLE, 0);
    SetWindowLong(_Handle, GWL_STYLE, Flags);

    {$IFDEF TRUE_FULLSCREEN}
    SetWindowPos(_Handle, HWND_TOPMOST, 0, 0, FullWidth, FullHeight, SWP_FRAMECHANGED Or SWP_SHOWWINDOW);
    {$ELSE}
    SetWindowPos(_Handle, 0, 0, 0, FullWidth, FullHeight, SWP_FRAMECHANGED Or SWP_SHOWWINDOW);
    {$ENDIF}
  End Else
  Begin
    {$IFDEF TRUE_FULLSCREEN}
    ChangeDisplaySettings(Nil, 0);
    {$ENDIF}

    // Moving back to windowed mode.
    SetWindowLong(_Handle, GWL_EXSTYLE, _savedExStyle);
    SetWindowLong(_Handle, GWL_STYLE, _savedStyle);

    SetWindowPos(_Handle, HWND_NOTOPMOST, _rcSaved.left, _rcSaved.top, _rcSaved.Right - _rcSaved.Left, _rcSaved.Bottom - _rcSaved.top, SWP_SHOWWINDOW);
  End;
  
  Result := True;
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

Procedure WindowsApplication.OpenAppStore(URL:TERRAString);
Var
  S:PAnsiChar;
Begin
  If (Pos('http',URL)>0) Or (Pos('file',URL)>0) Then
    S := PAnsiChar(URL)
  Else
    S := PAnsiChar('http://itunes.apple.com/us/app/myapp/id'+URL);
  ShellExecute(Application.Instance.Handle, 'open', S,nil,nil, SW_SHOWNORMAL) ;
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

Function WindowsApplication.SetOrientation(Value:Integer):Boolean;
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
  If (Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
    Result := settingsHintLow
  Else
    Result := settingsHintHigh;
End;

Procedure WindowsApplication.SetTitle(Const Name: TERRAString);
Var
  Temp:WideString;
Begin
	Inherited SetTitle(Name);

  Temp := WideString(Name);
  SetWindowTextW(_Handle, PWideChar(Temp));
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
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  Self.SetProcessorAffinity();

  // Initialize xinput gamepads
  For I:=0 To 3 Do
    InputManager.Instance.AddGamePad(XInputGamePad.Create(I));

  // Initialize other joysticks/gamepads
  For I:=0 To 3 Do
    InputManager.Instance.AddGamePad(WindowsGamePad.Create(I));

  Self.InitBuildInfo();

  Renderers.Add(OpenGLRenderer.Create());

  Result := True;
End;

Procedure WindowsApplication.InitIcon;
Const
  iconSize = 64;
Var
  Name:TERRAString;
  Src:Stream;
  offset:Integer;
  Data:PByteArray;
Begin
  Name := GetFileName(ParamStr(0), True)+'.ico';
  Src := FileManager.Instance.OpenStream(Name);
  If (Src = Nil) Or (Not (Src Is MemoryStream)) Then
    Exit;

  // Ahhh, this is the magic API.
  offset := LookupIconIdFromDirectoryEx(MemoryStream(Src).Buffer, True, iconSize, iconSize, LR_DEFAULTCOLOR);

  If (offset <> 0) Then
  Begin
    If (_Icon <> 0) Then
    Begin
      DestroyIcon(_Icon);
    End;

    Data := PByteArray(MemoryStream(Src).Buffer);
    _Icon := CreateIconFromResourceEx(@(Data[Offset]), Src.Size, True, $30000, iconSize, iconSize, LR_DEFAULTCOLOR Or LR_DEFAULTSIZE);

    SendMessage(_Handle, WM_SETICON, ICON_SMALL, _Icon);
    SendMessage(_Handle, WM_SETICON, ICON_BIG, _Icon);
    //This will ensure that the application icon gets changed too.
{    SendMessage(GetWindow(_Handle, GW_OWNER), WM_SETICON, ICON_SMALL, _Icon);
    SendMessage(GetWindow(_Handle, GW_OWNER), WM_SETICON, ICON_BIG, _Icon);}
  End;
End;

Procedure WindowsApplication.OnFatalError(Const ErrorMsg, CrashLog, Callstack: TERRAString);
Var
  S:TERRAString;
Begin
  _Running := False;

  S := 'A fatal error has occurred.' + CrLf + ErrorMsg + CrLf+CrashLog + CrLf+ Callstack;
  Windows.MessageBoxA(0, PAnsiChar(S), PAnsiChar(GetProgramName()), MB_OK Or MB_ICONERROR);
End;

Initialization
  LoadMultimedia();
Finalization
  ReleaseObject(_Application_Instance);
End.
