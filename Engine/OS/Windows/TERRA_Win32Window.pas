Unit TERRA_Win32Window;

Interface

Uses TERRA_Utils, TERRA_Object, TERRA_String, TERRA_Window, Windows, Messages;

Type
  Win32Window = Class(TERRAWindow)
    Protected
      _Icon:HICON;

      _savedExStyle:Cardinal;
      _savedStyle:Cardinal;
      _rcSaved:TRect;

      _BorderWidth:Integer;
      _BorderHeight:Integer;

      _OriginalWidth:Integer;
      _OriginalHeight:Integer;

      _FullscreenActive:Boolean;

      _Ready:Boolean;

      _CurrentDX:Integer;
      _CurrentDY:Integer;

      Procedure InitIcon();

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Override;
      Procedure SetTitle(Const Value:TERRAString); Override;
      Procedure SetState(Const State:TERRAWindowState); Override;
      Procedure SetSize(Width, Height:Integer); Override;

    Public
      Constructor Create(Const Title:TERRAString;  Width, Height:Integer; Fullscreen:Boolean);
      Procedure Release(); Override;

      Procedure Update(); Override;

      Procedure ChangeState(State:TERRAWindowState);

  End;

Implementation
Uses TERRA_Log, TERRA_Error, TERRA_Stream, TERRA_MemoryStream, TERRA_FileUtils,
  TERRA_Application, TERRA_OS, TERRA_Multimedia, TERRA_EngineManager, TERRA_InputManager;

//WM_WINDOWPOSCHANGING
Function WndProc(hWnd:HWND;Msg:UINT;wParam:wPARAM;lParam:LPARAM):LRESULT; Stdcall;
Var
  Delta:Integer;
  W,H, I:Integer;
  S:TERRAString;
  It:StringIterator;
  sz:TRECT;
  Temp:Boolean;
  App:WindowsApplication;
  MX, MY:Integer;

Procedure GetMouseCoords(Out X, Y:Integer);
Var
  P:PSmallInt;
Begin
  P := PSmallInt(@lParam);
  X := P^;
  Inc(P);
  Y := P^;
End;

Begin
  Result:=0;

  App := WindowsApplication(Application.Instance);
  If (Not Assigned(App)) Then
  Begin
    Result := DefWindowProcW(hWnd,Msg,wParam,lParam);
    Exit;
  End;

  If (*((Msg=WM_SYSCOMMAND) And (wParam = SC_MAXIMIZE))
  Or *)((Msg = WM_SYSKEYDOWN) And (wParam = keyEnter)) Then
  Begin
    App.Window.FullScreen := True;
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
        If (wParam = mci_Notify_Successful) And (Assigned(Engine.Music.CurrentTrack)) Then
          Engine.Music.CurrentTrack.Play();
      End;

    WM_SYSCOMMAND:
      Begin
        If (wParam = SC_MINIMIZE) Then
        Begin
          Win32Window(App.Window).ChangeState(Window_Minimized);
        End Else
        If (wParam = SC_RESTORE) Then
        Begin
          Win32Window(App.Window).ChangeState(Window_Normal);
        End;

        Result := DefWindowProcW(hWnd,Msg,wParam,lParam);
      End;

    WM_ACTIVATE,WM_ACTIVATEAPP:
      If App.CanReceiveEvents Then
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

        Win32Window(App.Window).ChangeState(Window_Normal);
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
                GetMouseCoords(MX, MY);
                App.AddCoordEvent(eventWindowResize, MX, MY, 0);
              End;

    WM_CLOSE: Begin
                PostQuitMessage(0);
                App.AddValueEvent(eventQuit, 0);
              End;

    WM_KEYDOWN: If (App.CanReceiveEvents) Then
                Begin // Set the pressed key (wparam) to equal true so we can check if its pressed
                  App.AddValueEvent(eventKeyDown, wParam);
                End;

    WM_KEYUP: If (App.CanReceiveEvents) Then
              Begin // Set the released key (wparam) to equal false so we can check if its pressed
                App.AddValueEvent(eventKeyUp, wParam);
              End;

    WM_CHAR:  If (App.CanReceiveEvents) Then
              Begin
                If (wParam=22) And  ($8000 And GetKeyState(VK_CONTROL)<>0) Then
                Begin
                  S := App.GetClipboard();
                  StringCreateIterator(S, It, False);
                  While It.HasNext() Do
                  Begin
                    App.AddValueEvent(eventKeyPress, Cardinal(It.GetNext()));
                  End;
                  
                End Else
                  App.AddValueEvent(eventKeyPress, wParam);
              End;

    WM_LBUTTONDOWN: If (App.CanReceiveEvents) Then
                    Begin
                      SetCapture(App.Window.Handle);
                      App.AddValueEvent(eventMouseDown, keyMouseLeft);
                    End;

    WM_RBUTTONDOWN: If (App.CanReceiveEvents)  Then
                    Begin
                      App.AddValueEvent(eventMouseDown, keyMouseRight);
                    End;

    WM_MBUTTONDOWN: If (App.CanReceiveEvents) Then
                    Begin
                      App.AddValueEvent(eventMouseDown, keyMouseMiddle);
                    End;

    WM_RBUTTONUP: If (App.CanReceiveEvents) Then
                  Begin
                    App.AddValueEvent(eventMouseUp, keyMouseRight);
                  End;

    WM_LBUTTONUP: If (App.CanReceiveEvents) Then
                  Begin
                    ReleaseCapture;
                    App.AddValueEvent(eventMouseUp, keyMouseLeft);
                  End;

    WM_MBUTTONUP: If (App.CanReceiveEvents) Then
                  Begin
                    App.AddValueEvent(eventMouseUp, keyMouseMiddle);
                  End;

    WM_MOUSEMOVE: If (App.CanReceiveEvents) And (Not App.Window.LockedCursor) Then
                  Begin
                    GetMouseCoords(MX, MY);
                    App.AddCoordEvent(eventMouseMove, MX, MY, 0);
                  End;

    (*WM_NCMOUSEMOVE: Begin
                      If (Not App._CursorVisible) And (Not App._IgnoreCursor)  Then
                      Begin
                        App._CursorVisible := True;
                        ShowCursor(True);
                      End;
                    End;*)

    WM_MOUSEWHEEL:If (App.CanReceiveEvents) Then
                  Begin
                    Delta := Integer(wParam Div High(Word));
                    App.AddFloatEvent(eventMouseWheel, Delta / WHEEL_DELTA);
                  End;

    WM_KILLFOCUS: If (App.CanReceiveEvents) Then
                  Begin
                    Engine.Input.Keys.Reset();
                    Win32Window(App.Window).ChangeState(window_Background);
                  End;

    Else
      Begin
        Result := DefWindowProcW(hWnd,Msg,wParam,lParam);  // Default result if nothing happens
      End;
  End;
End;


{ Win32Window }
Constructor Win32Window.Create(Const Title:TERRAString;  Width, Height:Integer; Fullscreen:Boolean);
Var
  I:Integer;
  wndClass:TWndClassW;         // Window class
  dwStyle:Cardinal;            // Window styles
  dwExStyle:Cardinal;          // Extended window styles
  Inst:HINST;             // Current instance
  X,Y,BW,BH:Integer;
  TitleStr:WideString;
Begin
  Inherited Create(Title, Width, Height, Fullscreen);
  
  {$IFDEF FOLDERWATCH}
  If _FolderManager = Nil Then
  Begin
    _FolderManager := WindowsFolderManager.Create();
    _FolderManager.Init();
  End;
  {$ENDIF}

//  FatalErrorHandler := WindowsErrorCallback;

  Inst := GetModuleHandle(Nil);        // Grab an instance for our window

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
    X := (Application.Instance.Screen.Width - Width) Div 2;
    Y := (Application.Instance.Screen.Height - Height) Div 2;

    //BW := GetSystemMetrics(SM_CXFIXEDFRAME)+GetSystemMetrics(SM_CXEDGE)*2;
    BH := GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYSIZEFRAME)*2+GetSystemMetrics(SM_CYEDGE)*2;
    BW := GetSystemMetrics(SM_CYSIZEFRAME)*2+GetSystemMetrics(SM_CYEDGE)*2;
//    BH := GetSystemMetrics(SM_CYSIZEFRAME)*2;
  End;

  _BorderWidth := BW;
  _BorderHeight := BH;

  _OriginalWidth := _Width;
  _OriginalHeight := _Height;

  _Position.X := X;
  _Position.Y := Y;



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
(*  If (_Width> Application.Instance.Screen.Width) Or (_Height> Application.Instance.Screen.Height) Then
    SetWindowPos(_Handle, Cardinal(00), Cardinal(00), 0, _Width, _Height, $400);*)

  If Not _Ready Then
  Begin
    _Ready := True;

    If (_Icon<>0) Then
    Begin
      SendMessage(_Handle, WM_SETICON, ICON_SMALL, _Icon);
      SendMessage(_Handle, WM_SETICON, ICON_BIG, _Icon);
    End Else
      InitIcon();

    If (Application.Instance.IsDebuggerPresent()) Then
      ForceLogFlush := True;
  End;

  UpdateWindow(_Handle);
  ShowWindow(_Handle,SW_SHOW);
  SetForegroundWindow(_Handle);
  SetFocus(_Handle);

  ShowCursor(False);

  ReleaseCapture();
  SetCapture(_Handle);

  If (_FullScreen) Then
  Begin
    _FullScreen := False;
    ToggleFullScreen;
  End;
End;

Procedure Win32Window.ChangeState(State: TERRAWindowState);
Begin
  _State := State;
  Application.Instance.OnStateChange(State);
End;

Procedure Win32Window.InitIcon;
Const
  iconSize = 64;
Var
  Name:TERRAString;
  Src:TERRAStream;
  offset:Integer;
  Data:PByteArray;
Begin
  Name := GetFileName(ParamStr(0), True)+'.ico';
  Src := Engine.Files.OpenFile(Name);
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

Function Win32Window.SetFullscreenMode(UseFullScreen:Boolean):Boolean;
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
    FullWidth := Application.Instance.Screen.Width;
    FullHeight := Application.Instance.Screen.Height;
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


Procedure Win32Window.Release();
Begin
	If (_Fullscreen)	Then
    ToggleFullScreen();

  ShowCursor(True);
  ReleaseCapture();

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

Procedure Win32Window.SetState(const State: TERRAWindowState);
Begin
  _State := State;
  Case State Of
  Window_Normal:    ShowWindow(_Handle,SW_RESTORE);
  Window_Minimized: ShowWindow(_Handle,SW_MINIMIZE);
  //Window_Maximized: ShowWindow(_Handle,SW_MAXIMIZE);
  End;
End;

Procedure Win32Window.SetTitle(Const Value:TERRAString); 
Var
  Temp:WideString;
Begin
	Inherited SetTitle(Name);

  Temp := WideString(Name);
  SetWindowTextW(_Handle, PWideChar(Temp));
End;

Procedure Win32Window.SetSize(Width, Height: Integer);
Begin
  SetWindowPos(_Handle, 0, 0, 0, Width, Height, SWP_FRAMECHANGED Or SWP_SHOWWINDOW);
End;

Procedure Win32Window.Update;
Var
  MousePos:TPoint;
  MX, MY:Integer;
  DX, DY:Integer;
Begin
  Inherited;

  If (Self.LockedCursor) And (Self.State = Window_Normal) Then
  Begin
    GetCursorPos(MousePos);

    MX := Trunc(Self._Position.X + (Self.Width * 0.5));
    MY := Trunc(Self._Position.Y + (Self.Height * 0.5));

    SetCursorPos(MX, MY);

    DX := MX - MousePos.X;
    DY := MY - MousePos.Y;

    If (DX<>_CurrentDX) Or (DY<>_CurrentDY) Then
    Begin
      _CurrentDX := DX;
      _CurrentDY := DY;
      Application.Instance.AddCoordEvent(eventJoystick, _CurrentDX, _CurrentDY, 0);
    End;

  End;
End;

End.
