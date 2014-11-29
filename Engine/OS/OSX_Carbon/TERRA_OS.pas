// mac port
Unit TERRA_OS;
{$I terra.inc}


{$LINKFRAMEWORK Carbon}

Interface
Uses TERRA_Utils, TERRA_Application, {$IFDEF GLDEBUG}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, MacOSAll, AGL;

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

Procedure DisplayMessage(S:String);
Function GetCurrentTime:TERRATime;
Function GetCurrentDate:TERRADate;
Function GetTime:Cardinal;
Function CreateApplicationClass:Application;

Type
  MacApplication = Class(Application)
    Protected
      _Context: TAGLContext;
      _Window:WindowRef;
      _Rect: MacOSAll.Rect;
           
      Function InitWindow:Boolean; Override;
      Function InitGraphics:Boolean; Override;
      Procedure CloseGraphics; Override;
      Procedure CloseWindow; Override;
      Procedure ProcessMessages; Override;
      Procedure SwapBuffers; Override;
      Procedure ToggleFullscreen; Override;
      Procedure SetState(State:Cardinal); Override;
  End;

Implementation
Uses TERRA_Log, TERRA_Unicode, dateutils, sysutils, TERRA_FileUtils;

Type
TRect = Record
 Left, Top, Right,Bottom:Integer;
End;

Procedure DisplayMessage(S:String);
Var
  alert:DialogRef;
  outHit:DialogItemIndex;
  ms, title:CFStringRef;
Begin
  title := CFSTR('TERRA');
  S := S + #0;
  ms := CFStringCreateWithCString(Nil, @(S[1]), 0);
  CreateStandardAlert(kAlertDefaultOKText, title, ms, Nil, alert);
  RunStandardAlert(alert, Nil, outHit);
  ms := Nil;
  title := Nil;
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
	
{
Var
	BaseTime:Single;

Function GetTime:Cardinal;
Begin
   Result := Cardinal(Trunc((Now-BaseTime) * 24 * 60 * 60 * 1000));
End;
}

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

Function GetTime:Cardinal;
Var
	t:Int64;
	f:Single;
Begin
	t := mach_absolute_time() - basetime;
	f := t / timeinfo.denom;
	f := f * timeinfo.numer;
	Result := Trunc(f / 1000000);
End;

Function CreateApplicationClass:Application;
Begin
	Result := MacApplication.Create();
End;

Function Carbon_CloseWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus;  MWPascal;
Var
  App:MacApplication;
Begin
  Result := CallNextEventHandler(AnextHandler, AEvent);

  App := MacApplication(UserData);
  If Not Assigned(App) Then
    Exit;

	App.Terminate;
End;

Function Carbon_ResizeWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus; MWPascal;
Var
  theWindow:WindowRef;
  theBounds:Rect;
  Width:Cardinal;
  Height:Cardinal;
  App:MacApplication;
Begin
  GetEventParameter(AEvent, kEventParamDirectObject, typeWindowRef, Nil, sizeof(WindowRef), Nil, @theWindow);
  GetEventParameter(AEvent, kEventParamCurrentBounds, typeQDRectangle, Nil, sizeof(Rect), Nil, @theBounds);
  Width := theBounds.right - theBounds.left;
  Height := theBounds.bottom - theBounds.top;

  Result := CallNextEventHandler(AnextHandler, AEvent);

  App := MacApplication(UserData);
  If Not Assigned(App) Then
    Exit;

  App.AddCoordEvent(eventWindowResize, Width, Height, 0);
End;


Function CarbonWindow_ShowWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus;  MWPascal;
Var
  EventKind: Cardinal;
  Minimized, Fullscreen:Boolean;
  App:MacApplication;
Begin
  Result := CallNextEventHandler(ANextHandler, AEvent);

	App := MacApplication(UserData);
	If Not Assigned(App) Then
		Exit;

  EventKind := GetEventKind(AEvent);

  Minimized := False;
  Case EventKind of
  kEventWindowCollapsed: Minimized := True;
  kEventWindowExpanded,
  kEventWindowZoomed:
    Begin
      Fullscreen := IsWindowInStandardState(App._Window, Nil, Nil);
      //LCLSendSizeMsg(AWidget.LCLObject, WidgetBounds.Right - WidgetBounds.Left, idgetBounds.Bottom - WidgetBounds.Top, Size_SourceIsInterface or Kind);
    End Else
      TERRA_Log.Log.Instance.Write(logDebug, 'App','CarbonWindow_ShowWindow invalid event kind');
  End;
End;

  // 1 = left, 2 = right, 3 = middle
Function GetCarbonMouseButton(AEvent: EventRef): Integer;
Var
  MouseButton: EventMouseButton;
  Modifiers: Cardinal;
Begin
  Result := 0;
  Modifiers := 0;

  GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(MouseButton), nil, @MouseButton);
  Result := MouseButton;

  GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil, SizeOf(Modifiers), nil, @Modifiers);

  If Result = keyMouseLeft then
  begin
    If (Modifiers and optionKey <> 0) Then
      Result := keyMouseMiddle
    Else
    If (Modifiers and controlKey <> 0) Then
      Result := keyMouseRight;
  End;
End;

Function CarbonWindow_MouseProc(ANextHandler: EventHandlerCallRef; AEvent: EventRef; UserData:Pointer):OSStatus; MWPascal;
Var
  App:MacApplication;
  EventKind: UInt32;
  MouseButton: Integer;
  MousePoint: HIPoint;
  Delta:Integer;
Begin
  Result := EventNotHandledErr;
  App := MacApplication(UserData);
  If Not Assigned(App) Then
  	Exit;

  GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil,  SizeOf(MousePoint), nil, @MousePoint);
  {App.Mouse.X :=  - App._Rect.Left;
  App.Mouse.Y := Trunc(MousePoint.Y) - App._Rect.Top;}

  EventKind := GetEventKind(AEvent);
  Case EventKind of
    kEventMouseDown:
      Begin
        MouseButton := GetCarbonMouseButton(AEvent);
        App.AddCoordEvent(eventMouseDown, Trunc(MousePoint.X), Trunc(MousePoint.Y), MouseButton);
      End;

    kEventMouseUp:
      Begin
        MouseButton := GetCarbonMouseButton(AEvent);
        App.AddCoordEvent(eventMouseUp, Trunc(MousePoint.X), Trunc(MousePoint.Y), MouseButton);
      End;

    kEventMouseMoved,
    kEventMouseDragged:
      Begin
        App.AddCoordEvent(eventMouseMove, Trunc(MousePoint.X), Trunc(MousePoint.Y), 0);
      End;

    kEventMouseWheelMoved:
      Begin
        GetEventParameter(AEvent, kEventParamMouseWheelDelta, typeSInt32, nil, SizeOf(Delta), nil, @Delta);

        App.AddValueEvent(eventMouseWheel, Delta);
      End;
  Else
    Begin
      TERRA_Log.Log.Instance.Write(logError, 'App', 'Invalid mouse event: '+IntToString(EventKind));
      Exit;
    End;
  End;

  Result := CallNextEventHandler(ANextHandler, AEvent);
End;

Var
	PrevKeyModifiers:Cardinal = 0;

Function CarbonWindow_KeyboardProc(ANextHandler: EventHandlerCallRef; AEvent: EventRef;  UserData:Pointer): OSStatus; MWPascal;
Var
  App:MacApplication;
  TempChar:Char;           //Ascii char, when possible (xx_(SYS)CHAR)
  VKKeyCode:Word;         //VK_ code
  IsSysKey: Boolean;        //Is alt (option) key down?
  EventKind: UInt32;        //The kind of this event

  // See what changed in the modifiers flag so that we can emulate a keyup/keydown
  // Note: this function assumes that only a bit of the flag can be modified at
  // once
  Procedure CheckModifiers;
  Var
    CurMod, diff:UInt32;
  Begin
    GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil, SizeOf(CurMod), nil, @CurMod);

    {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Got key modifier: '+IntToString(CurMod));{$ENDIF}

    //see what changed. we only care of bits 8 through 12
    diff := (PrevKeyModifiers xor CurMod) and $1F00;

    //diff is now equal to the mask of the bit that changed, so we can determine
    //if this change is a keydown (PrevKeyModifiers didn't have the bit set) or
    //a keyup (PrevKeyModifiers had the bit set)
    if (PrevKeyModifiers and diff)=0 then
      EventKind := kEventRawKeyDown
    else
      EventKind := kEventRawKeyUp;

    PrevKeyModifiers := CurMod;

    case diff of
      0          : exit;  //nothing (that we cared of) changed
      controlKey : VKKeyCode := keyControl; //command mapped to control
      shiftKey   : VKKeyCode := keyShift;
      ////alphaLock  : VKKeyCode := VK_CAPITAL; //caps lock
      //optionKey  : VKKeyCode := VK_MENU;    //option is alt
      //cmdKey     : VKKeyCode := VK_LWIN;    //meta... map to left Windows Key?
      Else
      Begin
        exit; //Error! More that one bit changed in the modifiers?
      End;
    End;
  End;

  Procedure TranslateMacKeyCode;
  Var
    DeadKeys: UInt32;
    TextLen : UInt32;
    CharLen : integer;
    widebuf: array[1..2] of widechar;
    U: Cardinal;
    Layout: UCKeyboardLayoutPtr;
    KeyboardLayout: KeyboardLayoutRef;
    CurrentKeyModifiers:Cardinal;
  Begin
    IsSysKey := (GetCurrentEventKeyModifiers and cmdKey)>0;

    //non-printable keys (see mackeycodes.inc)
    //for these keys, only send keydown/keyup (not char or UTF8KeyPress)
    GetEventParameter(AEvent, kEventParamKeyCode, typeUInt32, nil, Sizeof(VKKeyCode), nil, @VKKeyCode);

    {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Got keycode: '+IntToString(VKKeyCode));{$ENDIF}

    // get untranslated key (key without modifiers)
    KLGetCurrentKeyboardLayout(KeyboardLayout);
    KLGetKeyboardLayoutProperty(KeyboardLayout, kKLuchrData, Layout);

    CurrentKeyModifiers  := (GetCurrentEventKeyModifiers And (Not cmdkey)) Shr 8;

    TextLen := 0;
    DeadKeys := 0;
    CharLen := 0;

    If Assigned(Layout) Then
    Begin
      UCKeyTranslate(Layout^, VKKeyCode, kUCKeyActionDisplay, CurrentKeyModifiers, LMGetKbdType, kUCKeyTranslateNoDeadKeysMask, DeadKeys, 6, TextLen, @WideBuf[1]);

      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Called UCKeyTranslate: '+IntToString(TextLen));{$ENDIF}

      If TextLen>0 Then
      Begin
        u := UTF16CharacterToUnicode(@WideBuf[1], CharLen);

        {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Got Unicode: '+IntToString(U));{$ENDIF}

        If CharLen>0 Then
        Begin
          VKKeyCode := Word(U);

          If (VKKeyCode>127) Then //not ascii, get the Mac character.
          Begin
            GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil, Sizeof(TempChar), nil, @TempChar);
            VKKeyCode := Ord(TempChar);
          End;

          {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Final key result: '+IntToString(VKKeyCode));{$ENDIF}

          Exit;
        End;
      End;

      TextLen := 0;

      If IsSysKey Then
      begin // workaround for Command modifier suppressing shift
        DeadKeys := 0;
        UCKeyTranslate(Layout^, VKKeyCode, kUCKeyActionDisplay, CurrentKeyModifiers, LMGetKbdType,
            kUCKeyTranslateNoDeadKeysMask, DeadKeys, 6, TextLen, @WideBuf[1]);
      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Called UCKeyTranslate (syskey): '+IntToString(TextLen));{$ENDIF}
      End;

      Exit;
    End Else
    Begin
      // uchr style keyboard layouts not always available - fall back to older style
      KLGetKeyboardLayoutProperty(KeyboardLayout, kKLKCHRData, Layout);
      VKKeyCode := KeyTranslate(Layout, VKKeyCode, DeadKeys) And 255;
      // TODO: workaround for Command modifier suppressing shift?

      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Called KeyTranslate (nolayout): '+IntToString(VkKeyCode));{$ENDIF}
      Exit;
    End;

    //printable keys, for these keys, send char or UTF8KeyPress
    If TextLen = 0 Then
    Begin
      GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil, 6, @TextLen, @WideBuf[1]);
      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Called GetEventParameter: '+IntToString(TextLen));{$ENDIF}


      If TextLen>0 Then
      Begin
        u := UTF16CharacterToUnicode(@WideBuf[1], CharLen);
        If CharLen=0 Then
          Exit;

      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Got Unicode2: '+IntToString(U));{$ENDIF}

        VKKeyCode := Word(U);

        If (VKKeyCode>127) Then  //not ascii, get the Mac character.
        Begin
          GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil, Sizeof(TempChar), nil, @TempChar);
          VKKeyCode := Ord(TempChar);
        End;

        // the VKKeyCode is independent of the modifier
        // => use the VKKeyChar instead of the KeyChar
        If (VKKeyCode>= Ord('a')) And (VKKeyCode<=Ord('z')) Then
          Dec(VKKeyCode, 32);
      End;
    End;
  End;

Begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  App := MacApplication(UserData);
  If Not Assigned(App) Then
  	Exit;

  VKKeyCode := 0;

  EventKind := GetEventKind(AEvent);
  If EventKind = kEventRawKeyModifiersChanged Then
    CheckModifiers()
  Else
    TranslateMacKeyCode();

  If (VKKeyCode=0) Then
    Exit;  

  Case EventKind of
    kEventRawKeyDown,
    kEventRawKeyRepeat:
    Begin
      {$IFDEF DEBUGAPP}Log.Instance.Write(logDebug, 'App', 'Keyevent: '+IntToString(VKKeycode));{$ENDIF}

      App.AddValueEvent(eventKeyPress, VKKeyCode);

      If (VKKeyCode<256) Then
        App.AddValueEvent(eventKeyDown, VKKeyCode);
    End;

    kEventRawKeyUp:
    Begin
      If (VKKeyCode<256) Then
        App.AddValueEvent(eventKeyUp, VKKeyCode);
    End;
  End;
End;

Function GetDocumentsFolder():String;
Const
  kMaxPath = 1024;
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PChar;
begin
  pathBuffer := Allocmem(kMaxPath);
  Try
    Fillchar(pathBuffer^, kMaxPath, #0);
    Fillchar(theRef, Sizeof(theRef), #0);
    //theError := FSFindFolder(kOnAppropriateDisk, kPreferencesFolderType, kCreateFolder, theRef);
    theError := FSFindFolder(kUserDomain, kDocumentsFolderType, kCreateFolder, theRef);
    If (pathBuffer <> nil) and (theError = noErr) then
    Begin
      theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
      If theError = noErr Then
        Result := pathBuffer;
        //Result := UTF8ToAnsi(StrPas(pathBuffer));

      Result := Result + PathSeparator + GetFileName(ParamStr(0), True);
      If Not DirectoryExists(Result) Then
      Begin
        Log.Instance.Write(logDebug,'App', 'Creating dir '+Result);
        CreateDir(Result);
      End;
    End;
  Finally
    Freemem(pathBuffer);
  End;
End;

Function MacApplication.InitWindow:Boolean;
Var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
  KeySpecs: array[0..3] of EventTypeSpec;
  ShowWindowSpecs: array[0..2] of EventTypeSpec;
  WinContent: HIViewRef;
  Attributes: WindowAttributes;
  NewWindowClass: Integer;
  MinSize, MaxSize: HISize;
  GroupClass:Integer;
  RC:MacOSAll.Rect;
  WndRect, ClientRect:MacOSAll.Rect;
  disp: GDHandle;
Begin
  Log.Instance.Write(logDebug,'App', 'Creating window');

  Attributes := kWindowInWindowMenuAttribute Or kWindowStandardFloatingAttributes Or kWindowStandardHandlerAttribute;
  Attributes := Attributes Or kWindowLiveResizeAttribute Or {kWindowHideOnFullScreenAttribute Or kWindowCompositingAttribute Or} kWindowStandardDocumentAttributes;
  GroupClass := kDocumentWindowClass;
  NewWindowClass := kDocumentWindowClass;

  // get current resolution
  disp := GetMainDevice ();
  GetAvailableWindowPositioningBounds(disp, RC);

  RC.left := ((RC.right - RC.left) Shr 1) - (_Width Shr 1);
  RC.top := ((RC.bottom - RC.top) Shr 1) - (_Height Shr 1);

	RC.right := RC.left + _Width;
  RC.bottom := RC.top + _Height;

  _DocumentPath := GetDocumentsFolder();
  _StoragePath := _DocumentPath;
  Log.Instance.Write(logDebug,'App', 'Documents folder is '+_DocumentPath);
  
  Log.Instance.Write(logDebug,'App', 'Calling createwindow()');

  If CreateNewWindow(NewWindowClass, Attributes, RC, _Window)<>noErr Then
  Begin
    RaiseError('Unable to create a window!');
    Exit;
  End;
  
 Log.Instance.Write(logDebug,'App', 'Changing title');

  SetWTitle(_Window, _Title); // Set the windows title
  SetWindowGroup(_Window, GetWindowGroupOfClass(GroupClass));

  // creating wrapped views
 // HIViewFindByID(HIViewGetRoot(_Window), kHIViewWindowContentID, fWinContent);

  //SetWindowProperty(_Window, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  //SetControlProperty(fWinContent, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);

  MinSize.width := 320;
  MinSize.height := 240;
  MaxSize.width := _Width;
  MaxSize.height := _Height;
  SetWindowResizeLimits(_Window, @MinSize, @MaxSize);

Log.Instance.Write(logDebug,'App', 'Installing closewindow event');

  // Window Events
  TmpSpec.eventClass := kEventClassWindow;
  TmpSpec.eventKind := kEventWindowClosed;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(Carbon_CloseWindow), 1, @TmpSpec, Pointer(Self), nil);


Log.Instance.Write(logDebug,'App', 'Installing mouse events');
  MouseSpec[0].eventClass := kEventClassMouse;
  MouseSpec[0].eventKind := kEventMouseDown;
  MouseSpec[1].eventClass := kEventClassMouse;
  MouseSpec[1].eventKind := kEventMouseUp;
  MouseSpec[2].eventClass := kEventClassMouse;
  MouseSpec[2].eventKind := kEventMouseMoved;
  MouseSpec[3].eventClass := kEventClassMouse;
  MouseSpec[3].eventKind := kEventMouseDragged;
  MouseSpec[4].eventClass := kEventClassMouse;
  MouseSpec[4].eventKind := kEventMouseEntered;
  MouseSpec[5].eventClass := kEventClassMouse;
  MouseSpec[5].eventKind := kEventMouseExited;
  MouseSpec[6].eventClass := kEventClassMouse;
  MouseSpec[6].eventKind := kEventMouseWheelMoved;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(CarbonWindow_MouseProc), 7, @MouseSpec[0], Pointer(Self), nil);


Log.Instance.Write(logDebug,'App', 'Installing key events');
  KeySpecs[0].eventClass := kEventClassKeyboard;
  KeySpecs[0].eventKind := kEventRawKeyDown;
  KeySpecs[1].eventClass := kEventClassKeyboard;
  KeySpecs[1].eventKind := kEventRawKeyRepeat;
  KeySpecs[2].eventClass := kEventClassKeyboard;
  KeySpecs[2].eventKind := kEventRawKeyUp;
  KeySpecs[3].eventClass := kEventClassKeyboard;
  KeySpecs[3].eventKind := kEventRawKeyModifiersChanged;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(CarbonWindow_KeyboardProc), 4, @KeySpecs[0], Pointer(Self), nil);


Log.Instance.Write(logDebug,'App', 'Installing window events');
    ShowWindowSpecs[0].eventClass := kEventClassWindow;
  ShowWindowSpecs[0].eventKind := kEventWindowCollapsed;
  ShowWindowSpecs[1].eventClass := kEventClassWindow;
  ShowWindowSpecs[1].eventKind := kEventWindowExpanded;
  ShowWindowSpecs[2].eventClass := kEventClassWindow;
  ShowWindowSpecs[2].eventKind := kEventWindowZoomed;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(CarbonWindow_ShowWindow), 3, @ShowWindowSpecs[0], Pointer(Self), nil);

(*eventType.eventClass = kEventClassWindow;
eventType.eventKind = kEventWindowActivated;

eventType.eventClass = kEventClassWindow;
eventType.eventKind = kEventWindowDeactivated;
  *)
  

  Log.Instance.Write(logDebug,'App', 'Installing resize events');
  
  TmpSpec.eventClass := kEventClassWindow;
  TmpSpec.eventKind := kEventWindowBoundsChanged;
  InstallEventHandler(GetWindowEventTarget(_Window), Carbon_ResizeWindow, 1, @TmpSpec, Pointer(Self), nil);

  GetWindowBounds(_Window, kWindowStructureRgn, WndRect);
  GetWindowBounds(_Window, kWindowContentRgn, ClientRect);

  _Rect.Left := ClientRect.Left - WndRect.Left;
  _Rect.Top := ClientRect.Top - WndRect.Top;
  _Rect.Right := ClientRect.Right - WndRect.Left;
  _Rect.Bottom := ClientRect.Bottom - WndRect.Top;

 Log.Instance.Write(logDebug,'App', 'OK!');
 ShowWindow(_Window);

End;

Function MacApplication.InitGraphics:Boolean;
Var
  displayID:CGDirectDisplayID;
  openGLDisplayMask:CGOpenGLDisplayMask;
  attrib:Array[0..64] Of Integer;
  fmt:TAGLPixelFormat;
  index, Count:Integer;

	Procedure AddAttrib(ID:Integer); Overload;
	Begin
		Attrib[Index] := ID; Inc(Index);
	End;
	Procedure AddAttrib(ID,Value:Integer); Overload;
	Begin
		Attrib[Index] := ID; Inc(Index);
		Attrib[Index] := Value; Inc(Index);
	End;
Begin
//	Log.Instance.Write(logDebug, 'App', 'Init graphics');
// get display ID to use for a mask
	// the main display as configured via System Preferences
  displayID := CGMainDisplayID();
	openGLDisplayMask := CGDisplayIDToOpenGLDisplayMask(displayID);

// Solely as an example of possible use, this pixel format limits
// the possible renderers to those supported by the screen mask.
// In this case the main display.
	Count := 0;
	Repeat
		Index := 0;
		AddAttrib(AGL_RGBA);
		AddAttrib(AGL_DOUBLEBUFFER);
		AddAttrib(AGL_WINDOW);
		AddAttrib(AGL_RED_SIZE, 8);
		AddAttrib(AGL_GREEN_SIZE, 8);
		AddAttrib(AGL_BLUE_SIZE, 8);
		AddAttrib(AGL_ALPHA_SIZE, 8);
		AddAttrib(AGL_DEPTH_SIZE, 32);
		AddAttrib(AGL_ACCELERATED);
		AddAttrib(AGL_CLOSEST_POLICY);
		AddAttrib(AGL_NO_RECOVERY);
		AddAttrib(AGL_DISPLAY_MASK, openGLDisplayMask);
		If (Count=0) Then
		Begin
			AddAttrib(AGL_MULTISAMPLE);
			AddAttrib(AGL_SAMPLE_BUFFERS_ARB, 1);
			AddAttrib(AGL_SAMPLES_ARB, 4);
		End;
		AddAttrib(AGL_NONE);
		
		fmt := aglCreatePixelFormat(attrib); // New to Mac OS X v10.5

		Inc(Count);
	Until (Assigned(Fmt)) Or (Count>=2);

	If (fmt = Nil) Then
  Begin
    RaiseError('aglCreatePixelFormat failed!');
    Exit;
  End;

	// create an AGL context
	_context := aglCreateContext(fmt, Nil);
	If (_context = Nil) Then
	Begin
    RaiseError('Could not create OpenGL context');
    Exit;
  End;

	// pixel format is no longer needed
	aglDestroyPixelFormat(fmt);

	//aglSetDrawable(_context, GetWindowPort(_window));
	aglSetWindowRef(_context, _Window);

	// make the context the current context
	aglSetCurrentContext(_context);

	// VBL SYNC
	//GLint swap = 1;
	//aglSetInteger(m_context, AGL_SWAP_INTERVAL, &swap);

	glLoadExtensions;
	Result := True;	
//	Log.Instance.Write(logDebug, 'App', 'Graphics ok');
End;

Procedure MacApplication.CloseGraphics;
Begin
	If (Assigned(_context)) Then
	Begin
		aglSetWindowRef(_context, 0);

		aglSetCurrentContext(Nil);
		aglDestroyContext(_context);

		_context := Nil;
	End;
End;

Procedure MacApplication.CloseWindow;
Begin
	Log.Instance.Write(logDebug,'App', 'Destroying window');
  If Assigned(_Window) Then
  Begin
//    DisposeWindow(_Window); // crashes here, BUG?
    _Window := Nil;
  End;

	Log.Instance.Write(logDebug,'App', 'Ok');
End;

Procedure MacApplication.SwapBuffers;
Begin
	aglSwapBuffers(_Context);
End;

Procedure MacApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure MacApplication.ToggleFullscreen;
Begin
 Log.Instance.Write(logDebug, 'App', 'Toggling fullscreen');
End;

Procedure MacApplication.ProcessMessages;
var
  Target: EventTargetRef;
  Event: EventRef;
  CurEventClass: Integer;
  CurEventKind: Integer;
Begin
  Target := GetEventDispatcherTarget;

   While (ReceiveNextEvent(0, nil, kEventDurationNoWait, True,  Event) = noErr) Do
   Begin

    CurEventClass := GetEventClass(Event);
    CurEventKind := GetEventKind(Event);

    SendEventToEventTarget(Event, Target);
    ReleaseEvent(Event);
   End;
End;


Const
  BundleResourceFolder = '/Contents/Resources/';

Var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
  pathMedia: String;
Initialization
//	BaseTime := Now;
  mach_timebase_info(timeinfo);
  basetime := mach_absolute_time();
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  pathMedia := pathStr + BundleResourceFolder;
  ChDir(PathMedia);
End.