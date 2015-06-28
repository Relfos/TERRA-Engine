// mac port
Unit TERRA_OS;
{$I terra.inc}

{$LINKFRAMEWORK Carbon}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Application, MacOSAll, AGL;

Const
	PathSeparator = '/';
	CrLf = #10;

  keyCommand = 1;
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

  { CarbonApplication }
  CarbonApplication = Class(BaseApplication)
    Protected
      _Window:WindowRef;
      _Rect: MacOSAll.Rect;
      _InitRect: MacOSAll.Rect;
      _ScreenRect:MacOSAll.Rect;
      _Clipboard:PasteboardRef;

      _Display: GDHandle;

      _ToolbarHeight:Integer;

      Function InitSettings:Boolean; Override;
      Function InitWindow:Boolean; Override;
      Procedure CloseWindow; Override;
      Procedure ProcessMessages; Override;

      Function GetClipboardContent():TERRAString;

      Procedure UpdateScreenSize();


      Procedure MoveToBundleFolder(); Virtual;

      Function IsDebuggerPresent():Boolean; Override;
   Public
      Constructor Create();
      
      Procedure SetState(State:Cardinal); Override;

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Override;

      Class Function GetCurrentTime:TERRATime;
      Class Function GetCurrentDate:TERRADate;
      Class Function GetTime:Cardinal;

      Procedure OnFatalError(Const ErrorMsg, CrashLog, Callstack:TERRAString); Override;

      Class Function Instance:CarbonApplication;

      Property Handle:WindowRef Read _Window;

      Property InitRect: MacOSAll.Rect Read _InitRect;
  End;

  Application = CarbonApplication;


Implementation
Uses TERRA_Error, TERRA_Log, TERRA_InputManager, TERRA_FileUtils, TERRA_Renderer, TERRA_GLRenderer,
     BaseUnix, machapi, machexc, dateutils, sysutils, ctypes, sysctl, TERRA_MIDI_IO, TERRA_MIDI;

Var
   kUTTypeUTF16PlainText:CFStringRef;
   kUTTypeUTF8PlainText:CFStringRef;

procedure CreateCFString(const Data: CFDataRef; Encoding: CFStringEncoding; out AString: CFStringRef);
begin
  AString := nil;
  if Data = nil then Exit;
  AString := CFStringCreateWithBytes(nil, CFDataGetBytePtr(Data),
    CFDataGetLength(Data), Encoding, False);
end;

function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = kCFStringEncodingUTF8): TERRAString;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

procedure FreeCFString(var AString: CFStringRef);
begin
  if AString <> nil then
    CFRelease(Pointer(AString));
end;

Var
  _Application_Instance:CarbonApplication;

Constructor CarbonApplication.Create();
Begin
  _Application_Instance := Self;
  Inherited Create();
End;

Class Function CarbonApplication.Instance:CarbonApplication;
Begin
  Result := _Application_Instance;
End;

Class Function CarbonApplication.GetCurrentTime:TERRATime;
Var
 Datetime:Tdatetime;
Begin
  datetime := Now();
  Result.Hour     := hourof( datetime );
  Result.minute   := minuteof( datetime );
  Result.second   := secondof( datetime );
  Result.MiliSecond  := millisecondof( datetime );
End;

Class Function CarbonApplication.GetCurrentDate:TERRADate;
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

Class Function CarbonApplication.GetTime:Cardinal;
Var
	t:UInt64;
	f:Single;
Begin
	t := mach_absolute_time() - basetime;
	f := t / timeinfo.denom;
	f := f * timeinfo.numer;
	Result := Cardinal(Trunc(f / 1000000));
End;

Function Carbon_CloseWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus;  MWPascal;
Var
  App:CarbonApplication;
Begin
  Result := CallNextEventHandler(AnextHandler, AEvent);

  App := CarbonApplication(UserData);
  If Not Assigned(App) Then
    Exit;

  App.Terminate(True);
End;

Const
  CarbonQuitCommand = 'tiuq';  //kHICommandQuit
  CarbonWindowActivate = 'niws';  //kHICommandQuit

Function Carbon_HandleCommand(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus;  MWPascal;
Var
  App:CarbonApplication;
  Cmd:HICommand;
  CmdType:Array[0..3] Of AnsiChar;
Begin
  GetEventParameter(AEvent, kEventParamDirectObject, typeHICommand, Nil, sizeof(HICommand), Nil, @Cmd);

  Result := CallNextEventHandler(AnextHandler, AEvent);

  App := CarbonApplication(UserData);
  If Not Assigned(App) Then
    Exit;

  System.Move(Cmd.commandID, CmdType, 4);

  If (CmdType = CarbonWindowActivate) Then
     Exit;

  If (CmdType = CarbonQuitCommand) Then
     App.Terminate(True);
End;

Function Carbon_QuitEventHandler(var {%H-}AEvent: AppleEvent; var {%H-}Reply: AppleEvent;
  {%H-}Data: SInt32): OSErr; MWPascal;
Var
  App:CarbonApplication;
Begin
  App := CarbonApplication(Application.Instance);
  If Not Assigned(App) Then
     Exit;

  App.Terminate(True);

  Result := NoErr;
End;

Function Carbon_ResizeWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus; MWPascal;
Var
  theWindow:WindowRef;
  theBounds:Rect;
  Width:Cardinal;
  Height:Cardinal;
  App:CarbonApplication;
  ClientRect:MacOSAll.Rect;
Begin
  GetEventParameter(AEvent, kEventParamDirectObject, typeWindowRef, Nil, sizeof(WindowRef), Nil, @theWindow);
  GetEventParameter(AEvent, kEventParamCurrentBounds, typeQDRectangle, Nil, sizeof(Rect), Nil, @theBounds);
  Width := theBounds.right - theBounds.left;
  Height := theBounds.bottom - theBounds.top;

  Result := CallNextEventHandler(AnextHandler, AEvent);

  App := CarbonApplication(UserData);
  If Not Assigned(App) Then
    Exit;

  SizeWindow(App._Window, Width, Height, True);
  GetWindowBounds(App._Window, kWindowContentRgn, ClientRect);

  App.AddCoordEvent(eventWindowResize, clientRect.Right - clientRect.Left, clientRect.Bottom - clientRect.Top, 0);
End;


Function CarbonWindow_ShowWindow(ANextHandler:EventHandlerCallRef; AEvent:EventRef; UserData:Pointer):OSStatus;  MWPascal;
Var
  EventKind: Cardinal;
  Minimized, Fullscreen:Boolean;
  App:CarbonApplication;
Begin
  Result := CallNextEventHandler(ANextHandler, AEvent);

	App := CarbonApplication(UserData);
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
      TERRA_Log.Log(logDebug, 'App','CarbonWindow_ShowWindow invalid event kind');
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

Function noteoff(P:Pointer):Boolean; CDecl;
Begin
  MIDI_Out(MIDIEvent_NoteOff(0, 60, 127));
Result := False;
End;

Function CarbonWindow_MouseProc(ANextHandler: EventHandlerCallRef; AEvent: EventRef; UserData:Pointer):OSStatus; MWPascal;
Var
  App:CarbonApplication;
  EventKind: UInt32;
  MouseButton: Integer;
  MousePoint: HIPoint;
  Delta:Integer;
Begin
  Result := EventNotHandledErr;
  App := CarbonApplication(UserData);
  If Not Assigned(App) Then
  	Exit;

  GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil,  SizeOf(MousePoint), nil, @MousePoint);
  MousePoint.X := MousePoint.X - App._Rect.Left;
  MousePoint.Y := MousePoint.Y - App._Rect.Top;

  EventKind := GetEventKind(AEvent);
  Case EventKind of
    kEventMouseDown:
      Begin
        //MIDI_Out(MIDIEvent_SetInstrument(0, instrumentStringEnsemble));
        //MIDI_Out(MIDIEvent_NoteOn(0, 60, 127));
        //App.ExecuteLater(noteOff, 2000);

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
      TERRA_Log.Log(logError, 'App', 'Invalid mouse event: '+IntToString(EventKind));
      Exit;
    End;
  End;

  Result := CallNextEventHandler(ANextHandler, AEvent);
End;

Var
	PrevKeyModifiers:Cardinal = 0;

Function CarbonWindow_KeyboardProc(ANextHandler: EventHandlerCallRef; AEvent: EventRef;  UserData:Pointer): OSStatus; MWPascal;
Var
  App:CarbonApplication;
  TemPAnsiChar:AnsiChar;           //Ascii char, when possible (xx_(SYS)CHAR)
  VKKeyCode:Word;         //VK_ code
  CharPress:TERRAChar;

  IsSysKey: Boolean;        //Is alt (option) key down?
  EventKind: UInt32;        //The kind of this event

  I:Integer;
  It:StringIterator;
  S:TERRAString;

  // See what changed in the modifiers flag so that we can emulate a keyup/keydown
  // Note: this function assumes that only a bit of the flag can be modified at
  // once
  Procedure CheckModifiers;
  Var
    CurMod, diff:UInt32;
  Begin
    GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil, SizeOf(CurMod), nil, @CurMod);

    {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Got key modifier: '+IntToString(CurMod));{$ENDIF}

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
      cmdKey     : VKKeyCode := keyCommand;
      optionKey  : VKKeyCode := keyAlt;

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
    widebuf: array[1..2] of widechar;
    Layout: UCKeyboardLayoutPtr;
    KeyboardLayout: KeyboardLayoutRef;
    CurrentKeyModifiers:Cardinal;

  Begin
    IsSysKey := (GetCurrentEventKeyModifiers and cmdKey)>0;

    //non-printable keys (see mackeycodes.inc)
    //for these keys, only send keydown/keyup (not char or UTF8KeyPress)
    GetEventParameter(AEvent, kEventParamKeyCode, typeUInt32, nil, Sizeof(VKKeyCode), nil, @VKKeyCode);

    {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Got keycode: '+IntToString(VKKeyCode));{$ENDIF}

    // get untranslated key (key without modifiers)
    KLGetCurrentKeyboardLayout(KeyboardLayout);
    KLGetKeyboardLayoutProperty(KeyboardLayout, kKLuchrData, Layout);

    CurrentKeyModifiers  := (GetCurrentEventKeyModifiers And (Not cmdkey)) Shr 8;

    TextLen := 0;
    DeadKeys := 0;

    If Assigned(Layout) Then
    Begin
      UCKeyTranslate(Layout^, VKKeyCode, kUCKeyActionDisplay, CurrentKeyModifiers, LMGetKbdType, kUCKeyTranslateNoDeadKeysMask, DeadKeys, 6, TextLen, @WideBuf[1]);

      {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Called UCKeyTranslate: '+IntToString(TextLen));{$ENDIF}

      If TextLen>0 Then
      Begin
        CharPress := Word(WideBuf[1]);

        {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Got Unicode: '+CardinalToString(VKKeyCode));{$ENDIF}

        If (CharPress>127) Then //not ascii, get the Mac character.
        Begin
          GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil, Sizeof(TemPAnsiChar), nil, @TemPAnsiChar);
          VKKeyCode := Ord(TemPAnsiChar);
        End Else
        If (CharPress>=Ord('a')) And (CharPress<=Ord('z')) Then
           VKKeyCode := CharPress - 32
        Else
            VKKeyCode := CharPress;

        {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Final key result: '+IntToString(VKKeyCode));{$ENDIF}
        Exit;
      End;

      TextLen := 0;

      If IsSysKey Then
      begin // workaround for Command modifier suppressing shift
        DeadKeys := 0;
        UCKeyTranslate(Layout^, VKKeyCode, kUCKeyActionDisplay, CurrentKeyModifiers, LMGetKbdType,
            kUCKeyTranslateNoDeadKeysMask, DeadKeys, 6, TextLen, @WideBuf[1]);
      {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Called UCKeyTranslate (syskey): '+IntToString(TextLen));{$ENDIF}
      End;

      Exit;
    End Else
    Begin
      // uchr style keyboard layouts not always available - fall back to older style
      KLGetKeyboardLayoutProperty(KeyboardLayout, kKLKCHRData, Layout);
      VKKeyCode := KeyTranslate(Layout, VKKeyCode, DeadKeys) And 255;
      // TODO: workaround for Command modifier suppressing shift?

      {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Called KeyTranslate (nolayout): '+IntToString(VkKeyCode));{$ENDIF}
      Exit;
    End;

    //printable keys, for these keys, send char or UTF8KeyPress
    If TextLen = 0 Then
    Begin
      GetEventParameter(AEvent, kEventParamKeyUnicodes, typeUnicodeText, nil, 6, @TextLen, @WideBuf[1]);
      {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Called GetEventParameter: '+IntToString(TextLen));{$ENDIF}

      If TextLen>0 Then
      Begin
        CharPress := Word(WideBuf[1]);

        {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Got Unicode2: '+IntToString(VKKeyCode));{$ENDIF}

        If (CharPress>127) Then  //not ascii, get the Mac character.
        Begin
          GetEventParameter(AEvent, kEventParamKeyMacCharCodes, typeChar, nil, Sizeof(TemPAnsiChar), nil, @TemPAnsiChar);
          VKKeyCode := Ord(TemPAnsiChar);
        End Else
            VKKeyCode := CharPress;

        // the VKKeyCode is independent of the modifier
        // => use the VKKeyChar instead of the KeyChar
        If (VKKeyCode>= Ord('a')) And (VKKeyCode<=Ord('z')) Then
          Dec(VKKeyCode, 32);
      End;
    End;
  End;

Begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  App := CarbonApplication(UserData);
  If Not Assigned(App) Then
  	Exit;

  VKKeyCode := 0;
  CharPress := 0;

  EventKind := GetEventKind(AEvent);
  If EventKind = kEventRawKeyModifiersChanged Then
    CheckModifiers()
  Else
      TranslateMacKeyCode();

  If (VKKeyCode=0) And (CharPress=0) Then
    Exit;  

  Case EventKind of
    kEventRawKeyDown,
    kEventRawKeyRepeat:
    Begin
      {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'Keyevent: '+IntToString(VKKeycode));{$ENDIF}

      // clipboard paste
      If (CharPress = 118) And (InputManager.Instance.Keys.IsDown(keyCommand)) Then
      Begin
           S := App.GetClipboardContent();
           StringCreateIterator(S, It);
           While It.HasNext() Do
           Begin
               App.AddValueEvent(eventKeyPress,  It.GetNext());
           End;
      End Else
      // full screen
      If (VKKeyCode = keyEnter) And (InputManager.Instance.Keys.IsDown(keyAlt)) Then
      Begin
         App._ChangeToFullScreen := True;
      End Else
      Begin
           If CharPress>0 Then
              App.AddValueEvent(eventKeyPress, CharPress);

           If (VKKeyCode<256) Then
              App.AddValueEvent(eventKeyDown, VKKeyCode);
      End;
    End;

    kEventRawKeyUp:
    Begin
      If (VKKeyCode<256) Then
        App.AddValueEvent(eventKeyUp, VKKeyCode);
    End;
  End;
End;

Function GetDocumentsFolder():TERRAString;
Const
  kMaxPath = 1024;
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PAnsiChar;
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
        Log(logDebug,'App', 'Creating dir '+Result);
        CreateDir(Result);
      End;
    End;
  Finally
    Freemem(pathBuffer);
  End;
End;

Const
  _SC_NPROCESSORS_ONLN = 83;

Function sysconf(i:Integer):Clong; CDecl; External Name 'sysconf';

Const
  BundleResourceFolder = '/Contents/Resources/';
  PListFileName = '../Info.plist';

{
CFStringRef ver = CFBundleGetValueForInfoDictionaryKey(
                      CFBundleGetMainBundle(),
                      kCFBundleVersionKey);
NSString *appVersion = (NSString *)ver;
}

Procedure CarbonApplication.MoveToBundleFolder();
Var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
  pathMedia:TERRAString;
  BundleRef:CFBundleRef;
Begin
  BundleRef := CFBundleGetMainBundle();
  pathRef := CFBundleCopyBundleURL(BundleRef);
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  pathMedia := pathStr + BundleResourceFolder;
  ChDir(PathMedia);

End;

function CarbonApplication.InitSettings: Boolean;
Var
  loc:CFLocaleRef;
  countryCode:CFStringRef;
  langs:CFArrayRef;
  langCode:CFStringRef;

  Temp:Array[0..255] Of AnsiChar;

Begin
  Inherited InitSettings;

  MoveToBundleFolder();

  _DocumentPath := GetDocumentsFolder();
  _StoragePath := _DocumentPath;


    LoggingEnabled := True;
  //  LogFileName := _DocumentPath + LogFileName;

  Log(logDebug,'App', 'Documents folder is '+_DocumentPath);

  {If FileStream.Exists(PListFileName) Then
  Begin
    Log(logDebug,'App', 'Getting plist info...');
    Source := FileStream.Open(PListFileName);
    ReleaseObject(Source);
  End;}

  loc := CFLocaleCopyCurrent();
  Log(logDebug,'App', 'Getting user country...');
  countryCode := CFStringRef(CFLocaleGetValue(loc, kCFLocaleCountryCode));
  CFStringGetPascalString(countryCode, @Temp[0], 255, CFStringGetSystemEncoding());
  _Country := Temp[1] + Temp[2];
  _Country := StringUpper(_Country);
  Log(logDebug, 'App', 'Country: '+_Country);

  Log(logDebug,'App', 'Getting user language');
  langs := CFLocaleCopyPreferredLanguages();
  langCode := CFStringRef(CFArrayGetValueAtIndex (langs, 0));
  CFStringGetPascalString(langCode, @Temp[0], 255, CFStringGetSystemEncoding());
  _Language := Temp[1] + Temp[2];
  _Language := StringUpper(_Language);
  Log(logDebug, 'App', 'Language: '+_Language);

  Log(logDebug,'App', 'Getting cpu core count...');
  _CPUCores := sysconf(_SC_NPROCESSORS_ONLN);
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  // get current resolution
  Log(logDebug,'App', 'Getting screen resolution...');
  _Display := GetMainDevice();
  GetAvailableWindowPositioningBounds(_Display, _ScreenRect);

  _Screen.Width := _ScreenRect.right - _ScreenRect.left;
  _Screen.Height := _ScreenRect.bottom - _ScreenRect.top;

  Renderers.Add(OpenGLRenderer.Create());

  Result := True;
End;

function CarbonApplication.InitWindow: Boolean;
Var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
  KeySpecs: array[0..3] of EventTypeSpec;
  ShowWindowSpecs: array[0..2] of EventTypeSpec;
  WinContent: HIViewRef;
  Attributes: WindowAttributes;
  NewWindowClass: Integer;
  GroupClass:Integer;
  WndRect, ClientRect:MacOSAll.Rect;

  QuitAEHandler: AEEventHandlerUPP;

  kPasteboardClipboard:CFStringRef;
Begin
  Result := False;

  Log(logDebug,'App', 'Creating window');

  Attributes := kWindowInWindowMenuAttribute Or kWindowStandardFloatingAttributes Or kWindowStandardHandlerAttribute;
  Attributes := Attributes Or kWindowResizableAttribute {Or kWindowLiveResizeAttribute};
  Attributes := Attributes Or kWindowStandardDocumentAttributes {Or kWindowHideOnFullScreenAttribute Or kWindowCompositingAttribute Or} ;
  GroupClass := kDocumentWindowClass;
  NewWindowClass := kDocumentWindowClass;

  _Rect.left := _ScreenRect.left + (_Screen.Width - _Width) Shr 1;
  _Rect.top := _ScreenRect.top + (_Screen.Height  - _Height) Shr 1;

  _Rect.right := _Rect.left + _Width;
  _Rect.bottom := _Rect.top + _Height;
  _InitRect := _Rect;

  Log(logDebug,'App', 'Calling createwindow()');

  If CreateNewWindow(NewWindowClass, Attributes, _InitRect, _Window)<>noErr Then
  Begin
    RaiseError('Unable to create a window!');
    Exit;
  End;

 Log(logDebug,'App', 'Changing title');

  SetWTitle(_Window, _Title); // Set the windows title
  SetWindowGroup(_Window, GetWindowGroupOfClass(GroupClass));


  Log(logDebug,'App', 'Installing closewindow event');

  // Window Events
  TmpSpec.eventClass := kEventClassWindow;
  TmpSpec.eventKind := kEventWindowClosed;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(Carbon_CloseWindow), 1, @TmpSpec, Pointer(Self), nil);


  Log(logDebug,'App', 'Installing mouse events');
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


  Log(logDebug,'App', 'Installing key events');
  KeySpecs[0].eventClass := kEventClassKeyboard;
  KeySpecs[0].eventKind := kEventRawKeyDown;
  KeySpecs[1].eventClass := kEventClassKeyboard;
  KeySpecs[1].eventKind := kEventRawKeyRepeat;
  KeySpecs[2].eventClass := kEventClassKeyboard;
  KeySpecs[2].eventKind := kEventRawKeyUp;
  KeySpecs[3].eventClass := kEventClassKeyboard;
  KeySpecs[3].eventKind := kEventRawKeyModifiersChanged;
  InstallEventHandler(GetWindowEventTarget(_Window), NewEventHandlerUPP(CarbonWindow_KeyboardProc), 4, @KeySpecs[0], Pointer(Self), nil);


  Log(logDebug,'App', 'Installing window events');
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
  
  Log(logDebug,'App', 'Installing command events');
  TmpSpec.eventClass := kEventClassCommand;
  TmpSpec.eventKind := kEventCommandProcess;
  InstallEventHandler(GetWindowEventTarget(_Window), Carbon_HandleCommand, 1, @TmpSpec, Pointer(Self), nil);

  QuitAEHandler := NewAEEventHandlerUPP(AEEventHandlerProcPtr(Pointer(@Carbon_QuitEventHandler)));
  AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, QuitAEHandler, 0, False);

  Log(logDebug,'App', 'Installing resize events');
  TmpSpec.eventClass := kEventClassWindow;
  TmpSpec.eventKind := kEventWindowBoundsChanged;
  InstallEventHandler(GetWindowEventTarget(_Window), Carbon_ResizeWindow, 1, @TmpSpec, Pointer(Self), nil);

  GetWindowBounds(_Window, kWindowStructureRgn, WndRect);
  GetWindowBounds(_Window, kWindowContentRgn, ClientRect);

  _Rect.Left := ClientRect.Left - WndRect.Left;
  _Rect.Top := ClientRect.Top - WndRect.Top;
  _Rect.Right := ClientRect.Right - WndRect.Left;
  _Rect.Bottom := ClientRect.Bottom - WndRect.Top;

  _ToolbarHeight := _Rect.Top;

  Log(logDebug,'App', 'Initializing clipboard...');
  kPasteboardClipboard := CFSTR('com.apple.pasteboard.clipboard');
  PasteboardCreate(kPasteboardClipboard, _Clipboard);

  kUTTypeUTF8PlainText := CFSTR('public.utf8-plain-text');
  kUTTypeUTF16PlainText := CFSTR('public.utf16-plain-text');

  Log(logDebug,'App', 'OK!');
  ShowWindow(_Window);

  UpdateScreenSize();

  If (_FullScreen) Then
  Begin
    ToggleFullScreen;
  End;

  Result := True;
End;


procedure CarbonApplication.CloseWindow;
Begin
  If Assigned(_Clipboard) Then
  Begin
    CFRelease(_Clipboard);
    _Clipboard := Nil;
  End;

  Log(logDebug,'App', 'Destroying window');

  If Assigned(_Window) Then
  Begin
    DisposeWindow(_Window);
    _Window := Nil;
  End;

	Log(logDebug,'App', 'Ok');
End;

procedure CarbonApplication.SetState(State: Cardinal);
Begin
  // TODO
End;

Function CarbonApplication.SetFullscreenMode(UseFullScreen: Boolean):Boolean;
Var
    setAttr:Array[0..5] Of Integer;
    clearAttr:Array[0..5] Of Integer;
Begin
  setAttr[0] := kHIWindowBitCloseBox;
  setAttr[1] := kHIWindowBitZoomBox;
  setAttr[2] := 0;
  clearAttr[0] := kHIWindowBitNoTitleBar;
  clearAttr[1] := 0;

  If (UseFullScreen) Then
   Begin
        HideMenuBar();
        UpdateScreenSize();

        _Rect := _ScreenRect;

        HIWindowChangeAttributes(_Window, @clearAttr[0], @setAttr[0]);
  End Else
  Begin
    ShowMenuBar();
    UpdateScreenSize();

    _Rect := _InitRect;

    HIWindowChangeAttributes(_Window, @setAttr[0], @clearAttr[0]);
  End;


 (* _Width := _Rect.Right - _Rect.left;
  _Height := _Rect.Bottom - _Rect.Top;
   Self.AddCoordEvent(eventWindowResize, _Width, _Height, 0);
   *)
   SetWindowBounds(_Window, kWindowContentRgn, _Rect);


  Result := True;
End;

//http://stackoverflow.com/questions/2200277/detecting-debugger-on-mac-os-x
function CarbonApplication.IsDebuggerPresent: Boolean;
Var
   count:mach_msg_type_number_t;
   masks:TException_Mask_array;
   ports:TException_Handler_Array;
   behaviors:Texception_behavior_Array;
   flavors:TException_Flavor_Array;
   mask:exception_mask_t;
   res:kern_return_t;
   portIndex:mach_msg_type_number_t;
Begin
  count := 0;
  mask := EXC_MASK_BREAKPOINT; //EXC_MASK_ALL And (Not (EXC_MASK_RESOURCE Or EXC_MASK_GUARD));
  res := task_get_exception_ports(mach_task_self(), mask, @masks, count, @ports, @behaviors, @flavors);
  if (res = KERN_SUCCESS) Then
  Begin
        For portIndex := 0 To Pred(count) Do
        Begin
            if (ports[portIndex]<>0)  Then //And ((Not ports[portIndex])<>0) Then
            Begin
                Result := True;
                Exit;
            End;
        End;
  End;
  Result := False;
End;


procedure CarbonApplication.ProcessMessages;
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

function CarbonApplication.GetClipboardContent: TERRAString;
Var
   I:Integer;
   Count:LongWord;
   ID: PasteboardItemID;
   Flavors: CFArrayRef;
   FlavorData: CFDataRef;
   UTI, CFString: CFStringRef;
   Encoding: CFStringEncoding;
   S:TERRAString;

   Function HasFormat(Format: CFStringRef): Boolean;
   Var
     FlavorCount: CFIndex;
     J: Integer;
   Begin
     Result := False;
     FlavorCount := CFArrayGetCount(Flavors);
     for J := 0 to Pred(FlavorCount) do
       if UTTypeEqual(Format, CFArrayGetValueAtIndex(Flavors, J)) then
       begin
         Result := True;
         Break;
       end;
   End;

Begin
     Result := '';
     PasteboardSynchronize(_Clipboard);
     PasteboardGetItemCount(_Clipboard, Count{%H-});
     If (Count<1 ) Then
        Exit;

     For I:=Count DownTo 1 Do
     Begin
          PasteboardGetItemIdentifier(_Clipboard, I, ID{%H-});
          PasteboardCopyItemFlavors(_Clipboard, ID, Flavors{%H-});

          If HasFormat(kUTTypeUTF8PlainText) then
          Begin
             UTI := kUTTypeUTF8PlainText;   // check UTF-8 text
             Encoding := kCFStringEncodingUTF8;
          End Else
          If HasFormat(kUTTypeUTF16PlainText) then
          Begin
             UTI := kUTTypeUTF16PlainText; // check UTF-16 text
             Encoding := kCFStringEncodingUTF16;
          End Else
            Exit;

          // plain Encoding := CFStringGetSystemEncoding;

          PasteboardCopyItemFlavorData(_Clipboard, ID, UTI, FlavorData{%H-});

          If CFDataGetLength(FlavorData) = 0 Then
             Exit;

          CreateCFString(FlavorData, Encoding, CFString);
          S := CFStringtoStr(CFString);
          Result := S;
          FreeCFString(CFString);
          CFRelease(FlavorData);
          Exit;
     End;
End;

Procedure CarbonApplication.UpdateScreenSize;
Var
   MinSize, MaxSize: HISize;
begin
  //HIWindowGetAvailablePositioningBounds(kCGNullDirectDisplay,kHICoordSpace72DPIGlobal, _ScreenRect);
  GetAvailableWindowPositioningBounds(_Display, _ScreenRect);

  MinSize.width := 320;
  MinSize.height := 240;
  MaxSize.width := _ScreenRect.Right - _ScreenRect.Left;
  MaxSize.height := _ScreenRect.Bottom - _ScreenRect.Top;
  SetWindowResizeLimits(_Window, @MinSize, @MaxSize);
end;

Procedure CarbonApplication.OnFatalError(const ErrorMsg, CrashLog, Callstack: TERRAString);
Var
  S:TERRAString;
  alert:DialogRef;
  outHit:DialogItemIndex;
  ms, title:CFStringRef;
Begin
  S := 'A fatal error has occurred.' + CrLf + ErrorMsg + CrLf+CrashLog + CrLf+ Callstack + #0;

  ms := CFStringCreateWithCString(Nil, @(S[1]), 0);

  title := CFSTR(PAnsiChar(GetProgramName()));

  CreateStandardAlert(kAlertDefaultOKText, title, ms, Nil, alert);
  RunStandardAlert(alert, Nil, outHit);
  ms := Nil;
  title := Nil;

  _Running := False;
End;

Initialization
//	BaseTime := Now;
  mach_timebase_info(timeinfo);
  basetime := mach_absolute_time();
Finalization
  ReleaseObject(_Application_Instance);
End.
