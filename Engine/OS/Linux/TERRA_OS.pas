// Linux port

Unit TERRA_OS;
{$I terra.inc}

Interface
Uses sysutils, TERRA_Utils, TERRA_Application, unix, baseunix, dateutils, GLX,X,Xlib,Xutil,Keysym;

Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 22;
	keyTab        = 23;
	keyEnter      = 36;
	keyShift      = 50;
	keyControl    = 37;
	keyAlt        = 64;
	keyPause      = 127;
	keyEscape     = 9;
	keySpace      = 65;
	keyPageUp     = 112;
	keyPageDown   = 117;
	keyEnd        = 115;
	keyHome       = 110;

	keyLeft       = 113;
	keyUp         = 111;
	keyRight      = 114;
	keyDown       = 116;

	keyInsert     = 118;
	keyDelete     = 119;

	keyF1         = 67;
	keyF2         = 68;
	keyF3         = 69;
	keyF4         = 70;
	keyF5         = 71;
	keyF6         = 72;
	keyF7         = 73;
	keyF8         = 74;
	keyF9         = 75;
	keyF10        = 76;
	keyF11        = 77;
	keyF12        = 78;

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

Procedure DisplayMessage(S:AnsiString);
Function GetCurrentTime:TERRATime;
Function GetCurrentDate:TERRADate;
Function GetTime:Cardinal;
Function CreateApplicationClass:Application;

Type
  LinuxApplication = Class(Application)
    Protected
			_Display:PDisplay;
			_Screen:Integer;
			_Window:TWindow;
			_Attr:TXSetWindowAttributes;
			_Ctx:GLXContext;

      Function InitWindow:Boolean; Override;
      Function InitGraphics:Boolean; Override;
      Procedure CloseGraphics; Override;
      Procedure CloseWindow; Override;
      Procedure ProcessMessages; Override;

      Function TrySettings(zDepth:Integer; msaaSamples:Integer):PXVisualInfo;

	Procedure ToggleFullscreen; Override;
	Procedure SwapBuffers; Override;
	Procedure SetState(State:Cardinal); Override;

  End;

Implementation
Uses ctypes, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Procedure DisplayMessage(S:AnsiString);
Begin
//  TERRA_Log.Log(logNone,'System',S);
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

Function GetTime:Cardinal;
var
  ts: TTimeSpec;
  i: Int64;
begin
  clock_gettime(CLOCK_MONOTONIC, @ts);
  i := ts.tv_sec;
  i := i*1000 + ts.tv_nsec div 1000000;
  Result := i and $ffffffff; //cut to unsig 32bit

//assembler; asm DW 0310FH end;
  //fpgettimeofday(@t,nil);
//  Result := Cardinal(Trunc(t.seconds * 1000 + t.nanos + 0.5));
//  Result := Cardinal(Trunc((t.tv_sec * 1000) + (t.tv_usec / 1000000) + 0.5));
//   Result := Trunc(Now * 24 * 60 * 60 * 1000);
End;

Function CreateApplicationClass:Application;
Begin
  Result := LinuxApplication.Create();
End;

Const
  GLX_SAMPLE_BUFFERS  = 100000;
  GLX_SAMPLES         = 100001;

Function LinuxApplication.TrySettings(zDepth:Integer; msaaSamples:Integer):PXVisualInfo;
Var
  Attr:Array[0..10] Of Integer;
Begin
  Attr[0] := GLX_RGBA;
  Attr[1] := GLX_DOUBLEBUFFER;
  Attr[2] := GLX_DEPTH_SIZE;
  Attr[3] := zDepth;
  Attr[4] := GLX_STENCIL_SIZE;
  Attr[5] := 8;
  If (msaaSamples>0) Then
  Begin
    Attr[6] := GLX_SAMPLE_BUFFERS;
    Attr[7] := 1;
    Attr[8] := GLX_SAMPLES;
    Attr[9] := msaaSamples;
    Attr[10] := None;
  End Else
    Attr[6] := None;

  Result := glXChooseVisual(_Display, _Screen, @Attr);
End;

Const
  _SC_NPROCESSORS_ONLN = 83;

Function sysconf(i:Integer):CLong; CDecl; External Name 'sysconf';

Function LinuxApplication.InitWindow:Boolean;
Var
  Vi:PXVisualInfo;
  CMap:TColorMap;
  wmDelete:TAtom;

  Root:TWindow;
  Cursor:TCursor;
  CursorMask:TPixmap;
  DummyColor:TXColor;

  Lang:AnsiString;
Begin
  _Display:=XOpenDisplay(0);
  If (Not Assigned(_Display)) Then
  Begin
    RaiseError('CreateWindow: Cannot connect to X server.');
    Result:=False;
    Exit;
  End;

  _Screen:=DefaultScreen(_Display);

  VI := TrySettings(24, 4);
  If Not Assigned(VI) Then
    VI := TrySettings(24, 2);
  If Not Assigned(VI) Then
    VI := TrySettings(24, 0);

  If (Not Assigned(VI)) Then
  Begin
    RaiseError('CreateWindow: glXChooseVisual failed.');
    Result:=False;
    Exit;
  End;

  // create a GLX context
  _Ctx:=glXCreateContext(_Display, Vi, 0, True);

  Root:=RootWindow(_Display, Vi.Screen);

  // create a color map
  CMap:=XCreateColormap(_Display, Root, Vi.visual, AllocNone);
  _Attr.Colormap:=CMap;
  _Attr.border_pixel:=0;

  // create a window in window mode
  _Attr.Event_mask := ExposureMask Or StructureNotifyMask Or PointerMotionMask;
  _Attr.Event_mask := _Attr.Event_mask Or KeyPressMask Or KeyReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ButtonPressMask Or ButtonReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ExposureMask;

  _Window:= XCreateWindow(_Display, Root, 0, 0, _Width, _Height,
                         0, CopyFromParent, InputOutput, Vi.visual,
                         CWBorderPixel Or CWColormap Or CWEventMask,
                         @_Attr);
  // only set window title and handle wm_delete_events if in windowed mode
  wmDelete := XInternAtom(_Display, 'WM_DELETE_WINDOW', True);
  XSetWMProtocols(_Display, _Window, @wmDelete, 1);
  XSetStandardProperties(_Display, _Window, PAnsiChar(Title), PAnsiChar(Title),
                         None, Nil, 0, Nil);
  XMapRaised(_Display, _Window);

  // Hide cursor
  If (Not _IgnoreCursor) Then
  Begin
    CursorMask:=XCreatePixmap(_Display, Root, 1, 1, 1);
    DummyColor.Pixel:=0;
    DummyColor.Red:=0;
    DummyColor.Flags:=4;

    Cursor:=XCreatePixmapCursor(_Display, CursorMask, CursorMask, @DummyColor, @DummyColor, 0, 0);

    XFreePixmap(_Display, Cursormask);
    XDefineCursor(_Display, _Window, Cursor);
  End;

  Log(logDebug,'App', 'Getting user locale...');
  lang := GetEnvironmentVariable('LANG');
  _Language := GetNextWord(Lang, '_');
  _Country := GetNextWord(Lang, '.');

  If (_Country='') Then
    _Country := _Language;

  SetLength(_Language, 2);
  SetLength(_Country, 2);

  _Language := UpStr(_Language);
  _Country := UpStr(_Country);

  Log(logDebug, 'App', 'Country: '+_Country);
  Log(logDebug, 'App', 'Language: '+_Language);



  Log(logDebug,'App', 'Getting cpu core count...');
  _CPUCores := sysconf(_SC_NPROCESSORS_ONLN);
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  _Ready := True;
  Result:=True;
End;

Function LinuxApplication.InitGraphics:Boolean;
Var
  winDummy:TWindow;
  X,Y:Integer;
  borderDummy:Cardinal;
	_Depth:Cardinal;
Begin
	_Depth := 32;

  // connect the glx-context to the window
  glXMakeCurrent(_Display, _Window, _Ctx);
  XGetGeometry(_Display, _Window, @winDummy, @X, @Y,
               @_Width, @_Height, @borderDummy, @_Depth);

  glLoadExtensions;

  Result:=True;
End;

Procedure LinuxApplication.CloseGraphics;
Begin
  If (Not glXMakeCurrent(_Display, None, Nil)) Then
  Begin
    RaiseError('Could not release drawing context.');
  End;

  glXDestroyContext(_Display, _Ctx);
  _Ctx:=Nil;
End;

Procedure LinuxApplication.CloseWindow;
Begin
  XDestroyWindow(_Display, _Window);
End;

Procedure LinuxApplication.ToggleFullscreen;
Begin
End;

Procedure LinuxApplication.SwapBuffers;
Begin
  glXSwapBuffers(_Display, _Window);
End;

Procedure LinuxApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure LinuxApplication.ProcessMessages;
Var
  Event:TXEvent;
  Key:Byte;
  AtomName:PAnsiChar;
  WA:TXWindowAttributes;
Begin
    While (XPending(_Display)> 0) Do
    Begin
      XNextEvent(_Display, @Event);

      Case (Event._type) Of
      ButtonPress:  Case Event.xbutton.button Of
			1:AddValueEvent(eventMouseDown, keyMouseLeft);
			2:AddValueEvent(eventMouseDown, keyMouseMiddle);
			3:AddValueEvent(eventMouseDown, keyMouseRight);
			End;
      ButtonRelease:Case Event.xbutton.button Of
			1:AddValueEvent(eventMouseUp, keyMouseLeft);
			2:AddValueEvent(eventMouseUp, keyMouseMiddle);
			3:AddValueEvent(eventMouseUp, keyMouseRight);
			End;
      KeyPress:   Begin
                    Key := Event.xkey.keycode;
			AddValueEvent(eventKeyDown, Key);
		    	AddValueEvent(eventKeyPress, Key);
                  End;
      KeyRelease: Begin
			Key := Event.xkey.keycode;
			AddValueEvent(eventKeyUp, Key);
                  End;
      MotionNotify: If (Assigned(_Client)) Then
                    Begin
			AddCoordEvent(eventMouseMove, event.xmotion.X, event.xmotion.Y, 0);
                   End;
      Expose:	Begin
			XGetWindowAttributes(_Display, _Window, @WA);
			AddCoordEvent(eventWindowResize, WA.width, WA.height, 0);
	End;

      ClientMessage:  Begin
                        AtomName:=XGetAtomName(_Display, event.xClient.message_type);
                        If (AtomName='WM_PROTOCOLS') Then
                          _Running:=False;
                      End;
      End;
    End;
End;

End.
