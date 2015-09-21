// Linux port
//http://www.cl.cam.ac.uk/~mgk25/ucs/keysym2ucs.c

Unit TERRA_XWindow;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Window, unix, baseunix, dateutils, GLX,X,Xlib,Xutil,Keysym;

Type
  LinuxWindow = Class(TERRAWindow)
    Protected
        _Window:TWindow;
      	_ScreenHandle:Integer;
        _Display:PDisplay;
      	_Attr:TXSetWindowAttributes;


    Public
        Constructor Create(Const Title:TERRAString;  Width, Height:Integer; Fullscreen:Boolean);
        Procedure Release(); Override;

        Property Display:PDisplay Read _Display;
        Property Handle:TWindow Read _Window;
        Property ScreenHandle:Integer Read _ScreenHandle;
  End;


Implementation
Uses sysutils, ctypes, xrandr, xkblib,
  TERRA_Engine, TERRA_InputManager, TERRA_Gamepad, TERRA_Error, TERRA_Log, TERRA_Renderer, TERRA_GLRenderer;

Constructor LinuxWindow.Create(Const Title:TERRAString;  Width, Height:Integer; Fullscreen:Boolean);
Var
//  CMap:TColorMap;
  wmDelete:TAtom;

  Root:TWindow;
  Cursor:TCursor;
  CursorMask:TPixmap;
  DummyColor:TXColor;
Begin
  _Width := Width;
  _Height := Height;
  _Window := 0;

  _Display := XOpenDisplay(Nil);
  If (Not Assigned(_Display)) Then
  Begin
    Engine.RaiseError('CreateWindow: Cannot connect to X server.');
    Exit;
  End;

  _ScreenHandle := DefaultScreen(_Display);

  //Root := RootWindow(_Display, Vi.Screen);
  Root := RootWindow(_Display, _ScreenHandle);

{  // create a color map
  CMap := XCreateColormap(_Display, Root, Vi.visual, AllocNone);
  _Attr.Colormap:=CMap;}
  _Attr.border_pixel := 0;

  // create a window in window mode
  _Attr.Event_mask := ExposureMask Or StructureNotifyMask Or PointerMotionMask;
  _Attr.Event_mask := _Attr.Event_mask Or KeyPressMask Or KeyReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ButtonPressMask Or ButtonReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ExposureMask;

  _Window := XCreateWindow(_Display, Root, 0, 0, _Width, _Height, 0, CopyFromParent, InputOutput, {Vi.visual}Nil, CWBorderPixel {Or CWColormap} Or CWEventMask, @_Attr);
  _Handle := Cardinal(_Window);

  // only set window title and handle wm_delete_events if in windowed mode
  wmDelete := XInternAtom(_Display, 'WM_DELETE_WINDOW', True);
  XSetWMProtocols(_Display, _Window, @wmDelete, 1);
  XSetStandardProperties(_Display, _Window, PAnsiChar(Title), PAnsiChar(Title), None, Nil, 0, Nil);
  _Title := Title;

  XMapRaised(_Display, _Window);

  // Hide cursor
  Begin
    CursorMask := XCreatePixmap(_Display, Root, 1, 1, 1);
    DummyColor.Pixel:=0;
    DummyColor.Red:=0;
    DummyColor.Flags := 4;

    Cursor:=XCreatePixmapCursor(_Display, CursorMask, CursorMask, @DummyColor, @DummyColor, 0, 0);

    XFreePixmap(_Display, Cursormask);
    XDefineCursor(_Display, _Window, Cursor);
  End;
End;

Procedure LinuxWindow.Release();
Begin
  XDestroyWindow(_Display, _Window);
  Inherited;
End;

End.