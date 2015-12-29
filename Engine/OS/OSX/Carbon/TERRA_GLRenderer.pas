Unit TERRA_GLRenderer;

{$I terra.inc}

{$IFDEF WINDOWS}
{-$DEFINE DEBUG_GL}
{$DEFINE DEBUG_SHADERS}
{$ENDIF}

Interface

Uses MacOSAll,

     TERRA_AGL,
     {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_OpenGL{$ENDIF},
     TERRA_OpenGLCommon,
     TERRA_OpenGL,
     TERRA_GLCustomRenderer;

type
  OpenGLRenderer = Class(CustomOpenGLRenderer)
  Protected
    _Context: TAGLContext;
    function  CreateContext():Boolean; override;
    procedure DestroyContext(); override;
    procedure ChangeVSync(Const Value:Boolean); override;
  public
    procedure EndFrame(); override;
    procedure Resize(Width, Height : Integer); override;
  end;

implementation

uses SysUtils,
     TERRA_Engine,
     TERRA_Log,
     TERRA_Application,
     TERRA_GraphicsManager,
     TERRA_Error,
     TERRA_OS,
     TERRA_Texture,
     TERRA_RendererStats;

{ OpenGLRenderer }
Function OpenGLRenderer.CreateContext : Boolean;
Var
  displayID:CGDirectDisplayID;
  openGLDisplayMask:CGOpenGLDisplayMask;
  attrib:Array[0..64] Of Integer;
  fmt:TAGLPixelFormat;
  index, Samples:Integer;
  Swap:Integer;

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
  Result := _HasContext;
  If _HasContext Then
    Exit;

  LoadOpenGL();

  // get display ID to use for a mask
  // the main display as configured via System Preferences
  displayID := CGMainDisplayID();
  openGLDisplayMask := CGDisplayIDToOpenGLDisplayMask(displayID);

  // Solely as an example of possible use, this pixel format limits
  // the possible GraphicsManagers to those supported by the screen mask.
  // In this case the main display.
  Samples := Application.Instance.GetAntialiasSamples();
  Repeat
    Index := 0;
    AddAttrib(AGL_RGBA);
    AddAttrib(AGL_DOUBLEBUFFER);
    //AddAttrib(AGL_WINDOW);
    AddAttrib(AGL_RED_SIZE, 8);
    AddAttrib(AGL_GREEN_SIZE, 8);
    AddAttrib(AGL_BLUE_SIZE, 8);
    AddAttrib(AGL_ALPHA_SIZE, 8);
    AddAttrib(AGL_DEPTH_SIZE, 24);
    AddAttrib(AGL_STENCIL_SIZE, 8);
    AddAttrib(AGL_ACCELERATED, 1);
    AddAttrib(AGL_NO_RECOVERY, 1);
    //	AddAttrib(AGL_CLOSEST_POLICY);
    AddAttrib(AGL_DISPLAY_MASK, openGLDisplayMask);
  {  If (Samples>0) Then
    Begin
      AddAttrib(AGL_MULTISAMPLE);
      AddAttrib(AGL_SAMPLE_BUFFERS_ARB, 1);
      AddAttrib(AGL_SAMPLES_ARB, Samples);
    End;      }
    AddAttrib(AGL_NONE);

    fmt := aglCreatePixelFormat(attrib); // New to Mac OS X v10.5

    Samples := Samples Div 2;
  Until (Assigned(Fmt)) Or (Samples<=0);

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
  aglSetWindowRef(_context, Application.Instance.Handle);

  // make the context the current context
  aglSetCurrentContext(_context);

  If (Not Application.Instance.FullScreen) Then
     SetWindowBounds(Application.Instance.Handle, kWindowContentRgn, Application.Instance.InitRect);


  glLoadExtensions();

  _HasContext := True;
  Result := True;
End;

Procedure OpenGLRenderer.ChangeVSync(Const Value:Boolean);
Var
  N:Integer;
Begin
  If Value Then
    N := 1
  Else
    N := 0;

  {$IFDEF WINDOWS}
  If Assigned(wglSwapIntervalEXT) Then
      wglSwapIntervalEXT(N);  // Disable VSync
  {$ENDIF}

  {$IFDEF OSX}
  aglSetInteger(_context, AGL_SWAP_INTERVAL, @N);
  {$ENDIF}

  {$IFDEF LINUX}
  If Assigned(glXSwapIntervalEXT) Then
    glXSwapIntervalEXT(N);
  {$ENDIF}
End;

Procedure OpenGLRenderer.DestroyContext();
Begin
  If Not _HasContext Then
    Exit;

  _HasContext := False;

  {$IFDEF WINDOWS}
  // Makes current rendering context not current, and releases the device
  // context that is used by the rendering context.
  If (Not wglMakeCurrent(_hDC,0)) Then
  Begin
    Engine.RaiseError('Release of DC and RC failed.');
  End;

  // Attempts to delete the rendering context
  If (Not wglDeleteContext(_hRC)) Then
  Begin
    Engine.RaiseError('Release of rendering context failed.');
    _hRC:=0;
  End;

  ReleaseDC(Application.Instance.Window.Handle, _HDC);
  {$ENDIF}

  {$IFDEF LINUX}
  If (Not glXMakeCurrent(Application.Instance.Display, None, Nil)) Then
  Begin
    Engine.RaiseError('Could not release drawing context.');
  End;

  glXDestroyContext(Application.Instance.Display, _Ctx);
  _Ctx := Nil;
  {$ENDIF}

  {$IFDEF OSX}
  	If (Assigned(_context)) Then
  	Begin
  		aglSetWindowRef(_context, Nil);

  		aglSetCurrentContext(Nil);
  		aglDestroyContext(_context);

  		_context := Nil;
  	End;

    {$ENDIF}
End;

procedure OpenGLRenderer.EndFrame();
begin
  Inherited;

  {$IFDEF WINDOWS}
	Windows.SwapBuffers(_hDC);
  {$ENDIF}

  {$IFDEF LINUX}
    glXSwapBuffers(Application.Instance.Display, Application.Instance.Window.Handle);
  {$ENDIF}

  {$IFDEF OSX}
  	aglSwapBuffers(_Context);
  {$ENDIF}

  //Application.Instance.SetTitle( IntegerProperty.Stringify(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
end;

procedure OpenGLRenderer.Resize(Width, Height: Integer);
{$IFDEF OSX}
var
  bufferRect:Array[0..3] Of Integer;
{$ENDIF}
begin
  Inherited;

  {$IFDEF OSX}
  If (_Context <> Nil) Then
  Begin
    // Set the AGL buffer rectangle (i.e. the bounds that we will use)
    bufferRect[0] := 0; // 0 = left edge
    bufferRect[1] := 0; // 0 = bottom edge
    bufferRect[2] := Width; // width of buffer rect
    bufferRect[3] := Height; // height of buffer rect

    aglSetInteger(_Context, AGL_BUFFER_RECT, @bufferRect);

    aglEnable(_Context, AGL_BUFFER_RECT);
    aglUpdateContext(_Context);
  end;
  {$ENDIF}
end;

end.
