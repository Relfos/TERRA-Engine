Unit TERRA_GLRenderer;

{$I terra.inc}

{$IFDEF WINDOWS}
{-$DEFINE DEBUG_GL}
{$DEFINE DEBUG_SHADERS}
{$ENDIF}

Interface

Uses Windows,
     TERRA_GLCustomRenderer;

type
  OpenGLRenderer = Class(CustomOpenGLRenderer)
  Protected
    _HDC           : HDC;           // HDC of window
    _hRC           : HGLRC;         // OpenGL rendering context
    _PixelFormat   : Cardinal;

    function  CreateContext():Boolean; override;
    procedure ChangeVSync(Const Value:Boolean); override;
    procedure DestroyContext(); override;
  public
    procedure EndFrame(); override;

    property HDC : HDC Read _HDC;
  end;


implementation

uses TERRA_Object,
     TERRA_Engine,
     TERRA_Log,
     TERRA_Error,
     {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_OpenGL{$ENDIF},
     TERRA_OS;

{ OpenGLRenderer }
Function OpenGLRenderer.CreateContext : Boolean;
Var
  Pfd:PixelFormatDescriptor; // Settings for the OpenGL window
Begin
  inherited CreateContext();

  Result := _HasContext;
  If _HasContext Then
    Exit;

  LoadOpenGL();

  _HDC := GetDC(Application.Instance.Window.Handle);
  If _HDC=0 Then
  Begin
    Engine.RaiseError('Unable to retrieve a device context.');
    Halt;
  End;

 // Settings for the OpenGL window
  FillChar(Pfd,SizeOf(Pfd),0);
  With Pfd Do
  Begin
    nSize := SizeOf(PixelFormatDescriptor); // Size Of This Pixel Format Descriptor
    nVersion := 1;                          // The version of this data structure
    dwFlags := PFD_DRAW_TO_WINDOW Or PFD_SUPPORT_OPENGL Or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;            // RGBA color format
    cColorBits := 24;                       // OpenGL color depth
    cAlphaBits := 8;                        //
    cDepthBits := 24;                       // Specifies the depth of the depth buffer
    cStencilBits := 8;                      // Specificies the depth of the stencil buffer
    iLayerType := PFD_MAIN_PLANE;
  End;

  // Attempts to find the pixel format supported by a device context that is the best match to a given pixel format specification.
  {If (_MultiSampleInitialized) And (_MultisampleFormat<>0) Then
    _PixelFormat := _MultisampleFormat
  Else}
    _PixelFormat := ChoosePixelFormat(_hDC, @Pfd);

  If (_PixelFormat=0) Then
  Begin
    Engine.RaiseError('Unable to find a suitable pixel format.');
    Exit;
  End;

  // Sets the specified device context's pixel format to the format specified by the PixelFormat.
  If (Not SetPixelFormat(_HDC, _PixelFormat, @Pfd)) then
  Begin
    Engine.RaiseError('Unable to set the pixel format.');
    Exit;
  End;

  // Create a OpenGL rendering context
  _hRC := wglCreateContext(_hDC);
  If (_hRC = 0) Then
  Begin
    Engine.RaiseError('Unable to create an OpenGL rendering context.');
    Exit;
  End;

  // Makes the specified OpenGL rendering context the calling thread's current rendering context
  If (Not wglMakeCurrent(_hDC,_hRC))Then
  Begin
    Engine.RaiseError('Unable to activate OpenGL rendering context.');
    Exit;
  End;

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

  If Assigned(wglSwapIntervalEXT) Then
      wglSwapIntervalEXT(N);  // Disable VSync
End;

Procedure OpenGLRenderer.DestroyContext();
Begin
  If Not _HasContext Then
    Exit;

  _HasContext := False;

  // Makes current rendering context not current, and releases the device
  // context that is used by the rendering context.
  If (Not wglMakeCurrent(_hDC,0)) Then Begin
    Engine.RaiseError('Release of DC and RC failed.');
  End;

  // Attempts to delete the rendering context
  If (Not wglDeleteContext(_hRC)) Then Begin
    Engine.RaiseError('Release of rendering context failed.');
    _hRC:=0;
  End;

  ReleaseDC(Application.Instance.Window.Handle, _HDC);

  inherited DestroyContext();
End;

procedure OpenGLRenderer.EndFrame();
begin
  Inherited;

  Windows.SwapBuffers(_hDC);
end;

end.
