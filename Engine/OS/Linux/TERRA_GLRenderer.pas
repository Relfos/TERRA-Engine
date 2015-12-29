Unit TERRA_GLRenderer;

{$I terra.inc}

Interface

Uses x,
     xlib,
     GLX,
     Xutil,

     TERRA_XWindow,
     TERRA_OpenGL,
     TERRA_GLCustomRenderer;

type
  OpenGLRenderer = Class(CustomOpenGLRenderer)
  Protected
    _Ctx:GLXContext;
    function  CreateContext():Boolean; override;
    procedure DestroyContext(); override;
    procedure ChangeVSync(Const Value:Boolean); override;
  public
    procedure EndFrame(); override;
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
Const
  GLX_SAMPLE_BUFFERS  = 100000;
  GLX_SAMPLES         = 100001;
Var
  winDummy:TWindow;
  X,Y, Width, Height:Integer;
  borderDummy:Cardinal;
  Depth:Cardinal;
  Vi:PXVisualInfo;
  App:Application;

  Function TrySettings(zDepth:Integer; msaaSamples:Integer):PXVisualInfo;
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

    Result := glXChooseVisual(Application.Instance.Display, LinuxWindow(Application.Instance.Window).ScreenHandle, @Attr);
  End;

Begin
  Result := _HasContext;
  If _HasContext Then
    Exit;

  LoadOpenGL();

  VI := TrySettings(24, 4);
  If Not Assigned(VI) Then
    VI := TrySettings(24, 2);
  If Not Assigned(VI) Then
    VI := TrySettings(24, 0);

  If (Not Assigned(VI)) Then
  Begin
    Engine.RaiseError('CreateWindow: glXChooseVisual failed.');
    Exit;
  End;

  App := Application.Instance;

  // create a GLX context
  _Ctx := glXCreateContext(App.Display, Vi, Nil, True);

  Depth := 32;

  // connect the glx-context to the window
  glXMakeCurrent(App.Display, App.Window.Handle, _Ctx);
  XGetGeometry(App.Display, App.Window.Handle, @winDummy, @X, @Y, @Width, @Height, @borderDummy, @Depth);

  Application.Instance.AddCoordEvent(eventWindowResize, Width, Height, 0);

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

  If Assigned(glXSwapIntervalEXT) Then
    glXSwapIntervalEXT(N);
End;

Procedure OpenGLRenderer.DestroyContext();
Begin
  If Not _HasContext Then
    Exit;

  _HasContext := False;

  If (Not glXMakeCurrent(Application.Instance.Display, None, Nil)) Then
  Begin
    Engine.RaiseError('Could not release drawing context.');
  End;

  glXDestroyContext(Application.Instance.Display, _Ctx);
  _Ctx := Nil;
End;

procedure OpenGLRenderer.EndFrame();
begin
  Inherited;

  glXSwapBuffers(Application.Instance.Display, Application.Instance.Window.Handle);
end;


end.
