Unit TERRA_DemoApplication;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_String, TERRA_Application, TERRA_OS, TERRA_Scene,
  TERRA_Vector3D, TERRA_Color, TERRA_Camera,
  TERRA_Font, TERRA_FontRenderer, TERRA_Skybox, TERRA_Viewport, TERRA_Lights, TERRA_UIView;

Type
  DemoApplication = Class;

  DemoScene = Class(TERRAScene)
    Private
      _Owner:DemoApplication;
      _Sky:TERRASkybox;
      _Sun:DirectionalLight;
      _Main:TERRAViewport;
      _Camera:TERRACamera;
      _GUI:UIView;

      Function CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;

    Public
      Constructor Create(Owner:DemoApplication);
      Procedure Release; Override;

      Procedure RenderSprites(V:TERRAViewport); Override;
      Procedure RenderViewport(V:TERRAViewport); Override;

      Property Sun:DirectionalLight Read _Sun;
      Property MainViewport:TERRAViewport Read _Main;

      Property Camera:TERRACamera Read _Camera;
      Property GUI:UIView Read _GUI;
  End;

  DemoApplication = Class(Application)
    Protected
      _Scene:DemoScene;
      _Font:TERRAFont;
      _FontRenderer:TERRAFontRenderer;

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender(V:TERRAViewport); Virtual;

      Procedure OnMouseDown(X, Y: Integer; Button: Word); Override;
      Procedure OnMouseMove(X, Y: Integer); Override;
      Procedure OnMouseUp(X, Y: Integer; Button: Word); Override;

      Property Font:TERRAFont Read _Font;
      Property Scene:DemoScene Read _Scene;
  End;

Implementation
Uses TERRA_FileManager, TERRA_InputManager, TERRA_GraphicsManager;

{ Demo }
Procedure DemoApplication.OnCreate;
Begin
  Inherited;

  FileManager.Instance.AddPath('Assets');

  _Font := FontManager.Instance.DefaultFont;
  _FontRenderer := TERRAFontRenderer.Create();
  _FontRenderer.SetFont(_Font);

  _Scene := DemoScene.Create(Self);
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure DemoApplication.OnDestroy;
Begin
  ReleaseObject(_Scene);
  ReleaseObject(_FontRenderer);
End;

Procedure DemoApplication.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();

  If (Assigned(_Scene.MainViewport)) And (_Scene.MainViewport.Visible) Then
    _Scene.MainViewport.Camera.FreeCam();
End;

{ DemoScene }
Constructor DemoScene.Create(Owner:DemoApplication);
Begin
  Inherited Create();

  Self._Owner := Owner;
  _Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));
  _Sky := TERRASkybox.Create('sky');

  _Camera := PerspectiveCamera.Create('main');

  _Main := Self.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  _Main.SetPostProcessingState(True);

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorCreate(128, 128, 255);

  // Create a new UI
  _GUI := UIView.Create;

  // Register the font with the UI
  _GUI.DefaultFont := Self._Owner.Font;
End;

Procedure DemoScene.Release;
Begin
  Inherited;

  ReleaseObject(_Camera);

  ReleaseObject(_Sun);
  ReleaseObject(_Sky);
End;

Function DemoScene.CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;
Begin
  Result := TERRAViewport.Create(Name, _Camera, Width, Height);
  Result.SetTarget(GraphicsManager.Instance.DeviceViewport, 0.0, 0.0, 1.0, 1.0);
  GraphicsManager.Instance.AddViewport(Result);
  Result.Visible := True;
  Result.EnableDefaultTargets();
  Result.BackgroundColor := ColorNull;
End;

Procedure DemoScene.RenderSprites(V: TERRAViewport);
Begin
  _Owner._FontRenderer.DrawText(V, 5, 5, 50, 'FPS: '+IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
End;

Procedure DemoApplication.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  Scene.GUI.OnMouseDown(X, Y, Button);
End;

Procedure DemoApplication.OnMouseMove(X, Y: Integer);
Begin
  Scene.GUI.OnMouseMove(X, Y);
End;

Procedure DemoApplication.OnMouseUp(X, Y: Integer; Button: Word);
Begin
  Scene.GUI.OnMouseUp(X, Y, Button);
End;

Procedure DemoScene.RenderViewport(V: TERRAViewport);
Begin
  If (V = Self._Main) Then
  Begin
    GraphicsManager.Instance.AddRenderable(V, _Sky);
    LightManager.Instance.AddLight(V, Sun);
  End Else
  Begin
    GraphicsManager.Instance.AddRenderable(V, _GUI);
  End;

  _Owner.OnRender(V);
End;

Procedure DemoApplication.OnRender(V: TERRAViewport);
Begin
End;


End.

