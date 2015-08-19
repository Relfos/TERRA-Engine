Unit TERRA_DemoApplication;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_String, TERRA_Application, TERRA_OS, TERRA_Scene,
  TERRA_Vector3D, TERRA_Color, TERRA_Camera, TERRA_Ray, TERRA_UIDimension,
  TERRA_Mesh, TERRA_Texture,
  TERRA_Font, TERRA_FontRenderer, TERRA_Skybox, TERRA_Viewport, TERRA_Lights, TERRA_UIView, TERRA_ScreenFX,
  TERRA_TTF, TERRA_PNG, TERRA_JPG;

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

      _Floor:MeshInstance;

      Function CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;
      Function GetGUI: UIView;

      Function GetFloor:MeshInstance;

    Public
      Constructor Create(Owner:DemoApplication);
      Procedure Release; Override;

      Procedure RenderViewport(V:TERRAViewport); Override;

      Property Sun:DirectionalLight Read _Sun;
      Property MainViewport:TERRAViewport Read _Main;

      Property Camera:TERRACamera Read _Camera;
      Property GUI:UIView Read GetGUI;
      Property Floor:MeshInstance Read GetFloor;
  End;

  DemoApplication = Class(Application)
    Private
      _Scene:DemoScene;
      _Font:TERRAFont;
      _FontRenderer:TERRAFontRenderer;

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender(V:TERRAViewport); Virtual;

      Procedure OnMouseDown(Const X,Y:Single; Const Button: Word); Override;
      Procedure OnMouseMove(Const X,Y:Single); Override;
      Procedure OnMouseUp(Const X,Y:Single; Const Button: Word); Override;
			Procedure OnMouseWheel(Const X,Y:Single; Const Delta:Integer); Override;

			Procedure OnKeyDown(Key:Word); Override;
			Procedure OnKeyUp(Key:Word); Override;
			Procedure OnKeyPress(Key:TERRAChar); Override;


      Property Font:TERRAFont Read _Font;
      Property FontRenderer:TERRAFontRenderer Read _FontRenderer;
      Property Scene:DemoScene Read _Scene;
  End;

Implementation
Uses TERRA_EngineManager, TERRA_FileManager, TERRA_InputManager, TERRA_GraphicsManager;

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
  _Camera.SetPosition(VectorCreate(0, 5, -20));
  _Camera.SetView(VectorCreate(0, -0.25, 0.75));

  _Main := Self.CreateMainViewport('main', Owner.Width, Owner.Height);
  _Main.SetPostProcessingState(True);
//  _Main.FXChain.AddEffect(BloomFX.Create());
  //_Main.Visible := False;

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorCreate(128, 128, 255);
End;

Procedure DemoScene.Release;
Begin
  Inherited;

  ReleaseObject(_Camera);

  ReleaseObject(_Floor);

  ReleaseObject(_Sun);
  ReleaseObject(_Sky);
End;

Function DemoScene.GetGUI: UIView;
Begin
  If (_GUI = Nil) Then
  Begin
    // Create a new UI
    _GUI := UIView.Create('UI', UIPixels(_Owner.Width), UIPixels(_Owner.Height));

    // Register the font with the UI
    _GUI.DefaultFont := Self._Owner.Font;
  End;

  Result := _GUI;
End;

Function DemoScene.GetFloor: MeshInstance;
Var
  Tex:TERRATexture;
Begin
  If (_Floor = Nil) Then
  Begin
    Tex := Engine.Textures.GetItem('woodfloor_diffuse');


    _Floor := MeshInstance.Create(Engine.Meshes.PlaneMesh);
    _Floor.SetDiffuseMap(0, Tex);
    _Floor.SetScale(VectorConstant(40.0));
    _Floor.SetUVScale(0, 4, 4);
  End;

  Result := _Floor;
End;

Function DemoScene.CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;
Begin
  Result := TERRAViewport.Create(Name, _Camera, Width, Height);
  Result.SetTargetArea(0.0, 0.0, 1.0, 1.0);
  GraphicsManager.Instance.AddViewport(Result);
  Result.Visible := True;
  Result.EnableDefaultTargets();
  Result.BackgroundColor := ColorNull;
End;

Procedure DemoApplication.OnKeyDown(Key: Word);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnKeyDown(Key);
End;

Procedure DemoApplication.OnKeyUp(Key: Word);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnKeyUp(Key);
End;

Procedure DemoApplication.OnKeyPress(Key: TERRAChar);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnKeyPress(Key);
End;

Procedure DemoApplication.OnMouseDown(Const X,Y:Single; Const Button: Word);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnMouseDown(X, Y, Button);
End;

Procedure DemoApplication.OnMouseMove(Const X,Y:Single);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnMouseMove(X, Y);
End;

Procedure DemoApplication.OnMouseUp(Const X,Y:Single; Const Button:Word);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnMouseUp(X, Y, Button);
End;

Procedure DemoApplication.OnMouseWheel(Const X,Y:Single; Const Delta:Integer);
Begin
  If Assigned(Scene._GUI) Then
    Scene._GUI.OnMouseWheel(X, Y, Delta)
End;

Procedure DemoScene.RenderViewport(V: TERRAViewport);
Var
  R:Ray;
  Dir:Vector3D;
Begin
  If (V = Self._Main) Then
  Begin
    R := V.GetPickRay(Trunc(InputManager.Instance.Mouse.X), Trunc(InputManager.Instance.Mouse.Y));
    Dir := R.Direction;
    Dir.Normalize();
    //Sun.SetDirection(Dir);

    GraphicsManager.Instance.AddRenderable(V, _Sky);
    LightManager.Instance.AddLight(V, Sun);

    GraphicsManager.Instance.AddRenderable(V, _Floor);
  End Else
  Begin
    _Owner._FontRenderer.DrawText(V, 5, 5, 50, 'FPS: '+ IntegerProperty.Stringify(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
    GraphicsManager.Instance.AddRenderable(V, _GUI);
  End;

  _Owner.OnRender(V);
End;

Procedure DemoApplication.OnRender(V: TERRAViewport);
Begin
//  Sleep(500);
End;





End.

