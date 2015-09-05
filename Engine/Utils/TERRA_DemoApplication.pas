Unit TERRA_DemoApplication;

{$I terra.inc}

{-$DEFINE USE_OCULUS_RIFT}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_String, TERRA_Application, TERRA_OS,
  TERRA_Vector3D, TERRA_Color, TERRA_Camera, TERRA_Ray, TERRA_UIDimension,
  TERRA_Mesh, TERRA_Texture,
  TERRA_Font, TERRA_FontRenderer, TERRA_Skybox, TERRA_Viewport, TERRA_Lights,
  TERRA_UICursor, TERRA_UIView, TERRA_UITransition, TERRA_ScreenFX,
  TERRA_OGG, TERRA_WAVE,
  TERRA_TTF, TERRA_PNG, TERRA_JPG, TERRA_BMP, TERRA_PSD, TERRA_TGA, TERRA_GIF
  {$IFDEF USE_OCULUS_RIFT},TERRA_OculusRift{$ENDIF};

Type
  DemoApplication = Class({$IFDEF USE_OCULUS_RIFT}OculusApplication{$ELSE}Application{$ENDIF})
    Private
      _Font:TERRAFont;
      _FontRenderer:TERRAFontRenderer;

      _Sky:TERRASkybox;
      _Sun:DirectionalLight;
      _Main:TERRAViewport;
      _Camera:TERRACamera;
      _GUI:UIView;

      _Floor:MeshInstance;

      _ShowFPS:Boolean;

      Function CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;
      Function GetGUI: UIView;

      Function GetFloor:MeshInstance;
      Function GetMainViewport: TERRAViewport;
      Procedure SetShowFPS(const Value: Boolean);

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender2D(V:TERRAViewport); Virtual;
      Procedure OnRender3D(V:TERRAViewport); Virtual;

      Procedure OnMouseDown(Const X,Y:Single; Const Button: Word); Override;
      Procedure OnMouseMove(Const X,Y:Single); Override;
      Procedure OnMouseUp(Const X,Y:Single; Const Button: Word); Override;
			Procedure OnMouseWheel(Const X,Y:Single; Const Delta:Single); Override;

			Procedure OnKeyDown(Key:Word); Override;
			Procedure OnKeyUp(Key:Word); Override;
			Procedure OnKeyPress(Key:TERRAChar); Override;

      Property ShowFPS:Boolean Read _ShowFPS Write SetShowFPS;

      Property Sun:DirectionalLight Read _Sun;
      Property MainViewport:TERRAViewport Read GetMainViewport;

      Property Camera:TERRACamera Read _Camera;
      Property GUI:UIView Read GetGUI;
      Property Floor:MeshInstance Read GetFloor;

      Property Font:TERRAFont Read _Font;
      Property FontRenderer:TERRAFontRenderer Read _FontRenderer;
  End;

Implementation
Uses TERRA_EngineManager, TERRA_FileManager, TERRA_InputManager, TERRA_GraphicsManager;

{ Demo }
Procedure DemoApplication.OnCreate;
Begin
  Inherited;

  Engine.Files.AddFolder('Assets');

  _Font := Engine.Fonts['droid'];
  If Assigned(_Font) Then
  Begin
    _FontRenderer := TERRAFontRenderer.Create();
    _FontRenderer.SetFont(_Font);
  End;

  _Sun := DirectionalLight.Create(Vector3D_Create(-0.25, 0.75, 0.0));
  _Sky := TERRASkybox.Create('sky');

  {$IFDEF USE_OCULUS_RIFT}
  _Camera := OculusCamera.Create('main');
  {$ELSE}
  _Camera := PerspectiveCamera.Create('main');
  {$ENDIF}
  _Camera.SetPosition(Vector3D_Create(0, 5, -20));
  _Camera.SetView(Vector3D_Create(0, -0.25, 0.75));

  Engine.Cursors.SetCursor(cursor_Default, Engine.Textures['cursor_normal'], 0, 0);
  Engine.Cursors.SetCursor(cursor_Busy, Engine.Textures['cursor_busy'], 13, 14);
  Engine.Cursors.SetCursor(cursor_Precision, Engine.Textures['cursor_precision'], 15, 15);
  Engine.Cursors.SetCursor(cursor_Text, Engine.Textures['cursor_text'], 15, 14);
  Engine.Cursors.SetCursor(cursor_Forbidden, Engine.Textures['cursor_forbidden'], 15, 14);
  Engine.Cursors.SetCursor(cursor_ResizeVertical, Engine.Textures['cursor_vertical'], 15, 14);
  Engine.Cursors.SetCursor(cursor_ResizeHorizontal, Engine.Textures['cursor_horizontal'], 15, 15);
  Engine.Cursors.SetCursor(cursor_ResizeDiagonal1, Engine.Textures['cursor_diagonal1'], 15, 14);
  Engine.Cursors.SetCursor(cursor_ResizeDiagonal2, Engine.Textures['cursor_diagonal2'], 15, 15);

  Engine.Cursors.SetCursor(cursor_Move, Engine.Textures['cursor_move'], 15, 15);
  Engine.Cursors.SetCursor(cursor_Rotate, Engine.Textures['cursor_rotate'], 15, 15);
  Engine.Cursors.SetCursor(cursor_Link, Engine.Textures['cursor_link'], 0, 0);

  Engine.Graphics.DeviceViewport.BackgroundColor := ColorCreate(128, 128, 255);

  _ShowFPS := True;
End;

Procedure DemoApplication.OnDestroy;
Begin
  ReleaseObject(_FontRenderer);

  ReleaseObject(_Camera);

  ReleaseObject(_Floor);

  ReleaseObject(_Sun);
  ReleaseObject(_Sky);
End;

Procedure DemoApplication.OnIdle;
Begin
  If Engine.Input.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  Engine.Graphics.TestDebugKeys();

  If (Assigned(_Main)) And (_Main.Visible) Then
    MainViewport.Camera.FreeCam();
End;

Function DemoApplication.GetMainViewport: TERRAViewport;
Begin
  If (_Main = Nil) Then
    _Main := Self.CreateMainViewport('main', Width, Height);

  Result := _Main;
End;

Function DemoApplication.GetGUI: UIView;
Begin
  If (_GUI = Nil) Then
  Begin
    // Create a new UI
    _GUI := UIView.Create('UI', UIPercent(100), UIPercent(100));

    // Register the font with the UI
    _GUI.DefaultFont := Self.Font;

    // Setup the OnRender event
    _GUI.Viewport.OnRender := Self.OnRender2D;
  End;

  Result := _GUI;
End;

Function DemoApplication.GetFloor: MeshInstance;
Var
  Tex:TERRATexture;
Begin
  If (_Floor = Nil) Then
  Begin
    Tex := Engine.Textures.GetItem('woodfloor_diffuse');


    _Floor := MeshInstance.Create(Engine.Meshes.PlaneMesh);
    _Floor.SetDiffuseMap(0, Tex);
    _Floor.SetScale(Vector3D_Constant(40.0));
    _Floor.SetUVScale(0, 4, 4);
  End;

  Result := _Floor;
End;

Procedure DemoApplication.SetShowFPS(const Value: Boolean);
Begin
   _ShowFPS := Value;

   If (Value) Then
    Self.GUI.Viewport.Visible := True;
End;

Function DemoApplication.CreateMainViewport(Const Name:TERRAString; Width, Height:Integer):TERRAViewport;
Begin
  Result := TERRAViewport.Create(Name, _Camera, Width, Height);
  Result.SetTargetArea(0.0, 0.0, 1.0, 1.0);
  Engine.Graphics.AddViewport(Result);
  Result.Visible := True;
  Result.EnableDefaultTargets();
  Result.BackgroundColor := ColorNull;
  Result.OnRender := Self.OnRender3D;
  Result.SetPostProcessingState(True);

  {$IFDEF USE_OCULUS_RIFT}
  Result.VR := True;
  {$ENDIF}
End;

Procedure DemoApplication.OnKeyDown(Key: Word);
Begin
  If Assigned(_GUI) Then
    _GUI.OnKeyDown(Key);
End;

Procedure DemoApplication.OnKeyUp(Key: Word);
Begin
  If Assigned(_GUI) Then
    _GUI.OnKeyUp(Key);
End;

Procedure DemoApplication.OnKeyPress(Key: TERRAChar);
Begin
  If Assigned(_GUI) Then
    _GUI.OnKeyPress(Key);
End;

Procedure DemoApplication.OnMouseDown(Const X,Y:Single; Const Button: Word);
Begin
  If Assigned(_GUI) Then
    _GUI.OnMouseDown(X, Y, Button);
End;

Procedure DemoApplication.OnMouseMove(Const X,Y:Single);
Begin
  If Assigned(_GUI) Then
    _GUI.OnMouseMove(X, Y);
End;

Procedure DemoApplication.OnMouseUp(Const X,Y:Single; Const Button:Word);
Begin
  If Assigned(_GUI) Then
    _GUI.OnMouseUp(X, Y, Button);
End;

Procedure DemoApplication.OnMouseWheel(Const X,Y:Single; Const Delta:Single);
Begin
  If Assigned(_GUI) Then
    _GUI.OnMouseWheel(X, Y, Delta)
End;

Procedure DemoApplication.OnRender2D(V: TERRAViewport);
Begin
  If (Assigned(_FontRenderer)) And (_ShowFPS) Then
  Begin
    _FontRenderer.Reset();
    _FontRenderer.DrawText(V, 5, 25, 90, 'FPS: '+ IntegerProperty.Stringify(Engine.Graphics.Renderer.Stats.FramesPerSecond));
  End;

  Engine.Graphics.AddRenderable(V, _GUI);
End;

Procedure DemoApplication.OnRender3D(V: TERRAViewport);
Var
  R:Ray;
  Dir:Vector3D;
Begin
  R := V.GetPickRay(Trunc(Engine.Input.Mouse.X), Trunc(Engine.Input.Mouse.Y));
  Dir := R.Direction;
  Dir.Normalize();
  //Sun.SetDirection(Dir);

  Engine.Graphics.AddRenderable(V, _Sky);
  Engine.Lights.AddLight(V, Sun);

  Engine.Graphics.AddRenderable(V, _Floor);
End;


End.

