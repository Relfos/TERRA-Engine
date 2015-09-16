{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_Utils,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector3D,
  TERRA_Font,
  TERRA_UI,               
  TERRA_Lights,           
  TERRA_Viewport,
  TERRA_JPG,
  TERRA_PNG,
  TERRA_Texture,
  TERRA_Renderer,
  TERRA_FileManager,
  TERRA_Scene,
  TERRA_Mesh,
  TERRA_Skybox,
  TERRA_Color,
  TERRA_Matrix4x4,
  TERRA_ScreenFX,
  TERRA_InputManager;

Type
  MyScene = Class(Scene)
      Sky:Skybox;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

  Demo = Class(Application)
    Protected
      _Scene:MyScene;

    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;


Var
  MyInstance:MeshInstance;

  Sun:DirectionalLight;

  Fnt:Font;

{ Game }
Procedure Demo.OnCreate;
Var
  MyMesh:Mesh;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Renderer.Settings.PostProcessing.SetValue(True);

  GraphicsManager.Instance.ActiveViewport.FXChain.AddEffect(OutlineFX.Create);

  MyMesh := MeshManager.Instance.GetMesh('jeep');
  MyInstance := MeshInstance.Create(MyMesh);

  MyInstance.SetOutlineColor(ColorBlue);

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Scene);

  ReleaseObject(Sun);
  ReleaseObject(MyInstance);
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam;
End;


{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');
End;

Procedure MyScene.Release;
Begin
  Sky.Release;
End;

Procedure MyScene.RenderSprites;
Begin
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(MyInstance);
End;

Procedure MyScene.RenderSky;
Begin
  Sky.Render;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  Demo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

