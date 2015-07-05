{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
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
  TERRA_Widgets,
  TERRA_Color,
  TERRA_Matrix4x4,
  TERRA_ScreenFX,
  TERRA_MeshTopology,
  TERRA_InputManager;

Type
  MyScene = Class(Scene)
      Sky:Skybox;
      Main:Viewport;

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
  Solid:MeshInstance;

  DiffuseTex:Texture;
  GlowTex:Texture;

  Sun:DirectionalLight;

  Topo:MeshTopology;

  Fnt:Font;

{ Game }
Procedure Demo.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Renderer.Settings.PostProcessing.SetValue(True);

  DiffuseTex := TextureManager.Instance.GetTexture('cobble');
  GlowTex := TextureManager.Instance.GetTexture('cobble_glow');

  Topo := MeshTopology.Create(MeshManager.Instance.CylinderMesh);
  Topo.SubDivide3();
  Topo.Smooth();

  Solid := MeshInstance.Create(MeshManager.Instance.CylinderMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetPosition(VectorCreate(0, -30, -80));
  Solid.SetScale(VectorCreate(10.0, 40.0, 10.0));
  Solid.SetWireframeMode(0, True);

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(Topo);
  ReleaseObject(_Scene);
  ReleaseObject(Sun);
  ReleaseObject(Solid);
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();

  _Scene.Main.Camera.FreeCam;
End;

{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');

  Main := GraphicsManager.Instance.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
End;

Procedure MyScene.Release;
Begin
//  ReleaseObject(Main);
  ReleaseObject(Sky);
End;

Procedure MyScene.RenderSprites;
Begin
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(Solid);
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

