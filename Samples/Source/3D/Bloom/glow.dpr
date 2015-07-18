{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_Object,
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
      Sky:TERRASkybox;
      Main:TERRAViewport;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:TERRAViewport); Override;
      Procedure RenderViewport(V:TERRAViewport); Override;
      Procedure RenderSky(V:TERRAViewport); Override;
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

  DiffuseTex:TERRATexture;
  GlowTex:TERRATexture;

  Sun:DirectionalLight;

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

  Solid := MeshInstance.Create(MeshManager.Instance.CubeMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetGlowMap(0, GlowTex);
  Solid.SetPosition(VectorCreate(0, -30, -80));
  Solid.SetScale(VectorConstant(20.0));

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
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
  Sky := TERRASkybox.Create('sky');

  Main := GraphicsManager.Instance.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  Main.SetPostProcessingState(True);
  Main.FXChain.AddEffect(GlowFX.Create(2.0));
End;

Procedure MyScene.Release;
Begin
//  ReleaseObject(Main);
  ReleaseObject(Sky);
End;

Procedure MyScene.RenderSprites;
Begin
End;

Procedure MyScene.RenderViewport(V:TERRAViewport);
Begin
  LightManager.Instance.AddLight(V, Sun);
  GraphicsManager.Instance.AddRenderable(V, Solid);
End;

Procedure MyScene.RenderSky;
Begin
  Sky.Render(V);
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

