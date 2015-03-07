{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_RenderTarget, TERRA_Solids, TERRA_Texture,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4,
  TERRA_ScreenFX;

Type
  MyScene = Class(Scene)
      Sky:Skybox;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

  Game = Class(AppClient)
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

  Fnt:Font;

{ Game }
Procedure Game.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Settings.DynamicShadows.SetValue(False);
  GraphicsManager.Instance.Settings.PostProcessing.SetValue(False);
  GraphicsManager.Instance.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Settings.DepthOfField.SetValue(False);
  GraphicsManager.Instance.Settings.PostProcessing.SetValue(True);

  GraphicsManager.Instance.ActiveViewport.SetRenderTargetState(captureTargetEmission, True);
  GraphicsManager.Instance.ActiveViewport.FXChain.AddEffect(BloomFX.Create);

  DiffuseTex := TextureManager.Instance.GetTexture('cobble');
  GlowTex := TextureManager.Instance.GetTexture('cobble_glow');

  Solid := MeshInstance.Create(MeshManager.Instance.CubeMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetGlowMap(0, GlowTex);
  Solid.SetPosition(VectorCreate(0, -30, -80));
  Solid.SetScale(VectorUniform(20.0));

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Game.OnDestroy;
Begin
  _Scene.Release;

  Sun.Release();
  Solid.Release;
End;

Procedure Game.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

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
  ApplicationStart(Game.Create);
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

