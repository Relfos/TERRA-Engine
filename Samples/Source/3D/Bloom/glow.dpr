{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
  TERRA_MemoryManager,
  TERRA_DemoApplication,
  TERRA_Object,
  TERRA_Utils,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_Vector3D,
  TERRA_JPG,
  TERRA_PNG,
  TERRA_Texture,
  TERRA_Scene,
  TERRA_Mesh,
  TERRA_ScreenFX,
  TERRA_EngineManager,
  TERRA_InputManager;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
      Procedure OnRender(V:TERRAViewport); Override;
  End;

Var
  Solid:MeshInstance;

  DiffuseTex:TERRATexture;
  GlowTex:TERRATexture;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Renderer.Settings.PostProcessing.SetValue(True);

  Self.Scene.MainViewport.FXChain.AddEffect(GlowFX.Create(2.0));

  DiffuseTex := Engine.Textures.GetItem('cobble');
  GlowTex := Engine.Textures.GetItem('cobble_glow');

  Solid := MeshInstance.Create(Engine.Meshes.CubeMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetGlowMap(0, GlowTex);
  Solid.SetPosition(VectorCreate(0, 4, 0));
  Solid.SetScale(VectorConstant(2.0));

  Self.Scene.Floor.SetPosition(VectorZero);
End;

Procedure MyDemo.OnDestroy;
Begin
  ReleaseObject(Solid);

  Inherited;
End;

Procedure MyDemo.OnRender(V: TERRAViewport);
Begin
  GraphicsManager.Instance.AddRenderable(V, Solid);
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.



