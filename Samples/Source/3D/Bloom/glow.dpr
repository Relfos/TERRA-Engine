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


  DiffuseTex := TextureManager.Instance.GetTexture('cobble');
  GlowTex := TextureManager.Instance.GetTexture('cobble_glow');

  Solid := MeshInstance.Create(MeshManager.Instance.CubeMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetGlowMap(0, GlowTex);
  Solid.SetPosition(VectorCreate(0, -30, -80));
  Solid.SetScale(VectorConstant(20.0));
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

