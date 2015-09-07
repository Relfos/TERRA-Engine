{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
  TERRA_MemoryManager,
  TERRA_DemoApplication,
  TERRA_OS,
  TERRA_Object,
  TERRA_Utils,
  TERRA_Viewport,
  TERRA_Vector3D,
  TERRA_Texture,
  TERRA_ScreenFX,
  TERRA_Mesh,
  TERRA_Engine,
  TERRA_InputManager;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
      Procedure OnIdle; Override;
      Procedure OnRender3D(V:TERRAViewport); Override;
  End;

Var
  Solid:MeshInstance;

  DiffuseTex:TERRATexture;
  GlowTex:TERRATexture;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  Self.MainViewport.Visible := True;
  Self.MainViewport.VR := True;

  DiffuseTex := Engine.Textures.GetItem('cobble');

  Solid := MeshInstance.Create(Engine.Meshes.CubeMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetGlowMap(0, GlowTex);
  Solid.SetPosition(Vector3D_Create(0, 4, 0));
  Solid.SetScale(Vector3D_Constant(2.0));

  Self.Floor.SetPosition(Vector3D_Zero);
End;

Procedure MyDemo.OnDestroy;
Begin
  ReleaseObject(Solid);

  Inherited;
End;

procedure MyDemo.OnIdle;
begin
  inherited;

  If Engine.Input.Keys.WasPressed(keyEnter) Then
    Engine.Graphics.Renderer.GetScreenshot().Save('screenshot.jpg');
end;

Procedure MyDemo.OnRender3D(V: TERRAViewport);
Begin
  Engine.Graphics.AddRenderable(V, Solid);

  Inherited;
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



