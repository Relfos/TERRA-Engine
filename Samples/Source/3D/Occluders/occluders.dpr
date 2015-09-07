{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Object, TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_Viewport, TERRA_Texture, TERRA_Renderer,
  TERRA_JPG, TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory, TERRA_InputManager,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4,
  TERRA_DemoApplication, TERRA_Engine, TERRA_Occluder;

Type
  Demo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
      Procedure OnDestroy; Override;
			Procedure OnRender(V:TERRAViewport); Override;
  End;

Var
  Sphere:MeshInstance;
  P:TERRAOccluder;

{ Game }
Procedure Demo.OnCreate;
Var
  DiffuseTex:TERRATexture;
Begin
  Inherited;
  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Renderer.Settings.Specular.SetValue(True);

  DiffuseTex := Engine.Textures.GetTexture('cobble');

  Sphere := MeshInstance.Create(MeshManager.Instance.SphereMesh);
  Sphere.SetDiffuseMap(0, DiffuseTex);
  Sphere.SetScale(VectorConstant(20.0));
  Sphere.SetPosition(VectorCreate(0, -30, -80));

  Sphere.SetUVScale(0, 2.0, 2.0);

  Sphere.SetNormalMap(0, Engine.Textures.GetTexture('cobble_normal'));
  Sphere.SetSpecularMap(0, Engine.Textures.GetTexture('cobble_specular'));
  Sphere.SetDisplacementMap(0, Engine.Textures.GetTexture('cobble_disp'));


  P := TERRAOccluder.Create();
  P.SetTransform(Matrix4x4Translation(0, 0, 0), 80, 80);
End;

Procedure Demo.OnDestroy;
Begin
  Inherited;
  ReleaseObject(Sphere);
  ReleaseObject(P);
End;

Procedure Demo.OnRender(V:TERRAViewport);
Begin
  GraphicsManager.Instance.AddRenderable(V, P);
//  GraphicsManager.Instance.AddOccluder(V, P);

  GraphicsManager.Instance.AddRenderable(V, Sphere);

  Self.FontRenderer.DrawText(V, 5, 10, 5, 'Objects visible: '+IntToString(GraphicsManager.Instance.Renderer.Stats.RenderableCount));
  Self.FontRenderer.DrawText(V, 5, 25, 5, 'Occluders visible: '+IntToString(GraphicsManager.Instance.Renderer.Stats.OccluderCount));
  Self.FontRenderer.DrawText(V, 5, 40, 5, 'Draw calls: '+IntToString(GraphicsManager.Instance.Renderer.Stats.DrawCalls));
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

