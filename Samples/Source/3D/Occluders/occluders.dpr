{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Viewport, TERRA_Texture,
  TERRA_JPG, TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4;

Type
  Game = Class(AppClient)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

  MyScene = Class(Scene)
      Sky:Skybox;

      Constructor Create;
      Destructor Destroy; Reintroduce;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

Var
  Sphere:MeshInstance;
  P:Occluder;
  Fnt:Font;
  Sun:DirectionalLight;

{ Game }
Procedure Game.OnCreate;
Var
  DiffuseTex:Texture;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Settings.DynamicShadows.SetValue(False);
  GraphicsManager.Instance.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Settings.DepthOfField.SetValue(False);
//  GraphicsManager.Instance.Settings.ColorCorrection.SetValue(False);
//  GraphicsManager.Instance.Settings.Bloom.SetValue(False);

  DiffuseTex := TextureManager.Instance.GetTexture('cobble');

  Sphere := MeshInstance.Create(MeshManager.Instance.SphereMesh);
  Sphere.SetDiffuseMap(0, DiffuseTex);
  Sphere.SetScale(VectorUniform(20.0));
  Sphere.SetPosition(VectorCreate(0, -30, -80));

  P := Occluder.Create;
  P.SetTransform(Matrix4x4Translation(0, 0, 0), 50, 50);

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  GraphicsManager.Instance.Scene := MyScene.Create;
End;

Procedure Game.OnDestroy;
Begin
  Sphere.Destroy;
  P.Destroy;
  Sun.Destroy;
End;

Procedure Game.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam;
End;


{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');
End;

Destructor MyScene.Destroy;
begin
  Sky.Destroy;
end;

Procedure MyScene.RenderSprites;
Begin
  If Not Assigned(Fnt) Then
    Exit;

  Fnt.DrawText(5, 5, 5, 'Objects visible: '+IntToString(GraphicsManager.Instance.Stats.RenderableCount), ColorWhite);
  Fnt.DrawText(5, 15, 5, 'Occluders visible: '+IntToString(GraphicsManager.Instance.Stats.OccluderCount), ColorWhite);
  Fnt.DrawText(5, 25, 5, 'Draw calls: '+IntToString(GraphicsManager.Instance.Stats.DrawCalls), ColorWhite);
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  LightManager.Instance.AddLight(Sun);
  
  GraphicsManager.Instance.AddRenderable(P);
  GraphicsManager.Instance.AddOccluder(P);

  GraphicsManager.Instance.AddRenderable(Sphere);
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

