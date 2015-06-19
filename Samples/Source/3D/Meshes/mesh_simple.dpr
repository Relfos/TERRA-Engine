{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_Texture, TERRA_Renderer, TERRA_Mesh, TERRA_ShaderFactory,
  TERRA_FileManager, TERRA_Scene,  TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4,
  TERRA_ScreenFX, TERRA_VertexFormat, TERRA_InputManager;
                                                      
Type
  MyScene = Class(Scene)
      Sky:Skybox;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

  Game = Class(Application)
    Protected
      _Scene:MyScene;

    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;


Var
  CarInstance:MeshInstance;
  DwarfInstance:MeshInstance;

  Sun:DirectionalLight;

  Fnt:Font;

{ Game }
Procedure Game.OnCreate;
Var
  MyMesh:Mesh;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  MyMesh := MeshManager.Instance.GetMesh('jeep');
  If Assigned(MyMesh) Then
  Begin
    CarInstance :=MeshInstance.Create(MyMesh);
  End Else
    CarInstance := Nil;

  MyMesh := MeshManager.Instance.GetMesh('dwarf');
  If Assigned(MyMesh) Then
  Begin
    DwarfInstance :=MeshInstance.Create(MyMesh);
    DwarfInstance.SetPosition(VectorCreate(10, 0, 0));
    DwarfInstance.Animation.Play('walk');
  End Else
    DwarfInstance := Nil;

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);

  ReleaseObject(Sun);
  ReleaseObject(DwarfInstance);
  ReleaseObject(CarInstance);
End;

Procedure Game.OnIdle;
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
  GraphicsManager.Instance.AddRenderable(DwarfInstance);
  GraphicsManager.Instance.AddRenderable(CarInstance);
End;

Procedure MyScene.RenderSky;
Begin
  Sky.Render;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  Game.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

