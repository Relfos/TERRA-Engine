{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_String, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D,TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_Texture, TERRA_Renderer, TERRA_Mesh, TERRA_ShaderFactory,
  TERRA_FileManager, TERRA_Scene,  TERRA_Color, TERRA_Matrix4x4, TERRA_ClipRect,
  TERRA_ScreenFX, TERRA_VertexFormat, TERRA_InputManager, TERRA_DebugDraw;

Type
  MyScene = Class(Scene)

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
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
  NormalInstance:MeshInstance;
  SmoothedInstance:MeshInstance;

  ClonedMesh:Mesh;

  Sun:DirectionalLight;

  Fnt:Font;

  MeshName:TERRAString = 'monster220';

Procedure ReloadMesh();
Var
  MyMesh:Mesh;
Begin
  ReleaseObject(ClonedMesh);

  MyMesh := MeshManager.Instance.GetMesh(MeshName);
  If Assigned(MyMesh) Then
  Begin
    NormalInstance := MeshInstance.Create(MyMesh);

    ClonedMesh := MeshManager.Instance.CloneMesh(MeshName);
    ClonedMesh.Prefetch();
    ClonedMesh.SubDivide();
    ClonedMesh.Smooth();
    SmoothedInstance := MeshInstance.Create(ClonedMesh);

    NormalInstance.SetPosition(VectorCreate(-10, 0, 0));
    SmoothedInstance.SetPosition(VectorCreate(10, 0, 0));

    NormalInstance.Animation.Play('idle');
    SmoothedInstance.Animation.Play('idle');
  End;
End;

{ Game }
Procedure Game.OnCreate;
Begin
  //FileManager.Instance.AddPath('Assets');
  FileManager.Instance.AddPath('D:\code\minimonhd\trunk\output\mesh');
  FileManager.Instance.AddPath('D:\code\minimonhd\trunk\output\textures');

  Fnt := FontManager.Instance.DefaultFont;

  ReloadMesh();

  Sun := DirectionalLight.Create(VectorCreate(-0.05, 0.7, 0.25));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorGreen;
  GraphicsManager.Instance.ActiveViewport.Camera.SetPosition(VectorCreate(0, 20, 50));
  GraphicsManager.Instance.ActiveViewport.Camera.SetView(VectorCreate(0, -0.3, -0.7));
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);

  ReleaseObject(Sun);
  ReleaseObject(NormalInstance);
  ReleaseObject(SmoothedInstance);
  ReleaseObject(ClonedMesh);
End;

Procedure Game.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  If InputManager.Instance.Keys.WasPressed(keyEnter) Then
  Begin
    MeshName := StringCopy(MeshName, Length('monster') + 1, MaxInt);
    MeshName := 'monster' + StringPadLeft(IntToString(Succ(StringToInt(MeshName))), 3, Ord('0'));
    ReloadMesh();
  End;

  If InputManager.Instance.Keys.WasPressed(keySpace) Then
  Begin
    NormalInstance.SetWireframeMode(0, Not NormalInstance.GetWireframeMode(0));
    SmoothedInstance.SetWireframeMode(0, Not SmoothedInstance.GetWireframeMode(0));
  End;

  GraphicsManager.Instance.TestDebugKeys();

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam;
End;


{ MyScene }
Constructor MyScene.Create;
Begin
End;

Procedure MyScene.Release;
Begin
End;

Procedure MyScene.RenderSprites;
Var
  P:Vector3D;
  Clip:ClipRect;
Begin
  Clip.Style := clipNothing;

{  NormalInstance.Geometry.GetGroup(0).Vertices.GetVector3D(0, vertexPosition, P);
  P := NormalInstance.Transform.Transform(P);
  P := GraphicsManager.Instance.ActiveViewport.ProjectPoint(P);
  DrawPoint2D(VectorCreate2D(P.X, P.Y), ColorRed, 10, 2, Clip, False);}
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(NormalInstance);
  GraphicsManager.Instance.AddRenderable(SmoothedInstance);
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

