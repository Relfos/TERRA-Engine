{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_RenderTarget, TERRA_Solids, TERRA_Texture, TERRA_BoundingBox,
  TERRA_FileManager, TERRA_Scene, TERRA_MeshFilter, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4,
  TERRA_Math, TERRA_InputManager, TERRA_PhysicsManager, TERRA_NewtonPhysics;

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

      Procedure SpawnBall();
      Procedure SpawnWall(Const Pos, Normal:Vector3D; Size:Single; Tex:Texture);

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

Const
  RoomSize = 10;

Var
  Balls:Array Of MeshInstance;
  BallCount:Integer;

  Walls:Array Of MeshInstance;
  WallCount:Integer;

  Sun:DirectionalLight;

  Fnt:Font;

  PhysicsDebugMode:Boolean;

{ Game }
Procedure Game.OnCreate;
Var
  WorldBox:BoundingBox;
  I:Integer;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;

  GraphicsManager.Instance.ActiveViewport.Camera.Near := 0.1;
  GraphicsManager.Instance.ActiveViewport.Camera.Far := 2000;

  WorldBox.StartVertex := VectorUniform(-9999);
  WorldBox.EndVertex := VectorUniform(9999);
  PhysicsManager.Instance.System := NewtonPhysicsSystem.Create(WorldBox);

  Self.SpawnBall();

  Self.SpawnWall(VectorCreate(0, 0, 0), VectorUp, RoomSize*2, TextureManager.Instance['cobble']);
  Self.SpawnWall(VectorCreate(0, RoomSize * 0.5, -RoomSize), VectorCreate(0, 0, -1), RoomSize * 2, TextureManager.Instance['cobble']);
  Self.SpawnWall(VectorCreate(0, RoomSize * 0.5, RoomSize), VectorCreate(0, 0, 1), RoomSize * 2, TextureManager.Instance['cobble']);
  Self.SpawnWall(VectorCreate(-RoomSize, RoomSize * 0.5, 0), VectorCreate(-1, 0, 0), RoomSize * 2, TextureManager.Instance['cobble']);
  Self.SpawnWall(VectorCreate(RoomSize, RoomSize * 0.5, 0), VectorCreate(1, 0, 0), RoomSize * 2, TextureManager.Instance['cobble']);

  PhysicsManager.Instance.BeginTree();
  For I:=0 To Pred(WallCount) Do
    PhysicsManager.Instance.AddMeshToTree(Walls[I].Geometry.Filter, Walls[I].Transform);
  PhysicsManager.Instance.FinishTree();
End;

Procedure Game.OnDestroy;
Var
  I:Integer;
Begin
  _Scene.Release;

  Sun.Release();

  For I:=0 To Pred(BallCount) Do
    ReleaseObject(Balls[I]);

  For I:=0 To Pred(WallCount) Do
    ReleaseObject(Walls[I]);
End;

Procedure Game.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  If InputManager.Instance.Keys.WasPressed(Ord('M')) Then
    PhysicsDebugMode := Not PhysicsDebugMode;

  If InputManager.Instance.Keys.WasPressed(Ord('P')) Then
    Application.Instance.Paused := Not Application.Instance.Paused;

  If InputManager.Instance.Keys.WasPressed(Ord('B')) Then
    Self.SpawnBall();

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam;
End;

Procedure Game.SpawnBall();
Var
  Ball:MeshInstance;
  Size:Single;
Begin
  Size := RandomFloat(RoomSize * 0.1, RoomSize * 0.15);
  Ball := MeshInstance.Create(MeshManager.Instance.SphereMesh);
  Ball.SetDiffuseMap(0, TextureManager.Instance['marble_diffuse']);
  Ball.Position := VectorCreate(RandomFloat(-RoomSize*0.4, RoomSize*0.4), RoomSize * 1.5, RandomFloat(-RoomSize*0.4, RoomSize*0.4));
  Ball.Scale := VectorUniform(Size);

  Ball.SetDiffuseColor(ColorGrey(Random(255)));

  Ball.ActivatePhysics(20);

  Inc(BallCount);
  SetLength(Balls, BallCount);
  Balls[Pred(BallCount)] := Ball;
End;

Procedure Game.SpawnWall(Const Pos, Normal:Vector3D; Size:Single; Tex:Texture);
Var
  Wall:MeshInstance;
Begin
  Wall := MeshInstance.Create(CreatePlaneMesh(Normal, 4));
  Wall.SetDiffuseMap(0, Tex);
  Wall.Position := Pos;
  Wall.Scale := VectorUniform(Size);

  Wall.SetUVScale(0, (Size/RoomSize)*4,(Size/RoomSize)*4);

  Inc(WallCount);
  SetLength(Walls, WallCount);
  Walls[Pred(WallCount)] := Wall;
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
Var
  I:Integer;
Begin
  If PhysicsDebugMode Then
  Begin
    GraphicsManager.Instance.AddRenderable(PhysicsManager.Instance.System);
    Exit;
  End;

  LightManager.Instance.AddLight(Sun);

  For I:=0 To Pred(BallCount) Do
    GraphicsManager.Instance.AddRenderable(Balls[I]);

  For I:=0 To Pred(WallCount) Do
    GraphicsManager.Instance.AddRenderable(Walls[I]);
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

