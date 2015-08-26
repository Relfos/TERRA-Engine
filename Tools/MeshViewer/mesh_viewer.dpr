{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
  TERRA_MemoryManager,
  TERRA_DemoApplication,
  TERRA_Object,
  TERRA_Utils,
  TERRA_MeshFilter,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_Vector3D,
  TERRA_Color,
  TERRA_Matrix4x4,
  TERRA_Texture,
  TERRA_Mesh,
  TERRA_OS,
  TERRA_ScreenFX,
  TERRA_DebugDraw,
  TERRA_Renderer,
  TERRA_EngineManager,
  TERRA_FileManager,
  TERRA_MeshSkeleton, TERRA_MeshAnimation, TERRA_MeshAnimationNodes
//  TERRA_Assimp,
//  TERRA_Milkshape,
//  TERRA_XMLMeshImporter
;

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

  MyMesh:TERRAMesh;
  MyFilter:MeshFilter;

//  Mixer:AnimationCrossfader;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  Self.MainViewport.Visible := True;
  Self.Floor.SetPosition(Vector3D_Zero);

//  Engine.Graphics.Renderer.Settings.PostProcessing.SetValue(True);

(*
  ///MyFilter := AssimpFilter.Create();
  MyFilter := Milkshape3DModel.Create();
  //MyFilter.Load('succubus.ms3d');

  //MyFilter.Load('fox.ms3d');

  //MyFilter.Load('monster009b.ms3d');
  MyFilter.Load('result.ms3d');
  //MyFilter.Load('succubus.xml');

  //MyFilter.Load('D:\code\game resources\animations\brute\Brute@Run.dae');
  //MyFilter.Load('D:\code\game resources\animations\archer\Archer@DashForward.dae');

  MyMesh := TERRAMesh.CreateFromFilter(MyFilter);*)

  MyMesh := Engine.Meshes['brute'];

  Solid := MeshInstance.Create(MyMesh);
  Solid.SetScale(Vector3D_Constant(5));

  //Solid.Geometry.Skeleton.NormalizeJoints();

  //Solid.Animation.Play(Solid.Animation.Find('idle'));
  Solid.Animation.Play(Solid.Animation.Find('jump'));
  //Mixer := AnimationCrossfader.Create(AnimationNode.Create(Solid.Animation, Solid.Animation.Find('idle')), AnimationNode.Create(Solid.Animation, Solid.Animation.Find('walk')), 1000);
  //Solid.Animation.SetRoot(Mixer);
 //Solid.Animation.Play(Animation(AnimationManager.Instance.Resources.GetItemByIndex(0)));

// Solid.Animation.Processor := AssimpFilter(MyFilter).Model;

 //Milkshape3DModel.Save('output.ms3d', MyFilter);

  Self.MainViewport.Camera.SetView(Vector3D_Create(0, -0.25, -0.75));
  Self.MainViewport.Camera.SetPosition(Vector3D_Create(0, 20, 50));
//  Self._Scene.MainViewport.Camera.SetPosition(VectorCreate(0, 10, 25));
End;

Procedure MyDemo.OnDestroy;
Begin
  ReleaseObject(Solid);

  (*ReleaseObject(MyMesh);
  ReleaseObject(MyFilter);*)

  Inherited;
End;

Procedure MyDemo.OnIdle;
Begin
  inherited;

  If (Engine.Input.Keys.WasPressed(keyN)) Then
    Solid.Animation.Crossfade(Solid.Animation.Find('run'), 500);

  If (Engine.Input.Keys.WasPressed(keyM)) Then
    Solid.Animation.Crossfade(Solid.Animation.Find('idle'), 500);

  If (Engine.Input.Keys.WasPressed(keyM)) Then
    Solid.Animation.Crossfade(Solid.Animation.Find('jump'), 500);

End;

Procedure MyDemo.OnRender3D(V: TERRAViewport);
Begin
  Inherited;

  DrawSkeleton(V, Solid.Geometry.Skeleton, Solid.Animation, Solid.Transform, ColorGreen, 1.0);
  DrawBoundingBox(V, Solid.GetBoundingBox(), ColorBlue);
  Engine.Graphics.AddRenderable(V, Solid);
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

