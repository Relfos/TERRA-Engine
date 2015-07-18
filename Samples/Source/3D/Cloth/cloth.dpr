{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_MemoryManager,
  TERRA_DemoApplication,
  TERRA_OS,
  TERRA_Object,
  TERRA_Utils,
  TERRA_GraphicsManager,
  TERRA_Vector2D,
  TERRA_Vector3D,
  TERRA_UI,
  TERRA_Lights,
  TERRA_Viewport,
  TERRA_JPG,
  TERRA_PNG,
  TERRA_Texture,
  TERRA_Renderer,
  TERRA_Math,
  TERRA_FileManager,
  TERRA_Scene,
  TERRA_MeshFilter,
  TERRA_Mesh,
  TERRA_Color,
  TERRA_VertexFormat,
  TERRA_Resource,
  TERRA_Matrix4x4,
  TERRA_ScreenFX,
  TERRA_DebugDraw,
  TERRA_Cloth,
  TERRA_InputManager;

Const
  SphereRadius = 4.0;
  SphereCount = 6;

Type
  MyDemo = Class(DemoApplication)
    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender(V:TERRAViewport); Override;
  End;



Var
  //What do we want to draw?
  drawSprings, drawTriangles:Boolean;

  //Floor texture
  floorTex:TERRATexture;
  Floor:MeshInstance;


  Clothes:ClothSystem;

  MoveCloth:Boolean = False;

  ClothTex:TERRATexture;
  DiffuseTex:TERRATexture;

  Spheres:Array[0..Pred(SphereCount)] Of MeshInstance;
  ClothInstance:MeshInstance;

{ Game }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
Begin
  Inherited;

  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Renderer.Settings.PostProcessing.SetValue(True);

  DiffuseTex := TextureManager.Instance.GetTexture('cobble');
  ClothTex := TextureManager.Instance.GetTexture('cloth_diffuse');
  FloorTex := TextureManager.Instance.GetTexture('woodfloor_diffuse');

  Floor := MeshInstance.Create(MeshManager.Instance.PlaneMesh);
  Floor.SetDiffuseMap(0, FloorTex);
  Floor.SetPosition(VectorCreate(0, -SphereRadius, 0));
  Floor.SetScale(VectorConstant(SphereRadius * 10.0));
  Floor.SetUVScale(0, 4, 4);

  Clothes := ClothSystem.Create(ClothTex);
  ClothInstance := MeshInstance.Create(Clothes.Mesh);

  For I:=0 To Pred(SphereCount) Do
  Begin
    Spheres[I] := MeshInstance.Create(MeshManager.Instance.SphereMesh);
    Spheres[I].SetDiffuseMap(0, DiffuseTex);
    Spheres[I].SetPosition(VectorCreate(-3 + Cos(I*60*RAD) * SphereRadius * 2, 0, 1.5 + Sin(I*60*RAD) * SphereRadius * 2));
    Spheres[I].SetScale(VectorConstant(SphereRadius));
    Clothes.SetCollider(I, Spheres[I].Position, SphereRadius);
  End;

End;

Procedure MyDemo.OnDestroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(SphereCount) Do
    ReleaseObject(Spheres[I]);

  ReleaseObject(Clothes);
  ReleaseObject(Floor);
  ReleaseObject(ClothInstance);

  Inherited;
End;

Procedure MyDemo.OnIdle;
Begin
  Inherited;

	//Release corners
	If(InputManager.Instance.Keys.WasPressed(Ord('1'))) Then
		Clothes.UnpinParticle(0);

	If(InputManager.Instance.Keys.WasPressed(Ord('2'))) Then
		Clothes.UnpinParticle(gridSize-1);

	If(InputManager.Instance.Keys.WasPressed(Ord('3'))) Then
		Clothes.UnpinParticle(gridSize*(gridSize-1));

	If(InputManager.Instance.Keys.WasPressed(Ord('4'))) Then
		Clothes.UnpinParticle(gridSize*gridSize-1);

	If(InputManager.Instance.Keys.WasPressed(Ord('5'))) Then
  Begin
		Clothes.UnpinParticle(gridSize*(gridSize-1));
		Clothes.UnpinParticle(gridSize*gridSize-1);
  End;

	If(InputManager.Instance.Keys.WasPressed(Ord('M'))) Then
	Begin
		MoveCloth := Not MoveCloth;
	End;

	//Toggle drawing modes
	If(InputManager.Instance.Keys.WasPressed(Ord('Y'))) Then
	Begin
		//Clothes._Gravity.Scale(-1);
	End;

	If(InputManager.Instance.Keys.WasPressed(Ord('T'))) Then
	Begin
		drawSprings := Not drawSprings;
	End;

	If(InputManager.Instance.Keys.WasPressed(Ord('R'))) Then
	Begin
  	//Reset cloth
		Clothes.Reset();
  End;

  Clothes.Simulate();
End;


Procedure MyDemo.OnRender(V:TERRAViewport);
Var
  I:Integer;
Begin
  If Assigned(ClothInstance) Then
    ClothInstance.SetWireframeMode(0, drawSprings);

  For I:=0 To Pred(SphereCount) Do
    GraphicsManager.Instance.AddRenderable(V, Spheres[I]);

  GraphicsManager.Instance.AddRenderable(V, Floor);
  GraphicsManager.Instance.AddRenderable(V, ClothInstance);

  //DrawBoundingBox(V, ClothInstance.GetBoundingBox, ColorBlue);
End;


(*  Text.DrawText(10, 10, 10, 'FPS :'+IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
  Text.DrawText(10, 30, 10, 'Vertices :'+IntToString(Clothes._ParticleCount));
  Text.DrawText(10, 50, 10, 'Sprints :'+IntToString(Clothes._SpringCount));*)

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}

End.

