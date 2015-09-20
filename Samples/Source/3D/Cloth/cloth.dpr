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
  TERRA_Lights,
  TERRA_Viewport,
  TERRA_Texture,
  TERRA_Engine,
  TERRA_Renderer,
  TERRA_Math,
  TERRA_FileManager,
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

      Procedure OnRender3D(V:TERRAViewport); Override;
  End;



Var
  //What do we want to draw?
  drawSprings, drawTriangles:Boolean;

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

  // enable 3D rendering
  Self.MainViewport.Visible := True;

  Self.ShowFPS := True;

  Engine.Graphics.Renderer.Settings.NormalMapping.SetValue(True);
  Engine.Graphics.Renderer.Settings.PostProcessing.SetValue(True);

  DiffuseTex := Engine.Textures.GetItem('cobble');
  ClothTex := Engine.Textures.GetItem('cloth_diffuse');

  Clothes := ClothSystem.Create(ClothTex);
  ClothInstance := MeshInstance.Create(Clothes.Mesh);

  For I:=0 To Pred(SphereCount) Do
  Begin
    Spheres[I] := MeshInstance.Create(Engine.Meshes.SphereMesh);
    Spheres[I].SetDiffuseMap(0, DiffuseTex);
    Spheres[I].SetPosition(Vector3D_Create(-3 + Cos(I*60*RAD) * SphereRadius * 2, 0, 1.5 + Sin(I*60*RAD) * SphereRadius * 2));
    Spheres[I].SetScale(Vector3D_Constant(SphereRadius * 2));
    Clothes.SetCollider(I, Spheres[I].Position, SphereRadius);
  End;

  Self.Floor.SetPosition(Vector3D_Create(0, -SphereRadius, 0));
End;

Procedure MyDemo.OnDestroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(SphereCount) Do
    ReleaseObject(Spheres[I]);

  ReleaseObject(Clothes);
  ReleaseObject(ClothInstance);

  Inherited;
End;

Procedure MyDemo.OnIdle;
Begin
  Inherited;

	//Release corners
	If(Engine.Input.Keys.WasPressed(Ord('1'))) Then
		Clothes.UnpinParticle(0);

	If(Engine.Input.Keys.WasPressed(Ord('2'))) Then
		Clothes.UnpinParticle(gridSize-1);

	If(Engine.Input.Keys.WasPressed(Ord('3'))) Then
		Clothes.UnpinParticle(gridSize*(gridSize-1));

	If(Engine.Input.Keys.WasPressed(Ord('4'))) Then
		Clothes.UnpinParticle(gridSize*gridSize-1);

	If(Engine.Input.Keys.WasPressed(Ord('5'))) Then
  Begin
		Clothes.UnpinParticle(gridSize*(gridSize-1));
		Clothes.UnpinParticle(gridSize*gridSize-1);
  End;

	If(Engine.Input.Keys.WasPressed(Ord('M'))) Then
	Begin
		MoveCloth := Not MoveCloth;
	End;

	//Toggle drawing modes
	If(Engine.Input.Keys.WasPressed(Ord('Y'))) Then
	Begin
		//Clothes._Gravity.Scale(-1);
	End;

	If(Engine.Input.Keys.WasPressed(Ord('T'))) Then
	Begin
		drawSprings := Not drawSprings;
	End;

	If(Engine.Input.Keys.WasPressed(Ord('R'))) Then
	Begin
  	//Reset cloth
		Clothes.Reset();
  End;

  Clothes.Simulate();
End;


Procedure MyDemo.OnRender3D(V:TERRAViewport);
Var
  I:Integer;
Begin
  Inherited;
  
  If Assigned(ClothInstance) Then
    ClothInstance.SetWireframeMode(0, drawSprings);

  For I:=0 To Pred(SphereCount) Do
    Engine.Graphics.AddRenderable(V, Spheres[I]);

  Engine.Graphics.AddRenderable(V, ClothInstance);

  //DrawBoundingBox(V, ClothInstance.GetBoundingBox, ColorBlue);
End;


(*  Text.DrawText(10, 10, 10, 'FPS :'+IntToString(Engine.Graphics.Renderer.Stats.FramesPerSecond));
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

