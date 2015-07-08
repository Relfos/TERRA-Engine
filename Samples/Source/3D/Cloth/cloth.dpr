{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
//  MemCheck,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_Utils,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Vector3D,
  TERRA_Font,
  TERRA_FontRenderer,
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
  TERRA_Skybox,
  TERRA_Widgets,
  TERRA_Color,
  TERRA_VertexFormat,
  TERRA_Resource,
  TERRA_Matrix4x4,
  TERRA_ScreenFX,
  TERRA_DebugDraw,
  TERRA_InputManager;


Const
  MinimumDelta = 10;
  SimScale = 2;

  SphereRadius = 4.0;
  SphereCount = 6;

  DefaultGravity:Vector3d = (X:0.0; Y:-0.98 * SimScale; Z:0.0);

  // size of the cloth mesh
  ClothScale = 20.0; //10

//Values given to each spring
  StretchStiffness = 2.5 * ClothScale;
  BendStiffness = 1.0 * ClothScale;


//Values given to each ball
  mass = 0.01 * SimScale;

//Damping factor. Velocities are multiplied by this
  dampFactor=0.9;

//Grid complexity. This is the number of Particles across and down in the model
  gridSize = 13* SimScale; //13;

Type
  MyScene = Class(Scene)
      Sky:Skybox;
      Main:Viewport;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

  Demo = Class(Application)
    Protected
      _Scene:MyScene;

    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;


Type
  ClothSystem = Class;

  ClothSpring = Object
    Protected
    	//Indices of the Particles at either end of the spring
  	  P1, P2:Integer;

      _NaturalLength:Double;
    	_InverseLength:Double;

      _Stiffness:Double;
      
    Procedure Init(PID1, PID2: Integer; Len, Stiffness:Double);
  End;

  ClothSpringLink = Record
    SpringIndex:Integer;
    ParticleIndex:Integer;
  End;

  ClothParticle = Object
    Protected
	    CurrentPosition:Vector3D;
    	CurrentVelocity:Vector3D;

	    NextPosition:Vector3D;
  	  NextVelocity:Vector3D;

      _Tension:Vector3D;
	    _InverseMass:Single;

  	  //Is this ball held in position?
	    _Fixed:Boolean;
  End;

  ClothSystem = Class(TERRAObject)
    Protected
      _SpringCount:Integer;
      _Springs:Array Of ClothSpring;
      _ParticleCount:Integer;
      _Particles:Array Of ClothParticle;

      _Mesh:Mesh;
      _Group:MeshGroup;
      _VertexFormat:VertexFormat;

      _LastTime:Cardinal;
      _timeSinceLastUpdate:Cardinal;

      _Gravity:Vector3D;

      Procedure InitMesh();
      Procedure UpdateMesh();

    Public
      Constructor Create();
      Procedure Release(); Override;
      Procedure Reset();
      Procedure Simulate();
  End;

Var
  //What do we want to draw?
  drawSprings, drawTriangles:Boolean;

  //Floor texture
  floorTex:Texture;
  Floor:MeshInstance;


  Clothes:ClothSystem;

  MoveCloth:Boolean = False;

  ClothTex:Texture;
  DiffuseTex:Texture;

  Sun:DirectionalLight;

  Spheres:Array[0..Pred(SphereCount)] Of MeshInstance;
  ClothInstance:MeshInstance;

  Text:FontRenderer;

{ Game }
Procedure Demo.OnCreate;
Var
  I:Integer;
Begin
  FileManager.Instance.AddPath('Assets');

  Text := FontRenderer.Create();
  Text.SetFont(FontManager.Instance.DefaultFont);

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

  For I:=0 To Pred(SphereCount) Do
  Begin
    Spheres[I] := MeshInstance.Create(MeshManager.Instance.SphereMesh);
    Spheres[I].SetDiffuseMap(0, DiffuseTex);
    Spheres[I].SetPosition(VectorCreate(-3 + Cos(I*60*RAD) * SphereRadius * 2, 0, 1.5 + Sin(I*60*RAD) * SphereRadius * 2));
    Spheres[I].SetScale(VectorConstant(SphereRadius));
  End;

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Var
  I:Integer;
Begin
  ReleaseObject(_Scene);

  For I:=0 To Pred(SphereCount) Do
    ReleaseObject(Spheres[I]);

  ReleaseObject(Clothes);
  ReleaseObject(Sun);
  ReleaseObject(Floor);
  ReleaseObject(ClothInstance);

  ReleaseObject(Text);
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();
  _Scene.Main.Camera.FreeCam;

	//Release corners
	If(InputManager.Instance.Keys.WasPressed(Ord('1'))) Then
		Clothes._Particles[0]._Fixed := False;

	If(InputManager.Instance.Keys.WasPressed(Ord('2'))) Then
		Clothes._Particles[gridSize-1]._Fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('3'))) Then
		Clothes._Particles[gridSize*(gridSize-1)]._Fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('4'))) Then
		Clothes._Particles[gridSize*gridSize-1]._Fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('5'))) Then
  Begin
		Clothes._Particles[gridSize*(gridSize-1)]._Fixed := false;
		Clothes._Particles[gridSize*gridSize-1]._Fixed := false;
  End;

	If(InputManager.Instance.Keys.WasPressed(Ord('M'))) Then
	Begin
		MoveCloth := Not MoveCloth;
	End;

	//Toggle drawing modes
	If(InputManager.Instance.Keys.WasPressed(Ord('Y'))) Then
	Begin
		Clothes._Gravity.Scale(-1);
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


{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');

  Main := GraphicsManager.Instance.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  Main.Camera.SetPosition(VectorCreate(0, SphereRadius, SphereRadius * 6));

  Clothes := ClothSystem.Create();
  ClothInstance := MeshInstance.Create(Clothes._Mesh);
End;

Procedure MyScene.Release;
Begin
//  ReleaseObject(Main);
  ReleaseObject(Sky);
End;


Procedure MyScene.RenderSprites;
Begin
  Text.DrawText(10, 10, 10, 'FPS :'+IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
  Text.DrawText(10, 30, 10, 'Vertices :'+IntToString(Clothes._ParticleCount));
  Text.DrawText(10, 50, 10, 'Sprints :'+IntToString(Clothes._SpringCount));
End;

Procedure MyScene.RenderViewport(V:Viewport);
Var
  I:Integer;
Begin
  If Assigned(ClothInstance) Then
    ClothInstance.SetWireframeMode(0, drawSprings);

  LightManager.Instance.AddLight(Sun);

  For I:=0 To Pred(SphereCount) Do
    GraphicsManager.Instance.AddRenderable(Spheres[I]);

  GraphicsManager.Instance.AddRenderable(Floor);
  GraphicsManager.Instance.AddRenderable(ClothInstance);

  //DrawBoundingBox(V, ClothInstance.GetBoundingBox, ColorBlue);
End;


Procedure MyScene.RenderSky;
Begin
  Sky.Render;
End;

{ ClothSpring }
Procedure ClothSpring.Init(PID1, PID2: Integer; Len, Stiffness:Double);
Begin
  Self.P1 := PID1;
  Self.P2 := PID2;
  Self._NaturalLength := Len;
  Self._InverseLength := 1.0 / Len;
  Self._Stiffness := Stiffness;
End;

{ ClothSystem }
Constructor ClothSystem.Create;
Var
  I:Integer;
Begin
  _lastTime := 0;
  _timeSinceLastUpdate := 0;

  _Gravity := DefaultGravity;

	//Calculate number of Particles
	_ParticleCount := gridSize*gridSize;

	//Calculate number of springs
	//There is a spring pointing right for each ball which is not on the right edge,
	//and one pointing down for each ball not on the bottom edge
	_SpringCount := (gridSize-1)*gridSize*2;

	//There is a spring pointing down & right for each ball not on bottom or right,
	//and one pointing down & left for each ball not on bottom or left
	Inc(_SpringCount, (gridSize-1)*(gridSize-1)*2);

	//There is a spring pointing right (to the next but one ball)
	//for each ball which is not on or next to the right edge,
	//and one pointing down for each ball not on or next to the bottom edge
	Inc(_SpringCount, (gridSize-2)*gridSize*2);

	//Create space for Particles & springs
	SetLength(_Particles, _ParticleCount);
	SetLength(_Springs, _SpringCount);

  InitMesh();

	//Reset cloth
	Self.Reset();
End;

Procedure ClothSystem.InitMesh();
Var
  BallID:Integer;
  I, J, K:Integer;
  i0, i1, i2, i3:Integer;
  T:Triangle;
Begin
  ReleaseObject(_Mesh);
  _Mesh := Mesh.Create(rtDynamic, 'cloth');

  _VertexFormat := [vertexFormatPosition, vertexFormatNormal, vertexFormatColor, vertexFormatUV0];

  _Group := _Mesh.AddGroup(_VertexFormat);
  _Group.DiffuseMap := ClothTex;
  _Group.Flags := _Group.Flags Or meshGroupDoubleSided;

  _Group.TriangleCount := (gridSize * gridSize) * 2;
  _Group.VertexCount := (gridSize * gridSize);

  K := 0;
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize-1) Do
		Begin
      i0 := J*gridSize + I;
			i1 := J*gridSize + I + 1;
			i2 := (J+1)*gridSize + I;
			i3 := (J+1)*gridSize + I + 1;

      T.Indices[0] := i2;
      T.Indices[1] := i1;
      T.Indices[2] := i0;
      _Group.SetTriangle(T, K); Inc(K);

      T.Indices[0] := i2;
      T.Indices[1] := i3;
      T.Indices[2] := i1;
      _Group.SetTriangle(T, K); Inc(K);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      _Group.Vertices.SetVector2D(BallID, vertexUV0, VectorCreate2D(I/GridSize, J/GridSize));
      _Group.Vertices.SetColor(BallID, vertexColor, ColorWhite);
    End;
End;

Procedure ClothSystem.Reset();
Var
  I, J, K:Integer;
  BallID, X,Y:Integer;
  currentSpring:Integer;
  naturalLength:Single;

  U,V:Single;
Begin
	//Initialise the Particles in an evenly spaced grid in the x-z plane
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      U := (I/Pred(gridSize)) - 0.5;
      V := (J/Pred(gridSize)) - 0.5;

      BallID := J*gridSize+I;
			_Particles[BallID].CurrentPosition := VectorCreate(ClothScale * U, 8.5, ClothScale * V);
			_Particles[BallID].CurrentVelocity := VectorZero;

			_Particles[BallID]._InverseMass := 1 / Mass;
			_Particles[BallID]._Fixed := False;

      _Particles[BallID]._Tension := VectorZero;
		End;

  NaturalLength := _Particles[0].CurrentPosition.Distance(_Particles[1].CurrentPosition);

	//Fix the top left & top right Particles in place
	_Particles[0]._Fixed := true;
	_Particles[gridSize-1]._Fixed :=true;

	//Fix the bottom left & bottom right Particles
	_Particles[gridSize*(gridSize-1)]._Fixed := true;
	_Particles[gridSize*gridSize-1]._Fixed := true;

  //_Particles[(gridSize Shr 1) * GRidsize +  (gridSize Shr 1)]._Fixed :=true;

	//Initialise the springs
	currentSpring := 0;

	//The first (gridSize-1)*gridSize springs go from one ball to the next,
	//excluding those on the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+1, naturalLength, StretchStiffness);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*gridSize springs go from one ball to the one below,
	//excluding those on the bottom edge
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize) Do
    Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I, naturalLength, StretchStiffness);
			Inc(currentSpring);
    End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and right
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I+1, naturalLength*sqrt(2.0), BendStiffness);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and left
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I-1, naturalLength*sqrt(2.0), BendStiffness);
			Inc(currentSpring);
		End;

	//The first (gridSize-2)*gridSize springs go from one ball to the next but one,
	//excluding those on or next to the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-2) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+2, naturalLength*2, BendStiffness);
			Inc(currentSpring);
		End;


	//The next (gridSize-2)*gridSize springs go from one ball to the next but one below,
	//excluding those on or next to the bottom edge
  For J:=0 To Pred(gridSize-2) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+2)*gridSize+I, naturalLength*2, BendStiffness);
			Inc(currentSpring);
		End;


  UpdateMesh();
End;

Procedure ClothSystem.UpdateMesh();
Var
  I, J, BallID, Index, X, Y:Integer;
  normal:Vector3D;

  Vertices:VertexData;
Begin
  _Group.CalculateTriangleNormals();

  Vertices := _Group.LockVertices();

  //Calculate the normals on the current Particles
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
    Begin
      BallID := J*gridSize+I;
      Normal := VectorZero;
      For Y:=0 To 1 Do
        For X:=0 To 1 Do
        If (X+I<GridSize) And (Y+J<GridSize) Then
        Begin
          Index := (J+Y)* gridSize + (I + X) * 2;
          Normal.Add(_Group.GetTriangleNormal(Index));

          Inc(Index);
          Normal.Add(_Group.GetTriangleNormal(Index));
        End;

      //Normal.Normalize();
      Normal.Scale(0.25);
      Vertices.SetVector3D(BallID, vertexNormal, Normal);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      Vertices.SetVector3D(BallID, vertexPosition, _Particles[BallID].CurrentPosition);
    End;


  _Group.UnlockVertices();

  _Mesh.UpdateBoundingBox();
End;

Procedure ClothSystem.Simulate;
Var
  currentTime, timePassed:Cardinal;
  updateMade:Boolean;
  timePassedInSeconds:Single;
  I, J, K, N:Integer;

  springLength, Extension, Tension:Double;
  force, acceleration, tensionDirection:VECTOR3D;
  P:Vector3D;
Begin
	//set currentTime and timePassed
  If _lastTime =0 Then
  Begin
    _lastTime := Application.GetTime();
    Exit;
  End;

	currentTime := Application.GetTime();
	timePassed := currentTime - _lastTime;
	_lastTime := currentTime;

	//Update the physics in intervals of 10ms to prevent problems
	//with different frame rates causing different damping
	Inc(_timeSinceLastUpdate, timePassed);

	updateMade := false;	//did we update the positions etc this time?

	while (_timeSinceLastUpdate>MinimumDelta) Do
	Begin
		Dec(_timeSinceLastUpdate, MinimumDelta);
		timePassedInSeconds := 1.0/ (1000/MinimumDelta);
		updateMade := True;

		//Calculate the tensions in the springs
		For I:=0 To Pred(_SpringCount) Do
		Begin
      TensionDirection :=	VectorSubtract(_Particles[_Springs[i].P1].CurrentPosition, _Particles[_Springs[i].P2].CurrentPosition);

			springLength := TensionDirection.Length();
			extension := springLength - _Springs[i]._naturalLength;

			//_Springs[i].
      Tension := _Springs[i]._Stiffness * (Extension * _Springs[i]._InverseLength);

      TensionDirection.Scale(Tension  * (1 / springLength));

      _Particles[_Springs[i].P2]._Tension.Add(tensionDirection);
      tensionDirection.Scale(-1.0);
      _Particles[_Springs[i].P1]._Tension.Add(tensionDirection);
		End;


		//Calculate the nextParticles from the currentParticles
		For I:=0 To Pred(_ParticleCount) Do
		Begin
			//If the ball is fixed, transfer the position and zero the velocity, otherwise calculate
			//the new values
			If (_Particles[i]._Fixed) Then
			Begin
				_Particles[i].NextPosition := _Particles[i].CurrentPosition;
				_Particles[i].NextVelocity := VectorZero;
        (*If MoveCloth Then
          _Particles[i].NextPosition.Add(VectorCreate(0, 2 * timePassedInSeconds, 5 * timePassedInSeconds));*)
        Continue;
			End;

      //Calculate the force on this ball
      Force := VectorAdd(_Gravity, _Particles[I]._Tension);

			//Calculate the acceleration
			Acceleration := VectorScale(force, _Particles[i]._InverseMass);

			//Update velocity
			_Particles[i].NextVelocity := VectorAdd(_Particles[i].CurrentVelocity, VectorScale(acceleration, timePassedInSeconds));

			//Damp the velocity
			_Particles[i].NextVelocity.Scale(dampFactor);

			//Calculate new position
       //Forcse := VectorAdd(VectorScale(Particles[i].NextVelocity, 0.5), VectorScale(Particles[i].CurrentVelocity, 0.5));
       Force := _Particles[i].NextVelocity;

       Force.Scale(timePassedInSeconds);
			_Particles[i].NextPosition := VectorAdd(_Particles[i].CurrentPosition, Force);

			//Check against floor
			If(_Particles[i].NextPosition.y <= -sphereRadius * 0.92) Then
      Begin
			  _Particles[i].NextPosition.y := -sphereRadius * 0.92;
        _Particles[i].NextVelocity.y := 0;
         Continue;
      End;

			//Check against sphere (at origin)
      For J:=0 To Pred(SphereCount) Do
      Begin
        P := VectorSubtract(_Particles[i].NextPosition, Spheres[J].Position);
  			If (P.LengthSquared < Sqr(sphereRadius*1.08)) Then
        Begin
		  	  P.Normalize();
          P.Scale(sphereRadius*1.08);
          _Particles[i].NextPosition := VectorAdd(P, Spheres[J].Position);
          _Particles[i].NextVelocity := VectorZero;
          Break;
        End;
      End;
		End;

		//Swap the currentParticles and newParticles pointers
		For I:=0 To Pred(_ParticleCount) Do
    Begin
      _Particles[i].CurrentPosition := _Particles[i].NextPosition;
			_Particles[i].CurrentVelocity := _Particles[i].NextVelocity;
      _Particles[i]._Tension := VectorZero;
    End;
	End;

	//Calculate the normals if we have updated the positions
	If(updateMade) Then
    Self.UpdateMesh();
End;

Procedure ClothSystem.Release;
Begin
  ReleaseObject(_Mesh);
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

