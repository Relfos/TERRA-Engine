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
  TERRA_InputManager;

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


Const
  MinimumDelta = 10;

  SphereRadius = 4.0;

  Gravity:Vector3d = (X:0.0; Y:-0.98; Z:0.0);

//Values given to each spring
  DefaultSpringConstant = 15.0;

//Values given to each ball
  mass =0.01;

//Damping factor. Velocities are multiplied by this
  dampFactor=0.9;

  // size of the cloth mesh
  ClothScale = 20.0; //10

//Grid complexity. This is the number of Particles across and down in the model
  gridSize = 18*2; //13;

Type
  ClothSystem = Class;
  
  ClothSpring = Object
    Protected
      _System:ClothSystem;
      _Index:Integer;

    	//Indices of the Particles at either end of the spring
  	  ball1, ball2:Integer;

    	//Tension in the spring
	    tension:Single;

  	  springConstant:Single;
    	naturalLength:Single;

    Procedure Init(PID1, PID2: Integer; Len: Single);
  End;

  ClothParticle = Object
	  CurrentPosition:Vector3D;
  	CurrentVelocity:Vector3D;

	  NextPosition:Vector3D;
  	NextVelocity:Vector3D;

	  InverseMass:Single;

	//Is this ball held in position?
	  Fixed:Boolean;

    Springs:Array Of Integer;
    SpringCount:Integer;

    Procedure AddSpring(Index:Integer);
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

      _lastTime:Cardinal;
      _timeSinceLastUpdate:Cardinal;

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
  drawParticles, drawSprings, drawTriangles:Boolean;

  //Floor texture
  floorTex:Texture;
  Floor:MeshInstance;


  Clothes:ClothSystem;

  MoveCloth:Boolean = False;

  ClothTex:Texture;
  DiffuseTex:Texture;

  Sun:DirectionalLight;

  Sphere:MeshInstance;
  ClothInstance:MeshInstance;

  Text:FontRenderer;

{ Game }
Procedure Demo.OnCreate;
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

  Sphere := MeshInstance.Create(MeshManager.Instance.SphereMesh);
  Sphere.SetDiffuseMap(0, DiffuseTex);
  Sphere.SetPosition(VectorCreate(SphereRadius * 0.75, SphereRadius * 0.2, SphereRadius * 0.5));
  Sphere.SetScale(VectorConstant(SphereRadius));

  Sun := DirectionalLight.Create(VectorCreate(-0.25, 0.75, 0.0));

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Scene);

  ReleaseObject(Clothes);
  ReleaseObject(Sun);
  ReleaseObject(Sphere);
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
		Clothes._Particles[0].fixed := False;

	If(InputManager.Instance.Keys.WasPressed(Ord('2'))) Then
		Clothes._Particles[gridSize-1].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('3'))) Then
		Clothes._Particles[gridSize*(gridSize-1)].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('4'))) Then
		Clothes._Particles[gridSize*gridSize-1].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('5'))) Then
  Begin
		Clothes._Particles[gridSize*(gridSize-1)].fixed := false;
		Clothes._Particles[gridSize*gridSize-1].fixed := false;
  End;

	If(InputManager.Instance.Keys.WasPressed(Ord('M'))) Then
	Begin
		MoveCloth := Not MoveCloth;
	End;

	//Toggle drawing modes
	If(InputManager.Instance.Keys.WasPressed(Ord('Y'))) Then
	Begin
		drawParticles := Not drawParticles;
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
Begin
  If Assigned(ClothInstance) Then
    ClothInstance.SetWireframeMode(0, drawSprings);

  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(Sphere);
  GraphicsManager.Instance.AddRenderable(Floor);
  GraphicsManager.Instance.AddRenderable(ClothInstance);
End;


Procedure MyScene.RenderSky;
Begin
  Sky.Render;
End;

{ ClothSpring }
Procedure ClothSpring.Init(PID1, PID2: Integer; Len: Single);
Begin
  Self.ball1 := PID1;
  Self.ball2 := PID2;
  Self.naturalLength := Len;
  Self.springConstant := DefaultSpringConstant;

  _System._Particles[PID1].AddSpring(_Index);
  _System._Particles[PID2].AddSpring(_Index);
End;

{ ClothParticle }
Procedure ClothParticle.AddSpring(Index: Integer);
Begin
  Inc(SpringCount);
  SetLength(Springs, SpringCount);
  Springs[Pred(SpringCount)] := Index;
End;

{ ClothSystem }
Constructor ClothSystem.Create;
Var
  I:Integer;
Begin
  _lastTime := 0;
  _timeSinceLastUpdate := 0;

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

  For I:=0 To Pred(_SpringCount) Do
  Begin
    _Springs[I]._Index := I;
    _Springs[I]._System := Self;
  End;

  InitMesh();

	//Reset cloth
	Self.Reset();
End;

Procedure ClothSystem.InitMesh();
Var
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
End;

Procedure ClothSystem.Reset();
Var
  I, J, K:Integer;
  BallID, X,Y:Integer;
  currentSpring:Integer;

  naturalLength:Single;
Begin
	//Initialise the Particles in an evenly spaced grid in the x-z plane
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
			_Particles[BallID].CurrentPosition := VectorCreate(ClothScale * (I/Pred(gridSize)), 8.5, ClothScale * (J/ Pred(GridSize)));
			_Particles[BallID].CurrentVelocity := VectorZero;

			_Particles[BallID].InverseMass := 1 / Mass;
			_Particles[BallID].fixed := false;
		End;

  NaturalLength := ClothScale * (1/Pred(gridSize));

	//Fix the top left & top right Particles in place
	_Particles[0].fixed := true;
	_Particles[gridSize-1].fixed :=true;

	//Fix the bottom left & bottom right Particles
	_Particles[gridSize*(gridSize-1)].fixed := true;
	_Particles[gridSize*gridSize-1].fixed := true;

	//Initialise the springs
	currentSpring := 0;

	//The first (gridSize-1)*gridSize springs go from one ball to the next,
	//excluding those on the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+1, naturalLength);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*gridSize springs go from one ball to the one below,
	//excluding those on the bottom edge
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize) Do
    Begin
			_Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I, naturalLength);
			Inc(currentSpring);
    End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and right
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I+1, naturalLength*sqrt(2.0));
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and left
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+1)*gridSize+I-1, naturalLength*sqrt(2.0));
			Inc(currentSpring);
		End;

	//The first (gridSize-2)*gridSize springs go from one ball to the next but one,
	//excluding those on or next to the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-2) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, J*gridSize+I+2, naturalLength*2);
			Inc(currentSpring);
		End;


	//The next (gridSize-2)*gridSize springs go from one ball to the next but one below,
	//excluding those on or next to the bottom edge
  For J:=0 To Pred(gridSize-2) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      _Springs[CurrentSpring].Init(J*gridSize+I, (J+2)*gridSize+I, naturalLength*2);
			Inc(currentSpring);
		End;


  UpdateMesh();
End;

Procedure ClothSystem.UpdateMesh();
Var
  I, J, BallID, Index, X, Y:Integer;
  normal:Vector3D;
Begin
  _Group.CalculateTriangleNormals();

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

        //Normal := VectorUp;
      Normal.Normalize();
      _Group.Vertices.SetVector3D(BallID, vertexNormal, Normal);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      _Group.Vertices.SetVector3D(BallID, vertexPosition, _Particles[BallID].CurrentPosition);
      _Group.Vertices.SetVector2D(BallID, vertexUV0, VectorCreate2D(I/GridSize, J/GridSize));
      _Group.Vertices.SetColor(BallID, vertexColor, ColorWhite);
    End;

  _Group.ReleaseBuffer();
  _Mesh.UpdateBoundingBox();
End;

Procedure ClothSystem.Simulate;
Var
  currentTime, timePassed:Cardinal;
  updateMade:Boolean;
  timePassedInSeconds:Single;
  I, J, K:Integer;
  springLength, Extension:Single;
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
			springLength := _Particles[_Springs[i].ball1].CurrentPosition.Distance(_Particles[_Springs[i].ball2].CurrentPosition);

			extension := springLength - _Springs[i].naturalLength;

			_Springs[i].tension := (_Springs[i].springConstant*extension) / _Springs[i].naturalLength;
		End;

		//Calculate the nextParticles from the currentParticles
		For I:=0 To Pred(_ParticleCount) Do
		Begin
			//If the ball is fixed, transfer the position and zero the velocity, otherwise calculate
			//the new values
			If (_Particles[i].fixed) Then
			Begin
				_Particles[i].NextPosition := _Particles[i].CurrentPosition;
				_Particles[i].NextVelocity := VectorZero;

        If MoveCloth Then
          _Particles[i].NextPosition.Add(VectorCreate(0, 2 * timePassedInSeconds, 5 * timePassedInSeconds));
			End Else
			Begin
				//Calculate the force on this ball

        Force := gravity;

				//Loop through springs
        //For J:=0 To Pred(_SpringCount) Do
        For K:=0 To Pred(_Particles[I].SpringCount) Do
				Begin
          J := _Particles[I].Springs[K];
					//If this ball is "ball1" for this spring, add the tension to the force
					If(_springs[j].ball1 = i)  Then
					Begin
						tensionDirection :=	VectorSubtract(_Particles[_springs[j].ball2].CurrentPosition, _Particles[i].CurrentPosition);
						tensionDirection.Normalize();

            tensionDirection.Scale(_springs[j].tension);
						force.Add(tensionDirection);
					End;

					//Similarly if the ball is "ball2"
					if(_springs[j].ball2=i) Then
					Begin
						tensionDirection :=	VectorSubtract(_Particles[_springs[j].ball1].CurrentPosition, _Particles[i].CurrentPosition);
						tensionDirection.Normalize();

            tensionDirection.Scale(_springs[j].tension);
						force.Add(tensionDirection);
					End;
        End;

				//Calculate the acceleration
				acceleration := VectorScale(force, _Particles[i].InverseMass);

				//Update velocity
				_Particles[i].NextVelocity := VectorAdd(_Particles[i].CurrentVelocity, VectorScale(acceleration, timePassedInSeconds));

				//Damp the velocity
				_Particles[i].NextVelocity.Scale(dampFactor);

				//Calculate new position
        //Force := VectorAdd(VectorScale(Particles[i].NextVelocity, 0.5), VectorScale(Particles[i].CurrentVelocity, 0.5));
        Force := _Particles[i].NextVelocity;

        Force.Scale(timePassedInSeconds);
				_Particles[i].NextPosition := VectorAdd(_Particles[i].CurrentPosition, Force);

				//Check against sphere (at origin)
        P := VectorSubtract(_Particles[i].NextPosition, Sphere.Position);
				if (P.LengthSquared < Sqr(sphereRadius*1.08)) Then
        Begin
					P.Normalize();
          P.Scale(sphereRadius*1.08);
          _Particles[i].NextPosition := VectorAdd(P, Sphere.Position);
        End;

				//Check against floor
				if(_Particles[i].NextPosition.y <= -sphereRadius * 0.92) Then
        Begin
					_Particles[i].NextPosition.y := -sphereRadius * 0.92;
          _Particles[i].NextVelocity.y := 0;
        End;

			End;
		End;

		//Swap the currentParticles and newParticles pointers
		For I:=0 To Pred(_ParticleCount) Do
    Begin
      _Particles[i].CurrentPosition := _Particles[i].NextPosition;
			_Particles[i].CurrentVelocity := _Particles[i].NextVelocity;
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

