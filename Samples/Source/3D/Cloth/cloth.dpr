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

      Procedure ResetCloth();
      Procedure SimulateCloth();
      Procedure UpdateClothMesh();

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
  ClothSpring = Object
  	//Indices of the Particles at either end of the spring
	  ball1, ball2:Integer;

  	//Tension in the spring
	  tension:Single;

	  springConstant:Single;
  	naturalLength:Single;

    Procedure Init(Index, PID1, PID2: Integer; Len: Single);
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

Var
//Array of springs
  numSprings:Integer;
  springs:Array Of ClothSpring;

  //What do we want to draw?
  drawParticles, drawSprings, drawTriangles:Boolean;

  //Floor texture
  floorTex:Texture;
  Floor:MeshInstance;

  numParticles:Integer;
  Particles:Array Of ClothParticle;

  lastTime:Cardinal;
  timeSinceLastUpdate:Cardinal;

  ClothMesh:Mesh;
  ClothGroup:MeshGroup;
  ClothVertexFormat:VertexFormat;

  MoveCloth:Boolean = False;


  ClothTex:Texture;
  DiffuseTex:Texture;

  Sun:DirectionalLight;

  Sphere:MeshInstance;
  Cloth:MeshInstance;

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

  lastTime := 0;
  timeSinceLastUpdate := 0;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Scene);
  ReleaseObject(Sun);
  ReleaseObject(Sphere);
  ReleaseObject(Floor);
  ReleaseObject(Cloth);

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
		Particles[0].fixed := False;

	If(InputManager.Instance.Keys.WasPressed(Ord('2'))) Then
		Particles[gridSize-1].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('3'))) Then
		Particles[gridSize*(gridSize-1)].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('4'))) Then
		Particles[gridSize*gridSize-1].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('5'))) Then
  Begin
		Particles[gridSize*(gridSize-1)].fixed := false;
		Particles[gridSize*gridSize-1].fixed := false;
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
		_Scene.ResetCloth();
  End;


  _Scene.SimulateCloth();
End;


{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');

  Main := GraphicsManager.Instance.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  Main.Camera.SetPosition(VectorCreate(0, SphereRadius, SphereRadius * 6));

	//Calculate number of Particles
	numParticles := gridSize*gridSize;

	//Calculate number of springs
	//There is a spring pointing right for each ball which is not on the right edge, 
	//and one pointing down for each ball not on the bottom edge
	numSprings := (gridSize-1)*gridSize*2;

	//There is a spring pointing down & right for each ball not on bottom or right,
	//and one pointing down & left for each ball not on bottom or left
	Inc(numSprings, (gridSize-1)*(gridSize-1)*2);

	//There is a spring pointing right (to the next but one ball)
	//for each ball which is not on or next to the right edge,
	//and one pointing down for each ball not on or next to the bottom edge
	Inc(numSprings, (gridSize-2)*gridSize*2);

	//Create space for Particles & springs
	SetLength(Particles, numParticles);
	SetLength(springs, numSprings);

	//Reset cloth
	ResetCloth();
End;

Procedure MyScene.Release;
Begin
//  ReleaseObject(Main);
  ReleaseObject(Sky);
End;

Procedure MyScene.ResetCloth();
Var
  I, J, K:Integer;
  BallID, X,Y:Integer;
  currentSpring:Integer;
  i0, i1, i2, i3:Integer;
  T:Triangle;

  naturalLength:Single;
Begin
	//Initialise the Particles in an evenly spaced grid in the x-z plane
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
			Particles[BallID].CurrentPosition := VectorCreate(ClothScale * (I/Pred(gridSize)), 8.5, ClothScale * (J/ Pred(GridSize)));
			Particles[BallID].CurrentVelocity := VectorZero;

			Particles[BallID].InverseMass := 1 / Mass;
			Particles[BallID].fixed := false;
		End;

  NaturalLength := ClothScale * (1/Pred(gridSize));

	//Fix the top left & top right Particles in place
	Particles[0].fixed := true;
	Particles[gridSize-1].fixed :=true;

	//Fix the bottom left & bottom right Particles
	Particles[gridSize*(gridSize-1)].fixed := true;
	Particles[gridSize*gridSize-1].fixed := true;

	//Initialise the springs
	currentSpring := 0;

	//The first (gridSize-1)*gridSize springs go from one ball to the next,
	//excluding those on the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
			springs[currentSpring].Init(currentSpring, J*gridSize+I, J*gridSize+I+1, naturalLength);
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*gridSize springs go from one ball to the one below,
	//excluding those on the bottom edge
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize) Do
    Begin
			springs[currentSpring].Init(currentSpring, J*gridSize+I, (J+1)*gridSize+I, naturalLength);
			Inc(currentSpring);
    End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and right
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=0 To Pred(gridSize-1) Do
		Begin
      springs[currentSpring].Init(currentSpring, J*gridSize+I, (J+1)*gridSize+I+1, naturalLength*sqrt(2.0));
			Inc(currentSpring);
		End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and left
	//excluding those on the bottom or right
  For J:=0 To Pred(gridSize-1) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      springs[currentSpring].Init(currentSpring, J*gridSize+I, (J+1)*gridSize+I-1, naturalLength*sqrt(2.0));
			Inc(currentSpring);
		End;

	//The first (gridSize-2)*gridSize springs go from one ball to the next but one,
	//excluding those on or next to the right hand edge
  For J:=0 To Pred(gridSize) Do
    For I:=0 To Pred(gridSize-2) Do
		Begin
      springs[currentSpring].Init(currentSpring, J*gridSize+I, J*gridSize+I+2, naturalLength*2);
			Inc(currentSpring);
		End;


	//The next (gridSize-2)*gridSize springs go from one ball to the next but one below,
	//excluding those on or next to the bottom edge
  For J:=0 To Pred(gridSize-2) Do
    For I:=0 To Pred(gridSize) Do
		Begin
      springs[currentSpring].Init(currentSpring, J*gridSize+I, (J+2)*gridSize+I, naturalLength*2);
			Inc(currentSpring);
		End;

  ReleaseObject(ClothMesh);
  ClothMesh := Mesh.Create(rtDynamic, 'cloth');

  ClothVertexFormat := [vertexFormatPosition, vertexFormatNormal, vertexFormatColor, vertexFormatUV0];

  ClothGroup := ClothMesh.AddGroup(ClothVertexFormat);
  ClothGroup.DiffuseMap := ClothTex;
  ClothGroup.Flags := ClothGroup.Flags Or meshGroupDoubleSided;

  ClothGroup.TriangleCount := (gridSize * gridSize) * 2;
  ClothGroup.VertexCount := (gridSize * gridSize);

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
      ClothGroup.SetTriangle(T, K); Inc(K);

      T.Indices[0] := i2;
      T.Indices[1] := i3;
      T.Indices[2] := i1;
      ClothGroup.SetTriangle(T, K); Inc(K);
    End;

  UpdateClothMesh();

  ReleaseObject(Cloth);
  Cloth := MeshInstance.Create(ClothMesh);
End;

Procedure MyScene.UpdateClothMesh();
Var
  I, J, BallID, Index, X, Y:Integer;
  normal:Vector3D;
Begin
  ClothGroup.CalculateTriangleNormals();

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
          Normal.Add(ClothGroup.GetTriangleNormal(Index));

          Inc(Index);
          Normal.Add(ClothGroup.GetTriangleNormal(Index));
        End;

        //Normal := VectorUp;
      Normal.Normalize();
      ClothGroup.Vertices.SetVector3D(BallID, vertexNormal, Normal);
    End;

  For J:=0 To Pred(gridSize) Do
    For I:=1 To Pred(gridSize) Do
		Begin
      BallID := J*gridSize+I;
      ClothGroup.Vertices.SetVector3D(BallID, vertexPosition, Particles[BallID].CurrentPosition);
      ClothGroup.Vertices.SetVector2D(BallID, vertexUV0, VectorCreate2D(I/GridSize, J/GridSize));
      ClothGroup.Vertices.SetColor(BallID, vertexColor, ColorWhite);
    End;

  ClothGroup.ReleaseBuffer();
  ClothMesh.UpdateBoundingBox();
End;

Procedure MyScene.SimulateCloth();
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
  If lastTime=0 Then
  Begin
    lastTime := Application.GetTime();
    Exit;
  End;

	currentTime := Application.GetTime();
	timePassed := currentTime - lastTime;
	lastTime := currentTime;

	//Update the physics in intervals of 10ms to prevent problems
	//with different frame rates causing different damping
	Inc(timeSinceLastUpdate, timePassed);

	updateMade := false;	//did we update the positions etc this time?

	while (timeSinceLastUpdate>MinimumDelta) Do
	Begin
		Dec(timeSinceLastUpdate, MinimumDelta);
		timePassedInSeconds := 1.0/ (1000/MinimumDelta);
		updateMade := True;

		//Calculate the tensions in the springs
		For I:=0 To Pred(numSprings) Do
		Begin
			springLength := Particles[springs[i].ball1].CurrentPosition.Distance(Particles[springs[i].ball2].CurrentPosition);

			extension := springLength - springs[i].naturalLength;

			springs[i].tension := (springs[i].springConstant*extension)/springs[i].naturalLength;
		End;

		//Calculate the nextParticles from the currentParticles
		For I:=0 To Pred(numParticles) Do
		Begin
			//If the ball is fixed, transfer the position and zero the velocity, otherwise calculate
			//the new values
			if(Particles[i].fixed) Then
			Begin
				Particles[i].NextPosition := Particles[i].CurrentPosition;
				Particles[i].NextVelocity := VectorZero;

        If MoveCloth Then
          Particles[i].NextPosition.Add(VectorCreate(0, 2 * timePassedInSeconds, 5 * timePassedInSeconds));
			End Else
			Begin
				//Calculate the force on this ball

        Force := gravity;

				//Loop through springs
        //For J:=0 To Pred(numSprings) Do
        For K:=0 To Pred(Particles[I].SpringCount) Do
				Begin
          J := Particles[I].Springs[K];
					//If this ball is "ball1" for this spring, add the tension to the force
					If(springs[j].ball1 = i)  Then
					Begin
						tensionDirection :=	VectorSubtract(Particles[springs[j].ball2].CurrentPosition, Particles[i].CurrentPosition);
						tensionDirection.Normalize();

            tensionDirection.Scale(springs[j].tension);
						force.Add(tensionDirection);
					End;

					//Similarly if the ball is "ball2"
					if(springs[j].ball2=i) Then
					Begin
						tensionDirection :=	VectorSubtract(Particles[springs[j].ball1].CurrentPosition, Particles[i].CurrentPosition);
						tensionDirection.Normalize();

            tensionDirection.Scale(springs[j].tension);
						force.Add(tensionDirection);
					End;
        End;

				//Calculate the acceleration
				acceleration := VectorScale(force, Particles[i].InverseMass);

				//Update velocity
				Particles[i].NextVelocity := VectorAdd(Particles[i].CurrentVelocity, VectorScale(acceleration, timePassedInSeconds));

				//Damp the velocity
				Particles[i].NextVelocity.Scale(dampFactor);

				//Calculate new position
        //Force := VectorAdd(VectorScale(Particles[i].NextVelocity, 0.5), VectorScale(Particles[i].CurrentVelocity, 0.5));
        Force := Particles[i].NextVelocity;

        Force.Scale(timePassedInSeconds);
				Particles[i].NextPosition := VectorAdd(Particles[i].CurrentPosition, Force);

				//Check against sphere (at origin)
        P := VectorSubtract(Particles[i].NextPosition, Sphere.Position);
				if (P.LengthSquared < Sqr(sphereRadius*1.08)) Then
        Begin
					P.Normalize();
          P.Scale(sphereRadius*1.08);
          Particles[i].NextPosition := VectorAdd(P, Sphere.Position);
        End;

				//Check against floor
				if(Particles[i].NextPosition.y <= -sphereRadius * 0.92) Then
        Begin
					Particles[i].NextPosition.y := -sphereRadius * 0.92;
          Particles[i].NextVelocity.y := 0;
        End;

			End;
		End;

		//Swap the currentParticles and newParticles pointers
		For I:=0 To Pred(numParticles) Do
    Begin
      Particles[i].CurrentPosition := Particles[i].NextPosition;
			Particles[i].CurrentVelocity := Particles[i].NextVelocity;
    End;
	End;

	//Calculate the normals if we have updated the positions
	If(updateMade) Then
    Self.UpdateClothMesh();
End;

Procedure MyScene.RenderSprites;
Begin
  Text.DrawText(10, 10, 10, 'FPS :'+IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
  Text.DrawText(10, 30, 10, 'Vertices :'+IntToString(ClothGroup.Vertices.Count));
  Text.DrawText(10, 50, 10, 'Sprints :'+IntToString(NumSprings));
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  If Assigned(Cloth) Then
    Cloth.SetWireframeMode(0, drawSprings);

  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(Sphere);
  GraphicsManager.Instance.AddRenderable(Floor);
  GraphicsManager.Instance.AddRenderable(Cloth);
End;


Procedure MyScene.RenderSky;
Begin
  Sky.Render;
End;

{ ClothSpring }
Procedure ClothSpring.Init(Index, PID1, PID2: Integer; Len: Single);
Begin
  Self.ball1 := PID1;
  Self.ball2 := PID2;
  Self.naturalLength := Len;
  Self.springConstant := DefaultSpringConstant;

  Particles[PID1].AddSpring(Index);
  Particles[PID2].AddSpring(Index);
End;

{ ClothParticle }
Procedure ClothParticle.AddSpring(Index: Integer);
Begin
  Inc(SpringCount);
  SetLength(Springs, SpringCount);
  Springs[Pred(SpringCount)] := Index;
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

