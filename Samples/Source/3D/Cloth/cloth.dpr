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


Var
  Solid:MeshInstance;

  ClothTex:Texture;
  DiffuseTex:Texture;

  Sun:DirectionalLight;

  Cloth:MeshInstance;

  Fnt:Font;

Const
  MinimumDelta = 10;

  SphereRadius = 4.0;

  ClothScale = 1.0;

  Gravity:Vector3d = (X:0.0; Y:-0.98; Z:0.0);

//Values given to each spring
  springConstant = 15.0;
  naturalLength = 1.0;

//Values given to each ball
  mass =0.01;

//Damping factor. Velocities are multiplied by this
  dampFactor=0.9;

//Grid complexity. This is the number of balls across and down in the model
  gridSize=13;
  
Type
  BALL = Record
	  position:VECTOR3D;
  	velocity:VECTOR3D;

	  mass:Single;

	//Is this ball held in position?
	  fixed:Boolean;

  	//Vertex normal for this ball
	  normal:VECTOR3D ;
  End;

  SPRING = Record
  	//Indices of the balls at either end of the spring
	  ball1, ball2:Integer;

  	//Tension in the spring
	  tension:Single;

	  springConstant:Single;
  	naturalLength:Single;
  End;

Type
  BallArray = Array Of Ball;
  PBallArray = ^BallArray;

Var
//Array of springs
  numSprings:Integer;
  springs:Array Of SPRING;

//What do we want to draw?
drawBalls, drawSprings, drawTriangles:Boolean;

//Floor texture
floorTexture:Texture;

numBalls:Integer;
balls1,  balls2:BallArray;

//Pointers to the arrays. One holds the balls for the current frame, and one holds those
//for the next frame
currentBalls, nextBalls:PBallArray;

lastTime:Cardinal;
timeSinceLastUpdate:Cardinal;

  ClothMesh:Mesh;
  ClothGroup:MeshGroup;
  ClothVertexFormat:VertexFormat;

{ Game }
Procedure Demo.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);
  GraphicsManager.Instance.Renderer.Settings.PostProcessing.SetValue(True);

  DiffuseTex := TextureManager.Instance.GetTexture('cobble');
  ClothTex := TextureManager.Instance.GetTexture('cloth_diffuse');


  Solid := MeshInstance.Create(MeshManager.Instance.SphereMesh);
  Solid.SetDiffuseMap(0, DiffuseTex);
  Solid.SetPosition(VectorCreate(0, 0, 0));
  Solid.SetScale(VectorConstant(SphereRadius));

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
  ReleaseObject(Solid);
End;

Procedure Demo.OnIdle;
Var
  currentTime, timePassed:Cardinal;
  updateMade:Boolean;
  timePassedInSeconds:Single;
  I, J:Integer;
  springLength, Extension:Single;
  force, acceleration, tensionDirection:VECTOR3D;
  Temp:PBallArray;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();
  _Scene.Main.Camera.FreeCam;


	//Release corners
	If(InputManager.Instance.Keys.WasPressed(Ord('1'))) Then
		currentBalls^[0].fixed := False;

	If(InputManager.Instance.Keys.WasPressed(Ord('2'))) Then
		currentBalls^[gridSize-1].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('3'))) Then
		currentBalls^[gridSize*(gridSize-1)].fixed := false;

	If(InputManager.Instance.Keys.WasPressed(Ord('4'))) Then
		currentBalls^[gridSize*gridSize-1].fixed := false;

	//Toggle drawing modes
	If(InputManager.Instance.Keys.WasPressed(Ord('B'))) Then
	Begin
		drawBalls := Not drawBalls;
	End;

	If(InputManager.Instance.Keys.WasPressed(Ord('S'))) Then
	Begin
		drawSprings := Not drawSprings;
	End;

	If(InputManager.Instance.Keys.WasPressed(Ord('R'))) Then
	Begin
	//Reset cloth
		_Scene.ResetCloth();
  End;

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
			springLength := currentBalls^[springs[i].ball1].position.Distance(currentBalls^[springs[i].ball2].position);

			extension := springLength - springs[i].naturalLength;

			springs[i].tension := (springs[i].springConstant*extension)/springs[i].naturalLength;
		End;

		//Calculate the nextBalls from the currentBalls
		For I:=0 To Pred(numBalls) Do
		Begin
			//Transfer properties which do not change
			nextBalls^[i].fixed := currentBalls^[i].fixed;
			nextBalls^[i].mass := currentBalls^[i].mass;

			//If the ball is fixed, transfer the position and zero the velocity, otherwise calculate
			//the new values
			if(currentBalls^[i].fixed) Then
			Begin
				nextBalls^[i].position := currentBalls^[i].position;
				nextBalls^[i].velocity := VectorZero;
			End Else
			Begin
				//Calculate the force on this ball
				Force := gravity;

				//Loop through springs
        For J:=0 To Pred(numSprings) Do
				Begin
					//If this ball is "ball1" for this spring, add the tension to the force
					If(springs[j].ball1 = i)  Then
					Begin
						tensionDirection :=	VectorSubtract(currentBalls^[springs[j].ball2].position, currentBalls^[i].position);
						tensionDirection.Normalize();

            tensionDirection.Scale(springs[j].tension);
						force.Add(tensionDirection);
					End;

					//Similarly if the ball is "ball2"
					if(springs[j].ball2=i) Then
					Begin
						tensionDirection :=	VectorSubtract(currentBalls^[springs[j].ball1].position, currentBalls^[i].position);
						tensionDirection.Normalize();

            tensionDirection.Scale(springs[j].tension);
						force.Add(tensionDirection);
					End;
        End;

				//Calculate the acceleration
				acceleration := VectorScale(force, 1/currentBalls^[i].mass);

				//Update velocity
				nextBalls^[i].velocity := VectorAdd(currentBalls^[i].velocity, VectorScale(acceleration, timePassedInSeconds));

				//Damp the velocity
				nextBalls^[i].velocity.Scale(dampFactor);

				//Calculate new position
        Force := VectorAdd(nextBalls^[i].velocity, currentBalls^[i].velocity);
        Force.Scale(timePassedInSeconds/2);
				nextBalls^[i].position := VectorAdd(currentBalls^[i].position, Force);

				//Check against sphere (at origin)
				if (nextBalls^[i].position.LengthSquared < Sqr(sphereRadius*1.08)) Then
        Begin
					nextBalls^[i].position.Normalize();
          nextBalls^[i].position.Scale(sphereRadius*1.08);
        End;

				//Check against floor
				if(nextBalls^[i].position.y<-sphereRadius) Then
					nextBalls^[i].position.y := -sphereRadius;
			End;
		End;

		//Swap the currentBalls and newBalls pointers
		temp := currentBalls;
		currentBalls := nextBalls;
		nextBalls := currentBalls;
	End;

	//Calculate the normals if we have updated the positions
	If(updateMade) Then
    _Scene.UpdateClothMesh();

End;


{ MyScene }
Constructor MyScene.Create;
Begin
  Sky := Skybox.Create('sky');

  Main := GraphicsManager.Instance.CreateMainViewport('main', GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  Main.Camera.SetPosition(VectorCreate(0, SphereRadius, SphereRadius * 6));

	//Calculate number of balls
	numBalls := gridSize*gridSize;
		
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

	//Create space for balls & springs
	SetLength(balls1, numBalls);
	SetLength(balls2, numBalls);
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
  currentSpring:^Spring;
  T:Triangle;
Begin
	//Initialise the balls in an evenly spaced grid in the x-z plane
  For I:=0 To Pred(gridSize) Do
    For J:=0 To Pred(gridSize) Do
		Begin
			balls1[i*gridSize+j].position := VectorCreate(ClothScale * (j - (gridSize-1)/2), 8.5, ClothScale * (i- (gridSize-1)/2));
			balls1[i*gridSize+j].velocity := VectorZero;
			balls1[i*gridSize+j].mass := mass;
			balls1[i*gridSize+j].fixed := false;
		End;

	//Fix the top left & top right balls in place
	balls1[0].fixed := true;
	balls1[gridSize-1].fixed :=true;

	//Fix the bottom left & bottom right balls
	balls1[gridSize*(gridSize-1)].fixed := true;
	balls1[gridSize*gridSize-1].fixed := true;

	//Copy the balls into the other array
  For I:=0 To Pred(numBalls) Do
		balls2[i] := balls1[i];

	//Set the currentBalls and nextBalls pointers
	currentBalls := @balls1;
	nextBalls := @balls2;

	//Initialise the springs
	currentSpring := @springs[0];

	//The first (gridSize-1)*gridSize springs go from one ball to the next,
	//excluding those on the right hand edge
  For I:=0 To Pred(gridSize) Do
    For J:=0 To Pred(gridSize-1) Do
		Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := i*gridSize+j+1;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength;

			Inc(currentSpring);
		End;

	//The next (gridSize-1)*gridSize springs go from one ball to the one below,
	//excluding those on the bottom edge
  For I:=0 To Pred(gridSize-1) Do
    For J:=0 To Pred(gridSize) Do
    Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := (i+1)*gridSize+j;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength;

			Inc(currentSpring);
    End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and right
	//excluding those on the bottom or right
  For I:=0 To Pred(gridSize-1) Do
    For J:=0 To Pred(gridSize-1) Do
		Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := (i+1)*gridSize+j+1;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength*sqrt(2.0);

			Inc(currentSpring);
		End;

	//The next (gridSize-1)*(gridSize-1) go from a ball to the one below and left
	//excluding those on the bottom or right
  For I:=0 To Pred(gridSize-1) Do
    For J:=1 To Pred(gridSize) Do
		Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := (i+1)*gridSize+j-1;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength*sqrt(2.0);

			Inc(currentSpring);
		End;

	//The first (gridSize-2)*gridSize springs go from one ball to the next but one,
	//excluding those on or next to the right hand edge
  For I:=0 To Pred(gridSize) Do
    For J:=0 To Pred(gridSize-2) Do
		Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := i*gridSize+j+2;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength*2;

			Inc(currentSpring);
		End;


	//The next (gridSize-2)*gridSize springs go from one ball to the next but one below,
	//excluding those on or next to the bottom edge
  For I:=0 To Pred(gridSize-2) Do
    For J:=0 To Pred(gridSize) Do
		Begin
			currentSpring.ball1 := i*gridSize+j;
			currentSpring.ball2 := (i+2)*gridSize+j;

			currentSpring.springConstant := springConstant;
			currentSpring.naturalLength := naturalLength*2;

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
  For I:=0 To Pred(gridSize-1) Do
    For J:=1 To Pred(gridSize-1) Do
		Begin
      T.Indices[0] := i*gridSize+j;
      T.Indices[1] := i*gridSize+j+1;
      T.Indices[2] := (i+1)*gridSize+j;
      ClothGroup.SetTriangle(T, K); Inc(K);

      T.Indices[0] := (i+1)*gridSize+j;
      T.Indices[1] := (i+1)*gridSize+j+1;
      T.Indices[2] := i*gridSize+j+1;
      ClothGroup.SetTriangle(T, K); Inc(K);
    End;

  UpdateClothMesh();

  ReleaseObject(Cloth);
  Cloth := MeshInstance.Create(ClothMesh);
End;

Procedure MyScene.UpdateClothMesh();
Var
  I, J:Integer;
Begin
  For I:=0 To Pred(gridSize) Do
    For J:=1 To Pred(gridSize) Do
		Begin
      ClothGroup.Vertices.SetVector3D(i*gridSize+j, vertexPosition, currentBalls^[i*gridSize+j].position);
      ClothGroup.Vertices.SetVector3D(i*gridSize+j, vertexNormal, VectorUp); //currentBalls^[i*gridSize+j+1].normal);
      ClothGroup.Vertices.SetVector2D(i*gridSize+j, vertexUV0, VectorCreate2D(I/GridSize, J/GridSize)); //currentBalls^[i*gridSize+j+1].normal);
      ClothGroup.Vertices.SetColor(i*gridSize+j, vertexColor, ColorWhite);
    End;

    (*
		//Zero the normals on each ball
		for(int i=0; i<numBalls; ++i)
			currentBalls[i].normal.LoadZero();

		//Calculate the normals on the current balls
		for(int i=0; i<gridSize-1; ++i)
			for(int j=0; j<gridSize-1; ++j)
			Begin
				VECTOR3D & p0=currentBalls[i*gridSize+j].position;
				VECTOR3D & p1=currentBalls[i*gridSize+j+1].position;
				VECTOR3D & p2=currentBalls[(i+1)*gridSize+j].position;
				VECTOR3D & p3=currentBalls[(i+1)*gridSize+j+1].position;

				VECTOR3D & n0=currentBalls[i*gridSize+j].normal;
				VECTOR3D & n1=currentBalls[i*gridSize+j+1].normal;
				VECTOR3D & n2=currentBalls[(i+1)*gridSize+j].normal;
				VECTOR3D & n3=currentBalls[(i+1)*gridSize+j+1].normal;

				//Calculate the normals for the 2 triangles and add on
				VECTOR3D normal=(p1-p0).CrossProduct(p2-p0);

				n0+=normal;
				n1+=normal;
				n2+=normal;

				normal=(p1-p2).CrossProduct(p3-p2);

				n1+=normal;
				n2+=normal;
				n3+=normal;
			End;

		//Normalize normals
		for(int i=0; i<numBalls; ++i)
			currentBalls[i].normal.Normalize();
      *)
    
  ClothGroup.ReleaseBuffer();
  ClothMesh.UpdateBoundingBox();
End;

Procedure MyScene.RenderSprites;
Begin
End;

Procedure MyScene.RenderViewport(V:Viewport);
Begin
  LightManager.Instance.AddLight(Sun);
  GraphicsManager.Instance.AddRenderable(Solid);
  GraphicsManager.Instance.AddRenderable(Cloth);
End;


Procedure MyScene.RenderSky;
Begin
  Sky.Render;
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

