{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} GridPathfinding;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_Application, TERRA_Scene, TERRA_UI, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_FileManager,
  TERRA_PNG, TERRA_TTF, TERRA_Viewport, TERRA_SpriteManager, TERRA_Texture,
  TERRA_InputManager, TERRA_AIGridPath;

Const
  // Size of map tiles and objects
  TileSize = 24;
  // These values are added to map and objects coordinates
  // Used to center them in the screen
  MapOffsetX = 0;
  MapOffsetY = 0;

  // Number of vertical/horizontal tiles
  MapSize = 10;

  // Mapa tiles
  // 0 is empty, 1 is solid, 2 is node
  MapData:Array[0..Pred(MapSize),0..Pred(MapSize)] Of Byte=
          ( (1,1,1,1,1,1,1,1,1,1),
            (1,0,0,0,1,0,0,1,0,1),
            (1,0,1,1,0,0,1,1,0,1),
            (1,0,1,0,0,1,0,0,0,1),
            (1,0,0,0,1,0,0,1,0,1),
            (1,0,1,1,0,0,0,1,0,1),
            (1,0,0,1,1,0,1,1,0,1),
            (1,0,1,1,0,0,0,1,0,1),
            (1,0,0,0,0,0,1,0,0,1),
            (1,1,1,1,1,1,1,1,1,1)
          );

Type
  // This is our pathfinder object.
  // You must implement some of Pathfinders methods.
  // In the Pathfinder case, just two methods are needed.

  MyPathFinder=Class(GridPathFinder)
    Protected
      // The GetCost returns the cost of traveling from (PX,PY) to (X,Y)
      // In normal cases, this should be the distance between the two points
      Function GetCost(X,Y:Integer):Double; Override;

      //  The visit node is a sort of callback, that get called each time
      //  a node is visited during the search.
      //  This function is optional, and you don't need to override it,
      //  altough its very useful for debugging your pathfinder GetCost method.
      Procedure VisitNode(X,Y:Integer);Override;
  End;


  // A client is used to process application events
  Demo = Class(Application)
    Protected
      _Scene:Scene;

      // Our pathfinder object
      _Pathfinder:MyPathfinder;

			Procedure OnCreate; Override;
      Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;

      Procedure SelectResolution2D(Var Width, Height:Integer; Var Scale:Single); Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Var
  // Some textures needed by our objects
  GhostTex,BlockTex,ArrowTex,CrossTex, DotTex:Texture;

  // Ghost variables
  GhostX,GhostY:Integer;
  GhostUpdateTime:Cardinal;
  GhostPath:GridPath;
  GhostPosition:Integer;

  // Target variables
  TargetX,TargetY:Integer;

  // This is used to mark the tiles visited during the search
  MapVisited:Array[0..Pred(MapSize),0..Pred(MapSize)] Of Boolean;


// This is our implementation of the Pathfinder GetCost method
// Note - If the node (X,Y) shouldn't be visited, then this function need to return -1
//        In our case, this happen when a node is outside the map, or is a solid wall.
Function MyPathFinder.GetCost(X,Y:Integer):Double;
Begin
  // Verify if the map is within boundaries
  If (X>=0)And(X<MapSize)And(Y>=0)And(Y<MapSize) Then
  Begin
    // Verify if the tile is solid
    If (MapData[Y,X]<>1) Then
      Result := Sqrt(Sqr(X-_TargetX)+Sqr(Y - _TargetY))  // Return distance between the two points
    Else
      Result := -1; // Tile is solid, don't visit it
  End Else
    Result := -1;   // Tile doesnt exist
End;

//  Our implementation of the Pathfinder VisitNode method
//  This is called each time a node is visited, and we can do whatever we want with it.
//  However don't try anything complex inside this, this code should run as fast as possible.
//  You application will be blocked during this code execution, so mouse events and
//  other stuff like that won't happen.
Procedure MyPathFinder.VisitNode(X,Y:Integer);
Begin
  // Mark this tile as visited
  MapVisited[Y,X] := True;

  // Redraw the demo
//  Tutorial.DrawStuff;
End;

{ Game }
Procedure Demo.OnCreate;
Begin
  // Add asset folders
  FileManager.Instance.AddPath('assets');

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorGrey(128);

  GhostTex := TextureManager.Instance.GetTexture('ghost');
  BlockTex := TextureManager.Instance.GetTexture('block');
  ArrowTex := TextureManager.Instance.GetTexture('arrows');
  CrossTex := TextureManager.Instance.GetTexture('cross');
  DotTex := TextureManager.Instance.GetTexture('dot');

  // Init Ghost position
  GhostX := 5;
  GhostY := 5;

  TargetX := GhostX;
  TargetY := GhostY;

  // Create pathfinder object
  _Pathfinder := MyPathfinder.Create();

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnDestroy;
Begin
  // Destroy pathfinder object
  ReleaseObject(_Pathfinder);

  ReleaseObject(_Scene);
End;

{ MyScene }
Procedure MyScene.RenderSprites(V:Viewport);
Var
  S:QuadSprite;
  I,J:Integer;
  Mouse:MouseCursor;
  Node:GridPathNode;
Begin
  //  Draw mouse cursor
  Mouse  := InputManager.Instance.Mouse;
  S := SpriteManager.Instance.DrawSprite(Mouse.X, Mouse.Y, 80, ArrowTex);
  S.Rect.Width := 16;
  S.Rect.Height := 16;

  // Draw map
  For J:=0 To Pred(MapSize) Do
    For I:=0 To Pred(MapSize) Do
    Begin
      // If this tile is solid, draw it
      If MapData[J,I] = 1 Then
      Begin
        S := SpriteManager.Instance.DrawSprite(MapOffsetX+I*TileSize, MapOffsetY+J*TileSize, 45, BlockTex);
        S.Rect.Width := TileSize;
        S.Rect.Height := TileSize;
      End;

      // If this tile was visited, then draw a mark
      If MapVisited[J,I] Then
      Begin
        S := SpriteManager.Instance.DrawSprite(MapOffsetX+I*TileSize, MapOffsetY+J*TileSize, 50, CrossTex);
        S.Rect.Width := TileSize;
        S.Rect.Height := TileSize;
      End;
    End;

  // Draw target
  S := SpriteManager.Instance.DrawSprite(MapOffsetX+TargetX*TileSize, MapOffsetY+TargetY*TileSize, 80, ArrowTex);
  S.Rect.Width := TileSize;
  S.Rect.Height := TileSize;
  S.Mirror := True;

  // Draw ghost
  S := SpriteManager.Instance.DrawSprite(MapOffsetX+GhostX*TileSize, MapOffsetY+GhostY*TileSize, 85, GhostTex);
  S.Rect.Width := TileSize;
  S.Rect.Height := TileSize;

  // Draw path (if available)
  If Assigned(GhostPath) Then
  Begin
    For I:=0 To Pred(GhostPath.Size) Do
    Begin
      GhostPath.GetNode(I, Node);
      S := SpriteManager.Instance.DrawSprite(MapOffsetX + Node.X * TileSize + TileSize Div 4, MapOffsetY + Node.Y * TileSize + TileSize Div 4, 82, DotTex);
      S.Rect.Width := TileSize Div 2;
      S.Rect.Height := TileSize Div 2;
    End;
  End;
End;

// This is called every frame, so we put here our main loop code
Procedure Demo.OnIdle();
Var
  Node:GridPathNode;
Begin
  // Update Ghost, if a path is avaliable
  If (Assigned(GhostPath)) And (GetTime() - GhostUpdateTime>100) Then
  Begin
    GhostUpdateTime := GetTime();

    If GhostPath.GetNode(GhostPosition, Node) Then
    Begin
      GhostX := Node.X;
      GhostY := Node.Y;
      Inc(GhostPosition);
    End Else
    Begin
      ReleaseObject(GhostPath);
    End;
  End;

  If InputManager.Instance.Keys.WasReleased(keyEscape) Then
    Application.Instance.Terminate;
End;

Procedure Demo.SelectResolution2D(Var Width, Height:Integer; Var Scale:Single);
Begin
  Width := MapSize * TileSize;
  Height := MapSize * TileSize;
  Scale := 4.0;
End;

Procedure Demo.OnMouseDown(X,Y:Integer;Button:Word);
Begin
  If Assigned(GhostPath) Then
    Exit; // Ghost is moving, dont do anything for now

  // Calculate map coordinates
  // This is done by first converting to renderer coordinates.
  // Then we subtract the offset, and divide by the size of the tiles
  X := (Round(X-MapOffsetX) Div TileSize);
  Y := (Round(Y-MapOffsetY) Div TileSize);

  // If we clicked in a valid position
  If (X>=0)And(X<MapSize)And(Y>=0)And(Y<MapSize) Then
  Begin
    // If this tile isn't solid
    If MapData[Y,X]<>1 Then
    Begin
      //  Clear visited array
      FillChar(MapVisited, MapSize*MapSize, False);

      // Update target position
      TargetX := X;
      TargetY := Y;

      //  Now we search the path
      //  There are two versions of the search method.
      //  One for limited areas, the one we use here, where we must pass the
      //  minX, minY, maxX and maxY limits.
      //  You can omit these variables, and use the infinite area search method
      //  However, the limited method is faster and use less resources, so use it
      //  everytime you know the map limits.

      //  Note -  We use zero as flags, but in normal cases you'll should use
      //          sfSearchTimeOut, to allow the search to terminate
      //          In this demo, because of the path visualization callback
      //          we can't enable timeout.
      GhostPath := _PathFinder.SearchWithBounds(GhostX,GhostY, TargetX, TargetY, 0,0,Pred(MapSize),Pred(MapSize), 0);

      // reset position index
      GhostPosition := 0;

      (*
      If GhostPath = Nil Then
      Begin
        Case _PathFinder.ErrorCode Of
          pathSearchLimitReached: MessageBox('AI.SearchPath: Node stack overflow.');
          pathSearchTimeOut:      MessageBox('AI.SearchPath: Search timeout.');
          pathSearchFailed:       MessageBox('AI.SearchPath: No paths avaliable.');
        End;
      End;
      *)
    End;

  End;
End;


Begin
  // Start the application
  Demo.Create();
End.



