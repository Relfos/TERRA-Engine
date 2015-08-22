{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

uses
  TERRA_Application,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_DemoApplication,
  TERRA_ResourceManager,
  TERRA_Texture,
  TERRA_String,
  TERRA_Utils,
  TERRA_Object,
  TERRA_AIGridPath,
  TERRA_OS,
  TERRA_PNG,
  TERRA_Sprite,
  TERRA_EngineManager,
  TERRA_FileManager,
  TERRA_Math,
  TERRA_Image,
  TERRA_Color,
  TERRA_Resource,
  TERRA_Vector3D,
  TERRA_Vector2D,
  TERRA_LineDrawing,
  TERRA_DebugDraw,
  TERRA_Renderer,
  TERRA_InputManager;

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
  Demo = Class(DemoApplication)
    Protected
			Procedure OnCreate; Override;
			Procedure OnRender2D(V:TERRAViewport); Override;
      Procedure OnDestroy; Override;

      Procedure OnMouseDown(Const X,Y:Single; Const Button:Word); Override;

      Procedure OnIdle(); Override;
  End;


Var
  Tex:TERRATexture = Nil;

Var
  // Some textures needed by our objects
  GhostTex,BlockTex,ArrowTex,CrossTex, DotTex:TERRATexture;

  // Ghost variables
  GhostX,GhostY:Integer;
  GhostUpdateTime:Cardinal;
  GhostPath:GridPath;
  GhostPosition:Integer;

  // Target variables
  TargetX,TargetY:Integer;

  // Our pathfinder object
  _Pathfinder:MyPathfinder;

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
  Inherited;

  Self.GUI.Viewport.Visible := True;

  GhostTex := Engine.Textures.GetItem('ghost');
  BlockTex := Engine.Textures.GetItem('block');
  ArrowTex := Engine.Textures.GetItem('arrows');
  CrossTex := Engine.Textures.GetItem('cross');
  DotTex := Engine.Textures.GetItem('dot');

  // Init Ghost position
  GhostX := 5;
  GhostY := 5;

  TargetX := GhostX;
  TargetY := GhostY;

  // Create pathfinder object
  _Pathfinder := MyPathfinder.Create();

End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnDestroy;
Begin
  // Destroy pathfinder object
  ReleaseObject(_Pathfinder);

  Inherited;
End;

Procedure Demo.OnRender2D(V:TERRAViewport);
Var
  S:QuadSprite;
  I,J:Integer;
  Mouse:Vector2D;
  Node:GridPathNode;
Begin
  //  Draw mouse cursor
  Mouse  := Engine.Input.Mouse;
  S := V.SpriteRenderer.DrawSprite(Mouse.X, Mouse.Y, 80, ArrowTex);
  S.Rect.Width := 16;
  S.Rect.Height := 16;

  // Draw map
  For J:=0 To Pred(MapSize) Do
    For I:=0 To Pred(MapSize) Do
    Begin
      // If this tile is solid, draw it
      If MapData[J,I] = 1 Then
      Begin
        S := V.SpriteRenderer.DrawSprite(MapOffsetX+I*TileSize, MapOffsetY+J*TileSize, 10, BlockTex);
        S.Rect.Width := TileSize;
        S.Rect.Height := TileSize;
      End;

      // If this tile was visited, then draw a mark
      If MapVisited[J,I] Then
      Begin
        S := V.SpriteRenderer.DrawSprite(MapOffsetX+I*TileSize, MapOffsetY+J*TileSize, 15, CrossTex);
        S.Rect.Width := TileSize;
        S.Rect.Height := TileSize;
      End;
    End;

  // Draw target
  S := V.SpriteRenderer.DrawSprite(MapOffsetX+TargetX*TileSize, MapOffsetY+TargetY*TileSize, 20, ArrowTex);
  S.Rect.Width := TileSize;
  S.Rect.Height := TileSize;
  S.Mirror := True;

  // Draw ghost
  S := V.SpriteRenderer.DrawSprite(MapOffsetX+GhostX*TileSize, MapOffsetY+GhostY*TileSize, 30, GhostTex);
  S.Rect.Width := TileSize;
  S.Rect.Height := TileSize;

  // Draw path (if available)
  If Assigned(GhostPath) Then
  Begin
    For I:=0 To Pred(GhostPath.Size) Do
    Begin
      GhostPath.GetNode(I, Node);
      S := V.SpriteRenderer.DrawSprite(MapOffsetX + Node.X * TileSize + TileSize Div 4, MapOffsetY + Node.Y * TileSize + TileSize Div 4, 25, DotTex);
      S.Rect.Width := TileSize Div 2;
      S.Rect.Height := TileSize Div 2;
    End;
  End;

  Inherited;
End;

// This is called every frame, so we put here our main loop code
Procedure Demo.OnIdle();
Var
  Node:GridPathNode;
Begin
  Inherited;
  
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
End;

(*Procedure Demo.SelectResolution2D(Var Width, Height:Integer; Var Scale:Single);
Begin
  Width := MapSize * TileSize;
  Height := MapSize * TileSize;
  Scale := 4.0;
End;*)


Procedure Demo.OnMouseDown(Const X,Y:Single; Const Button:Word);
Var
  TX, TY:Integer;
Begin
  If Assigned(GhostPath) Then
    Exit; // Ghost is moving, dont do anything for now


  Self.GUI.GetLocalCoords(X, Y,  TX, TY);

  // Calculate map coordinates
  // This is done by first converting to renderer coordinates.
  // Then we subtract the offset, and divide by the size of the tiles
  TX := (Round(TX-MapOffsetX) Div TileSize);
  TY := (Round(TY-MapOffsetY) Div TileSize);

  // If we clicked in a valid position
  If (TX>=0) And (TX<MapSize) And (TY>=0) And (TY<MapSize) Then
  Begin
    // If this tile isn't solid
    If MapData[TY, TX]<>1 Then
    Begin
      //  Clear visited array
      FillChar(MapVisited, MapSize*MapSize, False);

      // Update target position
      TargetX := TX;
      TargetY := TY;

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


