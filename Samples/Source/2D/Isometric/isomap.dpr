{
  TERRA Engine - Isometric map rendering  (prototype, not working 100% yet)
  Author: Sergio Flores (Relfos@gmail.com)
}

Program Tutorial10;

Uses TERRA_MemoryManager, TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture, TERRA_InputManager,
  TERRA_UI, TERRA_FileManager, TERRA_SpriteManager, TERRA_Viewport, TERRA_Renderer,
  TERRA_Widgets, TERRA_PNG, TERRA_Scene, TERRA_Color, TERRA_Matrix4x4;

Const
  TileWidth = 64;
  TileHeight = 64;
  TileStepX = 64;
  TileStepY = 16;
  OddRowXOffset = 32;
  HeightTileOffset = 32;

  // Now comes the definition of our demo map
  MapWidth  = 50;   // Width of the map/number of tiles per row
  MapHeight = 50;   // Height of the map/number of tiles per column

Type
  TileStack = Object
    Tiles:Array Of Byte;
    Count:Integer;
    Top:Byte;

    Procedure Add(ID:Byte);
  End;

  Demo = Class(Application)
    Protected
      _Scene:Scene;

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;


Var
  Tileset:Texture;       // Our tileset texture
  MapX, MapY: Single;    // Scrolling coordinates
  LastUpdate:Integer;
  Map:Array[0..Pred(MapWidth), 0..Pred(MapHeight)] Of TileStack;

Procedure TileStack.Add(ID:Byte);
Begin
  Inc(Count);
  SetLength(Tiles, Count);
  Tiles[Pred(Count)] := ID;
End;


Procedure MakeStack(X,Y, Height:Integer; ID:Byte);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Height) Do
    Map[X,Y].Add(ID);
End;


{ Game }
Procedure Demo.OnCreate;
Var
  I,J:Integer;
Begin
  FileManager.Instance.AddPath('Assets');

  // Get our tileset
  Tileset := TextureManager.Instance.GetTexture('isotileset');

  // Change filtering to make the tileset rendering pixel perfect
  If Assigned(Tileset) Then
  Begin
    Tileset.Filter := filterLinear;
    Tileset.MipMapped := False;
  End;

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorBlack;

  For J:=0 To Pred(MapHeight) Do
  Begin
    For I:=0 To Pred(MapWidth) Do
    Begin
      Map[I,J].Count := 0;
      Map[I,J].Top := 0;
      Map[I,J].Add(1);

      If Random(20)<4 Then
        Map[I,J].Top := Random(5) + 50;

      If Random(50)<4 Then
        Map[I,J].Top := Random(10) + 110;
    End;
  End;

//  MakeStack(4, 6, 1, 34);
//  MakeStack(5, 6, 2, 34);
  MakeStack(1, 8, 2, 60);
  MakeStack(10, 8, 2, 55);

  Map[2,2].Top := 50;

  LastUpdate := Application.Instance.GetElapsedTime();

  // Create a empty scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Scene);
End;

Procedure Demo.OnIdle;
Var
  Delta:Single;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  Delta := Application.Instance.GetElapsedTime() - LastUpdate;

  If (Delta > 20) Then
  Begin
    LastUpdate := Application.Instance.GetElapsedTime();
    //Delta := 1.5;

    If InputManager.Instance.Keys.IsDown(keyUp) Then
      MapY := MapY - Delta;
    If InputManager.Instance.Keys.IsDown(keyDown) Then
      MapY := MapY + Delta;

    If InputManager.Instance.Keys.IsDown(keyLeft) Then
      MapX := MapX - Delta;
    If InputManager.Instance.Keys.IsDown(keyRight) Then
      MapX := MapX + Delta;
  End;
End;

{ MyScene }
// This is called every frame, so we put here our main loop code
Procedure MyScene.RenderSprites(V:Viewport);
Var
  S:QuadSprite;
  X,Y, XOfs:Single;
  TileID:Byte;
  I,J,K:Integer;
Begin
  // Draw all tiles in our map
    For J:=0 To Pred(MapHeight) Do
    Begin
      If (Odd(J)) Then
        XOfs := 32
      Else
        XOfs := 0;

      For I:=0 To Pred(MapWidth) Do
      Begin
        X := -MapX+ I * TileWidth + XOfs;

        For K:=0 To Pred(Map[I,J].Count) Do
        Begin
          TileID := Map[I,J].Tiles[K];

          // We add a new sprite for each tile
          // The scroll effect is created by adding MapX/MapY variables the sprite position
          Y := -MapY + J * 16 - 32 * K;
          S := SpriteManager.Instance.DrawSprite(X, Y, 10+K*0.1, Tileset);
          S.Rect.TileRemap(TileID Mod 10, TileID Div 10, 10, 16);

          S.Rect.Width := 67;
          S.Rect.Height := 67;
        End;

        TileID := Map[I,J].Top;
        // If this is a invisible tile then skip it
        If TileID=0 Then
          Continue;

          S := SpriteManager.Instance.DrawSprite(X,Y, 90, Tileset);
          S.Rect.TileRemap(TileID Mod 10, TileID Div 10, 10, 16);
      End;
    End;
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

