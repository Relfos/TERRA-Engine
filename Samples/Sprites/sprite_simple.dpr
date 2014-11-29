{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_PNG, 
  TERRA_SpriteManager, TERRA_FileManager, TERRA_Math, TERRA_Vector3D;
    
Type
  // A client is used to process application events
  MyGame = Class(AppClient)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Var
  Tex:Texture = Nil;

{ Game }
Procedure MyGame.OnCreate;
Begin
  // Load a Tex
  Tex := TextureManager.Instance.GetTexture('mario');

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.BackgroundColor := ColorBlue;
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnIdle;
Begin
  If Keys[keyEscape] Then
    Application.Instance.Terminate;
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  I:Integer;
  S:Sprite;
Begin
  // This is how sprite rendering works with TERRA.
  // 1st we ask the Renderer to create a new sprite, using a Tex and position.
  // Note that this sprite instance is only valid during the frame its created.
  // If needed we can configure the sprite properties.

  // Note - The third argument of VectorCreate is the sprite Layer, should be a value between 0 and 100
  //        Sprites with higher layer values appear below the others

  // Create a simple fliped sprite
  S := SpriteManager.Instance.AddSprite(220, 60, 50, Tex);
  S.Flip := True;

  // Create a line of sprites
  For I:=0 To 8 Do
  Begin
    S := SpriteManager.Instance.AddSprite(16+32*I, 10, 50, Tex);
    S.Mirror := Odd(I);    // Each odd sprite in line will be reflected
  End;

  // Create a line of rotated sprites
  For I:=0 To 8 Do
  Begin
    S := SpriteManager.Instance.AddSprite(16+32*I, 190, 50, Tex);
    S.SetScaleAndRotation(1, RAD * (I*360 Div 8));
  End;

  // A rotating sprite in the middle
  S := SpriteManager.Instance.AddSprite(100,100, 50, Tex);
  S.SetScaleAndRotation(1, RAD * ((GetTime() Div 15) Mod 360));  // Calculate rotation, in degrees, from current time

  // Some scaled sprites
  S := SpriteManager.Instance.AddSprite(10,120,55, Tex);
  S.SetScale(2.0);    // Double size

  S := SpriteManager.Instance.AddSprite(60,130,55, Tex);
  S.SetScale(1.5);    // 1.5 Size

  S := SpriteManager.Instance.AddSprite(95,145,55, Tex);
  S.SetScale(0.5);    // Half size

  // An alpha blended sprite
  S := SpriteManager.Instance.AddSprite(85, 60, 55, Tex);
  S.SetColor(ColorCreate(255, 255, 255, 128));

  // Some colored sprites
  S := SpriteManager.Instance.AddSprite(130,120,50, Tex);
  S.SetColor(ColorCreate(255,128,255)); // Purple tint

  S := SpriteManager.Instance.AddSprite(150,120,50, Tex);
  S.SetColor(ColorCreate(255,128,128)); // Red tint

  S := SpriteManager.Instance.AddSprite(170,120,50, Tex);
  S.SetColor(ColorCreate(128,255,128)); // Green tint

  S := SpriteManager.Instance.AddSprite(190,120,50, Tex);
  S.SetColor(ColorCreate(128,128,255)); // Blue tint

  S :=SpriteManager.Instance.AddSprite(210,120,50, Tex);
  S.SetColor(ColorCreate(255,255,128)); // Yellow tint
End;

Begin
  // Start the application
  ApplicationStart(MyGame.Create);
End.