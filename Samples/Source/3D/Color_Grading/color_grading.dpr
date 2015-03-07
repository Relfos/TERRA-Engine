{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_Utils, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_JPG, TERRA_PNG, TERRA_UI,
  TERRA_SpriteManager, TERRA_FileManager, TERRA_Math, TERRA_Vector3D;
                                      
Type
  // A client is used to process application events
  MyGame = Class(AppClient)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
			Procedure OnIdle; Override;

			Procedure OnMouseMove(X,Y:Integer); Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Var
  Tex:Texture = Nil;
  GradRamp:Texture = Nil;
  CurrentGrad:Texture;

  Percent:Single;

{ Game }
Procedure MyGame.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  // Load a Tex
  Tex := TextureManager.Instance.GetTexture('forest');
  Tex := TextureManager.Instance.GetTexture('test');
  If Assigned(Tex) Then
  Begin
    Tex.PreserveQuality := True;
    Tex.Uncompressed := True;
    Tex.BilinearFilter := True;
    Tex.Wrap := False;
  End;

  //GradRamp := TextureManager.Instance.GetTexture('negative');
  GradRamp := TextureManager.Instance.GetTexture('sepia');
  //GradRamp := TextureManager.Instance.GetTexture('monochrome');

  CurrentGrad := GradRamp;

  //CurrentGrad := TextureManager.Instance.DefaultColorTable;

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorWhite;

  Percent := 0.5;
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  I:Integer;
  S:Sprite;
Begin
  If Not Assigned(Tex) Then
    Exit;

  S := SpriteManager.Instance.DrawSprite(0, 0, 80, Tex);
  S.Rect.Width := UIManager.Instance.Width;
  S.Rect.Height := UIManager.Instance.Height;


  If (Percent>0) Then
  Begin
    S := SpriteManager.Instance.DrawSprite(0, 0, 50, Tex, CurrentGrad);
    S.Rect.Width := Trunc(UIManager.Instance.Width * Percent);
    S.Rect.Height := UIManager.Instance.Height;

    S.Rect.U2 := S.Rect.Width / UIManager.Instance.Width;
  End;
End;

// Called every time the mouse moves
Procedure MyGame.OnMouseMove(X, Y: Integer);
Begin
  Percent := X / Application.Instance.Width;
End;

Begin
  // Start the application
  ApplicationStart(MyGame.Create);
End.