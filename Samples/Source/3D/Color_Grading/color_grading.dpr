{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_Scene, TERRA_Utils, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_JPG, TERRA_PNG, TERRA_UI,
  TERRA_SpriteManager, TERRA_FileManager, TERRA_Math, TERRA_Vector3D,
  TERRA_Renderer, TERRA_InputManager;
                                      
Type
  // A client is used to process application events
  Demo = Class(Application)
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
Procedure Demo.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  // Load a Tex
  Tex := TextureManager.Instance.GetTexture('forest');
  If Assigned(Tex) Then
  Begin
    Tex.PreserveQuality := True;
    Tex.Uncompressed := True;
    Tex.Filter := filterBilinear;
    Tex.WrapMode := wrapNothing;
  End;

  //GradRamp := TextureManager.Instance.GetTexture('negative');
  GradRamp := TextureManager.Instance.GetTexture('sepia');
  //GradRamp := TextureManager.Instance.GetTexture('monochrome');

  CurrentGrad := GradRamp;

  //CurrentGrad := TextureManager.Instance.DefaultColorTable;

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorWhite;

  Percent := 0.5;
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  I:Integer;
  S:QuadSprite;
Begin
  If Not Assigned(Tex) Then
    Exit;

  S := SpriteManager.Instance.DrawSprite(0, 0, 50, Tex);
  S.Rect.Width := UIManager.Instance.Width;
  S.Rect.Height := UIManager.Instance.Height;


  If (Percent>0) Then
  Begin
    S := SpriteManager.Instance.DrawSprite(0, 0, 80, Tex, CurrentGrad);
    S.Rect.Width := Trunc(UIManager.Instance.Width * Percent);
    S.Rect.Height := UIManager.Instance.Height;

    S.Rect.U2 := S.Rect.Width / UIManager.Instance.Width;
  End;
End;

// Called every time the mouse moves
Procedure Demo.OnMouseMove(X, Y: Integer);
Begin
  Percent := X / Application.Instance.Width;
End;

Begin
  // Start the application
  Demo.Create();
End.