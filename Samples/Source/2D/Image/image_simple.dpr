{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

uses
  TERRA_Application,
  TERRA_Scene,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_ResourceManager,
  TERRA_Texture,
  TERRA_Utils,
  TERRA_Object,
  TERRA_OS,
  TERRA_PNG,
  TERRA_SpriteManager,
  TERRA_FileManager,
  TERRA_Math,
  TERRA_Image,
  TERRA_Color,
  TERRA_Resource,
  TERRA_Vector3D,
  TERRA_Vector2D,
  TERRA_Renderer,
  TERRA_InputManager;

Type
  // A client is used to process application events
  Demo = Class(Application)
    Protected
      _Scene:TERRAScene;

			Procedure OnCreate; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(TERRAScene)
      Procedure RenderSprites(V:TERRAViewport); Override;
  End;

Var
  Tex:TERRATexture = Nil;

{ Game }
Procedure Demo.OnCreate;
Var
  I:Integer;
  Img:Image;
Begin
  Inherited;

  // create a image at run-time
  Img := Image.Create(256, 256);

  // fill with red color
  Img.FillRectangleByUV(0, 0, 1, 1, ColorRed);

  // put some random circles into it
  For I:=1 To 20 Do
    Img.DrawCircleByUV(RandomFloat(0, 1), RandomFloat(0, 1), 50, ColorWhite);

  // Create a texture from a image
  Tex := TERRATexture.Create(rtDynamic, '');
  Tex.InitFromImage(Img);

  ReleaseObject(Img);

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;
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
  S:QuadSprite;
Begin
  S := SpriteManager.Instance.DrawSprite(20, 20, 50, Tex);
End;

Begin
  // Start the application
  Demo.Create();
End.
