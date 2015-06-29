{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Object, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture,
  TERRA_UI, TERRA_FileManager, TERRA_InputManager, TERRA_TTF,
  TERRA_PNG, TERRA_Scene, TERRA_SpriteManager, TERRA_ClipRect, TERRA_Color, TERRA_Matrix4x4,
  TERRA_UIWindow, TERRA_UISprite;

Type
  Game = Class(Application)
    Protected
      _Scene:Scene;

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnMouseDown(X,Y:Integer; Button:Word); Override;
			Procedure OnMouseUp(X,Y:Integer; Button:Word); Override;
			Procedure OnMouseMove(X,Y:Integer); Override;
			Procedure OnIdle; Override;
  End;

  MyScene = Class(Scene)
      Constructor Create;
      Procedure Release; Override;
  End;

Var
  Fnt:Font;
  MyUI:UI;
  MyWnd:UIWindow;
  Background:UISprite;

  Clip:ClipRect;

{ Game }
Procedure Game.OnCreate;
Var
  MyTex:Texture;
Begin
  FileManager.Instance.AddPath('Assets');

  // Load a font
  Fnt := FontManager.Instance.GetFont('droid');

  // Create a new UI
  MyUI := UI.Create;

  // Register the font with the UI
  MyUI.DefaultFont := Fnt;

  // Load a GUI skin
  MyUI.LoadSkin('ui_sample_skin');

  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a UI background
  If Assigned(MyTex) Then
  Begin
    Background := UISprite.Create('mybg', MyUI,  0, 0, 0);

    Background.SetTexture(MyTex);
    Background.Width := UIPixels(UIManager.Instance.Width);
    Background.Height := UIPixels(UIManager.Instance.Height);

    Background.U2 := 2;
    Background.V2 := 2;
     //VectorCreate2D(1,0.5), 0.1, VectorCreate2D(2.0, 2.0)
  End;

  // Create a empty scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);
End;

Procedure Game.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  Clip := ClipRectCreate(InputManager.Instance.Mouse.X - 150, InputManager.Instance.Mouse.Y - 150, 300, 300);

  //MyWnd.ClipRect := Clip;
  MyUI.ClipRect := Clip;
End;


Procedure Game.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  MyUI.OnMouseDown(X, Y, Button);
End;

Procedure Game.OnMouseMove(X, Y: Integer);
Begin
  MyUI.OnMouseMove(X, Y);
End;

Procedure Game.OnMouseUp(X, Y: Integer; Button: Word);
Begin
  MyUI.OnMouseUp(X, Y, Button);
End;

{ MyScene }
Constructor MyScene.Create;
Begin
  MyWnd := UIWindow.Create('mywnd', MyUI, 10, 10, 10, UIPixels(500), UIPixels(400), 'window');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;
End;

Procedure MyScene.Release;
Begin
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  Game.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

