{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture,
  TERRA_UI, TERRA_FileManager, TERRA_InputManager, TERRA_TTF,
  TERRA_PNG, TERRA_Scene, TERRA_Color, TERRA_Matrix4x4,
  TERRA_UIWindow, TERRA_UIButton, TERRA_UISprite, TERRA_UILabel, TERRA_UIFileBrowser;

Type
  Demo = Class(Application)
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
  Background:UISprite;
  Browser:UIFileBrowser;

{ Game }
Procedure Demo.OnCreate;
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

  // Create a empty scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Scene);
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
End;

Procedure Demo.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  MyUI.OnMouseDown(X, Y, Button);
End;

Procedure Demo.OnMouseMove(X, Y: Integer);
Begin
  MyUI.OnMouseMove(X, Y);
End;

Procedure Demo.OnMouseUp(X, Y: Integer; Button: Word);
Begin
  MyUI.OnMouseUp(X, Y, Button);
End;

{ MyScene }
Constructor MyScene.Create;
Var
  Btn:UIButton;

  MyTex:Texture;
Begin
  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a UI background
  If Assigned(MyTex) Then
  Begin
    Background := UISprite.Create('mybg', MyUI, 0, 0, 1);

    Background.SetTexture(MyTex);
    Background.Rect.Width := UIManager.Instance.Width;
    Background.Rect.Height := UIManager.Instance.Height;

    Background.Rect.U2 := 2;
    Background.Rect.V2 := 2;
  End;

  Browser := UIFileBrowser.Create('mybrower', MyUI, 0, 0, 10, UIPixels(500), UIPixels(300), MyUI.GetComponent('window'), MyUI.GetComponent('vscroll'));
  Browser.Draggable := True;
  Browser.Align := waCenter;
///  MYWnd.Color := ColorGrey(255, 128);
End;

Procedure MyScene.Release;
Begin
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

