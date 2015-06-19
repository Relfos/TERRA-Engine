{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Object, TERRA_Vector2D, TERRA_Font, TERRA_Texture,
  TERRA_UI, TERRA_FileManager, TERRA_InputManager,
  TERRA_PNG, TERRA_TTF,
  TERRA_Scene, TERRA_Color, TERRA_ClipRect,
  TERRA_UIWindow, TERRA_UIButton, TERRA_UITabs, TERRA_UISprite, TERRA_UILabel,
  TERRA_UIIcon, TERRA_UIScrollbar;

Type
  Demo = Class(Application)
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
  End;

Var
  Fnt:Font;
  SmallFnt:Font;
  MyUI:UI;
  MyWnd:UIWindow;
  Tabs:UITabList;
  Background:UISprite;
  MyScroll:UIScrollBar;

{ Game }
Procedure Demo.OnCreate;
Var
  MyTex:Texture;
Begin
  FileManager.Instance.AddPath('Assets');

  // Load a font
  Fnt := FontManager.Instance.GetFont('droid');

  // Load a second font, with custom size
  SmallFnt := FontManager.Instance.GetFont('droid@20');

  // Create a new UI
  MyUI := UI.Create;

  // Register the font with the UI
  MyUI.DefaultFont := Fnt;

  // Load a GUI skin
  MyUI.LoadSkin('ui_sample_skin');

  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a empty scene
  GraphicsManager.Instance.Scene := MyScene.Create;
End;

Procedure Demo.OnDestroy;
Begin
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  MyWnd.ClipChildren(ClipRectCreate(MyWnd.AbsolutePosition.X, MyWnd.AbsolutePosition.Y+26,  MyWnd.Size.X -30, MyWnd.Size.Y-(26*2)));
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
  W:Widget;
  I:Integer;
  MyTex:Texture;
Begin
  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a UI background
  If Assigned(MyTex) Then
  Begin
    Background := UISprite.Create('mybg', MyUI, 0, 0, 1);

    Background.SetTexture(MyTex);
    Background.Width := UIScreenWidthPercent(100);
    Background.Height := UIScreenHeightPercent(100);

    Background.U2 := 2;
    Background.V2 := 2;
  End;

  MyWnd := UIWindow.Create('mywnd', MyUI, 0, 0, 10, UIPixels(500), UIPixels(300), 'window');
  MyWnd.Align := waCenter;
  MyWnd.Draggable := True;

  Tabs := UITabList.Create('tabs1', MyWnd, 10, 0, 1, UIPixels(100), UIPixels(30), 'tabs');
  Tabs.Font := SmallFnt;
  //Tabs.Align := waTopRight;
  MyWnd.TabControl := Tabs;

  For I:=0 To 2 Do
    Tabs.AddTab('Tab'+IntToString(I), I);

  For I:=0 To 2 Do
    Btn := UIButton.Create('btn1', MyWnd, 20 + I * 120, 40, 10, UIPixels(100), UIPixels(50), 'Btn'+IntToString(I), 'button', I);

  MyScroll := UIScrollBar.Create('myscroll', MyWnd, 10, 20, 5, UIPixels(20), UIPixels(260), UIPixels(10), UIPixels(26), 'vscroll');
  MyScroll.Align := waTopRight;
  MyScroll.TabIndex := 0;
  MyScroll.Max := 600;

  // Not all widgets need a parent
  // If you pass Nil as parent, the screen will be considered their parent
  W := UILabel.Create('mylabel', MyWnd, 20, 20, 10, 'This is an aligned text label.');
  W.Align := waBottomRight;
  W.TabIndex := 1;

  W := UISprite.Create('test', MyWnd, 150, 50, 1, 'cobble', 0);
  W.Scroll := MyScroll;

  W := UISprite.Create('test', MyWnd, 150, 300, 1, 'fur_diffuse', 0);
  W.Scroll := MyScroll;

  W := UISprite.Create('test', MyWnd, 150, 550, 1, 'marble_diffuse', 0);
  W.Scroll := MyScroll;

  W := UIIcon.Create('test', MyWnd, 50, 50, 1, UIPixels(100), UIPixels(100), 'ghosticon', 2);
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

