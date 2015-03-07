{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture,
  TERRA_UI, TERRA_FileManager,
  TERRA_Widgets,
  TERRA_PNG, TERRA_TTF,
  TERRA_Scene, TERRA_Color;

Type
  Game = Class(AppClient)
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


{ Game }
Procedure Game.OnCreate;
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

  // Load a custom mouse cursor
  MyUI.LoadCursor('cursor.png');

  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a empty scene
  GraphicsManager.Instance.Scene := MyScene.Create;
End;

Procedure Game.OnDestroy;
Begin
End;

Procedure Game.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
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
Var
  Btn:UIButton;
  RB:UIRadioButton;
  CB:UICheckbox;
  W:Widget;
  I:Integer;
Begin
  MyWnd := UIWindow.Create('mywnd', MyUI, 10, 10, 10, 6, 4);
  MyWnd.Align := waCenter;

  Tabs := UITabList.Create('tabs1', MyUI, MyWnd, 0, 0, 1);
  Tabs.Font := SmallFnt;
  MyWnd.TabControl := Tabs;

  For I:=0 To 2 Do
    Tabs.AddTab('Tab'+IntToString(I), I);

  Tabs.Align := waTopRight;

  For I:=0 To 2 Do
  Begin
    Btn := UIButton.Create('btn1', MyUI, MyWnd, 20 + I * 80, 40, 10, 'Btn'+IntToString(I));
    Btn.TabIndex := 0;
  End;

  CB := UICheckbox.Create('mycheckbox', MyUI, MyWnd, 20, 100, 10, 'Testbox');
  CB.TabIndex := 2;

  Btn := UIButton.Create('closebtn', MyUI, MyWnd, 15, 30, 10, 'Close');
  Btn.Align := waBottomRight; // By default widgets are aligned to top/left
  Btn.TabIndex := 0;

  For I:=0 To 2 Do
  Begin
    RB := UIRadioButton.Create('myradio1', MyUI, MyWnd, 20, 150 + I*20, 10, 'RB'+IntToString(I));
    RB.TabIndex := 1;
  End;

  // Not all widgets need a parent
  // If you pass Nil as parent, the screen will be considered their parent
  W := UILabel.Create('mylabel', MyUI, MyWnd, 10, 5, 10, 'This is an aligned text label.');
  W.Align := waBottomRight;
  W.TabIndex := 2;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  ApplicationStart(Game.Create);
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

