{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture,
  TERRA_UI, TERRA_FileManager, TERRA_TTF,
  TERRA_Widgets, TERRA_PNG, TERRA_Scene, TERRA_Color, TERRA_Matrix4x4;

Type
  Game = Class(AppClient)
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
      Destructor Destroy; Reintroduce;
  End;

Var
  Fnt:Font;
  MyUI:UI;
  MyWnd:UIWindow;
  MyText:UILabel;
  ShowBtn:UIButton;
  Background:UISprite;

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

  // Load a custom mouse cursor
  MyUI.LoadCursor('cursor.png');

  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a UI background
  If Assigned(MyTex) Then
  Begin
    Background := UISprite.Create('mybg', MyUI, Nil, 0, 0, 0);

    Background.SetTexture(MyTex);
    Background.Rect.Width := UIManager.Instance.Width;
    Background.Rect.Height := UIManager.Instance.Height;


    Background.Rect.U2 := 2;
    Background.Rect.V2 := 2;
     //VectorCreate2D(1,0.5), 0.1, VectorCreate2D(2.0, 2.0)
  End;

  // Create a empty scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;
End;

Procedure Game.OnDestroy;
Begin
  _Scene.Destroy();
End;

Procedure Game.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler

// Simple button event handlers, change UI background color
Procedure OnRedClick(Src:Widget); Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(255, 200, 200);
End;

Procedure OnGreenClick(Src:Widget); Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(200, 255, 200);
End;

Procedure OnBlueClick(Src:Widget); Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color :=  ColorCreate(200, 200, 255);
End;

Procedure OnCheckBoxClick(Src:Widget); Cdecl;
Begin
  MyWnd.AllowDragging := UICheckBox(Src).Checked;
End;

Procedure OnShowClick(Src:Widget); Cdecl;
Begin
  MyWnd.Color := ColorWhite;
  MyWnd.Show(widgetAnimatePosY);
  ShowBtn.Hide(widgetAnimateAlpha);
End;

Procedure OnCloseClick(Src:Widget); Cdecl;
Begin
  MyWnd.Hide(widgetAnimateAlpha);
  ShowBtn.Show(widgetAnimateAlpha);
End;

// A event handler can handle different widgets if you want
// Just check the name of the src widget to see which widget called this event
// Note - Widget names are stored internally in UPPER CAPS
Procedure OnRadioClick(Src:Widget); Cdecl;
Begin
  If (Not Assigned(Src)) Then
    Exit;

  If (Src.Name = 'MYRADIO1') Then
    MyUI.Color := ColorCreate(255, 200, 200)
  Else
  If (Src.Name = 'MYRADIO2') Then
    MyUI.Color := ColorCreate(200, 255, 200)
  Else
  If (Src.Name = 'MYRADIO3') Then
    MyUI.Color := ColorCreate(200, 200, 255);
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
Begin
  MyWnd := UIWindow.Create('mywnd', MyUI, 10, 10, 10, 6, 4);
  MyWnd.AllowDragging := True;
  MyWnd.Align := waCenter;

  Btn := UIButton.Create('btn1', MyUI, MyWnd, 20, 40, 10, 'Red');
  Btn.OnMouseClick := OnRedClick; // Assign a onClick event handler

  Btn := UIButton.Create('btn2', MyUI, MyWnd, 100, 40, 10, 'Green');
  Btn.OnMouseClick := OnGreenClick; // Assign a onClick event handler

  Btn := UIButton.Create('btn3', MyUI, MyWnd, 180, 40, 10, 'Blue');
  Btn.OnMouseClick := OnBlueClick; // Assign a onClick event handler

  CB := UICheckbox.Create('mycheckbox', MyUI, MyWnd, 20, 100, 10, 'Draggable');
  CB.OnMouseClick := OnCheckBoxClick; // Assign a onClick event handler
  CB.Checked := True;

  Btn := UIButton.Create('closebtn', MyUI, MyWnd, 15, 30, 10, 'Close');
  Btn.OnMouseClick := OnCloseClick; // Assign a onClick event handler
  Btn.Align := waBottomRight; // By default widgets are aligned to top/left

  // Create three radio buttons, and group them by assigning the same group ID
  RB := UIRadioButton.Create('myradio1', MyUI, MyWnd, 20, 150, 10, 'Red');
  RB.Group := 0;
  RB.OnMouseClick := OnRadioClick;

  RB := UIRadioButton.Create('myradio2', MyUI, MyWnd, 20, 170, 10, 'Green');
  RB.Group := 0;
  RB.OnMouseClick := OnRadioClick;

  RB := UIRadioButton.Create('myradio3', MyUI, MyWnd, 20, 190, 10, 'Blue');
  RB.Group := 0;
  RB.OnMouseClick := OnRadioClick;

  // Not all widgets need a parent
  // If you pass Nil as parent, the screen will be considered their parent
  MyText := UILabel.Create('mylabel', MyUI, Nil, 20, 20, 10, 'This is an aligned text label.');
  MyText.Align := waBottomRight;

  ShowBtn := UIButton.Create('showbtn', MyUI, Nil, 20, 10, 10, 'Show');
  ShowBtn.Align := waTopRight;
  ShowBtn.Visible := False;   // Some elements can start invisible, and only appear when you need them
  ShowBtn.OnMouseClick := OnShowClick; // Assign a onClick event handler
End;

Destructor MyScene.Destroy;
Begin
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

