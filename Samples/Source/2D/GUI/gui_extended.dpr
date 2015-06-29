{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object, 
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_Utils,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Font,
  TERRA_Texture,
  TERRA_UI,
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_CollectionObjects,
  TERRA_TTF,
  TERRA_PNG,
  TERRA_Scene,
  TERRA_Color,
  TERRA_String,
  TERRA_Matrix4x4,
  TERRA_UIWindow,
  TERRA_UIButton,
  TERRA_UISprite,
  TERRA_UICheckbox,
  TERRA_UIRadioButton,
  TERRA_UILabel,
  TERRA_UIComboBox,
  TERRA_UIProgressBar,
  TERRA_UIEditText;

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

      Procedure OnKeyPress(Key:Word); Override;

			Procedure OnIdle; Override;
  End;

  MyScene = Class(Scene)
      Constructor Create;
      Procedure Release; Override;

      Procedure OnRedClick(Src:Widget);
      Procedure OnGreenClick(Src:Widget);
      Procedure OnBlueClick(Src:Widget);
      Procedure OnCheckBoxClick(Src:Widget);
      Procedure OnShowClick(Src:Widget);
      Procedure OnCloseClick(Src:Widget);
      Procedure OnRadioClick(Src:Widget);
  End;

Var
  Fnt:Font;
  MyUI:UI;
  MyWnd:UIWindow;
  MyText:UILabel;
  ShowBtn:UIButton;
  Background:UISprite;

  PBar:UIProgressBar;

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

  If Assigned(PBar) Then
    PBar.Percent := Abs(Cos(GetTime/1000)) * 100;
End;

Procedure Demo.OnKeyPress(Key:Word);
Begin
  MyUI.OnKeyPress(Key);
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
  I:Integer;

  Btn:UIButton;
  RB:UIRadioButton;
  CB:UICheckbox;

  Edt:UIEditText;

  Combo:UIComboBox;
  Content:List;

  MyTex:Texture;
Begin
  // Get background texture
  MyTex := TextureManager.Instance.GetTexture('background');

  // Create a UI background
  If Assigned(MyTex) Then
  Begin
    Background := UISprite.Create('mybg', MyUI, 0, 0, 1);

    Background.SetTexture(MyTex);
    Background.Width := UIScreenWidthPercent(100.0);
    Background.Height := UIScreenHeightPercent(100.0);

    Background.U2 := 2;
    Background.V2 := 2;
  End;

  MyWnd := UIWindow.Create('mywnd', MyUI, 0, 0, 10, UIPixels(500), UIPixels(500), 'window');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;

  Btn := UIButton.Create('btn1', MyWnd, 20, 40, 10, UIPixels(100), UIPixels(50), 'Red', 'round_button');
  Btn.OnMouseClick := OnRedClick; // Assign a onClick event handler

  Btn := UIButton.Create('btn2', MyWnd, 140, 40, 10, UIPixels(40), UIPixels(50), 'Green', 'button');
  Btn.OnMouseClick := OnGreenClick; // Assign a onClick event handler

  Btn := UIButton.Create('btn3', MyWnd, 200, 40, 10, UIPixels(100), UIPixels(15), 'Blue', 'button');
  Btn.OnMouseClick := OnBlueClick; // Assign a onClick event handler

  CB := UICheckbox.Create('mycheckbox', MyWnd, 20, 100, 10, UIPixels(20), True, 'Draggable', 'checkbox');
  CB.OnMouseClick := OnCheckBoxClick; // Assign a onClick event handler

  Btn := UIButton.Create('closebtn', MyWnd, 15, 30, 10,  UIPixels(150), UIPixels(50), 'Close', 'button');
  Btn.OnMouseClick := OnCloseClick; // Assign a onClick event handler
  Btn.Align := waBottomRight; // By default widgets are aligned to top/left

  // Create three radio buttons, and group them by assigning the same group ID
  RB := UIRadioButton.Create('myradio1', MyWnd, 20, 150, 10, UIPixels(20), 'Red', 'radiobutton');
  RB.Group := 0;
  RB.Tag := 0;
  RB.OnMouseClick := OnRadioClick;

  RB := UIRadioButton.Create('myradio2', MyWnd, 20, 180, 10, UIPixels(20), 'Green', 'radiobutton');
  RB.Group := 0;
  RB.Tag := 1;
  RB.OnMouseClick := OnRadioClick;

  RB := UIRadioButton.Create('myradio3', MyWnd, 20, 210, 10, UIPixels(20), 'Blue', 'radiobutton');
  RB.Group := 0;
  RB.Tag := 2;
  RB.OnMouseClick := OnRadioClick;

  Edt := UIEditText.Create('myedt', MyWnd, 20, 190, 10, UIPixels(300), UIPixels(50), 'editbox');
  Edt.Align := waTopRight;
  Edt.Caption := 'Write something here...';
//  Edt.LineCount := 5;

  Edt := UIEditText.Create('myedt', MyWnd, 20, 340, 10, UIPixels(300), UIPixels(50), 'editbox');
  Edt.Align := waTopRight;

  Combo := UIComboBox.Create('mycombo', MyWnd, 170, 120, 5, UIPixels(250), UIPixels(50), 'combobox');
  Content := List.Create();
  Content.Add(StringObject.Create('Black Leaves'));
  Content.Add(StringObject.Create('Cats'));
  Content.Add(StringObject.Create('Helicopters'));
  Content.Add(StringObject.Create('Heavy Strong Air'));
  Content.Add(StringObject.Create('Badum Badum Tish'));

  Combo.SetContent(Content);
  Combo.ItemIndex := 0;

  PBar := UIProgressBar.Create('myprogress', MyWnd, 230, 280, 1, UIPixels(220), UIPixels(20), 'progressbar');

  // Not all widgets need a parent
  // If you pass Nil as parent, the screen will be considered their parent
  MyText := UILabel.Create('mylabel', MyUI, 20, 20, 10, 'This is an aligned text label.');
  MyText.Align := waBottomRight;

  ShowBtn := UIButton.Create('showbtn', MyUI,  20, 10, 10,  UIPixels(150), UIPixels(50), 'Show', 'button');
  ShowBtn.Align := waTopRight;
  ShowBtn.Visible := False;   // Some elements can start invisible, and only appear when you need them
  ShowBtn.OnMouseClick := OnShowClick; // Assign a onClick event handler
End;

Procedure MyScene.Release;
Begin
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler

// Simple button event handlers, change UI background color
Procedure MyScene.OnRedClick(Src:Widget);
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(255, 200, 200);
End;

Procedure MyScene.OnGreenClick(Src:Widget);
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(200, 255, 200);
End;

Procedure MyScene.OnBlueClick(Src:Widget);
Begin
  If Assigned(Background) Then
    Background.Color :=  ColorCreate(200, 200, 255);
End;

Procedure MyScene.OnCheckBoxClick(Src:Widget);
Begin
  MyWnd.Draggable := UICheckBox(Src).Checked;
End;

Procedure MyScene.OnShowClick(Src:Widget);
Begin
  MyWnd.Color := ColorWhite;
  MyWnd.Show(widgetAnimatePosY);
  ShowBtn.Hide(widgetAnimateAlpha);
End;

Procedure MyScene.OnCloseClick(Src:Widget);
Begin
  MyWnd.Hide(widgetAnimateAlpha);
  ShowBtn.Show(widgetAnimateAlpha);
End;

// A event handler can handle different widgets if you want
// Just check the name of the src widget to see which widget called this event
Procedure MyScene.OnRadioClick(Src:Widget);
Begin
  If (Not Assigned(Src)) Then
    Exit;

  If (Src.Tag = 0) Then
    MyUI.Color := ColorCreate(255, 200, 200)
  Else
  If (Src.Tag = 1) Then
    MyUI.Color := ColorCreate(200, 255, 200)
  Else
  If (Src.Tag = 2) Then
    MyUI.Color := ColorCreate(200, 200, 255);
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

