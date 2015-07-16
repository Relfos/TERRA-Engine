{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector2D, TERRA_Font, TERRA_Texture, TERRA_SpriteManager,
  TERRA_UI, TERRA_FileManager, TERRA_TTF, TERRA_InputManager,
  TERRA_Widgets, TERRA_PNG, TERRA_Scene, TERRA_Color, TERRA_Matrix4x4;

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
(*  MyTex := TextureManager.Instance.GetTexture('background');

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
  End;*)

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
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler

// Simple button event handlers, change UI background color
Function OnRedClick(Src:Widget):Boolean; Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(255, 200, 200);
End;

Function OnGreenClick(Src:Widget):Boolean; Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color := ColorCreate(200, 255, 200);
End;

Function OnBlueClick(Src:Widget):Boolean; Cdecl;
Begin
  If Assigned(Background) Then
    Background.Color :=  ColorCreate(200, 200, 255);
End;

Function OnCheckBoxClick(Src:Widget):Boolean; Cdecl;
Begin
  MyWnd.Draggable := UICheckBox(Src).Checked;
End;

Function OnShowClick(Src:Widget):Boolean; Cdecl;
Begin
  MyWnd.Color := ColorWhite;
  MyWnd.Show(widgetAnimatePosY);
  ShowBtn.Hide(widgetAnimateAlpha);
End;

Function OnCloseClick(Src:Widget):Boolean; Cdecl;
Begin
  MyWnd.Hide(widgetAnimateAlpha);
  ShowBtn.Show(widgetAnimateAlpha);
End;

// A event handler can handle different widgets if you want
// Just check the name of the src widget to see which widget called this event
// Note - Widget names are stored internally in UPPER CAPS
Function OnRadioClick(Src:Widget):Boolean; Cdecl;
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
  MyWnd := UIWindow.Create('mywnd', MyUI, 10, 10, 50, 6, 4);
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;

  // Not all widgets need a parent
  // If you pass Nil as parent, the screen will be considered their parent
  MyText := UILabel.Create('mylabel', MyUI, Nil, 20, 20, 10, 'This is an aligned text label.');
  MyText.Align := waBottomRight;

  ShowBtn := UIButton.Create('showbtn', MyUI, Nil, 20, 10, 10, 'Show');
  ShowBtn.Align := waTopRight;
  ShowBtn.Visible := False;   // Some elements can start invisible, and only appear when you need them
  ShowBtn.OnMouseClick := OnShowClick; // Assign a onClick event handler
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
