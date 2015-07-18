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
  TERRA_UIDimension,
  TERRA_UIButton;

Type
  Demo = Class(Application)
    Protected
      _Scene:TERRAScene;

    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnMouseDown(X,Y:Integer; Button:Word); Override;
			Procedure OnMouseUp(X,Y:Integer; Button:Word); Override;
			Procedure OnMouseMove(X,Y:Integer); Override;

			Procedure OnIdle; Override;
  End;

  MyScene = Class(TERRAScene)
      Constructor Create;
      Procedure Release; Override;

      Procedure OnMyButtonClick(Src:Widget);
  End;

Var
  Fnt:TERRAFont;
  MyUI:UI;
  MyWnd:UIWindow;
  MyBtn:UIButton;

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
  I:Integer;
Begin
  MyWnd := UIWindow.Create('mywnd', MyUI, 0, 0, 10, UIPixels(300), UIPixels(150), 'window');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;

  MyBtn := UIButton.Create('btn1', MyWnd, 0, 0, 10, UIPixels(150), UIPixels(50), 'Click me', 'round_button');
  MyBtn.OnMouseClick := OnMyButtonClick; // Assign a onClick event handler
  MyBtn.Align := waCenter;
End;

Procedure MyScene.Release;
Begin
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler
Procedure MyScene.OnMyButtonClick(Src:Widget);
Begin
  IntToString(2);
 // MyUI.MessageBox('You clicked the button!');
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

