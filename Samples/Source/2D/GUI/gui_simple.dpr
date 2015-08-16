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
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_CollectionObjects,
  TERRA_TTF,
  TERRA_PNG,
  TERRA_Math,
  TERRA_Scene,
  TERRA_Color,
  TERRA_String,
  TERRA_Matrix4x4,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UITemplates,
  TERRA_UIImage,
  TERRA_UITiledRect,
  TERRA_UIDimension;

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

  DemoUIController = Class(UIController)
    Public
      Constructor Create();

      Procedure OnMyButtonClick(Src:UIWidget);
  End;
  
  MyScene = Class(TERRAScene)
      Constructor Create;
      Procedure Release; Override;
  End;

Var
  Fnt:TERRAFont;
  MyUI:UIView;
  MyWnd, MyBtn:UIWidget; 
  MyController:UIController;

{ Game }
Procedure Demo.OnCreate;
Begin
  FileManager.Instance.AddPath('Assets');

  // Load a font
  Fnt := FontManager.Instance.GetFont('droid');

  // Create a new UI
  MyUI := UIView.Create;

  // Register the font with the UI
  MyUI.DefaultFont := Fnt;

  // Create a empty scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;
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
Begin
  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', TextureManager.Instance.GetTexture('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', TextureManager.Instance.GetTexture('ui_button2'), 25, 10, 220, 37));

  MyController := DemoUIController.Create();

  MyWnd := UIInstancedWidget.Create('mywnd', MyUI, 0, 0, 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;
  MyWnd.Rotation := 45*RAD;
  MyWnd.Controller := MyController;

  MyBtn := UIInstancedWidget.Create('mybtn', MyWnd, 0, 0, 1, UIPixels(250), UIPixels(50), 'btn_template');
  MyBtn.Align := waCenter;
  MyBtn.Controller := MyController;
//  MyBtn.Draggable := True;

//  MyBtn.SetPropertyValue('caption', 'custom caption!');
//  MyBtn.Rotation := 45*RAD;
End;

Procedure MyScene.Release;
Begin
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler
Constructor DemoUIController.Create;
Begin
  Self._ObjectName := 'demo';
  SetHandler(widgetEvent_MouseDown, OnMyButtonClick); // Assign a onClick event handler
End;

Procedure DemoUIController.OnMyButtonClick(Src:UIWidget);
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

