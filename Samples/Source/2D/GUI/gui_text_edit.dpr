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
  TERRA_Image,
  TERRA_Texture,
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
      Procedure OnKeyPress(Key:TERRAChar); Override;

			Procedure OnIdle; Override;
  End;

  MyScene = Class(TERRAScene)
      Constructor Create;
      Procedure Release; Override;

      Procedure OnMyButtonClick(Src:UIWidget);
  End;

Var
  Fnt:TERRAFont;
  MyUI:UIView;
  MyWnd, MyEdit:UIWidget;

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

Procedure Demo.OnKeyPress(Key:TERRAChar);
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
Begin
  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', TextureManager.Instance.GetTexture('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIEditTextTemplate.Create('edit_template', TextureManager.Instance.GetTexture('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', MyUI, 0, 0, 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;

  MyEdit := UIInstancedWidget.Create('myedit', MyWnd, 0, 0, 1, UIPixels(350), UIPixels(50), 'edit_template');
  MyEdit.Align := waCenter;
  MyEdit.SetEventHandler(widgetEvent_MouseDown, OnMyButtonClick); // Assign a onClick event handler
End;

Procedure MyScene.Release;
Begin
End;

// GUI event handlers
// All event handlers must be procedures that receive a Widget as argument
// The Widget argument provides the widget that called this event handler
Procedure MyScene.OnMyButtonClick(Src:UIWidget);
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

