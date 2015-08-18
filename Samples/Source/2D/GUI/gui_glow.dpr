{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_EngineManager,
  TERRA_Utils,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_Renderer,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Font,
  TERRA_Texture,
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_Viewport,
  TERRA_TTF,
  TERRA_PNG,
  TERRA_Math,
  TERRA_Scene,
  TERRA_Color,
  TERRA_String,
  TERRA_ScreenFX,
  TERRA_Matrix4x4,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UITemplates,
  TERRA_UIImage,
  TERRA_UITiledRect,
  TERRA_UIDimension;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
  End;


  DemoUIController = Class(UIController)
    Public
      Constructor Create();

      Procedure OnMyButtonClick(Src:UIWidget);
  End;

Var
  MyWnd, MyBtn:UIWidget;
  MyController:UIController;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UIManager.Instance.Viewport.FXChain.AddEffect(GlowFX.Create());
  //UIManager.Instance.Viewport.FXChain.AddEffect(BloomFX.Create());
  //UIManager.Instance.Viewport.FXChain.AddEffect(VignetteFX.Create());


  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyController := DemoUIController.Create();

  MyWnd := UIInstancedWidget.Create('mywnd', Self.Scene.GUI, 0, 0, 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;
//  MyWnd.Rotation := 45*RAD;
  MyWnd.Controller := MyController;

  MyBtn := UIInstancedWidget.Create('mybtn', MyWnd, 0, 0, 1, UIPixels(250), UIPixels(50), 'btn_template');
  MyBtn.Align := waCenter;
  MyBtn.Controller := MyController;
//  MyBtn.Draggable := True;

//  MyBtn.SetPropertyValue('caption', 'custom caption!');
//  MyBtn.Rotation := 45*RAD;
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
  Src.Hide();
 // MyUI.MessageBox('You clicked the button!');
End;


{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}

End.

