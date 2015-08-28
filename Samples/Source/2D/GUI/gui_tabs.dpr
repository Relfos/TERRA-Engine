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
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Viewport,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UILabel,
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

Const
  TabCount = 3;

Var
  MyWnd, MyBtn:UIWidget;
  MyText:UILabel;
  MyImg:UIImage;
  MyTabGroups:Array[1..TabCount] Of UIWidget;

  MyController:UIController;

{ Game }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
Begin
  Inherited;

  UITemplates.AddTemplate(UITabbedWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98, TabCount));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  UITemplates.AddTemplate(UIButtonTemplate.Create('tab_template', Engine.Textures.GetItem('ui_tab_on'), 4, 4, 54, 21));

  MyController := DemoUIController.Create();

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;
  MyWnd.Controller := MyController;

  For I:=1 To TabCount Do
    MyTabGroups[I] := MyWnd.GetChildByName('tab_group', I);

    (*
  MyBtn := UIInstancedWidget.Create('mybtn', MyTabGroups[1], UIPixels(0), UIPixels(0), 1, UIPixels(250), UIPixels(50), 'btn_template');
  MyBtn.Align := waCenter;
  MyBtn.Controller := MyController;
//  MyBtn.Draggable := True;
//  MyBtn.SetPropertyValue('caption', 'custom caption!');

  MyImg := UIImage.Create('sampleimg', MyTabGroups[2], UIPixels(0), UIPixels(30), 1, UIPixels(64), UIPixels(64));
  MyImg.Align := waTopCenter;
  MyImg.Texture := Engine.Textures['ghost'];

  MyText := UILabel.Create('title', MyTabGroups[3], UIPixels(0), UIPixels(30), 1, UIPercent(100), UIPixels(64), 'This is a label that belongs to a tab!');
  MyText.Align := waBottomCenter;*)
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

