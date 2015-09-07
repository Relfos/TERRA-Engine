{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_Engine,
  TERRA_Utils,
  TERRA_String,
  TERRA_Stream,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Viewport,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UITemplates,
  TERRA_UIImage,
  TERRA_UILabel,
  TERRA_UITiledRect,
  TERRA_UIDimension;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
  End;

Var
  MyWnd, MyMapPanel, MyMapImg, MyMapCaption:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(500), UIPixels(500), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;

  MyMapPanel := UIWidgetGroup.Create('MyMapPanel', MyWnd, UIPixels(0), UIPixels(0), 0.1, UIPercent(100), UIPercent(100));
  MyMapPanel.Margin.Top := UIPixels(30);
  MyMapPanel.Margin.Bottom := UIPixels(25);
  MyMapPanel.Margin.Left := UIPixels(10);
  MyMapPanel.Margin.Right := UIPixels(10);

  MyMapImg := UIImage.Create('myimg', MyMapPanel, UIPixels(0), UIPixels(0), 0.1, UIPercent(100), UIPercent(100));
  MyMapImg.SetPropertyValue('image', 'world_map');

  MyMapCaption := UILabel.Create('mycaption', MyMapPanel, UIPixels(0), UIPixels(0), 1, UIPercent(100), UIPercent(100), 'Test');

  //MyText.Align := UIAlign_Center;
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

