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
  MyWnd, MyText, MyScrollPanel:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Var
  IpsuLorem:TERRAString;
  Src:TERRAStream;
Begin
  Inherited;

  Src := Engine.Files['ipsu.txt'];
  If Assigned(Src) Then
  Begin
    Src.ReadLines(IpsuLorem);
    ReleaseObject(Src);
  End;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(300), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;

  MyScrollPanel := UIWidgetGroup.Create('myscrollpanel', MyWnd, UIPixels(0), UIPixels(0), 0.1, UIPercent(100), UIPercent(100));
  MyScrollPanel.Margin.Top := UIPixels(30);
  MyScrollPanel.Margin.Bottom := UIPixels(25);
  MyScrollPanel.Margin.Left := UIPixels(10);
  MyScrollPanel.Margin.Right := UIPixels(10);

  MyText := UILabel.Create('mytext', MyScrollPanel, UIPixels(0), UIPixels(0), 1, UIPercent(100), UIPercent(100), IpsuLorem);
  MyText.AddAnimation(widget_Highlighted, 'color', 'FF5555FF');
  MyText.SetPropertyValue('style.align', 'right');
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

