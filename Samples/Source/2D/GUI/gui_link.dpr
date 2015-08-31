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


Const
  Content =  'This is a [i]link[/i] [color=darkviolet]example[/color], try clicking here:'+CrLf+'[url=www.pascalgameengine.com]Linky![/url]'+CrLf+CrLf+'[w]Wavy[/w], yes?';
Var
  MyWnd, MyText:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;
//  MyWnd.Rotation := 45*RAD;

  MyText := UILabel.Create('mytext', MyWnd, UIPixels(0), UIPixels(0), 1, UIPercent(100), UIPercent(60), Content);
  MyText.Align := UIAlign_Center;

//  MyBtn.SetPropertyValue('caption', 'custom caption!');
//  MyBtn.Rotation := 45*RAD;
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

