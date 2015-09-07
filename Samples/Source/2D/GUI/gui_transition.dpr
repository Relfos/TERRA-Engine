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
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Viewport,
  TERRA_UITransition,
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

      Procedure OnMyButtonClick(Src:UIWidget);
      Procedure OnTransitionFinished(Src:UIWidget);
  End;

Var
  MyWnd, MyBtn:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Sizable := True;
  MyWnd.Align := UIAlign_Center;

  MyBtn := UIInstancedWidget.Create('mybtn', MyWnd, UIPixels(0), UIPixels(0), 1, UIPixels(250), UIPixels(50), 'btn_template');
  MyBtn.Align := UIAlign_Center;
  MyBtn.SetEventHandler(widgetEvent_MouseDown, OnMyButtonClick);
  MyBtn.AddAnimation(widget_Highlighted, 'color', 'FF5555FF');
End;

Procedure MyDemo.OnMyButtonClick(Src:UIWidget);
Var
  MyTransition:UITransition;
Begin
  Src.SetPropertyValue('caption', 'Wait...');

  // note - we should not keep the transition in non-local variable, because it is auto-destroyed once the transition animation finishes!
  MyTransition := UIFade.Create(Self.GUI, Engine.Textures['screen_fade_dots'], 1000, 0);
  MyTransition.SetEventHandler(widgetEvent_Hide, Self.OnTransitionFinished);
End;

Procedure MyDemo.OnTransitionFinished(Src: UIWidget);
Begin
  MyBtn.SetPropertyValue('caption', '[w]Finished![/w]');
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

