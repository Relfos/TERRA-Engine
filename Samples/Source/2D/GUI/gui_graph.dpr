{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_Engine,
  TERRA_Renderer,
  TERRA_Utils,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Viewport,
  TERRA_Color,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UITemplates,
  TERRA_UIImage,
  TERRA_UIGraph,
  TERRA_UILabel,
  TERRA_UITiledRect,
  TERRA_UIDimension;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
      Procedure OnDestroy; Override;
      Procedure OnIdle; Override;
  End;

Var
  MyWnd:UIWidget;
  MyGraph:UISampledGraph;

  MyProp:FloatProperty;

Procedure MyDemo.OnCreate;
Var
  I:Integer;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(400), UIPixels(200), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Sizable := True;
  MyWnd.Align := UIAlign_Center;
 // MyWnd.Align := UIAlign_BottomCenter;
//  MyWnd.Align := UIAlign_RightCenter;
//  MyWnd.Rotation := 45*RAD;

  MyProp := FloatProperty.Create('test', 0.0);

  MyGraph := UISampledGraph.Create('myGraph', MyWnd, UIPixels(0), UIPixels(0), 1, UIPercent(80), UIPercent(60));
  MyGraph.Align := UIAlign_Center;
  MyGraph.Color := ColorGreen;
  //MyGraph.Target := MyProp;

  MyGraph.Target := Engine.Graphics.Renderer.Stats.FindProperty('fps');

End;

Procedure MyDemo.OnDestroy;
Begin
  Inherited;

  ReleaseObject(MyProp);
End;

Procedure MyDemo.OnIdle;
Begin
  Inherited;

  MyProp.Value := Random(30);
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

