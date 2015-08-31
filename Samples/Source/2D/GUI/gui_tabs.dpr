{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_EngineManager,
  TERRA_String,
  TERRA_Utils,
  TERRA_Color,
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

      Procedure OnMyButtonClick(Src:UIWidget);

      Procedure OnTabDown(Src:UIWidget);
  End;


Const
  TabCount = 3;

  TabText:Array[1..TabCount] Of TERRAString = ('Red', 'Yellow', 'Blue');
  TabColor:Array[1..TabCount] Of ColorRGBA = ((R:255; G:100; B:100; A:255), (R:255; G:255; B:100; A:255), (R:100; G:100; B:255; A:255) );

Var
  MyWnd, MyBtn:UIWidget;
  MyText:UILabel;
  MyImg:UIImage;
  MyTabGroups, MyTabs:Array[1..TabCount] Of UIWidget;
  SelectedTab:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
Begin
  Inherited;

  UITemplates.AddTemplate(UIButtonTemplate.Create('tab_template', Engine.Textures.GetItem('ui_tab_on'), 4, 4, 54, 21));

  UITemplates.AddTemplate(UITabbedWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98, TabCount, 'tab_template'));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;

  For I:=1 To TabCount Do
  Begin
    MyTabGroups[I] := MyWnd.GetChildByName('tab_group', I);
    MyTabs[I] := MyWnd.GetChildByName('tab_button', I);

    If Assigned(MyTabs[I]) Then
    Begin
      MyTabs[I].SetPropertyValue('caption', TabText[I]);
      MyTabs[I].SetEventHandler(widgetEvent_MouseDown, Self.OnTabDown);
      MyTabs[I].Color := TabColor[I];
      MyTabs[I].AddAnimation(widget_Default, 'color', ColorProperty.Stringify(TabColor[I]));
      MyTabs[I].AddAnimation(widget_Highlighted, 'color', 'FFFFFFFF');
    End;
  End;

  MyBtn := UIInstancedWidget.Create('mybtn', MyTabGroups[1], UIPixels(0), UIPixels(0), 1, UIPixels(250), UIPixels(50), 'btn_template');
  MyBtn.Align := UIAlign_Center;
  MyBtn.SetEventHandler(widgetEvent_MouseDown, Self.OnMyButtonClick);
//  MyBtn.Draggable := True;
//  MyBtn.SetPropertyValue('caption', 'custom caption!');

  MyImg := UIImage.Create('sampleimg', MyTabGroups[2], UIPixels(0), UIPixels(0), 1, UIPixels(64), UIPixels(64));
  MyImg.Align := UIAlign_Center;
  MyImg.Texture := Engine.Textures['ghost'];

  MyText := UILabel.Create('title', MyTabGroups[3], UIPixels(0), UIPixels(30), 1, UIPercent(100), UIPixels(64), 'This is a label that belongs to a tab!');
  MyText.Align := UIAlign_BottomCenter;
End;

Procedure MyDemo.OnMyButtonClick(Src: UIWidget);
Begin

End;

Procedure MyDemo.OnTabDown(Src: UIWidget);
Var
  I:Integer;
Begin
  If SelectedTab = Nil Then
    SelectedTab := MyTabs[1];

  If (SelectedTab = Src) Then
    Exit;

  SelectedTab := Src;

  For I:=1 To TabCount Do
    MyTabGroups[I].Visible := (I = Src.ID);
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

