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
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
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
  TERRA_Color,
  TERRA_String,
  TERRA_ScreenFX,
  TERRA_Matrix4x4,
  TERRA_Tween,
  TERRA_UIView,
  TERRA_UIWidget,
  TERRA_UITemplates,
  TERRA_UIImage,
  TERRA_UILabel,
  TERRA_UIEditText,
  TERRA_UITiledRect,
  TERRA_UIDimension;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;

      Procedure OnTogglePassword(Src:UIWidget);
      Procedure OnButtonSpin(Src:UIWidget);
  End;

Var
  Fnt:TERRAFont;
  MyWnd, MyEdit:UIWidget;
  MyBtn, MyBtn2:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', Engine.Textures.GetItem('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIEditTextTemplate.Create('edit_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', Engine.Textures.GetItem('ui_button2'), 25, 10, 220, 37));

  MyWnd := UIInstancedWidget.Create('mywnd', Self.GUI, UIPixels(0), UIPixels(0), 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := UIAlign_Center;

  MyEdit := UIInstancedWidget.Create('myedit', MyWnd, UIPixels(0), UIPixels(-20), 1, UIPixels(450), UIPixels(50), 'edit_template');
  MyEdit.Align := UIAlign_Center;

  MyBtn := UIInstancedWidget.Create('mybtn', MyWnd, UIPixels(-150), UIPixels(40), 1, UIPixels(150), UIPixels(50), 'btn_template');
  MyBtn.Align := UIAlign_BottomCenter;
  MyBtn.SetEventHandler(widgetEvent_MouseDown, Self.OnTogglePassword);
  MyBtn.SetPropertyValue('value', 'Password');

  MyBtn.AddAnimation(widget_Default, 'value', 'Password');
  MyBtn.AddAnimation(widget_Selected, 'value', 'Normal');

  MyBtn2 := UIInstancedWidget.Create('mybtn', MyWnd, UIPixels(150), UIPixels(40), 1, UIPixels(150), UIPixels(50), 'btn_template');
  MyBtn2.Align := UIAlign_BottomCenter;
  MyBtn2.SetEventHandler(widgetEvent_MouseDown, Self.OnButtonSpin);
  MyBtn2.SetPropertyValue('value', 'Spin');
End;

Procedure MyDemo.OnButtonSpin(Src:UIWidget);
Var
  Prop:TweenableProperty;
Begin
  Prop := TweenableProperty(MyWnd.FindProperty('rotation'));

  If Assigned(Prop) Then
  Begin
    If MyWnd.Rotation<>0 Then
      Prop.AddTweenFromBlob(easeLinear, '90', '0', 1000)
    Else
      Prop.AddTweenFromBlob(easeLinear, '0', '90', 1000);
  End;
End;

Procedure MyDemo.OnTogglePassword(Src:UIWidget);
Var
  Text:UIEditText;
Begin
  Text := UIEditText(MyEdit.FindComponent(UIEditText));
  If Text = Nil Then
    Exit;

  Text.PasswordField := Not Text.PasswordField;

  //Src.Selected := Not Src.Selected;
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

