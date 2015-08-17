{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
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
  TERRA_Scene,
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
  End;


  DemoUIController = Class(UIController)
    Public
      Constructor Create();

      Procedure OnMyButtonClick(Src:UIWidget);
  End;

Var
  Fnt:TERRAFont;
  MyController:UIController;
  MyWnd, MyEdit:UIWidget;
  MyBtn, MyBtn2:UIWidget;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  UITemplates.AddTemplate(UIWindowTemplate.Create('wnd_template', TextureManager.Instance.GetTexture('ui_window'), 45, 28, 147, 98));
  UITemplates.AddTemplate(UIEditTextTemplate.Create('edit_template', TextureManager.Instance.GetTexture('ui_button2'), 25, 10, 220, 37));
  UITemplates.AddTemplate(UIButtonTemplate.Create('btn_template', TextureManager.Instance.GetTexture('ui_button2'), 25, 10, 220, 37));

  MyController := DemoUIController.Create();

  MyWnd := UIInstancedWidget.Create('mywnd', Self.Scene.GUI, 0, 0, 10, UIPixels(643), UIPixels(231), 'wnd_template');
  MyWnd.Draggable := True;
  MyWnd.Align := waCenter;

  MyEdit := UIInstancedWidget.Create('myedit', MyWnd, 0, 0, 1, UIPixels(450), UIPixels(50), 'edit_template');
  MyEdit.Align := waCenter;

  MyBtn := UIInstancedWidget.Create('mybtn', MyWnd, -150, 25, 1, UIPixels(150), UIPixels(50), 'btn_template');
  MyBtn.Align := waBottomCenter;
  MyBtn.Controller := MyController;
  MyBtn.SetPropertyValue('value', 'Password');

  MyBtn.AddAnimation(widget_Default, 'value', 'Password');
  MyBtn.AddAnimation(widget_Selected, 'value', 'Normal');

  MyBtn2 := UIInstancedWidget.Create('mybtn', MyWnd, 150, 25, 1, UIPixels(150), UIPixels(50), 'btn_template');
  MyBtn2.Align := waBottomCenter;
  MyBtn2.Controller := MyController;
  MyBtn2.SetPropertyValue('value', 'Spin');

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
Var
  Text:UIEditText;
  Prop:TweenableProperty;
Begin
  If (Src = MyBtn2) Then
  Begin
    Prop := TweenableProperty(MyWnd.FindProperty('rotation'));

    If Assigned(Prop) Then
    Begin
      If MyWnd.Rotation<>0 Then
        Prop.AddTweenFromBlob(easeLinear, '90', '0', 1000)
      Else
        Prop.AddTweenFromBlob(easeLinear, '0', '90', 1000);
    End;

    Exit;
  End;

  Text := UIEditText(MyEdit.FindComponent(UIEditText));
  If Text = Nil Then
    Exit;

  Text.PasswordField := Not Text.PasswordField;

  //Src.Selected := Not Src.Selected;

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

