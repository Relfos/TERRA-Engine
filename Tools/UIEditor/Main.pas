unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, IceTabSet, Grids, ValEdit, ComCtrls,
  TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_VCLApplication, TERRA_OS,
  TERRA_String, TERRA_Scene, TERRA_Texture, TERRA_Font, TERRA_TTF,
  TERRA_Viewport, TERRA_FileManager, TERRA_SpriteManager, TERRA_PNG,
  TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color,
  TERRA_UI, TERRA_XML, TERRA_CustomPropertyEditor;

Const
  SnapValue = 10;

Type
  UIEditTool = (
    uitool_Empty,
    uitool_Button,
    uitool_Label
  );

  UIEditScene = Class;

  UIEditableView = Class(TERRAObject)
    Protected
      _Owner:UIEditScene;
      _Name:TERRAString;
      _Tab:TIceTab;
      _Target:UI;

    Public
      Constructor Create(Const Name:TERRAString; Owner:UIEditScene);
      Procedure Release(); Override;

      Function PickWidgetAt(X, Y:Integer):Widget;
  End;

  UIEditScene = Class(Scene)
    Protected
      _Font:Font;

      _ViewList:Array Of UIEditableView;
      _ViewCount:Integer;

      _SelectedView:UIEditableView;
      _SelectedWidget:Widget;
      _LastWidget:Widget;

      _CurrentTool:UIEditTool;

    Public
      Constructor Create();

      Procedure Release(); Override;

      Procedure RenderSprites(V:Viewport); Override;

      Procedure AddView(Const Name:TERRAString);

      Procedure AddWidget(W:Widget);
      Procedure AddButton(X, Y:Integer);
      Procedure AddLabel(X, Y:Integer);

      Procedure SelectWidget(W:Widget);
  End;

  TUIEditForm = class(TForm)
    TabList: TIceTabSet;
    RenderPanel: TPanel;
    MainMenu1: TMainMenu;
    Project1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Project2: TMenuItem;
    N3: TMenuItem;
    View1: TMenuItem;
    Component1: TMenuItem;
    View2: TMenuItem;
    Add1: TMenuItem;
    WidgetList: TTreeView;
    Button1: TMenuItem;
    Label1: TMenuItem;
    Window1: TMenuItem;
    Checkbox1: TMenuItem;
    Radiobox1: TMenuItem;
    Combobox1: TMenuItem;
    Icon1: TMenuItem;
    Sprite1: TMenuItem;
    PropertyList: TCustomPropertyEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

    Function AddNewTab(Const Name:TERRAString):TIceTab;
    procedure Button1Click(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WidgetListClick(Sender: TObject);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1Click(Sender: TObject);

  Protected
    Function FindWidgetNode(W:Widget):TTreeNode;

  Private
    _Scene:UIEditScene;
    _DragTarget:Widget;

  Public

    Property Scene:UIEditScene Read _Scene;
  End;

Var
  UIEditForm: TUIEditForm;

implementation
Uses TERRA_UIButton, TERRA_UILabel;

{$R *.dfm}

Var
  _Tex:Texture = Nil;

{ UIEditScene }
Constructor UIEditScene.Create;
Begin
  _CurrentTool := uitool_Empty;
  Self._ViewCount := 0;

  // Load a font
  Self._Font := FontManager.Instance.GetFont('droid');

  // set background color
  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorGrey(128);

  Self.AddView('Untitled');
End;

Procedure UIEditScene.AddWidget(W: Widget);
Begin
  Self._LastWidget := Self._SelectedWidget;
  _CurrentTool := uitool_Empty;

  UIEditForm.WidgetList.Items.AddChild(UIEditForm.FindWidgetNode(Nil), W.Name);

  UIEditForm.FormResize(UIEditForm.WidgetList);

  Self.SelectWidget(W);
End;

Procedure UIEditScene.AddButton(X, Y: Integer);
Begin
  Self.AddWidget(UIButton.Create('button', Self._SelectedView._Target,
    X, Y, 0.1,
    UIPixels(150), UIPixels(50), 'Button', 'round_button'));
End;

Procedure UIEditScene.AddLabel(X, Y: Integer);
Begin
  Self.AddWidget(UILabel.Create('label', Self._SelectedView._Target,
    X, Y, 0.1, 'text'));
End;

Procedure UIEditScene.AddView(Const Name:TERRAString);
Var
  V:UIEditableView;
Begin
  V := UIEditableView.Create(Name, Self);

  Inc(_ViewCount);
  SetLength(_ViewList, _ViewCount);
  _ViewList[Pred(_ViewCount)] := V;

  _SelectedView := V;
End;


Procedure UIEditScene.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewCount) Do
    ReleaseObject(_ViewList[I]);
End;

Procedure UIEditScene.RenderSprites(V: Viewport);
Var
  S:QuadSprite;
  Angle:Single;
Begin
  // A rotating sprite in the bottom, with Scale = 4x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := SpriteManager.Instance.DrawSprite(100, 100, 50, _Tex);
  S.SetScaleAndRotationRelative(VectorCreate2D(0.5, 0.5), 4.0, Angle);  // Calculate rotation, in degrees, from current time
End;

Procedure UIEditScene.SelectWidget(W: Widget);
Begin
  If (W = _SelectedWidget) Then
    Exit;

  _SelectedWidget := W;
  UIEditForm.PropertyList.Target := _SelectedWidget;
end;

{ TUIEditForm }
Function TUIEditForm.AddNewTab(Const Name:TERRAString):TIceTab;
begin
  Result := TabList.AddTab(Name);
end;

Procedure TUIEditForm.FormCreate(Sender: TObject);
Begin
  VCLApplication.Create(Self.RenderPanel);

  // Added Asset folder to search path
  FileManager.Instance.AddPath('..\..\samples\binaries\assets');

  // Load a Tex
  _Tex := TextureManager.Instance['ghost'];

  // Create a scene and set it as the current scene
  _Scene := UIEditScene.Create();
  GraphicsManager.Instance.SetScene(_Scene);
End;


procedure TUIEditForm.FormDestroy(Sender: TObject);
begin
  ReleaseObject(_Scene);
  Application.Instance.Terminate();
end;

procedure TUIEditForm.FormResize(Sender: TObject);
begin
  PropertyList.Visible := WidgetList.Items.Count>0;

  If PropertyList.Visible Then
  Begin
    WidgetList.Height := Self.Height Div 3;
    PropertyList.Top := WidgetList.Height + WidgetList.Top;
    PropertyList.Height := Self.Height - PropertyList.Top;
  End Else
  Begin
    WidgetList.Height := Self.Height -WidgetList.Top;
  End;

  RenderPanel.Left := PropertyList.Width + PropertyList.Left;
  RenderPanel.Width := Self.Width - RenderPanel.Left;
  RenderPanel.Height := Self.Height - RenderPanel.Top;
end;

procedure TUIEditForm.Button1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Button;
end;

procedure TUIEditForm.Label1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Label;
end;

procedure TUIEditForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  W:Widget;
begin
  Case Scene._CurrentTool Of
  uitool_Empty:
    If Assigned(Scene._SelectedView) Then
    Begin
      W := Scene._SelectedView.PickWidgetAt(X, Y);
      Scene.SelectWidget(W);

      If Assigned(W) Then
      Begin
        _DragTarget := W;
        _DragTarget.BeginDrag(X, Y);
      End;
    End;

  uitool_Button:
    Begin
      Self.Scene.AddButton(X, Y);
    End;

  uitool_Label:
    Begin
      Self.Scene.AddLabel(X, Y);
    End;

  End;

end;

procedure TUIEditForm.RenderPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(_DragTarget) Then
  Begin
    _DragTarget.FinishDrag();
    PropertyList.RequestUpdate();
  end;

end;

procedure TUIEditForm.RenderPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(_DragTarget) Then
  Begin
    If ssCtrl in Shift Then
    Begin
      X := (X Div SnapValue) * SnapValue;
      Y := (Y Div SnapValue) * SnapValue;
    End;

    _DragTarget.OnMouseMove(X, Y);
  end;
End;

Function TUIEditForm.FindWidgetNode(W: Widget): TTreeNode;
Begin
  Result := Nil;
End;


procedure TUIEditForm.WidgetListClick(Sender: TObject);
Var
  Node:TTreeNode;
  Source:Widget;
begin
  Node := Self.WidgetList.Selected;

  If Node = Nil Then
  Begin
    PropertyList.Target := Nil;
    Exit;
  End;

  Scene.SelectWidget(Scene._SelectedView._Target.GetWidget(Node.Text));
end;



{ UIEditableView }
Constructor UIEditableView.Create(const Name: TERRAString; Owner:UIEditScene);
Begin
  Self._Name := Name;
  Self._Owner := Owner;
  Self._Tab := UIEditForm.AddNewTab(Name);

  // Create a new UI
  Self._Target := UI.Create();

  // Register the font with the UI
  _Target.DefaultFont := Self._Owner._Font;

  // Load a GUI skin
  _Target.LoadSkin('ui_sample_skin');
End;

Function UIEditableView.PickWidgetAt(X, Y: Integer): Widget;
Begin
  Result := _Target.PickWidget(X, Y);
End;

Procedure UIEditableView.Release;
Begin
  UIManager.Instance.RemoveUI(_Target);
  ReleaseObject(_Target);
End;



end.
