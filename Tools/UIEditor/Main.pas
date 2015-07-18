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

  customDefault = crArrow;
  customMove    = crSize;
  customHorizontal  = crSizeWE;
  customVertical  = crSizeNS;
  customText = crIBeam;
  customLink = crHandPoint;

Type
  UIEditTool = (
    uitool_Empty,
    uitool_Window,
    uitool_Button,
    uitool_Label,
    uitool_Checkbox,
    uitool_Radiobutton,
    uitool_ProgressBar
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
      _DragMode:UIDragMode;

      Function GetNewTarget():Widget;

    Public
      Constructor Create();

      Procedure Release(); Override;

      Procedure RenderSprites(V:Viewport); Override;

      Procedure AddView(Const Name:TERRAString);

      Procedure AddWidget(W:Widget; X,Y:Integer);
      Procedure AddWindow(X, Y:Integer);
      Procedure AddButton(X, Y:Integer);
      Procedure AddLabel(X, Y:Integer);
      Procedure AddCheckbox(X, Y:Integer);
      Procedure AddRadioButton(X, Y:Integer);
      Procedure AddProgressBar(X,Y:Integer);

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
    ProgressBar1: TMenuItem;
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
    procedure Checkbox1Click(Sender: TObject);
    procedure ProgressBar1Click(Sender: TObject);
    procedure Radiobox1Click(Sender: TObject);
    procedure Window1Click(Sender: TObject);

  Protected
    Procedure CustomDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    Procedure CustomMeasureItem(Sender: TObject; ACanvas: TCanvas;  var Width, Height: Integer);


    Function FindWidgetNode(W:Widget):TTreeNode;
    Procedure UpdateWidgetTree();

    Procedure LoadCursor(ID:Integer; Name:TERRAString);
    Procedure LoadSkin();
    Procedure SetMenuSkin(Menu:TMenuItem);

    Procedure ChangeCursor(ID:Integer);

  Private
    _Scene:UIEditScene;
    _DragTarget:Widget;
    _Brush:TBrush;

  Public

    Property Scene:UIEditScene Read _Scene;
  End;

Var
  UIEditForm: TUIEditForm;
  SkinBGColor, SkinForeColor, SkinTextColor, SkinEditColor:TColor;


implementation
Uses TERRA_UIDimension, TERRA_UIWindow, TERRA_UIButton, TERRA_UILabel, TERRA_UICheckbox, TERRA_UIRadioButton, TERRA_UIProgressBar;

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

Procedure UIEditScene.AddWidget(W: Widget; X,Y:Integer);
Var
  Node:TTreeNode;
Begin
  Self._LastWidget := Self._SelectedWidget;
  _CurrentTool := uitool_Empty;

  If (W.Parent <> Self._SelectedView._Target) And (Assigned(W.Parent)) Then
  Begin
    X := Trunc(X - W.Parent.AbsolutePosition.X);
    Y := Trunc(Y - W.Parent.AbsolutePosition.Y);
    W.RelativePosition := VectorCreate2D(X, Y);
  End;

  Node := UIEditForm.WidgetList.Items.AddChildObject(UIEditForm.FindWidgetNode(W.Parent), W.Name, W);
  Node.Selected := True;

  UIEditForm.FormResize(UIEditForm.WidgetList);

  UIEditForm.UpdateWidgetTree();

  Self.SelectWidget(W);
End;


Procedure UIEditScene.AddWindow(X, Y: Integer);
Begin
  Self.AddWidget(UIWindow.Create('window', Self.GetNewTarget(),
    X, Y, 0.1,
    UIPixels(300), UIPixels(200),  'window'), X, Y);
End;


Procedure UIEditScene.AddButton(X, Y: Integer);
Begin
  Self.AddWidget(UIButton.Create('button', Self.GetNewTarget(),
    X, Y, 0.1,
    UIPixels(150), UIPixels(50), 'Button', 'round_button'), X, Y);
End;

Procedure UIEditScene.AddLabel(X, Y: Integer);
Begin
  Self.AddWidget(UILabel.Create('label', Self.GetNewTarget(), X, Y, 0.1, 'text'), X, Y);
End;

procedure UIEditScene.AddCheckbox(X, Y: Integer);
Begin
  Self.AddWidget(UICheckbox.Create('check', Self.GetNewTarget(), X, Y, 0.1, UIPixels(25), True, 'text', 'checkbox'), X, Y);
End;

procedure UIEditScene.AddRadioButton(X, Y: Integer);
Begin
  Self.AddWidget(UIRadioButton.Create('radio', Self.GetNewTarget(), X, Y, 0.1, UIPixels(25), 'text', 'checkbox'), X, Y);
End;

procedure UIEditScene.AddProgressBar(X, Y: Integer);
Var
  P:UIProgressBar;
begin
  P := UIProgressBar.Create('bar', Self.GetNewTarget(), X, Y, 0.1, UIPixels(200), UIPixels(30), 'progressbar');
  P.Percent.Value := 50;
  Self.AddWidget(P, X, Y);
end;

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

Function UIEditScene.GetNewTarget: Widget;
Begin
  If Assigned(_SelectedWidget) Then
    Result := _SelectedWidget
  Else
    Result := _SelectedView._Target;
End;

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

  _Brush := TBrush.Create();

  Self.LoadSkin();
End;


procedure TUIEditForm.FormDestroy(Sender: TObject);
begin
  _Brush.Destroy();

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


procedure TUIEditForm.Window1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Window;
end;

procedure TUIEditForm.Label1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Label;
end;

procedure TUIEditForm.Checkbox1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Checkbox;
end;

procedure TUIEditForm.Radiobox1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Radiobutton;
end;

procedure TUIEditForm.ProgressBar1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_ProgressBar;
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
        _DragTarget.BeginDrag(X, Y, _Scene._DragMode);
      End;
    End;

  uitool_Window:
    Begin
      Self.Scene.AddWindow(X, Y);
    End;

  uitool_Button:
    Begin
      Self.Scene.AddButton(X, Y);
    End;

  uitool_Label:
    Begin
      Self.Scene.AddLabel(X, Y);
    End;

  uitool_Checkbox:
    Begin
      Self.Scene.AddCheckbox(X, Y);
    End;

  uitool_Radiobutton:
    Begin
      Self.Scene.AddRadioButton(X, Y);
    End;

  uitool_ProgressBar:
    Begin
      Self.Scene.AddProgressBar(X, Y);
    End;


  End;

end;

procedure TUIEditForm.RenderPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(_DragTarget) Then
  Begin
    _DragTarget.FinishDrag();
    PropertyList.RequestUpdate();

    _DragTarget := Nil;
  end;

end;

procedure TUIEditForm.RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Const
  WidgetBorder = 10;
Var
  W:Widget;
  PX, PY, TargetCursor:Integer;
begin
  If Assigned(_DragTarget) Then
  Begin
    If ssCtrl in Shift Then
    Begin
      X := (X Div SnapValue) * SnapValue;
      Y := (Y Div SnapValue) * SnapValue;
    End;

    _DragTarget.OnMouseMove(X, Y);

    Exit;
  end;

  W := Scene._SelectedView.PickWidgetAt(X, Y);
  If Assigned(W) Then
  Begin
    PX := X - Trunc(W.AbsolutePosition.X);
    PY := Y - Trunc(W.AbsolutePosition.Y);

    If (PX<=WidgetBorder) Then
    Begin
      _Scene._DragMode := UIDrag_Left;
      TargetCursor := customHorizontal;
    End Else
    If (PY<=WidgetBorder) Then
    Begin
      _Scene._DragMode := UIDrag_Top;
      TargetCursor := customVertical;
    End Else
    If (PX>=Trunc(W.Size.X - WidgetBorder)) Then
    Begin
      _Scene._DragMode := UIDrag_Right;
      TargetCursor := customHorizontal;
    End Else
    If (PY>=Trunc(W.Size.Y - WidgetBorder)) Then
    Begin
      _Scene._DragMode := UIDrag_Bottom;
      TargetCursor := customVertical;
    End Else
    Begin
      _Scene._DragMode := UIDrag_Move;
      TargetCursor := customMove;
    End;

    If (Not W.SupportDrag(_Scene._DragMode)) Then
    Begin
      _Scene._DragMode := UIDrag_Move;
      TargetCursor := customMove;
    End;

    ChangeCursor(TargetCursor);
  End Else
    ChangeCursor(customDefault);
End;

Function TUIEditForm.FindWidgetNode(W: Widget): TTreeNode;
Var
  I:Integer;
Begin
  For I:=0 To Pred(WidgetList.Items.Count) Do
  If (WidgetList.Items[I].Data = W) Then
  Begin
    Result := WidgetList.Items[I];
    Exit;
  End;

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

Procedure TUIEditForm.UpdateWidgetTree;
Var
  I:Integer;
  N:TTreeNode;
Begin
  For I:=0 To Pred(WidgetList.Items.Count) Do
  Begin
    N := WidgetList.Items[I];
    N.Text := Widget(N.Data).Name;
  End;
End;

Procedure TUIEditForm.CustomDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
Const
  LinePad = 5;
var
  MY:Integer;
  Text:TERRAString;
  Flags: Integer;
Begin
  Flags      := DT_VCENTER Or DT_CENTER;

  Text := Trim(TMenuItem(Sender).Caption);

  ACanvas.Brush.Style := bsSolid;


  //ACanvas.FillRect(ARect);

  If (TMenuItem(Sender).IsLine) Then
  Begin
    MY := (ARect.Top + ARect.Bottom) Shr 1;
    ACanvas.Pen.Color := clBtnShadow; //SkinBGColor;
    ACanvas.MoveTo(ARect.Left + LinePad, MY);
    ACanvas.LineTo(ARect.Right - LinePad, MY);
  End Else
  Begin
    ACanvas.Brush.Color := SkinForeColor;
    ACanvas.Font.Color := SkinTextColor;
    DrawTextA(ACanvas.Handle, PAnsiChar(Text), StringLength(Text), ARect, Flags);
  End;
  //ACanvas.TextOut(LeftPos, TopPos, Text);
  //ACanvas.TextRect(ARect, ARect.Left, ARect.Top, Text);*)
End;

Procedure TUIEditForm.CustomMeasureItem(Sender: TObject; ACanvas: TCanvas; Var Width, Height: Integer);
Var
  Text:TERRAString;
Begin
  Text := Trim(TMenuItem(Sender).Caption);

  Width := 100;
 // Height := 40;
End;

Procedure TUIEditForm.LoadCursor(ID:Integer; Name: TERRAString);
Var
  Cur:HCURSOR;
  S:TERRAString;
Begin
  S := FileManager.Instance.SearchResourceFile('cursor_'+Name+'.cur');
  If S = '' Then
    S := FileManager.Instance.SearchResourceFile('cursor_'+Name+'.ani');

  Cur := LoadCursorFromFile(PAnsiChar(S));
  Screen.Cursors[ID] := Cur;
End;

Procedure TUIEditForm.LoadSkin;
Var
  MenuInfo:TMenuInfo;
Begin
  LoadCursor(customDefault, 'normal');
  LoadCursor(customMove, 'move');
  LoadCursor(customHorizontal, 'horizontal');
  LoadCursor(customVertical, 'vertical');
  LoadCursor(customText, 'text');
  LoadCursor(customLink, 'link');

  ChangeCursor(customDefault);

  SkinBGColor := RGB(38, 38, 38);
  SkinForeColor := RGB(70, 70, 70);
  SkinTextColor := RGB(254, 163, 0);
  SkinEditColor := RGB(128, 128, 128);


  TabList.BackgroundStartColor := SkinBGColor;
  TabList.BackgroundStopColor := TabList.BackgroundStartColor;
  TabList.TabStartColor :=  SkinForeColor;
  TabList.TabStopColor := TabList.TabStartColor;
  TabList.Font.Color := SkinTextColor;
  TabList.Cursor := customDefault;

  WidgetList.Color := SkinBGColor;
  WidgetList.Font.Color := SkinTextColor;
  WidgetList.Ctl3D := False;
  WidgetList.Cursor := customDefault;

  PropertyList.Color := SkinForeColor;
  PropertyList.Font.Color := SkinTextColor;
  PropertyList.MarginColor := SkinBGColor;
  PropertyList.EditColor := SkinEditColor;
  PropertyList.Ctl3D := False;
  PropertyList.Cursor := customDefault;

  _Brush.Style := bsSolid;
  _Brush.Color := SkinForeColor;

  MenuInfo.cbSize := SizeOf(MenuInfo);
  MenuInfo.fMask := MIM_BACKGROUND Or MIM_APPLYTOSUBMENUS;
  MenuInfo.hbrBack := _Brush.Handle;
  SetMenuInfo(MainMenu1.Handle, MenuInfo);

  SetMenuSkin(MainMenu1.Items);
End;

Procedure TUIEditForm.SetMenuSkin(Menu: TMenuItem);
Var
  I:Integer;
Begin
  Menu.OnAdvancedDrawItem := Self.CustomDrawItem;;
  Menu.OnMeasureItem := Self.CustomMeasureItem;

  For I:=0 To Pred(Menu.Count) Do
  Begin
    SetMenuSkin(Menu.Items[I]);
  End;
End;

Procedure TUIEditForm.ChangeCursor(ID: Integer);
Begin
  RenderPanel.Cursor := ID;
  WidgetList.Cursor := ID;
  TabList.Cursor := ID;
  PropertyList.Cursor := ID;
End;

(*Var
  C: TCanvas;
  R: TRect;
Begin
//  inherited;
  GetWindowRect(Handle, R);
  Message.Result := 1;
  C := TCanvas.Create;
  C.Handle := GetWindowDC(Handle);
  C.Brush.Color := SkinBGcolor;
  C.Pen.Color := SkinForeColor;
  C.Pen.Width := 2;
  C.Brush.Style := bsSolid;
  C.Rectangle(1, 1, R.Right - R.Left, R.Bottom - R.Top);
  C.Free;

  MainMenu1.Items[0].On
End;
*)


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
