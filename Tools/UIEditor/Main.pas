unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, IceTabSet, Grids, ValEdit, ComCtrls,
  TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_VCLApplication, TERRA_OS,
  TERRA_String, TERRA_Scene, TERRA_Texture, TERRA_Font, TERRA_TTF, TERRA_DebugDraw,
  TERRA_Viewport, TERRA_FileManager, TERRA_FileUtils, TERRA_SpriteManager,
  TERRA_PNG, TERRA_JPG,
  TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color,
  TERRA_UI, TERRA_XML, TERRA_Collections, TERRA_CustomPropertyEditor;

Const
  SnapValue = 10;

  UIFileFilter = 'UI files (*.xml)|*.xml';

  customDefault = crArrow;
  customMove    = crSize;
  customHorizontal  = crSizeWE;
  customVertical  = crSizeNS;
  customText = crIBeam;
  customLink = crHandPoint;
  customDiagonal1 = crSizeNESW;
  customDiagonal2 = crSizeNWSE;

Type
  UIEditTool = (
    uitool_Empty,
    uitool_Window,
    uitool_Button,
    uitool_Label,
    uitool_Checkbox,
    uitool_Radiobutton,
    uitool_ProgressBar,
    uitool_Sprite
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

      Procedure Open(FileName:TERRAString);
      Procedure Save(FileName:TERRAString);

      Function PickWidgetAt(X, Y:Integer):Widget;
  End;

  UIEditScene = Class(TERRAScene)
    Protected
      _Font:TERRAFont;

      _ViewList:Array Of UIEditableView;
      _ViewCount:Integer;

      _SelectedView:UIEditableView;
      _SelectedWidget:Widget;
      _LastWidget:Widget;

      _CurrentTool:UIEditTool;
      _DragMode:UIDragMode;

      _GridSize:Single;

      Function GetNewTarget(X,Y:Integer):Widget;

    Public
      Constructor Create();

      Procedure Release(); Override;

      Procedure RenderSprites(V:TERRAViewport); Override;

      Procedure SetGridSize(Size:Single);

      Procedure AddView(Const Name:TERRAString);

      Procedure AddWidget(W:Widget; X,Y:Integer);
      Procedure AddWindow(X, Y:Integer);
      Procedure AddButton(X, Y:Integer);
      Procedure AddLabel(X, Y:Integer);
      Procedure AddCheckbox(X, Y:Integer);
      Procedure AddRadioButton(X, Y:Integer);
      Procedure AddProgressBar(X,Y:Integer);
      Procedure AddSprite(X,Y:Integer);

      Procedure SelectWidget(W:Widget);
  End;

  TUIEditForm = class(TForm)
    TabList: TIceTabSet;
    RenderPanel: TPanel;
    MainMenu: TMainMenu;
    ProjectMenu: TMenuItem;
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
    WidgetMenu: TMenuItem;
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
    PopupMenu: TPopupMenu;
    Copy1: TMenuItem;
    Delete1: TMenuItem;
    ViewMenu: TMenuItem;
    GridMenu: TMenuItem;
    GridOffMenu: TMenuItem;
    GridSmallMenu: TMenuItem;
    GridMediumMenu: TMenuItem;
    GridLargeMenu: TMenuItem;
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
    procedure Delete1Click(Sender: TObject);
    procedure WidgetListEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure Sprite1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure GridOffMenuClick(Sender: TObject);
    procedure GridSmallMenuClick(Sender: TObject);
    procedure GridMediumMenuClick(Sender: TObject);
    procedure GridLargeMenuClick(Sender: TObject);

  Protected
    Procedure CustomDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    Procedure CustomMeasureItem(Sender: TObject; ACanvas: TCanvas;  var Width, Height: Integer);


    Function AddWidgetNode(W:Widget):TTreeNode;
    Function FindWidgetNode(W:Widget):TTreeNode;
    Procedure UpdateWidgetTree();
    Procedure BuildWidgetTree();

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
Uses TERRA_UIDimension, TERRA_UIWindow, TERRA_UIButton, TERRA_UILabel, TERRA_UICheckbox, TERRA_UIRadioButton, TERRA_UIProgressBar,
  TERRA_UISprite;

{$R *.dfm}

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

  Self.SetGridSize(20.0);
End;

Procedure UIEditScene.AddWidget(W: Widget; X,Y:Integer);
Var
  Node:TTreeNode;
Begin
  Self._LastWidget := Self._SelectedWidget;
  _CurrentTool := uitool_Empty;

  If (W.Parent <> Self._SelectedView._Target) And (Assigned(W.Parent)) Then
  Begin
    X := Trunc(UISnap(X - W.Parent.AbsolutePosition.X));
    Y := Trunc(UISnap(Y - W.Parent.AbsolutePosition.Y));
    W.RelativePosition := VectorCreate2D(X, Y);
  End;
          
  Node := UIEditForm.AddWidgetNode(W);

  UIEditForm.FormResize(UIEditForm.WidgetList);

  UIEditForm.UpdateWidgetTree();

  Self.SelectWidget(W);
End;


Procedure UIEditScene.AddWindow(X, Y: Integer);
Begin
  Self.AddWidget(UIWindow.Create('window', Self.GetNewTarget(X, Y),
    X, Y, 0.1,
    UIPixels(300), UIPixels(200),  'window'), X, Y);
End;


Procedure UIEditScene.AddButton(X, Y: Integer);
Begin
  Self.AddWidget(UIButton.Create('button', Self.GetNewTarget(X, Y),
    X, Y, 0.1,
    UIPixels(150), UIPixels(50), 'Button', 'round_button'), X, Y);
End;

Procedure UIEditScene.AddLabel(X, Y: Integer);
Begin
  Self.AddWidget(UILabel.Create('label', Self.GetNewTarget(X, Y), X, Y, 0.1, 'text'), X, Y);
End;

procedure UIEditScene.AddCheckbox(X, Y: Integer);
Begin
  Self.AddWidget(UICheckbox.Create('check', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(25), True, 'text', 'checkbox'), X, Y);
End;

procedure UIEditScene.AddRadioButton(X, Y: Integer);
Begin
  Self.AddWidget(UIRadioButton.Create('radio', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(25), 'text', 'checkbox'), X, Y);
End;

procedure UIEditScene.AddProgressBar(X, Y: Integer);
Var
  P:UIProgressBar;
begin
  P := UIProgressBar.Create('bar', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(200), UIPixels(30), 'progressbar');
  P.Percent.Value := 50;
  Self.AddWidget(P, X, Y);
end;

procedure UIEditScene.AddSprite(X, Y: Integer);
Var
  P:UISprite;
begin
  P := UISprite.Create('sprite', Self.GetNewTarget(X, Y), X, Y, 0.1, 'sprite');
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

Procedure UIEditScene.SelectWidget(W: Widget);
Var
  Node:TTreeNode;
begin
  If (W = _SelectedWidget) Then
    Exit;

  _SelectedWidget := W;
  UIEditForm.PropertyList.Target := _SelectedWidget;

  Node := UIEditForm.FindWidgetNode(_SelectedWidget);

  If Assigned(Node) Then
    Node.Selected := True;
End;

Function UIEditScene.GetNewTarget(X,Y:Integer): Widget;
Begin
  Result := _SelectedView.PickWidgetAt(X, Y);
  If Result = Nil Then
    Result := _SelectedView._Target;
End;

Procedure UIEditScene.SetGridSize(Size: Single);
Begin
  UISnapSize := Size;
  Self._GridSize := Size;
End;

Procedure UIEditScene.RenderSprites(V: TERRAViewport);
Var
  X,Y, Width:Single;
  I:Integer;
  GridColor:Color;
Begin
  If _GridSize>1 Then
  Begin
    GridColor := ColorGrey(64, 200);

    X := 0;
    I := 0;
    While X<V.Width Do
    Begin
      Inc(I);
      Width := 1.0;
      If (I Mod 5 = 0) Then
        Width := Width * 2;

      DrawLine2D(V, VectorCreate2D(X, 0), VectorCreate2D(X, V.Height),  GridColor, Width);
      X := X + _GridSize;
    End;

    Y := 0;
    I := 0;
    While Y<V.Height Do
    Begin
      Inc(I);
      Width := 1.0;
      If (I Mod 5 = 0) Then
        Width := Width * 2;

      DrawLine2D(V, VectorCreate2D(0, Y), VectorCreate2D(V.Width, Y),  GridColor, Width);
      Y := Y + _GridSize;
    End;

  End;
End;

{ TUIEditForm }
Function TUIEditForm.AddNewTab(Const Name:TERRAString):TIceTab;
begin
  Result := TabList.AddTab(Name);
  Result.Selected := True;
end;

Procedure TUIEditForm.FormCreate(Sender: TObject);
Var
  S:TERRAString;
Begin
  VCLApplication.Create(Self.RenderPanel);

  // Added Asset folder to search path
  FileManager.Instance.AddPath('..\..\samples\binaries\assets');
  FileManager.Instance.AddPath('D:\Code\Minimon\Output');

  // Create a scene and set it as the current scene
  _Scene := UIEditScene.Create();
  GraphicsManager.Instance.SetScene(_Scene);

  _Brush := TBrush.Create();

  Self.LoadSkin();

  S := FileManager.Instance.SearchResourceFile('ui_menu0.xml');
//  Self._Scene._SelectedView.Open(S);
End;


procedure TUIEditForm.FormDestroy(Sender: TObject);
begin
  _Brush.Destroy();

  ReleaseObject(_Scene);
  Application.Instance.Terminate();
end;

procedure TUIEditForm.FormResize(Sender: TObject);
Var
  MenuHeight:Integer;
begin
//  MenuHeight := GetSystemMetrics(SM_CYMENU);

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
  RenderPanel.Width := Self.ClientWidth - RenderPanel.Left;
  RenderPanel.Height := Self.ClientHeight - RenderPanel.Top;

  IntToString(RenderPanel.Height);
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

procedure TUIEditForm.Sprite1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Sprite;
end;

procedure TUIEditForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  W:Widget;
  pnt: TPoint;
begin
  If (Button = mbRight) Then
  Begin
    If GetCursorPos(pnt) then
      Self.PopupMenu.Popup(pnt.X, pnt.Y);
    Exit;
  End;

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

  uitool_Sprite:
    Begin
      Self.Scene.AddSprite(X, Y);
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
  P:Vector2D;
  PX, PY:Integer;
  TargetCursor:Integer;
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
    P := VectorCreate2D(X, Y);
    W.ConvertGlobalToLocal(P);
    PX := Trunc(P.X);
    PY := Trunc(P.Y);

    If (PX<=WidgetBorder) And (PY<=WidgetBorder) Then
    Begin
      _Scene._DragMode := UIDrag_TopLeft;
      TargetCursor := customDiagonal1;
    End Else
    If (PX>=Trunc(W.Size.X - WidgetBorder)) And (PY<=WidgetBorder) Then
    Begin
      _Scene._DragMode := UIDrag_TopRight;
      TargetCursor := customDiagonal2;
    End Else
    If (PX<=WidgetBorder) And (PY>=Trunc(W.Size.Y - WidgetBorder)) Then
    Begin
      _Scene._DragMode := UIDrag_BottomLeft;
      TargetCursor := customDiagonal2;
    End Else
    If (PX>=Trunc(W.Size.X - WidgetBorder)) And (PY>=Trunc(W.Size.Y - WidgetBorder)) Then
    Begin
      _Scene._DragMode := UIDrag_BottomRight;
      TargetCursor := customDiagonal1;
    End Else

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

Function TUIEditForm.AddWidgetNode(W:Widget):TTreeNode;
Begin
  Result := Nil;
  If (W = Nil) Then
    Exit;

  Result := FindWidgetNode(W);
  If Assigned(Result) Then
    Exit;

  If Assigned(W.Parent) Then
    Self.AddWidgetNode(W.Parent);

  Result := WidgetList.Items.AddChildObject(FindWidgetNode(W.Parent), W.Name, W);
End;

Procedure TUIEditForm.BuildWidgetTree();
Var
  It:Iterator;
  W:Widget;
Begin
  WidgetList.Items.Clear();

  It := Self._Scene._SelectedView._Target.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);

    AddWidgetNode(W);
  End;
  ReleaseObject(It);
End;

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
  Flags := DT_VCENTER Or DT_CENTER;

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

    If (odDisabled In State) then
      ACanvas.Font.Color := clBtnShadow
    Else
    If (odSelected In State) then
      ACanvas.Font.Color := clWhite
    Else
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
  LoadCursor(customDiagonal1, 'diagonal1');
  LoadCursor(customDiagonal2, 'diagonal2');

  ChangeCursor(customDefault);

  SkinBGColor := RGB(38, 38, 38);
  SkinForeColor := RGB(70, 70, 70);
  SkinTextColor := RGB(254, 163, 0);
  SkinEditColor := RGB(128, 128, 128);


  TabList.BackgroundStartColor := SkinBGColor;
  TabList.BackgroundStopColor := TabList.BackgroundStartColor;

  TabList.SelectedTabStartColor :=  SkinTextColor;
  TabList.SelectedTabStopColor := TabList.SelectedTabStartColor;

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
  SetMenuInfo(MainMenu.Handle, MenuInfo);
  SetMenuInfo(PopupMenu.Handle, MenuInfo);

  SetMenuSkin(MainMenu.Items);
  SetMenuSkin(PopupMenu.Items);
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

Procedure UIEditableView.Open(FileName: TERRAString);
Var
  Doc:XMLDocument;
Begin
  FileManager.Instance.AddPath(GetFilePath(FileName));

  Doc := XMLDocument.Create();
  Doc.LoadFromFile(FileName);
  Doc.SaveToObject(_Target);
  ReleaseObject(Doc);

  UIEditForm.BuildWidgetTree();

  UIEditForm._Scene._SelectedWidget := Nil;
End;

procedure UIEditableView.Save(FileName: TERRAString);
Var
  Doc:XMLDocument;
Begin
  Doc := XMLDocument.Create();

  Doc.LoadFromObject(_Target);

  Doc.SaveToFile(FileName, xmlSaveCompact);
  ReleaseObject(Doc);
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



procedure TUIEditForm.Delete1Click(Sender: TObject);
Var
  Node:TTreeNode;
begin
  If _Scene._SelectedWidget = Nil Then
    Exit;

  Node := Self.FindWidgetNode(_Scene._SelectedWidget);

  If Assigned(Node) Then
  Begin
    Node.DeleteChildren();
    Node.Delete();
  End;

  _Scene._SelectedView._Target.DeleteWidget(_Scene._SelectedWidget);
  PropertyList.Target := Nil;
end;

procedure TUIEditForm.WidgetListEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
  If Node.Data = Nil Then
    Exit;

  Widget(Node.Data).ObjectName := S;
end;



procedure TUIEditForm.Save1Click(Sender: TObject);
Var
  Dialog:TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  Dialog.Filter := UIFileFilter;
  Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofPathMustExist];

  If Dialog.Execute Then
    Self._Scene._SelectedView.Save(Dialog.FileName);

  Dialog.Destroy();
end;

procedure TUIEditForm.Open1Click(Sender: TObject);
Var
  Dialog:TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Self);
  Dialog.Filter := UIFileFilter;
  Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofPathMustExist];

  If Dialog.Execute Then
  Begin
    Self._Scene._SelectedView.Open(Dialog.FileName);
    Self.FormResize(Sender);
  End;

  Dialog.Destroy();
end;

procedure TUIEditForm.GridOffMenuClick(Sender: TObject);
begin
  Self._Scene.SetGridSize(0);
end;

procedure TUIEditForm.GridSmallMenuClick(Sender: TObject);
begin
  Self._Scene.SetGridSize(10);
end;

procedure TUIEditForm.GridMediumMenuClick(Sender: TObject);
begin
  Self._Scene.SetGridSize(20);
end;

procedure TUIEditForm.GridLargeMenuClick(Sender: TObject);
begin
  Self._Scene.SetGridSize(50);
end;

end.
