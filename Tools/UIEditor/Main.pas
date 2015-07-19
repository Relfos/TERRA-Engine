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
  TERRA_UI, TERRA_XML, TERRA_Collections, TERRA_CollectionObjects, TERRA_CustomPropertyEditor;

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
  FontMode = (
    font_Normal,
    font_Selected,
    font_Disabled
  );

  UIEditTool = (
    uitool_Empty,
    uitool_Window,
    uitool_Button,
    uitool_Label,
    uitool_Checkbox,
    uitool_Radiobutton,
    uitool_ProgressBar,
    uitool_Sprite,
    uitool_Combobox
  );

  UIEditScene = Class;

  UIEditableView = Class(CollectionObject)
    Protected
      _Owner:UIEditScene;
      _Tab:TIceTab;
      _Target:TERRAUI;

    Public
      Constructor Create(Const Name:TERRAString; Owner:UIEditScene);
      Procedure Release(); Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Function PickWidgetAt(X, Y:Integer; Ignore:Widget = Nil):Widget;
  End;

  UIEditScene = Class(TERRAScene)
    Protected
      _Font:TERRAFont;

      _Views:List;
      _Paths:List;

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
      
      Procedure Clear();

      Procedure RenderSprites(V:TERRAViewport); Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Procedure SetGridSize(Size:Single);

      Procedure Open(FileName:TERRAString);
      Procedure Save(FileName:TERRAString);

      Function AddView(Const Name:TERRAString):UIEditableView;

      Procedure AddWidget(W:Widget; X,Y:Integer);
      Procedure AddWindow(X, Y:Integer);
      Procedure AddButton(X, Y:Integer);
      Procedure AddLabel(X, Y:Integer);
      Procedure AddCheckbox(X, Y:Integer);
      Procedure AddRadioButton(X, Y:Integer);
      Procedure AddProgressBar(X,Y:Integer);
      Procedure AddSprite(X,Y:Integer);
      Procedure AddComboBox(X,Y:Integer);

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
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Combobox1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure WidgetListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  Protected
    _CurrentCursor:Integer;

    Procedure CustomDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    Procedure CustomMeasureItem(Sender: TObject; ACanvas: TCanvas;  var Width, Height: Integer);


    Function AddWidgetNode(W:Widget):TTreeNode;
    Function FindWidgetNode(W:Widget):TTreeNode;
    Procedure UpdateWidgetTree();
    Procedure BuildWidgetTree();

    Procedure LoadCursor(ID:Integer; Name:TERRAString);
    Procedure LoadSkin();
    Procedure SetMenuSkin(Menu:TMenuItem);
    Procedure ApplyFontStyle(Target:TFont; Mode:FontMode);

    Procedure ChangeCursor(ID:Integer);

  Private
    _Scene:UIEditScene;
    _DragTarget:Widget;
    _DropTarget:Widget;
    _Brush:TBrush;

  Public

    Property Scene:UIEditScene Read _Scene;
  End;

  UIEditorApplication = Class(VCLApplication)
    Public
      Function CreateProperty(Owner:TERRAObject; Const KeyName, ObjectType:TERRAString):TERRAObject; Override;
  End;


Var
  UIEditForm: TUIEditForm;
  SkinBGColor, SkinForeColor, SkinTextColor, SkinEditColor:TColor;


implementation
Uses TERRA_UIDimension, TERRA_UIWindow, TERRA_UIButton, TERRA_UILabel, TERRA_UICheckbox, TERRA_UIRadioButton, TERRA_UIProgressBar,
  TERRA_UISprite, TERRA_UIComboBox;

{$R *.dfm}

{ UIEditScene }
Constructor UIEditScene.Create;
Begin
  _ObjectName := 'project';
  _CurrentTool := uitool_Empty;

  // Load a font
  Self._Font := FontManager.Instance.GetFont('droid');

  // set background color
  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorGrey(128);

  Self.Clear();

  Self.AddView('Untitled');
  Self.SetGridSize(20.0);
End;

Procedure UIEditScene.Clear();
Begin
  UIEditForm.TabList.Tabs.Clear();
  UIEditForm.PropertyList.Target := Nil;

  ReleaseObject(_Views);
  ReleaseObject(_Paths);

  Self._Views := List.Create();
  Self._Views.Name := 'views';

  Self._Paths := List.Create();
  Self._Paths.Name := 'paths';

  Self._SelectedView := Nil;
End;

Procedure UIEditScene.Release;
Begin
  ReleaseObject(_Views);
  ReleaseObject(_Paths);
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

procedure UIEditScene.AddComboBox(X,Y:Integer);
Var
  P:UIComboBox;
  Content:List;
begin
  Content := List.Create();
  Content.Add(StringObject.Create('one'));
  Content.Add(StringObject.Create('two'));
  Content.Add(StringObject.Create('three'));
  P := UIComboBox.Create('combo', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(300), UIPixels(50), 'combobox');
  P.SetContent(Content);
  P.ItemIndex := 0;
  
  Self.AddWidget(P, X, Y);
end;

Function UIEditScene.AddView(Const Name:TERRAString):UIEditableView;
Begin
  Result := UIEditableView.Create(Name, Self);
  _Views.Add(Result);

  _SelectedView := Result;
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

Function UIEditScene.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Views;
  1:  Result := _Paths;
  Else
    Result := Nil;
  End;
End;

Procedure UIEditScene.Open(FileName: TERRAString);
Var
  Root:XMLNode;
Begin
  Self.Clear();

  FileManager.Instance.AddPath(GetFilePath(FileName));

  Root := XMLNode.Create();
  Root.LoadFromFile(FileName);
  Root.SaveToObject(Self);
  ReleaseObject(Root);

  If _Views.Count > 0 Then
  Begin
    _SelectedView := UIEditableView(_Views.First);
    UIEditForm.BuildWidgetTree();
    UIEditForm._Scene._SelectedWidget := Nil;
  End;
End;

procedure UIEditScene.Save(FileName: TERRAString);
Var
  Root:XMLNode;
Begin
  Root := XMLNode.Create();
  Root.LoadFromObject(Self);
  Root.SaveToFile(FileName, xmlSaveCompact);
  ReleaseObject(Root);
End;

{ UIEditableView }
Constructor UIEditableView.Create(const Name: TERRAString; Owner:UIEditScene);
Begin
  Self._ObjectName := Name;
  Self._Owner := Owner;
  Self._Tab := UIEditForm.AddNewTab(Name);

  // Create a new UI
  Self._Target := TERRAUI.Create();

  // Register the font with the UI
  _Target.DefaultFont := Self._Owner._Font;

  // Load a GUI skin
  _Target.LoadSkin('ui_sample_skin');
End;


Function UIEditableView.PickWidgetAt(X, Y: Integer; Ignore:Widget): Widget;
Begin
  Result := _Target.PickWidget(X, Y, Ignore);
End;

Procedure UIEditableView.Release;
Begin
  UIManager.Instance.RemoveUI(_Target);
  ReleaseObject(_Target);
End;


Function UIEditableView.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Target;
  Else
    Result := Inherited GetPropertyByIndex(Index - 1);
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
  _CurrentCursor := 9999;
  
  UIEditorApplication.Create(Self.RenderPanel);

  // Added Asset folder to search path
  FileManager.Instance.AddPath('..\..\samples\binaries\assets');
  FileManager.Instance.AddPath('D:\Code\Minimon\Output\Textures');

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

procedure TUIEditForm.Combobox1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Combobox;
end;

procedure TUIEditForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  W:Widget;
  pnt: TPoint;
  Node:TTreeNode;
begin
  If (Button = mbRight) Then
  Begin
    If GetCursorPos(pnt) then
    Begin
      W := Scene._SelectedView.PickWidgetAt(X, Y);
      If Assigned(W) Then
      Begin
        Scene.SelectWidget(W);
        Node := FindWidgetNode(W);
        If Assigned(Node) Then
          Node.Selected := True;

        Self.PopupMenu.Popup(pnt.X, pnt.Y);
      End;
    End;

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

  uitool_Combobox:
    Begin
      Self.Scene.AddComboBox(X, Y);
    End;

  End;

end;

procedure TUIEditForm.RenderPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Assigned(_DragTarget) Then
  Begin
    _DragTarget.FinishDrag();

    If (_DragTarget.Parent <> _DropTarget) Then
    Begin
      _DragTarget.Parent := _DropTarget;

      Self.BuildWidgetTree();
    End;

    PropertyList.RequestUpdate();

    _DragTarget := Nil;
    _DropTarget := Nil;

    ChangeCursor(customDefault);
  End;

End;

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

    _DropTarget := Scene._SelectedView.PickWidgetAt(X, Y, _DragTarget);
    If (_DropTarget <> _DragTarget.Parent) Then
    Begin
      TargetCursor := crHandPoint
    End Else
      TargetCursor := customDefault;

    ChangeCursor(TargetCursor);

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
Var
  I:Integer;
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

  For I:=0 To Pred(W.ChildrenCount) Do
    Self.AddWidgetNode(W.GetChildByIndex(I));
End;

Procedure TUIEditForm.BuildWidgetTree();
Var
  W:Widget;
Begin
  WidgetList.Items.Clear();

  W := Self._Scene._SelectedView._Target.First;
  While Assigned(W) Do
  Begin
    AddWidgetNode(W);
    W := W.Next;
  End;
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
      ApplyFontStyle(ACanvas.Font, font_Disabled)
    Else
    If (odSelected In State) then
      ApplyFontStyle(ACanvas.Font, font_Selected)
    Else
      ApplyFontStyle(ACanvas.Font, font_Normal);

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

Procedure TUIEditForm.ApplyFontStyle(Target:TFont; Mode:FontMode);
Begin
  If Mode = font_Disabled Then
    Target.Color := clBtnShadow
  Else
  If Mode = font_Selected Then
    Target.Color := clWhite
  Else
    Target.Color := SkinTextColor;

//  Target.Size := 10;
  Target.Name := 'Verdana';
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


  TabList.BackgroundStartColor := SkinForeColor;
  TabList.BackgroundStopColor := TabList.BackgroundStartColor;
  ApplyFontStyle(TabList.Font, font_Normal);
  ApplyFontStyle(TabList.SelectedFont, font_Selected);
  Self.Color := TabList.BackgroundStopColor;

  TabList.SelectedTabStartColor :=  SkinTextColor;
  TabList.SelectedTabStopColor := TabList.SelectedTabStartColor;

  TabList.TabStartColor :=  SkinForeColor;
  TabList.TabStopColor := TabList.TabStartColor;

  TabList.Cursor := customDefault;

  WidgetList.Color := SkinBGColor;
  ApplyFontStyle(WidgetList.Font, font_Normal);
  WidgetList.Ctl3D := False;
  WidgetList.Cursor := customDefault;

  PropertyList.Color := SkinForeColor;
  ApplyFontStyle(PropertyList.Font, font_Normal);
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
  If _CurrentCursor = ID Then
    Exit;

  _CurrentCursor := ID;
  RenderPanel.Cursor := ID;
  WidgetList.Cursor := ID;
  TabList.Cursor := ID;
  PropertyList.Cursor := ID;
  Screen.Cursor := ID;
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

  Widget(Node.Data).Name := S;
end;

procedure TUIEditForm.Save1Click(Sender: TObject);
Var
  Dialog:TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  Dialog.Filter := UIFileFilter;
  Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofPathMustExist];

  If Dialog.Execute Then
    Self._Scene.Save(Dialog.FileName);

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
    Self._Scene.Open(Dialog.FileName);
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

procedure TUIEditForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If ssCtrl In Shift Then
  Case Key Of
    Ord('1'): Self.Button1Click(Sender);
    Ord('2'): Self.Label1Click(Sender);
    Ord('3'): Self.Window1Click(Sender);
    Ord('4'): Self.Checkbox1Click(Sender);
    Ord('5'): Self.Radiobox1Click(Sender);
    Ord('6'): Self.Combobox1Click(Sender);
//    Ord('7'): Self.Icon1Click(Sender);
    Ord('8'): Self.Sprite1Click(Sender);
    Ord('9'): Self.ProgressBar1Click(Sender);
  End;
end;


{ UIEditorApplication }
Function UIEditorApplication.CreateProperty(Owner:TERRAObject; const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  If StringEquals('UIEditableView', ObjectType) Then
    Result := UIEditableView.Create(KeyName, UIEditForm._Scene)
  Else
    Result := Inherited CreateProperty(Owner, KeyName, ObjectType);
End;

procedure TUIEditForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  If WheelDelta<0 Then
    PropertyList.DoMouseWheelUp(Shift, MousePos)
  Else
    PropertyList.DoMouseWheelDown(Shift, MousePos);
end;

procedure TUIEditForm.WidgetListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  pnt: TPoint;
  Node:TTreeNode;
begin
  If (Button = mbRight) Then
  Begin
    Node := Self.WidgetList.GetNodeAt(X, Y);

    If Assigned(Node) Then
    Begin
      Node.Selected := True;
      If GetCursorPos(pnt) then
        Self.PopupMenu.Popup(pnt.X, pnt.Y);
    End;
    
    Exit;
  End;
end;

end.
