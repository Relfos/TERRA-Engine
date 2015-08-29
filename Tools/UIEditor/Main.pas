
unit Main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, Grids, ValEdit, ComCtrls,
  TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_VCLApplication, TERRA_OS, TERRA_Renderer,
  TERRA_String, TERRA_Texture, TERRA_Font, TERRA_TTF, TERRA_DebugDraw, TERRA_Renderable,
  TERRA_Viewport, TERRA_FileManager, TERRA_FileUtils, TERRA_Sprite, TERRA_EngineManager,
  TERRA_PNG, TERRA_JPG,
  TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color,
  TERRA_XML, TERRA_Collections, TERRA_List, TERRA_UIView, TERRA_UIWidget, TERRA_CustomPropertyEditor;

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
  //TargetTab = TIceTab;
  TargetTab = Integer;

  FontMode = (
    font_Normal,
    font_Selected,
    font_Disabled
  );

  UIEditTool = (
    uitool_Empty,
    uitool_Image,
    uitool_TiledRect,
    uitool_Label,
    uitool_InputText
  );

  UIEditScene = Class;

  UIGrid = Class(TERRARenderable)
    Protected
      _GridSize:Single;

    Public
      Function GetRenderBucket:Cardinal; Override;
      Procedure Render(V:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;
  End;

  UIEditableView = Class(TERRAObject)
    Protected
      _Owner:UIEditScene;
      _Tab:TargetTab;
      _Target:UIView;

    Public
      Constructor Create(Const Name:TERRAString; Owner:UIEditScene);
      Procedure Release(); Override;

      Procedure Select();

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Function PickWidgetAt(X, Y:Integer; Ignore:UIWidget = Nil):UIWidget;
  End;

  UIEditScene = Class(TERRAObject)
    Protected
      _Font:TERRAFont;

      _Views:TERRAList;
      _Paths:TERRAList;
      _Datasources:TERRAList;

      _SelectedView:UIEditableView;
      _SelectedWidget:UIWidget;
      _LastWidget:UIWidget;

      _CurrentTool:UIEditTool;
      _DragMode:UIDragMode;

      _Grid:UIGrid;

      Function GetNewTarget(X,Y:Integer):UIWidget;


    Public
      Constructor Create();

      Procedure Release(); Override;

      Procedure Clear();

      Procedure RenderViewport2D(V:TERRAViewport);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Procedure SetGridSize(Size:Single);

      Procedure Open(FileName:TERRAString);
      Procedure Save(FileName:TERRAString);

      Procedure Select(Index:Integer);

      Function AddView(Const Name:TERRAString):UIEditableView;

      Procedure AddWidget(W:UIWidget; X,Y:Integer);
      Procedure AddImage(X, Y:Integer);
      Procedure AddTiledRect(X, Y:Integer);
      Procedure AddLabel(X, Y:Integer);
      Procedure AddInputText(X, Y:Integer);

      Procedure SelectWidget(W:UIWidget);
  End;

  TUIEditForm = class(TForm)
    MainMenu: TMainMenu;
    ProjectMenu: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    NewProject: TMenuItem;
    N3: TMenuItem;
    View1: TMenuItem;
    Component1: TMenuItem;
    WidgetMenu: TMenuItem;
    Image1: TMenuItem;
    Label1: TMenuItem;
    TiledRect1: TMenuItem;
    InputText1: TMenuItem;
    PopupMenu: TPopupMenu;
    Copy1: TMenuItem;
    Delete1: TMenuItem;
    ViewMenu: TMenuItem;
    GridMenu: TMenuItem;
    GridOffMenu: TMenuItem;
    GridSmallMenu: TMenuItem;
    GridMediumMenu: TMenuItem;
    GridLargeMenu: TMenuItem;
    N4: TMenuItem;
    Settings1: TMenuItem;
    DataSources1: TMenuItem;
    Paths1: TMenuItem;
    TabList: TTabControl;
    WidgetList: TTreeView;
    PropertyList: TCustomPropertyEditor;
    RenderPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

    Function AddNewTab(Const Name:TERRAString):TargetTab;
    procedure Image1Click(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WidgetListClick(Sender: TObject);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1Click(Sender: TObject);
    procedure InputText1Click(Sender: TObject);
    procedure TiledRect1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure WidgetListEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure GridOffMenuClick(Sender: TObject);
    procedure GridSmallMenuClick(Sender: TObject);
    procedure GridMediumMenuClick(Sender: TObject);
    procedure GridLargeMenuClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure WidgetListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure View1Click(Sender: TObject);

(*    procedure TabListTabSelected(Sender: TObject; ATab: TargetTab;
      ASelected: Boolean);
    procedure TabListTabClose(Sender: TObject; ATab: TargetTab);*)
    
    procedure Settings1Click(Sender: TObject);
    procedure DataSources1Click(Sender: TObject);
    procedure Paths1Click(Sender: TObject);
    procedure NewProjectClick(Sender: TObject);
    procedure TabListChange(Sender: TObject);

  Protected
    _CurrentCursor:Integer;

    Procedure CustomDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    Procedure CustomMeasureItem(Sender: TObject; ACanvas: TCanvas;  var Width, Height: Integer);


    Function AddWidgetNode(W:UIWidget):TTreeNode;
    Function FindWidgetNode(W:UIWidget):TTreeNode;
    Procedure UpdateWidgetTree();
    Procedure BuildWidgetTree();

    Procedure LoadCursor(ID:Integer; Name:TERRAString);
    Procedure LoadSkin();
    Procedure SetMenuSkin(Menu:TMenuItem);
    Procedure ApplyFontStyle(Target:TFont; Mode:FontMode);

    Procedure ChangeCursor(ID:Integer);

  Private
    _Scene:UIEditScene;
    _DragTarget:UIWidget;
    _DropTarget:UIWidget;
    _Brush:TBrush;

  Public

    Property Scene:UIEditScene Read _Scene;
  End;

  UIEditorApplication = Class(VCLApplication)
    Public
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;
  End;


Var
  UIEditForm: TUIEditForm;
  SkinBGColor, SkinForeColor, SkinTextColor, SkinEditColor:TColor;


implementation
Uses TERRA_UIDimension, TERRA_UITemplates, TERRA_UIImage, TERRA_UITiledRect, TERRA_UILabel, TERRA_UIEditText, FormProjectSettings, FormListEdit;

{$R *.dfm}

Function UIGrid.GetRenderBucket:Cardinal;
Begin
  Result := renderBucket_Overlay;
End;

Procedure UIGrid.Render(V:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal);
Var
  X,Y, Width:Single;
  I:Integer;
  GridColor:ColorRGBA;
Begin
  If _GridSize<=1 Then
    Exit;

  GridColor := ColorGrey(64, 200);

  X := 0;
  I := 0;
  While X<V.Width Do
  Begin
    Inc(I);
    If (I Mod 5 = 0) Then           
      Width := 2.0
    Else
      Width := 1.0;

    DrawLine2D(V, Vector2D_Create(X, 0), Vector2D_Create(X, V.Height),  GridColor, Width);
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

    DrawLine2D(V, Vector2D_Create(0, Y), Vector2D_Create(V.Width, Y),  GridColor, Width);
    Y := Y + _GridSize;
  End;
End;

{ UIEditScene }
Constructor UIEditScene.Create;
Begin
  _ObjectName := 'project';
  _CurrentTool := uitool_Empty;

  // Load a font
  Self._Font := Engine.Fonts['droid'];

  // set background color
  Engine.Graphics.DeviceViewport.BackgroundColor := ColorGrey(128);

  Self.Clear();

//  Self.AddView('Untitled');

  _Grid := UIGrid.Create();
  Self.SetGridSize(20.0);
End;

Procedure UIEditScene.Clear();
Begin
  UIEditForm.TabList.Tabs.Clear();
  UIEditForm.PropertyList.Target := Nil;

  ReleaseObject(_Grid);
  ReleaseObject(_Views);
  ReleaseObject(_Paths);
  ReleaseObject(_Datasources);

  Self._Views := TERRAList.Create();
  Self._Views.Name := 'views';

  Self._Paths := TERRAList.Create();
  Self._Paths.Name := 'paths';

  Self._Datasources := TERRAList.Create();
  Self._Datasources.Name := 'datasources';

  Self._SelectedView := Nil;
End;

Procedure UIEditScene.Release;
Begin
  ReleaseObject(_Views);
  ReleaseObject(_Paths);
  ReleaseObject(_Datasources);
End;


Procedure UIEditScene.AddWidget(W:UIWidget; X,Y:Integer);
Var
  Node:TTreeNode;
Begin
  Self._LastWidget := Self._SelectedWidget;
  _CurrentTool := uitool_Empty;

  If (W.Parent <> Self._SelectedView._Target) And (Assigned(W.Parent)) Then
  Begin
    X := Trunc(UISnap(X - W.Parent.AbsolutePosition.X));
    Y := Trunc(UISnap(Y - W.Parent.AbsolutePosition.Y));
    W.RelativePosition := Vector2D_Create(X, Y);
  End;

  UIEditForm.FormResize(UIEditForm.WidgetList);
  UIEditForm.BuildWidgetTree();

  Self.SelectWidget(W);

  UIEditForm.FormResize(Nil);
End;


Procedure UIEditScene.AddImage(X, Y: Integer);
Var
  Img:UIImage;
  Tex:TERRATexture;
Begin
  Tex := Engine.Textures['default_image'];
  Img := UIImage.Create('image', Self.GetNewTarget(X, Y), UIPixels(X), UIPixels(Y), 0.1, UIPixels(0), UIPixels(0));
  Img.SetTexture(Tex);
  Self.AddWidget(Img, X, Y);
End;


Procedure UIEditScene.AddTiledRect(X, Y: Integer);
Begin
  Self.AddWidget(UITiledRect.Create('rect', Self.GetNewTarget(X, Y), UIPixels(X), UIPixels(Y), 0.1, UIPixels(300), UIPixels(200)), X, Y);
End;

Procedure UIEditScene.AddLabel(X, Y: Integer);
Begin
  Self.AddWidget(UILabel.Create('label', Self.GetNewTarget(X, Y), UIPixels(X), UIPixels(Y), 0.1, UIPixels(100), UIPixels(50), 'text'), X, Y);
End;

procedure UIEditScene.AddInputText(X, Y: Integer);
Begin
  Self.AddWidget(UIEditText.Create('label', Self.GetNewTarget(X, Y), UIPixels(X), UIPixels(Y), 0.1, UIPixels(100), UIPixels(50), 'text'), X, Y);
End;

{procedure UIEditScene.AddRadioButton(X, Y: Integer);
Begin
(*  Self.AddWidget(UIRadioButton.Create('radio', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(25), 'text', 'checkbox'), X, Y);*)
End;

procedure UIEditScene.AddProgressBar(X, Y: Integer);
(*Var
  P:UIProgressBar;*)
begin
(*  P := UIProgressBar.Create('bar', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(200), UIPixels(30), 'progressbar');
  P.Percent.Value := 50;
  Self.AddWidget(P, X, Y);*)
end;

procedure UIEditScene.AddSprite(X, Y: Integer);
begin
//  Self.AddWidget(UISprite.Create('sprite', Self.GetNewTarget(X, Y), X, Y, 0.1, 'sprite'), X, Y);
end;

procedure UIEditScene.AddComboBox(X,Y:Integer);
(*Var
  P:UIComboBox;
  Content:List;*)
begin
(*  Content := List.Create();
  Content.Add(StringObject.Create('one'));
  Content.Add(StringObject.Create('two'));
  Content.Add(StringObject.Create('three'));
  P := UIComboBox.Create('combo', Self.GetNewTarget(X, Y), X, Y, 0.1, UIPixels(300), UIPixels(50), 'combobox');
  P.SetContent(Content);
  P.ItemIndex := 0;

  Self.AddWidget(P, X, Y);*)
end;}

Function UIEditScene.AddView(Const Name:TERRAString):UIEditableView;
Begin
  Result := UIEditableView.Create(Name, Self);
  _Views.Add(Result);

  Result.Select();
  _SelectedView := Result;
End;

Procedure UIEditScene.SelectWidget(W:UIWidget);
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

Function UIEditScene.GetNewTarget(X,Y:Integer):UIWidget;
Begin
  Result := _SelectedView.PickWidgetAt(X, Y);
  If Result = Nil Then
    Result := _SelectedView._Target;
End;

Procedure UIEditScene.SetGridSize(Size: Single);
Begin
  UISnapSize := Size;
  _Grid._GridSize := Size;
End;

Procedure UIEditScene.RenderViewport2D(V: TERRAViewport);
Begin
  If (Self._SelectedView = Nil) Or (Self._SelectedView._Target = Nil)  Then
    Exit;

  If (V<>Self._SelectedView._Target.Viewport) Then
    Exit;

  Engine.Graphics.AddRenderable(V, Self._Grid);

  Self._SelectedView._Target.AutoResize();
  Engine.Graphics.AddRenderable(V, Self._SelectedView._Target);
End;

Function UIEditScene.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Views;
  1:  Result := _Paths;
  2:  Result := _Datasources;
  Else
    Result := Nil;
  End;
End;

Procedure UIEditScene.Open(FileName: TERRAString);
Var
  Root:XMLNode;
Begin
  Self.Clear();

  Engine.Files.AddFolder(GetFilePath(FileName));

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

procedure UIEditScene.Select(Index: Integer);
Var
  Item:TERRAObject;
begin
  If Assigned(_SelectedView) Then
    _SelectedView._Target.Visible := False;

  Item := _Views.GetItemByIndex(Index);

  UIEditForm.PropertyList.Target := Nil;

  If Assigned(Item) Then
  Begin
    _SelectedView := UIEditableView(Item);
    _SelectedView.Select();
  End;

  UIEditForm.BuildWidgetTree();
end;

{ UIEditableView }
Constructor UIEditableView.Create(const Name: TERRAString; Owner:UIEditScene);
Begin
  Self._ObjectName := Name;
  Self._Owner := Owner;

  // Create a new UI
  _Target := UIView.Create('ui', UIPercent(100), UIPercent(100));
  _Target.Viewport.OnRender := _Owner.RenderViewport2D;

  // Register the font with the UI
  _Target.DefaultFont := Self._Owner._Font;

  _Target.Visible := True;

  _Tab := UIEditForm.AddNewTab(Name);
End;


Procedure UIEditableView.Select();
begin
  _Target.Visible := True;
end;

Function UIEditableView.PickWidgetAt(X, Y: Integer; Ignore:UIWidget): UIWidget;
Begin
  Result := _Target.PickWidget(X, Y, widgetEventClass_Any, Ignore);
End;

Procedure UIEditableView.Release;
Begin
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
Function TUIEditForm.AddNewTab(Const Name:TERRAString):TargetTab;
begin
  //Result := TabList.AddTab(Name);
  Result := TabList.Tabs.Add(Name);
  TabList.TabIndex := Result;
  UIEditForm.FormResize(Nil);
  //Result.Selected := True;
end;

Procedure TUIEditForm.FormCreate(Sender: TObject);
Var
  S:TERRAString;
Begin
  _CurrentCursor := 9999;

  UIEditorApplication.Create(Self.RenderPanel);

  // Added Asset folder to search path
  Engine.Files.AddFolder('..\..\samples\binaries\assets');
//  FileManager.Instance.AddPath('D:\Code\Minimon\Output\Textures');

  // Create a scene and set it as the current scene
  _Scene := UIEditScene.Create();
  Self.NewProjectClick(Sender);

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
Var
  MenuHeight:Integer;
begin
//  MenuHeight := GetSystemMetrics(SM_CYMENU);

  PropertyList.Visible := WidgetList.Items.Count>0;

  TabList.Width := Self.Width;
  TabList.Height := Self.Height - TabList.Top;

  WidgetList.Top := TabList.TabHeight;

  If PropertyList.Visible Then
  Begin
    WidgetList.Height := Self.Height Div 3;
    PropertyList.Top := WidgetList.Height + WidgetList.Top;
    PropertyList.Height := Self.Height - PropertyList.Top;
  End Else
  Begin
    WidgetList.Height := Self.Height -WidgetList.Top;
  End;

  RenderPanel.Top := TabList.TabHeight;
  RenderPanel.Left := PropertyList.Width + PropertyList.Left;
  RenderPanel.Width := Self.ClientWidth - RenderPanel.Left;
  RenderPanel.Height := Self.ClientHeight - RenderPanel.Top;
end;

procedure TUIEditForm.Image1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Image;
end;


procedure TUIEditForm.TiledRect1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_TiledRect;
end;

procedure TUIEditForm.Label1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_Label;
end;

procedure TUIEditForm.InputText1Click(Sender: TObject);
begin
  Scene._CurrentTool := uitool_InputText;
end;

procedure TUIEditForm.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  W:UIWidget;
  pnt: TPoint;
  Node:TTreeNode;
begin
  If Scene._SelectedView = Nil Then
    Exit;

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

  uitool_Image:
    Begin
      Self.Scene.AddImage(X, Y);
    End;

  uitool_TiledRect:
    Begin
      Self.Scene.AddTiledRect(X, Y);
    End;

  uitool_Label:
    Begin
      Self.Scene.AddLabel(X, Y);
    End;

  uitool_InputText:
    Begin
      Self.Scene.AddInputText(X, Y);
    End;

  End;

end;

procedure TUIEditForm.RenderPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Scene._SelectedView = Nil Then
    Exit;

  If Assigned(_DragTarget) Then
  Begin
    _DragTarget.FinishDrag();

    If (_DropTarget = Nil) Then
      _DropTarget := _DragTarget.View;

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
  W:UIWidget;
  P:Vector2D;
  PX, PY:Integer;
  TargetCursor:Integer;
begin
  If Scene._SelectedView = Nil Then
    Exit;

  If Assigned(_DragTarget) Then
  Begin
    If ssCtrl in Shift Then
    Begin
      X := (X Div SnapValue) * SnapValue;
      Y := (Y Div SnapValue) * SnapValue;
    End;

    _DragTarget.OnHandleMouseMove(X, Y);

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
    P := Vector2D_Create(X, Y);
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

Function TUIEditForm.FindWidgetNode(W:UIWidget): TTreeNode;
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
  Source:UIWidget;
begin
  Node := Self.WidgetList.Selected;

  If Node = Nil Then
  Begin
    PropertyList.Target := Nil;
    Exit;
  End;

  Scene.SelectWidget(Scene._SelectedView._Target.GetChildByName(Node.Text));
end;

Function TUIEditForm.AddWidgetNode(W:UIWidget):TTreeNode;
Var
  I:Integer;
Begin
  Result := Nil;
  If (W = Nil) Then
    Exit;

  Result := FindWidgetNode(W);
  If Assigned(Result) Then
    Exit;

{  If Assigned(W.Parent) Then
    Self.AddWidgetNode(W.Parent);}

  Result := WidgetList.Items.AddChildObject(FindWidgetNode(W.Parent), W.Name, W);

  For I:=0 To Pred(W.ChildrenCount) Do
    Self.AddWidgetNode(W.GetChildByIndex(I));
End;

Procedure TUIEditForm.BuildWidgetTree();
Var
  W:UIWidget;
Begin
  WidgetList.Items.Clear();

  If Assigned(_Scene._SelectedView) Then
    Self.AddWidgetNode(_Scene._SelectedView._Target);
End;

Procedure TUIEditForm.UpdateWidgetTree;
Var
  I:Integer;
  N:TTreeNode;
Begin
  For I:=0 To Pred(WidgetList.Items.Count) Do
  Begin
    N := WidgetList.Items[I];
    N.Text := UIWidget(N.Data).Name;
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
  Location:TERRALocation;
Begin
  Location := Engine.Files.Search('cursor_'+Name+'.cur');
  If Location = Nil Then
    Location := Engine.Files.Search('cursor_'+Name+'.ani');

  Cur := LoadCursorFromFile(PAnsiChar(Location.Path));
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

  Self.Color := SkinForeColor;

  TabList.Canvas.Brush.Color := SkinForeColor;
  ApplyFontStyle(TabList.Font, font_Normal);

  (*TabList.BackgroundStartColor := SkinForeColor;
  TabList.BackgroundStopColor := TabList.BackgroundStartColor;
  ApplyFontStyle(TabList.Font, font_Normal);
  ApplyFontStyle(TabList.SelectedFont, font_Selected);

  TabList.SelectedTabStartColor :=  SkinTextColor;
  TabList.SelectedTabStopColor := TabList.SelectedTabStartColor;

  TabList.TabStartColor :=  SkinForeColor;
  TabList.TabStopColor := TabList.TabStartColor;

  TabList.Cursor := customDefault;*)

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

  //_Scene._SelectedView._Target.DeleteWidget(_Scene._SelectedWidget);
  _Scene._SelectedWidget.Delete();
  _Scene._SelectedWidget := Nil;
  PropertyList.Target := Nil;
end;

procedure TUIEditForm.WidgetListEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
  If Node.Data = Nil Then
    Exit;

  UIWidget(Node.Data).Name := S;
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

    Self._Scene.Select(TabList.TabIndex);

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
    Ord('1'): Self.Image1Click(Sender);
    Ord('2'): Self.TiledRect1Click(Sender);
    Ord('3'): Self.Label1Click(Sender);
    Ord('4'): Self.InputText1Click(Sender);
(*    Ord('5'): Self.Radiobox1Click(Sender);
    Ord('6'): Self.Combobox1Click(Sender);
//    Ord('7'): Self.Icon1Click(Sender);
    Ord('8'): Self.Sprite1Click(Sender);
    Ord('9'): Self.ProgressBar1Click(Sender);*)
  End;
end;


{ UIEditorApplication }
Function UIEditorApplication.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  If StringEquals('UIEditableView', ObjectType) Then
    Result := UIEditableView.Create(KeyName, UIEditForm._Scene)
  Else
    Result := Inherited CreateProperty(KeyName, ObjectType);
End;

procedure TUIEditForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  If WheelDelta>0 Then
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

procedure TUIEditForm.View1Click(Sender: TObject);
begin
  Self._Scene.AddView('test');

  Self.BuildWidgetTree();
end;

(*procedure TUIEditForm.TabListTabSelected(Sender: TObject; ATab: TargetTab; ASelected: Boolean);
begin
  If (_Scene = Nil) Or (Not ASelected) Then
    Exit;

  _Scene.Select(ATab.Index);
end;

procedure TUIEditForm.TabListTabClose(Sender: TObject; ATab: TargetTab);
Var
  Target:UIEditableView;
begin
  Target := _Scene._SelectedView;
  Self.PropertyList.Target := Nil;

  If (ATab.Index>0) Then
    _Scene.Select(Pred(ATab.Index))
  Else
    _Scene.Select(Succ(ATab.Index));

  _Scene._Views.Remove(Target);
  Self.TabList.RemoveTab(ATab);
end;*)

procedure TUIEditForm.Settings1Click(Sender: TObject);
begin
  ProjectSettingsForm.ShowModal;
end;

procedure TUIEditForm.DataSources1Click(Sender: TObject);
begin
  ListEditForm.ShowWithTarget(Self._Scene._Datasources, False);
end;

procedure TUIEditForm.Paths1Click(Sender: TObject);
begin
  ListEditForm.ShowWithTarget(Self._Scene._Paths, True);
end;

procedure TUIEditForm.NewProjectClick(Sender: TObject);
begin
  Self._Scene.AddView('untitled');
end;

procedure TUIEditForm.TabListChange(Sender: TObject);
begin
  If (_Scene = Nil) Then
    Exit;

  _Scene.Select(TabList.TabIndex);
end;

end.
