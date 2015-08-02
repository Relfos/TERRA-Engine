Unit TERRA_UIWidget;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_Collections,
  TERRA_Vector2D, TERRA_Color, TERRA_Matrix3x3, TERRA_Texture,
  TERRA_ClipRect, TERRA_Tween, TERRA_FontRenderer, TERRA_SpriteManager,
  TERRA_UIDimension, TERRA_EnumProperty, TERRA_DataSource;

Const
  waTopLeft     = 0;
  waTopCenter   = 1;
  waTopRight    = 2;
  waLeftCenter  = 3;
  waCenter      = 4;
  waRightCenter = 5;
  waBottomLeft     = 6;
  waBottomCenter   = 7;
  waBottomRight    = 8;

  widgetAnimateAlpha  = 1;
  widgetAnimatePosX   = 2;
  widgetAnimatePosY   = 4;
  widgetAnimateRotation = 8;
  widgetAnimateScale    = 16;
  widgetAnimateSaturation  = 32;
  widgetAnimatePosX_Bottom = 64;
  widgetAnimatePosY_Bottom = 128;

Type
  UIDragMode = (
    UIDrag_Move,
    UIDrag_Left,
    UIDrag_Right,
    UIDrag_Top,
    UIDrag_Bottom,

    UIDrag_TopLeft,
    UIDrag_TopRight,
    UIDrag_BottomLeft,
    UIDrag_BottomRight
  );



  UIDirection = (
    UIDirection_Vertical,
    UIDirection_Horizontal
  );


  WidgetState = (
    widget_Default,
    widget_Busy,
    widget_Selected,
    widget_Highlighted,
    widget_Disabled
  );

  UIWidget = Class;
  UIWidgetClass = Class Of UIWidget;
  WidgetEventHandler = Procedure(Src:UIWidget) Of Object;

	UIWidget = Class(CollectionObject)
    Private
      _Tested:Boolean;
      _RenderFrameID:Cardinal;

      _Width:DimensionProperty;
      _Height:DimensionProperty;
			_Position:Vector2DProperty;
      _Layer:FloatProperty;
      _Align:EnumProperty;
      _Color:ColorProperty;
      _Rotation:AngleProperty;
      _Scale:FloatProperty;
      _Saturation:FloatProperty;
			_Visible:BooleanProperty;
      _Skin:StringProperty;     
      _TabIndex:IntegerProperty;
      _DataSource:DataSourceProperty;

      _Deleted:Boolean;

			Function GetAbsolutePosition:Vector2D;
      Function GetRelativePosition:Vector2D;

      Function GetHeight: UIDimension;
      Function GetWidth: UIDimension;

      Function GetTabIndex: Integer;
      Procedure SetTabIndex(const Value: Integer);
      Procedure SetParent(Target:UIWidget);

		Protected
			_Parent:UIWidget;

      _ChildrenList:Array Of UIWidget;
      _ChildrenCount:Integer;

      _Scroll:UIWidget;
      _ScrollValue:Single;

      _BasePropertiesIndex:Integer;
      _CustomPropertiesIndex:Integer;

      _Tooltip:TERRAString;
      _NeedsUpdate:Boolean;
      _NeedsHide:Boolean;

      _State:WidgetState;

      _MouseOver:Boolean;

      _Enabled:Boolean;

      _TabControl:UIWidget;

      _Dragging: Boolean;
      _DragMode:UIDragMode;
      _DragX: Single;
      _DragY: Single;
      _DragSize:Vector2D;
      _DragStart:Vector2D;

      _Transform:Matrix3x3;
      _InverseTransform:Matrix3x3;
      _TransformChanged:Boolean;

      _LastState:Integer;

      _UsingHighLightProperties:Boolean;
      _SelectedWithKeyboard:Boolean;
      _InheritColor:Boolean;

      _Hitting:Boolean;
      _HitTime:Cardinal;

      _Size:Vector2D;
      _Pivot:Vector2D;
      _Center:Vector2D;

      _OriginalColor:Color;
      _OriginalPosition:Vector2D;

      _ColorTable:TERRATexture;

      _ClipRect:ClipRect;

      _VisibleFrame:Cardinal;

      _DropShadowColor:Color;

      _HighlightGroup:Integer;

      _Sprite:TERRASprite;

      Procedure UpdateSprite; Virtual; 

      Procedure InitProperties();
      Procedure ExpandProperties(Count:Integer);

      Function GetUpControl():UIWidget;
      Function GetDownControl():UIWidget;
      Function GetLeftControl():UIWidget;
      Function GetRightControl():UIWidget;

      Function GetAlign: Integer;
      Procedure SetAlign(const Value: Integer);

      Procedure SetAbsolutePosition(Pos:Vector2D);
      Procedure SetRelativePosition(Const Pos:Vector2D);

      Procedure SetVisible(Value:Boolean);
      Procedure SetLayer(Z:Single);
      Procedure SetColor(MyColor:Color);
      Procedure SetRotation(const Value: Single);
      Procedure SetSaturation(const Value: Single);
      Procedure SetScale(const Value: Single);

      Function GetRotation():Single;
      Function GetScale():Single;

      Function GetTabControl():UIWidget;

      Function GetDataValue():TERRAString;

      Procedure UpdateHighlight(); Overload;
      Procedure UpdateHighlight(Condition:Boolean); Overload;

      Procedure UpdateProperties();

      Function IsSelected: Boolean;

      Function AdjustWidth(NewWidth:Single):Single;
      Function AdjustHeight(NewHeight:Single):Single;

      Function HasMouseOver():Boolean; Virtual;

      Function GetFontRenderer: FontRenderer;

      Function OutsideClipRect(X,Y:Single):Boolean;

      Procedure ResetClipRect();

      Procedure SetEnabled(Value:Boolean);

      Procedure UpdateLanguage; 

      (*Procedure DrawText(Const Text:TERRAString; Const X,Y, Layer:Single; Const TextRect:Vector2D; Scale:Single; ID:Integer; Selected:Boolean; Const TextColor:Color);
      Procedure DrawComponent(X, Y, Layer:Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean = True);
      Procedure DrawCroppedComponent(X, Y, Layer, U1, V1, U2, V2:Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean = True);*)

      Procedure SetObjectName(const Value: TERRAString); Override;

      Procedure ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);

      Property FontRenderer:FontRenderer Read GetFontRenderer;

		Public
      Tag:Integer;
      DisableHighlights:Boolean;
      DisableUIColor:Boolean;
      UserData:TERRAString;
      Draggable:Boolean;

      OnMouseClick:WidgetEventHandler;
      OnMouseRelease:WidgetEventHandler;
      OnMouseOver:WidgetEventHandler;
      OnMouseOut:WidgetEventHandler;
      OnBeginDrag:WidgetEventHandler;
      OnEndDrag:WidgetEventHandler;

      Constructor Create(Const Name:TERRAString; Parent:UIWidget);
      Procedure Release; Override;

      Class Function AddTemplate(Template:UIWidget):UIWidget;
      Class Function GetTemplate(Const TemplateName:TERRAString):UIWidget;
      Class Function DeleteTemplate(Const TemplateName:TERRAString):Boolean;

      Procedure StartHighlight(); Virtual;
      Procedure StopHighlight(); Virtual;

      Function IsSelectable():Boolean; Virtual;
      Function CanHighlight(GroupID:Integer):Boolean;

      Procedure OnHit(Handler:WidgetEventHandler); Virtual;
      Procedure OnHighlight(Prev:UIWidget); Virtual;

      Procedure Delete();

      Function CanRender():Boolean;
      Function AllowsEvents(): Boolean;

      Procedure PickAt(Const X, Y:Integer; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget = Nil);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Procedure Render; Virtual;

      Procedure UpdateRects; Virtual;
      Function UpdateTransform():Boolean; Virtual;

			Function OnKeyDown(Key:Word):Boolean;Virtual;
			Function OnKeyUp(Key:Word):Boolean;Virtual;
			Function OnKeyPress(Key:Word):Boolean;Virtual;

			Function GetVisible:Boolean;
			Function GetLayer:Single;
      Function GetColor:Color;
			Function GetSaturation:Single;
      Function GetColorTable:TERRATexture;
      Function GetHighlightGroup:Integer;

      Procedure ClipChildren(Const Clip:ClipRect);

      Function GetDimension(Const Dim:UIDimension; Const Target:UIDimensionTarget):Single;

      Procedure ConvertGlobalToLocal(Var V:Vector2D);
      Procedure ConvertLocalToGlobal(Var V:Vector2D);

      Procedure SetPositionRelativeToOther(Other:UIWidget; OfsX, OfsY:Single);

      Procedure GetScrollOffset(Out OfsX, OfsY:Single);

			Procedure OnLanguageChange();Virtual;

      Procedure NullEventHandler(Src:UIWidget);

      Function OnRegion(X,Y:Single): Boolean; Virtual;
      Function OnCustomRegion(X,Y:Integer; X1,Y1,X2,Y2:Single):Boolean;

      Procedure AddChild(W:UIWidget);
      Procedure RemoveChild(W:UIWidget);
      Procedure RemoveAllChildren();
      Function GetChildByIndex(Index:Integer):UIWidget; Overload;
      Function GetChildByName(Const Name:TERRAString):UIWidget; Overload;
      Function GetChildByClass(ChildClass:UIWidgetClass; Index:Integer = 0):UIWidget; Overload;

      Function OnSelectRight():Boolean; Virtual;
      Function OnSelectLeft():Boolean; Virtual;
      Function OnSelectUp():Boolean; Virtual;
      Function OnSelectDown():Boolean; Virtual;
      Function OnSelectAction():Boolean; Virtual;

      Function IsOutsideScreen():Boolean;

      Procedure SetHeight(const Value: UIDimension);
      Procedure SetWidth(const Value: UIDimension);

      Procedure SetChildrenVisibilityByTag(Tag:Integer; Visibility:Boolean);

      Procedure SetClipRect(Value:ClipRect);
      Procedure UpdateClipRect(Clip:ClipRect; LeftBorder:Single = 0.0; TopBorder:Single = 0.0; RightBorder:Single = 0.0; BottomBorder:Single = 0.0);

      Function IsHighlighted():Boolean;
      Function HasHighlightedChildren():Boolean;

			Procedure OnMouseDown(X,Y:Integer; Button:Word); Virtual;
			Procedure OnMouseUp(X,Y:Integer; Button:Word); Virtual;
			Procedure OnMouseMove(X,Y:Integer); Virtual;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer);Virtual;

      Function GetIndex():Integer;

      Function IsSameFamily(Other:UIWidget):Boolean;

      Function GetSize:Vector2D; Virtual;

      Function GetClipRect():ClipRect;

      Procedure BeginDrag(X,Y:Integer; Mode:UIDragMode = UIDrag_Move);
      Procedure FinishDrag();
      Procedure CancelDrag();

      Function SupportDrag(Mode:UIDragMode):Boolean; Virtual; 

      Function Show(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;
      Function Hide(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;
      Function ToggleVisibility(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;

      //Procedure CenterOnScreen(CenterX:Boolean = True; CenterY:Boolean = True);
      //Procedure CenterOnParent(CenterX:Boolean = True; CenterY:Boolean = True);
      Procedure CenterOnPoint(X,Y:Single);

			Property Visible:Boolean Read GetVisible Write SetVisible;

			Property AbsolutePosition:Vector2D Read GetAbsolutePosition Write SetAbsolutePosition;
			Property RelativePosition:Vector2D Read GetRelativePosition Write SetRelativePosition;

      Property Pivot:Vector2D Read _Pivot Write _Pivot;
      Property Size:Vector2D Read GetSize;
			Property Layer:Single Read GetLayer Write SetLayer;

      Property TabIndex:Integer Read GetTabIndex Write SetTabIndex;
      Property TabControl:UIWidget Read GetTabControl Write _TabControl;

      Property InheritColor:Boolean Read _InheritColor Write _InheritColor;

      Property Color:TERRA_Color.Color Read GetColor Write SetColor;
      Property ColorTable:TERRATexture Read GetColorTable Write _ColorTable;
      Property Saturation:Single Read GetSaturation Write SetSaturation;
      Property Rotation:Single Read GetRotation Write SetRotation;
      Property Scale:Single Read GetScale Write SetScale;

      Property Scroll:UIWidget Read _Scroll Write _Scroll;

      Property Parent:UIWidget Read _Parent Write SetParent;
      Property Align:Integer Read GetAlign Write SetAlign;

      Property ChildrenCount:Integer Read _ChildrenCount;

      Property ClipRect:ClipRect Read GetClipRect Write SetClipRect;

      Property Center:Vector2D Read _Center Write _Center;

      Property Enabled:Boolean  Read _Enabled Write SetEnabled;

      Property Selected:Boolean Read IsSelected;

      Property DropShadowColor:Color Read _DropShadowColor Write _DropShadowColor;

      Property HighlightGroup:Integer Read GetHighlightGroup Write _HighlightGroup;

      Property Width:UIDimension Read GetWidth Write SetWidth;
      Property Height:UIDimension Read GetHeight Write SetHeight;

      Property Deleted:Boolean Read _Deleted;
      Property SelectedWithKeyboard:Boolean Read _SelectedWithKeyboard Write _SelectedWithKeyboard;
      Property TransformChanged:Boolean Read _TransformChanged Write _TransformChanged;
	End;

  UIInstancedWidget = Class(UIWidget)
    Protected
      _TemplateName:StringProperty;
      _TemplateIndex:Integer;

      Function InitFromTemplate(Template:UIWidget):UIWidget;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const TemplateName:TERRAString);
      Procedure Release(); Override;

      Function GetObjectType:TERRAString; Override;
  End;


Implementation

Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Math, TERRA_GraphicsManager, TERRA_UI, TERRA_UITiledRect;

Procedure ShowWidget(Source:UIWidget); CDecl;
Begin
  Source.Visible := True;
End;

Procedure HideWidget(Source:Pointer); CDecl;
Var
  W:UIWidget;
Begin
  W := UIWidget(Source);
  W.SetColor(W._OriginalColor);
  W.SetRelativePosition(W._OriginalPosition);
  W.SetVisible(False);

(* TODO  If (W.UI.Highlight = W) Or (W.HasHighlightedChildren()) Then
    W.UI.Highlight := Nil;*)
End;

{ UIWidget }
Constructor UIWidget.Create(Const Name:TERRAString; Parent:UIWidget);
Var
  Target:UIWidget;
Begin
  _ObjectName := Name;

  //_Component := UIComponentImage.Create();

  Self.InitProperties();

  SetVisible(True);
  _Enabled := True;

  _Pivot := VectorCreate2D(0.5, 0.5);
  SetScale(1.0);
  SetRotation(0.0);
  SetSaturation(1.0);
  SetColor(ColorWhite);
  _ColorTable := Nil;

  _ClipRect.Style := clipNothing;

  //_DropShadowColor := ColorNull;
  _DropShadowColor := ColorGrey(0, 255);


  _InheritColor := True;
  _TransformChanged := True;

  If Assigned(Parent) Then
    Parent.AddChild(Self);
End;


Procedure UIWidget.Release();
Begin
  ReleaseObject(_Sprite);
  ReleaseObject(_Width);
  ReleaseObject(_Height);
  ReleaseObject(_Visible);
  ReleaseObject(_Position);
  ReleaseObject(_Color);
  ReleaseObject(_Rotation);
  ReleaseObject(_Layer);
  ReleaseObject(_Scale);
  ReleaseObject(_Saturation);
  ReleaseObject(_Align);
  ReleaseObject(_Skin);
  ReleaseObject(_TabIndex);
  ReleaseObject(_DataSource);
End;

Procedure UIWidget.InitProperties;
Begin
  _Width := DimensionProperty.Create('width', UIPixels(0));
  _Height := DimensionProperty.Create('height', UIPixels(0));
  _Visible := BooleanProperty.Create('visible', True);
  _Position := Vector2DProperty.Create('position', VectorCreate2D(0, 0));
  _Layer := FloatProperty.Create('layer', 1.0);
  _Color := ColorProperty.Create('color', ColorWhite);
  _Rotation := AngleProperty.Create('rotation', 0.0);
  _Scale := FloatProperty.Create('scale', 1.0);
  _Saturation := FloatProperty.Create('saturation', 1.0);
  _Skin := StringProperty.Create('skin', '');
  _TabIndex := IntegerProperty.Create('tabindex', -1);
  _Align := EnumProperty.Create('align', 0, UIManager.Instance.AlignEnums);
  _DataSource := DataSourceProperty.Create('datasource', '');

  _BasePropertiesIndex := 0;
  _CustomPropertiesIndex := 13;
End;

Function UIWidget.GetPropertyByIndex(Index:Integer):TERRAObject;
Begin
  If (Index>=_CustomPropertiesIndex) Then
  Begin
    Dec(Index, _CustomPropertiesIndex);
    If (Index<_ChildrenCount) Then
      Result := _ChildrenList[Index]
    Else
      Result := Nil;
    Exit;
  End;

  Case Index Of
  0: Result := _Visible;
  1: Result := _Position;
  2: Result := _Layer;
  3: Result := _Align;
  4: Result := _Width;
  5: Result := _Height;
  6: Result := _Color;
  7: Result := _Rotation;
  8: Result := _Scale;
  9: Result := _Saturation;
  10: Result := _Skin;
  11: Result := _TabIndex;
  12: Result := _DataSource;
  Else
    Result := Nil;
  End;
End;

Procedure UIWidget.ExpandProperties(Count:Integer);
Begin
  _BasePropertiesIndex := _CustomPropertiesIndex;
  Inc(_CustomPropertiesIndex, Count);
End;


Function UIWidget.HasHighlightedChildren():Boolean;
Var
  I:Integer;
Begin
(* TODO  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I] = UI.Highlight) Then
  Begin
    Result := True;
    Exit;
  End Else
  Begin
    Result := _ChildrenList[I].HasHighlightedChildren();
    If Result Then
      Exit;
  End;
*)
  Result := False;
End;

Var
  _UITemplatesList:List;

Class Function UIWidget.AddTemplate(Template:UIWidget): UIWidget;
Begin
  Result := Template;

  If Result = Nil Then
    Exit;

  If _UITemplatesList = Nil Then
    _UITemplatesList := List.Create();

  _UITemplatesList.Add(Template);
End;

Class Function UIWidget.GetTemplate(Const TemplateName: TERRAString): UIWidget;
Var
  W:UIWidget;
  It:Iterator;
Begin
  Result := Nil;
  If _UITemplatesList = Nil Then
    Exit;

  It := _UITemplatesList.GetIterator();
  While It.HasNext Do
  Begin
    W := UIWidget(It.Value);

    If (StringEquals(W.Name, TemplateName)) Then
    Begin
      Result := W;
      Break;
    End;
  End;
  ReleaseObject(It);
End;


Class Function UIWidget.DeleteTemplate(Const TemplateName: TERRAString): Boolean;
Var
  W:UIWidget;
  It:Iterator;
Begin
  Result := False;
  If _UITemplatesList = Nil Then
    Exit;

  It := _UITemplatesList.GetIterator();
  While It.HasNext Do
  Begin
    W := UIWidget(It.Value);

    If (StringEquals(W.Name, TemplateName)) Then
    Begin
      W.Discard();
      Break;
    End;
  End;
  ReleaseObject(It);
End;


Function UIWidget.IsHighlighted: Boolean;
Begin
(* TODO
  Result := (UI._Highlight = Self) Or (_Parent<>Nil) And (_Parent.IsHighlighted);
  *)
End;

{
Procedure UIWidget.CenterOnScreen(CenterX:Boolean = True; CenterY:Boolean = True);
Begin
  _Align := waTopLeft;
  Self.UpdateRects;

  If CenterX Then
    _Position.X := (UIManager.Instance.Width * 0.5) - (_Size.X * 0.5  * _Scale);
  If CenterY Then
    _Position.Y := (UIManager.Instance.Height * 0.5) - (_Size.Y * 0.5 *_Scale);
End;}

Procedure UIWidget.CenterOnPoint(X,Y:Single);
Begin
  Self.Align := waTopLeft;
  Self.UpdateRects;

  _Position.X.Value := X - _Size.X * 0.5;
  _Position.Y.Value := Y - _Size.Y * 0.5;
End;

{Procedure UIWidget.CenterOnParent(CenterX, CenterY: Boolean);
Begin
  If Not Assigned(_Parent) Then
    Exit;

  Self.UpdateRects;
  _Align := waTopLeft;

  If CenterX Then
    _Position.X := (_Parent._Size.X * 0.5) - (_Size.X * 0.5);
  If CenterY Then
    _Position.Y := (_Parent._Size.Y * 0.5) - (_Size.Y * 0.5);
End;}

Procedure UIWidget.StartHighlight;
Begin
  Self._Color.Value := ColorBlack;
End;

Procedure UIWidget.StopHighlight;
Begin
  Self._Color.Value := ColorWhite;
End;

Procedure UIWidget.OnHit(Handler:WidgetEventHandler);
Var
  N:Integer;
  Target:TERRA_Color.Color;
  Ease:TweenEaseType;
Begin
  Target := ColorScale(Self.Color, 0.5);
  N := 150;

  Ease := easeLinear;

  Self._Color.AddTween(Ease, Target, N, 0);
  Self._Color.AddTween(Ease, Self.Color, N, N, TweenCallback(Handler), Self);
End;

Procedure UIWidget.OnLanguageChange;
Begin
  // do nothing
End;

Procedure UIWidget.UpdateRects();
Begin
  _Size.X := Self.GetDimension(Self.Width, uiDimensionWidth);
  _Size.Y := Self.GetDimension(Self.Height, uiDimensionHeight);
End;

Function UIWidget.GetSize: Vector2D;
Begin
  If (_Size.X<=0) Or (_Size.Y<=0) Then
    Self.UpdateRects();

  Result.X := _Size.X;
  Result.Y := _Size.Y;
End;

Function UIWidget.IsSameFamily(Other:UIWidget): Boolean;
Begin
  Result := (Self = Other);
  If (Not Result) And (Other<>Nil) And (Other._Parent<>Nil) Then
    Result := IsSameFamily(Other._Parent);
End;

Function UIWidget.IsSelectable():Boolean;
Begin
  Result := Assigned(Self.OnMouseClick);
End;

Procedure UIWidget.ConvertGlobalToLocal(Var V:Vector2D);
Begin
  If (Self.Rotation<>0.0) Then
    V := Self._InverseTransform.Transform(V);

  V.Subtract(Self.AbsolutePosition);
End;

Procedure UIWidget.ConvertLocalToGlobal(Var V:Vector2D);
Begin
  V.Add(Self.AbsolutePosition);

  If (Self.Rotation<>0.0) Then
    V := Self._Transform.Transform(V);
End;


Function UIWidget.CanHighlight(GroupID:Integer): Boolean;
Begin
  Result := (Self.Visible) And (Self.Enabled) And (Self.IsSelectable()) And (Self.HighlightGroup = GroupID) And (Not Self.HasPropertyTweens());
End;

Function UIWidget.GetDownControl():UIWidget;
Var
  W:UIWidget;
  It:Iterator;
  Base:Vector2D;
  GroupID:Integer;
  Min, Dist, Y:Single;
Begin
  Result := Nil;

  Min := 99999;
  Base := Self.AbsolutePosition;
  Base.Y := Base.Y + Self.Size.Y;
  GroupID := Self.HighlightGroup;

(* TODO  It := Self.UI.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (W = Self) Or (Not W.CanHighlight(GroupID)) Then
      Continue;

    Y := W.AbsolutePosition.Y;
    If (Y<Base.Y) Then
      Continue;

    Dist := W.AbsolutePosition.Distance(Base);
    If (Dist< Min) Then
    Begin
      Min := Dist;
      Result := W;
    End;
  End;
  ReleaseObject(It);*)
End;

Function UIWidget.GetUpControl():UIWidget;
Var
  W:UIWidget;
  It:Iterator;
  P,Base:Vector2D;
  GroupID:Integer;
  Min, Dist, Y:Single;
Begin
  Result := Nil;

(* TODO  Min := 99999;
  Base := Self.AbsolutePosition;
  GroupID := Self.HighlightGroup;

  It := Self.UI.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (W = Self) Or (Not W.CanHighlight(GroupID)) Then
      Continue;

    P := W.AbsolutePosition;
    P.Y := P.Y + W.Size.Y;
    Dist := P.Distance(Base);
    If (Dist< Min) And (P.Y<Base.Y) Then
    Begin
      Min := Dist;
      Result := W;
    End;
  End;
  ReleaseObject(It);*)
End;

Function UIWidget.GetRightControl():UIWidget;
Var
  W:UIWidget;
  It:Iterator;
  Base:Vector2D;
  Min, Dist, X:Single;
  GroupID:Integer;
Begin
  Result := Nil;

(*TODO  Min := 99999;
  Base := Self.AbsolutePosition;
  GroupID := Self.HighlightGroup;

  It := Self.UI.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (W = Self) Or (Not W.CanHighlight(GroupID)) Then
      Continue;

    X := W.AbsolutePosition.X;
    Dist := W.AbsolutePosition.Distance(Base);
    If (Dist< Min) And (X>Base.X + Self.Size.X) Then
    Begin
      Min := Dist;
      Result := W;
    End;
  End;
  ReleaseObject(It);*)
End;

Function UIWidget.GetLeftControl():UIWidget;
Var
  W:UIWidget;
  It:Iterator;
  P, Base:Vector2D;
  Min, Dist:Single;
  GroupID:Integer;
Begin
  Result := Nil;

(*TODO  Min := 99999;
  Base := Self.AbsolutePosition;
  GroupID := Self.HighlightGroup;

  It := Self.UI.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (W = Self) Or (Not W.CanHighlight(GroupID)) Then
      Continue;

    P := W.AbsolutePosition;
    P.X := P.X + W.Size.X;
    Dist := P.Distance(Base);
    If (Dist< Min) And (P.X<Base.X) Then
    Begin
      Min := Dist;
      Result := W;
    End;
  End;
  ReleaseObject(It);*)
End;

Function UIWidget.Show(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
Var
  X, Y, TY:Single;
  A:Byte;
Begin
  If Visible Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'UI', 'Showing '+Self.Name+' with animation '+IntToString(AnimationFlags));

  SetVisible(True);

  Self._NeedsHide := False;

  If (AnimationFlags And widgetAnimatePosX<>0) Then
  Begin
    X := _Position.X.Value;
    _Position.X.Value := -(Self.Size.X);
    _Position.X.AddTween(EaseType, X, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosX_Bottom<>0) Then
  Begin
    X := _Position.X.Value;
    _Position.X.Value := UIManager.Instance.Width + (Self.Size.X);
    _Position.X.AddTween(EaseType, X, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimatePosY<>0) Then
  Begin
    Y := _Position.Y.Value;

    TY := -Self.Size.Y;
    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      TY := TY - (UIManager.Instance.Height * 0.5);

    _Position.Y.Value := TY;
    _Position.Y.AddTween(EaseType, Y, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosY_Bottom<>0) Then
  Begin
    Y := _Position.Y.Value;

    TY := UIManager.Instance.Height + Self.Size.Y;
    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      TY := TY + (UIManager.Instance.Height * 0.5);

    _Position.Y.Value := TY;
    _Position.Y.AddTween(EaseType, Y, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateAlpha<>0) Then
  Begin
    A := _Color.Alpha.Value;
    _Color.Alpha.Value := 0;
    _Color.Alpha.AddTween(EaseType, A, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateRotation<>0) Then
  Begin
    X := GetRotation();
    SetRotation(X + (360.0 * RAD) * 4.0);
    _Rotation.AddTween(EaseType, X, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateScale<>0) Then
  Begin
    X := GetScale();
    SetScale(0.0);
    _Scale.AddTween(EaseType, X, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateSaturation<>0) Then
  Begin
    X := GetSaturation();
    SetSaturation(0.0);
    _Saturation.AddTween(EaseType, X, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  Result := True;
End;

Function UIWidget.Hide(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
Var
  Ofs:Single;
Begin
  If (Not Self.Visible) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Not Self._NeedsHide) Then
  Begin
    _OriginalPosition := _Position.Value;
    _OriginalColor := _Color.Value;
  End;

  If (AnimationFlags And widgetAnimatePosX<>0) Then
  Begin
    Ofs := -Self.Size.X;

    If (Self.Align = waCenter) Or (Self.Align = waTopCenter) Or (Self.Align = waBottomCenter) Then
      Ofs := Ofs - Self.AbsolutePosition.X;

    _Position.X.AddTween(EaseType, Ofs, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosX_Bottom<>0) Then
  Begin
    _Position.X.AddTween(EaseType, UIManager.Instance.Width +(Self.Size.X+15), Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimatePosY<>0) Then
  Begin
    Ofs := -Self.Size.Y;

    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      Ofs := Ofs - Self.AbsolutePosition.Y;

    _Position.Y.AddTween(EaseType, Ofs, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosY_Bottom<>0) Then
  Begin
    _Position.Y.AddTween(EaseType, UIManager.Instance.Height + Self.Size.Y, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateAlpha<>0) Then
  Begin
    _Color.Alpha.AddTween(EaseType, 0.0, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  Self._NeedsHide := True;
  Result := True;
End;

Function UIWidget.ToggleVisibility(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
Begin
  If Self.Visible Then
    Result := Hide(AnimationFlags, EaseType, Delay, Duration, Callback)
  Else
    Result := Show(AnimationFlags, EaseType, Delay, Duration, Callback);
End;

Function UIWidget.GetVisible:Boolean;
Begin
  If (Self = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

	Result := _Visible.Value;
  If (Result) And (Assigned(_Parent)) Then
  Begin
    Result := Result And (_Parent.Visible);

    (*TODO
    If (Result) And (Self.TabIndex>=0) And (Parent.TabControl<>Nil) And (_Parent.TabControl Is UITabList) Then
    Begin
      If (UITabList(_Parent.TabControl).SelectedIndex<>Self.TabIndex) Then
        Result := False;
    End;*)
  End;
End;

Procedure UIWidget.SetVisible(Value:Boolean);
Begin
  {If (Self = Nil) Then
    Exit;}

  If (Value = Self.Visible) Then
    Exit;

  //Log(logDebug,'UI', Self._Name+' visibility is now '+BoolToString(Value));
    
  _Visible.Value := Value;

  If Value Then
    _VisibleFrame := GraphicsManager.Instance.FrameID;

  If (Not Value) And (_UsingHighLightProperties) Then
    Self.StopHighlight();
End;

Procedure UIWidget.SetRelativePosition(Const Pos:Vector2D);
Begin
  If (Pos.X = _Position.X.Value) And (Pos.Y = _Position.Y.Value) Then
    Exit;

  _Position.Value := Pos;
  _TransformChanged := True;
End;

Procedure UIWidget.SetAbsolutePosition(Pos:Vector2D);
Begin
  If Assigned(_Parent) Then
    Pos.Subtract(_Parent.AbsolutePosition);

  Self.SetRelativePosition(Pos);
End;

Procedure UIWidget.SetLayer(Z:Single);
Begin
  If (Z = _Layer.Value) Then
    Exit;

  _Layer.Value := Z;
End;

Procedure UIWidget.SetColor(MyColor:Color);
Begin
(*  If (Cardinal(MyColor) = Cardinal(_Color)) Then
    Exit;*)

  _Color.Value := MyColor;
End;

Function UIWidget.GetColor:Color;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  TempAlpha:Byte;
  ParentColor:TERRA_Color.Color;
Begin
	Result := _Color.Value;
  If (Not _InheritColor) Then
    Exit;

	If (Assigned(_Parent)) Then
  Begin
    ParentColor := _Parent.GetColor();
    TempAlpha := Result.A;
		Result := ColorMultiply(Result, ParentColor);

    If ParentColor.A < TempAlpha Then
      Result.A := ParentColor.A
    Else
      Result.A := TempAlpha;
  End;
End;

Function UIWidget.GetRelativePosition:Vector2D;
Begin
  Result := _Position.Value;
End;

Function UIWidget.GetAbsolutePosition:Vector2D;
Var
  Width, Height:Single;
  ParentSize, Center:Vector2D;
Begin
  Result := _Position.Value;

  If (Parent = Nil) Then
    Exit;

  If (Align<>waTopLeft) Then
  Begin
    Width := _Size.X{ * _Scale};
    Height := _Size.Y{ * _Scale};

    {IF _Scale>1 Then
      IntToString(2);}

    ParentSize := _Parent.Size;
{      ParentSize.X := ParentSize.X * _Parent._Scale;
      ParentSize.Y := ParentSize.Y * _Parent._Scale;}

    Center.X := ParentSize.X * 0.5;
    Center.Y := ParentSize.Y * 0.5;

    Case Align Of
      waCenter:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

      waTopCenter:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
      End;

      waTopRight:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
      End;

      waLeftCenter:
      Begin
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

      waRightCenter:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

      waBottomLeft:
      Begin
        Result.Y := (ParentSize.Y - Height) - Result.Y;
      End;

      waBottomCenter:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
        Result.Y := (ParentSize.Y - Height) - Result.Y;
      End;

      waBottomRight:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
        Result.Y := (ParentSize.Y - Height) - Result.Y;
      End;
    End;
  End;

	If (Assigned(_Parent)) Then
		Result.Add(_Parent.GetAbsolutePosition());
End;

Function UIWidget.GetLayer:Single;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
	Result := _Layer.Value;

	If (Assigned(_Parent)) Then
		Result := Result + _Parent.GetLayer();
End;

Function UIWidget.GetSaturation:Single;
Begin
	Result := _Saturation.Value;
	If (Assigned(_Parent)) Then
		Result := Result * _Parent.GetSaturation();
End;

Function UIWidget.GetHighlightGroup:Integer;
Begin
	Result := _HighlightGroup;
	If (Assigned(_Parent)) And (_HighlightGroup<=0) Then
		Result := _Parent.GetHighlightGroup();
End;


Function UIWidget.GetColorTable:TERRATexture;
Begin
	Result := _ColorTable;
	If (Result = Nil) And (Assigned(_Parent)) Then
		Result := _Parent.GetColorTable();
End;

Procedure UIWidget.UpdateProperties();
Begin
  // do nothing
End;

(*Procedure UIWidget.DrawText(Const Text:TERRAString; Const X,Y, Layer:Single; Const TextRect:Vector2D; Scale:Single; ID:Integer; Selected:Boolean; Const TextColor:Color);
Var
  P:Vector2D;
  Z:Single;
  OfsX,OfsY:Single;
  M:Matrix3x3;
  C:TERRA_Color.Color;
  CurrentState, DefaultState:Integer;
Begin
  Self.CopyProperties(ID, Selected, CurrentState, DefaultState);

  C := _Properties.TextColor;
  C.A := _Properties.QuadColor.A;
  C := ColorMultiply(C, TextColor);
  //Color.A := Trunc(Color.A * UI.Instance._Alpha);

  P := Self.GetAbsolutePosition();
  Z := Self.GetLayer + Layer;

  Self.GetScrollOffset(OfsX, OfsY);

  P.X := P.X + X + OfsX;
  P.Y := P.Y + Y + OfsY;

  If (Scale = 1.0) Then
    M := _Transform
  Else
  Begin
    M := MatrixTransformAroundPoint2D(VectorCreate2D(P.X + TextRect.X* 0.5, P.Y + TextRect.Y* 0.5), MatrixScale2D(Scale, Scale));
    M := MatrixMultiply3x3(M, _Transform);
  End;

  _FontRenderer.SetFont(_Properties.TextFont);
  _FontRenderer.SetClipRect(_Properties.Clip);
  _FontRenderer.SetTransform(M);
  _FontRenderer.SetDropShadow(_DropShadowColor);
  _FontRenderer.SetColor(C);

  _FontRenderer.DrawText(P.X, P.Y, Z, Text);
End;*)


Function UIWidget.OnKeyDown(Key:Word):Boolean;
Begin
  RemoveHint(Key);
	Result := False;
End;

Function UIWidget.OnKeyUp(Key:Word):Boolean;
Var
  I:Integer;
Begin
  If Not Self.Visible Then
  Begin
    Result := False;
    Exit;
  End;

(*  If (Key = UI.Key_Action) Then
  Begin
    Result := Self.OnSelectAction();
  End Else
  If (Key = UI.key_Up) Then
  Begin
    Result := Self.OnSelectUp();
  End Else
  If (Key = UI.Key_Down) Then
  Begin
    Result := Self.OnSelectDown();
  End Else
  If (Key = UI.key_Left) Then
  Begin
    Result := Self.OnSelectLeft();
  End Else
  If (Key = UI.key_Right) Then
  Begin
    Result := Self.OnSelectRight();
  End Else
  	Result := False;

  If Result Then
    Exit;

  For I:=0 To Pred(_ChildrenCount) Do
  Begin
    Result := _ChildrenList[I].OnKeyUp(Key);
    If Result Then
      Exit;
  End;*)
End;

Function UIWidget.OnKeyPress(Key:Word):Boolean;
Begin
  RemoveHint(Key);
	Result := False;
End;

Function UIWidget.OnRegion(X,Y:Single): Boolean;
Var
  V:Vector2D;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X:'+IntToString(X)+' Y:'+IntToString(Y));{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.OnRegion called');{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X1:'+IntToString(Trunc(_Corners[0].X))+' Y1:'+IntToString(Trunc(_Corners[0].Y)));{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X2:'+IntToString(Trunc(_Corners[2].X))+' Y2:'+IntToString(Trunc(_Corners[2].Y)));{$ENDIF}

  If (GraphicsManager.Instance.FrameID = Self._VisibleFrame) Or (OutsideClipRect(X,Y)) Then
  Begin
    Result := False;
    {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Cliprect clipped!');{$ENDIF}
    Exit;
  End;

  V.X := X;
  V.Y := Y;
  Self.ConvertGlobalToLocal(V);

  Result := (V.X>=0.0) And (V.X <= Size.X) And (V.Y >= 0) And (V.Y <= Size.Y);
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Region result for '+_Name+' was '+BoolToString(Result));{$ENDIF}
End;

Function UIWidget.AllowsEvents(): Boolean;
Begin
  If (Not Visible) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;

(* TODO  If (_UI.Modal = Nil) Or  (Self = _UI.Modal) Then
    Exit;

  If Assigned(Self.Parent) Then
    Result := Self.Parent.AllowsEvents();*)
End;

Procedure UIWidget.PickAt(Const X, Y:Integer; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget);
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.PickAt called');{$ENDIF}

  If (Self.Layer < Max) Or (Not Self.OnRegion(X,Y)) Or (Self = Ignore) Then
    Exit;

  CurrentPick := Self;
  Max := Self.Layer;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].AllowsEvents()) Then
  Begin
    _ChildrenList[I].PickAt(X, Y, CurrentPick, Max, Ignore);
  End;
End;

Procedure UIWidget.BeginDrag(X,Y:Integer; Mode:UIDragMode);
Begin
  If (Assigned(OnBeginDrag)) Then
    Self.OnBeginDrag(Self);

(*  _UI._Dragger := Self;
  _DragMode := Mode;
  _Dragging := True;

  _DragStart := _Position.Value;
  _DragSize := _Size;

  _DragX := (X-_Position.X.Value);
  _DragY := (Y-_Position.Y.Value);

  TODO
  *)
End;

Procedure UIWidget.CancelDrag;
Begin
(*TODO  If _Dragging Then
  Begin
    _Position.Value := _DragStart;
    _Dragging := False;
    If _UI._Dragger = Self Then
      _UI._Dragger := Nil;

    Self._TransformChanged := True;
  End;
  *)
End;

Procedure UIWidget.FinishDrag();
Begin
(*TODO  If (_UI._Dragger <> Self) Then
    Exit;

  If (Assigned(OnEndDrag)) Then
    Self.OnEndDrag(Self);

  _Dragging := False;
  _UI._Dragger := Nil;*)
End;

Procedure UIWidget.OnMouseDown(X,Y:Integer;Button:Word);
Begin
  If (Draggable) Then
  Begin
    If (Not _Dragging) Then
      Self.BeginDrag(X,Y);

    Exit;
  End;

  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Found, and has handler: '+BoolToString(Assigned(OnMouseClick)));{$ENDIF}
  Self.OnHit(OnMouseClick);
End;

Procedure UIWidget.OnMouseUp(X,Y:Integer;Button:Word);
Var
  I:Integer;
Begin
  If (_Dragging) Then
  Begin
    Self.FinishDrag();
    Exit;
  End;

  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.OnMouseUp called');{$ENDIF}

  If (Assigned(OnMouseRelease)) Then
  Begin
    OnMouseRelease(Self);
  End;
End;

Procedure UIWidget.ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);
Var
  Extra:Single;
Begin
  Case Mode Of
    UIDrag_Move:
      Begin
        _Position.X.Value := UISnap(PX);;
        _Position.Y.Value := UISnap(PY);
      End;

    UIDrag_Left:
      Begin
        Extra := AdjustWidth(_DragSize.X + (_DragStart.X - PX));
        _Position.X.Value := PX + Extra;
        Self.UpdateRects();
      End;

    UIDrag_Top:
      Begin
        Extra := AdjustHeight(_DragSize.Y + (_DragStart.Y - PY));
        _Position.Y.Value := PY + Extra;
        Self.UpdateRects();
      End;

    UIDrag_Right:
      Begin
        AdjustWidth(_DragSize.X + (PX - _DragStart.X));
        Self.UpdateRects();
      End;

    UIDrag_Bottom:
      Begin
        AdjustHeight(_DragSize.Y + (PY - _DragStart.Y));
        Self.UpdateRects();
      End;

    End;
End;

Procedure UIWidget.OnMouseMove(X,Y:Integer);
Var
  I:Integer;
  B:Boolean;
  PX, PY:Single;
Begin
  If (_Dragging) Then
  Begin
    PX := Trunc(X - _DragX);
    PY := Trunc(Y - _DragY);

    Case _DragMode Of
    UIDrag_TopLeft:
      Begin
        Self.ApplyDragMode(PX, PY, UIDrag_Top);
        Self.ApplyDragMode(PX, PY, UIDrag_Left);
      End;

    UIDrag_TopRight:
      Begin
        Self.ApplyDragMode(PX, PY, UIDrag_Top);
        Self.ApplyDragMode(PX, PY, UIDrag_Right);
      End;

    UIDrag_BottomLeft:
      Begin
        Self.ApplyDragMode(PX, PY, UIDrag_Bottom);
        Self.ApplyDragMode(PX, PY, UIDrag_Left);
      End;

    UIDrag_BottomRight:
      Begin
        Self.ApplyDragMode(PX, PY, UIDrag_Bottom);
        Self.ApplyDragMode(PX, PY, UIDrag_Right);
      End;

    Else
      ApplyDragMode(PX, PY, _DragMode);
    End;

    _TransformChanged := True;

    Exit;
  End;
End;

Procedure UIWidget.OnMouseWheel(X,Y:Integer; Delta:Integer);
Begin
  // do nothing
End;

Procedure UIWidget.UpdateHighlight();
Begin
  UpdateHighlight(IsHighlighted());
End;

Procedure UIWidget.UpdateHighlight(Condition:Boolean);
Begin
  If (_UsingHighLightProperties = Condition) Then
    Exit;

  _UsingHighLightProperties := Condition;
  If (Condition) Then
    Self.StartHighlight()
  Else
    Self.StopHighlight();
End;

Function UIWidget.HasMouseOver: Boolean;
Begin
  Result := (Assigned(OnMouseOver)) Or (Assigned(OnMouseOut)) Or (Assigned(OnMouseClick));
End;

Procedure UIWidget.SetSaturation(const Value: Single);
Begin
  _Saturation.Value := Value;
  _NeedsUpdate := True;
End;

Procedure UIWidget.SetScale(const Value: Single);
Begin
  _Scale.Value := Value;
  _NeedsUpdate := True;
  _TransformChanged := True;
End;


Procedure UIWidget.SetRotation(const Value: Single);
Begin
  _Rotation.Value := Value;
  _NeedsUpdate := True;
  _TransformChanged := True;
End;

Function UIWidget.UpdateTransform():Boolean;
Var
  I:Integer;
  Center:Vector2D;
  Pos:Vector2D;
  W,H, Ratio:Single;
  OfsX,OfsY:Single;
Begin
  Result := False;

  If (_NeedsHide) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.HasPropertyTweens();
    _NeedsHide := False;
    HideWidget(Self);
  End;

  If (Not _TransformChanged) Then
    Exit;

  _TransformChanged := False;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) Then
    _ChildrenList[I]._TransformChanged := True;

  If Assigned(_Parent) Then
    Ratio := 1.0
  Else
    Ratio := UIManager.Instance.Ratio;

  Center := Self.GetSize();
  Center.X := Center.X * _Pivot.X;
  Center.Y := Center.Y * _Pivot.Y;
  Center.Add(Self.GetAbsolutePosition());

  If (_Rotation.Value <> 0.0) Then
    _Transform := MatrixRotationAndScale2D(_Rotation.Value, _Scale.Value, _Scale.Value * Ratio)
  Else
    _Transform := MatrixScale2D(_Scale.Value, _Scale.Value * Ratio);

  _Transform := MatrixTransformAroundPoint2D(Center, _Transform);

  If Assigned(_Parent) Then
    _Transform := MatrixMultiply3x3(_Transform, Parent._Transform);

  Self.GetScrollOffset(OfsX, OfsY);

  _InverseTransform := MatrixInverse2D(_Transform);

  Result := True;
End;

Function UIWidget.GetChildByIndex(Index:Integer):UIWidget;
Begin
  If (Index>=0) And (Index<=Self.ChildrenCount) Then
    Result := _ChildrenList[Index]
  Else
    Result := Nil;
End;

Function UIWidget.GetChildByName(Const Name:TERRAString):UIWidget;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (StringEquals(_ChildrenList[I].Name, Name)) Then
  Begin
    Result := _ChildrenList[I];
    Exit;
  End;

  Result := Nil;
End;

Function UIWidget.GetChildByClass(ChildClass:UIWidgetClass; Index:Integer):UIWidget;
Var
  I, Count:Integer;
Begin
  Count := -1;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].ClassType = ChildClass) Then
  Begin
    Inc(Count);
    If (Count = Index) Then
    Begin
      Result := _ChildrenList[I];
      Exit;
    End;
  End;

  Result := Nil;
End;

Procedure UIWidget.AddChild(W:UIWidget);
Begin
  If (W=Nil) Or (W = Self) Then
    Exit;

  W._Parent := Self;
  Inc(_ChildrenCount);
  SetLength(_ChildrenList, _ChildrenCount);
  _ChildrenList[Pred(_ChildrenCount)] := W;
End;

Procedure UIWidget.RemoveAllChildren();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
    ReleaseObject(_ChildrenList[I]);

  _ChildrenCount := 0;
End;

Procedure UIWidget.RemoveChild(W:UIWidget);
Var
  I:Integer;
Begin
  If (W = Nil) Then
    Exit;

  I := 0;
  While (I<_ChildrenCount) Do
  If (_ChildrenList[I] = W) Then
  Begin
    _ChildrenList[I]._Parent := Nil;
    _ChildrenList[I] := _ChildrenList[Pred(_ChildrenCount)];
    Dec(_ChildrenCount);
    Exit;
  End Else
    Inc(I);
End;


Procedure UIWidget.Render;
Var
  I:Integer;
Begin
  If (Not Self.Visible) Or (Self._Deleted) Then
    Exit;

(*  If (Assigned(_SkinComponent)) And (_SkinComponent.Name <> Self._Skin.Value) Then
    Self.LoadSkin(Self._Skin.Value);*)

  Self.UpdateProperties();
  Self.UpdateRects();
  Self.UpdateTransform();
//  Self.UpdateHighlight();
  Self.UpdateSprite();

  If Assigned(_Sprite) Then
    SpriteManager.Instance.QueueSprite(_Sprite);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) And (_ChildrenList[I].CanRender()) Then
    _ChildrenList[I].Render();
End;


Procedure UIWidget.OnHighlight(Prev:UIWidget);
Begin
  // do nothing
End;

Function UIWidget.IsSelected: Boolean;
Begin
  (* TODO Result := (Self = UI.Highlight) Or (Self = UI.Focus);*)
End;

Function UIWidget.OnSelectAction: Boolean;
Begin
  If (Self.Selected) And (Assigned(OnMouseClick)) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.OnHit(OnMouseClick);
    Result := True;
  End Else
    Result := False;
End;

Function UIWidget.OnSelectDown():Boolean;
Var
  W:UIWidget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetDownControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      (*TODO UI.Highlight := W; *)
    End;
  End;
End;

Function UIWidget.OnSelectLeft():Boolean;
Var
  W:UIWidget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetLeftControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      (* TODO UI.Highlight := W;*)
    End;
  End;
End;

Function UIWidget.OnSelectRight():Boolean;
Var
  W:UIWidget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetRightControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      (* TODO UI.Highlight := W;*)
    End;
  End;
End;

Function UIWidget.OnSelectUp():Boolean;
Var
  W:UIWidget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetUpControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      (* TODO UI.Highlight := W;*)
    End;
  End;
End;

Function UIWidget.GetClipRect:ClipRect;
Begin
  Result := Self._ClipRect;

  If (Assigned(_Parent)) Then
    Result.Merge(_Parent.GetClipRect())
(* TODO  Else
    Result.Merge(_UI._ClipRect)*);
End;

Procedure UIWidget.SetClipRect(Value:ClipRect);
Begin
  Self._ClipRect := Value;
End;

Procedure UIWidget.GetScrollOffset(Out OfsX, OfsY: Single);
Var
  TX, TY:Single;
//  Bar:UIScrollBar;
Begin
  OfsX := 0;
  OfsY := 0;
(* TODO
  If (Assigned(Scroll)) And (Scroll Is UIScrollBar) Then
  Begin
    Bar := UIScrollBar(Scroll);

    If (_ScrollValue<>Bar.Value) Then
    Begin
      Self._TransformChanged := True;
      _ScrollValue := Bar.Value;
    End;

    If (Bar.Kind = scrollHorizontal) Then
      OfsX := -Bar.Value
    Else
      OfsY := -Bar.Value;
  End;

  If Assigned(Parent) Then
  Begin
    Parent.GetScrollOffset(TX, TY);
    OfsX := OfsX + TX;
    OfsY := OfsY + TY;
  End;*)
End;

Procedure UIWidget.SetPositionRelativeToOther(Other:UIWidget; OfsX, OfsY: Single);
Var
  P:Vector2D;
Begin
  P := Other.AbsolutePosition;

  If (Other.Parent<>Nil) Then
    P.Subtract(Other.Parent.AbsolutePosition);

  P.Add(VectorCreate2D(OfsX, OfsY));

  Self.SetRelativePosition(P);
End;

Procedure UIWidget.SetObjectName(const Value:TERRAString);
Var
  Existed:Boolean;
Begin
(* TODO
  Existed := _UI._Widgets.Remove(Self);
  Self._ObjectName := Value;

  If Existed Then
    _UI._Widgets.Add(Self);*)
End;

Function UIWidget.OutsideClipRect(X, Y: Single): Boolean;
Var
  X1, Y1, X2, Y2:Single;
Begin
  If (_ClipRect.Style = clipNothing) Then
  Begin
    Result := False;
    Exit;
  End;

  If (_ClipRect.Style = clipEverything) Then
  Begin
    Result := True;
    Exit;
  End;

  _ClipRect.GetRealRect(X1, Y1, X2, Y2{, IsLandscapeOrientation(Application.Instance.Orientation)});

  Result := (X<X1) Or (Y<Y1) Or (X>=X2) Or (Y>=Y2);
End;

Procedure UIWidget.SetEnabled(Value: Boolean);
Begin
  Self._Enabled := Value;
End;

Procedure UIWidget.UpdateClipRect(Clip: ClipRect; LeftBorder,TopBorder, RightBorder, BottomBorder:Single);
Var
  Pos, Size:Vector2D;
Begin
  Pos := Self.AbsolutePosition;
  Size := Self.Size;
  Clip.X := Pos.X + LeftBorder;
  Clip.Y := Pos.Y + TopBorder;
  Clip.Width := Size.X - (RightBorder + LeftBorder);
  Clip.Height := Size.Y - (TopBorder + BottomBorder);
End;

Function UIWidget.GetIndex: Integer;
Var
  S:TERRAString;
Begin
  S := Self.Name;
  StringGetNextSplit(S, Ord('_'));
  Result := StringToInt(S);
End;

Function UIWidget.GetTabControl():UIWidget;
Begin
  If Assigned(_TabControl) Then
    Result := _TabControl
  Else
  If (Assigned(_Parent)) Then
    Result := _Parent.GetTabControl()
  Else
    Result := Nil;
End;

Function UIWidget.GetDataValue: TERRAString;
Var
  S:TERRAString;
Begin
  S := _DataSource.Value;

  If Assigned(Self.Parent) Then
    StringReplaceText('$', Self.Parent._DataSource.Value, S);

  Result := DataSourceManager.Instance.GetValueFromPath(S);
End;

Procedure UIWidget.SetChildrenVisibilityByTag(Tag: Integer; Visibility: Boolean);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (Assigned(_ChildrenList[I])) And (_ChildrenList[I].Tag = Tag) Then
    _ChildrenList[I].Visible := Visibility;
End;

Function UIWidget.CanRender: Boolean;
Var
  CurrentFrameID:Cardinal;
Begin
  CurrentFrameID := GraphicsManager.Instance.FrameID;
  If (_RenderFrameID = CurrentFrameID) Then
  Begin
    Result := False;
    Exit;
  End;

  _RenderFrameID := CurrentFrameID;
  Result := True;
End;

Procedure UIWidget.ResetClipRect;
Begin
  _ClipRect.Style := clipSomething;
  _ClipRect.X := Self.AbsolutePosition.X;
  _ClipRect.Y := Self.AbsolutePosition.Y;
  _ClipRect.Width := Self.Size.X;
  _ClipRect.Height := Self.Size.Y;
  (* TODO _ClipRect.Transform(_UI.Transform);*)
End;

Function UIWidget.OnCustomRegion(X, Y:Integer; X1, Y1, X2, Y2:Single): Boolean;
Var
  I:Integer;
  Pos:Vector2D;
  P:Array[0..3] Of Vector2D;
Begin
  Pos := Self.GetAbsolutePosition();
  P[0] := VectorCreate2D(X1, Y1);
  P[1] := VectorCreate2D(X2, Y1);
  P[2] := VectorCreate2D(X2, Y2);
  P[3] := VectorCreate2D(X1, Y2);

  For I:=0 To 3 Do
  Begin
    P[I].Add(Pos);
    P[I] := _Transform.Transform(P[I]);
  End;

  Result := (X>= P[0].X) And (X <= P[2].X) And (Y >= P[0].Y) And (Y <= P[2].Y);
End;

Function UIWidget.GetFontRenderer: FontRenderer;
Begin
  Result := UIManager.Instance.FontRenderer;
End;

Function UIWidget.IsOutsideScreen: Boolean;
Var
  P:Vector2D;
Begin
  P := Self.AbsolutePosition;
  Result := (P.X + Self.Size.X<0) Or (P.Y + Self.Size.Y<0) Or (P.X> UIManager.Instance.Width) Or (P.Y > UIManager.Instance.Height);
End;

Function UIWidget.GetDimension(Const Dim: UIDimension; Const Target:UIDimensionTarget): Single;
Begin
  If Dim.IsPercent Then
  Begin
    If Assigned(Parent) Then
    Begin
      If (Target = uiDimensionWidth) Then
        Result := (Dim.Value * 0.01) * Parent.Size.X
      Else
        Result := (Dim.Value * 0.01) * Parent.Size.Y
    End Else
    Begin
      If (Target = uiDimensionWidth) Then
        Result := (Dim.Value * 0.01) * UIManager.Instance.Width
      Else
        Result := (Dim.Value * 0.01) * UIManager.Instance.Height
    End;
  End Else
    Result := Dim.Value;
End;

Procedure UIWidget.ClipChildren(const Clip: ClipRect);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If Assigned(_ChildrenList[I].Scroll) Then
    _ChildrenList[I].SetClipRect(Clip);
End;

Function UIWidget.GetWidth: UIDimension;
Begin
  Result := _Width.Value;
End;

Function UIWidget.GetHeight: UIDimension;
Begin
  Result := _Height.Value;
End;

Procedure UIWidget.SetWidth(const Value: UIDimension);
Begin
  Self._Width.Value := Value;
  Self.UpdateRects();
End;

Procedure UIWidget.SetHeight(const Value: UIDimension);
Begin
  Self._Height.Value := Value;
  Self.UpdateRects();
End;


Function UIWidget.GetRotation: Single;
Begin
  Result := _Rotation.Value;
End;

Function UIWidget.GetScale: Single;
Begin
  Result := _Scale.Value;
End;

Procedure UIWidget.NullEventHandler(Src:UIWidget);
Begin
  // do nothing
End;

Function UIWidget.AdjustWidth(NewWidth: Single):Single;
Var
  Original, P:Single;
Begin
  Original := NewWidth;
  NewWidth := UISnap(NewWidth);

  If (NewWidth<UISnapSize) Then
      NewWidth := UISnapSize;

  Result := Original - NewWidth;

  If _Width.Value.IsPercent Then
  Begin
    P := (NewWidth / UIManager.Instance.Width) * 100;
    _Width.Value := UIPercent(P);
  End Else
    _Width.Value := UIPixels(NewWidth);
End;

Function UIWidget.AdjustHeight(NewHeight: Single):Single;
Var
  Original, P:Single;
Begin
  Original := NewHeight;
  NewHeight := UISnap(NewHeight);

  If (NewHeight<UISnapSize) Then
      NewHeight := UISnapSize;

  Result := Original - NewHeight;

  If _Height.Value.IsPercent Then
  Begin
    P := (NewHeight / UIManager.Instance.Height) * 100;
    _Height.Value := UIPercent(P);
  End Else
    _Height.Value := UIPixels(NewHeight);
End;

Function UIWidget.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := True;
End;

Procedure UIWidget.Delete();
Var
  I:Integer;
Begin
  Self._Deleted := True;

  For I:=0 To Pred(_ChildrenCount) Do
    _ChildrenList[I].Delete();

  _ChildrenCount := 0;

  If Assigned(_Parent) Then
    _Parent.RemoveChild(Self);
End;


Function UIWidget.GetAlign: Integer;
Begin
  Result := _Align.Value;
End;

Procedure UIWidget.SetAlign(const Value: Integer);
Begin
  _Align.Value := Value;
End;

Function UIWidget.GetTabIndex: Integer;
Begin
  Result := _TabIndex.Value;
End;

Procedure UIWidget.SetTabIndex(const Value: Integer);
Begin
  _TabIndex.Value  := Value;
End;

Function UIWidget.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
(*  If (StringEquals(ObjectType, 'UIButton')) Then
    Result := UIButton.Create(KeyName, Self, 0, 0, 50, UIPixels(10), UIPixels(10), '', 'button')
  Else
  If (StringEquals(ObjectType, 'UILabel')) Then
    Result := UILabel.Create(KeyName, Self, 0, 0, 50, '???')
  Else
  If (StringEquals(ObjectType, 'UIEditText')) Then
    Result := UIEditText.Create(KeyName, Self, 0, 0, 50, UIPixels(10), UIPixels(10), '???')
  Else
  If (StringEquals(ObjectType, 'UISprite')) Then
    Result := UISprite.Create(KeyName, Self, 0, 0, 50, 'sprite')
  Else
  If (StringEquals(ObjectType, 'UIIcon')) Then
    Result := UIIcon.Create(KeyName, Self, 0, 0, 50, UIPixels(10), UIPixels(10), 'icon')
  Else
  If (StringEquals(ObjectType, 'UIWindow')) Then
    Result := UIWindow.Create(KeyName, Self, 0, 0, 50, UIPixels(10), UIPixels(10), 'window')
  Else
  If (StringEquals(ObjectType, 'UICheckbox')) Then
    Result := UICheckbox.Create(KeyName, Self, 0, 0, 50, UIPixels(10), True, '???', 'checkbox')
  Else
  If (StringEquals(ObjectType, 'UITabList')) Then
    Result := UITabList.Create(KeyName, Self, 0, 0, 50, UIPixels(10), UIPixels(10), 'icon')
  Else*)
    Result := Nil;
End;

Procedure UIWidget.SetParent(Target:UIWidget);
Var
  P:Vector2D;
Begin
  P := Self.AbsolutePosition;
  If Assigned(Target) Then
    P.Subtract(Target.AbsolutePosition);

  Self.RelativePosition := P;

  If Assigned(Self.Parent) Then
  Begin
    Self.Parent.RemoveChild(Self);
  End;

  If Assigned(Target) Then
    Target.AddChild(Self)
(*  Else
    _UI.InsertIntoTopWidgets(Self)*);
End;

Procedure UIWidget.UpdateLanguage();
Var
  MyWidget:UIWidget;
  I:Integer;
Begin
  Self.OnLanguageChange();

  For I:=0 To Pred(_ChildrenCount) Do
    _ChildrenList[I].UpdateLanguage();
End;


Procedure UIWidget.UpdateSprite;
Begin
  // do nothing
End;

(*
  UIComponentImage = Class(UIComponent)
    Protected
      _Image:TextureProperty;
      _U1, _V1, _U2, _V2:Single;

      Procedure UpdateSprite; Override;

    Public
  End;

  UIComponentTiledGrid = Class(UIComponent)
    Protected
      _Image:TERRATexture;

  //    Procedure UpdateSprite; Override;

    Public
  End;
*)

{ UIComplexWidget }
Constructor UIInstancedWidget.Create(Const Name: TERRAString; Parent: UIWidget; X, Y, Z: Single; const Width, Height: UIDimension; Const TemplateName:TERRAString);
Var
  Template:UIWidget;
Begin
  Inherited Create(Name, Parent);

  _TemplateName := StringProperty.Create('template', TemplateName);

  Self.ExpandProperties(1);
  _TemplateIndex := _BasePropertiesIndex;

  Template := UIWidget.GetTemplate(TemplateName);
  Self.AddChild(Self.InitFromTemplate(Template));

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self.Width := Width;
  Self.Height := Height;
End;

Procedure UIInstancedWidget.Release;
Begin
  Inherited;

  ReleaseObject(_TemplateName);
End;

Function UIInstancedWidget.GetObjectType: TERRAString;
Begin
  Result := 'UITemplate';
End;

Function UIInstancedWidget.InitFromTemplate(Template:UIWidget):UIWidget;
Var
  I:Integer;
Begin
  If Template = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Template Is UIInstancedWidget) Then
  Begin
  End Else
  If (Template Is UITiledRect) Then
  Begin
    Result := UITiledRect.Create(Template.Name, Nil, 0, 0, 0, UIPixels(100), UIPixels(100), 0, 0, 1, 1);
  End Else
  Begin
    Result := UIWidget(Template.ClassType.Create());
    Result.Create(Template.Name, Nil);
  End;

  Result.CopyProperties(Template);

  For I:=0 To Pred(Template.ChildrenCount) Do
  Begin
    Result.AddChild(InitFromTemplate(Template.GetChildByIndex(I)));
  End;
End;


End.
