Unit TERRA_UIWidget;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_Collections,
  TERRA_Vector2D, TERRA_Color, TERRA_Matrix3x3, TERRA_Texture,
  TERRA_ClipRect, TERRA_Tween, TERRA_FontRenderer, TERRA_Sprite,
  TERRA_UIDimension, TERRA_EnumProperty, TERRA_DataSource, TERRA_Viewport;

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

  UIMacroBeginChar  = Ord('{');
  UIMacroEndChar    = Ord('}');
  UITranslationChar = Ord('#');
  UIPropertyChar    = Ord('!');
  UIDataSourceChar  = Ord('@');

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

  WidgetEventType = (
    widgetEvent_MouseDown,
    widgetEvent_MouseUp,
    widgetEvent_MouseOver,
    widgetEvent_MouseOut,
    widgetEvent_DragBegin,
    widgetEvent_DragEnd
  );

  UIWidget = Class;
  UIWidgetClass = Class Of UIWidget;
  WidgetEventHandler = Procedure(Src:UIWidget) Of Object;

  UIProperty = Record
    Prop:TERRAObject;
    Link:TERRAString;
    Custom:Boolean;
  End;

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
      _Draggable:BooleanProperty;

      _Deleted:Boolean;

			Function GetAbsolutePosition:Vector2D;
      Function GetRelativePosition:Vector2D;
      Function GetAlignedPosition:Vector2D;

      Function GetHeight: UIDimension;
      Function GetWidth: UIDimension;

      Procedure SetParent(Target:UIWidget);

      Function GetDraggable: Boolean;
      Procedure SetDraggable(const Value: Boolean);
      Function GetUIView: UIWidget;

		Protected
			_Parent:UIWidget;

      _ChildrenList:Array Of UIWidget;
      _ChildrenCount:Integer;

      _Scroll:UIWidget;
      _ScrollValue:Single;

      _Properties:Array Of UIProperty;
      _PropertyCount:Integer;

      _EventHandlers:Array [WidgetEventType] Of WidgetEventHandler;

      _Tooltip:TERRAString;
      _NeedsUpdate:Boolean;
      _NeedsHide:Boolean;

      _State:WidgetState;

      _MouseOver:Boolean;

      _Enabled:Boolean;

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

      Function AddProperty(Prop:TERRAObject; IsCustom:Boolean):TERRAObject;

      Procedure UpdateSprite(View:TERRAViewport); Virtual;

      Procedure InitProperties();

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

      Procedure SetTransform(const Value: Matrix3x3);

      Function GetRotation():Single;
      Function GetScale():Single;

      Function ResolveMacro(Const Value:TERRAString):TERRAString;

      Procedure UpdateProperties();

      Function IsSelected: Boolean;

      Function AdjustWidth(NewWidth:Single):Single;
      Function AdjustHeight(NewHeight:Single):Single;

      //Function HasMouseOver():Boolean; Virtual;

      Function GetFontRenderer: FontRenderer;

      Function OutsideClipRect(X,Y:Single):Boolean;

      Procedure ResetClipRect();

      Procedure SetEnabled(Value:Boolean);

      Procedure UpdateLanguage; 

      Procedure SetObjectName(const Value: TERRAString); Override;

      Procedure ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);

      Property FontRenderer:FontRenderer Read GetFontRenderer;

		Public
      Tag:Integer;
      DisableHighlights:Boolean;
      DisableUIColor:Boolean;
      UserData:TERRAString;


      Constructor Create(Const Name:TERRAString; Parent:UIWidget);
      Procedure Release; Override;

      Procedure StartHighlight(); Virtual;
      Procedure StopHighlight(); Virtual;

      Function IsSelectable():Boolean; Virtual;
      Function CanHighlight(GroupID:Integer):Boolean;

      Procedure OnHit(EventType:WidgetEventType); Virtual;
      Procedure OnHighlight(Prev:UIWidget); Virtual;

      Procedure Delete();

      Procedure SetEventHandler(EventType:WidgetEventType; Handler:WidgetEventHandler);
      Function GetEventHandler(EventType:WidgetEventType):WidgetEventHandler;
      Function CallEventHandler(EventType:WidgetEventType):Boolean;

      Function CanRender():Boolean;
      Function AllowsEvents(): Boolean;

      Procedure PickAt(Const X, Y:Integer; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget = Nil);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Procedure Render(View:TERRAViewport); Virtual;

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

			Procedure OnMouseDown(X,Y:Integer; Button:Word); Virtual;
			Procedure OnMouseUp(X,Y:Integer; Button:Word); Virtual;
			Procedure OnMouseMove(X,Y:Integer); Virtual;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer);Virtual;

      Function GetIndex():Integer;

      Function IsSameFamily(Other:UIWidget):Boolean;

      Function GetSize:Vector2D; Virtual;

      Function GetClipRect():ClipRect;

      Procedure BeginDrag(X,Y:Integer; Mode:UIDragMode);
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

      Property Draggable:Boolean Read GetDraggable Write SetDraggable;

      Property Deleted:Boolean Read _Deleted;
      Property TransformChanged:Boolean Read _TransformChanged Write _TransformChanged;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;

      Property View:UIWidget Read GetUIView;
	End;

  UIInstancedWidget = Class(UIWidget)
    Protected
      _TemplateName:StringProperty;

      Function InitFromTemplate(Template, Parent:UIWidget):UIWidget;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const TemplateName:TERRAString);
      Procedure Release(); Override;

      Function GetObjectType:TERRAString; Override;
  End;

  UITemplateList = Class(TERRAObject)
    Protected
      _Templates:List;

    Public
      Function AddTemplate(Template:UIWidget):UIWidget;
      Function GetTemplate(Const TemplateName:TERRAString):UIWidget;
      Function DeleteTemplate(Const TemplateName:TERRAString):Boolean;
  End;

  Function UITemplates():UITemplateList;

  Function UITranslationMacro(Const Value:TERRAString):TERRAString;
  Function UIPropertyMacro(Const Value:TERRAString):TERRAString;
  Function UIDataSourceMacro(Const Value:TERRAString):TERRAString;

Implementation

Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Math, TERRA_GraphicsManager, TERRA_UIView, TERRA_UITiledRect, TERRA_UIImage, TERRA_UILabel,
  TERRA_Localization;

Function UITranslationMacro(Const Value:TERRAString):TERRAString;
Begin
  Result := StringFromChar(UIMacroBeginChar) + StringFromChar(UITranslationChar) + Value + StringFromChar(UIMacroEndChar);
End;

Function UIPropertyMacro(Const Value:TERRAString):TERRAString;
Begin
  Result := StringFromChar(UIMacroBeginChar) + StringFromChar(UIPropertyChar) + Value + StringFromChar(UIMacroEndChar);
End;

Function UIDataSourceMacro(Const Value:TERRAString):TERRAString;
Begin
  Result := StringFromChar(UIMacroBeginChar) + StringFromChar(UIDataSourceChar) + Value + StringFromChar(UIMacroEndChar);
End;

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
End;

{ UIWidget }
Constructor UIWidget.Create(Const Name:TERRAString; Parent:UIWidget);
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
Var
  I:Integer;
Begin
  For I:=0 To Pred(_PropertyCount) Do
    ReleaseObject(Self._Properties[I].Prop);

  ReleaseObject(_Sprite);
End;

Procedure UIWidget.InitProperties;
Begin
  _Width := DimensionProperty(Self.AddProperty(DimensionProperty.Create('width', UIPixels(0)), False));
  _Height := DimensionProperty(Self.AddProperty(DimensionProperty.Create('height', UIPixels(0)), False));
  _Visible := BooleanProperty(Self.AddProperty(BooleanProperty.Create('visible', True), False));
  _Position := Vector2DProperty(Self.AddProperty(Vector2DProperty.Create('position', VectorCreate2D(0, 0)), False));
  _Layer := FloatProperty(Self.AddProperty(FloatProperty.Create('layer', 1.0), False));
  _Color := ColorProperty(Self.AddProperty(ColorProperty.Create('color', ColorWhite), False));
  _Rotation := AngleProperty(Self.AddProperty(AngleProperty.Create('rotation', 0.0), False));
  _Scale := FloatProperty(Self.AddProperty(FloatProperty.Create('scale', 1.0), False));
  _Saturation := FloatProperty(Self.AddProperty(FloatProperty.Create('saturation', 1.0), False));
  _Skin := StringProperty(Self.AddProperty(StringProperty.Create('skin', ''), False));
  _Draggable := BooleanProperty(Self.AddProperty(BooleanProperty.Create('draggable', False), False));
  _Align := EnumProperty(Self.AddProperty(EnumProperty.Create('align', 0, UIManager.Instance.AlignEnums), False));
End;

Function UIWidget.GetPropertyByIndex(Index:Integer):TERRAObject;
Begin
  If (Index>=_PropertyCount) Then
  Begin
    Dec(Index, _PropertyCount);
    If (Index<_ChildrenCount) Then
      Result := _ChildrenList[Index]
    Else
      Result := Nil;
    Exit;
  End;

  If (Index>=0) Then
    Result := _Properties[Index].Prop
  Else
    Result := Nil;
End;

Procedure UIWidget.CenterOnPoint(X,Y:Single);
Begin
  Self.Align := waTopLeft;
  Self.UpdateRects;

  _Position.X.Value := X - _Size.X * 0.5;
  _Position.Y.Value := Y - _Size.Y * 0.5;
End;

Procedure UIWidget.StartHighlight;
Begin
  Self._Color.Value := ColorBlack;
End;

Procedure UIWidget.StopHighlight;
Begin
  Self._Color.Value := ColorWhite;
End;

Procedure UIWidget.OnHit(EventType:WidgetEventType);
Var
  N:Integer;
  Target:TERRA_Color.Color;
  Ease:TweenEaseType;
Begin
  Target := ColorScale(Self.Color, 0.5);
  N := 100;

  Ease := easeLinear;

  Self._Color.AddTween(Ease, Target, N, 0);
  Self._Color.AddTween(Ease, Self.Color, N, N, TweenCallback(Self.GetEventHandler(EventType)), Self);
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
  Result := Assigned(_EventHandlers[widgetEvent_MouseDown]);
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

  If (Not Value) Then
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

Function UIWidget.GetAlignedPosition:Vector2D;
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
End;

Function UIWidget.GetAbsolutePosition:Vector2D;
Begin
  Result := Self.GetAlignedPosition();

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
Var
  UI:UIView;
Begin
  If (Not Visible) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;

  UI := UIView(Self.GetUIView);

  If (Assigned(UI)) And ((UI.Modal = Nil) Or  (Self = UI.Modal)) Then
    Exit;

  If Assigned(Self.Parent) Then
    Result := Self.Parent.AllowsEvents();
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
Var
  UI:UIView;
Begin
  UI := UIView(Self.GetUIView);
  If UI = Nil Then
    Exit;

  Self.CallEventHandler(widgetEvent_DragBegin);

  UI.Dragger := Self;
  _DragMode := Mode;
  _Dragging := True;

  _DragStart := _Position.Value;
  _DragSize := _Size;

  _DragX := (X-_Position.X.Value);
  _DragY := (Y-_Position.Y.Value);
End;

Procedure UIWidget.CancelDrag;
Var
  UI:UIView;
Begin
  UI := UIView(Self.GetUIView);
  If UI = Nil Then
    Exit;

  If _Dragging Then
  Begin
    _Position.Value := _DragStart;
    _Dragging := False;

    If UI.Dragger = Self Then
      UI.Dragger := Nil;

    Self._TransformChanged := True;
  End;
End;

Procedure UIWidget.FinishDrag();
Var
  UI:UIView;
Begin
  UI := UIView(Self.GetUIView);
  If UI = Nil Then
    Exit;

  If (UI.Dragger <> Self) Then
    Exit;

  Self.CallEventHandler(widgetEvent_DragEnd);

  _Dragging := False;
  UI.Dragger := Nil;
End;

Procedure UIWidget.OnMouseDown(X,Y:Integer;Button:Word);
Var
  TargetDragger:UIWidget;
Begin
  If (Draggable) Then
  Begin
    If (Not _Dragging) Then
    Begin
      If (Assigned(Parent)) And (Parent Is UIInstancedWidget) Then
        TargetDragger := Parent
      Else
        TargetDragger := Self;

      TargetDragger.BeginDrag(X,Y, UIDrag_Move);
    End;

    Exit;
  End;

  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Found, and has handler: '+BoolToString(Assigned(OnMouseClick)));{$ENDIF}
  Self.OnHit(widgetEvent_MouseDown);
End;

Procedure UIWidget.OnMouseUp(X,Y:Integer;Button:Word);
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.OnMouseUp called');{$ENDIF}

  Self.CallEventHandler(widgetEvent_MouseUp);
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

(*Function UIWidget.HasMouseOver: Boolean;
Begin
  Result := (Assigned(OnMouseOver)) Or (Assigned(OnMouseOut)) Or (Assigned(OnMouseClick));
End;*)

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

(*  If (Not _TransformChanged) Then
    Exit;*)

  _TransformChanged := False;

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

  _Transform := MatrixMultiply3x3(_Transform, MatrixTranslation2D(Self.GetAlignedPosition()));

  If Assigned(_Parent) Then
    _Transform := MatrixMultiply3x3(_Transform, Parent._Transform);

  Self.GetScrollOffset(OfsX, OfsY);

  _InverseTransform := MatrixInverse2D(_Transform);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) Then
  Begin
    _ChildrenList[I]._TransformChanged := True;
    _ChildrenList[I].UpdateTransform();
  End;

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


Procedure UIWidget.Render(View:TERRAViewport);
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
  Self.UpdateSprite(View);

  If Assigned(_Sprite) Then
    View.SpriteRenderer.QueueSprite(_Sprite);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) And (_ChildrenList[I].CanRender()) Then
    _ChildrenList[I].Render(View);
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
  If (Self.Selected) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.OnHit(widgetEvent_MouseDown);
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

Function UIWidget.ResolveMacro(Const Value:TERRAString):TERRAString;
Var
  I:Integer;
  Macro:TERRAString;
  C:TERRAChar;
Begin
  C := StringFirstChar(Value);

  Result := '';

  If (C = UITranslationChar) Then
  Begin
    Macro := StringCopy(Value, 2, MaxInt);
    Result := LocalizationManager.Instance.GetString(Macro);
    Exit;
  End Else
  If (C = UIPropertyChar) Then
  Begin
    Macro := StringCopy(Value, 2, MaxInt);
    For I:=0 To Pred(_PropertyCount) Do
    If (StringEquals(_Properties[I].Prop.Name, Macro)) Then
    Begin
      Result := _Properties[I].Prop.GetBlob();
      Exit;
    End;
  End Else
  If (C = UIDataSourceChar) Then
  Begin
    Result := 'LOFB';
(*    For I:=0 To Pred(_PropertyCount) Do
    If (StringEquals(_Properties[I].Prop.Name, Macro)) Then
    Begin
      Result := _Properties[I].Prop.GetBlob();
      Exit;
    End;*)
    Exit;                       
  End Else
  Begin
    Result := Value;
    Exit;
  End;

  If Assigned(Self.Parent) Then
    Result := Self.Parent.ResolveMacro(Value);
    
  //Result := DataSourceManager.Instance.GetValueFromPath(S);
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


Function UIWidget.AddProperty(Prop:TERRAObject; IsCustom:Boolean):TERRAObject;
Begin
  Result := Prop;
  Inc(_PropertyCount);
  SetLength(_Properties, _PropertyCount);
  _Properties[Pred(_PropertyCount)].Prop := Prop;
  _Properties[Pred(_PropertyCount)].Custom := IsCustom;
  _Properties[Pred(_PropertyCount)].Link := '';
End;

Function UIWidget.GetDraggable: Boolean;
Begin
  Result := _Draggable.Value;
End;

Procedure UIWidget.SetDraggable(const Value: Boolean);
Begin
  _Draggable.Value := Value;
End;

Function UIWidget.GetUIView: UIWidget;
Begin
  If Assigned(_Parent) Then
    Result := _Parent.GetUIView()
  Else
  If (Self Is UIView) Then
    Result := Self
  Else
    Result := Nil;
End;

Procedure UIWidget.SetTransform(const Value: Matrix3x3);
Begin
  _Transform := Value;
  _InverseTransform := MatrixInverse2D(Value);

  //_ClipRect.Style := clipSomething;
  _ClipRect.Style := clipNothing;
  _ClipRect.X := 0;
  _ClipRect.Y := 0;
  _ClipRect.Width := Self.Size.X;
  _ClipRect.Height := Self.Size.Y;
  _ClipRect.Transform(Value);

  _TransformChanged := True;
End;

Function UIWidget.GetEventHandler(EventType:WidgetEventType):WidgetEventHandler;
Begin
  Result := Self._EventHandlers[EventType];

  If (Assigned(Result)) Or (_Parent = Nil) Then
    Exit;

  Result := _Parent.GetEventHandler(EventType);
End;

Function UIWidget.CallEventHandler(EventType:WidgetEventType):Boolean;
Var
  Handler:WidgetEventHandler;
Begin
  Handler := Self.GetEventHandler(EventType);
  If Assigned(Handler) Then
    Handler(Self);
End;

Procedure UIWidget.SetEventHandler(EventType:WidgetEventType; Handler:WidgetEventHandler);
Begin
  Self._EventHandlers[EventType] := Handler;
End;

{ UIInstancedWidget }
Constructor UIInstancedWidget.Create(Const Name: TERRAString; Parent: UIWidget; X, Y, Z: Single; const Width, Height: UIDimension; Const TemplateName:TERRAString);
Var
  Template:UIWidget;
  Prop:TERRAObject;
  I:Integer;
Begin
  Inherited Create(Name, Parent);

  _TemplateName := StringProperty(Self.AddProperty(StringProperty.Create('template', TemplateName), False));

  Template := UITemplates.GetTemplate(TemplateName);
  If Assigned(Template) Then
  Begin
    For I:=0 To Pred(Template.ChildrenCount) Do
      Self.InitFromTemplate(Template.GetChildByIndex(I), Self);

    For I:=0 To Pred(Template._PropertyCount) Do
    If (Template._Properties[I].Custom) Then
    Begin
      Prop := TERRAObject(Template._Properties[I].Prop.ClassType.Create);
      Prop.Name := Template._Properties[I].Prop.Name;
      Prop.SetBlob(Template._Properties[I].Prop.GetBlob());
      Self.AddProperty(Prop, True);
    End;
  End;

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

Function UIInstancedWidget.InitFromTemplate(Template, Parent:UIWidget):UIWidget;
Var
  I:Integer;
Begin
  If Template = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Template Is UILabel) Then
  Begin
    Result := UILabel.Create(Template.Name, Parent, 0, 0, 0, UIPixels(100), UIPixels(100), 'basaf');
  End Else
  If (Template Is UITiledRect) Then
  Begin
    Result := UITiledRect.Create(Template.Name, Parent, 0, 0, 0, UIPixels(100), UIPixels(100), 0, 0, 1, 1);
  End Else
  Begin
    Log(logError, 'UI', 'Cannot instanciate template component of type '+Template.ClassName);
    Exit;
  End;

  Result.CopyProperties(Template);

  For I:=0 To Pred(Template.ChildrenCount) Do
  Begin
    InitFromTemplate(Template.GetChildByIndex(I), Result);
  End;
End;

{ UITemplateList }
Var
  _UITemplates:UITemplateList;

Function UITemplates():UITemplateList;
Begin
  If _UITemplates = Nil Then
    _UITemplates := UITemplateList.Create();

  Result := _UITemplates;
End;

Function UITemplateList.AddTemplate(Template:UIWidget): UIWidget;
Begin
  Result := Template;

  If Result = Nil Then
    Exit;

  If _Templates = Nil Then
    _Templates := List.Create();

  _Templates.Add(Template);
End;

Function UITemplateList.GetTemplate(Const TemplateName: TERRAString): UIWidget;
Var
  W:UIWidget;
  It:Iterator;
Begin
  Result := Nil;
  If _Templates = Nil Then
    Exit;

  It := _Templates.GetIterator();
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


Function UITemplateList.DeleteTemplate(Const TemplateName: TERRAString): Boolean;
Var
  W:UIWidget;
  It:Iterator;
Begin
  Result := False;
  If _Templates = Nil Then
    Exit;

  It := _Templates.GetIterator();
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

End.
