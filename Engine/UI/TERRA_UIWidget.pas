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
    widget_Hidden,
    widget_Selected,
    widget_Highlighted,
    widget_Dragged,
    widget_Disabled
  );

  WidgetEventType = (
    widgetEvent_Show,
    widgetEvent_Hide,
    widgetEvent_MouseDown,
    widgetEvent_MouseUp,
    widgetEvent_MouseOver,
    widgetEvent_MouseOut,
    widgetEvent_DragBegin,
    widgetEvent_DragEnd,
    widgetEvent_FocusBegin,
    widgetEvent_FocusEnd,
    widgetEvent_ContentChange
  );

  UIProperty = Record
    Prop:TERRAObject;
    Link:TERRAString;
    Custom:Boolean;
  End;

  UIWidgetStateAnimation = Record
    State:WidgetState;
    Ease:TweenEaseType;
    PropName:TERRAString;
    Value:TERRAString;
  End;

  UIWidget = Class;
  UIWidgetClass = Class Of UIWidget;

  UIWidgetEventHandler = Procedure(Src:UIWidget) Of Object;

  UIController = Class(TERRAObject)
    Private
      _Handlers:Array[WidgetEventType] Of UIWidgetEventHandler;

    Public
      Function GetHandler(EventType:WidgetEventType):UIWidgetEventHandler;
      Procedure SetHandler(EventType:WidgetEventType; Handler:UIWidgetEventHandler);
  End;

  UIControllerProperty = Class(TERRAObject)
    Protected
      _Value:UIController;

    Public
      Constructor Create(Const Name:TERRAString; Controller:UIController);

      Function IsValueObject():Boolean; Override;

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;
  End;

	UIWidget = Class(CollectionObject)
    Private
      _Tested:Boolean;
      _RenderFrameID:Cardinal;

      _Width:DimensionProperty;
      _Height:DimensionProperty;
			_Position:Vector2DProperty;
      _Pivot:Vector2DProperty;
      _Layer:FloatProperty;
      _Align:EnumProperty;
      _Color:ColorProperty;
      _Rotation:AngleProperty;
      _Scale:FloatProperty;
      _Saturation:FloatProperty;
			_Visible:BooleanProperty;
      _Draggable:BooleanProperty;
      _Controller:UIControllerProperty;

      _Deleted:Boolean;

      _State:WidgetState;

      _Scroll:Vector2D;

      _Animations:Array Of UIWidgetStateAnimation;
      _AnimationCount:Integer;

			Function GetAbsolutePosition:Vector2D;
      Function GetRelativePosition:Vector2D;
      Function GetAlignedPosition:Vector2D;

      Function GetHeight: UIDimension;
      Function GetWidth: UIDimension;

      Procedure SetParent(Target:UIWidget);

      Function GetDraggable: Boolean;
      Procedure SetDraggable(const Value: Boolean);
      Function GetUIView: UIWidget;

      Function IsEnabled: Boolean;

      Function GetClipRect: TERRAClipRect;
    function GetController: UIController;
    procedure SetController(const Value: UIController);
    procedure SetSelected(const Value: Boolean);

		Protected
			_Parent:UIWidget;

      _ChildrenList:Array Of UIWidget;
      _ChildrenCount:Integer;

      _Properties:Array Of UIProperty;
      _PropertyCount:Integer;

      _Tooltip:TERRAString;
      _NeedsUpdate:Boolean;
      _NeedsHide:Boolean;

      _Selected:Boolean;

      _Dragging: Boolean;
      _DragMode:UIDragMode;
      _DragX: Single;
      _DragY: Single;
      _DragSize:Vector2D;
      _DragStart:Vector2D;

      _Transform:Matrix3x3;
      _InverseTransform:Matrix3x3;
      _TransformChanged:Boolean;

      _InheritColor:Boolean;

      _Hitting:Boolean;
      _HitTime:Cardinal;

      _Size:Vector2D;
      _Center:Vector2D;

      _OriginalColor:ColorRGBA;
      _OriginalPosition:Vector2D;

      _ColorTable:TERRATexture;

      _ClipRect:TERRAClipRect;

      _VisibleFrame:Cardinal;

      _DropShadowColor:ColorRGBA;

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
      Procedure SetColor(MyColor:ColorRGBA);
      Procedure SetRotation(const Value: Single);
      Procedure SetSaturation(const Value: Single);
      Procedure SetScale(const Value: Single);

      Procedure SetTransform(const Value: Matrix3x3);

      Function GetRotation():Single;
      Function GetScale():Single;

      Function ResolveMacro(Const Value:TERRAString):TERRAString;

      Procedure UpdateProperties();

      Function AdjustWidth(NewWidth:Single):Single;
      Function AdjustHeight(NewHeight:Single):Single;

      //Function HasMouseOver():Boolean; Virtual;

      Function GetFontRenderer:TERRAFontRenderer;

      //Function OutsideClipRect(X,Y:Single):Boolean;

      Procedure UpdateLanguage;

      Procedure SetObjectName(const Value: TERRAString); Override;

      Procedure ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);

      Function GetPivot: Vector2D;
      Procedure SetPivot(const Value: Vector2D);

      Procedure SetState(Value:WidgetState);

      Procedure OnStateChange(); Virtual;

      Function GetEventHandler(EventType:WidgetEventType):UIWidgetEventHandler;

      Property FontRenderer:TERRAFontRenderer Read GetFontRenderer;

		Public
      Tag:Integer;
      DisableHighlights:Boolean;
      DisableUIColor:Boolean;
      UserData:TERRAString;

      CanReceiveEvents:Boolean;

      Constructor Create(Const Name:TERRAString; Parent:UIWidget);
      Procedure Release; Override;

      Function IsSelectable():Boolean; Virtual;
      Function CanHighlight(GroupID:Integer):Boolean;

      Procedure TriggerEvent(EventType:WidgetEventType); Virtual;
      Procedure OnHighlight(Prev:UIWidget); Virtual;

      Procedure Delete();

      Procedure AddAnimation(State:WidgetState; Const PropName, Value:TERRAString; Const Ease:TweenEaseType = easeLinear);

      Function CallEventHandler(EventType:WidgetEventType):Boolean;

      Function CanRender():Boolean;
      Function AllowsEvents(): Boolean;

      Procedure PickAt(Const X, Y:Integer; WithEventsOnly:Boolean; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget = Nil);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Procedure Render(View:TERRAViewport); Virtual;

      Procedure UpdateRects; Virtual;
      Function UpdateTransform():Boolean; Virtual;

			Function OnKeyDown(Key:Word):Boolean;Virtual;
			Function OnKeyUp(Key:Word):Boolean;Virtual;
			Function OnKeyPress(Key:TERRAChar):Boolean;Virtual;

			Function GetVisible:Boolean;
			Function GetLayer:Single;
      Function GetColor:ColorRGBA;
			Function GetSaturation:Single;
      Function GetColorTable:TERRATexture;
      Function GetHighlightGroup:Integer;

      Function GetDimension(Const Dim:UIDimension; Const Target:UIDimensionTarget):Single;

      Procedure ConvertGlobalToLocal(Var V:Vector2D);
      Procedure ConvertLocalToGlobal(Var V:Vector2D);

      Procedure SetPositionRelativeToOther(Other:UIWidget; OfsX, OfsY:Single);

      Function GetScrollOffset():Vector2D;

			Procedure OnLanguageChange();Virtual;

      Procedure NullEventHandler(Src:UIWidget);

      Function OnRegion(X,Y:Single): Boolean; Virtual;
      Function OnCustomRegion(X,Y:Integer; X1,Y1,X2,Y2:Single):Boolean;

      Procedure AddChild(W:UIWidget);
      Procedure RemoveChild(W:UIWidget);
      Procedure RemoveAllChildren();
      Function GetChildByIndex(Index:Integer):UIWidget;
      Function GetChildByName(Const Name:TERRAString):UIWidget;

      Function FindComponent(ComponentType:UIWidgetClass):UIWidget;

      Function OnSelectRight():Boolean; Virtual;
      Function OnSelectLeft():Boolean; Virtual;
      Function OnSelectUp():Boolean; Virtual;
      Function OnSelectDown():Boolean; Virtual;
      Function OnSelectAction():Boolean; Virtual;

      Procedure SetPropertyValue(Const PropName, Value:TERRAString);

      Function IsOutsideScreen():Boolean;

      Procedure SetHeight(const Value: UIDimension);
      Procedure SetWidth(const Value: UIDimension);

      Procedure SetChildrenVisibilityByTag(Tag:Integer; Visibility:Boolean);

			Procedure OnMouseUp(X,Y:Integer; Button:Word); Virtual;
			Procedure OnMouseMove(X,Y:Integer); Virtual;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer);Virtual;

      Function GetIndex():Integer;

      Function IsSameFamily(Other:UIWidget):Boolean;

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

      Property Pivot:Vector2D Read GetPivot Write SetPivot;
      Property Size:Vector2D Read _Size;
			Property Layer:Single Read GetLayer Write SetLayer;

      Property InheritColor:Boolean Read _InheritColor Write _InheritColor;

      Property Color:ColorRGBA Read GetColor Write SetColor;
      Property ColorTable:TERRATexture Read GetColorTable Write _ColorTable;
      Property Saturation:Single Read GetSaturation Write SetSaturation;
      Property Rotation:Single Read GetRotation Write SetRotation;
      Property Scale:Single Read GetScale Write SetScale;

      Property Parent:UIWidget Read _Parent Write SetParent;
      Property Align:Integer Read GetAlign Write SetAlign;

      Property State:WidgetState Read _State;

      Property Dragging:Boolean Read _Dragging;

      Property ChildrenCount:Integer Read _ChildrenCount;

      Property ClipRect:TERRAClipRect Read GetClipRect;

      Property Center:Vector2D Read _Center Write _Center;

      Property Enabled:Boolean  Read IsEnabled;

      Property Selected:Boolean Read _Selected Write SetSelected;

      Property Controller:UIController Read GetController Write SetController;

      Property DropShadowColor:ColorRGBA Read _DropShadowColor Write _DropShadowColor;

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

Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Math, TERRA_GraphicsManager,
  TERRA_UIView, TERRA_UITiledRect, TERRA_UIImage, TERRA_UILabel, TERRA_UIEditText,
  TERRA_DebugDraw, TERRA_Localization;

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
Var
  I:WidgetState;
Begin
  _ObjectName := Name;

  //_Component := UIComponentImage.Create();

  Self.InitProperties();

  SetVisible(True);

  SetState(widget_Default);

  SetScale(1.0);
  SetRotation(0.0);
  SetSaturation(1.0);
  SetColor(ColorWhite);
  _ColorTable := Nil;

  _ClipRect.Style := clipNothing;

  //_DropShadowColor := ColorNull;
  _DropShadowColor := ColorGrey(0, 255);

  Self.AddAnimation(widget_Default, 'color', 'FFFFFFFF', easeLinear);
  Self.AddAnimation(widget_Default, 'scale', '1.0', easeLinear);

  Self.AddAnimation(widget_Selected, 'color', '55FF55FF');
//  Self.AddAnimation(widget_Selected, 'scale', '2.0');

  Self.AddAnimation(widget_Highlighted, 'color', 'FF5555FF');

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
Var
  I:WidgetEventType;
Begin
  _Width := DimensionProperty(Self.AddProperty(DimensionProperty.Create('width', UIPixels(0)), False));
  _Height := DimensionProperty(Self.AddProperty(DimensionProperty.Create('height', UIPixels(0)), False));
  _Visible := BooleanProperty(Self.AddProperty(BooleanProperty.Create('visible', True), False));
  _Position := Vector2DProperty(Self.AddProperty(Vector2DProperty.Create('position', VectorCreate2D(0, 0)), False));
  _Pivot := Vector2DProperty(Self.AddProperty(Vector2DProperty.Create('pivot', VectorCreate2D(0.5, 0.5)), False));
  _Layer := FloatProperty(Self.AddProperty(FloatProperty.Create('layer', 1.0), False));
  _Color := ColorProperty(Self.AddProperty(ColorProperty.Create('color', ColorWhite), False));
  _Rotation := AngleProperty(Self.AddProperty(AngleProperty.Create('rotation', 0.0), False));
  _Scale := FloatProperty(Self.AddProperty(FloatProperty.Create('scale', 1.0), False));
  _Saturation := FloatProperty(Self.AddProperty(FloatProperty.Create('saturation', 1.0), False));
  _Draggable := BooleanProperty(Self.AddProperty(BooleanProperty.Create('draggable', False), False));
  _Align := EnumProperty(Self.AddProperty(EnumProperty.Create('align', 0, UIManager.Instance.AlignEnums), False));
  _Controller := UIControllerProperty(Self.AddProperty(UIControllerProperty.Create('controller', Nil), False));
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

Procedure UIWidget.TriggerEvent(EventType:WidgetEventType);
Var
  I, J, N, Count:Integer;
  TargetState:WidgetState;
  Prop:TERRAObject;
  TweenableProp:TweenableProperty;
  CurrentValue, TargetValue:TERRAString;
  Dispatched:Boolean;
  Callback:TweenCallback;
Begin
  If (Self.State = widget_Disabled) Then
    Exit;

  Case EventType Of
    widgetEvent_MouseDown:
      TargetState := widget_Selected;

    widgetEvent_MouseOver:
      TargetState := widget_Highlighted;

    widgetEvent_DragBegin:
      TargetState := widget_Dragged;

    widgetEvent_Hide:
      TargetState := widget_Hidden;

    widgetEvent_Show,
    widgetEvent_MouseOut,
    widgetEvent_FocusEnd,
    //widgetEvent_MouseUp,
    widgetEvent_DragEnd:
      If _Selected Then
        TargetState := widget_Selected
      Else
        TargetState := widget_Default;

    Else
      TargetState := Self.State;
  End;

  If (TargetState = Self.State) Then
  Begin
    CallEventHandler(EventType);
    Exit;
  End;

  N := 150;
  Count := 0;

  Dispatched := False;

  For I:=0 To Pred(_AnimationCount) Do
  If (_Animations[I].State = TargetState) Then
  Begin
    Prop := Self.FindProperty(_Animations[I].PropName);
    If (Prop = Nil) Then
      Continue;

    If (Prop Is TweenableProperty) Then
      TweenableProp := TweenableProperty(Prop)
    Else
      TweenableProp  := Nil;

    CurrentValue := Prop.GetBlob();
    TargetValue := _Animations[I].Value;

    For J:=0 To Pred(_AnimationCount) Do
    If (_Animations[J].State = Self.State) And (StringEquals(_Animations[J].PropName, Prop.Name)) Then
    Begin
      CurrentValue := _Animations[J].Value;
      Break;
    End;

    If (Assigned(TweenableProp)) And (_Animations[I].Ease <> easeNone) Then
    Begin
      If Dispatched Then
        Callback := Nil
      Else
      Begin
        Callback := TweenCallback(Self.GetEventHandler(EventType));
        Dispatched := True;
      End;

      TweenableProp.AddTweenFromBlob(_Animations[I].Ease, CurrentValue, TargetValue, N, 0, Callback, Self)
    End Else
    Begin
      Prop.SetBlob(TargetValue);
      If Not Dispatched Then
      Begin
        Dispatched := True;
        CallEventHandler(EventType);
      End;
    End;

    //Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Virtual; Abstract;
  End;

  SetState(TargetState);

  If (_State = widget_Selected) Then
    _Selected := True
  Else
  If (_State = widget_Default) Then
    _Selected := False;
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

Function UIWidget.IsSameFamily(Other:UIWidget): Boolean;
Begin
  Result := (Self = Other);
  If (Not Result) And (Other<>Nil) And (Other._Parent<>Nil) Then
    Result := IsSameFamily(Other._Parent);
End;

Function UIWidget.IsSelectable():Boolean;
Begin
  Result := Assigned(Self.GetEventHandler (widgetEvent_MouseDown));
End;

Procedure UIWidget.ConvertGlobalToLocal(Var V:Vector2D);
Begin
  V := Self._InverseTransform.Transform(V);
End;

Procedure UIWidget.ConvertLocalToGlobal(Var V:Vector2D);
Begin
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
  TY:Single;
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
    _Position.X.AddTween(EaseType, -(Self.Size.X), _Position.X.Value, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosX_Bottom<>0) Then
  Begin
    _Position.X.AddTween(EaseType, UIManager.Instance.Width + (Self.Size.X), _Position.X.Value, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimatePosY<>0) Then
  Begin
    TY := -Self.Size.Y;
    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      TY := TY - (UIManager.Instance.Height * 0.5);

    _Position.Y.AddTween(EaseType, TY, _Position.Y.Value, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosY_Bottom<>0) Then
  Begin
    TY := UIManager.Instance.Height + Self.Size.Y;
    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      TY := TY + (UIManager.Instance.Height * 0.5);

    _Position.Y.AddTween(EaseType, TY, _Position.Y.Value, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateAlpha<>0) Then
  Begin
    _Color.Alpha.AddTween(EaseType, 0, _Color.Alpha.Value, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateRotation<>0) Then
  Begin
    _Rotation.AddTween(EaseType, GetRotation() + (360.0 * RAD) * 4.0, GetRotation(), Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateScale<>0) Then
  Begin
    _Scale.AddTween(EaseType, 0.0, GetScale(), Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateSaturation<>0) Then
  Begin
    _Saturation.AddTween(EaseType, 0.0, GetSaturation(), Duration, Delay, Callback, Self);
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

    _Position.X.AddTween(EaseType, _Position.X.Value, Ofs, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosX_Bottom<>0) Then
  Begin
    _Position.X.AddTween(EaseType, _Position.X.Value, UIManager.Instance.Width +(Self.Size.X+15), Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimatePosY<>0) Then
  Begin
    Ofs := -Self.Size.Y;

    If (Self.Align = waCenter) Or (Self.Align = waLeftCenter) Or (Self.Align = waRightCenter) Then
      Ofs := Ofs - Self.AbsolutePosition.Y;

    _Position.Y.AddTween(EaseType, _Position.Y.Value, Ofs, Duration, Delay, Callback, Self);
    Callback := Nil;
  End Else
  If (AnimationFlags And widgetAnimatePosY_Bottom<>0) Then
  Begin
    _Position.Y.AddTween(EaseType, _Position.Y.Value, UIManager.Instance.Height + Self.Size.Y, Duration, Delay, Callback, Self);
    Callback := Nil;
  End;

  If (AnimationFlags And widgetAnimateAlpha<>0) Then
  Begin
    _Color.Alpha.AddTween(EaseType, _Color.Alpha.Value, 0, Duration, Delay, Callback, Self);
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
  Begin
    _VisibleFrame := GraphicsManager.Instance.FrameID;
    Self.TriggerEvent(widgetEvent_Show);
  End Else
  Begin
    Self.TriggerEvent(widgetEvent_Hide);
  End;
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

Procedure UIWidget.SetColor(MyColor:ColorRGBA);
Begin
(*  If (Cardinal(MyColor) = Cardinal(_Color)) Then
    Exit;*)

  _Color.Value := MyColor;
End;

Function UIWidget.GetColor:ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  TempAlpha:Byte;
  ParentColor:ColorRGBA;
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
    Width := Self._Size.X{ * _Scale};
    Height := _Size.Y{ * _Scale};

    {IF _Scale>1 Then
      IntToString(2);}

    ParentSize := _Parent.Size;

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

Function UIWidget.OnKeyPress(Key:TERRAChar):Boolean;
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

  If (GraphicsManager.Instance.FrameID = Self._VisibleFrame) {Or (OutsideClipRect(X,Y))} Then
  Begin
    Result := False;
    {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Cliprect clipped!');{$ENDIF}
    Exit;
  End;

  V.X := X;
  V.Y := Y;
  Self.ConvertGlobalToLocal(V);

  //DrawPoint2D(UIManager.Instance.Viewport, V, ColorYellow, 4);

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

Procedure UIWidget.PickAt(Const X, Y:Integer; WithEventsOnly:Boolean; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget);
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.PickAt called');{$ENDIF}

  If (Self.Layer < Max) Or (Not Self.OnRegion(X,Y)) Or (Self = Ignore) Then
    Exit;

  If (Not WithEventsOnly) Or (Self.CanReceiveEvents) Then
  Begin
    CurrentPick := Self;
    Max := Self.Layer;
  End;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].AllowsEvents()) Then
  Begin
    _ChildrenList[I].PickAt(X, Y, WithEventsOnly, CurrentPick, Max, Ignore);
  End;
End;

Procedure UIWidget.BeginDrag(X,Y:Integer; Mode:UIDragMode);
Var
  UI:UIView;
Begin
  UI := UIView(Self.GetUIView);
  If UI = Nil Then
    Exit;

  If (Assigned(Self.Parent)) And (Self.Parent Is UIInstancedWidget) Then
  Begin
    Self.Parent.BeginDrag(X, Y, Mode);
    Exit;
  End;

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

  Mat:Matrix3x3;
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

  Self.UpdateRects();

  If Assigned(_Parent) Then
    Ratio := 1.0
  Else
    Ratio := UIManager.Instance.Ratio;

  Center := Self.Size;
  Center.Scale(Self.Pivot);

  (*If _Rotation.Value<>0 Then
    _Rotation.Value := 1.0+RAD*(Trunc(Application.GetTime()/10));*)

  If (_Rotation.Value <> 0.0) Then
    Mat := MatrixRotationAndScale2D(_Rotation.Value, _Scale.Value, _Scale.Value * Ratio)
  Else
    Mat := MatrixScale2D(_Scale.Value, _Scale.Value * Ratio);

  Mat := MatrixTransformAroundPoint2D(Center, Mat);

  Pos := Self.GetAlignedPosition();
  Pos.Add(Self.GetScrollOffset());

  Mat := MatrixMultiply3x3(MatrixTranslation2D(Pos), Mat);

  Self.SetTransform(Mat);;
  _TransformChanged := False;

  Result := True;
End;


Procedure UIWidget.SetTransform(const Value: Matrix3x3);
Var
  I:Integer;
  Pos, SizeRect:Vector2D;
Begin
  _Transform := Value;

  If Assigned(_Parent) Then
    _Transform := MatrixMultiply3x3(Parent._Transform, _Transform);

  _InverseTransform := MatrixInverse2D(_Transform);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) Then
  Begin
    _ChildrenList[I]._TransformChanged := True;
    _ChildrenList[I].UpdateTransform();
  End;

  Pos := Self.AbsolutePosition;
  SizeRect := Self.Size;
  _ClipRect.Style := clipNothing;
  //_ClipRect.Style := clipSomething;
  _ClipRect.X := Pos.X {+ LeftBorder};
  _ClipRect.Y := Pos.Y {+ TopBorder};
  _ClipRect.Width := SizeRect.X {- (RightBorder + LeftBorder)};
  _ClipRect.Height := SizeRect.Y {- (TopBorder + BottomBorder)};


  //_ClipRect.Transform(Value);

  _TransformChanged := True;
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

Function UIWidget.FindComponent(ComponentType:UIWidgetClass):UIWidget;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].ClassType = ComponentType) Then
  Begin
    Result := _ChildrenList[I];
    Exit;
  End;

  For I:=0 To Pred(_ChildrenCount) Do
  Begin
    Result := _ChildrenList[I].FindComponent(ComponentType);
    If Assigned(Result) Then
      Exit;
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
  If (Self._Deleted) Or (Not Self.Visible) Then
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

Function UIWidget.OnSelectAction: Boolean;
Begin
  If (Self.Selected) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.TriggerEvent(widgetEvent_MouseDown);
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

Function UIWidget.GetScrollOffset():Vector2D;
Begin
  Result := Self._Scroll;

  If _Parent = Nil Then
    Exit;

  Result.Add(_Parent.GetScrollOffset());

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

(*Function UIWidget.OutsideClipRect(X, Y: Single): Boolean;
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
End;*)

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

Function UIWidget.GetFontRenderer:TERRAFontRenderer;
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

Function UIWidget.GetEventHandler(EventType:WidgetEventType):UIWidgetEventHandler;
Begin
  If (Assigned(_Controller._Value)) Then
  Begin
    Result := _Controller._Value._Handlers[EventType];
  End Else
    Result := Nil;

  If (Assigned(Result)) Or (_Parent = Nil) Then
    Exit;

  Result := _Parent.GetEventHandler(EventType);
End;

Function UIWidget.CallEventHandler(EventType:WidgetEventType):Boolean;
Var
  Handler:UIWidgetEventHandler;
Begin
  Handler := Self.GetEventHandler(EventType);
  If Assigned(Handler) Then
    Handler(Self);
End;

Function UIWidget.IsEnabled: Boolean;
Begin
  Result := (Self._State <> widget_Disabled);
End;

Procedure UIWidget.AddAnimation(State: WidgetState; const PropName, Value: TERRAString; Const Ease:TweenEaseType = easeLinear);
Begin
  Inc(_AnimationCount);
  SetLength(_Animations, _AnimationCount);

  _Animations[Pred(_AnimationCount)].State := State;
  _Animations[Pred(_AnimationCount)].PropName := PropName;
  _Animations[Pred(_AnimationCount)].Value := Value;
  _Animations[Pred(_AnimationCount)].Ease := Ease;
End;

Function UIWidget.GetPivot: Vector2D;
Begin
  Result := _Pivot.Value;
End;

Procedure UIWidget.SetPivot(const Value: Vector2D);
Begin
  _Pivot.Value := Value;
End;

Procedure UIWidget.OnStateChange;
Begin
  // do nothing
End;

Procedure UIWidget.SetState(Value: WidgetState);
Var
  I:Integer;
Begin
  _State := Value;

  Self.OnStateChange();

  For I:=0 To Pred(_ChildrenCount) Do
    _ChildrenList[I].SetState(Value);
End;

Function UIWidget.GetClipRect:TERRAClipRect;
Var
  Other:TERRAClipRect;
Begin
  Result := Self._ClipRect;

  If (Assigned(_Parent)) Then
  Begin
    Other := _Parent.ClipRect;
    Result.Merge(Other);
  End;
End;

Procedure UIWidget.SetPropertyValue(const PropName, Value: TERRAString);
Var
  Prop:TERRAObject;
Begin
  Prop := Self.FindProperty(PropName);
  If Assigned(Prop) Then
    Prop.SetBlob(Value);
End;

Function UIWidget.GetController: UIController;
Begin
  Result := _Controller._Value;
End;

Procedure UIWidget.SetController(const Value: UIController);
Begin
  _Controller._Value := Value;
End;

Procedure UIWidget.SetSelected(const Value: Boolean);
Begin
  If (_Selected = Value) Then
    Exit;

  _Selected := Value;

  If (Value) Then
    Self.TriggerEvent(widgetEvent_FocusBegin)
  Else
    Self.TriggerEvent(widgetEvent_FocusEnd);
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

  Self.CanReceiveEvents := True;
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

  If (Template Is UIEditText) Then
  Begin
    Result := UIEditText.Create(Template.Name, Parent, 0, 0, 0, UIPixels(100), UIPixels(100), '');
  End Else
  If (Template Is UILabel) Then
  Begin
    Result := UILabel.Create(Template.Name, Parent, 0, 0, 0, UIPixels(100), UIPixels(100), '??');
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
  Result.CanReceiveEvents := Template.CanReceiveEvents;

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

{ UIControllerProperty }
Constructor UIControllerProperty.Create(const Name: TERRAString; Controller:UIController);
Begin
  Self._ObjectName := Name;
  Self._Value := Controller;
End;


Function UIControllerProperty.GetObjectType: TERRAString;
Begin
  Result := 'uicontroller';
End;

Function UIControllerProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;

Function UIControllerProperty.GetBlob: TERRAString;
Begin
  If Assigned(_Value) Then
    Result := _Value.Name
  Else
    Result := '#';
End;


Procedure UIControllerProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := UIManager.Instance.GetControllerByName(Blob);
End;

{ UIController }

Function UIController.GetHandler(EventType: WidgetEventType): UIWidgetEventHandler;
Begin
  Result := _Handlers[EventType];
End;

Procedure UIController.SetHandler(EventType: WidgetEventType; Handler:UIWidgetEventHandler);
Begin
  _Handlers[EventType] := Handler;
End;

End.
