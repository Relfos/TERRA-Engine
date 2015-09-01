Unit TERRA_UIWidget;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_Collections, TERRA_List,
  TERRA_Vector2D, TERRA_Color, TERRA_Matrix3x3, TERRA_Texture, TERRA_Renderer,
  TERRA_ClipRect, TERRA_Tween, TERRA_Sprite, TERRA_Renderable,
  TERRA_UIDimension, TERRA_UICursor, TERRA_EnumProperty, TERRA_DataSource, TERRA_Viewport;

Const
  widgetAnimateAlpha  = 1;
  widgetAnimatePosX   = 2;
  widgetAnimatePosY   = 4;
  widgetAnimateRotation = 8;
  widgetAnimateScale    = 16;
  widgetAnimateSaturation  = 32;
  widgetAnimatePosX_Bottom = 64;
  widgetAnimatePosY_Bottom = 128;

  UIMacroBeginChar  = '{';
  UIMacroEndChar    = '}';
  UITranslationChar = '#';
  UIPropertyChar    = '!';
  UIDataSourceChar  = '@';

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
    UIDrag_BottomRight,

    UIDrag_Scroll
  );

  UIDirection = (
    UIDirection_Vertical,
    UIDirection_Horizontal
  );

  UILayout = (
    UILayout_Free,
    UILayout_Horizontal,
    UILayout_Vertical
  );

  UIAlign = (
    UIAlign_TopLeft = 0,
    UIAlign_TopCenter   = 1,
    UIAlign_TopRight    = 2,
    UIAlign_LeftCenter  = 3,
    UIAlign_Center      = 4,
    UIAlign_RightCenter = 5,
    UIAlign_BottomLeft     = 6,
    UIAlign_BottomCenter   = 7,
    UIAlign_BottomRight    = 8
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
    widgetEvent_ScrollDown,
    widgetEvent_ScrollUp,
    widgetEvent_FocusBegin,
    widgetEvent_FocusEnd,
    widgetEvent_ContentChange
  );

  WidgetEventClass = (
    widgetEventClass_Any,
    widgetEventClass_Visibility,
    widgetEventClass_Click,
    widgetEventClass_Scroll,
    widgetEventClass_Hover,
    widgetEventClass_Drag,
    widgetEventClass_Focus,
    widgetEventClass_Content
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

  UIWidget = Class(TERRARenderable)
    Private
      _Tested:Boolean;
      //_RenderFrameID:Cardinal;

      _Width:DimensionProperty;
      _Height:DimensionProperty;
			_X:DimensionProperty;
			_Y:DimensionProperty;
      _Margin:MarginProperty;
      _Pivot:Vector2DProperty;
      _Layer:FloatProperty;
      _Align:EnumProperty;
      _Color:ColorProperty;
      _Glow:ColorProperty;
      _Rotation:AngleProperty;
      _Scale:FloatProperty;
      _Saturation:FloatProperty;
			_Visible:BooleanProperty;
      _Draggable:BooleanProperty;

      _Handlers:Array[WidgetEventType] Of UIWidgetEventHandler;

      _Deleted:Boolean;

      _State:WidgetState;

      _CurrentScroll:Vector2D;
      _ScrollLimits:Vector2D;

      _Index:Integer;

      _Animations:Array Of UIWidgetStateAnimation;
      _AnimationCount:Integer;

			Function GetAbsolutePosition:Vector2D;
      Function GetAlignedPosition:Vector2D;
      Function GetRelativePosition:Vector2D;

      Function GetHeight: UIDimension;
      Function GetWidth: UIDimension;
      Function GetPosition_Left: UIDimension;
      Function GetPosition_Top: UIDimension;

      Procedure SetParent(Target:UIWidget);
      Procedure SetRelativePosition(Const Pos:Vector2D);

      Function GetDraggable: Boolean;
      Procedure SetDraggable(const Value: Boolean);
      Function GetUIView: UIWidget;

      Function IsEnabled: Boolean;

      Function GetClipRect: TERRAClipRect;
      Procedure SetSelected(const Value: Boolean);

      Function GetState: WidgetState;
      Procedure SetClipRect(const Value: TERRAClipRect);

      Function IsSelected: Boolean;
      Function IsScrollable: Boolean;


    Protected
      _Parent:UIWidget;

      _ChildrenList:Array Of UIWidget;
      _ChildrenCount:Integer;

      _Properties:Array Of UIProperty;
      _PropertyCount:Integer;

      _Tooltip:TERRAString;
      _NeedsUpdate:Boolean;

      _Dragging: Boolean;
      _DragMode:UIDragMode;
      _DragX: Single;
      _DragY: Single;
      _DragSize:Vector2D;
      _DragStart:Vector2D;
      _DragScroll:Vector2D;
      _DragStartLeft:UIDimension;
      _DragStartTop:UIDimension;

      _Transform:Matrix3x3;
      _InverseTransform:Matrix3x3;
      _TransformChanged:Boolean;

      _Hitting:Boolean;
      _HitTime:Cardinal;

      _CurrentSize:Vector2D;
      _FullSize:Vector2D;
      _Center:Vector2D;

      _ColorTable:TERRATexture;

      _ClipRect:TERRAClipRect;
      _CustomClip:Boolean;

      _VisibleFrame:Cardinal;

      _DropShadowColor:ColorRGBA;

      _HighlightGroup:Integer;

      _Sprite:TERRASprite;

      _CurrentCursor:TERRACursorType;

      Function AddProperty(Prop:TERRAObject; IsCustom:Boolean):TERRAObject;

      Procedure UpdateSprite(View:TERRAViewport); Virtual;

      Procedure InitProperties(Const Name:TERRAString); Virtual;

      Function GetRenderBucket: Cardinal; Override;

      Function GetUpControl():UIWidget;
      Function GetDownControl():UIWidget;
      Function GetLeftControl():UIWidget;
      Function GetRightControl():UIWidget;

      Function GetAlign:UIAlign;
      Procedure SetAlign(const Value:UIAlign);

      Procedure SetVisible(Value:Boolean);
      Procedure SetLayer(Z:Single);
      Procedure SetColor(Const Value:ColorRGBA);
      Procedure SetGlow(Const Value:ColorRGBA);
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

      //Function OutsideClipRect(X,Y:Single):Boolean;

      Procedure UpdateLanguage;

      Procedure SetObjectName(const Value: TERRAString); Override;

      Procedure ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);

      Function GetPivot: Vector2D;
      Procedure SetPivot(const Value: Vector2D);

      Procedure SetState(Value:WidgetState);

      Procedure OnStateChange(); Virtual;

      Procedure ConvertAlign(Const Direction:UIDirection);

      Function GetCurrentCursor():TERRACursorType;


    Public
      Tag:Integer;
      DisableHighlights:Boolean;
      DisableUIColor:Boolean;
      UserData:TERRAString;

      Constructor Create(Const Name:TERRAString; Parent:UIWidget);
      Procedure Release; Override;

      Function IsSelectable():Boolean; Virtual;
      //Function CanHighlight(GroupID:Integer):Boolean;

      Procedure TriggerEvent(EventType:WidgetEventType); Virtual;

      Procedure Delete();

      Procedure AddAnimation(State:WidgetState; Const PropName, Value:TERRAString; Const Ease:TweenEaseType = easeLinear);

      Function CanRender():Boolean;
      Function AllowsEvents(): Boolean;

      Procedure PickAt(Const X, Y:Integer; Const EventClass:WidgetEventClass; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget = Nil);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;

      Procedure UpdateRects; Virtual;
      Function UpdateTransform():Boolean; Virtual;

      Function GetVisible:Boolean;
      Function GetLayer:Single;
      Function GetColor:ColorRGBA;
      Function GetGlow:ColorRGBA;
      Function GetSaturation:Single;
      Function GetColorTable:TERRATexture;
      Function GetHighlightGroup:Integer;

      Function GetDimension(Const Dim:UIDimension; Const Target:UIDimensionTarget):Single;

      Procedure ConvertGlobalToLocal(Var V:Vector2D);
      Procedure ConvertLocalToGlobal(Var V:Vector2D);

      //Procedure SetPositionRelativeToOther(Other:UIWidget; OfsX, OfsY:Single);

      Function HasAnimation(State:WidgetState):Boolean;
      Function HasEvent(EventType:WidgetEventType):Boolean;

      Function ReactsToEventClass(Const EventClass:WidgetEventClass):Boolean; Virtual;
      Function CallEventHandler(EventType:WidgetEventType):Boolean;

      Function GetEventHandler(EventType: WidgetEventType): UIWidgetEventHandler;
      Procedure SetEventHandler(EventType: WidgetEventType; Handler:UIWidgetEventHandler);

      Function GetScrollOffset():Vector2D;
      Procedure SetScrollOffset(Const Ofs:Vector2D);

			Procedure OnLanguageChange();Virtual;

      Procedure NullEventHandler(Src:UIWidget);

      Function OnChildrenRegion(Const X,Y:Single):Boolean;
      Function OnRegion(Const X,Y:Single): Boolean; Virtual;
      Function OnCustomRegion(Const X,Y:Integer; X1,Y1,X2,Y2:Single):Boolean;

      Procedure AddChild(W:UIWidget);
      Procedure RemoveChild(W:UIWidget);
      Procedure RemoveAllChildren();
      Function GetChildByIndex(Index:Integer):UIWidget;
      Function GetChildByName(Const Name:TERRAString; ID:Integer = 1):UIWidget;

      Function FindComponent(ComponentType:UIWidgetClass):UIWidget;

      Function OnSelectRight():Boolean; Virtual;
      Function OnSelectLeft():Boolean; Virtual;
      Function OnSelectUp():Boolean; Virtual;
      Function OnSelectDown():Boolean; Virtual;
      Function OnSelectAction():Boolean; Virtual;

      Procedure SetPropertyValue(Const PropName, Value:TERRAString);

//      Function IsOutsideScreen():Boolean;

      Procedure SetHeight(const Value: UIDimension);
      Procedure SetWidth(const Value: UIDimension);
      Procedure SetPosition_Left(const Value: UIDimension);
      Procedure SetPosition_Top(const Value: UIDimension);

      //Procedure SetChildrenVisibilityByTag(Tag:Integer; Visibility:Boolean);

      Function OnHandleKeyDown(Key:Word):Boolean;Virtual;
      Function OnHandleKeyUp(Key:Word):Boolean;Virtual;
      Function OnHandleKeyPress(Key:TERRAChar):Boolean;Virtual;

      Procedure OnHandleMouseUp(X,Y:Integer; Button:Word); Virtual;
      Procedure OnHandleMouseMove(X,Y:Integer); Virtual;
      Procedure OnHandleMouseWheel(X,Y:Integer; Delta:Single);Virtual;

      Function GetIndex():Integer;

      Function IsSameFamily(Other:UIWidget):Boolean;

      Function BeginDrag(X,Y:Integer; Mode:UIDragMode):Boolean;
      Procedure FinishDrag();
      Procedure CancelDrag();

      Function SupportDrag(Mode:UIDragMode):Boolean; Virtual;

      (*Function Show(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;
      Function Hide(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;
      Function ToggleVisibility(AnimationFlags:Integer; EaseType:TweenEaseType = easeLinear; Delay:Cardinal = 0; Duration:Cardinal = 500; Callback:TweenCallback = Nil):Boolean;*)

      Procedure Show();
      Procedure Hide();

      //Procedure CenterOnScreen(CenterX:Boolean = True; CenterY:Boolean = True);
      //Procedure CenterOnParent(CenterX:Boolean = True; CenterY:Boolean = True);
      //Procedure CenterOnPoint(X,Y:Single);

      Function Hidden():Boolean;
			Property Visible:Boolean Read GetVisible Write SetVisible;

      Property CurrentCursor:TERRACursorType Read GetCurrentCursor;

			Property Left:UIDimension Read GetPosition_Left Write SetPosition_Left;
			Property Top:UIDimension Read GetPosition_Top Write SetPosition_Top;

      Property Pivot:Vector2D Read GetPivot Write SetPivot;
      Property CurrentSize:Vector2D Read _CurrentSize;
      Property FullSize:Vector2D Read _FullSize;
      Property ScrollLimits:Vector2D Read _ScrollLimits;
			Property Layer:Single Read GetLayer Write SetLayer;

      Property Color:ColorRGBA Read GetColor Write SetColor;
      Property Glow:ColorRGBA Read GetGlow Write SetGlow;
      Property ColorTable:TERRATexture Read GetColorTable Write _ColorTable;
      Property Saturation:Single Read GetSaturation Write SetSaturation;
      Property Rotation:Single Read GetRotation Write SetRotation;
      Property Scale:Single Read GetScale Write SetScale;

      Property Parent:UIWidget Read _Parent Write SetParent;
      Property Align:UIAlign Read GetAlign Write SetAlign;

      Property State:WidgetState Read GetState;

      Property Dragging:Boolean Read _Dragging;

      Property ChildrenCount:Integer Read _ChildrenCount;

      Property AbsolutePosition:Vector2D Read GetAbsolutePosition;
      Property RelativePosition:Vector2D Read GetRelativePosition Write SetRelativePosition;

      Property ClipRect:TERRAClipRect Read GetClipRect Write SetClipRect;

      Property Center:Vector2D Read _Center Write _Center;

      Property Enabled:Boolean  Read IsEnabled;

      Property Selected:Boolean Read IsSelected Write SetSelected;

      Property DropShadowColor:ColorRGBA Read _DropShadowColor Write _DropShadowColor;

      Property HighlightGroup:Integer Read GetHighlightGroup Write _HighlightGroup;

      Property Width:UIDimension Read GetWidth Write SetWidth;
      Property Height:UIDimension Read GetHeight Write SetHeight;
      Property Margin:MarginProperty Read _Margin;

      Property Draggable:Boolean Read GetDraggable Write SetDraggable;
      Property Scrollable:Boolean Read IsScrollable;

      Property Deleted:Boolean Read _Deleted;
      Property TransformChanged:Boolean Read _TransformChanged Write _TransformChanged;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;

      Property View:UIWidget Read GetUIView;

      Property ID:Integer Read _Index;
	End;

  UIWidgetGroup = Class(UIWidget)
    Protected
      _Layout:EnumProperty;
      _Padding:DimensionProperty;

      Function UpdateTransform():Boolean; Override;
      Procedure InitProperties(Const Name:TERRAString); Override;

      Function GetLayout: UILayout;
      Procedure SetLayout(const Value: UILayout);

      Function GetPadding: UIDimension;
      Procedure SetPadding(const Value: UIDimension);

(*      Function GetTemplate: TERRAString;
      Procedure SetTemplate(const Value: TERRAString);*)

      Function OnRegion(Const X,Y:Single): Boolean; Override;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension{; Const TemplateName:TERRAString = ''});

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;

      Property Layout:UILayout Read GetLayout Write SetLayout;
      Property Padding:UIDimension Read GetPadding Write SetPadding;
//      Property Template:TERRAString Read GetTemplate Write SetTemplate;
    End;

  UIScrollArea = Class(UIWidgetGroup)
    Protected
      Function SupportDrag(Mode:UIDragMode):Boolean; Override;
    Public
    End;

  UIInstancedWidget = Class(UIWidget)
    Protected
      _TemplateName:StringProperty;

      Function InitFromTemplate(Template, Parent:UIWidget):UIWidget;

      Function OnRegion(Const X,Y:Single): Boolean; Override;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const TemplateName:TERRAString);
      Procedure Release(); Override;

      Function GetObjectType:TERRAString; Override;
  End;

  UITemplateList = Class(TERRAObject)
    Protected
      _Templates:TERRAList;

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
  TERRA_DebugDraw, TERRA_EngineManager, TERRA_Localization;

Var
  _AlignEnums:EnumCollection;
  _DirectionEnums:EnumCollection;
  _LayoutEnums:EnumCollection;

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

(*Procedure ShowWidget(Source:UIWidget); CDecl;
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
End;*)

{ UIWidget }
Constructor UIWidget.Create(Const Name:TERRAString; Parent:UIWidget);
Var
  I:WidgetState;
Begin
  _ObjectName := Name;

  _RenderFlags := renderFlagsSkipFrustum;

  //_Component := UIComponentImage.Create();

  Self.InitProperties(Name);

  SetScale(1.0);
  SetRotation(0.0);
  SetSaturation(1.0);
  SetColor(ColorWhite);
  _ColorTable := Nil;

  SetState(widget_Default);
  
  _ClipRect.Style := clipNothing;
  _CurrentCursor := Cursor_Default;

  //_DropShadowColor := ColorNull;
  _DropShadowColor := ColorGrey(0, 255);

  Self.AddAnimation(widget_Default, 'color', 'FFFFFFFF', easeLinear);
  Self.AddAnimation(widget_Default, 'scale', '1.0', easeLinear);

//  Self.AddAnimation(widget_Selected, 'color', '55FF55FF');

//  Self.AddAnimation(widget_Highlighted, 'color', 'FF5555FF');

  Self.AddAnimation(widget_Hidden, 'color', 'FFFFFF00', easeLinear);

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

Procedure UIWidget.InitProperties(Const Name:TERRAString);
Var
  I:WidgetEventType;
Begin
  _Width := DimensionProperty(Self.AddProperty(DimensionProperty.Create('width', UIPixels(0)), False));
  _Height := DimensionProperty(Self.AddProperty(DimensionProperty.Create('height', UIPixels(0)), False));
  _Visible := BooleanProperty(Self.AddProperty(BooleanProperty.Create('visible', True), False));
  _X := DimensionProperty(Self.AddProperty(DimensionProperty.Create('left', UIPixels(0)), False));
  _Y := DimensionProperty(Self.AddProperty(DimensionProperty.Create('top', UIPixels(0)), False));
  _Margin := MarginProperty(Self.AddProperty(MarginProperty.Create('margin'), False));
  _Pivot := Vector2DProperty(Self.AddProperty(Vector2DProperty.Create('pivot', Vector2D_Create(0.5, 0.5)), False));
  _Layer := FloatProperty(Self.AddProperty(FloatProperty.Create('layer', 1.0), False));
  _Color := ColorProperty(Self.AddProperty(ColorProperty.Create('color', ColorWhite), False));
  _Glow := ColorProperty(Self.AddProperty(ColorProperty.Create('glow', ColorBlack), False));
  _Rotation := AngleProperty(Self.AddProperty(AngleProperty.Create('rotation', 0.0), False));
  _Scale := FloatProperty(Self.AddProperty(FloatProperty.Create('scale', 1.0), False));
  _Saturation := FloatProperty(Self.AddProperty(FloatProperty.Create('saturation', 1.0), False));
  _Draggable := BooleanProperty(Self.AddProperty(BooleanProperty.Create('draggable', False), False));
  _Align := EnumProperty(Self.AddProperty(EnumProperty.Create('align', 0, _AlignEnums), False));
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

(*Procedure UIWidget.CenterOnPoint(X,Y:Single);
Begin
  Self.Align := waTopLeft;
  Self.UpdateRects;

  _Position.X.Value := X - _Size.X * 0.5;
  _Position.Y.Value := Y - _Size.Y * 0.5;
End;*)

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

  If (Self.HasPropertyTweens()) Then
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
      (*If Self.Selected Then
        TargetState := widget_Selected
      Else*)
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

    (*For J:=0 To Pred(_AnimationCount) Do
    If (_Animations[J].State = Self.State) And (StringEquals(_Animations[J].PropName, Prop.Name)) Then
    Begin
      CurrentValue := _Animations[J].Value;
      Break;
    End;*)

    If (Assigned(TweenableProp)) And (_Animations[I].Ease <> easeNone) Then
    Begin
      If Dispatched Then
        Callback := Nil
      Else
      Begin
        Callback := TweenCallback(Self.GetEventHandler(EventType));
        Dispatched := True;
      End;

      (*If TweenableProp.Name = 'scale' Then
        DebugBreak;*)

      TweenableProp.AddTweenFromBlob(_Animations[I].Ease, CurrentValue, TargetValue, N, 0, Callback, Self)
    End Else
    Begin
      Prop.SetBlob(TargetValue);
    End;
  End;

  If Not Dispatched Then
  Begin
    Dispatched := True;
    CallEventHandler(EventType);
  End;

  SetState(TargetState);
End;

Procedure UIWidget.OnLanguageChange;
Begin
  // do nothing
End;

Procedure UIWidget.UpdateRects();
Var
  I:Integer;
  Temp, ClipMin, ClipMax:Vector2D;
Begin
  _CurrentSize.X := Self.GetDimension(Self.Width, uiDimensionWidth);
  _CurrentSize.Y := Self.GetDimension(Self.Height, uiDimensionHeight);

  If Not _CustomClip Then
  Begin
    _ClipRect.Style := clipSomething;
    ClipMin := Vector2D_Zero;
    ClipMax := _CurrentSize;

    ClipMin.Add(Self.GetScrollOffset);
    ClipMax.Add(Self.GetScrollOffset);

    _ClipRect.SetArea(ClipMin.X, ClipMin.Y, ClipMax.X, ClipMax.Y);
    _ClipRect.Transform(_Transform);
  End;

  _ScrollLimits := Self.CurrentSize;
  For I:=0 To Pred(_ChildrenCount) Do
  Begin
    Temp := _ChildrenList[I].RelativePosition;
    Temp.Add(_ChildrenList[I].FullSize);

    _ScrollLimits.X := FloatMax(_ScrollLimits.X, Temp.X);
    _ScrollLimits.Y := FloatMax(_ScrollLimits.Y, Temp.Y);
  End;
  _ScrollLimits.Subtract(Self.CurrentSize);
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


(*Function UIWidget.CanHighlight(GroupID:Integer): Boolean;
Begin
  Result := (Self.Visible) And (Self.Enabled) And (Self.IsSelectable()) And (Self.HighlightGroup = GroupID) And (Not Self.HasPropertyTweens());
End;*)

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
  Base.Y := Base.Y + Self.CurrentSize.Y;
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


Procedure UIWidget.Show();
Begin
  Self.TriggerEvent(widgetEvent_Show);
End;

Procedure UIWidget.Hide();
Begin
  Self.TriggerEvent(widgetEvent_Hide);
End;

(*Function UIWidget.Show(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
Var
  TY:Single;
  A:Byte;
Begin
  If Visible Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'UI', 'Showing '+Self.Name+' with animation '+ IntegerProperty.Stringify(AnimationFlags));

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
End;*)

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
    _VisibleFrame := Engine.Graphics.FrameID;
    Self.TriggerEvent(widgetEvent_Show);
  End Else
  Begin
    Self.TriggerEvent(widgetEvent_Hide);
  End;
End;

Procedure UIWidget.SetRelativePosition(Const Pos:Vector2D);
Begin
(*  If (Pos.X = Self.Left.Value) And (Pos.Y = _Top.Value) Then
    Exit;*)

  Self.Left := UIPixels(Pos.X);
  Self.Top := UIPixels(Pos.Y);
  _TransformChanged := True;
End;

(*Procedure UIWidget.SetAbsolutePosition(Pos:Vector2D);
Begin
  If Assigned(_Parent) Then
    Pos.Subtract(_Parent.AbsolutePosition);

  Self.SetRelativePosition(Pos);
End;*)

Procedure UIWidget.SetLayer(Z:Single);
Begin
  If (Z = _Layer.Value) Then
    Exit;

  _Layer.Value := Z;
End;

Procedure UIWidget.SetColor(Const Value:ColorRGBA);
Begin
  If (Cardinal(Value) = Cardinal(_Color.Value)) Then
    Exit;

  _Color.Value := Value;
End;

Procedure UIWidget.SetGlow(Const Value:ColorRGBA);
Begin
  If (Cardinal(Value) = Cardinal(_Glow.Value)) Then
    Exit;

  _Glow.Value := Value;
End;

Function UIWidget.GetColor:ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
	Result := Self._Color.Value;

	If (Assigned(_Parent)) Then
		Result := ColorMultiply(Result, Parent.GetColor());
End;

Function UIWidget.GetGlow:ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  ParentColor:ColorRGBA;
Begin
	Result := _Glow.Value;

	If (Assigned(_Parent)) Then
		Result := ColorMultiply(Result, _Parent.GetGlow());
End;

Function UIWidget.GetRelativePosition:Vector2D;
Begin
  Result.X := Self.GetDimension(Self.Left, uiDimensionLeft);
  Result.Y := Self.GetDimension(Self.Top, uiDimensionTop);
End;

Function UIWidget.GetAlignedPosition:Vector2D;
Var
  Width, Height:Single;
  ParentSize, ParentMargin, Center:Vector2D;
Begin
  Result := Self.RelativePosition;

  If (Parent = Nil) Then
    Exit;

  //ParentMargin := Vector2D_Create(Parent.GetDimension(Parent.Margin, uiDimensionWidth), Parent.GetDimension(Parent.Margin, uiDimensionHeight));

  If (Align = UIAlign_TopLeft) Then
  Begin
    Result.X := Result.X; // + ParentMargin.X;
    Result.Y := Result.Y; // + ParentMargin.Y;
    Exit;
  End;

  Width := Self.CurrentSize.X;
  Height := CurrentSize.Y;

  ParentSize := _Parent.CurrentSize;

//  Center.X := ParentMargin.X + ((ParentSize.X - ParentMargin.X * 2.0) * 0.5);
//  Center.Y := ParentMargin.Y + ((ParentSize.Y - ParentMargin.Y * 2.0) * 0.5);
  Center.X := (ParentSize.X* 0.5);
  Center.Y := (ParentSize.Y* 0.5);

  Case Align Of
    UIAlign_Center:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

    UIAlign_TopCenter:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
      End;

    UIAlign_TopRight:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
      End;

    UIAlign_LeftCenter:
      Begin
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

    UIAlign_RightCenter:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
        Result.Y := Result.Y + Center.Y - Height * 0.5;
      End;

    UIAlign_BottomLeft:
      Begin
        Result.Y := (ParentSize.Y - Height) - Result.Y;
      End;

    UIAlign_BottomCenter:
      Begin
        Result.X := Result.X + Center.X - Width * 0.5;
        Result.Y := (ParentSize.Y - Height) - Result.Y;
      End;

    UIAlign_BottomRight:
      Begin
        Result.X := (ParentSize.X - Width) - Result.X;
        Result.Y := (ParentSize.Y - Height) - Result.Y;
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

Function UIWidget.OnHandleKeyDown(Key:Word):Boolean;
Begin
  RemoveHint(Key);
	Result := False;
End;

Function UIWidget.OnHandleKeyUp(Key:Word):Boolean;
Var
  I:Integer;
Begin
  If Self.Hidden Then
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

Function UIWidget.OnHandleKeyPress(Key:TERRAChar):Boolean;
Begin
	Result := False;
End;

Function UIWidget.OnRegion(Const X,Y:Single): Boolean;
Var
  V:Vector2D;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X:'+ IntegerProperty.Stringify(X)+' Y:'+ IntegerProperty.Stringify(Y));{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.OnRegion called');{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X1:'+ IntegerProperty.Stringify(Trunc(_Corners[0].X))+' Y1:'+ IntegerProperty.Stringify(Trunc(_Corners[0].Y)));{$ENDIF}
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'X2:'+ IntegerProperty.Stringify(Trunc(_Corners[2].X))+' Y2:'+ IntegerProperty.Stringify(Trunc(_Corners[2].Y)));{$ENDIF}

  If (Engine.Graphics.FrameID = Self._VisibleFrame) {Or (OutsideClipRect(X,Y))} Then
  Begin
    Result := False;
    {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Cliprect clipped!');{$ENDIF}
    Exit;
  End;

  V.X := X;
  V.Y := Y;
  Self.ConvertGlobalToLocal(V);

  //DrawPoint2D(UIManager.Instance.Viewport, V, ColorYellow, 4);

  V.Subtract(Self.GetScrollOffset);

  Result := (V.X>=0.0) And (V.X <= CurrentSize.X) And (V.Y >= 0) And (V.Y <= CurrentSize.Y);
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', 'Region result for '+_Name+' was '+BoolToString(Result));{$ENDIF}
End;

Function UIWidget.AllowsEvents(): Boolean;
Var
  UI:UIView;
Begin
  If (Self.Hidden) Then
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

Procedure UIWidget.PickAt(Const X, Y:Integer; Const EventClass:WidgetEventClass; Var CurrentPick:UIWidget; Var Max:Single; Ignore:UIWidget);
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_GUI}Log(logDebug, 'UI', _Name+ '.PickAt called');{$ENDIF}

  If (Self.Layer < Max) Or (Not Self.OnRegion(X,Y)) Or (Self = Ignore) Or (Self.Hidden) Then
    Exit;

  If (Self.ReactsToEventClass(EventClass)) Then
  Begin
    CurrentPick := Self;
    Self.ReactsToEventClass(EventClass);
    Max := Self.Layer;
  End;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].AllowsEvents()) Then
  Begin
    _ChildrenList[I].PickAt(X, Y, EventClass, CurrentPick, Max, Ignore);
  End;
End;

Function UIWidget.BeginDrag(X,Y:Integer; Mode:UIDragMode):Boolean;
Var
  UI:UIView;
Begin
  Result := False;

  UI := UIView(Self.GetUIView);
  If UI = Nil Then
    Exit;

  If (Not Self.SupportDrag(Mode)) Then
  Begin
    If (Assigned(Self.Parent)) And (Self.Parent Is UIInstancedWidget) Then
      Result := Self.Parent.BeginDrag(X, Y, Mode);

    Exit;
  End;

  Self.CallEventHandler(widgetEvent_DragBegin);

  UI.Dragger := Self;
  _DragMode := Mode;
  _Dragging := True;

  _DragSize := CurrentSize;
  _DragScroll := Self._CurrentScroll;
  _DragStartLeft := Self.Left;
  _DragStartTop := Self.Top;
  _DragStart := Self.RelativePosition;

  _DragX := (X- _DragStart.X);
  _DragY := (Y- _DragStart.Y);

  Result := True;
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
    Self.Left := _DragStartLeft;
    Self.Top := _DragStartTop;
    Self._CurrentScroll := _DragScroll;
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

Procedure UIWidget.OnHandleMouseUp(X,Y:Integer;Button:Word);
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
        Self.Left := UIPixels(UISnap(PX));
        Self.Top := UIPixels(UISnap(PY));
      End;

    UIDrag_Left:
      Begin
        Extra := AdjustWidth(_DragSize.X + (_DragStart.X - PX));
        Self.Left := UIPixels(PX + Extra);
        Self.UpdateRects();
      End;

    UIDrag_Top:
      Begin
        Extra := AdjustHeight(_DragSize.Y + (_DragStart.Y - PY));
        Self.Top := UIPixels(PY + Extra);
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

    UIDrag_Scroll:
      Begin
        Self.SetScrollOffset(Vector2D_Subtract(Self._DragScroll, Vector2D_Create(PX, PY)));
        Self.UpdateRects();
      End;

    End;
End;

Procedure UIWidget.OnHandleMouseMove(X,Y:Integer);
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

Procedure UIWidget.SetScrollOffset(Const Ofs:Vector2D);
Begin
  _CurrentScroll := Ofs;

  If (_CurrentScroll.X < 0) Then
    _CurrentScroll.X := 0
  Else
  If (_CurrentScroll.X > Self.ScrollLimits.X) Then
    _CurrentScroll.X := Self.ScrollLimits.X;

  If (_CurrentScroll.Y < 0) Then
    _CurrentScroll.Y := 0
  Else
  If (_CurrentScroll.Y > Self.ScrollLimits.Y) Then
    _CurrentScroll.Y := Self.ScrollLimits.Y;
End;

Procedure UIWidget.OnHandleMouseWheel(X,Y:Integer; Delta:Single);
Var
  ScrollValue:Single;
Begin
  ScrollValue := 5;
  SetScrollOffset(Vector2D_Add(_CurrentScroll, Vector2D_Create(0, -ScrollValue * Delta)));

  If (Delta<0) Then
    Self.TriggerEvent(widgetEvent_ScrollDown)
  Else
    Self.TriggerEvent(widgetEvent_ScrollUp);
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
  W,H:Single;

  Mat:Matrix3x3;
Begin
  Result := False;

  (*If (_NeedsHide) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.HasPropertyTweens();
    _NeedsHide := False;
    HideWidget(Self);
  End;*)

(*  If (Not _TransformChanged) Then
    Exit;*)

  _TransformChanged := False;

  Self.UpdateRects();

  Center := Self.CurrentSize;
  Center.Scale(Self.Pivot);

  (*If _Rotation.Value<>0 Then
    _Rotation.Value := 1.0+RAD*(Trunc(Application.GetTime()/10));*)

  If (_Rotation.Value <> 0.0) Then
    Mat := Matrix3x3_RotationAndScale(_Rotation.Value, _Scale.Value, _Scale.Value)
  Else
    Mat := Matrix3x3_Scale(_Scale.Value, _Scale.Value);

  Mat := Matrix3x3_TransformAroundPoint(Center, Mat);

  Pos := Self.GetAlignedPosition();
  Pos.Subtract(Self.GetScrollOffset());

  Mat := Matrix3x3_Multiply(Matrix3x3_Translation(Pos), Mat);

  Self.SetTransform(Mat);
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
    _Transform := Matrix3x3_Multiply(Parent._Transform, _Transform);

  _InverseTransform := Matrix3x3_Inverse(_Transform);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) Then
  Begin
    _ChildrenList[I]._TransformChanged := True;
    _ChildrenList[I].UpdateTransform();
  End;

  Self.UpdateRects();
  _TransformChanged := True;
End;

Function UIWidget.GetChildByIndex(Index:Integer):UIWidget;
Begin
  If (Index>=0) And (Index<=Self.ChildrenCount) Then
    Result := _ChildrenList[Index]
  Else
    Result := Nil;
End;

Function UIWidget.GetChildByName(Const Name:TERRAString; ID:Integer):UIWidget;
Var      
  I:Integer;
Begin
  For I:=0 To Pred(Self._ChildrenCount) Do
  If (StringEquals(_ChildrenList[I].Name, Name)) And (_ChildrenList[I].ID = ID) Then
  Begin
    Result := _ChildrenList[I];
    Exit;
  End;

  For I:=0 To Pred(_ChildrenCount) Do
  Begin
    Result := _ChildrenList[I].GetChildByName(Name, ID);
    If Assigned(Result) Then
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
Var
  I, TargetID:Integer;
Begin
  If (W=Nil) Or (W = Self) Then
    Exit;

  TargetID := 1;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I] = W) Then
  Begin
    Exit;
  End Else
  If (StringEquals(_ChildrenList[I].Name, W.Name)) Then
  Begin
    Inc(TargetID);
  End;


  W._Parent := Self;
  W._Index := TargetID;
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

Procedure UIWidget.Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal);
Var
  I:Integer;
Begin
  If (Self._Deleted) Then
    Exit;

//  Application.Sleep(100);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].CanRender()) Then
    _ChildrenList[I].Render(View, Stage, Bucket);
                                
  Self.UpdateProperties();
  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateSprite(View);

  If Assigned(_Sprite) Then
  Begin
    View.SpriteRenderer.QueueSprite(_Sprite);
    //DrawClipRect(View, Self.ClipRect, ColorRed);
  End;


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
  If Assigned(_Parent) Then
    Result := _Parent._CurrentScroll
  Else
    Result := Vector2D_Create(0.0, 0.0);

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

(*Procedure UIWidget.SetPositionRelativeToOther(Other:UIWidget; OfsX, OfsY: Single);
Var
  P:Vector2D;
Begin
  P := Other.AbsolutePosition;

  If (Other.Parent<>Nil) Then
    P.Subtract(Other.Parent.AbsolutePosition);

  P.Add(Vector2D_Create(OfsX, OfsY));

  Self.SetRelativePosition(P);
End;*)

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
  StringGetNextSplit(S, '_');
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
    Result := Engine.Localization.GetString(Macro);
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
    Macro := StringCopy(Value, 2, MaxInt);
    Result := DataSourceManager.Instance.GetValueFromPath(Macro);
    Exit;
  End Else
  Begin
    Result := Value;
    Exit;
  End;

  If Assigned(Self.Parent) Then
    Result := Self.Parent.ResolveMacro(Value);
End;

(*Procedure UIWidget.SetChildrenVisibilityByTag(Tag: Integer; Visibility: Boolean);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (Assigned(_ChildrenList[I])) And (_ChildrenList[I].Tag = Tag) Then
    _ChildrenList[I].Visible := Visibility;
End;*)

Function UIWidget.CanRender: Boolean;
Var
  CurrentFrameID:Cardinal;
Begin
  Result := False;

  If (Not Self.Visible) Then
    Exit;

  If (Self.Color.A<=0) Then
    Exit;

(*  CurrentFrameID := Engine.Graphics.FrameID;
  If (_RenderFrameID = CurrentFrameID) Then
    Exit;

  _RenderFrameID := CurrentFrameID;*)
  Result := True;
End;

Function UIWidget.OnCustomRegion(Const X, Y:Integer; X1, Y1, X2, Y2:Single): Boolean;
Var
  I:Integer;
  Pos:Vector2D;
  P:Array[0..3] Of Vector2D;
Begin
  Pos := Self.GetAbsolutePosition();
  P[0] := Vector2D_Create(X1, Y1);
  P[1] := Vector2D_Create(X2, Y1);
  P[2] := Vector2D_Create(X2, Y2);
  P[3] := Vector2D_Create(X1, Y2);

  For I:=0 To 3 Do
  Begin
    P[I].Add(Pos);
    P[I] := _Transform.Transform(P[I]);
  End;

  Result := (X>= P[0].X) And (X <= P[2].X) And (Y >= P[0].Y) And (Y <= P[2].Y);
End;

(*Function UIWidget.IsOutsideScreen: Boolean;
Var
  P:Vector2D;
Begin
  P := Self.AbsolutePosition;
  Result := (P.X + Self.Size.X<0) Or (P.Y + Self.Size.Y<0) Or (P.X> View.Width) Or (P.Y > View.Height);
End;*)

Function UIWidget.GetDimension(Const Dim: UIDimension; Const Target:UIDimensionTarget): Single;
Var
  Percent:Single;
  TargetSize, ParentSize:Vector2D;
Begin
  If Dim.IsPercent Then
  Begin
    Percent := (Dim.Value * 0.01);
    If Assigned(Parent) Then
      ParentSize := Parent.CurrentSize
    Else
      ParentSize := Vector2D_Create(Application.Instance.Width, Application.Instance.Height);

    TargetSize := Vector2D_Scale(ParentSize, Percent);

    Case Target Of
      uiDimensionHorizontal:
        Result := TargetSize.X;

      uiDimensionVertical:
        Result := TargetSize.Y;

      uiDimensionLeft:
        Result := (TargetSize.X) + Self.GetDimension(Self.Margin.Left, uiDimensionHorizontal);

      uiDimensionTop:
        Result := (TargetSize.Y) + Self.GetDimension(Self.Margin.Top, uiDimensionVertical);

      uiDimensionWidth:
        Result := (TargetSize.X) - ( Self.GetDimension(Self.Margin.Left, uiDimensionHorizontal) + Self.GetDimension(Self.Margin.Right, uiDimensionHorizontal));

      uiDimensionHeight:
        Result := (TargetSize.Y) - (Self.GetDimension(Self.Margin.Top, uiDimensionVertical) + Self.GetDimension(Self.Margin.Bottom, uiDimensionVertical));
    End;

  End Else
  Begin
    Case Target Of
      uiDimensionHorizontal,
      uiDimensionVertical:
        Result := Dim.Value;

      uiDimensionLeft:
        Result := Dim.Value + Self.GetDimension(Self.Margin.Left, uiDimensionHorizontal);

      uiDimensionTop:
        Result := Dim.Value + Self.GetDimension(Self.Margin.Top, uiDimensionVertical);

      uiDimensionWidth:
        Result := Dim.Value - ( Self.GetDimension(Self.Margin.Left, uiDimensionHorizontal) + Self.GetDimension(Self.Margin.Right, uiDimensionHorizontal));

      uiDimensionHeight:
        Result := Dim.Value - (Self.GetDimension(Self.Margin.Top, uiDimensionVertical) + Self.GetDimension(Self.Margin.Bottom, uiDimensionVertical));
    End;
  End;

  (*If (Assigned(Parent)) Then
    Result := Result - Parent.GetDimension(Parent.Margin, Target) * 2;*)
End;

Function UIWidget.GetWidth: UIDimension;
Begin
  Result := _Width.Value;
End;

Function UIWidget.GetHeight: UIDimension;
Begin
  Result := _Height.Value;
End;

Function UIWidget.GetPosition_Left: UIDimension;
Begin
  Result := Self._X.Value;
End;

Function UIWidget.GetPosition_Top: UIDimension;
Begin
  Result := Self._Y.Value;
End;

Procedure UIWidget.SetPosition_Left(const Value: UIDimension);
Begin
  Self._X.Value := Value;
End;

Procedure UIWidget.SetPosition_Top(const Value: UIDimension);
Begin
  Self._Y.Value := Value;
End;

(*Function UIWidget.GetMargin: UIDimension;
Begin
  Result := _Margin.Value;
End;*)

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

(*Procedure UIWidget.SetMargin(const Value: UIDimension);
Begin
  Self._Margin.Value := Value;
  Self.UpdateRects();
End;*)

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
    P := (NewWidth / View.GetDimension(Width, uiDimensionWidth)) * 100;
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
    P := (NewHeight / View.GetDimension(Height, uiDimensionHeight)) * 100;
    _Height.Value := UIPercent(P);
  End Else
    _Height.Value := UIPixels(NewHeight);
End;

Function UIWidget.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Case Mode Of
  UIDrag_Move:  Result := Self.Draggable;

  UIDrag_Scroll: Result := Self.Scrollable;

  Else
    Result := False;
  End;
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


Function UIWidget.GetAlign:UIAlign;
Begin
  Result := UIAlign(_Align.Value);
End;

Procedure UIWidget.SetAlign(const Value:UIAlign);
Begin
  _Align.Value := Integer(Value);
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

  Self.Left := UIPixels(P.X);
  Self.Top := UIPixels(P.Y);

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

Procedure UIWidget.SetEventHandler(EventType: WidgetEventType; Handler:UIWidgetEventHandler);
Begin
  _Handlers[EventType] := Handler;
End;

Function UIWidget.GetEventHandler(EventType:WidgetEventType):UIWidgetEventHandler;
Begin
  Result := _Handlers[EventType];

(*  If (Assigned(Result)) Or (_Parent = Nil) Then
    Exit;

  Result := _Parent.GetEventHandler(EventType);*)
End;

Function UIWidget.CallEventHandler(EventType:WidgetEventType):Boolean;
Var
  Handler:UIWidgetEventHandler;
Begin
  Handler := Self.GetEventHandler(EventType);
  Result := Assigned(Handler);
  If Result Then
    Handler(Self);
End;

Function UIWidget.IsEnabled: Boolean;
Begin
  Result := (Self._State <> widget_Disabled);
End;

Procedure UIWidget.AddAnimation(State:WidgetState; const PropName, Value: TERRAString; Const Ease:TweenEaseType = easeLinear);
Var
  I, N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_AnimationCount) Do
  If (_Animations[I].State = State) And (StringEquals(_Animations[I].PropName, PropName)) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _AnimationCount;
    Inc(_AnimationCount);
    SetLength(_Animations, _AnimationCount);
    _Animations[N].State := State;
    _Animations[N].PropName := PropName;
  End;

  _Animations[N].Value := Value;
  _Animations[N].Ease := Ease;
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
  Prop := Self.FindPropertyWithPath(PropName);
  If Assigned(Prop) Then
    Prop.SetBlob(Value);
End;

Procedure UIWidget.SetSelected(const Value: Boolean);
Begin
  If (Self.Selected = Value) Then
    Exit;

  If (Value) Then
    Self.TriggerEvent(widgetEvent_FocusBegin)
  Else
    Self.TriggerEvent(widgetEvent_FocusEnd);
End;

function UIWidget.GetRenderBucket: Cardinal;
Begin
  Result := renderBucket_Overlay;
End;

Function UIWidget.GetState: WidgetState;
Begin
  If (Self.Visible) Then
    Result := _State
  Else
    Result := widget_Hidden;
End;

function UIWidget.Hidden: Boolean;
begin
  Result := (_State = widget_Hidden);
end;


Procedure UIWidget.SetClipRect(const Value: TERRAClipRect);
Begin
  If (Value.Style = clipSomething) Then
  Begin
    Self._CustomClip := True;
    Self._ClipRect := Value;
  End Else
    Self._CustomClip := False;
End;

Procedure UIWidget.ConvertAlign(const Direction: UIDirection);
Begin
  Case Direction Of
    UIDirection_Vertical:
    Case Self.Align Of
      UIAlign_TopLeft,
      UIAlign_TopRight:
        Self.Align := UIAlign_TopCenter;

      UIAlign_LeftCenter,
      UIAlign_RightCenter:
        Self.Align := UIAlign_Center;

      UIAlign_BottomLeft,
      UIAlign_BottomRight:
        Self.Align := UIAlign_BottomCenter;
    End;

    UIDirection_Horizontal:
    Case Self.Align Of
      UIAlign_TopCenter,
      UIAlign_TopRight:
        Self.Align := UIAlign_TopLeft;

      UIAlign_Center,
      UIAlign_RightCenter:
        Self.Align := UIAlign_LeftCenter;

      UIAlign_BottomCenter,
      UIAlign_BottomRight:
        Self.Align := UIAlign_BottomLeft;
    End;

  End;
End;

Function UIWidget.OnChildrenRegion(const X, Y: Single): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].OnRegion(X, Y)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function UIWidget.GetVisible: Boolean;
Begin
  Result := _Visible.Value;
End;


Function UIWidget.HasAnimation(State:WidgetState):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AnimationCount) Do
  If (_Animations[I].State = State) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function UIWidget.HasEvent(EventType:WidgetEventType):Boolean;
Begin
  Result := (Assigned(Self.GetEventHandler(EventType)));
End;

Function UIWidget.ReactsToEventClass(Const EventClass: WidgetEventClass): Boolean;
Begin
  Case EventClass Of
    widgetEventClass_Any:
      Result := True;

    widgetEventClass_Visibility:
      Result := (HasEvent(widgetEvent_Show)) Or (HasEvent(widgetEvent_Hide)) Or (HasAnimation(widget_Hidden));

    widgetEventClass_Click:
      Result := (HasEvent(widgetEvent_MouseDown) Or HasEvent(widgetEvent_MouseUp)) Or (Draggable) Or (Self.Scrollable);

    widgetEventClass_Scroll:
      Result := (HasEvent(widgetEvent_ScrollDown)) Or (HasEvent(widgetEvent_ScrollUp)) Or (Self.ScrollLimits.X>0) Or (Self.ScrollLimits.Y>0);

    widgetEventClass_Hover:
      Result := (Not Self.Selected) And ((HasEvent(widgetEvent_MouseOut)) Or (HasEvent(widgetEvent_MouseOver))  Or (HasAnimation(widget_Highlighted)));

    widgetEventClass_Drag:
      Result := (HasEvent(widgetEvent_DragEnd)) Or (HasEvent(widgetEvent_DragBegin)) Or (HasAnimation(widget_Dragged));

    widgetEventClass_Focus:
      Result := (HasEvent(widgetEvent_FocusEnd) Or HasEvent(widgetEvent_FocusBegin)) Or (HasAnimation(widget_Selected));

    widgetEventClass_Content:
      Result := (HasEvent(widgetEvent_ContentChange));

    Else
      Result := False;
  End;
End;

Function UIWidget.IsSelected: Boolean;
Begin
  Result := (Self.State = widget_Selected);
End;

Function UIWidget.IsScrollable: Boolean;
Begin
  Result := (Self.ScrollLimits.X>0) Or (Self.ScrollLimits.Y>0);
End;

Function UIWidget.GetCurrentCursor: TERRACursorType;
Begin
  Result := _CurrentCursor;

  If Result<>Cursor_Default Then
    Exit;

  If (SupportDrag(UIDrag_Scroll)) Then
    Result := Cursor_Move
  Else
  If (Assigned(_Parent)) Then
    Result := _Parent.GetCurrentCursor;
End;

{ UIWidgetGroup }
Constructor UIWidgetGroup.Create(Const Name:TERRAString; Parent:UIWidget;  Const X,Y:UIDimension; Const Layer:Single; const Width, Height: UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;
  Self.Width := Width;
  Self.Height := Height;

  Self.Layout := UILayout_Free;
//  Self.Template := TemplateName;
End;

Procedure UIWidgetGroup.InitProperties(const Name: TERRAString);
Begin
  Inherited;

  Self._Layout := EnumProperty(Self.AddProperty(EnumProperty.Create('layout', 0, _LayoutEnums), False));
  Self._Padding := DimensionProperty(Self.AddProperty(DimensionProperty.Create('padding', UIPercent(100)), False));
End;

Function UIWidgetGroup.GetLayout: UILayout;
Begin
  Result := UILayout(_Layout.Value);
End;

Function UIWidgetGroup.GetPadding: UIDimension;
Begin
  Result := _Padding.Value;
End;

Procedure UIWidgetGroup.SetLayout(const Value: UILayout);
Begin
  _Layout.Value := Integer(Value);
End;

Procedure UIWidgetGroup.SetPadding(const Value: UIDimension);
Begin
  _Padding.Value := Value;
End;

Function UIWidgetGroup.UpdateTransform: Boolean;
Var
   TotalVisible, I:Integer;
  Temp:Vector2D;
  TotalSize, PadSize, SepSize, CurrentPos:Single;
Begin
  //If (_TransformChanged) Then
  Begin
    CurrentPos := 0.0;
    Case Self.Layout Of
    UILayout_Horizontal:
      Begin
        TotalSize := 0;
        TotalVisible := 0;

        For I:=0 To Pred(_ChildrenCount) Do
        If (_ChildrenList[I].CanRender()) Then
        Begin
          Inc(TotalVisible);
          TotalSize := TotalSize + _ChildrenList[I].CurrentSize.X;
        End;

        If (TotalVisible>0) Then
        Begin
          PadSize := Self.CurrentSize.X - TotalSize;

          If (Self.Padding.IsPercent) Then
          Begin
            SepSize := PadSize / Succ(TotalVisible);
            SepSize := SepSize * Self.Padding.Value * 0.01;
          End Else
            SepSize := Self.Padding.Value;

          CurrentPos := SepSize;

          For I:=0 To Pred(_ChildrenCount) Do
          If (_ChildrenList[I].CanRender()) Then
          Begin
            _ChildrenList[I].ConvertAlign(UIDirection_Horizontal);
            _ChildrenList[I].Left := UIPixels(CurrentPos);

            CurrentPos := CurrentPos + SepSize + _ChildrenList[I].CurrentSize.X;
          End;

        End;

      End;
    End;
  End;

  Result := Inherited UpdateTransform;
End;

(*Function UIWidgetGroup.GetTemplate: TERRAString;
Begin
  Result := _Template.Value;
End;

Procedure UIWidgetGroup.SetTemplate(const Value: TERRAString);
Begin
  _Template.Value := Value;
End;*)

Procedure UIWidgetGroup.Render(View: TERRAViewport; Const Stage: RendererStage; const Bucket: Cardinal);
Var
  I:Integer;
Begin
(*  If (Self.ChildrenCount<=0) And (Self.Template<>'') Then
  Begin
  End;*)

  Inherited;
End;

Function UIWidgetGroup.OnRegion(const X, Y: Single): Boolean;
Begin
  Result := Self.OnChildrenRegion(X, Y);
End;

{ UIInstancedWidget }
Constructor UIInstancedWidget.Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; const Width, Height: UIDimension; Const TemplateName:TERRAString);
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
    Self.CopyProperties(Template);

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

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;
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
  S:TERRAString;
  Temp:UIWidget;
Begin
  Result := Nil;

  If Template = Nil Then
    Exit;

  If (Template Is UIEditText) Then
  Begin
    Result := UIEditText.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100), '');
  End Else
  If (Template Is UILabel) Then
  Begin
    Result := UILabel.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100), '??');
  End Else
  If (Template Is UITiledRect) Then
  Begin
    Result := UITiledRect.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100), 0, 0, 1, 1);
  End Else
  If (Template Is UIImage) Then
  Begin
    Result := UIImage.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100));
  End Else
  If (Template Is UIWidgetGroup) Then
  Begin
    Result := UIWidgetGroup.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100));
  End Else
  If (Template Is UIInstancedWidget) Then
  Begin
    Result := UIInstancedWidget.Create(Template.Name, Parent, UIPixels(0), UIPixels(0), 0, UIPixels(100), UIPixels(100), Template.Name);
  End Else
  Begin
    If (Not (Template Is UIInstancedWidget)) Then
    Begin
      S := Template.ClassName;
      Log(logError, 'UI', 'Cannot instanciate template component of type '+S);
      Exit;
    End;
  End;

  If Assigned(Result) Then
    Result.CopyProperties(Template)
  Else
    Result := Parent;

  For I:=0 To Pred(Template.ChildrenCount) Do
  Begin
    Temp := InitFromTemplate(Template.GetChildByIndex(I), Result);

    (*If Result = Parent Then
    Begin
      Temp.CopyProperties(Template);
    End;*)

  End;
End;

Function UIInstancedWidget.OnRegion(Const X, Y: Single): Boolean;
Begin
  Result := Self.OnChildrenRegion(X, Y);
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
    _Templates := TERRAList.Create();

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
      It.Discard();
      Break;
    End;
  End;
  ReleaseObject(It);
End;


{ UIScrollArea }
Function UIScrollArea.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := (Mode = UIDrag_Scroll);
End;

Initialization
  _AlignEnums := EnumCollection.Create();
  _AlignEnums.Add('Top.Left', Integer(UIAlign_TopLeft));
  _AlignEnums.Add('Top.Center', Integer(UIAlign_TopCenter));
  _AlignEnums.Add('Top.Right', Integer(UIAlign_TopRight));
  _AlignEnums.Add('Left.Center', Integer(UIAlign_LeftCenter));
  _AlignEnums.Add('Center', Integer(UIAlign_Center));
  _AlignEnums.Add('Right.Center', Integer(UIAlign_RightCenter));
  _AlignEnums.Add('Bottom.Left', Integer(UIAlign_BottomLeft));
  _AlignEnums.Add('Bottom.Center', Integer(UIAlign_BottomCenter));
  _AlignEnums.Add('Bottom.Right', Integer(UIAlign_BottomRight));

  _DirectionEnums := EnumCollection.Create();
  _DirectionEnums.Add('Vertical', Integer(UIDirection_Vertical));
  _DirectionEnums.Add('Horizontal', Integer(UIDirection_Horizontal));

  _LayoutEnums := EnumCollection.Create();
  _LayoutEnums.Add('Free', Integer(UILayout_Free));
  _LayoutEnums.Add('Horizontal', Integer(UILayout_Horizontal));
  _LayoutEnums.Add('Vertical', Integer(UILayout_Vertical));

Finalization
  ReleaseObject(_AlignEnums);
  ReleaseObject(_DirectionEnums);
  ReleaseObject(_LayoutEnums);
End.
