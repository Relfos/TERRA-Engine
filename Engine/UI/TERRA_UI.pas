{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_UI
 * Implements the UI class
 ***********************************************************************************************************************
}
Unit TERRA_UI;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Font, TERRA_Collections, TERRA_Image, TERRA_Utils, TERRA_TextureAtlas, TERRA_Application,
  TERRA_Vector3D, TERRA_Vector2D, TERRA_Matrix3x3, TERRA_Color, TERRA_Texture, TERRA_Math, TERRA_Tween,
  TERRA_SpriteManager, TERRA_Vector4D, TERRA_GraphicsManager, TERRA_FontRenderer, TERRA_UITransition, TERRA_Viewport,
  TERRA_UISkin, TERRA_UIDimension, TERRA_ClipRect, TERRA_EnumProperty, TERRA_Hashmap;

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

  TextureAtlasWidth = 1024;
  TextureAtlasHeight = 512;

Type
  TERRAUI = Class;

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

  Widget = Class;
  WidgetClass = Class Of Widget;
  WidgetEventHandler = Procedure(Src:Widget) Of Object;

	Widget = Class(CollectionObject)
    Private
      _UI:TERRAUI;
      _Next:Widget;
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

      _Deleted:Boolean;

      _SkinComponent:UISkinComponent;

      Procedure UpdateProperties();

			Function GetAbsolutePosition:Vector2D;
      Function GetRelativePosition:Vector2D;

      Function GetHeight: UIDimension;
      Function GetWidth: UIDimension;

      Function GetTabIndex: Integer;
      Procedure SetTabIndex(const Value: Integer);
      Procedure SetParent(Target:Widget);

		Protected
			_Parent:Widget;

      _Tooltip:TERRAString;
      _NeedsUpdate:Boolean;
      _NeedsHide:Boolean;

      _MouseOver:Boolean;

      _Enabled:Boolean;

      _TabControl:Widget;

      _Dragging: Boolean;
      _DragMode:UIDragMode;
      _DragX: Single;
      _DragY: Single;
      _DragSize:Vector2D;
      _DragStart:Vector2D;

      _ChildrenList:Array Of Widget;
      _ChildrenCount:Integer;

      _FontRenderer:FontRenderer;

      _Transform:Matrix3x3;
      _InverseTransform:Matrix3x3;
      _TransformChanged:Boolean;

      _Properties:UISkinProperty;
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
      _Font:TERRAFont;

      _ClipRect:ClipRect;

      _Scroll:Widget;
      _ScrollValue:Single;

      _VisibleFrame:Cardinal;

      _DropShadowColor:Color;

      _HighlightGroup:Integer;

      _BasePropertiesIndex:Integer;
      _CustomPropertiesIndex:Integer;

      Procedure InitProperties();
      Procedure ExpandProperties(Count:Integer);

      Procedure CopyValue(Other:CollectionObject); Override;
      Function Sort(Other:CollectionObject):Integer; Override;

      Function GetUpControl():Widget;
      Function GetDownControl():Widget;
      Function GetLeftControl():Widget;
      Function GetRightControl():Widget;

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
      Procedure SetFont(const Value:TERRAFont);

      Function GetRotation():Single;
      Function GetScale():Single;

      Function GetTabControl():Widget;

      Procedure UpdateHighlight(); Overload;
      Procedure UpdateHighlight(Condition:Boolean); Overload;

      Procedure ClearProperties();
      Procedure CopyProperties(ID:Integer; Selected:Boolean; Out CurrentState, DefaultState:Integer);

      Function IsSelected: Boolean;

			Function OnKeyDown(Key:Word):Boolean;Virtual;
			Function OnKeyUp(Key:Word):Boolean;Virtual;
			Function OnKeyPress(Key:Word):Boolean;Virtual;

      Function AllowsEvents(): Boolean;

      Function AdjustWidth(NewWidth:Single):Single;
      Function AdjustHeight(NewHeight:Single):Single;

      Procedure PickAt(Const X, Y:Integer; Var CurrentPick:Widget; Var Max:Single; Ignore:Widget = Nil);

      Function HasMouseOver():Boolean; Virtual;

      Procedure OnHit(Handler:WidgetEventHandler); Virtual;
      Procedure OnHighlight(Prev:Widget); Virtual;
      Procedure StartHighlight(); Virtual;
      Procedure StopHighlight(); Virtual;

      Function IsSelectable():Boolean; Virtual;
      Function CanHighlight(GroupID:Integer):Boolean;

      Function GetFontRenderer: FontRenderer;

      Function OutsideClipRect(X,Y:Single):Boolean;

      Function CanRender():Boolean;

      Procedure ResetClipRect();

      Procedure SetEnabled(Value:Boolean);

      Procedure DrawText(Const Text:TERRAString; Const X,Y, Layer:Single; Const TextRect:Vector2D; Scale:Single; ID:Integer; Selected:Boolean; Const TextColor:Color);
      Procedure DrawComponent(X, Y, Layer:Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean = True);
      Procedure DrawCroppedComponent(X, Y, Layer, U1, V1, U2, V2:Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean = True);

      Procedure SetObjectName(const Value: TERRAString); Override;

      Procedure ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);

      Property SkinComponent:UISkinComponent Read _SkinComponent;

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

      Constructor Create(Const Name:TERRAString; Parent:Widget; Const ComponentName:TERRAString);
      Procedure Release; Override;

      Procedure Delete();

      Procedure LoadSkin(Const ComponentName:TERRAString);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Override;

      Procedure Render; Virtual;

      Procedure UpdateRects; Virtual;
      Function UpdateTransform():Boolean; Virtual;

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

      Procedure SetPositionRelativeToOther(Other:Widget; OfsX, OfsY:Single);

      Procedure GetScrollOffset(Out OfsX, OfsY:Single);

			Procedure OnLanguageChange();Virtual;

      Procedure NullEventHandler(Src:Widget);

      Function OnRegion(X,Y:Single): Boolean; Virtual;
      Function OnCustomRegion(X,Y:Integer; X1,Y1,X2,Y2:Single):Boolean;

      Procedure AddChild(W:Widget);
      Procedure RemoveChild(W:Widget);
      Function GetChildByIndex(Index:Integer):Widget; Overload;
      Function GetChildByName(Const Name:TERRAString):Widget; Overload;
      Function GetChildByClass(ChildClass:WidgetClass; Index:Integer = 0):Widget; Overload;

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

      Function IsSameFamily(Other:Widget):Boolean;

      Function GetSize:Vector2D; Virtual;

      Function GetFont:TERRAFont;

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
      Property TabControl:Widget Read GetTabControl Write _TabControl;

      Property InheritColor:Boolean Read _InheritColor Write _InheritColor;

      Property Color:TERRA_Color.Color Read GetColor Write SetColor;
      Property ColorTable:TERRATexture Read GetColorTable Write _ColorTable;
      Property Saturation:Single Read GetSaturation Write SetSaturation;
      Property Rotation:Single Read GetRotation Write SetRotation;
      Property Scale:Single Read GetScale Write SetScale;
      Property Font:TERRAFont Read GetFont Write SetFont;

      Property Scroll:Widget Read _Scroll Write _Scroll;

      Property Parent:Widget Read _Parent Write SetParent;
      Property Align:Integer Read GetAlign Write SetAlign;

      Property ChildrenCount:Integer Read _ChildrenCount;

      Property ClipRect:ClipRect Read GetClipRect Write SetClipRect;

      Property Center:Vector2D Read _Center Write _Center;

      Property Enabled:Boolean  Read _Enabled Write SetEnabled;

      Property Selected:Boolean Read IsSelected;

      Property DropShadowColor:Color Read _DropShadowColor Write _DropShadowColor;

      Property HighlightGroup:Integer Read GetHighlightGroup Write _HighlightGroup;

      Property UI:TERRAUI Read _UI;
      Property Next:Widget Read _Next;
      Property FontRenderer:FontRenderer Read GetFontRenderer Write _FontRenderer;

      Property Width:UIDimension Read GetWidth Write SetWidth;
      Property Height:UIDimension Read GetHeight Write SetHeight;
	End;

  TERRAUI = Class(Widget)
    Protected
      _VirtualKeyboard:Widget;

		  _Focus:Widget;
      _Dragger:Widget;
      _Modal:Widget;
      _Highlight:Widget;
      _First:Widget;
      _Draw:Boolean;

      _Transition:UITransition;
      _Widgets:HashMap;

      _DefaultFont:TERRAFont;
      _Language:TERRAString;

      _WndCallback1:WidgetEventHandler;
      _WndCallback2:WidgetEventHandler;
      _PrevHighlight:Widget;

      _LastOver:Widget;
      _LastWidget:Widget;

      _InverseTransform:Matrix3x3;
      _ClipRect:ClipRect;

      _Skin:UISkinComponent;

      _HasDeletions:Boolean;

      Procedure UpdateLanguage();

      Procedure SetColorTable(const Value:TERRATexture);
      Procedure SetDefaultFont(const Value:TERRAFont);
      Procedure SetHighlight(const Value: Widget);
      Procedure SetDragger(const Value: Widget);

      Function GetHighlight:Widget;

      Procedure CloseWnd();
      Procedure InitStuff();

      Procedure OnOrientationChange;

      Function GetFontRenderer():FontRenderer;

      Function GetModal():Widget;

      Procedure InsertIntoTopWidgets(W:Widget);
      Procedure RemoveFromTopWidgets(W:Widget);

    Public
      CloseButton:Widget;

      Key_Up:Integer;
      Key_Down:Integer;
      Key_Right:Integer;
      Key_Left:Integer;
      Key_Action:Integer;
      Key_Cancel:Integer;

      System_Wnd:Widget;
      System_Text:Widget;
      System_Btn:Array[0..2] Of Widget;
      System_BG:Widget;

      Constructor Create;
      Procedure Release; Override;

      Procedure AddWidget(MyWidget:Widget);
      Procedure DeleteWidget(MyWidget:Widget);
      Function GetWidget(Const Name:TERRAString):Widget;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;
      Function GetObjectType:TERRAString; Override; 

      Function AddQuad(Const Quad:UIQuad; Const Props:UISkinProperty; Z:Single; Const Transform:Matrix3x3):QuadSprite;

      Procedure Clear;

      Function SelectNearestWidget(Target:Widget):Widget;
      Procedure GetFirstHighLight(GroupID:Integer);

      Function PickWidget(X,Y:Integer; Ignore:Widget = Nil):Widget;

		  Function OnKeyDown(Key:Word):Widget;
		  Function OnKeyUp(Key:Word):Widget;
		  Function OnKeyPress(Key:Word):Widget;

		  Function OnMouseDown(X,Y:Integer;Button:Word):Widget;
		  Function OnMouseUp(X,Y:Integer;Button:Word):Widget;
		  Function OnMouseWheel(X,Y:Integer; Delta:Integer):Widget;
		  Function OnMouseMove(X,Y:Integer):Widget;

      Procedure Render;

      Procedure AfterEffects;

      Function LoadSkin(Const FileName:TERRAString):Boolean;

      Function LoadImage(Name:TERRAString):TextureAtlasItem;
      Function GetComponent(Const Name:TERRAString):UISkinComponent;

      Procedure SetTransform(Const M:Matrix3x3);

      Procedure SetFocus(W:Widget);
      Procedure SetTransition(MyTransition:UITransition);

      Function GetVirtualKeyboard():Widget;

      Procedure SetFontRenderer(const Value: FontRenderer);

      Procedure MessageBox(Msg:TERRAString; Callback: WidgetEventHandler = Nil);
      Procedure ChoiceBox(Msg, Option1, Option2:TERRAString; Callback1:WidgetEventHandler = Nil; Callback2: WidgetEventHandler = Nil);
      Procedure ClearChoiceBox();
      Procedure ShowModal(W:Widget);
      Procedure InitTempWidgets();

      Function OnRegion(X, Y:Integer):Boolean;

      Property ColorTable:TERRATexture Read _ColorTable Write SetColorTable;
      Property Saturation:Single Read GetSaturation Write SetSaturation;

      Property Focus:Widget Read _Focus Write SetFocus;
      Property Dragger:Widget Read _Dragger Write SetDragger;

      Property Transition:UITransition Read _Transition Write SetTransition;

      Property VirtualKeyboard:Widget Read GetVirtualKeyboard;

      Property Widgets:HashMap Read _Widgets;

      Property LastWidget:Widget Read _LastWidget;

      Property DefaultFont:TERRAFont Read _DefaultFont Write SetDefaultFont;
      Property Modal:Widget Read GetModal Write _Modal;
      Property Highlight:Widget Read GetHighlight Write SetHighlight;

      Property Transform:Matrix3x3 Read _Transform Write SetTransform;

      Property FontRenderer:FontRenderer Read GetFontRenderer Write SetFontRenderer;

      Property ClipRect:ClipRect Read _ClipRect Write _ClipRect;

      Property First:Widget Read _First;
    End;

  UIManager = Class(ApplicationComponent)
    Protected
      _Viewport:TERRAViewport;
      _TextureAtlas:TextureAtlas;
      _UpdateTextureAtlas:Boolean;

      _UIList:Array Of TERRAUI;
      _UICount:Integer;

      _Ratio:Single;

      _FontRenderer:FontRenderer;

      _AlignEnums:EnumCollection;

      Procedure OnAppResize; Override;
      Procedure OnLanguageChange; Override;
      Procedure OnOrientationChange; Override;

      Procedure UpdateRatio();

      Function GetWidth:Integer;
      Function GetHeight:Integer;

      Function GetTextureAtlas:TextureAtlas;

      Function GetFontRenderer: FontRenderer;

      Procedure RenderUIs;

    Public
      Procedure Init; Override;
      Procedure Resume; Override;

      Procedure Release; Override;

      Class Function Instance:UIManager;

      Procedure AddUI(UI:TERRAUI);
      Procedure RemoveUI(UI:TERRAUI);

      Procedure TextureAtlasClear();

      Procedure Render;
      Procedure AfterEffects;

      Function CreateProperty(Owner:TERRAObject; Const KeyName, ObjectType:TERRAString):TERRAObject; Override;
      
      Procedure SetFontRenderer(const Value: FontRenderer);

      Function GetUI(Index:Integer):TERRAUI;

      Property Width:Integer Read GetWidth;
      Property Height:Integer Read GetHeight;

      Property TextureAtlas:TextureAtlas Read _TextureAtlas;

      Property Ratio:Single Read _Ratio;

      Property Count:Integer Read _UICount;

      Property FontRenderer:FontRenderer Read GetFontRenderer Write SetFontRenderer;

      Property Viewport:TERRAViewport Read _Viewport;
  End;

Function GetSpriteZOnTop(W:Widget; Ofs:Single = 1.0):Single;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Stream, TERRA_Renderer, TERRA_XML, TERRA_UITabs, TERRA_UIScrollBar,
  TERRA_Matrix4x4, TERRA_Log, TERRA_FileUtils, TERRA_FileManager, TERRA_InputManager,
  TERRA_UIVirtualKeyboard, TERRA_UIButton, TERRA_UISprite, TERRA_UILabel, TERRA_UIWindow,
  TERRA_UICheckbox, TERRA_UIEditText, TERRA_UIIcon;


Var
  _UIManager_Instance:ApplicationObject = Nil;

Function GetSpriteZOnTop(W:Widget; Ofs:Single):Single;
Begin
  //Result := (100 - W.GetLayer()) - Ofs;
  Result := W.GetLayer() + Ofs;
End;

Procedure ShowWidget(Source:Widget); CDecl;
Begin
  Source.Visible := True;
End;

Procedure HideWidget(Source:Pointer); CDecl;
Var
  W:Widget;
Begin
  W := Widget(Source);
  W.SetColor(W._OriginalColor);
  W.SetRelativePosition(W._OriginalPosition);
  W.SetVisible(False);

  If (W.UI.Highlight = W) Or (W.HasHighlightedChildren()) Then
    W.UI.Highlight := Nil;
End;


{ Widget }
Constructor Widget.Create(Const Name:TERRAString; Parent:Widget; Const ComponentName:TERRAString);
Begin
  _ObjectName := Name;

  Self.InitProperties();

  SetVisible(True);
  _Enabled := True;

  If (Assigned(Parent)) And (Parent Is TERRAUI) Then
  Begin
    _UI := TERRAUI(Parent);
    _Parent := Nil;
  End Else
  Begin
    _UI := Parent.UI;
    _Parent := Parent;
  End;

  Self.LoadSkin(ComponentName);

  _Pivot := VectorCreate2D(0.5, 0.5);
  SetScale(1.0);
  SetRotation(0.0);
  SetSaturation(1.0);
  SetColor(ColorWhite);
  _ColorTable := Nil;

  _ClipRect.Style := clipNothing;

  //_DropShadowColor := ColorNull;
  _DropShadowColor := ColorGrey(0, 255);

  _Font := UI._DefaultFont;
  _FontRenderer := UI.FontRenderer;

  _InheritColor := True;
  _TransformChanged := True;

  UI.AddWidget(Self);
End;


Procedure Widget.Release();
Begin
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
End;

Procedure Widget.InitProperties;
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
  _Align := EnumProperty.Create('align', 0, UIManager.Instance._AlignEnums);

  _BasePropertiesIndex := 0;
  _CustomPropertiesIndex := 12;
End;

Function Widget.GetPropertyByIndex(Index:Integer):TERRAObject;
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
  Else
    Result := Nil;
  End;
End;

Procedure Widget.ExpandProperties(Count:Integer);
Begin
  _BasePropertiesIndex := _CustomPropertiesIndex;
  Inc(_CustomPropertiesIndex, Count);
End;


Procedure Widget.LoadSkin(Const ComponentName:TERRAString);
Begin
  _Skin.Value := ComponentName;
  _SkinComponent := _UI.GetComponent(ComponentName);
End;

Function Widget.HasHighlightedChildren():Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
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

  Result := False;
End;

Function Widget.IsHighlighted: Boolean;
Begin
  Result := (UI._Highlight = Self) Or (_Parent<>Nil) And (_Parent.IsHighlighted);
End;

{
Procedure Widget.CenterOnScreen(CenterX:Boolean = True; CenterY:Boolean = True);
Begin
  _Align := waTopLeft;
  Self.UpdateRects;

  If CenterX Then
    _Position.X := (UIManager.Instance.Width * 0.5) - (_Size.X * 0.5  * _Scale);
  If CenterY Then
    _Position.Y := (UIManager.Instance.Height * 0.5) - (_Size.Y * 0.5 *_Scale);
End;}

Procedure Widget.CenterOnPoint(X,Y:Single);
Begin
  Self.Align := waTopLeft;
  Self.UpdateRects;

  _Position.X.Value := X - _Size.X * 0.5;
  _Position.Y.Value := Y - _Size.Y * 0.5;
End;

{Procedure Widget.CenterOnParent(CenterX, CenterY: Boolean);
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

Procedure Widget.StartHighlight;
Begin
  Self._Color.Value := ColorBlack;
End;

Procedure Widget.StopHighlight;
Begin
  Self._Color.Value := ColorWhite;
End;

Procedure Widget.OnHit(Handler:WidgetEventHandler);
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

Procedure Widget.OnLanguageChange;
Begin
  // do nothing
End;

Procedure Widget.UpdateRects();
Begin
  _Size.X := Self.GetDimension(Self.Width, uiDimensionWidth);
  _Size.Y := Self.GetDimension(Self.Height, uiDimensionHeight);
End;

Function Widget.GetSize: Vector2D;
Begin
  If (_Size.X<=0) Or (_Size.Y<=0) Then
    Self.UpdateRects();

  Result.X := _Size.X;
  Result.Y := _Size.Y;
End;

Procedure Widget.CopyValue(Other: CollectionObject);
Begin
  RemoveHint(Cardinal(Other));
  RaiseError('Not implemented!');
End;

Function Widget.IsSameFamily(Other: Widget): Boolean;
Begin
  Result := (Self = Other);
  If (Not Result) And (Other<>Nil) And (Other._Parent<>Nil) Then
    Result := IsSameFamily(Other._Parent);
End;

Function Widget.IsSelectable():Boolean;
Begin
  Result := Assigned(Self.OnMouseClick);
End;

Procedure Widget.ConvertGlobalToLocal(Var V:Vector2D);
Begin
  If (Self.Rotation<>0.0) Then
    V := Self._InverseTransform.Transform(V);

  V.Subtract(Self.AbsolutePosition);
End;

Procedure Widget.ConvertLocalToGlobal(Var V:Vector2D);
Begin
  V.Add(Self.AbsolutePosition);

  If (Self.Rotation<>0.0) Then
    V := Self._Transform.Transform(V);
End;


Function Widget.CanHighlight(GroupID:Integer): Boolean;
Begin
  Result := (Self.Visible) And (Self.Enabled) And (Self.IsSelectable()) And (Self.HighlightGroup = GroupID) And (Not Self.HasPropertyTweens());
End;

Function Widget.GetDownControl(): Widget;
Var
  W:Widget;
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

  It := Self.UI.Widgets.GetIterator();
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
  ReleaseObject(It);
End;

Function Widget.GetUpControl(): Widget;
Var
  W:Widget;
  It:Iterator;
  P,Base:Vector2D;
  GroupID:Integer;
  Min, Dist, Y:Single;
Begin
  Result := Nil;

  Min := 99999;
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
  ReleaseObject(It);
End;

Function Widget.GetRightControl(): Widget;
Var
  W:Widget;
  It:Iterator;
  Base:Vector2D;
  Min, Dist, X:Single;
  GroupID:Integer;
Begin
  Result := Nil;

  Min := 99999;
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
  ReleaseObject(It);
End;

Function Widget.GetLeftControl(): Widget;
Var
  W:Widget;
  It:Iterator;
  P, Base:Vector2D;
  Min, Dist:Single;
  GroupID:Integer;
Begin
  Result := Nil;

  Min := 99999;
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
  ReleaseObject(It);
End;

Function Widget.Show(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
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

Function Widget.Hide(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
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

Function Widget.ToggleVisibility(AnimationFlags:Integer; EaseType:TweenEaseType; Delay, Duration:Cardinal; Callback:TweenCallback):Boolean;
Begin
  If Self.Visible Then
    Result := Hide(AnimationFlags, EaseType, Delay, Duration, Callback)
  Else
    Result := Show(AnimationFlags, EaseType, Delay, Duration, Callback);
End;

Function Widget.GetVisible:Boolean;
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

    If (Result) And (Self.TabIndex>=0) And (Parent.TabControl<>Nil) And (_Parent.TabControl Is UITabList) Then
    Begin
      If (UITabList(_Parent.TabControl).SelectedIndex<>Self.TabIndex) Then
        Result := False;
    End;
  End;
End;

Procedure Widget.SetVisible(Value:Boolean);
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

Procedure Widget.SetRelativePosition(Const Pos:Vector2D);
Begin
  If (Pos.X = _Position.X.Value) And (Pos.Y = _Position.Y.Value) Then
    Exit;

  _Position.Value := Pos;
  _TransformChanged := True;
End;

Procedure Widget.SetAbsolutePosition(Pos:Vector2D);
Begin
  If Assigned(_Parent) Then
    Pos.Subtract(_Parent.AbsolutePosition);

  Self.SetRelativePosition(Pos);
End;

Procedure Widget.SetLayer(Z:Single);
Begin
  If (Z = _Layer.Value) Then
    Exit;

  _Layer.Value := Z;
End;

Procedure Widget.SetColor(MyColor:Color);
Begin
(*  If (Cardinal(MyColor) = Cardinal(_Color)) Then
    Exit;*)

  _Color.Value := MyColor;
End;

Function Widget.GetColor:Color;  {$IFDEF FPC} Inline;{$ENDIF}
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

Function Widget.GetRelativePosition:Vector2D;
Begin
  Result := _Position.Value;
End;

Function Widget.GetAbsolutePosition:Vector2D;
Var
  Width, Height:Single;
  ParentSize, Center:Vector2D;
Begin
  Result := _Position.Value;

  If (Align<>waTopLeft) Then
  Begin
    Width := _Size.X{ * _Scale};
    Height := _Size.Y{ * _Scale};

    {IF _Scale>1 Then
      IntToString(2);}

  	If (Assigned(_Parent)) Then
    Begin
      ParentSize := _Parent.Size;
{      ParentSize.X := ParentSize.X * _Parent._Scale;
      ParentSize.Y := ParentSize.Y * _Parent._Scale;}
    End Else
      ParentSize := VectorCreate2D(UIManager.Instance.Width, UIManager.Instance.Height);
      
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

Function Widget.GetLayer:Single;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
	Result := _Layer.Value;

	If (Assigned(_Parent)) Then
		Result := Result + _Parent.GetLayer();
End;

Function Widget.GetSaturation:Single;
Begin
	Result := _Saturation.Value;
	If (Assigned(_Parent)) Then
		Result := Result * _Parent.GetSaturation();
End;

Function Widget.GetHighlightGroup:Integer;
Begin
	Result := _HighlightGroup;
	If (Assigned(_Parent)) And (_HighlightGroup<=0) Then
		Result := _Parent.GetHighlightGroup();
End;


Function Widget.GetColorTable:TERRATexture;
Begin
	Result := _ColorTable;
	If (Result = Nil) And (Assigned(_Parent)) Then
		Result := _Parent.GetColorTable();
End;

Procedure Widget.ClearProperties();
Begin
  _LastState := -1;
End;

Procedure Widget.CopyProperties(ID:Integer; Selected:Boolean; Out CurrentState, DefaultState:Integer);
Begin
  If (Selected) Then
    CurrentState := 1
  Else
    CurrentState := 0;

  If (CurrentState = _LastState) Then
    Exit;

  Self.UpdateProperties();
  _LastState := CurrentState;

  DefaultState := CurrentState;

  If (Self._MouseOver) Then
    Inc(CurrentState, 2);

  If Assigned(_SkinComponent) Then
    _SkinComponent.GetProperties(_Properties, ID, CurrentState, DefaultState);
End;

Procedure Widget.DrawText(Const Text:TERRAString; Const X,Y, Layer:Single; Const TextRect:Vector2D; Scale:Single; ID:Integer; Selected:Boolean; Const TextColor:Color);
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
End;

Procedure Widget.DrawComponent(X, Y, Layer: Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean);
Begin
  Self.DrawCroppedComponent(X, Y, Layer, 0.0, 0.0, 1.0, 1.0, Width, Height, ID, Selected, ScaleColor);
End;

Procedure Widget.DrawCroppedComponent(X, Y, Layer, U1, V1, U2, V2:Single; Const Width, Height:UIDimension; ID:Integer; Selected:Boolean; ScaleColor:Boolean = True);
Var
  Target:UIQuadList;
  P:Vector2D;
  Z:Single;
  OfsX,OfsY:Single;
  I:Integer;

  CurrentState, DefaultState:Integer;
Begin
  If _SkinComponent = Nil Then
    Exit;

  Self.CopyProperties(ID, Selected, CurrentState, DefaultState);

  If (Self.HasPropertyTweens()) Then
    _TransformChanged := True;

  P := Self.GetAbsolutePosition();
  Z := Self.GetLayer() + Layer;

  Self.GetScrollOffset(OfsX, OfsY);

  P.X := P.X + OfsX + X;
  P.Y := P.Y + OfsY + Y;

  FillChar(Target, SizeOf(Target), 0);
  _SkinComponent.Draw(Target, P.X, P.Y, U1, V1, U2, V2, Trunc(Self.GetDimension(Width, uiDimensionWidth)), Trunc(Self.GetDimension(Height, uiDimensionHeight)), ID, CurrentState, DefaultState);

  //_FontRenderer.SetFont(Self.GetFont());
  _FontRenderer.SetClipRect(GetClipRect());
  _FontRenderer.SetTransform(_Transform);
  _FontRenderer.SetDropShadow(_DropShadowColor);
  //_FontRenderer.SetColor(C);

  //_FontRenderer.DrawText(P.X, P.Y, Z, Text);

  For I:=0 To Pred(Target.QuadCount) Do
  Begin
    UI.AddQuad(Target.Quads[I], _Properties, Z, _Transform);
  End;
End;

Function Widget.OnKeyDown(Key:Word):Boolean;
Begin
  RemoveHint(Key);
	Result := False;
End;

Function Widget.OnKeyUp(Key:Word):Boolean;
Var
  I:Integer;
Begin
  If Not Self.Visible Then
  Begin
    Result := False;
    Exit;
  End;

  If (Key = UI.Key_Action) Then
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
  End;
End;

Function Widget.OnKeyPress(Key:Word):Boolean;
Begin
  RemoveHint(Key);
	Result := False;
End;

Function Widget.OnRegion(X,Y:Single): Boolean;
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

Function Widget.AllowsEvents(): Boolean;
Begin
  If (Not Visible) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;

  If (_UI.Modal = Nil) Or  (Self = _UI.Modal) Then
    Exit;

  If Assigned(Self.Parent) Then
    Result := Self.Parent.AllowsEvents();
End;

Procedure Widget.PickAt(Const X, Y:Integer; Var CurrentPick:Widget; Var Max:Single; Ignore:Widget);
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

Procedure Widget.BeginDrag(X,Y:Integer; Mode:UIDragMode);
Begin
  If (Assigned(OnBeginDrag)) Then
    Self.OnBeginDrag(Self);

  _UI._Dragger := Self;
  _DragMode := Mode;
  _Dragging := True;

  _DragStart := _Position.Value;
  _DragSize := _Size;

  _DragX := (X-_Position.X.Value);
  _DragY := (Y-_Position.Y.Value);
End;

Procedure Widget.CancelDrag;
Begin
  If _Dragging Then
  Begin
    _Position.Value := _DragStart;
    _Dragging := False;
    If _UI._Dragger = Self Then
      _UI._Dragger := Nil;

    Self._TransformChanged := True;
  End;
End;

Procedure Widget.FinishDrag();
Begin
  If (_UI._Dragger <> Self) Then
    Exit;

  If (Assigned(OnEndDrag)) Then
    Self.OnEndDrag(Self);

  _Dragging := False;
  _UI._Dragger := Nil;
End;

Procedure Widget.OnMouseDown(X,Y:Integer;Button:Word);
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

Procedure Widget.OnMouseUp(X,Y:Integer;Button:Word);
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

Procedure Widget.ApplyDragMode(Const PX, PY:Single; Mode:UIDragMode);
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

Procedure Widget.OnMouseMove(X,Y:Integer);
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

Procedure Widget.OnMouseWheel(X,Y:Integer; Delta:Integer);
Begin
  // do nothing
End;

Function Widget.Sort(Other:CollectionObject):Integer;
Begin
  If (Self.Name<Widget(Other).Name) Then
    Result := 1
  Else
  If (Self.Name>Widget(Other).Name) Then
    Result := -1
  Else
    Result := 0;
End;

Procedure Widget.UpdateHighlight();
Begin
  UpdateHighlight(IsHighlighted());
End;

Procedure Widget.UpdateHighlight(Condition:Boolean);
Begin
  If (_UsingHighLightProperties = Condition) Then
    Exit;

  _UsingHighLightProperties := Condition;
  If (Condition) Then
    Self.StartHighlight()
  Else
    Self.StopHighlight();
End;

Function Widget.HasMouseOver: Boolean;
Begin
  Result := (Assigned(OnMouseOver)) Or (Assigned(OnMouseOut)) Or (Assigned(OnMouseClick));
End;

Procedure Widget.SetSaturation(const Value: Single);
Begin
  _Saturation.Value := Value;
  _NeedsUpdate := True;
End;

Procedure Widget.SetScale(const Value: Single);
Begin
  _Scale.Value := Value;
  _NeedsUpdate := True;
  _TransformChanged := True;
End;


Procedure Widget.SetRotation(const Value: Single);
Begin
  _Rotation.Value := Value;
  _NeedsUpdate := True;
  _TransformChanged := True;
End;

Function Widget.UpdateTransform():Boolean;
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
    _Transform := MatrixMultiply3x3(_Transform, Parent._Transform)
  Else
    _Transform := MatrixMultiply3x3(_Transform, _UI._Transform);

  Self.GetScrollOffset(OfsX, OfsY);

  _InverseTransform := MatrixInverse2D(_Transform);

  Result := True;
End;

Function Widget.GetChildByIndex(Index:Integer): Widget;
Begin
  If (Index>=0) And (Index<=Self.ChildrenCount) Then
    Result := _ChildrenList[Index]
  Else
    Result := Nil;
End;

Function Widget.GetChildByName(Const Name:TERRAString): Widget;
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

Function Widget.GetChildByClass(ChildClass: WidgetClass; Index:Integer): Widget;
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

Procedure Widget.AddChild(W: Widget);
Begin
  If (W=Nil) Or (W = Self) Then
    Exit;

  W._Parent := Self;
  Inc(_ChildrenCount);
  SetLength(_ChildrenList, _ChildrenCount);
  _ChildrenList[Pred(_ChildrenCount)] := W;
End;

Procedure Widget.RemoveChild(W:Widget);
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


Procedure Widget.Render;
Var
  I:Integer;
Begin
  If (Not Self.Visible) Or (Self._Deleted) Then
    Exit;

  If (Assigned(_SkinComponent)) And (_SkinComponent.Name <> Self._Skin.Value) Then
    Self.LoadSkin(Self._Skin.Value);

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I].Visible) And (_ChildrenList[I].CanRender()) Then
    _ChildrenList[I].Render();
End;


Procedure Widget.OnHighlight(Prev: Widget);
Begin
  // do nothing
End;

Function Widget.IsSelected: Boolean;
Begin
  Result := (Self = UI.Highlight) Or (Self = UI.Focus);
End;

Function Widget.OnSelectAction: Boolean;
Begin
  If (Self.Selected) And (Assigned(OnMouseClick)) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.OnHit(OnMouseClick);
    Result := True;
  End Else
    Result := False;
End;

Function Widget.OnSelectDown():Boolean;
Var
  W:Widget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetDownControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      UI.Highlight := W;
    End;
  End;
End;

Function Widget.OnSelectLeft():Boolean;
Var
  W:Widget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetLeftControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      UI.Highlight := W;
    End;
  End;
End;

Function Widget.OnSelectRight():Boolean;
Var
  W:Widget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetRightControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      UI.Highlight := W;
    End;
  End;
End;

Function Widget.OnSelectUp():Boolean;
Var
  W:Widget;
Begin
  Result := False;

  If (Self.Selected) Then
  Begin
    W := Self.GetUpControl();
    If (Assigned(W)) And (Not Self.HasPropertyTweens()) Then
    Begin
      Result := True;
      UI.Highlight := W;
    End;
  End;
End;

Function Widget.GetClipRect:ClipRect;
Begin
  Result := Self._ClipRect;

  If (Assigned(_Parent)) Then
    Result.Merge(_Parent.GetClipRect())
  Else
    Result.Merge(_UI._ClipRect);
End;

Function Widget.GetFont:TERRAFont;
Begin
  Result := _Font;

  If Result = Nil Then
    Result := _UI.DefaultFont;

  If Result = Nil Then
    Result := FontManager.Instance.DefaultFont;
End;

Procedure Widget.SetClipRect(Value:ClipRect);
Begin
  Self._ClipRect := Value;
End;

Procedure Widget.GetScrollOffset(Out OfsX, OfsY: Single);
Var
  TX, TY:Single;
  Bar:UIScrollBar;
Begin
  OfsX := 0;
  OfsY := 0;

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
  End;
End;

Procedure Widget.SetPositionRelativeToOther(Other: Widget; OfsX, OfsY: Single);
Var
  P:Vector2D;
Begin
  P := Other.AbsolutePosition;

  If (Other.Parent<>Nil) Then
    P.Subtract(Other.Parent.AbsolutePosition);

  P.Add(VectorCreate2D(OfsX, OfsY));

  Self.SetRelativePosition(P);
End;

Procedure Widget.SetObjectName(const Value:TERRAString);
Var
  Existed:Boolean;
Begin
  Existed := _UI._Widgets.Remove(Self);
  Self._ObjectName := Value;

  If Existed Then
    _UI._Widgets.Add(Self);
End;

Function Widget.OutsideClipRect(X, Y: Single): Boolean;
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

Procedure Widget.SetEnabled(Value: Boolean);
Begin
  Self._Enabled := Value;
End;

Procedure Widget.UpdateClipRect(Clip: ClipRect; LeftBorder,TopBorder, RightBorder, BottomBorder:Single);
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

Procedure Widget.SetFont(const Value:TERRAFont);
Begin
  If (Self._Font = Value) Then
    Exit;

  If Assigned(Value) Then
    Value.Prefetch();

  Self._Font := Value;
End;

Function Widget.GetIndex: Integer;
Var
  S:TERRAString;
Begin
  S := Self.Name;
  StringGetNextSplit(S, Ord('_'));
  Result := StringToInt(S); 
End;

Function Widget.GetTabControl():Widget;
Begin
  If Assigned(_TabControl) Then
    Result := _TabControl
  Else
  If (Assigned(_Parent)) Then
    Result := _Parent.GetTabControl()
  Else
    Result := Nil;
End;

Procedure Widget.SetChildrenVisibilityByTag(Tag: Integer; Visibility: Boolean);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (Assigned(_ChildrenList[I])) And (_ChildrenList[I].Tag = Tag) Then
    _ChildrenList[I].Visible := Visibility;
End;

Function Widget.CanRender: Boolean;
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

Procedure Widget.ResetClipRect;
Begin
  _ClipRect.Style := clipSomething;
  _ClipRect.X := Self.AbsolutePosition.X;
  _ClipRect.Y := Self.AbsolutePosition.Y;
  _ClipRect.Width := Self.Size.X;
  _ClipRect.Height := Self.Size.Y;
  _ClipRect.Transform(_UI.Transform);
End;

Function Widget.OnCustomRegion(X, Y:Integer; X1, Y1, X2, Y2:Single): Boolean;
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

Function Widget.GetFontRenderer: FontRenderer;
Begin
  Result := _FontRenderer;

  If Assigned(Result) Then
    Result.SetFont(Self.Font);
End;

Function Widget.IsOutsideScreen: Boolean;
Var
  P:Vector2D;
Begin
  P := Self.AbsolutePosition;
  Result := (P.X + Self.Size.X<0) Or (P.Y + Self.Size.Y<0) Or (P.X> UIManager.Instance.Width) Or (P.Y > UIManager.Instance.Height);
End;

Function Widget.GetDimension(Const Dim: UIDimension; Const Target:UIDimensionTarget): Single;
Begin
  If Dim.IsPercent Then
  Begin
    If (Target = uiDimensionWidth) Then
      Result := (Dim.Value * 0.01) * UIManager.Instance.Width
    Else
      Result := (Dim.Value * 0.01) * UIManager.Instance.Height
  End Else
    Result := Dim.Value;
End;

Procedure Widget.ClipChildren(const Clip: ClipRect);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If Assigned(_ChildrenList[I].Scroll) Then
    _ChildrenList[I].SetClipRect(Clip);
End;

Function Widget.GetWidth: UIDimension;
Begin
  Result := _Width.Value;
End;

Function Widget.GetHeight: UIDimension;
Begin
  Result := _Height.Value;
End;

Procedure Widget.SetWidth(const Value: UIDimension);
Begin
  Self._Width.Value := Value;
  Self.UpdateRects();
End;

Procedure Widget.SetHeight(const Value: UIDimension);
Begin
  Self._Height.Value := Value;
  Self.UpdateRects();
End;

Procedure Widget.UpdateProperties;
Begin
  _Properties.QuadColor := Self.GetColor();
  _Properties.TextColor := ColorWhite;

  _Properties.TextFont := Self.GetFont();

  _Properties.Saturation := Self.GetSaturation();
  _Properties.ColorTable := Self.GetColorTable();

  _Properties.Clip := Self.GetClipRect();

  //If (ScaleColor) And (Not DisableUIColor) Then
  Begin
    _Properties.QuadColor := ColorMultiply(_Properties.QuadColor, UI._Color.Value);
    _Properties.Saturation := _Properties.Saturation * UI._Saturation.Value;

    If (_Properties.ColorTable = Nil) Then
      _Properties.ColorTable := UI._ColorTable;
  End;

  If Not Enabled Then
    _Properties.Saturation := 0.0;
End;

Function Widget.GetRotation: Single;
Begin
  Result := _Rotation.Value;
End;

Function Widget.GetScale: Single;
Begin
  Result := _Scale.Value;
End;

Procedure Widget.NullEventHandler(Src:Widget);
Begin
  // do nothing
End;

Function Widget.AdjustWidth(NewWidth: Single):Single;
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

Function Widget.AdjustHeight(NewHeight: Single):Single;
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

Function Widget.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := True;
End;

Procedure Widget.Delete();
Var
  I:Integer;
Begin
  Self._Deleted := True;
  _UI._HasDeletions := True;

  If Assigned(_Parent) Then
    _Parent.RemoveChild(SElf);

  For I:=0 To Pred(_ChildrenCount) Do
    _ChildrenList[I].Delete();

  _ChildrenCount := 0;
End;


Function Widget.GetAlign: Integer;
Begin
  Result := _Align.Value;
End;

Procedure Widget.SetAlign(const Value: Integer);
Begin
  _Align.Value := Value;
End;

Function Widget.GetTabIndex: Integer;
Begin
  Result := _TabIndex.Value;
End;

Procedure Widget.SetTabIndex(const Value: Integer);
Begin
  _TabIndex.Value  := Value;
End;

Function Widget.CreateProperty(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  If (StringEquals(ObjectType, 'UIButton')) Then
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
  Else
    Result := Nil;
End;

Procedure Widget.SetParent(Target:Widget);
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
  Else
    _UI.InsertIntoTopWidgets(Self);
End;

{ TERRAUI }
Constructor TERRAUI.Create;
Begin
  Self.InitProperties();

  _ObjectName := 'UI';
  _Widgets := HashMap.Create(1024);
  SetVisible(True);

  _Transition := Nil;

  SetColor(ColorCreateFromFloat(1.0, 1.0, 1.0, 1.0));
  SetSaturation(1.0);
  _ColorTable := Nil;

  _FontRenderer := Nil;

  _Skin := UISkinComponent.Create(Nil, Nil);

  SetTransform(MatrixIdentity3x3);

  Key_Up := TERRA_OS.keyUp;
  Key_Down := TERRA_OS.keyDown;
  Key_Right := TERRA_OS.keyRight;
  Key_Left := TERRA_OS.keyLeft;
  Key_Action := TERRA_OS.keyEnter;
  Key_Cancel := TERRA_OS.keyEscape;

  _ClipRect.SetStyle(clipNothing);

  UIManager.Instance.AddUI(Self);
End;

Procedure TERRAUI.Release;
Var
  I:Integer;
Begin
  ReleaseObject(_Skin);
  ReleaseObject(_Transition);
	ReleaseObject(_Widgets);
End;

Procedure TERRAUI.Clear;
Begin
  Log(logError, 'UI', 'Clearing UI');
  _First := Nil;
  _Widgets.Clear;

  SetTransition(Nil);
  Log(logError, 'UI', 'UI is now clear.');
End;

(*Procedure UI.DrawCutsceneBars;
Var
  V:Array[0..11] Of Vector2D;
  X,H, Y:Single;
  PositionHandle:Integer;
  MyShader:Shader;
Begin
  X := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  _CutsceneAlpha:=1;
  Y := H * 0.1 * _CutsceneAlpha;

  V[0].X := 0.0;
  V[0].Y := 0.0;
  V[1].X := 0.0;
  V[1].Y := Y;
  V[2].X := X;
  V[2].Y := Y;
  V[3] := V[2];
  V[4].X := X;
  V[4].Y := 0;
  V[5] := V[0];

  V[6].X := 0.0;
  V[6].Y := H-Y;
  V[7].X := 0.0;
  V[7].Y := H;
  V[8].X := X;
  V[8].Y := H;
  V[9] := V[8];
  V[10].X := X;
  V[10].Y := H-Y;
  V[11] := V[6];

  MyShader := UIManager.Instance.GetCutsceneShader();
  If MyShader = Nil Then
    Exit;

  ShaderManager.Instance.Bind(MyShader);
  MyShader.SetUniform('projectionMatrix', GraphicsManager.Instance.ProjectionMatrix);

  PositionHandle := MyShader.GetAttribute('terra_position');

  GraphicsManager.Instance.Renderer.SetBlendMode(blendNone);

End;
End;*)

Procedure TERRAUI.SetFocus(W: Widget);
Begin
  If (W = _Focus) Then
    Exit;

  If (Assigned(_Focus)) Then
  Begin
    _Focus.StopHighlight();
    Self.SetHighlight(W);
  End;

  _Focus := W;
End;

Procedure TERRAUI.SetDefaultFont(const Value:TERRAFont);
Begin
  FontManager.Instance.PreFetch(Value);
  Self._DefaultFont := Value;
End;

Procedure TERRAUI.SetColorTable(Const Value:TERRATexture);
Begin
  Self._ColorTable := Value;
End;

Procedure TERRAUI.AddWidget(MyWidget:Widget);
Var
  Temp:Widget;
  Found:Boolean;
  It:Iterator;
  NameIndex:Integer;
  BaseName:TERRAString;
Begin
  BaseName := MyWidget.Name;
  NameIndex := 0;
  While Assigned(GetWidget(MyWidget.Name)) Do
  Begin
    If NameIndex<=0 Then
    Begin
      Temp := GetWidget(MyWidget.Name);
      Temp.Name := BaseName + '_'+IntToString(NameIndex);
    End;

    Inc(NameIndex);
    MyWidget.Name := BaseName + '_' + IntToString(NameIndex);
  End;

  If (Assigned(MyWidget.Parent)) Then
  Begin
    (*It := _Widgets.GetIterator();
    Found := False;
    While (It.HasNext) Do
    Begin
      Temp := Widget(It.Value);
      If (Temp = MyWidget._Parent) Then
      Begin
        Widget(Temp).AddChild(MyWidget);
        Found := True;
        Break;
      End;
    End;
    ReleaseObject(It);

    If Not Found Then
      Log(logWarning, 'UI', 'Error finding parent for '+ MyWidget.Name +'!');*)

    MyWidget.Parent.AddChild(MyWidget);
  End Else
    InsertIntoTopWidgets(MyWidget);

  _Widgets.Add(MyWidget);
End;

Procedure TERRAUI.InsertIntoTopWidgets(W:Widget);
Var
  Found:Boolean;
  Temp, Last:Widget;
Begin
  If (Assigned(_First)) Then
  Begin
    If (_First.Layer > W.Layer) Then
    Begin
      W._Next := _First;
      _First := W;
    End Else
    Begin
      Last := _First;
      Temp := _First._Next;
      Found := False;
      While (Assigned(Temp)) Do
      Begin
        If (Temp.Layer > W.Layer) Then
        Begin
          W._Next := Temp;
          Last._Next := W;
          Found := True;
          Break;
        End;
        Last := Temp;
        Temp := Temp._Next;
      End;

      If (Not Found) Then
      Begin
        Last._Next := W;
        W._Next := Nil;
      End;
    End;

  End Else
  Begin
    _First := W;
    W._Next := Nil;
  End;
End;

Procedure TERRAUI.RemoveFromTopWidgets(W:Widget);
Var
  Current, Temp:Widget;
Begin
  If (_First = W) Then
  Begin
    _First := _First.Next;
    Exit;
  End;

  Current := _First;
  While Assigned(Current) Do
  Begin
    Temp := Current.Next;
    If (Temp = W) Then
    Begin
      Current._Next := Temp.Next;
      Exit;
    End;

    Current := Temp;
  End;
End;

Procedure TERRAUI.DeleteWidget(MyWidget:Widget);
Begin
  If (MyWidget = Nil) Then
    Exit;

  MyWidget.Delete();
End;

Function SearchWidgetByName(P:CollectionObject; UserData:Pointer):Boolean; CDecl;
Begin
  Result := (Widget(P).Name = PString(Userdata)^);
End;

Function TERRAUI.GetWidget(Const Name:TERRAString):Widget;
Var
  WidgetName:TERRAString;
Begin
  WidgetName := GetFileName(Name, True);
  Result := Widget(_Widgets.Search(SearchWidgetByName, @WidgetName));
End;


Function TERRAUI.AddQuad(Const Quad:UIQuad; Const Props:UISkinProperty; Z:Single; Const Transform:Matrix3x3):QuadSprite;
Var
  Tex:TERRATexture;
Begin
  Tex := UIManager.Instance.TextureAtlas.GetTexture(Quad.PageID);
  Result := SpriteManager.Instance.DrawSprite(Quad.Pos.X, Quad.Pos.Y, Z, Tex, ColorTable, blendBlend, Saturation);
  If (Result = Nil) Then
    Exit;

  Result.Rect.Width := Trunc(Quad.Size.X);
  Result.Rect.Height := Trunc(Quad.Size.Y);

  Result.ClipRect := Props.Clip;

  Result.SetTransform(Transform);

  Result.SetColor(Props.QuadColor);
  //Result.Rect.UVRemap(Quad.StartUV.X, Quad.StartUV.Y, Quad.EndUV.X, Quad.EndUV.Y);
  Result.Rect.UVRemap(Quad.StartUV.X, Quad.StartUV.Y, Quad.EndUV.X, Quad.EndUV.Y);
End;

Procedure TERRAUI.UpdateLanguage();
Var
  MyWidget:Widget;
  It:Iterator;
Begin
  _Language := Application.Instance.Language;
  It := _Widgets.GetIterator();
  While (It.HasNext) Do
  Begin
    MyWidget := Widget(It.Value);
    MyWidget.OnLanguageChange();
  End;
  ReleaseObject(It);
End;

Procedure TERRAUI.Render;
Var
  Current, Temp:Widget;
  I, J:Integer;
  X,Y:Single;
  It:Iterator;
Begin
  _Draw := False;

  If (Assigned(_Highlight)) And ((Not _Highlight.Visible) Or (Not _Highlight.Enabled)) Then
  Begin
    _Highlight := SelectNearestWidget(_Highlight);
  End;

  If (Assigned(_Focus)) And (Not _Focus.Visible) Then
    _Focus := Nil;

  If (Assigned(_Modal)) And (Not _Modal.Visible) Then
    _Modal := Nil;

  If (_Language <> Application.Instance.Language) Then
    UpdateLanguage();

  //glEnable(glCoverage);

  (*ShaderManager.Instance.Bind(Self.GetShader);
  _Shader.SetUniform('texture', 0);
  _Shader.SetUniform('projectionMatrix', GraphicsManager.Instance.ProjectionMatrix);
  Q.X := GraphicsManager.Instance.Ratio2D.X;
  Q.Y := GraphicsManager.Instance.Ratio2D.Y;
  Q.Z := 1;
  Q.W := 1;
  _Shader.SetUniform('screen_ratio', Q);

  PositionHandle := _Shader.GetAttribute('terra_position');
  UVHandle := _Shader.GetAttribute('terra_UV0');
  ColorHandle := _Shader.GetAttribute('terra_color');
  *)

  If (UIManager.Instance._UpdateTextureAtlas) Then
    Exit;

  GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);

  If _HasDeletions Then
  Begin
    _HasDeletions := False;

    It := Self.Widgets.GetIterator();
    While It.HasNext() Do
    Begin
      Current := Widget(It.Value);

      If Current._Deleted Then
      Begin
        If Current.Parent = Nil Then
          Self.RemoveFromTopWidgets(Current);

        Current.Discard();
      End;
    End;
    ReleaseObject(It);
  End;

  While (Assigned(_First)) And (Assigned(_First.Parent)) Do
  Begin
    _First := _First.Next;
  End;

  Current := _First;
  While (Assigned(Current)) Do
  Begin
    Temp := Current.Next;
    If (Assigned(Temp)) And (Assigned(Temp.Parent)) Then
    Begin
      Current._Next := Temp.Next;
      Temp := Current._Next;
    End;

    If (Current.Visible) And (Current.CanRender()) Then
      Current.Render();

    Current := Temp;
  End;

  //glDisable(glCoverage);
End;

Procedure TERRAUI.AfterEffects;
Var
  CurrentTransitionID:Cardinal;
Begin
  {If Self._CutsceneAlpha>0 Then
    DrawCutsceneBars;}

  If (Assigned(_Transition)) Then
  Begin
    CurrentTransitionID := _Transition.ID;
    // note, this is tricky, since transition update can call a callback that remove or setup a new transition
    If (Not _Transition.Update) Then
    Begin
      If (Assigned(_Transition)) And (_Transition.ID = CurrentTransitionID) Then
        SetTransition(Nil);
    End;
  End;
End;

Procedure TERRAUI.SetTransition(MyTransition:UITransition);
Begin
  ReleaseObject(_Transition);

  _Transition := MyTransition;

  If Assigned(_Transition) Then
    _Transition.Transform := Self.Transform;
End;

Function TERRAUI.OnKeyDown(Key:Word):Widget;
Begin
  Result := Nil;

	If Assigned(_Highlight) Then
  Begin
		If _Highlight.OnKeyDown(Key) Then
      Result := _Highlight;
  End;
End;

Function TERRAUI.OnKeyUp(Key:Word):Widget;
Var
  MyWidget:Widget;
Begin
  Result := Nil;

	If Assigned(_Highlight) Then
  Begin
		If _Highlight.OnKeyUp(Key) Then
    Begin
      Result := _Highlight;
      Exit;
    End;
  End;
  
	If Assigned(_Focus) Then
  Begin
		If _Focus.OnKeyUp(Key) Then
    Begin
      Result := _Focus;
      Exit;
    End;
  End;

  MyWidget := _First;
  While (Assigned(MyWidget)) Do
  Begin
    If (Not Assigned(MyWidget._Parent)) And (MyWidget.Visible) Then
    Begin
      If MyWidget.OnKeyUp(Key) Then
      Begin
        Result := MyWidget;
        Exit;
      End;
    End;

    MyWidget := MyWidget._Next;
  End;
End;

Function TERRAUI.OnKeyPress(Key:Word):Widget;
Begin
  Result := Nil;
  Log(logDebug, 'UI', 'keypress: '+IntToString(Integer(Key)));

	If Assigned(_Focus) Then
  Begin
    Log(logDebug, 'UI', 'focus is '+_Focus.Name);
		_Focus.OnKeyPress(Key);
    Result := _Focus;
  End;

  Log(logDebug, 'UI', 'keypress done!');
End;

Function TERRAUI.PickWidget(X,Y:Integer; Ignore:Widget = Nil):Widget;
Var
	Current:Widget;
  Max:Single;
Begin
  _LastWidget := Nil;

//  ConvertGlobalToLocal(X, Y);

  Result := Nil;
  Max := -9999;

  Current := _First;
  While (Assigned(Current)) Do
  Begin
    If (Current.Parent = Nil) And (Current.AllowsEvents()) Then
    Begin
      Current.PickAt(X, Y, Result, Max, Ignore);
    End;

    Current := Current.Next;
  End;


  If (Self.Modal<>Nil) And (Assigned(Result)) And (Not Result.IsSameFamily(Modal)) Then
  Begin
    Result := Nil;
    {$IFDEF DEBUG_GUI}Log(logDebug, 'Game', 'Cancelled because of modal...');{$ENDIF}
  End;

  //Log(logDebug, 'Game', 'Found a Widget for picking: '+CurrentPick.Name);
  _LastWidget := Result;
End;

Function TERRAUI.OnMouseDown(X,Y:Integer;Button:Word):Widget;
Begin
  Result := Self.PickWidget(X,Y);

  If (Assigned(Result)) And (Result.Enabled) And (Not Result.HasPropertyTweens()) Then
    Result.OnMouseDown(X, Y, Button);
End;

Function TERRAUI.OnMouseUp(X,Y:Integer;Button:Word):Widget;
Begin
  Result := Self.PickWidget(X,Y);

  If (Assigned(Result)) And (Result.Enabled) And (Not Result.HasPropertyTweens()) Then
    Result.OnMouseUp(X, Y, Button);
End;

Function TERRAUI.OnMouseMove(X,Y:Integer):Widget;
Begin
  _LastWidget := Nil;

//  ConvertGlobalToLocal(X, Y);

  If Assigned(_Dragger) Then
  Begin
    _Dragger.OnMouseMove(X, Y);
    Result := _Dragger;
    _LastWidget := Result;
    Exit;
  End;

  Result := Self.PickWidget(X,Y);

  If (Assigned(Result)) Then
  Begin
    If (Result.Enabled) And (Not Result.HasPropertyTweens()) Then
      Result.OnMouseMove(X, Y);
  End;

  If (_LastOver <> Result) Then
  Begin
    If (Assigned(_LastOver)) And (Assigned(_LastOver.OnMouseOut)) Then
      _LastOver.OnMouseOut(Result);

    If (Assigned(Result)) And (Assigned(Result.OnMouseOver)) Then
      Result.OnMouseOver(_LastOver);

    _LastOver := Result;
  End;
End;

Function TERRAUI.OnMouseWheel(X,Y:Integer; Delta:Integer):Widget;
Begin
	If Assigned(_Focus) Then
  Begin
		_Focus.OnMouseWheel(X, Y, Delta);
    Result := _Focus;
    _LastWidget := Result;
    Exit;
  End;

  Result := Self.PickWidget(X,Y);

  If (Assigned(Result)) And (Result.Enabled) And (Not Result.HasPropertyTweens()) Then
    Result.OnMouseWheel(X, Y, Delta);
End;

Procedure TERRAUI.SetHighlight(const Value: Widget);
Var
  Prev, Temp:Widget;
Begin
  Prev := Self._Highlight;

  Self._Highlight := Value;

  If Assigned(Value) Then
  Begin
    Temp := Value;
    Value.OnHighlight(Prev);
    If (Temp = Self._Highlight) Then
      Self._Highlight._SelectedWithKeyboard := False;
  End;
End;

Procedure TERRAUI.SetDragger(const Value: Widget);
Begin
  Self._Dragger := Value;
End;

Procedure TERRAUI.CloseWnd();
Begin
  System_BG.Visible := False;
  System_Wnd.Visible := False;
  //MsgWnd.Hide(widgetAnimateAlpha);

  Modal := Nil;
  Highlight := _PrevHighlight;
End;

Function CloseMsgBox(Src:Widget):Boolean; Cdecl;
Var
  UI:TERRAUI;
Begin
  Result := True;

  If (Src = Nil) Then
    Exit;

  UI := Src._UI;
  If (UI = Nil) Then
    Exit;

  If (UI.System_Wnd = Nil) Or (UI.System_Wnd.HasPropertyTweens()) Then
    Exit;

  UI.CloseWnd();

  If Assigned(UI._WndCallback1) Then
    UI._WndCallback1(Src);

  UI._WndCallback1 := Nil;
  UI._WndCallback2 := Nil;
End;

Function CloseMsgBox2(Src:Widget):Boolean; Cdecl;
Var
  UI:TERRAUI;
Begin
  Result := True;

  If (Src = Nil) Then
    Exit;

  UI := Src._UI;
  If (UI = Nil) Then
    Exit;

  If (UI.System_Wnd = Nil) Or (UI.System_Wnd.HasPropertyTweens()) Then
    Exit;

  UI.CloseWnd();

  If Assigned(UI._WndCallback2) Then
    UI._WndCallback2(Src);

  UI._WndCallback1 := Nil;
  UI._WndCallback2 := Nil;
End;

Procedure TERRAUI.InitTempWidgets();
Var
  N,I:Integer;
//TODO  S:UISprite;
Begin
(*  System_Wnd := UIWindow(GetWidget(System_Name_Wnd));
  System_Text := UILabel(GetWidget(System_Name_Text));
  For I:=0 To 2 Do
    System_Btn[I] := UIButton(GetWidget(System_Name_Btn+IntToString(I)));

  If Not Assigned(System_Wnd) Then
  Begin
    System_Wnd := UIWindow.Create(System_Name_Wnd, Self, Nil, 0, 0, 97, UIPixels(500), UIPixels(200));
    System_Wnd.Visible := False;
    System_Text := UILabel.Create(System_Name_Text, Self, System_Wnd, 20, 20, 0.5, '??');
    For I:=0 To 2 Do
    Begin
      Case I Of
      0:  N := 0;
      1:  N := -100;
      2:  N := 100;
      Else
      	N := 0;
      End;

      System_Btn[I] := UIButton.Create(System_Name_Btn+IntToString(I), Self, System_Wnd, N, 20, 0.5, 'Ok');
      System_Btn[I].Align := waBottomCenter;
    End;
  End;

  System_BG := UISprite(GetWidget(System_Name_BG));
  If Not Assigned(System_BG) Then
  Begin
    S := UISprite.Create(System_Name_BG, Self, Nil, 0, 0, 96.5);
    S.Rect.Texture := TextureManager.Instance.WhiteTexture;
    S.Rect.Width := UIManager.Instance.Width;
    S.Rect.Height := UIManager.Instance.Height;
    S.Color := ColorGrey(0, 100);
    S.Visible := False;
    System_BG := S;
  End;*)
End;

Procedure TERRAUI.InitStuff();
Var
  I:Integer;
Begin
  InitTempWidgets();
  System_Wnd.Align := waCenter;
  Modal := System_Wnd;

  System_BG.Visible := True;
  System_Wnd.Visible := True;

(*  For I:=0 To 2 Do
    System_Btn[I].OnMouseClick := CloseMsgBox;
    TODO

  System_Btn[2].OnMouseClick := CloseMsgBox2;
    *)
End;

Procedure TERRAUI.MessageBox(Msg:TERRAString; Callback: WidgetEventHandler);
Var
  I:Integer;
Begin
  _WndCallback1 := Callback;
  InitStuff();

  (*TODO
  UILabel(System_Text).Caption := Msg;
  For I:=0 To 2 Do
    System_Btn[I].Visible := (I=0);

  _PrevHighlight := Highlight;
  If (Highlight<>Nil) Then
    Highlight := System_Btn[0];*)
End;


Procedure TERRAUI.ChoiceBox(Msg, Option1, Option2:TERRAString; Callback1:WidgetEventHandler = Nil; Callback2: WidgetEventHandler = Nil);
Var
  I:Integer;
Begin
  _WndCallback1 := Callback1;
  _WndCallback2 := Callback2;
  InitStuff();

  (*TODO
  UILabel(System_Text).Caption := System_Text._FontRenderer.AutoWrapText(Msg, System_Wnd.Size.X - 30);
  For I:=0 To 2 Do
    System_Btn[I].Visible := (I>0);

  UIButton(System_Btn[1]).Caption := Option1;
  UIButton(System_Btn[2]).Caption := Option2;

  _PrevHighlight := Highlight;
    Highlight := System_Btn[1];*)
End;

Procedure TERRAUI.ShowModal(W:Widget);
Begin
  If W = Nil Then
    Exit;

  InitTempWidgets();
  System_BG.Visible := True;

  If Not W.Visible Then
    W.Visible := True;

  Modal := W;

  If (Highlight<>Nil) Then
    Highlight := W;
End;

Procedure TERRAUI.ClearChoiceBox();
Begin
  Modal := Nil;

  If Assigned(System_BG) Then
    System_BG.Visible := False;

  If Assigned(System_Wnd) Then
    System_Wnd.Visible := False;
End;

Function TERRAUI.GetVirtualKeyboard: Widget;
Begin
  If (_VirtualKeyboard = Nil) Then
    _VirtualKeyboard := UIVirtualKeyboard.Create('vkb', Self, 97, 'keyboard');

  Result := _VirtualKeyboard;
End;

Procedure TERRAUI.OnOrientationChange;
Var
  MyWidget:Widget;
  It:Iterator;
Begin
  _Language := Application.Instance.Language;
  It := _Widgets.GetIterator();
  While (It.HasNext) Do
  Begin
    MyWidget := Widget(It.Value);
    If (MyWidget.Parent =  Nil) Then
      MyWidget._TransformChanged := True;
  End;
  ReleaseObject(It);
End;

Function TERRAUI.GetHighlight: Widget;
Begin
  If (Assigned(_Highlight)) And (Not _Highlight.Visible) Then
    _Highlight := Nil;

  Result := _Highlight;
End;

Procedure TERRAUI.SetTransform(const M: Matrix3x3);
Var
  It:Iterator;
  W:Widget;
Begin
  _Transform := M;
  _InverseTransform := MatrixInverse2D(M);

  _ClipRect.Style := clipSomething;
  _ClipRect.X := 0;
  _ClipRect.Y := 0;
  _ClipRect.Width := UIManager.Instance.Width;
  _ClipRect.Height := UIManager.Instance.Height;
  _ClipRect.Transform(M);

  It := Self.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);

    W._TransformChanged := True;
  End;
  ReleaseObject(It);
End;

Function TERRAUI.GetFontRenderer():FontRenderer;
Begin
  If Self._FontRenderer = Nil Then
    Self._FontRenderer := UIManager.Instance.FontRenderer;

  Result := Self._FontRenderer;
  Result.SetFont(Self._DefaultFont);
  Result.SetTransform(Self.Transform);
  Result.SetColor(ColorWhite);
  Result.SetClipRect(_ClipRect);
  Result.SetDropShadow(ColorGrey(0, 64));
End;

Function TERRAUI.OnRegion(X, Y: Integer): Boolean;
Begin
  Result := (X>=0) And (Y>=0) And (X<=UIManager.Instance.Width) And (Y<=UIManager.Instance.Height);
End;

Procedure TERRAUI.SetFontRenderer(const Value: FontRenderer);
Begin
  _FontRenderer := Value;
End;

Function TERRAUI.SelectNearestWidget(Target:Widget):Widget;
Var
  It:Iterator;
  Base:Vector2D;
  GroupID:Integer;
  Min, Dist:Single;
  W:Widget;
Begin
  Result := Nil;
  If Target = Nil Then
    Exit;

  Min := 99999;
  Base := Target.AbsolutePosition;
  GroupID := Target.HighlightGroup;

  It := Self.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (W = Target) Or (Not W.CanHighlight(GroupID)) Then
      Continue;

    Dist := W.AbsolutePosition.Distance(Base);
    If (Dist< Min) Then
    Begin
      Min := Dist;
      Result := W;
    End;
  End;
  ReleaseObject(It);
End;

Procedure TERRAUI.GetFirstHighLight(GroupID:Integer);
Var
  W:Widget;
  It:Iterator;
Begin
  It := Self.Widgets.GetIterator();
  While It.HasNext() Do
  Begin
    W := Widget(It.Value);
    If (Not W.CanHighlight(GroupID)) Then
      Continue;

    Self.Highlight := W;
    Break;
  End;
  ReleaseObject(It);
End;

Function TERRAUI.LoadImage(Name:TERRAString):TextureAtlasItem;
Var
  I:Integer;
  Source, Temp:Image;
  MyStream:Stream;
  S:TERRAString;
  Ext:ImageClassInfo;
Begin
  Name := GetFileName(Name, True);

  Log(logDebug, 'UI', 'Getting '+Name);
  Result := UIManager.Instance.GetTextureAtlas.Get(Name);
  If Assigned(Result) Then
    Exit;

  Log(logDebug, 'UI', 'Searching icons');
  S := '';
  I := 0;
  While (S='') And (I<GetImageExtensionCount()) Do
  Begin
    Ext := GetImageExtension(I);
    S := FileManager.Instance.SearchResourceFile(Name+'.'+Ext.Name);
    Inc(I);
  End;

  Log(logDebug, 'Game', 'Got '+S);
  If S<>'' Then
  Begin
    Log(logDebug, 'Game', 'Opening stream');
    MyStream := FileManager.Instance.OpenStream(S);
    Log(logDebug, 'Game', 'Creating image: '+S);

    Source := Image.Create(MyStream);
    Log(logDebug, 'Game', 'Image created: '+IntToString(Source.Width)+'x'+IntToString(Source.Height));

    Log(logDebug, 'Game', 'Adding to TextureAtlas');

    Result := UIManager.Instance.GetTextureAtlas.Add(Source, Name);
    UIManager.Instance._UpdateTextureAtlas := True;

    Log(logDebug, 'Game', 'TextureAtlas added');

    ReleaseObject(Source);
    ReleaseObject(MyStream);
  End Else
  Begin
    Log(logWarning,'UI', 'UI component not found. ['+Name+']');
    Result := Nil;
  End;
End;

Function TERRAUI.GetComponent(Const Name:TERRAString): UISkinComponent;
Begin
  If (Name = '') Then
    Result := Nil
  Else
    Result := _Skin.GetChildByName(Name);
End;

Function TERRAUI.LoadSkin(const FileName: TERRAString):Boolean;
Var
  Root:XMLNode;
  Location:TERRAString;
  I:Integer;
Begin
  Result := False;
  Location := FileManager.Instance.SearchResourceFile(FileName+'.xml');
  If Location = '' Then
    Exit;

  Root := XMLNode.Create();
  Root.LoadFromFile(Location);

  ReleaseObject(_Skin);
  _Skin := UISkinComponent.Create(Root, Nil);
  ReleaseObject(Root);

  Result := True;
End;

Function TERRAUI.GetModal: Widget;
Begin
  If (Assigned(_Modal)) And (Not _Modal.Visible) Then
  Begin
    Self.Modal := Nil;
  End;

  Result := Self._Modal;
End;

Function TERRAUI.GetPropertyByIndex(Index: Integer): TERRAObject;
Var
  P:Widget;
Begin
  P := _First;
  While (Index>0) And (Assigned(P)) Do
  Begin
    Dec(Index);
    P := P._Next;
  End;

  Result := P;
End;

Function TERRAUI.GetObjectType: TERRAString;
Begin
  Result := 'UI';
End;

{ UIManager }
Procedure UIManager.Init;
Begin
  _TextureAtlas := Nil;
  _Ratio := 1.0;
  _UpdateTextureAtlas := False;

  _AlignEnums := EnumCollection.Create();
  _AlignEnums.Add('TopLeft', waTopLeft);
  _AlignEnums.Add('TopCenter', waTopCenter);
  _AlignEnums.Add('TopRight', waTopRight);
  _AlignEnums.Add('LeftCenter', waLeftCenter);
  _AlignEnums.Add('Center', waCenter);
  _AlignEnums.Add('RightCenter', waRightCenter);
  _AlignEnums.Add('BottomLeft', waBottomLeft);
  _AlignEnums.Add('BottomCenter', waBottomCenter);
  _AlignEnums.Add('BottomRight', waBottomRight);

  // make UI view
  _Viewport := TERRAViewport.Create('UI', GraphicsManager.Instance.UI_Width, GraphicsManager.Instance.UI_Height, {$IFDEF FRAMEBUFFEROBJECTS}GraphicsManager.Instance.UI_Scale{$ELSE}1.0{$ENDIF});
  _Viewport.BackgroundColor := ColorNull;
  _Viewport.SetRenderTargetState(captureTargetColor, True);
  _Viewport.SetTarget(GraphicsManager.Instance.DeviceViewport, 0, 0, 1.0, 1.0);

  GraphicsManager.Instance.AddViewport(_Viewport);
End;

Procedure UIManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UICount) Do
    ReleaseObject(_UIList[I]);

  _UICount := 0;

  ReleaseObject(_FontRenderer);
  ReleaseObject(_TextureAtlas);

  ReleaseObject(_AlignEnums);

  _UIManager_Instance := Nil;
End;

Procedure UIManager.AddUI(UI:TERRAUI);
Var
  I:Integer;
Begin
  If (UI = Nil) Then
    Exit;

  For I:=0 To Pred(_UICount) Do
  If (_UIList[I] = UI) Then
    Exit;

  Inc(_UICount);
  SetLength(_UIList, _UICount);
  _UIList[Pred(_UICount)] := UI;
End;

Procedure UIManager.RemoveUI(UI:TERRAUI);
Var
  I:Integer;
Begin
  I := 0;
  While (I<_UICount) Do
  If (_UIList[I] = UI) Then
  Begin
    _UIList[I] := _UIList[Pred(_UICount)];
    Dec(_UICount);
  End Else
    Inc(I);
End;

Procedure UIManager.Render();
Var
  Flags:Cardinal;
  Target:RenderTargetInterface;
  Graphics:GraphicsManager;
  Projection:Matrix4x4;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginUIRendering');{$ENDIF}

  Graphics := GraphicsManager.Instance;

  _Viewport.BackgroundColor := ColorNull;
  Target := _Viewport.GetRenderTarget(captureTargetColor);
  If Assigned(Target) Then
  Begin
    Target.BackgroundColor := _Viewport.BackgroundColor;
    _Viewport.SetViewArea(0, 0, Target.Width, Target.Height);
    Target.BeginCapture();

    {$IFDEF PC_X}
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
    {$ENDIF}


    Graphics.Renderer.SetBlendMode(blendBlend);
    Graphics.SetFog(False);
    //glEnable(GL_SCISSOR_TEST);

    Projection := Matrix4x4Ortho(0.0, _Viewport.Width, _Viewport.Height, 0.0, -100, 100);
    Projection := Matrix4x4Multiply4x4(Projection, Matrix4x4Translation(0.375, 0.375, 0.0));

    _Viewport.SetViewArea(0, 0, _Viewport.Width, _Viewport.Height);

    Graphics.Renderer.ClearBuffer((Not Assigned(Graphics.Scene)), True, True);

    If (Not _Prefetching) Then
    Begin
      UIManager.Instance.RenderUIs();

      If (Assigned(Graphics.Scene)) And (Not Application.Instance.HasFatalError) Then
       Graphics.Scene.RenderSprites(_Viewport);
    End;

    SpriteManager.Instance.Render(Projection);

    If ( Not _Prefetching) Then
    Begin
      UIManager.Instance.AfterEffects();
    End;

//  glDisable(GL_SCISSOR_TEST);
  //glDisable(GL_ALPHA_TEST);

    Target.EndCapture();
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'FinishedUIRendering');{$ENDIF}
End;

Function UIManager.GetTextureAtlas: TextureAtlas;
Begin
  If (Not Assigned(_TextureAtlas)) Then
    _TextureAtlas := TERRA_TextureAtlas.TextureAtlas.Create('UI', TextureAtlasWidth, TextureAtlasHeight);

  Result := _TextureAtlas;
End;

Function UIManager.GetWidth: Integer;
Begin
  {If GraphicsManager.Instance.LandscapeOrientation Then
  Begin
    Result := GraphicsManager.Instance.Height;
  End Else
  Begin
    Result := GraphicsManager.Instance.Width;
  End;}
  Result := Self.Viewport.Width;
End;

Function UIManager.GetHeight: Integer;
Begin
  {If GraphicsManager.Instance.LandscapeOrientation Then
  Begin
    Result := GraphicsManager.Instance.Width;
  End Else
  Begin
    Result := GraphicsManager.Instance.Height;
  End;}
  Result := Self.Viewport.Height;
End;

{Procedure UIManager.Adjust(Width, Height: Integer);
Var
  A,B:Integer;
Begin
  A := IntMin(Self.Width, Self.Height);
  B := IntMax(Width, Height);
  If (A<=B) Then
    Self._DefaultFont.BilinearFilter := True;
  GraphicsManager.Instance.AdjustRatio(Width, Height);
End;}


Procedure UIManager.TextureAtlasClear();
Begin
  _UpdateTextureAtlas := True;
End;

Class Function UIManager.Instance: UIManager;
Begin
  If (_UIManager_Instance = Nil) Then
    _UIManager_Instance := InitializeApplicationComponent(UIManager, Nil);

  Result := UIManager(_UIManager_Instance.Instance);
End;

Procedure UIManager.OnLanguageChange;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UICount) Do
    _UIList[I].UpdateLanguage();
End;

Procedure UIManager.Resume;
Begin
  _UpdateTextureAtlas := True;
End;

Procedure UIManager.RenderUIs();
Var
  I:Integer;
Begin
  If (_UpdateTextureAtlas) Then
  Begin
    Log(logDebug, 'UI', 'Updating UI TextureAtlas');

    Self.GetTextureAtlas.Update();
    _UpdateTextureAtlas := False;

    For I:=0 To Pred(_TextureAtlas.PageCount) Do
      _TextureAtlas.GetTexture(I).Filter := filterBilinear;
  End;

  For I:=0 To Pred(_UICount) Do
  If (_UIList[I].Visible) Then
    _UIList[I].Render();
End;

Procedure UIManager.AfterEffects;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UICount) Do
  If _UIList[I].Visible Then
    _UIList[I].AfterEffects();
End;

Procedure UIManager.OnOrientationChange;
Var
  I:Integer;
Begin
  If (Viewport = Nil) Then
    Exit;

  UpdateRatio();
  For I:=0 To Pred(_UICount) Do
    _UIList[I].OnOrientationChange;
End;

Procedure UIManager.UpdateRatio;
Begin
 (* If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
    _Ratio := (Height/Width)
  Else*)
    _Ratio := 1.0;
End;

Function UIManager.GetUI(Index: Integer):TERRAUI;
Begin
  If (Index<0) Or (Index>=Count) Then
    Result := Nil
  Else
    Result := Self._UIList[Index];
End;

Procedure UIManager.SetFontRenderer(const Value: FontRenderer);
Begin
  If _FontRenderer = FontRenderer Then
    Exit;

  _FontRenderer := FontRenderer;
End;

Function UIManager.GetFontRenderer: FontRenderer;
Begin
  If _FontRenderer = Nil Then
    _FontRenderer := TERRA_FontRenderer.FontRenderer.Create();

  Result := _FontRenderer;
End;

Procedure UIManager.OnAppResize;
Var
  UIW, UIH:Integer;
Begin
  UIW := GraphicsManager.Instance.UI_Width;
  UIH := GraphicsManager.Instance.UI_Height;
  If (_Viewport.Width<>UIW) Or (_Viewport.Height<>UIH) Then
    _Viewport.Resize(UIW, UIH);
End;

Function UIManager.CreateProperty(Owner:TERRAObject; const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  If (StringEquals(ObjectType, 'UI')) Then
    Result := TERRAUI.Create()
  Else
    Result := Nil;
End;

End.
