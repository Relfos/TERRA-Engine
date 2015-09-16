Unit TERRA_UILabel;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_Viewport, TERRA_DebugDraw,
  TERRA_UICursor, TERRA_UIText;

Type
  UILabel = Class(UIText)
    Protected
      _NeedCaptionUpdate:Boolean;

      _Stretch:BooleanProperty;
      _Caption:StringProperty;

      _CurrentLink:TERRAString;

      Function GetLocalizationKey: TERRAString;

      Procedure UpdateSprite(); Override;

      Procedure UpdateCaption();

      Procedure SetText(Const S:TERRAString);
      Function GetText():TERRAString;

      Function ReactsToEventClass(Const EventClass:WidgetEventClass):Boolean; Override;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const Text:TERRAString);

      Function SupportDrag(Mode:UIDragMode):Boolean; Override;

      Procedure OnHandleMouseMove(X, Y: Integer); Override;
      Procedure OnHandleMouseUp(X,Y:Integer; Button:Word); Override;

			Procedure OnLanguageChange(); Override;

      Property LocalizationKey:TERRAString Read GetLocalizationKey;

      Property Caption:TERRAString Read GetText Write SetText;
  End;

Implementation
Uses TERRA_Localization, TERRA_OS, TERRA_FontRenderer;

Constructor UILabel.Create(const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const Text:TERRAString);
Begin
  Inherited Create(Name, Parent, X, Y, Layer, Width, Height);

  _Caption := StringProperty(Self.AddProperty(StringProperty.Create('caption', Text), False));
  _Stretch := BooleanProperty(Self.AddProperty(BooleanProperty.Create('stretch', False), False));

  _NeedCaptionUpdate := True;
  
  _ShouldWrap := True;
End;

Function UILabel.GetLocalizationKey: TERRAString;
Begin
  If StringFirstChar(_Caption.Value) = '#' Then
  Begin
    Result := StringCopy(_Caption.Value, 2, MaxInt);
  End Else
    Result := '';
End;

Procedure UILabel.OnLanguageChange;
Begin
  _NeedCaptionUpdate := True;
End;

Procedure UILabel.UpdateSprite();
Begin
//  If (_NeedCaptionUpdate) Then
  Begin
    _NeedCaptionUpdate := False;
    Self.UpdateCaption();
  End;

  _ShouldStretch := _Stretch.Value;

  Inherited UpdateSprite();
End;

Function UILabel.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := (Mode = UIDrag_Move);
End;

Procedure UILabel.UpdateCaption();
Var
  Result, S, S2:TERRAString;
  It:StringIterator;
Begin
  S := Self._Caption.Value;
  Result := '';
  Repeat
    It := StringCharPosIterator(UIMacroBeginChar, S, True);
    If Assigned(It) Then
    Begin
      It.Split(S2, S);
      ReleaseObject(It);
      
      Result := Result + S2;

      S2 := StringGetNextSplit(S, UIMacroEndChar);

      S2 := Self.ResolveMacro(S2);

      Result := Result + S2 + ' ';
    End Else
    Begin
      Result := Result + S;
      Break;
    End;

  Until False;

  _Text := Result;
End;


Procedure UILabel.SetText(const S: TERRAString);
Begin
  Inherited;

  Self.UpdateCaption();
End;

Function UILabel.GetText: TERRAString;
Begin
  Result := _Caption.Value;
End;

Function UILabel.ReactsToEventClass(const EventClass: WidgetEventClass): Boolean;
Begin
  If (_CurrentLink<>'') And ((EventClass = widgetEventClass_Hover) Or (EventClass = widgetEventClass_Click) ) Then
    Result := True
  Else
    Result := Inherited ReactsToEventClass(EventClass);
End;

Procedure UILabel.OnHandleMouseMove(X, Y: Integer);
Var
  Style:TERRAFontCharStyle;
  Hit:Vector2D;
  Temp:TERRAString;
Begin
  Hit := Vector2D_Create(X, Y);
  Self.ConvertGlobalToLocal(Hit);

  If (_FontRenderer.GetTextAt(Hit.X, Hit.Y, Temp, Style)) And (Style.Link<>'') Then
  Begin
    _CurrentLink := Style.Link;
    _CurrentCursor := Cursor_Link;
  End Else
  Begin
    _CurrentLink := '';
    _CurrentCursor := Cursor_Default;
  End;
End;

Procedure UILabel.OnHandleMouseUp(X, Y: Integer; Button: Word);
Begin
  If (_CurrentCursor = Cursor_Link) And (_CurrentLink<>'') Then
    Application.Instance.OpenURL(_CurrentLink);
End;

End.