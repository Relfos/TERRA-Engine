Unit TERRA_UIEditText;

{$I terra.inc}

Interface

Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_Collections, TERRA_Viewport, TERRA_UIText;

Const
  PasswordCharacter = Ord('*'); 

Type
  UIEditText = Class(UIText)
    Private
      _KoreanBaseJamo:Integer;
      _KoreanInitialJamo:Integer;
      _KoreanMedialJamo:Integer;
      _KoreanFinalJamo:Integer;

      Procedure UpdateJamos();

      Function GetMultiLine: Boolean;
      Function GetPasswordField: Boolean;
      Procedure SetMultiline(const Value: Boolean);
      Procedure SetPasswordField(const Value: Boolean);

      Procedure SetText(Const Value:TERRAString);
      Function GetText():TERRAString;
      
    Protected
      _Content:StringProperty;
      _MultiLine:BooleanProperty;
      _PasswordField:BooleanProperty;

      Function IsSelectable():Boolean; Override;

      Procedure UpdateSprite(View:TERRAViewport); Override;

      Procedure OnStateChange(); Override;

    Public
      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const Text:TERRAString);

      Function OnHandleKeyPress(Key:TERRAChar):Boolean; Override;

      Property MultiLine:Boolean Read GetMultiLine Write SetMultiline;
      Property PasswordField:Boolean Read GetPasswordField Write SetPasswordField;
      Property Content:TERRAString Read GetText Write SetText;
  End;


Implementation
Uses TERRA_Application, TERRA_OS, TERRA_Log, TERRA_UIView, TERRA_Localization
{$IFDEF VIRTUALKEYBOARD},TERRA_UIVirtualKeyboard{$ENDIF};

{ UIEditText }
Constructor UIEditText.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const Text:TERRAString);
Begin
  Inherited Create(Name, Parent, X,Y,Z, Width, Height);

  _Content := StringProperty(Self.AddProperty(StringProperty.Create('content', Text), False));
  _Multiline := BooleanProperty(Self.AddProperty(BooleanProperty.Create('multiline', False), False));
  _PasswordField := BooleanProperty(Self.AddProperty(BooleanProperty.Create('password', False), False));

  Self._KoreanInitialJamo := -1;
  Self._KoreanMedialJamo := -1;
  Self._KoreanFinalJamo := -1;
End;

Procedure UIEditText.UpdateJamos;
Var
  Jamo:Word;
  N:Integer;
  S:TERRAString;
Begin
  S := StringCopy(_Content.Value, 1, StringLength(_Content.Value)-3);
  If (_KoreanMedialJamo>=0) Then
  Begin
    If (_KoreanFinalJamo>=0) Then
      N := _KoreanFinalJamo
    Else
      N := 0;
    Jamo := (_KoreanInitialJamo*588)+(_KoreanMedialJamo*28)+N+44032;
  End Else
    Jamo := _KoreanBaseJamo;

  StringAppendChar(S, Jamo);
  _Content.Value := S;
End;

Function UIEditText.OnHandleKeyPress(Key:TERRAChar):Boolean;
Var
  I, Len:Integer;
  ChangedLine, Found:Boolean;
  It:Iterator;
  Wd:UIWidget;
  S:TERRAString;
Begin
  If (Not Self.Visible) Or (Self.HasPropertyTweens()) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Key = keyShift) Or (Key = keyControl) Or (Key = keyAlt) Then
  Begin
    Result := False;
    Exit;
  End;

  {$IFDEF DEBUG_CORE}Log(logDebug, 'UI', 'EditText: Got key : '+IntToString(Key));{$ENDIF}
  {$IFDEF DEBUG_CORE}Log(logDebug, 'UI', 'Backspace Is  '+IntToString(keyBackspace));{$ENDIF}

  ChangedLine := False;

  S := _Content.Value;

  If (Key = keyBackspace) Then
  Begin
    If (_KoreanFinalJamo>=0) Then
    Begin
      _KoreanFinalJamo := -1;
      UpdateJamos();
    End Else
    If (_KoreanMedialJamo>=0) Then
    Begin
      _KoreanMedialJamo := -1;
      UpdateJamos();
    End Else
    Begin
      _KoreanInitialJamo := -1;
      Len := StringLength(S);

      // check for font control chars/effects
      If (Len>=2) And (StringGetChar(S, -1) = Ord('\')) Then
      Begin
        StringDropChars(S, -2);
      End Else
      If (S<>'') Then
      Begin
        I := Len;
        If (StringLastChar(S) = Ord('}')) Then
        Begin
          While (I>=1) Do
          If (StringGetChar(S, I) = Ord('\')) Then
            Break
          Else
            Dec(I);

          If (I>0) Then
            S := StringCopy(S, 1, Pred(I))
          Else
            S := StringCopy(S, 1, Pred(Len));
        End Else
          S := StringCopy(S, 1, Pred(Len));
      End;
    End;

  End Else
  If (Key = keyEnter) Then
  Begin
    If (Self.MultiLine) Then
    Begin
      StringAppendChar(S, NewLineChar);
    End; // Else     Self.TriggerEvent(widgetEvent_Select);
  End Else
(*TODO  If (Key = keyTab) Then
  Begin
    Found := False;
    It := UI.Widgets.GetIterator();
    While It.HasNext Do
    Begin
      Wd := Widget(It.Value);
      If (Wd.Visible) And (Wd<>Self) And (Wd Is UIEditText) And (WD.AbsolutePosition.Y > Self.AbsolutePosition.Y) Then
      Begin
        UIEditText(Wd).SetFocus(True);
        Found := True;
        Break;
      End;
    End;
    ReleaseObject(It);

    If Not Found Then
    Begin
      It := UI.Widgets.GetIterator();
      While It.HasNext Do
      Begin
        Wd := Widget(It.Value);
        If (Wd.Visible) And (Wd<>Self) And (Wd Is UIEditText) And (WD.AbsolutePosition.Y <= Self.AbsolutePosition.Y) Then
        Begin
          UIEditText(Wd).SetFocus(True);
          Found := True;
          Break;
        End;
      End;
      ReleaseObject(It);
    End;
  End Else*)
  Begin
      If (_KoreanInitialJamo<0) Or (_KoreanFinalJamo>=0) Then
      Begin
        _KoreanInitialJamo := GetKoreanInitialJamo(Key);
        _KoreanMedialJamo := -1;
        _KoreanFinalJamo := -1;
        _KoreanBaseJamo := Key;
        StringAppendChar(S, Key);
      End Else
      If (_KoreanMedialJamo<0) And (_KoreanFinalJamo<0) Then
      Begin
        _KoreanMedialJamo := GetKoreanMedialJamo(Key);
        If _KoreanMedialJamo<0 Then
        Begin
          _KoreanInitialJamo := GetKoreanInitialJamo(Key);
          StringAppendChar(S, Key);
        End Else
          UpdateJamos();
      End Else
      If (_KoreanFinalJamo<0) Then
      Begin
        _KoreanFinalJamo := GetKoreanFinalJamo(Key);

        If (_KoreanFinalJamo<0) Then
        Begin
          _KoreanInitialJamo := GetKoreanInitialJamo(Key);
          _KoreanMedialJamo := -1;
          StringAppendChar(S, Key);
        End Else
          UpdateJamos();
      End Else
      Begin
        StringAppendChar(S, Key);
      End;

      (*
      // advance scroll
      W2 := FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');
      If (W2>Self.GetDimension(Self.Width, uiDimensionWidth)) And (W2>W) Then
        _ScrollIndex := _ScrollIndex + (W2-W);*)
  End;

  If (Not StringEquals(S, _Content.Value, False)) Then
  Begin
    _Content.Value := S;
    Self.TriggerEvent(widgetEvent_ContentChange);
  End;

  Result := True;
End;

Procedure UIEditText.SetText(const Value:TERRAString);
Begin
  Self._KoreanInitialJamo := -1;
  Self._KoreanMedialJamo := -1;
  Self._KoreanFinalJamo := -1;

  Self._Content.Value := Value;
End;

Function UIEditText.GetText: TERRAString;
Begin
  Result := _Content.Value;
End;

Function UIEditText.IsSelectable: Boolean;
Begin
  Result := True;
End;

Procedure UIEditText.OnStateChange;
Begin
  If (Self.State = widget_Selected) Then
  Begin
    UIView(Self.View).SetFocus(Self);
  End;
End;

Function UIEditText.GetMultiLine: Boolean;
Begin
  Result := _MultiLine.Value;
End;

Function UIEditText.GetPasswordField: Boolean;
Begin
  Result := _PasswordField.Value;
End;

Procedure UIEditText.SetMultiline(const Value: Boolean);
Begin
  _MultiLine.Value := Value;
End;

procedure UIEditText.SetPasswordField(const Value: Boolean);
Begin
  _PasswordField.Value := Value;
End;

Procedure UIEditText.UpdateSprite(View: TERRAViewport);
Begin
  If (Self._PasswordField.Value) Then
  Begin
    _Text := StringFill(StringLength(_Content.Value), PasswordCharacter);
  End Else
    _Text := _Content.Value;

  Inherited;
End;


End.
