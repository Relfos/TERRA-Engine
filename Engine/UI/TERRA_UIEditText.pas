Unit TERRA_UIEditText;

{$I terra.inc}

Interface

Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_Collections, TERRA_Viewport, TERRA_UILabel;

Type
  UIEditText = Class(UILabel)
    Protected
      _LineCount:Integer;
      _Lines:Array Of TERRAString;
      _LineIndex:Integer;
      _MaxLines:Integer;

      _KoreanBaseJamo:Integer;
      _KoreanInitialJamo:Integer;
      _KoreanMedialJamo:Integer;
      _KoreanFinalJamo:Integer;

      _InsideEvent:Boolean;

      Procedure UpdateJamos();

      Function IsSelectable():Boolean; Override;

      //Procedure UpdateSprite(View:TERRAViewport); Override;

      Procedure OnStateChange(); Override;

    Public
      OnEnter:WidgetEventHandler;
      OnChange:WidgetEventHandler;
      PasswordField:Boolean;
      Centered:Boolean;

      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const Text:TERRAString);

      Procedure SetText(Const Value:TERRAString);
      Function GetText:TERRAString;

      Procedure SetLineCount(const Value: Integer);

      Function GetCurrentLine:TERRAString;
      Procedure SetCurrentLine(const Value:TERRAString);

      Function OnKeyPress(Key:TERRAChar):Boolean; Override;

      Property Text:TERRAString Read GetText Write SetText;
      Property Line:TERRAString Read GetCurrentLine Write SetCurrentLine;
      Property LineCount:Integer Read _LineCount Write SetLineCount;
  End;


Implementation
Uses TERRA_Application, TERRA_OS, TERRA_Log, TERRA_UIView, TERRA_Localization
{$IFDEF VIRTUALKEYBOARD},TERRA_UIVirtualKeyboard{$ENDIF};

{ UIEditText }
Constructor UIEditText.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const Text:TERRAString);
Begin
  Inherited Create(Name, Parent, X,Y,Z, Width, Height, Text);

  Self.SetLineCount(1);
  Self._LineIndex := 0;
  Self._MaxLines := 0;

  Self.PasswordField := False;

  Self._KoreanInitialJamo := -1;
  Self._KoreanMedialJamo := -1;
  Self._KoreanFinalJamo := -1;
End;

Procedure UIEditText.SetLineCount(const Value: Integer);
Begin
  If (LineCount = Value) Then
    Exit;

  _LineCount := Value;
  _MaxLines := 1;
  SetLength(_Lines, _LineCount);

  Self.UpdateRects();
End;

Procedure UIEditText.UpdateJamos;
Var
  Jamo:Word;
  N:Integer;
Begin
  System.Delete(_Lines[_LineIndex], Length(_Lines[_LineIndex])-2, 3);
  If (_KoreanMedialJamo>=0) Then
  Begin
    If (_KoreanFinalJamo>=0) Then
      N := _KoreanFinalJamo
    Else
      N := 0;
    Jamo := (_KoreanInitialJamo*588)+(_KoreanMedialJamo*28)+N+44032;
  End Else
    Jamo := _KoreanBaseJamo;

  StringAppendChar(_Lines[_LineIndex], Jamo);
End;

Function UIEditText.OnKeyPress(Key:TERRAChar):Boolean;
Var
  I, Len:Integer;
  //KeyValue:TERRAString;
  W,W2:Single;
  ChangedLine, Found:Boolean;
  It:Iterator;
  Wd:UIWidget;
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

  If (Key = keyBackspace) Then
  Begin
    W := FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');

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
      Len := StringLength(_Lines[_LineIndex]);

      // check for font control chars/effects
      If (Len>=2) And (StringGetChar(_Lines[_LineIndex], -1) = Ord('\')) Then
      Begin
        StringDropChars(_Lines[_LineIndex], -2);
      End Else
      If (_Lines[_LineIndex]<>'') Then
      Begin
        I := Len;
        If (StringLastChar(_Lines[_LineIndex]) = Ord('}')) Then
        Begin
          While (I>=1) Do
          If (StringGetChar(_Lines[_LineIndex], I) = Ord('\')) Then
            Break
          Else
            Dec(I);

          If (I>0) Then
            _Lines[_LineIndex] := StringCopy(_Lines[_LineIndex], 1, Pred(I))
          Else
            _Lines[_LineIndex] := StringCopy(_Lines[_LineIndex], 1, Pred(Len));
        End Else
          _Lines[_LineIndex] := StringCopy(_Lines[_LineIndex], 1, Pred(Len));
      End Else
      If (_LineCount>1) And (_LineIndex>0) Then
      Begin
        Dec(_LineIndex);
        ChangedLine := True;
        W := FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');
      End;
    End;

  End Else
  If (Key = keyEnter) Then
  Begin
    If (_LineCount>1) And (_LineIndex<Pred(_LineCount)) Then
    Begin
      Inc(_LineIndex);
      _MaxLines := _LineIndex;
    End;

    If Assigned(OnEnter) Then
      OnEnter(Self);
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
    If (Assigned(Self.Font)) Then
    Begin
      W := FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');

      If (_KoreanInitialJamo<0) Or (_KoreanFinalJamo>=0) Then
      Begin
        _KoreanInitialJamo := GetKoreanInitialJamo(Key);
        _KoreanMedialJamo := -1;
        _KoreanFinalJamo := -1;
        _KoreanBaseJamo := Key;
        StringAppendChar(_Lines[_LineIndex], Key);
      End Else
      If (_KoreanMedialJamo<0) And (_KoreanFinalJamo<0) Then
      Begin
        _KoreanMedialJamo := GetKoreanMedialJamo(Key);
        If _KoreanMedialJamo<0 Then
        Begin
          _KoreanInitialJamo := GetKoreanInitialJamo(Key);
          StringAppendChar(_Lines[_LineIndex], Key);
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
          StringAppendChar(_Lines[_LineIndex], Key);
        End Else
          UpdateJamos();
      End Else
      Begin
        StringAppendChar(_Lines[_LineIndex], Key);
      End;

      (*
      // advance scroll
      W2 := FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');
      If (W2>Self.GetDimension(Self.Width, uiDimensionWidth)) And (W2>W) Then
        _ScrollIndex := _ScrollIndex + (W2-W);*)
    End;
  End;

  If (Assigned(OnChange)) And (Not _InsideEvent) Then
  Begin
    _InsideEvent := True;
    OnChange(Self);
    _InsideEvent := False;
  End;


  Self._Caption.Value := _Lines[_LineIndex];

  Result := True;
End;

Procedure UIEditText.SetText(const Value:TERRAString);
Begin
  Self._KoreanInitialJamo := -1;
  Self._KoreanMedialJamo := -1;
  Self._KoreanFinalJamo := -1;

  If (_LineCount<1) Then
    SetLineCount(1);

  Self._LineIndex := 0;
  Self.SetCurrentLine(Value);
End;

Function UIEditText.GetText:TERRAString;
Var
  I:Integer;
Begin
  If (_LineCount=1) Then
    Result := _Lines[0]
  Else
  Begin
    Result := '';
    For I:=0 To _LineIndex Do
    Begin
      Result := Result + _Lines[I];
      If (I<_LineIndex) Then
        Result := Result + '\n';
    End;
  End;
End;

Function UIEditText.GetCurrentLine:TERRAString;
Begin
  Result := _Lines[_LineIndex];
End;

Procedure UIEditText.SetCurrentLine(const Value:TERRAString);
Begin
  _Lines[_LineIndex] := ConvertFontCodes(Value);
  If (Assigned(OnChange)) And (Not _InsideEvent) Then
  Begin
    _InsideEvent := True;
    OnChange(Self);
    _InsideEvent := False;
  End;
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

End.