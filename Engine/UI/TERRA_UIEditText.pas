Unit TERRA_UIEditText;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_ClipRect, TERRA_UICaption, TERRA_Collections;

Type
  UIEditText = Class(UICaption)
    Protected
      _LineCount:Integer;
      _Lines:Array Of TERRAString;
      _LineIndex:Integer;
      _MaxLines:Integer;

      _ScrollIndex:Single;
      _Clip:ClipRect;

      {_HorScroll:UIScrollBar;
      _VertScroll:UIScrollBar;}

      _KoreanBaseJamo:Integer;
      _KoreanInitialJamo:Integer;
      _KoreanMedialJamo:Integer;
      _KoreanFinalJamo:Integer;

      _InsideEvent:Boolean;

      Procedure UpdateJamos();

      Function UpdateTransform():Boolean; Override;

      Function IsSelectable():Boolean; Override;

    Public
      OnEnter:WidgetEventHandler;
      OnChange:WidgetEventHandler;
      PasswordField:Boolean;
      Centered:Boolean;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;

      Procedure SetFocus(ShowKeyboard:Boolean);

      Procedure SetText(Const Value:TERRAString);
      Function GetText:TERRAString;

      Procedure SetLineCount(const Value: Integer);

      Function GetCurrentLine:TERRAString;
      Procedure SetCurrentLine(const Value:TERRAString);

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
      Procedure OnMouseUp(X,Y:Integer;Button:Word); Override;
      Function OnKeyPress(Key:Word):Boolean; Override;

      Property Text:TERRAString Read GetText Write SetText;
      Property Line:TERRAString Read GetCurrentLine Write SetCurrentLine;
      Property LineCount:Integer Read _LineCount Write SetLineCount;
  End;


Implementation
Uses TERRA_Application, TERRA_OS, TERRA_Log, TERRA_Localization
{$IFDEF VIRTUALKEYBOARD},TERRA_UIVirtualKeyboard{$ENDIF};

{ UIEditText }
Constructor UIEditText.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self.Width := Width;
  Self.Height := Height;

  Self.TabIndex := TabIndex;
  Self.Text := '';

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

  {If (LineCount>1) And (_HorScroll=Nil) Then
  Begin
    _HorScroll := UIScrollBar.Create(Name+'_horizontal_scroll', UI, Self, 0, 5, 1, 1, True);
    _HorScroll.Align := waBottomCenter;
    Repeat
      _HorScroll._Length := _HorScroll._Length + 1;
      _HorScroll.UpdateRects();
    Until (_HorScroll.Size.X>=Self.Size.X - 100);
  End;

  If (LineCount>1) And (_VertScroll=Nil) Then
  Begin
    _VertScroll := UIScrollBar.Create(Name+'_vertical_scroll', UI, Self, 5, 0, 1, 1, False);
    _VertScroll.Align := waRightCenter;
    Repeat
      _VertScroll._Length := _VertScroll._Length + 1;
      _VertScroll.UpdateRects();
    Until (_VertScroll.Size.Y>=Self.Size.Y - _Height);
  End;}
End;

Procedure UIEditText.SetFocus(ShowKeyboard:Boolean);
Begin
  UI.Focus := Self;
  If Not ShowKeyboard Then
    Exit;

  {$IFDEF MOBILE}
  {$IFDEF VIRTUALKEYBOARD}
  If (Not UseNativeKeyboard) Then
    VirtualKeyboard(UI.VirtualKeyboard).ShowFocus()
  Else
  {$ENDIF}
  focusKeyboard(PAnsiChar(Self.Text));
  {$ELSE}
  {$IFDEF VIRTUALKEYBOARD}
  UIVirtualKeyboard(UI.VirtualKeyboard).ShowFocus();
  {$ENDIF}
  {$ENDIF}
End;

Procedure UIEditText.OnMouseUp(X, Y: Integer; Button: Word);
Begin
  RemoveHint(X+Y+Button); //TODO - check this stupid hint
  {If (Assigned(Self._HorScroll)) And (_HorScroll.OnRegion(X,Y)) Then
  Begin
    Result := _HorScroll.OnMouseUp(X, Y, Button);
    Exit;
  End;

  If (Assigned(Self._VertScroll)) And (_VertScroll.OnRegion(X,Y)) Then
  Begin
    Result := _VertScroll.OnMouseUp(X, Y, Button);
    Exit;
  End;}
End;

Procedure UIEditText.OnMouseDown(X, Y: Integer; Button: Word);
Var
  I:Integer;
Begin
  If Not Visible Then
    Exit;

  RemoveHint(Button); //TODO - check this stupid hint

  {If (Assigned(Self._HorScroll)) And (_HorScroll.OnRegion(X,Y)) Then
  Begin
    Result := _HorScroll.OnMouseDown(X, Y, Button);
    Exit;
  End;

  If (Assigned(Self._VertScroll)) And (_VertScroll.OnRegion(X,Y)) Then
  Begin
    Result := _VertScroll.OnMouseDown(X, Y, Button);
    Exit;
  End;}

  //Pos := Self.GetAbsolutePosition();
  SetFocus(True);
End;

Function UIEditText.UpdateTransform:Boolean;
Var
  P:Vector2D;
Begin
  Result := Inherited UpdateTransform;
  If Not Result Then
    Exit;

  P := Self.AbsolutePosition;

  _Clip.X := P.X;
  _Clip.Y := P.Y;
  _Clip.Width := Size.X;
  _Clip.Height := Size.Y;
//  _Clip.Name := Self.Name;

  _Clip.Transform(Self.UI.Transform);

  Self._ClipRect := _Clip;
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

Function UIEditText.OnKeyPress(Key:Word): Boolean;
Var
  I, Len:Integer;
  //KeyValue:TERRAString;
  W,W2:Single;
  ChangedLine, Found:Boolean;
  It:Iterator;
  Wd:Widget;
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
    W := _FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');

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
        W := _FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');

        If (W>Self.GetDimension(Self.Width, uiDimensionWidth)) Then
          _ScrollIndex := W - Self.GetDimension(Self.Width, uiDimensionWidth)
        Else
          _ScrollIndex := 0;
      End;
    End;

    W2 := _FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');
    If (Not ChangedLine) And (_ScrollIndex>0) And (W2<W) Then
      _ScrollIndex := _ScrollIndex - (W-W2);
  End Else
  If (Key = keyEnter) Then
  Begin
    If (_LineCount>1) And (_LineIndex<Pred(_LineCount)) Then
    Begin
      Inc(_LineIndex);
      _ScrollIndex := 0;
      _MaxLines := _LineIndex;
    End;

    If Assigned(OnEnter) Then
      OnEnter(Self);
  End Else
  If (Key = keyTab) Then
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
  End Else
  Begin
    //KeyValue := UnicodeToUCS2(Key);

    If (Assigned(Self.Font)) Then
    Begin
      W := _FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');

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

      W2 := _FontRenderer.GetTextWidth(_Lines[_LineIndex] + '_');
      If (W2>Self.GetDimension(Self.Width, uiDimensionWidth)) And (W2>W) Then
        _ScrollIndex := _ScrollIndex + (W2-W);
    End;
  End;

  If (Assigned(OnChange)) And (Not _InsideEvent) Then
  Begin
    _InsideEvent := True;
    OnChange(Self);
    _InsideEvent := False;
  End;

  Result := True;
End;

Procedure UIEditText.SetText(const Value:TERRAString);
Begin
  Self._KoreanInitialJamo := -1;
  Self._KoreanMedialJamo := -1;
  Self._KoreanFinalJamo := -1;

  If (_LineCount<1) Then
    SetLineCount(1);

  Self._ScrollIndex := 0;
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

Procedure UIEditText.Render;
Var
  I,J,N, Count:Integer;
  A,B,X, HH:Single;
  S:TERRAString;
  P, TextRect:Vector2D;

  Procedure DrawLine(Column:Integer; Const S:TERRAString);
  Var
    Y:Single;
  Begin
    TextRect := _FontRenderer.GetTextRect(S);

    If (Centered) Then
      X := (Self.Size.X - TextRect.X)*0.5
    Else
      X := 0;

    Y := HH * Column + HH*0.5;

    If (Y + TextRect.Y>Self.GetDimension(Self.Height, uiDimensionHeight)) Then
    Begin
      Y := 0;
    End;

    Self.DrawText(S, X-_ScrollIndex, Y, 1.0, TextRect, Scale, 0, Self.IsSelected, ColorWhite);
  End;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  P := Self.AbsolutePosition;

  {If (UI.Highlight<>Nil) And (Not Self.IsHighlighted())
  And (UI.Focus=Self) And (Pos('KEY_', UI.Highlight.Name)<>1) Then
    UI.Focus := Nil;}

  Self.UpdateHighlight((UI.Focus = Self));

  If (UI.Focus <> Self) And (Self.IsHighlighted()) Then
    Self.SetFocus(False);

  Self.DrawComponent(0, 0, 0, Self.Width, Self.Height, 0, Self.IsSelected);

  HH := _FontRenderer.GetTextHeight('W', 1.0) * 1.1;

  Count := 0;
  For J:=0 To Pred(_LineCount) Do
  If (_Lines[J]<>'') Then
  Begin
    Inc(Count);

    If PasswordField Then
    Begin
      SetLength(S, StringLength(_Lines[J]));
      For I:=1 To Length(S) Do
        S[I] := '*';
    End Else
      S := _Lines[J];

    If (UI.Focus = Self) And (J=_LineIndex) And (Blink(200)) Then
      S := S +'_';

    DrawLine(J, S);
  End;

  If (Caption.Value <>'') And (Count = 0) Then
  Begin
    DrawLine(0, Caption.Value);
  End;

  Inherited;
End;


End.