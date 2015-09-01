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
 * TERRA_UIVirtualKeyboard
 * Implements a virtual keyboard widget
 ***********************************************************************************************************************
}

Unit TERRA_UIVirtualKeyboard;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UIDimension, TERRA_UIWidget, TERRA_UIImage, TERRA_UITiledRect, TERRA_UIText, TERRA_UILabel, TERRA_Color,
  TERRA_Log, TERRA_Viewport, TERRA_Renderer, TERRA_Localization;

Type
  KeyboardKeyType = (
    KeyType_Normal,
    KeyType_Special,
    KeyType_Space,
    KeyType_Shift,
    KeyType_Language
  );

Const
  MaxKeyboardRows = 16;
  MaxKeyboardLines = 5;

Type
  KeyboardLayoutKey = Record
    Value:TERRAChar;
    Alt:TERRAChar;
  End;

  KeyboardLayout = Class(TERRAObject)
    Protected
      _Desc:TERRAString;
      _Lines:Array[0..Pred(MaxKeyboardRows), 0..MaxKeyboardLines-2] Of KeyboardLayoutKey;
      _HasShift:Boolean;
      _HasPinyin:Boolean;

    Public
  End;

  UIVirtualKeyboardKey = Class(UIInstancedWidget)
    Protected
      _Label:TERRAString;
      _Suggestion:Integer;
      _Row:Integer;
      _Line:Integer;
      _KeyType:KeyboardKeyType;

      Procedure OnKeyDispatch(Src:UIWidget);

    Public
      Constructor Create(Parent:UIWidget; Const X, Y:UIDimension; Const Layer: Single; Const TemplateName:TERRAString);
  End;

  UIVirtualKeyboard = Class(UIWidget)
    Protected
      //_KeyTemplate:StringProperty;

      _KbScale:Single;

      _PreviousLayout:Integer;
      _CurrentLayout:Integer;
      _QueuedLayout:Integer;

      _LayoutContext:Integer;

      _ShiftMode:Boolean;

      _IsInitialized:Boolean;

      _Keys:Array[0..Pred(MaxKeyboardRows), 0..Pred(MaxKeyboardLines)] Of UIVirtualKeyboardKey;
      _BackKey:UIVirtualKeyboardKey;
      _EnterKey:UIVirtualKeyboardKey;

      _PreviousHighlight:UIWidget;

      _Pinyin:PinyinConverter;

      Function AddKey(Row, Line:Integer; Const TemplateName:TERRAString):UIVirtualKeyboardKey; Overload;
      Function AddKey(KeyType:KeyboardKeyType; Name:TERRAString; Row, Line:Integer; Callback:UIWidgetEventHandler; Const TemplateName:TERRAString):UIVirtualKeyboardKey; Overload;

      Procedure Initialize();

      Procedure Enable;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;

      Function GetKeyAdvance(Key:UIVirtualKeyboardKey):Single;

      Procedure DoKeyEvent(Row,Line:Integer);

      Procedure UpdatePinyin();

      Procedure OnBackKey(Src:UIWidget);
      Procedure OnEnterKey(Src:UIWidget);
      Procedure OnCloseKey(Src:UIWidget);
      Procedure OnShiftKey(Src:UIWidget);
      Procedure OnLanguageKey(Src:UIWidget);
      Procedure OnSymbolsKey(Src:UIWidget);

    Public
      Constructor Create(Name:TERRAString);
      Procedure Release; Override;

      Procedure RestorePosition();

      Procedure SelectKeyboardLayout(Index:Integer);

      Function GetKeyValue(Row, Line:Integer):TERRAChar;

      Procedure ShowFocus();

      Procedure Close;
  End;

Procedure AddDefaultKeyboardLayout();
Procedure AddKeyboardLayout(SourceFile:TERRAString);
Procedure AddKeyboardLayoutFromString(Src:TERRAString);

Function GetKeyboardLayout(ID:TERRAString):KeyboardLayout;

Procedure ClearKeyboardLayouts();

Const
  DefaultKeyboardLayout = 'ABC|qwertyuiop|asdfghjkl||zxcvbnm!?|QWERTYUIOP|ASDFGHJKL||ZXCVBNM!?';
  DefaultSymbolLayout = '123|1234567890|_#&$ELY()/||-+*":;=%[]';

Implementation
Uses TERRA_Error, TERRA_Vector2D, TERRA_Vector3D, TERRA_SoundManager,
  TERRA_Texture, TERRA_Application, TERRA_OS, TERRA_FileManager,
  TERRA_Stream, TERRA_UIEditText, TERRA_UIView, TERRA_EngineManager;

Var
  _KeyboardLayouts:Array Of KeyboardLayout;
  _KeyboardLayoutCount:Integer;
  _KeyboardLayoutContext:Integer;

Function KeyTypeToInt(KT:KeyboardKeyType):Integer;
Begin
  Case KT Of
    KeyType_Normal:
      Result := 0;

    KeyType_Space:
      Result := 2;

    Else
      Result := 1;
  End;
End;

Function StringToValue(S:TERRAString):Integer;
Begin
  If (S='') Then
    Result := 0
  Else
    Result := Ord(S[1]);
End;

{ VirtualKeyboard }
Function UIVirtualKeyboard.AddKey(Row, Line:Integer; Const TemplateName:TERRAString):UIVirtualKeyboardKey;
Begin
  Result := AddKey(KeyType_Normal, '', Row, Line, Self.NullEventHandler, TemplateName);
End;

Function UIVirtualKeyboard.AddKey(KeyType:KeyboardKeyType; Name:TERRAString; Row, Line:Integer; Callback:UIWidgetEventHandler; Const TemplateName:TERRAString):UIVirtualKeyboardKey;
Var
  I:Integer;
  Key:UIVirtualKeyboardKey;
Begin
  Key := UIVirtualKeyboardKey.Create(Self, UIPixels(0), UIPixels(0), 0.5, TemplateName);
  Key._KeyType := KeyType;
  Key.SetEventHandler(widgetEvent_MouseDown, Callback);
  Key._Suggestion := -1;
  Key._ObjectName := StringUpper('key_'+IntegerProperty.Stringify(Row)+'_'+IntegerProperty.Stringify(Line));
  Key._Label := Name;
  Key._Row := Row;
  Key._Line := Line;
  _Keys[Row, Line] := Key;

  Result := Key;
End;

Procedure UIVirtualKeyboard.OnBackKey(Src:UIWidget);
Begin
  If Assigned(Application.Instance()) Then
  Begin
    Application.Instance.OnKeyPress(TERRAChar(keyBackspace));

    Self.UpdatePinyin();
  End;
End;

Procedure UIVirtualKeyboard.OnEnterKey(Src:UIWidget);
Begin
  Application.Instance.OnKeyPress(TERRAChar(keyEnter));

  (*TODO
  If (Self.UI.Focus<>Nil) And (Self.UI.Focus Is UIEditText) And (UIEditText(Self.UI.Focus).LineCount=1) Then
    Self.Close();*)
End;

Procedure UIVirtualKeyboard.OnCloseKey(Src:UIWidget);
Begin
  Self.Close();
End;

Procedure UIVirtualKeyboard.OnShiftKey(Src:UIWidget);
Begin
  Self._ShiftMode := Not Self._ShiftMode;
End;

Procedure UIVirtualKeyboard.OnLanguageKey(Src:UIWidget);
Begin
  Inc(Self._CurrentLayout);
  If (Self._CurrentLayout>=_KeyboardLayoutCount) Then
    Self._CurrentLayout := 1;

  Self.SelectKeyboardLayout(Self._CurrentLayout);
End;

Procedure UIVirtualKeyboard.OnSymbolsKey(Src:UIWidget);
Begin
  If (Self._CurrentLayout=0) Then
    Self.SelectKeyboardLayout(Self._PreviousLayout)
  Else
  Begin
    Self._PreviousLayout := Self._CurrentLayout;
    Self.SelectKeyboardLayout(0);
  End;
End;

Constructor UIVirtualKeyboard.Create(Name:TERRAString);
Begin
  Inherited Create(Name, Nil);

  Self.Left := UIPixels(0);
  Self.Top := UIPixels(0);
  Self.Layer := 98;
  Self.Width := UIPercent(100);
  Self.Height := UIPercent(100);
End;

Procedure UIVirtualKeyboard.Initialize();
Const
  TemplateName = '';
Var
  I:Integer;
Begin
  If Assigned(View) Then
    UIView(View).VirtualKeyboard := Self;

//  Self.Height := UIPixels(320);

  Self._LayoutContext := -1;

  Self.RestorePosition();

  _CurrentLayout := 0;

  For I:=0 To (MaxKeyboardRows-2) Do
    AddKey(I, 0, TemplateName);
  _BackKey := AddKey(KeyType_Special, 'Back', Pred(MaxKeyboardrows), 0, Self.OnBackKey, TemplateName);

  For I:=0 To (MaxKeyboardRows-2) Do
    AddKey(I, 1, TemplateName);
  _EnterKey := AddKey(KeyType_Special, 'Enter', Pred(MaxKeyboardrows), 1, Self.OnEnterKey, TemplateName);

  For I:=0 To Pred(MaxKeyboardRows) Do
    AddKey(I, 2, TemplateName);

  AddKey(KeyType_Shift, 'Shift', 0, 3, Self.OnShiftKey, TemplateName);

  For I:=1 To (MaxKeyboardRows-1) Do
    AddKey(I, 3, TemplateName);

  AddKey(KeyType_Special, '?123', 0, 4, Self.OnSymbolsKey, TemplateName);
  AddKey(KeyType_Language, 'Lang', 1, 4, Self.OnLanguageKey, TemplateName);
  AddKey(KeyType_Special, '@', 2, 4, Self.NullEventHandler, TemplateName);
  AddKey(KeyType_Space, ' ', 3, 4, Self.NullEventHandler, TemplateName);
  AddKey(KeyType_Special, ',', 4, 4, Self.NullEventHandler, TemplateName);
  AddKey(KeyType_Special, '.', 5, 4, Self.NullEventHandler, TemplateName);
  AddKey(KeyType_Special, 'Close', 6, 4, Self.OnCloseKey, TemplateName);

  _QueuedLayout := -1;
  _CurrentLayout := -1;

  _IsInitialized := True;

//  Visible := False;
End;


Procedure UIVirtualKeyboard.Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal);
Var
  I,J:Integer;
Begin
  If Not _IsInitialized Then
    Self.Initialize();

  If (Self.Visible) And (_QueuedLayout>=0) Then
  Begin
    Self.SelectKeyboardLayout(_QueuedLayout);
    _QueuedLayout := -1;
  End;

  If (Self._LayoutContext <> _KeyboardLayoutContext) Then
    Self.SelectKeyboardLayout(_CurrentLayout);

  Self._ColorTable := Engine.Textures.DefaultColorTable;

  Inherited Render(View, Stage, Bucket);

(*  For J:=0 To Pred(MaxKeyboardLines) Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    If (_Keys[I,J]<>Nil) And (_Keys[I,J].Visible) Then
    Begin
      _Keys[I,J].Scale := _KbScale;
      _Keys[I,J].Render();
    End;*)
End;

Procedure UIVirtualKeyboard.RestorePosition;
Begin
(* TODO
  If (Self.UI.Focus<>Nil) And (Self.UI.Focus Is UIEditText) Then
  Begin
    If (UI.Focus.AbsolutePosition.Y >= UIManager.Instance.Height - Self.Size.Y) Then
      Self.Align := waTopLeft
    Else
      Self.Align := waBottomLeft;
  End;

  Self.RelativePosition := VectorCreate2D(0, 0);
  *)
End;

Procedure UIVirtualKeyboard.ShowFocus;
Begin
  If (_KeyboardLayoutCount<=0) Then
    AddDefaultKeyboardLayout();

  If (Not Self.Visible) Then
  Begin
    Self.RestorePosition();
    Self.Visible := True;
    //Self.Show(widgetAnimatePosY_Bottom);

    Enable();
  End;

  If (Self._CurrentLayout<0) Then
    SelectKeyboardLayout(1);
End;

Procedure UIVirtualKeyboard.Enable;
Begin
  Self.Visible := True;
End;

Procedure UIVirtualKeyboard.Close;
Begin
  Self.Visible := False;
End;

Function UIVirtualKeyboard.GetKeyValue(Row, Line: Integer):TERRAChar;
Begin
  If (_CurrentLayout<0) Or (_CurrentLayout>=_KeyboardLayoutCount) Then
    _CurrentLayout := 0;

  If (Line<0) Or (Line>MaxKeyboardLines-2) Then
    Line := 0;

  If (Row<0) Or (Row>=MaxKeyboardRows) Then
    Row := 0;

  If (Self._ShiftMode) Then
    Result := _KeyboardLayouts[_CurrentLayout]._Lines[Row, Line].Alt
  Else
    Result := _KeyboardLayouts[_CurrentLayout]._Lines[Row, Line].Value;
End;

Function UIVirtualKeyboard.GetKeyAdvance(Key:UIVirtualKeyboardKey):Single;
Begin
  //Result := _KbScale * (4 + Key.GetKeyWidth());
  Result := 0;
End;

Procedure UIVirtualKeyboard.SelectKeyboardLayout(Index: Integer);
Var
  I,J,N, Rows, PrevRow:Integer;
  X, Y:Single;
  IsVisible:Boolean;
Begin
  If (_KeyboardLayoutCount<=0) Then
  Begin
    AddDefaultKeyboardLayout();
  End;

  If (Not Self.Visible) Then
  Begin
    _QueuedLayout := Index;
    Exit;
  End;

  Self._CurrentLayout := Index;
  Self._LayoutContext := _KeyboardLayoutContext;

  //OfsX := (UIManager.Instance.Width-960*_KbScale) * 0.5;
  Rows := 0;
  For J:=0 To Pred(MaxKeyboardLines) Do
  Begin
    For I:=0 To Pred(MaxKeyboardRows) Do
    If (Assigned(_Keys[I,J])) Then
    Begin
      IsVisible := (GetKeyValue(_Keys[I,J]._Row, _Keys[I,J]._Line)>NullChar) Or (_Keys[I,J]._Label<>'');

      If IsVisible Then
      Begin
        Inc(Rows);
        Break;
      End;
    End;
  End;

  If (Rows=5) Then
    Self._KbScale := 0.8
  Else
    Self._KbScale := 1.0;

  {$IFDEF OUYA}
  _KbScale := _KbScale * 0.85;
  {$ENDIF}

  Y := 64*_KbScale*Rows;
  Y := (Self.CurrentSize.Y - Y) * 0.5;
  PrevRow := -1;

  For J:=0 To Pred(MaxKeyboardLines) Do
  Begin
    X := 0;

    For I:=0 To Pred(MaxKeyboardRows) Do
    If (Assigned(_Keys[I,J])) Then
    Begin
      If (J=3) And (_KeyboardLayouts[_CurrentLayout]._HasShift) Then
        _Keys[I,J]._Row := Pred(I)
      Else
        _Keys[I,J]._Row := I;

      IsVisible := (GetKeyValue(_Keys[I,J]._Row, _Keys[I,J]._Line)>NullChar) Or (_Keys[I,J]._Label<>'');

      If (_Keys[I,J]._KeyType = KeyType_Language) Then
      Begin
        IsVisible := (_KeyboardLayoutCount>2);
        If (Self._CurrentLayout<Pred(_KeyboardLayoutCount)) Then
          N := Succ(Self._CurrentLayout)
        Else
          N := 1;

        _Keys[I,J]._Label := _KeyboardLayouts[N]._Desc;
      End;

      If (I=0) And (J=3) Then
      Begin
        If (_KeyboardLayouts[_CurrentLayout]._HasShift) Then
        Begin
          _Keys[I,J].SetEventHandler(widgetEvent_MouseDown, Self.OnShiftKey);
          _Keys[I,J]._Label := 'Shift';
        End Else
        Begin
          _Keys[I,J].SetEventHandler(widgetEvent_MouseDown, Nil);
          _Keys[I,J]._Label := '';
        End;
      End;

      If (_Keys[I,J]._KeyType = KeyType_Shift) Then
      Begin
        IsVisible := (_KeyboardLayouts[_CurrentLayout]._HasShift);
      End;

      If IsVisible Then
      Begin
        _Keys[I,J].Visible := True;
        X := X + GetKeyAdvance(_Keys[I,J]);
      End Else
        _Keys[I,J].Visible := False;
    End;

    If (X<=0) Then
      Continue;

    PrevRow := J;
    X := (Self.CurrentSize.X - X) * 0.5;

    For I:=0 To Pred(MaxKeyboardRows) Do
    If (Assigned(_Keys[I,J])) And (_Keys[I,J].Visible) Then
    Begin
      _Keys[I,J].RelativePosition := Vector2D_Create(X, Y);
      X := X + GetKeyAdvance(_Keys[I,J]);
    End;

    Y := Y + 64 * _KbScale;;
  End;

  For J:=0 To Pred(MaxKeyboardLines) Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    Begin
      If (I>=Pred(MaxKeyboardRows)) Or (Not _Keys[Succ(I),J].Visible) Then
        Break;

      N := (I+1) Mod MaxKeyboardRows;
    End;
End;

Procedure UIVirtualKeyboard.Release;
Begin
  {ReleaseObject(Pinyin);}

  Inherited;
End;

Procedure UIVirtualKeyboard.DoKeyEvent(Row, Line: Integer);
Var
  Value:Word;
  Text:TERRAString;
Begin
(*TODO
  If (_Keys[Row, Line]._Suggestion>=0) And (Self._Pinyin<>Nil)
  And (Assigned(UI.Focus)) And (UI.Focus Is UIEditText) Then
  Begin
    Text := UIEditText(UI.Focus).Text;
    If Self._Pinyin.Replace(Text, _Keys[Row, Line]._Suggestion) Then
    Begin
      UIEditText(UI.Focus).Text := Text;
      Self.UpdatePinyin();
    End;
    Exit;
  End;

  Value := Self.GetKeyValue(Row, Line);
  If Value>0 Then
    Application.Instance.OnKeyPress(Value);

  Self.UpdatePinyin();

  TODO
  *)
End;

Procedure UIVirtualKeyboard.UpdatePinyin;
Var
  Text:TERRAString;
  N,I:Integer;
Begin
(*TODO
  If (_KeyboardLayouts[_CurrentLayout]._HasPinyin)
  And (Assigned(UI.Focus)) And (UI.Focus Is UIEditText) Then
  Begin
    If (_Pinyin = Nil) Then
      _Pinyin := PinyinConverter.Create();

    Text := UIEditText(UI.Focus).Text;
    _Pinyin.GetSuggestions(Text);

    For I:=0 To Pred(MaxKeyboardRows) Do
    If Assigned(_Keys[I, 3]) Then
      _Keys[I, 3]._Suggestion := -1;

    N := 0;
    For I:=0 To Pred(_Pinyin.Results) Do
    If (I>Pred(MaxKeyboardRows)) Or (_Keys[I, 3] = Nil) Then
      Break
    Else
    Begin
      _Keys[I, 3]._Suggestion := N;
      Inc(N);
    End;
  End;

  TODO
  *)
End;

{ UIVirtualKeyboardKey }
Constructor UIVirtualKeyboardKey.Create(Parent:UIWidget; Const X, Y:UIDimension; Const Layer: Single; Const TemplateName:TERRAString);
Begin
  Inherited Create(Name, Parent, X, Y, Layer, UIPercent(100), UIPercent(100), TemplateName);

  Self.SetEventHandler(widgetEvent_MouseDown, Self.OnKeyDispatch);
End;

Procedure UIVirtualKeyboardKey.OnKeyDispatch(Src:UIWidget);
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Engine.Audio.Play('ui_key');

(*  If Assigned(Self.Callback) Then
    Self.Callback(Self)
  Else
  If Assigned(Application.Instance()) Then
  Begin
    If (Self._Label<>'') Then
    Begin
      StringCreateIterator(Self._Label, It);
      While It.HasNext Do
      Begin
        Application.Instance.OnKeyPress(It.GetNext());
      End;
    End Else
      UIVirtualKeyboard(Parent).DoKeyEvent(Self._Row, Self._Line);
  End;*)
End;

{Procedure UIVirtualKeyboardKey.Render;
Var
  W, H:Integer;
  SS:TERRAString;
  HG:UIWidget;
  TextScale:Single;
  TextRect:Vector2D;
  Value:Word;
Begin
(* TODO
  HG := UI.Highlight;
  If (HG = Nil) Then
    UI.Highlight := Self;

  _Pivot := VectorCreate2D(0, 0.5);

  Self.UpdateProperties();
  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

 DrawComponent(0, 0, 0, Self.Width, Self.Height, KeyTypeToInt(_KeyType), Self.IsSelected);
  *)
  W := Self.GetKeyWidth();
  H := Self.GetKeyHeight();

  If (_Label<>'') Then
  Begin
    SS := _Label;
    TextScale := 1.0;
  End Else
  Begin
    Value := UIVirtualKeyboard(Parent).GetKeyValue(_Row, _Line);
    SS := '';

    If Value = Ord('@') Then
    Begin
      If (_Suggestion>=0) And (Assigned(UIVirtualKeyboard(Parent)._Pinyin)) Then
      Begin
        Value := UIVirtualKeyboard(Parent)._Pinyin.GetResult(_Suggestion);
        StringAppendChar(SS, Value);
        IntegerProperty.Stringify(_Row+_Line);
      End;

      TextScale := 1.0;
    End Else
    Begin
      StringAppendChar(SS, Value);
      TextScale := 1.0 /Self.Scale;
    End;
  End;

  If (SS<>'') Then
  Begin
    TextRect := FontRenderer.GetTextRect(SS);
(*TODO    Self.DrawText(SS, (W - TextRect.X)*0.5, (H - TextRect.Y)*0.5, 1.0, TextRect, TextScale, KeyTypeToInt(_KeyType), Self.IsSelected, ColorWhite);
*)
  End;
End;}

Procedure ClearKeyboardLayouts();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_KeyboardLayoutCount) Do
    ReleaseObject(_KeyboardLayouts[I]);

  _KeyboardLayoutCount := 0;
End;

Function GetKeyboardLayout(ID:TERRAString):KeyboardLayout;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_KeyboardLayoutCount) Do
  If (_KeyboardLayouts[I]._Desc = ID) Then
  Begin
    Result :=_KeyboardLayouts[I];
    Exit;
  End;

  Result := Nil;
End;

Procedure AddDefaultKeyboardLayout();
Begin
  AddKeyboardLayoutFromString(DefaultSymbolLayout);
  AddKeyboardLayoutFromString(DefaultKeyboardLayout);
End;

Procedure AddKeyboardLayout(SourceFile:TERRAString);
Var
  Src:TERRAStream;
  S,Data:TERRAString;
Begin
  Src := Engine.Files[SourceFile];
  If Src = Nil Then
    Exit;

  Data := '';
  S := '';
  While Not Src.EOF Do
  Begin
    Src.ReadLine(S);
    Data := Data + S + '|';
  End;
  ReleaseObject(Src);

  AddKeyboardLayoutFromString(Data);
End;

Procedure AddKeyboardLayoutFromString(Src:TERRAString);
Var
  Layout:KeyboardLayout;
  I,J,K:Integer;
  It:StringIterator;
  A,B:Byte;
  Value:TERRAChar;
  Desc, Line:TERRAString;
  IsSymbol:Boolean;
Begin
  Desc := StringGetNextSplit(Src, '|');
  IsSymbol := (Desc='123');

  Layout := GetKeyboardLayout(Desc);

  If Layout = Nil Then
  Begin
    Layout := KeyboardLayout.Create();
    Inc(_KeyboardLayoutCount);
    SetLength(_KeyboardLayouts, _KeyboardLayoutCount);
    _KeyboardLayouts[Pred(_KeyboardLayoutCount)] := Layout;
    Layout._Desc := Desc;
  End;

  Inc(_KeyboardLayoutContext);

  Layout._HasShift := False;
  Layout._HasPinyin := False;

  For J:=0 To MaxKeyboardLines-2 Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    Begin
      Layout._Lines[I, J].Value := NullChar;
      Layout._Lines[I, J].Alt := NullChar;
    End;

  For K:=1 To 2 Do
  Begin
    For J:=0 To MaxKeyboardLines-2 Do
    Begin
      Line := StringGetNextSplit(Src, '|');

      If (Line='') And (K=2) Then
      Begin
        For I:=0 To Pred(MaxKeyboardRows) Do
          Layout._Lines[I, J].Alt := Layout._Lines[I, J].Value;
        Continue;
      End;

      I := 0;
      StringCreateIterator(Line, It);
      While It.HasNext() Do
      Begin
        Value := It.GetNext();

        If (IsSymbol) And (Value = 'E') Then
          Value := #8364 // euro symbol
        Else
        If (IsSymbol) And (Value = 'L') Then
          Value := #163 // pound symbol
        Else
        If (IsSymbol) And (Value = 'Y') Then
          Value := #165; // yen symbol

        If (Value= '@') Then
          Layout._HasPinyin := True;

        If (K=2) Then
        Begin
          Layout._Lines[I, J].Alt := Value;
          Layout._HasShift := True;
        End Else
          Layout._Lines[I, J].Value := Value;

        Inc(I);
        If (I>=MaxKeyboardRows) Then
          Break;
      End;

    End;
  End;
End;

End.
