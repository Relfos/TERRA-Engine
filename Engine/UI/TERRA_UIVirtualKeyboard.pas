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
Uses TERRA_UI, TERRA_Utils, TERRA_Color, TERRA_Log, TERRA_Localization;

Const
  // key types
  keyNormal   = 0;
  keySpecial  = 1;
  keyLarge    = 2;

  MaxKeyboardRows = 16;
  MaxKeyboardLines = 5;

Type
  KeyboardLayoutKey = Record
    Value:Word;
    Alt:Word;
  End;

  KeyboardLayout = Class
    Protected
      _Desc:AnsiString;
      _Lines:Array[0..Pred(MaxKeyboardRows), 0..MaxKeyboardLines-2] Of KeyboardLayoutKey;
      _HasShift:Boolean;
      _HasPinyin:Boolean;

    Public
  End;

  UIVirtualKeyboardKey = Class(Widget)
    Protected
      _Label:AnsiString;
      _Suggestion:Integer;
      _Row:Integer;
      _Line:Integer;
      _KeyType:Integer;


      Procedure StartHighlight; Override;
      Procedure StopHighlight; Override;

      Function HasMouseOver():Boolean; Override;

      Function GetKeyWidth():Integer;
      Function GetKeyHeight():Integer;

			Procedure OnLanguageChange(); Override;

    Public
      Callback:WidgetEventHandler;

      Constructor Create(Parent:Widget; UI:UI; X, Y, Z: Single);

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
      Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;
  End;

  VirtualKeyboard = Class(Widget)
    Protected
      _KbScale:Single;

      _PreviousLayout:Integer;
      _CurrentLayout:Integer;
      _QueuedLayout:Integer;

      _LayoutContext:Integer;

      _ShiftMode:Boolean;

      _Keys:Array[0..Pred(MaxKeyboardRows), 0..Pred(MaxKeyboardLines)] Of UIVirtualKeyboardKey;
      _BackKey:UIVirtualKeyboardKey;
      _EnterKey:UIVirtualKeyboardKey;

      _PreviousHighlight:Widget;

      _Pinyin:PinyinConverter;

      Function AddKey(Row, Line:Integer):UIVirtualKeyboardKey; Overload;
      Function AddKey(KeyType:Integer; Name:AnsiString; Row, Line:Integer; Callback:WidgetEventHandler):UIVirtualKeyboardKey; Overload;


      Procedure StartHighlight; Override;
      Procedure StopHighlight; Override;

      Function HasMouseOver():Boolean; Override;

      Procedure Enable;

      Function GetKeyAdvance(Key:UIVirtualKeyboardKey):Single;

      Procedure DoKeyEvent(Row,Line:Integer);

      Procedure UpdatePinyin();

    Public
      Constructor Create(Name:AnsiString; UI:UI; Z:Single);
      Destructor Destroy; Override;

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Function OnMouseDown(X,Y:Integer; Button:Word):Boolean; Override;
      Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;

      Procedure RestorePosition();

      Procedure SelectKeyboardLayout(Index:Integer);

      Function GetKeyValue(Row, Line:Integer):Word;

      Procedure ShowFocus();

      Procedure Close;
  End;

Procedure AddDefaultKeyboardLayout();
Procedure AddKeyboardLayout(SourceFile:AnsiString);
Procedure AddKeyboardLayoutFromString(Src:AnsiString);

Function GetKeyboardLayout(ID:AnsiString):KeyboardLayout;

Procedure ClearKeyboardLayouts();

Const
  DefaultKeyboardLayout = 'ABC|qwertyuiop|asdfghjkl||zxcvbnm!?|QWERTYUIOP|ASDFGHJKL||ZXCVBNM!?';
  DefaultSymbolLayout = '123|1234567890|_#&$ELY()/||-+*":;=%[]';

Implementation
Uses TERRA_Vector2D, TERRA_Vector3D, TERRA_SpriteManager, TERRA_SoundManager,
  TERRA_Widgets, TERRA_Texture, TERRA_Application, TERRA_OS, TERRA_FileManager,
  TERRA_IO, TERRA_Unicode;

Var
  _KeyboardLayouts:Array Of KeyboardLayout;
  _KeyboardLayoutCount:Integer;
  _KeyboardLayoutContext:Integer;

Procedure KeyEventDispatcher(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
  I:Integer;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);

  SoundManager.Instance.Play('ui_key');
  If Assigned(Key.Callback) Then
    Key.Callback(Key)
  Else
  If Assigned(Application.Instance.Client) Then
  Begin
    If (Key._Label<>'') Then
    Begin
      For I:=1 To Length(Key._Label) Do
        Application.Instance.Client.OnKeyPress(Ord(Key._Label[I]));
    End Else
      VKB.DoKeyEvent(Key._Row, Key._Line);
  End;
End;

Function StringToValue(S:AnsiString):Integer;
Begin
  If (S='') Then
    Result := 0
  Else
    Result := Ord(S[1]);
End;

{ VirtualKeyboard }
Function VirtualKeyboard.AddKey(Row, Line:Integer):UIVirtualKeyboardKey;
Begin
  Result := AddKey(keyNormal, '', Row, Line, Nil);
End;

Function VirtualKeyboard.AddKey(KeyType:Integer; Name:AnsiString; Row, Line:Integer; Callback:WidgetEventHandler):UIVirtualKeyboardKey;
Var
  I:Integer;
  Key:UIVirtualKeyboardKey;
Begin
  Key := UIVirtualKeyboardKey.Create(Self, UI, 0, 0, 0.5);
  Key._KeyType := KeyType;
  Key.Callback := Callback;
  Key._Suggestion := -1;
  Key._Name := UpStr('key_'+IntToString(Row)+'_'+IntToString(Line));
  Key._Label := Name;
  Key._Row := Row;
  Key._Line := Line;
  _Keys[Row, Line] := Key;

  Result := Key;
End;

Procedure BackCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  If Assigned(Application.Instance.Client) Then
  Begin
    Application.Instance.Client.OnKeyPress(keyBackspace);

    VKB.UpdatePinyin();
  End;
End;

Procedure EnterCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  Application.Instance.Client.OnKeyPress(keyEnter);

  If (VKB.UI.Focus<>Nil) And (VKB.UI.Focus Is UIEditText) And (UIEditText(VKB.UI.Focus).LineCount=1) Then
    VKB.Close();
End;

Procedure CloseCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  VKB.Close();
End;

Procedure ShiftCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  VKB._ShiftMode := Not VKB._ShiftMode;
End;

Procedure LanguageCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  Inc(VKB._CurrentLayout);
  If (VKB._CurrentLayout>=_KeyboardLayoutCount) Then
    VKB._CurrentLayout := 1;

  VKB.SelectKeyboardLayout(VKB._CurrentLayout);
End;

Procedure SymbolsCallback(W:Widget); Cdecl;
Var
  Key:UIVirtualKeyboardKey;
  VKB:VirtualKeyboard;
Begin
  Key := UIVirtualKeyboardKey(W);
  If (Key = Nil) Then
    Exit;

  VKB := VirtualKeyboard(Key.Parent);
  If (VKB = Nil) Then
    Exit;

  If (VKB._CurrentLayout=0) Then
    VKB.SelectKeyboardLayout(VKB._PreviousLayout)
  Else
  Begin
    VKB._PreviousLayout := VKB._CurrentLayout;
    VKB.SelectKeyboardLayout(0);
  End;
End;

Constructor VirtualKeyboard.Create(Name:AnsiString; UI:UI; Z: Single);
Var
  I:Integer;
Begin
  Self.UpdateRects();

  Self._LayoutContext := -1;
  Self._Visible := True;
  Self._TabIndex := -1;
  Self._Name := Name;
  Self._Parent := Parent;
  Self.RestorePosition();
  Self._Layer := Z;

  Self.LoadComponent('ui_kb_bg');

  UI.AddWidget(Self);


  _CurrentLayout := 0;

  For I:=0 To (MaxKeyboardRows-2) Do
    AddKey(I, 0);
  _BackKey := AddKey(keySpecial, 'Back', Pred(MaxKeyboardrows), 0, BackCallback);

  For I:=0 To (MaxKeyboardRows-2) Do
    AddKey(I, 1);
  _EnterKey := AddKey(keySpecial, 'Enter', Pred(MaxKeyboardrows), 1, EnterCallback);

  For I:=0 To Pred(MaxKeyboardRows) Do
    AddKey(I, 2);

  AddKey(keySpecial, 'Shift', 0, 3, ShiftCallback);
  For I:=1 To (MaxKeyboardRows-1) Do
    AddKey(I, 3);

  AddKey(keySpecial, '?123', 0, 4, SymbolsCallback);
  AddKey(keySpecial, 'Lang', 1, 4, LanguageCallback);
  AddKey(keySpecial, '@', 2, 4, Nil);
  AddKey(keyLarge, ' ', 3, 4, Nil);
  AddKey(keySpecial, ',', 4, 4, Nil);
  AddKey(keySpecial, '.', 5, 4, Nil);
  AddKey(keySpecial, 'Close', 6, 4, CloseCallback);

  _QueuedLayout := -1;
  _CurrentLayout := -1;
  Visible := False;
End;

{Class Function VirtualKeyboard.Instance: VirtualKeyboard;
Begin
  If (_VKB = Nil) Then
  Begin
    _VKB := VirtualKeyboard.Create('_vkb', 97);
    _VKB.Visible := False;
  End;
  Result := _VKB;
End;}

Function VirtualKeyboard.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Var
  I,J:Integer;
Begin
  For J:=0 To Pred(MaxKeyboardLines) Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    If (_Keys[I,J]<>Nil) And (_Keys[I,J].Visible) And (_Keys[I,J].OnMouseUp(X, Y, Button)) Then
    Begin
      Result := True;
      Exit;
    End;

  Result := False;
End;

Procedure VirtualKeyboard.Render;
Var
  I,J:Integer;
Begin
  If (Self.Visible) And (_QueuedLayout>=0) Then
  Begin
    Self.SelectKeyboardLayout(_QueuedLayout);
    _QueuedLayout := -1;
  End;

  If (Self._LayoutContext <> _KeyboardLayoutContext) Then
    Self.SelectKeyboardLayout(_CurrentLayout);

  Self._ColorTable := TextureManager.Instance.DefaultColorTable;

  If (UI.Highlight <> Nil) And (Not (UI.Highlight Is UIVirtualKeyboardKey)) Then
    UI.Highlight := Nil;

  Self.UpdateTransform();

  For I:=0 To 31 Do
    Self.DrawComponent(0, VectorCreate(I*32, 0, 0), 0.0, 0.0, 1.0, 1.0, ColorWhite);

  For J:=0 To Pred(MaxKeyboardLines) Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    If (_Keys[I,J]<>Nil) And (_Keys[I,J].Visible) Then
    Begin
      _Keys[I,J].Scale := _KbScale;
      _Keys[I,J].Render();
    End;
End;

Procedure VirtualKeyboard.RestorePosition;
Var
  Y:Single;
Begin
  Y := UIManager.Instance.Height - Self.Size.Y;
  Self._Position := VectorCreate2D(0, Y);
End;

Procedure VirtualKeyboard.ShowFocus;
Begin
  If (_KeyboardLayoutCount<=0) Then
    AddDefaultKeyboardLayout();

  If (Not Self.Visible) Then
  Begin
    Self.RestorePosition();
    Self.Show(widgetAnimatePosY_Bottom);

    Enable();
  End;

  If (Self._CurrentLayout<0) Then
    SelectKeyboardLayout(1);
End;

Procedure VirtualKeyboard.UpdateRects;
Begin
  _Size.X := 960;
  _Size.Y := 260;
End;

Function VirtualKeyboard.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Begin
  RemoveHint(X+Y+Button); //TODO - check this stupid hint
  Result := False;
End;

Procedure VirtualKeyboard.Enable;
Begin
  _PreviousHighlight := UI.Highlight;
  If (UI.Highlight<>Nil) Then
    UI.Highlight := _Keys[0, 0];

  //UI.Modal := Self;
End;

Procedure VirtualKeyboard.StartHighlight; Begin End;
Procedure VirtualKeyboard.StopHighlight; Begin End;

Procedure VirtualKeyboard.Close;
Begin
  Self.Hide(widgetAnimatePosY_Bottom);
  If (UI.Modal = Self) Then
    UI.Modal := Nil;
  UI.Highlight := _PreviousHighlight;
End;

Function VirtualKeyboard.HasMouseOver: Boolean;
Begin
  Result := False;
End;

Function VirtualKeyboard.GetKeyValue(Row, Line: Integer): Word;
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

Function VirtualKeyboard.GetKeyAdvance(Key:UIVirtualKeyboardKey):Single;
Begin
  Result := _KbScale * (4 + Key.GetKeyWidth());
End;

Procedure VirtualKeyboard.SelectKeyboardLayout(Index: Integer);
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
      IsVisible := (GetKeyValue(_Keys[I,J]._Row, _Keys[I,J]._Line)>0) Or (_Keys[I,J]._Label<>'');

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
  Y := (Self.Size.Y - Y) * 0.5;
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

      IsVisible := (GetKeyValue(_Keys[I,J]._Row, _Keys[I,J]._Line)>0) Or (_Keys[I,J]._Label<>'');

      If (@_Keys[I,J].Callback = @LanguageCallback) Then
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
          _Keys[I,J].Callback := ShiftCallback;
          _Keys[I,J]._Label := 'Shift';
        End Else
        Begin
          _Keys[I,J].Callback := Nil;
          _Keys[I,J]._Label := '';
        End;
      End;

      If (@_Keys[I,J].Callback = @ShiftCallback) Then
      Begin
        IsVisible := (_KeyboardLayouts[_CurrentLayout]._HasShift);
      End;

      If IsVisible Then
      Begin
        _Keys[I,J].Visible := True;
        X := X + GetKeyAdvance(_Keys[I,J]);

      If (PrevRow>=0) Then
        UI.WrapControlsVertical(_Keys[I,PrevRow], _Keys[I,J]);

      End Else
        _Keys[I,J].Visible := False;
    End;

    If (X<=0) Then
      Continue;

    PrevRow := J;
    X := (Self.Size.X - X) * 0.5;

    For I:=0 To Pred(MaxKeyboardRows) Do
    If (Assigned(_Keys[I,J])) And (_Keys[I,J].Visible) Then
    Begin
      _Keys[I,J].Position := VectorCreate2D(X, Y);
      X := X + GetKeyAdvance(_Keys[I,J]);
    End;

    Y := Y + 64 * _KbScale;;
  End;

  For J:=0 To Pred(MaxKeyboardLines) Do
    For I:=0 To Pred(MaxKeyboardRows) Do
    Begin
      If (I>=Pred(MaxKeyboardRows)) Or (Not _Keys[Succ(I),J].Visible) Then
      Begin
        Case J Of
        0:
          Begin
            UI.WrapControlsHorizontal(_Keys[I,J], _BackKey);
            UI.WrapControlsHorizontal(_BackKey, _Keys[0,J]);
          End;

        1:
          Begin
            UI.WrapControlsHorizontal(_Keys[I,J], _EnterKey);
            UI.WrapControlsHorizontal(_EnterKey, _Keys[0,J]);
          End;

        Else
          UI.WrapControlsHorizontal(_Keys[I,J], _Keys[0, J]);
        End;

        Break;
      End;

      N := (I+1) Mod MaxKeyboardRows;
      UI.WrapControlsHorizontal(_Keys[I,J], _Keys[N, J]);
    End;

  For I:=0 To 5 Do
    UI.WrapControlsHorizontal(_Keys[I,4], _Keys[Succ(I), 4]);
End;

Destructor VirtualKeyboard.Destroy;
Begin
  {If Assigned(Pinyin) Then
    Pinyin.Destroy();}

  Inherited;
End;

Procedure VirtualKeyboard.DoKeyEvent(Row, Line: Integer);
Var
  Value:Word;
  Text:AnsiString;
Begin
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
    Application.Instance.Client.OnKeyPress(Value);

  Self.UpdatePinyin();
End;

Procedure VirtualKeyboard.UpdatePinyin;
Var
  Text:AnsiString;
  N,I:Integer;
Begin
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
End;

{ UIVirtualKeyboardKey }
Constructor UIVirtualKeyboardKey.Create(Parent:Widget; UI:UI; X, Y, Z: Single);
Begin
  Self._Visible := True;
  Self._TabIndex := -1;
  Self._Name := 'key';
  Self._Parent := Parent;
  Self._Position := VectorCreate2D(X,Y);
  Self._Layer := Z;

  Self.LoadComponent('ui_kb_a');
  Self.LoadComponent('ui_kb_b');
  Self.LoadComponent('ui_kb_c');

  Self.OnMouseClick := KeyEventDispatcher;

  UI.AddWidget(Self);

  Self._ColorTable := TextureManager.Instance.DefaultColorTable;
End;

Function UIVirtualKeyboardKey.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Begin
  RemoveHint(X+Y+Button); //TODO - check this stupid hint
  Result := False;
End;

Function UIVirtualKeyboardKey.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Begin
  RemoveHint(Button); //TODO - check this stupid hint

  If (Not Self.OnRegion(X,Y)) Then
  Begin
    Result := False;
    Exit;
  End;

  Self.OnMouseClick(Self);
  Result := True;
End;

Function UIVirtualKeyboardKey.HasMouseOver():Boolean;
Begin
  Result := False;
End;

Function UIVirtualKeyboardKey.GetKeyHeight(): Integer;
Begin
  If (_ComponentCount<=0) Then
    Result := 0
  Else
    Result := _ComponentList[_KeyType].Buffer.Height;
End;

Function UIVirtualKeyboardKey.GetKeyWidth(): Integer;
Begin
  If (_ComponentCount<=0) Then
    Result := 0
  Else
    Result := _ComponentList[_KeyType].Buffer.Width;
End;

Procedure UIVirtualKeyboardKey.Render;
Var
  W, H:Integer;
  SS:AnsiString;
  MyColor:TERRA_Color.Color;
  HG:Widget;
  TextScale:Single;
  Value:Word;
Begin
  HG := UI.Highlight;
  If (HG = Nil) Then
    UI.Highlight := Self;

  _Pivot := VectorCreate2D(0, 0.5);

  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

  If (Self.IsHighlighted) Or (UI.Highlight = Nil) Then
    MyColor := ColorWhite
  Else
    MyColor := ColorGrey(32);

  DrawComponent(_KeyType, VectorZero, 0.0, 0.0, 1.0, 1.0, MyColor, True);
  W := Self.GetKeyWidth();
  H := Self.GetKeyHeight();

  If (_Label<>'') Then
  Begin
    SS := _Label;
    TextScale := 1.0;
  End Else
  Begin
    Value := VirtualKeyboard(Parent).GetKeyValue(_Row, _Line);
    If Value = Ord('@') Then
    Begin
      If (_Suggestion>=0) And (Assigned(VirtualKeyboard(Parent)._Pinyin)) Then
      Begin
         Value := VirtualKeyboard(Parent)._Pinyin.GetResult(_Suggestion);
        SS := UnicodeToUCS2(Value);
        IntToString(_Row+_Line);
      End Else
        SS := '';
      TextScale := 1.0;
    End Else
    Begin
      SS := UnicodeToUCS2(Value);
      TextScale := 1.0 /Self.Scale;
    End;
  End;

  If (SS<>'') Then
    Self.DrawText(SS, VectorCreate((W-Self.Font.GetTextWidth(SS, TextScale))*0.5, (H-Self.Font.GetTextHeight(SS, TextScale))*0.5,1), ColorWhite, TextScale);
End;

Procedure UIVirtualKeyboardKey.StartHighlight; Begin End;
Procedure UIVirtualKeyboardKey.StopHighlight; Begin End;
Procedure UIVirtualKeyboardKey.OnLanguageChange; Begin End;

Procedure UIVirtualKeyboardKey.UpdateRects;
Begin
  _Size.X := GetKeyWidth();
  _Size.Y := GetKeyHeight();
End;

Procedure ClearKeyboardLayouts();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_KeyboardLayoutCount) Do
  Begin
    _KeyboardLayouts[I].Destroy();;
  End;

  _KeyboardLayoutCount := 0;
End;

Function GetKeyboardLayout(ID:AnsiString):KeyboardLayout;
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

Procedure AddKeyboardLayout(SourceFile:AnsiString);
Var
  Src:Stream;
  S,Data:AnsiString;
Begin
  Src := FileManager.Instance.OpenStream(SourceFile);
  If Src = Nil Then
    Exit;

  Data := '';
  S := '';
  While Not Src.EOF Do
  Begin
    Src.ReadUnicodeLine(S);
    Data := Data + S + '|';
  End;
  Src.Destroy();

  AddKeyboardLayoutFromString(Data);
End;

Procedure AddKeyboardLayoutFromString(Src:AnsiString);
Var
  Layout:KeyboardLayout;
  N,I,J,K:Integer;
  A,B:Byte;
  Value:Word;
  Desc, Line:AnsiString;
  IsSymbol:Boolean;
Begin
  Desc := ucs2_GetNextWord(Src, '|');
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
      Layout._Lines[I, J].Value := 0;
      Layout._Lines[I, J].Alt := 0;
    End;

  For K:=1 To 2 Do
  Begin
    For J:=0 To MaxKeyboardLines-2 Do
    Begin
      Line := ucs2_GetNextWord(Src, '|');

      If (Line='') And (K=2) Then
      Begin
        For I:=0 To Pred(MaxKeyboardRows) Do
          Layout._Lines[I, J].Alt := Layout._Lines[I, J].Value;
        Continue;
      End;

      N := 1;
      I := 0;
      While N<=Length(Line) Do
      Begin
        If (Line[N]=#255) Then
        Begin
          Inc(N); A := Ord(Line[N]);
          Inc(N); B := Ord(Line[N]);

          Value := A * 256 + B;
        End Else
        Begin
          Value := Ord(Line[N]);

          If (IsSymbol) And (Line[N]='E') Then
            Value := 8364; // euro symbol
          If (IsSymbol) And (Line[N]='L') Then
            Value := 163; // pound symbol
          If (IsSymbol) And (Line[N]='Y') Then
            Value := 165; // yen symbol

          If (Line[N]='@') Then
            Layout._HasPinyin := True;
        End;

        If (K=2) Then
        Begin
          Layout._Lines[I, J].Alt := Value;
          Layout._HasShift := True;
        End Else
          Layout._Lines[I, J].Value := Value;

        Inc(N);
        Inc(I);
        If (I>=MaxKeyboardRows) Then
          Break;
      End;

    End;
  End;
End;

End.
