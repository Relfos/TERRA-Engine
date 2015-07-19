Unit TERRA_UIComboBox;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UIDimension,
  TERRA_Collections;

Type
  UIComboBox = Class(Widget)
    Protected
      _ItemIndex:Integer;
      _ItemHighlight:Integer;
      _ShowList:Boolean;

      _BarWidth:UIDimension;
      _BarHeight:UIDimension;

      _HandleWidth:UIDimension;

      _ListWidth:UIDimension;
      _ListHeight:UIDimension;

      _Content:List;
      _Selected:CollectionObject;

      Function GetItem(Index:Integer):CollectionObject;
      Function GetItemAtIndex(X,Y:Integer):Integer;

      Procedure SetItemIndex(Const Value:Integer);

      Procedure UpdateRects(); Override;

    Public
      ShowLabelOnly:Boolean;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
      Procedure OnMouseMove(X,Y:Integer); Override;

      Procedure SetContent(Content:List);

      Procedure Select(Const Value:TERRAString);

      Property ItemIndex:Integer Read _ItemIndex Write SetItemIndex;
      Property Items[Index:Integer]:CollectionObject Read GetItem;
      Property Selected:CollectionObject Read _Selected;
  End;


Implementation
Uses TERRA_Localization;

{ UIComboBox }

Constructor UIComboBox.Create(Name:TERRAString; Parent:Widget; X, Y, Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self.TabIndex := TabIndex;
  Self.Width := Width;
  Self.Height := Height;
  Self._ItemHighlight := -1;
  Self._Content := Nil;

  _BarWidth := Width;
  _BarHeight := Height;
  _HandleWidth := UIPixels(40);
End;

Procedure UIComboBox.SetContent(Content:List);
Begin
  _Content := Content;
  _ItemIndex := 0;
  _Selected := Nil;
  _ItemHighlight := -1;
End;

Function UIComboBox.GetItem(Index: Integer):CollectionObject;
Begin
  If (_Content<>Nil) And (Index>=0) And (Index<_Content.Count) Then
    Result := _Content.GetItemByIndex(Index)
  Else
    Result := Nil;
End;

Procedure UIComboBox.SetItemIndex(Const Value:Integer);
Begin
  _ItemIndex := Value;
  _Selected := _Content.GetItemByIndex(Value);
End;

Function UIComboBox.GetItemAtIndex(X,Y:Integer):Integer;
Var
  Pos:Vector2D;
  I,HH:Integer;
  X1,X2,Y1,Y2:Integer;
Begin
  Result := -1;
  Pos := Self.AbsolutePosition;

  X1 := Trunc(Pos.X);
  X2 := Trunc(Pos.X + Self.GetDimension(_BarWidth, uiDimensionWidth));

  HH := Trunc(Self.GetDimension(_BarHeight, uiDimensionHeight));

  If (Assigned(_Content)) Then
  For I:=0 To Pred(_Content.Count) Do
  Begin
    Y1 := Trunc(Pos.Y + HH * Succ(I));
    Y2 := Y1 + HH;

    If  (X>=X1) And (X<=X2) And (Y>=Y1) And (Y<=Y2) Then
    Begin
      Result  := I;
      Exit;
    End;
  End;
End;

Procedure UIComboBox.OnMouseDown(X, Y: Integer; Button: Word);
Var
  Pos:Vector2D;
Begin
  RemoveHint(Button); //TODO - check this stupid hint

  Pos := Self.AbsolutePosition;

  If (_ItemHighlight < 0) And (_ShowList) Then
    _ItemHighlight := Self.GetItemAtIndex(X,Y);

  If (_ItemHighlight>=0) Then
  Begin
    SetItemIndex(_ItemHighlight);
    _ItemHighlight := -1;
    _ShowList := False;
    UI.Focus := Nil;

    Self.OnHit(OnMouseClick);
  End Else
  If (X >= Pos.X) And (X <= Pos.X + Self.GetDimension(_BarWidth, uiDimensionWidth) + Self.GetDimension(_HandleWidth, uiDimensionHeight)) And (Y>=Pos.Y) And (Y<=Pos.Y+ Self.GetDimension(_BarHeight, uiDimensionHeight)) And (Not ShowLabelOnly) Then
  Begin
    _ShowList := Not _ShowList;

    If (_ShowList) Then
      UI.Focus := Self;
  End;
End;

Procedure UIComboBox.OnMouseMove(X, Y: Integer);
Begin
  Inherited;

  _ItemHighLight := -1;
  If Not _ShowList Then
    Exit;

  _ItemHighLight := Self.GetItemAtIndex(X,Y);
End;

Procedure UIComboBox.Render;
Var
  YY,WW, HH:Single;
  I,J:Integer;
  P:CollectionObject;
  ZOfs:Single;
  S:TERRAString;
  TextRect:Vector2D;
  BW, HW:Single;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (_ItemIndex<0) Or (_Content = Nil) Then
    Exit;

  BW := Self.GetDimension(_BarWidth, uiDimensionWidth);
  HW := Self.GetDimension(_HandleWidth, uiDimensionWidth);

  If (Not ShowLabelOnly) Then
  Begin
    Self.DrawComponent(0, 0, 0, UIPixels(BW - HW), _BarHeight, 0, Self.IsSelected);
  End;

  Self.DrawComponent(BW - HW, 0, 0.0, _HandleWidth, _BarHeight, 1, Self.IsSelected);

  If (_Selected<>Nil) And (Assigned(Self.Font)) Then
  Begin
    S := _Selected.ToString();
    If (S<>'') And (S[1]='#') Then
      S := LocalizationManager.Instance.GetString(Copy(S, 2, MaxInt));

    TextRect := Self.FontRenderer.GetTextRect(S);
    Self.DrawText(S, 5, 5, 2.0, TextRect, 1.0, 1, Self.IsSelected, ColorWhite);
  End;

  If (_ShowList) And (UI.Focus<>Self) Then
    _Showlist := False;

  If (_ShowList) Then
  Begin
    ZOfs := 8;


    HH := Self.GetDimension(_BarHeight, uiDimensionHeight);

    _ListWidth := UIPixels(Self.GetDimension(_BarWidth, uiDimensionWidth) + Self.GetDimension(_HandleWidth, uiDimensionWidth));
    _ListHeight := UIPixels(HH * _Content.Count);

    Self.DrawComponent(0.0, HH, ZOfs, _ListWidth, _ListHeight, 2, Self.IsSelected);

    WW := Self.GetDimension(_ListWidth, uiDimensionWidth);

    P := _Content.First;
    For J:=0 To Pred(_Content.Count) Do
    Begin
      YY := HH + J * HH;

      If Assigned(Self.Font) Then
      Begin
        S := P.ToString();
        If (S<>'') And (S[1]='#') Then
          S := LocalizationManager.Instance.GetString(Copy(S, 2, MaxInt));

        TextRect := Self.FontRenderer.GetTextRect(S);

        Self.DrawText(S, (WW - TextRect.X) * 0.5, YY + TextRect.Y * 0.5, ZOfs + 0.5, TextRect, Scale, 2, (J =_ItemHighlight), ColorWhite);
      End;

      P := P.Next;
    End;
  End;

  Inherited;
End;

Procedure UIComboBox.Select(const Value: TERRAString);
Var
  P:CollectionObject;
  I:Integer;
Begin
  I := 0;
  P := _Content.First;
  While Assigned(P) Do
  If (P.ToString() = Value) Then
  Begin
    SetItemIndex(I);
    Exit;
  End Else
  Begin
    Inc(I);
    P := P.Next;
  End;
End;

Procedure UIComboBox.UpdateRects;
Begin
  Inherited;

  If _ShowList Then
    _Size.Y := _Size.Y + Self.GetDimension(_ListHeight, uiDimensionHeight);
End;

End.