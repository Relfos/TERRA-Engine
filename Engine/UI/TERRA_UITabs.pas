Unit TERRA_UITabs;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UICaption;

Type
  UITabEntry = Record
    Visible:Boolean;
    Caption:TERRAString;
    Name:TERRAString;
    Index:Integer;
  End;

  UITabList = Class(Widget)
    Protected
      _Tabs:Array Of UITabEntry;
      _TabCount:Integer;
      _TabHighlight:Integer;

      _SelectedIndex:Integer;

      Function GetTabAt(X,Y:Integer):Integer;
      Function GetSelectedCaption: TERRAString;
      Procedure SetSelectedCaption(const Value: TERRAString);

      Procedure SetSelectedIndex(const Value: Integer);

      Function IsSelectable():Boolean; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString); Overload;

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Procedure AddTab(Name:TERRAString; Index:Integer);
      Procedure SetTabVisibility(Index:Integer; Visibility:Boolean);
      Procedure ClearTabs();

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
      Procedure OnMouseMove(X,Y:Integer); Override;

      Function OnSelectRight():Boolean; Override;
      Function OnSelectLeft():Boolean; Override;

			Procedure OnLanguageChange(); Override;

      Property SelectedIndex:Integer Read _SelectedIndex Write SetSelectedIndex;
      Property Caption:TERRAString Read GetSelectedCaption Write SetSelectedCaption;
  End;



Implementation

{ UITabList }
Constructor UITabList.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.SetRelativePosition(VectorCreate2D(X,Y));

  Self.Layer := Z;
  Self.TabIndex := -1;

  Self.Width := Width;
  Self.Height := Height;

  If Assigned(Parent) Then
    Parent.TabControl := Self;

(*  If (Length(_ComponentList)>0) Then
  Begin
    _TabWidthOn := Self._ComponentList[0].Buffer.Width;
    _TabHeightOn := Self._ComponentList[0].Buffer.Height;
  End;

  If (Length(_ComponentList)>1) Then
  Begin
    _TabWidthOff := Self._ComponentList[1].Buffer.Width;
    _TabHeightOff := Self._ComponentList[1].Buffer.Height;
  End;*)

  Self.ClearTabs();
  Self.UpdateRects();
End;


Procedure UITabList.ClearTabs;
Begin
  _TabCount := 0;
  _SelectedIndex := -1;
  _TabHighlight := -1;
End;

Procedure UITabList.AddTab(Name: TERRAString; Index: Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Index = Index) Then
  Begin
    _Tabs[I].Name := Name;
    Exit;
  End;

  Inc(_TabCount);
  SetLength(_Tabs, _TabCount);
  _Tabs[Pred(_TabCount)].Name := Name;
  _Tabs[Pred(_TabCount)].Index := Index;
  _Tabs[Pred(_TabCount)].Visible := True;
  _Tabs[Pred(_TabCount)].Caption := GetLocalizedString(Name);

  If _SelectedIndex<0 Then
    SetSelectedIndex(Pred(_TabCount));
End;

Procedure UITabList.SetTabVisibility(Index: Integer; Visibility: Boolean);
Var
  I,J:Integer;
Begin
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Index = Index) Then
  Begin
    _Tabs[I].Visible := Visibility;
    If (Not Visibility) And (_SelectedIndex = Index) Then
    Begin
      For J:=0 To Pred(_TabCount) Do
      If (_Tabs[J].Visible) Then
      Begin
        _SelectedIndex := J;
        Break;
      End;
    End;

    Exit;
  End;
End;

Procedure UITabList.UpdateRects;
Begin
  _Size.X := Self.GetDimension(Self.Width, uiDimensionWidth) * _TabCount;
  _Size.Y := Self.GetDimension(Self.Height, uiDimensionHeight);
End;

Function UITabList.GetTabAt(X, Y: Integer): Integer;
Var
  I:Integer;
  TX,WW, HH:Single;
Begin
  TX := 0;
  WW := Self.GetDimension(Self.Width, uiDimensionWidth);
  HH := Self.GetDimension(Self.Height, uiDimensionHeight);
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Visible) Then
  Begin
    (*
    If (_Tabs[I].Index = Self._SelectedIndex) Or (_Tabs[I].Index = _TabHighlight) Then
      WW := _TabWidthOn
    Else
      WW := _TabWidthOff;*)

    If (Self.OnCustomRegion(X, Y, TX, -HH, TX+WW, 0)) Then
    Begin
      Result := _Tabs[I].Index;
      Exit;
    End;

    TX := TX + WW;
  End;

  Result := -1;
End;

Procedure UITabList.SetSelectedCaption(const Value: TERRAString);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Index = Self._SelectedIndex) Then
  Begin
    _Tabs[I].Caption := Value;
    Exit;
  End;
End;

Function UITabList.GetSelectedCaption: TERRAString;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Index = Self._SelectedIndex) Then
  Begin
    Result := _Tabs[I].Caption;
    Exit;
  End;

  Result := '';
End;

Procedure UITabList.Render;
Var
  I:Integer;
  MyColor:TERRA_Color.Color;
  Fnt:TERRAFont;
  Rect:Vector2D;
  X, WW, HH, Add:Single;
  IsSel:Boolean;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  If (_TabCount<=0) Then
    Exit;

  X := 0.0;
  For I:=0 To Pred(_TabCount) Do
  If (_Tabs[I].Visible) Then
  Begin
    IsSel := (I=_TabHighlight) Or (I=_SelectedIndex);

    (*If (IsSel) Then
    Begin
      Self.DrawComponent(0, VectorCreate(X, 0, 0), 0.0, 0.0, 1.0, 1.0, MyColor);
      WW := _TabWidthOn;
      HH := _TabHeightOn;
      Add := 0;
    End Else
    Begin
      Self.DrawComponent(1, VectorCreate(X, _TabHeightOn - _TabHeightOff, 0), 0.0, 0.0, 1.0, 1.0, MyColor);
      WW := _TabWidthOff;
      HH := _TabHeightOff;
      Add := _TabHeightOn - _TabHeightOff;
    End;*)

    WW := Self.GetDimension(Self.Width, uiDimensionWidth);
    HH := Self.GetDimension(Self.Height, uiDimensionHeight);

    If (IsSel) Then
      Add := 0
    Else
      Add := HH*0.1;

    Add := -HH + Add;

    Self.DrawComponent( X, Add, -2, Self.Width, Self.Height,  0, IsSel);

    Rect := _FontRenderer.GetTextRect(_Tabs[I].Caption);
    Self.DrawText(_Tabs[I].Caption, X + (WW-Rect.X) * 0.5, Add + (HH - Rect.Y) * 0.5, -1.5, Rect, Scale, 0, IsSel, ColorWhite);

    X := X + WW;
  End;
End;

Procedure UITabList.OnMouseDown(X, Y:Integer; Button:Word);
Var
  N:Integer;
Begin
  N := GetTabAt(X, Y);

  If (N>=0) And (N<>_SelectedIndex) Then
  Begin
    _SelectedIndex := N;
    _TabHighlight := -1;

    Self.OnHit(Self.OnMouseClick);
    Exit;
  End;

  Inherited;
End;

Procedure UITabList.OnMouseMove(X,Y:Integer);
Var
  I:Integer;
Begin
  {Result := False;

  If (Visible) Then
  Begin
    _TabHighlight := GetTabAt(X,Y);
  End;}

  Inherited;
End;

Procedure UITabList.OnLanguageChange;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TabCount) Do
    _Tabs[I].Caption := GetLocalizedString(_Tabs[I].Name);
End;

Function UITabList.OnSelectLeft():Boolean;
Var
  I:Integer;
Begin
  Result := False;

  If (Not Self.Selected) Then
    Exit;

  I := _SelectedIndex;
  While (I=_SelectedIndex) Or (Not _Tabs[I].Visible) Do
  Begin
    If I<=0 Then
      I := Pred(_TabCount)
    Else
      Dec(I);

    If (I = _SelectedIndex) Then
      Exit;
  End;

  SetSelectedIndex(I);
  Result := True;
End;

Function UITabList.OnSelectRight():Boolean;
Var
  I:Integer;
Begin
  Result := False;

  If (Not Self.Selected) Then
    Exit;

  I := _SelectedIndex;
  While (I=_SelectedIndex) Or (Not _Tabs[I].Visible) Do
  Begin
    If I>=Pred(_TabCount) Then
      I := 0
    Else
      Inc(I);

    If (I = _SelectedIndex) Then
      Exit;
  End;

  SetSelectedIndex(I);
  Result := True;
End;

Procedure UITabList.SetSelectedIndex(const Value: Integer);
Begin
  If (Self.IsSelected()) Then
    Self.UpdateHighlight(False);

  _SelectedIndex := Value;
  _TabHighlight := -1;

  If (Self.IsSelected()) Then
    Self.UpdateHighlight(True);

  Self.OnHit(Self.NullEventHandler);
End;

Function UITabList.IsSelectable: Boolean;
Begin
  Result := True;
End;

End.