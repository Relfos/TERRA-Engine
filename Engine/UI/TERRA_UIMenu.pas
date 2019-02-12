{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_UIMenu
 * Implements a menu widget
 ***********************************************************************************************************************
}
Unit TERRA_UIMenu;

Interface
Uses TERRA_Utils, TERRA_UI, TERRA_Widgets, TERRA_Classes,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Color;

Const
  MenuAnimationDuration = 250;

Type
  UIMenuItem = Class(Widget)
    Protected
      _Open:Boolean;
      _Caption:TERRAString;

      _Entries:Array Of UIMenuItem;
      _EntryCount:Integer;

      _Width:Single;
      _Height:Single;

      _OpenWidth:Single;
      _OpenHeight:Single;

      _OpenMenu:UIMenuItem;

      _Animated:Boolean;
      _AnimationTime:Cardinal;

    Public
      OnMouseClick:WidgetEventHandler;

      Constructor Create(Caption:TERRAString; Z:Single = 0.3);
      Procedure Release;

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Procedure AddItem(Item:UIMenuItem);

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;

      Property Caption:TERRAString Read _Caption Write _Caption;
  End;

Implementation
Uses TERRA_OS, TERRA_Math;

{ UIMenuItem }
Function UIMenuItem.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  I:Integer;
  Pos:Vector2D;
Begin
  Result := False;

  If (_Parent = Nil) Then
  Begin
    For I:=0 To Pred(_EntryCount) Do
    If _Entries[I].Visible Then
    Begin
      Result := _Entries[I].OnMouseDown(X, Y, Button);
      If Result Then
        Exit;
    End;
    Exit;
  End;

  Pos := Self.Position;
  If (X>=Pos.X) And (X<=Pos.X + _Width) And (Y>=Pos.Y) And (Y<=Pos.Y+_Height) Then
  Begin
    Result := True;
    _Open := True;

    If (_Open) And (Assigned(_Parent)) And (_Parent Is UIMenuItem) Then
    Begin
      If Assigned(UIMenuItem(_Parent)._OpenMenu) Then
        UIMenuItem(_Parent)._OpenMenu._Open := False;

      UIMenuItem(_Parent)._OpenMenu := Self;

      Self._Animated := True;
      Self._AnimationTime := GetTime();
    End;
  End;
End;

Procedure UIMenuItem.AddItem(Item: UIMenuItem);
Begin
  If Not Assigned(Item) Then
    Exit;

  Item.Parent := Self;
  Inc(_EntryCount);
  SetLength(_Entries, _EntryCount);
  _Entries[Pred(_EntryCount)] := Item;

  Self.UpdateRects();
End;

Constructor UIMenuItem.Create(Caption:TERRAString; Z:Single);
Begin
  Self._Visible := True;
  Self._Name := Name;
  Self._Layer := Z;
  Self._Parent := Nil;
  Self.SetPosition(VectorCreate2D(0,0));
  UI.Instance.AddWidget(Self);

  Self.LoadComponent('ui_menu_item');
  Self.LoadComponent('ui_menu_list');

  _Open := False;
  _Caption := Caption;
  _EntryCount := 0;

  If _ComponentCount>=1 Then
  Begin
    _Width := _ComponentList[0].Buffer.Width;
    _Height := _ComponentList[0].Buffer.Height;
  End;

  If _ComponentCount>=2 Then
  Begin
    _OpenWidth := _ComponentList[1].Buffer.Width;
    _OpenHeight := _ComponentList[1].Buffer.Height;
  End;
End;

Procedure UIMenuItem.Release;
Begin
  _EntryCount := 0;

  Inherited;
End;

Procedure UIMenuItem.UpdateRects;
Begin
  _Size.X := _Width;
  _Size.Y := _Height;
End;

Procedure UIMenuItem.Render;
Var
  TextRect:Vector2D;
  OfsX, OfsY, TX, Delta:Single;
  I:Integer;
Begin
  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

  If (_Parent = Nil) Then
  Begin
    _Width := 0;
    _Height := 0;
    For I:=0 To Pred(_EntryCount) Do
    If _Entries[I].Visible Then
    Begin
      _Entries[I].SetPosition(VectorCreate2D(_Width, 0));
      _Entries[I].Render();
      _Width := _Width + _Entries[I].Size.X;
      _Height := FloatMax(_Height, _Entries[I].Size.Y);
    End;

  End Else
  Begin
    TextRect := Self.Font.GetTextRect(_Caption);
    OfsX := (_Width - TextRect.X) * 0.5;
    OfsY := (_Height - TextRect.Y) * 0.5;

    DrawComponent(0, VectorCreate(0.0, 0.0, 0.0), 0.0, 0.0, 1.0, 1.0, ColorWhite);
    DrawText(_Caption, VectorCreate(OfsX, OfsY,  0.5), ColorWhite, 1.0);

    If (_Open) Then
    Begin
      If _Animated Then
      Begin
        Delta := GetTime() - _AnimationTime;
        Delta := Delta / MenuAnimationDuration;
        _Animated := Delta<1.0;
      End Else
        Delta := 1.0;

      For I:=0 To Pred(_EntryCount) Do
      Begin
        TextRect := Self.Font.GetTextRect(_Entries[I]._Caption);
        OfsX := (_OpenWidth - TextRect.X) * 0.5;
        OfsY := (_OpenHeight - TextRect.Y) * 0.5;

        DrawComponent(1, VectorCreate(0.0, _Height + _OpenHeight*I*Delta, 0.0), 0.0, 0.0, 1.0, 1.0, ColorWhite);
        DrawText(_Entries[I].Caption, VectorCreate(OfsX, OfsY + _Height + _OpenHeight*I*Delta,  1.2), ColorWhite, 1.0);
      End;
    End;
  End;

  Inherited;
End;


End.