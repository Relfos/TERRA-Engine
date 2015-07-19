Unit TERRA_UICheckbox;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UIDimension, TERRA_UICaption;

Type
  UICheckBox = Class(UICaption)
    Protected
      _Checked:BooleanProperty;
      _CheckIndex:Integer;

      Procedure SetChecked(Value:Boolean); Virtual;

      Function HasMouseOver():Boolean; Override;

      Function IsSelectable():Boolean; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Const InitialValue:Boolean; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Release; Override;

      Function GetPropertyByIndex(Index: Integer): TERRAObject; Override;

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;

      Property Checked:BooleanProperty Read _Checked;
  End;

Implementation
Uses TERRA_Math;

{ UICheckBox }
Constructor UICheckBox.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Const InitialValue:Boolean; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.TabIndex := TabIndex;

  Self.Caption.Value := Caption;
  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self._Checked := BooleanProperty.Create('checked', InitialValue);

  Self.Width := Size;
  Self.Height := Size;

  Self.ExpandProperties(1);
  _CheckIndex := _BasePropertiesIndex;

  _NeedsUpdate := True;
End;

Procedure UICheckBox.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  SetChecked(Not _Checked.Value);
End;

Function UICheckBox.HasMouseOver: Boolean;
Begin
  Result := True;
End;

Procedure UICheckBox.SetChecked(Value:Boolean);
Begin
  If (Value = _Checked.Value) Then
    Exit;

  _Checked.Value := Value;

  If (Self.Visible) And (Assigned(OnMouseClick)) Then
  Begin
    Self.OnHit(OnMouseClick);
  End;
End;

Const
  CheckBoxPixelOfs = 5;

Procedure UICheckBox.UpdateRects();
Begin
  Inherited;

  _Size.X := CheckBoxPixelOfs + Self.GetDimension(Self.Width, uiDimensionWidth) + _TextRect.X;
  _Size.Y := FloatMax(_TextRect.Y, Self.GetDimension(Self.Height, uiDimensionHeight));
End;

Procedure UICheckBox.Render;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  Self.DrawComponent(0.0, 0.0, 0.0, Self.Width, Self.Height, 0, _Checked.Value);

  Self.DrawText(Caption.Value, Self.GetDimension(Self.Width, uiDimensionWidth) + CheckBoxPixelOfs, (_Size.Y - _TextRect.Y) * 0.5, 0.5, _TextRect, 1.0, 0, _Checked.Value, ColorWhite);

  Inherited;
End;

Function UICheckBox.IsSelectable: Boolean;
Begin
  Result := True;
End;

Procedure UICheckBox.Release;
Begin
  Inherited;

  ReleaseObject(_Checked);
End;

Function UICheckBox.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _CheckIndex Then
    Result := Self._Checked
  Else
    Result := Inherited GetPropertyByIndex(Index);
End;

End.
