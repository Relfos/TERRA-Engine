Unit TERRA_UICheckbox;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UICaption;

Type
  UICheckBox = Class(UICaption)
    Protected
      _Checked:Boolean;

      Procedure SetChecked(Value:Boolean); Virtual;

      Function HasMouseOver():Boolean; Override;

      Function IsSelectable():Boolean; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Const InitialValue:Boolean; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;
      Procedure UpdateRects; Override;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;

      Procedure SetCheckedWithoutPropagation(Value:Boolean);

      Property Checked:Boolean Read _Checked Write SetChecked;
  End;

Implementation
Uses TERRA_Math;

{ UICheckBox }
Constructor UICheckBox.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Const InitialValue:Boolean; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self._TabIndex := TabIndex;

  Self.SetCaption(Caption);
  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self._Layer := Z;
  Self._Checked := False;

  Self._Width := Size;
  Self._Height := Size;

  Self._Checked := InitialValue; 

  _NeedsUpdate := True;
End;

Procedure UICheckBox.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  SetChecked(Not _Checked);
End;

Function UICheckBox.HasMouseOver: Boolean;
Begin
  Result := True;
End;

Procedure UICheckBox.SetCheckedWithoutPropagation(Value: Boolean);
Begin
  _Checked := Value;
End;

Procedure UICheckBox.SetChecked(Value:Boolean);
Begin
  If (Value = _Checked) Then
    Exit;

  _Checked := Value;

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

  _Size.X := CheckBoxPixelOfs + Self.GetDimension(_Width) + _TextRect.X;
  _Size.Y := FloatMax(_TextRect.Y, Self.GetDimension(_Height));
End;

Procedure UICheckBox.Render;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  Self.DrawComponent(0.0, 0.0, 0.0, _Width, _Height, 0, _Checked);

  Self.DrawText(_Caption, Self.GetDimension(_Width) + CheckBoxPixelOfs, (_Size.Y - _TextRect.Y) * 0.5, 0.5, _TextRect, 1.0, 0, _Checked);

  Inherited;
End;

Function UICheckBox.IsSelectable: Boolean;
Begin
  Result := True;
End;

End.
