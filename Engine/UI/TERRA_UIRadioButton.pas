Unit TERRA_UIRadioButton;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UIDimension, TERRA_UICheckbox;

Type
  UIRadioButton = Class(UICheckBox)
    Protected
      _IsFirstRadio:Boolean;

      Procedure SetChecked(Value:Boolean); Override;

    Public
      Group:Integer;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;

      Procedure Render; Override;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);
  End;

Implementation

Constructor UIRadioButton.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Size:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);
Var
  I, Count:Integer;
  W:Widget;
  RB:UIRadioButton;
Begin
  Inherited Create(Name, Parent, X, Y, Z, Size, False, Caption, ComponentName, TabIndex);

  Count := 0;
  For I:=0 To Pred(Parent.ChildrenCount) Do
  Begin
    W := Widget(Parent.GetChildByIndex(I));
    If W = Self Then
      Continue;

    If (W Is UIRadioButton) Then
    Begin
      RB := UIRadioButton(W);
      If (RB.Group = Self.Group) Then
      Begin
        Inc(Count);
        Break;
      End;
    End;
  End;

  _IsFirstRadio := (Count<=0);

  _NeedsUpdate := True;
End;

Procedure UIRadioButton.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  If (Not _Checked.Value) Then
    SetChecked(True);
End;

procedure UIRadioButton.Render;
begin
  Inherited;

  If (_IsFirstRadio) Then
  Begin
    _IsFirstRadio := False;
    Self.SetChecked(True);
  End;

end;

Procedure UIRadioButton.SetChecked(Value:Boolean);
Var
  I:Integer;
  W:Widget;
  RB:UIRadioButton;
Begin
  If (_Checked.Value = Value) Or (Not Value) Then
    Exit;

  _Checked.Value := Value;

  If (Not Value) Then
    Exit;

  If Assigned(OnMouseClick) Then
    OnMouseClick(Self);

  If Parent = Nil Then
    Exit;

  For I:=0 To Pred(Parent.ChildrenCount) Do
  Begin
    W := Widget(Parent.GetChildByIndex(I));
    If W = Self Then
      Continue;

    If (W Is UIRadioButton) Then
    Begin
      RB := UIRadioButton(W);
      If (RB.Group = Self.Group) Then
        RB._Checked.Value := False;
    End;
  End;
End;


End.