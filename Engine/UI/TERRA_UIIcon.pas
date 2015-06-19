Unit TERRA_UIIcon;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font;

Type
  UIIcon = Class(Widget)
    Protected

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;

      Procedure Render; Override;
  End;

Implementation

Constructor UIIcon.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Var
  Base:TERRAString;
Begin
  Inherited Create(Name, Parent, ComponentName);


  Self._Width := Width;
  Self._Height := Height;

  Self._TabIndex := TabIndex;

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  
  Self._Layer := Z;
End;


Procedure UIIcon.Render;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  Self.DrawComponent( 0.0, 0.0, 0.0, _Width, _Height,  0, Self.IsHighlighted());

  Inherited;
End;

Function UIIcon.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  Touched:Boolean;
Begin
  Touched := OnRegion(X,Y);
  If (Touched) And (Self.Visible) And (Assigned(OnMouseClick))  And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.OnHit(Self.OnMouseClick);
    Result := True;
  End Else
    Result := Inherited OnMouseDown(X,Y, Button);
End;


End.