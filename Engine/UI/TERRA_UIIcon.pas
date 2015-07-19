Unit TERRA_UIIcon;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font;

Type
  UIIcon = Class(Widget)
    Protected

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;

      Procedure Render; Override;
  End;

Implementation

Constructor UIIcon.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Var
  Base:TERRAString;
Begin
  Inherited Create(Name, Parent, ComponentName);


  Self.Width := Width;
  Self.Height := Height;

  Self.TabIndex := TabIndex;

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  
  Self.Layer := Z;
End;


Procedure UIIcon.Render;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  Self.DrawComponent( 0.0, 0.0, 0.0, Self.Width, Self.Height,  0, Self.IsHighlighted());

  Inherited;
End;

Procedure UIIcon.OnMouseDown(X, Y: Integer; Button: Word);
Var
  Touched:Boolean;
Begin
  Touched := OnRegion(X,Y);
  If (Touched) And (Self.Visible) And (Assigned(OnMouseClick))  And (Not Self.HasPropertyTweens()) Then
  Begin
    Self.OnHit(Self.OnMouseClick);
  End;

  Inherited;
End;


End.