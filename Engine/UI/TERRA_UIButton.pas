Unit TERRA_UIButton;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_UICaption;

Type
  UIButton = Class(UICaption)
    Public

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
  End;


Implementation
Uses TERRA_Math;

Constructor UIButton.Create(Name:TERRAString;  Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);
  Self._TabIndex := TabIndex;

  Self.SetCaption(Caption);
  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self._Layer := Z;

  Self._Width := Width;
  Self._Height := Height;

  _NeedsUpdate := True;
End;

Procedure UIButton.Render;
Var
  TX, TY:Single;
  FontScale:Single;
Begin
  Self.ClearProperties();

  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

  Self.DrawComponent(0, 0, 0, _Width, _Height, 0, Self.IsSelected);

  If (Assigned(Self.Font)) And (Caption<>'') Then
  Begin
    FontScale := 1.0;

    If (_TextRect.X>Self._Size.X) Then
      FontScale := Self._Size.X / (_TextRect.X + 10.0);

    If (_TextRect.Y>Self._Size.Y) Then
      FontScale := FloatMin(FontScale, Self._Size.Y / (_TextRect.Y + 10.0));

    TX := (Self._Size.X - _TextRect.X) * 0.5;
    TY := (Self._Size.Y - _TextRect.Y) * 0.5;

    Self.DrawText(_Caption, TX, TY, 0.25, _TextRect, FontScale, 0, Self.IsSelected);
  End;

  Inherited;
End;

Function UIButton.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Begin
  If (OnRegion(X,Y)) And (Self.Visible) And (Not Self.HasPropertyTweens()) Then
  Begin
    Result := True;
    Self.OnHit(OnMouseClick);
  End Else
    Result := Inherited OnMouseDown(X,Y, Button);
End;


End.