Unit TERRA_UIButton;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_UIDimension, TERRA_UICaption;

Type
  UIButton = Class(UICaption)
    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;
  End;


Implementation
Uses TERRA_Math;

Constructor UIButton.Create(Name:TERRAString;  Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Caption:TERRAString; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);
  Self.TabIndex := TabIndex;

  Self.Caption.Value := Caption;
  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self.Width := Width;
  Self.Height := Height;

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

  Self.DrawComponent(0, 0, 0, Self.Width, Self.Height, 0, Self.IsSelected);

  If (Assigned(Self.Font)) And (Caption.Value <> '' ) Then
  Begin
    FontScale := 1.0;

    If (_TextRect.X>Self._Size.X) Then
      FontScale := Self._Size.X / (_TextRect.X + 10.0);

    If (_TextRect.Y>Self._Size.Y) Then
      FontScale := FloatMin(FontScale, Self._Size.Y / (_TextRect.Y + 10.0));

    TX := (Self._Size.X - _TextRect.X) * 0.5;
    TY := (Self._Size.Y - _TextRect.Y) * 0.5;

    Self.DrawText(Caption.Value, TX, TY, 0.25, _TextRect, FontScale, 0, Self.IsSelected, ColorWhite);
  End;

  Inherited;
End;


End.