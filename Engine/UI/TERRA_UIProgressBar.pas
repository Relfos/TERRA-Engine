Unit TERRA_UIProgressBar;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_Tween;

Type
  UIProgressBar = Class(Widget)
    Protected
      _Percent:Single;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;


      Property Percent:Single Read _Percent Write _Percent;
  End;


Implementation

{ UIProgressBar }
Constructor UIProgressBar.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self._TabIndex := TabIndex;
  Self._Percent := 0;

  Self._Width := Width;
  Self._Height := Height;

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self._Layer := Z;
End;

Procedure UIProgressBar.Render;
Var
  K:Single;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  If (_Percent<0) Then
    _Percent := 0;
  If (_Percent>100) Then
    _Percent := 100;

  K := _Percent/100;

  If K<1.0 Then
    Self.DrawComponent(0, 0.0, 0,  _Width, _Height, 0, False);

  If K>0.0 Then
    Self.DrawCroppedComponent(0, 0.0, 0.1, 0.0, 0.0, K, 1.0, UIPixels(Self.GetDimension(_Width) * K), _Height, 0, True);

  Inherited;
End;


End.