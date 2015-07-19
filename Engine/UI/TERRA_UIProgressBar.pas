Unit TERRA_UIProgressBar;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UIDimension, TERRA_Tween;

Type
  UIProgressBar = Class(Widget)
    Protected
      _Percent:FloatProperty;
      _ProgressIndex:Integer;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;
      Procedure Release; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Percent:FloatProperty Read _Percent;
  End;


Implementation

{ UIProgressBar }
Constructor UIProgressBar.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.TabIndex := TabIndex;

  Self._Percent := FloatProperty.Create('percent', 0);

  Self.Width := Width;
  Self.Height := Height;

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self.ExpandProperties(1);
  _ProgressIndex := _BasePropertiesIndex;
End;

Function UIProgressBar.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _ProgressIndex Then
    Result := Self.Percent
  Else
    Result := Inherited GetPropertyByIndex(Index);
End;

Procedure UIProgressBar.Release;
Begin
  Inherited;

  ReleaseObject(_Percent);
End;

Procedure UIProgressBar.Render;
Var
  P, K:Single;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  P := _Percent.Value;
  If (P<0) Then
    P := 0
  Else
  If (P >100) Then
    P := 100;

  K := P / 100;

  If K<1.0 Then
    Self.DrawComponent(0, 0.0, 0,  Self.Width, Self.Height, 0, False);

  If K>0.0 Then
    Self.DrawCroppedComponent(0, 0.0, 0.1, 0.0, 0.0, K, 1.0, UIPixels(Self.GetDimension(Self.Width, uiDimensionWidth) * K), Self.Height, 0, True);

  Inherited;
End;

End.