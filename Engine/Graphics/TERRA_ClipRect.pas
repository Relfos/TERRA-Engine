Unit TERRA_ClipRect;

{$I terra.inc}

Interface
Uses TERRA_Vector2D, TERRA_Vector3D, TERRA_Matrix3x3;

Type
  ClipRectStyle = (
    clipNothing = 0,
    clipSomething = 1,
    clipEverything = 2
  );

  TERRAClipRect = Object
    Protected
      _Style:ClipRectStyle;
      _X1, _Y1:Single;
      _X2, _Y2:Single;

    Public
      Procedure SetArea(Const PX1, PY1, PX2, PY2:Single);

      Procedure Transform(Const M:Matrix3x3);

      Procedure Merge(Const Other:TERRAClipRect);

      Property X1:Single Read _X1;
      Property Y1:Single Read _Y1;

      Property X2:Single Read _X2;
      Property Y2:Single Read _Y2;

      Property Style:ClipRectStyle Read _Style Write _Style;
  End;

//Function ClipRectCreate(X,Y, Width, Height:Single):TERRAClipRect;


Implementation

{ TERRAClipRect }
Procedure TERRAClipRect.SetArea(Const PX1, PY1, PX2, PY2:Single);
Begin
  _X1 := PX1;
  _Y1 := PY1;
  _X2 := PX2;
  _Y2 := PY2;
End;

Procedure TERRAClipRect.Transform(const M: Matrix3x3);
Var
  I:Integer;
  P:Array[0..3] Of Vector2D;
  T:Vector2D;
  MinX, MinY, MaxX, MaxY:Single;
Begin
  P[0].X := _X1;
  P[0].Y := _Y1;

  P[1].X := _X2;
  P[1].Y := _Y1;

  P[2].X := _X2;
  P[2].Y := _Y2;

  P[3].X := _X1;
  P[3].Y := _Y2;

  MaxX := -9999;
  MaxY := -9999;

  MinX := 9999;
  MinY := 9999;

  For I:=0 To 3 Do
  Begin
    T := M.Transform(P[I]);

    If (T.X>MaxX) Then
      MaxX := T.X;

    If (T.Y>MaxY) Then
      MaxY := T.Y;

    If (T.X<MinX) Then
      MinX := T.X;

    If (T.Y<MinY) Then
      MinY := T.Y;
  End;

  Self._X1 := MinX;
  Self._Y1 := MinY;

  Self._X2 := MaxX;
  Self._Y2 := MaxY;
End;

Procedure TERRAClipRect.Merge(const Other: TERRAClipRect);
Var
  PX1, PY1, PX2, PY2: Single;
Begin
Exit;
  If (_Style = clipEverything) Or (Other._Style = clipNothing) Then
  Begin
    Exit;
  End;

  If (_Style = clipNothing) Then
  Begin
    _X1 := Other._X1;
    _Y1 := Other._Y1;
    _X2 := Other._X2;
    _Y2 := Other._Y2;
    _Style := Other._Style;
    Exit;
  End;

  If (PX1>_X1) Then
    _X1 := PX1;

  If (PY1>_Y1) Then
    _Y1 := PY1;

  If (PX2<_X2) Then
    _X2 := PX2;

  If (PY2<_Y2) Then
    _Y2 := PY2;
End;

End.