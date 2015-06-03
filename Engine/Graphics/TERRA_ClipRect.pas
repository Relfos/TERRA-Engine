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

  ClipRect = Object
    Protected
      _Style:ClipRectStyle;
      _X, _Y:Single;
      _Width, _Height:Single;

    Public
      Procedure SetHeight(const Value: Single);
      Procedure SetWidth(const Value: Single);
      Procedure SetX(const Value: Single);
      Procedure SetY(const Value: Single);
      Procedure SetStyle(Const ClipStyle:ClipRectStyle);

      Procedure GetRealRect(Out X1, Y1, X2, Y2:Single{; Landscape:Boolean});

      Procedure Transform(Const M:Matrix3x3);

      Procedure Merge(Const Other:ClipRect);

      Property X:Single Read _X Write SetX;
      Property Y:Single Read _Y Write SetY;

      Property Style:ClipRectStyle Read _Style Write SetStyle;

      Property Width:Single Read _Width Write SetWidth;
      Property Height:Single Read _Height Write SetHeight;
  End;

Function ClipRectCreate(X,Y, Width, Height:Single):ClipRect;


Implementation

{ ClipRect }
Procedure ClipRect.GetRealRect(Out X1, Y1, X2, Y2: Single{; Landscape:Boolean});
Var
  UIWidth, UIHeight:Integer;
Begin
{  If (Landscape) Then
  Begin
    UIWidth := GraphicsManager.Instance.UIViewport.Width;
    UIHeight := GraphicsManager.Instance.UIViewport.Height;
    X2 := UIWidth - (Self.Y);
    X1 := UIWidth - (X2 + Self.Height);
    Y2 := UIHeight - (Self.X);
    Y1 := UIHeight - (Y2 + Self.Width);
  End Else}
  Begin
    X1 := Self.X;
    X2 := X1 + Self.Width;
    Y1 := Self.Y;
    Y2 := Y1 + Self.Height;
  End;
End;

Procedure ClipRect.SetStyle(Const ClipStyle:ClipRectStyle);
Begin
  _Style := ClipStyle;
End;


Procedure ClipRect.SetHeight(const Value: Single);
Begin
  _Height := Value;
  _Style := clipSomething;
End;

Procedure ClipRect.SetWidth(const Value: Single);
Begin
  _Width := Value;
  _Style := clipSomething;
End;

Procedure ClipRect.SetX(const Value: Single);
Begin
  _X := Value;
  _Style := clipSomething;
End;

Procedure ClipRect.SetY(const Value: Single);
Begin
  _Y := Value;
  _Style := clipSomething;
End;

Procedure ClipRect.Transform(const M: Matrix3x3);
Var
  I:Integer;
  P:Array[0..3] Of Vector2D;
  T:Vector2D;
  MinX, MinY, MaxX, MaxY:Single;
Begin
  P[0].X := _X;
  P[0].Y := _Y;

  P[1].X := _X + _Width;
  P[1].Y := _Y;

  P[2].X := _X + _Width;
  P[2].Y := _Y + _Height;

  P[3].X := _X;
  P[3].Y := _Y + _Height;

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

  Self.X := MinX;
  Self.Y := MinY;

  Self.Width := MaxX - MinX;
  Self.Height := MaxY - MinY;
End;

Procedure ClipRect.Merge(const Other: ClipRect);
Var
  Diff:Single;
  X1, Y1, X2, Y2: Single;
  PX1, PY1, PX2, PY2: Single;
Begin
  If (_Style = clipEverything) Or (Other._Style = clipNothing) Then
  Begin
    Exit;
  End;

  If (_Style = clipNothing) Then
  Begin
    _X := Other._X;
    _Y := Other._Y;
    _Width := Other._Width;
    _Height := Other._Height;
    _Style := Other._Style;
    Exit;
  End;

  Self.GetRealRect(X1, Y1, X2, Y2);
  Other.GetRealRect(PX1, PY1, PX2, PY2);

  If (PX1>X1) Then
    X1 := PX1;

  If (PY1>Y1) Then
    Y1 := PY1;

  If (PX2<X2) Then
    X2 := PX2;

  If (PY2>Y2) Then
    Y2 := PY2;


  _X := X1;
  _Y := Y1;
  _Width := X2-X1;
  _Height := Y2-Y1;
End;

Function ClipRectCreate(X,Y, Width, Height:Single):ClipRect;
Begin
  Result._Style := clipSomething;
  Result._X := X;
  Result._Y := Y;
  Result._Width := Width;
  Result._Height := Height;
End;

End.