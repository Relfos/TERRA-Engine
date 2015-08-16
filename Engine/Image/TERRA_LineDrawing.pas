Unit TERRA_LineDrawing;

{$I terra.inc}

// http://freespace.virgin.net/hugo.elias/graphics/x_wuline.htm
Interface
Uses TERRA_Object, TERRA_Color, TERRA_Image;

Procedure WuLine(Target:Image; x1, y1, x2, y2:Integer);


Implementation
Uses Math;


  procedure swap(var a, b: longint);
  var
    tmp: longint;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;


Procedure WuLine(Target:Image; x1, y1, x2, y2:Integer);
var
  a, a_new:Byte;
  gradient, iy: Single;
  x, y: longint;
  px, py: plongint;
begin
  a := 255;

  If abs(y2 - y1) > abs(x2 - x1) Then
  Begin
    swap(x1, y1);
    swap(x2, y2);
    px := @y;
    py := @x;
  End Else
  Begin
    px := @x;
    py := @y;
  End;

  If x1 > x2 Then
  Begin
    swap(x1, x2);
    swap(y1, y2);
  End;

  x := x2 - x1;
  If x = 0 Then
    x := 1;

  gradient := (y2 - y1) / x;
  iy := y1;

  For x := x1 to x2 Do
  Begin
    a_new := round(a * frac(iy));
    y := floor(iy);

    Target.SetPixel(px^, py^, ColorGrey(255, a-a_new));
    inc(y);

    Target.SetPixel(px^, py^, ColorGrey(255, a_new));
    iy := iy + gradient;
  End;
End;


End.
