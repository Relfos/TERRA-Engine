Unit TERRA_DistanceField;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Image, TERRA_Color;

{ Computes a distance field transform of a high resolution binary source channel
and returns the result as a low resolution channel.

scale_down : The amount the source channel will be scaled down.
A value of 8 means the destination image will be 1/8th the size of the source

spread: The spread in pixels before the distance field clamps to (zero/one). The value
is specified in units of the destination image. The spread in the source image
will be spread*scale_down.
}

Function CreateDistanceField(Source:Image; Component, Scale:Cardinal; Spread:Single):Image;

Implementation

Function SignedDistance(Source:Image; Component, cx, cy:Integer; clamp:Single):Single;
Var
  w, h, dx, dy:Integer;
  x, y1, y2:Integer;
  min_x, max_x:Integer;
  d, d2:Single;
  cd, distance:Single;
  c:Single;
Begin
  w := Source.Width;
  h := Source.Height;

  c := Source.GetComponent(cx, cy, component)/255.0;
  cd := c - 0.5;

  min_x := cx - Trunc(clamp) - 1;
  if (min_x < 0) Then min_x := 0;
  max_x := cx + Trunc(clamp) + 1;
  if (max_x >= w) Then max_x := w-1;

  distance := clamp;
  For dy :=0 To Pred(Trunc(clamp) + 1) Do
  Begin
    If (dy > distance) Then
      Continue;

    If (cy - dy >=0) Then
    Begin
      y1 := cy-dy;
      For x:=min_x To max_x Do
      Begin
        If (x - cx > distance) Then
          Continue;

        c := Source.GetComponent(x, y1, component)/255.0;
        d := c - 0.5;
        If (cd*d<0) Then
        Begin
          d2 := (y1 - cy)*(y1 - cy) + (x-cx)*(x-cx);
          If (d2 < distance*distance) Then
            distance := Sqrt(d2);
        End;
      End;
    End;

    If (dy <> 0) And (cy+dy < h) Then
    Begin
      y2 := cy + dy;
      For x:=min_x To max_x Do
      Begin
        If (x - cx > distance) Then
          Continue;

        c := Source.GetComponent(x, y2, component)/255.0;
        d := c - 0.5;
        If (cd*d<0) Then
        Begin
          d2 := (y2 - cy)*(y2 - cy) + (x-cx)*(x-cx);
          If (d2 < distance*distance) Then
            distance := Sqrt(d2);
        End;
      End;
    End;
  End;

  If (cd > 0) Then
    Result := Distance
  Else
    Result := -distance;
End;

Function CreateDistanceField(Source:Image; Component, Scale:Cardinal; Spread:Single):Image;
Var
  x,y:Integer;
  sd, source_spread:Single;
  n:Single;
  c:Byte;
Begin
  Result := Image.Create(Source.Width Div Scale, Source.Height Div Scale);
  source_spread := spread * Scale;

  For Y:=0 To Pred(Result.Height) Do
    For X:=0 To Pred(Result.Width) Do
    Begin
      sd := SignedDistance(Source, Component, x*Scale + Scale Div 2, y*Scale + Scale Div 2, source_spread);
      n := (sd + source_spread) / (source_spread*2);
      C := Trunc(N*255);
      Result.SetPixel(X,Y, ColorGrey(C, C));
    End;
End;


End.