{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_DistanceFields
 * Implements distance fields for font rendering
 ***********************************************************************************************************************
}
Unit TERRA_DistanceField;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_Image, TERRA_Color;

{ Computes a distance field transform of a high resolution binary source channel
and returns the result as a low resolution channel.

scale_down : The amount the source channel will be scaled down.
A value of 8 means the destination image will be 1/8th the size of the source

spread: The spread in pixels before the distance field clamps to (zero/one). The value
is specified in units of the destination image. The spread in the source image
will be spread*scale_down.
}

Function CreateDistanceField(Source:TERRAImage; Component, Scale:Cardinal; Spread:Single):TERRAImage;

Implementation
Uses TERRA_Log, TERRA_ImageDrawing, TERRA_Engine;


Function SignedDistance(Source:PSingleArray; W, H, cx, cy:Integer; clamp:Single):Single;
Var
  dx, dy:Integer;
  x, y1, y2:Integer;
  min_x, max_x:Integer;
  d, d2:Single;
  cd, distance:Single;
Begin
  cd := Source[CX + CY*W];

  min_x := cx - Trunc(clamp) - 1;
  If (min_x < 0) Then
    min_x := 0;

  max_x := cx + Trunc(clamp) + 1;
  If (max_x >= w) Then
    max_x := w-1;

  Distance := clamp;
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

        d := Source[X + Y1 *W];
        If (cd*d<0) Then
        Begin
          d2 := (y1 - cy)*(y1 - cy) + (x-cx)*(x-cx);
          If (d2 < Sqr(distance)) Then
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

        d := Source[X + Y2 *W];
        If (cd*d<0) Then
        Begin
          d2 := (y2 - cy)*(y2 - cy) + (x-cx)*(x-cx);
          If (d2 < Sqr(Distance)) Then
            distance := Sqrt(d2);
        End;
      End;
    End;

    (*If (Distance<=0) Then
      Break;*)

  End;

  If (cd > 0) Then
    Result := Distance
  Else
    Result := -Distance;
End;

Const
  RescaleFactor = 1.0 / 255.0;

Function CreateDistanceField(Source:TERRAImage; Component, Scale:Cardinal; Spread:Single):TERRAImage;
Var
  X,Y:Integer;
  PX, PY:Integer;
  sd:Single;
  n:Single;
  c:Byte;

  Values:Array Of Single;
  It:ImageIterator;
Begin
  //REsult := TERRAImage.create(Source);  Exit;

  //Padding := Source.Width Div 10;
  //Engine.Log.Write(logWarning, 'Application', 'Making distance field glyph...');

  Result := TERRAImage.Create(Source.Width, Source.Height);

  //Result.Save('test.png');

  SetLength(Values, Source.Width * Source.Height);
  It := Source.Pixels([image_Read]);
  Case Component Of
  componentRed:
    While It.HasNext() Do
    Begin
      Values[It.X + It.Y * Source.Width] := (It.SourceColor.R * RescaleFactor) - 0.5;
    End;

  componentGreen:
    While It.HasNext() Do
    Begin
      Values[It.X + It.Y * Source.Width] := (It.SourceColor.G * RescaleFactor) - 0.5;
    End;

  componentBlue:
    While It.HasNext() Do
    Begin
      Values[It.X + It.Y * Source.Width] := (It.SourceColor.B * RescaleFactor) - 0.5;
    End;

  componentAlpha:
    While It.HasNext() Do
    Begin
      Values[It.X + It.Y * Source.Width] := (It.SourceColor.A * RescaleFactor) - 0.5;
    End;
  End;

  For Y:=0 To Pred(Result.Height) Do
    For X:=0 To Pred(Result.Width) Do
    Begin
      PX := X;
      PY := Y;

      sd := SignedDistance(@Values[0], Source.Width, Source.Height, PX, PY, Spread);
      n := (sd + Spread) / (Spread*2);

      C := Trunc(N*255);
      Result.SetPixel(X, Y, ColorGrey(C, C));
    End;
End;


End.