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

Function CreateDistanceField(Source:Image; Component, Scale:Cardinal; Spread:Single):Image;

Implementation
Uses TERRA_Log;

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

  If (CX<0) Or (CY<0) Or (CX>=Source.Width) Or (CY>=Source.Height) Then
    C := 0
  Else
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
    Result := -Distance
  Else
    Result := distance;
End;

Function CreateDistanceField(Source:Image; Component, Scale:Cardinal; Spread:Single):Image;
Var
  x,y:Integer;
  Padding:Integer;
  PX, PY:Integer;
  sd:Single;
  n:Single;
  c:Byte;

  Temp:Image;
Begin
  //Padding := Source.Width Div 10;
  Padding := 0;

  Log(logWarning, 'Application', 'Making distance field glyph...');

  If Padding> 0 Then
  Begin
    Temp := Image.Create(Source);
    Temp.Resize(Source.Width - Padding * 2, Source.Height - Padding * 2);

    Source := Image.Create(Source.Width, Source.Height);
    Source.Blit(Padding, Padding, 0, 0, Temp.Width, Temp.Height, Temp);
  End Else
    Temp := Nil;

  Result := Image.Create(Source.Width, Source.Height);

  For Y:=0 To Pred(Result.Height) Do
    For X:=0 To Pred(Result.Width) Do
    Begin
      PX := X;
      PY := Y;

      sd := SignedDistance(Source, Component, PX, PY, Spread);
      n := (sd + Spread) / (Spread*2);

      C := Trunc((1.0-N)*255);
      Result.SetPixel(X, Y, ColorGrey(C, C));
    End;

  ReleaseObject(Temp);

  If Padding> 0 Then
    ReleaseObject(Source);

(*  While (Scale>1) Do
  Begin
    Temp := Result;
    Result := Temp.MipMap();

    ReleaseObject(Temp);

    Scale := Scale Shr 1;
  End;*)
End;


End.