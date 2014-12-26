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
 * TERRA_Color
 * Implements Color operations in RGB and HSL space
 ***********************************************************************************************************************
}
Unit TERRA_Color;
{$I terra.inc}

Interface
Uses TERRA_Utils;

{$R-}

Type
  PColor = ^Color;
  Color=Packed Object
  {$IFDEF BGR}
    B:Byte;
    G:Byte;
    R:Byte;
    A:Byte;
  {$ELSE}
    R:Byte;
    G:Byte;
    B:Byte;
    A:Byte;
  {$ENDIF}
  End;

  PColorArray = ^ColorArray;
  ColorArray=Array[0..0] Of Color;

  PColorPalette = ^ColorPalette;
  ColorPalette = Array[0..255] Of Color;

Const
// Color constants
  {$IFDEF BGR}
  ColorWhite:Color=(B:255; G:255; R:255; A:255);
  ColorBlack:Color=(B:0; G:0; R:0; A:255);
  ColorNull:Color=(B:0; G:0; R:0; A:0);
  {$ELSE}
  ColorWhite:Color=(R:255; G:255; B:255; A:255);
  ColorBlack:Color=(R:0; G:0; B:0; A:255);
  ColorNull:Color=(R:0; G:0; B:0; A:0);
  {$ENDIF}

  {$IFDEF BGR}
  ColorRed:Color=(B:0; G:0; R:255; A:255);
  ColorBlue:Color=(B:255; G:0; R:0; A:255);
  ColorGreen:Color=(B:0; G:255; R:0; A:255);
  ColorYellow:Color=(B:0; G:255; R:255; A:255);
  {$ELSE}
  ColorRed:Color=(R:255; G:0; B:0; A:255);
  ColorBlue:Color=(R:0; G:0; B:255; A:255);
  ColorGreen:Color=(R:0; G:255; B:0; A:255);
  ColorYellow:Color=(R:255; G:255; B:0; A:255);
  {$ENDIF}

//#####################
//#  Color functions  #
//#####################

Function ColorToString(Const N:Color):AnsiString;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):Color;Overload;
Function ColorCreate(Const R,G,B:Single;A:Single=1.0):Color;Overload;
Function ColorCreate(HexValue:AnsiString):Color;Overload;

// Mixes colors
Function ColorMix(Const A,B:Color; Const Cur:Single):Color;Overload;
Function ColorMix(Const A,B,C:Color; Const U,V:Single):Color;Overload;

// blends A with B, using A alpha
Function ColorBlend(Const Src,Dest:Color):Color; Overload;
Function ColorBlend(Const Src,Dest:Color; A:Cardinal):Color; Overload;

Function ColorAdd(Const A,B:Color; Const Scale:Single=1.0):Color;
Function ColorSubtract(Const A,B:Color; Const Scale:Single=1.0):Color;

// Scale color
Function ColorScale(Const A,B:Color):Color;Overload;
Function ColorScale(Const A:Color; B:Single):Color;Overload;

// Color lerp
Function ColorLerp(Const A,B,C:Color):Color;

// Swap color channels
Function ColorSwap(Color:Color):Color;

Function ColorNegative(Color:Color):Color;

// Color conversion routines
Function ColorRGB15Decode(Source:Word):Color;
Function ColorRGB15Encode(Source:Color):Word;

Function ColorRGB8Decode(Source:Byte):Color;
Function ColorRGB8Encode(Source:Color):Byte;

Function ColorLuminance(Source:Color):Byte;
Function ColorHue(Source:Color):Byte;
Function ColorSaturation(Source:Color):Byte;  

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;

Function ColorHSLToRGB(Input:Color):Color;
Function ColorRGBToHSL(Input:Color):Color;

  // Color conversion
  Function ColorRGB16To32(Source:Word):Color;
  Function ColorRGB32To16(Source:Color):Word;
  Function ColorRGB32To8(Source:Color):Byte;
  Function ColorRGB8To32(Source:Byte):Color;

  Function ColorBGR16To32(Source:Word):Color;
  Function ColorBGR32To16(Source:Color):Word;
  Function ColorBGR32To8(Source:Color):Byte;
  Function ColorBGR8To32(Source:Byte):Color;

Implementation

// Color functions
Function ColorToString(Const N:Color):AnsiString;
Begin
  Result := IntToString(N.R)+'\'+
            IntToString(N.G)+'\'+
            IntToString(N.B)+'\'+
            IntToString(N.A);
End;

Function ColorCreate(HexValue:AnsiString):Color;
  Function H(C:AnsiChar):Byte;
  Begin
    C := UpCase(C);
    If (C>='0') And (C<='9') Then
      Result := Ord(C)-Ord('0')
    Else
      Result := Ord(C)-Ord('A')+10;
  End;
Begin
  If (Length(HexValue)<6) Then
  Begin
  	Result := ColorNull;
    Exit;
  End;

  If (HexValue[1]='#') Then
    HexValue := Copy(HexValue, 2, MaxInt);

  Result.R := H(HexValue[1])*16+H(HexValue[2]);
  Result.G := H(HexValue[3])*16+H(HexValue[4]);
  Result.B := H(HexValue[5])*16+H(HexValue[6]);
  Result.A := 255;
End;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
End;

Function ColorCreate(Const R,G,B:Single;A:Single=1.0):Color; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := Byte(Trunc(R*255));
  Result.G := Byte(Trunc(G*255));
  Result.B := Byte(Trunc(B*255));
  Result.A := Byte(Trunc(A*255));
End;

Function ColorAdd(Const A,B:Color; Const Scale:Single=1.0):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := IntMin(Integer(Trunc(A.R+B.R*Scale)),255);
  Result.G := IntMin(Integer(Trunc(A.G+B.G*Scale)),255);
  Result.B := IntMin(Integer(Trunc(A.B+B.B*Scale)),255);
  Result.A := IntMin(Integer(Trunc(A.A+B.A*Scale)),255);
End;

Function ColorSubtract(Const A,B:Color; Const Scale:Single=1.0):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=IntMax(Integer(Trunc(A.R-B.R*Scale)),0);
  Result.G:=IntMax(Integer(Trunc(A.G-B.G*Scale)),0);
  Result.B:=IntMax(Integer(Trunc(A.B-B.B*Scale)),0);
  Result.A:=IntMax(Integer(Trunc(A.A-B.A*Scale)),0);
End;

{$OverFlowChecks Off}

Function ColorBlend(Const Src,Dest:Color; A:Cardinal):Color;{$IFDEF FPC} Inline;{$ENDIF}
(*Var
	A,B:Single;
Begin
	A := Src.A / 255;
	B := 1 - A;
	Result.R := Trunc(Src.R * A + Dest.R * B);
	Result.G := Trunc(Src.G * A + Dest.G * B);
	Result.B := Trunc(Src.B * A + Dest.B * B);
	Result.A := Dest.A;
	*)
// http://stereopsis.com/doubleblend.html
Var
	srcrb, srcag:Cardinal;
	dstrb, dstag:Cardinal;
	drb, dag:Cardinal;
	rb, ag:Cardinal;
Begin
	dstrb := Cardinal(Dest) And $FF00FF;
	dstag := (Cardinal(Dest) Shr 8) And $FF00FF;

	srcrb := Cardinal(Src) And $FF00FF;
	srcag := (Cardinal(Src) Shr 8) And $FF00FF;

	drb := srcrb - dstrb;
	dag := srcag - dstag;

	drb := drb * a;  
	dag := dag * a;  
	drb := drb Shr 8;
	dag := dag Shr 8;

	rb  := (drb + dstrb) And $00FF00FF;
	ag  := ((dag + dstag) Shl 8) And $FF00FF00;

	Result := Color(Cardinal(rb Or ag));
End;

Function ColorBlend(Const Src,Dest:Color):Color;{$IFDEF FPC} Inline;{$ENDIF}
Var
	a:Cardinal;
	srcrb, srcg:Cardinal;
	dstrb, dstg:Cardinal;
	drb, dg:Cardinal;
	rb, g:Cardinal;
Begin
	a := (Cardinal(Src) Shr 24) + 1;

	dstrb := Cardinal(Dest) And $FF00FF;
	dstg := Cardinal(Dest) And $FF00;

	srcrb := Cardinal(Src) And $FF00FF;
	srcg := Cardinal(Src) And $FF00;

	drb := Cardinal(srcrb - dstrb);
	dg := Cardinal(srcg - dstg);

	drb := drb * a;
	dg := (dg * a);
	drb := drb Shr 8;
	dg := dg Shr 8;

	rb  := (drb + dstrb) And $00FF00FF;
	g  := (dg + dstg)  And $FF00;

  A := Cardinal(Dest) And $FF000000;

	Result := Color(Cardinal(rb Or g Or A));

    //Result.A := IntMax(Result.A, Src.A);
End;

Function ColorMix(Const A,B:Color; Const Cur:Single):Color; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc(A.R*Cur+B.R*(1-Cur));
  Result.G:=Trunc(A.G*Cur+B.G*(1-Cur));
  Result.B:=Trunc(A.B*Cur+B.B*(1-Cur));
  Result.A:=Trunc(A.A*Cur+B.A*(1-Cur));
End;

Function ColorMix(Const A,B,C:Color; Const U,V:Single):Color; {$IFDEF FPC} Inline;{$ENDIF}
Var
  W:Single;
Begin
  W:=1-(U+V);
  Result.R:=Byte(Trunc(A.R*U+B.R*V+C.R*W));
  Result.G:=Byte(Trunc(A.G*U+B.G*V+C.G*W));
  Result.B:=Byte(Trunc(A.B*U+B.B*V+C.B*W));
  Result.A:=255;
End;

Function ColorScale(Const A,B:Color):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc((A.R/255)*(B.R/255)*255);
  Result.G:=Trunc((A.G/255)*(B.G/255)*255);
  Result.B:=Trunc((A.B/255)*(B.B/255)*255);
  Result.A:=Trunc((A.A/255)*(B.A/255)*255);
End;

Function ColorScale(Const A:Color; B:Single):Color;Overload;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  X,Y,Z:Single;
Begin
  X := A.R*B;
  Y := A.G*B;
  Z := A.B*B;
  If (X>255) Then X := 255;
  If (Y>255) Then Y := 255;
  If (Z>255) Then Z := 255;
  Result.R := Trunc(X);
  Result.G := Trunc(Y);
  Result.B := Trunc(Z);
  Result.A := A.A;
End;

Function ColorLerp(Const A,B,C:Color):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc((A.R/255)*(B.R/255)+((255-A.R)/255)*(C.R/255));
  Result.G:=Trunc((A.G/255)*(B.G/255)+((255-A.G)/255)*(C.G/255));
  Result.B:=Trunc((A.B/255)*(B.B/255)+((255-A.B)/255)*(C.B/255));
  Result.A:=Trunc((A.A/255)*(B.A/255)+((255-A.A)/255)*(C.A/255));
End;

Function ColorNegative(Color:Color):Color;
Begin
  Result.R:=255-Color.R;
  Result.G:=255-Color.G;
  Result.B:=255-Color.B;
  Result.A:=Color.A;
End;

Function ColorSwap(Color:Color):Color;
Var
  N:Byte;
Begin
  N:=Color.R;
  Color.R:=Color.B;
  Color.B:=N;
  Result:=Color;
End;

Function ColorRGB15Decode(Source:Word):Color;
Var
  Temp:Word;
Begin
  With Result Do
  Begin
    Temp:=Word(Source Shl 11);
    Temp:=Word(Temp Shr 11);
    R:=Temp Shl 3;

    Temp:=Word(Source Shr 5);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    G:=Temp Shl 3;

    Temp:=Word(Source Shr 10);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    B:=Temp Shl 3;

    A:=255;
  End;
End;

Function ColorRGB8Decode(Source:Byte):Color;
Begin
	Result.R := Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G := Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.B := Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A := 255;
End;

Function ColorRGB8Encode(Source:Color):Byte;
Begin
	If Source.A=0 Then
	Begin
		Result:=0;
		Exit;
	End;

	// Convert from 0..255 to 0..7 range
	Source.R := Source.R Shr 5;
	Source.G := Source.G Shr 5;
	Source.B := Source.B Shr 6;
	Result := Source.B + (Source.G Shl 2) + (Source.R Shl 5);

  If (Result=0) Then
    Result := 1;
End;

Function ColorRGB15Encode(Source:Color):Word;
Begin
  // Convert from 0..255 to 0..63 range
  Source.R:=Source.R Shr 3;
  Source.G:=Source.G Shr 3;
  Source.B:=Source.B Shr 3;
  Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorHue(Source:Color):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := ColorRGBToHSL(Source).R;
End;

Function ColorSaturation(Source:Color):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := ColorRGBToHSL(Source).G;
End;

Function ColorLuminance(Source:Color):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Trunc(Source.R*0.3+Source.G*0.59+Source.B*0.11);
End;

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := GreyLevel;
  Result.G := GreyLevel;
  Result.B := GreyLevel;
  Result.A := Alpha;
End;

Function ColorHSLToRGB(Input:Color):Color;
Var
	v,r,g,b:Single;
  h,sl,l:Single;
  m,sv:Single;
  sextant:Integer;
  fract, vsf, mid1, mid2:Single;
Begin
  h := Input.R/255.0;
	sl := Input.G/255.0;
	l := Input.B/255.0;

	r := l;   // default to gray
	g := r;
	b := r;

  If (L<0.5) Then
	  v := (l * (1.0 + sl))
  Else
    v := (l + sl - l * sl);

  If (v > 0) Then
	Begin
		m := l + l - v;
		sv := (v - m ) / v;
		h := h * 6.0;
		sextant := Trunc(h);
		fract := h - sextant;

    vsf := v * sv * fract;
		mid1 := m + vsf;
		mid2 := v - vsf;

    Case sextant Of
      0:  Begin
				    r := v;
				    g := mid1;
				    b := m;
				  End;

			1:  Begin
				    r := mid2;
				    g := v;
				    b := m;
				  End;

      2:  Begin
				    r := m;
				    g := v;
				    b := mid1;
				  End;

			3:  Begin
				    r := m;
				    g := mid2;
				    b := v;
				  End;

			4:  Begin
				    r := mid1;
				    g := m;
				    b := v;
				  End;

			5:  Begin
				    r := v;
				    g := m;
				    b := mid2;
				  End;
		End;
	End;

	Result.R := Trunc(R * 255.0);
	Result.G := Trunc(G * 255.0);
	Result.B := Trunc(B * 255.0);
  Result.A := Input.A;
End;

// Given a Color (RGB Struct) in range of 0-255
// Return H,S,L in range of 0-1
Function ColorRGBToHSL(Input:Color):Color;
Var
  h,s,l, r,g,b:Single;
  v, m, vm, r2, g2, b2:Single;
Begin
  r := Input.R/255.0;
	g := Input.G/255.0;
	b := Input.B/255.0;

  // default to black
  Result.R := 0;
  Result.G := 0;
  Result.B := 0;
  Result.A := Input.A;

	v := r;
	If (g>v) Then
		v := g;
	If (b>v) Then
		v := b;
	m := r;
	if (g<m) Then
		m := g;
	if (b<m) Then
		m := b;

	l := (m + v) / 2.0;

  If (l <= 0.0) Then
    Exit;

	vm := v - m;
	s := vm;

  if (s > 0.0) Then
	Begin
    If (l <= 0.5) Then
		  s := s / (v + m )
    Else
      s := s / (2.0 - v - m) ;
  End Else
    Exit;

  r2 := (v - r) / vm;
  g2 := (v - g) / vm;
  b2 := (v - b) / vm;

	If (r = v) Then
	Begin
    If (g = m) Then
		  h := 5.0 + b2
    Else
      h := 1.0 - g2;
	End Else
	If (g = v) Then
	Begin
    If (b = m) Then
      h := 1.0 + r2
    Else
      h := 3.0 - b2;
	End Else
	Begin
		If (r = m) Then
      H := 3.0 + g2
    Else
      H := 5.0 - r2;
	End;

	h := h / 6.0;
	Result.R := Trunc(H * 255.0);
	Result.G := Trunc(S * 255.0);
	Result.B := Trunc(L * 255.0);
End;

// color format conversions
Function ColorRGB16To32(Source:Word):Color;
Var
	Temp:Word;
Begin
  With Result Do
  Begin
    Temp:=Word(Source Shl 11);
    Temp:=Word(Temp Shr 11);
    R:=Temp Shl 3;

    Temp:=Word(Source Shr 5);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    G:=Temp Shl 3;

    Temp:=Word(Source Shr 10);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    B:=Temp Shl 3;
  End;
End;

Function ColorRGB32To16(Source:Color):Word;
Begin
	// Convert from 0..255 to 0..63 range
	Source.R:=Source.R Shr 3;
	Source.G:=Source.G Shr 3;
	Source.B:=Source.B Shr 3;
	Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorRGB8To32(Source:Byte):Color;
Begin
	Result.R:=Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G:=Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.B:=Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A:=255;
End;

Function ColorRGB32To8(Source:Color):Byte;
Begin
	If Source.A<200 Then
	Begin
		Result:=0;
		Exit;
	End;

	// Convert from 0..255 to 0..7 range
	Source.R:=Source.R Shr 5;
	Source.G:=Source.G Shr 5;
	Source.B:=Source.B Shr 6;
	Result:=Source.B + (Source.G Shl 2) + (Source.R Shl 5);

  {If (Result=0) Then
    Result:=1;}
End;

Function ColorBGR16To32(Source:Word):Color;
Var
	Temp:Word;
Begin
  With Result Do
  Begin
    Temp:=Word(Source Shl 11);
    Temp:=Word(Temp Shr 11);
    B:=Temp Shl 3;

    Temp:=Word(Source Shr 5);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    G:=Temp Shl 3;

    Temp:=Word(Source Shr 10);
    Temp:=Word(Temp Shl 11);
    Temp:=Word(Temp Shr 11);
    R:=Temp Shl 3;
  End;
End;

Function ColorBGR32To16(Source:Color):Word;
Begin
	// Convert from 0..255 to 0..63 range
	Source.R:=Source.R Shr 3;
	Source.G:=Source.G Shr 3;
	Source.B:=Source.B Shr 3;
	Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorBGR8To32(Source:Byte):Color;
Begin
	Result.B:=Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G:=Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.R:=Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A:=255;
End;

Function ColorBGR32To8(Source:Color):Byte;
Begin
	If Source.A=0 Then
	Begin
		Result:=0;
		Exit;
	End;

	// Convert from 0..255 to 0..7 range
	Source.R:=Source.R Shr 6;
	Source.G:=Source.G Shr 5;
	Source.B:=Source.B Shr 5;
	Result:=Source.R + (Source.G Shl 2) + (Source.B Shl 5);
  If (Result=0) Then
    Result:=1;
End;


End.
