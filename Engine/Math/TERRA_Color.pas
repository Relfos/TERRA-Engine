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
Uses TERRA_String, TERRA_Utils, TERRA_Vector3D;

{$R-}

Type
  PColor = ^Color;
  Color = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
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

  ColorHSL = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    H:Byte;
    S:Byte;
    L:Byte;
    A:Byte;
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

Function ColorToString(Const N:Color):TERRAString;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):Color;
Function ColorCreateFromString(HexValue:TERRAString):Color;
Function ColorCreateFromFloat(Const R,G,B:Single; A:Single=1.0):Color;
Function ColorCreateFromVector3D(Const V:Vector3D; A:Single=1.0):Color;

Function ColorHSLCreate(Const H,S,L:Byte; A:Byte=255):ColorHSL;

// Mixes colors
Function ColorMix(Const A,B:Color; Const Cur:Single):Color;Overload;
Function ColorMix(Const A,B,C:Color; Const U,V:Single):Color;Overload;

// blends A with B, using A alpha
Function ColorBlend(Const Src,Dest:Color):Color; Overload;
Function ColorBlend(Const Src,Dest:Color; A:Cardinal):Color; Overload;

// Scale color
Function ColorScale(Const A:Color; B:Single):Color;

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

Function ColorRGBToHSL(Const Input:Color):ColorHSL;
Function ColorHSLToRGB(Const Input:ColorHSL):Color;

  // Color conversion
  Function ColorRGB16To32(Source:Word):Color;
  Function ColorRGB32To16(Source:Color):Word;
  Function ColorRGB32To8(Source:Color):Byte;
  Function ColorRGB8To32(Source:Byte):Color;

  Function ColorBGR16To32(Source:Word):Color;
  Function ColorBGR32To16(Source:Color):Word;
  Function ColorBGR32To8(Source:Color):Byte;
  Function ColorBGR8To32(Source:Byte):Color;


Type
  ColorCombineMode = (
    combineNone,
    combineBlend,
    combineMultiply,
    combineAdd,
    combineSubtract,
    combineDifference,
    combineScreen,
    combineOverlay,
    combineHardLight,
    combineSoftLight,
    combineDarken,
    combineLighten,
    combineDodge,
    combineBurn,
    combineColor,
    combineHue,
    combineSaturation,
    combineLuminosity
    );

Function ColorCombine(Const A, B:Color; Mode:ColorCombineMode):Color;

Function ColorAdd(Const A,B:Color):Color;
Function ColorMultiply(Const A,B:Color):Color;
Function ColorSubtract(Const A,B:Color):Color;
Function ColorDifference(Const A,B:Color):Color;
Function ColorScreen(Const A,B:Color):Color;
Function ColorOverlay(Const A,B:Color):Color;
Function ColorHardLight(Const A,B:Color):Color;
Function ColorSoftLight(Const A,B:Color):Color;
Function ColorDarken(Const A,B:Color):Color;
Function ColorLighten(Const A,B:Color):Color;
Function ColorDodge(Const A,B:Color):Color;
Function ColorBurn(Const A,B:Color):Color;
Function ColorCombineColor(Const A,B:Color):Color;
Function ColorCombineHue(Const A,B:Color):Color;
Function ColorCombineSaturation(Const A,B:Color):Color;
Function ColorCombineLuminosity(Const A,B:Color):Color;

Implementation
Uses TERRA_Math;

// Color functions
Function ColorToString(Const N:Color):TERRAString;
Begin
  Result := '#'+HexStr(N.R)+HexStr(N.G)+ HexStr(N.B)+ HexStr(N.A);
End;

Function ColorCreateFromString(HexValue:TERRAString):Color;
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

Function ColorCreateFromFloat(Const R,G,B:Single;A:Single=1.0):Color; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := Byte(Trunc(R*255));
  Result.G := Byte(Trunc(G*255));
  Result.B := Byte(Trunc(B*255));
  Result.A := Byte(Trunc(A*255));
End;

Function ColorCreateFromVector3D(Const V:Vector3D; A:Single=1.0):Color;
Begin
  Result := ColorCreateFromFloat(V.X, V.Y, V.Z, A);
End;

Function ColorHSLCreate(Const H,S,L:Byte; A:Byte=255):ColorHSL;
Begin
  Result.H := H;
  Result.S := S;
  Result.L := L;
  Result.A := A;
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

Function ColorScale(Const A:Color; B:Single):Color;
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
  Result := ColorRGBToHSL(Source).H;
End;

Function ColorSaturation(Source:Color):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := ColorRGBToHSL(Source).S;
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

Function ColorHSLToRGB(Const Input:ColorHSL):Color;
Var
	v,r,g,b:Single;
  H,Sl,L:Single;
  m,sv:Single;
  sextant:Integer;
  fract, vsf, mid1, mid2:Single;

  RGB, Temp:Vector3D;

Begin
(*  H := Input.H/255.0;
	S := Input.S/255.0;
	L := Input.L/255.0;

  RGB.X := Clamp(Abs(FloatMod(H*6.0 + 0.0, 6.0) -3.0)-1.0, 0.0, 1.0 );
  RGB.Y := Clamp(Abs(FloatMod(H*6.0+ 4.0, 6.0)-3.0)-1.0, 0.0, 1.0 );
  RGB.Z := Clamp(Abs(FloatMod(H*6.0+ 2.0, 6.0)-3.0)-1.0, 0.0, 1.0 );

  Temp := VectorMultiply(RGB, RGB);

  RGB := VectorSubtract(VectorConstant(3.0), VectorScale(RGB, 2.0));

	RGB := VectorMultiply(Temp, RGB); // cubic smoothing

  Result := ColorMix(ColorWhite, ColorCreateFromVector3D(RGB), S);

  Result.R := Trunc(255 * L * Result.R);
  Result.G := Trunc(255 * L * Result.G);
  Result.B := Trunc(255 * L * Result.B);
  Result.A :=Input.A;*)

  h := Input.H/255.0;
	sl := Input.S/255.0;
	l := Input.L/255.0;

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
Function ColorRGBToHSL(Const Input:Color):ColorHSL;
Var
  h,s,l, r,g,b:Single;
  v, m, vm, r2, g2, b2:Single;
Begin
  r := Input.R/255.0;
	g := Input.G/255.0;
	b := Input.B/255.0;

  // default to black
  Result.H := 0;
  Result.S := 0;
  Result.L := 0;
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
  Begin
    Result.H := 0;
    Result.S := 0;
    Result.L := Input.R;
    Exit;
  End;

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
	Result.H := Trunc(H * 255.0);
	Result.S := Trunc(S * 255.0);
	Result.L := Trunc(L * 255.0);
  Result.A := Input.A;
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

Function ColorAdd(Const A,B:Color):Color;  
Begin
  Result.R := IntMin(Integer(Trunc(Integer(A.R + B.R))), 255);
  Result.G := IntMin(Integer(Trunc(Integer(A.G + B.G))), 255);
  Result.B := IntMin(Integer(Trunc(Integer(A.B + B.B))), 255);
  Result.A := IntMin(Integer(Trunc(Integer(A.A + B.A))), 255);
End;

Function ColorMultiply(Const A,B:Color):Color; 
Begin
  Result.R:= Trunc((A.R/255)*(B.R/255)*255);
  Result.G:= Trunc((A.G/255)*(B.G/255)*255);
  Result.B:= Trunc((A.B/255)*(B.B/255)*255);
  Result.A:= Trunc((A.A/255)*(B.A/255)*255);
End;


Function ColorSubtract(Const A,B:Color):Color; 
Begin
  Result.R := IntMax(Integer(Trunc(A.R - B.R)), 0);
  Result.G := IntMax(Integer(Trunc(A.G - B.G)), 0);
  Result.B := IntMax(Integer(Trunc(A.B - B.B)), 0);
  Result.A := IntMax(Integer(Trunc(A.A - B.A)), 0);
End;

Function ColorDifference(Const A,B:Color):Color;
Begin
  Result.R := Abs(Integer(A.R - B.R));
  Result.G := Abs(Integer(A.G - B.G));
  Result.B := Abs(Integer(A.B - B.B));
  Result.A := Abs(Integer(A.A - B.A));
End;

Function ColorScreen(Const A,B:Color):Color;
  Function Screen(X,Y:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;

    F := 1.0 - ((1.0 - A) * (1.0 - B));

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;
Begin
  Result.R := Screen(A.R, B.R);
  Result.G := Screen(A.G, B.G);
  Result.B := Screen(A.B, B.B);
  Result.A := Screen(A.A, B.A);
End;

Function ColorOverlay(Const A,B:Color):Color;
  Function Overlay(X,Y:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;
    If (A<0.5) Then
      F := 2.0 * A * B
    Else
      F := 1.0 - 2.0 * (1.0 - A) * (1.0 - B);

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;

Begin
  Result.R := Overlay(A.R, B.R);
  Result.G := Overlay(A.G, B.G);
  Result.B := Overlay(A.B, B.B);
  Result.A := Overlay(A.A, B.A);
End;

{ Hard Light combines Multiply and Screen blend modes.
  Equivalent to Overlay, but with the bottom and top images swapped. }
Function ColorHardLight(Const A,B:Color):Color;
  Function HardLight(Y,X:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;
    If (A<0.5) Then
      F := 2.0 * A * B
    Else
      F := 1.0 - 2.0 * (1.0 - A) * (1.0 - B);

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;

Begin
  Result.R := HardLight(A.R, B.R);
  Result.G := HardLight(A.G, B.G);
  Result.B := HardLight(A.B, B.B);
  Result.A := HardLight(A.A, B.A);
End;

Function ColorSoftLight(Const A,B:Color):Color;
  Function SoftLight(Y,X:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;

    F := (1.0 - 2.0 * B) * Sqr(A) + (2.0 * B * A);

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;

Begin
  Result.R := SoftLight(A.R, B.R);
  Result.G := SoftLight(A.G, B.G);
  Result.B := SoftLight(A.B, B.B);
  Result.A := SoftLight(A.A, B.A);
End;

Function ColorDarken(Const A,B:Color):Color;
  Function Darken(X,Y:Byte):Byte;
  Begin
    If X<Y Then
      Result := X
    Else
      Result := Y;
  End;
Begin
  Result.R := Darken(A.R, B.R);
  Result.G := Darken(A.G, B.G);
  Result.B := Darken(A.B, B.B);
  Result.A := Darken(A.A, B.A);
End;

Function ColorLighten(Const A,B:Color):Color;
  Function Lighten(X,Y:Byte):Byte;
  Begin
    If X>Y Then
      Result := X
    Else
      Result := Y;
  End;
Begin
  Result.R := Lighten(A.R, B.R);
  Result.G := Lighten(A.G, B.G);
  Result.B := Lighten(A.B, B.B);
  Result.A := Lighten(A.A, B.A);
End;

Function ColorDodge(Const A,B:Color):Color;
  Function Dodge(X,Y:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;

    If (A>=1.0) Then
      F := 0
    Else
      F := B / (1.0 - A);

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;

Begin
  Result.R := Dodge(A.R, B.R);
  Result.G := Dodge(A.G, B.G);
  Result.B := Dodge(A.B, B.B);
  Result.A := Dodge(A.A, B.A);
End;

Function ColorBurn(Const A,B:Color):Color;
  Function Burn(X,Y:Byte):Byte;
  Var
    A, B, F:Single;
  Begin
    A := X/255.0;
    B := Y/255.0;

    If (A<=0.0) Then
      F := 0
    Else
      F := (1.0 - B) / A;

    Result := IntMin(Integer(Trunc(F*255.0)), 255);
  End;

Begin
  Result.R := Burn(A.R, B.R);
  Result.G := Burn(A.G, B.G);
  Result.B := Burn(A.B, B.B);
  Result.A := Burn(A.A, B.A);
End;

Function ColorCombineColor(Const A,B:Color):Color;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.H := Y.H;
  X.S := Y.S;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineHue(Const A,B:Color):Color;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.H := Y.H;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineSaturation(Const A,B:Color):Color;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.S := Y.S;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineLuminosity(Const A,B:Color):Color;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.L := Y.L;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombine(Const A, B:Color; Mode:ColorCombineMode):Color;
Begin
  Case Mode Of
    combineBlend:
      Begin
        Result := B;
      End;

    combineMultiply:
      Begin
        Result := ColorMultiply(A, B);
      End;

    combineAdd:
      Begin
        Result := ColorAdd(A, B);
      End;

    combineSubtract:
      Begin
        Result := ColorSubtract(A, B);
      End;

    combineDifference:
      Begin
        Result := ColorDifference(A, B);
      End;

    combineScreen:
      Begin
        Result := ColorScreen(A, B);
      End;

    combineOverlay:
      Begin
        Result := ColorOverlay(A, B);
      End;

    combineHardLight:
      Begin
        Result := ColorHardLight(A, B);
      End;

    combineSoftLight:
      Begin
        Result := ColorSoftLight(A, B);
      End;

    combineDarken:
      Begin
        Result := ColorDarken(A, B);
      End;

    combineLighten:
      Begin
        Result := ColorLighten(A, B);
      End;

    combineDodge:
      Begin
        Result := ColorDodge(A, B);
      End;

    combineBurn:
      Begin
        Result := ColorBurn(A, B);
      End;

    combineColor:
      Begin
        Result := ColorCombineColor(A, B);
      End;

    combineHue:
      Begin
        Result := ColorCombineHue(A, B);
      End;

    combineSaturation:
      Begin
        Result := ColorCombineSaturation(A, B);
      End;

    combineLuminosity:
      Begin
        Result := ColorCombineLuminosity(A, B);
      End;

    Else
      Result := A;
  End;
End;

End.
