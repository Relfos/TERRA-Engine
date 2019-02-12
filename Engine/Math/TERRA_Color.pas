{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_Tween;

{$R-}

Type
  PColorRGBA = ^ColorRGBA;
  {$IFDEF USE_OLD_OBJECTS}
  ColorRGBA = Packed Object
        R:Byte;
        G:Byte;
        B:Byte;
        A:Byte;
  {$ELSE}
  ColorRGBA = Record
    case Integer of
     0: (
        R:Byte;
        G:Byte;
        B:Byte;
        A:Byte);
     1:
     (Col: LongWord);
  {$ENDIF}
  End;

  ColorHSL = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    H:Byte;
    S:Byte;
    L:Byte;
    A:Byte;
  End;

  ColorProperty = Class(TweenableProperty)
    Protected
      _Red:ByteProperty;
      _Green:ByteProperty;
      _Blue:ByteProperty;
      _Alpha:ByteProperty;

      Function GetColorValue:ColorRGBA;
      Procedure SetColorValue(const NewValue:ColorRGBA);

      Procedure UpdateTweens(); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:ColorRGBA);
      Procedure Release(); Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:ColorRGBA; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Class Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Class Function Stringify(Const N:ColorRGBA):TERRAString;

      Property Red:ByteProperty Read _Red;
      Property Green:ByteProperty Read _Green;
      Property Blue:ByteProperty Read _Blue;
      Property Alpha:ByteProperty Read _Alpha;

      Property Value:ColorRGBA Read GetColorValue Write SetColorValue;
  End;

  PColorPalette = ^ColorPalette;
  ColorPalette = Array[0..255] Of ColorRGBA;

Const
// Color constants
  {$IFDEF BGR}
  ColorWhite:ColorRGBA=(B:255; G:255; R:255; A:255);
  ColorBlack:ColorRGBA=(B:0; G:0; R:0; A:255);
  ColorNull:ColorRGBA=(B:0; G:0; R:0; A:0);
  {$ELSE}
  ColorWhite:ColorRGBA=(R:255; G:255; B:255; A:255);
  ColorBlack:ColorRGBA=(R:0; G:0; B:0; A:255);
  ColorNull:ColorRGBA=(R:0; G:0; B:0; A:0);
  {$ENDIF}

  {$IFDEF BGR}
  ColorRed:ColorRGBA=(B:0; G:0; R:255; A:255);
  ColorBlue:ColorRGBA=(B:255; G:0; R:0; A:255);
  ColorGreen:ColorRGBA=(B:0; G:255; R:0; A:255);
  ColorYellow:ColorRGBA=(B:0; G:255; R:255; A:255);
  {$ELSE}
  ColorRed:ColorRGBA=(R:255; G:0; B:0; A:255);
  ColorBlue:ColorRGBA=(R:0; G:0; B:255; A:255);
  ColorGreen:ColorRGBA=(R:0; G:255; B:0; A:255);
  ColorYellow:ColorRGBA=(R:255; G:255; B:0; A:255);
  ColorMagenta:ColorRGBA=(R:255; G:0; B:255; A:255);
  ColorCyan:ColorRGBA=(R:0; G:255; B:255; A:255);
  {$ENDIF}


Function ColorCreate(Const R,G,B:Byte;A:Byte=255):ColorRGBA;
Function ColorCreateFromString(Const Value:TERRAString):ColorRGBA;
Function ColorCreateFromFloat(Const R,G,B:Single; A:Single=1.0):ColorRGBA;
Function ColorCreateFromVector3D(Const V:Vector3D; A:Single=1.0):ColorRGBA;
Function ColorCreateFromNormal(Const N:Vector3D; A:Single=1.0):ColorRGBA;

Function ColorToVector3D(Const C:ColorRGBA):Vector3D;

Function ColorDistance(Const A,B:ColorRGBA):Single;

Function ColorHSLCreate(Const H,S,L:Byte; A:Byte=255):ColorHSL;

// Mixes colors
Function ColorMix(Const A,B:ColorRGBA; Const Cur:Single):ColorRGBA;Overload;
Function ColorMix(Const A,B,C:ColorRGBA; Const U,V:Single):ColorRGBA;Overload;

// blends A with B, using A alpha
Function ColorBlend(Const Src,Dest:ColorRGBA):ColorRGBA; Overload;
Function ColorBlend(Const Src,Dest:ColorRGBA; A:Cardinal):ColorRGBA; Overload;

// Scale color
Function ColorScale(Const A:ColorRGBA; B:Single):ColorRGBA;

// Color lerp
Function ColorLerp(Const A,B,C:ColorRGBA):ColorRGBA;

// Swap color channels
Function ColorSwap(Color:ColorRGBA):ColorRGBA;

Function ColorNegative(Color:ColorRGBA):ColorRGBA;

// Color conversion routines
Function ColorRGB15Decode(Source:Word):ColorRGBA;
Function ColorRGB15Encode(Source:ColorRGBA):Word;

Function ColorRGB8Decode(Source:Byte):ColorRGBA;
Function ColorRGB8Encode(Source:ColorRGBA):Byte;

Function ColorLuminance(Source:ColorRGBA):Byte;
Function ColorHue(Source:ColorRGBA):Byte;
Function ColorSaturation(Source:ColorRGBA):Byte;  

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):ColorRGBA;

Function ColorRGBToHSL(Const Input:ColorRGBA):ColorHSL;
Function ColorHSLToRGB(Const Input:ColorHSL):ColorRGBA;

// Color conversion
Function ColorRGBA444To32(Source:Word):ColorRGBA;
Function ColorRGB555To32(Source:Word):ColorRGBA;
Function ColorRGBA555To32(Source:Word):ColorRGBA;
Function ColorRGB565To32(Source:Word):ColorRGBA;

Function ColorRGB32To16(Source:ColorRGBA):Word;
Function ColorRGB32To8(Source:ColorRGBA):Byte;
Function ColorRGB8To32(Source:Byte):ColorRGBA;

Function ColorBGR16To32(Source:Word):ColorRGBA;
Function ColorBGR32To16(Source:ColorRGBA):Word;
Function ColorBGR32To8(Source:ColorRGBA):Byte;
Function ColorBGR8To32(Source:Byte):ColorRGBA;


Type
  ColorCombineMode = (
    combineFirst,
    combineSecond,
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

Function ColorCombine(Const A, B:ColorRGBA; Mode:ColorCombineMode):ColorRGBA;

Function ColorAdd(Const A,B:ColorRGBA):ColorRGBA;
Function ColorMultiply(Const A,B:ColorRGBA):ColorRGBA;
Function ColorSubtract(Const A,B:ColorRGBA):ColorRGBA;
Function ColorDifference(Const A,B:ColorRGBA):ColorRGBA;
Function ColorScreen(Const A,B:ColorRGBA):ColorRGBA;
Function ColorOverlay(Const A,B:ColorRGBA):ColorRGBA;
Function ColorHardLight(Const A,B:ColorRGBA):ColorRGBA;
Function ColorSoftLight(Const A,B:ColorRGBA):ColorRGBA;
Function ColorDarken(Const A,B:ColorRGBA):ColorRGBA;
Function ColorLighten(Const A,B:ColorRGBA):ColorRGBA;
Function ColorDodge(Const A,B:ColorRGBA):ColorRGBA;
Function ColorBurn(Const A,B:ColorRGBA):ColorRGBA;
Function ColorCombineColor(Const A,B:ColorRGBA):ColorRGBA;
Function ColorCombineHue(Const A,B:ColorRGBA):ColorRGBA;
Function ColorCombineSaturation(Const A,B:ColorRGBA):ColorRGBA;
Function ColorCombineLuminosity(Const A,B:ColorRGBA):ColorRGBA;

Implementation
Uses TERRA_Math, TERRA_ColorNames;

// Color functions
Function ColorCreateFromString(Const Value:TERRAString):ColorRGBA;
  Function H(C:TERRAChar):Byte;
  Begin
    C := CharUpper(C);
    If (C>='0') And (C<='9') Then
      Result := Byte(Ord(C) - Ord('0'))
    Else
      Result := Byte(Ord(C) - Ord('A') + 10);
  End;

Var
  It:StringIterator;
  Init:TERRAChar;
  A,B:Byte;
Begin
  It := StringCreateIterator(Value);

  Init := It.GetNext();
  If (Init = '#') Then
  Begin
    A := H(It.GetNext());
  End Else
  Begin
    Result := CreateColorFromName(Value);
    If Result.A>0 Then
    Begin
      ReleaseObject(It);
      Exit;
    End;

    A := H(Init);
  End;

  B := H(It.GetNext());

  Result.R := A Shl 4 + B;

  A := H(It.GetNext());
  B := H(It.GetNext());
  Result.G := A Shl 4 + B;


  A := H(It.GetNext());
  B := H(It.GetNext());
  Result.B := A Shl 4 + B;

  If (It.HasNext()) Then
  Begin
    A := H(It.GetNext());
    B := H(It.GetNext());
    Result.A := A Shl 4 + B;
  End Else
    Result.A := 255;

  ReleaseObject(It);
End;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
End;

Function ColorCreateFromFloat(Const R,G,B:Single;A:Single=1.0):ColorRGBA; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := Byte(Trunc(R*255));
  Result.G := Byte(Trunc(G*255));
  Result.B := Byte(Trunc(B*255));
  Result.A := Byte(Trunc(A*255));
End;

Function ColorToVector3D(Const C:ColorRGBA):Vector3D;
Begin
  Result.X := C.R / 255;
  Result.Y := C.G / 255;
  Result.Z := C.B / 255;
End;

Function ColorCreateFromVector3D(Const V:Vector3D; A:Single=1.0):ColorRGBA;
Begin
  Result := ColorCreateFromFloat(V.X, V.Y, V.Z, A);
End;

Function ColorCreateFromNormal(Const N:Vector3D; A:Single=1.0):ColorRGBA;
Begin
  Result := ColorCreateFromFloat(N.X*0.5 + 0.5, N.Y*0.5 + 0.5, N.Z*0.5 + 0.5, A);
End;

Function ColorHSLCreate(Const H,S,L:Byte; A:Byte=255):ColorHSL;
Begin
  Result.H := H;
  Result.S := S;
  Result.L := L;
  Result.A := A;
End;

{$OverFlowChecks Off}

Function ColorBlend(Const Src,Dest:ColorRGBA; A:Cardinal):ColorRGBA;{$IFDEF FPC} Inline;{$ENDIF}
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

	Result := ColorRGBA(Cardinal(rb Or ag));
End;

Function ColorBlend(Const Src,Dest:ColorRGBA):ColorRGBA;{$IFDEF FPC} Inline;{$ENDIF}
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

	Result := ColorRGBA(Cardinal(rb Or g Or A));

    //Result.A := IntMax(Result.A, Src.A);
End;

Function ColorMix(Const A,B:ColorRGBA; Const Cur:Single):ColorRGBA; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc(A.R*Cur+B.R*(1-Cur));
  Result.G:=Trunc(A.G*Cur+B.G*(1-Cur));
  Result.B:=Trunc(A.B*Cur+B.B*(1-Cur));
  Result.A:=Trunc(A.A*Cur+B.A*(1-Cur));
End;

Function ColorMix(Const A,B,C:ColorRGBA; Const U,V:Single):ColorRGBA; {$IFDEF FPC} Inline;{$ENDIF}
Var
  W:Single;
Begin
  W:=1-(U+V);
  Result.R:=Byte(Trunc(A.R*U+B.R*V+C.R*W));
  Result.G:=Byte(Trunc(A.G*U+B.G*V+C.G*W));
  Result.B:=Byte(Trunc(A.B*U+B.B*V+C.B*W));
  Result.A:=255;
End;

Function ColorScale(Const A:ColorRGBA; B:Single):ColorRGBA;
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

Function ColorLerp(Const A,B,C:ColorRGBA):ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc((A.R/255)*(B.R/255)+((255-A.R)/255)*(C.R/255));
  Result.G:=Trunc((A.G/255)*(B.G/255)+((255-A.G)/255)*(C.G/255));
  Result.B:=Trunc((A.B/255)*(B.B/255)+((255-A.B)/255)*(C.B/255));
  Result.A:=Trunc((A.A/255)*(B.A/255)+((255-A.A)/255)*(C.A/255));
End;

Function ColorNegative(Color:ColorRGBA):ColorRGBA;
Begin
  Result.R:=255-Color.R;
  Result.G:=255-Color.G;
  Result.B:=255-Color.B;
  Result.A:=Color.A;
End;

Function ColorSwap(Color:ColorRGBA):ColorRGBA;
Var
  N:Byte;
Begin
  N := Color.R;
  Color.R := Color.B;
  Color.B := N;
  Result:=Color;
End;

Function ColorRGB15Decode(Source:Word):ColorRGBA;
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

Function ColorRGB8Decode(Source:Byte):ColorRGBA;
Begin
	Result.R := Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G := Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.B := Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A := 255;
End;

Function ColorRGB8Encode(Source:ColorRGBA):Byte;
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

Function ColorRGB15Encode(Source:ColorRGBA):Word;
Begin
  // Convert from 0..255 to 0..63 range
  Source.R:=Source.R Shr 3;
  Source.G:=Source.G Shr 3;
  Source.B:=Source.B Shr 3;
  Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorHue(Source:ColorRGBA):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := ColorRGBToHSL(Source).H;
End;

Function ColorSaturation(Source:ColorRGBA):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := ColorRGBToHSL(Source).S;
End;

Function ColorLuminance(Source:ColorRGBA):Byte;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Trunc(Source.R*0.3+Source.G*0.59+Source.B*0.11);
End;

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):ColorRGBA;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := GreyLevel;
  Result.G := GreyLevel;
  Result.B := GreyLevel;
  Result.A := Alpha;
End;

Function ColorHSLToRGB(Const Input:ColorHSL):ColorRGBA;
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
Function ColorRGBToHSL(Const Input:ColorRGBA):ColorHSL;
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
Function ColorRGB565To32(Source:Word):ColorRGBA;
Begin
  With Result Do
  Begin
    R := (Source and $F800) shr 8;
    G := (Source and $7E0) shr 3;
    B := (Source and $1F) shl 3;
    A := $FF;
  End;
End;

Function ColorRGBA555To32(Source:Word):ColorRGBA;
Begin
  With Result Do
  Begin
    R := (Source and $7C00) shr 7;
    G := (Source and $3E0) shr 2;
    B := (Source and $1F) shl 3;
    // If you can not show the "and $FF" error analysis for "or $FF"
    A := ((Source and $8000) shr 15) and $FF;
  End;
End;

Function ColorRGB555To32(Source:Word):ColorRGBA;
Begin
  With Result Do
  Begin
    R := (Source and $7C00) shr 7;
    G := (Source and $3E0) shr 2;
    B := (Source and $1F) shl 3;
    A := $FF;
  End;
End;

Function ColorRGBA444To32(Source:Word):ColorRGBA;
Begin
  With Result Do
  Begin
    R := (Source and $F00) shr 4;
    G := Source and $F0;
    B := (Source and $F) shl 4;
    A := (Source and $F000) shr 8;
  End;
End;

Function ColorRGB32To16(Source:ColorRGBA):Word;
Begin
	// Convert from 0..255 to 0..63 range
	Source.R:=Source.R Shr 3;
	Source.G:=Source.G Shr 3;
	Source.B:=Source.B Shr 3;
	Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorRGB8To32(Source:Byte):ColorRGBA;
Begin
	Result.R:=Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G:=Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.B:=Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A:=255;
End;

Function ColorRGB32To8(Source:ColorRGBA):Byte;
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

Function ColorBGR16To32(Source:Word):ColorRGBA;
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

Function ColorBGR32To16(Source:ColorRGBA):Word;
Begin
	// Convert from 0..255 to 0..63 range
	Source.B:=Source.B Shr 3;
	Source.G:=Source.G Shr 3;
	Source.R:=Source.R Shr 3;
	Result:=Source.R + ((Source.G Shl 5) + (Source.B Shl 10));
End;

Function ColorBGR8To32(Source:Byte):ColorRGBA;
Begin
	Result.B := Byte((((Source Shr 5) And $07) * 255 Div 7));
	Result.G := Byte(((Source Shr 2) And $07) * 255 Div 7);
	Result.R := Byte(((Source Shr 0) And $03) * 255 Div 3);
	Result.A :=255;
End;

Function ColorBGR32To8(Source:ColorRGBA):Byte;
Begin
	If Source.A=0 Then
	Begin
		Result:=0;
		Exit;
	End;

	// Convert from 0..255 to 0..7 range
	Source.B := Source.B Shr 6;
	Source.G := Source.G Shr 5;
	Source.R := Source.R Shr 5;
	Result := Source.R + (Source.G Shl 2) + (Source.B Shl 5);
  If (Result=0) Then
    Result:=1;
End;

Function ColorDistance(Const A,B:ColorRGBA):Single;
Begin
  Result := Trunc(Sqrt(Sqr(A.R - B.R) + Sqr(A.G - B.G) + Sqr(A.B - B.B) + Sqr(A.A - B.A) ));
End;

Function ColorAdd(Const A,B:ColorRGBA):ColorRGBA;
Begin
  Result.R := IntMin(Integer(Trunc(Integer(A.R + B.R))), 255);
  Result.G := IntMin(Integer(Trunc(Integer(A.G + B.G))), 255);
  Result.B := IntMin(Integer(Trunc(Integer(A.B + B.B))), 255);
  Result.A := IntMin(Integer(Trunc(Integer(A.A + B.A))), 255);
End;

Function ColorMultiply(Const A,B:ColorRGBA):ColorRGBA;
Begin
  Result.R:= Trunc((A.R/255)*(B.R/255)*255);
  Result.G:= Trunc((A.G/255)*(B.G/255)*255);
  Result.B:= Trunc((A.B/255)*(B.B/255)*255);
  Result.A:= Trunc((A.A/255)*(B.A/255)*255);
End;


Function ColorSubtract(Const A,B:ColorRGBA):ColorRGBA; 
Begin
  Result.R := IntMax(Integer(Trunc(A.R - B.R)), 0);
  Result.G := IntMax(Integer(Trunc(A.G - B.G)), 0);
  Result.B := IntMax(Integer(Trunc(A.B - B.B)), 0);
  Result.A := IntMax(Integer(Trunc(A.A - B.A)), 0);
End;

Function ColorDifference(Const A,B:ColorRGBA):ColorRGBA;
Begin
  Result.R := Abs(Integer(A.R - B.R));
  Result.G := Abs(Integer(A.G - B.G));
  Result.B := Abs(Integer(A.B - B.B));
  Result.A := Abs(Integer(A.A - B.A));
End;

Function ColorScreen(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorOverlay(Const A,B:ColorRGBA):ColorRGBA;
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
Function ColorHardLight(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorSoftLight(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorDarken(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorLighten(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorDodge(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorBurn(Const A,B:ColorRGBA):ColorRGBA;
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

Function ColorCombineColor(Const A,B:ColorRGBA):ColorRGBA;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.H := Y.H;
  X.S := Y.S;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineHue(Const A,B:ColorRGBA):ColorRGBA;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.H := Y.H;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineSaturation(Const A,B:ColorRGBA):ColorRGBA;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.S := Y.S;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombineLuminosity(Const A,B:ColorRGBA):ColorRGBA;
Var
  X,Y:ColorHSL;
Begin
  X := ColorRGBToHSL(A);
  Y := ColorRGBToHSL(B);

  X.L := Y.L;

  Result := ColorHSLToRGB(X);
End;

Function ColorCombine(Const A, B:ColorRGBA; Mode:ColorCombineMode):ColorRGBA;
Begin
  Case Mode Of
    combineFirst:
      Begin
        Result := A;
        Exit;
      End;

    combineSecond:
      Begin
        Result := B;
        Exit;
      End;

    combineBlend:
      Begin
        Result := A;
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

  Result := ColorMix(Result, B, Result.A/255);
  Result.A := 255;
End;

{ ColorProperty }
Constructor ColorProperty.Create(Const Name:TERRAString; const InitValue:ColorRGBA);
Begin
  _ObjectName := Name;
  _Red := ByteProperty.Create('r', InitValue.R);
  _Green := ByteProperty.Create('g', InitValue.G);
  _Blue := ByteProperty.Create('b', InitValue.B);
  _Alpha := ByteProperty.Create('a', InitValue.A);
End;

Procedure ColorProperty.Release;
Begin
  ReleaseObject(_Red);
  ReleaseObject(_Green);
  ReleaseObject(_Blue);
  ReleaseObject(_Alpha);
End;

Procedure ColorProperty.SetColorValue(const NewValue:ColorRGBA);
Begin
  Red.Value := NewValue.R;
  Green.Value := NewValue.G;
  Blue.Value := NewValue.B;
  Alpha.Value := NewValue.A;
End;

Function ColorProperty.GetColorValue:ColorRGBA;
Begin
  Result.R := Red.Value;
  Result.G := Green.Value;
  Result.B := Blue.Value;
  Result.A := Alpha.Value;
End;

Class Function ColorProperty.GetObjectType: TERRAString;
Begin
  Result := 'color';
End;

Procedure ColorProperty.AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.AddTween(Ease, ColorCreateFromString(StartValue), ColorCreateFromString(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure ColorProperty.AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:ColorRGBA; Duration, Delay:Cardinal; Callback: TweenCallback; CallTarget:TERRAObject);
Begin
  Self.Red.AddTween(Ease, StartValue.R, TargetValue.R, Duration, Delay, Callback, CallTarget);
  Self.Green.AddTween(Ease, StartValue.G, TargetValue.G, Duration, Delay, Nil);
  Self.Blue.AddTween(Ease, StartValue.B, TargetValue.B, Duration, Delay, Nil);
  Self.Alpha.AddTween(Ease, StartValue.A, TargetValue.A, Duration, Delay, Nil);
End;

Function ColorProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := Red;
  1:  Result := Green;
  2:  Result := Blue;
  3:  Result := Alpha;
  Else
    Result := Nil;
  End;
End;

Function ColorProperty.GetBlob: TERRAString;
Begin
  Result := ColorProperty.Stringify(Self.GetColorValue());
End;

Procedure ColorProperty.SetBlob(const Blob: TERRAString);
Begin
  Self.SetColorValue(ColorCreateFromString(Blob));
End;

Procedure ColorProperty.UpdateTweens;
Begin
  Red.UpdateTweens();
  Green.UpdateTweens();
  Blue.UpdateTweens();
  Alpha.UpdateTweens();
End;


Class Function ColorProperty.Stringify(Const N:ColorRGBA):TERRAString;
Begin
  Result := '#'+HexStr(N.R)+HexStr(N.G)+ HexStr(N.B)+ HexStr(N.A);
End;

End.
