Unit TERRA_ColorDither;

Interface

// all dither functions return either 1 or 0 only
Function ColorDither2x2(X,Y:Integer; Const Value:Single):Integer;
Function ColorDither4x4(X,Y:Integer; Const Value:Single):Integer;
Function ColorDither8x8(X,Y:Integer; Const Value:Single):Integer;

Implementation

Const
  Dither2x2:Array[0..3] Of Single = (0.25, 0.75, 1.0, 0.5);

Function ColorDither2x2(X,Y:Integer; Const Value:Single):Integer;
Var
  Limit:Single;
Begin
  Limit := Dither2x2[(X Mod 2)+ (Y Mod 2)*2];
  
  If (Value>=Limit) Then
    Result := 1
  Else
    Result := 0;
End;

Const
  Dither4x4 :Array[0..15] Of Integer = (
    1, 33, 9, 41,
    49, 17, 57, 25,
    13, 45, 5, 37,
    61, 29, 53, 21);

Function ColorDither4x4(X,Y:Integer; Const Value:Single):Integer;
Var
  Limit:Single;
Begin
  Limit := Succ(Dither4x4[(X Mod 4)+ (Y Mod 4)*4]) / 64.0;

  If (Value>=Limit) Then
    Result := 1
  Else
    Result := 0;
End;

Const
  Dither8x8:Array[0..63] Of Integer = (
    0, 32, 8, 40, 2, 34, 10, 42,
    48, 16, 56, 24, 50, 18, 58, 26,
    12, 44, 4, 36, 14, 46, 6, 38,
    60, 28, 52, 20, 62, 30, 54, 22,
     3, 35, 11, 43, 1, 33, 9, 41,
    51, 19, 59, 27, 49, 17, 57, 25,
    15, 47, 7, 39, 13, 45, 5, 37,
    63, 31, 55, 23, 61, 29, 53, 21);

Function ColorDither8x8(X,Y:Integer; Const Value:Single):Integer;
Var
  Limit:Single;
Begin
  Limit := Succ(Dither8x8[(X Mod 8)+ (Y Mod 8)*4]) / 64.0;

  If (Value>=Limit) Then
    Result := 1
  Else
    Result := 0;
End;

End.