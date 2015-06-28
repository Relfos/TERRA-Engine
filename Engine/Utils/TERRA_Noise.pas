Unit TERRA_Noise;
{$I terra.inc}
//http://www.gamedev.net/community/forums/mod/journal/journal.asp?jn=339903&reply_id=3089065

Interface

Uses TERRA_Utils, TERRA_Object, TERRA_Image, TERRA_Vector3D;

Const
  GradientTableSize = 256;
  CellGridSize = 32;


Type
  NoiseGenerator = Class(TERRAObject)
    Public
      Procedure Release; Override;

      Function Noise(X,Y,Z:Single):Single; Virtual; Abstract;
      //Function TiledNoise(X,Y,Z:Single; W, H:Single):Single;

      Procedure SaveToImage(Target:Image; Layer:Single; ColorMask:Cardinal);
    End;

  PerlinNoiseGenerator = Class(NoiseGenerator)
    Protected
      _Gradients:Array[0..Pred(GradientTableSize*3)] Of Single;

      Procedure InitGradients;

      Function Permutate(x: Integer): Integer;
      Function Index(ix, iy, iz: Integer): Integer;
      Function Lattice(ix, iy, iz: Integer; fx, fy, fz: Single): Single;
      Function Lerp(t, x0, x1: Single): Single;
      Function Smooth(x: Single): Single;

    Public
      Constructor Create;

      Function Noise(X,Y,Z:Single):Single; Override;
    End;

  CellNoiseGenerator = Class(NoiseGenerator)
    Protected
      _Points:Array Of Array Of Array Of Vector3D;

    Public
      Constructor Create;
      Function Noise(X,Y,Z:Single):Single; Override;
    End;

Implementation
Uses TERRA_Color, TERRA_OS, TERRA_Math;

{ NoiseGenerator }
Procedure NoiseGenerator.Release;
Begin
  // do nothing
End;

(*
Var
  W1, W2, W3, W4:Single;
  L:Single;
Begin
  W1 := (W - X) * (H - Y);
  W2 := (x) * (h - y);
  W3 := (x) * (y);
  W4 := (w - x) * (y);

  L := 1.0 / Sqrt(Sqr(W1)+ Sqr(W2)+ Sqr(W3)+ Sqr(W4));

  W1 := W1 * L;
  W2 := W2 * L;
  W3 := W3 * L;
  W4 := W4 * L;

  Result :=
    Noise(X, Y, Z) * W1 +
    Noise(x - w, y, Z) * W2 +
    Noise(x - w, y - h, Z) * W3 +
    Noise(x, y - h, Z) * W4;
*)

(*Function NoiseGenerator.TiledNoise(X, Y, Z, W, H: Single): Single;
Begin
  Result :=
    Noise(X, Y, Z) * (W - X) * (H - Y) +
    Noise(x - w, y, Z) * (x) * (h - y) +
    Noise(x - w, y - h, Z) * (x) * (y) +
    Noise(x, y - h, Z) * (w - x) * (y);
  Result := Result / (w * h);
End;*)

Procedure NoiseGenerator.SaveToImage(Target:Image; Layer:Single; ColorMask:Cardinal);
Const
  BorderSize = 0.25;
  BorderLeft = BorderSize;
  BorderRight = 1.0 - BorderSize;

Var
  I,J,K:Integer;
  TX, TY:Integer;
  W,H:Integer;
  X,Y, R, F:Single;

  N:Byte;
  S1, S2, S3:Single;
  W1, W2, W3:Single;

  A, B :Single;

  P:Color;
Begin
  W := Pred(Target.Width);
  H := Pred(Target.Height);
  //W := Target.Width Div Trunc(Frequency);
  //H := Target.Height Div Trunc(Frequency);


  For J:=0 To H Do
    For I:= 0 To W do
    Begin
      R := 0;

      TX := (I + (Target.Width Shr 1)) Mod Target.Width;
      TY := (J + (Target.Height Shr 1)) Mod Target.Height;

//      TX := I;
//      TY := J;

      X :=  Abs(0.5 - (I / W)) * 2.0;
      Y :=  Abs(0.5 - (J / H)) * 2.0;

      If Y<X Then
        X := Y;

      If X>0.25 Then
        X := 1.0
      Else
        X := (X / 0.25) * 0.5 + 0.5;

      R := Self.Noise((TX/W), (TY/H), Layer);

      (*If R>1.0 Then
        R := 1.0
      Else
      If (R<0) Then
        R := 0;*)

      N := Trunc(R * 255);

      P := Target.GetPixel(I, J);

      If (ColorMask And maskRed) <> 0 Then
        P.R := N;

      If (ColorMask And maskGreen) <> 0 Then
        P.G := N;

      If (ColorMask And maskBlue) <> 0 Then
        P.B := N;

      If (ColorMask And maskAlpha) <> 0 Then
        P.A := N;

      //P := ColorCreateFromFloat(X, X, X);

      // Write the result to the texture image.
      Target.SetPixel(I, J, P);
    End;
    //Result.Save('noise.png');
End;

{ PerlinNoiseGenerator }
Procedure PerlinNoiseGenerator.InitGradients;
Var
  I:Integer;
  Z,R,Theta: Single;
Begin
  // Generate random gradient vectors.
  For I:=0 to Pred(GradientTableSize) Do
  Begin
    z := 1 - 2*Random;
    r := sqrt(1 - z*z);
    theta := 2*PI*Random;
    _Gradients[i*3] := r*cos(theta);
    _Gradients[i*3 + 1] := r*sin(theta);
    _Gradients[i*3 + 2] := z;
  End;
End;

Const
  { Borrowed from Darwyn Peachey (see references above).
    The gradient table is indexed with an XYZ triplet, which is first turned
    into a single random index using a lookup in this table. The table simply
    contains all numbers in [0..255] in random order. }
  PERM: array [0..Pred(GradientTableSize)] of Byte = (
      225,155,210,108,175,199,221,144,203,116, 70,213, 69,158, 33,252,
        5, 82,173,133,222,139,174, 27,  9, 71, 90,246, 75,130, 91,191,
      169,138,  2,151,194,235, 81,  7, 25,113,228,159,205,253,134,142,
      248, 65,224,217, 22,121,229, 63, 89,103, 96,104,156, 17,201,129,
       36,  8,165,110,237,117,231, 56,132,211,152, 20,181,111,239,218,
      170,163, 51,172,157, 47, 80,212,176,250, 87, 49, 99,242,136,189,
      162,115, 44, 43,124, 94,150, 16,141,247, 32, 10,198,223,255, 72,
       53,131, 84, 57,220,197, 58, 50,208, 11,241, 28,  3,192, 62,202,
       18,215,153, 24, 76, 41, 15,179, 39, 46, 55,  6,128,167, 23,188,
      106, 34,187,140,164, 73,112,182,244,195,227, 13, 35, 77,196,185,
       26,200,226,119, 31,123,168,125,249, 68,183,230,177,135,160,180,
       12,  1,243,148,102,166, 38,238,251, 37,240,126, 64, 74,161, 40,
      184,149,171,178,101, 66, 29, 59,146, 61,254,107, 42, 86,154,  4,
      236,232,120, 21,233,209, 45, 98,193,114, 78, 19,206, 14,118,127,
       48, 79,147, 85, 30,207,219, 54, 88,234,190,122, 95, 67,143,109,
      137,214,145, 93, 92,100,245,  0,216,186, 60, 83,105, 97,204, 52
    );

Constructor PerlinNoiseGenerator.Create;
Begin
  // Initialize the random gradients before we start.
  InitGradients;
End;

Function PerlinNoiseGenerator.Permutate(x:Integer):Integer;
Const
  MASK = Pred(GradientTableSize);
Begin
  // Do a lookup in the permutation table.
  Result := PERM[x and MASK];
End;

Function PerlinNoiseGenerator.Index(ix, iy, iz: Integer): Integer;
Begin
  // Turn an XYZ triplet into a single gradient table index.
  Result := Permutate(ix + Permutate(iy + Permutate(iz)));
End;

Function PerlinNoiseGenerator.Lattice(ix, iy, iz: Integer; fx, fy, fz: Single): Single; {$IFDEF FPC} Inline; {$ENDIF}
Var
  g:Integer;
Begin
  // Look up a random gradient at [ix,iy,iz] and dot it with the [fx,fy,fz] vector.
  g := Index(ix, iy, iz)*3;
  Result := _Gradients[g]*fx + _Gradients[g+1]*fy + _Gradients[g+2]*fz;
End;

Function PerlinNoiseGenerator.Lerp(t, x0, x1: Single): Single; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  // Simple linear interpolation.
  Result := x0 + t*(x1-x0);
End;

Function PerlinNoiseGenerator.Smooth(x: Single): Single; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  { Smoothing curve. This is used to calculate interpolants so that the noise
    doesn't look blocky when the frequency is low. }
  Result := x*x*(3 - 2*x);
End;

Function PerlinNoiseGenerator.Noise(x, y, z: Single): Single;
Const
  PerlinSize = 32;
  RX = PerlinSize;
  RY = PerlinSize;
  RZ = PerlinSize;

Var
  ix, iy, iz: Integer;
  fx0, fx1, fy0, fy1, fz0, fz1: Single;
  wx, wy, wz: Single;
  vx0, vx1, vy0, vy1, vz0, vz1: Single;
Begin
  X := FloatMod(X * RX, RX);
  Y := FloatMod(Y * RY, RY);
  Z := FloatMod(Z * RZ, RZ);

  { The main noise function. Looks up the pseudorandom gradients at the nearest
    lattice points, dots them with the input vector, and interpolates the
    results to produce a single output value in [0, 1] range. }

  ix := Floor(x);
  fx0 := x - ix;
  fx1 := fx0 - 1;
  wx := Smooth(fx0);
  iy := Floor(y);
  fy0 := y - iy;
  fy1 := fy0 - 1;
  wy := Smooth(fy0);

  iz := Floor(z);
  fz0 := z - iz;
  fz1 := fz0 - 1;
  wz := Smooth(fz0);

  vx0 := Lattice(ix, iy, iz, fx0, fy0, fz0);
  vx1 := Lattice((ix+1) Mod RX, iy, iz, fx1, fy0, fz0);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, (iy+1) Mod RX, iz, fx0, fy1, fz0);
  vx1 := Lattice((ix+1) Mod RX, (iy+1) Mod RY, iz, fx1, fy1, fz0);
  vy1 := Lerp(wx, vx0, vx1);

  vz0 := Lerp(wy, vy0, vy1);

  vx0 := Lattice(ix, iy, (iz+1) Mod RZ, fx0, fy0, fz1);
  vx1 := Lattice((ix+1) Mod RX, iy, (iz+1) Mod RZ, fx1, fy0, fz1);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, (iy+1) Mod RY, (iz+1) Mod RZ, fx0, fy1, fz1);
  vx1 := Lattice((ix+1) Mod RX, (iy+1) Mod RY, (iz+1) Mod RZ, fx1, fy1, fz1);
  vy1 := Lerp(wx, vx0, vx1);

  vz1 := Lerp(wy, vy0, vy1);

  Result := Lerp(wz, vz0, vz1) + 0.5;

  If Result>1.0 Then
    Result := 1.0
  Else
  If Result<0.0 Then
    Result := 0.0;
End;

Constructor CellNoiseGenerator.Create;
Var
  I,J,K:Integer;
Begin
  RandSeed := Application.GetTime;
  SetLength(_Points, CellGridSize, CellGridSize, CellGridSize);
  For K:=0 To Pred(CellGridSize) Do
    For J:=0 To Pred(CellGridSize) Do
      For I:=0 To Pred(CellGridSize) Do
      Begin
        _Points[I,J,K] := VectorCreate(RandomFloat, RandomFloat, RandomFloat);
      End;
End;

Function CellNoiseGenerator.Noise(X,Y,Z:Single{; RX,RY,RZ:Integer}):Single;

Var
  TX,TY,TZ:Integer;
  CX,CY,CZ:Integer;
  P:Vector3D;
  I,J,K:Integer;
  Dist,R:Single;
  N:Integer;
Begin
  N := CellGridSize;

  X := X * N;
  Y := Y * N;
  Z := Z * N;

  TX := Trunc(X);
  TY := Trunc(Y);
  TZ := Trunc(Z);

  Dist := 9999;

  For K:=-1 To 1 Do
    For J:=-1 To 1 Do
      For I:=-1 To 1 Do
      Begin
        CX := (Word(TX+I) Mod N);
        CY := (Word(TY+J) Mod N);
        CZ := (Word(TZ+K) Mod N);

        P := _Points[CX,CY,CZ];

        CX := (TX+I);
        CY := (TY+J);
        CZ := (TZ+K);

        P.Add(VectorCreate(CX, CY, CZ));
        R := P.Distance(VectorCreate(X,Y,Z));
        If (R<Dist) Then
          Dist := R;
      End;

  If Dist>1.0 Then
    Dist := 1.0;
      
  Result := Dist;
End;


End.

