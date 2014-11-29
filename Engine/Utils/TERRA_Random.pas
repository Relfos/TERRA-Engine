{
  Generates pseudo-random numbers using the Mersenne Twister algorithm.
  See http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html and
  http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html for details on the algorithm.
}

Unit TERRA_Random;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_IO;

Const
  { Period parameters }
  _N = 624;
  _M = 397;

Type
  MersenneTwister = Class(TERRAObject)
    Protected
      _RandSeed:Cardinal;
      mt:array[0..Pred(_N)] of Int64; { the array for the state vector  }
      mti: Cardinal; { mti==N+1 means mt[N] is not initialized }

    Public
      Constructor Create();
      Destructor Destroy; Override;

      Procedure SetSeed(Seed:LongWord);
      Function Random(Max:Cardinal):LongWord;
      Function RandomFloat(Const min,max:Single):Single;

      Procedure SaveState(Dest:Stream);
      Procedure LoadState(Source:Stream);

      Function GetCRC():Cardinal;

      //Function RandomFloat(Max:Cardinal):Double;

      Property Seed:Cardinal Read _RandSeed;
  End;

Implementation
Uses TERRA_OS, TERRA_Math, TERRA_CRC32, TERRA_Log;

Const
  MATRIX_A   = $9908b0df;   { constant vector a }
  UPPER_MASK = $80000000; { most significant w-r bits }
  LOWER_MASK = $7fffffff; { least significant r bits }
{ Tempering parameters }
  TEMPERING_MASK_B = $9d2c5680;
  TEMPERING_MASK_C = $efc60000;

Constructor MersenneTwister.Create();
Begin
  mti := _N+1;
  SetSeed(GetTime());
End;

Destructor MersenneTwister.Destroy; 
Begin
End;

{ initializing the array with a NONZERO seed }
Procedure MersenneTwister.SetSeed(seed:Cardinal);
Begin
  _RandSeed := Seed;
  
  mt[0] := seed And $ffffffff;
  mti := 1;
  while(mti<_N)do
  begin
    mt[mti] := ((69069 * mt[mti-1]) and $ffffffff);
    Inc(mti);
  end;
  //DebugLb.Caption := 'seed: '+IntTostring(Seed);
End;

{
Function MersenneTwister.RandomFloat: Double;
const
  mag01: array[0..1] of LongWord =($0, MATRIX_A);
var
  y: LongWord;
  kk: LongInt;
begin
  if (mti >= N) then
  begin
    if (mti = (N+1))then
      SetRandSeedMT(4357);
    kk := 0;
    while(kk<(N-M))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+M] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    while(kk<(n-1))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+(M-N)] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    y := (mt[N-1]and UPPER_MASK)or(mt[0]and LOWER_MASK);
    mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];
    mti := 0;
  end;
  y := mt[mti];
  Inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7) and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y * 2.3283064370807974e-10;
end;}

Function MersenneTwister.Random(Max:LongWord): LongWord;
const
  mag01: array[0..1] of LongWord =($0, MATRIX_A);
var
  y: LongWord;
  kk: LongInt;
begin
  if (mti >= _N) then
  begin
    if (mti = (_N+1))then
      SetSeed(4357);
    kk := 0;
    while(kk<(_N-_M))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+_M] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    while(kk<(_n-1))do
    begin
      y := (mt[kk]and UPPER_MASK)or(mt[kk+1]and LOWER_MASK);
      mt[kk] := mt[kk+(_M-_N)] xor (y shr 1) xor mag01[y and $1];
      Inc(kk);
    end;
    y := (mt[_N-1]and UPPER_MASK)or(mt[0]and LOWER_MASK);
    mt[_N-1] := mt[_M-1] xor (y shr 1) xor mag01[y and $1];
    mti := 0;
  end;
  y := mt[mti];
  Inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7) and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y Mod Max;

  //DebugLb.Caption := DebugLb.Caption + '\nRnd('+IntToSTring(Max)+')='+IntToSTring(REsult);
end;

Function MersenneTwister.RandomFloat(Const min,max:Single):Single;
Begin
	Result := Min + ((max - min) * (Self.Random(RAND_MAX) * INV_RAND_MAX));
End;

Procedure MersenneTwister.SaveState(Dest:Stream);
Begin
  Dest.Write(@_RandSeed, 4);
  Dest.Write(@mt[0], _N * Sizeof(Int64));
  Dest.Write(@mti, 4);
End;

Procedure MersenneTwister.LoadState(Source:Stream);
Begin
  Source.Read(@_RandSeed, 4);
  Source.Read(@mt[0], _N * Sizeof(Int64));
  Source.Read(@mti, 4);
End;

Function MersenneTwister.GetCRC():Cardinal;
Begin
  Result := mti + GetCRC32(@mt[0], _N * Sizeof(Int64));
End;

Begin
  Log(logDebug, 'Random', 'Initializing random generator');
End.