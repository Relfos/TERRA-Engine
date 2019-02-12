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
 * TERRA_MurmurHash
 * Implements the MurmurHash2 string hashing algoritm
 ***********************************************************************************************************************
}

Unit TERRA_MurmurHash;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String;

Type
  HashKey = Cardinal;

Function Murmur2(Const Key:TERRAString):HashKey;

Implementation

{$OVERFLOWCHECKS OFF}

Const
//The default seed, $9747b28c, is from the original C library
  MurmurSeed: Cardinal =$9747b28c;

//https://code.google.com/p/smhasher/source/browse/trunk/MurmurHash2.cpp
//http://stackoverflow.com/questions/3690608/simple-string-hashing-function/3690631#3690631
Function Murmur2(Const Key:TERRAString):HashKey;
Const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
Var
  h:Cardinal;
  len:Cardinal;
  k, N:Cardinal;
  Data:PByte;
Begin
  Len := Length(Key);
  If Len<=0 Then
  Begin
    Result := 0;
    Exit;
  End;

  // Initialize the hash to a 'random' value
  h := MurmurSeed Xor len;

  // Mix 4 bytes at a time into the hash
  Data := @Key[1];

  While (Len >= 4) Do
  Begin
    k := 0;

    Inc(K, Ord(UpCase(AnsiChar(Data^))));
    Inc(Data);

    Inc(K, Ord(UpCase(AnsiChar(Data^))) Shl 8);
    Inc(Data);

    Inc(K, Ord(UpCase(AnsiChar(Data^))) Shl 16);
    Inc(Data);

    Inc(K, Ord(UpCase(AnsiChar(Data^))) Shl 24);
    Inc(Data);

    k := k*m;
    k := k xor (k shr r);
    k := k* m;

    h := h*m;
    h := h xor k;

    Dec(Len, 4);
  End;

  {   Handle the last few bytes of the input array
    S: ... $69 $18 $2f
  }

  If len >= 3 Then
  Begin
    Inc(Data, 2);
    K := Ord(UpCase(AnsiChar(Data^))) Shl 16;
    h := h Xor K;
  End Else
  If len = 2 Then
  Begin
    Inc(Data);
    K := Ord(UpCase(AnsiChar(Data^))) Shl 8;
    h := h Xor K;
  End Else
  If len = 1 Then
  Begin
    K := Ord(UpCase(AnsiChar(Data^)));
    h := h Xor K;
    h := h * m;
  End;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);
  h := h * m;
  h := h xor (h shr 15);

  Result := h;
End;

End.
