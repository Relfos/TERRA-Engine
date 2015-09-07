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
 * TERRA_String
 * Portable unicode String routines
 ***********************************************************************************************************************
}

//http://stackoverflow.com/questions/199260/how-do-i-reverse-a-utf-8-string-in-place

{$IFDEF OXYGENE}
namespace TERRA;

{$ELSE}
Unit TERRA_String;
{$I terra.inc}
{$ENDIF}

Interface

{$IFDEF VER150}
{$IFDEF WINDOWS}
{$IFDEF FULLDEBUG}
{$DEFINE USE_MEMCHECK}
{$ENDIF}
{$ENDIF}
{$ENDIF}

Uses
{$IFNDEF OXYGENE}
{$IFDEF USE_MEMCHECK}
MemCheck,
{$ELSE}
TERRA_MemoryManager,
{$ENDIF}
{$ENDIF}
  TERRA_Object;

Const
  NullChar = #0;
  NewLineChar = #13;

Type
  StringEncoding = {$IFDEF OXYGENE}Public Enum{$ENDIF}  (encodingUnknown, encodingASCII,  encodingUCS2LE, encodingUCS2BE, encodingUTF8);

  (*StringIteratorEntry = Record
    Value:TERRAChar;
    StartPos:Cardinal;
    EndPos:Cardinal;
  End;*)

  StringIterator = Class(TERRAObject)
    Protected
      _Chars:Array Of TERRAChar; //StringIteratorEntry;
      _Length:Integer; // length (in chars)

      _Prev:Integer;
      _Position:Integer; // current index

      _Reverse:Boolean;
      _AutoCase:Boolean;

      _SplitLength:Integer;

      Procedure SetPosition(Const Value:Integer);

    Public
      Class Function CanBePooled:Boolean; Override;

      Procedure Reset(Const S:TERRAString);

      Function HasNext():Boolean;
      Function GetNext():TERRAChar;
      Function PeekNext():TERRAChar;

      Procedure Restart();

      Procedure Advance();
      Procedure Rewind();

      Function Seek(Pos:Integer):Boolean; // jumps to character at specifc pos, can be negative

      Procedure Split(Out A, B:TERRAString);

      Property Length:Integer Read _Length;
      Property Position:Integer Read _Prev Write SetPosition;
      Property Reverse:Boolean Read _Reverse;
  End;

Const
  CurrentStringEncoding = encodingUTF8;

// string iterator functions
Function StringCreateIterator(Const S:TERRAString; AutoCase:Boolean = False):StringIterator;
Function StringCreateReverseIterator(Const S:TERRAString; AutoCase:Boolean = False):StringIterator;

Function StringToWideString(Const Str:TERRAString):WideString;

Function StringGetChar(Const S:TERRAString; Index:Integer):TERRAChar;

Function StringEquals(Const A,B:TERRAString; IgnoreCase:Boolean = True):Boolean;

{ Compares two strings alphabetically }
Function StringCompare(Const A,B:TERRAString; IgnoreCase:Boolean = False):Integer;

//result is number between 0 (zero) and 1 (one), where 0 means not similar at all and 1 means equal strings.
Function StringSimilarityRatio(const Str1, Str2:TERRAString; IgnoreCase: Boolean):Single;

Function StringContains(Const SubStr, Str:TERRAString):Boolean;
Function StringContainsChar(Const C:TERRAChar; Const Str:TERRAString):Boolean;

Function StringBeginsWith(Const SubStr, Str:TERRAString; IgnoreCase: Boolean):Boolean;
Function StringBeginsWithChar(Const C:TERRAChar; Const Str:TERRAString; IgnoreCase: Boolean):Boolean;

Function StringFirstChar(Const Str:TERRAString):TERRAChar;
Function StringLastChar(Const Str:TERRAString):TERRAChar;

Function StringLength(Const Str:TERRAString):Integer;

Function StringTrim(Const S:TERRAString):TERRAString;
Function StringTrimLeft(Const S:TERRAString):TERRAString;
Function StringTrimRight(Const S:TERRAString):TERRAString;

Function StringUpper(Const S:TERRAString):TERRAString;
Function StringLower(Const S:TERRAString):TERRAString;
Function StringCapitals(Const S:TERRAString):TERRAString;

Function StringFromChar(Const Value:TERRAChar):TERRAString;

Function StringCharCount(Const S:TERRAString; Const C:TERRAChar):Integer;

Function StringReverse(Const S:TERRAString):TERRAString;

Function StringIsUnicode(Const S:TERRAString):Boolean;

Procedure StringAppendChar(Var Str:TERRAString; Const C:TERRAChar);
Procedure StringPrependChar(Var Str:TERRAString; Const C:TERRAChar);

// removes N characters from begiinning or end (if count is negative)
Procedure StringDropChars(Var Str:TERRAString; Count:Integer);

Function BytesToChar(A, B:Byte):TERRAChar;
Function CharToByte(Const Value:TERRAChar):Byte;
Procedure CharToBytes(Const Value:TERRAChar; Out A,B:Byte);
Function CharValue(Const Value:TERRAChar):Cardinal;

Function CharUpper(Const C:TERRAChar):TERRAChar;
Function CharLower(Const C:TERRAChar):TERRAChar;

Function CharIsPunctuation(Const C:TERRAChar):Boolean;
Function CharIsAlphaNumeric(Const C:TERRAChar):Boolean;

//Returns the position of a substring inside of a string
Function StringPos(Const Substr, S:TERRAString; IgnoreCase:Boolean = False):Integer;
//Same as StringPos, but search starts from the end of the string.
Function StringPosReverse(Const Substr, S:TERRAString; IgnoreCase:Boolean = False):Integer;

Function StringPosIterator(Const Substr, S:TERRAString; IgnoreCase:Boolean = False):StringIterator;
Function StringPosReverseIterator(Const Substr, S:TERRAString; IgnoreCase:Boolean = False):StringIterator;

//Returns the position of a character inside of a string
Function StringCharPos(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):Integer;
Function StringCharPosReverse(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):Integer;

Function StringCharPosIterator(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):StringIterator;
Function StringCharPosReverseIterator(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):StringIterator;

//Returns the position of the first character from a list of characters
Function StringCharListPos(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean = False):Integer;
Function StringCharListPosIterator(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean = False):StringIterator;

// index can be negative, meaning copying Count chars from end of string
Function StringCopy(Const S:TERRAString; Index, Count:Integer):TERRAString;

Function StringSplit(Const S:TERRAString; Out A, B:TERRAString; Const C:TERRAString; IgnoreCase:Boolean = False):Boolean;
Function StringSplitByChar(Const S:TERRAString; Out A, B:TERRAString; C:TERRAChar; IgnoreCase:Boolean = False):Boolean;

Function StringGetNextSplit(Var S:TERRAString; Separator:TERRAChar):TERRAString;

Procedure StringReplaceText(Const Token, Value:TERRAString; Var S:TERRAString);
Procedure StringReplaceChar(Const Token, Value:TERRAChar; Var S:TERRAString);

Function StringPadLeft(Const S:TERRAString; ExpectedLength:Integer; Token:TERRAChar):TERRAString;
Function StringPadRight(Const S:TERRAString; ExpectedLength:Integer; Token:TERRAChar):TERRAString;

Function StringEncodeHTML(Const S:TERRAString):TERRAString;
Procedure StringRemoveSpecialHTMLChars(Var S:TERRAString);

Function StringFill(Length:Integer; C:TERRAChar):TERRAString;

// test if pattern Expression is found in string S
// example pattern: *.png
Function StringMatchRegEx(Const S, Expression:TERRAString; IgnoreCase:Boolean = True):Boolean;


//Function StringExplode(Const S, Token:TERRAString):StringArray;

Implementation

{$IFNDEF OXYGENE}
Uses TERRA_Error, TERRA_Engine;
{$ENDIF}

Function StringMatchRegEx(Const S, Expression:TERRAString; IgnoreCase:Boolean):Boolean;
Var
  It, SubIt:StringIterator;
  A, B:TERRAChar;

  Procedure CleanUp();
  Begin
    ReleaseObject(It);
    ReleaseObject(SubIt);
  End;
Begin
  Result := False;

  It := StringCreateIterator(S,  IgnoreCase);
  SubIt := StringCreateIterator(Expression, IgnoreCase);

  While (It.HasNext()) And (SubIt.HasNext()) Do
  Begin
    A := It.GetNext();
    B := SubIt.GetNext();

    If (B = '*') Then
    Begin
      While (B = '*') Do
      Begin
        If (Not SubIt.HasNext()) Then
        Begin
          Result := True;
          CleanUp();
          Exit;
        End;

        B := SubIt.GetNext();
      End;

      While (A <> B) Do
      Begin
        If (Not It.HasNext()) Then
        Begin
          CleanUp();
          Exit;
        End;

        A := It.GetNext();
      End;

    End Else
    If (A <> B) Then
    Begin
      CleanUp();
      Exit;
    End;
  End;

  CleanUp();
  Result := True;
End;

Function Min(const A, B, C: Integer): Integer;
Begin
  Result := A;
  If B < Result then
    Result := B;
  If C < Result then
    Result := C;
End;

Function DamerauLevenshteinDistance(const Str1, Str2:TERRAString; IgnoreCase:Boolean): Integer;
Var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2:StringIterator;
  pStr1R, pStr2R:StringIterator;
  D:Array Of Integer;
  Temp:Integer;

  A,B, PrevA, PrevB:TERRAChar;

  Procedure CleanUp();
  Begin
    ReleaseObject(PStr1);
    ReleaseObject(PStr2);
    ReleaseObject(PStr1R);
    ReleaseObject(PStr2R);
  End;
Begin
  LenStr1 := StringLength(Str1);
  LenStr2 := StringLength(Str2);

  // save a bit memory by making the second index points to the shorter string
  If LenStr1 < LenStr2 then
  Begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;

    pStr1 := StringCreateIterator(Str2, IgnoreCase);
    pStr2 := StringCreateIterator(Str1, IgnoreCase);

    pStr1R := StringCreateReverseIterator(Str2, IgnoreCase);
    pStr2R := StringCreateReverseIterator(Str1, IgnoreCase);
  End Else
  Begin
    pStr1 := StringCreateIterator(Str1, IgnoreCase);
    pStr2 := StringCreateIterator(Str2, IgnoreCase);

    pStr1R := StringCreateReverseIterator(Str1, IgnoreCase);
    pStr2R := StringCreateReverseIterator(Str2, IgnoreCase);
  End;

  // bypass leading identical characters
  While (LenStr2 <> 0) Do
  begin
    If (pStr1.PeekNext() <> pStr2.PeekNext()) Then
      Break;

    pStr1.GetNext();
    pStr2.GetNext();
    Dec(LenStr1);
    Dec(LenStr2);
  End;

  // bypass trailing identical characters
  While (LenStr2 <> 0) Do
  Begin
    If (pStr1R.PeekNext() <> pStr2R.PeekNext()) Then
      Break;

    pStr1R.GetNext();
    pStr2R.GetNext();

    Dec(LenStr1);
    Dec(LenStr2);
  End;

  // is the shorter string empty? so, the edit distance is length of the longer one
  If LenStr2 = 0 then
  Begin
    Result := LenStr1;
    CleanUp();
    Exit;
  End;

  // calculate the edit distance
  SetLength(D, (LenStr2 + 1));

  For I := 0 to LenStr2 do
    D[I] := I;

  //S1 := pStr1;
  Temp := pStr2.Position;

  PrevA := NullChar;
  PrevB := NullChar;

  For I := 1 to LenStr1 Do
  begin
    PrevCost := I - 1;
    Cost := I;
    pStr2.Position := Temp;

    A := pStr1.GetNext();

    For J := 1 to LenStr2 do
    Begin
      B := pStr2.GetNext();

      If (A = B) or ((I > 1) and (J > 1) and (A = PrevB) and (B = PrevA)) then
        Cost := PrevCost
      Else
        Cost := 1 + Min(Cost, PrevCost, D[J]);

      PrevCost := D[J];
      D[J] := Cost;

      PrevB := B;
    End;

    PrevA := A;
  End;

  Result := D[LenStr2];

  CleanUp();
End;

Function StringSimilarityRatio(const Str1, Str2:TERRAString; IgnoreCase: Boolean):Single;
Var
  MaxLen: Integer;
  Distance: Integer;
Begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);

  If MaxLen <> 0 then
  begin
    Distance := DamerauLevenshteinDistance(Str1, Str2, IgnoreCase);
    Result := Result - (Distance / MaxLen);
  end;
End;


Function StringToWideString(Const Str:TERRAString):WideString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  It := StringCreateIterator(Str);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    Result := Result + WideChar(C);
  End;
  ReleaseObject(It);
End;

Function StringContains(Const SubStr, Str:TERRAString):Boolean;
Begin
  Result := StringPos(SubStr, Str, True)>0;
End;

Function StringContainsChar(Const C:TERRAChar; Const Str:TERRAString):Boolean;
Begin
  Result := StringCharPos(C, Str, True)>0;
End;

Function StringBeginsWith(Const SubStr, Str:TERRAString; IgnoreCase:Boolean):Boolean;
Begin
  Result := StringPos(SubStr, Str, IgnoreCase) = 1;
End;

Function StringBeginsWithChar(Const C:TERRAChar; Const Str:TERRAString; IgnoreCase: Boolean):Boolean;
Var
  B:TERRAChar;
Begin
  B := StringFirstChar(Str);
  If (IgnoreCase) Then
    Result := CharUpper(C) = CharUpper(B)
  Else
    Result := (C = B);
End;

Function BytesToChar(A, B:Byte):TERRAChar;
Begin
  Result := TERRAChar(A + B Shl 8);
End;

Function CharToByte(Const Value:TERRAChar):Byte;
Begin
  Result := Byte(Value);
End;

Procedure CharToBytes(Const Value:TERRAChar; Out A,B:Byte);
Begin
  A := (Word(Value) Shr 8) And $FF;
  B := (Word(Value)) And $FF;
End;

Function CharValue(Const Value:TERRAChar):Cardinal;
Begin
  Result := Cardinal(Value);
End;

Function CharUpper(Const C:TERRAChar):TERRAChar;
Begin
  If (C>='a') And (C<='z') Then
    Result := TERRAChar(CharValue(C) - 32)
  Else
  If (C>=#1072) And (C<=#1103) Then // cyrillic
    Result := TERRAChar(CharValue(C) - 32)
  Else
  If (C>=#1104) And (C<=#1119) Then // cyrillic2
    Result := TERRAChar(CharValue(C) - 80)
  Else
    Result := C;
End;

Function CharLower(Const C:TERRAChar):TERRAChar;
Begin
  If (C>='A') And (C<='Z') Then
    Result := TERRAChar(CharValue(C) + 32)
  Else
  If (C>=#1040) And (C<=#1071) Then // cyrillic
    Result := TERRAChar(CharValue(C) + 32)
  Else
  If (C>=#1024) And (C<=#1039) Then // cyrillic2
    Result := TERRAChar(CharValue(C) + 80)
  Else
    Result := C;
End;

Function CharIsPunctuation(Const C:TERRAChar):Boolean;
Var
  N:Cardinal;
Begin
  N := CharValue(C);

  Result := (N=33) Or (N=44) Or (N=46) Or (N=63);
End;

Function CharIsAlphaNumeric(Const C:TERRAChar):Boolean;
Var
  N:Cardinal;
Begin
  N := CharValue(C);

  Result := ((N>=48) And (N<=57)) Or ((N>=65) And (N<=90)) Or ((N>=97) And (N<=122)) Or (N>=127) Or (CharIsPunctuation(C));
End;

Function StringGetChar(Const S:TERRAString; Index:Integer):TERRAChar;
Var
  Target:Integer;
  It:StringIterator;
Begin
  It := StringCreateIterator(S);

  If (Index<0) Then
    Target := It._Length + Succ(Index)
  Else
    Target := Index;

  If (Target>0) And (Target<= It._Length) Then
    Result := It._Chars[Target]
  Else
    Result := NullChar;

  ReleaseObject(It);
End;

Function StringFromChar(Const Value:TERRAChar):TERRAString;
Var
  N:Cardinal;
  A, B, C:Byte;
Begin
  N := CharValue(Value);

  If N<$80 Then
  Begin
    Result := TERRAString('' + Value);
    Exit;
  End;

  If N<=$7FF Then
  Begin
    A := $C0 Or (N Shr 6);
    B := $80 Or (N and $3F);
    Result := '' + Char(A) + Char(B);
    Exit;
  End;

  A := $E0 Or (N Shr 12);
  B := $80 Or ((N Shr 6) And $3F);
  C := $80 Or (N And $3F);
  Result := '' + Char(A) + Char(B) + Char(C);
End;

Procedure StringAppendChar(Var Str:TERRAString; Const C:TERRAChar);
Begin
  Str := Str + StringFromChar(C);
End;

Procedure StringPrependChar(Var Str:TERRAString; Const C:TERRAChar);
Begin
  Str := StringFromChar(C) + Str;
End;

Function StringPosIteratorSearch(Var It, SubIt:StringIterator; IgnoreCase:Boolean):Boolean;
Var
  A,B:TERRAChar;
  Temp:Integer;
  Found:Boolean;

  Procedure DoMatch();
  Begin
    If (SubIt.Position = 1) Then
    Begin
      Temp := It.Position;
    End;

    If Not SubIt.HasNext Then
    Begin
      It.Position := Temp;
      Found := True;
    End;
  End;
Begin
  Found := False;
  Temp := 1;

  While (It.HasNext) And (Not Found) Do
  Begin
    A := It.GetNext();
    B := SubIt.GetNext();

    If A = B Then
    Begin
      DoMatch();
    End Else
    Begin
      SubIt.Restart();
      B := SubIt.GetNext();

      If A = B Then
        DoMatch()
      Else
        SubIt.Restart();
    End;
  End;

  Result := Found;
End;

Function StringPosIterator(Const Substr, S:TERRAString; IgnoreCase:Boolean):StringIterator;
Var
  SubIt:StringIterator;
Begin
  Result := StringCreateIterator(S, IgnoreCase);
  SubIt := StringCreateIterator(SubStr, IgnoreCase);

  Result._SplitLength := SubIt.Length;
  If Not StringPosIteratorSearch(Result, SubIt, IgnoreCase) Then
    ReleaseObject(Result);
End;

Function StringPos(Const Substr, S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  It := StringPosIterator(SubStr, S, IgnoreCase);
  If Assigned(It) Then
  Begin
    Result := It.Position;
    ReleaseObject(It);
  End Else
    Result := 0;
End;

Function StringPosReverseIterator(Const Substr, S:TERRAString; IgnoreCase:Boolean):StringIterator;
Var
  SubIt:StringIterator;
Begin
  Result := StringCreateReverseIterator(S);
  SubIt := StringCreateReverseIterator(SubStr);

  Result._SplitLength := SubIt.Length;
  If Not StringPosIteratorSearch(Result, SubIt, IgnoreCase) Then
    ReleaseObject(Result);
End;

Function StringPosReverse(Const Substr, S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  It := StringPosReverseIterator(SubStr, S, IgnoreCase);
  If Assigned(It) Then
  Begin
    Result := It.Position;
    ReleaseObject(It);
  End Else
    Result := 0;
End;

Function StringCharListPosIterator(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean):StringIterator;
Var
  A,B:TERRAChar;
  SubIt:StringIterator;

  Procedure CleanUp();
  Begin
    ReleaseObject(Result);
    ReleaseObject(SubIt);
  End;
Begin
  Separator := NullChar;

  Result := StringCreateIterator(S, IgnoreCase);
  SubIt := StringCreateIterator(CharList, IgnoreCase);

  While (Result.HasNext) Do
  Begin
    A := Result.GetNext();

    SubIt.Restart();
    While SubIt.HasNext() Do
    Begin
      B := SubIt.GetNext();

      If (A = B) Then
      Begin
        Separator := B;
        ReleaseObjecT(SubIt);
        Exit;
      End;
    End;
  End;

  ReleaseObjecT(SubIt);
  ReleaseObject(Result);
End;

Function StringCharListPos(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  It := StringCharListPosIterator(CharList, S, Separator, IgnoreCase);
  If Assigned(It) Then
  Begin
    Result := It.Position;
    ReleaseObject(It);
  End Else
    Result := 0;
End;

Function StringCharPosIterator(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):StringIterator;
Var
  B:TERRAChar;
Begin
  If IgnoreCase Then
    C := CharUpper(C);

  Result := StringCreateIterator(S, IgnoreCase);
  While Result.HasNext Do
  Begin
    B := Result.GetNext();

    If B = C Then
      Exit;
  End;

  ReleaseObject(Result);
End;

Function StringCharPos(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  It := StringCharPosIterator(C, S, IgnoreCase);
  If Assigned(It) Then
  Begin
    Result := It.Position;
    ReleaseObject(It);
  End Else
    Result := 0;
End;

Function StringCharPosReverseIterator(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):StringIterator;
Var
  B:TERRAChar;
Begin
  Result := StringCreateReverseIterator(S, IgnoreCase);

  While Result.HasNext Do
  Begin
    B := Result.GetNext();

    If B = C Then
      Exit;
  End;

  ReleaseObject(Result);
End;

Function StringCharPosReverse(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  It := StringCharPosReverseIterator(C, S, IgnoreCase);
  If Assigned(It) Then
  Begin
    Result := It.Position;
    ReleaseObject(It);
  End Else
    Result := 0;
End;

{OPTIMIZE ME!}
Function StringCopy(Const S:TERRAString; Index, Count:Integer):TERRAString;
Var
  It:StringIterator;
  Temp:TERRAString;
  C:TERRAChar;
  I:Integer;
Begin
  If (Count = 0) Then
  Begin
    Result := '';
    Exit;
  End;

  It := StringCreateIterator(S);

  If (Index < 0) Then
  Begin
    Index := It.Length + Succ(Index);
  End;

  If (Count < 0) Then
  Begin
    Count := It.Length + Count;
  End;

  Result := '';
  If It.Seek(Index) Then
  Begin
    While (Count>0) And (It.HasNext()) Do
    Begin
      C := It.GetNext();
      StringAppendChar(Result, C);
      Dec(Count);
    End;
  End;

  ReleaseObject(It);
End;

Function StringSplit(Const S:TERRAString; Out A, B:TERRAString; Const C:TERRAString; IgnoreCase:Boolean):Boolean;
Var
  It:StringIterator;
Begin
  A := '';
  B := '';

  If S = '' Then
  Begin
    Result := False;
    Exit;
  End;

  It := StringPosIterator(C, S, IgnoreCase);
  Result := Assigned(It);
  If (Result) Then
  Begin
    It.Split(A, B);
    ReleaseObject(It);
  End;
End;


Function StringSplitByChar(Const S:TERRAString; Out A, B:TERRAString; C:TERRAChar; IgnoreCase:Boolean):Boolean;
Var
  It:StringIterator;
Begin
  A := '';
  B := '';

  If S = '' Then
  Begin
    Result := False;
    Exit;
  End;

  It := StringCharPosIterator(C, S, IgnoreCase);
  Result := Assigned(It);
  If (Result) Then
  Begin
    It.Split(A, B);
    ReleaseObject(It);
  End;
End;

Function StringLength(Const Str:TERRAString):Integer;
Var
  It:StringIterator;
Begin
  Result := 0;
  If Str = '' Then
    Exit;

  It := StringCreateIterator(Str);
  Result := It._Length;
  ReleaseObject(It);
End;

Procedure StringDropChars(Var Str:TERRAString; Count:Integer);
Var
  It:StringIterator;
  N, Min, Max, Len:Integer;
  C:TERRAChar;
  S:TERRAString;
Begin
  If Str = '' Then
    Exit;

  Len := StringLength(Str);

  If Count<0 Then
  Begin
    Min := 1;
    Max := Len + Count;
  End Else
  Begin
    Min := Succ(Count);
    Max := Len;
  End;

  N := 0;
  S := '';
  It := StringCreateIterator(Str);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    Inc(N);

    If (N>=Min) And (N<=Max) Then
      StringAppendChar(S, C);
  End;
  ReleaseObject(It);

  Str := S; 
End;

Function StringFirstChar(Const Str:TERRAString):TERRAChar;
Var
  It:StringIterator;
Begin
  It := StringCreateIterator(Str);
  If (It._Length>0) Then
    Result := It._Chars[1]
  Else
    Result := NullChar;

  ReleaseObject(It);
End;

Function StringLastChar(Const Str:TERRAString):TERRAChar;
Var
  It:StringIterator;
Begin
  It := StringCreateIterator(Str);
  If (It._Length>0) Then
    Result := It._Chars[It._Length]
  Else
    Result := NullChar;

  ReleaseObject(It);
End;

Function StringTrim(Const S:TERRAString):TERRAString;
Begin
  Result := StringTrimRight(StringTrimLeft(S));
End;

Function StringTrimLeft(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
  Temp:TERRAString;
Begin
  Result := '';
  It := StringCreateIterator(S);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C>#32) Then
    Begin
      It.Split(Temp, Result);
      StringPrependChar(Result, C);
      Break;
    End;
  End;

  ReleaseObject(It);
End;

Function StringTrimRight(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
  Temp:TERRAString;
Begin
  Result := '';
  It := StringCreateReverseIterator(S);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C>#32) Then
    Begin
      It.Split(Result, Temp);
      StringAppendChar(Result, C);
      Break;
    End;
  End;
  ReleaseObject(It);
End;

Function StringGetNextSplit(Var S:TERRAString; Separator:TERRAChar):TERRAString;
Var
  It:StringIterator;
Begin
  Result := '';

  If S = '' Then
    Exit;

  S := StringTrimLeft(S);
  If S = '' Then
    Exit;

  It := StringCharPosIterator(Separator, S);
  If Assigned(It) Then
  Begin
    It.Split(Result, S);
    ReleaseObject(It);

    S := StringTrimLeft(S);
  End Else
  Begin
    Result := S;
    S := '';
  End;
End;


//Converts a string to upcase
Function StringUpper(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  It := StringCreateIterator(S);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    C := CharUpper(C);
    StringAppendChar(Result, C);
  End;
  ReleaseObject(It);
End;

//Converts a string to lowercase
Function StringLower(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  It := StringCreateIterator(S);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    C := CharLower(C);
    StringAppendChar(Result, C);
  End;
  ReleaseObject(It);
End;

Function StringCapitals(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  It := StringCreateIterator(S);
  While It.HasNext Do
  Begin
    C := It.GetNext();

    If (It.Position = 1) Then
      C := CharUpper(C);
    StringAppendChar(Result, C);
  End;
  ReleaseObject(It);
End;

Function StringPadLeft(Const S:TERRAString; ExpectedLength:Integer; Token:TERRAChar):TERRAString;
Var
  Len, N:Integer;
Begin
  Len := StringLength(S);
  Result := S;

  N := ExpectedLength - Len;

  While N>0 Do
  Begin
    StringPrependChar(Result, Token);
    Dec(N);
  End;
End;

Function StringPadRight(Const S:TERRAString; ExpectedLength:Integer; Token:TERRAChar):TERRAString;
Var
  Len, N:Integer;
Begin
  Len := StringLength(S);
  Result := S;

  N := ExpectedLength - Len;

  While N>0 Do
  Begin
    StringAppendChar(Result, Token);
    Dec(N);
  End;
End;

Procedure StringReplaceText(Const Token, Value:TERRAString; Var S:TERRAString);
Var
  I, Len:Integer;
  S2:TERRAString;
Begin
  If (Token = Value) Or (S = '') Then
    Exit;

  I := StringPos(Token, S, True);
  If (I>0) Then
  Begin
    Len := StringLength(Token);
    S2 := StringCopy(S, I+ Len, MaxInt);
    S := StringCopy(S,1,(I-1));
    StringReplaceText(Token, Value, S2); // TODO optimize
    S := S + Value + S2;
  End;
End;

Procedure StringReplaceChar(Const Token, Value:TERRAChar; Var S:TERRAString);
Var
  It:StringIterator;
  A, B:TERRAString;
Begin
  If (Token = Value) Then
    Exit;

  It := StringCharPosIterator(Token, S, True);
  If (Assigned(It)) Then
  Begin
    It.Split(A, B);
    ReleaseObject(It);

    StringReplaceChar(Token, Value, B);

    S := A + StringFromChar(Value) + B;
  End;
End;

//https://www.google.pt/?gws_rd=ssl#q=pascal+html+encode+string
Function StringEncodeHTML(Const S:TERRAString):TERRAString;
Const
    hex : array[0..255] of string = (
     '%00', '%01', '%02', '%03', '%04', '%05', '%06', '%07',
     '%08', '%09', '%0a', '%0b', '%0c', '%0d', '%0e', '%0f',
     '%10', '%11', '%12', '%13', '%14', '%15', '%16', '%17',
     '%18', '%19', '%1a', '%1b', '%1c', '%1d', '%1e', '%1f',
     '%20', '%21', '%22', '%23', '%24', '%25', '%26', '%27',
     '%28', '%29', '%2a', '%2b', '%2c', '%2d', '%2e', '%2f',
     '%30', '%31', '%32', '%33', '%34', '%35', '%36', '%37',
     '%38', '%39', '%3a', '%3b', '%3c', '%3d', '%3e', '%3f',
     '%40', '%41', '%42', '%43', '%44', '%45', '%46', '%47',
     '%48', '%49', '%4a', '%4b', '%4c', '%4d', '%4e', '%4f',
     '%50', '%51', '%52', '%53', '%54', '%55', '%56', '%57',
     '%58', '%59', '%5a', '%5b', '%5c', '%5d', '%5e', '%5f',
     '%60', '%61', '%62', '%63', '%64', '%65', '%66', '%67',
     '%68', '%69', '%6a', '%6b', '%6c', '%6d', '%6e', '%6f',
     '%70', '%71', '%72', '%73', '%74', '%75', '%76', '%77',
     '%78', '%79', '%7a', '%7b', '%7c', '%7d', '%7e', '%7f',
     '%80', '%81', '%82', '%83', '%84', '%85', '%86', '%87',
     '%88', '%89', '%8a', '%8b', '%8c', '%8d', '%8e', '%8f',
     '%90', '%91', '%92', '%93', '%94', '%95', '%96', '%97',
     '%98', '%99', '%9a', '%9b', '%9c', '%9d', '%9e', '%9f',
     '%a0', '%a1', '%a2', '%a3', '%a4', '%a5', '%a6', '%a7',
     '%a8', '%a9', '%aa', '%ab', '%ac', '%ad', '%ae', '%af',
     '%b0', '%b1', '%b2', '%b3', '%b4', '%b5', '%b6', '%b7',
     '%b8', '%b9', '%ba', '%bb', '%bc', '%bd', '%be', '%bf',
     '%c0', '%c1', '%c2', '%c3', '%c4', '%c5', '%c6', '%c7',
     '%c8', '%c9', '%ca', '%cb', '%cc', '%cd', '%ce', '%cf',
     '%d0', '%d1', '%d2', '%d3', '%d4', '%d5', '%d6', '%d7',
     '%d8', '%d9', '%da', '%db', '%dc', '%dd', '%de', '%df',
     '%e0', '%e1', '%e2', '%e3', '%e4', '%e5', '%e6', '%e7',
     '%e8', '%e9', '%ea', '%eb', '%ec', '%ed', '%ee', '%ef',
     '%f0', '%f1', '%f2', '%f3', '%f4', '%f5', '%f6', '%f7',
     '%f8', '%f9', '%fa', '%fb', '%fc', '%fd', '%fe', '%ff');
Var
  It:StringIterator;
  C:TERRAChar;
  IsSafe:Boolean;
Begin
  Result := '';
  It := StringCreateIterator(S);
  While It.HasNext() Do
  Begin
    C := It.GetNext();

    If (C>='a') And (C<='z') Then
      IsSafe := True
    Else
    If (C>='A') And (C<='Z') Then
      IsSafe := True
    Else
    If (C>='0') And (C<='9') Then
      IsSafe := True
    Else
    If (C = '-') Or (C = '.') Or (C = '!') Or (C = '*') Or
    (C = '~') Or (C = '\') Or (C = '(') Or (C = ')') Or (C = '@') Then
      IsSafe := True
    Else
      IsSafe := False;

    If (IsSafe) Then
    Begin
      StringAppendChar(Result, C);
    End Else
    If (C = ' ') Then
    Begin
      Result := Result + '+';
    End Else
    If (C <= #$07F) Then
      Result := Result + Hex[CharValue(C)]
    Else
    If (C <= #$7FF) Then
    Begin
      Result := Result + Hex[$C0 Or (CharValue(C) Shr 6)] + Hex[$80 Or (CharValue(C) And $3F)];
    End Else
    Begin
      Result := Result + Hex[$E0 Or (CharValue(C) shr 12)];
      Result := Result + Hex[$80 Or ((CharValue(C) shr 6) And $3F)];
      Result := Result + Hex[$80 Or (CharValue(C) And $3F)];
    End;
  End;
  ReleaseObject(It);
End;

Procedure StringRemoveSpecialHTMLChars(Var S:TERRAString);
Begin
  StringReplaceText('&amp;', '&', S);
  StringReplaceText('&quot;', '"', S);
  StringReplaceText('&lt;', '<', S);
  StringReplaceText('&gt;', '>', S);
End;


Function StringReverse(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
Begin
  Result := '';
  It := StringCreateReverseIterator(S);
  While It.HasNext() Do
  Begin
    StringAppendChar(Result, It.GetNext());
  End;
  ReleaseObject(It);
End;

Function StringCharCount(Const S:TERRAString; Const C:TERRAChar):Integer;
Var
  It:StringIterator;
Begin
  Result := 0;
  It := StringCreateIterator(S);
  While It.HasNext() Do
  Begin
    If It.GetNext() = C Then
      Inc(Result);
  End;
  ReleaseObject(It);
End;

{Function StringExplode(Const S, Token:TERRAString):StringArray;
Var
  I:Integer;
Begin
  Result := Nil;
  I := StringCharPos(, FileName);
  While I>=1 Do
  Begin
    FileName := StringCopy(FileName, I+1, MaxInt);
    I := StringCharPosReverse(CharValue(PathSeparator), FileName);
  End;
End;}

Function StringIsUnicode(Const S:TERRAString):Boolean;
Var
  It:StringIterator;
Begin
  Result := False;
  
  It := StringCreateIterator(S);
  While It.HasNext() Do
  Begin
    If It.GetNext() > #127 Then
    Begin
      Result := True;
      Break;
    End;
  End;
  ReleaseObject(It);
End;

Function StringCreateIterator(Const S:TERRAString; AutoCase:Boolean):StringIterator;
Begin
  Result := StringIterator(Engine.Pool.Fetch(StringIterator));
  If Result = Nil Then
    Result := StringIterator.Create();

  Result._Reverse := False;
  Result._AutoCase := AutoCase;
  Result._SplitLength := 1;
  Result.Reset(S);
End;

Function StringCreateReverseIterator(Const S:TERRAString; AutoCase:Boolean):StringIterator;
Begin
  Result := StringIterator(Engine.Pool.Fetch(StringIterator));
  If Result = Nil Then
    Result := StringIterator.Create();

  Result._Reverse := True;
  Result._AutoCase := AutoCase;
  Result._SplitLength := 1;
  Result.Reset(S);
End;

{ StringIterator }
Procedure StringIterator.Reset(Const S:TERRAString);
Var
  A,B,C:Byte;
  Prev, I, N, RawSize:Cardinal;

  Function NextRawByte():Byte;
  Begin
    If (I>RawSize) Then
      Result := 0
    Else
    Begin
      Result := Byte(S[I]);
      Inc(I);
    End;
  End;

  Procedure AddChar(CC:TERRAChar);
  Begin
    Inc(_Length);

    If (System.Length(_Chars) < Succ(_Length)) Then
      SetLength(_Chars, Succ(_Length));
    _Chars[_Length] := CC;
    (*_Chars[Pred(_Length)].StartPos := Prev;
    _Chars[Pred(_Length)].EndPos := I; *)
  End;

Begin
  RawSize := System.Length(S);
  _Length := 0;

  I := 1;
  Prev := 0;
  Repeat
    If (I > RawSize) Then
      Break;

    Prev := I;

    A := NextRawByte();
    If (A<$80) Then
    Begin
      AddChar(TERRAChar(A));
      Continue;
    End Else
    If ((A And $E0) = $E0) Then
    Begin
      B := NextRawByte();
      C := NextRawByte();
      If (B = 0) Or (C = 0) Then
        Break;


      N := ((A And $0F) Shl 12) Or ((B And $3F) Shl 6) Or (C And $3F);
      AddChar(TERRAChar(N));
    End Else
    If ((A And $C0) = $C0) Then
    Begin
      B := NextRawByte();
      If (B = 0) Then
        Break;

      N := ((A And $1F) Shl 6) Or (B And $3F);
      AddChar(TERRAChar(N));
    End Else
      Break;

  Until False;

  Self.Restart();
End;

Function StringIterator.PeekNext: TERRAChar;
Var
  Temp:Integer;
Begin
  Temp := Self.Position;
  Result := Self.GetNext();
  Self.Position := Temp;
End;

Function StringFill(Length:Integer; C:TERRAChar):TERRAString;
Var
  I:Integer;
Begin
  {$IFDEF OXYGENE}
  Result := TERRAString.create(C, length);
  {$else}
  If (C<#255) Then
  Begin
    SetLength(Result, Length);
    For I:=1 To Length Do
      Result[I] := AnsiChar(C);
  End Else
  Begin
    Result := '';
    For I:=1 To Length Do
      StringAppendChar(Result, C);
  End;
  {$ENDIF}
End;

Function StringCompare(Const A,B:TERRAString; IgnoreCase:Boolean = False):Integer;
Var
  IA, IB:StringIterator;
  CA,CB:TERRAChar;

  Procedure CleanUp();
  Begin
    ReleaseObject(IA);
    ReleaseObject(IB);
  End;
Begin
  IA := StringCreateIterator(A, IgnoreCase);
  IB := StringCreateIterator(B, IgnoreCase);

  While (IA.HasNext) Or (IB.HasNext) Do
  Begin
    If (IA.HasNext) Then
      CA := IA.GetNext()
    Else
      CA := NullChar;

    If (IB.HasNext) Then
      CB := IB.GetNext()
    Else
      CB := NullChar;

    If (CA<CB) Then
    Begin
      Result := -1;
      CleanUp();
      Exit;
    End Else
    If (CA>CB) Then
    Begin
      Result := 1;
      CleanUp();
      Exit;
    End;
  End;

  Result := 0;
  CleanUp();
End;

Function StringEquals(Const A,B:TERRAString; IgnoreCase:Boolean):Boolean;
Var
  ItA, ItB:StringIterator;
  CA, CB:TERRAChar;

  Procedure CleanUp();
  Begin
    ReleaseObject(ItA);
    ReleaseObject(ItB);
  End;
Begin
  ItA := StringCreateIterator(A, IgnoreCase);
  ItB := StringCreateIterator(B, IgnoreCase);

  If (ItA.Length <> ItB.Length) Then
  Begin
    Result := False;
    CleanUp();
    Exit;
  End;

  While (ItA.HasNext()) Do
  Begin
    CA := ItA.GetNext();

    If (ItB.HasNext()) Then
    Begin
      CB := ItB.GetNext();
    End Else
    Begin
      Result := False;
      CleanUp();
      Exit;
    End;

    If (CA <> CB) Then
    Begin
      Result := False;
      CleanUp();
      Exit;
    End;
  End;

  Result := Not ItB.HasNext();
  CleanUp()
End;

{OPTIMIZE ME!}
Procedure StringIterator.Split(Out A, B: TERRAString);
Var
  I, Count:Integer;
Begin
  A := '';
  For I:=1 To Pred(Self.Position) Do
    StringAppendChar(A, _Chars[I]);

  B := '';
  For I := Self.Position + Self._SplitLength To Self.Length Do
    StringAppendChar(B, _Chars[I]);
End;

Function StringIterator.HasNext: Boolean;
Begin
  If (_Reverse) Then
    Result := (_Position>=1)
  Else
    Result := (_Position <= _Length);
End;

Function StringIterator.Seek(Pos: Integer):Boolean;
Begin
  Result := True;

  If (Pos<=0) Then
  Begin
    Result := False;
    Pos := 1;
  End Else
  If (Pos > _Length) Then
  Begin
    Result := False;
    Pos := _Length;
  End;

  _Position := Pos;
  _Prev := Pos;
End;

Procedure StringIterator.SetPosition(Const Value:Integer);
Begin
  Seek(Value);
End;

Procedure StringIterator.Advance();
Begin
  _Prev := _Position;
  If _Reverse Then
    Dec(_Position)
  Else
    Inc(_Position);
End;

Procedure StringIterator.Rewind();
Begin
  _Prev := _Position;
  If _Reverse Then
    Inc(_Position)
  Else
    Dec(_Position);
End;


Function StringIterator.GetNext():TERRAChar;
Begin
  If (_Position>0) And (_Position<=_Length) Then
  Begin
    Result := _Chars[_Position];

    If _AutoCase Then
      Result := CharUpper(Result);

    Self.Advance();
  End Else
    Result := NullChar;
End;

Procedure StringIterator.Restart();
Begin
  If (_Reverse) Then
    _Position := _Length
  Else
    _Position := 1;

  _Prev := _Position;
End;

Class Function StringIterator.CanBePooled: Boolean;
Begin
  Result := True;
End;

End.
