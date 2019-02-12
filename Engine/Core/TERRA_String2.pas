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

{$IFDEF USE_MEMCHECK}
Uses MemCheck;
{$ELSE}
{$IFNDEF DEBUG_LEAKS}
Uses TERRA_MemoryManager;
{$ENDIF}
{$ENDIF}

Const
  NullChar = 0;
  NewLineChar = 13;

Type
  TERRAChar = Cardinal;

  StringEncoding = (encodingUnknown, encodingASCII,  encodingUCS2LE, encodingUCS2BE, encodingUTF8);

Const
  CurrentStringEncoding = encodingUTF8;

Type
  StringIteratorState = Record
    Position:Integer; // logical position (note: in reverse iterators, this is the position counting from end)
    Index:Integer;   // real position (in raw bytes)
    PrevIndex:Integer;
  End;

  TERRAString = Class;

  StringIterator = Object
    Protected
      _Target:TERRAString;
      _State:StringIteratorState;
      _Size:Integer; // length (in bytes)
      _Remainder:Integer;

      _Reverse:Boolean;
      _AutoCase:Boolean;

      Function NextRawByte():Byte;

    Public
      Function HasNext():Boolean;
      Function GetNext():TERRAChar;
      Function PeekNext():TERRAChar;

      Procedure Reset();

      Procedure SaveState(Out State:StringIteratorState);
      Procedure RestoreState(Const State:StringIteratorState);

      Function Seek(Pos:Integer):Boolean; // jumps to character at specifc pos, can be negative

      Procedure Split(Out A, B:TERRAString);

      Property Position:Integer Read _State.Position;
      Property Reverse:Boolean Read _Reverse;
  End;

  TERRAString = Class
    Protected
      _Chars:Array Of Byte;
      _Length:Integer;

    Public
      Constructor Create(Const Value:String); Overload;
      Constructor Create(Const C:TERRAChar); Overload;
      Constructor Create(Const Value:TERRAString); Overload;
      Constructor Create(Length:Integer; Const C:TERRAChar); Overload;


      // string iterator functions
      Procedure CreateIterator(Const S:TERRAString; Out It:StringIterator; AutoCase:Boolean = False);
      Procedure CreateReverseIterator(Const S:TERRAString; Out It:StringIterator; AutoCase:Boolean = False);

      Function AsWideString():WideString;

      Function GetChar(Index:Integer):TERRAChar;

      Function Equals(Const Other:TERRAString; IgnoreCase:Boolean = True):Boolean;

      // Compares two strings alphabetically
      Function Compare(Const A,B:TERRAString; IgnoreCase:Boolean = False):Integer;

      Function Contains(Const SubStr:TERRAString):Boolean;
      Function ContainsChar(Const C:TERRAChar):Boolean;

      Function BeginsWith(Const SubStr:TERRAString; IgnoreCase: Boolean):Boolean;
      Function BeginsWithChar(Const C:TERRAChar; IgnoreCase: Boolean):Boolean;

      Function FirstChar():TERRAChar;
      Function LastChar():TERRAChar;

      Procedure Trim();
      Procedure TrimLeft();
      Procedure TrimRight();

      Procedure ToUpper();
      Procedure ToLower();

      Procedure Capitalize();

      Function CharCount(Const C:TERRAChar):Integer;

      Procedure Reverse();

      Function IsUnicode():Boolean;

      Procedure AppendChar(Const C:TERRAChar);
      Procedure PrependChar(Const C:TERRAChar);

      // removes N characters from begiinning or end (if count is negative)
      Procedure DropChars(Count:Integer);

      //Returns the position of a substring inside of a string
      Function StringPos(Const Substr:TERRAString; IgnoreCase:Boolean = False):Integer;
      //Same as StringPos, but search starts from the end of the string.
      Function StringPosReverse(Const Substr, S:TERRAString; IgnoreCase:Boolean = False):Integer;

      Function StringPosIterator(Const Substr, S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean = False):Boolean;
      Function StringPosReverseIterator(Const Substr, S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean = False):Boolean;

      //Returns the position of a character inside of a string
      Function CharPos(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):Integer;
      Function CharPosReverse(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean = False):Integer;

      Function CharPosIterator(C:TERRAChar; Const S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean = False):Boolean;
      Function CharPosReverseIterator(C:TERRAChar; Const S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean = False):Boolean;

      //Returns the position of the first character from a list of characters
      Function CharListPos(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean = False):Integer;
      Function CharListPosIterator(Const CharList, S:TERRAString; Out It:StringIterator; Out Separator:TERRAChar; IgnoreCase:Boolean = False):Boolean;

      // index can be negative, meaning copying Count chars from end of string
      Function Copy(Index, Count:Integer):TERRAString;

      Function Split(Const S:TERRAString; Out A, B:TERRAString; Const C:TERRAString; IgnoreCase:Boolean = False):Boolean;
      Function SplitByChar(Const S:TERRAString; Out A, B:TERRAString; C:TERRAChar; IgnoreCase:Boolean = False):Boolean;

      Function GetNextSplit(Separator:TERRAChar):TERRAString;
      Function ExtractNextWord(Out Separator:TERRAChar):TERRAString;

      Procedure ReplaceText(Const Token, Value:TERRAString; Var S:TERRAString);
      Procedure ReplaceChar(Const Token, Value:TERRAChar; Var S:TERRAString);

      Procedure PadLeft(ExpectedLength:Integer; Token:TERRAChar);
      Procedure PadRight(ExpectedLength:Integer; Token:TERRAChar);

      Procedure EncodeHTML();
      Procedure RemoveSpecialHTMLChars();

      //result is number between 0 (zero) and 1 (one), where 0 means not similar at all and 1 means equal strings.
      Function SimilarityRatio(const Str1, Str2:TERRAString; IgnoreCase: Boolean):Single;

      // test if pattern Expression is found in string S
      // example pattern: *.png
      Function MatchRegEx(Const Expression:TERRAString; IgnoreCase:Boolean = True):Boolean;

      Property Length:Integer Read _Length;
    End;

Function BytesToChar(A, B:Byte):TERRAChar;
Function CharToByte(Const Value:TERRAChar):Byte;
Procedure CharToBytes(Const Value:TERRAChar; Out A,B:Byte);

Function CharUpper(Const C:TERRAChar):TERRAChar;
Function CharLower(Const C:TERRAChar):TERRAChar;


Implementation
Uses TERRA_Error;

Function StringMatchRegEx(Const S, Expression:TERRAString; IgnoreCase:Boolean):Boolean;
Var
  It, SubIt:StringIterator;
  A, B:TERRAChar;
Begin
  Result := False;

  StringCreateIterator(S, It, IgnoreCase);
  StringCreateIterator(Expression, SubIt, IgnoreCase);

  While (It.HasNext()) And (SubIt.HasNext()) Do
  Begin
    A := It.GetNext();
    B := SubIt.GetNext();

    If (B = Ord('*')) Then
    Begin
      While (B = Ord('*')) Do
      Begin
        If (Not SubIt.HasNext()) Then
        Begin
          Result := True;
          Exit;
        End;

        B := SubIt.GetNext();
      End;

      While (A <> B) Do
      Begin
        If (Not It.HasNext()) Then
          Exit;

        A := It.GetNext();
      End;

    End Else
    If (A <> B) Then
      Exit;
  End;

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
  Temp:StringIteratorState;

  A,B, PrevA, PrevB:TERRAChar;
Begin
  LenStr1 := StringLength(Str1);
  LenStr2 := StringLength(Str2);

  // save a bit memory by making the second index points to the shorter string
  If LenStr1 < LenStr2 then
  Begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;

    StringCreateIterator(Str2, pStr1, IgnoreCase);
    StringCreateIterator(Str1, pStr2, IgnoreCase);

    StringCreateReverseIterator(Str2, pStr1R, IgnoreCase);
    StringCreateReverseIterator(Str1, pStr2R, IgnoreCase);
  End Else
  Begin
    StringCreateIterator(Str1, pStr1, IgnoreCase);
    StringCreateIterator(Str2, pStr2, IgnoreCase);

    StringCreateReverseIterator(Str1, pStr1R, IgnoreCase);
    StringCreateReverseIterator(Str2, pStr2R, IgnoreCase);
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
    Exit;
  End;

  // calculate the edit distance
  SetLength(D, (LenStr2 + 1));

  For I := 0 to LenStr2 do
    D[I] := I;

  //S1 := pStr1;
  pStr2.SaveState(Temp);

  PrevA := NullChar;
  PrevB := NullChar;

  For I := 1 to LenStr1 Do
  begin
    PrevCost := I - 1;
    Cost := I;
    pStr2.RestoreState(Temp);

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
  StringCreateIterator(Str, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    Result := Result + WideChar(C);
  End;
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

Function CharUpper(Const C:TERRAChar):TERRAChar;
Begin
  If (C>=Ord('a')) And (C<=Ord('z')) Then
    Result := C - 32
  Else
  If (C>=1072) And (C<=1103) Then // cyrillic
    Result := C - 32
  Else
  If (C>=1104) And (C<=1119) Then // cyrillic2
    Result := C - 80
  Else
    Result := C;
End;

Function CharLower(Const C:TERRAChar):TERRAChar;
Begin
  If (C>=Ord('A')) And (C<=Ord('Z')) Then
    Result := C + 32
  Else
  If (C>=1040) And (C<=1071) Then // cyrillic
    Result := C + 32
  Else
  If (C>=1024) And (C<=1039) Then // cyrillic2
    Result := C + 80
  Else
    Result := C;
End;

Function StringGetChar(Const S:TERRAString; Index:Integer):TERRAChar;
Var
  N, Target:Integer;
  It:StringIterator;
Begin
  If (Index<0) Then
    Target := StringLength(S) + Succ(Index)
  Else
    Target := Index;

  N := 0;

  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    Result := It.GetNext();
    Inc(N);

    //WriteLn(N,' ', Chr(Result));
    If N = Target Then
      Exit;
  End;

  Result := NullChar;
End;

Function StringFromChar(Const C:TERRAChar):TERRAString;
Begin
  If C<$80 Then
  Begin
    Result := Char(Byte(C));
    Exit;
  End;

  If C<=$7FF Then
  Begin
    Result := Char($C0 Or (C Shr 6)) + Char($80 Or (C and $3F));
    Exit;
  End;

  Result := Char($E0 Or (C Shr 12)) + Char($80 Or ((C Shr 6) And $3F)) + Char($80 Or (C And $3F));
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
  Temp:StringIteratorState;

  Procedure DoMatch();
  Begin
    If (SubIt.Position = 1) Then
    Begin
      It.SaveState(Temp);
    End;

    If Not SubIt.HasNext Then
    Begin
      It.RestoreState(Temp);

      It._Remainder := Pred(StringLength(SubIt._Target));
      Result := True;
    End;
  End;
Begin
  Result := False;

  While (It.HasNext) And (Not Result) Do
  Begin
    A := It.GetNext();
    B := SubIt.GetNext();

    If A = B Then
    Begin
      DoMatch();
    End Else
    Begin
      SubIt.Reset();
      B := SubIt.GetNext();

      If A = B Then
        DoMatch()
      Else
        SubIt.Reset();
    End;
  End;
End;

Function StringPosIterator(Const Substr, S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean):Boolean;
Var
  SubIt:StringIterator;
Begin
  StringCreateIterator(S, It, IgnoreCase);
  StringCreateIterator(SubStr, SubIt, IgnoreCase);

  Result := StringPosIteratorSearch(It, SubIt, IgnoreCase);
End;

Function StringPos(Const Substr, S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  If StringPosIterator(SubStr, S, It, IgnoreCase) Then
    Result := It.Position
  Else
    Result := 0;
End;

Function StringPosReverseIterator(Const Substr, S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean):Boolean;
Var
  SubIt:StringIterator;
Begin
  StringCreateReverseIterator(S, It);
  StringCreateReverseIterator(SubStr, SubIt);

  Result := StringPosIteratorSearch(It, SubIt, IgnoreCase);
End;

Function StringPosReverse(Const Substr, S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  If StringPosReverseIterator(SubStr, S, It, IgnoreCase) Then
    Result := It.Position
  Else
    Result := 0;
End;

Function StringCharListPosIterator(Const CharList, S:TERRAString; Out It:StringIterator; Out Separator:TERRAChar; IgnoreCase:Boolean):Boolean;
Var
  A,B:TERRAChar;
  SubIt:StringIterator;
Begin
  Result := False;
  Separator := NullChar;

  StringCreateIterator(S, It, IgnoreCase);
  StringCreateIterator(CharList, SubIt, IgnoreCase);

  While (It.HasNext) And (Not Result) Do
  Begin
    A := It.GetNext();

    SubIt.Reset();
    While SubIt.HasNext() Do
    Begin
      B := SubIt.GetNext();

      If (A = B) Then
      Begin
        Separator := B;
        Result := True;
        Exit;
      End;
    End;

  End;
End;

Function StringCharListPos(Const CharList, S:TERRAString; Out Separator:TERRAChar; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  If StringCharListPosIterator(CharList, S, It, Separator, IgnoreCase) Then
    Result := It.Position
  Else
    Result := 0;
End;

Function StringCharPosIterator(C:TERRAChar; Const S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean):Boolean;
Var
  B:TERRAChar;
Begin
  If IgnoreCase Then
    C := CharUpper(C);

  StringCreateIterator(S, It, IgnoreCase);
  While It.HasNext Do
  Begin
    B := It.GetNext();

    If B = C Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

Function StringCharPos(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  If StringCharPosIterator(C, S, It, IgnoreCase)  Then
    Result := It.Position
  Else
    Result := 0;
End;

Function StringCharPosReverseIterator(C:TERRAChar; Const S:TERRAString; Out It:StringIterator; IgnoreCase:Boolean):Boolean;
Var
  B:TERRAChar;
Begin
  StringCreateReverseIterator(S, It, IgnoreCase);
  While It.HasNext Do
  Begin
    B := It.GetNext();

    If B = C Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

Function StringCharPosReverse(C:TERRAChar; Const S:TERRAString; IgnoreCase:Boolean):Integer;
Var
  It:StringIterator;
Begin
  If StringCharPosReverseIterator(C, S, It, IgnoreCase)  Then
    Result := It.Position
  Else
    Result := 0;
End;

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

  If (Index < 0) Then
  Begin
    Index := StringLength(S) + Succ(Index);
  End;

  If (Count < 0) Then
  Begin
    Count := StringLength(S) + Count;
  End;

  StringCreateIterator(S, It);

  If Index>0 Then
    It.Seek(Index);

  It.Split(Temp, Result);

  If Count<MaxInt Then
  Begin
    StringCreateIterator(Result, It);
    While Count>0 Do
    Begin
      It.GetNext();
      Dec(Count);
    End;

    Result := System.Copy(Result, 0, Pred(IT._State.Index));
  End;
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

  Result := StringPosIterator(C, S, It, IgnoreCase);
  If (Result) Then
  Begin
    It.Split(A, B);
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

  Result := StringCharPosIterator(C, S, It, IgnoreCase);
  If (Result) Then
  Begin
    It.Split(A, B);
  End;
End;

Function StringLength(Const Str:TERRAString):Integer;
Var
  It:StringIterator;
Begin
  Result := 0;
  If Str = '' Then
    Exit;

  StringCreateIterator(Str, It);
  While It.HasNext Do
  Begin
    It.GetNext();
    Inc(Result);
  End;
End;

Procedure StringDropChars(Var Str:TERRAString; Count:Integer);
Var
  It:StringIterator;
  N, Min, Max, Len:Integer;
  C:TERRAChar;
  Result:TERRAString;
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
  Result := '';
  StringCreateIterator(Str, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    Inc(N);

    If (N>=Min) And (N<=Max) Then
      StringAppendChar(Result, C);
  End;

  Str := Result; 
End;

Function StringFirstChar(Const Str:TERRAString):TERRAChar;
Var
  It:StringIterator;
Begin
  StringCreateIterator(Str, It);
  While It.HasNext Do
  Begin
    Result := It.GetNext();
    Exit;
  End;

  Result := NullChar;
End;

Function StringLastChar(Const Str:TERRAString):TERRAChar;
Var
  It:StringIterator;
Begin
  Result := NullChar;

  StringCreateIterator(Str, It);
  While It.HasNext Do
  Begin
    Result := It.GetNext();
  End;
End;

Function StringTrim(Const S:TERRAString):TERRAString;
Begin
  If (StringFirstChar(S) = NullChar) Then
    Result := ''
  Else
    Result := StringTrimRight(StringTrimLeft(S));
End;

Function StringTrimLeft(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
  Temp:TERRAString;
Begin
  Result := '';
  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C>32) Then
    Begin
      It.Split(Temp, Result);
      StringPrependChar(Result, C);
      Exit;
    End;
  End;
End;

Function StringTrimRight(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
  Temp:TERRAString;
Begin
  If (StringCharPosIterator(NullChar, S, It)) Then
  Begin
    It.Split(Result, Temp);

    Result := StringTrimRight(Result);
    Exit;
  End;

  Result := '';
  StringCreateReverseIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C>32) Then
    Begin
      It.Split(Temp, Result);
      StringPrependChar(Result, C);

      Result := StringReverse(Result);
      Exit;
    End;
  End;
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

  If StringCharPosIterator(Separator, S, It) Then
  Begin
    It.Split(Result, S);

    S := StringTrimLeft(S);
  End Else
  Begin
    Result := S;
    S := '';
  End;
End;

Const
  WordSeparators: TERRAString = ' ,:><!?'+Chr(NewLineChar);

Function StringExtractNextWord(Var S:TERRAString; Out Separator:TERRAChar):TERRAString;
Var
  It:StringIterator;
  Temp:TERRAString;
Begin
  Result := '';
  Separator := NullChar;

  If S = '' Then
    Exit;

  If StringCharListPosIterator(WordSeparators, S, It, Separator) Then
  Begin
    It.Split(Result, S);
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
  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    C := CharUpper(C);
    StringAppendChar(Result, C);
  End;
End;

//Converts a string to lowercase
Function StringLower(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    C := CharLower(C);
    StringAppendChar(Result, C);
  End;
End;

Function StringCapitals(Const S:TERRAString):TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := '';
  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();

    If (It.Position = 1) Then
      C := CharUpper(C);
    StringAppendChar(Result, C);
  End;
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
  If (Token = Value) Then
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

  If (StringCharPosIterator(Token, S, It, True)) Then
  Begin
    It.Split(A, B);

    StringReplaceChar(Token, Value, B);

    S := A + StringFromChar(Value) + B;
  End;
End;

Function StringEquals(Const A,B:TERRAString; IgnoreCase:Boolean):Boolean;
Var
  ItA, ItB:StringIterator;
  CA, CB:TERRAChar;
Begin
  StringCreateIterator(A, ItA, IgnoreCase);
  StringCreateIterator(B, ItB, IgnoreCase);

  If (ItA._Size <> ItB._Size) Then
  Begin
    Result := False;
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
      Exit;
    End;

    If (CA <> CB) Then
    Begin
      Result := False;
      Exit;
    End;
  End;

  Result := Not ItB.HasNext();
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
  C:Word;
  IsSafe:Boolean;
Begin
  Result := '';
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    C := It.GetNext();

    If (C>=Ord('a')) And (C<=Ord('z')) Then
      IsSafe := True
    Else
    If (C>=Ord('A')) And (C<=Ord('Z')) Then
      IsSafe := True
    Else
    If (C>=Ord('0')) And (C<=Ord('9')) Then
      IsSafe := True
    Else
    If (C = Ord('-')) Or (C = Ord('.')) Or (C = Ord('!')) Or (C = Ord('*')) Or
    (C = Ord('~')) Or (C = Ord('\')) Or (C = Ord('(')) Or (C = Ord(')')) Or (C = Ord('@')) Then
      IsSafe := True
    Else
      IsSafe := False;

    If (IsSafe) Then
    Begin
      Result := Result + Chr(C);
    End Else
    If (C = Ord(' ')) Then 
    Begin
      Result := Result + '+';
    End Else
    If (C <= $07F) Then
      Result := Result + Hex[C]
    Else
    If (C <= $7FF) Then
    Begin
      Result := Result + Hex[$C0 Or (C Shr 6)] + Hex[$80 Or (C And $3F)];
    End Else
    Begin
      Result := Result + Hex[$E0 Or (C shr 12)];
      Result := Result + Hex[$80 Or ((C shr 6) And $3F)];
      Result := Result + Hex[$80 Or (C And $3F)];
    End;
  End;
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
  StringCreateReverseIterator(S, It);
  While It.HasNext() Do
  Begin
    StringAppendChar(Result, It.GetNext());
  End;
End;

Function StringCharCount(Const S:TERRAString; Const C:TERRAChar):Integer;
Var
  It:StringIterator;
Begin
  Result := 0;
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    If It.GetNext() = C Then
      Inc(Result);
  End;
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
    I := StringCharPosReverse(Ord(PathSeparator), FileName);
  End;
End;}

Function StringIsUnicode(Const S:TERRAString):Boolean;
Var
  It:StringIterator;
Begin
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    If It.GetNext() > 127 Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

Procedure StringCreateIterator(Const S:TERRAString; Out It:StringIterator; AutoCase:Boolean);
Begin
  It._Target := S;
  It._Reverse := False;
  It._AutoCase := AutoCase;

  It.Reset();
End;

Procedure StringCreateReverseIterator(Const S:TERRAString; Out It:StringIterator; AutoCase:Boolean);
Var
  Temp:TERRAString;
Begin
  Temp := '';
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    StringPrependChar(Temp, It.GetNext());
  End;

  It._Target := Temp;
  It._Reverse := True;
  It._AutoCase := AutoCase;
  It.Reset();
End;

{ StringIterator }

Procedure StringIterator.RestoreState(Const State: StringIteratorState);
Begin
  _State := State;
End;

Procedure StringIterator.SaveState(Out State: StringIteratorState);
Begin
  State := _State;
End;

Procedure StringIterator.Reset;
Begin
  _Size := Length(_Target);

  If (_Reverse) Then
    _State.Position := Succ(_Size)
  Else
    _State.Position := 0;

  _State.Index := 1;
  _State.PrevIndex := 0;

  _Remainder := 0;
End;


Function StringIterator.GetNext: TERRAChar;
Var
  A,B,C:Byte;
Begin
  If (_State.Index > _Size) Then
  Begin
    Result := NullChar;
    Exit;
  End;

  _State.PrevIndex := _State.Index;

  If (_Reverse) Then
    Dec(_State.Position)
  Else
    Inc(_State.Position);

  A := NextRawByte();
  If (A<$80) Then
  Begin
    Result := A;
  End Else
  If ((A And $E0) = $E0) Then
  Begin
    B := NextRawByte();
    C := NextRawByte();
    If (B = 0) Or (C = 0) Then
    Begin
      Result := NullChar;
      Exit;
    End;

    Result := ((A And $0F) Shl 12) Or ((B And $3F) Shl 6) Or (C And $3F);
  End Else
  If ((A And $C0) = $C0) Then
  Begin
    B := NextRawByte();
    If (B = 0) Then
    Begin
      Result := NullChar;
      Exit;
    End;

    Result := ((A And $1F) Shl 6) Or (B And $3F);
  End Else
    Result := NullChar;

  If _AutoCase Then
    Result := CharUpper(Result);
End;


Function StringIterator.PeekNext: TERRAChar;
Var
  Temp:StringIteratorState;
Begin
  Self.SaveState(Temp);
  Result := Self.GetNext();
  Self.RestoreState(Temp);
End;

Function StringIterator.NextRawByte():Byte;
Begin
  If (_State.Index > _Size) Then
  Begin
    Result := 0;
    Exit;
  End;

  Result := Byte(_Target[_State.Index]);
  Inc(_State.Index);
End;

Function StringFill(Length:Integer; C:TERRAChar):TERRAString;
Var
  I:Integer;
Begin
  If (C<255) Then
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
End;

Function StringCompare(Const A,B:TERRAString; IgnoreCase:Boolean = False):Integer;
Var
  IA, IB:StringIterator;
  CA,CB:TERRAChar;
Begin
  StringCreateIterator(A, IA, IgnoreCase);
  StringCreateIterator(B, IB, IgnoreCase);

  While (IA.HasNext) Or (IB.HasNext) Do
  Begin
    If (IA.HasNext) Then
      CA := IA.GetNext()
    Else
      CA := 0;

    If (IB.HasNext) Then
      CB := IB.GetNext()
    Else
      CB := 0;

    If (CA<CB) Then
    Begin
      Result := -1;
      Exit;
    End Else
    If (CA>CB) Then
    Begin
      Result := 1;
      Exit;
    End;
  End;

  Result := 0;
End;

// note: using System.Copy here for speed, since we know the raw indices already!
Procedure StringIterator.Split(Out A, B: TERRAString);
Var
  Count:Integer;
Begin
  A := Copy(_Target, 1, Pred(_State.PrevIndex));

  Count := Self._Remainder;
  While (Count>0) And (Self.HasNext()) Do
  Begin
    Self.GetNext();
    Dec(Count);
  End;

  B := Copy(_Target, _State.Index, MaxInt); // TODO
End;

Function StringIterator.HasNext: Boolean;
Begin
  Result := _State.Index <= _Size;
End;

Function StringIterator.Seek(Pos: Integer):Boolean;
Begin
  If (_State.Position = Pos) Then
  Begin
    Result := True;
    Exit;
  End;

  Self.Reset();

  If (Pos<0) Then
  Begin
    Pos := StringLength(_Target) + Succ(Pos);
  End;

  Dec(Pos);
  While (Self.Position<Pos) And (HasNext()) Do
    Self.GetNext();

  Result := (_State.Position = Pos);
End;

End.
