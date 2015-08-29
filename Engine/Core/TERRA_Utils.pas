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
 * TERRA_Utils
 * Various unsorted util functions
 ***********************************************************************************************************************
}


{$IFDEF OXYGENE}
namespace TERRA;

{$ELSE}
Unit TERRA_Utils;
{$I terra.inc}
{$ENDIF}

Interface

{$IFNDEF OXYGENE}
Uses TERRA_String;
{$ENDIF}

Type
  TERRATime = {$IFDEF OXYGENE} public {$ENDIF} Record
    Hour:Word;
    Minute:Word;
    Second:Word;
    MiliSecond:Word;
  End;

  TERRADate = {$IFDEF OXYGENE}Public {$ENDIF} Record
    Year:Word;
    Month:Byte;
    Day:Byte;
    WeekDay:Byte;
  End;

  {$IFDEF OXYGENE}
  PByte = ^Byte;
  PWord = ^Word;
  PCardinal = ^Cardinal;  
{$ENDIF}

  TERRAVersion={$IFDEF OXYGENE}Public {$ENDIF} Record
           Major:Byte;
           Minor:Byte;
           Build:Word;
          End;

{$IFNDEF FPC}
Type
  PtrInt = Integer;
  PtrUInt = Cardinal;
{$ENDIF}

  PByteArray=^ByteArray;
  ByteArray=Array[0..1024*64] Of Byte;

  PShortIntArray=^ShortIntArray;
  ShortIntArray=Array[0..1024*64] Of ShortInt;

  PWordArray=^WordArray;
  WordArray=Array[0..1024*64] Of Word;

  PSmallIntArray=^SmallIntArray;
  SmallIntArray=Array[0..1024*64] Of SmallInt;

  PIntegerArray=^IntegerArray;
  IntegerArray=Array[0..1024*64] Of Integer;

  PCardinalArray=^CardinalArray;
  CardinalArray=Array[0..1024*64] Of Cardinal;

  PSingleArray=^SingleArray;
  SingleArray=Array[0..1024*64] Of Single;

  PPointerArray=^ PointerArray;
  PointerArray=Array[0..65534] Of Pointer;

  PBooleanArray=^BooleanArray;
  BooleanArray=Array[0..1024*64] Of Boolean;

Const
{$IFDEF OXYGENE}
  EngineVersion:TERRAVersion= new Version(3, 5, 0);
{$ELSE}
  EngineVersion:TERRAVersion=(Major:4;Minor:1;Build:10);
{$ENDIF}

Const
  iTrue='TRUE';
  iFalse='FALSE';

  Tab = #9;

Type
  // Triangle struct
  {$IFDEF OXYGENE}
  Triangle = Class
  {$ELSE}
  PTriangle = ^Triangle;
  Triangle = Packed Record
  {$ENDIF}
               Indices:Array[0..2] Of Word;
            End;

  PTriangleArray=^TriangleArray;
  TriangleArray=Array[0..65536]Of Triangle;

  TriangleAdjancency = Packed Record
    Indices:Array[0..2] Of Integer;
  End;

//Misc
{$IFDEF OXYGENE}
Function Abs(S:Single):Single;
Function Exp(S:Single):Single;
Function Ln(S:Single):Single;
Function Trunc(S:Single):Single;
Function Round(S:Single):Single;
Function Frac(S:Single):Single;
Function Sin(S:Single):Single;
Function Cos(S:Single):Single;
Function ArcTan(S:Single):Single;
Function Sqrt(S:Single):Single;
Function Sqr(S:Single):Single;
Function Pos(Const S2, S:TERRAString):Integer;
Function Odd(N:Integer):Boolean;
Procedure Halt(ExitCode:Integer);
Procedure Delete(Var S:TERRAString; Ind, Count:Integer);
Function Copy(Const S:TERRAString; Ind,Count:Integer):TERRAString;
{$ENDIF}

Function IntMax(Const A,B:Integer):Integer;
Function IntMin(Const A,B:Integer):Integer;
Function LongMax(Const A,B:Cardinal):Cardinal;
Function LongMin(Const A,B:Cardinal):Cardinal;

//Function GetNextLine(Var S:TERRAString):TERRAString;
//Function StringExtractNextWord(Var S:TERRAString; Separator:TERRAString=' '):TERRAString;
//Function GetNextArg(Var S:TERRAString):TERRAString;
//Function GetNextToken(Var S:TERRAString; Separator, Op, Ed:TERRAChar):TERRAString;
//Function FindCloseBracket(Const S:TERRAString; Op, Ed:TERRAChar):Integer;

Function TicksToTime(Ticks:Cardinal):TERRATime;

Function CardinalToString(Const N:Cardinal):TERRAString;Overload;
Function Int64ToString(Const N:Int64):TERRAString;
Function UInt64ToString(Const N:UInt64):TERRAString;
Function BoolToString(Const N:Boolean):TERRAString;Overload;
Function VersionToString(Const N:TERRAVersion):TERRAString;Overload;
//Function TicksToString(Const N:Cardinal):TERRAString;Overload;

Function TimeToString(Const N:TERRATime; Const HourSep, MinSep, SecondSep:TERRAString):TERRAString;

Function HexStr(Const Value:Byte):TERRAString;Overload;
Function HexStr(Const Value:Word):TERRAString;Overload;
Function HexStr(Const Value:Cardinal):TERRAString;Overload;
Function HexStr(Const Value:UInt64):TERRAString;Overload;

Function HexStrToInt(Const S:TERRAString):Integer;

Procedure TimeStampToDateAndtime(TimeStamp:Cardinal; Var Date:TERRADate; Var Time:TERRATime);

Function StringToInt(Const S:TERRAString; CheckError:Boolean = True):Integer;
Function StringToCardinal(Const S:TERRAString; CheckError:Boolean = True):Cardinal;
Function StringToBool(Const S:TERRAString):Boolean;
Function StringToVersion(Const S:TERRAString):TERRAVersion;
Function StringToFloat(Const S:TERRAString):Single;

Procedure FillByte(Var Dest; Count:Integer; Value:Byte);
Procedure FillWord(Var Dest; Count:Integer; Value:Word);
Procedure FillLong(Var Dest; Count:Integer; Value:Cardinal);


Procedure SetFlag(Var N:Byte; Const Flag:Byte; Const Value:Boolean); Overload;
Procedure SetFlag(Var N:Word; Const Flag:Word; Const Value:Boolean); Overload;
Procedure SetFlag(Var N:Cardinal; Const Flag:Cardinal; Const Value:Boolean); Overload;

Function SafeDiv(Const A,B:Single; WhenZero:Single = 0.0):Single;

Procedure ByteSwap32(Var A:Cardinal);
Procedure ByteSwap16(Var A:Word);

Function GetMultiProgress(A, SmallTotal, B, BigTotal:Integer):Integer;

Function MinutesToTicks(Minutes:Single):Cardinal;

Procedure DebugBreak(Condition:Boolean = True);

Procedure RemoveHint(X:Integer);

Implementation

{$IFNDEF OXYGENE}
Uses TERRA_Log, TERRA_Error, TERRA_Object;
{$ENDIF}

Function SafeDiv(Const A,B:Single; WhenZero:Single = 0.0):Single;
Begin
  If (B<>0) Then
    Result := A / B
  Else
    Result := WhenZero;
End;

{$IFDEF OXYGENE}
Function Trunc(S:Single):Single;
Begin
    Result := System.Math.Truncate(S);
End;

Function Round(S:Single):Single;
Begin
    Result := System.Math.Round(S);
End;

Function Frac(S:Single):Single;
Begin
    Result := S-System.Math.Truncate(S);
End;

Function Abs(S:Single):Single;
Begin
    Result := System.Math.Abs(S);
End;

Function Exp(S:Single):Single;
Begin
    Result := System.Math.Exp(S);
End;

Function Ln(S:Single):Single;
Begin
    Result := System.Math.Log(S);
End;

Function Sin(S:Single):Single;
Begin
    Result := System.Math.Sin(S);
End;

Function Cos(S:Single):Single;
Begin
    Result := System.Math.Cos(S);
End;

Function ArcTan(S:Single):Single;
Begin
    Result := System.Math.Atan(S);
End;

Function Sqrt(S:Single):Single;
Begin
    Result := System.Math.Sqrt(S);
End;

Function Sqr(S:Single):Single;
Begin
    Result := S*S;
End;

Function Odd(N:Integer):Boolean;
Begin
    Result := (N Mod 2)<>0; 
End;

Function Pos(S2, S:TERRAString):Integer;
Begin
    Result := S.IndexOf(S2);
End;

Procedure Halt(ExitCode:Integer);
Begin
    System.Windows.Forms.Application.Exit();
End;

Procedure Delete(Var S:TERRAString; Ind, Count:Integer);
Begin
    S.Remove(Ind, Count); 
End;

Function Copy(S:TERRAString; Ind,Count:Integer):TERRAString;
Begin
    Result := S.Substring(Ind, Count);
End;

Function StringEqual(A,B:TERRAString):Boolean;
Begin
    If ((A=nil) And (B='')) Or ((A='') And (B=nil)) Then
        Result := True
    Else
        Result := A.Equals(B);
End;

Function StringEnd(A:TERRAString):Integer;
Begin
    Result := A.Length - 1;
End;

{$ENDIF}

(*

Function StringExtractNextWord(Var S:TERRAString; Separator:TERRAString=' '):TERRAString;
Var
  I:Integer;
Begin
  S := TrimLeft(S);
  If S =  '' Then
  Begin
    Result := '';
    Exit;
  End;

  I := Pos(Separator, S);
  If I<1 Then
  Begin
    Result := S;
    S := '';
  End Else
  Begin
    Result := Copy(S, 1, (I-1));
    S := Copy(S, (I+1), MaxInt);
  End;
  S := TrimLeft(S);
End;*)

{Function GetNextToken(Var S:TERRAString; Separator, Op, Ed:TERRAChar):TERRAString;
Var
 I,J,K:Integer;
Begin
  S := TrimLeft(S);
  If S='' Then
  Begin
    Result:='';
    Exit;
  End;
  If S[1]=Op Then
  Begin
    J := FindCloseBracket(S, Op, Ed);
    If J<1 Then
    Begin
        Result:=S;
        S:='';
    End Else
    Begin
        Result := Copy(S,1,J);
        S:=Copy(S,J+1,Length(S)-J);
    End;
   End Else
   Begin
    I := Pos(Separator,S);
    J := Pos(Op,S);
    K := Pos(Ed,S);
    If (J<I) And (J>=1) Or (I<1) Then I:=J;
    If (K<I) And (K>=1) Or (I<1) Then I:=K;
    If I<1 Then
    Begin
     Result:=S;
     S:='';
    End Else
    Begin
     If I=1 Then K:=1 Else K:=I-1;
     Result := Copy(S,1,K);
     S := Copy(S,I,Length(S)-I+1);
    End;
   End;
  S := TrimLeft(S);
End;

Function GetNextArg(Var S:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  I := Pos(',',S);
  If I<1 Then
  Begin
    Result := S;
    S := '';
  End Else
  Begin
    Result := Copy(S,1, I-1);
    S := Copy(S, I+1, Length(S));
  End;
End;

Function FindCloseBracket(Const S:TERRAString; Op:TERRAChar='(';Ed:TERRAChar=')'):Integer;
Var
 K,I:Integer;
Begin
  K := 0;
  Result := 1 - 1;
  For I:=1 To Length(S) Do
  If S[I] = Op Then
    Inc(K)
  Else
  If S[I] = Ed Then
  Begin
    If K=1 Then
    Begin
      Result := I;
      Break;
    End Else
      Dec(K);
  End;
End;}


(*Function TrimLeft(Const S:TERRAString):TERRAString;
Var
  I,L:Integer;
Begin
  L := Length(S);
  I := 1;
  {$IFDEF OXYGENE}
  While (I < L) And (S[I]<=' ') Do
  {$ELSE}
  While (I <= L) And (S[I]<=' ') Do
  {$ENDIF}
    Inc(I);
  Result := Copy(S,I,Maxint);
End;

Function TrimRight(Const S:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  I := Length(S);
  While (I>=1) And (S[I]<=' ') Do
    Dec(I);

  Result:=Copy(S,1,I);
End;

Function UpCase(Const C:TERRAChar):TERRAChar;
Begin
    If (C>='a')And(C<='z') Then
        Result := TERRAChar(Byte(C) - 32)
    Else
        Result := C;
End;

//Converts a string to upcase
Function StringUpper(Const S:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result := S;
  For I:=1 To Length(S) Do
    Result[I] := UpCase(Result[I]);
End;

//Converts a string to lowercase
Function StringLower(Const S:TERRAString):TERRAString;
Var
  I:Integer;
  C:TERRAChar;
Begin
  Result:='';
  For I:=1 To Length(S) Do
  Begin
    C:=S[I];
    If (C>='A')And(C<='Z') Then
      Inc(C,32);
    Result:=Result+C;
  End;
End;

Function CapStr(Const S:TERRAString):TERRAString;
Begin
  Result := StringLower(S);
  If Result<>'' Then
    Result[1] := UpCase(Result[1]);
End;*)

Function StringToInt(Const S:TERRAString; CheckError:Boolean):Integer;
Var
  Value, N:Integer;
  It:StringIterator;
  C:TERRAChar;
  Negative:Boolean;
Begin
  Result := 0;
  If S = '' Then
    Exit;

  N := 1;
  Negative := False;
  StringCreateReverseIterator(StringTrim(S), It);
  While It.HasNext Do
  Begin
    C := It.GetNext();

    If (C= '-') Then
    Begin
      Negative := True;
      Continue;
    End;

    If (C<'0') Or (C>'9') Then
    Begin
      {$IFDEF WINDOW}
      If CheckError Then
        RaiseError('String to int failure: '+S);
      {$ENDIF}
      Exit;
    End;

    Value := Ord(C) - Ord('0');
    Inc(Result, Value * N);

    N := N * 10;
  End;

  If Negative Then
    Result := -Result;
End;

Function StringToCardinal(Const S:TERRAString; CheckError:Boolean):Cardinal;
Var
  N:Int64;
  Value:Cardinal;
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := 0;

  If S = '' Then
    Exit;

  If (StringFirstChar(S) = '-') Then
  Begin
    Exit;
  End;

  N := 1;
  StringCreateReverseIterator(StringTrim(S), It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C<'0') Or (C>'9') Then
    Begin
      {$IFDEF WINDOW}
      If CheckError Then
        RaiseError('String to cardinal failure: '+S);
      {$ENDIF}

      Exit;
    End;

    Value := Ord(C) - Ord('0');
    Inc(Result, Value * N);

    N := N * 10;
  End;
End;


{$IFDEF OXYGENE}
Function StringToBool(Const S:TERRAString):Boolean;
Begin
  Result := StringUpper(S).Equals(iTrue);
End;

//Converts a string to an Single
Function StringToFloat(Const S:TERRAString):Single;
Begin
  Result := System.Convert.ToSingle(S);
End;

{$ELSE}
Function StringToBool(Const S:TERRAString):Boolean;
Begin
  Result:= (S='1') Or (StringUpper(S)=StringUpper(iTrue));
End;

//Converts a string to an Single
Function StringToFloat(Const S:TERRAString):Single;
Var
  I:Integer;
  X:Single;
Begin
  System.Val(S,X,I);
  Result := X;
End;
{$ENDIF}

Function StringToVersion(Const S:TERRAString):TERRAVersion;
Var
  S1,S2:TERRAString;
Begin
  S1 := S;
  S2 := StringGetNextSplit(S1, '.');
  Result.Major := StringToInt(S2);
  S2 := StringGetNextSplit(S1, '.');
  Result.Minor := StringToInt(S2);
  Result.Build := StringToInt(S1);
End;

Function Int64ToString(Const N:Int64):TERRAString;
Begin
  {$IFDEF OXYGENE} 
  Result := N.ToString();
  {$ELSE}
  Str(N, Result);
  {$ENDIF}
End;

Function UInt64ToString(Const N:UInt64):TERRAString;
Begin
  {$IFDEF OXYGENE} 
  Result := N.ToString();
  {$ELSE}
  Str(N, Result);
  {$ENDIF}
End;

Function BoolToString(Const N:Boolean):TERRAString;
Begin
  If N Then
    Result := iTrue
  Else
    Result := iFalse;
End;

Function TimeToString(Const N:TERRATime; Const HourSep, MinSep, SecondSep:TERRAString):TERRAString;
Begin
  Result := CardinalToString(N.Hour) + HourSep +
            CardinalToString(N.Minute) + MinSep;

  If SecondSep<>'' Then
    Result := Result +  CardinalToString(N.Second) + SecondSep;
End;

{$IFDEF OXYGENE}
Function CardinalToString(Const N:Cardinal):TERRAString;
Begin
    Result := System.Convert.ToString(N);
End;
{$ELSE}
Function CardinalToString(Const N:Cardinal):TERRAString;
Var
  S:TERRAString;
Begin
  Str(N,S);
  Result:=S;
End;
{$ENDIF}

Function VersionToString(Const N:TERRAVersion):TERRAString;
Begin
  Result := CardinalToString(N.Major)+'.'+CardinalToString(N.Minor)+'.'+CardinalToString(N.Build);
End;

Function TicksToTime(Ticks:Cardinal):TERRATime;
Begin
  Result.Hour := (Ticks Div (60 * 60 * 1000)); // Math.round() rounds the number up or down
  Result.Minute := (Ticks Div (60*1000));
  Result.Minute := Result.Minute  Mod 60;
  Result.Second := (Ticks Div 1000) Mod 60;
End;
{Function TicksToString(Const N:Cardinal):TERRAString;
Var
  Ext:TERRAString;
  X:Single;
  Int,Rem:Integer;
Begin
  If (N>=60000)Then
  Begin
    X:=N/60000;
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='m';
  End Else
  If (N>=1000)Then
  Begin
    X:=N/1000;
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='s';
  End Else
  Begin
    Int:=N;
    Rem:=0;
    Ext:='ms';
  End;

  Result:= IntegerProperty.Stringify(Int);
  If Rem>0 Then
    Result:=Result+'.'+ IntegerProperty.Stringify(Rem);
  Result:=Result+' '+Ext;
End;}


Function GetVersionID(Const Major,Minor,Build:Word):Word;
Begin
 Result:=(Major*1000)+(Minor*100)+Build;
End;

(*Procedure ReplaceText(Const Token,Value:TERRAString; Var S:TERRAString);
Var
  I:Integer;
  S2:TERRAString;
Begin
  If (Token = Value) Then
    Exit;

  I := Pos(StringUpper(Token),StringUpper(S));
  If (I>=1) Then
  Begin
    S2 := Copy(S,I+Length(Token),Length(S)-I);
    S := Copy(S,1,(I-1));
    ReplaceText(Token, Value, S2);
    S := S + Value + S2;
  End;
End;*)

Const
  Hex : Array[0..15] of TERRAChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

Function HexStr(Const Value:Byte):TERRAString;
Begin
  Result := ''+ Hex[(value SHR  4) AND $0F] + Hex[value AND $0F];
End;

Function HexStr(Const Value:Word):TERRAString;
Begin
  Result:= ''+ Hex[(value SHR 12) AND $0F] +  Hex[(value SHR  8) AND $0F] + Hex[(value SHR  4) AND $0F] + Hex[value AND $0F];
End;

Function HexStr(Const Value:Cardinal):TERRAString;
Begin
  Result := ''+ Hex[(value SHR 28) AND $0F] + Hex[(value SHR 24) AND $0F] + Hex[(value SHR 20) AND $0F] + Hex[(value SHR 16) AND $0F] + Hex[(value SHR 12) AND $0F] + Hex[(value SHR  8) AND $0F] + Hex[(value SHR  4) AND $0F] + Hex[value AND $0F];
End;

Function HexStr(Const Value:UInt64):TERRAString;Overload;
Begin
  Result := ''+ Hex[(value SHR 60) AND $0F] + Hex[(value SHR 56) AND $0F] + Hex[(value SHR 52) AND $0F] + Hex[(value SHR 48) AND $0F]+
  Hex[(value SHR 44) AND $0F] + Hex[(value SHR  40) AND $0F] + Hex[(value SHR  36) AND $0F] + Hex[value AND 32]+ Hex[(value SHR 28) AND $0F] +
  Hex[(value SHR 24) AND $0F] + Hex[(value SHR 20) AND $0F] + Hex[(value SHR 16) AND $0F] + Hex[(value SHR 12) AND $0F]+
  Hex[(value SHR  8) AND $0F] +  Result[15] + Hex[(value SHR  4) AND $0F] + Hex[value AND $0F];
End;

Function HexStrToInt(Const S:TERRAString):Integer;
Var
  J,K:Integer;
  C:TERRAChar;
  It:StringIterator;
Begin
  Result := 0;
  K := 1;
  StringCreateReverseIterator(S, It);
  While It.HasNext() Do
  Begin
    C := CharUpper(It.GetNext());

    For J:=0 To 15 Do
    If (C = Hex[J]) Then
    Begin
      Inc(Result, J * K);
      Break;
    End;

    K := K Shl 4;
  End;
End;

Procedure SetFlag(Var N:Byte; Const Flag:Byte; Const Value:Boolean);
Begin
  If (((N And Flag)<>0)<>Value)Then N:=N Xor Flag;
End;

Procedure SetFlag(Var N:Word; Const Flag:Word; Const Value:Boolean);
Begin
  If (((N And Flag)<>0)<>Value)Then N:=N Xor Flag;
End;

Procedure SetFlag(Var N:Cardinal; Const Flag:Cardinal; Const Value:Boolean);
Begin
  If (((N And Flag)<>0)<>Value)Then N:=N Xor Flag;
End;

{$IFNDEF OXYGENE}
Procedure FillByte(Var Dest; Count:Integer; Value:Byte);
Var
  P:PByte;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;

Procedure FillWord(Var Dest; Count:Integer; Value:Word);
Var
  P:PWord;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;

Procedure FillLong(Var Dest; Count:Integer; Value:Cardinal);
Var
  P:PCardinal;
Begin
  P:=@Dest;
  While (Count>0) Do
  Begin
    P^:=Value;
    Inc(P);
    Dec(Count);
  End;
End;
{$ENDIF}

Procedure ByteSwap32(Var A:Cardinal);
Var
  B1,B2,B3,B4:Byte;
  N:Cardinal;
Begin
  B1:=A And 255;
  B2:=(A Shr 8) And 255;
  B3:=(A Shr 16)And 255;
  B4:=(A Shr 24)And 255;

  N := (Cardinal(B1)Shl 24) + (Cardinal(B2) Shl 16) + (Cardinal(B3) Shl 8) + B4;
  A := N;
End;

Procedure ByteSwap16(Var A:Word);
Var
  B1,B2:Byte;
  N:Word;
Begin
  B1:=A And 255;
  B2:=(A Shr 8) And 255;

  N := (B1 Shl 8)+B2;
  A := N;
End;

Function IntMax(Const A,B:Integer):Integer; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function IntMin(Const A,B:Integer):Integer; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

Function LongMin(Const A,B:Cardinal):Cardinal; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A<B Then Result:=A Else Result:=B;
End;

Function LongMax(Const A,B:Cardinal):Cardinal; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If A>B Then Result:=A Else Result:=B;
End;

Function GetMultiProgress(A, SmallTotal, B, BigTotal:Integer):Integer;
Var
  X, Y:Single;
Begin
  If (SmallTotal>0) Then
    X := A / SmallTotal
  Else
    X := 0;

  If (BigTotal>0) Then
    Y := B / BigTotal
  Else
    Y := 0;

  Result := Trunc(100 * Y + (1/BigTotal) * X);
End;

{$IFDEF OXYGENE}
Constructor TERRAVersion.Create(Major,Minor,Build:Word);
Begin
    Self.Major := Major;
    Self.Minor := Minor;
    Self.Build := Build;
End;
{$ENDIF}


Function MinutesToTicks(Minutes:Single):Cardinal;
Begin
  Result := Trunc(1000*60*Minutes);
End;

Procedure TimeStampToDateAndtime(TimeStamp:Cardinal; Var Date:TERRADate; Var Time:TERRATime);
Var
  S,H,M,X,B,C,D,E,F:Integer;
Begin
  s := TimeStamp Mod 86400;
  TimeStamp := TimeStamp Div 86400;
  h := s Div 3600;
  m := (s Div 60) Mod 60;
  s := s Mod 60;
  x := (TimeStamp*4+102032) Div 146097 + 15;
  b := TimeStamp + 2442113 + x -(x Div 4);
  c := ((b*20)-2442) Div 7305;
  d := (b- 365*c) - (c Div 4);
  e := (d*1000) Div 30601;
  f := (d-e*30-e*601) Div 1000;

  If (E<14) Then
  Begin
    Date.Year := c-4716;
    Date.Month := e-1;
  End Else
  Begin
    Date.Year := c-4715;
    Date.Month := e-13;
  End;
  Date.Day := f;

  Time.Hour := H;
  Time.Minute := M;
  Time.Second := S;
  Time.MiliSecond := 0;
End;

Procedure DebugBreak(Condition:Boolean = True);
Begin
{$IFNDEF OXYGENE}
  {$IFNDEF FPC}
  If Condition Then
  Asm
    Int 3;
  End;
  {$ENDIF}
{$ENDIF}
End;

Procedure RemoveHint(X:Integer);
Begin
End;


End.







