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

Uses TERRA_String;

Type
  TERRATime=Record
    Hour:Word;
    Minute:Word;
    Second:Word;
    MiliSecond:Word;
  End;

  TERRADate=Record
    Year:Word;
    Month:Byte;
    Day:Byte;
    WeekDay:Byte;
  End;

  {$IFDEF OXYGENE}
  PByte = ^Byte;
  PWord = ^Word;
  PCardinal = ^Cardinal;

  TERRAVersion = Class
    Constructor Create(Major,Minor,Build:Word);
{$ELSE}
  TERRAVersion=Record
{$ENDIF}
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

Function IntToString(Const N:Integer):TERRAString;
Function CardinalToString(Const N:Cardinal):TERRAString;Overload;
Function Int64ToString(Const N:Int64):TERRAString;
Function UInt64ToString(Const N:UInt64):TERRAString;
Function FloatToString(N:Single):TERRAString;
Function BoolToString(Const N:Boolean):TERRAString;Overload;
Function VersionToString(Const N:TERRAVersion):TERRAString;Overload;
//Function TicksToString(Const N:Cardinal):TERRAString;Overload;

Function TimeToString(Const N:TERRATime; Const HourSep, MinSep, SecondSep:TERRAString):TERRAString;

Function HexStr(Const Value:Byte):TERRAString;Overload;
Function HexStr(Const Value:Word):TERRAString;Overload;
Function HexStr(Const Value:Cardinal):TERRAString;Overload;
Function HexStr(Const Value:UInt64):TERRAString;Overload;

Function HexStrToInt(Const S:TERRAString):Integer;

Function StringToBase64(Buf:TERRAString):TERRAString;
Function Base64ToString(B64:TERRAString):TERRAString;

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
Uses TERRA_Log, TERRA_Error;

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

    If (C= Ord('-')) Then
    Begin
      Negative := True;
      Continue;
    End;

    If (C<Ord('0')) Or (C>Ord('9')) Then
    Begin
      {$IFDEF WINDOW}
      If CheckError Then
        RaiseError('String to int failure: '+S);
      {$ENDIF}
      Exit;
    End;

    Value := C - Ord('0');
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

  If (StringFirstChar(S) = Ord('-')) Then
  Begin
    Exit;
  End;

  N := 1;
  StringCreateReverseIterator(StringTrim(S), It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    If (C<Ord('0')) Or (C>Ord('9')) Then
    Begin
      {$IFDEF WINDOW}
      If CheckError Then
        RaiseError('String to cardinal failure: '+S);
      {$ENDIF}

      Exit;
    End;

    Value := C - Ord('0');
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
  S2 := StringGetNextSplit(S1, Ord('.'));
  Result.Major := StringToInt(S2);
  S2 := StringGetNextSplit(S1, Ord('.'));
  Result.Minor := StringToInt(S2);
  Result.Build := StringToInt(S1);
End;

{$IFDEF OXYGENE}
Function IntToString(Const N:Integer):TERRAString;
Begin
  Result := System.Convert.ToString(N);
End;
{$ELSE}
Function IntToString(Const N:Integer):TERRAString;
Var
  S:TERRAString;
Begin
  Str(N,S);
  Result := S;
End;
{$ENDIF}

Function Int64ToString(Const N:Int64):TERRAString;
Var
  S:TERRAString;
Begin
  Str(N,S);
  Result := S;
End;

Function UInt64ToString(Const N:UInt64):TERRAString;
Var
  S:TERRAString;
Begin
  Str(N,S);
  Result := S;
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

Function FloatToString(N:Single):TERRAString;
Var
  X:Single;
  A,B, I:Integer;

  Current, DecimalPlaces:Integer;
Begin
  If (N<0) Then
    Result := '-'
  Else
    Result := '';

  N := Abs(N);
  A := Trunc(N);
  X := Frac(N);

  Result := Result + IntToString(A) +'.';

  DecimalPlaces := 0;
  I := 10;
  Repeat
    Current := Trunc(X * I) Mod 10;
    I := I * 10;
    Inc(DecimalPlaces);
  Until (DecimalPlaces>=7) Or (Current=0);

  B := 1;
  For I:=1 To DecimalPlaces Do
    B := B * 10;

  Result := Result + IntToString(Trunc(B * X));
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

  Result:=IntToString(Int);
  If Rem>0 Then
    Result:=Result+'.'+IntToString(Rem);
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
  Hex : Array[0..15] of AnsiChar = '0123456789ABCDEF';

{$IFDEF OXYGENE}
Function HexStr(Const Value:Byte):TERRAString;
Var
  txt:TERRAString;
Begin
  txt := new String('0', 2);
  txt[1] := Hex[(value SHR  4) AND $0F];
  txt[2] := Hex[value AND $0F];
  Result := txt;
End;

Function HexStr(Const Value:Word):TERRAString;
Var
  txt:TERRAString;
Begin
  txt := new String('0', 4);
  txt[1] := Hex[(value SHR 12) AND $0F];
  txt[2] := Hex[(value SHR  8) AND $0F];
  txt[3] := Hex[(value SHR  4) AND $0F];
  txt[4] := Hex[value AND $0F];
  Result := txt;
End;

Function HexStr(Const Value:Cardinal):TERRAString;
Var
  txt :TERRAString;
Begin
  txt := new String('0', 8);
  txt[1]:= Hex[(value SHR 28) AND $0F];
  txt[2]:= Hex[(value SHR 24) AND $0F];
  txt[3]:= Hex[(value SHR 20) AND $0F];
  txt[4]:= Hex[(value SHR 16) AND $0F];
  txt[5]:= Hex[(value SHR 12) AND $0F];
  txt[6]:= Hex[(value SHR  8) AND $0F];
  txt[7]:= Hex[(value SHR  4) AND $0F];
  txt[8]:= Hex[value AND $0F];
  Result:= txt;
End;
{$ELSE}
Function HexStr(Const Value:Byte):TERRAString;
Begin
  SetLength(Result, 2);
  Result[1] := Hex[(value SHR  4) AND $0F];
  Result[2] := Hex[value AND $0F];
End;

Function HexStr(Const Value:Word):TERRAString;
Begin
  SetLength(Result, 4);
  Result[1]:= Hex[(value SHR 12) AND $0F];
  Result[2]:= Hex[(value SHR  8) AND $0F];
  Result[3]:= Hex[(value SHR  4) AND $0F];
  Result[4]:= Hex[value AND $0F];
End;

Function HexStr(Const Value:Cardinal):TERRAString;
Begin
  SetLength(Result, 8);
  Result[1]:= Hex[(value SHR 28) AND $0F];
  Result[2]:= Hex[(value SHR 24) AND $0F];
  Result[3]:= Hex[(value SHR 20) AND $0F];
  Result[4]:= Hex[(value SHR 16) AND $0F];
  Result[5]:= Hex[(value SHR 12) AND $0F];
  Result[6]:= Hex[(value SHR  8) AND $0F];
  Result[7]:= Hex[(value SHR  4) AND $0F];
  Result[8]:= Hex[value AND $0F];
End;

Function HexStr(Const Value:UInt64):TERRAString;Overload;
Begin
  SetLength(Result, 16);
  Result[1]:= Hex[(value SHR 60) AND $0F];
  Result[2]:= Hex[(value SHR 56) AND $0F];
  Result[3]:= Hex[(value SHR 52) AND $0F];
  Result[4]:= Hex[(value SHR 48) AND $0F];
  Result[5]:= Hex[(value SHR 44) AND $0F];
  Result[6]:= Hex[(value SHR  40) AND $0F];
  Result[7]:= Hex[(value SHR  36) AND $0F];
  Result[8]:= Hex[value AND 32];
  Result[9]:= Hex[(value SHR 28) AND $0F];
  Result[10]:= Hex[(value SHR 24) AND $0F];
  Result[11]:= Hex[(value SHR 20) AND $0F];
  Result[12]:= Hex[(value SHR 16) AND $0F];
  Result[13]:= Hex[(value SHR 12) AND $0F];
  Result[14]:= Hex[(value SHR  8) AND $0F];
  Result[15]:= Hex[(value SHR  4) AND $0F];
  Result[16]:= Hex[value AND $0F];
End;
{$ENDIF}

Function HexStrToInt(Const S:TERRAString):Integer;
Var
  I,J,K:Integer;
Begin
  Result := 0;
  K := 1;
  For I:=Length(S) DownTo 1 Do
  Begin
    For J:=0 To 15 Do
    If (UpCase(S[I]) = Hex[J]) Then
    Begin
      Inc(Result, J * K);
      Break;
    End;

    K := K Shl 4;
  End;
End;

Const
  Base64Code:TERRAString= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                        'abcdefghijklmnopqrstuvwxyz'+
                        '0123456789+/';
  Pad = '=';

Function StringToBase64(Buf:TERRAString):TERRAString;
Var
  I:Integer;
  x1,x2,x3,x4:Byte;
  PadCount:Integer;
Begin
  PadCount := 0;
 // we need at least 3 input bytes...
  While Length(Buf)<3 Do
  Begin
    Buf := Buf + #0;
    Inc( PadCount );
  End;
 // ...and all input must be an even multiple of 3
  While ( length( Buf ) mod 3 ) <> 0 do
  Begin
    Buf:=Buf+#0; // if not, zero padding is added
    Inc(PadCount);
  End;

  Result := '';
  I := 1;

 // process 3-byte blocks or 24 bits
 {$IFDEF OXYGENE}
  While (I<Length(Buf)-2) Do
 {$ELSE}
  While (I<=Length(Buf)-2) Do
{$ENDIF}
  Begin
    // each 3 input bytes are transformed into 4 index values
    // in the range of  0..63, by taking 6 bits each step

    // 6 high bytes of first char
    x1 := ( Ord( Buf[i] ) shr 2 ) and $3F;

    // 2 low bytes of first char + 4 high bytes of second char
    x2 := ( ( Ord( Buf[i] ) shl 4 ) and $3F )
      or Ord( Buf[i + 1] ) shr 4;

    // 4 low bytes of second char + 2 high bytes of third char
    x3 := ( ( Ord( Buf[i + 1] ) shl 2 ) and $3F )
      or Ord( Buf[i + 2] ) shr 6;

    // 6 low bytes of third char
    x4 := Ord( Buf[i + 2] ) and $3F;

    // the index values point into the code array
    Result:=Result + Base64Code[x1 + 1] + Base64Code[x2 + 1]
                   + Base64Code[x3 + 1] + Base64Code[x4 + 1];
    Inc(i,3);
  End;

 // if needed, finish by forcing padding chars ('=')
 // at end Of TERRAString
  If PadCount>0 Then
    For i := Length( Result ) DownTo 1 do
    Begin
      Result[i] := Pad;
      Dec(PadCount);
      If PadCount=0 Then
        Break;
    End;
End;

// helper : given a char, returns the index in code table
Function Char2IDx(c:AnsiChar):Byte;
Var
  I:Integer;
Begin
  For I:=1 To Length(Base64Code) Do
  If Base64Code[i]=C Then
  Begin
    Result := (I-1);
    Exit;
  End;
  Result := Ord(Pad);
End;

{$RANGECHECKS OFF}
Function Base64ToString(B64:TERRAString):TERRAString;
Var
  I,PadCount:Integer;
  Block:TERRAString;
  N:Integer;
  A,B,C,D:Byte;
Begin
	// input _must_ be at least 4 chars long,
	// or multiple of 4 chars
	If (Length(B64)<4) Or (Length(B64) Mod 4<>0) Then
	Begin
		Result := '';		
		Exit;
	End;

  PadCount:=0;
  I:=Length(B64);
  // count padding chars, if any
  While (B64[i]=Pad) And (i>0) Do
  Begin
    Inc(PadCount);
    Dec(I);
  End;

  Result:='';
  i:=1;
  {$IFDEF OXYGENE}
  Block := new String('0', 3);
  {$ELSE}
  SetLength(Block,3);
  {$ENDIF}
  While i<=Length(B64)-3 Do
  Begin
    A := Char2Idx(B64[I+0]);
    B := Char2IDx(B64[I+1]);
    C := Char2IDx(B64[I+2]);
    D := Char2IDx(B64[I+3]);

    // reverse process of above
    N := (A Shl 2) Or (B Shr 4);
    Result := Result+Chr(N);

    N := (B Shl 4) Or (C Shr 2);
    Result := Result + Chr(N);

    N := (C Shl 6 ) Or D;
    Result := Result + Chr(N);
    Inc(i,4);
  End;

  // delete padding, if any
  While PadCount>0 Do
  Begin
    Delete(Result, Length(Result), 1);
    Dec(PadCount);
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
  {$IFNDEF FPC}
  If Condition Then
  Asm
    Int 3;
  End;
  {$ENDIF}
End;

Procedure RemoveHint(X:Integer);
Begin
End;


End.







