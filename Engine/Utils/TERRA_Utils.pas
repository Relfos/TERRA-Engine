{$IFDEF OXYGENE}
namespace TERRA;

{$ELSE}
Unit TERRA_Utils;
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

  ProgressNotifier = Class
    Protected
      _CurrentPhase:Integer;
      _PhaseCount:Integer;

    Public
      Procedure Reset(Count:Integer);
      Procedure Notify(Value:Single);
      Procedure OnProgress(Progress:Integer); Virtual; Abstract;

      Procedure NextPhase();

      Property PhaseCount:Integer Read _PhaseCount Write _PhaseCount;
      Property CurrentPhase:Integer Read _CurrentPhase Write _CurrentPhase;
  End;

Const
{$IFDEF OXYGENE}
  EngineVersion:TERRAVersion= new Version(3, 5, 0);
{$ELSE}
  EngineVersion:TERRAVersion=(Major:3;Minor:3;Build:0);
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
Function Pos(Const S2, S:AnsiString):Integer;
Function Odd(N:Integer):Boolean;
Procedure Halt(ExitCode:Integer);
Procedure Delete(Var S:AnsiString; Ind, Count:Integer);
Function Copy(Const S:AnsiString; Ind,Count:Integer):AnsiString;
{$ENDIF}

Function IntMax(Const A,B:Integer):Integer;
Function IntMin(Const A,B:Integer):Integer;
Function LongMax(Const A,B:Cardinal):Cardinal;
Function LongMin(Const A,B:Cardinal):Cardinal;

Function GetNextLine(Var S:AnsiString):AnsiString;
Function GetNextWord(Var S:AnsiString; Separator:AnsiString=' '):AnsiString;
Function GetNextArg(Var S:AnsiString):AnsiString;
Function GetNextToken(Var S:AnsiString; Separator:AnsiChar=','; Op:AnsiChar='(';Ed:AnsiChar=')'):AnsiString;
Function FindCloseBracket(Const S:AnsiString; Op:AnsiChar='(';Ed:AnsiChar=')'):Integer;

Function TrimLeft(Const S:AnsiString):AnsiString;
Function TrimRight(Const S:AnsiString):AnsiString;

Function UpStr(Const S:AnsiString):AnsiString;
Function LowStr(Const S:AnsiString):AnsiString;
Function CapStr(Const S:AnsiString):AnsiString;

Function StrLPad(Const S:AnsiString; N:Integer; Token:AnsiChar='0'):AnsiString;
Function StrRPad(Const S:AnsiString; N:Integer; Token:AnsiChar='0'):AnsiString;
Function StrClean(Const S:AnsiString):AnsiString;

Function TicksToTime(Ticks:Cardinal):TERRATime;

Function IntToString(Const N:Integer):AnsiString;
Function CardinalToString(Const N:Cardinal):AnsiString;Overload;
Function Int64ToString(Const N:Int64):AnsiString;
Function UInt64ToString(Const N:UInt64):AnsiString;
Function FloatToString(Const N:Single; DecimalPlaces:Integer = 4):AnsiString;
Function BoolToString(Const N:Boolean):AnsiString;Overload;
Function TimeToString(Const N:TERRATime):AnsiString;Overload;
Function VersionToString(Const N:TERRAVersion):AnsiString;Overload;
//Function TicksToString(Const N:Cardinal):AnsiString;Overload;

Function HexStr(Const Value:Byte):AnsiString;Overload;
Function HexStr(Const Value:Word):AnsiString;Overload;
Function HexStr(Const Value:Cardinal):AnsiString;Overload;
Function HexStr(Const Value:UInt64):AnsiString;Overload;

Function HexStrToInt(Const S:AnsiString):Integer;

Function StringToBase64(Buf:AnsiString):AnsiString;
Function Base64ToString(B64:AnsiString):AnsiString;

Function Tabs(Length:Integer):AnsiString;
Function Spaces(Length:Integer):AnsiString;

Procedure TimeStampToDateAndtime(TimeStamp:Integer; Var Date:TERRADate; Var Time:TERRATime);

Procedure ReplaceText(Const Token,Value:AnsiString; Var S:AnsiString);

Function PosRev(Const SubStr,Str:AnsiString):Integer;

Function UnicodePos(C:AnsiChar; S:AnsiString):Integer;
Function UnicodeChar(Code:Word):AnsiString;

Function StringToInt(Const S:AnsiString):Integer;
Function StringToCardinal(Const S:AnsiString):Cardinal;
Function StringToBool(Const S:AnsiString):Boolean;
Function StringToVersion(Const S:AnsiString):TERRAVersion;
Function StringToFloat(Const S:AnsiString):Single;

Function StringWithDoubleParagraphs(Const S:AnsiString):AnsiString;

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

Procedure RemoveSpecialHTMLChars(Var S:AnsiString);

Procedure RemoveHint(X:Integer);

Type
  TERRAObject = Class
    Public
      Destructor Destroy; Override;
  End;

Procedure DestroyObject(Obj:Pointer);

Implementation
Uses TERRA_Log;

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

Function Pos(S2, S:AnsiString):Integer;
Begin
    Result := S.IndexOf(S2);
End;

Procedure Halt(ExitCode:Integer);
Begin
    System.Windows.Forms.Application.Exit();
End;

Procedure Delete(Var S:AnsiString; Ind, Count:Integer);
Begin
    S.Remove(Ind, Count); 
End;

Function Copy(S:AnsiString; Ind,Count:Integer):AnsiString;
Begin
    Result := S.Substring(Ind, Count);
End;

Function StringEqual(A,B:AnsiString):Boolean;
Begin
    If ((A=nil) And (B='')) Or ((A='') And (B=nil)) Then
        Result := True
    Else
        Result := A.Equals(B);
End;

Function StringEnd(A:AnsiString):Integer;
Begin
    Result := A.Length - 1;
End;

{$ENDIF}

Function GetNextLine(Var S:AnsiString):AnsiString;
Var
  I,J:Integer;
Begin
  If S =  '' Then
  Begin
    Result := '';
    Exit;
  End;

  I := 1;
  While I<Length(S) Do
  If (S[I]=#255) Then
    Inc(I, 3)
  Else
  If (S[I]=#10) Or (S[I]=#13) Then
  Begin
    J := Pred(I);
    If (S[I+1]=#10) Or (S[I+1]=#13) Then
      Inc(I);

    Result := Copy(S, 1, J);
    S := Copy(S, Succ(I), MaxInt);
    Exit;
  End Else
    Inc(I);

  Result := S;
  S := '';
End;

Function GetNextWord(Var S:AnsiString; Separator:AnsiString=' '):AnsiString;
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
End;

Function GetNextToken(Var S:AnsiString; Separator:AnsiChar=','; Op:AnsiChar='(';Ed:AnsiChar=')'):AnsiString;
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

Function GetNextArg(Var S:AnsiString):AnsiString;
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

Function GetFileExt(FileName:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result:='';
  Repeat
    I:=Pos('.',FileName);
    If I>=1 Then
    Begin
      Result := Copy(FileName,I+1,Length(FileName)-I);
      FileName := Copy(FileName, (I+1), MaxInt);
    End;
  Until I<1;
End;

Function FindCloseBracket(Const S:AnsiString; Op:AnsiChar='(';Ed:AnsiChar=')'):Integer;
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
End;

Function StrClean(Const S:AnsiString):AnsiString;
Var
  I,N:Integer;
Begin
  N := Length(S);
  For I:=1 To Length(S) Do
    If (S[I]<' ') Then
    Begin
      N := (I-1);
      Break;
    End;
  Result := Copy(S, 1, N);
End;

Function TrimLeft(Const S:AnsiString):AnsiString;
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

Function TrimRight(Const S:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  I := Length(S);
  While (I>=1) And (S[I]<=' ') Do
    Dec(I);

  Result:=Copy(S,1,I);
End;

Function UpCase(Const C:AnsiChar):AnsiChar;
Begin
    If (C>='a')And(C<='z') Then
        Result := AnsiChar(Byte(C) - 32)
    Else
        Result := C;
End;

//Converts a string to upcase
Function UpStr(Const S:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result := S;
  For I:=1 To Length(S) Do
    Result[I] := UpCase(Result[I]);
End;

//Converts a string to lowercase
Function LowStr(Const S:AnsiString):AnsiString;
Var
  I:Integer;
  C:AnsiChar;
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

Function CapStr(Const S:AnsiString):AnsiString;
Begin
  Result := LowStr(S);
  If Result<>'' Then
    Result[1] := UpCase(Result[1]);
End;

{$IFDEF OXYGENE}
Function StringToInt(Const S:AnsiString):Integer;
Begin
  Result := System.Convert.ToInt32(S);
End;

Function StringToCardinal(Const S:AnsiString):Cardinal;
Begin
  Result := System.Convert.ToUInt32(S);
End;

Function StringToBool(Const S:AnsiString):Boolean;
Begin
  Result := UpStr(S).Equals(iTrue);
End;

//Converts a string to an Single
Function StringToFloat(Const S:AnsiString):Single;
Begin
  Result := System.Convert.ToSingle(S);
End;

{$ELSE}
Function StringToInt(Const S:AnsiString):Integer;
Var
 K:Integer;
Begin
  Val(S,Result,K);
End;

Function StringToCardinal(Const S:AnsiString):Cardinal;
Var
  K:Integer;
Begin
  If (S<>'') And (S[1]='-') Then
    Result := 0
  Else
    Val(S, Result, K);
End;

Function StringToBool(Const S:AnsiString):Boolean;
Begin
  Result:=(UpStr(S)=UpStr(iTrue));
End;

//Converts a string to an Single
Function StringToFloat(Const S:AnsiString):Single;
Var
  I:Integer;
  X:Single;
Begin
  System.Val(S,X,I);
  Result := X;
End;
{$ENDIF}

Function StringWithDoubleParagraphs(Const S:AnsiString):AnsiString;
Begin
  Result := S;
  ReplaceText('\n \n', '##', Result);
  ReplaceText('\n', '##', Result);
  ReplaceText('##', '\n \n', Result);
End;

Function StringToVersion(Const S:AnsiString):TERRAVersion;
Var
  S1,S2:AnsiString;
Begin
  S1 := S;
  S2 := GetNextWord(S1,'.');
  Result.Major := StringToInt(S2);
  S2 := GetNextWord(S1,'.');
  Result.Minor := StringToInt(S2);
  Result.Build := StringToInt(S1);
End;

Function StrLPad(Const S:AnsiString;N:Integer; Token:AnsiChar='0'):AnsiString;
Begin
  Result := S;
  While Length(Result)<N Do
    Result := Token + Result;
End;

Function StrRPad(Const S:AnsiString;N:Integer; Token:AnsiChar='0'):AnsiString;
Begin
  Result := S;
  While Length(Result)<N Do
    Result := Result + Token;
End;

{$IFDEF OXYGENE}
Function IntToString(Const N:Integer):AnsiString;
Begin
  Result := System.Convert.ToString(N);
End;
{$ELSE}
Function IntToString(Const N:Integer):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result := S;
End;
{$ENDIF}

Function Int64ToString(Const N:Int64):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result := S;
End;

Function UInt64ToString(Const N:UInt64):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result := S;
End;


Function BoolToString(Const N:Boolean):AnsiString;
Begin
  If N Then
    Result := iTrue
  Else
    Result := iFalse;
End;

Function TimeToString(Const N:TERRATime):AnsiString;
Begin
  Result := CardinalToString(N.Hour) + ':' +
            CardinalToString(N.Minute) + ':' +
            CardinalToString(N.Second);
End;

{$IFDEF OXYGENE}
Function CardinalToString(Const N:Cardinal):AnsiString;
Begin
    Result := System.Convert.ToString(N);
End;
{$ELSE}
Function CardinalToString(Const N:Cardinal):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result:=S;
End;
{$ENDIF}

Function VersionToString(Const N:TERRAVersion):AnsiString;
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

{$IFDEF OXYGENE}
Function FloatToString(Const N:Single):AnsiString;
Begin
  Result := System.Convert.ToString(N);
End;
{$ELSE}
Function FloatToString(Const N:Single; DecimalPlaces:Integer):AnsiString;
Var
  P,X:Single;
  A,B, I:Integer;
Begin
  P := N;
  A := Trunc(P);
  X := Abs(Frac(P));
  For I:=1 To DecimalPlaces Do
    X := X*10;
  B := Trunc(X);

  Result := IntToString(A)+'.'+IntToString(B);

  If (A=0) And (P<0) Then
    Result := '-' + Result;

  {If (StringToFloat(Result)<>N) Then
    Str(P,Result);}
End;
{$ENDIF}

{Function TicksToString(Const N:Cardinal):AnsiString;
Var
  Ext:AnsiString;
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

Procedure ReplaceText(Const Token,Value:AnsiString; Var S:AnsiString);
Var
  I:Integer;
  S2:AnsiString;
Begin
  If (Token = Value) Then
    Exit;

  I := Pos(Upstr(Token),Upstr(S));
  If (I>=1) Then
  Begin
    S2 := Copy(S,I+Length(Token),Length(S)-I);
    S := Copy(S,1,(I-1));
    ReplaceText(Token, Value, S2);
    S := S + Value + S2;
  End;
End;

Const
  Hex : Array[0..15] of AnsiChar = '0123456789ABCDEF';

{$IFDEF OXYGENE}
Function HexStr(Const Value:Byte):AnsiString;
Var
  txt:AnsiString;
Begin
  txt := new String('0', 2);
  txt[1] := Hex[(value SHR  4) AND $0F];
  txt[2] := Hex[value AND $0F];
  Result := txt;
End;

Function HexStr(Const Value:Word):AnsiString;
Var
  txt:AnsiString;
Begin
  txt := new String('0', 4);
  txt[1] := Hex[(value SHR 12) AND $0F];
  txt[2] := Hex[(value SHR  8) AND $0F];
  txt[3] := Hex[(value SHR  4) AND $0F];
  txt[4] := Hex[value AND $0F];
  Result := txt;
End;

Function HexStr(Const Value:Cardinal):AnsiString;
Var
  txt :AnsiString;
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
Function HexStr(Const Value:Byte):AnsiString;
Begin
  SetLength(Result, 2);
  Result[1] := Hex[(value SHR  4) AND $0F];
  Result[2] := Hex[value AND $0F];
End;

Function HexStr(Const Value:Word):AnsiString;
Begin
  SetLength(Result, 4);
  Result[1]:= Hex[(value SHR 12) AND $0F];
  Result[2]:= Hex[(value SHR  8) AND $0F];
  Result[3]:= Hex[(value SHR  4) AND $0F];
  Result[4]:= Hex[value AND $0F];
End;

Function HexStr(Const Value:Cardinal):AnsiString;
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

Function HexStr(Const Value:UInt64):AnsiString;Overload;
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

Function HexStrToInt(Const S:AnsiString):Integer;
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
  Base64Code:AnsiString= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                        'abcdefghijklmnopqrstuvwxyz'+
                        '0123456789+/';
  Pad = '=';

Function StringToBase64(Buf:AnsiString):AnsiString;
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
 // at end Of AnsiString
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
  If Base64Code[i]=c Then
  Begin
    Result := (I-1);
    Exit;
  End;
  Result := Ord(Pad);
End;

{$RANGECHECKS OFF}
Function Base64ToString(B64:AnsiString):AnsiString;
Var
  I,PadCount:Integer;
  Block:AnsiString;
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

Function UnicodePos(C:AnsiChar; S:AnsiString):Integer;
Var
  I:Integer;
Begin
  I :=1;
  While (I<=Length(S)) Do
  If (S[I]=C) Then
  Begin
    Result := I;
    Exit;
  End Else
  If (S[I]=#255) Then
    Inc(I, 3)
  Else
    Inc(I);

  Result := -1;
End;

Function UnicodeChar(Code:Word):AnsiString;
Var
  A,B:Byte;
Begin
  A := Byte(Code And $FF);
  B := Byte((Code Shr 8) And $FF);
  Result := #255 + Chr(B) + Chr(A);
End;

//Returns the position of a substring inside of a string
//Search starts from the end of the string.
Function PosRev(Const SubStr,Str:AnsiString):Integer;
Var
 I,K,N:Integer;
Begin
 K := 0;
 N := Length(SubStr);
 For I:=Length(Str) DownTo 1 Do
 Begin
  If Str[I]=SubStr[N] Then
  Begin
   If N=1 Then
   Begin
    K:=I;
    Break;
   End Else
    Dec(N);
  End Else
   N:=Length(SubStr);
 End;
 Result:=K;
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


Destructor TERRAObject.Destroy;
Var
  S:AnsiString;
Begin
  S := Self.ClassName;
  Log(logWarning, 'App', S + ' destructor needs override.');

  Inherited;
End;

Procedure DestroyObject(Obj:Pointer);
Var
  Temp:TERRAObject;
  S:AnsiString;
Begin
  If (Obj = Nil) Then
    Exit;

  Temp := TERRAObject(Obj^);
  If (Temp = Nil) Then
    Exit;

  If (Temp Is TERRAObject) Then
  Begin
    {$IFDEF DEBUG_CORE}
    {$IFNDEF OSX}
    Log(logDebug, 'App', 'Destroying '+ Temp.ClassName);
    {$ENDIF}
    {$ENDIF}
    Temp.Destroy();
  End Else
  Begin
    S := Temp.ClassName;
    Log(logWarning, 'App', S + ' is not a valid TERRA object.');
  End;

  Cardinal(Obj^) := 0;
End;

Function MinutesToTicks(Minutes:Single):Cardinal;
Begin
  Result := Trunc(1000*60*Minutes);
End;

Procedure TimeStampToDateAndtime(TimeStamp:Integer; Var Date:TERRADate; Var Time:TERRATime);
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

Function Tabs(Length:Integer):AnsiString;
Var
  I:Integer;
Begin
  SetLength(Result, Length);
  For I:=1 To Length Do
    Result[I] := Tab;
End;

Function Spaces(Length:Integer):AnsiString;
Var
  I:Integer;
Begin
  SetLength(Result, Length);
  For I:=1 To Length Do
    Result[I] := ' ';
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

Procedure RemoveSpecialHTMLChars(Var S:AnsiString);
Begin
  ReplaceText('&amp;', '&', S);
  ReplaceText('&quot;', '"', S);
  ReplaceText('&lt;', '<', S);
  ReplaceText('&gt;', '>', S);
End;

Procedure RemoveHint(X:Integer);
Begin
End;

{ ProgressNotifier }
Procedure ProgressNotifier.Reset(Count:Integer);
Begin
  _CurrentPhase := 0;
  _PhaseCount := Count;
End;

Procedure ProgressNotifier.NextPhase();
Begin
  If (_CurrentPhase<Pred(_PhaseCount)) Then
    Inc(_CurrentPhase);
End;

Procedure ProgressNotifier.Notify(Value:Single);
Begin
  If (_CurrentPhase<0) Then
    _CurrentPhase := 0;

  If (_PhaseCount<=0) Then
    _PhaseCount := 1;

  If (Value<0) Then
    Value := 0
  Else
  If (Value>1) Then
    Value := 1;

  If (_PhaseCount>1) Then
    Value := (_CurrentPhase/_PhaseCount) + Value * (1/_PhaseCount);

  Self.OnProgress(Trunc(Value*100));
End;

End.







