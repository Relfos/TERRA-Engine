Unit TERRA_Unicode;
{$I terra.inc}

Interface
Uses TERRA_Utils;

{$IFDEF FPC}
Function UTF16CharacterToUnicode(p: PWideChar; Var CharLen: integer): Cardinal;
Function UnicodeToUTF16(u: cardinal): widestring;

Function UTF16Length(const s: widestring): PtrInt; Overload;
Function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt; Overload;

Function UnicodeToUTF8(u: cardinal): shortstring; Overload;
Function UnicodeToUTF8(u: cardinal; Buf: PAnsiChar): integer; Overload;
{$ENDIF}

Function utf8_to_ucs2(input:AnsiString):AnsiString;
Function ucs2_to_utf8(input:AnsiString):AnsiString;

Function UnicodeToUCS2(Key:Word):AnsiString;

Function ucs2_length(Const s:AnsiString):Integer;
Function ucs2_realindex(Const S:AnsiString; Index:Integer):Integer;
Function ucs2_char(Const S:AnsiString; Index:Integer):Word;
Function ucs2_str(Const S:AnsiString; Index:Integer):AnsiString;
Function ucs2_ascii(Const S:AnsiString; Index:Integer):AnsiChar;
Function ucs2_pos(Const Substr, S:AnsiString):Integer; Overload;
Function ucs2_pos_nocase(Const Substr, S:AnsiString):Integer; Overload;
Function ucs2_pos(C:AnsiChar; Const S:AnsiString):Integer; Overload;
Function ucs2_Copy(Const S:AnsiString; Index, Count:Integer):AnsiString;
Function ucs2_GetNextWord(Var S:AnsiString; Separator:AnsiChar=' '):AnsiString;
Function ucs2_TrimLeft(Const S:AnsiString):AnsiString;
Function ucs2_TrimRight(Const S:AnsiString):AnsiString;
Function ucs2_Trim(Const S:AnsiString):AnsiString;
Procedure ucs2_ReplaceText(Const Token,Value:AnsiString; Var S:AnsiString);

Function ucs2_UpStr(Const S:AnsiString):AnsiString;
Function ucs2_LowStr(Const S:AnsiString):AnsiString;
Function ucs2_CapStr(Const S:AnsiString):AnsiString;

Function ucs2_encode_html(Const S:AnsiString):AnsiString;

Implementation
Uses TERRA_Error;


Function utf8_to_ucs2(input:AnsiString):AnsiString;
Var
  W:Word;
  A,B,C:Byte;
  I:Integer;
  Function Next:Byte;
  Begin
    If (I>0) And (I<=Length(Input)) Then
      Result := Byte(Input[I])
    Else
      Result := 0;
    Inc(I);
  End;
Begin
  I := 1;
  Result := '';
  If (Input='') Then
    Exit;

  Repeat
    A := Next;

    If (A<$80) Then
    Begin
      Result := Result + Char(A);
      Continue;
    End;

    If ((A And $E0) = $E0) Then
    Begin
      B := Next;
      C := Next;
      If (B = 0) Or (C = 0) Then
        Continue;

      W := ((A And $0F) Shl 12) Or ((B And $3F) Shl 6) Or (C And $3F);
      B := W And $FF;
      A := (W Shr 8) And $FF;

      If (A=0) Then
        Result := Result + Char(B)
      Else
        Result := Result + #255+Char(A)+Char(B);
      Continue;
    End;

    If ((A And $C0) = $C0) Then
    Begin
      B := Next;
      If (B = 0) Then
        Continue;
      W := ((A And $1F) Shl 6) Or (B And $3F);
      B := W And $FF;
      A := (W Shr 8) And $FF;
      If (A=0) Then
        Result := Result + Char(B)
      Else
        Result := Result + #255+Char(A)+Char(B);
    End;

  Until I>Length(Input);
End;

Function ucs2_to_utf8(input:AnsiString):AnsiString;
{Const
  byteMask = $BF;
  byteMark = $80;}
Var
  I, J, Len:Integer;
  N:Word;
Begin
  Result := '';
  Len := ucs2_length(Input);
  For I:=1 To Len Do
  Begin
    N := ucs2_char(input, I);
    If N<=$7F Then
      Result := Result + Char(N)
    Else
    If N<=$7FF Then
    Begin
      Result := Result + Char($c0 or (N shr 6));
      Result := Result + Char($80 or (N and $3f));
    End Else
    Begin
      Result := Result + Char($e0 or (N shr 12));
      Result := Result + Char($80 or ((N shr 6) and $3f));
      Result := Result + Char($80 or (N and $3f));
    End;
  End;
End;


{$IFDEF FPC}
Function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PAnsiChar): integer;
Begin
  case u of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(u));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (u shr 6)));
        Buf[1]:=char(byte($80 or (u and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (u shr 12)));
        Buf[1]:=char(byte((u shr 6) and $3f) or $80);
        Buf[2]:=char(byte(u and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (u shr 18)));
        Buf[1]:=char(byte((u shr 12) and $3f) or $80);
        Buf[2]:=char(byte((u shr 6) and $3f) or $80);
        Buf[3]:=char(byte(u and $3f) or $80);
      end;
  else
    Result:=0;
  end;
End;

Function UnicodeToUTF8(u: cardinal; Buf: PAnsiChar): integer;
begin
  Result := UnicodeToUTF8SkipErrors(u,Buf);
  if Result=0 Then
    RaiseError('UnicodeToUTF8: invalid unicode: '+IntToString(u));
end;

Function UnicodeToUTF8(u: cardinal): shortstring;
Begin
  Result[0] := chr(UnicodeToUTF8(u,@Result[1]));
End;

Function UTF16CharacterLength(p: PWideChar):Integer;
// returns length of UTF16 character in number of words
// The endianess of the machine will be taken.
begin
  if p<>nil then begin
    if (ord(p[0]) < $D800) or (ord(p[0]) > $DFFF) then
      Result:=1
    else
      Result:=2;
  end else begin
    Result:=0;
  end;
end;

Function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (WordCount>0) do begin
    inc(Result);
    CharLen:=UTF16CharacterLength(p);
    inc(p,CharLen);
    dec(WordCount,CharLen);
  end;
end;

function UTF16Length(const s: widestring): PtrInt;
begin
  Result:=UTF16Length(PWideChar(s),length(s));
end;

Function UTF16CharacterToUnicode(p: PWideChar; Var CharLen: integer): Cardinal;
Var
  w1: cardinal;
  w2: Cardinal;
begin
  if p<>nil then begin
    w1:=ord(p[0]);
    if (w1 < $D800) or (w1 > $DFFF) then begin
      // is 1 word character
      Result:=w1;
      CharLen:=1;
    end else begin
      // could be 2 word character
      w2:=ord(p[1]);
      if (w2>=$DC00) then begin
        // is 2 word character
        Result:=(w1-$D800) shl 10 + (w2-$DC00) + $10000;
        CharLen:=2;
      end else begin
        // invalid character
        Result:=w1;
        CharLen:=1;
      end;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

Function UnicodeToUTF16(u: cardinal): widestring;
Begin
  // u should be <= $10FFFF to fit into UTF-16

  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=system.widechar(u)
  else
    Result:=system.widechar($D800+((u - $10000) shr 10))+system.widechar($DC00+((u - $10000) and $3ff));
End;

{$ENDIF}

Function UnicodeToUCS2(Key:Word):AnsiString;
Var
  A,B:Byte;
Begin
  If (Key<=255) Then
    Result := Char(Key)
  Else
  Begin
    B := Key And $FF;
    A := (Key Shr 8) And $FF;
    Result := #255+Char(A)+Char(B);
  End;
End;

Function ucs2_UpCase(Const C:Word):Word;
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

Function ucs2_LowerCase(Const C:Word):Word;
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

Function ucs2_length(Const s:AnsiString):Integer;
Var
  I:Integer;
Begin
  Result := 0;

  If (S='') Then
    Exit;

  I := 1;
  While I<=Length(S) Do
  Begin
    Inc(Result);
    If (S[I]=#255) Then
      Inc(I, 3)
    Else
      Inc(I);
  End;
End;

Function ucs2_realindex(Const S:AnsiString; Index:Integer):Integer;
Var
  I:Integer;
Begin
  Result := 1;

  If (S='') Then
    Exit;

  I := 1;
  While I<=Length(S) Do
  Begin
    If (Result = Index) Then
    Begin
      Result := I;
      Exit;
    End;

    Inc(Result);
    If (S[I]=#255) Then
      Inc(I, 3)
    Else
      Inc(I);
  End;

  Result := Length(S) + 1;
End;

Function ucs2_char(Const S:AnsiString; Index:Integer):Word;
Var
  I,N:Integer;
Begin
  N := 0;
  Result := 0;

  If (S='') Then
    Exit;

  I := 1;
  While I<=Length(S) Do
  Begin
    Inc(N);
    If (N=Index) Then
    Begin
      If (S[I]=#255) Then
      Begin
        If (I<Length(S)-1) Then
          Result := Ord(S[I+1])*256 + Ord(S[I+2])
        Else
          Result := 0;
      End Else
        Result := Ord(S[I]);

      Exit;
    End;

    If (S[I]=#255) Then
      Inc(I, 3)
    Else
      Inc(I);
  End;
End;

Function ucs2_str(Const S:AnsiString; Index:Integer):AnsiString;
Var
  I,N:Integer;
Begin
  N := 0;
  Result := '';

  If (S='') Then
    Exit;

  I := 1;
  While I<=Length(S) Do
  Begin
    Inc(N);
    If (N=Index) Then
    Begin
      If (S[I]=#255) Then
      Begin
        If (I<Length(S)-1) Then
          Result := #255 + S[I+1] + S[I+2]
        Else
          Result := '';
      End Else
        Result := S[I];

      Exit;
    End;

    If (S[I]=#255) Then
      Inc(I, 3)
    Else
      Inc(I);
  End;
End;

Function ucs2_ascii(Const S:AnsiString; Index:Integer):AnsiChar;
Var
  I,N:Integer;
Begin
  N := 0;
  Result := #0;

  If (S='') Then
    Exit;

  I := 1;
  While I<=Length(S) Do
  Begin
    Inc(N);
    If (N=Index) Then
    Begin
      Result := S[I];
      Exit;
    End;

    If (S[I]=#255) Then
      Inc(I, 3)
    Else
      Inc(I);
  End;
End;

Function ucs2_pos(Const Substr, S:AnsiString):Integer;
Var
  Len, Len2, N, I:Integer;
Begin
  Result := 0;
  Len := ucs2_length(S);
  Len2 := ucs2_length(Substr);
  N := 1;
  For I:=1 To Len Do
  If (Ucs2_char(S, I) = ucs2_char(Substr, N)) Then
  Begin
    If N = 1 Then
      Result := I;

    If N = Len2 Then
      Exit
    Else
      Inc(N);
  End Else
  Begin
    N := 1;
    Result := 0;
  End;
End;

Function ucs2_pos(C:AnsiChar; Const S:AnsiString):Integer;
Var
  Len, I:Integer;
Begin
  Len := ucs2_length(S);
  For I:=1 To Len Do
  If (Ucs2_ascii(S, I) = C) Then
  Begin
    Result := I;
    Exit;
  End;
  Result := 0;
End;

Function ucs2_pos_nocase(Const Substr, S:AnsiString):Integer;
Var
  Len, Len2, N, I:Integer;
Begin
  Result := 0;
  Len := ucs2_length(S);
  Len2 := ucs2_length(Substr);
  N := 1;
  For I:=1 To Len Do
  If (ucs2_Upcase(Ucs2_char(S, I)) = ucs2_Upcase(ucs2_char(Substr, N))) Then
  Begin
    If N = 1 Then
      Result := I;

    If N = Len2 Then
      Exit
    Else
      Inc(N);
  End Else
  Begin
    N := 1;
    Result := 0;
  End;

  Result := 0;
End;

Function ucs2_Copy(Const S:AnsiString; Index, Count:Integer):AnsiString;
Var
  Len, Target, I:Integer;
Begin
  Len := ucs2_length(S);
  If (Count = MaxInt) Then
  Begin
    Target := ucs2_realindex(S, Index);
    Result := Copy(S, Target, MaxInt);
    {For I:=Index To Len Do
      Result := Result + ucs2_str(S, I);}
  End Else
  Begin
    Result := '';
    Dec(Count);
    Target := Index+Count;
    If (Target>Len) Then
      Target := Len;
    For I:=Index To Target Do
      Result := Result + ucs2_str(S, I);
  End;
End;

Function ucs2_GetNextWord(Var S:AnsiString; Separator:AnsiChar=' '):AnsiString;
Var
  I:Integer;
Begin
  S := ucs2_TrimLeft(S);
  If S = '' Then
  Begin
    Result := '';
    Exit;
  End;

  I := ucs2_Pos(Separator,S);
  If I<1 Then
  Begin
    Result := S;
    S := '';
  End Else
  Begin
    Result := ucs2_Copy(S, 1, (I-1));
    S := ucs2_Copy(S, (I+1), MaxInt);
  End;
  S := ucs2_TrimLeft(S);
End;

Function ucs2_TrimLeft(Const S:AnsiString):AnsiString;
Var
  I,L:Integer;
Begin
  L := ucs2_Length(S);
  I := 1;
  While (I <= L) And (ucs2_ascii(S, I)<=' ') Do
    Inc(I);
  Result := ucs2_Copy(S,I,Maxint);
End;

Function ucs2_TrimRight(Const S:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  I := ucs2_Length(S);
  While (I>=1) And (ucs2_ascii(S, I)<=' ') Do
    Dec(I);

  Result := ucs2_Copy(S,1,I);
End;

Function ucs2_Trim(Const S:AnsiString):AnsiString;
Var
  I,J,L:Integer;
Begin
  L := ucs2_Length(S);
  I := 1;
  While (I <= L) And (ucs2_ascii(S, I)<=' ') Do
    Inc(I);

  J := L;
  While (J>=1) And (ucs2_ascii(S, J)<=' ') Do
    Dec(J);

  L := Succ(J-I);
  Result := ucs2_Copy(S, I, L);
End;

//Converts a string to upcase
Function ucs2_UpStr(Const S:AnsiString):AnsiString;
Var
  Len, I:Integer;
Begin
  Result := '';
  Len := ucs2_Length(S);
  For I:=1 To Len Do
    Result := Result + UnicodeToUCS2(ucs2_UpCase(ucs2_char(S, I)));
End;

//Converts a string to lowercase
Function ucs2_LowStr(Const S:AnsiString):AnsiString;
Var
  Len, I:Integer;
Begin
  Result := '';
  Len := ucs2_Length(S);
  For I:=1 To Len Do
    Result := Result + UnicodeToUCS2(ucs2_LowerCase(ucs2_char(S, I)));
End;

Function ucs2_CapStr(Const S:AnsiString):AnsiString;
Begin
  Result := LowStr(S);
  If Result<>'' Then
    Result := UnicodeToUCS2(ucs2_UpCase(ucs2_char(Result, 1))) + ucs2_copy(Result, 2, MaxInt);
End;

//Returns the position of a substring inside of a string
//Search starts from the end of the string.
Function ucs2_PosRev(Const SubStr,Str:AnsiString):Integer;
Var
  Len, Len2, I,K,N:Integer;
Begin
  K := 0;
  Len2 := ucs2_Length(SubStr);
  N := len2;
  Len := ucs2_Length(Str);
  For I:=Len DownTo 11 Do
  Begin
    If ucs2_char(Str,I) = ucs2_char(SubStr, N) Then
    Begin
      If N=1 Then
      Begin
        K := I;
        Break;
      End Else
        Dec(N);
    End Else

    N := Len;
  End;

  Result := K;
End;

Procedure ucs2_ReplaceText(Const Token,Value:AnsiString; Var S:AnsiString);
Var
  I, Len:Integer;
  S2:AnsiString;
Begin
  If (Token = Value) Then
    Exit;

  I := ucs2_Pos_nocase(Token, S);
  If (I>=1) Then
  Begin
    Len := ucs2_Length(Token);
    S2 := ucs2_Copy(S, I+ Len, MaxInt);
    S := ucs2_Copy(S,1,(I-1));
    ucs2_ReplaceText(Token, Value, S2);
    S := S + Value + S2;
  End;
End;

//https://www.google.pt/?gws_rd=ssl#q=pascal+html+encode+string
Function ucs2_encode_html(Const S:AnsiString):AnsiString;
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
  I, Len:Integer;
  C:Word;
  IsSafe:Boolean;
Begin
  Result := '';
  Len := ucs2_Length(S);
  For I:=1 To Len Do
  Begin
    C := ucs2_char(S, I);

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
    (C = Ord('~')) Or (C = Ord('\')) Or (C = Ord('(')) Or (C = Ord(')')) Then
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

End.