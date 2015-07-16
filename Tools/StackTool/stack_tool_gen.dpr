program stack_tool;

{$APPTYPE CONSOLE}

uses SysUtils, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_FileStream, TERRA_MemoryStream;


Const
  MaxBuffer = 16;
Var
  CurBuf:Integer = 0;
  Buffers:Array[0..MaxBuffer] Of AnsiString;


Procedure InitBuffers(Size:Integer);
Var
  I:Integer;
Begin
  For I:=0 To MaxBuffer Do
  Begin
    SEtLength(Buffers[I], Size);
    Buffers[I, 1] := #0;
  End;

  Buffers[MaxBuffer] := #0;
End;

Function PCopy(S:PAnsiChar; Index, Count:Integer):PAnsiChar;
Var
  I:Integer;
Begin
  Result := @Buffers[CurBuf][1];

  If Count=MaxInt Then
  Begin
    I := 0;
    Repeat
      Result[I] := S[I];
      If S[I] = #0 Then
        Break;

      Inc(I);
    Until False;
  End Else
  Begin
    Move(Pointer(Cardinal(S) + Pred(Index))^, Result, Count);
  End;

  Inc(CurBuf);
  If CurBuf>=MaxBuffer Then
    CurBuf := 0;
End;

Function NullString:PAnsiChar;
Begin
  Result := @Buffers[MaxBuffer, 1];
End;

Function Contains(Const SubStr:AnsiString; S:PAnsiChar):Boolean;
Var
  i, N,L:Integer;
begin
  L := Length(SubStr);
  I := 0;
  N := 1;
  Repeat
    If (S[I]= #0) Then
      Break;
    If (S[I]= SubStr[N]) Then
    Begin
      If (N>=L) Then
      Begin
        Result := True;
        Exit;
      End;

      Inc(N);
      Inc(I);

      Continue;
    End;

    Inc(I);
    N := 1;
  Until False;
  Result := False;
End;

Function PTrim(S:PAnsiChar):PAnsiChar;
Begin
  Result := S;
  While Result^<=#32 do
    Inc(Result);
End;

Function Split(Var S:PAnsiChar; C:AnsiChar):PAnsiChar;
Var
  I:Integer;
Begin
  I := 0;
  Repeat
    If (S[I] = #0) Then
    Begin
      Result := S;
      S := NullString;
      Exit;
    End;

    If (S[I] = C) Then
    Begin
      Result := S;
      S[I] := #0;
      S := @S[Succ(I)];
      Exit;
    End;

    Inc(I);
  Until False;
End;

Function ReverseSplit(Var S:PAnsiChar; C:AnsiChar):PAnsiChar;
Var
  I, Last:Integer;
  Temp:PAnsiChar;
Begin
  Last := -1;
  Temp := S;
  I := 0;
  Repeat
    If (S[I] = #0) Then
      Break;

    If (S[I] = C) Then
    Begin
      Last := I;
      S := @S[Succ(I)];
      I := 0;
      Continue;
    End;

    Inc(I);
  Until False;

  Result := Temp;
  If Last>=0 Then
  Begin
    Temp[Last] := #0;
  End Else
  Begin
    S := NullString;
  End;
End;

Function GetID(Var S:PAnsiChar):PAnsiChar;
Var
  I:Integer;
Begin
  Repeat
    Result := Split(S, '$');
  Until ((Result[0]<>#0) And (Result<>'_')) Or (S[0]=#0);

  If Result = '' Then
    Exit;

  I := 0;
  While (Result[I] = '_') And (Result[I]<>#0) Do
    Inc(I);

  Result := @Result[I];

  I := 0;
  While (Result[I]<>#0) Do
    Inc(I);
  Dec(I);
  While (I>0) And (Result[I] = '_') Do
  Begin
    Result[I] := #0;
    Dec(I);
  End;

(*  I := Length(Result);
  While (Result[I] = '_') And (I>1) Do
    Dec(I);

  If I<Length(Result) Then
    Result := PCopy(Result, 1, I);*)
End;

Var
  Units:Array Of String;
  UnitCount:Integer;

Function GetUnitID(Const S:String):Cardinal;
Var I:Integer;
Begin
  For I:=0 tO Pred(UnitCount) Do
  If S = Units[I] Then
  Begin
    Result := I;
    Exit;
  End;

  Result := UnitCount;
  Inc(UnitCount);
  SetLength(Units, UnitCount);
  Units[Pred(UnitCount)] := S;
End;

Var
  Classes:Array Of String;
  ClassCount:Integer;

Function GetClassID(Const S:String):Cardinal;
Var I:Integer;
Begin
  For I:=0 tO Pred(ClassCount) Do
  If S = Classes[I] Then
  Begin
    Result := I;
    Exit;
  End;

  Result := ClassCount;
  Inc(ClassCount);
  SetLength(Classes, ClassCount);
  Classes[Pred(ClassCount)] := S;
End;

Var
  Src, Dest, Temp:Stream;
  Tag, Content:String;

  Address, UnitName, FuncName, ClassName, S, Lines:PAnsiChar;
  ClassID, UnitID, AddressValue:Cardinal;

  ClassHex, UnitHex:String[8];

  I, Count, N:Integer;

  BufLine:String[255];
Begin
  Src := MemoryStream.Create('D:\code\minimonhd\trunk\Android\libs\armeabi-v7a\libterra.map');
  SetLength(Content, Src.Size);
  Src.Read(@Content[1], Src.Size);
  Src.Release();

  Dest := MemoryStream.Create();
  Dest.Encoding := encodingASCII;
  Dest.WriteCardinal(0);

  Tag := '.rel.gnu.linkonce';
  I := Pos(Tag, Content);
  Content := Copy(Content, Length(Tag)+I+1, MaxInt);

  Tag := '.rel.text';
  I := Pos(Tag, Content);
  Content := Copy(Content, Length(Tag)+I+1, MaxInt) + #0;
  Lines :=  @Content[1];

  InitBuffers(Length(Content));

  Count := 0;
  While Lines[1] <>#0 Do
  Begin
    Lines := PTrim(Lines);
    Address := Split(Lines, ' ');
    Lines := PTrim(Lines);
    Split(Lines, ' ');
    Lines := PTrim(Lines);

    Split(Lines, #13);
    S := Split(Lines, #13);
    S := PTrim(S);

    ReverseSplit(S, '.');

    If (S<>'') And (S[0]='n') And (S[1]='_') And (S[2]='p')Then
      S := @S[3]
    Else
    If (S<>'') And (S[0]='n') And (S[1]='_') Then
      S := @S[2];

    UnitName := GetID(S);
    FuncName := S;

    If FuncName[0]=#0 Then
    Begin
      FuncName := UnitName;
      UnitName := 'system';
    End;

    If (FuncName[0] = '*') And (FuncName[1] = ')') Then
      Break;

    I := 0;
    While (FuncName[I]<>#0) Do
    Begin
      If FuncName[I]='_' Then
        FuncName[I] := '.';

      Inc(I);
    End;

    If Contains('JNI.$$.DEF', FuncName) Then
      Continue;

    If Contains('java.com.', FuncName) Then
    Begin
      ClassName := ReverseSplit(FuncName, '.');
    End;

    ClassID := GetClassID(ClassName);
    UnitID := GetUnitID(UnitName);

    Address[0] := ' ';
    Address[1]:= '$';

    (*ClassHex := HexStr(ClassID);
    UnitHex := HexStr(UnitID);

    BufLine := '($'+UnitHex+', $'+ClassHex+', '''+FuncName+''', '+Address+'), '#13#10;
    Buffer.Write(@BufLine[1], Length(BufLine));
    //WriteLn(UnitName,'.', FuncName, ' = ', Address);
    *)

    Val(Address, AddressValue, N);

    Dest.WriteCardinal(AddressValue);
    Dest.WriteCardinal(UnitID);
    Dest.WriteCardinal(ClassID);
    Dest.WriteString(FuncName);

    Inc(Count);

    (*If Count=46 Then
      IntToString(2);*)

    (*If Count>200 Then
      Break;*)
  End;

  //Dest := FileStream.Create('D:\code\minimonhd\trunk\Android\libs\armeabi-v7a\TERRA_RTI.pas');
  Dest.WriteCardinal(UnitCount);
  For I:=0 To Pred(UnitCount) Do
    Dest.WriteString(Units[I]);

  Dest.WriteCardinal(ClassCount);
  For I:=0 To Pred(ClassCount) Do
    Dest.WriteString(Classes[I]);

  Dest.Seek(0);
  Dest.WriteCardinal(Count);

  Temp := FileStream.Create('D:\code\minimonhd\trunk\Android\libs\armeabi-v7a\debug.bin');
  Dest.Copy(Temp);
  Temp.Release();

  Dest.Release();
end.
