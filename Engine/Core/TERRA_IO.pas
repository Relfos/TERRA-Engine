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
 * TERRA_IO
 * Implements generic input/output stream
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_IO;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I terra.inc}
{$ENDIF}

Interface

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  SysUtils, TERRA_Utils;

Const
 // Stream access/permission flags
  smRead    = 1;
  smWrite   = 2;
  smDynamic = 4;
  smShared  = 8;
  smAppend  = 16;
  smLargeAlloc = 32;
  smDefault = 7; //lfRead Or lfWrite or lfDynamic

  seASCII   = 0;
  seUCS2_LE = 1;
  seUCS2_BE = 2;
  seUTF8    = 3;

Type
  Stream = Class(TERRAObject)
     Protected
      _Pos:Cardinal;
      _Size:Cardinal;
      _Mode:Integer;
      _Name:AnsiString;
      _Encoding:Integer;

      Function GetEOF:Boolean;Virtual;

     Public
      Constructor Create(StreamMode:Integer=smDefault);
      Destructor Destroy; Override;

      Function Read(Data:Pointer; Length:Cardinal):Cardinal; Virtual;
      Function Write(Data:Pointer; Length:Cardinal):Cardinal; Virtual;

      Procedure ReadString(Var S:AnsiString; NullTerminated:Boolean = False);Virtual;
      Procedure WriteString(S:AnsiString; NullTerminated:Boolean = False);Virtual;

      Procedure ReadLine(Var S:AnsiString); Virtual;
      Procedure WriteLine(Const S:AnsiString=''); Virtual;
      Procedure WriteChars(Const S:AnsiString); Virtual;

      Procedure ReadLines(Var S:AnsiString); 

      Procedure ReadUnicodeLine(Var S:AnsiString);
      Procedure WriteUnicodeLine(Const S:AnsiString; Encoding:Integer);
      Procedure WriteUnicodeChars(Const S:AnsiString; Encoding:Integer);

      Procedure Copy(Dest:Stream);Overload;
      Procedure Copy(Dest:Stream;Offset,Count:Integer);Overload;
      Procedure CopyText(Dest:Stream);

      Procedure Seek(NewPosition:Cardinal);Virtual;
      Procedure Skip(Size:Integer);Virtual;
      Procedure Truncate;Virtual;

      Property Position:Cardinal Read _Pos Write Seek;
      Property Size:Cardinal Read _Size;

      Property Mode:Integer Read _Mode;

      Property EOF:Boolean Read GetEOF;

      Property Name:AnsiString Read _Name Write _Name;
      Property Encoding:Integer Read _Encoding;
     End;


  MemoryStream = Class(Stream)
    Protected
      _Buffer:PByte;

    Public
      Constructor Create(BufferSize:Integer; Buffer:PByte = Nil; StreamMode:Integer=smDefault); Overload;
      Constructor Create(FileName:AnsiString; StreamMode:Integer=smDefault);Overload;
      {$IFDEF OXYGENE}
      Procedure Destroy;Override;
      {$ELSE}
      Destructor Destroy;Override;
      {$ENDIF}

      Procedure SetBuffer(BufferSize:Integer; Buffer:PByte);

      Procedure Resize(NewSize:Integer);

      Function Read(Data:Pointer; Length:Cardinal):Cardinal; Override;
      Function Write(Data:Pointer; Length:Cardinal):Cardinal; Override;
      Procedure Truncate;Override;
      Procedure Seek(NewPosition:Cardinal);Override;

      Property Buffer:PByte Read _Buffer;
     End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_FileIO, TERRA_OS;

// Stream Object

Constructor Stream.Create(StreamMode:Integer=smDefault);
Begin
  _Name:='';
  _Mode:=StreamMode;
  _Pos:=0;
End;

Destructor Stream.Destroy;
Begin
  // do nothing
End;

Procedure Stream.Copy(Dest:Stream);
Var
 Count,BytesRead:Integer;
 Buffer:PByte;
 BufferSize:Integer;
 BlockSize:Integer;
 A,B:Integer;
Begin
  Seek(0);
  Count:=Self.Size;
  If (Dest.Size-Dest.Position<Count)And(Dest.Mode And smDynamic=0) Then
    Count:=Dest.Size-Dest.Position;

  BufferSize:=65534;
  If Count<BufferSize Then
    BufferSize:=Count;

  {$IFDEF OXYGENE}
  Buffer := new Byte[BufferSize];
  {$ELSE}
  GetMem(Buffer,BufferSize);
  {$ENDIF}

  BytesRead:=0;
  While BytesRead<Count Do
  Begin
    A:=Self.Size-Self.Position;
    B:=Dest.Size-Dest.Position;
    If Dest.Mode And smDynamic<>0 Then
      B:=A;

    BlockSize:=IntMin(IntMin(BufferSize,Count-BytesRead), IntMin(A,B));
    Read(Buffer, BlockSize);

    Dest.Write(Pointer(Buffer), BlockSize);
    Inc(BytesRead,BlockSize);
  End;

  {$IFDEF OXYGENE}
  Buffer := Nil;
  {$ELSE}
  FreeMem(Buffer,BufferSize);
  {$ENDIF}
End;

Procedure Stream.Copy(Dest:Stream;Offset,Count:Integer);
Var
  BytesRead:Integer;
  Buffer:PByte;
  BufferSize:Integer;
  BlockSize:Integer;
  A,B:Integer;
Begin
  Seek(Offset);
  If (Dest.Size-Dest.Position<Count)And(Dest.Mode And smDynamic=0) Then
    Count:=Dest.Size-Dest.Position;

  BufferSize:=65534;
  If Count<BufferSize Then
    BufferSize:=Count;

    {$IFDEF OXYGENE}
    Buffer := new Byte[BufferSize];
    {$ELSE}
  GetMem(Buffer,BufferSize);
    {$ENDIF}

  BytesRead:=0;
  While BytesRead<Count Do
  Begin
    A:=Self.Size-Self.Position;

    If A=0 Then
    Begin
      RaiseError('Buffer too small.');
      Exit;
    End;

    B:=Dest.Size-Dest.Position;
    If Dest.Mode And smDynamic<>0 Then
      B:=A;

    BlockSize:=IntMin(IntMin(BufferSize,Count-BytesRead), IntMin(A,B));
    Read(Buffer, BlockSize);

    Dest.Write(Pointer(Buffer), BlockSize);
    Inc(BytesRead,BlockSize);
  End;

{$IFDEF OXYGENE}
    Buffer := nil;
{$ELSE}
  FreeMem(Buffer,BufferSize);
{$ENDIF}
End;

Procedure Stream.CopyText(Dest:Stream);
Var
  C:AnsiChar;
  S:AnsiString;
Begin
  S:='';
  While Self.Position<Self.Size Do
  Begin
    Read(@C, 1);
    If (C=#10) Then
      Dest.WriteString(S)
    Else
    S:=S+C;
  End;
End;

Procedure Stream.Seek(NewPosition:Cardinal);
Begin
  _Pos := NewPosition;
End;

Procedure Stream.Skip(Size:Integer);
Begin
  If Size=0 Then
    Exit;

  Seek(_Pos+Size);
End;

Procedure Stream.Truncate;
Begin
  Log(logWarning,'IO','Method not supported in this stream.');
End;

Function Stream.Read(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result := 0;
End;

Function Stream.Write(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result := 0;
End;


Procedure Stream.ReadString(Var S:AnsiString; NullTerminated:Boolean = False);
Var
{$IFDEF OXYGENE}
  C:AnsiChar;
  I:Integer;
{$ENDIF}
  Len:Word;
  N:Byte;
Begin
  If (Not NullTerminated) Then
  Begin
    Read(@N, 1);
    If N=255 Then
      Read(@Len, 2)
    Else
      Len:=N;

    {$IFDEF OXYGENE}
    If (Len>0) Then
        S := new String('0', Len)
    Else
        S := nil;
    For I:=0 To (Len-1) Do
    Begin
        Read(@C, 1);
        S[I] := C;
    End;
    {$ELSE}
    SetLength(S,Len);
    If Len>0 Then
      Read(@(S[1]),Len);
    {$ENDIF}
  End Else
  Begin
    S := '';
    Repeat
      Read(@N, 1);
      If (N=0) Then
        Break;
      S := S + Chr(N);
    Until (False);
  End;
End;

Procedure Stream.WriteString(S:AnsiString; NullTerminated:Boolean = False);
Var
  Len:Word;
  N:Byte;
{$IFDEF OXYGENE}
  I:Integer;
  C:AnsiChar;
{$ENDIF}
Begin
  Len := Length(S);
  If (NullTerminated) Then
  Begin
    {$IFDEF OXYGENE}
    For I:=0 To (Len-1) Do
    Begin
        C := S[I];
        Write(Pointer(@C), 1);
    End;
    {$ELSE}
    Write(@S[1], Len);
    {$ENDIF}
    N := 0;
    Write(Pointer(@N), 1);
  End Else
  Begin
    If Len<255 Then
      N:=Len
    Else
      N:=255;
    Write(Pointer(@N), 1);

    If Len>=255 Then
      Write(Pointer(@Len), 2);

    {$IFDEF OXYGENE}
    For I:=0 To (Len-1) Do
    Begin
        C := S[I];
        Write(Pointer(@C), 1);
    End;
    {$ELSE}
    If Len>0 Then
      Write(@S[1], Len);
    {$ENDIF}
  End;
End;

Procedure Stream.WriteChars(Const S:AnsiString);
{$IFDEF OXYGENE}
Var
  C:AnsiChar;
  I:Integer;
{$ENDIF}
Begin
  {$IFDEF OXYGENE}
  For I:=0 To (S.Length-1) Do
  Begin
    C := S[I];
    Write(Pointer(@C), 1);
  End;
  {$ELSE}
  Write(@S[1],Length(S));
  {$ENDIF}
End;

Procedure Stream.WriteLine(Const S:AnsiString);
Begin
  WriteChars(S);
  WriteChars(#13#10);
End;

Procedure Stream.ReadLine(Var S:AnsiString);
Var
  C:AnsiChar;
Begin
  S:='';
  C:=#0;
  While (C<>#10)And(Position<Size) Do
  Begin
    Read(@C, 1);
    If (C<>#10)Or(C<>#13) Then
    S:=S+C;
  End;
  S:=TrimRight(S);
End;

Function Stream.GetEOF:Boolean;
Begin
  Result:=Position>=Size;
End;

Procedure Stream.ReadLines(Var S:AnsiString);
Var
  S2:AnsiString;
Begin
  S := '';
  S2 := '';
  While Not Self.EOF Do
  Begin
    Self.ReadLine(S2);
    S := S + S2 + CrLf;
  End;
End;

Procedure Stream.WriteUnicodeLine(Const S:AnsiString; Encoding: Integer);
Begin
  WriteUnicodeChars(S, Encoding);
  WriteUnicodeChars(#13#10, Encoding);
End;

Procedure Stream.WriteUnicodeChars(Const S:AnsiString; Encoding: Integer);
Var
  I:Integer;
  A,B:Byte;
  W:Word;
Begin
  Case Encoding Of
  seASCII:
    Begin
      Self.Write(@S[1], Length(S));
    End;

  seUTF8:
    Begin
      RaiseError('Write.Unicode.UTF8: Not implemented!');
    End;

  seUCS2_LE:
    Begin
      I:=1;
      While I<=Length(S) Do
      Begin
        If (S[I]<#255) Then
        Begin
          A := 0;
          B := Byte(S[I]);
          Inc(I);
        End Else
        Begin
          Inc(I); A := Byte(S[I]);
          Inc(I); B := Byte(S[I]);
          Inc(I);
        End;

        W := (A Shl 8) + B;
        Self.Write(@W, 2);
      End;
    End;

    seUCS2_BE:
    Begin
      I:=1;
      While I<=Length(S) Do
      Begin
        If (S[I]<#255) Then
        Begin
          A := 0;
          B := Byte(S[I]);
          Inc(I);
        End Else
        Begin
          Inc(I); A := Byte(S[I]);
          Inc(I); B := Byte(S[I]);
          Inc(I);
        End;

        W := (B Shl 8) + A;
        Self.Write(@W, 2);
      End;
    End;
  End;
End;

Procedure Stream.ReadUnicodeLine(Var S:AnsiString);
Var
  W:Word;
  A,B,C:Byte;

Procedure GetNextTwoChars();
Begin
  Self.Read(@W, 2);

  If (_Encoding = seUCS2_LE) Then
  Begin
    B := W And $FF;
    A := (W Shr 8) And $FF;
  End Else
  Begin
    A := W And $FF;
    B := (W Shr 8) And $FF;
  End;
End;

Begin
  S := '';

  If (Self.Position=0) Then
  Begin
    Self.Read(@A, 1);
    Self.Read(@B, 1);
    S := Char(A) + Char(B);

    _Encoding := seASCII;
    If (A = $FF) And (B = $FE) Then
      _Encoding := seUCS2_LE
    Else
    If (A = $FE) And (B = $FF) Then
      _Encoding := seUCS2_BE
    Else
    If (A = $EF) And (B = $BB) Then
    Begin
      Self.Read(@C, 1);
      If (C = $BF) Then
        _Encoding := seUTF8;

      S := S + Char(C);
    End;

    If (_Encoding<>seASCII) Then
      S := '';
  End;
  
  A := 0;
  B := 0;

  While (Not Self.EOF) Do
  Begin

    Case _Encoding Of
    seASCII:
      Begin
        Self.Read(@A, 1);
        If (A = 13) Then
        Begin
          Self.Read(@B, 1);
          If (B<>10) Then
            Self.Skip(-1);

          Break;
        End Else
        If (A=10) Then
        Begin
          Break;
        End Else
          S := S + Char(A);
      End;

    seUTF8:
      Begin
        Self.Read(@A, 1);
        If (A = 13) Or (A=10) Then
          Break;

        If (A<$80) Then
        Begin
          S := S + Char(A);
          Continue;
        End;

        If ((A And $E0) = $E0) Then
        Begin
          Self.Read(@B, 1);
          Self.Read(@C, 1);
          if (B = 0) Or (C = 0) Then
            Continue;

          W := ((A And $0F) Shl 12) Or ((B And $3F) Shl 6) Or (C And $3F);
          B := W And $FF;
          A := (W Shr 8) And $FF;
          If (B=13) And (A=0) Then
            Break
          Else
          If (A=0) Then
            S := S + Char(B)
          Else
            S := S + #255+Char(A)+Char(B);
          Continue;
        End;

        If ((A And $C0) = $C0) Then
        Begin
          Self.Read(@B, 1);
          If (B = 0) Then
            Continue;
          W := ((A And $1F) Shl 6) Or (B And $3F);
          B := W And $FF;
          A := (W Shr 8) And $FF;
          If (B=13) And (A=0) Then
            Break
          Else
          If (A=0) Then
            S := S + Char(B)
          Else
            S := S + #255+Char(A)+Char(B);
        End;
      End;

    seUCS2_LE,
    seUCS2_BE:
      Begin
        GetNextTwoChars();

        If (A=32) And (B=11) Then // invisible space
        Begin
          IntToString(2);
        End Else
        Begin
          If (B=13) And (A=0) Then
          Begin
            GetNextTwoChars();

            If (B=10) And (A=0) Then
              IntToString(2)
            Else
              Self.Skip(-2);
              
            Break;
          End;

          If (B=10) And (A=0) Then
          Begin
            IntToString(2);
            Break;
          End;

          If (A=0) Then
             S := S + Char(B)
          Else
            S := S + #255+Char(A)+Char(B);
        End;
      End;

    End;
  End;
End;

// MemoryStream object
Constructor MemoryStream.Create(BufferSize:Integer; Buffer:PByte; StreamMode:Integer);
Begin
  _Size := BufferSize;
  _Pos := 0;

{  If (StreamMode And smDynamic<>0) Then
    StreamMode:=StreamMode Xor smDynamic;}

  If (Buffer = Nil) Then
  Begin
    If (StreamMode And smShared<>0) Then
      StreamMode:=StreamMode Xor smShared;

    If (_Size>0) Then
      GetMem(_Buffer, _Size);
  End Else
  Begin
    _Buffer := Buffer;

    If (StreamMode And smShared=0) Then
      StreamMode := StreamMode Or smShared;
  End;

  Inherited Create(StreamMode);
End;

Constructor MemoryStream.Create(FileName:AnsiString; StreamMode:Integer=smDefault);
Var
  F:FileStream;
Begin
  Inherited Create(StreamMode);

  F := FileStream.Open(FileName);
  _Size := F.Size;

  Create(_Size, Nil, StreamMode);

  Log(logDebug, 'IO', 'Loaded memory stream from '+FileName+', size = '+IntToString(_Size));

  F.Read(_Buffer, _Size);
  F.Destroy;

  _Name := FileName;
End;

{$IFDEF OXYGENE}
Procedure MemoryStream.Destroy;
{$ELSE}
Destructor MemoryStream.Destroy;
{$ENDIF}
Begin
  If (_Mode And smShared=0) And (Assigned(_Buffer)) Then
  Begin
    //Log.Write(logDebug,'IO','MemoryStream.Destroy','Releasing '+MemStr(Size));
    FreeMem(_Buffer);
  End;

  _Size := 0;
  _Buffer:=Nil;
End;

Function MemoryStream.Read(Data:Pointer; Length:Cardinal):Cardinal;
Var
  P, P2:PByte;
  I:Integer;
Begin
  Result := 0;
  
  If (Length=0) Then
  Begin
    Exit;
  End;

  //Log(logDebug,'FileIO', 'Reading '+IntToString(Length)+' bytes from '+Self._Name);

  If Not Assigned(_Buffer) Then
  Begin
    RaiseError('Buffer not assigned.');
    Exit;
  End;

  If (_Pos>=_Size) Then
  Begin
    {$IFDEF PC}
    RaiseError('Cannot read from memory in '+Self._Name+' ('+IntToString(_Pos)+'/'+IntToString(_Size)+')');
    {$ENDIF}
    FillChar(Data^, Length, 0);
    Result := 0;
    Exit;
  End;

  If (_Pos+Length>_Size)Then
    Length:=_Size-_Pos;

  If (Length<=0) Then
    Exit;
    
  P := _Buffer;
  Inc(P, _Pos);
  P2 := PByte(Data);
  For I:=0 To (Length-1) Do
  Begin
    P2^ := P^;
    Inc(P2);
    Inc(P);
  End;

  Inc(_Pos,Length);
  Result := Length;
End;

Function MemoryStream.Write(Data:Pointer; Length:Cardinal):Cardinal;
Var
  I:Integer;
  P,P2:PByte;
  NewSize:Integer;
Begin
  Result := 0;

  If (_Pos+Length>_Size) Then
  Begin
    If (_Mode And smDynamic=0) Then
    Begin
      RaiseError('Cannot write to memory.');
      Exit;
    End Else
    Begin
      If (_Mode And smLargeAlloc<>0) Then
        NewSize := _Size * 2
      Else
        NewSize := _Size;

      If (_Size + Length>NewSize) Then
        NewSize := _Size + Length;

      Resize(NewSize);
    End;
  End;

  If Not Assigned(_Buffer) Then
  Begin
    RaiseError('Buffer not assigned.');
    Exit;
  End;

  If (_Pos+Length>_Size)Then
    Length := _Size-_Pos;

  If (Length<=0) Then
    Exit;

  P := _Buffer;
  Inc(P, _Pos);
  P2 := PByte(Data);
  For I:=0 To (Length-1) Do
  Begin
    P^ := P2^;
    Inc(P2);
    Inc(P);
  End;
  Inc(_Pos, Length);
  Result:=Length;
End;

Procedure MemoryStream.Seek(NewPosition:Cardinal);
Begin
  If NewPosition>_Size Then
  Begin
    RaiseError('Cannot seek in memory.');
    Exit;
  End;

  _Pos := NewPosition;
End;

Procedure MemoryStream.Truncate;
Var
  Temp:PByte;
Begin
  Temp := _Buffer;

  GetMem(_Buffer, _Pos);
  Move(Temp^, _Buffer^, _Pos);
  FreeMem(Temp);
  
  _Size := _Pos;
End;

Procedure MemoryStream.Resize(NewSize:Integer);
Var
  I, Min:Integer;
  Src, Dest:PByte;
  Ptr:Pointer;
Begin
  If (NewSize = Self.Size) Then
    Exit;

  GetMem(Ptr, NewSize);

  If (Assigned(_Buffer)) And (NewSize>0) Then
  Begin
    Src := _Buffer;
    Dest := Ptr;
    Min := IntMin(_Size, NewSize);

    For I:=0 To Pred(Min) Do
    Begin
      Dest^ := Src^;
      Inc(Dest);
      Inc(Src);
    End;

    FreeMem(_Buffer, _Size);
  End;

  _Size := NewSize;
  _Buffer := Ptr;
End;

Procedure MemoryStream.SetBuffer(BufferSize:Integer;Buffer:PByte);
Begin
  _Size := BufferSize;
  _Buffer := Buffer;
  _Pos := 0;
End;

End.
