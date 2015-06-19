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
 * TERRA_Stream
 * Implements generic input/output stream
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_Stream;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I terra.inc}
{$ENDIF}

Interface

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_FileUtils, TERRA_String;

Const
 // Stream access/permission flags
  smRead    = 1;
  smWrite   = 2;
  smDynamic = 4;
  smShared  = 8;
  smAppend  = 16;
  smLargeAlloc = 32;
  smDefault = smRead Or smWrite or smDynamic;

  EOL_Unix = 0;
  EOL_Windows = 1;

Type
  Stream = Class(TERRAObject)
     Protected
      _Pos:Cardinal;
      _Size:Cardinal;
      _Mode:Integer;
      _Name:TERRAString;
      _Encoding:StringEncoding;
      _EOL:Integer;

      Function GetEOF:Boolean;Virtual;

      Procedure ReadBOM();

     Public
      Constructor Create(StreamMode:Integer=smDefault);
      Procedure Release; Override;

      Function Read(Data:Pointer; Length:Cardinal):Cardinal; Virtual;
      Function Write(Data:Pointer; Length:Cardinal):Cardinal; Virtual;

      Function WriteShortInt(Const Value:ShortInt):Boolean; Virtual;
      Function WriteByte(Const Value:Byte):Boolean; Virtual;
      Function WriteChar(Const Value:TERRAChar):Boolean; Virtual;
      Function WriteSmallInt(Const Value:SmallInt):Boolean; Virtual;
      Function WriteWord(Const Value:Word):Boolean; Virtual;
      Function WriteInteger(Const Value:Integer):Boolean; Virtual;
      Function WriteCardinal(Const Value:Cardinal):Boolean; Virtual;
      Function WriteSingle(Const Value:Single):Boolean; Virtual;
      Function WriteBoolean(Const Value:Boolean):Boolean; Virtual;
      //Function WriteDouble(Const Value:Double):Boolean; Virtual;

      Function ReadShortInt(Out Value:ShortInt):Boolean; Virtual;
      Function ReadByte(Out Value:Byte):Boolean; Virtual;
      Function ReadChar(Out Value:TERRAChar):Boolean; Virtual;
      Function ReadSmallInt(Out Value:SmallInt):Boolean; Virtual;
      Function ReadWord(Out Value:Word):Boolean; Virtual;
      Function ReadInteger(Out Value:Integer):Boolean; Virtual;
      Function ReadCardinal(Out Value:Cardinal):Boolean; Virtual;
      Function ReadSingle(Out Value:Single):Boolean; Virtual;
      Function ReadBoolean(Out Value:Boolean):Boolean; Virtual;

      Procedure ReadString(Out S:TERRAString; NullTerminated:Boolean = False);Virtual;
      Procedure WriteString(Const S:TERRAString; NullTerminated:Boolean = False);Virtual;

      Function ReadHeader(Out S:FileHeader):Boolean; Virtual;
      Function WriteHeader(Const S:FileHeader): Boolean; Virtual;

      Procedure ReadLine(Var S:TERRAString); Virtual;
      Procedure WriteLine(Const S:TERRAString=''); Virtual;
      Procedure WriteChars(Const S:TERRAString); Virtual;

      Procedure ReadLines(Var S:TERRAString);

      Procedure ReadContent(Out S:TERRAString);

      Procedure WriteBOM(Encoding:StringEncoding);

      Procedure Copy(Dest:Stream);Overload;
      Procedure Copy(Dest:Stream;Offset,Count:Integer);Overload;
      //Procedure CopyText(Dest:Stream);

      Procedure Seek(NewPosition:Cardinal);Virtual;
      Procedure Skip(Size:Integer);Virtual;
      Procedure Truncate;Virtual;

      Property Position:Cardinal Read _Pos Write Seek;
      Property Size:Cardinal Read _Size;

      Property Mode:Integer Read _Mode;

      Property EOF:Boolean Read GetEOF;

      Property EOL:Integer Read _EOL Write _EOL;

      Property Name:TERRAString Read _Name Write _Name;
      Property Encoding:StringEncoding Read _Encoding Write _Encoding;
     End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS;

// Stream Object

Constructor Stream.Create(StreamMode:Integer=smDefault);
Begin
  _Name := '';
  _Mode := StreamMode;
  _Pos := 0;
  _Encoding := encodingUnknown;
  _EOL := EOL_Unix;
End;

Procedure Stream.Release;
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
  Buffer:PByteArray;
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

{Procedure Stream.CopyText(Dest:Stream);
Var
  C:TERRAChar;
  S:TERRAString;
Begin
  S:='';
  While Self.Position<Self.Size Do
  Begin
    Read(@C, 1);
    If (C = Ord(#10)) Then
      Dest.WriteString(S)
    Else
    S:=S+C;
  End;
End;}

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


Procedure Stream.ReadString(Out S:TERRAString; NullTerminated:Boolean = False);
Var
{$IFDEF OXYGENE}
  C:TERRAChar;
  I:Integer;
{$ENDIF}
  Encoding:Byte;
  Len:Word;
  C:TERRAChar;
Begin
  S := '';

  If (Not NullTerminated) Then
  Begin
    ReadWord(Len);
    If (Len<=0) Then
      Exit;

    ReadByte(Encoding);
    If Encoding <> Byte(CurrentStringEncoding) Then
    Begin
      Log(logError, 'IO', 'Unsupported binary string encoding in '+Self.Name);
      Exit;
    End;

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
      ReadChar(C);
      If (C = NullChar) Then
        Break;

      StringAppendChar(S, C);
    Until (False);
  End;
End;

Procedure Stream.WriteString(Const S:TERRAString; NullTerminated:Boolean = False);
Var
  Len:Word;
{$IFDEF OXYGENE}
  I:Integer;
  C:Byte;
{$ENDIF}
Begin
  Len := Length(S);
  If (Not NullTerminated) Then
  Begin
    WriteWord(Len);
    If Len<=0 Then
      Exit;
      
    WriteByte(Byte(CurrentStringEncoding));
  End;

  {$IFDEF OXYGENE}
  For I:=0 To (Len-1) Do
  Begin
    C := Byte(S[I]);
    WriteByte(C);
  End;
  {$ELSE}
  If Len>0 Then
    Write(@S[1], Len);
  {$ENDIF}

  If (NullTerminated) Then
    WriteByte(0);
End;

Procedure Stream.WriteChars(Const S:TERRAString);
Var
  It:StringIterator;
Begin
  If S = '' Then
    Exit;

  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    Self.WriteChar(It.GetNext());
  End;
End;

Procedure Stream.WriteLine(Const S:TERRAString);
Begin
  WriteChars(S);

  If _EOL = EOL_Windows Then
    WriteChars(#13#10)
  Else
    WriteChar(NewLineChar);
End;

Procedure Stream.ReadLine(Var S:TERRAString);
Var
  C:TERRAChar;
  Temp:Cardinal;
Begin
  S :='';
  C := NullChar;
  While (Position<Size) Do
  Begin
    ReadChar(C);

    If (C = NewLineChar) Then
    Begin
      Break;
    End Else
      StringAppendChar(S, C);
  End;
End;

Function Stream.GetEOF:Boolean;
Begin
  Result:=Position>=Size;
End;

Procedure Stream.ReadLines(Var S:TERRAString);
Var
  S2:TERRAString;
Begin
  S := '';
  S2 := '';
  While Not Self.EOF Do
  Begin
    Self.ReadLine(S2);
    S := S + S2 + StringFromChar(NewLineChar);
  End;
End;

{Procedure Stream.WriteUnicodeLine(Const S:TERRAString; Encoding: Integer);
Begin
  WriteUnicodeChars(S, Encoding);
  WriteUnicodeChars(#13#10, Encoding);
End;

Procedure Stream.WriteUnicodeChars(Const S:TERRAString; Encoding: Integer);
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    C := It.GetNext();
    Self.WriteChar(C);
  End;
End;

Procedure Stream.ReadUnicodeLine(Var S:TERRAString);
Var
  C:TERRAChar;
Begin
  S := '';

  While (Not Self.EOF) Do
  Begin
    Self.ReadChar(C);

    If (C = NewLineChar) Then
      Break;

    S := S + C;
  End;
End;}

Procedure Stream.WriteBOM(Encoding:StringEncoding);
Begin
  _Encoding := Encoding;
  Case  _Encoding Of
  encodingUCS2LE:
    Begin
      Self.WriteByte($FF);
      Self.WriteByte($FE);
    End;

  encodingUCS2BE:
    Begin
      Self.WriteByte($FE);
      Self.WriteByte($FF);
    End;

  encodingUTF8:
    Begin
      Self.WriteByte($EF);
      Self.WriteByte($BB);
      Self.WriteByte($BF);
    End;
  End;
End;

Procedure Stream.ReadBOM();
Var
  Temp:Cardinal;
  A, B, C:Byte;
Begin
  Temp := Self.Position;

  Self.ReadByte(A);
  Self.ReadByte(B);

  _Encoding := encodingASCII;

  If (A = $FF) And (B = $FE) Then
    _Encoding := encodingUCS2LE
  Else
  If (A = $FE) And (B = $FF) Then
    _Encoding := encodingUCS2BE
  Else
  If (A = $EF) And (B = $BB) Then
  Begin
    Self.ReadByte(C);
    If (C = $BF) Then
      _Encoding := encodingUTF8;
  End;

  If (_Encoding = encodingASCII) Then
    Self.Seek(Temp);
End;

Function Stream.ReadChar(Out Value: TERRAChar): Boolean;
Var
  W:Word;
  A,B,C,D:Byte;

Procedure GetNextTwoChars();
Begin
  Self.ReadWord(W);

  If (_Encoding = encodingUCS2BE) Then
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
  Result := True;

  If (Self.Position = 0) And (_Encoding = encodingUnknown) Then
    Self.ReadBOM();

  A := 0;
  B := 0;

  If _Encoding = encodingASCII Then
  Begin
    Self.ReadByte(A);
    If (A = NewLineChar) Then
    Begin
      If (Not Self.EOF) Then
      Begin
        Self.ReadByte(B);
        If (B<>10) Then
          Self.Skip(-1);
      End;

      Value := NewLineChar;
    End Else
    If (A=10) Then
    Begin
      Value := NewLineChar;
    End Else
      Value := A;

    Exit;
  End;

  A := 0;
  B := 0;

  If _Encoding = encodingUTF8 Then
  Begin
    Self.ReadByte(A);

    If (A<$80) Then
    Begin
      Value := A;
      Exit;
    End;

    If ((A And $F0)=$F0) Then
    Begin
      ReadByte(B);
      ReadByte(C);
      ReadByte(D);
      If (B = 0) Or (C = 0) Or (D = 0) Then
      Begin
        Value := NullChar;
        Log(logError, 'UTF8', 'Decoding error #1');
        Exit;
      End;

      Value := ((A And $0F) Shl 24) Or ((B And $0F) Shl 12) Or ((C And $3F) Shl 6) Or (D And $3F);
    End Else
    If ((A And $E0)=$E0) Then
    Begin
      ReadByte(B);
      ReadByte(C);
      If (B = 0) Or (C = 0) Then
      Begin
        Value := NullChar;
        Log(logError, 'UTF8', 'Decoding error #2');
        Exit;
      End;

      Value := ((A And $0F) Shl 12) Or ((B And $3F) Shl 6) Or (C And $3F);
    End Else
    If ((A And $C0)=$C0) Then
    Begin
      ReadByte(B);
      If (B = 0) Then
      Begin
        Value := NullChar;
        Log(logError, 'UTF8', 'Decoding error #3');
        Exit;
      End;

      Value := ((A And $1F) Shl 6) Or (B And $3F);
    End Else
    Begin
      Value := A;
      Log(logError, 'UTF8', 'Decoding error #4');
    End;

    Exit;
  End;

  If (_Encoding =encodingUCS2LE) Or (_Encoding = encodingUCS2BE) Then
  Begin
    GetNextTwoChars();

    If (A=10) And (B=0) Then
    Begin
      A := 13;
    End Else
    If (A=13) And (B=0) Then
    Begin
      GetNextTwoChars();

      If (A=10) And (B=0) Then
      Begin
        A := 13;
      End Else
        Self.Skip(-2);
    End;

    If (A = 13) Then
    Begin
      Value := NewLineChar;
    End Else
    If (A=32) And (B=11) Then // invisible space
    Begin
      Result := Self.ReadChar(Value);
      Exit;
    End Else
    If (A=0) Then
    Begin
      Value := B;
    End Else
    Begin
      Value := BytesToChar(A, B);
    End;

    Exit;
  End;

  Result := False;
End;

Function Stream.ReadByte(Out Value:Byte):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 1)>0;
End;

Function Stream.ReadWord(Out Value: Word):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 2)>0;
End;

Function Stream.ReadCardinal(Out Value: Cardinal):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 4)>0;
End;

Function Stream.ReadShortInt(Out Value: ShortInt):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 1)>0;
End;

Function Stream.ReadSmallInt(Out Value: SmallInt):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 2)>0;
End;

Function Stream.ReadInteger(Out Value: Integer):Boolean;
Begin
  Value := 0;
  Result := Self.Read(@Value, 4)>0;
End;

Function Stream.ReadSingle(Out Value: Single):Boolean;
Begin
  Value := 0.0;
  Result := Self.Read(@Value, 4)>0;
End;

Function Stream.ReadBoolean(Out Value: Boolean):Boolean;
Begin
  Value := False;
  Result := Self.Read(@Value, 1)>0;
End;

Function Stream.WriteByte(const Value: Byte): Boolean;
Begin
  Result := Self.Write(@Value, 1)>0;
End;

Function Stream.WriteCardinal(const Value: Cardinal): Boolean;
Begin
  Result := Self.Write(@Value, 4)>0;
End;

Function Stream.WriteInteger(const Value: Integer): Boolean;
Begin
  Result := Self.Write(@Value, 4)>0;
End;

Function Stream.WriteShortInt(const Value: ShortInt): Boolean;
Begin
  Result := Self.Write(@Value, 1)>0;
End;

Function Stream.WriteSingle(const Value: Single): Boolean;
Begin
  Result := Self.Write(@Value, 4)>0;
End;

Function Stream.WriteSmallInt(const Value: SmallInt): Boolean;
Begin
  Result := Self.Write(@Value, 2)>0;
End;

Function Stream.WriteWord(const Value: Word): Boolean;
Begin
  Result := Self.Write(@Value, 2)>0;
End;

Function Stream.WriteBoolean(const Value: Boolean): Boolean;
Begin
  Result := Self.Write(@Value, 1)>0;
End;

Function Stream.WriteChar(const Value: TERRAChar): Boolean;
Var
  A,B, C:Byte;
  W:Word;
Begin
  Result := False;

  If (Encoding = encodingUnknown) Then
  Begin
    Encoding := encodingASCII;
  End;

  Case Encoding Of
  encodingASCII:
    Begin
      B := CharToByte(Value);
      Result := Self.WriteByte(B);
    End;

  encodingUTF8:
    Begin
      RaiseError('Write.Unicode: UTF8 support not implemented!');
    End;

  encodingUCS2LE:
    Begin
      CharToBytes(Value, A,B);
      W := (A Shl 8) + B;
      Result := Self.WriteWord(W);
    End;

  encodingUCS2BE:
    Begin
      CharToBytes(Value, A,B);
      W := (B Shl 8) + A;
      Result := Self.WriteWord(W);
    End;

  Else
    Begin
      RaiseError('Write.Unicode: Not supported encoding!');
    End;
  End;
End;

Function Stream.ReadHeader(out S:FileHeader): Boolean;
Var
  I:Integer;
Begin
  For I:=1 To 4 Do
  Begin
    Result := Self.ReadByte(Byte(S[I]));
    If Not Result  Then
      Exit;
  End;
End;

Function Stream.WriteHeader(const S:FileHeader): Boolean;
Var
  I:Integer;
Begin
  For I:=1 To 4 Do
  Begin
    Result := Self.WriteByte(Byte(S[I]));
    If Not Result  Then
      Exit;
  End;
End;


Procedure Stream.ReadContent(Out S:TERRAString);
Begin
  S := '';
  SetLength(S, Self.Size);
  Self.Seek(0);
  Self.Read(@S[1], Self.Size);
End;

End.
