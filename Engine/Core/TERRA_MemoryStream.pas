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
 * TERRA_MemoryStream
 * Implements a memory stream
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_MemoryStream;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I terra.inc}
{$ENDIF}

Interface

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Stream;

Type
  MemoryStream = Class(Stream)
    Protected
      _Buffer:PByte;

    Public
      Constructor Create(BufferSize:Integer; Buffer:PByte = Nil; StreamMode:Integer=smDefault); Overload;
      Constructor Create(Const FileName:TERRAString; StreamMode:Integer=smDefault);Overload;
      {$IFDEF OXYGENE}
      Procedure Release;Override;
      {$ELSE}
      Procedure Release;Override;
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
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_FileStream, TERRA_OS;

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

Constructor MemoryStream.Create(Const FileName:TERRAString; StreamMode:Integer=smDefault);
Var
  F:FileStream;
Begin
  Inherited Create(StreamMode);

  F := FileStream.Open(FileName);
  _Size := F.Size;

  Create(_Size, Nil, StreamMode);

  Log(logDebug, 'IO', 'Loaded memory stream from '+FileName+', size = '+IntToString(_Size));

  F.Read(_Buffer, _Size);
  ReleaseObject(F);

  _Name := FileName;
End;

{$IFDEF OXYGENE}
Procedure MemoryStream.Release;
{$ELSE}
Procedure MemoryStream.Release;
{$ENDIF}
Begin
  If (_Mode And smShared=0) And (Assigned(_Buffer)) Then
  Begin
    //Log.Write(logDebug,'IO','MemoryStream.Release','Releasing '+MemStr(Size));
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
    Log(logWarning, 'IO', 'Cannot read from memory in '+Self._Name+' ('+IntToString(_Pos)+'/'+IntToString(_Size)+')');
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

  If Length<=0 Then
    Exit;

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
    RaiseError('Cannot seek in memory inside '+Self.Name+ ' at pos ' + CardinalToString(NewPosition)+ ' -> '+ CardinalToString(_Size));
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
