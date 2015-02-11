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
 * TERRA_FTP
 * Implements FTP protocol
 ***********************************************************************************************************************
}

Unit TERRA_FTP;

{$I terra.inc}

// FTP URL format
// ftp://username:password@host/path

Interface
Uses TERRA_String, TERRA_Error, TERRA_Utils, TERRA_Stream, TERRA_Sockets;

Const
  FTPPort = 21;

  ftp_Success = 0;
  ftp_UnknownError    = -1;
  ftp_InvalidSession  = -2;
  ftp_InvalidPassword = -3;
  ftp_InvalidPath     = -4;
  ftp_InvalidFile     = -5;
  ftp_TransferError   = -6;
  ftp_TransferFailed  = -7;

Type
  FTPSession = Class(TERRAObject)
    Protected
      _Stream:Socket;
      _ServerAddress:TERRAString;
      _LastResponse:TERRAString;
      _LastCode:Integer;

      _Active:Boolean;
      _ErrorCode:Integer;
      _LastCommand:TERRAString;

      Function GetStatus:Integer;
      Function EndSession(ErrorCode:Integer):Integer;

      Procedure SendCommand(S:TERRAString);

      Function GetSocket(S:TERRAString):Socket;

    Public
      Constructor Create(URL:TERRAString; Port:Integer = FTPPort);
      Destructor Destroy; Override;

      Function PutFile(FileName:TERRAString; Source:Stream; Notifier:ProgressNotifier=Nil):Integer; Overload;
      Function GetFile(FileName:TERRAString; Dest:Stream; Notifier:ProgressNotifier=Nil):Integer; Overload;

      Function GetFile(FileName:TERRAString; DestFile:TERRAString=''; Notifier:ProgressNotifier=Nil):Integer; Overload;
      Function PutFile(FileName:TERRAString; SourceFile:TERRAString=''; Notifier:ProgressNotifier=Nil):Integer; Overload;

      Property Connected:Boolean Read _Active;
  End;

  FTPMirrorSession = Class(TERRAObject)
    Protected
      _SessionList:Array Of FTPSession;
      _SessionCount:Integer;

      Function GetSession(Index:Integer):FTPSession;

    Public
      Constructor Create(URLList:TERRAString='');
      Destructor Destroy; Override;

      Procedure NewConnection(URL:TERRAString);

      Procedure PutFile(FileName:TERRAString; Source:Stream; Notifier:ProgressNotifier=Nil);Overload;
      Procedure GetFile(FileName:TERRAString; Dest:Stream; Notifier:ProgressNotifier=Nil);Overload;

      Procedure GetFile(FileName:TERRAString; DestFile:TERRAString=''; Notifier:ProgressNotifier=Nil);Overload;
      Procedure PutFile(FileName:TERRAString; SourceFile:TERRAString=''; Notifier:ProgressNotifier=Nil);Overload;

      Property Session[Index:Integer]:FTPSession Read GetSession;
      Property SessionCount:Integer Read _SessionCount;
  End;

Implementation
Uses TERRA_Application, TERRA_OS, TERRA_Log, TERRA_FileStream, TERRA_FileUtils, TERRA_MemoryStream;

Const
  BufferSize = High(Word);

Procedure FormatFTPPath(Var Path:TERRAString);
Begin
  If Path='' Then
    Exit;

  If StringFirstChar(Path)<> Ord('/') Then
    StringPrependChar(Path, Ord('/'));

  StringReplaceChar(Ord('\'), Ord('/'), Path);
End;

{ FTPSession }
Function FTPSession.GetStatus():Integer;
Var
  S:TERRAString;
  I:Integer;
Begin
  _LastResponse:='';
  Result := ftp_UnknownError;

  S := '';  
  Repeat
    _Stream.ReadString(S);
    If S='' Then
      Break;

    I:=Pos(' ', S);
    _LastResponse:=_LastResponse+Copy(S, Succ(I), MaxInt)+#13#10;
    Result := StringToInt(Copy(S, 1, Pred(I)));
  Until Result<>_LastCode;

  _LastResponse := StringTrim(_LastResponse);
  _LastCode := Result;

  Log(logDebug, 'FTP', _LastCommand+' -> '+IntToString(Result));
End;

Constructor FTPSession.Create(URL:TERRAString; Port:Integer);
Var
  I:Integer;
  S,Path:TERRAString;
  Username,Password:TERRAString;
Begin
  _Active := False;
  _ErrorCode := 0;

  I:=Pos('://', URL);
  S:=StringUpper(Copy(URL, 1, Pred(I)));
  If S<>'FTP' Then
  Begin
    RaiseError('Invalid URL.'+#13#10+URL);
    Exit;
  End;

  URL:=Copy(URL, I+3, MaxInt);

  I:=Pos('@', URL);
  If I>0 Then
  Begin
    S:=Copy(URL, 1, Pred(I));
    URL:=Copy(URL, Succ(I), MaxInt);

    I:=Pos(':', S);
    UserName := Copy(S, 1, Pred(I));
    Password := Copy(S, Succ(I), MaxInt);
  End Else
  Begin
    Username:='anonymous';
    Password:='';
  End;

  I:=Pos('/', URL);
  If I>0 Then
  Begin
    Path:=Copy(URL, Succ(I), MaxInt);
    URL:=Copy(URL, 1, Pred(I));
  End Else
    Path:='';

  _ServerAddress:=URL;
  _Stream := Socket.Create(_ServerAddress, Port);
  If (_Stream.EOF) Or (GetStatus<>220) Then
  Begin
    EndSession(ftp_InvalidSession);
    Exit;
  End;

  _Active := True;
  
  SendCommand('USER '+Username);
  If GetStatus=331 Then
  Begin
    SendCommand('PASS '+Password);
    If GetStatus<>230 Then
    Begin
      EndSession(ftp_InvalidPassword);
      Exit;
    End;
  End;

  If Path<>'' Then
  Begin
    FormatFTPPath(Path);
    SendCommand('CWD '+Path);
    If GetStatus<>250 Then
    Begin
      EndSession(ftp_InvalidPath);
      Exit;
    End;
  End;
End;

Destructor FTPSession.Destroy;
Begin
  EndSession(0);
End;

Function FTPSession.GetFile(FileName:TERRAString; Dest:Stream; Notifier:ProgressNotifier=Nil):Integer;
Var
  Path:TERRAString;
  BlockSize, Count, FileSize:Integer;
  Buffer:Pointer;
  Source:Socket;
Begin
  If Not Assigned(_Stream) Then
  Begin
    Result := ftp_InvalidSession;
    Exit;
  End;

  Path := GetFilePath(FileName);
  FileName := GetFileName(FileName, False);

  If (Path<>'') Then
  Begin
    FormatFTPPath(Path);

    SendCommand('CWD '+Path);
    If GetStatus<>200 Then
    Begin
      Result := EndSession(ftp_InvalidPath);
      Exit;
    End;
  End;

  SendCommand('TYPE I');
  If GetStatus<>200 Then
  Begin
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  SendCommand('SIZE '+FileName);
  If GetStatus<>213 Then
  Begin
    Result := EndSession(ftp_InvalidFile);
    Exit;
  End;


  FileSize := StringToInt(_LastResponse);

  SendCommand('PASV');
  If GetStatus<>227 Then
  Begin
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  SendCommand('RETR '+FileName);
  Source := GetSocket(_LastResponse);

  If (GetStatus<>150) Then
  Begin
    Source.Destroy();
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  Count:=0;
  GetMem(Buffer, BufferSize);
  While (Count<FileSize) Do
  Begin
    BlockSize:=IntMin(BufferSize, FileSize-Count);
    BlockSize:=Source.Read(Buffer, BlockSize);
    If BlockSize<0 Then
    Begin
      Source.Destroy;
      Result := EndSession(ftp_TransferError);
      Exit;
    End;

    If BlockSize>0 Then
    Begin
      Dest.Write(Buffer, BlockSize);
      Inc(Count, BlockSize);
      If Assigned(Notifier) Then
        Notifier.Notify(Count/FileSize);
    End;
  End;
  FreeMem(Buffer);
  Source.Destroy;

  If GetStatus<>226 Then
  Begin
    Result := EndSession(ftp_TransferFailed);
    Exit;
  End;

  If Dest Is MemoryStream Then
    Dest.Truncate();

  Log(logDebug, 'FTP', 'Saved stream with size '+IntToString(Dest.Position));

  Dest.Seek(0);

  Result := ftp_Success;
End;

Function FTPSession.PutFile(FileName:TERRAString; Source:Stream; Notifier:ProgressNotifier=Nil):Integer;
Var
  Path:TERRAString;
  BlockSize, FileSize, Count:Integer;
  Buffer:Pointer;
  Dest:Socket;
  S:TERRAString;
  I:Integer;
Begin
  If Not Assigned(_Stream) Then
  Begin
    Result := ftp_InvalidSession;
    Exit;
  End;

  Log(logDebug, 'FTP', 'Sending file: '+FileName);

  Path := GetFilePath(FileName);
  FileName := GetFileName(FileName, False);

  If (Path<>'') Then
  Begin
    FormatFTPPath(Path);

    SendCommand('CWD '+Path);
    If GetStatus<>200 Then
    Begin
      Result := EndSession(ftp_InvalidPath);
      Exit;
    End;
  End;

  SendCommand('TYPE I');
  If GetStatus<>200 Then
  Begin
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  FileSize:=Source.Size;

  SendCommand('PASV');
  If GetStatus<>227 Then
  Begin
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  SendCommand('STOR '+FileName);
  Dest := GetSocket(_LastResponse);

  If (GetStatus<>150) Or (Dest.EOF) Then
  Begin
    Dest.Destroy;
    Result := EndSession(ftp_TransferError);
    Exit;
  End;

  Count:=0;
  GetMem(Buffer, BufferSize);
  While (Count<FileSize) Do
  Begin
    BlockSize := IntMin(BufferSize, FileSize-Count);
    Source.Read(Buffer, BlockSize);
    BlockSize := Dest.Write(Buffer, BlockSize);
    If BlockSize<0 Then
    Begin
      Dest.Destroy;
      Result := EndSession(ftp_TransferError);
      Exit;
    End;

    If BlockSize>0 Then
      Inc(Count, BlockSize);

    If Assigned(Notifier) Then
      Notifier.Notify(Count/FileSize);
  End;
  FreeMem(Buffer);
  Dest.Destroy;

  If GetStatus<>226 Then
  Begin
    Result := EndSession(ftp_TransferFailed);
    Exit;
  End;

  Result := ftp_Success;
End;

Function FTPSession.GetFile(FileName, DestFile:TERRAString; Notifier:ProgressNotifier):Integer;
Var
  Stream:FileStream;
Begin
  If DestFile='' Then
    DestFile := Application.Instance.DocumentPath + PathSeparator + GetFileName(FileName, False);

  Stream:= FileStream.Create(DestFile);
  Result := GetFile(FileName, Stream, Notifier);
  Stream.Destroy;
End;

Function FTPSession.PutFile(FileName, SourceFile:TERRAString; Notifier:ProgressNotifier):Integer;
Var
  Stream:FileStream;
Begin
  If SourceFile='' Then
    SourceFile:=FileName;

  If Not FileStream.Exists(SourceFile) Then
  Begin
    Result := ftp_InvalidFile;
    Exit;
  End;

  Stream := FileStream.Open(SourceFile);
  Result := PutFile(FileName, Stream, Notifier);
  Stream.Destroy;
End;

Function FTPSession.EndSession(ErrorCode:Integer):Integer;
Begin
  _ErrorCode := ErrorCode;
  Result := ErrorCode;

  If Not Assigned(_Stream) Then
    Exit;

  Log(logDebug, 'FTP', 'Ending FTP session, code: '+IntToString(ErrorCode));

  SendCommand('QUIT');
  _Stream.Destroy();
  _Stream := Nil;

  _Active := False;
End;

Procedure FTPSession.SendCommand(S:TERRAString);
Begin
  _LastCommand := S;
  _Stream.WriteLine(S);
End;

Function FTPSession.GetSocket(S:TERRAString): Socket;
Var
  OutAddress:TERRAString;
  I:Integer;
  A,B:Word;
  Port:Cardinal;
begin
  Log(logDebug, 'FTP', 'FTP port: '+S);

  I := StringPos('(',S);
  S := StringCopy(S, Succ(I), MaxInt);

  I := StringPos(')',S);
  S := StringCopy(S, 1, Pred(I));

  OutAddress := '';
  For I:=1 To 3 Do
    OutAddress := OutAddress + StringGetNextSplit(S, Ord(',')) + '.';

  OutAddress := OutAddress + StringGetNextSplit(S, Ord(','));

  A := StringToInt(StringGetNextSplit(S, Ord(',')));
  Log(logDebug, 'FTP', 'Last:'+S);
  B := StringToInt(StringGetNextSplit(S, Ord(',')));

  Log(logDebug, 'FTP', 'A: '+IntToString(A));
  Log(logDebug, 'FTP', 'B: '+IntToString(B));

  Port := A*256 + B;
  Result := Socket.Create(OutAddress{_ServerAddress}, Port);
End;

{ FTPMirrorSession }
Constructor FTPMirrorSession.Create(URLList:TERRAString);
Var
  URL:TERRAString;
Begin
  While URLList<>'' Do
  Begin
    URL := StringGetNextSplit(URLList, Ord(','));
    NewConnection(URL);
  End;
End;

Procedure FTPMirrorSession.NewConnection(URL:TERRAString);
Begin
  Inc(_SessionCount);
  SetLength(_SessionList,_SessionCount);
  _SessionList[Pred(_SessionCount)]:=FTPSession.Create(URL);
End;


Destructor FTPMirrorSession.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SessionCount) Do
    _SessionList[I].Destroy;
End;

Procedure FTPMirrorSession.GetFile(FileName, DestFile:TERRAString; Notifier:ProgressNotifier);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SessionCount) Do
    _SessionList[I].GetFile(FileName, DestFile, Notifier);
End;

Procedure FTPMirrorSession.GetFile(FileName:TERRAString; Dest: Stream; Notifier:ProgressNotifier);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SessionCount) Do
    _SessionList[I].GetFile(FileName, Dest, Notifier);
End;

Function FTPMirrorSession.GetSession(Index: Integer): FTPSession;
Begin
  Result:=_SessionList[Index];
End;

Procedure FTPMirrorSession.PutFile(FileName, SourceFile:TERRAString; Notifier:ProgressNotifier);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SessionCount) Do
    _SessionList[I].PutFile(FileName, SourceFile, Notifier);
End;

Procedure FTPMirrorSession.PutFile(FileName:TERRAString; Source: Stream; Notifier:ProgressNotifier);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SessionCount) Do
    _SessionList[I].PutFile(FileName, Source, Notifier);
End;

End.
