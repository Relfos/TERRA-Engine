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
 * TERRA_HTTP
 * Implements HTTP protocol
 ***********************************************************************************************************************
}
Unit TERRA_HTTP;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Application, TERRA_OS, TERRA_Stream, TERRA_Sockets,
  TERRA_FileStream, TERRA_FileUtils, TERRA_FileManager;

{-$DEFINE ALLOW_PERSISTENT_CONNECTIONS}

Const
  HTTPProtocol='HTTP';
  FILEProtocol='FILE';

  HTTPSeparator:TERRAChar = Ord('/');

  HTTPPort = 80;
  BufferSize = 1024;
  DefaultClientName = 'TERRA Downloader v1.1';

  KeepAliveDuration = 10000;
  ConnectionTimeOut = 1000 * 60;

Type
  HTTPError = (
    httpOk                = 0,
    httpOffline ,
    httpInvalidURL,
    httpInvalidProtocol,
    httpNotFound,
    httpConnectionFailed,
    httpConnectionInterrupted,
    httpInvalidStream
  );

  HeaderTag = Record
    Name:TERRAString;
    Value:TERRAString;
  End;

  HTTPDownloader = Class;

  DownloadCallback = Procedure (Download:HTTPDownloader); Cdecl;

  HTTPConnection = Class(TERRAObject)
    _Host:TERRAString;
    _Port:Integer;
    _Socket:Socket;
    _Alive:Boolean;
    _Target:HTTPDownloader;
    _LastUpdate:Cardinal;
    _RequestCount:Integer;

    Constructor Create(Const HostName:TERRAString; Port:Integer);
    Procedure Release(); Override;
  End;

  HTTPDownloader = Class(TERRAObject)
    Protected
      _URL:TERRAString;
      _FileName:TERRAString;
      _ErrorCode:HTTPError;
      _Offset:Integer;
      _Target:Stream;
      _TargetName:TERRAString;
      _Callback:DownloadCallback;
      _Downloading:Boolean;
      _TotalSize:Integer;
      _Read:Integer;
      _Response:TERRAString;
      _Buffer:Pointer;
      _Protocol:TERRAString;
      _Path:TERRAString;
      _HostName:TERRAString;
      _Port:Integer;
      _Tags:Array Of HeaderTag;
      _TagCount:Integer;

      _Progress:Integer;
      _UpdateTime:Cardinal;
      _UserData:Pointer;
      _KeepAlive:Boolean;
      _Stream:Stream;

      _ChunkedTransfer:Boolean;
      _ClientName:TERRAString;
      _Cookie:TERRAString;

      _Connection:HTTPConnection;

      Function ReceiveHeader():Boolean;
      Procedure Update();
      Procedure ContinueTransfer(Count: Integer);
      Procedure WriteLeftovers();

      Procedure InitTransfer();
      Procedure RetryTransfer();

    Public

      Constructor Create(URL:TERRAString; Const Cookie:TERRAString; Dest:Stream; Callback:DownloadCallback = Nil; Port:Integer=HTTPPort; Const ClientName:TERRAString = DefaultClientName); // Returns file size in bytes
      Procedure Release; Override;

      Function GetHeaderProperty(Name:TERRAString):TERRAString;

      Function GetTag(Const Name:TERRAString):TERRAString;

      Property TagCount:Integer Read _TagCount;
      Property Progress:Integer Read _Progress;
      Property HostName:TERRAString Read _HostName;
      Property URL:TERRAString Read _URL;
      Property FileName:TERRAString Read _FileName;
      Property Target:Stream Read _Target;
      Property ErrorCode:HTTPError Read _ErrorCode;
      Property UserData:Pointer  Read _UserData;
      Property Cookie:TERRAString Read _Cookie;
  End;

  DownloadManager = Class(ApplicationComponent)
    Protected
      _Downloads:Array Of HTTPDownloader;
      _DownloadCount:Integer;


      _Connections:Array Of HTTPConnection;
      _ConnectionCount:Integer;

      Function GetOverallProgress:Integer;

      Function GetConnection(Const HostName:TERRAString; Port:Integer):HTTPConnection;

      Function HasDownloadsWithConnection(Connection:HTTPConnection):Boolean;
      Procedure InterruptConnections(Connection:HTTPConnection);

      Procedure ClearConnectionsToDownload(Download:HTTPDownloader);

    Public
      Constructor Create;
      Procedure Release; Override;

      Class Function Instance:DownloadManager;

      Procedure Update; Override;

      Procedure Flush;

      Function GetDownload(Index:Integer):HTTPDownloader;

      Function StartWithCookie(URL:TERRAString; Cookie:TERRAString;  Dest:Stream = Nil; Callback:DownloadCallback = Nil; UserData:Pointer = Nil; Port:Integer=HTTPPort; AllowCache:Boolean=True; Const ClientName:TERRAString = DefaultClientName):HTTPDownloader;
      Function Start(URL:TERRAString; Dest:Stream = Nil; Callback:DownloadCallback = Nil; UserData:Pointer = Nil; Port:Integer=HTTPPort; AllowCache:Boolean=True; Const ClientName:TERRAString = DefaultClientName):HTTPDownloader;

      Function Post(URL:TERRAString; Port:Integer=HTTPPort; Const ClientName:TERRAString = DefaultClientName):HTTPError;

      Function Put(URL:TERRAString; Source:Stream; Port:Integer=HTTPPort; Const ClientName:TERRAString = DefaultClientName):HTTPError;

      Property Count:Integer Read _DownloadCount;
      Property Progress:Integer Read GetOverallProgress;
  End;

Var
  DownloadTempPath:TERRAString;

Implementation
Uses TERRA_Log, TERRA_MemoryStream, TERRA_ResourceManager;

Function GetTempPath:TERRAString;
Begin
  If DownloadTempPath<>'' Then
    Result := DownloadTempPath
  Else
  If Assigned(Application.Instance()) Then
    Result := Application.Instance.TempPath
  Else
    Result := '';
End;

Procedure ExtractProtocol(Var URL:TERRAString; Out Protocol:TERRAString);
Var
  I:Integer;
Begin
  I := StringPos('://', URL);
  If I>0 Then
  Begin
    Protocol := StringUpper(Copy(URL,1,Pred(I)));
    URL := Copy(URL,I+3,Length(URL));
  End Else
    Protocol := HTTPProtocol;
End;

{ HTTPDownloader }
Constructor HTTPDownloader.Create(URL:TERRAString; Const Cookie:TERRAString; Dest:Stream; Callback:DownloadCallback; Port:Integer; Const ClientName:TERRAString);
Var
  I:Integer;
Begin
  _Connection := Nil;
  _ErrorCode := httpOK;
  _URL := URL;
  _Cookie := Cookie;
  _Offset := Dest.Position;
  _Target := Dest;
  _Callback := Callback;
  _Downloading := False;
  _Port := Port;
  _TotalSize := -1;

  GetMem(_Buffer, BufferSize);

  If (ClientName <> '') Then
    Self._ClientName := ClientName
  Else
    Self._ClientName := DefaultClientName;

  _Read := 0;
  _TagCount := 0;
  _Progress := 0;

  If (URL='') Or (Dest=Nil) Then
  Begin
    Log(logError, 'HTTP', 'Invalid download arguments.');
    _ErrorCode := httpInvalidURL;
    Exit;
  End;

  StringReplaceChar(Ord('\'), HTTPSeparator, URL);

  // extract http:// from url
  ExtractProtocol(URL, _Protocol);

  I := StringCharPosReverse(Ord('/'), URL);
  _FileName := StringCopy(URL, Succ(I), MaxInt);

  _Connection := Nil;

  _UpdateTime := GetTime();
  If (_Protocol = HTTPProtocol) Then
  Begin
    // Extract hostname from url
    If Pos('/', URL)<=0 Then
      URL:=URL+'/';

    I:=Pos('/',URL);

    If I>0 Then
    Begin
      _HostName := Copy(URL,1,Pred(I));
      _Path := Copy(URL, I, Length(URL));
    End Else
    Begin
      Log(logError, 'HTTP', 'Invalid URL: '+URL);
      _ErrorCode := httpInvalidURL;
      Exit;
    End;

    _Connection := DownloadManager.Instance.GetConnection(_HostName, Port);

    If _Connection._Target = Nil Then
    Begin
      Self.InitTransfer();
    End;

    Exit;
  End Else
  If (_Protocol=FILEProtocol) Then
  Begin
    If Not FileStream.Exists(URL) Then
    Begin
      Log(logError, 'HTTP', 'File not found: '+_URL);
      _ErrorCode := httpNotFound;
      Exit;
    End;

    _Stream := FileStream.Open(URL);
    _TotalSize := _Stream.Size;

    _Downloading := (_Progress<100);
  End Else
  Begin
    Log(logError, 'HTTP', 'Unknown protocol: '+_Protocol);
    _ErrorCode := httpInvalidProtocol;
    Exit;
  End;
End;

{
HTTP/1.1 200 OK
Date: Thu, 12 Jan 2006 23:25:37 GMT
Server: Apache/1.3.34 (Unix) mod_auth_passthrough/1.8 mod_log_bytes/1.2 mod_bwlimited/1.4 FrontPage/5.0.2.2635 mod_ssl/2.8.25 OpenSSL/0.9.7a PHP-CGI/0.1b
Last-Modified: Mon, 09 Jan 2006 16:37:56 GMT
ETag: "6c4126-f46-43c29164"
Accept-Ranges: bytes
Content-Length: 3910
}

Function HTTPDownloader.ReceiveHeader():Boolean;
Var
  I,X,Len:Integer;
  Tag,Value,S:TERRAString;
  Response:TERRAString;
Begin
  Result := False;
  _ChunkedTransfer := False;
  _KeepAlive := False;
  _TotalSize := -1;

  Len := _Stream.Read(_Buffer, BufferSize);
  If Len>0 Then
  Begin
    SetLength(Response, Len);
    Move(_Buffer^, Response[1], Len);

    Result := True;
    _TotalSize := 0;

    X:=0;
    While Response<>'' Do
    Begin
      I:=Pos(#10,Response);
      If I=0 Then
        I:=Length(Response);

      Value := StringTrim(StringCopy(Response,1,I));
      If (Value='') Then
      Begin
        Response := Copy(Response, 3, MaxInt);
        {If (Not Result) And (Not _ChunkedTransfer) Then
        Begin
          Result := True;
        End;}

        Break;
      End;

      Response := Copy(Response, Succ(I), Length(Response));

      If Pos('404 Not Found', Value)>0 Then
        Break;

      I:=Pos(':',Value);
      If (I=0)And(X>0) Then Break;

      Tag := Copy(Value,1,Pred(I));
      Value := StringCopy(Value, Succ(I), Length(Value));
      Value := StringTrim(Value);

      If (Tag<>'') Then
      Begin
        Inc(_TagCount);
        SetLength(_Tags, _TagCount);
        _Tags[Pred(_TagCount)].Name := Tag;
        _Tags[Pred(_TagCount)].Value := Value;

        If (StringEquals(Tag, 'set-cookie')) Then
        Begin
          Self._Cookie := StringGetNextSplit(Value, Ord(';'));
        End;

        If (StringEquals(Tag, 'connection')) Then
        Begin
          {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
          If (StringEquals(Value, 'keep-alive')) Then
            _KeepAlive := True
          Else
          If (StringEquals(Value, 'close')) Then
            _Connection._Alive := False;
          {$ELSE}
            _Connection._Alive := False;
          {$ENDIF}
        End;

        If (StringEquals(Tag, 'content-length')) Then
        Begin
          _TotalSize := StringToInt(Value);
        End;

        If (StringEquals(Tag, 'transfer-encoding')) Then
        Begin
          If (StringEquals(Value, 'chunked')) Then
            _ChunkedTransfer := True;
        End;
      End;

      Inc(X);
    End;

    _Response := Response;
    SetLength(Response,0);
  End Else
  Begin
    _ErrorCode := httpConnectionInterrupted;
    Exit;
  End;

  If (_ChunkedTransfer) And (_Response<>'') Then
  Begin
    I := StringCharPos(Ord(#13), _Response);
    S := StringCopy(_Response, 1, Pred(I));
    _Response := StringCopy(_Response, (I+2), MaxInt);
    Len := HexStrToInt(S) - StringLength(_Response);
    WriteLeftovers();
    If Len<=0 Then
      _Progress := 100
    Else
      ContinueTransfer(Len);
  End Else
    WriteLeftovers();
End;

Procedure HTTPDownloader.WriteLeftovers();
Begin
  If _Response<>'' Then
  Begin
    _Target.Write(@_Response[1], Length(_Response));
    Inc(_Read, Length(_Response));
    _Response := '';
  End;
End;

Procedure HTTPDownloader.ContinueTransfer(Count:Integer);
Var
  Len, Count2, Temp:Integer;
Begin
  If (Self._KeepAlive) Then
  Begin
    If (Assigned(_Connection)) Then
    Begin
      _Connection._LastUpdate := GetTime();
    End Else
    Begin
      Self._ErrorCode := httpConnectionInterrupted;
      Exit;
    End;
  End;

  While (Count>0) And (Not _Stream.EOF) Do
  Begin
    Count2 := Count;
    If Count2>BufferSize Then
      Count2 := BufferSize;

    Log(logDebug, 'HTTP', 'Reading '+IntToString(Count2));
    Len := _Stream.Read(_Buffer, Count2);
    Log(logDebug, 'HTTP', 'Got '+IntToString(Len));
    If Len>0 Then
    Begin
      _UpdateTime := GetTime;
      _Target.Write(_Buffer, Len);
      Inc(_Read, Len);
      Dec(Count, Len);

      If (_ChunkedTransfer) And (Count=0) Then
      Begin
        Len := _Stream.Read(@Temp, 2);
        IntToString(Len);
      End;
    End;
  End;

  If (_ChunkedTransfer) Then
  Begin
    _TotalSize := _Read;
    If (_Stream.EOF) Or (Count=0) Then
      _Progress := 100
    Else
      _Progress := 0;
  End Else
    _Progress := Trunc((_Read/_TotalSize)*100);

  If _Progress>=100 Then
  Begin
    _Progress := 100;
    _Downloading := False;
  End;
End;

Procedure HTTPDownloader.Update;
Var
  I,Len, Count:Integer;
  S:TERRAString;
Begin
  If (Not _Downloading) Then
    Exit;
          
  If (_TotalSize=0) Then
  Begin
    _Progress := 100;
    Exit;
  End;

  If (_ChunkedTransfer) Then
  Begin
    Len := _Stream.Read(_Buffer, 20);
    If (Len>0) Then
    Begin
      SetLength(_Response, Len);
      Move(_Buffer^, _Response[1],Len);
      I := Pos(#13#10, _Response);
      S := Copy(_Response, 1, Pred(I));
      _Response := Copy(_Response, I + 2, MaxInt);

      Count := HexStrToInt(S);
      If (Count<>646) And (Count<>6212) And (Count<>0) Then
        IntTostring(2);

      If (Count>0) Then
      Begin
        Dec(Count, Length(_Response));
        WriteLeftovers();
      End;
    End Else
      Count := 0;
  End Else
  Begin
    Count := _TotalSize - _Read;
  End;

  ContinueTransfer(Count);
End;

Procedure HTTPDownloader.Release;
Begin
  Log(logDebug, 'HTTP', 'Releasing transfer: '+URL);

  If Assigned(_Buffer) Then
    FreeMem(_Buffer);

  If Assigned(_Stream) Then
  Begin
    If (Assigned(_Connection)) Then
    Begin
      _Connection._LastUpdate := GetTime();

      If (Not Self._KeepAlive) Then
      Begin
        _Connection._Alive := False;
      End;
    End Else
      _Stream.Release();
  End;
End;

Function HTTPDownloader.GetHeaderProperty(Name:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result:='';
  For I:=0 To Pred(_TagCount) Do
  If (_Tags[I].Name = Name) Then
  Begin
    Result := _Tags[I].Value;
    Exit;
  End;
End;

Procedure HTTPDownloader.InitTransfer();
Var
  Request:TERRAString;
  N:Integer;
Begin
  _UpdateTime := GetTime();
  _TotalSize := -1;

  If (Self._ErrorCode<>httpOK) Then
    Exit;

  If (Self._Connection = Nil) Then
  Begin
    Self.RetryTransfer();
    Exit;
  End;

  If (Assigned(_Connection._Target)) And (_Connection._Target<>Self) Then
    Exit;

  _Connection._Target := Self;
  Self._Stream := _Connection._Socket;
      
  If (_Stream.EOF) Then
  Begin
    Log(logError, 'HTTP', 'Connection failed: '+URL);
    _ErrorCode := httpConnectionFailed;
    Exit;
  End;

  Log(logDebug, 'HTTP', 'Sending request to '+URL);

  Inc(_Connection._RequestCount);
  Log(logDebug, 'HTTP', 'Request count: '+IntToString(_Connection._RequestCount));
                 
  Request := 'GET '+ _Path+' HTTP/1.1'+#13#10;
  Request := Request + 'User-Agent: ' + _ClientName + #13#10;
  Request := Request + 'Host: '+ _HostName + #13#10;
  Request := Request + 'Accept: text/html, text/xml, image/gif, image/x-xbitmap, image/jpeg, */*'#13#10;

  If (Cookie<>'') And (Pos('=', Cookie)>0) Then
    Request := Request + 'Cookie: ' + Cookie + #13#10;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  Request := Request + 'Connection: keep-alive'+#13#10;
  {$ENDIF}
  
  Request := Request + #13#10;

  _Connection._LastUpdate := GetTime();

  N := Length(Request);
  If (_Stream.Write(@Request[1], N)<N) Then
  Begin
    Self.RetryTransfer();
    Exit;
  End;


  If ReceiveHeader() Then
  Begin
    _Downloading := (_TotalSize>=0) And (_Progress<100);
  End Else
  Begin
    Log(logDebug, 'HTTP', 'Download failed: '+URL);
    //Self.ReceiveHeader();

    If (_Connection._RequestCount>1) Then
    Begin
      Self.RetryTransfer();
      Exit;
    End;
  End;
End;

Procedure HTTPDownloader.RetryTransfer;
Begin
  Self._ErrorCode := httpOK;

  If Assigned(_Connection) Then
    _Connection._Alive := False;

  Log(logDebug, 'HTTP', 'Retrying download: '+URL);

  _Connection := DownloadManager.Instance.GetConnection(_Hostname, _Port);
  Self.InitTransfer();
End;

{ DownloadManager }
Var
  _DownloadManager_Instance:ApplicationObject = Nil;

Class Function DownloadManager.Instance:DownloadManager;
Begin
  If Not Assigned(_DownloadManager_Instance) Then
    _DownloadManager_Instance := InitializeApplicationComponent(DownloadManager, Nil);

  Result := DownloadManager(_DownloadManager_Instance.Instance);
End;

Constructor DownloadManager.Create;
Begin
  _DownloadCount := 0;
End;

Procedure DownloadManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_DownloadCount) Do
    _Downloads[I].Release;

  _DownloadManager_Instance := Nil;
End;

Procedure DownloadManager.Update();
Var
  Remove:Boolean;
  I, J:Integer;
Begin
  If (_Prefetching) Then
    Exit;

  //Application.Instance.Yeld();

  I:=0;
  While (I<_ConnectionCount) Do
  Begin
    Remove := False;
    If (_Connections[I]._Alive) Then
    Begin
      If (_Connections[I]._Socket.EOF) Then
        Remove := True
      Else
      If ((GetTime() - _Connections[I]._LastUpdate)>KeepAliveDuration) Then
      Begin
        If (Not Self.HasDownloadsWithConnection(_Connections[I])) Then
          Remove := True;
      End;
    End Else
      Remove := True;

    If (Remove) Then
    Begin
      Self.InterruptConnections(_Connections[I]);
      _Connections[I].Release();
      _Connections[I] := _Connections[Pred(_ConnectionCount)];
      Dec(_ConnectionCount);
    End Else
      Inc(I);
  End;

  I:=0;
  While (I<_DownloadCount) Do
  Begin
    Remove := False;

    If (_Downloads[I]._ErrorCode<>httpOk) Then
    Begin
      Log(logDebug, 'HTTP', 'Download error :'+_Downloads[i]._URL+' -> error ' +IntToString(Integer(_Downloads[I]._ErrorCode)));

      If Assigned(_Downloads[I]._Callback) Then
        _Downloads[I]._Callback(_Downloads[I]);

      Remove := True;
    End Else
    If (_Downloads[I]._TotalSize<0) Then
    Begin
      _Downloads[I].InitTransfer();
    End Else
    Begin
      {If (_Downloads[I]._Progress<100) And (GetTime - _Downloads[I]._UpdateTime>ConnectionTimeOut) Then
      Begin
        Log(logWarning, 'HTTP', 'Connection time out :'+_Downloads[i]._URL);
        _Downloads[I]._ErrorCode := httpConnectionTimeOut;
      End;}

      _Downloads[I].Update();

      If (_Downloads[I].Progress>=100) Then
      Begin
        Remove := True;

        Log(logDebug, 'HTTP', 'Download finished :'+_Downloads[i]._URL);
        Log(logDebug, 'HTTP', 'Total size: '+IntToString(_Downloads[i]._Target.Position));

        If (_Downloads[I]._Target Is MemoryStream) Then
        Begin
          Log(logDebug, 'HTTP', 'Truncating memory stream');
          _Downloads[I]._Target.Truncate;
        End;

        Log(logDebug, 'HTTP', 'Seeking to zero');
        _Downloads[I]._Target.Seek(_Downloads[I]._Offset);

        Log(logDebug, 'HTTP', 'Invokating callback');
        If Assigned(_Downloads[I]._Callback) Then
          _Downloads[I]._Callback(_Downloads[I]);
      End;
    End;

    If (Remove) Then
    Begin
      Self.ClearConnectionsToDownload(_Downloads[I]);

      _Downloads[I].Release();
      For J:=I To (_DownloadCount-2) Do
        _Downloads[J] := _Downloads[Succ(J)];

      Dec(_DownloadCount);
    End Else
      Inc(I);
  End;
End;

Function DownloadManager.StartWithCookie(URL:TERRAString; Cookie:TERRAString; Dest:Stream; Callback:DownloadCallback; UserData:Pointer; Port:Integer; AllowCache:Boolean; Const ClientName:TERRAString):HTTPDownloader;
Var
  S, FileName, CachedFile:TERRAString;
  NoCache:Boolean;
Begin
  NoCache := StringContains('.php', URL);
  If (NoCache) Then
  Begin
    AllowCache := False;
    FileName := '';
    CachedFile := '';
  End Else
  Begin
    FileName := URL;
    StringReplaceText(StringFromChar(HTTPSeparator), PathSeparator, FileName);
    FileName := GetFileName(FileName, False);
    CachedFile := FileManager.Instance.SearchResourceFile(FileName);
  End;

  If (Dest = Nil) Then
  Begin
    If (FileName<>'') Then
      Dest := FileStream.Create(GetTempPath + PathSeparator + FileName)
    Else
    Begin
      Dest := MemoryStream.Create(1024);
    End;
  End;

  If (CachedFile<>'') Then
  Begin
    Log(logDebug, 'HTTP', 'Cached: '+URL);
    URL := 'file://' + CachedFile;
  End;

  Log(logDebug, 'HTTP', 'Starting download to '+URL);
  If Cookie<>'' Then
    Log(logDebug, 'HTTP', 'Cookie: '+Cookie);
  Result := HTTPDownloader.Create(URL, Cookie, Dest, Callback, Port, ClientName);
  Result._UserData := UserData;
  Inc(_DownloadCount);
  SetLength(_Downloads, _DownloadCount);
  _Downloads[Pred(_DownloadCount)] := Result;
  Log(logDebug, 'HTTP', 'Download dispatched.');
End;

Function DownloadManager.Start(URL:TERRAString; Dest:Stream; Callback:DownloadCallback; UserData:Pointer; Port:Integer; AllowCache:Boolean; Const ClientName:TERRAString):HTTPDownloader;
Begin
  Result := StartWithCookie(URL, '', Dest, Callback, UserData, Port, AllowCache, ClientName);
End;

Function HTTPDownloader.GetTag(Const Name:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TagCount) Do
  If (StringEquals(_Tags[I].Name, Name)) Then
  Begin
    Result := _Tags[I].Value;
    Exit;
  End;

  Result := '';
End;

Procedure DownloadManager.Flush;
Begin
  Repeat
    Self.Update();
  Until Self.Count <=0;
End;

Function DownloadManager.GetOverallProgress: Integer;
Var
  I:Integer;
Begin
  If (Self._DownloadCount<=0) Then
  Begin
    Result := 100;
    Exit;
  End;

  Result := 0;
  For I:=0 To Pred(_DownloadCount) Do
    Inc(Result, _Downloads[I].Progress);

  Result := Result Div _DownloadCount;
End;

Function DownloadManager.GetDownload(Index: Integer): HTTPDownloader;
Begin
  If (Index<0) Or (Index>=_DownloadCount) Then
    Result := Nil
  Else
    Result := _Downloads[Index];
End;

Function DownloadManager.Post(URL:TERRAString; Port: Integer; Const ClientName:TERRAString):HTTPError;
Var
  It:StringIterator;
  Dest:Socket;
  Protocol, Request, Data, HostName:TERRAString;
Begin
  If StringPosIterator('://', URL, It) Then
  Begin
    It.Split(Protocol, URL);
  End Else
    Protocol := HTTPProtocol;

  If (Protocol<>HTTPProtocol) Then
  Begin
    Result := httpInvalidProtocol;
    Exit;
  End;

  If StringCharPosIterator(Ord('/'),URL, It) Then
  Begin
    It.Split(HostName, URL);
  End Else
  Begin
    Log(logError, 'HTTP', 'Invalid URL: '+URL);
    Result := httpInvalidURL;
    Exit;
  End;

  If StringCharPosIterator(Ord('?'),URL, It) Then
  Begin
    It.Split(URL, Data);
  End Else
  Begin
    Log(logError, 'HTTP', 'Invalid URL: '+URL);
    Result := httpInvalidURL;
    Exit;
  End;

  Request:= 'POST '+URL+' HTTP/1.1'+#13#10+
              'Host: '+HostName+#13#10+
              'User-Agent: '+ClientName+#13#10+
              'Accept: text/html, text/xml, image/gif, */*'#13#10+
              'Content-Type: application/x-www-form-urlencoded'#13#10+
              'Content-Length: '+IntToString(Length(Data))+#13#10+
              #13#10;

  Request := Request + Data;

  Dest := Socket.Create(HostName, Port);
  If (Dest.EOF) Then
  Begin
    Log(logError, 'HTTP', 'Connection failed: '+URL);
    Result := httpConnectionFailed;
    Exit;
  End;

  Log(logDebug, 'HTTP', 'Sending post request to '+URL);
  Dest.Write(@Request[1], Length(Request));
  Dest.Release;

  Result := httpOk;
End;

Function DownloadManager.Put(URL:TERRAString; Source: Stream; Port: Integer; Const ClientName:TERRAString):HTTPError;
Var
  I:Integer;
  Dest:Socket;
  Len:Integer;
  Protocol, Request, HostName:TERRAString;
  Response:TERRAString;
Begin
  If (Source = Nil) Or (Source.Size<=0) Then
  Begin
    Result := httpInvalidStream;
    Exit;
  End;


  I:=Pos('://',URL);
  If I>0 Then
  Begin
    Protocol := StringUpper(StringCopy(URL,1,Pred(I)));
    URL := StringCopy(URL, I+3, Length(URL));
  End Else
    Protocol := HTTPProtocol;

  If (Protocol<>HTTPProtocol) Then
  Begin
    Result := httpInvalidProtocol;
    Exit;
  End;

  I := Pos('/',URL);
  If I>0 Then
  Begin
    HostName := Copy(URL,1,Pred(I));
    URL := Copy(URL,I,Length(URL));
  End Else
  Begin
    Log(logError, 'HTTP', 'Invalid URL: '+URL);
    Result := httpInvalidURL;
    Exit;
  End;

  I := Pos('?', URL);
  If (I>0) Then
  Begin
    Result := httpInvalidURL;
    Exit;
  End;

  Source.Seek(0);

  Request:= 'PUT '+URL+' HTTP/1.1'+#13#10+
              'Host: '+HostName+#13#10+
              'Content-Length: '+IntToString(Source.Size)+#13#10+
              #13#10;

  Dest := Socket.Create(HostName, Port);
  If (Dest.EOF) Then
  Begin
    Log(logError, 'HTTP', 'Connection failed: '+URL);
    Result := httpConnectionFailed;
    Exit;
  End;

  Log(logDebug, 'HTTP', 'Sending post request to '+URL);
  Dest.Write(@Request[1], Length(Request));
  Source.Copy(Dest);
  Response := #13#10;
  Dest.Write(@Response[1], Length(Response));

  SetLength(Response, 1024);
  Len := Dest.Read(@Response[1], Length(Response));
  If Len>0 Then
  Begin
    SetLength(Response,Len);
    If (Pos(' ',Response)>0) Then
      IntToString(2);
  End;
  Dest.Release;


  Result := httpOk;
End;

Function DownloadManager.GetConnection(const HostName: TERRAString; Port: Integer): HTTPConnection;
Var
  I:Integer;
Begin
  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  For I:=0 To Pred(_ConnectionCount) Do
  If (_Connections[I]._Alive)  And (StringEquals(_Connections[I]._Host, HostName)) And (_Connections[I]._Port = Port) Then
  Begin
    Result := _Connections[I];
    Exit;
  End;
  {$ENDIF}

  Result := HTTPConnection.Create(HostName, Port);
  Inc(_ConnectionCount);
  SetLength(_Connections, _ConnectionCount);
  _Connections[Pred(_ConnectionCount)] := Result;
End;

Function DownloadManager.HasDownloadsWithConnection(Connection: HTTPConnection): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_DownloadCount) Do
  If (_Downloads[I]._Connection = Connection) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure DownloadManager.InterruptConnections(Connection: HTTPConnection);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_DownloadCount) Do
  If (_Downloads[I]._Connection = Connection) Then
  Begin
    _Downloads[I]._Connection := Nil;
    _Downloads[I]._Stream := Nil;
  End;
End;

Procedure DownloadManager.ClearConnectionsToDownload(Download: HTTPDownloader);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ConnectionCount) Do
  If (_Connections[I]._Target = Download) Then
  Begin
    _Connections[I]._Target := Nil;
  End;
End;

{ HTTPConnection }
Constructor HTTPConnection.Create(const HostName: TERRAString; Port: Integer);
Begin
  _Host := HostName;
  _Port := Port;
  _Target := Nil;
  _Alive := True;

  Log(logDebug, 'HTTP', 'Opening connection to '+ _Host);

  _Socket := Socket.Create(_Host, Port);
  _Socket.Blocking := True;
  
  _LastUpdate := GetTime();
End;

Procedure HTTPConnection.Release;
Begin
  Log(logDebug, 'HTTP', 'Closing connection to '+ _Host);

  _Socket.Release();
  Log(logDebug, 'HTTP', 'Closed connection to '+ _Host);
End;

End.
