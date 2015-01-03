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
Uses SysUtils, TERRA_Utils, TERRA_Application, TERRA_OS, TERRA_IO, TERRA_Sockets,
  TERRA_FileIO, TERRA_FileUtils, TERRA_FileManager;

Const
  HTTPProtocol='HTTP';
  FILEProtocol='FILE';

  HTTPPort = 80;
  BufferSize = 1024;
  DefaultClientName = 'TERRA Downloader v1.1';

  ConnectionTimeOut = 5000;

  httpOk                = 0;
  httpInvalidURL        = 1;
  httpInvalidProtocol   = 2;
  httpNotFound          = 3;
  httpConnectionFailed  = 4;
  httpConnectionTimeOut = 5;
  httpInvalidStream     = 6;

Type
  HeaderTag = Record
    Name:AnsiString;
    Value:AnsiString;
  End;

  HTTPDownloader = Class;

  DownloadCallback = Procedure (Download:HTTPDownloader); Cdecl;

  HTTPDownloader = Class(TERRAObject)
    Protected
      _URL:AnsiString;
      _FileName:AnsiString;
      _ErrorCode:Integer;
      _Offset:Integer;
      _Stream:Stream;
      _Target:Stream;
      _TargetName:AnsiString;
      _Callback:DownloadCallback;
      _Downloading:Boolean;
      _TotalSize:Integer;
      _Read:Integer;
      _Response:AnsiString;
      _Buffer:Pointer;
      _Protocol:AnsiString;
      _HostName:AnsiString;
      _Tags:Array Of HeaderTag;
      _TagCount:Integer;
      _Progress:Integer;
      _UpdateTime:Cardinal;
      _UserData:Pointer;
      _ChunkedTransfer:Boolean;
      _ClientName:AnsiString;
      _Cookie:AnsiString;

      Function ReceiveHeader:Integer;
      Procedure Update;
      Procedure ContinueTransfer(Count: Integer);
      Procedure WriteLeftovers();

    Public

      Constructor Create(URL:AnsiString; Const Cookie:AnsiString; Dest:Stream; Callback:DownloadCallback = Nil; Port:Integer=HTTPPort; Const ClientName:AnsiString = DefaultClientName); // Returns file size in bytes
      Destructor Destroy; Override;

      Function GetHeaderProperty(Name:AnsiString):AnsiString;

      Function GetTag(Name:AnsiString):AnsiString;

      Property TagCount:Integer Read _TagCount;
      Property Progress:Integer Read _Progress;
      Property HostName:AnsiString Read _HostName;
      Property URL:AnsiString Read _URL;
      Property FileName:AnsiString Read _FileName;
      Property Target:Stream Read _Target;
      Property ErrorCode:Integer Read _ErrorCode;
      Property UserData:Pointer  Read _UserData;
      Property Cookie:AnsiString Read _Cookie;
  End;

  DownloadManager = Class(ApplicationComponent)
    Protected
      _Downloads:Array Of HTTPDownloader;
      _DownloadCount:Integer;


      Function GetOverallProgress:Integer;

    Public
      Constructor Create;
      Destructor Destroy; Override;

      Class Function Instance:DownloadManager;

      Procedure Update; Override;

      Procedure Flush;

      Function GetDownload(Index:Integer):HTTPDownloader;

      Function StartWithCookie(URL:AnsiString; Const Cookie:AnsiString;  Dest:Stream = Nil; Callback:DownloadCallback = Nil; UserData:Pointer = Nil; Port:Integer=HTTPPort; AllowCache:Boolean=True; Const ClientName:AnsiString = DefaultClientName):HTTPDownloader;
      Function Start(Const URL:AnsiString; Dest:Stream = Nil; Callback:DownloadCallback = Nil; UserData:Pointer = Nil; Port:Integer=HTTPPort; AllowCache:Boolean=True; Const ClientName:AnsiString = DefaultClientName):HTTPDownloader;

      Function Post(URL:AnsiString; Port:Integer=HTTPPort; Const ClientName:AnsiString = DefaultClientName):Integer;

      Function Put(URL:AnsiString; Source:Stream; Port:Integer=HTTPPort; Const ClientName:AnsiString = DefaultClientName):Integer;

      Property Count:Integer Read _DownloadCount;
      Property Progress:Integer Read GetOverallProgress;
  End;

Var
  DownloadTempPath:AnsiString;

Implementation
Uses TERRA_Log, TERRA_ResourceManager;

Function GetTempPath:AnsiString;
Begin
  If DownloadTempPath<>'' Then
    Result := DownloadTempPath
  Else
  If Assigned(Application.Instance()) Then
    Result := Application.Instance.TempPath
  Else
    Result := '';
End;

// LHTTPDownloader class

Constructor HTTPDownloader.Create(URL:AnsiString; Const Cookie:AnsiString; Dest:Stream; Callback:DownloadCallback; Port:Integer; Const ClientName:AnsiString);
Var
  I:Integer;
  Request:AnsiString;
Begin
  _Stream := Nil;
  _ErrorCode := httpOK;
  _URL := URL;
  _Offset := Dest.Position;
  _Target := Dest;
  _Callback := Callback;
  GetMem(_Buffer, BufferSize);

  If (ClientName <> '') Then
    Self._ClientName := ClientName
  Else
    Self._ClientName := DefaultClientName;

  _Downloading := False;
  _Read := 0;
  _TagCount := 0;
  _Progress := 0;

  If (URL='') Or (Dest=Nil) Then
  Begin
    Log(logError, 'HTTP', 'Invalid download arguments.');
    _TotalSize := -1;
    _ErrorCode := httpInvalidURL;
    Exit;
  End;

  For I:=1 To Length(URL) Do
    If URL[I]='\' Then
      URL[I]:='/';

  // extract http:// from url
  I:=Pos('://',URL);
  If I>0 Then
  Begin
    _Protocol:=UpStr(Copy(URL,1,Pred(I)));
    URL:=Copy(URL,I+3,Length(URL));
  End Else
    _Protocol:=HTTPProtocol;

  I := PosRev('/', URL);
  _FileName := Copy(URL, Succ(I), MaxInt);

  _UpdateTime := GetTime;
  If (_Protocol = HTTPProtocol) Then
  Begin
    // Extract hostname from url
    If Pos('/', URL)<=0 Then
      URL:=URL+'/';
    I:=Pos('/',URL);
    If I>0 Then
    Begin
      _HostName:=Copy(URL,1,Pred(I));
      URL:=Copy(URL,I,Length(URL));
    End Else
    Begin
      Log(logError, 'HTTP', 'Invalid URL: '+URL);
      _TotalSize := -1;
      _ErrorCode := httpInvalidURL;
      Exit;
    End;

    _Stream := Socket.Create(_HostName, Port);
    If (_Stream.EOF) Then
    Begin
      Log(logError, 'HTTP', 'Connection failed: '+URL);
      _TotalSize := -1;
      _ErrorCode := httpConnectionFailed;
      Exit;
    End;

    Log(logDebug, 'HTTP', 'Sending request to '+URL);
    Request:= 'GET '+URL+' HTTP/1.1'+#13#10;
    Request := Request + 'User-Agent: ' + ClientName + #13#10;
    Request := Request + 'Host: '+ _HostName + #13#10;
    Request := Request + 'Accept: text/html, text/xml, image/gif, image/x-xbitmap, image/jpeg, */*'#13#10;

    If (Cookie<>'') And (Pos('=', Cookie)>0) Then
      Request := Request + 'Cookie: ' + Cookie;

    //Request := Request + 'Connection: keep-alive'+#13#10;
    Request := Request + #13#10;

    Socket(_Stream).Blocking := True;
    _Stream.Write(@Request[1], Length(Request));
    _TotalSize := ReceiveHeader;
  End Else
  If (_Protocol=FILEProtocol) Then
  Begin
    If Not FileExists(URL) Then
    Begin
      Log(logError, 'HTTP', 'File not found: '+_URL);
      _TotalSize := -1;
      _ErrorCode := httpNotFound;
      Exit;
    End;

    _Stream := FileStream.Open(URL);
    _TotalSize := _Stream.Size;
  End Else
  Begin
    Log(logError, 'HTTP', 'Unknown protocol: '+_Protocol);
    _TotalSize := -1;
    _ErrorCode := httpInvalidProtocol;
    Exit;
  End;

  _Downloading :=  (_Progress<100);
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

Function HTTPDownloader.ReceiveHeader:Integer;
Var
  I,X,Len:Integer;
  Tag,Value,S:AnsiString;
  Response:AnsiString;
Begin
  _TotalSize := -1;
  _ChunkedTransfer := False;
  _Cookie := '';

  Len := _Stream.Read(_Buffer, BufferSize);
  If Len>0 Then
  Begin
    SetLength(Response,Len);
    Move(_Buffer^,Response[1],Len);

    X:=0;
    While Response<>'' Do
    Begin
      I:=Pos(#10,Response);
      If I=0 Then
        I:=Length(Response);

      Value:=TrimLeft(TrimRight(Copy(Response,1,I)));
      If (Value='') Then
      Begin
        Response := Copy(Response, 3, MaxInt);
        If (_TotalSize<0) And (Not _ChunkedTransfer) Then
        Begin
          _TotalSize := 0;
        End;

        Break;
      End;

      Response := Copy(Response, Succ(I), Length(Response));

      If Pos('404 Not Found', Value)>0 Then
        Break;

      I:=Pos(':',Value);
      If (I=0)And(X>0) Then Break;

      Tag := Copy(Value,1,Pred(I));
      Tag := UpStr(Tag);
      Value:=Copy(Value,Succ(I),Length(Value));
      Value:=TrimLeft(TrimRight(Value));

      If (Tag<>'') Then
      Begin
        Inc(_TagCount);
        SetLength(_Tags, _TagCount);
        _Tags[Pred(_TagCount)].Name := Tag;
        _Tags[Pred(_TagCount)].Value := Value;

        If (Tag = 'SET-COOKIE') Then
        Begin
          Self._Cookie := Value;
        End;

        If Tag='CONTENT-LENGTH' Then
        Begin
          _TotalSize := StringToInt(Value);
        End;

        If (Tag='TRANSFER-ENCODING') Then
        Begin
          If (UpStr(Value)='CHUNKED') Then
            _ChunkedTransfer := True;
        End;
      End;

      Inc(X);
    End;

    _Response := Response;
    SetLength(Response,0);
  End;

  If (_ChunkedTransfer) And (_Response<>'') Then
  Begin
    I := Pos(#13, _Response);
    S := Copy(_Response, 1, Pred(I));
    _Response := Copy(_Response, (I+2), MaxInt);
    Len := HexStrToInt(S) - Length(_Response);
    WriteLeftovers();
    If Len<=0 Then
      _Progress := 100
    Else
      ContinueTransfer(Len);
  End Else
    WriteLeftovers();
  Result:=_TotalSize;
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
  S:AnsiString;
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

Destructor HTTPDownloader.Destroy;
Begin
  If Assigned(_Buffer) Then
    FreeMem(_Buffer);

  If Assigned(_Stream) Then
    _Stream.Destroy;
End;

Function HTTPDownloader.GetHeaderProperty(Name:AnsiString):AnsiString;
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

Destructor DownloadManager.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_DownloadCount) Do
    _Downloads[I].Destroy;

  _DownloadManager_Instance := Nil;
End;

Procedure DownloadManager.Update;
Var
  Remove:Boolean;
  I:Integer;
Begin
  If (Prefetching) Then
    Exit;

  {$IFDEF WINDOWS}
  Sleep(1);
  {$ENDIF}

  I:=0;
  While (I<_DownloadCount) Do
  Begin
    Remove := False;
    If (_Downloads[I]._Progress<100) And (GetTime - _Downloads[I]._UpdateTime>ConnectionTimeOut) Then
    Begin
      Log(logWarning, 'HTTP', 'Connection time out :'+_Downloads[i]._URL);
      _Downloads[I]._ErrorCode := httpConnectionTimeOut;
    End;

    If (_Downloads[I]._ErrorCode<>httpOk) Then
    Begin
      Log(logDebug, 'HTTP', 'Download error :'+_Downloads[i]._URL);

      If Assigned(_Downloads[I]._Callback) Then
        _Downloads[I]._Callback(_Downloads[I]);
      Remove := True;
    End Else
    Begin
      _Downloads[I].Update;
      If (_Downloads[I].Progress>=100) Then
      Begin
        Log(logDebug, 'HTTP', 'Download finished :'+_Downloads[i]._URL);

        If (_Downloads[I]._Target Is MemoryStream) Then
        Begin
          Log(logDebug, 'HTTP', 'Truncating memory stream');
          _Downloads[I]._Target.Truncate;
        End;

        Log(logDebug, 'HTTP', 'Seeking to zero');
        _Downloads[I]._Target.Seek(_Downloads[I]._Offset);

        Log(logDebug, 'HTTP', 'Invokating callback');
        If Assigned(_Downloads[I]._Callback) Then
        Begin
          {S := _Downloads[I]._URL;
          ReplaceAllText('/', PathSeparator, S);
          S := GetFileName(S, False);
          S := GetTempPath + PathSeparator + S;

          If (FileStream.Exists(S)) Then
          Begin
            _Downloads[I]._Target.Destroy;
            _Downloads[I]._Target := MemoryStream.Create(S);
          End;}

          _Downloads[I]._Callback(_Downloads[I]);
        End;
        Remove := True;
      End;
    End;

    If (Remove) Then
    Begin
      _Downloads[I].Destroy;
      _Downloads[I] := _Downloads[Pred(_DownloadCount)];
      Dec(_DownloadCount);
    End Else
      Inc(I);
  End;
End;

Function DownloadManager.StartWithCookie(URL:AnsiString; Const Cookie:AnsiString; Dest:Stream; Callback:DownloadCallback; UserData:Pointer; Port:Integer; AllowCache:Boolean; Const ClientName:AnsiString):HTTPDownloader;
Var
  S, FileName:AnsiString;
  NoCache:Boolean;
Begin
  FileName := URL;
  ReplaceText('/', PathSeparator, FileName);
  FileName := GetFileName(FileName, False);
  S := FileManager.Instance.SearchResourceFile(FileName);

  NoCache := Pos('.PHP', UpStr(URL))>0;

  If (Dest = Nil) Then
  Begin
    If (S<>'') And (Not NoCache) And (AllowCache) Then
    Begin
      Log(logDebug, 'HTTP', 'Cached: '+URL);
      URL := 'file://'+S;
      Dest := MemoryStream.Create(1024);
    End Else
    If (FileName<>'') Then
    Begin
      If NoCache Then
      Begin
        FileName := 'temp.php';
        //S := FileName;
        //FileName := GetNextWord(S, '?');
        Dest := MemoryStream.Create(1024*1024);
      End Else
        Dest := FileStream.Create(GetTempPath + PathSeparator + FileName);
    End;
  End;
  
  Log(logError, 'HTTP', 'Starting download.');
  Result := HTTPDownloader.Create(URL, Cookie, Dest, Callback, Port, ClientName);
  Result._UserData := UserData;
  Inc(_DownloadCount);
  SetLength(_Downloads, _DownloadCount);
  _Downloads[Pred(_DownloadCount)] := Result;
  Log(logError, 'HTTP', 'Download dispatched.');
End;

Function DownloadManager.Start(Const URL:AnsiString; Dest:Stream; Callback:DownloadCallback; UserData:Pointer; Port:Integer; AllowCache:Boolean; Const ClientName:AnsiString):HTTPDownloader;
Begin
  Result := StartWithCookie(URL, '', Dest, Callback, UserData, Port, AllowCache, ClientName);
End;

Function HTTPDownloader.GetTag(Name:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Name := UpStr(Name);
  For I:=0 To Pred(_TagCount) Do
  If (UpStr(_Tags[I].Name) = Name) Then
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

Function DownloadManager.Post(URL:AnsiString; Port: Integer; Const ClientName:AnsiString): Integer;
Var
  I:Integer;
  Dest:Socket;
  Protocol, Request, Data, HostName:AnsiString;
Begin
  I:=Pos('://',URL);
  If I>0 Then
  Begin
    Protocol := UpStr(Copy(URL,1,Pred(I)));
    URL := Copy(URL, I+3, Length(URL));
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
    Data := Copy(URL, Succ(I), MaxInt);
    URL := Copy(URL, 1, Pred(I));
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
  Dest.Destroy;

  Result := httpOk;
End;

Function DownloadManager.Put(URL:AnsiString; Source: Stream; Port: Integer; Const ClientName:AnsiString): Integer;
Var
  I:Integer;
  Dest:Socket;
  Len:Integer;
  Protocol, Request, HostName:AnsiString;
  Response:AnsiString;
Begin
  If (Source = Nil) Or (Source.Size<=0) Then
  Begin
    Result := httpInvalidStream;
    Exit;
  End;


  I:=Pos('://',URL);
  If I>0 Then
  Begin
    Protocol := UpStr(Copy(URL,1,Pred(I)));
    URL := Copy(URL, I+3, Length(URL));
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
  Dest.Destroy;


  Result := httpOk;
End;

End.
