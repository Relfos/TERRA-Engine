{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
Uses TERRA_Object,TERRA_String, TERRA_Utils, TERRA_Application, TERRA_OS, TERRA_Stream, TERRA_Sockets,
  TERRA_FileStream, TERRA_FileUtils, TERRA_FileManager, TERRA_Threads, TERRA_Mutex;

{-$DEFINE ALLOW_PERSISTENT_CONNECTIONS}

Const
  HTTPProtocol='HTTP';
  FILEProtocol='FILE';

  HTTPSeparator:TERRAChar = '/';

  HTTPPort = 80;
  BufferSize = 2048;
  DefaultClientName = 'TERRA Downloader v1.1';

  KeepAliveDuration = 10000;
  ConnectionTimeOut = 1000 * 60;

Type
  HTTPError = (
    httpOk                = 0,
    httpOffline           = 1,
    httpInvalidURL        = 2,
    httpInvalidProtocol   = 3,
    httpNotFound          = 4,
    httpConnectionFailed  = 5,
    httpConnectionInterrupted = 6,
    httpBug= 7,
    httpConnectionReadError = 8,
    httpServerError = 9,
    httpInvalidStream
  );

  HTTPMethod = (
    HTTP_GET,
    HTTP_POST,
    HTTP_PUT
    );

  HeaderTag = Record
    Name:TERRAString;
    Value:TERRAString;
  End;

  HTTPDownloader = Class;

  DownloadCallback = Procedure (Download:HTTPDownloader) Of Object;

  HTTPConnection = Class(TERRAObject)
    _Host:TERRAString;
    _Port:Integer;
    _Socket:NetSocket;
    _Alive:Boolean;
    _Target:HTTPDownloader;
    _LastUpdate:Cardinal;
    _RequestCount:Integer;

    Constructor Create(Const HostName:TERRAString; Port:Integer);
    Procedure Release(); Override;
  End;

  HTTPArgument = Class(TERRAObject)
    Protected
      _Name:TERRAString;

    Public
      Function GetValue(SafeEncode:Boolean):TERRAString; Virtual; Abstract;
  End;

  HTTPIntegerArgument = Class(HTTPArgument)
    Public
      Value:Integer;

      Constructor Create(Value:Integer);
      Function GetValue(SafeEncode:Boolean):TERRAString; Override;
  End;

  HTTPCardinalArgument = Class(HTTPArgument)
    Public
      Value:Cardinal;

      Constructor Create(Value:Cardinal);
      Function GetValue(SafeEncode:Boolean):TERRAString; Override;
  End;

  HTTPBooleanArgument = Class(HTTPArgument)
    Public
      Value:Boolean;

      Constructor Create(Value:Boolean);
      Function GetValue(SafeEncode:Boolean):TERRAString; Override;
  End;

  HTTPStringArgument = Class(HTTPArgument)
    Public
      Value:TERRAString;

      Constructor Create(Const Value:TERRAString);
      Function GetValue(SafeEncode:Boolean):TERRAString; Override;
  End;

  HTTPStreamArgument = Class(HTTPArgument)
    Public
      Value:TERRAStream;

      Constructor Create(Value:TERRAStream);
      Procedure Release; Override;
      Function GetValue(SafeEncode:Boolean):TERRAString; Override;
  End;

  HTTPArgumentList = Class(TERRAObject)
    Protected
      _Items:Array Of HTTPArgument;
      _Count:Integer;

      Procedure AddArg(Const ArgName:TERRAString; Arg:HTTPArgument);

    Public
      Procedure Release(); Override;

      Procedure AddString(Const ArgName, Value:TERRAString);
      Procedure AddInteger(Const ArgName:TERRAString; Value:Integer);
      Procedure AddCardinal(Const ArgName:TERRAString; Value:Cardinal);
      Procedure AddBoolean(Const ArgName:TERRAString; Value:Boolean);
      Procedure AddStream(Const ArgName:TERRAString; Value:TERRAStream);

      Function Get(Const ArgName:TERRAString):HTTPArgument;
      Function GetQueryString(SafeEncode:Boolean):TERRAString;
      Procedure Clear();
  End;

  HTTPRequest = Class(TERRAObject)
    Protected
      _URL:TERRAString;
      _HostName:TERRAString;
      _FilePath:TERRAString;
      _FileName:TERRAString;
      _Cookie:TERRAString;

      _Dest:TERRAStream;
      _Callback:DownloadCallback;
      _Port:Integer;
      _AllowCache:Boolean;
      _ClientName:TERRAString;
      _MethodType:HTTPMethod;
      _Protocol:TERRAString;


      _Data:TERRAString; // for POST requests

      _Arguments:HTTPArgumentList;

      Procedure Prepare(Out URL, CacheFile:TERRAString);

      Function GetMethodString():TERRAString;

      Procedure Release(); Override;

    Public
      Constructor Create(Const URL:TERRAString);

      Function GetFullURL():TERRAString;

      Procedure SetCookie(Const Cookie:TERRAString);
      Procedure SetTarget(Target:TERRAStream);
      Procedure SetMethod(Const MethodType:HTTPMethod);
      Procedure SetCallback(Callback:DownloadCallback);

      Property Callback:DownloadCallback Read _Callback Write SetCallback;

      Property ClientName:TERRAString Read _ClientName;
      Property FileName:TERRAString Read _FileName;
      Property FilePath:TERRAString Read _FilePath;
      Property HostName:TERRAString Read _HostName;
      Property Cookie:TERRAString Read _Cookie;
      Property Protocol:TERRAString Read _Protocol;
      Property Port:Integer Read _Port;
      Property Arguments:HTTPArgumentList Read _Arguments;
  End;

  HTTPDownloader = Class(TERRAObject)
    Protected
      _Request:HTTPRequest;
      _Connection:HTTPConnection;

      _InBackground:Boolean;

      _URL:TERRAString;
      _ErrorCode:HTTPError;
      _ErrorMessage:TERRAString;
      _Offset:Integer;
      _Target:TERRAStream;
      _TargetName:TERRAString;
      _Downloading:Boolean;
      _TotalSize:Integer;
      _Read:Integer;
      _Response:TERRAString;
      _Buffer:Pointer;
      _Path:TERRAString;
      _Tags:Array Of HeaderTag;
      _TagCount:Integer;

      _Progress:Integer;
      _UpdateTime:Cardinal;

      _FileSource:TERRAStream;

      {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
      _KeepAlive:Boolean;
      {$ENDIF}

      _ChunkedTransfer:Boolean;

      Procedure PrepareTransfer();
      Procedure ContinueTransfer(Count: Integer);
      Procedure RetryTransfer();

      Function ReceiveHeader():Boolean;
      Function ReadChunkHeader():Integer;
      Procedure WriteLeftovers();

      Function GetStream:TERRAStream;
      Function GetDest:TERRAStream;

      Property Source:TERRAStream Read GetStream;
      Property Dest:TERRAStream Read GetDest;

      Procedure Release; Override;

    Public
      Constructor Create(Request:HTTPRequest; InBackground:Boolean);

      Function GetHeaderProperty(Name:TERRAString):TERRAString;

      Function GetTag(Const Name:TERRAString):TERRAString;

      Procedure InitTransfer();
      Procedure Update();

      Property TagCount:Integer Read _TagCount;
      Property Progress:Integer Read _Progress;
      Property URL:TERRAString Read _URL;
      Property Request:HTTPRequest Read _Request;
      Property Target:TERRAStream Read _Target;
      Property ErrorCode:HTTPError Read _ErrorCode;
      Property ErrorMessage:TERRAString Read _ErrorMessage;

      Property InBackground:Boolean Read _InBackground;
      Property Offset:Integer Read _Offset;
      Property TotalSize:Integer Read _TotalSize;
  End;


Var
  DownloadTempPath:TERRAString;

Implementation
Uses TERRA_Log, TERRA_MemoryStream, TERRA_Engine, TERRA_ResourceManager;

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

Type
  HTTPThread = Class(TERRATask)
    Protected
      _Target:HTTPDownloader;

    Public
      Constructor Create(Target:HTTPDownloader);
      Procedure Execute; Override;
  End;

Constructor HTTPThread.Create(Target:HTTPDownloader);
Begin
  _Target := Target;
End;

Procedure HTTPThread.Execute();
Begin
  _Target.PrepareTransfer();

  Repeat
    If (_Target._TotalSize<0) Then
    Begin
      _Target.InitTransfer();
    End Else
      _Target.Update();

  Until (_Target.Progress>=100) Or (_Target.ErrorCode<>httpOk);
End;

{ HTTPDownloader }
Constructor HTTPDownloader.Create(Request:HTTPRequest; InBackground:Boolean);
Begin
  _Connection := Nil;
  _Request := Request;
  Self._InBackground := InBackground;

  If InBackground Then
    Engine.Tasks.RunTask(HTTPThread.Create(Self), True)
  Else
    PrepareTransfer();
End;

Procedure HTTPDownloader.PrepareTransfer();
Var
  I:Integer;
  CachedFile:TERRAString;
Begin
  Request.Prepare(_URL, CachedFile);

  If (CachedFile<>'') Then
  Begin
    Engine.Log.Write(logDebug, 'HTTP', 'Cached: '+URL);
    _URL := 'file://' + CachedFile;
  End;

  Engine.Log.Write(logDebug, 'HTTP', 'Starting download to '+URL);
  If Request.Cookie<>'' Then
    Engine.Log.Write(logDebug, 'HTTP', 'Cookie: '+ Request.Cookie);

  _ErrorCode := httpOK;
  _Offset := Dest.Position;
  _Target := Dest;
  _Downloading := False;
  _TotalSize := -1;

  GetMem(_Buffer, BufferSize);

  _Read := 0;
  _TagCount := 0;
  _Progress := 0;

  If (URL='') Or (Dest=Nil) Then
  Begin
    Engine.Log.Write(logError, 'HTTP', 'Invalid download arguments.');
    _ErrorCode := httpInvalidURL;
    Exit;
  End;

  _UpdateTime := Application.GetTime();
  If (Request.Protocol = HTTPProtocol) Then
  Begin
    _Connection := Engine.HTTP.GetConnection(Request.HostName, Request.Port);

    If _Connection._Target = Nil Then
    Begin
      Self.InitTransfer();
    End;

    Exit;
  End Else
  If (Request.Protocol=FILEProtocol) Then
  Begin
    If Not FileStream.Exists(URL) Then
    Begin
      Engine.Log.Write(logError, 'HTTP', 'File not found: '+_URL);
      _ErrorCode := httpNotFound;
      Exit;
    End;

    _FileSource := FileStream.Open(URL);
    _TotalSize := Source.Size;

    _Downloading := (_Progress<100);
  End Else
  Begin
    Engine.Log.Write(logError, 'HTTP', 'Unknown protocol: '+ Request.Protocol);
    _ErrorCode := httpInvalidProtocol;
    Exit;
  End;
End;

Function HTTPDownloader.ReadChunkHeader():Integer;
Var
  I:Integer;
  S:TERRAString;
Begin
  I := Pos(#13#10, _Response);
  S := Copy(_Response, 1, Pred(I));
  _Response := Copy(_Response, (I+2), MaxInt);

  //Log(logDebug, 'HTTP', 'Hex is '+S);

  Result := HexStrToInt(S);

  //Log(logDebug, 'HTTP', 'HexLenght is '+IntToString(Result));
  //Log(logDebug, 'HTTP', 'ResLenght is '+IntToString(Length(_Response)));

  If (Result<=0) Then
    Exit;

  If Length(_Response)>=Result Then
  Begin
    S := Copy(_Response, Result + 3, MaxInt);
    _Response := Copy(_Response, 1, Result + 2);
    Inc(_Read, Length(_Response));
    WriteLeftovers();

    _Response := S;
    Result := Self.ReadChunkHeader();
  End Else
    Dec(Result, Length(_Response));
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
  Tag,Value,S, Header:TERRAString;
  HTTPCode:Integer;
Begin
  Result := False;
  _ChunkedTransfer := False;
  _TotalSize := -1;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  _KeepAlive := False;
  {$ENDIF}

  Repeat
    Len := Source.Read(_Buffer, BufferSize);
    If Len>0 Then
    Begin
      SetLength(_Response, Len);
      Move(_Buffer^, _Response[1], Len);


      Tag := #13#10#13#10;
      I := StringPos(Tag, _Response);
      Header := StringCopy(_Response, 1, Pred(I));
      _Response := StringCopy(_Response, (I+Length(Tag)), MaxInt);

      Engine.Log.Write(logDebug, 'HTTP', 'Full response: '+_Response);

      Tag := StringGetNextSplit(Header, ' '); // HTTP/1.1
      Value := StringGetNextSplit(Header, ' ');

      HTTPCode := StringToInt(Value);
      If (HTTPCode<>200) Then
      Begin
        Result := False;

        Case HTTPCode Of
        400:  Value := 'Bad Request';
        401:  Value := 'Unauthorized';
        403:  Value := 'Forbidden';
        404:  Value := 'Not Found';
        405:  Value := 'Method Not Allowed';
        407:  Value := 'Proxy Authentication Required';
        408:  Value := 'Request Timeout';
        500: Value := 'Internal Server Error';
        501: Value := 'Not Implemented';
        502: Value := 'Bad Gateway';
        503: Value := 'Service Unavailable';
        504: Value := 'Gateway Timeout';
        505: Value := 'HTTP Version Not Supported';
        509: Value := 'Bandwidth Limit Exceeded';
        End;
        _ErrorCode := httpServerError;
        _ErrorMessage := Value;
        Exit;
      End;

      Result := True;
      _TotalSize := 0;

      X:=0;
      While Header<>'' Do
      Begin
        I := Pos(#10, Header);
        If I=0 Then
          I := Length(Header);

        Value := StringTrim(Copy(Header, 1, I));
        If (Value='') Then
        Begin
          Header := Copy(Header, 3, MaxInt);
          Break;
        End;

        Header := Copy(Header, Succ(I), Length(Header));

        I:=Pos(':',Value);
        If (I=0)And(X>0) Then Break;

        Tag := Copy(Value,1,Pred(I));
        Value := Copy(Value, Succ(I), Length(Value));
        Value := StringTrim(Value);

        If (Tag<>'') Then
        Begin
          Inc(_TagCount);
          SetLength(_Tags, _TagCount);
          _Tags[Pred(_TagCount)].Name := Tag;
          _Tags[Pred(_TagCount)].Value := Value;

          If (StringEquals(Tag, 'set-cookie')) Then
          Begin
            Request._Cookie := StringGetNextSplit(Value, ';');
          End;

          If (StringEquals(Tag, 'connection')) And (Assigned(_Connection)) Then
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
            Engine.Log.Write(logDebug, 'HTTP', 'Content-length '+IntegerProperty.Stringify(_TotalSize));
          End;

          If (StringEquals(Tag, 'transfer-encoding')) Then
          Begin
            If (StringEquals(Value, 'chunked')) Then
            Begin
              _ChunkedTransfer := True;
              Engine.Log.Write(logDebug, 'HTTP', 'This is a chunked transfer!');
            End;
          End;
        End;

        Inc(X);
      End;
    End;
  Until (Result) Or (Source = Nil) Or (Source.EOF);

  If (Source = Nil) Then
  Begin
    Engine.Log.Write(logWarning, 'HTTP', 'Connection source disappeared!');
    _ErrorCode := httpBug;
    _Downloading := False;
    Exit;
  End;

  If (_TotalSize<0) And (Not _ChunkedTransfer) Then
  Begin
    _TotalSize := Length(_Response);
    Engine.Log.Write(logDebug, 'HTTP', 'Adjusting content length to '+IntegerProperty.Stringify(_TotalSize));
  End;

  Engine.Log.Write(logDebug, 'HTTP', 'Response Remainder: '+_Response);

  If (Not Result) Then
  Begin
    _ErrorCode := httpConnectionInterrupted;
    Exit;
  End;

  If (_ChunkedTransfer) Then
  Begin
    Engine.Log.Write(logDebug, 'HTTP', 'Entering chunk mode...');
    If (_Response = '') Then
    Begin
      //Log(logDebug, 'HTTP', 'Fetching more...');

      Len := Source.Read(_Buffer, BufferSize);
      If Len>0 Then
      Begin
        SetLength(_Response, Len);
        Move(_Buffer^, _Response[1], Len);
      End;

      //Log(logDebug, 'HTTP', 'Fetched '+IntegerProperty.Stringify(Len));
    End;

    //Log(logDebug, 'HTTP', 'Leftover: '+_Response);

    If (_Response<>'') Then
    Begin
      Len := ReadChunkHeader();
    End Else
    Begin
      _ErrorCode := httpConnectionReadError;
      Result := False;
      Exit;
    End;

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
  Engine.Log.Write(logDebug, 'HTTP', 'Writing leftovers: '+_Response);
  If _Response<>'' Then
  Begin
    _Target.Write(@_Response[1], Length(_Response));
    Inc(_Read, Length(_Response));
    _Response := '';
  End;

  Engine.Log.Write(logDebug, 'HTTP', 'Target position: '+IntegerProperty.Stringify(_Target.Position));
End;

Procedure HTTPDownloader.ContinueTransfer(Count:Integer);
Var
  Len, Count2:Integer;
Begin
  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
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
  {$ENDIF}
 
  While (Count>0) Do
  Begin
    If (Source.EOF) Then
    Begin
      Engine.Log.Write(logWarning, 'HTTP', 'Connection closed!');
      _ErrorCode := httpConnectionReadError;
      _Downloading := False;
      Exit;
    End;

    Count2 := Count;
    If Count2>BufferSize Then
      Count2 := BufferSize;

    Engine.Log.Write(logDebug, 'HTTP', 'Reading '+IntegerProperty.Stringify(Count2));
    Len := Source.Read(_Buffer, Count2);
    Engine.Log.Write(logDebug, 'HTTP', 'Got '+IntegerProperty.Stringify(Len));
    If Len>0 Then
    Begin
      _UpdateTime := Application.GetTime;
      _Target.Write(_Buffer, Len);
      Inc(_Read, Len);
      Dec(Count, Len);

      {If (_ChunkedTransfer) And (Count=0) Then
      Begin
        Len := _Stream.ReadWord(Temp);
        IntegerProperty.Stringify(Len);
      End;}
    End;
  End;

  If (_ChunkedTransfer) Then
  Begin
    _TotalSize := _Read;
    If (Source.EOF) Or (Count=0) Then
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

  If (_TotalSize = 0) Then
  Begin
    _Progress := 100;
    Exit;
  End;

  If (_ChunkedTransfer) Then
  Begin
    Len := Source.Read(_Buffer, 20);
    If (Len>0) Then
    Begin
      SetLength(_Response, Len);
      Move(_Buffer^, _Response[1],Len);
      I := Pos(#13#10, _Response);
      S := Copy(_Response, 1, Pred(I));
      _Response := Copy(_Response, I + 2, MaxInt);

      Count := HexStrToInt(S);

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

Procedure HTTPDownloader.Release();
Begin
  Engine.Log.Write(logDebug, 'HTTP', 'Invokating callback');
  Request._Callback(Self);
  Request._Callback := Nil;

  ReleaseObject(_Target);
  ReleaseObject(_Request);

  Engine.Log.Write(logDebug, 'HTTP', 'Releasing transfer: '+URL);

  If Assigned(_Buffer) Then
    FreeMem(_Buffer);

  If (Assigned(_Connection)) Then
  Begin
  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
    _Connection._LastUpdate := GetTime();
    _Connection._Alive := False;
  {$ELSE}
    ReleaseObject(_Connection);
  {$ENDIF}
  End;

  If Assigned(_FileSource) Then
  Begin
    Engine.Log.Write(logDebug, 'HTTP', 'Releasing stream for '+URL);
    ReleaseObject(_FileSource);
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
  S:TERRAString;
  N:Integer;
Begin
  _UpdateTime := Application.GetTime();
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

  If (Source.EOF) Then
  Begin
    Engine.Log.Write(logError, 'HTTP', 'Connection failed: '+URL);
    _ErrorCode := httpConnectionFailed;
    Exit;
  End;

  Engine.Log.Write(logDebug, 'HTTP', 'Sending request to '+URL);

  Inc(_Connection._RequestCount);
  Engine.Log.Write(logDebug, 'HTTP', 'Request count: '+IntegerProperty.Stringify(_Connection._RequestCount));

  S := Request.GetMethodString() +' '+ Request.FilePath+' HTTP/1.1'+#13#10;
  S := S + 'User-Agent: ' + Request.ClientName + #13#10;
  S := S + 'Host: '+ Request.HostName + #13#10;
  S := S + 'Accept: text/html, text/xml, image/gif, image/x-xbitmap, image/jpeg, */*'#13#10;

  If (Request.Cookie<>'') And (Pos('=', Request.Cookie)>0) Then
    S := S + 'Cookie: ' + Request.Cookie + #13#10;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  S := S + 'Connection: keep-alive'+#13#10;
  {$ENDIF}

  If (Request._Data<>'') Then
  Begin
    S := S + 'Content-Type: application/x-www-form-urlencoded'#13#10;
    S := S + 'Content-Length: '+IntegerProperty.Stringify(Length(Request._Data))+#13#10;
  End;

  S := S + #13#10;

  If (Request._Data<>'') Then
  Begin
    S := S + Request._Data;
  End;

  _Connection._LastUpdate := Application.GetTime();

  N := Length(S);
  If (Source.Write(@S[1], N)<N) Then
  Begin
    Self.RetryTransfer();
    Exit;
  End;

  If (ReceiveHeader()) Then
  Begin
    _Downloading := (_TotalSize>0) And (_Progress<100);
  End Else
  Begin
    If (_Connection._RequestCount<=1) Then
    Begin
      Self.RetryTransfer();
      Exit;
    End Else
      Engine.Log.Write(logDebug, 'HTTP', 'Download failed: '+URL);
  End;
End;

Procedure HTTPDownloader.RetryTransfer;
Begin
  Self._ErrorCode := httpOK;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  If Assigned(_Connection) Then
    _Connection._Alive := False;
  {$ELSE}
  ReleaseObject(_Connection);
  {$ENDIF}

  Engine.Log.Write(logDebug, 'HTTP', 'Retrying download: '+URL);

  _Connection := Engine.HTTP.GetConnection(Request.Hostname, Request.Port);
  Self.InitTransfer();
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

Function HTTPDownloader.GetDest:TERRAStream;
Begin
  Result := Self._Request._Dest;
End;

Function HTTPDownloader.GetStream:TERRAStream;
Begin
  If Assigned(_Connection) Then
    Result := _Connection._Socket
  Else
    Result := Self._FileSource;
End;



{ HTTPConnection }
Constructor HTTPConnection.Create(const HostName: TERRAString; Port: Integer);
Begin
  _Host := HostName;
  _Port := Port;
  _Target := Nil;
  _Alive := True;

  Engine.Log.Write(logDebug, 'HTTP', 'Opening connection to '+ _Host);

  _Socket := NetSocket.Create(_Host, Port);
  _Socket.SetBlocking(True);

  _LastUpdate := Application.GetTime();
End;

Procedure HTTPConnection.Release;
Begin
  Engine.Log.Write(logDebug, 'HTTP', 'Closing connection to '+ _Host);

  ReleaseObject(_Socket);
  Engine.Log.Write(logDebug, 'HTTP', 'Closed connection to '+ _Host);
End;

{ HTTPRequest }
Constructor HTTPRequest.Create(const URL: TERRAString);
Begin
  _URL := URL;
  _Cookie := '';
  _Dest := Nil;
  _Callback := Nil;
  _Port := HTTPPort;
  _AllowCache := True;
  _ClientName := DefaultClientName;
  _MethodType := HTTP_GET;
  _Arguments := HTTPArgumentList.Create();
End;

Function HTTPRequest.GetFullURL: TERRAString;
Var
  Args:TERRAString;
Begin
  Result := Self._URL;
  Args := Arguments.GetQueryString(True);

  If (Self._MethodType = HTTP_GET) Then
  Begin
    _Data := '';
    If Args<>'' Then
      Result := Result + '?' + Args;
  End Else
    _Data := Args;
End;

Function HTTPRequest.GetMethodString: TERRAString;
Begin
  If Self._MethodType = HTTP_GET Then
    Result := 'GET'
  Else
  If Self._MethodType = HTTP_POST Then
    Result := 'POST'
  Else
  If Self._MethodType = HTTP_PUT Then
    Result := 'PUT'
  Else
    Result := 'DELETE';
End;

Procedure HTTPRequest.Prepare(Out URL, CacheFile:TERRAString);
Var
  I:Integer;
Begin
  URL := Self.GetFullURL();
  CacheFile := '';

  {NoCache := StringContains('.php', RequestURL);
  If (NoCache) Then
  Begin
    Request._AllowCache := False;
    FileName := '';
    CachedFile := '';
  End Else
  Begin
    FileName := RequestURL;
    StringReplaceText(StringFromChar(HTTPSeparator), PathSeparator, FileName);
    FileName := GetFileName(FileName, False);
    CachedFile := FileManager.Instance.SearchResourceFile(FileName);
  End;}

  StringReplaceChar('\', HTTPSeparator, URL);

  _HostName := URL;
  // extract http:// from url
  ExtractProtocol(_HostName, _Protocol);

  I := StringCharPos(HTTPSeparator, _HostName);
  _FilePath := StringCopy(_HostName, I, MaxInt);

  // Extract hostname from url
  _HostName := StringCopy(_HostName, 1, Pred(I));

  _FileName := _FilePath;
  I := StringCharPos('?', _FileName);
  If I>0 Then
    _FileName := StringCopy(_FilePath, 1, Pred(I));

  I := StringCharPosReverse(HTTPSeparator, _FileName);
  If I>0 Then
    _FileName := StringCopy(_FileName, Succ(I), MaxInt);

  If (_Dest = Nil) Then
  Begin
    If _Arguments._Count = 0 Then
      _Dest := FileStream.Create(GetTempPath + PathSeparator + FileName)
    Else
    Begin
      _Dest := MemoryStream.Create(1024);
    End;
  End;
End;

Procedure HTTPRequest.Release;
Begin
  ReleaseObject(_Arguments);
End;

Procedure HTTPRequest.SetCallback(Callback: DownloadCallback);
Begin
  Self._Callback := Callback;
End;

Procedure HTTPRequest.SetCookie(const Cookie: TERRAString);
Begin
  Self._Cookie := Cookie;
End;

Procedure HTTPRequest.SetMethod(const MethodType: HTTPMethod);
Begin
  Self._MethodType := MethodType; 
End;

Procedure HTTPRequest.SetTarget(Target:TERRAStream);
Begin
  Self._Dest := Target;
End;

{ HTTPArgumentList }
Procedure HTTPArgumentList.Release();
Begin
  Self.Clear();
End;

Procedure HTTPArgumentList.Clear();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Items[I]);

  _Count := 0;
End;

Function HTTPArgumentList.GetQueryString(SafeEncode:Boolean):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=0 To Pred(_Count) Do
  Begin
    If I>0 Then
      Result := Result + '&';

    Result := Result + _Items[I]._Name+'='+_Items[I].GetValue(SafeEncode);
  End;
End;

Function HTTPArgumentList.Get(const ArgName: TERRAString): HTTPArgument;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
  If (StringEquals(_Items[I]._Name, ArgName)) Then
  Begin
    Result := _Items[I];
    Exit;
  End;

  Result := Nil;
End;

Procedure HTTPArgumentList.AddArg(const ArgName: TERRAString; Arg: HTTPArgument);
Begin
  Arg._Name := ArgName;
  Inc(_Count);
  SetLength(_Items, _Count);
  _Items[Pred(_Count)]:= Arg;
End;

Procedure HTTPArgumentList.AddBoolean(const ArgName: TERRAString; Value: Boolean);
Begin
  Self.AddArg(ArgName, HTTPBooleanArgument.Create(Value));
End;

Procedure HTTPArgumentList.AddCardinal(const ArgName: TERRAString; Value: Cardinal);
Begin
  Self.AddArg(ArgName, HTTPCardinalArgument.Create(Value));
End;

Procedure HTTPArgumentList.AddInteger(const ArgName: TERRAString; Value: Integer);
Begin
  Self.AddArg(ArgName, HTTPIntegerArgument.Create(Value));
End;

Procedure HTTPArgumentList.AddStream(const ArgName: TERRAString; Value:TERRAStream);
Begin
  Self.AddArg(ArgName, HTTPStreamArgument.Create(Value));
End;

Procedure HTTPArgumentList.AddString(const ArgName, Value: TERRAString);
Begin
  Self.AddArg(ArgName, HTTPStringArgument.Create(Value));
End;

{ HTTPIntegerArgument }
Constructor HTTPIntegerArgument.Create(Value: Integer);
Begin
  Self.Value := Value;
End;

Function HTTPIntegerArgument.GetValue(SafeEncode:Boolean): TERRAString;
Begin
  Result := IntegerProperty.Stringify(Self.Value);
End;

{ HTTPCardinalArgument }
Constructor HTTPCardinalArgument.Create(Value: Cardinal);
Begin
  Self.Value := Value;
End;

Function HTTPCardinalArgument.GetValue(SafeEncode:Boolean): TERRAString;
Begin
  Result := CardinalToString(Self.Value);
End;

{ HTTPBooleanArgument }
Constructor HTTPBooleanArgument.Create(Value: Boolean);
Begin
  Self.Value := Value;
End;

Function HTTPBooleanArgument.GetValue(SafeEncode:Boolean): TERRAString;
Begin
  If Self.Value Then
    Result := '1'
  Else
    Result := '0';
End;

{ HTTPStringArgument }
Constructor HTTPStringArgument.Create(const Value: TERRAString);
Begin
  Self.Value := Value;
End;

Function HTTPStringArgument.GetValue(SafeEncode:Boolean): TERRAString;
Begin
  If SafeEncode Then
    Result := StringEncodeHTML(Self.Value)
  Else
    Result := Self.Value;
End;

{ HTTPStreamArgument }
Constructor HTTPStreamArgument.Create(Value:TERRAStream);
Begin
  Self.Value := Value;
End;

Function HTTPStreamArgument.GetValue(SafeEncode:Boolean): TERRAString;
Begin
  Self.Value.ReadContent(Result);
End;

Procedure HTTPStreamArgument.Release;
Begin
  ReleaseObject(Self.Value);
End;

End.
