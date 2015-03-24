Unit TERRA_WebServer;

Interface

Uses TERRA_Utils, TERRA_String, TERRA_WebSite, Terra_Stream, TERRA_MemoryStream;

Const
  HTTPPort=80;
  ServerName='LEAF WebServer v1.3';

Type
  HTTPResponse = Record
    Content:TERRAString;
    KeepAlive:Boolean;
    TimeOut:Integer;
  End;

  HTTPClient = Record
    ID:Cardinal;
    Source:Stream;
    TimeOut:Integer;
  End;

  WebServer = Class
    Private
      _ClientList:Array Of HTTPClient;
      _ClientCount:Integer;

      _LocalSite:WebSite;
      _Port:Integer;
      _NextUpdate:Cardinal;

      _Page:String;
      _Method:String;

      Procedure TranslateCGIcodes(Var S:String);

      Procedure KeepClient(Client:Stream; TimeOut:Integer);
      Procedure DiscardClient(ClientID:Integer);

      Function ProcessClient(Client:Stream):Integer;
      Function ProcessRequest(Request:String):HTTPResponse;

      Function HttpResp(Content:String; Status:Integer):String;Overload;
      Function HttpResp(Resp:HTMLResponse):String;Overload;

    Public
      Constructor Create(Site:WebSite; Port:Integer=HTTPPort);
      Destructor Destroy;Override;
      Procedure Update;

      Property Site:WebSite Read _LocalSite;
      Property Port:Integer Read _Port;
  End;

Implementation
Uses SysUtils, TERRA_Sockets, TERRA_Log, TERRA_OS;

Const
  MaxWaitingClients = 10;
  UpdateTime = 100;

{ WebServer }
Constructor WebServer.Create(Site:WebSite; Port:Integer=HTTPPort);
Begin
  _LocalSite := Site;
  _NextUpdate := GetTime() + UpdateTime;
  _Port := Port;
End;

Destructor WebServer.Destroy;
Begin
End;

Function WebServer.HttpResp(Content:String; Status:Integer):String;
Var
  Resp:HTMLResponse;
Begin
  Resp.Reset;
  Resp.Content:=Content;
  Resp.ContentType:=DefaultContentType;
  Resp.Status:=Status;
  Result:=HttpResp(Resp);
End;

//wrap content into HTTP response
Function WebServer.HttpResp(Resp:HTMLResponse):String;
Var
  CookieStr:String;
Begin
  With Resp Do
  Begin
    Case Status Of
      httpOK:         Result:='HTTP/1.1 200 OK';
      httpBadRequest: Result:='HTTP/1.1 400 Bad Request';
      httpDenied:     Result:='HTTP/1.1 403 Access Denied';
      httpRedirect:   Result:='HTTP/1.1 303 Redirect';
      Else            Result:='HTTP/1.1 404 Not Found'; //use not found by default
    End;

    If Status=httpRedirect Then  //interpret content as new location
    Begin
      If Content='' Then
        Content:='/'
      Else
      If Content[1]<>'/' Then
        Content:='/'+Content;
    End;

    Result:=Result+CrLf+ 'Server: '+ServerName;
    Result:=Result+CrLf+ 'Date: '+DateToStr(Date);

    If Status=httpRedirect Then
    Begin
      Result:=Result+CrLf+'Location:'+Content;
      Content:='';
    End;

    Result:=Result+CrLf+ 'Content-Type: '+ContentType;
    Result:=Result+CrLf+ 'Accept-Ranges: bytes';
    Result:=Result+CrLf+ 'Last-Modified: '+DateTimeToStr(Now);

    CookieStr:=GetCookies;
    If CookieStr<>'' Then
      Result:=Result+CrLf+ 'Set-Cookie: '+CookieStr;

    Result:=Result+CrLf+ 'Content-Length: '+ IntToStr(Length(Content));
    Result:=Result+CrLf;
    Result:=Result+CrLf+Content;
  End;
End;

//translates back special chars in forms (CGI convention)
Procedure WebServer.TranslateCGIcodes(Var S:String);
Begin
  ReplaceStr(S,'%2B','+');
  ReplaceStr(S,'%3D','=');
  ReplaceStr(S,'%26','&');
  ReplaceStr(S,'%28','(');
  ReplaceStr(S,'%29',')');
  ReplaceStr(S,'+',' ');
  ReplaceStr(S,'%25','%');
  ReplaceStr(S,'%23','#');
  ReplaceStr(S,'%24','$');
  ReplaceStr(S,'%27','''');
  ReplaceStr(S,'%22','"');
  ReplaceStr(S,'%5B','[');
  ReplaceStr(S,'%5D',']');
  ReplaceStr(S,'%2C',',');
  ReplaceStr(S,'%3A',':');
  ReplaceStr(S,'%3B',';');
  ReplaceStr(S,'%21','!');
  ReplaceStr(S,'%3F','?');
  ReplaceStr(S,'%2F','/');
  ReplaceStr(S,'%5C','\');
  //list not exhaustive
End;

Procedure WebServer.DiscardClient(ClientID:Integer);
Begin
  Log(logDebug, 'WebServer',' DiscardClient: '+HexStr(_ClientList[ClientID].ID));

  ReleaseObject(_ClientList[ClientID].Source);

  If (_ClientCount>1) Then
    _ClientList[ClientID] := _ClientList[Pred(_ClientCount)];

  Dec(_ClientCount);
  SetLength(_ClientList,_ClientCount);
End;

Procedure WebServer.KeepClient(Client:Stream; TimeOut:Integer);
Begin
  Inc(_ClientCount);
  SetLength(_ClientList,_ClientCount);
  _ClientList[Pred(_ClientCount)].Source := Client;
  _ClientList[Pred(_ClientCount)].TimeOut := TimeOut;
  _ClientList[Pred(_ClientCount)].ID := GetTime();
  Log(logDebug, 'WebServer', 'KeepClient: '+HexStr(_ClientList[Pred(_ClientCount)].ID));
End;

Function WebServer.ProcessRequest(Request:String):HTTPResponse;
Var
  Resp:HTMLResponse;
  KeepAlive:Boolean;

  I,TimeOut:Integer;
  Field,Value:String;
  Req:HTMLRequest;
Begin
  TimeOut:=300;
  Result.Content:='';
  Result.KeepAlive:=False;

  //check if valid HTTP request
  If Pos('HTTP',Request)=0 Then
  Begin       //invalid: send message and close (not really compliant with HTTP1.1 but easier)
    Log(logWarning, 'WebServer', 'Invalid request');
    Result.Content:=HTTPResp(Servername+': not HTTP request:'+Crlf+Request,httpBadRequest);
    Result.KeepAlive:=False;
    Exit;
  End;

  StringSplit(Request, Field, Request, CrLf, False);

  _Method := StringGetNextSplit(Field, Ord(' '));
  _Page := StringGetNextSplit(Field, Ord(' '));

  If (_Page<>'')And (_Page[1]='/') Then
    Delete(_Page,1,1);   // remove leading "/"
  ReplaceStr(_Page,'%20',' ');

  Req.Reset;
  Req.Page:=_Page;
  While (Request<>'') Do
  Begin
    StringSplit(Request, Value, Request, CrLf, False);

    If Value='' Then
      Break;

    Field := StringGetNextSplit(Value, Ord(' '));
    Field := Copy(Field,1,Pred(Length(Field)));

    If (StringEquals(Field, 'Keep-Alive')) Then
      TimeOut := StringToInt(Value)
    Else
    If (StringEquals(Field, 'Connection')) Then
      KeepAlive := Not (StringEquals(Value,'close'))
    Else
    If (StringEquals(Field, 'Cookie')) Then
      Req.SetCookies(Value);
  End;

  //If-modified-since
  //'12-05-2007 23:01:06'

  // we have got method and page name
  If (_Method='GET')Or(_Method='HEAD') Then
  Begin
    // call web page generator
    Resp:=_LocalSite.PageGet(Req);
    If (_Method='HEAD') Then
      Resp.Content:='';
  End Else
  If _Method='POST' Then
  Begin
    While Request<>'' Do
    Begin
      I:=Pos('&',Request);
      If I<=0 Then
        I:=Succ(Length(Request));

      Field:=Copy(Request,1,Pred(I));
      Request:=Copy(Request,Succ(I),MaxInt);

      I:=Pos('=',Field);
      Value:=Copy(Field,Succ(I),MaxInt);
      Field:=Copy(Field,1,Pred(I));
      TranslateCGIcodes(Value);

      Req.Fields[Field]:=Value;
    End;

    // call web page generator
    Resp:=_LocalSite.PagePost(Req);
  End Else
  Begin //everything else (PUT etc.) treated as unsupported
    Log(logWarning, 'WebServer', 'Method not supported ['+_Method+']');
    Resp.Content:=ServerName+': method '+ _Method +' unsupported';
    Resp.Status:=httpBadRequest;
  End;

  //send response
  Result.Content:=HttpResp(Resp);
  Result.KeepAlive:=KeepAlive;
  Result.TimeOut:=TimeOut;
End;

Function WebServer.ProcessClient(Client:Stream):Integer;
Const
  BufferSize=128;
Var
  Len,K:Integer;
  Buffer:String[BufferSize];
  Return:HTTPResponse;
  Request,Remaining:String;
  RequestComplete:Boolean;
Begin
  Result:=0;
  Remaining:='';

  Repeat
    Request:=Remaining;
    Repeat
      Len := Client.Read(@Buffer[1], BufferSize);
      Buffer[0] := Char(Len);
      Request:=Request+Buffer;
      If (Copy(Request,1,4)='POST') Then
      Begin
        RequestComplete:=(Len=0);
        Remaining:='';
      End Else
      Begin
        K:=Pos(CrLf+CrLf,Request);
        RequestComplete:=(Len=0)Or(K>0);
        If RequestComplete Then
          Remaining:=Copy(Request,K+4,MaxInt);
      End;
    Until (RequestComplete);

    If Request<>'' Then
    Begin
      Return:=ProcessRequest(Request);
      Result:=Return.TimeOut;
      If Return.Content<>'' Then
      Begin
        Len:=Length(Return.Content);
        Client.Write(@Return.Content[1],Len);
        Log(logDebug, 'WebServer', 'Processing request '+_Method+' :['+_Page+']');
      End Else
        Log(logError, 'WebServer','Cannot handle request');
    End Else
      Result:=0;

  Until (Remaining='');
End;

Procedure WebServer.Update;
Var
  I,TimeOut:Integer;
  Client:Stream;
Begin
  If GetTime<_NextUpdate Then
    Exit;

  I:=0;
  While (I<_ClientCount) Do
  Begin
    If (GetTime>_ClientList[I].TimeOut) Then
      DiscardClient(I)
    Else
    Begin
      TimeOut := ProcessClient(_ClientList[I].Source);
      If TimeOut<=0 Then
        DiscardClient(I)
      Else
      Begin
        _ClientList[I].TimeOut:=GetTime+TimeOut;
        Inc(I);
      End;
    End;
  End;

  Client:=OpenIncomingStream(_Port);
  If Assigned(Client) Then
  Begin
    TimeOut:=ProcessClient(Client);
    If TimeOut<=0 Then
      Client.Destroy
    Else
      KeepClient(Client,GetTime+TimeOut);
  End;

  _NextUpdate:=GetTime+UpdateTime;
End;

End.
