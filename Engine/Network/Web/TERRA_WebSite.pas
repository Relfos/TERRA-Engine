Unit TERRA_WebSite;

Interface
Uses TERRA_String, TERRA_Utils;

Const
  httpOK          = 200;
  httpBadRequest  = 400;
  httpNotFound    = 404;
  httpDenied      = 403;
  httpRedirect    = 303;

  DefaultContentType = 'text/html';

  CrLf = #13#10;

Type
  HTMLParam = Record
    Name:TERRAString;
    Value:TERRAString;
  End;

  HTMLTransaction = Object
    Protected
      CookieList:Array Of HTMLParam;
      CookieCount:Integer;

      Function GetCookie(Const Name:TERRAString):TERRAString;
      Procedure SetCookie(Const Name,Value:TERRAString);

    Public
      Procedure Reset;

      Procedure SetCookies(CookieStr:TERRAString);
      Function GetCookies:TERRAString;

      Property Cookies[Const Name:TERRAString]:TERRAString Read GetCookie Write SetCookie;
  End;

  HTMLRequest = Object(HTMLTransaction)
    Protected
      FieldList:Array Of HTMLParam;
      FieldCount:Integer;

      Function GetField(Const Name:TERRAString):TERRAString;
      Procedure SetField(Const Name,Value:TERRAString);

    Public
      Page:TERRAString;

      Procedure Reset;

      Property Fields[Const Name:TERRAString]:TERRAString Read GetField Write SetField;
  End;

  HTMLResponse = Object(HTMLTransaction)
    Public
      Content:TERRAString;       //content (html page, image etc., or new location if status=3),
                            // default is empty string
      ContentType:TERRAString;   //HTTP Content-Type field, default is "text/html"
                            //for images you should set image/jpeg or image/gif
                            // (although most browsers will guess the right format from the file extension)
                            //other examples:  application/msword etc., see.RFC2616

      Status:Integer;
  End;

  WebSite = Class
    Protected
      _Title:TERRAString;
      _WebFolder:TERRAString;

      Function HtmlHeader(Extra:TERRAString=''):TERRAString;
      Function LoadFileToString(Name:TERRAString):TERRAString;

      Function WebPageError(Page:TERRAString):TERRAString;Virtual;

    Public
      Constructor Create(Title:TERRAString);
      Destructor Destroy;Override;

      Function PageGet(Request:HTMLRequest):HTMLResponse; Virtual;
      Function PagePost(Request:HTMLRequest):HTMLResponse; Virtual;

      Property WebFolder:TERRAString Read _WebFolder Write _WebFolder;
    End;

Procedure ReplaceStr(var S: String; S1, S2: String);
Function StripHTTP(Path:TERRAString):TERRAString;

Implementation
Uses TERRA_Log, TERRA_FileStream, TERRA_FileUtils;

Function StripHTTP(Path:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  I:=Pos('/',Path);
  While I<>0 Do
  Begin
    Path:=Copy(Path,I+1,Length(Path)-I);
    I:=Pos('/',Path);
  End;
  Result:=Path;
End;

Procedure ReplaceStr(var S: String; S1, S2: String);
Var
  I:Integer;
  Sl,Sr:TERRAString;
Begin
  I:=Pos(s1,s);
  If I=0 Then
    Exit;
  Sl:='';Sr:=s;
  Repeat
    Sl:=Sl+Copy(sr,1,i-1)+S2;
    Sr:=Copy(Sr,I+Length(S1),Length(Sr));
    I:=Pos(s1,sr);
  Until I=0;
  S:=Sl+Sr;
End;

Function WebSite.HtmlHeader(Extra:TERRAString):TERRAString;
Begin
  Result:='<head>';
  Result:=Result + CrLf +'<meta http-equiv="Content-Type" ';
  Result:=Result + CrLf +'content="text/html; charset=iso-8859-1"> ';
  Result:=Result + CrLf +'<meta name="GENERATOR" content="'+_Title+'"> ';
  Result:=Result + CrLf +'<title> '+_Title+' </title>';
  If Extra<>'' Then
    Result:=Result + CrLf + Extra;
  Result:=Result + CrLf + '</head>';
End;

//load any file content to string  (used to transfer images,html etc.)
//returns empty string if error
Function WebSite.LoadFileToString(Name:TERRAString):TERRAString;
Var
  Source:FileStream;
  S:TERRAString;
Begin
  If Not FileStream.Exists(Name) Then
  Begin
    Result:='';
    Exit;
  End;

  Source := Filestream.Open(Name);
  SetLength(S, Source.Size);
  Source.Read(@S[1], Source.Size);
  Result := S;
  ReleaseObject(Source);
End;

Function WebSite.WebPageError(Page:TERRAString):TERRAString;
Begin
  If Page<>'' then
    Result:='error: "'+page+'" not found'
  Else
    Result:='error: the page you requested was not found';
End;

//page content, will be returned by server to the browser
Function WebSite.PageGet(Request:HTMLRequest):HTMLResponse;
Begin
  Result.Reset;
  If (Request.Page='')Then
    Request.Page := 'index.htm';

  If Not FileStream.Exists(Request.Page) Then
    Request.Page := WebFolder+'\'+Request.Page;

  //other pages: look for file
  If FileStream.Exists(Request.Page) Then  //ask for login for files in other directories
  Begin
    If GetFileExtension(Request.Page)='jpg' Then
      Result.ContentType:='image/jpeg'
    Else
    If GetFileExtension(Request.Page)='png' Then
      Result.ContentType:='image/png'
    Else
    If GetFileExtension(Request.Page)='doc' Then
      Result.ContentType:='application/msword'
    Else
    If GetFileExtension(Request.Page)='pdf' Then
      Result.ContentType:='application/pdf'
    Else
      Result.ContentType:=DefaultContentType;

    //for other extensions we leave the default text/html
    Result.Content:=LoadFileToString(Request.Page);
    Result.Status:=httpOK;
    Exit;
  End;
  Result.Status:=httpNotFound;
  Result.Content:=WebPageError(Request.Page);

  Log(logWarning, 'WebSite', 'Page not found: '+Request.Page);
End;

Function WebSite.PagePost(Request:HTMLRequest):HTMLResponse;
Begin
  Result.Reset;
  Result.Status:=httpNotFound;
  Result.ContentType:=DefaultContentType;
  Result.Content:=WebPageError(Request.Page); //not found
End;

Constructor WebSite.Create(Title:TERRAString);
Begin
 _Title:=Title;
End;

Destructor WebSite.Destroy;
Begin

End;

Function HTMLTransaction.GetCookie(Const Name:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result:='';
  For I:=0 To Pred(CookieCount) Do
    If (CookieList[I].Name=Name) Then
    Begin
      Result:=CookieList[I].Value;
      Exit;
    End;
End;

Function HTMLTransaction.GetCookies:TERRAString;
Var
  I:Integer;
Begin
  Result:='';
  For I:=0 To Pred(CookieCount) Do
    Result:=CookieList[I].Name+'="'+CookieList[I].Value+'"; Version="1"';
End;

Procedure HTMLTransaction.Reset;
Begin
  CookieCount:=0;
  SetLength(CookieList,0);
  CookieList:=Nil;
End;

Procedure HTMLTransaction.SetCookie(Const Name,Value:TERRAString);
Var
  I,N:Integer;
Begin
  N:=-1;
  For I:=0 To Pred(CookieCount) Do
    If (CookieList[I].Name=Name) Then
    Begin
      N:=I;
      Break;
    End;

  If (N<0) Then
  Begin
    Inc(CookieCount);
    SetLength(CookieList,CookieCount);
    N:=Pred(CookieCount);
    CookieList[N].Name:=Name;
  End;

  CookieList[N].Value:=Value;
End;

// HTMLRequest

Function HTMLRequest.GetField(Const Name:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result:='';
  For I:=0 To Pred(FieldCount) Do
    If (FieldList[I].Name=Name) Then
    Begin
      Result:=FieldList[I].Value;
      Exit;
    End;
End;

Procedure HTMLRequest.Reset;
Begin
  Inherited;

  FieldCount:=0;
  SetLength(FieldList,0);
  FieldList:=Nil;
End;

Procedure HTMLRequest.SetField(Const Name,Value:TERRAString);
Var
  I,N:Integer;
Begin
  N:=-1;
  For I:=0 To Pred(FieldCount) Do
    If (FieldList[I].Name=Name) Then
    Begin
      N:=I;
      Break;
    End;

  If (N<0) Then
  Begin
    Inc(FieldCount);
    SetLength(FieldList,FieldCount);
    N:=Pred(FieldCount);
    FieldList[N].Name:=Name;
  End;

  FieldList[N].Value:=Value;
End;

Procedure HTMLTransaction.SetCookies(CookieStr:TERRAString);
Var
  I:Integer;
  Tag,Value:TERRAString;
Begin
  While CookieStr<>'' Do
  Begin
    //Tag := GetNextWord(CookieStr); FIXME

    If Tag[1]<>'$' Then
    Begin
      I := Pos('=',Tag);
      Value := Copy(Tag,Succ(I),MaxInt);
      Tag := Copy(Tag,1,Pred(I));
      Value := Copy(Value,2,Length(Value)-2);
      Cookies[Tag] := Value;
      Exit;
    End;
  End;
End;

End.
