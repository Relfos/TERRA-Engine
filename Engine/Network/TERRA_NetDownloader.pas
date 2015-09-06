Unit TERRA_NetDownloader;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_HTTP, TERRA_Mutex;

Type
  DownloadManager = Class(TERRAObject)
    Protected
      _Downloads:Array Of HTTPDownloader;
      _DownloadCount:Integer;

      _Mutex:CriticalSection;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
      _Connections:Array Of HTTPConnection;
      _ConnectionCount:Integer;
  {$ENDIF}

      Function InvokeCallbackOnMainThread(Obj:TERRAObject):Boolean;

      Function GetOverallProgress:Integer;

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
      Function HasDownloadsWithConnection(Connection:HTTPConnection):Boolean;
      Procedure InterruptConnections(Connection:HTTPConnection);

      Procedure ClearConnectionsToDownload(Download:HTTPDownloader);
  {$ENDIF}

      Procedure DummyCallback(Download:HTTPDownloader);

    Public
      Constructor Create;
      Procedure Release; Override;

      Procedure Update;

      Procedure Flush;

      Function GetDownload(Index:Integer):HTTPDownloader;

      Function StartRequest(Request:HTTPRequest; InBackground:Boolean):HTTPDownloader;
      Function GetConnection(Const HostName:TERRAString; Port:Integer):HTTPConnection;

      Property Count:Integer Read _DownloadCount;
      Property Progress:Integer Read GetOverallProgress;
  End;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_MemoryStream;

{ DownloadManager }
Constructor DownloadManager.Create;
Begin
  _Mutex := CriticalSection.Create();
  _DownloadCount := 0;
End;

Procedure DownloadManager.Release;
Var
  I:Integer;
Begin
  _Mutex.Lock();
  For I:=0 To Pred(_DownloadCount) Do
    _Downloads[I].Request.SetCallback(DummyCallback);
  _Mutex.Unlock();

  Self.Flush();

  For I:=0 To Pred(_DownloadCount) Do
    ReleaseObject(_Downloads[I]);

  ReleaseObject(_Mutex);
End;

Procedure DownloadManager.DummyCallback(Download:HTTPDownloader);
Begin
  // do nothing
End;


Function DownloadManager.InvokeCallbackOnMainThread(Obj:TERRAObject):Boolean;
Begin
  ReleaseObject(Obj);
  Result := False;
End;

Procedure DownloadManager.Update();
Var
  Remove:Boolean;
  I, J:Integer;
Begin
  //Application.Instance.Yeld();

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  I := 0;
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
      ReleaseObject(_Connections[I]);
      _Connections[I] := _Connections[Pred(_ConnectionCount)];
      Dec(_ConnectionCount);
    End Else
      Inc(I);
  End;
  {$ENDIF}

  I:=0;
  _Mutex.Lock();
  While (I<_DownloadCount) Do
  Begin
    Remove := False;

    If (_Downloads[I].ErrorCode<>httpOk) Then
    Begin
      Log(logDebug, 'HTTP', 'Download error :'+_Downloads[i].URL+' -> error ' +IntegerProperty.Stringify(Integer(_Downloads[I].ErrorCode)));
      Remove := True;
    End Else
    If (Not _Downloads[I].InBackground) And (_Downloads[I].TotalSize<0) Then
    Begin
      _Downloads[I].InitTransfer();
    End Else
    Begin
      {If (_Downloads[I]._Progress<100) And (GetTime - _Downloads[I]._UpdateTime>ConnectionTimeOut) Then
      Begin
        Log(logWarning, 'HTTP', 'Connection time out :'+_Downloads[i]._URL);
        _Downloads[I]._ErrorCode := httpConnectionTimeOut;
      End;}

      If (Not _Downloads[I].InBackground) Then
      Begin
        _Downloads[I].Update();
      End;

      If (_Downloads[I].Progress>=100) Then
      Begin
        Remove := True;

        Log(logDebug, 'HTTP', 'Download finished :'+_Downloads[i].URL);
        Log(logDebug, 'HTTP', 'Total size: '+IntegerProperty.Stringify(_Downloads[i].Target.Position));

        If (_Downloads[I].Target Is MemoryStream) Then
        Begin
          Log(logDebug, 'HTTP', 'Truncating memory stream');
          _Downloads[I].Target.Truncate();
        End;

        Log(logDebug, 'HTTP', 'Seeking to zero');
        _Downloads[I].Target.Seek(_Downloads[I].Offset);
      End;
    End;

    If (Remove) Then
    Begin
    {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
      Self.ClearConnectionsToDownload(_Downloads[I]);
    {$ENDIF}

      If Assigned(_Downloads[I].Request.Callback) Then
        Application.Instance.PostCallback(InvokeCallbackOnMainThread, _Downloads[I])
      Else
        ReleaseObject(_Downloads[I]);

      For J:=I To (_DownloadCount-2) Do
        _Downloads[J] := _Downloads[Succ(J)];

      Dec(_DownloadCount);
    End Else
      Inc(I);
  End;
  _Mutex.Unlock();
End;

Function DownloadManager.StartRequest(Request:HTTPRequest; InBackground:Boolean):HTTPDownloader;
Begin
  If Request = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := HTTPDownloader.Create(Request, InBackground);

  _Mutex.Lock();
  Inc(_DownloadCount);
  SetLength(_Downloads, _DownloadCount);
  _Downloads[Pred(_DownloadCount)] := Result;

  _Mutex.Unlock();
  Log(logDebug, 'HTTP', 'Download prepared.');
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

  _Mutex.Lock();

  Result := 0;
  For I:=0 To Pred(_DownloadCount) Do
    Inc(Result, _Downloads[I].Progress);

  Result := Result Div _DownloadCount;

  _Mutex.Unlock();
End;

Function DownloadManager.GetDownload(Index: Integer): HTTPDownloader;
Begin
  _Mutex.Lock();
  If (Index<0) Or (Index>=_DownloadCount) Then
    Result := Nil
  Else
    Result := _Downloads[Index];

  _Mutex.Unlock();
End;

Function DownloadManager.GetConnection(const HostName: TERRAString; Port: Integer): HTTPConnection;
Var
  I:Integer;
Begin
  _Mutex.Lock();
  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  For I:=0 To Pred(_ConnectionCount) Do
  If (_Connections[I]._Alive)  And (StringEquals(_Connections[I]._Host, HostName)) And (_Connections[I]._Port = Port) Then
  Begin
    Result := _Connections[I];
    _Mutex.Unlock();
    Exit;
  End;
  {$ENDIF}

  Result := HTTPConnection.Create(HostName, Port);

  {$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
  Inc(_ConnectionCount);
  SetLength(_Connections, _ConnectionCount);
  _Connections[Pred(_ConnectionCount)] := Result;
  {$ENDIF}

  _Mutex.Unlock();
End;

{$IFDEF ALLOW_PERSISTENT_CONNECTIONS}
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
{$ENDIF}
End.


