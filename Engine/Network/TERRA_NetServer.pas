Unit TERRA_NetServer;

{$I terra.inc}

{$DEFINE NETDEBUG}
{$DEFINE SAVEPACKETS}


Interface
Uses TERRA_Application, TERRA_OS, TERRA_IO, TERRA_Sockets, TERRA_Network,
  TERRA_ThreadPool, TERRA_Utils, TERRA_Classes
  {$IFDEF SAVEPACKETS},TERRA_NetLogger{$ENDIF};

Type
  NetPacket = Record
    Msg:LNetMessage;
    Dest:SocketAddress;
    Sock:Socket;
    Time:Cardinal;
  End;

  // Client Info
  ClientInfo=Class
    ID:Word;
    UserName:AnsiString;
    Password:AnsiString;
    DeviceID:AnsiString;
    GUID:Word;
    Time:Cardinal;    //Last contact time
    Ping:Integer;     //If ping<0 then Client connection is dead
    Socket:TERRA_Sockets.Socket;
    Address:SocketAddress;
    Frames:Integer;
    UserData:Pointer;  //Not needed, Application specific use
  End;

  NetServer = Class(NetObject)
    Protected
      _Mutex:CriticalSection;
      _ClientList:Array Of ClientInfo; // Clients list
      _ClientCount:Integer;             // Max number of Clients allowed
      _RefreshTime:Cardinal;            // Search for dead connections time
      _WaitingConnections:Array Of Socket;
      _WaitingCount:Integer;

      _Packets:Array Of NetPacket;
      _PacketCount:Integer;
      _PacketMutex:CriticalSection;
      _PacketKeep:Boolean;
      _PacketImportant:Array[0..255] Of Boolean;
      _LastPacketUpdate:Cardinal;

      Procedure RemoveClient(Client:ClientInfo); //Event: When a Client leaves the server
      Function OnSendFail(Dest:SocketAddress; Sock:Socket; Msg:PNetMessage):Boolean; Override;
      Procedure OnPacketReceived(Sock:Socket; Msg: PNetMessage); Override;
      Function IsPacketImportant(Msg:PNetMessage):Boolean; Virtual;

      Procedure SendDroppedPackets;

    Public
      // Creates a new server instance
      Constructor Create(Name:AnsiString; Version,Port:Word; MaxClients:Word);

      // Handles messages
      Procedure Update; Override;

      // Send a message
      Procedure SendMessage(Msg:PNetMessage; ClientID:Word); Overload; Virtual;
      Procedure SendMessage(ClientID:Word; Opcode:Byte; Src:Stream; Owner:Word = 0); Overload;

      // Broadcast a message
      Procedure BroadcastMessage(Msg:PNetMessage);

      // Removes a Client from the server
      Procedure Kick(ClientID:Word);

      // Validates client username/password
      Function ValidateClient(UserName,Password,DeviceID:AnsiString; Var ErrorLog:AnsiString):Integer; Virtual;

      Function ValidateMessage(Msg:PNetMessage):Boolean; Override;

      // Shutdown a server and destroy it
      Destructor Destroy(ErrorCode:Word=errServerShutdown); Reintroduce;

      // Message handlers
      Procedure PingMessage(Msg:PNetMessage; Sock:Socket);
      Procedure JoinMessage(Msg:PNetMessage; Sock:Socket);
      Procedure DropMessage(Msg:PNetMessage; Sock:Socket);

      // Gets local Client
      Function GetClient(ID:Word):ClientInfo;
      Function GetClientByUsername(Name:AnsiString):ClientInfo;
      Function GetClientAddress(ID:Word):Cardinal;
      Function CreateIterator:Iterator;

      Function GetConnectedClients():Integer;

      // Network Events
      Procedure OnClientAdd(Client:ClientInfo);Virtual;    //Event: When a new Client enter the server
      Procedure OnClientRemove(Client:ClientInfo);Virtual; //Event: When a Client leaves the server

      Property ClientCount:Integer Read _ClientCount;
    End;

  NetServerIterator=Class(Iterator)
    Protected
      Server:NetServer;
      Pos,Count:Integer;

    Public
      Function HasNext:Boolean;Override;
      Function GetNext:ListObject;Override;
      Procedure Discard(Release:Boolean=True);Override;
  End;


Implementation
Uses TERRA_Log, TERRA_MultiThreadedServer, TERRA_NetReplayServer;


{********************
  LNetServer Class
 ********************}
Procedure NetServer.PingMessage(Msg:PNetMessage; Sock:Socket);
Var
  Client:ClientInfo;
Begin
  Client := GetClient(Msg.Owner);
  If Not Assigned(Client) Then
    Exit;

  Client.Ping := Client.Time;
  Client.Time := GetTime;
  Client.Ping := Integer(Client.Time) - Client.Ping;
End;

Procedure NetServer.JoinMessage(Msg:PNetMessage; Sock:Socket);
Var
  N,I,K:Integer;
  S:Stream;
  Guid, Version:Word;
  UserName,Password, DeviceID:AnsiString;
  Client:ClientInfo;
  Rm:LNetMessage;
  ErrorLog:AnsiString;
Begin
  S := MemoryStream.Create(Msg.Size, @(Msg.Data[0]));
  S.Read(@GUID, 2);
  S.Read(@Version, 2);
  S.ReadString(Username);
  S.ReadString(Password);
  If (Not S.EOF) Then
    S.ReadString(DeviceID)
  Else
    DeviceID := '';
  S.Destroy;

  If (Sock<>Nil) Then
  Begin
    For I:=1 To _ClientCount Do  //Search for duplicated GUIDs
    Begin
      Client := GetClient(I);
      If (Assigned(Client)) And(Client.Socket = Sock) Then
        Exit;
    End;
  End;

{  N:=-1;
  For I:=1 To _ClientCount Do  //Search for duplicated GUIDs
  Begin
    Client := GetClient(I);
    If (Assigned(Client)) And(Client.GUID = GUID) Then
    Begin
      N:=I; //If this GUID is the same,then the Client already is in the server
      Break;
    End;
  End;

  If N<>-1 Then //The Client GUID was found, this Client is already in
  Begin //Return a duplicated GUID error to the client
    CreateMessage(Rm,nmServerError, errDuplicatedGUID);
    ReturnMessage(Sock, @Rm);
    Exit;
  End;}

  If Version<>_Version Then
  Begin //Return a invalid version error to the client
    CreateMessage(Rm,nmServerError,errInvalidVersion);
    ReturnMessage(Sock, @Rm);
    Exit;
  End;

  ErrorLog := '';
  K := ValidateClient(UserName,Password, DeviceID, ErrorLog);
  If K<>0 Then
  Begin
    CreateMessage(Rm, nmServerError, K);
    S := MemoryStream.Create(256, @Rm.Data[2]);
    S.WriteString(ErrorLog);
    Rm.Size := S.Position + 2;
    S.Destroy;
    ReturnMessage(Sock, @Rm);
    Exit;
  End;

  _Mutex.Lock();
  N:=-1;
  For I:=1 To _ClientCount Do  //Search for a dead Client slot
  If _ClientList[I] = Nil Then
  Begin
    N := I; //If this Client is dead, then we can reuse the slot
    Break;
  End;
  _Mutex.Unlock();

  If N=-1 Then
  Begin
    CreateMessage(Rm,nmServerError,errServerFull);
    ReturnMessage(Sock, @Rm);
    Exit;
  End;

  _Mutex.Lock();
  Client := ClientInfo.Create;
  Client.Address := _Sender; // Store the new Client IP
  Client.Ping := 0;          // Reset Client ping
  Client.GUID := GUID;
  Client.Time := GetTime;
  Client.UserName := UserName;
  Client.ID := N;
  Client.UserData := Nil;
  Client.Password := Password;
  Client.DeviceID := DeviceID;
  Client.Socket := Sock;
  Client.Frames := 0;
  _ClientList[N] := Client;
  _Mutex.Unlock();
  {$IFDEF SAVEPACKETS}
  If (Not (Self Is NetworkReplayServer)) Then
    NetworkLogger.Instance.LogConnectionStart(N, UserName, Password, DeviceID);
  {$ENDIF};

  CreateMessage(Rm, nmServerAck, N);
  ReturnMessage(Sock, @Rm);

  If (Not (Self Is NetMultithreadedServer)) Then
    OnClientAdd(Client); //Calls the AddClient event
End;

Procedure NetServer.DropMessage(Msg:PNetMessage; Sock:Socket);
Var
  Client:ClientInfo;
Begin
  Client := GetClient(Msg.Owner);
  If Assigned(Client) Then
    RemoveClient(Client) //Calls the remove Client event
  Else
    RaiseError('Network.'+_Name+'.DropMessage: Invalid Client. ['+IntToString(Msg.Owner)+']');
End;

Constructor NetServer.Create(Name:AnsiString; Version, Port:Word; MaxClients:Word); {Creates a new server instance}
Var
  I:Integer;
Begin
  Inherited Create();

  _LocalId := 0;  //Servers always have a localID of zero
  _NetObject := Self;
  _Name := Name;
  _Port := Port;
  _WaitingCount := 0;
  _ClientCount := MaxClients;
  _RefreshTime := 0;
  _Version := Version;
  _OpcodeList[nmPing] := PingMessage;
  _OpcodeList[nmClientJoin] := JoinMessage;
  _OpcodeList[nmClientDrop] := DropMessage;

  _Mutex := CriticalSection.Create('');
  _PacketMutex := CriticalSection.Create('');

  SetLength(_ClientList, Succ(_ClientCount));
  _ClientList[0] := ClientInfo.Create;
  With _ClientList[0] Do
  Begin
    Name := 'Server';
    Ping := 0;
    GUID := 0;
  End;

  For I:=1 To 255 Do
    _PacketImportant[I] := True;

  _PacketImportant[nmPing] := False;

  For I:=1 To _ClientCount Do
    _ClientList[I] := Nil;
End;

// Validates a message
Function NetServer.ValidateMessage(Msg:PNetMessage):Boolean;
Var
  Client:ClientInfo;
Begin
  {$IFDEF NETDEBUG}WriteLn('Begin validation');{$ENDIF}
  Result := (Msg.Owner=ID_UNKNOWN) And (Msg.Opcode=nmClientJoin);

  If Not Result Then
  Begin
  {$IFDEF NETDEBUG}WriteLn('Calling getClient()');{$ENDIF}
    Client := GetClient(Msg.Owner);
    Result := (Assigned(Client)){ And (Client.Address.Address=_Sender.Address)};

    {$IFDEF NETDEBUG}WriteLn('Testing client');{$ENDIF}
    If Assigned(Client) Then
      Client.Time := GetTime
    Else
      Log(logWarning,'Network',_Name+'.Update: Invalid server message. ['+IntToString(Msg.Owner)+']');
  End;
  {$IFDEF NETDEBUG}WriteLn('End validation');{$ENDIF}
End;

// Handles messages
Procedure NetServer.Update;
Var
  I,J:Integer;
  Msg:PNetMessage;
  Rm:LNetMessage;
  ValidMsg:Boolean;
  Client:ClientInfo;
  Sock:Socket;
Begin
  _PacketKeep := True;

  UpdateIO;

  Sock := OpenIncomingStream(_Port);
  If Assigned(Sock) Then
  Begin
    Sock.SetBlocking(False);
    Sock.SetDelay(False);
    Sock.SetBufferSize(1024*128);
    Inc(_WaitingCount);
    SetLength(_WaitingConnections, _WaitingCount);
    _WaitingConnections[Pred(_WaitingCount)] := Sock;
  End;

  {If (Self.GetConnectedClients()>0) Then
    WriteLn('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Starting update cycle ');}
  For I:=1 To _ClientCount Do // Search dead connections, by pinging Clients
  Begin
    Client := GetClient(I);
    If (Not Assigned(Client)) Or (Client.Socket = Nil) Then
      Continue;

    {$IFDEF NETDEBUG}WriteLn('Processing client: ',Client.Username);{$ENDIF}
    J := 0;
    Repeat
      If ReceivePacket(Client.Socket, @RM) Then
      Begin
        If (IsPacketImportant(@RM)) Then
          Inc(J);
        Client.Time := GetTime();
        Inc(Client.Frames);
      End Else
        Break;
    Until (J>=5);
    {$IFDEF NETDEBUG}WriteLn('Packets received: ',J);{$ENDIF}
  End;

  I:=0;
  While (I<_WaitingCount) Do
  Begin
    {$IFDEF NETDEBUG}WriteLn('Processing waiting connection #',I);{$ENDIF}
    If (ReceivePacket(_WaitingConnections[I], @RM)) Then
    Begin
      _WaitingConnections[I] := _WaitingConnections[Pred(_WaitingCount)];
      Dec(_WaitingCount);
    End Else
      Inc(I);
  End;

  For I:=1 To _ClientCount Do // Search dead connections, by pinging Clients
  Begin
    Client := GetClient(I);
    If (Not Assigned(Client)) Or (Not Assigned(Client.Socket)) Then
        Continue;

    If (Client.Socket.Closed) Or (GetTime() - Client.Time>1000*60*2) Then
    Begin
      Log(logWarning,'Network',_Name+'.Update: ClientID='+IntToString(I)+' is dead.');
      RemoveClient(Client);       // Call RemoveClient event
    End Else
    If (GetTime-Client.Time)>PING_TIME Then
    Begin
      CreateShortMessage(Rm,nmPing);
      SendMessage(@Rm, I); // Pings the Client, to see if its alive
    End;
  End;

  Self.SendDroppedPackets();
End;

Function NetServer.IsPacketImportant(Msg:PNetMessage):Boolean;
Begin
  Result := Msg.Opcode>=10;
End;

Function NetServer.GetClientAddress(ID:Word):Cardinal;
Var
  Info:ClientInfo;
Begin
  Info := Self.GetClient(ID);
  If Info = Nil Then
  Begin
    Result := 0;
    Exit;
  End;

  Result := Info.Socket.Address;
End;

Function NetServer.GetClientByUsername(Name:AnsiString):ClientInfo;
Var
  I:Integer;
Begin
  Result := Nil;
  Name := UpStr(Name);
  _Mutex.Lock();
  For I:=1 To _ClientCount Do
  If (_ClientList[I]<>Nil) And (UpStr(_ClientList[I].UserName)=Name) Then
  Begin
    Result := _ClientList[I];
    Break;
  End;
  _Mutex.Unlock();
End;

Function NetServer.GetClient(ID:Word):ClientInfo;
Begin
  _Mutex.Lock();
  If (ID>0) And (ID<=_ClientCount) Then
  Begin
    If (_ClientList[ID]=Nil) Then
      Result := Nil
    Else
    Begin
      Result := _ClientList[ID];
    End;
  End Else
    Result := Nil;
  _Mutex.Unlock();
End;

Procedure NetServer.OnClientAdd(Client:ClientInfo);
Begin
End;

Procedure NetServer.OnClientRemove(Client:ClientInfo);
Begin
End;

Procedure NetServer.RemoveClient(Client:ClientInfo);
Var
  I:Integer;
  Done:boolean;
Begin
  If (Client = Nil) Then
    Exit;

  If (Not (Self Is NetMultithreadedServer)) Then
  Begin
    Self.OnClientRemove(Client);
  End;

  _Mutex.Lock();
  Done := (_ClientList[Client.ID] = Nil);
  _ClientList[Client.ID] := Nil;
  _Mutex.Unlock();

  If (Done) Then
    Exit;

  {$IFDEF SAVEPACKETS}
  If (Not (Self Is NetworkReplayServer)) Then
    NetworkLogger.Instance.LogConnectionEnd(Client.ID);
  {$ENDIF};

  Client.GUID := 0;
  Client.Address.Address := 0;
  If (Assigned(Client.Socket)) Then
  Begin
    _PacketMutex.Lock();
    For I:=0 To Pred(_PacketCount) Do
    If (_Packets[I].Sock = Client.Socket) Then
      _Packets[I].Sock := Nil;
    _PacketMutex.Unlock();

    If (Not (Self Is NetworkReplayServer)) Then
      Client.Socket.Destroy;
    Client.Socket := Nil;
  End;

  Client.Destroy;
End;

Function NetServer.ValidateClient(UserName,Password, DeviceID:AnsiString; Var ErrorLog:AnsiString):Integer;
Begin
  Log(logDebug,'Network',_Name+'.Validate: User='+Username+' Pass='+Password+' DeviceID='+DeviceID);
  Result := 0;
End;

// Send a message
Procedure NetServer.SendMessage(Msg:PNetMessage; ClientID:Word);
Var
  Client:ClientInfo;
Begin
  {$IFDEF NETDEBUG}WriteLn('Fetching client'); {$ENDIF}
  Client := GetClient(ClientID);

  {$IFDEF NETDEBUG}If Assigned(Client) Then WriteLn('Client: ',Client.ID) Else WriteLn('Client not found'); {$ENDIF}

  If Assigned(Client) Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('Sending packet'); {$ENDIF}
    SendPacket(Client.Address, Client.Socket, Msg);
    {$IFDEF NETDEBUG}WriteLn('Packet sent'); {$ENDIF}
  End Else
    Log(logWarning,'Network',_Name+'.SendMessage: Invalid client.['+IntToString(ClientID)+']');
End;

Procedure NetServer.SendMessage(ClientID:Word; Opcode:Byte; Src:Stream; Owner:Word = 0);
Var
  Msg:LNetMessage;
Begin
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Opcode := Opcode;
  Msg.Owner := Owner;
  If (Src<>Nil) Then
  Begin
    Msg.Size := Stream(Src).Position;
    Stream(Src).Seek(0);
    Stream(Src).Read(@Msg.Data[0], Msg.Size);
  End;

  SendMessage(@Msg, ClientID);
End;

// Broadcast a message
Procedure NetServer.BroadcastMessage(Msg:PNetMessage);
Var
  I:Integer;
Begin
  For I:=1 To _ClientCount Do
  If (I<>Msg.Owner) And (Assigned(GetClient(I))) Then
    SendMessage(Msg,I);
End;

// Removes a Client from the server
Procedure NetServer.Kick(ClientID:Word);
Var
  Msg:LNetMessage;
  Client:ClientInfo;
Begin
  Client := GetClient(ClientID);
  If Assigned(Client) Then
  Begin
    CreateMessage(Msg,nmServerShutdown,errKicked);
    SendMessage(@Msg, ClientID);

    RemoveClient(Client);
 End;
End;

// Shutdown a server and destroy it
Destructor NetServer.Destroy(ErrorCode:Word=errServerShutdown);
Var
  Client:ClientInfo;
  I:Integer;
  Msg:LNetMessage;
Begin
  CreateMessage(Msg,nmServerShutdown,ErrorCode); //Zero means normal shutdown
  BroadcastMessage(@Msg); //Notify Clients of the server shutdown

  For I:=1 To _ClientCount Do  //Search for duplicated GUIDs
  Begin
    Client := GetClient(I);
    If (Assigned(Client)) And (Client.Socket <>Nil) Then
    Begin
      Client.Socket.Destroy;
      Client.Socket := Nil;
    End;
  End;

  Inherited Destroy;

  _Mutex.Destroy;
End;

Function NetServer.CreateIterator:Iterator;
Begin
  Result := NetServerIterator.Create;
  NetServerIterator(Result).Pos:=0;
  NetServerIterator(Result).Count:=0;
  NetServerIterator(Result).Server:=Self;
End;

// LNetServerIterator

Function NetServerIterator.GetNext:ListObject;
Begin
  Inc(Pos);
  Result := Server.GetClient(Pos);
  If Result=Nil Then
    Result := GetNext
  Else
    Inc(Count);
End;

Function NetServerIterator.HasNext:Boolean;
Begin
  Result := (Count< NetServer(Server)._ClientCount);
End;

Procedure NetServerIterator.Discard(Release:Boolean=True);
Begin
  RaiseError('Network.'+Server._Name+'.Iterator.Discard: Cannot discard.');
End;

Function NetServer.GetConnectedClients: Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=1 To _ClientCount Do
  If Assigned(GetClient(I)) Then
    Inc(Result);
End;

Function NetServer.OnSendFail(Dest: SocketAddress; Sock: Socket; Msg: PNetMessage):Boolean;
Begin
  Result := False;
  If (Not _PacketKeep) Or (Not _PacketImportant[Msg.Opcode]) Then
    Exit;

  _PacketMutex.Lock();
  Inc(_PacketCount);
  SetLength(_Packets, _PacketCount);
  Move(Msg^, _Packets[Pred(_PacketCount)].Msg, SizeOf(LNetMessage));
  _Packets[Pred(_PacketCount)].Dest := Dest;
  _Packets[Pred(_PacketCount)].Sock := Sock;
  _Packets[Pred(_PacketCount)].Time := GetTime();
  _PacketMutex.Unlock();
End;

Procedure NetServer.SendDroppedPackets;
Var
  I, J:Integer;
Begin
  If (GetTime()-_LastPacketUpdate<1000) Then
    Exit;

  _LastPacketUpdate := GetTime();
  _PacketMutex.Lock();
  I := 0;
  _PacketKeep := False;
  While (I<_PacketCount) Do
  Begin
    {$IFDEF NETDEBUG}WriteLn('Sending delayed packet: opcode ',_Packets[I].Msg.Opcode);{$ENDIF}
    If (_Packets[I].Sock=Nil) Or (GetTime() - _Packets[I].Time>1000*60) Or (Self.SendPacket(_Packets[I].Dest, _Packets[I].Sock, @_Packets[I].Msg)) Then
    Begin
      For J:=0 To _PacketCount-2 Do
        _Packets[I] := _Packets[I+1];
      Dec(_PacketCount);
    End Else
      Inc(I);
  End;

  _PacketKeep := True;
  _PacketMutex.Unlock();
End;

Procedure NetServer.OnPacketReceived(Sock:Socket; Msg: PNetMessage);
Var
  I, N:Integer;
Begin
  {$IFDEF SAVEPACKETS}
  If (Not (Self Is NetworkReplayServer)) Then
  Begin
    _Mutex.Lock();
    N := -1;
    For I:=1 To _ClientCount Do  //Search for a dead Client slot
    If (_ClientList[I]<>Nil) And (_ClientList[I].Socket = Sock) Then
    Begin
      N := I; //If this Client is dead, then we can reuse the slot
      Break;
    End;
    _Mutex.Unlock();

    If (N>=0) Then
      NetworkLogger.Instance.LogPacket(N, Msg);
  End;
  {$ENDIF};
End;

End.
