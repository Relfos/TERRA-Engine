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
 * TERRA_NetServer
 * Implements a generic multiplayer game server
 ***********************************************************************************************************************
}
Unit TERRA_NetServer;

{$I terra.inc}

{-$DEFINE SAVEPACKETS}


Interface
Uses TERRA_String, TERRA_Application, TERRA_OS, TERRA_Stream, TERRA_Sockets, TERRA_Network,
  TERRA_Threads, TERRA_Mutex, TERRA_Utils, TERRA_Collections
  {$IFDEF SAVEPACKETS},TERRA_NetLogger{$ENDIF};

Type
  NetPacket = Record
    Msg:NetMessage;
    Dest:SocketAddress;
    Sock:Socket;
    Time:Cardinal;
  End;

  // Client Info
  ClientInfo = Class(ListObject)
    Public
      ID:Word;
      UserName:TERRAString;
      Password:TERRAString;
      DeviceID:TERRAString;
      Version:Word;
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
      _LastPacketUpdate:Cardinal;

      Procedure RemoveClient(Client:ClientInfo); //Event: When a Client leaves the server
      Function OnSendFail(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean; Override;
      Procedure OnPacketReceived(Sock:Socket; Msg: NetMessage); Override;
      //Function IsPacketImportant(Msg:NetMessage):Boolean; Virtual;

      Procedure SendDroppedPackets;

    Public
      // Creates a new server instance
      Constructor Create(Version,Port:Word; MaxClients:Word);

      // Handles messages
      Procedure Update; Override;

      // Send a message
      Function SendMessage(Msg:NetMessage; ClientID:Word; AutoRelease:Boolean = False):Boolean; Virtual;

      // Broadcast a message
      Procedure BroadcastMessage(Msg:NetMessage; AutoRelease:Boolean = False);

      // Removes a Client from the server
      Procedure Kick(ClientID:Word);

      // Validates client username/password
      Function ValidateClient(UserName, Password, DeviceID:TERRAString; Var ErrorLog:TERRAString):Integer; Virtual;

      Function ValidateMessage(Msg:NetMessage):Boolean; Override;

      // Shutdown a server and destroy it
      Procedure Release(ErrorCode:Word=errServerShutdown); Reintroduce;

      // Message handlers
      Procedure OnPingMessage(Msg:NetMessage; Sock:Socket);
      Procedure OnJoinMessage(Msg:NetMessage; Sock:Socket);
      Procedure OnDropMessage(Msg:NetMessage; Sock:Socket);

      // Creates a error message with an optional string
      Function CreateErrorMessage(ErrorCode:Word; Const Msg:TERRAString = ''):NetMessage; Virtual;

      // Create server login ack message
      Function CreateLoginMessage(Client:ClientInfo):NetMessage; Virtual;

      // Create server shutdown/logout message
      Function CreateShutdownMessage(ErrorCode:Word):NetMessage; Virtual;

      // Gets local Client
      Function GetClient(ID:Word):ClientInfo;
      Function GetClientByUsername(Const Name:TERRAString):ClientInfo;
      Function GetClientAddress(ID:Word):Cardinal;
      Function CreateIterator:Iterator;

      Function GetConnectedClients():Integer;

      // Network Events
      Procedure OnClientAdd(Client:ClientInfo);Virtual;    //Event: When a new Client enter the server
      Procedure OnClientRemove(Client:ClientInfo);Virtual; //Event: When a Client leaves the server

      Property ClientCount:Integer Read _ClientCount;
    End;

  NetServerIterator = Class(Iterator)
    Protected
      _Server:NetServer;
      _Position:Integer;
      _Count:Integer;

    Public
      Constructor Create(Server:NetServer);
      Function HasNext:Boolean; Override;
      Function GetNext:ListObject; Override;
      Function Discard():Boolean; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_MultiThreadedServer, TERRA_NetReplayServer;


{********************
  LNetServer Class
 ********************}
Procedure NetServer.OnPingMessage(Msg:NetMessage; Sock:Socket);
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

Procedure NetServer.OnJoinMessage(Msg:NetMessage; Sock:Socket);
Var
  N,I,K:Integer;
  Guid, Version:Word;
  UserName,Password, DeviceID:TERRAString;
  Client:ClientInfo;
  Rm:NetMessage;
  ErrorLog:TERRAString;
Begin
  Msg.Read(@GUID, 2);
  Msg.Read(@Version, 2);
  Msg.ReadString(Username);
  Msg.ReadString(Password);
  Msg.ReadString(DeviceID);

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
    ReturnMessage(Sock, CreateErrorMessage(errInvalidVersion), True);
    Exit;
  End;

  ErrorLog := '';
  K := ValidateClient(UserName,Password, DeviceID, ErrorLog);
  If K<>0 Then
  Begin
    ReturnMessage(Sock, CreateErrorMessage(K, ErrorLog), True);
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
    ReturnMessage(Sock, CreateErrorMessage(errServerFull), True);
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
  Client.Version := Version;
  _ClientList[N] := Client;
  _Mutex.Unlock();
  {$IFDEF SAVEPACKETS}
  If (Not (Self Is NetworkReplayServer)) Then
    NetworkLogger.Instance.LogConnectionStart(N, UserName, Password, DeviceID);
  {$ENDIF};

  ReturnMessage(Sock, CreateLoginMessage(Client), True);

  If (Not (Self Is NetMultithreadedServer)) Then
    OnClientAdd(Client); //Calls the AddClient event
End;

Procedure NetServer.OnDropMessage(Msg:NetMessage; Sock:Socket);
Var
  Client:ClientInfo;
Begin
  Client := GetClient(Msg.Owner);
  If Assigned(Client) Then
    RemoveClient(Client) //Calls the remove Client event
  Else
    Log(logError, 'NetServer', 'DropMessage: Invalid Client. ['+IntToString(Msg.Owner)+']');
End;

//Creates a new server instance
Constructor NetServer.Create(Version, Port:Word; MaxClients:Word);
Var
  I:Integer;
Begin
  Inherited Create();

  _LocalId := 0;  //Servers always have a localID of zero
  NetworkManager.Instance.AddObject(Self);
  
  _Port := Port;
  _WaitingCount := 0;
  _ClientCount := MaxClients;
  _RefreshTime := 0;
  _Version := Version;
  _OpcodeList[nmClientJoin] := OnJoinMessage;
  _OpcodeList[nmClientDrop] := OnDropMessage;

  _Mutex := CriticalSection.Create('');
  _PacketMutex := CriticalSection.Create('');

  SetLength(_ClientList, Succ(_ClientCount));
  _ClientList[0] := ClientInfo.Create;
  With _ClientList[0] Do
  Begin
    Ping := 0;
    GUID := 0;
  End;

  For I:=1 To _ClientCount Do
    _ClientList[I] := Nil;
End;

// Validates a message
Function NetServer.ValidateMessage(Msg:NetMessage):Boolean;
Var
  Client:ClientInfo;
Begin
  {$IFDEF DEBUG_NET}WriteLn('Begin validation');{$ENDIF}
  Result := (Msg.Owner=ID_UNKNOWN) And (Msg.Opcode=nmClientJoin);

  If Not Result Then
  Begin
  {$IFDEF DEBUG_NET}WriteLn('Calling getClient()');{$ENDIF}
    Client := GetClient(Msg.Owner);
    Result := (Assigned(Client)){ And (Client.Address.Address=_Sender.Address)};

    {$IFDEF DEBUG_NET}WriteLn('Testing client');{$ENDIF}
    If Assigned(Client) Then
      Client.Time := GetTime
    Else
      Log(logWarning,'Network',Self.ClassName+'.Update: Invalid server message. ['+IntToString(Msg.Owner)+']');
  End;
  {$IFDEF DEBUG_NET}WriteLn('End validation');{$ENDIF}
End;

// Handles messages
Procedure NetServer.Update;
Var
  I,J:Integer;
  Msg:NetMessage;
  Rm:NetMessage;
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

    {$IFDEF DEBUG_NET}WriteLn('Processing client: ',Client.Username);{$ENDIF}
    J := 0;
    Repeat
      If ReceivePacket(Client.Socket) Then
      Begin
        Client.Time := GetTime();
        Inc(Client.Frames);
      End Else
        Break;
    Until (J>=5);
    {$IFDEF DEBUG_NET}WriteLn('Packets received: ',J);{$ENDIF}
  End;

  I := 0;
  While (I<_WaitingCount) Do
  Begin
    {$IFDEF DEBUG_NET}WriteLn('Processing waiting connection #',I);{$ENDIF}
    If (ReceivePacket(_WaitingConnections[I])) Then
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

    If (Client.Socket.Closed) Then
    Begin
      Log(logWarning,'Network', Self.ClassName+'.Update: ClientID='+IntToString(I)+' is dead.');
      RemoveClient(Client);       // Call RemoveClient event
    End;
  End;

  Self.SendDroppedPackets();
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

Function NetServer.GetClientByUsername(Const Name:TERRAString):ClientInfo;
Var
  I:Integer;
Begin
  Result := Nil;
  _Mutex.Lock();
  For I:=1 To _ClientCount Do
  If (_ClientList[I]<>Nil) And (StringEquals(_ClientList[I].UserName, Name)) Then
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
      Client.Socket.Release;
    Client.Socket := Nil;
  End;

  Client.Release;
End;

Function NetServer.ValidateClient(UserName,Password, DeviceID:TERRAString; Var ErrorLog:TERRAString):Integer;
Begin
  Log(logDebug,'Network', Self.ClassName+'.Validate: User='+Username+' Pass='+Password+' DeviceID='+DeviceID);
  Result := 0;
End;

// Send a message
Function NetServer.SendMessage(Msg:NetMessage; ClientID:Word; AutoRelease:Boolean):Boolean;
Var
  Client:ClientInfo;
Begin
  If Msg = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  {$IFDEF DEBUG_NET}WriteLn('Fetching client'); {$ENDIF}
  Client := GetClient(ClientID);

  {$IFDEF DEBUG_NET}If Assigned(Client) Then WriteLn('Client: ',Client.ID) Else WriteLn('Client not found'); {$ENDIF}

  If Assigned(Client) Then
  Begin
    {$IFDEF DEBUG_NET}WriteLn('Sending packet'); {$ENDIF}
    Result := SendPacket(Client.Address, Client.Socket, Msg);
    {$IFDEF DEBUG_NET}WriteLn('Packet sent'); {$ENDIF}
  End Else
  Begin
    Log(logWarning,'Network', Self.ClassName+'.SendMessage: Invalid client.['+IntToString(ClientID)+']');
    Result := False;
  End;

  If AutoRelease Then
    ReleaseObject(Msg);
End;

// Broadcast a message
Procedure NetServer.BroadcastMessage(Msg:NetMessage; AutoRelease:Boolean);
Var
  I:Integer;
Begin
  If Msg = Nil Then
    Exit;
    
  For I:=1 To _ClientCount Do
  If (I<>Msg.Owner) And (Assigned(GetClient(I))) Then
    SendMessage(Msg, I);

  If AutoRelease Then
    ReleaseObject(Msg);
End;

// Removes a Client from the server
Procedure NetServer.Kick(ClientID:Word);
Var
  Msg:NetMessage;
  Client:ClientInfo;
Begin
  Client := GetClient(ClientID);
  If Assigned(Client) Then
  Begin
    SendMessage(CreateShutdownMessage(errKicked), ClientID, True);
    RemoveClient(Client);
 End;
End;

// Shutdown a server and destroy it
Procedure NetServer.Release(ErrorCode:Word=errServerShutdown);
Var
  Client:ClientInfo;
  I:Integer;
  Msg:NetMessage;
Begin
  Msg := CreateShutdownMessage(ErrorCode); //Zero means normal shutdown
  BroadcastMessage(Msg); //Notify Clients of the server shutdown
  ReleaseObject(Msg);

  For I:=1 To _ClientCount Do  //Search for duplicated GUIDs
  Begin
    Client := GetClient(I);
    If (Assigned(Client)) And (Client.Socket <>Nil) Then
    Begin
      Client.Socket.Release;
      Client.Socket := Nil;
    End;
  End;

  Inherited Release();

  _Mutex.Release;
End;

Function NetServer.CreateIterator:Iterator;
Begin
  Result := NetServerIterator.Create(Self);
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

Function NetServer.OnSendFail(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean;
Begin
  Result := False;
  If (Not _PacketKeep) Then
    Exit;

  _PacketMutex.Lock();
  Inc(_PacketCount);
  SetLength(_Packets, _PacketCount);
  _Packets[Pred(_PacketCount)].Msg := Msg;
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
    {$IFDEF DEBUG_NET}WriteLn('Sending delayed packet: opcode ',_Packets[I].Msg.Opcode);{$ENDIF}
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

Procedure NetServer.OnPacketReceived(Sock:Socket; Msg:NetMessage);
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

Function NetServer.CreateErrorMessage(ErrorCode:Word; Const Msg:TERRAString):NetMessage;
Begin
  Result := NetMessage.Create(nmServerError);
  Result.Write(@ErrorCode, 2);
  Result.WriteString(Msg);
End;

Function NetServer.CreateLoginMessage(Client:ClientInfo): NetMessage;
Begin
  Result := NetMessage.Create(nmServerAck);
  Result.WriteWord(Client.ID)
End;

Function NetServer.CreateShutdownMessage(ErrorCode:Word):NetMessage;
Begin
  Result := NetMessage.Create(nmServerShutdown);
  Result.WriteWord(ErrorCode);
End;

{ NetServerIterator }

Constructor NetServerIterator.Create(Server: NetServer);
Begin
  Self._Server := Server;
  Self._Position := 0;
  Self._Count := 0;
End;

Function NetServerIterator.GetNext:ListObject;
Begin
  Inc(_Position);
  Result := _Server.GetClient(_Position);
  If Result = Nil Then
    Result := GetNext()
  Else
    Inc(_Count);
End;

Function NetServerIterator.HasNext:Boolean;
Begin
  Result := (_Count< _Server._ClientCount);
End;

Function NetServerIterator.Discard():Boolean;
Begin
  _Server.Kick(_Position);
  Result := True;
End;

End.
