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

{-$DEFINE ALLOW_RESEND_ONFAIL}

Interface
Uses TERRA_String, TERRA_Application, TERRA_OS, TERRA_Stream, TERRA_Sockets, TERRA_Network,
  TERRA_Threads, TERRA_Mutex, TERRA_Utils, TERRA_ObjectArray, TERRA_Collections
  {$IFDEF SAVEPACKETS},TERRA_NetLogger{$ENDIF};

Type
  NetPacket = Record
    Msg:NetMessage;
    Dest:SocketAddress;
    Sock:NetSocket;
    Time:Cardinal;
  End;

  NetServer = Class;

  // Client Info
  ClientConnection = Class(CollectionObject)
    Protected
      _Socket:NetSocket;
      _Server:NetServer;
      _ID:Cardinal;
      _Version:Word;
      _Time:Cardinal;    //Last contact time
      _Frames:Integer;

      _Deleted:Boolean;
      _DeleteTime:Cardinal;

    Public
      UserName:TERRAString;
      Password:TERRAString;
      DeviceID:TERRAString;
      GUID:Word;
      Ping:Integer;     //If ping<0 then Client connection is dead

      Address:SocketAddress;

      Procedure Release(); Override; //      ReleaseObject(_Socket);

      Procedure Kick(); Virtual; // kick player from server

      Procedure Update();

      Property ID:Cardinal Read _ID;
      Property Socket:NetSocket Read _Socket;
      Property Server:NetServer Read _Server;
      Property Version:Word Read _Version;
      Property Time:Cardinal Read _Time;
      Property Deleted:Boolean Read _Deleted;
  End;

  NetServerMessageHandler = Procedure(Msg:NetMessage; Client:ClientConnection) Of Object;

  NetServer = Class(NetObject)
    Protected
      {_Clients:Array Of ClientConnection; // Clients list
      _ClientCount:Integer;}
      _Clients:ObjectArray;

      _MinVersion:Word;

      _RefreshTime:Cardinal;            // Search for dead connections time
      _WaitingConnections:Array Of NetSocket;
      _WaitingCount:Integer;

      {$IFDEF ALLOW_RESEND_ONFAIL}
      _Packets:Array Of NetPacket;
      _PacketCount:Integer;
      _PacketMutex:CriticalSection;
      _PacketKeep:Boolean;
      _LastPacketUpdate:Cardinal;
      {$ENDIF}

      _TotalConnections:Cardinal;

      _OpcodeList:Array[0..255]Of NetServerMessageHandler;

      Procedure RemoveClient(Client:ClientConnection); //Event: When a Client leaves the server
      Function OnSendFail(Dest:SocketAddress; Sock:NetSocket; Msg:NetMessage):Boolean; Override;
      Procedure OnPacketReceived(Sock:NetSocket; Msg: NetMessage); Override;
      //Function IsPacketImportant(Msg:NetMessage):Boolean; Virtual;

{$IFDEF ALLOW_RESEND_ONFAIL}
      Procedure SendDroppedPackets;
{$ENDIF}

      Function CreateClientConnection():ClientConnection; Virtual;
      //Function CreateClientList():List; Virtual;

      Function AllocClientID(Client:ClientConnection):Cardinal; Virtual;

      Procedure ProcessJoinRequest(Msg:NetMessage; Sock:NetSocket); Virtual;

      Function ValidateMessage(Msg:NetMessage; Sock:NetSocket):ClientConnection; Virtual;

      Procedure InitClientConnection(Client:ClientConnection); Virtual;

      Procedure CheckForIncomingConnections();

    Public
      // Creates a new server instance
      Constructor Create(MinVersion, MaxVersion,Port:Word);

      Procedure AddHandler(Opcode:Byte; Handler:NetServerMessageHandler); Virtual;

      // Handles messages
      Procedure Update; Override;

      // Send a message
      Function SendMessage(Msg:NetMessage; Client:ClientConnection; Owner:Cardinal; AutoRelease:Boolean = False):Boolean; Virtual;

      // Broadcast a message
      Procedure BroadcastMessage(Msg:NetMessage;  Owner:Cardinal; AutoRelease:Boolean = False);

      // Validates client username/password
      Function ValidateClient(UserName, Password, DeviceID:TERRAString; Var ErrorLog:TERRAString):Integer; Virtual;

      // Shutdown a server and destroy it
      Procedure Release(); Override;

      // Message handlers
      Procedure OnPingMessage(Msg:NetMessage; Client:ClientConnection);
      Procedure OnDropMessage(Msg:NetMessage; Client:ClientConnection);

      // Creates a error message with an optional string
      Function CreateErrorMessage(ErrorCode:Word; Const Msg:TERRAString = ''):NetMessage; Virtual;

      // Create server login ack message
      Function CreateLoginMessage(Client:ClientConnection):NetMessage; Virtual;

      // Gets local Client
      Function GetClientByID(ID:Cardinal):ClientConnection;
      Function GetClientByUsername(Const Name:TERRAString):ClientConnection;

      // Network Events
      Procedure OnClientAdd(Client:ClientConnection);Virtual;    //Event: When a new Client enter the server
      Procedure OnClientRemove(Client:ClientConnection);Virtual; //Event: When a Client leaves the server

      Property Clients:ObjectArray Read _Clients;
      //Property ClientCount:Integer Read _ClientCount;
    End;

Implementation
Uses TERRA_Log;


{ NetServer }
//Creates a new server instance
Constructor NetServer.Create(MinVersion, MaxVersion, Port:Word);
Var
  I:Integer;
Begin
  Inherited Create();

  _LocalId := 0;  //Servers always have a localID of zero
  NetworkManager.Instance.AddObject(Self);

  _Port := Port;
  _WaitingCount := 0;
  _RefreshTime := 0;
  _MinVersion := MinVersion;
  _Version := MaxVersion;

  For I:=0 To 255 Do
    _OpcodeList[I] := Nil;

  _OpcodeList[nmClientDrop] := OnDropMessage;

{$IFDEF ALLOW_RESEND_ONFAIL}
  _PacketMutex := CriticalSection.Create();
{$ENDIF}

  //_ClientCount := 0;
  _Clients := ObjectArray.Create(coThreadSafe, Nil);
  //_Clients := Self.CreateClientList();
End;

Procedure NetServer.AddHandler(Opcode:Byte; Handler:NetServerMessageHandler);
Begin
  Self._OpcodeList[Opcode] := Handler;
End;

Function NetServer.AllocClientID(Client: ClientConnection): Cardinal;
Begin
  Inc(_TotalConnections);
  Result := _TotalConnections;
End;

Function NetServer.CreateClientConnection: ClientConnection;
Begin
  Result := ClientConnection.Create();
End;

{Function NetServer.CreateClientList():List;
Begin
  Result := List.Create(collection_Unsorted, coThreadSafe);
End;}

Procedure NetServer.OnPingMessage(Msg:NetMessage; Client:ClientConnection);
Begin
  Client.Ping := Client.Time;
  Client._Time := Application.GetTime;
  Client.Ping := Integer(Client.Time) - Client.Ping;
End;

Procedure NetServer.ProcessJoinRequest(Msg:NetMessage; Sock:NetSocket);
Var
  It:Iterator;
  K:Integer;
  Guid, Version:Word;
  UserName,Password, DeviceID:TERRAString;
  Rm:NetMessage;
  ErrorLog:TERRAString;
  Client:ClientConnection;
Begin
  Msg.ReadWord(GUID);
  Msg.ReadWord(Version);
  Msg.ReadString(Username);
  Msg.ReadString(Password);
  Msg.ReadString(DeviceID);

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

  If (Version<_MinVersion) Or (Version>_Version) Then
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

  {ReturnMessage(Sock, CreateErrorMessage(errServerFull), True);
  Exit;}

  Client := Self.CreateClientConnection();
  Client._Server := Self;
  Client._Time := Application.GetTime();
  Client.Address := _Sender; // Store the new Client IP
  Client.Ping := 0;          // Reset Client ping
  Client.GUID := GUID;
  Client.UserName := UserName;
  Client.Password := Password;
  Client.DeviceID := DeviceID;
  Client._Socket := Sock;
  Client._Frames := 0;
  Client._Version := Version;
  Client._ID := AllocClientID(Client);

  {$IFDEF SAVEPACKETS} //FIXME
  If (Not (Self Is NetworkReplayServer)) Then
    NetworkLogger.Instance.LogConnectionStart(N, UserName, Password, DeviceID);
  {$ENDIF};

  {Inc(_ClientCount);
  SetLength(_Clients, _ClientCount);
  _Clients[Pred(_ClientCount)] := Client;}

  Clients.Add(Client);
  
  InitClientConnection(Client);

  ReturnMessage(Sock, CreateLoginMessage(Client), True);
End;

Procedure NetServer.OnDropMessage(Msg:NetMessage; Client:ClientConnection);
Begin
  If Assigned(Client) Then
    RemoveClient(Client); //Calls the remove Client event
End;

// Validates a message
Function NetServer.ValidateMessage(Msg:NetMessage; Sock:NetSocket):ClientConnection;
Begin
  Result := Nil;
  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Begin validation');{$ENDIF}
  If (Msg.Owner=ID_UNKNOWN) Then
    Exit;

  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Calling getClient()');{$ENDIF}
  Result := GetClientByID(Msg.Owner);

{  If (Assigned(Result)) And (Result.Address.Sin_Addr <> Sock.Address) Then
  Begin
    Result := Nil;
    Log(logWarning,'Network',Self.ClassName+'.Update: Invalid server message. ['+IntToString(Msg.Owner)+']');
  End;}
  
  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'End validation');{$ENDIF}
End;

// Handles messages
Procedure NetServer.Update;
Var
  It:Iterator;
  I, J:Integer;
  Msg:NetMessage;
  Rm:NetMessage;
  ValidMsg:Boolean;
  Client:ClientConnection;
Begin
{$IFDEF ALLOW_RESEND_ONFAIL}
  _PacketKeep := True;
{$ENDIF}

  UpdateIO;

  CheckForIncomingConnections();

  {If (Self.GetConnectedClients()>0) Then
    Log(logDebug, 'Server', '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Starting update cycle ');}

  It := _Clients.GetIterator();
  While It.HasNext() Do
  Begin
    Client := ClientConnection(It.Value);

    If (Client.Deleted) Then
      Client.Discard()
    Else
      Client.Update();
  End;
  ReleaseObject(It);

{  I := 0;
  While I<_ClientCount Do
  If (_Clients[I].Deleted) Then
  Begin
    ReleaseObject(_Clients[I]);
    _Clients[I] := _Clients[Pred(_ClientCount)];
    Dec(_ClientCount);
  End Else
  Begin
    _Clients[I].Update();
    Inc(I);
  End;}


  I := 0;
  While (I<_WaitingCount) Do
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Processing waiting connection #'+IntToString(I));{$ENDIF}
    If (ReceivePacket(_WaitingConnections[I])) Or (_WaitingConnections[I].Closed) Then
    Begin
      _WaitingConnections[I] := _WaitingConnections[Pred(_WaitingCount)];
      Dec(_WaitingCount);
    End Else
      Inc(I);
  End;

{$IFDEF ALLOW_RESEND_ONFAIL}
  Self.SendDroppedPackets();
{$ENDIF}  
End;

Function NetServer.GetClientByUsername(Const Name:TERRAString):ClientConnection;
Var
  I:Integer;
  It:Iterator;
  Client:ClientConnection;
Begin
  Result := Nil;

  {For I:=0 To Pred(_ClientCount) Do
  If (StringEquals(_Clients[I].UserName, Name)) Then
  Begin
    Result := _Clients[I];
    Break;
  End;}

  It := _Clients.GetIterator();
  While It.HasNext() Do
  Begin
    Client := ClientConnection(It.Value);
    If (StringEquals(Client.UserName, Name)) Then
    Begin
      Result := Client;
      Break;
    End;
  End;
  ReleaseObject(It);
End;

Function NetServer.GetClientByID(ID:Cardinal):ClientConnection;
Var
  I:Integer;
  It:Iterator;
  Client:ClientConnection;
Begin
  Result := Nil;

  {$IFDEF USE_SIMPLE_ARRAYS}
  For I:=0 To Pred(_ClientCount) Do
  If (_Clients[I].ID = ID) Then
  Begin
    Result := _Clients[I];
    Break;
  End;
  {$ELSE}

  It := _Clients.GetIterator();
  While It.HasNext() Do
  Begin
    Client := ClientConnection(It.Value);
    If (Client.ID = ID) Then
    Begin
      Result := Client;
      Break;
    End;
  End;
  ReleaseObject(It);
  {$ENDIF}
End;

Procedure NetServer.OnClientAdd(Client:ClientConnection);
Begin
End;

Procedure NetServer.OnClientRemove(Client:ClientConnection);
Begin
End;

Procedure NetServer.RemoveClient(Client:ClientConnection);
Var
  I:Integer;
Begin
  If (Client = Nil) Or (Client._Deleted) Then
    Exit;

  Self.OnClientRemove(Client);

  {$IFDEF SAVEPACKETS}
  If (Not (Self Is NetworkReplayServer)) Then
    NetworkLogger.Instance.LogConnectionEnd(Client.ID);
  {$ENDIF};

  {$IFDEF ALLOW_RESEND_ONFAIL}
  If (Assigned(Client.Socket)) Then
  Begin
    _PacketMutex.Lock();
    For I:=0 To Pred(_PacketCount) Do
    If (_Packets[I].Sock = Client.Socket) Then
      _Packets[I].Sock := Nil;
    _PacketMutex.Unlock();
  End;
  {$ENDIF}

  Client._Deleted := True;
  Client._DeleteTime := Application.GetTime();
End;

Function NetServer.ValidateClient(UserName,Password, DeviceID:TERRAString; Var ErrorLog:TERRAString):Integer;
Begin
  Log(logDebug,'Network', Self.ClassName+'.Validate: User='+Username+' Pass='+Password+' DeviceID='+DeviceID);
  Result := 0;
End;

// Send a message
Function NetServer.SendMessage(Msg:NetMessage; Client:ClientConnection;  Owner:Cardinal; AutoRelease:Boolean):Boolean;
Begin
  If Msg = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  Msg.Owner := Owner;

  {$IFDEF DEBUG_NET}If Assigned(Client) Then Log(logDebug, 'Server', 'Client: '+CardinalToString(Client.ID)) Else Log(logDebug, 'Server', 'Client not found'); {$ENDIF}

  If (Assigned(Client)) And (Assigned(Client.Socket)) Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Sending packet'); {$ENDIF}
    Result := SendPacket(Client.Address, Client.Socket, Msg);
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Packet sent'); {$ENDIF}
  End Else
  Begin
    Log(logWarning,'Network', Self.ClassName+'.SendMessage: Invalid client.['+CardinalToString(Client.ID)+']');
    Result := False;
  End;

  If AutoRelease Then
    ReleaseObject(Msg);
End;

// Broadcast a message
Procedure NetServer.BroadcastMessage(Msg:NetMessage; Owner:Cardinal; AutoRelease:Boolean);
Var
  I:Integer;
  It:Iterator;
  Client:ClientConnection;
Begin
  If Msg = Nil Then
    Exit;

  {$IFDEF USE_SIMPLE_ARRAYS}
  For I:=0 To Pred(_ClientCount) Do
  If (_Clients[I].ID <> Owner) Then
  Begin
    SendMessage(Msg, _Clients[I], Owner);
  End;
  {$ELSE}

  It := _Clients.GetIterator();
  While It.HasNext() Do
  Begin
    Client := ClientConnection(It.Value);

    If (Client.ID <> Owner) Then
      SendMessage(Msg, Client, Owner);
  End;
  {$ENDIF}

  If AutoRelease Then
    ReleaseObject(Msg);
End;

// Shutdown a server and destroy it
Procedure NetServer.Release();
Var
  I:Integer;
  Client:ClientConnection;
  It:Iterator;
  Msg:NetMessage;
Begin
  Msg := NetMessage.Create(nmServerShutdown);
  Msg.WriteWord(errServerShutdown);
  BroadcastMessage(Msg, 0); //Notify Clients of the server shutdown
  ReleaseObject(Msg);

  {$IFDEF USE_SIMPLE_ARRAYS}
  For I:=0 To Pred(_ClientCount) Do
  Begin
    ReleaseObject(_Clients[I]);
  End;
  {$ELSE}

  ReleaseObject(_Clients);
  {$ENDIF}


  Inherited;
End;

Function NetServer.OnSendFail(Dest:SocketAddress; Sock:NetSocket; Msg:NetMessage):Boolean;
Begin
  Result := False;

{$IFDEF ALLOW_RESEND_ONFAIL}
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

{$ELSE}
{$ENDIF}  
End;

{$IFDEF ALLOW_RESEND_ONFAIL}
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
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Sending delayed packet: opcode ' + IntToString(_Packets[I].Msg.Opcode));{$ENDIF}
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
{$ENDIF}

Procedure NetServer.OnPacketReceived(Sock:NetSocket; Msg:NetMessage);
Var
  I, N:Integer;
  Client:ClientConnection;
Begin
  If Msg.Opcode = nmClientJoin Then
  Begin
    ProcessJoinRequest(Msg, Sock);
    Exit;
  End;

  If Assigned(_OpcodeList[Msg.Opcode]) Then
  Begin
    Client := Self.GetClientByID(Msg.Owner);

    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Found handler, executing message '+IntToString(Msg.Opcode));{$ENDIF}
    _OpcodeList[Msg.Opcode](Msg, Client);

    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Executed message '+IntToString(Msg.Opcode));{$ENDIF}
  End;

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

Procedure NetServer.InitClientConnection(Client: ClientConnection);
Begin
  Client.Socket.SetBlocking(False);
  OnClientAdd(Client); //Calls the AddClient event
End;

Function NetServer.CreateErrorMessage(ErrorCode:Word; Const Msg:TERRAString):NetMessage;
Begin
  Result := NetMessage.Create(nmServerError);
  Result.Write(@ErrorCode, 2);
  Result.WriteString(Msg);
End;

Function NetServer.CreateLoginMessage(Client:ClientConnection): NetMessage;
Begin
  Result := NetMessage.Create(nmServerAck);
  Result.WriteCardinal(Client.ID);
End;


Procedure NetServer.CheckForIncomingConnections();
Var
  Sock:NetSocket;
Begin
  Sock := OpenIncomingStream(_Port);
  If Assigned(Sock) Then
  Begin
    Sock.SetDelay(False);
    //Sock.SetBufferSize(1024*128);
    Inc(_WaitingCount);
    SetLength(_WaitingConnections, _WaitingCount);
    _WaitingConnections[Pred(_WaitingCount)] := Sock;
  End;
End;

{ ClientConnection }
Procedure ClientConnection.Kick;
Var
  Msg:NetMessage;
Begin
  Msg := NetMessage.Create(nmServerShutdown);
  Msg.WriteWord(errKicked);
  Server.SendMessage(Msg, Self, 0);
  ReleaseObject(Msg);
  Self.Discard();
End;

Procedure ClientConnection.Release;
Begin
  ReleaseObject(_Socket);
End;

Procedure ClientConnection.Update;
Begin
  If (Socket = Nil) Then
    Exit;

  If (_Deleted) Then
    Exit;

  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Processing client: '+Self.Username);{$ENDIF}

  If (Socket.Closed) Then
  Begin
    Log(logWarning,'Network', 'ClientID='+IntToString(Self._ID)+' is dead.');
    _Server.RemoveClient(Self);
    Exit;
  End;

  If _Server.ReceivePacket(Socket) Then
  Begin
    Self._Time := Application.GetTime();
    Inc(_Frames);
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Packets received from '+CardinalToString(_ID));{$ENDIF}
  End;
End;

End.
