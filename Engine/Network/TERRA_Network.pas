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
 * TERRA_Network
 * Implements engine generic network objects
 ***********************************************************************************************************************
}
{-$DEFINE NETDEBUG}


Unit TERRA_Network;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_MemoryStream, TERRA_OS, TERRA_Sockets, TERRA_Application;

Const
//Message types
  nmIgnore          = 0;  // Dummy message
  nmUnused          = 1;  // Ping message
  nmUnused2         = 2;  // Pong message
  nmServerAck       = 3;  // Server ack,Ok
  nmServerError     = 4;  // Server error
  nmServerShutdown  = 5;  // Server shutdown
  nmClientJoin      = 6;  // Asking permission to enter a server
  nmClientDrop      = 7;  // Client drop

// Server error codes
  errNoError              = 0;    // No error
  errServerFull           = 1;    // Server is full, no more clients allowed
  errDuplicatedGUID       = 2;    // Login GUID duplicated
  errInvalidUsername      = 3;    // Username not valid
  errInvalidServerCommand = 4;    // Opcode invalid
  errUsernameAlreadyInUse = 5;    // Username is already used
  errUsernameBanned       = 6;    // Username banned/not allowed
  errInvalidVersion       = 7;    // Wrong client version
  errInvalidPassword      = 8;    // Password not valid
  errAlreadyConnected     = 9;    // account already connected

  errConnectionFailed     = 31;   // Connection failed
  errConnectionLost       = 32;   // Connection lost
  errServerShutdown       = 33;   // Server was shutdown
  errServerCrash          = 34;   // Server crashed
  errKicked               = 35;   // Client was kicked
  errConnectionTimeOut    = 36;   // Connection failed

  MessageQueueSize=256;
  MaxUserLength=10;
  MaxPassLength=10;

  SERVER_CONNECT_DURATION = 2000;   //5000
  SERVER_RETRY_FREQ       = 750;    // Connection to server retry frequency
  ID_UNKNOWN              = 65535;  // Used for anonymous messages
  PING_TIME               = 1000;    // Ping time
  PONG_TIME               = 2000;   // Pong time
  DROP_TIME               = 50000;   // A Client is kicked when his ping reaches this value
  PING_REFRESH            = 750;    // Search for dead connections time frequency
  PONG_REFRESH            = 800;    // Search for dead server time frequency
  RMUPDATE_TIME           = 1750;   // Used for resending RM Messages
  RMDEFAULT_RESEND        = 2500;   // Each message is resent after this time
  RMMAX_RESENDS           = 5;      // Number of resends before a packet is deleted
  GIDEXPIRE_TIME          = 500;    // GID messages are removed after expire time

Type
  NetStatus = (nsDisconnected , nsConnected);

  PMessageHeader = ^MessageHeader;
  MessageHeader = Packed Record
    Opcode:Byte;
    Owner:Word;
    Length:Word;
  End;

  // NetMessage
  NetMessage = Class(MemoryStream)
    Protected
      Function GetOwner:Word;
      Procedure SetOwner(Value: Word);

      Function GetLength:Word;
      Procedure SetLength(Value: Word);

      Function GetOpcode: Byte;

      Procedure AdjustSize();

    Public
      Constructor Create(Opcode:Byte);

      Property Opcode:Byte Read GetOpcode;
      Property Owner:Word Read GetOwner Write SetOwner;
      Property Length:Word Read GetLength Write SetLength;
  End;

  MessageHandler = Procedure(Msg:NetMessage; Sock:Socket) Of Object;

  NetObject = Class(TERRAObject)
    Protected
      _Input:Cardinal;        //Current input in bytes
      _Output:Cardinal;       //Current output in bytes

      //Function CreateSocket:Cardinal;
      Function SendPacket(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean;

      Function OnSendFail(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean; Virtual;
      Procedure OnPacketReceived(Sock:Socket; Msg:NetMessage); Virtual;

    Public
      _Address:SocketAddress;  // Local Address
      _Sender:SocketAddress;   // Address of message sender
      _HostName:TERRAString;         // Local host name
      _LocalID:Word;            // Local ID
      _Port:Word;               // Local port
      _Version:Word;            // Object Version
      _TotalDownload:Cardinal;  // Session download total
      _TotalUpload:Cardinal;    // Session upload total
      _NextUpdate:Cardinal;     // Next IO check time
      _Upload:Cardinal;         // Current upload kb/s
      _Download:Cardinal;       // Current download kb/s
      _OpcodeList:Array[0..255]Of MessageHandler;

      Constructor Create();  //Creates a new object instance
      Procedure Release;Reintroduce;Virtual; //Shutdown the object

      //Function MakeSocketAddress(Var SockAddr:SocketAddress; Port:Word; Hostname:TERRAString):Boolean;

      Function ReceivePacket(Sock:Socket):Boolean;
      Procedure UpdateIO; //Updates upload/download status
      Procedure Update;Virtual;Abstract; //Handles standart messages

      Procedure AddHandler(Opcode:Byte; Handler:MessageHandler); Virtual;

      // Validates a message
      Function ValidateMessage(Msg:NetMessage):Boolean; Virtual; Abstract;

      Procedure ReturnMessage(Sock:Socket; Msg:NetMessage; AutoRelease:Boolean = False); // Sends a message to the last sender

      // Encodes a message
      Function EncodeMessage(Msg:NetMessage):NetMessage; Virtual;

      // Decodes a message
      Function DecodeMessage(Msg:NetMessage):NetMessage; Virtual;

      // Message Handlers -> Need one of this for each message type
      Procedure OnInvalidMessage(Msg:NetMessage; Sock:Socket);
      Procedure IgnoreMessage(Msg:NetMessage; Sock:Socket);
  End;


  NetworkManager = Class(ApplicationComponent)
    Protected
      _Objects:Array Of NetObject;
      _ObjectCount:Integer;

    Public
      Procedure Update; Override;

      Procedure AddObject(Obj:NetObject);
      Procedure RemoveObject(Obj:NetObject);

      Procedure Release; Override;
      Class Function Instance:NetworkManager;
  End;


Function GetMsgDesc(MsgId:Byte):TERRAString;
Function GetNetErrorDesc(ErrorCode:Word):TERRAString;

//Function CreateMessageWithWord(Opcode:Byte; Code:Word):NetMessage;  // Creates a server message

Implementation
Uses TERRA_Error, TERRA_Log;

Var
  _NetworkManager:ApplicationObject;

Function GetNetErrorDesc(ErrorCode:Word):TERRAString;
Begin
  Case ErrorCode Of
    errNoError:             Result:='No error.';
    errServerFull:          Result:='Server full.';
    errDuplicatedGUID:      Result:='Duplicated GUID.';
    errInvalidUsername:     Result:='Invalid username.';
    errInvalidPassword:     Result:='Invalid password.';
    errAlreadyConnected:    Result:='Account already connected.';
    errInvalidServerCommand:Result:='Invalid server command.';
    errUserNameAlreadyInUse:Result:='Username already in use.';
    errUserNameBanned:      Result:='Username is banned.';
    errInvalidVersion:      Result:='Invalid version.';
    errConnectionFailed:    Result:='Unable to connect to server.';
    errConnectionLost:      Result:='Connection was lost.';
    errServerShutdown:      Result:='Server shutdown.';
    errServerCrash:         Result:='Server crash.';
    errKicked:              Result:='Kicked!';
    Else                    Result:='Unknown server error.['+IntToString(ErrorCode)+']';
  End;
End;

Function _GetOpcode(MsgId:Byte):TERRAString;
Begin
  Result:='Unknown message type['+IntToString(MsgId)+']';
End;

Function GetMsgDesc(MsgId:Byte):TERRAString;
Begin
  Case MsgId Of
    nmServerAck:      Result:='Server.Ack';
    nmServerError:    Result:='Server.Error';
    nmServerShutdown: Result:='Server.Shutdown';
    nmClientJoin:     Result:='Client.Join';
    nmClientDrop:     Result:='Client drop';
    nmIgnore:         Result:='Ignore';
  Else
    Result := 'opcode #'+IntToString(MsgID);
  End;
End;

Procedure LogMsg(Prefix:TERRAString;Msg:NetMessage;Postfix:TERRAString);
Var
  S:TERRAString;
Begin
  S:=Prefix+' "'+GetMsgDesc(Msg.Opcode)+'" Size='+IntToString(Msg.Length)+' ';
  S:=S+Postfix;
  Log(logDebug,'Network', S);
End;

{********************
   LNetObject Class
 ********************}
Procedure NetObject.AddHandler(Opcode: Byte; Handler: MessageHandler);
Begin
  Self._OpcodeList[Opcode] := Handler;
End;
        (*
Function NetObject.CreateSocket:Cardinal;
Begin
  Result := TERRA_Sockets._socket(PF_INET, SOCK_DGRAM, 0); //Create a network socket
  If Result = SOCKET_ERROR Then  //Check for errors
  Begin
    RaiseError('Network.CreateSocket: Unable to open a socket.');
    Result := SOCKET_ERROR;
    Exit;
  End;

  MakeNonBlocking(Result, False);
End;  *)
                          (*
Function NetObject.MakeSocketAddress(Var SockAddr:SocketAddress; Port:Word; Hostname:TERRAString):Boolean;
Var
  IP:TERRAString;
Begin
  Result := False;

  IP := LookupHostAddress(Hostname);  //Resolve the server address

  Log(logDebug, 'Network', 'Calling htons()');
  SockAddr.Family := PF_INET;     // Set the address format
  SockAddr.Port := htons(Port);   // Convert to network byte order (using htons) and set port
  // Specify the IP

  Log(logDebug, 'Network', 'Calling inet_addr()');
  SockAddr.Address := inet_addr(PAnsiChar(IP));
  FillChar(SockAddr.Zero[1],8,0); //Zero fill

  //Check for error in IP
  If SockAddr.Address=SOCKET_ERROR Then
  Begin
    RaiseError(Self.ClassName+'.MakeSocketAddress: Unable to resolve IP address from '+ Hostname +'.');
    Exit;
  End;

  Result := True;
  Log(logDebug, 'Network', 'Socket is ready');
End;   *)

Function NetObject.SendPacket(Dest:SocketAddress;  Sock:Socket; Msg:NetMessage):Boolean;
Var
  Len, Count:Integer;
  N:Integer;
  Timer:Cardinal;
  P:PByte;
Begin
  Result := False;
  If (Sock.Closed) Then
    Exit;

  Msg.AdjustSize();

  Msg := Self.EncodeMessage(Msg);
  If (Msg = Nil) Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Packet was dropped during encription');{$ENDIF}
    Exit;
  End;

  Msg.AdjustSize();


  {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Packet size: ',Msg.Length);{$ENDIF}
  //LogMsg(_Name+'.Send():',@Rm,' to '+GetIP(Dest.Address));

    //EncodeMessage(Msg);

  //Send packet
  {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Writing to socket');{$ENDIF}
  Len := Msg.Length + SizeOf(MessageHeader);
  Count := Len;
  P := Msg.Buffer;
  Timer := GetTime();
  Repeat
    N := Sock.Write(P, Count);

    If (N = SOCKET_ERROR) Then
    Begin
      If Not Self.OnSendFail(Dest, Sock, Msg) Then
      Begin
        Result := False;
        Exit;
      End;
    End;

    If (N>0) Then
    Begin
      Inc(P, N);
      Dec(Count, N);
    End;

    If (Count<=0) Then
    Begin
      Result := True;
      Exit;
    End;
  Until (GetTime() - Timer > 500);

  //Log(logWarning,'Network',_Name+'.SendPacket: Socket error');
End;

{Procedure NetMessageDispatcher(Msg:Pointer);
Begin
  If Msg = Nil Then
    Exit;

  _NetObject._OpcodeList[NetMessage(Msg).Opcode](NetMessage(Msg), Nil);
End;}

Function NetObject.ReceivePacket(Sock:Socket):Boolean;
Var
  P:PByte;
  N,Cnt, Rem:Longint;
  ValidMsg:Boolean;
  Header:MessageHeader;
  Msg:NetMessage;
Begin
  Result := False;
  If (Sock = Nil) Then
    Exit;

  //T := GetTime();

  {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Reading from socket');{$ENDIF}

  //Check for a message
  Cnt := 0;
  P := @Header;
  Repeat
    Rem := SizeOf(MessageHeader) - Cnt;
    N := Sock.Read(P, Rem);
    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Read result: ',N);{$ENDIF}

   //Check for errors
    If (N=SOCKET_ERROR) Or (N<=0) Then //There was no message waiting
    Begin
      Exit;
    End;

    Inc(Cnt, N);
    Inc(P, N);
    //Log(logDebug, 'Network', 'Received '+IntToString(N)+' bytes');
  Until (Cnt>=SizeOf(MessageHeader));

  Inc(_Input, SizeOf(MessageHeader) + Header.Length);

  Msg := NetMessage.Create(Header.Opcode);
  Msg.Resize(Header.Length + SizeOf(MessageHeader));
  Msg.Seek(0);
  Msg.Write(@Header, SizeOf(MessageHeader));

  If (Header.Length>0) Then
  Begin
    P := Msg._Buffer;
    Inc(P, SizeOf(MessageHeader));
    Cnt := 0;
    Repeat
      Rem := Header.Length - Cnt;
      N := Sock.Read(P, Rem);
      //If (N=SOCKET_ERROR) Or (N<=0) Then //There was no message waiting
      If (Sock.Closed) Then
      Begin
        Exit;
      End;

      Inc(Cnt, N);
      Inc(P, N);
    Until (Cnt >= Header.Length);
  End;

  Msg := Self.DecodeMessage(Msg);
  If Msg = Nil Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'A message was dropped when decoding...');{$ENDIF}
    Result := False;
    Exit;
  End;

  OnPacketReceived(Sock, Msg);

  {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Validating message');{$ENDIF}
  Msg.Seek(SizeOf(MessageHeader));
  ValidMsg := ValidateMessage(Msg);
  //Log(logDebug, 'Network', 'Rec: ',Msg.Opcode);

  If ValidMsg Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Invoking opcode ',Msg.Opcode);{$ENDIF}
    If Assigned(_OpcodeList[Header.Opcode]) Then
    Begin
      Msg.Seek(SizeOf(MessageHeader));
      _OpcodeList[Header.Opcode](Msg, Sock) // Call message handler
    End;
  End Else
  If (Header.Opcode<>nmServerAck) Then
    Log(logWarning,'Network',Self.ClassName+'.Update: Invalid opcode ['+IntToString(Header.Opcode)+']');

  ReleaseObject(Msg);

  Result := True;

  {$IFDEF DEBUG_NET}Log(logDebug, 'Network', 'Opcode ',Header.Opcode,' processed');{$ENDIF}
End;

Procedure NetObject.OnInvalidMessage(Msg:NetMessage; Sock:Socket);
Begin
  Log(logError, 'Network', 'InvalidMessage: Unknown opcode.['+IntToString(Msg.Opcode)+']');
End;

Procedure NetObject.IgnoreMessage(Msg:NetMessage; Sock:Socket);
Begin
  // do nothing
End;

Constructor NetObject.Create();
Var
 I:Integer;
Begin
  _Upload := 0;
  _Download := 0;
  _Input := 0;
  _Output := 0;
  _TotalUpload := 0;
  _TotalDownload := 0;
  For I:=0 To 255 Do
    _OpcodeList[I] := OnInvalidMessage;
  _OpcodeList[nmIgnore] := IgnoreMessage;
End;

Procedure NetObject.Release;
Begin
  NetworkManager.Instance.RemoveObject(Self);
End;

Procedure NetObject.UpdateIO;
Begin
  If GetTime>=_NextUpdate Then
  Begin
    _Download := _Input;
    _Upload := _Output;
    Inc(_TotalDownload, _Download);
    Inc(_TotalUpload, _Upload);
    _Input := 0;
    _Output := 0;
    _NextUpdate := GetTime+1000;
  End;
End;

Procedure NetObject.ReturnMessage(Sock:Socket; Msg:NetMessage; AutoRelease:Boolean); //Sends a message to the last sender
Begin
  If Msg = Nil Then
    Exit;

  SendPacket(_Sender, Sock, Msg);

  If AutoRelease  Then
    ReleaseObject(Msg);
End;


Procedure NetworkManager.Release;
Begin
  _NetworkManager := Nil;
End;

Class Function NetworkManager.Instance:NetworkManager;
Begin
  If Not Assigned(_NetworkManager) Then
    _NetworkManager := InitializeApplicationComponent(NetworkManager, Nil);

  Result := NetworkManager(_NetworkManager.Instance);
End;

Procedure NetworkManager.AddObject(Obj: NetObject);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
  If (_Objects[I]= Obj) Then
    Exit;

  Inc(_ObjectCount);
  SetLength(_Objects, _ObjectCount);
  _Objects[Pred(_ObjectCount)] := Obj;
End;

Procedure NetworkManager.RemoveObject(Obj: NetObject);
Var
  I:Integer;
Begin
  I := 0;
  While (I<_ObjectCount) Do
  If (_Objects[I] = Obj) Then
  Begin
    _Objects[I] := _Objects[Pred(_ObjectCount)];
    Dec(_ObjectCount);
    Exit;
  End Else
    Inc(I);
End;

Procedure NetworkManager.Update;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
    _Objects[I].Update();
End;

Function NetObject.OnSendFail(Dest: SocketAddress; Sock: Socket; Msg: NetMessage): Boolean;
Begin
  Sleep(200);
  Result := True;
End;

Procedure NetObject.OnPacketReceived(Sock:Socket; Msg: NetMessage);
Begin
  // do nothing
End;


Function NetObject.DecodeMessage(Msg: NetMessage): NetMessage;
Begin
  Result := Msg;
End;

Function NetObject.EncodeMessage(Msg: NetMessage): NetMessage;
Begin
  Result := Msg;
End;

{ NetMessage }
Procedure NetMessage.AdjustSize;
Begin
  If Length<=0 Then
    Length := Self.Position - SizeOf(MessageHeader);
End;

Constructor NetMessage.Create(Opcode: Byte);
Var
  Header:MessageHeader;
Begin
  Inherited Create(256);

  Header.Opcode := Opcode;
  Header.Owner := 0;
  Header.Length := 0;

  Self.Write(@Header, SizeOf(MessageHeader));
End;

Function NetMessage.GetLength: Word;
Var
  Header:PMessageHeader;
Begin
  Header := PMessageHeader(_Buffer);
  Result := Header.Length;
End;

Function NetMessage.GetOpcode:Byte;
Var
  Header:PMessageHeader;
Begin
  Header := PMessageHeader(_Buffer);
  Result := Header.Opcode;
End;

Function NetMessage.GetOwner: Word;
Var
  Header:PMessageHeader;
Begin
  Header := PMessageHeader(_Buffer);
  Result := Header.Owner;
End;

Procedure NetMessage.SetLength(Value: Word);
Var
  Header:PMessageHeader;
Begin
  Header := PMessageHeader(_Buffer);
  Header.Length := Value;
End;

Procedure NetMessage.SetOwner(Value: Word);
Var
  Header:PMessageHeader;
Begin
  Header := PMessageHeader(_Buffer);
  Header.Owner := Value;
End;

End.