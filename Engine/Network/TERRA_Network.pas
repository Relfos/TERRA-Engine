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
{-$DEFINE DEBUGMODE}
{-$DEFINE NETDEBUG}

{
@abstract(Network)
@author(Sergio Flores <relfos@gmail.com>)
@created(March 18, 2005)
@lastmod(September 29, 2005)
The Network unit provides a solid client/server system.
Uses UDP, with support for reliable messages.

Version History
   18/3/05  • Added type definitions and DLL functions import definitions
   24/3/05  • Bug resolved: External declarations need the StdCall directive
            • Implemented LNetServer and LNetClient classes
   25/3/05  • Implemented master class LNetObject
              • WinSock routines are now methods of this class
              • LNetServer and LNetClient are now descendants of this class
              • Contains a message queueing system
            • Finally Client-Server sample doesnt fail connecting to server
              • Bug: The same Client is duplicated when enter the server
                 • This is because of multiple Join packets send
            • Added AddClient and RemoveClient virtual methods
            • While watching the log files, some messages seem to be corrupted
              • Added EndOfMessage flag, to verify data integrity
            • Took me a whole day to remove all major bugs
              • Client-Server Network session now works stable
            • Added ConnectionStart and ConnectionEnd virtual methods
            • Bug: Only one client can connect to server
               • Other clients trying to connect will receive a duplicate GUID error
               • Fixed: Remove old debug code used to force duplicated GUIDs
    3/6/05  • Bug:Solved, Server side - Ping check wasnt working correctly.
                    • All Clients were dropped instantly
                    • The same Clients were droped constatly
                    • The droped client wasnt notified of the disconection
   26/6/05  • NetClient.Connect() changed, now use a username and password method.
            • NetServer.Validate is used to validate users.
                • Default implementation validates any user/password
            • NetClient.Connect bug fixed
                • JoinTimer wasnt initialized to zero
                • Lesson -> Always initialize variables, even with zero
   27/6/05  • NetClient.Validate() now returns a error code instead of a boolean
            • Now NetClient checks if the conection was lost and disconnects.
   28/6/05  • CreateServerMessage() renamed to CreateMessage()
            • Added CreateExtendedMessage()
   2/7/05   • Fixed some bugs
            • Added Network Output/Input bandwith usage status
            • Added login with version check
            • Implemented Pong messages
   3/7/05   • Implemented server behavior, allows switching between
              a small centered server, or a large scale aproach
                • The main diference is that when in large scale mode,
                  some broadcasts are avoided, like Client drops.
                  This reduces the server overload, but in order to
                  notify the other Clients, the server should
                  implement their own broadcasting system, with a
                  localized clustering selection.
                  The following event broadcasts are supressed
                  • ClientJoin: The AddClient event should handle this
                  • ClientLeave,
                    ClientDrop: The RemoveClient event should handle this
            • Fixed bug in server responding to Pong queries
            • Pong querie delay was too short, the overload was desneccessary
               • Now if the server doesnt respond in 5 seconds, a Pong message is sent
               • Then if the server doesnt respond in 15 seconds, the connection is shutdown
   4/7/05   • Implemented reliable message system
               • Just put a ERM instead of a EOM in the message tail
               • Then message will be resent until an ack is received
               • After 25 resends, the message will be ignored
            • Fixed reliable message system infinite loop
            • Download/Upload were switched
   6/7/05   • RM messages caused duplicated messages to be received
               • Added a GIDSliderWindow to check for duplicated messages
   7/7/05   • Implemented GIDFlush method
   8/7/05   • Implemented messages flags
               • The following flags are implemented
                 • msgEOM - EndOfMessage, for integrity check
                 • msgRM - Reliable Message
                 • msgRMO - Reliable Message, no reply
                 • msgNGID - Disable GID, Ignore duplicates
            • Fixed disconection bug
   12/7/05  • Added msgNGID flag to some server messages
            • Server no longer pings dead Clients
   17/7/05  • Remodeled and optimized opcode system
              • Now uses messages handlers
              • The new system also makes possible to replace default messages
            • Client now can resolve hostnames
              • Its possible now to connect to something like "xpow.gameserver.net"
            • Now reliable messages have individual resend timers
              • This reduces some desnecessary resends
   19/7/05  • GID messages now expire after some time
              • This removes the necessity of combining msgNGID and msgRM flags
              • Some network messagin problems fixed with this
   24/7/05  • RM message resend default timer changed to 1500 ms
   22/8/05  • Fixed memory leak with receive buffer
   29/8/05  • Fixed Client login bugs
            • Better debug log support
}

Unit TERRA_Network;
{$I terra.inc}

Interface
Uses SysUtils,
  TERRA_Utils, TERRA_IO, TERRA_OS, TERRA_Sockets, TERRA_Application;

Const
//Message types
  nmIgnore          = 0;  // Dummy message
  nmPing            = 1;  // Ping message
  nmUnused          = 2;  // Pong message
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

    Public
      Constructor Create(Opcode:Byte);

      Property Opcode:Byte Read GetOpcode;
      Property Owner:Word Read GetOwner Write SetOwner;
      Property Length:Word Read GetLength Write SetLength;
  End;

  MessageHandler = Procedure(Msg:NetMessage; Sock:Socket) Of Object;

  NetObject = Class
    Protected
      _UDPSocket:Integer;        //Socket handle
      _Input:Cardinal;        //Current input in bytes
      _Output:Cardinal;       //Current output in bytes

      Function CreateSocket:Integer;
      Function BindSocket(Port:Word):Integer;
      Function SendPacket(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean; 

      Function OnSendFail(Dest:SocketAddress; Sock:Socket; Msg:NetMessage):Boolean; Virtual;
      Procedure OnPacketReceived(Sock:Socket; Msg:NetMessage); Virtual;

    Public
      _Name:AnsiString;             // Local Client Name
      _Address:SocketAddress;  // Local Address
      _Sender:SocketAddress;   // Address of message sender
      _HostName:AnsiString;         // Local host name
      _LocalID:Word;            // Local ID
      _Port:Word;               // Local port
      _Version:Word;            // Object Version
      _TotalDownload:Cardinal;  // Session download total
      _TotalUpload:Cardinal;    // Session upload total
      _NextUpdate:Cardinal;     // Next IO check time
      _Upload:Cardinal;         // Current upload kb/s
      _Download:Cardinal;       // Current download kb/s
      _OpcodeList:Array[0..255]Of MessageHandler;

      // Encodes a message
      EncodeMessage:Procedure (Data:PByteArray;Size:Integer);
      // Decodes a message
      DecodeMessage:Procedure (Data:PByteArray;Size:Integer);

      Constructor Create();  //Creates a new object instance
      Destructor Destroy;Reintroduce;Virtual; //Shutdown the object

      Function MakeSocketAddress(Var SockAddr:SocketAddress; Port:Word; Hostname:AnsiString):SmallInt;

      Function ReceivePacket(Sock:Socket):Boolean;
      Procedure UpdateIO; //Updates upload/download status
      Procedure Update;Virtual;Abstract; //Handles standart messages

      Procedure AddHandler(Opcode:Byte; Handler:MessageHandler); Virtual;

      // Validates a message
      Function ValidateMessage(Msg:NetMessage):Boolean; Virtual; Abstract;

      Procedure ReturnMessage(Sock:Socket; Msg:NetMessage); // Sends a message to the last sender

      // Message Handlers -> Need one of this for each message type
      Procedure InvalidMessage(Msg:NetMessage; Sock:Socket);
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

      Destructor Destroy; Override;
      Class Function Instance:NetworkManager;
  End;


Function GetMsgDesc(MsgId:Byte):AnsiString;
Function GetNetErrorDesc(ErrorCode:Word):AnsiString;

//Function CreateMessageWithWord(Opcode:Byte; Code:Word):NetMessage;  // Creates a server message

Implementation
Uses TERRA_Error, TERRA_Log;

Var
  _NetworkManager:ApplicationObject;

Function GetNetErrorDesc(ErrorCode:Word):AnsiString;
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

Function _GetOpcode(MsgId:Byte):AnsiString;
Begin
  Result:='Unknown message type['+IntToString(MsgId)+']';
End;

Function GetMsgDesc(MsgId:Byte):AnsiString;
Begin
  Case MsgId Of
    nmServerAck:      Result:='Server.Ack';
    nmServerError:    Result:='Server.Error';
    nmServerShutdown: Result:='Server.Shutdown';
    nmClientJoin:     Result:='Client.Join';
    nmClientDrop:     Result:='Client drop';
    nmIgnore:         Result:='Ignore';
    nmPing:           Result:='Ping';
  Else
    Result := 'opcode #'+IntToString(MsgID);
  End;
End;

Procedure LogMsg(Prefix:AnsiString;Msg:NetMessage;Postfix:AnsiString);
Var
  S:AnsiString;
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

Function NetObject.CreateSocket:Integer;
Begin
  Result := TERRA_Sockets._socket(PF_INET, SOCK_DGRAM, 0); //Create a network socket
  If Result = SOCKET_ERROR Then  //Check for errors
  Begin
    RaiseError('Network.CreateSocket: Unable to open a socket.');
    Result := SOCKET_ERROR;
    Exit;
  End;

  MakeNonBlocking(Result, False);
End;

Function NetObject.MakeSocketAddress(Var SockAddr:SocketAddress; Port:Word; Hostname:AnsiString):SmallInt;
Var
  IP:AnsiString;
Begin
  Result:=0;

  IP := LookupHostAddress(Hostname);  //Resolve the server address

  Log(logDebug, 'Network', 'Calling htons()');
  SockAddr.Family := PF_INET;     // Set the address format
  SockAddr.Port := htons(Port);   // Convert to network byte order (using htons) and set port
  // Specify the IP

  Log(logDebug, 'Network', 'Calling inet_addr()');
  SockAddr.Address := inet_addr(PAnsiChar(IP));
  FillChar(SockAddr.Zero[1],8,0); //Zero fill

  //Check for error in IP
  If SockAddr.Address=-1 Then
  Begin
    RaiseError('Network.'+_Name+'.MakeSocketAddress: Unable to resolve IP address from '+ Hostname +'.');
    Result := SOCKET_ERROR;
    Exit;
  End;

  Log(logDebug, 'Network', 'Socket is ready');
End;

Function NetObject.BindSocket(Port:Word):Integer;
Var
  SocketInfo:SocketAddress;
  Opv:Integer;
Begin
  Opv:=1;
  If (setsockOpt(_UDPSocket, SOL_Socket, SO_REUSEADDR,@Opv,SizeOf(Opv))=SOCKET_ERROR) Then
  Begin
    RaiseError('Network.'+_Name+'.BindSocket: Unable to change socket mode.');
    Result:=SOCKET_ERROR;
    Exit;
  End;

  SocketInfo.Family:=PF_INET;   // Set the address format
  SocketInfo.Port := htons(Port); // Convert to network byte order (using htons) and set port
  SocketInfo.Address:=0;        // Specify local IP
  FillChar(SocketInfo.Zero[1],8,0); // Zero fill

   // Bind socket
  Result := bind(_UDPSocket, SocketInfo, SizeOf(SocketInfo));

  //Check for errors
  If Result=SOCKET_ERROR Then
  Begin
    RaiseError('Network.'+_Name+'.BindSocket: Unable to bind socket on port ' + IntToString(Port) + '.');
    Exit;
  End;
End;

Function NetObject.SendPacket(Dest:SocketAddress;  Sock:Socket; Msg:NetMessage):Boolean;
Var
  Len, Count:Integer;
  N:Integer;
  Timer:Cardinal;
  P:PByte;
Begin
  If (Sock.Closed) Then
  Begin
    Result := False;
    Exit;
  End;

  Msg.Length := Msg.Position - SizeOf(MessageHeader);
  {$IFDEF NETDEBUG}WriteLn('Packet size: ',Msg.Length);{$ENDIF}
  //LogMsg(_Name+'.Send():',@Rm,' to '+GetIP(Dest.Address));

    //EncodeMessage(Msg);

  //Send packet
  {$IFDEF NETDEBUG}WriteLn('Writing to socket');{$ENDIF}
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

  //Check for errors
  Result := False;
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

  {$IFDEF NETDEBUG}WriteLn('Reading from socket');{$ENDIF}

  //Check for a message
  Cnt := 0;
  P := @Header;
  Repeat
    Rem := SizeOf(MessageHeader) - Cnt;
    N := Sock.Read(P, Rem);
    {$IFDEF NETDEBUG}WriteLn('Read result: ',N);{$ENDIF}

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

  OnPacketReceived(Sock, Msg);

  {$IFDEF NETDEBUG}WriteLn('Validating message');{$ENDIF}
  Msg.Seek(SizeOf(MessageHeader));
  ValidMsg := ValidateMessage(Msg);
  //WriteLn('Rec: ',Msg.Opcode);

  If ValidMsg Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('Invoking opcode ',Msg.Opcode);{$ENDIF}
    If Assigned(_OpcodeList[Header.Opcode]) Then
    Begin
      Msg.Seek(SizeOf(MessageHeader));
      _OpcodeList[Header.Opcode](Msg, Sock) // Call message handler
    End;
  End Else
  If (Header.Opcode<>nmServerAck) Then
    Log(logWarning,'Network','Network.'+_Name+'.Update: Invalid opcode ['+IntToString(Header.Opcode)+']');

  DestroyObject(@Msg);

  {$IFDEF NETDEBUG}WriteLn('Opcode ',Header.Opcode,' processed');{$ENDIF}
End;

Procedure NetObject.InvalidMessage(Msg:NetMessage; Sock:Socket);
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
  _Name:='NetObject';

  _Upload := 0;
  _Download := 0;
  _Input := 0;
  _Output := 0;
  _TotalUpload := 0;
  _TotalDownload := 0;
  For I:=0 To 255 Do
    _OpcodeList[I] := InvalidMessage;
  _OpcodeList[nmIgnore] := IgnoreMessage;

  _UDPSocket := -1;
End;

Destructor NetObject.Destroy;
Begin
  If (_UDPSocket<>-1) Then
    CloseSocket(_UDPSocket);
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

Procedure NetObject.ReturnMessage(Sock:Socket; Msg:NetMessage); //Sends a message to the last sender
Begin
  SendPacket(_Sender, Sock, Msg);
End;


Destructor NetworkManager.Destroy;
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
Begin
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

{$IFDEF DEBUGMODE}
Procedure NetLogFilter(S,S2:AnsiString);
Begin
  WriteLn(S2);
End;
{$ENDIF}

Procedure NetObject.OnPacketReceived(Sock:Socket; Msg: NetMessage);
Begin
  // do nothing
End;

{ NetMessage }
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

Initialization
{$IFDEF DEBUGMODE}
  Log.Instance.SetFilter(logDebug, NetLogFilter);
  Log.Instance.SetFilter(logWarning, NetLogFilter);
{$ENDIF}
End.
