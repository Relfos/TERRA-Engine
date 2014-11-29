Unit TERRA_NetClient;

{$I terra.inc}

Interface
Uses TERRA_Application, TERRA_OS, TERRA_Sockets, TERRA_Network;

Type
  NetClient = Class(NetObject)
    Protected
      _Status:NetStatus;             // Connection to server status
      _ServerAddress:SocketAddress;  // Address of the server
      _GUID:Word;                     // Random number used by the server to search for duplicate Clients
      _LastPing:Cardinal;             // Used to check for dead connection
      _Latency:Cardinal;      // Latency to server
      _PingStart:Cardinal;    // Used to measure latency
      _JoinTime:Cardinal;

      _IsConnecting:Boolean;
      _UserName:AnsiString;
      _Password:AnsiString;
      _TCPSocket:Socket;

      Procedure ClearConnection(ErrorCode:Integer);

    Public
      //Creates a new client instance
      Constructor Create();

      //Destroys the client instance
      Destructor Destroy;Override;

      //Connects to a server
      Procedure Connect(Port,Version:Word; Server,UserName,Password:AnsiString);

      //Disconnects from server
      Procedure Disconnect(ErrorCode:Integer=0);

      //Handles messages
      Procedure Update; Override;

      Function CreateJoinMessage(Username, Password, DeviceID:AnsiString; GUID:Word):NetMessage; //Creates a server message

      //Send a message to the server
      Procedure SendMessage(Msg:NetMessage);

      Procedure SendEmptyMessage(Opcode:Byte); 

      Procedure ConnectionStart; Virtual; //Event: When a connection start
      Procedure ConnectionEnd(ErrorCode:Integer; ErrorLog:AnsiString); Virtual;   //Event: When a connection ends

      Function IsConnected():Boolean;

      Function ValidateMessage(Msg:NetMessage):Boolean; Override;

      //Message handlers
      Procedure PingMessage(Msg:NetMessage; Sock:Socket);
      Procedure ShutdownMessage(Msg:NetMessage; Sock:Socket);

      Property IsConnecting:Boolean Read _IsConnecting;

      Property Status:NetStatus Read _Status;
      Property Latency:Cardinal Read _Latency;
  End;

Implementation
Uses TERRA_Log;

{ NetClient }
Function NetClient.CreateJoinMessage(Username, Password, DeviceID:AnsiString; GUID:Word):NetMessage; //Creates a server message
Begin
  Result := NetMessage.Create(nmClientJoin);
  Result.Owner := ID_UNKNOWN;
  Result.Write(@GUID, 2);
  Result.Write(@_Version, 2);
  Result.WriteString(Username);
  Result.WriteString(Password);
  Result.WriteString(DeviceID);
End;

//Ping message
Procedure NetClient.PingMessage(Msg:NetMessage; Sock:Socket);
Begin
  Msg.Owner := _LocalId;
  SendMessage(Msg); //Tell server that we are alive
End;

Procedure NetClient.ShutdownMessage(Msg:NetMessage; Sock:Socket);
Var
  Code:Word;
Begin
  Msg.Read(@Code, 2);
  Disconnect(Code);
End;

 // Creates a new client instance
Constructor NetClient.Create();
Begin
  Inherited Create();

  NetworkManager.Instance.AddObject(Self);

  _Status := nsDisconnected;
  _Name := 'NetClient';
  _LastPing := 0;

  _OpcodeList[nmPing] := PingMessage;
  _OpcodeList[nmServerShutdown] := ShutdownMessage;

  _GUID:= (GetTime() Mod 65214);
End;

// Disconnects from server and destroys the client instance
Destructor NetClient.Destroy;
Begin
  NetworkManager.Instance.RemoveObject(Self);

  While (ReceivePacket(_TCPSocket)) Do;

  Disconnect();

  If Assigned(_TCPSocket) Then
  Begin
    _TCPSocket.Destroy;
    _TCPSocket := Nil;
  End;

  Inherited Destroy();
End;


Function NetClient.ValidateMessage(Msg:NetMessage):Boolean;
Var
  Code:Word;
  ErrorLog:AnsiString;
Begin
  //Is this an ACK?
  Case Msg.Opcode Of
    nmServerAck:
      Begin
        //Set our ID
        Msg.Read(@Code, 2);
        _LocalID := Code;
        _Status := nsConnected;
        _IsConnecting := False;
        ConnectionStart();
        Result := False;
        Exit;
      End;

    nmServerError:
      Begin
        Msg.Read(@Code, 2);
        If Not Msg.EOF Then
          Msg.ReadString(ErrorLog);
        Log(logError,'Network','ErrorMessage: '+GetNetErrorDesc(Code));

        Result := False;
        If (Code = errAlreadyConnected) And (Self.IsConnected) Then
        Begin
          Exit;
          // do nothing
        End Else
        Begin
          _IsConnecting := False;
          Self.ConnectionEnd(Code, ErrorLog);
          If (Code = errDuplicatedGUID) Then
            _GUID := (GetTime() Mod 65214);
          Exit;
        End;
      End;
    Else
      Result := True;
  End;
End;

Procedure NetClient.Connect(Port,Version:Word; Server, UserName, Password:AnsiString);
Var
  JoinMsg:NetMessage;
Begin
  If (_IsConnecting) Or (_TCPSocket<>Nil) Then
    Exit;

  _Port := Port;
  _Version := Version;
  _UserName := UserName;
  _Password := Password;

  _IsConnecting := True;

  Log(logDebug,'Network',_Name+'.Connect: '+Server);

  //Create a socket for sending/receiving messages
  _TCPSocket := Socket.Create(Server, _Port);
  If (_TCPSocket.Closed) Then
  Begin
    Self.ClearConnection(errConnectionFailed);
  End Else
  Begin
    _TCPSocket.SetBlocking(False);
    _TCPSocket.SetDelay(False);
    _JoinTime := GetTime();

    Log(logDebug, 'Network', 'Sending join message');
    JoinMsg := CreateJoinMessage(_UserName, _Password, Application.Instance.GetDeviceID(), _GUID);
    SendMessage(JoinMsg);  //Send the packet
    JoinMsg.Destroy();
  End;
End;

// Handles messages
Procedure NetClient.Update;
Var
  Delta:Integer;
Begin
  UpdateIO();

  If (Assigned(_TCPSocket)) Then
  Begin
    // Process messages
    While ReceivePacket(_TCPSocket) Do;
  End;

  If (_IsConnecting) Then
  Begin
    Delta := GetTime() - _JoinTime;
    If (Delta>20*1000) Then
    Begin
      _IsConnecting := False;
      Self.ClearConnection(errConnectionTimeOut);
    End;
  End Else
  If (GetTime - _LastPing>30*1000) Then
  Begin
    _LastPing := GetTime();
    Log(logDebug,'Network',_Name+'.Update: Pong');

    Self.SendEmptyMessage(nmPing); //Tell server that we are alive
  End;

  If (Self._TCPSocket<>Nil) And (Self._TCPSocket.Closed) Then
  Begin
    {$IFNDEF STAYALIVE}
    Log(logWarning,'Network',_Name+'.Update: Connection lost');
    Disconnect(errConnectionLost); //Conection lost
    {$ENDIF}
  End;
End;

Procedure NetClient.ClearConnection(ErrorCode:Integer);
Begin
  _Status := nsDisconnected;
  ConnectionEnd(ErrorCode,'');

  If Assigned(_TCPSocket) Then
  Begin
    _TCPSocket.Destroy;
    _TCPSocket := Nil;
  End;
End;

// Send a message to the server
Procedure NetClient.SendMessage(Msg:NetMessage);
Begin
  If (Msg = Nil) Then
    Exit;

  If (_Status <> nsConnected) And (Msg.Opcode<>nmClientJoin) Then
    Exit;

  _LastPing := GetTime();

  //_NextPong := GetTime() + PONG_TIME;
  If (Msg.Opcode <> nmClientJoin) Then
    Msg.Owner := _LocalID;

  If Not SendPacket(_ServerAddress, _TCPSocket, Msg) Then
  Begin
    ClearConnection(errConnectionLost);
  End;
End;

// Send a message to the server
Procedure NetClient.Disconnect(ErrorCode:Integer=0);
Begin
  If (_Status <> nsConnected) Then
    Exit;

  Self.SendEmptyMessage(nmClientDrop);
  ClearConnection(ErrorCode);
End;

Procedure NetClient.ConnectionStart; //Event: When a connection start
Begin
End;

Procedure NetClient.ConnectionEnd;   //Event: When a connection end
Begin
End;

Function NetClient.IsConnected():Boolean;
Begin
  Result := (Self._Status = nsConnected);
End;

Procedure NetClient.SendEmptyMessage(Opcode: Byte);
Var
  Msg:NetMessage;
Begin
  Msg := NetMessage.Create(Opcode);
  Self.SendMessage(Msg);
  Msg.Destroy();
End;

End.