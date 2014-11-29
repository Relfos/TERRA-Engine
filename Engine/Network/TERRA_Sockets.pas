{
@abstract(Net Streams)
@author(Sergio Flores <relfos@gmail.com>)
@created(December 1, 2005)
@lastmod(December 12, 2005)
The HTTP unit provides support for downloading files using the HTPP protocol.

Version History
  1/12/06   • First implementation
  3/12/06   • Some bug fixes
 24/07/06   • Remodeled to work with LNetStream
 28/12/06   • Added OpenIncomingStream, to work with incoming connections
 11/01/07   • Fixed bug when using different incoming ports
}

Unit TERRA_Sockets;
{$I terra.inc}

{-$DEFINE NETDEBUG}

{$IFDEF WINDOWS}{$UNDEF ANDROID}{$ENDIF}

Interface
Uses TERRA_Utils, TERRA_IO, TERRA_OS
{$IFDEF ANDROID},TERRA_Java{$ENDIF};

Const
  PACKET_SIZE = 1024;

Const
  // Socketlib constants
  PF_INET           = 2;      // Internet address format
  SOCK_STREAM       = 1;      // TCP format
  SOCK_DGRAM        = 2;      // UDP format
  SOCKET_ERROR      = -1;     // Error return value
  SOCK_VER:Word     = 514;    // Version of winsock to use
  SOL_SOCKET        = $FFFF;  // options for socket level
  SO_REUSEADDR      = $0004;  // allow local address reuse
  SO_BROADCAST      = $0020;
  SO_RCVTIMEO       = $1006;
  SO_SNDBUF         = $1001;
  IPPROTO_TCP       = 6;
  FIONBIO           = $8004667E; // Set socket to non-blocking
  INVALID_SOCKET    = Not(0);    // unsigned representation of -1
  INET_ANY          = 0;

  F_GETFL            = 3;
  F_SETFL             = 4;
  O_NONBLOCK          = 04000;

  WSAEWOULDBLOCK  = 10035;

Type
  PSocketAddress = ^SocketAddress;
  // Socket info structure
  SocketAddress = Packed Record
    Family:Word;
    Port:Word;
    Address:Integer;
    Zero:Array[1..8]Of AnsiChar;
  End;

  PHostEntity=^THostEntity;
  THostEntity=Packed Record
    Name:PAnsiChar;
    Aliases:^PAnsiChar;
    AddressType:{$IFDEF WINDOWS}SmallInt{$ELSE}Integer{$ENDIF};
    Length:{$IFDEF WINDOWS}SmallInt{$ELSE}Integer{$ENDIF};
    AddressList:^PAnsiChar;
   End;

{$IFDEF WINDOWS}
// Interface to WinSock
Const
  WinSockDLL='wsock32.dll';

Type
  // Winsock info structure
  WSADATA=Packed Record
    Version:Cardinal;
    Description:Array[1..257]Of AnsiChar;
    SystemStatus:Array[1..129]Of AnsiChar;
    MaxSockets:Integer;
    MaxUdpDg:Integer;
    VendorInfo:Integer;
    End;

  // API calls
  Function WSAStartup(wVersionRequested:Word;Var lpWSAData:WSADATA):Integer;StdCall;External WinSockDLL;
  Function WSACleanup:Integer;StdCall; external WinSockDLL;
  function WSAGetLastError: Integer; stdcall;external WinSockDLL;
  Function _socket(af,prototype,protocol:Integer):Integer;StdCall; external WinSockDLL name 'socket';
  Function bind(socket:Integer;Var bindto:SocketAddress;tolen:Integer):Integer;StdCall; external WinSockDLL;
  Function closesocket(socket:Integer):Integer;StdCall; external WinSockDLL;
  Function connect(socket:Integer;Var Addr:SocketAddress;AddrLen:Integer):Integer; Stdcall; external WinSockDLL;
  Function send(socket:Integer;Var Buffer;Length,Flags:Integer):Integer; Stdcall;external WinSockDLL;
  Function sendto(socket:Integer;Var Buffer;Length:Longint;Flags:Longint;Var addrto:SocketAddress;tolen:Longint):Longint;StdCall; external WinSockDLL;
  Function recv(socket:Integer;Var Buffer;len,flags:Integer):Integer;StdCall; external WinSockDLL;
  Function recvfrom(socket:Integer;Var Buffer;Len,Flags:Integer;Var AddrFrom:SocketAddress;Var FromLen:Integer):SmallInt;StdCall; external WinSockDLL;
  Function inet_addr(Addr:PAnsiChar):Integer;StdCall; external WinSockDLL;
  Function ioctlsocket(socket:Integer;cmd:Cardinal;var argp:Integer):Longint;StdCall; external WinSockDLL;
  Function htons(hostshort:Word):Word;StdCall; external WinSockDLL;
  Function listen(socket:Integer;Backlog:Integer):Integer;StdCall; external WinSockDLL;
  Function accept(socket:Integer;Var Addr:SocketAddress;Var Len:Integer):Integer; StdCall; external WinSockDLL;
  Function gethostname(Name:PAnsiChar;Len:Integer):Integer;Stdcall;external WinSockDLL;
  Function gethostbyname(Name:PAnsiChar):PHostEntity; Stdcall;external WinSockDLL;
  Function setsockopt(socket:Integer;level,optname:Integer;optval:Pointer;optlen:Integer): Integer;Stdcall;external WinSockDLL;
  Function shutdown(Socket:Integer;how:Integer):Integer; Stdcall;external WinSockDLL;
{$ELSE}
  function accept(s:longint; Var addr:SocketAddress; Var len:Integer):longint;cdecl;external 'libc' name 'accept';
  function bind(s:longint; Var addr:SocketAddress; tolen:Integer):longint;cdecl;external 'libc' name 'bind';
  function connect(s:longint; Var addr:SocketAddress; len:Integer):longint;cdecl;external 'libc' name 'connect';
  function listen(s:longint; backlog:longint):longint;cdecl;external 'libc' name 'listen';
  function recv(s:longint; Var buffer; len, flags:longint):integer;cdecl;external 'libc' name 'recv';
  function recvfrom(s:longint; Var buf; len, flags:longint; Var from:Socketaddress;
           Var fromlen:Integer):Integer;cdecl;external 'libc' name 'recvfrom';
  function send(s:longint; Var buffer; len, flags:longint):Integer;cdecl;external 'libc' name 'send';
  function sendto(s:longint; Var buffer; len, flags:longint; Var _to:Socketaddress;
           tolen:Integer):Integer;cdecl;external 'libc' name 'sendto';
  function setsockopt(s:longint; level:longint; optname:longint; optval:pointer; optlen:Integer):longint;cdecl;external 'libc' name 'setsockopt';
  function shutdown(s:longint; how:longint):longint;cdecl;external 'libc' name 'shutdown';
  function _socket(domain:longint; _type:longint; protocol:longint):longint;cdecl;external 'libc' name 'socket';
  function closesocket(sock:longint):Longint; external 'libc' name
  {$IFDEF OSX}'_close'{$ELSE}
  {$IFDEF IPHONE}'_close'{$ELSE}'close'{$ENDIF}{$ENDIF};

  Function gethostbyname(name:PAnsiChar):PHostEntity; cdecl; external 'libc' name 'gethostbyname';
    Function gethostname(namee:PAnsiChar; length:longint):longint;cdecl;external 'libc' name 'gethostname';
    function ioctl(_para1:longint; _para2:longint; arg3:Pointer):longint;cdecl; external 'libc' name 'ioctl';
  Function fcntl(s, cmd:Integer; arg:Int64):Integer; cdecl; external 'libc' name 'fcntl';
{$ENDIF}

Type
  Socket = Class(Stream)
    Protected
      _Handle:Integer;
      _Blocking:Boolean;
      _Closed:Boolean;
      _Address:Integer;
      _Error:Boolean;

      Function GetEOF:Boolean;Override;

    Public
      Constructor Create(CustomHandle:Integer); Overload;
      Constructor Create(Host:AnsiString; Port:Word); Overload;
      Destructor Destroy; Override;

      Function Read(Data:Pointer; Size:Cardinal):Cardinal;Override;
      Function Write(Data:Pointer; Size:Cardinal):Cardinal;Override;

      Procedure WriteString(S:AnsiString; NullTerminated:Boolean = False);Override;
      Procedure ReadString(Var S:AnsiString; NullTerminated:Boolean = False);Override;
      Procedure ReadLine(Var S:AnsiString);Override;

      Procedure SetBlocking(Block:Boolean);
      Procedure SetDelay(Delay:Boolean);
      Procedure SetBufferSize(Size:Integer);
      Procedure SetTimeOut(Duration:Integer);

      Property Closed:Boolean Read _Closed;
      Property Blocking:Boolean Read _Blocking Write SetBlocking;
      Property Address:Integer Read _Address;
      Property Error:Boolean Read _Error;
  End;

  // Returns IP from host address via DNS lookup
  Function LookUpHostAddress(HostName:AnsiString):AnsiString;

  // Retrieves name of localhost
  Function GetLocalHost:AnsiString;

  Procedure MakeNonBlocking(Handle:Integer; Block:Boolean);

  Function OpenIncomingStream(Port:Integer):Socket;

  Function GetIP(IP:Cardinal):AnsiString;

{$IFNDEF WINDOWS}
Function inet_addr(IP:PAnsiChar):Integer;
Function htons(host:Word):Word;
{$ENDIF}

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application;

{$IFNDEF WINDOWS}
Function inet_addr(IP:PAnsiChar):Integer;
Var
    S:AnsiString;
    I,j,k     : Longint;
    Temp : Array[1..4] Of Byte;
Begin
  Result := 0;
  S := IP;
  Temp[4] := StringToInt(GetNextWord(S, '.'));
  Temp[3] := StringToInt(GetNextWord(S, '.'));
  Temp[2] := StringToInt(GetNextWord(S, '.'));
  Temp[1] := StringToInt(S);

  Result := Temp[4];
  Result := Result or Integer( (Temp[3]) shl 8);
  Result := Result or Integer( (Temp[2]) shl 16);
  Result := Result or Integer( (Temp[1]) shl 24);
end;

Function htons(host:Word):Word;
Begin
    Result := Swap(Host);
End;
{$ENDIF}

// Decodes an IP stored in 32bit integer format to a string
Function GetIP(IP:Cardinal):AnsiString;
Var
  N:Array[0..4]Of Byte;
Begin
  Move(IP, N[0], 4);
  Result := IntToString(N[0])+'.'+IntToString(N[1])+'.'+IntToString(N[2])+'.'+IntToString(N[3]);
End;

{$IFDEF IPHONE}
Function ResolveHostAddress(HostName:AnsiString):AnsiString;
Begin
  Log(logDebug, 'Sockets', 'Looking up host: '+ HostName);
  Result := resolveHost(PAnsiChar(HostName));
  Log(logDebug, 'Sockets', 'Result: '+ Result);
End;

{$ELSE}
{$IFDEF ANDROID}
Function ResolveHostAddress(HostName:AnsiString):AnsiString;
Var
  Utils:JavaClass;
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Result := '127.0.0.1';

  Java_Begin(Frame);
  Utils := JavaClass.Create(UtilsClassPath, Frame);

  Params := JavaArguments.Create(Frame);
  Params.AddString(HostName);
  Result := Utils.CallStaticStringMethod('getNetAddress', Params);
  Params.Destroy();

  If Result = '' Then
    Result := '127.0.0.1';

  Utils.Destroy();
  Java_End(Frame);

  Log(logDebug, 'Sockets', 'Result: '+ Result);
End;
{$ELSE}
Function ResolveHostAddress(HostName:AnsiString):AnsiString;
Var
  Host:PHostEntity;
Begin
  Result:='127.0.0.1';
  If HostName='' Then
    Exit;

  If HostName[1] In ['0'..'9'] Then
  Begin
    Result := HostName;
    Exit;
  End;

  Log(logDebug, 'Sockets', 'Looking up host: '+ HostName);
  Host := PHostEntity(GetHostByName(PAnsiChar(HostName)));
  If (Not Assigned(Host)) Then
    Exit;

  Log(logDebug, 'Sockets', 'Found '+IntToString(Host.Length));
  If (Host.Length <= 0) Then
    Exit;

        Result := IntToString(Byte(Host.AddressList^[0]))+'.'+
                  IntToString(Byte(Host.AddressList^[1]))+'.'+
                  IntToString(Byte(Host.AddressList^[2]))+'.'+
                  IntToString(Byte(Host.AddressList^[3]));
  Log(logDebug, 'Sockets', 'Result: '+Result);
End;
{$ENDIF}
{$ENDIF}

Function GetLocalHost:AnsiString;
Var
  Len:Word;
  Buffer:Pointer;
Begin
  Len := 128;
  GetMem(Buffer, Len);
  GetHostName(Buffer, Len);
  Result := Copy(PAnsiChar(Buffer), 1, Length(PAnsiChar(Buffer)));
  FreeMem(Buffer, Len);
End;

Type
  AddressCache = Record
    Name:AnsiString;
    IP:AnsiString;
  End;

Var
  _Addresses:Array Of AddressCache;
  _AddressCount:Integer = 0;

Function LookUpHostAddress(HostName:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  HostName := LowStr(HostName);

  For I:=0 To Pred(_AddressCount) Do
  If (_Addresses[I].Name = HostName) Then
  Begin
    Result := _Addresses[I].IP;
    Exit;
  End;

  Result := ResolveHostAddress(HostName);
  Inc(_AddressCount);
  SetLength(_Addresses, _AddressCount);
  _Addresses[PreD(_AddressCount)].Name := HostName;
  _Addresses[PreD(_AddressCount)].IP := Result;
End;


(*Function LookUpHostAddress(HostName:AnsiString):AnsiString;
Var
  Host:THostEntry;
Begin
  Result:='127.0.0.1';
  If HostName<>'' Then
  Begin
    If HostName[1] In ['0'..'9'] Then
      Result:=HostName
    Else
    Begin
      If ResolveHostByName(HostName,Host) Then
        Result:=GetIP(Integer(Host.Addr));
    End;
  End;
End;


Function GetLocalHost:AnsiString;
Begin
  Result := GetHostName;
End;
*)

{ Socket }
Constructor Socket.Create(CustomHandle:Integer);
Begin
  _Closed := False;
  _Handle := CustomHandle;
  _Blocking := False;
End;

Constructor Socket.Create(Host:AnsiString; Port:Word);
Var
  IP:AnsiString;
  N:Integer;
  Addr:SocketAddress;
Begin
  _Handle := -1;
  _Blocking := True;
  _Closed := True;
  _Error := False;

  //Resolve the server address
  IP := LookupHostAddress(Host);
  If (IP = '127.0.0.1') And (Host<>IP) And (Host<>'localhost') Then
  Begin
    Log(logError, 'Sockets', 'Unable to resolve host address: '+Host);
    Exit;
  End;

  Log(logDebug, 'Sockets', 'Address found: '+IP);

  Log(logDebug, 'Sockets', 'Creating a socket, port '+IntToString(Port));
  _Handle := _socket(PF_INET, SOCK_STREAM, IPPROTO_TCP); //Create a network socket

  If _Handle = SOCKET_ERROR Then  //Check for errors
  Begin
    _Error := True;
    Log(logError, 'Sockets', 'Unable to open a socket, host='+Host+' port='+IntToString(Port));
    Exit;
  End;

  //Set the address format
  Addr.Family := PF_INET;
  //Convert to network byte order (using htons) and set port
  Addr.Port := htons(Port);
  //Specify the IP
  Addr.Address := inet_addr(PAnsiChar(IP));
  _Address := Addr.Address;

  //Zero fill
  FillChar(Addr.Zero[1], 8, 0);

  Log(logDebug, 'Sockets', 'Connecting');
  N := TERRA_Sockets.connect(_Handle, Addr, SizeOf(Addr));

  //Check for errors
  If N = SOCKET_ERROR Then
  Begin
    _Error := True;

    Log(logError, 'Sockets', 'Unable to connect to host: '+Host);
    {$IFDEF WINDOWS}
    Log(logError, 'Sockets', 'Sock error: '+IntToString(WSAGetLastError));
    {$ENDIF}
    Exit;
  End;

  Log(logDebug, 'Sockets', 'Socket is ready');
  _Closed := False;
End;

Destructor Socket.Destroy;
Begin
  If (_Handle>=0) Then
  Begin
    Shutdown(_Handle, 2);
    CloseSocket(_Handle);
  End;
End;

Procedure Socket.SetTimeOut(Duration: Integer);
Type
  TimeVal = Packed Record
    tv_sec:Cardinal;
    tv_usec:Cardinal;
  End;
Var
  TV:TimeVal;
Begin
  Duration := Duration Div 1000;
  TV.tv_sec := Duration;
  TV.tv_usec := 0;
  setsockopt(_Handle, SOL_SOCKET, SO_RCVTIMEO, @tv, sizeof(TV));
End;

Procedure Socket.SetDelay(Delay: Boolean);
Const
  TCP_NODELAY = 1;
Var
  N:Integer;
Begin
  If Delay Then
    N:=0
  Else
    N:=1;

  setsockopt(_Handle, IPPROTO_TCP, TCP_NODELAY, @N, 4);
End;

Procedure Socket.SetBufferSize(Size:Integer);
Begin
  setsockopt(_Handle, SOL_Socket, SO_SNDBUF, @Size, 4);
End;

Procedure Socket.SetBlocking(Block:Boolean);
Begin
  If (_Blocking=Block) Then
    Exit;

  _Blocking := Block;

  //Set the socket to non-blocking
  MakeNonBlocking(_Handle, Block);
End;


Function Socket.GetEOF:Boolean;
Begin
  Result := _Closed;
End;

Function Socket.Write(Data:Pointer; Size:Cardinal):Cardinal;
Var
  N:Integer;
  BlockSize:Integer;
Begin
  Result := 0;
  If EOF Then
    Exit;

  While Size>0 Do
  Begin
    If Size<PACKET_SIZE Then
      BlockSize := Size
    Else
      BlockSize := PACKET_SIZE;

    N := Send(_Handle, Data^, BlockSize,0);

    If (N = SOCKET_ERROR) Or (N<0) Then
    Begin
      _Error := True;
      Result := 0;
      Exit;
    End;

    Inc(PByte(Data), N);
    Dec(Size, N);
    Inc(Result, N);
  End;
End;

Function Socket.Read(Data:Pointer; Size:Cardinal):Cardinal;
Var
  N:Integer;
Begin
  {$IFDEF NETDEBUG} WriteLn('Begin sock.read');{$ENDIF}
  Result := 0;
  If (EOF) Or (Size<=0) Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('Bailout');{$ENDIF}
    Exit;
  End;

  {$IFDEF NETDEBUG}WriteLn('recv() call');{$ENDIF}
  N := Recv(_Handle, Data^, Size, 0);
  {$IFDEF NETDEBUG}WriteLn('result was ',N);{$ENDIF}

  If (N = SOCKET_ERROR) Or (N<0) Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('socket returned error');{$ENDIF}
    _Error := True;
    Result := 0;
    Exit;
  End;

  If (N=0) Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('socket was closed');{$ENDIF}
    _Closed := True;
  End;

  If (N<0 )Then
    Result := 0
  Else
    Result := N;
End;


Procedure Socket.ReadLine(Var S:AnsiString);
Var
  C:AnsiChar;
Begin
  S:='';
  C:=#0;
  While (C<>#13)And(Not EOF) Do
  Begin
    If Read(@C,1)<=0 Then
      Exit;

    If (C<>#13) Then
      S:=S+C;
  End;
  S:=TrimLeft(TrimRight(S));
End;

Procedure Socket.WriteString(S:AnsiString; NullTerminated:Boolean = False);
Var
  Len:Integer;
Begin
  If Pos(crLf,S)<=0 Then
    S := S + crLf;
  Len := Length(S);
  If Len>0 Then
    Write(@S[1],Len);
End;

Procedure Socket.ReadString(Var S:AnsiString; NullTerminated:Boolean = False);
Var
  N:Byte;
Begin
  S:='';
  Repeat
    If (Read(@N,1)<1) Or (N=10) Then
      Break;
    S:=S+Char(N);
  Until (False);
  S:=TrimLeft(TrimRight(S));
End;

Type
  WaitingSocket=Record
    Handle:Integer;
    Port:Integer;
  End;

Var
  WaitingList:Array Of WaitingSocket;
  WaitingCount:Integer = 0 ;

Function OpenIncomingStream(Port:Integer):Socket;
Var
  I,ID:Integer;
  Opv:Integer;
  ClientSock:Integer;
  ClientAddr:SocketAddress;
  Addr:SocketAddress;
Begin
  Result:=Nil;

  ID:=-1;
  For I:=0 To Pred(WaitingCount) Do
    If WaitingList[I].Port=Port Then
    Begin
      ID := I;
      Break;
    End;

  If ID=-1 Then
  Begin
    Inc(WaitingCount);
    SetLength(WaitingList,WaitingCount);
    ID:=Pred(WaitingCount);
    WaitingList[ID].Port := Port;
    WaitingList[ID].Handle := _socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    MakeNonBlocking(WaitingList[ID].Handle, False);

    If (setsockOpt(WaitingList[ID].Handle, SOL_Socket, SO_REUSEADDR, @Opv, SizeOf(Opv)) = SOCKET_ERROR) Then
    Begin
      RaiseError('Unable to change socket mode.');
      Exit;
    End;

    Addr.Family := PF_INET;
    Addr.Port := htons(Port);
    Addr.Address := 0;
    FillChar(Addr.Zero[1], 8, 0);

    If Bind(WaitingList[ID].Handle, Addr, SizeOf(Addr))<0 Then
    Begin
      RaiseError('Cannot bind socket.');
      Exit;
    End;

    Listen(WaitingList[ID].Handle, 5);
  End;

  Opv := SizeOf(ClientAddr);
  ClientSock := Accept(WaitingList[ID].Handle, ClientAddr, Opv);

  If ClientSock<>-1 Then
  Begin
    Result := Socket.Create(ClientSock);
    Result._Address := ClientAddr.Address;
  End;
End;

Procedure ReleaseSockets;
Var
  I:Integer;
Begin
  For I:=0 To Pred(WaitingCount) Do
  With WaitingList[I] Do
  Begin
    Shutdown(Handle, 2);
    CloseSocket(Handle);
  End;
End;

Procedure MakeNonBlocking(Handle:Integer; Block:Boolean);
Var
  N:Integer;
Begin
  If Block Then
    N:=0
  Else
    N:=1;

  //Set the socket to non-blocking
  {$IFDEF WINDOWS}
  ioctlsocket(Handle,FIONBIO,N);
  {$ELSE}
  {$IFDEF OSX}
    ioctl(Handle,Integer(FIONBIO),@N);
  {$ELSE}
  {$IFDEF IPHONE}
    ioctl(Handle,FIONBIO,@N);
  {$ELSE}
    N := fcntl(Handle, F_GETFL, 0);
    N := N Or O_NONBLOCK;
    fcntl(Handle, F_SETFL, N);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
End;

{$IFDEF WINDOWS}
Var
 Data:WSADATA;
 N:Longint;
{$ENDIF}
Initialization
{$IFDEF WINDOWS}
  N := WSAStartup(SOCK_VER,Data);
  //Check for errors
  If N<>0 Then
    Log(logError, 'Sockets', 'Unable to initialize Winsock session.');
{$ENDIF}
Finalization
  ReleaseSockets;
{$IFDEF WINDOWS}
  WSACleanup;
{$ENDIF}
End.
