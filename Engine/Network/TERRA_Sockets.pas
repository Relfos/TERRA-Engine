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
 * TERRA_Sockets
 * Implements portable sockets class
 ***********************************************************************************************************************
}

Unit TERRA_Sockets;
{$I terra.inc}

{-$DEFINE NETDEBUG}

{$IFDEF WINDOWS}{$UNDEF ANDROID}
{$ELSE}
{$DEFINE USE_FPC_SOCKETS}
{$ENDIF}


Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_OS
{$IFDEF USE_FPC_SOCKETS}, Sockets, BaseUnix{$ENDIF}
{$IFDEF ANDROID},TERRA_Java{$ENDIF};

Type
  PSocketAddress = Pointer;

  // Socket info structure
  // IPV4
  SocketAddress4 = Packed Record
    Family:Word;
    Port:Word;

    Address:Cardinal;
    Zero:Array[1..8]Of Byte;
  End;

  SocketAddress = SocketAddress4;

  (*
  // IPV6
  SocketAddress6 = Packed Record
    Family:Word;
    Port:Word;
    Flowinfo:Cardinal;     // IPv6 flow information
    Address:Array[1..8] Of Word;  // IPv6 address
    ScopeId:Cardinal;     // IPv6 scope-id
  End;
   *)
  PHostEntity = ^THostEntity;
  THostEntity = Packed Record
    Name:PAnsiChar;
    Aliases:^PAnsiChar;
    AddressType:{$IFDEF WINDOWS}SmallInt{$ELSE}Integer{$ENDIF};
    Length:{$IFDEF WINDOWS}SmallInt{$ELSE}Integer{$ENDIF};
    AddressList:^PAnsiChar;
   End;

Const
  PACKET_SIZE = 1024;

{$IFNDEF USE_FPC_SOCKETS}
  PF_INET           = 2;      // Internet address format


{$IFDEF WINDOWS}
  PF_INET6          = 23;
{$ELSE}
  PF_INET6          = 10;
{$ENDIF}

  SOCK_STREAM       = 1;      // TCP format
  SOCK_DGRAM        = 2;      // UDP format

  INVALID_SOCKET    = Not(0);    // unsigned representation of -1
  INET_ANY          = 0;

  SOL_SOCKET        = $FFFF;  // options for socket level

  IPPROTO_TCP       = 6;

  {$IFDEF LINUX}
  IPV6_V6ONLY       = 26;
  {$ELSE}
  IPV6_V6ONLY       = 27;
  {$ENDIF}

{$IFDEF WINDOWS}
// Interface to WinSock
Const
  WinSockDLL='wsock32.dll';

  FIONBIO           = $8004667E; // Set socket to non-blocking

  SOCK_VER        = 514;    // Version of winsock to use
  WSAEWOULDBLOCK  = 10035;

  MSG_NOSIGNAL = 0;

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
  Function socket(af,prototype,protocol:Integer):Integer;StdCall; external WinSockDLL name 'socket';
  Function bind(socket:Integer;bindto:PSocketAddress;tolen:Integer):Integer;StdCall; external WinSockDLL;
  Function closesocket(socket:Integer):Integer;StdCall; external WinSockDLL;
  Function connect(socket:Integer; Addr:PSocketAddress;AddrLen:Integer):Integer; Stdcall; external WinSockDLL;
  Function send(socket:Integer;Var Buffer;Length,Flags:Integer):Integer; Stdcall;external WinSockDLL;
  Function sendto(socket:Integer;Var Buffer;Length:Integer;Flags:Integer; addrto:PSocketAddress;tolen:Integer):Integer;StdCall; external WinSockDLL;
  Function recv(socket:Integer;Var Buffer;len,flags:Integer):Integer;StdCall; external WinSockDLL;
  Function recvfrom(socket:Integer;Var Buffer;Len,Flags:Integer; AddrFrom:PSocketAddress;Var FromLen:Integer):SmallInt;StdCall; external WinSockDLL;
  Function inet_addr(Addr:PAnsiChar):Cardinal;StdCall; external WinSockDLL;
  Function ioctlsocket(socket:Integer;cmd:Cardinal;var argp:Integer):Integer;StdCall; external WinSockDLL;
  Function htons(hostshort:Word):Word;StdCall; external WinSockDLL;
  Function listen(socket:Integer;Backlog:Integer):Integer;StdCall; external WinSockDLL;
  Function accept(socket:Integer; Addr:PSocketAddress;Var Len:Integer):Integer; StdCall; external WinSockDLL;
  Function gethostname(Name:PAnsiChar;Len:Integer):Integer;Stdcall;external WinSockDLL;
  Function gethostbyname(Name:PAnsiChar):PHostEntity; Stdcall;external WinSockDLL;
  Function setsockopt(socket:Integer;level,optname:Integer;optval:Pointer;optlen:Integer): Integer;Stdcall;external WinSockDLL;
  Function shutdown(Socket:Integer;how:Integer):Integer; Stdcall;external WinSockDLL;
{$ELSE}

  function accept(s:Integer; addr:PSocketAddress; Var len:Integer):Integer;cdecl;external 'libc' name 'accept';
  function bind(s:Integer; addr:PSocketAddress; tolen:Integer):Integer;cdecl;external 'libc' name 'bind';
  function connect(s:Integer; addr:PSocketAddress; len:Integer):Integer;cdecl;external 'libc' name 'connect';
  function listen(s:Integer; backlog:Integer):Integer;cdecl;external 'libc' name 'listen';
  function recv(s:Integer; Var buffer; len, flags:Integer):integer;cdecl;external 'libc' name 'recv';
  function recvfrom(s:Integer; Var buf; len, flags:Integer; from:PSocketaddress;
           Var fromlen:Integer):Integer;cdecl;external 'libc' name 'recvfrom';
  function send(s:Integer; Var buffer; len, flags:Integer):Integer;cdecl;external 'libc' name 'send';
  function sendto(s:Integer; Var buffer; len, flags:Integer; _to:PSocketaddress;
           tolen:Integer):Integer;cdecl;external 'libc' name 'sendto';
  function setsockopt(s:Integer; level:Integer; optname:Integer; optval:pointer; optlen:Integer):Integer;cdecl;external 'libc' name 'setsockopt';
  function shutdown(s:Integer; how:Integer):Integer;cdecl;external 'libc' name 'shutdown';
  function socket(domain:Integer; _type:Integer; protocol:Integer):Integer;cdecl;external 'libc' name 'socket';
  function closesocket(sock:Integer):Integer; external 'libc' name
  {$IFDEF OSX}'_close'{$ELSE}
  {$IFDEF IPHONE}'_close'{$ELSE}
  'close'{$ENDIF}
  {$ENDIF};


{$ENDIF}

{$IFDEF LINUX}{$DEFINE LINUX_SOCKETS}{$ENDIF}
//{$IFDEF ANDROID}{$DEFINE LINUX_SOCKETS}{$ENDIF}

Const
  {$IFDEF LINUX_SOCKETS}
  SO_REUSEADDR =  2;
  SO_BROADCAST =  6;
  SO_LINGER    =  13;
  SO_RCVTIMEO  =  20;
  SO_SNDBUF    =  7;
  {$ELSE}
  SO_REUSEADDR =  $0004;
  SO_BROADCAST =  $0020;
  SO_LINGER    =  $0080;
  SO_RCVTIMEO  =  $1006;
  SO_SNDBUF    =  $1001;
  {$ENDIF}

{$ELSE}
{$IFNDEF WINDOWS}
Const
  FIONBIO = $5421;

{$IFDEF LINUX}
  F_GETFL            = 3;
  F_SETFL             = 4;
  O_NONBLOCK          = 04000;
{$ELSE}
  F_SETFL             = 4;
  O_NONBLOCK          = $0004;
{$ENDIF}
  SOCK_NONBLOCK  = $40000000;


{$IFNDEF LINUX_SOCKETS}

{$IFNDEF LINUX}
MSG_NOSIGNAL = 0;
{$ENDIF}
SO_NOSIGPIPE = $1022;
{$ENDIF}

  Function gethostbyname(name:PAnsiChar):PHostEntity; cdecl; external 'libc' name 'gethostbyname';
  Function gethostname(namee:PAnsiChar; length:Integer):Integer;cdecl;external 'libc' name 'gethostname';
  Function ioctl(_para1:Integer; _para2:Integer; arg3:Pointer):Integer;cdecl; external 'libc' name 'ioctl';

  Function fcntl(fildes, cmd:Integer):Integer; cdecl; varargs; external 'libc' name 'fcntl';
{$ENDIF}
{$ENDIF}


   
Type
  NetSocket = Class(TERRAStream)
    Protected
      _Handle:Integer;
      _Blocking:Boolean;
      _Closed:Boolean;
      _Error:Boolean;
      _NetType:Integer;

      Function GetEOF:Boolean;Override;

    Public
      Constructor Create(CustomHandle:Integer); Overload;
      Constructor Create(Const Host:TERRAString; Port:Word); Overload;
      Procedure Release; Override;

      Function Read(Data:Pointer; Size:Cardinal):Cardinal; Override;
      Function Write(Data:Pointer; Size:Cardinal):Cardinal; Override;

      Procedure ReadLine(Var S:TERRAString); Override;

      Procedure SetBlocking(Block:Boolean);
      Procedure SetDelay(Delay:Boolean);
      Procedure SetBufferSize(Size:Integer);
      Procedure SetTimeOut(Duration:Integer);

      Property Closed:Boolean Read _Closed;
      Property Blocking:Boolean Read _Blocking Write SetBlocking;
      Property Error:Boolean Read _Error;
  End;

  // Returns IP from host address via DNS lookup
  Function LookUpHostAddress(HostName:TERRAString):TERRAString;

  // Retrieves name of localhost
  Function GetLocalHost:TERRAString;

  Function OpenIncomingStream(Port:Integer):NetSocket;

  Function GetIP(IP:Cardinal):TERRAString;

{$IFNDEF WINDOWS}
Function inet_addr(IP:PAnsiChar):Cardinal;
Function htons(Value:Word):Word;
{$ENDIF}

Const
  SOCKET_ERROR      = -1;     // Error return value

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Engine, TERRA_Application;


Function GetSocketNetType(Const Addr:TERRAString):Integer;
Begin                      (*
  If StringContains(':', Addr) Then
    Result := PF_INET6
  Else                   *)
    Result := PF_INET;
End;

{$IFDEF USE_FPC_SOCKETS}
  function accept(s:Integer; addr:PSocketAddress; Var len:Integer):Integer;
  Begin
    Result := fpaccept(S, Addr, @Len);
  End;

  function bind(s:Integer; addr:PSocketAddress; tolen:Integer):Integer;
  Begin
    Result := fpBind(S, addr, tolen);
  End;

  function connect(s:Integer; addr:PSocketAddress; len:Integer):Integer;
  Begin
    Result := fpConnect(S, Addr, Len);
  End;

  function listen(s:Integer; backlog:Integer):Integer;
  Begin
    Result := fpListen(S, backlog);
  End;

  function recv(s:Integer; Var buffer; len, flags:Integer):integer;
  Begin
    Result := fprecv(S, @Buffer, Len, Flags);
  End;

  function recvfrom(s:Integer; Var buf; len, flags:Integer; from:PSocketaddress; Var fromlen:Integer):Integer;
  Begin
    Result := fprecvfrom(S, @Buf, Len, Flags, From, @fromlen);
  End;

  function send(s:Integer; Var buffer; len, flags:Integer):Integer;
  Begin
    Result := fpSend(S, @buffer, len, flags);
  End;

  function sendto(s:Integer; Var buffer; len, flags:Integer; _to:PSocketaddress; tolen:Integer):Integer;
  Begin
    Result := fpSendTo(S, @Buffer, Len, Flags, _To, tolen);
  End;

  function setsockopt(s:Integer; level:Integer; optname:Integer; optval:pointer; optlen:Integer):Integer;
  Begin
    Result := fpsetsockopt(S, Level, Optname, optval, optlen);
  End;

  function shutdown(s:Integer; how:Integer):Integer;
  Begin
    Result := fpshutdown(S, How);
  End;

  function socket(domain:Integer; _type:Integer; protocol:Integer):Integer;
  Begin
    Result := fpSocket(Domain, _Type, protocol);
  End;

  function closesocket(sock:Integer):Integer;
  Begin
    Result := fpclose(Sock);
  End;
{$ENDIF}

(*{$IFNDEF WINDOWS}
function setsockopt(s:Integer; level:Integer; optname:Integer; optval:pointer; optlen:Integer):Integer;
Begin
  Result := fpsetsockopt(s, level, optname, optval, optlen);
End;
{$ENDIF}*)

{$IFNDEF WINDOWS}
Function inet_addr(IP:PAnsiChar):Cardinal;
Var
  S:TERRAString;
  I,j,k:Cardinal;
  Temp:Array[1..4] Of Byte;
Begin
  Result := 0;
  S := IP;
  Temp[4] := StringToInt(StringGetNextSplit(S, '.'));
  Temp[3] := StringToInt(StringGetNextSplit(S, '.'));
  Temp[2] := StringToInt(StringGetNextSplit(S, '.'));
  Temp[1] := StringToInt(S);

  Result := Temp[4];
  Result := Result Or (Temp[3]) Shl 8;
  Result := Result Or (Temp[2]) Shl 16;
  Result := Result Or (Temp[1]) Shl 24;
end;

Function htons(Value:Word):Word;
Begin
     Result := Swap(Value);
End;
{$ENDIF}

(*
Procedure inet_addr6_from_string(IP:TERRAString; Var Addr:SocketAddress6);
Var
  I:Integer;
  S:TERRAString;
Begin
  For I:=1 To 8 Do
  Begin
    S := StringGetNextSplit(IP, Ord(':'));
    Addr.Address[I] := HexStrToInt(S);
  End;
End;                  *)

// Decodes an IP stored in 32bit integer format to a string
Function GetIP(IP:Cardinal):TERRAString;
Var
  N:Array[0..4]Of Byte;
Begin
  Move(IP, N[0], 4);
  Result := IntegerProperty.Stringify(N[0])+'.'+IntegerProperty.Stringify(N[1])+'.'+IntegerProperty.Stringify(N[2])+'.'+IntegerProperty.Stringify(N[3]);
End;

{$IFDEF IPHONE}
Function ResolveHostAddress(HostName:TERRAString):TERRAString;
Begin
  Log(logDebug, 'Sockets', 'Looking up host: '+ HostName);
  Result := resolveHost(PAnsiChar(HostName));
  Log(logDebug, 'Sockets', 'Result: '+ Result);
End;

{$ELSE}
{$IFDEF ANDROID}
Function ResolveHostAddress(HostName:TERRAString):TERRAString;
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
  Result := Utils.CallStaticStringMethod(Frame, 'getNetAddress', Params);
  ReleaseObject(Params);

  If Result = '' Then
    Result := '127.0.0.1';

  ReleaseObject(Utils);
  Java_End(Frame);

  Log(logDebug, 'Sockets', 'Result: '+ Result);
End;
{$ELSE}

Function ResolveHostAddress(HostName:TERRAString):TERRAString;
Var
  Host:PHostEntity;
  C:TERRAChar;
Begin
  Result:='127.0.0.1';
  If HostName='' Then
    Exit;

  C := StringFirstChar(HostName);
  If (C>='0') And (C<='9') Then
  Begin
    Result := HostName;
    Exit;
  End;

  Engine.Log.Write(logDebug, 'Sockets', 'Looking up host: '+ HostName);
  Host := PHostEntity(GetHostByName(PAnsiChar(HostName)));
  If (Not Assigned(Host)) Then
    Exit;

  Engine.Log.Write(logDebug, 'Sockets', 'Found '+IntegerProperty.Stringify(Host.Length));
  If (Host.Length <= 0) Then
    Exit;

        Result := IntegerProperty.Stringify(Byte(Host.AddressList^[0]))+'.'+
                  IntegerProperty.Stringify(Byte(Host.AddressList^[1]))+'.'+
                  IntegerProperty.Stringify(Byte(Host.AddressList^[2]))+'.'+
                  IntegerProperty.Stringify(Byte(Host.AddressList^[3]));
  Engine.Log.Write(logDebug, 'Sockets', 'Result: '+Result);
End;
{$ENDIF}
{$ENDIF}

Function GetLocalHost:TERRAString;
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

(*Function SocketError():Integer;
Begin
  {$IFDEF WINDOWS}
  Result := WSAGetLastError();
  {$ELSE}
  Result := ErrNo;
  {$ENDIF}
End;*)


Type
  AddressCache = Record
    Name:TERRAString;
    IP:TERRAString;
  End;

Var
  _Addresses:Array Of AddressCache;
  _AddressCount:Integer = 0;

Function LookUpHostAddress(HostName:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  HostName := StringLower(HostName);

{For I:=0 To Pred(_AddressCount) Do
  If (_Addresses[I].Name = HostName) Then
  Begin
    Result := _Addresses[I].IP;
    Exit;
  End;}

  Result := ResolveHostAddress(HostName);
  Inc(_AddressCount);
  SetLength(_Addresses, _AddressCount);
  _Addresses[Pred(_AddressCount)].Name := HostName;
  _Addresses[Pred(_AddressCount)].IP := Result;
End;

Procedure MakeNonBlocking(Handle:Integer; Block:Boolean);
Var
  N, ErrorCode:Integer;
Begin
  If Block Then
    N := 0
  Else
    N := 1;

  //Set the socket to non-blocking
  {$IFDEF WINDOWS}
  ErrorCode := ioctlsocket(Handle, FIONBIO, N);
  {$ELSE}
  {$IFDEF MOBILE}
  ErrorCode := ioctl(Handle, FIONBIO, @N);
  {$ELSE}
  ErrorCode := fcntl(Handle, F_SETFL, {fcntl(Handle, F_GETFL, 0) Or }O_NONBLOCK);
  {$ENDIF}
  {$ENDIF}

  If ErrorCode = 0 Then
    Engine.Log.Write(logDebug, 'Sockets', 'Changed socket blocking mode for handle '+IntegerProperty.Stringify(Handle)+' -> ' +IntegerProperty.Stringify(N))
  Else
    Engine.Log.Write(logError, 'Sockets', 'Error changing blocking mode in socket '+IntegerProperty.Stringify(Handle));
End;

{ Socket }
Constructor NetSocket.Create(CustomHandle:Integer);
Var
  Arg:Integer;
Begin
  _Closed := False;
  _Handle := CustomHandle;
  _Blocking := True;

{$IFDEF OSX}
  Arg := 1;
  setsockopt(_Handle, SOL_SOCKET, SO_NOSIGPIPE, @Arg, Sizeof(Arg));
{$ENDIF}
End;

Constructor NetSocket.Create(Const Host:TERRAString; Port:Word);
Var
  IP:TERRAString;
  N:Integer;
  Addr4:SocketAddress4;
 // Addr6:SocketAddress6;
  TargetAddr:PSocketAddress;
  TargetSize:Integer;
Begin
  _Handle := -1;
  _Blocking := True;
  _Closed := True;
  _Error := False;

  //Resolve the server address
  IP := LookupHostAddress(Host);
  If (IP = '127.0.0.1') And (Host<>IP) And (Host<>'localhost') Then
  Begin
    Engine.Log.Write(logError, 'Sockets', 'Unable to resolve host address: '+Host);
    Exit;
  End;

  _NetType := GetSocketNetType(IP);

  Engine.Log.Write(logDebug, 'Sockets', 'Address found: '+IP);

  Engine.Log.Write(logDebug, 'Sockets', 'Creating a socket, port '+IntegerProperty.Stringify(Port));
  _Handle := socket(_NetType, SOCK_STREAM, IPPROTO_TCP); //Create a network socket

  If _Handle = SOCKET_ERROR Then  //Check for errors
  Begin
    _Error := True;
    Engine.Log.Write(logError, 'Sockets', 'Unable to open a socket, host='+Host+' port='+IntegerProperty.Stringify(Port));
    Exit;
  End;

  //Specify the IP
 (* If _NetType = PF_INET6 Then
  Begin
    //Zero fill
    FillChar(Addr6, SizeOf(Addr6), 0);

    //Set the address format
    Addr6.Family := _NetType;
    //Convert to network byte order (using htons) and set port
    Addr6.Port := htons(Port);

    inet_addr6_from_string(IP, Addr6);

    TargetAddr := @Addr6;
    TargetSize := SizeOf(Addr6);
  End Else   *)
  Begin
    //Zero fill
    FillChar(Addr4, SizeOf(Addr4), 0);

    //Set the address format
    Addr4.Family := _NetType;
    //Convert to network byte order (using htons) and set port
    Addr4.Port := htons(Port);

    Addr4.Address := inet_addr(PAnsiChar(IP));

    TargetAddr := @Addr4;
    TargetSize := SizeOf(Addr4);
  End;

  Engine.Log.Write(logDebug, 'Sockets', 'Connecting');
  N := TERRA_Sockets.connect(_Handle, TargetAddr, TargetSize);

  //Check for errors
  If N = SOCKET_ERROR Then
  Begin
    _Error := True;

    Engine.Log.Write(logError, 'Sockets', 'Unable to connect to host: '+Host);
    {$IFDEF WINDOWS}
    Engine.Log.Write(logError, 'Sockets', 'Sock error: '+IntegerProperty.Stringify(WSAGetLastError));
    {$ENDIF}
    Exit;
  End;

  Engine.Log.Write(logDebug, 'Sockets', 'Socket is ready');
  _Closed := False;
End;

Procedure NetSocket.Release;
Begin
  If (_Handle>0) Then
  Begin
    Engine.Log.Write(logDebug, 'Sockets', 'Shutting down socket '+IntegerProperty.Stringify(_Handle));
    Shutdown(_Handle, 2);

    {$IFNDEF LINUX}
    Engine.Log.Write(logDebug, 'Sockets', 'Closing down socket '+IntegerProperty.Stringify(_Handle));
    CloseSocket(_Handle);
    {$ENDIF}

    Engine.Log.Write(logDebug, 'Sockets', 'Destroyed NetSocket... ');

    _Handle := 0;
  End;
End;

Procedure NetSocket.SetTimeOut(Duration: Integer);
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

  If setsockopt(_Handle, SOL_SOCKET, SO_RCVTIMEO, @tv, sizeof(TV)) = 0 Then
  Begin
    Engine.Log.Write(logDebug, 'Sockets', 'Changed socket time out for handle '+IntegerProperty.Stringify(_Handle)+' -> '+IntegerProperty.Stringify(Duration));
  End Else
    Engine.Log.Write(logWarning, 'Sockets', 'Unable to change socket time out for handle '+IntegerProperty.Stringify(_Handle));
End;

Procedure NetSocket.SetDelay(Delay: Boolean);
Const
  TCP_NODELAY = 1;
Var
  N:Integer;
Begin
  If Delay Then
    N := 0
  Else
    N := 1;

  If setsockopt(_Handle, IPPROTO_TCP, TCP_NODELAY, @N, 4) = 0 Then
  Begin
    Engine.Log.Write(logDebug, 'Sockets', 'Change socket delay for handle '+IntegerProperty.Stringify(_Handle)+' -> ' +IntegerProperty.Stringify(N));
  End Else
    Engine.Log.Write(logWarning, 'Sockets', 'Unable to change socket delay for handle '+IntegerProperty.Stringify(_Handle));
End;

Procedure NetSocket.SetBufferSize(Size:Integer);
Begin
  If setsockopt(_Handle, SOL_Socket, SO_SNDBUF, @Size, 4) = 0 Then
  Begin
    Engine.Log.Write(logDebug, 'Sockets', 'Changed socket buffer size for handle '+IntegerProperty.Stringify(_Handle)+' -> '+IntegerProperty.Stringify(Size));
  End Else
    Engine.Log.Write(logWarning, 'Sockets', 'Unable to change socket buffer size for handle '+IntegerProperty.Stringify(_Handle));
End;

Procedure NetSocket.SetBlocking(Block:Boolean);
Begin
  _Blocking := Block;

  //Set the socket to non-blocking
  MakeNonBlocking(_Handle, Block);
End;


Function NetSocket.GetEOF:Boolean;
Begin
  Result := _Closed;
End;

Function NetSocket.Write(Data:Pointer; Size:Cardinal):Cardinal;
Var
  N:Integer;
  K:Integer;
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

    N := Send(_Handle, Data^, BlockSize, MSG_NOSIGNAL);

    If (N = SOCKET_ERROR) Or (N<0) Then
    Begin
      //K := SocketError();

      Self._Closed := True;
      
      _Error := True;
      Result := 0;
      Exit;
    End;

    Inc(PByte(Data), N);
    Dec(Size, N);
    Inc(Result, N);
  End;
End;

Function NetSocket.Read(Data:Pointer; Size:Cardinal):Cardinal;
Var
  N:Integer;
Begin
  {$IFDEF DEBUG_NET} Log(logDebug, 'Server', 'Begin sock.read from handle '+IntegerProperty.Stringify(_Handle));{$ENDIF}
  Result := 0;
  If (EOF) Or (Size<=0) Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'Bailout');{$ENDIF}
    Exit;
  End;

  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'recv() call from handle '+IntegerProperty.Stringify(_Handle));{$ENDIF}
  N := Recv(_Handle, Data^, Size, MSG_NOSIGNAL);
  {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'result was '+IntegerProperty.Stringify(N));{$ENDIF}

  If (N = SOCKET_ERROR) Or (N<0) Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'socket returned error');{$ENDIF}
    _Error := True;
    Result := 0;
    Exit;
  End;

  If (N=0) Then
  Begin
    {$IFDEF DEBUG_NET}Log(logDebug, 'Server', 'socket was closed');{$ENDIF}
    _Closed := True;
  End;

  If (N<0 )Then
    Result := 0
  Else
    Result := N;
End;

Procedure NetSocket.ReadLine(Var S:TERRAString);
Var
  C:TERRAChar;
Begin
  S :='';
  C := NullChar;
  While (Not Self.EOF) Do
  Begin
    ReadChar(C);

    If (C = NewLineChar) Then
    Begin
      Break;
    End Else
      StringAppendChar(S, C);
  End;
End;

Type
  WaitingSocket=Record
    Handle:Integer;
    Port:Integer;
  End;

Var
  WaitingList:Array Of WaitingSocket;
  WaitingCount:Integer = 0 ;

Function OpenIncomingStream(Port:Integer):NetSocket;
Var
  I,ID:Integer;
  Opv:Integer;
  ClientSock:Integer;
  ClientAddr:SocketAddress4;
  Addr:SocketAddress4;
  Handle:Integer;
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
    ID := Pred(WaitingCount);

    Handle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    WaitingList[ID].Port := Port;
    WaitingList[ID].Handle := Handle;

    Engine.Log.Write(logDebug, 'Sockets', 'Created listened socket with handle '+IntegerProperty.Stringify((Handle)));

    MakeNonBlocking(WaitingList[ID].Handle, False);

    Opv := 1;
    If (setsockOpt(WaitingList[ID].Handle, SOL_SOCKET, SO_REUSEADDR, @Opv, SizeOf(Opv)) = 0)Then
    Begin
      Engine.Log.Write(logDebug, 'Sockets', 'Reused socket address for handle '+IntegerProperty.Stringify((Handle)));
    End Else
      Engine.Log.Write(logWarning, 'Sockets', 'Unable to reuse socket address for handle '+IntegerProperty.Stringify((Handle)));

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.Family := PF_INET;
    Addr.Port := htons(Port);

    If Bind(WaitingList[ID].Handle, @Addr, SizeOf(Addr))<0 Then
    Begin
      Engine.RaiseError('Cannot bind NetSocket.');
      Exit;
    End;

    Engine.Log.Write(logDebug, 'Sockets', 'Listening for connections.');
    Listen(WaitingList[ID].Handle, 5);
  End;

  Opv := SizeOf(ClientAddr);

  ClientSock := Accept(WaitingList[ID].Handle, @ClientAddr, Opv);

  If ClientSock<>-1 Then
  Begin
    Engine.Log.Write(logDebug, 'Sockets', 'Accepted socket connection with handle '+IntegerProperty.Stringify(ClientSock));
    Result := NetSocket.Create(ClientSock);
    //Result._Address := ClientAddr.Address;
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

{$IFDEF WINDOWS}
Var
 Data:WSADATA;
 N:Integer;
{$ENDIF}

{$IFDEF LINUX}
Var
  sSet:Cardinal;
  sa:SigActionRec;
{$ENDIF}

Initialization
{$IFDEF WINDOWS}
  N := WSAStartup(SOCK_VER, Data);
  //Check for errors
  If N<>0 Then
    Engine.Log.Write(logError, 'Sockets', 'Unable to initialize Winsock session.');

{$ENDIF}
{$IFDEF LINUX}
//http://lists.freepascal.org/fpc-pascal/2013-May/038302.html
//http://www.freepascal.org/docs-html/rtl/baseunix/fpsigprocmask.html
{  sset := (1 Shl SIGPIPE);
  FpSigProcMask(SIG_BLOCK, @sset, Nil);}

{  FillChar(sa, SizeOf(Sa), 0);
  sa.sa_handler := __sighandler_t(Pointer(SIG_IGN));
  sa.sa_flags := 0;
  fpsigaction(SIGPIPE, @sa, Nil);}

{$ENDIF}
Finalization
  ReleaseSockets;
{$IFDEF WINDOWS}
  WSACleanup;
{$ENDIF}
End.

