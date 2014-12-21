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
 * TERRA_
 * Implements a multithreaded server
 ***********************************************************************************************************************
}
Unit TERRA_MultiThreadedServer;

{$I terra.inc}                   

{-$DEFINE DEBUGMODE}

Interface
Uses TERRA_Utils, TERRA_OS, TERRA_Network, TERRA_NetServer, TERRA_Sockets, TERRA_ThreadPool
  {$IFDEF WINDOWS},Windows{$ENDIF};

Type
  NetMultithreadedServer = Class(NetServer)
    Protected
      _Threads:Array Of Thread;
      _LastTime:Array Of Cardinal;
      _StartTime:Cardinal;

      _WaitingConnections:Array Of Socket;
      _WaitingCount:Integer;

      Procedure DispatchNewPlayer(Sock:Socket);
    Public
      // Creates a new server instance
      Constructor Create(Name:AnsiString; Version,Port:Word; MaxClients:Word);

      // Handles messages
      Procedure Update; Override;
  End;

Implementation
Uses TERRA_Log;

Var
  _ServerInstance:NetMultithreadedServer;

Type
  ServerThread = Class (Thread)
    Protected
      _PlayerID:Integer;
    Public
      Constructor Create(ID:Integer);
      Destructor Destroy; Override;
      Procedure Execute; Override;
  End;

{ ServerThread }
Constructor ServerThread.Create(ID: Integer);
Begin
  _PlayerID := ID;
  Inherited Create;
End;

Destructor ServerThread.Destroy;
Begin
  {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Thread '+IntToString(_PlayerID)+' terminated!');{$ENDIF}
  _ServerInstance._Threads[_PlayerID] := Nil;
End;

Procedure ServerThread.Execute;
Var
  Client:ClientInfo;
  Msg:LNetMessage;
Begin
  {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Thread '+IntToString(_PlayerID)+' running!');{$ENDIF}

  Client := _ServerInstance.GetClient(_PlayerID);
  _ServerInstance.OnClientAdd(Client);
  Client.Socket.SetBlocking(True);
//  Client.Socket.SetTimeOut(TCPJoinTimeOut);

  Repeat
    Client := _ServerInstance.GetClient(_PlayerID);
    _ServerInstance._LastTime[_PlayerID] := GetTime();

    If (Client = Nil) Or (Client.Socket = Nil) Then
    Begin
      {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Player '+IntToString(_PlayerID)+' exited gracefully!');{$ENDIF}
      Exit;
    End;

    If Not _ServerInstance.ReceivePacket(Client.Socket, @Msg) Then
    Begin
      {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Player '+IntToString(_PlayerID)+' timed out!');{$ENDIF}
      Break;
    End Else
      Inc(Client.Frames);

  Until (False);
  _ServerInstance.RemoveClient(Client);
End;

{ NetMultithreadedServer }
// Creates a new server instance
Constructor NetMultithreadedServer.Create(Name:AnsiString; Version,Port:Word; MaxClients:Word);
Begin
  Inherited Create(Name, Version, Port, MaxClients);
  SetLength(_Threads, Succ(_ClientCount));
  SetLength(_LastTime, Succ(_ClientCount));
  _ServerInstance := Self;
  _StartTime := GetTime();
  _WaitingCount := 0;
End;

Procedure NetMultithreadedServer.DispatchNewPlayer(Sock: Socket);
Var
  PlayerID:Integer;
  Client:ClientInfo;
  I:Integer;
  Hours,Minutes:Integer;
Begin
  PlayerID := -1;
  For I:=1 To _ClientCount Do // Search dead connections, by pinging Clients
  Begin
    Client := GetClient(I);
    If (Not Assigned(Client)) Then
      Continue;

    If (Client.Socket = Sock) Then
    Begin
      PlayerID := I;
      Break;
    End;
  End;

  If (PlayerID<0) Then
    Exit;

  Hours := (GetTime()-_StartTime) Div 1000;
  Minutes := (Hours Mod 3600) Div 60;
  Hours := Hours Div 3600;
  WriteLn('New player ID'+IntToString(PlayerID)+' at '+IntToString(Hours)+'h'+IntToString(Minutes));

  _LastTime[PlayerID] := GetTime();
  If _Threads[PlayerID] = Nil Then
    _Threads[PlayerID] := ServerThread.Create(PlayerID);
End;

Procedure NetMultithreadedServer.Update;
Var
  I:Integer;
  Msg:PNetMessage;
  Rm:LNetMessage;
  ValidMsg:Boolean;
  Client:ClientInfo;
  Sock:Socket;
Begin
  UpdateIO;
  {$IFDEF WINDOWS}
  Sleep(20);
  {$ENDIF}

  Sock := OpenIncomingStream(_Port);
  If Assigned(Sock) Then
  Begin
    Sock.SetBlocking(False);
    Sock.SetDelay(False);
    Inc(_WaitingCount);
    SetLength(_WaitingConnections, _WaitingCount);
    _WaitingConnections[Pred(_WaitingCount)] := Sock;
  End;

  I := 0;
  While (I<_WaitingCount) Do
  Begin
    {$IFDEF NETDEBUG}WriteLn('Processing waiting connection #',I);{$ENDIF}
    If (ReceivePacket(_WaitingConnections[I], @RM)) Then
    Begin
      DispatchNewPlayer(_WaitingConnections[I]);
      _WaitingConnections[I] := _WaitingConnections[Pred(_WaitingCount)];
      Dec(_WaitingCount);
    End Else
      Inc(I);
  End;

  For I:=1 To _ClientCount Do
  If (GetTime()-_LastTime[I]>5000) Then
  Begin
    Client := Self.GetClient(I);
    If (Assigned(Client)) And (Client.Frames<=0) Then
      Self.RemoveClient(Client);
  End;

  Self.SendDroppedPackets();
End;

End.
