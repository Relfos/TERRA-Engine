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
Uses TERRA_Utils, TERRA_OS, TERRA_Network, TERRA_NetServer, TERRA_Sockets, TERRA_Collections, TERRA_Mutex, TERRA_Threads
  {$IFDEF WINDOWS},Windows{$ENDIF};

Type
  ServerThread = Class (TERRAThread)
    Protected
      _Client:ClientConnection;

    Public
      Constructor Create(Client:ClientConnection);

      Procedure Release; Override;
      Procedure Execute; Override;

      Property Client:ClientConnection Read _Client;
  End;

  NetMultithreadedServer = Class(NetServer)
    Protected
      _StartTime:Cardinal;

      _Threads:Array Of ServerThread;
      _ThreadCount:Integer;
      _ThreadMutex:CriticalSection;

      Procedure DispatchNewPlayer(Sock:NetSocket);

      Procedure AllocThread(Client:ClientConnection);

      Procedure InitClientConnection(Client:ClientConnection); Override;

    Public
      // Creates a new server instance
      Constructor Create(MinVersion, MaxVersion, Port:Word);

      Procedure Release; Override;

      // Handles messages
      Procedure Update; Override;
  End;

Implementation
Uses TERRA_Log, TERRA_Object, TERRA_Engine;

Var
  _ServerInstance:NetMultithreadedServer;

{ ServerThread }
Constructor ServerThread.Create(Client: ClientConnection);
Begin
  Self._Client := Client;

  Inherited Create;
End;

Procedure ServerThread.Release();
Begin
  {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Thread '+IntToString(_PlayerID)+' terminated!');{$ENDIF}
  _Client := Nil;
End;

Procedure ServerThread.Execute;
Var
  Msg:NetMessage;
Begin
  {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Thread '+IntToString(_PlayerID)+' running!');{$ENDIF}

  _ServerInstance.OnClientAdd(Client);
  Client.Socket.SetBlocking(True);
//  Client.Socket.SetTimeOut(TCPJoinTimeOut);

  Repeat
    If (Client = Nil) Or (Client.Deleted) Then
      Break;

    Client.Update();
  Until (False);

   {$IFDEF DEBUGMODE}Log(logDebug, 'MT', '-------------->Player '+IntToString(_PlayerID)+' exited gracefully!');{$ENDIF}
   
  //_ServerInstance.RemoveClient(Client);
End;

{ NetMultithreadedServer }

// Creates a new server instance
Constructor NetMultithreadedServer.Create(MinVersion, MaxVersion, Port:Word);
Begin
  Inherited Create(MinVersion, MaxVersion, Port);

  _ServerInstance := Self;
  _StartTime := Application.GetTime();
  _WaitingCount := 0;

  _ThreadMutex := CriticalSection.Create();
End;

Procedure NetMultithreadedServer.AllocThread(Client:ClientConnection);
Var
  I:Integer;
  Target:ServerThread;
Begin
  Target := Nil;

  _ThreadMutex.Lock();
  {For I:=0 To Pred(_ThreadCount) Do
  If (_Threads[I]._Client = Nil) Then
  Begin
    Target := _Threads[I];
    Break;
  End;}

  If Target = Nil Then
  Begin
    Inc(_ThreadCount);
    SetLength(_Threads, _ThreadCount);

    Target := ServerThread.Create(Client);
    _Threads[Pred(_ThreadCount)] := Target;
  End;

  _ThreadMutex.Unlock();
End;

Procedure NetMultithreadedServer.DispatchNewPlayer(Sock:NetSocket);
Var
  Client, Target:ClientConnection;
  It:TERRAIterator;
  Hours,Minutes:Integer;
  ClientThread:TERRAThread;
Begin
  Target := Nil;

  // Search for client with associated socket
  It := Self.Clients.GetIterator();
  While It.HasNext() Do
  Begin
    Client := ClientConnection(It.Value);

    If (Client.Socket = Sock) Then
    Begin
      Target := Client;
      Break;
    End;
  End;
  ReleaseObject(It);

  If (Target = Nil) Then
    Exit;

  Client.Socket.SetBlocking(True);

  Hours := (Application.GetTime()-_StartTime) Div 1000;
  Minutes := (Hours Mod 3600) Div 60;
  Hours := Hours Div 3600;
  Engine.Log.Write(logDebug, 'Server', 'New player ID'+ IntegerProperty.Stringify(Target.ID)+' at '+ IntegerProperty.Stringify(Hours)+'h'+ IntegerProperty.Stringify(Minutes));

  AllocThread(Client);
End;

Procedure NetMultithreadedServer.Update;
Var
  I:Integer;
  Msg:NetMessage;
  Rm:NetMessage;
  ValidMsg:Boolean;
  Client:ClientConnection;
  Sock:NetSocket;
Begin
  UpdateIO;

  CheckForIncomingConnections();

  I := 0;
  While (I<_WaitingCount) Do
  Begin
    {$IFDEF DEBUG_NET}WriteLnLog(logDebug, 'Server', 'Processing waiting connection #'+IntToString(I));{$ENDIF}
    If (ReceivePacket(_WaitingConnections[I])) Then
    Begin
      DispatchNewPlayer(_WaitingConnections[I]);
      _WaitingConnections[I] := _WaitingConnections[Pred(_WaitingCount)];
      Dec(_WaitingCount);
    End Else
    If (_WaitingConnections[I].Closed) Then
    Begin
      _WaitingConnections[I] := _WaitingConnections[Pred(_WaitingCount)];
      Dec(_WaitingCount);
    End Else
      Inc(I);
  End;

  //Self.SendDroppedPackets();
End;

Procedure NetMultithreadedServer.InitClientConnection(Client: ClientConnection);
Begin
  Client.Socket.SetBlocking(True);
End;

Procedure NetMultithreadedServer.Release;
Begin
  Inherited;

  ReleaseObject(_ThreadMutex);
End;

End.
