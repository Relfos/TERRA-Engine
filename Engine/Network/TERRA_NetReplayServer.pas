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
 * Implements a replay server to replay saved network data
 ***********************************************************************************************************************
}
Unit TERRA_NetReplayServer;

{$I terra.inc}
{$DEFINE NETDEBUG}

Interface
Uses TERRA_Utils, TERRA_IO, TERRA_FileIO, TERRA_Network, TERRA_NetLogger, TERRA_NetServer;

Type
  NetworkReplayServer = Class(NetServer)
    Protected
      _Stream:FileStream;

      Procedure OnNewClient();
      Procedure OnConnectionClose();
      Procedure OnPacket();

    Public
      // Creates a new server instance
      Constructor Create(Name:AnsiString; Version,Port:Word; MaxClients:Word);

      Procedure SendMessage(Msg:PNetMessage; ClientID:Word); Override;

      // Handles messages
      Procedure Update; Override;
  End;


Implementation
Uses TERRA_OS, TERRA_Log, TERRA_ThreadPool, TERRA_Sockets;

{ NetworkReplayServer }
Constructor NetworkReplayServer.Create(Name:AnsiString; Version, Port, MaxClients: Word);
Var
  I:Integer;
Begin
  _LocalId := 0;  //Servers always have a localID of zero
  _NetObject := Self;
  _Name := Name;
  _Port := Port;
  _WaitingCount := 0;
  _ClientCount := MaxClients;
  _RefreshTime := 0;
  _Version := Version;

  For I:=0 To 255 Do
    _OpcodeList[I] := InvalidMessage;
  _OpcodeList[nmIgnore] := IgnoreMessage;
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

  _Stream := FileStream.Open('packets.dat');
End;

Procedure NetworkReplayServer.OnConnectionClose;
Var
  N:Integer;
  Client:ClientInfo;
Begin
  _Stream.Read(@N, 4);
  Client := Self.GetClient(N);
  Self.RemoveClient(Client);
End;

Procedure NetworkReplayServer.OnNewClient;
Var
  N:Integer;
  Client:ClientInfo;
  Username, Password, DeviceID, ErrorLog:AnsiString;
Begin
  _Stream.Read(@N, 4);
  _Stream.ReadString(UserName);
  _Stream.ReadString(Password);
  _Stream.ReadString(DeviceID);

  ValidateClient(UserName,Password, DeviceID, ErrorLog);

  Client := ClientInfo.Create;
  Client.Address := _Sender; // Store the new Client IP
  Client.Ping := 0;          // Reset Client ping
  Client.GUID := N;
  Client.Time := GetTime;
  Client.UserName := UserName;
  Client.ID := N;
  Client.UserData := Nil;
  Client.Password := Password;
  Client.DeviceID := DeviceID;
  Client.Socket := Socket(N);
  Client.Frames := 0;
  _ClientList[N] := Client;

  OnClientAdd(Client); //Calls the AddClient event
End;

Procedure NetworkReplayServer.OnPacket;
Var
  N:Integer;
  Client:ClientInfo;
  Msg:LNetMessage;
  ValidMsg:Boolean;
Begin
  _Stream.Read(@N, 4);
  _Stream.Read(@Msg, SizeOf(LNetMessage));

  If (Msg.Opcode<10) Then
    Exit;

  If (Msg.Opcode=108) Then
    IntToString(2);

  Client := Self.GetClient(N);

  {$IFDEF NETDEBUG}WriteLn('Validating message');{$ENDIF}
  ValidMsg := ValidateMessage(@Msg);
  //WriteLn('Rec: ',Msg.Opcode);

  If ValidMsg Then
  Begin
    {$IFDEF NETDEBUG}WriteLn('Invoking opcode ',Msg.Opcode);{$ENDIF}
    If Assigned(_OpcodeList[Msg.Opcode]) Then
      _OpcodeList[Msg.Opcode](@Msg, Client.Socket) // Call message handler
  End Else
    Log(logWarning,'Network','Network.'+_Name+'.Update: Invalid opcode ['+IntToString(Msg.Opcode)+']');

  {$IFDEF NETDEBUG}WriteLn('Opcode ',Msg.Opcode,' processed');{$ENDIF}

End;

Procedure NetworkReplayServer.SendMessage(Msg:PNetMessage; ClientID:Word);
Begin
  // do nothing
End;

Procedure NetworkReplayServer.Update;
Var
  Opcode:Byte;
Begin
  If Not _Stream.EOF Then
  Begin
    _Stream.Read(@Opcode, 1);
    Case Opcode Of
    networkNewConnection:
      OnNewClient();

    networkConnectionClosed:
      OnConnectionClose();

    networkPacket:
      OnPacket();
    End;

    If (_Stream.EOF) Then
  {$IFDEF NETDEBUG}WriteLn('Session finished!');{$ENDIF}
  End;
End;

End.
