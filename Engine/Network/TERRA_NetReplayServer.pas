sds{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_FileStream, TERRA_Network, TERRA_NetLogger, TERRA_NetServer;

Type
  NetworkReplayServer = Class(NetServer)
    Protected
      _Stream:FileStream;

      Procedure OnNewClient();
      Procedure OnConnectionClose();
      Procedure OnPacket();

    Public
      // Creates a new server instance
      Constructor Create(Version,Port:Word; MaxClients:Word);

      Function SendMessage(Msg:NetMessage; Client:ClientConnection;  Owner:Cardinal; AutoRelease:Boolean = False):Boolean; Override;

      // Handles messages
      Procedure Update; Override;
  End;


Implementation
Uses TERRA_OS, TERRA_Log, TERRA_Threads, TERRA_Mutex, TERRA_Sockets;

{ NetworkReplayServer }
Constructor NetworkReplayServer.Create(Version, Port, MaxClients: Word);
Var
  I:Integer;
Begin
  _LocalId := 0;  //Servers always have a localID of zero
  NetworkManager.Instance.AddObject(Self);

  _Port := Port;
  _WaitingCount := 0;
  _RefreshTime := 0;
  _Version := Version;

  For I:=0 To 255 Do
    _OpcodeList[I] := Nil;
    
{  _OpcodeList[nmClientJoin] := OnJoinMessage; FIXME
  _OpcodeList[nmClientDrop] := OnDropMessage;}

  _PacketMutex := CriticalSection.Create('');

  _Clients := Nil; //FIXME

  _Stream := FileStream.Open('packets.dat');
End;

Procedure NetworkReplayServer.OnConnectionClose;
Var
  N:Cardinal;
  Client:ClientConnection;
Begin
  _Stream.ReadCardinal(N);
  Client := Self.GetClientByID(N);
  Self.RemoveClient(Client);
End;

Procedure NetworkReplayServer.OnNewClient;
Var
  N:Integer;
  Client:ClientConnection;
  Username, Password, DeviceID, ErrorLog:TERRAString;
Begin
  _Stream.Read(@N, 4);
  _Stream.ReadString(UserName);
  _Stream.ReadString(Password);
  _Stream.ReadString(DeviceID);

  ValidateClient(UserName,Password, DeviceID, ErrorLog);

  Client := ClientConnection.Create;
  Client.Address := _Sender; // Store the new Client IP
  Client.Ping := 0;          // Reset Client ping
  Client.GUID := N;
  Client.Time := GetTime;
  Client.UserName := UserName;
//  Client._ID := N; FIXME
  Client.Password := Password;
  Client.DeviceID := DeviceID;
//  Client.Socket := Socket(N); FIXME
  Client.Frames := 0;
  Clients.Add(Client);

  OnClientAdd(Client); //Calls the AddClient event
End;

Procedure NetworkReplayServer.OnPacket;
Var
  N:Cardinal;
  Client:ClientConnection;
  Msg:NetMessage;
  ValidMsg:Boolean;
  Size:Integer;
Begin
  _Stream.ReadCardinal(N);
  _Stream.ReadInteger(Size);
  Msg := NetMessage.Create(0);
  _Stream.Copy(Msg, _Stream.Position, Size);
  Msg.Seek(0);

  If (Msg.Opcode<10) Then
    Exit;

  {If (Msg.Opcode=108) Then
    IntToString(2);}

  Client := Self.GetClientByID(N);

  //WriteLn('Rec: ',Msg.Opcode);

  If ValidMsg Then
  Begin
    {$IFDEF DEBUG_NET}WriteLn('Invoking opcode ',Msg.Opcode);{$ENDIF}
    If Assigned(_OpcodeList[Msg.Opcode]) Then
      _OpcodeList[Msg.Opcode](Msg, Client) // Call message handler
  End Else
    Log(logWarning,'Network', Self.ClassName+'.Update: Invalid opcode ['+IntToString(Msg.Opcode)+']');

  {$IFDEF DEBUG_NET}WriteLn('Opcode ',Msg.Opcode,' processed');{$ENDIF}

End;

Function NetworkReplayServer.SendMessage(Msg:NetMessage; Client:ClientConnection;  Owner:Cardinal; AutoRelease:Boolean):Boolean;
Begin
  Result := True;
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
  {$IFDEF DEBUG_NET}WriteLn('Session finished!');{$ENDIF}
  End;
End;

End.
