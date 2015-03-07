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
 * TERRA_NetLogger
 * Implements a net logger for Client/Server
 ***********************************************************************************************************************
}
Unit TERRA_NetLogger;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_FileStream, TERRA_Network;

Const
  networkNewConnection      = 0;
  networkConnectionClosed   = 1;
  networkPacket             = 2;

Type
  NetworkLogger = Class(TERRAObject)
    Protected
      _Stream:FileStream;

      Procedure InitStream(Opcode:Byte);

    Public
      Class Function Instance:NetworkLogger;
      Procedure LogConnectionStart(ID:Integer; UserName,Password,DeviceID:TERRAString);
      Procedure LogConnectionEnd(ID:Integer);
      Procedure LogPacket(ID:Integer; Msg:NetMessage);
  End;


Implementation

Var
  _LoggerInstance:NetworkLogger = Nil;

{ NetworkLogger }
Class Function NetworkLogger.Instance: NetworkLogger;
Begin
  If (_LoggerInstance = Nil) Then
    _LoggerInstance := NetworkLogger.Create;

  Result := _LoggerInstance;
End;

Procedure NetworkLogger.LogConnectionStart(ID: Integer; UserName, Password, DeviceID:TERRAString);
Begin
  Self.InitStream(networkNewConnection);
  _Stream.Write(@ID, 4);
  _Stream.WriteString(Username);
  _Stream.WriteString(Password);
  _Stream.WriteString(DeviceID);
End;

Procedure NetworkLogger.LogConnectionEnd(ID: Integer);
Begin
  Self.InitStream(networkConnectionClosed);
  _Stream.Write(@ID, 4);
End;

Procedure NetworkLogger.LogPacket(ID: Integer; Msg: NetMessage);
Var
  Size:Integer;
Begin
  Size := Msg.Size;
  
  Self.InitStream(networkPacket);
  _Stream.Write(@ID, 4);
  _Stream.Write(@Size, 4);
  Msg.Copy(_Stream);
End;

procedure NetworkLogger.InitStream(Opcode:Byte);
begin
  If (_Stream=Nil) Then
    _Stream := FileStream.Create('packets.dat');

  _Stream.Write(@Opcode, 1);
end;

End.