Unit TERRA_NetLogger;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_IO, TERRA_FileIO, TERRA_Network;

Const
  networkNewConnection      = 0;
  networkConnectionClosed   = 1;
  networkPacket             = 2;

Type
  NetworkLogger = Class
    Protected
      _Stream:FileStream;

      Procedure InitStream(Opcode:Byte);

    Public
      Class Function Instance:NetworkLogger;
      Procedure LogConnectionStart(ID:Integer; UserName,Password,DeviceID:AnsiString);
      Procedure LogConnectionEnd(ID:Integer);
      Procedure LogPacket(ID:Integer; Msg:PNetMessage);
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

Procedure NetworkLogger.LogConnectionStart(ID: Integer; UserName, Password, DeviceID:AnsiString);
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

Procedure NetworkLogger.LogPacket(ID: Integer; Msg: PNetMessage);
Begin
  Self.InitStream(networkPacket);
  _Stream.Write(@ID, 4);
  _Stream.Write(Msg, SizeOf(LNetMessage));
End;

procedure NetworkLogger.InitStream(Opcode:Byte);
begin
  If (_Stream=Nil) Then
    _Stream := FileStream.Create('packets.dat');

  _Stream.Write(@Opcode, 1);
end;

End.