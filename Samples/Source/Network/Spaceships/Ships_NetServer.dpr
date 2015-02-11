program sample_server;

{$APPTYPE CONSOLE}

{ To write a TERRA server is simple, derive a new class from NetServer.
  This class should register handlers for each network command using AddHandler() during the constructor

  The following methods can be overrided:

  OnClientAdd() - Called each time a new client connects successfully. Check the struct ClientInfo to see what data is available
  OnClientRemove() - Called each time a client disconnects or is dropped.

  ValidateClient() - Used to autenthicate a client username/password. Return zero in case of sucess, or an error code in case of failure
}

uses
  windows,
  SysUtils,
  Ships_Opcodes,
  TERRA_Sockets,
  TERRA_NetServer,
  TERRA_Network;

Type
  MyServer = Class(NetServer)
    Constructor Create(Version,Port:Word; MaxClients:Word);

    Procedure OnClientAdd(Client:ClientInfo); Override;    //Event: When a new Client enter the server
    Procedure OnClientRemove(Client:ClientInfo); Override; //Event: When a Client leaves the server

    // our custom message handler
    Procedure ResendMessage(Msg:NetMessage; Sock:Socket);
  End;

Var
  _Server:MyServer;

{ MyServer }
Procedure MyServer.ResendMessage(Msg:NetMessage; Sock:Socket);
Begin
  WriteLn('Msg: ', Msg.Opcode);
  Msg.Owner := 0;
  Self.BroadcastMessage(Msg); // just broadcast it to all players
End;

Constructor MyServer.Create(Version, Port, MaxClients: Word);
Begin
  Inherited Create(Version, Port, MaxClients);

  Self.AddHandler(netPlayerDrop, ResendMessage);
  Self.AddHandler(netPlayerData, ResendMessage);
End;

Procedure MyServer.OnClientAdd(Client:ClientInfo);
Begin
  WriteLn('Player ',Client.ID,' joined the server');
End;

Procedure MyServer.OnClientRemove(Client:ClientInfo);
Begin
  WriteLn('Player ',Client.ID,' left the server');
End;

begin
  WriteLn('Starting server version ',GameVersion,' at port ',GamePort);
  _Server := MyServer.Create(GameVersion, GamePort, MaxClients);
  Repeat
    _Server.Update;
    Sleep(1);
  Until False;
  _Server.Destroy();
end.

