program sample_server;

{$APPTYPE CONSOLE}

{ To write a TERRA server is simple, derive a new class from NetServer.
  This class should register handlers for each network command using AddHandler() during the constructor

  The following methods can be overrided:

  OnClientAdd() - Called each time a new client connects successfully. Check the struct ClientConnection to see what data is available
  OnClientRemove() - Called each time a client disconnects or is dropped.

  ValidateClient() - Used to autenthicate a client username/password. Return zero in case of sucess, or an error code in case of failure
}

Uses
  Ships_Opcodes,
  TERRA_Utils,
  TERRA_Sockets,
  TERRA_Object,
  TERRA_MultiThreadedServer,
  TERRA_NetServer,
  TERRA_Network;

Type
  MyServer = Class(NetMultithreadedServer)
    Constructor Create(Version,Port:Word);

    Procedure OnClientAdd(Client:ClientConnection); Override;    //Event: When a new Client enter the server
    Procedure OnClientRemove(Client:ClientConnection); Override; //Event: When a Client leaves the server

    // our custom message handler
    Procedure ResendMessage(Msg:NetMessage; Client:ClientConnection);
  End;

Var
  _Server:MyServer;

{ MyServer }
Procedure MyServer.ResendMessage(Msg:NetMessage; Client:ClientConnection);
Begin
  WriteLn('Msg: ', Msg.Opcode);
  Msg.Owner := 0;
  Self.BroadcastMessage(Msg, Client.ID); // just broadcast it to all players
End;

Constructor MyServer.Create(Version, Port: Word);
Begin
  Inherited Create(Version, Version, Port);

  Self.AddHandler(netPlayerDrop, ResendMessage);
  Self.AddHandler(netPlayerData, ResendMessage);
End;

Procedure MyServer.OnClientAdd(Client:ClientConnection);
Begin
  WriteLn('Player ',Client.ID,' joined the server');
End;

Procedure MyServer.OnClientRemove(Client:ClientConnection);
Begin
  WriteLn('Player ',Client.ID,' left the server');
End;

begin
  WriteLn('Starting server version ',GameVersion,' at port ',GamePort);
  _Server := MyServer.Create(GameVersion, GamePort);
  Repeat
    _Server.Update();
  Until False;
  ReleaseObject(_Server);
end.

