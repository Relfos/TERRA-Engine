Unit Ships_Opcodes;

{
  Having a separate unit file for constants shared by client and server is a good idea
}

{$I terra.inc}
Interface

Const
  netPlayerData = 101;
  netPlayerDrop = 102;

  GameVersion = 1;
  GamePort = 1111;
  MaxClients = 50;
  
Implementation

End.
