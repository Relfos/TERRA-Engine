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
 * TERRA_Matchmaking
 * Implements a matchmaking system for multiplayer games
 ***********************************************************************************************************************
}
Unit TERRA_MatchMaking;

{$I terra.inc}

{$DEFINE DEBUGMODE}

Interface
Uses TERRA_Utils, TERRA_Sockets, TERRA_Network, TERRA_Stream,
  TERRA_OS, TERRA_Application;

Const
  netMatchCreate  = 250;
  netMatchDelete  = 249;
  netMatchList    = 248;
  netMatchChat    = 247;
  netMatchJoin    = 246;
  netMatchLeave   = 245;
  netMatchStart   = 244;
  netMatchData    = 243;
  netMatchPlayerData= 242;
  netMatchError     = 241;
  netMatchAddBot    = 240;

  matchErrorEndOfMatch        = 200;
  matchErrorConnectionFailed  = 201;
  matchErrorRoomFull          = 202;
  matchErrorCannotHost        = 203;

  ConnectionDelay = 100;
  ConnectionDuration = 5 * 1000;

  BaseBotID = 9000;

Type
  PMatchUser = ^MatchUser;
	MatchUser = Record
    Public
      ID:Word;
  		Address:TERRAString;
  		Data:TERRAString;
      Sock:Socket;
      ConnectionFailed:Boolean;
      IsBot:Boolean;
      CanHost:Boolean;
	End;

	MatchRoom = Class(TERRAObject)
		Protected
			_MatchID:Word;
			_UserCount:Byte;
			_Users:Array Of MatchUser;
      _LocalIndex:Integer;
			_Data:TERRAString;
      _Visible:Boolean;
      _PlayerLimit:Byte;
      _GamePort:Word;

      Function HasUser(ID:Word):Boolean;
      Function IsOwner(ID:Word):Boolean;

      Procedure AddUser(ID:Word; Data, IP:TERRAString; LocalID:Integer; CanHost:Boolean);
      Procedure AddBot(ID:Word; Data:TERRAString);
      Procedure RemoveBot(ID:Word);

      // override this
 			Procedure OnPlayerChat(PlayerID:Word; Msg:TERRAString); Virtual;
			Procedure OnPlayerJoin(PlayerID:Word; Data:TERRAString); Virtual;
			Procedure OnPlayerLeave(PlayerID:Word); Virtual;
			Procedure OnPlayerDataChanged(PlayerID:Word; Data:TERRAString); Virtual;

			Procedure OnMatchDataChanged(Data:TERRAString); Virtual;
      Procedure OnMatchCancelled(); Virtual;
      Procedure OnMatchEnter(); Virtual;
      Procedure OnMatchStart(); Virtual;
      Procedure OnMatchConnecting(); Virtual;
      Procedure OnMatchEnd(ErrorCode:Integer); Virtual;

		Public
      Function SendMessage(Msg:PNetMessage; PlayerID:Word):Boolean; Overload;
      Procedure SendMessage(Opcode:Byte; Src:Stream; PlayerID:Word); Overload;

      Procedure BroadcastMessage(Msg:PNetMessage); Overload;
      Procedure BroadcastMessage(Opcode:Byte; Src:Stream); Overload;

      Function GetMessage(Msg:PNetMessage; PlayerID:Word):Boolean;

      Function GetUserByIndex(Index:Integer):PMatchUser;
      Function GetUserByID(ID:Word):PMatchUser;
      Function GetOwner():PMatchUser;
      Function GetLocalID:Word;

			Property MatchID:Word Read _MatchID;
			Property UserCount:Byte Read _UserCount;
      Property PlayerLimit:Byte Read _PlayerLimit;
      Property LocalIndex:Integer Read _LocalIndex;
      Property LocalID:Word Read GetLocalID;

      Property Data:TERRAString Read _Data;
	End;

	MatchClass = Class(TERRAObject) Of MatchRoom;

	MatchmakingServer = Class(TERRAObject)(NetServer)
		Protected
			_Matches:Array Of MatchRoom;
			_MatchCount:Integer;
      _CurrentID:Integer;

      Function GetUserMatch(ID:Word):Integer;
      Procedure SendPlayerList(MatchID, PlayerID:Word);
      Procedure BroadcastMatchList(N:Integer);

		Public
      Constructor Create(Name:TERRAString; Version,Port:Word; MaxClients:Word; Mode:Integer);

      Procedure OnMatchCreate(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchDelete(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchList(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchStart(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchJoin(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchLeave(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchChat(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchData(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchAddBot(Msg:PNetMessage; Sock:Socket);
      Procedure OnPlayerData(Msg:PNetMessage; Sock:Socket);
      Procedure OnBotData(Msg:PNetMessage; Sock:Socket);

      Procedure OnClientAdd(Client:PClientInfo);Override;    //Event: When a new Client enter the server
      Procedure OnClientRemove(Client:PClientInfo);Override; //Event: When a Client leaves the server

			Property MatchCount:Integer Read _MatchCount;
	End;

  MatchMakingBrowser = Class(TERRAObject)
    Protected
			_Matches:Array Of MatchRoom;
      _MatchCount:Integer;

      Procedure OnMatchAdd(ID:Word); Virtual;
      Procedure OnMatchChange(ID:Word); Virtual;
      Procedure OnMatchDelete(ID:Word); Virtual;

      Procedure OnServerError(ErrorCode:Integer); Virtual;

    Public
  		Function GetMatchByIndex(Index:Integer):MatchRoom;
			Function GetMatchByID(ID:Integer):MatchRoom;
			Property MatchCount:Integer Read _MatchCount;

  End;

	MatchmakingClient = Class(TERRAObject)(NetClient)
		Protected
      _CurrentMatch:MatchRoom;
      _MatchClass:MatchClass;
      _Browser:MatchMakingBrowser;
      _UserData:TERRAString;

      _GamePort:Cardinal;
      _Connecting:Boolean;
      _LastConnect:Cardinal;
      _ConnectionStart:Cardinal;
      _NewMatch:Boolean;
      _NewMatchTime:Cardinal;

      Procedure ConnectionStart; Override;
      Procedure ConnectionEnd(ErrorCode:Integer);  Override;

      Procedure Update; Override;

		Public
      Constructor Create(GamePort:Cardinal; UserData:TERRAString; Browser:MatchMakingBrowser; MyMatchClass:MatchClass; RouterIP:TERRAString = '192.168.1.1');

      Procedure RefreshMatchList();

      Procedure CreateMatch(MatchData, UserData:TERRAString; PlayerLimit:Byte);
      Procedure JoinMatch(MatchID:Word; Data:TERRAString);
      Procedure LeaveMatch();
      Procedure StartMatch();
      Procedure EndMatch();
      Function AddBotToMatch(Data:TERRAString):Word;
      Procedure RemoveBotFromMatch(BotID:Word);

      Function GetPlayerData(PlayerID:Word):TERRAString;
      Function GetMatchData():TERRAString;

			Procedure SendChat(Msg:TERRAString);
			Procedure SetPlayerData(Data:TERRAString);
			Procedure SetBotData(BotID:Word; Data:TERRAString);
			Procedure SetMatchData(Data:TERRAString);

      Procedure OnMatchDelete(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchList(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchChat(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchJoin(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchLeave(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchStart(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchData(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchCreate(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchError(Msg:PNetMessage; Sock:Socket);
      Procedure OnMatchAddBot(Msg:PNetMessage; Sock:Socket);
      Procedure OnPlayerDataChange(Msg:PNetMessage; Sock:Socket);

      Property CurrentMatch:MatchRoom Read _CurrentMatch;
      Property UserData:TERRAString Read _UserData Write SetPlayerData;
	End;

Implementation
//Uses TERRA_UPNP;

Function EncodeInt(S:TERRAString):Cardinal;
Var
  C:Array[0..3] Of TERRAChar;
Begin
  C[0] := S[1];
  C[1] := S[2];
  C[2] := S[3];
  C[3] := S[4];
  Move(C[0], Result, 4);
End;

Function DecodeInt(S:Cardinal):TERRAString;
Var
  C:Array[0..3] Of TERRAChar;
Begin
  Move(S, C[0], 4);
  SetLength(Result, 4);
  Result[1] := C[0];
  Result[2] := C[1];
  Result[3] := C[2];
  Result[4] := C[3];
End;

Function TestTCPSocket(Host:TERRAString; GamePort:Word):Boolean;
Var
  S:TERRAString;
  T, Ack:Cardinal;
  Sock2:Socket;
Begin
  Sock2 := Socket.Create(Host, GamePort);
  If Sock2.Closed Then
  Begin
    Result := False;
    Exit;
  End;
  Ack := EncodeInt('HELO');
  Sock2.Write(Ack, 4);
  Sock2.Blocking := False;
  T := GetTime + 1000;
  Repeat
    If Sock2.Read(Ack, 4)>=4 Then
      Break;
  Until (GetTime()>T);
  Sock2.Release;

  S := DecodeInt(Ack);
  Result := (S='HOST');
End;

{ MatchmakingServer }
Constructor MatchmakingServer.Create(Name:TERRAString; Version,Port,MaxClients:Word; Mode:Integer);
Begin
  Inherited;
  _MatchCount := 0;
  _CurrentID := 1;

  Self._OpcodeList[netMatchCreate] := OnMatchCreate;
  Self._OpcodeList[netMatchDelete] := OnMatchDelete;
  Self._OpcodeList[netMatchList] := OnMatchList;
  Self._OpcodeList[netMatchChat] := OnMatchChat;
  Self._OpcodeList[netMatchJoin] := OnMatchJoin;
  Self._OpcodeList[netMatchLeave] := OnMatchLeave;
  Self._OpcodeList[netMatchStart] := OnMatchStart;
  Self._OpcodeList[netMatchData] := OnMatchData;
  Self._OpcodeList[netMatchPlayerData] := OnPlayerData;
  Self._OpcodeList[netMatchAddBot] := OnMatchAddBot;
End;

Function MatchmakingServer.GetUserMatch(ID: Word): Integer;
Var
  I,J:Integer;
Begin
  Result := -1;
  For I:=0 To Pred(_MatchCount) Do
    For J:=0 To Pred(_Matches[I]._UserCount) Do
    If (_Matches[I]._Users[J].ID = ID) Then
    Begin
      Result := I;
      Exit;
    End;
End;

Procedure MatchmakingServer.OnPlayerData(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  Src:MemoryStream;
  Data:TERRAString;
  PlayerID:Word;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(PlayerID, 2);
  Src.ReadString(Data);
  Src.Release;

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (_Matches[N]._Users[I].ID = PlayerID) Then
  Begin
    _Matches[N]._Users[I].Data := Data;
    Break;
  End;

  {$IFDEF DEBUGMODE}
  WriteLn('Player data changed: ', Data);
  {$ENDIF}

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);
End;

Procedure MatchmakingServer.OnBotData(Msg: PNetMessage; Sock: Socket);
Var
  I, N, Count:Integer;
  Src:MemoryStream;
  Data:TERRAString;
  BotID:Word;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(BotID, 2);
  Src.ReadString(Data);
  Src.Release;

  Count := 0;
  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (_Matches[N]._Users[I].IsBot) Then
  Begin
    If (Count = BotID) Then
    Begin
      _Matches[N]._Users[I].Data := Data;
      Break;
    End Else
      Inc(Count);
  End;

  {$IFDEF DEBUGMODE}
  WriteLn('Bot data changed: ', Data);
  {$ENDIF}

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);
End;

Procedure MatchmakingServer.OnMatchData(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  Data:TERRAString;
  Src:MemoryStream;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.ReadString(Data);
  Src.Release;
  _Matches[N]._Data := Data;

  {$IFDEF DEBUGMODE}
  WriteLn('Match data changed: ', Data);
  {$ENDIF}

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);
End;

Procedure MatchmakingServer.OnMatchAddBot(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  Data:TERRAString;
  MatchID, BotID:Word;
  Src:MemoryStream;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(BotID, 2);
  Src.Read(MatchID, 2);
  Src.ReadString(Data);
  Src.Release;

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (_Matches[N]._Users[I].ID = BotID) Then
    Exit;

  _Matches[N].AddBot(BotID, Data);

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);

  {$IFDEF DEBUGMODE}
  WriteLn('Added bot to match ', N);
  {$ENDIF}
End;

Procedure MatchmakingServer.OnMatchJoin(Msg: PNetMessage; Sock: Socket);
Var
  PlayerID, MatchID, Error:Word;
  I, N:Integer;
  UserData, IP:TERRAString;
  Src, Dst:MemoryStream;
  CanHost:Boolean;
Begin
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(PlayerID, 2);
  Src.Read(MatchID, 2);
  Src.ReadString(UserData);
  Src.Release;

  N := -1;
  For I:=0 To Pred(_MatchCount) Do
  If (_Matches[I]._MatchID = MatchID) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then // match was not found
    Exit;

  If (_Matches[N]._UserCount>=_Matches[N]._PlayerLimit) Then
  Begin
    MatchID := N;
    Error := matchErrorRoomFull;
    Dst := MemoryStream.Create(256);
    Dst.Write(MatchID, 2);
    Dst.Write(Error, 2);
    Self.SendMessage(Msg.Owner, netMatchError, Dst);
    Dst.Release;

    {$IFDEF DEBUGMODE}
    WriteLn('Room ', N,' is full!');
    {$ENDIF}
    Exit;
  End;

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (_Matches[N]._Users[I].ID = Msg.Owner) Then
    Exit; // player already in

  {$IFDEF DEBUGMODE}
  WriteLn('A player joined room ', N);
  {$ENDIF}

  Dst := MemoryStream.Create(256);
  Dst.Write(_Matches[N]._MatchID, 2);
  SendMessage(Msg.Owner, netMatchCreate, Dst);
  Dst.Release;

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);

  IP := GetIP(Self.GetClientAddress(Msg.Owner));
  CanHost := TestTCPSocket(IP, _Matches[N]._GamePort);
  _Matches[N].AddUser(Msg.Owner, UserData, IP, Self._LocalID, CanHost);

  Dst := MemoryStream.Create(256);
  Dst.WriteString(_Matches[N]._Data);
  Self.SendMessage(Msg.Owner, netMatchData, Dst);
  Dst.Release;

  Self.SendPlayerList(N, Msg.Owner);

  BroadcastMatchList(N);
End;

Procedure MatchmakingServer.OnMatchChat(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  {$IFDEF DEBUGMODE}
  Src:MemoryStream;
  S:TERRAString;
  {$ENDIF}
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  {$IFDEF DEBUGMODE}
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.ReadString(S);
  Src.Release;
  WriteLn('Chat: ', S);
  {$ENDIF}

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(Msg, _Matches[N]._Users[I].ID);
End;

Procedure MatchmakingServer.OnMatchLeave(Msg: PNetMessage; Sock: Socket);
Var
  Src:MemoryStream;
  I, N:Integer;
  PlayerID:Word;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  {$IFDEF DEBUGMODE}
  WriteLn('Player ', Msg.Owner, ' is leaving the match');
  {$ENDIF}

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(PlayerID, 2);
  Src.Release;

  If (_Matches[N].IsOwner(PlayerID)) Then
  Begin
    OnMatchDelete(Msg, Sock);
  End Else
  Begin
    For I:=0 To Pred(_Matches[N]._UserCount) Do
    If (_Matches[N]._Users[I].ID<>PlayerID) And (Not _Matches[N]._Users[I].IsBot) Then
      Self.SendMessage(Msg, _Matches[N]._Users[I].ID);

    I:=0;
    While (I<_Matches[N]._UserCount) Do
    If (_Matches[N]._Users[I].ID = PlayerID) Then
    Begin
      _Matches[N]._Users[I] := _Matches[N]._Users[Pred(_Matches[N]._UserCount)];
      Dec(_Matches[N]._UserCount);
    End Else
      Inc(I);
  End;
End;

Procedure MatchmakingServer.OnClientAdd(Client: PClientInfo);
Begin
  Inherited;
  {$IFDEF DEBUGMODE}
  WriteLn('Client ', Client.Username,' entered the server');
  {$ENDIF}
End;

procedure MatchmakingServer.OnClientRemove(Client: PClientInfo);
Var
  I:Integer;
  ID:Word;
  Dst:MemoryStream;
  Msg:LNetMessage;
Begin
  Inherited;

  For I:=0 To Pred(Self._MatchCount) Do
  If (_Matches[I].HasUser(Client.ID)) Then
  Begin
    ID := Client.ID;
    Dst := MemoryStream.Create(256, @Msg.Data[0]);
    Dst.Write(ID, 2);
    Dst.Release;
    Msg.Owner := Client.ID;
    Msg.Size := 2;
    Msg.Opcode := netMatchLeave;
    Self.OnMatchLeave(@Msg, Nil);
  End;

  {$IFDEF DEBUGMODE}
  WriteLn('Client ', Client.Username,' left the server');
  {$ENDIF}
End;

Procedure MatchmakingServer.BroadcastMatchList(N:Integer);
Var
  I:Integer;
  Dst:MemoryStream;
Begin
  Dst := MemoryStream.Create(256);
  Dst.Write(_Matches[N]._MatchID, 2);
  Dst.Write(_Matches[N]._PlayerLimit, 1);
  Dst.Write(_Matches[N]._UserCount, 1);
  For I:=0 To Pred(_Matches[N]._UserCount) Do
  Begin
    Dst.Write(_Matches[N]._Users[I].ID, 2);
    Dst.WriteString(_Matches[N]._Users[I].Data);
  End;
  Dst.WriteString(_Matches[N]._Data);
  N := Dst.Position;

  For I:=1 To _ClientCount Do
  If Assigned(Self.GetClient(I)) Then
  Begin
    Dst.Seek(N);
    SendMessage(I, netMatchList, Dst);
  End;
  Dst.Release;
End;

Procedure MatchmakingServer.OnMatchCreate(Msg: PNetMessage; Sock: Socket);
Var
  Src, Dst:MemoryStream;
  MatchData, UserData, IP, S:TERRAString;
  I, N:Integer;
  PlayerLimit:Byte;
  GamePort:Word;

  Procedure ReturnError(Error:Word);
  Var
    MatchID:Word;
  Begin
    MatchID := 0;
    Dst := MemoryStream.Create(256);
    Dst.Write(MatchID, 2);
    Dst.Write(Error, 2);
    Self.SendMessage(Msg.Owner, netMatchError, Dst);
    Dst.Release;
  End;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N>=0) Then
  Begin
    Self.OnMatchDelete(Msg, Sock);
  End;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.ReadString(MatchData);
  Src.ReadString(UserData);
  Src.Read(PlayerLimit, 1);
  Src.Read(GamePort, 2);
  Src.Release;

  IP := GetIP(Self.GetClientAddress(Msg.Owner));
  If Not TestTCPSocket(IP, GamePort) Then
  Begin
    ReturnError(matchErrorCannotHost);
    Exit;
  End;

  {$IFDEF DEBUGMODE}
  WriteLn('Creating a new match ', MatchData);
  WriteLn('User data: ', UserData);
  WriteLn('Max users: ', PlayerLimit);
  {$ENDIF}

  N := _MatchCount;
  Inc(_MatchCount);
  SetLength(_Matches, _MatchCount);
  _Matches[N] := MatchRoom.Create();
  _Matches[N]._MatchID := _CurrentID;
  Inc(_CurrentID);

  _Matches[N]._Data := MatchData;
  _Matches[N]._PlayerLimit := PlayerLimit;
  _Matches[N]._GamePort := GamePort;

  _Matches[N]._UserCount := 0;
  _Matches[N].AddUser(Msg.Owner, UserData, IP, 0, True);

  Dst := MemoryStream.Create(256);
  Dst.Write(_Matches[N]._MatchID, 2);
  SendMessage(Msg.Owner, netMatchCreate, Dst);
  Dst.Release;

  Self.SendPlayerList(Pred(_MatchCount), Msg.Owner);

  Dst := MemoryStream.Create(256);
  Dst.WriteString(_Matches[N]._Data);
  Self.SendMessage(Msg.Owner, netMatchData, Dst);
  Dst.Release;

  BroadcastMatchList(N);
End;

Procedure MatchmakingServer.OnMatchDelete(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  Room:Word;
  Dst:MemoryStream;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  {$IFDEF DEBUGMODE}
  WriteLn('Deleting match ', N);
  {$ENDIF}

  Room := _Matches[N]._MatchID;
  Dst := MemoryStream.Create(256);
  Dst.Write(Room, 2);

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  If (Not _Matches[N]._Users[I].IsBot) Then
    Self.SendMessage(_Matches[N]._Users[I].ID, netMatchDelete, Dst, Msg.Owner);

  For I:=1 To _ClientCount Do
  If (Self.GetClient(I)<>Nil) And (Not _Matches[N].HasUser(I)) Then
    Self.SendMessage(I, netMatchDelete, Dst, Msg.Owner);

  Dst.Release;

  _Matches[N].Release;
  _Matches[N] := _Matches[Pred(_MatchCount)];
  Dec(_MatchCount);
End;

Procedure MatchmakingServer.OnMatchList(Msg: PNetMessage; Sock: Socket);
Var
  I, J:Integer;
  Dst:MemoryStream;
Begin
  {$IFDEF DEBUGMODE}
  WriteLn('Listing all matches: ', _MatchCount);
  {$ENDIF}

  Dst := MemoryStream.Create(256);
  For I:=0 To Pred(_MatchCount) Do
  Begin
    Dst.Seek(0);
    Dst.Write(_Matches[I]._MatchID, 2);
    Dst.Write(_Matches[I]._PlayerLimit, 1);
    Dst.Write(_Matches[I]._UserCount, 1);
    {For J:=0 To Pred(_Matches[I]._UserCount) Do
    Begin
      Dst.Write(_Matches[I]._Users[J].ID, 2);
      Dst.WriteString(_Matches[I]._Users[J].Data);
    End;            }
    Dst.WriteString(_Matches[I]._Data);
    Self.SendMessage(Msg.Owner, netMatchList, Dst);
  End;
  Dst.Release;
End;

Procedure MatchmakingServer.OnMatchStart(Msg: PNetMessage; Sock: Socket);
Var
  I, N, K:Integer;
  Dst:MemoryStream;
Begin
  N := Self.GetUserMatch(Msg.Owner);
  If (N<0) Then
    Exit;

  {$IFDEF DEBUGMODE}
  WriteLn('Match starting: ', N);
  {$ENDIF}

  Dst := MemoryStream.Create(256);
  For I:=0 To Pred(_Matches[N]._UserCount) Do
  Begin
    Dst.Write(_Matches[N]._Users[I].ID, 2);
    Dst.WriteString(_Matches[N]._Users[I].Address);
  End;
  K := Dst.Position;

  For I:=0 To Pred(_Matches[N]._UserCount) Do
  Begin
    Dst.Seek(K);
    If (Not _Matches[N]._Users[I].IsBot) Then
      Self.SendMessage(_Matches[N]._Users[I].ID, netMatchStart, Dst);
  End;

  Dst.Release;
End;

Procedure MatchmakingServer.SendPlayerList(MatchID, PlayerID:Word);
Var
  I:Integer;
  Dst:MemoryStream;
Begin
  Dst := MemoryStream.Create(256);
  For I:=0 To Pred(_Matches[MatchID]._UserCount) Do
  Begin
    Dst.Seek(0);
    Dst.Write(_Matches[MatchID]._Users[I].ID, 2);
    Dst.Write(MatchID, 2);
    Dst.WriteString(_Matches[MatchID]._Users[I].Data);
    SendMessage(PlayerID, netMatchJoin, Dst);
  End;
  Dst.Release;
End;

{ Match }
Function MatchRoom.GetMessage(Msg:PNetMessage; PlayerID:Word): Boolean;
Var
  N, I, PlayerIndex:Integer;
Begin
  Result := False;
  PlayerIndex := -1;
  For I:=0 To Pred(_UserCount) Do
  If (_Users[I].ID = PlayerID) Then
  Begin
    PlayerIndex := I;
    Break;
  End;

  If (PlayerIndex<0) Then
    Exit;

  If (_Users[PlayerIndex].Sock = Nil) Then
    Exit;

  N := _Users[PlayerIndex].Sock.Read(Msg^, MessageHeaderSize);
  If (N>0) Then
  Begin
    Result := True;
    If (Msg.Size>0) Then
      _Users[PlayerIndex].Sock.Read(Msg.Data[0], Msg.Size);
  End;
End;

Function MatchRoom.IsOwner(ID:Word):Boolean;
Begin
  Result := False;
  If (Self._UserCount<1) Then
    Exit;

  Result := Self._Users[0].ID = ID;
End;

Function MatchRoom.HasUser(ID: Word): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._UserCount) Do
  If (_Users[I].ID = ID) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure MatchRoom.OnMatchDataChanged(Data:TERRAString);
Begin
  // do nothing
End;

Procedure MatchRoom.OnPlayerDataChanged(PlayerID: Word; Data:TERRAString);
Begin
  // do nothing
End;

Procedure MatchRoom.OnPlayerChat(PlayerID: Word; Msg:TERRAString);
Begin
  // do nothing
End;

Procedure MatchRoom.OnPlayerJoin(PlayerID: Word; Data:TERRAString);
Begin
  // do nothing
End;

Procedure MatchRoom.OnPlayerLeave(PlayerID: Word);
Begin
  // do nothing
End;

Procedure MatchRoom.OnMatchEnd(ErrorCode: Integer);
Begin
  // do nothing
End;

Procedure MatchRoom.OnMatchCancelled;
Begin
  // do nothing
End;

Procedure MatchRoom.OnMatchStart;
Begin
  // do nothing
End;

Procedure MatchRoom.OnMatchConnecting;
Begin
  // do nothing
End;

Procedure MatchRoom.BroadcastMessage(Opcode: Byte; Src: Stream);
Var
  Msg:LNetMessage;
Begin
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Size := Stream(Src).Position;
  Msg.Opcode := Opcode;
  Msg.Owner := _Users[Self.LocalIndex].ID;
  Stream(Src).Seek(0);
  Stream(Src).Read(Msg.Data[0], Msg.Size);
  BroadcastMessage(@Msg);
End;

Procedure MatchRoom.SendMessage(Opcode: Byte; Src: Stream; PlayerID:Word);
Var
  Msg:LNetMessage;
Begin
  If (PlayerID>=BaseBotID) Then
  Begin
    WriteLn('Invalid player handle: ',PlayerID);
    Exit;
  End;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Size := Stream(Src).Position;
  Msg.Opcode := Opcode;
  Msg.Owner := _Users[Self.LocalIndex].ID;
  Stream(Src).Seek(0);
  Stream(Src).Read(Msg.Data[0], Msg.Size);
  SendMessage(@Msg, PlayerID);
End;

Function MatchRoom.SendMessage(Msg:PNetMessage; PlayerID:Word):Boolean;
Var
  I,PlayerIndex:Integer;
Begin
  Result := False;

  If (PlayerID>=BaseBotID) Then
  Begin
    WriteLn('Invalid player handle: ',PlayerID);
    Exit;
  End;

  PlayerIndex := -1;
  For I:=0 To Pred(_UserCount) Do
  If (_Users[I].ID = PlayerID) Then
  Begin
    PlayerIndex := I;
    Break;
  End;

  If (PlayerIndex<0) Or (PlayerIndex = _LocalIndex) Or (PlayerIndex>=_UserCount) Then
    Exit;

  If _Users[PlayerIndex].Sock<>Nil Then
  Begin
    _Users[PlayerIndex].Sock.Write(Msg^, MessageHeaderSize + Msg.Size);
    Result := True;
  End;
End;

Procedure MatchRoom.BroadcastMessage(Msg: PNetMessage);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UserCount) Do
  If (Self._LocalIndex<>I) And (_Users[I].Sock<>Nil) Then
    _Users[I].Sock.Write(Msg^, MessageHeaderSize + Msg.Size);
End;

Procedure MatchRoom.AddUser(ID:Word; Data, IP:TERRAString; LocalID:Integer; CanHost:Boolean);
Var
  I, N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_UserCount) Do
  If (_Users[I].ID = ID ) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _UserCount;
    Inc(_UserCount);
    SetLength(_Users, _UserCount);
  End;

  _Users[N].ID := ID;
  _Users[N].Data := Data;
  _Users[N].Address := IP;
  _Users[N].Sock := Nil;
  _Users[N].IsBot := False;
  _Users[N].CanHost := CanHost;

  If (ID = LocalID) Then
    Self._LocalIndex := N;
End;

Procedure MatchRoom.AddBot(ID:Word; Data:TERRAString);
Var
  I, N:Integer;
Begin
  N := _UserCount;
  Inc(_UserCount);
  SetLength(_Users, _UserCount);

  _Users[N].ID := ID;
  _Users[N].Data := Data;
  _Users[N].Address := '';
  _Users[N].Sock := Nil;
End;


Procedure MatchRoom.RemoveBot(ID: Word);
Var
  I, N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_UserCount) Do
  If (_Users[I].ID = ID) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Users[N] := _Users[Pred(_UserCount)];
  Dec(_UserCount);
End;

Function MatchRoom.GetUserByID(ID: Word): PMatchUser;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UserCount) Do
  If (_Users[I].ID = ID) Then
  Begin
    Result := @_Users[I];
    Exit;
  End;

  Result := Nil;
End;

{ MatchmakingClient }
procedure MatchmakingClient.ConnectionEnd(ErrorCode: Integer);
begin
  inherited;
  If (ErrorCode<>errNoError) Then
    _Browser.OnServerError(ErrorCode);
end;

Procedure MatchmakingClient.ConnectionStart;
Begin
  Inherited;
  Self.RefreshMatchList();
end;

Constructor MatchmakingClient.Create(GamePort:Cardinal; UserData:TERRAString; Browser:MatchMakingBrowser; MyMatchClass:MatchClass; RouterIP:TERRAString);
Begin
  Inherited Create(netTCP);

  _CurrentMatch := Nil;
  _GamePort := GamePort;

  {nat := UPNP.Create(5,10);
  If (nat.discovery()) Then
  Begin
    nat.add_port_mapping('test', '192.168.12.117', 1234,1234,'TCP');
  End;
  nat.Release;

  AddUPnPEntry(GamePort, Application.Instance.Title, RouterIP);
  }
  Self._MatchClass := MyMatchClass;
  Self._Browser := Browser;
  Self._UserData := UserData;

  Self._OpcodeList[netMatchDelete] := OnMatchDelete;
  Self._OpcodeList[netMatchList] := OnMatchList;
  Self._OpcodeList[netMatchChat] := OnMatchChat;
  Self._OpcodeList[netMatchJoin] := OnMatchJoin;
  Self._OpcodeList[netMatchLeave] := OnMatchLeave;
  Self._OpcodeList[netMatchStart] := OnMatchStart;
  Self._OpcodeList[netMatchData] := OnMatchData;
  Self._OpcodeList[netMatchCreate] := OnMatchCreate;
  Self._OpcodeList[netMatchError] := OnMatchError;
  Self._OpcodeList[netMatchPlayerData] := OnPlayerDataChange;
  Self._OpcodeList[netMatchAddBot] := OnMatchAddBot;
End;

Procedure MatchmakingClient.CreateMatch(MatchData, UserData:TERRAString; PlayerLimit:Byte);
Var
  Dst:MemoryStream;
Begin
  _NewMatch := True;
  _NewMatchTime := GetTime();

  Dst := MemoryStream.Create(256);
  Dst.WriteString(MatchData);
  Dst.WriteString(UserData);
  Dst.Write(PlayerLimit, 1);
  Dst.Write(_GamePort, 2);
  Self.SendMessage(netMatchCreate, Dst);
  Dst.Release;

  Self.RefreshMatchList();
End;

Function MatchmakingClient.GetMatchData:TERRAString;
Begin
  If (Assigned(Self._CurrentMatch)) Then
    Result := Self._CurrentMatch._Data
  Else
    Result := '';
End;

Function MatchmakingClient.GetPlayerData(PlayerID: Word):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  If (Assigned(Self._CurrentMatch)) Then
  Begin
    For I:=0 To Pred(Self._CurrentMatch._UserCount) Do
    If (Self._CurrentMatch._Users[I].ID = PlayerID) Then
    Begin
      Result := Self._CurrentMatch._Users[I].Data;
      Exit;
    End;
  End;
End;

Procedure MatchmakingClient.JoinMatch(MatchID: Word; Data:TERRAString);
Var
  PlayerID:Word;
  Dst:MemoryStream;
Begin
  _NewMatch := True;
  _NewMatchTime := GetTime();
  
  PlayerID := Self._LocalID;
  Dst := MemoryStream.Create(256);
  Dst.Write(PlayerID, 2);
  Dst.Write(MatchID, 2);
  Dst.WriteString(Data);
  Self.SendMessage(netMatchJoin, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.LeaveMatch();
Var
  ID:Word;
  Dst:MemoryStream;
Begin
  ID := Self._LocalID;
  Dst := MemoryStream.Create(256);
  Dst.Write(ID, 2);
  Self.SendMessage(netMatchLeave, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.EndMatch;
Var
  I:Integer;
Begin
  If (_CurrentMatch=Nil) Then
    Exit;

  For I:=0 To Pred(_CurrentMatch._UserCount) Do
  If (Assigned(_CurrentMatch._Users[I].Sock)) Then
  Begin
    _CurrentMatch._Users[I].Sock.Release;
    _CurrentMatch._Users[I].Sock := Nil;
  End;
End;

Procedure MatchmakingClient.StartMatch();
Var
  ID:Word;
  Dst:MemoryStream;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  ID := Self._LocalID;
  Dst := MemoryStream.Create(256);
  Dst.Write(_CurrentMatch._MatchID, 2);
  Self.SendMessage(netMatchStart, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.OnMatchChat(Msg:PNetMessage; Sock:Socket);
Var
  Src:MemoryStream;
  S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.ReadString(S);
  Src.Release;

  _CurrentMatch.OnPlayerChat(Msg.Owner, S);
End;

Procedure MatchmakingClient.OnMatchCreate(Msg:PNetMessage; Sock:Socket);
Var
  Src:MemoryStream;
  I:Integer;
  MatchID:Word;
Begin
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(MatchID, 2);
  Src.Release;

  For I:=0 To Pred(_Browser._MatchCount) Do
  If (_Browser._Matches[I]._MatchID = MatchID) Then
  Begin
    Self._CurrentMatch := _Browser._Matches[I];
    Self._CurrentMatch.OnMatchEnter();
    Exit;
  End;

  Inc(_Browser._MatchCount);
  SetLength(_Browser._Matches, _Browser._MatchCount);
  _Browser._Matches[Pred(_Browser._MatchCount)] := Self._MatchClass.Create();
  _Browser._Matches[Pred(_Browser._MatchCount)]._MatchID := MatchID;
  _Browser._Matches[Pred(_Browser._MatchCount)]._UserCount := 0;
  Self._CurrentMatch := _Browser._Matches[Pred(_Browser._MatchCount)];
  Self._CurrentMatch.OnMatchEnter();
End;

Procedure MatchmakingClient.OnMatchData(Msg: PNetMessage; Sock: Socket);
Var
  I:Integer;
  Src:MemoryStream;
  S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.ReadString(S);
  Src.Release;

  Self._CurrentMatch.OnMatchDataChanged(S);
End;

Procedure MatchmakingClient.OnMatchDelete(Msg: PNetMessage; Sock: Socket);
Var
  MatchID:Word;
  I:Integer;
  Src:MemoryStream;
Begin
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(MatchID, 2);
  Src.Release;

  If (_CurrentMatch <> Nil) And (_CurrentMatch._MatchID = MatchID) Then
    _CurrentMatch.OnMatchCancelled();

  I := 0;
  While (I<_Browser._MatchCount) Do
  If (_Browser._Matches[I]._MatchID = MatchID) Then
  Begin
    _Browser.OnMatchDelete(MatchID);
    _Browser._Matches[I].Release();
    _Browser._Matches[I] := _Browser._Matches[Pred(_Browser._MatchCount)];
    Dec(_Browser._MatchCount);
    Break;
  End Else
    Inc(I);

  _CurrentMatch := Nil;
End;

Procedure MatchmakingClient.OnMatchJoin(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  PlayerID, MatchID:Word;
  Src:MemoryStream;
  S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(PlayerID, 2);
  Src.Read(MatchID, 2);
  Src.ReadString(S);
  Src.Release;

  N := _CurrentMatch._UserCount;
  _CurrentMatch.AddUser(PlayerID, S, '', Self._LocalID, False);

  _CurrentMatch.OnPlayerJoin(PlayerID, S);
End;

Procedure MatchmakingClient.OnMatchAddBot(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  BotID, MatchID:Word;
  Src:MemoryStream;
  S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(BotID, 2);
  Src.Read(MatchID, 2);
  Src.ReadString(S);
  Src.Release;

  N := _CurrentMatch._UserCount;
  _CurrentMatch.AddBot(BotID, S);

  _CurrentMatch.OnPlayerJoin(BotID, S);
End;

Procedure MatchmakingClient.OnMatchLeave(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  ID:Word;
  Src:MemoryStream;
  S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(ID, 2);
  Src.Release;

  I := 0;
  While (I<_CurrentMatch._UserCount) Do
  If (_CurrentMatch._Users[I].ID = ID) Then
  Begin
    _CurrentMatch.OnPlayerLeave(ID);
    _CurrentMatch._Users[I] := _CurrentMatch._Users[Pred(_CurrentMatch._UserCount)];
    Break;
  End Else
    Inc(I);
End;

Procedure MatchmakingClient.OnMatchList(Msg: PNetMessage; Sock: Socket);
Var
  I, N:Integer;
  MatchID:Word;
  Count, PlayerLimit:Byte;
  Src:MemoryStream;
  S:TERRAString;
Begin
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(MatchID, 2);

  N := -1;
  For I:=0 To Pred(_Browser._MatchCount) Do
  If (_Browser._Matches[I]._MatchID = MatchID) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _Browser._MatchCount;
    Inc(_Browser._MatchCount);
    SetLength(_Browser._Matches, _Browser._MatchCount);
    _Browser._Matches[Pred(_Browser._MatchCount)] := Self._MatchClass.Create();
  End;

  _Browser._Matches[N]._MatchID := MatchID;

  Src.Read(PlayerLimit, 1);
  Src.Read(Count, 1);

  _Browser._Matches[N]._UserCount := Count;
  SetLength(_Browser._Matches[N]._Users, _Browser._Matches[N]._UserCount);
  {For I:=0 To Pred(_Browser._Matches[N]._UserCount) Do
  Begin
    Src.Read(_Browser._Matches[N]._Users[I].ID, 2);
    Src.ReadString(_Browser._Matches[N]._Users[I].Data);
  End;}

  Src.ReadString(S);
  _Browser._Matches[N]._Data := S;
  _Browser._Matches[N]._PlayerLimit := PlayerLimit;
  Src.Release;

  If Not _Browser._Matches[N]._Visible Then
  Begin
    _Browser._Matches[N]._Visible := True;
    _Browser.OnMatchAdd(MatchID);
  End Else
    _Browser.OnMatchChange(MatchID);
End;

Procedure MatchmakingClient.OnMatchStart(Msg: PNetMessage; Sock: Socket);
Var
  I,J:Integer;
  ID:Word;
  Src:MemoryStream;
  Addr, S:TERRAString;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  For I:=0 To Pred(_CurrentMatch._UserCount) Do
  Begin
    Src.Read(ID, 2);
    Src.ReadString(Addr);
    For J:=0 To Pred(_CurrentMatch._UserCount) Do
    If (_CurrentMatch._Users[J].ID = ID) Then
    Begin
      _CurrentMatch._Users[J].Address := Addr;
      Break;
    End;
  End;
  Src.Release;

  _Connecting := True;
  _LastConnect := 0;
  _ConnectionStart := GetTime;

  For I:=0 To Pred(_CurrentMatch._UserCount) Do
    _CurrentMatch._Users[I].ConnectionFailed := False;
    
  Self._CurrentMatch.OnMatchConnecting();
End;

Procedure MatchmakingClient.OnPlayerDataChange(Msg: PNetMessage; Sock: Socket);
Var
  Src:MemoryStream;
  PlayerID:Word;
  Data:TERRAString;
  I:Integer;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(PlayerID, 2);
  Src.ReadString(Data);
  Src.Release;

  For I:=0 To Pred(_CurrentMatch._UserCount) Do
  If (_CurrentMatch._Users[I].ID = PlayerID) Then
  Begin
    _CurrentMatch.OnPlayerDataChanged(PlayerID, Data);
    _CurrentMatch._Users[I].Data := Data;
    Break;
  End;
End;

Procedure MatchmakingClient.RefreshMatchList;
Var
  Msg:LNetMessage;
Begin
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Opcode := netMatchList;
  Self.SendMessage(@Msg);
End;

Procedure MatchmakingClient.SendChat(Msg:TERRAString);
Var
  Dst:MemoryStream;
Begin
  If Not Self.IsConnected Then
    Exit;

  Dst := MemoryStream.Create(256);
  Dst.WriteString(Msg);
  Self.SendMessage(netMatchChat, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.SetMatchData(Data:TERRAString);
Var
  Dst:MemoryStream;
Begin
  Dst := MemoryStream.Create(256);
  Dst.WriteString(Data);
  Self.SendMessage(netMatchData, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.RemoveBotFromMatch(BotID:Word);
Var
  Dst:MemoryStream;
Begin
  Dst := MemoryStream.Create(256);
  Dst.Write(BotID, 2);
  Self.SendMessage(netMatchLeave, Dst);
  Dst.Release;
End;

Function MatchmakingClient.AddBotToMatch(Data:TERRAString):Word;
Var
  Dst:MemoryStream;
  BotID, MatchID:Word;
  Found:Boolean;
  I:Integer;
Begin
  If _CurrentMatch = Nil Then
    Exit;

  If (_CurrentMatch._UserCount>=_CurrentMatch._PlayerLimit) Then
  Begin
    Result := 0;
    Exit;
  End;

  BotID := BaseBotID;
  Repeat
    Found := False;
    For I:=0 To Pred(_CurrentMatch._UserCount) Do
    If (_CurrentMatch._Users[I].ID = BotID) Then
    Begin
      Found := True;
      Break;
    End;

    If (Found) Then
      Inc(BotID);
  Until (Not Found);

  MatchID := _CurrentMatch._MatchID;

  Dst := MemoryStream.Create(256);
  Dst.Write(BotID, 2);
  Dst.Write(MatchID, 2);
  Dst.WriteString(Data);
  Self.SendMessage(netMatchAddBot, Dst);
  Dst.Release;

  Result := BotID;
End;

Procedure MatchmakingClient.SetBotData(BotID:Word; Data:TERRAString);
Var
  PlayerID:Word;
  Dst:MemoryStream;
Begin
  Dst := MemoryStream.Create(256);
  Dst.Write(BotID, 2);
  Dst.WriteString(Data);
  Self.SendMessage(netMatchPlayerData, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.SetPlayerData(Data:TERRAString);
Var
  PlayerID:Word;
  Dst:MemoryStream;
Begin
  PlayerID := Self._LocalID;
  Self._UserData := Data;

  Dst := MemoryStream.Create(256);
  Dst.Write(PlayerID, 2);
  Dst.WriteString(Data);
  Self.SendMessage(netMatchPlayerData, Dst);
  Dst.Release;
End;

Procedure MatchmakingClient.Update;
Var
  Sock:Socket;
  IP, S:TERRAString;
  Ack, T:Cardinal;
  I, Count:Integer;
Begin
  Inherited;

  If (_NewMatch) Then
  Begin
    Sock := OpenIncomingStream(_GamePort);
    If Assigned(Sock) Then
    Begin
      Sock.Read(Ack, 4);
      S := DecodeInt(Ack);
      If (S='HELO') Then
      Begin
        Ack := EncodeInt('HOST');
        Sock.Write(Ack, 4);
      End;
      Sock.Release;
    End;

    T := GetTime() - _NewMatchTime;
    If (T>=2000) Then
      _NewMatch := False;
  End;

  If (_Connecting) And (GetTime - _LastConnect> ConnectionDelay) Then
  Begin
    _LastConnect := GetTime();

    For I:=0 To Pred(_CurrentMatch._UserCount) Do
    If (I = _CurrentMatch._LocalIndex) Then
      Continue
    Else
    If (_CurrentMatch._Users[I].Sock<>Nil) Then
    Begin
      If (_CurrentMatch._Users[I].Sock.Closed) Then
      Begin
        _CurrentMatch._Users[I].Sock.Release;
        _CurrentMatch._Users[I].Sock := Nil;
      End;
      Continue;
    End Else
    If (_CurrentMatch._Users[_CurrentMatch._LocalIndex].ID > _CurrentMatch._Users[I].ID) Then
    Begin
      _CurrentMatch._Users[I].Sock := Socket.Create(_CurrentMatch._Users[I].Address, _GamePort);
    End;

    Sock := OpenIncomingStream(_GamePort);
    If Assigned(Sock) Then
    Begin
      IP := GetIP(Sock.Address);
      For I:=0 To Pred(_CurrentMatch._UserCount) Do
      If (_CurrentMatch._Users[_CurrentMatch._LocalIndex].ID < _CurrentMatch._Users[I].ID)
      And (IP = _CurrentMatch._Users[I].Address) Then
      Begin
        Sock.Blocking := False;
        _CurrentMatch._Users[I].Sock := Sock;
      End;
    End;

    Count := 0;
    For I:=0 To Pred(_CurrentMatch._UserCount) Do
    If (I = _CurrentMatch._LocalIndex) Then
      Inc(Count)
    Else
    If (_CurrentMatch._Users[I].Sock<>Nil) And (Not _CurrentMatch._Users[I].Sock.Closed) Then
      Inc(Count);

    If (Count>=_CurrentMatch._UserCount) Then
    Begin
      For I:=0 To Pred(_CurrentMatch._UserCount) Do
      If Assigned(_CurrentMatch._Users[I].Sock) Then
      Begin
        _CurrentMatch._Users[I].Sock.Blocking := False;
        _CurrentMatch._Users[I].Sock.SetDelay(False);
      End;
      
      _Connecting := False;
      _CurrentMatch.OnMatchStart();
    End Else
    If (GetTime - _ConnectionStart > ConnectionDuration) Then
    Begin
      _Connecting := False;
      For I:=0 To Pred(_CurrentMatch._UserCount) Do
        If Assigned(_CurrentMatch._Users[I].Sock) Then
        Begin
          _CurrentMatch._Users[I].Sock.Release;
          _CurrentMatch._Users[I].Sock := Nil;
          _CurrentMatch._Users[I].ConnectionFailed := False;
        End Else
        Begin
          _CurrentMatch._Users[I].ConnectionFailed := True;
        End;
      _CurrentMatch.OnMatchEnd(matchErrorConnectionFailed);
    End;

  End;
End;

Procedure MatchmakingClient.OnMatchError(Msg: PNetMessage; Sock: Socket);
Var
  MatchID, ErrorCode:Word;
  Src:MemoryStream;
Begin
  Src := MemoryStream.Create(256, @Msg.Data[0]);
  Src.Read(MatchID, 2);
  Src.Read(ErrorCode, 2);
  Src.Release;

  _Browser.OnServerError(ErrorCode);
End;

{ MatchMakingBrowser }
Function MatchMakingBrowser.GetMatchByIndex(Index: Integer): MatchRoom;
Begin
  If (Index<0) Or (Index>=_MatchCount) Then
    Result := Nil
  Else
    Result := _Matches[Index];
End;

Function MatchMakingBrowser.GetMatchByID(ID: Integer): MatchRoom;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MatchCount) Do
  If (_Matches[I]._MatchID = ID) Then
  Begin
    Result := _Matches[I];
    Exit;
  End;

  Result := Nil
End;

Procedure MatchMakingBrowser.OnMatchAdd(ID: Word);
Begin
  // do nothing
End;

Procedure MatchMakingBrowser.OnMatchChange(ID: Word);
Begin
  // do nothing
End;

Procedure MatchMakingBrowser.OnMatchDelete(ID: Word);
Begin
  // do nothing
End;

Procedure MatchMakingBrowser.OnServerError(ErrorCode:Integer);
Begin
  // do nothing
End;

Procedure MatchRoom.OnMatchEnter;
Begin
  // do nothing
End;

Function MatchRoom.GetUserByIndex(Index: Integer): PMatchUser;
Begin
  If (Index<0) Or (Index>=Self._UserCount) Then
    Result := Nil
  Else
    Result := @(_Users[Index]);
End;

Function MatchRoom.GetOwner: PMatchUser;
Begin
  If (Self._UserCount>0) Then
    Result := @_Users[0]
  Else
    Result := Nil;
End;

Function MatchRoom.GetLocalID:Word;
Begin
  Result := _Users[Self._LocalIndex].ID;
End;

End.