{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} Ships_NetClient;

Uses TERRA_DemoApplication, TERRA_Object,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_PNG, TERRA_FileManager,
  TERRA_Texture, TERRA_Network, TERRA_Sprite, TERRA_Sockets, TERRA_Viewport,
  TERRA_Math, TERRA_Vector3D, TERRA_Vector2D, TERRA_TTF, TERRA_Utils, TERRA_InputManager,
  TERRA_NetClient, TERRA_Engine, Ships_Opcodes;

Const
  ExplosionSpeed = 100;


Type
  // A client is used to process application events
  Demo = Class(DemoApplication)
    Protected
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender2D(View:TERRAViewport); Override;
  End;

{Creating a TERRA netclient is simple, register all custom message handlers in the constructor using AddHandler()
Method ConnectionStart() is called upon sucessful connection.
Method ConnectionEnd() is called when connection ends or fails}

  MyNetClient = Class(NetClient)
    Procedure ConnectionStart; Override;
    Procedure ConnectionEnd(ErrorCode:Integer; ErrorLog:String); Override;

    // our two custom message handlers
    Procedure OnPlayerDrop(Msg:NetMessage);
    Procedure OnPlayerData(Msg:NetMessage);

    Constructor Create;
    Procedure Release; Override;
  End;

  Explosion = Record
    X, Y:Single;
    Time:Integer;
  End;

  // a ship object
  Ship = Object
    ID:Integer;
    X, Y:Single;
    Rotation:Single;
    LastUpdate:Integer;
    Delta:Single;
    Thrust:Single;
    Tint:ColorRGBA;
    Dead:Boolean;

    Procedure Rotate(DW:Single);
    Procedure Accelerate(DS:Single);
  End;


Var
  // game resources
  _ShipTex:TERRATexture;
  _ExplosionTex:TERRATexture;

  // our netclient
  _Client:MyNetClient;

  // game objects
  _Explosions:Array Of Explosion;
  _ExplosionCount:Integer;

  _Ships:Array Of Ship;
  _ShipCount:Integer;

  // our local network ID
  _MyID:Integer;

  ConnectionFail:Boolean;

  NetUpdate:Cardinal;
  StatusMsg:String;

Function GetID(ID:Integer):Integer;
Var
  I:Integer;
Begin
  Result := -1;
  For I:= 0 To Pred(_ShipCount) Do
  If (_Ships[I].ID = ID) Then
  Begin
    Result := I;
    Exit;
  End;
End;

Procedure AddExplosion(X,Y:Single);
Begin
  Inc(_ExplosionCount);
  SetLength(_Explosions, _ExplosionCount);
  _Explosions[Pred(_ExplosionCount)].X := X;
  _Explosions[Pred(_ExplosionCount)].Y := Y;
  _Explosions[Pred(_ExplosionCount)].Time := Application.GetTime();
End;

Procedure AddShip(X, Y:Single; ID:Integer);
Begin
  Inc(_ShipCount);
  SetLength(_Ships, _ShipCount);
  _Ships[Pred(_ShipCount)].Dead := False;
  _Ships[Pred(_ShipCount)].X := X;
  _Ships[Pred(_ShipCount)].Y := Y;
  _Ships[Pred(_ShipCount)].ID := ID;
  _Ships[Pred(_ShipCount)].LastUpdate := Application.GetTime();
  _Ships[Pred(_ShipCount)].Thrust := 1.0;
  _Ships[Pred(_ShipCount)].Tint := ColorCreateFromFloat(RandomFloat(0.75, 1), RandomFloat(0.75, 1), RandomFloat(0.75, 1));
End;

{ Game }
Procedure Demo.OnCreate;
Begin
  Inherited;

  // enable 2D/GUI rendering

  Self.GUI.Viewport.Visible := True;

  StatusMsg := 'Not connected.';

  _ShipTex := Engine.Textures['ships'];
  _ExplosionTex := Engine.Textures['explosion'];

  _MyID := -1;

  _Client := MyNetClient.Create;
  _Client.Connect(GamePort, GameVersion, 'localhost', 'username', '');
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_Client);
End;

Procedure Demo.OnIdle;
Begin
  Inherited;

  If (_MyID>=0) Then
  Begin
    If Engine.Input.Keys.IsDown(keyLeft) Then
      _Ships[GetID(_MyID)].Rotate(1);

    If Engine.Input.Keys.IsDown(keyRight) Then
      _Ships[GetID(_MyID)].Rotate(-1);

    If Engine.Input.Keys.IsDown(keyUp) Then
      _Ships[GetID(_MyID)].Accelerate(1);

    If Engine.Input.Keys.IsDown(keyDown) Then
      _Ships[GetID(_MyID)].Accelerate(-1);
  End;
End;

Procedure Demo.OnRender2D(View:TERRAViewport);
Var
  S:TERRASprite;
  P,V:Vector3D;
  T:Cardinal;
  I:Integer;
  N, TX, TY:Integer;
  Delta ,DX, DY:Single;
  Msg:NetMessage;
Begin
  Inherited;
  
  If (ConnectionFail) And (Assigned(_Client)) Then
  Begin
    _Client.Release();
    _Client := Nil;
  End;

  // render some text
  Self.FontRenderer.DrawText(View, 50, 70, 10, StatusMsg);

  // update and render all explosions
  I:=0;
  While (I<_ExplosionCount) Do
  Begin
    N := ((Application.GetTime() - _Explosions[I].Time) Div ExplosionSpeed);
    If (N<10) Then
    Begin
      TX := N Mod 5;
      TY := N Div 5;
      S := Engine.FetchSprite();
      S.Layer := 20;
      S.SetTexture(_ExplosionTex);
      S.SetUVs((92*TX) / _ExplosionTex.Width, (92*TY) / _ExplosionTex.Height, (Succ(TX)*92) / _ExplosionTex.Width, (Succ(TY)*92) / _ExplosionTex.Height);
      S.Translate(_Explosions[I].X, _Explosions[I].Y);
      S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, 64, 64);
      Engine.Graphics.AddRenderable(View, S);

      Inc(I);
    End Else
    Begin
      _Explosions[I] := _Explosions[Pred(_ExplosionCount)];
      Dec(_ExplosionCount);
    End;
  End;

  // update and render all ships
  I := 0;
  While (I<_ShipCount) Do
  Begin
    Begin
      Delta := (Application.GetTime() - _Ships[I].LastUpdate);
      Delta := Delta/20;

      S := Engine.FetchSprite();
      S.Layer := 20;
      S.SetTexture(_ShipTex);
      S.SetUVs(64/_ShipTex.Width, 0, 112/_ShipTex.Width, 32/_ShipTex.Height);
      S.SetColor(_Ships[I].Tint);

      S.Rotate(_Ships[I].Rotation);
      S.Translate(_Ships[I].X, _Ships[I].Y);

      S.AddQuad(spriteAnchor_Center, Vector2D_Create(0, 0), 0.0, 64, 64);
      Engine.Graphics.AddRenderable(View, S);


      If (Delta>=1) Then
      Begin
        // Is this our local ship?
        If (_Ships[I].ID = _MyID) Then
        Begin
          _Ships[I].Delta := Delta;
          _Ships[I].LastUpdate := Application.GetTime();

          DX := Sin(_Ships[I].Rotation);
          DY := -Cos(_Ships[I].Rotation);

          Delta := Delta * _Ships[I].Thrust;
          _Ships[I].X := _Ships[I].X + Delta *DX;
          _Ships[I].Y := _Ships[I].Y + Delta *DY;

          // we moved a bit, notify the server (and the server will notify all players)
          If (Assigned(_Client)) And (Application.GetTime() - NetUpdate>100) Then
          Begin
            Msg := NetMessage.Create(netPlayerData);
            Msg.WriteInteger(_MyID);
            Msg.WriteSingle(_Ships[I].X);
            Msg.WriteSingle(_Ships[I].Y);
            Msg.WriteSingle(_Ships[I].Rotation);
            _Client.SendMessage(Msg);  // send it
            ReleaseObject(Msg);

            NetUpdate := Application.GetTime();
          End;
        End;

        // wrap around the ship if it reaches the screen edges
        If (_Ships[I].X> View.Width) Then
          _Ships[I].X := 0;

        If (_Ships[I].Y> View.Height) Then
          _Ships[I].Y := 0;

        If (_Ships[I].X<0) Then
          _Ships[I].X := View.Width;

        If (_Ships[I].Y<0) Then
          _Ships[I].Y := View.Height;

      End Else
        _Ships[I].Delta := 0;

      // was this ship dead?
      If (_Ships[I].Dead) And (_Ships[I].ID<>_MyID) Then
      Begin
        _Ships[I] := _Ships[Pred(_ShipCount)];
        Dec(_ShipCount);
        StatusMsg := ' Players connected:' + IntegerProperty.Stringify(_ShipCount);
      End Else
        Inc(I);
    End;
  End;
End;

{ Ship }

Procedure Ship.Rotate(DW: Single);
Begin
  Rotation := Rotation + DW * Delta * -0.1;
  If (Rotation<0) Then
    Rotation := Rotation + 360*RAD;
  If (Rotation>=360*RAD) Then
    Rotation := Rotation - 360*RAD;
End;

Procedure Ship.Accelerate(DS:Single);
Begin
  Thrust := Thrust + DS * Delta * 0.1;

  If (Thrust<0.1) Then
    Thrust := 0.1;

  If (Thrust>5) Then
    Thrust := 5;
End;

{ MyClient }
Constructor MyNetClient.Create;
Begin
  Inherited;
  Self.AddHandler(netPlayerDrop, OnPlayerDrop);
  Self.AddHandler(netPlayerData, OnPlayerData);
End;

Procedure MyNetClient.Release;
Var
  Msg:NetMessage;
Begin
  Msg := NetMessage.Create(netPlayerDrop);
  Msg.WriteInteger(_MyID);
  SendMessage(Msg);
  ReleaseObject(Msg);

  Inherited;
End;

Procedure MyNetClient.ConnectionStart;
Begin
  StatusMsg := ' Players connected: 1';

  _MyID := Self._LocalID;
  AddShip(Random(400), Random(300), _MyID);
End;

Procedure MyNetClient.ConnectionEnd(ErrorCode:Integer; ErrorLog:String);
Begin
  StatusMsg := 'Net error (code: '+IntegerProperty.Stringify(ErrorCode)+')';
  ConnectionFail := True;
End;

Procedure MyNetClient.OnPlayerData(Msg:NetMessage);
Var
  ID, I:Integer;
  X,Y, Rotation, Thrust:Single;
Begin
  Msg.ReadInteger(ID);
  Msg.ReadSingle(X);
  Msg.ReadSingle(Y);
  Msg.ReadSingle(Rotation);

  If (ID = _MyID) Then
    Exit;

  If (GetID(ID)<0) Then
  Begin
    AddShip(X, Y, ID);
    StatusMsg := ' Players connected:' + IntegerProperty.Stringify(_ShipCount);
  End;

  For I:=0 To Pred(_ShipCount) Do
  If (_Ships[I].ID = ID) Then
  Begin
    _Ships[I].X := X;
    _Ships[I].Y := Y;
    _Ships[I].Rotation := Rotation;
    Break;
  End;
End;

Procedure MyNetClient.OnPlayerDrop(Msg:NetMessage);
Var
  ID, I:Integer;
Begin
  Msg.ReadInteger(ID);

  If (ID = _MyID) Then
    Exit;

  For I:=0 To Pred(_ShipCount) Do
  If (_Ships[I].ID = ID) Then
  Begin
    _Ships[I].Dead := True;
    AddExplosion(_Ships[I].X, _Ships[I].Y);
  End;
End;

Begin
  // Start the application
  Demo.Create();
End.