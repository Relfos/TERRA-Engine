{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} Ships_NetClient;

Uses TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_UI, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_PNG, TERRA_FileManager,
  TERRA_Texture, TERRA_Network, TERRA_SpriteManager, TERRA_Sockets, TERRA_Viewport,
  TERRA_Math, TERRA_Vector3D, TERRA_Vector2D, TERRA_TTF, TERRA_Utils, TERRA_InputManager,
  TERRA_NetClient, Ships_Opcodes;

Const
  ExplosionSpeed = 100;


Type
  // A client is used to process application events
  MyGame = Class(AppClient)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(View:Viewport); Override;
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
    Tint:Color;
    Dead:Boolean;

    Procedure Rotate(DW:Single);
    Procedure Accelerate(DS:Single);
  End;


Var
  // game resources
  _ShipTex:Texture;
  _ExplosionTex:Texture;

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
  _Explosions[Pred(_ExplosionCount)].Time := GetTime;
End;

Procedure AddShip(X, Y:Single; ID:Integer);
Begin
  Inc(_ShipCount);
  SetLength(_Ships, _ShipCount);
  _Ships[Pred(_ShipCount)].Dead := False;
  _Ships[Pred(_ShipCount)].X := X;
  _Ships[Pred(_ShipCount)].Y := Y;
  _Ships[Pred(_ShipCount)].ID := ID;
  _Ships[Pred(_ShipCount)].LastUpdate := GetTime;
  _Ships[Pred(_ShipCount)].Thrust := 1.0;
  _Ships[Pred(_ShipCount)].Tint := ColorCreate(RandomFloat(0.75, 1), RandomFloat(0.75, 1), RandomFloat(0.75, 1));
End;

{ MyScene }
Procedure MyScene.RenderSprites(View:Viewport);
Var
  S:Sprite;
  P,V:Vector3D;
  T:Cardinal;
  I:Integer;
  N, TX, TY:Integer;
  Delta ,DX, DY:Single;
  Msg:NetMessage;
Begin
  If (ConnectionFail) And (Assigned(_Client)) Then
  Begin
    _Client.Release();
    _Client := Nil;
  End;

  // render some text
  UIManager.Instance.FontRenderer.DrawText(50, 70, 10, PAnsiChar(StatusMsg));

  // update and render all explosions
  I:=0;
  While (I<_ExplosionCount) Do
  Begin
    N := ((GetTime-_Explosions[I].Time) Div ExplosionSpeed);
    If (N<10) Then
    Begin
      TX := N Mod 5;
      TY := N Div 5;
      S := SpriteManager.Instance.DrawSprite(_Explosions[I].X, _Explosions[I].Y, 20, _ExplosionTex);
      S.Rect.PixelRemap(92*TX, 92*TY, Succ(TX)*92, Succ(TY)*92);
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
      Delta := (GetTime - _Ships[I].LastUpdate);
      Delta := Delta/20;

      S := SpriteManager.Instance.DrawSprite(_Ships[I].X, _Ships[I].Y, 20, _ShipTex);
      S.Rect.PixelRemap(64, 0, 112, 32);
      S.Anchor := VectorCreate2D(0.5, 0.5);
      S.SetScaleAndRotation(1.0, _Ships[I].Rotation);
      S.SetColor(_Ships[I].Tint);

      If (Delta>=1) Then
      Begin
        // Is this our local ship?
        If (_Ships[I].ID = _MyID) Then
        Begin
          _Ships[I].Delta := Delta;
          _Ships[I].LastUpdate := GetTime;

          DX := -Sin(_Ships[I].Rotation);
          DY := -Cos(_Ships[I].Rotation);

          Delta := Delta * _Ships[I].Thrust;
          _Ships[I].X := _Ships[I].X + Delta *DX;
          _Ships[I].Y := _Ships[I].Y + Delta *DY;

          // we moved a bit, notify the server (and the server will notify all players)
          If (Assigned(_Client)) And (GetTime - NetUpdate>100) Then
          Begin
            Msg := NetMessage.Create(netPlayerData);
            Msg.WriteInteger(_MyID);
            Msg.WriteSingle(_Ships[I].X);
            Msg.WriteSingle(_Ships[I].Y);
            Msg.WriteSingle(_Ships[I].Rotation);
            _Client.SendMessage(Msg);  // send it
            ReleaseObject(Msg);

            NetUpdate := GetTime;
          End;
        End;

        // wrap around the ship if it reaches the screen edges
        If (_Ships[I].X> GraphicsManager.Instance.Width) Then
          _Ships[I].X := 0;

        If (_Ships[I].Y> GraphicsManager.Instance.Height) Then
          _Ships[I].Y := 0;

        If (_Ships[I].X<0) Then
          _Ships[I].X := GraphicsManager.Instance.Width;

        If (_Ships[I].Y<0) Then
          _Ships[I].Y := GraphicsManager.Instance.Height;

      End Else
        _Ships[I].Delta := 0;

      // was this ship dead?
      If (_Ships[I].Dead) And (_Ships[I].ID<>_MyID) Then
      Begin
        _Ships[I] := _Ships[Pred(_ShipCount)];
        Dec(_ShipCount);
        StatusMsg := ' Players connected:' + IntToString(_ShipCount);
      End Else
        Inc(I);
    End;
  End;
End;

{ Game }
Procedure MyGame.OnCreate;
Begin
  StatusMsg := 'Not connected.';

  FileManager.Instance.AddPath('Assets');

  // Load a font
  _ShipTex := TextureManager.Instance.GetTexture('ships');
  _ExplosionTex := TextureManager.Instance.GetTexture('explosion');

  _MyID := -1;

  _Client := MyNetClient.Create;
  _Client.Connect(GamePort, GameVersion, 'localhost', 'username', '');

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorBlack;

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnDestroy;
Begin
  If Assigned(_Client) Then
    _Client.Release;
End;

Procedure MyGame.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  If (_MyID>=0) Then
  Begin
    If InputManager.Instance.Keys.IsDown(keyLeft) Then
      _Ships[GetID(_MyID)].Rotate(-1);

    If InputManager.Instance.Keys.IsDown(keyRight) Then
      _Ships[GetID(_MyID)].Rotate(1);

    If InputManager.Instance.Keys.IsDown(keyUp) Then
      _Ships[GetID(_MyID)].Accelerate(1);

    If InputManager.Instance.Keys.IsDown(keyDown) Then
      _Ships[GetID(_MyID)].Accelerate(-1);
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
  If (Thrust>2) Then
    Thrust := 2;
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
  AddShip(Random(GraphicsManager.Instance.Width), Random(GraphicsManager.Instance.Height), _MyID);
End;

Procedure MyNetClient.ConnectionEnd(ErrorCode:Integer; ErrorLog:String);
Begin
  StatusMsg := 'Net error (code: '+IntToString(ErrorCode)+')';
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
    StatusMsg := ' Players connected:' + IntToString(_ShipCount);
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
  ApplicationStart(MyGame.Create);
End.