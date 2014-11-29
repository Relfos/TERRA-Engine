Unit TERRA_Billboards;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Color, TERRA_Vector3D, TERRA_Shader, TERRA_Texture,
  TERRA_Mesh, TERRA_Vector2D, TERRA_ParticleRenderer, TERRA_UI;

Type
  PBillboard = ^Billboard;
  PBillboardGroup = ^BillboardGroup;

  Billboard = Object
    Position:Vector3D;
    Width, Height:Single;
    AnchorX, AnchorY:Single;
    Color:TERRA_Color.Color;
    U1, V1, U2, V2:Single;
    Group:PBillboardGroup;

    Procedure TileRemap(X,Y, TilesPerX, TilesPerY:Integer);
    Procedure TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
    Procedure PixelRemap(X1,Y1, X2, Y2:Integer; W:Integer=0; H:Integer=0);
    Procedure UVRemap(_U1, _V1, _U2, _V2:Single);
  End;

  BillboardGroup = Record
    _Billboards:Array Of Billboard;
    _BillboardCount:Integer;
    _Texture:Texture;
  End;

  BillboardManager = Class(TERRAObject)
    Protected
      _Groups:Array Of BillboardGroup;
      _GroupCount:Integer;

      _Shader:Shader;

      _Temp:Array Of ParticleVertex;



    Public
      Constructor Create;
      Destructor Destroy; Override;

      Class Function Instance:BillboardManager;


      Function AddBillboard(Position:Vector3D; Width, Height:Single; MyTexture:Texture):PBillboard;

      Procedure Render;

  End;

Implementation
Uses TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Var
  _BillboardInstance:BillboardManager;

Procedure Billboard.UVRemap(_U1, _V1, _U2, _V2:Single);
Begin
  Self.U1 := _U1;
  Self.V1 := _V1;
  Self.U2 := _U2;
  Self.V2 := _V2;
End;

Procedure Billboard.PixelRemap(X1, Y1, X2, Y2, W, H: Integer);
Begin
  If (Group._Texture = Nil) Then
    Exit;

  U1 := (X1/Group._Texture.Width*Group._Texture.Ratio.X);
  V1 := (Y1/Group._Texture.Height*Group._Texture.Ratio.Y);
  U2 := (X2/Group._Texture.Width*Group._Texture.Ratio.X);
  V2 := (Y2/Group._Texture.Height*Group._Texture.Ratio.Y);
 End;

Procedure Billboard.TileRemapByID(TileID, TilesPerRow, TileSize:Integer);
Var
  TX, TY:Integer;
  PX, PY:Single;
Begin
  If (Group._Texture = Nil) Then
    Exit;

  PX := (1/(Group._Texture.Width/Group._Texture.Ratio.X));
  PY := (1/(Group._Texture.Height/Group._Texture.Ratio.Y));

  TX := (TileID Mod TilesPerRow);
  TY := (TileID Div TilesPerRow);
  U1 := (TX/TilesPerRow + PX)*Group._Texture.Ratio.X;
  U2 := (Succ(TX)/TilesPerRow - PX)*Group._Texture.Ratio.X;
  V1 := (TY/TilesPerRow + PY)*Group._Texture.Ratio.Y;
  V2 := (Succ(TY)/TilesPerRow - PY)*Group._Texture.Ratio.Y;
End;

Procedure Billboard.TileRemap(X, Y, TilesPerX, TilesPerY: Integer);
Var
  SX, SY:Single;
  TX,TY:Single;
Begin
  If (Group._Texture = Nil) Then
    Exit;

  SX := (Group._Texture.Width/Group._Texture.Ratio.X) / TilesPerX;
  SY := (Group._Texture.Height/Group._Texture.Ratio.Y) / TilesPerY;
  TX := SX*X;
  TY := SY*Y;
  U1 := (TX/Group._Texture.Width*Group._Texture.Ratio.X);
  V1 := (TY/Group._Texture.Height*Group._Texture.Ratio.Y);
  U2 := (((TX+SX)-1)/Group._Texture.Width*Group._Texture.Ratio.X);
  V2 := (((TY+SY)-1)/Group._Texture.Height*Group._Texture.Ratio.Y);
End;

{ BillboardManager }
Function BillboardManager.AddBillboard(Position:Vector3D; Width, Height:Single; MyTexture:Texture):PBillboard;
Var
  N, K:Integer;
  I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I]._Texture = MyTexture) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _GroupCount;
    Inc(_GroupCount);
    If (Length(_Groups)<_GroupCount) Then
      SetLength(_Groups, _GroupCount);

    _Groups[N]._BillboardCount := 0;
    _Groups[N]._Texture := MyTexture;
  End;

  Inc(_Groups[N]._BillboardCount);
  If (Length(_Groups[N]._Billboards)<_Groups[N]._BillboardCount) Then
    SetLength(_Groups[N]._Billboards, _Groups[N]._BillboardCount);

  K := Pred(_Groups[N]._BillboardCount);
  Result := @_Groups[N]._Billboards[K];
  Result.Group := @_Groups[N];
  Result.Position := Position;
  Result.Width := Width;
  Result.Height := Height;
  Result.Color := ColorWhite;
  Result.AnchorX := 0;
  Result.AnchorY := 0;
  Result.U1 := 0.0;
  Result.V1 := 0.0;
  Result.U2 := 1.0;
  Result.V2 := 1.0;
End;

Constructor BillboardManager.Create;
Begin
End;

Destructor BillboardManager.Destroy;
Begin
  _BillboardInstance := Nil;
end;

Class Function BillboardManager.Instance: BillboardManager;
Begin
  If _BillboardInstance = Nil Then
    _BillboardInstance := BillboardManager.Create;

  Result := _BillboardInstance;
End;

Procedure BillboardManager.Render;
Const
  QuadOffsets:Array[0..3] Of Vector2D = (
    (X:-1.0; Y:1.0),
    (X: 1.0; Y:1.0),
    (X: 1.0; Y:-1.0),
    (X:-1.0; Y:-1.0)
    );
Var
  I,J, K, Ofs:Integer;
  P:Vector3D;
  CC:Color;
  Ratio, W, H:Single;
  Right, Up:Vector3D;
  PositionHandle, UVHandle, OfsHandle, SizeHandle, ColorHandle:Integer;
  U1,V1,U2,V2:Single;
Begin
  If (_GroupCount<=0) Then
    Exit;

  {If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
  Begin
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Up;
    Up.Scale(-1.0);
  End Else}
  Begin
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Up;
  End;

  Ratio := UIManager.Instance.Height / UIManager.Instance.Width;

  _Shader := ParticleManager.Instance.Shader;
  ShaderManager.Instance.Bind(_Shader);

  PositionHandle := _Shader.GetAttribute('terra_position');
  UVHandle := _Shader.GetAttribute('terra_UV0');
  OfsHandle := _Shader.GetAttribute('terra_ofs');
  SizeHandle := _Shader.GetAttribute('terra_size');
  ColorHandle := _Shader.GetAttribute('terra_color');

  If (PositionHandle<0) Then
    Exit;

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  _Shader.SetUniform('cameraUp', Up);
  _Shader.SetUniform('cameraRight', Right);
  _Shader.SetUniform('texture0', 0);
  _Shader.SetUniform('ratio', Ratio);

  GraphicsManager.Instance.SetBlendMode(blendBlend);

  //glDepthMask(False);

  For I:=0 To Pred(_GroupCount) Do
  Begin
    _Groups[I]._Texture.Bind(0);
    If (Length(_Temp)< _Groups[I]._BillboardCount * 6) Then
      SetLength(_Temp, _Groups[I]._BillboardCount * 6);

    Ofs := 0;
    For J:=0 To Pred(_Groups[I]._BillboardCount) Do
    Begin
      U1 := _Groups[I]._Billboards[J].U1;
      V1 := _Groups[I]._Billboards[J].V1;
      U2 := _Groups[I]._Billboards[J].U2;
      V2 := _Groups[I]._Billboards[J].V2;

      CC := _Groups[I]._Billboards[J].Color;
      P := _Groups[I]._Billboards[J].Position;
      W := _Groups[I]._Billboards[J].Width;
      H := _Groups[I]._Billboards[J].Height;

      {
      If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
      Begin
        _Temp[Ofs + 1].UV.X := U1;
        _Temp[Ofs + 1].UV.Y := V2;

        _Temp[Ofs + 2].UV.X := U2;
        _Temp[Ofs + 2].UV.Y := V2;

        _Temp[Ofs + 4].UV.X := U2;
        _Temp[Ofs + 4].UV.Y := V1;

        _Temp[Ofs + 0].UV.X := U1;
        _Temp[Ofs + 0].UV.Y := V1;
      End Else}
      Begin
        _Temp[Ofs + 0].UV.X := U2;
        _Temp[Ofs + 0].UV.Y := V1;

        _Temp[Ofs + 1].UV.X := U1;
        _Temp[Ofs + 1].UV.Y := V1;

        _Temp[Ofs + 2].UV.X := U1;
        _Temp[Ofs + 2].UV.Y := V2;

        _Temp[Ofs + 4].UV.X := U2;
        _Temp[Ofs + 4].UV.Y := V2;
      End;

      _Temp[Ofs + 0].Position := P;
      _Temp[Ofs + 0].Color := CC;
      _Temp[Ofs + 0].Ofs := QuadOffsets[0];
      _Temp[Ofs + 0].Size.X := W;
      _Temp[Ofs + 0].Size.Y := H;
      _Temp[Ofs + 5] := _Temp[Ofs + 0];

      _Temp[Ofs + 1].Position := P;
      _Temp[Ofs + 1].Color := CC;
      _Temp[Ofs + 1].Ofs := QuadOffsets[1];
      _Temp[Ofs + 1].Size.X := W;
      _Temp[Ofs + 1].Size.Y := H;

      _Temp[Ofs + 2].Position := P;
      _Temp[Ofs + 2].Color := CC;
      _Temp[Ofs + 2].Ofs := QuadOffsets[2];
      _Temp[Ofs + 2].Size.X := W;
      _Temp[Ofs + 2].Size.Y := H;
      _Temp[Ofs + 3] := _Temp[Ofs + 2];

      _Temp[Ofs + 4].Position := P;
      _Temp[Ofs + 4].Color := CC;
      _Temp[Ofs + 4].Ofs := QuadOffsets[3];
      _Temp[Ofs + 4].Size.X := W;
      _Temp[Ofs + 4].Size.Y := H;

      For K :=0 To 5 Do
      Begin
        _Temp[Ofs + K].Ofs.X := _Temp[Ofs + K].Ofs.X + _Groups[I]._Billboards[J].AnchorX;
        _Temp[Ofs + K].Ofs.Y := _Temp[Ofs + K].Ofs.Y + _Groups[I]._Billboards[J].AnchorY;
      End;

      Inc(Ofs, 6);
    End;

    glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, 40, @(_Temp[0].Position));
    glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, 40, @(_Temp[0].UV));
    glVertexAttribPointer(OfsHandle, 2, GL_FLOAT, False, 40, @(_Temp[0].Ofs));
    glVertexAttribPointer(SizeHandle, 2, GL_FLOAT, False, 40, @(_Temp[0].Size));
    glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, 40, @(_Temp[0].Color));

    glDrawArrays(GL_TRIANGLES, 0, _Groups[I]._BillboardCount * 6);
    GraphicsManager.Instance.Internal(0, _Groups[I]._BillboardCount * 2);
  End;

  _GroupCount := 0;
  glDepthMask(True);

  GraphicsManager.Instance.SetBlendMode(blendNone);
End;

Initialization
Finalization
  If Assigned(_BillboardInstance) Then
    _BillboardInstance.Destroy;
End.
