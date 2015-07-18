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
 * TERRA_Billboards
 * Implements billboard rendering
 ***********************************************************************************************************************
}
Unit TERRA_Billboards;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_Color, TERRA_Vector3D, TERRA_Texture, TERRA_Renderer,
  TERRA_Mesh, TERRA_Vector2D, TERRA_ParticleRenderer, TERRA_VertexFormat, TERRA_UI, TERRA_Viewport;

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
    _Texture:TERRATexture;
  End;

  BillboardManager = Class(TERRAObject)
    Protected
      _Groups:Array Of BillboardGroup;
      _GroupCount:Integer;

      _Shader:ShaderInterface;

      _Temp:VertexData;

    Public
      Constructor Create;
      Procedure Release; Override;

      Class Function Instance:BillboardManager;


      Function AddBillboard(Position:Vector3D; Width, Height:Single; MyTexture:TERRATexture):PBillboard;

      Procedure Render(View:TERRAViewport);

  End;

Implementation
Uses TERRA_GraphicsManager;

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
Function BillboardManager.AddBillboard(Position:Vector3D; Width, Height:Single; MyTexture:TERRATexture):PBillboard;
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

Procedure BillboardManager.Release;
Begin
  _BillboardInstance := Nil;
end;

Class Function BillboardManager.Instance: BillboardManager;
Begin
  If _BillboardInstance = Nil Then
    _BillboardInstance := BillboardManager.Create;

  Result := _BillboardInstance;
End;

Procedure BillboardManager.Render(View:TERRAViewport);
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
  Offset:Vector2D;
  CC:Color;
  Ratio, W, H:Single;
  Right, Up:Vector3D;
  U1,V1,U2,V2:Single;
  Graphics:GraphicsManager;
Begin
  If (_GroupCount<=0) Then
    Exit;

  Graphics := GraphicsManager.Instance;

  {If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
  Begin
    Up := Graphics.ActiveViewport.Camera.Right;
    Right := Graphics.ActiveViewport.Camera.Up;
    Up.Scale(-1.0);
  End Else}
  Begin
    Right := View.Camera.Right;
    Up := View.Camera.Up;
  End;

  Ratio := UIManager.Instance.Height / UIManager.Instance.Width;

  _Shader := ParticleManager.Instance.Shader;

  Graphics.Renderer.BindShader(_Shader);

  View.Camera.SetupUniforms();

  _Shader.SetVec3Uniform('cameraUp', Up);
  _Shader.SetVec3Uniform('cameraRight', Right);
  _Shader.SetIntegerUniform('texture0', 0);
  _Shader.SetFloatUniform('ratio', Ratio);

  Graphics.Renderer.SetBlendMode(blendBlend);

  //glDepthMask(False);

  If (_GroupCount>0) And (_Temp = Nil) Then
    _Temp := CreateParticleVertexData(16*6);

  For I:=0 To Pred(_GroupCount) Do
  Begin
    _Groups[I]._Texture.Bind(0);


    If (_Temp.Count < _Groups[I]._BillboardCount * 6) Then
      _Temp.Resize(_Groups[I]._BillboardCount * 6);

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
        _Temp.SetVector2D(Ofs + 0, vertexUV0, VectorCreate2D(U2, V1));
        _Temp.SetVector2D(Ofs + 1, vertexUV0, VectorCreate2D(U1, V1));
        _Temp.SetVector2D(Ofs + 2, vertexUV0, VectorCreate2D(U1, V2));
        _Temp.SetVector2D(Ofs + 4, vertexUV0, VectorCreate2D(U2, V2));
      End;

      _Temp.SetVector3D(Ofs + 0, vertexPosition, P);
      _Temp.SetColor(Ofs + 0, vertexColor, CC);
      _Temp.SetVector2D(Ofs + 0, vertexOfs, QuadOffsets[0]);
      _Temp.CopyVertex(Ofs + 0, Ofs + 5);

      _Temp.SetVector3D(Ofs + 1, vertexPosition, P);
      _Temp.SetColor(Ofs + 1, vertexColor, CC);
      _Temp.SetVector2D(Ofs + 1, vertexOfs, QuadOffsets[1]);

      _Temp.SetVector3D(Ofs + 2, vertexPosition,  P);
      _Temp.SetColor(Ofs + 2, vertexColor, CC);
      _Temp.SetVector2D(Ofs + 2, vertexOfs, QuadOffsets[2]);
      _Temp.CopyVertex(Ofs + 2, Ofs + 3);

      _Temp.SetVector3D(Ofs + 4, vertexPosition, P);
      _Temp.SetColor(Ofs + 4, vertexColor, CC);
      _Temp.SetVector2D(Ofs + 4, vertexOfs, QuadOffsets[3]);

      For K :=0 To 5 Do
      Begin
        _Temp.SetVector2D(Ofs + K, vertexSize, VectorCreate2D(W, H));
        _Temp.GetVector2D(Ofs + K, vertexOfs, Offset);
        Offset.X := Offset.X + _Groups[I]._Billboards[J].AnchorX;
        Offset.Y := Offset.Y + _Groups[I]._Billboards[J].AnchorY;
        _Temp.SetVector2D(Ofs + K, vertexOfs, Offset);
      End;

      Inc(Ofs, 6);
    End;


{    Graphics.Renderer.SetSourceVertexSize(40);
    Graphics.Renderer.SetAttributeSource('terra_position', typeVector3D, @(_Temp[0].Position));
    Graphics.Renderer.SetAttributeSource('terra_UV0', typeVector2D, @(_Temp[0].UV));
    Graphics.Renderer.SetAttributeSource('terra_ofs', typeVector2D, @(_Temp[0].Ofs));
    Graphics.Renderer.SetAttributeSource('terra_size', typeVector2D, @(_Temp[0].Size));
    Graphics.Renderer.SetAttributeSource('terra_color', typeColor, @(_Temp[0].Color));}

    Graphics.Renderer.SetCullMode(cullNone);

    Graphics.Renderer.SetVertexSource(_Temp);
    Graphics.Renderer.DrawSource(renderTriangles, _Groups[I]._BillboardCount * 6);
  End;

  _GroupCount := 0;

  Graphics.Renderer.SetBlendMode(blendNone);
End;

Initialization
Finalization
  If Assigned(_BillboardInstance) Then
    _BillboardInstance.Release;
End.
