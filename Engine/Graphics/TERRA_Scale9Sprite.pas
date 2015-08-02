Unit TERRA_Scale9Sprite;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_SpriteManager, TERRA_Texture, TERRA_Color, TERRA_Vector2D;

Type
  Scale9Tile = Record
    X:Single;
    Y:Single;
    Width:Single;
    Height:Single;

    U1:Single;
    V1:Single;

    U2:Single;
    V2:Single;
  End;

  Scale9Sprite = Class(TERRASprite)
    Protected
      _BaseColor:Color;
      _Position:Vector2D;
      _Width:Integer;
      _Height:Integer;

      _Tiles:Array[0..2, 0..2] Of Scale9Tile;

      Function GetTile(I,J:Integer):Scale9Tile;

      Procedure Rebuild(); Override;

    Public
      Constructor Create();

      Procedure SetPosition(Const Pos:Vector2D);
      Procedure SetSize(Const Width, Height:Integer);
      Procedure SetUVRect(Const U1, V1, U2, V2:Single);
      Procedure SetColor(Const C:Color); Override;

  End;

Implementation

{ Scale9Sprite }
Constructor Scale9Sprite.Create;
Begin
  _BaseColor := ColorWhite;
End;

Procedure Scale9Sprite.SetUVRect(Const U1, V1, U2, V2:Single);
Var
  I, J:Integer;
Begin
  For J:=0 To 2 Do
    For I:=0 To 2 Do
    Begin
      Case I Of
      0:Begin
          _Tiles[I, J].U1 := 0.0;
          _Tiles[I, J].U2 := U1;
        End;
      1:Begin
          _Tiles[I, J].U1 := U1;
          _Tiles[I, J].U2 := U2;
        End;
      2:Begin
          _Tiles[I, J].U1 := U2;
          _Tiles[I, J].U2 := 1.0;
        End;
      End;

      Case J Of
      0:Begin
          _Tiles[I, J].V1 := 0.0;
          _Tiles[I, J].V2 := V1;
        End;
      1:Begin
          _Tiles[I, J].V1 := V1;
          _Tiles[I, J].V2 := V2;
        End;
      2:Begin
          _Tiles[I, J].V1 := V2;
          _Tiles[I, J].V2 := 1.0;
        End;
      End;
    End;

  For J:=0 To 2 Do
    For I:=0 To 2 Do
    Begin
      _Tiles[I, J].X := _Tiles[I, J].U1 * _Texture.Width;
      _Tiles[I, J].Y := _Tiles[I, J].V1 * _Texture.Height;

      _Tiles[I, J].Width := ((_Tiles[I, J].U2 * _Texture.Width) - _Tiles[I, J].X);
      _Tiles[I, J].Height := ((_Tiles[I, J].V2 * _Texture.Height) - _Tiles[I, J].Y);
    End;
End;

Procedure Scale9Sprite.SetPosition(const Pos: Vector2D);
Begin
  _Position := Pos;
End;

Function Scale9Sprite.GetTile(I, J: Integer): Scale9Tile;
Begin
  Result := _Tiles[I,J];
End;

Procedure Scale9Sprite.SetColor(const C: Color);
Begin
  _BaseColor := C;
End;

Procedure Scale9Sprite.SetSize(const Width, Height:Integer);
Begin
  _Width := Width;
  _Height := Height;
End;

Procedure Scale9Sprite.Rebuild;
Var
  CompScaleX, CompScaleY, CompSizeX, CompSizeY, CompX, CompY:Single;
  I,J, CountX, CountY:Integer;
  LX,LY:Single;
  NW, NH:Single;
  BottomX, BottomY:Single;
  T:Scale9Tile;
  Offset:Integer;
Begin
  If _Vertices = Nil Then
    _Vertices := CreateSpriteVertexData(6);

  Offset := 0;
  LX := _Width - (Self.GetTile(0, 0).Width + Self.GetTile(2, 0).Width);
  LY := _Height - (Self.GetTile(0, 0).Height + Self.GetTile(0, 2).Height);

  T := Self.GetTile(1, 1);
  CompSizeX := T.Width;
  CountX := Trunc(LX / CompSizeX);
  If CountX<=0 Then
  Begin
    CountX := 1;
    CompScaleX := LX/CompSizeX;
  End Else
  Begin
    NW := CountX * CompSizeX;
    CompScaleX := LX/NW;
  End;
  CompSizeX := Trunc(CompSizeX * CompScaleX);

  CompSizeY := Self.GetTile(1, 1).Height;
  CountY := Trunc(LY / CompSizeY);
  If CountY<=0 Then
  Begin
    CountY := 1;
    CompScaleY := LY/CompSizeY;
  End Else
  Begin
    NH := CountY * CompSizeY;
    CompScaleY := LY/NH;
  End;
  CompSizeY := Trunc(CompSizeY * CompScaleY);

  CompX := Self.GetTile(0, 0).Width;
  For I:=1 To CountX Do
    CompX := CompX + CompSizeX;
  BottomX := CompX;

  CompY := Self.GetTile(0, 0).Height;
  For I:=1 To CountY Do
    CompY := CompY + CompSizeY;
  BottomY := CompY;

  CompX := Self.GetTile(0, 0).Width;
  For I:=1 To CountX Do
  Begin
    T := Self.GetTile(1, 0);
    Self.MakeQuad(Offset, VectorCreate2D(_Position.X + CompX, _Position.Y), T.U1, T.V1, T.U2, T.V2, CompSizeX, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

    T := Self.GetTile(1, 2);
    Self.MakeQuad(Offset,  VectorCreate2D(_Position.X + CompX, _Position.Y + BottomY), T.U1, T.V1, T.U2, T.V2, CompSizeX, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

    CompX := CompX + CompSizeX;
  End;

  CompY := Self.GetTile(0, 0).Height;
  For I:=1 To CountY Do
  Begin
    T := Self.GetTile(0, 1);
    Self.MakeQuad(Offset,  VectorCreate2D(_Position.X, _Position.Y + CompY), T.U1, T.V1, T.U2, T.V2, T.Width, CompSizeY, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

    T := Self.GetTile(2, 1);
    Self.MakeQuad(Offset,  VectorCreate2D(_Position.X + BottomX, _Position.Y + CompY), T.U1, T.V1, T.U2, T.V2, T.Width, CompSizeY, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

    CompY := CompY + CompSizeY;
  End;

  T := Self.GetTile(0, 0);
  Self.MakeQuad(Offset,  VectorCreate2D(_Position.X, _Position.Y), T.U1, T.V1, T.U2, T.V2, T.Width, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

  T := Self.GetTile(2, 0);
  Self.MakeQuad(Offset,  VectorCreate2D(_Position.X + BottomX, _Position.Y), T.U1, T.V1, T.U2, T.V2, T.Width, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

  T := Self.GetTile(0, 2);
  Self.MakeQuad(Offset,  VectorCreate2D(_Position.X, _Position.Y + BottomY), T.U1, T.V1, T.U2, T.V2, T.Width, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

  T := Self.GetTile(2, 2);
  Self.MakeQuad(Offset,  VectorCreate2D(_Position.X + BottomX, _Position.Y + BottomY), T.U1, T.V1, T.U2, T.V2, T.Width, T.Height, _BaseColor, _BaseColor, _BaseColor, _BaseColor);

  CompY := Self.GetTile(0, 0).Height;
  For J:=1 To CountY Do
  Begin
    CompX := Self.GetTile(0, 0).Width;

    T := Self.GetTile(1, 1);
    For I:=1 To CountX Do
    Begin
      Self.MakeQuad(Offset,  VectorCreate2D(_Position.X + CompX, _Position.Y + CompY), T.U1, T.V1, T.U2, T.V2, CompSizeX, CompSizeY, _BaseColor, _BaseColor, _BaseColor, _BaseColor);
      CompX := CompX + CompSizeX;
    End;

    CompY := CompY + CompSizeY;
  End;
End;


End.
