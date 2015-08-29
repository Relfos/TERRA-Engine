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
 * TERRA_Tilemap
 * Implements Tilemap rendering for TMX files (Tiled)
 ***********************************************************************************************************************
}
Unit TERRA_Tilemap;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Stream, TERRA_XML, TERRA_Utils, TERRA_Texture, TERRA_Sprite,
  TERRA_FileUtils, TERRA_Renderer, TERRA_ZLib;

Const
  TileMapExtension = 'tmx';

  MaxTileIDs = 4096;
  DefaultTileAnimationDuration = 500;

  TILE_FLIPPED_HORIZONTAL = 1 Shl 7;
  TILE_FLIPPED_VERTICAL   = 1 Shl 6;
  TILE_FLIPPED_DIAGONAL   = 1 Shl 5;

Type
  TileMap = Class;

  ObjectProperty = Record
    Key:TERRAString;
    Value:TERRAString;
  End;

  PTileObject = ^TileObject;
  TileObject = Object
    X,Y:Single;
    Width,Height:Single;
    TX,TY:Integer;

    Properties:Array Of ObjectProperty;
    PropertyCount:Integer;

    Constructor Create(PX,PY:Single; TileX, TileY:Integer; PropList:TERRAString);

    Function GetProperty(Key:TERRAString):TERRAString;
    Function HasProperty(Key:TERRAString):Boolean;
    Procedure AddProperty(Key,Value:TERRAString);
  End;

  TileExtraData = Record
    Name:TERRAString;
    Data:Array Of Array Of Byte;
  End;

  TileLayer = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Width:Integer;
      _Height:Integer;
      _TilesPerRow:Integer;
      _Data:Array Of Array Of Integer;
      _Flags:Array Of Array Of Byte;
      _Extra:Array Of TileExtraData;
      _ExtraCount:Integer;
      _Map:TileMap;

      Function DecompressString(Source, Compression:TERRAString):TERRAString;

    Public
      Visible:Boolean;

      Constructor Create(Name:TERRAString; W,H:Integer; Source, Compression:TERRAString; Map:TileMap); Overload;
      Constructor Create(P:XMLNode; Map:TileMap); Overload;

      Procedure Release; Override;

      Procedure Render(Depth:Single);

      Procedure SetTileAt(X, Y, Value:Integer);
      Function GetTileAt(X, Y:Integer):Integer;

      Procedure SetFlagAt(X, Y:Integer; Flag:Byte);
      Function GetFlagAt(X, Y:Integer):Integer;

      Function GetDataLayer(Name:TERRAString):Integer;
      Function GetDataAt(X, Y, DataLayer:Integer):Integer;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;
  End;

  TileInfo = Record
    Properties:Array Of ObjectProperty;
    PropertyCount:Integer;

    IsShadow:Boolean;
    IsTop:Boolean;

    AnimationCycle:Cardinal;
    AnimationGap:Cardinal;
    AnimationDuration:Cardinal;
  End;

  TileMap = Class(TERRAOBject)
    Protected
      _LayerCount:Integer;
      _Layers:Array Of TileLayer;
      _TileWidth:Integer;
      _TileHeight:Integer;
      _Tileset:Texture;
      _TilesetName:TERRASTring;
      _TileInfo:Array[0..Pred(MaxTileIDs)] Of TileInfo;
      _Palette:Array[0..Pred(MaxTileIDs)] Of Cardinal;

      _ObjectCount:Integer;
      _ObjectList:Array Of TileObject;

      _Properties:Array Of ObjectProperty;
      _PropertyCount:Integer;

      Function GetTileset: Texture;

    Public
      Name:TERRAString;
      CamX,CamY:Single;
      Scale:Single;

      Constructor Create(TileWidth, TileHeight:Integer); Overload;
      Procedure Release; Override;

      Function Load(Const SourceName:TERRAString):Boolean;

      Function AddLayer(Width,Height:Integer; Name:TERRAString):TileLayer;

      Procedure Render(Depth:Single);

      Function GetWidth:Integer;
      Function GetHeight:Integer;

      Procedure SetPosition(Var X,Y:Single);

      Function GetTileAt(X, Y:Integer; Layer:Integer=-1; UsePalette:Boolean = False):Integer; Cdecl;
      Function GetTileProperty(ID:Integer; Key:TERRAString):TERRAString;
      Function HasTileProperty(ID:Integer; Key:TERRAString):Boolean;

      Procedure SetLayerVisibility(Index:Integer; Value:Boolean);

      Function HasProperty(Key:TERRAString):Boolean;
      Function GetProperty(Key:TERRAString):TERRAString;

      Function GetObject(Index:Integer):PTileObject;

      Function GetLayer(Index:Integer):TileLayer; Overload;
      Function GetLayer(Const Name:TERRAString):TileLayer; Overload;

      Property Tileset:Texture Read GetTileset;

      Property ObjectCount:Integer Read _ObjectCount;
      Property LayerCount:Integer Read _LayerCount;
      Property TileWidth:Integer Read _TileWidth;
      Property TileHeight:Integer Read _TileHeight;
      Property Width:Integer Read GetWidth;
      Property Height:Integer Read GetHeight;

  End;

Implementation
Uses TERRA_UI, TERRA_OS, TERRA_FileManager, TERRA_MemoryStream, TERRA_XMLBinary, TERRA_Log;

{ TileMap }
Constructor TileMap.Create(TileWidth, TileHeight: Integer);
Begin
  Self._TileWidth := TileWidth;
  Self._TileHeight := TileHeight;
  Self._LayerCount := 0;
End;

Function TileMap.Load(Const SourceName:TERRAString):Boolean;
Var
  Doc:XMLDocument;
  Node, P, PP, PPP, PX:XMLNode;
  Name, S:TERRAString;
  I, J, K, N, W, H:Integer;
  Src:Stream;
Begin
  Result := False;
  Self.Scale := 1;
  Self.Name := GetFileName(SourceName, True);

  S := FileManager.Instance.SearchResourceFile(Self.Name+'.bin');
  If S='' Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Self.Name+'.'+TileMapExtension);
    If S='' Then
    Begin
      Log(logError, 'Tilemap', 'File not found: '+Self.Name);
      Exit;
    End;

    Src := MemoryStream.Create(S);
    Doc := XMLDocument.Create;
    Doc.Load(Src);
    ReleaseObject(Src);
  End Else
  Begin
    Doc := XMLLoadBinary(S);
  End;

  CamX := 0;
  CamY := 0;

  For I:=0 To Pred(MaxTileIDs) Do
  Begin
    _TileInfo[I].PropertyCount := 0;
    _TileInfo[I].AnimationCycle := 0;
    _Palette[I] := I;
  End;


  Node := Doc.Root;
  P := Node.GetNodeByName('properties');
  _PropertyCount := 0;
  If Assigned(P) Then
  Begin
    For I:=0 To Pred(P.NodeCount) Do
    Begin
      PP := P.GetNodeByIndex(I);
      Inc(Self._PropertyCount);
      SetLength(_Properties, _PropertyCount);
      _Properties[Pred(_PropertyCount)].Key := PP.GetNodeByName('name').Value;
      _Properties[Pred(_PropertyCount)].Value := PP.GetNodeByName('value').Value;
    End;
  End;


  P := Node.GetNodeByName('tileset');

  PP := P.GetNodeByName('tilewidth');
  _TileWidth := StringToInt(PP.Value);

  PP := P.GetNodeByName('tileheight');
  _TileHeight := StringToInt(PP.Value);

  For I:=0 To Pred(P.NodeCount) Do
  Begin
    PP := P.GetNodeByIndex(I);
    If (PP.Name<>'tile') Then
      Continue;

    N := StringToInt(PP.GetNodeByName('id').Value);
    PP := PP.GetNodeByName('properties');
    _TileInfo[N].PropertyCount := PP.NodeCount;
    SetLength(_TileInfo[N].Properties, _TileInfo[N].PropertyCount);
    For J:=0 To Pred(PP.NodeCount) Do
    Begin
      PPP := PP.GetNodeByIndex(J);
      _TileInfo[N].Properties[J].Key := PPP.GetNodeByName('name').Value;
      _TileInfo[N].Properties[J].Value := PPP.GetNodeByName('value').Value;
    End;
  End;

  For I:=0 To Pred(MaxTileIDs ) Do
  Begin
    _TileInfo[I].IsShadow := Self.HasTileProperty(I, 'shadow') ;
    _TileInfo[I].IsTop := Self.HasTileProperty(I, 'top');

    S := Self.GetTileProperty(I, 'cycle');
    If (S='') Then
      Continue;

    _TileInfo[I].AnimationCycle := StringToInt(S);
    S := Self.GetTileProperty(I, 'gap');
    _TileInfo[I].AnimationGap := StringToInt(S);
    S := Self.GetTileProperty(I, 'duration');
    If S<>'' Then
      _TileInfo[I].AnimationDuration := StringToInt(S)
    Else
      _TileInfo[I].AnimationDuration := DefaultTileAnimationDuration;
  End;

  PP := P.GetNodeByName('image');
  PPP := PP.GetNodeByName('source');
  S := GetFileName(PPP.Value, True);
  _TileSetName := S;

  {PPP := PP.GetNodeByName('height');
  _TilesPerCol := StringToInt(PPP.Value) Div _TileHeight;}

  _LayerCount := 0;
  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    P := Node.GetNodeByIndex(I);
    If (P.Name<>'layer') Then
      Continue;

    Inc(_LayerCount);
    SetLength(_Layers, _LayerCount);
    _Layers[Pred(_LayerCount)] := TileLayer.Create(P, Self);
  End;

  _ObjectCount := 0;
  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    P := Node.GetNodeByIndex(I);
    If (P.Name<>'objectgroup') Then
      Continue;

    For J:=0 To Pred(P.NodeCount) Do
    Begin
      PP := P.GetNodeByIndex(J);
      If (PP.Name<>'object') Then
        Continue;

      N := _ObjectCount;
      Inc(_ObjectCount);
      SetLength(_ObjectList, _ObjectCount);

      PPP := PP.GetNodeByName('x');
      If Assigned(PPP) Then
        _ObjectList[N].X := StringToInt(PPP.Value);

      PPP := PP.GetNodeByName('y');
      If Assigned(PPP) Then
        _ObjectList[N].Y := StringToInt(PPP.Value);

      PPP := PP.GetNodeByName('width');
      If Assigned(PPP) Then
        _ObjectList[N].Width := StringToInt(PPP.Value);

      PPP := PP.GetNodeByName('height');
      If Assigned(PPP) Then
        _ObjectList[N].Height := StringToInt(PPP.Value);

      _ObjectList[N].TX := Trunc(_ObjectList[N].X / Self._TileWidth);
      _ObjectList[N].TY := Trunc(_ObjectList[N].Y / Self._TileHeight);
      _ObjectList[N].PropertyCount := 0;

      PPP := PP.GetNodeByName('properties');
      If Assigned(PPP) Then
      Begin
        _ObjectList[N].PropertyCount := PPP.NodeCount;
        SetLength(_ObjectList[N].Properties, PPP.NodeCount);
        For K:=0 To Pred(PPP.NodeCount) Do
        Begin
          PX := PPP.GetNodeByIndex(K);
          _ObjectList[N].Properties[K].Key := PX.GetNodeByName('name').Value;
          _ObjectList[N].Properties[K].Value := PX.GetNodeByName('value').Value;
        End;
      End;
    End;
  End;

  ReleaseObject(Doc);

  Result := True;  
End;


Function TileMap.AddLayer(Width,Height:Integer; Name:TERRAString):TileLayer;
Begin
  Inc(_LayerCount);
  SetLength(_Layers, _LayerCount);
  Result := TileLayer.Create(Name, Width, Height, '', '', Self);
  _Layers[Pred(_LayerCount)] := Result;
End;

Function TileMap.GetHeight: Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_LayerCount) Do
  If (_Layers[I].Height>Result) Then
    Result := _Layers[I].Height;
End;

Function TileMap.GetWidth: Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_LayerCount) Do
  If (_Layers[I].Width>Result) Then
    Result := _Layers[I].Width;
End;

Procedure TileMap.Render(Depth:Single);
Var
  I:Integer;
  N:Cardinal;
  Z:Single;
Begin
  For I:=0 To Pred(MaxTileIDs) Do
  If (_TileInfo[I].AnimationCycle>0) Then
  Begin
    N := (Application.GetTime() Div _TileInfo[I].AnimationDuration) Mod _TileInfo[I].AnimationCycle;
    _Palette[I] := Cardinal(I) + N * _TileInfo[I].AnimationGap;
  End;

  Z := Depth;
  For I:=0 To Pred(_LayerCount) Do
  If (_Layers[I].Visible) Then
  Begin
    _Layers[I].Render(Z-I);
    SpriteManager.Instance.Flush;
  End;
End;

Procedure TileMap.SetPosition(Var X, Y: Single);
Var
  MaxX, MaxY:Single;
Begin
  If (X<0) Then X := 0;
  If (Y<0) Then Y := 0;

  MaxX := (_Layers[0].Width * _TileWidth* Scale ) - UIManager.Instance.Width;
  MaxY := (_Layers[0].Height * _TileHeight* Scale )  - UIManager.Instance.Height;

  If (X>MaxX) Then X := MaxX;
  If (Y>MaxY) Then Y := MaxY;

  CamX := X;
  CamY := Y;
End;

Procedure TileMap.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._LayerCount) Do
    ReleaseObject(_Layers[I]);
    
  Self._LayerCount := 0;
End;

Procedure TileMap.SetLayerVisibility(Index: Integer; Value: Boolean);
Begin
  If (Index>=0) And (Index<_LayerCount) Then
    _Layers[Index].Visible := Value;
End;

Function TileMap.GetObject(Index: Integer): PTileObject;
Begin
  If (Index<0) Or (Index>=_ObjectCount) Then
    Result := Nil
  Else
    Result := @_ObjectList[Index];
End;

Function TileMap.GetLayer(Index: Integer): TileLayer;
Begin
  If (Index<0) Or (Index>=_LayerCount) Then
    Result := Nil
  Else
    Result := _Layers[Index];
End;

Function TileMap.GetLayer(Const Name:TERRAString): TileLayer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_LayerCount) Do
  If (StringEquals(_Layers[I]._Name, Name)) Then
  Begin
    Result := _Layers[I];
    Exit;
  End;

  Result := Nil
End;

Function TileMap.GetTileset: Texture;
Begin
  If (_Tileset = Nil) Then
  Begin
    _Tileset := TextureManager.Instance.GetTexture(_TilesetName);
    If Assigned(_Tileset) Then
    Begin
      _Tileset.Filter := filterLinear;
      _Tileset.WrapMode := wrapNothing;
      _Tileset.MipMapped := False;
    End;
  End;

  Result := _TileSet;
End;

{ TileLayer }
Constructor TileLayer.Create(P: XMLNode; Map:TileMap);
Var
  X,Y, K :Integer;
  Name:TERRAString;
  W, H, I: Integer;
  Data, Compression:TERRAString;
  PP, PPP:XMLNode;
  B:Byte;
Begin
  PP := P.GetNodeByName('name');
  Name := PP.Value;

  PP := P.GetNodeByName('width');
  W := StringToInt(PP.Value);

  PP := P.GetNodeByName('height');
  H := StringToInt(PP.Value);

  PP := P.GetNodeByName('data');

  PPP := PP.GetNodeByName('compression');
  If Assigned(PPP) Then
    Compression := PPP.Value
  Else
    Compression := '';

  Self.Create(Name, W, H, PP.Value, Compression, Map);


  For I:=0 To Pred(P.NodeCount) Do
  Begin
    PP :=P.GetNodeByIndex(I);
    If PP.Name<>'extra' Then
      Continue;

    Inc(Self._ExtraCount);
    SetLength(Self._Extra, Self._ExtraCount);

    PPP := PP.GetNodeByName('name');
    Self._Extra[Pred(Self._ExtraCount)].Name := PPP.Value;

    Data := PP.Value;

    PPP := PP.GetNodeByName('compression');
    If Assigned(PPP) Then
      Compression := PPP.Value
    Else
      Compression := '';

    Data := Self.DecompressString(Data, Compression);
    SetLength(Self._Extra[Pred(Self._ExtraCount)].Data, _Width, _Height);

    K := 1;
    X := 0;
    Y := 0;
    While (K<=W*H) Do
    Begin
      B := Byte(Data[K]);
      Self._Extra[Pred(Self._ExtraCount)].Data[X,Y] := B;
      Inc(K);
      Inc(X);
      If (X>=Width) Then
      Begin
        X := 0;
        Inc(Y);
      End;
    End;
  End;
End;

Constructor TileLayer.Create(Name:TERRAString; W, H: Integer; Source, Compression:TERRAString; Map:TileMap);
Var
  X,Y, K :Integer;
  A,B,C,D:Byte;
  T:Cardinal;
Begin
  Self.Visible := True;
  Self._Map := Map;
  Self._Name := Name;
  Self._ExtraCount := 0;
  Self._Width := W;
  Self._Height := H;
  Self._TilesPerRow := W Div Map._TileWidth;
  SetLength(_Data, W, H);
  SetLength(_Flags, W, H);

  If (Source='') Then
  Begin
    Log(logError, 'Tilemap', 'Error creating tile layer for map '+Map.Name);
    Exit;
  End;

  Source := DecompressString(Source, Compression);

  K := 0;
  X := 0;
  Y := 0;
  While (K<W*H) Do
  Begin
    A := Byte(Source[K*4+1]);
    B := Byte(Source[K*4+2]);
    C := Byte(Source[K*4+3]);
    D := Byte(Source[K*4+4]);
    _Data[X,Y] := {(D Shl 24) + }(C Shl 16) + (B Shl 8) + A;
    _Flags[X,Y] := D;

    Inc(K);
    Inc(X);
    If (X>=Width) Then
    Begin
      X := 0;
      Inc(Y);
    End;
  End;
End;

Function TileLayer.DecompressString(Source, Compression:TERRAString):TERRAString;
Var
  Mem, Dst:MemoryStream;
Begin
  Source := StringTrim(Source);
  Source := Base64ToString(Source);

  If (Source<>'') And (Compression='zlib') Then
  Begin
    Mem := MemoryStream.Create(Length(Source), @Source[1]);
    Dst := MemoryStream.Create(_Width * _Height * 8);
    zUncompress(Mem, Dst);
    SetLength(Result, Dst.Position);
    Dst.Seek(0);
    Dst.Read(@Result[1], Length(Result));
    ReleaseObject(Dst);
    ReleaseObject(Mem);
  End Else
    Result := Source;
End;

Procedure TileLayer.SetTileAt(X, Y, Value:Integer);
Begin
  If (X<0) Or (Y<0) Or (X>=Width) Or (Y>=Height) Then
    Exit;

  _Data[X,Y] := Value;
End;

function TileLayer.GetTileAt(X, Y: Integer): Integer;
Begin
  If (X<0) Or (Y<0) Or (X>=Width) Or (Y>=Height) Then
    Result := 0
  Else
    Result := _Data[X,Y];
End;

Procedure TileLayer.SetFlagAt(X, Y:Integer; Flag:Byte);
Begin
  If (X<0) Or (Y<0) Or (X>=Width) Or (Y>=Height) Then
    Exit
  Else
    _Flags[X,Y] := Flag;
End;

Function TileLayer.GetFlagAt(X, Y: Integer): Integer;
Begin
  If (X<0) Or (Y<0) Or (X>=Width) Or (Y>=Height) Then
    Result := 0
  Else
    Result := _Flags[X,Y];
End;

Procedure TileLayer.Render(Depth: Single);
Var
  X1,Y1,X2,Y2:Integer;
  I,J, N:Integer;
  Z:Single;
  S:QuadSprite;
  Tx, Ty:Integer;
Begin
  X1 := Trunc(_Map.CamX/_Map._TileWidth/_Map.Scale);
  Y1 := Trunc(_Map.CamY/_Map._TileHeight/_Map.Scale);

  X2 := Succ(X1) + Trunc((UIManager.Instance.Width/_Map._TileWidth)/_Map.Scale);
  Y2 := Succ(Y1) + Trunc((UIManager.Instance.Height/_Map._TileHeight)/_Map.Scale);

  If (X1<0) Then
    X1 := 0;
  If (X2>=Width) Then
    X2 := Pred(Width);

  If (Y1<0) Then
    Y1 := 0;
  If (Y2>=Height) Then
    Y2 := Pred(Height);

  For J:=Y1 To Y2 Do
    For I:=X1 To X2 Do
    Begin
      N := _Data[I, J];
      If (N<=0) Then
        Continue;

      Dec(N);
      N := _Map._Palette[N];
      If (_Map._TileInfo[N].IsTop) Then
        Z := Depth - 10
      Else
        Z := Depth;
      S := SpriteManager.Instance.DrawSprite(I*_Map._TileWidth*_Map.Scale - _Map.CamX, J*_Map._TileHeight*_Map.Scale - _Map.CamY, Z, _Map.Tileset);
      Tx := (N Mod _TilesPerRow);
      Ty := (N Div _TilesPerRow);
      S.Rect.PixelRemap(Tx*_Map._TileWidth, Ty*_Map._TileWidth, Succ(Tx)*_Map._TileWidth-1, Succ(Ty)*_Map._TileWidth-1, _Map.GetTileset());
      S.Rect.Width := _Map._TileWidth;
      S.Rect.Height := _Map._TileHeight;
      S.SetScale(_Map.Scale, _Map.Scale);

      S.Mirror := (_Flags[I,J] And TILE_FLIPPED_HORIZONTAL<>0);
      S.Flip := (_Flags[I,J] And TILE_FLIPPED_VERTICAL<>0);
    End;
End;

Function TileLayer.GetDataAt(X, Y, DataLayer: Integer): Integer;
Begin
  If (DataLayer<0) Or (DataLayer>=_ExtraCount) Then
    Result := 0
  Else
    Result := _Extra[DataLayer].Data[X,Y];
End;

Function TileLayer.GetDataLayer(Name:TERRAString): Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._ExtraCount) Do
  If (Self._Extra[I].Name = Name) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := -1;
End;

Procedure TileLayer.Release;
Begin
  // do nothing
End;

{ TileObject }
Procedure TileObject.AddProperty(Key, Value:TERRAString);
Begin
  If HasProperty(Key) Then
    Exit;
  Inc(PropertyCount);
  SetLength(Properties, PropertyCount);
  Properties[Pred(PropertyCount)].Key := Key;
  Properties[Pred(PropertyCount)].Value := Value;
End;

Constructor TileObject.Create(PX, PY: Single; TileX, TileY:Integer; PropList:TERRAString);
Var
  S2:TERRAString;
Begin
  Self.X := PX;
  Self.Y := PY;
  Self.TX := TileX;
  Self.TY := TileY;
  Self.PropertyCount := 0;

  While PropList<>'' Do
  Begin
    S2 := StringGetNextSplit(PropList, Ord('|'));
    Inc(Self.PropertyCount);
    SetLength(Properties, PropertyCount);
    Properties[Pred(PropertyCount)].Key := StringGetNextSplit(S2, Ord('='));
    Properties[Pred(PropertyCount)].Value := S2;
  End;
End;

Function TileObject.GetProperty(Key:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=0 To Pred(PropertyCount) Do
  If (Properties[I].Key = Key) Then
  Begin
    Result := Properties[I].Value;
    Exit;
  End;
End;

Function TileObject.HasProperty(Key:TERRAString): Boolean;
Var
  I:Integer;
Begin
  Result := False;
  For I:=0 To Pred(PropertyCount) Do
  If (Properties[I].Key = Key) Then
  Begin
    Result := True;
    Exit;
  End;
End;

Function TileMap.GetTileAt(X, Y:Integer; Layer:Integer=-1; UsePalette:Boolean = False):Integer;
Var
  I:Integer;
Begin
  Result := 0;
  If (_LayerCount<=0) Then
    Exit;

  If (X<0) Then
    X := 0;

  If (Y<0) Then
    Y := 0;

  If (X>= _Layers[0].Width) Then
    X := Pred(_Layers[0].Width);

  If (Y>= _Layers[0].Height) Then
    Y := Pred(_Layers[0].Height);

  For I:=Pred(_LayerCount) DownTo 0 Do
  If (I <> Layer) And (Layer>=0) Then
    Continue
  Else
  If (_Layers[I]._Data[X,Y]>0) Then
  Begin
    Result := _Layers[I]._Data[X,Y];
    If (UsePalette) Then
      Result := _Palette[Result];

    If (Not _TileInfo[Result].IsShadow) And (Not _TileInfo[Result].IsTop) Then
      Exit;
  End;
End;

Function TileMap.GetTileProperty(ID:Integer; Key:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  If (ID>=0) And (ID<MaxTileIDs) Then
  Begin
    For I:=0 To Pred(_TileInfo[ID].PropertyCount) Do
    If (_TileInfo[ID].Properties[I].Key = Key) Then
    Begin
      Result := _TileInfo[ID].Properties[I].Value;
      Exit;
    End;
  End;
End;

Function TileMap.HasTileProperty(ID:Integer; Key:TERRAString):Boolean;
Var
  I:Integer;
Begin
  Result := False;
  If (ID>=0) And (ID<MaxTileIDs) Then
  Begin
    For I:=0 To Pred(_TileInfo[ID].PropertyCount) Do
    If (_TileInfo[ID].Properties[I].Key = Key) Then
    Begin
      Result := True;
      Exit;
    End;
  End;
End;

Function TileMap.GetProperty(Key:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=0 To Pred(_PropertyCount) Do
  If (_Properties[I].Key = Key) Then
  Begin
    Result := _Properties[I].Value;
    Exit;
  End;
End;

Function TileMap.HasProperty(Key:TERRAString):Boolean;
Var
  I:Integer;
Begin
  Result := False;
  For I:=0 To Pred(_PropertyCount) Do
  If (_Properties[I].Key = Key) Then
  Begin
    Result := True;
    Exit;
  End;
End;


End.