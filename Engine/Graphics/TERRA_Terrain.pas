Unit TERRA_Terrain;

{$I terra.inc}
Interface

{$DEFINE VERTEXCOLORS}

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Math, TERRA_GraphicsManager, TERRA_Vector3D, TERRA_Matrix, TERRA_Color, TERRA_Vector2D, TERRA_Texture,
  TERRA_BoundingBox, TERRA_Image, TERRA_Ray, TERRA_Plane, TERRA_DebugRender, TERRA_MS3D, TERRA_IO, TERRA_FileIO,
  TERRA_VertexBufferObject, TERRA_ShaderFactory, TERRA_Shader, TERRA_Lights;

Const
  TileSize = 15;
  UVScale = 0.5;

  GlobalLightDir: Vector3D = (X:0.4; Y:0.5; Z:-0.1);

  terrainFlipAX     = 1;
  terrainFlipAY     = 2;
  terrainFlipBX     = 4;
  terrainFlipBY     = 8;
  terrainFlipMaskX  = 16;
  terrainFlipMaskY  = 32;

Type
  PHeightMapVertex = ^HeightMapVertex;
  HeightMapVertex = Packed Record
    Position:Vector3D;
    Normal:Vector3D;
    Color:TERRA_Color.Color;
    UV:Vector2D;
  End;

  PTerrainVertexArray = ^TerrainVertexArray;
  TerrainVertexArray=Array[0..65536]Of HeightmapVertex;

  PTerrainTile = ^TerrainTile;
  TerrainTile = Record
    Offset:Integer;
  End;

  Terrain = Class (Renderable)
    Protected
      _Width, _Height:Integer;
      _Tiles:Array Of Array Of TerrainTile;
      _Heightmap:Array Of Array Of Single;

      _Vertices:Array Of HeightMapVertex;
      _VertexCount:Integer;
      _Triangles:Array Of Triangle;
      _TriangleCount:Integer;

      _BoundingBox:BoundingBox;
      _NeedsUpdate:Boolean;

      Procedure ClearGeometry;

      Function GetTriangles:PTriangleArray;
      Function GetVertices:PTerrainVertexArray;

    Public

      _Textures:Array[0..1] Of Texture;

      Constructor Create(Width, Height:Integer);
      Destructor Destroy; Override;

      Function GetHeightAt(P:Vector3D; Var Normal:Vector3D):Single;

      Procedure GenerateGeometry;

      Function LightTest(R:Ray):Boolean;

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render; Override;

      Procedure ExportMesh(FileName:AnsiString);

      Function GetNormalAt(P:Vector3D):Vector3D;

      Function GetVertex(Index:Integer):PHeightMapVertex;
      Function GetTriangle(Index:Integer):PTriangle;

      Property VertexCount:Integer Read _VertexCount;
      Property TriangleCount:Integer Read _TriangleCount;

      Property Triangles:PTriangleArray Read GetTriangles;
      Property Vertices:PTerrainVertexArray Read GetVertices;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Procedure Terrain.ClearGeometry;
Begin
	SetLength(_Vertices, 0);
	SetLength(_Triangles, 0);
	_VertexCount := 0;
	_TriangleCount := 0;
End;

Constructor Terrain.Create(Width, Height: Integer);
Var
  I,J:Integer;
Begin
  Self._Width := Width;
  Self._Height := Height;

  SetLength(_Tiles, Width, Height);
  SetLength(_Heightmap, Succ(Width), Succ(Height));

  For J:=0 To Height Do
    For I:=0 To Width Do
      _Heightmap[I,J] := 0;

  _NeedsUpdate := True;
End;

Destructor Terrain.Destroy;
Begin
  Inherited; 
  Self.ClearGeometry;
End;

Procedure Terrain.GenerateGeometry;
Var
  I,J,K,N:Integer;
  A,B,C,D:Integer;

  {$IFNDEF TILEDTEXTURE}
  U1,V1,U2,V2:Single;
  {$ENDIF}
Begin
  ClearGeometry;
  //ClearDisplayList;

  _VertexCount := _Width * _Height * 4;
  SetLength(_Vertices, _VertexCount);

  _TriangleCount := _Width * _Height * 2;
  SetLength(_Triangles, _TriangleCount);

  N := 0;
  K := 0;

  For J:=0 To _Height Do
    For I:=0 To _Width Do
      _Heightmap[I,J] := Sin(I+J)*2;

  _BoundingBox.Reset;
  N := 0;
  For J:=0 To Pred(_Height) Do
  Begin
    For I:=0 To Pred(_Width) Do
    Begin
      _Tiles[I,J].Offset := N;

      _Vertices[N].Position := VectorCreate(I*TileSize, _HeightMap[I,J], J*TileSize);
      _Vertices[N].Color := ColorWhite;
      _Vertices[N].Normal := GetNormalAt(_Vertices[N].Position);
      A := N; Inc(N);

      _Vertices[N].Position := VectorCreate(Succ(I)*TileSize, _HeightMap[Succ(I),J], J*TileSize);
      _Vertices[N].Color := ColorWhite;
      _Vertices[N].Normal := GetNormalAt(_Vertices[N].Position);
      B := N; Inc(N);

      _Vertices[N].Position := VectorCreate(I*TileSize, _HeightMap[I,Succ(J)], Succ(J)*TileSize);
      _Vertices[N].Color := ColorWhite;
      _Vertices[N].Normal := GetNormalAt(_Vertices[N].Position);
      C := N; Inc(N);

      _Vertices[N].Position := VectorCreate(Succ(I)*TileSize, _HeightMap[Succ(I), Succ(J)], Succ(J)*TileSize);
      _Vertices[N].Color := ColorWhite;
      _Vertices[N].Normal := GetNormalAt(_Vertices[N].Position);
      D := N; Inc(N);

      _Triangles[K].A := D;
      _Triangles[K].B := B;
      _Triangles[K].C := A;
      Inc(K);
      _Triangles[K].A := A;
      _Triangles[K].B := C;
      _Triangles[K].C := D;
      Inc(K);

      U1 := I * UVScale;
      V1 := J * UVScale;
      U2 := Succ(I) * UVScale;
      V2 := Succ(J) * UVScale;

      _Vertices[_Tiles[I,J].Offset + 0].UV.X := U1;
      _Vertices[_Tiles[I,J].Offset + 0].UV.Y := V1;
      _Vertices[_Tiles[I,J].Offset + 1].UV.X := U2;
      _Vertices[_Tiles[I,J].Offset + 1].UV.Y := V1;
      _Vertices[_Tiles[I,J].Offset + 2].UV.X := U1;
      _Vertices[_Tiles[I,J].Offset + 2].UV.Y := V2;
      _Vertices[_Tiles[I,J].Offset + 3].UV.X := U2;
      _Vertices[_Tiles[I,J].Offset + 3].UV.Y := V2;
    End;
  End;
End;

Function Terrain.GetBoundingBox:BoundingBox;
Begin
  If (_NeedsUpdate) Then
    Self.GenerateGeometry;

  Result := Self._BoundingBox;
End;

Function Terrain.GetHeightAt(P:Vector3D; Var Normal:Vector3D):Single;
Var
  Tile:PTerrainTile;
  TX, TY:Integer;
  Index:Integer;
Begin
  TX := Trunc(P.X/TileSize);
  TY := Trunc(P.Z/TileSize);

  If (TX<0) Then
    TX := 0
  Else
  If (TX>=_Width) Then
    TX := Pred(_Width);

  If (TY<0) Then
    TY := 0
  Else
  If (TY>=_Height) Then
    TY := Pred(_Height);

  Tile := @(_Tiles[TX, TY]);
  Index := Tile.Offset;
  Result := GetQuadHeight(_Vertices[Index+0].Position, _Vertices[Index+1].Position, _Vertices[Index+2].Position, _Vertices[Index+3].Position, P, @Normal);
End;

Function Terrain.GetTriangle(Index: Integer): PTriangle;
Begin
  If (Index<0) Or (Index>=_TriangleCount) Then
    Result := Nil
  Else
    Result := @(_Triangles[Index]);
End;

Function Terrain.GetVertex(Index: Integer): PHeightMapVertex;
Begin
  If (Index<0) Or (Index>=_VertexCount) Then
    Result := Nil
  Else
    Result := @(_Vertices[Index]);
End;

Procedure Terrain.Render;
Var
  I:Integer;
  MyShader:Shader;
  Flags:Cardinal;
  LightCount:Integer;
  PositionHandle, UVHandle, ColorHandle, NormalHandle:Integer;
  C:Color;
Begin
  GraphicsManager.Instance.SetBlendMode(blendNone);
  Texture.Bind(_Textures[0], 0);
  Texture.Bind(_Textures[1], 1);

  Flags := 0;
  If (GraphicsManager.Instance.Settings.Fog.Enabled) Then
    Flags := Flags Or shaderFog;

  LightCount := LightManager.Instance.SortLights(VectorZero, _BoundingBox.Radius, {$IFDEF PC}6{$ELSE}2{$ENDIF});

  Flags := Flags Or shaderTriplanar;

  If (Not LightManager.Instance.Sun.Enabled) Then
    Flags := Flags Or shaderDisableSun;

  If (GraphicsManager.Instance.RenderStage = renderStagePick) Then
    Flags := Flags Or shaderOutputPick
  Else
  If GraphicsManager.Instance.RenderStage = renderStageNormal Then
    Flags := Flags Or shaderOutputNormal;

  MyShader := ShaderFactory.Instance.GetShader(Flags, LightCount);

  ShaderManager.Instance.Bind(MyShader);

  MyShader.SetUniform('modelMatrix', MatrixIdentity);
  MyShader.SetUniform('textureMatrix', MatrixIdentity);

  If (GraphicsManager.Instance.RenderStage = renderStagePick) Then
  Begin
    C := GraphicsManager.Instance.GetPickColor(Self);
    MyShader.SetUniform('pickColor', C);
  End;

  MyShader.SetUniform('diffuseMap', 0);
  MyShader.SetUniform('diffuseMap2', 1);

  MyShader.SetUniform('diffuse_color', ColorWhite);
  MyShader.SetUniform('specular_power', 0.0);
  MyShader.SetUniform('emission_factor', 0.0);
  MyShader.SetUniform('specular_color', ColorWhite);

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  PositionHandle := MyShader.GetAttribute('terra_position');
  UVHandle := MyShader.GetAttribute('terra_UV0');
  ColorHandle := MyShader.GetAttribute('terra_color');
  NormalHandle := MyShader.GetAttribute('terra_normal');

  glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(HeightMapVertex), @(_Vertices[0].Position));    
  glVertexAttribPointer(UVHandle, 3, GL_FLOAT, False, SizeOf(HeightMapVertex), @(_Vertices[0].UV));  
  glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(HeightMapVertex), @(_Vertices[0].Color));         
  glVertexAttribPointer(NormalHandle, 3, GL_FLOAT, False, SizeOf(HeightMapVertex), @(_Vertices[0].Position));    

  glDrawElements(GL_TRIANGLES, 3 * _TriangleCount, GL_UNSIGNED_SHORT, _Triangles); 
  GraphicsManager.Instance.Internal(0 , _TriangleCount);
End;

Procedure GetTileUV(ID:Byte; Var U1,V1, U2,V2:Single; TilesPerRow, TilesPerColumn:Integer; FlipX, FlipY:Boolean);
Var
  Ofs, Temp:Single;
Begin
  U1 := (1/TilesPerRow*(ID Mod TilesPerRow));
  V1 := (1/TilesPerColumn*(ID Div TilesPerRow));
  U2 := (U1+1/TilesPerRow);
  V2 := (V1+1/TilesPerColumn);

  Ofs := 1/256;
  U1 := U1 + Ofs;
  V1 := V1 + Ofs;
  U2 := U2 - Ofs;
  V2 := V2 - Ofs;

  If (FlipX) Then
  Begin
    Temp := U1;
    U1 := U2;
    U2 := Temp;
  End;

  If (FlipY) Then
  Begin
    Temp := V1;
    V1 := V2;
    V2 := Temp;
  End;
End;

Function Terrain.LightTest(R: Ray): Boolean;
Var
  J:Integer;
  V0, V1, V2:Vector3D;
  T,U,V:Single;
Begin
  For J:=0 To Pred(_TriangleCount) Do
  Begin
    V0 := _Vertices[_Triangles[J].A].Position;
    V1 := _Vertices[_Triangles[J].B].Position;
    V2 := _Vertices[_Triangles[J].C].Position;
    If (R.TriangleIntersect(V0, V1, V2, T, U, V)) And (T>0.0) Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

Function Terrain.GetVertices:PTerrainVertexArray;
Begin
  If (_VertexCount>0) Then
    Result := @(_Vertices[0])
  Else
    Result := Nil;
End;

Function Terrain.GetTriangles:PTriangleArray;
Begin
  If (_TriangleCount>0) Then
    Result := @(_Triangles[0])
  Else
    Result := Nil;
End;

procedure Terrain.ExportMesh(FileName:AnsiString);
Var
  MS3d:Milkshape3DObject;
  Dest:FileStream;
  I,J:Integer;
  N:Vector3D;
Begin
  Ms3d.NumVertices := _VertexCount;
  ms3d.NumTriangles := _TriangleCount;
  ms3d.NumGroups := 1;
  ms3d.NumMaterials := 0;
  ms3d.NumJoints := 0;
  ms3d.NumGroupComments := 0;
  ms3d.NumMaterialComments :=0;
  ms3d.NumModelComments := 0;
  ms3d.NumJointsComments := 0;
  ms3d.SubVersion := 1;
  SetLength(Ms3d.Groups, ms3d.NumGroups);
  Ms3d.Groups[0].Name[1] := 'e';
  Ms3d.Groups[0].Name[2] := 'x';
  Ms3d.Groups[0].Name[3] := 'p';
  Ms3d.Groups[0].Name[4] := 'o';
  Ms3d.Groups[0].Name[5] := 'r';
  Ms3d.Groups[0].Name[6] := 't';
  Ms3d.Groups[0].Name[7] := #0;
  Ms3d.Groups[0].MaterialIndex := -1;

  SetLength(Ms3d.Vertices, Ms3d.NumVertices);
  SetLength(Ms3d.Triangles, ms3d.NumTriangles);

  For J:=0 To Pred(_VertexCount) Do
  Begin
    ms3d.Vertices[J].Vertex := _Vertices[J].Position;
    ms3d.Vertices[J].BoneIndex := -1;
  End;

  For J:=0 To Pred(_TriangleCount) Do
  Begin
    ms3d.Triangles[J].VertexIndices[1] := _Triangles[J].A;
    ms3d.Triangles[J].VertexIndices[2] := _Triangles[J].B;
    ms3d.Triangles[J].VertexIndices[3] := _Triangles[J].C;

    ms3d.Triangles[J].SmoothingGroup := 1;
    ms3d.Triangles[J].GroupIndex := 0;
    ms3d.Triangles[J].VertexNormals[1] := _Vertices[_Triangles[J].A].Position;
    ms3d.Triangles[J].VertexNormals[2] := _Vertices[_Triangles[J].B].Position;
    ms3d.Triangles[J].VertexNormals[3] := _Vertices[_Triangles[J].C].Position;
    ms3d.Triangles[J].VertexNormals[1].Normalize;
    ms3d.Triangles[J].VertexNormals[2].Normalize;
    ms3d.Triangles[J].VertexNormals[3].Normalize;

    For I:=1 To 3 Do
    Begin
      ms3d.Triangles[J].S[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].UV.X;
      ms3d.Triangles[J].T[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].UV.Y;
    End;
  End;

  Ms3d.Groups[0].NumTriangles := ms3d.NumTriangles;
  SetLength(Ms3d.Groups[0].TriangleIndices, ms3d.NumTriangles);
  For I:=0 To Pred(ms3d.NumTriangles) Do
    Ms3d.Groups[0].TriangleIndices[I] := I;

  Dest := FileStream.Create(FileName);
  Ms3d.Save(Dest);
  Dest.Destroy;
End;

Function Terrain.GetNormalAt(P: Vector3D): Vector3D;
Begin
  Self.GetHeightAt(P, Result);
End;

End.
