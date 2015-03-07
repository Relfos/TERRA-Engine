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
 * TERRA_MetaCollection
 * Implements metaballs and other volumetric shapes 
 ***********************************************************************************************************************
}
Unit TERRA_MetaCollection;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Vector3D, TERRA_Matrix, TERRA_Color, TERRA_BoundingBox, TERRA_GraphicsManager,
  TERRA_Texture, TERRA_Plane, TERRA_XML, TERRA_Image, TERRA_DebugDraw;

Const
  UVScale = 0.25;

Type
  MetaCollection = Class;

  MetaObject = Class(XMLElement)
    Owner:MetaCollection;
    Position:Vector3D;

    Function Evaluate(P:Vector3D):Single; Virtual; Abstract;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaBall = Class(MetaObject)
    Radius:Single;

    Constructor Create(P:Vector3D; Radius:Single);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaBox = Class(MetaObject)
    Width:Single;
    Height:Single;
    Depth:Single;

    Constructor Create(P:Vector3D; X,Y,Z:Single);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaCylinder = Class(MetaObject)
    Radius:Single;
    Height:Single;

    Constructor Create(P:Vector3D; Radius, Height:Single);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaTorus = Class(MetaObject)
    OuterRadius:Single;
    InnerRadius:Single;

    Constructor Create(P:Vector3D; Inner, Outer:Single);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaPlane = Class(MetaObject)
    Normal:Vector3D;

    Constructor Create(P, Normal:Vector3D);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  MetaTerrain = Class(MetaObject)
    Protected
      Source:Image;

    Public
      FileName:TERRAString;
      Scale:Single;
      Offset:Single;

    Constructor Create(FileName:TERRAString; Scale, Offset:Single);
    Function Evaluate(P:Vector3D):Single; Override;

    Procedure XMLRegisterStructure; Override;
  End;

  PGridPoint = ^GridPoint;
  GridPoint = Record
    P:Vector3D;
    Value:Single;  // Result of the metaball equations at this point
  End;

  GridCube = record
    GridPoint:Array [0..7] of PGridPoint; // Points to 8 grid points (cube)
  End;

  MetaVertex = Record
    Position:Vector3D;
    Color:TERRA_Color.Color;
  End;

  MetaCollection = Class(Renderable)
    Protected
      _Size:Vector3D;
      _Limits:BoundingBox;
      _BoundingBox:BoundingBox;

      _Vertices:Array Of MetaVertex;
      _VertexCount:Integer;

      _Triangles:Array Of Triangle;
      _TriangleCount:Integer;

      _MetaList:Array Of MetaObject;
      _MetaCount:Integer;

      _Update:Boolean;
      _GridSizeX:Integer;
      _GridSizeY:Integer;
      _GridSizeZ:Integer;
      _Grid:Array Of Array Of Array Of GridPoint;  // for this demo set max gridsize = 50
      _Cubes:Array Of Array Of Array Of GridCube;

      Procedure InitGrid;
      Procedure CreateCubeTriangles(const GridCube : GridCube);

      Function AddVertex(P:Vector3D):Word;
      Procedure AddTriangle(A,B,C:Integer);

      Function XMLNewElement(Name:TERRAString):XMLElement; Override;
      Function XMLGetElement(Index:Integer):XMLElement; Override;
      Function XMLGetElementCount():Integer; Override;

      Procedure UpdateGeometry;

    Public
      Constructor Create(ResX, ResY, ResZ:Integer; Size:Vector3D);

      Procedure SetResolution(ResX, ResY, ResZ:Integer);

      Procedure AddMetaObject(Obj:MetaObject);

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render; Override;

      Procedure ExportMesh(ExportFileName:TERRAString);

      Procedure DeleteLastObject;

      Property TriangleCount:Integer Read _TriangleCount;
      Property ObjectCount:Integer Read _MetaCount;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Stream, TERRA_FileStream, TERRA_FileUtils, TERRA_MS3D,
  TERRA_Shader, TERRA_ResourceManager;

{$I marchingcubes.inc}

Constructor MetaCollection.Create(ResX, ResY, ResZ:Integer; Size:Vector3D);
Begin
  Self._Size := Size;
  SetResolution(ResX, ResY, ResZ);

  If FileStream.Exists('meta.xml') Then
  Begin
    Self.XMLLoad('meta.xml');
  End Else
  Begin
    Self.AddMetaObject(MetaBall.Create(VectorCreate(0, 0, 0), 10));
    Self.AddMetaObject(MetaBall.Create(VectorCreate(10, 10, 10), 10));

    //Self.AddMetaObject(MetaPlane.Create(VectorCreate(0, 0, 0), VectorCreate(0, -1, 0)));
    Self.AddMetaObject(MetaTerrain.Create('terrain.png', 0.25, 0.5));
  End;
End;

Function Interpolate(const C1, C2 : Vector3D; Val1, Val2 : Single) : Vector3D;
var
  mu : Single;
begin
  if Abs(Val1) = 1 then
    Result := C1
  else
  if Abs(Val2) = 1 then
    Result := C2
  else
  if Val1 = Val2 then
    Result := C1
  else
  begin
    mu := (1 - Val1) / (Val2 - Val1);
    Result.x := C1.x + mu * (C2.x - C1.x);
    Result.y := C1.y + mu * (C2.y - C1.y);
    Result.z := C1.z + mu * (C2.z - C1.z);
  end;
end;


Procedure MetaCollection.InitGrid;
Var
  cx, cy, cz : Integer;
Begin
  // Create the grid positions
  for cx := 0 to _GridSizeX do
    for cy := 0 to _GridSizeY do
      for cz := 0 to _GridSizeZ do
      begin
        _Grid[cx, cy, cz].P.X := (cx/_GridSizeX);   // grid from -1 to 1
        _Grid[cx, cy, cz].P.Y := (cY/_GridSizeY);   // grid from -1 to 1
        _Grid[cx, cy, cz].P.Z := (cZ/_GridSizeZ);    // grid from -1 to 1
        _Grid[cx, cy, cz].P := VectorMultiply(_Grid[cx, cy, cz].P, _Size);
        _Grid[cx, cy, cz].P.Add(VectorScale(_Size, -0.5));
      end;

  // Create the cubes. Each cube points to 8 grid points
  for cx := 0 to _GridSizeX-1 do
    for cy := 0 to _GridSizeY-1 do
      for cz := 0 to _GridSizeZ-1 do
      begin
        _Cubes[cx,cy,cz].GridPoint[0] := @_Grid[cx,   cy,   cz  ];
        _Cubes[cx,cy,cz].GridPoint[1] := @_Grid[cx+1, cy,   cz  ];
        _Cubes[cx,cy,cz].GridPoint[2] := @_Grid[cx+1, cy,   cz+1];
        _Cubes[cx,cy,cz].GridPoint[3] := @_Grid[cx,   cy,   cz+1];
        _Cubes[cx,cy,cz].GridPoint[4] := @_Grid[cx,   cy+1, cz  ];
        _Cubes[cx,cy,cz].GridPoint[5] := @_Grid[cx+1, cy+1, cz  ];
        _Cubes[cx,cy,cz].GridPoint[6] := @_Grid[cx+1, cy+1, cz+1];
        _Cubes[cx,cy,cz].GridPoint[7] := @_Grid[cx,   cy+1, cz+1];
      end;
End;

Procedure MetaCollection.CreateCubeTriangles(const GridCube : GridCube);
var
  I : Integer;
  CubeIndex: Integer;
  VertList : Array[0..11] of Vector3D;
  A,B,C:Integer;
Begin
  // Determine the index into the edge table which tells
  // us which vertices are inside/outside the metaballs
  CubeIndex := 0;
  if GridCube.GridPoint[0]^.Value <1 then CubeIndex := CubeIndex or 1;
  if GridCube.GridPoint[1]^.Value <1 then CubeIndex := CubeIndex or 2;
  if GridCube.GridPoint[2]^.Value <1 then CubeIndex := CubeIndex or 4;
  if GridCube.GridPoint[3]^.Value <1 then CubeIndex := CubeIndex or 8;
  if GridCube.GridPoint[4]^.Value <1 then CubeIndex := CubeIndex or 16;
  if GridCube.GridPoint[5]^.Value <1 then CubeIndex := CubeIndex or 32;
  if GridCube.GridPoint[6]^.Value <1 then CubeIndex := CubeIndex or 64;
  if GridCube.GridPoint[7]^.Value <1 then CubeIndex := CubeIndex or 128;

  // Check if the cube is entirely in/out of the surface
  if edgeTable[CubeIndex] = 0 then
    Exit;

  // Find the vertices where the surface intersects the cube.
  with GridCube do
  begin
    if (edgeTable[CubeIndex] and 1) <> 0 then
      VertList[0] := Interpolate(GridPoint[0]^.P, GridPoint[1]^.P, GridPoint[0]^.Value, GridPoint[1]^.Value);
    if (edgeTable[CubeIndex] and 2) <> 0 then
      VertList[1] := Interpolate(GridPoint[1]^.P, GridPoint[2]^.P, GridPoint[1]^.Value, GridPoint[2]^.Value);
    if (edgeTable[CubeIndex] and 4) <> 0 then
      VertList[2] := Interpolate(GridPoint[2]^.P, GridPoint[3]^.P, GridPoint[2]^.Value, GridPoint[3]^.Value);
    if (edgeTable[CubeIndex] and 8) <> 0 then
      VertList[3] := Interpolate(GridPoint[3]^.P, GridPoint[0]^.P, GridPoint[3]^.Value, GridPoint[0]^.Value);
    if (edgeTable[CubeIndex] and 16) <> 0 then
      VertList[4] := Interpolate(GridPoint[4]^.P, GridPoint[5]^.P, GridPoint[4]^.Value, GridPoint[5]^.Value);
    if (edgeTable[CubeIndex] and 32) <> 0 then
      VertList[5] := Interpolate(GridPoint[5]^.P, GridPoint[6]^.P, GridPoint[5]^.Value, GridPoint[6]^.Value);
    if (edgeTable[CubeIndex] and 64) <> 0 then
      VertList[6] := Interpolate(GridPoint[6]^.P, GridPoint[7]^.P, GridPoint[6]^.Value, GridPoint[7]^.Value);
    if (edgeTable[CubeIndex] and 128) <> 0 then
      VertList[7] := Interpolate(GridPoint[7]^.P, GridPoint[4]^.P, GridPoint[7]^.Value, GridPoint[4]^.Value);
    if (edgeTable[CubeIndex] and 256) <> 0 then
      VertList[8] := Interpolate(GridPoint[0]^.P, GridPoint[4]^.P, GridPoint[0]^.Value, GridPoint[4]^.Value);
    if (edgeTable[CubeIndex] and 512) <> 0 then
      VertList[9] := Interpolate(GridPoint[1]^.P, GridPoint[5]^.P, GridPoint[1]^.Value, GridPoint[5]^.Value);
    if (edgeTable[CubeIndex] and 1024) <> 0 then
      VertList[10] := Interpolate(GridPoint[2]^.P, GridPoint[6]^.P, GridPoint[2]^.Value, GridPoint[6]^.Value);
    if (edgeTable[CubeIndex] and 2048) <> 0 then
      VertList[11] := Interpolate(GridPoint[3]^.P, GridPoint[7]^.P, GridPoint[3]^.Value, GridPoint[7]^.Value);
  end;

  // Draw the triangles for this cube
  I := 0;
  While TriangleTable[CubeIndex, i] <> -1 do
  Begin
    A := AddVertex(VertList[TriangleTable[CubeIndex][i]]);
    B := AddVertex(VertList[TriangleTable[CubeIndex][i+1]]);
    C := AddVertex(VertList[TriangleTable[CubeIndex][i+2]]);

    AddTriangle(A,B,C);
    Inc(I, 3);
  End;
End;

Procedure MetaCollection.AddTriangle(A, B, C: Integer);
Begin
  If (_TriangleCount>65350) Then
    Exit;

  Inc(_TriangleCount);
  SetLength(_Triangles, _TriangleCount);
  _Triangles[Pred(_TriangleCount)].A := C;
  _Triangles[Pred(_TriangleCount)].B := B;
  _Triangles[Pred(_TriangleCount)].C := A;
End;

Function MetaCollection.AddVertex(P: Vector3D): Word;
Var
  C:Single;
Begin
  If (_VertexCount>65350) Then
  Begin
    Result := 0;
    Exit;
  End;

  _BoundingBox.Add(P);
  Result := _VertexCount;
  Inc(_VertexCount);
  SetLength(_Vertices, _VertexCount);
  _Vertices[Pred(_VertexCount)].Position := P;
  P.Normalize;
  _Vertices[Pred(_VertexCount)].Color := ColorCreate(P.X * 0.5 + 0.5, P.Y * 0.5 + 0.5, P.Z * 0.5 + 0.5, 1.0);
End;

Function MetaCollection.GetBoundingBox: BoundingBox;
Begin
  If _Update Then
    Self.UpdateGeometry;

  //Result := _BoundingBox;
  Result := _Limits;
End;

Procedure MetaCollection.Render;
Var
  I,J:Integer;
  P:Vector3D;
  C:Color;
  M:Matrix;
Begin
  If _Update Then
    Self.UpdateGeometry;

  DrawBoundingBox(_Limits, ColorWhite);
  If _TriangleCount<0 Then
    Exit;

  glUseProgram(0);
  glMatrixMode(GL_MODELVIEW);
  M := GraphicsManager.Instance.ActiveViewport.Camera.Matrix;
  glLoadMatrixf(@M);

  glMatrixMode(GL_PROJECTION);
  M := GraphicsManager.Instance.ActiveViewport.Projection;
  glLoadMatrixf(@M);

  glDisable(GL_LIGHTING);
  glDisable(GL_FOG);
  glDisable(GL_TEXTURE_2D);

  glBegin(GL_TRIANGLES);
  For I:=0 To Pred(_TriangleCount) Do
  For J:=0 To 2 Do
  Begin
    P := _Vertices[_Triangles[I].Indices[J]].Position;
    C := _Vertices[_Triangles[I].Indices[J]].Color;
    glColor3ub(C.R, C.G, C.B);
    glVertex3f(P.X, P.Y, P.Z);
  End;
  glEnd();

  DrawBoundingBox(_BoundingBox, ColorWhite);
End;

Procedure MetaCollection.UpdateGeometry;
Var
  I:Integer;
  cx, cy, cz : Integer;
Begin
  _Update := False;
  _TriangleCount := 0;
  _VertexCount := 0;
  Self.InitGrid;

  _Limits.StartVertex := _Grid[0,0,0].P;
  _Limits.EndVertex := _Grid[_GridSizeX, _GridSizeY, _GridSizeZ].P;

  For cx := 0 to _GridSizeX do
    For cy := 0 to _GridSizeY do
      For cz := 0 to _GridSizeZ do
        with _Grid[cx, cy, cz] do
        begin
          If (Cy>Trunc(_GridSizeY *0.9)) Then
            Value :=1
          Else
            Value :=0;
          for I :=0 to Pred(_MetaCount) do  // go through all meta balls
            Value := Value + _MetaList[I].Evaluate(P);
        end;

    For cx := 0 to _GridSizeX-1 do
      for cy := 0 to _GridSizeY-1 do
        for cz := 0 to _GridSizeZ-1 do
          CreateCubeTriangles(_Cubes[cx, cy, cz]);

  //Self.ExportMesh('export.ms3d');
End;

Procedure MetaCollection.ExportMesh(ExportFileName:TERRAString);
Var
  MS3d:Milkshape3DObject;
  Dest:FileStream;
  I,J:Integer;
  N:Vector3D;
Begin
  If _Update Then
    Self.UpdateGeometry;

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
  SetLength(Ms3d.Groups, 1);
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

    N := TriangleNormal(_Vertices[_Triangles[J].A].Position, _Vertices[_Triangles[J].B].Position, _Vertices[_Triangles[J].C].Position);
    If (Abs(N.X)>Abs(N.Y)) And (Abs(N.X)>Abs(N.Z)) Then
    Begin
      For I:=1 To 3 Do
      Begin
        ms3d.Triangles[J].S[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.Z;
        ms3d.Triangles[J].T[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.Y;
      End;
    End Else
    If (Abs(N.Y)>Abs(N.X)) And (Abs(N.Y)>Abs(N.Z)) Then
    Begin
      For I:=1 To 3 Do
      Begin
        ms3d.Triangles[J].S[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.X;
        ms3d.Triangles[J].T[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.Z;
      End;
    End Else
    Begin
      For I:=1 To 3 Do
      Begin
        ms3d.Triangles[J].S[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.X;
        ms3d.Triangles[J].T[I] := _Vertices[_Triangles[J].Indices[Pred(I)]].Position.Y;
      End;
    End;

    For I:=1 To 3 Do
    Begin
      ms3d.Triangles[J].S[I] := ms3d.Triangles[J].S[I] * UVScale;
      ms3d.Triangles[J].T[I] := ms3d.Triangles[J].T[I] * UVScale;
    End;
  End;

  Ms3d.Groups[0].NumTriangles := ms3d.NumTriangles;
  SetLength(Ms3d.Groups[0].TriangleIndices, ms3d.NumTriangles);
  For I:=0 To Pred(ms3d.NumTriangles) Do
    Ms3d.Groups[0].TriangleIndices[I] := I;

  Dest := FileStream.Create(ExportFileName);
  Ms3d.Save(Dest);
  Dest.Release;
End;

Procedure MetaCollection.AddMetaObject(Obj:MetaObject);
Begin
  Inc(_MetaCount);
  SetLength(_MetaList, _MetaCount);
  _MetaList[Pred(_MetaCount)] := Obj;
  Obj.Owner := Self;
  _Update := True;
End;

{ MetaBall }

Constructor MetaBall.Create(P: Vector3D; Radius: Single);
Begin
  Self.Position := P;
  Self.Radius := Radius;
End;

Function MetaBall.Evaluate(P: Vector3D):Single;
Var
  Dist:Vector3D;
begin
  Dist := VectorSubtract(P, Position);
  Result := Sqr(Radius) /(Sqr(Dist.X)  + Sqr(Dist.y) + Sqr(Dist.z));
end;

Procedure MetaBall.XMLRegisterStructure;
Begin
  Inherited;
  Self.XMLRegisterElement('radius', @Radius, xmlSingle);
End;

{ MetaBox }

Constructor MetaBox.Create(P: Vector3D; X, Y, Z: Single);
Begin
  Self.Position := P;
  Self.Width := X;
  Self.Height := Y;
  Self.Depth := Z;
End;

Function MetaBox.Evaluate(P: Vector3D): Single;
Var
  Dist:Vector3D;
begin
  Dist := VectorSubtract(P, Position);
  If (Abs(Dist.X)<=Width) And (Abs(Dist.Y)<=Height) And (Abs(Dist.Z)<=Depth) Then
    Result := 1
  Else
    Result :=0;
End;

Procedure MetaBox.XMLRegisterStructure;
Begin
  Inherited;
  Self.XMLRegisterElement('width', @Width, xmlSingle);
  Self.XMLRegisterElement('height', @Height, xmlSingle);
  Self.XMLRegisterElement('depth', @Depth, xmlSingle);
End;

{ MetaCylinder }

Constructor MetaCylinder.Create(P: Vector3D; Radius, Height: Single);
Begin
  Self.Position := P;
  Self.Radius := Radius;
  Self.Height := Height;
End;

Function MetaCylinder.Evaluate(P: Vector3D): Single;
Var
  Dist:Vector3D;
Begin
  Dist := VectorSubtract(P, Position);

  If (Abs(Dist.Y)<=Height) Then
  Begin
    Result := Sqr(Radius) - (Sqr(Dist.X) + Sqr(Dist.Z));
  End Else
    Result := 0;
End;

Procedure MetaCylinder.XMLRegisterStructure;
Begin
  Inherited;
  Self.XMLRegisterElement('radius', @Radius, xmlSingle);
  Self.XMLRegisterElement('height', @Height, xmlSingle);
End;

{ MetaTorus }

Constructor MetaTorus.Create(P: Vector3D; Inner, Outer: Single);
Begin
  Self.Position := P;
  Self.OuterRadius := Outer;
  Self.InnerRadius := Inner;
End;

Function MetaTorus.Evaluate(P: Vector3D): Single;
Var
  Dist:Vector3D;
Begin
  Dist := VectorSubtract(P, Position);

  Result := (Sqr(InnerRadius) /( Sqr((OuterRadius)- Sqrt(Sqr(Dist.X) + Sqr(Dist.Y))) + Sqr(Dist.Z)));
End;

Procedure MetaTorus.XMLRegisterStructure;
Begin
  Inherited;
  Self.XMLRegisterElement('innerradius', @InnerRadius, xmlSingle);
  Self.XMLRegisterElement('outerradius', @OuterRadius, xmlSingle);
End;

{ MetaTerrain }
Constructor MetaTerrain.Create(FileName:TERRAString; Scale, Offset:Single);
Begin
  Self.Scale := Scale;
  Self.Offset := Offset;
  Self.FileName := FileName;
End;

Function MetaTerrain.Evaluate(P: Vector3D): Single;
Var
  X,Z,H:Single;
Begin
  If Source = Nil Then
    Source := Image.Create(FileName);

  X := (P.X - Owner._Limits.StartVertex.X)/(Owner._Limits.EndVertex.X - Owner._Limits.StartVertex.X);
  Z := (P.Z - Owner._Limits.StartVertex.Z)/(Owner._Limits.EndVertex.Z - Owner._Limits.StartVertex.Z);

  H := Source.GetPixel(Trunc(X*Source.Width), Trunc(Z*Source.Height)).R / 255;
  H := H * (Owner._Limits.EndVertex.Y - Owner._Limits.StartVertex.Y) * Scale + Owner._Limits.StartVertex.Y;
  Result := H-P.Y+((Owner._Limits.EndVertex.Y - Owner._Limits.StartVertex.Y)*Offset);
End;

procedure MetaTerrain.XMLRegisterStructure;
Begin
  Inherited;
  Self.XMLRegisterElement('filename', @FileName, xmlString);
End;


Function MetaCollection.XMLGetElement(Index: Integer): XMLElement;
Begin
  Result := _MetaList[Index];
End;

Function MetaCollection.XMLGetElementCount(): Integer;
Begin
  Result := _MetaCount;
End;

Function MetaCollection.XMLNewElement(Name:TERRAString): XMLElement;
Begin
  If Name = 'MetaBall' Then
    Result := MetaBall.Create(VectorZero, 1)
  Else
  If Name = 'MetaBox' Then
    Result := MetaBox.Create(VectorZero, 1, 1, 1)
  Else
  If Name = 'MetaCylinder' Then
    Result := MetaCylinder.Create(VectorZero, 1, 1)
  Else
  If Name = 'MetaTorus' Then
    Result := MetaTorus.Create(VectorZero, 1, 1)
  Else
    Result := Nil;

  If Assigned(Result) Then
    Self.AddMetaObject(MetaObject(Result));
End;

{ MetaObject }

Procedure MetaObject.XMLRegisterStructure;
Begin
  Self.XMLRegisterElement('position', @Position, xmlVector);
End;

Procedure MetaCollection.SetResolution(ResX, ResY, ResZ: Integer);
Begin
  Self._GridSizeX := ResX;
  Self._GridSizeY := ResY;
  Self._GridSizeZ := ResZ;

  SetLength(_Grid, ResX+1, ResY+1, ResZ+1);
  SetLength(_Cubes, ResX, ResY, ResZ);

  _Update := True;
End;

{ MetaPlane }

Constructor MetaPlane.Create(P, Normal: Vector3D);
Begin
  Self.Position := P;
  Self.Normal := Normal;
End;

Function MetaPlane.Evaluate(P: Vector3D): Single;
Var
  MP:Plane;
Begin
  MP := PlaneCreate(Position, Normal);
  Result := MP.Distance(P);
End;

Procedure MetaPlane.XMLRegisterStructure;
Begin
  inherited;
  Self.XMLRegisterElement('normal', @Normal, xmlVector);
End;

procedure MetaCollection.DeleteLastObject;
begin
  If _MetaCount<=0 Then
    Exit;

  _MetaList[Pred(_MetaCount)].Release;
  Dec(_MetaCount);
  _Update := True;
end;

End.
