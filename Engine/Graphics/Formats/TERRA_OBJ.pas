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
 * TERRA_OBJ
 * Implements a OBJ model loader/writer/meshfilter
 ***********************************************************************************************************************
}

Unit TERRA_OBJ;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Color, TERRA_Vector3D, TERRA_Vector2D, TERRA_Texture,
  TERRA_Math, TERRA_Mesh, TERRA_Resource, TERRA_MeshFilter, TERRA_OS, TERRA_FileUtils,
  TERRA_FileManager, TERRA_FileStream, TERRA_MemoryStream, TERRA_Renderer, TERRA_VertexFormat;

Type
  POBJMaterial=^OBJMaterial;
  OBJMaterial=Record        // Material Structure
    Name:AnsiString;
    Ambient:Color;
    Diffuse:Color;
    Specular:Color;
    Shininess:Single;
    DiffuseTexture:AnsiString;
    SpecularTexture:AnsiString;
  End;

  POBJFace=^OBJFace;
  OBJFace=Record
    VertexIndices:Array[0..2] Of Integer; // indexes to vertices
    UVIndices:Array[0..2] Of Integer;     // indexes to vertex textures
    NormalIndices:Array[0..2] Of Integer; // indexes to vertex normals
  End;


  MyVertex = Record
    Position:Vector3D;
    Normal:Vector3D;
    TextureCoords:Vector2D;
  End;

  POBJGroup = ^OBJGroup;
  OBJGroup = Object
    Name:AnsiString;
    FaceCount:Integer;            // Number of faces
    FaceList:Array Of OBJFace;   // The faces in the group
    MaterialIndex:Integer;          // index to Material

    Triangles:Array Of Triangle;
    Vertices:Array Of MyVertex;
    VertexCount:Integer;
  End;

  OBJVertex=Record
    Vertex:Integer;
  End;

  OBJModel = Class(MeshFilter)
    Protected
      _MaterialFile:AnsiString;

      _GroupCount:Integer;
      _GroupList:Array Of OBJGroup;

      _VertexCount:Integer;
      _VertexList:Array Of Vector3D;

      _NormalCount:Integer;
      _NormalList:Array Of Vector3D;

      _UVCount:Integer;
      _UVList:Array Of Vector2D;

      _MaterialCount:Integer;
      _MaterialList:Array Of OBJMaterial;

      Procedure ReadVertexData(S:AnsiString);
      Procedure ReadFaceData(S:AnsiString);
      Procedure GetMaterialName(S:AnsiString);
      Procedure CreateMaterial(S:AnsiString);
      Procedure ReadMaterial(S:AnsiString);
      Procedure ReadShininess(S:AnsiString);
      Procedure ReadTexture(S:AnsiString);
      Procedure LoadMaterials(S:AnsiString);

      Function GetMaterial(Index:Integer): POBJMaterial;

    Public
      Function Load(Source:Stream):Boolean; Override;
      Class Function Save(Dest:Stream; MyMesh:MeshFilter):Boolean; Override;

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):VertexFormat; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;

      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;
  End;

Implementation
Uses TERRA_Log, TERRA_ResourceManager, TERRA_GraphicsManager, SysUtils, StrUtils;

//  Gets the X, Y, Z coordinates from a String
Function GetVector(S:AnsiString):Vector3D;
Var
  P,P2:Integer;
Begin
  S:= Trim(Copy(S, 3, Length(S)));
  P:= Pos(' ', S);
  P2:= PosEx(' ', S, P+1);

  With Result Do
  Begin
    X := StringToFloat(Copy(S, 1, P-1));
    Y := StringToFloat(Copy(S, P+1, P2-P-1));
    Z := StringToFloat(Copy(S, P2+1, System.Length(S)));
  End;
End;

//  Returns the U, V texture coordinates of a texture from a String
Function GetTextureCoords(S:AnsiString):Vector2D;
Var
  P,P2:Integer;
Begin
  P:=Pos(' ', S);
  P2 := PosEx(' ', S, P+1);

  Result.X := StringToFloat(Copy(S, P+1, P2-P-1));
  Result.Y := StringToFloat(Copy(S, P2+1, System.Length(S)));
  Result.Y := 1.0 - Result.Y;
end;

//  Reads Vertex coords, Normals and Texture coords from a String
Procedure OBJModel.ReadVertexData(S:AnsiString);
Var
  C:Vector3D;
  T:Vector2D;
Begin
  Case S[2] of
    ' ' : Begin                      // Read the vertex coords
            C:=GetVector(S);
            Inc(_VertexCount);
            SetLength(_VertexList, _VertexCount);
            _VertexList[Pred(_VertexCount)] := C;
          End;
    'N' : Begin                      // Read the vertex normals
            C:=GetVector(S);
            Inc(_NormalCount);
            SetLength(_NormalList, _NormalCount);
            _NormalList[Pred(_NormalCount)] := C;
          End;
    'T' : Begin                      // Read the vertex texture coords
            T := GetTextureCoords(S);
            Inc(_UVCount);
            SetLength(_UVList, _UVCount);
            _UVList[Pred(_UVCount)] := T;
          End;
  End;
End;

//  Reads the faces/triangles info for the model
//  Data is stored as "f f f" OR "f/t f/t /ft" OR "f/t/n .. f/t/n"
Procedure OBJModel.ReadFaceData(S:AnsiString);
Const
  MaxPolySize=32;
Var
  P,P2,P3:Integer;
  Face:OBJFace;
  Group:POBJGroup;
  I,Index:Integer;
  S2, S3:AnsiString;
  VertexIndices,UVIndices,NormalIndices:Array[0..Pred(MaxPolySize)]Of Integer;
Begin
  P:=Pos(' ', S);
  S:=Trim(Copy(S, P+1, length(S)));

  If (_GroupCount<=0) Then
  Begin
    Inc(_GroupCount);
    SetLength(_GroupList, _GroupCount);
    _GroupList[Pred(_GroupCount)].Name := 'group'+IntToString(_GroupCount);
  End;

  Group := @(_GroupList[Pred(_GroupCount)]);

  Index:=0;
  While (Length(S)>0) Do
  begin
    S2 := StringGetNextSplit(S, Ord(' '));

    Inc(Index);
    S3 := StringGetNextSplit(S2, Ord('/'));
    VertexIndices[Pred(Index)] := Pred(StringToInt(S3));

    S3 := StringGetNextSplit(S2, Ord('/'));
    If (S3<>'') Then
      UVIndices[Pred(Index)] := Pred(StringToInt(S3)) 
    Else
      UVIndices[Pred(Index)] := -1;

    If (S2<>'') Then
      NormalIndices[Pred(Index)] := Pred(StringToInt(S2))
    Else
      NormalIndices[Pred(Index)] := -1;
  End;

  For I:=2 To Pred(Index) Do
  Begin
    Face.VertexIndices[0] := VertexIndices[0];
    Face.VertexIndices[1] := VertexIndices[I-1];
    Face.VertexIndices[2] := VertexIndices[I-0];

    Face.NormalIndices[0] := NormalIndices[0];
    Face.NormalIndices[1] := NormalIndices[I-1];
    Face.NormalIndices[2] := NormalIndices[I-0];

    Face.UVIndices[0] := UVIndices[0];
    Face.UVIndices[1] := UVIndices[I-1];
    Face.UVIndices[2] := UVIndices[I-0];

    Inc(Group.FaceCount);
    SetLength(Group.FaceList, Group.FaceCount);
    Group.FaceList[Pred(Group.FaceCount)] := Face;
  End;
End;

//  Get the name of the material for the group
Procedure OBJModel.GetMaterialName(S:AnsiString);
Var
  I,P:Integer;
Begin
  If copy(S, 1, 6)<>'USEMTL' Then
    Exit;  // false call

  P:=Pos(' ', S);
  S:=Copy(S, P+1, length(S));

  For I :=0 To Pred(_MaterialCount) Do
    If _MaterialList[I].Name=S then
    Begin
      _GroupList[Pred(_GroupCount)].MaterialIndex := I;
      Break;
    End;
End;

//  Create a new material
Procedure OBJModel.CreateMaterial(S:AnsiString);
Var
  Material:POBJMaterial;
Begin
  If Copy(S,1,6) <> 'NEWMTL' Then
    Exit;

  Inc(_MaterialCount);
  SetLength(_MaterialList, _MaterialCount);
  S := Trim(Copy(S, 7, Length(S)));
  Material := @(_MaterialList[Pred(_MaterialCount)]);

  Material.Ambient := ColorWhite;
  Material.Diffuse := ColorWhite;
  Material.Specular := ColorBlack;
  Material.Shininess := 60;
  Material.DiffuseTexture := '';
  Material.SpecularTexture := '';
  Material.Name := S;
End;

//  Get Material Color values
Procedure OBJModel.ReadMaterial(S:AnsiString);
Var
  MyColor:Color;
  P,P2:Integer;
  Ch:AnsiChar;
  Material:POBJMaterial;
Begin
  Ch := S[2];
  S:=Trim(Copy(S, 3, Length(S)));
  P:=Pos(' ', S);
  P2:=PosEx(' ', S, P+1);
  StringReplaceText(',', '.', S);

  MyColor.R   := Round(StringToFloat(Copy(S, 1, P-1))*255);
  MyColor.G := Round(StringToFloat(Copy(S, P+1, P2-P-1))*255);
  MyColor.B  := Round(StringToFloat(Copy(S, P2+1, Length(S)))*255);
  MyColor.A := 255;

  Material := @(_MaterialList[Pred(_MaterialCount)]);
  Case CH of
    'A' : Material.Ambient  := MyColor;
    'D' : Material.Diffuse  := MyColor;
    'S' : Material.Specular := MyColor;
  End;
End;

//  Get material specular highlight
Procedure OBJModel.ReadShininess(S:AnsiString);
Begin
  S := StringTrim(Copy(S, 3, Length(S)));
  StringReplaceText(',', '.', S);

  _MaterialList[Pred(_MaterialCount)].Shininess := StringToFloat(S);
End;

//  Load texture for material
Procedure OBJModel.ReadTexture(S:AnsiString);
Var
  I:Integer;
  Tag:AnsiString;
Begin
  I := Pos(' ', S);
  Tag := Copy(S, 1, Pred(I));
  S := Copy(S, Succ(I), MaxInt);
  If (StringUpper(Tag) = 'MAP_KD') Then
    _MaterialList[Pred(_MaterialCount)].DiffuseTexture := S
  Else
  If (StringUpper(Tag) = 'MAP_KS') Then
    _MaterialList[Pred(_MaterialCount)].SpecularTexture := S;
End;

//  Load the materials from the material file
Procedure OBJModel.LoadMaterials(S:AnsiString);
Var
  P:Integer;
  Filename:AnsiString;
  F:Stream;
Begin
  If Copy(S, 1, 6) <> 'MTLLIB' Then
    Exit;  // false call

  P := Pos(' ', S);
  FileName := Copy(S, P+1, length(S));

  S := FileManager.Instance.SearchResourceFile(FileName);

  If S='' Then
  Begin
    Log(logWarning, 'OBJ','LoadMaterials: Cannot find the material file.['+FileName+']');
    Exit;
  End;

  F := MemoryStream.Create(S);
  While Not(F.EOF) Do
  Begin
    F.ReadLine(S);
    If (S<>'') And (S[1] <> '#') then
    Begin
      S := Uppercase(S);
      Case S[1] of
        'N' : Begin
                If S[2] = 'S' Then ReadShininess(S);  // Get specular highlight amount
                If S[2] = 'E' Then CreateMaterial(S);  // create new material
              End;
        'K' : ReadMaterial(S);     // Material properties
        'M' : ReadTexture(S);      // Map material to texture
      End;
    End;
  End;
  F.Release;
End;

Function OBJModel.GetMaterial(Index:Integer): POBJMaterial;
Begin
  If _GroupList[Index].MaterialIndex<>-1 Then
    Result := @(_MaterialList[_GroupList[Index].MaterialIndex])
  Else
    Result := Nil;
End;

Function OBJModel.Load(Source:Stream):Boolean;
Var
  S,S2:AnsiString;
  Index:Integer;
  I,J,N:Integer;
  OBJGroup:POBJGroup;
  Material:POBJMaterial;
  VertexPool:Array Of OBJVertex;
  VertexPoolSize:Integer;

  Function AllocVertex(Vertex,Face,Index:Integer):Integer;
  Var
    I,K:Integer;
  Begin
    K:=-1;
    For I:=0 To Pred(VertexPoolSize) Do
    If (VertexPool[I].Vertex=Vertex) Then
    Begin
      K:=I;
      Break;
    End;
    K := -1;

    If K=-1 Then
    Begin
      Inc(VertexPoolSize);
      SetLength(VertexPool,VertexPoolSize);
      K:=Pred(VertexPoolSize);

      OBJGroup.VertexCount := VertexPoolSize;
      SetLength(OBJGroup.Vertices, OBJGroup.VertexCount);
    End;

      VertexPool[K].Vertex := Vertex;

      With OBJGroup.Vertices[K] Do
      Begin
        Position := _VertexList[Vertex];

        With _GroupList[N].FaceList[Face] Do
        Begin
          If UVIndices[Index]>=0 Then
            TextureCoords := _UVList[UVIndices[Index]];
          If NormalIndices[Index]>=0 Then
            Normal := _NormalList[NormalIndices[Index]];
        End;
      End;

    Result := K;
  End;
Begin
  Result := False;
  _MaterialFile := '';
  _VertexCount := 0;
  _NormalCount := 0;
  _UVCount := 0;
  _GroupCount := 0;
  _MaterialCount := 0;

  FileManager.Instance.AddPath(GetFilePath(Source.Name));

  While Not Source.EOF Do
  Begin
    Source.ReadLine(S);
//    Log(logDebug, 'OBJ', S);
    If (S<>'') And (S[1]<>'#') Then
    Begin
      S:=Uppercase(S);
      Case S[1] Of
        'O','G':  Begin
                Inc(_GroupCount);
                SetLength(_GroupList, _GroupCount);
                S2 := Trim(Copy(S, 2, length(S)));
                _GroupList[Pred(_GroupCount)].Name := S2;
              End;
        'V':  ReadVertexData(S);   // Read Vertex Data (coord, normal, texture)
        'F' : ReadFaceData(S);     // Read faces
        'U' : GetMaterialName(S);  // Get the material name
        'M' : LoadMaterials(S);    // Get the material name
      End;
    End;
  End;

  For N:=0 To Pred(_GroupCount) Do
  Begin
    OBJGroup := @(_GroupList[N]);
{    If OBJGroup.MaterialIndex<>-1 Then
      Material := @(_MaterialList[OBJGroup.MaterialIndex])
    Else
      Material := Nil;

    If Assigned(Material) Then
    Begin
      MeshGroup.Diffuse := Material.Diffuse;
      MeshGroup.Specular := ColorScale(Material.Specular,Material.Ambient);
      MeshGroup.Shininess := Material.Shininess;
    End Else
    Begin
      MeshGroup.Diffuse := ColorWhite;
      MeshGroup.Specular := ColorBlack;
      MeshGroup.Shininess := 0;
    End;

    MeshGroup.Emission:=clBlack;

    If (Assigned(Material))And(Material.Texture<>'') Then
    Begin
      MeshGroup.Texture:=Mesh.GetTexture(Material.Texture);
    End Else
      MeshGroup.Texture:=Nil;}

    VertexPoolSize := 0;
    OBJGroup.VertexCount := 0;

    SetLength(OBJGroup.Triangles, OBJGroup.FaceCount);
    For I:=0 To Pred(OBJGroup.FaceCount) Do
      For J:=0 To 2 Do
      Begin
        Index := AllocVertex(OBJGroup.FaceList[I].VertexIndices[J],I,J);
        OBJGroup.Triangles[I].Indices[J] := Index;
      End;

    SetLength(VertexPool,0);
  End;

  Result := True;
End;


Class Function OBJModel.Save(Dest:Stream; MyMesh:MeshFilter):Boolean;
Var
  I,J,K, N:Integer;
  Count:Integer;
  S:AnsiString;
  GroupOfs:Array Of Integer;
  P:Vector3D;
  UV:Vector2D;
  T:Triangle;
  F:Single;
  C:Color;
  Group:MeshGroup;
  Ofs:Integer;
Begin
 Result := True;
  SetLength(GroupOfs, MyMesh.GetGroupCount);
  Ofs := 0;

{  For N:=0 To Pred(Mesh.GetGroupCount) Do
  Begin
    Dest.WriteLine('g '+Mesh.GetGroupName(N));
  End;
 }


  Dest.WriteLine('#Exported by TERRA');

  If (Dest Is FileStream) Then
    Dest.WriteLine('mtllib '+GetFileName(Dest.Name, True)+'.mtl');

  Dest.WriteLine('#vertices');
  For I:=0 To Pred(MyMesh.GetGroupCount) Do
  Begin
    GroupOfs[I] := Ofs;
    Count := MyMesh.GetVertexCount(I);
    Inc(Ofs, Count);
    For J:=0 To Pred(Count) Do
    Begin
      P := MyMesh.GetVertexPosition(I, J);
      S := Format('v %8.6f %8.6f %8.6f', [P.X, P.Y, P.Z]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);
    End;
  End;

  Dest.WriteLine('#normals');
  For I:=0 To Pred(MyMesh.GetGroupCount) Do
  Begin
    Count := MyMesh.GetVertexCount(I);
    For J:=0 To Pred(Count) Do
    Begin
      P := MyMesh.GetVertexNormal(I, J);
      S := Format('vn %8.6f %8.6f %8.6f', [P.X, P.Y, P.Z]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);
    End;
  End;

  Dest.WriteLine('#texture coords');
  For I:=0 To Pred(MyMesh.GetGroupCount) Do
  Begin
    Count := MyMesh.GetVertexCount(I);
    For J:=0 To Pred(Count) Do
    Begin
      UV := MyMesh.GetVertexUV(I, J);
      S := Format('vt %8.6f %8.6f', [UV.X, 1.0-UV.Y]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);
    End;
  End;

  Dest.WriteLine('#faces');
  For I:=0 To Pred(MyMesh.GetGroupCount) Do
  Begin
    Dest.WriteLine('g group'+IntToString(Succ(I)));

    If (Dest Is FileStream) Then
      Dest.WriteLine('usemtl '+MyMesh.GetGroupName(I));

  If Pos('poly',MyMesh.GetGroupName(I))>0 then
    IntToString(2);


    Dest.WriteLine('s 1');

    Count := MyMesh.GetTriangleCount(I);
    For J:=0 To Pred(Count) Do
    Begin
      T := MyMesh.GetTriangle(I, J);
      S:=Format('f %d/%d/%d %d/%d/%d %d/%d/%d',[Succ(T.Indices[0] + GroupOfs[I]), Succ(T.Indices[0] + GroupOfs[I]), Succ(T.Indices[0] + GroupOfs[I]),
                                                Succ(T.Indices[1] + GroupOfs[I]), Succ(T.Indices[1] + GroupOfs[I]), Succ(T.Indices[1] + GroupOfs[I]),
                                                Succ(T.Indices[2] + GroupOfs[I]), Succ(T.Indices[2] + GroupOfs[I]), Succ(T.Indices[2] + GroupOfs[I])]);
      Dest.WriteLine(S);
    End;
  End;

  Dest.WriteLine('#EOF');

  If (Dest Is FileStream) Then
  Begin
    S := GetFilePath(Dest.Name);
    If S<>'' Then
      S := S + PathSeparator;
    S := S + GetFileName(Dest.Name, True) + '.mtl';
    Dest := FileStream.Create(S);

    For I:=0 To Pred(MyMesh.GetGroupCount) Do
    Begin
      Dest.WriteLine('newmtl '+MyMesh.GetGroupName(I));
      Dest.WriteLine('Ka 0.000 0.000 0.000');

      C := MyMesh.GetDiffuseColor(I);
      S := Format('Kd %8.6f %8.6f %8.6f', [C.R/255, C.G/255, C.B/255]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);

      If (C.A<255) Then
      Begin
        S := Format('Tr %8.6f', [C.A/255]);
        StringReplaceText(',', '.', S);
        Dest.WriteLine(S);
      End;

      C := ColorBlack; //MyMesh.GetSpecularColor(I);
      S := Format('Ks %8.6f %8.6f %8.6f', [C.R/255, C.G/255, C.B/255]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);

      F := 0.0; //MyMesh.GetSpecularFactor(I);
      S := Format('Ns %8.6f', [F]);
      StringReplaceText(',', '.', S);
      Dest.WriteLine(S);

      S := MyMesh.GetDiffuseMapName(I);
      If S<>'' Then
      Begin
        S := GetFileName(S, True)+'.png';
        If (FileManager.Instance.SearchResourceFile(S)='') Then
          S := GetFileName(S, True)+'.jpg';
        Dest.WriteLine('map_Kd '+ S);
      End;

      {S := MyMesh.GetSpecularMapName(I);
      If S<>'' Then
        Dest.WriteLine('map_Ks '+ GetFileName(S, False));}
    End;

    Dest.Release;
  End;
End;

Function OBJModel.GetDiffuseColor(GroupID: Integer): Color;
Var
  Mat:POBJMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.Diffuse
  Else
    Result := ColorWhite;
End;

Function OBJModel.GetDiffuseMapName(GroupID: Integer):AnsiString;
Var
  Mat:POBJMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.DiffuseTexture
  Else
    Result := '';
End;

Function OBJModel.GetEmissiveMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function OBJModel.GetGroupBlendMode(GroupID: Integer): Cardinal;
Begin
  Result := blendBlend;
End;

Function OBJModel.GetGroupCount: Integer;
Begin
  Result := _GroupCount;
End;

Function OBJModel.GetGroupName(GroupID: Integer):AnsiString;
Begin
  Result := _GroupList[GroupID].Name;
End;

{Function OBJModel.GetSpecularColor(GroupID: Integer): Color;
Var
  Mat:POBJMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.Specular
  Else
    Result := ColorBlack;
End;

Function OBJModel.GetSpecularFactor(GroupID: Integer): Single;
Var
  Mat:POBJMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.Shininess
  Else
    Result := 0.0;
End;}

Function OBJModel.GetSpecularMapName(GroupID: Integer):AnsiString;
Var
  Mat:POBJMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.SpecularTexture
  Else
    Result := '';
End;

Function OBJModel.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _GroupList[GroupID].Triangles[Index];
End;

Function OBJModel.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _GroupList[GroupID].FaceCount;
End;

Function OBJModel.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _GroupList[GroupID].VertexCount;
End;

Function OBJModel.GetVertexFormat(GroupID: Integer):VertexFormat;
Begin
  Result := [vertexFormatPosition, vertexFormatNormal, vertexFormatUV0];
End;

Function OBJModel.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _GroupList[GroupID].Vertices[Index].Normal;
End;

Function OBJModel.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _GroupList[GroupID].Vertices[Index].Position;
End;

Function OBJModel.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _GroupList[GroupID].Vertices[Index].TextureCoords;
End;

Initialization
  RegisterMeshFilter(OBJModel, 'OBJ');
End.
