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
 * TERRA_Collada
 * Implements a Collada loader/mesh filter
 ***********************************************************************************************************************
}
Unit TERRA_Collada;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_OS, TERRA_Stream, TERRA_XML, TERRA_Math, TERRA_Vector3D,
  TERRA_Vector2D, TERRA_Log, TERRA_Color, TERRA_MeshFilter, TERRA_FileUtils,
  TERRA_FileStream, TERRA_VertexFormat;

Type
  PColladaSource = ^ColladaSource;
  ColladaSource = Record
    ID:AnsiString;
    Count:Integer;
    Stride:Integer;
    Values:Array Of Vector3D;
  End;

	ColladaVertex = Packed Record
		Position:Vector3D;
    TextureCoords:Vector2D;
    Normal:Vector3D;
    BoneIndex:Single;
    Tangent:Vector3D;
    Handness:Single;
    TextureCoords2: Vector2D;
    Color:TERRA_Color.Color;
	End;

  PColladaImage = ^ColladaImage;
  ColladaImage = Record
    ID:AnsiString;
    Path:AnsiString;
  End;

  ColladaSurface = Record
    ID:AnsiString;
    Image:AnsiString;
  End;

  ColladaSampler = Record
    ID:AnsiString;
    Surface:AnsiString;
  End;

  PColladaMaterial =^ColladaMaterial;
  ColladaMaterial = Record
    ID:AnsiString;
    Specular:Color;
    Shininess:Single;
    Reflectivity:Single;
    Transparency:Single;
    Emission:Color;
    DiffuseMap:PColladaImage;
  End;

  ColladaGroup = Record
    Name:AnsiString;
    VertexCount:Integer;
    Vertices:Array Of ColladaVertex;

    Positions:Array Of Integer;
    Normals:Array Of Integer;
    Tangents:Array Of Integer;
    UV1:Array Of Integer;
    UV2:Array Of Integer;
    Colors:Array Of Integer;

    TriangleCount:Integer;
    Triangles:Array Of Triangle;

    Material:PColladaMaterial;
  End;

  ModelCollada = Class(MeshFilter)
    Protected
      _Groups:Array Of ColladaGroup;
      _GroupCount:Integer;

      _Images:Array Of ColladaImage;
      _ImageCount:Integer;

      _Materials:Array Of ColladaMaterial;
      _MaterialCount:Integer;

      Procedure LoadImages(Node:XMLNode);
      Procedure LoadGeometry(Node:XMLNode);
      Procedure LoadMaterials(Node, Node2:XMLNode);

      Function GetMaterial(MatName:AnsiString):PColladaMaterial;
      Function GetImage(ID:AnsiString):PColladaImage;
      Procedure AddGroup(Node:XMLNode);

    Public
      Function Load(Source:Stream):Boolean; Override;

      // filter interface
      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):VertexFormat; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
//    Function GetVertexTangent(GroupID, Index:Integer):Vector3D; Override;
//    Function GetVertexHandness(GroupID, Index:Integer):Single; Override;
//    Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
//    Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;
//    Function GetVertexUV2(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;
      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;
  End;

Implementation

Procedure PrintXML(X:XMLNode; Level:Integer = 0);
Var
  I:Integer;
Begin
  If X=Nil Then
    Exit;

  // format output with tabs
  For I:=1 To Level Do
    Write(#9);
  WriteLn(X.Name);

  // print all children
  For I:=0 To Pred(X.NodeCount) Do
    PrintXML(X.GetNodeByIndex(I), Succ(Level));
End;

{ ModelCollada }
Procedure ModelCollada.AddGroup(Node:XMLNode);
Var
  Tag, S, S2:AnsiString;
  I, J, N, K:Integer;
  O1, O2, O3, O4:Integer;
  Child, FA, P, P2:XMLNode;
  Group:^ColladaGroup;
  Sources:Array Of ColladaSource;
  SourceCount:Integer;
  Ok:Boolean;
  T:Vector3D;

  HasNormals, HasUV1, HasUV2:Boolean;
  SrcVtx, SrcNormal, SrcUV1, SrcUV2:PColladaSource;


  Function GetVertex(Vert, Norm, UV:Integer):Integer;
  Var
    I:Integer;
  Begin
    For I:=0 To Pred(Group.VertexCount) Do
    If (Group.Positions[I] = Vert) And (Group.Normals[I] = Norm) And (Group.UV1[I] = UV) Then
    Begin
      Result := I;
      Break;
    End;

    Result := Group.VertexCount;
    Inc(Group.VertexCount);
    SetLength(Group.Vertices, Group.VertexCount);
    SetLength(Group.Positions, Group.VertexCount);
    SetLength(Group.Normals, Group.VertexCount);
    SetLength(Group.UV1, Group.VertexCount);

    Group.Positions[Result] := Vert;
    Group.Normals[Result] := Norm;
    Group.UV1[Result] := UV;
  End;

  Procedure AddTriangle(N, K, Vert, Norm, UV:Integer);
  Begin
    Group.Triangles[N].Indices[K] := GetVertex(Vert, Norm, UV);
  End;

  Function GetSource(ID:AnsiString):PColladaSource;
  Var
    I:Integer;
  Begin
    If (ID[1]='#') Then
      ID := Copy(ID, 2, MaxInt);

    For I:=0 To Pred(SourceCount) Do
    If (Sources[I].ID = ID) Then
    Begin
      Result := @(Sources[I]);
      Exit;
    End;

    Result := Nil;
  End;
Begin
  If Node = Nil Then
    Exit;

  Inc(_GroupCount);
  SetLength(_Groups, _GroupCount);
  Group := @(_Groups[Pred(_GroupCount)]);
  Group.VertexCount := 0;
  P := Node.GetNodeByName('name');
  If Assigned(P) Then
    Group.Name := P.Value
  Else
    Group.Name := 'Group'+IntToString(_GroupCount);

  Node := Node.GetNodeByName('mesh');
  If Not Assigned(Node) Then
    Exit;

  SourceCount := 0;
  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    Child := Node.GetNodeByIndex(I);
    If Child.Name = 'source' Then
    Begin
      P := Child.GetNodeByName('id');
      If Not Assigned(P) Then
        Continue;

      Inc(SourceCount);
      SetLength(Sources, SourceCount);
      Sources[Pred(SourceCount)].ID := P.Value;
      FA := Child.GetNodeByName('float_array');

      P := Child.GetNodeByName('technique_common');
      P := P.GetNodeByName('accessor');
      P2 := P;

      P := P2.GetNodeByName('count');
      Sources[Pred(SourceCount)].Count := StringToInt(P.Value);

      P := P2.GetNodeByName('stride');
      Sources[Pred(SourceCount)].Stride := StringToInt(P.Value);

      SetLength(Sources[Pred(SourceCount)].Values, Sources[Pred(SourceCount)].Count);
      N := 0;
      S := FA.Value;
      While (S<>'') Do
      Begin
        S2 := StringGetNextSplit(S, Ord(' '));
        Sources[Pred(SourceCount)].Values[N].X := StringToFloat(S2);

        If (Sources[Pred(SourceCount)].Stride>=2) Then
        Begin
          S2 := StringGetNextSplit(S, Ord(' '));
          Sources[Pred(SourceCount)].Values[N].Y := StringToFloat(S2);
        End;

        If (Sources[Pred(SourceCount)].Stride>=3) Then
        Begin
          S2 := StringGetNextSplit(S, Ord(' '));
          Sources[Pred(SourceCount)].Values[N].Z := StringToFloat(S2);
        End;

        Inc(N);
      End;

    End Else
    If (Child.Name = 'vertices') Then
    Begin
      Ok := False;
      P := Child.GetNodeByName('input');
      If Assigned(P) Then
      Begin
        P := P.GetNodeByName('source');
        If Assigned(P) Then
        Begin
          SrcVtx := GetSource(P.Value);
          Ok := True;
        End;
      End;

      If Not Ok Then
        Log(logError, 'Collada', 'Invalid collada vertices section.');
    End Else
    If (Child.Name = 'triangles') Then
    Begin
      P := Child.GetNodeByName('count');
      Group.TriangleCount := StringToInt(P.Value);
      SetLength(Group.Triangles, Group.TriangleCount);

      P := Child.GetNodeByName('material');
      If Assigned(P) Then
        Group.Material := GetMaterial(P.Value);

      SrcNormal := Nil;
      SrcUV1 := Nil;
      SrcUV2 := Nil;
      For J:=0 To Pred(Child.NodeCount) Do
      Begin
        P := Child.GetNodeByIndex(J);
        If (P.Name='input') Then
        Begin
          FA := P.GetNodeByName('semantic');
          S := FA.Value;

          If (S='NORMAL') Then
          Begin
            FA := P.GetNodeByName('source');
            SrcNormal := GetSource(FA.Value);
          End;

          If (S='TEXCOORD') Then
          Begin
            FA := P.GetNodeByName('source');
            SrcUV1 := GetSource(FA.Value);
          End;
        End Else
        If (P.Name = 'p') Then
        Begin
          Group.VertexCount := 0;
          S := P.Value;
          N := 0;
          K := 0;
          While S<>'' Do
          Begin
            S2 := StringGetNextSplit(S, Ord(' '));
            O1 := StringToInt(S2);

            S2 := StringGetNextSplit(S, Ord(' '));
            O2 := StringToInt(S2);

            S2 := StringGetNextSplit(S, Ord(' '));
            O3 := StringToInt(S2);

            AddTriangle(N, K, O1, O2, O3);
            Inc(K);
            If (K>2) Then
            Begin
              K := 0;
              Inc(N);
            End;
          End;
        End;
      End;
    End;
  End;


  For I:=0 To Pred(Group.VertexCount) Do
  Begin
    If Assigned(SrcVtx) Then
      Group.Vertices[I].Position := SrcVtx.Values[Group.Positions[I]];

    If Assigned(SrcNormal) Then
      Group.Vertices[I].Normal := SrcNormal.Values[Group.Normals[I]];

    If Assigned(SrcUV1) Then
    Begin
      T := SrcUV1.Values[Group.UV1[I]];
      Group.Vertices[I].TextureCoords := VectorCreate2D(T.X, T.Y);
    End;

    If Assigned(SrcUV2) Then
    Begin
      T := SrcUV1.Values[Group.UV2[I]];
      Group.Vertices[I].TextureCoords2 := VectorCreate2D(T.X, T.Y);
    End;
  End;
End;

Function ModelCollada.GetImage(ID:AnsiString):PColladaImage;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ImageCount) Do
  If (_Images[I].ID = ID) Then
  Begin
    Result := @(_Images[I]);
    Exit;
  End;

  Result := Nil;
End;

Function ModelCollada.GetMaterial(MatName:AnsiString): PColladaMaterial;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MaterialCount) Do
  If (_Materials[I].ID = MatName) Then
  Begin
    Result := @(_Materials[I]);
    Exit;
  End;

  Result := Nil;
End;

Procedure ModelCollada.LoadMaterials(Node, Node2:XMLNode);
Var
  Child, P, P2:XMLNode;
  I,J,K, N:Integer;
  S, ID:AnsiString;
  Mat:PColladaMaterial;
  Samplers:Array Of ColladaSampler;
  SamplerCount:Integer;
  Surfaces:Array Of ColladaSurface;
  SurfaceCount:Integer;

  Function ReadColor(S:AnsiString):Color;
  Var
    S2:AnsiString;
  Begin
    S2 := StringGetNextSplit(S, Ord(' '));
    Result.R := Trunc(StringToFloat(S2) * 255);

    S2 := StringGetNextSplit(S, Ord(' '));
    Result.G := Trunc(StringToFloat(S2) * 255);

    S2 := StringGetNextSplit(S, Ord(' '));
    Result.B := Trunc(StringToFloat(S2) * 255);

    S2 := StringGetNextSplit(S, Ord(' '));
    If (S2='') Then
      S2 := '1';
    Result.A := Trunc(StringToFloat(S2) * 255);
  End;

  Function SearchEffect(ID:AnsiString):XMLNode;
  Var
    I:Integer;
    P, Child:XMLNode;
  Begin
    If (ID[1]='#') Then
      ID := Copy(ID, 2, MaxInt);

    For I:=0 To Pred(Node2.NodeCount) Do
    Begin
      Child := Node2.GetNodeByIndex(I);
      P := Child.GetNodeByName('id');
      If P = Nil Then
        Continue;

      If (P.Value = ID) Then
      Begin
        Result := Child;
        Exit;
      End;
    End;

    Result := Nil;
  End;

  Function GetTexture(Sampler:AnsiString):PColladaImage;
  Var
    K,N:Integer;
  Begin
    N := -1;
    For K:=0 To Pred(SamplerCount) Do
    If (Samplers[K].ID = P.Value) Then
    Begin
      N := K;
      Break;
    End;

    If (N>=0) Then
    Begin
      For K:=0 To Pred(SurfaceCount) Do
      If (Surfaces[K].ID = Samplers[N].Surface) Then
      Begin
        Result := Self.GetImage(Surfaces[K].Image);
        Exit;
      End;
    End;

    Result := Nil;
  End;

Begin
  SamplerCount := 0;
  SurfaceCount := 0;

  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    Child := Node.GetNodeByIndex(I);
    Inc(_MaterialCount);
    SetLength(_Materials, _MaterialCount);
    P := Child.GetNodeByName('id');
    Mat := @_Materials[Pred(_MaterialCount)];
    Mat.ID := P.Value;
    P := Child.GetNodeByName('instance_effect');
    P := P.GetNodeByName('url');
    S := P.Value;

    Child := SearchEffect(S);
    If Not Assigned(Child) Then
    Begin
      Log(logWarning, 'Collada', 'Could not find material: '+ S);
      Continue;
    End;

    Child := Child.GetNodeByName('profile_COMMON');
    If Assigned(Child) Then
    For J:=0 To Pred(Child.NodeCount) Do
    Begin
      P := Child.GetNodeByIndex(J);
      If (P.Name = 'newparam') Then
      Begin
        P2 := P;

        P := P2.GetNodeByName('sid');
        ID := P.Value;

        P := P2.GetNodeByName('surface');
        If Assigned(P) Then
        Begin
          Inc(SurfaceCount);
          SetLength(Surfaces, SurfaceCount);
          Surfaces[Pred(SurfaceCount)].ID := ID;
          P := P.GetNodeByName('init_from');
          If Assigned(P) Then
            Surfaces[Pred(SurfaceCount)].Image := P.Value;
        End;

        P := P2.GetNodeByName('sampler2D');
        If Assigned(P) Then
        Begin
          Inc(SamplerCount);
          SetLength(Samplers, SamplerCount);
          Samplers[Pred(SamplerCount)].ID := ID;
          
          P := P.GetNodeByName('source');
          If Assigned(P) Then
            Samplers[Pred(SamplerCount)].Surface := P.Value;
        End;
      End Else
      If (P.Name = 'technique') Then
      Begin
        P2 := P.GetNodeByName('blinn');
        If P2=Nil Then
          P2 := P.GetNodeByName('phong');
        If P2<>Nil Then
        Begin
          For K:=0 To Pred(P2.NodeCount) Do
          Begin
            P := P2.GetNodeByIndex(K);
            If (P.Name = 'emission') Then
            Begin
              P := P.GetNodeByName('color');
              Mat.Emission := ReadColor(P.Value);
            End Else
            If (P.Name = 'specular') Then
            Begin
              P := P.GetNodeByName('color');
              Mat.Specular := ReadColor(P.Value);
            End Else
            If (P.Name = 'shininess') Then
            Begin
              P := P.GetNodeByName('float');
              Mat.Shininess := StringToFloat(P.Value);
            End Else
            If (P.Name = 'diffuse') Then
            Begin
              P := P.GetNodeByName('texture');
              P := P.GetNodeByName('texture');
              Mat.DiffuseMap := GetTexture(P.Value);
            End Else
              Log(logWarning, 'Collada', 'Material attribute ignored: '+P.Name);
          End;
        End Else
        Begin
          Log(logWarning, 'Collada', 'Could not find technique for material: '+ Mat.ID);
          Continue;
        End;
      End;
    End;
  End;
End;

Procedure ModelCollada.LoadGeometry(Node:XMLNode);
Var
  I:Integer;
  Child:XMLNode;
Begin
  If Node = Nil Then
    Exit;

  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    Child := Node.GetNodeByIndex(I);
    If Child.Name = 'geometry' Then
      Self.AddGroup(Child);
  End;
End;

Procedure ModelCollada.LoadImages(Node: XMLNode);
Var
  P:XMLNode;
  I:Integer;
  S:AnsiString;
Begin
  _ImageCount := 0;

  If (Node = Nil) Then
    Exit;

  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    P := Node.GetNodeByIndex(I);
    If (P.Name<>'image') Then
      Continue;

    Inc(_ImageCount);
    SetLength(_Images, _ImageCount);
    _Images[Pred(_ImageCount)].Id := P.GetNodeByName('id').Value;
    S := P.GetNodeByName('init_from').Value;
    S := GetOSIndependentFileName(S);
    S := GetFileName(S, False);
    _Images[Pred(_ImageCount)].Path := S;
  End;
End;

Function ModelCollada.Load(Source: Stream): Boolean;
Var
  S:AnsiString;
  Collada:XMLDocument;
  Node, Node2:XMLNode;
Begin
	 Result := True;
  Collada := XMLDocument.Create;
  Collada.Load(Source);
  S := '';
//  S := Self.GetTag(Source);
  Result := S ='';
  PrintXML(Collada.Root, 0);

  Node := Collada.Root.GetNodeByName('library_images');
  LoadImages(Node);

  Node := Collada.Root.GetNodeByName('library_materials');
  Node2 := Collada.Root.GetNodeByName('library_effects');
  LoadMaterials(Node, Node2);

  Node := Collada.Root.GetNodeByName('library_geometries');
  LoadGeometry(Node);

  Collada.Release;
End;

// filter interface
Function ModelCollada.GetGroupCount: Integer;
Begin
  Result := _GroupCount;
End;

Function ModelCollada.GetGroupName(GroupID: Integer):AnsiString;
Begin
  Result := _Groups[GroupID].Name;
End;

Function ModelCollada.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Groups[GroupID].Triangles[Index];
End;

Function ModelCollada.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _Groups[GroupID].TriangleCount;
End;

Function ModelCollada.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Groups[GroupID].VertexCount;
End;

Function ModelCollada.GetVertexFormat(GroupID: Integer):VertexFormat;
Begin
  Result := [vertexFormatPosition, vertexFormatNormal, vertexFormatUV0];
End;

Function ModelCollada.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Normal;
End;

Function ModelCollada.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Position;
End;

Function ModelCollada.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Groups[GroupID].Vertices[Index].TextureCoords;
End;

Function ModelCollada.GetDiffuseMapName(GroupID: Integer):AnsiString;
Begin
  If Assigned(_Groups[GroupID].Material) Then
  Begin
    If Assigned(_Groups[GroupID].Material.DiffuseMap) Then
      Result := _Groups[GroupID].Material.DiffuseMap.Path
    Else
      Result := '';
  End Else
    Result := '';
End;

Function ModelCollada.GetEmissiveMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function ModelCollada.GetSpecularMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function ModelCollada.GetDiffuseColor(GroupID: Integer): Color;
Begin
  If Assigned(_Groups[GroupID].Material) Then
    Result := ColorGrey(255, Trunc(255*_Groups[GroupID].Material.Transparency))
  Else
    Result := ColorWhite;
End;

{Function ModelCollada.GetEmissiveFactor(GroupID: Integer): Single;
Begin
  If Assigned(_Groups[GroupID].Material) Then
    Result := ColorLuminance(_Groups[GroupID].Material.Emission)/255
  Else
    Result := 0.0;
End;

Function ModelCollada.GetSpecularColor(GroupID: Integer): Color;
Begin
  If Assigned(_Groups[GroupID].Material) Then
    Result := _Groups[GroupID].Material.Specular
  Else
    Result := ColorBlack;
End;

Function ModelCollada.GetSpecularFactor(GroupID: Integer): Single;
Begin
  If Assigned(_Groups[GroupID].Material) Then
    Result := _Groups[GroupID].Material.Shininess
  Else
    Result := 0.0;
End;}

Initialization
  RegisterMeshFilter(ModelCollada, 'DAE');
End.