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
 * TERRA_Milkshape
 * Implements a Milkshape loader/writer/mesh filter
 ***********************************************************************************************************************
}

Unit TERRA_Milkshape;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Stream, TERRA_INI, TERRA_Vector3D, TERRA_Vector2D, TERRA_Matrix4x4,
  TERRA_Color, TERRA_FileStream, TERRA_FileUtils, TERRA_Vector4D, TERRA_MeshFilter, TERRA_VertexFormat;

Const
  MS3D_HEADER='MS3D000000';

  MS3D_MAX_VERTICES=65534;
  MS3D_MAX_TRIANGLES=65534;
  MS3D_MAX_GROUPS=255;
  MS3D_MAX_MATERIALS=128;
  MS3D_MAX_JOINTS=128;
  MS3D_MAX_KEYFRAMES=216;  // increase when needed

// flags
  MS3D_SELECTED   = 1;
  MS3D_HIDDEN     = 2;
  MS3D_SELECTED2  = 4;
  MS3D_DIRTY      = 8;

Type
  Milkshape3DHeader=Packed Record
    Id:Array[1..10]Of AnsiChar; // always "MS3D000000"
    Version:Integer;        // 4
  End;

  Milkshape3DVertex=Packed Record
    Flags:Byte;  // SELECTED | SELECTED2 | HIDDEN
    Vertex:Vector3D;
    BoneIndex:Shortint;  // -1 = no bone
    ReferenceCount:Byte;
  End;

  Milkshape3DTriangle=Packed Record
    Flags:Word; // SELECTED | SELECTED2 | HIDDEN
    VertexIndices:Array[1..3]Of Word;
    VertexNormals:Array[1..3]Of Vector3D;
    S:Array[1..3]Of Single;
    T:Array[1..3]Of Single;
    SmoothingGroup:Byte;  // 1 - 32
    GroupIndex:Byte;
  End;

  Milkshape3DGroup=Packed Record
    Flags:Byte;  // SELECTED | HIDDEN
    Name:Array[1..32]Of AnsiChar;
    NumTriangles:Word;
    TriangleIndices:Array Of Word;  // the groups group the triangles
    MaterialIndex:Shortint;   // -1 = no material
    Comment:AnsiString;
  End;

  Milkshape3DColor=Packed Record
    R:Single;
    G:Single;
    B:Single;
    A:Single;
  End;

  PMilkshape3DMaterial = ^Milkshape3DMaterial;
  Milkshape3DMaterial=Packed Record
    Name:Array[1..32]Of AnsiChar;
    Ambient:Milkshape3DColor;
    Diffuse:Milkshape3DColor;
    Specular:Milkshape3DColor;
    Emissive:Milkshape3DColor;
    Shininess:Single;    // 0.0f - 128.0f
    Transparency:Single; // 0.0f - 1.0f
    Mode:Byte;           // 0, 1, 2 is unused now
    Texture:Array[1..128]Of AnsiChar;  // texture.bmp
    AlphaMap:Array[1..128]Of AnsiChar; // alpha.bmp
    Comment:AnsiString;
  End;

  Milkshape3DKeyFrame=Packed Record
    Time:Single;    // time in seconds
    Vector:Vector3D; // local position/rotation
  End;

  PMilkshape3DJoint=^Milkshape3DJoint;
  Milkshape3DJoint=Packed Object
    Flags:Byte;  // SELECTED | DIRTY
    Name:Array[1..32]Of AnsiChar;
    ParentName:Array[1..32]Of AnsiChar;
    Rotation:Vector3D;
    Position:Vector3D;
    NumKeyFramesRot:SmallInt;
    NumKeyFramesTrans:SmallInt;
    KeyFramesRot:Array Of Milkshape3DKeyFrame;  // local animation matrices
    KeyFramesTrans:Array Of Milkshape3DKeyFrame;  // local animation matrices
    Comment:AnsiString;

    // calculated, not present in file
    Parent:PMilkshape3DJoint;
    AbsolutePosition:Vector3D;
    RelativePosition:Vector3D;
    AbsoluteMatrix:Matrix4x4;
    RelativeMatrix:Matrix4x4;
    Ready:Boolean;

    Procedure Init;
  End;

  Milkshape3DComment=Record
	  Index:Integer;	// index of group, material or joint
	  CommentLength:Integer;	// length of comment (terminating '\0' is not saved), "MC" has comment length of 2 (not 3)
	  Comment:Array Of AnsiChar;  // comment
  End;

  Milkshape3DObject = Object
    Header:Milkshape3DHeader;
    NumVertices:Word;
    Vertices:Array Of Milkshape3DVertex;
    NumTriangles:Word;
    Triangles:Array Of Milkshape3DTriangle;
    NumGroups:Word;
    Groups:Array Of Milkshape3DGroup;
    NumMaterials:Word;
    Materials:Array Of Milkshape3DMaterial;
    AnimationFPS:Single;
    CurrentTime:Single;
    TotalFrames:Integer;
    NumJoints:SmallInt;
    Joints:Array Of Milkshape3DJoint;
    SubVersion:Integer;
    NumGroupComments:Integer;
    NumMaterialComments:Integer;
    NumJointsComments:Integer;
    NumModelComments:Integer;
    Comment:AnsiString;

    Procedure Clear();

    Function Load(Source:Stream):Boolean;
    Function Save(Dest:Stream):Boolean;

    Function GetParentOf(Child:Integer):Integer;
    Function IsChildOf(Child, Parent:Integer):Boolean;

    Function IsJointUsed(Index:Integer):Boolean;
    Procedure RemoveJoint(Index:Integer);

    Function ConvertTexturesToTextureAtlas(FileName:AnsiString):Boolean;

    Function GetMaterialFile(MaterialIndex:Integer; SourceFile:AnsiString):AnsiString;
  End;

  MS3DVertex = Record
    Position:Vector3D;
    Normal:Vector3D;
    TexCoords:Vector2D;
    BoneIndex:Integer;
  End;

  MS3DGroup = Record
    Vertices:Array Of MS3DVertex;
    VertexCount:Integer;

    Triangles:Array Of Triangle;
    TriangleCount:Integer;

    BlendMode:Integer;
    Flags:Cardinal;
  End;

  Milkshape3DModel = Class(MeshFilter)
    Protected
      _MS3D:Milkshape3DObject;
      _Groups:Array Of MS3DGroup;

      Function GetMaterial(GroupID:Integer):PMilkshape3DMaterial;

    Public
      Function Load(Source:Stream):Boolean; Override;
      Class Function Save(Dest:Stream; MyMesh:MeshFilter):Boolean; Override;

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):VertexFormat; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;

      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;

      Property Data:Milkshape3DObject Read _MS3D;
  End;


Implementation
Uses TERRA_Error, TERRA_Log, TERRA_TextureAtlas, TERRA_Image, TERRA_Application, TERRA_ResourceManager;

{ Milkshape3DObject }
Function Milkshape3DObject.GetMaterialFile(MaterialIndex:Integer; SourceFile:AnsiString):AnsiString;
Begin
  If (MaterialIndex=-1) Or (Materials[MaterialIndex].Texture[1]=#0) Then
  Begin
    Result := '';
    Exit;
  End;

  Result := StringTrim(Materials[MaterialIndex].Texture);
  Result := GetOSIndependentFileName(Result);
  If (Pos('.',Result)<=0) Then
    Result := Result + '.png';

  If (FileStream.Exists(Result)) Then
    Exit;

  Result := GetFilePath(SourceFile)+GetFileName(Result,False);
  If (FileStream.Exists(Result)) Then
    Exit;

  Result := GetFileName(Result, False);
  //Result := ResourceManager.Instance.SearchResourceFile(Result);
End;



Function Milkshape3DObject.Load(Source:Stream):Boolean;
Var
  I,J,Index:Integer;
  Dest:FileStream;
  P:Vector3D;
  PName:AnsiString;
Begin
  Self.Clear();

  Result := False;
  Source.Read(@Header, SizeOf(Header));
  If Header.Id<>MS3D_Header Then
  Begin
    RaiseError('LoadMS3D: Invalid MS3D model.');
    Exit;
  End;

    Source.Read(@NumVertices,SizeOf(NumVertices));
    SetLength(Vertices,NumVertices);
    Source.Read(@Vertices[0],SizeOf(Milkshape3DVertex)*NumVertices);
    Source.Read(@NumTriangles,SizeOf(NumTriangles));
    SetLength(Triangles,NumTriangles);
    If NumTriangles>0 Then
      Source.Read(@Triangles[0],SizeOf(Milkshape3DTriangle)*NumTriangles);

    Source.Read(@NumGroups,SizeOf(NumGroups));
    SetLength(Groups,NumGroups);
    For I:=0 To Pred(NumGroups) Do
    With Groups[I] Do
    Begin
      Source.Read(@Flags,SizeOf(Flags));
      Source.Read(@Name,SizeOf(Name));
      Source.Read(@NumTriangles,SizeOf(NumTriangles));
      SetLength(TriangleIndices,NumTriangles);
      If NumTriangles>0 Then
        Source.Read(@TriangleIndices[0],SizeOf(Word)*NumTriangles);
      Source.Read(@MaterialIndex,SizeOf(MaterialIndex));
    End;

    Source.Read(@NumMaterials,SizeOf(NumMaterials));
    SetLength(Materials,NumMaterials);
    For I:=0 To Pred(NumMaterials) Do
    Begin
      Source.Read(@Materials[I],SizeOf(Milkshape3DMaterial)-SizeOf(String));
    End;

    Source.Read(@AnimationFPS,SizeOf(AnimationFPS));
    Source.Read(@CurrentTime,SizeOf(CurrentTime));
    Source.Read(@TotalFrames,SizeOf(TotalFrames));
    Source.Read(@NumJoints,SizeOf(NumJoints));
    SetLength(Joints,NumJoints);
    For I:=0 To NumJoints-1 Do
    With Joints[I] Do
    Begin
      Source.Read(@Flags,SizeOf(Flags));
      Source.Read(@Name,SizeOf(Name));
      Source.Read(@ParentName,SizeOf(ParentName));
      Source.Read(@Rotation,SizeOf(Rotation));
      Source.Read(@Position,SizeOf(Position));
      Source.Read(@NumKeyFramesRot,SizeOf(NumKeyFramesRot));
      Source.Read(@NumKeyFramesTrans,SizeOf(NumKeyFramesTrans));
      SetLength(KeyFramesRot,NumKeyFramesRot);
      SetLength(KeyFramesTrans,NumKeyFramesTrans);
      If (NumKeyFramesRot>0) Then
	      Source.Read(@KeyFramesRot[0],SizeOf(Milkshape3DKeyFrame)*NumKeyFramesRot);
	  If (NumKeyFramesTrans>0) Then
	      Source.Read(@KeyFramesTrans[0],SizeOf(Milkshape3DKeyFrame)*NumKeyFramesTrans);
    End;

    If Source.Position<Source.Size Then
    Begin
      Source.Read(@SubVersion,SizeOf(SubVersion));
      If SubVersion>=1 Then
      Begin
        Source.Read(@NumGroupComments,SizeOf(NumGroupComments));
        For I:=1 To NumGroupComments Do
        Begin
          Source.Read(@Index, SizeOf(Index));
          Source.Read(@J, SizeOf(J));
          SetLength(Groups[Index].Comment,J);
          Source.Read(@Groups[Index].Comment[1], J);
        End;

        Source.Read(@NumMaterialComments, SizeOf(NumMaterialComments));
        For I:=1 To NumMaterialComments Do
        Begin
          Source.Read(@Index, SizeOf(Index));
          Source.Read(@J, SizeOf(J));
          SetLength(Materials[Index].Comment,J);
          Source.Read(@Materials[Index].Comment[1], J);
        End;

        Source.Read(@NumJointsComments,SizeOf(NumJointsComments));
        For I:=1 To NumJointsComments Do
        Begin
          Source.Read(@Index, SizeOf(Index));
          Source.Read(@J, SizeOf(J));
          SetLength(Joints[Index].Comment,J);
          Source.Read(@Joints[Index].Comment[1], J);
        End;

        Source.Read(@NumModelComments,SizeOf(Integer));
        If NumModelComments>0 Then
        Begin
          Source.Read(@J, SizeOf(Integer));
          SetLength(Comment,J);
          Source.Read(@Comment[1], J);
        End;
      End;
    End;

  For I:=0 To Pred(NumJoints) Do
  Begin
    Joints[I].Parent := Nil;
    For J:=0 To Pred(NumJoints) Do
    If (StringEquals(StringTrim(Joints[J].Name), StringTrim(Joints[I].ParentName))) Then
    Begin
      Joints[I].Parent := @Joints[J];
      Break;
    End;

    Joints[I].Ready := False;
  End;

  For I:=0 To Pred(NumJoints)  Do
    Joints[I].Init();

  //Dest := FileStream.Create('d:\code\minimonhd\trunk\output\bones.txt');
  For I:=0 To Pred(NumJoints) Do
  Begin
    Joints[I].AbsolutePosition := Joints[I].AbsoluteMatrix.Transform(VectorZero);
    Joints[I].RelativePosition := Joints[I].RelativeMatrix.Transform(VectorZero);

    {If Assigned(Joints[I].Parent) Then
      VectorSubtract(Joints[I].Parent.TargetPosition);
    Else
      Joints[I].TargetPosition := P;}

    {If Assigned(Joints[I].Parent) Then
      PName := StrClean(Joints[I].Parent.Name)
    Else
      PName := '';
    Dest.WriteLine(StrClean(Joints[I].Name)+' ('+PName+') '+FloatToString(Joints[I].TargetPosition.X)+' '+FloatToString(Joints[I].TargetPosition.Y)+' '+FloatToString(Joints[I].TargetPosition.Z));
    }
  End;
  //ReleaseObject(Dest);

  Result := True;
End;

Function Milkshape3DObject.Save(Dest:Stream):Boolean;
Var
  I,J,Index:Integer;
Begin
  Result := True;
  Header.Version := 4;
  For I:=1 To 10 Do
    Header.Id[I] := MS3D_HEADER[I];
  Dest.Write(@Header,SizeOf(Header));

    Dest.Write(@NumVertices,SizeOf(NumVertices));
    Dest.Write(@Vertices[0],SizeOf(Milkshape3DVertex)*NumVertices);
    Dest.Write(@NumTriangles,SizeOf(NumTriangles));
    Dest.Write(@Triangles[0],SizeOf(Milkshape3DTriangle)*NumTriangles);

    Dest.Write(@NumGroups,SizeOf(NumGroups));
    For I:=0 To Pred(NumGroups) Do
    With Groups[I] Do
    Begin
      Dest.Write(@Flags,SizeOf(Flags));
      Dest.Write(@Name,SizeOf(Name));
      Dest.Write(@NumTriangles,SizeOf(NumTriangles));
      If NumTriangles>0 Then
        Dest.Write(@TriangleIndices[0], SizeOf(Word)*NumTriangles);
      Dest.Write(@MaterialIndex,SizeOf(MaterialIndex));
    End;

    Dest.Write(@NumMaterials,SizeOf(NumMaterials));
    For I:=0 To Pred(NumMaterials) Do
    Begin
      Dest.Write(@Materials[I],SizeOf(Milkshape3DMaterial)-SizeOf(String));
    End;

    Dest.Write(@AnimationFPS,SizeOf(AnimationFPS));
    Dest.Write(@CurrentTime,SizeOf(CurrentTime));
    Dest.Write(@TotalFrames,SizeOf(TotalFrames));
    Dest.Write(@NumJoints,SizeOf(NumJoints));

    For I:=0 To NumJoints-1 Do
    With Joints[I] Do
    Begin
      Dest.Write(@Flags,SizeOf(Flags));
      Dest.Write(@Name,SizeOf(Name));
      Dest.Write(@ParentName,SizeOf(ParentName));
      Dest.Write(@Rotation,SizeOf(Rotation));
      Dest.Write(@Position,SizeOf(Position));
      Dest.Write(@NumKeyFramesRot,SizeOf(NumKeyFramesRot));
      Dest.Write(@NumKeyFramesTrans,SizeOf(NumKeyFramesTrans));
      SetLength(KeyFramesRot,NumKeyFramesRot);
      SetLength(KeyFramesTrans,NumKeyFramesTrans);
      If NumKeyFramesRot>0 Then
        Dest.Write(@KeyFramesRot[0],SizeOf(Milkshape3DKeyFrame)*NumKeyFramesRot);
      If NumKeyFramesTrans>0 Then
        Dest.Write(@KeyFramesTrans[0],SizeOf(Milkshape3DKeyFrame)*NumKeyFramesTrans);
    End;

      SubVersion := 1;
      Dest.Write(@SubVersion,SizeOf(SubVersion));
      If SubVersion>=1 Then
      Begin
        Dest.Write(@NumGroupComments,SizeOf(NumGroupComments));
        For I:=1 To NumGroupComments Do
        Begin
          Dest.Write(@Index,SizeOf(Index));
          J := Length(Groups[Index].Comment);
          Dest.Write(@J, SizeOf(J));
          If J>0 Then
            Dest.Write(@Groups[Index].Comment[1],J);
        End;

        Dest.Write(@NumMaterialComments,SizeOf(NumMaterialComments));
        For I:=1 To NumMaterialComments Do
        Begin
          Dest.Write(@Index, SizeOf(Index));
          J := Length(Materials[Index].Comment);
          Dest.Write(@J, SizeOf(J));
          If J>0 Then
            Dest.Write(@Materials[Index].Comment[1],J);
        End;

        Dest.Write(@NumJointsComments, SizeOf(NumJointsComments));
        For I:=1 To NumJointsComments Do
        Begin
          Dest.Write(@Index,SizeOf(Index));
          J := Length(Joints[Index].Comment);
          Dest.Write(@J,SizeOf(J));
          If J>0 Then
          Dest.Write(@Joints[Index].Comment[1],J);
        End;

        Dest.Write(@NumModelComments,SizeOf(Integer));
        If NumModelComments>0 Then
        Begin
          J := Length(Comment);
          Dest.Write(@J,SizeOf(Integer));
          Dest.Write(@Comment[1],J);
        End;
      End;

End;

Function Milkshape3DObject.ConvertTexturesToTextureAtlas(FileName:AnsiString):Boolean;
Var
  I,J,K:Integer;
  Index:Integer;
  S:AnsiString;
  MyTextureAtlas:TextureAtlas;
  CW,CH:Integer;
  Img:Image;
  Textures:Array Of Image;
  CI:Array Of TextureAtlasItem;

  UOffset,UScale:Single;
  VOffset,VScale:Single;
  TextureName:AnsiString;
Begin
  CW := 0;
  CH := 0;
  SetLength(Textures, NumMaterials);
  For I:=0 To Pred(NumMaterials) Do
  Begin
    S := GetMaterialFile(I, FileName);
    If (S='') Then
      Continue;
    Textures[I] := Image.Create(S);
    If (Textures[I].Width>CW) Then
      CW := Textures[I].Width;
    If (Textures[I].Height>CH) Then
      CH := Textures[I].Height;
  End;

  SetLength(CI,NumMaterials);

  CW := NearestPowerOfTwo(CW);
  CH := NearestPowerOfTwo(CH);

  Repeat
    MyTextureAtlas := TextureAtlas.Create('ms3d', CW, CH);
    For I:=0 To Pred(NumMaterials) Do
      CI[I] := MyTextureAtlas.Add(Textures[I], Materials[I].Name);

    Result := MyTextureAtlas.Update;

    If Result Then
    Begin
      TextureName := 'textures\'+GetFileName(FileName,True)+'.png';
      Img := MyTextureAtlas.GetTexture(0).GetImage;
      Img.Save(TextureName);
      ReleaseObject(Img);
    End Else
    Begin
      If (CW<=CH) Then
        CW := CW * 2
      Else
        CH := CH * 2;
      ReleaseObject(MyTextureAtlas);

      If (CW>2048) Or (CH>2048) Then
        Exit;
    End;

  Until Result;
  For I:=0 To Pred(NumMaterials) Do
    ReleaseObject(Textures[I]);

  For J:=0 To Pred(NumGroups) Do
  Begin
    Index := Groups[J].MaterialIndex;
    If Index<0 Then
      Continue;
    Groups[J].MaterialIndex := 0;

    UOffset := CI[Index].X1;
    UScale := CI[Index].Buffer.Width / CW;

    VOffset := CI[Index].Y1;
    VScale := CI[Index].Buffer.Height / CH;

    For I:=0 To Pred(Groups[J].NumTriangles) Do
    For K:=1 To 3 Do
    Begin
      Triangles[Groups[J].TriangleIndices[I]].S[K] := UOffset + Triangles[Groups[J].TriangleIndices[I]].S[K] * UScale;
      Triangles[Groups[J].TriangleIndices[I]].T[K] := VOffset + Triangles[Groups[J].TriangleIndices[I]].T[K] * VScale;
    End;
  End;

  ReleaseObject(MyTextureAtlas);

  NumMaterials := 1;
  S:=TextureName;
  For I:=1 To Length(S) Do
  Materials[0].Texture[I] := S[I];
  Materials[0].Texture[Succ(Length(S))] := #0;
End;

Function Milkshape3DObject.IsJointUsed(Index:Integer):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self.NumVertices) Do
  If (Self.Vertices[I].BoneIndex = Index) Then
  Begin
    Result := True;
    Exit;
  End;


  For I:=0 To Pred(Self.NumJoints) Do
  If (IsChildOf(I, Index)) And (IsJointUsed(I)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure Milkshape3DObject.RemoveJoint(Index: Integer);
Var
  I:Integer;
Begin
  //WriteLn('removing ',Joints[Index].Name);ReadLn;

  For I:=Index To NumJoints-2 Do
    Joints[I] := Joints[I+1];

  For I:=0 To Pred(NumVertices) Do
  If (Vertices[I].BoneIndex>=Index) Then
    Dec(Vertices[I].BoneIndex);


  Dec(NumJoints);
End;

Function Milkshape3DObject.GetParentOf(Child:Integer):Integer;
Var
  I:Integer;
  S, PName:AnsiString;
Begin
  PName := StringTrim(Joints[Child].ParentName);
  For I:=0 To Pred(NumJoints) Do
  Begin
    S := StringTrim(Joints[I].Name);
    If (StringEquals(S, PName)) Then
    Begin
      Result := I;
      Exit;
    End;
  End;

  Result := -1;
End;

Function Milkshape3DObject.IsChildOf(Child, Parent: Integer): Boolean;
Var
  N:Integer;
Begin
  If (Parent = -1) Then
  Begin
    Result := False;
    Exit;
  End;

  N := GetParentOf(Child);
  Result := N = Parent;
  If (Result) Or (N<0) Then
    Exit;

  Result := IsChildOf(N, Parent);
End;

Procedure Milkshape3DObject.Clear;
Var
  I,J:Integer;
Begin
    Header.Version :=0;
    NumVertices:= 0;
    NumTriangles := 0;
    NumGroups := 0;
    NumMaterials :=0 ;
    AnimationFPS :=0 ;
    CurrentTime := 0;
    TotalFrames := 0;
    NumJoints :=0 ;
    SubVersion := 0;
    NumGroupComments := 0;
    NumMaterialComments := 0;
    NumJointsComments := 0;
    NumModelComments :=0 ;
    Comment := '';

    SetLength(Vertices, 0);
    SetLength(Triangles, 0);
    SetLength(Groups, 0);
    SetLength(Materials, 0);
    SetLength(Joints, 0);
End;

{ Milkshape3DModel }

Function Milkshape3DModel.GetMaterial(GroupID: Integer): PMilkshape3DMaterial;
Var
  Index:Integer;
Begin
  Index := _MS3D.Groups[GroupID].MaterialIndex;
  If (Index>=0) Then
    Result := @(_MS3D.Materials[Index])
  Else
    Result := Nil;
End;


Function Milkshape3DModel.Load(Source: Stream): Boolean;
Var
  I,J,K:Integer;
  N,W,Z:Integer;
  Group:^MS3DGroup;
  VP:MS3DVertex;
Begin
  Result := False;
  If Not _MS3D.Load(Source) Then
    Exit;

  SetLength(_Groups, _MS3D.NumGroups);
  For N:=0 To Pred(_MS3D.NumGroups) Do
  Begin
    Group := @(_Groups[N]);
    Group.TriangleCount := _MS3D.Groups[N].NumTriangles;
    SetLength(Group.Triangles, Group.TriangleCount);
    Group.VertexCount := 0;

    For I:=0 To Pred(Group.TriangleCount) Do
      For J:=0 To 2 Do
        Begin
          W:=-1;

			    Z := _MS3D.Groups[N].TriangleIndices[I];
			    VP.Position := _MS3D.Vertices[_MS3D.Triangles[Z].VertexIndices[Succ(J)]].Vertex;
			    VP.Normal :=  _MS3D.Triangles[Z].VertexNormals[Succ(J)];
			    VP.TexCoords.X:= _MS3D.Triangles[Z].S[Succ(J)];
			    VP.TexCoords.Y := _MS3D.Triangles[Z].T[Succ(J)];
			    VP.BoneIndex := Succ(_MS3D.Vertices[_MS3D.Triangles[Z].VertexIndices[Succ(J)]].BoneIndex);

			    For K:=0 To Pred(Group.VertexCount) Do
			    If (Group.Vertices[K].Position.Distance(VP.Position)<=0.001) And (Group.Vertices[K].Normal.Distance(VP.Normal)<=0.001)
			    And(Group.Vertices[K].TexCoords.Distance(VP.TexCoords)<=0.001) And (Group.Vertices[K].BoneIndex=VP.BoneIndex) Then
			    Begin
				    W:=K;
				    Break;
			    End;

			    If W=-1 Then
          Begin
				    Inc(Group.VertexCount);
            SetLength(Group.Vertices, Group.VertexCount);
				    W := Pred(Group.VertexCount);
				    Group.Vertices[W] := VP;
			    End;

		      Group.Triangles[I].Indices[J] := W;
		    End;
  End;

  Result := True;
End;

Class Function Milkshape3DModel.Save(Dest:Stream; MyMesh:MeshFilter):Boolean;
Var
  I,J,K, N:Integer;
  AnimID:Integer;
  Count:Integer;
  MS3D:Milkshape3DObject;
  S:AnsiString;
  T:Triangle;
  UV:Vector2D;
  VOfs, TOfs:Integer;
  Key:MeshVectorKey;
  Ratio:Single;

  Function ClampTime(T:Single):Single;
  Begin
    Result := Round(T * MS3D.AnimationFPS  * Ratio) / MS3D.AnimationFPS;
  End;

  Function NewColor(C:Color):Milkshape3DColor;
  Begin
    Result.R := C.R / 255;
    Result.G := C.G / 255;
    Result.B := C.B / 255;
    Result.A := C.A / 255;
  End;

  Function GetMaterial(ID:Integer):Integer;
  Var
    I,N:Integer;
    S:AnsiString;
    DiffuseMap:AnsiString;
  Begin
    DiffuseMap := MyMesh.GetDiffuseMapName(ID);
    For I:=0 To Pred(MS3D.NumMaterials) Do
    If (Copy(MS3D.Materials[I].Texture, 1, Pred(Pos(#0,MS3D.Materials[I].Texture)))  = DiffuseMap) Then
    Begin
      Result := I;
      Exit;
    End;

    N := MS3D.NumMaterials;
    Inc(MS3D.NumMaterials);
    SetLength(MS3D.Materials, MS3D.NumMaterials);
    S := 'mat'+IntToString(N);
    Move(S[1], MS3D.Materials[N].Name[1], Length(S));
    MS3D.Materials[N].Name[Succ(Length(S))] := #0;
    MS3D.Materials[N].Ambient := NewColor(ColorBlack);
    MS3D.Materials[N].Diffuse := NewColor(MyMesh.GetDiffuseColor(N));
    MS3D.Materials[N].Specular := NewColor(ColorBlack);
    MS3D.Materials[N].Emissive := NewColor(ColorBlack);
    MS3D.Materials[N].Shininess := 0;
    MS3D.Materials[N].Transparency := MS3D.Materials[N].Diffuse.A;
    MS3D.Materials[N].Mode := 0;
    MS3D.Materials[N].Comment := '';
    S := DiffuseMap;
    If (S<>'') Then
    Begin
      Move(S[1], MS3D.Materials[N].Texture[1], Length(S));
      MS3D.Materials[N].Texture[Succ(Length(S))] := #0;
    End Else
      MS3D.Materials[N].Texture[1] := #0;
    MS3D.Materials[N].AlphaMap[1] := #0;

    Result := N;
  End;
Begin
	Result := True;
  FillChar(MS3D, SizeOf(MS3D), 0);
  MS3D.NumGroups := MyMesh.GetGroupCount;
  MS3D.NumVertices := 0;
  MS3D.NumTriangles := 0;
  MS3D.NumMaterials := 0;
  MS3D.NumJoints := 0;
  MS3D.SubVersion := 1;

  SetLength(MS3D.Groups, MS3D.NumGroups);
  SetLength(MS3D.Materials, 0);

  VOfs := 0;
  TOfs := 0;
  For N:=0 To Pred(MS3D.NumGroups) Do
  Begin
    S := MyMesh.GetGroupName(N);
    If S<>'' Then
      Move(S[1], MS3D.Groups[N].Name[1], Length(S));
    MS3D.Groups[N].Name[Succ(Length(S))] := #0;
    MS3D.Groups[N].MaterialIndex := GetMaterial(N);
    MS3D.Groups[N].NumTriangles := MyMesh.GetTriangleCount(N);
    SetLength(MS3D.Groups[N].TriangleIndices, MS3D.Groups[N].NumTriangles);
    MS3D.Groups[N].Comment := '';
    MS3D.Groups[N].Flags := 0;

    If (MyMesh.GetGroupFlags(N) And meshGroupHidden<>0) Then
      MS3D.Groups[N].Flags := MS3D.Groups[N].Flags Or MS3D_HIDDEN;

    Inc(MS3D.NumTriangles, MS3D.Groups[N].NumTriangles);
    SetLength(MS3D.Triangles, MS3D.NumTriangles);
    For I:=0 To Pred(MS3D.Groups[N].NumTriangles) Do
    Begin
      T := MyMesh.GetTriangle(N, I);
      MS3D.Groups[N].TriangleIndices[I] := TOfs + I;
      MS3D.Triangles[TOfs + I].Flags := MS3D.Groups[N].Flags;
      For J:=0 To 2 Do
      Begin
        UV := MyMesh.GetVertexUV(N, T.Indices[J]);
        MS3D.Triangles[TOfs + I].VertexIndices[Succ(J)] := T.Indices[J] + VOfs;
        MS3D.Triangles[TOfs + I].VertexNormals[Succ(J)] := MyMesh.GetVertexNormal(N, T.Indices[J]);
        MS3D.Triangles[TOfs + I].S[Succ(J)] := UV.X;
        MS3D.Triangles[TOfs + I].T[Succ(J)] := 1.0 - UV.Y;
        MS3D.Triangles[TOfs + I].SmoothingGroup := 1;
        MS3D.Triangles[TOfs + I].GroupIndex := N;
      End;
    End;
    Inc(TOfs, MS3D.Groups[N].NumTriangles);

    Count := MyMesh.GetVertexCount(N);
    Inc(MS3D.NumVertices, Count);
    SetLength(MS3D.Vertices, MS3D.NumVertices);
    For I:=0 To Pred(Count) Do
    Begin
      MS3D.Vertices[VOfs + I].Vertex := MyMesh.GetVertexPosition(N, I);
      MS3D.Vertices[VOfs + I].BoneIndex := MyMesh.GetVertexBone(N, I) -1;
      MS3D.Vertices[VOfs + I].Flags := MS3D.Groups[N].Flags;
      MS3D.Vertices[VOfs + I].ReferenceCount := 1;
    End;
    Inc(VOfs, Count);
  End;

  AnimID := 0;
  MS3D.TotalFrames := Trunc(MyMesh.GetAnimationDuration(AnimID));
  If (MS3D.TotalFrames>0) Then
    Ratio := Succ(MS3D.TotalFrames) / MS3D.TotalFrames
  Else
    Ratio := 1.0;
  Inc(MS3D.TotalFrames);
  MS3D.CurrentTime := 0;
  MS3D.AnimationFPS := 24.0;


  If (MS3D.TotalFrames>0) Then
  Begin
    MS3D.NumModelComments := 1;
    MS3D.Comment := 'Name='+MyMEsh.GetAnimationName(AnimID)+', Start=1, End='+IntToString(MS3D.TotalFrames)+', Loop='+BoolToString(MyMesh.GetAnimationLoop(AnimID))+#0;
    MS3D.Comment := StringLower(MS3D.Comment);
  End;
  
  MS3D.NumJoints := MyMesh.GetBoneCount;
  SetLength(MS3D.Joints, MS3D.NumJoints);
  For I:=0 To Pred(MS3D.NumJoints) Do
  Begin
    S := MyMesh.GetBoneName(I);
    If (S<>'') Then
    Begin
      For J:=1 To Length(S) Do
        MS3D.Joints[I].Name[J] := S[J];
      MS3D.Joints[I].Name[Length(S)+1] := #0;
    End;

    J := MyMesh.GetBoneParent(I);
    If (J>=0) Then
    Begin
      S := MyMesh.GetBoneName(J);
      For J:=1 To Length(S) Do
        MS3D.Joints[I].ParentName[J] := S[J];
      MS3D.Joints[I].ParentName[Length(S)+1] := #0;
    End Else
      MS3D.Joints[I].ParentName[1] := #0;

//      WriteLn(MS3D.Joints[I].Name, ' parent is ',MS3D.Joints[I].ParentName);

      MS3D.Joints[I].NumKeyFramesTrans := MyMesh.GetPositionKeyCount(AnimID, I);
      MS3D.Joints[I].NumKeyFramesRot := MyMesh.GetRotationKeyCount(AnimID, I);
      MS3D.Joints[I].Position := MyMesh.GetBonePosition(I);
      MS3D.Joints[I].Rotation := MyMesh.GetBoneRotation(I);
      MS3D.Joints[I].Flags := 0;

      SetLength(MS3D.Joints[I].KeyFramesTrans, MS3D.Joints[I].NumKeyFramesTrans);
      For J := 0 To Pred(MS3D.Joints[I].NumKeyFramesTrans) Do
      Begin
        Key := MyMesh.GetPositionKey(AnimID, I, J);
        MS3D.Joints[I].KeyFramesTrans[J].Time := ClampTime(Key.Time);
        MS3D.Joints[I].KeyFramesTrans[J].Vector := Key.Value;
      End;

      MS3D.Joints[I].NumKeyFramesRot := MyMesh.GetRotationKeyCount(AnimID, I);
      SetLength(MS3D.Joints[I].KeyFramesRot, MS3D.Joints[I].NumKeyFramesRot);
      For J := 0 To Pred(MS3D.Joints[I].NumKeyFramesRot) Do
      Begin
        Key := MyMesh.GetRotationKey(AnimID, I, J);
        MS3D.Joints[I].KeyFramesRot[J].Time := ClampTime(Key.Time);
        MS3D.Joints[I].KeyFramesRot[J].Vector := Key.Value;
      End;
  End;

  MS3D.Save(Dest);
End;

Function GetColor(C:Milkshape3DColor):Color;
Begin
  Result.R := Trunc(C.R*255);
  Result.G := Trunc(C.G*255);
  Result.B := Trunc(C.B*255);
  Result.A := Trunc(C.A*255);
End;

Function Milkshape3DModel.GetDiffuseColor(GroupID:Integer):Color;
Var
  Mat:PMilkshape3DMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
  Begin
    Result := GetColor(Mat.Diffuse);
    Result.A := Trunc(255*Mat.Transparency);
  End Else
    Result := ColorWhite;
End;

{Function Milkshape3DModel.GetSpecularColor(GroupID: Integer): Color;
Var
  Mat:PMilkshape3DMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := GetColor(Mat.Specular)
  Else
    Result := ColorWhite;
End;

Function Milkshape3DModel.GetSpecularFactor(GroupID: Integer): Single;
Var
  Mat:PMilkshape3DMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := Mat.Shininess
  Else
    Result := 0.0;
End;}

Function Milkshape3DModel.GetSpecularMapName(GroupID: Integer):AnsiString;
Var
  S:AnsiString;
Begin
  S := Self.GetDiffuseMapName(GroupID);
  If S<>'' Then
    Result := S + '_spec'
  Else
    Result := '';
End;

Function Milkshape3DModel.GetEmissiveMapName(GroupID: Integer):AnsiString;
Var
  S:AnsiString;
Begin
  S := Self.GetDiffuseMapName(GroupID);
  If S<>'' Then
    Result := S + '_glow'
  Else
    Result := '';
End;

Function Milkshape3DModel.GetDiffuseMapName(GroupID: Integer):AnsiString;
Var
  I:Integer;
  Mat:PMilkshape3DMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
  Begin
    Result := '';
    For I:=1 To 128 Do
    If Mat.Texture[I]>=#32 Then
      Result := Result + Mat.Texture[I]
    Else
      Break;

    Result := StringTrimLeft(Result);
  End Else
    Result := '';
End;

{Function Milkshape3DModel.GetEmissiveFactor(GroupID: Integer): Single;
Var
  Mat:PMilkshape3DMaterial;
Begin
  Mat := Self.GetMaterial(GroupID);
  If Assigned(Mat) Then
    Result := ColorLuminance(GetColor(Mat.Emissive))/255
  Else
    Result := 0.0;
End;}

Function Milkshape3DModel.GetGroupBlendMode(GroupID: Integer): Cardinal;
Begin
  Result := _Groups[GroupID].BlendMode;
End;

Function Milkshape3DModel.GetGroupCount: Integer;
Begin
  Result := Self._MS3D.NumGroups;
end;

Function Milkshape3DModel.GetGroupFlags(GroupID: Integer): Cardinal;
Begin
  Result := _Groups[GroupID].Flags;
End;

Function Milkshape3DModel.GetGroupName(GroupID: Integer):AnsiString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=1 To 32 Do
  If (_MS3D.Groups[GroupID].Name[I]>#32) Then
    Result := Result + _MS3D.Groups[GroupID].Name[I]
  Else
    Break;
End;

Function Milkshape3DModel.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Groups[GroupID].Triangles[Index];
End;

Function Milkshape3DModel.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _MS3D.Groups[GroupID].NumTriangles;
End;

Function Milkshape3DModel.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Groups[GroupID].VertexCount;
end;

Function Milkshape3DModel.GetVertexFormat(GroupID: Integer): VertexFormat;
Begin
  Result := [vertexFormatPosition, vertexFormatNormal, vertexFormatUV0, vertexFormatBone];
End;

Function Milkshape3DModel.GetVertexBone(GroupID, Index: Integer): Integer;
Begin
  Result := _Groups[GroupID].Vertices[Index].BoneIndex;
End;

Function Milkshape3DModel.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Normal;
End;

Function Milkshape3DModel.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Position;
End;

Function Milkshape3DModel.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Groups[GroupID].Vertices[Index].TexCoords;
End;


{ Milkshape3DJoint }
Procedure Milkshape3DJoint.Init;
Begin
  If (Ready) Then
    Exit;

  If (Assigned(Parent)) And (Not Parent.Ready) Then
    Parent.Init;

  RelativeMatrix := Matrix4x4Multiply4x3(Matrix4x4Translation(Position), Matrix4x4Rotation(Rotation));

	// Each bone's final matrix is its relative matrix concatenated onto its
	// parent's final matrix (which in turn is ....)
	//
	If ( Parent = nil ) Then					// this is the root node
  Begin
    AbsoluteMatrix := RelativeMatrix;
  End Else									// not the root node
	Begin
		// m_final := parent's m_final * m_rel (matrix concatenation)
    AbsoluteMatrix := Matrix4x4Multiply4x3(Parent.AbsoluteMatrix, RelativeMatrix);
	End;

  Ready := True;
End;

Initialization
  RegisterMeshFilter(Milkshape3DModel, 'MS3D');
End.
