Unit TERRA_Assimp;

{$I terra.inc}

Interface
Uses TERRA_Mesh, TERRA_Math, TERRA_Utils, TERRA_IO,
  TERRA_FileIO, TERRA_FileUtils, TERRA_MeshFilter, TERRA_OS, TERRA_Quaternion,
  TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_Matrix, Assimp;

Function ASSIMP_Import(SourceFile, TargetDir:AnsiString):AnsiString;

Type
  AssimpBone = Class
    ID:Integer;
    Name:AnsiString;
    LocalTransform:Matrix;
    GlobalTransform:Matrix;
    Parent:AssimpBone;
    Node:PAINode;
  End;

  AssimpFilter = Class(MeshFilter)
    Protected
      scene:PaiScene;

      Bones:Array Of AssimpBone;
      BoneCount:Integer;

      Function FindNode(Name:AnsiString; Root:pAiNode):pAiNode;

      //Function GetBoneAt(Var BoneID:Integer):Integer;
      Function GetBoneIDByName(Name:AnsiString):Integer;

    Public
      Constructor Create(Source:AnsiString);
      Destructor Destroy;

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexTangent(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexHandness(GroupID, Index:Integer):Single; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;
      Function GetVertexUV2(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;
      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;

      Function GetAnimationCount():Integer; Override;
      Function GetAnimationName(Index:Integer):AnsiString; Override;
      Function GetAnimationDuration(Index:Integer):Single; Override;

      Function GetPositionKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetRotationKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetScaleKeyCount(AnimationID, BoneID:Integer):Integer; Override;

      Function GetPositionKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey; Override;
      Function GetScaleKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey; Override;
      Function GetRotationKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey; Override;

      Function GetBoneCount():Integer; Override;
      Function GetBoneName(BoneID:Integer):AnsiString; Override;
      Function GetBoneParent(BoneID:Integer):Integer; Override;
      Function GetBonePosition(BoneID:Integer):Vector3D; Override;
  End;


Implementation
Uses TERRA_GraphicsManager;

Function ASSIMP_Import(SourceFile, TargetDir:AnsiString):AnsiString;
Var
  dest:FileStream;
  mymesh:Mesh;
  group:MeshGroup;
  I, J, K, N:Integer;
  X,Y,Z:Single;
  T:Triangle;
  Filter:MeshFilter;
Begin
  WriteLn('ASSIMP_Import: ',SourceFile);
  filter := ASSimpFilter.Create(SourceFile);
  If Assigned(ASSimpFilter(Filter).scene) Then
  Begin
    MyMesh := Mesh.CreateFromFilter(Filter);

    //ModelMilkshape3D.Save(TargetDir + PathSeparator + GetFileName(SourceFile, True)+'.ms3d', Filter);

    WriteLn('Saving...');
    Result := TargetDir + PathSeparator + GetFileName(SourceFile, True)+'.mesh';
    Dest := FileStream.Create(Result);
    MyMesh.Save(Dest);
    Dest.Destroy;
    MyMesh.Destroy;

    Filter.Destroy;
  End Else
  Begin
    Writeln('ASSIMP:Error!');
    Result := '';
  End;
End;

Var
  c:aiLogStream;
{ AssimpFilter }

Constructor AssimpFilter.Create(Source:AnsiString);
Var
  Flags:Cardinal;
  I, J, N:Integer;
  node, P:PAInode;
  S:AnsiString;
  M:Matrix;
Begin
  flags := 	aiProcess_CalcTangentSpace Or
	aiProcess_GenSmoothNormals				Or
//	aiProcess_JoinIdenticalVertices			Or
	aiProcess_ImproveCacheLocality			Or
	aiProcess_LimitBoneWeights				Or
	aiProcess_RemoveRedundantMaterials  Or
	aiProcess_SplitLargeMeshes				Or
	aiProcess_Triangulate					Or
	aiProcess_GenUVCoords            Or
	aiProcess_SortByPType            Or
	//aiProcess_FindDegenerates        Or
	aiProcess_FindInvalidData;

  scene := aiImportFile(PAnsiChar(Source), flags);
  If (Scene = Nil) Then
    Exit;

  BoneCount := 0;
  For I:=0 To Pred(scene.mNumMeshes) Do
  If (scene.mMeshes[I].mNumBones>0) Then
  Begin
    BoneCount := 1;
    SetLength(Bones, BoneCount);
    Bones[0] := AssimpBone.Create;
    Bones[0].ID := 0;
    Bones[0].Name := aiStringGetValue(Scene.mRootNode.mName);
    Bones[0].Parent := Nil;
    Bones[0].Node := Scene.mRootNode;
    Break;
  End;

  For I:=0 To Pred(scene.mNumMeshes) Do
    For J:=0 To Pred(Scene.mMeshes[I].mNumBones) Do
    If (Self.GetBoneIDByName(aiStringGetValue(Scene.mMeshes[I].mBones[J].mName))<0) Then
    Begin
      Inc(BoneCount);
      SetLength(Bones, BoneCount);
      Bones[Pred(BoneCount)] := AssimpBone.Create;
      Bones[Pred(BoneCount)].ID := Pred(BoneCount);
      Bones[Pred(BoneCount)].Name := aiStringGetValue(Scene.mMeshes[I].mBones[J].mName);
      Bones[Pred(BoneCount)].Parent := Nil;
      Bones[Pred(BoneCount)].Node := Self.FindNode(Bones[Pred(BoneCount)].Name, Scene.mRootNode);
   End;

  For I:=0 To Pred(BoneCount) Do
  Begin
    Node := Bones[I].Node;
    For J:=0 To 15 Do
      Bones[I].LocalTransform.V[J] := Node.mTransformation.V[J];
      Bones[I].LocalTransform := MatrixTranspose(Bones[I].LocalTransform);
      Bones[I].GlobalTransform := Bones[I].LocalTransform;

    Node := Node.mParent;
    While Assigned(Node) Do
    Begin
      For J:=0 To 15 Do
        M.V[J] := Node.mTransformation.V[J];
        M := MatrixTranspose(M);

      Bones[I].GlobalTransform := MatrixMultiply4x3(M, Bones[I].GlobalTransform);

      N := Self.GetBoneIDByName(aiStringGetValue(Node.mName));
      If (N>=0) And (Bones[I].Parent = Nil) Then
      Begin
        Bones[I].Parent := Bones[N];
        WriteLn(Bones[I].Name ,' parent is ', Bones[N].Name);
      End;

      Node := Node.mParent;
    End;
  End;

    ///Bones[I].Transform := MatrixInverse(Bones[I].Transform);

  For I:=0 To Pred(BoneCount) Do
  If Bones[I].Parent = Nil Then
  Begin
      WriteLn(Bones[I].Name,' has no parent!');
  End;
  //ReadLn;
End;

Destructor AssimpFilter.Destroy;
Begin
    aiReleaseImport(scene);
End;

Function AssimpFilter.GetDiffuseColor(GroupID: Integer): Color;
Begin
  Result := ColorWhite;
End;

Function AssimpFilter.GetDiffuseMapName(GroupID: Integer):AnsiString;
Var
  prop:PAImaterialProperty;
Begin
  aiGetMaterialProperty(scene.mMaterials[scene.mMeshes[GroupID].mMaterialIndex], _AI_MATKEY_TEXTURE_BASE, aiTextureType_DIFFUSE, 0, prop);
  If Assigned(Prop) Then
  Begin
    SetLength(Result, Prop.mDataLength);
    Move(Prop.mData^, Result[1], Prop.mDataLength);
    Result := TrimLeft(TrimRight(Result));
    Result := GetFileName(Result, False);
    StringTofloat(Result);
  End Else
    Result := '';
End;

Function AssimpFilter.GetEmissiveMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function AssimpFilter.GetGroupBlendMode(GroupID: Integer): Cardinal;
Begin
  Result := blendBlend;
End;

Function AssimpFilter.GetGroupCount: Integer;
Begin
  Result := Scene.mNumMeshes;
End;

Function AssimpFilter.GetGroupFlags(GroupID: Integer): Cardinal;
Begin
  Result := meshGroupCastShadow Or meshGroupPick;
End;

function AssimpFilter.GetGroupName(GroupID: Integer):AnsiString;
begin
  Result := aiStringGetValue(scene.mMeshes[GroupID].mName);
  Result := TrimLeft(TrimRight(Result));
  If (Result='') Then
    Result := 'group'+IntToString(GroupID);
end;

Function AssimpFilter.GetSpecularMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

function AssimpFilter.GetTriangle(GroupID, Index: Integer): Triangle;
begin
  Result.Indices[0] := scene.mMeshes[GroupID].mFaces[Index].mIndices[0];
  Result.Indices[1] := scene.mMeshes[GroupID].mFaces[Index].mIndices[1];
  Result.Indices[2] := scene.mMeshes[GroupID].mFaces[Index].mIndices[2];
end;

function AssimpFilter.GetTriangleCount(GroupID: Integer): Integer;
begin
  Result := scene.mMeshes[GroupID].mNumFaces;
end;

Function AssimpFilter.GetVertexBone(GroupID, Index: Integer): Integer;
Var
  I, J ,K:Integer;
  W:Single;
Begin
  Result := -1;
  If (GroupID<0) Or (GroupID>=scene.mNumMeshes) Or (scene = Nil) Then
    Exit;

  W := -1;
  K := GroupID;
  For I:=0 To Pred(scene.mMeshes[K].mNumBones) Do
  Begin
    For J:=0 To Pred(scene.mMeshes[K].mBones[I].mNumWeights) Do
    Begin
      If (scene.mMeshes[K].mBones[I].mWeights[J].mVertexId=Index) And (scene.mMeshes[K].mBones[I].mWeights[J].mWeight>W) Then
      Begin
        Result := GetBoneIDByName(aiStringGetValue(scene.mMeshes[K].mBones[I].mName));
        W := scene.mMeshes[K].mBones[I].mWeights[J].mWeight;
      End;
    End;
  End;
End;

Function AssimpFilter.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  Result := ColorWhite;
End;

Function AssimpFilter.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := scene.mMeshes[GroupID].mNumVertices;
End;

Function AssimpFilter.GetVertexFormat(GroupID: Integer): Cardinal;
Begin
  Result := meshFormatNormal Or meshFormatTangent Or meshFormatUV1;
End;

Function AssimpFilter.GetVertexHandness(GroupID, Index: Integer): Single;
Begin
  Result := 1;
End;

function AssimpFilter.GetVertexNormal(GroupID, Index: Integer): Vector3D;
begin
  Result.X := scene.mMeshes[GroupID].mNormals[Index].X;
  Result.Y := scene.mMeshes[GroupID].mNormals[Index].Y;
  Result.Z := scene.mMeshes[GroupID].mNormals[Index].Z;
end;

Function AssimpFilter.GetVertexPosition(GroupID, Index: Integer): Vector3D;
begin
  Result.X := scene.mMeshes[GroupID].mVertices[Index].X;
  Result.Y := scene.mMeshes[GroupID].mVertices[Index].Y;
  Result.Z := scene.mMeshes[GroupID].mVertices[Index].Z;
end;

Function AssimpFilter.GetVertexTangent(GroupID, Index: Integer): Vector3D;
begin
  Result.X := scene.mMeshes[GroupID].mTangents[Index].X;
  Result.Y := scene.mMeshes[GroupID].mTangents[Index].Y;
  Result.Z := scene.mMeshes[GroupID].mTangents[Index].Z;
end;

Function AssimpFilter.GetVertexUV(GroupID, Index: Integer): Vector2D;
begin
  Result.X := scene.mMeshes[GroupID].mTextureCoords[0][Index].X;
  Result.Y := 1 - scene.mMeshes[GroupID].mTextureCoords[0][Index].Y;
end;

Function AssimpFilter.GetVertexUV2(GroupID, Index: Integer): Vector2D;
begin
  Result.X := scene.mMeshes[GroupID].mTextureCoords[1][Index].X;
  Result.Y := 1- scene.mMeshes[GroupID].mTextureCoords[1][Index].Y;
End;

Function AssimpFilter.GetAnimationCount():Integer;
Begin
  Result := Scene.mNumAnimations;
End;

Function AssimpFilter.GetAnimationName(Index:Integer):AnsiString;
Begin
  Result := aiStringGetValue(Scene.mAnimations[Index].mName);
End;

Function AssimpFilter.GetBoneCount: Integer;
Begin
  Result := Self.BoneCount;
end;

Function AssimpFilter.GetBoneName(BoneID: Integer):AnsiString;
Begin
  Result := Bones[boneID].Name;
End;

Function AssimpFilter.GetAnimationDuration(Index:Integer):Single;
Begin
  Result := Scene.mAnimations[Index].mDuration;
End;

Function AssimpFilter.GetPositionKeyCount(AnimationID, BoneID:Integer):Integer;
Var
  Channel:Integer;
Begin
  If (AnimationID>=Scene.mNumAnimations) Then
  Begin
    Result := 0;
    Exit;
  End;

  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
  Begin
    Result := 0;
    Exit;
  End;
  Result := Scene.mAnimations[AnimationID].mChannels[Channel].mNumPositionKeys;
End;

Function AssimpFilter.GetRotationKeyCount(AnimationID, BoneID:Integer):Integer;
Var
  Channel:Integer;
Begin
  If (AnimationID>=Scene.mNumAnimations) Then
  Begin
    Result := 0;
    Exit;
  End;

  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
  Begin
    Result := 0;
    Exit;
  End;
  Result := Scene.mAnimations[AnimationID].mChannels[Channel].mNumRotationKeys;
End;

Function AssimpFilter.GetScaleKeyCount(AnimationID, BoneID:Integer):Integer;
Var
  Channel:Integer;
Begin
  If (AnimationID>=Scene.mNumAnimations) Then
  Begin
    Result := 0;
    Exit;
  End;

  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
  Begin
    Result := 0;
    Exit;
  End;
  Result := Scene.mAnimations[AnimationID].mChannels[Channel].mNumScalingKeys;
End;

Function AssimpFilter.GetPositionKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey;
Var
  Channel:Integer;
Begin
  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
    Exit;

  Result.Value.X := Scene.mAnimations[AnimationID].mChannels[Channel].mPositionKeys[Index].mValue.x;
  Result.Value.Y := Scene.mAnimations[AnimationID].mChannels[Channel].mPositionKeys[Index].mValue.Y;
  Result.Value.Z := Scene.mAnimations[AnimationID].mChannels[Channel].mPositionKeys[Index].mValue.Z;
  Result.Time := Scene.mAnimations[AnimationID].mChannels[Channel].mPositionKeys[Index].mTime;
End;

Function AssimpFilter.GetScaleKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey;
Var
  Channel:Integer;
Begin
  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
    Exit;

  Result.Value.X := Scene.mAnimations[AnimationID].mChannels[Channel].mScalingKeys[Index].mValue.x;
  Result.Value.Y := Scene.mAnimations[AnimationID].mChannels[Channel].mScalingKeys[Index].mValue.Y;
  Result.Value.Z := Scene.mAnimations[AnimationID].mChannels[Channel].mScalingKeys[Index].mValue.Z;
  Result.Time := Scene.mAnimations[AnimationID].mChannels[Channel].mScalingKeys[Index].mTime;
End;

Function AssimpFilter.GetRotationKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey;
Var
  Channel:Integer;
  Q:Quaternion;
Begin
  Channel := aiAnimationGetChannel(Scene.mAnimations[AnimationID], Bones[BoneID].Name);
  If (Channel<0) Then
    Exit;

  With Scene.mAnimations[AnimationID].mChannels[Channel].mRotationKeys[Index].mValue Do
  Q := QuaternionCreate(x, y, z, w);

  Result.Value := QuaternionToEuler(Q);
  Result.Time := Scene.mAnimations[AnimationID].mChannels[Channel].mRotationKeys[Index].mTime;
End;


Function AssimpFilter.FindNode(Name:AnsiString; Root:pAiNode): pAiNode;
Var
  I:Integer;
Begin
  If (aiStringGetValue(Root.mName) = Name) Then
  Begin
    Result := Root;
    Exit;
  End;

  For I:=0 To Pred(Root.mNumChildren) Do
  Begin
    Result := FindNode(Name, root.mChildren[I]);
    If Assigned(Result) Then
      Exit;
  End;

  Result := Nil;
End;

Function AssimpFilter.GetBoneParent(BoneID: Integer): Integer;
Begin
  If Assigned(Bones[BoneID].Parent) Then
    Result := Bones[BoneID].Parent.ID
  Else
    Result := -1;
End;

Function AssimpFilter.GetBoneIDByName(Name:AnsiString): Integer;
Var
  I, J, N:Integer;
Begin
  Result := -1;
  For I:=0 To Pred(BoneCount) Do
  If (Bones[I].Name = Name) Then
  Begin
    Result := I;
    Exit;
  End;
End;

Function AssimpFilter.GetBonePosition(BoneID: Integer): Vector3D;
Var
  A,B:Vector3D;
Begin
  A := Bones[BoneID].GlobalTransform.Transform(VectorZero);
  If Assigned(Bones[BoneID].Parent) Then
  Begin
    B := Bones[Bones[BoneID].Parent.ID].GlobalTransform.Transform(VectorZero);
    Result := VectorSubtract(A, B);
  End Else
    Result := A;
End;

Initialization
//  c:= aiGetPredefinedLogStream(aiDefaultLogStream_STDOUT, Nil);
//  aiAttachLogStream(@c);
Finalization
//  aiDetachAllLogStreams();
End.