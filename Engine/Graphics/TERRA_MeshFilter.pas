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
 * TERRA_MeshFilter
 * Implements the Mesh Filter, used to convert between different mesh formats
 ***********************************************************************************************************************
}
Unit TERRA_MeshFilter;

{$I terra.inc}

Interface

Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Vector4D,
  TERRA_Quaternion, TERRA_Color, TERRA_Stream, TERRA_VertexFormat;

Const
  meshGroupHidden       = 1 Shl 0;  // Group is hidden
  meshGroupDoubleSided  = 1 Shl 1;
  meshGroupCastShadow   = 1 Shl 2;
  //meshGroupTransparency = 1 Shl 3;
  //meshGroupAlphaTest    = 1 Shl 4;
  meshGroupPick         = 1 Shl 5;
  meshGroupLightmap     = 1 Shl 6;
  meshGroupSpheremap    = 1 Shl 7;
  meshGroupDepthOff     = 1 Shl 8;
  meshGroupTriplanar    = 1 Shl 9;
  meshGroupHueShift     = 1 Shl 10;
  meshGroupLightOff     = 1 Shl 11;
  meshGroupOverrideAmbient = 1 Shl 12;
  meshGroupAlphaMap     = 1 Shl 13;
  meshGroupWireframe    = 1 Shl 14;
  meshGroupColorOff     = 1 Shl 15;
  meshGroupOutlineOff   = 1 Shl 16;
  meshGroupForceOpaque  = 1 Shl 17;
  meshGroupLinked       = 1 Shl 18;
  meshGroupVegetation   = 1 Shl 19;
  meshGroupIgnoreColorTable  = 1 Shl 20;
  meshGroupStencilTest  = 1 Shl 21;
  meshGroupNormalsOff   = 1 Shl 22;
  meshGroupSkybox       = 1 Shl 23;
  meshGroupWaterMap     = 1 Shl 24;
  meshGroupDynamic      = 1 Shl 25;
  meshGroupReflective   = 1 Shl 26;
  meshGroupStencilMask  = 1 Shl 27;
  meshGroupReflectiveMap = 1 Shl 28;
  meshGroupShadowOnly  = 1 Shl 29;

{  mgMirror    = 2;  // Group is a reflective surface
  mgCullFace  = 4;  // Group is one sided/two sided
  mgShadow    = 8;  // Group cast shadow
  mgPick      = 128;  // Group is pickable
  mgCollision = 512;
  mgSphereMap = 1024;
  mgAlphaTest = 2048;
  mgTransparency = 4096;
  mgCloth     = 8192;
  mgOverrideMaterial = 16384;
}

Type
  MeshVectorKey = Record
    Value:Vector3D;
    Time:Single;
  End;

  {MeshQuaternionKey = Record
    Value:Quaternion;
    Time:Single;
  End;}

  MeshFilter = Class(TERRAObject)
    Public
      Function GetGroupCount:Integer; Virtual;
      Function GetGroupName(GroupID:Integer):TERRAString; Virtual;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Virtual;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Virtual;

      Function GetTriangleCount(GroupID:Integer):Integer; Virtual;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Virtual;

      Function GetVertexCount(GroupID:Integer):Integer; Virtual;
      Function GetVertexFormat(GroupID:Integer):VertexFormat; Virtual;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Virtual;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Virtual;
      Function GetVertexTangent(GroupID, Index:Integer):Vector4D; Virtual;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Virtual;
      Function GetVertexColor(GroupID, Index:Integer):Color; Virtual;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Virtual;
      Function GetVertexUV2(GroupID, Index:Integer):Vector2D; Virtual;

      Function GetDiffuseColor(GroupID:Integer):Color; Virtual;
      Function GetAmbientColor(GroupID:Integer):Color; Virtual;

      Function GetDiffuseMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetTriplanarMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetGlowMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetRefractionMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetSpecularMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetEmissiveMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetAlphaMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetLightMapName(GroupID:Integer):TERRAString; Virtual;
      Function GetColorRampName(GroupID:Integer):TERRAString; Virtual;

      Function GetBoneCount():Integer; Virtual;
      Function GetBoneName(BoneID:Integer):TERRAString; Virtual;
      Function GetBoneParent(BoneID:Integer):Integer; Virtual;
      Function GetBonePosition(BoneID:Integer):Vector3D; Virtual;
      Function GetBoneRotation(BoneID:Integer):Vector3D; Virtual;

      Function GetAnimationCount():Integer; Virtual;
      Function GetAnimationName(AnimationID:Integer):TERRAString; Virtual;
      Function GetAnimationFrameRate(AnimationID:Integer):Single; Virtual;
      Function GetAnimationDuration(AnimationID:Integer):Single; Virtual;
      Function GetAnimationLoop(AnimationID:Integer):Boolean; Virtual;

      Function GetPositionKeyCount(AnimationID, BoneID:Integer):Integer; Virtual;
      Function GetRotationKeyCount(AnimationID, BoneID:Integer):Integer; Virtual;
      Function GetScaleKeyCount(AnimationID, BoneID:Integer):Integer; Virtual;

      Function GetPositionKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Virtual;
      Function GetScaleKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Virtual;
      Function GetRotationKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Virtual;

      Function Load(Source:Stream):Boolean; Overload; Virtual;
      Function Load(FileName:TERRAString):Boolean; Overload;

      Class Function Save(Dest:Stream; MyMesh:MeshFilter):Boolean; Overload; Virtual;
      Class Function Save(FileName:TERRAString; MyMesh:MeshFilter):Boolean; Overload;
    End;

  MeshFilterClass = Class Of MeshFilter;
  MeshFilterType = Record
    Filter:MeshFilterClass;
    Extension:TERRAString;
  End;

  Procedure RegisterMeshFilter(Filter:MeshFilterClass; Extension:TERRAString);
  Function CreateMeshFilter(FileName:TERRAString):MeshFilter;

Var
  MeshFilterList:Array Of MeshFilterType;
  MeshFilterCount:Integer = 0;

Implementation
Uses TERRA_FileStream, TERRA_FileUtils, TERRA_MemoryStream, TERRA_GraphicsManager, TERRA_Renderer;

Procedure RegisterMeshFilter(Filter:MeshFilterClass; Extension:TERRAString);
Begin
  Inc(MeshFilterCount);
  SetLength(MeshFilterList, MeshFilterCount);
  MeshFilterList[Pred(MeshFilterCount)].Filter := Filter;
  MeshFilterList[Pred(MeshFilterCount)].Extension := Extension;
End;

Function CreateMeshFilter(FileName:TERRAString):MeshFilter;
Var
  Ext:TERRAString;
  Src:Stream;
  I:Integer;
Begin
  Result := Nil;
  If Not FileStream.Exists(FileName) Then
    Exit;

  Ext := GetFileExtension(FileName);
  For I:=0 To Pred(MeshFilterCount) Do
  If (StringEquals(MeshFilterList[I].Extension, Ext)) Then
  Begin
    Src := MemoryStream.Create(FileName);
    Result := MeshFilterList[I].Filter.Create;
    Result.Load(Src);
    ReleaseObject(Src);
    Exit;
  End;
End;

{ MeshFilter }
Function MeshFilter.GetDiffuseColor(GroupID: Integer): Color;
Begin
  Result := ColorWhite;
End;

Function MeshFilter.GetDiffuseMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetEmissiveMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetGroupCount: Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetGroupFlags(GroupID: Integer): Cardinal;
Begin
  Result := meshGroupCastShadow Or meshGroupPick;
End;

Function MeshFilter.GetGroupName(GroupID: Integer):TERRAString;
Begin
  Result := 'Group'+IntToString(Succ(GroupID));
End;

Function MeshFilter.GetSpecularMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
end;

Function MeshFilter.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result.Indices[0] := 0;
  Result.Indices[1] := 0;
  Result.Indices[2] := 0;
End;

Function MeshFilter.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetVertexBone(GroupID, Index: Integer): Integer;
Begin
  Result := -1;
End;

Function MeshFilter.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  Result := ColorWhite;
End;

Function MeshFilter.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetVertexFormat(GroupID: Integer):VertexFormat;
Begin
  Result := [vertexFormatPosition];
End;

Function MeshFilter.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := VectorUp;
End;

Function MeshFilter.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := VectorZero;
End;

Function MeshFilter.GetVertexTangent(GroupID, Index: Integer): Vector4D;
Begin
  Result := VectorCreate4D(0,1,0,1);
End;

Function MeshFilter.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result.X := 0;
  Result.Y := 0;
End;

Function MeshFilter.GetVertexUV2(GroupID, Index: Integer): Vector2D;
Begin
  Result.X := 0;
  Result.Y := 0;
End;

Class Function MeshFilter.Save(Dest:Stream; MyMesh: MeshFilter): Boolean;
Begin
  Result := False;
End;

Function MeshFilter.Load(Source: Stream): Boolean;
Begin
  Result := False;
End;

Function MeshFilter.Load(FileName:TERRAString): Boolean;
Var
  Src:Stream;
Begin
  Src := MemoryStream.Create(FileName);
  Result := Load(Src);
  ReleaseObject(Src);
End;

Class Function MeshFilter.Save(FileName:TERRAString; MyMesh: MeshFilter): Boolean;
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Result := Save(Dest, MyMesh);
  ReleaseObject(Dest);
End;

Function MeshFilter.GetGroupBlendMode(GroupID: Integer): Cardinal;
Begin
  Result := blendNone;
End;

Function MeshFilter.GetBoneCount: Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetBoneName(BoneID: Integer):TERRAString;
Begin
  Result := 'bone'+IntToString(BoneID);
End;

Function MeshFilter.GetPositionKeyCount(AnimationID, BoneID:Integer):Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetRotationKeyCount(AnimationID, BoneID:Integer):Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetScaleKeyCount(AnimationID, BoneID:Integer):Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetPositionKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey;
Begin
  Result.Value := VectorZero;
  Result.Time := 0;
End;

Function MeshFilter.GetScaleKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey;
Begin
  Result.Value := VectorOne;
  Result.Time := 0;
End;

Function MeshFilter.GetRotationKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey;
Begin
  Result.Value := VectorZero;
  Result.Time := 0;
End;

Function MeshFilter.GetAnimationCount: Integer;
Begin
  Result := 0;
End;

Function MeshFilter.GetAnimationDuration(AnimationID:Integer):Single;
Begin
  Result := 0;
End;

Function MeshFilter.GetAnimationName(AnimationID: Integer):TERRAString;
Begin
  Result := 'animation'+IntToString(AnimationID);
End;

Function MeshFilter.GetBoneParent(BoneID: Integer): Integer;
Begin
  Result := -1;
End;

Function MeshFilter.GetBonePosition(BoneID: Integer): Vector3D;
Begin
  Result := VectorZero;
End;

Function MeshFilter.GetBoneRotation(BoneID: Integer): Vector3D;
Begin
  Result := VectorZero;
End;

Function MeshFilter.GetAnimationFrameRate(AnimationID: Integer): Single;
Begin
  Result := 24.0;
End;

Function MeshFilter.GetAnimationLoop(AnimationID: Integer): Boolean;
Begin
  Result := False;
End;

Function MeshFilter.GetAlphaMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

function MeshFilter.GetAmbientColor(GroupID: Integer): Color;
Begin
  Result := ColorBlack;
End;

Function MeshFilter.GetColorRampName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetGlowMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetLightMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetRefractionMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

Function MeshFilter.GetTriplanarMapName(GroupID: Integer):TERRAString;
Begin
  Result := '';
End;

End.