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
 * TERRA_Skeleton
 * Implements the Skeleton class used for mesh skinning
 ***********************************************************************************************************************
}
Unit TERRA_MeshSkeleton;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Stream, TERRA_Resource, TERRA_Vector3D, TERRA_Math,
  TERRA_Matrix4x4, TERRA_Vector2D, TERRA_Color, TERRA_Quaternion, TERRA_ResourceManager;

Type
  MeshSkeleton = Class;

  MeshBoneKind = (
    meshBone_Default = 0,
    meshBone_Root = 1,
    meshBone_Dummy = 2
  );

  MeshBone = Class(TERRAObject)
    Protected
      _ID:Integer;
      _SkinningIndex:Byte;
      _Parent:MeshBone;
      _Owner:MeshSkeleton;

      _RelativeMatrix:Matrix4x4;
      _OffsetMatrix:Matrix4x4;

      _Kind:MeshBoneKind;

      Function GetLength():Single;

      Function GetAbsolutePosition():Vector3D;

      Function GetAbsoluteMatrix: Matrix4x4;

      Function GetNormal: Vector3D;

      Procedure SetRelativeMatrix(const Value: Matrix4x4);

    Public
      Constructor Create(ID:Integer; Parent:MeshBone);
      Procedure Release; Override;

      Function Read(Source:Stream):TERRAString;
      Procedure Write(Dest:Stream);

      //Procedure SetLength(Const Value:Single);

      Function GetRoot():MeshBone;

      Property ID:Integer Read _ID;
      Property SkinningIndex:Byte Read _SkinningIndex;
      Property Parent:MeshBone Read _Parent;
      Property Owner:MeshSkeleton Read _Owner;

      Property Length:Single Read GetLength; // Write SetLength;

      Property Normal:Vector3D Read GetNormal;

      Property AbsoluteMatrix:Matrix4x4 Read GetAbsoluteMatrix;
      Property RelativeMatrix:Matrix4x4 Read _RelativeMatrix Write SetRelativeMatrix;
      Property OffsetMatrix:Matrix4x4 Read _OffsetMatrix;

      Property AbsolutePosition:Vector3D Read GetAbsolutePosition;

      Property Kind:MeshBoneKind Read _Kind;
  End;

  MeshSkeleton = Class(TERRAObject)
    Protected
      _BoneList:Array Of MeshBone;
      _BoneCount:Integer;

      _Hash:Cardinal;

    Public
      Name:String;
//      BindPose:Array Of Matrix4x4;

      Procedure Release; Override;

      Procedure NormalizeJoints();

      Procedure Clone(Other:MeshSkeleton);

      Procedure Read(Source:Stream);
      Procedure Write(Dest:Stream);

      Function AddBone(Parent:MeshBone = Nil):MeshBone;

      Function GetBoneByIndex(Index:Integer):MeshBone;
      Function GetBoneByName(Const Name:TERRAString):MeshBone;

      Function GetBoneLength(Index:Integer):Single;

      Property BoneCount:Integer Read _BoneCount;

      Property Hash:Cardinal Read _Hash;
  End;


Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_GraphicsManager, TERRA_FileStream, TERRA_FileUtils;

{ MeshBone }

Constructor MeshBone.Create(ID: Integer; Parent: MeshBone);
Begin
  _Parent := Parent;
  _ID := ID;
End;

Procedure MeshBone.Release;
Begin
  // do nothing
End;

(*Procedure MeshBone.SetLength(const Value: Single);
Var
  CurrentPos, ParentPos, R:Vector3D;
Begin
  If (Self.Parent=Nil) Then
    Exit;

  CurrentPos := Self.GetAbsolutePosition();
  ParentPos := Parent.GetAbsolutePosition();
  R := VectorSubtract(CurrentPos, ParentPos);
  R.Normalize();
  R.Scale(Value);

  //VectorSubtract(CurrentPos[Bone.Index], CurrentPos[Bone.Parent.Index])

  Self._Translation := R;
  Self._Orientation := QuaternionZero;
End;*)


Function MeshBone.GetLength: Single;
Var
  P:Vector3D;
Begin
  If (Self.Parent=Nil) Then
    Result := 0
  Else
  Begin
    P := VectorSubtract(Self.GetAbsolutePosition(), Parent.GetAbsolutePosition());
    Result := P.Length();
  End;
End;

Function MeshBone.GetNormal: Vector3D;
Begin
  If (Self.Parent=Nil) Then
    Result := VectorUp
  Else
  Begin
    Result := VectorSubtract(Self.GetAbsolutePosition(), Parent.GetAbsolutePosition());
    Result.Normalize();
  End;
End;

Function MeshBone.Read(Source:Stream):TERRAString;
Var
  N:Byte;
  ParentName:TERRAString;
Begin
  Source.ReadString(_ObjectName);
  Source.ReadString(ParentName);
  _Parent := Nil;

  Source.ReadByte(N);
  _Kind := MeshBoneKind(N);

  Source.Read(@_RelativeMatrix, SizeOf(_RelativeMatrix));

  If (_Kind = meshBone_Dummy) Then
  Begin
    _OffsetMatrix := Matrix4x4Identity;
    _SkinningIndex := 0;
  End Else
  Begin
    Source.ReadByte(_SkinningIndex);
    Source.Read(@_OffsetMatrix, SizeOf(_OffsetMatrix));
  End;

  Result := ParentName;
End;

Procedure MeshBone.Write(Dest: Stream);
Begin
  RaiseError('Not implemented');
End;

Function MeshBone.GetAbsolutePosition: Vector3D;
Begin
  Result := Self.AbsoluteMatrix.Transform(VectorZero);
End;


Function MeshBone.GetAbsoluteMatrix: Matrix4x4;
Begin
  Result := _RelativeMatrix;

  If Assigned(Parent) Then
    Result := Matrix4x4Multiply4x3(_Parent.AbsoluteMatrix, Result);
End;


Procedure MeshBone.SetRelativeMatrix(const Value: Matrix4x4);
Begin
  Self._RelativeMatrix := Value;
End;

Function MeshBone.GetRoot: MeshBone;
Begin
  If Parent = Nil Then
    Result := Self
  Else
    Result := Parent.GetRoot;
End;

{ MeshSkeleton }
Function MeshSkeleton.AddBone(Parent:MeshBone):MeshBone;
Begin
  Result := MeshBone.Create(_BoneCount, Parent);
  
  Inc(_BoneCount);
  SetLength(_BoneList, _BoneCount);
  _BoneList[ Pred(_BoneCount)] := Result;
End;

Function MeshSkeleton.GetBoneByIndex(Index:Integer):MeshBone;
Begin
  If (Index<0) Or (Index>=_BoneCount) Then
    Result := Nil
  Else
    Result := (_BoneList[Index]);
End;

Function MeshSkeleton.GetBoneByName(Const Name:TERRAString): MeshBone;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneCount) Do
  If (StringEquals(Name, _BoneList[I].Name)) Then
  Begin
    Result := _BoneList[I];
    Exit;
  End;

  Result := Nil;
End;

Procedure MeshSkeleton.Read(Source: Stream);
Var
  Parents:Array Of TERRAString;
  I:Integer;
Begin
  Source.ReadInteger(_BoneCount);
  SetLength(_BoneList, _BoneCount);
  SetLength(Parents, _BoneCount);
  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I] := Nil;

  For I:=0 To Pred(_BoneCount) Do
  Begin
    _BoneList[I] := MeshBone.Create(I, Nil);
    _BoneList[I]._ID := I;
    _BoneList[I]._Owner := Self;
    Parents[I] := _BoneList[I].Read(Source);
  End;

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I]._Parent := Self.GetBoneByName(Parents[I]);
End;

Procedure MeshSkeleton.Write(Dest: Stream);
Begin
  RaiseError('Not implemented');
End;

Procedure MeshSkeleton.Release;
Var
  I:Integer;
Begin
  _BoneCount := Length(_BoneList);

  For I:=0 To Pred(_BoneCount) Do
    ReleaseObject(_BoneList[I]);

  SetLength(_BoneList, 0);
End;

Function MeshSkeleton.GetBoneLength(Index: Integer): Single;
Var
  A, B:Vector3D;
Begin
  If (Index<0) Or (_BoneList[Index].Parent = Nil) Then
  Begin
    Result := 0;
    Exit;
  End;

  A := _BoneList[Index].AbsoluteMatrix.Transform(VectorZero);
  B := _BoneList[_BoneList[Index].Parent.ID].AbsoluteMatrix.Transform(VectorZero);
  Result := A.Distance(B);
End;

Procedure MeshSkeleton.Clone(Other: MeshSkeleton);
Var
  I:Integer;
  Bone:MeshBone;
Begin
  If (Other = Nil) Then
    Exit;

  Self.Name := Other.Name;

  Self._Hash := Application.GetTime();

  For I:=0 To Pred(_BoneCount) Do
    ReleaseObject(_BoneList[I]);

  Self._BoneCount := Other._BoneCount;
  SetLength(Self._BoneList, _BoneCount);

  For I:=0 To Pred(_BoneCount) Do
  Begin
    Bone := Other.GetBoneByIndex(I);
    _BoneList[I] := MeshBone.Create(I, Nil);
    _BoneList[I].Name := Bone.Name;
    _BoneList[I]._ID := I;
    _BoneList[I]._Owner := Self;

    _BoneList[I]._RelativeMatrix := Bone.RelativeMatrix;

    If Assigned(Bone.Parent) Then
      _BoneList[I]._Parent := Self.GetBoneByName(Bone.Parent.Name)
    Else
      _BoneList[I]._Parent := Nil;
  End;
End;



Procedure MeshSkeleton.NormalizeJoints;
Var
  I:Integer;
  Bone:MeshBone;
  CurrentPos:Array Of Vector3D;
  Angles:Vector3D;
  A, B, C, M:Matrix4x4;
Begin
  SetLength(CurrentPos, Self.BoneCount);
  For I:=0 To Pred(Self.BoneCount) Do
  Begin
    Bone := Self.GetBoneByIndex(I);
    CurrentPos[Bone.ID] := Bone.AbsolutePosition;
  End;

  For I:=0 To Pred(Self.BoneCount) Do
  Begin
    Bone := Self.GetBoneByIndex(I);

    If Assigned(Bone.Parent) Then
      B := Matrix4x4Translation(VectorSubtract(CurrentPos[Bone.ID], CurrentPos[Bone.Parent.ID]))
    Else
      B := Matrix4x4Translation(Bone.AbsolutePosition);

    Bone._RelativeMatrix := B;
  End;

  CurrentPos := Nil;
End;


End.
