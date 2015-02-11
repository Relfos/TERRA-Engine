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
 * TERRA_SMD
 * Implements SMD (from Valve) model loader/mesh filter
 ***********************************************************************************************************************
}
Unit TERRA_SMD;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Vector3D, TERRA_Matrix4x4, TERRA_Math, TERRA_Color,
  TERRA_Vector4D, TERRA_Vector2D, TERRA_MeshFilter, TERRA_FileStream, TERRA_FileUtils,
  TERRA_OS, TERRA_MemoryStream;

Type
  SMDValue = Record
    Position:Vector3D;
    Rotation:Vector3D;
  End;

  SMDFrame = Record
    Time:Single;
    Values:Array Of SMDValue;
  End;

  SMDVertex = Record
    Position:Vector3D;
    Normal:Vector3D;
    UV:Vector2D;
    BoneIndex:Integer;
  End;

  PSMDBone = ^SMDBone;
  SMDBone = Record
    Name:AnsiString;
    Index:Integer;
    Parent:PSMDBone;
    Ready:Boolean;

    Position:Vector3D;

    RelativeMatrix:Matrix4x4;
    AbsoluteMatrix:Matrix4x4;
  End;

  SMDAnimation = Object
    Protected
      _Bones:Array Of SMDBone;
      _BoneCount:Integer;
      _Frames:Array Of SMDFrame;
      _FrameCount:Integer;
      _Length:Single;

      _Time:Single;
      _CurrentFrame:Integer;
      _CurrentDelta :Single;

      Procedure UpdateBone(Var Bone:SMDBone; FrameIndex:Integer);

    Public
      Procedure Load(Source:Stream); Overload;
      Procedure Load(FileName:AnsiString); Overload;

      Procedure Update(Time: Single);

      Procedure Render(Transform:Matrix4x4);

      Function GetBone(Index:Integer):PSMDBone;
      Function GetFramePosition(BoneIndex:Integer):Vector3D;
      Function GetFrameRotation(BoneIndex:Integer):Vector4D;

      Function GetFrame(Index:Integer):SMDFrame;

      Function GetBoneLength(Index:Integer):Single;

      Property BoneCount:Integer Read _BoneCount;
      Property Duration:Single Read _Length;
      Property FrameCount:Integer Read _FrameCount;
  End;

  SMDBoneReference = Record
    Index:Integer;
    Name:AnsiString;
    Parent:Integer;
    Position:Vector3D;
    Rotation:Vector3D;
  End;

  SMDReference = Object
    Vertices:Array Of SMDVertex;
    VertexCount:Integer;
    Triangles:Array Of Triangle;
    TriangleCount:Integer;
    Texture:AnsiString;
    _Bones:Array Of SMDBoneReference;
    _BoneCount:Integer;

    Procedure Load(Source:Stream); Overload;
    Procedure Load(FileName:AnsiString); Overload;
  End;

  SMDModel = Class(MeshFilter)
    Protected
      _Reference:SMDReference;

    Public
      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexHandness(GroupID, Index:Integer):Single; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;
      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;

      Function GetBoneCount():Integer; Override;
      Function GetBoneName(BoneID:Integer):AnsiString; Override;
      Function GetBoneParent(BoneID:Integer):Integer; Override;
      Function GetBonePosition(BoneID:Integer):Vector3D; Override;
      Function GetBoneRotation(BoneID:Integer):Vector3D; Override;

      Function GetAnimationCount():Integer; Override;
      Function GetAnimationName(Index:Integer):AnsiString; Override;
      Function GetAnimationDuration(Index:Integer):Single; Override;

      Function GetPositionKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetRotationKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetScaleKeyCount(AnimationID, BoneID:Integer):Integer; Override;

      Function GetPositionKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey; Override;
      Function GetRotationKey(AnimationID, BoneID:Integer; Index:Integer):MeshVectorKey; Override;

      Function Load(Source:Stream):Boolean; Overload; Override;
      Function Load(FileName:AnsiString):Boolean; Overload;
  End;

Implementation
Uses TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

{ SMDAnimation }

Procedure SMDAnimation.Load(Source: Stream);
Var
  I, J, N:Integer;
  S, S2:AnsiString;
  V:SMDValue;
  Parents:Array Of Integer;
Begin
  _Length := 0;

  Source.ReadLine(S); // version
  Source.ReadLine(S); // nodes
  Repeat
    Source.ReadLine(S);
    If (StringUpper(S)='END') Then
      Break;

    Inc(Self._BoneCount);
    SetLength(_Bones, _BoneCount);
    S2 := StringGetNextSplit(S, Ord(' '));
    S := Copy(S,2, MaxInt);
    S2 := StringGetNextSplit(S, Ord('"'));
    _Bones[Pred(_BoneCount)].Name := S2;
    _Bones[Pred(_BoneCount)].Index := Pred(_BoneCount);
    SetLEngth(Parents, _BoneCount);
    Parents[Pred(_BoneCount)] := StringToInt(S);
  Until (False);

  For I:=0 To Pred(_BoneCount) Do
  If (Parents[I]=-1) Then
    _Bones[I].Parent := Nil
  Else
    _Bones[I].Parent := @(_Bones[Parents[I]]);

  Source.ReadLine(S); // skeleton
  Repeat
    Source.ReadLine(S);
    If (StringUpper(S)='END') Then
      Break;

    StringGetNextSplit(S, Ord(' '));
    N := _FrameCount;
    Inc(_FrameCount);
    SetLength(_Frames, _FrameCount);
    _Frames[N].Time := StringToFloat(S);
    If (_Frames[N].Time>_Length) Then
      _Length := _Frames[N].Time;
    SetLength(_Frames[N].Values, _BoneCount);

    For I:=0 To Pred(_BoneCount) Do
    Begin
      Source.ReadLine(S);
      StringGetNextSplit(S, Ord(' ')); // skip index

      V.Position.X := StringToFloat(StringGetNextSplit(S, Ord(' ')));
      V.Position.Z := StringToFloat(StringGetNextSplit(S, Ord(' ')));
      V.Position.Y := StringToFloat(StringGetNextSplit(S, Ord(' ')));

      V.Rotation.X := StringToFloat(StringGetNextSplit(S, Ord(' ')));
      V.Rotation.Z := StringToFloat(StringGetNextSplit(S, Ord(' ')));
      V.Rotation.Y := StringToFloat(StringGetNextSplit(S, Ord(' ')));
      _Frames[N].Values[I] := V;
    End;
  Until (False);
End;

Procedure SMDAnimation.Load(FileName:AnsiString);
Var
  Source:Stream;
Begin
  Source := MemoryStream.Create(FileName);
  Load(Source);
  Source.Destroy;
End;

Procedure SMDAnimation.UpdateBone(Var Bone:SMDBone; FrameIndex:Integer);
Var
  T,R:Vector3D;
Begin
  If (Bone.Ready) Then
    Exit;

  If (Assigned(Bone.Parent)) And (Not Bone.Parent.Ready) Then
    UpdateBone(Bone.Parent^, FrameIndex);

  T := _Frames[FrameIndex].Values[Bone.Index].Position;
  R := _Frames[FrameIndex].Values[Bone.Index].Rotation;
  Bone.RelativeMatrix := Matrix4x4Multiply4x3(Matrix4x4Translation(T), Matrix4x4Rotation(R));

	// Each bone's final matrix is its relative matrix concatenated onto its
	// parent's final matrix (which in turn is ....)
	//
	If ( Bone.Parent = nil ) Then					// this is the root node
  Begin
    Bone.AbsoluteMatrix := Bone.RelativeMatrix;
  End Else									// not the root node
	Begin
		// m_final := parent's m_final * m_rel (matrix concatenation)
    Bone.AbsoluteMatrix := Matrix4x4Multiply4x3(Bone.Parent.AbsoluteMatrix, Bone.RelativeMatrix);
	End;

  Bone.Ready := True;
End;

Function SMDAnimation.GetFrame(Index:Integer):SMDFrame;
Begin
  Result := _Frames[Index];
End;

Function SMDAnimation.GetFramePosition(BoneIndex:Integer):Vector3D;
Var
  A, B:Integer;
Begin
  A := _CurrentFrame;
  B := (A + 1) Mod _FrameCount;
  Result := VectorInterpolate(_Frames[A].Values[BoneIndex].Position, _Frames[B].Values[BoneIndex].Position, _CurrentDelta);
End;

Function SMDAnimation.GetFrameRotation(BoneIndex:Integer):Vector4D;
Var
  A, B:Integer;
  Q1, Q2:Vector4D;
Begin
  A := _CurrentFrame;
  B := (A + 1) Mod _FrameCount;

  Q1 := Vector4DRotation(_Frames[A].Values[BoneIndex].Rotation);
  Q2 := Vector4DRotation(_Frames[B].Values[BoneIndex].Rotation);

  Result := Vector4DSlerp(Q1, Q2, _CurrentDelta);
//  Result := Vector4DConjugate(Result);
End;

Procedure SMDAnimation.Update(Time: Single);
Var
  J, I, N:Integer;
  Delta, K:Single;
  Q1, Q2:Vector4D;
  PA,PB:Array Of Vector3D;
Begin
  _Time := Time;

  J := 0;
  For I:=1 To Pred(_FrameCount) Do
  If (_Frames[I].Time>Time) Then
    Break
  Else
    J := I;

  N := Succ(J);
  If (N>=_FrameCount) Then
  Begin
    N := 0;
    Delta := 0.0;
  End Else
  Begin
    Delta := _Frames[N].Time - _Frames[J].Time;
    Delta := (Time - _Frames[J].Time) / Delta;
  End;

  _CurrentFrame := N;
  _CurrentDelta := Delta;

  For I:=0 To Pred(_BoneCount) Do
    _Bones[I].Ready := False;

  For I:=0 To Pred(_BoneCount) Do
    UpdateBone(_Bones[I], J);

  SetLength(PA, _BoneCount);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    PA[I] := _Bones[I].AbsoluteMatrix.Transform(VectorZero);
{    K := PA[I].Y;
    PA[I].Y := PA[I].Z;
    PA[I].Z := -K;}
  End;

  For I:=0 To Pred(_BoneCount) Do
    _Bones[I].Ready := False;

  For I:=0 To Pred(_BoneCount) Do
    UpdateBone(_Bones[I], N);

  SetLength(PB, _BoneCount);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    PB[I] := _Bones[I].AbsoluteMatrix.Transform(VectorZero);
{    K := PB[I].Y;
    PB[I].Y := PB[I].Z;
    PB[I].Z := -K;}
  End;

  For I:=0 To Pred(_BoneCount) Do
  Begin
    _Bones[I].Position := VectorInterpolate(PA[I], PB[I], Delta);
  End;
End;

Procedure SMDAnimation.Render(Transform: Matrix4x4);
Var
  A, B:Vector3D;
  I:Integer;
Begin
{$IFDEF PC}
  GraphicsManager.Instance.EnableColorShader(ColorBlue, Transform);

  glLineWidth(2);                         
  GraphicsManager.Instance.SetBlendMode(blendNone);

  glDepthMask(True);                      
  glDepthRange(0,0.0001);                 

  glBegin(GL_LINES);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    A := 	_Bones[I].Position;
    If (Assigned(_Bones[I].Parent)) Then
      B := _Bones[_Bones[I].Parent.Index].Position
    Else
      Continue;

    With A Do
      glVertex3f(X,Y,Z);
    With B Do
      glVertex3f(X,Y,Z);
  End;
  glEnd;

  glPointSize(10);
  glBegin(GL_POINTS);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    With _Bones[I].Position Do
      glVertex3f(X,Y,Z);
  End;
  glEnd;

  glDepthMask(True);                      
  glDepthRange(0,1);                      
{$ENDIF}
End;

Function SMDAnimation.GetBone(Index: Integer): PSMDBone;
Begin
  If (Index<0) Or (Index>=_BoneCount) Then
    Result := Nil
  Else
    Result := @(_Bones[Index]);
End;

Function SMDAnimation.GetBoneLength(Index: Integer): Single;
Var
  A,B:Vector3D;
Begin
  Result := 0;
  If (_Bones[Index].Parent = Nil) Then
    Exit;

  A := _Bones[Index].AbsoluteMatrix.Transform(VectorZero);
  B := _Bones[Index].Parent.AbsoluteMatrix.Transform(VectorZero);
  Result := A.Distance(B);
End;

{ SMDReference }
Procedure SMDReference.Load(Source: Stream);
Var
  I,K:Integer;
  S, S2:AnsiString;
Begin
  Source.ReadLine(S); // version
  Source.ReadLine(S); // nodes
  Repeat
    Source.ReadLine(S);
    If (StringUpper(S)='END') Then
      Break;

    Inc(Self._BoneCount);
    SetLength(_Bones, _BoneCount);
    S2 := StringGetNextSplit(S, Ord(' '));
    S := Copy(S,2, MaxInt);
    S2 := StringGetNextSplit(S, Ord('"'));
    _Bones[Pred(_BoneCount)].Name := S2;
    _Bones[Pred(_BoneCount)].Index := Pred(_BoneCount);
    _Bones[Pred(_BoneCount)].Parent := StringToInt(S);
  Until (False);

  Source.ReadLine(S); // skeleton
  Source.ReadLine(S); // time
  For I:=0 To Pred(_BoneCount) Do
  Begin
    Source.ReadLine(S);
    StringGetNextSplit(S, Ord(' ')); // skip index

    _Bones[I].Position.X := StringToFloat(StringGetNextSplit(S, Ord(' ')));
    _Bones[I].Position.Y := StringToFloat(StringGetNextSplit(S, Ord(' ')));
    _Bones[I].Position.Z := StringToFloat(StringGetNextSplit(S, Ord(' ')));

    _Bones[I].Rotation.X := StringToFloat(StringGetNextSplit(S, Ord(' ')));
    _Bones[I].Rotation.Y := StringToFloat(StringGetNextSplit(S, Ord(' ')));
    _Bones[I].Rotation.Z := StringToFloat(StringGetNextSplit(S, Ord(' ')));
  End;

  Source.ReadLine(S); //end
  Source.ReadLine(S); //triangles

  Texture := '';
  While Not Source.EOF Do
  Begin
    Source.ReadLine(S); //texture, skip
    If (Texture = '') Then
      Texture := S;

    K := VertexCount;
    For I:=1 To 3 Do
    Begin
      Source.ReadLine(S);
      Inc(VertexCount);
      SetLength(Vertices, VertexCount);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].BoneIndex := StringToInt(S2);

      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Position.X := StringToFloat(S2);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Position.Y := StringToFloat(S2);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Position.Z := StringToFloat(S2);

      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Normal.X := StringToFloat(S2);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Normal.Y := StringToFloat(S2);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].Normal.Z := StringToFloat(S2);

      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].UV.X := StringToFloat(S2);
      S2 := StringGetNextSplit(S, Ord(' '));
      Vertices[Pred(VertexCount)].UV.Y := StringToFloat(S2);
    End;

    Inc(TriangleCount);
    SetLength(Triangles, TriangleCount);
    Triangles[Pred(TriangleCount)].Indices[0] := K + 0;
    Triangles[Pred(TriangleCount)].Indices[1] := K + 1;
    Triangles[Pred(TriangleCount)].Indices[2] := K + 2;
  End;
End;

Procedure SMDReference.Load(FileName:AnsiString);
Var
  S:Stream;
Begin
  S := FileStream.Open(FileName);
  Self.Load(S);
  S.Destroy;
End;

{ SMDModel }
Function SMDModel.GetAnimationCount: Integer;
Begin
  Result := 1;
End;

Function SMDModel.GetAnimationDuration(Index: Integer): Single;
Begin
  Result := 0;
End;

Function SMDModel.GetAnimationName(Index: Integer):AnsiString;
Begin
  Result := 'animation'+IntToString(index);
End;

Function SMDModel.GetBoneCount: Integer;
Begin
  Result := _Reference._BoneCount;
End;

Function SMDModel.GetBoneName(BoneID: Integer):AnsiString;
Begin
  Result := _Reference._Bones[BoneID].Name;
End;

Function SMDModel.GetBoneParent(BoneID: Integer): Integer;
Begin
  Result := _Reference._Bones[BoneID].Parent;
End;

Function SMDModel.GetBonePosition(BoneID: Integer): Vector3D;
Begin
  Result := _Reference._Bones[BoneID].Position;
  {Result := _Animation._Bones[BoneID].Position;
  If (_Animation._Bones[BoneID].Parent<>Nil) Then
    Result := VectorSubtract(Result, _Animation._Bones[BoneID].Parent.Position);}
End;

Function SMDModel.GetBoneRotation(BoneID: Integer): Vector3D;
Begin
  Result := _Reference._Bones[BoneID].Rotation;
  //Result := VectorZero;
End;

Function SMDModel.GetPositionKey(AnimationID, BoneID, Index: Integer): MeshVectorKey;
Var
  K:Single;
Begin
	Result.Time := 0.0;
	Result.Value := VectorZero;
  {Result.Value := _Animation._Frames[Index].Values[BoneID].Position;
  Result.Time := _Animation._Frames[Index].Time;}
  //Result.Value.Subtract(Self.GetBonePosition(BoneID));
End;

Function SMDModel.GetPositionKeyCount(AnimationID,BoneID: Integer): Integer;
Begin
  Result := 0;
End;

Function SMDModel.GetRotationKey(AnimationID, BoneID,Index: Integer): MeshVectorKey;
Begin
  {Result.Value := _Animation._Frames[Index].Values[BoneID].Rotation;
  Result.Time := _Animation._Frames[Index].Time;}
	Result.Time := 0.0;
	Result.Value := VectorZero;
  Result.Value.Subtract(Self.GetBoneRotation(BoneID));
End;

Function SMDModel.GetRotationKeyCount(AnimationID,BoneID: Integer): Integer;
Begin
  Result := 0;//_Animation._FrameCount;
End;

Function SMDModel.GetScaleKeyCount(AnimationID, BoneID: Integer): Integer;
Begin
  Result := 0;
End;

Function SMDModel.GetDiffuseColor(GroupID: Integer): Color;
Begin
  Result := ColorWhite;
end;

Function SMDModel.GetDiffuseMapName(GroupID: Integer):AnsiString;
Begin
  Result := _Reference.Texture;
End;

Function SMDModel.GetEmissiveMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function SMDModel.GetGroupBlendMode(GroupID: Integer): Cardinal;
Begin
  Result := blendBlend;
End;

Function SMDModel.GetGroupCount: Integer;
Begin
  Result := 1;
End;

Function SMDModel.GetGroupName(GroupID: Integer):AnsiString;
Begin
  Result := 'smdgroup'+IntToString(GroupID);
End;

Function SMDModel.GetSpecularMapName(GroupID: Integer):AnsiString;
Begin
  Result := '';
End;

Function SMDModel.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Reference.Triangles[Index];
End;

Function SMDModel.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _Reference.TriangleCount;
End;

Function SMDModel.GetVertexBone(GroupID, Index: Integer): Integer;
Begin
  Result := _Reference.Vertices[Index].BoneIndex;
End;

Function SMDModel.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  Result := ColorWhite;
End;

Function SMDModel.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Reference.VertexCount;
End;

Function SMDModel.GetVertexFormat(GroupID: Integer): Cardinal;
Begin
  Result := meshFormatNormal Or meshFormatBone;
End;

Function SMDModel.GetVertexHandness(GroupID, Index: Integer): Single;
Begin
  Result := 1;
End;

Function SMDModel.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Reference.Vertices[Index].Normal;
End;

Function SMDModel.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Reference.Vertices[Index].Position;
End;

Function SMDModel.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Reference.Vertices[Index].UV;
End;

Function SMDModel.Load(FileName:AnsiString): Boolean;
Begin
  Self._Reference.Load(FileName);
  Result := True;
End;

Function SMDModel.Load(Source: Stream): Boolean;
Var
  FileName, Path, S:AnsiString;
Begin
  Self._Reference.Load(Source);
  Result := True;
End;

Initialization
  RegisterMeshFilter(SMDModel, 'SMD');
End.