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

  MeshBone = Class(TERRAObject)
    Name:TERRAString;
    Index:Integer;
    Parent:MeshBone;
    Owner:MeshSkeleton;

    Color:Color;
    Selected:Boolean;

    StartPosition:Vector3D;
    {$IFNDEF NO_ROTS}
    StartRotation:Vector3D;

    AbsoluteRotation:Quaternion;
    RelativeRotation:Quaternion;
    {$ENDIF}

    AbsoluteMatrix:Matrix4x4;
    RelativeMatrix:Matrix4x4;

    Ready:Boolean;

    Procedure Init;

    Procedure Release; Override;

    Function Read(Source:Stream):TERRAString;
    Procedure Write(Dest:Stream);

    Function GetLength():Single;
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

      Procedure Init();

      Procedure Clone(Other:MeshSkeleton);

      Procedure Read(Source:Stream);
      Procedure Write(Dest:Stream);

      Function AddBone:MeshBone;
      Function GetBone(Index:Integer):MeshBone; Overload;
      Function GetBone(Const Name:TERRAString):MeshBone; Overload;

      Function GetBoneLength(Index:Integer):Single;

      Procedure Render(Const Transform:Matrix4x4; Instance:Pointer);

      Property BoneCount:Integer Read _BoneCount;

      Property Hash:Cardinal Read _Hash;
  End;


Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_GraphicsManager, TERRA_FileStream, TERRA_FileUtils;

{ MeshBone }
Procedure MeshBone.Release;
Begin
  // do nothing
End;

Function MeshBone.GetLength: Single;
Var
  P:Vector3D;
Begin
  If (Self.Parent=Nil) Then
    Result := 0
  Else
  Begin
    P := VectorSubtract(Self.StartPosition, Parent.StartPosition);
    Result := P.Length();
  End;
End;

Procedure MeshBone.Init;
Begin
  If (Ready) Then
    Exit;

  If (Assigned(Parent)) And (Not Parent.Ready) Then
    Parent.Init;

{$IFNDEF NO_ROTS}
  RelativeMatrix := Matrix4x4Multiply4x3(Matrix4x4Translation(startPosition), Matrix4x4Rotation(startRotation));
  RelativeRotation := QuaternionRotation(StartRotation);
{$ELSE}
  RelativeMatrix := Matrix4x4Translation(startPosition);
{$ENDIF}

	// Each bone's final matrix is its relative matrix concatenated onto its
	// parent's final matrix (which in turn is ....)
	//
	If ( Parent = nil ) Then					// this is the root node
  Begin
    AbsoluteMatrix := RelativeMatrix;
    {$IFNDEF NO_ROTS}
    AbsoluteRotation := QuaternionZero;
    {$ENDIF}
  End Else									// not the root node
	Begin
		// m_final := parent's m_final * m_rel (matrix concatenation)
    AbsoluteMatrix := Matrix4x4Multiply4x3(Parent.AbsoluteMatrix, RelativeMatrix);
    {$IFNDEF NO_ROTS}
    AbsoluteRotation := QuaternionMultiply(Parent.AbsoluteRotation, RelativeRotation);
    {$ENDIF}
	End;

  Ready := True;
End;

Function MeshBone.Read(Source:Stream):TERRAString;
Var
  I:Integer;
Begin
  Source.ReadString(Name);
  Source.ReadString(Result);
  Parent := Nil;

  Source.Read(@StartPosition, SizeOf(Vector3D));
  {$IFNDEF NO_ROTS}
  Source.Read(@StartRotation, SizeOf(Vector3D));
  {$ELSE}
  Source.Skip(SizeOf(Vector3D));
  {$ENDIF}

  Ready := False;
End;

Procedure MeshBone.Write(Dest:Stream);
Begin
  Dest.WriteString(Name);
  If (Assigned(Parent)) Then
    Dest.WriteString(Parent.Name)
  Else
    Dest.WriteString('');
  Dest.Write(@StartPosition, SizeOf(StartPosition));
  {$IFNDEF NO_ROTS}
  Dest.Write(@StartRotation, SizeOf(StartRotation));
  {$ELSE}
  Dest.Write(@StartPosition, SizeOf(StartPosition));
  {$ENDIF}
End;

{ MeshSkeleton }
Function MeshSkeleton.AddBone:MeshBone;
Begin
  Inc(_BoneCount);
  SetLength(_BoneList, _BoneCount);
  Result := MeshBone.Create;
  _BoneList[ Pred(_BoneCount)] := Result;
  Result.Color := ColorWhite;
  Result.Selected := False;
End;

Function MeshSkeleton.GetBone(Index:Integer):MeshBone;
Begin
  If (Index<0) Or (Index>=_BoneCount) Then
    Result := Nil
  Else
    Result := (_BoneList[Index]);
End;

Procedure MeshSkeleton.Render(Const Transform:Matrix4x4; Instance:Pointer);
Var
  I:Integer;
  A, B:Vector3D;
Begin
{$IFDEF PCs}
  GraphicsManager.Instance.BeginColorShader(ColorWhite, Transform);

  glLineWidth(2);                         

  GraphicsManager.Instance.SetFog(False);
  GraphicsManager.Instance.SetBlendMode(blendNone);

  glDepthMask(True);                      
  glDepthRange(0,0.0001);                 

  glBegin(GL_LINES);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    If Assigned(Instance) Then
    Begin
      A := 	MeshInstance(MeshInstance(Instance)).BoneMatrixList[Succ(I)].Transform(VectorZero);
  	  If (Assigned(_BoneList[I].Parent)) Then
        B := MeshInstance(MeshInstance(Instance)).BoneMatrixList[Succ(_BoneList[I].Parent.Index)].Transform(VectorZero)
      Else
        Continue;
    End Else
    Begin
      A := 	_BoneList[I].AbsoluteMatrix.Transform(VectorZero);
  	  If (Assigned(_BoneList[I].Parent)) Then
        B := _BoneList[_BoneList[I].Parent.Index].AbsoluteMatrix.Transform(VectorZero)
      Else
        Continue;
    End;

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
    If Assigned(Instance) Then
      A := 	MeshInstance(MeshInstance(Instance)).BoneMatrixList[Succ(I)].Transform(VectorZero)
    Else
      A := 	_BoneList[I].AbsoluteMatrix.Transform(VectorZero);

    With A Do
      glVertex3f(X,Y,Z);
  End;
  glEnd;

  (*
  QuadObj:=gluNewQuadric;
  gluQuadricNormals(QuadObj, GLU_NONE);
  gluQuadricTexture(QuadObj, False);

  For I:=0 To Pred(_BoneCount) Do
  Begin
    A := 	MeshInstance(Instance).BoneMatrixList[Succ(I)].Transform(VectorZero);
    Shader.SetUniform('modelMatrix', MatrixMultiply4x3(Transform, MatrixTranslation(A)));
    gluSphere(QuadObj, 0.25, 8, 8);           
  End;

  gluDeleteQuadric(QuadObj);              
*)

  glDepthMask(True);                      
  glDepthRange(0,1);                      

  GraphicsManager.Instance.EndColorShader;
{$ENDIF}
End;

Procedure MeshSkeleton.Read(Source: Stream);
Var
  Parents:Array Of TERRAString;
  I:Integer;
Begin
  Source.Read(@_BoneCount, 4);
  SetLength(_BoneList, _BoneCount);
  SetLength(Parents, _BoneCount);
  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I] := Nil;

  For I:=0 To Pred(_BoneCount) Do
  Begin
    _BoneList[I] := MeshBone.Create;
    _BoneList[I].Index := I;
    _BoneList[I].Owner := Self;
    _BoneList[I].Ready := False;
    _BoneList[I].Color := ColorWhite;
    _BoneList[I].Selected := False;
    Parents[I] := _BoneList[I].Read(Source);
  End;

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Parent := Self.GetBone(Parents[I]);

  Self.Init();
End;

Procedure MeshSkeleton.Init;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Ready := False;

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Init();

  (*SetLength(BindPose, Succ(_BoneCount));
  BindPose[0] := Matrix4x4Identity;
  For I:=0 To Pred(_BoneCount) Do
    BindPose[Succ(I)] := _BoneList[I].AbsoluteMatrix;*)

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].AbsoluteMatrix := Matrix4x4Inverse(_BoneList[I].AbsoluteMatrix);
End;

Procedure MeshSkeleton.Write(Dest: Stream);
Var
  I:Integer;
Begin
  Dest.Write(@_BoneCount, 4);
  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Write(Dest);
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

Function MeshSkeleton.GetBone(Const Name:TERRAString): MeshBone;
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
  B := _BoneList[_BoneList[Index].Parent.Index].AbsoluteMatrix.Transform(VectorZero);
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
    Bone := Other.GetBone(I);
    _BoneList[I] := MeshBone.Create;
    _BoneList[I].Name := Bone.Name;
    _BoneList[I].Index := I;
    _BoneList[I].Owner := Self;
    _BoneList[I].Color := Bone.Color;
    _BoneList[I].Selected := Bone.Selected;
    _BoneList[I].StartPosition := Bone.StartPosition;
    {$IFNDEF NO_ROTS}
    _BoneList[I].StartRotation := Bone.StartRotation;
    {$ENDIF}
    _BoneList[I].Ready := Bone.Ready;
    _BoneList[I].AbsoluteMatrix := Bone.AbsoluteMatrix;
    _BoneList[I].RelativeMatrix := Bone.RelativeMatrix;

    {$IFNDEF NO_ROTS}
    _BoneList[I].AbsoluteRotation := Bone.AbsoluteRotation;
    _BoneList[I].RelativeRotation := Bone.RelativeRotation;
    {$ENDIF}

    If Assigned(Bone.Parent) Then
      _BoneList[I].Parent := Self.GetBone(Bone.Parent.Name)
    Else
      _BoneList[I].Parent := Nil;
  End;

(*  SetLength(BindPose, Succ(_BoneCount));
  For I:=0 To _BoneCount Do
    BindPose[I] := Other.BindPose[I];*)
End;



End.
