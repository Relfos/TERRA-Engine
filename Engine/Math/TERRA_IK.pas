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
 * TERRA_IK
 * Implements Inverse Kinematics
 ***********************************************************************************************************************
}
// http://www.geekyblogger.com/2008/04/cyclic-coordinate-descent-ccd.html

Unit TERRA_IK;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_Matrix, TERRA_Quaternion,
  TERRA_MeshAnimation;

Const
  ikWaiting     = 0;
  ikProcessing  = 1;
  ikFinished    = 2;
  ikFailed      = 3;

Type
  IKBone = Class(TERRAObject)
    Public
      Bone:MeshBone;
      Position:Vector3D;
      Rotation:Quaternion;
  End;

  IKChain = Class(TERRAObject)
    Protected
      _Bones:Array Of IKBone;
      _Count:Integer;
      _Target:Vector3D;
      _Iterations:Integer;
      _Status:Integer;
      _Transform:Matrix;
      _State:AnimationState;

      Procedure CheckDOFRestrictions(I:Integer);

    Public
      Constructor Create;
      Procedure Release;

      Procedure AddBone(B:MeshBone);

      Procedure Start(endPos:Vector3D; State:AnimationState; Transform:Matrix);
      Function Solve(Iterations:Integer):Boolean;

      Function GetBone(Index:Integer):IKBone;
      Function Contains(BoneID:Integer):Boolean;

      Procedure Render;

      Property Size:Integer Read _Count;
      Property Iterations:Integer Read _Iterations;
      Property Status:Integer Read _Status;
  End;

Const
  IK_POS_THRESH = 1.0;
  MAX_IK_TRIES = 10;

Implementation
Uses TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Color, TERRA_Log, Math;

// Procedure:	CheckDOFRestrictions
// Purpose:		Make sure link is within valid DOF
Procedure IKChain.CheckDOFRestrictions(I:Integer);
Var
  Euler:Vector3D;
  B:MeshBone;
  Q:Quaternion;
  Changed:Boolean;
  Procedure TestMax(Var P:Single; Limit:Single);
  Begin
    If (P>Limit) Then
    Begin
      P := Limit;
      Changed := True;
    End;
  End;
  Procedure TestMin(Var P:Single; Limit:Single);
  Begin
    If (P<Limit) Then
    Begin
      P := Limit;
      Changed := True;
    End;
  End;
Begin
	// FIRST STEP IS TO CONVERT LINK QUATERNION BACK TO EULER ANGLES

  Q := _Bones[I].Rotation;
	Euler := QuaternionToEuler(_Bones[I].Rotation);
  B := _Bones[I].Bone;

  Changed := False;

	// CHECK THE DOF SETTINGS
	TestMax(Euler.x, B.MaxRotation.X);
	TestMax(Euler.y, B.MaxRotation.Y);
	TestMax(Euler.z, B.MaxRotation.Z);
	TestMin(Euler.x, B.MinRotation.X);
	TestMin(Euler.y, B.MinRotation.Y);
	TestMin(Euler.z, B.MinRotation.Z);

  If (Changed) Then
    _Bones[I].Rotation := QuaternionRotation(Euler);

  Q.Subtract(_Bones[I].Rotation);
End;

Procedure IKChain.Start(endPos:Vector3D; State:AnimationState; Transform:Matrix);
Var
  B:MeshBone;
  I:Integer;
Begin
  _Transform := Transform;
  _Status := ikProcessing;
  _Iterations := 0;
  _Target := endPos;
  _State := State;

  Self.Solve(1);
End;

///////////////////////////////////////////////////////////////////////////////
// Procedure:	ComputeCCDLink
// Purpose:		Compute an IK Solution to an end effector position in 3D
// Arguments:	End Target (x,y,z)

// Returns:		TRUE if a solution exists, FALSE if the position isn't in reach
///////////////////////////////////////////////////////////////////////////////

Function IKChain.Solve(Iterations:Integer):Boolean;
Var
  I:Integer;
	rootPos,curEnd,desiredEnd,targetVector,curVector,crossResult:Vector3D;
	cosAngle,turnAngle:Double;
	link, N:Integer;
	aquat:Quaternion;

  Procedure UpdatePositions;
  Var
    I:Integer;
    T, Rel, M:Matrix;
    P:Vector3D;
  Begin
    If (_Bones[0].Bone.Parent<>Nil) Then
      T := _State.Transforms[_Bones[0].Bone.Parent.Index+1]
    Else
      T := MatrixIdentity;

    For I:=0 To Pred(_Count) Do
    Begin
      Rel := _Bones[I].Bone.RelativeMatrix;
      M := QuaternionMatrix(_Bones[I].Rotation);
      Rel := MatrixMultiply4x3(Rel, M);
      T := MatrixMultiply4x3(T, Rel);

      P := T.Transform(VectorZero);
      P := _Transform.Transform(P);

      _Bones[I].Position := P;
//      Log(logDebug, 'IK', _Bones[I].Bone.Name);
    End;
  End;

Begin
	// START AT THE LAST LINK IN THE CHAIN
	link := _Count-2;
  N := 0;

  UpdatePositions;

	Repeat
		// THE COORDS OF THE X,Y,Z POSITION OF THE ROOT OF THIS BONE IS IN THE MATRIX
		// TRANSLATION PART WHICH IS IN THE 12,13,14 POSITION OF THE MATRIX
		rootPos := _Bones[link].Position;

		// POSITION OF THE END EFFECTOR
		curEnd := _Bones[Pred(_Count)].Position;

		// DESIRED END EFFECTOR POSITION
		desiredEnd := _Target;

		// SEE IF I AM ALREADY CLOSE ENOUGH
		if (curEnd.Distance(desiredEnd) > IK_POS_THRESH) Then
		Begin
			// CREATE THE VECTOR TO THE CURRENT EFFECTOR POS
			curVector := VectorSubtract(curEnd, rootPos);

			// CREATE THE DESIRED EFFECTOR POSITION VECTOR
			targetVector := VectorSubtract(_Target, rootPos);

			// NORMALIZE THE VECTORS (EXPENSIVE, REQUIRES A SQRT)
			curVector.Normalize;
			targetVector.Normalize;

			// THE DOT PRODUCT GIVES ME THE COSINE OF THE DESIRED ANGLE
			cosAngle := targetVector.Dot(curVector);

			// IF THE DOT PRODUCT RETURNS 1.0, I DON'T NEED TO ROTATE AS IT IS 0 DEGREES
			If (cosAngle < 0.99999) Then
			Begin
				// USE THE CROSS PRODUCT TO CHECK WHICH WAY TO ROTATE
				crossResult := VectorCross(curVector, targetVector);
				crossResult.Normalize;

				turnAngle := arccos(cosAngle);	// GET THE ANGLE
				// DAMPING
				//if (m_Damping && turnDeg > m_Link[link].damp_width)
				//	turnDeg = m_Link[link].damp_width;

        aquat := QuaternionFromAxisAngle(crossResult, turnAngle);
        _Bones[Link].Rotation := QuaternionMultiply(_Bones[Link].Rotation, aquat);

				// HANDLE THE DOF RESTRICTIONS IF I WANT THEM
				//if (m_DOF_Restrict)
          CheckDOFRestrictions(Link);

				// RECALC ALL THE MATRICES WITHOUT DRAWING ANYTHING
				UpdatePositions;		// CHANGE THIS TO TRUE IF YOU WANT TO SEE THE ITERATION
			End;

      Dec(Link);
			If (link < 0) Then
        link := _Count - 2;	// START OF THE CHAIN, RESTART
  End;

  If (curEnd.Distance(desiredEnd) <= IK_POS_THRESH) Then
  Begin
    _Status := ikFinished;
    Result := True;
    Exit;
  End;

	// QUIT IF I AM CLOSE ENOUGH OR BEEN RUNNING LONG ENOUGH
  Inc(_Iterations);
  Inc(N);
	Until (_Iterations>MAX_IK_TRIES) Or (N>=Iterations);

  If (_Iterations > MAX_IK_TRIES) Then
    _Status := ikFailed;

  Result := False;
End;

Constructor IKChain.Create;
Begin
  _Count := 0;
End;

Procedure IKChain.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
    _Bones[I].Release;
End;

{ IKChain }
Procedure IKChain.AddBone(B: MeshBone);
Var
  I,N:Integer;
Begin
  Inc(_Count);
  SetLength(_Bones, _Count);
  For I:=Pred(_Count) DownTo 1 Do
    _Bones[I] := _Bones[I-1];

  _Bones[0] := IKBone.Create;
  _Bones[0].Bone := B;
  _Bones[0].Rotation := QuaternionRotation(VectorZero);
End;

Function IKChain.GetBone(Index: Integer): IKBone;
Begin
  If (Index>=0) And (Index<_Count) Then
    Result := _Bones[Index]
  Else
    Result := Nil;
End;

Function IKChain.Contains(BoneID:Integer):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
  If (_Bones[I].Bone.Index = BoneID) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure IKChain.Render;
Var
  P:Vector3D;
  I:Integer;
Begin
  glDepthMask(True);                      
  glDepthRange(0,0.0001);                 
  glPointSize(12);

  GraphicsManager.Instance.EnableColorShader(ColorYellow, MatrixIdentity);
  glBegin(GL_POINTS);
  For I:=0 To Pred(_Count) Do
  Begin
    P := _Bones[I].Position;
    glVertex3f(P.X, P.Y, P.Z);
  End;
	glEnd();

  glDepthMask(True);                      
  glDepthRange(0,1);                      
End;

End.
