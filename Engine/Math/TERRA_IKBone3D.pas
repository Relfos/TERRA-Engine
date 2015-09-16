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
 * Implements inverse kinematics
 ***********************************************************************************************************************
}
Unit TERRA_IKBone3D;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_Vector2D, TERRA_Vector3D, TERRA_Quaternion, TERRA_Matrix4x4;

Type
  IKBone3D = Class(TERRAObject)
    Protected
      _Parent:IKBone3D;
      _Child:IKBone3D;

      _ChildCount:Integer;

      _Position:Vector3D;
      _Rotation:Quaternion;

    	// DOF CONSTRAINTS
      _MaxRot:Vector3D;
      _MinRot:Vector3D;

      // DAMPENING SETTINGS
      _DampWidth:Single;
      _DampStrength:Single;

      Function GetEffector():IKBone3D;
      Function GetChainSize: Integer;

      Function GetValidRotation(Q:Quaternion):Quaternion;

    Public
      Constructor Create(ChainSize:Integer; Parent:IKBone3D = Nil);
      Procedure Release(); Override;

      Function GetRelativeMatrix:Matrix4x4;
      Function GetAbsoluteMatrix:Matrix4x4;

      Function GetChainBone(Index:Integer):IKBone3D;

      Function Solve(Const EndPos:Vector3D; ApplyDamping, ApplyDOF:Boolean):Boolean;

      Property Position:Vector3D Read _Position Write _Position;
      Property Rotation:Quaternion Read _Rotation Write _Rotation;

      Property ChainSize:Integer Read GetChainSize;
      Property ChildCount:Integer Read _ChildCount;

      Property Parent:IKBone3D Read _Parent;
      Property Child:IKBone3D Read _Child;
  End;

Implementation
Uses TERRA_Math;

Const
  MAX_IK_TRIES  =	100;	// max iteratiors for the CCD loop (TRIES = # / LINKS)
  IK_POS_THRESH	= 0.1;	// distance thresold for sucess

{ IKBone3D }
Constructor IKBone3D.Create(ChainSize:Integer; Parent:IKBone3D = Nil);
Begin
  If (Parent = Nil) Then
    Dec(ChainSize);

  Self._Parent := Parent;
  Self._ChildCount := ChainSize;

  If ChainSize>0 Then
    Self._Child := IKBone3D.Create(Pred(ChainSize), Self)
  Else
    Self._Child := Nil;

  _DampWidth := 5.0 * RAD;

  Self._Rotation := QuaternionZero;

  // default DOF restrictions
  _MinRot := VectorCreate(0, 0, -30 * RAD);
  _MaxRot := VectorCreate(0, 0, 30 * RAD);
End;

Procedure IKBone3D.Release;
Begin
  If Assigned(_Child) Then
    ReleaseObject(_Child);
End;

Function IKBone3D.GetChainSize: Integer;
Begin
  Result := Succ(_ChildCount);
End;

Function IKBone3D.GetAbsoluteMatrix: Matrix4x4;
Begin
  Result := Self.GetRelativeMatrix();
  If Assigned(Self.parent) Then
    Result := Matrix4x4Multiply4x3(Parent.GetAbsoluteMatrix(), Result);
End;

Function IKBone3D.GetRelativeMatrix: Matrix4x4;
Begin
  Result := Matrix4x4Multiply4x3(Matrix4x4Translation(_Position), QuaternionMatrix4x4(_Rotation));
End;

Function IKBone3D.GetEffector():IKBone3D;
Begin
  If Self.Child = Nil Then
    Result := Self
  Else
    Result := Self.Child.GetEffector();
End;

Function IKBone3D.Solve(Const EndPos:Vector3D; ApplyDamping, ApplyDOF:Boolean): Boolean;
Var
	rootPos,curEnd, desiredEnd:Vector3D;
  targetVector,curVector:Vector3D;
  crossResult:Vector3D;
	cosAngle,turnAngle:Double;
	Effector, Link:IKBone3D;
  Tries:Integer;
  Q:Quaternion;
Begin
	// START AT THE LAST LINK IN THE CHAIN
  Effector := Self.GetEffector();
	Link := Effector.Parent;
	Tries := 0;						// LOOP COUNTER SO I KNOW WHEN TO QUIT
	Repeat
		// THE COORDS OF THE X,Y,Z POSITION OF THE ROOT OF THIS BONE IS IN THE MATRIX
		// TRANSLATION PART WHICH IS IN THE 12,13,14 POSITION OF THE MATRIX
		rootPos := Link.GetAbsoluteMatrix().GetTranslation;

		// POSITION OF THE END EFFECTOR
		curEnd := Effector.GetAbsoluteMatrix().GetTranslation;

		// DESIRED END EFFECTOR POSITION
		desiredEnd := endPos;

		// SEE IF I AM ALREADY CLOSE ENOUGH
		If (VectorSubtract(curEnd, desiredEnd).LengthSquared <= IK_POS_THRESH) Then
    Begin
      Result := True;
      Exit;
    End;

    // CREATE THE VECTOR TO THE CURRENT EFFECTOR POS
		curVector := VectorSubtract(curEnd, rootPos);

		// CREATE THE DESIRED EFFECTOR POSITION VECTOR
		targetVector := VectorSubtract(endPos, rootPos);

    // NORMALIZE THE VECTORS (EXPENSIVE, REQUIRES A SQRT)
		curVector.Normalize;
		targetVector.Normalize;

    // THE DOT PRODUCT GIVES ME THE COSINE OF THE DESIRED ANGLE
		cosAngle := VectorDot(targetVector, curVector);

		// IF THE DOT PRODUCT RETURNS 1.0, I DON'T NEED TO ROTATE AS IT IS 0 DEGREES
		If (cosAngle < 0.99999) Then
    Begin
      // USE THE CROSS PRODUCT TO CHECK WHICH WAY TO ROTATE
			crossResult := VectorCross(targetVector, curVector);
      crossResult.Normalize();

      turnAngle := arccos(cosAngle);	// GET THE ANGLE

      // DAMPING
			If (ApplyDamping) And (turnAngle > Link._DampWidth) Then
        turnAngle := Link._DampWidth;

      Q := QuaternionFromAxisAngle(crossResult, turnAngle);	// ACTUALLY TURN THE LINK

      Q := QuaternionMultiply(Link._Rotation, Q);
      Q.Normalize();

      Link._Rotation := Self.GetValidRotation(Q);
    End;

    Link := Link.Parent;

		If (Link = Nil) Then
      Link := Effector.Parent;	// START OF THE CHAIN, RESTART

	  // QUIT IF BEEN RUNNING LONG ENOUGH
    Inc(Tries);
	Until  (tries >= MAX_IK_TRIES);

	Result := False;
End;

Function IKBone3D.GetChainBone(Index: Integer): IKBone3D;
Begin
  If (Index = 0) Then
    Result := Self
  Else
  If (Index<0) Or (_Child = Nil) Then
    Result := Nil
  Else
    Result := _Child.GetChainBone(Pred(Index));
End;

Function IKBone3D.IsValidRotation(Q:Quaternion):Boolean;
Var
  Euler:Vector3D;
Begin
	// FIRST STEP IS TO CONVERT LINK QUATERNION BACK TO EULER ANGLES
	Euler := QuaternionToEuler(Q);

  Result := False;
	If (euler.x > Self._MaxRot.X) Then
		Exit;

	If (euler.x < Self._MinRot.X) Then
		Exit;

	If (euler.y > Self._MaxRot.y) Then
		Exit;

	If (euler.y < Self._MinRot.y) Then
		Exit;

	If (euler.z > Self._MaxRot.z) Then
		Exit;

	If (euler.z < Self._MinRot.z) Then
		Exit;

  Result := True;
	// CHECK THE DOF SETTINGS
(*	If (euler.x > Self._MaxRot.X) Then
		euler.x := Self._MaxRot.x;

	If (euler.x < Self._MinRot.X) Then
		euler.x := Self._MinRot.x;

	If (euler.y > Self._MaxRot.y) Then
		euler.y := Self._MaxRot.y;

	If (euler.y < Self._MinRot.y) Then
		euler.y := Self._MinRot.y;

	If (euler.z > Self._MaxRot.z) Then
		euler.z := _MaxRot.z
  Else
	If (euler.z < Self._MinRot.z) Then
		euler.z := Self._MinRot.z;

  Result := QuaternionRotation(Euler);*)
End;

End.
