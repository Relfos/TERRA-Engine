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
Unit TERRA_IKBone2D;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_Vector2D, TERRA_Vector3D, TERRA_Matrix3x3;

Type
  IKBone2D = Class(TERRAObject)
    Protected
      _Parent:IKBone2D;
      _Child:IKBone2D;

      _ChildCount:Integer;

      _Position:Vector2D;
      _Rotation:Single;

    	// DOF CONSTRAINTS
      _MaxRot:Single;
      _MinRot:Single;

      // DAMPENING SETTINGS
      _DampWidth:Single;
      _DampStrength:Single;

      Function GetEffector():IKBone2D;
      Function GetChainSize: Integer;

    Public
      Constructor Create(ChainSize:Integer; Parent:IKBone2D = Nil; DOF:Single = 40);
      Procedure Release(); Override;

      Function GetRelativeMatrix:Matrix3x3;
      Function GetAbsoluteMatrix:Matrix3x3;

      Procedure SetLimits(Min, Max:Single);
      Procedure SetLimitsInDegrees(Min, Max:Single);

      Function GetChainBone(Index:Integer):IKBone2D;

      Function Solve(Const EndPos:Vector2D; ApplyDamping, ApplyDOF:Boolean):Boolean;

      Property Position:Vector2D Read _Position Write _Position;
      Property Rotation:Single Read _Rotation Write _Rotation;

      Property ChainSize:Integer Read GetChainSize;
      Property ChildCount:Integer Read _ChildCount;

      Property Parent:IKBone2D Read _Parent;
      Property Child:IKBone2D Read _Child;
  End;

Implementation
Uses TERRA_Math;

Const
  MAX_IK_TRIES  =	100;		// max iteratiors for the CCD loop (TRIES = # / LINKS)
  IK_POS_THRESH	= 1.0;	// angle thresold for sucess

{ IKBone2D }
Constructor IKBone2D.Create(ChainSize:Integer; Parent:IKBone2D = Nil; DOF:Single = 40);
Begin
  If (Parent = Nil) Then
    Dec(ChainSize);

  Self._Parent := Parent;
  Self._ChildCount := ChainSize;

  If ChainSize>0 Then
    Self._Child := IKBone2D.Create(Pred(ChainSize), Self)
  Else
    Self._Child := Nil;

  _DampWidth := 5.0 * RAD;

  // default DOF restrictions
  _MinRot := -DOF * RAD;
  _MaxRot := DOF * RAD;
End;

Procedure IKBone2D.Release;
Begin
  If Assigned(_Child) Then
    ReleaseObject(_Child);
End;

Function IKBone2D.GetChainSize: Integer;
Begin
  Result := Succ(_ChildCount);
End;

Function IKBone2D.GetAbsoluteMatrix: Matrix3x3;
Begin
  Result := Self.GetRelativeMatrix();
  If Assigned(Self.parent) Then
    Result := Matrix3x3_Multiply(Parent.GetAbsoluteMatrix(), Result);
End;

Function IKBone2D.GetRelativeMatrix: Matrix3x3;
Begin
  Result := Matrix3x3_Rotation(_Rotation);
  Result.SetTranslation(_Position);
End;

Function IKBone2D.GetEffector():IKBone2D;
Begin
  If Self.Child = Nil Then
    Result := Self
  Else
    Result := Self.Child.GetEffector();
End;

Function IKBone2D.Solve(Const EndPos:Vector2D; ApplyDamping, ApplyDOF:Boolean): Boolean;
Var
	rootPos,curEnd, desiredEnd:Vector2D;
  targetVector,curVector:Vector2D;
  crossResult:Vector3D;
	cosAngle,turnAngle:Double;
	Effector, Link:IKBone2D;
  Tries:Integer;
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
    If (Vector2D_Subtract(curEnd, desiredEnd).LengthSquared <= IK_POS_THRESH) Then
    Begin
      Result := True;
      Exit;
    End;

    // CREATE THE VECTOR TO THE CURRENT EFFECTOR POS
		curVector := Vector2D_Subtract(curEnd, rootPos);

		// CREATE THE DESIRED EFFECTOR POSITION VECTOR
		targetVector := Vector2D_Subtract(endPos, rootPos);

		// NORMALIZE THE VECTORS (EXPENSIVE, REQUIRES A SQRT)
		curVector.Normalize;
		targetVector.Normalize;

		// THE DOT PRODUCT GIVES ME THE COSINE OF THE DESIRED ANGLE
		cosAngle := Vector2D_Dot(targetVector, curVector);

    // IF THE DOT PRODUCT RETURNS 1.0, I DON'T NEED TO ROTATE AS IT IS 0 DEGREES
		If (cosAngle < 0.99999) Then
    Begin
      // USE THE CROSS PRODUCT TO CHECK WHICH WAY TO ROTATE
			crossResult := Vector3D_Cross(Vector3D_Create(targetVector.X, targetVector.Y, 0), Vector3D_Create(curVector.X, curVector.Y, 0));
			If (crossResult.z > 0.0)	Then // IF THE Z ELEMENT IS POSITIVE, ROTATE CLOCKWISE
			Begin
			  turnAngle := arccos(cosAngle);	// GET THE ANGLE

				// DAMPING
				If (ApplyDamping) And (turnAngle > Link._DampWidth) Then
          turnAngle := Link._DampWidth;

        Link._Rotation := Link._Rotation - turnAngle;	// ACTUALLY TURN THE LINK

				// DOF RESTRICTIONS
				If (ApplyDOF) And (Link._Rotation < Link._MinRot) Then
          Link._Rotation := Link._MinRot;

      End Else
      If (crossResult.z < 0.0) Then	// ROTATE COUNTER CLOCKWISE
			Begin
        turnAngle := ArcCos(cosAngle);
				// DAMPING
        If (ApplyDamping) And (turnAngle > Link._DampWidth) Then
				  turnAngle := Link._DampWidth;

        Link._Rotation := Link._Rotation + turnAngle;	// ACTUALLY TURN THE LINK

				// DOF RESTRICTIONS
				If (ApplyDOF) And (Link._Rotation > Link._MaxRot) Then
          Link._Rotation := Link._MaxRot;
      End;
		End;

    Link := Link.Parent;

    If (Link = Nil) Then
      Link := Effector.Parent;	// START OF THE CHAIN, RESTART

	  // QUIT IF BEEN RUNNING LONG ENOUGH
    Inc(Tries);
	Until  (tries >= MAX_IK_TRIES);

	Result := False;
End;

Function IKBone2D.GetChainBone(Index: Integer): IKBone2D;
Begin
  If (Index = 0) Then
    Result := Self
  Else
  If (Index<0) Or (_Child = Nil) Then
    Result := Nil
  Else
    Result := _Child.GetChainBone(Pred(Index));
End;


Procedure IKBone2D.SetLimits(Min, Max: Single);
Begin
  _MaxRot := Max;
  _MinRot := Min;
End;

Procedure IKBone2D.SetLimitsInDegrees(Min, Max: Single);
Begin
  Self.SetLimits(Min * RAd, Max  * RAD);
End;

End.
