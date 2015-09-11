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
 * TERRA_MeshAnimation
 * Implements the Animation resource and other Animation classes
 ***********************************************************************************************************************
}
Unit TERRA_MeshAnimation;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Stream, TERRA_Resource, TERRA_Vector3D, TERRA_Math,
  TERRA_Matrix4x4, TERRA_Vector2D, TERRA_Vector4D, TERRA_Color, TERRA_Quaternion, TERRA_ResourceManager,
  TERRA_MeshFilter, TERRA_MeshSkeleton;

Const
  TimeCompressionLimit = 20000;
  CompressionLimit1   = 127;
  CompressionLimit2   = 20000;

Type
  BoneAnimation = Class;

  VectorKeyframeArray = Class(TERRAObject)
    Protected
      _Owner:BoneAnimation;

      Procedure AddValueKey(Time:Single; Const Value:Vector4D);

    Public
      KeyFrames:Array Of MeshAnimationKeyframe;
      Count:Integer;

      Constructor Create(Owner:BoneAnimation);

      Procedure Clone(Other:VectorKeyframeArray);

      Function GetKey(Time:Single):Integer;

      Procedure AddVector3DKey(Const Time:Single; Const Value:Vector3D);
      Procedure AddQuaternionKey(Const Time:Single; Const Value:Quaternion);
      Procedure AddColorKey(Const Time:Single; Const Value:ColorRGBA);

      Function GetExactKey(Time:Single):Integer;

      Procedure Load(Source:TERRAStream);
      Procedure Save(Dest:TERRAStream);

      Function GetLength():Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Procedure Clear();

      Property Owner:BoneAnimation Read _Owner;
      Property Length:Single Read GetLength;
  End;

  Animation = Class;

  AnimationTransformBlock = Object
    Translation:Vector3D;
    Rotation:Quaternion;
    Scale:Vector3D;
  End;

  BoneAnimation = Class(TERRAObject)
      Name:TERRAString;
      ID:Integer;
      Owner:Animation;

      Positions:VectorKeyframeArray;
      Rotations:VectorKeyframeArray;
      Scales:VectorKeyframeArray;

      Constructor Create(ID:Integer; Owner:Animation);
      Procedure Release; Override;

      Procedure Load(Source:TERRAStream);
      Procedure Save(Dest:TERRAStream);

      Function GetLength():Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Procedure GetTransform(Time:Single; Out Block:AnimationTransformBlock);

      Property Length:Single Read GetLength;
  End;

  Animation = Class(TERRAResource)
    Protected
      _BoneCount:Integer;
      _Bones:Array Of BoneAnimation;
      _Duration:Single; // max * last key

      //Procedure computeTransforms(targetBone:MeshBone; sourceSkeleton, targetSkeleton:MeshSkeleton; frameId, animLength:Integer; Const ratio:Vector3D);

      Procedure RetargetBone(SourceBone, TargetBone:MeshBone; SourceAnimation, TargetAnimation:BoneAnimation);

    Public
      FPS:Single;
      Loop:Boolean;
      LoopPoint:Single;
      Speed:Single;

      Next:TERRAString;

      Procedure InitFromFilter(Const AnimationID:Integer; Source:MeshFilter);

      Procedure Clone(Other:Animation);
      Function Retarget(SourceSkeleton, TargetSkeleton:MeshSkeleton):Animation;

      Function Load(Source:TERRAStream):Boolean; Override;
      Procedure Save(Dest:TERRAStream); Overload;
      Procedure Save(FileName:TERRAString); Overload;

      Class Function GetManager:TERRAObject; Override;

      Function GetLength:Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Function AddBone(Const Name:TERRAString):BoneAnimation;
      Function GetBone(Index:Integer):BoneAnimation;
      Function GetBoneIndex(Const Name:TERRAString):Integer;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Property BoneCount:Integer Read _BoneCount;
      Property Length:Single Read GetLength;
  End;

  AnimationManager = Class(ResourceManager)
    Public
      Function GetAnimation(Name:TERRAString; ValidateError:Boolean = True):Animation;
   End;

Function FrameToTime(Frame, FPS:Single):Single;
Function TimeToFrame(Time, FPS:Single):Integer;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_Engine, TERRA_GraphicsManager, TERRA_FileStream, TERRA_FileUtils, TERRA_MeshAnimationNodes,
  TERRA_FileFormat;

Function FrameToTime(Frame, FPS:Single):Single;
Begin
  If (FPS>0) Then
    Result := Frame / FPS
  Else
    Result := 0;
End;

Function TimeToFrame(Time, FPS:Single):Integer;
Begin
  If (FPS>0) Then
    Result := Trunc(Time * FPS)
  Else
    Result := 0;
End;

{ AnimationManager }
Function AnimationManager.GetAnimation(Name:TERRAString; ValidateError:Boolean):Animation;
Var
  Location:TERRALocation;
Begin
  Result := Nil;
  Name := StringTrim(Name);
  If (Name='') Then
    Exit;

  If (StringFirstChar(Name) = '@') Then
    Name := StringCopy(Name, 2, MaxInt);

  Result := Animation(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    Location := Engine.Files.Search(Name+'.anim');
    If Assigned(Location) Then
    Begin
      Result := Animation.Create(rtLoaded, Location);
      Result.Priority := 70;
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      Engine.RaiseError('Could not find animation. ['+Name +']');
  End;
End;


{ VectorKeyframeArray }
Function VectorKeyframeArray.GetKey(Time:Single):Integer;
Var
  I:Integer;
Begin
  If (Count > 0) And (Time >= KeyFrames[Pred(Count)].Time) Then
  Begin
    Result := Pred(Count);
    Exit;
  End;

  Result := 0;
	For I:=1 To Pred(Count) Do
  Begin
	  If (KeyFrames[I].Time >= Time) Then
    Begin
      Result := I;
  	  Break;
    End;
  End;
End;

Function VectorKeyframeArray.GetExactKey(Time:Single):Integer;
Var
  I:Integer;
Begin
  Result := -1;
	For I:=0 To Pred(Count) Do
  Begin
	  If (Abs(KeyFrames[I].Time-Time)<=0.001) Then
    Begin
      Result := I;
  	  Exit;
    End;
  End;
End;

Procedure VectorKeyframeArray.AddValueKey(Time:Single; Const Value:Vector4D);
Var
  I, N:Integer;
Begin
  If (Time<0) Then
    Time := 0;

  N := -1;
  For I:=0 To Pred(Count) Do
  If KeyFrames[I].Time>Time Then
  Begin
    N := I;
    Break;
  End;

  Inc(Count);
  SetLength(KeyFrames, Count);
  If (N>=0) Then
  Begin
    For I:=Pred(Count) DownTo Succ(N) Do
      KeyFrames[I] := KeyFrames[I-1];
  End Else
    N := Pred(Count);

  KeyFrames[N].Time := Time;
  KeyFrames[N].Value := Value;
End;

Procedure VectorKeyframeArray.AddVector3DKey(Const Time:Single; Const Value:Vector3D);
Begin
  Self.AddValueKey(Time, Vector4D_Create(Value.X, Value.Y, Value.Z, 1.0));
End;

Procedure VectorKeyframeArray.AddQuaternionKey(Const Time:Single; Const Value:Quaternion);
Begin
  Self.AddValueKey(Time, Vector4D_Create(Value.X, Value.Y, Value.Z, Value.Z));
End;

Procedure VectorKeyframeArray.AddColorKey(Const Time:Single; Const Value:ColorRGBA);
Begin
  Self.AddValueKey(Time, Vector4D_Create(Value.R/255, Value.G/255, Value.B/255, Value.A/255));
End;


Procedure VectorKeyframeArray.Load(Source:TERRAStream);
Var
  I:Integer;
  Time:Single;
  Range:Vector3D;
  MaxTime:Single;
  PX,PY,PZ:SmallInt;
  SX,SY,SZ:ShortInt;
Begin
  Source.ReadInteger(Count);
 { If (StringLower(_Owner.Owner.Name)='monster084_idle') Then
    IntToString(2);}

  SetLength(KeyFrames, Count);
  For I:=0 To Pred(Count) Do
  Begin
    Source.ReadSingle(KeyFrames[I].Time);
    Source.ReadSingle(KeyFrames[I].Value.X);
    Source.ReadSingle(KeyFrames[I].Value.Y);
    Source.ReadSingle(KeyFrames[I].Value.Z);
    Source.ReadSingle(KeyFrames[I].Value.W);
  End;
End;

Procedure VectorKeyframeArray.Save(Dest:TERRAStream);
Var
  I:Integer;
Begin
  Dest.WriteInteger(Count);

  For I:=0 To Pred(Count) Do
  Begin
    Dest.WriteSingle(KeyFrames[I].Time);
    Dest.WriteSingle(KeyFrames[I].Value.X);
    Dest.WriteSingle(KeyFrames[I].Value.Y);
    Dest.WriteSingle(KeyFrames[I].Value.Z);
    Dest.WriteSingle(KeyFrames[I].Value.W);
  End;
End;

Procedure VectorKeyframeArray.CloseLoop;
Begin
  If Count<=0 Then
    Exit;

  Keyframes[Pred(Count)].Value := Keyframes[0].Value;
End;

{ BoneAnimation }
Constructor BoneAnimation.Create(ID:Integer; Owner:Animation);
Begin
  Self.ID := ID;
  Self.Owner := Owner;
  Self.Positions := VectorKeyframeArray.Create(Self);
  Self.Rotations := VectorKeyframeArray.Create(Self);
  Self.Scales := VectorKeyframeArray.Create(Self);
End;

Procedure BoneAnimation.Release;
Begin
  ReleaseObject(Positions);
  ReleaseObject(Rotations);
  ReleaseObject(Scales);
End;

Procedure BoneAnimation.Crop(Time: Single);
Begin
  Positions.Crop(Time);
  Rotations.Crop(Time);
  Scales.Crop(Time);
End;

Procedure BoneAnimation.CloseLoop;
Begin
  Positions.CloseLoop();
  Rotations.CloseLoop();
  Scales.CloseLoop();
End;

Function BoneAnimation.GetLength: Single;
Begin
  Result := FloatMax(Self.Positions.Length, FloatMax(Self.Rotations.Length, Self.Scales.Length));
End;

Procedure BoneAnimation.Load(Source:TERRAStream);
Begin
  Source.ReadString(Name);
  Positions.Load(Source);
  Rotations.Load(Source);
  Scales.Load(Source);
End;

Procedure BoneAnimation.Save(Dest:TERRAStream);
Var
  I:Integer;
Begin
  Dest.WriteString(Name);
  Positions.Save(Dest);
  Rotations.Save(Dest);
  Scales.Save(Dest);
End;

Procedure BoneAnimation.GetTransform(Time:Single; Out Block:AnimationTransformBlock);

Var
  Key, LastKey:Integer;
  DeltaTime : Single;
	Fraction : Single;

  Q1,Q2:Quaternion;
Begin
  // Find appropriate position key frame
  Key := Positions.GetKey(Time);
	If ( Key > 0 ) Then
	Begin
    LastKey := Pred(Key);
	  // Interpolate between 2 key frames

  	// time between the 2 key frames
		deltaTime := Positions.KeyFrames[Key].Time - Positions.KeyFrames[LastKey].Time;

		// relative position of interpolation point to the keyframes [0..1]
		Fraction := ((Time - Positions.KeyFrames[LastKey].Time) / deltaTime);
    If (Fraction < 0.0) Then
      Fraction := 0.0
    Else
    If (Fraction > 1.0) Then
      Fraction := 1.0;

    Block.Translation.X := Positions.KeyFrames[LastKey].Value.X + fraction * (Positions.KeyFrames[Key].Value.X - Positions.KeyFrames[LastKey].Value.X);
	  Block.Translation.Y := Positions.KeyFrames[LastKey].Value.Y + fraction * (Positions.KeyFrames[Key].Value.Y - Positions.KeyFrames[LastKey].Value.Y);
  	Block.Translation.Z := Positions.KeyFrames[LastKey].Value.Z + fraction * (Positions.KeyFrames[Key].Value.Z - Positions.KeyFrames[LastKey].Value.Z);
  End Else
  If (Key=0) And (Positions.Count>0) Then
 	  Block.Translation := Positions.KeyFrames[Key].GetVector3D()
  Else
    Block.Translation := Vector3D_Zero;

	// Rotation
  // Find appropriate rotation key frame
  Key := Rotations.GetKey(Time);
	If (Key > 0) Then
  Begin
    LastKey := Pred(Key);

	  // Interpolate between 2 key frames
  	// time between the 2 key frames
		deltaTime := Rotations.Keyframes[Key].Time - Rotations.Keyframes[LastKey].Time;

		// relative position of interpolation point to the keyframes [0..1]
	  Fraction := (Time - Rotations.Keyframes[LastKey].Time) / deltaTime;
    If (Fraction<0.0) Then
      Fraction := 0.0;
    If (Fraction>1.0) Then
      Fraction := 1.0;

   	Q1 := Rotations.Keyframes[LastKey].GetQuaternion();
	  Q2 := Rotations.Keyframes[Key].GetQuaternion();
  	Block.Rotation := QuaternionSlerp(Q1, Q2, Fraction);
    Block.Rotation.Normalize();
  End Else
  If (Key=0) And (Rotations.Count>0) Then
  Begin
    Block.Rotation := Rotations.Keyframes[Key].GetQuaternion();
  End Else
    Block.Rotation := QuaternionZero;

    //TODO
  Block.Scale := Vector3D_One;
End;


{ VectorKeyFrameArray }
Constructor VectorKeyframeArray.Create(Owner:BoneAnimation);
Begin
  Self._Owner := Owner;
End;

Procedure VectorKeyframeArray.Clone(Other: VectorKeyframeArray);
Var
  I:Integer;
Begin
  If Other = Nil Then
    Exit;

  Self.Count := Other.Count;
  SetLength(Self.Keyframes, Count);
  For I:=0 To Pred(Count) Do
    Self.Keyframes[I] := Other.Keyframes[I];
End;

Procedure VectorKeyframeArray.Clear;
Begin
  Count := 0;
End;

Function VectorKeyframeArray.GetLength: Single;
Begin
  If Count>0 Then
    Result := Keyframes[Pred(Count)].Time
  Else
    Result := 0;
End;

Procedure VectorKeyframeArray.Crop(Time: Single);
Var
  I:Integer;
Begin
  For I:=1 To Pred(Count) Do
  If (Keyframes[I].Time>Time) Then
  Begin
    Self.Count := Pred(I);
    SetLength(Keyframes, Self.Count);
    Exit;
  End;
End;

{ Animation }
Class Function Animation.GetManager:TERRAObject;
Begin
  Result := Engine.Animations;
End;

Function Animation.Load(Source:TERRAStream):Boolean;
Var
  Header:FileHeader;
  I, J, Count:Integer;
  S:TERRAString;
  Bone:BoneAnimation;
Begin
  Source.Read(@Header, 4);
  If (Not CompareFileHeader(Header, 'ANIM')) Then
  Begin
    Result := False;
    Exit;
  End;

  Source.ReadSingle(FPS);
  Source.ReadBoolean(Loop);
  Source.ReadSingle(LoopPoint);
  Source.ReadSingle(Speed);
  Source.ReadString(Next);

//  Speed := 0.1;

  Source.ReadInteger(Count);
  For I:=0 To Pred(Count) Do
  Begin
    Bone := Self.AddBone('');
    Bone.Load(Source);
  End;

  SetStatus(rsReady);
  Result := True;
End;

Procedure Animation.Save(FileName:TERRAString);
Var
  Stream:FileStream;
Begin
  Stream := FileStream.Create(FileName);
  Save(Stream);
  ReleaseObject(Stream);
End;

Procedure Animation.Save(Dest:TERRAStream);
Var
  Header:FileHeader;
  I:Integer;
Begin
  Header := 'ANIM';
  Dest.Write(@Header, 4);

  Dest.WriteSingle(FPS);
  Dest.WriteBoolean(Loop);
  Dest.WriteSingle(LoopPoint);
  Dest.WriteSingle(Speed);
  Dest.WriteString(Next);

  Dest.WriteInteger(_BoneCount);
  For I:=0 To Pred(_BoneCount) Do
    _Bones[I].Save(Dest);
End;

Function Animation.GetBoneIndex(Const Name:TERRAString):Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._BoneCount) Do
  If (StringEquals(Self._Bones[I].Name, Name)) Then
  Begin
    Result:=I;
    Exit;
  End;

  Result := -1;
End;

Function Animation.AddBone(Const Name:TERRAString):BoneAnimation;
Begin
  Result := BoneAnimation.Create(_BoneCount, Self);
  Result.Name := Name;

  Inc(_BoneCount);
  SetLength(_Bones, _BoneCount);
  _Bones[Pred(_BoneCount)] := Result;
End;

Function Animation.Unload: Boolean;
Var
  I,J:Integer;
Begin
  For I:=0 To Pred(_BoneCount) Do
    ReleaseObject(_Bones[I]);

  SetLength(_Bones,0);
  _BoneCount := 0;

  Result := Inherited Unload();
End;

Function Animation.Update: Boolean;
Begin
  Inherited Update();
  Result := True;
End;

(*

Linear interpolation (Vectors, scalars):
delta = b - a;
// alpha = [0..1]
c = a + delta*alpha

3x3 rotation matrix form (right-hand element evaluated first):

delta = b * transpose(a) // transpose(a) followed by b.
delta.getAxisAngle(axis,deltaAngle)
// alpha = [0..1]
c = axisAngleToMatrix(axis,deltaAngle*alpha) * a


inline void interpolate(const Mat3 & a,const Mat3 & b,flt alpha,Mat3 & c) {
  Mat3 delta = b ^ !a; // ^ = matrix product, ! = matrix transpose.
  Vec3 axis;
  flt deltaAngle;
  delta.getAxisAngle(axis,deltaAngle);
  Mat3 rm(axis,deltaAngle*alpha);
  c = rm ^ a;
} // interpolate

*)

Function Animation.GetBone(Index: Integer): BoneAnimation;
Begin
  If (Index<0) Or (Index>=_BoneCount) Then
    Result := Nil
  Else
    Result := _Bones[Index];
End;

Function Animation.GetLength: Single;
Var
  I:Integer;
Begin
  Result := 0;

  If (System.Length(_Bones)<Self.BoneCount) Then
    Exit;

  For I:=0 To Pred(Self.BoneCount) Do
    Result := FloatMax(Result, _Bones[I].GetLength());
End;

Procedure Animation.Crop(Time: Single);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self.BoneCount) Do
    _Bones[I].Crop(Time);
End;

Procedure Animation.CloseLoop;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self.BoneCount) Do
    _Bones[I].CloseLoop();
End;

Procedure Animation.Clone(Other: Animation);
Var
  I,J:Integer;
Begin
  If Other = Nil Then
    Exit;

  Self._BoneCount := Other.BoneCount;
  SetLength(Self._Bones, _BoneCount);
  For I:=0 To Pred(_BoneCount) Do
  Begin
    _Bones[I] := BoneAnimation.Create(I, Self);
    _Bones[I].Name := Other._Bones[I].Name;

    _Bones[I].Positions := VectorKeyframeArray.Create(_Bones[I]);
    _Bones[I].Positions.Clone(Other._Bones[I].Positions);

    _Bones[I].Rotations := VectorKeyframeArray.Create(_Bones[I]);
    _Bones[I].Rotations.Clone(Other._Bones[I].Rotations);

    _Bones[I].Scales := VectorKeyframeArray.Create(_Bones[I]);
    _Bones[I].Scales.Clone(Other._Bones[I].Scales);
  End;

  Self.FPS := Other.FPS;
  Self.Loop := Other.Loop;
  Self.LoopPoint := Other.LoopPoint;
  Self.Speed := Other.Speed;
  Self.Next := Other.Next;
End;

{this method recursively computes the transforms for each bone for a given frame, from a given sourceSkeleton (with bones updated to that frame)
the Bind transforms are the transforms of the bone when it's in the rest pose (aka T pose).
Wrongly called worldBindRotation in Bone implementation, those transforms are expressed in model space
the Model space transforms are the transforms of the bone in model space once the frame transforms has been applied}

Procedure Animation.RetargetBone(SourceBone, TargetBone:MeshBone; SourceAnimation, TargetAnimation:BoneAnimation);
Procedure CalcMatrices(Bone:MeshBone; Const FrameRotation:Vector3D; Out Bind, Frame:Matrix4x4);
Var
  T:Vector3D;
  Q:Quaternion;
Begin
(*  Q := SourceBone.Orientation;
  T := SourceBone.Translation;
  Bind := Matrix4x4Multiply4x3(Matrix4x4Translation(T), QuaternionMatrix4x4(Q));

  // Add the animation state to the rest position
  Q := QuaternionMultiply(SourceBone.Orientation, QuaternionRotation(FrameRotation));
  T := VectorAdd(SourceBone.Translation, {Block.Translation} VectorZero);
  Frame := Matrix4x4Multiply4x3(Matrix4x4Translation(T), QuaternionMatrix4x4(Q));*)
End;

Var
  I:Integer;
  SourceParent, TargetParent:MeshBone;

  CrossResult, BindPos, FramePos, ParentPos:Vector3D;

  turnAngle, cosAngle:Single;
  Q:Quaternion;

  BindMatrix, FrameMatrix:Matrix4x4;

  SourceAxis, TargetAxis, Direction:Vector3D;

Begin
  SourceParent := SourceBone.Parent;
  TargetParent := TargetBone.Parent;

  TargetAnimation.Positions.Count := 0;
  TargetAnimation.Scales.count := 0;

  If SourceParent = Nil Then
  Begin
    TargetAnimation.Rotations.Count := 0;

    Exit;
  End;

(*  For I:=0 To Pred(TargetAnimation.Rotations.Count) Do
    Begin
      CalcMatrices(SourceBone, TargetAnimation.Rotations.KeyFrames[I].Value, BindMatrix, FrameMatrix);

      BindPos := BindMatrix.Transform(SourceBone.AbsolutePosition);
      FramePos := FrameMatrix.Transform(SourceBone.AbsolutePosition);

      TargetAxis := VectorSubtract(FramePos, BindPos);
      TargetAxis.Normalize();

      SourceAxis := TargetBone.Normal;

      cosAngle := VectorDot(TargetAxis, SourceAxis);

      CrossResult := VectorCross(TargetAxis, SourceAxis);
      CrossResult.Normalize();

      turnAngle := arccos(cosAngle);	// GET THE ANGLE

      //  turnAngle := 45*RAD;  CrossResult := VectorUp;

      Q := QuaternionFromAxisAngle(crossResult, turnAngle);


      TargetAnimation.Rotations.KeyFrames[I].Value := QuaternionToEuler(Q);
    End;*)
End;

Function Animation.Retarget(SourceSkeleton, TargetSkeleton: MeshSkeleton): Animation;
Var
  I,J, K:Integer;
  Rots:VectorKeyframeArray;

  SourceBone, TargetBone:MeshBone;

  BonePos, ParentPos, U, V, W:Vector3D;

  inverseTargetParentModelRot:Quaternion;
  targetLocalRot, targetInverseBindRotation, twist:Quaternion;

  M:Matrix4x4;
Begin
Exit;
  TargetSkeleton.NormalizeJoints();

  Result := Animation.Create(rtDynamic);
  Result.Clone(Self);

  For I:=0 To Pred(_BoneCount) Do
  Begin
    SourceBone := SourceSkeleton.GetBoneByName(_Bones[I].Name);
    TargetBone := TargetSkeleton.GetBoneByName(_Bones[I].Name);

    Self.RetargetBone(SourceBone, TargetBone, Self._Bones[I], Result._Bones[I]);
  End;
End;

Procedure Animation.InitFromFilter(Const AnimationID:Integer; Source: MeshFilter);
Var
  I, J:Integer;
  Bone:BoneAnimation;
Begin
  _BoneCount := 0;
  Self.FPS := Source.GetAnimationFrameRate(AnimationID);
  Self.Loop := Source.GetAnimationLoop(AnimationID);
  Self.Speed := 1;

  For I:=0 To Pred(Source.GetBoneCount()) Do
  Begin
    Bone := Self.AddBone(Source.GetBoneName(I));

    Bone.Positions.Count := Source.GetPositionKeyCount(AnimationID, I);
    SetLength(Bone.Positions.Keyframes, Bone.Positions.Count);
    For J:=0 To Pred(Bone.Positions.Count) Do
      Bone.Positions.Keyframes[J] := Source.GetPositionKey(AnimationID, I, J);

    Bone.Rotations.Count := Source.GetRotationKeyCount(AnimationID, I);
    SetLength(Bone.Rotations.Keyframes, Bone.Rotations.Count);
    For J:=0 To Pred(Bone.Rotations.Count) Do
      Bone.Rotations.Keyframes[J] := Source.GetRotationKey(AnimationID, I, J);

    Bone.Scales.Count := Source.GetScaleKeyCount(AnimationID, I);
    SetLength(Bone.Scales.Keyframes, Bone.Scales.Count);
    For J:=0 To Pred(Bone.Scales.Count) Do
      Bone.Scales.Keyframes[J] := Source.GetScaleKey(AnimationID, I, J);
  End;
End;

End.
