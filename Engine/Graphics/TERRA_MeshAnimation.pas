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
  TERRA_Matrix4x4, TERRA_Vector2D, TERRA_Color, TERRA_Quaternion, TERRA_ResourceManager, TERRA_MeshSkeleton;

Const
  TimeCompressionLimit = 20000;
  CompressionLimit1   = 127;
  CompressionLimit2   = 20000;

Type
  BoneAnimation = Class;

  VectorKeyFrame=Record
    Time:Single;
    Value:Vector3D;
  End;

  VectorKeyframeArray = Class(TERRAObject)
    Protected
      _Owner:BoneAnimation;

    Public
      Keyframes:Array Of VectorKeyFrame;
      Count:Integer;

      Constructor Create(Owner:BoneAnimation);

      Procedure Clone(Other:VectorKeyframeArray);

      Function GetKey(Time:Single):Integer;
      Procedure AddKey(Time:Single; Value:Vector3D);

      Function GetExactKey(Time:Single):Integer;

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

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
    Scale:Vector3D;
    Rotation:Quaternion;
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

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

      Function GetLength():Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Procedure GetTransform(Time:Single; Out Block:AnimationTransformBlock);

      Property Length:Single Read GetLength;
  End;


  Animation = Class(Resource)
    Protected
      _BoneCount:Integer;
      _Bones:Array Of BoneAnimation;
      _Duration:Single; // max * last key

    Public
      FPS:Single;
      Loop:Boolean;
      LoopPoint:Single;
      Speed:Single;

      Next:TERRAString;

      Procedure Clone(Other:Animation);

      Function Load(Source:Stream):Boolean; Override;
      Procedure Save(Dest:Stream); Overload;
      Procedure Save(FileName:TERRAString); Overload;

      Class Function GetManager:Pointer; Override;

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
      Procedure Release; Override;
      Class Function Instance:AnimationManager;

      Function GetAnimation(Name:TERRAString; ValidateError:Boolean = True):Animation;
   End;

Function FrameToTime(Frame, FPS:Single):Single;
Function TimeToFrame(Time, FPS:Single):Integer;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_GraphicsManager, TERRA_FileStream, TERRA_FileUtils;

Var
  _AnimationManager:ApplicationObject;

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
Class Function AnimationManager.Instance:AnimationManager;
Begin
  If _AnimationManager = Nil Then
  Begin
    _AnimationManager := InitializeApplicationComponent(AnimationManager, Nil);
    AnimationManager(_AnimationManager.Instance).AutoUnload := False;
  End;
  Result := AnimationManager(_AnimationManager.Instance);
End;


Procedure AnimationManager.Release;
Begin
  Inherited;
  _AnimationManager := Nil;
End;

Function AnimationManager.GetAnimation(Name:TERRAString; ValidateError:Boolean):Animation;
Var
  S:TERRAString;
Begin
  Result := Nil;
  Name := StringTrim(Name);
  If (Name='') Then
    Exit;

  If (StringFirstChar(Name) = Ord('@')) Then
    Name := StringCopy(Name, 2, MaxInt);

  Result := Animation(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Name+'.anim');
    If S<>'' Then
    Begin
      Result := Animation.Create(rtLoaded, S);
      Result.Priority := 70;
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      RaiseError('Could not find animation. ['+Name +']');
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

Procedure VectorKeyframeArray.AddKey(Time:Single; Value:Vector3D);
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

Procedure VectorKeyframeArray.Load(Source:Stream);
Var
  I:Integer;
  Time:Word;
  Range:Vector3D;
  MaxTime:Single;
  PX,PY,PZ:SmallInt;
  SX,SY,SZ:ShortInt;
Begin
  Source.Read(@Count, 4);
  Source.Read(@Range, SizeOf(Vector3D));
  Source.Read(@MaxTime, 4);

 { If (StringLower(_Owner.Owner.Name)='monster084_idle') Then
    IntToString(2);}

  SetLength(KeyFrames, Count);
  For I:=0 To Pred(Count) Do
  Begin
    Source.Read(@Time, 2);

    KeyFrames[I].Time := (Time/TimeCompressionLimit) * MaxTime;
    If (KeyFrames[I].Time>MaxTime) Then
      IntToString(TRunc(KeyFrames[I].Time+MaxTime));

    {Source.Read(@PX, 2);
    Source.Read(@PY, 2);
    Source.Read(@PZ, 2);
    KeyFrames[I].Value.X := (PX/CompressionLimit2)* Range.X;
    KeyFrames[I].Value.Y := (PY/CompressionLimit2)* Range.Y;
    KeyFrames[I].Value.Z := (PZ/CompressionLimit2)* Range.Z;}

    Source.Read(@SX, 1);
    Source.Read(@SY, 1);
    Source.Read(@SZ, 1);
    KeyFrames[I].Value.X := (SX/CompressionLimit1)* Range.X;
    KeyFrames[I].Value.Y := (SY/CompressionLimit1)* Range.Y;
    KeyFrames[I].Value.Z := (SZ/CompressionLimit1)* Range.Z;

    //Source.Read(@KeyFrames[I].Value, SizeOf(Vector3D));
  End;
End;

Procedure VectorKeyframeArray.Save(Dest:Stream);
Var
  I:Integer;
  Time:Word;
  MaxTime:Single;
  PX,PY,PZ:SmallInt;
  SX,SY,SZ:ShortInt;
  Range:Vector3D;
Begin
  Range := VectorZero;
  MaxTime := 0;
  For I:=0 To Pred(Count) Do
  Begin
    Range.X := FloatMax(Range.X, Abs(KeyFrames[I].Value.X));
    Range.Y := FloatMax(Range.Y, Abs(KeyFrames[I].Value.Y));
    Range.Z := FloatMax(Range.Z, Abs(KeyFrames[I].Value.Z));
    MaxTime := FloatMax(MaxTime, KeyFrames[I].Time);
  End;

  If Range.Length<=0 Then
    Count := 0;

  Dest.Write(@Count, 4);
  Dest.Write(@Range, SizeOf(Vector3D));
  Dest.Write(@MaxTime, 4);

  For I:=0 To Pred(Count) Do
  Begin
    Time := Trunc(SafeDiv(KeyFrames[I].Time, MaxTime)*TimeCompressionLimit);
    Dest.Write(@Time, 2);

    SX := Trunc(SafeDiv(KeyFrames[I].Value.X,Range.X)*CompressionLimit1);
    SY := Trunc(SafeDiv(KeyFrames[I].Value.Y,Range.Y)*CompressionLimit1);
    SZ := Trunc(SafeDiv(KeyFrames[I].Value.Z,Range.Z)*CompressionLimit1);
    Dest.Write(@SX, 1);
    Dest.Write(@SY, 1);
    Dest.Write(@SZ, 1);

    {PX := Trunc((KeyFrames[I].Value.X/Range.X)*CompressionLimit2);
    PY := Trunc((KeyFrames[I].Value.Y/Range.Y)*CompressionLimit2);
    PZ := Trunc((KeyFrames[I].Value.Z/Range.Z)*CompressionLimit2);
    Dest.Write(@PX, 2);
    Dest.Write(@PY, 2);
    Dest.Write(@PZ, 2);}

//  Dest.Write(@KeyFrames[I].Value, SizeOf(Vector3D));
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

Procedure BoneAnimation.Load(Source:Stream);
Begin
  Source.ReadString(Name);
  Positions.Load(Source);
  Rotations.Load(Source);
  Scales.Load(Source);
End;

Procedure BoneAnimation.Save(Dest:Stream);
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
 	  Block.Translation := Positions.KeyFrames[Key].Value
  Else
    Block.Translation := VectorZero;

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

   	Q1 := QuaternionRotation(Rotations.Keyframes[LastKey].Value);
	  Q2 := QuaternionRotation(Rotations.Keyframes[Key].Value);
  	Block.Rotation := QuaternionSlerp(Q1,Q2, Fraction);
  End Else
  If (Key=0) And (Rotations.Count>0) Then
  Begin
    Block.Rotation := QuaternionRotation(Rotations.Keyframes[Key].Value);
  End Else
    Block.Rotation := QuaternionRotation(VectorZero);

    //TODO
  Block.Scale := VectorOne;
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
Class Function Animation.GetManager: Pointer;
Begin
  Result := AnimationManager.Instance;
End;

Function Animation.Load(Source:Stream):Boolean;
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

  Source.Read(@FPS, 4);
  Source.Read(@Loop, 1);
  Source.Read(@LoopPoint, 4);
  Source.Read(@Speed, 4);
  Source.ReadString(Next);

  Source.Read(@Count, 4);
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

Procedure Animation.Save(Dest:Stream);
Var
  Header:FileHeader;
  I:Integer;
Begin
  Header := 'ANIM';
  Dest.Write(@Header, 4);

  Dest.Write(@FPS, 4);
  Dest.Write(@Loop, 1);
  Dest.Write(@LoopPoint, 4);
  Dest.Write(@Speed, 4);
  Dest.WriteString(Next);

  Dest.Write(@_BoneCount, 4);
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


End.
