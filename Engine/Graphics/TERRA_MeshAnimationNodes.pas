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
Unit TERRA_MeshAnimationNodes;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Stream, TERRA_Resource, TERRA_Vector3D, TERRA_Math,
  TERRA_Matrix4x4, TERRA_Vector2D, TERRA_Color, TERRA_Quaternion, TERRA_ResourceManager,
  TERRA_MeshAnimation, TERRA_MeshSkeleton;

Const
  animationTargetDiffuse  = 1;
  animationTargetSpecular = 2;
  animationTargetGlow     = 3;

  DefaultCrossfadeDuration = 500;

Type
  AnimationCallback = Function (Target:TERRAObject):Boolean Of Object;
  AnimationState = Class;

  AnimationObject = Class(TERRAObject)
      _Name:TERRAString;

      Procedure UpdateAnimation(); Virtual;

      Function HasAnimation(MyAnimation:Animation):Boolean; Virtual; Abstract;
      Function GetActiveAnimation:Animation; Virtual; Abstract;

      Function HasBone(Bone:Integer):Boolean; Virtual; Abstract;
      Function GetTransform(BoneIndex:Integer):AnimationTransformBlock; Virtual; Abstract;

      Function Finished:Boolean; Virtual; Abstract;

      Function Collapse:AnimationObject; Virtual;

      Procedure Release; Override;
  End;

  AnimationMixer = Class(AnimationObject)
      _A, _B:AnimationObject;
      Alpha:Single;

      Procedure Release; Override;

      Function Finished:Boolean; Override;

      Function GetActiveAnimation:Animation; Override;

      Function HasAnimation(MyAnimation:Animation):Boolean; Override;
      Function HasBone(Bone:Integer):Boolean; Override;
      Function GetTransform(BoneIndex:Integer):AnimationTransformBlock; Override;
  End;

  AnimationCrossfader = Class(AnimationMixer)
    Protected
      _StartTime:Cardinal;
      _Duration:Cardinal;

    Public
      Constructor Create(Src,Dest:AnimationObject; Duration:Cardinal);

      Function Collapse:AnimationObject; Override;
      
      Function Finished:Boolean; Override;
      Function GetTransform(BoneIndex:Integer):AnimationTransformBlock; Override;
  End;

  AnimationNode = Class(AnimationObject)
    Protected
      _Animation:Animation;
      _FrameStart:Cardinal;
      _Speed:Single;

      _CurrentFrame:Integer;

      _Owner:AnimationState;
      _IndexList:Array Of Integer;

      _SetFrame:Integer;

      _Time:Single;
      _UpdateID:Cardinal;

      _Hash:Cardinal;

      Function RunCallback():Boolean;

    Public
      Constructor Create(Owner:AnimationState; MyAnimation:Animation);
//      Procedure Init(Owner:AnimationState; MyAnimation:Animation);

      Procedure UpdateAnimation(); Override;

      Function HasAnimation(MyAnimation:Animation):Boolean; Override;
      Function GetActiveAnimation:Animation; Override;

      Function HasBone(Bone:Integer):Boolean; Override;
      Function GetTransform(BoneIndex:Integer):AnimationTransformBlock; Override;

      Procedure SetCurrentFrame(Frame:Integer);

      Procedure PauseOnFrame(Frame:Integer);

      Function GetFrameTime(Frame:Integer):Single;

      Procedure Skip(Ms:Cardinal);

      Procedure SetSpeed(const Value: Single);

      Function Finished:Boolean; Override;

      Property Animation:TERRA_MeshAnimation.Animation Read _Animation;
      Property Speed:Single Read _Speed Write SetSpeed;

      Property CurrentFrame:Integer Read _CurrentFrame;

  End;

  AnimationBoneState = Class(TERRAObject)
    _Owner:AnimationState;
    _BoneName:TERRAString;
    _ID:Integer;
    _Block:AnimationTransformBlock;
    _Ready:Boolean;
    _Parent:AnimationBoneState;
    _AbsoluteMatrix:Matrix4x4;
    _RelativeMatrix:Matrix4x4;
    _FrameMatrix:Matrix4x4;

    _BoneSpaceMatrix:Matrix4x4;

    Procedure UpdateTransform;

    Procedure Release; Override;
  End;

  AnimationProcessor = Procedure (State:AnimationState);

  AnimationState = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Next:TERRAString;

      _BoneCount:Integer;
      _BoneStates:Array Of AnimationBoneState;

      _LastTime:Cardinal;
      _Root:AnimationObject;

      _LastAnimation:TERRAString;

      _Speed:Single;

      _CallbackTarget:TERRAObject;
      _Callback:AnimationCallback;
      _CallbackFrame:Integer;

      _UpdateID:Cardinal;

      Procedure AddBone(Bone:MeshBone);

      Procedure CollapseNode(Var Node:AnimationObject);

      Procedure UpdateAnimationName(MyAnimation:Animation);

    Public
      Processor:AnimationProcessor;
      Transforms:Array Of Matrix4x4;

      Constructor Create(TargetSkeleton:MeshSkeleton);
      Procedure Release; Override;

      Procedure Update;

      Procedure SetSpeed(Value:Single);

      Procedure SetRoot(Node:AnimationObject);
      Procedure SetCallback(Callback:AnimationCallback; CallbackTarget:TERRAObject = Nil; CallbackFrame:Integer=-1);

      Function GetAbsoluteMatrix(Index:Integer):Matrix4x4;
      Function GetRelativeMatrix(Index:Integer):Matrix4x4;

      Function Play(Name:TERRAString; Rescale:Single=0):Boolean; Overload;
      Function Play(MyAnimation:Animation; Rescale:Single=0):Boolean; Overload;

      Function Crossfade(Name:TERRAString; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;
      Function Crossfade(MyAnimation:Animation; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;

      Property Speed:Single Read _Speed Write SetSpeed;
      Property Root:AnimationObject Read _Root;

      Property LastAnimation:TERRAString Read _LastAnimation;
  End;


Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_GraphicsManager, TERRA_FileStream, TERRA_FileUtils;

{ AnimationState }
Constructor AnimationState.Create(TargetSkeleton:MeshSkeleton);
Var
  I:Integer;
Begin
  _Speed := 1;
  Processor := Nil;

  _Name := TargetSkeleton.Name;
  I := Pos('.', _Name);
  If (I>0) Then
    _Name := Copy(_Name, Succ(I), MaxInt);

  _BoneCount := 0;
  _LastTime := Application.Instance.GetElapsedTime();

  For I:=0 To Pred(TargetSkeleton.BoneCount) Do
    Self.AddBone(TargetSkeleton.GetBone(I));
End;

Procedure AnimationState.AddBone(Bone:MeshBone);
Var
  I:Integer;
Begin
  Inc(_BoneCount);
  SetLength(_BoneStates, _BoneCount);
  SetLength(Transforms, Succ(_BoneCount));

  Bone.Init();  

  _BoneStates[Pred(_BoneCount)] := AnimationBoneState.Create;
  _BoneStates[Pred(_BoneCount)]._BoneName := Bone.Name;
  _BoneStates[Pred(_BoneCount)]._RelativeMatrix := Bone.RelativeMatrix;
  _BoneStates[Pred(_BoneCount)]._BoneSpaceMatrix:= Bone.AbsoluteMatrix;
  _BoneStates[Pred(_BoneCount)]._Owner := Self;
  _BoneStates[Pred(_BoneCount)]._ID := Pred(_BoneCount);
  _BoneStates[Pred(_BoneCount)]._Parent := Nil;

  If Assigned(Bone.Parent) Then
  Begin
    For I:=0 To Pred(_BoneCount) Do
    If (StringEquals(_BoneStates[I]._BoneName, Bone.Parent.Name)) Then
    Begin
      _BoneStates[Pred(_BoneCount)]._Parent := _BoneStates[I];
      Break;
    End;
  End;
End;

Procedure AnimationState.Update;
Var
  I:Integer;
  Time, Delta:Cardinal;
Begin
  If (Length(Transforms)<=0) Then
  Begin
    SetLength(Transforms, Succ(_BoneCount));
  End;

  Transforms[0] := Matrix4x4Identity;

  If (_Next<>'') Then
  Begin
    Self.Play(_Next);
    _Next := '';
  End;


  _UpdateID := GraphicsManager.Instance.FrameID;

{  If (Assigned(_QueueAnimation)) And ((Not Assigned(_Root)) Or (_Root Is AnimationCrossfader) And (AnimationCrossfader(_Root).Alpha >=1)) Then
  Begin
    If Self.Crossfade(_QueueAnimation, _QueueDuration) Then
      _QueueAnimation := Nil;
  End;}

  If Assigned(_Root) Then
    Self.CollapseNode(_Root);

  If Not Assigned(_Root) Then
  Begin
    For I:=1 To _BoneCount Do
      Transforms[I] := Matrix4x4Identity;

    Exit;
  End;

  Time := Application.Instance.GetElapsedTime();
  Delta := Time - _LastTime;

  {If (Delta<10) Then
    Exit;
   }

  _LastTime := Time;

  If Length(_BoneStates)<_BoneCount Then
    Exit;

  // Get all bones tranformations
  For I:=0 To Pred(_BoneCount) Do
  If Assigned(_Root) Then
  Begin
    _BoneStates[I]._Block := _Root.GetTransform(I);
    _BoneStates[I]._Ready := False;
  End;

  For I:=0 To Pred(_BoneCount) Do
    _BoneStates[I].UpdateTransform();

  For I:=0 To Pred(_BoneCount) Do
    Transforms[Succ(I)] := _BoneStates[I]._AbsoluteMatrix;

  If Assigned(Processor) Then
    Processor(Self);

  For I:=1 To _BoneCount Do
  Begin
    Transforms[I] := Matrix4x4Multiply4x3(Transforms[I], Self._BoneStates[Pred(I)]._BoneSpaceMatrix);
  End;
End;

Procedure AnimationState.SetRoot(Node: AnimationObject);
Begin
  ReleaseObject(_Root);
  _Root := Node;
End;

Procedure AnimationState.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneCount) Do
    ReleaseObject(_BoneStates[I]);

  _BoneCount := 0;

  ReleaseObject(_Root);
End;

Function AnimationState.Play(Name:TERRAString; Rescale:Single):Boolean;
Var
  MyAnimation: Animation;
Begin
  MyAnimation := AnimationManager.Instance.GetAnimation(_Name + '_' + Name, False);
  If Assigned(MyAnimation) Then
    Result := Self.Play(MyAnimation, Rescale)
  Else
  Begin
    Log(logWarning, 'Animation', 'Animation not found: '+Name);
    Result := False;
  End;
End;

Function AnimationState.Play(MyAnimation: Animation; Rescale:Single=0):Boolean;
Var
  Dur:Single;
Begin
  If (MyAnimation = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Rescale>0) Then
  Begin
    MyAnimation.Prefetch();
    If (MyAnimation.FPS>0) Then
    Begin
      Dur := MyAnimation.Length * 1000;
      Self.SetSpeed(Dur/Rescale);
    End;
  End;

  {$IFDEF DEBUG_ANIMATIONS_ANIMATIONS} Log(logDebug,'Animation', 'Playing animation '+MyAnimation._Name);{$ENDIF}
  If (Assigned(Root)) And (Root.HasAnimation(MyAnimation)) Then
  Begin
    Result := True;
    Exit;
  End;

  {$IFDEF DEBUG_ANIMATIONS_ANIMATIONS}Log(logDebug,'Animation', 'Setting root');{$ENDIF}
  Self.SetRoot(AnimationNode.Create(Self, MyAnimation));

  UpdateAnimationName(MyAnimation);

  Result := True;
End;

Function AnimationState.Crossfade(Name:TERRAString; Duration:Cardinal):Boolean;
Var
  MyAnimation: Animation;
Begin
  MyAnimation := AnimationManager.Instance.GetAnimation(_Name + '_' + Name, False);
  If Assigned(MyAnimation) Then
    Result := Self.Crossfade(MyAnimation, Duration)
  Else
    Result := False;
End;

Function AnimationState.Crossfade(MyAnimation: Animation; Duration:Cardinal):Boolean;
Var
  Node:AnimationCrossfader;
Begin
  If (MyAnimation = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  If (_Root = Nil) Then
  Begin
    Result := Play(MyAnimation);
    Exit;
  End;

  Self.CollapseNode(_Root);

  If Not Assigned(_Root) Then
  Begin
    Result := Self.Play(MyAnimation);
    Exit;
  End;

  If (_Root.HasAnimation(MyAnimation)) Then
  Begin
    Result := False;
    Exit;
  End;

  {If (_Root Is AnimationCrossfader) And (AnimationCrossfader(_Root).Alpha <1) Then
  Begin
    If (_QueueAnimation=Nil) Then
    Begin
      _QueueAnimation := MyAnimation;
      _QueueDuration := Duration;
    End;
    Result := False;
    Exit;
  End;}

  If (_Root Is AnimationCrossFader) Then
  Begin
    Node := AnimationCrossfader(_Root);
    _Root := Node._B;
    Node._B := Nil;

    ReleaseObject(Node);
  End;

  UpdateAnimationName(MyAnimation);

  Result := True;
  _Root := AnimationCrossfader.Create(_Root, AnimationNode.Create(Self, MyAnimation), Duration);
End;

Function AnimationState.GetAbsoluteMatrix(Index: Integer): Matrix4x4;
Begin
  Result := Transforms[Index+1];
End;

Function AnimationState.GetRelativeMatrix(Index: Integer): Matrix4x4;
Begin
  Result := _BoneStates[Index]._FrameMatrix;
End;

Procedure AnimationState.SetSpeed(Value: Single);
Begin
  _Speed := Value;
End;

Procedure AnimationState.SetCallback(Callback:AnimationCallback; CallbackTarget:TERRAObject; CallbackFrame:Integer);
Begin
  Self._Callback := Callback;
  Self._CallbackFrame := CallbackFrame;
  Self._CallbackTarget:= CallbackTarget;
End;

Procedure AnimationState.CollapseNode(Var Node:AnimationObject);
Var
  Temp:AnimationObject;
Begin
  If (Node = Nil) Then
    Exit;

  Temp := Node;
  Node := Node.Collapse();
  If Temp<>Node Then
    ReleaseObject(Temp);
End;

Procedure AnimationState.UpdateAnimationName(MyAnimation: Animation);
Begin
  _LastAnimation := MyAnimation.Name;
  StringReplaceText(_Name + '_', '', _LastAnimation);
  _LastAnimation := StringLower(_LastAnimation);
End;

{ AnimationBoneState }
Procedure AnimationBoneState.Release;
Begin
  // do nothing
End;

Procedure AnimationBoneState.UpdateTransform;
Begin
  If (_Ready) Then
    Exit;

  If (Assigned(_Parent)) And (Not _Parent._Ready) Then
    _Parent.UpdateTransform;

  // Now we know the position and rotation for this animation frame.
	// Let's calculate the transformation matrix (m_final) for this bone...

	// Create a transformation matrix from the position and rotation
	// m_frame: additional transformation for this frame of the animation
  _FrameMatrix := Matrix4x4Multiply4x3(Matrix4x4Translation(_Block.Translation), QuaternionMatrix4x4(_Block.Rotation));

	// Add the animation state to the rest position
  _FrameMatrix := Matrix4x4Multiply4x3(_RelativeMatrix, _FrameMatrix);

	If (_Parent = nil ) Then					// this is the root node
  Begin
    _AbsoluteMatrix := _FrameMatrix;
  End Else									// not the root node
	Begin
		// m_final := parent's m_final * m_rel (matrix concatenation)
    _AbsoluteMatrix := Matrix4x4Multiply4x3(_Parent._AbsoluteMatrix, _FrameMatrix);
	End;

  _Ready := True;
End;

{ AnimationMixer }
Function AnimationMixer.HasBone(Bone:Integer):Boolean;
Begin
  Result := (_A.HasBone(Bone)) Or (_B.HasBone(Bone));
End;

Function AnimationMixer.GetTransform(BoneIndex:Integer):AnimationTransformBlock;
Var
  SA, SB:AnimationTransformBlock;
  Beta:Single;
Begin
  SA := _A.GetTransform(BoneIndex);
  SB := _B.GetTransform(BoneIndex);

  Beta := 1.0 - Alpha;

  If (Alpha<=0) Then
  Begin
    Result.Translation := SA.Translation;
    Result.Rotation := SA.Rotation;
    Result.Scale:= SA.Scale;
  End Else
  If (Alpha>=1) Then
  Begin
    Result.Translation := SB.Translation;
    Result.Rotation := SB.Rotation;
    Result.Scale:= SB.Scale;
  End Else
  Begin
    Result.Translation.X := SB.Translation.X * Alpha + SA.Translation.X * Beta;
    Result.Translation.Y := SB.Translation.Y * Alpha + SA.Translation.Y * Beta;
    Result.Translation.Z := SB.Translation.Z * Alpha + SA.Translation.Z * Beta;

    Result.Rotation := QuaternionSlerp(SA.Rotation, SB.Rotation, Alpha);

    Result.Scale.X := SB.Scale.X * Alpha + SA.Scale.X * Beta;
    Result.Scale.Y := SB.Scale.Y * Alpha + SA.Scale.Y * Beta;
    Result.Scale.Z := SB.Scale.Z * Alpha + SA.Scale.Z * Beta;
  End;
End;

Procedure AnimationMixer.Release;
Begin
  ReleaseObject(_A);
  ReleaseObject(_B);
End;

Function AnimationMixer.HasAnimation(MyAnimation: Animation): Boolean;
Var
  Beta:Single;
Begin
  Beta := 1 - Alpha;
  Result := (_B.HasAnimation(MyAnimation)) And (Self.Alpha>0) Or (_A.HasAnimation(MyAnimation)) And (Beta>0);
End;

Function AnimationMixer.Finished: Boolean;
Begin
  Result := False;
End;

Function AnimationMixer.GetActiveAnimation: Animation;
Begin
  Result := _A.GetActiveAnimation();
End;

{ AnimationCrossfader }
Constructor AnimationCrossfader.Create(Src,Dest:AnimationObject;  Duration: Cardinal);
Begin
  _Name := Src._Name + ' -> ' + Dest._Name;
  _StartTime := Application.Instance.GetElapsedTime();
  _Duration := Duration;
  _A := Src;
  _B := Dest;
End;

Function AnimationCrossfader.Finished: Boolean;
Var
  Alpha:Single;
Begin
  Alpha := Application.Instance.GetElapsedTime() - _StartTime;
  Alpha := Alpha / _Duration;
  Result := (Alpha>=1);
End;

Function AnimationCrossfader.GetTransform(BoneIndex:Integer):AnimationTransformBlock;
Begin
  Alpha := Application.Instance.GetElapsedTime() - _StartTime;
  Alpha := Alpha / _Duration;
  If (Alpha>=1) Then
    Alpha := 1;

  Result := Inherited GetTransform(BoneIndex);
End;

Function AnimationCrossfader.Collapse: AnimationObject;
Begin
  If Not Self.Finished Then
  Begin
    Result := Self;
    Exit;
  End;

  If (_A<>_B) Then
    ReleaseObject(_A);

  Result := _B;

  // we no longer control B, and A was destroyed
  _A := Nil;
  _B := Nil;
End;

{ AnimationNode }
Constructor AnimationNode.Create(Owner:AnimationState; MyAnimation:Animation);
Var
  I:Integer;
Begin
  _Animation := MyAnimation;
  If (_Animation = Nil) Then
    Exit;

  _Name := MyAnimation.Name;

  If (MyAnimation.Status<>rsReady) Then
  Begin
    {$IFDEF DEBUG_ANIMATIONS}Log(logDebug,'Animation', 'Prefetching animation '+MyAnimation._Name);{$ENDIF}
    MyAnimation.Prefetch();
  End;

  {$IFDEF DEBUG_ANIMATIONS}Log(logDebug,'Animation', 'Initializng animation '+MyAnimation._Name);{$ENDIF}
  _Owner := Owner;


  {$IFDEF DEBUG_ANIMATIONS}
  Log(logDebug,'App','Animation stats for '+MyAnimation.Name);
  Log(logDebug,'App','Speed: '+FloatToString(MyAnimation.Speed));
  Log(logDebug,'App','FPS: '+FloatToString(MyAnimation.FPS));
  Log(logDebug,'App','Duration: '+FloatToString(MyAnimation.Duration));
  {$ENDIF}

  _FrameStart := Application.Instance.GetElapsedTime();
  SetSpeed(1.0);
  _SetFrame := -1;

  SetLength(_IndexList, _Owner._BoneCount);
  For I:=0 To Pred(_Owner._BoneCount) Do
    _IndexList[I] := _Animation.GetBoneIndex(_Owner._BoneStates[I]._BoneName);
End;

Function AnimationNode.HasBone(Bone: Integer): Boolean;
Begin
  Result := (_IndexList[Bone]>=0);
End;

Function AnimationNode.GetTransform(BoneIndex:Integer): AnimationTransformBlock;
Var
  Bone:BoneAnimation;
Begin
  _Animation.Touch();

  Self.UpdateAnimation();

  If (_Animation.IsReady) Then
  Begin
    Bone := _Animation.GetBone(_IndexList[BoneIndex]);

    If Assigned(Bone) Then
    Begin
      Bone.GetTransform(_Time, Result);
      Exit;
    End;
  End;

  Result.Translation := VectorZero;
  Result.Rotation := QuaternionZero;
  Result.Scale := VectorOne;
End;

Function AnimationNode.HasAnimation(MyAnimation: Animation): Boolean;
Begin
  Result := (Self._Animation = MyAnimation);
End;

Function AnimationNode.Finished: Boolean;
Var
  Time:Single;
Begin
  Time := ((Application.Instance.GetElapsedTime() - _FrameStart) * _Speed) / 1000.0;
  Result := (Time>_Animation.Length) And (Not _Animation.Loop);
End;

Procedure AnimationNode.PauseOnFrame(Frame:Integer);
Begin
  _SetFrame := Frame;
End;

Procedure AnimationNode.Skip(Ms: Cardinal);
Begin
  _FrameStart := Application.GetTime - Ms;
End;

Function AnimationNode.GetActiveAnimation: Animation;
Begin
  Result := Self._Animation;
End;

Function AnimationNode.GetFrameTime(Frame: Integer): Single;
Begin
  If (_Animation=Nil) Then
    Result := 0.0
  Else
    Result := FrameToTime(Frame, _Animation.FPS);
End;

Procedure AnimationNode.SetSpeed(const Value: Single);
Begin
  _Speed := _Owner.Speed * _Animation.Speed * Value;
End;

Procedure AnimationNode.SetCurrentFrame(Frame: Integer);
Var
  T:Single;
Begin
  If _Animation = Nil Then
    Exit;

  T := FrameToTime(Frame, _Animation.FPS);
  _FrameStart := (Application.Instance.GetElapsedTime()) - Trunc(T*1000);
End;

Function AnimationNode.RunCallback():Boolean;
Var
  Temp:AnimationCallback;
Begin
  Temp := Self._Owner._Callback;
  Result := Self._Owner._Callback(Self._Owner._CallbackTarget);

  If (Pointer(@Self._Owner._Callback) = Pointer(@Temp)) Then
    Self._Owner._Callback := Nil;
End;

Procedure AnimationNode.UpdateAnimation();
Var
  Len:Single;
Begin
  If _Owner._UpdateID = Self._UpdateID Then
    Exit;

  Self._UpdateID := _Owner._UpdateID;

  Len := _Animation.Length;
  If (_SetFrame>=0) Then
  Begin
    _Time := Self.GetFrameTime(_SetFrame);
  End Else
    _Time := ((Application.Instance.GetElapsedTime() - _FrameStart) * _Speed) / 1000.0;

  If (Len<=0) Then
    _CurrentFrame := 0
  Else
    _CurrentFrame := TimeToFrame(_Time, _Animation.FPS);

  If (Assigned(Self._Owner._Callback)) And ((_CurrentFrame>=Self._Owner._CallbackFrame) And (Self._Owner._CallbackFrame>0)) Then
  Begin
    If Not RunCallback() Then
      Exit;
  End;

  If (_Time> Len) And (_SetFrame<0) Then
  Begin
    If (Assigned(Self._Owner._Callback)) And (Self._Owner._CallbackFrame<0) Then
    Begin
      If Not RunCallback() Then
        Exit;
    End;

    //_Animation.Loop := false;
    If (_Animation.Loop) Then
    Begin
      If (Len<=0) Then
        _Time := 0
      Else
        _Time := (_Animation.LoopPoint * Len);

      _FrameStart := (Application.Instance.GetElapsedTime()) - Trunc(_Time*1000);
    End Else
    Begin
      If (_Animation.Next<>'') Then
      Begin
        {MyAnimation := AnimationManager.Instance.GetAnimation(_Animation.Next, False);
        If Not Assigned(MyAnimation) Then
        Begin
          S := _Animation._Name;
          I := PosRev('_', S);
          S := Copy(S, 1, I);
          MyAnimation := AnimationManager.Instance.GetAnimation(S+_Animation.Next, False);
        End;

        If Assigned(MyAnimation) Then
          Self.Init(MyAnimation);}

        Self._Owner._Next := _Animation.Next;
      End;

      _Time := Len;
      _CurrentFrame := TimeToFrame(Len, _Animation.FPS);
    End;
  End;
End;

{ AnimationObject }
Function AnimationObject.Collapse:AnimationObject;
Begin
  Result := Self;
End;

Procedure AnimationObject.Release;
Begin
// do nothing
End;

Procedure AnimationObject.UpdateAnimation;
Begin
// do nothing
End;

End.
