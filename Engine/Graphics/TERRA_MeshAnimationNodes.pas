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
 * TERRA_MeshAnimationNodes
 * Implements the mesh animation node system
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
      Procedure UpdateAnimation(); Virtual;

      Function HasAnimation(MyAnimation:Animation):Boolean; Virtual;
      Function GetActiveAnimation:Animation; Virtual;

      Function HasTransform(Bone:Integer):Boolean; Virtual; Abstract;
      Function GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean; Virtual; Abstract;

      Function Finished:Boolean; Virtual;

      Function Collapse:AnimationObject; Virtual;

      Procedure Release; Override;
  End;

  AnimationMixer = Class(AnimationObject)
    Protected
      _A, _B:AnimationObject;

    Public
      Alpha:Single;

      Constructor Create(Src,Dest:AnimationObject);

      Procedure Release; Override;

      Function Finished:Boolean; Override;

      Function GetActiveAnimation:Animation; Override;

      Function HasAnimation(MyAnimation:Animation):Boolean; Override;


      Function HasTransform(Bone:Integer):Boolean; Override;
      Function GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean; Override;
  End;

  AnimationCrossfader = Class(AnimationMixer)
    Protected
      _StartTime:Cardinal;
      _Duration:Cardinal;

    Public
      Constructor Create(Src,Dest:AnimationObject; Const Duration:Cardinal);

      Procedure Reset(Const Duration:Cardinal);

      Function Collapse:AnimationObject; Override;

      Function Finished:Boolean; Override;

      Function GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean; Override;
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

      Function HasTransform(Bone:Integer):Boolean; Override;
      Function GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean; Override;

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
    Protected
      _Owner:AnimationState;
      _ID:Integer;
      _Ready:Boolean;

      _Parent:AnimationBoneState;

      _RelativeMatrix:Matrix4x4;
      _OffsetMatrix:Matrix4x4;
      _FinalTransform:Matrix4x4;

      _SkinningIndex:Integer;
      _Kind:MeshBoneKind;

      Procedure UpdateTransform(Node:AnimationObject);

    Public
      Procedure Release; Override;

      Property ID:Integer Read _ID;
      Property Parent:AnimationBoneState Read _Parent;
  End;

  AnimationProcessor = Class(TERRAObject)
    Protected
      _Owner:AnimationState;

    Public
      Procedure GetTransform(Target:AnimationBoneState; Node:AnimationObject; Out Transform:Matrix4x4); Virtual;
  End;

  AnimationState = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Next:TERRAString;

      _BoneCount:Integer;
      _BoneStates:Array Of AnimationBoneState;

      _MaxActiveBones:Integer;

      _LastTime:Cardinal;
      _Root:AnimationObject;

      _LastAnimation:TERRAString;

      _Speed:Single;

      _CallbackTarget:TERRAObject;
      _Callback:AnimationCallback;
      _CallbackFrame:Integer;

      _UpdateID:Cardinal;

      _Processor:AnimationProcessor;

      _Transforms:Array Of Matrix4x4;

      Procedure AddBone(Bone:MeshBone);

      Procedure CollapseNode(Var Node:AnimationObject);

      Procedure UpdateAnimationName(MyAnimation:Animation);

      Procedure SetProcessor(Value:AnimationProcessor);

    Public

      //AllowRootMovement:Boolean;

      Constructor Create(TargetSkeleton:MeshSkeleton);
      Procedure Release; Override;

      Procedure Update;

      Procedure SetSpeed(Value:Single);

      Procedure SetRoot(Node:AnimationObject);
      Procedure SetCallback(Callback:AnimationCallback; CallbackTarget:TERRAObject = Nil; CallbackFrame:Integer=-1);

      Function GetAbsoluteMatrix(Index:Integer):Matrix4x4;
      Function GetBonePoseMatrix(Index:Integer):Matrix4x4;

      Function GetBoneByName(Const Name:TERRAString):AnimationBoneState;

      Function Find(Name:TERRAString):Animation;
      Function Play(MyAnimation:Animation; Rescale:Single=0):Boolean;

      Function Crossfade(Name:TERRAString; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;
      Function Crossfade(MyAnimation:Animation; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;

      Property Speed:Single Read _Speed Write SetSpeed;
      Property Root:AnimationObject Read _Root Write SetRoot;

      Property LastAnimation:TERRAString Read _LastAnimation;

      Property MaxActiveBones:Integer Read _MaxActiveBones;

      Property Processor:AnimationProcessor Read _Processor Write SetProcessor;
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

  Self.SetProcessor(AnimationProcessor.Create());

  _Name := TargetSkeleton.Name;
  I := Pos('.', _Name);
  If (I>0) Then
    _Name := Copy(_Name, Succ(I), MaxInt);

  _BoneCount := 0;
  _LastTime := Application.Instance.GetElapsedTime();

  For I:=0 To Pred(TargetSkeleton.BoneCount) Do
    Self.AddBone(TargetSkeleton.GetBoneByIndex(I));

  _MaxActiveBones := 0;
  For I:=0 To Pred(TargetSkeleton.BoneCount) Do
  If (_BoneStates[I]._Kind <> meshBone_Dummy) And (_BoneStates[I]._SkinningIndex >= _MaxActiveBones) Then
    _MaxActiveBones := Succ(_BoneStates[I]._SkinningIndex);

  SetLength(_Transforms, _MaxActiveBones);
End;

Procedure AnimationState.AddBone(Bone:MeshBone);
Var
  I:Integer;
Begin
  Inc(_BoneCount);
  SetLength(_BoneStates, _BoneCount);

  _BoneStates[Pred(_BoneCount)] := AnimationBoneState.Create;
  _BoneStates[Pred(_BoneCount)]._ObjectName := Bone.Name;

  _BoneStates[Pred(_BoneCount)]._OffsetMatrix := Bone.OffsetMatrix;
  _BoneStates[Pred(_BoneCount)]._RelativeMatrix := Bone.RelativeMatrix;
  _BoneStates[Pred(_BoneCount)]._Kind := Bone.Kind;
  _BoneStates[Pred(_BoneCount)]._SkinningIndex := Bone.SkinningIndex;


  _BoneStates[Pred(_BoneCount)]._Owner := Self;
  _BoneStates[Pred(_BoneCount)]._ID := Pred(_BoneCount);
  _BoneStates[Pred(_BoneCount)]._Parent := Nil;

  If Assigned(Bone.Parent) Then
  Begin
    For I:=0 To Pred(_BoneCount) Do
    If (StringEquals(_BoneStates[I].Name, Bone.Parent.Name)) Then
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
  BoneState:AnimationBoneState;
  M:Matrix4x4;
Begin
  If (Length(_Transforms)<=0) Then
    Exit;
    
  _Transforms[0] := Matrix4x4Identity;

  If (_Next<>'') Then
  Begin
    Self.Play(Find(_Next));
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

  If Assigned(_Root) Then
  Begin
    Time := Application.Instance.GetElapsedTime();
    Delta := Time - _LastTime;

    _LastTime := Time;

    If Length(_BoneStates)<_BoneCount Then
      Exit;
  End;

  // Get all bones tranformations
  For I:=0 To Pred(_BoneCount) Do
  Begin
    _BoneStates[I]._Ready := False;
  End;

  For I:=0 To Pred(_BoneCount) Do
    _BoneStates[I].UpdateTransform(_Root);

  For I:=0 To Pred(_BoneCount) Do
  Begin
    BoneState := _BoneStates[I];
    If BoneState._SkinningIndex<=0 Then
      Continue;
      
    _Transforms[BoneState._SkinningIndex] := Matrix4x4Multiply4x3(BoneState._FinalTransform, BoneState._OffsetMatrix);
  End;

//  FloatToString(Transforms[1].V[1]);
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
  ReleaseObject(_Processor);
End;

Function AnimationState.Find(Name:TERRAString):Animation;
Var
  MyAnimation: Animation;
Begin
  MyAnimation := AnimationManager.Instance.GetAnimation(_Name + '@' + Name, False);
  If Assigned(MyAnimation) Then
    Result := MyAnimation
  Else
  Begin
    Log(logWarning, 'Animation', 'Animation not found: '+Name);
    Result := Nil;
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
  Result := _BoneStates[Index]._FinalTransform;
End;

Function AnimationState.GetBonePoseMatrix(Index: Integer): Matrix4x4;
Begin
  If (Index<0) Or (Index>=Length(_Transforms)) Then
    Result := Matrix4x4Identity
  Else
    Result := _Transforms[Index];
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

Function AnimationState.GetBoneByName(Const Name:TERRAString):AnimationBoneState;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._BoneCount) Do
  If (StringEquals(Name, _BoneStates[I].Name)) Then
  Begin
    Result := _BoneStates[I];
    Exit;
  End;

  Result := Nil;
End;

Procedure AnimationState.SetProcessor(Value:AnimationProcessor);
Begin
  ReleaseObject(_Processor);

  _Processor := Value;

  If Assigned(Value) Then
    _Processor._Owner := Self;
End;

{ AnimationBoneState }
Procedure AnimationBoneState.Release;
Begin
  // do nothing
End;

Procedure AnimationBoneState.UpdateTransform(Node:AnimationObject);
Var
  I:Integer;
  Block:AnimationTransformBlock;
  LocalTransform:Matrix4x4;
Begin
  If _Ready Then
    Exit;

  // Now we know the position and rotation for this animation frame.
	// Let's calculate the transformation matrix (m_final) for this bone...

	// Create a transformation matrix from the position and rotation
	// m_frame: additional transformation for this frame of the animation


  _Owner.Processor.GetTransform(Self, Node, LocalTransform);

  // m_final := parent's m_final * m_rel (matrix concatenation)
  If (Assigned(_Parent)) Then
  Begin
    _Parent.UpdateTransform(Node);
    _FinalTransform := Matrix4x4Multiply4x3(_Parent._FinalTransform, LocalTransform);
  End Else
    _FinalTransform := LocalTransform;

  (*Temp := _Owner.Processor.PostTransform(_Owner, Self, _Block);
  Temp.MoveTransformOrigin(_FrameGlobalTransform.Transform(VectorZero));
  _FrameAbsoluteMatrix := Matrix4x4Multiply4x3(Temp, _FrameAbsoluteMatrix);*)

  _Ready := True;
End;

{ AnimationMixer }
Constructor AnimationMixer.Create(Src, Dest: AnimationObject);
Begin
  Self.Name := Src.Name + ' -> ' + Dest.Name;
  _A := Src;
  _B := Dest;
End;

Function AnimationMixer.HasTransform(Bone:Integer):Boolean;
Begin
  Result := (_A.HasTransform(Bone)) Or (_B.HasTransform(Bone));
End;

Function AnimationMixer.GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean;
Var
  SA, SB:AnimationTransformBlock;
  Beta:Single;
Begin
  _A.GetTransform(BoneIndex, SA);
  _B.GetTransform(BoneIndex, SB);

  Beta := 1.0 - Alpha;

  If (Alpha<=0) Then
  Begin
    Block.Translation := SA.Translation;
    Block.Rotation := SA.Rotation;
    Block.Scale:= SA.Scale;
  End Else
  If (Alpha>=1) Then
  Begin
    Block.Translation := SB.Translation;
    Block.Rotation := SB.Rotation;
    Block.Scale:= SB.Scale;
  End Else
  Begin
    Block.Translation.X := SB.Translation.X * Alpha + SA.Translation.X * Beta;
    Block.Translation.Y := SB.Translation.Y * Alpha + SA.Translation.Y * Beta;
    Block.Translation.Z := SB.Translation.Z * Alpha + SA.Translation.Z * Beta;

    Block.Rotation := QuaternionSlerp(SA.Rotation, SB.Rotation, Alpha);

    Block.Scale.X := SB.Scale.X * Alpha + SA.Scale.X * Beta;
    Block.Scale.Y := SB.Scale.Y * Alpha + SA.Scale.Y * Beta;
    Block.Scale.Z := SB.Scale.Z * Alpha + SA.Scale.Z * Beta;
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
Constructor AnimationCrossfader.Create(Src,Dest:AnimationObject;  Const Duration: Cardinal);
Begin
  Inherited Create(Src, Dest);

  Reset(Duration);
End;

Procedure AnimationCrossfader.Reset(Const Duration:Cardinal);
Begin
  _StartTime := Application.Instance.GetElapsedTime();
  _Duration := Duration;
End;

Function AnimationCrossfader.Finished: Boolean;
Var
  Alpha:Single;
Begin
  Alpha := Application.Instance.GetElapsedTime() - _StartTime;
  Alpha := Alpha / _Duration;
  Result := (Alpha>=1);
End;

Function AnimationCrossfader.GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean;
Begin
  Alpha := Application.Instance.GetElapsedTime() - _StartTime;
  Alpha := Alpha / _Duration;
  If (Alpha>=1) Then
    Alpha := 1;

  Result := Inherited GetTransform(BoneIndex, Block);
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

  Self.Name := MyAnimation.Name;

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
    _IndexList[I] := _Animation.GetBoneIndex(_Owner._BoneStates[I].Name);
End;

Function AnimationNode.HasTransform(Bone: Integer): Boolean;
Begin
  Result := (_IndexList[Bone] >= 0);
End;

Function AnimationNode.GetTransform(BoneIndex:Integer; Var Block:AnimationTransformBlock):Boolean;
Var
  Bone:BoneAnimation;
Begin
  Result := False;
  _Animation.Touch();

  Self.UpdateAnimation();

  If (_Animation.IsReady) Then
  Begin
    Bone := _Animation.GetBone(_IndexList[BoneIndex]);

    If Assigned(Bone) Then
    Begin
      Bone.GetTransform(_Time, Block);
      Result := True;
      Exit;
    End;
  End;
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

Function AnimationObject.Finished: Boolean;
Begin
  Result := False;
End;

Function AnimationObject.GetActiveAnimation: Animation;
Begin
  Result := Nil;
End;

Function AnimationObject.HasAnimation(MyAnimation: Animation): Boolean;
Begin
  Result := False;
End;

Procedure AnimationObject.Release;
Begin
// do nothing
End;

Procedure AnimationObject.UpdateAnimation;
Begin
// do nothing
End;

{ AnimationProcessor }
Procedure AnimationProcessor.GetTransform(Target:AnimationBoneState; Node:AnimationObject; Out Transform:Matrix4x4);
Var
  Block:AnimationTransformBlock;
  Offset:Vector3D;
begin
  If (Assigned(Node)) And (Node.HasTransform(Target.ID)) Then
  Begin
    Node.GetTransform(Target.ID, Block);
    Transform := Matrix4x4Multiply4x3(Matrix4x4Translation(Block.Translation), QuaternionMatrix4x4(Block.Rotation));

    If (Target._Kind = meshBone_Root) {And (Not _Owner.AllowRootMovement)} Then
    Begin
      Offset := Transform.GetTranslation();
      Offset.X := 0;
      Offset.Y := 0;
      Transform.SetTranslation(Offset);
    End;
  End Else
    Transform := Target._RelativeMatrix;
End;

End.

