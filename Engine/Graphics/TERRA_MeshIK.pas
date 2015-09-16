Unit TERRA_MeshIK;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Vector2D, TERRA_Vector3D, TERRA_Quaternion, TERRA_Math,
  TERRA_MeshSkeleton, TERRA_IKBone2D, TERRA_MeshAnimation, TERRA_MeshAnimationNodes;

Type
  IKChainAxis = (
    IKAxis_X,
    IKAxis_Y,
    IKAxis_Z
  );

  MeshIKChain = Class(TERRAObject)
    Protected
      _Root:MeshBone;
      _Effector:MeshBone;

      _IKChain:IKBone2D;

      _Rotations:Array Of Vector3D;

      Procedure ResetChain();
      Procedure SetChainPositions(Axis:IKChainAxis);

      Function SolveForAxis(Const TargetEffectorPosition:Vector3D; Axis:IKChainAxis):Boolean;

    Public
      Constructor Create(Effector:MeshBone; ChainSize:Integer);
      Procedure Release(); Override;

      Function Solve(TargetEffectorPosition:Vector3D):Boolean;

      Property Root:MeshBone Read _Root;
      Property Effector:MeshBone Read _Effector;
      Property IKChain:IKBone2D Read _IKChain;
  End;

  MeshIKController = Class(AnimationObject)
    Protected
      _Target:MeshSkeleton;

      _Chains:Array Of MeshIKChain;
      _ChainCount:Integer;

    Public
      Constructor Create(Target:MeshSkeleton);

      Function AddChain(const BoneName:TERRAString; ChainSize:Integer):MeshIKChain;

      Function GetTransform(BoneIndex:Integer):AnimationTransformBlock; Override;
  End;

Implementation

{ MeshIKChain }
Constructor MeshIKChain.Create(Effector:MeshBone; ChainSize:Integer);
Var
  Bone:MeshBone;
  Child:IKBone2D;
Begin
  _IKChain := IKBone2D.Create(ChainSize);
  _Effector := Effector;

  SetLength(_Rotations, ChainSize);

  Bone := _Effector;
  While (ChainSize>0) Do
  Begin
    Dec(ChainSize);

    Child := _IKChain.GetChainBone(ChainSize);
    Child.Name := 'IK_'+Bone.Name;

    If ChainSize>0 Then
      Bone := Bone.Parent;
  End;

  _Root := Bone;

  Self.IKChain.GetChainBone(0).SetLimitsInDegrees(-90, 70);

  Self.SetChainPositions(IKAxis_Z);
End;


Procedure MeshIKChain.Release;
Begin
  ReleaseObject(_IKChain);
End;

Procedure MeshIKChain.ResetChain;
Var
  Index, ChainSize:Integer;
  Child:IKBone2D;
  Bone:MeshBone;
Begin
  Index := 0;
  ChainSize := Self._IKChain.ChainSize;
  Bone := _Effector;
  While (ChainSize>0) Do
  Begin
    Dec(ChainSize);

    Child := _IKChain.GetChainBone(ChainSize);
    Child.Rotation := 0.0;

    Bone := Bone.Parent;
  End;
End;


{ Copies 3d bones positions to IK bone chain }
Procedure MeshIKChain.SetChainPositions(Axis:IKChainAxis);
Var
  Index, ChainSize:Integer;
  Pos:Vector3D;
  Child:IKBone2D;
  Bone:MeshBone;

  RootPos:Vector3D;
Begin
  RootPos := _Root.GetAbsolutePosition();

  Index := 0;
  ChainSize := Self._IKChain.ChainSize;
  Bone := _Effector;
  While (ChainSize>0) Do
  Begin
    Dec(ChainSize);

    Pos := Bone.GetAbsolutePosition();
    Pos.Subtract(RootPos);
    Child := _IKChain.GetChainBone(ChainSize);

    Case Axis Of
    IKAxis_X: Child.Position := VectorCreate2D(Pos.Z, Pos.Y);
    IKAxis_Y: Child.Position := VectorCreate2D(Pos.X, Pos.Z);
    Else
      Child.Position := VectorCreate2D(Pos.X, Pos.Y);
    End;

    Bone := Bone.Parent;
  End;
End;

Function MeshIKChain.SolveForAxis(Const TargetEffectorPosition:Vector3D; Axis:IKChainAxis):Boolean;
Var
  EndPos:Vector2D;
  I:Integer;
  Rot:Single;
Begin
  Self.SetChainPositions(Axis);

  Case Axis Of
    IKAxis_X: EndPos := VectorCreate2D(TargetEffectorPosition.Z, TargetEffectorPosition.Y);
    IKAxis_Y: EndPos := VectorCreate2D(TargetEffectorPosition.X, TargetEffectorPosition.Z);
    Else
      EndPos := VectorCreate2D(TargetEffectorPosition.X, TargetEffectorPosition.Y);
  End;

  Result := _IKChain.Solve(EndPos, True, True);

  For I:=0 To Pred(Self.IKChain.ChainSize) Do
  Begin
    Rot := Self.IKChain.GetChainBone(I).Rotation;
    Case Axis Of
      IKAxis_X: _Rotations[I].X := Rot;
      IKAxis_Y: _Rotations[I].Y := Rot;
      Else
        _Rotations[I].Z := Rot;
    End;
  End;
End;


Function MeshIKChain.Solve(TargetEffectorPosition: Vector3D):Boolean;
Var
  RootPos:Vector3D;
Begin
  Self.ResetChain();

  RootPos := _Root.GetAbsolutePosition();
  TargetEffectorPosition.Subtract(RootPos);

  If (Abs(TargetEffectorPosition.Z)<1.0) Then
  Begin
    Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_Z);
  End Else
  If (Abs(TargetEffectorPosition.Y)<1.0) Then
  Begin
    Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_Y);
  End Else
  If (Abs(TargetEffectorPosition.X)<1.0) Then
  Begin
    Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_X);
  End Else
  Begin
    Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_Z);
    Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_Y);
  End;
  //Result := Self.SolveForAxis(TargetEffectorPosition, IKAxis_X);
End;

{ MeshIKController }
Function MeshIKController.AddChain(const BoneName: TERRAString; ChainSize: Integer):MeshIKChain;
Var
  Bone:MeshBone;
Begin
  Bone := _Target.GetBoneByName(BoneName);
  If Bone = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := MeshIKChain.Create(Bone, ChainSize);

  Inc(_ChainCount);
  SetLength(_Chains, _ChainCount);
  _Chains[Pred(_ChainCount)] := Result;
End;

Constructor MeshIKController.Create(Target: MeshSkeleton);
Begin
  _Target := Target;
End;

Function MeshIKController.GetTransform(BoneIndex: Integer): AnimationTransformBlock;
Var
  TargetBone:MeshBone;
  ChainBone:IKBone2D;
  I, ChainIndex:Integer;
Begin
  Result.Translation := VectorZero;
  Result.Scale := VectorConstant(1.0);
  Result.Rotation := QuaternionZero;

  For I:=0 To Pred(_ChainCount) Do
  Begin
    TargetBone := _Chains[I].Effector;
    ChainIndex := _Chains[I].IKChain.ChainSize;
    While (ChainIndex>0) Do
    Begin
      Dec(ChainIndex);

      If (TargetBone.Index = BoneIndex) Then
      Begin
        ChainBone := _Chains[I].IKChain.GetChainBone(ChainIndex);
        Result.Rotation := QuaternionRotation(_Chains[I]._Rotations[ChainIndex]);
        Exit;
      End;

      TargetBone := TargetBone.Parent;
    End;
  End;

End;

End.
