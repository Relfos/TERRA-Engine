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
Uses TERRA_Utils, TERRA_IO, TERRA_Resource, TERRA_Vector3D, TERRA_Math,
  TERRA_Matrix4x4, TERRA_Vector2D, TERRA_Color, TERRA_Vector4D, TERRA_ResourceManager;

Const
  animationTargetDiffuse  = 1;
  animationTargetSpecular = 2;
  animationTargetGlow     = 3;

  TimeCompressionLimit = 20000;
  CompressionLimit1   = 127;
  CompressionLimit2   = 20000;

  DefaultCrossfadeDuration = 500;

Type
  MeshSkeleton = Class;

  AnimationCallback = Procedure (P:Pointer); Cdecl;

  MeshBone = Class(TERRAObject)
    Name:AnsiString;
    Index:Integer;
    Parent:MeshBone;
    Owner:MeshSkeleton;

    Color:Color;
    Selected:Boolean;

    StartPosition:Vector3D;
    {$IFNDEF NO_ROTS}
    StartRotation:Vector3D;

    AbsoluteRotation:Vector4D;
    RelativeRotation:Vector4D;
    {$ENDIF}

    AbsoluteMatrix:Matrix4x4;
    RelativeMatrix:Matrix4x4;

    Ready:Boolean;

    Procedure Init;

    Destructor Destroy; Override;

    Function Read(Source:Stream):AnsiString;
    Procedure Write(Dest:Stream);

    Function GetLength():Single;
  End;

  MeshSkeleton = Class(TERRAObject)
    Protected
      _BoneList:Array Of MeshBone;
      _BoneCount:Integer;

    Public
      Name:String;
      BindPose:Array Of Matrix4x4;

      Destructor Destroy; Override;

      Procedure Init();

      Procedure Clone(Other:MeshSkeleton);

      Procedure Read(Source:Stream);
      Procedure Write(Dest:Stream);

      Function AddBone:MeshBone;
      Function GetBone(Index:Integer):MeshBone; Overload;
      Function GetBone(Name:AnsiString):MeshBone; Overload;

      Function GetBoneLength(Index:Integer):Single;

      Procedure Render(Const Transform:Matrix4x4; Instance:Pointer);

      Property BoneCount:Integer Read _BoneCount;
  End;

  BoneAnimation = Class;

  VectorKeyFrame=Record
    Time:Single;
    Value:Vector3D;
  End;

  VectorKeyframeArray = Class
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

  PAnimationTransformBlock = ^AnimationTransformBlock;
  AnimationTransformBlock = Object
    Translation:Vector3D;
    Scale:Vector3D;
    Rotation:Vector4D;
  End;

  BoneAnimation = Class(TERRAObject)
      Name:AnsiString;
      ID:Integer;
      Owner:Animation;

      Positions:VectorKeyframeArray;
      Rotations:VectorKeyframeArray;
      Scales:VectorKeyframeArray;

      Constructor Create(ID:Integer; Owner:Animation);
      Destructor Destroy; Override;

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

      Function GetLength():Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Procedure GetTransform(Time:Single; Var Block:AnimationTransformBlock);

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

      Next:AnsiString;

      Procedure Clone(Other:Animation);

      Function Load(Source:Stream):Boolean; Override;
      Procedure Save(Dest:Stream); Overload;
      Procedure Save(FileName:AnsiString); Overload;

      Class Function GetManager:Pointer; Override;

      Function GetLength:Single;

      Procedure Crop(Time:Single);
      Procedure CloseLoop();

      Function AddBone(Name:AnsiString):BoneAnimation;
      Function GetBone(Index:Integer):BoneAnimation;
      Function GetBoneIndex(Name:AnsiString):Integer;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Procedure OnContextLost; Override;

      Property BoneCount:Integer Read _BoneCount;
      Property Length:Single Read GetLength;
  End;

  AnimationState = Class;

  AnimationObject = Class(TERRAObject)
      _Name:AnsiString;

      Procedure UpdateAnimation(); Virtual;

      Function HasAnimation(MyAnimation:Animation):Boolean; Virtual; Abstract;
      Function GetActiveAnimation:Animation; Virtual; Abstract;

      Function HasBone(Bone:Integer):Boolean; Virtual; Abstract;
      Function GetTransform(Bone:Integer):AnimationTransformBlock; Virtual; Abstract;

      Function Finished:Boolean; Virtual; Abstract;

      Function Collapse:AnimationObject; Virtual;

      Destructor Destroy; Override;
  End;

  AnimationMixer = Class(AnimationObject)
      _A, _B:AnimationObject;
      Alpha:Single;

      Destructor Destroy; Override;

      Function Finished:Boolean; Override;

      Function GetActiveAnimation:Animation; Override;

      Function HasAnimation(MyAnimation:Animation):Boolean; Override;
      Function HasBone(Bone:Integer):Boolean; Override;
      Function GetTransform(Bone:Integer):AnimationTransformBlock; Override;
  End;

  AnimationCrossfader = Class(AnimationMixer)
    Protected
      _StartTime:Cardinal;
      _Duration:Cardinal;

    Public
      Constructor Create(Src,Dest:AnimationObject; Duration:Cardinal);

      Function Collapse:AnimationObject; Override;
      
      Function Finished:Boolean; Override;
      Function GetTransform(Bone:Integer):AnimationTransformBlock; Override;
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

    Public
      Constructor Create(Owner:AnimationState; MyAnimation:Animation);
      Procedure Init(MyAnimation:Animation);

      Procedure UpdateAnimation(); Override;

      Function HasAnimation(MyAnimation:Animation):Boolean; Override;
      Function GetActiveAnimation:Animation; Override;

      Function HasBone(Bone:Integer):Boolean; Override;
      Function GetTransform(Bone:Integer):AnimationTransformBlock; Override;

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
    _ID:Integer;
    _Block:AnimationTransformBlock;
    _Ready:Boolean;
    _Parent:AnimationBoneState;
    _AbsoluteMatrix:Matrix4x4;
    _FrameMatrix:Matrix4x4;
    _Bone:MeshBone;

    Procedure UpdateTransform;

    Destructor Destroy; Override;
  End;

  AnimationProcessor = Procedure (State:AnimationState);

  AnimationState = Class(TERRAObject)
    Protected
      _Name:AnsiString;
      _Next:AnsiString;

      _Skeleton:MeshSkeleton;

      _BoneCount:Integer;
      _BoneStates:Array Of AnimationBoneState;

      _LastTime:Cardinal;
      _Root:AnimationObject;

      _LastAnimation:AnsiString;

      _QueueAnimation:Animation;
      _QueueDuration:Integer;

      _Speed:Single;

      _UserData:Pointer;
      _Callback:AnimationCallback;
      _CallbackFrame:Integer;

      _UpdateID:Cardinal;

      Procedure AddBone(Bone:MeshBone);

      Procedure CollapseNode(Var Node:AnimationObject);

      Procedure UpdateAnimationName(MyAnimation:Animation);

    Public
      Processor:AnimationProcessor;
      Transforms:Array Of Matrix4x4;

      Constructor Create(Name:AnsiString; MySkeleton:MeshSkeleton);
      Destructor Destroy; Override;

      Procedure Update;

      Procedure SetSpeed(Value:Single);

      Procedure SetRoot(Node:AnimationObject);
      Procedure SetCallback(Callback:AnimationCallback; UserData:Pointer = Nil; CallbackFrame:Integer=-1);

      Function GetAbsoluteMatrix(Index:Integer):Matrix4x4;
      Function GetRelativeMatrix(Index:Integer):Matrix4x4;

      Function Play(Name:AnsiString; Rescale:Single=0):Boolean; Overload;
      Function Play(MyAnimation:Animation; Rescale:Single=0):Boolean; Overload;

      Function Crossfade(Name:AnsiString; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;
      Function Crossfade(MyAnimation:Animation; Duration:Cardinal = DefaultCrossfadeDuration):Boolean; Overload;

      Property Speed:Single Read _Speed Write SetSpeed;
      Property Root:AnimationObject Read _Root;

      Property LastAnimation:AnsiString Read _LastAnimation;
  End;

  AnimationManager = Class(ResourceManager)
    Public
      Destructor Destroy; Override;
      Class Function Instance:AnimationManager;

      Function GetAnimation(Name:AnsiString; ValidateError:Boolean = True):Animation;
   End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Application, TERRA_OS, TERRA_FileManager,  TERRA_Mesh,
  TERRA_GraphicsManager, TERRA_FileIO, TERRA_FileUtils;

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


Destructor AnimationManager.Destroy;
Begin
  Inherited;
  _AnimationManager := Nil;
End;

Function AnimationManager.GetAnimation(Name:AnsiString; ValidateError:Boolean):Animation;
Var
  S:AnsiString;
Begin
  Result := Nil;
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
    Exit;

  If (Name[1]='@') Then
    Name := Copy(Name, 2, MaxInt);

  Result := Animation(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Name+'.anim');
    If S<>'' Then
    Begin
      Result := Animation.Create(S);
      Result.Priority := 70;
      Result.InBackground := False;
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      RaiseError('Could not find animation. ['+Name +']');
  End;
End;

{ MeshBone }
Destructor MeshBone.Destroy;
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
  RelativeRotation := Vector4DRotation(StartRotation);
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
    AbsoluteRotation := Vector4DZero;
    {$ENDIF}
  End Else									// not the root node
	Begin
		// m_final := parent's m_final * m_rel (matrix concatenation)
    AbsoluteMatrix := Matrix4x4Multiply4x3(Parent.AbsoluteMatrix, RelativeMatrix);
    {$IFNDEF NO_ROTS}
    AbsoluteRotation := Vector4DMultiply(Parent.AbsoluteRotation, RelativeRotation);
    {$ENDIF}
	End;

  Ready := True;
End;

Function MeshBone.Read(Source:Stream):AnsiString;
Var
  I:Integer;
Begin
  Source.ReadString(Name);
  Source.ReadString(Result);
  Parent := Nil;
  Result := UpStr(Result);

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
  Parents:Array Of AnsiString;
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

  SetLength(BindPose, Succ(_BoneCount));
  BindPose[0] := Matrix4x4Identity;
  For I:=0 To Pred(_BoneCount) Do
    BindPose[Succ(I)] := _BoneList[I].AbsoluteMatrix;

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

Destructor MeshSkeleton.Destroy;
Var
  I:Integer;
Begin
  _BoneCount := Length(_BoneList);

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Destroy;

  SetLength(_BoneList, 0);
End;

Function MeshSkeleton.GetBone(Name:AnsiString): MeshBone;
Var
  I:Integer;
Begin
  Name := Upstr(Name);
  For I:=0 To Pred(_BoneCount) Do
  If (Name = UpStr(_BoneList[I].Name)) Then
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

  For I:=0 To Pred(_BoneCount) Do
    _BoneList[I].Destroy;

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

  SetLength(BindPose, Succ(_BoneCount));
  For I:=0 To _BoneCount Do
    BindPose[I] := Other.BindPose[I];
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

 { If (LowStr(_Owner.Owner.Name)='monster084_idle') Then
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

Destructor BoneAnimation.Destroy;
Begin
  Positions.Destroy();
  Rotations.Destroy();
  Scales.Destroy();
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
  Name := UpStr(Name);

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

Procedure BoneAnimation.GetTransform(Time:Single; Var Block:AnimationTransformBlock);

Var
  Key, LastKey:Integer;
  DeltaTime : Single;
	Fraction : Single;

  Q1,Q2:Vector4D;
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

   	Q1 := Vector4DRotation(Rotations.Keyframes[LastKey].Value);
	  Q2 := Vector4DRotation(Rotations.Keyframes[Key].Value);
  	Block.Rotation := Vector4DSlerp(Q1,Q2, Fraction);
  End Else
  If (Key=0) And (Rotations.Count>0) Then
  Begin
    Block.Rotation := Vector4DRotation(Rotations.Keyframes[Key].Value);
  End Else
    Block.Rotation := Vector4DRotation(VectorZero);

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
  S:AnsiString;
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

  _Status := rsReady;
  Result := True;
End;

Procedure Animation.Save(FileName:AnsiString);
Var
  Stream:FileStream;
Begin
  Stream := FileStream.Create(FileName);
  Save(Stream);
  Stream.Destroy;
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

Function Animation.GetBoneIndex(Name:AnsiString):Integer;
Var
  I:Integer;
Begin
  Name := UpStr(Name);
  For I:=0 To Pred(Self._BoneCount) Do
  If (Self._Bones[I].Name = Name) Then
  Begin
    Result:=I;
    Exit;
  End;

  Result := -1;
End;

Function Animation.AddBone(Name:AnsiString):BoneAnimation;
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
    _Bones[I].Destroy();

  SetLength(_Bones,0);
  _BoneCount := 0;

  _Status := rsUnloaded;
  Result := True;
End;

Function Animation.Update: Boolean;
Begin
  Inherited Update();
  Result := True;
End;

Procedure Animation.OnContextLost;
Begin
  _ContextID := Application.Instance.ContextID;
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

{ AnimationState }
Constructor AnimationState.Create(Name:AnsiString; MySkeleton:MeshSkeleton);
Var
  I:Integer;
  Parent:AnsiString;
  B:MeshBone;
Begin
  _Speed := 1;
  Processor := Nil;

  _Skeleton := MySkeleton;

  I := Pos('.', Name);
  If (I>0) Then
    Name := Copy(Name, Succ(I), MaxInt);
  _Name := Name;

  _BoneCount := 0;
  _LastTime := Application.Instance.GetElapsedTime();

  For I:=0 To Pred(MySkeleton.BoneCount) Do
    Self.AddBone(MySkeleton.GetBone(I));
End;

Procedure AnimationState.AddBone(Bone:MeshBone);
Var
  I:Integer;
Begin
  Inc(_BoneCount);
  SetLength(_BoneStates, _BoneCount);
  SetLength(Transforms, Succ(_BoneCount));

  _BoneStates[Pred(_BoneCount)] := AnimationBoneState.Create;
  _BoneStates[Pred(_BoneCount)]._Bone := Bone;
  _BoneStates[Pred(_BoneCount)]._Owner := Self;
  _BoneStates[Pred(_BoneCount)]._ID := Pred(_BoneCount);
  _BoneStates[Pred(_BoneCount)]._Parent := Nil;

  If Assigned(Bone.Parent) Then
  Begin
    For I:=0 To Pred(_BoneCount) Do
    If (_BoneStates[I]._Bone = Bone.Parent) Then
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

  Self.CollapseNode(_Root);

  If Not Assigned(_Root) Then
  Begin
    {For I:=0 To Pred(_BoneCount) Do
      Transforms[Succ(I)] := _Skeleton.BindPose[Succ(I)];}

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

  {For I:=1 To _BoneCount Do
    Transforms[I] := MatrixMultiply4x3(Transforms[I], _Skeleton.BindPose[I]);}
End;

Procedure AnimationState.SetRoot(Node: AnimationObject);
Begin
  If Assigned(_Root) Then
    _Root.Destroy;

  _Root := Node;
End;

Destructor AnimationState.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneCount) Do
    _BoneStates[I].Destroy;
    
  If Assigned(_Root) Then
    _Root.Destroy;
End;

Function AnimationState.Play(Name:AnsiString; Rescale:Single):Boolean;
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

Function AnimationState.Crossfade(Name:AnsiString; Duration:Cardinal):Boolean;
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
    Self.Play(MyAnimation);
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
    Node.Destroy;
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

Procedure AnimationState.SetCallback(Callback: AnimationCallback; UserData: Pointer; CallbackFrame:Integer);
Begin
  Self._Callback := Callback;
  Self._CallbackFrame := CallbackFrame;
  Self._UserData := UserData;
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
    Temp.Destroy();
End;

Procedure AnimationState.UpdateAnimationName(MyAnimation: Animation);
Begin
  _LastAnimation := MyAnimation.Name;
  ReplaceText(_Name + '_', '', _LastAnimation);
  _LastAnimation := LowStr(_LastAnimation);
End;

{ AnimationBoneState }
Destructor AnimationBoneState.Destroy;
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
  _FrameMatrix := Matrix4x4Multiply4x3(Matrix4x4Translation(_Block.Translation), Vector4DMatrix4x4(_Block.Rotation));

	// Add the animation state to the rest position
  _FrameMatrix := Matrix4x4Multiply4x3(_Bone.RelativeMatrix, _FrameMatrix);

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

Function AnimationMixer.GetTransform(Bone:Integer):AnimationTransformBlock;
Var
  SA, SB:AnimationTransformBlock;
  Beta:Single;
Begin
  SA := _A.GetTransform(Bone);
  SB := _B.GetTransform(Bone);

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

    Result.Rotation := Vector4DSlerp(SA.Rotation, SB.Rotation, Alpha);

    Result.Scale.X := SB.Scale.X * Alpha + SA.Scale.X * Beta;
    Result.Scale.Y := SB.Scale.Y * Alpha + SA.Scale.Y * Beta;
    Result.Scale.Z := SB.Scale.Z * Alpha + SA.Scale.Z * Beta;
  End;
End;

Destructor AnimationMixer.Destroy;
Begin
  If Assigned(_A) Then
    _A.Destroy;
  If Assigned(_B) Then
    _B.Destroy;
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

Function AnimationCrossfader.GetTransform(Bone:Integer):AnimationTransformBlock;
Var
  SA, SB:AnimationTransformBlock;
  Beta:Single;
Begin
  Alpha := Application.Instance.GetElapsedTime() - _StartTime;
  Alpha := Alpha / _Duration;
  If (Alpha>=1) Then
    Alpha := 1;

  Result := Inherited GetTransform(Bone);
End;

Function AnimationCrossfader.Collapse: AnimationObject;
Begin
  If Not Self.Finished Then
  Begin
    Result := Self;
    Exit;
  End;

  If (_A<>_B) Then
    _A.Destroy();

  Result := _B;

  // we no longer control B, and A was destroyed
  _A := Nil;
  _B := Nil;
End;

{ AnimationNode }
Constructor AnimationNode.Create(Owner: AnimationState; MyAnimation:Animation);
Var
  I:Integer;
Begin
  _Name := MyAnimation.Name;

  If (MyAnimation.Status<>rsReady) Then
  Begin
    {$IFDEF DEBUG_ANIMATIONS}Log(logDebug,'Animation', 'Prefetching animation '+MyAnimation._Name);{$ENDIF}
    MyAnimation.Prefetch();
  End;

  _Owner := Owner;
  {$IFDEF DEBUG_ANIMATIONS}Log(logDebug,'Animation', 'Initializng animation '+MyAnimation._Name);{$ENDIF}
  Init(MyAnimation);
End;

Function AnimationNode.HasBone(Bone: Integer): Boolean;
Begin
  Result := (_IndexList[Bone]>=0);
End;

Procedure AnimationNode.Init(MyAnimation:Animation);
Var
  I:Integer;
Begin
  _Animation := MyAnimation;
  If (_Animation = Nil) Then
    Exit;

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
    _IndexList[I] := _Animation.GetBoneIndex(_Owner._BoneStates[I]._Bone.Name);
End;

Function AnimationNode.GetTransform(Bone:Integer): AnimationTransformBlock;
Begin
  _Animation._Time := GetTime;

  Self.UpdateAnimation();

  If (_Animation.IsReady) And (Bone<Length(_IndexList)) And (_IndexList[Bone]>=0)
  And (_IndexList[Bone]<_Animation.BoneCount) Then
    _Animation._Bones[_IndexList[Bone]].GetTransform(_Time, Result)
  Else
  Begin
    Result.Translation := VectorZero;
    Result.Rotation := Vector4DZero;
    Result.Scale := VectorOne;
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
  _FrameStart := GetTime - Ms;
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

Procedure AnimationNode.UpdateAnimation();
Var
  I:Integer;
  S:AnsiString;
  MyAnimation:TERRA_MeshAnimation.Animation;
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

  If (Assigned(Self._Owner._Callback)) And (_CurrentFrame>=Self._Owner._CallbackFrame) And (Self._Owner._CallbackFrame>0) Then
  Begin
    Self._Owner._Callback(Self._Owner._UserData);
    Self._Owner._Callback := Nil;
  End;

  If (_Time> Len) And (_SetFrame<0) Then
  Begin
    If (Assigned(Self._Owner._Callback)) And (Self._Owner._CallbackFrame<0) Then
    Begin
      Self._Owner._Callback(Self._Owner._UserData);
      Self._Owner._Callback := Nil;
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

Destructor AnimationObject.Destroy;
Begin
// do nothing
End;

Procedure AnimationObject.UpdateAnimation;
Begin
// do nothing
End;

Initialization
  RegisterResourceClass(Animation);
End.
