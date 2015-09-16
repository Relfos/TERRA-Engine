{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_DemoApplication, TERRA_Utils, TERRA_Object, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_String,
  TERRA_Vector2D, TERRA_Mesh, TERRA_MeshSkeleton, TERRA_MeshAnimation, TERRA_MeshAnimationNodes,
  TERRA_FileManager, TERRA_Color, TERRA_DebugDraw, TERRA_Resource, TERRA_Ray, TERRA_Plane,
  TERRA_ScreenFX, TERRA_Math, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_Quaternion, TERRA_InputManager,
  TERRA_FileStream, TERRA_IKBone2D, TERRA_MeshIK;

Type
  MyDemo = Class(DemoApplication)
    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;

      Procedure OnIdle; Override;

      Procedure OnRender(V:TERRAViewport); Override;

			Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseUp(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseMove(X,Y:Integer); Override;
  End;


Var
  TargetInstance:MeshInstance;
  Dragging:Boolean;
  TargetPos:Vector3D;
  TargetColor:Color;

  IKController:MeshIKController;
  LeftArm, RightArm:MeshIKChain;
  TargetChain:MeshIKChain;

{ MyDemo }
Procedure MyDemo.OnCreate;
Var
  MyMesh:TERRAMesh;
  Skeleton:MeshSkeleton;
  Bone:MeshBone;
Begin
  Inherited;

  Self.Scene.MainViewport.Camera.SetPosition(VectorCreate(0, 9, -15));
  Self.Scene.MainViewport.Camera.SetView(VectorCreate(0, -0.25, 1));

  MyMesh := MeshManager.Instance.GetMesh('ninja');
  If Assigned(MyMesh) Then
  Begin
    TargetInstance :=MeshInstance.Create(MyMesh);
    TargetInstance.SetPosition(VectorCreate(0, 0, 0));
  End Else
    TargetInstance := Nil;

  Skeleton := TargetInstance.Geometry.Skeleton;
  Skeleton.NormalizeJoints();

  IKController := MeshIKController.Create(Skeleton);
  LeftArm := IKController.AddChain('lwrist', 3);
  RightArm := IKController.AddChain('rwrist', 3);

  TargetChain := LeftArm;

  TargetInstance.Animation.Root := IKController;
End;

Procedure MyDemo.OnDestroy;
Begin
  Inherited;

  ReleaseObject(TargetInstance);
  //ReleaseObject(Chain);
End;

Procedure MyDemo.OnIdle;
Begin
  Inherited;

  If InputManager.Instance.Keys.WasPressed(keyV) Then
    TargetChain := LeftArm;

  If InputManager.Instance.Keys.WasPressed(keyB) Then
    TargetChain := RightArm;
End;

(*Procedure DrawIKBone3D(V:TERRAViewport; Chain:MeshIKChain; Bone:IKBone3D);
Var
  A,B:Vector3D;
  Root:Vector3D;
Begin
  If Assigned(Bone.Child) Then
  Begin
    A := Bone.GetAbsoluteMatrix().Transform(VectorZero);
    B := Bone.Child.GetAbsoluteMatrix().Transform(VectorZero);

    Root := Chain.Root.GetAbsolutePosition();

    A.Add(Root);
    B.Add(Root);

    DrawLine3D(V, A, B, ColorBlue, 2);

    DrawIKBone3D(V, Chain, Bone.Child);
  End;
End;*)

Procedure DrawIKBone2D(V:TERRAViewport; Chain:MeshIKChain; Bone:IKBone2D);
Var
  PA,PB:Vector2D;
  A,B:Vector3D;
  Root:Vector3D;
Begin
  If Assigned(Bone.Child) Then
  Begin
    PA := Bone.GetAbsoluteMatrix().Transform(VectorZero2D);
    PB := Bone.Child.GetAbsoluteMatrix().Transform(VectorZero2D);

    Root := Chain.Root.GetAbsolutePosition();

    A := VectorCreate(PA.X, PA.Y, 0);
    B := VectorCreate(PB.X, PB.Y, 0);

    A.Add(Root);
    B.Add(Root);

    DrawLine3D(V, A, B, ColorBlue, 2);

    DrawIKBone2D(V, Chain, Bone.Child);
  End;
End;

Procedure MyDemo.OnRender(V:TERRAViewport);
Var
  Bone:MeshBone;
Begin
  If (V<>Self._Scene.MainViewport) Then
    Exit;

  DrawSkeleton(V, TargetInstance.Geometry.Skeleton,  TargetInstance.Animation, TargetInstance.Transform, ColorRed, 4.0);
  GraphicsManager.Instance.AddRenderable(V, TargetInstance);

  DrawIKBone2D(V, TargetChain, TargetChain.IKChain);

  DrawSphere(V, TargetPos, 20.0, TargetColor, 3);
End;

Procedure MyDemo.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  Dragging := True;

  If Button = keyMouseRight Then
  Begin
    Self.OnMouseMove(X, Y);
    Self.OnMouseUp(X, Y, Button);
    Exit;
  End;

End;

Procedure MyDemo.OnMouseMove(X, Y: Integer);
Var
  R:Ray;
  P:Plane;
  T:Single;
Begin
  If Not Dragging Then
    Exit;

  R := Self._Scene.MainViewport.GetPickRay(X, Y);
  P := PlaneCreate(VectorZero, VectorCreate(0, 0, -1));

  If R.Intersect(P, T) Then
  Begin
    TargetPos := R.IntersectionPoint(T);

    If TargetChain.Solve(TargetPos) Then
      TargetColor := ColorWhite
    Else
      TargetColor := ColorRed;
  End;

End;

Procedure MyDemo.OnMouseUp(X, Y: Integer; Button: Word);
Begin
  Dragging := False;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}

End.

