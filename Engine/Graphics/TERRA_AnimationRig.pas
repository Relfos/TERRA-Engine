Unit TERRA_AnimationRig;

{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_Resource, TERRA_Utils, TERRA_String, TERRA_Mesh, TERRA_MeshSkeleton,
  TERRA_Vector3D, TERRA_Matrix4x4;

Type
  RigBoneType = (
    rigBone_Root = 0,
    rigBone_Pelvis = 1,
    rigBone_Spin = 2,
    rigBone_Hip = 3,
    rigBone_UpperLeg = 4,
    rigBone_LowerLeg = 5,
    rigBone_Foot = 6,
    rigBone_Clavicle = 7,
    rigBone_UpperArm = 8,
    rigBone_LowerArm = 9,
    rigBone_Hand = 10,
    rigBone_Neck = 11,
    rigBone_Head = 12
  );

  AnimationRig = Class(TERRAObject)
    Protected
      _Skeleton:MeshSkeleton;

      Procedure GenerateSkeleton(); Virtual; Abstract;
    Public
  //    Function GenerateMesh():TERRAMesh; Virtual; Abstract;

      Property Skeleton:MeshSkeleton Read _Skeleton;
  End;

  BipedAnimationRig = Class(AnimationRig)
    Protected

      Procedure GenerateSkeleton(); Override;

    Public
//      Function GenerateMesh():TERRAMesh; Override;
  End;

Implementation
Uses TERRA_Solids;

{ BipedAnimationRig }
{Function BipedAnimationRig.GenerateMesh: TERRAMesh;
Const
  HeadSize = 1;
  LegScale = HeadSize * 0.4;
  LegSize = HeadSize * 2;
  LegOfs = 0.3;

  FootSize = HeadSize;
Var
  I:Integer;
  Temp:SolidMesh;
Begin
  Result := TERRAMesh.Create(rtDynamic, '');


  Temp := CubeMesh.Create(2);
  Temp.Transform(Matrix4x4Transform(VectorCreate(0, 00, 0), VectorZero, VectorCreate(HeadSize, HeadSize * 0.5, HeadSize * 0.5)));
  MergeSolidToMesh(Temp, Result);

(*  Temp := SphereMesh.Create(6);
  MergeSolidToMesh(Temp, Result);*)

  For I:=0 To 1 Do
  Begin
    Temp := CylinderMesh.Create(2, 12);
    Temp.Transform(Matrix4x4Transform(VectorCreate(-LegOfs, -0.75 * HeadSize - LegSize * I, 0), VectorZero, VectorCreate(LegScale, LegSize, LegScale)));
    MergeSolidToMesh(Temp, Result);

    Temp := CylinderMesh.Create(2, 12);
    Temp.Transform(Matrix4x4Transform(VectorCreate(LegOfs, -0.75 * HeadSize - LegSize * I, 0), VectorZero, VectorCreate(LegScale, LegSize, LegScale)));
    MergeSolidToMesh(Temp, Result);
  End;

  Temp := CubeMesh.Create(2);
  Temp.Transform(Matrix4x4Transform(VectorCreate(-LegOfs, -LegSize * 1.95, FootSize * 0.25), VectorZero, VectorCreate(FootSize * 0.5, FootSize * 0.25, FootSize)));
  MergeSolidToMesh(Temp, Result);

  Temp := CubeMesh.Create(2);
  Temp.Transform(Matrix4x4Transform(VectorCreate(LegOfs, -LegSize * 1.95, FootSize * 0.25), VectorZero, VectorCreate(FootSize * 0.5, FootSize * 0.25, FootSize)));
  MergeSolidToMesh(Temp, Result);

  Result.UpdateBoundingBox();
End;}

{ AnimationRig }


{ BipedAnimationRig }
Procedure BipedAnimationRig.GenerateSkeleton;
Begin
  inherited;

End;

End.
