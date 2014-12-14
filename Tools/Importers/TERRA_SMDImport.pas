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
 * TERRA_SMDImport
 * Implements SMD importer
 ***********************************************************************************************************************
}
Unit TERRA_SMDImport;

{$I terra.inc}

Interface
Uses TERRA_Application, TERRA_SMD, TERRA_MeshAnimation, TERRA_Utils, TERRA_OS;

implementation

Uses TERRA_Mesh, TERRA_INI, TERRA_IO, TERRA_Matrix, TERRA_ResourceManager,
  TERRA_Vector3D, TERRA_Vector2D, TERRA_Math, TERRA_Color, TERRA_Log,
  SysUtils, TERRA_MeshFilter, TERRA_FileImport, TERRA_FileIO,
  TERRA_FileUtils, TERRA_MS3D;

{Procedure ImportAnimation(Const SMD:SMDAnimation; TargetDir, Prefix:AnsiString);
Var
  I, J, K:Integer;
  Parser:INIParser;
  MyStream:Stream;
  Dest:Stream;
  Anim:Animation;
  StartFrame, EndFrame, LoopFrame:Integer;
  ActionName, NextAction, S:AnsiString;
  LoopAnimation:Boolean;
  BaseTime, Z:Single;
  TargetName:AnsiString;
  ActionList:Array Of AnsiString;
Begin
    MyStream := MemoryStream.Create(Length(MS3D.Comment), @(MS3D.Comment[1]));
    Parser := INIParser.Create;
    Parser.ParseCommas:=True;
    Parser.AddToken('Start',tkInteger,@StartFrame);
    Parser.AddToken('End',tkInteger,@EndFrame);
    Parser.AddToken('LoopFrame',tkInteger,@LoopFrame);
    Parser.AddToken('Loop',tkBoolean,@LoopAnimation);
    Parser.AddToken('Name',tkString,@ActionName);
    Parser.AddToken('Next',tkString,@NextAction);

    While Not MyStream.EOF Do
    Begin
      MyStream.ReadLine(S);
      If S='' Then
        Continue;

      StartFrame:=0;
      EndFrame:=0;
      LoopAnimation := False;
      LoopFrame:=0;
      ActionName:='untitled_animation';
      NextAction:='';
      Parser.LoadFromString(S);
      If EndFrame=0 Then
        EndFrame:=StartFrame;
      If LoopFrame=0 Then
        LoopFrame:=StartFrame;

      TargetName := lowStr(Prefix+'_'+ActionName);
      Anim := Animation.Create(TargetName);
      WriteLn('Importing animation: ', TargetName);
      Anim.FPS := MS3D.AnimationFPS;

      BaseTime := StartFrame/Anim.FPS;
      Anim.Duration := (EndFrame - StartFrame);
      Anim.Loop := LoopAnimation;
      Anim.LoopPoint := (LoopFrame - StartFrame);

      Anim.BoneCount := MS3D.NumJoints;
      SetLength(Anim.Bones, Anim.BoneCount);

      For J:=0 To Pred(MS3D.NumJoints) Do
      Begin
        Anim.Bones[J].Name := Skeleton.GetBone(J).Name;
        Anim.Bones[J].ID := J;

        Anim.Bones[J].Positions.Count := 0;
        For K:=0 To Pred(MS3D.Joints[J].NumKeyFramesTrans) Do
        Begin
          Z := Trunc(0.5 +  MS3D.Joints[J].KeyFramesTrans[K].Time * Anim.FPS);
          If (Z>=Pred(StartFrame)) And (Z<=EndFrame) Then
            Anim.Bones[J].Positions.AddKey(MS3D.Joints[J].KeyFramesTrans[K].Time - BaseTime, MS3D.Joints[J].KeyFramesTrans[K].Vector);
        End;

        Anim.Bones[J].Rotations.Count := 0;
        For K:=0 To Pred(MS3D.Joints[J].NumKeyFramesRot) Do
        Begin
          Z := Trunc(0.5 + MS3D.Joints[J].KeyFramesRot[K].Time * Anim.FPS);
          If (Z>=StartFrame) And (Z<=EndFrame) Then
            Anim.Bones[J].Rotations.AddKey(MS3D.Joints[J].KeyFramesRot[K].Time - BaseTime, MS3D.Joints[J].KeyFramesRot[K].Vector);
        End;

        Anim.Bones[J].Scales.Count := 0;
      End;

      Anim.Save(TargetDir + PathSeparator + TargetName+'.anim');
      Anim.Destroy;
    End;
    MyStream.Destroy;
    Parser.Destroy;
End;}

Function SMDImporter(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer; Settings:AnsiString):AnsiString;
Var
  Source, Dest:Stream;
  DestFile:AnsiString;
  S:AnsiString;
  SMDAnim:SMDAnimation;
  Filter:MeshFilter;
  MyMesh:Mesh;
  Anim:Animation;
  TargetName:AnsiString;
  StartFrame, EndFrame, LoopFrame:Integer;
  BaseTime:Single;
  LoopAnimation:Boolean;
  I,J,K,Z:Integer;
  Frame:SMDFrame;
  Bone:BoneAnimation;
Begin
  Source := MemoryStream.Create(SourceFile);
  SetLength(S, Source.Size);
  Source.Read(@S[1], Length(S));
  Source.Seek(0);
  If Pos('triangles', S)>0 Then
  Begin

    DestFile := TargetDir + PathSeparator + GetFileName(SourceFile, True) + '.mesh';
    If (Not AssetModified(SourceFile, DestFile)) Then
    Begin
      Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
      Result := DestFile;
      Exit;
    End;

    Log(logConsole, 'Import', 'Reading SMD file ('+GetFileName(SourceFile, False)+')...');

    Source.Seek(0);
    Filter := SMDModel.Create;
    Filter.Load(Source);
    //ModelMilkshape3D.Save('output\test.ms3d', Filter);
    MyMesh := Mesh.CreateFromFilter(Filter);
    Dest := FileStream.Create(Destfile);
    MyMesh.Save(Dest);
    Dest.Destroy;
    Filter.Destroy;
    MyMesh.Destroy;
  End Else
  Begin
    Log(logConsole, 'Import', 'Importing SMD animation ('+GetFileName(SourceFile, False)+')...');
    FillChar(SMDAnim, SizeOf(SMDAnim), 0);
    Source.Seek(0);
    SMDAnim.Load(Source);

    TargetName := GetFileName(SourceFile,True);
    StartFrame := 0;
    EndFrame := SMDAnim.FrameCount; // SMDAnim.Duration;
    LoopAnimation := True;
    LoopFrame := StartFrame;

    Begin
      Anim := Animation.Create(TargetName);
      Anim.FPS := 24;
      BaseTime := StartFrame/Anim.FPS;
      Anim.Loop := LoopAnimation;
      Anim.LoopPoint := (LoopFrame - StartFrame);

      For J:=0 To Pred(Anim.BoneCount) Do
      Begin
        Bone := Anim.AddBone(SMDAnim.GetBone(J).Name);

        Bone.Positions.Count := 0;
        Bone.Rotations.Count := 0;

        For K:=0 To Pred(SMDAnim.FrameCount) Do
        Begin
          Frame := SMDAnim.GetFrame(K);
          Z := Trunc(Frame.Time);
          If (Z>=Pred(StartFrame)) And (Z<=EndFrame) Then
          Begin
            Bone.Positions.AddKey(Frame.Time - BaseTime, Frame.Values[J].Position);
            Bone.Rotations.AddKey(Frame.Time - BaseTime, Frame.Values[J].Rotation);
          End;
        End;

        Bone.Scales.Count := 0;
      End;

      Anim.Save(TargetDir + PathSeparator + TargetName+'.anim');
      Anim.Destroy;
    End;
  End;
End;



Initialization
  RegisterFileImporter('smd', 'mesh', SMDImporter);
End.
