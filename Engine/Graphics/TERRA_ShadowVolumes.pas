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
 * TERRA_ShadowVolumes
 * Implements hard shadows via shadow volumes
 ***********************************************************************************************************************
}

Unit TERRA_ShadowVolumes;
{$I terra.inc}

{-$DEFINE CARMACKREVERSE}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_Utils, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Math, TERRA_Stream, TERRA_VertexFormat;

Const
  MinShadowAngle = 0.5;
  ExtrusionValue = 100.0;
  BiasExtrusionValue  = 0.05;
  ShadowRebuildTime   = 10;
  AnimationCacheTime  = 1.0;

Type
  ShadowVolume = Class(TERRAObject)
    Private
      _ExtrudedVertices:Array Of Vector3D;
      _ExtrudedVertexCount:Integer;

      {$IFDEF CARMACKREVERSE}
      _CappingVertices:Array Of Vector3D;
      _CappingVertexCount:Integer;
      {$ENDIF}

      _BoundingBox:BoundingBox;
      _LastTime:Cardinal;

    Public
      Procedure Release; Override;

      Function Rebuild(MeshPtr:Pointer; InstancePtr:Pointer):Boolean;
      Procedure Render;

      Procedure Translate(Ofs:Vector3D);

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

      Property BoundingBox:TERRA_BoundingBox.BoundingBox Read _BoundingBox;
      Property Time:Cardinal Read _LastTime;
    End;

Implementation
Uses TERRA_OS, TERRA_GraphicsManager, TERRA_Lights, TERRA_Mesh, TERRA_Renderer, TERRA_MeshFilter, TERRA_Log, TERRA_Matrix4x4;

Type
  GroupVertex = Record
    VertexID:Word;
    GroupID:Word;
  End;

  Edge=Record
      A:GroupVertex;
      B:GroupVertex;
  End;

Var
  Edges:Array[0..Pred(High(Word))] Of Edge;

{$Q-}
// Adds an edge to a list of silohuette edges of a shadow volume.
Procedure AddEdge(Var EdgeCount:Integer; V0,V1:GroupVertex);
Var
 I:Integer;
Begin
  // Remove interior edges (which appear in the list twice)
  For I:=0 To Pred(EdgeCount)Do
  Begin
    If ((Edges[I].A.VertexID=V0.VertexID) And (Edges[I].A.GroupID=V0.GroupID) And
        (Edges[I].B.VertexID=V1.VertexID) And (Edges[I].B.GroupID=V1.GroupID))
    Or ((Edges[I].A.VertexID=V1.VertexID) And (Edges[I].A.GroupID=V1.GroupID) And
        (Edges[I].B.VertexID=V0.VertexID) And (Edges[I].B.GroupID=V0.GroupID)) Then
      Begin
        Edges[I].A:=Edges[Pred(EdgeCount)].A;
        Edges[I].B:=Edges[Pred(EdgeCount)].B;
        Dec(EdgeCount);
        Exit;
      End;
  End;

  Edges[EdgeCount].A := V0;
  Edges[EdgeCount].B := V1;
  Inc(EdgeCount);
End;

// ShadowVolume

// Takes a mesh as input, and uses it to build a shadowvolume. The
// technique used considers each triangle of the mesh, and adds it's
// edges to a temporary list. The edge list is maintained, such that
// only silohuette edges are kept. Finally, the silohuette edges are
// extruded to make the shadow volume vertex list.

Function ShadowVolume.Rebuild(MeshPtr:Pointer; InstancePtr:Pointer):Boolean;
Var
  I,J, K:Integer;
  EdgeCount:Integer;
  A,B,C:GroupVertex;
  V0,V1,V2,V3,V4:Vector3D;
  VS,VX:Vector3D;
  Normal:Vector3D;

  V:Vector3D;
  BoneIndex:Single;

  L:Vector3D;
  Instance:MeshInstance;
  VertexBuffer:Array Of Array Of Vector3D;
  ShowGroup:Array Of Boolean;

  TM:Matrix4x4;

  T:Triangle;
  Group:MeshGroup;
  MyMesh:TERRAMesh;
Begin
  MyMesh := MeshPtr;
  Result := True;
  Instance := MeshInstance(InstancePtr);
  If (Application.GetTime-_LastTime < ShadowRebuildTime) Or (Not Assigned(Instance)) Then
    Exit;
  _LastTime := Application.GetTime;

  EdgeCount:=0;

  L := LightManager.Instance.GetDefaultDirection();
  //L.Scale(-1.0);
  {If L.Y<0 Then
    L.Y := - L.Y;

  If L.Y< MinShadowAngle Then
    L.Y := MinShadowAngle;
  }
  L.Normalize;

  _ExtrudedVertexCount := 0;
  {$IFDEF CARMACKREVERSE}
  _CappingVertexCount := 0;
  {$ENDIF}

  SetLength(ShowGroup, MyMesh.GroupCount);
  SetLength(VertexBuffer, MyMesh.GroupCount);
  For J:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    Group := MyMesh.GetGroup(J);
    ShowGroup[J] := Instance.GetVisibility(J);
    If (Not ShowGroup[J]) Or (Group.Flags And meshGroupCastShadow=0) Then
      Continue;

    SetLength(VertexBuffer[J], Group.VertexCount);

    For I:=0 To Pred(Length(VertexBuffer[J])) Do
    Begin
      Group.Vertices.GetVector3D(I, vertexPosition, V);
      Group.Vertices.GetFloat(I, vertexBone, BoneIndex);
      If (BoneIndex>0) Then
      Begin
        K := Trunc(BoneIndex);
        If (Instance.Animation = Nil) Or (Instance.Animation.Root = Nil) Then
          //TM := (MyMesh.Skeleton.BindPose[K])
          TM := Matrix4x4Identity
        Else
          TM := (Instance.Animation.Transforms[K]);

        V := TM.Transform(V);
      End;

      VertexBuffer[J,I] := Instance.Transform.Transform(V);
    End;
  End;

  //For each face
  For J:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    Group := MyMesh.GetGroup(J);
    If (Not ShowGroup[J]) Or (Group.Flags And meshGroupCastShadow=0)  Then
      Continue;

    A.GroupID := J;
    B.GroupID := J;
    C.GroupID := J;

    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTriangle(I);
      A.VertexID := T.Indices[0];
      B.VertexID := T.Indices[1];
      C.VertexID := T.Indices[2];

      V0 := VertexBuffer[J, A.VertexID];
      V1 := VertexBuffer[J, B.VertexID];
      V2 := VertexBuffer[J, C.VertexID];

      Normal := TriangleNormal(V0,V1,V2);

      If (Normal.Dot(L) >= 0.0) Then
      Begin
        AddEdge(EdgeCount,A,B);
        AddEdge(EdgeCount,B,C);
        AddEdge(EdgeCount,C,A);
      End;
    End;
  End;

  VS := VectorScale(L, ExtrusionValue);
  VX := VectorScale(L, BiasExtrusionValue);

  SetLength(_ExtrudedVertices, _ExtrudedVertexCount+EdgeCount*6);
  For I:=0 To Pred(EdgeCount) Do
  Begin
    V1:=VertexBuffer[Edges[I].A.GroupID, Edges[I].A.VertexID];
    V2:=VertexBuffer[Edges[I].B.GroupID, Edges[I].B.VertexID];

    V1:=VectorSubtract(V1,VX);
    V2:=VectorSubtract(V2,VX);

    V3:=VectorSubtract(V1,VS);
    V4:=VectorSubtract(V2,VS);

    // Add a quad to the vertex list
    _ExtrudedVertices[_ExtrudedVertexCount]:=V3; Inc(_ExtrudedVertexCount);
    _ExtrudedVertices[_ExtrudedVertexCount]:=V4; Inc(_ExtrudedVertexCount);
    _ExtrudedVertices[_ExtrudedVertexCount]:=V2; Inc(_ExtrudedVertexCount);
    _ExtrudedVertices[_ExtrudedVertexCount]:=V2; Inc(_ExtrudedVertexCount);
    _ExtrudedVertices[_ExtrudedVertexCount]:=V1; Inc(_ExtrudedVertexCount);
    _ExtrudedVertices[_ExtrudedVertexCount]:=V3; Inc(_ExtrudedVertexCount);
  End;

  {$IFDEF CARMACKREVERSE}
  //For each face
  For J:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    Group := MyMesh.GetGroup(J);
    If (Not ShowGroup[J]) Then
      Continue;

    A.GroupID := J;
    B.GroupID := J;
    C.GroupID := J;

    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      T := Group.GetTriangle(I);
      A.VertexID := T.A;
      B.VertexID := T.B;
      C.VertexID := T.C;

      V0 := VertexBuffer[A.GroupID, A.VertexID];
      V1 := VertexBuffer[B.GroupID, B.VertexID];
      V2 := VertexBuffer[C.GroupID, C.VertexID];

      Normal := TriangleNormal(V0,V1,V2);
      Dot := Normal.Dot(L);

      If (Dot<ScalarZero) Then
      Begin
        SetLength(_CappingVertices, _CappingVertexCount+3);
        V0:=VectorSubtract(V0,VS);
        V1:=VectorSubtract(V1,VS);
        V2:=VectorSubtract(V2,VS);

        _CappingVertices[_CappingVertexCount]:=V0;Inc(_CappingVertexCount);
        _CappingVertices[_CappingVertexCount]:=V1;Inc(_CappingVertexCount);
        _CappingVertices[_CappingVertexCount]:=V2;Inc(_CappingVertexCount);
      End Else
      If (Dot>=ScalarZero) Then
      Begin
        SetLength(_CappingVertices, _CappingVertexCount+3);
        V0:=VectorSubtract(V0,VX);
        V1:=VectorSubtract(V1,VX);
        V2:=VectorSubtract(V2,VX);

        _CappingVertices[_CappingVertexCount]:=V0;Inc(_CappingVertexCount);
        _CappingVertices[_CappingVertexCount]:=V1;Inc(_CappingVertexCount);
        _CappingVertices[_CappingVertexCount]:=V2;Inc(_CappingVertexCount);
      End;
    End;
  End;
  {$ENDIF}

  If (_ExtrudedVertexCount{$IFDEF CARMACKREVERSE}+_CappingVertexCount{$ENDIF}<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  _BoundingBox.StartVertex := VectorConstant(ExtrusionValue*2);
  _BoundingBox.EndVertex := VectorConstant(-ExtrusionValue*2);

  For I:=0 To Pred(_ExtrudedVertexCount) Do
  With _ExtrudedVertices[I] Do
  Begin
    _BoundingBox.StartVertex.x := FloatMin(_BoundingBox.StartVertex.x,X);
    _BoundingBox.StartVertex.y := FloatMin(_BoundingBox.StartVertex.y,Y);
    _BoundingBox.StartVertex.z := FloatMin(_BoundingBox.StartVertex.z,Z);
    _BoundingBox.EndVertex.x := FloatMax(_BoundingBox.EndVertex.x,X);
    _BoundingBox.EndVertex.y := FloatMax(_BoundingBox.EndVertex.y,Y);
    _BoundingBox.EndVertex.z := FloatMax(_BoundingBox.EndVertex.z,Z);
  End;

  {$IFDEF CARMACKREVERSE}
  For I:=0 To Pred(_CappingVertexCount) Do
  With _CappingVertices[I] Do
  Begin
    _BoundingBox.StartVertex.x := FloatMin(_BoundingBox.StartVertex.x,X);
    _BoundingBox.StartVertex.y := FloatMin(_BoundingBox.StartVertex.y,Y);
    _BoundingBox.StartVertex.z := FloatMin(_BoundingBox.StartVertex.z,Z);
    _BoundingBox.EndVertex.x := FloatMax(_BoundingBox.EndVertex.x,X);
    _BoundingBox.EndVertex.y := FloatMax(_BoundingBox.EndVertex.y,Y);
    _BoundingBox.EndVertex.z := FloatMax(_BoundingBox.EndVertex.z,Z);
  End;
  {$ENDIF}
End;

Procedure ShadowVolume.Render;
Var
  Graphics:GraphicsManager;
Begin
  If (_ExtrudedVertexCount<=0) Or (Length(_ExtrudedVertices)<=0) Then
	  Exit;

  Log(logDebug, 'Shadow', 'Drawing shadow volume: '+IntToSTring(_ExtrudedVertexCount));

  Graphics := GraphicsManager.Instance;

  {Graphics.Renderer.SetSourceVertexSize(SizeOf(Vector3D));
  Graphics.Renderer.SetAttributeSource('terra_position', vertexPosition, typeVector3D, @(_ExtrudedVertices[0]));}
  Graphics.Renderer.SetVertexSource(Nil);
  Graphics.Renderer.DrawSource(renderTriangles, _ExtrudedVertexCount);

  {$IFDEF CARMACKREVERSE}
  If GraphicsManager.Instance.ShowShadowVolumes Then
    glColor4ub(255,0,0,128);

  Graphics.Renderer.DrawSource(renderTriangles,  SizeOf(Vector3D), @(_CappingVertices[0]));
  Graphics.Renderer.DrawSource(renderQuads, _CappingVertexCount);

  (*
  glBegin(GL_TRIANGLES);
  For I:=0 To Pred(_CappingVertexCount) Do
  Begin
    glVertex3fv(@(_CappingVertices[I]));
  End;
  glEnd;*)
  {$ENDIF}
End;

  (*If Assigned(glActiveStencilFaceEXT) Then
  Begin
    {$IFDEF CARMACKREVERSE}
    If _CappingVertexCount>0 Then
    Begin
      glDisable(GL_CULL_FACE);
      glEnable(GL_STENCIL_TEST_TWO_SIDE_EXT);

      glActiveStencilFaceEXT(GL_BACK);
      glStencilOp(GL_KEEP, GL_INCR, GL_KEEP);
      glActiveStencilFaceEXT(GL_FRONT);
      glStencilOp(GL_KEEP, GL_DECR, GL_KEEP);

      RenderVolume();

      glEnable(GL_CULL_FACE);
      glDisable(GL_STENCIL_TEST_TWO_SIDE_EXT);
    End Else
    {$ENDIF}
    Begin
      glDisable(GL_CULL_FACE);
      glEnable(GL_STENCIL_TEST_TWO_SIDE_EXT);

      glActiveStencilFaceEXT(GL_BACK);
      glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
      glActiveStencilFaceEXT(GL_FRONT);
      glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);

      RenderVolume();

      glEnable(GL_CULL_FACE);
      glDisable(GL_STENCIL_TEST_TWO_SIDE_EXT);
    End;
  End Else


    {$IFDEF CARMACKREVERSE}
    If _CappingVertexCount>0 Then
    Begin
      glCullFace(GL_FRONT);
      glStencilOp(GL_KEEP,GL_INCR,GL_KEEP);
      RenderVolume();

      glCullFace(GL_BACK);
      glStencilOp(GL_KEEP,GL_DECR,GL_KEEP);
      RenderVolume();
    End Else
    {$ENDIF}
  *)

Procedure ShadowVolume.Release;
Begin
  SetLength(_ExtrudedVertices, 0);
{$IFDEF CARMACKREVERSE}
  SetLength(_CappingVertices, 0);
{$ENDIF}
End;

Procedure ShadowVolume.Translate(Ofs:Vector3D);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ExtrudedVertexCount) Do
  Begin
    _ExtrudedVertices[I].X := _ExtrudedVertices[I].X + Ofs.X;
    _ExtrudedVertices[I].Y := _ExtrudedVertices[I].Y + Ofs.Y;
    _ExtrudedVertices[I].Z := _ExtrudedVertices[I].Z + Ofs.Z;
  End;
{$IFDEF CARMACKREVERSE}
  For I:=0 To Pred(_CappingVertexCount) Do
    _CappingVertices[I].Add(Ofs);
{$ENDIF}
End;

Procedure ShadowVolume.Load(Source: Stream);
Begin
  Source.Read(@_ExtrudedVertexCount, 4);
  SetLength(_ExtrudedVertices, _ExtrudedVertexCount);
  If (_ExtrudedVertexCount>0) Then
    Source.Read(@_ExtrudedVertices[0], _ExtrudedVertexCount * SizeOf(Vector3D));
End;

Procedure ShadowVolume.Save(Dest: Stream);
Begin
  Dest.Write(@_ExtrudedVertexCount, 4);
  If (_ExtrudedVertexCount>0) Then
    Dest.Write(@_ExtrudedVertices[0], _ExtrudedVertexCount * SizeOf(Vector3D));
End;

End.

