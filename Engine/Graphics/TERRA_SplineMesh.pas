{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_SplineMesh
 * Generates an extruded mesh from a spline plus a base mesh(for roads, etc)
 ***********************************************************************************************************************
}
Unit TERRA_SplineMesh;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Matrix, TERRA_Quaternion,
  TERRA_Splines, TERRA_Color, TERRA_Mesh, TERRA_MeshFilter, TERRA_Classes;

Const
  DefaultSegmentCount = 100;

Type
  SplineMesh = Class(MeshFilter)
    Protected
		  _vertices:Array Of MeshVertex;
      _VertexCount:Integer;

		  _triangles:Array Of Triangle;
      _TriangleCount:Integer;

  	  _Spline:Spline;
      _SegmentCount:Integer;
      _BaseMesh:Mesh;

      Procedure Clear();
      Procedure CalculateBentMesh(Group:MeshGroup; Var vIndex:Integer; Const verticesFront, verticesBack:IntegerArray;
                                Var centerFront, centerBack:Vector3D; param0, param1:Single);

    Public
      ScaleX:Single;
      ScaleY:Single;
      ScaleU:Single;
      ScaleV:Single;
      swapUV:Boolean;

      Constructor Create(MySpline:Spline; BaseMesh:Mesh; SegmentCount:Integer = DefaultSegmentCount);
      Procedure UpdateMesh();

      Function GetGroupCount:Integer; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;
    End;

Implementation


Constructor SplineMesh.Create(MySpline:Spline; BaseMesh:Mesh; SegmentCount:Integer);
Begin
  Self._Spline := MySpline;
  Self._BaseMesh := BaseMesh;
  Self._SegmentCount := SegmentCount;
  ScaleX := 1.0;
  ScaleY := 1.0;
  ScaleU := 1.0;
  ScaleV := 1.0;
  swapUV := False;
End;

Procedure SplineMesh.Clear();
Begin
  _VertexCount := 0;
  _TriangleCount := 0;
End;

Procedure SplineMesh.UpdateMesh();
Var
  I, J, N, vIndex, segment:Integer;
  V:MeshVertex;
  Group:MeshGroup;
  verticesFront, verticesBack:IntegerArray;
  centerFront, centerBack:Vector3D;
  param0, param1: Single;
Begin
  //Reset the generated meshes
  Self.Clear();

  If (_Spline = Nil) Or (Self._SegmentCount<=0) Then
    Exit;

  //Gather model data
  _BaseMesh.Prefetch();
  Group := _BaseMesh.GetGroup(0);

  //Allocate some memory for new mesh data
  _VertexCount := Group.VertexCount * _segmentCount;
  SetLength(Self._vertices, _VertexCount);
  _TriangleCount := Group.TriangleCount * _segmentCount;
  SetLength(Self._triangles, _TriangleCount);

  //Group front/rear vertices together
  verticesFront.Count := 0;
  verticesBack.Count := 0;

  centerFront := VectorZero;
  centerBack := VectorZero;

  For I:=0 To Pred(Group.VertexCount) Do
	Begin
    V := Group.GetVertex(I);
    If (V.Position.z > 0 ) Then
    Begin
      verticesFront.Add( i );
      centerFront.Add(V.Position);;
    End Else
    If (V.Position.z < 0) Then
    Begin
      verticesBack.Add( i );
			centerBack.Add(V.Position);
    End;
  End;

  centerFront.Scale(1/verticesFront.Count);
  centerBack.Scale(1/verticesBack.Count);

  vIndex := 0;
  For segment := 0 To Pred(_segmentCount) Do
  Begin
    param0 := segment / _segmentCount;
    param1 := (segment+1) / _segmentCount;

    If (param1 >= 1.0) Then
      param1 := param1 - 0.00001;

    CalculateBentMesh(Group, vIndex, verticesFront, verticesBack, centerFront, centerBack, param0, param1);

    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      N := I+(segment*Group.TriangleCount);
      _Triangles[N] := Group.GetTriangle(i);
      For J:=0 To 2 Do
        Inc(_Triangles[N].Indices[J], Group.VertexCount * segment);
    End;

  End;
End;

Procedure SplineMesh.CalculateBentMesh(Group:MeshGroup; Var vIndex:Integer; Const verticesFront, verticesBack:IntegerArray;
                            Var centerFront, centerBack:Vector3D; param0, param1:Single);
Var
  pos0, pos1:Vector3D;
  rot0, rot1:Quaternion;
  I:Integer;
  tmpVert:Vector3D;
  tmpNormal:Vector3D;
  tmpUV:Vector2D;
Begin
  //spline.transform.InverseTransformPoint(spline.GetPositionOnSpline( param0 ));
  pos0 := _Spline.GetPosition(param0);
  pos1 := _Spline.GetPosition(param1);

  //rot0 = spline.GetOrientationOnSpline( param0 ) * Quaternion.Inverse( spline.transform.localRotation );
  rot0 := _Spline.GetOrientation(param0);
  rot1 := _Spline.GetOrientation(param1);

	For I:=0 To Pred(Group.VertexCount) Do
  Begin
    _vertices[vIndex] := Group.GetVertex(I);
    tmpVert := _vertices[vIndex].Position;
    tmpUV := _vertices[vIndex].TextureCoords;
		tmpNormal := _vertices[vIndex].Normal;

    If (verticesBack.Contains(i)) Then
		Begin
				tmpVert.Subtract(centerBack);
				tmpVert.Scale(VectorCreate(ScaleX, ScaleY, 1.0));

				tmpVert := QuaternionTransform(rot0, tmpVert);
				tmpVert.Add(pos0);

				tmpNormal := QuaternionTransform(rot0, tmpNormal);

				If (Not swapUV ) Then
					tmpUV.y := param0
				Else
					tmpUV.x := param0;
    End Else
    If (verticesFront.Contains(i)) Then
    Begin
      tmpVert.Subtract(centerFront);
      tmpVert.Scale(VectorCreate(ScaleX, ScaleY, 1.0 ) );

      tmpVert := QuaternionTransform(rot1, tmpVert);
      tmpVert.Add(pos1);

      tmpNormal := QuaternionTransform(rot1, tmpNormal);

      If (Not swapUV ) Then
        tmpUV.y := param1
      Else
        tmpUV.x := param1;
    End;

		_vertices[vIndex].Position := tmpVert;
//		_vertices[vIndex].TextureCoords := VectorCreate2D(tmpUV.X * ScaleU, tmpUV.Y * ScaleV);
    _vertices[vIndex].Normal := tmpNormal;
    Inc(vIndex);
  End;
End;

Function SplineMesh.GetGroupCount: Integer;
Begin
  Result := 1;
End;

Function SplineMesh.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Triangles[Index];
End;

Function SplineMesh.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := Self._TriangleCount;
End;

Function SplineMesh.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  Result := _Vertices[Index].Color;
End;

Function SplineMesh.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := Self._VertexCount;
End;

Function SplineMesh.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Vertices[Index].Normal;
End;

Function SplineMesh.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Vertices[Index].Position;
End;

Function SplineMesh.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Vertices[Index].TextureCoords;
End;

Function SplineMesh.GetVertexFormat(GroupID: Integer): Cardinal;
Begin
  Result := meshFormatNormal Or meshFormatColor Or meshFormatUV1;
End;

End.

