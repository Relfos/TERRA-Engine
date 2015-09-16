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
 * TERRA_Tesselator
 * Implements polygon tesselation
 ***********************************************************************************************************************
}

Unit TERRA_Tesselator;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Math, TERRA_Vector2D, TERRA_Vector3D, TERRA_GLU;

Type
  Tesselator = Class(TERRAObject)
    Protected
      _PointList: Array of Vector2D;    //  polygon point
      _PointCount: integer;

      _VertexList: Array of Vector2D;  //  triangulated data
      _VertexCount: integer;

      _TriangleList:Array Of Triangle;
      _TriangleCount:Integer;

      Procedure AddVertex(X,Y:Single);

    Public
      Constructor Create;
      Procedure Release;

      Procedure BeginTesselation;
      Procedure AddPoint(X,Y:Single);
      Procedure EndTesselation;

      Function GetVertex(Index:Integer):Vector2D;
      Function GetTriangle(Index:Integer):Triangle;

      Function GetVertexCount:Integer;
      Function GetTriangleCount:Integer;
  End;

Implementation

Type
     TGLArrayf4 = array[0..3] of Single;
     TGLArrayd3 = array[0..2] of Double;
     TGLArrayd6 = array[0..5] of Double;
     PGLArrayd6 = ^TGLArrayd6;
     TGLArrayvertex4 = array[0..3] of PGLArrayd6;
     PGLArrayvertex4 = ^TGLArrayvertex4;
     PGLArrayf4 = ^TGLArrayf4;

Threadvar
  PolygonClass: Tesselator;

procedure Tesselator.AddVertex(x, y: single);
begin
    Inc(_VertexCount);
    SetLength(_VertexList, _VertexCount);

    _VertexList[_VertexCount-1].X := X;
    _VertexList[_VertexCount-1].Y := Y;
end;

constructor Tesselator.Create();
begin
  inherited Create();
  _PointCount := 0;
  _VertexCount := 0;
end;

Procedure Tesselator.Release();
begin
  SetLength(_PointList, 0);
  SetLength(_VertexList, 0);
end;

Procedure Tesselator.AddPoint(X,Y:Single);
Begin
  Inc(_PointCount);
  SetLength(_PointList, _PointCount);
  _PointList[_PointCount-1].X := X;
  _PointList[_PointCount-1].Y := Y;
End;

Procedure Tesselator.BeginTesselation;
Begin
  _PointCount := 0;
  _VertexCount := 0;
  _Trianglecount := 0;
End;

Procedure Tesselator.EndTesselation;
var
  I,J:Integer;
  tess:Pointer;
  test:Array[0..2] Of Double;
  pol:Vector3D;

  A,B,C:Vector2D;

procedure iTessBeginCB(which: Integer); stdcall;
begin
  //PolygonClass.tessBegin(which);
end;

procedure iTessEndCB(); stdcall;
begin
  //PolygonClass.tessEnd();
end;

procedure iTessEdgeCB(flag: boolean; lpContext: pointer); stdcall;
begin
      //just do nothing to force GL_TRIANGLES !!!
end;

procedure iTessVertexCB(data: PGLArrayd6); stdcall;
begin
  PolygonClass.AddVertex(data[0], data[1]);
end;


procedure iTessCombineCB(newVertex : PGLArrayd6; neighborVertex : Pointer;
                      neighborWeight : Pointer; var outData : Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
var
  vertex: PGLArrayd6;
  loop: integer;
begin
  new(vertex);

  vertex[0] := newVertex^[0];
  vertex[1] := newVertex^[1];
  vertex[2] := newVertex^[2];

  // return output data (vertex coords and others)
  outData:= vertex;
end;

begin
  PolygonClass := Self;

  tess := gluNewTess();

  gluTessCallback(tess, GLU_TESS_BEGIN, @iTessBeginCB );
  gluTessCallback(tess, GLU_TESS_END, @iTessEndCB);
  gluTessCallback(tess, GLU_TESS_VERTEX, @iTessVertexCB);
  gluTessCallback(tess, GLU_TESS_COMBINE, @iTessCombineCB);  //does not work for font?
  gluTessCallback(tess, GLU_TESS_EDGE_FLAG_DATA, @iTessEdgeCB); //force triangles

  gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO );

  gluTessBeginPolygon(tess, nil);                   // with NULL data
  gluTessBeginContour(tess);

  For I:=0 To Pred(_PointCount) Do
  begin
      pol.X := _PointList[I].X;
      pol.Y := _PointList[I].Y;
      pol.Z := 0;

      test[0] := pol.X;
      test[1] := pol.Y;
      test[2] := pol.Z;
      gluTessVertex(tess, @test[0], @pol);
  end;

  gluTessEndContour(tess);
  gluTessEndPolygon(tess);
  gluDeleteTess(tess);        // delete after tessellation

  PolygonClass := Nil;

  _TriangleCount := _VertexCount Div 3;
  SetLength(_TriangleList, _TriangleCount);
  For J:=0 To Pred(_TriangleCount) Do
  Begin
    _TriangleList[J].Indices[0] := J*3 + 0;
    _TriangleList[J].Indices[1] := J*3 + 1;
    _TriangleList[J].Indices[2] := J*3 + 2;
  End;
End;

Function Tesselator.GetVertex(Index:Integer):Vector2D;
Begin
  Result := _VertexList[Index];
End;

Function Tesselator.GetTriangle(Index:Integer):Triangle;
Begin
  Result := _TriangleList[Index];
End;

Function Tesselator.GetVertexCount:Integer;
Begin
  Result := _VertexCount;
End;

Function Tesselator.GetTriangleCount:Integer;
Begin
  Result := _TriangleCount;
End;

end.