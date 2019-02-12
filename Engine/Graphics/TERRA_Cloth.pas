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
 * TERRA_Cloth
 * Implements cloth mesh
 ***********************************************************************************************************************
}

Unit TERRA_Cloth;
{$I terra.inc}

{
TODO
- add stick and slider constraints

GPU ARRAY
- 1 texture with each particle position x,y,z, mass
- 2 texture with each particle last position x,y,z, fixed
- 3 texture with each particle acceleration x,y,z
- 4 texture with contrainsts A,B, rest distance

}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_GraphicsManager, TERRA_Texture, TERRA_Quaternion, TERRA_Classes, TERRA_Math,
  TERRA_BoundingBox, TERRA_Vector3D, TERRA_Vector2D, TERRA_Ray, TERRA_Particles, TERRA_VerletParticle,
  TERRA_Color, TERRA_Shader;

Type
  VerletCloth = Class(TERRAObject)
    Protected
		  _Shader:Shader;

      _ParticleSystem:VerletSystem;
      _Vertices:Array Of ParticleVertex;
      _VertexCount:Integer;
      _Triangles:Array Of Triangle;
      _TriangleCount:Integer;
      _SourceMesh:Pointer;
      _NormalIndices:Array Of IntegerArray;
      _Normals:Array Of Vector3D;

	{ A private method used by drawShaded() and addWindForcesForTriangle() to retrieve the
	normal vector of the triangle defined by the position of the particles p1, p2, and p3.
	The magnitude of the normal vector is equal to the area of the parallelogram defined by p1, p2 and p3
	}
	    Function calcTriangleNormal(A,B,C:Integer):Vector3D;

	// A private method used by windForce() to calcualte the wind force for a single triangle defined by p1,p2,p3
	    Procedure AddWindForcesForTriangle(A,B,C:Integer; direction:Vector3D);

      Procedure UpdateNormals;

  Public

	  //This is a important constructor for the entire system of particles and constraints*/
	  Constructor Create(SourceMeshGroup:Pointer);
    Procedure Release;

	{ drawing the cloth as a smooth shaded (and colored according to column) OpenGL triangular mesh
	Called from the display() method
	The cloth is seen as consisting of triangles for four particles in the grid as follows:

	(x,y)   *--* (x+1,y)
	        | /|
	        |/ |
	(x,y+1) *--* (x+1,y+1)

	}
	  Procedure Render(Instance:Pointer);
    Function GetBoundingBox:BoundingBox;

    Procedure AddForce(F:Vector3D);

	// used to add wind forces to all particles, is added for each triangle since the final force is proportional to the triangle area as seen from the wind direction
	  Procedure WindForce(direction:Vector3D);

	  Procedure BallCollision(center:Vector3D; Radius:Single);

    Procedure PinPoint(Index:Integer);

    Function Intersect(Const R:Ray; Var T:Single):Boolean;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_ResourceManager, TERRA_Matrix, TERRA_Mesh, TERRA_MeshInstance,
  TERRA_Lights;

Function VerletCloth.calcTriangleNormal(A,B,C:Integer):Vector3D;
Var
  VA,VB:Vector3D;
  p1,p2,p3:PVerletParticle;
Begin
  P1 := _ParticleSystem.GetParticle(A);
  P2 := _ParticleSystem.GetParticle(B);
  P3 := _ParticleSystem.GetParticle(C);
  VA := VectorSubtract(P2.Position, P1.Position);
  VB := VectorSubtract(P3.Position, P1.Position);
  Result := VectorCross(VA,VB);
  Result.Normalize;
End;

Procedure VerletCloth.addWindForcesForTriangle(A,B,C:Integer; direction:Vector3D);
Var
  D, Force, Normal:Vector3D;
  p1,p2,p3:PVerletParticle;
Begin
  P1 := _ParticleSystem.GetParticle(A);
  P2 := _ParticleSystem.GetParticle(B);
  P3 := _ParticleSystem.GetParticle(C);
  Normal := CalcTriangleNormal(A,B,C);
  D := Normal;
  D.Normalize;
  Force := VectorScale(Normal, VectorDot(D, direction));
  p1.addForce(force);
	p2.addForce(force);
  p3.addForce(force);
End;

Constructor VerletCloth.Create(SourceMeshGroup:Pointer);
Var
  Pos:Vector3D;
  P:PVerletParticle;
  I,J:Integer;
  Group:MeshGroup;
  V:MeshVertex;
  T:Triangle;
  MinDist:Single;
Begin
  _SourceMesh := SourceMeshGroup;
  Group := SourceMeshGroup;

  _Shader := ResourceManager.Instance.GetShader('cloth');

  _VertexCount := Group.VertexCount;

  SetLength(_Vertices, _VertexCount);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    V := Group.GetVertex(I);
    _Vertices[I].Position := V.Position; // insert particle in column x at y'th row
    _Vertices[I].UV := V.TextureCoords;
    _Vertices[I].Normal := V.Normal;
    _Vertices[I].Color := ColorWhite;
  End;

  _ParticleSystem := VerletSystem.Create(@(_Vertices[0]), _VertexCount, False);

  // create geometry
  _TriangleCount := Group.TriangleCount;
  SetLength(_Triangles, _TriangleCount);
  SetLength(_NormalIndices, _VertexCount);
  SetLength(_Normals, _TriangleCount);
  For I:=0 To Pred(Group.TriangleCount) Do
  Begin
    T := Group.GetTriangle(I);

    _Triangles[I].A := T.A;
    _Triangles[I].B := T.B;
    _Triangles[I].C := T.C;

    _NormalIndices[T.A].Add(I);
    _NormalIndices[T.B].Add(I);
    _NormalIndices[T.C].Add(I);
  End;

  T := Group.GetTriangle(0);
  MinDist := _Vertices[T.A].Position.Distance(_Vertices[T.B].Position)* 1.2;

  For I:=0 To Pred(_VertexCount) Do
    For J:=0 To Pred(_VertexCount) Do
      If (I<>J) And (_Vertices[I].Position.Distance(_Vertices[J].Position)<=MinDist) Then
        _ParticleSystem.AddConstraint(I, J);
End;

Procedure VerletCloth.Release;
Begin
  ReleaseObject(_ParticleSystem);
End;

Procedure VerletCloth.PinPoint(Index:Integer);
Var
  P:PVerletParticle;
Begin
  P := _ParticleSystem.GetParticle(Index);
//  P.offsetPos(VectorCreate(0.0,-0.5,0.0)); // moving the particle a bit towards the center, to make it hang more natural - because I like it ;)
  P.Fixed := True;
End;

Function VerletCloth.GetBoundingBox:BoundingBox;
Begin
  Result := _ParticleSystem.BoundingBox;
End;

Procedure VerletCloth.UpdateNormals;
Var
  I,J,N:Integer;
Begin
  For I:=0 To Pred(_TriangleCount) Do
    _Normals[I] := calcTriangleNormal(_Triangles[I].A, _Triangles[I].B, _Triangles[I].C);

  //create smooth per particle normals by adding up all the (hard) triangle normals that each particle is part of
  For I:=0 To Pred(_VertexCount) Do
    For J := 0 To Pred(_NormalIndices[I].Count) Do
    Begin
      N := _NormalIndices[I].Items[J];
      _Vertices[I].Normal.Add(_Normals[N]);
    End;
End;

Procedure VerletCloth.Render(Instance:Pointer);
Var
  Normal, Color:Vector3D;
  I,J:Integer;
  Group:MeshGroup;
Begin
  Group := MeshGroup(Self._SourceMesh);

{  If (Assigned(LightManager.Instance.ActiveShadowMap)) Then
  Begin
    LightManager.Instance.ActiveShadowMap.SetShader(False);
  End Else}
  Begin
    Texture.Bind(Group.DiffuseMap, 0);
  	Shader.Bind(_Shader);
    Shader.SetUniform('diffuseMap',0);
    GraphicsManager.Instance.ActiveViewport.Camera.SetUniforms;
  End;

  Shader.SetUniform('modelMatrix', MeshInstance(Instance).Transform);

  _ParticleSystem.Update;

  UpdateNormals;

{$IFNDEF MOBILE}
  glDisable(GL_CULL_FACE);    
  glBegin(GL_TRIANGLES);
  For I:=0 To Pred(_TriangleCount) Do
    For J:=0 To 2 Do
    Begin
    	glNormal3fv(@_Vertices[_Triangles[I].Indices[J]].Normal);
    	glColor4fv(@_Vertices[_Triangles[I].Indices[J]].Color);
      glMultiTexCoord2fv(GL_TEXTURE0, @_Vertices[_Triangles[I].Indices[J]].UV);
    	glVertex3fv(@_Vertices[_Triangles[I].Indices[J]].Position);
    End;
  glEnd;  
  glEnable(GL_CULL_FACE);  
  
{$ENDIF}
End;

Procedure VerletCloth.AddForce(F:Vector3D);
Begin
  _ParticleSystem.AddForce(F);
End;

Procedure VerletCloth.WindForce(direction:Vector3D);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TriangleCount) Do
    AddWindForcesForTriangle(_Triangles[I].A, _Triangles[I].B, _Triangles[I].C, direction);
End;

Procedure VerletCloth.BallCollision(center:Vector3D; Radius:Single);
Begin
  _ParticleSystem.BallCollision(Center, Radius);
End;

Function VerletCloth.Intersect(Const R:Ray; Var T:Single):Boolean;
Begin
  Result := False;
End;

End.
