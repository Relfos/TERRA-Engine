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
 * TERRA_DebugDraw
 * Implements debug drawing objects
 ***********************************************************************************************************************
}
Unit TERRA_DebugDraw;

{$I terra.inc}
Interface

Uses TERRA_String, TERRA_GraphicsManager, TERRA_Renderer, TERRA_Color, TERRA_BoundingBox, TERRA_Frustum,
  TERRA_Ray, TERRA_Matrix4x4, TERRA_Vector3D, TERRA_Vector2D, TERRA_Utils, TERRA_SpriteManager,
  TERRA_MeshSkeleton, TERRA_MeshAnimationNodes, TERRA_Collision2D, TERRA_Splines, TERRA_ClipRect;

// 3d drawing
Procedure DrawBoundingBox(Const MyBox:BoundingBox; Color:TERRA_Color.Color; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Procedure DrawRay(Const MyRay:Ray; Color:Color; Length:Single=0; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Procedure DrawLine3D(A,B:Vector3D; MyColor:Color; Thickness:Single = 2.0; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Procedure DrawSpline(Const S:Spline; Color:TERRA_Color.Color; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);

Procedure DrawFrustum(Const MyFrustum:Frustum; Color:TERRA_Color.Color);
Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);

Procedure DrawSkeleton(Const MySkeleton:MeshSkeleton; MyColor:Color; State:AnimationState; Transform:Matrix4x4; DrawJoints:Boolean = True; UseBoneColors:Boolean = False);
Procedure DrawBone(Const MySkeleton:MeshSkeleton; BoneID:Integer; Color:Color; State:AnimationState; Transform:Matrix4x4; Width:Single=2);


// 2d drawing
Procedure DrawLine2D(P1,P2:Vector2D; MyColor:Color; Layer:Single; Width:Single = 2.0; Pattern:Integer=0);

Procedure DrawPolygon2D(Poly:Polygon2D; MyColor:Color; Layer:Single);
Procedure DrawPointCloud(Cloud:PointCloud2D; MyColor:Color; Layer:Single);

Procedure DrawRect(Line:Line2D; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect; KeepAlive:Boolean = False); Overload;
Procedure DrawRect(X1,Y1,X2,Y2:Single; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect; KeepAlive:Boolean = False); Overload;

Procedure DrawFilledRect(Line:Line2D; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect); Overload;
Procedure DrawFilledRect(X1,Y1,X2,Y2:Single; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect); Overload;

Procedure DrawPoint2D(Const P:Vector2D; MyColor:Color; Layer:Single; Size:Integer; Const Clip:ClipRect; KeepAlive:Boolean = False);
Procedure DrawPoint3D(Const P:Vector3D; MyColor:Color);

Function GetDebugDrawShader():ShaderInterface;

Procedure DrawDebug3DObjects();
Procedure DrawDebug2DObjects();
Procedure ClearTemporaryDebug3DObjects();
Procedure ClearAllDebug3DObjects();

Type
  DebugObject = Class(Renderable)
    Protected
      _KeepAlive:Boolean;
      _AlwaysOnTop:Boolean;
      _Is2D:Boolean;
    Public

      Property Is2D:Boolean Read _Is2D;
  End;

  DebugBoundingBox = Class(DebugObject)
    MyColor:Color;
    Box:BoundingBox;

    Constructor Create(Box:BoundingBox; MyColor:Color);
    Procedure Render(TranslucentPass:Boolean); Override;
    Function GetBoundingBox:BoundingBox; Override;
  End;

  DebugSpline = Class (DebugObject)
    MyColor:Color;
    S:Spline;

    Constructor Create(S:Spline; MyColor:Color);
    Procedure Render(TranslucentPass:Boolean); Override;
    Function GetBoundingBox:BoundingBox; Override;
  End;

  DebugRay = Class (DebugObject)
    MyColor:Color;
    R:Ray;
    Length:Single;

    Constructor Create(R:Ray; MyColor:Color; Length:Single);
    Procedure Render(TranslucentPass:Boolean); Override;
    Function GetBoundingBox:BoundingBox; Override;
  End;

  DebugLine = Class (DebugObject)
    MyColor:Color;
    A,B:Vector3D;
    Thickness:Single;

    Constructor Create(A,B:Vector3D; MyColor:Color; Thickness:Single);
    Procedure Render(TranslucentPass:Boolean); Override;
    Function GetBoundingBox:BoundingBox; Override;
  End;

  DebugRect = Class (DebugObject)
    MyColor:Color;
    A,B:Vector2D;
    Layer:Single;
    Thickness:Integer;
    Clip:ClipRect;

    Constructor Create(A,B:Vector2D; Layer:Single; MyColor:Color; Thickness:Integer; Clip:ClipRect);
    Procedure Render(TranslucentPass:Boolean); Override;
    Function GetBoundingBox:BoundingBox; Override;
  End;

//  Procedure AddDebugObject(Obj:DebugObject);

Implementation
Uses TERRA_OS, TERRA_Math, TERRA_Texture;

Var
  DebugObjects:Array Of DebugObject;
  DebugObjectCount:Integer;

Procedure DrawDebug3DObjects();
Var
  I:Integer;
Begin
  For I:=0 To Pred(DebugObjectCount) Do
  If (Not DebugObjects[I].Is2D) Then
    GraphicsManager.Instance.AddRenderable(DebugObjects[I]);
End;

Procedure DrawDebug2DObjects();
Var
  I:Integer;
Begin
  For I:=0 To Pred(DebugObjectCount) Do
    If (DebugObjects[I].Is2D) Then
      DebugObjects[I].Render(False);
End;

Procedure ClearTemporaryDebug3DObjects();
Var
  I:Integer;
Begin
  I:=0;
  While (I<DebugObjectCount) Do
  If (Not DebugObjects[I]._KeepAlive) Then
  Begin
    ReleaseObject(DebugObjects[I]);
    DebugObjects[I] := DebugObjects[Pred(DebugObjectCount)];
    Dec(DebugObjectCount);
  End Else
    Inc(I);
End;

Procedure ClearAllDebug3DObjects();
Var
  I:Integer;
Begin
  For I:=0 To Pred(DebugObjectCount) Do
    ReleaseObject(DebugObjects[I]);
    
  DebugObjectCount := 0;
End;

Procedure AddDebugObject(Obj:DebugObject; KeepAlive, AlwaysOnTop:Boolean);
Begin
  Obj._AlwaysOnTop := AlwaysOnTop;
  Obj._KeepAlive := KeepAlive;


  Inc(DebugObjectCount);
  SetLength(DebugObjects, DebugObjectCount);
  DebugObjects[Pred(DebugObjectCount)] := Obj;
End;

Var
  _Shader:ShaderInterface;

Function MyColorShader():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying lowp vec4 color;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  color = terra_color;}');
  Line('}');
  Line('fragment {');
	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  void main()	{');
  Line('    lowp vec4 c = color;');
  Line('    gl_FragColor = c;}');
  Line('}  ');
  Result := S;
End;


Function GetDebugDrawShader():ShaderInterface;
Begin
  If (_Shader = Nil) Then
  Begin
    _Shader := GraphicsManager.Instance.Renderer.CreateShader();
    _Shader.Generate('debugdraw', MyColorShader()); 
  End;

  Result := _Shader;
End;

Procedure DrawBoundingBox(Const MyBox:BoundingBox; Color:TERRA_Color.Color; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Begin
  AddDebugObject(DebugBoundingBox.Create(MyBox, Color), KeepAlive, AlwaysOnTop);
End;

Procedure DrawSpline(Const S:Spline; Color:TERRA_Color.Color; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Begin
  If (S=Nil) Then
    Exit;

  AddDebugObject(DebugSpline.Create(S, Color), KeepAlive, AlwaysOnTop);
End;

Procedure DrawFrustum(Const MyFrustum:Frustum; Color:TERRA_Color.Color);
Var
  _Vertices:BoundingBoxVertices;
Begin
{$IFDEF PC}
  GraphicsManager.Instance.EnableColorShader(Color, Matrix4x4Identity);

  _Vertices := MyFrustum.Vertices;

(*	glLineWidth(2.0);
	glColor4f(1.0, 1.0, 1.0, 1.0);

	glBegin(GL_LINE_LOOP);
	glVertex3f(_Vertices[2].X, _Vertices[2].Y, _Vertices[2].z);
	glVertex3f(_Vertices[1].X, _Vertices[1].Y, _Vertices[1].z);
  glVertex3f(_Vertices[3].X, _Vertices[3].Y, _Vertices[3].z);
  glVertex3f(_Vertices[4].X, _Vertices[4].Y, _Vertices[4].z);
	glEnd();

	glBegin(GL_LINE_LOOP);
	glVertex3f(_Vertices[6].X, _Vertices[6].Y, _Vertices[6].z);
	glVertex3f(_Vertices[5].X, _Vertices[5].Y, _Vertices[5].z);
  glVertex3f(_Vertices[7].X, _Vertices[7].Y, _Vertices[7].z);
  glVertex3f(_Vertices[8].X, _Vertices[8].Y, _Vertices[8].z);
	glEnd();

	glBegin(GL_LINE_LOOP);
	glVertex3f(_Vertices[1].X, _Vertices[1].Y, _Vertices[1].z);
	glVertex3f(_Vertices[3].X, _Vertices[3].Y, _Vertices[3].z);
  glVertex3f(_Vertices[7].X, _Vertices[7].Y, _Vertices[7].z);
  glVertex3f(_Vertices[5].X, _Vertices[5].Y, _Vertices[5].z);
	glEnd();

	glBegin(GL_LINE_LOOP);
	glVertex3f(_Vertices[2].X, _Vertices[2].Y, _Vertices[2].z);
	glVertex3f(_Vertices[4].X, _Vertices[4].Y, _Vertices[4].z);
  glVertex3f(_Vertices[8].X, _Vertices[8].Y, _Vertices[8].z);
  glVertex3f(_Vertices[6].X, _Vertices[6].Y, _Vertices[6].z);
	glEnd();

  BIBI
*)
{$ENDIF}
End;

Procedure DrawRay(Const MyRay:Ray; Color:Color; Length:Single=0; KeepAlive:Boolean = False; AlwaysOnTop:Boolean=False);
Begin
  AddDebugObject(DebugRay.Create(MyRay, Color, Length), KeepAlive, AlwaysOnTop);
End;

Procedure DrawLine3D(A,B:Vector3D; MyColor:Color; Thickness:Single; KeepAlive:Boolean; AlwaysOnTop:Boolean);
Begin
  AddDebugObject(DebugLine.Create(A, B, MyColor, Thickness), KeepAlive, AlwaysOnTop);
End;

Procedure DrawPoint3D(Const P:Vector3D; MyColor:Color);
Begin
{$IFDEF PC}
(*  glDepthMask(True);
  glDepthRange(0,0.0001);                 
  GraphicsManager.Instance.EnableColorShader(Color, Matrix4x4Identity);
  GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);
	glLineWidth(2.0); 
  glPointSize(10);
  glBegin(GL_POINTS);
  glVertex3f(P.X, P.Y, P.Z);
  glEnd();
  glDepthMask(True);                      
  glDepthRange(0,1);
  BIBI*)
{$ENDIF}
End;


Procedure DrawSkeleton(Const MySkeleton:MeshSkeleton; MyColor:Color; State:AnimationState; Transform:Matrix4x4; DrawJoints, UseBoneColors:Boolean);
Var
  Bone:MeshBone;
  A,B:Vector3D;
  I,J:Integer;
Begin
{$IFDEF PC}
(*  GraphicsManager.Instance.EnableColorShader(MyColor, Transform);
	glLineWidth(2.0);

  glDepthMask(True);
  glDepthRange(0,0.0001);

  GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);

  For I:=0 To Pred(MySkeleton.BoneCount) Do
  Begin
    Bone := MySkeleton.GetBone(I);
    If (Bone.Parent = Nil) Then
      Continue;

    A := State.Transforms[Bone.Index+1].Transform(VectorZero);
    B := State.Transforms[Bone.Parent.Index+1].Transform(VectorZero);

    If (UseBoneColors) Then
      ShaderManager.Instance.ActiveShader.SetUniform('out_color', Bone.Color);

  	glBegin(GL_LINE_STRIP);
  	glVertex3f(A.x, A.y, A.z);
  	glVertex3f(B.x, B.y, B.z);
  	glEnd();
  End;

  If Not DrawJoints Then
    Exit;

  glPointSize(10);
  ShaderManager.Instance.ActiveShader.SetUniform('out_color', ColorWhite);
  glBegin(GL_POINTS);
  For I:=0 To Pred(MySkeleton.BoneCount) Do
  Begin
    Bone := MySkeleton.GetBone(I);
    If (Bone.Selected) Then
      Continue;
    A := State.Transforms[Bone.Index+1].Transform(VectorZero);
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd;

  ShaderManager.Instance.ActiveShader.SetUniform('out_color', ColorRed);
  glBegin(GL_POINTS);
  For I:=0 To Pred(MySkeleton.BoneCount) Do
  Begin
    Bone := MySkeleton.GetBone(I);
    If (Not Bone.Selected) Then
      Continue;
    A := State.Transforms[Bone.Index+1].Transform(VectorZero);
    glVertex3f(A.X, A.Y, A.Z);
  End;
  glEnd;

  GraphicsManager.Instance.Renderer.SetBlendMode(blendNone);
  glDepthMask(True);                      
  glDepthRange(0,1);
  BIBI
  *)
{$ENDIF}
End;

Procedure DrawBone(Const MySkeleton:MeshSkeleton; BoneID:Integer; Color:Color; State:AnimationState; Transform:Matrix4x4; Width:Single);
Var
  Bone:MeshBone;
  A,B:Vector3D;
  I,J:Integer;
Begin
  Bone := MySkeleton.GetBone(BoneID);
  If (Bone.Parent = Nil) Then
    Exit;

{$IFDEF PC}
(*
  GraphicsManager.Instance.EnableColorShader(Color, Transform);
	glLineWidth(Width);

  glDepthMask(True);                      
  glDepthRange(0,0.0001);                 

  A := State.Transforms[Bone.Index+1].Transform(VectorZero);
  B := State.Transforms[Bone.Parent.Index+1].Transform(VectorZero);

  glBegin(GL_LINE_STRIP);
  glVertex3f(A.x, A.y, A.z);
  glVertex3f(B.x, B.y, B.z);
  glEnd();

  glPointSize(10);
  glBegin(GL_POINTS);
  glVertex3f(A.X, A.Y, A.Z);
  glEnd;

  glDepthMask(True);                      
  glDepthRange(0,1);
  BIBI*)                      
{$ENDIF}
End;

Procedure DrawPlane(Const Position, Normal:Vector3D; Scale:Single; Color:TERRA_Color.Color);
Var
  U,V:Vector3D;
  A,B,C,D:Vector3D;
Begin
  If (Abs(Normal.Y)>Abs(Normal.X)) And (Abs(Normal.Y)>Abs(Normal.Z)) Then
  Begin
    U := VectorCreate(Normal.X, Normal.Z, Normal.Y);
  End Else
  Begin
    U := VectorCreate(Normal.Z, Normal.Y, Normal.X);
  End;
  V := VectorCross(Normal, U);

  A := VectorAdd(VectorScale(U, Scale), VectorScale(V, Scale));
  B := VectorAdd(VectorScale(U, -Scale), VectorScale(V, Scale));
  C := VectorAdd(VectorScale(U, -Scale), VectorScale(V, -Scale));
  D := VectorAdd(VectorScale(U, Scale), VectorScale(V, -Scale));

{$IFDEF PC}
  GraphicsManager.Instance.EnableColorShader(Color, Matrix4x4Translation(Position));

(*  	glBegin(GL_QUADS);
  	glVertex3f(A.x, A.y, A.z);
  	glVertex3f(B.x, B.y, B.z);
  	glVertex3f(C.x, C.y, C.z);
  	glVertex3f(D.x, D.y, D.z);
  	glEnd();
    BIBI*)
{$ENDIF}
End;

Type
  Vertex2D = Packed Record
    Position:Vector3D;
    Color:TERRA_Color.Color;
  End;


Procedure DrawPolygon2D(Poly:Polygon2D; MyColor:Color; Layer:Single);
Var
  I,K:Integer;
Begin
  If (Poly.VertexCount<=0) Then
    Exit;
  {$IFDEF PC}

(*  glBegin(GL_LINE_STRIP);
    For I:=0 To (Poly.VertexCount) Do
    Begin
      If (I<Poly.VertexCount) Then
        K := I
      Else
        K := 0;
      glColor4ub(MyColor.R, MyColor.G, MyColor.B, MyColor.A);
      glVertex3f(Poly.Vertices[K].X , Poly.Vertices[K].Y , -Layer);
    End;
  glEnd;
  BIBI *)
  {$ENDIF}
End;

Procedure DrawLine2D(P1,P2:Vector2D; MyColor:Color; Layer, Width:Single; Pattern:Integer);
Var
  MyShader:ShaderInterface;
  PositionHandle, ColorHandle:Integer;
Begin
  {$IFDEF PC}
(*	glLineWidth(Width);

  If (Pattern<>0) Then
  Begin
    glLineStipple(1, Pattern);
    glEnable(GL_LINE_STIPPLE);
  End;

  MyShader := GetDebugDrawShader();
  ShaderManager.Instance.Bind(MyShader);

  PositionHandle := MyShader.GetAttribute('terra_position');
  ColorHandle := MyShader.GetAttribute('terra_color');

  MyShader.SetUniform('projectionMatrix', GraphicsManager.Instance.ProjectionMatrix);
  GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);

  glBegin(GL_LINE_STRIP);
    glVertexAttrib4ubv(ColorHandle, @MyColor);
    glVertexAttrib3f(PositionHandle, P1.X, P1.Y, -Layer);
    glVertexAttrib3f(PositionHandle, P2.X, P2.Y, -Layer);
  glEnd();

  If (Pattern<>0) Then
    glDisable(GL_LINE_STIPPLE);
   BIBI *)
  {$ENDIF}
End;

Procedure DrawPoint2D(Const P:Vector2D; MyColor:Color; Layer:Single; Size:Integer; Const Clip:ClipRect; KeepAlive:Boolean);
Var
  L:Line2D;
Begin
  L.P1.X := P.X-Size*0.5;
  L.P1.Y := P.Y-Size*0.5;
  L.P2.X := P.X+Size*0.5;
  L.P2.Y := P.Y+Size*0.5;
  DrawRect(L, MyColor, Layer, 1, Clip, KeepAlive);
End;

Procedure DrawRect(X1,Y1,X2,Y2:Single; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect; KeepAlive:Boolean);
Var
  L:Line2D;
Begin
  L.P1.X := X1;
  L.P1.Y := Y1;
  L.P2.X := X2;
  L.P2.Y := Y2;
  DrawRect(L, MyColor, Layer, Width, Clip, KeepAlive);
End;

Procedure DrawRect(Line:Line2D; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect; KeepAlive:Boolean);
Begin
  AddDebugObject(DebugRect.Create(Line.P1, Line.P2, Layer, MyColor, Width, Clip), KeepAlive, False);
End;

Procedure DrawFilledRect(X1,Y1,X2,Y2:Single; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect);
Var
  L:Line2D;
Begin
  L.P1.X := X1;
  L.P1.Y := Y1;
  L.P2.X := X2;
  L.P2.Y := Y2;
  DrawRect(L, MyColor, Layer, Width, Clip);
End;

Procedure DrawFilledRect(Line:Line2D; MyColor:Color; Layer:Single; Width:Integer; Const Clip:ClipRect);
Var
  I:Integer;
  MyShader:ShaderInterface;
  PositionHandle, ColorHandle:Integer;
  Min,Max:Vector2D;
  Vertices:Array[0..5] Of Vertex2D;
  Procedure EmitVertex(Index:Integer; X,Y,Z:Single);
  Begin
    Vertices[Index].Position := VectorCreate(X, Y, Z);
    Vertices[Index].Color := MyColor;
  End;
Begin
  {$IFDEF PC}
(*  MyShader := GetDebugDrawShader();
  ShaderManager.Instance.Bind(MyShader);

  PositionHandle := MyShader.GetAttribute('terra_position');
  ColorHandle := MyShader.GetAttribute('terra_color');

  MyShader.SetUniform('projectionMatrix', GraphicsManager.Instance.ProjectionMatrix);
  GraphicsManager.Instance.Renderer.SetBlendMode(blendBlend);

  glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(Vertex2D), @(Vertices[0].Position));
  glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(Vertex2D), @(Vertices[0].Color));

  For I:=0 To Pred(Width) Do
  Begin
    Min.X := FloatMin(Line.P1.X, Line.P2.X) + I;
    Min.Y := FloatMin(Line.P1.Y, Line.P2.Y) + I;
    Max.X := FloatMax(Line.P1.X, Line.P2.X) - I;
    Max.Y := FloatMax(Line.P1.Y, Line.P2.Y) - I;

    EmitVertex(0, Min.X, Max.Y, -Layer);
    EmitVertex(1, Max.X, Max.Y, -Layer);
    EmitVertex(2, Max.X, Min.Y, -Layer);
    EmitVertex(3, Max.X, Min.Y, -Layer);
    EmitVertex(4, Min.X, Min.Y, -Layer);
    EmitVertex(5, Min.X, Max.Y, -Layer);

    glDrawArrays(GL_TRIANGLES, 0, 6);
  End;
  BIBI *)
  {$ENDIF}
End;

Procedure DrawPointCloud(Cloud:PointCloud2D; MyColor:Color; Layer:Single);
Var
  I:Integer;
Begin
  {$IFDEF PC}
(*  glPointSize(3.0);

  glBegin(GL_POINTS);
  glColor4ub(MyColor.R, MyColor.G, MyColor.B, MyColor.A);
  For I:=0 To Pred(Cloud.PointCount) Do
  Begin
    glVertex3f(Cloud.Points[I].X, Cloud.Points[I].Y, -Layer);
  End;
  glEnd();
  BIBI *)
  {$ENDIF}
End;

{ DebugBoundingBox }
Constructor DebugBoundingBox.Create(Box: BoundingBox; MyColor: Color);
Begin
  Inherited Create;
  Self.Box := Box;
  Self.MyColor := MyColor;
End;

Function DebugBoundingBox.GetBoundingBox: BoundingBox;
Begin
  Result := Box;
End;

Procedure DebugBoundingBox.Render;
Var
  MyShader:ShaderInterface;
  Position:Integer;
  Min,Max:Vector2D;

  {$IFDEF PC}
  Procedure EmitVertex(X,Y,Z:Single);
  Begin
    // glVertexAttrib4f(Position, X, Y, Z, 1.0); BIBI
  End;
  {$ENDIF}
Begin
  {$IFDEF PC}
(*  If (_AlwaysOnTop) Then
    glDisable(GL_DEPTH_TEST);
  MyShader := GraphicsManager.Instance.EnableColorShader(MyColor, Matrix4x4Identity);
  Position := MyShader.GetAttribute('terra_position');

	glLineWidth(2.0); 

  With Self.Box Do
  Begin
	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, StartVertex.y, StartVertex.z);
  EmitVertex(EndVertex.x, StartVertex.y, StartVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, StartVertex.y, StartVertex.z);
  EmitVertex(StartVertex.x, StartVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(EndVertex.x, StartVertex.y, StartVertex.z);
  EmitVertex(EndVertex.x, StartVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, StartVertex.y, EndVertex.z);
  EmitVertex(EndVertex.x, StartVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, EndVertex.y, StartVertex.z);
  EmitVertex(EndVertex.x, EndVertex.y, StartVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, EndVertex.y, StartVertex.z);
  EmitVertex(StartVertex.x, EndVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(EndVertex.x, EndVertex.y, StartVertex.z);
  EmitVertex(EndVertex.x, EndVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, EndVertex.y, EndVertex.z);
  EmitVertex(EndVertex.x, EndVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, StartVertex.y, StartVertex.z);
  EmitVertex(StartVertex.x, EndVertex.y, StartVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(EndVertex.x, StartVertex.y, StartVertex.z);
  EmitVertex(EndVertex.x, EndVertex.y, StartVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(StartVertex.x, StartVertex.y, EndVertex.z);
  EmitVertex(StartVertex.x, EndVertex.y, EndVertex.z);
	glEnd();

	glBegin(GL_LINE_STRIP);
  EmitVertex(EndVertex.x, StartVertex.y, EndVertex.z);
  EmitVertex(EndVertex.x, EndVertex.y, EndVertex.z);
	glEnd();
  End;

  If (_AlwaysOnTop) Then
    glEnable(GL_DEPTH_TEST);

    BIBI
    *)
{$ENDIF}
End;

{ DebugSpline }
Constructor DebugSpline.Create(S: Spline; MyColor: Color);
Begin
  Inherited Create;
  Self.S := S;
  Self.MyColor := MyColor;
End;

Function DebugSpline.GetBoundingBox: BoundingBox;
Var
  I:Integer;
Begin
  Result.StartVertex := VectorZero;
  Result.EndVertex := VectorZero;
  Result.Reset();
  For I:=0 To Pred(S.PointCount) Do
    Result.Add(S.GetPointByIndex(I));
End;

procedure DebugSpline.Render;
Var
  I:Integer;
  P:Vector3D;
  T, Step:Single;
  MyShader:ShaderInterface;
  Position:Integer;
Begin
  If (S.PointCount<=1) Then
    Exit;

  S.Update();

{$IFDEF PC}
(*	Step := 0.01;

  If (_AlwaysOnTop) Then
    glDisable(GL_DEPTH_TEST);

  MyShader := GraphicsManager.Instance.EnableColorShader(MyColor, Matrix4x4Identity);
  Position := MyShader.GetAttribute('terra_position');

  glBegin(GL_LINE_STRIP);
  T := 0.0;
  Repeat
    P := S.GetPosition(T);
    glVertexAttrib3f(Position, p.x, p.y, p.z);
    T := T + Step;
  Until (T>=1.0);
  glEnd();

  If (_AlwaysOnTop) Then
    glEnable(GL_DEPTH_TEST);

    BIBI
    *)

(* TODO - replace quadrics

  Q := gluNewQuadric();

  For I:=0 To Pred(_PointCount) Do
  Begin
    Shader.SetUniform('modelMatrix', MatrixTranslation(_Points[I].Position));
    gluSphere(Q, 0.2, 8, 8);
  End;

	gluDeleteQuadric(Q);
  *)
{$ENDIF}
End;

{ DebugRay }

Constructor DebugRay.Create(R: Ray; MyColor: Color; Length:Single);
Begin
  Inherited Create;
  Self.R := R;
  Self.MyColor := MyColor;
  If Length<=0 Then
    Length := 1000.0;
  Self.Length := Length;
End;

Function DebugRay.GetBoundingBox: BoundingBox;
Begin
  Result.StartVertex := VectorZero;
  Result.EndVertex := VectorZero;
  Result.Reset();
  Result.Add(R.Origin);
  Result.Add(R.IntersectionPoint(Self.Length));
  FloatToString(Result.StartVertex.X);
End;

Procedure DebugRay.Render;
Var
  P:Vector3D;
  Position:Integer;
  MyShader:ShaderInterface;
Begin
  {$IFDEF PC}
  (*
  If (_AlwaysOnTop) Then
    glDisable(GL_DEPTH_TEST);

  MyShader := GraphicsManager.Instance.EnableColorShader(MyColor, Matrix4x4Identity);
  Position := MyShader.GetAttribute('terra_position');

	glLineWidth(2.0);

  P := VectorAdd(R.Origin, VectorScale(R.Direction, Length));

	glBegin(GL_LINE_STRIP);
	glVertexAttrib4f(Position, R.Origin.x, R.Origin.y, R.Origin.z, 1.0);
	glVertexAttrib4f(Position, P.x, P.y, P.z, 1.0);
	glEnd();

  If (_AlwaysOnTop) Then
    glEnable(GL_DEPTH_TEST);

    BIBI
    *)
  {$ENDIF}
End;

{ DebugLine }
Constructor DebugLine.Create(A, B: Vector3D; MyColor: Color; Thickness:Single);
Begin
  Self.A := A;
  Self.B := B;
  Self.MyColor := MyColor;
  Self.Thickness := Thickness;
End;

Function DebugLine.GetBoundingBox: BoundingBox;
Begin
  Result.StartVertex := VectorZero;
  Result.EndVertex := VectorZero;
  Result.Reset;
  Result.Add(A);
  Result.Add(B);
End;

Procedure DebugLine.Render;
Var
  Position:Integer;
  MyShader:ShaderInterface;
Begin
  {$IFDEF PC}
(*  If (_AlwaysOnTop) Then
    glDisable(GL_DEPTH_TEST);

  MyShader := GraphicsManager.Instance.EnableColorShader(MyColor, Matrix4x4Identity);
  Position := MyShader.GetAttribute('terra_position');

	glLineWidth(Self.Thickness);

  glBegin(GL_LINE_STRIP);
    glColor4ub(MyColor.R, MyColor.G, MyColor.B, MyColor.A);
	  glVertexAttrib4f(Position, A.x, A.y, A.z, 1.0);
	  glVertexAttrib4f(Position, B.x, B.y, B.z, 1.0);
  glEnd();

  If (_AlwaysOnTop) Then
    glEnable(GL_DEPTH_TEST);

    BIBI*)
  {$ENDIF}
End;

{ DebugRect }
Constructor DebugRect.Create(A, B: Vector2D; Layer:Single; MyColor: Color; Thickness:Integer; Clip:ClipRect);
Begin
  Self.A := A;
  Self.B := B;
  Self.Layer := Layer;
  Self.MyColor := MyColor;
  Self.Thickness := Thickness;
  Self._Is2D := True;
  Self.Clip := Clip;
End;

Function DebugRect.GetBoundingBox: BoundingBox;
Begin
  Result.StartVertex := VectorZero;
  Result.EndVertex := VectorZero;
End;

Procedure DebugRect.Render(TranslucentPass: Boolean);
Var
  I:Integer;
  Tex:Texture;
  MinX, MinY, MaxX, MaxY:Single;
  S:QuadSprite;
Begin
  Tex := TextureManager.Instance.WhiteTexture;
  If Tex = Nil Then
    Exit;

  MinX := FloatMin(A.X, B.X);
  MinY := FloatMin(A.Y, B.Y);
  MaxX := FloatMax(A.X, B.X);
  MaxY := FloatMax(A.Y, B.Y);

  S := SpriteManager.Instance.DrawSprite(MinX, MinY, Layer, Tex);
  S.SetColor(MyColor);
  S.Rect.Width := Trunc(MaxX-MinX);
  S.Rect.Height := Self.Thickness;
  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MinX, MaxY - Self.Thickness, Layer, Tex);
  S.SetColor(MyColor);
  S.Rect.Width := Trunc(MaxX-MinX);
  S.Rect.Height := Self.Thickness;
  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MinX, MinY, Layer, Tex);
  S.SetColor(MyColor);
  S.Rect.Width := Self.Thickness;
  S.Rect.Height := Trunc(MaxY-MinY);
  S.ClipRect := Clip;

  S := SpriteManager.Instance.DrawSprite(MaxX- Self.Thickness, MinY, Layer, Tex);
  S.SetColor(MyColor);
  S.Rect.Width := Self.Thickness;
  S.Rect.Height := Trunc(MaxY-MinY);
  S.ClipRect := Clip;
End;

End.
