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
 * TERRA_LightingParticles
 * Implements lighting rays rendering
 ***********************************************************************************************************************
}
Unit TERRA_LightingParticles;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
    TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_BoundingBox, TERRA_GraphicsManager, TERRA_Texture,
    TERRA_Color, TERRA_Matrix4x4, TERRA_VertexFormat, TERRA_Renderable, TERRA_Viewport, TERRA_Renderer;

Type
  LightingCollection = Class(TERRARenderable)
    Protected
      _A, _B:Vector3D;
      _Mid:Vector3D;
      _SparkCount:Integer;
      _Segments:Integer;
      _Vertices:TERRAVertexBuffer;
      _Scale:Single;
      _Width:Integer;
      _Y:Array Of Single;
      _Color:ColorRGBA;
      _BoundingBox:BoundingBox;
      _LastTime:Cardinal;

      Procedure Update(View:TERRAViewport); Override;

    Public
      Constructor Create(A,B:Vector3D; Scale:Single = 5.0; Width:Integer = 1);

      Function SetPoints(A,B:Vector3D):Boolean;
      Procedure SetColor(Const Color:ColorRGBA);


      Function GetBoundingBox:BoundingBox; Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Override;
      Procedure GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType); Override;

      Property Color:ColorRGBA Read _Color Write SetColor;
  End;

Implementation
Uses TERRA_Engine, TERRA_ResourceManager, TERRA_Camera, TERRA_OS;

Var
  _LightingShader:ShaderInterface = Nil;

Constructor LightingCollection.Create(A, B: Vector3D; Scale:Single; Width:Integer);
Begin
  _Width := Width;
  _Scale := Scale;
  _SparkCount := 16;
  SetColor(ColorCreate(100, 76, 200, 120));
  SetPoints(A,B);
End;

Function LightingCollection.GetBoundingBox: BoundingBox;
Begin
  Result := _BoundingBox;
End;

Procedure LightingCollection.Render(View:TERRAViewport; Const Stage:RendererStage); 
Var
  Rnd, Rnd2:Single;
  I,J,K:Integer;
  U, V, D:Vector3D;
  Dx,Dy,Dz:Single;
  PosHandle:Integer;
  M:Matrix4x4;
  Graphics:GraphicsManager;
  MyShader:ShaderInterface;
Begin
  If (Stage <> renderStageDiffuse) And (Stage <> renderStageGlow) Then
    Exit;

  Engine.Textures.WhiteTexture.Bind(0);
  Graphics := Engine.Graphics;

  If (_Color.A<255) Then
    Graphics.Renderer.SetBlendMode(blendAdd);

(*  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Features.Shaders.Avaliable) Then
  Begin
    M := GraphicsManager.Instance.ActiveViewport.Camera.Projection;
    M := Matrix4x4Multiply4x4(M, GraphicsManager.Instance.ActiveViewport.Camera.Transform);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@M);
    M := Matrix4x4Identity;
    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@M);
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@M);
    glColor4ub(_Color.R, _Color.G, _Color.B, _Color.A);
  End;
  {$ENDIF}*)

  dx := _A.X - _B.X;
  dy := _A.Y - _B.Y;
  dz := _A.Z - _B.Z;

  //glLineWidth(_Width);

  Engine.RaiseError('TODO: lighting particles');
  //MyShader := Graphics.EnableColorShader(_Color, Matrix4x4Identity);

  For I :=1 to _SparkCount Do
  Begin
    K := 0;
    For J :=0 to Pred(_Segments) do
    Begin
      rnd := 0.08 * RandomFloat(-_Scale, _Scale);
      rnd2 := 0.08 * RandomFloat(-_Scale, _Scale);
      U.X := (_A.X - dx*J/_Segments) + rnd;
      U.Y := (_A.Y - dy*J/_Segments);
      U.Z := (_A.Z - dz*J/_Segments) + rnd2;
      V := U;

      U.Y := U.Y + 0.02 * _Scale + _y[J] + rnd;
      V.Y := V.Y - 0.02 * _Scale + _y[J] + rnd;

      _Vertices.SetVector3D(K, vertexPosition, Vector3D_Create(U.X, U.Y, U.Z)); Inc(K);
      _Vertices.SetVector3D(K, vertexPosition, Vector3D_Create(V.X, V.Y, V.Z)); Inc(K);
    End;

(*  {$IFDEF PC}
    If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
    Begin
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, SizeOf(Vector3D), @_Vertices[0]);
      glDrawArrays(GL_TRIANGLES, 0, K);
    End Else
  {$ENDIF}
      glVertexAttribPointer(PosHandle, 3, GL_FLOAT, False, SizeOf(Vector3D), @(_Vertices[0]));*)


    Graphics.Renderer.SetVertexSource(_Vertices);
    Graphics.Renderer.DrawSource( renderLineStrip, K);

    //glDrawArrays(GL_LINE_STRIP, 0, K);

(*  {$IFDEF PC}
    If (Not GraphicsManager.Instance.Features.Shaders.Avaliable) Then
    Begin
      glDisableClientState(GL_VERTEX_ARRAY);
    End;
  {$ENDIF}*)
  End;
End;


Function LightingCollection.SetPoints(A, B: Vector3D):Boolean;
Var
  Dist:Single;
Begin
  If (_A.Distance(A)<Epsilon) And (_B.Distance(B)<Epsilon) Then
  Begin
    Result := False;
    Exit;
  End;

  Dist := A.Distance(B);
  _Segments := Trunc(Dist * 5.0);
  If (_Segments<2) Then
    _Segments := 2;
  If (Length(_Y)<_Segments) Then
    SetLength(_Y, _Segments);
  _Y[0] := 0.0;
  _Y[Pred(_Segments)] := 0.0;

  _A := A;
  _B := B;

  _BoundingBox.Reset;
  _BoundingBox.Add(_A);
  _BoundingBox.Add(_B);

  If (_Vertices = Nil) Then
    _Vertices := TERRAVertexBuffer.Create([vertexFormatPosition], _Segments * 2)
  Else
  If (_Vertices.Count<_Segments * 2) Then
    _Vertices.Resize(_Segments * 2);

  Result := True;
End;

Procedure LightingCollection.SetColor(Const Color:ColorRGBA);
Begin
  _Color := Color;
End;

Procedure LightingCollection.Update;
Var
  I:Integer;
  Time:Cardinal;
  Delta:Single;
Begin
  If (Application.Instance.Paused) Then
    Exit;

  {Time := Application.Instance.GetTime;
  Delta := Time - _LastTime;
  Delta := Delta * 0.15;
  _LastTime := Time;}
  Delta := _Scale * 0.15;

  // calculate new Y coordinate. new = old + random.
  For I :=1 to _Segments-2 do
  Begin
    _Y[I] :=_Y[I] + RandomFloat(-_Scale, _Scale);
    If _Y[I] > _Y[I-1] + Delta Then _Y[I] := _Y[I-1] + Delta;
    If _Y[I] < _Y[I-1] - Delta Then _Y[I] := _Y[I-1] - Delta;
    If _Y[I] > _Y[I+1] + Delta Then _Y[I] := _Y[I+1] + Delta;
    If _Y[I] < _Y[I+1] - Delta Then _Y[I] := _Y[I+1] - Delta;
    If _Y[I] >  _Scale Then _Y[I] := _Scale;
    If _Y[I] < -_Scale Then _Y[I] :=-_Scale;
  End;
End;

Procedure LightingCollection.GetBucketDetails(View:TERRAViewport; out Depth:Cardinal; out Layer:RenderableLayer; out AlphaType:RenderableAlphaType);
Var
  Pos:Vector3D;
  Box:BoundingBox;
Begin
  If (View.Camera Is PerspectiveCamera) Then
  Begin
    Box := Self.GetBoundingBox;
    Pos := Vector3D_Add(Box.Center , Vector3D_Scale(PerspectiveCamera(View.Camera).View, -Box.Radius));
    Depth := Trunc(Pos.Distance(View.Camera.Position))
  End Else
    Depth := 0;

  AlphaType := Renderable_Blend;
  Layer := RenderableLayer_Default;
End;

End.
