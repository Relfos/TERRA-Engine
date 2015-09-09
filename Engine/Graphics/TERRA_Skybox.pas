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
 * TERRA_Skybox
 * Implements a skybox
 ***********************************************************************************************************************
}
Unit TERRA_Skybox;

{$i terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Math, TERRA_Texture, TERRA_Stream, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Color, TERRA_ShaderFactory, TERRA_Matrix4x4, TERRA_Renderer, TERRA_VertexFormat, TERRA_Viewport, TERRA_BoundingBox, TERRA_Renderable;

Type
{  SkyboxVertex = Packed Record
    Position:Vector2D;
    Normal:Vector3D;
  End;

  SkyboxFixedVertex = Packed Record
    Position:Vector3D;
    TexCoord:Vector2D;
  End;}

  TERRASkybox = Class(TERRARenderable)
    Protected
      _Cubemap:CubemapInterface;       // Skybox textures
      _Textures:Array[0..5] Of TERRATexture;
      _Color:ColorRGBA;           // Skybox color
      _Rotation:Single;

      _Vertices:TERRAVertexBuffer;


      {$IFDEF PC}
      Procedure RenderFixedPipeline();
      {$ENDIF}

    Public
      Constructor Create(SkyTexture:TERRAString);
      Procedure Release; Override;

      Procedure GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType); Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Override;

      Property Color:ColorRGBA Read _Color Write _Color;
      //Property Texture:CubemapTexture Read _Cubemap;
      Property Rotation:Single Read _Rotation Write _Rotation;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_Engine, TERRA_ResourceManager, TERRA_Log, TERRA_OS, TERRA_Camera,
  TERRA_Image, TERRA_ShaderManager;


{ Skybox }
Constructor TERRASkyBox.Create(SkyTexture:TERRAString);
Var
  I, N:Integer;
  W,H:Integer;
  S:TERRAString;
  Img:TERRAImage;
Begin
  Self._ObjectName := 'skybox';
  _Color := ColorWhite;

  _Vertices := TERRAVertexBuffer.Create([vertexFormatPosition, vertexFormatNormal], 6);

  If (Engine.Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    _Cubemap := Engine.Graphics.Renderer.CreateCubeMap();

    _Cubemap.LoadFromFile(SkyTexture);

    For I:=0 To 5 Do
      _Textures[I] := Nil;

  End Else
  Begin
    _Cubemap := Nil;
     // Assign if possible, all 6 sides
     For I:=0 To 5 Do
      _Textures[I] := Engine.Textures.GetItem(SkyTexture+'_' + CubeFaceNames[I]);
  End;
End;

Procedure TERRASkybox.GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType);
Begin
  Depth := 0;
  Layer := RenderableLayer_Skybox;
  AlphaType := Renderable_Blend;
End;

Procedure TERRASkybox.Release;
Begin
  ReleaseObject(_Vertices);
  ReleaseObject(_Cubemap);
End;

Procedure TERRASkyBox.Render(View:TERRAViewport; Const Stage:RendererStage);
Var
  I:Integer;
  CamVertices:BoundingBoxVertices;
  Projection:Matrix4x4;
  Camera:TERRACamera;
  MyShader:ShaderInterface;

  Graphics:GraphicsManager;

  Function GetNormal(V:Vector3D):Vector3D;
  Begin
    Result := Vector3D_Subtract(V, Camera.Position);
    Result.Normalize;
  End;

Begin
  Graphics := Engine.Graphics;

  If (Stage <> renderStageDiffuse) Then
    Exit;

  Graphics.Renderer.SetBlendMode(blendNone);

  {$IFDEF PC}
  If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    RenderFixedPipeline();
    Exit;
  End;
  {$ENDIF}

  If (Stage=renderStageNormal) Then
  Begin
    MyShader := Get_SkyboxNormalShader();
  End Else
  Begin
    MyShader := Get_SkyboxDiffuseShader();
  End;

  If Not MyShader.IsReady() Then
    Exit;

  Camera := View.Camera;
  If Camera = Nil Then
    Exit;

  CamVertices := Camera.Frustum.Vertices;

  _Vertices.SetVector3D(0, vertexNormal, GetNormal(CamVertices[5]));
  _Vertices.SetVector3D(0, vertexPosition, Vector3D_Create(1.0, 0.0, 0.0));
  _Vertices.CopyVertex(0, 5);

  _Vertices.SetVector3D(1, vertexNormal, GetNormal(CamVertices[7]));
  _Vertices.SetVector3D(1, vertexPosition, Vector3D_Create(1.0, 1.0, 0.0));

  _Vertices.SetVector3D(2, vertexNormal, GetNormal(CamVertices[8]));
  _Vertices.SetVector3D(2, vertexPosition, Vector3D_Create(0.0, 1.0, 0.0));
  _Vertices.CopyVertex(2, 3);

  _Vertices.SetVector3D(4, vertexNormal, GetNormal(CamVertices[6]));
  _Vertices.SetVector3D(4, vertexPosition, Vector3D_Create(0.0, 0.0, 0.0));

  Projection := Matrix4x4_Ortho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  Graphics.Renderer.BindShader(MyShader);

  // We need to disable zbuffer, otherwise other geometry wont be GraphicsManager correctly
  Graphics.Renderer.SetDepthMask(False);
	//glDepthRange(0.99, 1.0);

  Graphics.Renderer.BindSurface(_Cubemap, 0);

  MyShader.SetIntegerUniform('skyTexture', 0);
  MyShader.SetColorUniform('skyColor', _Color);

  MyShader.SetMat4Uniform('rotationMatrix', Matrix4x4_Rotation(0, _Rotation, 0));
  MyShader.SetMat4Uniform('projectionMatrix', Projection);
  MyShader.SetMat4Uniform('reflectionMatrix', Engine.Graphics.ReflectionMatrixSky);

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SkyboxVertex));
  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D, @(V[0].Position));
  Graphics.Renderer.SetAttributeSource(TERRA_NORMAL_ATTRIBUTE, typeVector3D, @(V[0].Normal));}

  Graphics.Renderer.SetVertexSource(_Vertices);
  Graphics.Renderer.DrawSource(renderTriangles, 6);

  // Restore zwriting
  Graphics.Renderer.SetDepthMask(True);
	//glDepthRange(0.0, 1.0);
End;

{$IFDEF PC}
{Var
  _SkyGeometry:Array[0..5] Of SkyboxFixedVertex;}

Procedure TERRASkybox.RenderFixedPipeline;
Var
  I,J:Integer;
  Size:Single;
  M,M2:Matrix4x4;
  Cam:TERRACamera;
  Ofs:Vector3D;
  Graphics:GraphicsManager;
Begin
(*  Graphics := GraphicsManager.Instance;

  Graphics.Renderer.SetDepthMask(False);

  Graphics.Renderer.SetCullMode(cullNone);

  //glDisable(GL_LIGHTING);

  Cam := GraphicsManager.Instance.MainViewport.Camera;

  M := GraphicsManager.Instance.MainViewport.Camera.Projection;

  Graphics.Renderer.SetProjectionMatrix(M);

  M := Cam.Transform;
  M.V[12] := 0;
  M.V[13] := 0;
  M.V[14] := 0;

  Graphics.Renderer.SetModelMatrix(M);
  Graphics.Renderer.SetTextureMatrix(Matrix4x4Identity);

  {glColor4f(1.0, 1.0, 1.0, 1.0);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);}

  Size := 100;
  Ofs := VectorScale(VectorUniform(Size), 0.5);

  // Draw all 6 skybox sides
  For I:=0 To 5 Do
  If Assigned(_Textures[I]) Then
  Begin
    _Textures[I].Bind(0);

    _SkyGeometry[0].Position := VectorCreate(0, Size, 0);
    _SkyGeometry[0].TexCoord := VectorCreate2D(0, 0);

    _SkyGeometry[1].Position := VectorCreate(Size, Size, 0);
    _SkyGeometry[1].TexCoord := VectorCreate2D(1, 0);

    _SkyGeometry[2].Position := VectorCreate(Size, 0, 0);
    _SkyGeometry[2].TexCoord := VectorCreate2D(1, 1);

    _SkyGeometry[4].Position := VectorCreate(0, 0, 0);
    _SkyGeometry[4].TexCoord := VectorCreate2D(0, 1);

    Case I Of
        1:
          For J:=0 To 5 Do
          Begin
            _SkyGeometry[J].Position := VectorCreate(Size-_SkyGeometry[J].Position.X, _SkyGeometry[J].Position.Y, Size-_SkyGeometry[J].Position.Z);
            _SkyGeometry[J].TexCoord := VectorCreate2D(_SkyGeometry[J].TexCoord.X, _SkyGeometry[J].TexCoord.Y);
          End;

        2:
          For J:=0 To 5 Do
          Begin
            _SkyGeometry[J].Position := VectorCreate(_SkyGeometry[J].Position.Y, Size-_SkyGeometry[J].Position.Z, _SkyGeometry[J].Position.X);
            _SkyGeometry[J].TexCoord := VectorCreate2D(1-_SkyGeometry[J].TexCoord.X, _SkyGeometry[J].TexCoord.Y);
          End;

        3:
          For J:=0 To 5 Do
          Begin
            _SkyGeometry[J].Position := VectorCreate(_SkyGeometry[J].Position.Y, _SkyGeometry[J].Position.Z, _SkyGeometry[J].Position.X);
            _SkyGeometry[J].TexCoord := VectorCreate2D(1-_SkyGeometry[J].TexCoord.X, _SkyGeometry[J].TexCoord.Y);
          End;

        4:
          For J:=0 To 5 Do
          Begin
            _SkyGeometry[J].Position := VectorCreate(_SkyGeometry[J].Position.Z, _SkyGeometry[J].Position.Y, _SkyGeometry[J].Position.X);
            _SkyGeometry[J].TexCoord := VectorCreate2D(1-_SkyGeometry[J].TexCoord.X, _SkyGeometry[J].TexCoord.Y);
          End;

        5:
          For J:=0 To 5 Do
          Begin
            _SkyGeometry[J].Position := VectorCreate(Size-_SkyGeometry[J].Position.Z, _SkyGeometry[J].Position.Y, Size-_SkyGeometry[J].Position.X);
            _SkyGeometry[J].TexCoord := VectorCreate2D(1-_SkyGeometry[J].TexCoord.X, _SkyGeometry[J].TexCoord.Y);
          End;
    End;

    _SkyGeometry[3] := _SkyGeometry[2];
    _SkyGeometry[5] := _SkyGeometry[0];

    For J:=0 To 5 Do
      _SkyGeometry[J].Position.Subtract(Ofs);

{    glVertexPointer(3, GL_FLOAT, SizeOf(SkyboxFixedVertex), @(_SkyGeometry[0].Position));
    glTexCoordPointer(2, GL_FLOAT, SizeOf(SkyboxFixedVertex), @(_SkyGeometry[0].TexCoord));
    glDrawArrays(GL_TRIANGLES, 0, 6);}
  End;

{  GraphicsManager.Instance.Internal(0 , 12);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);}

  // Restore zwriting
  Graphics.Renderer.SetDepthMask(True);*)
End;
{$ENDIF}


End.
