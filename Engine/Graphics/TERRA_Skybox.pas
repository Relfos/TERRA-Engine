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

      _Vertices:VertexData;


      {$IFDEF PC}
      Procedure RenderFixedPipeline();
      {$ENDIF}

    Public
      Constructor Create(SkyTexture:TERRAString);
      Procedure Release; Override;

      Function GetRenderBucket:Cardinal; Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;

      Property Color:ColorRGBA Read _Color Write _Color;
      //Property Texture:CubemapTexture Read _Cubemap;
      Property Rotation:Single Read _Rotation Write _Rotation;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_EngineManager, TERRA_ResourceManager, TERRA_Log, TERRA_OS, TERRA_Camera,
  TERRA_Image;

Var
  _SkyboxShader:ShaderInterface = Nil;
  _SkyboxNormalShader:ShaderInterface = Nil;

(*Function GetShader_Skybox:TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
	Line('  uniform mat4 cameraMatrix;');
	Line('  uniform mat4 modelMatrix;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  attribute vec4 terra_position;');
  Line('  attribute vec2 terra_UV0;');
	Line('  varying vec4 local_position;');
	Line('  varying vec4 world_position;');
	Line('void main()	{');
  Line('  local_position = terra_position;');
  Line('  world_position = modelMatrix * local_position;');
  Line('  gl_Position = gl_ProjectionMatrix * cameraMatrix * world_position;');
  Line('  gl_TexCoord[0].st = terra_UV0;	}');
  Line('}');
  Line('fragment {');
	Line('  uniform sampler2D texture;');
	Line('  uniform vec4 skyColor;');
  Line('void main(){');
  Line('  vec4 color = texture2D(texture, gl_TexCoord[0].st);');
  Line('  gl_FragColor = color * skyColor;}');
  Line('}');
  Result := S;
End;*)

Function GetShader_Skybox(OutputMode:Integer):TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('varying highp vec3 normal;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 rotationMatrix;');
  Line('  uniform mat4 reflectionMatrix;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec3 terra_normal;');
  Line('  void main()	{');
  Line('  gl_Position = projectionMatrix * terra_position;');
  Line('  highp vec4 n = reflectionMatrix * vec4(terra_normal, 1.0);');
  Line('  normal = (rotationMatrix * n).xyz;}');
  Line('}');
  Line('fragment {');
  Line('varying highp vec3 normal;');
  Line('uniform samplerCube skyTexture;');
  Line('uniform lowp vec4 skyColor;');
  Line('  void main()	{');
  Line('  highp vec3 n = normalize(normal);');
  If (OutputMode And shader_OutputNormal<>0) Then
  Begin
    Line('  n *= 0.5; n += vec3(0.5, 0.5, 0.5);');
    Line('  gl_FragColor = vec4(n, 0.0);}');
  End Else
  Begin
    Line('  lowp vec4 sky = textureCube(skyTexture, n) * skyColor; ');
    Line('  gl_FragColor = vec4(sky.rgb, 1.0);}');
    //Line('  gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0);}');
  End;

  Line('}');
  Result := S;
End;

{ Skybox }
Constructor TERRASkyBox.Create(SkyTexture:TERRAString);
Var
  I, N:Integer;
  W,H:Integer;
  S:TERRAString;
  Img:Image;
Begin
  Self._ObjectName := 'skybox';
  _Color := ColorWhite;

  _RenderFlags := renderFlagsSkipFrustum Or renderFlagsSkipSorting;


  _Vertices := VertexData.Create([vertexFormatPosition, vertexFormatNormal], 6);

  If (GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
  Begin
    _Cubemap := GraphicsManager.Instance.Renderer.CreateCubeMap();

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

Function TERRASkybox.GetRenderBucket: Cardinal;
Begin
  Result := renderBucket_Sky;
End;

Procedure TERRASkybox.Release;
Begin
  ReleaseObject(_Vertices);
  ReleaseObject(_Cubemap);
End;

Procedure TERRASkyBox.Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal);
Var
  I:Integer;
  CamVertices:BoundingBoxVertices;
  Projection:Matrix4x4;
  Camera:TERRACamera;
  MyShader:ShaderInterface;

  Graphics:GraphicsManager;

  Function GetNormal(V:Vector3D):Vector3D;
  Begin
    Result := VectorSubtract(V, Camera.Position);
    Result.Normalize;
  End;

Begin
  Graphics := GraphicsManager.Instance;

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
    If (_SkyboxNormalShader = Nil) Then
    Begin
      _SkyboxNormalShader := Graphics.Renderer.CreateShader();
      _SkyboxNormalShader.Generate('skybox_normal', GetShader_Skybox(shader_OutputNormal));
    End;
    MyShader := _SkyboxNormalShader;
  End Else
  Begin
    If (_SkyboxShader = Nil) Then
    Begin
      _SkyboxShader := Graphics.Renderer.CreateShader();
      _SkyboxShader.Generate('skybox', GetShader_Skybox(0));
    End;
    MyShader := _SkyboxShader;
  End;

  If Not MyShader.IsReady() Then
    Exit;

  Camera := View.Camera;
  If Camera = Nil Then
    Exit;

  CamVertices := Camera.Frustum.Vertices;

  _Vertices.SetVector3D(0, vertexNormal, GetNormal(CamVertices[5]));
  _Vertices.SetVector3D(0, vertexPosition, VectorCreate(1.0, 0.0, 0.0));
  _Vertices.CopyVertex(0, 5);

  _Vertices.SetVector3D(1, vertexNormal, GetNormal(CamVertices[7]));
  _Vertices.SetVector3D(1, vertexPosition, VectorCreate(1.0, 1.0, 0.0));

  _Vertices.SetVector3D(2, vertexNormal, GetNormal(CamVertices[8]));
  _Vertices.SetVector3D(2, vertexPosition, VectorCreate(0.0, 1.0, 0.0));
  _Vertices.CopyVertex(2, 3);

  _Vertices.SetVector3D(4, vertexNormal, GetNormal(CamVertices[6]));
  _Vertices.SetVector3D(4, vertexPosition, VectorCreate(0.0, 0.0, 0.0));

  Projection := Matrix4x4Ortho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  Graphics.Renderer.BindShader(MyShader);

  // We need to disable zbuffer, otherwise other geometry wont be GraphicsManager correctly
  Graphics.Renderer.SetDepthMask(False);
	//glDepthRange(0.99, 1.0);

  Graphics.Renderer.BindSurface(_Cubemap, 0);

  MyShader.SetIntegerUniform('skyTexture', 0);
  MyShader.SetColorUniform('skyColor', _Color);

  MyShader.SetMat4Uniform('rotationMatrix', Matrix4x4Rotation(0, _Rotation, 0));
  MyShader.SetMat4Uniform('projectionMatrix', Projection);
  MyShader.SetMat4Uniform('reflectionMatrix', GraphicsManager.Instance.ReflectionMatrixSky);

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
