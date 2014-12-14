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
  TERRA_Utils, TERRA_Math, TERRA_Texture, TERRA_IO, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Color, TERRA_Shader, TERRA_ShaderFactory,TERRA_Matrix, TERRA_Cubemap;

Type
  SkyboxVertex = Packed Record
    Position:Vector2D;
    Normal:Vector3D;
  End;

  SkyboxFixedVertex = Packed Record
    Position:Vector3D;
    TexCoord:Vector2D;
  End;

  Skybox = Class(TERRAObject)
    Protected
      _Cubemap:CubemapTexture;       // Skybox textures
      _Textures:Array[0..5] Of Texture;
      _Color:Color;           // Skybox color
      _Rotation:Single;

      {$IFDEF PC}
      Procedure RenderFixedPipeline();
      {$ENDIF}

    Public
      Constructor Create(SkyTexture:AnsiString);
      Destructor Destroy; Override;

      Procedure Render(BlendMode:Integer = 0);  // Renders the skybox

      Property Color:TERRA_Color.Color Read _Color Write _Color;
      //Property Texture:CubemapTexture Read _Cubemap;
      Property Rotation:Single Read _Rotation Write _Rotation;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_ResourceManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Log,
  TERRA_OS, TERRA_Camera, TERRA_BoundingBox, TERRA_Viewport;

Var
  _SkyboxShader :Shader = Nil;
  _SkyboxNormalShader :Shader = Nil;

(*Function GetShader_Skybox:AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
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

Function GetShader_Skybox(OutputMode:Integer):AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
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
  End;
  Line('}');
  Result := S;
End;

{ Skybox }
Constructor SkyBox.Create(SkyTexture:AnsiString);
Var
  I, N:Integer;
  S:AnsiString;
Begin
  _Color := ColorWhite;

  If (GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    _Cubemap := CubemapTexture.Create(SkyTexture);

    For I:=0 To 5 Do
      _Textures[I] := Nil;

  End Else
  Begin
    _Cubemap := Nil;
     // Assign if possible, all 6 sides
     For I:=0 To 5 Do
      _Textures[I] := TextureManager.Instance.GetTexture(SkyTexture+'_' + CubeFaceNames[I]);
  End;
End;

// Renders the skybox
Destructor Skybox.Destroy;
Begin
  If Assigned(_Cubemap) Then
    _Cubemap.Destroy;
End;

Procedure SkyBox.Render(BlendMode:Integer);
Var
  I:Integer;
  Vertices:BoundingBoxVertices;
  Projection:Matrix;
  V:Array[0..6] Of SkyboxVertex;
  Camera:TERRA_Camera.Camera;
  PositionHandle:Integer;
  NormalHandle:Integer;
  MyShader:Shader;
  View:Viewport;


  Function GetNormal(V:Vector3D):Vector3D;
  Begin
    Result := VectorSubtract(V, Camera.Position);
    Result.Normalize;
  End;

Begin
  GraphicsManager.Instance.SetBlendMode(BlendMode);

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    RenderFixedPipeline();
    Exit;
  End;
  {$ENDIF}

  If (GraphicsManager.Instance.RenderStage=renderStageNormal) Then
  Begin
    If (_SkyboxNormalShader = Nil) Then
    Begin
      _SkyboxNormalShader := Shader.CreateFromString(GetShader_Skybox(shader_OutputNormal), 'skybox_normal');
      ShaderManager.Instance.AddShader(_SkyboxNormalShader);
    End;
    MyShader := _SkyboxNormalShader;
  End Else
  Begin
    If (_SkyboxShader = Nil) Then
    Begin
      _SkyboxShader := Shader.CreateFromString(GetShader_Skybox(0), 'skybox');
      ShaderManager.Instance.AddShader(_SkyboxShader);
    End;
    MyShader := _SkyboxShader;
  End;

  If Not MyShader.IsReady() Then
    Exit;

  View := GraphicsManager.Instance.ActiveViewport;
  If View = Nil Then
    Exit;

  Camera := View.Camera;
  If Camera = Nil Then
    Exit;

  Vertices := Camera.Frustum.Vertices;

  Projection := MatrixOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  V[0].Normal := GetNormal(Vertices[5]);
	V[0].Position := VectorCreate2D( 1.0, 0.0);
  V[5] := V[0];

  V[1].Normal := GetNormal(Vertices[7]);
  V[1].Position := VectorCreate2D(1.0, 1.0);

  V[2].Normal := GetNormal(Vertices[8]);
  V[2].Position := VectorCreate2D(0.0, 1.0);
  V[3] := V[2];

  V[4].Normal := GetNormal(Vertices[6]);
  V[4].Position := VectorCreate2D( 0.0, 0.0);


  ShaderManager.Instance.Bind(MyShader);

  PositionHandle := MyShader.GetAttribute('terra_position');
  If (PositionHandle<0) Then
    Exit;

  NormalHandle :=   MyShader.GetAttribute( 'terra_normal');
  If (NormalHandle<0) Then
    Exit;

  // We need to disable zbuffer, otherwise other geometry wont be GraphicsManager correctly
  glDepthMask(False);
	//glDepthRange(0.99, 1.0);

  CubeMapTexture.Bind(_Cubemap);

  MyShader.SetUniform('skyTexture', 0);
  MyShader.SetUniform('skyColor', _Color);

  MyShader.SetUniform('rotationMatrix', MatrixRotation(0, _Rotation, 0));
  MyShader.SetUniform('projectionMatrix', Projection);
  MyShader.SetUniform('reflectionMatrix', GraphicsManager.Instance.ReflectionMatrixSky);

  glVertexAttribPointer(PositionHandle, 2, GL_FLOAT, False, SizeOf(SkyboxVertex), @(V[0].Position));
  glVertexAttribPointer(NormalHandle, 3, GL_FLOAT, False, SizeOf(SkyboxVertex), @(V[0].Normal));
  glDrawArrays(GL_TRIANGLES, 0, 6);

  CubeMapTexture.Bind(Nil);

  // Restore zwriting
  glDepthMask(True);
	//glDepthRange(0.0, 1.0);
End;

{$IFDEF PC}
Var
  _SkyGeometry:Array[0..5] Of SkyboxFixedVertex;

Procedure Skybox.RenderFixedPipeline;
Var
  I,J:Integer;
  Size:Single;
  M,M2:Matrix;
  Cam:Camera;
  Ofs:Vector3D;
Begin
  glDepthMask(False);
  glDisable(GL_CULL_FACE);

  glDisable(GL_LIGHTING);

  Cam := GraphicsManager.Instance.MainViewport.Camera;

  M := GraphicsManager.Instance.MainViewport.Camera.Projection;
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@M);

  M := Cam.Transform;
  M.V[12] := 0;
  M.V[13] := 0;
  M.V[14] := 0;
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@M);

  M := MatrixIdentity;
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixf(@M);

  glColor4f(1.0, 1.0, 1.0, 1.0);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);

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

    glVertexPointer(3, GL_FLOAT, SizeOf(SkyboxFixedVertex), @(_SkyGeometry[0].Position));
    glTexCoordPointer(2, GL_FLOAT, SizeOf(SkyboxFixedVertex), @(_SkyGeometry[0].TexCoord));
    glDrawArrays(GL_TRIANGLES, 0, 6);
  End;

  GraphicsManager.Instance.Internal(0 , 12);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  // Restore zwriting
  glDepthMask(True);
End;
{$ENDIF}


End.
