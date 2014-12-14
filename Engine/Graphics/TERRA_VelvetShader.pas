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
 * TERRA_VelvetShader
 * Implements a velvet shader
 ***********************************************************************************************************************
}

Unit TERRA_VelvetShader;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Image, TERRA_Shader, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Texture, TERRA_ShaderFactory, TERRA_Color;

Type
  VelvetShader = Class(ShaderEmitter)
    Protected
    Public
      FXStart:Single;
      FXEnd:Single;
      Multiplier:Single;
      RimColor:Color;

      Procedure Init; Override;

      Procedure CustomShading(Flags:Cardinal); Override;
      Procedure FragmentUniforms(Flags:Cardinal); Override;
      Procedure Bind(Flags:Cardinal; LightCount:Integer); Override;

    Public
  End;

Implementation
Uses TERRA_Application, TERRA_OS;

{ VelvetShader }
Procedure VelvetShader.Init;
Begin
  FXStart := 0.5;
  FXEnd := 0.01;
  Multiplier := 2.0;
  RimColor := ColorRed;
End;

Procedure VelvetShader.Bind(Flags: Cardinal; LightCount: Integer);
Var
  _Shader:Shader;
Begin
  _Shader := ShaderManager.Instance.ActiveShader;
  If _Shader = Nil Then
    Exit;

  _Shader.SetUniform('Rim_Start', FXStart);
  _Shader.SetUniform('Rim_End', FXEnd);
  _Shader.SetUniform('Rim_Multiplier', Multiplier);
  _Shader.SetUniform('Rim_Color', RimColor);

  Inherited;
End;


Procedure VelvetShader.CustomShading(Flags: Cardinal);
Begin
  If (Flags And shaderOutputNormal<>0) Or (Flags And shaderOutputSpecular<>0) Or (Flags And shaderOutputGlow<>0) Then
  Begin
    Inherited;
    Exit;
  End;

  Line('mediump vec3 N = normalize(vertex_normal);');
  Line('mediump vec3 V = normalize(cameraPosition - world_position.xyz);');
  Line('mediump float rim = smoothstep(Rim_Start , Rim_End , dot(N,V));');
	Line('mediump vec3 L =  normalize(sunDirection );');
  Line('mediump float lightAmount = max(dot(N, L), 0.0);');
  Line('result.rgb = result.rgb * lightAmount * sunColor.rgb  + rim*Rim_Multiplier * Rim_Color.rgb;');
  Line('result.a = 1.0;');
End;

Procedure VelvetShader.FragmentUniforms(Flags: Cardinal);
Begin
  Inherited;

  If (Flags And shaderOutputNormal<>0) Or (Flags And shaderOutputSpecular<>0) Or (Flags And shaderOutputGlow<>0)
  Or (Flags And shaderOutputOutline<>0) Then
    Exit;

	Line('  uniform highp float Rim_Start;');
	Line('  uniform highp float Rim_End;');
	Line('  uniform highp float Rim_Multiplier;');
	Line('  uniform highp vec4 Rim_Color;');
End;


End.
