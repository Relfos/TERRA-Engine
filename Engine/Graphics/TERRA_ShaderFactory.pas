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
 * TERRA_ShaderFactory
 * Implements a uber shader
 ***********************************************************************************************************************
}

Unit TERRA_ShaderFactory;
{$I terra.inc}

//http://kylehalladay.com/blog/tutorial/2014/02/18/Fresnel-Shaders-From-The-Ground-Up.html
//https://ericpacelli.wordpress.com/2014/04/21/realistic-water-in-opengl-and-glsl/
//https://mtnphil.wordpress.com/2012/08/25/water-flow-shader/

{-$DEFINE SIMPLESHADER}

{-$DEFINE DEBUG_LIGHTMAP}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Shader, TERRA_Application, TERRA_Lights, TERRA_BoundingBox, TERRA_Vector4D;

Const
  NormalMapUniformName = 'normalMap';
  DecalMapUniformName = 'decalMap';
  SpecularMapUniformName = 'specularMap';
  AlphaMapUniformName = 'alphaMap';
  ReflectiveMapUniformName = 'reflectiveMap';
  ReflectionMapUniformName = 'reflectionMap';
  SphereMapUniformName = 'sphereMap';
  ColorRampUniformName = 'colorRamp';
  FlowMapUniformName = 'flowMap';
  NoiseMapUniformName = 'noiseMap';

  shaderSkinning    = 1 Shl 0;
  shaderSpecular    = 1 Shl 1;
  shaderLightMap    = 1 Shl 2;
  //shaderSpecularMap = 1 Shl 3;
  shaderNormalMap   = 1 Shl 4;
  shaderCubeMap  = 1 Shl 5;
  shaderColorRamp   = 1 Shl 6;
  shaderSkipAmbient = 1 Shl 7;
  shaderScreenMask   = 1 Shl 8;
  shaderVegetation  = 1 Shl 9;
  shaderAlphaTest   = 1 Shl 10;
  shaderAddSigned   = 1 Shl 11;
  shaderDecalMap    = 1 Shl 12;
  shaderSphereMap   = 1 Shl 13;
  shaderTriplanar   = 1 Shl 14;
  shaderAlphaMap    = 1 Shl 15;
  shaderFresnelTerm = 1 Shl 16;
  shaderColorTable  = 1 Shl 17;
  shaderWireframe       = 1 Shl 18;
  shaderColorOff        = 1 Shl 19;
  shaderClipPlane       = 1 Shl 20;
  shaderGhost           = 1 Shl 21;
  shaderReflectiveMap   = 1 Shl 22;
  shaderFlowMap         = 1 Shl 23;
  shaderNoiseMap        = 1 Shl 24;
  shaderTextureMatrix   = 1 Shl 25;


  shader_OutputNormal    = 1 Shl 1;
  shader_OutputSpecular  = 1 Shl 2;
  shader_OutputGlow      = 1 Shl 3;
  shader_OutputRefraction= 1 Shl 4;
  shader_OutputOutline   = 1 Shl 5;
  shader_OutputColor     = 1 Shl 6;
  shader_OutputReflection= 1 Shl 7;

Type
  ShaderEntry = Class(TERRAObject)
    FXFlags:Cardinal;
    OutFlags:Cardinal;
    DirLightCount:Integer;
    PointLightCount:Integer;
    SpotLightCount:Integer;
    LightModel:Integer;
    FogFlags:Integer;
    Shader:TERRA_Shader.Shader;

    Procedure Release; Override;
  End;

  ShaderEmitter = Class(TERRAObject)
    Protected
      _Buffer:TERRAString;

      Procedure Line(S2:TERRAString);

      Procedure Init; Virtual;

      Procedure Varyings(FxFlags, OutFlags, FogFlags:Cardinal); Virtual;
      Procedure VertexUniforms(FxFlags, OutFlags,FogFlags:Cardinal); Virtual;
      Procedure VertexPass(FxFlags, OutFlags,FogFlags:Cardinal); Virtual;
      Procedure FragmentUniforms(FxFlags, OutFlags,FogFlags:Cardinal); Virtual;
      Procedure CustomShading(FxFlags, OutFlags,FogFlags:Cardinal); Virtual;
      Procedure FinalPass(FxFlags, OutFlags,FogFlags:Cardinal); Virtual;
      Procedure EmitReflectionCoord(FxFlags, OutFlags,FogFlags:Cardinal; Const NormalVarName:TERRAString);

      Procedure EmitDecodeBoneMat;


      Procedure Bind(FxFlags, OutFlags, FogFlags:Cardinal; Const Lights:LightBatch); Virtual;

    Public
      Function Emit(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):TERRAString;
  End;

  ShaderFactory = Class(ApplicationComponent)
    Protected
      _Shaders:Array Of ShaderEntry;
      _ShaderCount:Integer;
      _Emitter:ShaderEmitter;

    Public
      Class Function Instance:ShaderFactory;
      Procedure Release; Override;

      Procedure Init; Override;
      Procedure OnContextLost; Override;
      
      Procedure Clear;

      Procedure SetShaderEmitter(Emitter:ShaderEmitter);

      Function GetShader(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):Shader;
  End;

Implementation
Uses TERRA_Mesh, TERRA_GraphicsManager, TERRA_ColorGrading, TERRA_OS,
  {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};
Var
  _ShaderFactory_Instance:ApplicationObject;


{ ShaderEmitter }
Procedure ShaderEmitter.Line(S2:TERRAString);
Begin
  _Buffer := _Buffer + S2 + crLf;
End;

Procedure ShaderEmitter.Varyings;
Begin
	Line('varying highp vec4 world_position;');
	Line('varying highp vec4 local_position;');
	Line('varying highp vec4 screen_position;');
  Line('varying lowp vec4 vertex_color;');

	Line('varying highp vec4 texCoord0;');

  If (FxFlags and shaderLightmap<>0) Or (FxFlags And shaderAlphaMap<>0) Or (FxFlags And shaderFresnelTerm<>0) Then
  	Line('varying highp vec4 lightCoord;');

  If ((FxFlags and shaderCubeMap<>0) Or (FxFlags and shaderSphereMap<>0)) And (FxFlags And shaderNormalMap=0) Then
  	Line('varying highp vec4 reflCoord;');

  If ((FxFlags And shaderFresnelTerm<>0) Or (FxFlags And shaderGhost<>0)) And (FxFlags And shaderNormalMap=0) Then
  	Line('varying mediump float fresnelTerm;');

  If (FxFlags And shaderFresnelTerm<>0) Or (FxFlags And shaderGhost<>0) Then
  Begin
  	Line('  const mediump float fresnelPower = 1.0;');
  	Line('  const mediump float fresnelScale = 0.5;');
  End;


  If (FxFlags and shaderLightmap=0) Or (OutFlags and shader_OutputOutline<>0) Then
  	Line('varying mediump vec3 vertex_normal;');

  If (FxFlags and shaderNormalMap<>0) Then
  Begin
	  Line('  varying mediump vec3 tangent;');
  	Line('  varying mediump vec3 binormal;');
  End;

  If (FxFlags and shaderNormalMap<>0) Or (FxFlags and shaderGhost<>0)
  Or (FxFlags and shaderFresnelTerm<>0) Or (FxFlags And shaderSphereMap<>0) Then
  Begin
    Line('  varying mediump vec3 eye_vector;');
  End;

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
  	Line('  varying mediump float clipDistance;');
  End;
End;

Procedure ShaderEmitter.CustomShading;
Begin
  If (FxFlags and shaderSpecular<>0) Then
  Begin
	  Line('	mediump vec3 r = normalize(reflect(-sunDirection, normal));');
    Line('	mediump vec3 sp = dot(r, normalize(cameraView));');
    Line('  mediump float nDoth = max(sp, 0.0);');
  End;

  If (FxFlags and shaderColorRamp<>0) Then
  Begin
    If (FxFlags and shaderSpecular<>0) Then
      Line('  result *= texture2D('+ColorRampUniformName+', vec2(shading, nDoth));')
    Else
      Line('  result *= texture2D('+ColorRampUniformName+', vec2(shading, 0.0));');
    //Line('  result += 0.5 * texture2D(colorRamp, vec2(shading, 0.0));');
  End Else
  Begin
    Line('  result *= shading;');

    If (FxFlags and shaderSpecular<>0) Then
    Begin
      Line('  lowp vec4 spec = specular * pow(nDoth, specular_power);');
      Line('  result.rgb += spec.rgb;');
    End;
  End;
End;

Procedure ShaderEmitter.VertexUniforms;
Begin
	Line('uniform mat4 cameraMatrix;');
	Line('uniform mat4 modelMatrix;');
  Line('uniform mat4 projectionMatrix;');

  If (FxFlags And shaderTextureMatrix<>0) Then
	  Line('uniform mat4 textureMatrix;');

  If (OutFlags And shader_OutputOutline<>0) Then
    Line('  uniform mediump float outlineScale;');

  If (FxFlags and shaderSkinning<>0) Then
    Line('uniform vec4 boneVectors['+IntToString(Succ(MaxBones)*3)+'];');

  If (FxFlags and shaderNormalMap<>0) Or (FxFlags and shaderGhost<>0) Or (FxFlags and shaderSphereMap<>0) Or (FxFlags and shaderFresnelTerm<>0) Then
  Begin
  	Line('  uniform highp vec3 cameraPosition;');
  End;

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
  	Line('  uniform highp vec4 clipPlane;');
  End;
End;

Procedure ShaderEmitter.FragmentUniforms;
Begin
  If (FxFlags And shaderSkipAmbient=0) Then
    Line('  uniform lowp vec4 ambient_color;');

	Line('  uniform lowp sampler2D diffuseMap;');
  If (FxFlags and shaderTriplanar<>0) Then
  	Line('  uniform lowp sampler2D diffuseMap2;');

  If (FxFlags and shaderSpecular<>0) Then
  	Line('  uniform lowp sampler2D '+SpecularMapUniformName+';');

  If (FxFlags and shaderReflectiveMap<>0) Then
  	Line('  uniform lowp sampler2D '+ReflectiveMapUniformName+';');

  If (FxFlags and shaderAlphamap<>0) Then
    Line('  uniform lowp sampler2D '+AlphaMapUniformName+';')
  Else
  If (FxFlags and shaderLightmap<>0) Then
    Line('  uniform lowp sampler2D lightMap;')
  Else
  	Line('  uniform mediump vec3 sunDirection;');

  Line('  uniform lowp vec4 sunColor;');

  If (FxFlags and shaderNormalmap<>0) Then
  Begin
  	Line('  uniform lowp sampler2D '+NormalMapUniformName+';');
  End;

  If (FxFlags and shaderFlowMap<>0) Then
  Begin
    Line('  uniform lowp sampler2D '+FlowMapUniformName+';');
    Line('  uniform mediump vec3 flowCycle;');
    Line('  uniform highp vec4 flowBounds;');
    Line('  uniform highp float flowSpeed;');
  End;

  If (FxFlags and shaderNoiseMap<>0) Then
  	Line('  uniform lowp sampler2D '+NoiseMapUniformName+';');

  If (FxFlags and shaderDecalMap<>0) Then
  	Line('  uniform lowp sampler2D '+DecalMapUniformName+';');

  If (OutFlags And shader_OutputOutline<>0) Then
    Line('  uniform mediump vec4 outlineColor;');

  If (OutFlags And shader_OutputColor<>0) Then
    Line('  uniform mediump vec4 targetColor;');

  {If (Flags And shaderOutputRefraction<>0) Then
  	Line('  uniform lowp sampler2D refractionMap;');}

  If (OutFlags And shader_OutputReflection<>0) Then
    Line('  uniform lowp float reflectionFactor;');

  If (FxFlags and shaderColorRamp<>0) Then
  	Line('  uniform lowp sampler2D '+ColorRampUniformName+';');

  If (FxFlags And shaderScreenMask<>0) Then
  	Line('  uniform lowp sampler2D screenMask;');

	Line('  uniform lowp vec4 diffuse_color;');

  If (FxFlags and shaderCubeMap<>0) Then
  	Line('  uniform samplerCube cubeMap;')
  Else
  If (FxFlags and shaderSphereMap<>0) Then
	  Line('  uniform sampler2D '+SphereMapUniformName+';');

	Line('  uniform highp vec3 cameraPosition;');
	Line('  uniform highp vec3 cameraView;');
  Line('  uniform highp float zFar;');
  Line('  uniform highp float zNear;');

  If (FogFlags<>0) Then
  Begin
  	Line('  const highp float LOG2 = 1.442695;');
  	Line('  uniform lowp vec4 fogColor;');
	  Line('  uniform highp float fogDensity;');

    If (FogFlags And fogDistance<>0) Then
    	Line('  uniform highp float fogStart;');

    If (FogFlags And fogHeight<>0) Then
    	Line('  uniform highp float fogHeight;');
  End;

  If (OutFlags And shader_OutputReflection<>0) Then
	  Line('  uniform lowp sampler2D '+ReflectionMapUniformName+';');

  If (OutFlags And shader_OutputGlow<>0) Then
	  Line('  uniform lowp sampler2D glowMap;');
End;

Procedure ShaderEmitter.EmitDecodeBoneMat;
Begin
  Line('mat4 decodeBoneMat(int id){');
  Line('vec4 b1 = boneVectors[id*3+0];');
  Line('vec4 b2 = boneVectors[id*3+1];');
  Line('vec4 b3 = boneVectors[id*3+2];');
  Line('mat4 result;');

  Line('result[0][0] = b1.x;');
  Line('result[1][0] = b1.y;');
  Line('result[2][0] = b1.z;');
  Line('result[3][0] = b1.w;');

  Line('result[0][1] = b2.x;');
  Line('result[1][1] = b2.y;');
  Line('result[2][1] = b2.z;');
  Line('result[3][1] = b2.w;');

  Line('result[0][2] = b3.x;');
  Line('result[1][2] = b3.y;');
  Line('result[2][2] = b3.z;');
  Line('result[3][2] = b3.w;');

  Line('result[0][3] = 0.0;');
  Line('result[1][3] = 0.0;');
  Line('result[2][3] = 0.0;');
  Line('result[3][3] = 1.0;');

  Line('return result;}');
End;

Procedure ShaderEmitter.EmitReflectionCoord(FxFlags, OutFlags,FogFlags:Cardinal; Const NormalVarName:TERRAString);
Begin
  If (FxFlags and shaderCubeMap<>0) Then
  Begin
    Line('  highp vec3 u = normalize(vec3(world_position));');
    Line('  highp vec3 n = mat3(cameraMatrix) * '+NormalVarName+';');
    Line('  highp vec3 r = reflect(u, n);');
    Line('  reflCoord.xyz = r;');
  End Else
  If (FxFlags And shaderSphereMap<>0) Then
  Begin
		Line(' highp vec3 r = reflect(eye_vector, '+NormalVarName+');');
		Line(' highp float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );');
		Line(' reflCoord = vec4(r.x/m + 0.5, r.y/m + 0.5, 0.0, 0.0);');
  End;
End;

Function ShaderEmitter.Emit(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):TERRAString;
Var
  N, I:Integer;
  S2:TERRAString;
Begin
  _Buffer := '';

{  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_VECTORS, @N);
  Log(logDebug,'Shader', 'Max uniform vectors: '+IntToString(N)); //128 - 32 mats
  glGetIntegerv(GL_MAX_VARYING_VECTORS, @N);
  Log(logDebug,'Shader', 'Max varying vectors: '+IntToString(N)); // 8}

(*	Line('vertex {');
	Line('attribute highp vec4 terra_position;');
  Line('attribute highp float terra_boneIndex;');
	Line('  uniform mat4 cameraMatrix;');
	Line('  uniform mat4 modelMatrix;');
	Line('  uniform mat4 textureMatrix;');
  Line('  uniform mat4 projectionMatrix;');
  Line('	uniform vec4 boneVectors[108];');

  EmitDecodeBoneMat();

	Line('varying highp vec4 world_position;');
	Line('varying highp vec4 local_position;');
	Line('varying highp vec4 screen_position;');

	Line('void main() {');
  Line('vec4 local_position = terra_position;');
  If (Flags and shaderSkinning<>0) Then
  Begin
    Line('    int boneIndex = int(terra_boneIndex);');
    Line('    mat4 boneMatrix = decodeBoneMat(boneIndex);');
    Line('		local_position = boneMatrix * local_position;');
  End;
  Line(' world_position = modelMatrix * local_position;');
  Line(' screen_position = projectionMatrix * cameraMatrix * modelMatrix * local_position;');
  Line('gl_Position = projectionMatrix * cameraMatrix * modelMatrix * local_position;');
	Line('}}');

	Line('fragment	{ ');
  Line('void main()	{ ');
  Line('  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);}}');
  Result := _Buffer;
  Exit;*)

{$IFDEF PC}
  Line('version { 120 }');
{$ENDIF}
  Line('vertex {');
	Line('attribute highp vec4 terra_position;');
  Line('  attribute lowp vec4 terra_color;');

  Line('attribute highp vec4 terra_UV0;');

  If (FxFlags and shaderLightmap<>0) Or (FxFlags and shaderalphaMap<>0) Or (FxFlags And shaderFresnelTerm<>0) Then
  	Line('attribute highp vec4 terra_UV1;');

  Line('attribute mediump vec3 terra_normal;');

  If (FxFlags and shaderNormalMap<>0) Then
	  Line('attribute mediump vec4 terra_tangent;');

  If (FxFlags and shaderSkinning<>0) Then
  	Line('attribute highp float terra_boneIndex;');

  Self.VertexUniforms(FxFlags, OutFlags, FogFlags);

  If (FxFlags and shaderSkinning<>0) Then
    Self.EmitDecodeBoneMat();

  Varyings(FxFlags, OutFlags, FogFlags);


  If (FxFlags And shaderVegetation<>0) Then
  Begin
    Line('  uniform highp float vegetationBase;');
    Line('  uniform highp float vegetationSize;');
    Line('  uniform highp float vegetationTime;');
    Line('  uniform highp float vegetationBend;');
    Line('  const highp float PI = 3.14159;');
  End;

(*  If (Flags And shaderVertexWaves<>0) Then
  Begin
    Line('  uniform highp float wave_time;');
    Line('  uniform highp float wave_amplitude;');

    Line(' mediump float calculateWave(mediump float pos, mediump float amplitude, mediump float frequency){');
    Line('float theta = pos * 0.1 + wave_time * 0.1;');
    Line('return amplitude * sin(theta* frequency)* 0.5;}');

  {
    Line('  const lowp float waterHeight = 5.0;');

    Line('mediump float wave(float speed, float amplitude, float wavelength, vec2 direction, vec4 position) {');
    Line('float frequency = 2*pi/wavelength;');
    Line('float phase = speed * frequency;');
    Line('float theta = dot(direction, vec2(position.x, position.z));');
    Line('return amplitude * sin(theta * frequency + wave_time * phase);}');}
  End;*)

	Line('void main() {');

  Line('vec4 local_position = terra_position;');

  If (FxFlags and shaderSkinning<>0) Then
  Begin
    Line('    int boneIndex = int(terra_boneIndex);');
    Line('    mat4 boneMatrix = decodeBoneMat(boneIndex);');
    Line('		local_position = boneMatrix * local_position;');
  End;
  
  If (FxFlags and shaderLightmap=0) Or (OutFlags and shader_OutputOutline<>0) Then
  Begin
    If (FxFlags and shaderSkinning<>0) Then
    Begin
      Line('  vertex_normal = terra_normal;');
      {$IFDEF PC}
      Line('  vertex_normal = mat3(boneMatrix) * vertex_normal;');
      Line('  vertex_normal = mat3(modelMatrix) * vertex_normal;');
      Line('  vertex_normal = normalize(vertex_normal);');
      {$ENDIF}
    End Else
      Line('  vertex_normal = mat3(modelMatrix) * terra_normal;');
    If (FxFlags and shaderNormalMap<>0) Then
    Begin
      Line('  tangent = normalize(mat3(modelMatrix) * terra_tangent.xyz);');
      Line('  binormal = normalize(cross(vertex_normal, tangent)) * terra_tangent.w;');
    End;
  End;

  If (OutFlags And shader_OutputOutline<>0) Then
  Begin
    Line('		local_position.xyz = local_position.xyz + vertex_normal * outlineScale;');
  End;

  (*If (Flags And shaderVertexWaves<>0) Then
  Begin
    Line('mediump float waveHeight = 0;');
    Line('  waveHeight += calculateWave(local_position.x, 5.0, wave_amplitude);');
    Line('  waveHeight += calculateWave(local_position.z, 3.0, wave_amplitude);');
    Line('local_position.y += waveHeight;');
  End;*)

  If (FxFlags And shaderVegetation<>0) Then
  Begin
    Line('lowp float bendFactor = (local_position.y - vegetationBase) / vegetationSize;');
    Line('  bendFactor = clamp(bendFactor, 0.0, 1.0);');
    Line('  bendFactor *= bendFactor;');
    Line('  bendFactor *= vegetationBend;');
    Line('  mediump vec2 bendOfs = vec2(cos(fract(vegetationTime*0.3)*PI*2.0), sin(fract(vegetationTime*0.1)*PI*2.0));');
    Line('  bendOfs *= bendFactor;');
    Line('local_position.x += bendOfs.x;');
    Line('local_position.z += bendOfs.y;');
  End;

  Line(' world_position = modelMatrix * local_position;');

  If (FxFlags and shaderNormalMap<>0) Or (FxFlags and shaderGhost<>0)
  Or (FxFlags and shaderFresnelTerm<>0) Or (FxFlags And shaderSphereMap<>0) Then
  Begin
      Line('  eye_vector = normalize(world_position.xyz - cameraPosition);');
  End;

  If ((FxFlags And shaderFresnelTerm<>0) Or (FxFlags And shaderGhost<>0)) And (FxFlags And shaderNormalMap=0) Then
  Begin
  	Line('  fresnelTerm = 1.0 * pow(1.0 + dot(eye_vector, vertex_normal), fresnelPower);');
  End;

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
    Line('  clipDistance = dot(world_position.xyz, clipPlane.xyz) + clipPlane.w;');
  End;

  Line(' screen_position = projectionMatrix * cameraMatrix * world_position;');
  {$IFDEF ANDROID}
  // STUPID HACK FOR GALAXY TAB2, possibly a driver bug, fuck this
  Line('gl_Position = projectionMatrix * cameraMatrix * modelMatrix * local_position;');
  //Line('world_position.w = local_position.w;');
  //Line('gl_Position = projectionMatrix * cameraMatrix * world_position;');
  {$ELSE}
  Line('  gl_Position = screen_position;');
  {$ENDIF}

  Line('  vertex_color = terra_color;');

  If (FxFlags And shaderFresnelTerm<>0) And (FxFlags And shaderTextureMatrix<>0)Then
    Line('  lightCoord = textureMatrix * terra_UV1;')
  Else
  If (FxFlags And shaderFresnelTerm<>0) Or  (FxFlags and shaderLightmap<>0) Or (FxFlags and shaderAlphaMap<>0) Then
    Line('  lightCoord = terra_UV1;');

  If (FxFlags And shaderTextureMatrix<>0) Then
    Line('  texCoord0 = textureMatrix * terra_UV0;')
  Else
    Line('  texCoord0 = terra_UV0;');

  If (FxFlags And shaderNormalMap=0) Then
    EmitReflectionCoord(FxFlags, OutFlags, FogFlags, 'vertex_normal');

  Self.VertexPass(FxFlags, OutFlags, FogFlags);

  Line('	}');
  Line('}');

(*	Line('fragment	{ ');
  	Line('varying mediump vec3 vertex_normal;');
  Line('void main()	{ ');
  Line('  gl_FragColor = vec4(vertex_normal * 0.5 + 0.5, 1.0);}}');
  Result := _Buffer;

  Log(logDebug, 'Shader', _Buffer);
  Exit;
  //-----------------
*)

  Line('fragment {');
  Self.Varyings(FxFlags, OutFlags, FogFlags);
  Self.FragmentUniforms(FxFlags, OutFlags, FogFlags);

	Line('  lowp vec4 diffuse;');
	Line('  lowp vec4 color;');
	Line('  lowp vec4 specular;');

  If (FxFlags and shaderLightmap=0) Then
  	Line('  mediump vec3 normal;');


  If (FogFlags<>0) Then
  Begin
    Line('lowp float calculateFog()	{');
    If (FogFlags And fogDistance<>0) Then
    Begin
      Line('  highp float z = length(world_position.xyz - cameraPosition);');
      Line('  z = max(z - fogStart, 0.0) / zFar;');
      Line('  highp float distanceFactor = exp2( -fogDensity * fogDensity * z * z * LOG2 );');
    End;

    If (FogFlags And fogHeight<>0) Then
    Begin
      Line('  highp float y = world_position.y;');
      Line('  y = max(y-fogHeight, 0.0) / zFar;');
      Line('  highp float heightFactor = exp2( -fogDensity * fogDensity * y * y * LOG2 );');
    End;

    If (FogFlags And fogDistance<>0) And (FogFlags And fogHeight<>0) Then
    Begin
      Line('  lowp float fogFactor = max(heightFactor, distanceFactor);');
    End Else
    If (FogFlags And fogDistance<>0) Then
    Begin
      Line('  lowp float fogFactor = distanceFactor;');
    End Else
    If (FogFlags And fogHeight<>0) Then
    Begin
      Line('  lowp float fogFactor = heightFactor;');
    End;

    Line('  fogFactor = clamp(fogFactor, 0.0, 1.0);');
    Line('  return fogFactor;');
	  Line('}');
  End;

  If (FxFlags And shaderWireframe<>0) Then
  Begin
  	Line('void main()	{');
    Line('  color = diffuse_color;');

    If (FogFlags<>0) Then
    Begin
      Line('  lowp float fogFactor = calculateFog(); ');
      Line('  color = mix(fogColor, color, fogFactor);');
    End;

    Line('  gl_FragColor = color;}');
    Line('}');
    Result := _Buffer;
    Exit;
  End;

  If (FxFlags And shaderColorOff<>0) Then
  Begin
  	Line('void main()	{');
    Line('  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);}');
    Line('}');
    Result := _Buffer;
    Exit;
  End;

  If (FxFlags And shaderLightmap=0) Then
  Begin
    If (LightModel <> lightModelSimple) Then
    Begin
    	Line('mediump float halfDot(mediump vec3 A, mediump vec3 B)	{');
      Line('  mediump float result = dot(A, B);');
      Line('  result *= 0.5;');
      Line('  result += 0.5;');
      Line('  result = result * result;');
      Line('  return result;	}');
    End;

    For I:=1 To Lights.DirectionalLightCount Do
    Begin
 	    Line('  uniform highp vec3 dlightDirection'+IntToString(I)+';');
  	  Line('  uniform lowp vec4 dlightColor'+IntToString(I)+';');
    End;

    For I:=1 To Lights.PointLightCount Do
    Begin
   	  Line('  uniform highp vec3 plightPosition'+IntToString(I)+';');
	    Line('  uniform lowp vec4 plightColor'+IntToString(I)+';');
  	  Line('  uniform highp float plightRadius'+IntToString(I)+';');
    End;

    For I:=1 To Lights.SpotLightCount Do
    Begin
   	  Line('  uniform highp vec3 slightPosition'+IntToString(I)+';');
   	  Line('  uniform mediump vec3 slightDirection'+IntToString(I)+';');
   	  Line('  uniform mediump vec3 slightCross'+IntToString(I)+';');
	    Line('  uniform lowp vec4 slightColor'+IntToString(I)+';');
  	  Line('  uniform highp float slightCosInnerAngle'+IntToString(I)+';');
  	  Line('  uniform mediump float slightCosOuterAngle'+IntToString(I)+';');
      Line('  uniform lowp sampler2D slightCookie'+IntToString(I)+';');
      Line('  uniform mat4 slightMatrix'+IntToString(I)+';');
    End;

    If (Lights.SpotLightCount>0) Then
    Begin
      If (FogFlags<>0) Then
        Line('lowp vec4 spotLight(lowp float fogFactor, highp vec3 lightPosition, lowp vec4 lightColor, mediump vec3 lightDir, mediump float cos_inner_cone_angle, mediump float cos_outer_cone_angle, mat4 lightMatrix, lowp sampler2D cookieTex){')
      Else
        Line('lowp vec4 spotLight(highp vec3 lightPosition, lowp vec4 lightColor, mediump vec3 lightDir, mediump float cos_inner_cone_angle, mediump float cos_outer_cone_angle, mat4 lightMatrix, lowp sampler2D cookieTex){');
      Line('  mediump vec3 direction = lightPosition - world_position.xyz;');
      Line('  highp float dist = length(direction);');
      Line('  direction /= dist;');

      Line('  highp vec4 posLightSpace = lightMatrix * world_position;');
      Line('  posLightSpace.xyz /= posLightSpace.w;');
      Line('  lowp vec4 cookieColor = texture2D(cookieTex, posLightSpace.xy);');
      //Line('  lowp vec4 cookieColor = vec4(posLightSpace.x*0.5 + 0.5, posLightSpace.y*0.5 + 0.5, 0.0, 1.0);');

      Line('  highp float cos_cur_angle = dot(direction, lightDir);');
      Line('  highp float cos_inner_minus_outer_angle = cos_inner_cone_angle - cos_outer_cone_angle;');

      Line('  lowp vec4 result = diffuse * lightColor;');

      If (LightModel <> lightModelSimple) Then
      Begin
        Line('  highp float shading = clamp((cos_cur_angle - cos_outer_cone_angle) / cos_inner_minus_outer_angle, 0.0, 1.0);');
        CustomShading(FxFlags, OutFlags, FogFlags);
      End;

      If (FogFlags<>0) Then
          Line('result = mix(fogColor, result, fogFactor);');

      Line('  return result * cookieColor ;	}');
    End;

    If (Lights.PointLightCount>0) Then
    Begin
      If (FogFlags<>0) Then
        S2 := 'lowp float fogFactor, '
      Else
        S2 := '';

      Line('lowp vec4 pointLight('+S2+'highp vec3 lightPosition, lowp vec4 lightColor, highp float radius){');
      Line('  highp vec3 direction = lightPosition - world_position.xyz;');
      Line('  highp float dist = length(direction);');
      Line('  highp float att = 1.0 - min(dist*radius, 1.0);');
      Line('  direction /= dist;');

      Line('  lowp vec4 result = diffuse * lightColor;');

      If (LightModel <> lightModelSimple) Then
      Begin
        Line('  mediump float shading = halfDot(normal, direction);');
        CustomShading(FxFlags, OutFlags, FogFlags);
      End Else
      Begin
      End;

      If (FogFlags<>0) Then
          Line('result = mix(fogColor, result, fogFactor);');

      Line('  return result *att;	}');
    End;

    If (Lights.DirectionalLightCount>0) Then
    Begin
      If (FogFlags<>0) Then
    	  Line('lowp vec4 directionalLight(lowp float fogFactor, highp vec3 direction, lowp vec4 lightColor){')
      Else
    	  Line('lowp vec4 directionalLight(highp vec3 direction, lowp vec4 lightColor){');

      Line('  mediump float shading = halfDot(normal, direction);');
      Line('  lowp vec4 result = diffuse * lightColor;');
      CustomShading(FxFlags, OutFlags, FogFlags);
  	  Line('	return result;}');
    End;
  End;

  If (FxFlags And shaderColorTable<>0) Then
    Line(GetColorTableShaderCode());

	Line('void main()	{');
  {$IFDEF SIMPLESHADER}
  Line('  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);}');
  {$ELSE}

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
    Line('  if (clipDistance>0.0) discard;');
  End;

  If (OutFlags And shader_OutputColor<>0) Then
  Begin
    Line('  gl_FragColor = targetColor;}');
  End Else
  Begin

    If (FxFlags and shaderFlowMap<>0) Then
    Begin
      //Line('  highp vec2 flowDirection = vec2(1.0, 0.0); ');
      Line('  mediump vec2 worldUV = vec2(world_position.x, world_position.z);');
      Line('  worldUV -= flowBounds.xy;');
      Line('  worldUV *= flowBounds.zw;');

      Line('  lowp vec2 flowDirection = texture2D('+FlowMapUniformName+', worldUV).xy;');
      Line('  flowDirection = (flowDirection - 0.5) * 2.0;');

      Line('  highp vec2 flowOffset1 = flowDirection * flowSpeed * flowCycle.x;');
      Line('  highp vec2 flowOffset2 = flowDirection * flowSpeed * (flowCycle.x + 0.5);');

      If (FxFlags and shaderNoiseMap<>0) Then
      Begin
        Line('  highp vec2 uvnoise = texture2D('+NoiseMapUniformName+', vec2(flowCycle.z, flowCycle.z)).rg;');
        Line('  flowOffset2 += uvnoise; ');
      End;
    End;

  If (FxFlags and shaderLightmap=0) Then
  Begin
    Line('  normal = normalize(vertex_normal);');

    If (FxFlags and shaderNormalMap<>0) Then
    Begin
      If (FxFlags and shaderFresnelTerm <> 0) Then
        Line('  highp vec2 normalUV = lightCoord.st;')
      Else
        Line('  highp vec2 normalUV = texCoord0.st;');

      If (FxFlags and shaderFlowMap<>0) Then
      Begin
        Line('  highp vec3 bump1 = texture2D('+NormalMapUniformName+', normalUV + flowOffset1).rgb;');
        Line('  highp vec3 bump2 = texture2D('+NormalMapUniformName+', normalUV + flowOffset2).rgb;');
        Line('  highp vec3 n = mix(bump1, bump2, flowCycle.y);');
        Line('  n = normalize(n);');
      End Else
        Line('  highp vec3 n = texture2D('+NormalMapUniformName+', normalUV).rgb * 2.0 - 1.0;');

      //Line('  n = normalize(n);');
      Line('  highp vec3 t = normalize(tangent);');
      Line('  highp vec3 b = normalize(binormal);');
      Line('  normal = (t * n.x) + (b * n.y) + (normal * n.z);');
      Line('  normal = normalize(normal);');

      If (FxFlags and shaderCubeMap<>0) Or (FxFlags and shaderSphereMap<>0) Then
        Line('mediump vec4 reflCoord;');

      EmitReflectionCoord(FxFlags, OutFlags, FogFlags, 'normal');
    End;

    If ((FxFlags And shaderFresnelTerm<>0) Or (FxFlags And shaderGhost<>0)) And (FxFlags And shaderNormalMap<>0) Then
    Begin
    	Line('  mediump float fresnelTerm = fresnelScale * pow(1.0 + dot(eye_vector, normal), fresnelPower);');
      Line('  fresnelTerm = min(fresnelTerm, 1.0);');
    End;
  End;

  //Flags := Flags Or shaderOutputNormal;
  If (OutFlags And shader_OutputOutline<>0) Then
  Begin
        Line('  diffuse = texture2D(diffuseMap, texCoord0.st);');
        Line('  diffuse *= vertex_color; ');
        Line('diffuse *= diffuse_color;');
        If (FxFlags and shaderAlphaTest<>0) Then
          Line('  if (diffuse.a<0.1) discard;');
//    Line('  color.rgb = diffuse.rgb * 0.333;');
        Line('  color = outlineColor;');

      If (FogFlags<>0) Then
      Begin
        Line('  lowp float fogFactor = calculateFog();');
        Line('color = mix(fogColor, color, fogFactor);');
      End;

      If (FxFlags And shaderSkipAmbient=0) Then
        Line('  color *= ambient_color;');

    Line('  color.a = diffuse.a;');
//    Line('  color = vec4(0.0, 0.0, 0.0, 1.0);');
  End Else
  If (OutFlags And shader_OutputRefraction<>0) Then
  Begin
    Line('  color.rgb = normal * 0.5 + 0.5;');

    Line('  diffuse = texture2D(diffuseMap, texCoord0.st);');
    Line('  diffuse *= vertex_color; ');

    If (FxFlags and shaderAlphaTest<>0) Then
      Line('  if (diffuse.a<0.1) discard;');

    Line('  color.a = 1.0;'); // refraction ammount
  End Else
  If (OutFlags And shader_OutputNormal<>0) Then
  Begin
    Line('  color.rgb = normal * 0.5 + 0.5;');

    Line('  diffuse = texture2D(diffuseMap, texCoord0.st);');
    Line('  diffuse *= vertex_color; ');

    //If (Flags and shaderAlphaTest<>0) Then
    Line('  if (diffuse.a<0.3) discard;');

    Line('  highp float zz = ((screen_position.z / screen_position.w) + 1.0) * 0.5;');
    Line('  zz = (2.0 * zNear) / (zFar + zNear - zz * (zFar - zNear));');
    Line('  color.a = zz;');
    Line('  color.a = 1.0;');
  End Else
  If (OutFlags And shader_OutputGlow<>0) Then
  Begin
    Line('  color = texture2D(glowMap, texCoord0.st);');
    Line('  color.a = 1.0;');
  End Else                                
  If (OutFlags And shader_OutputReflection<>0) Then
  Begin
    If (OutFlags And shaderFresnelTerm<>0) Then
    Begin
      Line('  lowp float waterRef = fresnelTerm * reflectionFactor;');
      //Line('  waterRef = 1.0 - waterRef;');
      Line('  color = vec4(waterRef);');
    End Else
    Begin
      Line('  color = texture2D('+ReflectionMapUniformName+', texCoord0.st) *  reflectionFactor;');
      //Line('  color.rgb = vec3(0.0, 0.0, 1.0);');
      Line('  color.a = 1.0;');
    End;
  End Else
  Begin
    If (FxFlags And shaderTriplanar<>0) Then
    Begin
      Line('  lowp vec3 absNormal = abs(normal);');
      Line('  lowp vec3 blend_weights = absNormal;');
      //Line('  blend_weights = blend_weights - 0.2679f;
      //Line('  blend_weights /= (blend_weights.x + blend_weights.y + blend_weights.z).xxx;
      Line('  lowp float uvfactor = 10.0;');
      Line('  lowp vec4 diffuse1 = texture2D(diffuseMap, world_position.xz / uvfactor);');
      Line('  lowp vec4 diffuse2 = texture2D(diffuseMap2, world_position.yz / uvfactor);');
      Line('  lowp vec4 diffuse3 = texture2D(diffuseMap2, world_position.xy / uvfactor);');
      Line('  diffuse = diffuse2 * blend_weights.x + diffuse1 * blend_weights.y + diffuse3 * blend_weights.z;');
    End Else
    If (FxFlags and shaderFlowMap<>0) Then
    Begin
      Line('  highp vec4 diff1 = texture2D(diffuseMap, texCoord0.st + flowOffset1);');
      Line('  highp vec4 diff2 = texture2D(diffuseMap, texCoord0.st + flowOffset2);');
      Line('  diffuse = mix(diff1, diff2, flowCycle.y);');
    End Else
      Line('  diffuse = texture2D(diffuseMap, texCoord0.st);');

    If (FxFlags And shaderDecalMap<>0) Then
    Begin
      Line('lowp vec4 decalColor = texture2D('+DecalMapUniformName+', texCoord0.st);');
      Line('  diffuse.rgb = mix(diffuse.rgb, decalColor.rgb, decalColor.a);');
    End;

    Line('  diffuse *= vertex_color; ');

    If (FxFlags and shaderAlphaTest<>0) Then
      Line('  if (diffuse.a<0.1) discard;');

    If (FxFlags and shaderSpecular<>0) Then
      Line('  specular = texture2D('+SpecularMapUniformName+', texCoord0.st);');

    If (FxFlags and shaderCubeMap<>0) Then
    Begin
      Line('  lowp vec4 reflection = textureCube(cubeMap, reflCoord.xyz);');
    End Else
    If (FxFlags and shaderSphereMap<>0) Then
    Begin
      Line('  lowp vec4 reflection = texture2D('+SphereMapUniformName+', reflCoord.xy);');
    End;

    If (FxFlags and shaderCubeMap<>0) Or (FxFlags and shaderSphereMap<>0) Then
    Begin
      If (FxFlags and shaderReflectiveMap<>0) Then
      Begin
        Line('  lowp vec4 refpow = texture2D('+ReflectiveMapUniformName+', texCoord0.st);');
        Line('  diffuse = mix(diffuse, reflection, refpow);');
        //Line('  diffuse = refpow;');
      End Else
      If (FxFlags and shaderFresnelTerm = 0) Then
        Line('  diffuse *= reflection;');
    End;

    Line('diffuse *= diffuse_color;');

    If (FxFlags and shaderLightmap<>0) Then
    Begin
      Line('  color = texture2D(lightMap, lightCoord.st);');
      If (FxFlags and shaderAddSigned<>0) Then
        Line('  color.rgb += (diffuse.rgb - 0.5);')
      Else
        Line('  color.rgb *= diffuse.rgb * sunColor.rgb;');
    End Else
    Begin
      If (FogFlags<>0) Then
        Line('  lowp float fogFactor = calculateFog();');

      If (FxFlags And shaderSkipAmbient<>0) Then
      Begin
        Line('  color = vec4(0.0, 0.0, 0.0, 1.0);');
      End Else
      Begin
        Line('  color = diffuse;');

        If (FogFlags<>0) Then
          Line('  color = mix(fogColor, color, fogFactor);');

        Line('  color *= ambient_color;');
      End;

      If (FogFlags<>0) Then
        S2 := 'fogFactor, '
      Else
        S2 := '';

      For I:=1 To Lights.DirectionalLightCount Do
        Line('  color += directionalLight('+S2+'dlightDirection'+IntToString(I)+', dlightColor'+IntToString(I)+');');

      For I:=1 To Lights.PointLightCount Do
        Line('  color += pointLight('+S2+'plightPosition'+IntToString(I)+', plightColor'+IntToString(I)+', plightRadius'+IntToString(I)+');');

      For I:=1 To Lights.SpotLightCount Do
        Line('  color += spotLight('+S2+'slightPosition'+IntToString(I)+', slightColor'+IntToString(I)+', slightDirection'+IntToString(I)+', slightCosInnerAngle'+IntToString(I)+', slightCosOuterAngle'+IntToString(I)+', slightMatrix'+IntToString(I)+', slightCookie'+IntToString(I)+');');
    End;

    If (FxFlags and shaderFresnelTerm <> 0) Then
    Begin
      //Line('  lowp float fresnelTerm = dot(eye_vector, normal);');
      //Line('  fresnelTerm = 1.0 - max(0.0, fresnelTerm);');

      //Line('  fresnelTerm *= fresnelTerm;');
      //Line('  color.rgb = vec3(fresnelTerm);');

      If (FxFlags and shaderCubeMap<>0) Or (FxFlags and shaderSphereMap<>0) Then
        Line('  color.rgb = mix(reflection.rgb, color.rgb, fresnelTerm);')
      Else
        Line('  color.a = fresnelTerm;');

      //Line('  color.rgb = normal * 0.5 + 0.5;');

      {Line('  color.rgb = reflection.rgb;');
      Line('  color.a = 1.0;');}
    End Else
    If (FxFlags and shaderAlphaMap<>0) Then
    Begin
      //Line('  lowp vec4 alpha_color = texture2D(alphaMap, lightCoord.st);');
    //        Line('  color *= alpha_color;');
      Line('  color.a = texture2D('+AlphaMapUniformName+', lightCoord.st).a;');
    End Else
      Line('  color.a = diffuse.a;');
  		//color.rgb = normal * 0.5 + 0.5;
  //  Line('  color.rgb = t.xyz * 0.5 + 0.5;');
  End;

    If (FxFlags and shaderGhost <> 0) Then
    Begin
      //Line('  lowp float edn = dot(eye_vector, normal);');
      //Line('  lowp float ghostTerm = 1.0 - max(edn, 0.0);');
      Line('  lowp vec4 ghostColor = diffuse_color * fresnelTerm;');
      Line('  color = ghostColor;');
    End Else

 {   If (Flags And shaderColor<>0) Then
      Line('  color = vertex_color;');
  }

  Self.FinalPass(FxFlags, OutFlags, FogFlags);


  //Line('  color.rgb = pow(color, vec3(1.0 / 2.2));');
    //Line('  color.rgb = normal * 0.5 + 0.5;');
    //Line('  color.rgb = vec3(1.0, 0.0, 0.0);');

  {$IFDEF DEBUG_LIGHTMAP}
    If (FxFlags and shaderLightmap<>0) Then
    Begin
      Line('  color.rgb = texture2D(lightMap, lightCoord.st).rgb;');
    End;
  {$ENDIF}

  If (FxFlags And shaderColorTable<>0) Then
    Line('  color.rgb = ColorTableLookup(color.rgb);');

  If (FxFlags And shaderScreenMask<>0) Then
  Begin
    Line('  lowp vec3 spos = screen_position.xyz / screen_position.w;');
    Line('  spos = spos * 0.5 + 0.5; ');
    //Line('  color.rgb = texture2D(screenMask, spos.st).rgb;');
    //Line('  color.rgb = vec3(spos.x, 0.0, spos.y);');
    Line('  color.a = texture2D(screenMask, spos.st).r;');
  End;

  Line('  gl_FragColor = color;}');
  End;
  {$ENDIF}
  Line('}');
  Result := _Buffer;
End;

Procedure ShaderEmitter.FinalPass(FxFlags, OutFlags,FogFlags:Cardinal);
Begin
  // do nothing
End;


Procedure ShaderEmitter.Bind(FxFlags, OutFlags,FogFlags:Cardinal; Const Lights:LightBatch);
Begin
  // do nothing
End;

Procedure ShaderEmitter.VertexPass(FxFlags, OutFlags, FogFlags: Cardinal);
Begin

End;

Procedure ShaderEmitter.Init;
Begin
  // do nothing
End;

{ ShaderFactory }
Procedure ShaderFactory.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ShaderCount) Do
    ReleaseObject(_Shaders[I]);

  _ShaderCount := 0;
End;

Procedure ShaderFactory.Init;
Begin
  _Emitter := ShaderEmitter.Create();
  _Emitter.Init();
End;

Procedure ShaderFactory.Release;
Begin
  If Assigned(_Emitter) Then
  Begin
    _Emitter.Release();
    _Emitter := Nil;
  End;

  Self.Clear;
  _ShaderFactory_Instance := Nil;
End;

Function ShaderFactory.GetShader(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):Shader;
Var
  I:Integer;
  S:ShaderEntry;
  Location:TERRAString;
  SS, Name:TERRAString;
Begin
  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'ShaderFactory', 'Searching for shader with flags '+CardinalToString(Flags));
  {$ENDIF}

  For I:=0 To Pred(_ShaderCount) Do
  Begin
    S := _Shaders[I];
    If  (S.FxFlags = FxFlags) And (S.OutFlags = OutFlags)
    And (S.FogFlags = FogFlags) And (S.LightModel = LightModel)
    And (S.DirLightCount = Lights.DirectionalLightCount)
    And (S.PointLightCount = Lights.PointLightCount)
    And (S.SpotLightCount = Lights.SpotLightCount) Then
    Begin
    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'ShaderFactory', 'Found, binding... ');
    {$ENDIF}

      Result := S.Shader;

      _Emitter.Bind(FxFlags, OutFlags, FogFlags, Lights);

      Exit;
    End;
  End;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'ShaderFactory', 'Not found, creating new shader...');
  {$ENDIF}

  S := ShaderEntry.Create();

  Inc(_ShaderCount);
  SetLength(_Shaders, _ShaderCount);
  _Shaders[Pred(_ShaderCount)] := S;
  
  S.FxFlags := FxFlags;
  S.OutFlags := OutFlags;
  S.FogFlags := FogFlags;

  S.LightModel := LightModel;
  S.DirLightCount := Lights.DirectionalLightCount;
  S.PointLightCount := Lights.PointLightCount;
  S.SpotLightCount := Lights.SpotLightCount;

  Name := 'terra';
  If (FxFlags And shaderSkinning<>0) Then
    name := name + '_SKINNING;';

  If (FxFlags And shaderSpecular<>0) Then
    name := name + '_SPECULAR;';

  If (FxFlags And shaderNormalMap<>0) Then
    name := name + '_NORMALMAP;';

  If (FxFlags And shaderLightmap<>0) Then
    name := name + '_LIGHTMAP;';

  If (FxFlags And shaderCubeMap<>0) Then
    name := name + '_CUBEMAP;';

  If (FxFlags And shaderColorRamp<>0) Then
    name := name + '_COLORRAMP;';

  If (FxFlags And shaderSkipAmbient<>0) Then
    name := name + '_AMBIENTOFF;';

(*  If (Flags and shaderVertexWaves<>0) Then
    name := name + '_WAVES;';*)

  If (FxFlags and shaderVegetation<>0) Then
    name := name + '_VEGETATION;';

  If (FxFlags and shaderAlphaTest<>0) Then
    name := name + '_ALPHATEST;';

  If (FxFlags and shaderAddSigned<>0) Then
    name := name + '_ADDSIGNED;';

  If (FxFlags And shaderDecalMap<>0) Then
    name := name + '_DECALMAP;';

  If (FxFlags And shaderSphereMap<>0) Then
    name := name + '_SPHEREMAP;';

  If (FxFlags And shaderTriplanar<>0) Then
    name := name + '_TRIPLANAR;';

  If (FxFlags And shaderAlphamap<>0) Then
    name := name + '_ALPHAMAP;';

  If (FxFlags And shaderFlowMap<>0) Then
    name := name + '_FLOW;';

  If (FxFlags And shaderColorTable<>0) Then
    name := name + '_COLORTABLE;';

  If (OutFlags And shader_OutputNormal<>0) Then
    name := name + '_OUTNORMAL;';

  If (OutFlags And shader_OutputSpecular<>0) Then
    name := name + '_OUTSPECULAR;';

  If (OutFlags And shader_OutputGlow<>0) Then
    name := name + '_OUTGLOW;';

  If (OutFlags And shader_OutputColor<>0) Then
    name := name + '_OUTCOLOR;';

  If (FxFlags And shaderFresnelTerm<>0) Then
    name := name + '_FRESNEL;';

  If (OutFlags And shader_OutputRefraction<>0) Then
    name := name + '_REFRACTION;';

  If (OutFlags And shader_OutputOutline<>0) Then
    name := name + '_OUTLINE;';

  If (OutFlags And shader_OutputReflection<>0) Then
    name := name + '_REFLECTION;';

  If (FxFlags And shaderWireframe<>0) Then
    name := name + '_WIREFRAME;';

  If (FxFlags and shaderColorOff<>0) Then
    name := name + '_COLOROFF;';

  If (FxFlags and shaderReflectiveMap<>0) Then
    name := name + '_REFMAP;';

  Name := Name + ';SM'+IntToString(LightModel)+';F'+IntToString(FogFlags)+';';

  If Lights.DirectionalLightCount>0 Then
    name := name + 'DL'+IntToString(Lights.DirectionalLightCount)+';';
  If Lights.PointLightCount>0 Then
    name := name + 'PL'+IntToString(Lights.PointLightCount)+';';
  If Lights.SpotLightCount>0 Then
    name := name + 'SL'+IntToString(Lights.SpotLightCount)+';';

  Name := StringLower(Name);
  StringReplaceText(';', '_', Name);

  If StringContains('_', Name) Then
    StringDropChars(Name, -1);

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'ShaderFactory', 'Preparing shader '+Name);
  {$ENDIF}

  SS := _Emitter.Emit(FxFlags, OutFlags, FogFlags, LightModel, Lights);

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'ShaderFactory', 'Got shader code, compiling');
  {$ENDIF}

  S.Shader := Shader.CreateFromString(SS, Name);
  Result := S.Shader;
  _Emitter.Bind(FxFlags, OutFlags, FogFlags, Lights);
End;

Class Function ShaderFactory.Instance: ShaderFactory;
Begin
  If Not Assigned(_ShaderFactory_Instance) Then
    _ShaderFactory_Instance := InitializeApplicationComponent(ShaderFactory, GraphicsManager);

  Result := ShaderFactory(_ShaderFactory_Instance.Instance);
End;

Procedure ShaderFactory.OnContextLost;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ShaderCount) Do
  If (Assigned(_Shaders[I].Shader)) Then
    _Shaders[I].Shader.OnContextLost();
End;

Procedure ShaderFactory.SetShaderEmitter(Emitter: ShaderEmitter);
Begin
  If Assigned(_Emitter) Then
    _Emitter.Release;
  Self._Emitter := Emitter;
  Emitter.Init();
End;

{ ShaderEntry }
Procedure ShaderEntry.Release;
Begin
  ReleaseObject(Shader);
End;

End.
