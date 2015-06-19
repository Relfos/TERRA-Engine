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
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Renderer, TERRA_Application,
  TERRA_Lights, TERRA_BoundingBox, TERRA_Vector4D, TERRA_Color;

Const
  NormalMapUniformName = 'normalMap';
  DisplacementMapUniformName = 'displacementMap';
  DecalMapUniformName = 'decalMap';
  SpecularMapUniformName = 'specularMap';
  AlphaMapUniformName = 'alphaMap';
  ReflectiveMapUniformName = 'reflectiveMap';
  ReflectionMapUniformName = 'reflectionMap';
  SphereMapUniformName = 'sphereMap';
  ToonRampUniformName = 'colorRamp';
  FlowMapUniformName = 'flowMap';
  NoiseMapUniformName = 'noiseMap';
  ShadowMapUniformName = 'shadowMap';
  ColorMapUniformName = 'colorMap';

  DitherPatternMapUniformName = 'ditherMap';
  DitherScaleUniformName = 'dither_scale';

  shaderSkinning    = 1 Shl 0;
  shaderSpecular    = 1 Shl 1;
  shaderLightMap    = 1 Shl 2;
  shaderShadowMap = 1 Shl 3;
  shaderNormalMap   = 1 Shl 4;
  shaderCubeMap  = 1 Shl 5;
  shaderToonRamp   = 1 Shl 6;
  shaderVertexColor = 1 Shl 7;
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
  shaderParallaxBump    = 1 Shl 26;
  shaderDitherColor     = 1 Shl 27;
  shaderCartoonHue      = 1 Shl 28;
  shaderSelfIllumn      = 1 Shl 29;

  //ParallaxScale

  shader_OutputDiffuse    = 1 Shl 1;
  shader_OutputNormal    = 1 Shl 2;
  shader_OutputSpecular  = 1 Shl 3;
  shader_OutputGlow      = 1 Shl 4;
  shader_OutputRefraction= 1 Shl 5;
  shader_OutputOutline   = 1 Shl 6;
  shader_OutputFixedColor = 1 Shl 7;
  shader_OutputReflection= 1 Shl 8;
  shader_OutputShadow= 1 Shl 9;

Type
  ShaderEntry = Class(TERRAObject)
    FXFlags:Cardinal;
    OutFlags:Cardinal;
    DirLightCount:Integer;
    PointLightCount:Integer;
    SpotLightCount:Integer;
    LightModel:Integer;
    FogFlags:Integer;
    Shader:ShaderInterface;

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

      Function GetShader(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):ShaderInterface;
  End;

Implementation

Uses TERRA_Log, TERRA_Mesh, TERRA_GraphicsManager, TERRA_ColorGrading, TERRA_OS;

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
	Line('varying highp vec4 clip_position;');

  If (FxFlags And shaderVertexColor<>0) Then
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


  If (FogFlags<>0) Then
  Begin
      Line('  varying lowp float fogFactor; ');
  End;

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
	  Line('	mediump vec3 r = normalize(reflect(direction, normal));');
    Line('	mediump float sp = dot(r, normalize(cameraView));');
    Line('  mediump float nDoth = max(sp, 0.0);');
  End;

  If (FxFlags and shaderToonRamp<>0) Then
  Begin
    (*If (FxFlags and shaderSpecular<>0) Then
      Line('  result *= texture2D('+ToonRampUniformName+', vec2(shading, nDoth));')
    Else*)
      //Line('  result *= texture2D('+ToonRampUniformName+', vec2(shading, 0.0));');

      //Line('  result = vec4(shading);');

      Line('  shading = texture2D('+ToonRampUniformName+', vec2(shading, 0.0)).r;');

(*        If (FxFlags and shaderAddSigned<>0) Then
          Line('  result = texture2D('+ToonRampUniformName+', vec2(shading, 0.0)) + (result - 0.5);')
        Else
          Line('  result *= texture2D('+ToonRampUniformName+', vec2(shading, 0.0)).r;');
*)
      //Line('result = vec4(0.5,0.5,0.5,1.0) + (result - 0.5);')

    //Line('  result += 0.5 * texture2D(ToonRamp, vec2(shading, 0.0));');
  End;

  If (FxFlags And shaderCartoonHue<>0) Then
  Begin
    Line('  result = cartoonHueAdjust(result, shading);');
{        Line('  shading = floor(shading*8.0)/8.0;');
        Line('  lowp vec3 rampValue = vec3(shading);');
        Line('  result = rampValue + (result - 0.5);');}
  End Else
  Begin
      Line('  result *= shading;');

    If (FxFlags and shaderSpecular<>0) Then
    Begin
      Line('  float specular_power = 2.2;');
      Line('  lowp vec4 spec = specular * pow(nDoth, specular_power);');
      Line('  result.rgb += spec.rgb;');
    End;
  End;
End;

Procedure ShaderEmitter.VertexUniforms;
Begin
	Line('uniform mat4 cameraMatrix;');
	Line('uniform mat4 modelMatrix;');
	Line('uniform mat4 modelMatrixInverse;');
  Line('uniform mat4 projectionMatrix;');

  If (FxFlags And shaderTextureMatrix<>0) Then
	  Line('uniform mat4 textureMatrix;');

  If (OutFlags And shader_OutputOutline<>0) Then
    Line('  uniform mediump float outlineScale;');

  If (FxFlags and shaderSkinning<>0) Then
    Line('uniform vec4 boneVectors['+IntToString(Succ(MaxBones)*3)+'];');

  //If (FxFlags and shaderNormalMap<>0) Or (FxFlags and shaderGhost<>0) Or (FxFlags and shaderSphereMap<>0) Or (FxFlags and shaderFresnelTerm<>0) Then
  Begin
  	Line('  uniform highp vec3 cameraPosition;');
  End;

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
  	Line('  uniform highp vec4 clipPlane;');
  End;

  If (FogFlags<>0) Then
  Begin
  	Line('  const highp float LOG2 = 1.442695;');

    If (FogFlags And fogDistance<>0) Then
    Begin
    	Line('  uniform highp float fogDistanceCenter;');
    	Line('  uniform highp float fogDistanceSize;');
    End;

    If (FogFlags And fogHeight<>0) Then
    Begin
    	Line('  uniform highp float fogHeightStart;');
    	Line('  uniform highp float fogHeightEnd;');
    End;

    If (FogFlags And fogBox<>0) Then
    Begin
    	Line('  uniform highp vec3 fogBoxAreaStart;');
    	Line('  uniform highp vec3 fogBoxAreaEnd;');
    	Line('  uniform highp float fogBoxSize;');
    End;
  End;
End;

Procedure ShaderEmitter.FragmentUniforms;
Begin
  If (FxFlags And shaderSelfIllumn<>0) Then
    Line('  uniform lowp vec4 illum_color;');

	Line('  uniform lowp sampler2D diffuseMap;');
  If (FxFlags and shaderTriplanar<>0) Then
  	Line('  uniform lowp sampler2D diffuseMap2;');

  If (FxFlags and shaderShadowMap<>0) Then
  Begin
  	Line('  uniform lowp sampler2D '+ShadowMapUniformName+';');
  End;

  If (FxFlags and shaderSpecular<>0) Then
  	Line('  uniform lowp sampler2D '+SpecularMapUniformName+';');

  If (FxFlags and shaderReflectiveMap<>0) Then
  	Line('  uniform lowp sampler2D '+ReflectiveMapUniformName+';');

  If (FxFlags and shaderAlphamap<>0) Then
    Line('  uniform lowp sampler2D '+AlphaMapUniformName+';')
  Else
  If (FxFlags and shaderLightmap<>0) Then
    Line('  uniform lowp sampler2D lightMap;');

  Line('  uniform lowp vec4 sunColor;');

  If (FxFlags and shaderNormalmap<>0) Then
  Begin
  	Line('  uniform lowp sampler2D '+NormalMapUniformName+';');
    If (FxFlags and shaderParallaxBump<>0) Then
    Begin
  	  Line('  uniform lowp sampler2D '+DisplacementMapUniformName+';');
    End;
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

  If (OutFlags And shader_OutputFixedColor<>0) Then
    Line('  uniform mediump vec4 targetColor;');

  {If (Flags And shaderOutputRefraction<>0) Then
  	Line('  uniform lowp sampler2D refractionMap;');}

  If (OutFlags And shader_OutputReflection<>0) Then
    Line('  uniform lowp float reflectionFactor;');

  If (FxFlags and shaderToonRamp<>0) Then
  	Line('  uniform lowp sampler2D '+ToonRampUniformName+';');

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
  	Line('  uniform lowp vec4 fogColor;');
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
	Line('varying highp vec3 screen_position;');

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

  If (FxFlags and shaderLightmap<>0) Or (FxFlags and shaderAlphaMap<>0) Or (FxFlags And shaderFresnelTerm<>0) Then
  	Line('attribute highp vec4 terra_UV1;');

  Line('attribute mediump vec3 terra_normal;');

  If (FxFlags and shaderNormalMap<>0) Then
	  Line('attribute mediump vec4 terra_tangent;');

  If (FxFlags and shaderSkinning<>0) Then
  	Line('attribute highp float terra_bone;');

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

  Line('local_position = terra_position;');

  If (FxFlags and shaderSkinning<>0) Then
  Begin
    Line('    int boneIndex = int(terra_bone);');
    Line('    mat4 boneMatrix = decodeBoneMat(boneIndex);');
    Line('		local_position = boneMatrix * local_position;');
  End;
  
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

  If (FogFlags<>0) Then
  Begin
    If (FogFlags And fogDistance<>0) Then
    Begin
      Line('  highp float zdist = length(world_position.xyz - cameraPosition.xyz);');
      Line('  highp float distanceFactor = (fogDistanceCenter - zdist) / fogDistanceSize;');
      Line('  distanceFactor *= 0.5;');
      Line('  distanceFactor += 0.5;');
      //exp2( -fogDensity * fogDensity * z * z * LOG2 );');
    End Else
      Line('  highp float distanceFactor = 0.0;');

    If (FogFlags And fogHeight<>0) Then
    Begin
      Line('  highp float y = world_position.y;');
      Line('  y = max(y-fogHeight, 0.0) / zFar;');
      Line('  highp float heightFactor = exp2( -fogDensity * fogDensity * y * y * LOG2 );');
    End Else
      Line('  highp float heightFactor = 0.0;');

    If (FogFlags And fogBox<>0) Then
    Begin
      //Line('  highp vec3 fogBoxAreaCenter = (fogBoxAreaStart + fogBoxAreaEnd) * 0.5;');

      Line('      highp float boxDistX1 = abs(world_position.x - fogBoxAreaStart.x); ');
      Line('      highp float boxDistX2 = abs(world_position.x - fogBoxAreaEnd.x); ');
      Line('      highp float boxDistZ1 = abs(world_position.z - fogBoxAreaStart.z); ');
      Line('      highp float boxDistZ2 = abs(world_position.z - fogBoxAreaEnd.z); ');

      Line('  highp float boxDist = min(min(boxDistX1, boxDistX2), min(boxDistZ1, boxDistZ2));');

      Line('  highp float boxFactor = boxDist / fogBoxSize;');
      Line('  boxFactor = min(boxFactor, 1.0);');
    End Else
      Line('  highp float boxFactor = 0.0;');

    Line('  fogFactor = min(distanceFactor + heightFactor + boxFactor, 1.0);');
  End;

  If GraphicsManager.Instance.Renderer.Settings.SurfaceProjection<>surfacePlanar Then
  Begin
    Line(' highp vec4 cyofs = world_position;');
    Line('  cyofs.xyz -= cameraPosition.xyz;');
    Line('  highp float curvatureAmmount = -0.001;');

    If GraphicsManager.Instance.Renderer.Settings.SurfaceProjection = surfaceSpherical Then
      Line('  cyofs = vec4( 0.0, ((cyofs.x * cyofs.x) + (cyofs.z * cyofs.z)) * curvatureAmmount, 0.0, 0.0);')
    Else
      Line('  cyofs = vec4( 0.0, (cyofs.z * cyofs.z) * curvatureAmmount, 0.0, 0.0);');
    Line('  local_position += modelMatrixInverse * cyofs;');
    Line(' world_position = modelMatrix * local_position;');
  End;


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

  Line(' clip_position = projectionMatrix * cameraMatrix * world_position;');
  
  {$IFDEF ANDROID}
  // STUPID HACK FOR GALAXY TAB2, possibly a driver bug, fuck this
  Line('gl_Position = projectionMatrix * cameraMatrix * modelMatrix * local_position;');
  //Line('world_position.w = local_position.w;');
  //Line('gl_Position = projectionMatrix * cameraMatrix * world_position;');
  {$ELSE}
  Line('  gl_Position = clip_position;');
  {$ENDIF}


  If (FxFlags And shaderVertexColor<>0) Then
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

  Line('  mediump vec3 normal;');

  If (FxFlags And shaderWireframe<>0) Then
  Begin
  	Line('void main()	{');
    Line('  color = diffuse_color;');

    If (FogFlags<>0) Then
    Begin
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

  If (FxFlags And shaderCartoonHue<>0) Then
  Begin
  (*
      Line('const lowp mat3 rgb2yiq = mat3(0.299, 0.587, 0.114, 0.595716, -0.274453, -0.321263, 0.211456, -0.522591, 0.311135);');
      Line('const lowp mat3 yiq2rgb = mat3(1.0, 0.9563, 0.6210, 1.0, -0.2721, -0.6474, 1.0, -1.1070, 1.7046);');
    	Line('mediump vec3 ShiftHue(mediump vec3 color, mediump float hueShift)	{');
      Line('vec3 yColor = rgb2yiq * color.rgb;');
      Line('float originalHue = atan(yColor.b, yColor.g);');
      Line('float finalHue = originalHue + hueShift;');
      Line('float chroma = sqrt(yColor.b*yColor.b+yColor.g*yColor.g);');
      Line('vec3 yFinalColor = vec3(yColor.r, chroma * cos(finalHue), chroma * sin(finalHue));');
      Line('return yiq2rgb*yFinalColor;}');*)

    (*
    Line('lowp vec3 rgb2hsv(vec3 c){');
    Line('lowp vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);');
    Line('lowp vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));');
    Line('lowp vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));');
    Line('lowp float d = q.x - min(q.w, q.y);');
    Line('lowp float e = 1.0e-10;');
    Line('return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);}');

    Line('lowp vec3 hsv2rgb(vec3 c){');
    Line('lowp vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);');
    Line('lowp vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);');
    Line('return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}');
      *)

      (*Line('  uniform mediump vec4 hue_low;');
      Line('  uniform mediump vec4 hue_high;');            *)

      Line('  uniform mediump vec4 hue_yellow;');
      Line('  uniform mediump vec4 hue_green ;');
      Line('  uniform mediump vec4 hue_purple;');
      Line('  uniform mediump vec4 hue_black;');
    	Line('mediump vec4 cartoonHueAdjust(mediump vec4 color, mediump vec4 shade)	{');

        (*
        Line('lowp vec4 target_mid = shade * color;');
        Line('lowp float gray = dot(target_mid.rgb, vec3(0.299, 0.587, 0.114)); ');


        Line('lowp vec3 hue_color = rgb2hsv(target_mid.rgb);');
        Line('lowp vec4 target_low = vec4(hue_low.x, (hue_low.y + hue_color.y) * 0.5, hue_color.z, color.a);');
        Line('lowp vec4 target_high = vec4(hue_high.x, (hue_high.y + hue_color.y) * 0.5, hue_color.z, color.a);');

        Line('target_low.rgb = hsv2rgb(target_low.rgb);');
        Line('target_high.rgb = hsv2rgb(target_high.rgb);');

        Line('lowp vec4 weight_low;');
        Line('lowp vec4 weight_high;');
        //lowp float g = hue_color.z;');

        Line('if (gray<=0.5) { weight_low = 1.0 - (shade * 2.0);  weight_high = vec4(0.0); } ');
        Line('else { weight_high = ((shade - 0.5) * 2.0);  weight_low = vec4(0.0); }');

        Line(' lowp vec4 weight_mid = vec4(1.0) - (weight_low + weight_high);');
        Line('return target_low * weight_low + target_mid * weight_mid + target_high * weight_high; }');


        //Line('return target_low;}');
          *)
(*      Line('  color = ShiftHue(color, -90.0);');
      Line('return color; }');*)


(*      Line('  mediump vec3 yellow = vec3(1.0, 1.0, 0.5);');
      Line('  mediump vec3 purple = vec3(1.0, 0.4, 1.0);');
      Line('  mediump vec3 green = vec3(0.25, 1.0, 0.25);');
      *)


      Line('  mediump vec4 SA = mix(hue_green, hue_yellow, shade);');
      Line('  mediump vec4 SB = mix(hue_purple, hue_yellow, shade);');

      Line(' lowp float gray = 1.0 - dot(shade.rgb, vec3(0.299, 0.587, 0.114));');
      Line('   if (gray<0.75) { gray = 0.0;} else { gray -= 0.75; gray *= 4.0;} ');

      Line('  shade -= 0.5;');
      Line('  shade *= 0.5;');
      Line('  mediump vec4 temp = clamp(color + shade * SA, 0.0, 1.0);	');
      Line('  lowp vec4 result = mix(SB, temp, 0.75);');
      Line('return max(vec4(0.0), result - hue_black * gray);}');



      (*Line('lowp vec4 target_mid = shade * color;');
      Line('lowp float target_gray = dot(target_mid.rgb, vec3(0.299, 0.587, 0.114)); ');
      Line('  lowp flo distance = abs(0.5 - target_gray) * 2.0;');
      Line('  return mix(SB, temp, distance); }');
       *)

(*      Line('     gray = max(gray, 0.75);');
     *)
      //Line('return mix(result, target_mid, gray); }');
      //Line('return SB; }');
      //Line('return temp; }');

  End;

  If (OutFlags And shader_OutputDiffuse<>0) Then
  Begin
//    If (LightModel <> lightModelSimple) Then
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
      End;

      Line('  return result * cookieColor * shading;	}');
    End;

    If (Lights.PointLightCount>0) Then
    Begin
      Line('lowp vec4 pointLight(highp vec3 lightPosition, lowp vec4 lightColor, highp float radius){');
      Line('  highp vec3 direction = lightPosition - world_position.xyz;');
      Line('  highp float dist = length(direction);');
      Line('  highp float att = 1.0 - min(dist*radius, 1.0);');
      Line('  direction /= dist;');

      Line('  return halfDot(normal, direction) * att * lightColor;}');
    End;

  // 8x8 Bayer ordered dithering pattern.
  // Each input pixel is scaled to the 0..63 range
    (*Line('int dither[64] = {0, 32, 8, 40, 2, 34, 10, 42,');
    Line('48, 16, 56, 24, 50, 18, 58, 26,');
    Line('12, 44, 4, 36, 14, 46, 6, 38,');
    Line('60, 28, 52, 20, 62, 30, 54, 22,');
    Line(' 3, 35, 11, 43, 1, 33, 9, 41,');
    Line('51, 19, 59, 27, 49, 17, 57, 25,');
    Line('15, 47, 7, 39, 13, 45, 5, 37,');
    Line('63, 31, 55, 23, 61, 29, 53, 21};');
    {Line('int x = int(mod(uv.x * dither_scale, 8));');
    Line('int y = int(mod(uv.y * dither_scale, 8));');
    Line('float limit = (dither[x + y * 8]+1)/64.0;');}
    *)

    If (FxFlags And shaderDitherColor<>0) Then
    Begin
      Line('  uniform lowp sampler2D '+DitherPatternMapUniformName+';');

      Line('  uniform mediump float '+DitherScaleUniformName+';');
      Line('float dither_shade(float shade, vec2 uv){');
      Line('float limit = texture2D('+DitherPatternMapUniformName+', uv * '+DitherScaleUniformName+').r;');
      Line('if (shade < limit) return 0.0;');
      Line('return 1.0;}');

      (*Line('vec4 dither_color(vec4 shade, vec2 uv){');
      Line('vec4 limit = texture2D(ditherTexture, uv * dither_scale);');
      Line('float dr = 1.0; if (shade.r < limit.r) dr =0.0;');
      Line('float dg = 1.0; if (shade.g < limit.g) dg =0.0;');
      Line('float db = 1.0; if (shade.b < limit.b) db =0.0;');
      Line('return vec4(dr, dg, db, 1.0);}');*)
    End;


    If (Lights.DirectionalLightCount>0) Then
    Begin
      If (FxFlags And shaderDitherColor<>0) Then
        S2 := ', highp vec2 localUV, lowp vec2 colorIndex'
      Else
        S2 := '';

    	  Line('lowp vec4 directionalLight(highp vec3 direction, lowp vec4 lightColor'+S2+'){');

      Line('  mediump float shading = halfDot(normal, direction);');

      If (FxFlags And shaderDitherColor<>0) Then
      Begin
        (*Line('	vec4 greyA = texture2D('+DitherPaletteMapUniformName+', colorIndex);');
        Line('	result = greyA;');*)

        //Line(' shading += colorIndex.x;');
        Line('shading += (colorIndex.x - 0.5);');

        //Line(' shading *= 0.5;');

        Line('	lowp float shade_count = 8.0;');
        Line('	lowp float shade_value = shading * shade_count;');
        Line('	shade_value = min(shade_count, shade_value + 1.0); ');
        Line('  float dither_factor = fract(shade_value);');

(*        Line('	float shadeA = floor(shading);');
        Line('	float shadeB = max(shadeA - 1.0, 0.0);');

        Line('	vec4 greyA = texture2D('+DitherPaletteMapUniformName+', vec2(shadeA/ shade_count, colorIndex.y));');
        Line('	vec4 greyB = texture2D('+DitherPaletteMapUniformName+', vec2(shadeB/ shade_count, colorIndex.y));');
        *)

        (*Line('  lowp vec3 blend_weights = abs(normal);');
        Line('  blend_weights = blend_weights - 0.2679f;');
        Line('  lowp float blend_factor = 2.0;');
        Line('  highp vec2 ditherPos1 = local_position.xz;');
        Line('  highp vec2 ditherPos2 = local_position.yz;');
        Line('  highp vec2 ditherPos3 = local_position.xy;');
        Line('  highp vec2 ditherUV = ditherPos2 * blend_weights.x + ditherPos1 * blend_weights.y + ditherPos3 * blend_weights.z;');
        Line(' ditherUV *= blend_factor;');*)

        Line('	float sub_shading = dither_shade(dither_factor, localUV);');
        //Line('	lowp vec4 result = mix(greyB, greyA, sub_shading);');

        Line('	lowp vec4 result = vec4(shading, colorIndex.y, sub_shading, 1.0);'); // FIX THIS WE NOW USE SHADOWS NOT COLORS!

        //Line('	result = (greyA + result) * 0.5;');
        //Line('	result = greyA;');

        //Line('	sub_shading = dither_shade(0.5, localUV);');

        // debug
        //Line('	result = vec4(sub_shading, sub_shading, sub_shading, 1.0);');
        //Line('	result = vec4(shading, shading, shading, 1.0);');
      End Else
      Begin
        Line('  lowp vec4 result = shading * lightColor;');
      End;


  	  Line('	return result;}');
    End;
  End;

  If (FxFlags And shaderColorTable<>0) Then
    Line(GetColorTableShaderCode());

(*  If (OutFlags And shader_OutputDiffuse<>0) Then
  Begin
    Line('vec4 screen_blend(vec4 target, vec4 blend){');
    Line('return 1.0 - (1.0 - target) * (1.0 - blend);}');
  End;*)

	Line('void main()	{');

  If (FxFlags and shaderShadowMap<>0) Or (OutFlags <>shader_OutputDiffuse) Then
  Begin
    Line('  mediump vec3 screen_position = clip_position.xyz / clip_position.w;');
    Line('  screen_position *= vec3(0.5);');
    Line('  screen_position += vec3(0.5);');
  End;
  
  {$IFDEF SIMPLESHADER}
  Line('  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);}');
  {$ELSE}

  If (FxFlags and shaderClipPlane<>0) Then
  Begin
    Line('  if (clipDistance>0.0) discard;');
  End;

  If (OutFlags And shader_OutputFixedColor<>0) Then
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

    Line('highp vec2 localUV = texCoord0.st;');
    If (FxFlags and shaderParallaxBump<>0) Then
    Begin
      Line('float parallaxScale = 0.025;');
      Line('float parallaxBias = -0.5;');
      Line('float height = texture2D('+DisplacementMapUniformName+', localUV).r;');
		  Line('float altitude = height + parallaxBias;');
		  Line('localUV  += altitude * parallaxScale *normalize(cameraView).xy;');
    End;


    Line('  normal = normalize(vertex_normal);');

    If (FxFlags and shaderNormalMap<>0) Then
    Begin
      If (FxFlags and shaderFresnelTerm <> 0) Then
        Line('  highp vec2 normalUV = lightCoord.st;')
      Else
        Line('  highp vec2 normalUV = localUV;');

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

//  OutFlags := shader_OutputNormal;
  If (OutFlags And shader_OutputOutline<>0) Then
  Begin
        Line('  diffuse = texture2D(diffuseMap, localUV);');

        If (FxFlags And shaderVertexColor<>0) Then
           Line('  diffuse *= vertex_color; ');

        Line('diffuse *= diffuse_color;');
        If (FxFlags and shaderAlphaTest<>0) Then
          Line('  if (diffuse.a<0.1) discard;');
//    Line('  color.rgb = diffuse.rgb * 0.333;');
        Line('  color = outlineColor;');

    Line('  color.a = diffuse.a;');
//    Line('  color = vec4(0.0, 0.0, 0.0, 1.0);');
  End Else
  If (OutFlags And shader_OutputRefraction<>0) Then
  Begin
    Line('  color.rgb = normal * 0.5 + 0.5;');

    Line('  diffuse = texture2D(diffuseMap, localUV);');

    If (FxFlags And shaderVertexColor<>0) Then
       Line('  diffuse *= vertex_color; ');

    If (FxFlags and shaderAlphaTest<>0) Then
      Line('  if (diffuse.a<0.1) discard;');

    Line('  color.a = 1.0;'); // refraction ammount
  End Else
  If (OutFlags And shader_OutputNormal<>0) Then
  Begin
    Line('  color.rgb = normal * 0.5 + 0.5;');

    Line('  diffuse = texture2D(diffuseMap, localUV);');

    If (FxFlags And shaderVertexColor<>0) Then
       Line('  diffuse *= vertex_color; ');

    //If (Flags and shaderAlphaTest<>0) Then
    Line('  if (diffuse.a<0.3) discard;');

    Line('  highp float zz = screen_position.z;');
    Line('  zz = (2.0 * zNear) / (zFar + zNear - zz * (zFar - zNear));');
    Line('  color.a = zz;');
    Line('  color.a = 1.0;');
  End Else
  If (OutFlags And shader_OutputGlow<>0) Then
  Begin
    Line('  color = texture2D(glowMap, localUV);');
    Line('  color.a = 1.0;');
  End Else                                
  If (OutFlags And shader_OutputShadow<>0) Then
  Begin
    Line('  diffuse = texture2D(diffuseMap, localUV);');

    If (FxFlags and shaderLightmap<>0) Then
    Begin
      Line('  color = texture2D(lightMap, lightCoord.st);');

(*      If (FxFlags and shaderAddSigned<>0) Then
      Begin
        Line('  color += (color - 0.5);');
      End;*)

      Line('  color.a = diffuse.a;');
    End Else
      Line('  color = vec4(diffuse_color.r, diffuse_color.g, diffuse_color.b, diffuse.a);');
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
      Line('  color = texture2D('+ReflectionMapUniformName+', localUV) *  reflectionFactor;');
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
      Line('  highp vec4 diff1 = texture2D(diffuseMap, localUV + flowOffset1);');
      Line('  highp vec4 diff2 = texture2D(diffuseMap, localUV + flowOffset2);');
      Line('  diffuse = mix(diff1, diff2, flowCycle.y);');
    End Else
      Line('  diffuse = texture2D(diffuseMap, localUV);');

    If (FxFlags And shaderDecalMap<>0) Then
    Begin
      Line('lowp vec4 decalColor = texture2D('+DecalMapUniformName+', localUV);');
      Line('  diffuse.rgb = mix(diffuse.rgb, decalColor.rgb, decalColor.a);');
    End;

    If (FxFlags And shaderVertexColor<>0) Then
    Begin
(*         If (FxFlags and shaderAddSigned<>0) Then
            Line('diffuse.rgb += (vertex_color.rgb - 0.5);')
         Else*)
            Line('  diffuse *= vertex_color; ');
    End;

    If (FxFlags and shaderAlphaTest<>0) Then
      Line('  if (diffuse.a<0.1) discard;');

    If (FxFlags and shaderSpecular<>0) Then
      Line('  specular = texture2D('+SpecularMapUniformName+', localUV);');

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
        Line('  lowp vec4 refpow = texture2D('+ReflectiveMapUniformName+', localUV);');
        Line('  diffuse.rgb = mix(diffuse, reflection, refpow).rgb;');
        //Line('  diffuse = refpow;');
      End Else
      If (FxFlags and shaderFresnelTerm = 0) Then
        Line('  diffuse *= reflection;');
    End;

    If (FxFlags And shaderDitherColor<>0) Then
    Begin
    End Else
    Begin
      Line('diffuse *= diffuse_color;');
    End;

    If (FxFlags and shaderShadowMap<>0) Then
    Begin
      Line('lowp float shadow = texture2D('+ShadowMapUniformName +', screen_position.xy).r;');
    End Else
    Begin
      Line('lowp float shadow = 1.0;');
    End;
    Line('lowp vec4 lightAccum = vec4(0.0);');

    If (FxFlags And shaderDitherColor<>0) Then
    Begin
      Line('  highp vec2 colorIndex = vec2(diffuse.g, diffuse.r);');
      I := 1;

      If (FxFlags and shaderLightmap<>0) Then
      Begin
        Line('  lowp float intensity = texture2D(lightMap, lightCoord.st).r;');
        Line('  colorIndex.x += (intensity - 0.5);');
      End;

      If Lights.DirectionalLightCount>0 Then
        Line('  color = directionalLight(dlightDirection'+IntToString(I)+', dlightColor'+IntToString(I)+', localUV, colorIndex);');
    End Else
    If (FxFlags And shaderSelfIllumn<>0) Then
    Begin
      Line('  color = diffuse * illum_color;');
    End Else
    Begin
      For I:=1 To Lights.DirectionalLightCount Do
        Line('  lightAccum += directionalLight(dlightDirection'+IntToString(I)+', dlightColor'+IntToString(I)+');');

      For I:=1 To Lights.PointLightCount Do
        Line('  lightAccum += pointLight(plightPosition'+IntToString(I)+', plightColor'+IntToString(I)+', plightRadius'+IntToString(I)+');');

      For I:=1 To Lights.SpotLightCount Do
        Line('  lightAccum += spotLight(slightPosition'+IntToString(I)+', slightColor'+IntToString(I)+', slightDirection'+IntToString(I)+', slightCosInnerAngle'+IntToString(I)+', slightCosOuterAngle'+IntToString(I)+', slightMatrix'+IntToString(I)+', slightCookie'+IntToString(I)+');');

        If (FxFlags And shaderCartoonHue<>0) Then
          Line('  color = cartoonHueAdjust(diffuse * shadow, lightAccum * shadow);')
        Else
          Line('  color = diffuse * shadow * lightAccum;');
        //Line('  color = color * shadow + screen_blend(vec4(shadow), color);');
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
    If ((FxFlags And shaderDitherColor)<>0) Then
    Begin
      Line('  if (diffuse.a<1.0) ');
      Line('  color.a = 0.5 * dither_shade(diffuse.a, localUV);');
    End Else
      Line('  color.a = diffuse.a;');

//  If (OutFlags And shader_OutputDiffuse<>0) And (FxFlags and shaderShadowMap<>0) Then
  //  Line('  color.rgb = vec3(shadow);');

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


  If (FogFlags<>0) Then
  Begin
    Line('  color = mix(fogColor, color, fogFactor);');
  End;

  //Line('  color.rgb = pow(color, vec3(1.0 / 2.2));');
    //Line('  color.rgb = normal * 0.5 + 0.5;');
    //Line('  color.rgb = vec3(1.0, 0.0, 0.0);');

  {$IFDEF DEBUG_LIGHTMAP}
    If (FxFlags and shaderLightmap<>0) Then
    Begin
      //Line('  color.rgb = vertex_color.rgb; ');
      Line('  color.rgb = texture2D(lightMap, lightCoord.st).rgb;');
    End;
  {$ENDIF}

  If (FxFlags And shaderColorTable<>0) Then
    Line('  color.rgb = ColorTableLookup(color.rgb);');

  If (FxFlags And shaderScreenMask<>0) Then
  Begin
    //Line('  color.rgb = texture2D(screenMask, spos.st).rgb;');
    //Line('  color.rgb = vec3(spos.x, 0.0, spos.y);');
    Line('  color.a = texture2D(screenMask, screen_position.xy).r;');
  End;

{  If (OutFlags And shader_OutputDiffuse<>0) And (BlendMode<>combineNone) Then
  Begin
    Line('lowp vec3 baseColor = texture2D('+ColorMapUniformName +', screen_position.xy).rgb;');
  	//Line('  color.rgb = mix(baseColor, color.rgb, color.a);');
    Line('  color.rgb = 1.0 - baseColor;');
  End;}

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

Function ShaderFactory.GetShader(FxFlags, OutFlags, FogFlags, LightModel:Cardinal; Const Lights:LightBatch):ShaderInterface;
Var
  I:Integer;
  S:ShaderEntry;
  Location:TERRAString;
  SS, Name:TERRAString;
//  BlendMode:ColorCombineMode;
Begin
{  If GraphicsManager.Instance.Renderer.ActiveBlendMode Then
    BlendMode := combineBlend
  Else
    BlendMode := combineNone;}

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'ShaderFactory', 'Searching for shader with flags '+CardinalToString(FXFlags));
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

  If (FxFlags And shaderToonRamp<>0) Then
    name := name + '_TOONRAMP;';

  If (FxFlags And shaderSelfIllumn<>0) Then
    name := name + '_SELFILLUM;';

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

  If (OutFlags And shader_OutputDiffuse<>0) Then
    name := name + '_OUTDIFFUSE;';

  If (OutFlags And shader_OutputNormal<>0) Then
    name := name + '_OUTNORMAL;';

  If (OutFlags And shader_OutputSpecular<>0) Then
    name := name + '_OUTSPECULAR;';

  If (OutFlags And shader_OutputGlow<>0) Then
    name := name + '_OUTGLOW;';

  If (OutFlags And shader_OutputFixedColor<>0) Then
    name := name + '_OUTCOLOR;';

  If (OutFlags And shader_OutputRefraction<>0) Then
    name := name + '_OUTREFRACTION;';

  If (OutFlags And shader_OutputShadow<>0) Then
    name := name + '_OUTSHADOW;';

  If (OutFlags And shader_OutputOutline<>0) Then
    name := name + '_OUTLINE;';

  If (OutFlags And shader_OutputReflection<>0) Then
    name := name + '_OUTREFLECTION;';

  If (FxFlags And shaderFresnelTerm<>0) Then
    name := name + '_FRESNEL;';

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

  S.Shader := GraphicsManager.Instance.Renderer.CreateShader();
  S.Shader.Generate(Name, SS);
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
    _Shaders[I].Shader.Invalidate();
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
