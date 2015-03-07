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
 * TERRA_Renderer
 * Implements a generic renderer
 ***********************************************************************************************************************
}
Unit TERRA_Renderer;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_OS, TERRA_Image;

Const
  textureFilteringPoint     = 0;
  textureFilteringBilinear  = 1;

Type
  Renderer = Class;

  GraphicInterface = Class(TERRAObject)
    Protected
      _Owner:Renderer;
      _Size:Cardinal;
      _Valid:Boolean;

    Public
      Property Size:Cardinal Read _Size;
      Property Owner:Renderer Read _Owner;
      Property Valid:Boolean Read _Valid;
  End;

  TextureInterface = Class(GraphicInterface)
    Protected

    Public
      Function Generate(Pixels:Pointer; Width, Height:Integer):Boolean; Virtual; Abstract;

      Procedure Update(Pixels:Pointer; X,Y, Width, Height:Integer); Virtual; Abstract;
      Function Read():Image; Virtual; Abstract;

      Procedure Bind(Slot:Integer); Virtual; Abstract;
      Procedure SetSettings(FilterMode:Integer; MipMapping, Wrap:Boolean); Virtual; Abstract;
  End;

  VertexBufferInterface = Class(GraphicInterface)
    Protected
      Procedure Init(); Virtual;Abstract;
      Procedure Submit(); Virtual;Abstract;

    Public
      Function Generate(VertexData, IndexData, EdgeData:Pointer; VertexCount, TriangleCount:Integer; VertexSize:Integer; DynamicUsage:Boolean):Boolean;
      Procedure Release; Override;

      Procedure AddAttribute(Name:TERRAString; Count:Integer; Format:Integer; Normalized:Boolean; Skip:Boolean = False);
      Procedure SetIndexList(IndexList:Pointer; TriangleCount:Integer);

      Procedure Draw(Wireframe:Boolean);

      Procedure Update(Data:PByte); Virtual; Abstract;
  End;

  ShaderInterface = Class(GraphicInterface)
    Protected

    Public
      Function Generate(VertexCode, FragmentCode:TERRAString):Boolean; Virtual; Abstract;

      Procedure Bind(); Virtual; Abstract;

			Procedure SetIntegerUniform(Const Name:TERRAString; Const Value:Integer); Virtual; Abstract;
			Procedure SetFloatUniform(Const Name:TERRAString; Const Value:Single); Virtual; Abstract;
			Procedure SetVec2Uniform(Const Name:TERRAString; Const Value:Vector2D); Virtual; Abstract;
			Procedure SetVec3Uniform(Const Name:TERRAString; const Value:Vector3D); Virtual; Abstract;
			Procedure SetVec4Uniform(Const Name:TERRAString; const Value:Quaternion); Virtual; Abstract;
      Procedure SetMat3Uniform(Const Name:TERRAString; Value:Matrix2D); Virtual; Abstract;
      Procedure SetMat4Uniform(Const Name:TERRAString; Value:Matrix); Virtual; Abstract;

      Function GetUniform(Name:TERRAString):Integer; Virtual; Abstract;
  End;

	GraphicsManagerSetting = Object
		Protected
			_Enabled:Boolean;
			_Avaliable:Boolean;

		Public
      Function Enabled:Boolean;
      Function Avaliable:Boolean;

      Procedure SetValue(Value:Boolean);
	End;

	RendererVariableSetting = Object
		Protected
			_Quality:Integer;

      Procedure SetQuality(Value:Integer);

		Public
      Property Quality:Integer Read _Quality Write SetQuality;
	End;

	RendererSettings = Class(TERRAObject)
    Protected
      _MaxAnisotrophy:Integer;
			_MaxTextureSize:Integer;
      _MaxTextureUnits:Integer;
      _multiSampleCount:Integer;
      _maxRenderTargets:Integer;
      _MaxUniformVectors:Integer;
      _Changed:Boolean;

      _ShadowSplitCount:Integer;
      _ShadowSplitWeight:Single;
      _ShadowMapSize:Integer;
      _ShadowBias:Single;

      _VertexCacheSize:Integer;

    Public
		  FrameBufferObject:GraphicsManagerFeature;
  		CubemapTexture:GraphicsManagerFeature;
      FloatTexture:GraphicsManagerFeature;
  		TextureArray:GraphicsManagerFeature;
      SeparateBlends:GraphicsManagerFeature;
      SeamlessCubeMap:GraphicsManagerFeature;
      PackedStencil:GraphicsManagerFeature;
      NPOT:GraphicsManagerFeature;
      Shaders:GraphicsManagerFeature;

      AlphaFade:GraphicsManagerVariableSetting;
      DynamicShadows:GraphicsManagerVariableSetting;
      Textures:GraphicsManagerVariableSetting;
  		TextureCompression:GraphicsManagerVariableSetting;
      DeferredLighting:GraphicsManagerSetting;
      DeferredFog:GraphicsManagerSetting;
      DeferredShadows:GraphicsManagerSetting;
      SelfShadows:GraphicsManagerSetting;
      DepthOfField:GraphicsManagerSetting;
      PostProcessing:GraphicsManagerSetting;
      NormalMapping:GraphicsManagerSetting;
      LightMapping:GraphicsManagerSetting;
      ToonShading:GraphicsManagerSetting;
      AlphaTesting:GraphicsManagerSetting;
      Specular:GraphicsManagerSetting;
      Fur:GraphicsManagerSetting;
      Sky:GraphicsManagerSetting;
      Reflections:GraphicsManagerSetting;
      DynamicLights:GraphicsManagerSetting;
      SSAO:GraphicsManagerSetting;
	  	VertexBufferObject:GraphicsManagerSetting;

      FogMode:Integer;
      FogColor:Color;
      FogDensity:Single;
      FogStart:Single;
      FogHeight:Single;

      Procedure Release; Override;

      Property ShadowSplitCount:Integer Read _ShadowSplitCount Write _ShadowSplitCount;
      Property ShadowSplitWeight:Single Read _ShadowSplitWeight Write _ShadowSplitWeight;
      Property ShadowMapSize:Integer Read _ShadowMapSize Write _ShadowMapSize;
      Property ShadowBias:Single Read _ShadowBias Write _ShadowBias;

      Property MaxAnisotrophy:Integer Read _MaxAnisotrophy;
			Property MaxTextureSize:Integer Read _MaxTextureSize;
      Property MaxTextureUnits:Integer Read _MaxTextureUnits;
      Property MultiSampleCount:Integer Read _multiSampleCount;
      Property VertexCacheSize:Integer Read _VertexCacheSize;
	End;

  Renderer = Class(TERRAObject)
    Function CreateTexture():TextureInterface; Virtual; Abstract;
    Function CreateVertexBuffer():VertexBufferInterface; Virtual; Abstract;
  End;


Implementation

End.