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

// combine shader.setintuniform and texture.bind into one call!!!

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_OS, TERRA_Collections, TERRA_Image, TERRA_VertexFormat,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Matrix3x3, TERRA_Matrix4x4,
  TERRA_Plane, TERRA_BoundingBox, TERRA_Color, TERRA_List;

Const
  MaxTextureHandles = 2048;
  MaxFrameBufferHandles = 128;

  textureFilteringPoint     = 0;
  textureFilteringBilinear  = 1;

  cubemap_PositiveX = 0;
  cubemap_NegativeX = 1;
  cubemap_PositiveY = 2;
  cubemap_NegativeY = 3;
  cubemap_PositiveZ = 4;
  cubemap_NegativeZ = 5;

  blendNone     = 0;
  blendBlend    = 1;
  blendAdd      = 2;         //GL_ONE  GL_ONE
  blendFilter   = 3;      //GL_DST_COLOR GL_ZERO
  blendModulate = 4;
  blendJoin     = 5;
  blendZero     = 6;
  blendOne      = 7;
  blendColor    = 8;
  blendColorAdd    = 9;
  blendReflection  = 10;

  clearColor    = 1;
  clearDepth    = 2;
  clearStencil  = 4;
  clearAll      = clearColor Or clearDepth Or clearStencil;

Const
  CubeFaceNames:Array[0..5] Of TERRAString = ( 'right', 'left', 'up','down', 'front', 'back');

  RenderCaptureTargets = 8;
  TotalCaptureTargets = 11;

Type
  RenderTargetType = (
    captureTargetInvalid  = -1,
    captureTargetColor    = 0,
    captureTargetNormal   = 1,
    captureTargetEmission = 2,
    captureTargetRefraction = 3,
    captureTargetOutline = 4,
    captureTargetReflection = 5,
    captureTargetShadow = 6,
    captureTargetPosition = 7,
    effectTargetGlow  = 8,
    effectTargetBloom  = 9,
    effectTargetEdge  = 10
  );


  RendererStage = (
    renderStageDiffuse      = 1,
    renderStageNormal       = 2,
    renderStageGlow         = 4,
    renderStageRefraction   = 8,
    renderStageOutline      = 16,
    renderStageReflection   = 32,
    renderStageShadow       = 64
//    renderStageAlpha        = 128
  );

  GraphicsRenderer = Class;

  RendererQuality = (qualityDisabled, qualityLow, qualityMedium, qualityHigh);

  StatType = (statTriangles, statShaders, statRenderables, statLights, statOccluders);

  CullMode = (cullNone, cullFront, cullBack);

  CompareMode = (compareNever, compareLess, compareEqual, compareLessOrEqual,
          compareGreater, compareDifferent, compareGreaterOrEqual, compareAlways);

  StencilOperation = (stencilKeep, stencilReplace, stencilIncrement, stencilDecrement,
    stencilInvert, stencilIncrementWithWrap, stencilDecrementWithWrap);

  RenderPrimitive = (renderPoints, renderLines, renderTriangles, renderLineStrip, renderTriangleStrip);

  PixelSizeType = (pixelSizeByte, pixelSizeFloat);

  TextureFilterMode = (filterLinear, filterBilinear);
  TextureWrapMode = (wrapNothing = 0, wrapVertical = 1, wrapHorizontal  = 2, wrapAll = 3);
  TextureColorFormat = (textureFormat_RGB, textureFormat_RGBA, textureFormat_BGR, textureFormat_BGRA, textureFormat_Alpha);

  SurfaceProjectionMode = (surfacePlanar, surfaceCylindrical, surfaceSpherical);

  SurfaceOrigin = (surfaceTopLeft, surfaceBottomRight);

  (*BlendOperation = (blendOne, blendZero,
                    blendSourceColor, blendSourceAlpha, blendOneMinusSourceColor, blendOneMinusSourceAlpha,
                    blendDestColor, blendDestAlpha, blendOneMinusDestColor, blendOneMinusDestAlpha);*)

  RendererState = Class(TERRAObject)
    StencilTest:Boolean;
    StencilMode:CompareMode;
    StencilID:Cardinal;
    StencilMask:Cardinal;
    StencilFailOP:StencilOperation;
    StencilZFailOP:StencilOperation;
    StencilZPassOp:StencilOperation;

    ColorMask:Cardinal;

    DepthMask:Boolean;
    DepthTest:Boolean;
    DepthMode:CompareMode;

    Cull:CullMode;

    Blend:Integer;
    
  End;

  GraphicInterface = Class(TERRAObject)
    Protected
      _Owner:GraphicsRenderer;
      _Context:Integer;
      _SizeInBytes:Cardinal;

      Procedure Initialize(); Virtual;

      Function IsComplete():Boolean; Virtual;

    Public
      Constructor Create(Owner:GraphicsRenderer);
      Procedure Release(); Override;

      Function IsValid():Boolean;

      Procedure Invalidate(); Virtual; Abstract;

      Property SizeInBytes:Cardinal Read _SizeInBytes;
      Property Owner:GraphicsRenderer Read _Owner;
      Property Valid:Boolean Read IsValid;
  End;

  SurfaceInterface = Class(GraphicInterface)
    Protected
      _Width:Cardinal;
      _Height:Cardinal;

      _WrapMode:TextureWrapMode;
      _MipMapped:Boolean;
      _Filter:TextureFilterMode;

      Function GetOrigin:SurfaceOrigin; Virtual;

    Public
      Function Bind(Slot:Integer):Boolean; Virtual; Abstract;

      Function GetImage():Image; Virtual;
      Function GetPixel(X,Y:Integer):ColorRGBA; Virtual;

      Procedure SetFilter(Value:TextureFilterMode); Virtual;
      Procedure SetWrapMode(Value:TextureWrapMode); Virtual;
      Procedure SetMipMapping(Value:Boolean); Virtual;

      Property Width:Cardinal Read _Width;
      Property Height:Cardinal Read _Height;

      Property WrapMode:TextureWrapMode Read _WrapMode Write SetWrapMode;
      Property MipMapped:Boolean Read _MipMapped Write SetMipMapping;
      Property Filter:TextureFilterMode Read _Filter Write SetFilter;

      Property Origin:SurfaceOrigin Read GetOrigin;
  End;

  TextureInterface = Class(SurfaceInterface)
    Protected

    Public
      Function Generate(Pixels:Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Virtual; Abstract;

      Function Update(Pixels:Pointer; X,Y, Width, Height:Integer):Boolean; Virtual; 
  End;

  RenderTargetInterface = Class(SurfaceInterface)
    Protected
      _BackgroundColor:ColorRGBA;

      _HasDepthBuffer:Boolean;
      _HasStencilBuffer:Boolean;

      _PixelSize:PixelSizeType;

    Public
      Function Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer, StencilBuffer:Boolean):Boolean; Virtual; Abstract;

	    // Render to this target
	    Procedure BeginCapture(Flags:Cardinal = clearAll); Virtual; Abstract;
	    Procedure EndCapture; Virtual; Abstract;

      Procedure SetBackgroundColor(Const Value:ColorRGBA); Virtual;

      Procedure Resize(NewWidth, NewHeight:Integer); Virtual; Abstract;

      Property BackgroundColor:ColorRGBA Read _BackgroundColor Write SetBackgroundColor;

      Property PixelSize:PixelSizeType Read _PixelSize;
  End;

  CubeMapInterface = Class(SurfaceInterface)
    Protected

      Procedure Initialize(); Override;

    Public
      Function Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Virtual; Abstract;

      Function UpdateFace(FaceID:Integer; Pixels:Pointer; X,Y, Width, Height:Integer):Boolean; Virtual;

      Function LoadFromFile(Const FileName:TERRAString):Boolean;
  End;

  ShaderAttribute = Record
    Name:TERRAString;
    Handle:Integer;
  End;


  ShaderInterface = Class(GraphicInterface)
    Protected
      _Name:TERRAString;

      _Attributes:Array Of ShaderAttribute;
      _AttributeCount:Integer;

      Procedure AddAttributes(Source:TERRAString);

      Function Bind():Boolean; Virtual; Abstract;
      Function Unbind():Boolean; Virtual; Abstract;


    Public
      Function Generate(Const Name:TERRAString; ShaderCode:TERRAString):Boolean; Virtual; Abstract;

      Function IsReady():Boolean; Virtual; Abstract;

			Procedure SetIntegerUniform(Const Name:TERRAString; Const Value:Integer); Virtual; Abstract;
			Procedure SetFloatUniform(Const Name:TERRAString; Const Value:Single); Virtual; Abstract;
			Procedure SetVec2Uniform(Const Name:TERRAString; Const Value:Vector2D); Virtual; Abstract;
			Procedure SetVec3Uniform(Const Name:TERRAString; const Value:Vector3D); Virtual; Abstract;
			Procedure SetVec4Uniform(Const Name:TERRAString; const Value:Vector4D); Virtual; Abstract;
      Procedure SetMat3Uniform(Const Name:TERRAString; Value:Matrix3x3); Virtual; Abstract;
      Procedure SetMat4Uniform(Const Name:TERRAString; Value:Matrix4x4); Virtual; Abstract;
      Procedure SetVec4ArrayUniform(Const Name:TERRAString; Count:Integer; Values:PVector4D); Virtual; Abstract;

			Procedure SetColorUniform(Const Name:TERRAString; Const Value:ColorRGBA);
			Procedure SetPlaneUniform(Const Name:TERRAString; Const Value:Plane);

      Function HasUniform(Const Name:TERRAString):Boolean; Virtual; 

      Function GetUniform(Name:TERRAString):Integer; Virtual; Abstract;
      Function GetAttributeHandle(Const Name:TERRAString):Integer; Virtual; Abstract;
  End;

  TriangleEdgesState = Record
    Visible:Array[0..2] Of Boolean;
  End;

  PTriangleEdgesStateArray = ^TriangleEdgesStateArray;
  TriangleEdgesStateArray = Array[0..1024*64] Of TriangleEdgesState;

  VertexAttribute = Record
    Name:TERRAString;
    Handle:Integer;
    Count:Integer;
    Size:Integer;
    Format:DataFormat;
    Normalized:Boolean;
    Offset:Integer;
  End;

  VertexBufferInterface = Class(GraphicInterface)
    Protected
      _Dynamic:Boolean;
      _Vertices:VertexData;
      _TriangleCount:Integer;
      _EdgeCount:Integer;
      _IndexList:PTriangleArray;
      _EdgeList:PTriangleEdgesStateArray;
      _WireframeIndices:PWordArray;

      Procedure Submit(Wireframe:Boolean); Virtual;Abstract;

    Public
      Function Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean; Virtual;Abstract;
      Procedure Release(); Override;

      //4, GL_UNSIGNED_BYTE, True
//        _Buffer.AddAttribute('terra_boneIndex', 1, GL_FLOAT, False, Not _Owner._Skinning);
  //      _Buffer.AddAttribute('terra_tangent', 4, GL_FLOAT, False, Not _Owner._NormalMapping);


      Procedure SetIndexList(IndexList:Pointer; TriangleCount:Integer);

      Procedure Draw(Wireframe:Boolean);

      Function Update(Data:PByte):Boolean; Virtual; 
  End;

      RendererFeature =  Class(TERRAObject)
	      Protected
		      _Avaliable:Boolean;

	      Public
		      Property Avaliable:Boolean Read _Avaliable Write _Avaliable;
      End;


	RendererSetting = Class(TERRAObject)
		Protected
                         _Owner:GraphicsRenderer;
			_Enabled:Boolean;
                        _Available:Boolean;

		Public
      Constructor Create(Owner:GraphicsRenderer; Available:Boolean);
      Procedure SetValue(NewValue:Boolean);

      Property Enabled:Boolean Read _Enabled Write SetValue;
	End;

	RendererVariableSetting = Class(TERRAObject)
		Protected
                         _Owner:GraphicsRenderer;
			_Quality:RendererQuality;

		Public
      Procedure SetValue(NewValue:RendererQuality);

      Property Value:RendererQuality Read _Quality Write SetValue;
	End;

	RendererFeatures = Class(TERRAObject)
    Protected
      _Owner:GraphicsRenderer;
      _MaxAnisotrophy:Integer;
			_MaxTextureSize:Integer;
      _MaxTextureUnits:Integer;
      _multiSampleCount:Integer;
      _maxRenderTargets:Integer;
      _MaxUniformVectors:Integer;

      _VertexCacheSize:Integer;

    Public
		  FrameBufferObject:RendererFeature;
  		CubemapTexture:RendererFeature;
      FloatTexture:RendererFeature;
  		TextureArray:RendererFeature;
      SeparateBlends:RendererFeature;
      SeamlessCubeMap:RendererFeature;
      StencilBuffer:RendererFeature;
      PackedStencil:RendererFeature;
      NPOT:RendererFeature;
      Shaders:RendererFeature;
  		TextureCompression:RendererFeature;
	  	VertexBufferObject:RendererFeature;
      PostProcessing:RendererFeature;
      DeferredLighting:RendererFeature;
      Outlines:RendererFeature;

      Constructor Create(Owner:GraphicsRenderer);
      Procedure Release(); Override;

      Procedure WriteToLog();

      Property MaxAnisotrophy:Integer Read _MaxAnisotrophy;
			Property MaxTextureSize:Integer Read _MaxTextureSize;
      Property MaxTextureUnits:Integer Read _MaxTextureUnits;
      Property MaxRenderTargets:Integer Read _MaxRenderTargets;
      Property MultiSampleCount:Integer Read _multiSampleCount;
      Property VertexCacheSize:Integer Read _VertexCacheSize;
	End;

	RendererSettings = Class(TERRAObject)
    Protected
      _Owner:GraphicsRenderer;
      _Changed:Boolean;

      _ShadowSplitCount:Integer;
      _ShadowSplitWeight:Single;
      _ShadowMapSize:Integer;
      _ShadowBias:Single;

    Public

      AlphaFade:RendererSetting;
      DynamicShadows:RendererSetting;
      Textures:RendererVariableSetting;
      TextureCompression:RendererSetting;
      DeferredLighting:RendererSetting;
      DeferredFog:RendererSetting;
      DeferredShadows:RendererSetting;
      SelfShadows:RendererSetting;
      DepthOfField:RendererSetting;
      PostProcessing:RendererSetting;
      NormalMapping:RendererSetting;
      ToonShading:RendererSetting;
      AlphaTesting:RendererSetting;
      Specular:RendererSetting;
      Fur:RendererSetting;
      Sky:RendererSetting;
      Reflections:RendererSetting;
      DynamicLights:RendererSetting;
      SSAO:RendererSetting;
      VertexBufferObject:RendererSetting;
      Outlines:RendererSetting;

      CartoonHues:RendererSetting;
      CartoonHueGreen:ColorRGBA;
      CartoonHueYellow:ColorRGBA;
      CartoonHuePurple:ColorRGBA;
      CartoonHueBlack:ColorRGBA;

      FogMode:Integer;
      FogColor:ColorRGBA;
      FogDistanceStart:Single;
      FogDistanceEnd:Single;
      FogHeightStart:Single;
      FogHeightEnd:Single;
      FogBoxArea:BoundingBox;
      FogBoxSize:Single;

      SurfaceProjection:SurfaceProjectionMode;


      Constructor Create(Owner:GraphicsRenderer);
      Procedure Release; Override;

      Property ShadowSplitCount:Integer Read _ShadowSplitCount Write _ShadowSplitCount;
      Property ShadowSplitWeight:Single Read _ShadowSplitWeight Write _ShadowSplitWeight;
      Property ShadowMapSize:Integer Read _ShadowMapSize Write _ShadowMapSize;
      Property ShadowBias:Single Read _ShadowBias Write _ShadowBias;

      Property Changed:Boolean Read _Changed;
	End;

  RendererStats = Class(TERRAObject)
    Protected
      Procedure Reset;

    Public
      TriangleCount:Integer;
      ShaderSwitches:Integer;
      DrawCalls:Integer;
      LightCount:Integer;
      OccluderCount:Integer;
      RenderableCount:Integer;
      FramesPerSecond:Integer;
  End;

  GraphicsRenderer = Class(TERRAObject)
    Protected
      _Name:TERRAString;

      _CurrentContext:Integer;

      _Features:RendererFeatures;
      _Settings:RendererSettings;

      _DeviceName:TERRAString;
      _DeviceVendor:TERRAString;
      _DeviceVersion:TERRAVersion;

      _ProjectionMatrix:Matrix4x4;
      _ModelMatrix:Matrix4x4;
      _TextureMatrix:Matrix4x4;

      //_BackgroundColor:Color;
      _DiffuseColor:ColorRGBA;

      _Stats:RendererStats;
      _PrevStats:RendererStats;

      _Frames:Integer;

      _ActiveShader:ShaderInterface;

      _CurrentSource:VertexData;

      _VSync:Boolean;

      Function Initialize():Boolean; Virtual; Abstract;
      Procedure InitSettings();

      Function CreateContext():Boolean; Virtual;
      Procedure DestroyContext(); Virtual;

      Procedure SetVSync(Const Value:Boolean);
      Procedure ChangeVSync(Const Value:Boolean); Virtual;
      
    Public
      Constructor Create();
      Procedure Release(); Override;

      Function CreateTexture():TextureInterface; Virtual; Abstract;
      Function CreateCubeMap():CubeMapInterface; Virtual; Abstract;
      Function CreateVertexBuffer():VertexBufferInterface; Virtual; Abstract;
      Function CreateShader():ShaderInterface; Virtual; Abstract;
      Function CreateRenderTarget():RenderTargetInterface; Virtual; Abstract;

      //Procedure Invalidate(); Virtual; Abstract;
      Procedure Reset();
      Procedure ResetState(); Virtual;

      Procedure OnContextLost();

      Procedure Resize(Width, Height: Integer); Virtual;

      Function BindShader(Shader:ShaderInterface):Boolean;
      Function BindSurface(Surface:SurfaceInterface; Slot:Integer):Boolean;

      Procedure InternalStat(Stat:StatType; Count:Integer = 1);

      Procedure SetClearColor(Const ClearColor:ColorRGBA); Virtual; Abstract;
      Procedure ClearBuffer(Color, Depth, Stencil:Boolean); Virtual; Abstract;

      Procedure SetStencilTest(Enable:Boolean); Virtual; Abstract;
      Procedure SetStencilFunction(Mode:CompareMode; StencilID:Cardinal; Mask:Cardinal = $FFFFFFFF); Virtual; Abstract;
      Procedure SetStencilOp(fail, zfail, zpass:StencilOperation); Virtual; Abstract;

      Procedure SetColorMask(Red, Green, Blue, Alpha:Boolean); Virtual; Abstract;
      Procedure SetDepthMask(WriteZ:Boolean); Virtual; Abstract;
      Procedure SetDepthTest(Enable:Boolean); Virtual; Abstract;
      Procedure SetDepthFunction(Mode:CompareMode); Virtual; Abstract;


      Procedure SetCullMode(Mode:CullMode); Virtual; Abstract;

      Procedure SetBlendMode(BlendMode:Integer); Virtual; Abstract;

      Procedure OnSettingsChange();

      Function GetScreenshot():Image; Virtual;

      Procedure SetProjectionMatrix(Const Mat:Matrix4x4); Virtual; Abstract;
      Procedure SetModelMatrix(Const Mat:Matrix4x4); Virtual; Abstract;
      Procedure SetTextureMatrix(Const Mat:Matrix4x4); Virtual; Abstract;

      Procedure SetViewport(X,Y, Width, Height:Integer); Virtual; Abstract;

      Procedure SetScissorState(Enabled:Boolean); Virtual; Abstract;
      Procedure SetScissorArea(X,Y, Width, Height:Integer); Virtual; Abstract;

      Procedure SetDiffuseColor(Const C:ColorRGBA); Virtual; Abstract;
      //Procedure SetBackgroundColor(Const C:Color);

      Procedure SetVertexSource(Data:VertexData);
      Procedure SetAttributeSource(Const Name:TERRAString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer); Virtual; Abstract;
      Procedure DrawSource(Primitive:RenderPrimitive; Count:Integer); Virtual; Abstract;
      Procedure DrawIndexedSource(Primitive:RenderPrimitive; Count:Integer; Indices:PWord); Virtual; Abstract;

      Procedure BeginFrame(); Virtual;
      Procedure EndFrame(); Virtual;

      Procedure UpdateFrameCounter();

      Property CurrentContext:Integer Read _CurrentContext;

      Property DiffuseColor:ColorRGBA Read _DiffuseColor Write SetDiffuseColor;
      //Property BackgroundColor:Color Read _BackgroundColor Write SetBackgroundColor;

      Property Name:TERRAString Read _Name;

      Property Features:RendererFeatures Read _Features;
      Property Settings:RendererSettings Read _Settings;
      Property Stats:RendererStats Read _PrevStats;


      Property ActiveShader:ShaderInterface Read _ActiveShader;

      Property VSync:Boolean Read _VSync Write SetVSync;

      Property DeviceName:TERRAString Read _DeviceName;
      Property DeviceVendor:TERRAString Read _DeviceVendor;
      Property DeviceVersion:TERRAVersion Read _DeviceVersion;
  End;

  Function Renderers():List;

Implementation
Uses TERRA_Error, TERRA_EngineManager, TERRA_FileManager, TERRA_Lights, TERRA_Math, TERRA_Log,
  TERRA_Texture, TERRA_NullRenderer;

Var
  _RendererList:List = Nil;

Function Renderers():List;
Begin
  If _RendererList = Nil Then
  Begin
    _RendererList := List.Create();
    _RendererList.Add(NullRenderer.Create());
  End;

  Result := _RendererList;
End;

{ RendererSettings }
Constructor RendererSettings.Create(Owner: GraphicsRenderer);
Begin
  Self._Owner := Owner;

  _Changed := True;

{  _ShadowSplitCount := 2;
  _ShadowSplitWeight := 05;
  _ShadowMapSize:Integer;
  _ShadowBias:Single;}

  AlphaFade := RendererSetting.Create(_Owner, True);
  DynamicShadows := RendererSetting.Create(_Owner, True);
  TextureCompression := RendererSetting.Create(_Owner, _Owner.Features.TextureCompression.Avaliable);
  DeferredLighting := RendererSetting.Create(_Owner, _Owner.Features.DeferredLighting.Avaliable);
  DeferredFog := RendererSetting.Create(_Owner, _Owner.Features.DeferredLighting.Avaliable);
  DeferredShadows := RendererSetting.Create(_Owner, _Owner.Features.DeferredLighting.Avaliable);
  SelfShadows := RendererSetting.Create(_Owner, True);
  DepthOfField := RendererSetting.Create(_Owner, True);
  PostProcessing := RendererSetting.Create(_Owner, _Owner.Features.PostProcessing.Avaliable);
  NormalMapping := RendererSetting.Create(_Owner, True);
  ToonShading := RendererSetting.Create(_Owner, True);
  AlphaTesting := RendererSetting.Create(_Owner, True);
  Specular := RendererSetting.Create(_Owner, True);
  Fur := RendererSetting.Create(_Owner, True);
  Sky := RendererSetting.Create(_Owner, True);
  Reflections := RendererSetting.Create(_Owner, True);
  DynamicLights := RendererSetting.Create(_Owner, True);
  SSAO := RendererSetting.Create(_Owner, True);
  VertexBufferObject := RendererSetting.Create(_Owner, _Owner.Features.VertexBufferObject.Avaliable);
  Outlines := RendererSetting.Create(_Owner, True);
  CartoonHues := RendererSetting.Create(_Owner, True);

  {FogMode:Integer;
      FogColor:Color;
      FogDensity:Single;
      FogStart:Single;
      FogHeight:Single;}
End;

Procedure RendererSettings.Release();
Begin
  ReleaseObject(AlphaFade);
  ReleaseObject(DynamicShadows);
  ReleaseObject(TextureCompression);
  ReleaseObject(DeferredLighting);
  ReleaseObject(DeferredFog);
  ReleaseObject(DeferredShadows);
  ReleaseObject(SelfShadows);
  ReleaseObject(DepthOfField);
  ReleaseObject(PostProcessing);
  ReleaseObject(NormalMapping);
  ReleaseObject(ToonShading);
  ReleaseObject(AlphaTesting);
  ReleaseObject(Specular);
  ReleaseObject(Fur);
  ReleaseObject(Sky);
  ReleaseObject(Reflections);
  ReleaseObject(DynamicLights);
  ReleaseObject(SSAO);
  ReleaseObject(VertexBufferObject);
  ReleaseObject(Outlines);
  ReleaseObject(CartoonHues);
End;

{ RendererSetting }
Constructor RendererSetting.Create(Owner:GraphicsRenderer; Available:Boolean);
Begin
  _Owner := Owner;
  _Available := Available;
End;

Procedure RendererSetting.SetValue(NewValue: Boolean);
Begin
  If (NewValue) And (Not Self._Available) Then
    NewValue := False;

  If (_Enabled = NewValue) Then
    Exit;

	_Enabled := NewValue;
  _Owner._Settings._Changed := True;
End;

Procedure RendererVariableSetting.SetValue(NewValue:RendererQuality);
Begin
  If (_Quality=Value) Then
    Exit;

	_Quality := Value;
  _Owner._Settings._Changed := True;
End;

{ RendererFeatures }
Constructor RendererFeatures.Create(Owner:GraphicsRenderer);
Begin
  _Owner := Owner;


  FrameBufferObject := RendererFeature.Create();
  CubemapTexture := RendererFeature.Create();
  FloatTexture := RendererFeature.Create();
  TextureArray := RendererFeature.Create();
  SeparateBlends := RendererFeature.Create();
  SeamlessCubeMap := RendererFeature.Create();
  StencilBuffer := RendererFeature.Create();
  PackedStencil := RendererFeature.Create();
  NPOT := RendererFeature.Create();
  Shaders := RendererFeature.Create();
  TextureCompression := RendererFeature.Create();
  VertexBufferObject := RendererFeature.Create();
  PostProcessing := RendererFeature.Create();
  DeferredLighting := RendererFeature.Create();
  Outlines := RendererFeature.Create();
End;

Procedure RendererFeatures.Release();
Begin
  ReleaseObject(FrameBufferObject);
  ReleaseObject(CubemapTexture);
  ReleaseObject(FloatTexture);
  ReleaseObject(TextureArray);
  ReleaseObject(SeparateBlends);
  ReleaseObject(SeamlessCubeMap);
  ReleaseObject(StencilBuffer);
  ReleaseObject(PackedStencil);
  ReleaseObject(NPOT);
  ReleaseObject(Shaders);
  ReleaseObject(TextureCompression);
  ReleaseObject(VertexBufferObject);
  ReleaseObject(PostProcessing);
  ReleaseObject(DeferredLighting);
  ReleaseObject(Outlines);
End;

Procedure RendererFeatures.WriteToLog();
Begin
  Log(logDebug, 'Renderer', 'Device: '+ _Owner._DeviceName);
  Log(logDebug, 'Renderer', 'Vendor: '+ _Owner._DeviceVendor);
  Log(logDebug, 'Renderer', 'Shaders: '+  BoolToString(Shaders.Avaliable));
  Log(logDebug, 'Renderer', 'Max texture slots:' +  IntegerProperty.Stringify(_MaxTextureUnits));
  Log(logDebug, 'Renderer', 'Max texture size:' +  IntegerProperty.Stringify(_MaxTextureSize));
  Log(logDebug, 'Renderer', 'Max render targets:' +  IntegerProperty.Stringify(_maxRenderTargets));
  Log(logDebug, 'Renderer', 'Texture compression: '+  BoolToString(TextureCompression.Avaliable));
  Log(logDebug, 'Renderer', 'VertexBufferObject: '+  BoolToString(VertexBufferObject.Avaliable));
  Log(logDebug, 'Renderer', 'FrameBufferObject: '+  BoolToString(FrameBufferObject.Avaliable));
  Log(logDebug, 'Renderer', 'CubemapTexture: '+  BoolToString(CubemapTexture.Avaliable));
  Log(logDebug, 'Renderer', 'FloatTexture: '+  BoolToString(FloatTexture.Avaliable));
  Log(logDebug, 'Renderer', 'TextureArray: '+  BoolToString(TextureArray.Avaliable));
  Log(logDebug, 'Renderer', 'SeparateBlends: '+  BoolToString(SeparateBlends.Avaliable));
  Log(logDebug, 'Renderer', 'SeamlessCubeMap: '+  BoolToString(SeamlessCubeMap.Avaliable));
  Log(logDebug, 'Renderer', 'NPOT: '+  BoolToString(NPOT.Avaliable));
End;

{ GraphicInterface }
Constructor GraphicInterface.Create(Owner:GraphicsRenderer);
Begin
  Self._Owner := Owner;
  Self._Context := Owner.CurrentContext;

  Self.Initialize();
End;

Procedure GraphicInterface.Release();
Begin
  // do nothing
End;

Function GraphicInterface.IsValid: Boolean;
Begin
  Result := (IsComplete) And (_Owner<>Nil) And (_Owner.CurrentContext = Self._Context);
End;

Procedure GraphicInterface.Initialize;
Begin
  // do nothing
End;

Function GraphicInterface.IsComplete: Boolean;
Begin
  Result := True;
End;

{ Renderer }
Constructor GraphicsRenderer.Create;
Begin
  _Settings := Nil;
  _Features := Nil;
  _Stats := Nil;
  _PrevStats := Nil;

  {$IFDEF LINUX}
 // Self.Reset();
  {$ENDIF}
End;

Procedure GraphicsRenderer.Release();
Begin
  Inherited;

  ReleaseObject(_Settings);
  ReleaseObject(_Features);
  ReleaseObject(_PrevStats);
  ReleaseObject(_Stats);
End;

Procedure GraphicsRenderer.OnSettingsChange;
Begin
  If (Settings.VertexBufferObject.Enabled) And (Not Features.Shaders.Avaliable) Then
    Settings.VertexBufferObject.SetValue(False);
End;

Procedure GraphicsRenderer.InternalStat(Stat: StatType; Count: Integer);
Begin
  Case Stat Of
  statTriangles:
    Begin
      Inc(_Stats.TriangleCount, Count);
      Inc(_Stats.DrawCalls);
    End;

  statShaders:
    Begin
      Inc(_Stats.ShaderSwitches, Count);
    End;

  statRenderables:
    Begin
      Inc(_Stats.RenderableCount, Count);
    End;

  statLights:
    Begin
      Inc(_Stats.LightCount, Count);
    End;

  statOccluders:
    Begin
      Inc(_Stats.OccluderCount, Count);
    End;

  End;
End;

Procedure GraphicsRenderer.BeginFrame;
Var
  Temp:RendererStats;
Begin
  _Stats.LightCount := LightManager.Instance.LightCount;

  Temp := _Stats;
  _Stats := _PrevStats;
  _PrevStats := Temp;

  _Stats.Reset();
  _Stats.FramesPerSecond := _PrevStats.FramesPerSecond;

  //Application.Instance.SetTitle( IntegerProperty.Stringify(N));
End;

{ VertexBufferInterface }
(*Procedure VertexBufferInterface.AddAttribute(Const Name:TERRAString; Format:DataFormat; Skip:Boolean);
Var
  Count, Size:Integer;
  Normalized:Boolean;
Begin
  Normalized := False;
  Case Format Of
  typeFloat:
    Begin
      Size := 4;
      Count := 1;
    End;

  typeByte:
    Begin
      Size := 1;
      Count := 1;
    End;

  typeVector2D:
    Begin
      Size := 2 * 4;
      Count := 2;
    End;

  typeVector3D:
    Begin
      Size := 3 * 4;
      Count := 3;
    End;

  typeVector4D:
    Begin
      Size := 4 * 4;
      Count := 4;
    End;

  typeColor:
    Begin
      Size := 1;
      Count := 4;
      Normalized := True;
    End;

  {$IFNDEF ANDROID}
//  typeInteger:  Size := Count * 4;
  {$ENDIF}
  Else
    Log(logWarning, 'VBO', 'Invalid VBO attribute type ['+ IntegerProperty.Stringify(Integer(Format))+']');
  End;

  If (Not Skip) Then
  Begin
    Inc(_AttributeCount);
    SetLength(_Attributes, _AttributeCount);
    _Attributes[Pred(_AttributeCount)].Name := Name;
    _Attributes[Pred(_AttributeCount)].Size := Size;
    _Attributes[Pred(_AttributeCount)].Count := Count;
    _Attributes[Pred(_AttributeCount)].Format := Format;
    _Attributes[Pred(_AttributeCount)].Normalized := Normalized;
    _Attributes[Pred(_AttributeCount)].Handle := -1;
    _Attributes[Pred(_AttributeCount)].Offset := _CurrentOffset;
  End;

  Inc(_CurrentOffset, Size);
End;*)

Procedure VertexBufferInterface.Release();
Begin
  If Assigned(_WireframeIndices) Then
  Begin
    FreeMem(_WireframeIndices);
    _WireframeIndices := Nil;
  End;
End;

Procedure VertexBufferInterface.Draw(Wireframe: Boolean);
Var
  I, J, K, Ofs:Integer;
  Shader:ShaderInterface;
Begin
  If (_TriangleCount<=0) Then
    Exit;

  If (Wireframe) And (_WireframeIndices = Nil) Then
  Begin
    If (Assigned(_EdgeList)) Then
    Begin
      _EdgeCount := 0;
      For I:=0 To Pred(_TriangleCount) Do
        For J:=0 To 2 Do
        If (_EdgeList[I].Visible[J]) Then
          Inc(_EdgeCount);
    End Else
      _EdgeCount := 3 * _TriangleCount;

    GetMem(_WireframeIndices, SizeOf(Word) * 2 * _EdgeCount);
    Ofs := 0;
    For I:=0 To Pred(_TriangleCount) Do
      For J:=0 To 2 Do
      If (_EdgeList = Nil) Or (_EdgeList[I].Visible[J]) Then
      Begin
        K := (J+1) Mod 3;
        _WireframeIndices[Ofs] := _IndexList[I].Indices[J]; Inc(Ofs);
        _WireframeIndices[Ofs] := _IndexList[I].Indices[K]; Inc(Ofs);
      End;
  End;

  If _Owner.ActiveShader<>Nil Then
    Shader := _Owner.ActiveShader
  Else
    Shader := Nil;

  Self.Submit(Wireframe);
End;

Procedure VertexBufferInterface.SetIndexList(IndexList:Pointer; TriangleCount: Integer);
Begin
  _IndexList := IndexList;
  _TriangleCount := TriangleCount;
End;

Procedure GraphicsRenderer.EndFrame();
Begin
  Inc(_Frames);
End;

Procedure GraphicsRenderer.UpdateFrameCounter();
Begin
  _Stats.FramesPerSecond := _Frames;
  _Frames := 1;
End;

Procedure GraphicsRenderer.InitSettings();
Begin
  _Stats := RendererStats.Create();
  _PrevStats := RendererStats.Create();

  _Settings := RendererSettings.Create(Self);

  Settings.TextureCompression.SetValue(Features.TextureCompression.Avaliable);
  Settings.VertexBufferObject.SetValue(Features.Shaders.Avaliable);
  Settings.PostProcessing.SetValue(Features.PostProcessing.Avaliable);
  Settings.DeferredFog.SetValue(False);
  Settings.DeferredShadows.SetValue(False);

  Settings.DeferredLighting.SetValue(False);
  Settings.DeferredFog.SetValue(False);
  Settings.DeferredShadows.SetValue(False);
  Settings.FogMode := 0;

  Settings.DynamicShadows.SetValue(False);
  //Settings.DynamicShadows.Quality := qualityMedium;

  //Settings.Textures.Quality := qualityMedium;

  Settings.DepthOfField.SetValue(False);
  Settings.NormalMapping.SetValue(False);
  Settings.DynamicLights.SetValue(True);
  Settings.ToonShading.SetValue(False);
  {$IFDEF MOBILE}
  Settings.AlphaTesting.SetValue(False);
  {$ELSE}
  Settings.AlphaTesting.SetValue(True);
  {$ENDIF}

  Settings.Specular.SetValue(False);
  Settings.Fur.SetValue(True);
  Settings.Reflections.SetValue(True);
  Settings.Sky.SetValue(True);
  Settings.SelfShadows.SetValue(False);
  Settings.SSAO.SetValue(False);

  Settings.ShadowSplitCount := 3;
  Settings.ShadowSplitWeight := 0.75;
  Settings.ShadowMapSize := 1024;
  Settings.ShadowBias := 2.0;

  {$IFDEF DISABLEOUTLINES}
  Features.Outlines._Avaliable := False;
  {$ELSE}
  Features.Outlines._Avaliable := True;
  {$ENDIF}
  Settings.Outlines._Enabled := Features.Outlines._Avaliable;

  //Settings.FloatTexture.Avaliable := False;
//  Settings.PostProcessing.Enabled := False;

  Settings.FogColor := ColorCreate(255, 0,0, 255);
  Settings.FogDistanceStart := 0.2;
  Settings.FogDistanceEnd := 1.0;

  Settings.CartoonHueGreen := ColorCreate(64, 255, 64);
  Settings.CartoonHueYellow := ColorCreate(255, 255, 64);
  Settings.CartoonHuePurple := ColorCreate(255, 64, 255);
  Settings.CartoonHueBlack := ColorNull;

  Settings._Changed := True;
End;

Procedure GraphicsRenderer.Reset;
Begin
  If Settings = Nil Then
  Begin
    If (Not CreateContext()) Then
    Begin
      RaiseError('Cannot create renderer context!');
      Exit;
    End;

    If (Self.Initialize()) And (Assigned(_Features)) Then
      _Features.WriteToLog()
    Else
    Begin
      RaiseError('Renderer failed to initialize!');
      Exit;
    End;

    InitSettings();
  End;

  Self.ResetState();
End;

Procedure GraphicsRenderer.ResetState;
Begin
  // do nothing
End;

Function GraphicsRenderer.CreateContext: Boolean;
Begin
  Result := True;
End;

Procedure GraphicsRenderer.DestroyContext;
Begin
  // do nothing
End;

Procedure GraphicsRenderer.OnContextLost;
Var
  N:Integer;
Begin
  N := Self._CurrentContext;

  Self.DestroyContext();
  Self.CreateContext();

  Self.Reset();

  Self._CurrentContext := N + 1;
End;

Function GraphicsRenderer.BindShader(Shader:ShaderInterface):Boolean;
Begin
  If Shader = Nil Then
  Begin
    RaiseError('Cannot bind null shader!');
    Result := False;
    Exit;
  End;

  If (_ActiveShader <> Shader) And (Assigned(_ActiveShader)) Then
  Begin
    _ActiveShader.Unbind();
  End;

  _ActiveShader := Shader;
  Result := _ActiveShader.Bind();
End;

Function GraphicsRenderer.BindSurface(Surface:SurfaceInterface; Slot:Integer):Boolean;
Begin
  If (Surface = Nil) Then
  Begin
    Engine.Textures.NullTexture.Bind(Slot);
    //RaiseError('Cannot bind null surface!');
    Result := False;
    Exit;
  End;

  Result := Surface.Bind(Slot);
End;

Procedure GraphicsRenderer.SetVertexSource(Data: VertexData);
Var
  I:Integer;
Begin
  Self._CurrentSource := Data;

  If Data = Nil Then
    Exit;

  {$IFDEF DEBUG_SHADERS}
  If Assigned(_ActiveShader) Then
  Begin
    For I:=0 To Pred(_ActiveShader._AttributeCount) Do
    If (Not Data.HasAttributeWithName(_ActiveShader._Attributes[I].Name)) Then
    Begin
      RaiseError('Vertex buffer is missing attribute '+_ActiveShader._Attributes[I].Name);
    End;
  End;
  {$ENDIF}
End;

Procedure GraphicsRenderer.Resize(Width, Height: Integer);
Begin
  // do nothing
End;

Function GraphicsRenderer.GetScreenshot: Image;
Begin
  Result := Nil;
End;

Procedure GraphicsRenderer.SetVSync(const Value: Boolean);
Begin
  If _VSync = Value Then
    Exit;

  _VSync := Value;
  Self.ChangeVSync(Value);
End;

Procedure GraphicsRenderer.ChangeVSync(const Value: Boolean);
Begin
  // do nothing
End;

{ ShaderInterface }
Procedure ShaderInterface.AddAttributes(Source: TERRAString);
Var
  I:Integer;
  S, S2:TERRAString;
Begin
  _AttributeCount := 0;
  S := Source;
  StringReplaceText('gl_Position', 'IGNORE',S);
  If (Pos('gl_', S)>0) Then
  Begin
     Log(logWarning, 'Shader', 'The following shader has deprecated attributes: '{+ Self.Name});
  End;

  Repeat
    I := Pos('attribute', Source);
    If (I<=0) Then
      Break;

    Source := Copy(Source, I + 10, MaxInt);
    S := StringGetNextSplit(Source, Ord(' '));      // type
    S := StringUpper(S);
    If (S='HIGHP') Or (S='LOWP') Or (S='MEDIUMP') Then
      S := StringGetNextSplit(Source, Ord(' '));      // type
    S2 := StringGetNextSplit(Source, Ord(';'));      // name

    Inc(_AttributeCount);
    SetLength(_Attributes, _AttributeCount);
    _Attributes[Pred(_AttributeCount)].Name := S2;
    _Attributes[Pred(_AttributeCount)].Handle := -1;
  Until (Source='');

  If (_AttributeCount<=0) Then
  Begin
    Log(logWarning, 'Shader', 'The following shader has no attributes: '{+ Self.Name});
  End;
End;

Procedure ShaderInterface.SetColorUniform(Const Name:TERRAString; Const Value:ColorRGBA);
Begin
  Self.SetVec4Uniform(Name, VectorCreate4D(Value.R / 255.0, Value.G / 255.0, Value.B / 255.0, Value.A / 255.0));
End;

Procedure ShaderInterface.SetPlaneUniform(const Name: AnsiString; Const Value: Plane);
Begin
  Self.SetVec4Uniform(Name, VectorCreate4D(Value.A, Value.B, Value.C, Value.D));
End;

Function ShaderInterface.HasUniform(const Name: TERRAString): Boolean;
Begin
  Result := GetUniform(Name)>=0;
End;

{ RenderTargetInterface }
Procedure RenderTargetInterface.SetBackgroundColor(const Value:ColorRGBA);
Begin
  Self._BackgroundColor := Value;
End;

Function SurfaceInterface.GetImage: Image;
Begin
  Result := Nil;
End;

Function SurfaceInterface.GetOrigin: SurfaceOrigin;
Begin
  Result := surfaceTopLeft;
End;

Function SurfaceInterface.GetPixel(X, Y: Integer):ColorRGBA;
Begin
  Result := ColorBlack;
End;

Procedure SurfaceInterface.SetFilter(Value: TextureFilterMode);
Begin
  Self._Filter := Filter;
End;

Procedure SurfaceInterface.SetMipMapping(Value: Boolean);
Begin
  Self._MipMapped := Value;
End;

Procedure SurfaceInterface.SetWrapMode(Value: TextureWrapMode);
Begin
  Self._WrapMode := Value;
End;

Var
  I:Integer;

{ CubeMapInterface }
Procedure CubeMapInterface.Initialize;
Begin
  Inherited;

  _WrapMode := wrapNothing;
End;

Function CubeMapInterface.LoadFromFile(Const FileName: TERRAString): Boolean;
Var
  W,H, N:Integer;
  Img:Image;
  S, Ext:TERRAString;
  Waiting:Boolean;
  Info:ImageClassInfo;
Begin
  W := 0;
  H := 0;
  Waiting := True;
  For I:=0 To 5 Do
  Begin
    N := 0;
    S := '';
    While (S='') And (N<GetImageExtensionCount()) Do
    Begin
      Info := GetImageExtension(N);
      S := FileManager.Instance.SearchResourceFile(FileName + '_' + CubeFaceNames[I] + '.' + Info.Name);
      Inc(N);
    End;

    If (S='') Then
    Begin
      Log(logWarning, 'Cubemap', 'Could not load cubemap face '+CubeFaceNames[I]+' for '+FileName);
      Continue;
    End;

    Img := Image.Create(S);
    If (Img.Width<=0) Then
    Begin
      ReleaseObject(Img);
      Img := Image.Create(W, H);
    End Else
    Begin
      W := Img.Width;
      H := Img.Height;
    End;

    If W<=0 Then
      Continue;

    If Waiting Then
    Begin
      Self.Generate(W, H, textureFormat_RGBA, textureFormat_RGBA, pixelSizeByte);
      Waiting := False;
    End;

    Self.UpdateFace(I, Img.RawPixels, 0, 0, Img.Width, Img.Height);
    ReleaseObject(Img);
  End;

  Result := True;
End;

Function CubeMapInterface.UpdateFace(FaceID: Integer; Pixels: Pointer; X, Y, Width, Height: Integer): Boolean;
Begin
  Result := False;
End;

{ RendererStats }
Procedure RendererStats.Reset;
Begin
  TriangleCount := 0;
  ShaderSwitches := 0;
  DrawCalls := 0;
  LightCount := 0;
  OccluderCount := 0;
  RenderableCount := 0;
  FramesPerSecond := 0;
End;

{ TextureInterface }
Function TextureInterface.Update(Pixels: Pointer; X, Y, Width,  Height: Integer):Boolean;
Begin
  Result := False;
End;
Function VertexBufferInterface.Update(Data: PByte): Boolean;
Begin
  Result := False;
End;


Initialization
Finalization
  ReleaseObject(_RendererList);
End.
