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
 * TERRA_GraphicsManager
 * Implements the global graphics manager
 ***********************************************************************************************************************
}

Unit TERRA_GraphicsManager;
{$I terra.inc}

{$IFDEF WINDOWS}
{$DEFINE PRECISIONTIMER}
{$ENDIF}

{-$DEFINE TESTFULLSCREENSHADER}

{$IFDEF POSTPROCESSING}
{$IFDEF FRAMEBUFFEROBJECTS}
  {$DEFINE HAS_REFLECTIONS}
{$ENDIF}
{$ENDIF}


Interface
Uses {$IFNDEF DEBUG_LEAKS}TERRA_MemoryManager,{$ENDIF} {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Downsampler, TERRA_Shader, //TERRA_Renderer,
  {$IFDEF POSTPROCESSING}TERRA_ScreenFX,{$ENDIF}
  {$IFDEF SHADOWMAPS}TERRA_ShadowMaps,{$ENDIF}
  {$IFDEF PRECISIONTIMER}TERRA_Timer,{$ENDIF}
  {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_BoundingBox, TERRA_Camera, TERRA_Color, TERRA_Matrix4x4,
  TERRA_Utils, TERRA_Texture, TERRA_Scene, TERRA_Vector3D,
  TERRA_RenderTarget, TERRA_Viewport, TERRA_Application,
  TERRA_Image, TERRA_Math, TERRA_Vector2D, TERRA_Ray, TERRA_Collections;

Type
  GraphicsManagerCallback = Procedure;

	GraphicsManagerFeature = Object
		Protected
			_Avaliable:Boolean;

		Public
			Property Avaliable:Boolean Read _Avaliable;
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

	GraphicsManagerVariableSetting = Object(GraphicsManagerSetting)
		Protected
			_Quality:Integer;

      Procedure SetQuality(Value:Integer);

		Public
      Property Quality:Integer Read _Quality Write SetQuality;
	End;

	GraphicsManagerSettings = Class(TERRAObject)
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
      Outlines:GraphicsManagerSetting;

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

Const
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

  lightModelDefault   = 0;
  lightModelSimple    = 1;

  //FogMode
  fogOff      = 0;
  fogDistance = 1;
  fogHeight   = 2;

  renderStageDiffuse      = 1;
  renderStageNormal       = 2;
  renderStageGlow         = 4;
  renderStageRefraction   = 8;
  renderStageOutline      = 16;
  renderStageReflection   = 32;
  renderStageShadow       = 64;
  renderStagePostProcess  = 128;

  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;
  renderFlagsSkipReflections  = 4;

  MaxTextureHandles = 2048;
  MaxFrameBufferHandles = 128;

Type
  Renderable = Class(TERRAObject)
    Protected
      _Distance:Single;
      _LOD:Single;
//      _IsReflection:Boolean;
      _WasVisible:Boolean;
      _Flags:Integer;
      _LastUpdate:Cardinal;

    Public
{      ReflectionPoint:Vector3D;
      ReflectionNormal:Vector3D;}

      Procedure Release; Override;

      Procedure Update; Virtual;

      Function GetName():TERRAString; Virtual;

      Function GetBoundingBox:BoundingBox; Virtual; Abstract;
      Procedure Render(TranslucentPass:Boolean); Virtual; Abstract;

      Procedure RenderLights(); Virtual;

      Function IsOpaque():Boolean; Virtual;
      Function IsTranslucent():Boolean; Virtual;

      Property LOD:Single Read _LOD;
      Property WasVisible:Boolean Read _WasVisible;
  End;

  RenderableProxy =  Class(ListObject)
    Protected
      _Object:Renderable;
    Public

      Constructor Create(Obj:Renderable);

      Function Sort(Other:ListObject):Integer; Override;
      Procedure CopyValue(Other:ListObject); Override;
  End;

Const
  MaxRenderables  = 4096;
  MaxLODLevel = 3;

  FogDensityScale = 0.06;

  QualityDisabled = 0;
  QualityLow = 1;
  QualityMedium = 2;
  QualityHigh = 3;

Type
//  SortMethod = (Unsorted, SortBackToFront, SortFrontToBack);

  Occluder = Class(Renderable)
    Protected
      _Next:Occluder;
      _P1,_P2,_P3,_P4:Vector3D;
      _StartVertex, _EndVertex:Vector3D;
      _BoundingBox:BoundingBox;

      Function OccluderOccluded(Occ:Occluder):Boolean;

    Public
      Function IsVisible:Boolean;

      Procedure Update; Override;
      
      Procedure SetTransform(Transform:Matrix4x4; Width,Height:Single);

      Function PointOccluded(P:Vector3D):Boolean;
      Function BoxOccluded(Box:BoundingBox; V:Viewport):Boolean;

      Procedure Render(TranslucentPass:Boolean); Override;
      Function GetBoundingBox:BoundingBox; Override;
  End;

  GraphicsManagerStats = Object
    Public
      TriangleCount:Integer;
      ShaderSwitches:Integer;
      DrawCalls:Integer;
      LightCount:Integer;
      OccluderCount:Integer;
      RenderableCount:Integer;
      FramesPerSecond:Integer;
  End;

	GraphicsManager = Class(ApplicationComponent)
		Protected
      _Viewports:Array Of Viewport;
      _ViewportCount:Integer;
      _CurrentViewport:Viewport;
      _MainViewport:Viewport;
      _DeviceViewport:Viewport;
      _UIViewport:Viewport;

      _Cameras:Array Of Camera;
      _CameraCount:Integer;

      _ReflectionCamera:Camera;

			_Settings:GraphicsManagerSettings;

      _Width:Integer;
      _Height:Integer;

      _DepthSize:Integer;

      _LightModel:Integer;

      _Scene:Scene;

      _Stats:GraphicsManagerStats;
      _PrevStats:GraphicsManagerStats;

      _Frames:Integer;
      _ElapsedTime:Single;

      {$IFNDEF PRECISIONTIMER}
      _LastTime:Cardinal;
      _LastSecondTime:Cardinal;
      {$ELSE}
      _accumTimeSec:Single;
      _Timer:Timer;
      {$ENDIF}

      _BackgroundColor:Color;

      _Occluders:Occluder;

      _BucketOpaque:Pool;
      _BucketAlpha:Pool;
      {$IFDEF REFLECTIONS_WITH_STENCIL}
      _BucketReflection:Pool;
      {$ENDIF}

      _Device:TERRAString;
      _Vendor:TERRAString;
      _Version:TERRAVersion;

      _FogEnable:Boolean;
      _CurrentBlendMode:Integer;

      _WindVector:Vector3D;
      _RenderStage:Integer;
      _FrameID:Cardinal;

      _Projection:Matrix4x4;
      _OrientationMatrix4x4:Matrix4x4;

      _NeedsContextRestore:Boolean;

      _StencilID:Byte;

      _UsedTextures:Array[0..Pred(MaxTextureHandles)] Of Boolean;
      _UsedFrameBuffers:Array[0..Pred(MaxFrameBufferHandles)] Of Boolean;
      _UsedRenderBuffers:Array[0..Pred(MaxFrameBufferHandles)] Of Boolean;

      _ReflectionsEnabled:Boolean;
      _ReflectionPoint:Vector3D;
      _ReflectionNormal:Vector3D;

      Procedure RenderUI;
      Procedure RenderStencilShadows(View:Viewport);
      Procedure RenderSceneInternal(View:Viewport; Pass:Integer);
      Procedure RenderViewport(View:Viewport);
      Procedure RenderList(RenderList:List; TranslucentPass:Boolean);

      Procedure ResetGLState();

      Procedure OnSettingsChange();

      Procedure OnAppResize; Override;
      Procedure OnContextLost; Override;
      Procedure OnOrientationChange; Override;
      Procedure OnViewportChange(X1, Y1, X2, Y2:Integer); Override;

      Procedure RestoreContext;

    Public
      ShowShadowVolumes:Boolean;
      ShowWireframe:Boolean;
      ShowDebugTarget:Integer;

      ReflectionMatrix:Matrix4x4;
      ReflectionMatrixSky:Matrix4x4;
      ReflectionActive:Boolean;

      {$IFDEF REFLECTIONS_WITH_STENCIL}
      ReflectionTemp:Boolean;
      ReflectionStencil:Boolean;
      {$ELSE}
      ReflectionMask:Texture;
      {$ENDIF}

      Render3D:Boolean;
      Render2D:Boolean;

      EnviromentMap:Texture;
      ColorRamp:Texture;

      Procedure Init; Override;
      Procedure Update; Override;

      Class Function Instance:GraphicsManager;

      Procedure Release; Override;

      Procedure RenderShadowmap(View:Viewport);
      Procedure RenderReflections(View:Viewport);

			Procedure RenderScene();

      Procedure TestDebugKeys();

      Procedure Internal(Offset, Count:Integer);

      //Procedure RenderCubeMapToFile(Position:LVector; FileName:TERRAString);

      Function IsBoxVisible(Box: BoundingBox): Boolean;

	    Procedure DrawFullscreenQuad(CustomShader:Shader; X1,Y1,X2,Y2:Single);

      Function SwapScene(MyScene:Scene):Scene;
      Procedure SetScene(MyScene:Scene);
      Procedure SetWind(WindDirection:Vector3D; WindIntensity:Single);

      Function GetPickRay(View:Viewport; TX,TY:Integer):Ray;

      Function ProjectPoint(Pos:Vector3D; V:Viewport):Vector3D;
      Function ProjectBoundingBox(Box:BoundingBox; V:Viewport):BoundingBox;

			Property Settings:GraphicsManagerSettings Read _Settings;

      Property ElapsedTime:Single Read _elapsedTime;
      Property Stats:GraphicsManagerStats Read _PrevStats;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;

      Procedure SetBackgroundColor(BG:Color);


      Function AddRenderable(MyRenderable:Renderable; Flags:Cardinal = 0):Boolean;
      //Procedure AttachRenderable(MyRenderable, Owner:Renderable);
      Procedure DeleteRenderable(MyRenderable:Renderable);

      Procedure AddOccluder(MyOccluder:Occluder);

      Procedure SetBlendMode(BlendMode:Integer);
      Procedure SetFog(Value:Boolean);

      Procedure AddViewport(V:Viewport);
      Procedure DeleteViewport(V:Viewport);
      Function GetViewport(Index:Integer):Viewport;
      Procedure SetCurrentViewport(V:Viewport);

      Function GenerateTexture():Cardinal;
      Function GenerateFrameBuffer():Cardinal;
      Function GenerateRenderBuffer():Cardinal;

      Function GenerateStencilID():Byte; 

      Procedure DeleteTexture(Var Handle:Cardinal);
      Procedure DeleteFrameBuffer(Var Handle:Cardinal);
      Procedure DeleteRenderBuffer(Var Handle:Cardinal);

      Function GetScreenshot():Image;

      Function EnableColorShader(Const MyColor:Color; Const Transform:Matrix4x4):Shader;
      Function EnableTextureShader(Const MyColor:Color; Tex:Texture; Const Transform:Matrix4x4):Shader;
      Function EnableColoredTextureShader(Tex:Texture; Const Transform:Matrix4x4):Shader;

      Procedure EnableReflection(Const ReflectionPoint, ReflectionNormal:Vector3D);

      Property ViewportCount:Integer Read _ViewportCount;

      Procedure AddCamera(Cam:Camera);
      Procedure DeleteCamera(Cam:Camera);
      Function GetCamera(Index:Integer):Camera;

      Class Function IsShuttingDown:Boolean;

      Property CameraCount:Integer Read _CameraCount;

      Property ActiveViewport:Viewport Read _CurrentViewport Write SetCurrentViewport;
      Property MainViewport:Viewport Read _MainViewport;
      Property DeviceViewport:Viewport Read _DeviceViewport;
      Property UIViewport:Viewport Read _UIViewport;

      Property Scene:TERRA_Scene.Scene Read _Scene Write SetScene;

      Property WindVector:Vector3D Read _WindVector;

      Property FrameID:Cardinal Read _FrameID;
      Property RenderStage:Integer Read _RenderStage;

      Property Device:TERRAString Read _Device;
      Property Vendor:TERRAString Read _Vendor;
      Property Version:TERRAVersion Read _Version;

      Property ProjectionMatrix:Matrix4x4 Read _Projection;

      Property LightModel:Integer Read _LightModel Write _LightModel;
	End;

Function GetDefaultFullScreenShader():Shader;

Implementation
Uses TERRA_OS, TERRA_Log, TERRA_UI, TERRA_ResourceManager,
  TERRA_Frustum, TERRA_Lights, TERRA_SpriteManager, TERRA_Mesh,
  TERRA_Decals, TERRA_Billboards, TERRA_ParticleRenderer, TERRA_DebugDraw
;

Var
  _GraphicsManager_Instance:ApplicationObject = Nil;
  _ShuttingDown:Boolean = False;

  _SimpleColor:Shader;
  _SimpleTexture:Shader;
  _SimpleTextureColored:Shader;
  _FullscreenQuadShader:Shader;
  _FullscreenColorShader:Shader;

Class Function GraphicsManager.IsShuttingDown:Boolean;
Begin
  Result := _ShuttingDown;
End;


Function GetShader_SimpleColor():TERRAString;
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
  Line('  attribute highp vec4 terra_position;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * modelMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
	Line('  uniform lowp vec4 out_color;');
	Line('	void main()	{');
	Line('	gl_FragColor = out_color;}');
  Line('}');
  Result := S;
End;

Function GetShader_SimpleTexture():TERRAString;
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
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec4 terra_uv;');
  Line('  varying highp vec2 myUVS;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * modelMatrix * terra_position;');
  Line('    myUVS = terra_uv.xy;');
  Line('}}');
  Line('fragment {');
  Line('  varying highp vec2 myUVS;');
  Line('  uniform sampler2D out_texture;');
	Line('  uniform lowp vec4 out_color;');
	Line('	void main()	{');
  Line('  lowp vec4 color = texture2D(out_texture, myUVS.xy);');
	Line('	gl_FragColor = color  * out_color;}');
//	Line('	gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);}');
  Line('}');
  Result := S;
End;

Function GetShader_ColoredTexture():TERRAString;
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
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec4 terra_uv;');
  Line('  attribute highp vec4 terra_color;');
  Line('  varying highp vec2 myUVS;');
  Line('  varying highp vec4 myColor;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * modelMatrix * terra_position;');
  Line('    myUVS = terra_uv.xy;');
  Line('    myColor = terra_color;');
  Line('}}');
  Line('fragment {');
  Line('  varying highp vec2 myUVS;');
  Line('  varying highp vec4 myColor;');
  Line('  uniform sampler2D out_texture;');
	Line('  uniform lowp vec4 out_color;');
	Line('	void main()	{');
  Line('  lowp vec4 color = texture2D(out_texture, myUVS.xy);');
	Line('	gl_FragColor = color  * myColor;}');
//	Line('	gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);}');
  Line('}');
  Result := S;
End;

Function GetShader_StencilVolumeShader():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
	Line('  uniform mat4 cameraMatrix;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  attribute highp vec4 terra_position;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
	Line('	void main()	{');
	Line('	gl_FragColor = vec4(1.0, 1.0, 0.0, 0.5);}');
  Line('}');
  Result := S;
End;

Function GetShader_FullscreenColor():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('  attribute highp vec4 terra_position;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
  Line('  uniform mediump vec4 color;');
	Line('  void main()	{');
  Line('    gl_FragColor = color;}');
  Line('}  ');
  Result := S;
End;

Function GetShader_FullscreenQuad():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying mediump vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec3 terra_UV0;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  texCoord = terra_UV0.xy;}');
  Line('}');
  Line('fragment {');
	Line('  varying mediump vec2 texCoord;');
	Line('  uniform sampler2D texture;');
  Line('  uniform mediump vec4 color;');
	Line('  void main()	{');
  Line('    lowp vec4 c = texture2D(texture, texCoord.st);');
  Line('    c = color * c;');
  {$IFDEF TESTFULLSCREENSHADER}
  Line('    gl_FragColor = vec4(0.0,1.0, 0.0, 1.0);}');
  {$ELSE}
  Line('    gl_FragColor = c;}');
  {$ENDIF}
  Line('}  ');
  Result := S;
End;

Function GetDefaultFullScreenShader():Shader;
Begin
  If (_FullscreenQuadShader = Nil) Then
  Begin
    _FullscreenQuadShader := Shader.CreateFromString(GetShader_FullscreenQuad(), 'fullscreen_quad');
    ShaderManager.Instance.AddShader(_FullscreenQuadShader);
  End;

  Result := _FullscreenQuadShader;
End;

{ Occluder }
Procedure Occluder.SetTransform(Transform:Matrix4x4; Width,Height:Single);
Var
  X1,X2,Y1,Y2:Single;
Begin
  X1 := -Width*0.5;
  X2 := -X1;
  Y1 := 0.0;
  Y2 := Height;

  _P1 := Transform.Transform(VectorCreate(X1, Y1, 0.0));
  _P2 := Transform.Transform(VectorCreate(X2, Y1, 0.0));
  _P3 := Transform.Transform(VectorCreate(X2, Y2, 0.0));
  _P4 := Transform.Transform(VectorCreate(X1, Y2, 0.0));

  _BoundingBox.Reset;
  _BoundingBox.Add(_P1);
  _BoundingBox.Add(_P2);
  _BoundingBox.Add(_P3);
  _BoundingBox.Add(_P4);
End;

Function Occluder.IsVisible:Boolean;
Begin
  Result:=Not ((_EndVertex.X<0) Or (_StartVertex.X>GraphicsManager.Instance.Width) Or
          (_EndVertex.Y<0) Or (_StartVertex.Y>GraphicsManager.Instance.Height) Or
          ((_StartVertex.Z>1) And (_EndVertex.Z>1)));
End;

Procedure Occluder.Update;
Var
   T1,T2,T3,T4:Vector3D;
Begin
  T1 := GraphicsManager.Instance.ActiveViewport.ProjectPoint(_P1);
  T2 := GraphicsManager.Instance.ActiveViewport.ProjectPoint(_P2);
  T3 := GraphicsManager.Instance.ActiveViewport.ProjectPoint(_P3);
  T4 := GraphicsManager.Instance.ActiveViewport.ProjectPoint(_P4);

  _StartVertex := VectorMin(T1, VectorMin(T2, VectorMin(T3, T4)));
  _EndVertex := VectorMax(T1, VectorMax(T2, VectorMax(T3, T4)));
End;

Function Occluder.PointOccluded(P:Vector3D):Boolean;
Begin
    Result := (P.X>_StartVertex.X) And (P.X<_EndVertex.X) And
            (P.Y>_StartVertex.Y) And (P.Y<_EndVertex.Y) And
            (P.Z>FloatMin(_StartVertex.Z, _EndVertex.Z));
End;

Function Occluder.OccluderOccluded(Occ:Occluder):Boolean;
Begin
  Result := (Occ._StartVertex.X>_StartVertex.X) And (Occ._StartVertex.X<_EndVertex.X) And (Occ._StartVertex.Z>FloatMin(_StartVertex.Z, _EndVertex.Z))
          And (Occ._EndVertex.X>_StartVertex.X) And (Occ._EndVertex.X<_EndVertex.X) And (Occ._EndVertex.Z>FloatMin(_StartVertex.Z, _EndVertex.Z));
End;

Function Occluder.BoxOccluded(Box:BoundingBox; V:Viewport):Boolean;
Var
  K:Single;
  A,B:Vector3D;
  {
  Image:TERRA_Image.Image;
  Snap:Boolean;}
Begin
  If (_StartVertex.Z>1) Or (_EndVertex.Z>1) Then
  Begin
    Result := False;
    Exit;
  End;

  {Snap := Application.Instance.Input.Keys.WasPressed(Ord('J'));
  If Snap Then
  Begin
    Image := TERRA_Image.Image.Create;
    Image.New(GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  End;}

  Box := GraphicsManager.Instance.ProjectBoundingBox(Box, V);
  A := Box.StartVertex;
  B := Box.EndVertex;

  If A.X>B.X Then
  Begin
    K := A.X;
    A.X := B.X;
    B.X := K;
  End;

  If A.Y>B.Y Then
  Begin
    K:=A.Y;
    A.Y:=B.Y;
    B.Y:=K;
  End;

  If ((B.X<0) Or (A.X>Application.Instance.Width) Or (B.Y<0) Or (A.Y>Application.Instance.Height) Or
      ((A.Z>1) And (B.Z>1))) Then
  Begin
    Result:=True;
    Exit;
  End;

  {If Snap Then
  Begin
    Image.FillRectangle(Integer(Round(_StartVertex.X)), Integer(Round(_StartVertex.Y)), Integer(Round(_EndVertex.X)), Integer(Round(_EndVertex.Y)), ColorRed);
    Image.FillRectangle(Integer(Round(A.X)), Integer(Round(A.Y)), Integer(Round(B.X)), Integer(Round(B.Y)), ColorBlue);
    Image.Save('occlusion.png');
    Image.Release;
    Halt;
  End;}

  Result:=(Self.PointOccluded(A) And Self.PointOccluded(B));
End;

Function Occluder.GetBoundingBox:BoundingBox;
Begin
  Result := _BoundingBox;
End;

Procedure Occluder.Render(TranslucentPass:Boolean);
Begin
{$IFDEF PC}
  If (TranslucentPass) Then
    Exit;

  GraphicsManager.Instance.EnableColorShader(ColorWhite, Matrix4x4Identity);
  glLineWidth(3);
//  glLineStipple(1, $FF);
//  glEnable(GL_LINE_STIPPLE);

  TextureManager.Instance.WhiteTexture.Bind(0);
  
  glDisable(GL_CULL_FACE);
   glBegin(GL_LINE_STRIP);
  With _P1 Do
    glVertex3f(X,Y,Z);
  With _P2 Do
    glVertex3f(X,Y,Z);
  With _P3 Do
    glVertex3f(X,Y,Z);
  With _P4 Do
    glVertex3f(X,Y,Z);
  With _P1 Do
    glVertex3f(X,Y,Z);
  glEnd;


//  glLineStipple(1, $FFFF);
//  glDisable(GL_LINE_STIPPLE);
{$ENDIF}
End;

{ GraphicsManagerSettings }
Procedure GraphicsManagerSettings.Release;
Begin
  // do nothing
End;

{ GraphicsManagerSetting }
Function GraphicsManagerSetting.Enabled:Boolean;
Begin
  Result := _Enabled;
End;

Function GraphicsManagerSetting.Avaliable:Boolean;
Begin
  Result := _Avaliable;
End;

Procedure GraphicsManagerSetting.SetValue(Value: Boolean);
Begin
  If (Value) And (Not Self._Avaliable) Then
    Value := False;

  If (_Enabled=Value) Then
    Exit;

	Self._Enabled := Value;
  GraphicsManager(_GraphicsManager_Instance.Instance)._Settings._Changed := True;
End;

Procedure GraphicsManagerVariableSetting.SetQuality(Value: Integer);
Begin
  If (_Quality=Value) Then
    Exit;
	Self._Quality := Value;
  GraphicsManager(_GraphicsManager_Instance.Instance)._Settings._Changed := True;
End;

Class Function GraphicsManager.Instance:GraphicsManager;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (Not Assigned(_GraphicsManager_Instance)) Then
    _GraphicsManager_Instance := InitializeApplicationComponent(GraphicsManager, Nil);

  Result := GraphicsManager(_GraphicsManager_Instance.Instance);
End;

Procedure GraphicsManager.Init;
Var
  I:Integer;
  V:Viewport;
  OW, OH:Integer;
  S:TERRAString;
  HasShaders:Boolean;
Begin
  Log(logDebug, 'GraphicsManager', 'Initializing');

  _CurrentViewport := Nil;
  _MainViewport := Nil;
  _DeviceViewport := Nil;
  _UIViewport := Nil;
  _DepthSize := 2048;
  _Settings := GraphicsManagerSettings.Create;

  _BucketOpaque := Pool.Create(coAppend Or coSorted);
  _BucketAlpha :=  Pool.Create(coAppend Or coSortedInverted);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  _BucketReflection := Pool.Create(coAppend);
  {$ENDIF}

  FillChar(_Stats, SizeOf(_Stats), 0);
  FillChar(_PrevStats, SizeOf(_PrevStats), 0);

  If Application.Instance = Nil Then
    Exit;

  _Width := Application.Instance.Width;
  _Height := Application.Instance.Height;

  _BackgroundColor := ColorCreate(0, 0, 0, 255);

  {$IFDEF PRECISIONTIMER}
  _Timer := Timer.Create;
  _accumTimeSec := 0.0;
  {$ENDIF}

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'OnAppCreate');{$ENDIF}


  _FogEnable := False;

  ResetGLState();

  {$IFDEF MOBILE}
  glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @_Settings._MaxTextureUnits);
  {$ELSE}
  glGetIntegerv(GL_MAX_TEXTURE_UNITS, @_Settings._MaxTextureUnits);
  {$ENDIF}
  Log(logDebug, 'GraphicsManager', 'Max texture slots:' + IntToString(_Settings._MaxTextureUnits));

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, @_Settings._MaxTextureSize);
  Log(logDebug, 'GraphicsManager', 'Max texture size:' + IntToString(_Settings._MaxTextureSize));

{$IFDEF MOBILE}
	_Settings._maxRenderTargets := 0;
{$ELSE}
  glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @_Settings._maxRenderTargets);
  Log(logDebug, 'GraphicsManager', 'Max render targets:' + IntToString(_Settings._maxRenderTargets));
{$ENDIF}

  Log(logDebug, 'GraphicsManager', 'Width='+IntToString(_Width)+' Height='+IntToString(_Height));

{$IFDEF PC}
  If (glExtensionSupported('GL_EXT_texture_filter_anisotropic')) Then
  Begin
    glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @_Settings._maxAnisotrophy); 
  End Else
{$ENDIF}
    _Settings._maxAnisotrophy := 0;

  If (glExtensionSupported('GL_ARB_multisample')) Then
    _Settings._multiSampleCount := 4
  Else
    _Settings._multiSampleCount := 0;

  _Settings._VertexCacheSize := 32; 

  _Device := glGetString(GL_RENDERER);   
  _Vendor := glGetString(GL_VENDOR);     

  Log(logDebug, 'GraphicsManager', 'Device: '+_Device);
  Log(logDebug, 'GraphicsManager', 'Vendor: '+_Vendor);

  _Vendor := StringUpper(_Vendor);
  If Pos('INTEL', _Vendor)>0 Then
    _Vendor := 'INTEL'
  Else
  If Pos('NVIDIA', _Vendor)>0 Then
    _Vendor := 'NVIDIA'
  Else
  If (Pos('ATI', _Vendor)>0) Or (Pos('AMD', _Vendor)>0) Then
    _Vendor := 'ATI';

  {$IFDEF PC}
	_Settings.TextureCompression._Avaliable :=  glExtensionSupported('GL_EXT_texture_compression_s3tc');
  {$ELSE}
  {$IFDEF MOBILE}
  _Settings.TextureCompression._Avaliable :=  True;
  {$ELSE}
  _Settings.TextureCompression._Avaliable :=  False;
  {$ENDIF}
  {$ENDIF}

  _Settings.TextureCompression._Enabled := _Settings.TextureCompression._Avaliable;

  {$IFDEF PC}
	HasShaders := (glExtensionSupported('GL_ARB_vertex_shader')) And (glExtensionSupported('GL_ARB_fragment_shader'));
//  HasShaders := HasShaders And (Pos('MESA', _Vendor)<=0);
  {$ELSE}

{$IFDEF IPHONE}
  HasShaders := shadersAvailable();
{$ELSE}
  HasShaders := True;
{$ENDIF}
  {$ENDIF}

  {$IFDEF DISABLESHADERS}
  HasShaders := False;
  {$ENDIF}

  _Settings.Shaders._Avaliable := HasShaders;
	_Settings.VertexBufferObject._Avaliable := glExtensionSupported('GL_ARB_vertex_buffer_object');
  _Settings.VertexBufferObject._Enabled := _Settings.Shaders._Avaliable;

  {$IFDEF FRAMEBUFFEROBJECTS}
    {$IFDEF PC}
  	_Settings.FrameBufferObject._Avaliable := glExtensionSupported('GL_ARB_framebuffer_object') Or glExtensionSupported('GL_EXT_framebuffer_object');
    {$ENDIF}

    {$IFDEF MOBILE}
	  _Settings.FrameBufferObject._Avaliable := True;
    {$ENDIF}
  {$ELSE}
  _Settings.FrameBufferObject._Avaliable := False;
  {$ENDIF}

  {$IFDEF POSTPROCESSING}
  _Settings.PostProcessing._Avaliable := (Settings.FrameBufferObject._Avaliable) {And (_Settings._maxRenderTargets>=4)};
  {$ELSE}
  _Settings.PostProcessing._Avaliable := False;
  {$ENDIF}
  _Settings.PostProcessing._Enabled := _Settings.PostProcessing._Avaliable;

  _Settings.CubeMapTexture._Avaliable := glExtensionSupported('GL_ARB_texture_cube_map');
  _Settings.SeparateBlends._Avaliable := glExtensionSupported('GL_EXT_draw_buffers2');
  _Settings.SeamlessCubeMap._Avaliable := glExtensionSupported('GL_ARB_seamless_cube_map') Or glExtensionSupported('GL_AMD_seamless_cubemap_per_texture');
  {$IFDEF MOBILE}
  _Settings.NPOT._Avaliable := HasShaders;
  {$ELSE}
  _Settings.NPOT._Avaliable := glExtensionSupported('GL_ARB_texture_non_power_of_two');//OES_texture_npot
  {$ENDIF}

  {$IFDEF MOBILE}
  _Settings.PackedStencil._Avaliable := glExtensionSupported('GL_OES_packed_depth_stencil');
  {$ELSE}
  _Settings.PackedStencil._Avaliable := True;
  {$ENDIF}

  {$IFDEF DISABLEOUTLINES}
  _Settings.Outlines._Avaliable := False;
  {$ELSE}
  _Settings.Outlines._Avaliable := True;
  {$ENDIF}
  _Settings.Outlines._Enabled := _Settings.Outlines._Avaliable;

  Log(logDebug, 'GraphicsManager', 'Texture compression: '+  BoolToString(_Settings.TextureCompression.Avaliable));

  Log(logDebug, 'GraphicsManager', 'Shaders: '+  BoolToString(HasShaders));

  Log(logDebug, 'GraphicsManager', 'VertexBufferObject: '+  BoolToString(_Settings.VertexBufferObject.Avaliable));
  Log(logDebug, 'GraphicsManager', 'FrameBufferObject: '+  BoolToString(_Settings.FrameBufferObject.Avaliable));
  Log(logDebug, 'GraphicsManager', 'CubemapTexture: '+  BoolToString(_Settings.CubemapTexture.Avaliable));
  Log(logDebug, 'GraphicsManager', 'FloatTexture: '+  BoolToString(_Settings.FloatTexture.Avaliable));
  Log(logDebug, 'GraphicsManager', 'TextureArray: '+  BoolToString(_Settings.TextureArray.Avaliable));
  Log(logDebug, 'GraphicsManager', 'SeparateBlends: '+  BoolToString(_Settings.SeparateBlends.Avaliable));
  Log(logDebug, 'GraphicsManager', 'SeamlessCubeMap: '+  BoolToString(_Settings.SeamlessCubeMap.Avaliable));
  Log(logDebug, 'GraphicsManager', 'NPOT: '+  BoolToString(_Settings.NPOT.Avaliable));

  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_VECTORS, @_Settings._MaxUniformVectors);
  If (_Settings._MaxUniformVectors<128) Then
    _Settings.VertexBufferObject._Enabled := False;

  S := ' ' + glGetExtensionString();
  StringReplaceText(' ', crLf+#9, S);
  Log(logDebug, 'GraphicsManager', 'Extensions: '+ S);

  _Version := StringToVersion('0.0.0');

  {$IFNDEF MOBILE}
  If (HasShaders) Then
  Begin
    S := glGetString(GL_SHADING_LANGUAGE_VERSION);   
    If (S<>'') Then
    Begin
      Log(logDebug, 'GraphicsManager', 'Version: '+ S);

      I := Pos(' ', S);
      If (I>0) Then
        S := Copy(S, 1, Pred(I));
      _Version := StringToVersion(S);
    End;

    Log(logDebug,'GraphicsManager','GLSL version:'+VersionToString(_Version));
  End Else
  Begin
    Log(logError, 'GraphicsManager', 'Unsupported videocard!');
  End;
  {$ENDIF}

  Render3D := True;
  Render2D := True;

  ShowDebugTarget := -1;

{
  _Settings.TextureCompression._Avaliable := True;
  _Settings.Shaders._Avaliable := True;
  _Settings.VertexBufferObject._Avaliable := True;
  _Settings.FrameBufferObject._Avaliable := True;
  _Settings.VolumeTexture._Avaliable := False;
  _Settings.CubemapTexture._Avaliable := True;
  _Settings.FloatTexture._Avaliable := False;
  _Settings.TextureArray._Avaliable := False;
  _Settings.SeparateBlends._Avaliable := False;
  _Settings.SeamlessCubeMap._Avaliable := False;}

  {TEXTURE_CUBE_MAP_SEAMLESS
  GL_ARB_shader_texture_lod
GL_ARB_texture_compression_rgtc

GL_ARB_draw_instanced

// for gpu particles
GL_ARB_draw_indirect
http://www.opengl.org/registry/specs/ARB/draw_indirect.txt
http://www.opengl.org/registry/specs/EXT/texture_buffer_object.txt
GL_EXT_texture_buffer_object

// useful for HDR
GL_EXT_texture_shared_exponent
http://www.opengl.org/registry/specs/EXT/texture_shared_exponent.txt

// setuniform replacing?
GL_EXT_gpu_program_parameters

GL_EXT_texture_sRGB
http://www.opengl.org/registry/specs/EXT/texture_sRGB.txt

   }

  _Settings.TextureArray._Avaliable := glExtensionSupported('GL_EXT_texture_array');

  _Settings.FloatTexture._Avaliable := glExtensionSupported('GL_ARB_color_buffer_float') Or
                            glExtensionSupported('GL_ATI_pixel_format_float') Or
                            glExtensionSupported('GL_NV_float_buffer');

  _Settings.DeferredLighting._Avaliable := (_Settings._maxRenderTargets>=4) And _Settings.FrameBufferObject._Avaliable;
  _Settings.DeferredFog._Avaliable :=  _Settings.DeferredLighting._Avaliable;
  _Settings.DeferredShadows._Avaliable := _Settings.DeferredLighting._Avaliable;
  _Settings.AlphaFade._Avaliable := True;

  {$IFDEF IPHONE}
  _Settings.DynamicShadows._Avaliable := glExtensionSupported('GL_OES_packed_depth_stencil') Or glExtensionSupported('GL_OES_stencil8');
  {$ELSE}
  _Settings.DynamicShadows._Avaliable := True;
  {$ENDIF}

  _Settings.DeferredLighting._Enabled := _Settings.DeferredLighting._Avaliable;
  _Settings.DeferredFog._Enabled := True;
  _Settings.DeferredShadows._Enabled := True;
  _Settings.FogMode := 0;

  _Settings.DynamicShadows._Enabled := False;

  _Settings.DynamicShadows._Quality := qualityMedium;
  _Settings.Textures._Quality := qualityMedium;

  _Settings.DepthOfField._Avaliable := True;
  _Settings.DepthOfField._Enabled := True;

  _Settings.NormalMapping._Avaliable := True;
  _Settings.NormalMapping._Enabled := False;

  _Settings.LightMapping._Avaliable := True;
  _Settings.LightMapping._Enabled := True;

  _Settings.DynamicLights._Avaliable := True;
  _Settings.DynamicLights._Enabled := True;

  _Settings.ToonShading._Avaliable := True;
  _Settings.ToonShading._Enabled := True;

  _Settings.AlphaTesting._Avaliable := True;
  {$IFDEF MOBILE}
  _Settings.AlphaTesting._Enabled := False;
  {$ELSE}
  _Settings.AlphaTesting._Enabled := True;
  {$ENDIF}

  _Settings.Specular._Avaliable := True;
  _Settings.Specular._Enabled := False;

  _Settings.Fur._Avaliable := True;
  _Settings.Fur._Enabled := True;

  {$IFDEF HAS_REFLECTIONS}
  _Settings.Reflections._Avaliable := True;
  {$ELSE}
  _Settings.Reflections._Avaliable := False;
  {$ENDIF}
  _Settings.Reflections._Enabled := False;

  _Settings.Sky._Avaliable := True;
  _Settings.Sky._Enabled := True;

  _Settings.SelfShadows._Avaliable := True;
  _Settings.SelfShadows._Enabled := False;

  _Settings.SSAO._Avaliable := _Settings.PostProcessing._Avaliable;
  _Settings.SSAO._Enabled := False;

  _Settings.ShadowSplitCount := 3;
  _Settings.ShadowSplitWeight := 0.75;
  _Settings.ShadowMapSize := 1024;
  _Settings.ShadowBias := 2.0;

  Log(logDebug, 'GraphicsManager', 'Device resolution: '+IntToString(_Width)+' x ' +IntToString(_Height));

  _DeviceViewport := Viewport.Create('device', _Width, _Height);

  OW := _Width;
  OH := _Height;

  If Assigned(Application.Instance.Client) Then
  Begin
    Application.Instance.Client.SelectResolution3D(OW,OH);
  End;

  {If (Self.LandscapeOrientation) Then
  Begin
    Temp := OW;
    OW := OH;
    OH := Temp;
  End;}

  Log(logDebug, 'GraphicsManager', 'Selected 3D resolution: '+IntToString(OW)+' x ' +IntToString(OH));

  V := Viewport.Create('main', OW, OH);
  AddViewport(V);

  V.DrawSky := True;
  V.SetRenderTargetState(captureTargetColor, True);
  {$IFDEF POSTPROCESSING}
  V.SetPostProcessingState(_Settings.PostProcessing._Avaliable);
  {$ELSE}
  V.SetPostProcessingState(False);
  {$ENDIF}

  // make UI view
  _UIViewport := Viewport.Create('UI', Application.Instance.UI_Width, Application.Instance.UI_Height, {$IFDEF FRAMEBUFFEROBJECTS}Application.Instance.UI_Scale{$ELSE}1.0{$ENDIF});
  _UIViewport.SetRenderTargetState(captureTargetColor, True);
  _UIViewport.SetTarget(_DeviceViewport, 0, 0, 1.0, 1.0);

  ShowWireframe := False;
  _Settings._Changed := True;

  //_Settings.FloatTexture._Avaliable := False;
//  _Settings.PostProcessing._Enabled := False;

  _Settings.FogColor := ColorCreate(255, 0,0, 255);
  _Settings.FogDensity := 30.0;
  _Settings.FogStart := 0.2;

  Self.ReflectionMatrixSky := Matrix4x4Identity;
  Self.ReflectionMatrix := Matrix4x4Identity;
  
  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure GraphicsManager.AddViewport(V:Viewport);
Begin
  If (V = Nil) Then
    Exit;

  V.OffScreen := True;

  Inc(_ViewportCount);
  SetLength(_Viewports, _ViewportCount);
  _Viewports[Pred(_ViewportCount)] := V;

  If Not Assigned(ActiveViewport) Then
    ActiveViewport := V;

  If Not Assigned(_MainViewport) Then
  Begin
    _MainViewport := V;
    _MainViewport.SetTarget(_DeviceViewport, 0, 0, 1.0, 1.0);
  End
End;

Function GraphicsManager.GetViewport(Index:Integer):Viewport;
Begin
  If (Index<0) Or (Index>=_ViewportCount) Then
    Result := Nil
  Else
    Result := _Viewports[Index];
End;

Procedure GraphicsManager.DeleteViewport(V:Viewport);
Var
  N, I:Integer;
Begin
  If (V=Self.ActiveViewport) Then
    ActiveViewport := Nil;

  N := -1;
  For I:=0 To Pred(_ViewportCount) Do
  If (_Viewports[I] = V) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Viewports[N].Release;
  _Viewports[N] := _Viewports[Pred(_ViewportCount)];
  Dec(_ViewportCount);
End;

Procedure GraphicsManager.AddCamera(Cam:Camera);
Begin
  Inc(_CameraCount);
  SetLength(_Cameras, _CameraCount);
  _Cameras[Pred(_CameraCount)] := Cam;
End;

Function GraphicsManager.GetCamera(Index:Integer):Camera;
Begin
  If (Index<0) Or (Index>=_CameraCount) Then
    Result := Nil
  Else
    Result := _Cameras[Index];
End;

Procedure GraphicsManager.DeleteCamera(Cam:Camera);
Var
  N, I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_CameraCount) Do
  If (_Cameras[I] = Cam) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Cameras[N].Release;
  _Cameras[N] := _Cameras[Pred(_CameraCount)];
  Dec(_CameraCount);
End;

(*Procedure GraphicsManager.RenderAds;
Var
  MyShader:Shader;
Begin
  Self.SetViewArea(0, 0, _Width, _Height);

  MyShader := GetDefaultFullScreenShader();
  ShaderManager.Instance.Bind(MyShader);
  //TextureManager.Instance.WhiteTexture.Bind(0);
  _AdTex.Bind(0);
  //Log(logDebug, 'GraphicsManager', 'Adsize: '+IntToString(_AdTex.Width)+' '+IntToString(_AdTex.Height));
  MyShader.SetUniform('texture', 0);
  MyShader.SetUniform('color', ColorWhite);
  GraphicsManager.Instance.SetBlendMode(blendBlend);
  GraphicsManager.Instance.SetBlendMode(blendNone);

{$IFDEF EMULATED_LANDSCAPE}
    If (_AdOnBottom) Then
        GraphicsManager.Instance.DrawFullscreenQuad(MyShader, 1.0 - (_AdHeight/_Width), 0.0, 1.0, 1.0)
    Else
        GraphicsManager.Instance.DrawFullscreenQuad(MyShader, 0.0, 0.0, _AdHeight/_Width, 1.0);
{$ELSE}
  If (_AdOnBottom) Then
    GraphicsManager.Instance.DrawFullscreenQuad(MyShader, 0.0, 1.0 - (_AdHeight/_Height), 1.0, 1.0)
  Else
    GraphicsManager.Instance.DrawFullscreenQuad(MyShader, 0.0, 0.0, 1.0, _AdHeight/_Height);
{$ENDIF}
End;
*)

Procedure GraphicsManager.RenderUI;
Var
  Flags:Cardinal;
  Target:RenderTarget;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderUI');{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginUIRendering');{$ENDIF}

  _UIViewport.BackgroundColor := ColorNull;
  Target := _UIViewport.GetRenderTarget(captureTargetColor);
  If Assigned(Target) Then
  Begin
    Target.ClearColor := _UIViewport.BackgroundColor;
    Target.BeginCapture();

    {$IFDEF PC}
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
    {$ENDIF}


    Self.SetBlendMode(blendAdd);
    Self.SetBlendMode(blendBlend);
    Self.SetFog(False);
    //glEnable(GL_SCISSOR_TEST);

    _Projection := Matrix4x4Ortho(0.0, _UIViewport.Width, _UIViewport.Height, 0.0, 0, 100);
    _Projection := Matrix4x4Multiply4x4(_Projection, Matrix4x4Translation(0.375, 0.375, 0.0));

    _UIViewport.SetViewArea(0, 0, _UIViewport.Width, _UIViewport.Height);

    Flags := GL_DEPTH_BUFFER_BIT  Or GL_STENCIL_BUFFER_BIT;
    If (Not Assigned(_Scene)) Or (_ViewportCount<=0) Then
      Flags := Flags Or GL_COLOR_BUFFER_BIT;
    glClear(Flags);

    If (Not _Prefetching) Then
    Begin
      UIManager.Instance.Render();

      If (Assigned(Scene)) And (Not Application.Instance.HasFatalError) Then
       Scene.RenderSprites(Nil);
    End;

    DrawDebug2DObjects();

    SpriteManager.Instance.Render();

    If ( Not _Prefetching) Then
    Begin
      UIManager.Instance.AfterEffects();
    End;

//  glDisable(GL_SCISSOR_TEST);
  //glDisable(GL_ALPHA_TEST);

    Target.EndCapture();
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'FinishedUIRendering');{$ENDIF}


  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure GraphicsManager.RenderShadowmap(View:Viewport);
Begin
  If (Not Assigned(_Scene)) Then
    Exit;

  _Scene.RenderShadowCasters(View);
End;

{$IFNDEF REFLECTIONS_WITH_STENCIL}
Procedure GraphicsManager.RenderReflections(View:Viewport);
Var
  Normal:Vector3D;
//  _Plane:Plane;
Begin
  If (Not Assigned(_Scene)) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderReflections');{$ENDIF}

  Self.ReflectionMask := View.GetRenderTarget(captureTargetReflection);

  If Self.ReflectionMask = Nil Then
    Exit;

  Self.ReflectionMatrix := Matrix4x4Mirror(_ReflectionPoint, _ReflectionNormal);
  Self.ReflectionMatrixSky := Matrix4x4Mirror(VectorZero, _ReflectionNormal);
    Normal := VectorScale(_ReflectionNormal, -1.0);
    {_Plane.A := Normal.X;
    _Plane.B := Normal.Y;
    _Plane.C := Normal.Z;
    _Plane.D := - Obj.ReflectionPoint.Dot(Normal);}

  ActiveViewport.Camera.SetClipPlane(_ReflectionPoint, _ReflectionNormal);

  glEnable(GL_CULL_FACE);
  glCullFace(GL_FRONT);

  Self.ReflectionActive := True;
  Self.RenderList(_BucketOpaque, False);
  Self.RenderList(_BucketAlpha, True);
  Self.ReflectionActive := False;

  glCullFace(GL_BACK);

  ActiveViewport.Camera.RemoveClipPlane();

  Self.ReflectionMatrixSky := Matrix4x4Identity;
  Self.ReflectionMatrix := Matrix4x4Identity;
  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

{$ELSE}
Procedure GraphicsManager.RenderReflections(View:Viewport);
Var
  StencilID:Byte;
  P:ListObject;
  Obj:Renderable;
  Normal:Vector3D;
//  _Plane:Plane;
Begin
  If (Not Assigned(_Scene)) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderReflections');{$ENDIF}

  _BucketReflection.Clear();

  _renderStage := renderStageReflection;
  _Scene.RenderReflections(View);

  If (_FullscreenColorShader = Nil) Then
  Begin
    _FullscreenColorShader := Shader.CreateFromString(GetShader_FullscreenColor(), 'fullscreen_color');
    ShaderManager.Instance.AddShader(_FullscreenColorShader);
  End;

  P := _BucketReflection.First;
  While (Assigned(P)) Do
  Begin
    _renderStage := renderStageReflection;

    Obj := RenderableProxy(P)._Object;
    If Obj = Nil Then
      Break;


    {$IFDEF REFLECTIONS_WITH_ALPHA}
    Self.SetBlendMode(blendNone);
    ShaderManager.Instance.Bind(_FullscreenColorShader);
    _FullscreenColorShader.SetUniform('color', ColorCreate(255, 0, 0, 0));
    glColorMask(False, False, False, True);
    Self.DrawFullscreenQuad(_FullscreenColorShader, 0, 0, 1, 1);
    {$ENDIF}

    Self.ReflectionMatrix4x4 := Matrix4x4Mirror(Obj.ReflectionPoint, Obj.ReflectionNormal);
    Self.ReflectionMatrix4x4Sky := Matrix4x4Mirror(VectorZero, Obj.ReflectionNormal);
    Normal := VectorScale(Obj.ReflectionNormal, -1.0);
    {_Plane.A := Normal.X;
    _Plane.B := Normal.Y;
    _Plane.C := Normal.Z;
    _Plane.D := - Obj.ReflectionPoint.Dot(Normal);}

    StencilID := GenerateStencilID();

    {$IFNDEF REFLECTIONS_WITH_ALPHA}
    // render plane to stencil
    //glClear(GL_STENCIL_BUFFER_BIT);
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS, StencilID, $FFFFFFFF);
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
    glColorMask(False, False, False, False);
    {$ENDIF}

    glDepthMask(False);

    Self.ReflectionTemp := True;
    Self.ReflectionStencil := True;

    If (Obj.IsOpaque) Then
      Obj.Render(False);

    If (Obj.IsTranslucent) Then
      Obj.Render(True);

    Self.ReflectionTemp := False;
    Self.ReflectionStencil := False;

    _renderStage := renderStageDiffuse;

    glDepthMask(True);
    glColorMask(True, True, True, True);

    {$IFNDEF REFLECTIONS_WITH_ALPHA}
    glStencilFunc(GL_EQUAL, StencilID, $FFFFFFFF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    {$ENDIF}


    ActiveViewport.Camera.SetClipPlane(Obj.ReflectionPoint, Obj.ReflectionNormal);

    If (View.DrawSky) And (Not View.Camera.Ortho) Then
       _Scene.RenderSky(View);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    Self.ReflectionActive := True;
    Self.RenderList(_BucketOpaque, False);
    Self.RenderList(_BucketAlpha, True);
    Self.ReflectionActive := False;
    glCullFace(GL_BACK);

    ActiveViewport.Camera.RemoveClipPlane();

    {$IFNDEF REFLECTIONS_WITH_ALPHA}
    glDisable(GL_STENCIL_TEST);
    {$ENDIF}

    _renderStage := renderStageDiffuse;
    Self.ReflectionTemp := True;

    {$IFNDEF DEBUG_REFLECTIONS}
    If (Obj.IsOpaque) Then
      Obj.Render(False);

    If (Obj.IsTranslucent) Then
      Obj.Render(True);
    {$ENDIF}

    Self.ReflectionTemp := False;

    P := P.Next;
  End;

  _renderStage := renderStageDiffuse;
  Self.ReflectionMatrix4x4Sky := Matrix4x4Identity;
  Self.ReflectionMatrix4x4 := Matrix4x4Identity;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;
{$ENDIF}

Var
  _StencilVolumeShader:Shader;
  _StencilShadowShader:Shader;

Const
  StencilQuad:Array[0..11] Of Single = (1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0);

Procedure GraphicsManager.RenderStencilShadows(View:Viewport);
Var
  StencilID:Byte;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderStencilShadows');{$ENDIF}

  If (_FullscreenColorShader = Nil) Then
  Begin
    _FullscreenColorShader := Shader.CreateFromString(GetShader_FullscreenColor(), 'fullscreen_color');
    ShaderManager.Instance.AddShader(_FullscreenColorShader);
  End;

  StencilID := 64;

  Self.SetBlendMode(blendNone);

  ShaderManager.Instance.Bind(_FullscreenColorShader);
  _FullscreenColorShader.SetUniform('color', ColorWhite);

  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, StencilID, $FFFFFFFF);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  glColorMask(False, False, False, False);

  Self.DrawFullscreenQuad(_FullscreenColorShader, 0, 0, 1, 1);

  {$IFDEF PC}
  glActiveTexture(GL_TEXTURE0);
  glDisable(GL_TEXTURE_2D);
  {$ENDIF}

  // Disable z-buffer writes (note: z-testing still occurs), and enable the stencil-buffer
  glDepthMask(False);
  glEnable(GL_DEPTH_TEST);

  If ShowShadowVolumes Then
  Begin
    GraphicsManager.Instance.SetBlendMode(blendJoin);
    glColorMask(True, True, True, True);
  End Else
    glColorMask(False, False, False, False);

  If (_StencilVolumeShader = Nil) Then
  Begin
    _StencilVolumeShader := Shader.CreateFromString(GetShader_StencilVolumeShader(), 'stencilvolumes');
    ShaderManager.Instance.AddShader(_StencilVolumeShader);
  End;

  Log(logDebug, 'Shadow', 'Drawing stencil shadows');

  ShaderManager.Instance.Bind(_StencilVolumeShader);
  _StencilVolumeShader.SetUniform('cameraMatrix', Self.ActiveViewport.Camera.Transform);
  _StencilVolumeShader.SetUniform('projectionMatrix', Self.ActiveViewport.Camera.Projection);

  _RenderStage := renderStageShadow;

  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  // Set up stencil compare fuction, reference value, and masks.
  // Stencil test passes if ((ref  and mask) cmpfn (stencil  and mask)) is true.
  glStencilFunc(GL_ALWAYS, 0, $FFFFFFFF);
  glStencilOp(GL_KEEP,GL_KEEP, GL_INCR_WRAP);

  _Scene.RenderShadowCasters(View);

  If Not ShowShadowVolumes Then
  Begin
    glCullFace(GL_FRONT);
    glStencilOp(GL_KEEP, GL_KEEP, GL_DECR_WRAP);
    _Scene.RenderShadowCasters(View);

    glCullFace(GL_BACK);

    SetBlendMode(blendBlend);

    If (_FullscreenColorShader = Nil) Then
    Begin
      _FullscreenColorShader := Shader.CreateFromString(GetShader_FullscreenColor(), 'fullscreen_color');
      ShaderManager.Instance.AddShader(_FullscreenColorShader);
    End;

    ShaderManager.Instance.Bind(_FullscreenColorShader);
    _FullscreenColorShader.SetUniform('color', ColorCreate(Byte(0), Byte(0), Byte(0), Byte(55)));

    // Only write where stencil val >= 1 (count indicates # of shadows that overlap that pixel)
    glStencilFunc(GL_EQUAL, Succ(StencilID), $FFFFFFFF);

    glColorMask(True, True, True, True);

    // Draws a big gray polygon over scene according to the mask in the stencil buffer. (Any pixel with stencil==1 is in the shadow.)
    Self.DrawFullscreenQuad(_FullscreenColorShader, 0, 0, 1, 1);

    SetBlendMode(blendBlend);
  End;

  _RenderStage := renderStageDiffuse;

  SetBlendMode(blendNone);

  //Restore render states
  glStencilFunc(GL_ALWAYS, $0, $FFFFFFFF);
  glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
  glDisable(GL_STENCIL_TEST);

  glDepthMask(True);

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure GraphicsManager.SetWind(WindDirection:Vector3D; WindIntensity:Single);
Begin
  _WindVector := WindDirection;
  _WindVector.Normalize;
  _WindVector.Scale(WindIntensity);
End;

Procedure GraphicsManager.RenderSceneInternal(View:Viewport; Pass: Integer);
Var
  N:Integer;
Begin
  {$IFDEF POSTPROCESSING}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Or (Not View.IsPostProcessingEnabled()) Then
    N := renderStageDiffuse
  Else
  Case Pass Of
    captureTargetColor:   N := renderStageDiffuse;
    captureTargetNormal:  N := renderStageNormal;
    captureTargetEmission: N := renderStageGlow;
    captureTargetRefraction: N := renderStageRefraction;
    captureTargetReflection: N := renderStageReflection;
    captureTargetOutline: N := renderStageOutline;
    Else
      Exit;
  End;
  {$ELSE}
  N := renderStageDiffuse;
  {$ENDIF}

  If (N = renderStageReflection) And (Not Self._ReflectionsEnabled) Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderSceneInternal');{$ENDIF}

  _RenderStage := N;

  If (Assigned(_Scene)) And (View.DrawSky) And (Not View.Camera.Ortho) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderSky');{$ENDIF}

    {$IFDEF POSTPROCESSING}
    If Pass = captureTargetRefraction Then
    Begin
      IntToString(2);
    End Else
    If Pass = captureTargetEmission Then
      _Scene.RenderSkyEmission(View)
    Else
    {$ENDIF}
      _Scene.RenderSky(View);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderOpaqueBucket');{$ENDIF}

  {$IFNDEF DEBUG_REFLECTIONS}
  RenderList(_BucketOpaque, False);

  If (_RenderStage <> renderStageNormal) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderDecals');{$ENDIF}
    DecalManager.Instance.Render();

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderBillboards');{$ENDIF}
    BillboardManager.Instance.Render();
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderAlphaBucket');{$ENDIF}
  RenderList(_BucketAlpha, True);
  {$ENDIF}

  If (_RenderStage = renderStageDiffuse) Then
  Begin

    If (_ReflectionsEnabled) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderReflections');{$ENDIF}
      Self.RenderReflections(View);
    End;

    If (GraphicsManager.Instance.Settings.Shaders.Avaliable) And (_Settings.DynamicShadows.Enabled) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderStencilShadows');{$ENDIF}
      RenderStencilShadows(View);
    End;
    
  End;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure GraphicsManager.RenderViewport(View:Viewport);
Var
  I, Count:Integer;
  Target:RenderTarget;
Begin
  If Not View.Active Then
    Exit;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderViewport');{$ENDIF}
  SetCurrentViewport(View);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'View.Bind');{$ENDIF}
  View.Bind();

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'LightManager.Clear');{$ENDIF}
  LightManager.Instance.Clear;
  _Occluders := Nil;
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Buckets.Clear');{$ENDIF}

  _BucketOpaque.Clear();
  _BucketAlpha.Clear();

  _ReflectionsEnabled := False;

  // fill renderables list
  _RenderStage := renderStageDiffuse;
  If (Assigned(_Scene)) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderEverything');{$ENDIF}
    _Scene.RenderViewport(View);
  End;

  DrawDebug3DObjects();

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Particles.Render');{$ENDIF}
  ParticleManager.Instance.Render();

  Count := 0;
  For I:=Pred(MaxCaptureTargets) DownTo 0 Do
  Begin
    Target := View.GetRenderTarget(I);
    If (Target = Nil) Then
      Continue;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering viewport: '+View.Name+', target '+TargetNames[I]+', width:'+IntToString(Target.Width)+', height:'+IntToString(Target.Height));{$ENDIF}

    Case I Of
      captureTargetRefraction:
        Target.ClearColor := ColorNull;

      captureTargetReflection:
        Target.ClearColor := ColorBlack;

      captureTargetColor:
        Target.ClearColor := View.BackgroundColor;

      captureTargetOutline:
        Target.ClearColor := ColorBlack;
        //Target.ClearColor := ColorRed;
    Else
      Target.ClearColor := ColorBlack;
    End;

    Target.BeginCapture();
    Self.RenderSceneInternal(View, I);
    Target.EndCapture();

    Inc(Count);

    {If (_RenderStage = renderStageDiffuse) And (Application.Instance.Input.Keys.WasPressed(keyMouseLeft)) Then
      Target.Save(Application.Instance.DocumentPath+PathSeparator+ 'frame.png');}
    
    {$IFDEF PC}
    {If (_RenderStage = renderStageGlow) And (Application.Instance.Input.Keys.WasPressed(Ord('M'))) Then
      Target.Save('bloom.png');
     If (_RenderStage = renderStageRefraction) And (Application.Instance.Input.Keys.WasPressed(Ord('N'))) Then
      Target.Save('refraction.png');}
    {$ENDIF}
  End;

  If (Count<=0) Then
    Log(logWarning, 'GraphicsManager', 'Invalid viewport: '+View.Name);

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure GraphicsManager.RenderScene;
Var
  I:Integer;
Begin
  If Not Assigned(_Scene) Then
    Exit;


  If _Settings._Changed Then
    OnSettingsChange;


  Inc(_FrameID);

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'RenderScene');{$ENDIF}


  //glAlphaFunc(GL_GREATER, 0.1);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginSceneRendering '+IntToString(_ViewportCount));{$ENDIF}

  If (Not Application.Instance.HasFatalError) Then
  For I:=Pred(_ViewportCount) DownTo 0 Do
    RenderViewport(_Viewports[I]);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'EndSceneRendering');{$ENDIF}

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Type
  FullscreenQuadArray = Array[0..23] Of Single;

Procedure InitFullScreenQuad(Var FullscreenQuad:FullscreenQuadArray; X1,Y1,X2,Y2:Single; Orientation:Integer);
Begin
  FullscreenQuad[8] := X2;
  FullscreenQuad[9] := Y1;

  FullscreenQuad[4] := X2;
  FullscreenQuad[5] := Y2;

  FullscreenQuad[0] := X1;
  FullscreenQuad[1] := Y2;

  FullscreenQuad[20] := X1;
  FullscreenQuad[21] := Y2;

  FullscreenQuad[16] := X1;
  FullscreenQuad[17] := Y1;

  FullscreenQuad[12] := X2;
  FullscreenQuad[13] := Y1;

  Case Orientation Of
    orientationLandscapeLeft:
    Begin
      FullscreenQuad[2] := 1.0;
      FullscreenQuad[3] := 0.0;

      FullscreenQuad[6] :=  1.0;
      FullscreenQuad[7] :=  1.0;

      FullscreenQuad[10] := 0.0;
      FullscreenQuad[11] := 1.0;

      FullscreenQuad[14] := 0.0;
      FullscreenQuad[15] := 1.0;

      FullscreenQuad[18] := 0.0;
      FullscreenQuad[19] := 0.0;

      FullscreenQuad[22] := 1.0;
      FullscreenQuad[23] := 0.0;
    End;

    orientationLandscapeRight:
    Begin
      FullscreenQuad[2] := 0.0;
      FullscreenQuad[3] := 1.0;

      FullscreenQuad[6] :=  0.0;
      FullscreenQuad[7] :=  0.0;

      FullscreenQuad[10] := 1.0;
      FullscreenQuad[11] := 0.0;

      FullscreenQuad[14] := 1.0;
      FullscreenQuad[15] := 0.0;

      FullscreenQuad[18] := 1.0;
      FullscreenQuad[19] := 1.0;

      FullscreenQuad[22] := 0.0;
      FullscreenQuad[23] := 1.0;
    End;

    orientationPortrait:
    Begin
      FullscreenQuad[2] := 0.0;
      FullscreenQuad[3] := 0.0;

      FullscreenQuad[6] :=  1.0;
      FullscreenQuad[7] :=  0.0;

      FullscreenQuad[10] := 1.0;
      FullscreenQuad[11] := 1.0;

      FullscreenQuad[14] := 1.0;
      FullscreenQuad[15] := 1.0;

      FullscreenQuad[18] := 0.0;
      FullscreenQuad[19] := 1.0;

      FullscreenQuad[22] := 0.0;
      FullscreenQuad[23] := 0.0;
    End;

    orientationPortraitInverted:
    Begin
      FullscreenQuad[2] := 1.0;
      FullscreenQuad[3] := 1.0;

      FullscreenQuad[6] :=  0.0;
      FullscreenQuad[7] :=  1.0;

      FullscreenQuad[10] := 0.0;
      FullscreenQuad[11] := 0.0;

      FullscreenQuad[14] := 0.0;
      FullscreenQuad[15] := 0.0;

      FullscreenQuad[18] := 1.0;
      FullscreenQuad[19] := 0.0;

      FullscreenQuad[22] := 1.0;
      FullscreenQuad[23] := 1.0;
    End;
  End;
End;

Procedure GraphicsManager.DrawFullscreenQuad(CustomShader:Shader; X1,Y1,X2,Y2:Single);
Var
  M,Projection:Matrix4x4;
  I:Integer;
  PositionHandle, UVHandle:Integer;
  //Delta:Single;
  {Temp, }FullscreenQuad:FullscreenQuadArray;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'DrawFullScreenQuad');{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'CurrentView: ('+FloatToString(_ViewX)+','+FloatToString(_ViewY)+','+FloatToString(_ViewWidth)+','+FloatToString(_ViewHeight)+')');{$ENDIF}
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'DrawFullScreenQuad: ('+FloatToString(X1)+','+FloatToString(Y1)+','+FloatToString(X2)+','+FloatToString(Y2)+')');{$ENDIF}

  {$IFDEF TESTFULLSCREENSHADER}
  CustomShader := Nil;
  {$ENDIF}

  If (CustomShader = Nil) And (Settings.Shaders.Avaliable) Then
  Begin
    CustomShader := GetDefaultFullScreenShader();
    ShaderManager.Instance.Bind(CustomShader);
  End;

  Y1 := 1.0 - Y1;
  Y2 := 1.0 - Y2;

  {$IFNDEF DISABLEORIENTATIONANIMATIONS}
  Delta := Application.Instance.GetOrientationDelta();
  If Delta<1 Then
  Begin
    InitFullScreenQuad(Temp, X1,Y1,X2,Y2, Application.Instance.PreviousOrientation);
    InitFullScreenQuad(FullscreenQuad, X1,Y1,X2,Y2, Application.Instance.Orientation);

    I := 2;
    While I<=23 Do
    Begin
      FullscreenQuad[I] := FullscreenQuad[I] * Delta + Temp[I] * (1- Delta);
      Inc(I);
      FullscreenQuad[I] := FullscreenQuad[I] * Delta + Temp[I] * (1- Delta);
      Inc(I, 3);
    End;
  End Else
  {$ENDIF}
    InitFullScreenQuad(FullscreenQuad, X1,Y1,X2,Y2, Application.Instance.Orientation);

  Projection := Matrix4x4Ortho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);
  glDepthMask(False);

  {$IFDEF PC}
  If (Not Settings.Shaders.Avaliable) Then
  Begin
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@Projection);

    M := Matrix4x4Identity;

    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@M);

    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@M);

    For I:=Pred(Self.Settings._MaxTextureUnits) DownTo 1 Do
    Begin
      glActiveTexture(GL_TEXTURE0 + I);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      glDisable(GL_TEXTURE_2D);
    End;

    glColor4f(1.0, 1.0, 1.0, 1.0);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);

    glVertexPointer(3, GL_FLOAT, 16, @(FullscreenQuad[0]));
    glTexCoordPointer(2, GL_FLOAT, 16, @(FullscreenQuad[2]));
  End Else
  {$ENDIF}
  Begin
    CustomShader.SetUniform('projectionMatrix', Projection);
    PositionHandle := CustomShader.GetAttribute('terra_position');
    UVHandle := CustomShader.GetAttribute('terra_UV0');

    If (PositionHandle>=0) Then
      glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, 16, @(FullscreenQuad[0]));
      
    If (UVHandle>=0) Then
      glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, 16, @(FullscreenQuad[2]));

  End;

  glDrawArrays(GL_TRIANGLES, 0, 6);
  Inc(_Stats.TriangleCount, 2);

  {$IFDEF PC}
  If (Not Settings.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;
  {$ENDIF}

  glEnable(GL_DEPTH_TEST);
  glDepthMask(True);

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;


{Procedure GraphicsManager.RenderCubeMapAt(Position:LVector; FrameBuffer:FrameBufferObject);
Var
  I:Integer;
Begin
	glMatrix4x4Mode(GL_PROJECTION);
	glPushMatrix4x4();
	glLoadIdentity();
	gluPerspective(90.0, 1.0, 0.1, CAMERA_ZFAR);

	for (int I = 0; I<6; I++)
	Begin
		Vector v;
		Vector up;
		if (I==2 || I == 3)
			up.Set(0.0f, 0.0f, 1.0f);
		else
			up.Set(0.0f, 1.0f, 0.0f);
		Case (I) Of
		case 0: v.Set(1.0, 0.0, 0.0);
				break;
		case 1: v.Set(-1.0, 0.0, 0.0);
				break;
		case 2: v.Set(0.0, 1.0, 0.0);
				break;
		case 3: v.Set(0.0, -1.0, 0.0);
				break;
		case 4: v.Set(0.0, 0.0, 1.0);
				break;
		case 5: v.Set(0.0, 0.0, -1.0);
				break;
		End;

		glMatrix4x4Mode(GL_MODELVIEW);
		glPushMatrix4x4();
		glLoadIdentity();
		gluLookAt(pos.x, pos.y, pos.z, pos.x + v.x, pos.y + v.y, pos.z + v.z, up.x, up.y, up.z);

		_rtCubemap->BeginCapture();
			glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
			glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
			Scene::Instance()->Draw();
			RenderSkyBox();
		_rtCubemap->EndCapture();

		glPopMatrix4x4();

		char ss[50];
		sprintf(ss, "%s_%s.png", fileName.c_str(), CubeMapTexture::GetFaceName(I));
		_rtCubemap->Save(ss);
	End;

	glMatrix4x4Mode(GL_PROJECTION);
	glPopMatrix4x4();
End;}

Procedure InsertIntoPool(Target:Pool; MyRenderable:Renderable; Unsorted:Boolean);
Var
  Proxy:ListObject;
Begin
  Proxy := Target.Recycle();
  If Assigned(Proxy) Then
  Begin
    RenderableProxy(Proxy)._Object := MyRenderable;
  End Else
    Proxy := RenderableProxy.Create(MyRenderable);

  Target.Add(Proxy, True);
End;

Procedure RemoveFromPool(Target:List; MyRenderable:Renderable);
Var
  P:RenderableProxy;
Begin
  P := RenderableProxy(Target.First);
  While Assigned(P) Do
  Begin
    If (P._Object = MyRenderable) Then
    Begin
      P._Object := Nil;
      Exit;
    End;
    P := RenderableProxy(P.Next);
  End;
End;

Procedure GraphicsManager.DeleteRenderable(MyRenderable:Renderable);
Begin
  RemoveFromPool(_BucketOpaque, MyRenderable);
  RemoveFromPool(_BucketAlpha, MyRenderable);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  RemoveFromPool(_BucketReflection, MyRenderable);
  {$ENDIF}
End;

{Procedure GraphicsManager.AttachRenderable(MyRenderable, Owner:Renderable);
Var
  P:Renderable;
Begin
  P := Owner._Next;
  Owner._Next := MyRenderable;
  MyRenderable._Next := P;
  MyRenderable._Previous := Owner;
  If Assigned(P) Then
    P._Previous := MyRenderable;
End;}

Function GraphicsManager.IsBoxVisible(Box:BoundingBox):Boolean;
Var
  Occ:Occluder;
Begin
  Result := False;
  If (Not ActiveViewport.Camera.Frustum.BoxVisible(Box)) Then
    Exit;

  // occlusion test
  Occ := _Occluders;
  While Assigned(Occ) Do
  If (Occ.BoxOccluded(Box, ActiveViewport)) Then
  Begin
    Exit;
  End Else
    Occ := Occ._Next;

  Result := True;
End;

Function GraphicsManager.AddRenderable(MyRenderable:Renderable; Flags:Cardinal):Boolean;
Const
  LODS:Array[0..MaxLODLevel] Of Single = (0.0, 0.4, 0.8, 1.0);
Var
  Box:BoundingBox;
  Pos:Vector3D;
  I:Integer;
  FarDist:Single;
  Unsorted:Boolean;
Begin
  If Not Assigned(MyRenderable) Then
  Begin
    Result := False;
    Exit;
  End;

  Unsorted := (Flags And renderFlagsSkipSorting<>0);
  MyRenderable._Flags := Flags;

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (_RenderStage = renderStageReflection) Then
  Begin
    InsertIntoPool(_BucketReflection, MyRenderable, Unsorted);
    Exit;
  End;
  {$ENDIF}

  If (_RenderStage<>renderStageDiffuse) Then
  Begin
    MyRenderable.Render(False);
    Result := True;
    Exit;
  End;

//Log(logDebug, 'GraphicsManager', 'Rendering lights...');
//Log(logDebug, 'GraphicsManager', 'Class: '+MyRenderable.ClassName);

  If (_Settings.DynamicLights.Enabled) Then
    MyRenderable.RenderLights();

  If (MyRenderable._LastUpdate <> Self._FrameID) Then
  Begin
    MyRenderable._LastUpdate := Self._FrameID;
    MyRenderable.Update();
  End;
    
  // frustum test
  Box := MyRenderable.GetBoundingBox;
  If (Flags And renderFlagsSkipFrustum=0) And (Not Self.IsBoxVisible(Box)) Then
  Begin
    MyRenderable._WasVisible := False;
    Result := False;
    Exit;
  End;

  Inc(_Stats.RenderableCount);
  MyRenderable._WasVisible := True;

  Pos := VectorAdd(Box.Center , VectorScale(ActiveViewport.Camera.View, -Box.Radius));
  MyRenderable._Distance := Pos.Distance(ActiveViewport.Camera.Position);

  FarDist := Self.ActiveViewport.Camera.Far;

  For I:=1 To MaxLODLevel Do
  If (MyRenderable._Distance < LODS[I]*FarDist) Then
  Begin
    MyRenderable._LOD := Pred(I) + ((MyRenderable._Distance - (LODS[Pred(I)]*FarDist)) / ((LODS[I] - LODS[Pred(I)])*FarDist));
    Break;
  End Else
  If (I >= MaxLODLevel) Then
    MyRenderable._LOD := MaxLODLevel;

  If (MyRenderable.IsTranslucent()) Then
    InsertIntoPool(_BucketAlpha, MyRenderable, Unsorted);

  If (MyRenderable.IsOpaque()) Then
    InsertIntoPool(_BucketOpaque, MyRenderable, Unsorted);

  Result := True;
End;


Procedure GraphicsManager.SetFog(Value:Boolean); {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If (_FogEnable = Value) Then
    Exit;

  _FogEnable := Value;
  (*
  If _FogEnable Then
    glEnable(GL_FOG)
  Else
    glDisable(GL_FOG);
    *)
End;

Procedure GraphicsManager.SetBlendMode(BlendMode:Integer);
Var
  NeedsAlpha:Boolean;
Begin
  If (BlendMode = _CurrentBlendMode) Then
    Exit;

  NeedsAlpha := BlendMode>0;

  (*If (Settings.SeparateBlends.Avaliable) And (Settings.PostProcessing.Avaliable)
  And (Shader.ActiveShader<>Nil) And (Shader.ActiveShader.MRT) Then
  Begin
    If NeedsAlpha Then
      glEnableIndexedEXT(GL_BLEND, 0)
    Else
      glDisableIndexedEXT(GL_BLEND, 0);

  End Else*)
  Begin
    If NeedsAlpha Then
      glEnable(GL_BLEND)
    Else
      glDisable(GL_BLEND);

  End;

  Case BlendMode Of
  blendBlend:   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  blendAdd:     glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  blendFilter:  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  blendModulate:glBlendFunc(GL_SRC_COLOR, GL_ONE);
  blendJoin:    glBlendFunc(GL_ONE, GL_ONE);
  blendZero:    glBlendFunc(GL_ZERO, GL_ZERO);
  blendOne:     glBlendFunc(GL_ONE, GL_ZERO);
  blendColor:   glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);
  blendColorAdd:   glBlendFunc(GL_SRC_COLOR, GL_ONE);
  blendReflection:   glBlendFunc(GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA);
  End;

  _CurrentBlendMode := BlendMode;
End;

Function GraphicsManager.EnableColorShader(Const MyColor:Color; Const Transform:Matrix4x4):Shader;
Begin
  If (Not Assigned(_SimpleColor)) Then
  Begin
    _SimpleColor := Shader.CreateFromString(GetShader_SimpleColor(), 'simple_color');
    ShaderManager.Instance.AddShader(_SimpleColor);
  End;

  Result := _SimpleColor;
  If (Not Assigned(Result)) Then
    Exit;

  ShaderManager.Instance.Bind(Result);
  Result.SetUniform('cameraMatrix', ActiveViewport.Camera.Transform);
  Result.SetUniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetUniform('modelMatrix', Transform);
  Result.SetUniform('out_color', MyColor);
End;

Function GraphicsManager.EnableTextureShader(Const MyColor:Color; Tex:Texture; Const Transform:Matrix4x4):Shader;
Begin
  If (Not Assigned(_SimpleTexture)) Then
  Begin
    _SimpleTexture := Shader.CreateFromString(GetShader_SimpleTexture(), 'simple_texture');
    ShaderManager.Instance.AddShader(_SimpleTexture);
  End;

  Tex.Bind(0);

  Result := _SimpleTexture;
  If (Not Assigned(Result)) Then
    Exit;

  ShaderManager.Instance.Bind(Result);
  Result.SetUniform('cameraMatrix', ActiveViewport.Camera.Transform);
  Result.SetUniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetUniform('modelMatrix', Transform);
  Result.SetUniform('out_color', MyColor);
  Result.SetUniform('out_texture', 0);
End;

Function GraphicsManager.EnableColoredTextureShader(Tex:Texture; Const Transform:Matrix4x4):Shader;
Begin
  If (Not Assigned(_SimpleTextureColored)) Then
  Begin
    _SimpleTextureColored := Shader.CreateFromString(GetShader_ColoredTexture(), 'colored_texture');
    ShaderManager.Instance.AddShader(_SimpleTextureColored);
  End;

  Tex.Bind(0);

  Result := _SimpleTextureColored;
  If (Not Assigned(Result)) Then
    Exit;

  ShaderManager.Instance.Bind(Result);
  Result.SetUniform('cameraMatrix', ActiveViewport.Camera.Transform);
  Result.SetUniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetUniform('modelMatrix', Transform);
  Result.SetUniform('out_texture', 0);
End;

Procedure GraphicsManager.Release;
Var
  I:Integer;
Begin
  _ShuttingDown := True;
  Log(logDebug, 'GraphicsManager', 'Shutting down');

  For I:=0 To Pred(_CameraCount) Do
    ReleaseObject(_Cameras[I]);
  _CameraCount := 0;

  For I:=0 To Pred(_ViewportCount) Do
    ReleaseObject(_Viewports[I]);
  _ViewportCount := 0;


  ReleaseObject(_BucketOpaque);
  ReleaseObject(_BucketAlpha);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  ReleaseObject(_BucketReflection);
  {$ENDIF}

  ReleaseObject(_UIViewport);
  ReleaseObject(_DeviceViewport);

  ReleaseObject(_Settings);

  {$IFDEF PRECISIONTIMER}
  ReleaseObject(_Timer);
  {$ENDIF}

  SetScene(Nil);

  _GraphicsManager_Instance := Nil;
End;


Procedure GraphicsManager.Update;
Var
  I:Cardinal;
  Target:RenderTarget;
{$IFNDEF PRECISIONTIMER}
  Time:Cardinal;
{$ENDIF}
Begin
  If (_Width<=0) Or (_Height<=0) Then
    Exit;

  If (_NeedsContextRestore) Then
  Begin
    _NeedsContextRestore := False;
    Self.RestoreContext();
  End;

  {$IFNDEF DEBUG_LEAKS}
  MemoryManager.BeginFrame();
  {$ENDIF}
  
  _Stats.LightCount := LightManager.Instance.LightCount;
  _PrevStats := _Stats;
  FillChar(_Stats, SizeOf(_Stats), 0);
  _Stats.FramesPerSecond := _PrevStats.FramesPerSecond;

  //Application.Instance.SetTitle(IntToString(N));

{$IFNDEF PRECISIONTIMER}
  Time := GetTime;
  _ElapsedTime := (Time - _LastTime) / 1000.0;
  _LastTime := Time;

  If (Time - _LastSecondTime >= 1000) Then
  Begin
    _Stats.framesPerSecond := _Frames;
    _Frames := 1;
    _LastSecondTime := Time;
  End Else
    Inc(_Frames);
{$ELSE}
  If (_Timer<>Nil) Then
  Begin
    _elapsedTime := _timer.GetElapsedTime();
    _accumTimeSec := _accumTimeSec + _elapsedTime;

    if (_accumTimeSec >= 1.0) Then
    Begin
      _Stats.framesPerSecond := _Frames;
      _Frames := 1;
      _accumTimeSec := 0.0;
    End Else
      Inc(_Frames);
  End;
{$ENDIF}

  glClearColor(_BackgroundColor.R/255, _BackgroundColor.G/255, _BackgroundColor.B/255, 0{_BackgroundColor.A/255});

  If (Not _Prefetching) And (Render3D) Then
    Self.RenderScene;


// {$IFDEF PC} Render2D  := Application.Instance.Input.Keys[keyF1];{$ENDIF}

  If Render2D Then
    Self.RenderUI();

  _DeviceViewport.Bind();

  _DeviceViewport.Restore(True);

  Target := _DeviceViewport.GetRenderTarget(captureTargetColor);

  {$IFDEF IPHONE}
  If Target = Nil Then
    Exit;
  {$ENDIF}

  If Assigned(Target) Then
    Target.BeginCapture();

  For I:=0 To Pred(_ViewportCount) Do
  If (_Viewports[I].Active) And (_Viewports[I].Target = _DeviceViewport) Then
    _Viewports[I].DrawToTarget(True);

  If (Render2D) And (Self.ShowDebugTarget<=0) Then
    _UIViewport.DrawToTarget(False);

  {$IFDEF IPHONE}
  FrameBufferObject(Target).PresentToScreen();
  Target.EndCapture();
  {$ELSE}
  If Assigned(Target) Then
    Target.EndCapture();
  //_DeviceViewport.DrawFullScreen();
  {$ENDIF}

  ClearTemporaryDebug3DObjects();
End;

Procedure GraphicsManager.OnAppResize;
Var
  I:Integer;
Begin
  {$IFDEF MOBILE}
  Exit;
  {$ENDIF}

  _Width := Application.Instance.Width;
  _Height := Application.Instance.Height;

  Log(logDebug, 'GraphicsManager', 'Resizing viewports');
  {If Assigned(_DeviceViewport) Then
    _DeviceViewport.Resize(_Width, _Height);}
  OnViewportChange(0, 0, _Width, _Height);
End;

Procedure GraphicsManager.SetScene(MyScene: Scene);
Begin
  If (Self = Nil) Then
    Exit;

  If (MyScene = _Scene) Then
    Exit;

  {If Assigned(_Scene) Then
    _Scene.Release;}

  _Scene := MyScene;
End;

Procedure GraphicsManager.RenderList(RenderList:List; TranslucentPass:Boolean);
Var
  P:RenderableProxy;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering bucket'); {$ENDIF}

  P := RenderableProxy(RenderList.First);
  While (Assigned(P)) Do
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Fetching next...');{$ENDIF}
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', P.ToString());{$ENDIF}

    If (Assigned(P._Object)) Then
    Begin
      If (Not Self.ReflectionActive) Or (P._Object._Flags And renderFlagsSkipReflections=0) Then
        P._Object.Render(TranslucentPass);
    End;

    P := RenderableProxy(P.Next);
  End;
End;

Procedure GraphicsManager.AddOccluder(MyOccluder: Occluder);
Var
  Occ:Occluder;
  F:Frustum;
Begin
  If Not Assigned(MyOccluder) Then
  Begin
    Exit;
  End;

  MyOccluder.Update;
  Occ := _Occluders;
  While Assigned(Occ) Do
  If (Occ.OccluderOccluded(MyOccluder)) Then
  Begin
    Exit;
  End Else
    Occ := Occ._Next;

  If (Not ActiveViewport.Camera.Frustum.BoxVisible(MyOccluder._BoundingBox)) Then
    Exit;

  MyOccluder._Next := _Occluders;
  _Occluders := MyOccluder;

  Inc(_Stats.OccluderCount);
End;

Procedure GraphicsManager.Internal(Offset, Count:Integer);
Begin
  If (Offset=0) Then
  Begin
    Inc(_Stats.TriangleCount, Count);
    Inc(_Stats.DrawCalls);
  End;
  {If (Offset>0) And (Offset<16) Then
    Inc(PInteger(Cardinal(@(_Stats.TriangleCount)) + Offset * 4)^, Count);}
End;

Procedure GraphicsManager.SetBackgroundColor(BG: Color);
Begin
  _BackgroundColor := BG;
  If Assigned(_MainViewport) Then
    _MainViewport.BackgroundColor := BG;
End;

Function GraphicsManager.SwapScene(MyScene:Scene):Scene;
Begin
  Result := _Scene;
  _Scene := MyScene;
End;

Function GraphicsManager.GetScreenshot(): Image;
Begin
  Result := Image.Create(Width, Height);
  glReadPixels(0, 0, _Width, _Height, GL_RGBA, GL_UNSIGNED_BYTE, Result.Pixels);
  Result.Process(IMP_FlipVertical);
End;

{ Renderable }
Procedure Renderable.Release;
Begin
  GraphicsManager.Instance.DeleteRenderable(Self);
End;

Function Renderable.GetName:TERRAString;
Begin
  Result := Self.ClassName + '_'+HexStr(Cardinal(Self));
End;

Function Renderable.IsOpaque: Boolean;
Begin
  Result := True;
End;

Function Renderable.IsTranslucent: Boolean;
Begin
  Result := False;
End;

Procedure Renderable.RenderLights;
Begin
  // do nothing
End;

Procedure Renderable.Update();
Begin
  // do nothing
End;

{ RenderableProxy }
Constructor RenderableProxy.Create(Obj: Renderable);
Begin
  Self._Object := Obj;
End;

Procedure RenderableProxy.CopyValue(Other: ListObject);
Begin
  Self._Object := RenderableProxy(Other)._Object;
End;

Function RenderableProxy.Sort(Other: ListObject): Integer;
Var
  A,B:Renderable;
Begin
  A := Self._Object;
  B := RenderableProxy(Other)._Object;
  If (A=Nil) Or (B=Nil) Then
    Result := 0
  Else
  If (A._Distance<B._Distance) Then
    Result := 1
  Else
  If (A._Distance>B._Distance) Then
    Result := -1
  Else
    Result := 0;
End;

Procedure GraphicsManager.SetCurrentViewport(V: Viewport);
Begin
  _CurrentViewport := V;
End;

Function GraphicsManager.ProjectBoundingBox(Box: BoundingBox; V:Viewport): BoundingBox;
Var
  I:Integer;
  Vertices:BoundingBoxVertices;
Begin
  Box.GetVertices(Vertices);
  For I:=1 To 8 Do
  Begin
    Vertices[I] := ProjectPoint(Vertices[I], V);

    If I=1 Then
    Begin
      Result.StartVertex := Vertices[1];
      Result.EndVertex := Vertices[1];
    End Else
    Begin
      Result.StartVertex := VectorMin(Vertices[I],Result.StartVertex);
      Result.EndVertex := VectorMax(Vertices[I],Result.EndVertex);
    End;
  End;
End;

Function GraphicsManager.ProjectPoint(Pos: Vector3D; V:Viewport): Vector3D;
Var
  RX, RY:Single;
Begin
  Result := V.ProjectPoint(Pos);
  Rx := SafeDiv(Self.UIViewport.Width, V.Width);
  Ry := SafeDiv(Self.UIViewport.Height, V.Height);

  Result.X := Result.X * Rx;
  Result.Y := Result.Y * Ry;
End;

Function GraphicsManager.GetPickRay(View:Viewport; TX, TY: Integer): Ray;
Var
  RX, RY:Single;
Begin
  // convert from UI to window coords
  Rx := SafeDiv(Self.DeviceViewport.Width, Self.UIViewport.Width);
  Ry := SafeDiv(Self.DeviceViewport.Height, Self.UIViewport.Height);

  TX := Trunc(TX*Rx);
  TY := Trunc(TY*Ry);

  // convert from window coords to viewport coords
  Rx := SafeDiv(View.Width, Self.DeviceViewport.Width);
  Ry := SafeDiv(View.Height, Self.DeviceViewport.Height);

  TX := Trunc(TX*Rx);
  TY := Trunc(TY*Ry);
  Result := View.GetPickRay(TX, TY);
End;

Procedure GraphicsManager.RestoreContext;
Var
  I:Integer;
  Img:Image;
Begin
  Log(logDebug, 'GraphicsManager', 'Restoring rendering context');
  Self.ResetGLState();

  _DeviceViewport.OnContextLost();
  _UIViewport.OnContextLost();

  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].OnContextLost();
End;

Procedure GraphicsManager.OnContextLost;
Begin
  _NeedsContextRestore := True;
End;

Procedure GraphicsManager.ResetGLState;
Begin
	glEnable(GL_CULL_FACE);

  {$IFDEF PC}
	glEnable(GL_TEXTURE_2D);
  {$ENDIF}

  glEnable(GL_DEPTH_TEST);
	//glDisable(GL_FOG);
	//glEnable(GL_LIGHTING);

  //glDisable(GL_ALPHA_TEST);
  //glDisable(GL_LINE_SMOOTH);
  //glClearDepth(1.0);
  glClearStencil(0);
  glStencilMask($FFFFFFF);
  glDepthFunc(GL_LEQUAL);
End;

Function GraphicsManager.GenerateFrameBuffer: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new frame buffer...');
  Repeat
    glGenFramebuffers(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxFrameBufferHandles) Or (Not _UsedFrameBuffers[Result]);

  If (Result<MaxFrameBufferHandles) Then
    _UsedFrameBuffers[Result] := True;
End;

Function GraphicsManager.GenerateRenderBuffer: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new render buffer...');
  Repeat
    glGenRenderbuffers(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxFrameBufferHandles) Or (Not _UsedRenderBuffers[Result]);

  If (Result<MaxFrameBufferHandles) Then
    _UsedRenderBuffers[Result] := True;
End;

Function GraphicsManager.GenerateTexture: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new texture...');
  Repeat
    glGenTextures(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxTextureHandles) Or (Not _UsedTextures[Result]);

  If (Result<MaxTextureHandles) Then
    _UsedTextures[Result] := True;
End;

Procedure GraphicsManager.DeleteFrameBuffer(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteFramebuffers(1, @Handle);
  If (Handle < MaxFrameBufferHandles) Then
    _UsedFrameBuffers[Handle] := False;

  Handle := 0;
End;

Procedure GraphicsManager.DeleteRenderBuffer(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteRenderbuffers(1, @Handle);
  If (Handle < MaxFrameBufferHandles) Then
    _UsedRenderBuffers[Handle] := False;

  Handle := 0;
End;

Procedure GraphicsManager.DeleteTexture(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteTextures(1, @Handle);
  If (Handle < MaxTextureHandles) Then
    _UsedTextures[Handle] := False;

  Handle := 0;
End;

Procedure GraphicsManager.OnOrientationChange;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewportCount) Do
  If Assigned(_Viewports[I].Camera) Then
    _Viewports[I].Camera.Refresh();
End;

Procedure GraphicsManager.OnViewportChange(X1, Y1, X2, Y2: Integer);
Var
  Img:Image;
  I:Integer;
Begin
    If _DeviceViewport = Nil Then
        Exit;

  _DeviceViewport.OffsetX := X1;
  _DeviceViewport.OffsetY := (_Height - Y2);
  _DeviceViewport.Resize(X2-X1, Y2-Y1);
End;

Function GraphicsManager.GenerateStencilID:Byte;
Begin
  Inc(_StencilID);
  If (_StencilID>=128) Then
    _StencilID := 1;

  Result := _StencilID;
End;

Procedure GraphicsManager.OnSettingsChange;
Begin
  If (Settings.VertexBufferObject.Enabled) And (Not Settings.Shaders.Avaliable) Then
    Settings.VertexBufferObject.SetValue(False);
End;

Procedure GraphicsManager.TestDebugKeys;
Begin
  If (Application.Instance.Input.Keys.WasPressed(Ord('1'))) Then
    Self.ShowDebugTarget := -1;

  If (Application.Instance.Input.Keys.WasPressed(Ord('2'))) Then
    Self.ShowDebugTarget := captureTargetColor;

  If (Application.Instance.Input.Keys.WasPressed(Ord('3'))) Then
    Self.ShowDebugTarget := captureTargetNormal;

  If (Application.Instance.Input.Keys.WasPressed(Ord('4'))) Then
    Self.ShowDebugTarget := captureTargetEmission;

  If (Application.Instance.Input.Keys.WasPressed(Ord('5'))) Then
    Self.ShowDebugTarget := captureTargetRefraction;

  If (Application.Instance.Input.Keys.WasPressed(Ord('6'))) Then
    Self.ShowDebugTarget := captureTargetReflection;

  If (Application.Instance.Input.Keys.WasPressed(Ord('7'))) Then
    Self.ShowDebugTarget := captureTargetOutline;
End;

Procedure GraphicsManager.EnableReflection(Const ReflectionPoint, ReflectionNormal: Vector3D);
Begin
  {$IFDEF HAS_REFLECTIONS}
  Self._ReflectionsEnabled := True;
  Self._ReflectionPoint := ReflectionPoint;
  Self._ReflectionNormal := ReflectionNormal;
  {$ELSE}
  Self._ReflectionsEnabled := False;
  {$ENDIF}
End;

End.