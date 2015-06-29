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

{-$DEFINE TESTFULLSCREENSHADER}

{$IFDEF POSTPROCESSING}
{$IFDEF FRAMEBUFFEROBJECTS}
  {$DEFINE HAS_REFLECTIONS}
{$ENDIF}
{$ENDIF}


Interface
Uses {$IFNDEF DEBUG_LEAKS}TERRA_MemoryManager,{$ENDIF} {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Downsampler, TERRA_Renderer,
  {$IFDEF POSTPROCESSING}TERRA_ScreenFX,{$ENDIF}
  {$IFDEF SHADOWMAPS}TERRA_ShadowMaps,{$ENDIF}
  TERRA_BoundingBox, TERRA_Camera, TERRA_Color, TERRA_Matrix4x4,
  TERRA_Utils, TERRA_Texture, TERRA_Scene, TERRA_Vector3D,
  TERRA_Viewport, TERRA_Application, TERRA_VertexFormat,
  TERRA_Image, TERRA_Math, TERRA_Vector2D, TERRA_Ray, TERRA_Collections, TERRA_Pool;

Const
  //FogMode
  fogOff      = 0;
  fogDistance = 1;
  fogHeight   = 2;
  fogBox      = 4;

  renderStageDiffuse      = 1;
  renderStageNormal       = 2;
  renderStageGlow         = 4;
  renderStageRefraction   = 8;
  renderStageOutline      = 16;
  renderStageReflection   = 32;
  renderStageShadow       = 64;
//  renderStageAlpha        = 128;

  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;
  renderFlagsSkipReflections  = 4;

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

  RenderableProxy =  Class(CollectionObject)
    Protected
      _Object:Renderable;
    Public

      Constructor Create(Obj:Renderable);

      Function Sort(Other:CollectionObject):Integer; Override;
      Procedure CopyValue(Other:CollectionObject); Override;
  End;

Const
  MaxRenderables  = 4096;
  MaxLODLevel = 3;

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

	GraphicsManager = Class(ApplicationComponent)
		Protected
      _Viewports:Array Of Viewport;
      _ViewportCount:Integer;
      _CurrentViewport:Viewport;

      _DeviceViewport:Viewport;
      _UIViewport:Viewport;

      _Cameras:Array Of Camera;
      _CameraCount:Integer;

      _ReflectionCamera:Camera;

      _Width:Integer;
      _Height:Integer;
      _DepthSize:Integer;

      _UIWidth:Integer;
      _UIHeight:Integer;
      _UIScale:Single;
      
      _Scene:Scene;

      _Occluders:Occluder;

      _FullScreenQuadVertices:VertexData;

      _BucketOpaque:Pool;
      _BucketAlpha:Pool;
      {$IFDEF REFLECTIONS_WITH_STENCIL}
      _BucketReflection:Pool;
      {$ENDIF}

      _FogEnable:Boolean;
      _CurrentBlendMode:Integer;

      _WindVector:Vector3D;
      _RenderStage:Integer;
      _FrameID:Cardinal;

      _Projection:Matrix4x4;
      _OrientationMatrix4x4:Matrix4x4;

      _NeedsContextRestore:Boolean;

      _StencilID:Byte;

      _Renderer:GraphicsRenderer;

      _ReflectionsEnabled:Boolean;
      _ReflectionPoint:Vector3D;
      _ReflectionNormal:Vector3D;

      _LastTime:Cardinal;
      _LastSecondTime:Cardinal;

      _ElapsedTime:Single;

      _SimpleColor:ShaderInterface;
      _SimpleTexture:ShaderInterface;
      _SimpleTextureColored:ShaderInterface;
      _FullscreenQuadShader:ShaderInterface;
      _FullscreenColorShader:ShaderInterface;

      Procedure RenderUI;
      Procedure RenderStencilShadows(View:Viewport);
      Procedure RenderSceneInternal(View:Viewport; Pass:RenderTargetType);
      Procedure RenderViewport(View:Viewport);
      Procedure RenderList(RenderList:List; TranslucentPass:Boolean);

      Procedure OnAppResize; Override;
      Procedure OnContextLost; Override;
      Procedure OnOrientationChange; Override;
      Procedure OnViewportChange(X1, Y1, X2, Y2:Integer); Override;

      Procedure RestoreContext;

      Procedure SetRenderer(Value: GraphicsRenderer);

    Public
      ShowShadowVolumes:Boolean;
      ShowWireframe:Boolean;
      ShowDebugTarget:RenderTargetType;

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
      ToonRamp:Texture;

      Procedure Init; Override;
      Procedure Update; Override;

      Class Function Instance:GraphicsManager;

      Procedure Release; Override;

      Procedure RenderShadowmap(View:Viewport);
      Procedure RenderReflections(View:Viewport);

			Procedure RenderScene();

      Function GetDefaultFullScreenShader():ShaderInterface;

      Procedure TestDebugKeys();

      Function IsBoxVisible(Box: BoundingBox): Boolean;

	    Procedure DrawFullscreenQuad(CustomShader:ShaderInterface; X1,Y1,X2,Y2:Single);

      Function SwapScene(MyScene:Scene):Scene;
      Procedure SetScene(MyScene:Scene);
      Procedure SetWind(WindDirection:Vector3D; WindIntensity:Single);

      Function GetPickRay(View:Viewport; TX,TY:Integer):Ray;

      Function ProjectPoint(Pos:Vector3D; V:Viewport):Vector3D;
      Function ProjectBoundingBox(Box:BoundingBox; V:Viewport):BoundingBox;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;


      Function AddRenderable(MyRenderable:Renderable; Flags:Cardinal = 0):Boolean;
      //Procedure AttachRenderable(MyRenderable, Owner:Renderable);
      Procedure DeleteRenderable(MyRenderable:Renderable);

      Procedure AddOccluder(MyOccluder:Occluder);

      Procedure SetFog(Value:Boolean);

      Procedure AddViewport(V:Viewport);
      Procedure DeleteViewport(V:Viewport);
      Function GetViewport(Index:Integer):Viewport;
      Procedure SetCurrentViewport(V:Viewport);

      Function GenerateStencilID():Byte; 

      Function EnableColorShader(Const MyColor:Color; Const Transform:Matrix4x4):ShaderInterface;
      Function EnableTextureShader(Const MyColor:Color; Tex:Texture; Const Transform:Matrix4x4):ShaderInterface;
      Function EnableColoredTextureShader(Tex:Texture; Const Transform:Matrix4x4):ShaderInterface;

      Procedure EnableReflection(Const ReflectionPoint, ReflectionNormal:Vector3D);

      Property ElapsedTime:Single Read _elapsedTime;

      Property ViewportCount:Integer Read _ViewportCount;

      Procedure AddCamera(Cam:Camera);
      Procedure DeleteCamera(Cam:Camera);
      Function GetCamera(Index:Integer):Camera;

      Class Function IsShuttingDown:Boolean;

      Property CameraCount:Integer Read _CameraCount;

      Property Renderer:GraphicsRenderer Read _Renderer Write SetRenderer;

      Property ActiveViewport:Viewport Read _CurrentViewport Write SetCurrentViewport;
      Property DeviceViewport:Viewport Read _DeviceViewport;
      Property UIViewport:Viewport Read _UIViewport;

      Property Scene:TERRA_Scene.Scene Read _Scene Write SetScene;

      Property WindVector:Vector3D Read _WindVector;

      Property FrameID:Cardinal Read _FrameID;
      Property RenderStage:Integer Read _RenderStage;

      Property ProjectionMatrix:Matrix4x4 Read _Projection;

      Property UI_Width:Integer Read _UIWidth;
      Property UI_Height:Integer Read _UIHeight;
      Property UI_Scale:Single Read _UIScale;
	End;

Implementation

Uses TERRA_Error, TERRA_OS, TERRA_Log, TERRA_UI, TERRA_ResourceManager, TERRA_InputManager,
  TERRA_Frustum, TERRA_Lights, TERRA_SpriteManager, TERRA_Mesh,
  TERRA_Decals, TERRA_Billboards, TERRA_ParticleRenderer, TERRA_DebugDraw;

Var
  _GraphicsManager_Instance:ApplicationObject = Nil;
  _ShuttingDown:Boolean = False;

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
    ReleaseObject(Image)
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
{$IFDEF PC_X}
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

{ GraphicsManager }
Class Function GraphicsManager.Instance:GraphicsManager;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (Not Assigned(_GraphicsManager_Instance)) Then
    _GraphicsManager_Instance := InitializeApplicationComponent(GraphicsManager, Nil);

  Result := GraphicsManager(_GraphicsManager_Instance.Instance);
End;

Procedure GraphicsManager.Init;
Var
  OW, OH:Integer;
  S:TERRAString;
  RendererID:Integer;
Begin
  Log(logDebug, 'GraphicsManager', 'Initializing');

  _CurrentViewport := Nil;
  _DeviceViewport := Nil;
  _UIViewport := Nil;
  _DepthSize := 2048;

  _BucketOpaque := Pool.Create(collection_Sorted_Ascending);
  _BucketAlpha :=  Pool.Create(collection_Sorted_Descending);

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  _BucketReflection := Pool.Create(coAppend);
  {$ENDIF}

  If Application.Instance = Nil Then
    Exit;

  _Width := Application.Instance.Width;
  _Height := Application.Instance.Height;

  _FogEnable := False;

  RendererID := Application.Instance.SelectRenderer();
  If (RendererID<0) Or (RendererID>=Renderers.Count) Then
    RendererID := 0;

  SetRenderer(GraphicsRenderer(Renderers.GetItemByIndex(RendererID)));

  If Self.Renderer = Nil Then
  Begin
    RaiseError('Failed to initialized renderer with ID '+IntToString(RendererID));
    Exit;
  End;

  Log(logDebug, 'GraphicsManager', 'Width='+IntToString(_Width)+' Height='+IntToString(_Height));

  Application.Instance.SetViewport(0,0,_Width,_Height);

  _UIWidth := _Width;
  _UIHeight := _Height;
  _UIScale := 1.0;
  Application.Instance.SelectResolution2D(_UIWidth, _UIHeight, _UIScale);
  Log(logDebug, 'App', 'Selected UI resolution: '+IntToString(_UIWidth)+' x ' +IntToString(_UIHeight));
   Log(logDebug, 'App', 'Selected UI scale: '+FloatToString(_UIScale));

  Render3D := True;
  Render2D := True;

  ShowDebugTarget := captureTargetInvalid;

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



  Log(logDebug, 'GraphicsManager', 'Device resolution: '+IntToString(_Width)+' x ' +IntToString(_Height));

  _DeviceViewport := Viewport.Create('device', _Width, _Height);

  OW := _Width;
  OH := _Height;

  If Assigned(Application.Instance()) Then
  Begin
    Application.Instance.SelectResolution3D(OW,OH);
  End;

  {If (Self.LandscapeOrientation) Then
  Begin
    Temp := OW;
    OW := OH;
    OH := Temp;
  End;}

  Log(logDebug, 'GraphicsManager', 'Selected 3D resolution: '+IntToString(OW)+' x ' +IntToString(OH));

  // make UI view
  _UIViewport := Viewport.Create('UI', Self.UI_Width, Self.UI_Height, {$IFDEF FRAMEBUFFEROBJECTS}Self.UI_Scale{$ELSE}1.0{$ENDIF});
  _UIViewport.SetRenderTargetState(captureTargetColor, True);
  _UIViewport.SetTarget(_DeviceViewport, 0, 0, 1.0, 1.0);

  ShowWireframe := False;

  Self.ReflectionMatrixSky := Matrix4x4Identity;
  Self.ReflectionMatrix := Matrix4x4Identity;
 
End;

Function GraphicsManager.GetDefaultFullScreenShader():ShaderInterface;
Begin
  If (_FullscreenQuadShader = Nil) Then
  Begin
    _FullscreenQuadShader := GraphicsManager.Instance.Renderer.CreateShader();
    _FullscreenQuadShader.Generate('fullscreen_quad', GetShader_FullscreenQuad());
  End;

  Result := _FullscreenQuadShader;
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

  ReleaseObject(_Viewports[N]);
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

  ReleaseObject(_Cameras[N]);
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
  GraphicsManager.Instance.Renderer.SetBlendMode(blendNone);

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
  Target:RenderTargetInterface;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginUIRendering');{$ENDIF}

  Self.ActiveViewport := _UIViewport;
  _UIViewport.BackgroundColor := ColorNull;
  Target := _UIViewport.GetRenderTarget(captureTargetColor);
  If Assigned(Target) Then
  Begin
    Target.BackgroundColor := _UIViewport.BackgroundColor;
    Target.BeginCapture();

    {$IFDEF PC_X}
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
    {$ENDIF}


    Self.Renderer.SetBlendMode(blendBlend);
    Self.SetFog(False);
    //glEnable(GL_SCISSOR_TEST);

    _Projection := Matrix4x4Ortho(0.0, _UIViewport.Width, _UIViewport.Height, 0.0, -100, 100);
    _Projection := Matrix4x4Multiply4x4(_Projection, Matrix4x4Translation(0.375, 0.375, 0.0));

    _UIViewport.SetViewArea(0, 0, _UIViewport.Width, _UIViewport.Height);

    Renderer.ClearBuffer((Not Assigned(_Scene)) Or (_ViewportCount<=0), True, True);

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

  Self.ReflectionMask := View.GetRenderTexture(captureTargetReflection);

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

  Renderer.SetCullMode(cullFront);

  Self.ReflectionActive := True;
  Self.RenderList(_BucketOpaque, False);
  Self.RenderList(_BucketAlpha, True);
  Self.ReflectionActive := False;

  Renderer.SetCullMode(cullBack);

  ActiveViewport.Camera.RemoveClipPlane();

  Self.ReflectionMatrixSky := Matrix4x4Identity;
  Self.ReflectionMatrix := Matrix4x4Identity;
End;

{$ELSE}
Procedure GraphicsManager.RenderReflections(View:Viewport);
Var
  StencilID:Byte;
  P:CollectionObject;
  Obj:Renderable;
  Normal:Vector3D;
//  _Plane:Plane;
Begin
  If (Not Assigned(_Scene)) Then
    Exit;

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
    Renderer.SetStencilTest(True);
    Renderer.SetStencilFunction(compareAlways, StencilID);
    Renderer.SetStencilOp(stencilReplace, stencilReplace, stencilReplace);
    Renderer.SetColorMask(False, False, False, False);
    {$ENDIF}

    Renderer.SetDepthMask(False);

    Self.ReflectionTemp := True;
    Self.ReflectionStencil := True;

    If (Obj.IsOpaque) Then
      Obj.Render(False);

    If (Obj.IsTranslucent) Then
      Obj.Render(True);

    Self.ReflectionTemp := False;
    Self.ReflectionStencil := False;

    _renderStage := renderStageDiffuse;

    Renderer.SetDepthMask(True);
    Renderer.SetColorMask(True, True, True, True);

    {$IFNDEF REFLECTIONS_WITH_ALPHA}
    Renderer.SetStencilFunction(compareEqual, StencilID);
    Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilKeep);
    {$ENDIF}

    ActiveViewport.Camera.SetClipPlane(Obj.ReflectionPoint, Obj.ReflectionNormal);

    If (View.DrawSky) And (Not View.Camera.Ortho) Then
       _Scene.RenderSky(View);

    Renderer.SetCullMode(cullFront);
    Self.ReflectionActive := True;
    Self.RenderList(_BucketOpaque, False);
    Self.RenderList(_BucketAlpha, True);
    Self.ReflectionActive := False;
    Renderer.SetCullMode(cullBack);

    ActiveViewport.Camera.RemoveClipPlane();

    {$IFNDEF REFLECTIONS_WITH_ALPHA}
    Renderer.SetStencilTest(False);
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
End;
{$ENDIF}

Var
  _StencilVolumeShader:ShaderInterface;
  _StencilShadowShader:ShaderInterface;

Const
  StencilQuad:Array[0..11] Of Single = (1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0);

Procedure GraphicsManager.RenderStencilShadows(View:Viewport);
Var
  StencilID:Byte;
Begin
  If (_FullscreenColorShader = Nil) Then
  Begin
    _FullscreenColorShader := Self.Renderer.CreateShader();
    _FullscreenColorShader.Generate('fullscreen_color', GetShader_FullscreenColor());
  End;

  StencilID := 64;

  Self.Renderer.SetBlendMode(blendNone);

  Self.Renderer.BindShader(_FullscreenColorShader);
  _FullscreenColorShader.SetColorUniform('color', ColorWhite); //BIBI

  Renderer.SetStencilTest(True);
  Renderer.SetStencilFunction(compareAlways, StencilID);
  Renderer.SetStencilOp(stencilReplace, stencilReplace, stencilReplace);
  Renderer.SetColorMask(False, False, False, False);

  Self.DrawFullscreenQuad(_FullscreenColorShader, 0, 0, 1, 1);

  TextureManager.Instance.WhiteTexture.Bind(0);

  // Disable z-buffer writes (note: z-testing still occurs), and enable the stencil-buffer
  Renderer.SetDepthMask(False);
  Renderer.SetDepthTest(True);

  If ShowShadowVolumes Then
  Begin
    GraphicsManager.Instance.Renderer.SetBlendMode(blendJoin);
    Renderer.SetColorMask(True, True, True, True);
  End Else
    Renderer.SetColorMask(False, False, False, False);

  If (_StencilVolumeShader = Nil) Then
  Begin
    _StencilVolumeShader := Self.Renderer.CreateShader();
    _StencilVolumeShader.Generate('stencilvolumes', GetShader_StencilVolumeShader()); 
  End;

  Log(logDebug, 'Shadow', 'Drawing stencil shadows');

  Self.Renderer.BindShader(_StencilVolumeShader);
  _StencilVolumeShader.SetMat4Uniform('cameraMatrix', Self.ActiveViewport.Camera.Transform); // BIBI
  _StencilVolumeShader.SetMat4Uniform('projectionMatrix', Self.ActiveViewport.Camera.Projection); // BIBI

  _RenderStage := renderStageShadow;

  Renderer.SetCullMode(cullBack);

  // Set up stencil compare fuction, reference value, and masks.
  // Stencil test passes if ((ref  and mask) cmpfn (stencil  and mask)) is true.
  Renderer.SetStencilFunction(compareAlways, 0);
  Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilIncrementWithWrap);

  _Scene.RenderShadowCasters(View);

  If Not ShowShadowVolumes Then
  Begin
    Renderer.SetCullMode(cullFront);
    Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilDecrementWithWrap);
    _Scene.RenderShadowCasters(View);

    Renderer.SetCullMode(cullBack);

    Renderer.SetBlendMode(blendBlend);

    If (_FullscreenColorShader = Nil) Then
    Begin
      _FullscreenColorShader := Self.Renderer.CreateShader();
      _FullscreenColorShader.Generate('fullscreen_color', GetShader_FullscreenColor());
    End;

    Self.Renderer.BindShader(_FullscreenColorShader);
    _FullscreenColorShader.SetColorUniform('color', ColorCreate(Byte(0), Byte(0), Byte(0), Byte(55))); // BIBI

    // Only write where stencil val >= 1 (count indicates # of shadows that overlap that pixel)
    Renderer.SetStencilFunction(compareEqual, Succ(StencilID));

    Renderer.SetColorMask(True, True, True, True);

    // Draws a big gray polygon over scene according to the mask in the stencil buffer. (Any pixel with stencil==1 is in the shadow.)
    Self.DrawFullscreenQuad(_FullscreenColorShader, 0, 0, 1, 1);

    Renderer.SetBlendMode(blendBlend);
  End;

  _RenderStage := renderStageDiffuse;

  Renderer.SetBlendMode(blendNone);

  //Restore render states
  Renderer.SetStencilFunction(compareAlways, 0);
  Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilKeep);
  Renderer.SetStencilTest(False);
  Renderer.SetDepthMask(True);
End;

Procedure GraphicsManager.SetWind(WindDirection:Vector3D; WindIntensity:Single);
Begin
  _WindVector := WindDirection;
  _WindVector.Normalize;
  _WindVector.Scale(WindIntensity);
End;

Procedure GraphicsManager.RenderSceneInternal(View:Viewport; Pass:RenderTargetType);
Var
  N:Integer;
Begin
  {$IFDEF POSTPROCESSING}
  If (Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Or (Not View.HasPostProcessing()) Then
    N := renderStageDiffuse
  Else
  Case Pass Of
    captureTargetColor:   N := renderStageDiffuse;
    captureTargetNormal:  N := renderStageNormal;
    captureTargetEmission: N := renderStageGlow;
    captureTargetRefraction: N := renderStageRefraction;
    captureTargetReflection: N := renderStageReflection;
    captureTargetShadow: N := renderStageShadow;
    captureTargetOutline: N := renderStageOutline;
    captureTargetAlpha: N := renderStageDiffuse;
    Else
      Exit;
  End;
  {$ELSE}
  N := renderStageDiffuse;
  {$ENDIF}

  If (N = renderStageReflection) And (Not Self._ReflectionsEnabled) Then
    Exit;

  If (N = renderStageShadow) Then
  Begin
    View.SetRenderTargetState(Pass, True);
  End;

  _RenderStage := N;

  If (Assigned(_Scene)) And (View.DrawSky) And (Not View.Camera.Ortho) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderSky');{$ENDIF}

    {$IFDEF POSTPROCESSING}
    If Pass = captureTargetRefraction Then
    Begin
      //IntToString(2);
    End Else
    If Pass = captureTargetEmission Then
      _Scene.RenderSkyEmission(View)
    Else
    {$ENDIF}
      _Scene.RenderSky(View);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderOpaqueBucket');{$ENDIF}

  {$IFNDEF DEBUG_REFLECTIONS}

{$IFDEF ADVANCED_ALPHA_BLEND}
  If Pass = captureTargetAlpha Then
  Begin
    RenderList(_BucketOpaque, False);
    RenderList(_BucketAlpha, True)
  End Else
  Begin
    RenderList(_BucketOpaque, False);

    If (_RenderStage <> renderStageNormal) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderDecals');{$ENDIF}
      DecalManager.Instance.Render();

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderBillboards');{$ENDIF}
      BillboardManager.Instance.Render();
    End;
  End;
{$ELSE}
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
{$ENDIF}

  If (_ReflectionsEnabled) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderReflections');{$ENDIF}
    Self.RenderReflections(View);
  End;

(*    If (Renderer.Features.Shaders.Avaliable) And (Renderer.Settings.DynamicShadows.Enabled) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderStencilShadows');{$ENDIF}
      RenderStencilShadows(View);
    End;*)
End;

Procedure GraphicsManager.RenderViewport(View:Viewport);
Var
  I, J, Count, SubViews:Integer;
  Target:RenderTargetInterface;
Begin
  If Not View.Active Then
    Exit;

  SetCurrentViewport(View);

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


  If (View.VR) Then
    SubViews := 1
  Else
    SubViews := 0;

  View.SetRenderTargetState(captureTargetShadow, Self.Renderer.Settings.DynamicShadows.Enabled);

  Count := 0;
  For J:=0 To SubViews Do
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'View.Bind');{$ENDIF}
    View.Bind(J);

    For I := Pred(RenderCaptureTargets) DownTo 0 Do
    Begin
      Target := View.GetRenderTarget(RenderTargetType(I));
      If (Target = Nil) Then
        Continue;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering viewport: '+View.Name+', target '+IntToString(I)+', width:'+IntToString(Target.Width)+', height:'+IntToString(Target.Height));{$ENDIF}

      Case RenderTargetType(I) Of
        captureTargetRefraction:
          Target.BackgroundColor := ColorNull;

        captureTargetAlpha:
          Target.BackgroundColor := ColorNull;

        captureTargetReflection:
          Target.BackgroundColor := ColorBlack;

        captureTargetColor:
          Target.BackgroundColor := View.BackgroundColor;

        captureTargetShadow:
          Target.BackgroundColor := ColorRed;

        captureTargetOutline:
          Target.BackgroundColor := ColorBlack;
          //Target.ClearColor := ColorRed;
      Else
        Target.BackgroundColor := ColorBlack;
      End;

      Target.BeginCapture();
      Self.RenderSceneInternal(View, RenderTargetType(I));
      Target.EndCapture();

      Inc(Count);

      {$IFDEF PC}
      (*
      If (_RenderStage = renderStageDiffuse) And (InputManager.Instance.Keys.WasPressed(KeyH)) Then
        Target.GetImage.Save('frame.png');*)
      
      {If (_RenderStage = renderStageGlow) And (Application.Instance.Input.Keys.WasPressed(Ord('M'))) Then
        Target.Save('bloom.png');
       If (_RenderStage = renderStageRefraction) And (Application.Instance.Input.Keys.WasPressed(Ord('N'))) Then
        Target.Save('refraction.png');}
      {$ENDIF}
    End;
  End;

  If (Count<=0) Then
    Log(logWarning, 'GraphicsManager', 'Invalid viewport: '+View.Name);
End;

Procedure GraphicsManager.RenderScene;
Var
  I:Integer;
Begin
  If Not Assigned(_Scene) Then
    Exit;

  //glAlphaFunc(GL_GREATER, 0.1);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginSceneRendering '+IntToString(_ViewportCount));{$ENDIF}

  If (Not Application.Instance.HasFatalError) Then
  For I:=Pred(_ViewportCount) DownTo 0 Do
    RenderViewport(_Viewports[I]);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'EndSceneRendering');{$ENDIF}
End;

Procedure InitFullScreenQuad(FullscreenQuad:VertexData; X1,Y1,X2,Y2:Single; Orientation:Integer);
Begin
  FullscreenQuad.SetVector3D(0, vertexPosition, VectorCreate(X1, Y2, 0.0));
  FullscreenQuad.SetVector3D(1, vertexPosition, VectorCreate(X2, Y2, 0.0));
  FullscreenQuad.SetVector3D(2, vertexPosition, VectorCreate(X2, Y1, 0.0));
  FullscreenQuad.SetVector3D(3, vertexPosition, VectorCreate(X2, Y1, 0.0));
  FullscreenQuad.SetVector3D(4, vertexPosition, VectorCreate(X1, Y1, 0.0));
  FullscreenQuad.SetVector3D(5, vertexPosition, VectorCreate(X1, Y2, 0.0));

  Case Orientation Of
    orientationLandscapeLeft:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, VectorCreate2D(1.0, 0.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, VectorCreate2D(1.0, 1.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, VectorCreate2D(0.0, 1.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, VectorCreate2D(0.0, 1.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, VectorCreate2D(0.0, 0.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, VectorCreate2D(1.0, 0.0));
    End;

    orientationLandscapeRight:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, VectorCreate2D(0.0, 1.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, VectorCreate2D(0.0, 0.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, VectorCreate2D(1.0, 0.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, VectorCreate2D(1.0, 0.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, VectorCreate2D(1.0, 1.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, VectorCreate2D(0.0, 1.0));
    End;

    orientationPortrait:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, VectorCreate2D(0.0, 0.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, VectorCreate2D(1.0, 0.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, VectorCreate2D(1.0, 1.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, VectorCreate2D(1.0, 1.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, VectorCreate2D(0.0, 1.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, VectorCreate2D(0.0, 0.0));
    End;

    orientationPortraitInverted:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, VectorCreate2D(1.0, 1.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, VectorCreate2D(0.0, 1.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, VectorCreate2D(0.0, 0.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, VectorCreate2D(0.0, 0.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, VectorCreate2D(1.0, 0.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, VectorCreate2D(1.0, 1.0));
    End;
  End;
End;

Procedure GraphicsManager.DrawFullscreenQuad(CustomShader:ShaderInterface; X1,Y1,X2,Y2:Single);
Var
  M,Projection:Matrix4x4;
  I:Integer;
  PositionHandle, UVHandle:Integer;
  //Delta:Single;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'DrawFullScreenQuad: ('+FloatToString(X1)+','+FloatToString(Y1)+','+FloatToString(X2)+','+FloatToString(Y2)+')');{$ENDIF}

  {$IFDEF TESTFULLSCREENSHADER}
  CustomShader := Nil;
  {$ENDIF}

  If (CustomShader = Nil) And (Renderer.Features.Shaders.Avaliable) Then
  Begin
    CustomShader := GetDefaultFullScreenShader();
    Self.Renderer.BindShader(CustomShader);
  End;

  Y1 := 1.0 - Y1;
  Y2 := 1.0 - Y2;

  If _FullScreenQuadVertices = Nil Then
    _FullScreenQuadVertices := VertexData.Create([vertexFormatPosition, vertexFormatUV0], 6);

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
    InitFullScreenQuad(_FullScreenQuadVertices, X1,Y1,X2,Y2, Application.Instance.Orientation);

  Projection := Matrix4x4Ortho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  Renderer.SetCullMode(cullNone);
  Renderer.SetDepthTest(False);
  Renderer.SetDepthMask(False);

  Renderer.SetProjectionMatrix(Projection);
  Renderer.SetModelMatrix(Matrix4x4Identity);
  Renderer.SetTextureMatrix(Matrix4x4Identity);
  Renderer.SetDiffuseColor(ColorWhite);


{  Renderer.SetSourceVertexSize(16);
  Renderer.SetAttributeSource('terra_position', typeVector3D, @(FullscreenQuad[0]));
  Renderer.SetAttributeSource('terra_UV0', typeVector2D, @(FullscreenQuad[2]));}

  Renderer.SetVertexSource(_FullScreenQuadVertices);
  Renderer.DrawSource(renderTriangles, 6);

  (*If (Not Renderer.Features.Shaders.Avaliable) Then
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
    End;*)

    (*glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);

    glVertexPointer(3, GL_FLOAT, 16, @(FullscreenQuad[0]));
    glTexCoordPointer(2, GL_FLOAT, 16, @(FullscreenQuad[2]));


  If (Not Settings.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;*)

  Renderer.SetDepthTest(True);
  Renderer.SetDepthMask(True);
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
  Proxy:CollectionObject;
Begin
  Proxy := Target.Recycle();
  If Assigned(Proxy) Then
  Begin
    RenderableProxy(Proxy)._Object := MyRenderable;
  End Else
    Proxy := RenderableProxy.Create(MyRenderable);

  Target.Add(Proxy);
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

  If (Renderer.Settings.DynamicLights.Enabled) Then
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

  Renderer.InternalStat(statRenderables);
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

Function GraphicsManager.EnableColorShader(Const MyColor:Color; Const Transform:Matrix4x4):ShaderInterface;
Begin
  If (Not Assigned(_SimpleColor)) Then
  Begin
    _SimpleColor := Self.Renderer.CreateShader();
    _SimpleColor.Generate('simple_color', GetShader_SimpleColor()); 
  End;

  Result := _SimpleColor;
  If (Not Assigned(Result)) Then
    Exit;

  Self.Renderer.BindShader(Result);
  Result.SetMat4Uniform('cameraMatrix', ActiveViewport.Camera.Transform); //BIBI
  Result.SetMat4Uniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetMat4Uniform('modelMatrix', Transform);
  Result.SetColorUniform('out_color', MyColor); //BIBI
End;

Function GraphicsManager.EnableTextureShader(Const MyColor:Color; Tex:Texture; Const Transform:Matrix4x4):ShaderInterface;
Begin
  If (Not Assigned(_SimpleTexture)) Then
  Begin
    _SimpleTexture := Self.Renderer.CreateShader();
    _SimpleTexture.Generate('simple_texture', GetShader_SimpleTexture()); 
  End;

  Tex.Bind(0);

  Result := _SimpleTexture;
  If (Not Assigned(Result)) Then
    Exit;

  Self.Renderer.BindShader(Result);
  Result.SetMat4Uniform('cameraMatrix', ActiveViewport.Camera.Transform); //BIBI
  Result.SetMat4Uniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetMat4Uniform('modelMatrix', Transform);
  Result.SetColorUniform('out_color', MyColor);
  Result.SetIntegerUniform('out_texture', 0);
End;

Function GraphicsManager.EnableColoredTextureShader(Tex:Texture; Const Transform:Matrix4x4):ShaderInterface;
Begin
  If (Not Assigned(_SimpleTextureColored)) Then
  Begin
    _SimpleTextureColored := Self.Renderer.CreateShader();
    _SimpleTextureColored.Generate('colored_texture', GetShader_ColoredTexture());
  End;

  Tex.Bind(0);

  Result := _SimpleTextureColored;
  If (Not Assigned(Result)) Then
    Exit;

  Self.Renderer.BindShader(Result);
  Result.SetMat4Uniform('cameraMatrix', ActiveViewport.Camera.Transform); //BIBI
  Result.SetMat4Uniform('projectionMatrix', ActiveViewport.Camera.Projection);
  Result.SetMat4Uniform('modelMatrix', Transform);
  Result.SetIntegerUniform('out_texture', 0);
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

  ReleaseObject(_FullScreenQuadVertices);

  ReleaseObject(_SimpleColor);
  ReleaseObject(_SimpleTexture);
  ReleaseObject(_SimpleTextureColored);
  ReleaseObject(_FullscreenQuadShader);
  ReleaseObject(_FullscreenColorShader);

  ReleaseObject(_BucketOpaque);
  ReleaseObject(_BucketAlpha);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  ReleaseObject(_BucketReflection);
  {$ENDIF}

  ReleaseObject(_UIViewport);
  ReleaseObject(_DeviceViewport);

  SetScene(Nil);

  _GraphicsManager_Instance := Nil;
End;


Procedure GraphicsManager.Update;
Var
  I:Integer;
  Target:RenderTargetInterface;
  Time:Cardinal;
  UpdateFPS:Boolean;
Begin
  If (_Width<=0) Or (_Height<=0) Then
    Exit;

  If (_NeedsContextRestore) Then
  Begin
    _NeedsContextRestore := False;
    Self.RestoreContext();
  End;

  Time := Application.GetTime();
  _ElapsedTime := (Time - _LastTime) / 1000.0;
  _LastTime := Time;

  UpdateFPS := (Time - _LastSecondTime >= 1000);
  If (UpdateFPS) Then
    _LastSecondTime := Time;

  {$IFNDEF DEBUG_LEAKS}
  MemoryManager.BeginFrame();
  {$ENDIF}

  Renderer.BeginFrame();

  If Renderer.Settings.Changed Then
    Renderer.OnSettingsChange();

  Inc(_FrameID);

  If (Not _Prefetching) And (Render3D) Then
    Self.RenderScene;


  // resolve offscreen buffers
  For I:=Pred(_ViewportCount) DownTo 0 Do
  If (_Viewports[I].Active) And (_Viewports[I].AutoResolve) Then
  Begin
    _Viewports[I].DrawToTarget(True, True);
  End;

// {$IFDEF PC} Render2D  := Application.Instance.Input.Keys[keyF1];{$ENDIF}

  If Render2D Then
    Self.RenderUI();

  _DeviceViewport.Bind(0);
  _DeviceViewport.Restore(True);


   Target := _DeviceViewport.GetRenderTarget(captureTargetColor);

 If Assigned(Target) Then
    Target.BeginCapture();

  For I:=0 To Pred(_ViewportCount) Do
  If (_Viewports[I].Active) And (Not _Viewports[I].AutoResolve) Then
  Begin
    If (_Viewports[I].Target = _DeviceViewport) Then
      _Viewports[I].DrawToTarget(True, True)
    Else
    If (_Viewports[I].Target = Nil) Then
      _Viewports[I].DrawToTarget(True, True);
  End;

  If (Render2D) And (Integer(Self.ShowDebugTarget) <=0) Then
    _UIViewport.DrawToTarget(False, False);

  If Assigned(Target) Then
    Target.EndCapture();
 
  ClearTemporaryDebug3DObjects();

  Renderer.EndFrame();
  If UpdateFPS Then
    Renderer.UpdateFrameCounter();
End;

Procedure GraphicsManager.OnAppResize;
Var
  I:Integer;
  NewW, NewH:Integer;
  UIW, UIH:Integer;
Begin
  {$IFDEF MOBILE}
  Exit;
  {$ENDIF}

  _Width := Application.Instance.Width;
  _Height := Application.Instance.Height;

  _Renderer.Resize(_Width, _Height);

  NewW := Trunc(_Width);
  NewH :=  Trunc(_Height);

  Application.Instance.SelectResolution2D(NewW, NewH, _UIScale);
  If (NewW<>_UIWidth) Or (NewH<>_UIHeight) Then
  Begin
    _UIWidth := NewW;
    _UIHeight := NewH;
  End;

  Log(logDebug, 'GraphicsManager', 'Resizing viewports');
  {If Assigned(_DeviceViewport) Then
    _DeviceViewport.Resize(_Width, _Height);}
  OnViewportChange(0, 0, _Width, _Height);

  UIW := Self.UI_Width;
  UIH := Self.UI_Height;
  If (UIViewport.Width<>UIW) Or (UIViewport.Height<>UIH) Then
    UIViewport.Resize(UIW, UIH);
End;

Procedure GraphicsManager.SetScene(MyScene: Scene);
Begin
  If (Self = Nil) Then
    Exit;

  If (MyScene = _Scene) Then
    Exit;

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

  Renderer.InternalStat(statOccluders);
End;

Function GraphicsManager.SwapScene(MyScene:Scene):Scene;
Begin
  Result := _Scene;
  _Scene := MyScene;
End;

Procedure GraphicsManager.SetRenderer(Value: GraphicsRenderer);
Begin
  ReleaseObject(_Renderer);

  _Renderer := Value;
  If _Renderer = Nil Then
    Exit;

  Log(logDebug, 'GraphicsManager', 'Initializing Renderer: '+_Renderer.Name);
  _Renderer.Reset();
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

Procedure RenderableProxy.CopyValue(Other: CollectionObject);
Begin
  Self._Object := RenderableProxy(Other)._Object;
End;

Function RenderableProxy.Sort(Other: CollectionObject): Integer;
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

{Procedure GraphicsManager.SetViewArea(X, Y, Width, Height: Integer);
Begin
  _ViewX := X;
  _ViewY := Y;
  _ViewWidth := Width;
  _ViewHeight := Height;

  Renderer.SetViewport(X,Y, Width, Height);
End;}

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
  If Renderer = Nil Then
    Exit;
    
  Log(logDebug, 'GraphicsManager', 'Restoring rendering context');
  Renderer.OnContextLost();

  _DeviceViewport.OnContextLost();
  _UIViewport.OnContextLost();

  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].OnContextLost();
End;

Procedure GraphicsManager.OnContextLost;
Begin
  _NeedsContextRestore := True;
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

Procedure GraphicsManager.TestDebugKeys;
Var
  Input:InputManager;
Begin
  Input := InputManager.Instance;

  If (Input.Keys.IsDown(KeyShift)) Then
  Begin
    If (Input.Keys.WasPressed(keyF1)) Then
      Self.ShowDebugTarget := effectTargetGlow;

    If (Input.Keys.WasPressed(keyF2)) Then
      Self.ShowDebugTarget := effectTargetBloom;

    If (Input.Keys.WasPressed(keyF3)) Then
      Self.ShowDebugTarget := effectTargetEdge;

    If (Input.Keys.WasPressed(keyF4)) Then
      Self.ShowDebugTarget := captureTargetAlpha;
  End Else
  Begin
    If (Input.Keys.WasPressed(keyF1)) Then
      Self.ShowDebugTarget := captureTargetInvalid;

    If (Input.Keys.WasPressed(keyF2)) Then
      Self.ShowDebugTarget := captureTargetColor;

    If (Input.Keys.WasPressed(keyF3)) Then
      Self.ShowDebugTarget := captureTargetNormal;

    If (Input.Keys.WasPressed(keyF4)) Then
      Self.ShowDebugTarget := captureTargetEmission;

    If (Input.Keys.WasPressed(keyF5)) Then
      Self.ShowDebugTarget := captureTargetRefraction;

    If (Input.Keys.WasPressed(keyF6)) Then
      Self.ShowDebugTarget := captureTargetReflection;

    If (Input.Keys.WasPressed(keyF7)) Then
      Self.ShowDebugTarget := captureTargetOutline;

    If (Input.Keys.WasPressed(keyF8)) Then
      Self.ShowDebugTarget := captureTargetShadow;

    If (Input.Keys.WasPressed(keyF9)) Then
      Self.ShowDebugTarget := captureTargetPosition;
  End;
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
