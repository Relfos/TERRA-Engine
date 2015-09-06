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

(*
TEXTURE_CUBE_MAP_SEAMLESS
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

*)


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
  TERRA_String, TERRA_Object, TERRA_Downsampler, TERRA_Renderer,
  {$IFDEF POSTPROCESSING}TERRA_ScreenFX,{$ENDIF}
  {$IFDEF SHADOWMAPS}TERRA_ShadowMaps,{$ENDIF}
  TERRA_BoundingBox, TERRA_Camera, TERRA_Color, TERRA_Matrix4x4,
  TERRA_Utils, TERRA_Texture, TERRA_Vector3D,
  TERRA_Viewport, TERRA_Application, TERRA_VertexFormat, TERRA_Renderable,
  TERRA_Image, TERRA_Math, TERRA_Vector2D, TERRA_Ray, TERRA_Collections;

Type
	GraphicsManager = Class(TERRAObject)
		Protected
      _Viewports:Array Of TERRAViewport;
      _ViewportCount:Integer;
      _CurrentViewport:TERRAViewport;

      _DeviceViewport:TERRAViewport;

      _ReflectionCamera:TERRACamera;

      _FullScreenQuadVertices:VertexData;

      _FogEnable:Boolean;
      _CurrentBlendMode:Integer;

      _WindVector:Vector3D;
      _FrameID:Cardinal;

      _OrientationMatrix4x4:Matrix4x4;

      _StencilID:Byte;

      _Renderer:GraphicsRenderer;

      _ReflectionsEnabled:Boolean;
      _ReflectionPoint:Vector3D;
      _ReflectionNormal:Vector3D;

      _LastTime:Cardinal;
      _LastSecondTime:Cardinal;

      _ElapsedTime:Single;

      _FullscreenQuadShader:ShaderInterface;
      _FullscreenColorShader:ShaderInterface;

      _Renderables:RenderableManager;

      Procedure RenderStencilShadows(View:TERRAViewport);
      Procedure RenderViewport(View:TERRAViewport);

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
      ReflectionMask:TERRATexture;
      {$ENDIF}

      EnviromentMap:TERRATexture;
      ToonRamp:TERRATexture;

      Constructor Create; 
      Procedure Release; Override;

      Procedure Update;
      
      Procedure ResizeDevice(Const Width, Height:Integer);
      Procedure OnOrientationChange;

      //Procedure RenderShadowmap(View:TERRAViewport);
      //Procedure RenderReflections(View:Viewport);

      Function GetDefaultFullScreenShader():ShaderInterface;

      Procedure TestDebugKeys();

      Function IsBoxVisible(View:TERRAViewport; Const Box:BoundingBox): Boolean;

	    Procedure DrawFullscreenQuad(CustomShader:ShaderInterface; X1,Y1,X2,Y2:Single);

      Procedure SetWind(WindDirection:Vector3D; WindIntensity:Single);

      (*Function GetPickRay(View:TERRAViewport; TX,TY:Integer):Ray;
      Function ProjectPoint(Pos:Vector3D; V:TERRAViewport):Vector3D;
      Function ProjectBoundingBox(Box:BoundingBox; V:TERRAViewport):BoundingBox;*)


      Function AddRenderable(View:TERRAViewport; MyRenderable:TERRARenderable):Boolean;
      Procedure DeleteRenderable(MyRenderable:TERRARenderable);

//      Procedure AddOccluder(View:TERRAViewport; MyOccluder:Occluder);

      Procedure SetFog(Value:Boolean);

      Procedure AddViewport(V:TERRAViewport);
      Procedure DeleteViewport(V:TERRAViewport);
      Function GetViewport(Index:Integer):TERRAViewport;

      Function GenerateStencilID():Byte; 

      Procedure EnableReflection(Const ReflectionPoint, ReflectionNormal:Vector3D);

      Property ElapsedTime:Single Read _elapsedTime;

      Property ViewportCount:Integer Read _ViewportCount;

      Class Function IsShuttingDown:Boolean;

      Property Renderer:GraphicsRenderer Read _Renderer Write SetRenderer;

      Property DeviceViewport:TERRAViewport Read _DeviceViewport;

      Property WindVector:Vector3D Read _WindVector;

      Property FrameID:Cardinal Read _FrameID;

      //Property ProjectionMatrix:Matrix4x4 Read _Projection;
	End;

Implementation

Uses TERRA_Error, TERRA_EngineManager, TERRA_OS, TERRA_Log, TERRA_ResourceManager, TERRA_InputManager,
  TERRA_Frustum, TERRA_Lights, TERRA_Mesh, TERRA_ShaderManager, TERRA_DebugDraw;

Var
  _ShuttingDown:Boolean = False;

Class Function GraphicsManager.IsShuttingDown:Boolean;
Begin
  Result := _ShuttingDown;
End;



{ GraphicsManager }
Constructor GraphicsManager.Create();
Var
  OW, OH:Integer;
  S:TERRAString;
  RendererID:Integer;
Begin
  Log(logDebug, 'GraphicsManager', 'Initializing');

  _CurrentViewport := Nil;
  _DeviceViewport := Nil;

  _Renderables := RenderableManager.Create();

  If Application.Instance = Nil Then
    Exit;

  _FogEnable := False;

  RendererID := Application.Instance.SelectRenderer();
  If (RendererID<0) Or (RendererID >= Engine.Renderers.Count) Then
    RendererID := 0;

  SetRenderer(GraphicsRenderer(Engine.Renderers.GetItemByIndex(RendererID)));

  If Self.Renderer = Nil Then
  Begin
    RaiseError('Failed to initialized renderer with ID '+ IntegerProperty.Stringify(RendererID));
    Exit;
  End;

  Log(logDebug, 'GraphicsManager', 'Width='+ IntegerProperty.Stringify(Application.Instance.Window.Width)+' Height='+ IntegerProperty.Stringify(Application.Instance.Window.Width));

  ShowDebugTarget := captureTargetInvalid;

  Log(logDebug, 'GraphicsManager', 'Device resolution: '+ IntegerProperty.Stringify(Application.Instance.Window.Width)+' x ' + IntegerProperty.Stringify(Application.Instance.Window.Width));

  _DeviceViewport := TERRAViewport.Create('device', Nil, Application.Instance.Window.Width, Application.Instance.Window.Height);

  ShowWireframe := False;

  Self.ReflectionMatrixSky := Matrix4x4_Identity;
  Self.ReflectionMatrix := Matrix4x4_Identity;
End;

Function GraphicsManager.GetDefaultFullScreenShader():ShaderInterface;
Begin
  If (_FullscreenQuadShader = Nil) Then
  Begin
    _FullscreenQuadShader := Engine.Graphics.Renderer.CreateShader();
    _FullscreenQuadShader.Generate('fullscreen_quad', GetShader_FullscreenQuad());
  End;

  Result := _FullscreenQuadShader;
End;

Procedure GraphicsManager.AddViewport(V:TERRAViewport);
Begin
  If (V = Nil) Then
    Exit;

  Inc(_ViewportCount);
  SetLength(_Viewports, _ViewportCount);
  _Viewports[Pred(_ViewportCount)] := V;
End;

Function GraphicsManager.GetViewport(Index:Integer):TERRAViewport;
Begin
  If (Index<0) Or (Index>=_ViewportCount) Then
    Result := Nil
  Else
    Result := _Viewports[Index];
End;

Procedure GraphicsManager.DeleteViewport(V:TERRAViewport);
Var
  N, I:Integer;
Begin
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

(*Procedure GraphicsManager.RenderAds;
Var
  MyShader:Shader;
Begin
  Self.SetViewArea(0, 0, _Width, _Height);

  MyShader := GetDefaultFullScreenShader();
  ShaderManager.Instance.Bind(MyShader);
  //Engine.Textures.WhiteTexture.Bind(0);
  _AdTex.Bind(0);
  //Log(logDebug, 'GraphicsManager', 'Adsize: '+ IntegerProperty.Stringify(_AdTex.Width)+' '+ IntegerProperty.Stringify(_AdTex.Height));
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

(*Procedure GraphicsManager.RenderShadowmap(View:TERRAViewport);
Begin
  If (Not Assigned(_Scene)) Then
    Exit;

  _Scene.RenderShadowCasters(View);
End;*)

(*{$IFNDEF REFLECTIONS_WITH_STENCIL}
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
{$ENDIF}*)

Var
  _StencilVolumeShader:ShaderInterface;
  _StencilShadowShader:ShaderInterface;

Const
  StencilQuad:Array[0..11] Of Single = (1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0);

Procedure GraphicsManager.RenderStencilShadows(View:TERRAViewport);
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

  Engine.Textures.WhiteTexture.Bind(0);

  // Disable z-buffer writes (note: z-testing still occurs), and enable the stencil-buffer
  Renderer.SetDepthMask(False);
  Renderer.SetDepthTest(True);

  If ShowShadowVolumes Then
  Begin
    Engine.Graphics.Renderer.SetBlendMode(blendJoin);
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
  _StencilVolumeShader.SetMat4Uniform('cameraMatrix', View.Camera.Transform); // BIBI
  _StencilVolumeShader.SetMat4Uniform('projectionMatrix', View.Camera.Projection); // BIBI

//  _RenderStage := renderStageShadow;

  Renderer.SetCullMode(cullBack);

  // Set up stencil compare fuction, reference value, and masks.
  // Stencil test passes if ((ref  and mask) cmpfn (stencil  and mask)) is true.
  Renderer.SetStencilFunction(compareAlways, 0);
  Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilIncrementWithWrap);

  //_Scene.RenderShadowCasters(View);

  If Not ShowShadowVolumes Then
  Begin
    Renderer.SetCullMode(cullFront);
    Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilDecrementWithWrap);
    //_Scene.RenderShadowCasters(View);

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

//  _RenderStage := renderStageDiffuse;

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

Procedure GraphicsManager.RenderViewport(View:TERRAViewport);
Var
  I, J, Count, SubViews:Integer;
  Stage:RendererStage;
  Target:RenderTargetInterface;
  Pass:RenderTargetType;
Begin
  If Not View.Visible Then
    Exit;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'LightManager.Clear');{$ENDIF}
  Engine.Lights.Clear;
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Buckets.Clear');{$ENDIF}

  _Renderables.Clear();

  _ReflectionsEnabled := False;

  // fill renderables list
  If (Assigned(View.OnRender)) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderEverything');{$ENDIF}
    View.OnRender(View);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Particles.Render');{$ENDIF}
  Engine.Particles.Render(View);

  _Renderables.RenderOverlays(View, renderStageDiffuse);
  View.SpriteRenderer.Prepare();

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
      Pass := RenderTargetType(I);
      Target := View.GetRenderTarget(Pass);
      If (Target = Nil) Then
        Continue;

      If (Self.ShowDebugTarget <> captureTargetInvalid) And (Self.ShowDebugTarget <> RenderTargetType(I)) Then
        Continue;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering viewport: '+View.Name+', target '+ IntegerProperty.Stringify(I)+', width:'+ IntegerProperty.Stringify(Target.Width)+', height:'+ IntegerProperty.Stringify(Target.Height));{$ENDIF}

      Case Pass Of
        captureTargetRefraction:
          Target.BackgroundColor := ColorNull;

        (*captureTargetAlpha:
          Target.BackgroundColor := ColorNull;*)

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

      View.SetViewArea(0, 0, Target.Width, Target.Height);

      If (Pass <> captureTargetColor) And ((Not Engine.Graphics.Renderer.Features.Shaders.Avaliable) {$IFDEF POSTPROCESSING}Or (Not View.HasPostProcessing()){$ENDIF}) Then
        Continue;

      Case Pass Of
        captureTargetColor:   Stage := renderStageDiffuse;
        captureTargetNormal:  Stage := renderStageNormal;
        captureTargetEmission: Stage := renderStageGlow;
        captureTargetRefraction: Stage := renderStageRefraction;
        captureTargetReflection: Stage := renderStageReflection;
        captureTargetShadow: Stage := renderStageShadow;
        captureTargetOutline: Stage := renderStageOutline;
        //captureTargetAlpha: Stage := renderStageDiffuse;
        Else
          Continue;
      End;

    If (Stage = renderStageReflection) And (Not Self._ReflectionsEnabled) Then
      Exit;

    If (Stage = renderStageShadow) Then
    Begin
      View.SetRenderTargetState(Pass, True);
    End;

    Target.BeginCapture();
      _Renderables.RenderBuckets(View, Stage);
      View.SpriteRenderer.Render(View.Camera.Projection, View.Camera.Transform, Stage);
    Target.EndCapture();
    Inc(Count);


(*  If (_ReflectionsEnabled) And (_RenderStage = renderStageDiffuse) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderReflections');{$ENDIF}
    Self.RenderReflections(View);
  End;*)

(*    If (Renderer.Features.Shaders.Avaliable) And (Renderer.Settings.DynamicShadows.Enabled) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderStencilShadows');{$ENDIF}
      RenderStencilShadows(View);
    End;*)

      {$IFDEF PC}
      (*If (Stage = renderStageDiffuse) And (InputManager.Instance.Keys.WasPressed(KeyH)) Then
        Target.GetImage.Save('frame.png');*)

      {If (_RenderStage = renderStageGlow) And (Application.Instance.Input.Keys.WasPressed(Ord('M'))) Then
        Target.Save('bloom.png');
       If (_RenderStage = renderStageRefraction) And (Application.Instance.Input.Keys.WasPressed(Ord('N'))) Then
        Target.Save('refraction.png');}
      {$ENDIF}
    End;
  End;

  View.SpriteRenderer.Clear();



  If (Count<=0) Then
    Log(logWarning, 'GraphicsManager', 'Invalid viewport: '+View.Name);
End;

Procedure InitFullScreenQuad(FullscreenQuad:VertexData; X1,Y1,X2,Y2:Single; Orientation:TERRAOrientation);
Begin
  FullscreenQuad.SetVector3D(0, vertexPosition, Vector3D_Create(X1, Y2, 0.0));
  FullscreenQuad.SetVector3D(1, vertexPosition, Vector3D_Create(X2, Y2, 0.0));
  FullscreenQuad.SetVector3D(2, vertexPosition, Vector3D_Create(X2, Y1, 0.0));
  FullscreenQuad.SetVector3D(3, vertexPosition, Vector3D_Create(X2, Y1, 0.0));
  FullscreenQuad.SetVector3D(4, vertexPosition, Vector3D_Create(X1, Y1, 0.0));
  FullscreenQuad.SetVector3D(5, vertexPosition, Vector3D_Create(X1, Y2, 0.0));

  Case Orientation Of
    orientation_LandscapeLeft:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, Vector2D_Create(1.0, 0.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, Vector2D_Create(1.0, 1.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, Vector2D_Create(0.0, 1.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, Vector2D_Create(0.0, 1.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, Vector2D_Create(0.0, 0.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, Vector2D_Create(1.0, 0.0));
    End;

    orientation_LandscapeRight:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, Vector2D_Create(0.0, 1.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, Vector2D_Create(0.0, 0.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, Vector2D_Create(1.0, 0.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, Vector2D_Create(1.0, 0.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, Vector2D_Create(1.0, 1.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, Vector2D_Create(0.0, 1.0));
    End;

    orientation_Portrait:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, Vector2D_Create(0.0, 0.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, Vector2D_Create(1.0, 0.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, Vector2D_Create(1.0, 1.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, Vector2D_Create(1.0, 1.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, Vector2D_Create(0.0, 1.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, Vector2D_Create(0.0, 0.0));
    End;

    orientation_PortraitInverted:
    Begin
      FullscreenQuad.SetVector2D(0, vertexUV0, Vector2D_Create(1.0, 1.0));
      FullscreenQuad.SetVector2D(1, vertexUV0, Vector2D_Create(0.0, 1.0));
      FullscreenQuad.SetVector2D(2, vertexUV0, Vector2D_Create(0.0, 0.0));
      FullscreenQuad.SetVector2D(3, vertexUV0, Vector2D_Create(0.0, 0.0));
      FullscreenQuad.SetVector2D(4, vertexUV0, Vector2D_Create(1.0, 0.0));
      FullscreenQuad.SetVector2D(5, vertexUV0, Vector2D_Create(1.0, 1.0));
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
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'DrawFullScreenQuad: ('+FloatProperty.Stringify(X1)+','+FloatProperty.Stringify(Y1)+','+FloatProperty.Stringify(X2)+','+FloatProperty.Stringify(Y2)+')');{$ENDIF}

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

  Projection := Matrix4x4_Ortho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

  Renderer.SetCullMode(cullNone);
  Renderer.SetDepthTest(False);
  Renderer.SetDepthMask(False);

  Renderer.SetProjectionMatrix(Projection);
  Renderer.SetModelMatrix(Matrix4x4_Identity);
  Renderer.SetTextureMatrix(Matrix4x4_Identity);
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

   View.SetViewArea(0, 0, curRT.Width, curRT.Height); 
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

Function GraphicsManager.IsBoxVisible(View:TERRAViewport; Const Box:BoundingBox):Boolean;
(*Var
  Occ:Occluder;*)
Begin
  Result := False;
  If (Not View.Camera.Frustum.BoxVisible(Box)) Then
    Exit;

(*  // occlusion test
  Occ := _Occluders;
  While Assigned(Occ) Do
  If (Occ.BoxOccluded(Box, View)) Then
  Begin
    Exit;
  End Else
    Occ := Occ._Next;*)

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

Procedure GraphicsManager.Release;
Var
  I:Integer;
Begin
  _ShuttingDown := True;
  Log(logDebug, 'GraphicsManager', 'Shutting down');

  For I:=0 To Pred(_ViewportCount) Do
    ReleaseObject(_Viewports[I]);
  _ViewportCount := 0;

  ReleaseObject(_FullScreenQuadVertices);

  ReleaseObject(_FullscreenQuadShader);
  ReleaseObject(_FullscreenColorShader);

  ReleaseObject(_Renderables);

  ReleaseObject(_DeviceViewport);
End;


Procedure GraphicsManager.Update;
Var
  I:Integer;
  TempDebugTarget:RenderTargetType;
  Target:RenderTargetInterface;
  Time:Cardinal;
  UpdateFPS:Boolean;
  NextViewport:TERRAViewport;
Begin
  If (Self.Renderer = Nil) Then
    Exit;

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

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'BeginSceneRendering '+ IntegerProperty.Stringify(_ViewportCount));{$ENDIF}
  If (Not Application.Instance.HasFatalError) Then

  //For I:=Pred(_ViewportCount) DownTo 0 Do
  For I:=0 To Pred(_ViewportCount) Do
  If (_Viewports[I].AutoResolve) Then
  Begin
    RenderViewport(_Viewports[I]);
//    _Viewports[I].GetRenderTarget(captureTargetColor).GetImage.Save('step1.png');
  End;

  // resolve offscreen buffers
  If (Self.ShowDebugTarget = captureTargetInvalid) Then
  Begin
    For I:=Pred(_ViewportCount) DownTo 0 Do
    If (_Viewports[I].Visible) And (_Viewports[I].AutoResolve) Then
    Begin
      _Viewports[I].DrawToTarget(Nil);
    End;
  End;

  For I:=Pred(_ViewportCount) DownTo 0  Do
  If (Not _Viewports[I].AutoResolve) Then
    RenderViewport(_Viewports[I]);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'EndSceneRendering');{$ENDIF}

  _DeviceViewport.Bind(0);
  _DeviceViewport.Restore(True);

  Target := _DeviceViewport.GetRenderTarget(captureTargetColor);

  If Assigned(Target) Then
  Begin
    _DeviceViewport.SetViewArea(0, 0, Target.Width, Target.Height);
    Target.BeginCapture();
  End;

  Repeat
    NextViewport := Nil;

    For I:=0 To Pred(_ViewportCount) Do
    If (_Viewports[I].Visible) And (Not _Viewports[I].AutoResolve) And (_Viewports[I].FrameID <> Self.FrameID) Then
    Begin
      If (NextViewport = Nil) Or (_Viewports[I].Layer < NextViewport.Layer) Then
        NextViewport := _Viewports[I];
    End;

    If Assigned(NextViewport) Then
      NextViewport.DrawToTarget(_DeviceViewport)
    Else
      Break;

  Until False;

  If Assigned(Target) Then
    Target.EndCapture();

  Renderer.EndFrame();
  If UpdateFPS Then
    Renderer.UpdateFrameCounter();
End;

Procedure GraphicsManager.ResizeDevice(Const Width, Height:Integer);
Begin
  {$IFDEF MOBILE}
  Exit;
  {$ENDIF}

  If Assigned(_Renderer) Then
    _Renderer.Resize(Width, Height);

  If Assigned(_DeviceViewport) Then
    _DeviceViewport.Resize(Width, Height);

  Log(logDebug, 'GraphicsManager', 'Resizing viewports');
End;

(*Procedure GraphicsManager.AddOccluder(View:TERRAViewport; MyOccluder: Occluder);
Var
  Occ:Occluder;
  F:Frustum;
Begin
  If Not Assigned(MyOccluder) Then
  Begin
    Exit;
  End;

  MyOccluder.Update(View);
  Occ := _Occluders;
  While Assigned(Occ) Do
  If (Occ.OccluderOccluded(MyOccluder)) Then
  Begin
    Exit;
  End Else
    Occ := Occ._Next;

  If (Not View.Camera.Frustum.BoxVisible(MyOccluder._BoundingBox)) Then
    Exit;

  MyOccluder._Next := _Occluders;
  _Occluders := MyOccluder;

  Renderer.InternalStat(statOccluders);
End;*)

Procedure GraphicsManager.SetRenderer(Value: GraphicsRenderer);
Begin
  ReleaseObject(_Renderer);

  _Renderer := Value;
  If _Renderer = Nil Then
    Exit;

  Log(logDebug, 'GraphicsManager', 'Initializing Renderer: '+_Renderer.Name);
  _Renderer.Reset();
End;


(*
Function GraphicsManager.ProjectPoint(Pos: Vector3D; V:TERRAViewport): Vector3D;
Var
  RX, RY:Single;
Begin
  Result := V.ProjectPoint(Pos);
  Rx := SafeDiv(Self.UIViewport.Width, V.Width);
  Ry := SafeDiv(Self.UIViewport.Height, V.Height);

  Result.X := Result.X * Rx;
  Result.Y := Result.Y * Ry;
End;

Function GraphicsManager.GetPickRay(View:TERRAViewport; TX, TY: Integer): Ray;
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
End;*)

(*Procedure GraphicsManager.RestoreContext;
Var
  I:Integer;
  Img:TERRAImage;
Begin
  If Renderer = Nil Then
    Exit;

  Log(logDebug, 'GraphicsManager', 'Restoring rendering context');
  Renderer.OnContextLost();

  _DeviceViewport.OnContextLost();

  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].OnContextLost();
End;
 *)

Procedure GraphicsManager.OnOrientationChange;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewportCount) Do
  If Assigned(_Viewports[I].Camera) Then
    _Viewports[I].Camera.Refresh();
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
  Input := Engine.Input;

  If (Input.Keys.IsDown(KeyShift)) Then
  Begin
    If (Input.Keys.WasPressed(keyF1)) Then
      Self.ShowDebugTarget := effectTargetGlow;

    If (Input.Keys.WasPressed(keyF2)) Then
      Self.ShowDebugTarget := effectTargetBloom;

    If (Input.Keys.WasPressed(keyF3)) Then
      Self.ShowDebugTarget := effectTargetEdge;

    (*If (Input.Keys.WasPressed(keyF4)) Then
      Self.ShowDebugTarget := captureTargetAlpha;*)
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


Function GraphicsManager.AddRenderable(View:TERRAViewport; MyRenderable:TERRARenderable): Boolean;
Begin
  Result := _Renderables.AddRenderable(View, MyRenderable);
End;

Procedure GraphicsManager.DeleteRenderable(MyRenderable:TERRARenderable);
Begin
  _Renderables.DeleteRenderable(MyRenderable);
End;

End.
