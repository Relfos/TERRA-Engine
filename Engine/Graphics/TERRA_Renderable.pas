Unit TERRA_Renderable;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Renderer,
  TERRA_Viewport, TERRA_Collections, TERRA_List, TERRA_RadixSort;

Const
  //FogMode
  fogOff      = 0;
  fogDistance = 1;
  fogHeight   = 2;
  fogBox      = 4;

  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;
  renderFlagsSkipReflections  = 4;

Type
  RenderableManager = Class;

  RenderableLayer = (
    RenderableLayer_Skybox,
    RenderableLayer_Default,
    RenderableLayer_Reflections
  );

  RenderableAlphaType = (
    Renderable_Opaque,
    Renderable_Blend,
    Renderable_Additive,
    Renderable_Subtractive
  );

  { TERRARenderable }
  TERRARenderable = Class(TERRAObject)
    Private
      _RenderKey:Integer;
    
    Protected
      _Manager:RenderableManager;
      _RenderFlags:Cardinal;

      _LastUpdate:Cardinal;

//      _IsReflection:Boolean;
    Public
{      ReflectionPoint:Vector3D;
      ReflectionNormal:Vector3D;}

      Temporary:Boolean;

      Procedure Release; Override;

      Function SortID:Integer; Override;

      Procedure Update(View:TERRAViewport); Virtual;

      Procedure GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType); Virtual;

      Function GetBoundingBox:BoundingBox; Virtual;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Virtual; Abstract;

      Procedure RenderLights(View:TERRAViewport); Virtual;

      //Property WasVisible:Boolean Read _WasVisible;

      Property RenderFlags:Cardinal Read _RenderFlags;
  End;

(*  TERRASpriteRenderer = Class(TERRAObject)
    Protected
      _SpriteList:Array Of TERRASprite;
      _SpriteCount:Integer;

      Procedure RenderSprite(Sprite:TERRASprite; Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);

   Public
      Procedure InitBatches();
      Procedure Release; Override;

      Procedure Prepare;
      Procedure Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
      Procedure Clear;

      Procedure QueueSprite(S:TERRASprite);

      { Fetches a temporary sprite that will be disposed in the next frame }
      Function FetchSprite():TERRASprite; //Const Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):TERRASprite;
  End;*)

  RenderableManager = Class(TERRAObject)
    Protected
      _Renderables:TERRAList;

    Public

      Constructor Create();
      Procedure Release(); Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage);
      Procedure Clear();

      Function AddRenderable(View:TERRAViewport; Renderable:TERRARenderable):Boolean;
      Procedure DeleteRenderable(MyRenderable:TERRARenderable);
  End;


Implementation
Uses TERRA_Engine, TERRA_GraphicsManager, TERRA_RendererStats;

{ TERRARenderable }
procedure TERRARenderable.Release;
Begin
  If Assigned(_Manager) Then
    _Manager.DeleteRenderable(Self);
End;

procedure TERRARenderable.RenderLights(View: TERRAViewport);
Begin
  // do nothing
End;

procedure TERRARenderable.Update(View: TERRAViewport);
Begin
  // do nothing
End;

Function TERRARenderable.GetBoundingBox: BoundingBox;
Begin
       FillChar(Result, SizeOf(Result), 0);
End;

function TERRARenderable.SortID: Integer;
Begin
  Result := Self._RenderKey;
End;

Procedure TERRARenderable.GetBucketDetails(View: TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType);
Begin
  Depth := 0;
  AlphaType := Renderable_Opaque;
  Layer := RenderableLayer_Default;
End;

{ RenderableManager }
Constructor RenderableManager.Create();
Begin
  _Renderables := TERRAList.Create(collection_Sorted_Ascending, coShared);
End;

Procedure RenderableManager.Render(View:TERRAViewport; Const Stage:RendererStage);
Var
  It:TERRAIterator;
  Renderable:TERRARenderable;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering bucket'); {$ENDIF}

  It := _Renderables.GetIterator();
  While (It.HasNext) Do
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Fetching next...');{$ENDIF}
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', P.ToString());{$ENDIF}

    Renderable := TERRARenderable(It.Value);
    If (Assigned(Renderable)) Then
    Begin
      If (Not Engine.Graphics.ReflectionActive) Or (Renderable.RenderFlags And renderFlagsSkipReflections=0) Then
        Renderable.Render(View, Stage);

      (*If Renderable.Temporary Then
        It.Discard();*)
    End;
  End;
  ReleaseObject(It);

  (*

    If (_RenderStage <> renderStageNormal) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderDecals');{$ENDIF}
      DecalManager.Instance.Render();

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderBillboards');{$ENDIF}
      BillboardManager.Instance.Render();
    End;
*)
End;


Procedure RenderableManager.DeleteRenderable(MyRenderable:TERRARenderable);
Begin
  _Renderables.Remove(MyRenderable);
End;

Procedure RenderableManager.Clear;
Begin
  _Renderables.Clear();
End;

Function RenderableManager.AddRenderable(View:TERRAViewport; Renderable:TERRARenderable):Boolean;
(*Const
  LODS:Array[0..MaxLODLevel] Of Single = (0.0, 0.4, 0.8, 1.0);*)
Var
  Pos:Vector3D;
  I:Integer;
  FarDist:Single;
  Unsorted:Boolean;
  Graphics:GraphicsManager;

  Depth:Cardinal;
  Layer:RenderableLayer;
  AlphaType:RenderableAlphaType;
Begin
  If Not Assigned(Renderable) Then
  Begin
    Result := False;
    Exit;
  End;

  Unsorted := (Renderable.RenderFlags And renderFlagsSkipSorting<>0);

  Renderable._Manager := Self;

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (_RenderStage = renderStageReflection) Then
  Begin
    _BucketReflection.Add(Renderable);
    Exit;
  End;
  {$ENDIF}

  Graphics := Engine.Graphics;

//Log(logDebug, 'GraphicsManager', 'Rendering lights...');
//Log(logDebug, 'GraphicsManager', 'Class: '+MyTERRARenderable.ClassName);

  If (Graphics.Renderer.Settings.DynamicLights.Enabled) Then
    Renderable.RenderLights(View);

  If (Renderable._LastUpdate <> Graphics.FrameID) Then
  Begin
    Renderable._LastUpdate := Graphics.FrameID;
    Renderable.Update(View);
  End;

  // frustum test
  If (Renderable.RenderFlags And renderFlagsSkipFrustum=0) And (Not View.Camera.IsBoxVisible(Renderable.GetBoundingBox)) Then
  Begin
    Result := False;
    Exit;
  End;

    (*For I:=1 To MaxLODLevel Do
    If (MyTERRARenderable._Distance < LODS[I]*FarDist) Then
    Begin
      MyTERRARenderable._LOD := Pred(I) + ((MyTERRARenderable._Distance - (LODS[Pred(I)]*FarDist)) / ((LODS[I] - LODS[Pred(I)])*FarDist));
      Break;
    End Else
    If (I >= MaxLODLevel) Then
      MyTERRARenderable._LOD := MaxLODLevel;*)

  Graphics.Renderer.Stats.Update(RendererStat_Renderables);

  Renderable.GetBucketDetails(View, Depth, Layer, AlphaType);
  Renderable._RenderKey := Depth;

  _Renderables.Add(Renderable);

  Result := True;
End;

procedure RenderableManager.Release;
begin
  ReleaseObject(_Renderables);
End;


End.
