Unit TERRA_Renderable;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Renderer,
  TERRA_Viewport, TERRA_Collections, TERRA_Pool;

Const
  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;
  renderFlagsSkipReflections  = 4;

  renderBucket_Opaque       = 1;
  renderBucket_Translucent  = 2;
  renderBucket_Sky          = 4;
  renderBucket_Reflections  = 8;
  renderBucket_Occluder     = 16;

Type
  RenderableManager = Class;

  Renderable = Class(TERRAObject)
    Protected
      _Manager:RenderableManager;

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

      Procedure Update(View:TERRAViewport); Virtual;

      Function GetName():TERRAString; Virtual;

      Function GetRenderBucket:Cardinal; Virtual;

      Function GetBoundingBox:BoundingBox; Virtual; Abstract;
      Procedure Render(View:TERRAViewport; Const Bucket:Cardinal); Virtual; Abstract;

      Procedure RenderLights(View:TERRAViewport); Virtual;

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

  RenderableManager = Class(TERRAObject)
    Protected
      _BucketSky:Pool;
      _BucketOpaque:Pool;
      _BucketAlpha:Pool;
      {$IFDEF REFLECTIONS_WITH_STENCIL}
      _BucketReflection:Pool;
      {$ENDIF}

      Procedure RenderList(View:TERRAViewport; RenderList:List; Const Bucket:Cardinal);

      Procedure InsertIntoPool(Target:Pool; MyRenderable:Renderable; Unsorted:Boolean);
      Procedure RemoveFromPool(Target:List; MyRenderable:Renderable);

    Public
      Constructor Create();
      Procedure Release(); Override;

      Procedure Render(View:TERRAViewport);
      Procedure Clear();

      Function AddRenderable(View:TERRAViewport; MyRenderable:Renderable; Flags:Cardinal):Boolean;
      Procedure DeleteRenderable(MyRenderable:Renderable);
  End;


Implementation
Uses TERRA_GraphicsManager, TERRA_Decals, TERRA_Billboards;

{ Renderable }
Procedure Renderable.Release;
Begin
  If Assigned(_Manager) Then
    _Manager.DeleteRenderable(Self);
End;

Function Renderable.GetName:TERRAString;
Begin
  Result := Self.ClassName + '_'+HexStr(Cardinal(Self));
End;

Procedure Renderable.RenderLights;
Begin
  // do nothing
End;

Procedure Renderable.Update(View:TERRAViewport);
Begin
  // do nothing
End;

Function Renderable.GetRenderBucket:Cardinal;
Begin
  Result := 0;
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

{ RenderableManager }
Constructor RenderableManager.Create();
Begin
  _BucketSky := Pool.Create(collection_Sorted_Descending);
  _BucketOpaque := Pool.Create(collection_Sorted_Ascending);
  _BucketAlpha :=  Pool.Create(collection_Sorted_Descending);

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  _BucketReflection := Pool.Create(coAppend);
  {$ENDIF}
End;


Procedure RenderableManager.RenderList(View:TERRAViewport; RenderList:List; Const Bucket:Cardinal);
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
      If (Not GraphicsManager.Instance.ReflectionActive) Or (P._Object._Flags And renderFlagsSkipReflections=0) Then
        P._Object.Render(View, Bucket);
    End;

    P := RenderableProxy(P.Next);
  End;
End;

Procedure RenderableManager.Render(View:TERRAViewport);
Begin
  RenderList(View, _BucketSky, renderBucket_Sky);

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
    RenderList(View, _BucketOpaque, renderBucket_Opaque);

    If (GraphicsManager.Instance.RenderStage <> renderStageNormal) Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderDecals');{$ENDIF}
      DecalManager.Instance.Render(View);

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderBillboards');{$ENDIF}
      BillboardManager.Instance.Render(View);
    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Scene.RenderAlphaBucket');{$ENDIF}
    RenderList(View, _BucketAlpha, renderBucket_Opaque);
    {$ENDIF}
{$ENDIF}
End;

Procedure RenderableManager.DeleteRenderable(MyRenderable:Renderable);
Begin
  RemoveFromPool(_BucketSky, MyRenderable);
  RemoveFromPool(_BucketOpaque, MyRenderable);
  RemoveFromPool(_BucketAlpha, MyRenderable);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  RemoveFromPool(_BucketReflection, MyRenderable);
  {$ENDIF}
End;

Procedure RenderableManager.Clear;
Begin
  _BucketSky.Clear();
  _BucketOpaque.Clear();
  _BucketAlpha.Clear();
End;


Procedure RenderableManager.InsertIntoPool(Target:Pool; MyRenderable:Renderable; Unsorted:Boolean);
Var
  Proxy:CollectionObject;
Begin
  MyRenderable._Manager := Self;

  Proxy := Target.Recycle();
  If Assigned(Proxy) Then
  Begin
    RenderableProxy(Proxy)._Object := MyRenderable;
  End Else
    Proxy := RenderableProxy.Create(MyRenderable);

  Target.Add(Proxy);
End;

Procedure RenderableManager.RemoveFromPool(Target:List; MyRenderable:Renderable);
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

Function RenderableManager.AddRenderable(View:TERRAViewport; MyRenderable:Renderable; Flags:Cardinal):Boolean;
Const
  LODS:Array[0..MaxLODLevel] Of Single = (0.0, 0.4, 0.8, 1.0);
Var
  Box:BoundingBox;
  Pos:Vector3D;
  I:Integer;
  FarDist:Single;
  Unsorted:Boolean;
  Graphics:GraphicsManager;
  Bucket:Cardinal;
Begin
  If Not Assigned(MyRenderable) Then
  Begin
    Result := False;
    Exit;
  End;

  Unsorted := (Flags And renderFlagsSkipSorting<>0);
  MyRenderable._Flags := Flags;
  Bucket := MyRenderable.GetRenderBucket();

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (_RenderStage = renderStageReflection) Then
  Begin
    InsertIntoPool(_BucketReflection, MyRenderable, Unsorted);
    Exit;
  End;
  {$ENDIF}

  Graphics := GraphicsManager.Instance;

  If (Graphics.RenderStage <> renderStageDiffuse) Then
  Begin
    DebugBreak;
    //MyRenderable.Render(View, Bucket);
    Result := True;
    Exit;
  End;

//Log(logDebug, 'GraphicsManager', 'Rendering lights...');
//Log(logDebug, 'GraphicsManager', 'Class: '+MyRenderable.ClassName);

  If (Graphics.Renderer.Settings.DynamicLights.Enabled) Then
    MyRenderable.RenderLights(View);

  If (MyRenderable._LastUpdate <> Graphics.FrameID) Then
  Begin
    MyRenderable._LastUpdate := Graphics.FrameID;
    MyRenderable.Update(View);
  End;

  If (Bucket And renderBucket_Sky = 0)  Then
  Begin
    // frustum test
    Box := MyRenderable.GetBoundingBox;
    If (Flags And renderFlagsSkipFrustum=0) And (Not Graphics.IsBoxVisible(View, Box)) Then
    Begin
      MyRenderable._WasVisible := False;
      Result := False;
      Exit;
    End;

    MyRenderable._WasVisible := True;

    Pos := VectorAdd(Box.Center , VectorScale(View.Camera.View, -Box.Radius));
    MyRenderable._Distance := Pos.Distance(View.Camera.Position);

    FarDist := View.Camera.Far;

    For I:=1 To MaxLODLevel Do
    If (MyRenderable._Distance < LODS[I]*FarDist) Then
    Begin
      MyRenderable._LOD := Pred(I) + ((MyRenderable._Distance - (LODS[Pred(I)]*FarDist)) / ((LODS[I] - LODS[Pred(I)])*FarDist));
      Break;
    End Else
    If (I >= MaxLODLevel) Then
      MyRenderable._LOD := MaxLODLevel;

  End;

  Graphics.Renderer.InternalStat(statRenderables);

  If (Bucket And renderBucket_Translucent<>0) Then
    InsertIntoPool(_BucketAlpha, MyRenderable, Unsorted);

  If (Bucket And renderBucket_Opaque<>0) Then
    InsertIntoPool(_BucketOpaque, MyRenderable, Unsorted);

  If (Bucket And renderBucket_Sky<>0) Then
    InsertIntoPool(_BucketSky, MyRenderable, Unsorted);

  Result := True;
End;

procedure RenderableManager.Release;
begin
  ReleaseObject(_BucketSky);
  ReleaseObject(_BucketOpaque);
  ReleaseObject(_BucketAlpha);
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  ReleaseObject(_BucketReflection);
  {$ENDIF}
End;


End.
