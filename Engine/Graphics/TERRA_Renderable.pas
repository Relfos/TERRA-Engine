Unit TERRA_Renderable;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Renderer,
  TERRA_Viewport, TERRA_Collections, TERRA_List, TERRA_Texture, TERRA_HashMap;

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
    Protected
      _LastUpdate:Cardinal;

//      _IsReflection:Boolean;
    Public
{      ReflectionPoint:Vector3D;
      ReflectionNormal:Vector3D;}

      Temporary:Boolean;

      Class Function CanBePooled:Boolean; Override;

      Procedure Update(View:TERRAViewport); Virtual;

      Procedure GetBucketDetails(View:TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType); Virtual;
      Procedure GetMaterialDetails(Out Shader:ShaderInterface; Out Texture:TERRATexture); Virtual;

      Function GetBoundingBox:BoundingBox; Virtual;

      Function TestVisibility(View:TERRAViewport):Boolean; Virtual;

      Function IsCompatibleWith(Other:TERRARenderable):Boolean;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Virtual; Abstract;

      Procedure OnAddToList(View:TERRAViewport; Target:RenderableManager); Virtual;

      Procedure RenderLights(View:TERRAViewport); Virtual;
  End;

  RenderableItem = Record
    SortKey:Cardinal;
    Renderable:TERRARenderable;
  End;

  RenderableArray = Array Of RenderableItem;

  RenderableList = Object
    Count:Cardinal;
    Items:RenderableArray;
    Temp:RenderableArray;

    Histogram:Array[0..255] Of Integer;
    OffsetTable:Array[0..255] Of integer;

    Procedure Init(Size:Cardinal);
    Procedure Push(View:TERRAViewport; Renderable:TERRARenderable);

    Procedure Grow();

    Procedure ByteRadixSort(Var Dest, Source:RenderableArray; ShiftCount:integer);
    Procedure Sort();
  End;

  RenderableManager = Class(TERRAObject)
    Protected
      _Batches:TERRAHashMap;

      _Renderables:RenderableList;

      Procedure Batch(View:TERRAViewport; Renderable:TERRARenderable);

      Procedure DisposeRenderableAtIndex(I:Integer);

    Public

      Constructor Create();
      Procedure Release(); Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage);
      Procedure Clear();

      Function AddRenderable(View:TERRAViewport; Renderable:TERRARenderable):Boolean;
      Procedure DeleteRenderable(Renderable:TERRARenderable);
  End;


Implementation
Uses TERRA_Engine, TERRA_GraphicsManager, TERRA_RendererStats, TERRA_GeometryBatching, TERRA_Sprite;

{ TERRARenderable }
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
  Result.StartVertex := Vector3D_Zero;
  Result.EndVertex := Vector3D_Zero;
End;

Procedure TERRARenderable.GetBucketDetails(View: TERRAViewport; Out Depth:Cardinal; Out Layer:RenderableLayer; Out AlphaType:RenderableAlphaType);
Begin
  Depth := 0;
  AlphaType := Renderable_Opaque;
  Layer := RenderableLayer_Default;
End;

Class Function TERRARenderable.CanBePooled: Boolean;
Begin
  Result := True;
End;

Procedure TERRARenderable.OnAddToList(View:TERRAViewport; Target:RenderableManager);
Var
  It:TERRAIterator;
  Other:TERRARenderable;
Begin
//  s

(*  It := Target.GetIterator();
  While It.HasNext Do
  Begin
    Other := TERRARenderable(It.Value);

    If (Self.IsCompatibleWith(Other)) Then
    Begin

      If (Other Is TERRAGeometryBatch) Then
      Begin
        Batch := TERRAGeometryBatch(Other);
        If Batch.MergeRenderable(Self) Then
        Begin
          ReleaseObject(It);
          Exit;
        End;

      End Else
      Begin

      End;

      Break;
    End;
  End;
  ReleaseObject(It);
*)

  Target.Batch(View, Self);
End;

Function TERRARenderable.TestVisibility(View: TERRAViewport): Boolean;
Begin
  Result := View.Camera.IsBoxVisible(Self.GetBoundingBox);
End;

Function TERRARenderable.IsCompatibleWith(Other: TERRARenderable): Boolean;
Begin
  Result := (Other.ClassType = Self.ClassType);

  If Result Then
    Result := (TERRASprite(Other).Texture = TERRASprite(Self).Texture);
End;

Procedure TERRARenderable.GetMaterialDetails(out Shader: ShaderInterface; out Texture: TERRATexture);
Begin
  Shader := Nil;
  Texture := Nil;
End;

{ RenderableManager }
Constructor RenderableManager.Create();
Begin
  _Renderables.Init(1024);
  _Batches := TERRAHashMap.Create();
End;

Procedure RenderableManager.Release;
Begin
//  ReleaseObject(_Renderables); no longer a class!
  ReleaseObject(_Batches);
End;

Procedure RenderableManager.Render(View:TERRAViewport; Const Stage:RendererStage);
Var
  I:Integer;
  Renderable:TERRARenderable;
Begin
  If (Stage = renderStageDiffuse) Then
    _Renderables.Sort();

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Rendering bucket'); {$ENDIF}

  For I:=0 To Pred(_Renderables.Count) Do
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', 'Fetching next...');{$ENDIF}
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'GraphicsManager', P.ToString());{$ENDIF}

    Renderable := _Renderables.Items[I].Renderable;
    If (Assigned(Renderable)) Then
    Begin
      {If (Not Engine.Graphics.ReflectionActive) Or (Renderable.RenderFlags And renderFlagsSkipReflections=0) Then}
        Renderable.Render(View, Stage);
    End;
  End;
End;


Procedure RenderableManager.DeleteRenderable(Renderable:TERRARenderable);
Var
  I:Integer;
  Temp:TERRARenderable;
Begin
  For I:=0 To Pred(_Renderables.Count) Do
  Begin
    Temp := _Renderables.Items[I].Renderable;
    If (Temp = Renderable) Then
      _Renderables.Items[I].Renderable := Nil;
  End;
End;

Procedure RenderableManager.DisposeRenderableAtIndex(I:Integer);
Var
  Temp:TERRARenderable;
Begin
  Temp := _Renderables.Items[I].Renderable;
  If (Assigned(Temp)) And (Temp.Temporary) Then
  Begin
    ReleaseObject(Temp);
    _Renderables.Items[I].Renderable := Nil;
  End;
End;

Procedure RenderableManager.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Renderables.Count) Do
    DisposeRenderableAtIndex(I);

  _Renderables.Count := 0;
End;

Function RenderableManager.AddRenderable(View:TERRAViewport; Renderable:TERRARenderable):Boolean;
(*Const
  LODS:Array[0..MaxLODLevel] Of Single = (0.0, 0.4, 0.8, 1.0);*)
Var
  Pos:Vector3D;
  I:Integer;
  FarDist:Single;
  Graphics:GraphicsManager;
Begin
  If Not Assigned(Renderable) Then
  Begin
    Result := False;
    Exit;
  End;

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
  If (Not Renderable.TestVisibility(View)) Then
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

  Renderable.OnAddToList(View, Self);

  Result := True;
End;

Procedure RenderableManager.Batch(View:TERRAViewport; Renderable:TERRARenderable);
Var
  I:Integer;
  Other:TERRARenderable;
  Batch:TERRAGeometryBatch;

  BatchKey:TERRAString;
  Shader: ShaderInterface;
  Texture: TERRATexture;
Begin
(*  Renderable.GetMaterialDetails(Shader, Texture);
  BatchKey := Shader.Name +'_'+Texture.Name;

  Batch := TERRAGeometryBatch(_Batches.GetItemByKey(BatchKey));
  If Assigned(Batch) Then
  Begin
    If Batch.MergeRenderable(Renderable) Then
      Exit;
  End Else
  For I:=0 To Pred(_Renderables.Count) Do
  Begin
    Other := _Renderables.Items[I].Renderable;
    If (Assigned(Other)) And (Renderable.IsCompatibleWith(Other)) And (Not (Other Is TERRAGeometryBatch)) Then
    Begin
      Batch := TERRAGeometryBatch.Create(Other);
      Batch.MergeRenderable(Renderable);
      Batch.Name := BatchKey;
      _Batches.Add(Batch);

      DisposeRenderableAtIndex(I);
      _Renderables.Items[I].Renderable := Batch;

      Break;
    End;
  End;

  *)

  _Renderables.Push(View, Renderable);
End;

{ RenderableList }
Procedure RenderableList.ByteRadixSort(var Dest, Source: RenderableArray; ShiftCount: integer);
Var
  I, Index:Integer;
  Swap:RenderableItem;
  Value, ValueToIndex :Cardinal;
Begin
  // biuld the histogram
  For I:=0 To 255 Do
    Histogram[I] := 0;

  Index := 0;
  While (Index<Count) Do
  Begin
    Value := Source[Index].SortKey;
    ValueToIndex := (Value Shr ShiftCount) And $FF;
    Inc(Histogram[ValueToIndex]);
    Inc(Index);
  End;

  // biuld the offsettable
  OffsetTable[0] := 0;
  for Index := 1 To 255 Do
    OffsetTable[index] := OffsetTable[index-1] + Histogram[index-1];

  // do the inner loop of the radix sort
  Index := 0;
  While (Index<Count) Do
  Begin
    Swap := Source[index];
    Value := Swap.SortKey;
    ValueToIndex := (Value Shr ShiftCount) And $FF;
    Dest[OffsetTable[ValueToIndex]] := Swap;
    Inc(OffsetTable[ValueToIndex]);
    Inc(Index);
  End;
End;

Procedure RenderableList.Sort;
Begin
  // sort based on the 1st byte
  ByteRadixSort(Temp, Items, 0);

  // sort based on the 2nd byte
  ByteRadixSort(Items, Temp, 8);

  // sort based on the 3rd byte
  ByteRadixSort(Temp, Items, 16);

  // sort based on the 4th byte
  ByteRadixSort(Items, Temp, 24);
End;

Procedure RenderableList.Init(Size: Cardinal);
Begin
  SetLength(Items, Size);
  SetLength(Temp, Size);
  Count := 0;
End;

Procedure RenderableList.Push(View:TERRAViewport; Renderable:TERRARenderable);
Var
  Depth:Cardinal;
  Layer:RenderableLayer;
  AlphaType:RenderableAlphaType;
Begin
  If Renderable = Nil Then
    Exit;

  Renderable.GetBucketDetails(View, Depth, Layer, AlphaType);

  Self.Grow();

  Items[Pred(Count)].Renderable := Renderable;
  Items[Pred(Count)].SortKey := Depth;
End;

Procedure RenderableList.Grow;
Var
  NewLen:Integer;
Begin
  Inc(Count);

  While (Length(Items)<Count) Do
  Begin
    NewLen := Length(Items) * 2;
    SetLength(Items, NewLen);
    SetLength(Temp, NewLen);
  End;
End;

End.
