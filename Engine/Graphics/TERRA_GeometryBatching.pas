Unit TERRA_GeometryBatching;

Interface
Uses TERRA_Object, TERRA_Vector3D, TERRA_Renderable, TERRA_Sprite;

Type
  TERRAGeometryBatch = Class(TERRASprite)
    Public
      Constructor Create(Other:TERRARenderable);
      Function MergeRenderable(Other:TERRARenderable):Boolean;
  End;

Implementation
Uses TERRA_Engine, TERRA_Log, TERRA_VertexFormat, TERRA_ClipRect;

Constructor TERRAGeometryBatch.Create(Other: TERRARenderable);
Begin
  Inherited Create;

  Self.MergeRenderable(Other);
End;

Function TERRAGeometryBatch.MergeRenderable(Other: TERRARenderable):Boolean;
Var
  I, BaseID, IndexOffset:Integer;
  InIt, OutIt:VertexIterator;
  Src, Dest:SpriteVertex;
  Pos:Vector3D;

  RequiredVertices, RequiredIndices:Integer;

  OtherSprite:TERRASprite;
Begin
  Result := False;

  If (Self.Geometry.Indices.Count > (1024 * 32)) Then
    Exit;

  If (Not (Other Is TERRASprite)) Then
    Exit;

  OtherSprite := TERRASprite(Other);

  If (OtherSprite.ClipRect.Style = clipEverything) Or (OtherSprite.Geometry.Indices.Count<=0) Then
    Exit;

  BaseID := Geometry.Vertices.Count;
  IndexOffset := Geometry.Indices.Count;

  RequiredVertices := Geometry.Vertices.Count + OtherSprite.Geometry.Vertices.Count;
  RequiredIndices := Self.Geometry.Indices.Count + OtherSprite.Geometry.Indices.Count;

  Geometry.Vertices.Resize(RequiredVertices);
  Geometry.Indices.Resize(RequiredIndices);


  OutIt := Geometry.Vertices.GetIteratorForClass(SpriteVertex);
  InIt := OtherSprite.Geometry.Vertices.GetIteratorForClass(SpriteVertex);
  While (InIt.HasNext()) Do
  Begin
    If (Not OutIt.HasNext()) Then
    Begin
      Engine.Log.Write(logWarning, 'Sprite', 'Failed batch merge of sprite vertices...');
      Break;
    End;

    Src := SpriteVertex(InIt.Value);
    Dest := SpriteVertex(OutIt.Value);

    Pos := OtherSprite.Transform.Transform(Src.Position);

    //Pos.X := Trunc(Pos.X);
    //Pos.Y := Trunc(Pos.Y);
    //Pos.Z := Pos.Z + S.Layer;

    Dest.Position := Pos;
    Dest.Color := Src.Color;
    Dest.Glow := Src.Glow;
    Dest.Saturation := Src.Saturation;
    Dest.ClipRect := Src.ClipRect;
    Dest.TexCoord := Src.TexCoord;

    (*  MinX := FloatMin(MinX, Pos.X);
      MinY := FloatMin(MinY, Pos.Y);
      MaxX := FloatMax(MaxX, Pos.X);
      MaxY := FloatMax(MaxY, Pos.Y);*)
  End;
  ReleaseObject(InIt);
  ReleaseObject(OutIt);

  For I:=0 To Pred(OtherSprite.Geometry.Indices.Count) Do
    Geometry.Indices.SetIndex(IndexOffset + I, OtherSprite.Geometry.Indices.GetIndex(I) + BaseID);

  Result := True;
End;

End.
