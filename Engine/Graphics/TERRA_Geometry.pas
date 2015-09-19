Unit TERRA_Geometry;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_VertexFormat, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_Renderer;

Type
  TERRAIndexBuffer = Class(TERRAObject)
    Protected
      _List:Array Of Word;
      _Count:Cardinal;

    Public
      Constructor Create();

      Class Function CanBePooled:Boolean; Override;

      Procedure Resize(NewSize:Cardinal);

      Procedure SetIndex(Const Index:Cardinal; Const Value:Word);
      Function GetIndex(Const Index:Cardinal):Word;

      Property Count:Cardinal Read _Count;
  End;

  TERRAGeometry = Class(TERRAObject)
    Protected
      _Indices:TERRAIndexBuffer;
      _Vertices:TERRAVertexBuffer;

    Public
      Class Function CanBePooled:Boolean; Override;
      
      Constructor Create();
      Procedure Release(); Override;

      Procedure Render();

      Property Vertices:TERRAVertexBuffer Read _Vertices;
      Property Indices:TERRAIndexBuffer Read _Indices;
  End;

Implementation
Uses TERRA_Engine, TERRA_GraphicsManager;

Constructor TERRAGeometry.Create;
Const
  SpriteVertexFormat = [vertexFormatPosition, vertexFormatColor, vertexFormatUV0, vertexFormatUV1, vertexFormatUV2];
Begin
  _Vertices := TERRAVertexBuffer(Engine.Pool.Fetch(TERRAVertexBuffer));
  If Assigned(_Vertices) Then
    _Vertices.Create(SpriteVertexFormat, 6)
  Else
    _Vertices := TERRAVertexBuffer.Create(SpriteVertexFormat, 6);

  _Vertices.SetAttributeFormat(vertexUV1, typeVector4D);
  _Vertices.SetAttributeFormat(vertexUV2, typeVector4D);

  _Indices := TERRAIndexBuffer(Engine.Pool.Fetch(TERRAIndexBuffer));
  If Assigned(_Indices) Then
    _Indices.Create()
  Else
    _Indices := TERRAIndexBuffer.Create();
End;

Procedure TERRAGeometry.Release;
Begin
  ReleaseObject(_Vertices);
  ReleaseObject(_Indices);
End;

Class Function TERRAGeometry.CanBePooled: Boolean;
Begin
  Result := True;
End;

Procedure TERRAGeometry.Render;
Var
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;

  Graphics.Renderer.SetVertexSource(Self.Vertices);
  Graphics.Renderer.DrawIndexedSource(renderTriangles, _Indices.Count, @(_Indices._List[0]));
End;

{ TERRAIndexBuffer }
Constructor TERRAIndexBuffer.Create;
Begin
  _Count := 0;
End;

Function TERRAIndexBuffer.GetIndex(Const Index:Cardinal):Word;
Begin
  If (Index<0) Or (Index>=_Count) Then
    Result := 0
  Else
    Result := _List[Index];
End;

Procedure TERRAIndexBuffer.SetIndex(Const Index:Cardinal; Const Value:Word);
Begin
  _List[Index] := Value;
End;

Procedure TERRAIndexBuffer.Resize(NewSize: Cardinal);
Begin
  If (Length(_List)< NewSize) Then
    SetLength(_List, NewSize);

  _Count := NewSize;
End;


Class Function TERRAIndexBuffer.CanBePooled: Boolean;
Begin
  Result := True;
End;

End.
