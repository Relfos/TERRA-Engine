Unit TERRA_VertexFormat;

{$I terra.inc}

{-$DEFINE DEBUG_VERTEX_FORMAT}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Stream,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color;

Const
  vertexPosition  = 0;
  vertexNormal    = 1;
  vertexTangent   = 2;
  vertexBiTangent = 3;
  vertexBone      = 4;
  vertexColor     = 5;
  vertexHue       = 6;
  vertexUV0       = 7;
  vertexUV1       = 8;
  vertexUV2       = 9;
  vertexUV3       = 10;
  vertexUV4       = 11;

  MaxVertexAttributes = 12;

Type
  VertexFormatAttribute = (
    vertexFormatPosition,
    vertexFormatNormal,
    vertexFormatTangent,
    vertexFormatBiTangent,
    vertexFormatBone,
    vertexFormatHue,
    vertexFormatColor,
    vertexFormatUV0,
    vertexFormatUV1,
    vertexFormatUV2,
    vertexFormatUV3,
    vertexFormatUV4
  );

  VertexFormat = Set Of VertexFormatAttribute;

  DataFormat = (typeNull, typeFloat, typeByte, typeVector2D, typeVector3D, typeVector4D, typeColor);

  TERRAVertexBuffer = Class;


        { TERRAVertex }

 TERRAVertex = Class(TERRACollectionObject)
    Protected
      _Target:TERRAVertexBuffer;
      _VertexID:Integer;

      Procedure GetFloat(Attribute:Cardinal; Out Value:Single);
      Procedure GetColor(Attribute:Cardinal; Out Value:ColorRGBA);
      Procedure GetVector2D(Attribute:Cardinal; Out Value:Vector2D);
      Procedure GetVector3D(Attribute:Cardinal; Out Value:Vector3D);
      Procedure GetVector4D(Attribute:Cardinal; Out Value:Vector4D);

      Procedure SetFloat(Attribute:Cardinal; Const Value:Single);
      Procedure SetColor(Attribute:Cardinal; Const Value:ColorRGBA);
      Procedure SetVector2D(Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Attribute:Cardinal; Const Value:Vector4D);

      Procedure Load(); Virtual;
      Procedure Save(); Virtual;

    Public
      Constructor Create();
      Procedure Release(); Override;

      Function HasAttribute(Attribute:Cardinal):Boolean;

      Class Function CanBePooled: Boolean; Override;
    End;

  SimpleVertex = Class(TERRAVertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
      Position:Vector3D;
  End;

  VertexClass = Class Of TERRAVertex;

  VertexIterator = Class(TERRAIterator)
    Protected
      _Target:TERRAVertexBuffer;
      _LastIndex:Integer;
      _CurrentVertex:TERRAVertex;

      Procedure Setup(Target:TERRAVertexBuffer; V:VertexClass);

      Function ObtainNext():TERRACollectionObject; Override;

      Procedure Release(); Override;
      Procedure JumpToIndex(Position: Integer); Override;

    Public
      Procedure Reset(); Override;
  End;

  TERRAVertexBuffer = Class(TERRACollection)
    Protected
      _Values:Array Of Single;
      _Format:VertexFormat;
      _VertexSize:Cardinal;
      _ElementsPerVertex:Cardinal;

      _Names:Array[0..Pred(MaxVertexAttributes)] Of TERRAString;
      _Formats:Array[0..Pred(MaxVertexAttributes)] Of DataFormat;
      _Offsets:Array[0..Pred(MaxVertexAttributes)] Of Integer;

      Function GetAttributeOffsetInBytes(Attribute:Cardinal):Integer;
      Function GetAttributeOffsetInFloats(Attribute:Cardinal):Integer;

      Function GetAttributeSizeInBytes(Attribute:Cardinal):Integer;
      Function GetAttributeSizeInFloats(Attribute:Cardinal):Integer;

      Function GetAttributePosition(Const Index, Attribute:Cardinal):Integer;
      Function GetVertexPosition(Index:Cardinal):Integer;

      {$IFDEF DEBUG_VERTEX_FORMAT}
      Procedure ExpectAttributeFormat(Attribute:Cardinal; Format:DataFormat);
      {$ENDIF}

      Function GetBuffer:Pointer;

    Public
      Constructor Create(Format:VertexFormat; VertexCount:Integer);
      Procedure Release(); Override;

      Class Function CanBePooled:Boolean; Override;

      Procedure ConvertToFormat(NewFormat:VertexFormat);

      Procedure ReadAttribute(Attribute:VertexFormatAttribute; Format:DataFormat; Source:TERRAStream);
      Procedure Write(Dest:TERRAStream);

      Function HasAttribute(Attribute:Cardinal):Boolean;
      Function HasAttributeWithName(Const Name:TERRAString):Boolean;


      Procedure AddAttribute(Attribute:VertexFormatAttribute);

      Procedure SetAttributeFormat(Attribute:Cardinal; Value:DataFormat);
      Procedure SetAttributeName(Attribute:Cardinal; Const Value:TERRAString);

      Function Bind(AbsoluteOffsets:Boolean):Boolean;

      Function Clone():TERRAVertexBuffer;

      Procedure GetFloat(Index:Integer; Attribute:Cardinal; Out Value:Single);
      Procedure GetColor(Index:Integer; Attribute:Cardinal; Out Value:ColorRGBA);
      Procedure GetVector2D(Index:Integer; Attribute:Cardinal; Out Value:Vector2D);
      Procedure GetVector3D(Index:Integer; Attribute:Cardinal; Out Value:Vector3D);
      Procedure GetVector4D(Index:Integer; Attribute:Cardinal; Out Value:Vector4D);

      Procedure SetFloat(Index:Integer; Attribute:Cardinal; Const Value:Single);
      Procedure SetColor(Index:Integer; Attribute:Cardinal; Const Value:ColorRGBA);
      Procedure SetVector2D(Index:Integer; Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Index:Integer; Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Index:Integer; Attribute:Cardinal; Const Value:Vector4D);

      Function GetIterator():TERRAIterator; Override;
      Function GetIteratorForClass(V:VertexClass):VertexIterator;
      Function GetVertex(V:VertexClass; Index:Integer):TERRAVertex;

      Function GetVertexSizeInBytes():Cardinal;

      Procedure CopyBuffer(Other:TERRAVertexBuffer);
      Procedure CopyVertex(SourceIndex, DestIndex:Cardinal; SourceData:TERRAVertexBuffer = Nil);

      Procedure Resize(NewSize:Cardinal);

      {Procedure SetFloat(Index, Attribute:Cardinal; Value:Single);
      Procedure SetColor(Index, Attribute:Cardinal; Const Value:Color);
      Procedure SetVector2D(Index, Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Index, Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Index, Attribute:Cardinal; Const Value:Vector4D);}

      Property Format:VertexFormat Read _Format;
      Property Size:Cardinal Read GetVertexSizeInBytes;
      Property Buffer:Pointer Read GetBuffer;
  End;

  Function VertexFormatFromFlags(Value:Cardinal):VertexFormat;
  Function VertexFormatToFlags(Const Value:VertexFormat):Cardinal;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_GraphicsManager, TERRA_Engine, TERRA_Renderer;

Const
  DefaultAttributeNames:Array[0..Pred(MaxVertexAttributes)] Of TERRAString =
      ('terra_position',
      'terra_normal',
      'terra_tangent',
      'terra_bitangent',
      'terra_bone',
      'terra_color',
      'terra_hue',
      'terra_UV0',
      'terra_UV1',
      'terra_UV2',
      'terra_UV3',
      'terra_UV4');


Function VertexFormatFromFlags(Value:Cardinal):VertexFormat;
Begin
  Result := [];
  Move(Value, Result, SizeOf(VertexFormat));
End;

Function VertexFormatToFlags(Const Value:VertexFormat):Cardinal;
Begin
  Result := 0;
  Move(Value, Result, SizeOf(VertexFormat));
End;

Function VertexFormatAttributeValue(Attr:VertexFormatAttribute):Cardinal;
Begin
  Case Attr Of
    vertexFormatPosition: Result := vertexPosition;
    vertexFormatNormal:   Result := vertexNormal;
    vertexFormatTangent:  Result := vertexTangent;
    vertexFormatBiTangent:  Result := vertexBiTangent;
    vertexFormatBone:     Result := vertexBone;
    vertexFormatHue:      Result := vertexHue;
    vertexFormatColor:    Result := vertexColor;
    vertexFormatUV0:   Result := vertexUV0;
    vertexFormatUV1:   Result := vertexUV1;
    vertexFormatUV2:   Result := vertexUV2;
    vertexFormatUV3:   Result := vertexUV3;
    vertexFormatUV4:   Result := vertexUV4;
    Else
      Result := 0;
  End;
End;

Function GetFormatSizeInBytes(Format:DataFormat):Integer;
Begin
  Case Format Of
  typeVector2D: Result := SizeOf(Vector2D);
  typeVector3D: Result := SizeOf(Vector3D);
  typeVector4D: Result := SizeOf(Vector4D);
  typeFloat: Result := SizeOf(Single);
  typeColor: Result := SizeOf(ColorRGBA);
  typeByte: Result := SizeOf(Byte);
  Else
      Result := 0;
  End;
End;

Function GetFormatSizeInFloats(Format:DataFormat):Integer;
Begin
  Case Format Of
  typeNull:     Result := 0;
  typeVector2D: Result := 2;
  typeVector3D: Result := 3;
  typeVector4D: Result := 4;
  Else
    Result := 1;
  End;
End;

Function GetDefaultAttributeFormat(Attribute:Cardinal):DataFormat;
Begin
  Case Attribute Of
    vertexPosition,
    vertexNormal,
    vertexTangent,
    vertexBiTangent:
      Result := typeVector3D;

    vertexColor:
      Result := typeColor;

    vertexUV0,
    vertexUV1,
    vertexUV2,
    vertexUV3,
    vertexUV4:
      Result := typeVector2D;
  Else
    Result := typeFloat;
  End;
End;

{ TERRAVertexBuffer }
Constructor TERRAVertexBuffer.Create(Format:VertexFormat; VertexCount:Integer);
Var
  I:Integer;
  VF:VertexFormatAttribute;
Begin
  Self._Format := Format;

  _ElementsPerVertex := 0;
  _VertexSize := 0;
  For VF:=vertexFormatPosition To vertexFormatUV4 Do
  Begin
    I := VertexFormatAttributeValue(VF);

    _Names[I] := '';

    If (VF In Format) Then
    Begin
      _Formats[I] := GetDefaultAttributeFormat(I);
      _Offsets[I] := _ElementsPerVertex;

      Inc(_ElementsPerVertex, Self.GetAttributeSizeInFloats(I));
      Inc(_VertexSize, Self.GetAttributeSizeInBytes(I));
    End Else
    Begin
      _Offsets[I] := -1;
      _Formats[I] := typeNull;
    End;
  End;

  Self._ItemCount := 0;

  Self.Resize(VertexCount);
End;

Procedure TERRAVertexBuffer.Release();
Begin
  _ItemCount := 0;
End;


{$IFDEF DEBUG_VERTEX_FORMAT}
Procedure TERRAVertexBuffer.ExpectAttributeFormat(Attribute: Cardinal; Format: DataFormat);
Begin
  If (Attribute<MaxVertexAttributes)Then
  Begin
    If (_Formats[Attribute] <> Format) And (_Formats[Attribute] <> typeNull) Then
      RaiseError('Trying to access attribute '+DefaultAttributeNames[Attribute]+' using invalid format!');
  End;
End;
{$ENDIF}

Procedure TERRAVertexBuffer.SetAttributeFormat(Attribute:Cardinal; Value:DataFormat);
Var
  I, Diff:Integer;
  OldFormat:DataFormat;
Begin
  If (Attribute>=MaxVertexAttributes)Then
    Exit;

  OldFormat := _Formats[Attribute];
  If (OldFormat = Value) Then
    Exit;

  _Formats[Attribute] := Value;

  Diff := GetFormatSizeInFloats(Value) - GetFormatSizeInFloats(OldFormat);
  If Diff = 0 Then
    Exit;

  Inc(_ElementsPerVertex, Diff);
  Inc(_VertexSize, Diff * 4);

  For I:=Succ(Attribute) To vertexUV4 Do
  If (_Offsets[I]>=0) Then
    Inc(_Offsets[I], Diff);

  Self.Resize(Self._ItemCount);
End;

Procedure TERRAVertexBuffer.SetAttributeName(Attribute: Cardinal; const Value:TERRAString);
Begin
  If (Attribute<MaxVertexAttributes)Then
    _Names[Attribute] := Value;
End;

Function TERRAVertexBuffer.HasAttributeWithName(Const Name:TERRAString):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxVertexAttributes) Do
  Begin
    If ((Self._Names[I]<>'') And (StringEquals(_Names[I], Name))) Or (StringEquals(Name, DefaultAttributeNames[I])) Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;


Function TERRAVertexBuffer.HasAttribute(Attribute: Cardinal): Boolean;
Begin
  Result := (GetAttributeOffsetInFloats(Attribute)>=0);
End;


Procedure TERRAVertexBuffer.AddAttribute(Attribute:VertexFormatAttribute);
Var
  NewFormat:VertexFormat;
Begin
  If Self.HasAttribute(VertexFormatAttributeValue(Attribute)) Then
    Exit;

  NewFormat := Self.Format;
  Include(NewFormat, Attribute);

  Self.ConvertToFormat(NewFormat);
End;

Function TERRAVertexBuffer.GetAttributeOffsetInFloats(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := _Offsets[Attribute]
  Else
    Result := -1;
End;

Function TERRAVertexBuffer.GetAttributeOffsetInBytes(Attribute: Cardinal): Integer;
Begin
  Result := GetAttributeOffsetInFloats(Attribute) * 4;
End;

Function TERRAVertexBuffer.GetAttributeSizeInFloats(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := GetFormatSizeInFloats(_Formats[Attribute])
  Else
    Begin
      Result := 0;
      Engine.RaiseError('Invalid attribute: '+CardinalToString(Attribute));
    End;
End;

Function TERRAVertexBuffer.GetAttributeSizeInBytes(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := GetFormatSizeInBytes(_Formats[Attribute])
  Else
  Begin
    Result := 0;
    Engine.RaiseError('Invalid attribute: '+CardinalToString(Attribute));
  End;
End;

Function TERRAVertexBuffer.GetAttributePosition(Const Index, Attribute: Cardinal):Integer;
Var
  Ofs:Cardinal;
Begin
  Result := -1;

  If (Index>=_ItemCount) Or (_Values = Nil) Then
    Exit;

  If (Attribute<MaxVertexAttributes)Then
  Begin
    If _Offsets[Attribute]>=0 Then
    Begin
      Result := Index * _ElementsPerVertex + _Offsets[Attribute];
      Exit;
    End;
  End;

//  IntToString(2);
  //RaiseError('Attribute '+GetDefaultAttributeName(Attribute) +' does not exist in this buffer!');
End;

Procedure TERRAVertexBuffer.ReadAttribute(Attribute:VertexFormatAttribute; Format:DataFormat; Source:TERRAStream);
Var
  I:Integer;
  Ofs, AttrSize, BlockSize:Integer;
  AttrID:Cardinal;
  Dest:PByte;
Begin
  Self.AddAttribute(Attribute);
  AttrID := VertexFormatAttributeValue(Attribute);

  Self.SetAttributeFormat(AttrID, Format);

  AttrSize := Self.GetAttributeSizeInBytes(AttrID);
  Ofs := Self.GetAttributeOffsetInBytes(AttrID);
  BlockSize := Self.GetVertexSizeInBytes();

  Dest := Self.GetBuffer();
  Inc(Dest, Ofs);

  For I:=0 To Pred(Self.Count) Do
  Begin
    Source.Read(Dest, AttrSize);
    Inc(Dest, BlockSize);
  End;
End;

Procedure TERRAVertexBuffer.Write(Dest:TERRAStream);
Begin
  Dest.WriteInteger(Self.Count);
  If Self.Count>0 Then
    Dest.Write(@_Values[0], Self._VertexSize * Self.Count);
End;

Function TERRAVertexBuffer.GetVertexPosition(Index: Cardinal):Integer;
Begin
  If Index>=_ItemCount Then
    Result := -1
  Else
    Result := Index * _ElementsPerVertex;
End;

Procedure TERRAVertexBuffer.GetFloat(Index:Integer; Attribute:Cardinal; Out Value:Single);
Var
  Pos:Integer;
Begin
  Pos := Self.GetAttributePosition(Index, Attribute);
  If (Pos>=0) And (Pos<Length(_Values)) Then
    Value := _Values[Pos]
  Else
    Value := 0.0;
End;

Procedure TERRAVertexBuffer.SetFloat(Index:Integer; Attribute:Cardinal; Const Value:Single);
Var
  Pos:Integer;
Begin
  Pos := Self.GetAttributePosition(Index, Attribute);
  If (Pos>=0) And (Pos<Length(_Values)) Then
    _Values[Pos] := Value;
End;

Procedure TERRAVertexBuffer.GetColor(Index:Integer; Attribute:Cardinal; Out Value:ColorRGBA);
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeColor);
  {$ENDIF}
  Self.GetFloat(Index, Attribute, Single(Value));
End;

Procedure TERRAVertexBuffer.SetColor(Index:Integer; Attribute:Cardinal; Const Value:ColorRGBA);
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeColor);
  {$ENDIF}
  Self.SetFloat(Index, Attribute, Single(Value));
End;

Procedure TERRAVertexBuffer.GetVector2D(Index:Integer; Attribute:Cardinal; Out Value:Vector2D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector2D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := Vector2D_Create(0.0, 0.0);
    Exit;
  End;

  If (Pos<Length(_Values)) Then
    Value.X := _Values[Pos]
  Else
    Value.X := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.Y := _Values[Pos]
  Else
    Value.Y := 0.0;
End;

Procedure TERRAVertexBuffer.SetVector2D(Index:Integer; Attribute:Cardinal; Const Value:Vector2D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector2D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
    Exit;

  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.X;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Y;
End;

Procedure TERRAVertexBuffer.GetVector3D(Index:Integer; Attribute:Cardinal; Out Value:Vector3D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector3D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := Vector3D_Create(0.0, 0.0, 0.0);
    Exit;
  End;

  If (Pos<Length(_Values)) Then
    Value.X := _Values[Pos]
  Else
    Value.X := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.Y := _Values[Pos]
  Else
    Value.Y := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.Z := _Values[Pos]
  Else
    Value.Z := 0.0;
End;

Procedure TERRAVertexBuffer.SetVector3D(Index:Integer; Attribute:Cardinal; Const Value:Vector3D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector3D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
    Exit;

  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.X;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Y;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Z;
End;


Procedure TERRAVertexBuffer.GetVector4D(Index:Integer; Attribute:Cardinal; Out Value:Vector4D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector4D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := Vector4D_Create(0.0, 0.0, 0.0, 1.0);
    Exit;
  End;

  If (Pos<Length(_Values)) Then
    Value.X := _Values[Pos]
  Else
    Value.X := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.Y := _Values[Pos]
  Else
    Value.Y := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.Z := _Values[Pos]
  Else
    Value.Z := 0.0;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    Value.W := _Values[Pos]
  Else
    Value.W := 1.0;
End;

Procedure TERRAVertexBuffer.SetVector4D(Index:Integer; Attribute:Cardinal; Const Value:Vector4D);
Var
  Pos:Integer;
Begin
  {$IFDEF DEBUG_VERTEX_FORMAT}
  ExpectAttributeFormat(Attribute, typeVector4D);
  {$ENDIF}

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
    Exit;

  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.X;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Y;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Z;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.W;
End;

Function TERRAVertexBuffer.Bind(AbsoluteOffsets:Boolean):Boolean;
Var
  I:Integer;
  BaseOfs:PtrUInt;
  AttrOfs:Pointer;
  Name:TERRAString;
  Shader:ShaderInterface;
Begin
  Result := False;

  If Self._Values = Nil Then
    Exit;

  Engine.Graphics.Renderer.SetVertexSource(Self);

  Shader := Engine.Graphics.Renderer.ActiveShader;
  If Not Assigned(Shader) Then
  Begin
    Engine.Log.Write(logWarning, 'VBO', 'No shader!');
    Exit;
  End;

  {If Self.HasAttribute(vertexHue) then
    IntToString(2);}

{  For I:=0 To Pred(_AttributeCount) Do
  Begin
    _Attributes[I].Handle := Shader.GetAttributeHandle(_Attributes[I].Name);
    If (_Attributes[I].Handle<0) Then
      Engine.Log.Write(logDebug, 'VBO', 'Attribute '+_Attributes[I].Name+' is missing from the shader.');
  End;}

  For I:=0 To Pred(MaxVertexAttributes) Do
  Begin
    If (_Offsets[I]<0) Then
      Continue;

    If (_Names[I] <> '' ) Then
      Name := _Names[I]
    Else
      Name := DefaultAttributeNames[I];

    BaseOfs := _Offsets[I] * 4;

    If AbsoluteOffsets Then
      AttrOfs := Pointer(PtrUInt(@(_Values[0])) + BaseOfs)
    Else
      AttrOfs := Pointer(BaseOfs);

    Engine.Graphics.Renderer.SetAttributeSource(Name, I, _Formats[I], AttrOfs);
  End;

  Result := True;
End;                   

Function TERRAVertexBuffer.Clone: TERRAVertexBuffer;
Var
  I:Integer;
Begin
  Result := TERRAVertexBuffer.Create(Self._Format, Self._ItemCount);
  If Assigned(_Values) Then
    Move(_Values[0], Result._Values[0], Self._ItemCount * Self._VertexSize);

  For I:=0 To Pred(MaxVertexAttributes) Do
  Begin
    Result._Names[I] := Self._Names[I];
    Result._Formats[I] := Self._Formats[I];
    Result._Offsets[I] := Self._Offsets[I];
  End;
End;

Function TERRAVertexBuffer.GetIteratorForClass(V:VertexClass):VertexIterator;
Begin
  Result := VertexIterator(Engine.Pool.Fetch(VertexIterator));
  If Assigned(Result) Then
    Result.Create(Self)
  Else
    Result := VertexIterator.Create(Self);

  Result.Setup(Self, V);
End;

Function TERRAVertexBuffer.GetIterator():TERRAIterator;
Begin
  Result := Self.GetIteratorForClass(SimpleVertex);
End;

Procedure TERRAVertexBuffer.Resize(NewSize: Cardinal);
Var
  NewLen, CurrentLen, ExpectedLen:Integer;
Begin
  ExpectedLen := NewSize * _ElementsPerVertex;

  _ItemCount := NewSize;

  CurrentLen := Length(_Values);
  If ExpectedLen <= CurrentLen Then
    Exit;

  If (_Values = Nil) Then
    SetLength(_Values, ExpectedLen)
  Else
  Begin
    NewLen := Length(_Values);
    If NewLen<ExpectedLen Then
    Begin
      Repeat
        SetLength(_Values, NewLen * 2);
        NewLen := Length(_Values);
      Until (NewLen >= ExpectedLen);
    End;
  End;
End;

Procedure TERRAVertexBuffer.CopyBuffer(Other: TERRAVertexBuffer);
Begin
  Move(Other._Values[0], Self._Values[0], _VertexSize * _ItemCount);
End;

Procedure TERRAVertexBuffer.CopyVertex(SourceIndex, DestIndex:Cardinal; SourceData:TERRAVertexBuffer);
Var
  I:Integer;
  A:Single;
  B:Vector2D;
  C:Vector3D;
  D:Vector4D;
Begin
  If SourceData = Nil Then
    SourceData := Self;

  For I:=0 To Pred(MaxVertexAttributes) Do
  If (Self._Offsets[I]<0) Then
    Continue // we don't need this attribute, skip it
  Else
  Begin
    If (SourceData._Offsets[I]<0) Then
    Begin
      Continue;   // source does not contain this!
    End Else
    Begin
      Case Self._Formats[I] Of
      typeVector4D:
        Begin
          SourceData.GetVector4D(SourceIndex, I, D);
          Self.SetVector4D(DestIndex, I, D);
        End;

      typeVector3D:
        Begin
          SourceData.GetVector3D(SourceIndex, I, C);
          Self.SetVector3D(DestIndex, I, C);
        End;

      typeVector2D:
        Begin
          SourceData.GetVector2D(SourceIndex, I, B);
          Self.SetVector2D(DestIndex, I, B);
        End;

      Else
        Begin
          SourceData.GetFloat(SourceIndex, I, A);
          Self.SetFloat(DestIndex, I, A);
        End;
      End;
    End;
  End;
End;

Function TERRAVertexBuffer.GetBuffer: Pointer;
Begin
  If Assigned(_Values) Then
    Result := @(_Values[0])
  Else
    Result := Nil;
End;

Procedure TERRAVertexBuffer.ConvertToFormat(NewFormat: VertexFormat);
Var
  Temp:TERRAVertexBuffer;
  I,J:Integer;
  A:Single;
  B:Vector2D;
  C:Vector3D;
  D:Vector4D;
  E:ColorRGBA;
Begin
  If NewFormat = Self.Format Then
    Exit;

  Temp := TERRAVertexBuffer.Create(NewFormat, Self.Count);
  For I:=0 To Pred(Self.Count) Do
  Begin
    For J:=0 To Pred(MaxVertexAttributes) Do
    If (Temp.HasAttribute(J)) Then
    Begin
      Case Temp._Formats[J] Of
      typeVector2D:
        Begin
          Self.GetVector2D(I, J, B);
          Temp.SetVector2D(I,J, B);
        End;

      typeVector3D:
        Begin
          Self.GetVector3D(I, J, C);
          Temp.SetVector3D(I,J, C);
        End;

      typeVector4D:
        Begin
          Self.GetVector4D(I, J, D);
          Temp.SetVector4D(I,J, D);
        End;

      typeFloat:
        Begin
          Self.GetFloat(I, J, A);
          Temp.SetFloat(I,J, A);
        End;

      typeColor:
        Begin
          Self.GetColor(I, J, E);
          Temp.SetColor(I,J, E);
        End;
      End;
    End;
  End;

  SetLength(_Values, 0);
  _Values := Temp._Values;
  Temp._Values := Nil;
  Temp._ItemCount := 0;

  Self._Format := Temp._Format;
  Self._VertexSize := Temp._VertexSize;
  Self._ElementsPerVertex := Temp._ElementsPerVertex;

  For J:=0 To Pred(MaxVertexAttributes) Do
  Begin
    Self._Formats[J] := Temp._Formats[J];
    Self._Names[J] := Temp._Names[J];
    Self._Offsets[J] := Temp._Offsets[J];
  End;

  ReleaseObject(Temp);
End;

Function TERRAVertexBuffer.GetVertexSizeInBytes():Cardinal;
Begin
  Result := _VertexSize;
End;

Class Function TERRAVertexBuffer.CanBePooled: Boolean;
Begin
  Result := True;
End;

Function TERRAVertexBuffer.GetVertex(V:VertexClass; Index:Integer):TERRAVertex;
Begin
  Result := TERRAVertex(Engine.Pool.Fetch(V));
  If Assigned(V) Then
    Result.Create()
  Else
    Result := V.Create();
    
  Result._Target := Self;
  Result._VertexID := Index;
  Result.Load();
End;

{ VertexIterator }
Procedure VertexIterator.Setup(Target:TERRAVertexBuffer; V:VertexClass);
Begin
  Self._Target := Target;
  Self._LastIndex := 0;

  Self._CurrentVertex := TERRAVertex(Engine.Pool.Fetch(V));
  If Self._CurrentVertex = Nil Then
    Self._CurrentVertex := V.Create()
  Else
    Self._CurrentVertex.Create();


  Self._CurrentVertex._Target := Target;
  Self.JumpToIndex(0);
End;

Function VertexIterator.ObtainNext:TERRACollectionObject;
Begin
  If (Self.Index<_Target.Count) Then
  Begin
    Self.JumpToIndex(Self.Index);
    Result := Self._CurrentVertex;
  End Else
    Result := Nil;
End;

Procedure VertexIterator.JumpToIndex(Position:Integer);
Var
  ShouldLoad:Boolean;
Begin
  If _CurrentVertex = Nil then
  Begin
     IntegerProperty.Stringify(2);
    Exit;
  End;
    
  If _LastIndex <> Position Then
  Begin
    If (Self._CurrentVertex._VertexID>=0) Then
      Self._CurrentVertex.Save();

    ShouldLoad := True;
  End Else
    ShouldLoad := (Position=0);

  If ShouldLoad Then
  Begin
    Self._CurrentVertex._VertexID := Position;
    If Position>=0 Then
      Self._CurrentVertex.Load();

    _LastIndex := Position;
  End;
//  _CurrentVertex := Pointer(PtrUInt(_Target._Data) + VertexIndex * _Target._VertexSize);
End;

Procedure VertexIterator.Reset;
Begin
  _LastIndex := 0;
End;

Procedure VertexIterator.Release;
Begin
  ReleaseObject(Self._CurrentVertex);
  Self._LastIndex := 0;
  Inherited Release();
End;

{ TERRAVertex }
constructor TERRAVertex.Create;
Begin
  Self._VertexID := -1;
  Self._Item := Self;
End;

procedure TERRAVertex.GetColor(Attribute: Cardinal; out Value: ColorRGBA);
Begin
  _Target.GetColor(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.SetColor(Attribute: Cardinal; const Value: ColorRGBA);
Begin
  _Target.SetColor(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.GetFloat(Attribute: Cardinal; out Value: Single);
Begin
  _Target.GetFloat(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.SetFloat(Attribute: Cardinal; const Value: Single);
Begin
  _Target.SetFloat(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.GetVector2D(Attribute: Cardinal; out Value: Vector2D);
Begin
  _Target.GetVector2D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.SetVector2D(Attribute: Cardinal; const Value: Vector2D);
Begin
  _Target.SetVector2D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.GetVector3D(Attribute: Cardinal; out Value: Vector3D);
Begin
  _Target.GetVector3D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.SetVector3D(Attribute: Cardinal; const Value: Vector3D);
Begin
  _Target.SetVector3D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.GetVector4D(Attribute: Cardinal; out Value: Vector4D);
Begin
  _Target.GetVector4D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.SetVector4D(Attribute: Cardinal; const Value: Vector4D);
Begin
  _Target.SetVector4D(Self._VertexID, Attribute, Value);
End;

procedure TERRAVertex.Load;
begin
  // do nothing
end;

procedure TERRAVertex.Save;
begin
  // do nothing
end;

function TERRAVertex.HasAttribute(Attribute: Cardinal): Boolean;
Begin
  Result := _Target.HasAttribute(Attribute);
End;

procedure TERRAVertex.Release;
Begin
  If (Self._VertexID>=0) Then
  Begin
    Self.Save();
    _VertexID := -1;
  End;
End;

class function TERRAVertex.CanBePooled: Boolean;
Begin
  Result := True;
End;

{ SimpleVertex }
Procedure SimpleVertex.Load;
Begin
  Self.GetVector3D(vertexPosition, Position);
End;

Procedure SimpleVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Position);
End;

End.
