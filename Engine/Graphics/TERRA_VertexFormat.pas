Unit TERRA_VertexFormat;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Stream,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color;

Const
  vertexPosition  = 0;
  vertexNormal    = 1;
  vertexTangent   = 2;
  vertexBone      = 3;
  vertexColor     = 4;
  vertexHue       = 5;
  vertexUV0       = 6;
  vertexUV1       = 7;
  vertexUV2       = 8;
  vertexUV3       = 9;
  vertexUV4       = 10;

  MaxVertexAttributes = 11;

Type
  VertexFormatAttribute = (
    vertexFormatPosition,
    vertexFormatNormal,
    vertexFormatTangent,
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

  VertexData = Class;


	Vertex = Class(CollectionObject)
    Private
      {$IFNDEF DISABLEALLOCOPTIMIZATIONS}
      Class Function NewInstance:TObject; Override;
      Procedure FreeInstance; Override;
      {$ENDIF}

    Protected
      _Target:VertexData;
      _VertexID:Integer;

      Procedure Release(); Override;

      Procedure GetFloat(Attribute:Cardinal; Out Value:Single);
      Procedure GetColor(Attribute:Cardinal; Out Value:Color);
      Procedure GetVector2D(Attribute:Cardinal; Out Value:Vector2D);
      Procedure GetVector3D(Attribute:Cardinal; Out Value:Vector3D);
      Procedure GetVector4D(Attribute:Cardinal; Out Value:Vector4D);

      Procedure SetFloat(Attribute:Cardinal; Const Value:Single);
      Procedure SetColor(Attribute:Cardinal; Const Value:Color);
      Procedure SetVector2D(Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Attribute:Cardinal; Const Value:Vector4D);

      Procedure Load(); Virtual; Abstract;
      Procedure Save(); Virtual; Abstract;

    Public
      Constructor Create();
      Function HasAttribute(Attribute:Cardinal):Boolean;
	End;

  SimpleVertex = Class(Vertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
      Position:Vector3D;
  End;

  VertexClass = Class Of Vertex;

  VertexIterator = Class(Iterator)
    Protected
      _Target:VertexData;
      _LastIndex:Integer;
      _CurrentVertex:Vertex;

      Procedure Init(Target:VertexData; V:VertexClass);

      Function ObtainNext():CollectionObject; Override;

      Procedure Release(); Override;
      Procedure JumpToIndex(Position: Integer); Override;

    Public
      Procedure Reset(); Override;
  End;

  VertexData = Class(Collection)
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

      Function GetAttributePosition(Index, Attribute:Cardinal):Integer;
      Function GetVertexPosition(Index:Cardinal):Integer;

      Procedure ExpectAttributeFormat(Attribute:Cardinal; Format:DataFormat);

      Function GetBuffer:Pointer;

    Public
      Constructor Create(Format:VertexFormat; VertexCount:Integer);
      Procedure Release(); Override;

      Procedure ConvertToFormat(NewFormat:VertexFormat);

      Procedure Read(Source:Stream);
      Procedure Write(Dest:Stream);

      Function HasAttribute(Attribute:Cardinal):Boolean;
      Function HasAttributeWithName(Const Name:TERRAString):Boolean;


      Procedure AddAttribute(Attribute:VertexFormatAttribute);

      Procedure SetAttributeFormat(Attribute:Cardinal; Value:DataFormat);
      Procedure SetAttributeName(Attribute:Cardinal; Const Value:TERRAString);

      Function Bind(AbsoluteOffsets:Boolean):Boolean;

      Function Clone():VertexData;

      Procedure GetFloat(Index:Integer; Attribute:Cardinal; Out Value:Single);
      Procedure GetColor(Index:Integer; Attribute:Cardinal; Out Value:Color);
      Procedure GetVector2D(Index:Integer; Attribute:Cardinal; Out Value:Vector2D);
      Procedure GetVector3D(Index:Integer; Attribute:Cardinal; Out Value:Vector3D);
      Procedure GetVector4D(Index:Integer; Attribute:Cardinal; Out Value:Vector4D);

      Procedure SetFloat(Index:Integer; Attribute:Cardinal; Const Value:Single);
      Procedure SetColor(Index:Integer; Attribute:Cardinal; Const Value:Color);
      Procedure SetVector2D(Index:Integer; Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Index:Integer; Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Index:Integer; Attribute:Cardinal; Const Value:Vector4D);

      Function GetIterator():Iterator; Override;
      Function GetIteratorForClass(V:VertexClass):VertexIterator;
      Function GetVertex(V:VertexClass; Index:Integer):Vertex;

      Procedure CopyBuffer(Other:VertexData);
      Procedure CopyVertex(SourceIndex, DestIndex:Cardinal; SourceData:VertexData = Nil);

      Procedure Resize(NewSize:Cardinal);

      {Procedure SetFloat(Index, Attribute:Cardinal; Value:Single);
      Procedure SetColor(Index, Attribute:Cardinal; Const Value:Color);
      Procedure SetVector2D(Index, Attribute:Cardinal; Const Value:Vector2D);
      Procedure SetVector3D(Index, Attribute:Cardinal; Const Value:Vector3D);
      Procedure SetVector4D(Index, Attribute:Cardinal; Const Value:Vector4D);}

      Property Format:VertexFormat Read _Format;
      Property Size:Cardinal Read _VertexSize;
      Property Buffer:Pointer Read GetBuffer;
  End;

  Function VertexFormatFromFlags(Value:Cardinal):VertexFormat;
  Function VertexFormatToFlags(Const Value:VertexFormat):Cardinal;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_GraphicsManager, TERRA_Renderer
{$IFNDEF DISABLEALLOCOPTIMIZATIONS}, TERRA_StackObject{$ENDIF};

Const
  DefaultAttributeNames:Array[0..Pred(MaxVertexAttributes)] Of TERRAString =
      ('terra_position',
      'terra_normal',
      'terra_tangent',
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
  typeColor: Result := SizeOf(Color);
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
    vertexPosition: Result := typeVector3D;
    vertexNormal: Result := typeVector3D;
    vertexTangent: Result := typeVector4D;
    vertexColor: Result := typeColor;

    vertexUV0,
    vertexUV1,
    vertexUV2,
    vertexUV3,
    vertexUV4: Result := typeVector2D;
  Else
    Result := typeFloat;
  End;
End;

{ VertexData }
Constructor VertexData.Create(Format:VertexFormat; VertexCount:Integer);
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

Procedure VertexData.Release();
Begin
  If (Assigned(_Values)) Then
  Begin
    SetLength(_Values, 0);
    _Values := Nil;
  End;
End;


Procedure VertexData.ExpectAttributeFormat(Attribute: Cardinal; Format: DataFormat);
Begin
  {$IFDEF PC}
  If (Attribute<MaxVertexAttributes)Then
  Begin
    If (_Formats[Attribute] <> Format) And (_Formats[Attribute] <> typeNull) Then
      RaiseError('Trying to access attribute '+DefaultAttributeNames[Attribute]+' using invalid format!');
  End;
  {$ENDIF}
End;

Procedure VertexData.SetAttributeFormat(Attribute:Cardinal; Value:DataFormat);
Begin
  If (Attribute<MaxVertexAttributes)Then
    _Formats[Attribute] := Value;
End;

Procedure VertexData.SetAttributeName(Attribute: Cardinal; const Value:TERRAString);
Begin
  If (Attribute<MaxVertexAttributes)Then
    _Names[Attribute] := Value;
End;

Function VertexData.HasAttributeWithName(Const Name:TERRAString):Boolean;
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


Function VertexData.HasAttribute(Attribute: Cardinal): Boolean;
Begin
  Result := (GetAttributeOffsetInFloats(Attribute)>=0);
End;


Procedure VertexData.AddAttribute(Attribute:VertexFormatAttribute);
Var
  NewFormat:VertexFormat;
Begin
  If Self.HasAttribute(VertexFormatAttributeValue(Attribute)) Then
    Exit;

  NewFormat := Self.Format;
  Include(NewFormat, Attribute);

  Self.ConvertToFormat(NewFormat);
End;

Function VertexData.GetAttributeOffsetInFloats(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := _Offsets[Attribute]
  Else
    Result := -1;
End;

Function VertexData.GetAttributeOffsetInBytes(Attribute: Cardinal): Integer;
Begin
  Result := GetAttributeOffsetInFloats(Attribute) * 4;
End;

Function VertexData.GetAttributeSizeInFloats(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := GetFormatSizeInFloats(_Formats[Attribute])
  Else
    Begin
      Result := 0;
      RaiseError('Invalid attribute: '+CardinalToString(Attribute));
    End;
End;

Function VertexData.GetAttributeSizeInBytes(Attribute: Cardinal): Integer;
Begin
  If (Attribute<MaxVertexAttributes)Then
    Result := GetFormatSizeInBytes(_Formats[Attribute])
  Else
  Begin
    Result := 0;
    RaiseError('Invalid attribute: '+CardinalToString(Attribute));
  End;
End;

Function VertexData.GetAttributePosition(Index, Attribute: Cardinal):Integer;
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

Procedure VertexData.Read(Source:Stream);
Var
  NewSize:Integer;
Begin
  Source.ReadInteger(NewSize);
  Self.Resize(NewSize);
  If Self.Count>0 Then
    Source.Read(@_Values[0], Self._VertexSize * Self.Count);
End;

Procedure VertexData.Write(Dest:Stream);
Begin
  Dest.WriteInteger(Self.Count);
  If Self.Count>0 Then
    Dest.Write(@_Values[0], Self._VertexSize * Self.Count);
End;

Function VertexData.GetVertexPosition(Index: Cardinal):Integer;
Begin
  If Index>=_ItemCount Then
    Result := -1
  Else
    Result := Index * _ElementsPerVertex;
End;

Procedure VertexData.GetFloat(Index:Integer; Attribute:Cardinal; Out Value:Single);
Var
  Pos:Integer;
Begin
  Pos := Self.GetAttributePosition(Index, Attribute);
  If (Pos>=0) And (Pos<Length(_Values)) Then
    Value := _Values[Pos]
  Else
    Value := 0.0;
End;

Procedure VertexData.SetFloat(Index:Integer; Attribute:Cardinal; Const Value:Single);
Var
  Pos:Integer;
Begin
  Pos := Self.GetAttributePosition(Index, Attribute);
  If (Pos>=0) And (Pos<Length(_Values)) Then
    _Values[Pos] := Value;
End;

Procedure VertexData.GetColor(Index:Integer; Attribute:Cardinal; Out Value:Color);
Begin
//  ExpectAttributeFormat(Attribute, typeColor);
  Self.GetFloat(Index, Attribute, Single(Value));
End;

Procedure VertexData.SetColor(Index:Integer; Attribute:Cardinal; Const Value:Color);
Begin
//  ExpectAttributeFormat(Attribute, typeColor);
  Self.SetFloat(Index, Attribute, Single(Value));
End;

Procedure VertexData.GetVector2D(Index:Integer; Attribute:Cardinal; Out Value:Vector2D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector2D);

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := VectorCreate2D(0.0, 0.0);
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

Procedure VertexData.SetVector2D(Index:Integer; Attribute:Cardinal; Const Value:Vector2D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector2D);

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
    Exit;

  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.X;

  Inc(Pos);
  If (Pos<Length(_Values)) Then
    _Values[Pos] := Value.Y;
End;

Procedure VertexData.GetVector3D(Index:Integer; Attribute:Cardinal; Out Value:Vector3D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector3D);

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := VectorCreate(0.0, 0.0, 0.0);
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

Procedure VertexData.SetVector3D(Index:Integer; Attribute:Cardinal; Const Value:Vector3D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector3D);

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


Procedure VertexData.GetVector4D(Index:Integer; Attribute:Cardinal; Out Value:Vector4D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector4D);

  Pos := Self.GetAttributePosition(Index, Attribute);
  If Pos<0 Then
  Begin
    Value := VectorCreate4D(0.0, 0.0, 0.0, 1.0);
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

Procedure VertexData.SetVector4D(Index:Integer; Attribute:Cardinal; Const Value:Vector4D);
Var
  Pos:Integer;
Begin
//  ExpectAttributeFormat(Attribute, typeVector4D);

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

Function VertexData.Bind(AbsoluteOffsets:Boolean):Boolean;
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

  GraphicsManager.Instance.Renderer.SetVertexSource(Self);

  Shader := GraphicsManager.Instance.Renderer.ActiveShader;
  If Not Assigned(Shader) Then
  Begin
    Log(logWarning, 'VBO', 'No shader!');
    Exit;
  End;

  {If Self.HasAttribute(vertexHue) then
    IntToString(2);}

{  For I:=0 To Pred(_AttributeCount) Do
  Begin
    _Attributes[I].Handle := Shader.GetAttributeHandle(_Attributes[I].Name);
    If (_Attributes[I].Handle<0) Then
      Log(logDebug, 'VBO', 'Attribute '+_Attributes[I].Name+' is missing from the shader.');
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

    GraphicsManager.Instance.Renderer.SetAttributeSource(Name, I, _Formats[I], AttrOfs);
  End;

  Result := True;
End;                   

Function VertexData.Clone: VertexData;
Var
  I:Integer;
Begin
  Result := VertexData.Create(Self._Format, Self._ItemCount);
  If Assigned(_Values) Then
    Move(_Values[0], Result._Values[0], Self._ItemCount * Self._VertexSize);

  For I:=0 To Pred(MaxVertexAttributes) Do
  Begin
    Result._Names[I] := Self._Names[I];
    Result._Formats[I] := Self._Formats[I];
    Result._Offsets[I] := Self._Offsets[I];
  End;
End;

Function VertexData.GetIteratorForClass(V:VertexClass):VertexIterator;
Begin
  Result := VertexIterator.Create(Self);
  Result.Init(Self, V);
End;

Function VertexData.GetIterator():Iterator;
Begin
  Result := Self.GetIteratorForClass(SimpleVertex);
End;

Procedure VertexData.Resize(NewSize: Cardinal);
Var
  NewLen, ExpectedLen:Integer;
Begin
  If _ItemCount = NewSize Then
    Exit;

  _ItemCount := NewSize;

  ExpectedLen := _ItemCount * _ElementsPerVertex;

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
      Until (NewLen >= ExpectedLen) Or (NewLen>65500);
    End;
  End;
End;

Procedure VertexData.CopyBuffer(Other: VertexData);
Begin
  Move(Other._Values[0], Self._Values[0], _VertexSize * _ItemCount);
End;

Procedure VertexData.CopyVertex(SourceIndex, DestIndex:Cardinal; SourceData:VertexData);
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

Function VertexData.GetVertex(V: VertexClass; Index: Integer): Vertex;
Begin
  Result := V.Create();
  Result._Target := Self;
  Result._VertexID := Index;
  Result.Load();
End;

Function VertexData.GetBuffer: Pointer;
Begin
  If Assigned(_Values) Then
    Result := @(_Values[0])
  Else
    Result := Nil;
End;

Procedure VertexData.ConvertToFormat(NewFormat: VertexFormat);
Var
  Temp:VertexData;
  I,J:Integer;
  A:Single;
  B:Vector2D;
  C:Vector3D;
  D:Vector4D;
  E:Color;
Begin
  If NewFormat = Self.Format Then
    Exit;

  Temp := VertexData.Create(NewFormat, Self.Count);
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

{ VertexIterator }
Procedure VertexIterator.Init(Target:VertexData; V:VertexClass);
Begin
  Self._Target := Target;
  Self._LastIndex := 0;

  Self._CurrentVertex := V.Create();
  Self._CurrentVertex._Target := Target;
  Self.JumpToIndex(0);
End;

Function VertexIterator.ObtainNext:CollectionObject;
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
    IntToString(2);
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

{ Vertex }
Constructor Vertex.Create;
Begin
  Self._VertexID := -1;
End;

Procedure Vertex.GetColor(Attribute:Cardinal; Out Value:Color);
Begin
  _Target.GetColor(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.SetColor(Attribute:Cardinal; Const Value:Color);
Begin
  _Target.SetColor(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.GetFloat(Attribute:Cardinal; Out Value:Single);
Begin
  _Target.GetFloat(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.SetFloat(Attribute:Cardinal; Const Value:Single);
Begin
  _Target.SetFloat(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.GetVector2D(Attribute:Cardinal; Out Value:Vector2D);
Begin
  _Target.GetVector2D(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.SetVector2D(Attribute:Cardinal; Const Value:Vector2D);
Begin
  _Target.SetVector2D(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.GetVector3D(Attribute:Cardinal; Out Value:Vector3D);
Begin
  _Target.GetVector3D(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.SetVector3D(Attribute:Cardinal; Const Value:Vector3D);
Begin
  _Target.SetVector3D(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.GetVector4D(Attribute:Cardinal; Out Value:Vector4D);
Begin
  _Target.GetVector4D(Self._VertexID, Attribute, Value);
End;

Procedure Vertex.SetVector4D(Attribute:Cardinal; Const Value:Vector4D);
Begin
  _Target.SetVector4D(Self._VertexID, Attribute, Value);
End;

Function Vertex.HasAttribute(Attribute: Cardinal): Boolean;
Begin
  Result := _Target.HasAttribute(Attribute);
End;

Procedure Vertex.Release;
Begin
  If (Self._VertexID>=0) Then
  Begin
    Self.Save();
    _VertexID := -1;
  End;
End;

{$IFNDEF DISABLEALLOCOPTIMIZATIONS}
Class Function Vertex.NewInstance: TObject;
Var
  ObjSize, GlobalSize:Integer;
Begin
  ObjSize := InstanceSize();

  Result := StackAlloc(ObjSize);

  InitInstance(Result);
End;

Procedure Vertex.FreeInstance;
Begin
End;
{$ENDIF}

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
