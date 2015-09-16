Unit TERRA_ShaderCompiler;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_VertexFormat, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Matrix3x3, TERRA_Matrix4x4;

Type
	ShaderType = (
    shaderType_Vertex,
    shaderType_Fragment
  );

	ShaderNodeType = (
		shaderNode_Invalid,
    shaderNode_Output, // cannot use this as input!
		shaderNode_Float,
		shaderNode_Vector2D,
		shaderNode_Vector3D,
		shaderNode_Vector4D,
		shaderNode_Matrix3x3,
		shaderNode_Matrix4x4,
		shaderNode_Texture2D,
		shaderNode_CubeMap
	);

  ShaderSamplerType = (
    shaderSampler_Texture2D,
    shaderSampler_Cubemap
  );

  ShaderFunctionType = (
    // unary
    shaderFunction_Length,
    shaderFunction_Sqrt,
    shaderFunction_Abs,
    shaderFunction_Normalize,
    shaderFunction_Sign,
    shaderFunction_Ceil,
    shaderFunction_Floor,
    shaderFunction_Round,
    shaderFunction_Trunc,
    shaderFunction_Frac,
    shaderFunction_Cos,
    shaderFunction_Sin,
    shaderFunction_Tan,

    // binary
    shaderFunction_Add,
    shaderFunction_Subtract,
    shaderFunction_Multiply,
    shaderFunction_Divide,
    shaderFunction_Modulus,
    shaderFunction_DotProduct,
    shaderFunction_CrossProduct,
    shaderFunction_Pow,
    shaderFunction_Min,
    shaderFunction_Max,
    shaderFunction_Step,
    shaderFunction_SmoothStep,

    // ternary
    shaderFunction_Lerp
  );

  ShaderOutputType = (
    shaderOutput_Position,
    shaderOutput_Normal,
    shaderOutput_Diffuse,
    shaderOutput_Specular,
    shaderOutput_Emission,
    shaderOutput_Refraction,
    shaderOutput_Shadow,
    shaderOutput_Outline
  );


  ShaderAttribute = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Kind:VertexFormatAttribute;
      _Type:ShaderNodeType;

    Public
  End;

  ShaderUniform = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Type:ShaderNodeType;
      _Usage:ShaderType;

    Public
  End;

  ShaderBlock = Class(TERRAObject)
    Protected
      _UsageCount:Integer;
      _Index:Integer;
      _Next:ShaderBlock;

      _Inlined:Boolean;

    Public
      Function EmitCode():TERRAString; Virtual;
      Function EmitType():TERRAString; Virtual;

      Function Acessor():TERRAString; Virtual;

      Property Next:ShaderBlock Read _Next;
  End;

  ShaderCompiler = Class(TERRAObject)
    Protected
      _Attributes:Array Of ShaderAttribute;
      _AttributeCount:Integer;

      _Uniforms:Array Of ShaderUniform;
      _UniformCount:Integer;

      _FirstBlock:ShaderBlock;
      _CurrentIndex:Integer;

      Procedure InsertBlock(Block, Before:ShaderBlock);
      Procedure UnlinkBlock(Block:ShaderBlock);

      Procedure AddLine(Var S:TERRAString; Const Line:TERRAString);

      Function AddAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderAttribute;
      Function AddUniform(Const Name:TERRAString; UType:ShaderNodeType):ShaderUniform;

      // abstract methods
      Function CreateFloatConstant(Const N:Single):ShaderBlock; Virtual; Abstract;
      Function CreateVec2Constant(Const V:Vector2D):ShaderBlock; Virtual; Abstract;
      Function CreateVec3Constant(Const V:Vector3D):ShaderBlock; Virtual; Abstract;
      Function CreateVec4Constant(Const V:Vector4D):ShaderBlock; Virtual; Abstract;

      Function CreateUniform(Const Name:TERRAString; Kind:ShaderNodeType):ShaderBlock; Virtual; Abstract;
      Function CreateAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderBlock; Virtual; Abstract;
      Function CreateOutput(Arg:ShaderBlock; Const OutType:ShaderOutputType):ShaderBlock; Virtual; Abstract;

      Function CreateTextureSampler(SamplerArg, TexCoordArg:ShaderBlock):ShaderBlock; Virtual; Abstract;
      Function CreateSwizzle(Arg:ShaderBlock; Mask:Cardinal):ShaderBlock; Virtual; Abstract;
      Function CreateUnaryFunctionCall(Arg:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Virtual; Abstract;
      Function CreateBinaryFunctionCall(Arg1, Arg2:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Virtual; Abstract;
      Function CreateTernaryFunctionCall(Arg1, Arg2, Arg3:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Virtual; Abstract;

    Public
      Function GenerateFloatConstant(Const N:Single):ShaderBlock;
      Function GenerateVec2Constant(Const V:Vector2D):ShaderBlock;
      Function GenerateVec3Constant(Const V:Vector3D):ShaderBlock;
      Function GenerateVec4Constant(Const V:Vector4D):ShaderBlock;

      Function GenerateUniform(Const Name:TERRAString; Kind:ShaderNodeType):ShaderBlock;
      Function GenerateAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderBlock;
      Function GenerateOutput(Arg:ShaderBlock; Const OutType:ShaderOutputType):ShaderBlock;

      Function GenerateTextureSampler(SamplerArg, TexCoordArg:ShaderBlock):ShaderBlock;
      Function GenerateSwizzle(Arg:ShaderBlock; Mask:Cardinal):ShaderBlock;
      Function GenerateUnaryFunctionCall(Arg:ShaderBlock; Func:ShaderFunctionType):ShaderBlock;
      Function GenerateBinaryFunctionCall(Arg1, Arg2:ShaderBlock; Func:ShaderFunctionType):ShaderBlock;
      Function GenerateTernaryFunctionCall(Arg1, Arg2, Arg3:ShaderBlock; Func:ShaderFunctionType):ShaderBlock;

      Function GenerateCode(Out VertexShader, FragmentShader:TERRAString):Boolean; Virtual;
  End;

Implementation

{ ShaderBlock }
Function ShaderBlock.Acessor: TERRAString;
Begin
  If _Inlined Then
    Result := Self.EmitCode()
  Else
    Result := 'temp'+IntegerProperty.Stringify(Self._Index);
End;

Function ShaderBlock.EmitCode:TERRAString;
Begin
  Result := '';
End;

Function ShaderBlock.EmitType:TERRAString;
Begin
  Result := '';
End;

{ ShaderCompiler }
Function ShaderCompiler.GenerateCode(Out VertexShader, FragmentShader:TERRAString):Boolean;
Begin
  Result := False;
End;

Function ShaderCompiler.GenerateAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderBlock;
Begin
  Result := Self.CreateAttribute(Name, AttrKind, AttrType);
  Self.AddAttribute(Name, AttrKind, AttrType);
End;

Function ShaderCompiler.GenerateFloatConstant(Const N:Single):ShaderBlock;
Begin
  Result := Self.CreateFloatConstant(N);
End;

Function ShaderCompiler.GenerateUniform(const Name: TERRAString; Kind: ShaderNodeType):ShaderBlock;
Begin
  Result := Self.CreateUniform(Name, Kind);
  Self.AddUniform(Name, Kind);
End;

Function ShaderCompiler.GenerateVec2Constant(Const V:Vector2D):ShaderBlock;
Begin
  Result := Self.CreateVec2Constant(V);
End;

Function ShaderCompiler.GenerateVec3Constant(Const V:Vector3D):ShaderBlock;
Begin
  Result := Self.CreateVec3Constant(V);
End;

Function ShaderCompiler.GenerateVec4Constant(Const V:Vector4D):ShaderBlock;
Begin
  Result := Self.CreateVec4Constant(V);
End;

Function ShaderCompiler.GenerateOutput(Arg:ShaderBlock; Const OutType: ShaderOutputType): ShaderBlock;
Begin
  Result := Self.CreateOutput(Arg, OutType);
  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(Arg, Result);
End;

Function ShaderCompiler.GenerateUnaryFunctionCall(Arg: ShaderBlock; Func: ShaderFunctionType): ShaderBlock;
Begin
  Result := Self.CreateUnaryFunctionCall(Arg, Func);
  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(Arg, Result);
End;

Function ShaderCompiler.GenerateBinaryFunctionCall(Arg1, Arg2: ShaderBlock;  Func: ShaderFunctionType): ShaderBlock;
Begin
  Result := Self.CreateBinaryFunctionCall(Arg1, Arg2, Func);

  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(Arg1, Result);
  Self.InsertBlock(Arg2, Result);
End;

Function ShaderCompiler.GenerateTernaryFunctionCall(Arg1, Arg2, Arg3:ShaderBlock; Func:ShaderFunctionType):ShaderBlock;
Begin
  Result := Self.CreateTernaryFunctionCall(Arg1, Arg2, Arg3, Func);

  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(Arg1, Result);
  Self.InsertBlock(Arg2, Result);
  Self.InsertBlock(Arg3, Result);
End;

Function ShaderCompiler.GenerateSwizzle(Arg: ShaderBlock; Mask:Cardinal): ShaderBlock;
Begin
  Result := Self.CreateSwizzle(Arg, Mask);

  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(Arg, Result);
End;

Function ShaderCompiler.GenerateTextureSampler(SamplerArg, TexCoordArg:ShaderBlock):ShaderBlock;
Begin
  Result := Self.CreateTextureSampler(SamplerArg, TexCoordArg);

  Self.InsertBlock(Result, Nil);
  Self.InsertBlock(SamplerArg, Result);
  Self.InsertBlock(TexCoordArg, Result);
End;

Procedure ShaderCompiler.AddLine(Var S:TERRAString; Const Line:TERRAString);
Begin
  S := S + Line + #13#10;
End;

Function ShaderCompiler.AddAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderAttribute;
Begin
  Result := ShaderAttribute.Create();
  Result._Name := Name;
  Result._Kind := AttrKind;
  Result._Type := AttrType;

  Inc(_AttributeCount);
  SetLength(_Attributes, _AttributeCount);
  _Attributes[Pred(_AttributeCount)] := Result;
End;

Function ShaderCompiler.AddUniform(Const Name:TERRAString; UType:ShaderNodeType):ShaderUniform;
Begin
  Result := ShaderUniform.Create();
  Result._Name := Name;
  Result._Type := UType;

  Inc(_UniformCount);
  SetLength(_Uniforms, _UniformCount);
  _Uniforms[Pred(_UniformCount)] := Result;
End;

Procedure ShaderCompiler.InsertBlock(Block, Before:ShaderBlock);
Var
  Current, Previous:ShaderBlock;
Begin
  If Block = Nil Then
    Exit;

  Inc(Block._UsageCount);
  Block._Index := _CurrentIndex;
  Inc(_CurrentIndex);

  If (Self._FirstBlock = Nil) Then
  Begin
    Self._FirstBlock := Block;
    Block._Next := Nil;
    Exit;
  End;

  If (Before = Nil) Then
  Begin
    Current := _FirstBlock;
    While (Assigned(Current._Next)) Do
    Begin
      Current := Current._Next;
    End;

    Current._Next := Block;
    Exit;
  End;

  Previous := Nil;
  Current := _FirstBlock;
  While (Assigned(Current)) Do
  Begin
    // block already here and before the other block
    If (Current = Block) Then
      Exit;

    If (Current = Before) Then
    Begin
      Self.UnlinkBlock(Block);

      If Assigned(Previous) Then
      Begin
        Previous._Next := Block;
      End Else
      Begin
        _FirstBlock := Block;
      End;

      Block._Next := Current;

      Exit;
    End;

    Previous := Current;
    Current := Current._Next;
  End;
End;

Procedure ShaderCompiler.UnlinkBlock(Block: ShaderBlock);
Var
  Current, Previous:ShaderBlock;
Begin
  Previous := Nil;
  Current := _FirstBlock;
  While (Assigned(Current)) Do
  Begin
    If (Current = Block) Then
    Begin
      If Assigned(Previous) Then
      Begin
        Previous._Next := Current._Next;
      End Else
        _FirstBlock := Current._Next;

      Exit;
    End;

    Current := Current._Next;
  End;
End;

End.