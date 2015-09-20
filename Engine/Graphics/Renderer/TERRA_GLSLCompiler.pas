Unit TERRA_GLSLCompiler;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_ShaderNode, TERRA_ShaderCompiler, TERRA_VertexFormat, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D;

Type
  GLSLFloatConstantBlock = Class(ShaderBlock)
    Protected
      _Value:Single;

    Public
      Constructor Create(N:Single);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLVec2ConstantBlock = Class(ShaderBlock)
    Protected
      _Value:Vector2D;

    Public
      Constructor Create(Const V:Vector2D);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLVec3ConstantBlock = Class(ShaderBlock)
    Protected
      _Value:Vector3D;

    Public
      Constructor Create(Const V:Vector3D);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLVec4ConstantBlock = Class(ShaderBlock)
    Protected
      _Value:Vector4D;

    Public
      Constructor Create(Const V:Vector4D);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLOutputBlock = Class(ShaderBlock)
    Protected
      _OutType:ShaderOutputType;
      _Arg:ShaderBlock;

    Public
      Constructor Create(Arg:ShaderBlock; OutType:ShaderOutputType);

      Function EmitCode():TERRAString; Override;

      Function Acessor():TERRAString; Override;
  End;

  GLSLAttributeBlock = Class(ShaderBlock)
    Protected
      _Name:TERRAString;
      _Kind:VertexFormatAttribute;
      _Type:ShaderNodeType;

    Public
      Constructor Create(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType);

      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLFunctionBlock = Class(ShaderBlock)
    Protected
      _Func:ShaderFunctionType;

      Function GetFunctionName():TERRAString;

    Public
  End;

  GLSLUnaryFunctionBlock = Class(GLSLFunctionBlock)
    Protected
      _Arg:ShaderBlock;

    Public
      Constructor Create(Arg:ShaderBlock; Func:ShaderFunctionType);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLBinaryFunctionBlock = Class(GLSLFunctionBlock)
    Protected
      _Arg1:ShaderBlock;
      _Arg2:ShaderBlock;

    Public
      Constructor Create(Arg1, Arg2:ShaderBlock; Func:ShaderFunctionType);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLTernaryFunctionBlock = Class(GLSLFunctionBlock)
    Protected
      _Arg1:ShaderBlock;
      _Arg2:ShaderBlock;
      _Arg3:ShaderBlock;

    Public
      Constructor Create(Arg1, Arg2, Arg3:ShaderBlock; Func:ShaderFunctionType);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLSwizzleBlock = Class(ShaderBlock)
    Protected
      _Mask:Cardinal;
      _Arg:ShaderBlock;

    Public
      Constructor Create(Arg:ShaderBlock; Mask:Cardinal);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLTextureSamplerBlock = Class(ShaderBlock)
    Protected
      _SamplerArg:ShaderBlock;
      _TexCoordArg:ShaderBlock;

    Public
      Constructor Create(SamplerArg, TexCoordArg:ShaderBlock);
      Function EmitCode():TERRAString; Override;
      Function EmitType():TERRAString; Override;
  End;

  GLSLShaderCompiler = Class(ShaderCompiler)
    Protected
      Function CreateFloatConstant(Const N:Single):ShaderBlock; Override;
      Function CreateVec2Constant(Const V:Vector2D):ShaderBlock; Override;
      Function CreateVec3Constant(Const V:Vector3D):ShaderBlock; Override;
      Function CreateVec4Constant(Const V:Vector4D):ShaderBlock; Override;

      Function CreateUniform(Const Name:TERRAString; Kind:ShaderNodeType):ShaderBlock; Override;
      Function CreateAttribute(Const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderBlock; Override;
      Function CreateOutput(Arg:ShaderBlock; Const OutType:ShaderOutputType):ShaderBlock; Override;

      Function CreateTextureSampler(SamplerArg, TexCoordArg:ShaderBlock):ShaderBlock; Override;
      Function CreateSwizzle(Arg:ShaderBlock; Mask:Cardinal):ShaderBlock; Override;
      Function CreateUnaryFunctionCall(Arg:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Override;
      Function CreateBinaryFunctionCall(Arg1, Arg2:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Override;
      Function CreateTernaryFunctionCall(Arg1, Arg2, Arg3:ShaderBlock; Func:ShaderFunctionType):ShaderBlock; Override;

    Public
      Function GenerateCode(Out VertexShader, FragmentShader:TERRAString):Boolean; Override;

  End;

Implementation


{ GLSLShaderCompiler }
Function GLSLShaderCompiler.GenerateCode(Out VertexShader, FragmentShader:TERRAString):Boolean;
Var
  I:Integer;
  S:TERRAString;
  Current:ShaderBlock;
Begin
  VertexShader := '';
  Self.AddLine(VertexShader, 'attribute highp vec4 terra_position;');
	Self.AddLine(VertexShader, 'uniform mat4 cameraMatrix;');
	Self.AddLine(VertexShader, 'uniform mat4 modelMatrix;');
	Self.AddLine(VertexShader, 'uniform mat4 modelMatrixInverse;');
  Self.AddLine(VertexShader, 'uniform mat4 projectionMatrix;');

  Self.AddLine(VertexShader, 'void main(){');
  Self.AddLine(VertexShader, ' gl_Position = projectionMatrix * cameraMatrix * modelMatrix * terra_position;');
  Self.AddLine(VertexShader, '}');


  FragmentShader := '';

  Self.AddLine(FragmentShader, 'void main(){');

  //Self.AddLine(Result, 'gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);');

  Current := Self._FirstBlock;
  While Assigned(Current) Do
  Begin
    S := Current.EmitType() + ' ' +Current.Acessor() + ' = ' + Current.EmitCode() + ';';
    Self.AddLine(FragmentShader, S);

    Current := Current.Next;
  End;

  Self.AddLine(FragmentShader, '}');

  Result := True;
End;

Function GLSLShaderCompiler.CreateFloatConstant(Const N: Single): ShaderBlock;
Begin
  Result := GLSLFloatConstantBlock.Create(N);
End;

Function GLSLShaderCompiler.CreateVec2Constant(Const V: Vector2D): ShaderBlock;
Begin
  Result := GLSLVec2ConstantBlock.Create(V);
End;

Function GLSLShaderCompiler.CreateVec3Constant(Const V: Vector3D): ShaderBlock;
Begin
  Result := GLSLVec3ConstantBlock.Create(V);
End;

Function GLSLShaderCompiler.CreateVec4Constant(const V: Vector4D): ShaderBlock;
Begin
  Result := GLSLVec4ConstantBlock.Create(V);
End;

Function GLSLShaderCompiler.CreateAttribute(const Name:TERRAString; Const AttrKind:VertexFormatAttribute; AttrType:ShaderNodeType):ShaderBlock;
Begin
  Result := GLSLAttributeBlock.Create(Name, AttrKind, AttrType);
End;

Function GLSLShaderCompiler.CreateUnaryFunctionCall(Arg:ShaderBlock; Func:ShaderFunctionType): ShaderBlock;
Begin
  Result := GLSLUnaryFunctionBlock.Create(Arg, Func);
End;

Function GLSLShaderCompiler.CreateBinaryFunctionCall(Arg1, Arg2:ShaderBlock; Func:ShaderFunctionType):ShaderBlock;
Begin
  Result := GLSLBinaryFunctionBlock.Create(Arg1, Arg2, Func);
End;

Function GLSLShaderCompiler.CreateTernaryFunctionCall(Arg1, Arg2, Arg3:ShaderBlock; Func: ShaderFunctionType): ShaderBlock;
Begin
  Result := GLSLTernaryFunctionBlock.Create(Arg1, Arg2, Arg3, Func);
End;

Function GLSLShaderCompiler.CreateOutput(Arg:ShaderBlock; Const OutType:ShaderOutputType):ShaderBlock;
Begin
  Result := GLSLOutputBlock.Create(Arg, OutType);
End;

Function GLSLShaderCompiler.CreateSwizzle(Arg:ShaderBlock; Mask:Cardinal): ShaderBlock;
Begin
  Result := GLSLSwizzleBlock.Create(Arg, Mask);
End;

Function GLSLShaderCompiler.CreateTextureSampler(SamplerArg, TexCoordArg:ShaderBlock): ShaderBlock;
Begin
  Result := GLSLTextureSamplerBlock.Create(SamplerArg, TexCoordArg);
End;

Function GLSLShaderCompiler.CreateUniform(const Name: TERRAString; Kind: ShaderNodeType): ShaderBlock;
Begin
     Result := Nil; //FIXME
End;

{ GLSLFloatConstantBlock }
Constructor GLSLFloatConstantBlock.Create(N: Single);
Begin
  Self._Value := N;
End;

Function GLSLFloatConstantBlock.EmitCode: TERRAString;
Begin
  Result := FloatProperty.Stringify(_Value);
End;

Function GLSLFloatConstantBlock.EmitType: TERRAString;
Begin
  Result := 'float';
End;

{ GLSLVec2ConstantBlock }

Constructor GLSLVec2ConstantBlock.Create(const V: Vector2D);
Begin
  Self._Value := V;
End;

Function GLSLVec2ConstantBlock.EmitCode: TERRAString;
Begin
  Result := 'vec2('+FloatProperty.Stringify(_Value.X)+', '+ FloatProperty.Stringify(_Value.Y)+ ')';
End;

Function GLSLVec2ConstantBlock.EmitType: TERRAString;
Begin
  Result := 'vec2';
End;

{ GLSLVec3ConstantBlock }

Constructor GLSLVec3ConstantBlock.Create(const V: Vector3D);
Begin
  Self._Value := V;
End;

Function GLSLVec3ConstantBlock.EmitCode: TERRAString;
Begin
  Result := 'vec3('+FloatProperty.Stringify(_Value.X)+', '+ FloatProperty.Stringify(_Value.Y)+ ', '+FloatProperty.Stringify(_Value.Z)+')';
End;

Function GLSLVec3ConstantBlock.EmitType: TERRAString;
Begin
  Result := 'vec3';
End;

{ GLSLVec4ConstantBlock }

Constructor GLSLVec4ConstantBlock.Create(const V: Vector4D);
Begin
  Self._Value := V;
End;

Function GLSLVec4ConstantBlock.EmitCode: TERRAString;
Begin
  Result := 'vec4('+FloatProperty.Stringify(_Value.X)+', '+ FloatProperty.Stringify(_Value.Y)+ ', '+FloatProperty.Stringify(_Value.Z)+ ', '+FloatProperty.Stringify(_Value.W)+')';
End;

Function GLSLVec4ConstantBlock.EmitType: TERRAString;
Begin
  Result := 'vec4';
End;

{ GLSLAttributeBlock }
Constructor GLSLAttributeBlock.Create(Const Name:TERRAString; Const AttrKind: VertexFormatAttribute; AttrType: ShaderNodeType);
Begin
  Self._Name := Name;
  Self._Kind := AttrKind;
  Self._Type := AttrType;
End;

Function GLSLAttributeBlock.EmitCode: TERRAString;
Begin
  Result := _Name;
End;

Function GLSLAttributeBlock.EmitType: TERRAString;
Begin
  Case _Type Of
		shaderNode_Float: Result := 'float';
		shaderNode_Vector2D: Result := 'vec2';
		shaderNode_Vector3D: Result := 'vec3';
		shaderNode_Vector4D: Result := 'vec4';
		shaderNode_Matrix3x3: Result := 'mat3';
		shaderNode_Matrix4x4: Result := 'mat4';
		shaderNode_Texture2D: Result := 'sampler2D';
		shaderNode_CubeMap: Result := 'samplerCubemap';
  Else
    Result := 'ERROR';
  End;
End;

{ GLSLOutputBlock }
Function GLSLOutputBlock.Acessor: TERRAString;
Begin
  Result := 'gl_FragColor';
End;

Constructor GLSLOutputBlock.Create(Arg:ShaderBlock; OutType: ShaderOutputType);
Begin
  Self._Arg := Arg;
  Self._OutType := OutType;
End;

Function GLSLOutputBlock.EmitCode: TERRAString;
Begin
  Result := _Arg.Acessor();
End;

{ GLSLFunctionBlock }
Function GLSLFunctionBlock.GetFunctionName: TERRAString;
Begin
  Case _Func Of
    shaderFunction_Length: Result := 'length';
    shaderFunction_Sqrt: Result := 'sqrt';
    shaderFunction_Abs: Result := 'abs';
    shaderFunction_Normalize: Result := 'normalize';
    shaderFunction_Sign: Result := 'sign';
    shaderFunction_Ceil: Result := 'ceil';
    shaderFunction_Floor: Result := 'floor';
    shaderFunction_Round: Result := 'round';
    shaderFunction_Trunc: Result := 'trunc';
    shaderFunction_Frac: Result := 'frac';
    shaderFunction_Cos: Result := 'cos';
    shaderFunction_Sin: Result := 'sin';
    shaderFunction_Tan: Result := 'tan';

    // binary
    shaderFunction_Add: Result := '+';
    shaderFunction_Subtract: Result := '-';
    shaderFunction_Multiply: Result := '*';
    shaderFunction_Divide: Result := '/';
    shaderFunction_Modulus: Result := '%';
    shaderFunction_DotProduct: Result := 'dot';
    shaderFunction_CrossProduct: Result := 'cross';
    shaderFunction_Pow: Result := 'pow';
    shaderFunction_Min: Result := 'min';
    shaderFunction_Max: Result := 'max';
    shaderFunction_Step: Result := 'step';
    shaderFunction_SmoothStep: Result := 'smoothstep';

    // ternary
    shaderFunction_Lerp: Result := 'mix';

    Else
      Begin
        Result := '!!ERROR';
        Exit;
      End;
  End;
End;

{ GLSLUnaryFunctionBlock }
Constructor GLSLUnaryFunctionBlock.Create(Arg:ShaderBlock; Func: ShaderFunctionType);
Begin
  Self._Func := Func;
  Self._Arg := Arg;
End;

Function GLSLUnaryFunctionBlock.EmitCode: TERRAString;
Begin
  Result := Self.GetFunctionName() + '(' + _Arg.Acessor() + ')';
End;

Function GLSLUnaryFunctionBlock.EmitType: TERRAString;
Begin
  Result := Self._Arg.EmitType();
End;

{ GLSLBinaryFunctionBlock }
Constructor GLSLBinaryFunctionBlock.Create(Arg1, Arg2: ShaderBlock; Func: ShaderFunctionType);
Begin
  Self._Func := Func;
  Self._Arg1 := Arg1;
  Self._Arg2 := Arg2;
End;

Function GLSLBinaryFunctionBlock.EmitCode: TERRAString;
Var
  C:TERRAChar;
Begin
  Result := Self.GetFunctionName();
  C := StringFirstChar(Result);
  If (C<'A') Then
  Begin
    Result := '('+ _Arg1.Acessor() + ' '+Result + ' '+ _Arg2.Acessor() +')';
  End Else
  Begin
    Result := Result+'('+ _Arg1.Acessor() + ', '+ _Arg2.Acessor() +')';
  End;
End;

Function GLSLBinaryFunctionBlock.EmitType: TERRAString;
Begin
  Result := _Arg1.EmitType();
End;

{ GLSLTernaryFunctionBlock }
Constructor GLSLTernaryFunctionBlock.Create(Arg1, Arg2, Arg3: ShaderBlock; Func: ShaderFunctionType);
Begin
  Self._Func := Func;
  Self._Arg1 := Arg1;
  Self._Arg2 := Arg2;
  Self._Arg3 := Arg3;
End;

Function GLSLTernaryFunctionBlock.EmitCode: TERRAString;
Begin
  Result := Self.GetFunctionName() +'('+ _Arg1.Acessor() + ', '+ _Arg2.Acessor()+ ', '+ _Arg3.Acessor() +')';
End;

Function GLSLTernaryFunctionBlock.EmitType:TERRAString;
Begin
  Result := _Arg1.EmitType();
End;

{ GLSLSwizzleBlock }
Constructor GLSLSwizzleBlock.Create(Arg: ShaderBlock; Mask: Cardinal);
Begin
  Self._Mask := Mask;
  Self._Arg := Arg;
End;

Function GLSLSwizzleBlock.EmitCode:TERRAString;
Begin
  Result := '';

  If ((_Mask And vectorComponentX)<>0) Then
    Result := Result + 'x';

  If ((_Mask And vectorComponentY)<>0) Then
    Result := Result + 'y';

  If ((_Mask And vectorComponentZ)<>0) Then
    Result := Result + 'z';

  If ((_Mask And vectorComponentW)<>0) Then
    Result := Result + 'w';

  Result := _Arg.Acessor() + '.' + Result;
End;

Function GLSLSwizzleBlock.EmitType:TERRAString;
Var
  Count:Integer;
Begin
  Count := 0;

  If ((_Mask And vectorComponentX)<>0) Then
    Inc(Count);

  If ((_Mask And vectorComponentY)<>0) Then
    Inc(Count);

  If ((_Mask And vectorComponentZ)<>0) Then
    Inc(Count);

  If ((_Mask And vectorComponentW)<>0) Then
    Inc(Count);


  Case Count Of
  1:  Result := 'float';
  2:  Result := 'vec2';
  3:  Result := 'vec3';
  4:  Result := 'vec4';
  Else
    Result := 'ERROR!';
  End;
End;

{ GLSLTextureSamplerBlock }
Constructor GLSLTextureSamplerBlock.Create(SamplerArg, TexCoordArg: ShaderBlock);
Begin
  Self._SamplerArg := SamplerArg;
  Self._TexCoordArg := TexCoordArg;
End;

Function GLSLTextureSamplerBlock.EmitCode: TERRAString;
Begin
  Result := 'texture2D('+ _SamplerArg.Acessor() + ', '+ _TexCoordArg.Acessor() +')';
End;

Function GLSLTextureSamplerBlock.EmitType: TERRAString;
Begin
  Result := 'sampler2D';
End;

End.
