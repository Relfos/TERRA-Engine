Unit TERRA_NullRenderer;

{$I terra.inc}
Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Renderer, TERRA_VertexFormat,
  TERRA_Color, TERRA_Image, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D,
  TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ShaderNode;

Type
  NullFeatures = Class(RendererFeatures)
    Public
      Constructor Create(Owner:GraphicsRenderer);
  End;

  NullVBO = Class(VertexBufferInterface)
    Protected
      Procedure Submit(Wireframe:Boolean); Override;

    Public
      Function Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean; Override;
      Procedure Invalidate(); Override;
  End;

  NullTexture = Class(TextureInterface)
    Protected
      _Handle:Cardinal;

    Public
      Function Generate(Pixels:Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;

      Function Bind(Slot:Integer):Boolean; Override;

      Function GetImage():TERRAImage; Override;

      Procedure Invalidate(); Override;
  End;

  NullCubeMap = Class(CubeMapInterface)
    Protected
      _Handle:Cardinal;

    Public
      Function Bind(Slot:Integer):Boolean; Override;

      Function Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;

      Procedure Invalidate(); Override;
  End;

  NullFBO = Class(RenderTargetInterface)
    Public
      Function Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer, StencilBuffer:Boolean):Boolean; Override;

      Function Bind(Slot:Integer):Boolean; Override;

	    Procedure BeginCapture(Flags:Cardinal = clearAll); Override;
	    Procedure EndCapture; Override;

      Procedure Resize(NewWidth, NewHeight:Integer); Override;

      Function GetImage():TERRAImage; Override;
      Function GetPixel(X,Y:Integer):ColorRGBA; Override;

      Procedure Invalidate(); Override;
  End;

  NullShaderAttribute = Record
    Name:TERRAString;
    Handle:Integer;
  End;

  NullShader = Class(ShaderInterface)
    Protected
      _VertexCode:TERRAString;
	    _FragmentCode:TERRAString;

	    _VertexShaderHandle:Cardinal;
	    _FragmentShaderHandle:Cardinal;

      _Program:Cardinal;
      _Linked:Boolean;
      _MRT:Boolean;

      _Attributes:Array Of NullShaderAttribute;
      _AttributeCount:Integer;


    Public
      Function Generate(Const Name:TERRAString; Shader:TERRAShaderGroup):Boolean; Override;

      Function IsReady():Boolean; Override;

      Function Bind():Boolean; Override;
      Function Unbind():Boolean; Override;

			Procedure SetIntegerUniform(Const Name:TERRAString; Const Value:Integer); Override;
			Procedure SetFloatUniform(Const Name:TERRAString; Const Value:Single); Override;
			Procedure SetVec2Uniform(Const Name:TERRAString; Const Value:Vector2D); Override;
			Procedure SetVec3Uniform(Const Name:TERRAString; const Value:Vector3D); Override;
			Procedure SetVec4Uniform(Const Name:TERRAString; const Value:Vector4D); Override;
      Procedure SetMat3Uniform(Const Name:TERRAString; Value:Matrix3x3); Override;
      Procedure SetMat4Uniform(Const Name:TERRAString; Value:Matrix4x4); Override;
      Procedure SetVec4ArrayUniform(Const Name:TERRAString; Count:Integer; Values:Array Of Vector4D); Override;

      Function GetUniform(Name:TERRAString):Integer; Override;
      Function GetAttributeHandle(Const Name:TERRAString):Integer; Override;

      Procedure Invalidate(); Override;
  End;

  NullRenderer = Class(GraphicsRenderer)
    Protected
      Function Initialize():Boolean; Override;

    Public
      Procedure ResetState(); Override;

      Function CreateTexture():TextureInterface; Override;
      Function CreateCubeMap():CubeMapInterface; Override;
      Function CreateVertexBuffer():VertexBufferInterface; Override;
      Function CreateShader():ShaderInterface; Override;
      Function CreateRenderTarget():RenderTargetInterface; Override;

      Procedure ClearBuffer(Color, Depth, Stencil:Boolean); Override;
      Procedure SetClearColor(Const ClearColor:ColorRGBA); Override;

      Procedure SetStencilTest(Enable:Boolean); Override;
      Procedure SetStencilOp(fail, zfail, zpass:StencilOperation); Override;
      Procedure SetStencilFunction(Mode:CompareMode; StencilID:Cardinal; Mask:Cardinal = $FFFFFFFF);  Override;

      Procedure SetColorMask(Red, Green, Blue, Alpha:Boolean); Override;

      Procedure SetDepthMask(WriteZ:Boolean); Override;
      Procedure SetDepthTest(Enable:Boolean); Override;
      Procedure SetDepthFunction(Mode:CompareMode); Override;

      Procedure SetCullMode(Mode:CullMode); Override;

      Procedure SetBlendMode(BlendMode:Integer); Override;

      Procedure SetProjectionMatrix(Const Mat:Matrix4x4); Override;
      Procedure SetModelMatrix(Const Mat:Matrix4x4);  Override;
      Procedure SetTextureMatrix(Const Mat:Matrix4x4); Override;

      Procedure SetScissorState(Enabled:Boolean); Override;
      Procedure SetScissorArea(X,Y, Width, Height:Integer); Override;

      Procedure SetViewport(X,Y, Width, Height:Integer); Override;

      Procedure SetAttributeSource(Const Name:TERRAString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer); Override;

      Procedure SetDiffuseColor(Const C:ColorRGBA); Override;

      Procedure DrawSource(Primitive:RenderPrimitive; Count:Integer); Override;
      Procedure DrawIndexedSource(Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord); Override;
    Public

  End;

Implementation
Uses TERRA_Log, TERRA_Application, TERRA_GraphicsManager, TERRA_FileManager, TERRA_FileUtils, TERRA_FileStream, TERRA_Error;

{ NullFeatures }
Constructor NullFeatures.Create(Owner:GraphicsRenderer);
Begin
  Inherited Create(Owner);

  _MaxTextureUnits := 8;
	_MaxTextureSize := 4096;
	_maxRenderTargets := 4;
  _maxAnisotrophy := 0;
  _multiSampleCount := 0;

  _VertexCacheSize := 32;

  TextureCompression.Avaliable :=  False;

  Shaders.Avaliable := True;

	VertexBufferObject.Avaliable := True;
  FrameBufferObject.Avaliable := True;
  PostProcessing.Avaliable := True;

  CubeMapTexture.Avaliable := True;
  SeparateBlends.Avaliable := True;
  SeamlessCubeMap.Avaliable := True;
  NPOT.Avaliable := True;
  PackedStencil.Avaliable := True;

  _MaxUniformVectors := 1024;

  TextureArray.Avaliable := False;

  FloatTexture.Avaliable := True;

  DeferredLighting.Avaliable := True;

  StencilBuffer.Avaliable := True;

End;

{ NullRenderer }
Procedure NullRenderer.ClearBuffer(Color, Depth, Stencil:Boolean);
Begin
End;

Procedure NullRenderer.SetClearColor(const ClearColor:ColorRGBA);
Begin
End;


Function NullRenderer.Initialize():Boolean;
Begin
  _Features := NullFeatures.Create(Self);

  _DeviceName := 'Null Renderer';
  _DeviceVendor := 'TERRA';
  _DeviceVersion := StringToVersion('2.0.0');

  Result := True;
End;

Procedure NullRenderer.SetColorMask(Red, Green, Blue, Alpha: Boolean);
Begin
End;

Procedure NullRenderer.SetDepthMask(WriteZ: Boolean);
Begin
End;

Procedure NullRenderer.SetDepthFunction(Mode: CompareMode);
Begin
End;

Procedure NullRenderer.SetStencilTest(Enable: Boolean);
Begin
End;

Procedure NullRenderer.SetStencilFunction(Mode: CompareMode; StencilID, Mask: Cardinal);
Begin
End;

Procedure NullRenderer.SetStencilOp(fail, zfail, zpass: StencilOperation);
Begin
End;

Procedure NullRenderer.SetCullMode(Mode: CullMode);
Begin
End;

Procedure NullRenderer.SetDepthTest(Enable: Boolean);
Begin
End;

Procedure NullRenderer.SetBlendMode(BlendMode: Integer);
Begin
End;

Procedure NullRenderer.SetModelMatrix(Const Mat: Matrix4x4);
Begin
End;

Procedure NullRenderer.SetProjectionMatrix(Const Mat:Matrix4x4);
Begin
End;

Procedure NullRenderer.SetTextureMatrix(Const Mat: Matrix4x4);
Begin
End;

Procedure NullRenderer.SetDiffuseColor(Const C:ColorRGBA);
Begin
End;

Procedure NullRenderer.SetAttributeSource(Const Name:TERRAString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer);
Begin
End;

Procedure NullRenderer.DrawSource(Primitive: RenderPrimitive; Count: Integer);
Begin
End;

Procedure NullRenderer.DrawIndexedSource(Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord);
Begin
End;

Procedure NullRenderer.SetViewport(X, Y, Width, Height:Integer);
Begin
End;

Procedure NullRenderer.ResetState();
Begin
End;


Procedure NullRenderer.SetScissorArea(X,Y, Width, Height:Integer);
Begin
End;

Procedure NullRenderer.SetScissorState(Enabled: Boolean);
Begin
End;

Function NullRenderer.CreateCubeMap: CubeMapInterface;
Begin
  Result := NullCubeMap.Create(Self);
End;

Function NullRenderer.CreateRenderTarget: RenderTargetInterface;
Begin
  Result := NullFBO.Create(Self);
End;

Function NullRenderer.CreateTexture: TextureInterface;
Begin
  Result := NullTexture.Create(Self);
End;

Function NullRenderer.CreateVertexBuffer: VertexBufferInterface;
Begin
  Result := NullVBO.Create(Self);
End;

Function NullRenderer.CreateShader: ShaderInterface;
Begin
  Result := NullShader.Create(Self);
End;

{ NullVBO }
Function NullVBO.Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean;
Var
  Index:Single;
  I, N:Integer;
  Flags:Integer;
  P:Pointer;
Begin
  Self._Vertices := Vertices;
  Self._IndexList := IndexData;
  Self._EdgeList := EdgeData;
  Self._Dynamic := DynamicUsage;
  Self._TriangleCount := TriangleCount;
  Self._EdgeCount := 0;
  Self._WireframeIndices := Nil;
  Result := True;
End;

Procedure NullVBO.Invalidate;
Begin
End;

Procedure NullVBO.Submit(Wireframe: Boolean);
Begin
End;

{ NullCubeMap }
Function NullCubeMap.Bind(Slot: Integer):Boolean;
Begin
  Result := True;
End;

Function NullCubeMap.Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType): Boolean;
Begin
  _Width := Width;
  _Height := Height;

  Self.SetWrapMode(wrapNothing);
  Self.MipMapped := True;
  Self.SetFilter(filterBilinear);

  Result := True;
End;

Procedure NullCubeMap.Invalidate;
Begin
End;

{ NullFBO }
Function NullFBO.Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer,StencilBuffer:Boolean):Boolean;
Begin
  Self._SizeInBytes := Width * Height * 4 * 2;

  _BackgroundColor := ColorCreate(Byte(0), Byte(0), Byte(0), Byte(0));

  _PixelSize := PixelSize;

	_Width := Width;
	_Height := Height;

  Result := True;
End;

Procedure NullFBO.BeginCapture(Flags: Cardinal);
Begin

End;

Procedure NullFBO.EndCapture;
Begin
End;

Procedure NullFBO.Resize(NewWidth, NewHeight:Integer);
Begin
  _Width := NewWidth;
  _Height := NewHeight;
End;

Function NullFBO.Bind(Slot: Integer):Boolean;
Begin
  Result := True;
End;

Function NullFBO.GetImage():TERRAImage;
Begin
  Result := TERRAImage.Create(_Width, _Height);
End;

Function NullFBO.GetPixel(X,Y:Integer):ColorRGBA;
Begin
  Result := ColorNull;
End;

Procedure NullFBO.Invalidate;
Begin
End;

{ NullTexture }
Function NullTexture.Bind(Slot: Integer): Boolean;
Begin
  Result := True;
End;

Function NullTexture.Generate(Pixels: Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType): Boolean;
Begin
  _Width := Width;
  _Height := Height;
  _SizeInBytes := Trunc(4 * _Width * _Height);
  Result := True;
End;

Function NullTexture.GetImage:TERRAImage;
Begin
  Result := TERRAImage.Create(_Width, _Height);
End;


Procedure NullTexture.Invalidate;
Begin
End;

{ NullShader }
Function NullShader.Bind:Boolean;
Begin
  Result := True;
End;

Function NullShader.GetAttributeHandle(const Name: TERRAString): Integer;
Begin
  Result := 1;
End;

Function NullShader.GetUniform(Name: TERRAString): Integer;
Begin
  Result := 2;
End;


Procedure NullShader.Invalidate;
Begin
  _VertexShaderHandle := 0;
  _FragmentShaderHandle := 0;
  _Program := 0;

  _AttributeCount := 0;
//  _Status := rsUnloaded;
  _Linked := False;
End;

Function NullShader.IsReady: Boolean;
Begin
  Result := _Linked;
End;

Procedure NullShader.SetFloatUniform(const Name: TERRAString; const Value: Single);
Begin
End;

Procedure NullShader.SetIntegerUniform(const Name: TERRAString; const Value: Integer);
Begin
End;

Procedure NullShader.SetMat3Uniform(const Name: TERRAString; Value: Matrix3x3);
Begin
End;

Procedure NullShader.SetMat4Uniform(const Name: TERRAString; Value: Matrix4x4);
Begin
End;

Procedure NullShader.SetVec2Uniform(const Name: TERRAString; const Value: Vector2D);
Begin
End;

Procedure NullShader.SetVec3Uniform(const Name: TERRAString; const Value: Vector3D);
Begin
End;

Procedure NullShader.SetVec4Uniform(const Name: TERRAString; const Value:Vector4D);
Begin
End;

Procedure NullShader.SetVec4ArrayUniform(const Name: TERRAString; Count:Integer; Values: Array Of Vector4D);
Begin
End;

Function NullShader.Generate(const Name: TERRAString; Shader:TERRAShaderGroup): Boolean;
Begin
  Result := True;
End;


Function NullShader.Unbind: Boolean;
Begin
  Result := True;
End;

End.
