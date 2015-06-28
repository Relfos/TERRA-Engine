Unit TERRA_GLES2Renderer;

{$I terra.inc}
Interface

Uses
  TERRA_String, TERRA_Utils, TERRA_Renderer, TERRA_VertexFormat,
  TERRA_OpenGLES, 
  TERRA_Color, TERRA_Image, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D,
  TERRA_Matrix3x3, TERRA_Matrix4x4,
  TERRA_OpenGLCommon;

Type
  OpenGLES2Features = Class(RendererFeatures)
    Public
      Constructor Create(Owner:GraphicsRenderer);
  End;

  OpenGLES2VBO = OpenGLVBO;

  OpenGLES2Texture = Class(TextureInterface)
    Protected
      _Handle:Cardinal;
      _ShouldGenMips:Boolean;

    Public
      Function Generate(Pixels:Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;
      Procedure Release(); Override;

      Function Bind(Slot:Integer):Boolean; Override;

      Procedure SetFilter(Value:TextureFilterMode); Override;
      Procedure SetWrapMode(Value:TextureWrapMode); Override;
      Procedure SetMipMapping(Value:Boolean); Override;

      Function Update(Pixels:Pointer; X,Y, Width, Height:Integer):Boolean; Override;

      Function GetImage():Image; Override;

      Procedure Invalidate(); Override;
  End;

  OpenGLES2CubeMap = Class(CubeMapInterface)
    Protected
      _Handle:Cardinal;
      _ShouldGenMips:Boolean;

    Public
      Procedure Release(); Override;

      Function Bind(Slot:Integer):Boolean; Override;

      Procedure SetFilter(Value:TextureFilterMode); Override;
      Procedure SetWrapMode(Value:TextureWrapMode); Override;
      Procedure SetMipMapping(Value:Boolean); Override;

      Function Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;

      Function UpdateFace(FaceID:Integer; Pixels:Pointer; X,Y, Width, Height:Integer):Boolean; Override;

      Procedure Invalidate(); Override;
  End;

  OpenGLES2FBO = Class(RenderTargetInterface)
    Protected
	    _Handle:Cardinal;
	    _mfb:Cardinal;
	    _color_rb:Cardinal;
	    _depth_rb:Cardinal;
        _stencil_rb:Cardinal;
  	  _targets:Array Of Cardinal;
      _targetCount:Integer;
	    _internalformat:Cardinal;
	    _multisample:Boolean;
      _drawBuffers:Array Of Cardinal;
      _hasDepthBuffer:Boolean;
      _hasStencilBuffer:Boolean;
      _Shared:Boolean;

	    _type:PixelSizeType;

      _Complete:Boolean;

  	  // Create a render texture
	    Function Init():Boolean;

      Function GetErrorString(Code:Cardinal):TERRAString;

      Function GetOrigin: SurfaceOrigin; Override;

    Public
      Function Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer, StencilBuffer:Boolean):Boolean; Override;

  	  // Free OpenGLES2 memory
	    Procedure Release(); Override;

      Function Bind(Slot:Integer):Boolean; Override;

      Procedure SetFilter(Value:TextureFilterMode); Override;
      Procedure SetWrapMode(Value:TextureWrapMode); Override;
      Procedure SetMipMapping(Value:Boolean); Override;

	    // Render to this target
	    Procedure BeginCapture(Flags:Cardinal); Override;
	    Procedure EndCapture; Override;

      Procedure Resize(NewWidth, NewHeight:Integer); Override;

      Function GetImage():Image; Override;
      Function GetPixel(X,Y:Integer):Color; Override;

      Procedure Invalidate(); Override;

      {$IFDEF IPHONE}
      Procedure PresentToScreen(); Override;
      {$ENDIF}
  End;

  OpenGLES2Shader = OpenGLShader;

  OpenGLES2Renderer = Class(GraphicsRenderer)
    Protected
      _UsedTextures:Array[0..Pred(MaxTextureHandles)] Of Boolean;
      _UsedFrameBuffers:Array[0..Pred(MaxFrameBufferHandles)] Of Boolean;
      _UsedRenderBuffers:Array[0..Pred(MaxFrameBufferHandles)] Of Boolean;

      Function GenerateTexture():Cardinal;
      Function GenerateFrameBuffer():Cardinal;
      Function GenerateRenderBuffer():Cardinal;

      Procedure DeleteTexture(Var Handle:Cardinal);
      Procedure DeleteFrameBuffer(Var Handle:Cardinal);
      Procedure DeleteRenderBuffer(Var Handle:Cardinal);

      Function CreateContext():Boolean; Override;
      Procedure DestroyContext(); Override;

      Function Initialize():Boolean; Override;

      Procedure ApplyTextureFilter(Handle, TextureKind:Integer; MipMapped, ShouldGenMips:Boolean; Filter:TextureFilterMode);
      Procedure ApplyTextureWrap(Handle, TextureKind:Integer; WrapMode:TextureWrapMode);

    Public
      Procedure ResetState(); Override;
      Procedure BeginFrame(); Override;
      Procedure EndFrame(); Override;

      Function CreateTexture():TextureInterface; Override;
      Function CreateCubeMap():CubeMapInterface; Override;
      Function CreateVertexBuffer():VertexBufferInterface; Override;
      Function CreateShader():ShaderInterface; Override;
      Function CreateRenderTarget():RenderTargetInterface; Override;

      Procedure ClearBuffer(Color, Depth, Stencil:Boolean); Override;
      Procedure SetClearColor(Const ClearColor:Color); Override;

      Procedure SetStencilTest(Enable:Boolean); Override;
      Procedure SetStencilFunction(Mode:CompareMode; StencilID, Mask:Cardinal); Override;
      Procedure SetStencilOp(fail, zfail, zpass:StencilOperation); Override;

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

      Procedure SetAttributeSource(Const Name:AnsiString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer); Override;

      Procedure SetDiffuseColor(Const C:Color); Override;

      Procedure DrawSource(Primitive:RenderPrimitive; Count:Integer); Override;
      Procedure DrawIndexedSource(Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord); Override;
    Public

  End;

Implementation
Uses TERRA_Log, TERRA_Application, TERRA_GraphicsManager, TERRA_FileManager, TERRA_FileUtils, 
  TERRA_Error, TERRA_OS;

(*
{$IFDEF IPHONE}
Procedure SetRenderbufferStorage(); cdecl; external;
Procedure PresentRenderBuffer(); cdecl; external;
{$ENDIF}
*)

{ OpenGLES2Features }
Constructor OpenGLES2Features.Create(Owner:GraphicsRenderer);
Var
  HasShaders:Boolean;
  S:AnsiString;
Begin
  Inherited Create(Owner);

  glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @_MaxTextureUnits);

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, @_MaxTextureSize);

	_maxRenderTargets := 0;

  _maxAnisotrophy := 0;

  If (glExtensionSupported('GL_ARB_multisample')) Then
    _multiSampleCount := 4
  Else
    _multiSampleCount := 0;

  _VertexCacheSize := 32;

  TextureCompression.Avaliable :=  True;

  {$IFDEF DISABLESHADERS}
  HasShaders := False;
  {$ELSE}
  HasShaders := True;
  {$ENDIF}

  Shaders.Avaliable := HasShaders;

	VertexBufferObject.Avaliable := True; //glExtensionSupported('GL_ARB_vertex_buffer_object');

  {$IFDEF FRAMEBUFFEROBJECTS}
  FrameBufferObject.Avaliable := True;
  {$ELSE}
  FrameBufferObject.Avaliable := False;
  {$ENDIF}


  {$IFDEF POSTPROCESSING}
  PostProcessing.Avaliable := (FrameBufferObject.Avaliable) And (MaxRenderTargets>=4);
  {$ELSE}
  PostProcessing.Avaliable := False;
  {$ENDIF}

  CubeMapTexture.Avaliable := glExtensionSupported('GL_ARB_texture_cube_map');
  SeparateBlends.Avaliable := glExtensionSupported('GL_EXT_draw_buffers2');
  SeamlessCubeMap.Avaliable := glExtensionSupported('GL_ARB_seamless_cube_map') Or glExtensionSupported('GL_AMD_seamless_cubemap_per_texture');

  NPOT.Avaliable := HasShaders;

  PackedStencil.Avaliable := glExtensionSupported('GL_OES_packed_depth_stencil');

  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_VECTORS, @_MaxUniformVectors);
  If (_MaxUniformVectors<128) Then
    VertexBufferObject.Avaliable := False;

  TextureArray.Avaliable := glExtensionSupported('GL_EXT_texture_array');

  FloatTexture.Avaliable := glExtensionSupported('GL_ARB_color_buffer_float') Or
                            glExtensionSupported('GL_ATI_pixel_format_float') Or
                            glExtensionSupported('GL_NV_float_buffer');

  DeferredLighting.Avaliable := (MaxRenderTargets>=4) And (FrameBufferObject.Avaliable);

  StencilBuffer.Avaliable := PackedStencil.Avaliable Or glExtensionSupported('GL_OES_stencil8');

  S := glGetExtensionString();
  TERRA_Log.Log(logDebug, 'Renderer', 'Extensions: '+ S);
End;


Function StencilOpToGL(Op:StencilOperation):Integer;
Begin
  Case Op Of
  stencilKeep:      Result := GL_KEEP;
  stencilReplace:   Result := GL_REPLACE;
  stencilIncrement: Result := GL_INCR;
  stencilDecrement: Result := GL_DECR;
  stencilInvert:    Result := GL_INVERT;
  stencilIncrementWithWrap: Result := GL_INCR_WRAP;
  stencilDecrementWithWrap: Result := GL_DECR_WRAP;
  End;
End;

Function PrimitiveToGL(Primitive:RenderPrimitive):Integer;
Begin
  Case Primitive Of
  renderPoints:     Result := GL_POINTS;
  renderLines:      Result := GL_LINES;
  renderTriangles:  Result := GL_TRIANGLES;
  renderLineStrip:  Result := GL_LINE_STRIP;
  renderTriangleStrip: Result := GL_TRIANGLE_STRIP;
  End;
End;

Function TextureColorFormatToGL(Format:TextureColorFormat):Integer;
Begin
  Case Format Of
  colorRGB:   Result := GL_RGB;
  colorRGBA:  Result := GL_RGBA;
  colorBGR:   Result := GL_RGB;
  colorBGRA:   Result := GL_RGBA;
  colorAlpha: Result := GL_ALPHA; //GL_LUMINANCE;
  End;
End;

Function DataFormatToGL(Format:DataFormat):Integer;
Begin
  Case Format Of
  typeColor:  Result := GL_UNSIGNED_BYTE;
  typeByte:   Result := GL_UNSIGNED_BYTE;
  typeFloat:    Result:= GL_FLOAT;
  typeVector2D: Result:= GL_FLOAT;
  typeVector3D: Result := GL_FLOAT;
  typeVector4D: Result := GL_FLOAT;
    Else
      Result := 0;
  End;
End;

Function ByteFormatToGL(ByteFormat:PixelSizeType):Integer;
Begin
  Case ByteFormat Of
  pixelSizeByte: Result := GL_UNSIGNED_BYTE;
(*  //pixelSizeFloat:
  ,
                    GL_UNSIGNED_SHORT_5_6_5,
                    GL_UNSIGNED_SHORT_4_4_4_4, and
                    GL_UNSIGNED_SHORT_5_5_5_1.*)
    Else
      Result := 0;
  End;
End;


{ OpenGLES2Renderer }
Procedure OpenGLES2Renderer.ClearBuffer(Color, Depth, Stencil:Boolean);
Var
  Flags:Cardinal;
Begin
  Flags := 0;

  If (Color) Then
    Flags := Flags Or GL_COLOR_BUFFER_BIT;

  If (Depth) Then
    Flags := Flags Or GL_DEPTH_BUFFER_BIT;

  If (Stencil) Then
    Flags := Flags Or GL_STENCIL_BUFFER_BIT;

  If (Flags<>0) Then
    glClear(Flags);
End;

Procedure OpenGLES2Renderer.SetClearColor(const ClearColor: Color);
Begin
  glClearColor(ClearColor.R/255, ClearColor.G/255, ClearColor.B/255, ClearColor.A/255);
End;

Function OpenGLES2Renderer.CreateContext: Boolean;
Begin
  Result:=False;

  LoadOpenGL();

  Result := True;
End;

Procedure OpenGLES2Renderer.DestroyContext();
Begin
End;

Function OpenGLES2Renderer.Initialize():Boolean;
Var
  I:Integer;
  S:AnsiString;
Begin
  Result := CreateContext();

  If Not Result Then
    Exit;

  _Features := OpenGLES2Features.Create(Self);

  _DeviceName := glGetString(GL_RENDERER);
  _DeviceVendor := glGetString(GL_VENDOR);

  _DeviceVersion := StringToVersion('0.0.0');

  Result := True;
End;

Procedure OpenGLES2Renderer.SetColorMask(Red, Green, Blue, Alpha: Boolean);
Begin
  glColorMask(Red, Green, Blue, Alpha);
End;

Procedure OpenGLES2Renderer.SetDepthMask(WriteZ: Boolean);
Begin
  glDepthMask(WriteZ);
End;

Procedure OpenGLES2Renderer.SetDepthFunction(Mode: CompareMode);
Begin
  glDepthFunc(CompareToGL(Mode));
End;

Procedure OpenGLES2Renderer.SetStencilTest(Enable: Boolean);
Begin
  If Enable Then
    glEnable(GL_STENCIL_TEST)
  Else
    glDisable(GL_STENCIL_TEST);
End;

Procedure OpenGLES2Renderer.SetStencilFunction(Mode: CompareMode; StencilID, Mask: Cardinal);
Begin
  glStencilFunc(CompareToGL(Mode), StencilID, $FFFFFFFF);
End;

Procedure OpenGLES2Renderer.SetStencilOp(fail, zfail, zpass: StencilOperation);
Begin
  glStencilOp(StencilOpToGL(Fail), StencilOpToGL(ZFail), StencilOpToGL(ZPass));
End;


Procedure OpenGLES2Renderer.SetCullMode(Mode: CullMode);
Begin
  If (Mode = cullNone) Then
  Begin
    glDisable(GL_CULL_FACE);
  End Else
  Begin
    glEnable(GL_CULL_FACE);
    If (Mode = cullFront) Then
      glCullFace(GL_FRONT)
    Else
      glCullFace(GL_BACK);
  End;
End;

Procedure OpenGLES2Renderer.SetDepthTest(Enable: Boolean);
Begin
  If Enable Then
    glEnable(GL_DEPTH_TEST)
  Else
    glDisable(GL_DEPTH_TEST);
End;

Procedure OpenGLES2Renderer.SetBlendMode(BlendMode: Integer);
Var
  NeedsAlpha:Boolean;
Begin
{glEnable(GL_BLEND);
glBlendFunc(GL_ONE, GL_ONE);
exit;}

{  If (BlendMode = _CurrentBlendMode) Then
    Exit;}

  NeedsAlpha := BlendMode>0;

  (*If (Settings.SeparateBlends.Avaliable) And (Settings.PostProcessing.Avaliable)
  And (Shader.ActiveShader<>Nil) And (Shader.ActiveShader.MRT) Then
  Begin
    If NeedsAlpha Then
      glEnableIndexedEXT(GL_BLEND, 0)
    Else
      glDisableIndexedEXT(GL_BLEND, 0);

  End Else*)
  Begin
    If NeedsAlpha Then
    Begin
      glEnable(GL_BLEND);
    End Else
    Begin
      glDisable(GL_BLEND);
    End;

  End;


  If (NeedsAlpha) Then
  Case BlendMode Of
  blendBlend:   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  blendAdd:     glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  blendFilter:  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  blendModulate:glBlendFunc(GL_SRC_COLOR, GL_ONE);
  blendJoin:    glBlendFunc(GL_ONE, GL_ONE);
  blendZero:    glBlendFunc(GL_ZERO, GL_ZERO);
  blendOne:     glBlendFunc(GL_ONE, GL_ZERO);
  blendColor:   glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);
  blendColorAdd:   glBlendFunc(GL_SRC_COLOR, GL_ONE);
  blendReflection:   glBlendFunc(GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA);
  End;

 //_CurrentBlendMode := BlendMode;
End;

Procedure OpenGLES2Renderer.SetModelMatrix(Const Mat: Matrix4x4);
Begin
  _ModelMatrix := Mat;

  If Assigned(Self.ActiveShader) Then
    Self.ActiveShader.SetMat4Uniform('modelMatrix', Mat);
End;

Procedure OpenGLES2Renderer.SetProjectionMatrix(Const Mat:Matrix4x4);
Begin
  _ProjectionMatrix := Mat;

  If Assigned(Self.ActiveShader) Then
    Self.ActiveShader.SetMat4Uniform('projectionMatrix', Mat);
End;

Procedure OpenGLES2Renderer.SetTextureMatrix(Const Mat: Matrix4x4);
Begin
  _TextureMatrix := Mat;

  If Assigned(Self.ActiveShader) Then
    Self.ActiveShader.SetMat4Uniform('textureMatrix', Mat);
End;

Procedure OpenGLES2Renderer.SetDiffuseColor(Const C: Color);
Begin
  _DiffuseColor := C;

  If Assigned(Self.ActiveShader) Then
    Self.ActiveShader.SetColorUniform('diffuse', C);
End;

Procedure OpenGLES2Renderer.SetAttributeSource(Const Name:AnsiString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer);
Var
  Count, Format:Integer;
  Norm:Boolean;
  Handle:Integer;
Begin
  Format := DataFormatToGL(ElementType);
  Case ElementType Of
  typeColor:
    Begin
      Count := 4;
      Norm := True;
    End;

  typeByte:
    Begin
      Count := 1;
      Norm := True;
    End;

  typeFloat:
    Begin
      Count := 1;
      Norm := False;
    End;

  typeVector2D:
    Begin
      Count := 2;
      Norm := False;
    End;

  typeVector3D:
    Begin
      Count := 3;
      Norm := False;
    End;

  typeVector4D:
    Begin
      Count := 4;
      Norm := False;
    End;

    Else
      Exit;
  End;

  If _CurrentSource = Nil Then
  Begin
    RaiseError('Please call GraphicsRenderer.SetVertexSize() before drawing anything!');
    Exit;
  End;

  If Self.ActiveShader = Nil Then
    Exit;

  Handle := Self.ActiveShader.GetAttributeHandle(Name);

  If (Handle<0) Then
    Exit;

  glVertexAttribPointer(Handle, Count, Format, Norm, _CurrentSource.Size, AttributeSource);
End;

Procedure OpenGLES2Renderer.DrawSource(Primitive: RenderPrimitive; Count: Integer);
Begin
  If (Count<0) Then
    Exit;

  If Assigned(_CurrentSource) Then
    _CurrentSource.Bind(True)
  Else
    RaiseError('Cannot draw null buffer!');

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Renderer', 'glDrawArrays: '+IntToString(Count));
  {$ENDIF}

  glDrawArrays(PrimitiveToGL(Primitive), 0, Count);
  Inc(_Stats.TriangleCount, Count Div 3);

{  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);}

  _CurrentSource := Nil;
End;

Procedure OpenGLES2Renderer.DrawIndexedSource(Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord);
Begin
  If (Count<0) Then
    Exit;

  If Assigned(_CurrentSource) Then
    _CurrentSource.Bind(True)
  Else
    RaiseError('Cannot draw null buffer!');

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Renderer', 'glDrawElements: '+IntToString(Count));
  {$ENDIF}

  glDrawElements(PrimitiveToGL(Primitive), Count, GL_UNSIGNED_SHORT, Indices);

  Inc(_Stats.TriangleCount, Count Div 3);
End;

Procedure OpenGLES2Renderer.SetViewport(X, Y, Width, Height:Integer);
Begin
  glViewport(X,Y, Width, Height);
End;

Procedure OpenGLES2Renderer.ResetState();
Begin
	glEnable(GL_CULL_FACE);

  glEnable(GL_DEPTH_TEST);
	//glDisable(GL_FOG);
	//glEnable(GL_LIGHTING);

  //glDisable(GL_ALPHA_TEST);
  //glDisable(GL_LINE_SMOOTH);
  //glClearDepth(1.0);
  glClearStencil(0);
  glStencilMask($FFFFFFF);
  glDepthFunc(GL_LEQUAL);
End;


Function OpenGLES2Renderer.GenerateFrameBuffer: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new frame buffer...');
  Repeat
    glGenFramebuffers(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxFrameBufferHandles) Or (Not _UsedFrameBuffers[Result]);

  If (Result<MaxFrameBufferHandles) Then
    _UsedFrameBuffers[Result] := True;
End;

Function OpenGLES2Renderer.GenerateRenderBuffer: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new render buffer...');
  Repeat
    glGenRenderbuffers(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxFrameBufferHandles) Or (Not _UsedRenderBuffers[Result]);

  If (Result<MaxFrameBufferHandles) Then
    _UsedRenderBuffers[Result] := True;
End;

Function OpenGLES2Renderer.GenerateTexture: Cardinal;
Begin
  Log(logDebug, 'GraphicsManager', 'Generating a new texture...');
  Repeat
    glGenTextures(1, @Result);
    Log(logDebug, 'GraphicsManager', 'Got handle: '+IntToString(Result));
  Until (Result>=MaxTextureHandles) Or (Not _UsedTextures[Result]);

  If (Result<MaxTextureHandles) Then
    _UsedTextures[Result] := True;
End;

Procedure OpenGLES2Renderer.DeleteFrameBuffer(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteFramebuffers(1, @Handle);
  If (Handle < MaxFrameBufferHandles) Then
    _UsedFrameBuffers[Handle] := False;

  Handle := 0;
End;

Procedure OpenGLES2Renderer.DeleteRenderBuffer(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteRenderbuffers(1, @Handle);
  If (Handle < MaxFrameBufferHandles) Then
    _UsedRenderBuffers[Handle] := False;

  Handle := 0;
End;

Procedure OpenGLES2Renderer.DeleteTexture(Var Handle: Cardinal);
Begin
  If (Handle<=0) Then
    Exit;

  glDeleteTextures(1, @Handle);
  If (Handle < MaxTextureHandles) Then
    _UsedTextures[Handle] := False;

  Handle := 0;
End;

Procedure OpenGLES2Renderer.SetScissorArea(X,Y, Width, Height:Integer);
Begin
  glScissor(X, Y, Width, Height);
End;

Procedure OpenGLES2Renderer.SetScissorState(Enabled: Boolean);
Begin
  If Enabled Then
    glEnable(GL_SCISSOR_TEST)
  Else
    glDisable(GL_SCISSOR_TEST);
End;

Procedure OpenGLES2Renderer.ApplyTextureFilter(Handle, TextureKind:Integer; MipMapped, ShouldGenMips:Boolean; Filter:TextureFilterMode);
Begin
  If Handle = 0 Then
    Exit;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting texture filtering');{$ENDIF}

  If (Filter = filterBilinear) Then
  Begin
    If (MipMapped) Then
      glTexParameteri(TextureKind, GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
    Else
      glTexParameteri(TextureKind, GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(TextureKind, GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  End Else
  Begin
    If (MipMapped) Then
      glTexParameteri(TextureKind, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST)
    Else
      glTexParameteri(TextureKind, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(TextureKind, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Generating mipmap');{$ENDIF}
  If (MipMapped) And (ShouldGenMips) Then
  Begin
    glGenerateMipmap(TextureKind);
  End;
End;

Procedure OpenGLES2Renderer.ApplyTextureWrap(Handle, TextureKind:Integer; WrapMode:TextureWrapMode);
Begin
  If Handle = 0 Then
    Exit;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting wrap mode');{$ENDIF}

  If ((Cardinal(WrapMode) And Cardinal(wrapHorizontal))<>0) Then
    glTexParameteri(TextureKind, GL_TEXTURE_WRAP_S, GL_REPEAT)
  Else
    glTexParameteri(TextureKind, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);

  If ((Cardinal(WrapMode) And Cardinal(wrapVertical))<>0) Then
    glTexParameteri(TextureKind, GL_TEXTURE_WRAP_T, GL_REPEAT)
  Else
    glTexParameteri(TextureKind, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
End;

Function OpenGLES2Renderer.CreateCubeMap: CubeMapInterface;
Begin
  Result := OpenGLES2CubeMap.Create(Self);
End;

Function OpenGLES2Renderer.CreateRenderTarget: RenderTargetInterface;
Begin
  Result := OpenGLES2FBO.Create(Self);
End;

Function OpenGLES2Renderer.CreateTexture: TextureInterface;
Begin
  Result := OpenGLES2Texture.Create(Self);
End;

Function OpenGLES2Renderer.CreateVertexBuffer: VertexBufferInterface;
Begin
  Result := OpenGLES2VBO.Create(Self);
End;

Function OpenGLES2Renderer.CreateShader: ShaderInterface;
Begin
  Result := OpenGLES2Shader.Create(Self);
End;

Procedure OpenGLES2Renderer.BeginFrame;
Begin
  Inherited;

  //glClearColor(_BackgroundColor.R/255, _BackgroundColor.G/255, _BackgroundColor.B/255, 0{_BackgroundColor.A/255});
End;

Procedure OpenGLES2Renderer.EndFrame();
Begin
  Inherited;
End;

{ OpenGLES2CubeMap }
Function OpenGLES2CubeMap.Bind(Slot: Integer):Boolean;
Begin
  glActiveTexture(GL_TEXTURE0 + Slot);

  Result := (_Handle>0) And (Self.IsValid());
  If Not Result Then
  Begin
    glBindTexture(GL_TEXTURE_CUBE_MAP, 0);
    Exit;
  End;

  glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);
  Result := True;
End;

Function OpenGLES2CubeMap.Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType): Boolean;
Begin
  _Width := Width;
  _Height := Height;

  _Handle := OpenGLES2Renderer(_Owner).GenerateTexture();
  glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);

  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, TextureColorFormatToGL(TargetFormat),	_Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, TextureColorFormatToGL(TargetFormat),	_Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, TextureColorFormatToGL(TargetFormat),	_Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, TextureColorFormatToGL(TargetFormat),	_Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, TextureColorFormatToGL(TargetFormat),	_Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, TextureColorFormatToGL(TargetFormat), _Width, _Height, 0,	TextureColorFormatToGL(SourceFormat), GL_UNSIGNED_BYTE, Nil);

  _ShouldGenMips := True;

  Self.SetWrapMode(wrapNothing);
  Self.MipMapped := True;
  Self.SetFilter(filterBilinear);

  Result := True;
End;

Function OpenGLES2CubeMap.UpdateFace(FaceID: Integer; Pixels: Pointer; X, Y, Width, Height: Integer):Boolean;
Var
  N:Integer;
Begin
  Case FaceID Of
  cubemap_PositiveX: N := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
  cubemap_NegativeX: N := GL_TEXTURE_CUBE_MAP_NEGATIVE_X;

  cubemap_PositiveY: N := GL_TEXTURE_CUBE_MAP_POSITIVE_Y;
  cubemap_NegativeY: N := GL_TEXTURE_CUBE_MAP_NEGATIVE_Y;

  cubemap_PositiveZ: N := GL_TEXTURE_CUBE_MAP_POSITIVE_Z;
  cubemap_NegativeZ: N := GL_TEXTURE_CUBE_MAP_NEGATIVE_Z;
  End;

  glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);
  glTexImage2D(N, 0, GL_RGBA8,	_Width, _Height, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Pixels);

  _ShouldGenMips := True;

  Result := True;
End;

Procedure OpenGLES2CubeMap.Invalidate;
Begin
  _Handle := 0;
End;

Procedure OpenGLES2CubeMap.Release();
Begin
  OpenGLES2Renderer(_Owner).DeleteTexture(_Handle);
End;

Procedure OpenGLES2CubeMap.SetFilter(Value: TextureFilterMode);
Begin
  Self._Filter := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureFilter(_Handle, GL_TEXTURE_CUBE_MAP, MipMapped, _ShouldGenMips, Filter);
  _ShouldGenMips := False;
End;

procedure OpenGLES2CubeMap.SetMipMapping(Value: Boolean);
Begin
  Self._MipMapped := Value;
End;

procedure OpenGLES2CubeMap.SetWrapMode(Value: TextureWrapMode);
Begin
  Self._WrapMode := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureWrap(_Handle, GL_TEXTURE_CUBE_MAP, WrapMode);
End;

{ OpenGLES2FBO }
Function OpenGLES2FBO.Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer,StencilBuffer:Boolean):Boolean;
Var
  I:Integer;
Begin
  Self._Size := Width * Height * 4 * 2;

  _BackgroundColor := ColorCreate(Byte(0), Byte(0), Byte(0), Byte(0));

  _PixelSize := PixelSize;

	_Handle := 0;
	_color_rb := 0;
	_depth_rb := 0;

//  _ContextID := Application.Instance.ContextID;

  If (Multisample) And (_Owner.Features.MultiSampleCount<=0) Then
    Multisample := False;

  _TargetCount := TargetCount;
  SetLength(_Targets, _TargetCount);
  SetLength(_DrawBuffers, _TargetCount);
  For I:=0 To Pred(_TargetCount) Do
  Begin
    _targets[I] := 0;
    _DrawBuffers[I] := GL_COLOR_ATTACHMENT0 + I;
  End;

  _Shared := False;

	_Width := Width;
	_Height := Height;
	_type := PixelSize;
	_multisample := multisample;
  _hasDepthBuffer := DepthBuffer;
  _HasStencilBuffer := StencilBuffer;

  Log(logDebug,'Framebuffer', 'Creating Framebuffer with size: '+IntToString(_Width)+' x '+IntToString(_Height));

  _internalformat := GL_RGBA8;

  Result := Self.Init();
End;


Function OpenGLES2FBO.GetErrorString(Code: Cardinal): TERRAString;
Begin
	Case Code Of
		GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
        Result := 'Framebuffer incomplete: Attachment is NOT complete.';

		GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
        Result := 'Framebuffer incomplete: No image is attached to FBO.';

		GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS:
        Result := 'Framebuffer incomplete: Attached images have different dimensions.';

		GL_FRAMEBUFFER_INCOMPLETE_FORMATS:
        Result := 'Framebuffer incomplete: Color attached images have different internal formats.';

		GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
        Result := 'Framebuffer incomplete: Draw buffer.';

		GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
        Result := 'Framebuffer incomplete: Read buffer.';

		GL_FRAMEBUFFER_UNSUPPORTED:
        Result := 'Unsupported by FBO implementation.';
		Else
        Result := 'Unknow FBO error code '+CardinalToString(Code);
    End;
End;

Function OpenGLES2FBO.Init():Boolean;
Var
  I, Status:Integer;
  R:OpenGLES2Renderer;
Begin
  Log(logDebug, 'Framebuffer','Initializing framebuffer: '{+ Self.Name});

  R := OpenGLES2Renderer(_Owner);
  
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  {$IFDEF IPHONE}
{    bool isSimulator = ( 0 == strcmp((const char*)"Apple Software GraphicsManager",
                                       (const char*) glGetString(GL_GraphicsManager)) )?TRUE:FALSE;

	char *extensions = glGetString(GL_EXTENSIONS);
	hasStencil = (strstr(extensions, "GL_OES_packed_depth_stencil")!=0);
	hasMsaa = (_msaaSamples>0) && (strstr(extensions, "GL_APPLE_framebuffer_multisample")!=0);}

			{// simulator hasMsaa = false;

		glGenFramebuffersOES(1, &viewFramebuffer);
		glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);

		glGenRenderbuffersOES(1, &viewRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);

		[context renderbufferStorage:GL_RENDERBUFFER_OES fromDrawable:(CAEAGLLayer*)self.layer];
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_COLOR_ATTACHMENT0_OES, GL_RENDERBUFFER_OES, viewRenderbuffer);
		glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_WIDTH_OES, &backingWidth);
		glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_HEIGHT_OES, &backingHeight);

		glGenRenderbuffersOES(1, &depthRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, depthRenderbuffer);
		glRenderbufferStorageOES(GL_RENDERBUFFER_OES, GL_DEPTH_COMPONENT24_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_DEPTH_ATTACHMENT_OES, GL_RENDERBUFFER_OES, depthRenderbuffer);

		glGenRenderbuffersOES(1, &stencilRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, stencilRenderbuffer);
		glRenderbufferStorageOES(GL_RENDERBUFFER_OES, GL_STENCIL_INDEX8_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_STENCIL_ATTACHMENT_OES, GL_RENDERBUFFER_OES, stencilRenderbuffer);

     // msaa
		glGenFramebuffersOES(1, &viewFramebuffer);
		glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);

		glGenRenderbuffersOES(1, &viewRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);

		[context renderbufferStorage:GL_RENDERBUFFER_OES fromDrawable:(CAEAGLLayer*)self.layer];
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_COLOR_ATTACHMENT0_OES, GL_RENDERBUFFER_OES, viewRenderbuffer);
		glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_WIDTH_OES, &backingWidth);
		glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_HEIGHT_OES, &backingHeight);

		glGenFramebuffersOES(1, &msaaFramebuffer);
		glGenRenderbuffersOES(1, &msaaRenderBuffer);

		glBindFramebufferOES(GL_FRAMEBUFFER_OES, msaaFramebuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, msaaRenderBuffer);

		glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER_OES, _msaaSamples, GL_RGB5_A1_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_COLOR_ATTACHMENT0_OES, GL_RENDERBUFFER_OES, msaaRenderBuffer);
		glGenRenderbuffersOES(1, &msaaDepthBuffer);

		glBindRenderbufferOES(GL_RENDERBUFFER_OES, msaaDepthBuffer);
		glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER_OES, _msaaSamples, GL_DEPTH24_STENCIL8_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_DEPTH_ATTACHMENT_OES, GL_RENDERBUFFER_OES, msaaDepthBuffer);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_STENCIL_ATTACHMENT_OES, GL_RENDERBUFFER_OES, msaaDepthBuffer);
    }

{RaiseError('initializing empty iphone fbo');
  If (_Name='device_target0') Then
  Begin
    _Handle := R.GenerateFrameBuffer();
    glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

    _color_rb := R.GenerateRenderBuffer();
    glBindRenderbuffer(GL_RENDERBUFFER, _color_rb);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, _color_rb);
    SetRenderbufferStorage(); //[context renderbufferStorage:GL_RENDERBUFFER fromDrawable:(CAEAGLLayer*)self.layer];
    Log(logDebug,'Framebuffer', 'Linked framebuffer to display memory');

    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_WIDTH, @_Width);
    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_HEIGHT, @_Height);
    Log(logDebug,'Framebuffer', 'Framebuffer size:  '+IntToString(_Width)+' x '+IntToString(_Height));

    _depth_rb := R.GenerateRenderBuffer();
    glBindRenderbuffer(GL_RENDERBUFFER, _depth_rb);

    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, _Width, _Height);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
    End;}
  {$ENDIF}

  If (_Handle = 0) Then
  Begin
    // Create a framebuffer and renderbuffer
    _Handle := R.GenerateFrameBuffer();
    _depth_rb := R.GenerateRenderBuffer();
    Log(logDebug,'Framebuffer', 'Created framebuffer with handle: '+IntToString(_Handle));

    // Create a texture to hold the frame buffer
    _Targets[0] := R.GenerateTexture();
	  glBindTexture(GL_TEXTURE_2D, _Targets[0]);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, _Width, _Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);
    Log(logDebug,'Framebuffer', 'Framebuffer size: W: '+IntToString(_Width)+' H: '+IntToString(_Height));

    If _Owner.Features.PackedStencil.Avaliable Then
    Begin
      I := GL_DEPTH24_STENCIL8_OES;
      Log(logDebug,'Framebuffer', 'Packed stencil supported, using it!');
    End Else
      I := GL_DEPTH_COMPONENT16;

    //bind renderbuffer
    glBindRenderbuffer(GL_RENDERBUFFER, _depth_rb);
    glRenderbufferStorage(GL_RENDERBUFFER, I, _Width, _Height);
    Log(logDebug,'Framebuffer', 'Binding depth renderbuffer to framebuffer with handle: '+IntToString(_depth_rb));

    // bind the framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

    // specify texture as color attachment
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _Targets[0], 0);
    Log(logDebug,'Framebuffer', 'Binding texture to framebuffer with handle: '+IntToString(_Targets[0]));

    // specify depth_renderbufer as depth attachment
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_Rb);

    If _Owner.Features.PackedStencil.Avaliable Then
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
  End;

	// check for errors
	Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  _Complete := (Status = GL_FRAMEBUFFER_COMPLETE);

  If Not _Complete Then
    Log(logError, 'Framebuffer', GetErrorString(Status));

  // set default framebuffer
	glBindFramebuffer(GL_FRAMEBUFFER, 0);

  Result := _Complete;
End;

Procedure OpenGLES2FBO.Invalidate;
Var
  I:Integer;
Begin
  _Handle := 0;
	_color_rb := 0;
  _depth_rb := 0;
  _stencil_rb := 0;

  For I:=0 To Pred(_TargetCount) Do
    _Targets[I] := 0;
End;

Procedure OpenGLES2FBO.BeginCapture(Flags: Cardinal);
Var
  ClearFlags:Cardinal;
Begin
  If (_Handle = 0) Then
    Self.Init();

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','Begin framebuffer capture: W:'+IntToString(_Width)+' H:'+IntToString(_Height));{$ENDIF}

	If (_multisample) Then
  Begin
		glBindFramebuffer(GL_FRAMEBUFFER, _mfb);
	End Else
  Begin
		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);
  End;

  GraphicsManager.Instance.ActiveViewport.SetViewArea(0, 0, _Width, _Height);

  If (Flags<>0) Then
  Begin
    ClearFlags := 0;

    If ((Flags And clearColor)<>0) Then
      ClearFlags := ClearFlags Or GL_COLOR_BUFFER_BIT;

    If ((Flags And clearDepth)<>0) Then
      ClearFlags := ClearFlags Or GL_DEPTH_BUFFER_BIT;

    If ((Flags And clearStencil)<>0) Then
      ClearFlags := ClearFlags Or GL_STENCIL_BUFFER_BIT;

    glClearStencil(0);
    glClearColor(_BackgroundColor.R/255.0, _BackgroundColor.G/255.0, _BackgroundColor.B/255.0, _BackgroundColor.A/255.0);
    glClear(ClearFlags);
  End;
End;

Procedure OpenGLES2FBO.EndCapture;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','End framebuffer capture');{$ENDIF}

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
End;

Procedure OpenGLES2FBO.Resize(NewWidth, NewHeight:Integer);
Begin
	Self.Release();
  _Width := NewWidth;
  _Height := NewHeight;
	Self.Init();
End;

Function OpenGLES2FBO.Bind(Slot: Integer):Boolean;
Begin
  Result := (_Handle>0) And (Self.IsValid()) And (_Complete);
  If Not Result Then
  Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    Result := False;
    Exit;
  End;

	glActiveTexture(GL_TEXTURE0 + Slot);
  {$IFDEF PC}
	glEnable(GL_TEXTURE_2D);
  {$ENDIF}

	glBindTexture(GL_TEXTURE_2D, _Targets[0]);
  Result := True;
End;

Procedure OpenGLES2FBO.Release();
Var
  I:Integer;
  R:OpenGLES2Renderer;
Begin
{  If (_ContextID <> Application.Instance.ContextID) Then
  Begin
  Self.Invalidate();
    Exit;
  End;}

  R := OpenGLES2Renderer(_Owner);

  R.DeleteRenderBuffer(_color_rb);

	If (Not _Shared) Then
  Begin
		R.DeleteRenderBuffer(_depth_rb);
    R.DeleteRenderBuffer(_stencil_rb);
  End;

  For I:=0 To Pred(_TargetCount) Do
		R.DeleteTexture(_Targets[I]);

  R.DeleteFrameBuffer(_Handle);
End;

Function OpenGLES2FBO.GetImage():Image;
Begin
  Result := Image.Create(_Width, _Height);

	glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

	glReadPixels(0,0, _Width, _Height, GL_RGBA, GL_UNSIGNED_BYTE, Result.Pixels);
	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	Result.Process(IMP_FlipVertical);
End;

Function OpenGLES2FBO.GetPixel(X,Y:Integer):Color;
Var
  P:Color;
Begin
  Y := _Height - Y;
  P := ColorNull;
  Result := P;
End;

{$IFDEF IPHONE}
Procedure OpenGLES2FBO.PresentToScreen();
Begin
(*
{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','Presenting framebuffer: '+_Name);{$ENDIF}
  glBindRenderbuffer(GL_RENDERBUFFER, _color_rb);
  PresentRenderBuffer();
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
*)
End;
{$ENDIF}

Procedure OpenGLES2FBO.SetFilter(Value: TextureFilterMode);
Begin
  _Filter := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureFilter(_Handle, GL_TEXTURE_2D, False, False, Filter);
End;

procedure OpenGLES2FBO.SetMipMapping(Value: Boolean);
Begin
  _MipMapped := Value;
End;

procedure OpenGLES2FBO.SetWrapMode(Value: TextureWrapMode);
Begin
  _WrapMode := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureWrap(_Handle, GL_TEXTURE_2D, WrapMode);
End;

Function OpenGLES2FBO.GetOrigin: SurfaceOrigin;
Begin
  Result := surfaceBottomRight;
End;

{ OpenGLES2Texture }
Function OpenGLES2Texture.Bind(Slot: Integer): Boolean;
Begin
  glActiveTexture(GL_TEXTURE0 + Slot);

  Result := (_Handle>0) And (Self.IsValid());
  If Not Result Then
  Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    Result := False;
    Exit;
  End;

  glBindTexture(GL_TEXTURE_2D, _Handle);
  Result := True;
End;

Function OpenGLES2Texture.Generate(Pixels: Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType): Boolean;
Var
  Mult:Single;
Begin
  _Handle := OpenGLES2Renderer(_Owner).GenerateTexture();
  _Width := Width;
  _Height := Height;

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, _Handle);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Uploading texture frame...');{$ENDIF}
  glTexImage2D(GL_TEXTURE_2D, 0, TextureColorFormatToGL(TargetFormat), Width, Height, 0, TextureColorFormatToGL(SourceFormat), ByteFormatToGL(ByteFormat), Pixels);

  //_Source.Save('debug\temp\pp'+IntTOString(I)+'.png');

(*  If (_Format = GL_COMPRESSED_RGBA) Then
  Begin
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, @_Format);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE, @_Size);
  End;
*)

  Case ByteFormatToGL(ByteFormat) Of
  GL_UNSIGNED_SHORT_4_4_4_4,
  GL_UNSIGNED_SHORT_5_5_5_1,
  GL_UNSIGNED_SHORT_5_6_5:
    Begin
      Mult := 2;
    End;

  Else
    Mult := 4.0;
  End;

  _Size := Trunc(Mult * _Width * _Height);

  _ShouldGenMips := True;
  
  Result := True;
End;

Function OpenGLES2Texture.GetImage:Image;
Begin
  Log(logDebug, 'Texture', 'Getting image from texture '{+Self.Name});

  Result := Image.Create(_Width, _Height);
End;

Procedure OpenGLES2Texture.Invalidate;
Begin
  _Handle := 0;
End;

Function OpenGLES2Texture.Update(Pixels: Pointer; X, Y, Width, Height: Integer):Boolean;
Begin
	glBindTexture(GL_TEXTURE_2D, _Handle);
	//glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, Source.Width, Source.Height, _TargetFormat, _ByteFormat, Pixels);
  glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, Pixels);

  _ShouldGenMips := True;

  Result := True;
End;

Procedure OpenGLES2Texture.Release;
Begin
  OpenGLES2Renderer(_Owner).DeleteTexture(_Handle);
End;

Procedure OpenGLES2Texture.SetFilter(Value: TextureFilterMode);
Begin
  Self._Filter := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureFilter(_Handle, GL_TEXTURE_2D, MipMapped, _ShouldGenMips, Filter);

  _ShouldGenMips := False;
End;

Procedure OpenGLES2Texture.SetMipMapping(Value: Boolean);
Begin
  Self._MipMapped := Value;
End;

Procedure OpenGLES2Texture.SetWrapMode(Value: TextureWrapMode);
Begin
  Self._WrapMode := Value;
  OpenGLES2Renderer(_Owner).ApplyTextureWrap(_Handle, GL_TEXTURE_2D, Value);
End;


End.