Unit TERRA_D3D9Renderer;

{$I terra.inc}

Interface
Uses Windows, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Renderer, TERRA_VertexFormat,
  TERRA_Color, TERRA_Image, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D,
  TERRA_Matrix3x3, TERRA_Matrix4x4,
  DXTypes, Direct3D9, D3DX9;

Type
  D3D9Features = Class(RendererFeatures)
    Public
      Constructor Create(Owner:GraphicsRenderer);
  End;

  D3D9VBO = Class(VertexBufferInterface)
    Protected
      _VertexBuffer:IDirect3DVertexBuffer9;
      _IndexBuffer:IDirect3DIndexBuffer9;

      Procedure Submit(Wireframe:Boolean); Override;

    Public
      Destructor Destroy; Override;

      Function Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean; Override;

      Procedure Update(Data:PByte); Override;

      (*Function Lock:Pointer;
      Procedure Unlock;*)
  End;

  D3D9Texture = Class(TextureInterface)
    Protected
      _Handle:IDirect3DTexture9;

      Procedure ApplySettings(Slot:Integer); Override;
      Function InternalBind(Slot:Integer):Boolean; Override;

    Public
      Function Generate(Pixels:Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;
      Procedure Release(); Override;

      Procedure Update(Pixels:Pointer; X,Y, Width, Height:Integer); Override;

      Function GetImage():Image; Override;

      Procedure Invalidate(); Override;
  End;

  D3D9CubeMap = Class(CubeMapInterface)
    Protected
      _Handle:IDirect3DCubeTexture9;

      Procedure ApplySettings(Slot:Integer); Override;
      Function InternalBind(Slot:Integer):Boolean; Override;

    Public
      Procedure Release(); Override;

      Function Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; Override;

      Procedure UpdateFace(FaceID:Integer; Pixels:Pointer; X,Y, Width, Height:Integer); Override;

      Procedure Invalidate(); Override;
  End;

  D3D9FBO = Class(RenderTargetInterface)
    Protected
      _RenderTexture:IDirect3DTexture9;
	    _ColorBuffer:IDirect3DSurface9;
      _ZBuffer:IDirect3DSurface9;
	    _internalformat:D3DFormat;

	    _multisample:Boolean;
	    _type:PixelSizeType;

  	  _targets:Array Of Cardinal;
      _targetCount:Integer;
      _Shared:Boolean;

  	  // Create a render texture
	    Procedure Init();

      Procedure ApplySettings(Slot:Integer); Override;
      Function InternalBind(Slot:Integer):Boolean; Override;

    Public
      Function Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer, StencilBuffer:Boolean):Boolean; Override;

  	  // Free D3D9 memory
	    Procedure Release(); Override;

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

  D3D9ShaderAttribute = Record
    Name:TERRAString;
    Handle:Integer;
  End;

  D3D9Shader = Class(ShaderInterface)
    Protected
      _VertexCode:TERRAString;
	    _FragmentCode:TERRAString;

	    _VertexShader:IDirect3DVertexShader9;
	    _PixelShader:IDirect3DPixelShader9;

      _Attributes:Array Of D3D9ShaderAttribute;
      _AttributeCount:Integer;

      Procedure UniformError(Const Name:TERRAString);

      Procedure AddAttributes(Source:TERRAString);

      Function CompileShader(Const Source:TERRAString; IsPixelShader:Boolean):Boolean;

      Procedure Unload();

    Public
      Function Generate(Const Name:TERRAString; ShaderCode:TERRAString):Boolean; Override;

      Function IsReady():Boolean; Override;

      Procedure Bind(); Override;

			Procedure SetIntegerUniform(Const Name:TERRAString; Const Value:Integer); Override;
			Procedure SetFloatUniform(Const Name:TERRAString; Const Value:Single); Override;
			Procedure SetVec2Uniform(Const Name:TERRAString; Const Value:Vector2D); Override;
			Procedure SetVec3Uniform(Const Name:TERRAString; const Value:Vector3D); Override;
			Procedure SetVec4Uniform(Const Name:TERRAString; const Value:Quaternion); Override;
      Procedure SetMat3Uniform(Const Name:TERRAString; Value:Matrix3x3); Override;
      Procedure SetMat4Uniform(Const Name:TERRAString; Value:Matrix4x4); Override;
      Procedure SetVec4ArrayUniform(Const Name:TERRAString; Count:Integer; Values:PMatrix4x4); Override;

      Function HasUniform(Const Name:TERRAString):Boolean; Override;

      Function GetUniform(Name:TERRAString):Integer; Override;
      Function GetAttributeHandle(Const Name:TERRAString):Integer; Override;

      Procedure Invalidate(); Override;
  End;

  D3D9Renderer = Class(GraphicsRenderer)
    Protected
      _Device:IDirect3DDevice9;
      _D3D:IDirect3D9;
      //_backBufferSurfaceDesc: PD3DSurfaceDesc;
      _DeviceSettings:PD3DPresentParameters;

      _MainSurface:IDirect3DSurface9;
      _MainZBuffer:IDirect3DSurface9;

      _ClearColor:Color;
      _ClearDepth:Single;
      _ClearStencil:Cardinal;

      Function CreateContext():Boolean; Override;
      Procedure DestroyContext(); Override;

      Function Initialize():Boolean; Override;

      Procedure ApplyTextureSettings(Slot:Integer; NPOT, MipMapped:Boolean; Filter:TextureFilterMode; WrapMode:TextureWrapMode);

    Public
      Procedure Reset; Override;

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

      Procedure DrawSource(Data:VertexData; Primitive:RenderPrimitive; Count:Integer); Override;
      Procedure DrawIndexedSource(Data:VertexData; Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord); Override;
    Public

  End;

Implementation
Uses TERRA_Log, TERRA_Application, TERRA_GraphicsManager, TERRA_FileManager, TERRA_FileUtils, TERRA_FileStream, TERRA_Error;

Function Direct3DCreate9 (SDKVersion: LongWord): Pointer; stdcall; External 'd3d9.dll';

Procedure D3DCheck(Result:HRESULT);
Begin
  If Failed(Result) Then
    RaiseError('D3D9 failure, with code '+CardinalToString(Result));
End;

Function CompareToD3D(Mode:CompareMode):Integer;
Begin
  Case Mode Of
  compareNever:       Result := D3DCMP_NEVER;
  compareLess:        Result := D3DCMP_LESS;
  compareEqual:       Result := D3DCMP_EQUAL;
  compareLessOrEqual: Result := D3DCMP_LESSEQUAL;
  compareGreater:     Result := D3DCMP_GREATER;
  compareDifferent:   Result := D3DCMP_NOTEQUAL;
  compareGreaterOrEqual: Result := D3DCMP_GREATEREQUAL;
  compareAlways:      Result := D3DCMP_ALWAYS;
  End;
End;

Function StencilOpToD3D(Op:StencilOperation):Integer;
Begin
  Case Op Of
  stencilKeep:      Result := D3DSTENCILOP_KEEP;
  stencilReplace:   Result := D3DSTENCILOP_REPLACE;
  stencilIncrement: Result := D3DSTENCILOP_INCRSAT;
  stencilDecrement: Result := D3DSTENCILOP_DECRSAT;
  stencilInvert:    Result := D3DSTENCILOP_INVERT;
  stencilIncrementWithWrap: Result := D3DSTENCILOP_INCR;
  stencilDecrementWithWrap: Result := D3DSTENCILOP_DECR;
  End;
End;

Function PrimitiveToD3D(Primitive:RenderPrimitive):D3DPRIMITIVETYPE;
Begin
  Case Primitive Of
  renderPoints:     Result := D3DPT_POINTLIST;
  renderLines:      Result := D3DPT_LINELIST;
  renderTriangles:  Result := D3DPT_TRIANGLELIST;
  renderLineStrip:  Result := D3DPT_LINESTRIP;
  renderTriangleStrip: Result := D3DPT_TRIANGLESTRIP;
  End;
End;

Function DataFormatToD3D(Format:DataFormat):D3DDECLTYPE;
Begin
  Case Format Of
  typeColor:  Result := D3DDECLTYPE_UBYTE4N;
  typeFloat:    Result:= D3DDECLTYPE_FLOAT1;
  typeVector2D: Result:= D3DDECLTYPE_FLOAT2;
  typeVector3D: Result := D3DDECLTYPE_FLOAT3;
  typeVector4D: Result := D3DDECLTYPE_FLOAT4;
    Else
      Result := D3DDECLTYPE_UNUSED;
  End;
End;

{Function ByteFormatToD3D(ByteFormat:PixelSizeType):Integer;
Begin
  Case ByteFormat Of
  pixelSizeByte: Result := GL_UNSIGNED_BYTE;
    Else
      Result := 0;
  End;
End;}

Function TextureColorFormatToD3D(Format:TextureColorFormat):D3DFORMAT;
Begin
  Case Format Of
  colorRGB:   Result := D3DFMT_R8G8B8;
  colorRGBA:  Result := D3DFMT_A8R8G8B8;
  colorBGR:   Result := D3DFMT_R8G8B8;
  colorBGRA:   Result := D3DFMT_A8R8G8B8;
  colorAlpha: Result := D3DFMT_A8; 
  End;
End;

{ D3D9Renderer }
Function D3D9GraphicsRenderer.CreateContext: Boolean;
Var
  Adapter:Cardinal;
  DeviceID:TD3DAdapterIdentifier9;
Begin
  Result := False;

  If Not D3DXCheckVersion(D3D_SDK_VERSION, D3DX_SDK_VERSION) then
  Begin
    Log(logError, 'Renderer', 'Error initializing Direct3D, version check failed');
    Exit;
  End;

  Adapter := D3DADAPTER_DEFAULT;

	FillChar(_DeviceSettings, SizeOf(_DeviceSettings), 0);
	_DeviceSettings.Windowed := True;
	_DeviceSettings.SwapEffect := D3DSWAPEFFECT_DISCARD;
	_DeviceSettings.EnableAutoDepthStencil := True;
	_DeviceSettings.AutoDepthStencilFormat := D3DFMT_D24S8;
	_DeviceSettings.hDeviceWindow := Application.Instance.Handle;
	_DeviceSettings.BackBufferWidth := Application.Instance.Width;
	_DeviceSettings.BackBufferHeight := Application.Instance.Height;
	_DeviceSettings.BackBufferFormat := D3DFMT_A8R8G8B8;
	_DeviceSettings.MultiSampleType := D3DMULTISAMPLE_NONE;

  _D3D := IDirect3D9(Direct3DCreate9(D3D_SDK_VERSION));
  If Failed(_D3D.CreateDevice(Adapter, D3DDEVTYPE_HAL,
    Application.Instance.Handle, D3DCREATE_HARDWARE_VERTEXPROCESSING, @_DeviceSettings, _Device)) Then
  Begin
    Log(logError, 'Renderer', 'Error initializing Direct3D, device init failed');
    Exit;
  End;

  _D3D.GetAdapterIdentifier(Adapter, 0, DeviceID);

  _DeviceName := DeviceID.DeviceName;
  _DeviceVersion.Major := DeviceID.DriverVersionHighPart;
  _DeviceVersion.Minor := DeviceID.DriverVersionLowPart;
  _DeviceVersion.Build := 0;

  _Device.GetRenderTarget(0, _MainSurface);
  _Device.GetDepthStencilSurface(_MainZBuffer);

  Result := True;
End;

Procedure D3D9GraphicsRenderer.DestroyContext();
Begin
  If Assigned(_Device) Then
  Begin
    _Device._Release();
    _Device := Nil;
  End;

  If Assigned(_D3D) Then
  Begin
    _D3D._Release();
    _D3D := Nil;
  End;
End;

Function D3D9GraphicsRenderer.Initialize():Boolean;
Var
  I:Integer;
  S:AnsiString;
Begin
  Result := CreateContext();

  If Not Result Then
    Exit;

  _Features := D3D9Features.Create(Self);

(*
  {$IFNDEF MOBILE}
  If (Features.Shaders.Avaliable) Then
  Begin
    S := glGetString(GL_SHADING_LANGUAGE_VERSION);
    If (S<>'') Then
    Begin
      Log(logDebug, 'GraphicsManager', 'Version: '+ S);

      I := Pos(' ', S);
      If (I>0) Then
        S := Copy(S, 1, Pred(I));
      _Version := StringToVersion(S);
    End;

    Log(logDebug,'GraphicsManager','GLSL version:'+VersionToString(_Version));
  End;
  {$ENDIF}*)

  Result := True;
End;

Procedure D3D9GraphicsRenderer.SetColorMask(Red, Green, Blue, Alpha: Boolean);
Var
  colorMask:Cardinal;
Begin
  colorMask := 0;

  If Red Then
    colorMask := colorMask Or D3DCOLORWRITEENABLE_RED;

  If Green Then
    colorMask := colorMask Or D3DCOLORWRITEENABLE_GREEN;

  If Blue Then
    colorMask := colorMask Or D3DCOLORWRITEENABLE_BLUE;

  If Alpha Then
    colorMask := colorMask Or D3DCOLORWRITEENABLE_ALPHA;

  _Device.SetRenderState(D3DRS_COLORWRITEENABLE, colorMask);
End;

Procedure D3D9GraphicsRenderer.SetDepthMask(WriteZ: Boolean);
Begin
  _Device.SetRenderState(D3DRS_ZWRITEENABLE, Cardinal(WriteZ));
End;

Procedure D3D9GraphicsRenderer.SetStencilTest(Enable: Boolean);
Begin
  _Device.SetRenderState(D3DRS_STENCILENABLE, Cardinal(Enable));
End;

Procedure D3D9GraphicsRenderer.SetStencilFunction(Mode: CompareMode; StencilID, Mask: Cardinal);
Begin
  _Device.SetRenderState(D3DRS_STENCILFUNC, CompareToD3D(Mode));
  _Device.SetRenderState(D3DRS_STENCILMASK, Mask);
  _Device.SetRenderState(D3DRS_STENCILREF, StencilID);
End;

Procedure D3D9GraphicsRenderer.SetStencilOp(fail, zfail, zpass: StencilOperation);
Begin
  _Device.SetRenderState(D3DRS_STENCILFAIL, StencilOpToD3D(Fail));
  _Device.SetRenderState(D3DRS_STENCILZFAIL, StencilOpToD3D(ZFail));
  _Device.SetRenderState(D3DRS_STENCILPASS, StencilOpToD3D(ZPass));
End;

Procedure D3D9GraphicsRenderer.ClearBuffer(Color, Depth, Stencil:Boolean);
Var
  Flags:Cardinal;
Begin
  Flags := 0;

  If (Color) Then
    Flags := Flags Or D3DCLEAR_TARGET;

  If (Depth) Then
    Flags := Flags Or D3DCLEAR_ZBUFFER;

  If (Stencil) Then
    Flags := Flags Or D3DCLEAR_STENCIL;

  If (Flags<>0) Then
  	_Device.Clear(0, Nil, Flags, D3DCOLOR(ClearColor), _ClearDepth, _ClearStencil);
End;

Procedure D3D9GraphicsRenderer.SetClearColor(const ClearColor: Color);
Begin
  _ClearColor := ClearColor;
End;

Procedure D3D9GraphicsRenderer.SetCullMode(Mode: CullMode);
Var
  Value:Cardinal;
Begin
  Case Mode Of
  cullNone: Value := D3DCULL_NONE;
  cullFront: Value := D3DCULL_CW;
  Else
    Value := D3DCULL_CCW;
  End;

  _Device.SetRenderState(D3DRS_CULLMODE, Value);
End;

Procedure D3D9GraphicsRenderer.SetDepthTest(Enable: Boolean);
Begin
  If Enable Then
    _Device.SetRenderState(D3DRS_ZENABLE, D3DZB_TRUE)
  Else
    _Device.SetRenderState(D3DRS_ZENABLE, D3DZB_FALSE);
End;

Procedure D3D9GraphicsRenderer.SetDepthFunction(Mode: CompareMode);
Begin
  _Device.SetRenderState(D3DRS_ZFUNC, CompareToD3D(Mode));
End;

Procedure D3D9GraphicsRenderer.SetBlendMode(BlendMode: Integer);
Var
  NeedsAlpha:Boolean;
  SrcBlend, DestBlend:Cardinal;
Begin
{glEnable(GL_BLEND);
glBlendFunc(GL_ONE, GL_ONE);
exit;}

{  If (BlendMode = _CurrentBlendMode) Then
    Exit;}

  NeedsAlpha := BlendMode>0;

  _Device.SetRenderState(D3DRS_ALPHABLENDENABLE, Cardinal(NeedsAlpha));

  If (NeedsAlpha) Then
  Begin
    Case BlendMode Of
    blendBlend:
      Begin
        SrcBlend := D3DBLEND_SRCALPHA;
        DestBlend := D3DBLEND_INVSRCALPHA;
      End;

    blendAdd:
      Begin
        SrcBlend := D3DBLEND_SRCALPHA;
        DestBlend := D3DBLEND_ONE;
      End;

    blendFilter:  
      Begin
        SrcBlend := D3DBLEND_ONE;
        DestBlend := D3DBLEND_INVSRCALPHA;
      End;

    blendModulate:
      Begin
        SrcBlend := D3DBLEND_SRCCOLOR;
        DestBlend := D3DBLEND_ONE;
      End;

    blendJoin:
      Begin
        SrcBlend := D3DBLEND_ONE;
        DestBlend := D3DBLEND_ONE;
      End;

    blendZero:
      Begin
        SrcBlend := D3DBLEND_ZERO;
        DestBlend := D3DBLEND_ZERO;
      End;

    blendOne:
      Begin
        SrcBlend := D3DBLEND_ONE;
        DestBlend := D3DBLEND_ZERO;
      End;

    blendColor:
      Begin
        SrcBlend := D3DBLEND_SRCCOLOR;
        DestBlend := D3DBLEND_INVSRCCOLOR;
      End;

    blendColorAdd:   
      Begin
        SrcBlend := D3DBLEND_SRCCOLOR;
        DestBlend := D3DBLEND_ONE;
      End;

    blendReflection:   
      Begin
        SrcBlend := D3DBLEND_DESTALPHA;
        DestBlend := D3DBLEND_INVDESTALPHA;
      End;

    End;

    _Device.SetRenderState(D3DRS_SRCBLEND, SrcBlend);
    _Device.SetRenderState(D3DRS_DESTBLEND, destBlend);
    //mDevice->SetRenderState(D3DRS_BLENDOP, gl_d3d9::ConvertBlendOp(blendState.blendEquationRGB));
  End;

 //_CurrentBlendMode := BlendMode;
End;

Procedure D3D9GraphicsRenderer.SetModelMatrix(Const Mat: Matrix4x4);
Begin
  _ModelMatrix := Mat;

{  If (Features.Shaders.Avaliable) Then
  Begin
    If Assigned(Self.ActiveShader) Then
    Begin
      Self.ActiveShader.SetMat4Uniform('modelMatrix', Mat);
      Exit;
    End;
  End;

  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@Mat);
  TODO      }
End;

Procedure D3D9GraphicsRenderer.SetProjectionMatrix(Const Mat:Matrix4x4);
Begin
  _ProjectionMatrix := Mat;

{  If (Features.Shaders.Avaliable) Then
  Begin
    If Assigned(Self.ActiveShader) Then
    Begin
      Self.ActiveShader.SetMat4Uniform('projectionMatrix', Mat);
      Exit;
    End;
  End;

  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@Mat);

  TODO}
End;

Procedure D3D9GraphicsRenderer.SetTextureMatrix(Const Mat: Matrix4x4);
Begin
  _TextureMatrix := Mat;
{
  If (Features.Shaders.Avaliable) Then
  Begin
    If Assigned(Self.ActiveShader) Then
    Begin
      Self.ActiveShader.SetMat4Uniform('textureMatrix', Mat);
      Exit;
    End;
  End;

  glActiveTexture(GL_TEXTURE0);
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixf(@Mat);

  TODO}
End;

Procedure D3D9GraphicsRenderer.SetDiffuseColor(Const C: Color);
Begin
  _DiffuseColor := C;

{  If (Features.Shaders.Avaliable) Then
  Begin
    If Assigned(Self.ActiveShader) Then
    Begin
      Self.ActiveShader.SetColorUniform('diffuse', C);
      Exit;
    End;
  End;

  glColor4f(C.R/255, C.G/255, C.B/255, C.A/255);

  TODO
  }
End;

Procedure D3D9GraphicsRenderer.SetAttributeSource(Const Name:AnsiString; AttributeKind:Cardinal; ElementType:DataFormat; AttributeSource:Pointer);
Var
  Count:Integer;
  Format:D3DDECLTYPE;
  Norm:Boolean;
  Handle:Integer;
Begin
  Format := DataFormatToD3D(ElementType);
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

  If _CurrentVertexSize<=0 Then
  Begin
    RaiseError('Please call GraphicsRenderer.SetVertexSize() before drawing anything!');
    Exit;
  End;

  If Self.ActiveShader = Nil Then
    Exit;

  Handle := Self.ActiveShader.GetAttributeHandle(Name);

  If (Handle<0) Then
    Exit;

  {  TODO
  glVertexAttribPointer(Handle, Count, Format, Norm, _CurrentVertexSize, AttributeSource);}
End;

Procedure D3D9GraphicsRenderer.DrawSource(Data:VertexData; Primitive: RenderPrimitive; Count: Integer);
Begin
  If (Count<0) Then
    Exit;

  If Assigned(Data) Then
    Data.Bind(True)
  Else
    RaiseError('Cannot draw null buffer!');

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Renderer', 'glDrawArrays: '+IntToString(Count));
  {$ENDIF}

  _Device.DrawPrimitive(PrimitiveToD3D(Primitive), 0, Count);
  Inc(_Stats.TriangleCount, Count Div 3);

  _CurrentVertexSize := 0;
End;

Procedure D3D9GraphicsRenderer.DrawIndexedSource(Data:VertexData; Primitive:RenderPrimitive; Count:Integer; Indices:System.PWord);
Var
  PrimitiveCount:Integer;
Begin
  If (Count<0) Then
    Exit;

  Data.Bind(True);

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Renderer', 'glDrawElements: '+IntToString(Count));
  {$ENDIF}

  Case Primitive Of
  renderPoints:     PrimitiveCount := Count;
  renderLines:      PrimitiveCount := Count Shr 1;
  renderTriangles:  PrimitiveCount := Count Div 3;
  renderLineStrip:  PrimitiveCount := Count - 1;
  renderTriangleStrip: PrimitiveCount := Count - 2;
  End;

  //_Device.SetIndices(indexBuffer->getBuffer());
  _Device.DrawIndexedPrimitiveUP(PrimitiveToD3D(Primitive), 0, Count, PrimitiveCount, Indices, D3DFMT_INDEX16, Data, _CurrentVertexSize);
  Inc(_Stats.TriangleCount, PrimitiveCount);
End;

Procedure D3D9GraphicsRenderer.BeginFrame;
Begin
  Inherited;

  //before rendering something, you have to call this
  _Device.BeginScene();

  //Clear render region with blue
	_Device.Clear(0, Nil, D3DCLEAR_TARGET, D3DCOLOR_XRGB(0,0,255), 1.0, 0);
End;

Procedure D3D9GraphicsRenderer.EndFrame();
Begin
  _Device.EndScene();
  _Device.Present(Nil,  Nil, 0, Nil);
End;

Procedure D3D9GraphicsRenderer.SetViewport(X, Y, Width, Height:Integer);
Var
  dxViewport:D3DVIEWPORT9;
Begin
  dxViewport.X := X;
  dxViewport.Y := Y;
  dxViewport.Width := Width - X;
  dxViewport.Height := Height - Y;
  dxViewport.MinZ := 0.1;
  dxViewport.MaxZ := 1000;

  _Device.SetViewport(dxViewport);
End;

Procedure D3D9GraphicsRenderer.Reset();
Begin
End;

Procedure D3D9GraphicsRenderer.SetScissorArea(X,Y, Width, Height:Integer);
Var
  Scissor:TRect;
Begin
  Scissor.Left := X;
  Scissor.Top := Y;
  Scissor.Right := X + Width;
  Scissor.Bottom := Y + Height;
  _Device.SetScissorRect(@Scissor);
End;

Procedure D3D9GraphicsRenderer.SetScissorState(Enabled: Boolean);
Begin
  _Device.SetRenderState(D3DRS_SCISSORTESTENABLE, Cardinal(Enabled));
End;

Procedure D3D9GraphicsRenderer.ApplyTextureSettings(Slot:Integer; NPOT, MipMapped:Boolean; Filter:TextureFilterMode; WrapMode:TextureWrapMode);
Begin
  {$IFDEF PC}
    {$IFNDEF WINDOWS}
    MipMapped := False; {FIXME}
    {$ENDIF}
  {$ENDIF}

  If (Not Features.Shaders.Avaliable) Then
    MipMapped := False;

  {$IFDEF MOBILE}
  If (NPOT) Then
  Begin
    Filter := filterLinear;
    MipMapped := False;
    WrapMode := wrapNothing;
  End;
  {$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting texture filtering');{$ENDIF}

  If (MipMapped) Then
    _Device.SetSamplerState(Slot, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR)
  Else
    _Device.SetSamplerState(Slot, D3DSAMP_MIPFILTER, D3DTEXF_POINT);

  If (Filter = filterBilinear) Then
  Begin
    _Device.SetSamplerState(Slot, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    _Device.SetSamplerState(Slot, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  End Else
  Begin
    _Device.SetSamplerState(Slot, D3DSAMP_MINFILTER, D3DTEXF_POINT);
    _Device.SetSamplerState(Slot, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting wrap mode');{$ENDIF}

  If ((Cardinal(WrapMode) And Cardinal(wrapHorizontal))<>0) Then
    _Device.SetSamplerState(Slot, D3DSAMP_ADDRESSU, D3DTADDRESS_WRAP)
  Else
    _Device.SetSamplerState(Slot, D3DSAMP_ADDRESSU, D3DTADDRESS_CLAMP);

  If ((Cardinal(WrapMode) And Cardinal(wrapVertical))<>0) Then
    _Device.SetSamplerState(Slot, D3DSAMP_ADDRESSV, D3DTADDRESS_WRAP)
  Else
    _Device.SetSamplerState(Slot, D3DSAMP_ADDRESSV, D3DTADDRESS_CLAMP);

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Generating mipmap');{$ENDIF}
  If (MipMapped) Then
  Begin
//    glGenerateMipmap(TextureKind);
  End;

  {$IFNDEF MOBILE}
{	If (_Owner.Settings.Textures.Quality>=QualityHigh) And (_Onwer.Settings.MaxAnisotrophy > 1) Then
  Begin
	  glTexParameteri(TextureKind, GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, GraphicsManager.Instance.Settings.MaxAnisotrophy);
  End; BIBI}
  {$ENDIF}
End;

Function D3D9GraphicsRenderer.CreateCubeMap: CubeMapInterface;
Begin
  Result := D3D9CubeMap.Create(Self);
End;

Function D3D9GraphicsRenderer.CreateRenderTarget: RenderTargetInterface;
Begin
  Result := D3D9FBO.Create(Self);
End;

Function D3D9GraphicsRenderer.CreateTexture: TextureInterface;
Begin
  Result := D3D9Texture.Create(Self);
End;

Function D3D9GraphicsRenderer.CreateVertexBuffer: VertexBufferInterface;
Begin
  Result := D3D9VBO.Create(Self);
End;

Function D3D9GraphicsRenderer.CreateShader: ShaderInterface;
Begin
  Result := D3D9Shader.Create(Self);
End;


{ D3D9Features }
Constructor D3D9Features.Create(Owner:GraphicsRenderer);
Var
  Caps:PD3DCaps9;
  S:AnsiString;
Begin
  Inherited Create(Owner);

  D3D9Renderer(_Owner)._Device.GetDeviceCaps(Caps^);


  _MaxTextureUnits := Caps.MaxSimultaneousTextures;


  _MaxTextureSize := Caps.MaxTextureWidth;

  _maxRenderTargets := Caps.NumSimultaneousRTs;


  _maxAnisotrophy := Caps.MaxAnisotropy;

  _VertexCacheSize := 32;

  TextureCompression.Avaliable :=  True;

	Shaders.Avaliable := Caps.PixelShaderVersion>=2;

	VertexBufferObject.Avaliable := True;

  FrameBufferObject.Avaliable := True;

  PostProcessing.Avaliable := (FrameBufferObject.Avaliable) And (MaxRenderTargets>=4);

  CubeMapTexture.Avaliable := (Caps.TextureCaps And D3DPTEXTURECAPS_CUBEMAP)<>0;
  SeparateBlends.Avaliable := False;
  SeamlessCubeMap.Avaliable := False;
  NPOT.Avaliable := (Caps.TextureCaps And D3DPTEXTURECAPS_POW2)=0;

  PackedStencil.Avaliable := True;

  _MaxUniformVectors := Caps.MaxVertexShaderConst;
  If (_MaxUniformVectors<128) Then
    VertexBufferObject.Avaliable := False;

  TextureArray.Avaliable := False;

  FloatTexture.Avaliable := False;

  DeferredLighting.Avaliable := (MaxRenderTargets>=4) And (FrameBufferObject.Avaliable);

  StencilBuffer.Avaliable := True;
End;

{ D3D9VBO }
Function D3D9VBO.Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean;
Var
  Index:Single;
  I, N:Integer;
  Flags:Integer;
  Temp:Pointer;
Begin
  Self._Vertices := Vertices;
  Self._IndexList := IndexData;
  Self._EdgeList := EdgeData;
  Self._Dynamic := DynamicUsage;
  Self._TriangleCount := TriangleCount;
  Self._EdgeCount := 0;
  Self._WireframeIndices := Nil;
  Self._Ready := True;
  Result := True;

  If _Dynamic Then
    Flags := D3DUSAGE_DYNAMIC
  Else
    Flags := 0;//D3DUSAGE_WRITEONLY

  D3DCheck(D3D9Renderer(_Owner)._Device.CreateVertexBuffer(Vertices.Count * Vertices.Size, Flags, 0{CUSTOMFVF}, D3DPOOL_MANAGED, _VertexBuffer, Nil));
  Self.Update(Vertices.Buffer);

  D3DCheck(D3D9Renderer(_Owner)._Device.CreateIndexBuffer(TriangleCount * SizeOf(Word), 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, _IndexBuffer, Nil));
  Self._IndexBuffer.Lock(0, 0, Temp, 0);
  Move(IndexData^, Temp^, TriangleCount * SizeOf(Word));
  _IndexBuffer.Unlock();

  _Ready := True;
End;

Destructor D3D9VBO.Destroy;
Begin
  If Assigned(_VertexBuffer) Then
  Begin
    _VertexBuffer._Release();
    _VertexBuffer := Nil;
  End;
  
  If Assigned(_IndexBuffer) Then
  Begin
    _IndexBuffer._Release();
    _IndexBuffer := Nil;
  End;

  Inherited;
End;

Procedure D3D9VBO.Submit(Wireframe:Boolean);
Var
  I:Integer;
Begin
  _Vertices.Bind(False);

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Mesh', 'glDrawElements: '+IntToString(_TriangleCount*3));
  {$ENDIF}

  D3D9Renderer(_Owner)._Device.SetStreamSource(0, _VertexBuffer, 0, _Vertices.Size);
  D3D9Renderer(_Owner)._Device.SetIndices(_IndexBuffer);

  If WireFrame Then
  Begin
    //glDrawElements(GL_LINES, _EdgeCount * 2, GL_UNSIGNED_SHORT, @(_WireframeIndices[0]));
  End Else
  Begin
    D3D9Renderer(_Owner)._Device.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, _Vertices.Count, 0, _TriangleCount);
  End;
//    GraphicsManager.Instance.Internal(0 , _TriangleCount);
End;


Procedure D3D9VBO.Update(Data:PByte);
Var
  Temp:Pointer;
Begin
  If (_VertexBuffer = Nil) Then
    Exit;

  Self._VertexBuffer.Lock(0, 0, Temp, 0);
  Move(Data^, Temp^, _Vertices.Count * _Vertices.Size);
  _VertexBuffer.Unlock();
End;

{ D3D9CubeMap }
Function D3D9CubeMap.InternalBind(Slot: Integer):Boolean;
Begin
  If _Handle = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  D3D9Renderer(_Owner)._Device.SetTexture(Slot, _Handle);
  Result := True;
End;

Function D3D9CubeMap.Generate(Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType):Boolean; 
Begin
  If (Not _Ready) Then
  Begin
    _Width := Width;
    _Height := Height;

    D3DCheck(D3D9Renderer(_Owner)._Device.CreateCubeTexture(_Width, 0, D3DUSAGE_AUTOGENMIPMAP Or D3DUSAGE_DYNAMIC, TextureColorFormatToD3D(TargetFormat), D3DPOOL_MANAGED, _Handle, Nil));

    _Ready := True;
    _SettingsChanged := True;
  End;
End;

Procedure D3D9CubeMap.UpdateFace(FaceID: Integer; Pixels: Pointer; X, Y, Width, Height: Integer);
Var
  N:D3DCUBEMAP_FACES;
  OutRect:D3DLOCKED_RECT;
  InRect:TRect;
Begin
  Case FaceID Of
  cubemap_PositiveX: N := D3DCUBEMAP_FACE_POSITIVE_X;
  cubemap_NegativeX: N := D3DCUBEMAP_FACE_NEGATIVE_X;

  cubemap_PositiveY: N := D3DCUBEMAP_FACE_POSITIVE_Y;
  cubemap_NegativeY: N := D3DCUBEMAP_FACE_NEGATIVE_Y;

  cubemap_PositiveZ: N := D3DCUBEMAP_FACE_POSITIVE_Z;
  cubemap_NegativeZ: N := D3DCUBEMAP_FACE_NEGATIVE_Z;
  End;

  InRect.Left := X;
  InRect.Top := Y;
  InRect.Right := X + Width;
  InRect.Bottom := Y + Height;
  _Handle.LockRect(N, 0, OutRect, @InRect, 0);
  Move(Pixels^, OutRect.pBits^, _Width * _Height * 4);
  _Handle.UnlockRect(N, 0);
End;

Procedure D3D9CubeMap.Invalidate;
Begin
  _Handle := Nil;
End;

Procedure D3D9CubeMap.Release();
Begin
  If Assigned(_Handle) Then
  Begin
    _Handle._Release();
    _Handle := Nil;
  End;
End;

Procedure D3D9CubeMap.ApplySettings(Slot:Integer);
Begin
  D3D9Renderer(_Owner).ApplyTextureSettings(Slot, Self.NPOT, MipMapped, Filter, WrapMode);
End;

{ D3D9FBO }
Function D3D9FBO.Generate(Width, Height:Integer; MultiSample:Boolean; PixelSize:PixelSizeType; TargetCount:Integer; DepthBuffer,StencilBuffer:Boolean):Boolean;
Var
  I:Integer;
Begin
  Self._Size := Width * Height * 4 * 2;

  _BackgroundColor := ColorCreate(Byte(0), Byte(0), Byte(0), Byte(0));

  _PixelSize := PixelSize;

  If (Multisample) And (_Owner.Features.MultiSampleCount<=0) Then
    Multisample := False;

  _TargetCount := TargetCount;
  SetLength(_Targets, _TargetCount);
  For I:=0 To Pred(_TargetCount) Do
  Begin
    _targets[I] := 0;
    //_DrawBuffers[I] := GL_COLOR_ATTACHMENT0 + I;
  End;

  _Shared := False;

	_Width := Width;
	_Height := Height;
	_type := PixelSize;
	_multisample := multisample;
  _hasDepthBuffer := DepthBuffer;
  _HasStencilBuffer := StencilBuffer;

  Log(logDebug,'Framebuffer', 'Creating Framebuffer with size: '+IntToString(_Width)+' x '+IntToString(_Height));

	If (_type = pixelSizeFloat) Then
		_internalFormat := D3DFMT_A16B16G16R16F
	Else
		_internalFormat := D3DFMT_A8R8G8B8;

  Self.Init();
  _Ready := True;
End;

Procedure D3D9FBO.Init();
Var
  MultiSampleType:TD3DMultiSampleType;
  MultisampleQuality:Cardinal;
Begin
  If _MultiSample Then
    MultiSampleType := D3DMULTISAMPLE_8_SAMPLES
  Else
    MultiSampleType := D3DMULTISAMPLE_NONE;

  MultisampleQuality := 0;

  D3DCheck(D3D9Renderer(_Owner)._Device.CreateTexture(_Width, _Height, 1, D3DUSAGE_RENDERTARGET, _InternalFormat, D3DPOOL_MANAGED, _RenderTexture, Nil));

  D3DCheck(_RenderTexture.GetSurfaceLevel(0, _ColorBuffer));

  //D3DCheck(D3D9Renderer(_Owner)._Device.CreateRenderTarget(_Width, _Height, _InternalFormat, MultiSampleType, MultisampleQuality, False, _ColorBuffer, Nil));

  D3DCheck(D3D9Renderer(_Owner)._Device.CreateDepthStencilSurface(_Width, _Height, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, False, _ZBuffer, Nil));
End;

Procedure D3D9FBO.Invalidate;
Var
  I:Integer;
Begin
  _ColorBuffer := Nil;
  _ZBuffer := Nil;

  For I:=0 To Pred(_TargetCount) Do
    _Targets[I] := 0;

  _Ready := False;
End;

Procedure D3D9FBO.BeginCapture(Flags: Cardinal);
Var
  ClearFlags:Cardinal;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','Begin framebuffer capture: W:'+IntToString(_Width)+' H:'+IntToString(_Height));{$ENDIF}

  D3D9Renderer(_Owner)._Device.SetRenderTarget(0, _ColorBuffer);
  D3D9Renderer(_Owner)._Device.SetDepthStencilSurface(_ZBuffer);

  GraphicsManager.Instance.ActiveViewport.SetViewArea(0, 0, _Width, _Height);

  If (Flags<>0) Then
  Begin
    ClearFlags := 0;

    If ((Flags And clearColor)<>0) Then
      ClearFlags := ClearFlags Or D3DCLEAR_TARGET;

    If ((Flags And clearDepth)<>0) Then
      ClearFlags := ClearFlags Or D3DCLEAR_ZBUFFER;

    If ((Flags And clearStencil)<>0) Then
      ClearFlags := ClearFlags Or D3DCLEAR_STENCIL;

    If (ClearFlags<>0) Then
  	  D3D9Renderer(_Owner)._Device.Clear(0, Nil, ClearFlags, D3DCOLOR(_BackgroundColor), 1.0, 0);
  End;
End;

Procedure D3D9FBO.EndCapture;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','End framebuffer capture');{$ENDIF}

  D3D9Renderer(_Owner)._Device.SetRenderTarget(0, D3D9Renderer(_Owner)._MainSurface);
  D3D9Renderer(_Owner)._Device.SetDepthStencilSurface(D3D9Renderer(_Owner)._MainZBuffer);
End;

Procedure D3D9FBO.Resize(NewWidth, NewHeight:Integer);
Begin
	Self.Release();
  _Width := NewWidth;
  _Height := NewHeight;
	Self.Init();
End;

Function D3D9FBO.InternalBind(Slot: Integer):Boolean;
Begin
  If _ColorBuffer = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  D3D9Renderer(_Owner)._Device.SetTexture(Slot, _RenderTexture);
  Result := True;
End;

Procedure D3D9FBO.Release();
Var
  I:Integer;
Begin
  If Assigned(_ColorBuffer) Then
  Begin
    _ColorBuffer._Release();
    _ColorBuffer := Nil;
  End;

  If Assigned(_ZBuffer) Then
  Begin
    _ZBuffer._Release();
    _ZBuffer := Nil;
  End;

  If Assigned(_RenderTexture) Then
  Begin
    _RenderTexture._Release();
    _RenderTexture := Nil;
  End;
End;

Function D3D9FBO.GetImage():Image;
Var
  OutRect:D3DLOCKED_RECT;
Begin
  Result := Image.Create(_Width, _Height);

  _RenderTexture.LockRect(0, OutRect, Nil, 0);
  Move(OutRect.pBits^, Result.Pixels^, Result.Width * Result.Height * 4);
  _RenderTexture.UnlockRect(0);

//	Result.Process(IMP_FlipVertical);
End;

Function D3D9FBO.GetPixel(X,Y:Integer):Color;
Var
  P:Color;
Begin
  Y := _Height - Y;
  P := ColorNull;
  Result := P;
End;

Procedure D3D9FBO.ApplySettings(Slot:Integer);
Begin
  D3D9Renderer(_Owner).ApplyTextureSettings(Slot, Self.NPOT, MipMapped, Filter, WrapMode);
End;

{ D3D9Texture }
Function D3D9Texture.InternalBind(Slot: Integer): Boolean;
Begin
  If _Handle = Nil Then
  Begin
    Result := False;
    Exit;
  End;

  D3D9Renderer(_Owner)._Device.SetTexture(Slot, _Handle);
  Result := True;
End;


Function D3D9Texture.Generate(Pixels: Pointer; Width, Height:Integer; SourceFormat, TargetFormat:TextureColorFormat; ByteFormat:PixelSizeType): Boolean;
Begin
  If (Not _Ready) Then
  Begin
    _Width := Width;
    _Height := Height;

    D3DCheck(D3D9Renderer(_Owner)._Device.CreateTexture(_Width, _Height, 0, D3DUSAGE_AUTOGENMIPMAP, TextureColorFormatToD3D(TargetFormat), D3DPOOL_MANAGED, _Handle, Nil));

    _Ready := True;
    _SettingsChanged := True;
  End;

  _Size := Trunc(4 * _Width * _Height);

  Self.Update(Pixels, 0, 0, Width, Height);
  
  Self._SettingsChanged := True;
  _Ready := True;
End;

Function D3D9Texture.GetImage:Image;
Var
  OutRect:D3DLOCKED_RECT;
Begin
  Result := Image.Create(_Width, _Height);

  _Handle.LockRect(0, OutRect, Nil, 0);
  Move(OutRect.pBits^, Result.Pixels^, Result.Width * Result.Height * 4);
  _Handle.UnlockRect(0);

//	Result.Process(IMP_FlipVertical);
End;

Procedure D3D9Texture.ApplySettings(Slot:Integer);
Begin
  D3D9Renderer(_Owner).ApplyTextureSettings(Slot, Self.NPOT, MipMapped, Filter, WrapMode);
End;

Procedure D3D9Texture.Invalidate;
Begin
  _Handle := Nil;
End;

Procedure D3D9Texture.Update(Pixels: Pointer; X, Y, Width, Height: Integer);
Var
  OutRect:D3DLOCKED_RECT;
  InRect:TRect;
Begin
  InRect.Left := X;
  InRect.Top := Y;
  InRect.Right := X + Width;
  InRect.Bottom := Y + Height;
  _Handle.LockRect(0, OutRect, @InRect, 0);
  Move(Pixels^, OutRect.pBits^, _Width * _Height * 4);
  _Handle.UnlockRect(0);

  If (MipMapped) Then
    Self._SettingsChanged := True;
End;

Procedure D3D9Texture.Release;
Begin
  If Assigned(_Handle) Then
  Begin
    _Handle._Release();
    _Handle := Nil;
  End;
End;

{ D3D9Shader }
(*Procedure D3D9Shader.AddAttributes(Source: TERRAString);
Var
  I:Integer;
  S, S2:TERRAString;
Begin
  _AttributeCount := 0;
  S := Source;
  StringReplaceText('gl_Position', 'IGNORE',S);
  If (Pos('gl_', S)>0) Then
  Begin
     Log(logWarning, 'Shader', 'The following shader has deprecated attributes: '{+ Self.Name});
  End;

  Repeat
    I := Pos('attribute', Source);
    If (I<=0) Then
      Break;

    Source := Copy(Source, I + 10, MaxInt);
    S := StringGetNextSplit(Source, Ord(' '));      // type
    S := StringUpper(S);
    If (S='HIGHP') Or (S='LOWP') Or (S='MEDIUMP') Then
      S := StringGetNextSplit(Source, Ord(' '));      // type
    S2 := StringGetNextSplit(Source, Ord(';'));      // name

    Inc(_AttributeCount);
    SetLength(_Attributes, _AttributeCount);
    _Attributes[Pred(_AttributeCount)].Name := S2;
    _Attributes[Pred(_AttributeCount)].Handle := -1;
  Until (Source='');

  If (_AttributeCount<=0) Then
  Begin
    Log(logWarning, 'Shader', 'The following shader has no attributes: '{+ Self.Name});
  End;
End;*)

Function D3D9Shader.CompileShader(Const Source:TERRAString; IsPixelShader:Boolean): Boolean;
Var
  compileFlags:Cardinal;
  microcode:ID3DXBUFFER;
  errors:ID3DXBUFFER;
  ErrorStr:PAnsiChar;

  CompileStatus, ShaderLength:Integer;
  LogInfo,PS:TERRAString;
  LogLength,slen:Integer;
  Dest:FileStream;
  P:Pointer;
  {$IFDEF DEBUG_SHADERS}
  FileName:TERRAString;
  {$ENDIF}
Begin
  compileFlags := 0;
  Result := False;

  // Create the shader
  // Assemble source into microcode
  If Failed(D3DXAssembleShader(PAnsiChar(Source), Length(Source), Nil,  Nil, compileFlags, @microcode, @errors)) Then
  Begin
    ErrorStr := PAnsiChar(errors.GetBufferPointer());
    Log(logError, 'Renderer', 'Cannot assemble D3D9 shader, errors:' + ErrorStr);
    errors._Release();
    Exit;
  End;

  If IsPixelShader Then
    D3DCheck(D3D9Renderer(_Owner)._Device.CreatePixelShader(microcode.GetBufferPointer(), _PixelShader))
  Else
    D3DCheck(D3D9Renderer(_Owner)._Device.CreateVertexShader(microcode.GetBufferPointer(), _VertexShader));

  Result := True;
End;

Procedure D3D9Shader.Bind;
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Shader', 'Binding shader: ');
  {$ENDIF}

  If (_AttributeCount<=0) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'Shader', 'Adding attributes');
    {$ENDIF}

    AddAttributes(_VertexCode);
  End;

  D3D9Renderer(_Owner)._Device.SetVertexShader(_VertexShader);
  D3D9Renderer(_Owner)._Device.SetPixelShader(_PixelShader);

  _Owner.ActiveShader := Self;

(*  For I:=0 To Pred(_AttributeCount) Do
  Begin
    If (_Attributes[I].Handle<0) Then
    Begin
      _Attributes[I].Handle := glGetAttribLocation(_Program, PAnsiChar(_Attributes[I].Name));
      If (_Attributes[I].Handle<0) Then
      Begin
      {$IFDEF DEBUG_GRAPHICS}
        Log(logError, 'Shader', 'Could not find attribute '+_Attributes[I].Name+' on shader: '+_Name);
      {$ENDIF}
        Continue;
      End;
    End;

    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'Shader', 'Enabling attribarray '+_Attributes[I].Name);
    {$ENDIF}
    glEnableVertexAttribArray(_Attributes[I].Handle);
  End;*)

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Shader', 'End bind');
  {$ENDIF}
End;

Function D3D9Shader.GetAttributeHandle(const Name: TERRAString): Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AttributeCount) Do
  If (_Attributes[I].Name = Name) Then
  Begin
    Result := _Attributes[I].Handle;
    Exit;
  End;

//  Log(logError, 'Shader', 'Attribute '+Name+' not found in shader '+_Name);
  Result := -1;
End;

Function D3D9Shader.GetUniform(Name: TERRAString): Integer;
Begin
  Result := glGetUniformLocation(_Program, PAnsiChar(Name));
End;

Function D3D9Shader.HasUniform(const Name: TERRAString): Boolean;
Begin
  Result := GetUniform(Name)>=0;
End;

Procedure D3D9Shader.Invalidate;
Begin
  If (_Owner.ActiveShader = Self) Then
    _Owner.ActiveShader := Nil;

  _VertexShaderHandle := 0;
  _FragmentShaderHandle := 0;
  _Program := 0;

  _AttributeCount := 0;
//  _Status := rsUnloaded;
  _Linked := False;
End;

Function D3D9Shader.IsReady: Boolean;
Begin
  Result := _Linked;
End;

Procedure D3D9Shader.UniformError(const Name: TERRAString);
Begin
  {$IFDEF PC}
//  Log(logWarning, 'Shader', 'Invalid uniform: '+Name+' in '+Self._Name);
  {$ENDIF}
End;


Procedure D3D9Shader.SetFloatUniform(const Name: TERRAString; const Value: Single);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := glGetUniformLocation(_Program, PAnsiChar(Name));
  If (ID>=0) Then
  Begin
	  glUniform1f(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetIntegerUniform(const Name: TERRAString; const Value: Integer);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := glGetUniformLocation(_Program, PAnsiChar(Name));
  If (ID>=0) Then
  Begin
	  glUniform1i(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetMat3Uniform(const Name: TERRAString; Value: Matrix3x3);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    glUniformMatrix3fv(Id, 1, False, @(Value.V[0]));
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetMat4Uniform(const Name: TERRAString; Value: Matrix4x4);
Var
  ID:Integer;
  IsModelMatrix:Boolean;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
{    If (GraphicsManager.Instance().RenderStage = renderStageReflection) Then
    Begin
      IsModelMatrix := False;

      If Length(Name) = 11 Then
      Begin
        IsModelMatrix := (UpCase(Name[1])='M') And (UpCase(Name[2])='O') And (UpCase(Name[3])='D') And (UpCase(Name[4])='E') And (UpCase(Name[5])='L')
           And (UpCase(Name[6])='M')  And (UpCase(Name[7])='A') And (UpCase(Name[8])='T');
      End;

      If IsModelMatrix Then
        Value := Matrix4x4Multiply4x3(GraphicsManager.Instance().ReflectionMatrix, Value);
    End; BIBI}
      

    glUniformMatrix4fv(Id, 1, False, @(Value.V[0]));
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetVec2Uniform(const Name: TERRAString; const Value: Vector2D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform2fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetVec3Uniform(const Name: TERRAString; const Value: Vector3D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform3fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetVec4Uniform(const Name: TERRAString; const Value: Quaternion);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform4fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.SetVec4ArrayUniform(const Name: TERRAString; Count:Integer; Values: PMatrix4x4);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    glUniform4fv(Id, Count Div 4, Pointer(Values));
  End Else
    UniformError(Name);
End;

Procedure D3D9Shader.Unload;
Begin
  If (_Owner.ActiveShader = Self) Then
    _Owner.ActiveShader := Nil;

  If (_Program>0) Then
  Begin
    If (_VertexShaderHandle>0) Then
    Begin
//      If (Self._Context = Application.Instance.ContextID) Then
      Begin
        glDetachShader(_Program, _VertexShaderHandle);
        glDeleteShader(_VertexShaderHandle);
      End;

      _VertexShaderHandle := 0;
    End;

    If (_FragmentShaderHandle>0) Then
    Begin
//      If (Self._ContextID = Application.Instance.ContextID) Then
      Begin
        glDetachShader(_Program, _FragmentShaderHandle);
        glDeleteShader(_FragmentShaderHandle);
      End;

      _FragmentShaderHandle := 0;
    End;

  //  If (Self._ContextID = Application.Instance.ContextID) Then
    Begin
      glDeleteProgram(_Program);
    End;

    _Program := 0;
  End;

  _AttributeCount := 0;
//  _Status := rsUnloaded;
  _Linked := False;
//	Result := True;
End;

Function D3D9Shader.Generate(const Name: TERRAString; ShaderCode: TERRAString): Boolean;
Var
  I:Integer;

Function ReadBlock(Name:TERRAString):TERRAString;
Var
  S2:TERRAString;
  I:Integer;
  N:Integer;
Begin
  I := Pos(StringUpper(Name), StringUpper(ShaderCode));
  If (I>0) Then
  Begin
    S2 := Copy(ShaderCode, I +1, MaxInt);
    I := Pos('{', S2);
    S2 := Copy(S2, I+1, MaxInt);
    I := 1;
    N := 0;
    Repeat
      If (S2[I]='}') Then
      Begin
        If (N=0) Then
          Break
        Else
          Dec(N);
      End Else
      If (S2[I]='{') Then
        Inc(N);
      Inc(I);
    Until (I>=Length(S2));

    ShaderCode := Copy(S2, I + 1, MaxInt);
    S2 := Copy(S2, 1, I-1);
    Result := S2;
  End Else
    Result := '';

  Result := StringTrim(Result);
End;

Var
  Version:TERRAString;
Begin
  _Name := Name;
  Log(logDebug, 'Shader', 'Creating shader from string: '+ Name);

{  Version := Version + '#define ' + _Owner.Vendor + StringFromChar(NewLineChar);
  If (HasGLSL120) Then
    Version := Version + '#define MATRIX_CAST' + StringFromChar(NewLineChar);

  If (_Owner.Features.PostProcessing.Avaliable) Then
    Version := Version + '#define POSTPROCESSING' + StringFromChar(NewLineChar);

  If (_Owner.Settings.NormalMapping.Enabled) Then
    Version := Version + '#define NORMAL_MAPPING' + StringFromChar(NewLineChar);

{  If (_Owner.Features.FloatTexture.Avaliable) Then
    Version := Version + '#define FLOATBUFFERS' + StringFromChar(NewLineChar);

  If (_Owner.Settings.DepthOfField.Enabled) Then
    Version := Version + '#define DEPTHOFFIELD' + StringFromChar(NewLineChar);

  If (_Owner.Settings.ShadowSplitCount>1) Then
    Version := Version + '#define SHADOWSPLIT1' + StringFromChar(NewLineChar);
  If (_Owner.Settings.ShadowSplitCount>2) Then
    Version := Version + '#define SHADOWSPLIT2' + StringFromChar(NewLineChar);
  If (_Owner.Settings.ShadowSplitCount>3) Then
    Version := Version + '#define SHADOWSPLIT3' + StringFromChar(NewLineChar);}

  Version := ''; //'#version 120'+StringFromChar(NewLineChar);
	_VertexCode := Version + ReadBlock('vertex');
	_FragmentCode := Version + ReadBlock('fragment');

  _MRT := Pos('gl_FragData', _FragmentCode)>0;

//  Inherited Update();

  _AttributeCount := 0;
  If (_VertexCode ='') Or (_FragmentCode ='') Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'Shader', 'Compiling vertex code for ' + Name);

  _Linked := False;
  Result := CompileShader(_VertexCode, GL_VERTEX_SHADER, _VertexShaderHandle);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Compiling fragment code for ' + Name);

  Result := CompileShader(_FragmentCode, GL_FRAGMENT_SHADER, _FragmentShaderHandle);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Linking ' + Name);
  Result := LinkProgram;
  Log(logDebug, 'Shader', 'Finished linking ' +Name+', result='+BoolToString(Result));

  Log(logDebug, 'Shader', 'Shader loaded ok!');
End;



End.