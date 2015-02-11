{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_FrameBufferObject
 * Implements a framebuffer object
 ***********************************************************************************************************************
}

Unit TERRA_FrameBufferObject;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF},
  TERRA_String, TERRA_Utils, TERRA_Image, TERRA_Color, TERRA_Vector2D, TERRA_RenderTarget;

Const
  FBO_COLOR8	= 1;
  FBO_COLOR16	= 2;

Type
  FrameBufferObject = Class(RenderTarget)
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

      _ContextID:Integer;

	    _type:Integer;

      _Complete:Boolean;

  	  // Create a render texture
	    Procedure Init;

  	  // Free OpenGL memory
	    Procedure Release;

      Function GetErrorString(Code:Integer):TERRAString;

	    Procedure InitCapture(Flags:Cardinal); Override;

    Public
	    Constructor Create(Name:TERRAString; Width, Height, ColorType:Integer; DepthBuffer, StencilBuffer, Multisample:Boolean; Targets:Integer);
      Constructor CreateShared(Name:TERRAString; OtherFBO:FrameBufferObject; ColorType, Targets:Integer);
	    Destructor Destroy; Override;

	    // Use it as a texture
	    Procedure Bind(Slot:Integer); Override;

	    Procedure Resize(NewWidth, NewHeight:Integer);

	    // Render to this texture
	    Procedure EndCapture; Override;

      Function GetPixel(X,Y:Integer):Color; Override;
      Function GetImage():Image; Override;

      {$IFDEF IPHONE}
      Procedure PresentToScreen();
      {$ENDIF}
  End;


Implementation
Uses TERRA_Error, TERRA_GraphicsManager, TERRA_Resource, TERRA_Texture, TERRA_Application, TERRA_OS, TERRA_Log;

Function FrameBufferObject.GetErrorString(Code:Integer):TERRAString;
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
        Result := 'Unknow FBO error.';
    End;
End;

Constructor FrameBufferObject.Create(Name:TERRAString; Width, Height, ColorType:Integer; DepthBuffer, StencilBuffer, Multisample:Boolean; Targets:Integer);
Var
  I:Integer;
Begin
  Self._Location := '';
  Self._Name := Name;
  Self._Size := Width * Height * 4 * 2;
  Self._Status := rsReady;

  _ClearColor := ColorCreate(Byte(0), Byte(0), Byte(0), Byte(0));

	_Handle := 0;
	_color_rb := 0;
	_depth_rb := 0;

  _ContextID := Application.Instance.ContextID;

  If (Multisample) And (GraphicsManager.Instance.Settings.MultiSampleCount<=0) Then
    Multisample := False;

  _TargetCount := Targets;
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
	_type := colorType;
	_multisample := multisample;
  _hasDepthBuffer := DepthBuffer;
  _HasStencilBuffer := StencilBuffer;

  _Ratio := VectorCreate2D(1.0, 1.0);

  Log(logDebug,'Framebuffer', 'Creating Framebuffer with size: '+IntToString(_Width)+' x '+IntToString(_Height));
  
  {$IFDEF PC}
	If (_type = FBO_COLOR16) Then
		_internalformat := GL_RGBA16F
	Else
  {$ENDIF}
		_internalformat := GL_RGBA8;

  Self.Init();
End;

Constructor FrameBufferObject.CreateShared(Name:TERRAString; OtherFBO:FrameBufferObject; ColorType, Targets:Integer);
Var
  I, Status:Integer;
Begin
  Self._Location := '';
  Self._Name := Name;
  Self._Size := Width * Height * 4 * 2;
  Self._Status := rsReady;

	_Handle := 0;
	_color_rb := 0;
	_depth_rb := OtherFBO._depth_rb;

  _TargetCount := Targets;
  SetLength(_Targets, _TargetCount);
  SetLength(_DrawBuffers, _TargetCount);
  For I:=0 To Pred(_TargetCount) Do
  Begin
    _targets[I] := 0;
    _DrawBuffers[I] := GL_COLOR_ATTACHMENT0 + I;
  End;

  _Shared := True;

	_Width := OtherFBO.Width;
	_Height := OtherFBO.Height;
	_type := ColorType;
	_multisample := false;
  _hasDepthBuffer := OtherFBO._hasDepthBuffer;
  _HasStencilBuffer := OtherFBO._hasStencilBuffer;

  {$IFDEF PC}
	If (_type = FBO_COLOR16) Then
		_internalformat := GL_RGBA16F
	Else
  {$ENDIF}
		_internalformat := GL_RGBA8;

		// initalize FrameBufferObject
    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();

		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

		// initialize color texture
    For I:=0 To Pred(_TargetCount) Do
    Begin
      _Targets[I] := GraphicsManager.Instance.GenerateTexture();
	  	glBindTexture(GL_TEXTURE_2D, _Targets[I]);
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      {$IFDEF PC}
      If (_type=FBO_COLOR16) Then
		  Begin
			  glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_HALF_FLOAT, Nil);
      End Else
      {$ENDIF}
  		Begin
	  	  glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);        
      End;
			glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + I, GL_TEXTURE_2D, _Targets[I], 0);      
		End;

  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);      
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);

	// check for errors
	Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  _Complete := (Status = GL_FRAMEBUFFER_COMPLETE);

  If Not _Complete Then
    Log(logError, 'Framebuffer', GetErrorString(Status));

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
End;

Destructor FrameBufferObject.Destroy;
Begin
  Self.Release;
End;

Procedure FrameBufferObject.Bind(Slot:Integer);
Begin
  If (Not _Complete) Then
  Begin
    TextureManager.Instance.NullTexture.Bind(Slot);
  End;

	glActiveTexture(GL_TEXTURE0 + Slot);
  {$IFDEF PC}
	glEnable(GL_TEXTURE_2D);
  {$ENDIF}

	glBindTexture(GL_TEXTURE_2D, _Targets[0]);
End;

Procedure FrameBufferObject.Init;
Var
  I, Status:Integer;
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType,'Init');{$ENDIF}

  Log(logDebug, 'Framebuffer','Initializing framebuffer: '+_Name);

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

  If (_Name='device_target0') Then
  Begin
    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();
    glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

    _color_rb := GraphicsManager.Instance.GenerateRenderBuffer();
    glBindRenderbuffer(GL_RENDERBUFFER, _color_rb);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, _color_rb);
    SetRenderbufferStorage(); //[context renderbufferStorage:GL_RENDERBUFFER fromDrawable:(CAEAGLLayer*)self.layer];
    Log(logDebug,'Framebuffer', 'Linked framebuffer to display memory');

    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_WIDTH, @_Width);
    glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_HEIGHT, @_Height);
    Log(logDebug,'Framebuffer', 'Framebuffer size:  '+IntToString(_Width)+' x '+IntToString(_Height));

    _depth_rb := GraphicsManager.Instance.GenerateRenderBuffer();
    glBindRenderbuffer(GL_RENDERBUFFER, _depth_rb);

    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, _Width, _Height);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
  End;
  {$ENDIF}

  {$IFDEF MOBILE}
  If (_Handle = 0) Then
  Begin
    // Create a framebuffer and renderbuffer
    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();
    _depth_rb := GraphicsManager.Instance.GenerateRenderBuffer();
    Log(logDebug,'Framebuffer', 'Created framebuffer with handle: '+IntToString(_Handle));

    // Create a texture to hold the frame buffer
    _Targets[0] := GraphicsManager.Instance.GenerateTexture();
	  glBindTexture(GL_TEXTURE_2D, _Targets[0]);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, _Width, _Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);
    Log(logDebug,'Framebuffer', 'Framebuffer size: W: '+IntToString(_Width)+' H: '+IntToString(_Height));

    If GraphicsManager.Instance.Settings.PackedStencil.Avaliable Then
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

    If GraphicsManager.Instance.Settings.PackedStencil.Avaliable Then
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
  End;
  {$ENDIF}

  {$IFDEF PC}
	If (_multisample) Then
	Begin
    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();
		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);

		_Targets[0] := GraphicsManager.Instance.GenerateTexture();
		glBindTexture(GL_TEXTURE_2D, _Targets[0]);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

		// initialize color texture
    {$IFDEF PC}
    If (_type = FBO_COLOR16) Then
    Begin
      _internalformat := GL_RGBA16F;
      glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_HALF_FLOAT, Nil);      
    End Else
    {$ENDIF}
    Begin
      _internalformat := GL_RGBA8;
      glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);    
		End;
		glBindTexture(GL_TEXTURE_2D, 0);      

    _color_rb := GraphicsManager.Instance.GenerateRenderBuffer();
		glBindRenderbuffer(GL_RENDERBUFFER, _color_rb);
		glRenderbufferStorageMultisample(GL_RENDERBUFFER, GraphicsManager.Instance.Settings.MultiSampleCount , _internalformat, Width, Height);
		glBindRenderbuffer(GL_RENDERBUFFER, 0);    

    If (_HasDepthBuffer) Then
    Begin
  		_depth_rb := GraphicsManager.Instance.GenerateRenderBuffer();             
	  	glBindRenderbuffer(GL_RENDERBUFFER, _depth_rb);      
  		glRenderbufferStorageMultisample(GL_RENDERBUFFER, GraphicsManager.Instance.Settings.MultiSampleCount, {$IFDEF IPHONE}GL_DEPTH24_STENCIL8_OES{$ELSE}GL_DEPTH24_STENCIL8{$ENDIF}, Width, Height);      
	  	glBindRenderbuffer(GL_RENDERBUFFER, 0);
    End;

    _MFB := GraphicsManager.Instance.GenerateFrameBuffer();
		glBindFramebuffer(GL_FRAMEBUFFER, _mfb);    
		glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, _color_rb);

    If (_HasDepthBuffer) Then
    Begin
  	  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);      
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
    End;

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();
		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);    
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _Targets[0], 0);    
	End Else
	Begin
		// initalize FrameBufferObject
    _Handle := GraphicsManager.Instance.GenerateFrameBuffer();
		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);
    Log(logDebug,'Framebuffer', 'Created framebuffer with handle: '+IntToString(_Handle));

		// initialize color texture
    For I:=0 To Pred(_TargetCount) Do
    Begin
      _Targets[I] := GraphicsManager.Instance.GenerateTexture();
	  	glBindTexture(GL_TEXTURE_2D, _Targets[I]);      
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);      
	  	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);      

      If (_type=FBO_COLOR16) Then
		  Begin
			  glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_HALF_FLOAT, Nil);
      End Else
  		Begin
	  	  glTexImage2D(GL_TEXTURE_2D, 0, _internalformat, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);
      End;
			glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + I, GL_TEXTURE_2D, _Targets[I], 0);

      Log(logDebug,'Framebuffer', 'Binding texture to framebuffer with handle: '+IntToString(_Targets[I]));
    End;

		If (_HasDepthBuffer) Then
		Begin
			// initialize depth renderbuffer
			_depth_rb := GraphicsManager.Instance.GenerateRenderBuffer();
			glBindRenderbuffer(GL_RENDERBUFFER, _depth_rb);
			glRenderbufferStorage(GL_RENDERBUFFER, {$IFDEF IPHONE}GL_DEPTH24_STENCIL8_OES{$ELSE}GL_DEPTH24_STENCIL8{$ENDIF}, Width, Height);

			glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, _depth_rb);
		End;
	End;
  {$ENDIF}

	// check for errors
	Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);    
  If (Status<>GL_FRAMEBUFFER_COMPLETE) Then
			RaiseError(GetErrorString(Status));

  // set default framebuffer
	glBindFramebuffer(GL_FRAMEBUFFER, 0);    

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure FrameBufferObject.Release;
Var
  I:Integer;
Begin
  If (_ContextID <> Application.Instance.ContextID) Then
  Begin
        _Handle := 0;
		_color_rb := 0;
		_depth_rb := 0;
        _stencil_rb := 0;

    For I:=0 To Pred(_TargetCount) Do
  		_Targets[I] := 0;

    Exit;
  End;

  GraphicsManager.Instance.DeleteRenderBuffer(_color_rb);

	If (Not _Shared) Then
    Begin
		GraphicsManager.Instance.DeleteRenderBuffer(_depth_rb);
        GraphicsManager.Instance.DeleteRenderBuffer(_stencil_rb);
    End;

  For I:=0 To Pred(_TargetCount) Do
		GraphicsManager.Instance.DeleteTexture(_Targets[I]);

  GraphicsManager.Instance.DeleteFrameBuffer(_Handle);
End;

Procedure FrameBufferObject.Resize(NewWidth, NewHeight:Integer);
Begin
	Self.Release;
  _Width := NewWidth;
  _Height := NewHeight;
	Self.Init;
End;

Procedure FrameBufferObject.InitCapture(Flags:Cardinal);
Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType,'BeginCapture');{$ENDIF}

  If (_Handle = 0) Then
    Self.Init();

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','Begin framebuffer capture: '+_Name+' W:'+IntToString(_Width)+' H:'+IntToString(_Height));{$ENDIF}

	If (_multisample) Then
  Begin
		glBindFramebuffer(GL_FRAMEBUFFER, _mfb);
	End Else
  Begin
		glBindFramebuffer(GL_FRAMEBUFFER, _Handle);
  End;

  {$IFDEF PC}
  glDrawBuffers(_TargetCount, @_DrawBuffers[0]);
  {$ENDIF}

  GraphicsManager.Instance.ActiveViewport.SetViewArea(0, 0, Width, _Height);

  If (Flags<>0) Then
  Begin
    glClearStencil(0);
    glClearColor(_ClearColor.R/255.0, _ClearColor.G/255.0, _ClearColor.B/255.0, _ClearColor.A/255.0);
    glClear(Flags);
  End;

  _Active := True;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure FrameBufferObject.EndCapture;

Begin
  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'EndCapture');{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','End framebuffer capture: '+_Name);{$ENDIF}

  {$IFDEF PC}
	If (_multisample) Then
	Begin
		glBindFramebuffer(GL_READ_FRAMEBUFFER, _mfb);
		glReadBuffer(GL_COLOR_ATTACHMENT0);
		glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _Handle);
		glDrawBuffer(GL_COLOR_ATTACHMENT0);
		glBlitFramebuffer(0, 0, _Width, _Height, 0, 0, _Width, _Height, GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT, GL_NEAREST);
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
	End Else
  {$ENDIF}
  Begin
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
  End;

  _Active := False;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Function FrameBufferObject.GetPixel(X,Y:Integer):Color;
Var
  P:Color;
Begin
  Y := Height - Y;
  {$IFDEF PC}
	glBindFramebuffer(GL_FRAMEBUFFER, _Handle);    
	glReadBuffer(GL_COLOR_ATTACHMENT0);           
	glReadPixels(X,Y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @P);  
	glBindFramebuffer(GL_FRAMEBUFFER, 0);             
  {$ELSE}
  P := ColorNull;
  {$ENDIF}
  Result := P;
End;

Function FrameBufferObject.GetImage():Image;
Begin
  Result := Image.Create(_Width, _Height);

	glBindFramebuffer(GL_FRAMEBUFFER, _Handle);
  {$IFDEF PC}
	glReadBuffer(GL_COLOR_ATTACHMENT0 + 0);
  {$ENDIF}

	glReadPixels(0,0, _Width, _Height, GL_RGBA, GL_UNSIGNED_BYTE, Result.Pixels);
	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	Result.Process(IMP_FlipVertical);
End;

{$IFDEF IPHONE}
Procedure FrameBufferObject.PresentToScreen;
Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Framebuffer','Presenting framebuffer: '+_Name);{$ENDIF}

  glBindRenderbuffer(GL_RENDERBUFFER, _color_rb);
  PresentRenderBuffer();
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
End;
{$ENDIF}

End.


