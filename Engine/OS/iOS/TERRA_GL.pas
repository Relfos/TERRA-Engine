{
@abstract(OpenGL)
@author(Sergio Flores <relfos@gmail.com>)
@created(February 25, 2006)
@lastmod(March 1, 2006)
The GL unit provides a cross-plataform OpenGL interface.
Supports versions upto OpenGL 2.0.
Only extensions used by LEAF are included.
}

Unit TERRA_GL;
{$I terra.inc}

{$PACKRECORDS C}

Interface
Uses TERRA_Utils, TERRA_Log, DynLibs, Math;

Const
  OpenGLLibName='/System/Library/Frameworks/OpenGLES.framework/OpenGLES';

{$I glconsts.inc}

// GL core functions.
Var
  glActiveTexture:Procedure (texture:Integer); cdecl;
  glAttachShader:Procedure (programname, shader:Cardinal); cdecl;
  glBindAttribLocation:Procedure (_program:Integer; index:Integer; name:PTERRAChar); cdecl;
  glBindBuffer:Procedure (target, buffer:Integer); cdecl;
  glBindFramebuffer:Procedure (target, framebuffer:Cardinal); cdecl;
  glBindRenderbuffer:Procedure (target, renderbuffer:Cardinal); cdecl;
  glBindTexture:Procedure (target, texture:Cardinal); cdecl;
  glBlendColor:Procedure (red, green, blue, alpha:Single); cdecl;
  glBlendEquation:Procedure ( mode:Cardinal); cdecl;
  glBlendEquationSeparate:Procedure (modeRGB, modeAlpha:Cardinal); cdecl;
  glBlendFunc:Procedure (sfactor, dfactor:Cardinal); cdecl;
  glBlendFuncSeparate:Procedure (srcRGB, dstRGB, srcAlpha, dstAlpha:Cardinal); cdecl;
  glBufferData :Procedure(target, size:Cardinal; data:Pointer; usage:Cardinal); cdecl;
  glBufferSubData:Procedure (target, offset, size:Cardinal; data:Pointer); cdecl;
  glCheckFramebufferStatus:Function (target:Integer):Integer; cdecl;
  glClear:Procedure (mask:Cardinal); cdecl;
  glClearColor:Procedure (red, green, blue, alpha:Single); cdecl;
  glClearDepthf:Procedure (depth:Single); cdecl;
  glClearStencil:Procedure (s:Cardinal); cdecl;
  glColorMask:Procedure (red, green, blue, alpha:Boolean); cdecl;
  glCompileShader:Procedure (shader:Cardinal); cdecl;
  glCompressedTexImage2D:Procedure (target, level, internalformat, width, height, border, imageSize:Cardinal; data:Pointer); cdecl;
  glCompressedTexSubImage2D:Procedure (target, level, xoffset, yoffset, width, height, format, imageSize:Cardinal; data:Pointer); cdecl;
  glCopyTexImage2D:Procedure (target, level, internalformat, x, y, width, height, border:Cardinal); cdecl;
  glCopyTexSubImage2D:Procedure (target, level, xoffset, yoffset, x, y, width,height:Cardinal); cdecl;
  glCreateProgram:Function ():Integer; cdecl;
  glCreateShader:Function (shadertype:Integer):Integer; cdecl;
  glCullFace:Procedure (mode:Cardinal); cdecl;
  glDeleteBuffers:Procedure (n:Cardinal;  buffers:PCardinal); cdecl;
  glDeleteFramebuffers:Procedure (n:Cardinal; framebuffers:PCardinal); cdecl;
  glDeleteProgram:Procedure (_program:Cardinal); cdecl;
  glDeleteRenderbuffers:Procedure (n:Cardinal; renderbuffers:PCardinal); cdecl;
  glDeleteShader:Procedure (shader:Cardinal); cdecl;
  glDeleteTextures:Procedure (n:Cardinal; textures:PCardinal); cdecl;
  glDepthFunc:Procedure (func:Cardinal); cdecl;
  glDepthMask:Procedure (flag:Boolean); cdecl;
  glDepthRange:Procedure (zNear, zFar:Single); cdecl;
  glDetachShader:Procedure (_program, shader:Cardinal); cdecl;
  glDisable:Procedure (cap:Cardinal); cdecl;
  glDisableVertexAttribArray:Procedure (index:Cardinal); cdecl;
  glDrawArrays:Procedure (mode, first, count:Cardinal); cdecl;
  glDrawElements:Procedure (mode, count, _type:Cardinal; indices:Pointer); cdecl;
  glEnable:Procedure (cap:Cardinal); cdecl;
  glEnableVertexAttribArray:Procedure (index:Cardinal); cdecl;
  glFinish:Procedure (); cdecl;
  glFlush:Procedure (); cdecl;
  glFramebufferRenderbuffer:Procedure (target, attachment, renderbuffertarget, renderbuffer:Cardinal); cdecl;
  glFramebufferTexture2D:Procedure (target, attachment, textarget, texture, level:Cardinal); cdecl;
  glFrontFace:Procedure (mode:Cardinal); cdecl;
  glGenBuffers:Procedure (n:Cardinal; buffers:PCardinal); cdecl;
  glGenerateMipmap:Procedure (target:Cardinal); cdecl;
  glGenFramebuffers:Procedure (n:Cardinal; framebuffers:PCardinal); cdecl;
  glGenRenderbuffers:Procedure (n:Cardinal; renderbuffers:PCardinal); cdecl;
  glGenTextures:Procedure (n:Cardinal; textures:PCardinal); cdecl;
  glGetActiveAttrib:Procedure (_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PTERRAChar); cdecl;
  glGetActiveUniform:Procedure (_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PTERRAChar); cdecl;
  glGetAttachedShaders:Procedure (_program, maxcount:Cardinal; Var count:Integer; shaders:PCardinal); cdecl;
  glGetAttribLocation:Function (_program:Cardinal; name:PTERRAChar):Integer; cdecl;
  glGetBooleanv:Procedure (pname:Cardinal; params:PBoolean); cdecl;
  glGetBufferParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetError:Function ():Integer; cdecl;
  glGetFloatv:Procedure (pname:Cardinal; params:PSingle); cdecl;
  glGetFramebufferAttachmentParameteriv:Procedure (target, attachment, pname:Cardinal; params:PInteger); cdecl;
  glGetIntegerv :Procedure(pname:Cardinal; params:PInteger); cdecl;
  glGetProgramiv :Procedure(_program,pname:Cardinal; params:PInteger); cdecl;
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar); cdecl;
  glGetRenderbufferParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetShaderiv:Procedure (shader, pname:Cardinal; params:PInteger); cdecl;
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar); cdecl;
  glGetShaderPrecisionFormat:Procedure (shadertype, precisiontype:Cardinal; Var range, precision:Integer); cdecl;
  glGetShaderSource:Procedure (shader, bufsize:Cardinal; Var length:Integer; source:PTERRAChar); cdecl;
  glGetString:Function (name:Cardinal):PTERRAChar; cdecl;
  glGetTexParameterfv:Procedure (target, pname:Cardinal; params:PSingle); cdecl;
  glGetTexParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetUniformfv:Procedure (_program, location:Cardinal;  params:PSingle); cdecl;
  glGetUniformiv:Procedure (_program, location:Cardinal; params:PInteger); cdecl;
  glGetUniformLocation:Function (_program:Cardinal; name:PTERRAChar):Integer; cdecl;
  glGetVertexAttribfv:Procedure (index, pname:Cardinal; params:PSingle); cdecl;
  glGetVertexAttribiv:Procedure (index, pname:Cardinal; params:PInteger); cdecl; 
  glGetVertexAttribPointerv:Procedure (index, pname:Cardinal; pointer:PPointer); cdecl;
  glHint:Procedure (target, mode:Cardinal); cdecl;
  glIsBuffer:Function (buffer:Cardinal):Boolean; cdecl;
  glIsEnabled:Function (cap:Cardinal):Boolean; cdecl;
  glIsFramebuffer:Function (framebuffer:Cardinal):Boolean; cdecl;
  glIsProgram:Function (_program:Cardinal):Boolean; cdecl;
  glIsRenderbuffer:Function (renderbuffer:Cardinal):Boolean; cdecl;
  glIsShader:Function (shader:Cardinal):Boolean; cdecl;
  glIsTexture:Function (texture:Cardinal):Boolean; cdecl; 
  glLineWidth:Procedure (width:Single); cdecl;
  glLinkProgram:Procedure (_program:Cardinal); cdecl;
  glPixelStorei:Procedure (pname:Cardinal; param:Integer); cdecl;
  glPolygonOffset:Procedure (factor, units:Single); cdecl;
  glReadPixels:Procedure (x, y, width, height, format, _type:Cardinal; pixels:Pointer); cdecl;
  glReleaseShaderCompiler :Procedure(); cdecl;
  glRenderbufferStorage:Procedure (target, internalformat, width, height:Cardinal); cdecl;
  glSampleCoverage:Procedure (value:Single; invert:Boolean); cdecl;
  glScissor:Procedure (x, y, width, height:Cardinal); cdecl;
  glShaderBinary:Procedure (n:Cardinal; shaders:PCardinal; binaryformat:Cardinal; binary:Pointer; length:Cardinal); cdecl;
  glShaderSource:Procedure (shader: Cardinal; count: Integer; const _string: PTERRAChar; const length: PInteger); cdecl;
  glStencilFunc:Procedure (func, ref, mask:Cardinal); cdecl;
  glStencilFuncSeparate:Procedure (face, func, ref, mask:Cardinal); cdecl;
  glStencilMask :Procedure(mask:Cardinal); cdecl;
  glStencilMaskSeparate:Procedure (face, mask:Cardinal); cdecl;
  glStencilOp:Procedure (fail, zfail, zpass:Cardinal); cdecl;
  glStencilOpSeparate:Procedure (face, fail, zfail, zpass:Cardinal); cdecl;
  glTexImage2D :Procedure(target, level, internalformat, width, height, border, format, _type:Cardinal; pixels:Pointer); cdecl;
  glTexParameterf:Procedure (target, pname:Cardinal; param:Single); cdecl;
  glTexParameterfv:Procedure (target, pname:Cardinal; params:PSingle); cdecl;
  glTexParameteri:Procedure (target, pname:Cardinal; param:Integer); cdecl;
  glTexParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glTexSubImage2D:Procedure (target, level, xoffset, yoffset, width, height, format, _type:Cardinal; pixels:Pointer); cdecl;

  glUniform1f: procedure(location: Integer; v0: Single); cdecl;
  glUniform2f: procedure(location: Integer; v0: Single; v1: Single); cdecl;
  glUniform3f: procedure(location: Integer; v0: Single; v1: Single; v2: Single); cdecl;
  glUniform4f: procedure(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); cdecl;
  glUniform1i: procedure(location: Integer; v0: Integer); cdecl;
  glUniform2i: procedure(location: Integer; v0: Integer; v1: Integer); cdecl;
  glUniform3i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer); cdecl;
  glUniform4i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); cdecl;
  glUniform1fv: procedure(location: Integer; count: Integer; const value: PSingle); cdecl;
  glUniform2fv: procedure(location: Integer; count: Integer; const value: PSingle); cdecl;
  glUniform3fv: procedure(location: Integer; count: Integer; const value: PSingle); cdecl;
  glUniform4fv: procedure(location: Integer; count: Integer; const value: PSingle); cdecl;
  glUniform1iv: procedure(location: Integer; count: Integer; const value: PInteger); cdecl;
  glUniform2iv: procedure(location: Integer; count: Integer; const value: PInteger); cdecl;
  glUniform3iv: procedure(location: Integer; count: Integer; const value: PInteger); cdecl;
  glUniform4iv: procedure(location: Integer; count: Integer; const value: PInteger); cdecl;
  glUniformMatrix2fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl;
  glUniformMatrix3fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl;
  glUniformMatrix4fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl;

  glUseProgram:Procedure (_program:Cardinal); cdecl;
  glValidateProgram:Procedure (_program:Cardinal); cdecl;
  glVertexAttrib1f:Procedure (indx:Cardinal; x:Single); cdecl;
  glVertexAttrib1fv:Procedure (indx:Cardinal; values:PSingle); cdecl;
  glVertexAttrib2f:Procedure (indx:Cardinal; x, y:Single); cdecl;
  glVertexAttrib2fv:Procedure (indx:Cardinal; values:PSingle); cdecl;
  glVertexAttrib3f:Procedure (indx:Cardinal; x, y, z:Single); cdecl;
  glVertexAttrib3fv:Procedure (indx:Cardinal; values:PSingle); cdecl;
  glVertexAttrib4f:Procedure (indx:Cardinal; x, y, z, w:Single); cdecl;
  glVertexAttrib4fv:Procedure (indx:Cardinal; values:PSingle); cdecl;
  glVertexAttribPointer: procedure(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); cdecl;
  glViewport:Procedure (x, y, width, height:Cardinal); cdecl;

  // GL ES1
  glAlphaFunc:Procedure (func: Cardinal; ref: Single); cdecl;
  glEnableClientState: Procedure(aarray: Cardinal); cdecl;
  glDisableClientState: Procedure(aarray: Cardinal); cdecl;
  glVertexPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glTexCoordPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glColorPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glColor4f:Procedure (red, green, blue, alpha: Single); cdecl;
  glMatrixMode:Procedure (mode: Cardinal); cdecl;
  glLoadMatrixf:Procedure (const m: PSingle); cdecl;


  // Extensions
  glRenderbufferStorageMultisample:Procedure (target, samples, internalformat, width, height:Cardinal); cdecl;
  glResolveMultisampleFramebuffer:Procedure  (); cdecl;

  glDiscardFramebuffer: Procedure(target, count:Integer; attachments:PInteger);

Procedure glClearDepth(depth:Single);
  
Procedure glLoadExtensions;

Function glExtensionSupported(Extension:TERRAString):Boolean;
Function glGetExtensionString():TERRAString;


Implementation
Uses TERRA_Error, TERRA_Application;

Var
  OpenGLHandle:TLibHandle;
  ExtensionsList:TERRAString='';


Procedure glClearDepth(depth:Single);
Begin
  glClearDepthf(depth);
End;
  
Function glGetExtensionString():TERRAString;
Begin
  Result := ExtensionsList;
End;

Function glGetProcAddress(Proc:TERRAString):Pointer;
Begin
  Result := GetProcAddress(OpenGLHandle, PTERRAChar(Proc));
  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PTERRAChar(Proc+'OES'));

  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PTERRAChar(Proc+'APPLE'));

  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PTERRAChar(Proc+'EXT'));

  If Not Assigned(Result) Then
    Log(logWarning,'GL', 'Function '+Proc+' not avaliable.');
End;

Procedure LoadOpenGL(LibName:TERRAString);
Begin
	TERRA_Log.Log(logDebug, 'GL', 'loading openGL');
  Log(logDebug, 'OpenGL', 'Loading library');

  OpenGLHandle := LoadLibrary(PTERRAChar(LibName));
  If OpenGLHandle=0 Then
  Begin
    RaiseError('Error loading OpenGL from '+LibName);
    Exit;
  End;

  glActiveTexture := glGetProcAddress('glActiveTexture');
  glAttachShader := glGetProcAddress('glAttachShader');
  glBindAttribLocation := glGetProcAddress('glBindAttribLocation');
  glBindBuffer := glGetProcAddress('glBindBuffer');
  glBindFramebuffer := glGetProcAddress('glBindFramebuffer');
  glBindRenderbuffer := glGetProcAddress('glBindRenderbuffer');
  glBindTexture := glGetProcAddress('glBindTexture');
  glBlendColor := glGetProcAddress('glBlendColor');
  glBlendEquation := glGetProcAddress('glBlendEquation');
  glBlendEquationSeparate := glGetProcAddress('glBlendEquationSeparate');
  glBlendFunc := glGetProcAddress('glBlendFunc');
  glBlendFuncSeparate := glGetProcAddress('glBlendFuncSeparate ');
  glBufferData := glGetProcAddress('glBufferData');
  glBufferSubData := glGetProcAddress('glBufferSubData');
  glCheckFramebufferStatus := glGetProcAddress('glCheckFramebufferStatus');
  glClear := glGetProcAddress('glClear');
  glClearColor := glGetProcAddress('glClearColor');
  glClearDepthf := glGetProcAddress('glClearDepthf');
  glClearStencil := glGetProcAddress('glClearStencil');
  glColorMask := glGetProcAddress('glColorMask');
  glCompileShader := glGetProcAddress('glCompileShader');
  glCompressedTexImage2D := glGetProcAddress('glCompressedTexImage2D');
  glCompressedTexSubImage2D := glGetProcAddress('glCompressedTexSubImage2D');
  glCopyTexImage2D := glGetProcAddress('glCopyTexImage2D');
  glCopyTexSubImage2D := glGetProcAddress('glCopyTexSubImage2D');
  glCreateProgram := glGetProcAddress('glCreateProgram');
  glCreateShader := glGetProcAddress('glCreateShader');
  glCullFace := glGetProcAddress('glCullFace');
  glDeleteBuffers := glGetProcAddress('glDeleteBuffers');
  glDeleteFramebuffers := glGetProcAddress('glDeleteFramebuffers');
  glDeleteProgram := glGetProcAddress('glDeleteProgram');
  glDeleteRenderbuffers := glGetProcAddress('glDeleteRenderbuffers');
  glDeleteShader := glGetProcAddress('glDeleteShader');
  glDeleteTextures := glGetProcAddress('glDeleteTextures');
  glDepthFunc := glGetProcAddress('glDepthFunc');
  glDepthMask := glGetProcAddress('glDepthMask');
  glDepthRange := glGetProcAddress('glDepthRangef');
  glDetachShader := glGetProcAddress('glDetachShader');
  glDisable := glGetProcAddress('glDisable');
  glDisableVertexAttribArray := glGetProcAddress('glDisableVertexAttribArray');
  glDrawArrays := glGetProcAddress('glDrawArrays');
  glDrawElements := glGetProcAddress('glDrawElements');
  glEnable := glGetProcAddress('glEnable');
  glEnableVertexAttribArray := glGetProcAddress('glEnableVertexAttribArray');
  glFinish := glGetProcAddress('glFinish');
  glFlush := glGetProcAddress('glFlush');
  glFramebufferRenderbuffer := glGetProcAddress('glFramebufferRenderbuffer');
  glFramebufferTexture2D := glGetProcAddress('glFramebufferTexture2D');
  glFrontFace := glGetProcAddress('glFrontFace');
  glGenBuffers := glGetProcAddress('glGenBuffers');
  glGenerateMipmap := glGetProcAddress('glGenerateMipmap');
  glGenFramebuffers := glGetProcAddress('glGenFramebuffers');
  glGenRenderbuffers := glGetProcAddress('glGenRenderbuffers');
  glGenTextures := glGetProcAddress('glGenTextures');
  glGetActiveAttrib := glGetProcAddress('glGetActiveAttrib');
  glGetActiveUniform := glGetProcAddress('glGetActiveUniform');
  glGetAttachedShaders := glGetProcAddress('glGetAttachedShaders');
  glGetAttribLocation := glGetProcAddress('glGetAttribLocation');
  glGetBooleanv := glGetProcAddress('glGetBooleanv');
  glGetBufferParameteriv := glGetProcAddress('glGetBufferParameteriv');
  glGetError := glGetProcAddress('glGetError');
  glGetFloatv := glGetProcAddress('glGetFloatv');
  glGetFramebufferAttachmentParameteriv := glGetProcAddress('glGetFramebufferAttachmentParameteriv');
  glGetIntegerv := glGetProcAddress('glGetIntegerv');
  glGetProgramiv := glGetProcAddress('glGetProgramiv');
  glGetProgramInfoLog := glGetProcAddress('glGetProgramInfoLog');
  glGetRenderbufferParameteriv := glGetProcAddress('glGetRenderbufferParameteriv');
  glGetShaderiv := glGetProcAddress('glGetShaderiv');
  glGetShaderInfoLog := glGetProcAddress('glGetShaderInfoLog');
  glGetShaderPrecisionFormat := glGetProcAddress('glGetShaderPrecisionFormat');
  glGetShaderSource := glGetProcAddress('glGetShaderSource');
  glGetString := glGetProcAddress('glGetString');
  glGetTexParameterfv := glGetProcAddress('glGetTexParameterfv');
  glGetTexParameteriv := glGetProcAddress('glGetTexParameteriv');
  glGetUniformfv := glGetProcAddress('glGetUniformfv');
  glGetUniformiv := glGetProcAddress('glGetUniformiv');
  glGetUniformLocation := glGetProcAddress('glGetUniformLocation');
  glGetVertexAttribfv := glGetProcAddress('glGetVertexAttribfv');
  glGetVertexAttribiv := glGetProcAddress('glGetVertexAttribiv');
  glGetVertexAttribPointerv := glGetProcAddress('glGetVertexAttribPointerv');
  glHint := glGetProcAddress('glHint');
  glIsBuffer := glGetProcAddress('glIsBuffer');
  glIsEnabled := glGetProcAddress('glIsFramebuffer');
  glIsFramebuffer := glGetProcAddress('glIsFramebuffer');
  glIsProgram := glGetProcAddress('glIsProgram');
  glIsRenderbuffer := glGetProcAddress('glIsRenderbuffer');
  glIsShader := glGetProcAddress('glIsShader');
  glIsTexture := glGetProcAddress('glIsTexture');
  glLineWidth := glGetProcAddress('glLineWidth');
  glLinkProgram := glGetProcAddress('glLinkProgram');
  glPixelStorei := glGetProcAddress('glPixelStorei');
  glPolygonOffset := glGetProcAddress('glPolygonOffset');
  glReadPixels := glGetProcAddress('glReadPixels');
  glReleaseShaderCompiler := glGetProcAddress('glReleaseShaderCompiler');
  glRenderbufferStorage := glGetProcAddress('glRenderbufferStorage');
  glSampleCoverage := glGetProcAddress('glSampleCoverage');
  glScissor := glGetProcAddress('glScissor');
  glShaderBinary := glGetProcAddress('glShaderBinary');
  glShaderSource := glGetProcAddress('glShaderSource');
  glStencilFunc := glGetProcAddress('glStencilFunc');
  glStencilFuncSeparate := glGetProcAddress('glStencilFuncSeparate');
  glStencilMask := glGetProcAddress('glStencilMask');
  glStencilMaskSeparate := glGetProcAddress('glStencilMaskSeparate');
  glStencilOp := glGetProcAddress('glStencilOp');
  glStencilOpSeparate := glGetProcAddress('glStencilOpSeparate');
  glTexImage2D := glGetProcAddress('glTexImage2D');
  glTexParameterf := glGetProcAddress('glTexParameterf');
  glTexParameterfv := glGetProcAddress('glTexParameterfv');
  glTexParameteri := glGetProcAddress('glTexParameteri');
  glTexParameteriv := glGetProcAddress('glTexParameteriv');
  glTexSubImage2D := glGetProcAddress('glTexSubImage2D');
  glUniform1f := glGetProcAddress('glUniform1f');
  glUniform1fv := glGetProcAddress('glUniform1fv');
  glUniform1i := glGetProcAddress('glUniform1i');
  glUniform1iv := glGetProcAddress('glUniform1iv');
  glUniform2f := glGetProcAddress('glUniform2f');
  glUniform2fv := glGetProcAddress('glUniform2fv');
  glUniform2i := glGetProcAddress('glUniform2i');
  glUniform2iv := glGetProcAddress('glUniform2iv');
  glUniform3f := glGetProcAddress('glUniform3f');
  glUniform3fv := glGetProcAddress('glUniform3fv');
  glUniform3i := glGetProcAddress('glUniform3i');
  glUniform3iv := glGetProcAddress('glUniform3iv');
  glUniform4f := glGetProcAddress('glUniform4f');
  glUniform4fv := glGetProcAddress('glUniform4fv');
  glUniform4i := glGetProcAddress('glUniform4i');
  glUniform4iv := glGetProcAddress('glUniform4iv');
  glUniformMatrix2fv := glGetProcAddress('glUniformMatrix2fv');
  glUniformMatrix3fv := glGetProcAddress('glUniformMatrix3fv');
  glUniformMatrix4fv := glGetProcAddress('glUniformMatrix4fv');
  glUseProgram := glGetProcAddress('glUseProgram');
  glValidateProgram := glGetProcAddress('glValidateProgram');
  glVertexAttrib1f := glGetProcAddress('glVertexAttrib1f');
  glVertexAttrib1fv := glGetProcAddress('glVertexAttrib1fv');
  glVertexAttrib2f := glGetProcAddress('glVertexAttrib2f');
  glVertexAttrib2fv := glGetProcAddress('glVertexAttrib2fv');
  glVertexAttrib3f := glGetProcAddress('glVertexAttrib3f');
  glVertexAttrib3fv := glGetProcAddress('glVertexAttrib3fv');
  glVertexAttrib4f := glGetProcAddress('glVertexAttrib4f');
  glVertexAttrib4fv := glGetProcAddress('glVertexAttrib4fv');
  glVertexAttribPointer := glGetProcAddress('glVertexAttribPointer');

  glDiscardFramebuffer := glGetProcAddress('glDiscardFramebuffer');

  glRenderbufferStorageMultisample := glGetProcAddress('glRenderbufferStorageMultisample');
  glResolveMultisampleFramebuffer := glGetProcAddress('glResolveMultisampleFramebuffer');

  glAlphaFunc := glGetProcAddress('glAlphaFunc');
  glEnableClientState := glGetProcAddress('glEnableClientState');
  glDisableClientState := glGetProcAddress('glDisableClientState');
  glVertexPointer := glGetProcAddress('glVertexPointer');
  glTexCoordPointer := glGetProcAddress('glTexCoordPointer');
  glColorPointer := glGetProcAddress('glColorPointer');
  glColor4f := glGetProcAddress('glColor4f');

  glMatrixMode := glGetProcAddress('glMatrixMode');
  glLoadMatrixf := glGetProcAddress('glLoadMatrixf');
  
  glViewport := glGetProcAddress('glViewport');
End;

Procedure glLoadExtensions;
Begin
  LoadOpenGL(OpenGLLibName);
End;

Procedure FreeOpenGL;
Begin
  If OpenGLHandle<>0 Then
    FreeLibrary(OpenGLHandle);
End;

Function glExtensionSupported(Extension:TERRAString):Boolean;
Begin
  If (Extension='') Then
  Begin
    Result := False;
    Exit;
  End;

  If (ExtensionsList='') Then
  Begin
    ExtensionsList := PTERRAChar(glGetString(GL_EXTENSIONS));
  End;

  Result := Pos(Extension,ExtensionsList)>0;
End;

Initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
Finalization
  FreeOpenGL;
End.

