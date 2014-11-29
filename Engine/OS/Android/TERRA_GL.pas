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

Interface
Uses TERRA_Utils, Math;

Const
  libname = 'libGLESv2.so';

  {$I glconsts.inc}

// GL core functions.
  Procedure glActiveTexture(texture:Integer); cdecl; external libname;
  Procedure glAttachShader(programname, shader:Cardinal); cdecl; external libname;
  Procedure glBindAttribLocation(_program:Integer; index:Integer; name:PAnsiChar); cdecl; external libname;
  Procedure glBindBuffer(target, buffer:Integer); cdecl; external libname;
  Procedure glBindFramebuffer(target, framebuffer:Cardinal); cdecl; external libname;
  Procedure glBindRenderbuffer(target, renderbuffer:Cardinal); cdecl; external libname;
  Procedure glBindTexture(target, texture:Cardinal); cdecl; external libname;
  Procedure glBlendColor(red, green, blue, alpha:Single); cdecl; external libname;
  Procedure glBlendEquation( mode:Cardinal); cdecl; external libname;
  Procedure glBlendEquationSeparate(modeRGB, modeAlpha:Cardinal); cdecl; external libname;
  Procedure glBlendFunc(sfactor, dfactor:Cardinal); cdecl; external libname;
  Procedure glBlendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha:Cardinal); cdecl; external libname;
  Procedure glBufferData (target, size:Cardinal; data:Pointer; usage:Cardinal); cdecl; external libname;
  Procedure glBufferSubData(target, offset, size:Cardinal; data:Pointer); cdecl; external libname;
  Function glCheckFramebufferStatus(target:Integer):Integer; cdecl; external libname;
  Procedure glClear(mask:Cardinal); cdecl; external libname;
  Procedure glClearColor(red, green, blue, alpha:Single); cdecl; external libname;
  Procedure glClearDepthf(depth:Single); cdecl; external libname;
  Procedure glClearStencil(s:Cardinal); cdecl; external libname;
  Procedure glColorMask(red, green, blue, alpha:Boolean); cdecl; external libname;
  Procedure glCompileShader(shader:Cardinal); cdecl; external libname;
  Procedure glCompressedTexImage2D(target, level, internalformat, width, height, border, imageSize:Cardinal; data:Pointer); cdecl; external libname;
  Procedure glCompressedTexSubImage2D(target, level, xoffset, yoffset, width, height, format, imageSize:Cardinal; data:Pointer); cdecl; external libname;
  Procedure glCopyTexImage2D(target, level, internalformat, x, y, width, height, border:Cardinal); cdecl; external libname;
  Procedure glCopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width,height:Cardinal); cdecl; external libname;
  Function glCreateProgram():Integer; cdecl; external libname;
  Function glCreateShader(shadertype:Integer):Integer; cdecl; external libname;
  Procedure glCullFace(mode:Cardinal); cdecl; external libname;
  Procedure glDeleteBuffers(n:Cardinal;  buffers:PCardinal); cdecl; external libname;
  Procedure glDeleteFramebuffers(n:Cardinal; framebuffers:PCardinal); cdecl; external libname;
  Procedure glDeleteProgram(_program:Cardinal); cdecl; external libname;
  Procedure glDeleteRenderbuffers(n:Cardinal; renderbuffers:PCardinal); cdecl; external libname;
  Procedure glDeleteShader(shader:Cardinal); cdecl; external libname;
  Procedure glDeleteTextures(n:Cardinal; textures:PCardinal); cdecl; external libname;
  Procedure glDepthFunc(func:Cardinal); cdecl; external libname;
  Procedure glDepthMask(flag:Boolean); cdecl; external libname;
  Procedure glDepthRange(zNear, zFar:Single); cdecl; external libname name 'glDepthRangef';
  Procedure glDetachShader(_program, shader:Cardinal); cdecl; external libname;
  Procedure glDisable(cap:Cardinal); cdecl; external libname;
  Procedure glDisableVertexAttribArray(index:Cardinal); cdecl; external libname;
  Procedure glDrawArrays(mode, first, count:Cardinal); cdecl; external libname;
  Procedure glDrawElements(mode, count, _type:Cardinal; indices:Pointer); cdecl; external libname;
  Procedure glEnable(cap:Cardinal); cdecl; external libname;
  Procedure glEnableVertexAttribArray(index:Cardinal); cdecl; external libname;
  Procedure glFinish(); cdecl; external libname;
  Procedure glFlush(); cdecl; external libname;
  Procedure glFramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer:Cardinal); cdecl; external libname;
  Procedure glFramebufferTexture2D(target, attachment, textarget, texture, level:Cardinal); cdecl; external libname;
  Procedure glFrontFace(mode:Cardinal); cdecl; external libname;
  Procedure glGenBuffers(n:Cardinal; buffers:PCardinal); cdecl; external libname;
  Procedure glGenerateMipmap(target:Cardinal); cdecl; external libname;
  Procedure glGenFramebuffers(n:Cardinal; framebuffers:PCardinal); cdecl; external libname;
  Procedure glGenRenderbuffers(n:Cardinal; renderbuffers:PCardinal); cdecl; external libname;
  Procedure glGenTextures(n:Cardinal; textures:PCardinal); cdecl; external libname;
  Procedure glGetActiveAttrib(_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl; external libname;
  Procedure glGetActiveUniform(_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl; external libname;
  Procedure glGetAttachedShaders(_program, maxcount:Cardinal; Var count:Integer; shaders:PCardinal); cdecl; external libname;
  Function glGetAttribLocation(_program:Cardinal; name:PAnsiChar):Integer; cdecl; external libname;
  Procedure glGetBooleanv(pname:Cardinal; params:PBoolean); cdecl; external libname;
  Procedure glGetBufferParameteriv(target, pname:Cardinal; params:PInteger); cdecl; external libname;
  Function glGetError():Integer; cdecl; external libname;
  Procedure glGetFloatv(pname:Cardinal; params:PSingle); cdecl; external libname;
  Procedure glGetFramebufferAttachmentParameteriv(target, attachment, pname:Cardinal; params:PInteger); cdecl; external libname;
  Procedure glGetIntegerv(pname:Cardinal; params:PInteger); cdecl; external libname;
  Procedure glGetProgramiv(_program,pname:Cardinal; params:PInteger); cdecl; external libname;
  procedure glGetProgramInfoLog(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl; external libname;
  Procedure glGetRenderbufferParameteriv(target, pname:Cardinal; params:PInteger); cdecl; external libname;
  Procedure glGetShaderiv(shader, pname:Cardinal; params:PInteger); cdecl; external libname;
  procedure glGetShaderInfoLog(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl; external libname;
  Procedure glGetShaderPrecisionFormat(shadertype, precisiontype:Cardinal; Var range, precision:Integer); cdecl; external libname;
  Procedure glGetShaderSource(shader, bufsize:Cardinal; Var length:Integer; source:PAnsiChar); cdecl; external libname;
  Function glGetString(name:Cardinal):PAnsiChar; cdecl; external libname;
  Procedure glGetTexParameterfv(target, pname:Cardinal; params:PSingle); cdecl; external libname;
  Procedure glGetTexParameteriv(target, pname:Cardinal; params:PInteger); cdecl; external libname;
  Procedure glGetUniformfv(_program, location:Cardinal;  params:PSingle); cdecl; external libname;
  Procedure glGetUniformiv(_program, location:Cardinal; params:PInteger); cdecl; external libname;
  Function glGetUniformLocation(_program:Cardinal; name:PAnsiChar):Integer; cdecl; external libname;
  Procedure glGetVertexAttribfv(index, pname:Cardinal; params:PSingle); cdecl; external libname;
  Procedure glGetVertexAttribiv(index, pname:Cardinal; params:PInteger); cdecl; external libname; 
  Procedure glGetVertexAttribPointerv(index, pname:Cardinal; pointer:PPointer); cdecl; external libname;
  Procedure glHint(target, mode:Cardinal); cdecl; external libname;
  Function glIsBuffer(buffer:Cardinal):Boolean; cdecl; external libname;
  Function glIsEnabled(cap:Cardinal):Boolean; cdecl; external libname;
  Function glIsFramebuffer(framebuffer:Cardinal):Boolean; cdecl; external libname;
  Function glIsProgram(_program:Cardinal):Boolean; cdecl; external libname;
  Function glIsRenderbuffer(renderbuffer:Cardinal):Boolean; cdecl; external libname;
  Function glIsShader(shader:Cardinal):Boolean; cdecl; external libname;
  Function glIsTexture(texture:Cardinal):Boolean; cdecl; external libname;
  Procedure glLineWidth(width:Single); cdecl; external libname;
  Procedure glLinkProgram(_program:Cardinal); cdecl; external libname;
  Procedure glPixelStorei(pname:Cardinal; param:Integer); cdecl; external libname;
  Procedure glPolygonOffset(factor, units:Single); cdecl; external libname;
  Procedure glReadPixels(x, y, width, height, format, _type:Cardinal; pixels:Pointer); cdecl; external libname;
  Procedure glReleaseShaderCompiler(); cdecl; external libname;
  Procedure glRenderbufferStorage(target, internalformat, width, height:Cardinal); cdecl; external libname;
  Procedure glSampleCoverage(value:Single; invert:Boolean); cdecl; external libname;
  Procedure glScissor(x, y, width, height:Cardinal); cdecl; external libname;
  Procedure glShaderBinary(n:Cardinal; shaders:PCardinal; binaryformat:Cardinal; binary:Pointer; length:Cardinal); cdecl; external libname;
  Procedure glShaderSource(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); cdecl; external libname;
  Procedure glStencilFunc(func, ref, mask:Cardinal); cdecl; external libname;
  Procedure glStencilFuncSeparate(face, func, ref, mask:Cardinal); cdecl; external libname;
  Procedure glStencilMask(mask:Cardinal); cdecl; external libname;
  Procedure glStencilMaskSeparate(face, mask:Cardinal); cdecl; external libname;
  Procedure glStencilOp(fail, zfail, zpass:Cardinal); cdecl; external libname;
  Procedure glStencilOpSeparate(face, fail, zfail, zpass:Cardinal); cdecl; external libname;
  Procedure glTexImage2D(target, level, internalformat, width, height, border, format, _type:Cardinal; pixels:Pointer); cdecl; external libname;
  Procedure glTexParameterf(target, pname:Cardinal; param:Single); cdecl; external libname;
  Procedure glTexParameterfv(target, pname:Cardinal; params:PSingle); cdecl; external libname;
  Procedure glTexParameteri(target, pname:Cardinal; param:Integer); cdecl; external libname;
  Procedure glTexParameteriv(target, pname:Cardinal; params:PInteger); cdecl; external libname;
  Procedure glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, _type:Cardinal; pixels:Pointer); cdecl; external libname;

  procedure glUniform1f(location: Integer; v0: Single); cdecl; external libname;
  procedure glUniform2f(location: Integer; v0: Single; v1: Single); cdecl; external libname;
  procedure glUniform3f(location: Integer; v0: Single; v1: Single; v2: Single); cdecl; external libname;
  procedure glUniform4f(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); cdecl; external libname;
  procedure glUniform1i(location: Integer; v0: Integer); cdecl; external libname;
  procedure glUniform2i(location: Integer; v0: Integer; v1: Integer); cdecl; external libname;
  procedure glUniform3i(location: Integer; v0: Integer; v1: Integer; v2: Integer); cdecl; external libname;
  procedure glUniform4i(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); cdecl; external libname;
  procedure glUniform1fv(location: Integer; count: Integer; const value: PSingle); cdecl; external libname;
  procedure glUniform2fv(location: Integer; count: Integer; const value: PSingle); cdecl; external libname;
  procedure glUniform3fv(location: Integer; count: Integer; const value: PSingle); cdecl; external libname;
  procedure glUniform4fv(location: Integer; count: Integer; const value: PSingle); cdecl; external libname;
  procedure glUniform1iv(location: Integer; count: Integer; const value: PInteger); cdecl; external libname;
  procedure glUniform2iv(location: Integer; count: Integer; const value: PInteger); cdecl; external libname;
  procedure glUniform3iv(location: Integer; count: Integer; const value: PInteger); cdecl; external libname;
  procedure glUniform4iv(location: Integer; count: Integer; const value: PInteger); cdecl; external libname;
  procedure glUniformMatrix2fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; external libname;
  procedure glUniformMatrix3fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; external libname;
  procedure glUniformMatrix4fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; external libname;

  Procedure glUseProgram(_program:Cardinal); cdecl; external libname;
  Procedure glValidateProgram(_program:Cardinal); cdecl; external libname;
  Procedure glVertexAttrib1f(indx:Cardinal; x:Single); cdecl; external libname;
  Procedure glVertexAttrib1fv(indx:Cardinal; values:PSingle); cdecl; external libname;
  Procedure glVertexAttrib2f(indx:Cardinal; x, y:Single); cdecl; external libname;
  Procedure glVertexAttrib2fv(indx:Cardinal; values:PSingle); cdecl; external libname;
  Procedure glVertexAttrib3f(indx:Cardinal; x, y, z:Single); cdecl; external libname;
  Procedure glVertexAttrib3fv(indx:Cardinal; values:PSingle); cdecl; external libname;
  Procedure glVertexAttrib4f(indx:Cardinal; x, y, z, w:Single); cdecl; external libname;
  Procedure glVertexAttrib4fv(indx:Cardinal; values:PSingle); cdecl; external libname;
  procedure glVertexAttribPointer(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); cdecl; external libname;
  Procedure glViewport(x, y, width, height:Cardinal); cdecl; external libname;

  //Procedure glDiscardFramebufferEXT(target, count:Integer; attachments:PInteger);

Function glExtensionSupported(Extension:AnsiString):Boolean;
Function glGetExtensionString():AnsiString;


Procedure glClearDepth(depth:Single);

Implementation
Uses TERRA_Error, TERRA_Application;

Var
  ExtensionsList:AnsiString='';


Procedure glClearDepth(depth:Single);
Begin
  glClearDepthf(depth);
End;

Function glGetExtensionString():AnsiString;
Begin
  Result := ExtensionsList;
End;
  
Function glExtensionSupported(Extension:AnsiString):Boolean;
Begin
  If (Extension='') Then
  Begin
    Result := False;
    Exit;
  End;

  If (ExtensionsList='') Then
  Begin
    ExtensionsList := PAnsiChar(glGetString(GL_EXTENSIONS));
  End;

  Result := Pos(Extension,ExtensionsList)>0;
End;

Initialization
{  If (Application.IsOUYA()) Then
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);}
  //SetExceptionMask([exInvalidOp, exOverflow, exUnderflow, exPrecision]);
Finalization
End.

