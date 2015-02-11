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
 * TERRA_DebugGL
 * Implements OpenGL debug wrapper
 ***********************************************************************************************************************
}

Unit TERRA_DebugGL;

{$I terra.inc}

Interface
{$IFDEF WINDOWS}
Uses Windows;
{$ENDIF}

{$I glconsts.inc}

Procedure glEnable(cap: Cardinal);
Procedure glDisable(cap: Cardinal);

Procedure glCullFace(mode: Cardinal);

Procedure glClear(mask: Cardinal);
Procedure glClearColor(red, green, blue, alpha: Single);
Procedure glClearDepth(depth: Double);
Procedure glClearStencil(s: Integer);

Procedure glDepthMask(flag: Boolean);
Procedure glColorMask(red, green, blue, alpha: Boolean);

Procedure glLineWidth(width: Single);

Procedure glDepthFunc(func: Cardinal); 

Procedure glBlendFunc(sfactor, dfactor: Cardinal);

Procedure glStencilFunc(func: Cardinal; ref: Integer; mask: Cardinal);
Procedure glStencilOp(fail, zfail, zpass: Cardinal);
Procedure glStencilMask(mask: Cardinal);


Procedure glScissor(x, y: Integer; width, height: Integer);
Procedure glViewport(x, y: Integer; width, height: Integer);

Procedure glGetIntegerv(pname: Cardinal; params: PInteger);
Function glGetString(name: Cardinal): PTERRAChar; 

procedure glVertexAttribPointer(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer);
Procedure glDrawArrays(mode: Cardinal; first: Integer; count: Integer);
Procedure glDrawElements(mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer);

procedure glGenBuffers(n: Integer; buffers: PCardinal);
procedure glDeleteBuffers(n: Integer; const buffers: PCardinal);
procedure glBindBuffer(target: Cardinal; buffer: Cardinal);
procedure glBufferData(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal);
procedure glBufferSubData(target: Cardinal; offset: Integer; size: Integer; const data: Pointer);

Procedure glBindTexture(target: Cardinal; texture: Cardinal);
Procedure glTexImage2D(target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer);
Procedure glGenTextures(n: Integer; textures: PCardinal);
Procedure glDeleteTextures(n: Integer; const textures: PCardinal);
procedure glActiveTexture(texture: Cardinal);
Procedure glTexParameterf(target: Cardinal; pname: Cardinal; param: Single);
Procedure glTexParameteri(target: Cardinal; pname: Cardinal; param: Integer);
Procedure glCopyTexImage2D(target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer);
Procedure glTexSubImage2D(target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer); 

procedure glGenerateMipmap(target:Cardinal);

Procedure glReadPixels(x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer);

procedure glGenFramebuffers(n: Integer; framebuffers: PCardinal);
procedure glBindFramebuffer(target: Cardinal; framebuffer: Cardinal);
procedure glFramebufferTexture2D(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer);
procedure glFramebufferRenderbuffer(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal);
function glCheckFramebufferStatus(target: Cardinal): Cardinal;
procedure glGenRenderbuffers(n: Integer; renderbuffers: PCardinal);
procedure glBindRenderbuffer(target: Cardinal; renderbuffer: Cardinal);
procedure glDeleteRenderbuffers(n: Integer; const renderbuffers: PCardinal);
procedure glRenderbufferStorage(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer);
procedure glDeleteFramebuffers(n: Integer; const framebuffers: PCardinal);
procedure glGetRenderbufferParameteriv(target: Cardinal; pname: Cardinal; params: PInteger);

Function glIsProgram(_program:Cardinal):Boolean;
function glGetUniformLocation(_program: Cardinal; const name: PTERRAChar): Integer;
procedure glDetachShader(_program: Cardinal; shader: Cardinal);
procedure glCompileShader(shader: Cardinal);
procedure glLinkProgram(_program: Cardinal);
function glCreateProgram(): Cardinal;
function glCreateShader(_type: Cardinal): Cardinal;
procedure glDeleteProgram(_program: Cardinal);
procedure glDeleteShader(shader: Cardinal);
procedure glShaderSource(shader: Cardinal; count: Integer; const _string: PTERRAChar; const length: PInteger);
procedure glGetShaderiv(shader: Cardinal; pname: Cardinal; params: PInteger);
procedure glGetShaderInfoLog(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar);
procedure glAttachShader(_program: Cardinal; shader: Cardinal);
procedure glGetProgramiv(_program: Cardinal; pname: Cardinal; params: PInteger);
procedure glGetProgramInfoLog(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar);
procedure glUseProgram(_program: Cardinal);
function glGetAttribLocation(_program: Cardinal; const name: PTERRAChar): Integer;

procedure glDisableVertexAttribArray(index: Cardinal);
procedure glEnableVertexAttribArray(index: Cardinal);

procedure glUniform1f(location: Integer; v0: Single);
procedure glUniform1i(location: Integer; v0: Integer);
procedure glUniform2fv(location: Integer; count: Integer; const value: PSingle);
procedure glUniform3fv(location: Integer; count: Integer; const value: PSingle);
procedure glUniform4fv(location: Integer; count: Integer; const value: PSingle);
procedure glUniformMatrix3fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle);
procedure glUniformMatrix4fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle);


{$IFDEF PC}
procedure glRenderbufferStorageMultisample(target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer);
procedure glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal);
procedure glDrawBuffers(n: Integer; const bufs: PCardinal);
Procedure glReadBuffer(mode: Cardinal);
Procedure glDrawBuffer(mode: Cardinal); 

Procedure glVertexPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Procedure glColorPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Procedure glTexCoordPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Procedure glEnableClientState(aarray: Cardinal);
Procedure glDisableClientState(aarray: Cardinal);

Procedure glMatrixMode(mode: Cardinal);
Procedure glLoadMatrixf(const m: PSingle);

Procedure glTexEnvi(target: Cardinal; pname: Cardinal; param: Integer);
Procedure glTexEnvfv(target: Cardinal; pname: Cardinal; const params: PSingle);

Procedure glGetTexLevelParameteriv(target: Cardinal; level: Integer; pname: Cardinal; params: PInteger);

Procedure glColor4f(red, green, blue, alpha: Single);
Procedure glColor4ub(red, green, blue, alpha: Byte);
Procedure glVertex3f(x, y, z: Single);
procedure glVertexAttrib4f(index: Cardinal; x: Single; y: Single; z: Single; w: Single);
procedure glVertexAttrib4ubv(index: Cardinal; const v: PByte);
procedure glVertexAttrib3f(index: Cardinal; x: Single; y: Single; z: Single);

Procedure glLineStipple(factor: Integer; pattern: Word);
Procedure glDepthRange(zNear, zFar: Double);
Procedure glPointSize(size: Single);
Procedure glBegin(mode: Cardinal);
Procedure glEnd;

Procedure glClipPlane(plane: Cardinal; const equation: PDouble); 

Procedure glGetTexImage(target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer);

Procedure glAlphaFunc(func: Cardinal; ref: Single);
{$ENDIF}

{$IFDEF WINDOWS}
function wglSwapIntervalEXT(interval: Integer): BOOL;
Function InitMultisample(hWnd: HWND; pfd: PIXELFORMATDESCRIPTOR;  h_dc: HDC):Cardinal;
{$ENDIF}

Procedure glLoadExtensions();
Function glExtensionSupported(Extension:TERRAString):Boolean;
Function glGetExtensionString():TERRAString;

Var
  _LastGLCall:TERRAString;
  _LastGLParams:TERRAString;

Implementation
Uses TERRA_Utils, TERRA_Log, TERRA_OS, TERRA_GL, TERRA_Shader, TERRA_Debug;

Procedure DebugOpenGL();
Var
  ErrorCode:Cardinal;
  S:TERRAString;
Begin
  {$IFDEF PC}
  S := GetCallstack();

  If (Pos('Texture.Update',S)>0) Then
    Exit;

  If (Pos('GraphicsManager.OnAppCreate',S)>0) Then
    Exit;

  If (Pos('Capture',S)>0) Then
    Exit;
  {$ENDIF}

  ErrorCode := TERRA_GL.glGetError();
  If ErrorCode = GL_NO_ERROR Then
  Begin
    _LastGLParams := '';
    Exit;
  End;

  IntToString(2);

  Case ErrorCode Of
  GL_INVALID_ENUM:      S := 'Invalid enum';
  GL_INVALID_VALUE:     S := 'Invalid value';
  GL_INVALID_OPERATION: S := 'Invalid operation';
//  GL_STACK_OVERFLOW:    S := 'Stack overflow';
//  GL_STACK_UNDERFLOW:   S := 'Stack underflow';
  GL_OUT_OF_MEMORY:     S := 'Out of memory';
  Else
    S := 'Unknown error 0x'+HexStr(ErrorCode);
  End;

  S := 'OpenGL Error ['+S+'] in '+ _LastGLCall;
  If _LastGLParams<>'' Then
    S := S + '('+_LastGLParams+')';
  {S := S +', Shader=';
  If ShaderManager.Instance.ActiveShader<>Nil Then
    S := S + ShaderManager.Instance.ActiveShader.Name
  Else
    S := S + '[NULL]';}

  S := S + CrLf + 'Callstack:'+Crlf+GetCallStack();

  Log(logError,'OpenGL', S);
  Halt;
End;

Procedure glLoadExtensions();
Begin
  TERRA_GL.glLoadExtensions();
End;

Procedure glLineWidth(width: Single);
Begin
  _LastGLCall := 'glLineWidth';
  TERRA_GL.glLineWidth(width);
DebugOpenGL(); End;

procedure glDisableVertexAttribArray(index: Cardinal);
Begin
  _LastGLCall := 'glDisableVertexAttribArray';
  TERRA_GL.glDisableVertexAttribArray(index);
DebugOpenGL(); End;

procedure glEnableVertexAttribArray(index: Cardinal);
Begin
  _LastGLCall := 'glEnableVertexAttribArray';
  TERRA_GL.glEnableVertexAttribArray(index);
DebugOpenGL(); End;

function glGetAttribLocation(_program: Cardinal; const name: PTERRAChar): Integer;
Begin
  _LastGLCall := 'glGetAttribLocation';
  Result := TERRA_GL.glGetAttribLocation(_program, name);
DebugOpenGL(); End;

procedure glUseProgram(_program: Cardinal);
Begin
  _LastGLCall := 'glUseProgram';
  TERRA_GL.glUseProgram(_program);
DebugOpenGL(); End;

procedure glUniformMatrix3fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle);
Begin
  _LastGLCall := 'glUniformMatrix3fv';
  TERRA_GL.glUniformMatrix3fv(location, count, transpose, value);
DebugOpenGL(); End;

procedure glUniformMatrix4fv(location: Integer; count: Integer; transpose: Boolean; const value: PSingle);
Begin
  _LastGLCall := 'glUniformMatrix4fv';
  TERRA_GL.glUniformMatrix4fv(location, count, transpose, value);
DebugOpenGL(); End;

procedure glUniform1f(location: Integer; v0: Single);
Begin
  _LastGLCall := 'glUniform1f';
  TERRA_GL.glUniform1f(location, v0);
DebugOpenGL(); End;

procedure glUniform1i(location: Integer; v0: Integer);
Begin
  _LastGLCall := 'glUniform1i';
  TERRA_GL.glUniform1i(location, v0);
DebugOpenGL(); End;

procedure glUniform2fv(location: Integer; count: Integer; const value: PSingle);
Begin
  _LastGLCall := 'glUniform2fv';
  TERRA_GL.glUniform2fv(location, count, value);
DebugOpenGL(); End;

procedure glUniform3fv(location: Integer; count: Integer; const value: PSingle);
Begin
  _LastGLCall := 'glUniform3fv';
  TERRA_GL.glUniform3fv(location, count, value);
DebugOpenGL(); End;

procedure glUniform4fv(location: Integer; count: Integer; const value: PSingle);
Begin
  _LastGLCall := 'glUniform4fv';
  TERRA_GL.glUniform4fv(location, count, value);
DebugOpenGL(); End;

procedure glGetProgramInfoLog(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar);
Begin
  _LastGLCall := 'glGetProgramInfoLog';
  TERRA_GL.glGetProgramInfoLog(_program, bufsize, length, infolog);
DebugOpenGL(); End;

procedure glGetProgramiv(_program: Cardinal; pname: Cardinal; params: PInteger);
Begin
  _LastGLCall := 'glGetProgramiv';
  TERRA_GL.glGetProgramiv(_program, pname, params);
DebugOpenGL(); End;

procedure glAttachShader(_program: Cardinal; shader: Cardinal);
Begin
  _LastGLCall := 'glAttachShader';
  TERRA_GL.glAttachShader(_program, shader);
DebugOpenGL(); End;

procedure glGetShaderInfoLog(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PTERRAChar);
Begin
  _LastGLCall := 'glGetShaderInfoLog';
  TERRA_GL.glGetShaderInfoLog(shader, bufsize, length, infolog);
DebugOpenGL(); End;

procedure glGetShaderiv(shader: Cardinal; pname: Cardinal; params: PInteger);
Begin
  _LastGLCall := 'glGetShaderiv';
  TERRA_GL.glGetShaderiv(shader, pname, params);
DebugOpenGL(); End;

procedure glShaderSource(shader: Cardinal; count: Integer; const _string: PTERRAChar; const length: PInteger);
Begin
  _LastGLCall := 'glShaderSource';
  TERRA_GL.glShaderSource(shader, count, _string, length);
DebugOpenGL(); End;

procedure glDetachShader(_program: Cardinal; shader: Cardinal);
Begin
  _LastGLCall := 'glDetachShader';
  TERRA_GL.glDetachShader(_program, shader);
DebugOpenGL(); End;

procedure glLinkProgram(_program: Cardinal);
Begin
  _LastGLCall := 'glLinkProgram';
  TERRA_GL.glLinkProgram(_program);
DebugOpenGL(); End;

procedure glCompileShader(shader: Cardinal);
Begin
  _LastGLCall := 'glCompileShader';
  TERRA_GL.glCompileShader(shader);
DebugOpenGL(); End;

function glCreateProgram(): Cardinal;
Begin
  _LastGLCall := 'glCreateProgram';
  Result := TERRA_GL.glCreateProgram();
DebugOpenGL(); End;

function glCreateShader(_type: Cardinal): Cardinal;
Begin
  _LastGLCall := 'glCreateShader';
  Result := TERRA_GL.glCreateShader(_type);
DebugOpenGL(); End;


procedure glDeleteProgram(_program: Cardinal);
Begin
  _LastGLCall := 'glDeleteProgram';
  TERRA_GL.glDeleteProgram(_program);
DebugOpenGL(); End;

procedure glDeleteShader(shader: Cardinal);
Begin
  _LastGLCall := 'glDeleteShader';
  TERRA_GL.glDeleteShader(shader);
DebugOpenGL(); End;


Function glIsProgram(_program:Cardinal):Boolean;
Begin
  _LastGLCall := 'glIsProgram';
  Result := TERRA_GL.glIsProgram(_program);
DebugOpenGL(); End;

function glGetUniformLocation(_program: Cardinal; const name: PTERRAChar): Integer;
Begin
  _LastGLCall := 'glGetUniformLocation';
  Result := TERRA_GL.glGetUniformLocation(_program, name);
DebugOpenGL(); End;

procedure glRenderbufferStorage(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer);
Begin
  _LastGLCall := 'glRenderbufferStorage';
  TERRA_GL.glRenderbufferStorage(target, internalformat, width, height);
DebugOpenGL(); End;

procedure glDeleteFramebuffers(n: Integer; const framebuffers: PCardinal);
Begin
  _LastGLCall := 'glDeleteFramebuffers';
  TERRA_GL.glDeleteFramebuffers(n, framebuffers);
DebugOpenGL(); End;

procedure glGetRenderbufferParameteriv(target: Cardinal; pname: Cardinal; params: PInteger);
Begin
  _LastGLCall := 'glGetRenderbufferParameteriv';
  TERRA_GL.glGetRenderbufferParameteriv(target, pname, params);
  DebugOpenGL();
End;


procedure glDeleteRenderbuffers(n: Integer; const renderbuffers: PCardinal);
Begin
  _LastGLCall := 'glDeleteRenderbuffers';
  TERRA_GL.glDeleteRenderbuffers(n, renderbuffers);
DebugOpenGL(); End;

procedure glBindRenderbuffer(target: Cardinal; renderbuffer: Cardinal);
Begin
  _LastGLCall := 'glBindRenderbuffer';
  TERRA_GL.glBindRenderbuffer(target, renderbuffer);
DebugOpenGL(); End;

procedure glGenRenderbuffers(n: Integer; renderbuffers: PCardinal);
Begin
  _LastGLCall := 'glGenRenderbuffers';
  TERRA_GL.glGenRenderbuffers(n, renderbuffers);
DebugOpenGL(); End;

function glCheckFramebufferStatus(target: Cardinal): Cardinal;
Begin
  _LastGLCall := 'glCheckFramebufferStatus';
  Result := TERRA_GL.glCheckFramebufferStatus(target);
DebugOpenGL(); End;

procedure glFramebufferRenderbuffer(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal);
Begin
  _LastGLCall := 'glFramebufferRenderbuffer';
  TERRA_GL.glFramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer);
DebugOpenGL(); End;

procedure glFramebufferTexture2D(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer);
Begin
  _LastGLCall := 'glFramebufferTexture2D';
  TERRA_GL.glFramebufferTexture2D(target, attachment, textarget, texture, level);
DebugOpenGL(); End;

procedure glBindFramebuffer(target: Cardinal; framebuffer: Cardinal);
Begin
  _LastGLCall := 'glBindFramebuffer';
  TERRA_GL.glBindFramebuffer(target, framebuffer);
DebugOpenGL(); End;

procedure glGenFramebuffers(n: Integer; framebuffers: PCardinal);
Begin
  _LastGLCall := 'glGenFramebuffers';
  TERRA_GL.glGenFramebuffers(n, framebuffers);
DebugOpenGL(); End;

Function glGetString(name: Cardinal): PTERRAChar;
Begin
  _LastGLCall := 'glGetString';
  Result := TERRA_GL.glGetString(name);
DebugOpenGL(); End;

Function glGetExtensionString():TERRAString;
Begin
  _LastGLCall := 'glGetExtensionString';
  Result := TERRA_GL.glGetExtensionString();
DebugOpenGL(); End;

Function glExtensionSupported(Extension:TERRAString):Boolean;
Begin
  _LastGLCall := 'glExtensionSupported';
  Result := TERRA_GL.glExtensionSupported(extension);
DebugOpenGL(); End;

Procedure glGetIntegerv(pname: Cardinal; params: PInteger);
Begin
  _LastGLCall := 'glGetIntegerv';
  TERRA_GL.glGetIntegerv(pname, params);
DebugOpenGL(); End;

Procedure glViewport(x, y: Integer; width, height: Integer);
Begin
  _LastGLCall := 'glViewport';
  TERRA_GL.glViewport(x, y, width, height);
DebugOpenGL(); End;

Procedure glScissor(x, y: Integer; width, height: Integer);
Begin
  _LastGLCall := 'glScissor';
  TERRA_GL.glScissor(x, y, width, height);
DebugOpenGL(); End;

Procedure glColorMask(red, green, blue, alpha: Boolean);
Begin
  _LastGLCall := 'glColorMask';
  TERRA_GL.glColorMask(red, green, blue, alpha);
DebugOpenGL(); End;

Procedure glBlendFunc(sfactor, dfactor: Cardinal);
Begin
  _LastGLCall := 'glBlendFunc';
  TERRA_GL.glBlendFunc(sfactor, dfactor);
DebugOpenGL(); End;

Procedure glDepthMask(flag: Boolean);
Begin
  _LastGLCall := 'glDepthMask';
  TERRA_GL.glDepthMask(flag);
DebugOpenGL(); End;

Procedure glCullFace(mode: Cardinal);
Begin
  _LastGLCall := 'glCullFace';
  TERRA_GL.glCullFace(mode);
DebugOpenGL(); End;

Procedure glClearColor(red, green, blue, alpha: Single);
Begin
  _LastGLCall := 'glClearColor';
  TERRA_GL.glClearColor(red, green, blue, alpha);
DebugOpenGL(); End;

Procedure glClearDepth(depth: Double);
Begin
  _LastGLCall := 'glClearDepth';
  TERRA_GL.glClearDepth(depth);
DebugOpenGL(); End;

Procedure glClearStencil(s: Integer);
Begin
  _LastGLCall := 'glClearStencil';
  TERRA_GL.glClearStencil(s);
DebugOpenGL(); End;

procedure glGenerateMipmap(target:Cardinal);
Begin
  _LastGLCall := 'glGenerateMipmap';
  TERRA_GL.glGenerateMipmap(Target);
DebugOpenGL(); End;

Procedure glReadPixels(x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer);
Begin
  _LastGLCall := 'glReadPixels';
  TERRA_GL.glReadPixels(x, y, width, height, format, atype, pixels);
DebugOpenGL(); End;

Procedure glClear(mask: Cardinal);
Begin
  _LastGLCall := 'glClear';
  TERRA_GL.glClear(mask);
DebugOpenGL(); End;

Procedure glTexParameterf(target: Cardinal; pname: Cardinal; param: Single);
Begin
  _LastGLCall := 'glTexParameterf';
  TERRA_GL.glTexParameterf(target, pname, param);
DebugOpenGL(); End;

Procedure glTexParameteri(target: Cardinal; pname: Cardinal; param: Integer);
Begin
  _LastGLCall := 'glTexParameteri';
  TERRA_GL.glTexParameteri(target, pname, param);
DebugOpenGL(); End;

Procedure glCopyTexImage2D(target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer);
Begin
  _LastGLCall := 'glCopyTexImage2D';
  TERRA_GL.glCopyTexImage2D(target, level, internalformat, x, y, width, height, border);
DebugOpenGL(); End;

Procedure glTexSubImage2D(target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer);
Begin
  _LastGLCall := 'glTexSubImage2D';
  TERRA_GL.glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, atype, pixels);
DebugOpenGL(); End;

procedure glActiveTexture(texture: Cardinal);
Begin
  _LastGLCall := 'glActiveTexture';
  TERRA_GL.glActiveTexture(texture);
DebugOpenGL(); End;

Procedure glTexImage2D(target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer);
Begin
  _LastGLCall := 'glTexImage2D';
  TERRA_GL.glTexImage2D(target, level, internalformat, width, height, border, format, atype, pixels);
DebugOpenGL(); End;

Procedure glGenTextures(n: Integer; textures: PCardinal);
Begin
  _LastGLCall := 'glGenTextures';
  TERRA_GL.glGenTextures(n, textures);
DebugOpenGL(); End;

Procedure glDeleteTextures(n: Integer; const textures: PCardinal);
Begin
  _LastGLCall := 'glDeleteTextures';
  TERRA_GL.glDeleteTextures(n, textures);
DebugOpenGL(); End;

Procedure glBindTexture(target: Cardinal; texture: Cardinal);
Begin
  _LastGLCall := 'glBindTexture';
  TERRA_GL.glBindTexture(target, texture);
DebugOpenGL(); End;

procedure glDeleteBuffers(n: Integer; const buffers: PCardinal);
Begin
  _LastGLCall := 'glDeleteBuffers';
  TERRA_GL.glDeleteBuffers(n, buffers);
DebugOpenGL(); End;

procedure glBufferSubData(target: Cardinal; offset: Integer; size: Integer; const data: Pointer);
Begin
  _LastGLCall := 'glBufferSubData';
  TERRA_GL.glBufferSubData(target, offset, size, data);
DebugOpenGL(); End;

procedure glBufferData(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal);
Begin
  _LastGLCall := 'glBufferData';
  TERRA_GL.glBufferData(target, size, data, usage);
DebugOpenGL(); End;

procedure glBindBuffer(target: Cardinal; buffer: Cardinal);
Begin
  _LastGLCall := 'glBindBuffer';
  TERRA_GL.glBindBuffer(target, buffer);
DebugOpenGL(); End;

procedure glGenBuffers(n: Integer; buffers: PCardinal);
Begin
  _LastGLCall := 'glGenBuffers';
  TERRA_GL.glGenBuffers(n, buffers);
DebugOpenGL(); End;

Procedure glStencilOp(fail, zfail, zpass: Cardinal);
Begin
  _LastGLCall := 'glStencilOp';
  TERRA_GL.glStencilOp(fail, zfail, zpass);
DebugOpenGL(); End;

Procedure glStencilMask(mask: Cardinal);
Begin
  _LastGLCall := 'glStencilMask';
  TERRA_GL.glStencilMask(mask);
DebugOpenGL(); End;

Procedure glStencilFunc(func: Cardinal; ref: Integer; mask: Cardinal);
Begin
  _LastGLCall := 'glStencilFunc';
  TERRA_GL.glStencilFunc(func, ref, mask);
DebugOpenGL(); End;

Procedure glEnable(cap: Cardinal);
Begin
  _LastGLCall := 'glEnable';
  _LastGLParams := HexStr(cap);
  TERRA_GL.glEnable(cap);
DebugOpenGL(); End;

Procedure glDisable(cap:Cardinal);
Begin
  If (Cap=$DE1) Then
    IntToString(2);

  _LastGLCall := 'glDisable';
  _LastGLParams := HexStr(cap);
  TERRA_GL.glDisable(cap);
DebugOpenGL(); End;


procedure glVertexAttribPointer(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer);
Begin
  _LastGLCall := 'glVertexAttribPointer';
  TERRA_GL.glVertexAttribPointer(index, size, _type, normalized, stride, pointer);
DebugOpenGL(); End;

Procedure glDrawElements(mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer);
Begin
  _LastGLCall := 'glDrawElements';
  TERRA_GL.glDrawElements(mode, count, atype, indices);
DebugOpenGL(); End;

Procedure glDrawArrays(mode: Cardinal; first: Integer; count: Integer);
Begin
  _LastGLCall := 'glDrawArrays';
  TERRA_GL.glDrawArrays(mode, first, count);
DebugOpenGL(); End;

Procedure glDepthFunc(func: Cardinal); 
Begin
  _LastGLCall := 'glDepthFunc';
  TERRA_GL.glDepthFunc(func);
  DebugOpenGL();
End;


{$IFDEF PC}
Procedure glMatrixMode(mode: Cardinal);
Begin
  _LastGLCall := 'glMatrixMode';
  TERRA_GL.glMatrixMode(mode);
DebugOpenGL(); End;

Procedure glLoadMatrixf(const m: PSingle);
Begin
  _LastGLCall := 'glLoadMatrixf';
  TERRA_GL.glLoadMatrixf(m);
DebugOpenGL(); End;

Procedure glDrawBuffer(mode: Cardinal);
Begin
  _LastGLCall := 'glDrawBuffer';
  TERRA_GL.glDrawBuffer(mode);
DebugOpenGL(); End;

Procedure glReadBuffer(mode: Cardinal);
Begin
  _LastGLCall := 'glReadBuffer';
  TERRA_GL.glReadBuffer(mode);
DebugOpenGL(); End;

procedure glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal);
Begin
  _LastGLCall := 'glBlitFramebuffer';
  TERRA_GL.glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
DebugOpenGL(); End;

procedure glDrawBuffers(n: Integer; const bufs: PCardinal);
Begin
  _LastGLCall := 'glDrawBuffers';
  TERRA_GL.glDrawBuffers(n, bufs);
DebugOpenGL(); End;


Procedure glBegin(mode: Cardinal);
Begin
  _LastGLCall := 'glBegin';
  TERRA_GL.glBegin(mode);
DebugOpenGL(); End;

Procedure glVertex3f(x, y, z: Single);
Begin
  _LastGLCall := 'glVertex3f';
  TERRA_GL.glVertex3f(x, y, z);
DebugOpenGL(); End;

procedure glVertexAttrib3f(index: Cardinal; x: Single; y: Single; z: Single);
Begin
  _LastGLCall := 'glVertexAttrib3f';
  TERRA_GL.glVertexAttrib3f(index, x, y, z);
DebugOpenGL(); End;

procedure glVertexAttrib4f(index: Cardinal; x: Single; y: Single; z: Single; w: Single);
Begin
  _LastGLCall := 'glVertexAttrib4f';
  TERRA_GL.glVertexAttrib4f(index, x, y, z, w);
DebugOpenGL(); End;

procedure glVertexAttrib4ubv(index: Cardinal; const v: PByte);
Begin
  _LastGLCall := 'glVertexAttrib4ubv';
  TERRA_GL.glVertexAttrib4ubv(index, v);
DebugOpenGL(); End;

Procedure glEnd(); 
Begin
  _LastGLCall := 'glEnd';
  TERRA_GL.glEnd();
DebugOpenGL(); End;

Procedure glLineStipple(factor: Integer; pattern: Word);
Begin
  _LastGLCall := 'glLineStipple';
  TERRA_GL.glLineStipple(factor, pattern);
DebugOpenGL(); End;

Procedure glDepthRange(zNear, zFar: Double);
Begin
  _LastGLCall := 'glDepthRange';
  TERRA_GL.glDepthRange(zNear, zFar);
DebugOpenGL(); End;

Procedure glPointSize(size: Single);
Begin
  _LastGLCall := 'glPointSize';
  TERRA_GL.glPointSize(size);
DebugOpenGL(); End;

Procedure glColor4ub(red, green, blue, alpha: Byte);
Begin
  _LastGLCall := 'glColor4ub';
  TERRA_GL.glColor4ub(red, green, blue, alpha);
DebugOpenGL(); End;

Procedure glColor4f(red, green, blue, alpha: Single);
Begin
  _LastGLCall := 'glColor4f';
  TERRA_GL.glColor4f(red, green, blue, alpha);
DebugOpenGL(); End;

Procedure glAlphaFunc(func: Cardinal; ref: Single);
Begin
  _LastGLCall := 'glAlphaFunc';
  TERRA_GL.glAlphaFunc(func, ref);
DebugOpenGL(); End;


Procedure glGetTexImage(target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer);
Begin
  _LastGLCall := 'glGetTexImage';
  TERRA_GL.glGetTexImage(Target, level, format, atype, pixels);
DebugOpenGL(); End;

Procedure glTexCoordPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Begin
  _LastGLCall := 'glTexCoordPointer';
  TERRA_GL.glTexCoordPointer(size, atype, stride, pointer);
DebugOpenGL(); End;

Procedure glColorPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Begin
  _LastGLCall := 'glColorPointer';
  TERRA_GL.glColorPointer(size, atype, stride, pointer);
DebugOpenGL(); End;

Procedure glVertexPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer);
Begin
  _LastGLCall := 'glVertexPointer';
  TERRA_GL.glVertexPointer(size, atype, stride, pointer);
DebugOpenGL(); End;

procedure glRenderbufferStorageMultisample(target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer);
Begin
  _LastGLCall := 'glRenderbufferStorageMultisample';
  TERRA_GL.glRenderbufferStorageMultisample(target, samples, internalformat, width, height);
DebugOpenGL(); End;

Procedure glDisableClientState(aarray: Cardinal);
Begin
  _LastGLCall := 'glDisableClientState';
  TERRA_GL.glDisableClientState(aarray);
DebugOpenGL(); End;

Procedure glEnableClientState(aarray: Cardinal);
Begin
  _LastGLCall := 'glEnableClientState';
  TERRA_GL.glEnableClientState(aarray);
DebugOpenGL(); End;

Procedure glClipPlane(plane: Cardinal; const equation: PDouble);
Begin
  _LastGLCall := 'glClipPlane';
  TERRA_GL.glClipPlane(plane, equation);
DebugOpenGL(); End;

Procedure glTexEnvi(target: Cardinal; pname: Cardinal; param: Integer);
Begin
  _LastGLCall := 'glTexEnvi';
  TERRA_GL.glTexEnvi(target,pname, param);
DebugOpenGL(); End;

Procedure glTexEnvfv(target: Cardinal; pname: Cardinal; const params: PSingle);
Begin
  _LastGLCall := 'glTexEnvfv';
  TERRA_GL.glTexEnvfv(target,pname, params);
DebugOpenGL(); End;

Procedure glGetTexLevelParameteriv(target: Cardinal; level: Integer; pname: Cardinal; params: PInteger);
Begin
  _LastGLCall := 'glGetTexLevelParameteriv';
  TERRA_GL.glGetTexLevelParameteriv(target, level, pname, params);
DebugOpenGL(); End;

{$ENDIF}

{$IFDEF WINDOWS}
function wglSwapIntervalEXT(interval: Integer): BOOL;
Begin
  _LastGLCall := 'wglSwapIntervalEXT';
  Result := TERRA_GL.wglSwapIntervalEXT(interval);
DebugOpenGL(); End;

Function InitMultisample(hWnd: HWND; pfd: PIXELFORMATDESCRIPTOR;  h_dc: HDC):Cardinal;
Begin
  Result := TERRA_GL.InitMultisample(hWnd, pfd, h_dc);
End;

{$ENDIF}


End.








