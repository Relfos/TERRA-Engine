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
Uses TERRA_Log, TERRA_Matrix4x4, TERRA_Math,Windows;

{$IFDEF WINDOWS}{$UNDEF MOBILE}{$DEFINE PC}{$UNDEF ANDROID}{$ENDIF}

Const
  OpenGLLibName='opengl32.dll';
  Glu32 = 'glu32.dll';

{$I glconsts.inc}

Procedure glAccum(op:Cardinal; value: Single); stdcall; External OpenGLLibName;
Procedure glAlphaFunc(func: Cardinal; ref: Single); stdcall; External OpenGLLibName;
Procedure glArrayElement(i: Integer); stdcall; External OpenGLLibName;
Procedure glBegin(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glBindTexture(target: Cardinal; texture: Cardinal); stdcall; External OpenGLLibName;
Procedure glBlendFunc(sfactor, dfactor: Cardinal); stdcall; External OpenGLLibName;
Procedure glCallList(list: Cardinal); stdcall; External OpenGLLibName;
Procedure glCallLists(n: Integer; atype: Cardinal; const lists: Pointer); stdcall; External OpenGLLibName;
Procedure glClear(mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glClearAccum(red, green, blue, alpha: Single); stdcall; External OpenGLLibName;
Procedure glClearColor(red, green, blue, alpha: Single); stdcall; External OpenGLLibName;
Procedure glClearDepth(depth: Double); stdcall; External OpenGLLibName;
Procedure glClearIndex(c: Single); stdcall; External OpenGLLibName;
Procedure glClearStencil(s: Integer); stdcall; External OpenGLLibName;
Procedure glClipPlane(plane: Cardinal; const equation: PDouble); stdcall; External OpenGLLibName;
Procedure glColor3b(red, green, blue: Shortint); stdcall; External OpenGLLibName;
Procedure glColor3bv(const v: PShortint); stdcall; External OpenGLLibName;
Procedure glColor3d(red, green, blue: Double); stdcall; External OpenGLLibName;
Procedure glColor3dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glColor3f(red, green, blue: Single); stdcall; External OpenGLLibName;
Procedure glColor3fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glColor3i(red, green, blue: Integer); stdcall; External OpenGLLibName;
Procedure glColor3iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glColor3s(red, green, blue: SmallInt); stdcall; External OpenGLLibName;
Procedure glColor3sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glColor3ub(red, green, blue: Byte); stdcall; External OpenGLLibName;
Procedure glColor3ubv(const v: PByte); stdcall; External OpenGLLibName;
Procedure glColor3ui(red, green, blue: Cardinal); stdcall; External OpenGLLibName;
Procedure glColor3uiv(const v: PCardinal); stdcall; External OpenGLLibName;
Procedure glColor3us(red, green, blue: Word); stdcall; External OpenGLLibName;
Procedure glColor3usv(const v: PWord); stdcall; External OpenGLLibName;
Procedure glColor4b(red, green, blue, alpha: Shortint); stdcall; External OpenGLLibName;
Procedure glColor4bv(const v: PShortint); stdcall; External OpenGLLibName;
Procedure glColor4d(red, green, blue, alpha: Double); stdcall; External OpenGLLibName;
Procedure glColor4dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glColor4f(red, green, blue, alpha: Single); stdcall; External OpenGLLibName;
Procedure glColor4fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glColor4i(red, green, blue, alpha: Integer); stdcall; External OpenGLLibName;
Procedure glColor4iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glColor4s(red, green, blue, alpha: SmallInt); stdcall; External OpenGLLibName;
Procedure glColor4sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glColor4ub(red, green, blue, alpha: Byte); stdcall; External OpenGLLibName;
Procedure glColor4ubv(const v: PByte); stdcall; External OpenGLLibName;
Procedure glColor4ui(red, green, blue, alpha: Cardinal); stdcall; External OpenGLLibName;
Procedure glColor4uiv(const v: PCardinal); stdcall; External OpenGLLibName;
Procedure glColor4us(red, green, blue, alpha: Word); stdcall; External OpenGLLibName;
Procedure glColor4usv(const v: PWord); stdcall; External OpenGLLibName;
Procedure glColorMask(red, green, blue, alpha: Boolean); stdcall; External OpenGLLibName;
Procedure glColorMaterial(face, mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glColorPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); stdcall; External OpenGLLibName;
Procedure glCopyPixels(x, y: Integer; width, height: Integer; atype: Cardinal); stdcall; External OpenGLLibName;
Procedure glCopyTexImage1D (target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width: Integer; border: Integer); stdcall; External OpenGLLibName;
Procedure glCopyTexImage2D(target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer); stdcall; External OpenGLLibName;
Procedure glCopyTexSubImage1D(target: Cardinal; level, xoffset, x, y: Integer; width: Integer); stdcall; External OpenGLLibName;
Procedure glCopyTexSubImage2D(target: Cardinal; level, xoffset, yoffset, x, y: Integer; width, height: Integer); stdcall; External OpenGLLibName;
Procedure glCullFace(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glDeleteLists(list: Cardinal; range: Integer); stdcall; External OpenGLLibName;
Procedure glDeleteTextures(n: Integer; const textures: PCardinal); stdcall; External OpenGLLibName;
Procedure glDepthFunc(func: Cardinal); stdcall; External OpenGLLibName;
Procedure glDepthMask(flag: Boolean); stdcall; External OpenGLLibName;
Procedure glDepthRange(zNear, zFar: Double); stdcall; External OpenGLLibName;
Procedure glDisable(cap: Cardinal); stdcall; External OpenGLLibName;
Procedure glDisableClientState(aarray: Cardinal); stdcall; External OpenGLLibName;
Procedure glDrawArrays(mode: Cardinal; first: Integer; count: Integer); stdcall; External OpenGLLibName;
Procedure glDrawBuffer(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glDrawElements(mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer); stdcall; External OpenGLLibName;
Procedure glDrawPixels(width, height: Integer; format, atype: Cardinal; const pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glEnable(cap: Cardinal); stdcall; External OpenGLLibName;
Procedure glEnableClientState(aarray: Cardinal); stdcall; External OpenGLLibName;
Procedure glEnd; stdcall; External OpenGLLibName;
Procedure glEndList; stdcall; External OpenGLLibName;
Procedure glFinish; stdcall; External OpenGLLibName;
Procedure glFlush; stdcall; External OpenGLLibName;
Procedure glFogf(pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glFogfv(pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glFogi(pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glFogiv(pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glFrontFace(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glFrustum(left, right, bottom, top, zNear, zFar: Double); stdcall; External OpenGLLibName;
Function glGenLists(range: Integer): Cardinal; stdcall; External OpenGLLibName;
Procedure glGenTextures(n: Integer; textures: PCardinal); stdcall; External OpenGLLibName;
Procedure glGetBooleanv(pname: Cardinal; params: PBoolean); stdcall; External OpenGLLibName;
Procedure glGetClipPlane(plane: Cardinal; equation: PDouble); stdcall; External OpenGLLibName;
Procedure glGetDoublev(pname: Cardinal; params: PDouble); stdcall; External OpenGLLibName;
Function glGetError: Cardinal; stdcall; External OpenGLLibName;
Procedure glGetFloatv(pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetIntegerv(pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glGetLightfv(light, pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetLightiv(light, pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glGetMapdv(target, query: Cardinal; v: PDouble); stdcall; External OpenGLLibName;
Procedure glGetMapfv(target, query: Cardinal; v: PSingle); stdcall; External OpenGLLibName;
Procedure glGetMapiv(target, query: Cardinal; v: Integer); stdcall; External OpenGLLibName;
Procedure glGetMaterialfv(face, pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetMaterialiv(face, pname: Cardinal; params: Integer); stdcall; External OpenGLLibName;
Procedure glGetPointerv(pname: Cardinal; params: Pointer); stdcall; External OpenGLLibName;
Procedure glGetPolygonStipple(mask: PByte); stdcall; External OpenGLLibName;
Function glGetString(name: Cardinal): PAnsiChar; stdcall; External OpenGLLibName;
Procedure glGetTexEnvfv(target, pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetTexEnviv(target, pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glGetTexGendv(coord, pname: Cardinal; params: PDouble); stdcall; External OpenGLLibName;
Procedure glGetTexGenfv(coord, pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetTexGeniv(coord, pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glGetTexImage(target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glGetTexLevelParameterfv(target: Cardinal; level: Integer; pname: Cardinal; params: Pointer); stdcall; External OpenGLLibName;
Procedure glGetTexLevelParameteriv(target: Cardinal; level: Integer; pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glGetTexParameterfv(target, pname: Cardinal; params: PSingle); stdcall; External OpenGLLibName;
Procedure glGetTexParameteriv(target, pname: Cardinal; params: PInteger); stdcall; External OpenGLLibName;
Procedure glHint(target, mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glIndexMask(mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glInterleavedArrays(format: Cardinal; stride: Integer; const pointer: Pointer); stdcall; External OpenGLLibName;
Function glIsEnabled(cap: Cardinal): Boolean; stdcall; External OpenGLLibName;
Function glIsList(list: Cardinal): Boolean; stdcall; External OpenGLLibName;
Function glIsTexture(texture: Cardinal): Boolean; stdcall; External OpenGLLibName;
Procedure glLightModelf(pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glLightModelfv(pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glLightModeli(pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glLightModeliv(pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glLightf(light, pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glLightfv(light, pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glLighti(light, pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glLightiv(light, pname: Cardinal; const params: Integer); stdcall; External OpenGLLibName;
Procedure glLineStipple(factor: Integer; pattern: Word); stdcall; External OpenGLLibName;
Procedure glLineWidth(width: Single); stdcall; External OpenGLLibName;
Procedure glLoadIdentity; stdcall; External OpenGLLibName;
Procedure glLoadMatrixd(const m: PDouble); stdcall; External OpenGLLibName;
Procedure glLoadMatrixf(const m: PSingle); stdcall; External OpenGLLibName;
Procedure glLogicOp(opcode: Cardinal); stdcall; External OpenGLLibName;
Procedure glMaterialf(face, pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glMaterialfv(face, pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glMateriali(face, pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glMaterialiv(face, pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glMatrixMode(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glMultMatrixd(const m: PDouble); stdcall; External OpenGLLibName;
Procedure glMultMatrixf(const m: PSingle); stdcall; External OpenGLLibName;
Procedure glNewList(list: Cardinal; mode: Cardinal); stdcall; External OpenGLLibName;

Procedure glNormal3f(nx, ny, nz: Single); stdcall; External OpenGLLibName;
Procedure glNormal3fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glNormalPointer(atype: Cardinal; stride: Integer; const pointer: Pointer); stdcall; External OpenGLLibName;

Procedure glOrtho(left, right, bottom, top, zNear, zFar: Double); stdcall; External OpenGLLibName;

Procedure glPointSize(size: Single); stdcall; External OpenGLLibName;
Procedure glPolygonMode(face, mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glPolygonOffset(factor, units: Single); stdcall; External OpenGLLibName;
Procedure glPolygonStipple(const mask: PByte); stdcall; External OpenGLLibName;
Procedure glPopAttrib; stdcall; External OpenGLLibName;
Procedure glPopClientAttrib; stdcall; External OpenGLLibName;
Procedure glPopMatrix; stdcall; External OpenGLLibName;
Procedure glPrioritizeTextures(n: Integer; const textures: PCardinal; const priorities: PSingle); stdcall; External OpenGLLibName;
Procedure glPushAttrib(mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glPushClientAttrib(mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glPushMatrix; stdcall; External OpenGLLibName;
Procedure glReadBuffer(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glReadPixels(x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer); stdcall; External OpenGLLibName;
Function glRenderMode(mode: Integer): Integer; stdcall; External OpenGLLibName;
Procedure glRotated(angle, x, y, z: Double); stdcall; External OpenGLLibName;
Procedure glRotatef(angle, x, y, z: Single); stdcall; External OpenGLLibName;
Procedure glScaled(x, y, z: Double); stdcall; External OpenGLLibName;
Procedure glScalef(x, y, z: Single); stdcall; External OpenGLLibName;
Procedure glScissor(x, y: Integer; width, height: Integer); stdcall; External OpenGLLibName;
Procedure glSelectBuffer(size: Integer; buffer: PCardinal); stdcall; External OpenGLLibName;
Procedure glShadeModel(mode: Cardinal); stdcall; External OpenGLLibName;
Procedure glStencilFunc(func: Cardinal; ref: Integer; mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glStencilMask(mask: Cardinal); stdcall; External OpenGLLibName;
Procedure glStencilOp(fail, zfail, zpass: Cardinal); stdcall; External OpenGLLibName;
Procedure glTexCoord1d(s: Double); stdcall; External OpenGLLibName;
Procedure glTexCoord1dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glTexCoord1f(s: Single); stdcall; External OpenGLLibName;
Procedure glTexCoord1fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glTexCoord1i(s: Integer); stdcall; External OpenGLLibName;
Procedure glTexCoord1iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glTexCoord1s(s: SmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord1sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord2d(s, t: Double); stdcall; External OpenGLLibName;
Procedure glTexCoord2dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glTexCoord2f(s, t: Single); stdcall; External OpenGLLibName;
Procedure glTexCoord2fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glTexCoord2i(s, t: Integer); stdcall; External OpenGLLibName;
Procedure glTexCoord2iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glTexCoord2s(s, t: SmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord2sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord3d(s, t, r: Double); stdcall; External OpenGLLibName;
Procedure glTexCoord3dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glTexCoord3f(s, t, r: Single); stdcall; External OpenGLLibName;
Procedure glTexCoord3fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glTexCoord3i(s, t, r: Integer); stdcall; External OpenGLLibName;
Procedure glTexCoord3iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glTexCoord3s(s, t, r: SmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord3sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord4d(s, t, r, q: Double); stdcall; External OpenGLLibName;
Procedure glTexCoord4dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glTexCoord4f(s, t, r, q: Single); stdcall; External OpenGLLibName;
Procedure glTexCoord4fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glTexCoord4i(s, t, r, q: Integer); stdcall; External OpenGLLibName;
Procedure glTexCoord4iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glTexCoord4s(s, t, r, q: SmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoord4sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glTexCoordPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); stdcall; External OpenGLLibName;
Procedure glTexEnvf(target: Cardinal; pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glTexEnvfv(target: Cardinal; pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glTexEnvi(target: Cardinal; pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glTexEnviv(target: Cardinal; pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glTexGend(coord: Cardinal; pname: Cardinal; param: Double); stdcall; External OpenGLLibName;
Procedure glTexGendv(coord: Cardinal; pname: Cardinal; const params: PDouble); stdcall; External OpenGLLibName;
Procedure glTexGenf(coord: Cardinal; pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glTexGenfv(coord: Cardinal; pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glTexGeni(coord: Cardinal; pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glTexGeniv(coord: Cardinal; pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glTexImage1D(target: Cardinal; level, internalformat: Integer; width: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glTexImage2D(target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glTexParameterf(target: Cardinal; pname: Cardinal; param: Single); stdcall; External OpenGLLibName;
Procedure glTexParameterfv(target: Cardinal; pname: Cardinal; const params: PSingle); stdcall; External OpenGLLibName;
Procedure glTexParameteri(target: Cardinal; pname: Cardinal; param: Integer); stdcall; External OpenGLLibName;
Procedure glTexParameteriv(target: Cardinal; pname: Cardinal; const params: PInteger); stdcall; External OpenGLLibName;
Procedure glTexSubImage1D(target: Cardinal; level, xoffset: Integer; width: Integer; format, atype: Cardinal; const pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glTexSubImage2D(target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer); stdcall; External OpenGLLibName;
Procedure glTranslated(x, y, z: Double); stdcall; External OpenGLLibName;
Procedure glTranslatef(x, y, z: Single); stdcall; External OpenGLLibName;
Procedure glVertex2d(x, y: Double); stdcall; External OpenGLLibName;
Procedure glVertex2dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glVertex2f(x, y: Single); stdcall; External OpenGLLibName;
Procedure glVertex2fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glVertex2i(x, y: Integer); stdcall; External OpenGLLibName;
Procedure glVertex2iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glVertex2s(x, y: SmallInt); stdcall; External OpenGLLibName;
Procedure glVertex2sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glVertex3d(x, y, z: Double); stdcall; External OpenGLLibName;
Procedure glVertex3dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glVertex3f(x, y, z: Single); stdcall; External OpenGLLibName;
Procedure glVertex3fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glVertex3i(x, y, z: Integer); stdcall; External OpenGLLibName;
Procedure glVertex3iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glVertex3s(x, y, z: SmallInt); stdcall; External OpenGLLibName;
Procedure glVertex3sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glVertex4d(x, y, z, w: Double); stdcall; External OpenGLLibName;
Procedure glVertex4dv(const v: PDouble); stdcall; External OpenGLLibName;
Procedure glVertex4f(x, y, z, w: Single); stdcall; External OpenGLLibName;
Procedure glVertex4fv(const v: PSingle); stdcall; External OpenGLLibName;
Procedure glVertex4i(x, y, z, w: Integer); stdcall; External OpenGLLibName;
Procedure glVertex4iv(const v: PInteger); stdcall; External OpenGLLibName;
Procedure glVertex4s(x, y, z, w: SmallInt); stdcall; External OpenGLLibName;
Procedure glVertex4sv(const v: PSmallInt); stdcall; External OpenGLLibName;
Procedure glVertexPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); stdcall; External OpenGLLibName;
Procedure glViewport(x, y: Integer; width, height: Integer); stdcall; External OpenGLLibName;

Var
  glDrawRangeElements: procedure(mode: Cardinal; start: Cardinal; _end: Cardinal; count: Integer; _type: Cardinal; const indices: Pointer); stdcall;
  glTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Integer; width: Integer; height: Integer; depth: Integer; border: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); stdcall;
  glTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); stdcall;
  glCopyTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); stdcall;
  glBlendEquation:Procedure(mode:Cardinal); stdcall;


Var
  glActiveTexture: procedure(texture: Cardinal); stdcall;
  glClientActiveTexture: procedure(texture: Cardinal); stdcall;
  glMultiTexCoord1d: procedure(target: Cardinal; s: Double); stdcall;
  glMultiTexCoord1dv: procedure(target: Cardinal; const v: PDouble); stdcall;
  glMultiTexCoord1f: procedure(target: Cardinal; s: Single); stdcall;
  glMultiTexCoord1fv: procedure(target: Cardinal; const v: PSingle); stdcall;
  glMultiTexCoord1i: procedure(target: Cardinal; s: Integer); stdcall;
  glMultiTexCoord1iv: procedure(target: Cardinal; const v: PInteger); stdcall;
  glMultiTexCoord1s: procedure(target: Cardinal; s: SmallInt); stdcall;
  glMultiTexCoord1sv: procedure(target: Cardinal; const v: PSmallInt); stdcall;
  glMultiTexCoord2d: procedure(target: Cardinal; s: Double; t: Double); stdcall;
  glMultiTexCoord2dv: procedure(target: Cardinal; const v: PDouble); stdcall;
  glMultiTexCoord2f: procedure(target: Cardinal; s: Single; t: Single); stdcall;
  glMultiTexCoord2fv: procedure(target: Cardinal; const v: PSingle); stdcall;
  glMultiTexCoord2i: procedure(target: Cardinal; s: Integer; t: Integer); stdcall;
  glMultiTexCoord2iv: procedure(target: Cardinal; const v: PInteger); stdcall;
  glMultiTexCoord2s: procedure(target: Cardinal; s: SmallInt; t: SmallInt); stdcall;
  glMultiTexCoord2sv: procedure(target: Cardinal; const v: PSmallInt); stdcall;
  glMultiTexCoord3d: procedure(target: Cardinal; s: Double; t: Double; r: Double); stdcall;
  glMultiTexCoord3dv: procedure(target: Cardinal; const v: PDouble); stdcall;
  glMultiTexCoord3f: procedure(target: Cardinal; s: Single; t: Single; r: Single); stdcall;
  glMultiTexCoord3fv: procedure(target: Cardinal; const v: PSingle); stdcall;
  glMultiTexCoord3i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer); stdcall;
  glMultiTexCoord3iv: procedure(target: Cardinal; const v: PInteger); stdcall;
  glMultiTexCoord3s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt); stdcall;
  glMultiTexCoord3sv: procedure(target: Cardinal; const v: PSmallInt); stdcall;
  glMultiTexCoord4d: procedure(target: Cardinal; s: Double; t: Double; r: Double; q: Double); stdcall;
  glMultiTexCoord4dv: procedure(target: Cardinal; const v: PDouble); stdcall;
  glMultiTexCoord4f: procedure(target: Cardinal; s: Single; t: Single; r: Single; q: Single); stdcall;
  glMultiTexCoord4fv: procedure(target: Cardinal; const v: PSingle); stdcall;
  glMultiTexCoord4i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer; q: Integer); stdcall;
  glMultiTexCoord4iv: procedure(target: Cardinal; const v: PInteger); stdcall;
  glMultiTexCoord4s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt; q: SmallInt); stdcall;
  glMultiTexCoord4sv: procedure(target: Cardinal; const v: PSmallInt); stdcall;
  glLoadTransposeMatrixf: procedure(const m: PSingle); stdcall;
  glLoadTransposeMatrixd: procedure(const m: PDouble); stdcall;
  glMultTransposeMatrixf: procedure(const m: PSingle); stdcall;
  glMultTransposeMatrixd: procedure(const m: PDouble); stdcall;
  glSampleCoverage: procedure(value: Single; invert: Boolean); stdcall;
  glCompressedTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; depth: Integer; border: Integer; imageSize: Integer; const data: Pointer); stdcall;
  glCompressedTexImage2D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; border: Integer; imageSize: Integer; const data: Pointer); stdcall;
  glCompressedTexImage1D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; border: Integer; imageSize: Integer; const data: Pointer); stdcall;
  glCompressedTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); stdcall;
  glCompressedTexSubImage2D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; width: Integer; height: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); stdcall;
  glCompressedTexSubImage1D: procedure(target: Cardinal; level: Integer; xoffset: Integer; width: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); stdcall;
  glGetCompressedTexImage: procedure(target: Cardinal; level: Integer; img: Pointer); stdcall;


Var
  glBlendFuncSeparate: procedure(sfactorRGB: Cardinal; dfactorRGB: Cardinal; sfactorAlpha: Cardinal; dfactorAlpha: Cardinal); stdcall;
  glFogCoordf: procedure(coord: Single); stdcall;
  glFogCoordfv: procedure(const coord: PSingle); stdcall;
  glFogCoordd: procedure(coord: Double); stdcall;
  glFogCoorddv: procedure(const coord: PDouble); stdcall;
  glFogCoordPointer: procedure(_type: Cardinal; stride: Integer; const pointer: Pointer); stdcall;
  glMultiDrawArrays: procedure(mode: Cardinal; first: PInteger; count: PInteger; primcount: Integer); stdcall;
  glMultiDrawElements: procedure(mode: Cardinal; const count: PInteger; _type: Cardinal; const indices: Pointer; primcount: Integer); stdcall;
  glPointParameterf: procedure(pname: Cardinal; param: Single); stdcall;
  glPointParameterfv: procedure(pname: Cardinal; const params: PSingle); stdcall;
  glPointParameteri: procedure(pname: Cardinal; param: Integer); stdcall;
  glPointParameteriv: procedure(pname: Cardinal; const params: PInteger); stdcall;


Procedure gluPerspective(fovy, aspect, zNear, zFar: Double); stdcall; External Glu32;
Procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: Double); stdcall; External Glu32;


Var
  glGenQueries: procedure(n: Integer; ids: PCardinal); stdcall;
  glDeleteQueries: procedure(n: Integer; const ids: PCardinal); stdcall;
  glIsQuery: function(id: Cardinal): Boolean; stdcall;
  glBeginQuery: procedure(target: Cardinal; id: Cardinal); stdcall;
  glEndQuery: procedure(target: Cardinal); stdcall;
  glGetQueryiv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetQueryObjectiv: procedure(id: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetQueryObjectuiv: procedure(id: Cardinal; pname: Cardinal; params: PCardinal); stdcall;
  glBindBuffer: procedure(target: Cardinal; buffer: Cardinal); stdcall;
  glDeleteBuffers: procedure(n: Integer; const buffers: PCardinal); stdcall;
  glGenBuffers: procedure(n: Integer; buffers: PCardinal); stdcall;
  glIsBuffer: function(buffer: Cardinal): Boolean; stdcall;
  glBufferData: procedure(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal); stdcall;
  glBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; const data: Pointer); stdcall;
  glGetBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; data: Pointer); stdcall;
  glMapBuffer: function(target: Cardinal; access: Cardinal): Pointer; stdcall;
  glUnmapBuffer: function(target: Cardinal): Boolean; stdcall;
  glGetBufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetBufferPointerv: procedure(target: Cardinal; pname: Cardinal; params: Pointer); stdcall;


Var
  glBlendEquationSeparate: procedure(modeRGB: Cardinal; modeAlpha: Cardinal); stdcall;
  glDrawBuffers: procedure(n: Integer; const bufs: PCardinal); stdcall;
  glStencilOpSeparate: procedure(face: Cardinal; sfail: Cardinal; dpfail: Cardinal; dppass: Cardinal); stdcall;
  glStencilFuncSeparate: procedure(frontfunc: Cardinal; backfunc: Cardinal; ref: Integer; mask: Cardinal); stdcall;
  glStencilMaskSeparate: procedure(face: Cardinal; mask: Cardinal); stdcall;
  glAttachShader: procedure(_program: Cardinal; shader: Cardinal); stdcall;
  glBindAttribLocation: procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); stdcall;
  glCompileShader: procedure(shader: Cardinal); stdcall;
  glCreateProgram: function(): Cardinal; stdcall;
  glCreateShader: function(_type: Cardinal): Cardinal; stdcall;
  glDeleteProgram: procedure(_program: Cardinal); stdcall;
  glDeleteShader: procedure(shader: Cardinal); stdcall;
  glDetachShader: procedure(_program: Cardinal; shader: Cardinal); stdcall;
  glDisableVertexAttribArray: procedure(index: Cardinal); stdcall;
  glEnableVertexAttribArray: procedure(index: Cardinal); stdcall;
  glGetActiveAttrib: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); stdcall;
  glGetActiveUniform: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); stdcall;
  glGetAttachedShaders: procedure(_program: Cardinal; maxCount: Integer; count: PInteger; obj: PCardinal); stdcall;
  glGetAttribLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; stdcall;
  glGetProgramiv: procedure(_program: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); stdcall;
  glGetShaderiv: procedure(shader: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); stdcall;
  glGetShaderSource: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; source: PAnsiChar); stdcall;
  glGetUniformLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; stdcall;
  glGetUniformfv: procedure(_program: Cardinal; location: Integer; params: PSingle); stdcall;
  glGetUniformiv: procedure(_program: Cardinal; location: Integer; params: PInteger); stdcall;
  glGetVertexAttribdv: procedure(index: Cardinal; pname: Cardinal; params: PDouble); stdcall;
  glGetVertexAttribfv: procedure(index: Cardinal; pname: Cardinal; params: PSingle); stdcall;
  glGetVertexAttribiv: procedure(index: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glGetVertexAttribPointerv: procedure(index: Cardinal; pname: Cardinal; pointer: Pointer); stdcall;
  glIsProgram: function(_program: Cardinal): Boolean; stdcall;
  glIsShader: function(shader: Cardinal): Boolean; stdcall;
  glLinkProgram: procedure(_program: Cardinal); stdcall;
  glShaderSource: procedure(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); stdcall;
  glUseProgram: procedure(_program: Cardinal); stdcall;
  glUniform1f: procedure(location: Integer; v0: Single); stdcall;
  glUniform2f: procedure(location: Integer; v0: Single; v1: Single); stdcall;
  glUniform3f: procedure(location: Integer; v0: Single; v1: Single; v2: Single); stdcall;
  glUniform4f: procedure(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); stdcall;
  glUniform1i: procedure(location: Integer; v0: Integer); stdcall;
  glUniform2i: procedure(location: Integer; v0: Integer; v1: Integer); stdcall;
  glUniform3i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer); stdcall;
  glUniform4i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); stdcall;
  glUniform1fv: procedure(location: Integer; count: Integer; const value: PSingle); stdcall;
  glUniform2fv: procedure(location: Integer; count: Integer; const value: PSingle); stdcall;
  glUniform3fv: procedure(location: Integer; count: Integer; const value: PSingle); stdcall;
  glUniform4fv: procedure(location: Integer; count: Integer; const value: PSingle); stdcall;
  glUniform1iv: procedure(location: Integer; count: Integer; const value: PInteger); stdcall;
  glUniform2iv: procedure(location: Integer; count: Integer; const value: PInteger); stdcall;
  glUniform3iv: procedure(location: Integer; count: Integer; const value: PInteger); stdcall;
  glUniform4iv: procedure(location: Integer; count: Integer; const value: PInteger); stdcall;
  glUniformMatrix2fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); stdcall;
  glUniformMatrix3fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); stdcall;
  glUniformMatrix4fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); stdcall;
  glValidateProgram: procedure(_program: Cardinal); stdcall;
  glVertexAttrib1d: procedure(index: Cardinal; x: Double); stdcall;
  glVertexAttrib1dv: procedure(index: Cardinal; const v: PDouble); stdcall;
  glVertexAttrib1f: procedure(index: Cardinal; x: Single); stdcall;
  glVertexAttrib1fv: procedure(index: Cardinal; const v: PSingle); stdcall;
  glVertexAttrib1s: procedure(index: Cardinal; x: SmallInt); stdcall;
  glVertexAttrib1sv: procedure(index: Cardinal; const v: PSmallInt); stdcall;
  glVertexAttrib2d: procedure(index: Cardinal; x: Double; y: Double); stdcall;
  glVertexAttrib2dv: procedure(index: Cardinal; const v: PDouble); stdcall;
  glVertexAttrib2f: procedure(index: Cardinal; x: Single; y: Single); stdcall;
  glVertexAttrib2fv: procedure(index: Cardinal; const v: PSingle); stdcall;
  glVertexAttrib2s: procedure(index: Cardinal; x: SmallInt; y: SmallInt); stdcall;
  glVertexAttrib2sv: procedure(index: Cardinal; const v: PSmallInt); stdcall;
  glVertexAttrib3d: procedure(index: Cardinal; x: Double; y: Double; z: Double); stdcall;
  glVertexAttrib3dv: procedure(index: Cardinal; const v: PDouble); stdcall;
  glVertexAttrib3f: procedure(index: Cardinal; x: Single; y: Single; z: Single); stdcall;
  glVertexAttrib3fv: procedure(index: Cardinal; const v: PSingle); stdcall;
  glVertexAttrib3s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt); stdcall;
  glVertexAttrib3sv: procedure(index: Cardinal; const v: PSmallInt); stdcall;
  glVertexAttrib4Nbv: procedure(index: Cardinal; const v: PShortint); stdcall;
  glVertexAttrib4Niv: procedure(index: Cardinal; const v: PInteger); stdcall;
  glVertexAttrib4Nsv: procedure(index: Cardinal; const v: PSmallInt); stdcall;
  glVertexAttrib4Nub: procedure(index: Cardinal; x: Byte; y: Byte; z: Byte; w: Byte); stdcall;
  glVertexAttrib4Nubv: procedure(index: Cardinal; const v: PByte); stdcall;
  glVertexAttrib4Nuiv: procedure(index: Cardinal; const v: PCardinal); stdcall;
  glVertexAttrib4Nusv: procedure(index: Cardinal; const v: PWord); stdcall;
  glVertexAttrib4bv: procedure(index: Cardinal; const v: PShortint); stdcall;
  glVertexAttrib4d: procedure(index: Cardinal; x: Double; y: Double; z: Double; w: Double); stdcall;
  glVertexAttrib4dv: procedure(index: Cardinal; const v: PDouble); stdcall;
  glVertexAttrib4f: procedure(index: Cardinal; x: Single; y: Single; z: Single; w: Single); stdcall;
  glVertexAttrib4fv: procedure(index: Cardinal; const v: PSingle); stdcall;
  glVertexAttrib4iv: procedure(index: Cardinal; const v: PInteger); stdcall;
  glVertexAttrib4s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt; w: SmallInt); stdcall;
  glVertexAttrib4sv: procedure(index: Cardinal; const v: PSmallInt); stdcall;
  glVertexAttrib4ubv: procedure(index: Cardinal; const v: PByte); stdcall;
  glVertexAttrib4uiv: procedure(index: Cardinal; const v: PCardinal); stdcall;
  glVertexAttrib4usv: procedure(index: Cardinal; const v: PWord); stdcall;
  glVertexAttribPointer: procedure(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); stdcall;
  glGenerateMipmap: procedure (target:Cardinal); stdcall;


Var
  glIsRenderbuffer: function(renderbuffer: Cardinal): Boolean; stdcall;
  glBindRenderbuffer: procedure(target: Cardinal; renderbuffer: Cardinal); stdcall;
  glDeleteRenderbuffers: procedure(n: Integer; const renderbuffers: PCardinal); stdcall;
  glGenRenderbuffers: procedure(n: Integer; renderbuffers: PCardinal); stdcall;
  glRenderbufferStorage: procedure(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer); stdcall;
  glGetRenderbufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); stdcall;
  glIsFramebuffer: function(framebuffer: Cardinal): Boolean; stdcall;
  glBindFramebuffer: procedure(target: Cardinal; framebuffer: Cardinal); stdcall;
  glDeleteFramebuffers: procedure(n: Integer; const framebuffers: PCardinal); stdcall;
  glGenFramebuffers: procedure(n: Integer; framebuffers: PCardinal); stdcall;
  glCheckFramebufferStatus: function(target: Cardinal): Cardinal; stdcall;
  glFramebufferTexture1D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); stdcall;
  glFramebufferTexture2D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); stdcall;
  glFramebufferTexture3D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer; zoffset: Integer); stdcall;
  glFramebufferRenderbuffer: procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); stdcall;
  glGetFramebufferAttachmentParameteriv: procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PInteger); stdcall;

  glRenderbufferStorageMultisample: procedure (target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer); stdcall;
  glBlitFramebuffer: procedure (srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal); stdcall;


  glEnableIndexedEXT: procedure(target, index:Integer); stdcall;
  glDisableIndexedEXT: procedure(target, index:Integer); stdcall;

Var
  wglGetPixelFormatAttribiv: function(hdc: HDC; iPixelFormat: Integer; iLayerPlane: Integer; nAttributes: Cardinal; const piAttributes: PInteger; piValues: PInteger): BOOL; stdcall;
  wglGetPixelFormatAttribfv: function(hdc: HDC; iPixelFormat: Integer; iLayerPlane: Integer; nAttributes: Cardinal; const piAttributes: PInteger; pfValues: PSingle): BOOL; stdcall;
  wglChoosePixelFormat: function(hdc: HDC; const piAttribIList: PInteger; const pfAttribFList: PSingle; nMaxFormats: Cardinal; piFormats: PInteger; nNumFormats: PCardinal): BOOL; stdcall;

// ******************************************************************************
//  WGL_ARB_pbuffer
// ******************************************************************************

Var
  wglCreatePbuffer: function(hDC: HDC; iPixelFormat: Integer; iWidth: Integer; iHeight: Integer; const piAttribList: PInteger): THandle; stdcall;
  wglGetPbufferDC: function(hPbuffer: THandle): HDC; stdcall;
  wglReleasePbufferDC: function(hPbuffer: THandle; hDC: HDC): Integer; stdcall;
  wglDestroyPbuffer: function(hPbuffer: THandle): BOOL; stdcall;
  wglQueryPbuffer: function(hPbuffer: THandle; iAttribute: Integer; piValue: PInteger): BOOL; stdcall;

// ******************************************************************************
//  WGL_EXT_swap_control
// ******************************************************************************
  wglSwapIntervalEXT: function(interval: Integer): BOOL; stdcall;
  wglGetSwapIntervalEXT: function(): Integer; stdcall;

Var
  glActiveStencilFaceEXT: Procedure(face: Integer); stdcall;

Procedure glLoadExtensions;
Function glExtensionSupported(Extension:AnsiString):Boolean;
Function glGetExtensionString():AnsiString;

Function InitMultisample(hWnd: HWND; pfd: PIXELFORMATDESCRIPTOR;  h_dc: HDC):Cardinal;

Implementation
Uses TERRA_Error, TERRA_Application, Math;

Var
  ExtensionsList:AnsiString='';

Function glGetExtensionString():AnsiString;
Begin
  Result := ExtensionsList;
End;

Function wglGetProcAddress(proc:PAnsiChar):Pointer; stdcall; external 'OpenGL32.dll';

Var
  glExtGetProcAddress:function(proc:PAnsiChar):Pointer; {$IFDEF MSWINDOWS}stdcall;{$ENDIF}
  OpenGLHandle:THandle;

Function glGetProcAddress(Proc:AnsiString; Alt:AnsiString=''):Pointer;cdecl;
Begin
  Result:=glExtGetProcAddress(PAnsiChar(Proc));
  If Not Assigned(Result) Then
    Result:=glExtGetProcAddress(PAnsiChar(Proc+'ARB'));

  If (Not Assigned(Result))And(Alt<>'#') Then
    If Alt<>'' Then
      Result := glGetProcAddress(Alt)
    Else
      Log(logWarning,'GL', 'Function '+Proc+' not avaliable.');
End;

Procedure LoadOpenGL(LibName:AnsiString);
Begin
  Log(logDebug, 'OpenGL', 'Loading OpenGL.');

  glExtGetProcAddress := wglGetProcAddress;

  OpenGLHandle := LoadLibraryA(PAnsiChar(LibName));
  If OpenGLHandle=0 Then
  Begin
    RaiseError('Error loading OpenGL from '+LibName);
    Exit;
  End;
End;

Procedure defaultglGenerateMipmap(Target:Cardinal); stdcall;
Begin
  glTexParameteri(Target, GL_GENERATE_MIPMAP, 1);
End;

function defaultwglSwapIntervalEXT(interval: Integer): BOOL; stdcall;
Begin
  Result := True;
  // do nothing
End;

Procedure dummyglActiveTexture(texture: Cardinal); stdcall;
Begin
  // do nothing
End;

Procedure dummyglClientActiveTexture(texture: Cardinal); stdcall;
Begin
  // do nothing
End;

Procedure glLoadExtensions;
Begin
  Log(logDebug, 'OpenGL', 'Loading extensions');

  // OpenGL 1.2
  glDrawRangeElements := glGetProcAddress('glDrawRangeElements');
  glTexImage3D := glGetProcAddress('glTexImage3D','#');
  glTexSubImage3D := glGetProcAddress('glTexSubImage3D');
  glCopyTexSubImage3D := glGetProcAddress('glCopyTexSubImage3D');
  glBlendEquation := glGetProcAddress('glBlendEquation');

  // OpenGL 1.3
  glActiveTexture := glGetProcAddress('glActiveTexture');
  glClientActiveTexture := glGetProcAddress('glClientActiveTexture');
  glMultiTexCoord1d := glGetProcAddress('glMultiTexCoord1d');
  glMultiTexCoord1dv := glGetProcAddress('glMultiTexCoord1dv');
  glMultiTexCoord1f := glGetProcAddress('glMultiTexCoord1f');
  glMultiTexCoord1fv := glGetProcAddress('glMultiTexCoord1fv');
  glMultiTexCoord1i := glGetProcAddress('glMultiTexCoord1i');
  glMultiTexCoord1iv := glGetProcAddress('glMultiTexCoord1iv');
  glMultiTexCoord1s := glGetProcAddress('glMultiTexCoord1s');
  glMultiTexCoord1sv := glGetProcAddress('glMultiTexCoord1sv');
  glMultiTexCoord2d := glGetProcAddress('glMultiTexCoord2d');
  glMultiTexCoord2dv := glGetProcAddress('glMultiTexCoord2dv');
  glMultiTexCoord2f := glGetProcAddress('glMultiTexCoord2f');
  glMultiTexCoord2fv := glGetProcAddress('glMultiTexCoord2fv');
  glMultiTexCoord2i := glGetProcAddress('glMultiTexCoord2i');
  glMultiTexCoord2iv := glGetProcAddress('glMultiTexCoord2iv');
  glMultiTexCoord2s := glGetProcAddress('glMultiTexCoord2s');
  glMultiTexCoord2sv := glGetProcAddress('glMultiTexCoord2sv');
  glMultiTexCoord3d := glGetProcAddress('glMultiTexCoord3d');
  glMultiTexCoord3dv := glGetProcAddress('glMultiTexCoord3dv');
  glMultiTexCoord3f := glGetProcAddress('glMultiTexCoord3f');
  glMultiTexCoord3fv := glGetProcAddress('glMultiTexCoord3fv');
  glMultiTexCoord3i := glGetProcAddress('glMultiTexCoord3i');
  glMultiTexCoord3iv := glGetProcAddress('glMultiTexCoord3iv');
  glMultiTexCoord3s := glGetProcAddress('glMultiTexCoord3s');
  glMultiTexCoord3sv := glGetProcAddress('glMultiTexCoord3sv');
  glMultiTexCoord4d := glGetProcAddress('glMultiTexCoord4d');
  glMultiTexCoord4dv := glGetProcAddress('glMultiTexCoord4dv');
  glMultiTexCoord4f := glGetProcAddress('glMultiTexCoord4f');
  glMultiTexCoord4fv := glGetProcAddress('glMultiTexCoord4fv');
  glMultiTexCoord4i := glGetProcAddress('glMultiTexCoord4i');
  glMultiTexCoord4iv := glGetProcAddress('glMultiTexCoord4iv');
  glMultiTexCoord4s := glGetProcAddress('glMultiTexCoord4s');
  glMultiTexCoord4sv := glGetProcAddress('glMultiTexCoord4sv');
  glLoadTransposeMatrixf := glGetProcAddress('glLoadTransposeMatrixf');
  glLoadTransposeMatrixd := glGetProcAddress('glLoadTransposeMatrixd');
  glMultTransposeMatrixf := glGetProcAddress('glMultTransposeMatrixf');
  glMultTransposeMatrixd := glGetProcAddress('glMultTransposeMatrixd');
  glSampleCoverage := glGetProcAddress('glSampleCoverage');
  glCompressedTexImage3D := glGetProcAddress('glCompressedTexImage3D');
  glCompressedTexImage2D := glGetProcAddress('glCompressedTexImage2D');
  glCompressedTexImage1D := glGetProcAddress('glCompressedTexImage1D');
  glCompressedTexSubImage3D := glGetProcAddress('glCompressedTexSubImage3D');
  glCompressedTexSubImage2D := glGetProcAddress('glCompressedTexSubImage2D');
  glCompressedTexSubImage1D := glGetProcAddress('glCompressedTexSubImage1D');
  glGetCompressedTexImage := glGetProcAddress('glGetCompressedTexImage');

  // OpenGL 1.4
  glBlendFuncSeparate := glGetProcAddress('glBlendFuncSeparate','#');
  glFogCoordf := glGetProcAddress('glFogCoordf','#');
  glFogCoordfv := glGetProcAddress('glFogCoordfv','#');
  glFogCoordd := glGetProcAddress('glFogCoordd','#');
  glFogCoorddv := glGetProcAddress('glFogCoorddv','#');
  glFogCoordPointer := glGetProcAddress('glFogCoordPointer','#');
  glMultiDrawArrays := glGetProcAddress('glMultiDrawArrays','#');
  glMultiDrawElements := glGetProcAddress('glMultiDrawElements','#');
  glPointParameterf := glGetProcAddress('glPointParameterf','#');
  glPointParameterfv := glGetProcAddress('glPointParameterfv','#');
  glPointParameteri := glGetProcAddress('glPointParameteri','#');
  glPointParameteriv := glGetProcAddress('glPointParameteriv','#');

  // OpenGL 1.5
  glGenQueries := glGetProcAddress('glGenQueries');
  glDeleteQueries := glGetProcAddress('glDeleteQueries');
  glIsQuery := glGetProcAddress('glIsQuery');
  glBeginQuery := glGetProcAddress('glBeginQuery');
  glEndQuery := glGetProcAddress('glEndQuery');
  glGetQueryiv := glGetProcAddress('glGetQueryiv');
  glGetQueryObjectiv := glGetProcAddress('glGetQueryObjectiv');
  glGetQueryObjectuiv := glGetProcAddress('glGetQueryObjectuiv');
  glBindBuffer := glGetProcAddress('glBindBuffer');
  glDeleteBuffers := glGetProcAddress('glDeleteBuffers');
  glGenBuffers := glGetProcAddress('glGenBuffers');
  glIsBuffer := glGetProcAddress('glIsBuffer');
  glBufferData := glGetProcAddress('glBufferData');
  glBufferSubData := glGetProcAddress('glBufferSubData');
  glGetBufferSubData := glGetProcAddress('glGetBufferSubData');
  glMapBuffer := glGetProcAddress('glMapBuffer');
  glUnmapBuffer := glGetProcAddress('glUnmapBuffer');
  glGetBufferParameteriv := glGetProcAddress('glGetBufferParameteriv');
  glGetBufferPointerv := glGetProcAddress('glGetBufferPointerv');

  // OpenGL 2.0
  glBlendEquationSeparate := glGetProcAddress('glBlendEquationSeparate','#');
  glDrawBuffers := glGetProcAddress('glDrawBuffers','#');
  glStencilOpSeparate := glGetProcAddress('glStencilOpSeparate','#');
  glStencilFuncSeparate := glGetProcAddress('glStencilFuncSeparate','#');
  glStencilMaskSeparate := glGetProcAddress('glStencilMaskSeparate','#');
  glAttachShader := glGetProcAddress('glAttachShader','glAttachObject');
  glBindAttribLocation := glGetProcAddress('glBindAttribLocation','#');
  glCompileShader := glGetProcAddress('glCompileShader','#');
  glCreateProgram := glGetProcAddress('glCreateProgram','glCreateProgramObject');
  glCreateShader := glGetProcAddress('glCreateShader','glCreateShaderObject');
  glDeleteProgram := glGetProcAddress('glDeleteProgram','glDeleteObject');
  glDeleteShader := glGetProcAddress('glDeleteShader','glDeleteObject');
  glDetachShader := glGetProcAddress('glDetachShader','glDetachObject');
  glDisableVertexAttribArray := glGetProcAddress('glDisableVertexAttribArray');
  glEnableVertexAttribArray := glGetProcAddress('glEnableVertexAttribArray');
  glGetActiveAttrib := glGetProcAddress('glGetActiveAttrib','#');
  glGetActiveUniform := glGetProcAddress('glGetActiveUniform','#');
  glGetAttachedShaders := glGetProcAddress('glGetAttachedShaders','glGetAttachedObjects');
  glGetAttribLocation := glGetProcAddress('glGetAttribLocation','#');
  glGetProgramiv := glGetProcAddress('glGetObjectParameteriv','glGetProgramiv');
  glGetProgramInfoLog := glGetProcAddress('glGetProgramInfoLog','glGetInfoLog');
  glGetShaderiv := glGetProcAddress('glGetShaderiv','glGetObjectParameteriv');
  glGetShaderInfoLog := glGetProcAddress('glGetShaderInfoLog','glGetInfoLog');
  glGetShaderSource := glGetProcAddress('glGetShaderSource','#');
  glGetUniformLocation := glGetProcAddress('glGetUniformLocation','#');
  glGetUniformfv := glGetProcAddress('glGetUniformfv','#');
  glGetUniformiv := glGetProcAddress('glGetUniformiv','#');
  glGetVertexAttribdv := glGetProcAddress('glGetVertexAttribdv','#');
  glGetVertexAttribfv := glGetProcAddress('glGetVertexAttribfv','#');
  glGetVertexAttribiv := glGetProcAddress('glGetVertexAttribiv','#');
  glGetVertexAttribPointerv := glGetProcAddress('glGetVertexAttribPointerv','#');
  glIsProgram := glGetProcAddress('glIsProgram','#');
  glIsShader := glGetProcAddress('glIsShader','#');
  glLinkProgram := glGetProcAddress('glLinkProgram','#');
  glShaderSource := glGetProcAddress('glShaderSource','#');
  glUseProgram := glGetProcAddress('glUseProgram','glUseProgramObject');
  glUniform1f := glGetProcAddress('glUniform1f','#');
  glUniform2f := glGetProcAddress('glUniform2f','#');
  glUniform3f := glGetProcAddress('glUniform3f','#');
  glUniform4f := glGetProcAddress('glUniform4f','#');
  glUniform1i := glGetProcAddress('glUniform1i','#');
  glUniform2i := glGetProcAddress('glUniform2i','#');
  glUniform3i := glGetProcAddress('glUniform3i','#');
  glUniform4i := glGetProcAddress('glUniform4i','#');
  glUniform1fv := glGetProcAddress('glUniform1fv','#');
  glUniform2fv := glGetProcAddress('glUniform2fv','#');
  glUniform3fv := glGetProcAddress('glUniform3fv','#');
  glUniform4fv := glGetProcAddress('glUniform4fv','#');
  glUniform1iv := glGetProcAddress('glUniform1iv','#');
  glUniform2iv := glGetProcAddress('glUniform2iv','#');
  glUniform3iv := glGetProcAddress('glUniform3iv','#');
  glUniform4iv := glGetProcAddress('glUniform4iv','#');
  glUniformMatrix2fv := glGetProcAddress('glUniformMatrix2fv','#');
  glUniformMatrix3fv := glGetProcAddress('glUniformMatrix3fv','#');
  glUniformMatrix4fv := glGetProcAddress('glUniformMatrix4fv','#');
  glValidateProgram := glGetProcAddress('glValidateProgram','#');
  glVertexAttrib1d := glGetProcAddress('glVertexAttrib1d');
  glVertexAttrib1dv := glGetProcAddress('glVertexAttrib1dv');
  glVertexAttrib1f := glGetProcAddress('glVertexAttrib1f');
  glVertexAttrib1fv := glGetProcAddress('glVertexAttrib1fv');
  glVertexAttrib1s := glGetProcAddress('glVertexAttrib1s');
  glVertexAttrib1sv := glGetProcAddress('glVertexAttrib1sv');
  glVertexAttrib2d := glGetProcAddress('glVertexAttrib2d');
  glVertexAttrib2dv := glGetProcAddress('glVertexAttrib2dv');
  glVertexAttrib2f := glGetProcAddress('glVertexAttrib2f');
  glVertexAttrib2fv := glGetProcAddress('glVertexAttrib2fv');
  glVertexAttrib2s := glGetProcAddress('glVertexAttrib2s');
  glVertexAttrib2sv := glGetProcAddress('glVertexAttrib2sv');
  glVertexAttrib3d := glGetProcAddress('glVertexAttrib3d');
  glVertexAttrib3dv := glGetProcAddress('glVertexAttrib3dv');
  glVertexAttrib3f := glGetProcAddress('glVertexAttrib3f');
  glVertexAttrib3fv := glGetProcAddress('glVertexAttrib3fv');
  glVertexAttrib3s := glGetProcAddress('glVertexAttrib3s');
  glVertexAttrib3sv := glGetProcAddress('glVertexAttrib3sv');
  glVertexAttrib4Nbv := glGetProcAddress('glVertexAttrib4Nbv');
  glVertexAttrib4Niv := glGetProcAddress('glVertexAttrib4Niv');
  glVertexAttrib4Nsv := glGetProcAddress('glVertexAttrib4Nsv');
  glVertexAttrib4Nub := glGetProcAddress('glVertexAttrib4Nub');
  glVertexAttrib4Nubv := glGetProcAddress('glVertexAttrib4Nubv');
  glVertexAttrib4Nuiv := glGetProcAddress('glVertexAttrib4Nuiv');
  glVertexAttrib4Nusv := glGetProcAddress('glVertexAttrib4Nusv');
  glVertexAttrib4bv := glGetProcAddress('glVertexAttrib4bv');
  glVertexAttrib4d := glGetProcAddress('glVertexAttrib4d');
  glVertexAttrib4dv := glGetProcAddress('glVertexAttrib4dv');
  glVertexAttrib4f := glGetProcAddress('glVertexAttrib4f');
  glVertexAttrib4fv := glGetProcAddress('glVertexAttrib4fv');
  glVertexAttrib4iv := glGetProcAddress('glVertexAttrib4iv');
  glVertexAttrib4s := glGetProcAddress('glVertexAttrib4s');
  glVertexAttrib4sv := glGetProcAddress('glVertexAttrib4sv');
  glVertexAttrib4ubv := glGetProcAddress('glVertexAttrib4ubv');
  glVertexAttrib4uiv := glGetProcAddress('glVertexAttrib4uiv');
  glVertexAttrib4usv := glGetProcAddress('glVertexAttrib4usv');
  glVertexAttribPointer := glGetProcAddress('glVertexAttribPointer');
  glGenerateMipmap := glGetProcAddress('glGenerateMipmap');

  If Not Assigned(glGenerateMipmap) Then
    glGenerateMipmap := defaultglGenerateMipmap;

  If Not Assigned(glActiveTexture) Then
    glActiveTexture := dummyglActiveTexture;

  If Not Assigned(glClientActiveTexture) Then
    glClientActiveTexture := dummyglClientActiveTexture;

  If glExtensionSupported('GL_EXT_framebuffer_object') Then
  Begin
    glIsRenderbuffer:= glGetProcAddress('glIsRenderbuffer');
    glBindRenderbuffer:= glGetProcAddress('glBindRenderbuffer');
    glDeleteRenderbuffers:= glGetProcAddress('glDeleteRenderbuffers');
    glGenRenderbuffers:= glGetProcAddress('glGenRenderbuffers');
    glRenderbufferStorage:= glGetProcAddress('glRenderbufferStorage');
    glGetRenderbufferParameteriv:= glGetProcAddress('glGetRenderbufferParameteriv');
    glIsFramebuffer:= glGetProcAddress('glIsFramebuffer');
    glBindFramebuffer:= glGetProcAddress('glBindFramebuffer');
    glDeleteFramebuffers:= glGetProcAddress('glDeleteFramebuffers');
    glGenFramebuffers:= glGetProcAddress('glGenFramebuffers');
    glCheckFramebufferStatus:= glGetProcAddress('glCheckFramebufferStatus');
    glFramebufferTexture1D:= glGetProcAddress('glFramebufferTexture1D');
    glFramebufferTexture2D:= glGetProcAddress('glFramebufferTexture2D');
    glFramebufferTexture3D:= glGetProcAddress('glFramebufferTexture3D');
    glFramebufferRenderbuffer:= glGetProcAddress('glFramebufferRenderbuffer');
    glGetFramebufferAttachmentParameteriv:= glGetProcAddress('glGetFramebufferAttachmentParameteriv');
    glGenerateMipmap:= glGetProcAddress('glGenerateMipmap');
    glRenderbufferStorageMultisample:= glGetProcAddress('glRenderbufferStorageMultisample');
    glBlitFramebuffer:= glGetProcAddress('glBlitFramebuffer');
  End;

  If glExtensionSupported('GL_EXT_stencil_two_side') Then
  Begin
    glActiveStencilFaceEXT := glGetProcAddress('glActiveStencilFaceEXT');
  End;

  If glExtensionSupported('GL_EXT_draw_buffers2') Then
  Begin
    glEnableIndexedEXT := glGetProcAddress('glEnableIndexedEXT');
    glDisableIndexedEXT := glGetProcAddress('glDisableIndexedEXT');
  End;

  wglCreatePbuffer := glGetProcAddress('wglCreatePbuffer');
  wglGetPbufferDC := glGetProcAddress('wglGetPbufferDC');
  wglReleasePbufferDC := glGetProcAddress('wglReleasePbufferDC');
  wglDestroyPbuffer := glGetProcAddress('wglDestroyPbuffer');
  wglQueryPbuffer := glGetProcAddress('wglQueryPbuffer');

  wglSwapIntervalEXT := glGetProcAddress('wglSwapIntervalEXT');
  wglGetSwapIntervalEXT := glGetProcAddress('wglGetSwapIntervalEXT');

  If Not Assigned(wglSwapIntervalEXT) Then
    wglSwapIntervalEXT := defaultwglSwapIntervalEXT;

  wglGetPixelFormatAttribiv := glGetProcAddress('wglGetPixelFormatAttribiv');
  wglGetPixelFormatAttribfv := glGetProcAddress('wglGetPixelFormatAttribfv');
  wglChoosePixelFormat := glGetProcAddress('wglChoosePixelFormat');
End;

Procedure FreeOpenGL;
Begin
  If OpenGLHandle<>0 Then
    FreeLibrary(OpenGLHandle);
End;

Function glExtensionSupported(Extension:AnsiString):Boolean;
Begin
  If (Extension='') Then
  Begin
    Result:=False;
    Exit;
  End;

  If (ExtensionsList='') Then
    ExtensionsList := PAnsiChar(glGetString(GL_EXTENSIONS));

  Result := Pos(Extension,ExtensionsList)>0;
End;

Function wglExtensionSupported(const extension:AnsiString): boolean;             // Je rozšíøení podporováno?
var
  wglGetExtString: function(hdc: HDC): PAnsiChar; stdcall;
  supported: PAnsiChar;
begin
  supported := nil;
  wglGetExtString := wglGetProcAddress('wglGetExtensionsStringARB');            // Pokud je to možné, pokusí se wglGetExtensionStringARB použít na aktuální DC
  if Assigned(wglGetExtString) then                                             // WGL OpenGL rozšíøení
    supported := wglGetExtString(wglGetCurrentDC);
  if supported = nil then                                                       // Zkusí ještì standardní OpenGL øetìzec s rozšíøeními
    supported := glGetString(GL_EXTENSIONS);
  if supported = nil then                                                       // Pokud selže i toto, není øetìzec dostupný
    begin
    Result := false;
    exit;
    end;
  if Pos(extension,supported) = 0 then                                          // Testování obsahu øetìzce
    begin
    Result := false;                                                            // Podøetìzec není v øetìzci
    exit;                                                                       // Rozšíøení nebylo nalezeno
    end;
  Result := true;                                                               // Rozšíøení bylo nalezeno
end;

Function InitMultisample(hWnd: HWND; pfd: PIXELFORMATDESCRIPTOR; h_dc: HDC):Cardinal;
Var
  wglChoosePixelFormatARB: function(hdc: HDC; const piAttribIList: PInteger; const pfAttribFList: PSingle; nMaxFormats: Cardinal; piFormats: PInteger; nNumFormats: PCardinal): BOOL; stdcall;
  pixelFormat: integer;
  valid: boolean;
  numFormats: UINT;
  fAttributes: array of Single;
  iAttributes: array of integer;
Begin
  Result := 0;

  if not wglExtensionSupported('WGL_ARB_multisample') then                    // Existuje øetìzec ve WGL
    Exit;

  wglChoosePixelFormatARB := wglGetProcAddress('wglChoosePixelFormatARB');      // Získání pixel formátu
  If not Assigned(wglChoosePixelFormatARB) then                                           // Daný pixel formát není dostupný
    exit;

  SetLength(fAttributes,2);
  fAttributes[0] := 0;
  fAttributes[1] := 0;
  SetLength(iAttributes,22);
  iAttributes[0] := WGL_DRAW_TO_WINDOW_ARB;
  iAttributes[1] := 1;
  iAttributes[2] := WGL_SUPPORT_OPENGL_ARB;
  iAttributes[3] := 1;
  iAttributes[4] := WGL_ACCELERATION_ARB;
  iAttributes[5] := WGL_FULL_ACCELERATION_ARB;
  iAttributes[6] := WGL_COLOR_BITS_ARB;
  iAttributes[7] := pfd.cColorBits;
  iAttributes[8] := WGL_ALPHA_BITS_ARB;
  iAttributes[9] := pfd.cAlphaBits;
  iAttributes[10] := WGL_DEPTH_BITS_ARB;
  iAttributes[11] := pfd.cDepthBits;
  iAttributes[12] := WGL_STENCIL_BITS_ARB;
  iAttributes[13] := pfd.cStencilBits;
  iAttributes[14] := WGL_DOUBLE_BUFFER_ARB;
  iAttributes[15] := 1;
  iAttributes[16] := WGL_SAMPLE_BUFFERS_ARB;
  iAttributes[17] := 1;
  iAttributes[18] := WGL_SAMPLES_ARB;
  iAttributes[19] := 4;
  iAttributes[20] := 0;
  iAttributes[21] := 0;
  valid := wglChoosePixelFormatARB(h_dc,@iattributes[0],@fattributes[0],1,@pixelFormat,@numFormats);
  If valid and (numFormats >= 1) then                                           // Vráceno true a poèet formátù je vìtší než jedna
  Begin
    Result := pixelFormat;
    SetLength(fAttributes,0);
    SetLength(iAttributes,0);
    Exit;
  End;

  iAttributes[19] := 2;                                                         // Ètyøi vzorkování nejsou dostupná, test dvou
  valid := wglChoosePixelFormatARB(h_dc,@iAttributes,@fAttributes,1,@pixelFormat,@numFormats);
  If valid and (numFormats >= 1) then
  Begin
    Result := pixelFormat;
    SetLength(fAttributes,0);
    SetLength(iAttributes,0);
    exit;
  End;

  SetLength(fAttributes,0);
  SetLength(iAttributes,0);
End;

Initialization
  SetExceptionMask([exInvalidOp, exDenormalized, {exZeroDivide, }exOverflow, exUnderflow, exPrecision]);
//  SetExceptionMask([exInvalidOp]);
  //Set8087CW($133F);
  LoadOpenGL(OpenGLLibName);
Finalization
  FreeOpenGL;
End.

