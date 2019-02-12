{
@abstract(OpenGL)
@author(Sergio Flores <>)
@created(February 25, 2006)
@lastmod(March 1, 2006)
The GL unit provides a cross-plataform OpenGL interface.
Supports versions upto OpenGL 2.0.
Only extensions used by LEAF are included.
}

Unit TERRA_GL;
{$I terra.inc}

Interface
Uses TERRA_Log, TERRA_Utils, TERRA_Math, GLX,DynLibs;

Const
  OpenGLLibName='libGL.so';

{$I glconsts.inc}

Procedure glAccum(op:Cardinal; value: Single); cdecl; External OpenGLLibName;
Procedure glAlphaFunc(func: Cardinal; ref: Single); cdecl; External OpenGLLibName;
Procedure glArrayElement(i: Integer); cdecl; External OpenGLLibName;
Procedure glBegin(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glBindTexture(target: Cardinal; texture: Cardinal); cdecl; External OpenGLLibName;
Procedure glBlendFunc(sfactor, dfactor: Cardinal); cdecl; External OpenGLLibName;
Procedure glCallList(list: Cardinal); cdecl; External OpenGLLibName;
Procedure glCallLists(n: Integer; atype: Cardinal; const lists: Pointer); cdecl; External OpenGLLibName;
Procedure glClear(mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glClearAccum(red, green, blue, alpha: Single); cdecl; External OpenGLLibName;
Procedure glClearColor(red, green, blue, alpha: Single); cdecl; External OpenGLLibName;
Procedure glClearDepth(depth: Double); cdecl; External OpenGLLibName;
Procedure glClearIndex(c: Single); cdecl; External OpenGLLibName;
Procedure glClearStencil(s: Integer); cdecl; External OpenGLLibName;
Procedure glClipPlane(plane: Cardinal; const equation: PDouble); cdecl; External OpenGLLibName;
Procedure glColor3b(red, green, blue: Shortint); cdecl; External OpenGLLibName;
Procedure glColor3bv(const v: PShortint); cdecl; External OpenGLLibName;
Procedure glColor3d(red, green, blue: Double); cdecl; External OpenGLLibName;
Procedure glColor3dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glColor3f(red, green, blue: Single); cdecl; External OpenGLLibName;
Procedure glColor3fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glColor3i(red, green, blue: Integer); cdecl; External OpenGLLibName;
Procedure glColor3iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glColor3s(red, green, blue: SmallInt); cdecl; External OpenGLLibName;
Procedure glColor3sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glColor3ub(red, green, blue: Byte); cdecl; External OpenGLLibName;
Procedure glColor3ubv(const v: PByte); cdecl; External OpenGLLibName;
Procedure glColor3ui(red, green, blue: Cardinal); cdecl; External OpenGLLibName;
Procedure glColor3uiv(const v: PCardinal); cdecl; External OpenGLLibName;
Procedure glColor3us(red, green, blue: Word); cdecl; External OpenGLLibName;
Procedure glColor3usv(const v: PWord); cdecl; External OpenGLLibName;
Procedure glColor4b(red, green, blue, alpha: Shortint); cdecl; External OpenGLLibName;
Procedure glColor4bv(const v: PShortint); cdecl; External OpenGLLibName;
Procedure glColor4d(red, green, blue, alpha: Double); cdecl; External OpenGLLibName;
Procedure glColor4dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glColor4f(red, green, blue, alpha: Single); cdecl; External OpenGLLibName;
Procedure glColor4fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glColor4i(red, green, blue, alpha: Integer); cdecl; External OpenGLLibName;
Procedure glColor4iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glColor4s(red, green, blue, alpha: SmallInt); cdecl; External OpenGLLibName;
Procedure glColor4sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glColor4ub(red, green, blue, alpha: Byte); cdecl; External OpenGLLibName;
Procedure glColor4ubv(const v: PByte); cdecl; External OpenGLLibName;
Procedure glColor4ui(red, green, blue, alpha: Cardinal); cdecl; External OpenGLLibName;
Procedure glColor4uiv(const v: PCardinal); cdecl; External OpenGLLibName;
Procedure glColor4us(red, green, blue, alpha: Word); cdecl; External OpenGLLibName;
Procedure glColor4usv(const v: PWord); cdecl; External OpenGLLibName;
Procedure glColorMask(red, green, blue, alpha: Boolean); cdecl; External OpenGLLibName;
Procedure glColorMaterial(face, mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glColorPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; External OpenGLLibName;
Procedure glCopyPixels(x, y: Integer; width, height: Integer; atype: Cardinal); cdecl; External OpenGLLibName;
Procedure glCopyTexImage1D (target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width: Integer; border: Integer); cdecl; External OpenGLLibName;
Procedure glCopyTexImage2D(target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer); cdecl; External OpenGLLibName;
Procedure glCopyTexSubImage1D(target: Cardinal; level, xoffset, x, y: Integer; width: Integer); cdecl; External OpenGLLibName;
Procedure glCopyTexSubImage2D(target: Cardinal; level, xoffset, yoffset, x, y: Integer; width, height: Integer); cdecl; External OpenGLLibName;
Procedure glCullFace(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glDeleteLists(list: Cardinal; range: Integer); cdecl; External OpenGLLibName;
Procedure glDeleteTextures(n: Integer; const textures: PCardinal); cdecl; External OpenGLLibName;
Procedure glDepthFunc(func: Cardinal); cdecl; External OpenGLLibName;
Procedure glDepthMask(flag: Boolean); cdecl; External OpenGLLibName;
Procedure glDepthRange(zNear, zFar: Double); cdecl; External OpenGLLibName;
Procedure glDisable(cap: Cardinal); cdecl; External OpenGLLibName;
Procedure glDisableClientState(aarray: Cardinal); cdecl; External OpenGLLibName;
Procedure glDrawArrays(mode: Cardinal; first: Integer; count: Integer); cdecl; External OpenGLLibName;
Procedure glDrawBuffer(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glDrawElements(mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer); cdecl; External OpenGLLibName;
Procedure glDrawPixels(width, height: Integer; format, atype: Cardinal; const pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glEnable(cap: Cardinal); cdecl; External OpenGLLibName;
Procedure glEnableClientState(aarray: Cardinal); cdecl; External OpenGLLibName;
Procedure glEnd; cdecl; External OpenGLLibName;
Procedure glEndList; cdecl; External OpenGLLibName;
Procedure glFinish; cdecl; External OpenGLLibName;
Procedure glFlush; cdecl; External OpenGLLibName;
Procedure glFogf(pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glFogfv(pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glFogi(pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glFogiv(pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glFrontFace(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glFrustum(left, right, bottom, top, zNear, zFar: Double); cdecl; External OpenGLLibName;
Function glGenLists(range: Integer): Cardinal; cdecl; External OpenGLLibName;
Procedure glGenTextures(n: Integer; textures: PCardinal); cdecl; External OpenGLLibName;
Procedure glGetBooleanv(pname: Cardinal; params: PBoolean); cdecl; External OpenGLLibName;
Procedure glGetClipPlane(plane: Cardinal; equation: PDouble); cdecl; External OpenGLLibName;
Procedure glGetDoublev(pname: Cardinal; params: PDouble); cdecl; External OpenGLLibName;
Function glGetError: Cardinal; cdecl; External OpenGLLibName;
Procedure glGetFloatv(pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetIntegerv(pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glGetLightfv(light, pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetLightiv(light, pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glGetMapdv(target, query: Cardinal; v: PDouble); cdecl; External OpenGLLibName;
Procedure glGetMapfv(target, query: Cardinal; v: PSingle); cdecl; External OpenGLLibName;
Procedure glGetMapiv(target, query: Cardinal; v: Integer); cdecl; External OpenGLLibName;
Procedure glGetMaterialfv(face, pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetMaterialiv(face, pname: Cardinal; params: Integer); cdecl; External OpenGLLibName;
Procedure glGetPointerv(pname: Cardinal; params: Pointer); cdecl; External OpenGLLibName;
Procedure glGetPolygonStipple(mask: PByte); cdecl; External OpenGLLibName;
Function glGetString(name: Cardinal): PAnsiChar; cdecl; External OpenGLLibName;
Procedure glGetTexEnvfv(target, pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetTexEnviv(target, pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glGetTexGendv(coord, pname: Cardinal; params: PDouble); cdecl; External OpenGLLibName;
Procedure glGetTexGenfv(coord, pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetTexGeniv(coord, pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glGetTexImage(target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glGetTexLevelParameterfv(target: Cardinal; level: Integer; pname: Cardinal; params: Pointer); cdecl; External OpenGLLibName;
Procedure glGetTexLevelParameteriv(target: Cardinal; level: Integer; pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glGetTexParameterfv(target, pname: Cardinal; params: PSingle); cdecl; External OpenGLLibName;
Procedure glGetTexParameteriv(target, pname: Cardinal; params: PInteger); cdecl; External OpenGLLibName;
Procedure glHint(target, mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glIndexMask(mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glInterleavedArrays(format: Cardinal; stride: Integer; const pointer: Pointer); cdecl; External OpenGLLibName;
Function glIsEnabled(cap: Cardinal): Boolean; cdecl; External OpenGLLibName;
Function glIsList(list: Cardinal): Boolean; cdecl; External OpenGLLibName;
Function glIsTexture(texture: Cardinal): Boolean; cdecl; External OpenGLLibName;
Procedure glLightModelf(pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glLightModelfv(pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glLightModeli(pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glLightModeliv(pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glLightf(light, pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glLightfv(light, pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glLighti(light, pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glLightiv(light, pname: Cardinal; const params: Integer); cdecl; External OpenGLLibName;
Procedure glLineStipple(factor: Integer; pattern: Word); cdecl; External OpenGLLibName;
Procedure glLineWidth(width: Single); cdecl; External OpenGLLibName;
Procedure glLoadIdentity; cdecl; External OpenGLLibName;
Procedure glLoadMatrixd(const m: PDouble); cdecl; External OpenGLLibName;
Procedure glLoadMatrixf(const m: PSingle); cdecl; External OpenGLLibName;
Procedure glLogicOp(opcode: Cardinal); cdecl; External OpenGLLibName;
Procedure glMaterialf(face, pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glMaterialfv(face, pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glMateriali(face, pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glMaterialiv(face, pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glMatrixMode(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glMultMatrixd(const m: PDouble); cdecl; External OpenGLLibName;
Procedure glMultMatrixf(const m: PSingle); cdecl; External OpenGLLibName;
Procedure glNewList(list: Cardinal; mode: Cardinal); cdecl; External OpenGLLibName;

Procedure glNormal3f(nx, ny, nz: Single); cdecl; External OpenGLLibName;
Procedure glNormal3fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glNormalPointer(atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; External OpenGLLibName;

Procedure glOrtho(left, right, bottom, top, zNear, zFar: Double); cdecl; External OpenGLLibName;

Procedure glPointSize(size: Single); cdecl; External OpenGLLibName;
Procedure glPolygonMode(face, mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glPolygonOffset(factor, units: Single); cdecl; External OpenGLLibName;
Procedure glPolygonStipple(const mask: PByte); cdecl; External OpenGLLibName;
Procedure glPopAttrib; cdecl; External OpenGLLibName;
Procedure glPopClientAttrib; cdecl; External OpenGLLibName;
Procedure glPopMatrix; cdecl; External OpenGLLibName;
Procedure glPrioritizeTextures(n: Integer; const textures: PCardinal; const priorities: PSingle); cdecl; External OpenGLLibName;
Procedure glPushAttrib(mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glPushClientAttrib(mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glPushMatrix; cdecl; External OpenGLLibName;
Procedure glReadBuffer(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glReadPixels(x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer); cdecl; External OpenGLLibName;
Function glRenderMode(mode: Integer): Integer; cdecl; External OpenGLLibName;
Procedure glRotated(angle, x, y, z: Double); cdecl; External OpenGLLibName;
Procedure glRotatef(angle, x, y, z: Single); cdecl; External OpenGLLibName;
Procedure glScaled(x, y, z: Double); cdecl; External OpenGLLibName;
Procedure glScalef(x, y, z: Single); cdecl; External OpenGLLibName;
Procedure glScissor(x, y: Integer; width, height: Integer); cdecl; External OpenGLLibName;
Procedure glSelectBuffer(size: Integer; buffer: PCardinal); cdecl; External OpenGLLibName;
Procedure glShadeModel(mode: Cardinal); cdecl; External OpenGLLibName;
Procedure glStencilFunc(func: Cardinal; ref: Integer; mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glStencilMask(mask: Cardinal); cdecl; External OpenGLLibName;
Procedure glStencilOp(fail, zfail, zpass: Cardinal); cdecl; External OpenGLLibName;
Procedure glTexCoord1d(s: Double); cdecl; External OpenGLLibName;
Procedure glTexCoord1dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glTexCoord1f(s: Single); cdecl; External OpenGLLibName;
Procedure glTexCoord1fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glTexCoord1i(s: Integer); cdecl; External OpenGLLibName;
Procedure glTexCoord1iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glTexCoord1s(s: SmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord1sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord2d(s, t: Double); cdecl; External OpenGLLibName;
Procedure glTexCoord2dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glTexCoord2f(s, t: Single); cdecl; External OpenGLLibName;
Procedure glTexCoord2fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glTexCoord2i(s, t: Integer); cdecl; External OpenGLLibName;
Procedure glTexCoord2iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glTexCoord2s(s, t: SmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord2sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord3d(s, t, r: Double); cdecl; External OpenGLLibName;
Procedure glTexCoord3dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glTexCoord3f(s, t, r: Single); cdecl; External OpenGLLibName;
Procedure glTexCoord3fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glTexCoord3i(s, t, r: Integer); cdecl; External OpenGLLibName;
Procedure glTexCoord3iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glTexCoord3s(s, t, r: SmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord3sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord4d(s, t, r, q: Double); cdecl; External OpenGLLibName;
Procedure glTexCoord4dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glTexCoord4f(s, t, r, q: Single); cdecl; External OpenGLLibName;
Procedure glTexCoord4fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glTexCoord4i(s, t, r, q: Integer); cdecl; External OpenGLLibName;
Procedure glTexCoord4iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glTexCoord4s(s, t, r, q: SmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoord4sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glTexCoordPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; External OpenGLLibName;
Procedure glTexEnvf(target: Cardinal; pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glTexEnvfv(target: Cardinal; pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glTexEnvi(target: Cardinal; pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glTexEnviv(target: Cardinal; pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glTexGend(coord: Cardinal; pname: Cardinal; param: Double); cdecl; External OpenGLLibName;
Procedure glTexGendv(coord: Cardinal; pname: Cardinal; const params: PDouble); cdecl; External OpenGLLibName;
Procedure glTexGenf(coord: Cardinal; pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glTexGenfv(coord: Cardinal; pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glTexGeni(coord: Cardinal; pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glTexGeniv(coord: Cardinal; pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glTexImage1D(target: Cardinal; level, internalformat: Integer; width: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glTexImage2D(target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glTexParameterf(target: Cardinal; pname: Cardinal; param: Single); cdecl; External OpenGLLibName;
Procedure glTexParameterfv(target: Cardinal; pname: Cardinal; const params: PSingle); cdecl; External OpenGLLibName;
Procedure glTexParameteri(target: Cardinal; pname: Cardinal; param: Integer); cdecl; External OpenGLLibName;
Procedure glTexParameteriv(target: Cardinal; pname: Cardinal; const params: PInteger); cdecl; External OpenGLLibName;
Procedure glTexSubImage1D(target: Cardinal; level, xoffset: Integer; width: Integer; format, atype: Cardinal; const pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glTexSubImage2D(target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer); cdecl; External OpenGLLibName;
Procedure glTranslated(x, y, z: Double); cdecl; External OpenGLLibName;
Procedure glTranslatef(x, y, z: Single); cdecl; External OpenGLLibName;
Procedure glVertex2d(x, y: Double); cdecl; External OpenGLLibName;
Procedure glVertex2dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glVertex2f(x, y: Single); cdecl; External OpenGLLibName;
Procedure glVertex2fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glVertex2i(x, y: Integer); cdecl; External OpenGLLibName;
Procedure glVertex2iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glVertex2s(x, y: SmallInt); cdecl; External OpenGLLibName;
Procedure glVertex2sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glVertex3d(x, y, z: Double); cdecl; External OpenGLLibName;
Procedure glVertex3dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glVertex3f(x, y, z: Single); cdecl; External OpenGLLibName;
Procedure glVertex3fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glVertex3i(x, y, z: Integer); cdecl; External OpenGLLibName;
Procedure glVertex3iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glVertex3s(x, y, z: SmallInt); cdecl; External OpenGLLibName;
Procedure glVertex3sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glVertex4d(x, y, z, w: Double); cdecl; External OpenGLLibName;
Procedure glVertex4dv(const v: PDouble); cdecl; External OpenGLLibName;
Procedure glVertex4f(x, y, z, w: Single); cdecl; External OpenGLLibName;
Procedure glVertex4fv(const v: PSingle); cdecl; External OpenGLLibName;
Procedure glVertex4i(x, y, z, w: Integer); cdecl; External OpenGLLibName;
Procedure glVertex4iv(const v: PInteger); cdecl; External OpenGLLibName;
Procedure glVertex4s(x, y, z, w: SmallInt); cdecl; External OpenGLLibName;
Procedure glVertex4sv(const v: PSmallInt); cdecl; External OpenGLLibName;
Procedure glVertexPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; External OpenGLLibName;
Procedure glViewport(x, y: Integer; width, height: Integer); cdecl; External OpenGLLibName;

//  GL_version_1_2

Var
  glDrawRangeElements: procedure(mode: Cardinal; start: Cardinal; _end: Cardinal; count: Integer; _type: Cardinal; const indices: Pointer); cdecl;
  glTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Integer; width: Integer; height: Integer; depth: Integer; border: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); cdecl;
  glTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); cdecl;
  glCopyTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  glBlendEquation:Procedure(mode:Cardinal); cdecl;

//  GL_version_1_3

Var
  glActiveTexture: procedure(texture: Cardinal); cdecl;
  glClientActiveTexture: procedure(texture: Cardinal); cdecl;
  glMultiTexCoord1d: procedure(target: Cardinal; s: Double); cdecl;
  glMultiTexCoord1dv: procedure(target: Cardinal; const v: PDouble); cdecl;
  glMultiTexCoord1f: procedure(target: Cardinal; s: Single); cdecl;
  glMultiTexCoord1fv: procedure(target: Cardinal; const v: PSingle); cdecl;
  glMultiTexCoord1i: procedure(target: Cardinal; s: Integer); cdecl;
  glMultiTexCoord1iv: procedure(target: Cardinal; const v: PInteger); cdecl;
  glMultiTexCoord1s: procedure(target: Cardinal; s: SmallInt); cdecl;
  glMultiTexCoord1sv: procedure(target: Cardinal; const v: PSmallInt); cdecl;
  glMultiTexCoord2d: procedure(target: Cardinal; s: Double; t: Double); cdecl;
  glMultiTexCoord2dv: procedure(target: Cardinal; const v: PDouble); cdecl;
  glMultiTexCoord2f: procedure(target: Cardinal; s: Single; t: Single); cdecl;
  glMultiTexCoord2fv: procedure(target: Cardinal; const v: PSingle); cdecl;
  glMultiTexCoord2i: procedure(target: Cardinal; s: Integer; t: Integer); cdecl;
  glMultiTexCoord2iv: procedure(target: Cardinal; const v: PInteger); cdecl;
  glMultiTexCoord2s: procedure(target: Cardinal; s: SmallInt; t: SmallInt); cdecl;
  glMultiTexCoord2sv: procedure(target: Cardinal; const v: PSmallInt); cdecl;
  glMultiTexCoord3d: procedure(target: Cardinal; s: Double; t: Double; r: Double); cdecl;
  glMultiTexCoord3dv: procedure(target: Cardinal; const v: PDouble); cdecl;
  glMultiTexCoord3f: procedure(target: Cardinal; s: Single; t: Single; r: Single); cdecl;
  glMultiTexCoord3fv: procedure(target: Cardinal; const v: PSingle); cdecl;
  glMultiTexCoord3i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer); cdecl;
  glMultiTexCoord3iv: procedure(target: Cardinal; const v: PInteger); cdecl;
  glMultiTexCoord3s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt); cdecl;
  glMultiTexCoord3sv: procedure(target: Cardinal; const v: PSmallInt); cdecl;
  glMultiTexCoord4d: procedure(target: Cardinal; s: Double; t: Double; r: Double; q: Double); cdecl;
  glMultiTexCoord4dv: procedure(target: Cardinal; const v: PDouble); cdecl;
  glMultiTexCoord4f: procedure(target: Cardinal; s: Single; t: Single; r: Single; q: Single); cdecl;
  glMultiTexCoord4fv: procedure(target: Cardinal; const v: PSingle); cdecl;
  glMultiTexCoord4i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer; q: Integer); cdecl;
  glMultiTexCoord4iv: procedure(target: Cardinal; const v: PInteger); cdecl;
  glMultiTexCoord4s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt; q: SmallInt); cdecl;
  glMultiTexCoord4sv: procedure(target: Cardinal; const v: PSmallInt); cdecl;
  glLoadTransposeMatrixf: procedure(const m: PSingle); cdecl;
  glLoadTransposeMatrixd: procedure(const m: PDouble); cdecl;
  glMultTransposeMatrixf: procedure(const m: PSingle); cdecl;
  glMultTransposeMatrixd: procedure(const m: PDouble); cdecl;
  glSampleCoverage: procedure(value: Single; invert: Boolean); cdecl;
  glCompressedTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; depth: Integer; border: Integer; imageSize: Integer; const data: Pointer); cdecl;
  glCompressedTexImage2D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; border: Integer; imageSize: Integer; const data: Pointer); cdecl;
  glCompressedTexImage1D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; border: Integer; imageSize: Integer; const data: Pointer); cdecl;
  glCompressedTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); cdecl;
  glCompressedTexSubImage2D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; width: Integer; height: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); cdecl;
  glCompressedTexSubImage1D: procedure(target: Cardinal; level: Integer; xoffset: Integer; width: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); cdecl;
  glGetCompressedTexImage: procedure(target: Cardinal; level: Integer; img: Pointer); cdecl;

//  GL_version_1_4
Var
  glBlendFuncSeparate: procedure(sfactorRGB: Cardinal; dfactorRGB: Cardinal; sfactorAlpha: Cardinal; dfactorAlpha: Cardinal); cdecl;
  glFogCoordf: procedure(coord: Single); cdecl;
  glFogCoordfv: procedure(const coord: PSingle); cdecl;
  glFogCoordd: procedure(coord: Double); cdecl;
  glFogCoorddv: procedure(const coord: PDouble); cdecl;
  glFogCoordPointer: procedure(_type: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glMultiDrawArrays: procedure(mode: Cardinal; first: PInteger; count: PInteger; primcount: Integer); cdecl;
  glMultiDrawElements: procedure(mode: Cardinal; const count: PInteger; _type: Cardinal; const indices: Pointer; primcount: Integer); cdecl;
  glPointParameterf: procedure(pname: Cardinal; param: Single); cdecl;
  glPointParameterfv: procedure(pname: Cardinal; const params: PSingle); cdecl;
  glPointParameteri: procedure(pname: Cardinal; param: Integer); cdecl;
  glPointParameteriv: procedure(pname: Cardinal; const params: PInteger); cdecl;

//  GL_version_1_5
Var
  glGenQueries: procedure(n: Integer; ids: PCardinal); cdecl;
  glDeleteQueries: procedure(n: Integer; const ids: PCardinal); cdecl;
  glIsQuery: function(id: Cardinal): Boolean; cdecl;
  glBeginQuery: procedure(target: Cardinal; id: Cardinal); cdecl;
  glEndQuery: procedure(target: Cardinal); cdecl;
  glGetQueryiv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetQueryObjectiv: procedure(id: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetQueryObjectuiv: procedure(id: Cardinal; pname: Cardinal; params: PCardinal); cdecl;
  glBindBuffer: procedure(target: Cardinal; buffer: Cardinal); cdecl;
  glDeleteBuffers: procedure(n: Integer; const buffers: PCardinal); cdecl;
  glGenBuffers: procedure(n: Integer; buffers: PCardinal); cdecl;
  glIsBuffer: function(buffer: Cardinal): Boolean; cdecl;
  glBufferData: procedure(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal); cdecl;
  glBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; const data: Pointer); cdecl;
  glGetBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; data: Pointer); cdecl;
  glMapBuffer: function(target: Cardinal; access: Cardinal): Pointer; cdecl;
  glUnmapBuffer: function(target: Cardinal): Boolean; cdecl;
  glGetBufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetBufferPointerv: procedure(target: Cardinal; pname: Cardinal; params: Pointer); cdecl;

//  GL_version_2_0
Var
  glBlendEquationSeparate: procedure(modeRGB: Cardinal; modeAlpha: Cardinal); cdecl;
  glDrawBuffers: procedure(n: Integer; const bufs: PCardinal); cdecl;
  glStencilOpSeparate: procedure(face: Cardinal; sfail: Cardinal; dpfail: Cardinal; dppass: Cardinal); cdecl;
  glStencilFuncSeparate: procedure(frontfunc: Cardinal; backfunc: Cardinal; ref: Integer; mask: Cardinal); cdecl;
  glStencilMaskSeparate: procedure(face: Cardinal; mask: Cardinal); cdecl;
  glAttachShader: procedure(_program: Cardinal; shader: Cardinal); cdecl;
  glBindAttribLocation: procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); cdecl;
  glCompileShader: procedure(shader: Cardinal); cdecl;
  glCreateProgram: function(): Cardinal; cdecl;
  glCreateShader: function(_type: Cardinal): Cardinal; cdecl;
  glDeleteProgram: procedure(_program: Cardinal); cdecl;
  glDeleteShader: procedure(shader: Cardinal); cdecl;
  glDetachShader: procedure(_program: Cardinal; shader: Cardinal); cdecl;
  glDisableVertexAttribArray: procedure(index: Cardinal); cdecl;
  glEnableVertexAttribArray: procedure(index: Cardinal); cdecl;
  glGetActiveAttrib: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); cdecl;
  glGetActiveUniform: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); cdecl;
  glGetAttachedShaders: procedure(_program: Cardinal; maxCount: Integer; count: PInteger; obj: PCardinal); cdecl;
  glGetAttribLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; cdecl;
  glGetProgramiv: procedure(_program: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl;
  glGetShaderiv: procedure(shader: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl;
  glGetShaderSource: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; source: PAnsiChar); cdecl;
  glGetUniformLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; cdecl;
  glGetUniformfv: procedure(_program: Cardinal; location: Integer; params: PSingle); cdecl;
  glGetUniformiv: procedure(_program: Cardinal; location: Integer; params: PInteger); cdecl;
  glGetVertexAttribdv: procedure(index: Cardinal; pname: Cardinal; params: PDouble); cdecl;
  glGetVertexAttribfv: procedure(index: Cardinal; pname: Cardinal; params: PSingle); cdecl;
  glGetVertexAttribiv: procedure(index: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glGetVertexAttribPointerv: procedure(index: Cardinal; pname: Cardinal; pointer: Pointer); cdecl;
  glIsProgram: function(_program: Cardinal): Boolean; cdecl;
  glIsShader: function(shader: Cardinal): Boolean; cdecl;
  glLinkProgram: procedure(_program: Cardinal); cdecl;
  glShaderSource: procedure(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); cdecl;
  glUseProgram: procedure(_program: Cardinal); cdecl;
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
  glValidateProgram: procedure(_program: Cardinal); cdecl;
  glVertexAttrib1d: procedure(index: Cardinal; x: Double); cdecl;
  glVertexAttrib1dv: procedure(index: Cardinal; const v: PDouble); cdecl;
  glVertexAttrib1f: procedure(index: Cardinal; x: Single); cdecl;
  glVertexAttrib1fv: procedure(index: Cardinal; const v: PSingle); cdecl;
  glVertexAttrib1s: procedure(index: Cardinal; x: SmallInt); cdecl;
  glVertexAttrib1sv: procedure(index: Cardinal; const v: PSmallInt); cdecl;
  glVertexAttrib2d: procedure(index: Cardinal; x: Double; y: Double); cdecl;
  glVertexAttrib2dv: procedure(index: Cardinal; const v: PDouble); cdecl;
  glVertexAttrib2f: procedure(index: Cardinal; x: Single; y: Single); cdecl;
  glVertexAttrib2fv: procedure(index: Cardinal; const v: PSingle); cdecl;
  glVertexAttrib2s: procedure(index: Cardinal; x: SmallInt; y: SmallInt); cdecl;
  glVertexAttrib2sv: procedure(index: Cardinal; const v: PSmallInt); cdecl;
  glVertexAttrib3d: procedure(index: Cardinal; x: Double; y: Double; z: Double); cdecl;
  glVertexAttrib3dv: procedure(index: Cardinal; const v: PDouble); cdecl;
  glVertexAttrib3f: procedure(index: Cardinal; x: Single; y: Single; z: Single); cdecl;
  glVertexAttrib3fv: procedure(index: Cardinal; const v: PSingle); cdecl;
  glVertexAttrib3s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt); cdecl;
  glVertexAttrib3sv: procedure(index: Cardinal; const v: PSmallInt); cdecl;
  glVertexAttrib4Nbv: procedure(index: Cardinal; const v: PShortint); cdecl;
  glVertexAttrib4Niv: procedure(index: Cardinal; const v: PInteger); cdecl;
  glVertexAttrib4Nsv: procedure(index: Cardinal; const v: PSmallInt); cdecl;
  glVertexAttrib4Nub: procedure(index: Cardinal; x: Byte; y: Byte; z: Byte; w: Byte); cdecl;
  glVertexAttrib4Nubv: procedure(index: Cardinal; const v: PByte); cdecl;
  glVertexAttrib4Nuiv: procedure(index: Cardinal; const v: PCardinal); cdecl;
  glVertexAttrib4Nusv: procedure(index: Cardinal; const v: PWord); cdecl;
  glVertexAttrib4bv: procedure(index: Cardinal; const v: PShortint); cdecl;
  glVertexAttrib4d: procedure(index: Cardinal; x: Double; y: Double; z: Double; w: Double); cdecl;
  glVertexAttrib4dv: procedure(index: Cardinal; const v: PDouble); cdecl;
  glVertexAttrib4f: procedure(index: Cardinal; x: Single; y: Single; z: Single; w: Single); cdecl;
  glVertexAttrib4fv: procedure(index: Cardinal; const v: PSingle); cdecl;
  glVertexAttrib4iv: procedure(index: Cardinal; const v: PInteger); cdecl;
  glVertexAttrib4s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt; w: SmallInt); cdecl;
  glVertexAttrib4sv: procedure(index: Cardinal; const v: PSmallInt); cdecl;
  glVertexAttrib4ubv: procedure(index: Cardinal; const v: PByte); cdecl;
  glVertexAttrib4uiv: procedure(index: Cardinal; const v: PCardinal); cdecl;
  glVertexAttrib4usv: procedure(index: Cardinal; const v: PWord); cdecl;
  glVertexAttribPointer: procedure(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); cdecl;
  glGenerateMipmap: procedure (target:Cardinal); cdecl;

// ******************************************************************************
//  GL_EXT_framebuffer_object
// ******************************************************************************
Var
  glIsRenderbuffer: function(renderbuffer: Cardinal): Boolean; cdecl;
  glBindRenderbuffer: procedure(target: Cardinal; renderbuffer: Cardinal); cdecl;
  glDeleteRenderbuffers: procedure(n: Integer; const renderbuffers: PCardinal); cdecl;
  glGenRenderbuffers: procedure(n: Integer; renderbuffers: PCardinal); cdecl;
  glRenderbufferStorage: procedure(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer); cdecl;
  glGetRenderbufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); cdecl;
  glIsFramebuffer: function(framebuffer: Cardinal): Boolean; cdecl;
  glBindFramebuffer: procedure(target: Cardinal; framebuffer: Cardinal); cdecl;
  glDeleteFramebuffers: procedure(n: Integer; const framebuffers: PCardinal); cdecl;
  glGenFramebuffers: procedure(n: Integer; framebuffers: PCardinal); cdecl;
  glCheckFramebufferStatus: function(target: Cardinal): Cardinal; cdecl;
  glFramebufferTexture1D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); cdecl;
  glFramebufferTexture2D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); cdecl;
  glFramebufferTexture3D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer; zoffset: Integer); cdecl;
  glFramebufferRenderbuffer: procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); cdecl;
  glGetFramebufferAttachmentParameteriv: procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PInteger); cdecl;

  glRenderbufferStorageMultisample: procedure (target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer); cdecl;
  glBlitFramebuffer: procedure (srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal); cdecl;


  glEnableIndexedEXT: procedure(target, index:Integer); cdecl;
  glDisableIndexedEXT: procedure(target, index:Integer); cdecl;

  glActiveStencilFaceEXT: Procedure(face: Integer); cdecl = nil;

Procedure glLoadExtensions;

Function glGetExtensionString():AnsiString;

Function glExtensionSupported(Extension:AnsiString):Boolean;

Var
  ExtensionsList:AnsiString='';

Implementation
Uses TERRA_Application, TERRA_Error;

Function glGetExtensionString():AnsiString;
Begin
  Result := ExtensionsList;
End;

Var
  glExtGetProcAddress:function(proc:PAnsiChar):Pointer;
  OpenGLHandle:TLibHandle;

Procedure xglClientActiveTexture(texture: Cardinal);cdecl;
Begin
End;

Procedure xglActiveTexture(texture: Cardinal); cdecl;
Begin
End;

Function glxGetProcAddress(proc:PAnsiChar):Pointer;
Begin
  Result := GetProcAddress(OpenGLHandle, proc);
End;

Function glGetProcAddress(Proc:AnsiString; Alt:AnsiString=''):Pointer;cdecl;
Begin
  Result:=glExtGetProcAddress(PAnsiChar(Proc));
  If Not Assigned(Result) Then
    Result:=glExtGetProcAddress(PAnsiChar(Proc+'ARB'));

  If (Not Assigned(Result))And(Alt<>'#') Then
    If Alt<>'' Then
      Result:=glGetProcAddress(Alt)
    Else
      Log(logWarning,'GL', 'Function '+Proc+' not avaliable.');
End;

Procedure LoadOpenGL(LibName:AnsiString);
Begin
	TERRA_Log.Log(logDebug, 'GL', 'loading openGL');
  Log(logDebug, 'OpenGL', 'Loading library');

  glExtGetProcAddress := glxGetProcAddress;

  OpenGLHandle := LoadLibrary(PAnsiChar(LibName));
  If OpenGLHandle=0 Then
  Begin
    RaiseError('Error loading OpenGL from '+LibName);
    Exit;
  End;

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

  If Not Assigned(glActiveTexture) Then
    glActiveTexture:=xglActiveTexture;
  If Not Assigned(glClientActiveTexture) Then
    glClientActiveTexture:=xglClientActiveTexture;

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
    glBlitFramebuffer:= glGetProcAddress('glBlitFramebuffer');;
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

Initialization
  Set8087CW($133F);
  LoadOpenGL(OpenGLLibName);
Finalization
  FreeOpenGL;
End.
