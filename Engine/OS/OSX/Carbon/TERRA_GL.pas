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

{$LINKLIB GL}

Interface
Uses TERRA_Log, DynLibs;

Const
  OpenGLLibName='/System/Library/Frameworks/OpenGL.framework/Versions/Current/Libraries/libGL.dylib';

{$I glconsts.inc}

Procedure glAccum(op:Cardinal; value: Single); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glAlphaFunc(func: Cardinal; ref: Single); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glArrayElement(i: Integer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glBegin(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glBindTexture(target: Cardinal; texture: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glBlendFunc(sfactor, dfactor: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCallList(list: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCallLists(n: Integer; atype: Cardinal; const lists: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClear(mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClearAccum(red, green, blue, alpha: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClearColor(red, green, blue, alpha: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClearDepth(depth: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClearIndex(c: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClearStencil(s: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glClipPlane(plane: Cardinal; const equation: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3b(red, green, blue: Shortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3bv(const v: PShortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3d(red, green, blue: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3f(red, green, blue: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3i(red, green, blue: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3s(red, green, blue: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3ub(red, green, blue: Byte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3ubv(const v: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3ui(red, green, blue: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3uiv(const v: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3us(red, green, blue: Word); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor3usv(const v: PWord); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4b(red, green, blue, alpha: Shortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4bv(const v: PShortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4d(red, green, blue, alpha: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4f(red, green, blue, alpha: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4i(red, green, blue, alpha: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4s(red, green, blue, alpha: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4ub(red, green, blue, alpha: Byte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4ubv(const v: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4ui(red, green, blue, alpha: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4uiv(const v: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4us(red, green, blue, alpha: Word); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColor4usv(const v: PWord); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColorMask(red, green, blue, alpha: Boolean); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColorMaterial(face, mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glColorPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCopyPixels(x, y: Integer; width, height: Integer; atype: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCopyTexImage1D (target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width: Integer; border: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCopyTexImage2D(target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCopyTexSubImage1D(target: Cardinal; level, xoffset, x, y: Integer; width: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCopyTexSubImage2D(target: Cardinal; level, xoffset, yoffset, x, y: Integer; width, height: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glCullFace(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDeleteLists(list: Cardinal; range: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDeleteTextures(n: Integer; const textures: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDepthFunc(func: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDepthMask(flag: Boolean); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDepthRange(zNear, zFar: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDisable(cap: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDisableClientState(aarray: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDrawArrays(mode: Cardinal; first: Integer; count: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDrawBuffer(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDrawElements(mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glDrawPixels(width, height: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glEnable(cap: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glEnableClientState(aarray: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glEnd; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glEndList; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFinish; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFlush; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFogf(pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFogfv(pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFogi(pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFogiv(pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFrontFace(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glFrustum(left, right, bottom, top, zNear, zFar: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glGenLists(range: Integer): Cardinal; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGenTextures(n: Integer; textures: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetBooleanv(pname: Cardinal; params: PBoolean); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetClipPlane(plane: Cardinal; equation: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetDoublev(pname: Cardinal; params: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glGetError: Cardinal; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetFloatv(pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetIntegerv(pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetLightfv(light, pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetLightiv(light, pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetMapdv(target, query: Cardinal; v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetMapfv(target, query: Cardinal; v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetMapiv(target, query: Cardinal; v: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetMaterialfv(face, pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetMaterialiv(face, pname: Cardinal; params: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetPointerv(pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetPolygonStipple(mask: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glGetString(name: Cardinal): PAnsiChar; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexEnvfv(target, pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexEnviv(target, pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexGendv(coord, pname: Cardinal; params: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexGenfv(coord, pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexGeniv(coord, pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexImage(target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexLevelParameterfv(target: Cardinal; level: Integer; pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexLevelParameteriv(target: Cardinal; level: Integer; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexParameterfv(target, pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glGetTexParameteriv(target, pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glHint(target, mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glIndexMask(mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glInterleavedArrays(format: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glIsEnabled(cap: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glIsList(list: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glIsTexture(texture: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightModelf(pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightModelfv(pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightModeli(pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightModeliv(pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightf(light, pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightfv(light, pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLighti(light, pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLightiv(light, pname: Cardinal; const params: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLineStipple(factor: Integer; pattern: Word); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLineWidth(width: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLoadIdentity; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLoadMatrixd(const m: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLoadMatrixf(const m: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glLogicOp(opcode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMaterialf(face, pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMaterialfv(face, pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMateriali(face, pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMaterialiv(face, pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMatrixMode(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMultMatrixd(const m: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glMultMatrixf(const m: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glNewList(list: Cardinal; mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;

Procedure glNormal3f(nx, ny, nz: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glNormal3fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glNormalPointer(atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;

Procedure glOrtho(left, right, bottom, top, zNear, zFar: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;

Procedure glPointSize(size: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPolygonMode(face, mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPolygonOffset(factor, units: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPolygonStipple(const mask: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPopAttrib; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPopClientAttrib; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPopMatrix; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPrioritizeTextures(n: Integer; const textures: PCardinal; const priorities: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPushAttrib(mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPushClientAttrib(mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glPushMatrix; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glReadBuffer(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glReadPixels(x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Function glRenderMode(mode: Integer): Integer; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glRotated(angle, x, y, z: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glRotatef(angle, x, y, z: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glScaled(x, y, z: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glScalef(x, y, z: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glScissor(x, y: Integer; width, height: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glSelectBuffer(size: Integer; buffer: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glShadeModel(mode: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glStencilFunc(func: Cardinal; ref: Integer; mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glStencilMask(mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glStencilOp(fail, zfail, zpass: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1d(s: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1f(s: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1i(s: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1s(s: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord1sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2d(s, t: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2f(s, t: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2i(s, t: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2s(s, t: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord2sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3d(s, t, r: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3f(s, t, r: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3i(s, t, r: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3s(s, t, r: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord3sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4d(s, t, r, q: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4f(s, t, r, q: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4i(s, t, r, q: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4s(s, t, r, q: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoord4sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexCoordPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexEnvf(target: Cardinal; pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexEnvfv(target: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexEnvi(target: Cardinal; pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexEnviv(target: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGend(coord: Cardinal; pname: Cardinal; param: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGendv(coord: Cardinal; pname: Cardinal; const params: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGenf(coord: Cardinal; pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGenfv(coord: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGeni(coord: Cardinal; pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexGeniv(coord: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexImage1D(target: Cardinal; level, internalformat: Integer; width: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexImage2D(target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexParameterf(target: Cardinal; pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexParameterfv(target: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexParameteri(target: Cardinal; pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexParameteriv(target: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexSubImage1D(target: Cardinal; level, xoffset: Integer; width: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTexSubImage2D(target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTranslated(x, y, z: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glTranslatef(x, y, z: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2d(x, y: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2f(x, y: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2i(x, y: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2s(x, y: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex2sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3d(x, y, z: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3f(x, y, z: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3i(x, y, z: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3s(x, y, z: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex3sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4d(x, y, z, w: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4dv(const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4f(x, y, z, w: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4fv(const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4i(x, y, z, w: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4iv(const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4s(x, y, z, w: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertex4sv(const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glVertexPointer(size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;
Procedure glViewport(x, y: Integer; width, height: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF} External OpenGLLibName;

//  GL_version_1_2
Var
  glDrawRangeElements: procedure(mode: Cardinal; start: Cardinal; _end: Cardinal; count: Integer; _type: Cardinal; const indices: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Integer; width: Integer; height: Integer; depth: Integer; border: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCopyTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBlendEquation:Procedure(mode:Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_3
Var
  glActiveTexture: procedure(texture: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glClientActiveTexture: procedure(texture: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1d: procedure(target: Cardinal; s: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1f: procedure(target: Cardinal; s: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1i: procedure(target: Cardinal; s: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1s: procedure(target: Cardinal; s: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2d: procedure(target: Cardinal; s: Double; t: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2f: procedure(target: Cardinal; s: Single; t: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2i: procedure(target: Cardinal; s: Integer; t: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2s: procedure(target: Cardinal; s: SmallInt; t: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3d: procedure(target: Cardinal; s: Double; t: Double; r: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3f: procedure(target: Cardinal; s: Single; t: Single; r: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4d: procedure(target: Cardinal; s: Double; t: Double; r: Double; q: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4f: procedure(target: Cardinal; s: Single; t: Single; r: Single; q: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer; q: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt; q: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glLoadTransposeMatrixf: procedure(const m: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glLoadTransposeMatrixd: procedure(const m: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultTransposeMatrixf: procedure(const m: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultTransposeMatrixd: procedure(const m: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glSampleCoverage: procedure(value: Single; invert: Boolean); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; depth: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage2D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage1D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage2D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; width: Integer; height: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage1D: procedure(target: Cardinal; level: Integer; xoffset: Integer; width: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetCompressedTexImage: procedure(target: Cardinal; level: Integer; img: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_4
Var
  glBlendFuncSeparate: procedure(sfactorRGB: Cardinal; dfactorRGB: Cardinal; sfactorAlpha: Cardinal; dfactorAlpha: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFogCoordf: procedure(coord: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFogCoordfv: procedure(const coord: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFogCoordd: procedure(coord: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFogCoorddv: procedure(const coord: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFogCoordPointer: procedure(_type: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiDrawArrays: procedure(mode: Cardinal; first: PInteger; count: PInteger; primcount: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMultiDrawElements: procedure(mode: Cardinal; const count: PInteger; _type: Cardinal; const indices: Pointer; primcount: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glPointParameterf: procedure(pname: Cardinal; param: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glPointParameterfv: procedure(pname: Cardinal; const params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glPointParameteri: procedure(pname: Cardinal; param: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glPointParameteriv: procedure(pname: Cardinal; const params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_5
Var
  glGenQueries: procedure(n: Integer; ids: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteQueries: procedure(n: Integer; const ids: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glIsQuery: function(id: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBeginQuery: procedure(target: Cardinal; id: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glEndQuery: procedure(target: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetQueryiv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetQueryObjectiv: procedure(id: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetQueryObjectuiv: procedure(id: Cardinal; pname: Cardinal; params: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBindBuffer: procedure(target: Cardinal; buffer: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteBuffers: procedure(n: Integer; const buffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGenBuffers: procedure(n: Integer; buffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glIsBuffer: function(buffer: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBufferData: procedure(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; const data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; data: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glMapBuffer: function(target: Cardinal; access: Cardinal): Pointer; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUnmapBuffer: function(target: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetBufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetBufferPointerv: procedure(target: Cardinal; pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

//  GL_version_2_0
Var
  glBlendEquationSeparate: procedure(modeRGB: Cardinal; modeAlpha: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDrawBuffers: procedure(n: Integer; const bufs: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glStencilOpSeparate: procedure(face: Cardinal; sfail: Cardinal; dpfail: Cardinal; dppass: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glStencilFuncSeparate: procedure(frontfunc: Cardinal; backfunc: Cardinal; ref: Integer; mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glStencilMaskSeparate: procedure(face: Cardinal; mask: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glAttachShader: procedure(_program: Cardinal; shader: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBindAttribLocation: procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCompileShader: procedure(shader: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCreateProgram: function(): Cardinal; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCreateShader: function(_type: Cardinal): Cardinal; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteProgram: procedure(_program: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteShader: procedure(shader: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDetachShader: procedure(_program: Cardinal; shader: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDisableVertexAttribArray: procedure(index: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glEnableVertexAttribArray: procedure(index: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetActiveAttrib: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetActiveUniform: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetAttachedShaders: procedure(_program: Cardinal; maxCount: Integer; count: PInteger; obj: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetAttribLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetProgramiv: procedure(_program: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetShaderiv: procedure(shader: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetShaderSource: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; source: PAnsiChar); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetUniformLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetUniformfv: procedure(_program: Cardinal; location: Integer; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetUniformiv: procedure(_program: Cardinal; location: Integer; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribdv: procedure(index: Cardinal; pname: Cardinal; params: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribfv: procedure(index: Cardinal; pname: Cardinal; params: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribiv: procedure(index: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribPointerv: procedure(index: Cardinal; pname: Cardinal; pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glIsProgram: function(_program: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glIsShader: function(shader: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glLinkProgram: procedure(_program: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glShaderSource: procedure(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUseProgram: procedure(_program: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform1f: procedure(location: Integer; v0: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform2f: procedure(location: Integer; v0: Single; v1: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform3f: procedure(location: Integer; v0: Single; v1: Single; v2: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform4f: procedure(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform1i: procedure(location: Integer; v0: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform2i: procedure(location: Integer; v0: Integer; v1: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform3i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform4i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform1fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform2fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform3fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform4fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform1iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform2iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform3iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniform4iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix2fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix3fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix4fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glValidateProgram: procedure(_program: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1d: procedure(index: Cardinal; x: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1f: procedure(index: Cardinal; x: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1s: procedure(index: Cardinal; x: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2d: procedure(index: Cardinal; x: Double; y: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2f: procedure(index: Cardinal; x: Single; y: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2s: procedure(index: Cardinal; x: SmallInt; y: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3d: procedure(index: Cardinal; x: Double; y: Double; z: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3f: procedure(index: Cardinal; x: Single; y: Single; z: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nbv: procedure(index: Cardinal; const v: PShortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Niv: procedure(index: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nsv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nub: procedure(index: Cardinal; x: Byte; y: Byte; z: Byte; w: Byte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nubv: procedure(index: Cardinal; const v: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nuiv: procedure(index: Cardinal; const v: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nusv: procedure(index: Cardinal; const v: PWord); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4bv: procedure(index: Cardinal; const v: PShortint); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4d: procedure(index: Cardinal; x: Double; y: Double; z: Double; w: Double); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4f: procedure(index: Cardinal; x: Single; y: Single; z: Single; w: Single); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4iv: procedure(index: Cardinal; const v: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt; w: SmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4ubv: procedure(index: Cardinal; const v: PByte); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4uiv: procedure(index: Cardinal; const v: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4usv: procedure(index: Cardinal; const v: PWord); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glVertexAttribPointer: procedure(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

Var
  glIsRenderbuffer: function(renderbuffer: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBindRenderbuffer: procedure(target: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteRenderbuffers: procedure(n: Integer; const renderbuffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGenRenderbuffers: procedure(n: Integer; renderbuffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glRenderbufferStorage: procedure(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetRenderbufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glIsFramebuffer: function(framebuffer: Cardinal): Boolean; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBindFramebuffer: procedure(target: Cardinal; framebuffer: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDeleteFramebuffers: procedure(n: Integer; const framebuffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGenFramebuffers: procedure(n: Integer; framebuffers: PCardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glCheckFramebufferStatus: function(target: Cardinal): Cardinal; {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture1D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture2D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture3D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer; zoffset: Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glFramebufferRenderbuffer: procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGetFramebufferAttachmentParameteriv: procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glGenerateMipmap: procedure(target: Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

  glRenderbufferStorageMultisample: procedure (target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glBlitFramebuffer: procedure (srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}


  glEnableIndexed: procedure(target, index:Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}
  glDisableIndexed: procedure(target, index:Integer); {$IFDEF MSWINDOWS}cdecl;{$ELSE}cdecl;{$ENDIF}

Var
  glActiveStencilFaceEXT: Procedure(face: Integer); cdecl = nil;

Procedure glLoadExtensions;

Function glExtensionSupported(Extension:AnsiString):Boolean;

Function glGetExtensionString():AnsiString;

Implementation
Uses TERRA_Error;

Var
  glExtGetProcAddress:function(proc:PAnsiChar):Pointer;
  OpenGLHandle:TLibHandle;
  ExtensionsList:AnsiString='';

Function glGetExtensionString():AnsiString;
Begin
  Result := ExtensionsList;
End;

Function alxGetProcAddress(proc:PAnsiChar):Pointer;
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

  glExtGetProcAddress := alxGetProcAddress;

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

  If (glExtensionSupported('GL_framebuffer_object'))
Or (glExtensionSupported('GL_EXT_framebuffer_object'))    Then
  Begin
    glIsRenderbuffer := glGetProcAddress('glIsRenderbuffer');
    glBindRenderbuffer := glGetProcAddress('glBindRenderbuffer');
    glDeleteRenderbuffers := glGetProcAddress('glDeleteRenderbuffers');
    glGenRenderbuffers := glGetProcAddress('glGenRenderbuffers');
    glRenderbufferStorage := glGetProcAddress('glRenderbufferStorage');
    glGetRenderbufferParameteriv := glGetProcAddress('glGetRenderbufferParameteriv');
    glIsFramebuffer := glGetProcAddress('glIsFramebuffer');
    glBindFramebuffer := glGetProcAddress('glBindFramebuffer');
    glDeleteFramebuffers := glGetProcAddress('glDeleteFramebuffers');
    glGenFramebuffers := glGetProcAddress('glGenFramebuffers');
    glCheckFramebufferStatus := glGetProcAddress('glCheckFramebufferStatus');
    glFramebufferTexture1D := glGetProcAddress('glFramebufferTexture1D');
    glFramebufferTexture2D := glGetProcAddress('glFramebufferTexture2D');
    glFramebufferTexture3D := glGetProcAddress('glFramebufferTexture3D');
    glFramebufferRenderbuffer := glGetProcAddress('glFramebufferRenderbuffer');
    glGetFramebufferAttachmentParameteriv := glGetProcAddress('glGetFramebufferAttachmentParameteriv');
    glGenerateMipmap := glGetProcAddress('glGenerateMipmap');
    glRenderbufferStorageMultisample := glGetProcAddress('glRenderbufferStorageMultisample');
    glBlitFramebuffer := glGetProcAddress('glBlitFramebuffer');;
  End;

  If glExtensionSupported('GL_stencil_two_side') Then
  Begin
    glActiveStencilFaceEXT := glGetProcAddress('glActiveStencilFaceEXT');
  End;

  If glExtensionSupported('GL_draw_buffers2') Then
  Begin
    glEnableIndexed := glGetProcAddress('glEnableIndexedEXT');
    glDisableIndexed := glGetProcAddress('glDisableIndexedEXT');
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
  Begin
    ExtensionsList := PAnsiChar(glGetString(GL_EXTENSIONS));
  End;

  Result := Pos(Extension,ExtensionsList)>0;
End;

Initialization
  Set8087CW($133F);
  LoadOpenGL(OpenGLLibName);
Finalization
  FreeOpenGL;
End.

