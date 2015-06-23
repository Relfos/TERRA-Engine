{
@abstract(OpenGL)
@author(Sergio Flores <relfos@gmail.com>)
@created(February 25, 2006)
@lastmod(March 1, 2006)
The GL unit provides a cross-plataform OpenGL interface.
Supports versions upto OpenGL 2.0.
Only extensions used by LEAF are included.
}

Unit TERRA_OpenGLES;
{$I terra.inc}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Log, Math,
{$IFDEF FPC}DynLibs {$ELSE} Windows{$ENDIF};

Const
  {$IFDEF WINDOWS} OpenGLLibName = 'libGLESv2.dll'; {$ENDIF}
  {$IFDEF ANDROID} OpenGLLibName = 'libGLESv2.so'; {$ENDIF}
  {$IFDEF IOS} OpenGLLibName = '/System/Library/Frameworks/OpenGLES.framework/OpenGLES'; {$ENDIF}

// OpenGL ES core versions
  GL_ES_VERSION_2_0 = 1;

// ClearBufferMask
  GL_DEPTH_BUFFER_BIT  = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;

// BeginMode
  GL_POINTS           = $0000;
  GL_LINES            = $0001;
  GL_LINE_LOOP        = $0002;
  GL_LINE_STRIP       = $0003;
  GL_TRIANGLES        = $0004;
  GL_TRIANGLE_STRIP   = $0005;
  GL_TRIANGLE_FAN     = $0006;

  GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
  GL_POINT_SMOOTH_HINT = $0C51;
  GL_LINE_SMOOTH_HINT = $0C52;
  GL_FOG_HINT = $0C54;
  GL_GENERATE_MIPMAP_HINT = $8192;

// AlphaFunction
//(not supported in ES20)

// BlendingFactorDest
  GL_ZERO                         =  0;
  GL_ONE                          =  1;
  GL_SRC_COLOR                    =  $0300;
  GL_ONE_MINUS_SRC_COLOR          =  $0301;
  GL_SRC_ALPHA                    =  $0302;
  GL_ONE_MINUS_SRC_ALPHA          =  $0303;
  GL_DST_ALPHA                    =  $0304;
  GL_ONE_MINUS_DST_ALPHA          =  $0305;

// BlendingFactorSrc
//      GL_ZERO
//      GL_ONE
  GL_DST_COLOR                    =  $0306;
  GL_ONE_MINUS_DST_COLOR          =  $0307;
  GL_SRC_ALPHA_SATURATE           =  $0308;
//      GL_SRC_ALPHA
//      GL_ONE_MINUS_SRC_ALPHA
//      GL_DST_ALPHA
//      GL_ONE_MINUS_DST_ALPHA

// BlendEquationSeparate
  GL_FUNC_ADD                     =  $8006;
  GL_BLEND_EQUATION               =  $8009;
  GL_BLEND_EQUATION_RGB           =  $8009;    // same as BLEND_EQUATION
  GL_BLEND_EQUATION_ALPHA         =  $883D;

// BlendSubtract
  GL_FUNC_SUBTRACT                =  $800A;
  GL_FUNC_REVERSE_SUBTRACT        =  $800B;

// Separate Blend Functions
  GL_BLEND_DST_RGB                =  $80C8;
  GL_BLEND_SRC_RGB                =  $80C9;
  GL_BLEND_DST_ALPHA              =  $80CA;
  GL_BLEND_SRC_ALPHA              =  $80CB;
  GL_CONSTANT_COLOR               =  $8001;
  GL_ONE_MINUS_CONSTANT_COLOR     =  $8002;
  GL_CONSTANT_ALPHA               =  $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA     =  $8004;
  GL_BLEND_COLOR                  =  $8005;

// Buffer Objects
  GL_ARRAY_BUFFER                  = $8892;
  GL_ELEMENT_ARRAY_BUFFER          = $8893;
  GL_ARRAY_BUFFER_BINDING          = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING  = $8895;

  GL_STREAM_DRAW                   = $88E0;
  GL_STATIC_DRAW                   = $88E4;
  GL_DYNAMIC_DRAW                  = $88E8;

  GL_BUFFER_SIZE                   = $8764;
  GL_BUFFER_USAGE                  = $8765;

  GL_CURRENT_VERTEX_ATTRIB         = $8626;

// CullFaceMode
  GL_FRONT                         = $0404;
  GL_BACK                          = $0405;
  GL_FRONT_AND_BACK                = $0408;

// DepthFunction
{      GL_NEVER */
/*      GL_LESS */
/*      GL_EQUAL */
/*      GL_LEQUAL */
/*      GL_GREATER */
/*      GL_NOTEQUAL */
/*      GL_GEQUAL */
/*      GL_ALWAYS */
}

// EnableCap
  GL_TEXTURE_2D                    = $0DE1;
  GL_CULL_FACE                     = $0B44;
  GL_BLEND                         = $0BE2;
  GL_DITHER                        = $0BD0;
  GL_STENCIL_TEST                  = $0B90;
  GL_DEPTH_TEST                    = $0B71;
  GL_SCISSOR_TEST                  = $0C11;
  GL_POLYGON_OFFSET_FILL           = $8037;
  GL_SAMPLE_ALPHA_TO_COVERAGE      = $809E;
  GL_SAMPLE_COVERAGE               = $80A0;

// ErrorCode
  GL_NO_ERROR                      = 0;
  GL_INVALID_ENUM                  = $0500;
  GL_INVALID_VALUE                 = $0501;
  GL_INVALID_OPERATION             = $0502;
  GL_OUT_OF_MEMORY                 = $0505;

// FrontFaceDirection
  GL_CW                            = $0900;
  GL_CCW                           = $0901;

// GetPName
  GL_LINE_WIDTH                     = $0B21;
  GL_ALIASED_POINT_SIZE_RANGE       = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE       = $846E;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_STENCIL_BACK_FUNC              = $8800;
  GL_STENCIL_BACK_FAIL              = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL   = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS   = $8803;
  GL_STENCIL_BACK_REF               = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK        = $8CA4;
  GL_STENCIL_BACK_WRITEMASK         = $8CA5;
  GL_VIEWPORT                       = $0BA2;
  GL_SCISSOR_BOX                    = $0C10;
//      GL_SCISSOR_TEST
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
//      GL_POLYGON_OFFSET_FILL
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_TEXTURE_BINDING_2D             = $8069;
  GL_SAMPLE_BUFFERS                 = $80A8;
  GL_SAMPLES                        = $80A9;
  GL_SAMPLE_COVERAGE_VALUE          = $80AA;
  GL_SAMPLE_COVERAGE_INVERT         = $80AB;

// GetTextureParameter
{      GL_TEXTURE_MAG_FILTER */
/*      GL_TEXTURE_MIN_FILTER */
/*      GL_TEXTURE_WRAP_S */
/*      GL_TEXTURE_WRAP_T }

  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS     = $86A3;

// HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;


// DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_FIXED                          = $140C;

// PixelFormat
  GL_DEPTH_COMPONENT                = $1902;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;
  GL_RGBA8 = GL_RGBA;

// PixelType
//     GL_UNSIGNED_BYTE
  GL_UNSIGNED_SHORT_4_4_4_4         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1         = $8034;
  GL_UNSIGNED_SHORT_5_6_5           = $8363;

// Shaders
 GL_FRAGMENT_SHADER                  = $8B30;
 GL_VERTEX_SHADER                    = $8B31;
 GL_MAX_VERTEX_ATTRIBS               = $8869;
 GL_MAX_VERTEX_UNIFORM_VECTORS       = $8DFB;
 GL_MAX_VARYING_VECTORS              = $8DFC;
 GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
 GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS   = $8B4C;
 GL_MAX_TEXTURE_IMAGE_UNITS          = $8872;
 GL_MAX_FRAGMENT_UNIFORM_VECTORS     = $8DFD;
 GL_SHADER_TYPE                      = $8B4F;
 GL_DELETE_STATUS                    = $8B80;
 GL_LINK_STATUS                      = $8B82;
 GL_VALIDATE_STATUS                  = $8B83;
 GL_ATTACHED_SHADERS                 = $8B85;
 GL_ACTIVE_UNIFORMS                  = $8B86;
 GL_ACTIVE_UNIFORM_MAX_LENGTH        = $8B87;
 GL_ACTIVE_ATTRIBUTES                = $8B89;
 GL_ACTIVE_ATTRIBUTE_MAX_LENGTH      = $8B8A;
 GL_SHADING_LANGUAGE_VERSION         = $8B8C;
 GL_CURRENT_PROGRAM                  = $8B8D;

// StencilFunction
 GL_NEVER                          = $0200;
 GL_LESS                           = $0201;
 GL_EQUAL                          = $0202;
 GL_LEQUAL                         = $0203;
 GL_GREATER                        = $0204;
 GL_NOTEQUAL                       = $0205;
 GL_GEQUAL                         = $0206;
 GL_ALWAYS                         = $0207;

// StencilOp
//      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  GL_INVERT                         = $150A;
  GL_INCR_WRAP                      = $8507;
  GL_DECR_WRAP                      = $8508;

// StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

// TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

  GL_DEPTH24_STENCIL8 = $88F0;

// TextureMinFilter
//      GL_NEAREST
//      GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;

// TextureParameterName
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;

// TextureTarget
//      GL_TEXTURE_2D
  GL_TEXTURE                        = $1702;

  GL_TEXTURE_CUBE_MAP               = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP       = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X    = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X    = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y    = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z    = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    = $851A;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE      = $851C;

// TextureUnit
  GL_TEXTURE0                       = $84C0;
  GL_TEXTURE1                       = $84C1;
  GL_TEXTURE2                       = $84C2;
  GL_TEXTURE3                       = $84C3;
  GL_TEXTURE4                       = $84C4;
  GL_TEXTURE5                       = $84C5;
  GL_TEXTURE6                       = $84C6;
  GL_TEXTURE7                       = $84C7;
  GL_TEXTURE8                       = $84C8;
  GL_TEXTURE9                       = $84C9;
  GL_TEXTURE10                      = $84CA;
  GL_TEXTURE11                      = $84CB;
  GL_TEXTURE12                      = $84CC;
  GL_TEXTURE13                      = $84CD;
  GL_TEXTURE14                      = $84CE;
  GL_TEXTURE15                      = $84CF;
  GL_TEXTURE16                      = $84D0;
  GL_TEXTURE17                      = $84D1;
  GL_TEXTURE18                      = $84D2;
  GL_TEXTURE19                      = $84D3;
  GL_TEXTURE20                      = $84D4;
  GL_TEXTURE21                      = $84D5;
  GL_TEXTURE22                      = $84D6;
  GL_TEXTURE23                      = $84D7;
  GL_TEXTURE24                      = $84D8;
  GL_TEXTURE25                      = $84D9;
  GL_TEXTURE26                      = $84DA;
  GL_TEXTURE27                      = $84DB;
  GL_TEXTURE28                      = $84DC;
  GL_TEXTURE29                      = $84DD;
  GL_TEXTURE30                      = $84DE;
  GL_TEXTURE31                      = $84DF;
  GL_ACTIVE_TEXTURE                 = $84E0;

// TextureWrapMode
  GL_REPEAT                         = $2901;
  GL_CLAMP_TO_EDGE                  = $812F;
  GL_MIRRORED_REPEAT                = $8370;

// Uniform Types
 GL_FLOAT_VEC2                     = $8B50;
 GL_FLOAT_VEC3                     = $8B51;
 GL_FLOAT_VEC4                     = $8B52;
 GL_INT_VEC2                       = $8B53;
 GL_INT_VEC3                       = $8B54;
 GL_INT_VEC4                       = $8B55;
 GL_BOOL                           = $8B56;
 GL_BOOL_VEC2                      = $8B57;
 GL_BOOL_VEC3                      = $8B58;
 GL_BOOL_VEC4                      = $8B59;
 GL_FLOAT_MAT2                     = $8B5A;
 GL_FLOAT_MAT3                     = $8B5B;
 GL_FLOAT_MAT4                     = $8B5C;
 GL_SAMPLER_2D                     = $8B5E;
 GL_SAMPLER_CUBE                   = $8B60;

// Vertex Arrays
  GL_VERTEX_ATTRIB_ARRAY_ENABLED        = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE           = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE         = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE           = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED     = $886A;
  GL_VERTEX_ATTRIB_ARRAY_POINTER        = $8645;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;

// Read Format
  GL_IMPLEMENTATION_COLOR_READ_TYPE   = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;

// Shader Source
  GL_COMPILE_STATUS                 = $8B81;
  GL_INFO_LOG_LENGTH                = $8B84;
  GL_SHADER_SOURCE_LENGTH           = $8B88;
  GL_SHADER_COMPILER                = $8DFA;

// Shader Binary
  GL_SHADER_BINARY_FORMATS          = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS      = $8DF9;

// Shader Precision-Specified Types
  GL_LOW_FLOAT                      = $8DF0;
  GL_MEDIUM_FLOAT                   = $8DF1;
  GL_HIGH_FLOAT                     = $8DF2;
  GL_LOW_INT                        = $8DF3;
  GL_MEDIUM_INT                     = $8DF4;
  GL_HIGH_INT                       = $8DF5;

// Framebuffer Object.
  GL_FRAMEBUFFER                    = $8D40;
  GL_RENDERBUFFER                   = $8D41;

  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGB565                         = $8D62;
  GL_DEPTH_COMPONENT16              = $81A5;
  GL_STENCIL_INDEX                  = $1901;
  GL_STENCIL_INDEX8                 = $8D48;

  GL_RENDERBUFFER_WIDTH             = $8D42;
  GL_RENDERBUFFER_HEIGHT            = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT   = $8D44;
  GL_RENDERBUFFER_RED_SIZE          = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE        = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE         = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE        = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE        = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE      = $8D55;

  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE           = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME           = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL         = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;

  GL_COLOR_ATTACHMENT0              = $8CE0;
  GL_DEPTH_ATTACHMENT               = $8D00;
  GL_STENCIL_ATTACHMENT             = $8D20;

  GL_NONE                           = 0;

  GL_FRAMEBUFFER_COMPLETE                      = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = $8CD9;
  GL_FRAMEBUFFER_UNSUPPORTED                   = $8CDD;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_STATUS_ERROR = $8CDE;

  GL_FRAMEBUFFER_BINDING            = $8CA6;
  GL_RENDERBUFFER_BINDING           = $8CA7;
  GL_MAX_RENDERBUFFER_SIZE          = $84E8;

  GL_INVALID_FRAMEBUFFER_OPERATION  = $0506;

  //Extensions
  GL_DEPTH_COMPONENT24_OES          = $81A6;
  GL_DEPTH_COMPONENT32_OES          = $81A7;

  // GL_OES_packed_depth_stencil
  GL_DEPTH_STENCIL_OES              = $84F9;
  GL_UNSIGNED_INT_24_8_OES          = $84FA;
  GL_DEPTH24_STENCIL8_OES           = $88F0;

	GL_STENCIL_INDEX8_OES 			= $8D48;

  // GL_OES_rgb8_rgba8
  GL_RGB8_OES                        = $8051;
  GL_RGBA8_OES                       = $8058;

// APPLE extension tokens

// GL_APPLE_framebuffer_multisample
  GL_RENDERBUFFER_SAMPLES_APPLE                          = $8CAB;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_APPLE            = $8D56;
  GL_MAX_SAMPLES_APPLE                                   = $8D57;
  GL_READ_FRAMEBUFFER_APPLE                              = $8CA8;
  GL_DRAW_FRAMEBUFFER_APPLE                              = $8CA9;
  GL_DRAW_FRAMEBUFFER_BINDING_APPLE                      = $8CA6;
  GL_READ_FRAMEBUFFER_BINDING_APPLE                      = $8CAA;

// GL_APPLE_texture_format_BGRA8888
  GL_BGRA_EXT                                            = $80E1;
  GL_BGRA = GL_BGRA_EXT;

// GL core functions.
Var
  glActiveTexture:Procedure (texture:Integer); cdecl;
  glAttachShader:Procedure (programname, shader:Cardinal); cdecl;
  glBindAttribLocation:Procedure (_program:Integer; index:Integer; name:PAnsiChar); cdecl;
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
  glGetActiveAttrib:Procedure (_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl;
  glGetActiveUniform:Procedure (_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl;
  glGetAttachedShaders:Procedure (_program, maxcount:Cardinal; Var count:Integer; shaders:PCardinal); cdecl;
  glGetAttribLocation:Function (_program:Cardinal; name:PAnsiChar):Integer; cdecl;
  glGetBooleanv:Procedure (pname:Cardinal; params:PBoolean); cdecl;
  glGetBufferParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetError:Function ():Integer; cdecl;
  glGetFloatv:Procedure (pname:Cardinal; params:PSingle); cdecl;
  glGetFramebufferAttachmentParameteriv:Procedure (target, attachment, pname:Cardinal; params:PInteger); cdecl;
  glGetIntegerv :Procedure(pname:Cardinal; params:PInteger); cdecl;
  glGetProgramiv :Procedure(_program,pname:Cardinal; params:PInteger); cdecl;
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl;
  glGetRenderbufferParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetShaderiv:Procedure (shader, pname:Cardinal; params:PInteger); cdecl;
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl;
  glGetShaderPrecisionFormat:Procedure (shadertype, precisiontype:Cardinal; Var range, precision:Integer); cdecl;
  glGetShaderSource:Procedure (shader, bufsize:Cardinal; Var length:Integer; source:PAnsiChar); cdecl;
  glGetString:Function (name:Cardinal):PAnsiChar; cdecl;
  glGetTexParameterfv:Procedure (target, pname:Cardinal; params:PSingle); cdecl;
  glGetTexParameteriv:Procedure (target, pname:Cardinal; params:PInteger); cdecl;
  glGetUniformfv:Procedure (_program, location:Cardinal;  params:PSingle); cdecl;
  glGetUniformiv:Procedure (_program, location:Cardinal; params:PInteger); cdecl;
  glGetUniformLocation:Function (_program:Cardinal; name:PAnsiChar):Integer; cdecl;
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
  glShaderSource:Procedure (shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); cdecl;
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

  // Extensions
  glRenderbufferStorageMultisample:Procedure (target, samples, internalformat, width, height:Cardinal); cdecl;
  glResolveMultisampleFramebuffer:Procedure(); cdecl;

  glDiscardFramebuffer: Procedure(target, count:Integer; attachments:PInteger);

  // GL ES1
  {$IFDEF LEGACY_GLES}
  glAlphaFunc:Procedure (func: Cardinal; ref: Single); cdecl;
  glEnableClientState: Procedure(aarray: Cardinal); cdecl;
  glDisableClientState: Procedure(aarray: Cardinal); cdecl;
  glVertexPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glTexCoordPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glColorPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl;
  glColor4f:Procedure (red, green, blue, alpha: Single); cdecl;
  glMatrixMode:Procedure (mode: Cardinal); cdecl;
  glLoadMatrixf:Procedure (const m: PSingle); cdecl;
  {$ENDIF}

Procedure glClearDepth(depth:Single);

Function glExtensionSupported(Extension:TERRAString):Boolean;
Function glGetExtensionString():TERRAString;

Procedure LoadOpenGL();

Implementation
Uses TERRA_Error, TERRA_Application;

Type
  TLibHandle = Cardinal;

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
  Result := GetProcAddress(OpenGLHandle, PAnsiChar(Proc));
  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PAnsiChar(Proc+'OES'));

  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PAnsiChar(Proc+'APPLE'));

  If Not Assigned(Result) Then
    Result := GetProcAddress(OpenGLHandle, PAnsiChar(Proc+'EXT'));

  If Not Assigned(Result) Then
    Log(logWarning,'GL', 'Function '+Proc+' not avaliable.');
End;

Procedure LoadOpenGL();
Begin
	TERRA_Log.Log(logDebug, 'GL', 'loading openGL');
  Log(logDebug, 'OpenGL', 'Loading library');

  OpenGLHandle := LoadLibrary(PAnsiChar(OpenGLLibName));
  If OpenGLHandle=0 Then
  Begin
    RaiseError('Error loading OpenGL from '+OpenGLLibName);
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

  glViewport := glGetProcAddress('glViewport');

  {$IFDEF LEGACY_GLES}
  glAlphaFunc := glGetProcAddress('glAlphaFunc');
  glEnableClientState := glGetProcAddress('glEnableClientState');
  glDisableClientState := glGetProcAddress('glDisableClientState');
  glVertexPointer := glGetProcAddress('glVertexPointer');
  glTexCoordPointer := glGetProcAddress('glTexCoordPointer');
  glColorPointer := glGetProcAddress('glColorPointer');
  glColor4f := glGetProcAddress('glColor4f');

  glMatrixMode := glGetProcAddress('glMatrixMode');
  glLoadMatrixf := glGetProcAddress('glLoadMatrixf');
  {$ENDIF}
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
    ExtensionsList := PAnsiChar(glGetString(GL_EXTENSIONS));
  End;

  Result := Pos(Extension,ExtensionsList)>0;
End;

Initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
Finalization
  FreeOpenGL;
End.

