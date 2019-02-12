{
@abstract(OpenGL)
@author(Sergio Flores <>)
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

  {$IFDEF IOS}
  {$DEFINE STATIC_LINKING}
  {$LINKLIB GL}
  OpenGLLibName = '/System/Library/Frameworks/OpenGLES.framework/OpenGLES';
  {$ENDIF}


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
{$IFNDEF STATIC_LINKING}
Var
{$ENDIF}

  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glActiveTexture{$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(texture:Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glAttachShader{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(programname, shader:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBindAttribLocation{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program:Integer; index:Integer; name:PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBindBuffer{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, buffer:Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBindFramebuffer{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, framebuffer:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBindRenderbuffer{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, renderbuffer:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBindTexture{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, texture:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBlendColor{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(red, green, blue, alpha:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBlendEquation{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}( mode:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBlendEquationSeparate{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(modeRGB, modeAlpha:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBlendFunc{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(sfactor, dfactor:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBlendFuncSeparate{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(srcRGB, dstRGB, srcAlpha, dstAlpha:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBufferData {$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(target, size:Cardinal; data:Pointer; usage:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glBufferSubData{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, offset, size:Cardinal; data:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glCheckFramebufferStatus{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (target:Integer):Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glClear{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(mask:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glClearColor{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(red, green, blue, alpha:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glClearDepthf{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(depth:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glClearStencil{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(s:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glColorMask{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(red, green, blue, alpha:Boolean); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCompileShader{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shader:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCompressedTexImage2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, internalformat, width, height, border, imageSize:Cardinal; data:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCompressedTexSubImage2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, xoffset, yoffset, width, height, format, imageSize:Cardinal; data:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCopyTexImage2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, internalformat, x, y, width, height, border:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCopyTexSubImage2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, xoffset, yoffset, x, y, width,height:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glCreateProgram{$IFNDEF STATIC_LINKING}:Function{$ENDIF}():Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glCreateShader{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (shadertype:Integer):Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glCullFace{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(mode:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteBuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal;  buffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteFramebuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; framebuffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteProgram{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteRenderbuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; renderbuffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteShader{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shader:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDeleteTextures{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; textures:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDepthFunc{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(func:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDepthMask{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(flag:Boolean); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDepthRange{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(zNear, zFar:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDetachShader{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, shader:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDisable{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(cap:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDisableVertexAttribArray{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(index:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDrawArrays{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(mode, first, count:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glDrawElements{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(mode, count, _type:Cardinal; indices:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glEnable{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(cap:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glEnableVertexAttribArray{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(index:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glFinish{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glFlush{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glFramebufferRenderbuffer{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, attachment, renderbuffertarget, renderbuffer:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glFramebufferTexture2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, attachment, textarget, texture, level:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glFrontFace{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(mode:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGenBuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; buffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGenerateMipmap{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGenFramebuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; framebuffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGenRenderbuffers{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; renderbuffers:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGenTextures{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; textures:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetActiveAttrib{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetActiveUniform{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, index, bufsize:Cardinal; Var length, size, _type:Integer; name:PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetAttachedShaders{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, maxcount:Cardinal; Var count:Integer; shaders:PCardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glGetAttribLocation{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (_program:Cardinal; name:PAnsiChar):Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetBooleanv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(pname:Cardinal; params:PBoolean); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetBufferParameteriv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glGetError{$IFNDEF STATIC_LINKING}:Function{$ENDIF} ():Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetFloatv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(pname:Cardinal; params:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetFramebufferAttachmentParameteriv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, attachment, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetIntegerv {$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetProgramiv {$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(_program,pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetProgramInfoLog{$IFNDEF STATIC_LINKING}:procedure{$ENDIF}(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetRenderbufferParameteriv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetShaderiv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shader, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetShaderInfoLog{$IFNDEF STATIC_LINKING}: procedure{$ENDIF}(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetShaderPrecisionFormat{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shadertype, precisiontype:Cardinal; Var range, precision:Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF} glGetShaderSource{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shader, bufsize:Cardinal; Var length:Integer; source:PAnsiChar); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF} glGetString{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (name:Cardinal):PAnsiChar; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetTexParameterfv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetTexParameteriv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetUniformfv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, location:Cardinal;  params:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetUniformiv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program, location:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glGetUniformLocation{$IFNDEF STATIC_LINKING}:Function{$ENDIF}(_program:Cardinal; name:PAnsiChar):Integer; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetVertexAttribfv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(index, pname:Cardinal; params:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetVertexAttribiv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(index, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glGetVertexAttribPointerv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(index, pname:Cardinal; pointer:PPointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glHint{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, mode:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsBuffer{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (buffer:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsEnabled{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (cap:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsFramebuffer{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (framebuffer:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsProgram{$IFNDEF STATIC_LINKING}:Function{$ENDIF} (_program:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsRenderbuffer{$IFNDEF STATIC_LINKING}:Function {$ENDIF}(renderbuffer:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsShader{$IFNDEF STATIC_LINKING}:Function {$ENDIF} (shader:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Function{$ENDIF}glIsTexture{$IFNDEF STATIC_LINKING}:Function {$ENDIF}(texture:Cardinal):Boolean; cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glLineWidth{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(width:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glLinkProgram{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glPixelStorei{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(pname:Cardinal; param:Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glPolygonOffset{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(factor, units:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glReadPixels{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(x, y, width, height, format, _type:Cardinal; pixels:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glReleaseShaderCompiler {$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glRenderbufferStorage{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, internalformat, width, height:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glSampleCoverage{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(value:Single; invert:Boolean); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glScissor{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(x, y, width, height:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glShaderBinary{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(n:Cardinal; shaders:PCardinal; binaryformat:Cardinal; binary:Pointer; length:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glShaderSource{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilFunc{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(func, ref, mask:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilFuncSeparate{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(face, func, ref, mask:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilMask {$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(mask:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilMaskSeparate{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(face, mask:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilOp{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(fail, zfail, zpass:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glStencilOpSeparate{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(face, fail, zfail, zpass:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexImage2D {$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, internalformat, width, height, border, format, _type:Cardinal; pixels:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexParameterf{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; param:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexParameterfv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexParameteri{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; param:Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexParameteriv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, pname:Cardinal; params:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glTexSubImage2D{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, level, xoffset, yoffset, width, height, format, _type:Cardinal; pixels:Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}

  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform1f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform2f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Single; v1: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform3f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Single; v1: Single; v2: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform4f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform1i{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform2i{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Integer; v1: Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform3i{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Integer; v1: Integer; v2: Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform4i{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform1fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform2fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform3fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform4fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform1iv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform2iv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform3iv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniform4iv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; const value: PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniformMatrix2fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniformMatrix3fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUniformMatrix4fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}

  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glUseProgram{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glValidateProgram{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(_program:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib1f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; x:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib1fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; values:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib2f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; x, y:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib2fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; values:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib3f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; x, y, z:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}  glVertexAttrib3fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; values:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib4f{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; x, y, z, w:Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttrib4fv{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(indx:Cardinal; values:PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glVertexAttribPointer{$IFNDEF STATIC_LINKING}:procedure{$ENDIF}(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glViewport{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(x, y, width, height:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}

  // Extensions
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glRenderbufferStorageMultisample{$IFNDEF STATIC_LINKING}:Procedure {$ENDIF}(target, samples, internalformat, width, height:Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glResolveMultisampleFramebuffer{$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$IFDEF STATIC_LINKING}Procedure{$ENDIF}glDiscardFramebuffer{$IFNDEF STATIC_LINKING}:Procedure{$ENDIF}(target, count:Integer; attachments:PInteger); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}

  // GL ES1
  {$IFDEF LEGACY_GLES}
  glAlphaFunc:Procedure (func: Cardinal; ref: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glEnableClientState: Procedure(aarray: Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glDisableClientState: Procedure(aarray: Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glVertexPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glTexCoordPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glColorPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glColor4f:Procedure (red, green, blue, alpha: Single); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glMatrixMode:Procedure (mode: Cardinal); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  glLoadMatrixf:Procedure (const m: PSingle); cdecl; {$IFDEF STATIC_LINKING}External; {$ENDIF}
  {$ENDIF}

Procedure glClearDepth(depth:Single);

Function glExtensionSupported(Extension:TERRAString):Boolean;
Function glGetExtensionString():TERRAString;

Procedure LoadOpenGL();

Implementation
Uses TERRA_Error, TERRA_Application;

{$IFNDEF STATIC_LINKING}
Type
  TLibHandle = Cardinal;

Var
  OpenGLHandle:TLibHandle;
{$ENDIF}

Var
  ExtensionsList:TERRAString='';


Procedure glClearDepth(depth:Single);
Begin
  glClearDepthf(depth);
End;
  
Function glGetExtensionString():TERRAString;
Begin
  Result := ExtensionsList;
End;

{$IFNDEF STATIC_LINKING}
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
{$ENDIF}

Procedure LoadOpenGL();
Begin
{$IFNDEF STATIC_LINKING}
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

{$ENDIF}
End;

{$IFNDEF STATIC_LINKING}
Procedure FreeOpenGL;
Begin
  If OpenGLHandle<>0 Then
    FreeLibrary(OpenGLHandle);
End;
{$ENDIF}

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
{$IFNDEF STATIC_LINKING}
  FreeOpenGL;
{$ENDIF}
End.

