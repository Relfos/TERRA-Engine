{
@abstract(OpenGL)
@author(Sergio Flores <>)
@created(February 25, 2006)
@lastmod(March 1, 2006)
The GL unit provides a cross-plataform OpenGL interface.
Supports versions upto OpenGL 2.0.
Only extensions used by LEAF are included.
}

Unit TERRA_OpenGL;

{$I terra.inc}

{$R-}
Interface
Uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF FPC}Math,DynLibs,{$ENDIF}
  {$IFDEF LINUX}GLX,{$ENDIF}
  {$IFDEF OSX}TERRA_AGL,{$ENDIF}
  TERRA_String, TERRA_Log, TERRA_Matrix4x4, TERRA_Math;

Type
  TLibHandle = Cardinal;

Const
  {$IFDEF WINDOWS}OpenGLLibName = 'opengl32.dll';{$ENDIF}
  {$IFDEF LINUX}OpenGLLibName = 'libGL.so';{$ENDIF}
  {$IFDEF OSX}
  {$LINKLIB GL}
  OpenGLLibName='/System/Library/Frameworks/OpenGL.framework/Versions/Current/Libraries/libGL.dylib';
  {$ENDIF}


  // AlphaFunction
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // AttribMask
  GL_CURRENT_BIT                    = $00000001;
  GL_POINT_BIT                      = $00000002;
  GL_LINE_BIT                       = $00000004;
  GL_POLYGON_BIT                    = $00000008;
  GL_POLYGON_STIPPLE_BIT            = $00000010;
  GL_PIXEL_MODE_BIT                 = $00000020;
  GL_LIGHTING_BIT                   = $00000040;
  GL_FOG_BIT                        = $00000080;
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_ACCUM_BUFFER_BIT               = $00000200;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_VIEWPORT_BIT                   = $00000800;
  GL_TRANSFORM_BIT                  = $00001000;
  GL_ENABLE_BIT                     = $00002000;
  GL_COLOR_BUFFER_BIT               = $00004000;
  GL_HINT_BIT                       = $00008000;
  GL_EVAL_BIT                       = $00010000;
  GL_LIST_BIT                       = $00020000;
  GL_TEXTURE_BIT                    = $00040000;
  GL_SCISSOR_BIT                    = $00080000;
  GL_ALL_ATTRIB_BITS                = $000FFFFF;

  // BeginMode
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;
  GL_QUAD_STRIP                     = $0008;
  GL_POLYGON                        = $0009;

  // BlendingFactorDest
  GL_ZERO                           = 0;
  GL_ONE                            = 1;
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;

  // BlendingFactorSrc
  //      GL_ZERO
  //      GL_ONE
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;
  //      GL_SRC_ALPHA
  //      GL_ONE_MINUS_SRC_ALPHA
  //      GL_DST_ALPHA
  //      GL_ONE_MINUS_DST_ALPHA

  // ClientArrayType
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY

  GL_FUNC_ADD = $8006;
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;

  // ClipPlaneName
  GL_CLIP_PLANE0                    = $3000;
  GL_CLIP_PLANE1                    = $3001;
  GL_CLIP_PLANE2                    = $3002;
  GL_CLIP_PLANE3                    = $3003;
  GL_CLIP_PLANE4                    = $3004;
  GL_CLIP_PLANE5                    = $3005;

  // ColorMaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // ColorMaterialParameter
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR
  //      GL_EMISSION
  //      GL_AMBIENT_AND_DIFFUSE

  // ColorPointerType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // CullFaceMode
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_2_BYTES                        = $1407;
  GL_3_BYTES                        = $1408;
  GL_4_BYTES                        = $1409;
  GL_DOUBLE                         = $140A;

  // DepthFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // DrawBufferMode
  GL_NONE                           = 0;
  GL_FRONT_LEFT                     = $0400;
  GL_FRONT_RIGHT                    = $0401;
  GL_BACK_LEFT                      = $0402;
  GL_BACK_RIGHT                     = $0403;
  GL_FRONT                          = $0404;
  GL_BACK                           = $0405;
  GL_LEFT                           = $0406;
  GL_RIGHT                          = $0407;
  GL_FRONT_AND_BACK                 = $0408;
  GL_AUX0                           = $0409;
  GL_AUX1                           = $040A;
  GL_AUX2                           = $040B;
  GL_AUX3                           = $040C;

  // Enable
  //      GL_FOG
  //      GL_LIGHTING
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_LINE_STIPPLE
  //      GL_POLYGON_STIPPLE
  //      GL_CULL_FACE
  //      GL_ALPHA_TEST
  //      GL_BLEND
  //      GL_INDEX_LOGIC_OP
  //      GL_COLOR_LOGIC_OP
  //      GL_DITHER
  //      GL_STENCIL_TEST
  //      GL_DEPTH_TEST
  //      GL_CLIP_PLANE0
  //      GL_CLIP_PLANE1
  //      GL_CLIP_PLANE2
  //      GL_CLIP_PLANE3
  //      GL_CLIP_PLANE4
  //      GL_CLIP_PLANE5
  //      GL_LIGHT0
  //      GL_LIGHT1
  //      GL_LIGHT2
  //      GL_LIGHT3
  //      GL_LIGHT4
  //      GL_LIGHT5
  //      GL_LIGHT6
  //      GL_LIGHT7
  //      GL_TEXTURE_GEN_S
  //      GL_TEXTURE_GEN_T
  //      GL_TEXTURE_GEN_R
  //      GL_TEXTURE_GEN_Q
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_POINT_SMOOTH
  //      GL_LINE_SMOOTH
  //      GL_POLYGON_SMOOTH
  //      GL_SCISSOR_TEST
  //      GL_COLOR_MATERIAL
  //      GL_NORMALIZE
  //      GL_AUTO_NORMAL
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_POLYGON_OFFSET_POINT
  //      GL_POLYGON_OFFSET_LINE
  //      GL_POLYGON_OFFSET_FILL

  // ErrorCode
  GL_NO_ERROR                       = 0;
  GL_INVALID_ENUM                   = $0500;
  GL_INVALID_VALUE                  = $0501;
  GL_INVALID_OPERATION              = $0502;
  GL_STACK_OVERFLOW                 = $0503;
  GL_STACK_UNDERFLOW                = $0504;
  GL_OUT_OF_MEMORY                  = $0505;

  // FogMode
  //      GL_LINEAR
  GL_EXP                            = $0800;
  GL_EXP2                           = $0801;

  // HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // FogParameter
  //      GL_FOG_COLOR
  //      GL_FOG_DENSITY
  //      GL_FOG_END
  //      GL_FOG_INDEX
  //      GL_FOG_MODE
  //      GL_FOG_START

  // GetTarget
  GL_CURRENT_COLOR                  = $0B00;
  GL_CURRENT_INDEX                  = $0B01;
  GL_CURRENT_NORMAL                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS         = $0B03;
  GL_CURRENT_RASTER_COLOR           = $0B04;
  GL_CURRENT_RASTER_INDEX           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS  = $0B06;
  GL_CURRENT_RASTER_POSITION        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID  = $0B08;
  GL_CURRENT_RASTER_DISTANCE        = $0B09;
  GL_POINT_SMOOTH                   = $0B10;
  GL_POINT_SIZE                     = $0B11;
  GL_POINT_SIZE_RANGE               = $0B12;
  GL_POINT_SIZE_GRANULARITY         = $0B13;
  GL_LINE_SMOOTH                    = $0B20;
  GL_LINE_WIDTH                     = $0B21;
  GL_LINE_WIDTH_RANGE               = $0B22;
  GL_LINE_WIDTH_GRANULARITY         = $0B23;
  GL_LINE_STIPPLE                   = $0B24;
  GL_LINE_STIPPLE_PATTERN           = $0B25;
  GL_LINE_STIPPLE_REPEAT            = $0B26;
  GL_LIST_MODE                      = $0B30;
  GL_MAX_LIST_NESTING               = $0B31;
  GL_LIST_BASE                      = $0B32;
  GL_LIST_INDEX                     = $0B33;
  GL_POLYGON_MODE                   = $0B40;
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_POLYGON_STIPPLE                = $0B42;
  GL_EDGE_FLAG                      = $0B43;
  GL_CULL_FACE                      = $0B44;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_LIGHTING                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE           = $0B52;
  GL_LIGHT_MODEL_AMBIENT            = $0B53;
  GL_SHADE_MODEL                    = $0B54;
  GL_COLOR_MATERIAL_FACE            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER       = $0B56;
  GL_COLOR_MATERIAL                 = $0B57;
  GL_FOG                            = $0B60;
  GL_FOG_INDEX                      = $0B61;
  GL_FOG_DENSITY                    = $0B62;
  GL_FOG_START                      = $0B63;
  GL_FOG_END                        = $0B64;
  GL_FOG_MODE                       = $0B65;
  GL_FOG_COLOR                      = $0B66;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_TEST                     = $0B71;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_ACCUM_CLEAR_VALUE              = $0B80;
  GL_STENCIL_TEST                   = $0B90;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_MATRIX_MODE                    = $0BA0;
  GL_NORMALIZE                      = $0BA1;
  GL_VIEWPORT                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH          = $0BA3;
  GL_PROJECTION_STACK_DEPTH         = $0BA4;
  GL_TEXTURE_STACK_DEPTH            = $0BA5;
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;
  GL_TEXTURE_MATRIX                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH      = $0BB1;
  GL_ALPHA_TEST                     = $0BC0;
  GL_ALPHA_TEST_FUNC                = $0BC1;
  GL_ALPHA_TEST_REF                 = $0BC2;
  GL_DITHER                         = $0BD0;
  GL_BLEND_DST                      = $0BE0;
  GL_BLEND_SRC                      = $0BE1;
  GL_BLEND                          = $0BE2;
  GL_LOGIC_OP_MODE                  = $0BF0;
  GL_INDEX_LOGIC_OP                 = $0BF1;
  GL_COLOR_LOGIC_OP                 = $0BF2;
  GL_AUX_BUFFERS                    = $0C00;
  GL_DRAW_BUFFER                    = $0C01;
  GL_READ_BUFFER                    = $0C02;
  GL_SCISSOR_BOX                    = $0C10;
  GL_SCISSOR_TEST                   = $0C11;
  GL_INDEX_CLEAR_VALUE              = $0C20;
  GL_INDEX_WRITEMASK                = $0C21;
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_INDEX_MODE                     = $0C30;
  GL_RGBA_MODE                      = $0C31;
  GL_DOUBLEBUFFER                   = $0C32;
  GL_STEREO                         = $0C33;
  GL_RENDER_MODE                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_POINT_SMOOTH_HINT              = $0C51;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
  GL_FOG_HINT                       = $0C54;
  GL_TEXTURE_GEN_S                  = $0C60;
  GL_TEXTURE_GEN_T                  = $0C61;
  GL_TEXTURE_GEN_R                  = $0C62;
  GL_TEXTURE_GEN_Q                  = $0C63;
  GL_PIXEL_MAP_I_TO_I               = $0C70;
  GL_PIXEL_MAP_S_TO_S               = $0C71;
  GL_PIXEL_MAP_I_TO_R               = $0C72;
  GL_PIXEL_MAP_I_TO_G               = $0C73;
  GL_PIXEL_MAP_I_TO_B               = $0C74;
  GL_PIXEL_MAP_I_TO_A               = $0C75;
  GL_PIXEL_MAP_R_TO_R               = $0C76;
  GL_PIXEL_MAP_G_TO_G               = $0C77;
  GL_PIXEL_MAP_B_TO_B               = $0C78;
  GL_PIXEL_MAP_A_TO_A               = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE          = $0CB9;
  GL_UNPACK_SWAP_BYTES              = $0CF0;
  GL_UNPACK_LSB_FIRST               = $0CF1;
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_UNPACK_SKIP_ROWS               = $0CF3;
  GL_UNPACK_SKIP_PIXELS             = $0CF4;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_SWAP_BYTES                = $0D00;
  GL_PACK_LSB_FIRST                 = $0D01;
  GL_PACK_ROW_LENGTH                = $0D02;
  GL_PACK_SKIP_ROWS                 = $0D03;
  GL_PACK_SKIP_PIXELS               = $0D04;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAP_COLOR                      = $0D10;
  GL_MAP_STENCIL                    = $0D11;
  GL_INDEX_SHIFT                    = $0D12;
  GL_INDEX_OFFSET                   = $0D13;
  GL_RED_SCALE                      = $0D14;
  GL_RED_BIAS                       = $0D15;
  GL_ZOOM_X                         = $0D16;
  GL_ZOOM_Y                         = $0D17;
  GL_GREEN_SCALE                    = $0D18;
  GL_GREEN_BIAS                     = $0D19;
  GL_BLUE_SCALE                     = $0D1A;
  GL_BLUE_BIAS                      = $0D1B;
  GL_ALPHA_SCALE                    = $0D1C;
  GL_ALPHA_BIAS                     = $0D1D;
  GL_DEPTH_SCALE                    = $0D1E;
  GL_DEPTH_BIAS                     = $0D1F;
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_INDEX_BITS                     = $0D51;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_ACCUM_RED_BITS                 = $0D58;
  GL_ACCUM_GREEN_BITS               = $0D59;
  GL_ACCUM_BLUE_BITS                = $0D5A;
  GL_ACCUM_ALPHA_BITS               = $0D5B;
  GL_NAME_STACK_DEPTH               = $0D70;
  GL_AUTO_NORMAL                    = $0D80;
  GL_MAP1_COLOR_4                   = $0D90;
  GL_MAP1_INDEX                     = $0D91;
  GL_MAP1_NORMAL                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1           = $0D93;
  GL_MAP1_TEXTURE_COORD_2           = $0D94;
  GL_MAP1_TEXTURE_COORD_3           = $0D95;
  GL_MAP1_TEXTURE_COORD_4           = $0D96;
  GL_MAP1_VERTEX_3                  = $0D97;
  GL_MAP1_VERTEX_4                  = $0D98;
  GL_MAP2_COLOR_4                   = $0DB0;
  GL_MAP2_INDEX                     = $0DB1;
  GL_MAP2_NORMAL                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4           = $0DB6;
  GL_MAP2_VERTEX_3                  = $0DB7;
  GL_MAP2_VERTEX_4                  = $0DB8;
  GL_MAP1_GRID_DOMAIN               = $0DD0;
  GL_MAP1_GRID_SEGMENTS             = $0DD1;
  GL_MAP2_GRID_DOMAIN               = $0DD2;
  GL_MAP2_GRID_SEGMENTS             = $0DD3;
  GL_TEXTURE_1D                     = $0DE0;
  GL_TEXTURE_2D                     = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE           = $0DF2;
  GL_SELECTION_BUFFER_POINTER       = $0DF3;
  GL_SELECTION_BUFFER_SIZE          = $0DF4;

  //      GL_TEXTURE_BINDING_1D
  //      GL_TEXTURE_BINDING_2D
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_VERTEX_ARRAY_SIZE
  //      GL_VERTEX_ARRAY_TYPE
  //      GL_VERTEX_ARRAY_STRIDE
  //      GL_NORMAL_ARRAY_TYPE
  //      GL_NORMAL_ARRAY_STRIDE
  //      GL_COLOR_ARRAY_SIZE
  //      GL_COLOR_ARRAY_TYPE
  //      GL_COLOR_ARRAY_STRIDE
  //      GL_INDEX_ARRAY_TYPE
  //      GL_INDEX_ARRAY_STRIDE
  //      GL_TEXTURE_COORD_ARRAY_SIZE
  //      GL_TEXTURE_COORD_ARRAY_TYPE
  //      GL_TEXTURE_COORD_ARRAY_STRIDE
  //      GL_EDGE_FLAG_ARRAY_STRIDE
  //      GL_POLYGON_OFFSET_FACTOR
  //      GL_POLYGON_OFFSET_UNITS

  // GetTextureParameter
  //      GL_TEXTURE_MAG_FILTER
  //      GL_TEXTURE_MIN_FILTER
  //      GL_TEXTURE_WRAP_S
  //      GL_TEXTURE_WRAP_T
  GL_TEXTURE_WIDTH                  = $1000;
  GL_TEXTURE_HEIGHT                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT        = $1003;
  GL_TEXTURE_BORDER_COLOR           = $1004;
  GL_TEXTURE_BORDER                 = $1005;
  //      GL_TEXTURE_RED_SIZE
  //      GL_TEXTURE_GREEN_SIZE
  //      GL_TEXTURE_BLUE_SIZE
  //      GL_TEXTURE_ALPHA_SIZE
  //      GL_TEXTURE_LUMINANCE_SIZE
  //      GL_TEXTURE_INTENSITY_SIZE
  //      GL_TEXTURE_PRIORITY
  //      GL_TEXTURE_RESIDENT

  // IndexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // LightModelParameter
  //      GL_LIGHT_MODEL_AMBIENT
  //      GL_LIGHT_MODEL_LOCAL_VIEWER
  //      GL_LIGHT_MODEL_TWO_SIDE

  // LightName
  GL_LIGHT0                         = $4000;
  GL_LIGHT1                         = $4001;
  GL_LIGHT2                         = $4002;
  GL_LIGHT3                         = $4003;
  GL_LIGHT4                         = $4004;
  GL_LIGHT5                         = $4005;
  GL_LIGHT6                         = $4006;
  GL_LIGHT7                         = $4007;

  // LightParameter
  GL_AMBIENT                        = $1200;
  GL_DIFFUSE                        = $1201;
  GL_SPECULAR                       = $1202;
  GL_POSITION                       = $1203;
  GL_SPOT_DIRECTION                 = $1204;
  GL_SPOT_EXPONENT                  = $1205;
  GL_SPOT_CUTOFF                    = $1206;
  GL_CONSTANT_ATTENUATION           = $1207;
  GL_LINEAR_ATTENUATION             = $1208;
  GL_QUADRATIC_ATTENUATION          = $1209;

  // InterleavedArrays
  //      GL_V2F
  //      GL_V3F
  //      GL_C4UB_V2F
  //      GL_C4UB_V3F
  //      GL_C3F_V3F
  //      GL_N3F_V3F
  //      GL_C4F_N3F_V3F
  //      GL_T2F_V3F
  //      GL_T4F_V4F
  //      GL_T2F_C4UB_V3F
  //      GL_T2F_C3F_V3F
  //      GL_T2F_N3F_V3F
  //      GL_T2F_C4F_N3F_V3F
  //      GL_T4F_C4F_N3F_V4F

  // ListMode
  GL_COMPILE                        = $1300;
  GL_COMPILE_AND_EXECUTE            = $1301;

  // ListNameType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_2_BYTES
  //      GL_3_BYTES
  //      GL_4_BYTES

  // LogicOp
  GL_CLEAR                          = $1500;
  GL_AND                            = $1501;
  GL_AND_REVERSE                    = $1502;
  GL_COPY                           = $1503;
  GL_AND_INVERTED                   = $1504;
  GL_NOOP                           = $1505;
  GL_XOR                            = $1506;
  GL_OR                             = $1507;
  GL_NOR                            = $1508;
  GL_EQUIV                          = $1509;
  GL_INVERT                         = $150A;
  GL_OR_REVERSE                     = $150B;
  GL_COPY_INVERTED                  = $150C;
  GL_OR_INVERTED                    = $150D;
  GL_NAND                           = $150E;
  GL_SET                            = $150F;

  // MapTarget
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4

  // MaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // MaterialParameter
  GL_EMISSION                       = $1600;
  GL_SHININESS                      = $1601;
  GL_AMBIENT_AND_DIFFUSE            = $1602;
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR

  // MatrixMode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // NormalPointerType
  //      GL_BYTE
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // PixelCopyType
  GL_COLOR                          = $1800;
  GL_DEPTH                          = $1801;
  GL_STENCIL                        = $1802;

  // PixelFormat
  GL_COLOR_INDEX                    = $1900;
  GL_STENCIL_INDEX                  = $1901;
  GL_DEPTH_COMPONENT                = $1902;
  GL_RED                            = $1903;
  GL_GREEN                          = $1904;
  GL_BLUE                           = $1905;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;

  GL_RGBA16F                        = $881A;
  GL_RGB16F                         = $881B;
  GL_HALF_FLOAT                     = $140B;

  GL_DEPTH24_STENCIL8 = $88F0;


  // PolygonMode
  GL_POINT                          = $1B00;
  GL_LINE                           = $1B01;
  GL_FILL                           = $1B02;

  // ShadingModel
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // StencilFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // StencilOp
  //      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  //      GL_INVERT

  // StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // TextureCoordName
  GL_S                              = $2000;
  GL_T                              = $2001;
  GL_R                              = $2002;
  GL_Q                              = $2003;

  // TextureEnvMode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  //      GL_BLEND
  //      GL_REPLACE

  // TextureEnvParameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;

  // TextureEnvTarget
  GL_TEXTURE_ENV                    = $2300;

  // TextureGenMode
  GL_EYE_LINEAR                     = $2400;
  GL_OBJECT_LINEAR                  = $2401;
  GL_SPHERE_MAP                     = $2402;

  // TextureGenParameter
  GL_TEXTURE_GEN_MODE               = $2500;
  GL_OBJECT_PLANE                   = $2501;
  GL_EYE_PLANE                      = $2502;

  // TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

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
  //      GL_TEXTURE_BORDER_COLOR
  //      GL_TEXTURE_PRIORITY

  // TextureTarget
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_PROXY_TEXTURE_1D
  //      GL_PROXY_TEXTURE_2D

  // TextureWrapMode
  GL_CLAMP                          = $2900;
  GL_REPEAT                         = $2901;

  // VertexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // ClientAttribMask
{  GL_CLIENT_PIXEL_STORE_BIT         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS         = $FFFFFFFF;
 }
  // polygon_offset
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
  GL_POLYGON_OFFSET_POINT           = $2A01;
  GL_POLYGON_OFFSET_LINE            = $2A02;
  GL_POLYGON_OFFSET_FILL            = $8037;

  // texture
  GL_ALPHA4                         = $803B;
  GL_ALPHA8                         = $803C;
  GL_ALPHA12                        = $803D;
  GL_ALPHA16                        = $803E;
  GL_LUMINANCE4                     = $803F;
  GL_LUMINANCE8                     = $8040;
  GL_LUMINANCE12                    = $8041;
  GL_LUMINANCE16                    = $8042;
  GL_LUMINANCE4_ALPHA4              = $8043;
  GL_LUMINANCE6_ALPHA2              = $8044;
  GL_LUMINANCE8_ALPHA8              = $8045;
  GL_LUMINANCE12_ALPHA4             = $8046;
  GL_LUMINANCE12_ALPHA12            = $8047;
  GL_LUMINANCE16_ALPHA16            = $8048;
  GL_INTENSITY                      = $8049;
  GL_INTENSITY4                     = $804A;
  GL_INTENSITY8                     = $804B;
  GL_INTENSITY12                    = $804C;
  GL_INTENSITY16                    = $804D;
  GL_R3_G3_B2                       = $2A10;
  GL_RGB4                           = $804F;
  GL_RGB5                           = $8050;
  GL_RGB8                           = $8051;
  GL_RGB10                          = $8052;
  GL_RGB12                          = $8053;
  GL_RGB16                          = $8054;
  GL_RGBA2                          = $8055;
  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGBA8                          = $8058;
  GL_RGB10_A2                       = $8059;
  GL_RGBA12                         = $805A;
  GL_RGBA16                         = $805B;
  GL_TEXTURE_RED_SIZE               = $805C;
  GL_TEXTURE_GREEN_SIZE             = $805D;
  GL_TEXTURE_BLUE_SIZE              = $805E;
  GL_TEXTURE_ALPHA_SIZE             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE         = $8060;
  GL_TEXTURE_INTENSITY_SIZE         = $8061;

  // texture_object
{  GL_TEXTURE_PRIORITY               = $8066;
  GL_TEXTURE_RESIDENT               = $8067;
  GL_TEXTURE_BINDING_1D             = $8068;
  GL_TEXTURE_BINDING_2D             = $8069;
 }
  // vertex_array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;
  GL_EDGE_FLAG_ARRAY                = $8079;
  GL_VERTEX_ARRAY_SIZE              = $807A;
  GL_VERTEX_ARRAY_TYPE              = $807B;
  GL_VERTEX_ARRAY_STRIDE            = $807C;
  GL_NORMAL_ARRAY_TYPE              = $807E;
  GL_NORMAL_ARRAY_STRIDE            = $807F;
  GL_COLOR_ARRAY_SIZE               = $8081;
  GL_COLOR_ARRAY_TYPE               = $8082;
  GL_COLOR_ARRAY_STRIDE             = $8083;
  GL_INDEX_ARRAY_TYPE               = $8085;
  GL_INDEX_ARRAY_STRIDE             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE         = $808C;
  GL_VERTEX_ARRAY_POINTER           = $808E;
  GL_NORMAL_ARRAY_POINTER           = $808F;
  GL_COLOR_ARRAY_POINTER            = $8090;
  GL_INDEX_ARRAY_POINTER            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER        = $8093;


//  GL_version_1_2
Const
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_RESCALE_NORMAL = $803A;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_CLAMP_TO_EDGE = $812F;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SINGLE_COLOR = $81F9;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;

//  GL_version_1_3
Const
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX = $84E6;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_MULTISAMPLE_BIT = $20000000;
  GL_NORMAL_MAP = $8511;
  GL_REFLECTION_MAP = $8512;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_ALPHA = $84E9;
  GL_COMPRESSED_LUMINANCE = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
  GL_COMPRESSED_INTENSITY = $84EC;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;
  GL_CLAMP_TO_BORDER_SGIS = $812D;
  GL_COMBINE = $8570;
  GL_COMBINE_RGB = $8571;
  GL_COMBINE_ALPHA = $8572;
  GL_SOURCE0_RGB = $8580;
  GL_SOURCE1_RGB = $8581;
  GL_SOURCE2_RGB = $8582;
  GL_SOURCE0_ALPHA = $8588;
  GL_SOURCE1_ALPHA = $8589;
  GL_SOURCE2_ALPHA = $858A;
  GL_OPERAND0_RGB = $8590;
  GL_OPERAND1_RGB = $8591;
  GL_OPERAND2_RGB = $8592;
  GL_OPERAND0_ALPHA = $8598;
  GL_OPERAND1_ALPHA = $8599;
  GL_OPERAND2_ALPHA = $859A;
  GL_RGB_SCALE = $8573;
  GL_ADD_SIGNED = $8574;
  GL_INTERPOLATE = $8575;
  GL_SUBTRACT = $84E7;
  GL_CONSTANT = $8576;
  GL_PRIMARY_COLOR = $8577;
  GL_PREVIOUS = $8578;
  GL_DOT3_RGB = $86AE;
  GL_DOT3_RGBA = $86AF;
  GL_ADD = $104;

//  GL_version_1_4
Const
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_POINT_DISTANCE_ATTENUATION = $8129;
  GL_GENERATE_MIPMAP = $8191;
  GL_GENERATE_MIPMAP_HINT = $8192;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_MIRRORED_REPEAT = $8370;
  GL_FOG_COORDINATE_SOURCE = $8450;
  GL_FOG_COORDINATE = $8451;
  GL_FRAGMENT_DEPTH = $8452;
  GL_CURRENT_FOG_COORDINATE = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORDINATE_ARRAY = $8457;
  GL_COLOR_SUM = $8458;
  GL_CURRENT_SECONDARY_COLOR = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D;
  GL_SECONDARY_COLOR_ARRAY = $845E;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_FILTER_CONTROL = $8500;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_DEPTH_TEXTURE_MODE = $884B;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;

//  GL_version_1_5
Const
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_SAMPLES_PASSED = $8914;
  GL_FOG_COORD_SRC = $8450;
  GL_FOG_COORD = $8451;
  GL_CURRENT_FOG_COORD = $8453;
  GL_FOG_COORD_ARRAY_TYPE = $8454;
  GL_FOG_COORD_ARRAY_STRIDE = $8455;
  GL_FOG_COORD_ARRAY_POINTER = $8456;
  GL_FOG_COORD_ARRAY = $8457;
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = $889D;
  GL_SRC0_RGB = $8580;
  GL_SRC1_RGB = $8581;
  GL_SRC2_RGB = $8582;
  GL_SRC0_ALPHA = $8588;
  GL_SRC1_ALPHA = $8589;
  GL_SRC2_ALPHA = $858A;

  GL_COMPRESSED_RGB_S3TC_DXT1			= $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1		= $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3		= $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5		= $83F3;

//  GL_version_2_0
Const
  GL_BLEND_EQUATION_RGB = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_POINT_SPRITE = $8861;
  GL_COORD_REPLACE = $8862;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_MAX_TEXTURE_COORDS = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
 GL_MAX_VERTEX_UNIFORM_VECTORS       = $8DFB;
 GL_MAX_VARYING_VECTORS              = $8DFC;
 GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
 GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS   = $8B4C;
 GL_MAX_FRAGMENT_UNIFORM_VECTORS     = $8DFD;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_PIXEL_UNPACK_BUFFER       = $88EC;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;


// ******************************************************************************
//  GL_EXT_framebuffer_object
// ******************************************************************************
Const
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
{  GL_STENCIL_INDEX = $8D45;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;}
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET = $8CD4;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_FRAMEBUFFER_STATUS_ERROR = $8CDE;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

  GL_DRAW_FRAMEBUFFER_BINDING  = $8CA6;
  GL_READ_FRAMEBUFFER          = $8CA8;
  GL_DRAW_FRAMEBUFFER          = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;

Const
  VBO_FREE_MEMORY_ATI                     = $87FB;
  TEXTURE_FREE_MEMORY_ATI                 = $87FC;
  RENDERBUFFER_FREE_MEMORY_ATI            = $87FD;


{$IFDEF WINDOWS}
// ******************************************************************************
//  WGL_ARB_pixel_format
// ******************************************************************************
Const
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;
  WGL_SAMPLE_BUFFERS_ARB = $2041;                                               // Symbolické konstanty pro multisampling
  WGL_SAMPLES_ARB	= $2042;

Const
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;

  WGL_BIND_TO_TEXTURE_RGB_ARB         = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB        = $2071;
  WGL_TEXTURE_FORMAT_ARB              = $2072;
  WGL_TEXTURE_TARGET_ARB              = $2073;
  WGL_MIPMAP_TEXTURE_ARB              = $2074;
  WGL_TEXTURE_RGB_ARB                 = $2075;
  WGL_TEXTURE_RGBA_ARB                = $2076;
  WGL_NO_TEXTURE_ARB                  = $2077;
  WGL_TEXTURE_1D_ARB                  = $2079;
  WGL_TEXTURE_2D_ARB                  = $207A;

{$ENDIF}

// ************************************
//  GL_EXT_texture_compression_s3tc
// ************************************
Const
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

Const
  GL_STENCIL_TEST_TWO_SIDE_EXT = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT = $8911;

// GL_EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT                          = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                      = $84FF;


Var
glAccum:Procedure (op:Cardinal; value: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glAlphaFunc:Procedure (func: Cardinal; ref: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glArrayElement:Procedure (i: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glBegin:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glBindTexture:Procedure (target: Cardinal; texture: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glBlendFunc:Procedure (sfactor, dfactor: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCallList:Procedure (list: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCallLists:Procedure (n: Integer; atype: Cardinal; const lists: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClear:Procedure (mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClearAccum:Procedure (red, green, blue, alpha: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClearColor:Procedure (red, green, blue, alpha: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClearDepth:Procedure (depth: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClearIndex:Procedure (c: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClearStencil:Procedure (s: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glClipPlane:Procedure (plane: Cardinal; const equation: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3b:Procedure (red, green, blue: Shortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3bv:Procedure (const v: PShortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3d:Procedure (red, green, blue: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3f:Procedure (red, green, blue: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3i:Procedure (red, green, blue: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3s:Procedure (red, green, blue: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3ub:Procedure (red, green, blue: Byte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3ubv:Procedure (const v: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3ui:Procedure (red, green, blue: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3uiv:Procedure (const v: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3us:Procedure (red, green, blue: Word); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor3usv:Procedure (const v: PWord); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4b:Procedure (red, green, blue, alpha: Shortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4bv:Procedure (const v: PShortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4d:Procedure (red, green, blue, alpha: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4f:Procedure (red, green, blue, alpha: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4i:Procedure (red, green, blue, alpha: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4s:Procedure (red, green, blue, alpha: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4ub:Procedure (red, green, blue, alpha: Byte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4ubv:Procedure (const v: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4ui:Procedure (red, green, blue, alpha: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4uiv:Procedure (const v: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4us:Procedure (red, green, blue, alpha: Word); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColor4usv:Procedure (const v: PWord); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColorMask:Procedure (red, green, blue, alpha: Boolean); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColorMaterial:Procedure (face, mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glColorPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCopyPixels:Procedure (x, y: Integer; width, height: Integer; atype: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCopyTexImage1D :Procedure (target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width: Integer; border: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCopyTexImage2D:Procedure (target: Cardinal; level: Integer; internalFormat: Cardinal; x, y: Integer; width, height: Integer; border: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCopyTexSubImage1D:Procedure (target: Cardinal; level, xoffset, x, y: Integer; width: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCopyTexSubImage2D:Procedure (target: Cardinal; level, xoffset, yoffset, x, y: Integer; width, height: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glCullFace:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDeleteLists:Procedure (list: Cardinal; range: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDeleteTextures:Procedure (n: Integer; const textures: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDepthFunc:Procedure (func: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDepthMask:Procedure (flag: Boolean); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDepthRange:Procedure (zNear, zFar: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDisable:Procedure (cap: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDisableClientState:Procedure (aarray: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDrawArrays:Procedure (mode: Cardinal; first: Integer; count: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDrawBuffer:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDrawElements:Procedure (mode: Cardinal; count: Integer; atype: Cardinal; const indices: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glDrawPixels:Procedure (width, height: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glEnable:Procedure (cap: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glEnableClientState:Procedure (aarray: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glEnd:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glEndList:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFinish:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFlush:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFogf:Procedure (pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFogfv:Procedure (pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFogi:Procedure (pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFogiv:Procedure (pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFrontFace:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glFrustum:Procedure (left, right, bottom, top, zNear, zFar: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGenLists:Function (range: Integer): Cardinal; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGenTextures:Procedure (n: Integer; textures: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetBooleanv:Procedure (pname: Cardinal; params: PBoolean); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetClipPlane:Procedure (plane: Cardinal; equation: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetDoublev:Procedure (pname: Cardinal; params: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetError:Function():Cardinal; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetFloatv:Procedure (pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetIntegerv:Procedure (pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetLightfv:Procedure (light, pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetLightiv:Procedure (light, pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetMapdv:Procedure (target, query: Cardinal; v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetMapfv:Procedure (target, query: Cardinal; v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetMapiv:Procedure (target, query: Cardinal; v: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetMaterialfv:Procedure (face, pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetMaterialiv:Procedure (face, pname: Cardinal; params: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetPointerv:Procedure (pname: Cardinal; params: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetPolygonStipple:Procedure (mask: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetString:Function (name: Cardinal): PAnsiChar; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexEnvfv:Procedure (target, pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexEnviv:Procedure (target, pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexGendv:Procedure (coord, pname: Cardinal; params: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexGenfv:Procedure (coord, pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexGeniv:Procedure (coord, pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexImage:Procedure (target: Cardinal; level: Integer; format: Cardinal; atype: Cardinal; pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexLevelParameterfv:Procedure (target: Cardinal; level: Integer; pname: Cardinal; params: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexLevelParameteriv:Procedure (target: Cardinal; level: Integer; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexParameterfv:Procedure (target, pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glGetTexParameteriv:Procedure (target, pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glHint:Procedure (target, mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glIndexMask:Procedure (mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glInterleavedArrays:Procedure (format: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glIsEnabled:Function  (cap: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glIsList:Function (list: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glIsTexture:Function  (texture: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightModelf:Procedure (pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightModelfv:Procedure (pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightModeli:Procedure (pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightModeliv:Procedure (pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightf:Procedure (light, pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightfv:Procedure (light, pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLighti:Procedure (light, pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLightiv:Procedure (light, pname: Cardinal; const params: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLineStipple:Procedure (factor: Integer; pattern: Word); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLineWidth:Procedure (width: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLoadIdentity:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLoadMatrixd:Procedure (const m: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLoadMatrixf:Procedure (const m: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glLogicOp:Procedure (opcode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMaterialf:Procedure (face, pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMaterialfv:Procedure (face, pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMateriali:Procedure (face, pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMaterialiv:Procedure (face, pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMatrixMode:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMultMatrixd:Procedure (const m: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glMultMatrixf:Procedure (const m: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glNewList:Procedure (list: Cardinal; mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

glPixelStorei:Procedure (pname, param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

glNormal3f:Procedure (nx, ny, nz: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glNormal3fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glNormalPointer:Procedure (atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

glOrtho:Procedure (left, right, bottom, top, zNear, zFar: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

glPointSize:Procedure (size: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPolygonMode:Procedure (face, mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPolygonOffset:Procedure (factor, units: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPolygonStipple:Procedure (const mask: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPopAttrib:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPopClientAttrib:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPopMatrix:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPrioritizeTextures:Procedure (n: Integer; const textures: PCardinal; const priorities: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPushAttrib:Procedure (mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPushClientAttrib:Procedure (mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glPushMatrix:Procedure(); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glReadBuffer:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glReadPixels:Procedure (x, y: Integer; width, height: Integer; format, atype: Cardinal; pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glRenderMode:Function (mode: Integer): Integer; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glRotated:Procedure (angle, x, y, z: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glRotatef:Procedure (angle, x, y, z: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glScaled:Procedure (x, y, z: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glScalef:Procedure (x, y, z: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glScissor:Procedure (x, y: Integer; width, height: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glSelectBuffer:Procedure (size: Integer; buffer: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glShadeModel:Procedure (mode: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glStencilFunc:Procedure (func: Cardinal; ref: Integer; mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glStencilMask:Procedure (mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glStencilOp:Procedure (fail, zfail, zpass: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1d:Procedure (s: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1f:Procedure (s: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1i:Procedure (s: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1s:Procedure (s: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord1sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2d:Procedure (s, t: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2f:Procedure (s, t: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2i:Procedure (s, t: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2s:Procedure (s, t: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord2sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3d:Procedure (s, t, r: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3f:Procedure (s, t, r: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3i:Procedure (s, t, r: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3s:Procedure (s, t, r: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord3sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4d:Procedure (s, t, r, q: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4f:Procedure (s, t, r, q: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4i:Procedure (s, t, r, q: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4s:Procedure (s, t, r, q: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoord4sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexCoordPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexEnvf:Procedure (target: Cardinal; pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexEnvfv:Procedure (target: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexEnvi:Procedure (target: Cardinal; pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexEnviv:Procedure (target: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGend:Procedure (coord: Cardinal; pname: Cardinal; param: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGendv:Procedure (coord: Cardinal; pname: Cardinal; const params: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGenf:Procedure (coord: Cardinal; pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGenfv:Procedure (coord: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGeni:Procedure (coord: Cardinal; pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexGeniv:Procedure (coord: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexImage1D:Procedure (target: Cardinal; level, internalformat: Integer; width: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexImage2D:Procedure (target: Cardinal; level, internalformat: Integer; width, height: Integer; border: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexParameterf:Procedure (target: Cardinal; pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexParameterfv:Procedure (target: Cardinal; pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexParameteri:Procedure (target: Cardinal; pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexParameteriv:Procedure (target: Cardinal; pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexSubImage1D:Procedure (target: Cardinal; level, xoffset: Integer; width: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTexSubImage2D:Procedure (target: Cardinal; level, xoffset, yoffset: Integer; width, height: Integer; format, atype: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTranslated:Procedure (x, y, z: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glTranslatef:Procedure (x, y, z: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2d:Procedure (x, y: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2f:Procedure (x, y: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2i:Procedure (x, y: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2s:Procedure (x, y: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex2sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3d:Procedure (x, y, z: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3f:Procedure (x, y, z: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3i:Procedure (x, y, z: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3s:Procedure (x, y, z: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex3sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4d:Procedure (x, y, z, w: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4dv:Procedure (const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4f:Procedure (x, y, z, w: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4fv:Procedure (const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4i:Procedure (x, y, z, w: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4iv:Procedure (const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4s:Procedure (x, y, z, w: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertex4sv:Procedure (const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glVertexPointer:Procedure (size: Integer; atype: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
glViewport:Procedure (x, y: Integer; width, height: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_2

Var
  glDrawRangeElements: procedure(mode: Cardinal; start: Cardinal; _end: Cardinal; count: Integer; _type: Cardinal; const indices: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Integer; width: Integer; height: Integer; depth: Integer; border: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; _type: Cardinal; const pixels: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; x: Integer; y: Integer; width: Integer; height: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBlendEquation:Procedure(mode:Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_3

Var
  glActiveTexture: procedure(texture: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glClientActiveTexture: procedure(texture: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1d: procedure(target: Cardinal; s: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1f: procedure(target: Cardinal; s: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1i: procedure(target: Cardinal; s: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1s: procedure(target: Cardinal; s: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord1sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2d: procedure(target: Cardinal; s: Double; t: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2f: procedure(target: Cardinal; s: Single; t: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2i: procedure(target: Cardinal; s: Integer; t: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2s: procedure(target: Cardinal; s: SmallInt; t: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord2sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3d: procedure(target: Cardinal; s: Double; t: Double; r: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3f: procedure(target: Cardinal; s: Single; t: Single; r: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord3sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4d: procedure(target: Cardinal; s: Double; t: Double; r: Double; q: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4dv: procedure(target: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4f: procedure(target: Cardinal; s: Single; t: Single; r: Single; q: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4fv: procedure(target: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4i: procedure(target: Cardinal; s: Integer; t: Integer; r: Integer; q: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4iv: procedure(target: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4s: procedure(target: Cardinal; s: SmallInt; t: SmallInt; r: SmallInt; q: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiTexCoord4sv: procedure(target: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glLoadTransposeMatrixf: procedure(const m: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glLoadTransposeMatrixd: procedure(const m: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultTransposeMatrixf: procedure(const m: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultTransposeMatrixd: procedure(const m: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glSampleCoverage: procedure(value: Single; invert: Boolean); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage3D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; depth: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage2D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; height: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexImage1D: procedure(target: Cardinal; level: Integer; internalformat: Cardinal; width: Integer; border: Integer; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage3D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; zoffset: Integer; width: Integer; height: Integer; depth: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage2D: procedure(target: Cardinal; level: Integer; xoffset: Integer; yoffset: Integer; width: Integer; height: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompressedTexSubImage1D: procedure(target: Cardinal; level: Integer; xoffset: Integer; width: Integer; format: Cardinal; imageSize: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetCompressedTexImage: procedure(target: Cardinal; level: Integer; img: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_4
Var
  glBlendFuncSeparate: procedure(sfactorRGB: Cardinal; dfactorRGB: Cardinal; sfactorAlpha: Cardinal; dfactorAlpha: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFogCoordf: procedure(coord: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFogCoordfv: procedure(const coord: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFogCoordd: procedure(coord: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFogCoorddv: procedure(const coord: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFogCoordPointer: procedure(_type: Cardinal; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiDrawArrays: procedure(mode: Cardinal; first: PInteger; count: PInteger; primcount: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMultiDrawElements: procedure(mode: Cardinal; const count: PInteger; _type: Cardinal; const indices: Pointer; primcount: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glPointParameterf: procedure(pname: Cardinal; param: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glPointParameterfv: procedure(pname: Cardinal; const params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glPointParameteri: procedure(pname: Cardinal; param: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glPointParameteriv: procedure(pname: Cardinal; const params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

//  GL_version_1_5
Var
  glGenQueries: procedure(n: Integer; ids: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteQueries: procedure(n: Integer; const ids: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glIsQuery: function(id: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBeginQuery: procedure(target: Cardinal; id: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glEndQuery: procedure(target: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetQueryiv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetQueryObjectiv: procedure(id: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetQueryObjectuiv: procedure(id: Cardinal; pname: Cardinal; params: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBindBuffer: procedure(target: Cardinal; buffer: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteBuffers: procedure(n: Integer; const buffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGenBuffers: procedure(n: Integer; buffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glIsBuffer: function(buffer: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBufferData: procedure(target: Cardinal; size: Integer; const data: Pointer; usage: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; const data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetBufferSubData: procedure(target: Cardinal; offset: Integer; size: Integer; data: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glMapBuffer: function(target: Cardinal; access: Cardinal): Pointer; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUnmapBuffer: function(target: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetBufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetBufferPointerv: procedure(target: Cardinal; pname: Cardinal; params: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

//  GL_version_2_0
Var
  glBlendEquationSeparate: procedure(modeRGB: Cardinal; modeAlpha: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDrawBuffers: procedure(n: Integer; const bufs: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glStencilOpSeparate: procedure(face: Cardinal; sfail: Cardinal; dpfail: Cardinal; dppass: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glStencilFuncSeparate: procedure(frontfunc: Cardinal; backfunc: Cardinal; ref: Integer; mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glStencilMaskSeparate: procedure(face: Cardinal; mask: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glAttachShader: procedure(_program: Cardinal; shader: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBindAttribLocation: procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCompileShader: procedure(shader: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCreateProgram: function(): Cardinal; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCreateShader: function(_type: Cardinal): Cardinal; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteProgram: procedure(_program: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteShader: procedure(shader: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDetachShader: procedure(_program: Cardinal; shader: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDisableVertexAttribArray: procedure(index: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glEnableVertexAttribArray: procedure(index: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetActiveAttrib: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetActiveUniform: procedure(_program: Cardinal; index: Cardinal; bufSize: Integer; length: PInteger; size: PInteger; _type: PCardinal; name: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetAttachedShaders: procedure(_program: Cardinal; maxCount: Integer; count: PInteger; obj: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetAttribLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetProgramiv: procedure(_program: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetProgramInfoLog: procedure(_program: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetShaderiv: procedure(shader: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetShaderInfoLog: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; infoLog: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetShaderSource: procedure(shader: Cardinal; bufSize: Integer; length: PInteger; source: PAnsiChar); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetUniformLocation: function(_program: Cardinal; const name: PAnsiChar): Integer; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetUniformfv: procedure(_program: Cardinal; location: Integer; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetUniformiv: procedure(_program: Cardinal; location: Integer; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribdv: procedure(index: Cardinal; pname: Cardinal; params: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribfv: procedure(index: Cardinal; pname: Cardinal; params: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribiv: procedure(index: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetVertexAttribPointerv: procedure(index: Cardinal; pname: Cardinal; pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glIsProgram: function(_program: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glIsShader: function(shader: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glLinkProgram: procedure(_program: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glShaderSource: procedure(shader: Cardinal; count: Integer; const _string: PAnsiChar; const length: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUseProgram: procedure(_program: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform1f: procedure(location: Integer; v0: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform2f: procedure(location: Integer; v0: Single; v1: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform3f: procedure(location: Integer; v0: Single; v1: Single; v2: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform4f: procedure(location: Integer; v0: Single; v1: Single; v2: Single; v3: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform1i: procedure(location: Integer; v0: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform2i: procedure(location: Integer; v0: Integer; v1: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform3i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform4i: procedure(location: Integer; v0: Integer; v1: Integer; v2: Integer; v3: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform1fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform2fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform3fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform4fv: procedure(location: Integer; count: Integer; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform1iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform2iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform3iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniform4iv: procedure(location: Integer; count: Integer; const value: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix2fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix3fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix4fv: procedure(location: Integer; count: Integer; transpose: Boolean; const value: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glValidateProgram: procedure(_program: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1d: procedure(index: Cardinal; x: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1f: procedure(index: Cardinal; x: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1s: procedure(index: Cardinal; x: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib1sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2d: procedure(index: Cardinal; x: Double; y: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2f: procedure(index: Cardinal; x: Single; y: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2s: procedure(index: Cardinal; x: SmallInt; y: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib2sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3d: procedure(index: Cardinal; x: Double; y: Double; z: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3f: procedure(index: Cardinal; x: Single; y: Single; z: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib3sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nbv: procedure(index: Cardinal; const v: PShortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Niv: procedure(index: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nsv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nub: procedure(index: Cardinal; x: Byte; y: Byte; z: Byte; w: Byte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nubv: procedure(index: Cardinal; const v: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nuiv: procedure(index: Cardinal; const v: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4Nusv: procedure(index: Cardinal; const v: PWord); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4bv: procedure(index: Cardinal; const v: PShortint); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4d: procedure(index: Cardinal; x: Double; y: Double; z: Double; w: Double); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4dv: procedure(index: Cardinal; const v: PDouble); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4f: procedure(index: Cardinal; x: Single; y: Single; z: Single; w: Single); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4fv: procedure(index: Cardinal; const v: PSingle); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4iv: procedure(index: Cardinal; const v: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4s: procedure(index: Cardinal; x: SmallInt; y: SmallInt; z: SmallInt; w: SmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4sv: procedure(index: Cardinal; const v: PSmallInt); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4ubv: procedure(index: Cardinal; const v: PByte); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4uiv: procedure(index: Cardinal; const v: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttrib4usv: procedure(index: Cardinal; const v: PWord); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttribPointer: procedure(index: Cardinal; size: Integer; _type: Cardinal; normalized: Boolean; stride: Integer; const pointer: Pointer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGenerateMipmap: procedure (target:Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

// ******************************************************************************
//  GL_EXT_framebuffer_object
// ******************************************************************************
Var
  glIsRenderbuffer: function(renderbuffer: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBindRenderbuffer: procedure(target: Cardinal; renderbuffer: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteRenderbuffers: procedure(n: Integer; const renderbuffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGenRenderbuffers: procedure(n: Integer; renderbuffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glRenderbufferStorage: procedure(target: Cardinal; internalformat: Cardinal; width: Integer; height: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetRenderbufferParameteriv: procedure(target: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glIsFramebuffer: function(framebuffer: Cardinal): Boolean; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBindFramebuffer: procedure(target: Cardinal; framebuffer: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDeleteFramebuffers: procedure(n: Integer; const framebuffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGenFramebuffers: procedure(n: Integer; framebuffers: PCardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glCheckFramebufferStatus: function(target: Cardinal): Cardinal; {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture1D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture2D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture3D: procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: Integer; zoffset: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferRenderbuffer: procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glGetFramebufferAttachmentParameteriv: procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PInteger); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

  glRenderbufferStorageMultisample: procedure (target:Cardinal; samples:Integer; internalformat:Cardinal;  width, height:Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glBlitFramebuffer: procedure (srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1:Integer; mask:Cardinal; filter:Cardinal); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}


  glEnableIndexedEXT: procedure(target, index:Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
  glDisableIndexedEXT: procedure(target, index:Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

  glActiveStencilFaceEXT: Procedure(face: Integer); {$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}

{$IFDEF WINDOWS}
  wglSwapIntervalEXT: function(interval: Integer): Boolean; stdcall = Nil;
  //wglGetSwapIntervalEXT: function(): Integer; stdcall;
{$ENDIF}


{$IFDEF LINUX}
  glXSwapIntervalEXT: function(interval: Integer): Boolean; CDecl = Nil;
{$ENDIF}

Procedure glLoadExtensions;

Function glGetExtensionString():AnsiString;

Function glExtensionSupported(Extension:AnsiString):Boolean;

Procedure LoadOpenGL();

Var
  ExtensionsList:AnsiString='';


Implementation

Uses TERRA_OS, TERRA_Error;

Function glGetExtensionString():AnsiString;
Begin
  Result := ExtensionsList;
End;

Var
  OpenGLHandle:TLibHandle;

Procedure defaultglGenerateMipmap(Target:Cardinal);{$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
Begin
  glTexParameteri(Target, GL_GENERATE_MIPMAP, 1);
End;

Procedure dummyglActiveTexture(texture: Cardinal);{$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
Begin
  // do nothing
End;

Procedure dummyglClientActiveTexture(texture: Cardinal);{$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
Begin
  // do nothing
End;

{$IFDEF WINDOWS}
Function wglGetProcAddress(proc:PAnsiChar):Pointer; stdcall; external 'OpenGL32.dll';
{$ENDIF}

Function SearchOpenGLProc(Const ProcName:TERRAString):Pointer;
Begin
  Result := Nil;

  {$IFDEF WINDOWS}
  Result := wglGetProcAddress(PAnsiChar(ProcName));
  {$ENDIF}

  {$IFDEF LINUX}
  If (Application.Instance.GetOption('glx')='1') Then
    Result := glxGetProcAddress(PAnsiChar(ProcName));
  {$ENDIF}


  If Result = Nil Then
    Result := GetProcAddress(OpenGLHandle, PAnsiChar(ProcName));
End;

Function glGetProcAddress(Proc:AnsiString; Alt:AnsiString=''):Pointer;{$IFDEF WINDOWS}StdCall;{$ELSE}cdecl;{$ENDIF}
Begin
  Result := SearchOpenGLProc(PAnsiChar(Proc));
  If Not Assigned(Result) Then
    Result := SearchOpenGLProc(PAnsiChar(Proc+'ARB'));

  If (Not Assigned(Result)) And (Alt<>'#') Then
    If Alt<>'' Then
      Result := SearchOpenGLProc(Alt)
    Else
      Log(logWarning,'GL', 'Function '+Proc+' not avaliable.');
End;

Procedure LoadOpenGL();
Var
   LibName:String;
Begin
  {$IFDEF DELPHI}
  Set8087CW($133F);
  {$ELSE}
  {$IFNDEF OSX}
  SetExceptionMask([exInvalidOp, exDenormalized, {exZeroDivide, }exOverflow, exUnderflow, exPrecision]);
  {$ENDIf}
  {$ENDIF}

  LibName :=  OpenGLLibName + Application.Instance.GetOption('gllib');

  Log(logDebug, 'GL', 'Loading openGL library from '+LibName);

  {$IFDEF WINDOWS}
  OpenGLHandle := LoadLibraryA(PAnsiChar(LibName));
  {$ELSE}
  OpenGLHandle := LoadLibrary(LibName);
  {$ENDIF}

  If OpenGLHandle=0 Then
  Begin
    RaiseError('Error loading OpenGL');
    Exit;
  End;
End;

Procedure glLoadExtensions;
Begin
  Log(logDebug, 'OpenGL', 'Loading extensions');

  // base OpenGL
	glAccum := glGetProcAddress('glAccum');
	glAlphaFunc := glGetProcAddress('glAlphaFunc');
	glArrayElement := glGetProcAddress('glArrayElement');
	glBegin := glGetProcAddress('glBegin');
	glBindTexture := glGetProcAddress('glBindTexture');
	glBlendFunc := glGetProcAddress('glBlendFunc');
	glCallList := glGetProcAddress('glCallList');
	glCallLists := glGetProcAddress('glCallLists');
	glClear := glGetProcAddress('glClear');
	glClearAccum := glGetProcAddress('glClearAccum');
	glClearColor := glGetProcAddress('glClearColor');
	glClearDepth := glGetProcAddress('glClearDepth');
	glClearIndex := glGetProcAddress('glClearIndex');
	glClearStencil := glGetProcAddress('glClearStencil');
	glClipPlane := glGetProcAddress('glClipPlane');
	glColor3b := glGetProcAddress('glColor3b');
	glColor3bv := glGetProcAddress('glColor3bv');
	glColor3d := glGetProcAddress('glColor3d');
	glColor3dv := glGetProcAddress('glColor3dv');
	glColor3f := glGetProcAddress('glColor3f');
	glColor3fv := glGetProcAddress('glColor3fv');
	glColor3i := glGetProcAddress('glColor3i');
	glColor3iv := glGetProcAddress('glColor3iv');
	glColor3s := glGetProcAddress('glColor3s');
	glColor3sv := glGetProcAddress('glColor3sv');
	glColor3ub := glGetProcAddress('glColor3ub');
	glColor3ubv := glGetProcAddress('glColor3ubv');
	glColor3ui := glGetProcAddress('glColor3ui');
	glColor3uiv := glGetProcAddress('glColor3uiv');
	glColor3us := glGetProcAddress('glColor3us');
	glColor3usv := glGetProcAddress('glColor3usv');
	glColor4b := glGetProcAddress('glColor4b');
	glColor4bv := glGetProcAddress('glColor4bv');
	glColor4d := glGetProcAddress('glColor4d');
	glColor4dv := glGetProcAddress('glColor4dv');
	glColor4f := glGetProcAddress('glColor4f');
	glColor4fv := glGetProcAddress('glColor4fv');
	glColor4i := glGetProcAddress('glColor4i');
	glColor4iv := glGetProcAddress('glColor4iv');
	glColor4s := glGetProcAddress('glColor4s');
	glColor4sv := glGetProcAddress('glColor4sv');
	glColor4ub := glGetProcAddress('glColor4ub');
	glColor4ubv := glGetProcAddress('glColor4ubv');
	glColor4ui := glGetProcAddress('glColor4ui');
	glColor4uiv := glGetProcAddress('glColor4uiv');
	glColor4us := glGetProcAddress('glColor4us');
	glColor4usv := glGetProcAddress('glColor4usv');
	glColorMask := glGetProcAddress('glColorMask');
	glColorMaterial := glGetProcAddress('glColorMaterial');
	glColorPointer := glGetProcAddress('glColorPointer');
	glCopyPixels := glGetProcAddress('glCopyPixels');
	glCopyTexImage1D := glGetProcAddress('glCopyTexImage1D');
	glCopyTexImage2D := glGetProcAddress('glCopyTexImage2D');
	glCopyTexSubImage1D := glGetProcAddress('glCopyTexSubImage1D');
	glCopyTexSubImage2D := glGetProcAddress('glCopyTexSubImage2D');
	glCullFace := glGetProcAddress('glCullFace');
	glDeleteLists := glGetProcAddress('glDeleteLists');
	glDeleteTextures := glGetProcAddress('glDeleteTextures');
	glDepthFunc := glGetProcAddress('glDepthFunc');
	glDepthMask := glGetProcAddress('glDepthMask');
	glDepthRange := glGetProcAddress('glDepthRange');
	glDisable := glGetProcAddress('glDisable');
	glDisableClientState := glGetProcAddress('glDisableClientState');
	glDrawArrays := glGetProcAddress('glDrawArrays');
	glDrawBuffer := glGetProcAddress('glDrawBuffer');
	glDrawElements := glGetProcAddress('glDrawElements');
	glDrawPixels := glGetProcAddress('glDrawPixels');
	glEnable := glGetProcAddress('glEnable');
	glEnableClientState := glGetProcAddress('glEnableClientState');
	glEnd := glGetProcAddress('glEnd');
	glEndList := glGetProcAddress('glEndList');
	glFinish := glGetProcAddress('glFinish');
	glFlush := glGetProcAddress('glFlush');
	glFogf := glGetProcAddress('glFogf');
	glFogfv := glGetProcAddress('glFogfv');
	glFogi := glGetProcAddress('glFogi');
	glFogiv := glGetProcAddress('glFogiv');
	glFrontFace := glGetProcAddress('glFrontFace');
	glFrustum := glGetProcAddress('glFrustum');
	glGenLists := glGetProcAddress('glGenLists');
	glGenTextures := glGetProcAddress('glGenTextures');
	glGetBooleanv := glGetProcAddress('glGetBooleanv');
	glGetClipPlane := glGetProcAddress('glGetClipPlane');
	glGetDoublev := glGetProcAddress('glGetDoublev');
	glGetError := glGetProcAddress('glGetError');
	glGetFloatv := glGetProcAddress('glGetFloatv');
	glGetIntegerv := glGetProcAddress('glGetIntegerv');
	glGetLightfv := glGetProcAddress('glGetLightfv');
	glGetLightiv := glGetProcAddress('glGetLightiv');
	glGetMapdv := glGetProcAddress('glGetMapdv');
	glGetMapfv := glGetProcAddress('glGetMapfv');
	glGetMapiv := glGetProcAddress('glGetMapiv');
	glGetMaterialfv := glGetProcAddress('glGetMaterialfv');
	glGetMaterialiv := glGetProcAddress('glGetMaterialiv');
	glGetPointerv := glGetProcAddress('glGetPointerv');
	glGetPolygonStipple := glGetProcAddress('glGetPolygonStipple');
	glGetString := glGetProcAddress('glGetString');
	glGetTexEnvfv := glGetProcAddress('glGetTexEnvfv');
	glGetTexEnviv := glGetProcAddress('glGetTexEnviv');
	glGetTexGendv := glGetProcAddress('glGetTexGendv');
	glGetTexGenfv := glGetProcAddress('glGetTexGenfv');
	glGetTexGeniv := glGetProcAddress('glGetTexGeniv');
	glGetTexImage := glGetProcAddress('glGetTexImage');
	glGetTexLevelParameterfv := glGetProcAddress('glGetTexLevelParameterfv');
	glGetTexLevelParameteriv := glGetProcAddress('glGetTexLevelParameteriv');
	glGetTexParameterfv := glGetProcAddress('glGetTexParameterfv');
	glGetTexParameteriv := glGetProcAddress('glGetTexParameteriv');
	glHint := glGetProcAddress('glHint');
	glIndexMask := glGetProcAddress('glIndexMask');
	glInterleavedArrays := glGetProcAddress('glInterleavedArrays');
	glIsEnabled := glGetProcAddress('glIsEnabled');
	glIsList := glGetProcAddress('glIsList');
	glIsTexture := glGetProcAddress('glIsTexture');
	glLightModelf := glGetProcAddress('glLightModelf');
	glLightModelfv := glGetProcAddress('glLightModelfv');
	glLightModeli := glGetProcAddress('glLightModeli');
	glLightModeliv := glGetProcAddress('glLightModeliv');
	glLightf := glGetProcAddress('glLightf');
	glLightfv := glGetProcAddress('glLightfv');
	glLighti := glGetProcAddress('glLighti');
	glLightiv := glGetProcAddress('glLightiv');
	glLineStipple := glGetProcAddress('glLineStipple');
	glLineWidth := glGetProcAddress('glLineWidth');
	glLoadIdentity := glGetProcAddress('glLoadIdentity');
	glLoadMatrixd := glGetProcAddress('glLoadMatrixd');
	glLoadMatrixf := glGetProcAddress('glLoadMatrixf');
	glLogicOp := glGetProcAddress('glLogicOp');
	glMaterialf := glGetProcAddress('glMaterialf');
	glMaterialfv := glGetProcAddress('glMaterialfv');
	glMateriali := glGetProcAddress('glMateriali');
	glMaterialiv := glGetProcAddress('glMaterialiv');
	glMatrixMode := glGetProcAddress('glMatrixMode');
	glMultMatrixd := glGetProcAddress('glMultMatrixd');
	glMultMatrixf := glGetProcAddress('glMultMatrixf');
	glNewList := glGetProcAddress('glNewList');
	glNormal3f := glGetProcAddress('glNormal3f');
	glNormal3fv := glGetProcAddress('glNormal3fv');
	glNormalPointer := glGetProcAddress('glNormalPointer');
	glOrtho := glGetProcAddress('glOrtho');
  glPixelStorei := glGetProcAddress('glPixelStorei');
	glPointSize := glGetProcAddress('glPointSize');
	glPolygonMode := glGetProcAddress('glPolygonMode');
	glPolygonOffset := glGetProcAddress('glPolygonOffset');
	glPolygonStipple := glGetProcAddress('glPolygonStipple');
	glPopAttrib := glGetProcAddress('glPopAttrib');
	glPopClientAttrib := glGetProcAddress('glPopClientAttrib');
	glPopMatrix := glGetProcAddress('glPopMatrix');
	glPrioritizeTextures := glGetProcAddress('glPrioritizeTextures');
	glPushAttrib := glGetProcAddress('glPushAttrib');
	glPushClientAttrib := glGetProcAddress('glPushClientAttrib');
	glPushMatrix := glGetProcAddress('glPushMatrix');
	glReadBuffer := glGetProcAddress('glReadBuffer');
	glReadPixels := glGetProcAddress('glReadPixels');
	glRenderMode := glGetProcAddress('glRenderMode');
	glRotated := glGetProcAddress('glRotated');
	glRotatef := glGetProcAddress('glRotatef');
	glScaled := glGetProcAddress('glScaled');
	glScalef := glGetProcAddress('glScalef');
	glScissor := glGetProcAddress('glScissor');
	glSelectBuffer := glGetProcAddress('glSelectBuffer');
	glShadeModel := glGetProcAddress('glShadeModel');
	glStencilFunc := glGetProcAddress('glStencilFunc');
	glStencilMask := glGetProcAddress('glStencilMask');
	glStencilOp := glGetProcAddress('glStencilOp');
	glTexCoord1d := glGetProcAddress('glTexCoord1d');
	glTexCoord1dv := glGetProcAddress('glTexCoord1dv');
	glTexCoord1f := glGetProcAddress('glTexCoord1f');
	glTexCoord1fv := glGetProcAddress('glTexCoord1fv');
	glTexCoord1i := glGetProcAddress('glTexCoord1i');
	glTexCoord1iv := glGetProcAddress('glTexCoord1iv');
	glTexCoord1s := glGetProcAddress('glTexCoord1s');
	glTexCoord1sv := glGetProcAddress('glTexCoord1sv');
	glTexCoord2d := glGetProcAddress('glTexCoord2d');
	glTexCoord2dv := glGetProcAddress('glTexCoord2dv');
	glTexCoord2f := glGetProcAddress('glTexCoord2f');
	glTexCoord2fv := glGetProcAddress('glTexCoord2fv');
	glTexCoord2i := glGetProcAddress('glTexCoord2i');
	glTexCoord2iv := glGetProcAddress('glTexCoord2iv');
	glTexCoord2s := glGetProcAddress('glTexCoord2s');
	glTexCoord2sv := glGetProcAddress('glTexCoord2sv');
	glTexCoord3d := glGetProcAddress('glTexCoord3d');
	glTexCoord3dv := glGetProcAddress('glTexCoord3dv');
	glTexCoord3f := glGetProcAddress('glTexCoord3f');
	glTexCoord3fv := glGetProcAddress('glTexCoord3fv');
	glTexCoord3i := glGetProcAddress('glTexCoord3i');
	glTexCoord3iv := glGetProcAddress('glTexCoord3iv');
	glTexCoord3s := glGetProcAddress('glTexCoord3s');
	glTexCoord3sv := glGetProcAddress('glTexCoord3sv');
	glTexCoord4d := glGetProcAddress('glTexCoord4d');
	glTexCoord4dv := glGetProcAddress('glTexCoord4dv');
	glTexCoord4f := glGetProcAddress('glTexCoord4f');
	glTexCoord4fv := glGetProcAddress('glTexCoord4fv');
	glTexCoord4i := glGetProcAddress('glTexCoord4i');
	glTexCoord4iv := glGetProcAddress('glTexCoord4iv');
	glTexCoord4s := glGetProcAddress('glTexCoord4s');
	glTexCoord4sv := glGetProcAddress('glTexCoord4sv');
	glTexCoordPointer := glGetProcAddress('glTexCoordPointer');
	glTexEnvf := glGetProcAddress('glTexEnvf');
	glTexEnvfv := glGetProcAddress('glTexEnvfv');
	glTexEnvi := glGetProcAddress('glTexEnvi');
	glTexEnviv := glGetProcAddress('glTexEnviv');
	glTexGend := glGetProcAddress('glTexGend');
	glTexGendv := glGetProcAddress('glTexGendv');
	glTexGenf := glGetProcAddress('glTexGenf');
	glTexGenfv := glGetProcAddress('glTexGenfv');
	glTexGeni := glGetProcAddress('glTexGeni');
	glTexGeniv := glGetProcAddress('glTexGeniv');
	glTexImage1D := glGetProcAddress('glTexImage1D');
	glTexImage2D := glGetProcAddress('glTexImage2D');
	glTexParameterf := glGetProcAddress('glTexParameterf');
	glTexParameterfv := glGetProcAddress('glTexParameterfv');
	glTexParameteri := glGetProcAddress('glTexParameteri');
	glTexParameteriv := glGetProcAddress('glTexParameteriv');
	glTexSubImage1D := glGetProcAddress('glTexSubImage1D');
	glTexSubImage2D := glGetProcAddress('glTexSubImage2D');
	glTranslated := glGetProcAddress('glTranslated');
	glTranslatef := glGetProcAddress('glTranslatef');
	glVertex2d := glGetProcAddress('glVertex2d');
	glVertex2dv := glGetProcAddress('glVertex2dv');
	glVertex2f := glGetProcAddress('glVertex2f');
	glVertex2fv := glGetProcAddress('glVertex2fv');
	glVertex2i := glGetProcAddress('glVertex2i');
	glVertex2iv := glGetProcAddress('glVertex2iv');
	glVertex2s := glGetProcAddress('glVertex2s');
	glVertex2sv := glGetProcAddress('glVertex2sv');
	glVertex3d := glGetProcAddress('glVertex3d');
	glVertex3dv := glGetProcAddress('glVertex3dv');
	glVertex3f := glGetProcAddress('glVertex3f');
	glVertex3fv := glGetProcAddress('glVertex3fv');
	glVertex3i := glGetProcAddress('glVertex3i');
	glVertex3iv := glGetProcAddress('glVertex3iv');
	glVertex3s := glGetProcAddress('glVertex3s');
	glVertex3sv := glGetProcAddress('glVertex3sv');
	glVertex4d := glGetProcAddress('glVertex4d');
	glVertex4dv := glGetProcAddress('glVertex4dv');
	glVertex4f := glGetProcAddress('glVertex4f');
	glVertex4fv := glGetProcAddress('glVertex4fv');
	glVertex4i := glGetProcAddress('glVertex4i');
	glVertex4iv := glGetProcAddress('glVertex4iv');
	glVertex4s := glGetProcAddress('glVertex4s');
	glVertex4sv := glGetProcAddress('glVertex4sv');
	glVertexPointer := glGetProcAddress('glVertexPointer');
	glViewport := glGetProcAddress('glViewport');

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

  {$IFDEF WINDOWS}
  wglSwapIntervalEXT := glGetProcAddress('wglSwapIntervalEXT');
  //wglGetSwapIntervalEXT := glGetProcAddress('wglGetSwapIntervalEXT');
  {$ENDIF}

{$IFDEF LINUX}
  glXSwapIntervalEXT := glGetProcAddress('glXSwapIntervalEXT');
{$ENDIF}
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

    {$IFDEF WINDOWS}
    //ExtensionsList := ExtensionsList + ' ' + PAnsiChar(wglGetExtensionsString());
    {$ENDIF}
  End;

  Result := Pos(Extension,ExtensionsList)>0;
End;

Initialization
Finalization
  FreeOpenGL;
End.
