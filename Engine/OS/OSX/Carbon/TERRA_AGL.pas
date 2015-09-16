Unit TERRA_AGL;

{$ifdef fpc}
  {$mode objfpc}
  {$Packrecords C}
  {$calling cdecl}
  {$linkframework AGL}
{$endif}

Interface
Uses MacOSAll;

// AGL API version.
Const
   AGL_VERSION_2_0 = 1;
{
** Macintosh device type.
 }

type
   TGDHandle = PInteger;
   TCGrafPtr = Pointer;

   PAGLDevice = ^TAGLDevice;
   TAGLDevice = TGDHandle;
{
** Macintosh drawable type.
 }

   PAGLDrawable = ^TAGLDrawable;
   TAGLDrawable = TCGrafPtr;
{
** AGL opaque data.
 }

   TAGLGraphicsManagerInfo = Pointer;

   TAGLPixelFormat = Pointer;

   TAGLContext = Pointer;

   TAGLPbuffer = Pointer;
   PAGLPbuffer = ^TAGLPbuffer;
{********************************************************************** }
{
** Attribute names for aglChoosePixelFormat and aglDescribePixelFormat.
 }

const
   AGL_NONE = 0;
{ choose from all available GraphicsManagers           }
   AGL_ALL_GraphicsManagerS = 1;     
{ depth of the index buffer                     }
   AGL_BUFFER_SIZE = 2;     
{ level in plane stacking                       }
   AGL_LEVEL = 3;     
{ choose an RGBA format                         }
   AGL_RGBA = 4;     
{ double buffering supported                    }
   AGL_DOUBLEBUFFER = 5;     
{ stereo buffering supported                    }
   AGL_STEREO = 6;     
{ number of aux buffers                         }
   AGL_AUX_BUFFERS = 7;     
{ number of red component bits                  }
   AGL_RED_SIZE = 8;     
{ number of green component bits                }
   AGL_GREEN_SIZE = 9;     
{ number of blue component bits                 }
   AGL_BLUE_SIZE = 10;     
{ number of alpha component bits                }
   AGL_ALPHA_SIZE = 11;     
{ number of depth bits                          }
   AGL_DEPTH_SIZE = 12;     
{ number of stencil bits                        }
   AGL_STENCIL_SIZE = 13;     
{ number of red accum bits                      }
   AGL_ACCUM_RED_SIZE = 14;     
{ number of green accum bits                    }
   AGL_ACCUM_GREEN_SIZE = 15;     
{ number of blue accum bits                     }
   AGL_ACCUM_BLUE_SIZE = 16;     
{ number of alpha accum bits                    }
   AGL_ACCUM_ALPHA_SIZE = 17;     
{
** Extended attributes
 }
{ frame buffer bits per pixel                   }
   AGL_PIXEL_SIZE = 50;     
{ never choose smaller buffers than requested   }
   AGL_MINIMUM_POLICY = 51;     
{ choose largest buffers of type requested      }
   AGL_MAXIMUM_POLICY = 52;     
{ choose an off-screen capable GraphicsManager         }
   AGL_OFFSCREEN = 53;     
{ choose a full-screen capable GraphicsManager         }
   AGL_FULLSCREEN = 54;     
{ number of multi sample buffers                }
   AGL_SAMPLE_BUFFERS_ARB = 55;
{ number of samples per multi sample buffer     }
   AGL_SAMPLES_ARB = 56;     
{ independent depth and/or stencil buffers for the aux buffer  }
   AGL_AUX_DEPTH_STENCIL = 57;     
{ color buffers store floating point pixels     }
   AGL_COLOR_FLOAT = 58;     
{ choose multisample                            }
   AGL_MULTISAMPLE = 59;     
{ choose supersample                            }
   AGL_SUPERSAMPLE = 60;     
{ request alpha filtering                       }
   AGL_SAMPLE_ALPHA = 61;     
{
** GraphicsManager management
 }
{ request GraphicsManager by ID                        }
   AGL_GraphicsManager_ID = 70;     
{ choose a single GraphicsManager for all screens      }
   AGL_SINGLE_GraphicsManager = 71;     
{ disable all failure recovery systems          }
   AGL_NO_RECOVERY = 72;     
{ choose a hardware accelerated GraphicsManager        }
   AGL_ACCELERATED = 73;     
{ choose the closest color buffer to request    }
   AGL_CLOSEST_POLICY = 74;     
{ GraphicsManager does not need failure recovery       }
   AGL_ROBUST = 75;     
{ back buffer contents are valid after swap     }
   AGL_BACKING_STORE = 76;     
{ GraphicsManager is multi-processor safe              }
   AGL_MP_SAFE = 78;     
{ can be used to render to a window             }
   AGL_WINDOW = 80;     
{ single window can span multiple screens       }
   AGL_MULTISCREEN = 81;     
{ virtual screen number                         }
   AGL_VIRTUAL_SCREEN = 82;     
{ GraphicsManager is opengl compliant                  }
   AGL_COMPLIANT = 83;    
   
   AGL_DISPLAY_MASK = 84;
    
{ can be used to render to a pbuffer            }
   AGL_PBUFFER = 90;     
{ can be used to render offline to a pbuffer	   }
   AGL_REMOTE_PBUFFER = 91;     
{
** Property names for aglDescribeGraphicsManager
 }
{ #define AGL_OFFSCREEN          53  }
{ #define AGL_FULLSCREEN         54  }
{ #define AGL_GraphicsManager_ID        70  }
{ #define AGL_ACCELERATED        73  }
{ #define AGL_ROBUST             75  }
{ #define AGL_BACKING_STORE      76  }
{ #define AGL_MP_SAFE            78  }
{ #define AGL_WINDOW             80  }
{ #define AGL_MULTISCREEN        81  }
{ #define AGL_COMPLIANT          83  }
{ #define AGL_PBUFFER            90  }
   AGL_BUFFER_MODES = 100;     
   AGL_MIN_LEVEL = 101;     
   AGL_MAX_LEVEL = 102;     
   AGL_COLOR_MODES = 103;     
   AGL_ACCUM_MODES = 104;     
   AGL_DEPTH_MODES = 105;     
   AGL_STENCIL_MODES = 106;     
   AGL_MAX_AUX_BUFFERS = 107;     
   AGL_VIDEO_MEMORY = 120;     
   AGL_TEXTURE_MEMORY = 121;     
   AGL_GraphicsManager_COUNT = 128;     
{
** Integer parameter names
 }
{ Enable or set the swap rectangle               }
   AGL_SWAP_RECT = 200;     
{ Enable or set the buffer rectangle             }
   AGL_BUFFER_RECT = 202;     
{ Enable or disable the swap async limit         }
   AGL_SWAP_LIMIT = 203;     
{ Enable or disable colormap tracking            }
   AGL_COLORMAP_TRACKING = 210;     
{ Set a colormap entry to index, r, g, b       }
   AGL_COLORMAP_ENTRY = 212;     
{ Enable or disable all rasterization            }
   AGL_RASTERIZATION = 220;     
{ 0 -> Don't sync, n -> Sync every n retrace     }
   AGL_SWAP_INTERVAL = 222;     
{ Validate state for multi-screen functionality  }
   AGL_STATE_VALIDATION = 230;     
{ Set the buffer name. Allows for multi ctx to share a buffer  }
   AGL_BUFFER_NAME = 231;     
{ Order the current context in front of all the other contexts.  }
   AGL_ORDER_CONTEXT_TO_FRONT = 232;     
{ aglGetInteger only - returns the ID of the drawable surface for the context  }
   AGL_CONTEXT_SURFACE_ID = 233;     
{ aglGetInteger only - returns the display ID(s) of all displays touched by the context, up to a maximum of 32 displays  }
   AGL_CONTEXT_DISPLAY_ID = 234;     
{ Position of OpenGL surface relative to window: 1 -> Above window, -1 -> Below Window  }
   AGL_SURFACE_ORDER = 235;     
{ Opacity of OpenGL surface: 1 -> Surface is opaque (default), 0 -> non-opaque  }
   AGL_SURFACE_OPACITY = 236;     
{ Enable or set the drawable clipping region  }
   AGL_CLIP_REGION = 254;     
{ Enable the capture of only a single display for aglFullScreen, normally disabled  }
   AGL_FS_CAPTURE_SINGLE = 255;     
{ 2 params.   Width/height of surface backing size      }
   AGL_SURFACE_BACKING_SIZE = 304;     
{ Enable or disable surface backing size override  }
   AGL_ENABLE_SURFACE_BACKING_SIZE = 305;     
{ Flag surface to candidate for deletion  }
   AGL_SURFACE_VOLATILE = 306;     
{
** Option names for aglConfigure.
 }
{ Set the size of the pixel format cache         }
   AGL_FORMAT_CACHE_SIZE = 501;     
{ Reset the pixel format cache                   }
   AGL_CLEAR_FORMAT_CACHE = 502;     
{ Whether to retain loaded GraphicsManagers in memory   }
   AGL_RETAIN_GraphicsManagerS = 503;     
{ buffer_modes  }
   AGL_MONOSCOPIC_BIT = $00000001;     
   AGL_STEREOSCOPIC_BIT = $00000002;     
   AGL_SINGLEBUFFER_BIT = $00000004;     
   AGL_DOUBLEBUFFER_BIT = $00000008;     
{ bit depths  }
   AGL_0_BIT = $00000001;     
   AGL_1_BIT = $00000002;     
   AGL_2_BIT = $00000004;     
   AGL_3_BIT = $00000008;     
   AGL_4_BIT = $00000010;     
   AGL_5_BIT = $00000020;     
   AGL_6_BIT = $00000040;     
   AGL_8_BIT = $00000080;     
   AGL_10_BIT = $00000100;     
   AGL_12_BIT = $00000200;     
   AGL_16_BIT = $00000400;     
   AGL_24_BIT = $00000800;     
   AGL_32_BIT = $00001000;     
   AGL_48_BIT = $00002000;     
   AGL_64_BIT = $00004000;     
   AGL_96_BIT = $00008000;     
   AGL_128_BIT = $00010000;     
{ color modes  }
{ 8 rgb bit/pixel,     RGB=7:0, inverse colormap          }
   AGL_RGB8_BIT = $00000001;     
{ 8-8 argb bit/pixel,  A=7:0, RGB=7:0, inverse colormap   }
   AGL_RGB8_A8_BIT = $00000002;     
{ 8 rgb bit/pixel,     B=7:6, G=5:3, R=2:0                }
   AGL_BGR233_BIT = $00000004;     
{ 8-8 argb bit/pixel,  A=7:0, B=7:6, G=5:3, R=2:0         }
   AGL_BGR233_A8_BIT = $00000008;     
{ 8 rgb bit/pixel,     R=7:5, G=4:2, B=1:0                }
   AGL_RGB332_BIT = $00000010;     
{ 8-8 argb bit/pixel,  A=7:0, R=7:5, G=4:2, B=1:0         }
   AGL_RGB332_A8_BIT = $00000020;     
{ 16 rgb bit/pixel,    R=11:8, G=7:4, B=3:0               }
   AGL_RGB444_BIT = $00000040;     
{ 16 argb bit/pixel,   A=15:12, R=11:8, G=7:4, B=3:0      }
   AGL_ARGB4444_BIT = $00000080;     
{ 8-16 argb bit/pixel, A=7:0, R=11:8, G=7:4, B=3:0        }
   AGL_RGB444_A8_BIT = $00000100;     
{ 16 rgb bit/pixel,    R=14:10, G=9:5, B=4:0              }
   AGL_RGB555_BIT = $00000200;     
{ 16 argb bit/pixel,   A=15, R=14:10, G=9:5, B=4:0        }
   AGL_ARGB1555_BIT = $00000400;     
{ 8-16 argb bit/pixel, A=7:0, R=14:10, G=9:5, B=4:0       }
   AGL_RGB555_A8_BIT = $00000800;     
{ 16 rgb bit/pixel,    R=15:11, G=10:5, B=4:0             }
   AGL_RGB565_BIT = $00001000;     
{ 8-16 argb bit/pixel, A=7:0, R=15:11, G=10:5, B=4:0      }
   AGL_RGB565_A8_BIT = $00002000;     
{ 32 rgb bit/pixel,    R=23:16, G=15:8, B=7:0             }
   AGL_RGB888_BIT = $00004000;     
{ 32 argb bit/pixel,   A=31:24, R=23:16, G=15:8, B=7:0    }
   AGL_ARGB8888_BIT = $00008000;     
{ 8-32 argb bit/pixel, A=7:0, R=23:16, G=15:8, B=7:0      }
   AGL_RGB888_A8_BIT = $00010000;     
{ 32 rgb bit/pixel,    R=29:20, G=19:10, B=9:0            }
   AGL_RGB101010_BIT = $00020000;     
{ 32 argb bit/pixel,   A=31:30  R=29:20, G=19:10, B=9:0   }
   AGL_ARGB2101010_BIT = $00040000;     
{ 8-32 argb bit/pixel, A=7:0  R=29:20, G=19:10, B=9:0     }
   AGL_RGB101010_A8_BIT = $00080000;     
{ 48 rgb bit/pixel,    R=35:24, G=23:12, B=11:0           }
   AGL_RGB121212_BIT = $00100000;     
{ 48 argb bit/pixel,   A=47:36, R=35:24, G=23:12, B=11:0  }
   AGL_ARGB12121212_BIT = $00200000;     
{ 64 rgb bit/pixel,    R=47:32, G=31:16, B=15:0           }
   AGL_RGB161616_BIT = $00400000;     
{ 64 argb bit/pixel,   A=63:48, R=47:32, G=31:16, B=15:0  }
   AGL_ARGB16161616_BIT = $00800000;     
{ 8 bit color look up table (deprecated)                  }
   AGL_INDEX8_BIT = $20000000;     
{ 16 bit color look up table (deprecated)				    }
   AGL_INDEX16_BIT = $40000000;     
{ 64 rgb bit/pixel,    half float                         }
   AGL_RGBFLOAT64_BIT = $01000000;     
{ 64 argb bit/pixel,   half float                         }
   AGL_RGBAFLOAT64_BIT = $02000000;     
{ 128 rgb bit/pixel,   ieee float                         }
   AGL_RGBFLOAT128_BIT = $04000000;     
{ 128 argb bit/pixel,  ieee float                         }
   AGL_RGBAFLOAT128_BIT = $08000000;     
{ 256 rgb bit/pixel,   ieee double                        }
   AGL_RGBFLOAT256_BIT = $10000000;     
{ 256 argb bit/pixel,  ieee double                        }
   AGL_RGBAFLOAT256_BIT = $20000000;     
{
** Error return values from aglGetError.
 }
{ no error                         }
   AGL_NO_ERROR = 0;     
{ invalid pixel format attribute   }
   AGL_BAD_ATTRIBUTE = 10000;     
{ invalid GraphicsManager property        }
   AGL_BAD_PROPERTY = 10001;     
{ invalid pixel format             }
   AGL_BAD_PIXELFMT = 10002;     
{ invalid GraphicsManager info            }
   AGL_BAD_RENDINFO = 10003;     
{ invalid context                  }
   AGL_BAD_CONTEXT = 10004;     
{ invalid drawable                 }
   AGL_BAD_DRAWABLE = 10005;     
{ invalid graphics device          }
   AGL_BAD_GDEV = 10006;     
{ invalid context state            }
   AGL_BAD_STATE = 10007;     
{ invalid numerical value          }
   AGL_BAD_VALUE = 10008;     
{ invalid share context            }
   AGL_BAD_MATCH = 10009;     
{ invalid enumerant                }
   AGL_BAD_ENUM = 10010;     
{ invalid offscreen drawable       }
   AGL_BAD_OFFSCREEN = 10011;     
{ invalid offscreen drawable       }
   AGL_BAD_FULLSCREEN = 10012;     
{ invalid window                   }
   AGL_BAD_WINDOW = 10013;     
{ invalid pointer                  }
   AGL_BAD_POINTER = 10014;     
{ invalid code module              }
   AGL_BAD_MODULE = 10015;     
{ memory allocation failure        }
   AGL_BAD_ALLOC = 10016;     
{ invalid CoreGraphics connection  }
   AGL_BAD_CONNECTION = 10017;     
{********************************************************************** }
{
** Pixel format functions
 }
(* Const before type ignored *)
(* Const before type ignored *)

function aglChoosePixelFormat(gdevs:PAGLDevice; ndev:Integer; attribs:PInteger):TAGLPixelFormat;cdecl;external;
function aglCreatePixelFormat(attribs:PInteger):TAGLPixelFormat;cdecl;external;

procedure aglDestroyPixelFormat(pix:TAGLPixelFormat);cdecl;external;

function aglNextPixelFormat(pix:TAGLPixelFormat):TAGLPixelFormat;cdecl;external;

function aglDescribePixelFormat(pix:TAGLPixelFormat; attrib:Integer; value:PInteger):Boolean;cdecl;external;

function aglDevicesOfPixelFormat(pix:TAGLPixelFormat; ndevs:Integer):PAGLDevice;cdecl;external;

function aglSetWindowRef(ctx:TAGLContext;  window:WindowRef):Boolean;cdecl;external;

{
** GraphicsManager information functions
 }
(* Const before type ignored *)
function aglQueryGraphicsManagerInfo(gdevs:PAGLDevice; ndev:Integer):TAGLGraphicsManagerInfo;cdecl;external;

procedure aglDestroyGraphicsManagerInfo(rend:TAGLGraphicsManagerInfo);cdecl;external;

function aglNextGraphicsManagerInfo(rend:TAGLGraphicsManagerInfo):TAGLGraphicsManagerInfo;cdecl;external;

function aglDescribeGraphicsManager(rend:TAGLGraphicsManagerInfo; prop:Integer; value:Integer):Boolean;cdecl;external;

{
** Context functions
 }
function aglCreateContext(pix:TAGLPixelFormat; share:TAGLContext):TAGLContext;cdecl;external;

function aglDestroyContext(ctx:TAGLContext):Boolean;cdecl;external;

function aglCopyContext(src:TAGLContext; dst:TAGLContext; mask:Cardinal):Boolean;cdecl;external;

function aglUpdateContext(ctx:TAGLContext):Boolean;cdecl;external;

{
** Current state functions
 }
function aglSetCurrentContext(ctx:TAGLContext):Boolean;cdecl;external;

function aglGetCurrentContext:TAGLContext;cdecl;external;

{
** Drawable Functions
 }
function aglSetDrawable(ctx:TAGLContext; draw:TAGLDrawable):Boolean;cdecl;external;

function aglSetOffScreen(ctx:TAGLContext; width:Integer; height:Integer; rowbytes:Integer; baseaddr:Pointer):Boolean;cdecl;external;

function aglSetFullScreen(ctx:TAGLContext; width:Integer; height:Integer; freq:Integer; device:Integer):Boolean;cdecl;external;

function aglGetDrawable(ctx:TAGLContext):TAGLDrawable;cdecl;external;

{
** Virtual screen functions
 }
function aglSetVirtualScreen(ctx:TAGLContext; screen:Integer):Boolean;cdecl;external;

function aglGetVirtualScreen(ctx:TAGLContext):Integer;cdecl;external;

{
** Obtain version numbers
 }
procedure aglGetVersion(Var major, minor:Integer);cdecl;external;

{
** Global library options
 }
function aglConfigure(pname:Integer; param:Cardinal):Boolean;cdecl;external;

{
** Swap functions
 }
procedure aglSwapBuffers(ctx:TAGLContext);cdecl;external;

{
** Per context options
 }
function aglEnable(ctx:TAGLContext; pname:Integer):Boolean;cdecl;external;

function aglDisable(ctx:TAGLContext; pname:Integer):Boolean;cdecl;external;

function aglIsEnabled(ctx:TAGLContext; pname:Integer):Boolean;cdecl;external;

(* Const before type ignored *)
function aglSetInteger(ctx:TAGLContext; pname:Integer; params:PInteger):Boolean;cdecl;external;

function aglGetInteger(ctx:TAGLContext; pname:Integer; params:PInteger):Boolean;cdecl;external;

{
** Font function
 }
function aglUseFont(ctx:TAGLContext; fontID:Integer; face:Byte; size:Integer; first:Integer;
           count:Integer; base:Integer):Boolean;cdecl;external;

{
** Error functions
 }
function aglGetError:Integer;cdecl;external;

(* Const before type ignored *)
function aglErrorString(code:Integer):PAnsiChar;cdecl;external;

{
** Soft reset function
 }
procedure aglResetLibrary;cdecl;external;

{
** Surface texture function
 }
procedure aglSurfaceTexture(context:TAGLContext; target:Integer; internalformat:Integer; surfacecontext:TAGLContext);cdecl;external;

{
** PBuffer functions
 }
function aglCreatePBuffer(width:Integer; height:Integer; target:Integer; internalFormat:Integer; max_level:longint;
           pbuffer:PAGLPbuffer):Boolean;cdecl;external;

function aglDestroyPBuffer(pbuffer:TAGLPbuffer):Boolean;cdecl;external;

function aglDescribePBuffer(pbuffer:TAGLPbuffer; Var width:Integer; Var height:Integer; Var target:Integer; Var internalFormat:Integer;
           Var max_level:Integer):Boolean;cdecl;external;

function aglTexImagePBuffer(ctx:TAGLContext; pbuffer:TAGLPbuffer; source:Integer):boolean;cdecl;external;

{
** Pbuffer Drawable Functions
 }
function aglSetPBuffer(ctx:TAGLContext; pbuffer:TAGLPbuffer; face:Integer; level:Integer; screen:Integer):Boolean;cdecl;external;

function aglGetPBuffer(ctx:TAGLContext; Var pbuffer:TAGLPbuffer; Var face:Integer; Var level:Integer; Var screen:Integer):Boolean;cdecl;external;

{
** CGL functions
 }
function aglGetCGLContext(ctx:TAGLContext; cgl_ctx:Ppointer):Boolean;cdecl;external;

function aglGetCGLPixelFormat(pix:TAGLPixelFormat; cgl_pix:Ppointer):Boolean;cdecl;external;

{
** CGL opaque data.
}
type
  CGLContextObj = Pointer;
  PCGLContextObj = ^CGLContextObj;
  CGLPixelFormatObj = Pointer;
  PCGLPixelFormatObj = ^CGLPixelFormatObj;
  CGLGraphicsManagerInfoObj = Pointer;
  CGLPBufferObj = Pointer;

{
** Attribute names for CGLChoosePixelFormat and CGLDescribePixelFormat.
}
  CGLPixelFormatAttribute = (
	kCGLPFAAllGraphicsManagers       =   1,	{ choose from all available GraphicsManagers          }
	kCGLPFADoubleBuffer       =   5,	{ choose a double buffered pixel format        }
	kCGLPFAStereo             =   6,	{ stereo buffering supported                   }
	kCGLPFAAuxBuffers         =   7,	{ number of aux buffers                        }
	kCGLPFAColorSize          =   8,	{ number of color buffer bits                  }
	kCGLPFAAlphaSize          =  11,	{ number of alpha component bits               }
	kCGLPFADepthSize          =  12,	{ number of depth buffer bits                  }
	kCGLPFAStencilSize        =  13,	{ number of stencil buffer bits                }
	kCGLPFAAccumSize          =  14,	{ number of accum buffer bits                  }
	kCGLPFAMinimumPolicy      =  51,	{ never choose smaller buffers than requested  }
	kCGLPFAMaximumPolicy      =  52,	{ choose largest buffers of type requested     }
	kCGLPFAOffScreen          =  53,	{ choose an off-screen capable GraphicsManager        }
	kCGLPFAFullScreen         =  54,	{ choose a full-screen capable GraphicsManager        }
	kCGLPFASampleBuffers      =  55,	{ number of multi sample buffers               }
	kCGLPFASamples            =  56,	{ number of samples per multi sample buffer    }
	kCGLPFAAuxDepthStencil    =  57,	{ each aux buffer has its own depth stencil    }
	kCGLPFAColorFloat         =  58,	{ color buffers store floating point pixels    }
	kCGLPFAMultisample        =  59,	{ choose multisampling                         }
	kCGLPFASupersample        =  60,	{ choose supersampling                         }
	kCGLPFASampleAlpha        =  61,	{ request alpha filtering                      }

	kCGLPFAGraphicsManagerID         =  70,	{ request GraphicsManager by ID                       }
	kCGLPFASingleGraphicsManager     =  71,	{ choose a single GraphicsManager for all screens     }
	kCGLPFANoRecovery         =  72,	{ disable all failure recovery systems         }
	kCGLPFAAccelerated        =  73,	{ choose a hardware accelerated GraphicsManager       }
	kCGLPFAClosestPolicy      =  74,	{ choose the closest color buffer to request   }
	kCGLPFARobust             =  75,	{ GraphicsManager does not need failure recovery      }
	kCGLPFABackingStore       =  76,	{ back buffer contents are valid after swap    }
	kCGLPFAMPSafe             =  78,	{ GraphicsManager is multi-processor safe             }
	kCGLPFAWindow             =  80,	{ can be used to render to an onscreen window  }
	kCGLPFAMultiScreen        =  81,	{ single window can span multiple screens      }
	kCGLPFACompliant          =  83,	{ GraphicsManager is opengl compliant                 }
	kCGLPFADisplayMask        =  84,	{ mask limiting supported displays             }
	kCGLPFAPBuffer            =  90,	{ can be used to render to a pbuffer           }
	kCGLPFARemotePBuffer      =  91,	{ can be used to render offline to a pbuffer   }
	kCGLPFAVirtualScreenCount = 128 	{ number of virtual screens in this format     }
);
  PCGLPixelFormatAttribute = ^CGLPixelFormatAttribute;
{
** Property names for CGLDescribeGraphicsManager.
}
  CGLGraphicsManagerProperty = (
	kCGLRPOffScreen           =  53,
	kCGLRPFullScreen          =  54,
	kCGLRPGraphicsManagerID          =  70,
	kCGLRPAccelerated         =  73,
	kCGLRPRobust              =  75,
	kCGLRPBackingStore        =  76,
	kCGLRPMPSafe              =  78,
	kCGLRPWindow              =  80,
	kCGLRPMultiScreen         =  81,
	kCGLRPCompliant           =  83,
	kCGLRPDisplayMask         =  84,
	kCGLRPBufferModes         = 100,	{ a bitfield of supported buffer modes          }
	kCGLRPColorModes          = 103,	{ a bitfield of supported color buffer formats  }
	kCGLRPAccumModes          = 104,	{ a bitfield of supported accum buffer formats  }
	kCGLRPDepthModes          = 105,	{ a bitfield of supported depth buffer depths   }
	kCGLRPStencilModes        = 106,	{ a bitfield of supported stencil buffer depths }
	kCGLRPMaxAuxBuffers       = 107,	{ maximum number of auxilliary buffers          }
	kCGLRPMaxSampleBuffers    = 108,	{ maximum number of sample buffers              }
	kCGLRPMaxSamples          = 109,	{ maximum number of samples                     }
	kCGLRPSampleModes         = 110,	{ a bitfield of supported sample modes          }
	kCGLRPSampleAlpha         = 111,	{ support for alpha sampling                    }
	kCGLRPVideoMemory         = 120,	{ total video memory                            }
	kCGLRPTextureMemory       = 121,	{ video memory useable for texture storage      }
	kCGLRPGPUVertProcCapable  = 122,	{ GraphicsManager capable of GPU vertex processing     }
	kCGLRPGPUFragProcCapable  = 123,	{ GraphicsManager capable of GPU fragment processing   }
	kCGLRPGraphicsManagerCount       = 128 	{ the number of GraphicsManagers in this GraphicsManager info }
);

{
** Enable names for CGLEnable, CGLDisable, and CGLIsEnabled.
}
  CGLContextEnable = (
	kCGLCESwapRectangle    = 201,	{ Enable or disable the swap rectangle          }
	kCGLCESwapLimit        = 203,	{ Enable or disable the swap async limit        }
	kCGLCERasterization    = 221,	{ Enable or disable all rasterization           }
	kCGLCEStateValidation  = 301,	{ Validate state for multi-screen functionality }
	kCGLCESurfaceBackingSize = 305, { Enable or disable surface backing size override }
	kCGLCEDisplayListOptimization = 307,  { Ability to turn off display list optimizer }
	kCGLCEMPEngine = 313            { Enable or disable multi-threaded GL engine    }
);

{
** Parameter names for CGLSetParameter and CGLGetParameter.
}
  CGLContextParameter = (
	kCGLCPSwapRectangle     = 200,  { 4 params.  Set or get the swap rectangle (x, y, w, h)  }
	kCGLCPSwapInterval      = 222,  { 1 param.   0 -> Don't sync, n -> Sync every n retrace  }
	kCGLCPDispatchTableSize = 224,  { 1 param.   Get the dispatch table size                 }
	{ Note: kCGLCPClientStorage is always a pointer-sized parameter, even though the API claims GLint. }
	kCGLCPClientStorage     = 226,  { 1 param.   Context specific generic storage            }
	kCGLCPSurfaceTexture    = 228,  { 3 params.  SID, target, internal_format                }
{  - Used by AGL - }
{  AGL_STATE_VALIDATION     230    }
{  AGL_BUFFER_NAME          231    }
{  AGL_ORDER_CONTEXT_TO_FRONT  232 }
{  AGL_CONTEXT_SURFACE_ID   233    }
{  AGL_CONTEXT_DISPLAY_ID   234    }
	kCGLCPSurfaceOrder      = 235,  { 1 param.   1 -> Above window, -1 -> Below Window       }
	kCGLCPSurfaceOpacity    = 236,  { 1 param.   1 -> Surface is opaque (default), 0 -> non-opaque }
{  - Used by AGL - }
{  AGL_CLIP_REGION          254   }
{  AGL_FS_CAPTURE_SINGLE    255   }
	kCGLCPSurfaceBackingSize = 304,  	{ 2 params.   Width/height of surface backing size     }
{ AGL_SURFACE_VOLATILE		 306    }
	kCGLCPSurfaceSurfaceVolatile = 306,	{ 1 param.   Surface volatile state                                     }
	kCGLCPReclaimResources		 = 308,	{ 0 params.  }
	kCGLCPCurrentGraphicsManagerID      = 309,	{ 1 param.   Retrieves the current GraphicsManager ID         }
	kCGLCPGPUVertexProcessing	 = 310, { 1 param.   Currently processing vertices with GPU (get) }
	kCGLCPGPUFragmentProcessing	 = 311 { 1 param.   Currently processing fragments with GPU (get) }
);

{
** Option names for CGLSetOption and CGLGetOption.
}
  CGLGlobalOption = (
	kCGLGOFormatCacheSize  = 501,	{ Set the size of the pixel format cache        }
	kCGLGOClearFormatCache = 502,	{ Reset the pixel format cache if true          }
	kCGLGORetainGraphicsManagers  = 503,	{ Whether to retain loaded GraphicsManagers in memory  }
	kCGLGOResetLibrary     = 504,	{ *** DEPRECATED in MacOS X 10.4 ***            }
	                             	{ Do a soft reset of the CGL library if true    }
	kCGLGOUseErrorHandler  = 505	{ Call the Core Graphics handler on CGL errors  }
);

{
** Error return values from CGLGetError.
}
  CGLError = (
	kCGLNoError            = 0,     { no error }
	kCGLBadAttribute       = 10000,	{ invalid pixel format attribute  }
	kCGLBadProperty        = 10001,	{ invalid GraphicsManager property       }
	kCGLBadPixelFormat     = 10002,	{ invalid pixel format            }
	kCGLBadGraphicsManagerInfo    = 10003,	{ invalid GraphicsManager info           }
	kCGLBadContext         = 10004,	{ invalid context                 }
	kCGLBadDrawable        = 10005,	{ invalid drawable                }
	kCGLBadDisplay         = 10006,	{ invalid graphics device         }
	kCGLBadState           = 10007,	{ invalid context state           }
	kCGLBadValue           = 10008,	{ invalid numerical value         }
	kCGLBadMatch           = 10009,	{ invalid share context           }
	kCGLBadEnumeration     = 10010,	{ invalid enumerant               }
	kCGLBadOffScreen       = 10011,	{ invalid offscreen drawable      }
	kCGLBadFullScreen      = 10012,	{ invalid offscreen drawable      }
	kCGLBadWindow          = 10013,	{ invalid window                  }
	kCGLBadAddress         = 10014,	{ invalid pointer                 }
	kCGLBadCodeModule      = 10015,	{ invalid code module             }
	kCGLBadAlloc           = 10016,	{ invalid memory allocation       }
	kCGLBadConnection      = 10017 	{ invalid CoreGraphics connection }
);


{
** Buffer modes
}
const
  kCGLMonoscopicBit  = $00000001;
  kCGLStereoscopicBit= $00000002;
  kCGLSingleBufferBit= $00000004;
  kCGLDoubleBufferBit= $00000008;

{
** Depth and stencil buffer depths
}
  kCGL0Bit           = $00000001;
  kCGL1Bit           = $00000002;
  kCGL2Bit           = $00000004;
  kCGL3Bit           = $00000008;
  kCGL4Bit           = $00000010;
  kCGL5Bit           = $00000020;
  kCGL6Bit           = $00000040;
  kCGL8Bit           = $00000080;
  kCGL10Bit          = $00000100;
  kCGL12Bit          = $00000200;
  kCGL16Bit          = $00000400;
  kCGL24Bit          = $00000800;
  kCGL32Bit          = $00001000;
  kCGL48Bit          = $00002000;
  kCGL64Bit          = $00004000;
  kCGL96Bit          = $00008000;
  kCGL128Bit         = $00010000;

{
** Color and accumulation buffer formats.
}
  kCGLRGB444Bit      = $00000040;  { 16 rgb bit/pixel,    R=11:8, G=7:4, B=3:0              }
  kCGLARGB4444Bit    = $00000080;  { 16 argb bit/pixel,   A=15:12, R=11:8, G=7:4, B=3:0     }
  kCGLRGB444A8Bit    = $00000100;  { 8-16 argb bit/pixel, A=7:0, R=11:8, G=7:4, B=3:0       }
  kCGLRGB555Bit      = $00000200;  { 16 rgb bit/pixel,    R=14:10, G=9:5, B=4:0             }
  kCGLARGB1555Bit    = $00000400;  { 16 argb bit/pixel,   A=15, R=14:10, G=9:5, B=4:0       }
  kCGLRGB555A8Bit    = $00000800;  { 8-16 argb bit/pixel, A=7:0, R=14:10, G=9:5, B=4:0      }
  kCGLRGB565Bit      = $00001000;  { 16 rgb bit/pixel,    R=15:11, G=10:5, B=4:0            }
  kCGLRGB565A8Bit    = $00002000;  { 8-16 argb bit/pixel, A=7:0, R=15:11, G=10:5, B=4:0     }
  kCGLRGB888Bit      = $00004000;  { 32 rgb bit/pixel,    R=23:16, G=15:8, B=7:0            }
  kCGLARGB8888Bit    = $00008000;  { 32 argb bit/pixel,   A=31:24, R=23:16, G=15:8, B=7:0   }
  kCGLRGB888A8Bit    = $00010000;  { 8-32 argb bit/pixel, A=7:0, R=23:16, G=15:8, B=7:0     }
  kCGLRGB101010Bit   = $00020000;  { 32 rgb bit/pixel,    R=29:20, G=19:10, B=9:0           }
  kCGLARGB2101010Bit = $00040000;  { 32 argb bit/pixel,   A=31:30  R=29:20, G=19:10, B=9:0  }
  kCGLRGB101010_A8Bit= $00080000;  { 8-32 argb bit/pixel, A=7:0  R=29:20, G=19:10, B=9:0    }
  kCGLRGB121212Bit   = $00100000;  { 48 rgb bit/pixel,    R=35:24, G=23:12, B=11:0          }
  kCGLARGB12121212Bit= $00200000;  { 48 argb bit/pixel,   A=47:36, R=35:24, G=23:12, B=11:0 }
  kCGLRGB161616Bit   = $00400000;  { 64 rgb bit/pixel,    R=63:48, G=47:32, B=31:16         }
  kCGLRGBA16161616Bit= $00800000;  { 64 argb bit/pixel,   R=63:48, G=47:32, B=31:16, A=15:0 }
  kCGLRGBFloat64Bit  = $01000000;  { 64 rgb bit/pixel,    half float                        }
  kCGLRGBAFloat64Bit = $02000000;  { 64 argb bit/pixel,   half float                        }
  kCGLRGBFloat128Bit = $04000000;  { 128 rgb bit/pixel,   ieee float                        }
  kCGLRGBAFloat128Bit= $08000000;  { 128 argb bit/pixel,  ieee float                        }
  kCGLRGBFloat256Bit = $10000000;  { 256 rgb bit/pixel,   ieee double                       }
  kCGLRGBAFloat256Bit= $20000000;  { 256 argb bit/pixel,  ieee double                       }

{
** Sampling modes
}
  kCGLSupersampleBit = $00000001;
  kCGLMultisampleBit = $00000002;

{ Obsolete }
  kCGLARGB16161616Bit = kCGLRGBA16161616Bit;

Implementation

End.

