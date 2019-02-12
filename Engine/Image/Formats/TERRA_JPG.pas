{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_JPG
 * Implements JPG loader/saver
 ***********************************************************************************************************************
}

Unit TERRA_JPG;

{$i terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Stream, TERRA_Image, TERRA_Log, TERRA_FileFormat, TERRA_Application, TERRA_MemoryStream;
  //sdJpegImage, sdJpegTypes, sdMapIterator;

Type
  JPEGFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:TERRAStream):Boolean; Override;
      Function LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
      Function SaveToStream(Target:TERRAObject; Dest:TERRAStream):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_Engine, TERRA_FileStream, TERRA_FileUtils, TERRA_Color, TERRA_Math;

Type
  // Supported encoding methods in this implementation
  TsdJpegEncodingMethod = (
    emUnspecified,
    emBaselineDCT,
    emExtendedDCT,
    emProgressiveDCT
  );

  TsdZigZagArray = array[0..63 + 16] of byte;
  PsdZigZagArray = ^TsdZigZagArray;
  TsdIntArray64 = array[0..63] of integer;

  TsdCoefBlock = array[0..63] of smallint;
  PsdCoefBlock = ^TsdCoefBlock;
  TsdSampleBlock = array[0..63] of byte;
  PsdSampleBlock = ^TsdSampleBlock;


  // Used to construct a histogram (frequency count) of encoded huffman symbols
  Tsd8bitHuffmanHistogram = array[0..255] of integer;
  Psd8bitHuffmanHistogram = ^Tsd8bitHuffmanHistogram;

  // Lookup table for Huffman decoding. The Huffman code is left-aligned
  // in the table, Len indicates the number of bits to take out of the stream,
  // Value is the associated symbol. If Len = 0, Value indicates an index
  // to a follow-up table (for codelengths > 8)
  TsdHuffmanLookupTable = record
    Len:   array[0..255] of byte;
    Value: array[0..255] of smallint;
  end;
  PsdHuffmanLookupTable = ^TsdHuffmanLookupTable;
  
  TsdJpegQuality = Byte;

  TsdJpegScale = (
    jsFull,  // Read the complete image (DC + AC 1..63)
    jsDiv2,  // Read only 1/2 of the image (DC + AC 1..15)
    jsDiv4,  // Read only 1/4 of the image (DC + AC 1..3)
    jsDiv8   // Read only 1/8 of the image (DC only)
  );

  TsdQuantizationPrecision = (
    qp8bit,
    qp16bit
  );

  TsdJpegDCTCodingMethod = (
    dmFast,
    dmAccurate
  );

  TsdJpegColorSpace = (
    jcAutoDetect,   // Auto-detect the colorspace from the file
    jcGray,         // 1-Channel grayscale
    jcGrayA,        // 1-Channel grayscale with Alpha channel
    jcRGB,          // (standard) RGB
    jcRGBA,         // (standard) RGB with Alpha channel
    jcYCbCr,        // Jpeg Y-Cb-Cr
    jcYCbCrA,       // Jpeg Y-Cb-Cr with Alpha channel
    jcCMYK,         // CMYK
    jcYCbCrK,       // CMYK represented in 4 channels as YCbCrK
    jcYCCK,         // YCCK
    jcPhotoYCC,     // Photo YCC
    jcPhotoYCCA,    // Photo YCCA
    jcITUCieLAB     // ITU G3FAX CieLAB (for use in colour faxes)
  );

  TsdJpegLoadOption = (
    loOnlyMetadata, // If set, only meta-data is read (exits when SOS is encountered)
    loTileMode      // If set, the loadfromstream only finds the start of each MCU tile
  );
  TsdJpegLoadOptions = set of TsdJpegLoadOption;

  // Huffman code
  TsdHuffmanCode = record
    L: integer;    // Symbol length
    Code: integer; // Associated huffman code
    V: integer;    // Value for huffman code
  end;
  PsdHuffmanCode = ^TsdHuffmanCode;

  // Huffman table values + codes specified in DHT marker
  TsdHuffmanTable = class(TERRAObject)
  private
    FItems: array of TsdHuffmanCode;
    function GetItems(Index: integer): PsdHuffmanCode;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
  public
    property Items[Index: integer]: PsdHuffmanCode read GetItems; default;
    property Count: integer read GetCount write SetCount;
  end;

  TsdHuffmanTableList = class(TERRAObject)
  private
    _Tables:Array Of TsdHuffmanTable;
    _Count:Integer;

    function GetItems(Index: integer): TsdHuffmanTable;
  public
    Procedure Release; Override;
    Procedure Clear;
    property Items[Index: integer]: TsdHuffmanTable read GetItems; default;
    Property Count:Integer Read _Count;
  end;

  // Quantization table specified in DQT marker
  TsdQuantizationTable = class(TERRAObject)
  public
    // Quantization values Q
    FQuant: array[0..63] of word;
    // Precision P
    FPrecision: TsdQuantizationPrecision;
    // transpose
    procedure Transpose;
  end;

  TsdQuantizationTableList = Class(TERRAObject)
  private
    _Tables:Array Of TsdQuantizationTable;
    _Count:Integer;

    function GetItems(Index: integer): TsdQuantizationTable;
  public
    Procedure Release; Override;

    Procedure Clear();
    property Items[Index: integer]: TsdQuantizationTable read GetItems; default;
    Property Count:Integer Read _Count;
  end;

  // Frame component specified in SOF marker
  TsdFrameComponent = class(TERRAObject)
  public
    // Horizontal sampling factor H
    FHorzSampling: integer;
    // Vertical sampling factor V
    FVertSampling: integer;
    // Component identifier C (can be ascii)
    FComponentID: integer;
    // Quantization table destination Tq
    FQTable: integer;
  end;

  TsdFrameComponentList = class(TERRAObject)
  private
    _Frames:Array Of TsdFrameComponent;
    _Count:Integer;

    function GetItems(Index: integer): TsdFrameComponent;
  public
    Procedure Release; Override;

    Procedure Clear();

    property Items[Index: integer]: TsdFrameComponent read GetItems; default;
    Property Count:Integer Read _Count;
  end;

  // Scan component specified in SOS marker
  TsdScanComponent = class(TERRAObject)
  public
    // Index into frame components list, Cidx
    FComponent: integer;
    // DC entropy table destination Td
    FDCTable: integer;
    // AC entropy table destination Ta
    FACTable: integer;
    // Used as predictor in DC coding
    FPredictor: smallint;
  end;

  TsdScanComponentList = class(TERRAObject)
  private
    _Scans:Array Of TsdScanComponent;
    _Count:Integer;

    function GetItems(Index: integer): TsdScanComponent;
  public
    Procedure Release; Override;

    Procedure Clear();
    property Items[Index: integer]: TsdScanComponent read GetItems; default;
    Property Count:Integer Read _Count;
  end;


  // Collected component data from markers
  TsdJpegInfo = class(TERRAObject)
  public
    // Repository of tables, are updated after DQT or DHT markers
    FDCHuffmanTables: TsdHuffmanTableList;
    FACHuffmanTables: TsdHuffmanTableList;
    FQuantizationTables: TsdQuantizationTableList;
    // List of frames
    FFrames: TsdFrameComponentList;
    // List of scans
    FScans: TsdScanComponentList;
    // Number of image components in frame, Nf
    FFrameCount: integer;
    // Number of image components in scan, Ns
    FScanCount: integer;
    // Maximum of all H_i in current scan, Hmax
    FHorzSamplingMax: integer;
    // Maximum of all V_i in current scan, Vmax
    FVertSamplingMax: integer;
    // Restart interval MCU count (0 means disabled), updated after RST marker, Ri
    FRestartInterval: integer;
    // Image width, X
    FWidth: integer;
    // Image Height, Y
    FHeight: integer;
    // Jpeg encoding method
    FEncodingMethod: TsdJpegEncodingMethod;
    // Sample precision in bits for samples, P;
    FSamplePrecision: integer;
    // Start of spectral selection, Ss
    FSpectralStart: integer;
    // End of spectral selection, Se
    FSpectralEnd: integer;
    // Succ Approximation high bitpos, Ah
    FApproxHigh: integer;
    // Succ Approximation low bitpos, Al
    FApproxLow: integer;
    // Width of the MCU block in pixels
    FMcuWidth: integer;
    // Height of the MCU block in pixels
    FMcuHeight: integer;
    // Horizontal MCU count
    FHorzMcuCount: integer;
    // Vertical MCU count
    FVertMcuCount: integer;
    //
    FWaitForDNL: boolean;
    // Width of a tile in pixels during TileMode
    FTileWidth: integer;
    // Height of a tile in pixels during TileMode
    FTileHeight: integer;
    //

    Constructor Create; virtual;
    Procedure Release; Override;
    Procedure Clear;
  End;

  TsdIteratorMethod = (
    imReaderX,    // From x=0  , y=0   to x=W-1, y=0  , then to x=0  , y=1   and on
    imReaderXInv, // From x=W-1, y=0   to x=0  , y=0  , then to x=W-1, y=1   and on
    imReaderXBtm, // From x=0  , y=H-1 to x=W-1, y=H-1, then to X=1  , y=H-1 and on
    imReaderY,    // From x=0  , y=0   to x=0  , y=H-1, then to x=1  , y=0   and on
    imReaderYInv, // From x=0  , y=H-1 to x=0  , y=0  , then to x=1  , y=H-1 and on
    imZigZag,     // Zig zag: top line from left to right, then next from right to left, etc
    imLineByLine, // Move to next cell to the right, until end of the scanline
    imColByCol    // Move to next cell down, until last row
  );
  
  TsdMapIterator = class(TERRAObject)
  private
    FCount: integer;
    FDelta: integer;
    FLine: integer;
    FLineFirst: pbyte;
    FThis: pbyte;
    FMap: pbyte;
    FWidth: integer;
    FHeight: integer;
    FScanStride: integer;
    FCellStride: integer;
    FBitCount: integer;
    FMethod: TsdIteratorMethod;
    function IsDirect: boolean;
    function GetDirection: integer;
  protected
  public
    procedure Assign(Source:TERRAObject); 
    // Call First to get a pointer to the first cell
    function First: pbyte;
    // Call Next iteratively to get the next cell, until it returns nil
    function Next: pbyte;
    // Get the pointer to the cell at X,Y. Next does *not* work from this
    // position, the First/Next paradigm cannot be mixed.
    function At(X, Y: integer): pbyte;
    // Same thing, but checks dimensions and returns nil if not within them
    function SafeAt(X, Y: integer): pbyte;
    // integer at X, Y
    function IntAt(X, Y: integer): pinteger;
    // "single" float at X, Y
    function SingleAt(X, Y: integer): psingle;
    // Increment the Map pointer so it points to a different channel (e.g.
    // 0 for Blue, 1 for Red, 2 for Green in RGB 24bpp bitmap).
    procedure IncrementMap(AChannel: integer);
    // Distance between cell 0 of scanline 0 and cell 0 of scanline 1 in bytes
    property ScanStride: integer read FScanStride write FScanStride;
    // Distance between cell 0 and cell 1
    property CellStride: integer read FCellStride write FCellStride;
    // Number of bits for each cell, eg with PixelFormat = pf15bits, BitCount = 15.
    // This property is merely for client apps that want to exchange their individual
    // pixel data.
    property BitCount: integer read FBitCount write FBitCount;
    // Pointer to cell 0, 0
    property Map: pbyte read FMap write FMap;
    // Width of map in pixels
    property Width: integer read FWidth write FWidth;
    // Height of map in pixels
    property Height: integer read FHeight write FHeight;
    // Iterator method, see TsdIteratorMethod for an explanation.
    property Method: TsdIteratorMethod read FMethod write FMethod;
    // Current line number (if multi-line iteration)
    property Line: integer read FLine;
    // Current direction (if zig-zag)
    property Direction: integer read GetDirection;
  end;

  // Abstract color transform
  TsdColorTransform = class(TERRAObject)
  public
    constructor Create; virtual;
    // Transform Count colors from Source to Dest
    procedure Transform(Source, Dest: pointer; Count: integer); virtual; abstract;
    // Source cellstride (#bytes per cell)
    function SrcCellStride: integer; virtual; abstract;
    // Dest cellstride (#bytes per cell)
    function DstCellStride: integer; virtual; abstract;
  end;

  // Transform class
  TsdColorTransformClass = class of TsdColorTransform;

  // Null transform: 8bit/pixel direct copy.
  TsdNullTransform8bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Null transform: 16bit/pixel direct copy.
  TsdNullTransform16bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Null transform: 24bit/pixel direct copy.
  TsdNullTransform24bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Null transform: 32bit/pixel direct copy.
  TsdNullTransform32bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Abstract JFIF transform (transforms used by JPEG's JFIF spec)
  TsdJfifTransform = class(TsdColorTransform)
  private
    FColorConvScale: integer;
    function RangeLimitDescale(A: integer): integer;
  public
    constructor Create; override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Abstract JFIF inverse transform (RGB to YCbCr space)
  TsdJfifInvTransform = class(TsdJfifTransform)
  private
    F__toCb: integer;
    F__toCr: integer;
    FRtoY_: array[0..255] of integer;
    FGtoY_: array[0..255] of integer;
    FBtoY_: array[0..255] of integer;
    FRtoCb: array[0..255] of integer;
    FGtoCb: array[0..255] of integer;
    FBtoCb: array[0..255] of integer;
    FRtoCr: array[0..255] of integer;
    FGtoCr: array[0..255] of integer;
    FBtoCr: array[0..255] of integer;
    procedure InitRGBToYCbCrTables;
  public
    constructor Create; override;
  end;

  // RGB (24bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TsdTransformBGRToYCbCr = class(TsdJfifInvTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGB (24bit) to Gray (8bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). It uses
  // the same formula to find Gray as in RGB->YCbCr
  TsdTransformBGRToGray = class(TsdJfifInvTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function DstCellStride: integer; override;
  end;

  // RGBA (32bit) to Y-Cb-Cr-A (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TsdTransformBGRAToYCbCrA = class(TsdJfifInvTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // RGBA (32bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel is ignored.
  TsdTransformBGRAToYCbCr = class(TsdJfifInvTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
  end;

  // Inversion transform: invert colour triplets RGB->BGR, can be used for any
  // 24bit triplet of colours that needs to be inverted in order.
  TsdTransformInvertTriplet24bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  End;

  // Gray (8bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value.
  TsdTransformGrayToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Gray + Alpha (16bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channel is ignored.
  TsdTransformGrayAToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Gray + Alpha (16bit) to BGRA (32bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channels are copied.
  TsdTransformGrayAToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // RGB (24bit) to BGRA (32bit) transform. The output alpha (A) channel is
  // set to $FF.
  TsdTransformRGBToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // RGBA (32bit) to BGR (24bit) transform. The input alpha (A) channel is ignored.
  TsdTransformRGBAToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // BGRA (32bit) to BGR (24bit) transform. This transform uses a parameter,
  // BkColor (TColor) that is used to fill the background, while the colors are
  // blended using the "over" operator. The background color by default is clWhite.
  // This routine assumes alpha pre-multiplied colors.
  TsdTransformBGRAToBGR = class(TsdColorTransform)
  private
    FBkColor: cardinal;
    procedure SetBkColor(const Value: cardinal);
    function GetBkColor: cardinal;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    property BkColor: cardinal read GetBkColor write SetBkColor;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // Abstract JFIF forward transform (YCbCr to RGB space)
  TsdJfifFwdTransform = class(TsdJfifTransform)
  private
    F__toR: integer;
    F__toG: integer;
    F__toB: integer;
    FY_toRT: array[0..255] of integer;
    FCrtoRT: array[0..255] of integer;
    FCbtoGT: array[0..255] of integer;
    FCrtoGT: array[0..255] of integer;
    FCbtoBT: array[0..255] of integer;
    procedure InitYCbCrTables;
  public
    constructor Create; override;
  end;
  
  // Y-Cb-Cr (24bit) to gray (8it) colour transform. The Y channel is used
  // as grayscale directly, Cb and Cr channels are not used.
  TsdTransformYCbCrToGray = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function DstCellStride: integer; override;
  end;

  // Y-Cb-Cr-A (32bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The input
  // alpha (A) channel is ignored.
  TsdTransformYCbCrAToBGR = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
  end;

  // YCbCrK to BGR. YCbCr is first converted to CMY, then CMYK is converted to RGB
  TsdTransformYCbCrKToBGR = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
  end;

  // Y-Cb-Cr-A (32bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TsdTransformYCbCrAToBGRA = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform.
  TsdTransformCMYKToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform, Adobe specific.
  TsdTransformCMYKToBGR_Adobe = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
    function DstCellStride: integer; override;
  end;

  // YCCK (32bit) to BGR (24bit) colour transform. The CMY channels are coded as
  // Y-Cb-Cr, thus first unencoded to CMY, then combined with K to do CMYK to RGB.
  TsdTransformYCCKToBGR = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
  end;

  // YCCK to BGR color transform, as Adobe does it. Experimental status!
  TsdTransformYCCKToBGR_Adobe = class(TsdJfifFwdTransform)
  private
    F0_65: integer;
    F44_8: integer;
    procedure InitConst;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function SrcCellStride: integer; override;
  end;

  // Y-Cb-Cr (24bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TsdTransformYCbCrToBGR = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr (24bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel A is set to $FF.
  TsdTransformYCbCrToBGRA = class(TsdJfifFwdTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    function DstCellStride: integer; override;
  end;

  // CIE L*a*b* (24bit) to BGR (24bit), using parameters
  // - based on 24bit (3*8 bit) nulltransform
  TsdTransformCIELabToBGR = class(TsdNullTransform24bit)
  private
    FXw: double;
    FYw: double;
    FZw: double;
    FAmin: double;
    FAmax: double;
    FBmin: double;
    FBmax: double;
    FAofs: integer;
    FBofs: integer;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    // White point
    property Xw: double read FXw write FXw;
    property Yw: double read FYw write FYw;
    property Zw: double read FZw write FZw;
    // Range
    property Amin: double read FAmin write FAmin;
    property Amax: double read FAmax write FAmax;
    property Bmin: double read FBmin write FBmin;
    property Bmax: double read FBmax write FBmax;
    // Offset
    property Aofs: integer read FAofs write FAofs;
    property Bofs: integer read FBofs write FBofs;
  end;
  
  // ITU CIE L*a*b* (24bit) to BGR (24bit), with canned parameters, which are
  // set in the constructor
  TsdTransformITUCIELabToBGR = class(TsdTransformCIELabToBGR)
  public
    constructor Create; override;
  end;
  
  { TODO }
  TsdTransformRGBAToGray = class(TsdColorTransform);
  TsdTransformRGBToCMYK = class(TsdColorTransform);
  TsdTransformRGBToYCCK = class(TsdColorTransform);
  TsdTransformCMYKToYCCK = class(TsdColorTransform);

  
  TsdJpegMarker = class(TERRAObject)
  private
    FMarkerTag: byte;
    FOwner: TERRAObject;
  protected
    FStream: MemoryStream;
    FCodingInfo: TsdJpegInfo;
    function GetMarkerName: Utf8String; virtual;
    procedure StoreData(S: TERRAStream; Size: integer);

  public
    Constructor Create(ACodingInfo: TsdJpegInfo; ATag: byte); virtual;
    Procedure Release; Override;

    class function GetByte(S: TERRAStream): byte;
    class function GetWord(S: TERRAStream): word;
    class procedure PutByte(S: TERRAStream; B: byte);
    class procedure PutWord(S: TERRAStream; W: word);
    class function GetSignature: AnsiString; virtual;
    class function GetMarker: Byte; virtual;
    class function IsSegment(AMarker: Byte; AStream:TERRAStream): Boolean; virtual;
    procedure LoadFromStream(S: TERRAStream; Size: integer);
    procedure SaveToStream(S: TERRAStream);

    procedure ReadMarker; virtual;
    procedure WriteMarker; virtual;
    // Any of the mkXXXX constants defined in sdJpegConsts
    property MarkerTag: byte read FMarkerTag;
    // 3letter description of the marker or the hex description
    property MarkerName: Utf8String read GetMarkerName;
    // marker data stored in its stream
    property Stream:MemoryStream read FStream;
    // Reference to owner TsdJpegFormat, set when adding to the list, and used
    // for DoDebugOut
    property Owner: TERRAObject read FOwner write FOwner;
  end;

  TsdDHTMarkerInfo = record
    BitLengths: array[0..15] of byte;
    BitValues: array of byte;
    Tc, Th: byte;
  end;
  PsdDHTMarkerInfo = ^TsdDHTMarkerInfo;

  TsdDQTMarker = class(TsdJpegMarker)
  private
  protected
    function GetMarkerName: Utf8String; override;
  public
    FTableIndices: array of byte;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TsdDHTMarker = class(TsdJpegMarker)
  private
  protected
    function GetMarkerName: Utf8String; override;
  public
    FMarkerInfo: array of TsdDHTMarkerInfo;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TsdDRIMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TsdSOFnMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TsdSOSMarkerInfo = record
    ComponentID: byte;
    DCTable: byte;
    ACTable: byte;
  end;

  TsdSOSMarker = class(TsdJpegMarker)
  private
    FSpectralStart,
    FSpectralEnd,
    FApproxHigh,
    FApproxLow: byte;
  protected
    procedure FindScanComponent(AScan: TsdScanComponent; AId: byte);
    function GetMarkerName: Utf8String; override;
  public
    FScanCount: byte;
    FMarkerInfo: array of TsdSOSMarkerInfo;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TsdSOIMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
  end;

  TsdEOIMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
  end;

  TsdJpegCoder = class(TERRAObject)
  protected
    FInfo: TsdJpegInfo; // reference to jpeg coding info
    FMethod: TsdJpegDCTCodingMethod; // fast or accurate
    FHasCoefficients: boolean;
    FHasSamples: boolean;
    FScale: TsdJpegScale;
    FTileMode: boolean;
  public
    constructor Create(AOwner: TERRAObject; AInfo: TsdJpegInfo); virtual;
    procedure Clear; virtual;
    procedure Initialize(AScale: TsdJpegScale); virtual;
    procedure Encode(S: TERRAStream; Iteration: cardinal); virtual; abstract;
    procedure Decode(S: TERRAStream; Iteration: cardinal); virtual; abstract;
    procedure DecodeBlock(S: TERRAStream; XStart, YStart, XCount, YCount: integer); virtual; abstract;
    procedure Finalize; virtual;
    procedure ForwardDCT; virtual; abstract;
    procedure InverseDCT; virtual; abstract;
    // Get the values from the image described with map iterator AImage, and put
    // them in the sample maps. Use ATransform to transform the colors.
    procedure SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); virtual; abstract;
    // Build the image that is described with the map iterator AImage, based on the
    // decoded samples. Transform the decoded samples color space to the image color
    // space with ATransform.
    procedure SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); virtual; abstract;
    function CreateDHTMarker: TsdDHTMarker; virtual;
    property Method: TsdJpegDCTCodingMethod read FMethod write FMethod;
    property HasCoefficients: boolean read FHasCoefficients write FHasCoefficients;
    property HasSamples: boolean read FHasSamples write FHasSamples;
    property Scale: TsdJpegScale read FScale;
    property TileMode: boolean read FTileMode write FTileMode;
  end;

  TsdJpegMarkerClass = class of TsdJpegMarker;
  TsdJpegMarkerSet = set of byte;

  TsdJpegMarkerList = class(TERRAObject)
  private
    _Markers:Array Of TsdJpegMarker;
    _MarkerCount:Integer;

    FOwner: TERRAObject;// Reference to owner TsdJpegFormat
    function GetItems(Index: integer): TsdJpegMarker;

  public
    constructor Create(AOwner: TERRAObject);

    Procedure Clear;

    function ByTag(AMarkerTag: byte): TsdJpegMarker;
    function ByClass(AClass: TsdJpegMarkerClass): TsdJpegMarker;
    function HasMarker(ASet: TsdJpegMarkerSet): boolean;
    procedure RemoveMarkers(ASet: TsdJpegMarkerSet);
    procedure InsertAfter(ASet: TsdJpegMarkerSet; AMarker: TsdJpegMarker);
    procedure Add(AItem:TsdJpegMarker);
    property Items[Index: integer]: TsdJpegMarker read GetItems; default;

    Function Extract(Marker: TsdJpegMarker):TsdJpegMarker;
    procedure Insert(Pos:Integer; Marker: TsdJpegMarker);

    Property Count:Integer Read _MarkerCount;
  end;

  // ICC color profile class
  TsdJpegICCProfile = class(TERRAObject)
  private
    FData: array of byte;
    function GetData: pointer;
    function GetDataLength: integer;
  public
    procedure LoadFromStream(S: TERRAStream);
    procedure SaveToStream(S: TERRAStream);
    procedure ReadFromMarkerList(AList: TsdJpegMarkerList);
    procedure WriteToMarkerList(AList: TsdJpegMarkerList);
    property Data: pointer read GetData;
    property DataLength: integer read GetDataLength;
  end;

  TsdPixelFormat =
    (spf1bit, spf2bit, spf4bit, spf8bit, spf10bit, spf12bit, spf15bit, spf16bit,
     spf24bit, spf30bit, spf32bit, spf36bit, spf48bit, spf64bit);

  TsdJpegImage = Class;
  TsdJpegSaveOptions = class(TERRAObject)
  private
    FOwner: TsdJpegImage;
    FQuality: TsdJpegQuality;
    FOptimizeHuffmanTables: boolean;
    FCodingMethod: TsdJpegEncodingMethod;
    FUseSubSampling: boolean;
  protected
    procedure AddMarkers(AStored: TsdJpegColorSpace; AWidth, AHeight: integer);
    procedure SetupDefaultHuffmanTables; virtual;
    procedure SetupQuantTables; virtual;
    procedure SetTableMultiplication(ATable: TsdQuantizationTable; MultiplyPercent: integer;
      const ADefaultTable: TsdIntArray64);
  public
    constructor Create(AOwner: TsdJpegImage);
    property Quality: TsdJpegQuality read FQuality write FQuality;
    property OptimizeHuffmanTables: boolean read FOptimizeHuffmanTables write FOptimizeHuffmanTables;
    property CodingMethod: TsdJpegEncodingMethod read FCodingMethod write FCodingMethod;
    property UseSubSampling: boolean read FUseSubSampling write FUseSubSampling;
  end;

  TsdAPPnMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
  end;

  TsdRSTMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
  end;

  TsdDNLMarker = class(TsdJpegMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;


  TsdCOMMarker = class(TsdAppnMarker)
  private
    function GetComment: AnsiString;
    procedure SetComment(const Value: AnsiString);
  protected
    function GetMarkerName: Utf8String; override;
  public
    procedure ReadMarker; override;
    property Comment: AnsiString read GetComment write SetComment;
  end;

  
  TsdG3FAXMarker = class(TsdAPPnMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
  end;


  // Added by Dec
  // AVI1 marker can be found in frames in MotionJpeg avi files
  // In some cases there are no DHT and we must add it manually
  // See TsdJpegFormat.LoadFromStream for details
  TsdAVI1Marker = class(TsdAPPnMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
  end;

  
  TsdICCProfileMarker = class(TsdAppnMarker)
  private
    FIsValid: boolean;
    FCurrentMarker: byte;
    FMarkerCount: byte;
    function GetCurrentMarker: byte;
    function GetMarkerCount: byte;
    function GetData: pointer;
    function GetDataLength: integer;
    procedure SetDataLength(const Value: integer);
    procedure SetCurrentMarker(const Value: byte);
    procedure SetMarkerCount(const Value: byte);
  protected
    function GetIsValid: boolean;
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
    property IsValid: boolean read GetIsValid;
    property CurrentMarker: byte read GetCurrentMarker write SetCurrentMarker;
    property MarkerCount: byte read GetMarkerCount write SetMarkerCount;
    property Data: pointer read GetData;
    property DataLength: integer read GetDataLength write SetDataLength;
  end;

  // If an APP2  marker segment containing an embedded ICC profile is also present,
  // then the YCbCr is converted to RGB according to the formulas given in the
  // JFIF spec, and the ICC profile is assumed to refer to the resulting RGB space.
  TsdEXIFMarker = class(TsdAPPnMarker)
  protected
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
  end;

  TsdIPTCMarker = class(TsdAPPnMarker)
  protected
    function GetMarkerName: Utf8String; override;
  end;

  TsdJFIFUnits = (
    juNoUnits,
    juXandYareDotsPerInch,
    juXandYareDotsPerCm
  );

  // If a JFIF APP0 marker segment is present, the colorspace is known to be either
  // grayscale or YCbCr.
  // JFIF spec:
  // http://www.jpeg.org/public/jfif.pdf
  TsdJFIFMarker = class(TsdAPPnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FUnits: TsdJFIFUnits;
    FXDensity, FYDensity: word;
    FXThumbnail, FYThumbnail: byte;
    function GetUnits: TsdJFIFUnits;
    function GetVersion: word;
    function GetXDensity: word;
    function GetXThumbnail: byte;
    function GetYDensity: word;
    function GetYThumbnail: byte;
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
    constructor Create(AInfo: TsdJpegInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Version: word read GetVersion;
    property Units: TsdJFIFUnits read GetUnits;
    property XDensity: word read GetXDensity;
    property YDensity: word read GetYDensity;
    property XThumbnail: byte read GetXThumbnail;
    property YThumbnail: byte read GetYThumbnail;
  end;

  // If an Adobe APP14 marker segment is present, the colorspace is determined by
  // consulting the transform  flag. The transform flag takes one of three values:
  //
  // * 2 - The image is encoded as YCCK (implicitly converted from CMYK on encoding).
  // * 1 - The image is encoded as YCbCr (implicitly converted from RGB on encoding).
  // * 0 - Unknown. 3-channel images are assumed to be RGB, 4-channel images are
  //       assumed to be CMYK.
  TsdAdobeApp14Marker = class(TsdAppnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FFlags0: word;
    FFlags1: word;
    FTransform: byte;
    function GetTransform: byte;
    procedure SetTransform(const Value: byte);
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function GetMarkerName: Utf8String; override;
  public
    class function GetSignature: AnsiString; override;
    class function GetMarker: Byte; override;
    constructor Create(AInfo: TsdJpegInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Transform: byte read GetTransform write SetTransform;
  end;

  TsdJpegImage = class(TERRAObject)
  private
    // FOnDebugOut: TsdDebugEvent; this is already defined in TDebugComponent
    FDataSize: int64;
    FCoder: TsdJpegCoder;
    FMarkers: TsdJpegMarkerList;

    // Jpeg coding info
    FJpegInfo: TsdJpegInfo;

    // coder stream
    FCoderStream: MemoryStream;
    FLoadOptions: TsdJpegLoadOptions;
    FLoadScale: TsdJpegScale;
    FMapIterator: TsdMapIterator;
    FMapWidth: integer;
    FMapHeight: integer;
    FPixelFormat: TsdPixelFormat;
    FICCProfile: TsdJpegICCProfile;
    FStoredCS: TsdJpegColorSpace;
    FBitmapCS: TsdJpegColorSpace;
    FDCTCodingMethod: TsdJpegDCTCodingMethod;

    FSaveOptions: TsdJpegSaveOptions;

    _Target:TERRAImage;

    function GetExifInfo: TsdEXIFMarker;
    function GetIptcInfo: TsdIPTCMarker;
    function GetJfifInfo: TsdJFIFMarker;
    function GetAdobeAPP14Info: TsdAdobeApp14Marker;
    function GetICCProfile: TsdJpegICCProfile;
    procedure SetICCProfile(const Value: TsdJpegICCProfile);
    function GetComment: AnsiString;
    procedure SetComment(const Value: AnsiString);
    function GetHeight: integer;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
    function GetWidth: integer;
    procedure GetBitmapTileSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);
    procedure AVI1MarkerCheck;

    Procedure ProvideStrip(ALeft, ATop: integer; ABitmapIter: TsdMapIterator; Target:TERRAImage);

  protected
    procedure EntropyDecodeSkip(S: TERRAStream);
    procedure InitializeDecode;
    // Load the next marker; returns the markertag read (or mkNone if not found)
    function LoadMarker(S: TERRAStream): byte;
    // Get the required color transform from bitmap to samples, based on detected
    // color space in file and bitmap
    procedure GetColorTransformFromBitmap(var AClass: TsdColorTransformClass);
    // Get the required color transform from samples to bitmap, based on detected
    // color space in file and bitmap
    procedure GetColorTransformToBitmap(var AClass: TsdColorTransformClass;
      var AFormat: TsdPixelFormat);
    function HasSamples: boolean;
    function HasCoefficients: boolean;
    function VerifyBitmapColorSpaceForSave: TsdJpegColorSpace;
    procedure AddMinimalMarkersForColorSpaceDetection(AColors: TsdJpegColorSpace);

    Procedure FillIterator();

  public
    constructor Create();
    Procedure Release; Override;

    // Clear the jpeg format: all data (coder and markers) and the bitmap
    procedure Clear;
    // Save info in the bitmap iterator to the Jpeg stream, aka compress the image.
    procedure Compress(AIterator: TsdMapIterator);
    // Get the size of the bitmap that must be created to hold the decoded
    // information
    procedure GetBitmapSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);

    // After the image is loaded from stream, LoadJpeg actually decodes the
    // image. If DoCreateBitmap is true, it creates and renders the bitmap, thru
    // OnCreateMap
    procedure LoadJpeg(AScale: TsdJpegScale; DoCreateBitmap: boolean);

    // Load a Jpeg image from the stream S. It is best to use a TMemoryStream
    // because the bitreader (which causes most reads to the stream) has
    // a specially optimized version to read from TMemoryStream streams. The
    // stream S will be read from from S.Position, and if everything goes well,
    // the stream will be positioned directly after the last (EOI) marker. An
    // exception will be raised if the stream is corrupt or truncated.
    procedure LoadFromStream(S: TERRAStream; Target:TERRAImage);

    // In case of LoadOption [loTileMode] is included, after the LoadFromStream,
    // individual tile blocks can be loaded which will be put in the resulting
    // bitmap. The tile loaded will contain all the MCU blocks that fall within
    // the specified bounds ALeft/ATop/ARight/ABottom. Note that these are var
    // parameters, after calling this procedure they will be updated to the MCU
    // block borders. ALeft/ATop can subsequently be used to draw the resulting
    // TsdJpegFormat.Bitmap to a canvas.
    procedure LoadTileBlock(var ALeft, ATop, ARight, ABottom: integer);

    // After SaveJpeg, the original CoderStream is replaced by the new encoded stream
    // by the coder.
    procedure SaveJpeg();

    // Save the Jpeg image to stream S. Set SaveOptions before saving. Use
    // SaveBitmap first in order to encode the new bitmap to be saved.
    procedure SaveToStream(S: TERRAStream);

    // Encode a Jpeg image strip by strip. OnCreateMap with the partial
    // bitmap needs to be provided to fill one strip of the
    // jpeg file at a time (a strip consists of a row of MCU blocks, usually
    // 8 or 16 pixels high).
    // Only one pass over the data is required, but the resulting jpeg will
    // be saved with standard Huffman tables (as provided in the Jpeg specification)
    procedure SaveBitmapStripByStrip(Target:TERRAImage);

    // call UpdateBitmap after calling LoadJpeg(Scale, False)
    Procedure UpdateBitmap;
    // All metadata markers will be extracted from the file, and put in AList.
    // AList must be initialized (AList := TsdJpegMarkerList.Create)
    procedure ExtractMetadata(AList: TsdJpegMarkerList);
    // Inject the metadata in AList into the marker list of the file. All existing
    // metadata will be removed first; then the markers in AList will be added
    // below the SOI marker.
    procedure InjectMetadata(AList: TsdJpegMarkerList);
    // does the jpeg format have a bitmap already?
    function HasBitmap: boolean;
    // Returns true if the Jpeg has a valid ICC profile. Use property ICCProfile
    // to actually get it.
    function HasICCProfile: boolean;
    // Call this function to detect what colorspace is used in the file. This
    // can only be detected *after* the file is loaded. Set LoadOptions to
    // [loOnlyMetadata] to quickly do a test load to use this function.
    function DetectInternalColorSpace: TsdJpegColorSpace;
    // Reference to low-level information of file. JpegInfo.Width/Height provides
    // the size of the coded image.
    property JpegInfo: TsdJpegInfo read FJpegInfo;
    // Reference to the Jpeg coder. TsdJpegCoder is a generic coder, and specific
    // implementations are present for baseline DCT and progressive DCT.
    property Coder: TsdJpegCoder read FCoder;
    // size in bytes of raw data
    property DataSize: int64 read FDataSize;
    // Reference to the list of low-level markers present in the file (valid after
    // loading).
    property Markers: TsdJpegMarkerList read FMarkers;

  published
    // Pointer to JFIF info marker (if any)
    property JfifInfo: TsdJFIFMarker read GetJfifInfo;
    // Pointer to EXIF info marker (if any)
    property ExifInfo: TsdEXIFMarker read GetExifInfo;
    // Pointer to IPTC info marker (if any)
    property IptcInfo: TsdIPTCMarker read GetIptcInfo;
    // Pointer to Adobe APP14 info marker (if any)
    property AdobeAPP14Info: TsdAdobeApp14Marker read GetAdobeAPP14Info;
    // Read ICCProfile to get a TsdJpegICCProfile object back, in case ICC profile
    // data is available in the markers. The object has a Data pointer and a
    // DataLength property, and it can be used with e.g. LittleCMS. The profile
    // is valid until you load a new file or free the jpeg component.
    property ICCProfile: TsdJpegICCProfile read GetICCProfile write SetICCProfile;
    // Read and write a Jpeg comment (implemented through the COM marker).
    property Comment: AnsiString read GetComment write SetComment;
    // Read Bitmap to get a pointer to a TBitmap object back, that has the currently
    // loaded image. The TBitmap object is no longer valid when the jpeg format class is freed.
    // Assign to Bitmap in order to save the bitmap to a jpeg file. Note: when
    // assigning a new bitmap, the metadata is destroyed. In order to preserve
    // metadata, use a construct like this:
    // <code>
    // List := TsdJpegMarkerList.Create;
    // Jpg.ExtractMetadata(List);
    // Jpg.Bitmap := MyBitmap;
    // Jpg.InjectMetadata(List);
    // Jpg.SaveToFile('test.jpg');
    // List.Free;
    // </code>
    property ImageWidth: integer read GetImageWidth;
    property ImageHeight: integer read GetImageHeight;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    // Include loOnlyMetadata if you want to only load metadata and not decode
    // the image. Include loTileMode to load the image first, without decoding
    // it directly, so that LoadTileBlock can be used to load tiles of the image.
    property LoadOptions: TsdJpegLoadOptions read FLoadOptions write FLoadOptions;
    // Set LoadScale to anything other than jsFull to load a downscaled image, which
    // will be faster than the full image. jsDiv2 will download an image that is
    // half the size in X and Y, jsDiv4 will be quarter size, jsDiv8 will be 1/8
    // size.
    property LoadScale: TsdJpegScale read FLoadScale write FLoadScale;
    // The colorspace present in the file. If jcAutoDetect (default), the software will
    // try to detect which colorspace the JPEG file contains. This info is present
    // in some markers, or in the frame definition. If set to another value,
    // the software will use this as given, and assume that the data in the file
    // is using the given colorspace.
    property StoredCS: TsdJpegColorSpace read FStoredCS write FStoredCS;
    // The colorspace that will be generated when outputting to a bitmap. If set to
    // jcAutoDetect, the stored color space present in the file will be used to
    // directly output the data without color conversion. Default value for
    // BitmapCS is jcRGB.
    property BitmapCS: TsdJpegColorSpace read FBitmapCS write FBitmapCS;
    // Method used for forward and inverse DCT transform. dmAccurate will use an
    // accurate integer method (slow), dmFast will use fast but less accurate
    // integer method. When reading a Jpeg, this only impacts the visual quality.
    // When writing Jpeg, the resulting file quality will be impacted by the
    // method chosen.
    property DCTCodingMethod: TsdJpegDctCodingMethod read FDCTCodingMethod write FDCTCodingMethod;
    // Options that influence how Jpeg images are compressed and saved.
    property SaveOptions: TsdJpegSaveOptions read FSaveOptions;
  end;

  // Holds data for one image component in the frame, provides method
  // to add/extract samples to/from the MCU currently being decoded/encoded
  TsdJpegBlockMap = class(TERRAObject)
  private
    FCoef: array of smallint;
    FCoefBackup: array of smallint; // used when adjusting brightness/contrast
    FSample: array of byte;
    FFrame: TsdFrameComponent; // Pointer to frame info
    FHorzBlockCount: integer; // Horizontal block count
    FVertBlockCount: integer; // Vertical block count
    FBlockStride: integer; // number of samples per block
    FScanStride: integer; // width of a scanline
  protected
    procedure CreateMap; virtual;
  public
    procedure SetSize(AHorzMcuCount, AVertMcuCount: integer;
      AFrame: TsdFrameComponent; ABlockStride: integer);
    procedure Resize(AHorzBlockCount, AVertBlockCount: integer);
    procedure ReduceBlockSize(ANewSize: integer);
    // Number of blocks in the MCU belonging to this image
    function McuBlockCount(AScanCount: integer): integer;
    // Total number of blocks in image (size / 8x8)
    function TotalBlockCount: integer;
    function GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
    function GetCoefPointer(BlockX, BlockY: integer): pointer;
    function GetSamplePointer(BlockX, BlockY: integer): pointer;
    function FirstCoef: pointer;
    function FirstCoefBackup: pointer;
    function HasCoefBackup: boolean;
    procedure MakeCoefBackup;
    procedure ClearCoefBackup;
    property HorzBlockCount: integer read FHorzBlockCount;
    property VertBlockCount: integer read FVertBlockCount;
    property BlockStride: integer read FBlockStride;
    property ScanStride: integer read FScanStride;
    property Frame: TsdFrameComponent read FFrame;
  end;

  TsdBlockMapList = class(TERRAObject)
  Private
    _Maps:Array Of TsdJpegBlockMap;
    _Count:Integer;

    function GetItems(Index: integer): TsdJpegBlockMap;
  Public
    Procedure Release; Override;

    property Items[Index: integer]: TsdJpegBlockMap read GetItems; default;
    Property Count:Integer Read _Count;
  End;

  // Common ancestor for blockbased jpeg codecs like baseline and progressive. It
  // contains a list of blockmaps, which contain DCT coefficients and raw samples
  // for each frame component in the image. We do not reuse the coefficient memory
  // for the samples, so we can still do operations on the coefficients after
  // doing the IDCT.
  TsdJpegBlockCoder = class(TsdJpegCoder)
  private
    FMaps: TsdBlockMapList;
    FBuffer: array of byte;
    FBufferCellStride: integer;
    FBufferScanStride: integer;
  protected
    FBlockStride: integer;
    procedure CorrectBlockStride;
    function BlockstrideForScale(AScale: TsdJpegScale): integer; virtual;
    procedure GetBlockstrideParams(ABlockstride: integer;
      var ABlockWidth, AMcuWidth, AMcuHeight: integer);
    procedure McuRowFromBuffer(McuY: integer; ABlockWidth: integer);
    procedure McuRowToBuffer(McuY: integer; ABlockWidth: integer);
    procedure SetupMaps(SpecialSize: boolean; AHorzMcuCount, AVertMcuCount: integer);
  public
    constructor Create(AOwner: TERRAObject; AInfo: TsdJpegInfo); override;
    Procedure Release; Override;
    
    procedure Clear; override;
    procedure SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); override;
    procedure SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); override;
    procedure ForwardDCT; override;
    procedure InverseDCT; override;
    property Maps: TsdBlockMapList read FMaps;
    property BlockStride: integer read FBlockStride;
  end;

  TsdEntropyCoder = class(TERRAObject)
  public
    constructor Create; virtual;
  end;

  TsdEntropyCoderList = class(TERRAObject)
  private
    _Coders:Array Of TsdEntropyCoder;
    _Count:Integer;

    function GetItems(Index: integer): TsdEntropyCoder;
    procedure SetItems(Index: integer; const Value: TsdEntropyCoder);
  public
    Procedure Clear;
    property Items[Index: integer]: TsdEntropyCoder read GetItems write SetItems; default;
    Property Count:Integer Read _Count;
  end;

  // Abstract bit reader class
  TsdBitReader = class(TERRAObject)
  private
    FBits: cardinal;
    FBitsLeft: integer;
    FStream: TERRAStream;
    // Errors - we don't use exceptions here, since we need highspeed
    FHitMarker: boolean;
    FHitEndOfStream: boolean;
    FMarkerTag: byte;
  protected
    function GetStreamPos: int64; virtual;
    procedure SetStreamPos(const Value: int64); virtual;
  public
    // These are public for fast access. Not the most beatiful under OO design
    // but speed requests sacrifices
    ThisByte: PByte;
    NextByte: PByte;
    constructor Create(S: TERRAStream); virtual;
    function GetBits(Count: integer): cardinal;
    procedure RemoveBits(Count: integer); virtual; abstract;
    procedure Reset; virtual;
    procedure Reload; virtual;
    function HasError: boolean;
    // Are we at a marker, and there are no more bits?
    function HitMarkerNoBitsLeft: boolean;
    property HitEndOfStream: boolean read FHitEndOfStream;
    // Are we at a marker? (there may still be up to 24 bits left)
    property HitMarker: boolean read FHitMarker;
    property MarkerTag: byte read FMarkerTag;
    property Bits: cardinal read FBits write FBits;
    property BitsLeft: integer read FBitsLeft write FBitsLeft;
    property StreamPos: int64 read GetStreamPos write SetStreamPos;
  end;

  // Bit writer that saves to any stream. The writer has a buffer of approx. 1K to
  // store the data locally until it fills up or Restart is called. In that case
  // the buffer is flushed to the stream.
  TsdBitWriter = class(TERRAObject)
  private
    FStored: cardinal;
    FBitsStored: integer;
    FStream: TERRAStream;
    FBuffer: array[0..1030] of byte;
    FBufferPos: integer;
    procedure Emit(B: byte);
    procedure FlushBuffer;
  public
    constructor Create(S: TERRAStream); virtual;
    function CountBits(AValue: integer): integer;
    procedure PutCode(ACode: PsdHuffmanCode); virtual;
    procedure PutCodeExtend(ACode: PsdHuffmanCode; AValue: integer; ABitCount: integer); virtual;
    procedure PutBits(Bits: cardinal; Count: integer); virtual;
    procedure Restart; virtual;
  end;

  TsdDryRunBitWriter = class(TsdBitWriter)
  private
    FHistogram: Psd8bitHuffmanHistogram;
  public
    procedure PutCode(ACode: PsdHuffmanCode); override;
    procedure PutCodeExtend(ACode: PsdHuffmanCode; AValue: integer; ABitCount: integer); override;
    procedure PutBits(Bits: cardinal; Count: integer); override;
    procedure Restart; override;
    property Histogram: Psd8bitHuffmanHistogram read FHistogram write FHistogram;
  end;
  
  // Minimum Coded Unit block (MCU)
  TsdMCUBlock = record
    Values: PsdCoefBlock;
    PPred: Psmallint;
    DCTable: integer;
    ACTable: integer;
    BlockIdx: integer;
    MapIdx: integer;
  end;
  PsdMCUBlock = ^TsdMCUBlock;

  // This is a non-optimal reader but it works for stream types that are not
  // memory stream. When using file streams it is advisable to first copy the
  // Jpeg to a memory stream before calling LoadFromStream.
  // This reader gets bytes one at a time from the stream, aka lots of calls!
  TsdStreamBitReader = class(TsdBitReader)
  public
    constructor Create(S: TERRAStream); override;
    procedure RemoveBits(Count: integer); override;
  end;
  
  TsdJpegTile = class
  public
    FMcuIndex: integer;
    FStreamPos: int64;
    FBits: cardinal;
    FBitsLeft: integer;
    FPredictors: array of smallint;
  end;

  TsdJpegTileList = class(TERRAObject)
  private
    _Tiles:Array Of TsdJpegTile;
    _Count:Integer;


    function GetItems(Index: integer): TsdJpegTile;
  public
    Procedure Clear;

    Procedure Add(Tile:TsdJpegTile);
    function IndexByMcuIndex(AMcuIndex: integer): integer;
    property Items[Index: integer]: TsdJpegTile read GetItems; default;
    Property Count:Integer Read _Count;
  end;

  // TCustomSortedList is a TERRAObjectList descendant providing easy sorting
  // capabilities, while keeping simplicity. Override the DoCompare method
  // to compare two items.
  TCustomSortedList = class(TERRAObject)
  private
    _List:Array Of TERRAObject;
    _Count:Integer;

    FSorted: boolean;
    procedure SetSorted(AValue: boolean);
  protected
    // Override this method to implement the object comparison between two
    // items. The default just compares the item pointers
    function DoCompare(Item1, Item2: TERRAObject): integer; virtual;
  public
    constructor Create(AOwnsObjects: boolean = true);
    function Add(AItem: TERRAObject): integer;
    procedure Append(AItem: TERRAObject);

    Procedure Clear;

    Procedure Insert(Pos:Integer; AItem: TERRAObject);
    // AddUnique behaves just like Add but checks if the item to add is unique
    // by checking the result of the Find function. If the item is found it is
    // replaced by the new item (old item removed), unless RaiseError = True, in
    // that case an exception is raised.
    function AddUnique(Item: TERRAObject): integer; virtual;
    function Find(Item: TERRAObject; out Index: integer): boolean; virtual;
    // Find (multiple) items equal to Item, and return Index of first equal
    // item and the number of multiples in Count
    procedure FindMultiple(Item: TERRAObject; out AIndex, ACount: integer); virtual;
    procedure Sort; virtual;

    Procedure Delete(Pos:Integer);
    Function Get(Index:Integer):TERRAObject;
    Function IndexOf(Item:TERRAObject):Integer;

    property Sorted: boolean read FSorted write SetSorted default true;
    Property Count:Integer Read _Count;
  end;

  TItemCompareEvent = function(Item1, Item2: TERRAObject; Info: pointer): integer of object;
  TItemCompareMethod = function(Item1, Item2: TERRAObject; Info: pointer): integer;
  TPointerCompareMethod = function(Ptr1, Ptr2: pointer): integer;

  // TSortedList is an object list that provides an events or method template
  // to compare items. Assign either OnCompare (for an event) or CompareMethod
  // (for a method template) to do the comparison of items. Additional information
  // required for the compare method can be passed with the CompareInfo pointer.
  TSortedList = class(TCustomSortedList)
  private
    FCompareInfo: pointer;
    FOnCompare: TItemCompareEvent;
    FCompareMethod: TItemCompareMethod;
  protected
    function DoCompare(Item1, Item2: TERRAObject): integer; override;
  public
    property CompareInfo: pointer read FCompareInfo write FCompareInfo;
    // Use CompareMethod if you want to specify a compare method as stand-alone method
    property CompareMethod: TItemCompareMethod read FCompareMethod write FCompareMethod;
    // Use OnCompare if you want to specify a compare method as a method of a class
    property OnCompare: TItemCompareEvent read FOnCompare write FOnCompare;
  end;

  // Generic Huffman coder implementing shared methods
  TsdHuffmanCoder = class(TsdEntropyCoder)
  private
  protected
    FCodes: array of TsdHuffmanCode;
  public
    procedure GenerateCodeTable(ATable: TsdHuffmanTable); virtual;
  end;

  TsdHuffmanNode = Class(TERRAObject)
  private
    FBitCount: integer;
    FCount: integer;
    FCode: PsdHuffmanCode;
    FB0: TsdHuffmanNode;
    FB1: TsdHuffmanNode;
  public
    Procedure Release; Override;
    property BitCount: integer read FBitCount write FBitCount;
    property Count: integer read FCount write FCount;
    property Code: PsdHuffmanCode read FCode write FCode;
    property B0: TsdHuffmanNode read FB0 write FB0;
    property B1: TsdHuffmanNode read FB1 write FB1;
  end;

  
  TsdHuffmanNodeList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdHuffmanNode;
  protected
    function DoCompare(Item1, Item2: TERRAObject): integer; override;
  public
    property Items[Index: integer]: TsdHuffmanNode read GetItems; default;
  end;

  // General 8-bit huffman encoder
  Tsd8bitHuffmanEncoder = class(TsdHuffmanCoder)
  private
    FHistogram: Tsd8bitHuffmanHistogram;
    FNodes: TsdHuffmanNodeList; // list of huffman nodes (count, code and leaves)
    function GetHistogram: Psd8bitHuffmanHistogram;
  public
    constructor Create; override;
    Procedure Release; Override;
    procedure GenerateCodeTable(ATable: TsdHuffmanTable); override;
    procedure OptimiseHuffmanFromHistogram(var Item: TsdDHTMarkerInfo);
    property Histogram: Psd8bitHuffmanHistogram read GetHistogram;
  end;

  // Generic Huffman decoder
  TsdHuffmanDecoder = class(TsdHuffmanCoder)
  private
  protected
    FLookup: array of TsdHuffmanLookupTable;
    FLookupCount: word;
  public
    {$IFDEF DETAILS}
    FCountCodes: integer;
    FCountBits: integer;
    {$ENDIF}
    procedure AddToLookupTable(Table, Code, Len, Value: integer);
    procedure GenerateLookupTables(Table: TsdHuffmanTable); virtual;
  end;


  // General 8-bit huffman decoder
  Tsd8bitHuffmanDecoder = class(TsdHuffmanDecoder)
  public
    procedure GenerateLookupTables(Table: TsdHuffmanTable); override;
  end;

  // Specific Huffman DC baseline decoder
  TsdDCBaselineHuffmanDecoder = class(Tsd8bitHuffmanDecoder)
  private
  public
    procedure DecodeMcuBlock(var ABlock: TsdMcuBlock; AReader: TsdBitReader);
  end;

  // Specific Huffman AC baseline decoder
  TsdACBaselineHuffmanDecoder = class(Tsd8bitHuffmanDecoder)
  private
  public
    procedure DecodeMcuBlock(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      AZigZag: PsdZigZagArray);
    // Special routine for jsDiv8 scale loading, just skipping this data
    procedure DecodeMcuBlockSkip(AReader: TsdBitReader);
  end;

  // Specific Huffman DC baseline encoder
  TsdDCBaselineHuffmanEncoder = class(Tsd8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TsdMcuBlock; AWriter: TsdBitWriter);
  end;

  // Specific Huffman AC baseline encoder
  TsdACBaselineHuffmanEncoder = class(Tsd8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TsdMcuBlock; AWriter: TsdBitWriter);
  end;

  TsdDCProgressiveHuffmanDecoder = class(TsdDCBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      ApproxLow: integer);
  end;

  TsdACProgressiveHuffmanDecoder = class(TsdACBaselineHuffmanDecoder)
  public
     // Progressive
    procedure DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
  end;


  // The Jpeg Baseline coder implements the baseline huffman DC and AC decoding
  // and encoding
  TsdJpegBaselineCoder = class(TsdJpegBlockCoder)
  private
  protected
    FDCCoders: TsdEntropyCoderList;
    FACCoders: TsdEntropyCoderList;
    FMcu: array of TsdMcuBlock;
    FMcuBlockCount: integer;
    FBitReader: TsdBitReader;
    FBitWriter: TsdBitWriter;
    FMcuIndex: integer;
    FHorzMcuCount, FVertMcuCount: integer;
    FRstIndex: integer;
    FZigZag: PsdZigZagArray;
    FIsDryRun: boolean;
    FTiles: TsdJpegTileList;
    procedure DoMcuBlockCount;
    procedure InitializeDecoderTables; virtual;
    procedure InitializeEncoderTables; virtual;
    procedure DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean); virtual;
    procedure EncodeMcu(AMcuX, AMcuY: integer); virtual;
    procedure ResetDecoder;
    procedure ResetEncoder;
    procedure HandleEndOfStreamError(S: TERRAStream); virtual;
    procedure HandleRestartInterval(S: TERRAStream; Warn: boolean); virtual;
    procedure HandleHitMarkerError(S: TERRAStream); virtual;
    function HandleDNLMarker(AMcuY: integer; S: TERRAStream): boolean; virtual;
    procedure ResizeVerticalMcu(NewVertMcuCount: integer); virtual;
  public
    constructor Create(AOwner: TERRAObject; AInfo: TsdJpegInfo); override;
    Procedure Release; Override;
    
    procedure Clear; override;
    procedure Initialize(AScale: TsdJpegScale); override;
    procedure Decode(S: TERRAStream; Iteration: cardinal); override;
    procedure DecodeBlock(S: TERRAStream; XStart, YStart, XCount, YCount: integer); override;
    procedure Encode(S: TERRAStream; Iteration: cardinal); override;
    procedure EncodeStripStart(S: TERRAStream);
    procedure EncodeStrip(S: TERRAStream);
    procedure EncodeStripClose;
    function CreateDHTMarker: TsdDHTMarker; override;
  end;

  TsdJpegProgressiveCoder = class(TsdJpegBaselineCoder)
  private
    FEOBRun: integer;
    FIsDCBand: boolean;
    FIsFirst: boolean;
  protected
    procedure DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean); override;
    procedure InitializeDecoderTables; override;
    function BlockstrideForScale(AScale: TsdJpegScale): integer; override;
    procedure HandleRestartInterval(S: TERRAStream; Warn: boolean); override;
  public
    procedure Decode(S: TERRAStream; Iteration: cardinal); override;
    procedure Finalize; override;
  end;

  // Same as baseline coder
  TsdJpegExtendedCoder = class(TsdJpegBaselineCoder)
  end;

  // Method definition for forward DCT on one block of samples
  TFDCTMethod = procedure(const Sample: TsdSampleBlock; out Coef: TsdCoefBlock;
    var Wrksp: TsdIntArray64);

  // Method definition for inverse DCT on one block of coefficients
  TIDCTMethod = procedure(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
    const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
  
  TsdJpegDCT = class(TERRAObject)
  private
    FQuant: TsdIntArray64;
    FMap: TsdJpegBlockMap;
    FMethod: TsdJpegDCTCodingMethod;
  public
    procedure BuildQuantTableFrom(ATable: TsdQuantizationTable);
    property Map: TsdJpegBlockMap read FMap write FMap;
    property Method: TsdJpegDCTCodingMethod read FMethod write FMethod;
  end;

  // Forward DCT
  TsdJpegFDCT = class(TsdJpegDCT)
  public
    procedure PerformFDCT(ATable: TsdQuantizationTable);
  end;

  // Inverse DCT
  TsdJpegIDCT = class(TsdJpegDCT)
  public
    procedure PerformIDCT;
  end;

Const
  // Jpeg markers defined in Table B.1
  mkNone  = 0;

  mkSOF0  = $c0; // Baseline DCT + Huffman encoding
  mkSOF1  = $c1; // Extended Sequential DCT + Huffman encoding
  mkSOF2  = $c2; // Progressive DCT + Huffman encoding
  mkSOF3  = $c3; // Lossless (sequential) + Huffman encoding

  mkSOF5  = $c5; // Differential Sequential DCT + Huffman encoding
  mkSOF6  = $c6; // Differential Progressive DCT + Huffman encoding
  mkSOF7  = $c7; // Differential Lossless (sequential) + Huffman encoding

  mkJPG   = $c8; // Reserved for Jpeg extensions
  mkSOF9  = $c9; // Extended Sequential DCT + Arithmetic encoding
  mkSOF10 = $ca; // Progressive DCT + Arithmetic encoding
  mkSOF11 = $cb; // Lossless (sequential) + Arithmetic encoding

  mkSOF13 = $cd; // Differential Sequential DCT + Arithmetic encoding
  mkSOF14 = $ce; // Differential Progressive DCT + Arithmetic encoding
  mkSOF15 = $cf; // Differential Lossless (sequential) + Arithmetic encoding

  mkDHT   = $c4; // Define Huffman Table

  mkDAC   = $cc; // Define Arithmetic Coding

  mkRST0  = $d0; // Restart markers
  mkRST1  = $d1;
  mkRST2  = $d2;
  mkRST3  = $d3;
  mkRST4  = $d4;
  mkRST5  = $d5;
  mkRST6  = $d6;
  mkRST7  = $d7;

  mkSOI   = $d8; // Start of Image
  mkEOI   = $d9; // End of Image
  mkSOS   = $da; // Start of Scan
  mkDQT   = $db; // Define Quantization Table
  mkDNL   = $dc; // Define Number of Lines
  mkDRI   = $dd; // Define Restart Interval
  mkDHP   = $de; // Define Hierarchical Progression
  mkEXP   = $df; // Expand reference components

  // For APPn markers see:
  // http://www.ozhiker.com/electronics/pjmt/jpeg_info/app_segments.html

  mkAPP0  = $e0; // APPn markers - APP0 = JFIF
  mkAPP1  = $e1; //                APP1 = EXIF or XMP
  mkAPP2  = $e2; //                ICC colour profile
  mkAPP3  = $e3;
  mkAPP4  = $e4;
  mkAPP5  = $e5;
  mkAPP6  = $e6;
  mkAPP7  = $e7;
  mkAPP8  = $e8;
  mkAPP9  = $e9;
  mkAPP10 = $ea;
  mkAPP11 = $eb;
  mkAPP12 = $ec;
  mkAPP13 = $ed; //                APP13 = IPTC or Adobe IRB
  mkAPP14 = $ee; //                APP14 = Adobe
  mkAPP15 = $ef;

  mkJPG0  = $f0; // JPGn markers - reserved for JPEG extensions
  mkJPG13 = $fd;
  mkCOM   = $fe; // Comment

  mkTEM   = $01; // Reserved for temporary use

  cColorSpaceNames: array[TsdJpegColorSpace] of AnsiString =
  ('AutoDetect', 'Gray', 'GrayA', 'RGB', 'RGBA', 'YCbCr', 'YCbCrA',
   'CMYK', 'CMYK as YCbCrK', 'YCCK', 'PhotoYCC', 'PhotoYCCA', 'ITU CieLAB');

   cDefaultJpgCompressionQuality = 80;

  // This matrix maps zigzag position to the left/right
  // top/down normal position inside the 8x8 block.
  cJpegInverseZigZag1x1: TsdZigZagArray =
    ( 0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag2x2: TsdZigZagArray =
    ( 0,  1,  2,  0,  3,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag4x4: TsdZigZagArray =
    ( 0,  1,  4,  8,  5,  2,  3,  6,
      9, 12,  0, 13, 10,  7,  0,  0,
      0, 11, 14,  0,  0,  0,  0,  0,
     15,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag8x8: TsdZigZagArray =
    ( 0,  1,  8, 16,  9,  2,  3, 10,
     17, 24, 32, 25, 18, 11,  4,  5,
     12, 19, 26, 33, 40, 48, 41, 34,
     27, 20, 13,  6,  7, 14, 21, 28,
     35, 42, 49, 56, 57, 50, 43, 36,
     29, 22, 15, 23, 30, 37, 44, 51,
     58, 59, 52, 45, 38, 31, 39, 46,
     53, 60, 61, 54, 47, 55, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegForwardZigZag8x8: TsdZigZagArray =
    ( 0,  1,  5,  6, 14, 15, 27, 28,
      2,  4,  7, 13, 16, 26, 29, 42,
      3,  8, 12, 17, 25, 30, 41, 43,
      9, 11, 18, 24, 31, 40, 44, 53,
     10, 19, 23, 32, 39, 45, 52, 54,
     20, 22, 33, 38, 46, 51, 55, 60,
     21, 34, 37, 47, 50, 56, 59, 61,
     35, 36, 48, 49, 57, 58, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegNaturalZigZag8x8: TsdZigZagArray =
    ( 0,  1,  2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 20, 21, 22, 23,
     24, 25, 26, 27, 28, 29, 30, 31,
     32, 33, 34, 35, 36, 37, 38, 39,
     40, 41, 42, 43, 44, 45, 46, 47,
     48, 49, 50, 51, 52, 53, 54, 55,
     56, 57, 58, 59, 60, 61, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  // entry n equals 1 shl (n-1)
  cExtendTest: array[0..15] of integer =
    ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
     $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

  // entry n equals (-1 shl n) + 1
  cExtendOffset: array[0..15] of integer =
   (0, ((-1) shl 1 ) + 1, ((-1) shl 2 ) + 1, ((-1) shl 3 ) + 1, ((-1) shl 4 ) + 1,
       ((-1) shl 5 ) + 1, ((-1) shl 6 ) + 1, ((-1) shl 7 ) + 1, ((-1) shl 8 ) + 1,
       ((-1) shl 9 ) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
       ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

  // These are the sample quantization tables given in JPEG spec section K.1.
  // The spec says that the values given produce "good" quality, and
  // when divided by 2, "very good" quality.
  cStdLuminanceQuantTbl: TsdIntArray64 =
   (16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99);

  cStdChrominanceQuantTbl: TsdIntArray64 =
   (17,  18,  24,  47,  99,  99,  99,  99,
    18,  21,  26,  66,  99,  99,  99,  99,
    24,  26,  56,  99,  99,  99,  99,  99,
    47,  66,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99);

  // These are standard Huffman tables for general use
  cHuffmanBitsDcLum: array[0..15] of byte =
    (0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  cHuffmanValDCLum: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  cHuffmanBitsDCChrom: array[0..15] of byte =
    (0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0);
  cHuffmanValDCChrom: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

  const cHuffmanBitsACLum: array[0..15] of byte =
    (0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7d);
  const cHuffmanValACLum: array[0..161] of byte =
    ( $01, $02, $03, $00, $04, $11, $05, $12,
      $21, $31, $41, $06, $13, $51, $61, $07,
      $22, $71, $14, $32, $81, $91, $a1, $08,
      $23, $42, $b1, $c1, $15, $52, $d1, $f0,
      $24, $33, $62, $72, $82, $09, $0a, $16,
      $17, $18, $19, $1a, $25, $26, $27, $28,
      $29, $2a, $34, $35, $36, $37, $38, $39,
      $3a, $43, $44, $45, $46, $47, $48, $49,
      $4a, $53, $54, $55, $56, $57, $58, $59,
      $5a, $63, $64, $65, $66, $67, $68, $69,
      $6a, $73, $74, $75, $76, $77, $78, $79,
      $7a, $83, $84, $85, $86, $87, $88, $89,
      $8a, $92, $93, $94, $95, $96, $97, $98,
      $99, $9a, $a2, $a3, $a4, $a5, $a6, $a7,
      $a8, $a9, $aa, $b2, $b3, $b4, $b5, $b6,
      $b7, $b8, $b9, $ba, $c2, $c3, $c4, $c5,
      $c6, $c7, $c8, $c9, $ca, $d2, $d3, $d4,
      $d5, $d6, $d7, $d8, $d9, $da, $e1, $e2,
      $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
      $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  cHuffmanBitsACChrom: array[0..15] of byte =
    (0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77);
  cHuffmanValACChrom: array[0..161] of byte =
    ( $00, $01, $02, $03, $11, $04, $05, $21,
      $31, $06, $12, $41, $51, $07, $61, $71,
      $13, $22, $32, $81, $08, $14, $42, $91,
      $a1, $b1, $c1, $09, $23, $33, $52, $f0,
      $15, $62, $72, $d1, $0a, $16, $24, $34,
      $e1, $25, $f1, $17, $18, $19, $1a, $26,
      $27, $28, $29, $2a, $35, $36, $37, $38,
      $39, $3a, $43, $44, $45, $46, $47, $48,
      $49, $4a, $53, $54, $55, $56, $57, $58,
      $59, $5a, $63, $64, $65, $66, $67, $68,
      $69, $6a, $73, $74, $75, $76, $77, $78,
      $79, $7a, $82, $83, $84, $85, $86, $87,
      $88, $89, $8a, $92, $93, $94, $95, $96,
      $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
      $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
      $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
      $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
      $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
      $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
      $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  // Motion Jpeg DHT segment
  cMjpgDHTSeg: packed array[0..415] of byte = (
    $00, $00, $01, $05, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00, $00,
    $00, $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $01,
    $00, $03, $01, $01, $01, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00,

    $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $10, $00,
    $02, $01, $03, $03, $02, $04, $03, $05, $05, $04, $04, $00, $00, $01, $7D,
    $01, $02, $03, $00, $04, $11, $05, $12, $21, $31, $41, $06, $13, $51, $61,
    $07, $22, $71, $14, $32, $81, $91, $A1, $08, $23, $42, $B1, $C1, $15, $52,
    $D1, $F0, $24, $33, $62, $72, $82, $09, $0A, $16, $17, $18, $19, $1A, $25,
    $26, $27, $28, $29, $2A, $34, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,

    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $83,
    $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98, $99,
    $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2, $D3,
    $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, $11, $00, $02,
    $01, $02, $04, $04, $03, $04, $07, $05, $04, $04, $00, $01, $02, $77, $00,

    $01, $02, $03, $11, $04, $05, $21, $31, $06, $12, $41, $51, $07, $61, $71,
    $13, $22, $32, $81, $08, $14, $42, $91, $A1, $B1, $C1, $09, $23, $33, $52,
    $F0, $15, $62, $72, $D1, $0A, $16, $24, $34, $E1, $25, $F1, $17, $18, $19,
    $1A, $26, $27, $28, $29, $2A, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,
    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $82,
    $83, $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98,

    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5,
    $B6, $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2,
    $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA);

const

  // For AA&N IDCT method, multipliers are equal to quantization
  // coefficients scaled by scalefactor[row]*scalefactor[col], where
  //    scalefactor[0] := 1
  //    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
  // To get integer precision, the multiplier table is scaled by 14 bits
  cIFastQuantScales : array[0..63] of integer =
    ({ precomputed values scaled up by 14 bits }
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
     21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
     19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
      8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
      4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247);

const
  // Fast IDCT: we use 9 bits of precision, so must multiply by 2^9
  cIFastConstBits = 9;
  cIFastRangeBits = cIFastConstBits + 3;
  cIFastConstScale = 1 shl cIFastConstBits;

const
  FIX_1_082392200F = integer(Round(cIFastConstScale * 1.082392200));
  FIX_1_414213562F = integer(Round(cIFastConstScale * 1.414213562));
  FIX_1_847759065F = integer(Round(cIFastConstScale * 1.847759065));
  FIX_2_613125930F = integer(Round(cIFastConstScale * 2.613125930));

const
  // Accurate FDCT
  cConstBits = 13;
  cConstScaleA = 1 SHL cConstBits;
  cPass1Bits = 2;

const
  // Constants used in forward DCT
  FIX_0_298631336AF = Round(cConstScaleA * 0.298631336);
  FIX_0_390180644AF = Round(cConstScaleA * 0.390180644);
  FIX_0_541196100AF = Round(cConstScaleA * 0.541196100);
  FIX_0_765366865AF = Round(cConstScaleA * 0.765366865);
  FIX_0_899976223AF = Round(cConstScaleA * 0.899976223);
  FIX_1_175875602AF = Round(cConstScaleA * 1.175875602);
  FIX_1_501321110AF = Round(cConstScaleA * 1.501321110);
  FIX_1_847759065AF = Round(cConstScaleA * 1.847759065);
  FIX_1_961570560AF = Round(cConstScaleA * 1.961570560);
  FIX_2_053119869AF = Round(cConstScaleA * 2.053119869);
  FIX_2_562915447AF = Round(cConstScaleA * 2.562915447);
  FIX_3_072711026AF = Round(cConstScaleA * 3.072711026);

const
  // Accurate IDCT
  cIAccConstBits = 9;
  cIAccRangeBits = cIAccConstBits + 3;
  // we use 9 bits of precision, so must multiply by 2^9
  cIAccConstScale = 1 shl cIAccConstBits;
  cCenterSample = 128;
  cMaxSample    = 255;

const
  // Constants used in Inverse DCT accurate
  FIX_0_298631336AI = Round(cIAccConstScale * 0.298631336);
  FIX_0_390180644AI = Round(cIAccConstScale * 0.390180644);
  FIX_0_541196100AI = Round(cIAccConstScale * 0.541196100);
  FIX_0_765366865AI = Round(cIAccConstScale * 0.765366865);
  FIX_0_899976223AI = Round(cIAccConstScale * 0.899976223);
  FIX_1_175875602AI = Round(cIAccConstScale * 1.175875602);
  FIX_1_501321110AI = Round(cIAccConstScale * 1.501321110);
  FIX_1_847759065AI = Round(cIAccConstScale * 1.847759065);
  FIX_1_961570560AI = Round(cIAccConstScale * 1.961570560);
  FIX_2_053119869AI = Round(cIAccConstScale * 2.053119869);
  FIX_2_562915447AI = Round(cIAccConstScale * 2.562915447);
  FIX_3_072711026AI = Round(cIAccConstScale * 3.072711026);

  sInternalError                   = 'Internal error';
  sUnsupportedEncoding             = 'Unsupported encoding: SOF%d';
  sMarkerExpected                  = 'Jpeg Marker expected';
  sUnsupportedBitsPerSample        = 'Unsupported bits per sample';
  sInvalidTableClass               = 'Invalid table class in DHT marker';
  sInputStreamChopped              = 'Input stream prematurely chopped';
  sUnexpectedMarkerInEncodedStream = 'Unexpected marker in encoded stream';
  sInvalidFrameRef                 = 'Invalid frame reference in scan component';
  sNoColorTransformation           = 'No color transformation available for current settings';
  sNoDCTCoefficentsAvailable       = 'No DCT coefficients available (compress first)';
  sOperationOnlyFor8x8             = 'Operation can only be performed with LoadScale = jsFull';
  sBitmapIsEmptyCannotSave         = 'Bitmap is empty; cannot save';
  sInvalidFormatForSelectedCS      = 'Invalid bitmap format for selected color space';
  sCommentCannotBeSet              = 'Comment cannot be set before assigning bitmap';
  sDNLMarkerExpected               = 'DNL marker expected';
  sUnsupportedColorSpace           = 'Unsupported color space';
  sOnProvideStripMustBeAssigned    = 'OnProvideStrip must be assigned';
  sOnCreateMapMustBeAssigned       = 'OnCreateMap must be assigned';
  sCannotUseTileMode               = 'Cannot use tilemode with progressive jpeg';
  sRangeErrorInTileLoading         = 'Range error in tiled loading: make sure to select tilemode';

  sAddingNonUniqueObject = 'Adding non-unique object to list is not allowed';
  sListMustBeSorted = 'List must be sorted';

var
  glJpegMarkerClassList:Array Of TsdJpegMarkerClass;
  glJpegMarkerClassCount:Integer;

procedure RegisterJpegMarkerClass(AClass: TsdJpegMarkerClass);
var
  i: Integer;
  Res: Boolean;
  M1, M2: Byte;
  S1, S2: AnsiString;
  Temp:TsdJpegMarkerClass;
begin
  Inc(glJpegMarkerClassCount);
  SetLength(glJpegMarkerClassList, glJpegMarkerClassCount);
  glJpegMarkerClassList[Pred(glJpegMarkerClassCount)] := AClass;
  repeat
    Res := False;
    for i := 0 to glJpegMarkerClassCount - 2 do
    begin
      S1 := TsdJpegMarkerClass(glJpegMarkerClassList[i]).GetSignature;
      S2 := TsdJpegMarkerClass(glJpegMarkerClassList[i + 1]).GetSignature;
      if S1 > S2 then
      begin
        Temp := glJpegMarkerClassList[I];
        glJpegMarkerClassList[I] := glJpegMarkerClassList[I+1];
        glJpegMarkerClassList[I+1] := Temp;
        Res := True;
      end  else
        if S1 = S2 then
        begin
          M1 := TsdJpegMarkerClass(glJpegMarkerClassList[i]).GetMarker;
          M2 := TsdJpegMarkerClass(glJpegMarkerClassList[i + 1]).GetMarker;
          if M1 > M2 then
          begin
            Temp := glJpegMarkerClassList[I];
            glJpegMarkerClassList[I] := glJpegMarkerClassList[I+1];
            glJpegMarkerClassList[I+1] := Temp;
            Res := True;
          end
        end
    end;
  until not Res;
end;

function FindJpegMarkerClassList(AMarker: Byte; AStream: TERRAStream): TsdJpegMarkerClass;
var
  i: Integer;
  SavePos: Int64;
begin
  Result := nil;
  if glJpegMarkerClassList = nil then
    Exit;
  SavePos := AStream.Position;
  try
    for i := glJpegMarkerClassCount - 1 downto 0 do
      try
        AStream.Position := SavePos;
        if TsdJpegMarkerClass(glJpegMarkerClassList[i]).IsSegment(AMarker, AStream) then
        begin
          Result := TsdJpegMarkerClass(glJpegMarkerClassList[i]);
          Break;
        end;
      except
      end;
  finally
    AStream.Position := SavePos;
  end;
end;

function ComparePointer(Item1, Item2: pointer): integer;
begin
  if PtrUInt(Item1) < PtrUInt(Item2) then
    Result := -1
  else
    if PtrUInt(Item1) > PtrUInt(Item2) then
      Result := 1
    else
      Result := 0;
end;

function CompareInteger(Int1, Int2: integer): integer;
begin
  if Int1 < Int2 then
    Result := -1
  else
    if Int1 > Int2 then
      Result := 1
    else
      Result := 0;
end;


function sdPixelFormatToByteCount(const AFormat: TsdPixelFormat): cardinal;
begin
  case AFormat of
  spf1bit: Result := 1;
  spf2bit: Result := 1;
  spf4bit: Result := 1;
  spf8bit: Result := 1;
  spf10bit: Result := 2;
  spf12bit: Result := 2;
  spf15bit: Result := 2;
  spf16bit: Result := 2;
  spf24bit: Result := 3;
  spf30bit: Result := 4;
  spf32bit: Result := 4;
  spf36bit: Result := 5;
  spf48bit: Result := 6;
  spf64bit: Result := 8;
  else
    Result := 0;
  end;
end;

function sdGetDivisor(AScale: TsdJpegScale): integer;
begin
  case AScale of
  jsFull: Result := 1;
  jsDiv2: Result := 2;
  jsDiv4: Result := 4;
  jsDiv8: Result := 8;
  else
    Result := 1;
  end;
end;

  
{ TsdHuffmanTable }
function TsdHuffmanTable.GetCount: integer;
begin
  Result := length(FItems);
end;

function TsdHuffmanTable.GetItems(Index: integer): PsdHuffmanCode;
begin
  Result := @FItems[Index];
end;

procedure TsdHuffmanTable.SetCount(const Value: integer);
begin
  SetLength(FItems, Value);
end;

{ TsdHuffmanTableList }
procedure TsdHuffmanTableList.Clear;
Var
  I:Integer;
begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Tables[I]);
  _Count := 0;
end;

function TsdHuffmanTableList.GetItems(Index: integer): TsdHuffmanTable;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Tables, _Count);

    Result := TsdHuffmanTable.Create;
    _Tables[Index] := Result;
  End Else
    Result := _Tables[Index];
end;

procedure TsdHuffmanTableList.Release;
begin
  Clear;
  inherited;
end;

{ TsdQuantizationTable }
procedure TsdQuantizationTable.Transpose;
var
  x, y, i, j: integer;
  Temp: word;
begin
  // transpose indices in table, but we must do this with the forward zigzag
  for y := 0 to 6 do
    for x := y + 1 to 7 do
    begin
      i := cJpegForwardZigZag8x8[x + y * 8];
      j := cJpegForwardZigZag8x8[x * 8 + y];
      Temp := FQuant[i];
      FQuant[i] := FQuant[j];
      FQuant[j] := Temp;
    end;
end;

{ TsdQuantizationTableList }
procedure TsdQuantizationTableList.Clear;
Var
  I:Integer;
begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Tables[I]);

  _Count := 0;
end;

function TsdQuantizationTableList.GetItems(Index: integer): TsdQuantizationTable;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Tables, _Count);

    Result := TsdQuantizationTable.Create;
    _Tables[Index] := Result;
  End Else
    Result := _Tables[Index];
end;

procedure TsdQuantizationTableList.Release;
begin
  Clear;
  Inherited;
end;

{ TsdFrameComponentList }
procedure TsdFrameComponentList.Clear;
Var
  I:Integer;
begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Frames[I]);

  _Count := 0;
end;

function TsdFrameComponentList.GetItems(Index: integer): TsdFrameComponent;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Frames, _Count);

    Result := TsdFrameComponent.Create;
    _Frames[Index] := Result;
  End Else
    Result := _Frames[Index];
end;

procedure TsdFrameComponentList.Release;
begin
  Clear;
  inherited;
end;

{ TsdScanComponentList }
procedure TsdScanComponentList.Clear;
Var
  I:Integer;
begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Scans[I]);
  _Count := 0;
end;

function TsdScanComponentList.GetItems(Index: integer): TsdScanComponent;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Scans, _Count);

    Result := TsdScanComponent.Create;
    _Scans[Index] := Result;
  End Else
    Result := _Scans[Index];
end;

procedure TsdScanComponentList.Release;
begin
  Clear;
  inherited;

end;

{ TsdJpegInfo }
procedure TsdJpegInfo.Clear;
begin
  // Clear all data in Info
  FDCHuffmanTables.Clear;
  FACHuffmanTables.Clear;
  FQuantizationTables.Clear;
  FFrames.Clear;
  FScans.Clear;

  FFrameCount := 0;
  FScanCount := 0;
  FHorzSamplingMax := 0;
  FVertSamplingMax := 0;
  FRestartInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FEncodingMethod := emUnspecified;
  FSamplePrecision := 0;
  FSpectralStart := 0;
  FSpectralEnd := 0;
  FApproxHigh := 0;
  FApproxLow := 0;
  FWaitForDNL := False;
end;

constructor TsdJpegInfo.Create;
begin
  inherited Create;
  FDCHuffmanTables := TsdHuffmanTableList.Create;
  FACHuffmanTables := TsdHuffmanTableList.Create;
  FQuantizationTables := TsdQuantizationTableList.Create;
  FFrames := TsdFrameComponentList.Create;
  FScans := TsdScanComponentList.Create;
end;

Procedure TsdJpegInfo.Release;
Begin
  ReleaseObject(FDCHuffmanTables);
  ReleaseObject(FACHuffmanTables);
  ReleaseObject(FQuantizationTables);
  ReleaseObject(FFrames);
  ReleaseObject(FScans);
  inherited;
End;

{ TsdMapIterator }
procedure TsdMapIterator.Assign(Source:TERRAObject);
var
  S: TsdMapIterator;
begin
  if Source is TsdMapIterator then
  begin
    S := TsdMapIterator(Source);
    FCount := S.FCount;
    FDelta := S.FDelta;
    FLine := S.FLine;
    FLineFirst := S.FLineFirst;
    FThis := S.FThis;
    FMap := S.FMap;
    FWidth := S.FWidth;
    FHeight := S.FHeight;
    FScanStride := S.FScanStride;
    FCellStride := S.FCellStride;
    FMethod := S.FMethod;
  end else
    inherited;
end;

function TsdMapIterator.At(X, Y: integer): pbyte;
begin
  Result := FMap;
  inc(Result, X * FCellStride + Y * FScanStride);
end;

function TsdMapIterator.First: pbyte;
begin
  FThis := FMap;
  FLine := 0;

  // Iterator method
  case FMethod of
  imReaderX:
    begin
      if IsDirect then
        FCount := FWidth * FHeight
      else
        FCount := FWidth;
      FDelta := FCellStride;
    end;
  imReaderXInv:
    begin
      FCount := FWidth;
      FDelta := -FCellStride;
      inc(FThis, (FWidth - 1) * FCellStride);
    end;
  imReaderXBtm:
    begin
      FCount := FWidth;
      FDelta := FCellStride;
      inc(FThis, (FHeight - 1) * FScanStride);
    end;
  imReaderY, imColByCol:
    begin
      FCount := FHeight;
      FDelta := FScanStride;
    end;
  imReaderYInv:
    begin
      FCount := FHeight;
      FDelta := -FScanStride;
      inc(FThis, (FHeight - 1) * FScanStride);
    end;
  imLineByLine, imZigZag:
    begin
      FCount := FWidth;
      FDelta := FCellStride;
    end;
  else
    FThis := nil;
  end;

  FLineFirst := FThis;
  Result := FThis;
end;

function TsdMapIterator.GetDirection: integer;
begin
  if FDelta > 0 then
    Result := 1
  else
    Result := -1;
end;

procedure TsdMapIterator.IncrementMap(AChannel: integer);
begin
  inc(FMap, AChannel);
end;

function TsdMapIterator.IntAt(X, Y: integer): pinteger;
begin
  Result := pinteger(At(X, Y));
end;

function TsdMapIterator.IsDirect: boolean;
begin
  Result := (FCellStride * FWidth = FScanStride);
end;

function TsdMapIterator.Next: pbyte;
begin
  dec(FCount);

  // increment the pointer by delta as long as there are enough iterations
  // (FCount > 0)
  if FCount > 0 then
  begin
    // increment
    inc(FThis, FDelta);
  end else
  begin

    // No more simple iterations are available (FCount <= 0), so now we look at
    // the iterator method
    case FMethod of

    imLineByLine, imColByCol:
      begin
        // Just one line or col, so we exit with nil
        Result := nil;
        exit;
      end;

    imReaderX, imReaderXInv:
      begin
        inc(FLine);
        if (FLine = FHeight) or IsDirect then
        begin
          Result := nil;
          exit;
        end;
        inc(FLineFirst, FScanStride);
        FThis := FLineFirst;
        FCount := FWidth;
      end;

    imReaderXBtm:
      begin
        inc(FLine);
        if FLine = FHeight then
        begin
          Result := nil;
          exit;
        end;
        dec(FLineFirst, FScanStride);
        FThis := FLineFirst;
        FCount := FWidth;
      end;

    imReaderY, imReaderYInv:
      begin
        inc(FLine);
        if FLine = FWidth then
        begin
          Result := nil;
          exit;
        end;
        inc(FLineFirst, FCellStride);
        FThis := FLineFirst;
        FCount := FHeight;
      end;

    imZigZag:
      begin
        inc(FLine);
        if FLine = FHeight then
        begin
          Result := nil;
          exit;
        end;
        inc(FLineFirst, FScanStride);
        FThis := FLineFirst;
        if odd(FLine) then
          inc(FThis, FCellStride * (FWidth - 1));
        FDelta := -FDelta;
        FCount := FWidth;
      end;
    end;
  end;
  Result := FThis;
end;

function TsdMapIterator.SafeAt(X, Y: integer): pbyte;
begin
  Result := nil;

  // Check dimensions
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
    exit;

  // Dimensions ok, so call At
  Result := At(X, Y);
end;

function TsdMapIterator.SingleAt(X, Y: integer): psingle;
begin
  Result := Psingle(At(X, Y));
end;

{ TsdColorTransform }
constructor TsdColorTransform.Create;
// We need a virtual constructor so it can be overridden
begin
  inherited Create;
end;

{ TsdJpegMarker }
constructor TsdJpegMarker.Create(ACodingInfo: TsdJpegInfo; ATag: byte);
begin
  inherited Create;
  FCodingInfo := ACodingInfo;
  FMarkerTag := ATag;
  FStream := MemoryStream.Create();
end;

Procedure TsdJpegMarker.Release;
begin
  ReleaseObject(FStream);
  inherited;
end;

class function TsdJpegMarker.GetByte(S: TERRAStream): byte;
begin
  S.ReadByte(Result);
end;

function TsdJpegMarker.GetMarkerName: Utf8String;
begin
  Result := HexStr(FMarkerTag);
end;

class function TsdJpegMarker.GetWord(S: TERRAStream): word;
var
  W: word;
begin
  S.ReadWord(W);
  Result := Swap(W);
end;

procedure TsdJpegMarker.LoadFromStream(S:TERRAStream; Size: integer);
begin
  //DoDebugOut(Self, wsInfo, Format('<loading marker %s, length:%d>', [MarkerName, Size]));
  // by default, we copy the marker data to the marker stream,
  // overriding methods may use other means
  StoreData(S, Size);
  // Read the marker (default does nothing but is overridden in descendants)
  ReadMarker;
end;

class procedure TsdJpegMarker.PutByte(S: TERRAStream; B: byte);
begin
  S.WriteByte(B);
end;

class procedure TsdJpegMarker.PutWord(S: TERRAStream; W: word);
begin
  W := Swap(W);
  S.WriteWord(W);
end;

procedure TsdJpegMarker.ReadMarker;
begin
// default does nothing
end;

procedure TsdJpegMarker.SaveToStream(S: TERRAStream);
begin
  // the default SaveToStream method. If the marker was modified, the FStream was already
  // updated with .WriteMarker
  if FStream.Size > 0 then
  begin
    //DoDebugOut(Self, wsInfo, Format('saving marker %s, length:%d', [MarkerName, FStream.Size]));
    FStream.Position := 0;
    FStream.Copy(S);
  end;
end;

procedure TsdJpegMarker.StoreData(S: TERRAStream; Size: integer);
begin
  // We store the data for later use
  FStream.Seek(0);
  S.Copy(FStream, S.Position, Size);
  FStream.Position := 0;
end;

procedure TsdJpegMarker.WriteMarker;
begin
// default does nothing
end;

// Added by Dec begin
class function TsdJpegMarker.GetSignature: AnsiString;
begin
  Result := '';
end;

class function TsdJpegMarker.GetMarker: Byte;
begin
  Result := 0;
end;

class function TsdJpegMarker.IsSegment(AMarker: Byte; AStream: TERRAStream): Boolean;
var
  S: Word;
  Sign: AnsiString;
begin
  Result := AMarker = GetMarker;
  if not Result then
    Exit;
  Sign := GetSignature;
  if Sign = '' then
    Exit;
  S := GetWord(AStream);
  Result := S >= Length(Sign);
  if not Result then
    Exit;
  AStream.Read(@Sign[1], Length(Sign));
  Result := Sign = GetSignature;
end;
// Added by Dec end

{ TsdDHTMarker }
function TsdDHTMarker.GetMarkerName: Utf8String;
begin
  Result := 'DHT';
end;

procedure TsdDHTMarker.ReadMarker;
var
  i, j, Idx, Count, InfoCount: integer;
  Item: PsdDHTMarkerInfo;
  B: byte;
  Table: TsdHuffmanTable;
begin
  // Define Huffman Table
  SetLength(FMarkerInfo, 0);
  InfoCount := 0;
  repeat
    SetLength(FMarkerInfo, InfoCount + 1);
    Item := @FMarkerInfo[InfoCount];
    inc(InfoCount);
    B := GetByte(FStream);
    Item.Tc := B shr 4;
    Item.Th := B and $0F;

    // Number of elements for each bitsize
    FStream.Read(@Item.BitLengths[0], 16);

    // Count values
    Count := 0;
    for i := 0 to 15 do
      inc(Count, Item.BitLengths[i]);

    // Set pointer and table info
    Table := nil;
    case Item.Tc of
    0:
      begin
        Table := TsdHuffmanTableList(FCodingInfo.FDCHuffmanTables)[Item.Th];
        //DoDebugOut(Self, wsInfo, Format('DC Huffman table=%d, length=%d', [Item.Th, Count]));
      end;
    1:
      begin
        Table := TsdHuffmanTableList(FCodingInfo.FACHuffmanTables)[Item.Th];
        //DoDebugOut(Self, wsInfo, Format('AC Huffman table=%d, length=%d', [Item.Th, Count]));
      end;
    else
      //DoDebugOut(Self, wsFail, sInvalidTableClass);
    end;

    // Set table length
    if assigned(Table) then
    begin
      Table.Count := Count;
      SetLength(Item.BitValues, Count);

      // Read values
      Idx := 0;
      for i := 0 to 15 do
      begin
        for j := 0 to Item.BitLengths[i] - 1 do
        begin
          Table[Idx].L := i + 1;
          Item.BitValues[Idx] := GetByte(FStream);
          Table[Idx].V := Item.BitValues[Idx];
          inc(Idx);
        end;
      end;
    end;
  until (FStream.Position = FStream.Size) or (Table = nil);
end;

procedure TsdDHTMarker.WriteMarker;
var
  i, Count: integer;
  B: byte;
  Item: PsdDHTMarkerInfo;
  Table: TsdHuffmanTable;
  //local
  procedure SetTableValues;
  var
    i, j, Idx: integer;
  begin
    Idx := 0;
    for i := 0 to 15 do
      for j := 0 to Item.BitLengths[i] - 1 do
      begin
        Table[Idx].L := i + 1;
        Table[Idx].V := Item.BitValues[Idx];
        inc(Idx);
      end;
  end;
//main
begin
  FStream.Seek(0);
  for i := 0 to length(FMarkerInfo) - 1 do
  begin
    Item := @FMarkerInfo[i];
    B := Item.Tc shl 4 + Item.Th;
    PutByte(FStream, B);
    case Item.Tc of
    0: Table := TsdHuffmanTableList(FCodingInfo.FDCHuffmanTables)[Item.Th];
    1: Table := TsdHuffmanTableList(FCodingInfo.FACHuffmanTables)[Item.Th];
    end;
    Count := length(Item.BitValues);
    // Set table length
    Table.Count := Count;
    SetTableValues;
    // Number of elements for each bitsize
    FStream.Write(@Item.BitLengths[0], 16);
    // Write values
    if Count > 0 then
      FStream.Write(@Item.BitValues[0], Count);
  end;
end;

{ TsdJpegMarkerList }
constructor TsdJpegMarkerList.Create(AOwner: TERRAObject);
begin
  FOwner := AOwner;
end;

procedure TsdJpegMarkerList.Add(AItem:TsdJpegMarker);
begin
  Inc(_MarkerCount);
  SetLength(_Markers, _MarkerCount);
  _Markers[Pred(_MarkerCount)] := AItem;
  AItem.Owner := FOwner;
end;

function TsdJpegMarkerList.ByTag(AMarkerTag: byte): TsdJpegMarker;
var
  I:Integer;
begin
  Result := nil;

  For I:=0 To Pred(_MarkerCount) Do
    if _Markers[i].MarkerTag = AMarkerTag then
    begin
      Result := Items[i];
      exit;
    end;
end;

function TsdJpegMarkerList.ByClass(AClass: TsdJpegMarkerClass): TsdJpegMarker;
var
  i: integer;
begin
  Result := nil;
  For I:=0 To Pred(_MarkerCount) Do
    if _Markers[i] is AClass then
    begin
      Result := Items[i];
      exit;
    end;
end;


function TsdJpegMarkerList.GetItems(Index: integer): TsdJpegMarker;
begin
  If (Index>=0) And (Index<_MarkerCount) Then
    Result := _Markers[Index]
  Else
    Result := Nil;
end;

function TsdJpegMarkerList.HasMarker(ASet: TsdJpegMarkerSet): boolean;
var
  i: integer;
begin
  Result := False;
  For I:=0 To Pred(_MarkerCount) Do
    if _Markers[i].MarkerTag in ASet then
    begin
      Result := True;
      exit;
    end;
end;

procedure TsdJpegMarkerList.InsertAfter(ASet: TsdJpegMarkerSet; AMarker: TsdJpegMarker);
var
  i, N: integer;
begin
  N := -1;
  For I:=0 To Pred(_MarkerCount) Do
    if _Markers[i].MarkerTag in ASet then
    begin
      N := I;
      Break;
    end;

  If N>=0 Then
    Insert(N, AMarker)
  Else
    Add(AMarker);
end;

procedure TsdJpegMarkerList.Insert(Pos:Integer; Marker: TsdJpegMarker);
Var
  I:Integer;
Begin
  Add(Marker);

  For I:=Pred(_MarkerCount) DownTo Pos Do
    _Markers[I] := _Markers[I-1];

  _Markers[Pos] := Marker;
End;

procedure TsdJpegMarkerList.RemoveMarkers(ASet: TsdJpegMarkerSet);
var
  i: integer;
begin
  I := 0;

  While (I<_MarkerCount) Do
  if _Markers[i].MarkerTag in ASet then
  Begin
    _Markers[i] := _Markers[Pred(_MarkerCount)];
    Dec(_MarkerCount);
  End Else
    Inc(I);
End;

procedure TsdJpegMarkerList.Clear;
begin
  _MarkerCount := 0;
end;

Function TsdJpegMarkerList.Extract(Marker: TsdJpegMarker):TsdJpegMarker;
var
  i: integer;
begin
  I := 0;

  Result := Marker;

  While (I<_MarkerCount) Do
  if _Markers[i] = Marker then
  Begin
    _Markers[i] := _Markers[Pred(_MarkerCount)];
    Dec(_MarkerCount);
    Exit;
  End Else
    Inc(I);
end;

{ TsdICCProfileMarker }
class function TsdICCProfileMarker.GetSignature: AnsiString;
begin
  Result := 'ICC_PROFILE'#0;
end;

class function TsdICCProfileMarker.GetMarker: Byte;
begin
  Result := $E2;
end;

function TsdICCProfileMarker.GetCurrentMarker: byte;
begin
  GetIsValid;
  Result := FCurrentMarker;
end;

function TsdICCProfileMarker.GetData: pointer;
var
  PData: PByte;
begin
  GetIsValid;
  if not FIsValid then
    Result := nil
  else
  begin
    PData := FStream.Buffer;
    inc(PData, 14);
    Result := PData;
  end;
end;

function TsdICCProfileMarker.GetDataLength: integer;
begin
  GetIsValid;
  if not FIsValid then
    Result := 0
  else
    Result := FStream.Size - 14;
end;

function TsdICCProfileMarker.GetIsValid: boolean;
var
  Magic: array[0..11] of AnsiChar;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;
  FStream.Position := 0;
  FStream.Read(@Magic, 12);
  FIsValid := (Magic = 'ICC_PROFILE');
  if not FIsValid then
    exit;
  Result := True;
  FCurrentMarker := GetByte(FStream);
  FMarkerCount := GetByte(FStream);
  // ICC-Profile data follows
end;

function TsdICCProfileMarker.GetMarkerCount: byte;
begin
  GetIsValid;
  Result := FMarkerCount;
end;

procedure TsdICCProfileMarker.SetCurrentMarker(const Value: byte);
begin
  FStream.Position := 12;
  PutByte(FStream, Value);
  FCurrentMarker := Value;
end;

procedure TsdICCProfileMarker.SetDataLength(const Value: integer);
var
  Magic: AnsiString;
begin
  FStream.Resize(Value + 14);
  FStream.Position := 0;
  Magic := 'ICC_PROFILE'#0;
  FStream.Write(@Magic[1], 12);
end;

procedure TsdICCProfileMarker.SetMarkerCount(const Value: byte);
begin
  FStream.Position := 13;
  PutByte(FStream, Value);
  FMarkerCount := Value;
end;

function TsdICCProfileMarker.GetMarkerName: Utf8String;
begin
  Result := 'ICCProfile';
end;


{ TsdJpegCoder }
procedure TsdJpegCoder.Clear;
begin
  FHasCoefficients := False;
  FHasSamples := False;
  FScale := jsFull;
end;

constructor TsdJpegCoder.Create(AOwner: TERRAObject; AInfo: TsdJpegInfo);
begin
  inherited Create;

  FInfo := AInfo;
end;

function TsdJpegCoder.CreateDHTMarker: TsdDHTMarker;
begin
  Result := nil;
end;

procedure TsdJpegCoder.Finalize;
begin
// default does nothing
end;

procedure TsdJpegCoder.Initialize(AScale: TsdJpegScale);
begin
  FScale := AScale;
end;

{ TsdJpegICCProfile }
function TsdJpegICCProfile.GetData: pointer;
begin
  if length(FData) > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TsdJpegICCProfile.GetDataLength: integer;
begin
  Result := length(FData);
end;

procedure TsdJpegICCProfile.LoadFromStream(S: TERRAStream);
begin
  SetLength(FData, S.Size);
  S.Position := 0;
  S.Read(@FData[0], S.Size);
end;

procedure TsdJpegICCProfile.ReadFromMarkerList(AList: TsdJpegMarkerList);
var
  i, j, DataLen, MarkerCount: integer;
  Markers: array of TsdICCProfileMarker;
  M: TsdICCProfileMarker;
  P: PByte;
begin
  // Determine total length and get list of markers
  DataLen := 0;
  MarkerCount := 0;
  SetLength(Markers, AList.Count);
  for i := 0 to AList.Count - 1 do
    if AList[i] is TsdICCProfileMarker then
    begin
      M := TsdICCProfileMarker(AList[i]);
      if not M.IsValid then continue;
      inc(DataLen, M.DataLength);
      Markers[MarkerCount] := M;
      inc(MarkerCount);
    end;
  if DataLen <= 0 then exit;

  // Sort markers by index
  for i := 0 to MarkerCount - 2 do
    for j := i + 1 to MarkerCount - 1 do
      if Markers[i].CurrentMarker > Markers[j].CurrentMarker then
      begin
        M := Markers[i];
        Markers[i] := Markers[j];
        Markers[j] := M;
      end;

  // Extract marker data into our data
  SetLength(FData, DataLen);
  P := @FData[0];
  for i := 0 to MarkerCount - 1 do
  begin
    Move(Markers[i].Data^, P^, Markers[i].DataLength);
    inc(P, Markers[i].DataLength);
  end;
end;

procedure TsdJpegICCProfile.SaveToStream(S: TERRAStream);
begin
  if length(FData) > 0 then
    S.Write(@FData[0], length(FData))
end;

procedure TsdJpegICCProfile.WriteToMarkerList(AList: TsdJpegMarkerList);
const
  cChunkSize = 60000;
var
  i, Count, Chunk, Left, Base: integer;
  Markers: array of TsdICCProfileMarker;
  P: Pbyte;
begin
  // Create an array of markers with the profile data
  Count := (DataLength + cChunkSize - 1) div cChunkSize;
  Left := DataLength;
  P := Data;
  SetLength(Markers, Count);
  for i := 0 to Count - 1 do
  begin
    Markers[i] := TsdICCProfileMarker.Create(nil, mkApp2);
    Chunk := IntMin(Left, cChunkSize);
    Markers[i].DataLength := Chunk;
    Move(P^, Markers[i].Data^, Chunk);
    Markers[i].CurrentMarker := i + 1;
    Markers[i].MarkerCount := Count;
    inc(P, Chunk);
    dec(Left, Chunk);
  end;
  // Insert them into the markerlist
  Base := IntMin(AList.Count, 2);
  for i := Count - 1 downto 0 do
    AList.Insert(Base, Markers[i]);
end;

{ TsdJpegSaveOptions }

procedure TsdJpegSaveOptions.AddMarkers(AStored: TsdJpegColorSpace;
  AWidth, AHeight: integer);
var
  i: integer;
  Info: TsdJpegInfo;
  Frame: TsdFrameComponent;
  SOF: TsdSOFnMarker;
  SOS: TsdSOSMarker;
begin
  // Set the correct FInfo fields
  Info := FOwner.FJpegInfo;
  Info.FWidth := AWidth;
  Info.FHeight := AHeight;
  Info.FSamplePrecision := 8;
  case AStored of
  jcGray:
    Info.FFrameCount := 1;
  jcGrayA:
    Info.FFrameCount := 2;
  jcRGB, jcYCbCr, jcPhotoYCC:
    Info.FFrameCount := 3;
  jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
    Info.FFrameCount := 4;
  else
    Engine.RaiseError('JPG '+sUnsupportedColorSpace);
  end;

  // Subsampling used?
  case AStored of
  jcYCbCr, jcYCbCrA, jcYCCK, jcPhotoYCC, jcPhotoYCCA:
    FUseSubSampling := True
  else
    FUseSubSampling := False;
  end;

  // Set up frame sampling
  for i := 0 to Info.FFrameCount - 1 do
  begin
    Frame := Info.FFrames[i];
    if (i in [1, 2]) then
    begin
      // Subsampled frame
      Frame.FHorzSampling := 1;
      Frame.FVertSampling := 1;
      if FUseSubSampling then
        Frame.FQTable := 1
      else
        Frame.FQTable := 0;
    end else
    begin
      // Full frame
      if FUseSubSampling then
      begin
        Frame.FHorzSampling := 2;
        Frame.FVertSampling := 2;
      end else
      begin
        Frame.FHorzSampling := 1;
        Frame.FVertSampling := 1;
      end;
      Frame.FQTable := 0;
    end;
    Frame.FComponentID := i + 1;
  end;
  if FUseSubSampling then
  begin
    Info.FHorzSamplingMax := 2;
    Info.FVertSamplingMax := 2;
  end else
  begin
    Info.FHorzSamplingMax := 1;
    Info.FVertSamplingMax := 1;
  end;

  // Setup and add quant tables
  SetupQuantTables;

  // Create and add SOFn marker
  SOF := TsdSOFnMarker.Create(Info, mkSOF0);
  FOwner.Markers.Add(SOF);

  // Create and add default Huffman tables if required
  if not OptimizeHuffmanTables then
    SetupDefaultHuffmanTables;

  // Create and add SOS marker
  SOS := TsdSOSMarker.Create(Info, mkSOS);
  FOwner.Markers.Add(SOS);

  SetLength(SOS.FMarkerInfo, Info.FFrameCount);
  SOS.FScanCount := Info.FFrameCount;
  for i := 0 to Info.FFrameCount - 1 do
  begin
    SOS.FMarkerInfo[i].ComponentID := i + 1;
    SOS.FMarkerInfo[i].DCTable := Info.FFrames[i].FQTable;
    SOS.FMarkerInfo[i].ACTable := Info.FFrames[i].FQTable;
  end;

end;

constructor TsdJpegSaveOptions.Create(AOwner: TsdJpegImage);
begin
  inherited Create;
  FOwner := AOwner;
  FQuality := cDefaultJpgCompressionQuality;
  FOptimizeHuffmanTables := True;
  FCodingMethod := emBaselineDCT;
  FUseSubSampling := True;
end;

procedure TsdJpegSaveOptions.SetTableMultiplication(ATable: TsdQuantizationTable;
  MultiplyPercent: integer; const ADefaultTable: TsdIntArray64);
var
  i, Q: integer;
begin
  for i := 0 to 63 do
  begin
    Q := (ADefaultTable[cJpegInverseZigZag8x8[i]] * MultiplyPercent + 50) div 100;
    // ensure that quant factor is in valid range
    if Q <= 0 then
      Q := 1
    else
      if Q > 255 then
        Q := 255;
    // set table quant factor i
    ATable.FQuant[i] := Q;
  end;
end;

procedure TsdJpegSaveOptions.SetupDefaultHuffmanTables;
var
  M: TsdDHTMarker;
  procedure FillTable(var Info: TsdDHTMarkerInfo; Bits, Values: Pbyte; Tc, Th: byte);
  var
    i, Count: integer;
  begin
    Count := 0;
    Info.Tc := Tc;
    Info.Th := Th;
    for i := 0 to 15 do
    begin
      Info.BitLengths[i] := Bits^;
      inc(Count, Bits^);
      inc(Bits);
    end;
    SetLength(Info.BitValues, Count);
    for i := 0 to Count - 1 do
    begin
      Info.BitValues[i] := Values^;
      inc(Values);
    end;
  end;
begin
  M := TsdDHTMarker.Create(FOwner.FJpegInfo, mkDHT);
  if FUseSubsampling then
    SetLength(M.FMarkerInfo, 4)
  else
    SetLength(M.FMarkerInfo, 2);

  // Luminance tables (always used)
  FillTable(M.FMarkerInfo[0], @cHuffmanBitsDcLum[0], @cHuffmanValDcLum[0], 0, 0);
  FillTable(M.FMarkerInfo[1], @cHuffmanBitsAcLum[0], @cHuffmanValAcLum[0], 1, 0);

  if FUseSubsampling then
  begin
    // Chrominance tables (only when subsampling is used)
    FillTable(M.FMarkerInfo[2], @cHuffmanBitsDcChrom[0], @cHuffmanValDcChrom[0], 0, 1);
    FillTable(M.FMarkerInfo[3], @cHuffmanBitsAcChrom[0], @cHuffmanValAcChrom[0], 1, 1);
  end;
  FOwner.Markers.Add(M);
end;

procedure TsdJpegSaveOptions.SetupQuantTables;
var
  QMul: integer;
  T: TsdQuantizationTable;
  M: TsdDQTMarker;
begin
  if FQuality < 1 then
    FQuality := 1
  else
    if FQuality > 100 then
      FQuality := 100;

  // Calculation of quant multiplication factor
  if FQuality < 50 then
    QMul := 5000 div FQuality
  else
    QMul := 200 - FQuality * 2;

  // Create DQT marker
  M := TsdDQTMarker.Create(FOwner.FJpegInfo, mkDQT);
  if FUseSubSampling then
    SetLength(M.FTableIndices, 2)
  else
    SetLength(M.FTableIndices, 1);

  // Quant table 0
  T := FOwner.FJpegInfo.FQuantizationTables[0];
  SetTableMultiplication(T, QMul, cStdLuminanceQuantTbl);
  M.FTableIndices[0] := 0;

  // Quant table 1
  if FUseSubSampling then
  begin
    T := FOwner.FJpegInfo.FQuantizationTables[1];
    SetTableMultiplication(T, QMul, cStdChrominanceQuantTbl);
    M.FTableIndices[1] := 1;
  end;

  // Add DQT marker
  FOwner.Markers.Add(M);
end;

{ TsdDQTMarker }
function TsdDQTMarker.GetMarkerName: Utf8String;
begin
  Result := 'DQT';
end;

procedure TsdDQTMarker.ReadMarker;
var
  i, Count: integer;
  B: byte;
  P, T: byte;
  Table: TsdQuantizationTable;
  function TabVal(x: integer): integer;
  begin
    Result := Table.FQuant[cJpegForwardZigZag8x8[i * 8 + x]];
  end;
begin
  // Define Quantization Table
  // Read quantization table(s)
  SetLength(FTableIndices, 0);
  Count := 0;
  repeat
    B := GetByte(FStream);
    P := B shr 4;
    T := B and $0F;
    // Store for later use
    SetLength(FTableIndices, Count + 1);
    FTableIndices[Count] := T;
    inc(Count);
    // Initialize table
    Table := FCodingInfo.FQuantizationTables[T];
    Table.FPrecision := TsdQuantizationPrecision(P);
    case P of
    0:
      for i := 0 to 63 do
        Table.FQuant[i] := GetByte(FStream);
    1:
      for i := 0 to 63 do
        Table.FQuant[i] := GetWord(FStream);
    end;//case

    (*
    case Table.FPrecision of
    qp8bit: DoDebugOut(Self, wsInfo, Format('QTable=%d precision=8bit', [T]));
    qp16bit: DoDebugOut(Self, wsInfo, Format('QTable=%d precision=16bit', [T]));
    end;

    for i := 0 to 7 do
      DoDebugOut(Self, wsInfo, Format('%3d %3d %3d %3d %3d %3d %3d %3d',
        [TabVal(0), TabVal(1), TabVal(2), TabVal(3), TabVal(4), TabVal(5), TabVal(6), TabVal(7)]));
        *)
  until FStream.Position = FStream.Size;
end;

procedure TsdDQTMarker.WriteMarker;
var
  i, j: integer;
  B: byte;
  Table: TsdQuantizationTable;
begin
  FStream.Seek(0);
  for i := 0 to length(FTableIndices) - 1 do
  begin
    Table := FCodingInfo.FQuantizationTables[FTableIndices[i]];
    case Table.FPrecision of
    qp8bit:  B := $00;
    qp16bit: B := $10;
    end;
    B := B or FTableIndices[i];
    FStream.WriteByte(B);
    // Write table
    case Table.FPrecision of
    qp8bit:
      for j := 0 to 63 do
        PutByte(FStream, Table.FQuant[j]);
    qp16bit:
      for j := 0 to 63 do
        PutWord(FStream, Table.FQuant[j]);
    end;
  end;
end;

{ TsdSOFnMarker }
function TsdSOFnMarker.GetMarkerName: Utf8String;
begin
  Result := 'SOF' + HexStr(Byte(MarkerTag and $0F));
end;

procedure TsdSOFnMarker.ReadMarker;
var
  i: integer;
  B, Nf: byte;
  Frame: TsdFrameComponent;
begin
  // start of frame x
  inherited;

  // Determine encoding
  //DoDebugOut(Self, wsInfo, Format('SOFn marker: %x', [MarkerTag]));

  case MarkerTag of
  mkSOF0:
    begin
      FCodingInfo.FEncodingMethod := emBaselineDCT;
      //DoDebugOut(Self, wsInfo, 'coding method: baseline DCT (SOF0)');
    end;
  mkSOF1:
    begin
      FCodingInfo.FEncodingMethod := emExtendedDCT;
      //DoDebugOut(Self, wsInfo, 'coding method: extended DCT (SOF1)');
    end;
  mkSOF2:
    begin
      FCodingInfo.FEncodingMethod := emProgressiveDCT;
      //DoDebugOut(Self, wsInfo, 'coding method: progressive DCT (SOF2)');
    end;
  mkSOF3, mkSOF5..mkSOF7, mkSOF9..mkSOF11, mkSOF13..mkSOF15:
    begin
      // we do not yet support anything fancy
      //DoDebugOut(Self, wsWarn, Format(sUnsupportedEncoding, [(MarkerTag and $0F)]));
      exit;
    end;
  else
    begin
      // unknown encoding
      //DoDebugOut(Self, wsWarn, Format('unknown encoding %x', [MarkerTag]));
      exit;
    end;
  end;//case

  // Read Frame Header
  FCodingInfo.FSamplePrecision := GetByte(FStream);
  FCodingInfo.FHeight := GetWord(FStream);
  FCodingInfo.FWidth := GetWord(FStream);

  // The weird case of FInfo.Y = 0: we expect a DNL marker somewhere telling
  // us the actual Y dimension. We set WaitForDNL to true.
  if FCodingInfo.FHeight = 0 then
    FCodingInfo.FWaitForDNL := True;

  // Variable Nf: Number of image components in frame
  Nf := GetByte(FStream);
  FCodingInfo.FFrameCount := Nf;
  //DoDebugOut(Self, wsInfo, Format('Image %dx%d, %d frames, %dbit samples',  [FCodingInfo.FWidth, FCodingInfo.FHeight, FCodingInfo.FFrameCount, FCodingInfo.FSamplePrecision]));

  for i := 0 to Nf - 1 do
  begin
    Frame := FCodingInfo.FFrames[i];
    Frame.FComponentID := GetByte(FStream);  // Image component (can be ASCII too!)
    B := GetByte(FStream);
    if Nf = 1 then
    begin
      // Jpeg spec specifies that with just one frame (no interlace), we need to
      // have a 1x1 MCU, so these ones should be 1, even if they read differently.
      Frame.FHorzSampling := 1;
      Frame.FVertSampling := 1;
    end else
    begin
      Frame.FHorzSampling := B shr 4;     // Horizontal blocksize in MCU
      Frame.FVertSampling := B and $0F;   // Vertical blocksize in MCU
    end;
    Frame.FQTable := GetByte(FStream); // Index into quantization table array
    //DoDebugOut(Self, wsInfo, Format('Frame %d: %dx%d sampling ID=%s QTable=%d', [i, Frame.FHorzSampling, Frame.FVertSampling, IntToHex(Frame.FComponentID, 2), Frame.FQTable]));
  end;
end;

procedure TsdSOFnMarker.WriteMarker;
var
  i: integer;
  B: byte;
  Frame: TsdFrameComponent;
begin
  FStream.Seek(0);
  // Write Frame Header
  PutByte(FStream, FCodingInfo.FSamplePrecision);
  PutWord(FStream, FCodingInfo.FHeight);
  PutWord(FStream, FCodingInfo.FWidth);
  PutByte(FStream, FCodingInfo.FFrameCount);
  for i := 0 to FCodingInfo.FFrameCount - 1 do
  begin
    Frame := FCodingInfo.FFrames[i];
    PutByte(FStream, Frame.FComponentID);
    B := Frame.FHorzSampling shl 4 + Frame.FVertSampling;
    PutByte(FStream, B);
    PutByte(FStream, Frame.FQTable);
  end;
end;

{ TsdSOSMarker }

procedure TsdSOSMarker.FindScanComponent(AScan: TsdScanComponent; AId: byte);
var
  i: integer;
begin
  // Let's find the index of the component this one belongs to
  AScan.FComponent := -1;
  for i := 0 to FCodingInfo.FFrameCount - 1 do
    if FCodingInfo.FFrames[i].FComponentID = AId then
      AScan.FComponent := i;

  // Make sure we have a frame for this scan
(*  if AScan.FComponent = -1 then
    DoDebugOut(Self, wsFail, sInvalidFrameRef);*)
end;

function TsdSOSMarker.GetMarkerName: Utf8String;
begin
  Result := 'SOS';
end;

procedure TsdSOSMarker.ReadMarker;
var
  i: integer;
  B, Cs: byte;
  Scan: TsdScanComponent;
begin
  // Start of Scan
  //DoDebugOut(Self, wsInfo, '<SOS marker>');

  // Variable Ns, number of image components in scan
  FScanCount := GetByte(FStream);
  FCodingInfo.FScanCount := FScanCount;
  FCodingInfo.FScans.Clear;
  SetLength(FMarkerInfo, FScanCount);

  (*if FScanCount = 1 then
    DoDebugOut(Self, wsInfo, 'Single Channel')
  else
    DoDebugOut(Self, wsInfo, Format('Interleaved (%d channels)', [FScanCount]));*)

  // read table specifiers
  for i := 0 to FScanCount - 1 do
  begin
    Scan := FCodingInfo.FScans[i];

    Cs := GetByte(FStream); // Image component reference (can be ASCII too!)
    FMarkerInfo[i].ComponentID := Cs;
    FindScanComponent(Scan, Cs);

    B := GetByte(FStream);
    FMarkerInfo[i].DCTable := B shr 4;   // DC entropy table selector
    FMarkerInfo[i].ACTable := B and $0F; // AC entropy table selector
    Scan.FDCTable := FMarkerInfo[i].DCTable;
    Scan.FACTable := FMarkerInfo[i].ACTable;
    Scan.FPredictor := 0;       // Predictor (used for diff'ing the DC component)
    //DoDebugOut(Self, wsInfo, Format('Channel %d DCTable: %d, ACTable: %d', [Scan.FComponent, Scan.FDCTable, Scan.FACTable]))
  end;

  // read Ss, Se, these are used in progressive scans
  FSpectralStart := GetByte(FStream);
  FCodingInfo.FSpectralStart := FSpectralStart;

  FSpectralEnd := GetByte(FStream);
  FCodingInfo.FSpectralEnd := FSpectralEnd;

  // read Ah, Al, these are used in progressive scans
  B := GetByte(FStream);
  FApproxHigh := B shr 4;
  FCodingInfo.FApproxHigh := FApproxHigh;
  FApproxLow := B and $0F;
  FCodingInfo.FApproxLow  := FApproxLow;

  // Following is entropy coded data
  (*if FCodingInfo.FEncodingMethod = emProgressiveDCT then
    DoDebugOut(Self, wsInfo, Format('Progressive params: Ss=%d, Se=%d, Ah=%d, Al=%d', [FSpectralStart, FSpectralEnd, FApproxHigh, FApproxLow]));*)
end;

procedure TsdSOSMarker.WriteMarker;
// Write SOS data, and also apply it back to FCodingInfo for use by the decoder.
var
  i: integer;
  B: byte;
  Scan: TsdScanComponent;
begin
  FStream.Seek(0);
  PutByte(FStream, FScanCount);

  // write table specifiers
  FCodingInfo.FScanCount := FScanCount;
  for i := 0 to FScanCount - 1 do
  begin
    Scan := FCodingInfo.FScans[i];
    PutByte(FStream, FMarkerInfo[i].ComponentID);
    FindScanComponent(Scan, FMarkerInfo[i].ComponentID);
    B := FMarkerInfo[i].DCTable shl 4 + FMarkerInfo[i].ACTable;
    PutByte(FStream, B);
    Scan.FDCTable := FMarkerInfo[i].DCTable;
    Scan.FACTable := FMarkerInfo[i].ACTable;
    Scan.FPredictor := 0;
  end;

  // Write Ss, Se, Ah and Al
  B := FApproxHigh shl 4 + FApproxLow;
  PutByte(FStream, FSpectralStart);
  PutByte(FStream, FSpectralEnd);
  PutByte(FStream, B);
  FCodingInfo.FSpectralStart := FSpectralStart;
  FCodingInfo.FSpectralEnd := FSpectralEnd;
  FCodingInfo.FApproxHigh := FApproxHigh;
  FCodingInfo.FApproxLow  := FApproxLow;
end;

{ TsdAPPnMarker }
procedure TsdAPPnMarker.ReadMarker;
begin
end;

function TsdAPPnMarker.GetMarkerName: Utf8String;
begin
  Result := 'APP' + IntegerProperty.Stringify(FMarkerTag and $0F);
end;

{ TsdEXIFMarker }
class function TsdEXIFMarker.GetSignature: AnsiString;
begin
  Result := 'EXIF'#0;
end;

class function TsdEXIFMarker.GetMarker: Byte;
begin
  Result := $E1;
end;

function TsdEXIFMarker.GetMarkerName: Utf8String;
begin
  Result := 'EXIF';
end;

{ TsdIPTCMarker }
Function TsdIPTCMarker.GetMarkerName: Utf8String;
begin
  Result := 'IPTC';
end;

{ TsdJFIFMarker }

class function TsdJFIFMarker.GetSignature: AnsiString;
begin
  Result := 'JFIF';
end;

class function TsdJFIFMarker.GetMarker: Byte;
begin
  Result := $E0;
end;

constructor TsdJFIFMarker.Create(AInfo: TsdJpegInfo; ATag: byte);
begin
  inherited;
  // Set sensible defaults
  FVersion := 258;
  FUnits := juXandYAreDotsPerInch;
  FXDensity := 600;
  FYDensity := 600;
  // Save data
  SaveData;
end;

function TsdJFIFMarker.GetIsValid: boolean;
var
  Magic: array[0..4] of AnsiChar;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;
  FStream.Position := 0;
  FStream.Read(@Magic, 5);
  FIsValid := (Magic = 'JFIF');
  if not FIsValid then
    exit;
  Result := True;
  FVersion := GetWord(FStream);
  FStream.ReadByte(Byte(FUnits));
  FXDensity := GetWord(FStream);
  FYDensity := GetWord(FStream);
  FXThumbnail := GetByte(FStream);
  FYThumbnail := GetByte(FStream);
end;

function TsdJFIFMarker.GetUnits: TsdJFIFUnits;
begin
  GetIsValid;
  Result := FUnits;
end;

function TsdJFIFMarker.GetVersion: word;
begin
  GetIsValid;
  Result := FVersion;
end;

function TsdJFIFMarker.GetXDensity: word;
begin
  GetIsValid;
  Result := FXDensity;
end;

function TsdJFIFMarker.GetXThumbnail: byte;
begin
  GetIsValid;
  Result := FXThumbnail;
end;

function TsdJFIFMarker.GetYDensity: word;
begin
  GetIsValid;
  Result := FYDensity;
end;

function TsdJFIFMarker.GetYThumbnail: byte;
begin
  GetIsValid;
  Result := FYThumbnail;
end;

procedure TsdJFIFMarker.SaveData;
var
  Magic: array[0..4] of AnsiChar;
begin
  Magic := 'JFIF'#0;
  FStream.Seek(0);
  FStream.Write(@Magic, 5);
  PutWord(FStream, FVersion);
  FStream.WriteByte(Byte(FUnits));
  PutWord(FStream, FXDensity);
  PutWord(FStream, FYDensity);
  PutByte(FStream, FXThumbnail);
  PutByte(FStream, FYThumbnail);
end;

function TsdJFIFMarker.GetMarkerName: Utf8String;
begin
  Result := 'JFIF';
end;

{ TsdAdobeApp14Marker }

class function TsdAdobeApp14Marker.GetSignature: AnsiString;
begin
  Result := 'Adobe';
end;

class function TsdAdobeApp14Marker.GetMarker: Byte;
begin
  Result := $EE;
end;

constructor TsdAdobeApp14Marker.Create(AInfo: TsdJpegInfo; ATag: byte);
begin
  inherited;
  // Defaults
  FVersion := 100;
  SaveData;
end;

function TsdAdobeApp14Marker.GetIsValid;
var
  Magic: array[0..4] of AnsiChar;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;

  // Check length of Adobe marker
  if FStream.Size <> 12 then
    exit;
  FStream.Position := 0;
  FStream.Read(@Magic, 5);
  FIsValid := (Magic = 'Adobe');
  if not FIsValid then
    exit;

  Result := True;
  FVersion := GetWord(FStream);
  FFlags0 := GetWord(FStream);
  FFlags1 := GetWord(FStream);
  FTransform := GetByte(FStream);
end;

function TsdAdobeApp14Marker.GetTransform: byte;
begin
  GetIsValid;
  Result := FTransform;
end;

procedure TsdAdobeApp14Marker.SaveData;
var
  Magic: array[0..4] of AnsiChar;
begin
  Magic := 'Adobe';
  FStream.Seek(0);
  FStream.Write(@Magic, 5);
  PutWord(FStream, FVersion);
  PutWord(FStream, FFlags0);
  PutWord(FStream, FFlags1);
  PutByte(FStream, FTransform);
end;

procedure TsdAdobeApp14Marker.SetTransform(const Value: byte);
begin
  GetIsValid;
  FTransform := Value;
  SaveData;
end;

function TsdAdobeApp14Marker.GetMarkerName: Utf8String;
begin
  Result := 'APP14';
end;

{ TsdAVI1Marker }
class function TsdAVI1Marker.GetSignature: AnsiString;
begin
  Result := 'AVI1';
end;

class function TsdAVI1Marker.GetMarker: Byte;
begin
  Result := $E0;
end;

function TsdAVI1Marker.GetMarkerName: Utf8String;
begin
  Result := 'AVI1';
end;

{ TsdG3FAXMarker }

class function TsdG3FAXMarker.GetSignature: AnsiString;
begin
  Result := 'G3FAX';
end;

class function TsdG3FAXMarker.GetMarker: Byte;
begin
  Result := $E1;
end;

function TsdG3FAXMarker.GetMarkerName: Utf8String;
begin
  Result := 'G3FAX';
end;

{ TsdJpegImage }

procedure TsdJpegImage.AddMinimalMarkersForColorSpaceDetection(AColors: TsdJpegColorSpace);
var
  M: TsdJpegMarker;
begin
  Markers.Insert(0, TsdSOIMarker.Create(FJpegInfo, mkSOI));
  // JFIF marker if these color spaces
  if AColors in [jcGray, jcYCbCr] then
    Markers.Insert(1, TsdJFIFMarker.Create(FJpegInfo, mkAPP0))
  else
    // Adobe APP14 marker if these color spaces
    if AColors in [jcRGB, jcCMYK, jcYCCK] then
    begin
      M := TsdAdobeAPP14Marker.Create(FJpegInfo, mkAPP14);
      case AColors of
      jcRGB, jcCMYK: TsdAdobeAPP14Marker(M).Transform := 0;
      jcYCCK: TsdAdobeAPP14Marker(M).Transform := 2;
      end;
      Markers.Insert(1, M);
    end;
end;

procedure TsdJpegImage.AVI1MarkerCheck;
// check if this jpeg is part of ab AVI file
var
  i: integer;
  DHTExists: boolean;
  AVI1Exists: boolean;
  DHTMarker: TsdDHTMarker;
  MS:TERRAStream;
begin
  // Added by Dec start
  DHTExists := False;
  AVI1Exists := False;
  for i := 0 to FMarkers.Count - 1 do
  begin
    DHTExists := DHTExists or (FMarkers[i] is TsdDHTMarker);
    AVI1Exists := AVI1Exists or (FMarkers[i] is TsdAVI1Marker);
  end;
  if not DHTExists and AVI1Exists then
  begin
    DHTMarker := TsdDHTMarker.Create(FJpegInfo, mkDHT);
    FMarkers.Insert(FMarkers.Count - 1, DHTMarker);
    MS := MemoryStream.Create();
    try
      MS.Write(@cMjpgDHTSeg, SizeOf(cMjpgDHTSeg));
      MS.Position := 0;
      DHTMarker.LoadFromStream(MS, MS.Size);
    finally
      MS.Free;
    end;
  end;
  // Added by Dec end
end;

procedure TsdJpegImage.Clear;
begin
  // Clear any lists/objects we have
  FMarkers.Clear;
  FJpegInfo.Clear;
  FCoderStream.Seek(0);

  // Free any coder we used
  ReleaseObject(FCoder);

  // We free the color profile
  ReleaseObject(FICCProfile);
end;

constructor TsdJpegImage.Create();
begin
  // Owned objects
  FMarkers := TsdJpegMarkerList.Create(Self);
  FJpegInfo := TsdJpegInfo.Create;
  FCoderStream := MemoryStream.Create();
  FSaveOptions := TsdJpegSaveOptions.Create(Self);
  FMapIterator := TsdMapIterator.Create;

  // Defaults
  FStoredCS := jcAutoDetect;
  FBitmapCS := jcRGBA;
  FDctCodingMethod := dmAccurate;
end;

Procedure TsdJpegImage.Release;
begin
  ReleaseObject(FSaveOptions);
  ReleaseObject(FCoder);
  ReleaseObject(FMapIterator);
  ReleaseObject(FICCProfile);
  ReleaseObject(FJpegInfo);
  ReleaseObject(FCoderStream);
  ReleaseObject(FMarkers);
  inherited;
end;

function TsdJpegImage.DetectInternalColorSpace: TsdJpegColorSpace;
var
  JFIF: TsdJFIFMarker;
  Adobe: TsdAdobeApp14Marker;
  IDStr: AnsiString;
  // local
  function GetComponentIDString: AnsiString;
  var
    i: integer;
  begin
    SetLength(Result, FJpegInfo.FFrameCount);
    for i := 0 to FJpegInfo.FFrameCount - 1 do
      Result[i + 1] := AnsiChar(FJpegInfo.FFrames[i].FComponentID);
  end;
begin
  // Defaults: Based on component count
  Result := jcAutoDetect;
  case FJpegInfo.FFrameCount of
  1: Result := jcGray;
  2: Result := jcGrayA;
  3: Result := jcYCbCr;
  4: Result := jcYCCK;
  end;

  // Check JFIF marker
  JFIF := GetJFIFInfo;
  if assigned(JFIF) and JFIF.IsValid then
    // We have a JFIF marker: if component count is 1 or 3, above assumptions are correct
    if FJpegInfo.FFrameCount in [1, 3] then
      exit;

  // Check Adobe APP14 marker
  Adobe := GetAdobeAPP14Info;
  if assigned(Adobe) and Adobe.IsValid then
  begin
    // We have an Adobe APP14 marker
    case Adobe.Transform of
    0:
      begin
        case FJpegInfo.FFrameCount of
        3: Result := jcRGB;
        4: Result := jcCMYK;
        end;
      end;
    1: Result := jcYCbCr;
    2: Result := jcYCCK;
    end;
    exit;
  end;

  // Check for ITU G3FAX format
  if FMarkers.ByClass(TsdG3FAXMarker) <> nil then
  begin
    Result := jcITUCieLAB;
    exit;
  end;

  // No subsampling used?
  if (FJpegInfo.FHorzSamplingMax = 1) and (FJpegInfo.FVertSamplingMax = 1) then
  begin
    // No subsampling used -> Change YC method to RGB or CMYK
    case FJpegInfo.FFrameCount of
    3: Result := jcRGB;
    4: Result := jcCMYK;
    end;
  end;

  // Use component ID's
  IDStr := GetComponentIDString;
  case FJpegInfo.FFrameCount of
  3:
    begin
      // Possible ID strings
      if IDStr = #1#2#3 then
        Result := jcYCbCr;
      if IDStr = 'RGB' then
        Result := jcRGB;
      if IDStr = 'YCc' then
        Result := jcPhotoYCC;
    end;
  4:
    begin
      // Possible ID strings
      if IDStr = #1#2#3#4 then
      begin
        if HasICCProfile then
          // Note: in fact, in cases seen, this represents CMYK instead of RGBA,
          // so to decode: decode to RGBA as usual, then pretend these channels
          // are CMYK, and convert to final colour space.
          // Seen in: scanners (always with ICC profile present - which has CMYK profile)
          Result := jcYCbCrK
        else
          Result := jcYCbCrA;
      end;
      if IDStr = 'RGBA' then
        Result := jcRGBA;
      if IDStr = 'YCcA' then
        Result := jcPhotoYCCA;
    end;
  end;
end;

function sdBitCountToPixelFormat(const ABitCount: cardinal): TsdPixelFormat;
begin
  case ABitCount of
  1: Result := spf1bit;
  2: Result := spf2bit;
  4: Result := spf4bit;
  8: Result := spf8bit;
  10: Result := spf10bit;
  12: Result := spf12bit;
  15: Result := spf15bit;
  16: Result := spf16bit;
  24: Result := spf24bit;
  32: Result := spf32bit;
  36: Result := spf36bit;
  48: Result := spf48bit;
  64: Result := spf64bit;
  else
    Result := spf8bit;
  end;
end;

procedure TsdJpegImage.Compress(AIterator: TsdMapIterator);
// compress the Jpeg image, using the bitmap in AIterator
var
  TransformClass: TsdColorTransformClass;
  Transform: TsdColorTransform;
  StoredCS: TsdJpegColorSpace;
begin
  // check iterator
  if not assigned(AIterator) or (AIterator.Width * AIterator.Height = 0) then
  begin
    //DoDebugOut(Self, wsFail, sBitmapIsEmptyCannotSave);
    exit;
  end;

  {if not HasCoefficients then}
  begin

    // BitCount to sdpixelformat
    FPixelFormat := sdBitCountToPixelFormat(AIterator.BitCount);

    // coding info and important private data width/height
    FJpegInfo.FWidth := AIterator.Width;
    FJpegInfo.FHeight := AIterator.Height;

    FLoadScale := jsFull;
    FMapWidth := FJpegInfo.FWidth;
    FMapHeight := FJpegInfo.FHeight;

    // If no coder yet, we create the baseline coder
    if not assigned(FCoder) then
      FCoder := TsdJpegBaselineCoder.Create(Self, FJpegInfo);

    // compressing the bitmap can only be done in full scale (8x8)
    if FCoder.Scale <> jsFull then
    begin
      //DoDebugOut(Self, wsFail, sOperationOnlyFor8x8);
      exit;
    end;

    // Verify incoming bitmap format versus bitmap colorspace
    StoredCS := VerifyBitmapColorSpaceForSave;

    // We create minimal default markers to warrant color space detection
    // later on
    AddMinimalMarkersForColorSpaceDetection(StoredCS);

    // Ask save options to add DQT, SOFn and SOS marker
    FSaveOptions.AddMarkers(StoredCS, FMapWidth, FMapHeight);

    // Color transform
    GetColorTransformFromBitmap(TransformClass);
    if not assigned(TransformClass) then
    begin
      //DoDebugOut(Self, wsWarn, sInvalidFormatForSelectedCS);
      exit;
    end;

    Transform := TransformClass.Create;
    if assigned(AIterator) then
    begin
      try
        // Initialize coder (this sets map sizes etc)
        FCoder.Initialize(jsFull);
        // Get samples from bitmap data
        // AV here in laz
        FCoder.SamplesFromImage(AIterator, Transform);
        // Now convert samples to coefficients. This also does the quantization
        FCoder.ForwardDCT;
      finally
        Transform.Free;
      end;
    end;

    // We also must add an EOI marker
    FMarkers.Add(TsdEOIMarker.Create(FJpegInfo, mkEOI));

    // coder now has coefficients
    FCoder.HasCoefficients := True;
  end;
end;

procedure TsdJpegImage.EntropyDecodeSkip(S: TERRAStream);
// In case we want to skip the entropy-encoded stream, but inspect markers
var
  B, ReadBytes, Tag: byte;
  First, Last, P: PByte;
begin
  if (S is MemoryStream) then
  begin
    // Fast skip based on memorystream
    First := MemoryStream(S).Buffer;
    Last := First;
    inc(Last, S.Size);
    P := First;
    inc(P, S.Position);
    while cardinal(P) < cardinal(Last) do
    begin
      // Scan stream for $FF + <marker>
      if P^ = $FF then
      begin
        inc(P);
        if P^ <> 0 then
        begin
          dec(P, 1);
          S.Position := cardinal(P) - cardinal(First);
          exit;
        end;
      end;
      inc(P);
    end;
  end else
  begin
    // Slow skip for general streams
    repeat
      ReadBytes := S.Read(@B, 1);
      if B = $FF then
      begin
        S.ReadByte(Tag);
        if Tag <> 0 then
        begin
          S.Seek(S.Position-2);
          exit;
        end;
      end;
    until ReadBytes = 0;
  end;
end;

procedure TsdJpegImage.ExtractMetadata(AList: TsdJpegMarkerList);
var
  Idx: integer;
begin
  Idx := 0;
  while Idx < FMarkers.Count do
  begin
    if FMarkers[Idx].MarkerTag in [mkAPP0..mkAPP15, mkCOM] then
      AList.Add(FMarkers.Extract(FMarkers[Idx]))
    else
      inc(Idx);
  end;
end;

function TsdJpegImage.GetAdobeAPP14Info: TsdAdobeApp14Marker;
begin
  Result := TsdAdobeAPP14Marker(FMarkers.ByTag(mkApp14));
end;

procedure TsdJpegImage.GetBitmapSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);
var
  W, H, Divisor: integer;
begin
  W := FJpegInfo.FWidth;
  H := FJpegInfo.FHeight;
  Divisor := sdGetDivisor(AScale);
  AWidth  := (W + Divisor - 1) div Divisor;
  AHeight := (H + Divisor - 1) div Divisor;
end;

procedure TsdJpegImage.GetBitmapTileSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);
var
  W, H, Divisor: integer;
begin
  W := FJpegInfo.FTileWidth;
  H := FJpegInfo.FTileHeight;
  Divisor := sdGetDivisor(AScale);
  AWidth  := (W + Divisor - 1) div Divisor;
  AHeight := (H + Divisor - 1) div Divisor;
end;

procedure TsdJpegImage.GetColorTransformFromBitmap(var AClass: TsdColorTransformClass);
var
  InternalCS, Input: TsdJpegColorSpace;
begin
  // At this point we can use DetectInternalColorSpace to find the color space of
  // the file, since all parameters and markes have been set. We can also trust
  // the FBitmapColorspace to match with the bitmap.

//  DoDebugOut(Self, wsInfo, 'Color conversion bitmap->samples:');
  if FStoredCS = jcAutoDetect then
  begin
    InternalCS := DetectInternalColorSpace;
//    DoDebugOut(Self, wsInfo, Format(' Internal colorsp: %s (detected)', [cColorSpaceNames[InternalCS]]));
  end else
  begin
    InternalCS := FStoredCS;
  //  DoDebugOut(Self, wsInfo, Format(' Internal colorsp: %s (selected)', [cColorSpaceNames[InternalCS]]));
  end;

  Input := FBitmapCS;
  if Input = jcAutoDetect then
  begin
    case FPixelFormat of
    spf8bit:  Input := jcGray;
    spf24bit: Input := jcRGB;
    spf32bit: Input := jcRGBA;
    end;
  end;

  // Defaults
  AClass := nil;
  case FJpegInfo.FFrameCount of
  1: if FPixelFormat = spf8bit  then AClass := TsdNullTransform8bit;
  2: if FPixelFormat = spf16bit then AClass := TsdNullTransform16bit;
  3: if FPixelFormat = spf24bit then AClass := TsdNullTransform24bit;
  4: if FPixelFormat = spf32bit then AClass := TsdNullTransform32bit;
(*  else
    DoDebugOut(Self, wsWarn, 'FCodingInfo.FrameCount = 0');*)
  end;

  // Specific transforms
  case InternalCS of
  jcGray:
    case Input of
    jcRGB: AClass := TsdTransformBGRToGray;
    jcRGBA: AClass := TsdTransformRGBAToGray;
    end;
  jcRGB:
    case Input of
    jcRGB: AClass := TsdTransformInvertTriplet24bit;
    end;
  jcYCbCr, jcPhotoYCC:
    case Input of
    jcRGB: AClass := TsdTransformBGRToYCbCr;
    jcRGBA: AClass := TsdTransformBGRAToYCbCr;
    end;
  jcYCbCrA:
    case Input of
    jcRGBA: AClass := TsdTransformBGRAToYCbCrA;
    end;
  jcCMYK:
    case Input of
    jcRGB: AClass := TsdTransformRGBToCMYK;
    end;
  jcYCCK, jcPhotoYCCA:
    case Input of
    jcRGB: AClass := TsdTransformRGBToYCCK;
    jcCMYK: AClass := TsdTransformCMYKToYCCK;
    end;
  end;
end;

procedure TsdJpegImage.GetColorTransformToBitmap(
  var AClass: TsdColorTransformClass; var AFormat: TsdPixelFormat);
var
  Warning: boolean;
  InternalCS, OutputCS: TsdJpegColorSpace;
  // helper
  procedure SetClassAndFormat(C: TsdColorTransformClass; F: TsdPixelFormat);
  begin
    AClass := C;
    AFormat := F;
  end;
begin
  // default class and pixelformat
  case FJpegInfo.FFrameCount of
  1: SetClassAndFormat(TsdNullTransform8bit, spf8bit);
  2: SetClassAndFormat(TsdNullTransform16bit, spf16bit);
  3: SetClassAndFormat(TsdNullTransform24bit, spf24bit);
  4: SetClassAndFormat(TsdNullTransform32bit, spf32bit);
  else
    //DoDebugOut(Self, wsWarn, 'FCodingInfo.FrameCount = 0');
    SetClassAndFormat(nil, spf24bit);
  end;

  // Determine stored colorspace
  //DoDebugOut(Self, wsInfo, 'Color conversion samples->bitmap:');
  if FStoredCS = jcAutoDetect then
  begin
    InternalCS := DetectInternalColorSpace;
    //DoDebugOut(Self, wsInfo, Format(' Internal colorsp: %s (detected)', [cColorSpaceNames[InternalCS]]));
  end else
  begin
    InternalCS := FStoredCS;
    //DoDebugOut(Self, wsInfo, Format(' Internal colorsp: %s (selected)', [cColorSpaceNames[InternalCS]]));
  end;

  // Determine bitmap colorspace
  if FBitmapCS = jcAutoDetect then
  begin
    OutputCS := InternalCS;
    //DoDebugOut(Self, wsInfo, Format(' Bitmap colorsp: %s (no change)', [cColorSpaceNames[OutputCS]]));
  end else
  begin
    OutputCS := FBitmapCS;
    //DoDebugOut(Self, wsInfo, Format(' Bitmap colorsp: %s (selected)', [cColorSpaceNames[OutputCS]]));
  end;

  // Determine what conversion and pixelformat to use
  Warning := False;
  case InternalCS of
  jcGray:
    case OutputCS of
    jcRGB: SetClassAndFormat(TsdTransformGrayToBGR, spf24bit);
    jcGray:;
    else
      Warning := True;
    end;
  jcGrayA:
    case OutputCS of
    jcRGB:  SetClassAndFormat(TsdTransformGrayAToBGR, spf24bit);
    jcRGBA: SetClassAndFormat(TsdTransformGrayAToBGRA, spf32bit);
    jcGrayA:;
    else
      Warning := True;
    end;
  jcRGB:
    case OutputCS of
    jcRGBA: SetClassAndFormat(TsdTransformRGBToBGRA, spf32bit);
    jcRGB: SetClassAndFormat(TsdTransformInvertTriplet24bit, spf24bit);
    else
      Warning := True;
    end;
  jcRGBA:
    case OutputCS of
    jcRGb: SetClassAndFormat(TsdTransformRGBAToBGR, spf24bit);
    jcRGBA:;
    else
      Warning := True;
    end;
  jcYCbCr, jcPhotoYCc:
    case OutputCS of
    jcGray: SetClassAndFormat(TsdTransformYCbCrToGray, spf8bit);
    jcRGB:  SetClassAndFormat(TsdTransformYCbCrToBGR, spf24bit);
    jcRGBA: SetClassAndFormat(TsdTransformYCbCrToBGRA, spf32bit);
    else
      Warning := True;
    end;
  jcYCbCrA, jcPhotoYCcA:
    case OutputCS of
    jcRGB:  SetClassAndFormat(TsdTransformYCbCrAToBGR, spf24bit);
    jcRGBA: SetClassAndFormat(TsdTransformYCbCrAToBGRA, spf32bit);
    else
      Warning := True;
    end;
  jcYCbCrK:
    case OutputCS of
    jcRGB: SetClassAndFormat(TsdTransformYCbCrKToBGR, spf24bit);
    else
      Warning := True;
    end;
  jcCMYK:
    case OutputCS of
    jcRGB: SetClassAndFormat(TsdTransformCMYKToBGR_Adobe, spf24bit);
    else
      Warning := True;
    end;
  jcYCCK:
    case OutputCS of
    jcRGB: SetClassAndFormat(TsdTransformYCCKToBGR, spf24bit);
    else
      Warning := True;
    end;
  jcITUCieLAB:
    case OutputCS of
    jcRGB: SetClassAndFormat(TsdTransformITUCIELabToBGR, spf24bit);
    else
      Warning := True;
    end;
  else
    Warning := True;
  end;

  (*if (OutputCS <> InternalCS) and Warning then
    DoDebugOut(Self, wsWarn, ' Warning: no color transform could be found (stored colors are output)');*)
end;

function TsdJpegImage.GetComment: AnsiString;
var
  M: TsdCOMMarker;
begin
  M := TsdCOMMarker(FMarkers.ByTag(mkCOM));
  if not assigned(M) then
    Result := ''
  else
    Result := M.Comment;
end;

function TsdJpegImage.GetExifInfo: TsdEXIFMarker;
begin
  Result := TsdEXIFMarker(FMarkers.ByTag(mkApp1));
end;

function TsdJpegImage.GetHeight: integer;
var
  D: integer;
begin
  D := sdGetDivisor(FLoadScale);
  Result := (GetImageHeight + D - 1) div D;
end;

function TsdJpegImage.GetICCProfile: TsdJpegICCProfile;
// return the ICC profile from the ICCProfile markers
var
  M: TsdICCProfileMarker;
begin
  if assigned(FICCProfile) then
  begin
    Result := FICCProfile;
    exit;
  end;

  // Do we have an ICC profile?
  Result := nil;
  M := TsdICCProfileMarker(FMarkers.ByClass(TsdICCProfileMarker));
  if not assigned(M) or not M.IsValid then
    exit;

  FICCProfile := TsdJpegICCProfile.Create;
  FICCProfile.ReadFromMarkerList(FMarkers);
  Result := FICCProfile;
end;

function TsdJpegImage.GetImageHeight: integer;
begin
  if assigned(FJpegInfo) then
    Result := FJpegInfo.FHeight
  else
    Result := 0;
end;

function TsdJpegImage.GetImageWidth: integer;
begin
  if assigned(FJpegInfo) then
    Result := FJpegInfo.FWidth
  else
    Result := 0;
end;

function TsdJpegImage.GetIptcInfo: TsdIPTCMarker;
begin
  Result := TsdIPTCMarker(FMarkers.ByTag(mkApp13));
end;

function TsdJpegImage.GetJfifInfo: TsdJFIFMarker;
begin
  Result := TsdJFIFMarker(FMarkers.ByTag(mkApp0));
end;

function TsdJpegImage.GetWidth: integer;
var
  D: integer;
begin
  D := sdGetDivisor(FLoadScale);
  Result := (GetImageWidth + D - 1) div D;
end;

function TsdJpegImage.HasBitmap: boolean;
begin
  Result := assigned(FMapIterator.Map);
end;

function TsdJpegImage.HasCoefficients: boolean;
begin
  Result := False;
  if assigned(FCoder) then
    Result := FCoder.HasCoefficients;
end;

function TsdJpegImage.HasICCProfile: boolean;
// Determine if we have a valid ICC profile
var
  M: TsdICCProfileMarker;
begin
  // ICC profile already read?
  if assigned(FICCProfile) then
  begin
    Result := True;
    exit;
  end;
  // Do we have an ICC profile?
  M := TsdICCProfileMarker(FMarkers.ByClass(TsdICCProfileMarker));
  Result := assigned(M) and M.IsValid;
end;

function TsdJpegImage.HasSamples: boolean;
begin
  Result := False;
  if assigned(FCoder) then
    Result := FCoder.HasSamples;
end;

procedure TsdJpegImage.InitializeDecode;
begin
  // Create correct codec
  case FJpegInfo.FEncodingMethod of
  emBaselineDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegBaselineCoder) then
      begin
        ReleaseObject(FCoder);
        FCoder := TsdJpegBaselineCoder.Create(Self, FJpegInfo);
      end;
    end;
  emExtendedDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegExtendedCoder) then
      begin
        ReleaseObject(FCoder);
        FCoder := TsdJpegExtendedCoder.Create(Self, FJpegInfo);
      end;
    end;
  emProgressiveDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegProgressiveCoder) then
      begin
        ReleaseObject(FCoder);
        FCoder := TsdJpegProgressiveCoder.Create(Self, FJpegInfo);
      end;
    end;
  else
    ReleaseObject(FCoder);
    exit;
  end;
  FCoder.Clear;
  FCoder.Method := FDCTCodingMethod;
  FCoder.TileMode := loTileMode in FLoadOptions;
  FCoder.Initialize(FLoadScale);
end;

procedure TsdJpegImage.InjectMetadata(AList: TsdJpegMarkerList);
begin
  FMarkers.RemoveMarkers([mkAPP0..mkAPP15, mkCOM]);
  while AList.Count > 0 do
    FMarkers.Insert(1, AList.Extract(AList[AList.Count - 1]));
end;

procedure TsdJpegImage.LoadJpeg(AScale: TsdJpegScale; DoCreateBitmap: boolean);
var
  i: integer;
  Iteration: cardinal;
  MarkerTag, ExtraTag: byte;
begin
  FLoadScale := AScale;

  //DoDebugOut(Self, wsInfo, Format('LoadJpeg with LoadScale=%d', [integer(FLoadScale)]));

  // iterate thru the markers to initialize, decode and finalize the coder
  for i := 0 to FMarkers.Count - 1 do
  begin
    MarkerTag := FMarkers[i].MarkerTag;
    case MarkerTag of
    mkSOF0..mkSOF2:
      begin
        // Method is defined.. we can initialise the coder
        InitializeDecode;
      end;
    mkSOS:
      if assigned(FCoder) then
      begin
        //DoDebugOut(Self, wsinfo, Format('decode with FCoderStream size=%d',        [FCoderStream.Size]));

        Iteration := 0;
        FCoder.Decode(FCoderStream, Iteration);

        // in progressive jpegs there can be additional SOS markers
        while FCoderStream.Position < FCoderStream.Size do
        begin
          // optional additional SOS tag?
          ExtraTag := LoadMarker(FCoderStream);
          case ExtraTag of
          mkSOS:
            begin
              inc(Iteration);
              FCoder.Decode(FCoderStream, Iteration);
            end;
          mkDHT:
            begin
              // not the right place but we will be lenient
              //DoDebugOut(Self, wsWarn, 'incorrect place for DHT marker (must be *before* first SOS)');
            end;
          else
            //DoDebugOut(Self, wsinfo, Format('FCoderStream pos=%d size=%d', FCoderStream.Position, FCoderStream.Size]));
            // signal that we are done
            FCoderStream.Position := FCoderStream.Size;
          end;
        end;

      end;
    mkEOI:
      begin
        FCoder.Finalize;
      end;
    end;
  end;

  // if DoCreateBitmap, we update the bitmap
  if DoCreateBitmap then
    {Res := }UpdateBitmap;
end;

Procedure TsdJpegImage.FillIterator();
Begin
  _Target.New(FMapIterator.Width, FMapIterator.Height);

  FMapIterator.Map := PByte(_Target.RawPixels);
  FMapIterator.ScanStride := _Target.Width * 4;
  FMapIterator.CellStride := 4;
  FMapIterator.BitCount := 32;
End;

Procedure TsdJpegImage.UpdateBitmap;
var
  Transform: TsdColorTransform;
  TransformClass: TsdColorTransformClass;
begin
  // If we do not have coefficients we have not loaded anything yet, so exit
  if not HasCoefficients then
    exit;

  // Do we need to update the bitmap?
  if not HasSamples then
  begin
    GetColorTransformToBitmap(TransformClass, FPixelFormat);
    if TransformClass = nil then
    begin
      //DoDebugOut(Self, wsWarn, sNoColorTransformation);
      exit;
    end;
    Transform := TransformClass.Create;
    try
      // Inverse-DCT the coefficients, this also does the unquantization
      FCoder.InverseDCT;

      // Find the bitmap size based on decoded info and required scale
      GetBitmapSize(FLoadScale, FMapWidth, FMapHeight);

      // Set bitmap size and pixelformat, and get the iterator to pass as argument
      FMapIterator.Width := FMapWidth;
      FMapIterator.Height := FMapHeight;
      FMapIterator.CellStride := sdPixelFormatToByteCount(FPixelFormat);

      // this should also update the FMapIterator from the application
      FillIterator();

      // Ask the coder to put the samples in the bitmap
      FCoder.SamplesToImage(FMapIterator, Transform);

    finally
      Transform.Free;
    end;
    FCoder.HasSamples := True;
  end;
end;

procedure TsdJpegImage.LoadFromStream(S: TERRAStream; Target:TERRAImage);
// LoadFromStream does not yet create or load a bitmap, it only
// loads all the properties of the markers and coder so
// that LoadJpeg() can load the bitmap
var
  MarkerTag: byte;
  EOI1, EOI2: pbyte;
  EOIMarker: TsdEOIMarker;
begin
  _Target := Target;

  // first clear our data
  Clear;

  // size in bytes of the data stream S
  FDataSize := S.Size;

    // load another marker of the markers in the jpeg
    repeat

      MarkerTag := LoadMarker(S);

    until (MarkerTag = mkSOS) or (MarkerTag = mkNone);

    if MarkerTag = mkSOS then
    begin
      // before we start with the scan (SOS), we must check if the
      // AVI1 tag exists, while there is no DHT. If no DHT,
      // it will be created and added just before the SOS.
      AVI1MarkerCheck;

      // the coder stream starts right after the mkSOS marker data
      // and ends till the end of the file
      FCoderStream.Seek(0);
      S.Copy(FCoderStream, S.Position, S.Size - S.Position);
      FCoderStream.Position := 0;

      if FCoderStream.Size >= 2 then
      begin
        // detect EOI
        EOI1 := FCoderStream.Buffer;
        inc(EOI1, FCoderStream.Size - 2);
        EOI2 := EOI1;
        inc(EOI2);

        if (EOI1^ = $FF) and (EOI2^ = mkEOI) then
        begin
          // the EOI marker is found, we add it to the marker list so
          // we also write the EOI when saving
          // we must remove the two bytes of the FCoderStream, since the
          // EOI is not part of the coder stream
          FCoderStream.Resize(FCoderStream.Size - 2);
          //DoDebugOut(Self, wsInfo, '<EOI marker>');
          EOIMarker := TsdEOIMarker.Create(FJpegInfo, mkEOI);
          FMarkers.Add(EOIMarker);
        end;
      end;
    end;
end;

procedure TsdJpegImage.LoadTileBlock(var ALeft, ATop, ARight, ABottom: integer);
// LoadTileBlock certainly may have bugs! But it seems that tiled loading works
// for the baseline coder.
var
  Divisor, McuW, McuH: integer;
  XStart, YStart, XCount, YCount: integer;
  McuLeft, McuTop, McuRight, McuBottom: integer;
  i: integer;
  MarkerTag: byte;
  Transform: TsdColorTransform;
  TransformClass: TsdColorTransformClass;
begin
  //DoDebugOut(Self, wsInfo, Format('Load tile block [%d %d %d %d]',    [ALeft, ATop, ARight, ABottom]));

  if not assigned(FCoder) then
  begin
    // iterate thru the markers to initialize, decode and finalize the coder
    for i := 0 to FMarkers.Count - 1 do
    begin
      MarkerTag := FMarkers[i].MarkerTag;
      case MarkerTag of
      mkSOF0..mkSOF2:
        begin
          // Method is defined.. we can initialise the coder
          InitializeDecode;
        end;
      mkSOS:
        if FCoder is TsdJpegBaselineCoder then
          TsdJpegBaselineCoder(FCoder).Decode(FCoderStream, 0);
      end;
    end;
  end;

  // baseline check
  if FCoder.ClassType <> TsdJpegBaselineCoder then
  begin
    //DoDebugOut(Self, wsWarn, 'tiled loading only possible with baseline jpeg');
    exit;
  end;

  // Determine MCU block area
  Divisor := sdGetDivisor(FLoadScale);
  McuW := FJpegInfo.FMCUWidth div Divisor;
  McuH := FJpegInfo.FMCUHeight div Divisor;

  McuLeft := IntMax(0, ALeft div McuW);
  McuRight := IntMin(FJpegInfo.FHorzMCUCount, (ARight + McuW - 1) div McuW);
  McuTop := IntMax(0, ATop div McuH);
  McuBottom := IntMin(FJpegInfo.FVertMcuCount, (ABottom + McuH - 1) div McuH);

  XCount := McuRight - McuLeft;
  YCount := McuBottom - McuTop;
  XStart := McuLeft;
  YStart := McuTop;
  ALeft := McuLeft * McuW;
  ATop := McuTop * McuH;
  ARight := McuRight * McuW;
  ABottom := McuBottom * McuH;

  // Anything to load?
  if (XCount <= 0) or (YCount <= 0) then
    exit;

  FJpegInfo.FTileWidth := McuW * XCount * Divisor;
  FJpegInfo.FTileHeight := McuH * YCount * Divisor;

  // decode the block
  FCoder.DecodeBlock(FCoderStream, XStart, YStart, XCount, YCount);

  // put it in the map
  GetColorTransformToBitmap(TransformClass, FPixelFormat);
  if TransformClass = nil then
  begin
    //DoDebugOut(Self, wsWarn, sNoColorTransformation);
    exit;
  end;

  Transform := TransformClass.Create;
  try
    // Inverse-DCT the coefficients, this also does the unquantization
    FCoder.InverseDCT;

    // Find the bitmap tile size based on decoded info and required scale
    GetBitmapTileSize(FLoadScale, FJpegInfo.FTileWidth, FJpegInfo.FTileHeight);

    // Set tile bitmap size and pixelformat, and get the iterator to pass as argument
    FMapIterator.Width := FJpegInfo.FTileWidth;
    FMapIterator.Height := FJpegInfo.FTileHeight;
    FMapIterator.CellStride := sdPixelFormatToByteCount(FPixelFormat);

    // this should update the FMapIterator from the application
    FillIterator();

    // Ask the coder to put the samples in the bitmap
    FCoder.SamplesToImage(FMapIterator, Transform);

  finally
    Transform.Free;
  end;
  FCoder.HasSamples := True;

end;

function TsdJpegImage.LoadMarker(S: TERRAStream): byte;
var
  B, MarkerTag, BytesRead: byte;
  Marker: TsdJpegMarker;
  JpegMarkerClass: TsdJpegMarkerClass;
  Size: word;
  StreamPos: integer;
begin
  // default is no marker
  Result := mkNone;

  // Read markers from the stream, until a non $FF is encountered
  //DoDebugOut(Self, wsInfo, Format('Current Pos: %.6d', [S.Position]));
  BytesRead := S.Read(@B, 1);
  if BytesRead = 0 then
  begin
    //DoDebugOut(Self, wsWarn, sMarkerExpected);
    exit;
  end;

  // Do we have a marker?
  if B = $FF then
  begin
    // Which marker?
    S.ReadByte(MarkerTag);
    while MarkerTag = $FF do
    begin
      MarkerTag := mkNone;
      //DoDebugOut(Self, wsWarn, Format('Error: duplicate $FF encountered at %.6d', [S.Position - 1]));
      S.ReadByte(MarkerTag);
    end;

    case MarkerTag of
    mkAPP0..mkAPP15:
      begin
        JpegMarkerClass := FindJpegMarkerClassList(MarkerTag, S);
        if JpegMarkerClass = nil then
          JpegMarkerClass := TsdAPPnMarker;
        Marker := JpegMarkerClass.Create(FJpegInfo, MarkerTag);
      end;
    mkDHT:
      Marker := TsdDHTMarker.Create(FJpegInfo, MarkerTag);
    mkDQT:
      Marker := TsdDQTMarker.Create(FJpegInfo, MarkerTag);
    mkDRI:
      Marker := TsdDRIMarker.Create(FJpegInfo, MarkerTag);
    mkSOF0..mkSOF3, mkSOF5..mkSOF7, mkSOF9..mkSOF11, mkSOF13..mkSOF15:
      Marker := TsdSOFnMarker.Create(FJpegInfo, MarkerTag);
    mkSOS:
      Marker := TsdSOSMarker.Create(FJpegInfo, MarkerTag);
    mkSOI:
      Marker := TsdSOIMarker.Create(FJpegInfo, MarkerTag);
    mkEOI:
      Marker := TsdEOIMarker.Create(FJpegInfo, MarkerTag);
    mkRST0..mkRST7:
      Marker := TsdRSTMarker.Create(FJpegInfo, MarkerTag);
    mkCOM:
      Marker := TsdCOMMarker.Create(FJpegInfo, MarkerTag);
    mkDNL:
      Marker := TsdDNLMarker.Create(FJpegInfo, MarkerTag);
    else
      // General marker
      Marker := TsdJpegMarker.Create(FJpegInfo, MarkerTag);
    end;

    // Add marker to our list
    FMarkers.Add(Marker);
    Marker.Owner := Self;

    if MarkerTag in [mkAPP0..mkAPP15, mkDHT, mkDQT, mkDRI,
      mkSOF0, mkSOF1, mkSOF2, mkSOF3, mkSOF5, mkSOF6, mkSOF7, mkSOF9, mkSOF10, mkSOF11, mkSOF13, mkSOF14, mkSOF15,
      mkSOS, mkCOM, mkDNL] then
    begin
      // Read length of marker
      Size := TsdJpegMarker.GetWord(S) - 2;
    end else
      Size := 0;

    StreamPos := S.Position;

    // Load the marker payload
    Marker.LoadFromStream(S, Size);

    // The SOS marker indicates start of entropy coding (start of scan),
    // EOI indicates end of image. SOF0 and SOF2 indicate
    // baseline and progressive starts, we do not use Marker.LoadFromStream for these.
    if not (MarkerTag in [mkSOF0..mkSOF2, mkSOS, mkRST0..mkRST7, mkEOI]) then
    begin

      // Find correct stream position
      S.Position := StreamPos + Size;
    end;
    
  end else
  begin

    // B <> $FF is an error, we try to be flexible
    //DoDebugOut(Self, wsWarn, Format('Error: marker expected at %.6d', [S.Position - 1]));
    repeat
      BytesRead := S.Read(@B, 1);
    until (BytesRead = 0) or (B = $FF);

    if BytesRead = 0 then
      Engine.RaiseError('JPEG: '+sMarkerExpected);
    S.Seek(S.Position -1);
    //DoDebugOut(Self, wsHint, Format('Resuming at %.6d', [S.Position]));
  end;
  Result := MarkerTag;
end;

procedure TsdJpegImage.SaveJpeg();
// write the markers and save the coder stream
const
  cFF: byte = $FF;
var
  i: integer;
  M: TsdJpegMarker;
  SeenDHT: boolean;
  DHT: TsdDHTMarker;
begin
  // no coefficients? then CompressJpeg should have been called
  if not HasCoefficients then
  begin
    //DoDebugOut(Self, wsFail, sNoDCTCoefficentsAvailable);
    exit;
  end;

  // We can now repeatedly save up to the last SOS, then ask the codec to encode
  // the scan
  SeenDHT := False;
  i := 0;
  
  while i < FMarkers.Count do
  begin
    M := Markers[i];

    if M is TsdDHTMarker then
      SeenDHT := True;

    if (M is TsdSOSMarker) and not SeenDHT then
    begin
      // We are at Start of Scan but have not saved a Huffman table, so we must
      // create one and do that now. First we apply scan data by writing the SOS marker.
      M.WriteMarker;

      // Now create the optimized huffman tables for this scan, by doing a dry
      // run, indicated by nil
      //DoDebugOut(Self, wsInfo, 'doing dry-run encoding for Huffman table');
      FCoder.Encode(nil, 0);

      // Ask the coder to create the DHT marker for us, as a
      // result of the dry-run information
      DHT := FCoder.CreateDHTMarker;
      //DoDebugOut(Self, wsInfo, 'writing DHT marker');
      if assigned(DHT) then
      begin
        DHT.WriteMarker;
        // If a marker was created, then insert it and continue, so it will be saved
        //DoDebugOut(Self, wsInfo, 'inserting new DHT marker');
        FMarkers.Insert(i - 1, DHT);
        SeenDHT := True;
        Continue;
      end;
    end;

    if not (M.MarkerTag in [mkSOI, mkEOI, mkRST0..mkRST7]) then
    begin
      //DoDebugOut(Self, wsInfo, Format('Writing marker %s', [M.MarkerName]));
      // Writing a marker will also make the marker update itself in the CodingInfo
      // object, so when calling FCoder.Encode later, it will have the current data
      M.WriteMarker;
    end;

    // Encode and save data
    if M is TsdSOSMarker then
    begin
      FCoderStream.Resize(0);
      FCoder.Encode(FCoderStream, 0);
      // bring position back to 0 for future load/saves
      FCoderStream.Position := 0;
    end;

    // Next marker
    inc(i);
  end;

  // the client can now use SaveToStream to save this new jpeg to a stream
end;

procedure TsdJpegImage.SaveBitmapStripByStrip(Target:TERRAImage);
const
  cFF: byte = $FF;
var
  BitmapIter: TsdMapIterator;
  TransformClass: TsdColorTransformClass;
  Transform: TsdColorTransform;
  Stored: TsdJpegColorSpace;
  i, y: integer;
  M: TsdJpegMarker;
  BaselineCoder: TsdJpegBaselineCoder;
begin
  Transform := nil;
  BitmapIter := TsdMapIterator.Create();
  try
    // Check if bitmap is created with correct pixelformat
    case FBitmapCS of
    jcGray:
      BitmapIter.BitCount := 8;
    jcGrayA:
      BitmapIter.BitCount := 16;
    jcRGB, jcYCbCr, jcPhotoYCC:
      BitmapIter.BitCount := 24;
    jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
      BitmapIter.BitCount := 32;
    else
      BitmapIter.BitCount := 24;
    end;
    FPixelFormat := sdBitCountToPixelFormat(BitmapIter.BitCount);
    BitmapIter.CellStride := BitmapIter.BitCount div 8;

    // We create the baseline coder
    ReleaseObject(FCoder);
    FCoder := TsdJpegBaselineCoder.Create(Self, FJpegInfo);
    BaselineCoder := TsdJpegBaselineCoder(FCoder);

    // Verify incoming bitmap format versus bitmap colorspace
    Stored := VerifyBitmapColorSpaceForSave;

    // We create minimal default markers to warrant color space detection
    // later on
    AddMinimalMarkersForColorSpaceDetection(Stored);

    // Ask save options to add DQT, SOFn and SOS marker. We use pre-defined
    // Huffman tables because we only do one pass over the image
    FSaveOptions.OptimizeHuffmanTables := False;
    FSaveOptions.AddMarkers(Stored, Target.Width, Target.Height);

    // We also must add an EOI marker
    FMarkers.Add(TsdEOIMarker.Create(FJpegInfo, mkEOI));

    // Color transform
    GetColorTransformFromBitmap(TransformClass);
    if not assigned(TransformClass) then
    begin
      //DoDebugOut(Self, wsFail, sInvalidFormatForSelectedCS);
      exit;
    end;
    Transform := TransformClass.Create;

    // Initialize coder (this sets map sizes etc)
    FCoder.TileMode := True; // avoid allocating full buffer
    FCoder.Initialize(jsFull); // will calculate MCU height

    // bitmap strip size
    BitmapIter.Width := Target.Width;
    BitmapIter.Height := FJpegInfo.FMcuHeight;

    // Result is usually a TBitmap (Win32/Linux, etc), but it is up to the application
    //FillIterator();

    // Get iterator
    //GetBitmapIterator(FBitmap, BmpIter);

    // Now we can save the image, and interactively ask application for strips
    // We can now repeatedly save up to the last SOS, then ask the codec to encode
    // the scan
    i := 0;
    while i < FMarkers.Count do
    begin

      M := Markers[i];
      //DoDebugOut(Self, wsInfo, Format('Writing marker %s', [IntToHex(M.MarkerTag, 2)]));
      if not (M.MarkerTag in [mkSOI, mkEOI, mkRST0..mkRST7]) then
      begin
        // Writing a marker will also make the marker update itself in the Info
        // object, so when calling FCoder.Encode later, it will have the current data
        M.WriteMarker;
      end;

      if M is TsdSOSMarker then
      begin

        // Start encoder in strip-by-strip mode. This will calculate MCU height
        BaselineCoder.EncodeStripStart(FCoderStream);

        // Encode strips one by one
        for y := 0 to FJpegInfo.FVertMcuCount - 1 do
        begin
          // Call the OnProvideStrip event
          // first, reset the bitnapiter method (since it gets changed by FCoder.SamplesFromImage later)
          BitmapIter.Method := imReaderX;
          ProvideStrip(0, y * FJpegInfo.FMcuHeight, BitmapIter, Target);

          // Get samples from bitmap data
          FCoder.SamplesFromImage(BitmapIter, Transform);
          // Now convert samples to coefficients. This also does the quantization
          FCoder.ForwardDCT;
          // And encode them to the stream
          BaselineCoder.EncodeStrip(FCoderStream);
        end;

        // finalise encoder
        BaselineCoder.EncodeStripClose;

      end;

      // Next marker
      inc(i);
    end;

  finally
    BitmapIter.Free;
    Transform.Free;
  end;
end;

procedure TsdJpegImage.SaveToStream(S: TERRAStream);
const
  cFF: byte = $FF;
var
  i: integer;
  Marker: TsdJpegMarker;
  MS: MemoryStream;
  MarkerSize, SwappedSize: word;
begin
  // loop thru the markers
  for i := 0 to FMarkers.Count - 1 do
  begin
    Marker := Markers[i];

    // write the marker tag
    S.WriteByte(cFF);
    S.WriteByte(Marker.MarkerTag);
    //DoDebugOut(Self, wsInfo, Format('Saving marker %s', [Marker.MarkerName]));

    if not (Marker.MarkerTag in [mkSOI, mkEOI, mkRST0..mkRST7]) then
    begin
      MS := MemoryStream.Create;
      try
        // save the marker to a memory stream
        Marker.SaveToStream(MS);
        MarkerSize := MS.Size + 2;
        SwappedSize := Swap(MarkerSize);
        MS.Position := 0;
        // write the marker size
        S.WriteWord(SwappedSize);
        // write the marker
        MS.Copy(S);
      finally
        MS.Free;
      end;
    end;

    // after the SOS save coding stream
    if Marker is TsdSOSMarker then
    begin
      //DoDebugOut(Self, wsInfo, Format('Saving coder stream (%d bytes)', [FCoderStream.Size]));
      FCoderStream.Position := 0;
      FCoderStream.Copy(S);
      // bring position back to 0 for future load/saves
      FCoderStream.Position := 0;
    end;

  end;
end;

procedure TsdJpegImage.SetComment(const Value: AnsiString);
var
  M: TsdCOMMarker;
begin
  M := TsdCOMMarker(FMarkers.ByTag(mkCOM));
  if not assigned(M) then
  begin
    // We do not yet have a marker
    if not FMarkers.HasMarker([mkSOI]) then
      Engine.RaiseError('JPG' +sCommentCannotBeSet);
    // Create the marker and insert after SOI or JFIF marker (whichever comes last)
    M := TsdCOMMarker.Create(FJpegInfo, mkCOM);
    FMarkers.InsertAfter([mkSOI, mkAPP0], M);
  end;
  M.Comment := Value;
end;

procedure TsdJpegImage.SetICCProfile(const Value: TsdJpegICCProfile);
begin
  ReleaseObject(FICCProfile);
  FMarkers.RemoveMarkers([mkApp2]);
  if assigned(Value) then
    Value.WriteToMarkerList(FMarkers);
end;

function TsdJpegImage.VerifyBitmapColorSpaceForSave: TsdJpegColorSpace;
var
  Error: boolean;
begin
  Error := False;
  Result := FStoredCS;

  // Verify bitmap colorspace, raise error if pixelformat differs
  case FBitmapCS of
  jcAutoDetect:
    begin
      // Ensure we have some valid pixelformat
      if not (FPixelFormat in [spf8bit, spf24bit, spf32bit]) then
        FPixelFormat := spf24bit;
      case FPixelFormat of
      spf8bit:  Result := jcGray;
      spf24bit: Result := jcYCbCr;
      spf32bit: Result := jcYCCK;
      end;
    end;
  jcGray:
    Error := FPixelFormat <> spf8bit;
  jcGrayA:
    Error := FPixelFormat <> spf16bit;
  jcRGB, jcYCbCr, jcPhotoYCC:
    Error := FPixelFormat <> spf24bit;
  jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
    Error := FPixelFormat <> spf32bit;
  end;
  if Error then
    Engine.RaiseError('JPG: '+sInvalidFormatForSelectedCS);

  // Select correct colorspace to store
  if Result = jcAutoDetect then
  begin
    case FBitmapCS of
    jcGray:
      Result := jcGray;
    jcGrayA:
      Result := jcGrayA;
    jcRGB, jcYCbCr:
      Result := jcYCbCr;
    jcRGBA, jcYCbCrA:
      Result := jcYCbCrA;
    else
      Result := FBitmapCS;
    end;
  end;
end;

procedure TsdJpegImage.ProvideStrip(ALeft, ATop: integer; ABitmapIter: TsdMapIterator; Target: TERRAImage);
begin
  ABitmapIter.Map := PByte(Target.GetPixelOffset(ALeft, ATop));
  ABitmapIter.ScanStride := Target.Width * 4;
  ABitmapIter.CellStride := 4;
  ABitmapIter.BitCount := 32;
end;

{ TsdNullTransform8bit }

procedure TsdNullTransform8bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count);
end;

{ TsdNullTransform16bit }

function TsdNullTransform16bit.DstCellStride: integer;
begin
  Result := 2;
end;

function TsdNullTransform16bit.SrcCellStride: integer;
begin
  Result := 2;
end;

procedure TsdNullTransform16bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 2);
end;

{ TsdNullTransform24bit }

function TsdNullTransform24bit.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdNullTransform24bit.SrcCellStride: integer;
begin
  Result := 3;
end;

procedure TsdNullTransform24bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 3);
end;

{ TsdNullTransform32bit }

function TsdNullTransform32bit.DstCellStride: integer;
begin
  Result := 4;
end;

function TsdNullTransform32bit.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdNullTransform32bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 4);
end;


{ TsdTransformInvertTriplet24bit }

function TsdTransformInvertTriplet24bit.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformInvertTriplet24bit.SrcCellStride: integer;
begin
  Result := 3;
end;

procedure TsdTransformInvertTriplet24bit.Transform(Source, Dest: pointer; Count: integer);
var
  T: byte;
  X1S, X2S, X3S: PByte;
  X1D, X2D, X3D: PByte;
begin
  // Source pointers straightforward
  X1S := Source;
  X2S := Source; inc(X2S);
  X3S := Source; inc(X3S, 2);
  // Dest pointers layed out inverted
  X3D := Dest;
  X2D := Dest; inc(X2D);
  X1D := Dest; inc(X1D, 2);

  // Check if Src = Dst
  if Source = Dest then
  begin

    // Repeat Count times
    while Count > 0 do
    begin
      T    := X1S^;
      X1S^ := X3S^;
      X3S^ := T;

      inc(X1S, 3); inc(X3S, 3);
      dec(Count);
    end;

  end else
  begin

    // Repeat Count times
    while Count > 0 do
    begin
      X1D^ := X1S^;
      X2D^ := X2S^;
      X3D^ := X3S^;
      inc(X1S, 3); inc(X2S, 3); inc(X3S, 3);
      inc(X1D, 3); inc(X2D, 3); inc(X3D, 3);
      dec(Count);
    end;

  end;
end;

{ TsdJfifTransform }

function TsdJfifTransform.RangeLimitDescale(A: integer): integer;
begin
  Result := A div FColorConvScale;
  if Result < 0 then
    Result := 0
  else
    if Result > 255 then
      Result := 255;
end;

constructor TsdJfifTransform.Create;
begin
  inherited Create;
  FColorConvScale := 1 shl 10;
end;

function TsdJfifTransform.DstCellStride: integer;
begin
  // these are defaults, can be overridden
  Result := 3;
end;

function TsdJfifTransform.SrcCellStride: integer;
begin
  // these are defaults, can be overridden
  Result := 3;
end;

{ TsdJfifFwdTransform }

procedure TsdJfifFwdTransform.InitYCbCrTables;
{ YCbCr to RGB conversion: These constants come from JFIF spec

  R = Y                      + 1.402 (Cr-128)
  G = Y - 0.34414 (Cb-128) - 0.71414 (Cr-128)
  B = Y + 1.772 (Cb-128)

  or

  R = Y                + 1.402 Cr - 179.456
  G = Y - 0.34414 Cb - 0.71414 Cr + 135.53664
  B = Y +   1.772 Cb              - 226.816
}
var
  i: integer;
begin
  F__toR := Round(-179.456   * FColorConvScale);
  F__toG := Round( 135.53664 * FColorConvScale);
  F__toB := Round(-226.816   * FColorConvScale);
  for i := 0 to 255 do
  begin
    FY_toRT[i] := Round(  1       * FColorConvScale * i);
    FCrtoRT[i] := Round(  1.402   * FColorConvScale * i);
    FCbtoGT[i] := Round( -0.34414 * FColorConvScale * i);
    FCrtoGT[i] := Round( -0.71414 * FColorConvScale * i);
    FCbtoBT[i] := Round(  1.772   * FColorConvScale * i);
  end;
end;

constructor TsdJfifFwdTransform.Create;
begin
  inherited Create;
  InitYCbCrTables;
end;

{ TsdTransformYCbCrToRGB }
procedure TsdTransformYCbCrToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  R := Dest;
  G := Dest; inc(G);
  B := Dest; inc(B, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[Y^];
    Ri := Yi +                FCrtoRT[Cr^] + F__toR;
    Gi := Yi + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG;
    Bi := Yi + FCbtoBT[Cb^]                + F__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    // Advance pointers
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrToRGBA }

function TsdTransformYCbCrToBGRA.DstCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformYCbCrToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  // RGB is layed out in memory as BGR
  R := Dest;
  G := Dest; inc(G);
  B := Dest; inc(B, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[Y^];
    Ri := Yi +                FCrtoRT[Cr^] + F__toR;
    Gi := Yi + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG;
    Bi := Yi + FCbtoBT[Cb^]                + F__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    A^ := $FF;
    // Advance pointers
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrToGray }

function TsdTransformYCbCrToGray.DstCellStride: integer;
begin
  Result := 1;
end;

procedure TsdTransformYCbCrToGray.Transform(Source, Dest: pointer; Count: integer);
var
  G, Y: PByte;
begin
  Y  := Source;
  G := Dest;
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    G^ := Y^;
    // Advance pointers
    inc(Y, 3);
    inc(G);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrAToRGB }

function TsdTransformYCbCrAToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformYCbCrAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[Y^];
    Ri := Yi +                FCrtoRT[Cr^] + F__toR;
    Gi := Yi + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG;
    Bi := Yi + FCbtoBT[Cb^]                + F__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrAToRGBA }

function TsdTransformYCbCrAToBGRA.DstCellStride: integer;
begin
  Result := 4;
end;

function TsdTransformYCbCrAToBGRA.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformYCbCrAToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr, YA: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  YA := Source; inc(YA, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[Y^];
    Ri := Yi +                FCrtoRT[Cr^] + F__toR;
    Gi := Yi + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG;
    Bi := Yi + FCbtoBT[Cb^]                + F__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    A^ := YA^;
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(YA, 4);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrKToRGB }

function TsdTransformYCbCrKToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformYCbCrKToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Ci, Mi, Yi, Ki, Ii, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ii := FY_toRT[Y^];

    Ci := Ii +                FCrtoRT[Cr^] + F__toR;// cyan
    Mi := Ii + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG;// magenta
    Yi := Ii + FCbtoBT[Cb^]                + F__toB;// yellow
    Ki := 255 * FColorConvScale - FY_toRT[K^];      // black

    // In YCbCrK, the CMYK values must be converted to produce RGB
    // Do the conversion in int
    Ri := 255 * FColorConvScale - Ci - Ki;
    Gi := 255 * FColorConvScale - Mi - Ki;
    Bi := 255 * FColorConvScale - Yi - Ki;

    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);

    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformCMYKToRGB }

function TsdTransformCMYKToBGR.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformCMYKToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformCMYKToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, C, M, Y, K: PByte;
  Ri, Gi, Bi: integer;
  function RangeLimit(A: integer): integer;
  begin
    Result := A;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  C := Source;
  M := Source; inc(M);
  Y := Source; inc(Y, 2);
  K := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := 255 - C^ - K^;
    Gi := 255 - M^ - K^;
    Bi := 255 - Y^ - K^;
    R^ := RangeLimit(Ri);
    G^ := RangeLimit(Gi);
    B^ := RangeLimit(Bi);
    // Advance pointers
    inc(C, 4); inc(M, 4); inc(Y, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformCMYKToBGR_Adobe }

function TsdTransformCMYKToBGR_Adobe.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformCMYKToBGR_Adobe.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformCMYKToBGR_Adobe.Transform(Source, Dest: pointer; Count: integer);
// When all in range [0..1]
//    CMY -> CMYK                         | CMYK -> CMY
//    Black=minimum(Cyan,Magenta,Yellow)  | Cyan=minimum(1,Cyan*(1-Black)+Black)
//    Cyan=(Cyan-Black)/(1-Black)         | Magenta=minimum(1,Magenta*(1-Black)+Black)
//    Magenta=(Magenta-Black)/(1-Black)   | Yellow=minimum(1,Yellow*(1-Black)+Black)
//    Yellow=(Yellow-Black)/(1-Black)     |
//    RGB -> CMYK                         | CMYK -> RGB
//    Black=minimum(1-Red,1-Green,1-Blue) | Red=1-minimum(1,Cyan*(1-Black)+Black)
//    Cyan=(1-Red-Black)/(1-Black)        | Green=1-minimum(1,Magenta*(1-Black)+Black)
//    Magenta=(1-Green-Black)/(1-Black)   | Blue=1-minimum(1,Yellow*(1-Black)+Black)
//    Yellow=(1-Blue-Black)/(1-Black)     |
var
  R, G, B, C, M, Y, K: PByte;
  Ck, Mk, Yk, Cu, Mu, Yu, Ku: integer;
  Ri, Gi, Bi: integer;
  function RangeLimit(A: integer): integer;
  begin
    Result := A;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  // CMYK layout
  C := Source;
  M := Source; inc(M);
  Y := Source; inc(Y, 2);
  K := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Original colour channels are inverted: uninvert them here
    Ku := 255 - K^;
    Cu := 255 - C^;
    Mu := 255 - M^;
    Yu := 255 - Y^;

    // CMYK -> CMY
    Ck := (Cu * K^) div 255;
    Mk := (Mu * K^) div 255;
    Yk := (Yu * K^) div 255;

    //CMY -> RGB
    Ri := 255 - (Ck + Ku);
    Gi := 255 - (Mk + Ku);
    Bi := 255 - (Yk + Ku);

    // Range limit
    R^ := RangeLimit(Ri);
    G^ := RangeLimit(Gi);
    B^ := RangeLimit(Bi);

    // Advance pointers
    inc(C, 4); inc(M, 4); inc(Y, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCCKToBGR }

function TsdTransformYCCKToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformYCCKToBGR.Transform(Source, Dest: pointer; Count: integer);
// YCCK is a colorspace where the CMY part of CMYK is first converted to RGB, then
// transformed to YCbCr as usual. The K part is appended without any changes.
// To transform back, we do the YCbCr -> RGB transform, then add K
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Cu, Mu, Yu, Ko, Kk: integer;
  function RangeLimit(V: integer): byte;
  begin
    if V < 0 then
      Result := 0
    else
      if V > 255 then
        Result := 255
      else
        Result := V;
  end;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[Y^];
    Ko := K^; // Inverse of K (K seems to be inverted in the file)
    Kk := (255 - Ko) * FColorConvScale; // Real K, with fixed precision

    // YCbCr converted back to CMY part of CMYK
    Cu := (Yi                + FCrtoRT[Cr^] + F__toR); //=original C of CMYK
    Mu := (Yi + FCbToGT[Cb^] + FCrtoGT[Cr^] + F__toG); //=original M of CMYK
    Yu := (Yi + FCbtoBT[Cb^]                + F__toB); //=original Y of CMYK

    // CMYK->RGB
    R^ := RangeLimitDescale(255 * FColorConvScale - (Cu * Ko) div 255 - Kk);
    G^ := RangeLimitDescale(255 * FColorConvScale - (Mu * Ko) div 255 - Kk);
    B^ := RangeLimitDescale(255 * FColorConvScale - (Yu * Ko) div 255 - Kk);

    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCCKToBGR_Adobe }

procedure TsdTransformYCCKToBGR_Adobe.InitConst;
begin
// YCCK to RGB for Adobe images is different. First, the Y, Cr and Cb are inverted,
// and k* = 220 - K. The normal YCbCr to RGB is then applied. As a last step,
// the values are scaled by 0.65 around 128
{float k = 220 - K[i], y = 255 - Y[i], cb = 255 - Cb[i], cr = 255 - Cr[i];

double val = y + 1.402 * (cr - 128) - k;
val = (val - 128) * .65f + 128;
R = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

val = y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128) - k;
val = (val - 128) * .65f + 128;
G = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

val = y + 1.772 * (cb - 128) - k;
val = (val - 128) * .65f + 128;
B = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

X* = (X - 128) * 0.65 + 128 <=>
X* = X * 0.65 + 128 - 128 * 0.65 <=>
X* = X * 0.65 + 44.8
}
  F0_65 := round(0.65 * FColorConvScale);
  F44_8 := round(44.8 * FColorConvScale);// 128 - 0.65 * 128
end;

constructor TsdTransformYCCKToBGR_Adobe.Create;
begin
  inherited Create;
  InitConst;
end;

procedure TsdTransformYCCKToBGR_Adobe.Transform(Source, Dest: pointer;
  Count: integer);
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Ki, Ri, Gi, Bi, Cbi, Cri: integer;
  function ScaleAndRangeLimit(A: integer): integer;
  begin
    // First the scaling
    A := (A * F0_65) div FColorConvScale + F44_8;
    // Undo fixed precision and range limit
    Result := A div FColorConvScale;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);
  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := FY_toRT[255 - Y^];
    Cbi := 255 - Cb^;
    Cri := 255 - Cr^;
    Ki := (220 - K^) * FColorConvScale;
    Ri := Yi                + FCrtoRT[Cri] + F__toR - Ki;
    Gi := Yi + FCbToGT[Cbi] + FCrtoGT[Cri] + F__toG - Ki;
    Bi := Yi + FCbtoBT[Cbi]                + F__toB - Ki;
    R^ := ScaleAndRangeLimit(Ri);
    G^ := ScaleAndRangeLimit(Gi);
    B^ := ScaleAndRangeLimit(Bi);
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

function TsdTransformYCCKToBGR_Adobe.SrcCellStride: integer;
begin
  Result := 4;
end;

{ TsdTransformGrayToBGR }

function TsdTransformGrayToBGR.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformGrayToBGR.SrcCellStride: integer;
begin
  Result := 1;
end;

procedure TsdTransformGrayToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    // Advance pointers
    inc(Y, 1);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformGrayAToBGR }

function TsdTransformGrayAToBGR.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformGrayAToBGR.SrcCellStride: integer;
begin
  Result := 2;
end;

procedure TsdTransformGrayAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    // Advance pointers
    inc(Y, 2);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformGrayAToBGRA }

function TsdTransformGrayAToBGRA.DstCellStride: integer;
begin
  Result := 4;
end;

function TsdTransformGrayAToBGRA.SrcCellStride: integer;
begin
  Result := 2;
end;

procedure TsdTransformGrayAToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, YA: PByte;
begin
  Y := Source;
  YA := Source; inc(YA);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    A^ := YA^;
    // Advance pointers
    inc(Y, 2); inc(Ya, 2);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformRGBToBGRA }

function TsdTransformRGBToBGRA.DstCellStride: integer;
begin
  Result := 4;
end;

function TsdTransformRGBToBGRA.SrcCellStride: integer;
begin
  Result := 3;
end;

procedure TsdTransformRGBToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source; inc(Gs);
  Bs := Source; inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Rs^;
    G^ := Gs^;
    B^ := Bs^;
    A^ := $FF;
    // Advance pointers
    inc(Rs, 3); inc(Gs, 3); inc(Bs, 3);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformRGBAToBGR }

function TsdTransformRGBAToBGR.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformRGBAToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformRGBAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source; inc(Gs);
  Bs := Source; inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Rs^;
    G^ := Gs^;
    B^ := Bs^;
    // Advance pointers
    inc(Rs, 4); inc(Gs, 4); inc(Bs, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

constructor TsdTransformBGRAToBGR.Create;
begin
  inherited;
  FBkColor := $FFFFFFFF;
end;

function TsdTransformBGRAToBGR.GetBkColor: cardinal;
begin
  Result := FBkColor and $00FFFFFF;
end;

procedure TsdTransformBGRAToBGR.SetBkColor(const Value: cardinal);
begin
  FBkColor := Value or $FF000000;
end;

procedure TsdTransformBGRAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  t: integer;
  R, G, B, A, Rd, Gd, Bd: PByte;
  Rb, Gb, Bb: byte;
begin
  // ARGB source is layed out in memory as BGRA
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);
  A := Source; inc(A, 3);

  // RGB dest is layed out in memory as BGR
  Bd := Dest;
  Gd := Dest; inc(Gd);
  Rd := Dest; inc(Rd, 2);

  // Background colors
  Rb :=  FBkColor and $000000FF;
  Gb := (FBkColor and $0000FF00) shr 8;
  Bb := (FBkColor and $00FF0000) shr 16;

  // Repeat Count times..
  while Count > 0 do
  begin
    if A^ = 0 then
    begin

      // Fully transparent: background color
      Rd^ := Rb;
      Gd^ := Gb;
      Bd^ := Bb;

    end else
    begin
      if A^ = 255 then
      begin

        // Fully opaque: foreground color
        Rd^ := R^;
        Gd^ := G^;
        Bd^ := B^;

      end else
      begin

        // Semi-transparent: "Src over Dst" operator (Porter-Duff),
        // for pre-multiplied colors, unrolled for speed

        t := A^ * Rb + $80;
        Rd^ := R^ + Rb - (t shr 8 + t) shr 8;

        t := A^ * Gb + $80;
        Gd^ := G^ + Gb - (t shr 8 + t) shr 8;

        t := A^ * Bb + $80;
        Bd^ := B^ + Bb - (t shr 8 + t) shr 8;

      end;
    end;

    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    inc(Rd, 3); inc(Gd, 3); inc(Bd, 3);
    dec(Count);
  end;
end;


function TsdTransformBGRAToBGR.DstCellStride: integer;
begin
  Result := 3;
end;

function TsdTransformBGRAToBGR.SrcCellStride: integer;
begin
  Result := 4;
end;

{ TsdJfifInvTransform }

procedure TsdJfifInvTransform.InitRGBToYCbCrTables;
{  RGB to YCbCr conversion: These constants come from JFIF spec

  Y =    0.299  R + 0.587  G + 0.114  B
  Cb = - 0.1687 R - 0.3313 G + 0.5    B + 128
  Cr =   0.5    R - 0.4187 G - 0.0813 B + 128
}
var
  i: integer;
begin
  F__toCb := Round(128 * FColorConvScale);
  F__toCr := Round(128 * FColorConvScale);
  for i := 0 to 255 do
  begin
    FRtoY_[i] := Round( 0.299  * FColorConvScale * i);
    FGtoY_[i] := Round( 0.587  * FColorConvScale * i);
    FBtoY_[i] := Round( 0.114  * FColorConvScale * i);
    FRtoCb[i] := Round(-0.1687 * FColorConvScale * i);
    FGtoCb[i] := Round(-0.3313 * FColorConvScale * i);
    FBtoCb[i] := Round( 0.5    * FColorConvScale * i);
    FRtoCr[i] := Round( 0.5    * FColorConvScale * i);
    FGtoCr[i] := Round(-0.4187 * FColorConvScale * i);
    FBtoCr[i] := Round(-0.0813 * FColorConvScale * i);
  end;
end;

constructor TsdJfifInvTransform.Create;
begin
  inherited Create;
  InitRGBToYCbCrTables;
end;

{ TsdTransformBGRToYCbCr }

procedure TsdTransformBGRToYCbCr.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: integer;
begin
  //DoDebugOut(Self, wsInfo, Format('source=%d, count=%d, nulling...(test)', [integer(Source), Count]));
  // RGB is layed out in memory as BGR
  R := Source;
  G := Source; inc(G);
  B := Source; inc(B, 2);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
    Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
    // Advance pointers
    inc(R, 3); inc(G, 3); inc(B, 3);
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    dec(Count);
  end;
end;

{ TsdTransformBGRToGray }

function TsdTransformBGRToGray.DstCellStride: integer;
begin
  Result := 1;
end;

procedure TsdTransformBGRToGray.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);

  Y  := Dest;
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Y^  := RangeLimitDescale(FRtoY_[R^] + FGtoY_[G^] + FBtoY_[B^]);
    // Advance pointers
    inc(R, 3); inc(G, 3); inc(B, 3);
    inc(Y, 1);
    dec(Count);
  end;
end;

{ TsdTransformBGRAToYCbCrA }

function TsdTransformBGRAToYCbCrA.DstCellStride: integer;
begin
  Result := 4;
end;

function TsdTransformBGRAToYCbCrA.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformBGRAToYCbCrA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr, Ay: PByte;
  Ri, Gi, Bi: integer;
begin
  // RGB is layed out in memory as BGR
  R := Source;
  G := Source; inc(G);
  B := Source; inc(B, 2);
  A := Source; inc(A, 3);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);
  Ay := Dest; inc(Ay, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
    Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
    Ay^ := A^;
    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(Ay, 4);
    dec(Count);
  end;
end;

{ TsdTransformBGRAToYCbCr }

function TsdTransformBGRAToYCbCr.SrcCellStride: integer;
begin
  Result := 4;
end;

procedure TsdTransformBGRAToYCbCr.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
    Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4);
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    dec(Count);
  end;
end;

{ TsdTransformCIELabToBGR }

constructor TsdTransformCIELabToBGR.Create;
begin
  inherited;
  // White point: Defaults to D65 (as recommended by CCIR XA/11)
  FXw := 0.9505;
  FYw := 1.0;
  FZw := 1.0890;
  // Range
  FAmin := -100;
  FAmax :=  100;
  FBmin := -100;
  FBmax :=  100;
  // Offset
  FAofs := 128;
  FBofs := 128;
end;

procedure TsdTransformCIELabToBGR.Transform(Source, Dest: pointer;
  Count: integer);
var
  Lb, Ab, Bb: PByte;
  Ld, Ad, Bd, L, M, N, X, Y, Z, R, G, B: double;
  Rf, Gf, Bf: PByte;
  // Limit to interval [0..1]
  function Limit(X: double): double;
  begin
    Result := X;
    if Result < 0 then
      Result := 0
    else if Result > 1 then
      Result := 1;
  end;
  function RangeLimitDescale(X: double): integer;
  begin
    Result := round(X * 255);
    if Result < 0 then
      Result := 0
    else if Result > 255 then
      Result := 255;
  end;
  function GFunc(X: double): double;
  // See PDF spec, section 4.5
  begin
    if X >= 6/29 then
      Result := X * X * X
    else
      Result := (108/841) * (X - (4/29));
  end;
  // sRGB gamma function
  function Gamma(X: double): double;
  begin
    if X < 0.0031308 then
      Result := 12.92 * X
    else
      Result := 1.055 * Power(X, 1.0/2.4) - 0.055;
  end;
begin
  // CIE Lab
  Lb := Source;
  Ab := Source; inc(Ab);
  Bb := Source; inc(Bb, 2);

  // RGB is layed out in memory as BGR
  Bf := Dest;
  Gf := Dest; inc(Gf);
  Rf := Dest; inc(Rf, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // First stage: adjust range
    Ld := Lb^ * (100/255);

    Ad := Ab^ - FAofs;
{    if Ad < FAmin then
      Ad := FAmin
    else
      if Ad > FAmax then
        Ad := FAmax;}

    Bd := Bb^ - FBofs;
{    if Bd < FBmin then
      Bd := FBmin
    else
      if Bd > FBmax then
        Bd := FBmax;}

    // Second stage: calculate LMN
    L := (Ld + 16) / 116 + Ad / 500;
    M := (Ld + 16) / 116;
    N := (Ld + 16) / 116 - Bd / 200;

    // Third stage: calculate XYZ
    X := FXw * GFunc(L);
    Y := FYw * GFunc(M);
    Z := FZw * GFunc(N);
{   X := Limit(X);
    Y := Limit(Y);
    Z := Limit(Z);}

    // Fourth stage: calculate sRGB:
    // XYZ to RGB matrix for sRGB, D65.. see
    // http://www.brucelindbloom.com/index.html?ColorCalculator.html

    R :=  3.24071   * X +  -1.53726  * Y + -0.498571  * Z;
    G := -0.969258  * X +   1.87599  * Y +  0.0415557 * Z;
    B :=  0.0556352 * X +  -0.203996 * Y +  1.05707   * Z;

    // Correct to sRGB
    R := Gamma(R);
    G := Gamma(G);
    B := Gamma(B);

    // Final stage: convert to RGB and limit
    Rf^ := RangeLimitDescale(R);
    Gf^ := RangeLimitDescale(G);
    Bf^ := RangeLimitDescale(B);

    // Advance pointers
    inc(Lb, 3); inc(Ab, 3); inc(Bb, 3);
    inc(Rf, 3); inc(Gf, 3); inc(Bf, 3);
    dec(Count);
  end;
end;

{ TsdTransformITUCIELabToBGR }

constructor TsdTransformITUCIELabToBGR.Create;
begin
  inherited;
  // Range
{  FAmin := -21760/255;
  FAmax :=  21590/255;
  FBmin := -19200/255;
  FBmax :=  31800/255;}
  // Offset
  FAofs := 128;
  FBofs :=  96;
end;

{ TsdDRIMarker }
function TsdDRIMarker.GetMarkerName: Utf8String;
begin
  Result := 'DRI';
end;

procedure TsdDRIMarker.ReadMarker;
begin
  // Define Restart Interval
  // Read restart interval MCU count
  FCodingInfo.FRestartInterval := GetWord(FStream);
  //DoDebugOut(Self, wsInfo, Format('Restart interval: %d', [FCodingInfo.FRestartInterval]));
end;

procedure TsdDRIMarker.WriteMarker;
begin
  FStream.Seek(0);
  PutWord(FStream, FCodingInfo.FRestartInterval);
end;

{ TsdSOIMarker }

function TsdSOIMarker.GetMarkerName: Utf8String;
begin
  Result := 'SOI';
end;

procedure TsdSOIMarker.ReadMarker;
begin
  // Start of Image
  //DoDebugOut(Self, wsInfo, '<SOI marker>');
end;

{ TsdEOIMarker }

function TsdEOIMarker.GetMarkerName: Utf8String;
begin
  Result := 'EOI';
end;

procedure TsdEOIMarker.ReadMarker;
begin
  // End of Image
  //DoDebugOut(Self, wsInfo, '<EOI marker>');
end;

{ TsdRSTMarker }

function TsdRSTMarker.GetMarkerName: Utf8String;
begin
  Result := 'RST';
end;

procedure TsdRSTMarker.ReadMarker;
begin
  //DoDebugOut(Self, wsInfo, Format('<RST%s marker>', [IntToHex(MarkerTag and $0F, 1), FStream.Size]));
end;

{ TsdDNLMarker }

function TsdDNLMarker.GetMarkerName: Utf8String;
begin
  Result := 'DNL';
end;

procedure TsdDNLMarker.ReadMarker;
begin
  FCodingInfo.FHeight := GetWord(FStream);
  //DoDebugOut(Self, wsInfo, Format('Image height: %d', [FCodingInfo.FHeight]));
end;

procedure TsdDNLMarker.WriteMarker;
begin
  FStream.Seek(0);
  PutWord(FStream, FCodingInfo.FHeight);
end;

{ TsdCOMMarker }

function TsdCOMMarker.GetComment: AnsiString;
begin
  SetLength(Result, FStream.Size);
  if FStream.Size = 0 then
    exit;
  FStream.Position := 0;
  FStream.Read(@Result[1], FStream.Size);
end;

function TsdCOMMarker.GetMarkerName: Utf8String;
begin
  Result := 'COM';
end;

procedure TsdCOMMarker.ReadMarker;
begin
  //DoDebugOut(Self, wsInfo, GetComment);
end;

procedure TsdCOMMarker.SetComment(const Value: AnsiString);
var
  Size: integer;
begin
  FStream.Seek(0);
  Size := length(Value);
  if Size >=0 then
    exit;
  FStream.Write(@Value[1], Size);
end;

{ TsdJpegBlockMap }

procedure TsdJpegBlockMap.ClearCoefBackup;
begin
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.CreateMap;
var
  Count: integer;
begin
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  // Clear the coefficients (since the decoder doesn't always reset them to 0)
  if Count > 0 then
    FillChar(FCoef[0], Count * SizeOf(smallint), 0);
  // Clear backup
  ClearCoefBackup;  
end;

function TsdJpegBlockMap.FirstCoef: pointer;
begin
  Result := @FCoef[0];
end;

function TsdJpegBlockMap.FirstCoefBackup: pointer;
begin
  Result := @FCoefBackup[0];
end;

function TsdJpegBlockMap.GetCoefPointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FCoef[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
var
  X, Y: integer;
begin
  X := FFrame.FHorzSampling * AMcuX;
  Y := FFrame.FVertSampling * AMcuY;
  while AMcuIdx >= FFrame.FHorzSampling do
  begin
    inc(Y);
    dec(AMcuIdx, FFrame.FHorzSampling);
  end;
  inc(X, AMcuIdx);
  Result := @FCoef[X * FBlockStride + Y * FScanStride];
end;

function TsdJpegBlockMap.GetSamplePointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FSample[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.HasCoefBackup: boolean;
begin
  Result := length(FCoefBackup) > 0;
end;

procedure TsdJpegBlockMap.MakeCoefBackup;
var
  Count: integer;
begin
  Count := length(FCoef);
  if Count <= 0 then exit;
  SetLength(FCoefBackup, Count);
  Move(FCoef[0], FCoefBackup[0], Count * SizeOf(smallint));
end;

function TsdJpegBlockMap.McuBlockCount(AScanCount: integer): integer;
begin
  if AScanCount = 1 then
    Result := 1
  else
    Result := FFrame.FHorzSampling * FFrame.FVertSampling;
end;

procedure TsdJpegBlockMap.ReduceBlockSize(ANewSize: integer);
var
  i, j, Count, Stride: integer;
  Sc, Dc: Psmallint;
  Ss, Ds: Pbyte;
begin
  if FBlockstride <> 64 then
    exit;

  Count := FHorzBlockCount * FVertBlockCount;

  // coefs
  Sc := @FCoef[0]; Dc := Sc;
  Stride := ANewSize * SizeOf(smallint);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Sc^, Dc^, Stride);
        inc(Dc, ANewSize);
      end;
      inc(Sc, 8);
    end;
  end;

  // samples
  Ss := @FSample[0]; Ds := Ss;
  Stride := ANewSize * SizeOf(byte);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Ss^, Ds^, Stride);
        inc(Ds, ANewSize);
      end;
      inc(Ss, 8);
    end;
  end;
  FBlockStride := ANewSize * ANewSize;
  Resize(FHorzBlockCount, FVertBlockCount);
end;

procedure TsdJpegBlockMap.Resize(AHorzBlockCount, AVertBlockCount: integer);
var
  Count: integer;
begin
  FHorzBlockCount := AHorzBlockCount;
  FVertBlockCount := AVertBlockCount;
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.SetSize(AHorzMcuCount, AVertMcuCount: integer;
  AFrame: TsdFrameComponent; ABlockStride: integer);
begin
  FFrame := AFrame;
  FBlockStride := ABlockStride;

  // Determine block dimensions
  FHorzBlockCount := AHorzMcuCount * FFrame.FHorzSampling;
  FVertBlockCount := AVertMcuCount * FFrame.FVertSampling;

  // Assume the data is valid, we can create the map
  CreateMap;
end;

function TsdJpegBlockMap.TotalBlockCount: integer;
begin
  Result := FHorzBlockCount * FVertBlockCount;
end;

{ TsdBlockMapList }
function TsdBlockMapList.GetItems(Index: integer): TsdJpegBlockMap;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Maps, _Count);

    Result := TsdJpegBlockMap.Create;
    _Maps[Index] := Result;
  End Else
    Result := _Maps[Index];
end;

procedure TsdBlockMapList.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
    ReleaseObject(_Maps[I]);

  _Count := 0;

  inherited;
End;

{ TsdJpegBlockCoder }

function TsdJpegBlockCoder.BlockstrideForScale(AScale: TsdJpegScale): integer;
begin
  case AScale of
  jsFull: Result := 64;
  jsDiv2: Result := 16;
  jsDiv4: Result := 4;
  jsDiv8: Result := 1;
  else
    Result := 0;
  end;
end;

procedure TsdJpegBlockCoder.Clear;
var
  i: integer;
begin
  inherited;
  // We only clear the backup coefficents, not the data itself, so that any new
  // SetSize may reuse already allocated memory
  for i := 0 to Maps.Count - 1 do
    Maps[i].ClearCoefBackup;
end;

procedure TsdJpegBlockCoder.CorrectBlockStride;
var
  i, NewSize: integer;
begin
  if (FBlockStride = 64) and (FScale <> jsFull) then
  begin
    // We must reduce the map blockstrides first
    NewSize := 0;
    case FScale of
    jsDiv2: NewSize := 4;
    jsDiv4: NewSize := 2;
    jsDiv8: NewSize := 1;
    end;
    for i := 0 to FMaps.Count - 1 do
      FMaps[i].ReduceBlockSize(NewSize);
    FBlockStride := NewSize * NewSize;
  end;
end;

constructor TsdJpegBlockCoder.Create(AOwner: TERRAObject; AInfo: TsdJpegInfo);
begin
  inherited;
  FMaps := TsdBlockMapList.Create;
end;

Procedure TsdJpegBlockCoder.Release;
begin
  ReleaseObject(FMaps);
  inherited;
end;

procedure TsdJpegBlockCoder.ForwardDCT;
var
  i: integer;
  FDCT: TsdJpegFDCT;
begin
  FDCT := TsdJpegFDCT.Create;
  try
    for i := 0 to FInfo.FFrameCount - 1 do
    begin
      FDCT.Map := FMaps[i];
      FDCT.PerformFDCT(FInfo.FQuantizationTables[FInfo.FFrames[i].FQTable]);
    end;
  finally
    FDCT.Free;
  end;
end;

procedure TsdJpegBlockCoder.GetBlockstrideParams(ABlockstride: integer;
  var ABlockWidth, AMcuWidth, AMcuHeight: integer);
begin
  case ABlockStride of
  64:
    begin
      ABlockWidth := 8;
      AMcuWidth := FInfo.FMcuWidth;
      AMcuHeight := FInfo.FMcuHeight;
    end;
  16:
    begin
      ABlockWidth := 4;
      AMcuWidth := FInfo.FMcuWidth div 2;
      AMcuHeight := FInfo.FMcuHeight div 2;
    end;
   4:
    begin
      ABlockWidth := 2;
      AMcuWidth := FInfo.FMcuWidth div 4;
      AMcuHeight := FInfo.FMcuHeight div 4;
    end;
   1:
    begin
      ABlockWidth := 1;
      AMcuWidth := FInfo.FMcuWidth div 8;
      AMcuHeight := FInfo.FMcuHeight div 8;
    end;
  else
    ABlockWidth := 0; // avoid warnings
    AMcuWidth := 0;
    AMcuHeight := 0;
  end;
end;

procedure TsdJpegBlockCoder.InverseDCT;
var
  i: integer;
  IDCT: TsdJpegIDCT;
begin
  IDCT := TsdJpegIDCT.Create;
  try
    IDCT.Method := FMethod;
    for i := 0 to FInfo.FFrameCount - 1 do
    begin
      IDCT.Map := FMaps[i];
      IDCT.BuildQuantTableFrom(FInfo.FQuantizationTables[FInfo.FFrames[i].FQTable]);
      IDCT.PerformIDCT;
    end;
  finally
    IDCT.Free;
  end;
end;

procedure TsdJpegBlockCoder.McuRowFromBuffer(McuY, ABlockWidth: integer);
var
  i, j, row, col, xblock, yblock, yi, m, V: integer;
  XRepeat, YRepeat, XYArea: integer;
  PixBlockStride: integer;
  Map: TsdJpegBlockMap;
  Frame: TsdFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FFrameCount - 1 do
  begin
    // Process Map
    Map := FMaps[m];
    Frame := FInfo.FFrames[m];
    PScan := PFirst;
    XRepeat := FInfo.FHorzSamplingMax div Frame.FHorzSampling;
    YRepeat := FInfo.FVertSamplingMax div Frame.FVertSampling;
    XYArea := XRepeat * YRepeat;
    PixBlockStride := ABlockWidth * XRepeat * FBufferCellStride;
    // We process VertSampling rows
    for yi := 0 to Frame.FVertSampling - 1 do
    begin
      // y is the block row-index into the map
      yblock := McuY * Frame.FVertSampling + yi;
      // Reset the block pointer to the start of the scanline
      PBlock := PScan;
      // We process a row of DCT blocks
      for xblock := 0 to Map.HorzBlockCount - 1 do
      begin
        // Pointer to the samples in this block
        PValue := Map.GetSamplePointer(xblock, yblock);
        // Reset the pixel pointer to the start of the block
        PPixel := PBlock;
        // Rows of block
        for row := 0 to ABlockWidth - 1 do
        begin
          // Check for optimized version
          if (XRepeat = 1) and (YRepeat = 1) then
          begin
            // Optimized version for no repeats
            // Columns of block
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy pixel to value
              PValue^ := PPixel^;
              inc(PPixel, FBufferCellStride);
              inc(PValue);
            end;
          end else
          begin
            // Repeats in at least one direction
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy pixel(s) to value and average
              V := 0;
              for i := 0 to XRepeat - 1 do
              begin
                inc(V, PPixel^);
                // vertical repeats?
                PCopy := PPixel;
                for j := 1 to YRepeat - 1 do
                begin
                  inc(PCopy, FBufferScanStride);
                  inc(V, PCopy^);
                end;
                inc(PPixel, FBufferCellStride);
              end;
              PValue^ := V div XYArea;
              inc(PValue);
            end;
          end;
          // Go to the next row in the block. Since we ran through the row, we
          // must also undo the blockstride
          inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
        end;
        //
        inc(PBlock, PixBlockStride);
      end;
      inc(PScan, FBufferScanStride * ABlockWidth * YRepeat);
    end;
    inc(PFirst);
  end;
end;

procedure TsdJpegBlockCoder.McuRowToBuffer(McuY: integer; ABlockWidth: integer);
var
  i, j, row, col, xblock, yblock, yi, m: integer;
  XRepeat, YRepeat: integer;
  PixBlockStride: integer;
  Map: TsdJpegBlockMap;
  Frame: TsdFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FFrameCount - 1 do
  begin
    // Process Map
    Map := FMaps[m];
    Frame := FInfo.FFrames[m];
    PScan := PFirst;
    XRepeat := FInfo.FHorzSamplingMax div Frame.FHorzSampling;
    YRepeat := FInfo.FVertSamplingMax div Frame.FVertSampling;
    PixBlockStride := ABlockWidth * XRepeat * FBufferCellStride;
    // We process VertSampling rows
    for yi := 0 to Frame.FVertSampling - 1 do
    begin
      // y is the block row-index into the map
      yblock := McuY * Frame.FVertSampling + yi;
      // Reset the block pointer to the start of the scanline
      PBlock := PScan;
      // We process a row of DCT blocks
      for xblock := 0 to Map.HorzBlockCount - 1 do
      begin
        // Pointer to the samples in this block
        PValue := Map.GetSamplePointer(xblock, yblock);
        // Reset the pixel pointer to the start of the block
        PPixel := PBlock;
        // Rows of block
        for row := 0 to ABlockWidth - 1 do
        begin
          // Check for optimized version
          if (XRepeat = 1) and (YRepeat = 1) then
          begin
            // Optimized version for no repeats
            // Columns of block
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy value to pixel
              PPixel^ := PValue^;
              inc(PPixel, FBufferCellStride);
              inc(PValue);
            end;
          end else
          begin
            // Repeats in at least one direction
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy value to pixel(s)
              for i := 0 to XRepeat - 1 do
              begin
                PPixel^ := PValue^;
                // vertical repeats?
                PCopy := PPixel;
                for j := 1 to YRepeat - 1 do
                begin
                  inc(PCopy, FBufferScanStride);
                  PCopy^ := PValue^;
                end;
                inc(PPixel, FBufferCellStride);
              end;
              inc(PValue);
            end;
          end;
          // Go to the next row in the block. Since we ran through the row, we
          // must also undo the blockstride
          inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
        end;
        //
        inc(PBlock, PixBlockStride);
      end;
      inc(PScan, FBufferScanStride * ABlockWidth * YRepeat);
    end;
    inc(PFirst);
  end;
end;

procedure TsdJpegBlockCoder.SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform);
var
  x, y, yi, BufPos, HorzCount: integer;
  BlockWidth, McuWidth, McuHeight: integer;
  PCell, PBuff, PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockstride, BlockWidth, McuWidth, McuHeight);

  // Create a buffer of McuHeight scanlines
  HorzCount := FInfo.FHorzMcuCount * McuWidth;

  FBufferCellStride := FInfo.FFrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;

  AImage.Method := imColByCol;
  PImage := AImage.First;

  // checks
  if not assigned(PImage) then
  begin
//    DoDebugOut(Self, wsFail, 'image is not assigned!');
    exit;
  end;

  if (AImage.CellStride < FBufferCellStride) then
  begin
//    DoDebugOut(Self, wsFail,      Format('image cellstride insufficient (image cellstride = %d, buffer cellstride = %d)',        [AImage.CellStride, FBufferCellStride]));
    exit;
  end;

  // create a buffer
  SetLength(FBuffer, FBufferScanStride * McuHeight);

  y := 0;
  while assigned(PImage) and (y < FInfo.FVertMcuCount) do
  begin
    // Color convert and put data in buffer
    BufPos := 0;

    yi := 0;
    while assigned(PImage) and (yi < McuHeight) do
    begin

      if AImage.CellStride = FBufferCellStride then
      begin

        // Transform one row of colors with default image cellstride
        ATransform.Transform(PImage, @FBuffer[BufPos], AImage.Width);

      end else
      begin

        // transform just one pixel
        x := 0;
        PCell := PImage;
        PBuff := @FBuffer[BufPos];
        while assigned(PCell) and (x < AImage.Width) do
        begin
          // 1 pixel transformation
          ATransform.Transform(PCell, PBuff, 1);

          // increment pointers
          inc(PBuff, FBufferCellStride);
          inc(PCell, AImage.CellStride);
          inc(x);
        end;

      end;

      // next scanline
      PImage := AImage.Next;
      inc(BufPos, FBufferScanStride);

      // increment yi
      inc(yi);
    end;

    // Combine buffer into jpeg sample maps
    McuRowFromBuffer(y, BlockWidth);

    // increment y
    inc(y);
  end;
end;

procedure TsdJpegBlockCoder.SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform);
var
  x, y, yi, BufPos, HorzCount: integer;
  BlockWidth, McuWidth, McuHeight: integer;
  PCell, PBuff, PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockstride, BlockWidth, McuWidth, McuHeight);

  // Create a buffer of McuHeight scanlines
  if FTileMode then
    HorzCount := FInfo.FTileWidth
  else
    HorzCount := FInfo.FHorzMcuCount * McuWidth;

  FBufferCellStride := FInfo.FFrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;

  // We only do the first col 0, thus this iterator loops through all the rows,
  // col 0.
  AImage.Method := imColByCol;
  PImage := AImage.First;

  // checks
  if not assigned(PImage) then
  begin
//    DoDebugOut(Self, wsFail, 'image is not assigned!');
    exit;
  end;

  // eg Adobe YCCK -> RGB uses 4 ch orig to 3 ch internal, so this strict check
  // needs to be relaxed: ATransform.DstCellStride instead of FBufferCellStride 
  if (AImage.CellStride < ATransform.DstCellStride) then
  begin
//    DoDebugOut(Self, wsFail,      Format('image cellstride insufficient (image cellstride = %d, transform dest cellstride = %d)',        [AImage.CellStride, FBufferCellStride]));
    exit;
  end;

  // create a buffer
  SetLength(FBuffer, FBufferScanStride * McuHeight);

  y := 0;
  while assigned(PImage) and (y < FInfo.FVertMcuCount) do
  begin

    // Combine jpeg sample maps into buffer of width x mcu height
    McuRowToBuffer(y, BlockWidth);

    // Color convert and put data in image
    BufPos := 0;
    yi := 0;
    while assigned(PImage) and (yi < McuHeight) do
    begin

      if AImage.CellStride = FBufferCellStride then
      begin

        // Transform one row of colors with default image cellstride
        ATransform.Transform(@FBuffer[BufPos], PImage, AImage.Width);

      end else
      begin
        // transform just one pixel
        x := 0;
        PCell := PImage;
        PBuff := @FBuffer[BufPos];
        while assigned(PCell) and (x < AImage.Width) do
        begin
          // 1 pixel transformation
          ATransform.Transform(PBuff, PCell, 1);

          // increment pointers
          inc(PBuff, FBufferCellStride);
          inc(PCell, AImage.CellStride);
          inc(x);
        end;
      end;

      // next scanline
      PImage := AImage.Next;
      inc(BufPos, FBufferScanStride);

      // increment yi
      inc(yi);
    end;

    // increment y
    inc(Y)
  end;
end;

procedure TsdJpegBlockCoder.SetupMaps(SpecialSize: boolean; AHorzMcuCount, AVertMcuCount: integer);
var
  i, HorzSampling, VertSampling: integer;
begin
  // Calculate Hmax, Vmax
  FInfo.FHorzSamplingMax := 0;
  FInfo.FVertSamplingMax := 0;
  for i := 0 to FInfo.FFrameCount - 1 do
  begin
    HorzSampling := FInfo.FFrames[i].FHorzSampling;
    if HorzSampling >= Finfo.FHorzSamplingMax then
      FInfo.FHorzSamplingMax := HorzSampling;
    VertSampling := FInfo.FFrames[i].FVertSampling;
    if VertSampling >= Finfo.FVertSamplingMax then
      FInfo.FVertSamplingMax := VertSampling;
  end;

  // MCU size in pixels
  FInfo.FMcuWidth := FInfo.FHorzSamplingMax * 8;
  FInfo.FMcuHeight := FInfo.FVertSamplingMax * 8;

  // MCU count
  FInfo.FHorzMcuCount :=  (FInfo.FWidth  + FInfo.FMcuWidth  - 1) div FInfo.FMcuWidth;
  FInfo.FVertMcuCount :=  (FInfo.FHeight + FInfo.FMcuHeight - 1) div FInfo.FMcuHeight;

  // create maps with given counts
  if SpecialSize then
    for i := 0 to FInfo.FFrameCount - 1 do
      FMaps[i].SetSize(AHorzMcuCount, AVertMcuCount, FInfo.FFrames[i], FBlockStride)
  else
  begin
    for i := 0 to FInfo.FFrameCount - 1 do
      FMaps[i].SetSize(FInfo.FHorzMcuCount, FInfo.FVertMcuCount, FInfo.FFrames[i], FBlockStride);
  end;

end;

{ TsdJpegBaselineCoder }

procedure TsdJpegBaselineCoder.Clear;
begin
  inherited;
  FDCCoders.Clear;
  FACCoders.Clear;
  FTiles.Clear;
end;

constructor TsdJpegBaselineCoder.Create(AOwner: TERRAObject; AInfo: TsdJpegInfo);
begin
  inherited;
  FDCCoders := TsdEntropyCoderList.Create;
  FACCoders := TsdEntropyCoderList.Create;
  FTiles := TsdJpegTileList.Create;
end;

function TsdJpegBaselineCoder.CreateDHTMarker: TsdDHTMarker;
var
  i: integer;
  C: Tsd8bitHuffmanEncoder;
  Item: PsdDHTMarkerInfo;
  ItemCount: integer;
begin
  Result := TsdDHTMarker.Create(FInfo, mkDHT);
  ItemCount := 0;

  // Loop through the DC tables
  for i := 0 to FDCCoders.Count - 1 do
  begin
    C := FDCCoders[i] as Tsd8bitHuffmanEncoder;
    if C is Tsd8bitHuffmanEncoder then
    begin
      SetLength(Result.FMarkerInfo, ItemCount + 1);
      Item := @Result.FMarkerInfo[ItemCount];
      Item.Tc := 0;
      Item.Th := i;
      inc(ItemCount);
      C.OptimiseHuffmanFromHistogram(Item^);
    end;
  end;

  // Loop through the AC tables
  for i := 0 to FACCoders.Count - 1 do
  begin
    C := FACCoders[i] as Tsd8bitHuffmanEncoder;
    if C is Tsd8bitHuffmanEncoder then
    begin
      SetLength(TsdDHTMarker(Result).FMarkerInfo, ItemCount + 1);
      Item := @TsdDHTMarker(Result).FMarkerInfo[ItemCount];
      Item.Tc := 1;
      Item.Th := i;
      inc(ItemCount);
      C.OptimiseHuffmanFromHistogram(Item^);
    end;
  end;
  if ItemCount = 0 then
    ReleaseObject(Result);
end;

procedure TsdJpegBaselineCoder.Decode(S: TERRAStream; Iteration: cardinal);
var
  Tile: TsdJpegTile;
  i: integer;
  McuX, McuY: integer;
{$IFDEF DETAILS}
  CountTotal: int64;
  CountCodes, CountBits: int64;
{$ENDIF}
begin
  if Iteration = 0 then
  begin
    // reset position
    S.Position := 0;

    //DoDebugOut(Self, wsInfo, Format('decoding starts (position:%d, iter:%d)',      [S.Position, Iteration]));

  end else
  begin

//    DoDebugOut(Self, wsInfo, Format('decoding continues (position:%d, iter:%d)',     [S.Position, Iteration]));

  end;

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the decoder tables for DC and AC in this scan
  InitializeDecoderTables;

  // Initialize bit reader
  FBitReader := TsdStreamBitReader.Create(S);
  try

    FTiles.Clear;
    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;
    repeat

      if (McuX = 0) and FInfo.FWaitForDNL then
      begin
        // Check if we have enough size vertically, in case of waiting for DNL marker
        if McuY >= FVertMcuCount then
          // Resize the maps, 16 MCU lines at a time. This 16 is an arbitrary number
          ResizeVerticalMcu(McuY + 16);
      end;

      // Tiled loading? Then we create the tile info for each 8 McuX blocks
      if FTileMode and (McuX mod 8 = 0)then
      begin
        Tile := TsdJpegTile.Create;
        Tile.FMcuIndex := FMcuIndex;
        Tile.FStreamPos := FBitReader.StreamPos;
        Tile.FBits := FBitReader.Bits;
        Tile.FBitsLeft := FBitReader.BitsLeft;
        SetLength(Tile.FPredictors, FInfo.FScans.Count);
        for i := 0 to FInfo.FScans.Count - 1 do
          Tile.FPredictors[i] := FInfo.FScans[i].FPredictor;
        FTiles.Add(Tile);
      end;

      // Decode one MCU, skip if tiled loading is in effect
      DecodeMcu(McuX, McuY, FTileMode);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
      begin
        McuX := 0;
        inc(McuY);
        if FInfo.FWaitForDNL then
          if HandleDNLMarker(McuY, S) then
            Break;
      end;

      // Check for errors
      if FBitReader.HitEndOfStream then
      begin
        HandleEndOfStreamError(S);
      end;

      // Check for restart interval
      if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
      begin
        HandleRestartInterval(S, True);
      end;

      // Check for markers
      if FBitReader.HitMarkerNoBitsLeft then
      begin
        HandleHitMarkerError(S);
        McuX := FMcuIndex mod FHorzMcuCount;
        McuY := FMcuIndex div FHorzMcuCount;
      end;

    until not FInfo.FWaitForDNL and (McuY = FVertMcuCount);

    // For good measure we add one more tile if in tilemode (without any data though)
    if FTileMode then
    begin
      Tile := TsdJpegTile.Create;
      Tile.FMcuIndex := FMcuIndex;
      FTiles.Add(Tile);
    end;

    ResetDecoder;

    {$IFDEF DETAILS}
    CountCodes := 0;
    CountBits := 0;
    for i := 0 to FDCCoders.Count - 1 do
    begin
      if FDCCoders[i] is TsdDCBaselineHuffmanDecoder then
      begin
        inc(CountCodes, TsdDCBaselineHuffmanDecoder(FDCCoders[i]).FCountCodes);
        inc(CountBits,  TsdDCBaselineHuffmanDecoder(FDCCoders[i]).FCountBits);
      end;
    end;
    for i := 0 to FACCoders.Count - 1 do
    begin
      if FACCoders[i] is TsdACBaselineHuffmanDecoder then
      begin
        inc(CountCodes, TsdACBaselineHuffmanDecoder(FACCoders[i]).FCountCodes);
        inc(CountBits , TsdACBaselineHuffmanDecoder(FACCoders[i]).FCountBits);
      end;
    end;

    // Report
    CountTotal := CountCodes + CountBits;

    // if CountTotal = 0, avoid div by zero
    if CountTotal = 0 then
      CountTotal := 1;

    DoDebugOut(Self, wsInfo, Format('Codes bitcout = %d (%3.1f%%)',
      [CountCodes, CountCodes * 100/CountTotal]));
    DoDebugOut(Self, wsInfo, Format('Bits  bitcout = %d (%3.1f%%)',
      [CountBits, CountBits * 100/CountTotal]));
    {$ENDIF}

  finally
    ReleaseObject(FBitReader);
    FHasCoefficients := True;
    FHasSamples := False;
  end;
end;

procedure TsdJpegBaselineCoder.DecodeBlock(S: TERRAStream; XStart, YStart, XCount, YCount: integer);
var
  x, y, i, Idx, McuIdx: integer;
  Tile: TsdJpegTile;
begin
  // Setup maps with this special count
  SetupMaps(True, XCount, YCount);

  // Initialize bit reader
  FBitReader := TsdStreamBitReader.Create(S);
  try

    for y := 0 to YCount - 1 do
    begin
      if y + YStart >= FVertMcuCount then
        break;
      FMcuIndex := (y + YStart) * FHorzMcuCount + XStart;

      // Find tile that has equal or smaller mcuindex
      Idx := FTiles.IndexByMcuIndex(FMcuIndex); // index in tilelist
      if Idx = FTiles.Count then
      begin
        //DoDebugOut(Self, wsFail, sRangeErrorInTileLoading);
        exit;
      end;
      if FTiles[Idx].FMcuIndex > FMcuIndex then
        dec(Idx);

      // Position bitreader and reset predictors
      Tile := FTiles[Idx];
      FBitReader.StreamPos := Tile.FStreamPos;
      FBitReader.Bits := Tile.FBits;
      FBitReader.BitsLeft := Tile.FBitsLeft;
      for i := 0 to length(Tile.FPredictors) - 1 do
        FInfo.FScans[i].FPredictor := Tile.FPredictors[i];

      // Skip preceding mcu's
      McuIdx := Tile.FMcuIndex;
      while McuIdx < FMcuIndex do
      begin
        DecodeMcu(0, 0, True);
        inc(McuIdx);
        if (FInfo.FRestartInterval > 0) and (McuIdx mod FInfo.FRestartInterval = 0) then
          HandleRestartInterval(S, False);
      end;

      for x := 0 to XCount - 1 do
      begin
        if x + XStart >= FHorzMcuCount then
          break;
        // Now don't skip
        DecodeMcu(x, y, False);
        inc(FMcuIndex);
        // Check for restart interval
        if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
          HandleRestartInterval(S, False);
      end;
    end;

  finally
    ReleaseObject(FBitReader);
    FHasCoefficients := True;
    FHasSamples := False;
  end;
end;

procedure TsdJpegBaselineCoder.DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean);
var
  i: integer;
  McuBlock: PsdMCUBlock;
  Dummy: TsdCoefBlock;
begin
  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];

    // Initialize MCU values pointer
    if Skip then
      McuBlock.Values := @Dummy[0]
    else
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx);

    // Each MCU block has an index to a DC and AC table, use it to do the decoding
    TsdDCBaselineHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeMcuBlock(McuBlock^, FBitReader);
    if (FScale = jsDiv8) or Skip then
      TsdACBaselineHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeMcuBlockSkip(FBitReader)
    else
      TsdACBaselineHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeMcuBlock(McuBlock^, FBitReader, FZigZag);
    if FBitReader.HitEndOfStream then
      exit;
  end;
end;

Procedure TsdJpegBaselineCoder.Release;
begin
  ReleaseObject(FDCCoders);
  ReleaseObject(FACCoders);
  ReleaseObject(FTiles);
  inherited;
end;

procedure TsdJpegBaselineCoder.DoMcuBlockCount;
var
  HSize, VSize: integer;
  i: integer;
  Frame: TsdFrameComponent;
begin
  if FInfo.FScanCount = 1 then
  begin
    // Single channel: spec tells there can only be one MCU block
    FMcuBlockCount := 1;
    // calculate # blocks in horz and vert direction
    Frame := FInfo.FFrames[FInfo.FScans[0].FComponent];
    HSize := 8 * FInfo.FHorzSamplingMax div Frame.FHorzSampling;
    VSize := 8 * FInfo.FVertSamplingMax div Frame.FVertSampling;
    FHorzMcuCount := (FInfo.FWidth + HSize - 1) div HSize;
    FVertMcuCount := (FInfo.FHeight + VSize - 1) div VSize;
  end else
  begin
    // Multi channel
    FHorzMcuCount := FInfo.FHorzMcuCount;
    FVertMcuCount := FInfo.FVertMcuCount;
    FMcuBlockCount := 0;
    for i := 0 to FInfo.FScanCount - 1 do
      inc(FMcuBlockCount, Maps[FInfo.FScans[i].FComponent].McuBlockCount(FInfo.FScanCount));
  end;
  SetLength(FMcu, FMcuBlockCount);
end;

procedure TsdJpegBaselineCoder.Encode(S: TERRAStream; Iteration: cardinal);
var
  B: byte;
  McuX, McuY: integer;
begin
  FIsDryRun := (S = nil);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit reader
  if FIsDryRun then
    FBitWriter := TsdDryRunBitWriter.Create(S)
  else
    FBitWriter := TsdBitWriter.Create(S);
  try

    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;
    repeat

      // Encode one MCU
      EncodeMcu(McuX, McuY);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
      begin
        McuX := 0;
        inc(McuY);
      end;

      if McuY = FVertMcuCount then break;

      // Check for restart interval
      if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
      begin
        // Restart interval
        ResetEncoder;
        if not FIsDryRun then
        begin
          // write RST
          B := $FF;
          S.WriteByte(B);
          B := (FRstIndex mod 8) + mkRST0;
          S.WriteByte(B);
        end;
        inc(FRstIndex);
      end;

    until (McuY = FVertMcuCount);
    ResetEncoder;

  finally
    ReleaseObject(FBitWriter);
  end;
end;

procedure TsdJpegBaselineCoder.EncodeMcu(AMcuX, AMcuY: integer);
var
  i: integer;
  McuBlock: PsdMCUBlock;
  DC: TsdDCBaselineHuffmanEncoder;
  AC: TsdACBaselineHuffmanEncoder;
begin
  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];
    // Initialize MCU values pointer
    McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx);
    // Each MCU block has an index to a DC and AC table, use it to do the encoding
    DC := TsdDCBaselineHuffmanEncoder(FDCCoders[McuBlock.DCTable]);
    AC := TsdACBaselineHuffmanEncoder(FACCoders[McuBlock.ACTable]);
    if FIsDryRun then
      TsdDryRunBitWriter(FBitWriter).Histogram := DC.Histogram;
    DC.EncodeMcuBlock(McuBlock^, FBitWriter);
    if FIsDryRun then
      TsdDryRunBitWriter(FBitWriter).Histogram := AC.Histogram;
    AC.EncodeMcuBlock(McuBlock^, FBitWriter);
  end;
end;

procedure TsdJpegBaselineCoder.EncodeStrip(S: TERRAStream);
var
  McuX: integer;
  B: byte;
begin
  McuX := 0;
  repeat

    // Encode one MCU
    EncodeMcu(McuX, 0);
    inc(FMcuIndex);
    inc(McuX);
    if McuX = FHorzMcuCount then
      break;

    // Check for restart interval
    if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
    begin
      // Restart interval
      ResetEncoder;
      // write RST
      B := $FF;
      S.WriteByte(B);
      B := (FRstIndex mod 8) + mkRST0;
      S.WriteByte(B);
      inc(FRstIndex);
    end;

  until False;
end;

procedure TsdJpegBaselineCoder.EncodeStripClose;
begin
  ResetEncoder;
  ReleaseObject(FBitWriter);
end;

procedure TsdJpegBaselineCoder.EncodeStripStart(S: TERRAStream);
begin
  // Setup maps to the size of one strip
  SetupMaps(True, FInfo.FHorzMCUCount, 1);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit writer
  FBitWriter := TsdBitWriter.Create(S);

  FMcuIndex := 0;
  FRstIndex := 0;
end;

function TsdJpegBaselineCoder.HandleDNLMarker(AMcuY: integer; S: TERRAStream): boolean;
var
  ReadBytes: integer;
  B, Tag: byte;
begin
  Result := False;
  if FBitReader.HitMarker then
  begin
    // It should be a DNL marker
    ResetDecoder;
    ReadBytes := S.Read(@B, 1);
    if not (ReadBytes = 1) or (B <> $FF) then
    begin
      //DoDebugOut(Self, wsFail, sDNLMarkerExpected);
      exit;
    end;
    S.ReadByte(Tag);
    if Tag <> mkDNL then
    begin
      //DoDebugOut(Self, wsWarn, sDNLMarkerExpected);
      exit;
    end;
    FInfo.FWaitForDNL := False;
    ResizeVerticalMcu(AMcuY);
    Result := True;
  end;
end;

procedure TsdJpegBaselineCoder.HandleEndOfStreamError(S: TERRAStream);
begin
  // Serious error: there weren't enough bits in the stream
(*  if FBitReader.HitMarkerNoBitsLeft then
    DoDebugOut(Self, wsFail, Format('Error: Hit Marker $%s', [IntToHex(FBitReader.MarkerTag, 2)]));*)
  ResetDecoder;
  //DoDebugOut(Self, wsFail, Format('Error: Premature stream end at position %d', [S.Position]));
end;

procedure TsdJpegBaselineCoder.HandleHitMarkerError(S: TERRAStream);
begin
  case FBitReader.MarkerTag of
  mkRST0..mkRST7:
    begin
      // We found a restart too early, set McuIndex to the correct value
      //DoDebugOut(Self, wsWarn, Format('Restart interval %d (too early)', [FRstIndex]));
      inc(FRstIndex);
      FMcuIndex := FRstIndex * FInfo.FRestartInterval;
      ResetDecoder;
      S.Skip(2);
      FBitReader.Reload;
    end;
  end;//case
end;

procedure TsdJpegBaselineCoder.HandleRestartInterval(S: TERRAStream; Warn: boolean);
// Restart interval
var
  SuperfluousCount, ReadBytes: integer;
  B: byte;
  Tag: byte;
begin
  ResetDecoder;
  // find + skip restart
  SuperfluousCount := 0;
  repeat
    ReadBytes := S.Read(@B, 1);
    if B = $FF then
    begin
      S.ReadByte(Tag);
      case Tag of
      mkRST0..mkRST7:
        begin
          // Check restart interval
          (*if Warn then
            if (Tag - mkRST0) <> (FRstIndex mod 8) then
              DoDebugOut(Self, wsWarn, Format('WARNING: Restart interval error (expected: %d, found: %d)',
                [Tag - mkRST0, FRstIndex mod 8]));*)
          break;
        end;
      mkEOI:
        begin
          S.Seek(S.Position -2);
          break;
        end;
      else
        // Any other tag is an error
        (*if Warn then
          DoDebugOut(Self, wsWarn, sUnexpectedMarkerInEncodedStream);*)
        break;
      end;
    end;
    // If we're here, we had superfluous bytes in the stream
    if ReadBytes > 0 then
      inc(SuperfluousCount);
  until ReadBytes = 0;

  (*if SuperfluousCount > 0 then
    DoDebugOut(Self, wsWarn, Format('WARNING: %d superfluous bytes found at pos %d', [SuperfluousCount, S.Position - SuperfluousCount]));*)

  inc(FRstIndex);
  FBitReader.Reload;
end;

procedure TsdJpegBaselineCoder.Initialize(AScale: TsdJpegScale);
begin
  inherited;

  // Determine blockstride
  FBlockStride := BlockStrideForScale(Scale);

  // Setup image maps in frame
  if FTileMode then
    // In tilemode, we create maps with zero size, size will be set later
    SetupMaps(True, 0, 0)
  else
    // otherwise we create maps at full-size
    SetupMaps(False, 0, 0);
end;

procedure TsdJpegBaselineCoder.InitializeDecoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCBaselineHuffmanDecoder;
  AC: TsdACBaselineHuffmanDecoder;
begin
  // Zigzag array that is used
  case BlockStride of
  64: FZigZag := @cJpegInverseZigZag8x8;
  16: FZigZag := @cJpegInverseZigZag4x4;
   4: FZigZag := @cJpegInverseZigZag2x2;
   1: FZigZag := @cJpegInverseZigZag1x1;
  end;

  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.FScans[i];
    // Create DC and AC decoders for i-th image
    if not assigned(FDCCoders[Scan.FDCTable])
       and (TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable].Count > 0) then
    begin
      DC := TsdDCBaselineHuffmanDecoder.Create();
      FDCCoders[Scan.FDCTable] := DC;
      DC.GenerateLookupTables(TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable]);
    end;
    if not assigned(FACCoders[Scan.FACTable])
       and (TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable].Count > 0) then
    begin
      AC := TsdACBaselineHuffmanDecoder.Create();
      FACCoders[Scan.FACTable] := AC;
      AC.GenerateLookupTables(TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable]);
    end;
    // Assign table numbers to MCU blocks
    for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
    begin
      FMcu[Idx].DCTable := Scan.FDCTable;
      FMcu[Idx].ACTable := Scan.FACTable;
      FMcu[Idx].PPred := @Scan.FPredictor;
      FMcu[Idx].BlockIdx := j;
      FMcu[Idx].MapIdx := Scan.FComponent;
      inc(Idx);
    end;
  end;
end;

procedure TsdJpegBaselineCoder.InitializeEncoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCBaselineHuffmanEncoder;
  AC: TsdACBaselineHuffmanEncoder;
begin
  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.FScans[i];
    // Create DC and AC decoders for i-th image
    if not assigned(FDCCoders[Scan.FDCTable])
       and ((TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable].Count > 0) or FIsDryRun) then
    begin
      DC := TsdDCBaselineHuffmanEncoder.Create();
      FDCCoders[Scan.FDCTable] := DC;
      DC.GenerateCodeTable(TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable]);
    end;
    if not assigned(FACCoders[Scan.FACTable])
       and ((TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable].Count > 0) or FIsDryRun) then
    begin
      AC := TsdACBaselineHuffmanEncoder.Create();
      FACCoders[Scan.FACTable] := AC;
      AC.GenerateCodeTable(TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable]);
    end;
    // Assign table numbers to MCU blocks
    for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
    begin
      FMcu[Idx].DCTable := Scan.FDCTable;
      FMcu[Idx].ACTable := Scan.FACTable;
      FMcu[Idx].PPred := @Scan.FPredictor;
      FMcu[Idx].BlockIdx := j;
      FMcu[Idx].MapIdx := Scan.FComponent;
      inc(Idx);
    end;
  end;
end;

procedure TsdJpegBaselineCoder.ResetDecoder;
var
  i: integer;
begin
  FBitReader.Reset;
  // Also reset the DC PRED values
  for i := 0 to FInfo.FScanCount - 1 do
    FInfo.FScans[i].FPredictor := 0;
end;

procedure TsdJpegBaselineCoder.ResetEncoder;
var
  i: integer;
begin
  if assigned(FBitWriter) then
    FBitWriter.Restart;
  // Also reset the DC PRED values
  for i := 0 to FInfo.FScanCount - 1 do
    FInfo.FScans[i].FPredictor := 0;
end;

procedure TsdJpegBaselineCoder.ResizeVerticalMcu(NewVertMcuCount: integer);
var
  i: integer;
  HorzBlockCount, VertBlockCount: integer;
begin
  FVertMcuCount := NewVertMcuCount;
  FInfo.FVertMcuCount :=  NewVertMcuCount;

  // Resize maps
  for i := 0 to FInfo.FFrameCount - 1 do
  begin
    HorzBlockCount := FInfo.FHorzMcuCount * FInfo.FFrames[i].FHorzSampling;
    VertBlockCount := FInfo.FVertMcuCount * FInfo.FFrames[i].FVertSampling;
    Maps[i].Resize(HorzBlockCount, VertBlockCount);
  end;
end;

{ TsdJpegProgressiveCoder }

function TsdJpegProgressiveCoder.BlockstrideForScale(AScale: TsdJpegScale): integer;
begin
  // Blockstride is *always* 64 for progressive coding, because the coder depends
  // on AC coefficents being set.
  Result := 64;
end;

procedure TsdJpegProgressiveCoder.Decode(S: TERRAStream; Iteration: cardinal);
begin
  // Decide which band (DC or AC) and whether first scan
  FIsDCBand := FInfo.FSpectralStart = 0;
  FIsFirst := FInfo.FApproxHigh = 0;
  FEOBRun := 0;

  //DoDebugOut(Self, wsInfo, format('IsDCBand=%d, IsFirst=%d',
  //  [integer(FIsDCBand), integer(FIsFirst)]));

  if FTileMode then
    Engine.RaiseError('JPG: '+sCannotUseTileMode);

  // Use the standard decoder, with overridden methods
  inherited Decode(S, Iteration);
end;

procedure TsdJpegProgressiveCoder.DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean);
var
  i: integer;
  McuBlock: PsdMCUBlock;
begin
  //if (AMcuX=0) and (AMcuY=0) then
  //  DoDebugOut(Self, wsInfo, format(
  //    'progressive decode mcux=%d mcuy=%d isdcband=%d isfirst=%d eobrun=%d',
  //    [AMcuX, AMcuY, integer(FIsDCBand), integer(FIsFirst), FEOBRun]));

  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];

    // Initialize MCU values pointer
    if FInfo.FScanCount > 1 then
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx)
    else
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointer(AMcuX, AMcuY);

    // Each MCU block has an index to a DC and AC table, use it to do the decoding
    if FIsDCBand and assigned(FDCCoders[McuBlock.DCTable]) then
    begin
      if FIsFirst then
        TsdDCProgressiveHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeProgFirst(McuBlock^,
          FBitReader, FInfo.FApproxLow)
      else
        TsdDCProgressiveHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeProgRefine(McuBlock^,
          FBitReader, FInfo.FApproxLow);
    end;
    if not FIsDCBand and assigned(FACCoders[McuBlock.ACTable]) then
    begin
      if FIsFirst then
        TsdACProgressiveHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeProgFirst(McuBlock^,
          FBitReader, FEOBRun, FInfo.FSpectralStart, FInfo.FSpectralEnd, FInfo.FApproxLow)
      else
        TsdACProgressiveHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeProgRefine(McuBlock^,
          FBitReader, FEOBRun, FInfo.FSpectralStart, FInfo.FSpectralEnd, FInfo.FApproxLow);
    end;

    if FBitReader.HitEndOfStream then
    begin
      //DoDebugOut(Self, wsInfo, 'hit end of stream');
      exit;
    end;
  end;
end;

procedure TsdJpegProgressiveCoder.Finalize;
begin
  CorrectBlockStride;
end;

procedure TsdJpegProgressiveCoder.HandleRestartInterval(S: TERRAStream; Warn: boolean);
begin
  inherited;
  // Reset EOB run
  FEOBRun := 0;
end;

procedure TsdJpegProgressiveCoder.InitializeDecoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCProgressiveHuffmanDecoder;
  AC: TsdACProgressiveHuffmanDecoder;
begin
  // Initialize tables to use (max 4 per AC/DC)
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.FScans[i];

    // Create DC and AC decoders for i-th image
    if FIsDCBand
       and not assigned(FDCCoders[Scan.FDCTable])
       and (TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable].Count > 0) then
    begin
      DC := TsdDCProgressiveHuffmanDecoder.Create();
      FDCCoders[Scan.FDCTable] := DC;
      DC.GenerateLookupTables(TsdHuffmanTableList(FInfo.FDCHuffmanTables)[Scan.FDCTable]);
    end;
    if not FIsDCBand
       and not assigned(FACCoders[Scan.FACTable])
       and (TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable].Count > 0) then
    begin
      AC := TsdACProgressiveHuffmanDecoder.Create();
      FACCoders[Scan.FACTable] := AC;
      AC.GenerateLookupTables(TsdHuffmanTableList(FInfo.FACHuffmanTables)[Scan.FACTable]);
    end;

    // Assign table numbers to MCU blocks
    for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
    begin
      FMcu[Idx].DCTable := Scan.FDCTable;
      FMcu[Idx].ACTable := Scan.FACTable;
      FMcu[Idx].PPred := @Scan.FPredictor;
      FMcu[Idx].BlockIdx := j;
      FMcu[Idx].MapIdx := Scan.FComponent;
      inc(Idx);
    end;
  end;
end;



{ TsdJpegFDCT }
procedure ForwardDCTIntAccurate8x8(const Sample: TsdSampleBlock; out Coef: TsdCoefBlock;
  var Wrksp: TsdIntArray64);
var
  i: integer;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  z1, z2, z3, z4, z5, z10, z11, z12, z13: integer;
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp10, tmp11, tmp12, tmp13: integer;
  // local
  function DescaleMin(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl (cConstBits - cPass1Bits));
  end;
  function DescalePlus(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl (cConstBits + cPass1Bits));
  end;
  function DescalePass(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl cPass1Bits);
  end;
// main
begin

  // Pass 1: process rows.
  // Note results are scaled up by sqrt(8) compared to a true DCT;
  // furthermore, we scale the results by 2**PASS1_BITS.
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  s4 := @Sample[4]; s5 := @Sample[5]; s6 := @Sample[6]; s7 := @Sample[7];
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  w4 := @Wrksp[4]; w5 := @Wrksp[5]; w6 := @Wrksp[6]; w7 := @Wrksp[7];

  for i := 0 to 7 do
  begin
    // Samples are in range 0..255, but we must put them in range -128..127
    // So if two samples are added, we should substract 2 times the centersample
    // value, and if two samples are substracted, we do not have to correct.
    tmp0 := s0^ + s7^ - 2 * cCenterSample;
    tmp1 := s1^ + s6^ - 2 * cCenterSample;
    tmp2 := s2^ + s5^ - 2 * cCenterSample;
    tmp3 := s3^ + s4^ - 2 * cCenterSample;
    tmp7 := s0^ - s7^;
    tmp6 := s1^ - s6^;
    tmp5 := s2^ - s5^;
    tmp4 := s3^ - s4^;

    // Even part per LL&M figure 1 --- note that published figure is faulty;
    // rotator "sqrt(2)*c1" should be "sqrt(2)*c6".

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    w0^ := (tmp10 + tmp11) shl cPass1Bits;
    w4^ := (tmp10 - tmp11) shl cPass1Bits;

    z1 := (tmp12 + tmp13) * FIX_0_541196100AF;
    w2^ := DescaleMin(z1 + tmp13 * FIX_0_765366865AF);
    w6^ := DescaleMin(z1 - tmp12 * FIX_1_847759065AF);

    // Odd part per figure 8 --- note paper omits factor of sqrt(2).
    // cK represents cos(K*pi/16).
    // i0..i3 in the paper are tmp4..tmp7 here.

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := (z3 + z4) * FIX_1_175875602AF; // sqrt(2) * c3

    tmp4 := tmp4 * FIX_0_298631336AF; // sqrt(2) * (-c1+c3+c5-c7)
    tmp5 := tmp5 * FIX_2_053119869AF; // sqrt(2) * ( c1+c3-c5+c7)
    tmp6 := tmp6 * FIX_3_072711026AF; // sqrt(2) * ( c1+c3+c5-c7)
    tmp7 := tmp7 * FIX_1_501321110AF; // sqrt(2) * ( c1+c3-c5-c7)
    z1 := - z1 * FIX_0_899976223AF; // sqrt(2) * (c7-c3)
    z2 := - z2 * FIX_2_562915447AF; // sqrt(2) * (-c1-c3)
    z3 := - z3 * FIX_1_961570560AF; // sqrt(2) * (-c3-c5)
    z4 := - z4 * FIX_0_390180644AF; // sqrt(2) * (c5-c3)

    Inc(z3, z5);
    Inc(z4, z5);

    w7^ := DescaleMin(tmp4 + z1 + z3);
    w5^ := DescaleMin(tmp5 + z2 + z4);
    w3^ := DescaleMin(tmp6 + z2 + z3);
    w1^ := DescaleMin(tmp7 + z1 + z4);

    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;

  // Pass 2: process columns.
  // We remove the PASS1_BITS scaling, but leave the results scaled up
  // by an overall factor of 8.

  p0 := @Coef[ 0]; p1 := @Coef[ 8]; p2 := @Coef[16]; p3 := @Coef[24];
  p4 := @Coef[32]; p5 := @Coef[40]; p6 := @Coef[48]; p7 := @Coef[56];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 8]; w2 := @Wrksp[16]; w3 := @Wrksp[24];
  w4 := @Wrksp[32]; w5 := @Wrksp[40]; w6 := @Wrksp[48]; w7 := @Wrksp[56];
  for i := 0 to 7 do
  begin
    tmp0 := w0^ + w7^;
    tmp7 := w0^ - w7^;
    tmp1 := w1^ + w6^;
    tmp6 := w1^ - w6^;
    tmp2 := w2^ + w5^;
    tmp5 := w2^ - w5^;
    tmp3 := w3^ + w4^;
    tmp4 := w3^ - w4^;

    // Even part per LL&M figure 1 --- note that published figure is faulty;
    // rotator "sqrt(2)*c1" should be "sqrt(2)*c6".

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    p0^ := DescalePass(tmp10 + tmp11);
    p4^ := DescalePass(tmp10 - tmp11);

    z1 := (tmp12 + tmp13) * FIX_0_541196100AF;
    p2^ := DescalePlus(z1 + tmp13 * FIX_0_765366865AF);
    p6^ := DescalePlus(z1 - tmp12 * FIX_1_847759065AF);

    // Odd part per figure 8 --- note paper omits factor of sqrt(2).
    // cK represents cos(K*pi/16).
    // i0..i3 in the paper are tmp4..tmp7 here.

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := (z3 + z4) * FIX_1_175875602AF; // sqrt(2) * c3 }

    tmp4 := tmp4 * FIX_0_298631336AF; // sqrt(2) * (-c1+c3+c5-c7)
    tmp5 := tmp5 * FIX_2_053119869AF; // sqrt(2) * ( c1+c3-c5+c7)
    tmp6 := tmp6 * FIX_3_072711026AF; // sqrt(2) * ( c1+c3+c5-c7)
    tmp7 := tmp7 * FIX_1_501321110AF; // sqrt(2) * ( c1+c3-c5-c7)
    z1 := - z1 * FIX_0_899976223AF; // sqrt(2) * (c7-c3)
    z2 := - z2 * FIX_2_562915447AF; // sqrt(2) * (-c1-c3)
    z3 := - z3 * FIX_1_961570560AF; // sqrt(2) * (-c3-c5)
    z4 := - z4 * FIX_0_390180644AF; // sqrt(2) * (c5-c3)

    Inc(z3, z5);
    Inc(z4, z5);

    p7^ := DescalePlus(tmp4 + z1 + z3);
    p5^ := DescalePlus(tmp5 + z2 + z4);
    p3^ := DescalePlus(tmp6 + z2 + z3);
    p1^ := DescalePlus(tmp7 + z1 + z4);

    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3); inc(p4); inc(p5); inc(p6); inc(p7);
    inc(w0); inc(w1); inc(w2); inc(w3); inc(w4); inc(w5); inc(w6); inc(w7);
  end;
end;


procedure TsdJpegFDCT.PerformFDCT(ATable: TsdQuantizationTable);
var
  i, j, k: integer;
  PCoef: PsdCoefBlock;
  PSample: PsdSampleBlock;
  Work: TsdIntArray64;
  FFDctMethod: TFDCTMethod;
  CVal, QVal: SmallInt;
begin
  // Quantization coefficients, unzigzagged
  for i := 0 to 63 do
    // We multiply divisors by 8 because the FDCT will create values that
    // are multiplied by 8 (shl 3) versus what they should be according
    // to theoretical DCT.
    FQuant[cJpegInverseZigZag8x8[i]] := ATable.FQuant[i] * 8;

  // Forward DCT method (we always use this one)
  FFDctMethod := ForwardDCTIntAccurate8x8;

  for j := 0 to FMap.VertBlockCount - 1 do
  begin
    for i := 0 to FMap.HorzBlockCount - 1 do
    begin
      // DCT the samples into coefficients
      PSample := FMap.GetSamplePointer(i, j);
      PCoef := FMap.GetCoefPointer(i, j);
      FFDctMethod(PSample^, PCoef^, Work);

      // Quantize the coefficients
      for k := 0 to 63 do
      begin
        CVal := PCoef[k];
        QVal := FQuant[k];
        if CVal < 0 then
        begin
          CVal := -CVal;
          inc(CVal, QVal shr 1); // rounding
          if CVal >= QVal then
            CVal := - (CVal div QVal)
          else
            CVal := 0;
        end else
        begin
          inc(CVal, QVal shr 1); // rounding
          if CVal >= QVal then
            CVal := CVal div QVal
          else
            CVal := 0;
        end;
        PCoef[k] := CVal;
      end;
    end;
  end;
end;

{ TsdEntropyCoder }
constructor TsdEntropyCoder.Create;
begin
  inherited Create;
end;

{ TsdEntropyCoderList }
procedure TsdEntropyCoderList.Clear;
begin
  _Count := 0;
end;

function TsdEntropyCoderList.GetItems(Index: integer): TsdEntropyCoder;
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Coders, _Count);
  End;

  If (Index>=0) And (Index<_Count) Then
    Result := _Coders[Index]
  Else
    Result := Nil;
end;

procedure TsdEntropyCoderList.SetItems(Index: integer; const Value: TsdEntropyCoder);
begin
  if Index >= Count then
  Begin
    _Count := Index + 1;
    SetLength(_Coders, _Count);
  End;

  If (Index>=0) And (Index<_Count) Then
    _Coders[Index] := Value;
end;

{ TsdBitReader }

constructor TsdBitReader.Create(S: TERRAStream);
begin
  inherited Create;
  // These two points to bits register, which is inverted in memory,
  // so increment by 3 for first (MSB) byte, and by 2 for next byte
  ThisByte := @FBits; inc(ThisByte, 3);
  NextByte := @FBits; inc(NextByte, 2);
  FStream := S;
end;

function TsdBitReader.GetBits(Count: integer): cardinal;
begin
  // Count is guaranteed <= 16 under normal circumstances
  if Count > FBitsLeft then
    FHitEndOfStream := True;
  Result := FBits shr (32 - Count);
  RemoveBits(Count);
end;

function TsdBitReader.GetStreamPos: int64;
begin
  Result := FStream.Position;
end;

function TsdBitReader.HasError: boolean;
begin
  Result := FHitMarker or FHitEndOfStream;
end;

function TsdBitReader.HitMarkerNoBitsLeft: boolean;
begin
  Result := FHitMarker and (FBitsLeft <= 0);
end;

procedure TsdBitReader.Reload;
begin
  // Fill 'r up
  RemoveBits(0);
end;

procedure TsdBitReader.Reset;
begin
  FBits := 0;
  FBitsLeft := 0;
  FHitMarker := False;
  FHitEndOfStream := False;
  FMarkerTag := 0;
end;

procedure TsdBitReader.SetStreamPos(const Value: int64);
begin
  FStream.Position := Value;
end;


{ TsdStreamBitReader }
constructor TsdStreamBitReader.Create(S: TERRAStream);
begin
  inherited Create(S);
  // Fill up register, with trick to call RemoveBits
  RemoveBits(0);
end;

procedure TsdStreamBitReader.RemoveBits(Count: integer);
var
  B: byte;
  BytesRead: integer;
begin
  FBits := FBits shl Count;
  dec(FBitsLeft, Count);
  while(FBitsLeft <= 24) do
  begin
    BytesRead := FStream.Read(@B, 1);
    if BytesRead = 0 then break;
    if B = $FF then
    begin
      // Skipping $FF00 and markers
      FStream.Read(@B, 1);
      if B = $00 then
      begin
        // Skip $00, add $FF
        FBits := FBits + $FF shl (24 - FBitsLeft);
        inc(FBitsLeft, 8);
        continue;
      end else
      begin
        // We hit a marker
        FHitMarker := True;
        FMarkerTag := B;
        FStream.Seek(FStream.Position -2);
        break;
      end;
    end;
    FBits := FBits + B shl (24 - FBitsLeft);
    inc(FBitsLeft, 8);
  end;
end;

{ TsdBitWriter }

function TsdBitWriter.CountBits(AValue: integer): integer;
begin
  if AValue < 0 then AValue := -AValue;
  Result := 0;
  while AValue > 0 do
  begin
    inc(Result);
    AValue := AValue shr 1;
  end;
end;

constructor TsdBitWriter.Create(S: TERRAStream);
begin
  inherited Create;
  FStream := S;
end;

procedure TsdBitWriter.Emit(B: byte);
begin
  FBuffer[FBufferPos] := B;
  inc(FBufferPos);
  if B = $FF then
  begin
    FBuffer[FBufferPos] := 0;
    inc(FBufferPos);
  end;
end;

procedure TsdBitWriter.FlushBuffer;
begin
  if FBufferPos > 0 then
    FStream.Write(@FBuffer[0], FBufferPos);
  FBufferPos := 0;
end;

procedure TsdBitWriter.PutBits(Bits: cardinal; Count: integer);
begin
  inc(FBitsStored, Count);
  FStored := FStored + Bits shl (32 - FBitsStored);
  if FBitsStored >= 16 then
  begin
    Emit(FStored shr 24);
    Emit(FStored shr 16 and $FF);
    FStored := FStored shl 16;
    FBitsStored := FBitsStored - 16;
    if FBufferPos >= 1024 then
      FlushBuffer;
  end;
end;

procedure TsdBitWriter.PutCode(ACode: PsdHuffmanCode);
begin
  if ACode.L = 0 then
  begin
    //DoDebugOut(Self, wsWarn, 'invalid Huffman code');
  end;
  PutBits(ACode.Code, ACode.L);
end;

procedure TsdBitWriter.PutCodeExtend(ACode: PsdHuffmanCode; AValue, ABitCount: integer);
begin
  PutCode(ACode);
  if ABitCount = 0 then
    exit;
  if ABitCount = 1 then
  begin
    if AValue > 0 then
      PutBits(1, 1)
    else
      PutBits(0, 1);
    exit;
  end;
  if AValue > 0 then
    PutBits(AValue, ABitCount)
  else
    PutBits(AValue - cExtendOffset[ABitCount], ABitCount);
end;

procedure TsdBitWriter.Restart;
begin
  if FBitsStored > 0 then
  begin
    while FBitsStored mod 8 <> 0 do
      PutBits(1, 1);
    if FBitsStored = 8 then
      Emit(FStored shr 24);
  end;
  FStored := 0;
  FBitsStored := 0;
  FlushBuffer;
end;

{ TsdDryRunBitWriter }

procedure TsdDryRunBitWriter.PutBits(Bits: cardinal; Count: integer);
begin
// this does nothing
end;

procedure TsdDryRunBitWriter.PutCode(ACode: PsdHuffmanCode);
begin
  // increment the histogram
  inc(FHistogram[ACode.V]);
end;

procedure TsdDryRunBitWriter.PutCodeExtend(ACode: PsdHuffmanCode; AValue,
  ABitCount: integer);
begin
  PutCode(ACode);
end;

procedure TsdDryRunBitWriter.Restart;
begin
// this does nothing
end;

{ TsdJpegTileList }
procedure TsdJpegTileList.Add(Tile: TsdJpegTile);
begin
  Inc(_Count);
  SetLength(_Tiles, _Count);
  _Tiles[Pred(_Count)] := Tile;
end;

procedure TsdJpegTileList.Clear;
begin
  _Count := 0;
end;

function TsdJpegTileList.GetItems(Index: integer): TsdJpegTile;
begin
  If (Index>=0) And (Index<_Count) Then
    Result := _Tiles[Index]
  Else
    Result := Nil;
end;

function TsdJpegTileList.IndexByMcuIndex(AMcuIndex: integer): integer;
var
  Min, Max: integer;
begin
  // Find position for insert - binary method
  Min := 0;
  Max := Count;
  while Min < Max do begin
    Result := (Min + Max) div 2;
    case CompareInteger(Items[Result].FMcuIndex, AMcuIndex) of
    -1: Min := Result + 1;
     0: exit;
     1: Max := Result;
    end;
  end;
  Result := Min;
end;

{ TCustomSortedList }
procedure TCustomSortedList.Append(AItem: TERRAObject);
begin
  Insert(Count, AItem);
end;

function TCustomSortedList.Add(AItem: TERRAObject): integer;
begin
  if Sorted then
  begin

    Find(AItem, Result);
    Insert(Result, AItem);

  end else
  Begin
    Inc(_Count);
    SetLength(_List, _Count);
    _List[Pred(_Count)] := AItem;
  End;
end;

function TCustomSortedList.AddUnique(Item: TERRAObject): integer;
begin
  if Find(Item, Result) then
  begin
    Delete(Result);
  end;
  Insert(Result, Item);
end;

constructor TCustomSortedList.Create(AOwnsObjects: boolean);
begin
  FSorted := True;
end;

function TCustomSortedList.DoCompare(Item1, Item2: TERRAObject): integer;
begin
  Result := ComparePointer(Item1, Item2);
end;

function TCustomSortedList.Find(Item: TERRAObject; out Index: integer): boolean;
var
  AMin, AMax: integer;
begin
  Result := False;

  if Sorted then
  begin

    // Find position for insert - binary method
    Index := 0;
    AMin := 0;
    AMax := Count;
    while AMin < AMax do
    begin
      Index := (AMin + AMax) div 2;
      case DoCompare(_List[Index], Item) of
      -1: AMin := Index + 1;
       0: begin
            Result := True;
            exit;
          end;
       1: AMax := Index;
      end;
    end;
    Index := AMin;

  end else
  begin

    // If not a sorted list, then find it with the IndexOf() method
    Index := IndexOf(Item);
    if Index >= 0 then
    begin
      Result := True;
      exit;
    end;

    // Not found: set it to Count
    Index := Count;
  end;
end;

procedure TCustomSortedList.FindMultiple(Item: TERRAObject; out AIndex, ACount: integer);
var
  IdxStart: integer;
  IdxClose: integer;
begin
  if not Sorted then
    Engine.RaiseError(sListMustBeSorted);

  ACount := 0;

  // Find one
  if not Find(Item, AIndex) then
    exit;

  // Check upward from item
  IdxStart := AIndex;
  while (IdxStart > 0) and (DoCompare(_List[IdxStart - 1], Item) = 0) do
    dec(IdxStart);

  // Check downward from item
  IdxClose := AIndex;
  while (IdxClose < Count - 1) and (DoCompare(_List[IdxClose + 1], Item) = 0) do
    inc(IdxClose);

  // Result
  AIndex := IdxStart;
  ACount := IdxClose - IdxStart + 1;
end;

procedure TCustomSortedList.SetSorted(AValue: boolean);
begin
  if AValue <> FSorted then
  begin
    FSorted := AValue;
    if FSorted then
      Sort;
  end;
end;

procedure TCustomSortedList.Sort;
  //local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid, Temp: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid:= (Lo + Hi) div 2;
    repeat
      while DoCompare(_List[Lo], _List[Mid]) < 0 do
        Inc(Lo);
      while DoCompare(_List[Hi], _List[Mid]) > 0 do
        Dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        Temp := Lo;
        Lo := Hi;
        Hi := Temp;
        if Mid = Lo then
          Mid := Hi
        else
          if Mid = Hi then
            Mid := Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    if Hi > iLo then
      QuickSort(iLo, Hi);

    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;
// main
begin
  if Count > 1 then
  begin
    QuickSort(0, Count - 1);
  end;
  FSorted := True;
end;

(*procedure TCustomSortedList.Delete(Item: TERRAObject);
Var
  I:Integer;
Begin
  I := 0;

  While (I<_Count) Do
  if _List[i] = Item then
  Begin
    _List[i] := _list[Pred(_Count)];
    Dec(_Count);
    Exit;
  End Else
    Inc(I);
End;*)

procedure TCustomSortedList.Delete(Pos:Integer);
Var
  I:Integer;
Begin
  For I:=Pos To Pred(_Count) Do
    _List[I] := _List[I+1];

  Dec(_Count);
End;

procedure TCustomSortedList.Insert(Pos: Integer; AItem: TERRAObject);
Var
  I:Integer;
Begin
  Add(AItem);

  For I:=Pred(_Count) DownTo Pos Do
    _List[I] := _List[I-1];

  _List[Pos] := AItem;
End;

function TCustomSortedList.IndexOf(Item: TERRAObject): Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
  If (_List[I] = Item) Then
  Begin
    Result := I;
    Exit;
  End;
  Result := -1;
end;

function TCustomSortedList.Get(Index: Integer): TERRAObject;
begin
  If (Index>=0) And (Index<_Count) Then
    Result := _List[Index]
  Else
    Result := Nil;
end;

procedure TCustomSortedList.Clear;
begin
  _Count := 0;
end;

{ TSortedList }

function TSortedList.DoCompare(Item1, Item2: TERRAObject): integer;
begin
  if assigned(FOnCompare) then
    Result := FOnCompare(Item1, Item2, FCompareInfo)
  else if assigned(FCompareMethod) then
    Result := FCompareMethod(Item1, Item2, FCompareInfo)
  else
    Result := ComparePointer(Item1, Item2);
end;

{ TsdHuffmanCoder }

procedure TsdHuffmanCoder.GenerateCodeTable(ATable: TsdHuffmanTable);
var
  i, k, Idx, Len: integer;
  Code, Size: integer;
  MaxVal: integer;
begin
  // Generate a list of codes for the table (See Fig. C.2)
  Code := 0;
  MaxVal := 0;
  Size := ATable[0].L;
  k := 0;
  Len := ATable.Count;
  while k < Len do
  begin
    while (k < Len) and (ATable[k].L = Size) do
    begin
      ATable[k].Code := Code;
      if ATable[k].V > MaxVal then
        MaxVal := ATable[k].V;
      inc(Code);
      inc(k);
    end;
    Code := Code shl 1;
    inc(Size);
  end;

  SetLength(FCodes, MaxVal + 1); // 0..MaxVal
  for i := 0 to ATable.Count - 1 do
  begin
    Idx := ATable[i].V;
    FCodes[Idx].L := ATable[i].L;
    FCodes[Idx].V := ATable[i].V;
    FCodes[Idx].Code := ATable[i].Code;
  end;

end;

{ TsdHuffmanDecoder }

procedure TsdHuffmanDecoder.AddToLookupTable(Table, Code, Len, Value: integer);
var
  i, Iter, Mask: integer;
  Base: integer;
  Next: integer;
  Lookup: PsdHuffmanLookupTable;
begin
  Lookup := @FLookup[Table];
  if Len <= 8 then
  begin
    // Fill all the lsb bit entries with the same value
    Iter := 1 shl (8 - Len);
    Base := Code shl (8 - Len);
    for i := 0 to Iter - 1 do
    begin
      Lookup.Len  [Base + i] := Len;
      Lookup.Value[Base + i] := Value;
    end;
  end else
  begin
    // We need to follow a table or instantiate one
    Base := Code shr (Len - 8);
    Next := Lookup.Value[Base];
    if Next = 0 then
    begin
      // No followup table yet, create one
      inc(FLookupCount);
      if (length(FLookup) <= FLookupCount) then
      begin
        SetLength(FLookup, length(FLookup) * 2);
      end;

      // Next table, set its pointer
      Next := FLookupCount;
      FLookup[Table].Value[Base] := Next;
    end;
    // There is a follow up table, add
    Mask := 1 shl (Len - 8) - 1;
    AddToLookupTable(Next, Code and Mask, Len - 8, Value);
  end;
end;

procedure TsdHuffmanDecoder.GenerateLookupTables(Table: TsdHuffmanTable);
begin
  // Generate the code table first
  GenerateCodeTable(Table);
  // Start with clean 4 lookup tables
  SetLength(FLookup, 0);
  SetLength(FLookup, 4);
  FLookupCount := 0;
  // Default does nothing more
end;

{ Tsd8bitHuffmanDecoder }

procedure Tsd8bitHuffmanDecoder.GenerateLookupTables(Table: TsdHuffmanTable);
var
  i: integer;
begin
  inherited;
  for i := 0 to length(FCodes) - 1 do begin
    if FCodes[i].L > 0 then
      AddToLookupTable(0, FCodes[i].Code, FCodes[i].L, i);
  end;
end;

{ TsdHuffmanNode }
Procedure TsdHuffmanNode.Release;
begin
  ReleaseObject(FB0);
  ReleaseObject(FB1);
  inherited;
end;

{ TsdHuffmanNodeList }

function TsdHuffmanNodeList.DoCompare(Item1, Item2: TERRAObject): integer;
var
  L1, L2: TsdHuffmanNode;
begin
  // Sort by count, smallest first
  L1 := TsdHuffmanNode(Item1);
  L2 := TsdHuffmanNode(Item2);

  // Compare by bitcount first (smallest bitcount first)
  Result := CompareInteger(L1.BitCount, L2.BitCount);
  if Result = 0 then
  begin
    // Compare by frequency count (largest count first)
    Result := -CompareInteger(L1.Count, L2.Count);
  end;
end;

function TsdHuffmanNodeList.GetItems(Index: integer): TsdHuffmanNode;
begin
  Result := TsdHuffmanNode(Get(Index));
end;

{ Tsd8bitHuffmanEncoder }

constructor Tsd8bitHuffmanEncoder.Create;
begin
  inherited;
  // do not own objects
  FNodes := TsdHuffmanNodeList.Create(False);
end;

Procedure Tsd8bitHuffmanEncoder.Release;
begin
  ReleaseObject(FNodes);
  inherited;
end;

procedure Tsd8bitHuffmanEncoder.GenerateCodeTable(ATable: TsdHuffmanTable);
var
  i: integer;
begin
  if ATable.Count = 0 then
  begin
    // Uninitialized table: create just codes for histogramming
    SetLength(FCodes, 256);
    for i := 0 to 255 do
      FCodes[i].V := i;
    exit;
  end;
  inherited;
end;

function Tsd8bitHuffmanEncoder.GetHistogram: Psd8bitHuffmanHistogram;
begin
  Result := @FHistogram;
end;

procedure Tsd8bitHuffmanEncoder.OptimiseHuffmanFromHistogram(var Item: TsdDHTMarkerInfo);
// Create an optimized huffman table from the data gathered in the histogram by
// the dry-run
var
  i: integer;
  N, N0, N1, Top: TsdHuffmanNode;

  // Recursive procedure: add values with their bitcount to the nodelist
  procedure AddBranch(ABranch: TsdHuffmanNode; ABitCount: integer);
  begin
    // Branch B0
    if assigned(ABranch.B0.Code) then
    begin
      ABranch.B0.BitCount := ABitCount;
      FNodes.Add(ABranch.B0);
    end else
      AddBranch(ABranch.B0, ABitCount + 1);

    // Branch B1
    if assigned(ABranch.B1.Code) then
    begin
      ABranch.B1.BitCount := ABitCount;
      FNodes.Add(ABranch.B1);
    end else
      AddBranch(ABranch.B1, ABitCount + 1);
  end;

// main
begin
  // initialise the FNodes before clearing and adding!
  if not assigned(FNodes) then
  begin
    FNodes := TsdHuffmanNodeList.Create(False);
  end;

  // Start by adding nodes in sorted fashion
  FNodes.Clear;
  for i := 0 to length(FCodes) - 1 do
  begin
    if FHistogram[i] = 0 then
      continue;
    N := TsdHuffmanNode.Create;
    N.Code := @FCodes[i];
    N.Count := FHistogram[i];
    FNodes.Add(N);
  end;

  // Initialize huffman data
  SetLength(Item.BitValues, FNodes.Count);
  for i := 0 to 15 do
    Item.BitLengths[i] := 0;
  if FNodes.Count = 0 then
    exit;

  // Repeat combining nodes until there's only one
  while FNodes.Count >= 2 do
  begin
    // Two last nodes with smallest frequency count
    N0 := FNodes[FNodes.Count - 1];
    N1 := FNodes[FNodes.Count - 2];

    // Delete two last from list
    FNodes.Delete(FNodes.Count - 1);
    FNodes.Delete(FNodes.Count - 1);

    // New containing node
    N := TsdHuffmanNode.Create;
    N.B0 := N0;
    N.B1 := N1;
    N.Count := N0.Count + N1.Count;

    // Add new one to list (sorted)
    FNodes.Add(N);
  end;

  // Top item
  Top := FNodes[0];
  FNodes.Clear;

  // Start adding them again, now sorted by bitcount
  if assigned(Top.Code) then
  begin
    // If there is only one, we add it directly with bitcount 1
    Top.BitCount := 1;
    FNodes.Add(Top);
  end else
  begin
    // Recursive call on the tree
    AddBranch(Top, 1);
  end;

  // Since our table is compacted, and the jpeg spec says we must not have codes
  // with all ones, we will increase the bitcount of the last item
  N := FNodes[FNodes.Count - 1];
  N.BitCount := N.BitCount + 1;

  // Check maximum bit count; this should NOT exceed 16 bits for jpeg
  while FNodes[FNodes.Count - 1].BitCount > 16 do
  begin
    // Extract last two with largest bitcounts
    N0 := FNodes[FNodes.Count - 1];
    N1 := FNodes[FNodes.Count - 2];
    FNodes.Delete(FNodes.Count - 1);
    FNodes.Delete(FNodes.Count - 1);

    // Find item with at least 2 bits less
    i := FNodes.Count - 1;
    while FNodes[i].BitCount > N0.BitCount - 2 do
      dec(i);
    N := FNodes[i];
    FNodes.Delete(i);

    // Increment this leaf, decrement one of the other two, and set the other
    // to the same as this one. This preserves bitspace
    N.BitCount := N.BitCount + 1;
    N0.BitCount := N0.BitCount - 1;
    N1.BitCount := N.BitCount;

    // Add these again in a sorted way
    FNodes.Add(N);
    FNodes.Add(N0);
    FNodes.Add(N1);
  end;

  // We should now have a sorted list of codes by bitcount, and we can construct
  // the huffman table
  for i := 0 to FNodes.Count - 1 do
  begin
    N := FNodes[i];
    inc(Item.BitLengths[N.BitCount - 1]);
    Item.BitValues[i] := N.Code.V;
  end;
  FNodes.Clear;
  Top.Free;
end;

{ TsdDCBaselineHuffmanDecoder }
procedure TsdDCBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TsdMcuBlock; AReader: TsdBitReader);
var
  S, Code: smallint; // S = category
  Bits: word;
  Idx, Len: byte;
  Table: PsdHuffmanLookupTable;
begin
  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := AReader.ThisByte^;
  Table := @FLookup[0];
  Len := Table.Len[Idx];
  S := Table.Value[Idx];
  if Len = 0 then
  begin
    Idx := AReader.NextByte^;
    Table := @FLookup[S];
    Len := 8 + Table.Len[Idx];
    S := Table.Value[Idx];
  end;

  // We already have the code, but need to actually remove the bits from the stream
  AReader.RemoveBits(Len);
  {$IFDEF DETAILS}
  inc(FCountCodes, Len);
  {$ENDIF}

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  {$IFDEF DETAILS}
  inc(FCountBits, S);
  {$ENDIF}
  case S of
  0: Code := ABlock.PPred^;
  1:
    begin
      if AReader.GetBits(1) = 1 then
        Code := ABlock.PPred^ + 1
      else
        Code := ABlock.PPred^ - 1;
    end;
  else
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    if Bits < cExtendTest[S] then
      Code := ABlock.PPred^ + Bits + cExtendOffset[S]
    else
      Code := ABlock.PPred^ + Bits;
  end;//case

  // Update block
  ABlock.Values[0] := Code;

  // Update image component's predictor
  ABlock.PPred^ := Code;
end;

{ TsdACBaselineHuffmanDecoder }

procedure TsdACBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; AZigZag: PsdZigZagArray);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Bits, Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  Values := ABlock.Values;

  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);
    {$IFDEF DETAILS}
    inc(FCountCodes, Len);
    {$ENDIF}

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
    begin
      if R = 15 then
      begin
        // 16 sample runlength, no sample setting
        inc(k, 16);
        continue;
      end else
      begin
        // All other values except R = 0 are undefined, we take it as to
        // jump out for these too. R=0,S=0 means end of block
        break;
      end;
    end;

    // Increment range-coded index
    inc(k, R);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    {$IFDEF DETAILS}
    inc(FCountBits, S);
    {$ENDIF}
    kz := AZigZag[k];
    if kz > 0 then
    begin
      if S = 1 then
      begin
        // Optimized for S = 1 (very often)
        if Bits = 0 then
          Values[kz] := -1
        else
          Values[kz] := 1;
      end else
      begin
        // S > 1
        if Bits < cExtendTest[S] then
          Values[kz] := Bits + cExtendOffset[S]
        else
          Values[kz] := Bits;
      end;
    end;
    inc(k);

  // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

procedure TsdACBaselineHuffmanDecoder.DecodeMcuBlockSkip(AReader: TsdBitReader);
var
  k: integer; // Position in zigzag
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;

  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
    begin
      if R = 15 then
      begin
        // 16 sample runlength, no sample setting
        inc(k, 16);
        continue;
      end else
      begin
        // All other values except R = 0 are undefined, we take it as to
        // jump out for these too. R=0,S=0 means end of block
        break;
      end;
    end;

    // Increment range-coded index
    inc(k, R + 1);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    AReader.GetBits(S);

  // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

{ TsdDCBaselineHuffmanEncoder }

procedure TsdDCBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TsdMcuBlock;
  AWriter: TsdBitWriter);
var
  S, Diff: smallint; // S = category
begin
  Diff := ABlock.Values[0] - ABlock.PPred^;
  ABlock.PPred^ := ABlock.Values[0];

  // count the bits
  S := AWriter.CountBits(Diff);

  // Put S code  + extend
  AWriter.PutCodeExtend(@FCodes[S], Diff, S);
end;

{ TsdACBaselineHuffmanEncoder }

procedure TsdACBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TsdMcuBlock;
  AWriter: TsdBitWriter);
var
  k: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S, Diff: integer; // RS = range,category
begin
  Values := ABlock.Values;
  R := 0;
  k := 1;
  repeat
    Diff := Values[cJpegInverseZigZag8x8[k]];
    inc(k);
    if Diff = 0 then
    begin
      inc(R);
      continue;
    end;
    while R >= 16 do
    begin
      // Code an RS = $F0
      AWriter.PutCode(@FCodes[$F0]);
      dec(R, 16);
    end;
    // Code the value
    S := AWriter.CountBits(Diff);
    // RS value
    RS := R shl 4 + S;
    R := 0;
    AWriter.PutCodeExtend(@FCodes[RS], Diff, S);
  until k = 64;

  // if we have R > 0 this means we must code end of block
  if R > 0 then
    AWriter.PutCode(@FCodes[$00]);
end;

{ TsdDCProgressiveHuffmanDecoder }

procedure TsdDCProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader; ApproxLow: integer);
var
  S, Code: smallint; // S = category
  Bits: word;
  Idx, Len: byte;
  Table: PsdHuffmanLookupTable;
begin
  //DoDebugOut(Self, wsInfo, 'DC DecodeProgFirst');

  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := AReader.ThisByte^;
  Table := @FLookup[0];
  Len := Table.Len[Idx];
  S := Table.Value[Idx];
  if Len = 0 then
  begin
    Idx := AReader.NextByte^;
    Table := @FLookup[S];
    Len := 8 + Table.Len[Idx];
    S := Table.Value[Idx];
  end;

  // We already have the code, but need to actually remove the bits from the stream
  AReader.RemoveBits(Len);

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  Code := 0;
  if S > 0 then
  begin
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    if Bits < cExtendTest[S] then
      Code := Bits + cExtendOffset[S]
    else
      Code := Bits;
  end;
  inc(Code, ABlock.PPred^);

  // Update image component's predictor
  ABlock.PPred^ := Code;

  // Update block
  ABlock.Values[0] := Code shl ApproxLow;
end;

procedure TsdDCProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; ApproxLow: integer);
var
  Plus: integer;
  Value: Psmallint;
begin
  //DoDebugOut(Self, wsInfo, 'DC DecodeProgRefine');
  Plus := 1 shl ApproxLow;
  Value := @ABlock.Values[0];

  // Update block
  if AReader.GetBits(1) = 1 then
  begin
    if Value^ > 0 then
      inc(Value^, Plus)
    else
      dec(Value^, Plus);
  end;
end;

{ TsdACProgressiveHuffmanDecoder }

procedure TsdACProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte, NextByte: PByte;
begin
  //DoDebugOut(Self, wsInfo, 'AC DecodeProgFirst');

  // Part of EOB run? In that case, decrement and exit
  if EOBRun > 0 then
  begin
    dec(EOBRun);
    exit;
  end;

  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Check if we're at the end of the spectral band
  while k <= SSEnd do
  begin

    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S <> 0 then
    begin

      // Increment range-coded index
      inc(k, R);

      // Process the S code, it's an index into a category.
      // We use the EXTEND function, Figure F12
      R := AReader.GetBits(S);
      if R < cExtendTest[S] then
        S := R + cExtendOffset[S]
      else
        S := R;

      kz := cJpegInverseZigZag8x8[k];
      if kz > 0 then
        Values[kz] := S shl ApproxLow;

    end else
    begin

      if R = 15 then
      begin

        // 16 sample runlength, no sample setting
        inc(k, 15);

      end else
      begin

        // EOB run
        EOBRun := 1 shl R;
        if R > 0 then
        begin
          R := AReader.GetBits(R);
          inc(EOBRun, R);
        end;
        dec(EOBRun);
        break;

      end;
    end;
    inc(k);
  end;
end;

procedure TsdACProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer;
  Values: PsdCoefBlock;
  RS, R, S, Plus: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte, NextByte: PByte;
begin
  //DoDebugOut(Self, wsInfo, 'AC DecodeProgRefine');

  // Prepare some local variables for fast access
  Plus := 1 shl ApproxLow;
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Not part of EOB run?
  if EOBRun = 0 then
  begin

    while k <= SSEnd do
    begin
      // Get the RS code. Since its guaranteed to have <= 16 bits we can use
      // this two-step mechanism (without loop)
      Idx := ThisByte^;
      Len := Table1.Len[Idx];
      RS := Table1.Value[Idx];
      if Len = 0 then
      begin
        Idx := NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2.Len[Idx];
        RS := Table2.Value[Idx];
      end;

      // We already have the code, but need to actually remove the bits from the stream
      AReader.RemoveBits(Len);

      // Split range,category
      R := RS shr 4;
      S := RS and $0F;

      if (S = 0) and (R < 15) then
      begin
        // EOB run
        EOBRun := 1 shl R;
        if R <> 0 then
        begin
          R := AReader.GetBits(R);
          inc(EOBRun, R);
        end;
        break;
      end;

      if S <> 0 then
      begin
        case AReader.GetBits(1) of
        1: S :=  Plus;
        0: S := -Plus;
        end;
      end;

      // Fill values for remainder
      repeat
        kz := cJpegInverseZigZag8x8[k];
        if Values[kz] <> 0 then
        begin
          if AReader.GetBits(1) = 1 then
          begin
            if Values[kz] > 0 then
              inc(Values[kz], Plus)
            else
              dec(Values[kz], Plus);
          end;
        end else
        begin
          dec(R);
          if R < 0 then break;
        end;
        inc(k);
      until k > SSEnd;

      if k <= SSend then
      begin
        if S <> 0 then
        begin
          kz := cJpegInverseZigZag8x8[k];
          if kz > 0 then
            Values[kz] := S;
        end;
      end;

      // Increment range-coded index
      inc(k);

    end;//while
  end;// EOBRun = 0

  // Deal with EOBRun
  if EOBRun > 0 then
  begin

    while k <= SSEnd do
    begin
      kz := cJpegInverseZigZag8x8[k];
      if Values[kz] <> 0 then
      begin
        if AReader.GetBits(1) = 1 then
        begin
          if Values[kz] > 0 then
            inc(Values[kz], Plus)
          else
            dec(Values[kz], Plus);
        end;
      end;
      inc(k);
    end;

    // decrement the EOB run
    dec(EOBRun);
  end;
end;

{ TsdJpegDCT }

procedure TsdJpegDCT.BuildQuantTableFrom(ATable: TsdQuantizationTable);
// we must use the inverse zig-zag
var
  i: integer;
begin
  if (FMethod = dmAccurate) or (FMap.BlockStride < 64) then
  begin
    // Get the quantization values from the table (and undo zigzag)
    for i := 0 to 63 do
      FQuant[cJpegInverseZigZag8x8[i]] := ATable.FQuant[i];
    // Premultiply the quantization factors
    for i := 0 to 63 do
      // give correct bit precision
      FQuant[i] := FQuant[i] * cIAccConstScale;
  end else
  begin
    // Get the quantization values from the table (and undo zigzag)
    for i := 0 to 63 do
      FQuant[cJpegInverseZigZag8x8[i]] := ATable.FQuant[i];
    // Premultiply the quantization factors
    for i := 0 to 63 do
      // scales are with 14 bits of precision, we only want 9 so divide
      // by 5 bits of precision
      FQuant[i] := (FQuant[i] * cIFastQuantScales[i]) div (1 shl (14 - cIFastConstBits));
  end;
end;


{ TsdJpegIDCT }
// integer multiply with shift arithmetic right
function MultiplyA(A, B: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := (A * B) div cIAccConstScale;
end;

// Descale and range limit to byte domain. We shift right over
// 12 bits: 9 bits to remove precision, and 3 bits to get rid of the additional
// factor 8 introducted by the IDCT transform.
function RangeLimitA(A: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := A div (1 shl cIAccRangeBits) + cCenterSample;
  if Result < 0 then
    Result := 0
  else
    if Result > cMaxSample then
      Result := cMaxSample;
end;

// integer multiply with shift arithmetic right
function MultiplyF(A, B: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := (A * B) div cIFastConstScale;
end;

// Descale and range limit to byte domain. We shift right over
// 13 bits: 10 bits to remove precision, and 3 bits to get rid of the additional
// factor 8 introducted by the IDCT transform.
function RangeLimitF(A: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := A div (1 shl cIFastRangeBits) + 128;
  if Result < 0 then
    Result := 0
  else
    if Result > 255 then
      Result := 255;
end;


procedure InverseDCTIntFast8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: integer;
  tmp10, tmp11, tmp12, tmp13: integer;
  z5, z10, z11, z12, z13: integer;
begin
  QIdx := 0;
  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 8]; p2 := @Coef[16]; p3 := @Coef[24];
  p4 := @Coef[32]; p5 := @Coef[40]; p6 := @Coef[48]; p7 := @Coef[56];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 8]; w2 := @Wrksp[16]; w3 := @Wrksp[24];
  w4 := @Wrksp[32]; w5 := @Wrksp[40]; w6 := @Wrksp[48]; w7 := @Wrksp[56];
  for i := 0 to 7 do
  begin
    if (p1^ = 0) and (p2^ = 0) and (p3^ = 0) and (p4^ = 0) and
       (p5^ = 0) and (p6^ = 0) and (p7^ = 0) then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci; w2^ := dci; w3^ := dci;
      w4^ := dci; w5^ := dci; w6^ := dci; w7^ := dci;
    end else
    begin
      // Even part

      tmp0 := p0^ * Quant[QIdx     ];
      tmp1 := p2^ * Quant[QIdx + 16];
      tmp2 := p4^ * Quant[QIdx + 32];
      tmp3 := p6^ * Quant[QIdx + 48];

      tmp10 := tmp0 + tmp2;	// phase 3
      tmp11 := tmp0 - tmp2;

      tmp13 := tmp1 + tmp3;	// phases 5-3
      tmp12 := MultiplyF(tmp1 - tmp3, FIX_1_414213562F) - tmp13; // 2*c4

      tmp0 := tmp10 + tmp13;	// phase 2
      tmp3 := tmp10 - tmp13;
      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      // Odd part

      tmp4 := p1^ * Quant[QIdx +  8];
      tmp5 := p3^ * Quant[QIdx + 24];
      tmp6 := p5^ * Quant[QIdx + 40];
      tmp7 := p7^ * Quant[QIdx + 56];

      z13 := tmp6 + tmp5;		// phase 6
      z10 := tmp6 - tmp5;
      z11 := tmp4 + tmp7;
      z12 := tmp4 - tmp7;

      tmp7 := z11 + z13;		// phase 5
      tmp11 := MultiplyF(z11 - z13, FIX_1_414213562F); // 2*c4

      z5    := MultiplyF(z10 + z12, FIX_1_847759065F); // 2*c2
      tmp10 := MultiplyF(z12, FIX_1_082392200F) - z5; // 2*(c2-c6)
      tmp12 := MultiplyF(z10, - FIX_2_613125930F) + z5; // -2*(c2+c6)

      tmp6 := tmp12 - tmp7;	// phase 2
      tmp5 := tmp11 - tmp6;
      tmp4 := tmp10 + tmp5;

      w0^ := tmp0 + tmp7;
      w7^ := tmp0 - tmp7;
      w1^ := tmp1 + tmp6;
      w6^ := tmp1 - tmp6;
      w2^ := tmp2 + tmp5;
      w5^ := tmp2 - tmp5;
      w4^ := tmp3 + tmp4;
      w3^ := tmp3 - tmp4;

    end;
    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3); inc(p4); inc(p5); inc(p6); inc(p7);
    inc(w0); inc(w1); inc(w2); inc(w3); inc(w4); inc(w5); inc(w6); inc(w7);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  w4 := @Wrksp[4]; w5 := @Wrksp[5]; w6 := @Wrksp[6]; w7 := @Wrksp[7];
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  s4 := @Sample[4]; s5 := @Sample[5]; s6 := @Sample[6]; s7 := @Sample[7];
  for i := 0 to 7 do
  begin
    if (w1^ = 0) and (w2^ = 0) and (w3^ = 0) and (w4^ = 0) and
       (w5^ = 0) and (w6^ = 0) and (w7^ = 0) then
    begin
      dcs := RangeLimitF(w0^);
      s0^ := dcs; s1^ := dcs; s2^ := dcs; s3^ := dcs;
      s4^ := dcs; s5^ := dcs; s6^ := dcs; s7^ := dcs;
    end else
    begin

      // Even part

      tmp10 := w0^ + w4^;
      tmp11 := w0^ - w4^;

      tmp13 := w2^ + w6^;
      tmp12 := MultiplyF(w2^ - w6^, FIX_1_414213562F) - tmp13;

      tmp0 := tmp10 + tmp13;
      tmp3 := tmp10 - tmp13;
      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      // Odd part

      z13 := w5^ + w3^;
      z10 := w5^ - w3^;
      z11 := w1^ + w7^;
      z12 := w1^ - w7^;

      tmp7 := z11 + z13;		// phase 5
      tmp11 := MultiplyF(z11 - z13, FIX_1_414213562F); // 2*c4

      z5    := MultiplyF(z10 + z12, FIX_1_847759065F); // 2*c2
      tmp10 := MultiplyF(z12, FIX_1_082392200F) - z5; // 2*(c2-c6)
      tmp12 := MultiplyF(z10, - FIX_2_613125930F) + z5; // -2*(c2+c6)

      tmp6 := tmp12 - tmp7;	// phase 2
      tmp5 := tmp11 - tmp6;
      tmp4 := tmp10 + tmp5;

      // Final output stage: scale down by a factor of 8 and range-limit

      s0^ := RangeLimitF(tmp0 + tmp7);
      s7^ := RangeLimitF(tmp0 - tmp7);
      s1^ := RangeLimitF(tmp1 + tmp6);
      s6^ := RangeLimitF(tmp1 - tmp6);
      s2^ := RangeLimitF(tmp2 + tmp5);
      s5^ := RangeLimitF(tmp2 - tmp5);
      s4^ := RangeLimitF(tmp3 + tmp4);
      s3^ := RangeLimitF(tmp3 - tmp4);

    end;
    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;
end;

procedure InverseDCTIntAccurate4x4(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3: Psmallint;
  w0, w1, w2, w3: Pinteger;
  s0, s1, s2, s3: Pbyte;
  z1, z2, z3, z4, z5: integer;
  tmp0, tmp1, tmp2, tmp3, tmp10, tmp11, tmp12, tmp13: integer;
begin
  QIdx := 0;

  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 4]; p2 := @Coef[ 8]; p3 := @Coef[12];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 4]; w2 := @Wrksp[ 8]; w3 := @Wrksp[12];
  for i := 0 to 3 do
  begin
    if (p1^ = 0) and (p2^ = 0) and (p3^ = 0) then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci; w2^ := dci; w3^ := dci;
    end else
    begin
      // Even part:

      z2 := p2^ * Quant[QIdx + 2 * 8];

      z1 := MultiplyA(z2, FIX_0_541196100AI);
      tmp3 := z1 + MultiplyA(z2, FIX_0_765366865AI);

      z2 := p0^ * Quant[QIdx + 0 * 8];

      tmp10 := z2 + tmp3;
      tmp13 := z2 - tmp3;
      tmp11 := z2 + z1;
      tmp12 := z2 - z1;

      // Odd part:

      z3 := p3^ * Quant[QIdx + 3 * 8];
      z4 := p1^ * Quant[QIdx + 1 * 8];

      z5 := MultiplyA(z3 + z4, FIX_1_175875602AI);

      tmp2 := MultiplyA(z3, FIX_3_072711026AI);
      tmp3 := MultiplyA(z4, FIX_1_501321110AI);
      z1 := MultiplyA(z4, - FIX_0_899976223AI);
      z2 := MultiplyA(z3, - FIX_2_562915447AI);
      z3 := MultiplyA(z3, - FIX_1_961570560AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z3, z5);
      Inc(z4, z5);

      tmp0 := z1 + z3;
      tmp1 := z2 + z4;
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      w0^ := tmp10 + tmp3;
      w3^ := tmp11 - tmp2;
      w1^ := tmp12 + tmp1;
      w2^ := tmp13 - tmp0;

    end;
    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3);
    inc(w0); inc(w1); inc(w2); inc(w3);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  for i := 0 to 3 do
  begin
    if (w1^ = 0) and (w2^ = 0) and (w3^ = 0) then
    begin
      dcs := RangeLimitA(w0^);
      s0^ := dcs; s1^ := dcs; s2^ := dcs; s3^ := dcs;
    end else
    begin
      // Even part:

      z2 := w2^;

      z1 := MultiplyA(z2, FIX_0_541196100AI);
      tmp3 := z1 + MultiplyA(z2, FIX_0_765366865AI);

      tmp0 := w0^;

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp0 + z1;
      tmp12 := tmp0 - z1;

      // Odd part:

      tmp2 := w3^;
      tmp3 := w1^;

      z3 := tmp2;
      z4 := tmp3;
      z5 := MultiplyA(z3 + z4, FIX_1_175875602AI);

      tmp2 := MultiplyA(tmp2, FIX_3_072711026AI);
      tmp3 := MultiplyA(tmp3, FIX_1_501321110AI);
      z1 := MultiplyA(z4, - FIX_0_899976223AI);
      z2 := MultiplyA(z3, - FIX_2_562915447AI);
      z3 := MultiplyA(z3, - FIX_1_961570560AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z3, z5);
      Inc(z4, z5);

      tmp0 := z1 + z3;
      tmp1 := z2 + z4;
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimitA(tmp10 + tmp3);
      s3^ := RangeLimitA(tmp11 - tmp2);
      s1^ := RangeLimitA(tmp12 + tmp1);
      s2^ := RangeLimitA(tmp13 - tmp0);

    end;

    // Advance block pointers
    inc(s0, 4); inc(s1, 4); inc(s2, 4); inc(s3, 4);
    inc(w0, 4); inc(w1, 4); inc(w2, 4); inc(w3, 4);
  end;
end;

procedure InverseDCTIntAccurate2x2(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1: Psmallint;
  w0, w1: Pinteger;
  s0, s1: Pbyte;
  z1, z2, z4, z5: integer;
  tmp0, tmp3, tmp10: integer;
begin
  QIdx := 0;
  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 2];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 2];
  for i := 0 to 1 do
  begin
    if p1^ = 0 then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci;
    end else
    begin
      z2 := p0^ * Quant[QIdx + 0 * 8];

      z4 := p1^ * Quant[QIdx + 1 * 8];

      z5 := MultiplyA(z4, FIX_1_175875602AI);

      tmp3 := MultiplyA(z4, FIX_1_501321110AI);
      z1 := MultiplyA(z4, - FIX_0_899976223AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z4, z5);

      tmp0 := z1 + z5;
      Inc(tmp3, z1 + z4);

      w0^ := z2 + tmp3;
      w1^ := z2 - tmp0;

    end;
    // Advance block pointers
    inc(p0); inc(p1);
    inc(w0); inc(w1);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1];
  s0 := @Sample[0]; s1 := @Sample[1];
  for i := 0 to 1 do
  begin
    if w1^ = 0 then
    begin
      dcs := RangeLimitA(w0^);
      s0^ := dcs; s1^ := dcs;
    end else
    begin
      tmp10 := w0^;

      z4 := w1^;

      z5 := MultiplyA(z4, FIX_1_175875602AI);

      tmp3 := MultiplyA(z4, FIX_1_501321110AI);
      z1 := MultiplyA(z4, - FIX_0_899976223AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z4, z5);

      tmp0 := z1 + z5;
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimitA(tmp10 + tmp3);
      s1^ := RangeLimitA(tmp10 - tmp0);

    end;
    // Advance block pointers
    inc(s0, 2); inc(s1, 2);
    inc(w0, 2); inc(w1, 2);
  end;
end;

procedure InverseDCTIntAccurate1x1(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
begin
  // Just the DC value to process
  Sample[0] := RangeLimitA(Coef[0] * Quant[0]);
end;

procedure InverseDCTIntAccurate8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  z1, z2, z3, z4, z5: integer;
  tmp0, tmp1, tmp2, tmp3, tmp10, tmp11, tmp12, tmp13: integer;
begin
  QIdx := 0;
  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 8]; p2 := @Coef[16]; p3 := @Coef[24];
  p4 := @Coef[32]; p5 := @Coef[40]; p6 := @Coef[48]; p7 := @Coef[56];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 8]; w2 := @Wrksp[16]; w3 := @Wrksp[24];
  w4 := @Wrksp[32]; w5 := @Wrksp[40]; w6 := @Wrksp[48]; w7 := @Wrksp[56];
  for i := 0 to 7 do
  begin
    if (p1^ = 0) and (p2^ = 0) and (p3^ = 0) and (p4^ = 0) and
       (p5^ = 0) and (p6^ = 0) and (p7^ = 0) then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci; w2^ := dci; w3^ := dci;
      w4^ := dci; w5^ := dci; w6^ := dci; w7^ := dci;
    end else
    begin
      // Even part

      z2 := p2^ * Quant[QIdx + 2 * 8];
      z3 := p6^ * Quant[QIdx + 6 * 8];

      z1 := MultiplyA(z2 + z3, FIX_0_541196100AI);
      tmp2 := z1 + MultiplyA(z3, - FIX_1_847759065AI);
      tmp3 := z1 + MultiplyA(z2, FIX_0_765366865AI);

      z2 := p0^ * Quant[QIdx + 0 * 8];
      z3 := p4^ * Quant[QIdx + 4 * 8];

      tmp0 := (z2 + z3);
      tmp1 := (z2 - z3);

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp1 + tmp2;
      tmp12 := tmp1 - tmp2;

      // Odd part

      tmp0 := p7^ * Quant[QIdx + 7 * 8];
      tmp1 := p5^ * Quant[QIdx + 5 * 8];
      tmp2 := p3^ * Quant[QIdx + 3 * 8];
      tmp3 := p1^ * Quant[QIdx + 1 * 8];

      z1 := tmp0 + tmp3;
      z2 := tmp1 + tmp2;
      z3 := tmp0 + tmp2;
      z4 := tmp1 + tmp3;
      z5 := MultiplyA(z3 + z4, FIX_1_175875602AI);

      tmp0 := MultiplyA(tmp0, FIX_0_298631336AI);
      tmp1 := MultiplyA(tmp1, FIX_2_053119869AI);
      tmp2 := MultiplyA(tmp2, FIX_3_072711026AI);
      tmp3 := MultiplyA(tmp3, FIX_1_501321110AI);
      z1 := MultiplyA(z1, - FIX_0_899976223AI);
      z2 := MultiplyA(z2, - FIX_2_562915447AI);
      z3 := MultiplyA(z3, - FIX_1_961570560AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z3, z5);
      Inc(z4, z5);

      Inc(tmp0, z1 + z3);
      Inc(tmp1, z2 + z4);
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      w0^ := tmp10 + tmp3;
      w7^ := tmp10 - tmp3;
      w1^ := tmp11 + tmp2;
      w6^ := tmp11 - tmp2;
      w2^ := tmp12 + tmp1;
      w5^ := tmp12 - tmp1;
      w3^ := tmp13 + tmp0;
      w4^ := tmp13 - tmp0;

    end;
    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3); inc(p4); inc(p5); inc(p6); inc(p7);
    inc(w0); inc(w1); inc(w2); inc(w3); inc(w4); inc(w5); inc(w6); inc(w7);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  w4 := @Wrksp[4]; w5 := @Wrksp[5]; w6 := @Wrksp[6]; w7 := @Wrksp[7];
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  s4 := @Sample[4]; s5 := @Sample[5]; s6 := @Sample[6]; s7 := @Sample[7];
  for i := 0 to 7 do
  begin
    if (w1^ = 0) and (w2^ = 0) and (w3^ = 0) and (w4^ = 0) and
       (w5^ = 0) and (w6^ = 0) and (w7^ = 0) then
    begin
      dcs := RangeLimitA(w0^);
      s0^ := dcs; s1^ := dcs; s2^ := dcs; s3^ := dcs;
      s4^ := dcs; s5^ := dcs; s6^ := dcs; s7^ := dcs;
    end else
    begin

      // Even part:
      z2 := w2^;
      z3 := w6^;

      z1 := MultiplyA(z2 + z3, FIX_0_541196100AI);
      tmp2 := z1 + MultiplyA(z3, - FIX_1_847759065AI);
      tmp3 := z1 + MultiplyA(z2, FIX_0_765366865AI);

      tmp0 := w0^ + w4^;
      tmp1 := w0^ - w4^;

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp1 + tmp2;
      tmp12 := tmp1 - tmp2;

      // Odd part:
      tmp0 := w7^;
      tmp1 := w5^;
      tmp2 := w3^;
      tmp3 := w1^;

      z1 := tmp0 + tmp3;
      z2 := tmp1 + tmp2;
      z3 := tmp0 + tmp2;
      z4 := tmp1 + tmp3;
      z5 := MultiplyA(z3 + z4, FIX_1_175875602AI);

      tmp0 := MultiplyA(tmp0, FIX_0_298631336AI);
      tmp1 := MultiplyA(tmp1, FIX_2_053119869AI);
      tmp2 := MultiplyA(tmp2, FIX_3_072711026AI);
      tmp3 := MultiplyA(tmp3, FIX_1_501321110AI);
      z1 := MultiplyA(z1, - FIX_0_899976223AI);
      z2 := MultiplyA(z2, - FIX_2_562915447AI);
      z3 := MultiplyA(z3, - FIX_1_961570560AI);
      z4 := MultiplyA(z4, - FIX_0_390180644AI);

      Inc(z3, z5);
      Inc(z4, z5);

      Inc(tmp0, z1 + z3);
      Inc(tmp1, z2 + z4);
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimitA(tmp10 + tmp3);
      s7^ := RangeLimitA(tmp10 - tmp3);
      s1^ := RangeLimitA(tmp11 + tmp2);
      s6^ := RangeLimitA(tmp11 - tmp2);
      s2^ := RangeLimitA(tmp12 + tmp1);
      s5^ := RangeLimitA(tmp12 - tmp1);
      s3^ := RangeLimitA(tmp13 + tmp0);
      s4^ := RangeLimitA(tmp13 - tmp0);

    end;
    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;
end;


procedure TsdJpegIDCT.PerformIDCT;
var
  i, j: integer;
  PCoef: PsdCoefBlock;
  PSample: PsdSampleBlock;
  Work: TsdIntArray64;
  FIDctMethod: TIDCTMethod;
begin
  // Select method
  FIDctMethod := nil;
  case FMethod of
  dmFast:
    begin
      case FMap.BlockStride of
      64: FIDctMethod := InverseDCTIntFast8x8; // 8x8
      16: FIDctMethod := InverseDCTIntAccurate4x4; // 4x4
       4: FIDctMethod := InverseDCTIntAccurate2x2; // 2x2
       1: FIDctMethod := InverseDCTIntAccurate1x1; // 1x1
      end;
    end;
  dmAccurate:
    begin
      case FMap.BlockStride of
      64: FIDctMethod := InverseDCTIntAccurate8x8; // 8x8
      16: FIDctMethod := InverseDCTIntAccurate4x4; // 4x4
       4: FIDctMethod := InverseDCTIntAccurate2x2; // 2x2
       1: FIDctMethod := InverseDCTIntAccurate1x1; // 1x1
      end;
    end;
  end;

  if not assigned(FIDctMethod) then exit;
  for j := 0 to FMap.VertBlockCount - 1 do
    for i := 0 to FMap.HorzBlockCount - 1 do
    begin
      PCoef := FMap.GetCoefPointer(i, j);
      PSample := FMap.GetSamplePointer(i, j);
      FIDctMethod(PCoef^, PSample^, FQuant, Work);
    end;
end;



{ JPEGFormat }
Function JPEGFormat.Identify(Source: TERRAStream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Result := CompareFileHeader(ID, 'ÿØÿà');
End;

Function JPEGFormat.LoadFromStream(Target: TERRAObject; Source: TERRAStream): Boolean;
Var
  Image:TERRAImage;
  jpeg:TsdJpegImage;
Begin
  Result := False;
  Image := TERRAImage(Target);

  jpeg := TsdJpegImage.Create();
  jpeg.LoadFromStream(Source, Image);
  jpeg.LoadJpeg(jsFull, True);
  jpeg.Destroy();

  Result := True;
End;

Function JPEGFormat.SaveToStream(Target:TERRAObject; Dest:TERRAStream):Boolean;
Var
  Image:TERRAImage;
  jpeg:TsdJpegImage;
Begin
  Result := False;
  Image := TERRAImage(Target);

  jpeg := TsdJpegImage.Create();
  jpeg.SaveBitmapStripByStrip(Image);
  jpeg.SaveJpeg();
  jpeg.SaveToStream(Dest);
  jpeg.Destroy();

  Result := True;
End;

Initialization
  RegisterJpegMarkerClass(TsdJFIFMarker);
  RegisterJpegMarkerClass(TsdAVI1Marker);
  RegisterJpegMarkerClass(TsdEXIFMarker);
  RegisterJpegMarkerClass(TsdG3FAXMarker);
  RegisterJpegMarkerClass(TsdICCProfileMarker);
  RegisterJpegMarkerClass(TsdAdobeApp14Marker);

  Engine.Formats.Add(JPEGFormat.Create(TERRAImage, 'jpg'));
End.

