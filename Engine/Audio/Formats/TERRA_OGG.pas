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
 * TERRA_OGG
 * Implements a OGG loader/decoder
 ***********************************************************************************************************************
}

Unit TERRA_OGG;

{$I terra.inc}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_SoundStreamer, TERRA_Sound, TERRA_FileFormat, TERRA_Log;

Type
  OGGFormat = Class(TERRAFileFormat)
    Protected
      Function Identify(Source:TERRAStream):Boolean; Override;

    Public
      Function LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
  End;

Const
   // global configuration settings (e.g. set these in the project/makefile),
   // or just set them in this file at the top (although ideally the first few
   // should be visible when the header file is compiled too, although it's not
   // crucial)

   // STB_VORBIS_NO_PULLDATA_API
   //     does not compile the code for the non-pushdata APIs
   // {$DEFINE STB_VORBIS_NO_PULLDATA_API}

   // STB_VORBIS_NO_INTEGER_CONVERSION
   //     does not compile the code for converting audio sample data from
   //     float to integer (implied by STB_VORBIS_NO_PULLDATA_API)
   // {$DEFINE STB_VORBIS_NO_INTEGER_CONVERSION}

   // STB_VORBIS_NO_FAST_SCALED_FLOAT
   //      does not use a fast float-to-int trick to accelerate float-to-int on
   //      most platforms which requires endianness be defined correctly.
   //{$DEFINE STB_VORBIS_NO_FAST_SCALED_FLOAT}
   
   // STB_VORBIS_MAX_CHANNELS [number]
   //     globally define this to the maximum number of channels you need.
   //     The spec does not put a restriction on channels except that
   //     the count is stored in a byte, so 255 is the hard limit.
   //     Reducing this saves about 16 bytes per value, so using 16 saves
   //     (255-16)*16 or around 4KB. Plus anything other memory usage
   //     I forgot to account for. Can probably go as low as 8 (7.1 audio),
   //     6 (5.1 audio), or 2 (stereo only).
   STB_VORBIS_MAX_CHANNELS = 16;  // enough for anyone?

   // STB_VORBIS_PUSHDATA_CRC_COUNT [number]
   //     after a flush_pushdata(), stb_vorbis begins scanning for the
   //     next valid page, without backtracking. when it finds something
   //     that looks like a page, it streams through it and verifies its
   //     CRC32. Should that validation fail, it keeps scanning. But it's
   //     possible that _while_ streaming through to check the CRC32 of
   //     one candidate page, it sees another candidate page. This #define
   //     determines how many "overlapping" candidate pages it can search
   //     at once. Note that "real" pages are typically ~4KB to ~8KB, whereas
   //     garbage pages could be as big as 64KB, but probably average ~16KB.
   //     So don't hose ourselves by scanning an apparent 64KB page and
   //     missing a ton of real ones in the interim; so minimum of 2
   STB_VORBIS_PUSHDATA_CRC_COUNT = 4;
   
   // STB_VORBIS_FAST_HUFFMAN_LENGTH [number]
   //     sets the log size of the huffman-acceleration table.  Maximum
   //     supported value is 24. with larger numbers, more decodings are O(1),
   //     but the table size is larger so worse cache missing, so you'll have
   //     to probe (and try multiple ogg vorbis files) to find the sweet spot.
   STB_VORBIS_FAST_HUFFMAN_LENGTH = 10;

   // STB_VORBIS_FAST_BINARY_LENGTH [number]
   //     sets the log size of the binary-search acceleration table. this
   //     is used in similar fashion to the fast-huffman size to set initial
   //     parameters for the binary search

   // STB_VORBIS_FAST_HUFFMAN_INT
   //     The fast huffman tables are much more efficient if they can be
   //     stored as 16-bit results instead of 32-bit results. This restricts
   //     the codebooks to having only 65535 possible outcomes, though.
   //     (At least, accelerated by the huffman table.)
   {$IFNDEF STB_VORBIS_FAST_HUFFMAN_INT}
   {$DEFINE STB_VORBIS_FAST_HUFFMAN_SHORT}
   {$ENDIF}

   // STB_VORBIS_NO_HUFFMAN_BINARY_SEARCH
   //     If the 'fast huffman' search doesn't succeed, then stb_vorbis falls
   //     back on binary searching for the correct one. This requires storing
   //     extra tables with the huffman codes in sorted order. Defining this
   //     symbol trades off space for speed by forcing a linear search in the
   //     non-fast case, except for "sparse" codebooks.
   // {$DEFINE STB_VORBIS_NO_HUFFMAN_BINARY_SEARCH}

   // STB_VORBIS_DIVIDES_IN_RESIDUE
   //     stb_vorbis precomputes the result of the scalar residue decoding
   //     that would otherwise require a divide per chunk. you can trade off
   //     space for time by defining this symbol.
   // {$DEFINE STB_VORBIS_DIVIDES_IN_RESIDUE}

   // STB_VORBIS_DIVIDES_IN_CODEBOOK
   //     vorbis VQ codebooks can be encoded two ways: with every case explicitly
   //     stored, or with all elements being chosen from a small range of values,
   //     and all values possible in all elements. By default, stb_vorbis expands
   //     this latter kind out to look like the former kind for ease of decoding,
   //     because otherwise an integer divide-per-vector-element is required to
   //     unpack the index. If you define STB_VORBIS_DIVIDES_IN_CODEBOOK, you can
   //     trade off storage for speed.
   // {$DEFINE STB_VORBIS_DIVIDES_IN_CODEBOOK}

   // STB_VORBIS_CODEBOOK_SHORTS
   //     The vorbis file format encodes VQ codebook floats as ax+b where a and
   //     b are floating point per-codebook constants, and x is a 16-bit int.
   //     Normally, stb_vorbis decodes them to floats rather than leaving them
   //     as 16-bit ints and computing ax+b while decoding. This is a speed/space
   //     tradeoff; you can save space by defining this flag.
   {$IFNDEF STB_VORBIS_CODEBOOK_SHORTS}
   {$DEFINE STB_VORBIS_CODEBOOK_FLOATS}
   {$ENDIF}

   // STB_VORBIS_DIVIDE_TABLE
   //     this replaces small integer divides in the floor decode loop with
   //     table lookups. made less than 1% difference, so disabled by default.

   // STB_VORBIS_NO_INLINE_DECODE
   //     disables the inlining of the scalar codebook fast-huffman decode.
   //     might save a little codespace; useful for debugging
   // {$DEFINE STB_VORBIS_NO_INLINE_DECODE}

   // STB_VORBIS_NO_DEFER_FLOOR
   //     Normally we only decode the floor without synthesizing the actual
   //     full curve. We can instead synthesize the curve immediately. This
   //     requires more memory and is very likely slower, so I don't think
   //     you'd ever want to do it except for debugging.
   // {$DEFINE STB_VORBIS_NO_DEFER_FLOOR}

   {$IFDEF STB_VORBIS_NO_PULLDATA_API}
   {$DEFINE STB_VORBIS_NO_INTEGER_CONVERSION}
   {$ENDIF}

   {$IFDEF STB_VORBIS_NO_CRT}
   {$ENDIF}

   {$IFNDEF STB_VORBIS_NO_INTEGER_CONVERSION}
   {$IFNDEF STB_VORBIS_NO_FAST_SCALED_FLOAT}
   // only need endianness for fast-float-to-int, which we don't
   // use for pushdata
   {$IFNDEF STB_VORBIS_BIG_ENDIAN}
   {$UNDEF STB_VORBIS_ENDIAN}
   {$ELSE}
   {$DEFINE STB_VORBIS_ENDIAN}
   {$ENDIF}
   {$ENDIF}
   {$ENDIF}

   MAX_BLOCKSIZE_LOG =  13;   // from specification
   MAX_BLOCKSIZE = 1 shl MAX_BLOCKSIZE_LOG;

   // NOTE
   //
   // Some arrays below are tagged "//varies", which means it's actually
   // a variable-sized piece of data, but rather than malloc I assume it's
   // small enough it's better to just allocate it all together with the
   // main thing
   //
   // Most of the variables are specified with the smallest size I could pack
   // them into. It might give better performance to make them all full-sized
   // integers. It should be safe to freely rearrange the structures or change
   // the sizes larger--nothing relies on silently truncating etc., nor the
   // order of variables.

   FAST_HUFFMAN_TABLE_SIZE =  1 shl STB_VORBIS_FAST_HUFFMAN_LENGTH;
   FAST_HUFFMAN_TABLE_MASK = FAST_HUFFMAN_TABLE_SIZE - 1;
   
//   CRC32_POLY =  $04c11db7;   // from spec

   // code length assigned to a value with no huffman encoding
   NO_CODE = 255;

   PAGEFLAG_continued_packet = 1;
   PAGEFLAG_first_page       = 2;
   PAGEFLAG_last_page        = 4;

   ogg_page_header: array [0..4-1] of byte = ($4f, $67, $67, $53);
   EOP = -1;
   INVALID_BITS = -1;

   VORBIS_packet_id = 1;
   VORBIS_packet_comment = 3;
   VORBIS_packet_setup = 5;

   // the following table is block-copied from the specification
   inverse_db_table: array[0..256-1] of single =
   (
      1.0649863e-07, 1.1341951e-07, 1.2079015e-07, 1.2863978e-07,
      1.3699951e-07, 1.4590251e-07, 1.5538408e-07, 1.6548181e-07,
      1.7623575e-07, 1.8768855e-07, 1.9988561e-07, 2.1287530e-07,
      2.2670913e-07, 2.4144197e-07, 2.5713223e-07, 2.7384213e-07,
      2.9163793e-07, 3.1059021e-07, 3.3077411e-07, 3.5226968e-07,
      3.7516214e-07, 3.9954229e-07, 4.2550680e-07, 4.5315863e-07, 
      4.8260743e-07, 5.1396998e-07, 5.4737065e-07, 5.8294187e-07,
      6.2082472e-07, 6.6116941e-07, 7.0413592e-07, 7.4989464e-07, 
      7.9862701e-07, 8.5052630e-07, 9.0579828e-07, 9.6466216e-07,
      1.0273513e-06, 1.0941144e-06, 1.1652161e-06, 1.2409384e-06,
      1.3215816e-06, 1.4074654e-06, 1.4989305e-06, 1.5963394e-06, 
      1.7000785e-06, 1.8105592e-06, 1.9282195e-06, 2.0535261e-06,
      2.1869758e-06, 2.3290978e-06, 2.4804557e-06, 2.6416497e-06,
      2.8133190e-06, 2.9961443e-06, 3.1908506e-06, 3.3982101e-06,
      3.6190449e-06, 3.8542308e-06, 4.1047004e-06, 4.3714470e-06,
      4.6555282e-06, 4.9580707e-06, 5.2802740e-06, 5.6234160e-06, 
      5.9888572e-06, 6.3780469e-06, 6.7925283e-06, 7.2339451e-06, 
      7.7040476e-06, 8.2047000e-06, 8.7378876e-06, 9.3057248e-06, 
      9.9104632e-06, 1.0554501e-05, 1.1240392e-05, 1.1970856e-05, 
      1.2748789e-05, 1.3577278e-05, 1.4459606e-05, 1.5399272e-05, 
      1.6400004e-05, 1.7465768e-05, 1.8600792e-05, 1.9809576e-05, 
      2.1096914e-05, 2.2467911e-05, 2.3928002e-05, 2.5482978e-05, 
      2.7139006e-05, 2.8902651e-05, 3.0780908e-05, 3.2781225e-05, 
      3.4911534e-05, 3.7180282e-05, 3.9596466e-05, 4.2169667e-05, 
      4.4910090e-05, 4.7828601e-05, 5.0936773e-05, 5.4246931e-05, 
      5.7772202e-05, 6.1526565e-05, 6.5524908e-05, 6.9783085e-05, 
      7.4317983e-05, 7.9147585e-05, 8.4291040e-05, 8.9768747e-05, 
      9.5602426e-05, 0.00010181521, 0.00010843174, 0.00011547824,
      0.00012298267, 0.00013097477, 0.00013948625, 0.00014855085,
      0.00015820453, 0.00016848555, 0.00017943469, 0.00019109536, 
      0.00020351382, 0.00021673929, 0.00023082423, 0.00024582449, 
      0.00026179955, 0.00027881276, 0.00029693158, 0.00031622787, 
      0.00033677814, 0.00035866388, 0.00038197188, 0.00040679456, 
      0.00043323036, 0.00046138411, 0.00049136745, 0.00052329927, 
      0.00055730621, 0.00059352311, 0.00063209358, 0.00067317058,
      0.00071691700, 0.00076350630, 0.00081312324, 0.00086596457, 
      0.00092223983, 0.00098217216, 0.0010459992,  0.0011139742,
      0.0011863665,  0.0012634633,  0.0013455702,  0.0014330129, 
      0.0015261382,  0.0016253153,  0.0017309374,  0.0018434235, 
      0.0019632195,  0.0020908006,  0.0022266726,  0.0023713743, 
      0.0025254795,  0.0026895994,  0.0028643847,  0.0030505286,
      0.0032487691,  0.0034598925,  0.0036847358,  0.0039241906, 
      0.0041792066,  0.0044507950,  0.0047400328,  0.0050480668, 
      0.0053761186,  0.0057254891,  0.0060975636,  0.0064938176, 
      0.0069158225,  0.0073652516,  0.0078438871,  0.0083536271, 
      0.0088964928,  0.009474637,   0.010090352,   0.010746080, 
      0.011444421,   0.012188144,   0.012980198,   0.013823725, 
      0.014722068,   0.015678791,   0.016697687,   0.017782797, 
      0.018938423,   0.020169149,   0.021479854,   0.022875735,
      0.024362330,   0.025945531,   0.027631618,   0.029427276, 
      0.031339626,   0.033376252,   0.035545228,   0.037855157, 
      0.040315199,   0.042935108,   0.045725273,   0.048696758, 
      0.051861348,   0.055231591,   0.058820850,   0.062643361, 
      0.066714279,   0.071049749,   0.075666962,   0.080584227, 
      0.085821044,   0.091398179,   0.097337747,   0.10366330, 
      0.11039993,    0.11757434,    0.12521498,    0.13335215,
      0.14201813,    0.15124727,    0.16107617,    0.17154380,
      0.18269168,    0.19456402,    0.20720788,    0.22067342, 
      0.23501402,    0.25028656,    0.26655159,    0.28387361, 
      0.30232132,    0.32196786,    0.34289114,    0.36517414, 
      0.38890521,    0.41417847,    0.44109412,    0.46975890, 
      0.50028648,    0.53279791,    0.56742212,    0.60429640, 
      0.64356699,    0.68538959,    0.72993007,    0.77736504,
      0.82788260,    0.88168307,    0.9389798,     1.0
   );

{$IFDEF STB_VORBIS_DIVIDE_TABLE}
   DIVTAB_NUMER = 32;
   DIVTAB_DENOM = 64;
{$ENDIF}

   SAMPLE_unknown = $ffffffff;

   PLAYBACK_MONO  = 1;
   PLAYBACK_LEFT  = 2;
   PLAYBACK_RIGHT = 4;

   L = (PLAYBACK_LEFT  or PLAYBACK_MONO);
   C = (PLAYBACK_LEFT  or PLAYBACK_RIGHT or PLAYBACK_MONO);
   R = (PLAYBACK_RIGHT or PLAYBACK_MONO);

channel_position: array [0..6,0..5] of Shortint =
(
   ( 0, 0, 0, 0, 0, 0 ),
   ( C, 0, 0, 0, 0, 0 ),
   ( L, R, 0, 0, 0, 0 ),
   ( L, C, R, 0, 0, 0 ),
   ( L, R, L, R, 0, 0 ),
   ( L, C, R, L, R, 0 ),
   ( L, C, R, L, R, C )
);

channel_selector: array [0..2,0..1] of integer = ( (0,0), (PLAYBACK_MONO,0), (PLAYBACK_LEFT, PLAYBACK_RIGHT) );

type
   ////////   ERROR CODES
   STBVorbisError =      (
      VORBIS__no_error=0,
      VORBIS_need_more_data=1,             // not a real error

      VORBIS_invalid_api_mixing,           // can't mix API modes
      VORBIS_outofmem,                     // not enough memory
      VORBIS_feature_not_supported,        // uses floor 0
      VORBIS_too_many_channels,            // STB_VORBIS_MAX_CHANNELS is too small
      VORBIS_file_open_failure,            // fopen() failed
      VORBIS_seek_without_length,          // can't seek in unknown-length file

      VORBIS_unexpected_eof=10,            // file is truncated?
      VORBIS_seek_invalid,                 // seek past EOF

      // decoding errors (corrupt/invalid stream) -- you probably
      // don't care about the exact details of these

      // vorbis errors:
      VORBIS_invalid_setup=20,
      VORBIS_invalid_stream,

      // ogg errors:
      VORBIS_missing_capture_pattern=30,
      VORBIS_invalid_stream_structure_version,
      VORBIS_continued_packet_flag_invalid,
      VORBIS_incorrect_stream_serial_number,
      VORBIS_invalid_first_page,
      VORBIS_bad_packet_type,
      VORBIS_cant_find_last_page,
      VORBIS_seek_failed );
   Integer = Longint;
{$IFDEF FPC}
   uint64 = QWord;
   //int64 = int64;
{$ENDIF}
   pSmallInt = ^SmallInt;
   pInteger = ^Integer;
{$IFDEF FPC}
   puint64 = ^QWord;
{$ENDIF}
   pint64 = ^int64;
   TOutput = array [0..STB_VORBIS_MAX_CHANNELS-1] of pSingle;
   POutput = ^TOutput;

   FArray = Array of Single;
   PFArray = ^FArray;

   BArray = Array of Byte;
   PBArray = ^BArray;

   FFArray = Array of Array of Single;
   PFFArray = ^FFArray;

   FPArray = Array [0..STB_VORBIS_MAX_CHANNELS-1] of PSingle;
   PFPArray = ^FPArray;

   FFFArray = Array of Array of Array of Single;
   PFFFArray = ^FFFArray;

   SArray = Array of shortint;
   PSArray = ^SArray;

   SSArray = Array of Array of ShortInt;
   PSSArray = ^SSArray;

//   BBArray = array of array of Byte;
   BBArray = array of BArray;
   PBBArray = ^BBArray;

   WWArray = array of array of Word;
   PWWArrary = ^WWArray;

   IArray = array of SmallInt;
   PIArrary = ^IArray;

   IIArray = array of array of SmallInt;
   PIIArrary = ^IIArray;

   BPArray = Array of array of PByte;
   PBPArray = ^BPArray;

   BBPArray = Array of array of array of  PByte;
   PBBPArray = ^BBPArray;

//   BBBArray = Array of array of array of  Byte;
   BBBArray = array of BBArray;
   PBBBArray = ^BBBArray;

   stb_vorbis_alloc = record
      alloc_buffer: PAnsiChar;
      alloc_buffer_length_in_bytes: Integer;
   end;
   pstb_vorbis_alloc = ^stb_vorbis_alloc;

   stb_vorbis_info = record
      sample_rate: Word;
      channels: Integer;

      setup_memory_required: Cardinal;
      setup_temp_memory_required: Cardinal;
      temp_memory_required: Cardinal;

      max_frame_size: Integer;
   end;
   pstb_vorbis_info = ^stb_vorbis_info;
   
   {$IFDEF STB_VORBIS_CODEBOOK_FLOATS}
   codetype = Single;
   {$ELSE}
   codetype = Word;
   {$ENDIF}
   pcodetype = ^codetype;
      
   Codebook = record
      dimensions, entries: Integer;
      codeword_lengths: BArray;
      minimum_value: Single;
      delta_value: Single;
      value_bits: Byte;
      lookup_type: Byte;
      sequence_p: Byte;
      sparse: Byte;
      lookup_values: Cardinal;
      multiplicands: Array of codetype;
      codewords: array of Cardinal;
      {$IFDEF STB_VORBIS_FAST_HUFFMAN_SHORT}
      fast_huffman: array [0..FAST_HUFFMAN_TABLE_SIZE-1] of SmallInt;
      {$ELSE}
      fast_huffman: array [0..FAST_HUFFMAN_TABLE_SIZE-1] of Integer;
      {$ENDIF}
      sorted_codewords: array of Cardinal;
      sorted_values: array of Integer;
      sorted_entries: Integer;
   end;
   PCodebook = ^Codebook;
   
   TFloor0 = record
      order: Byte;
      rate: Word;
      bark_map_size: Word;
      amplitude_bits: Byte;
      amplitude_offset: Byte;
      number_of_books: Byte;
      book_list: array [0..16-1] of Byte; // varies
   end;
   PFloor0 = ^TFloor0;
   
   TFloor1 = record
      partitions: Byte;
      partition_class_list: array [0..32-1] of Byte; // varies
      class_dimensions: array [0..16-1] of Byte; // varies
      class_subclasses: array [0..16-1] of Byte; // varies
      class_masterbooks: array [0..16-1] of Byte; // varies
      subclass_books: array [0..16-1,0..8-1] of SmallInt; // varies
      Xlist: array [0..31*8+2-1] of Word; // varies
      sorted_order: array [0..31*8+2-1] of Byte;
      neighbors: array [0..31*8+2-1,0..2-1] of Byte;
      floor1_multiplier: Byte;
      rangebits: Byte;
      values: Integer;
   end;
   PFloor1 = ^TFloor1;
      
   TFloor = packed record
      case Integer of
         0:(floor0: TFloor0);
         1:(floor1: TFloor1);
      end;
   PFloor=^TFloor;

   ResidueBooks = Array [0..8-1] of SmallInt;
   PResidueBooks =^ResidueBooks;
   Residue = record
      _begin, _end: Cardinal;
      part_size: Cardinal;
      classifications: Byte;
      classbook: Byte;
      classdata: BBArray;
      residue_books: Array  of ResidueBooks;
   end;
   PResidue = ^Residue;

   MappingChannel = record
      magnitude: Byte;
      angle: Byte;
      mux: Byte;
   end;
   PMappingChannel = ^MappingChannel;

   Mapping = record
      coupling_steps: Word;
      chan: Array of MappingChannel;
      submaps: Byte;
      submap_floor: array [0..15-1] of Byte; // varies
      submap_residue: array [0..15-1] of Byte; // varies
   end;
   PMapping = ^Mapping;

   TMode = record
      blockflag: Byte;
      mapping: Byte;
      windowtype: Word;
      transformtype: Word;
   end;
   PMode = ^TMode;

   CRCscan = record
      goal_crc: Cardinal;    // expected crc if match
      bytes_left: Integer;  // bytes left in packet
      crc_so_far: Cardinal;  // running crc
      bytes_done: Integer;  // bytes processed in _current_ chunk
      sample_loc: Cardinal;  // granule pos encoded in page
   end;
   PCRCscan = ^CRCscan;

   ProbedPage = record
      page_start, page_end: Cardinal;
      after_previous_page_start: Cardinal;
      first_decoded_sample: Cardinal;
      last_decoded_sample: Cardinal;
   end;
   PProbedPage =^ProbedPage;

   ByteFile_Depricated = file of Byte;
   TFileHandle = Integer;

   stb_vorbis = record
      // user-accessible info
      sample_rate: Cardinal;
      channels: Integer;

      setup_memory_required: Cardinal;
      temp_memory_required: Cardinal;
      setup_temp_memory_required: Cardinal;

      // input config
      stream: pByte;
      stream_start: pByte;
      stream_end: pByte;

      stream_len: Cardinal;

      push_mode: Boolean;

      first_audio_page_offset: Cardinal;

      p_first, p_last: ProbedPage;

      // memory management
      alloc: stb_vorbis_alloc;
      setup_offset: Cardinal;
      temp_offset: Cardinal;

      // run-time results
      eof: Boolean;
      error: STBVorbisError;

      // user-useful data
      
      // header info
      blocksize: array [0..2-1] of Integer;
      blocksize_0, blocksize_1: Integer;
      codebook_count: Integer;
      codebooks:array of codebook;
      floor_count: Integer;
      floor_types: array [0..64-1] of Word; // varies
      floor_config: array of TFloor;
      residue_count: Integer;
      residue_types: array [0..64-1] of Word; // varies
      residue_config: array of Residue;
      mapping_count: Integer;
      mapping: array of Mapping;
      mode_count: Integer;
      mode_config: array [0..64-1] of TMode;  // varies

      total_samples: Cardinal;

      // decode buffer
      channel_buffers: TOutput;
      outputs: TOutput;

      previous_window: TOutput;
      previous_length: Integer;

      {$IFNDEF STB_VORBIS_NO_DEFER_FLOOR}
      finalY: array [0..STB_VORBIS_MAX_CHANNELS-1] of array of SmallInt;
      {$ELSE}
      floor_buffers: TOutput;
      {$ENDIF}

      current_loc: Cardinal; // sample location of next frame to decode
      current_loc_valid: Boolean;

      // per-blocksize precomputed data

      // twiddle factors
      A,B,C,window: array [0..2-1] of array of Single;
      bit_reverse: array [0..2-1] of array of Word;

      // current page/packet/segment streaming info
      serial: Cardinal; // stream serial number for verification
      last_page: Integer;
      segment_count: Integer;
      segments:array[0..255-1] of Byte;
      page_flag: Byte;
      bytes_in_seg: Byte;
      first_decode: Boolean;
      next_seg: Integer;
      last_seg: Boolean;  // flag that we're on the last segment
      last_seg_which: Integer; // what was the segment number of the last seg?
      acc: Cardinal;
      valid_bits: Integer;
      packet_bytes: Integer;
      end_seg_with_known_loc: Integer;
      known_loc_for_packet: Cardinal;
      discard_samples_deferred: Integer;
      samples_output: Cardinal;

      // push mode scanning
      page_crc_tests: Integer; // only in push_mode: number of tests active; -1 if not searching

      scan: array [0..STB_VORBIS_PUSHDATA_CRC_COUNT-1] of CRCscan;

      // sample-access
      channel_buffer_start: Integer;
      channel_buffer_end: Integer;
   end;
   pstb_vorbis = stb_vorbis;
   vorb = stb_vorbis;
   pvorb = ^vorb;

   // this has been repurposed so y is now the original index instead of y
   Point = record
      x,y: Word;
   end;
   PPoint = ^Point;

{$IFDEF STB_VORBIS_DIVIDE_TABLE}
   integer_divide_table array [DIVTAB_NUMER][DIVTAB_DENOM] of ShortInt; // 2KB
{$ENDIF}

{$IFNDEF STB_VORBIS_NO_DEFER_FLOOR}
   YTYPE = SmallInt ;
{$ELSE}
   YTYPE = int;
{$ENDIF}
   PYTYPE = ^YTYPE;
   
   YArray = Array of Ytype;
   PYArray = ^YArray;


   // get general information about the file
   function stb_vorbis_get_info(f:pvorb):stb_vorbis_info;
   // get the last error detected (clears it, too)
   function stb_vorbis_get_error(f:pvorb):STBVorbisError;
   // this function returns the offset (in samples) from the beginning of the
   // file that will be returned by the next decode, if it is known, or -1
   // otherwise. after a flush_pushdata() call, this may take a while before
   // it becomes valid again.
   // NOT WORKING YET after a seek with PULLDATA API
   function stb_vorbis_get_sample_offset(f:pvorb):integer;
   // returns the current seek point within the file, or offset from the beginning
   // of the memory buffer. In pushdata mode it returns 0.
   function  stb_vorbis_get_file_offset(f:pvorb):Cardinal;
   // close an ogg vorbis file and free all memory in use
   procedure stb_vorbis_close(f:pvorb);

   ///////////   PUSHDATA API
   // this API allows you to get blocks of data from any source and hand
   // them to stb_vorbis. you have to buffer them; stb_vorbis will tell
   // you how much it used, and you have to give it the rest next time;
   // and stb_vorbis may not have enough data to work with and you will
   // need to give it the same data again PLUS more. Note that the Vorbis
   // specification does not bound the size of an individual frame.


   function stb_vorbis_open_pushdata(
//            data: array of Byte;
            data: pByte;
            data_len:integer;            // the memory available for decoding
            var data_used:integer;       // only defined if result is not NULL
            var _error:STBVorbisError;
            alloc:pstb_vorbis_alloc):pvorb;
   // create a vorbis decoder by passing in the initial data block containing
   //    the ogg&vorbis headers (you don't need to do parse them, just provide
   //    the first N bytes of the file--you're told if it's not enough, see below)
   // on success, returns an stb_vorbis *, does not set error, returns the amount of
   //    data parsed/consumed on this call in *datablock_memory_consumed_in_bytes;
   // on failure, returns NULL on error and sets *error, does not change *datablock_memory_consumed
   // if returns NULL and *error is VORBIS_need_more_data, then the input block was
   //       incomplete and you need to pass in a larger block from the start of the file

   function stb_vorbis_decode_frame_pushdata(
            f:pvorb;                 // the file we're decoding
            data:pByte;
            //data:array of Byte; 
            data_len:integer;        // the memory available for decoding
            var channels:integer;    // place to write number of float * buffers
            output:POutput;      // place to write float ** array of float * buffers
            var samples:integer      // place to write number of output samples
        ):integer;
   // decode a frame of audio sample data if possible from the passed-in data block
   //
   // return value: number of bytes we used from datablock
   // possible cases:
   //     0 bytes used, 0 samples output (need more data)
   //     N bytes used, 0 samples output (resynching the stream, keep going)
   //     N bytes used, M samples output (one frame of data)
   // note that after opening a file, you will ALWAYS get one N-bytes,0-sample
   // frame, because Vorbis always "discards" the first frame.
   //
   // Note that on resynch, stb_vorbis will rarely consume all of the buffer,
   // instead only datablock_length_in_bytes-3 or less. This is because it wants
   // to avoid missing parts of a page header if they cross a datablock boundary,
   // without writing state-machiney code to record a partial detection.
   //
   // The number of channels returned are stored in *channels (which can be
   // NULL--it is always the same as the number of channels reported by
   // get_info). *output will contain an array of float* buffers, one per
   // channel. In other words, (*output)[0][0] contains the first sample from
   // the first channel, and (*output)[1][0] contains the first sample from
   // the second channel.

   procedure stb_vorbis_flush_pushdata(f:pvorb);
   // inform stb_vorbis that your next datablock will not be contiguous with
   // previous ones (e.g. you've seeked in the data); future attempts to decode
   // frames will cause stb_vorbis to resynchronize (as noted above), and
   // once it sees a valid Ogg page (typically 4-8KB, as large as 64KB), it
   // will begin decoding the _next_ frame.
   //
   // if you want to seek using pushdata, you need to seek in your file, then
   // call stb_vorbis_flush_pushdata(), then start calling decoding, then once
   // decoding is returning you data, call stb_vorbis_get_sample_offset, and
   // if you don't like the result, seek your file again and repeat.

   //////////   PULLING INPUT API
   // This API assumes stb_vorbis is allowed to pull data from a source--
   // either a block of memory containing the _entire_ vorbis stream, or a
   // FILE * that you or it create, or possibly some other reading mechanism
   // if you go modify the source to replace the FILE * case with some kind
   // of callback to your code. (But if you don't support seeking, you may
   // just want to go ahead and use pushdata.)


   function stb_vorbis_decode_memory(mem:pByte; len:integer; var channels, SampleRate:integer; var output:pSmallInt):Integer;
   // decode an entire file and output the data interleaved into a malloc()ed
   // buffer stored in *output. The return value is the number of samples
   // decoded, or -1 if the file could not be opened or was not an ogg vorbis file.
   // When you're done with it, just free() the pointer returned in *output.

   function stb_vorbis_open_memory(data: pByte; len: Cardinal; var _error: STBVorbisError; alloc: pstb_vorbis_alloc):pvorb;
   // create an ogg vorbis decoder from an ogg vorbis stream in memory (note
   // this must be the entire stream!). on failure, returns NULL and sets *error

   function stb_vorbis_seek_frame(f:pvorb; sample_number:Cardinal):Boolean;
   function stb_vorbis_seek(f:pvorb; sample_number:Cardinal):Boolean;
   // NOT WORKING YET
   // these functions seek in the Vorbis file to (approximately) 'sample_number'.
   // after calling seek_frame(), the next call to get_frame_*() will include
   // the specified sample. after calling stb_vorbis_seek(), the next call to
   // stb_vorbis_get_samples_* will start with the specified sample. If you
   // do not need to seek to EXACTLY the target sample when using get_samples_*,
   // you can also use seek_frame().

   procedure stb_vorbis_seek_start(f:pvorb);
   // this function is equivalent to stb_vorbis_seek(f,0), but it
   // actually works

   function stb_vorbis_stream_length_in_samples(f:pvorb):Cardinal;
   function stb_vorbis_stream_length_in_seconds(f:pvorb):Single;
   // these functions return the total length of the vorbis stream

   function stb_vorbis_get_frame_float(f:pvorb; var channels:Integer; output:POutput):Integer;
   // decode the next frame and return the number of samples. the number of
   // channels returned are stored in *channels (which can be NULL--it is always
   // the same as the number of channels reported by get_info). *output will
   // contain an array of float* buffers, one per channel. These outputs will
   // be overwritten on the next call to stb_vorbis_get_frame_*.
   //
   // You generally should not intermix calls to stb_vorbis_get_frame_*()
   // and stb_vorbis_get_samples_*(), since the latter calls the former.

   {$IFNDEF STB_VORBIS_NO_INTEGER_CONVERSION}
   function stb_vorbis_get_frame_short_interleaved(f:pvorb; num_c:integer; buffer:pSmallInt; num_shorts:integer):Cardinal;
   function stb_vorbis_get_frame_short(f:pvorb; num_c:integer;  buffer:pSmallInt; num_samples:integer):Cardinal;
   {$ENDIF}
   // decode the next frame and return the number of samples per channel. the
   // data is coerced to the number of channels you request according to the
   // channel coercion rules (see below). You must pass in the size of your
   // buffer(s) so that stb_vorbis will not overwrite the end of the buffer.
   // The maximum buffer size needed can be gotten from get_info(); however,
   // the Vorbis I specification implies an absolute maximum of 4096 samples
   // per channel. Note that for interleaved data, you pass in the number of
   // shorts (the size of your array), but the return value is the number of
   // samples per channel, not the total number of samples.

   // Channel coercion rules:
   //    Let M be the number of channels requested, and N the number of channels present,
   //    and Cn be the nth channel; let stereo L be the sum of all L and center channels,
   //    and stereo R be the sum of all R and center channels (channel assignment from the
   //    vorbis spec).
   //        M    N       output
   //        1    k      sum(Ck) for all k
   //        2    *      stereo L, stereo R
   //        k    l      k > l, the first l channels, then 0s
   //        k    l      k <= l, the first k channels
   //    Note that this is not _good_ surround etc. mixing at all! It's just so
   //    you get something useful.

   //function stb_vorbis_get_samples_float(f:pvorb; channels:integer; buffer:POutput; num_samples:integer):integer;
   // gets num_samples samples, not necessarily on a frame boundary--this requires
   // buffering so you have to supply the buffers. DOES NOT APPLY THE COERCION RULES.
   // Returns the number of samples stored per channel; it may be less than requested
   // at the end of the file. If there are no more samples in the file, returns 0.

   {$IFNDEF STB_VORBIS_NO_INTEGER_CONVERSION}
   function stb_vorbis_get_samples_short_interleaved(f:pvorb; channels:integer;
            buffer:array of SmallInt; num_shorts:integer):integer;
   function stb_vorbis_get_samples_short(f:pvorb; channels:integer; buffer:array of SmallInt; len:integer):integer;
   {$ENDIF}
   // gets num_samples samples, not necessarily on a frame boundary--this requires
   // buffering so you have to supply the buffers. Applies the coercion rules above
   // to produce 'channels' channels. Returns the number of samples stored per channel;
   // it may be less than requested at the end of the file. If there are no more
   // samples in the file, returns 0.


Implementation
Uses TERRA_FileUtils, TERRA_Error, TERRA_CRC32, TERRA_Math, TERRA_EngineManager, TERRA_AudioBuffer, TERRA_AudioMixer, Sysutils;

Var
{$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
   part_classdata: BBBArray;
{$ELSE}
   classifications:array of array of int;
{$ENDIF}

function IncPointer(p:PShortInt; i:integer):PShortInt; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PByte; i:integer):PByte; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PSmallInt; i:integer):PSmallInt; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PYTYPE; i:integer):PYTYPE; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PWord; i:integer):PWord; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PInteger; i:integer):PInteger; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PSingle; i:integer):PSingle; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function IncPointer(p:PFPArray; i:integer):PFPArray; overload;
begin
  Result := p;
  Inc(Result, i);
end;

function CompareMemRange(p1,p2:Pointer; Len:Integer):Integer;
begin
  if CompareMem(p1,p2,Len) then Result := 0
  else Result := 1;
end;

function ReallocMem(p:Pointer; Size:Integer):Pointer;
begin
  Result := System.ReallocMemory(p, Size)
end;

procedure qsort(var A: array of Cardinal; len:Cardinal);
   procedure sort(l,r: Integer);
     var i,j:Integer;
     x,y: Cardinal;
   begin
     i := l;
     j := r;
     x := A[ (l + r) div 2 ];
     repeat
       while A[i] < x do inc(i);
       while x < A[j] do dec(j);
       if not (i>j) then
         begin
           y    := A[i];
           A[i] := A[j];
           A[j] := y;
           inc(i);
           dec(j);
         end;
     until i>j;
     if l < j then sort(l,j);
     if i < r then sort(i,r);
   end;
begin
   sort(0,len-1);
end;

procedure qsort_points(var A: array of Point; len:Cardinal);
   procedure sort(l,r: Integer);
   var
     i,j: Integer;
     x,y:Point;
   begin
     i := l;
     j := r;
     x := A[ (l + r) div 2 ];
     repeat
       while A[i].x < x.x do inc(i);
       while x.x < A[j].x do dec(j);
       if not (i>j) then
         begin
           y    := A[i];
           A[i] := A[j];
           A[j] := y;
           inc(i);
           dec(j);
         end;
     until i>j;
     if l < j then sort(l,j);
     if i < r then sort(i,r);
   end;
begin
   sort(0,len-1);
end;


{$IFNDEF  stb_prof}
function  stb_prof(x:integer):Integer;
begin
  Result := 0;
end;
{$ENDIF}

Function IS_PUSH_MODE(f:pvorb) : Boolean;
Begin
  Result := f.push_mode;
End;

Function error(f: pvorb; e:STBVorbisError):Boolean;
Begin
  Log(logError, 'OGG', 'Decoding error occurred: ' +  IntegerProperty.Stringify(Integer(E)));

  f.error := e;
  If (not f.eof) and (e<>VORBIS_need_more_data) then
    f.error := e; // breakpoint for debugging

  Result := false;

   //Halt;//!! if need halt on error !!
End;
   
// these functions are used for allocating temporary memory
// while decoding. if you can afford the stack space, use
// alloca(); otherwise, provide a temp buffer and it will
// allocate out of those.

function array_size_required(count,size:Integer):Integer;
begin
   Result := count*(sizeof(Pointer)+(size));
end;

function setup_temp_malloc(f:pvorb; sz:Cardinal):Pointer;
begin
   sz := (sz+3) and  (not 3);
   if f.alloc.alloc_buffer<>nil then begin
      if (f.temp_offset-sz < f.setup_offset) then begin
         Result := nil;
         Exit;
      end;
      f.temp_offset := f.temp_offset-sz;
      Result := f.alloc.alloc_buffer+f.temp_offset;
      Exit;
   end;
   GetMem(Result, sz);
end;

function temp_alloc(f:pvorb; size:Integer):Pointer;
begin
   if f.alloc.alloc_buffer<>nil then
      Result := setup_temp_malloc(f,size)
   else
      GetMem(Result, size);
end;

function temp_alloc_save(f:pvorb):Integer;
begin
   Result := f.temp_offset;
end;

function temp_alloc_restore(f:pvorb; p:Integer):Integer;
begin
   f.temp_offset := p;
   Result := f.temp_offset;
end;

function setup_malloc(f:pvorb; sz:Cardinal):Pointer;
begin
   sz := (sz+3) and (not 3);
   f.setup_memory_required := f.setup_memory_required+sz;
   if (f.alloc.alloc_buffer<>nil) then begin
      if (f.setup_offset+sz > f.temp_offset) then begin
         Result := nil;
         Exit;
      end;
      Result :=  f.alloc.alloc_buffer + f.setup_offset;
      f.setup_offset := f.setup_offset+sz;
      Exit;
   end;

   If sz>0 then
    GetMem(Result, sz)
   Else
    Result := nil;
end;

procedure setup_free(f:pvorb; p:Pointer);
begin
   if (f.alloc.alloc_buffer<>nil) then Exit; // do nothing; setup mem is not a stack
   FreeMem(p);
end;

procedure setup_temp_free(f:pvorb; p:Pointer; sz:Cardinal);
begin
   if (f.alloc.alloc_buffer<>nil) then begin 
      f.temp_offset := f.temp_offset + ((sz+3) and (not 3));
      Exit;
   end;
   FreeMem(p);
end;

// used in setup, and for huffman that doesn't go fast path
function  bit_reverse(n:Cardinal):Cardinal;
begin
  n := ((n and $AAAAAAAA) shr  1) or ((n and $55555555) shl 1);
  n := ((n and $CCCCCCCC) shr  2) or ((n and $33333333) shl 2);
  n := ((n and $F0F0F0F0) shr  4) or ((n and $0F0F0F0F) shl 4);
  n := ((n and $FF00FF00) shr  8) or ((n and $00FF00FF) shl 8);
  Result := (n shr 16) or (n shl 16);
end;

// this is a weird definition of log2() for which log2(1) = 1, log2(2) = 2, log2(4) = 3
// as required by the specification. fast(?) implementation from stb.h
// OPTIMIZE: called multiple times per-packet with "constants"; move to setup
function ilog(n:Integer):Integer;
   const log2_4: array [0..15] of Byte = ( 0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4 );
   var one:Cardinal;
begin
   one:=1;
   Result:=0;
   // 2 compares if n < 16, 3 compares otherwise (4 if signed or n > 1<<29)
   if (n < Integer(one shl 14)) then begin
      if (n < Integer(one shl 4)) then Result:=0+log2_4[n]
      else begin
         if (n < Integer(one shl 9)) then Result:=5+log2_4[n shr 5]
         else Result:=10+log2_4[n shr 10];
      end;
   end
   else begin
      if (n < Integer(one shl 24)) then begin
         if (n < Integer(one shl 19)) then Result:=15+log2_4[n shr 15]
         else Result:=20+log2_4[n shr 20];
       end
       else begin
         if (n < Integer(one shl 29)) then Result:=25+log2_4[n shr 25]
         else begin
            if (n < Integer(one shl 31)) then Result:=30+log2_4[n shr 30]
            else Result:=0; // signed n returns 0
         end;    
       end;
   end;               
end;

/////////////////////// LEAF SETUP FUNCTIONS //////////////////////////
//
// these functions are only called at setup, and only a few times
// per file



// zlib & jpeg huffman tables assume that the output symbols
// can either be arbitrarily arranged, or have monotonically
// increasing frequencies--they rely on the lengths being sorted;
// this makes for a very simple generation algorithm.
// vorbis allows a huffman table with non-sorted lengths. This
// requires a more sophisticated construction, since symbols in
// order do not map to huffman codes "in order".
//static void add_entry(Codebook *c, Cardinal huff_code, int symbol, int count, int len, Cardinal *values)
procedure add_entry(c:PCodebook; huff_code:Cardinal; symbol:integer; count:integer; len:integer; var values:array of Cardinal);
begin
   if c.sparse=0 then c.codewords[symbol] := huff_code
   else begin
      c.codewords[count] := huff_code;
      c.codeword_lengths[count] := len;
      values[count] := symbol;
   end;
end;

function compute_codewords(c:PCodebook; len:array of Byte; n:integer; var values:array of Cardinal):Boolean;
var
   i,k,m: integer;
   available: array[1..32] of Cardinal;
   res: Cardinal;
   z,y : integer;
begin
   m:=0;
   for i:=1 to 32 do available[i]:=0;
   
   // find the first entry
   for k:=0 to n-1 do if len[k] < NO_CODE then break;
   if k = n then begin assert(c.sorted_entries=0); Result:=true; Exit; end;
   // add to the list
   add_entry(c, 0, k, m, len[k], values);
   Inc(m);
   // add all available leaves
   for i:=1 to len[k] do available[i] := 1 shl (32-i);
   // note that the above code treats the first case specially,
   // but it's really the same as the following code, so they
   // could probably be combined (except the initial code is 0,
   // and I use 0 in available[] to mean 'empty')
   for i:=k+1 to n-1 do begin
      z := len[i];
      if z = NO_CODE then continue;
      // find lowest available leaf (should always be earliest,
      // which is what the specification calls for)
      // note that this property, and the fact we can never have
      // more than one free leaf at a given level, isn't totally
      // trivial to prove, but it seems true and the assert never
      // fires, so!
      while (z>0) and (available[z]=0) do Dec(z);
      if z=0 then begin  assert(false); Result:=false; exit; end;
      res := available[z];
      available[z] := 0;
      add_entry(c, bit_reverse(res), i, m, len[i], values);
      Inc(m);
      // propogate availability up the tree
      if z <> len[i] then begin 
         for y:=len[i] downto z+1 do begin
            assert(available[y]=0);
            available[y] := res + (1 shl (32-y));
         end;
      end;
   end;
   Result := true;
end;

// accelerated huffman table allows fast O(1) match of all symbols
// of length <= STB_VORBIS_FAST_HUFFMAN_LENGTH
procedure compute_accelerated_huffman(c:PCodebook);
var
   i,len: Word;
   z: Cardinal;
begin
   for i:=0 to FAST_HUFFMAN_TABLE_SIZE-1 do c.fast_huffman[i]:=-1;
   if c.sparse>0 then len:=c.sorted_entries else len:=c.entries;

   {$IFDEF STB_VORBIS_FAST_HUFFMAN_SHORT}
   if len>32767 then len:=32767; // largest possible value we can encode!
   {$ENDIF}
   for i:=0 to len-1 do begin
      if c.codeword_lengths[i]<=STB_VORBIS_FAST_HUFFMAN_LENGTH then begin
         if c.sparse<>0 then z:=bit_reverse(c.sorted_codewords[i]) else z:=c.codewords[i];
         // set table entries for all bit combinations in the higher bits
         while z<FAST_HUFFMAN_TABLE_SIZE do begin
             c.fast_huffman[z] := i;
             z := z+ (1 shl c.codeword_lengths[i]);
         end;
      end;
   end;
end;

function Cardinal_compare(p,q: pCardinal):integer;
begin
   if p^ = q^ then Result:=0
   else if p^ < q^ then Result:=-1
   else Result:=1;
end;

function include_in_sort(c:PCodebook; len:Byte):Boolean;
begin
   if c.sparse<>0 then begin  assert(len<>NO_CODE); Result:=true; Exit; end;
   if len=NO_CODE then begin Result:=false; Exit; end;
   if len>STB_VORBIS_FAST_HUFFMAN_LENGTH then begin Result:=true; Exit; end;
   Result:=false;
end;

// if the fast table above doesn't work, we want to binary
// search them... need to reverse the bits
procedure compute_sorted_huffman(c:PCodebook; lengths: array of Byte; values: array of Cardinal);
var
   i,len,k,x,n,m,huff_len: integer;
   code: Cardinal;
begin
   // build a list of all the entries
   // OPTIMIZATION: don't include the short ones, since they'll be caught by FAST_HUFFMAN.
   // this is kind of a frivolous optimization--I don't see any performance improvement,
   // but it's like 4 extra lines of code, so.
   if c.sparse=0 then begin
      k:=0;
      for i:=0 to c.entries-1 do 
         if include_in_sort(c, lengths[i]) then begin
            c.sorted_codewords[k] := bit_reverse(c.codewords[i]);
            Inc(k);
         end;   
      assert(k=c.sorted_entries);
   end else begin
      for i:=0 to c.sorted_entries-1 do
         c.sorted_codewords[i] := bit_reverse(c.codewords[i]);
   end;
   

   qsort(c.sorted_codewords, c.sorted_entries);

   c.sorted_codewords[c.sorted_entries] := $ffffffff;

   if c.sparse>0 then len:=c.sorted_entries else len:=c.entries;
   // now we need to indicate how they correspond; we could either
   //   #1: sort a different data structure that says who they correspond to
   //   #2: for each sorted entry, search the original list to find who corresponds
   //   #3: for each original entry, find the sorted entry
   // #1 requires extra storage, #2 is slow, #3 can use binary search!
   for i:=0 to len-1 do begin
      if c.sparse>0 then  huff_len:=lengths[values[i]] else huff_len:=lengths[i];
      if include_in_sort(c,huff_len) then begin
         code := bit_reverse(c.codewords[i]);
         x:=0; 
         n:=c.sorted_entries;
         while n > 1do begin
            // invariant: sc[x] <= code < sc[x+n]
            m := x + (n shr 1);
            if c.sorted_codewords[m] <= code then begin
               x := m;
               n := n - (n shr 1);
            end else begin
               n := n shr 1;
            end;
         end;
         assert(c.sorted_codewords[x]=code);
         if c.sparse>0 then begin
            c.sorted_values[x] := values[i];
            c.codeword_lengths[x] := huff_len;
         end else begin
            c.sorted_values[x] := i;
         end;
      end;
   end;
end;

// only run while parsing the header (3 times)
function vorbis_validate(data:PAnsiChar):Boolean;
const
   vorbis: array [1..6] Of AnsiChar = ( 'v', 'o', 'r', 'b', 'i', 's' );
begin
{$IFDEF FPC}
   if compareMemRange(data, @vorbis[1], 6) = 0 then Result:=true else Result:=false;
{$ELSE}
  Result := CompareMem(data, @vorbis[1], 6);
{$ENDIF}
end;

// called from setup only, once per code book
// (formula implied by specification)
Function lookup1_values(entries,dim: integer):integer;
Var
  r:Integer;
  N:Single;
Begin
//  Log(logDebug, 'OGG', 'l1 '+ IntegerProperty.Stringify(entries)+' / ' + IntegerProperty.Stringify(Dim));
//  Log(logDebug, 'OGG', 'l1a');
  N := lnXP1(entries-1);

  //Log(logDebug, 'OGG', 'l1b');
  N := exp(N/dim);

//  Log(logDebug, 'OGG', 'l1c');
  r := floor(N);

  //Log(logDebug, 'OGG', 'l2');
  if floor(power(r+1, dim))<=entries then
    inc(r);

//  Log(logDebug, 'OGG', 'l3');
  If (power(r+1, dim)<=entries) Or (floor(power(r,dim))>entries) Then // (int),floor() as above
  Begin
    RaiseError('Assertion failed while decoding stream.');
  End;

//  Log(logDebug, 'OGG', 'l4');
  Result := r;
End;

// called twice per file
procedure compute_twiddle_factors(n:integer; var A,B,C: array of single);
var n4,n8,k,k2: Integer;
begin
   n4 := n shr 2;
   n8 := n shr 3;
   k2:=0;

   for k:=0 to n4-1 do begin
      A[k2  ] :=  cos(4*k*PI/n);
      A[k2+1] := -sin(4*k*PI/n);
      B[k2  ] := cos((k2+1)*PI/n/2) * 0.5;
      B[k2+1] := sin((k2+1)*PI/n/2) * 0.5;
      k2:=k2+2;
   end;
   k2:=0;
   for k:=0 to n8-1 do begin
      C[k2  ] :=  cos(2*(k2+1)*PI/n);
      C[k2+1] := -sin(2*(k2+1)*PI/n);
      k2:=k2+2;
   end;
end;

procedure compute_window(n:integer; var window: array of single);
var n2,i: integer;
begin
   n2 := n shr 1;
   for i:=0 to n2-1 do
      window[i] := sin(0.5 * PI * Sqr(sin((i - 0 + 0.5) / n2 * 0.5 * PI)));
end;

procedure compute_bitreverse(n:integer; var rev:array of Word);
var ld,i,n8: integer;
begin
   ld := ilog(n) - 1; // ilog is off-by-one from normal definitions
   n8 := n shr 3;
   for i:=0 to n8-1 do
      rev[i] := (bit_reverse(i) shr (32-ld+3)) shl 2;
end;

function init_blocksize(f:pvorb; b,n:integer):Boolean;
var n2,n4,n8:integer;
begin
   n2 := n shr 1;
   n4 := n shr 2;
   n8 := n shr 3;

   setLength(f.A[b],n2);
   setLength(f.B[b],n2);
   setLength(f.C[b],n4);
   compute_twiddle_factors(n, f.A[b], f.B[b], f.C[b]);
   setLength(f.window[b],n2);
   compute_window(n, f.window[b]);
   setLength(f.bit_reverse[b],n8);
   compute_bitreverse(n, f.bit_reverse[b]);
   Result:=true;
end;

procedure neighbors(x: array of Word; n:integer; var plow, phigh: integer);
var low,high,i: integer;
begin
   low := -1;
   high := 65536;
   for i:=0 to n-1 do begin
      if (x[i]>low) and (x[i]<x[n]) then begin plow:=i; low:=x[i]; end;
      if (x[i]<high) and (x[i]>x[n]) then begin  phigh:=i; high:=x[i]; end;
   end;
end;

function point_compare(p,q:PPoint):integer;
begin
   if p.x=q.x then Result:=0
   else if p.x<q.x then Result:=-1
   else Result:=1;
end;

/////////////////////// END LEAF SETUP FUNCTIONS //////////////////////////
Function get8(z:pvorb):Byte;
Begin
   Result:=0;

   if PtrUInt(z.stream) >= PtrUInt(z.stream_end) Then
    begin
      z.eof:=true;
      Exit;
    end;

    Result:=z.stream^;
    Inc(z.stream);
End;

function get32(f:pvorb):Cardinal;
begin
   Result := get8(f);
   Result := Result+(get8(f) shl 8);
   Result := Result+(get8(f) shl 16);
   Result := Result+(get8(f) shl 24);
end;

Function getn(z:pvorb; var data:array of Byte; n:integer):Boolean;
Var
  nread,i:integer;
Begin
  Result := False;

  If (PtrUInt(z.stream)+n > PtrUInt(z.stream_end)) Then
  Begin
    z.eof := True;
    Exit;
  End;

  For i:=0 to n-1 Do
    data[i] := (IncPointer(z.stream,i))^;

  //Move(z.stream,data,n);//!!not working for unknown reason
  z.stream := IncPointer(z.stream,n);
  Result := true;
End;

Procedure skip(z:pvorb; n:integer);
Begin
  z.stream := IncPointer(z.stream,n);
  If PtrUInt(z.stream) >= PtrUInt(z.stream_end) Then
    z.eof:=true;
End;

Function set_file_offset(f:pvorb; loc:Cardinal):boolean;
Begin
  Result := False;
  f.eof := false;

  If (PtrUInt(f.stream_start)+loc>=PtrUInt(f.stream_end)) or (PtrUInt(f.stream_start)+loc<PtrUInt(f.stream_start)) Then
  Begin
    f.stream := f.stream_end;
    f.eof := true;
  End Else
  Begin
    f.stream := IncPointer(f.stream_start, loc);
    Result := true;
  End;
End;

function capture_pattern(f:pvorb):Boolean;
begin
   Result:=false;
   if $4f <> get8(f) then Exit;
   if $67 <> get8(f) then Exit;
   if $67 <> get8(f) then Exit;
   if $53 <> get8(f) then Exit;
   Result:=true;
end;

function start_page_no_capturepattern(f:pvorb):Boolean;
var 
   loc0,loc1,n:Cardinal;
   i,len:integer;
   p:ProbedPage;
begin
   // stream structure version
   if 0<>get8(f) then begin Result:=error(f, VORBIS_invalid_stream_structure_version); Exit; end;
   // header flag
   f.page_flag := get8(f);
   // absolute granule position
   loc0 := get32(f); 
   loc1 := get32(f);
   // TODO: validate loc0,loc1 as valid positions?
   // stream serial number -- vorbis doesn't interleave, so discard
   get32(f);
   //if (f->serial != get32(f)) return error(f, VORBIS_incorrect_stream_serial_number);
   // page sequence number
   n := get32(f);
   f.last_page := n;
   // CRC32
   get32(f);
   // page_segments
   f.segment_count := get8(f);
   if not getn(f, f.segments, f.segment_count) then begin
      Result:=error(f, VORBIS_unexpected_eof);
      Exit;
   end;
   // assume we _don't_ know any the sample position of any segments
   f.end_seg_with_known_loc := -2;
   //if (loc0 <> (not 0)) or (loc1 <> (not 0)) then begin
   if (loc0 <> $FFFFFFFF) or (loc1 <> $FFFFFFFF) then begin
      // determine which packet is the last one that will complete
      for i:=f.segment_count-1 downto 0 do
         if f.segments[i] < 255 then break;
      // 'i' is now the index of the _last_ segment of a packet that ends
      if i >= 0 then begin
         f.end_seg_with_known_loc := i;
         f.known_loc_for_packet   := loc0;
      end;
   end;
   if f.first_decode then begin
      len := 0;
      for i:=0 to f.segment_count-1 do 
         len := len + f.segments[i];
      len := len + 27 + f.segment_count;
      p.page_start := f.first_audio_page_offset;
      p.page_end := p.page_start + len;
      p.after_previous_page_start := p.page_start;
      p.first_decoded_sample := 0;
      p.last_decoded_sample := loc0;
      f.p_first := p;
   end;
   f.next_seg := 0;
   Result:=true;
end;

function  start_page(f:pvorb):Boolean;
begin
   if  not capture_pattern(f) then begin
      Result:=error(f, VORBIS_missing_capture_pattern);
      Exit;
   end;
   Result:=start_page_no_capturepattern(f);
end;

function start_packet(f:pvorb):Boolean;
begin
   while f.next_seg = -1 do begin
      if not start_page(f) then begin Result:=false; Exit; end;
      if (f.page_flag and PAGEFLAG_continued_packet)<>0 then begin
         Result:=error(f, VORBIS_continued_packet_flag_invalid);
         Exit;
      end;
   end;
   f.last_seg := false;
   f.valid_bits := 0;
   f.packet_bytes := 0;
   f.bytes_in_seg := 0;
   // f->next_seg is now valid
   Result:=true;
end;

function maybe_start_packet(f:pvorb):Boolean;
var x:Byte;
begin
   Result:=false;
   if f.next_seg = -1 then begin
      x := get8(f);
      if f.eof then Exit; // EOF at page boundary is not an error!
      if $4f <> x       then begin Result:=error(f, VORBIS_missing_capture_pattern); Exit; end;
      if $67 <> get8(f) then begin Result:=error(f, VORBIS_missing_capture_pattern); Exit; end;
      if $67 <> get8(f) then begin Result:=error(f, VORBIS_missing_capture_pattern); Exit; end;
      if $53 <> get8(f) then begin Result:=error(f, VORBIS_missing_capture_pattern); Exit; end;
      if  not start_page_no_capturepattern(f) then Exit;
      if (f.page_flag and PAGEFLAG_continued_packet)<>0 then begin
         // set up enough state that we can read this packet if we want,
         // e.g. during recovery
         f.last_seg := false;
         f.bytes_in_seg := 0;
         Result:=error(f, VORBIS_continued_packet_flag_invalid);
         Exit;
      end;
   end;
   Result:=start_packet(f);
end;

function next_segment(f:pvorb):integer;
var len:integer;
begin
   Result:=0;
   if f.last_seg then Exit;
   if f.next_seg = -1 then begin
      f.last_seg_which := f.segment_count-1; // in case start_page fails
      if not start_page(f) then begin f.last_seg:=true; Exit; end;
      if not (f.page_flag and PAGEFLAG_continued_packet)=0 then begin
         error(f, VORBIS_continued_packet_flag_invalid);
         Exit;
      end;
   end;
   len := f.segments[f.next_seg];
   Inc(f.next_seg);
   if len < 255 then begin
      f.last_seg := true;
      f.last_seg_which := f.next_seg-1;
   end;
   if f.next_seg >= f.segment_count then
      f.next_seg := -1;
//   assert(f.bytes_in_seg > 0);
   f.bytes_in_seg := len;
   Result := len;
end;

function get8_packet_raw(f:pvorb):integer;
begin
   result:=EOP;
   if f.bytes_in_seg=0 then begin
      if f.last_seg then Exit
      else if next_segment(f)=0 then Exit;
   end;   
   assert(f.bytes_in_seg > 0);
   Dec(f.bytes_in_seg);
   Inc(f.packet_bytes);
   Result:=get8(f);
end;

function get8_packet(f:pvorb):integer;
begin
   Result := get8_packet_raw(f);
   f.valid_bits := 0;
end;

procedure flush_packet(f:pvorb);
begin
   while get8_packet_raw(f) <> EOP do;
end;

// OPTIMIZE: this is the secondary bit decoder, so it's probably not as important
// as the huffman decoder?
function get_bits(f:pvorb; n:integer):Cardinal;
var 
   z: Cardinal;
   iz:integer;
begin
   Result:=0;
   if f.valid_bits < 0 then Exit;
   if f.valid_bits < n then begin
      if n > 24 then begin
         // the accumulator technique below would not work correctly in this case
         z := get_bits(f, 24);
         z := z + (get_bits(f, n-24) shl 24);
         Result:=z;
         Exit;
      end;
      if f.valid_bits = 0 then f.acc := 0;
      while f.valid_bits < n do begin
         iz := get8_packet_raw(f);
         if iz = EOP then begin
            f.valid_bits := INVALID_BITS;
            Exit;
         end;
         z:=iz;
         f.acc := f.acc + (z shl f.valid_bits);
         f.valid_bits := f.valid_bits + 8;
      end;
   end;
   if f.valid_bits < 0 then Exit;
   z := f.acc and PtrUInt((1 shl n)-1);
   f.acc := f.acc shr n;
   f.valid_bits := f.valid_bits - n;
   Result:=z;
end;

function get_bits_signed(f:pvorb; n:integer):Integer;
var z:Cardinal;
begin
   z := get_bits(f, n);
   if (z and (1 shl (n-1)))<>0 then
      z := z + (not((1 shl n) - 1));
   Result:=z;
end;

// OPTIMIZE: primary accumulator for huffman
// expand the buffer to as many bits as possible without reading off end of packet
// it might be nice to allow f->valid_bits and f->acc to be stored in registers,
// e.g. cache them locally and decode locally
procedure  prep_huffman(f:pvorb);
var z:integer;
begin
   if f.valid_bits <= 24 then begin
      if f.valid_bits = 0 then f.acc:=0;
      repeat
         if f.last_seg and (f.bytes_in_seg=0) then Exit;
         z := get8_packet_raw(f);
         if z = EOP then Exit;
         f.acc := f.acc + (z shl f.valid_bits);
         f.valid_bits := f.valid_bits+8;
      until (f.valid_bits > 24);
   end;
end;

function codebook_decode_scalar_raw(f:pvorb; c:PCodebook):integer;
var
   i,x,n,m,len: integer;
   code: Cardinal;
begin
   prep_huffman(f);

   assert((Length(c.sorted_codewords)>0) or (Length(c.codewords)>0));
   // cases to use binary search: sorted_codewords && !c->codewords
   //                             sorted_codewords && c->entries > 8
   //if (c->entries > 8 ? c->sorted_codewords!=NULL : !c->codewords) 
   if (c.entries>8) and (Length(c.sorted_codewords)>0) or
      (c.entries<=8) and (Length(c.codewords)=0) then begin
      // binary search
      code := bit_reverse(f.acc);
      x:=0;
      n:=c.sorted_entries;

      while n > 1 do begin
         // invariant: sc[x] <= code < sc[x+n]
         m := x + (n shr 1);
         if c.sorted_codewords[m] <= code then begin
            x := m;
            n := n- (n shr 1);
         end else begin
            n := n shr 1;
         end;
      end;
      // x is now the sorted index
      if c.sparse=0 then x:=c.sorted_values[x];
      // x is now sorted index if sparse, or symbol otherwise
      len := c.codeword_lengths[x];
      if f.valid_bits >= len then begin
         f.acc := f.acc shr len;
         f.valid_bits := f.valid_bits-len;
         Result:=x;
         Exit;
      end;

      f.valid_bits := 0;
      Result:=-1;
      Exit;
   end;

   // if small, linear search
   assert(c.sparse=0);
   for i:=0 to c.entries-1 do begin
      if c.codeword_lengths[i] = NO_CODE then continue;
      if c.codewords[i] = (f.acc and ((1 shl c.codeword_lengths[i])-1)) then begin
         if f.valid_bits >= c.codeword_lengths[i] then begin
            f.acc := f.acc shr c.codeword_lengths[i];
            f.valid_bits := f.valid_bits - c.codeword_lengths[i];
            Result:=i;
            Exit;
         end;
         f.valid_bits := 0;
         Result:=-1;
         Exit;
      end;
   end;

   error(f, VORBIS_invalid_stream);
   f.valid_bits := 0;
   Result:=-1;
end;

function codebook_decode_scalar(f:pvorb; c:PCodebook):integer;
var i: integer;
begin
   if f.valid_bits < STB_VORBIS_FAST_HUFFMAN_LENGTH then
      prep_huffman(f);
   // fast huffman table lookup
   i := f.acc and FAST_HUFFMAN_TABLE_MASK;
   i := c.fast_huffman[i];
   if i >= 0 then begin
      f.acc := f.acc shr c.codeword_lengths[i];
      f.valid_bits := f.valid_bits - c.codeword_lengths[i];
      if f.valid_bits < 0 then begin f.valid_bits:=0; Result:=-1; Exit; end;
      Result:=i;
      Exit;
   end;
   Result:=codebook_decode_scalar_raw(f,c);
end;

{$IFNDEF STB_VORBIS_NO_INLINE_DECODE}

procedure DECODE_RAW(var v:integer; f:pvorb; c:PCodebook);
var n:integer;
begin
   if f.valid_bits < STB_VORBIS_FAST_HUFFMAN_LENGTH then
      prep_huffman(f);
   v := f.acc and FAST_HUFFMAN_TABLE_MASK;
   v := c.fast_huffman[v];
   if v >= 0 then begin
      n := c.codeword_lengths[v];
      f.acc := f.acc shr n;
      f.valid_bits := f.valid_bits - n;
      if f.valid_bits < 0 then begin f.valid_bits:=0; v:=-1; end;
   end else begin
      v := codebook_decode_scalar_raw(f,c);
   end;
end;

{$ELSE}

procedure DECODE_RAW(var v:integer; f:pvorb; c:PCodebook);
begin
    v := codebook_decode_scalar(f,c);
end;

{$ENDIF}

procedure DECODE(var v:integer; f:pvorb; c:PCodebook);
begin
   DECODE_RAW(v,f,c);
   if c.sparse>0 then v := c.sorted_values[v];
end;

{$IFNDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
procedure DECODE_VQ(var v:integer; f:pvorb; c:PCodebook);
begin
   DECODE_RAW(v,f,c);
end;
{$ELSE}
procedure DECODE_VQ(var v:integer; f:pvorb; c:PCodebook);
begin
   DECODE(v,f,c);
end;
{$ENDIF}

// CODEBOOK_ELEMENT_FAST is an optimization for the CODEBOOK_FLOATS case
// where we avoid one addition
{$IFNDEF STB_VORBIS_CODEBOOK_FLOATS}
function CODEBOOK_ELEMENT(c:pcodebook;off:integer):codetype;
begin
   Result:=c.multiplicands[off] * c.delta_value + c->minimum_value;
end;
function CODEBOOK_ELEMENT_FAST(c:pcodebook;off:integer):codetype;
begin
   Result:=c.multiplicands[off] * c->delta_value;
end;     
function CODEBOOK_ELEMENT_BASE(c:pcodebook):codetype;
begin
   REsult:=c.minimum_value;
end;
{$ELSE}
function CODEBOOK_ELEMENT(c:pcodebook;off:integer):codetype;
begin
   Result:=c.multiplicands[off];
end;   
function CODEBOOK_ELEMENT_FAST(c:pcodebook;off:integer):codetype;
begin
   Result:=c.multiplicands[off];
end;   
function CODEBOOK_ELEMENT_BASE(c:pcodebook):codetype;
begin
   Result:=0;
end;   
{$ENDIF}

function codebook_decode_start(f:pvorb; c:pcodebook; len:integer):integer;
var z: integer;
begin
   z := -1;

   // type 0 is only legal in a scalar context
   if c.lookup_type = 0 then
      error(f, VORBIS_invalid_stream)
   else begin
      DECODE_VQ(z,f,c);
      if c.sparse>0 then assert(z<c.sorted_entries);
      if z < 0 then begin  // check for EOP
         if f.bytes_in_seg=0 then
            if f.last_seg then begin
               Result:=z;
               Exit;
            end;
         error(f, VORBIS_invalid_stream);
      end;
   end;
   Result:=z;
end;

function codebook_decode(f:pvorb; c:pcodebook; output: pSingle; len:integer):boolean;
var
   i,z: integer;
   last, val: codetype;
{$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
   _div,off:integer;
{$ENDIF}
begin
   Result:=false;
   z := codebook_decode_start(f,c,len);
   if z < 0 then Exit;
   if len>c.dimensions then  len := c.dimensions;

{$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
   if c.lookup_type = 1 then begin
      last := CODEBOOK_ELEMENT_BASE(c);
      _div := 1;
      for i:=0 to len-1 do begin
         //off := (z / _div) mod c.lookup_values;
         off := (z div _div) mod c.lookup_values;
         val := CODEBOOK_ELEMENT_FAST(c,off) + last;
         (output+i)^ := (output+i)^ + val;
         if c.sequence_p>0 then last := val + c.minimum_value;
         _div := _div * c.lookup_values;
      end;
      Result:=true;
      Exit;
   end;
{$ENDIF}

   z := z * c.dimensions;
   if c.sequence_p>0 then begin
      last := CODEBOOK_ELEMENT_BASE(c);
      for i:=0 to len-1 do begin
         val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
         (IncPointer(output,i))^ := (IncPointer(output,i))^ + val;
         last := val + c.minimum_value;
      end;
   end else begin
      last := CODEBOOK_ELEMENT_BASE(c);
      for i:=0 to len-1 do begin
         (IncPointer(output,i))^ := (IncPointer(output,i))^ + CODEBOOK_ELEMENT_FAST(c,z+i) + last;
      end;
   end;

   Result:=true;
end;

function codebook_decode_step(f:pvorb; c:pcodebook; output: pSingle; len,step:integer):Boolean;
var
   i,z:integer;
   last,val:codetype;
{$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
   _div,off:integer;
{$ENDIF}
begin
   Result:=false;
   z := codebook_decode_start(f,c,len);
   last := CODEBOOK_ELEMENT_BASE(c);
   if z < 0 then Exit;
   if len>c.dimensions then len:=c.dimensions;

{$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
   if c.lookup_type = 1 then begin
      _div := 1;
      for i:=0 to len-1 do begin
         //off := (z / _div) mod c.lookup_values;
         off := (z div _div) mod c.lookup_values;
         val := CODEBOOK_ELEMENT_FAST(c,off) + last;
         (output+i*step)^ := (output+i*step)^ + val;
         if c.sequence_p>0 then last := val;
         _div := _div * c.lookup_values;
      end;
      Result:=true;
      Exit;
   end;
{$ENDIF}

   z := z * c.dimensions;
   for i:=0 to len-1 do begin
      val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
      (IncPointer(output,i*step))^ := (IncPointer(output,i*step))^ + val;
      if c.sequence_p>0 then last := val;
   end;

   Result:=true;
end;

function codebook_decode_deinterleave_repeat(f:pvorb; c:pcodebook; var outputs:PFPArray;
         ch:integer; var c_inter,p_inter:integer; len,total_decode:integer):Boolean;
var
   i,z,effective: integer;
   last:codetype;
   val:codetype;
{$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
   _div,off:integer;
{$ENDIF}   
begin
   effective := c.dimensions;

   // type 0 is only legal in a scalar context
   if c.lookup_type = 0 then begin Result:=error(f, VORBIS_invalid_stream); Exit; end;

   while total_decode > 0 do begin
      last := CODEBOOK_ELEMENT_BASE(c);
      DECODE_VQ(z,f,c);
      {$IFNDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
      assert((c.sparse=0) or (z<c.sorted_entries));
      {$ENDIF}
      if z < 0 then begin
         if f.bytes_in_seg=0 then
            if f.last_seg then begin Result:=false; Exit; end;
         Result:=error(f, VORBIS_invalid_stream);
         Exit;
      end;

      // if this will take us off the end of the buffers, stop short!
      // we check by computing the length of the virtual interleaved
      // buffer (len*ch), our current offset within it (p_inter*ch)+(c_inter),
      // and the length we'll be using (effective)
      if c_inter+p_inter*ch+effective > len*ch then begin
         effective := len*ch - (p_inter*ch - c_inter);
      end;

   {$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
      if c.lookup_type = 1 then begin
         _div := 1;
         for i:=0 to effective-1 do begin
            //off := (z / _div) mod c.lookup_values;
            off := (z div _div) mod c.lookup_values;
            val := CODEBOOK_ELEMENT_FAST(c,off) + last;
            (IncPointer((outputs)^[c_inter], p_inter))^ := (IncPointer((outputs)^[c_inter], p_inter))^ + val;
            Inc(c_inter);
            if c_inter = ch then begin c_inter=0; Inc(p_inter); end;
            if c.sequence_p>0 then last:=val;
            _div := _div * c.lookup_values;
         end;
      end else
   {$ENDIF}
      begin
         z := z * c.dimensions;
         if c.sequence_p>0 then begin
            for i:=0 to effective-1 do begin
               val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
               (IncPointer((outputs)^[c_inter], p_inter))^ := (IncPointer((outputs)^[c_inter], p_inter))^ + val;
               Inc(c_inter);
               if c_inter = ch then begin c_inter:=0; Inc(p_inter); end;
               last := val;
            end;
         end else begin
            for i:=0 to effective-1 do begin
               val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
               (IncPointer((outputs)^[c_inter],p_inter))^ := (IncPointer((outputs)^[c_inter],p_inter))^ + val;
               Inc(c_inter);
               if c_inter = ch then begin c_inter:=0; Inc(p_inter); end;
            end;
         end;
      end;

      total_decode := total_decode - effective;
   end;
   result:=true;
end;

{$IFNDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
function codebook_decode_deinterleave_repeat_2(f:pvorb; c:pcodebook; var outputs:PFPArray;
         var c_inter,p_inter:integer; len,total_decode:integer):Boolean;
var
   i,z,effective: integer;
   last,val: codetype;
   z0,z1: pSingle;
begin
   effective := c.dimensions;

   // type 0 is only legal in a scalar context
   if c.lookup_type = 0 then begin Result:=error(f, VORBIS_invalid_stream); Exit; end;

   while total_decode > 0 do begin
      last := CODEBOOK_ELEMENT_BASE(c);
      DECODE_VQ(z,f,c);

      if z < 0 then begin
         if f.bytes_in_seg>0 then
            if f.last_seg then begin Result:=false; Exit; end;
         Result:=error(f, VORBIS_invalid_stream);
         Exit;
      end;

      // if this will take us off the end of the buffers, stop short!
      // we check by computing the length of the virtual interleaved
      // buffer (len*ch), our current offset within it (p_inter*ch)+(c_inter),
      // and the length we'll be using (effective)
      if c_inter+p_inter*2+effective > len*2 then begin
         effective := len*2 - (p_inter*2 - c_inter);
      end;

      z := z * c.dimensions;
      stb_prof(11);
      if c.sequence_p>0 then begin
         // haven't optimized this case because I don't have any examples
         for i:=0 to effective-1 do begin
            val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
            (IncPointer((outputs)^[c_inter],p_inter))^ := (IncPointer((outputs)^[c_inter],p_inter))^ + val;
            Inc(c_inter);
            if c_inter = 2 then begin c_inter:=0; Inc(p_inter); end;
            last := val;
         end;
      end else begin
         i:=0;
         if c_inter = 1 then begin
            val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
            (IncPointer((outputs)^[c_inter],p_inter))^ := (IncPointer((outputs)^[c_inter],p_inter))^ + val;
            c_inter:=0; Inc(p_inter);
            Inc(i);
         end;
         z0 := outputs^[0];
         z1 := outputs^[1];
         while i+1<effective do begin
            (IncPointer(z0,p_inter))^ := (IncPointer(z0,p_inter))^ + CODEBOOK_ELEMENT_FAST(c,z+i) + last;
            (IncPointer(z1,p_inter))^ := (IncPointer(z1,p_inter))^ + CODEBOOK_ELEMENT_FAST(c,z+i+1) + last;
            Inc(p_inter);
            i := i+2;
         end;
         if i<effective then begin
            val := CODEBOOK_ELEMENT_FAST(c,z+i) + last;
            (IncPointer((outputs)^[c_inter],p_inter))^ := (IncPointer((outputs)^[c_inter],p_inter))^ + val;
            Inc(c_inter);
            if c_inter=2 then begin c_inter:=0; Inc(p_inter); end;
         end;
      end;

      total_decode := total_decode - effective;
   end;
   Result:=true;
end;
{$ENDIF}

function predict_point(x,x0,x1,y0,y1:integer):integer;
var dy,adx,err,off:integer;
begin
   dy := y1 - y0;
   adx := x1 - x0;
   // OPTIMIZE: force int division to round in the right direction... is this necessary on x86?
   err := abs(dy) * (x - x0);
   off := err div adx;
   if dy<0 then Result:=y0-off else Result:=y0+off;
end;

// OPTIMIZE: if you want to replace this bresenham line-drawing routine,
// note that you must produce bit-identical output to decode correctly;
// this specific sequence of operations is specified in the spec (it's
// drawing integer-quantized frequency-space lines that the encoder
// expects to be exactly the same)
//     ... also, isn't the whole point of Bresenham's algorithm to NOT
// have to divide in the setup? sigh.
{$IFNDEF STB_VORBIS_NO_DEFER_FLOOR}
function LINE_OP(a,b:codetype):codetype;
begin
   result:=a * b;
end;   
{$ELSE}
function LINE_OP(a,b:codetype):codetype;
begin
   result := b;
end;   
{$ENDIF}


procedure draw_line(output:psingle; x0,y0,x1,y1,n:integer);
var
   dy,adx,ady,base,x,y,err,sy: integer;

begin
   dy := y1 - y0;
   adx := x1 - x0;
   ady := abs(dy);
   x:=x0;
   y:=y0;
   err := 0;

{$IFDEF STB_VORBIS_DIVIDE_TABLE}
   if (adx<DIVTAB_DENOM) and (ady<DIVTAB_NUMER) then begin
      if dy < 0 then begin
         base := -integer_divide_table[ady][adx];
         sy := base-1;
      end else begin
         base :=  integer_divide_table[ady][adx];
         sy := base+1;
      end;
   end else begin
      base := dy div adx;
      if dy < 0 then sy := base-1 else sy := base+1;
   end;
{$ELSE}
   base := dy div adx;
   if dy < 0 then sy := base-1 else sy := base+1;
{$ENDIF}
   ady := ady - abs(base) * adx;
   if x1 > n then x1 := n;
   IncPointer(output,x)^:=LINE_OP(IncPointer(output,x)^, inverse_db_table[y]);
   Inc(x);
   //for (++x; x < x1; ++x)
   while x<x1 do begin
      err := err + ady;
      if err>=adx then begin
         err := err - adx;
         y := y + sy;
      end else
         y := y + base;
      IncPointer(output,x)^:=LINE_OP(IncPointer(output,x)^, inverse_db_table[y]);
      Inc(x);
   end;
end;

function residue_decode(f:pvorb; book:pcodebook; var target: pSingle; offset,n,rtype: integer):Boolean;
var k,step: integer;
begin
   Result:=false;
   if rtype=0 then begin
      step := n div book.dimensions;
      for k:=0 to step-1  do
         if not codebook_decode_step(f, book, IncPointer(target,offset+k), n-offset-k, step) then
            Exit;
   end else begin
      k:=0;
      while k<n do begin
         if not codebook_decode(f, book, IncPointer(target,offset), n-k) then
            Exit;
         k := k + book.dimensions;
         offset := offset + book.dimensions;
      end;
   end;
   Result:=true;
end;

procedure decode_residue(f:pvorb; residue_buffers:PFPArray; ch,n,rn:integer; do_not_decode: array of Boolean);
Label done;
var
   i,ii,j,pass,rtype,c,classwords,n_read,
   part_read,temp_alloc_point,//len,
   pcount,class_set,z,c_inter,p_inter,
   q,n1,b,offset :integer;
   r: PResidue;
   cb:pcodebook;
   target: psingle;
begin
   q:=0;
   r := @f.residue_config[rn];
   rtype := f.residue_types[rn];
   c := r.classbook;
   classwords := f.codebooks[c].dimensions;
   n_read := r._end - r._begin;
   part_read := n_read div r.part_size;
   temp_alloc_point := temp_alloc_save(f);
   {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
   setLength(part_classdata,f.channels);
   
   For i:=0 to Pred(f.channels) Do
    setlength(part_classdata[i],part_read);//part_classdata[i]:=getMem(part_read*sizeof(pByte));

   {$ELSE}
   setLength(classifications,0,0);
   setLength(classifications,f.channels,part_read);
   {$ENDIF}

   stb_prof(2);
   for i:=0 to ch-1 do
      if not do_not_decode[i] then
         //memset(residue_buffers[i], 0, sizeof(float) * n);
         for ii:=0 to n-1 do (IncPointer((residue_buffers)^[i],ii))^:=0;

   if (rtype=2) and (ch<>1) then begin
      //len := ch * n;
      //for j:=0 to ch-1 do begin inc(i); if not do_not_decode[j] then break; end;
      j:=0;
      while j<ch do begin
         inc(j);
         if not do_not_decode[j-1] then break;
      end;
      if j=ch then goto done;

      stb_prof(3);
      for pass:=0 to 8-1 do begin
         pcount := 0;
         class_set := 0;
         if ch=2 then begin
            stb_prof(13);
            while pcount<part_read do begin
               z := r._begin + pcount*r.part_size;
               c_inter := (z and 1);
               p_inter := z shr 1;
               if pass=0 then begin
                  cb := @f.codebooks[r.classbook];
                  DECODE(q,f,cb);
                  if q=EOP then goto done;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  part_classdata[0][class_set] := r.classdata[q];
                  {$ELSE}
                  for i:=classwords-1 downto 0 do begin
                     classifications[0][i+pcount] := q mod r.classifications;
                     q := q div r->classifications;
                  end;
                  {$ENDIF}
               end;
               stb_prof(5);
               //for (i=0; i < classwords && pcount < part_read; ++i, ++pcount) {
               i:=0;
               while (i<classwords) and (pcount<part_read) do begin
                  z := r._begin + pcount*r.part_size;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  c := part_classdata[0][class_set][i];
                  {$ELSE}
                  c := classifications[0][pcount];
                  {$ENDIF}
                  b := r.residue_books[c][pass];
                  if b>=0 then begin
                     cb := @f.codebooks[b];
                     stb_prof(20);  // accounts for X time
                     {$IFDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
                     if not codebook_decode_deinterleave_repeat(f, cb, residue_buffers, ch, c_inter, p_inter, n, r.part_size) then
                        goto done;
                     {$ELSE}
                     // saves 1%
                     if not codebook_decode_deinterleave_repeat_2(f, cb, residue_buffers, c_inter, p_inter, n, r.part_size) then
                        goto done;
                     {$ENDIF}
                     stb_prof(7);
                  end else begin
                     z := z + r.part_size;
                     c_inter := z and 1;
                     p_inter := z shr 1;
                  end;
                  Inc(i);
                  Inc(pcount);
               end;
               stb_prof(8);
               {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
               Inc(class_set);
               {$ENDIF}
            end;
         end else if ch=1 then begin
            while pcount<part_read do begin
               z := r._begin + pcount*r.part_size;
               c_inter:=0;
               p_inter := z;
               if pass=0 then begin
                  cb := @f.codebooks[r.classbook];
                  DECODE(q,f,cb);
                  if q=EOP then goto done;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  part_classdata[0][class_set] := r.classdata[q];
                  {$ELSE}
                  for i:=classwords-1 downto 0 do begin
                     classifications[0][i+pcount] := q mod r.classifications;
                     q := q div r->classifications;
                  end;
                  {$ENDIF}
               end;
               //for (i=0; i < classwords && pcount < part_read; ++i, ++pcount) {
               i:=0;
               while (i<classwords) and (pcount<part_read) do begin
                  z := r._begin + pcount*r.part_size;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  c := part_classdata[0][class_set][i];
                  {$ELSE}
                  c := classifications[0][pcount];
                  {$ENDIF}
                  b := r.residue_books[c][pass];
                  if b>=0 then begin
                     cb := @f.codebooks[b];
                     stb_prof(22);
                     if not codebook_decode_deinterleave_repeat(f, cb, residue_buffers, ch, c_inter, p_inter, n, r.part_size) then
                        goto done;
                     stb_prof(3);
                  end else begin
                     z := z + r.part_size;
                     c_inter := 0;
                     p_inter := z;
                  end;
                  Inc(i);
                  Inc(pcount);
               end;
               {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
               Inc(class_set);
               {$ENDIF}
            end;
         end else begin
            while pcount<part_read do begin
               z := r._begin + pcount*r.part_size;
               c_inter := z mod ch;
               p_inter := z div ch;
               if pass=0 then begin
                  cb := @f.codebooks[r.classbook];
                  DECODE(q,f,cb);
                  if q=EOP then goto done;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  part_classdata[0][class_set] := r.classdata[q];
                  {$ELSE}
                  for i:=classwords-1 downto 0 do begin
                     classifications[0][i+pcount] := q mod r.classifications;
                     q := q div r->classifications;
                  end;
                  {$ENDIF}
               end;
               //for (i=0; i < classwords && pcount < part_read; ++i, ++pcount) {
               i:=0;
               while (i<classwords) and (pcount<part_read) do begin
                  z := r._begin + pcount*r.part_size;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  c := part_classdata[0][class_set][i];
                  {$ELSE}
                  c := classifications[0][pcount];
                  {$ENDIF}
                  b := r.residue_books[c][pass];
                  if b>=0 then begin
                     cb := @f.codebooks[b];
                     stb_prof(22);
                     if not codebook_decode_deinterleave_repeat(f, cb, residue_buffers, ch, c_inter, p_inter, n, r.part_size) then
                        goto done;
                     stb_prof(3);
                  end else begin
                     z := z + r.part_size;
                     c_inter := z mod ch;
                     p_inter := z div ch;
                  end;
                  Inc(i);
                  Inc(pcount);
               end;
               {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
               Inc(class_set);
               {$ENDIF}
            end;
         end;
      end;
      goto done;
   end;
   stb_prof(9);

   for pass:=0 to 8-1 do begin
      pcount := 0;
      class_set:=0;
      while pcount<part_read do begin
         if pass=0 then begin
            for j:=0 to ch-1 do begin
               if not do_not_decode[j] then begin
                  cb := @f.codebooks[r.classbook];
                  DECODE(q,f,cb);
                  if q=EOP then goto done;
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  part_classdata[j][class_set] := r.classdata[q];
                  {$ELSE}
                  for i:=classwords-1 downto 0 do begin
                     classifications[j][i+pcount] = q mod r.classifications;
                     q := q div r->classifications;
                  end;
                  {$ENDIF}
               end;
            end;
         end;
         //for (i=0; i < classwords && pcount < part_read; ++i, ++pcount) {
         i:=0;
         while (i<classwords) and (pcount<part_read) do begin
            for j:=0 to ch-1 do begin
               if not do_not_decode[j] then begin
                  {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
                  c := part_classdata[j][class_set][i];
                  {$ELSE}
                  c := classifications[j][pcount];
                  {$ENDIF}
                  b := r.residue_books[c][pass];
                  if b>=0 then begin
                     target := IncPointer(residue_buffers,j)^[0];
                     offset := r._begin + pcount * r.part_size;
                     n1 := r.part_size;
                     cb := @f.codebooks[b];
                     if not residue_decode(f, cb, target, offset, n1, rtype) then
                        goto done;
                  end;
               end;
            end;
            Inc(i);
            Inc(pcount);
         end;
         {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
         Inc(class_set);
         {$ENDIF}
      end;
   end;
done:
   stb_prof(0);
   {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
   for i:=0 to f.channels-1 do setlength(part_classdata[i],0);//freeMem(part_classdata[i]);
   setLength(part_classdata,0);
   {$ELSE}
   setLength(classifications,0,0);
   {$ENDIF}
   temp_alloc_restore(f,temp_alloc_point);
end;


// the following were split out into separate functions while optimizing;
// they could be pushed back up but eh. __forceinline showed no change;
// they're probably already being inlined.
procedure  imdct_step3_iter0_loop(n:integer; e:Psingle; i_off,k_off: integer; A:Psingle);
var
   ee0,ee2 :PSingle;
   k00_20, k01_21: single;
   i:integer;
begin
   ee0 := IncPointer(e, i_off);
   ee2 := IncPointer(ee0, k_off);

   assert((n and 3)=0);
   for i:=(n shr 2) downto 1 do begin
      k00_20  := ee0^ - ee2^;
      k01_21  := IncPointer(ee0,-1)^- IncPointer(ee2,-1)^;
      ee0^ := ee0^ + ee2^;//ee0[ 0] = ee0[ 0] + ee2[ 0];
      IncPointer(ee0,-1)^ := IncPointer(ee0,-1)^ + IncPointer(ee2,-1)^;//ee0[-1] = ee0[-1] + ee2[-1];
      ee2^ := k00_20 * A^ - k01_21 * IncPointer(A,+1)^;
      IncPointer(ee2,-1)^ := k01_21 * A^ + k00_20 * IncPointer(A,+1)^;
      A := IncPointer(A, 8);

      k00_20  := IncPointer(ee0,-2)^ - IncPointer(ee2,-2)^;
      k01_21  := IncPointer(ee0,-3)^ - IncPointer(ee2,-3)^;
      IncPointer(ee0,-2)^ := IncPointer(ee0,-2)^ + IncPointer(ee2,-2)^;//ee0[-2] = ee0[-2] + ee2[-2];
      IncPointer(ee0,-3)^ := IncPointer(ee0,-3)^ + IncPointer(ee2,-3)^;//ee0[-3] = ee0[-3] + ee2[-3];
      IncPointer(ee2,-2)^ := k00_20 * A^ - k01_21 * IncPointer(A,+1)^;
      IncPointer(ee2,-3)^ := k01_21 * A^ + k00_20 * IncPointer(A,+1)^;
      A := IncPointer(A, 8);

      k00_20  := IncPointer(ee0,-4)^ - IncPointer(ee2,-4)^;
      k01_21  := IncPointer(ee0,-5)^ - IncPointer(ee2,-5)^;
      IncPointer(ee0,-4)^ := IncPointer(ee0,-4)^ + IncPointer(ee2,-4)^;//ee0[-4] = ee0[-4] + ee2[-4];
      IncPointer(ee0,-5)^ := IncPointer(ee0,-5)^ + IncPointer(ee2,-5)^;//ee0[-5] = ee0[-5] + ee2[-5];
      IncPointer(ee2,-4)^ := k00_20 * A^ - k01_21 * IncPointer(A,+1)^;
      IncPointer(ee2,-5)^ := k01_21 * A^ + k00_20 * IncPointer(A,+1)^;
      A := IncPointer(A, 8);

      k00_20  := IncPointer(ee0,-6)^ - IncPointer(ee2,-6)^;
      k01_21  := IncPointer(ee0,-7)^ - IncPointer(ee2,-7)^;
      IncPointer(ee0,-6)^ := IncPointer(ee0,-6)^ + IncPointer(ee2,-6)^;//ee0[-6] = ee0[-6] + ee2[-6];
      IncPointer(ee0,-7)^ := IncPointer(ee0,-7)^ + IncPointer(ee2,-7)^;//ee0[-7] = ee0[-7] + ee2[-7];
      IncPointer(ee2,-6)^ := k00_20 * A^ - k01_21 * IncPointer(A,+1)^;
      IncPointer(ee2,-7)^ := k01_21 * A^ + k00_20 * IncPointer(A,+1)^;
      A := IncPointer(A, 8);
      ee0 := IncPointer(ee0, -8);
      ee2 := IncPointer(ee2, -8);
   end;
end;

procedure imdct_step3_inner_r_loop(lim:integer; e:psingle; d0,k_off:integer; A:psingle; k1:integer);
var
   i: integer;
   k00_20, k01_21: single;
   e0,e2: psingle;
begin
   e0 := IncPointer(e, + d0);
   e2 := IncPointer(e0, + k_off);

   for i:=(lim shr 2) downto 1do begin
      k00_20 := e0^ - e2^;
      k01_21 := IncPointer(e0,-1)^ - IncPointer(e2,-1)^;
      e0^ := e0^ + e2^;//e0[-0] = e0[-0] + e2[-0];
      IncPointer(e0,-1)^ := IncPointer(e0,-1)^ + IncPointer(e2,-1)^;//e0[-1] = e0[-1] + e2[-1];
      (e2)^ := (k00_20)*A^ - (k01_21) * IncPointer(A,+1)^;
      IncPointer(e2,-1)^ := (k01_21)*A^ + (k00_20) * IncPointer(A,+1)^;
      A := IncPointer(A, k1);

      k00_20 := IncPointer(e0,-2)^ - IncPointer(e2,-2)^;
      k01_21 := IncPointer(e0,-3)^ - IncPointer(e2,-3)^;
      IncPointer(e0,-2)^ := IncPointer(e0,-2)^ + IncPointer(e2,-2)^;//e0[-2] = e0[-2] + e2[-2];
      IncPointer(e0,-3)^ := IncPointer(e0,-3)^ + IncPointer(e2,-3)^;//e0[-3] = e0[-3] + e2[-3];
      IncPointer(e2,-2)^ := (k00_20)*A^ - (k01_21) * IncPointer(A,+1)^;
      IncPointer(e2,-3)^ := (k01_21)*A^ + (k00_20) * IncPointer(A,+1)^;
      A := IncPointer(A, k1);

      k00_20 := IncPointer(e0,-4)^ - IncPointer(e2,-4)^;
      k01_21 := IncPointer(e0,-5)^ - IncPointer(e2,-5)^;
      IncPointer(e0,-4)^ := IncPointer(e0,-4)^ + IncPointer(e2,-4)^;//e0[-4] = e0[-4] + e2[-4];
      IncPointer(e0,-5)^ := IncPointer(e0,-5)^ + IncPointer(e2,-5)^;//e0[-5] = e0[-5] + e2[-5];
      IncPointer(e2,-4)^ := (k00_20)*A^ - (k01_21) * IncPointer(A,+1)^;
      IncPointer(e2,-5)^ := (k01_21)*A^ + (k00_20) * IncPointer(A,+1)^;
      A := IncPointer(A, k1);

      k00_20 := IncPointer(e0,-6)^ - IncPointer(e2,-6)^;
      k01_21 := IncPointer(e0,-7)^ - IncPointer(e2,-7)^;
      IncPointer(e0,-6)^ := IncPointer(e0,-6)^ + IncPointer(e2,-6)^;//e0[-6] = e0[-6] + e2[-6];
      IncPointer(e0,-7)^ := IncPointer(e0,-7)^ + IncPointer(e2,-7)^;//e0[-7] = e0[-7] + e2[-7];
      IncPointer(e2,-6)^ := (k00_20)*A^ - (k01_21) * IncPointer(A,+1)^;
      IncPointer(e2,-7)^ := (k01_21)*A^ + (k00_20) * IncPointer(A,+1)^;

      e0 := IncPointer(e0, -8);
      e2 := IncPointer(e2, -8);

      A := IncPointer(A, k1);
   end;
end;

procedure imdct_step3_inner_s_loop(n:integer; e:Psingle; i_off,k_off:integer; A:psingle; a_off,k0:integer);
var
   i: integer;
   A0,A1,A2,A3,A4,A5,A6,A7,k00,k11: single;
   ee0,ee2: psingle;
begin
   A0 := A^;
   A1 := IncPointer(A,+1)^;
   A2 := IncPointer(A,+a_off)^;
   A3 := IncPointer(A,+a_off+1)^;
   A4 := IncPointer(A,+a_off*2+0)^;
   A5 := IncPointer(A,+a_off*2+1)^;
   A6 := IncPointer(A,+a_off*3+0)^;
   A7 := IncPointer(A,+a_off*3+1)^;

   ee0 := IncPointer(e,  +i_off);
   ee2 := IncPointer(ee0,+k_off);

   for i:=n downto 1 do begin
      k00     := ee0^ - ee2^;
      k11     := IncPointer(ee0,-1)^ - IncPointer(ee2,-1)^;
      ee0^    := (ee0)^ + ee2^;
      IncPointer(ee0,-1)^:= IncPointer(ee0,-1)^ + IncPointer(ee2,-1)^;
      ee2^    := (k00) * A0 - (k11) * A1;
      IncPointer(ee2,-1)^:= (k11) * A0 + (k00) * A1;

      k00     := IncPointer(ee0,-2)^ - IncPointer(ee2,-2)^;
      k11     := IncPointer(ee0,-3)^ - IncPointer(ee2,-3)^;
      IncPointer(ee0,-2)^:= IncPointer(ee0,-2)^ + IncPointer(ee2,-2)^;
      IncPointer(ee0,-3)^:= IncPointer(ee0,-3)^ + IncPointer(ee2,-3)^;
      IncPointer(ee2,-2)^:= (k00) * A2 - (k11) * A3;
      IncPointer(ee2,-3)^:= (k11) * A2 + (k00) * A3;

      k00     := IncPointer(ee0,-4)^ - IncPointer(ee2,-4)^;
      k11     := IncPointer(ee0,-5)^ - IncPointer(ee2,-5)^;
      IncPointer(ee0,-4)^:= IncPointer(ee0,-4)^ + IncPointer(ee2,-4)^;
      IncPointer(ee0,-5)^:= IncPointer(ee0,-5)^ + IncPointer(ee2,-5)^;
      IncPointer(ee2,-4)^:= (k00) * A4 - (k11) * A5;
      IncPointer(ee2,-5)^:= (k11) * A4 + (k00) * A5;

      k00     := IncPointer(ee0,-6)^ - IncPointer(ee2,-6)^;
      k11     := IncPointer(ee0,-7)^ - IncPointer(ee2,-7)^;
      IncPointer(ee0,-6)^:= IncPointer(ee0,-6)^ + IncPointer(ee2,-6)^;
      IncPointer(ee0,-7)^:= IncPointer(ee0,-7)^ + IncPointer(ee2,-7)^;
      IncPointer(ee2,-6)^:= (k00) * A6 - (k11) * A7;
      IncPointer(ee2,-7)^:= (k11) * A6 + (k00) * A7;

      ee0 := IncPointer(ee0, -k0);
      ee2 := IncPointer(ee2, -k0);
   end;
end;

procedure iter_54(z:psingle);
var
   k00,k11,k22,k33,
   y0,y1,y2,y3: single;

begin

   k00  := z^ - IncPointer(z,-4)^;
   y0   := z^ + IncPointer(z,-4)^;
   y2   := IncPointer(z,-2)^ + IncPointer(z,-6)^;
   k22  := IncPointer(z,-2)^ - IncPointer(z,-6)^;

   z^ := y0 + y2;      // z0 + z4 + z2 + z6
   IncPointer(z,-2)^ := y0 - y2;      // z0 + z4 - z2 - z6

   // done with y0,y2

   k33  := IncPointer(z,-3)^ - IncPointer(z,-7)^;

   IncPointer(z,-4)^ := k00 + k33;    // z0 - z4 + z3 - z7
   IncPointer(z,-6)^ := k00 - k33;    // z0 - z4 - z3 + z7

   // done with k33

   k11  := IncPointer(z,-1)^ - IncPointer(z,-5)^;
   y1   := IncPointer(z,-1)^ + IncPointer(z,-5)^;
   y3   := IncPointer(z,-3)^ + IncPointer(z,-7)^;

   IncPointer(z,-1)^ := y1 + y3;      // z1 + z5 + z3 + z7
   IncPointer(z,-3)^ := y1 - y3;      // z1 + z5 - z3 - z7
   IncPointer(z,-5)^ := k11 - k22;    // z1 - z5 + z2 - z6
   IncPointer(z,-7)^ := k11 + k22;    // z1 - z5 - z2 + z6
end;

procedure imdct_step3_inner_s_loop_ld654(n:integer; e:psingle; i_off:integer; A:psingle; base_n:integer);
var
   a_off: integer;
   A2,k00,k11: single;
   z,base: psingle;
begin
   //k_off := -8;
   a_off := base_n shr 3;
   A2 := IncPointer(A,+a_off)^;
   z := IncPointer(e, i_off);
   base := IncPointer(z, -16 * n);

   while PtrUInt(z)>PtrUInt(base) do begin
      k00   := IncPointer(z,-0)^ - IncPointer(z,-8)^;
      k11   := IncPointer(z,-1)^ - IncPointer(z,-9)^;
      IncPointer(z,-0)^:= IncPointer(z,-0)^ + IncPointer(z,-8)^;
      IncPointer(z,-1)^:= IncPointer(z,-1)^ + IncPointer(z,-9)^;
      IncPointer(z,-8)^:=  k00;
      IncPointer(z,-9)^:=  k11 ;

      k00    := IncPointer(z, -2)^ - IncPointer(z, -10)^;
      k11    := IncPointer(z, -3)^ - IncPointer(z, -11)^;
      IncPointer(z, -2)^:= IncPointer(z, -2)^ + IncPointer(z, -10)^;
      IncPointer(z, -3)^:= IncPointer(z, -3)^ + IncPointer(z, -11)^;
      IncPointer(z,-10)^:= (k00+k11) * A2;
      IncPointer(z,-11)^:= (k11-k00) * A2;

      k00    := IncPointer(z,-12)^ - IncPointer(z, -4)^;  // reverse to avoid a unary negation
      k11    := IncPointer(z, -5)^ - IncPointer(z, -13)^;
      IncPointer(z, -4)^:= IncPointer(z, -4)^ + IncPointer(z,-12)^;
      IncPointer(z, -5)^:= IncPointer(z, -5)^ + IncPointer(z,-13)^;
      IncPointer(z,-12)^:= k11;
      IncPointer(z,-13)^:= k00;

      k00    := IncPointer(z,-14)^ - IncPointer(z, -6)^;  // reverse to avoid a unary negation
      k11    := IncPointer(z, -7)^ - IncPointer(z,-15)^;
      IncPointer(z, -6)^:= IncPointer(z, -6)^ + IncPointer(z,-14)^;
      IncPointer(z, -7)^:= IncPointer(z, -7)^ + IncPointer(z,-15)^;
      IncPointer(z,-14)^:= (k00+k11) * A2;
      IncPointer(z,-15)^:= (k00-k11) * A2;

      iter_54(z);
      iter_54(IncPointer(z,-8));
      z := IncPointer(z, -16);
   end;
end;

procedure inverse_mdct(buffer:psingle; n:integer; f:pvorb; blocktype:integer);
var
   n2,n4,n8,l,ld,save_point: integer;//,n3_4
   k0,k0_2,i,lim,k1,rlim,r,i_off: integer;
   k4: integer;
   buffer2: array of single;
   buf2,A,A0,u,v,d,e,AA,e_stop,d0,d1,d2,d3,e0,e1,C,B: psingle;
   v40_20,v41_21,a02,a11,b0,b1,b2,b3,p0,p1,p2,p3: single;
   bitrev: pWord;
begin
   n2 := n shr 1;
   n4 := n shr 2;
   n8 := n shr 3;
   //n3_4 := n - n4;
   // OPTIMIZE: reduce register pressure by using fewer variables?
   save_point := temp_alloc_save(f);
   //float *buf2 = (float *) temp_alloc(f, n2 * sizeof(*buf2));
   setlength(buffer2,n2);
   buf2:=@buffer2[0];
//   u:=nil;
   v:=nil;
   // twiddle factors
   A := @f.A[blocktype][0];

   // IMDCT algorithm from "The use of multirate filter banks for coding of high quality digital audio"
   // See notes about bugs in that paper in less-optimal implementation 'inverse_mdct_old' after this function.

   // kernel from paper


   // merged:
   //   copy and reflect spectral data
   //   step 0

   // note that it turns out that the items added together during
   // this step are, in fact, being added to themselves (as reflected
   // by step 0). inexplicable inefficiency! this became obvious
   // once I combined the passes.

   // so there's a missing 'times 2' here (for adding X to itself).
   // this propogates through linearly to the end, where the numbers
   // are 1/2 too small, and need to be compensated for.

   d := IncPointer(buf2,+n2-2);
   AA := A;
   e := buffer;
   e_stop := IncPointer(buffer,+n2);
   while e<>e_stop do begin
      IncPointer(d,+1)^ := (e^ * AA^ - IncPointer(e,+2)^*IncPointer(AA,+1)^);
      d^ := (e^ * IncPointer(AA,+1)^ + IncPointer(e,+2)^*AA^);
      d := IncPointer(d, -2);
      AA := IncPointer(AA, +2);
      e := IncPointer(e, +4);
   end;

   e := IncPointer(buffer,+n2-3);
   while PtrUInt(d)>=PtrUInt(buf2) do begin
      IncPointer(d,+1)^ := (-IncPointer(e,+2)^ * AA^ - -e^*IncPointer(AA,+1)^);
      d^ := (-IncPointer(e,+2)^ * IncPointer(AA,+1)^ + -e^*AA^);
      d := IncPointer(d, -2);
      AA := IncPointer(AA, +2);
      e := IncPointer(e, -4);
   end;

   // now we use symbolic names for these, so that we can
   // possibly swap their meaning as we change which operations
   // are in place

   u := buffer;
   v := buf2;

   // step 2    (paper output is w, now u)
   // this could be in place, but the data ends up in the wrong
   // place... _somebody_'s got to swap it, so this is nominated
   AA := IncPointer(A,+n2-8);

   e0 := IncPointer(v,+n4);
   e1 := v;

   d0 := IncPointer(u,+n4);
   d1 := u;

   while PtrUInt(AA)>=PtrUInt(A) do begin
      v41_21 := IncPointer(e0,+1)^ - IncPointer(e1,+1)^;
      v40_20 := e0^ - e1^;
      IncPointer(d0,+1)^:= IncPointer(e0,+1)^ + IncPointer(e1,+1)^;
      d0^    := e0^ + e1^;
      IncPointer(d1,+1)^:= v41_21*IncPointer(AA,+4)^ - v40_20*IncPointer(AA,+5)^;
      d1^    := v40_20*IncPointer(AA,+4)^ + v41_21*IncPointer(AA,+5)^;

      v41_21 := IncPointer(e0,+3)^ - IncPointer(e1,+3)^;
      v40_20 := IncPointer(e0,+2)^ - IncPointer(e1,+2)^;
      IncPointer(d0,+3)^:= IncPointer(e0,+3)^ + IncPointer(e1,+3)^;
      IncPointer(d0,+2)^:= IncPointer(e0,+2)^ + IncPointer(e1,+2)^;
      IncPointer(d1,+3)^:= v41_21*AA^ - v40_20*IncPointer(AA,+1)^;
      IncPointer(d1,+2)^:= v40_20*AA^ + v41_21*IncPointer(AA,+1)^;

      AA := IncPointer(AA, -8);

      d0 := IncPointer(d0, +4);
      d1 := IncPointer(d1, +4);
      e0 := IncPointer(e0, +4);
      e1 := IncPointer(e1, +4);
   end;
   // step 3
   ld := ilog(n) - 1; // ilog is off-by-one from normal definitions

   // optimized step 3:

   // the original step3 loop can be nested r inside s or s inside r;
   // it's written originally as s inside r, but this is dumb when r
   // iterates many times, and s few. So I have two copies of it and
   // switch between them halfway.

   // this is iteration 0 of step 3
   imdct_step3_iter0_loop(n shr 4, u, n2-1-n4*0, -(n shr 3), A);
   imdct_step3_iter0_loop(n shr 4, u, n2-1-n4*1, -(n shr 3), A);

   // this is iteration 1 of step 3
   imdct_step3_inner_r_loop(n shr 5, u, n2-1 - n8*0, -(n shr 4), A, 16);
   imdct_step3_inner_r_loop(n shr 5, u, n2-1 - n8*1, -(n shr 4), A, 16);
   imdct_step3_inner_r_loop(n shr 5, u, n2-1 - n8*2, -(n shr 4), A, 16);
   imdct_step3_inner_r_loop(n shr 5, u, n2-1 - n8*3, -(n shr 4), A, 16);

   l:=2;
   //for (; l < (ld-3)>>1; ++l) {
   while l<(ld-3) shr 1 do begin
      k0 := n shr (l+2);
      k0_2 := k0 shr 1;
      lim := 1 shl (l+1);
      for i:=0 to lim-1 do
         imdct_step3_inner_r_loop(n shr (l+4), u, n2-1 - k0*i, -k0_2, A, 1 shl (l+3));
      Inc(l);   
   end;
   //for (; l < ld-6; ++l) {
   while l<ld-6 do begin
      k0 := n shr (l+2);
      k1 := 1 shl (l+3);
      k0_2 := k0 shr 1;
      rlim := n shr (l+6);
      lim := 1 shl (l+1);
      A0 := A;
      i_off := n2-1;
      for r:=rlim downto 1 do begin
         imdct_step3_inner_s_loop(lim, u, i_off, -k0_2, A0, k1, k0);
         A0 := IncPointer(A0, k1*4);
         i_off := i_off - 8;
      end;
      Inc(l);
   end;

   // iterations with count:
   //   ld-6,-5,-4 all interleaved together
   //       the big win comes from getting rid of needless flops
   //         due to the constants on pass 5 & 4 being all 1 and 0;
   //       combining them to be simultaneous to improve cache made little difference
   imdct_step3_inner_s_loop_ld654(n shr 5, u, n2-1, A, n);

   // output is u

   // step 4, 5, and 6
   // cannot be in-place because of step 5

   bitrev := @f.bit_reverse[blocktype][0];
   // weirdly, I'd have thought reading sequentially and writing
   // erratically would have been better than vice-versa, but in
   // fact that's not what my testing showed. (That is, with
   // j = bitreverse(i), do you read i and write j, or read j and write i.)

   d0 := IncPointer(v,+n4-4);
   d1 := IncPointer(v,+n2-4);
   while PtrUInt(d0)>=PtrUInt(v) do begin
      k4 := bitrev^;
      IncPointer(d1,+3)^ := IncPointer(u,+k4+0)^;
      IncPointer(d1,+2)^ := IncPointer(u,+k4+1)^;
      IncPointer(d0,+3)^ := IncPointer(u,+k4+2)^;
      IncPointer(d0,+2)^ := IncPointer(u,+k4+3)^;

      k4 := IncPointer(bitrev,+1)^;
      IncPointer(d1,+1)^ := IncPointer(u,+k4+0)^;
      IncPointer(d1,+0)^ := IncPointer(u,+k4+1)^;
      IncPointer(d0,+1)^ := IncPointer(u,+k4+2)^;
      IncPointer(d0,+0)^ := IncPointer(u,+k4+3)^;

      d0 := IncPointer(d0, -4);
      d1 := IncPointer(d1, -4);
      bitrev := IncPointer(bitrev, +2);
   end;
   // (paper output is u, now v)


   // data must be in buf2
   assert(v=buf2);

   // step 7   (paper output is v, now v)
   // this is now in place
   C := @f.C[blocktype][0];

   d := v;
   e := IncPointer(v, + n2 - 4);

   while PtrUInt(d)<PtrUInt(e) do begin
      a02 := d^ - IncPointer(e,+2)^;
      a11 := IncPointer(d,+1)^ + IncPointer(e,+3)^;

      b0 := IncPointer(C,+1)^*a02 + IncPointer(C,+0)^*a11;
      b1 := IncPointer(C,+1)^*a11 - IncPointer(C,+0)^*a02;

      b2 := IncPointer(d,+0)^ + IncPointer(e, +2)^;
      b3 := IncPointer(d,+1)^ - IncPointer(e, +3)^;

      (d)^ := b2 + b0;
      IncPointer(d,+1)^ := b3 + b1;
      IncPointer(e,+2)^ := b2 - b0;
      IncPointer(e,+3)^ := b1 - b3;

      a02 := IncPointer(d,+2)^ - IncPointer(e,+0)^;
      a11 := IncPointer(d,+3)^ + IncPointer(e,+1)^;

      b0 := IncPointer(C,+3)^*a02 + IncPointer(C,+2)^*a11;
      b1 := IncPointer(C,+3)^*a11 - IncPointer(C,+2)^*a02;

      b2 := IncPointer(d,+2)^ + (e)^;
      b3 := IncPointer(d,+3)^ - IncPointer(e, +1)^;

      IncPointer(d,+2)^ := b2 + b0;
      IncPointer(d,+3)^ := b3 + b1;
      (e)^ := b2 - b0;
      IncPointer(e,+1)^ := b1 - b3;

      C := IncPointer(C, +4);
      d := IncPointer(d, +4);
      e := IncPointer(e, -4);
   end;

   // data must be in buf2


   // step 8+decode   (paper output is X, now buffer)
   // this generates pairs of data a la 8 and pushes them directly through
   // the decode kernel (pushing rather than pulling) to avoid having
   // to make another pass later

   // this cannot POSSIBLY be in place, so we refer to the buffers directly

   B := @f.B[blocktype][0];
   B := IncPointer(B, + n2 - 8);
   e := IncPointer(buf2, + n2 - 8);
   d0 := buffer;
   d1 := IncPointer(buffer,+n2-4);
   d2 := IncPointer(buffer,+n2);
   d3 := IncPointer(buffer,+n-4);
   while PtrUInt(e)>=PtrUInt(v) do begin
      p3 :=  IncPointer(e,+6)^*IncPointer(B,+7)^ - IncPointer(e,+7)^*IncPointer(B,+6)^;
      p2 := -IncPointer(e,+6)^*IncPointer(B,+6)^ - IncPointer(e,+7)^*IncPointer(B,+7)^;

      (d0)^ :=   p3;
      IncPointer(d1,+3)^ := - p3;
      (d2)^ :=   p2;
      IncPointer(d3,+3)^ :=   p2;

      p1 :=  IncPointer(e,+4)^*IncPointer(B,+5)^ - IncPointer(e,+5)^*IncPointer(B,+4)^;
      p0 := -IncPointer(e,+4)^*IncPointer(B,+4)^ - IncPointer(e,+5)^*IncPointer(B,+5)^;

      IncPointer(d0,+1)^ :=   p1;
      IncPointer(d1,+2)^ := - p1;
      IncPointer(d2,+1)^ :=   p0;
      IncPointer(d3,+2)^ :=   p0;

      p3 :=  IncPointer(e,+2)^*IncPointer(B,+3)^ - IncPointer(e,+3)^*IncPointer(B,+2)^;
      p2 := -IncPointer(e,+2)^*IncPointer(B,+2)^ - IncPointer(e,+3)^*IncPointer(B,+3)^;

      IncPointer(d0,+2)^ :=   p3;
      IncPointer(d1,+1)^ := - p3;
      IncPointer(d2,+2)^ :=   p2;
      IncPointer(d3,+1)^ :=   p2;

      p1 :=  e^*IncPointer(B,+1)^ - IncPointer(e,+1)^*(B)^;
      p0 := -(e)^*(B)^ - IncPointer(e,+1)^*IncPointer(B,+1)^;

      IncPointer(d0,+3)^ :=   p1;
      (d1)^ := - p1;
      IncPointer(d2,+3)^ :=   p0;
      (d3)^ :=   p0;

      B := IncPointer(B, -8);
      e := IncPointer(e, -8);
      d0 := IncPointer(d0, +4);
      d2 := IncPointer(d2, +4);
      d1 := IncPointer(d1, -4);
      d3 := IncPointer(d3, -4);
   end;

   temp_alloc_restore(f,save_point);
   setlength(buffer2,0);
end;

function  get_window(f:pvorb; len:integer):Psingle;
begin
   len := len shl 1;
   if len=f.blocksize_0 then begin Result:=@f.window[0][0]; Exit; end;
   if len=f.blocksize_1 then begin Result:=@f.window[1][0]; Exit; end;
//   assert(false);
   Result:=nil;
end;

function do_floor(f:pvorb; map:pmapping; i,n:integer; target:psingle; finalY: PYTYPE; step2_flag: pByte):boolean;
var
   n2,s, floor,j,q,lx,ly,hy,hx: integer;
   g: PFloor1;
begin
   n2 := n shr 1;
   s := map.chan[i].mux;
   floor := map.submap_floor[s];
   if f.floor_types[floor]=0then begin
      Result:=error(f, VORBIS_invalid_stream);
      Exit;
   end else begin
      g := @f.floor_config[floor].floor1;
      lx:=0;
      ly := finalY^ * g.floor1_multiplier;
      for q:=1 to g.values-1 do begin
         j := g.sorted_order[q];
         {$IFNDEF STB_VORBIS_NO_DEFER_FLOOR}
         if IncPointer(finalY,+j)^>=0 then
         {$ELSE}
         if step2_flag[j] then
         {$ENDIF}
         begin
            hy := IncPointer(finalY,+j)^ * g.floor1_multiplier;
            hx := g.Xlist[j];
            draw_line(target, lx,ly, hx,hy, n2);
            lx := hx;
            ly := hy;
         end;
      end;
      if lx<n2 then
         // optimization of: draw_line(target, lx,ly, n,ly, n2);
         for j:=lx to n2-1 do
            IncPointer(target,+j)^:=LINE_OP(IncPointer(target,+j)^, inverse_db_table[ly]);
   end;
   Result:=true;
end;

function vorbis_decode_initial(f:pvorb; var p_left_start,p_left_end,p_right_start,p_right_end,mode:integer):Boolean;
label retry;
var
   m:TMode;
   i,n,prev,next,window_center: integer;
begin
   f.channel_buffer_start:=0;
   f.channel_buffer_end:=0;

retry:
   if f.eof then begin result:=false; Exit; end;
   if not maybe_start_packet(f) then begin result:=false; Exit; end;
   // check packet type
   if get_bits(f,1)<>0 then begin
      if IS_PUSH_MODE(f) then begin
         Result:=error(f,VORBIS_bad_packet_type);
         Exit;
      end;
      while EOP<>get8_packet(f) do;
      goto retry;
   end;

   if f.alloc.alloc_buffer<>nil then
      assert(f.alloc.alloc_buffer_length_in_bytes=f.temp_offset);

   i := get_bits(f, ilog(f.mode_count-1));
   if i=EOP then begin result:=false; Exit; end;
   if i>=f.mode_count then  begin result:=false; Exit; end;
   mode := i;
   m := f.mode_config[i];
   if m.blockflag>0 then begin
      n := f.blocksize_1;
      prev := get_bits(f,1);
      next := get_bits(f,1);
   end else begin
      prev:=0;
      next:=0;
      n := f.blocksize_0;
   end;

// WINDOWING

   window_center := n shr 1;
   if (m.blockflag>0) and (prev=0) then begin
      p_left_start := (n - f.blocksize_0) shr 2;
      p_left_end   := (n + f.blocksize_0) shr 2;
   end else begin
      p_left_start := 0;
      p_left_end   := window_center;
   end;
   if (m.blockflag>0) and  (next=0) then begin
      p_right_start := (n*3 - f.blocksize_0) shr 2;
      p_right_end   := (n*3 + f.blocksize_0) shr 2;
   end else begin
      p_right_start := window_center;
      p_right_end   := n;
   end;
   Result:=true;
end;

function vorbis_decode_packet_rest(f:pvorb; var len:integer; _mode: TMode;
         left_start,left_end,right_start,right_end:integer; var p_left:integer):Boolean;
label err;
const
   range_list: array [0..4-1] of integer = ( 256, 128, 86, 64 );
var
   map: PMapping;
   i,i1,ii,j,k,n,n2,s,floor,range,offset,//,window_center
   pclass,cdim,cbits,csub,cval,book,ch,
   low,high,pred,highroom,lowroom,room,val,temp,r: integer;//,t
   zero_channel: array [0..256-1] of Boolean;
   really_zero_channel: array [0..256-1] of Boolean;
   g: PFloor1;
   finalY: pSmallInt;
   step2_flag: array [0..256-1] of Byte;
   cb: PCodebook;
   residue_buffers: FPArray;
   do_not_decode: array [0..256-1] of Boolean;
   a,m: pSingle;
   a2,m2: Single;
   current_end: Cardinal;
begin
   val:=0;
// WINDOWING

   FillChar(do_not_decode, SizeOf(do_not_decode), 0);
   FillChar(zero_channel, SizeOf(zero_channel), 0);

   n := f.blocksize[_mode.blockflag];
   //window_center := n shr 1;

   map := @f.mapping[_mode.mapping];
      
// FLOORS
   n2 := n shr 1;

   stb_prof(1);
   for i:=0 to f.channels-1 do begin
      s := map.chan[i].mux;
      zero_channel[i] := false;
      floor := map.submap_floor[s];
      if f.floor_types[floor]=0 then begin
         Result:=error(f, VORBIS_invalid_stream);
         Exit;
      end else begin
         g := @f.floor_config[floor].floor1;
         if get_bits(f, 1)<>0 then begin
            range := range_list[g.floor1_multiplier-1];
            offset := 2;
            finalY := @f.finalY[i][0];
            finalY^ := get_bits(f, ilog(range)-1);
            IncPointer(finalY,+1)^ := get_bits(f, ilog(range)-1);
            for j:=0 to g.partitions-1 do begin
               pclass := g.partition_class_list[j];
               cdim := g.class_dimensions[pclass];
               cbits := g.class_subclasses[pclass];
               csub := (1 shl cbits)-1;
               cval := 0;
               if cbits<>0 then begin
                  cb := @f.codebooks[g.class_masterbooks[pclass]];
                  DECODE(cval,f,cb);
               end;
               for k:=0 to cdim-1 do begin
                  book := g.subclass_books[pclass][cval and csub];
                  cval := cval shr cbits;
                  if book>=0 then begin
                     cb := @f.codebooks[book];
                     DECODE(temp,f,cb);
                     IncPointer(finalY,+offset)^ := temp;
                     Inc(offset);
                  end else begin
                     IncPointer(finalY,+offset)^ := 0;
                     Inc(offset);
                  end;
               end;
            end;
            if f.valid_bits=INVALID_BITS then goto err; // behavior according to spec
            step2_flag[0]:=1;
            step2_flag[1]:=1;
            for j:=2 to g.values-1 do begin
               low := g.neighbors[j][0];
               high := g.neighbors[j][1];
               //neighbors(g->Xlist, j, &low, &high);
               pred := predict_point(g.Xlist[j], g.Xlist[low], g.Xlist[high], IncPointer(finalY,+low)^, IncPointer(finalY,+high)^);
               val := IncPointer(finalY,+j)^;
               highroom := range - pred;
               lowroom := pred;
               if highroom<lowroom then
                  room := highroom * 2
               else
                  room := lowroom * 2;
               if val<>0 then begin
                  step2_flag[low]:=1;
                  step2_flag[high]:=1;
                  step2_flag[j]:=1;
                  if val>=room then
                     if highroom>lowroom then
                        IncPointer(finalY,+j)^ := val - lowroom + pred
                     else
                        IncPointer(finalY,+j)^ := pred - val + highroom - 1
                  else
                     if (val and 1)<>0 then
                        IncPointer(finalY,+j)^ := pred - ((val+1) shr 1)
                     else
                        IncPointer(finalY,+j)^ := pred + (val shr 1);
               end else begin
                  step2_flag[j] := 0;
                  IncPointer(finalY,+j)^ := pred;
               end;
            end;

{$IFDEF STB_VORBIS_NO_DEFER_FLOOR}
            do_floor(f, map, i, n, f.floor_buffers[i], finalY, step2_flag);
{$ELSE}
            // defer final floor computation until _after_ residue
            for j:=0 to g.values-1 do begin
               if step2_flag[j]=0 then IncPointer(finalY,+j)^ := -1;
            end;
{$ENDIF}
         end else begin
err:
            zero_channel[i] := true;
         end;
         // So we just defer everything else to later

         // at this point we've decoded the floor into buffer
      end;
   end;
   stb_prof(0);
   // at this point we've decoded all floors

   if f.alloc.alloc_buffer<>nil then
      assert(f.alloc.alloc_buffer_length_in_bytes = f.temp_offset);

   // re-enable coupled channels if necessary
   //memcpy(really_zero_channel, zero_channel, sizeof(really_zero_channel[0]) * f->channels);
   //move(zero_channel, really_zero_channel, sizeof(Boolean) * f.channels);
   for i:=0 to 256-1 do really_zero_channel[i]:=zero_channel[i];
   for i:=0 to map.coupling_steps-1 do
      if (not zero_channel[map.chan[i].magnitude]) or  (not zero_channel[map.chan[i].angle]) then begin
         zero_channel[map.chan[i].magnitude] := false;
         zero_channel[map.chan[i].angle] := false;
      end;

// RESIDUE DECODE
   for i1:=0 to map.submaps-1 do begin
      ch := 0;
      for j:=0 to f.channels-1 do begin
         if map.chan[j].mux=i1 then begin
            if zero_channel[j] then begin
               do_not_decode[ch] := true;
               residue_buffers[ch] := nil;
            end else begin
               do_not_decode[ch] := false;
               residue_buffers[ch] := f.channel_buffers[j];
            end;
            Inc(ch);
         end;
      end;
      r := map.submap_residue[i1];
      //t := f.residue_types[r];
      decode_residue(f, @residue_buffers[0], ch, n2, r, do_not_decode);
   end;

   if (f.alloc.alloc_buffer<>nil) then
      assert(f.alloc.alloc_buffer_length_in_bytes=f.temp_offset);

// INVERSE COUPLING
   stb_prof(14);
   for i1:=map.coupling_steps-1 downto 0 do begin
      n2 := n shr 1;
      m := f.channel_buffers[map.chan[i1].magnitude];
      a := f.channel_buffers[map.chan[i1].angle    ];
      for j:=0 to n2-1 do begin
         if IncPointer(m,+j)^>0 then begin
            if IncPointer(a,+j)^>0 then begin
               m2 := IncPointer(m,+j)^;
               a2 := IncPointer(m,+j)^ - IncPointer(a,+j)^;
            end else begin
               a2 := IncPointer(m,+j)^;
               m2 := IncPointer(m,+j)^ + IncPointer(a,+j)^;
            end;
         end else begin
            if IncPointer(a,+j)^>0 then begin
               m2 := IncPointer(m,+j)^;
               a2 := IncPointer(m,+j)^ + IncPointer(a,+j)^;
            end else begin
               a2 := IncPointer(m,+j)^;
               m2 := IncPointer(m,+j)^ - IncPointer(a,+j)^;
            end;
          end;
         IncPointer(m,+j)^ := m2;
         IncPointer(a,+j)^ := a2;
      end;
   end;

   // finish decoding the floors
{$IFNDEF STB_VORBIS_NO_DEFER_FLOOR}
   stb_prof(15);
   for i1:=0 to f.channels-1 do begin
      if really_zero_channel[i1] then begin
         //memset(f->channel_buffers[i], 0, sizeof( *f->channel_buffers[i]) * n2);
         for ii:=0 to n2-1 do IncPointer(f.channel_buffers[i1],+ii)^:=0;
      end else begin
         do_floor(f, map, i1, n, f.channel_buffers[i1], @f.finalY[i1][0], nil);
      end;
   end;
{$ELSE}
   for i1:=0 to f.channels-1 do begin
      if really_zero_channel[i1] then begin
         //memset(f->channel_buffers[i], 0, sizeof( *f->channel_buffers[i]) * n2);
         for ii:=0 to n2-1 do f.channel_buffers[i1][ii]:=0;
      end  else begin
         for j:=0 to n2-1 do
            f.channel_buffers[i1][j] := f.channel_buffers[i1][j] * f.floor_buffers[i1][j];
      end;
   end;
{$ENDIF}

// INVERSE MDCT
   stb_prof(16);
   for i1:=0 to f.channels-1 do
      inverse_mdct(f.channel_buffers[i1], n, f, _mode.blockflag);
   stb_prof(0);

   // this shouldn't be necessary, unless we exited on an error
   // and want to flush to get to the next packet
   flush_packet(f);

   if f.first_decode then begin
      // assume we start so first non-discarded sample is sample 0
      // this isn't to spec, but spec would require us to read ahead
      // and decode the size of all current frames--could be done,
      // but presumably it's not a commonly used feature
      f.current_loc := -n2; // start of first frame is positioned for discard
      // we might have to discard samples "from" the next frame too,
      // if we're lapping a large block then a small at the start?
      f.discard_samples_deferred := n - right_end;
      f.current_loc_valid := true;
      f.first_decode := false;
   end else if f.discard_samples_deferred<>0 then begin
      left_start := left_start + f.discard_samples_deferred;
      p_left := left_start;
      f.discard_samples_deferred := 0;
   end else if (f.previous_length=0) and f.current_loc_valid then begin
      // we're recovering from a seek... that means we're going to discard
      // the samples from this packet even though we know our position from
      // the last page header, so we need to update the position based on
      // the discarded samples here
      // but wait, the code below is going to add this in itself even
      // on a discard, so we don't need to do it here...
   end;

   // check if we have ogg information about the sample # for this packet
   if f.last_seg_which=f.end_seg_with_known_loc then begin
      // if we have a valid current loc, and this is final:
      if f.current_loc_valid and ((f.page_flag and PAGEFLAG_last_page)<>0) then begin
         current_end := f.known_loc_for_packet - (n-right_end);
         // then let's infer the size of the (probably) short final frame
         if current_end<f.current_loc+right_end then begin
            if current_end<f.current_loc then begin
               // negative truncation, that's impossible!
               len := 0;
            end else begin
               len := current_end - f.current_loc;
            end;
            len := len + left_start;
            f.current_loc := f.current_loc + len;
            Result:=true;
            Exit;
         end;
      end;
      // otherwise, just set our sample loc
      // guess that the ogg granule pos refers to the _middle_ of the
      // last frame?
      // set f->current_loc to the position of left_start
      f.current_loc := f.known_loc_for_packet - (n2-left_start);
      f.current_loc_valid := true;
   end;
   if f.current_loc_valid then
      f.current_loc := f.current_loc + (right_start - left_start);

   if f.alloc.alloc_buffer<>nil then 
      assert(f.alloc.alloc_buffer_length_in_bytes=f.temp_offset);
   len := right_end;  // ignore samples after the window goes to 0
   
   
   Result:=true;
end;

function vorbis_decode_packet(f:pvorb; var len,p_left,p_right: integer):Boolean;
var mode, left_end, right_end: integer;
begin
   Result:=false;
   mode:=0;
   left_end:=0;
   right_end:=0;
   if not vorbis_decode_initial(f, p_left, left_end, p_right, right_end, mode) then Exit;
   Result:=vorbis_decode_packet_rest(f, len, f.mode_config[mode], p_left, left_end, p_right, right_end, p_left);
end;

function vorbis_finish_frame(f:pvorb; len,left,right:integer):integer;
var
   prev,i,j,n: integer;
   w: psingle;
begin
   // we use right&left (the start of the right- and left-window sin()-regions)
   // to determine how much to return, rather than inferring from the rules
   // (same result, clearer code); 'left' indicates where our sin() window
   // starts, therefore where the previous window's right edge starts, and
   // therefore where to start mixing from the previous buffer. 'right'
   // indicates where our sin() ending-window starts, therefore that's where
   // we start saving, and where our returned-data ends.

   // mixin from previous window
   if f.previous_length<>0 then begin
      n := f.previous_length;
      w := get_window(f, n);
      for i:=0 to f.channels-1 do
         for j:=0 to n-1 do 
            IncPointer(f.channel_buffers[i],+left+j)^ :=
               IncPointer(f.channel_buffers[i],+left+j)^*IncPointer(w,+j)^ +
               IncPointer(f.previous_window[i],      j)^*IncPointer(w,+n-1-j)^;
   end;

   prev := f.previous_length;

   // last half of this data becomes previous window
   f.previous_length := len - right;

   // OPTIMIZE: could avoid this copy by double-buffering the
   // output (flipping previous_window with channel_buffers), but
   // then previous_window would have to be 2x as large, and
   // channel_buffers couldn't be temp mem (although they're NOT
   // currently temp mem, they could be (unless we want to level
   // performance by spreading out the computation))
   for i:=0 to  f.channels-1 do begin
      j:=0;
      while right+j<len do begin
         IncPointer(f.previous_window[i],+j)^ := IncPointer(f.channel_buffers[i],+right+j)^;
         Inc(j);
      end;
   end;

   if prev=0 then begin
      // there was no previous packet, so this data isn't valid...
      // this isn't entirely true, only the would-have-overlapped data
      // isn't valid, but this seems to be what the spec requires
      Result:=0;
      Exit;
   end;

   // truncate a short frame
   if len<right then  right := len;

   f.samples_output := f.samples_output + (right-left);

   Result := right - left;
end;

procedure vorbis_pump_first_frame(f:pvorb);
var
   len,right,left: integer;

begin
   if vorbis_decode_packet(f, len, left, right) then
      vorbis_finish_frame(f, len, left, right);
end;

function is_whole_packet_present(f:pvorb; end_page: Boolean):Boolean;
var
   s,n: integer;
   first: boolean;
   p,q: pByte;
begin
   // make sure that we have the packet available before continuing...
   // this requires a full ogg parse, but we know we can fetch from f->stream

   // instead of coding this out explicitly, we could save the current read state,
   // read the next packet with get8() until end-of-packet, check f->eof, then
   // reset the state? but that would be slower, esp. since we'd have over 256 bytes
   // of state to restore (primarily the page segment table)

   s := f.next_seg;
   first := true;
   p := f.stream;

   if s <> -1 then begin  // if we're not starting the packet with a 'continue on next page' flag
      while s<f.segment_count do begin
         p := IncPointer(p, + f.segments[s]);
         if f.segments[s] < 255 then break; // stop at first short segment
         Inc(s);   
      end;
      // either this continues, or it ends it...
      if end_page then
         if s<f.segment_count-1 then begin
            Result:=error(f, VORBIS_invalid_stream);
            Exit;
         end;
      if s=f.segment_count then s:=-1; // set 'crosses page' flag
      if PtrUInt(p)>PtrUInt(f.stream_end) then begin
         Result:=error(f, VORBIS_need_more_data);
         Exit;
      end;
      first := false;
   end;
   while s=-1 do begin
      // check that we have the page header ready
      if PtrUInt(p)+26>=PtrUInt(f.stream_end) then begin
         Result:=error(f, VORBIS_need_more_data);
         Exit;
      end;
      // validate the page
      if CompareMemRange(p,@ogg_page_header[0],4)<>0 then begin
         Result:=error(f, VORBIS_invalid_stream);
         Exit;
      end;
      if IncPointer(p,+4)^ <> 0 then begin
         Result:=error(f, VORBIS_invalid_stream);
         Exit;
      end;
      if first then begin // the first segment must NOT have 'continued_packet', later ones MUST
         if f.previous_length<>0 then
            if (IncPointer(p,+5)^ and PAGEFLAG_continued_packet)<>0 then begin
               Result:=error(f, VORBIS_invalid_stream);
               Exit;
            end;
         // if no previous length, we're resynching, so we can come in on a continued-packet,
         // which we'll just drop
      end else begin
         if (IncPointer(p,+5)^ and PAGEFLAG_continued_packet)=0 then begin
            Result:=error(f, VORBIS_invalid_stream);
            Exit;
         end;
      end;
      n := IncPointer(p,+26)^; // segment counts
      q := IncPointer(p,+27);  // q points to segment table
      p := IncPointer(q, +n); // advance past header
      // make sure we've read the segment table
      if PtrUInt(p)>PtrUInt(f.stream_end) then begin
         Result:=error(f, VORBIS_need_more_data);
         Exit;
      end;
      for s:=0 to n-1 do begin
         p := IncPointer(p, IncPointer(q,+s)^);
         if IncPointer(q,+s)^<255 then break;
      end;
      if end_page then
         if s<n-1 then begin
            Result:=error(f, VORBIS_invalid_stream);
            Exit;
         end;
      if s=f.segment_count then s:=-1; // set 'crosses page' flag
      if PtrUInt(p)>PtrUInt(f.stream_end) then begin
         Result:=error(f, VORBIS_need_more_data);
         Exit;
      end;
      first := false;
   end;
   Result:=true;
end;

function start_decoder(f:pvorb):Boolean;
label _skip;
var
   header:Array [0..5] of Byte;
   x,y: Byte;
   len,i,j,k,max_submaps,longest_floorlist,
   log0,log1,ordered,sorted_count,total,
   current_entry,current_length,limit,n,
   present,ii,q,sparse,z,_div,off,
   max_class,c,low,hi,temp,classwords,
   mapping_type,max_part_read,n_read,part_read: integer;
   values: array of Cardinal;
   lengths: BArray;
   size,zz,imdct_mem,classify_mem: Cardinal;
   mults:array of Word;
   //p: array [0..31*8+2-1] of Point;
   p: array of Point;
   residue_cascade: array [0..64-1] of Byte;
   high_bits,low_bits: Byte;
   cb:PCodeBook;
   r: PResidue;
   m: PMapping; 
   g0: PFloor0;
   g1: PFloor1;
   mm:PMode;     

begin
   max_submaps:=0;
   longest_floorlist:=0;

   // first page, first packet

   If not start_page(f) then
   Begin
    Result := false;
    Exit;
   End;
   
   FillChar(residue_cascade, SizeOf(residue_cascade), 0);
   cb := nil;

   // validate page flag
   if (f.page_flag and PAGEFLAG_first_page)=0 then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   if (f.page_flag and PAGEFLAG_last_page)<>0 then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   if (f.page_flag and PAGEFLAG_continued_packet)<>0 then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   // check for expected packet length
   if (f.segment_count <> 1) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   if (f.segments[0] <> 30) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;

   // read packet
   // check packet header
   if (get8(f) <> VORBIS_packet_id) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   if (not getn(f, header, 6)) then begin
      result:=error(f, VORBIS_unexpected_eof);
      Exit;
   end;
   if (not vorbis_validate(@header)) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   // vorbis_version
   if (get32(f) <> 0) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   f.channels := get8(f);
   if (f.channels=0) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   if (f.channels > STB_VORBIS_MAX_CHANNELS) then begin
      result:=error(f, VORBIS_too_many_channels);
      Exit;
   end;
   f.sample_rate := get32(f);
   if (f.sample_rate=0) then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;
   get32(f); // bitrate_maximum
   get32(f); // bitrate_nominal
   get32(f); // bitrate_minimum
   x := get8(f);

   log0 := x and 15;
   log1 := x shr 4;
   f.blocksize_0 := 1 shl log0;
   f.blocksize_1 := 1 shl log1;
   if (log0 < 6) or (log0 > 13) then begin
      result:=error(f, VORBIS_invalid_setup);
      Exit;
   end;
   if (log1 < 6) or (log1 > 13) then begin
      result:=error(f, VORBIS_invalid_setup);
      Exit;
   end;
   if (log0 > log1) then begin
      result:=error(f, VORBIS_invalid_setup);
      Exit;
   end;

   // framing_flag
   x := get8(f);
   if (x and 1)=0 then begin
      result:=error(f, VORBIS_invalid_first_page);
      Exit;
   end;

   // second packet!
   if not start_page(f) then
   begin
    Result:=false; Exit;
   end;

   if not start_packet(f) then begin Result:=false; Exit; end;
   repeat
      len := next_segment(f);
      skip(f, len);
      f.bytes_in_seg := 0;
   until len=0;

   // third packet!
   if not start_packet(f) then begin Result:=false; Exit; end;

   //crc32_init(); // always init it, to avoid multithread race conditions

   if get8_packet(f)<>VORBIS_packet_setup then begin
      result:=error(f, VORBIS_invalid_setup);
      Exit;
   end;

   for i:=0 to 5 do
    header[i] := get8_packet(f);

   if not vorbis_validate(@header) then begin
      result:=error(f, VORBIS_invalid_setup);
      Exit;
   end;

   // codebooks
   f.codebook_count := get_bits(f,8) + 1;
   //f->codebooks = (Codebook *) setup_malloc(f, sizeof( *f->codebooks) * f->codebook_count);
   setlength(f.codebooks,f.codebook_count);
   //if (f->codebooks == NULL)                        return error(f, VORBIS_outofmem);
   //memset(f->codebooks, 0, sizeof( *f->codebooks) * f->codebook_count);
   For i:=0 to Pred(f.codebook_count) Do
   Begin
      total:=0;
      cb := @f.codebooks[i];

//      Log(logDebug, 'OGG', 'X '+ IntegerProperty.Stringify(I)+' / ' + IntegerProperty.Stringify(Pred(f.codebook_count)));

      x := get_bits(f, 8); if (x <> $42) then begin Result:=error(f, VORBIS_invalid_setup); Exit; end;
      x := get_bits(f, 8); if (x <> $43) then begin Result:=error(f, VORBIS_invalid_setup); Exit; end;
      x := get_bits(f, 8); if (x <> $56) then begin Result:=error(f, VORBIS_invalid_setup); Exit; end;
      x := get_bits(f, 8);
      cb.dimensions := (get_bits(f, 8) shl 8) + x;
      x := get_bits(f, 8);
      y := get_bits(f, 8);
      cb.entries := (get_bits(f, 8) shl 16) + (y shl 8) + x;
      ordered := get_bits(f,1);
      if ordered<>0 then cb.sparse:=0 else cb.sparse:=get_bits(f,1);

      if cb.sparse<>0 then begin
         //lengths = (Byte *) setup_temp_malloc(f, c->entries);
         setlength(lengths,cb.entries);
      end else begin
         //lengths = c->codeword_lengths = (Byte *) setup_malloc(f, c->entries);
         setlength(cb.codeword_lengths,cb.entries);
         lengths:=cb.codeword_lengths;
      end;

      if ordered<>0 then begin
         current_entry := 0;
         current_length := get_bits(f,5) + 1;
         while current_entry<cb.entries do begin
            limit := cb.entries - current_entry;
            n := get_bits(f, ilog(limit));
            if current_entry+n > cb.entries then begin
               Result:=error(f, VORBIS_invalid_setup);
               Exit;
            end;
            //memset(lengths + current_entry, current_length, n);
            for ii:=0 to n-1 do lengths[current_entry+ii]:=current_length;
            current_entry := current_entry + n;
            Inc(current_length);
         end;
      end else begin
         for j:=0 to cb.entries-1 do begin
            if cb.sparse<>0 then present:=get_bits(f,1) else present:=1;
            if present<>0 then begin
               lengths[j] := get_bits(f, 5) + 1;
               Inc(total);
            end else begin
               lengths[j] := NO_CODE;
            end;
         end;
      end;

      if (cb.sparse<>0) and (total>=(cb.entries shr 2)) then begin
         // convert sparse items to non-sparse!
         if cb.entries>f.setup_temp_memory_required then
            f.setup_temp_memory_required := cb.entries;

         //c->codeword_lengths = (Byte *) setup_malloc(f, c->entries);
         //memcpy(c->codeword_lengths, lengths, c->entries);
         setlength(cb.codeword_lengths,cb.entries);
         for ii:=0 to cb.entries-1 do cb.codeword_lengths[ii]:=lengths[ii];
         //setup_temp_free(f, lengths, c->entries); // note this is only safe if there have been no intervening temp mallocs!
         lengths := cb.codeword_lengths;
         cb.sparse := 0;
      end;

      // compute the size of the sorted tables
      if cb.sparse<>0 then begin
         sorted_count := total;
         //assert(total != 0);
      end else begin
         sorted_count := 0;
         {$IFNDEF STB_VORBIS_NO_HUFFMAN_BINARY_SEARCH}
         for j:=0 to cb.entries-1 do
            if (lengths[j]>STB_VORBIS_FAST_HUFFMAN_LENGTH) and (lengths[j] <> NO_CODE) then
               Inc(sorted_count);
         {$ENDIF}
      end;

      cb.sorted_entries := sorted_count;
      setlength(values,0);

      if cb.sparse=0 then begin
         //c->codewords = (Cardinal *) setup_malloc(f, sizeof(c->codewords[0]) * c->entries);
         setlength(cb.codewords,cb.entries);
      end else begin
         if cb.sorted_entries<>0 then begin
            //c->codeword_lengths = (Byte *) setup_malloc(f, c->sorted_entries);
            setlength(cb.codeword_lengths,cb.sorted_entries);
            //c->codewords = (Cardinal *) setup_temp_malloc(f, sizeof( *c->codewords) * c->sorted_entries);
            setlength(cb.codewords,cb.sorted_entries);
            //values = (Cardinal *) setup_temp_malloc(f, sizeof( *values) * c->sorted_entries);
            setlength(values,cb.sorted_entries);
         end;
         //size = c->entries + (sizeof( *c->codewords) + sizeof( *values)) * c->sorted_entries;
         size := cb.entries + (sizeof(Cardinal) + sizeof(Cardinal)) * cb.sorted_entries;
         if size>f.setup_temp_memory_required then
            f.setup_temp_memory_required := size;
      end;

      if not compute_codewords(cb, lengths, cb.entries, values) then begin
         //if (c->sparse) setup_temp_free(f, values, 0);
         if cb.sparse<>0 then setlength(values,0);
         Result:=error(f, VORBIS_invalid_setup);
         Exit;
      end;

      if cb.sorted_entries<>0 then begin
         // allocate an extra slot for sentinels
         //c->sorted_codewords = (Cardinal *) setup_malloc(f, sizeof( *c->sorted_codewords) * (c->sorted_entries+1));
         setlength(cb.sorted_codewords,cb.sorted_entries+1);
         // allocate an extra slot at the front so that c->sorted_values[-1] is defined
         // so that we can catch that case without an extra if
         //c->sorted_values    = ( int   *) setup_malloc(f, sizeof( *c->sorted_values   ) * (c->sorted_entries+1));
         setlength(cb.sorted_values,cb.sorted_entries+1);
         //if (c->sorted_values) { ++c->sorted_values; c->sorted_values[-1] = -1; }
         compute_sorted_huffman(cb, lengths, values);
      end;

      if cb.sparse<>0 then begin
         //setup_temp_free(f, values, sizeof( *values)*c->sorted_entries);
         //setup_temp_free(f, c->codewords, sizeof( *c->codewords)*c->sorted_entries);
         //setup_temp_free(f, lengths, c->entries);
         //c->codewords = NULL;
         setlength(values,0);
         setlength(cb.codewords,0);
         setlength(lengths,0);
      end;

      compute_accelerated_huffman(cb);

      cb.lookup_type := get_bits(f, 4);
      if cb.lookup_type > 2 then
      begin
        Result := error(f, VORBIS_invalid_setup);
        Exit;
      end;

      If cb.lookup_type > 0 then
      begin
         cb.minimum_value := float32_unpack(get_bits(f, 32));
         cb.delta_value := float32_unpack(get_bits(f, 32));
         cb.value_bits := get_bits(f, 4)+1;
         cb.sequence_p := get_bits(f,1);

         if cb.lookup_type=1 Then
         Begin
            cb.lookup_values := lookup1_values(cb.entries, cb.dimensions);
         end else
         Begin
            cb.lookup_values := cb.entries * cb.dimensions;
         End;
         //mults = (Word *) setup_temp_malloc(f, sizeof(mults[0]) * c->lookup_values);
         setlength(mults, cb.lookup_values);

         for j:=0 to Pred(cb.lookup_values) do
         begin
            q := get_bits(f, cb.value_bits);
            if q=EOP then
            Begin
               //setup_temp_free(f,mults,sizeof(mults[0])*c->lookup_values);
               setlength(mults,0);
               Result := error(f, VORBIS_invalid_setup);
               Exit;
            end;
            mults[j] := q;
         end;

{$IFNDEF STB_VORBIS_DIVIDES_IN_CODEBOOK}
         if cb.lookup_type=1 then begin
            sparse := cb.sparse;
            // pre-expand the lookup1-style multiplicands, to avoid a divide in the inner loop
            if sparse<>0 then begin
               if cb.sorted_entries=0 then goto _skip;
               //c->multiplicands = (codetype *) setup_malloc(f, sizeof(c->multiplicands[0]) * c->sorted_entries * c->dimensions);
               setlength(cb.multiplicands,cb.sorted_entries*cb.dimensions);
            end else
               //c->multiplicands = (codetype *) setup_malloc(f, sizeof(c->multiplicands[0]) * c->entries * c->dimensions);
               setlength(cb.multiplicands,cb.entries*cb.dimensions);
            //if (c->multiplicands == NULL) { setup_temp_free(f,mults,sizeof(mults[0])*c->lookup_values); return error(f, VORBIS_outofmem); }
            if sparse<>0 then len:=cb.sorted_entries else len:=cb.entries;
            for j:=0 to len-1 do begin
               if sparse<>0 then z:=cb.sorted_values[j] else z:=j;
               _div:=1;
               for k:=0 to cb.dimensions-1 do begin
                  //off := (z / _div) mod cb.lookup_values;
                  off := (z div _div) mod cb.lookup_values;
                  cb.multiplicands[j*cb.dimensions + k] :=
                         {$IFNDEF STB_VORBIS_CODEBOOK_FLOATS}
                            mults[off];
                         {$ELSE}
                            mults[off]*cb.delta_value + cb.minimum_value;
                            // in this case (and this case only) we could pre-expand c->sequence_p,
                            // and throw away the decode logic for it; have to ALSO do
                            // it in the case below, but it can only be done if
                            //    STB_VORBIS_CODEBOOK_FLOATS
                            //   !STB_VORBIS_DIVIDES_IN_CODEBOOK
                         {$ENDIF}
                  _div := _div * cb.lookup_values;
               end;
            end;
            //setup_temp_free(f, mults,sizeof(mults[0])*c->lookup_values);
            setlength(mults,0);
            cb.lookup_type := 2;
         end
         else
{$ENDIF}
         begin
            //c->multiplicands = (codetype *) setup_malloc(f, sizeof(c->multiplicands[0]) * c->lookup_values);
            setlength(cb.multiplicands,cb.lookup_values);
            {$IFNDEF STB_VORBIS_CODEBOOK_FLOATS}
            //memcpy(c->multiplicands, mults, sizeof(c->multiplicands[0]) * c->lookup_values);
            for ii:=0 to cb.lookup_values-1 do cb.multiplicands[ii]:=mults[ii];
            {$ELSE}
            for j:=0 to cb.lookup_values-1 do
               cb.multiplicands[j] := mults[j] * cb.delta_value + cb.minimum_value;
            //setup_temp_free(f, mults,sizeof(mults[0])*c->lookup_values);
            setlength(mults,0);
            {$ENDIF}
         end;
_skip:

         {$IFDEF STB_VORBIS_CODEBOOK_FLOATS}
         If (cb.lookup_type=2) and (cb.sequence_p<>0) Then
         Begin
            for j:=1 to cb.lookup_values-1 do
               cb.multiplicands[j] := cb.multiplicands[j-1];
            cb.sequence_p := 0;
         End;
         {$ENDIF}
      end;

   End;

   // time domain transfers (notused)

   x := get_bits(f, 6) + 1;
   for i:=0 to x-1 do begin
      zz := get_bits(f, 16);
      if zz<>0 then begin Result:=error(f, VORBIS_invalid_setup); Exit; end;
   end;

   // Floors
   f.floor_count := get_bits(f, 6)+1;
   //f.floor_config = (Floor *)  setup_malloc(f, f->floor_count * sizeof( *f->floor_config));
   setlength(f.floor_config,f.floor_count);
   for i:=0 to f.floor_count-1 do begin
      f.floor_types[i] := get_bits(f, 16);
      if f.floor_types[i]>1 then begin result:=error(f, VORBIS_invalid_setup); Exit; end;
      if f.floor_types[i]=0 then begin
         g0 := @f.floor_config[i].floor0;
         g0.order := get_bits(f,8);
         g0.rate := get_bits(f,16);
         g0.bark_map_size := get_bits(f,16);
         g0.amplitude_bits := get_bits(f,6);
         g0.amplitude_offset := get_bits(f,8);
         g0.number_of_books := get_bits(f,4) + 1;
         for j:=0 to g0.number_of_books-1 do
            g0.book_list[j] := get_bits(f,8);
         result:=error(f, VORBIS_feature_not_supported);
         Exit;
      end else begin
         g1 := @f.floor_config[i].floor1;
         max_class := -1; 
         g1.partitions := get_bits(f, 5);
         for j:=0 to g1.partitions-1 do begin
            g1.partition_class_list[j] := get_bits(f, 4);
            if g1.partition_class_list[j] > max_class then
               max_class := g1.partition_class_list[j];
         end;
         for j:=0 to max_class do begin
            g1.class_dimensions[j] := get_bits(f, 3)+1;
            g1.class_subclasses[j] := get_bits(f, 2);
            if g1.class_subclasses[j]<>0 then begin
               g1.class_masterbooks[j] := get_bits(f, 8);
               if g1.class_masterbooks[j]>=f.codebook_count then begin 
                  Result:=error(f, VORBIS_invalid_setup);
                  Exit;
               end;
            end;
            for k:=0 to (1 shl g1.class_subclasses[j])-1 do begin
               g1.subclass_books[j][k] := SmallInt(get_bits(f,8))-1;
               if g1.subclass_books[j][k]>=f.codebook_count then begin
                  result:=error(f, VORBIS_invalid_setup);
                  Exit;
               end;
            end;
         end;
         g1.floor1_multiplier := get_bits(f,2)+1;
         g1.rangebits := get_bits(f,4);
         g1.Xlist[0] := 0;
         g1.Xlist[1] := 1 shl g1.rangebits;
         g1.values := 2;
         for j:=0 to g1.partitions-1 do begin
            c := g1.partition_class_list[j];
            for k:=0 to g1.class_dimensions[c]-1 do begin
               g1.Xlist[g1.values] := get_bits(f, g1.rangebits);
               Inc(g1.values);
            end;
         end;
         // precompute the sorting
         setlength(p,g1.values);
         for j:=0 to g1.values-1 do begin
            p[j].x := g1.Xlist[j];
            p[j].y := j;
         end;
         //qsort(p, g->values, sizeof(p[0]), point_compare);
         qsort_points(p, length(p));
         for j:=0 to g1.values-1 do
            g1.sorted_order[j] := p[j].y;
         // precompute the neighbors
         for j:=2 to g1.values-1 do begin
            neighbors(g1.Xlist, j, low, hi);
            g1.neighbors[j][0] := low;
            g1.neighbors[j][1] := hi;
         end;

         if g1.values>longest_floorlist then
            longest_floorlist := g1.values;
      end;
   end;

   // Residue
   f.residue_count := get_bits(f, 6)+1;
   //f->residue_config = (Residue *) setup_malloc(f, f->residue_count * sizeof( *f->residue_config));
   setlength(f.residue_config,f.residue_count);
   for i:=0 to f.residue_count-1 do begin
      r := @f.residue_config[i];
      f.residue_types[i] := get_bits(f, 16);
      if f.residue_types[i]>2 then begin result:=error(f, VORBIS_invalid_setup); Exit; end;
      r._begin := get_bits(f, 24);
      r._end := get_bits(f, 24);
      r.part_size := get_bits(f,24)+1;
      r.classifications := get_bits(f,6)+1;
      r.classbook := get_bits(f,8);
      for j:=0 to r.classifications-1 do begin
         high_bits:=0;
         low_bits:=get_bits(f,3);
         if get_bits(f,1)<>0 then high_bits:=get_bits(f,5);
         residue_cascade[j] := high_bits*8 + low_bits;
      end;
      //r->residue_books = (short ( *)[8]) setup_malloc(f, sizeof(r->residue_books[0]) * r->classifications);
      setlength(r.residue_books, r.classifications);
      for j:=0 to r.classifications-1 do begin
         for k:=0 to 8-1 do begin
            if (residue_cascade[j] and (1 shl k))<>0 then begin
               r.residue_books[j][k] := get_bits(f, 8);
               if r.residue_books[j][k]>=f.codebook_count then begin
                  result:=error(f, VORBIS_invalid_setup);
                  Exit;
               end;              
            end else begin
               r.residue_books[j][k] := -1;
            end;
         end;
      end;
      // precompute the classifications[] array to avoid inner-loop mod/divide
      // call it 'classdata' since we already have r->classifications
      //r->classdata = (Byte **) setup_malloc(f, sizeof( *r->classdata) * f->codebooks[r->classbook].entries);
      setlength(r.classdata, f.codebooks[r.classbook].entries);
      //if (!r->classdata) return error(f, VORBIS_outofmem);
      //memset(r->classdata, 0, sizeof( *r->classdata) * f->codebooks[r->classbook].entries);
      for j:=0 to f.codebooks[r.classbook].entries-1 do begin
         classwords := f.codebooks[r.classbook].dimensions;
         temp := j;
         //r->classdata[j] = (Byte *) setup_malloc(f, sizeof(r->classdata[j][0]) * classwords);
         setlength(r.classdata[j], classwords);
         for k:=classwords-1 downto 0 do begin
            r.classdata[j][k] := temp mod r.classifications;
            temp := temp div r.classifications;
         end;
      end;
   end;

   f.mapping_count := get_bits(f,6)+1;
   //f->mappingmapping = (Mapping *) setup_malloc(f, f->mapping_count * sizeof( *f->mapping));
   setlength(f.mapping, f.mapping_count);
   for i:=0 to f.mapping_count-1 do begin
      m := @f.mapping[i];      
      mapping_type := get_bits(f,16);
      if mapping_type<>0 then begin result:=error(f, VORBIS_invalid_setup); Exit; end;
      //m->chan = (MappingChannel *) setup_malloc(f, f->channels * sizeof( *m->chan));
      setlength(m.chan, f.channels);
      if get_bits(f,1)<>0 then m.submaps := get_bits(f,4)
      else m.submaps := 1;
      if m.submaps>max_submaps then max_submaps:=m.submaps;
      if get_bits(f,1)<>0 then begin
         m.coupling_steps := get_bits(f,8)+1;
         for k:=0 to m.coupling_steps-1 do begin
            m.chan[k].magnitude := get_bits(f, ilog(f.channels)-1);
            m.chan[k].angle := get_bits(f, ilog(f.channels)-1);
            if (m.chan[k].magnitude >= f.channels) then begin result:=error(f, VORBIS_invalid_setup); exit; end;
            if (m.chan[k].angle     >= f.channels) then begin result:=error(f, VORBIS_invalid_setup); exit; end;
            if (m.chan[k].magnitude = m.chan[k].angle) then begin result:=error(f, VORBIS_invalid_setup); exit; end;
         end;
      end else
         m.coupling_steps := 0;

      // reserved field
      if get_bits(f,2)<>0 then begin result:=error(f, VORBIS_invalid_setup); exit; end;
      if m.submaps > 1 then begin
         for j:=0 to f.channels-1 do begin
            m.chan[j].mux := get_bits(f, 4);
            if m.chan[j].mux >= m.submaps then begin result:=error(f, VORBIS_invalid_setup); exit; end;
         end;
      end else
         // SPECIFICATION: this case is missing from the spec
         for j:=0 to f.channels-1 do
            m.chan[j].mux := 0;

      for j:=0 to m.submaps-1 do begin
         get_bits(f,8); // discard
         m.submap_floor[j] := get_bits(f,8);
         m.submap_residue[j] := get_bits(f,8);
         if m.submap_floor[j] >= f.floor_count then begin result:=error(f, VORBIS_invalid_setup); exit; end;
         if m.submap_residue[j] >= f.residue_count then begin result:=error(f, VORBIS_invalid_setup); exit; end;
      end;
   end;

   // Modes
   f.mode_count := get_bits(f, 6)+1;
   for i:=0 to f.mode_count-1 do begin
      mm := @f.mode_config[i];
      mm.blockflag := get_bits(f,1);
      mm.windowtype := get_bits(f,16);
      mm.transformtype := get_bits(f,16);
      mm.mapping := get_bits(f,8);
      if mm.windowtype <> 0  then begin result:=error(f, VORBIS_invalid_setup); exit; end;
      if mm.transformtype <> 0 then begin result:=error(f, VORBIS_invalid_setup); exit; end;
      if mm.mapping >= f.mapping_count then begin result:=error(f, VORBIS_invalid_setup); exit; end;
   end;

   flush_packet(f);

   f.previous_length := 0;

   For i:=0 to Pred(F.channels) do
   Begin             
      //f->channel_buffers[i] = (float *) setup_malloc(f, sizeof(float) * f->blocksize_1);
      getMem(f.channel_buffers[i], f.blocksize_1*sizeof(single));
      //f->previous_window[i] = (float *) setup_malloc(f, sizeof(float) * f->blocksize_1/2);
      getMem(f.previous_window[i], sizeof(single)*f.blocksize_1 div 2);
      //f->finalY[i]          = (SmallInt *) setup_malloc(f, sizeof(SmallInt) * longest_floorlist);
      setlength(f.finalY[i], longest_floorlist);
      {$IFDEF STB_VORBIS_NO_DEFER_FLOOR}
      //f->floor_buffers[i]   = (float *) setup_malloc(f, sizeof(float) * f->blocksize_1/2);
      getMem(f.floor_buffers[i], sizeof(single)*f.blocksize_1 div 2);
      {$ENDIF}
   End;

   if not init_blocksize(f, 0, f.blocksize_0) then begin result:=false; Exit; end;
   if not init_blocksize(f, 1, f.blocksize_1) then begin result:=false; Exit; end;
   f.blocksize[0] := f.blocksize_0;
   f.blocksize[1] := f.blocksize_1;

{$IFDEF STB_VORBIS_DIVIDE_TABLE}
   if integer_divide_table[1][1]=0 then
      for i:=0 to DIVTAB_NUMER-1 do
         for j:=1 to DIVTAB_DENOM-1 do
            integer_divide_table[i][j] := i / j;
{$ENDIF}

   // compute how much temporary memory is needed

   // 1.
   imdct_mem := (f.blocksize_1 * sizeof(single) shr 1);
   max_part_read:=0;
   for i:=0 to f.residue_count-1 do begin
      r := @f.residue_config[i];
      n_read := r._end - r._begin;
      part_read := n_read div r.part_size;
      if part_read>max_part_read then max_part_read:=part_read;
   end;

   {$IFNDEF STB_VORBIS_DIVIDES_IN_RESIDUE}
   classify_mem := f.channels * (sizeof(pointer) + max_part_read * sizeof(pByte));
   {$ELSE}
   classify_mem := f.channels * (sizeof(pointer) + max_part_read * sizeof(pint));
   {$ENDIF}

   f.temp_memory_required := classify_mem;
   if imdct_mem>f.temp_memory_required then f.temp_memory_required:=imdct_mem;

   f.first_decode:=true;

   if f.alloc.alloc_buffer<>nil then begin
      assert(f.temp_offset=f.alloc.alloc_buffer_length_in_bytes);
      // check if there's enough temp memory so we don't error later
      if f.setup_offset+sizeof(f^)+f.temp_memory_required>f.temp_offset then begin
         result:=error(f, VORBIS_outofmem);
         Exit;
      end;
   end;

   f.first_audio_page_offset := stb_vorbis_get_file_offset(f);

   result:=true;
end;

procedure vorbis_deinit(p:pvorb);
var
   i,j: integer;//,ii
   r: residue;
   cb:codebook;
begin
   for i:=0 to p.residue_count-1 do begin
      r := p.residue_config[i];
      for j:=0 to High(r.classdata) do
         setlength(r.classdata[j],0);
      setlength(r.classdata,0);

      setlength(r.residue_books,0);
   end;

   for i:=0 to High(p.codebooks) do begin
      cb := p.codebooks[i];
      setlength(cb.codeword_lengths,0);
      setlength(cb.multiplicands,0);
      setlength(cb.codewords,0);
      setlength(cb.sorted_codewords,0);
      setlength(cb.sorted_values,0);
   end;
   setlength(p.codebooks,0);

   setlength(p.floor_config,0);
   setlength(p.residue_config,0);
   for i:=0 to High(p.mapping) do
      setlength(p.mapping[i].chan,0);
   setlength(p.mapping,0);

   For i:=0 to Pred(p.channels) do
   Begin
      freeMem(p.channel_buffers[i]);
      freeMem(p.previous_window[i]);
      {$IFDEF STB_VORBIS_NO_DEFER_FLOOR}
      freeMem(p.floor_buffers[i]);
      {$ENDIF}
      setlength(p.finalY[i],0);
   end;

   For i:=0 to 1 Do
   Begin
      SetLength(p.A[i],0);
      SetLength(p.B[i],0);
      SetLength(p.C[i],0);
      SetLength(p.window[i],0);
      SetLength(p.bit_reverse[i],0);
   End;
End;

procedure stb_vorbis_close(f:pvorb);
begin
   If f=nil Then
    exit;

   vorbis_deinit(f);
   Dispose(f);
   //setup_free(p,p);
End;

procedure vorbis_init(p:pvorb; z: pstb_vorbis_alloc);
begin
//   memset(p, 0, sizeof( *p)); // NULL out all malloc'd pointers to start
   if z<>nil then begin
      p.alloc := z^;
      p.alloc.alloc_buffer_length_in_bytes := (p.alloc.alloc_buffer_length_in_bytes+3) and (not 3);
      p.temp_offset := p.alloc.alloc_buffer_length_in_bytes;
   end else begin
      p.alloc.alloc_buffer:=nil;
      p.alloc.alloc_buffer_length_in_bytes := 0;
      p.temp_offset := p.alloc.alloc_buffer_length_in_bytes;
   end;
   p.eof := false;
   p.error := VORBIS__no_error;
   p.stream := nil;
   setlength(p.codebooks,0);
   p.page_crc_tests := -1;
end;

function stb_vorbis_get_sample_offset(f:pvorb):integer;
begin
   if f.current_loc_valid then result:=f.current_loc
   else result:=-1;
end;

function stb_vorbis_get_info(f:pvorb):stb_vorbis_info;
begin
   result.channels := f.channels;
   result.sample_rate := f.sample_rate;
   result.setup_memory_required := f.setup_memory_required;
   result.setup_temp_memory_required := f.setup_temp_memory_required;
   result.temp_memory_required := f.temp_memory_required;
   result.max_frame_size := f.blocksize_1 shr 1;
end;

function stb_vorbis_get_error(f:pvorb):STBVorbisError;
begin
   Result:=f.error;
   f.error := VORBIS__no_error;
end;

//function vorbis_alloc(f:pvorb):pvorb;
function vorbis_alloc:pvorb;
begin
   //stb_vorbis *p = (stb_vorbis *) setup_malloc(f, sizeof( *p));
   New(Result);
   FillChar(Result^, SizeOf(vorb), 0);
end;

procedure stb_vorbis_flush_pushdata(f:pvorb);
begin
   f.previous_length := 0;
   f.page_crc_tests  := 0;
   f.discard_samples_deferred := 0;
   f.current_loc_valid := false;
   f.first_decode := false;
   f.samples_output := 0;
   f.channel_buffer_start := 0;
   f.channel_buffer_end := 0;
end;

//function vorbis_search_for_page_pushdata(f:pvorb; data: array of Byte; data_len:integer):integer;
function vorbis_search_for_page_pushdata(f:pvorb; data: pByte; data_len:integer):integer;
var
   i,n,j,len,m: integer;
   crc: Cardinal;
begin
   For i:=0 to Pred(f.page_crc_tests) Do
      f.scan[i].bytes_done := 0;

   // if we have room for more scans, search for them first, because
   // they may cause us to stop early if their header is incomplete
   if f.page_crc_tests<STB_VORBIS_PUSHDATA_CRC_COUNT Then
   begin
      if data_len<4 then
      begin
        result:=0;
        exit;
      end;
      data_len := data_len - 3; // need to look for 4-byte sequence, so don't miss
                     // one that straddles a boundary
      for i:=0 to data_len-1 do begin
         if IncPointer(data,+i)^=$4f then begin
            if 0=compareMemRange(IncPointer(data,+i), @ogg_page_header, 4) then begin
               // make sure we have the whole page header
               if (i+26>=data_len) or (i+27+IncPointer(data,+i+26)^>=data_len) then begin
                  // only read up to this page start, so hopefully we'll
                  // have the whole page header start next time
                  data_len := i;
                  break;
               end;
               // ok, we have it all; compute the length of the page
               len := 27 + IncPointer(data,+i+26)^;
               for j:=0 to IncPointer(data,+i+26)^-1 do
                  len := len +IncPointer(data,+i+27+j)^;
               // scan everything up to the embedded crc (which we must 0)
               crc := 0;
               for j:=0 to 22-1 do
                  {crc := }crc32_update(crc, IncPointer(data,+i+j)^);

               j := 22;
               // now process 4 0-bytes
               while j<26 do begin
                  {crc := }crc32_update(crc, 0);
                  Inc(j);
               end;
               // len is the total number of bytes we need to scan
               n := f.page_crc_tests;
               Inc(f.page_crc_tests);
               f.scan[n].bytes_left := len-j;
               f.scan[n].crc_so_far := crc;
               f.scan[n].goal_crc := IncPointer(data,+i+22)^ + (IncPointer(data,+i+23)^ shl 8) + (IncPointer(data,+i+24)^ shl 16) + (IncPointer(data,+i+25)^ shl 24);
               // if the last frame on a page is continued to the next, then
               // we can't recover the sample_loc immediately
               //if data[i+27+data[i+26]-1]=255 then
               if IncPointer(data,+i+27+IncPointer(data,+i+26)^-1)^=255 then
                  f.scan[n].sample_loc := $FFFFFFFF//(not 0)
               else
                  f.scan[n].sample_loc := IncPointer(data,+i+6)^ + (IncPointer(data,+i+7)^ shl 8) + (IncPointer(data,+i+ 8)^ shl 16) + (IncPointer(data,+i+ 9)^ shl 24);
               f.scan[n].bytes_done := i+j;
               if f.page_crc_tests=STB_VORBIS_PUSHDATA_CRC_COUNT then break;
               // keep going if we still have room for more
            end;
         end;
      end;
   end;

   i:=0;
   while i<f.page_crc_tests-1 do begin
      n := f.scan[i].bytes_done;
      m := f.scan[i].bytes_left;
      if m>data_len-n then m:=data_len-n;
      // m is the bytes to scan in the current chunk
      crc:=f.scan[i].crc_so_far;
      for j:=0 to m-1 do
         {crc := }crc32_update(crc, IncPointer(data,+n+j)^);
      f.scan[i].bytes_left := f.scan[i].bytes_left - m;
      f.scan[i].crc_so_far := crc;
      if f.scan[i].bytes_left=0 then begin
         // does it match?
         if f.scan[i].crc_so_far=f.scan[i].goal_crc then begin
            // Houston, we have page
            data_len := n+m; // consumption amount is wherever that scan ended
            f.page_crc_tests := -1; // drop out of page scan mode
            f.previous_length := 0; // decode-but-don't-output one frame
            f.next_seg := -1;       // start a new page
            f.current_loc := f.scan[i].sample_loc; // set the current sample location
                                    // to the amount we'd have decoded had we decoded this page
            f.current_loc_valid := f.current_loc <> $FFFFFFFF;//(not 0);
            result:=data_len;
            Exit;
         end;
         // delete entry
         Dec(f.page_crc_tests);
         f.scan[i] := f.scan[f.page_crc_tests];
      end else begin
         Inc(i);
      end;
   end;

   result:=data_len;
end;

// return value: number of bytes we used
function stb_vorbis_decode_frame_pushdata(
         f:pvorb;                 // the file we're decoding
         data:pByte;
         //data:array of Byte;
         data_len:integer;        // the memory available for decoding
         var channels:integer;    // place to write number of float * buffers
         output:POutput;      // place to write float ** array of float * buffers
         var samples:integer      // place to write number of output samples
     ):integer;
var
   i,len,right,left: integer;
   _error: STBVorbisError;
begin

   if not IS_PUSH_MODE(f) then begin result:=0; error(f, VORBIS_invalid_api_mixing); exit; end;

   if f.page_crc_tests>=0 then begin
      samples := 0;
      result:=vorbis_search_for_page_pushdata(f, data, data_len);
      Exit;
   end;

   f.stream     := data;
   f.stream_end := IncPointer(data,+data_len);//@data[data_len];
   f.error      := VORBIS__no_error;

   // check that we have the entire packet in memory
   if not is_whole_packet_present(f, FALSE) then begin
      samples := 0;
      result := 0;
      Exit;
   end;

   if not vorbis_decode_packet(f, len, left, right) then begin
      // save the actual error we encountered
      _error := f.error;
      if _error=VORBIS_bad_packet_type then begin
         // flush and resynch
         f.error := VORBIS__no_error;
         while get8_packet(f)<>EOP do if f.eof then break;
         samples := 0;
         result:=PtrUInt(f.stream)-PtrUInt(data);//@data[0];
         Exit;
      end;
      if _error=VORBIS_continued_packet_flag_invalid then begin
         if f.previous_length=0 then begin
            // we may be resynching, in which case it's ok to hit one
            // of these; just discard the packet
            f.error := VORBIS__no_error;
            while get8_packet(f)<>EOP do if f.eof then break;
            samples := 0;
            result:=PtrUInt(f.stream) - PtrUInt(data);//@data[0];
            Exit;
         end;
      end;
      // if we get an error while parsing, what to do?
      // well, it DEFINITELY won't work to continue from where we are!
      stb_vorbis_flush_pushdata(f);
      // restore the error that actually made us bail
      f.error := _error;
      samples := 0;
      result:=1;
      Exit;
   end;

   // success!
   len := vorbis_finish_frame(f, len, left, right);
   for i:=0 to f.channels-1 do begin
      f.outputs[i] := IncPointer(f.channel_buffers[i],+left);
      output[i]:=IncPointer(f.channel_buffers[i],+left);
   end;

   channels := f.channels;
   samples := len;
   result:=PtrUInt(f.stream) - PtrUInt(data);//@data[0];
end;

function stb_vorbis_open_pushdata(
         data: pByte;
//         data: array of Byte;
         data_len:integer;            // the memory available for decoding
         var data_used:integer;       // only defined if result is not NULL
         var _error:STBVorbisError;
         alloc:pstb_vorbis_alloc):pvorb;
var
   f: pvorb;
begin
   result := vorbis_alloc;
   if result=nil then exit;
   f:=result;

   vorbis_init(f, alloc);
   f.stream     := data;
   f.stream_end := IncPointer(data,+data_len);//@data[data_len];
   f.push_mode  := true;
   if not start_decoder(f) then begin
      if f.eof then _error := VORBIS_need_more_data
      else _error := Result.error;
      stb_vorbis_close(f);
      result:=nil;
      exit;
   end;

   data_used := PtrUInt(f.stream) - PtrUInt(data);//@data[0];
   _error := VORBIS__no_error;
end;

Function stb_vorbis_get_file_offset(f:pvorb):Cardinal;
Begin
   result := PtrUInt(f.stream) - PtrUInt(f.stream_start);
End;

// DATA-PULLING API
Function vorbis_find_page(f:pvorb; var _end,last: Cardinal):Boolean;
label invalid;
var
   n,i,s: integer;
   retry_loc, crc, goal, len:Cardinal;
   header: array [0..27-1] of Byte;
begin
   while true do begin
      if f.eof then begin result:=false; exit; end;
      n := get8(f);
      if n=$0f then begin // page header
         retry_loc := stb_vorbis_get_file_offset(f);
         // check if we're off the end of a file_section stream
         if retry_loc-25>f.stream_len then begin result:=false; exit; end;
         // check the rest of the header
         for i:=0 to 4-1 do
            if get8(f)<>ogg_page_header[i] then break;
         if f.eof then begin result:=false; exit; end;
         if i=3 then begin
            for i:=0 to 4-1 do header[i] := ogg_page_header[i];
            i := 4-1;
            while i<27 do begin
               header[i] := get8(f);
               Inc(i);
            end;  
            if f.eof then begin result:=false; exit; end;
            if header[4]<>0 then goto invalid;
            goal := header[22] + (header[23] shl 8) + (header[24] shl 16) + (header[25] shl 24);
            for i:=22 to 26-1 do header[i] := 0;
            crc := 0;
            for i:=0 to 27-1 do {crc := }crc32_update(crc, header[i]);
            len := 0;
            for i:=0 to header[26]-1 do begin
               s := get8(f);
               {crc := }crc32_update(crc, s);
               len := len + s;
            end;

            if (len<>0) and f.eof Then
            Begin
              Result := False;
              Exit;
            End;

            For I:=0 To Pred(len) Do
              {crc := }crc32_update(crc, get8(f));

            // finished parsing probable page
            If crc=goal Then
            Begin
               // we could now check that it's either got the last
               // page flag set, OR it's followed by the capture
               // pattern, but I guess TECHNICALLY you could have
               // a file with garbage between each ogg page and recover
               // from it automatically? So even though that paranoia
               // might decrease the chance of an invalid decode by
               // another 2^32, not worth it since it would hose those
               // invalid-but-useful files?
               _end := stb_vorbis_get_file_offset(f);
               if (header[5] and $04)>0 then
                last := 1
               else
                last := 0;
               set_file_offset(f, retry_loc-1);
               Result := true;
               Exit;
            End;
         End;
invalid:
         // not a valid page, so rewind and look for next one
         set_file_offset(f, retry_loc);
      end;
   end;
end;

// seek is implemented with 'interpolation search'--this is like
// binary search, but we use the data values to estimate the likely
// location of the data item (plus a bit of a bias so when the
// estimation is wrong we don't waste overly much time)

// ogg vorbis, in its insane infinite wisdom, only provides
// information about the sample at the END of the page.
// therefore we COULD have the data we need in the current
// page, and not know it. we could just use the end location
// as our only knowledge for bounds, seek back, and eventually
// the binary search finds it. or we can try to be smart and
// not waste time trying to locate more pages. we try to be
// smart, since this data is already in memory anyway, so
// doing needless I/O would be crazy!
function vorbis_analyze_page(f:pvorb; z:PProbedPage):Boolean;
label bail;
var
   header: array [0..27-1] of Byte;
   lacing: array [0..255-1] of Byte;
   packet_type: array [0..255-1] of Byte;
   num_packet,i,len: integer;//,previous
   samples: Cardinal;
   n,b: Byte;//,m
   packet_start:boolean;
begin
   //previous:=0;
   // record where the page starts
   z.page_start := stb_vorbis_get_file_offset(f);

   // parse the header
   getn(f, header, 27);
   assert((header[0]=ord('O')) and (header[1]=ord('g')) and (header[2]=ord('g')) and (header[3]=ord('S')));
   getn(f, lacing, header[26]);

   // determine the length of the payload
   len := 0;
   for i:=0 to header[26]-1 do
      len := len + lacing[i];

   // this implies where the page ends
   z.page_end := z.page_start + 27 + header[26] + len;

   // read the last-decoded sample out of the data
   z.last_decoded_sample := header[6] + (header[7] shl 8) + (header[8] shl 16) + (header[9] shl 16);

   if (header[5] and 4)<>0 Then begin
      // if this is the last page, it's not possible to work
      // backwards to figure out the first sample! whoops! fuck.
      z.first_decoded_sample := SAMPLE_unknown;
      set_file_offset(f, z.page_start);
      result:=true;
      exit;
   end;

   // scan through the frames to determine the sample-count of each one...
   // our goal is the sample # of the first fully-decoded sample on the
   // page, which is the first decoded sample of the 2nd page

   num_packet:=0;
   
   FillChar(packet_type, SizeOf(packet_type), 0);
   
   packet_start := ((header[5] and 1)=0);

   for i:=0 to header[26]-1 do begin
      if packet_start then begin
         if lacing[i]=0 then goto bail; // trying to read from zero-length packet
         n := get8(f);
         // if bottom bit is non-zero, we've got corruption
         if (n and 1)<>0 then goto bail;
         n := n shr 1;
         b := ilog(f.mode_count-1);
         n := n and ((1 shl b)-1);
         if n>=f.mode_count then goto bail;
         //if (num_packet=0) and (f.mode_config[n].blockflag<>0) then
         //   previous := m and 1;
         packet_type[num_packet] := f.mode_config[n].blockflag;
         Inc(num_packet);
         skip(f, lacing[i]-1);
      end else
         skip(f, lacing[i]);
      packet_start := (lacing[i] < 255);
   end;

   // now that we know the sizes of all the pages, we can start determining
   // how much sample data there is.

   samples := 0;

   // for the last packet, we step by its whole length, because the definition
   // is that we encoded the end sample loc of the 'last packet completed',
   // where 'completed' refers to packets being split, and we are left to guess
   // what 'end sample loc' means. we assume it means ignoring the fact that
   // the last half of the data is useless without windowing against the next
   // packet... (so it's not REALLY complete in that sense)
   if (num_packet > 1) then
      samples := samples + f.blocksize[packet_type[num_packet-1]];

   for i:=num_packet-2 downto 1 do begin
      // now, for this packet, how many samples do we have that
      // do not overlap the following packet?
      if packet_type[i]=1 then
         if packet_type[i+1]=1 then
            samples := samples + (f.blocksize_1 shr 1)
         else
            samples := samples + ((f.blocksize_1 - f.blocksize_0) shr 2) + (f.blocksize_0 shr 1)
      else
         samples := samples + (f.blocksize_0 shr 1);
   end;
   // now, at this point, we've rewound to the very beginning of the
   // _second_ packet. if we entirely discard the first packet after
   // a seek, this will be exactly the right sample number. HOWEVER!
   // we can't as easily compute this number for the LAST page. The
   // only way to get the sample offset of the LAST page is to use
   // the end loc from the previous page. But what that returns us
   // is _exactly_ the place where we get our first non-overlapped
   // sample. (I think. Stupid spec for being ambiguous.) So for
   // consistency it's better to do that here, too. However, that
   // will then require us to NOT discard all of the first frame we
   // decode, in some cases, which means an even weirder frame size
   // and extra code. what a fucking pain.
   
   // we're going to discard the first packet if we
   // start the seek here, so we don't care about it. (we could actually
   // do better; if the first packet is long, and the previous packet
   // is short, there's actually data in the first half of the first
   // packet that doesn't need discarding... but not worth paying the
   // effort of tracking that of that here and in the seeking logic)
   // except crap, if we infer it from the _previous_ packet's end
   // location, we DO need to use that definition... and we HAVE to
   // infer the start loc of the LAST packet from the previous packet's
   // end location. fuck you, ogg vorbis.

   z.first_decoded_sample := z.last_decoded_sample - samples;

   // restore file state to where we were
   set_file_offset(f, z.page_start);
   result:=true;
   Exit;

   // restore file state to where we were
bail:
   set_file_offset(f, z.page_start);
   result:=false;
end;

function vorbis_seek_frame_from_page(f:pvorb; page_start,first_sample,target_sample:Cardinal; fine:boolean):Boolean;
var
   left_start,left_end,right_start,right_end,mode,i,
   frame,frames_to_skip, data_to_skip,start,j,n: integer;
   frame_start: Cardinal;
   ff: TOutput;
begin
   frame:=0;
   // first_sample is the sample # of the first sample that doesn't
   // overlap the previous page... note that this requires us to
   // _partially_ discard the first packet! bleh.
   set_file_offset(f, page_start);

   f.next_seg := -1;  // force page resync

   frame_start := first_sample;
   // frame start is where the previous packet's last decoded sample
   // was, which corresponds to left_end... EXCEPT if the previous
   // packet was long and this packet is short? Probably a bug here.


   // now, we can start decoding frames... we'll only FAKE decode them,
   // until we find the frame that contains our sample; then we'll rewind,
   // and try again
   while true do begin

      if not vorbis_decode_initial(f, left_start, left_end, right_start, right_end, mode) then begin
         result:=error(f, VORBIS_seek_failed);
         exit;
      end;   

      if frame=0 then start := left_end else start := left_start;

      // the window starts at left_start; the last valid sample we generate
      // before the next frame's window start is right_start-1
      if target_sample<frame_start+right_start-start then break;

      flush_packet(f);
      if f.eof then begin
         result:=error(f, VORBIS_seek_failed);
         Exit;
      end;

      frame_start := frame_start + right_start - start;

      Inc(frame);
   end;

   // ok, at this point, the sample we want is contained in frame #'frame'

   // to decode frame #'frame' normally, we have to decode the
   // previous frame first... but if it's the FIRST frame of the page
   // we can't. if it's the first frame, it means it falls in the part
   // of the first frame that doesn't overlap either of the other frames.
   // so, if we have to handle that case for the first frame, we might
   // as well handle it for all of them, so:
   if target_sample>frame_start+(left_end - left_start)then begin
      // so what we want to do is go ahead and just immediately decode
      // this frame, but then make it so the next get_frame_float() uses
      // this already-decoded data? or do we want to go ahead and rewind,
      // and leave a flag saying to skip the first N data? let's do that
      frames_to_skip := frame;  // if this is frame #1, skip 1 frame (#0)
      data_to_skip := left_end - left_start;
   end else begin
      // otherwise, we want to skip frames 0, 1, 2, ... frame-2
      // (which means frame-2+1 total frames) then decode frame-1,
      // then leave frame pending
      frames_to_skip := frame - 1;
      assert(frames_to_skip>=0);
      data_to_skip := -1;      
   end;

   set_file_offset(f, page_start);
   f.next_seg := - 1; // force page resync

   for i:=0 to frames_to_skip-1 do begin
      maybe_start_packet(f);
      flush_packet(f);
   end;

   if data_to_skip>=0 then begin
      n := f.blocksize_0 shr 1;
      f.discard_samples_deferred := data_to_skip;
      for i:=0 to f.channels-1 do
         for j:=0 to n-1 do
            IncPointer(f.previous_window[i],+j)^ := 0;
      f.previous_length := n;
      frame_start := frame_start + data_to_skip;
   end else begin
      f.previous_length := 0;
      vorbis_pump_first_frame(f);
   end;

   // at this point, the NEXT decoded frame will generate the desired sample
   if fine then begin
      // so if we're doing sample accurate streaming, we want to go ahead and decode it!
      if target_sample<>frame_start then begin
         stb_vorbis_get_frame_float(f, n, @ff);
         assert(target_sample>frame_start);
         assert(f.channel_buffer_start+target_sample-frame_start<f.channel_buffer_end);
         f.channel_buffer_start := f.channel_buffer_start + (target_sample - frame_start);
      end;
   end;

   result:=true;
end;

function vorbis_seek_base(f:pvorb; sample_number:Cardinal; fine:boolean):Boolean;
var
   p: array [0..2-1] of ProbedPage;
   q: ProbedPage;
   z,probe,start_offset,end_offset,start_sample,
   end_sample,probe2, tmp: Cardinal;
   attempts: integer;
   ff:single;
begin
   if IS_PUSH_MODE(f) then begin result:=error(f, VORBIS_invalid_api_mixing); exit; end;

   // do we know the location of the last page?
   if f.p_last.page_start=0 then begin
      z := stb_vorbis_stream_length_in_samples(f);
      if z=0 then begin result:=error(f, VORBIS_cant_find_last_page); Exit; end;
   end;

   p[0] := f.p_first;
   p[1] := f.p_last;

   if sample_number>=f.p_last.last_decoded_sample then 
      sample_number := f.p_last.last_decoded_sample-1;

   if sample_number<f.p_first.last_decoded_sample then begin
      vorbis_seek_frame_from_page(f, p[0].page_start, 0, sample_number, fine);
      result:=true;
      exit;
   end else begin
      attempts:=0;
      while p[0].page_end<p[1].page_start do begin

         // copy these into local variables so we can tweak them
         // if any are unknown
         start_offset := p[0].page_end;
         end_offset   := p[1].after_previous_page_start; // an address known to seek to page p[1]
         start_sample := p[0].last_decoded_sample;
         end_sample   := p[1].last_decoded_sample;

         // currently there is no such tweaking logic needed/possible?
         if (start_sample=SAMPLE_unknown) or (end_sample=SAMPLE_unknown) then begin
            result:=error(f, VORBIS_seek_failed);
            Exit;
         end;

         // now we want to lerp between these for the target samples...
      
         // step 1: we need to bias towards the page start...
         if start_offset+4000<end_offset then end_offset := end_offset - 4000;

         // now compute an interpolated search loc
         ff:=(end_offset - start_offset) / (end_sample - start_sample) * (sample_number - start_sample);
         probe := start_offset + floor(ff);

         // next we need to bias towards binary search...
         // code is a little wonky to allow for full 32-bit unsigned values
         if attempts>=4 then begin
            probe2 := start_offset + ((end_offset - start_offset) shr 1);
            if attempts>=8 then 
               probe:=probe2
            else if probe<probe2 then
               probe := probe + ((probe2 - probe) shr 1)
            else
               probe := probe2 + ((probe - probe2) shr 1);
         end;
         Inc(attempts);

         set_file_offset(f, probe);
         if not vorbis_find_page(f, tmp, tmp) then begin result:=error(f, VORBIS_seek_failed);exit;end;
         if not vorbis_analyze_page(f, @q) then begin result:=error(f, VORBIS_seek_failed);exit;end;
         q.after_previous_page_start := probe;

         // it's possible we've just found the last page again
         if q.page_start=p[1].page_start then begin
            p[1] := q;
            continue;
         end;

         if sample_number<q.last_decoded_sample then p[1]:=q else p[0]:=q;
      end;

      if (p[0].last_decoded_sample<=sample_number) and (sample_number<p[1].last_decoded_sample) then begin
         vorbis_seek_frame_from_page(f, p[1].page_start, p[0].last_decoded_sample, sample_number, fine);
         result:=true;
         exit;
      end;
      result:=error(f, VORBIS_seek_failed);
   end;
end;

function stb_vorbis_seek_frame(f:pvorb; sample_number:Cardinal):Boolean;
begin
   result:=vorbis_seek_base(f, sample_number, FALSE);
end;

function stb_vorbis_seek(f:pvorb; sample_number:Cardinal):Boolean;
begin
   result:=vorbis_seek_base(f, sample_number, TRUE);
end;

procedure stb_vorbis_seek_start(f:pvorb);
begin
   if IS_PUSH_MODE(f) then begin error(f, VORBIS_invalid_api_mixing); exit; end;
   set_file_offset(f, f.first_audio_page_offset);
   f.previous_length := 0;
   f.first_decode := true;
   f.next_seg := -1;
   vorbis_pump_first_frame(f);
end;

function stb_vorbis_stream_length_in_samples(f:pvorb):Cardinal;
label done;
var
   restore_offset,previous_safe,_end,
   last_page_loc,lo,hi,last: Cardinal;
   header: array [0..6-1] of byte;
begin

   if IS_PUSH_MODE(f) then begin result:=0; error(f, VORBIS_invalid_api_mixing); exit; end;
   if f.total_samples=0 then begin

      // first, store the current decode position so we can restore it
      restore_offset := stb_vorbis_get_file_offset(f);

      // now we want to seek back 64K from the end (the last page must
      // be at most a little less than 64K, but let's allow a little slop)
      if (f.stream_len>=65536) and (f.stream_len-65536>=f.first_audio_page_offset) then
         previous_safe := f.stream_len - 65536
      else
         previous_safe := f.first_audio_page_offset;

      set_file_offset(f, previous_safe);
      // previous_safe is now our candidate 'earliest known place that seeking
      // to will lead to the final page'

      if not vorbis_find_page(f, _end, last) then begin
         // if we can't find a page, we're hosed!
         f.error := VORBIS_cant_find_last_page;
         f.total_samples := $ffffffff;
         goto done;
      end;

      // check if there are more pages
      last_page_loc := stb_vorbis_get_file_offset(f);

      // stop when the last_page flag is set, not when we reach eof;
      // this allows us to stop short of a 'file_section' end without
      // explicitly checking the length of the section
      while last=0 do begin
         set_file_offset(f, _end);
         if not vorbis_find_page(f, _end, last) then begin
            // the last page we found didn't have the 'last page' flag
            // set. whoops!
            break;
         end;
         previous_safe := last_page_loc+1;
         last_page_loc := stb_vorbis_get_file_offset(f);
      end;

      set_file_offset(f, last_page_loc);

      // parse the header
      getn(f, header, 6);
      // extract the absolute granule position
      lo := get32(f);
      hi := get32(f);
      if (lo=$ffffffff) and (hi=$ffffffff) then begin
         f.error := VORBIS_cant_find_last_page;
         f.total_samples := SAMPLE_unknown;
         goto done;
      end;
      if hi<>0 then lo := $fffffffe; // saturate
      f.total_samples := lo;

      f.p_last.page_start := last_page_loc;
      f.p_last.page_end   := _end;
      f.p_last.last_decoded_sample := lo;
      f.p_last.first_decoded_sample := SAMPLE_unknown;
      f.p_last.after_previous_page_start := previous_safe;

done:
      set_file_offset(f, restore_offset);
   end;
   if f.total_samples=SAMPLE_unknown then result:=0 else result:=f.total_samples;
end;

function stb_vorbis_stream_length_in_seconds(f:pvorb):single;
begin
   result:=stb_vorbis_stream_length_in_samples(f) / f.sample_rate;
end;



function stb_vorbis_get_frame_float(f:pvorb; var channels:integer; output:pOutput):integer;
var
   len,right,left,i: integer;
begin
   if IS_PUSH_MODE(f) then begin result:=0; error(f, VORBIS_invalid_api_mixing); exit; end;

   if not vorbis_decode_packet(f, len, left, right) then begin
      f.channel_buffer_start := 0;
      f.channel_buffer_end := 0;
      result:=0;
      exit;
   end;

   len := vorbis_finish_frame(f, len, left, right);
   for i:=0 to f.channels-1 do begin
      f.outputs[i] := IncPointer(f.channel_buffers[i],+left);
      output[i]:=IncPointer(f.channel_buffers[i],+left);
   end;   

   f.channel_buffer_start := left;
   f.channel_buffer_end   := left+len;

   channels := f.channels;
   //for i:=0 to channels-1 do
   //output := @f.outputs;
   result:=len;
end;

function stb_vorbis_open_memory(data: pByte; len: Cardinal; var _error: STBVorbisError; alloc: pstb_vorbis_alloc):pvorb;
Var
  f:pvorb;
Begin
   Result := Nil;
   If Data=nil Then
    Exit;

   Result := vorbis_alloc();
   If Result=nil Then
    Exit;

   f := Result;
   vorbis_init(f, alloc);

   f.stream := data;
   f.stream_end := IncPointer(data, len);
   f.stream_start := f.stream;
   f.stream_len := len;
   f.push_mode := false;

   If start_decoder(f) Then
   Begin
      vorbis_pump_first_frame(f);
      Exit;
   End;

   _error := f.error;
   stb_vorbis_close(f);
   Result := nil;
end;

{$IFNDEF STB_VORBIS_NO_INTEGER_CONVERSION}

{$IFNDEF STB_VORBIS_NO_FAST_SCALED_FLOAT}
//   typedef union {
//      float f;
//      int i;
//   } float_conv;
//   typedef char stb_vorbis_float_size_test[sizeof(float)==4 && sizeof(int) == 4];
//   #define FASTDEF(x) float_conv x

// add (1<<23) to convert to int, then divide by 2^SHIFT, then add 0.5/2^SHIFT to round
//#define MAGIC(SHIFT) (1.5f * (1 << (23-SHIFT)) + 0.5f/(1 << SHIFT))
function MAGIC(SHIFT:integer):single;
begin
   result := 1.5 * (1 shl (23-SHIFT)) + 0.5/(1 shl SHIFT);
end;

//#define ADDEND(SHIFT) (((150-SHIFT) << 23) + (1 << 22))
function ADDEND(SHIFT:Integer):longint;
begin
   result := ((150-SHIFT) shl 23) + (1 shl 22);
end;

//#define FAST_SCALED_FLOAT_TO_INT(temp,x,s) (temp.f = (x) + MAGIC(s), temp.i - ADDEND(s))

function FAST_SCALED_FLOAT_TO_INT(x:Single; s:Integer) : Integer;
type
   float_conv = packed record
      case Integer of
         0:(f:single);
         1:(i:Integer);
//         1:(i:SmallInt);
      end;
var
   temp: float_conv;
begin
   temp.f := x + MAGIC(s);
   temp.i := temp.i - ADDEND(s);
   Result := temp.i;
end;

{$ELSE}

function FAST_SCALED_FLOAT_TO_INT(x:Single; s:Integer) : SmallInt;
begin
   Result := Floor(x * (1 shl s));
end;

{$ENDIF}



//procedure copy_samples(dest:array of SmallInt; src: array of single; len:Cardinal);
procedure copy_samples(dest: pSmallInt; src: pSingle; len:Cardinal);
var 
   i:SmallInt;
   v:longint;
begin
   for i:=0 to len-1 do begin
      v := FAST_SCALED_FLOAT_TO_INT(IncPointer(src,i)^,15);
      if Cardinal(v+32768)>65535 then 
         if v<0 then v:=-32768 else v:=32767;
      IncPointer(dest,i)^ := v;
   end;
end;

procedure compute_samples(mask:integer; output:pSmallInt; num_c:integer; data:pOutput; d_offset,len:integer);
const  BUFFER_SIZE = 32;
var
   buffer: array [0..BUFFER_SIZE-1] of single;
   i,j,o,n:integer;
   v:longint;
begin
   n := BUFFER_SIZE;
   o:=0;
   while o<len do begin
      for i:=0 to BUFFER_SIZE-1 do buffer[i]:=0;
      if o+n>len then n:=len-o;
      for j:=0 to num_c-1 do begin
         if (channel_position[num_c][j] and mask)<>0 then
            for i:=0 to n-1 do
               buffer[i] := buffer[i] + IncPointer(data[j],+d_offset+o+i)^;
      end;
      for i:=0 to n-1 do begin
         v := FAST_SCALED_FLOAT_TO_INT(buffer[i],15);
         if Cardinal(v+32768)>65535 then
            if v<0 then v:=-32768 else v:=32767;
         IncPointer(output,+o+i)^ := v;
      end;
      o:=o+BUFFER_SIZE;
   end;
end;

procedure compute_stereo_samples(output: pSmallInt; num_c:integer; data:POutput; d_offset,len:integer);
const  BUFFER_SIZE = 32;
var
   buffer: array [0..BUFFER_SIZE-1] of single;
   i,j,o,n,o2,m: integer;
   v:longint;

begin
   n := BUFFER_SIZE shr 1;
   // o is the offset in the source data
   o:=0;
   while o<len do begin
      // o2 is the offset in the output data
      o2 := o shl 1;
      for i:=0 to BUFFER_SIZE-1 do buffer[i]:=0;
      if o+n>len then n:=len-o;
      for j:=0 to num_c-1 do begin
         m := channel_position[num_c][j] and (PLAYBACK_LEFT or PLAYBACK_RIGHT);
         if m = (PLAYBACK_LEFT or PLAYBACK_RIGHT) then begin
            for i:=0 to n-1 do begin
               buffer[i*2+0] := buffer[i*2+0] + IncPointer(data[j],+d_offset+o+i)^;
               buffer[i*2+1] := buffer[i*2+1] + IncPointer(data[j],+d_offset+o+i)^;
            end;
         end else if m=PLAYBACK_LEFT then begin
            for i:=0 to n-1 do begin
               buffer[i*2+0] := buffer[i*2+0] + IncPointer(data[j],+d_offset+o+i)^;
            end;
         end else if m=PLAYBACK_RIGHT then begin
            for i:=0 to n-1 do begin
               buffer[i*2+1] := buffer[i*2+1] + IncPointer(data[j],+d_offset+o+i)^;
            end;
         end;
      end;
      for i:=0 to (n shl 1)-1 do begin
         v := FAST_SCALED_FLOAT_TO_INT(buffer[i],15);
         if Cardinal(v + 32768) > 65535 then
            if v<0 then v:=-32768 else v:=32767;
         IncPointer(output,+o2+i)^ := v;
      end;
      o:=o+BUFFER_SIZE;
   end;
end;

procedure convert_samples_short(buf_c:integer; buffer: pSmallInt; b_offset,data_c:integer;
          data:POutput; d_offset,samples:integer);
var i,j,limit:integer;
begin
   if (buf_c<>data_c) and (buf_c<=2) and (data_c<=6) then begin
      for i:=0 to buf_c-1 do
         compute_samples(channel_selector[buf_c][i], IncPointer(buffer,+i+b_offset), data_c, data, d_offset, samples)
   end else begin
   if buf_c<data_c then limit:=buf_c else limit:=data_c;
      for i:=0 to limit-1 do
         copy_samples(IncPointer(buffer,+i+b_offset), data[i], samples);
      i := limit;
      while i<buf_c do begin
         for j:=0 to samples-1 do IncPointer(buffer,+i+b_offset)^:=0;
         inc(i);
      end;
   end;
end;

function stb_vorbis_get_frame_short(f:pvorb; num_c:integer;  buffer: pSmallInt; num_samples:integer):Cardinal;
var 
   output:TOutput;
   len,channels:integer;
begin
   len := stb_vorbis_get_frame_float(f, channels, @output);
   if len>num_samples then len := num_samples;
   if len<>0 then
      convert_samples_short(num_c, buffer, 0, f.channels, @output, 0, len);
   result:=len;
end;

procedure convert_channels_short_interleaved(buf_c:integer; buffer: pSmallInt; data_c:integer;
          data:POutput; d_offset,len:integer);
var
   i,j,limit,k:integer;          
   v:longint;
   f: single;
begin
   if (buf_c<>data_c) and (buf_c<=2) and (data_c<=6) then begin
      assert(buf_c=2);
      for i:=0 to buf_c-1 do 
         compute_stereo_samples(buffer, data_c, data, d_offset, len);
   end else begin
      k:=0;
      if buf_c<data_c then limit:=buf_c else limit:=data_c;
      for j:=0 to len-1 do begin
         for i:=0 to limit-1 do begin
            f := IncPointer(data[i],+d_offset+j)^;
            v := FAST_SCALED_FLOAT_TO_INT(f,15);
            if Cardinal(v+32768)>65535 then 
               if v<0 then v:=-32768 else v:=32767;
            IncPointer(buffer,+k)^ := v;
            Inc(k);
         end;
         i := limit;
         while i<buf_c do begin
            IncPointer(buffer,+k)^ := 0;
            inc(k);
            inc(i);
         end;   
      end;
   end;
end;

function stb_vorbis_get_frame_short_interleaved(f:pvorb; num_c:integer; buffer:pSmallInt; num_shorts:integer):Cardinal;
var
   output: TOutput;
   len,channels:integer;
begin
   if num_c=1 then begin result:=stb_vorbis_get_frame_short(f,num_c,buffer, num_shorts); exit; end;
   len:=stb_vorbis_get_frame_float(f, channels, @output);
   if len<>0 then  begin
      if len*num_c>num_shorts then len:=num_shorts div num_c;
      convert_channels_short_interleaved(num_c, buffer, f.channels, @output, 0, len);
   end;
   result:=len;
end;

function stb_vorbis_get_samples_short_interleaved(f:pvorb; channels:integer; 
         buffer:array of SmallInt; num_shorts:integer):integer;
var
   outputs: TOutput;
   len,n,z,k,kk,ch:integer;     
   buf:array of SmallInt;    
begin
   len := num_shorts div channels;
   n:=0;
   kk:=0;
   z := f.channels;
   if z>channels then z:=channels;
   buf:=@buffer[kk];
   while n<len do begin
      k := f.channel_buffer_end - f.channel_buffer_start;
      if n+k>=len then k:=len-n;
      if k<>0 then
         convert_channels_short_interleaved(channels, @buf[0], f.channels, @f.channel_buffers, f.channel_buffer_start, k);
      //buffer += k*channels;
      kk:=kk+k*channels;
      buf:=@buffer[kk];   
      n := n + k;
      f.channel_buffer_start := f.channel_buffer_start + k;
      if n=len then break;
      if stb_vorbis_get_frame_float(f, ch, @outputs)=0 then break;
   end;
   result:=n;
end;

function stb_vorbis_get_samples_short(f:pvorb; channels:integer; buffer:array of SmallInt; len:integer):integer;
var
   outputs: TOutput;
   n,z,k,ch: Integer;
begin
   n:=0;
   z:=f.channels;
   if z>channels then z:=channels;
   while n<len do begin
      k := f.channel_buffer_end - f.channel_buffer_start;
      if n+k >= len then k:=len-n;
      if k<>0 then
         convert_samples_short(channels, {TEST!}@buffer, n, f.channels, @f.channel_buffers, f.channel_buffer_start, k);
      n := n + k;
      f.channel_buffer_start := f.channel_buffer_start + k;
      if n=len then break;
      if stb_vorbis_get_frame_float(f, ch, @outputs)=0 then break;
   end;
   result:=n;
end;

function stb_vorbis_decode_memory(mem:pByte; len:integer; var channels, SampleRate:integer; var output:pSmallInt):Integer;
Var
  data_len,offset,total,limit,n: integer;
  _error: STBVorbisError;
  v:Pvorb;
Begin
  v := stb_vorbis_open_memory(mem, len, _error, nil);
  if v=nil Then
  Begin
    Result:=-1;
    Exit;
  End;


   limit := v.channels * 4096;
   channels := v.channels;
   SampleRate := V.sample_rate;
   offset:=0;
   data_len:=0;
   total := limit;
   getMem(output, total*sizeof(SmallInt));
   While true Do
   Begin
      n := stb_vorbis_get_frame_short_interleaved(v, v.channels, IncPointer(output, offset), total-offset);
      If n=0 Then
        Break;

      data_len := data_len + n;
      offset := offset + n*v.channels;

      If offset+limit > total Then
      Begin
         total := total * 2;
         output:=reallocMem(output,total*sizeof(SmallInt));
      End;
   End;


   Result := data_len;

   stb_vorbis_close(v);
End;
{$ENDIF}

Function stb_vorbis_get_samples_float_interleaved(f:pvorb; channels:integer; buffer:array of single; num_floats:integer):integer;
Var
   outputs: TOutput;
   len,n,z,i,j,k,kk,ch: integer;
Begin
   len := num_floats div channels;
   n:=0;
   kk:=0;

   z := f.channels;

   If z>channels Then
    z:=channels;

   while n<len Do
   Begin
      k := f.channel_buffer_end - f.channel_buffer_start;
      if n+k>=len then k:=len-n;
      for j:=0 to k-1 do begin
         for i:=0 to z-1 do begin
            buffer[kk] := IncPointer(f.channel_buffers[i],+f.channel_buffer_start+j)^;
            Inc(kk);
         end;
         i := z;
         While i<channels do
         Begin
            buffer[kk] := 0;
            Inc(kk);
            Inc(i);
         end;   
      end;
      n := n + k;
      f.channel_buffer_start := f.channel_buffer_start + k;
      if n=len then  break;
      if stb_vorbis_get_frame_float(f, ch, @outputs)=0 then break;
   end;
   result:=n;
end;

(*function stb_vorbis_get_samples_float(f:pvorb; channels:integer; buffer:POutput; num_samples:integer):integer;
var
   outputs:TOutput;
   n,z,i,j,k,ch: integer;
begin
   n:=0;
   z := f.channels;
   if z>channels then z:=channels;
   while n<num_samples do begin
      k := f.channel_buffer_end - f.channel_buffer_start;
      if n+k>=num_samples then k:=num_samples-n;
      if k<>0 then begin
         for i:=0 to z-1 do
            //memcpy(buffer[i]+n, f->channel_buffers+f->channel_buffer_start, sizeof(float)*k);
            for j:=0 to k-1 do
            Begin
              IncPointer(buffer[i],+n+k)^ := IncPointer(PSingle(Cardinal(@f.channel_buffers[i]) + f.channel_buffer_start),+k)^;
            End;s

         i := z;
         while i<channels do begin
            //memset(buffer[i]+n, 0, sizeof(float) * k);
            for j:=0 to k-1 do IncPointer(buffer[i],+n+k)^:=0;
            inc(i);
         end;
      end;
      n := n + k;
      f.channel_buffer_start := f.channel_buffer_start + k;
      if n=num_samples then break;
      if stb_vorbis_get_frame_float(f, ch, @outputs)=0 then break;
   end;
   result:=n;
end;*)

Const
  OggBufferSize = 1024 * 8;
  OggSamplesBufferSize = 1024 * 32;
  OggMaxLeftovers = 1024;

Type
  OggStreamer = Class(SoundStream)
    Protected
      _Vorbis:Pvorb;
      _Info:stb_vorbis_info;
      _Error:STBVorbisError;
      _BaseOffset:Integer;
      _TargetSampleRate:Integer;
      _Temp:Array[0..Pred(OggBufferSize)] Of Byte;

      _LeftOversBuffer:TERRAAudioBuffer;
      _LeftOversTotal:Integer;

      Function ResampleSound(left, right:PSingle; Offset, Length:Integer):Integer;
      Function FillBuffer(Offset:Integer):Integer;

      Procedure InitStream; Override;
      Procedure RequestMoreSamples(); Override;


      Class Function Validate(Source:TERRAStream):Boolean; Override;

    Public
      Procedure Release; Override;
  End;

Procedure OggStreamer.InitStream;
Var
  Q, N, Used:Integer;
  Data:Pointer;
Begin
  Self._Loop := True;

  Q := 4096;
  Repeat
    GetMem(Data, Q);
    _Source.Seek(0);
    _Source.Read(Data, Q);

    _Vorbis := stb_vorbis_open_pushdata(Data, Q, Used, _Error, Nil);
    FreeMem(Data);

    If (_Vorbis = Nil) Then
    Begin
      If (_Error = VORBIS_need_more_data) Then
      Begin
        Inc(Q, 1024);
        Continue;
      End;

      RaiseError('Cannot load '+_Source.Name+ ', error code: '+ IntegerProperty.Stringify(Integer(_Error)));
      Exit;
    End;

  Until Assigned(_Vorbis);

  _BaseOffset := Used;
  _Source.Seek(_BaseOffset);

  _Info := stb_vorbis_get_info(_Vorbis);
  _TargetSampleRate := _Info.sample_rate; // 44100;

  Self._Buffer := TERRAAudioBuffer.Create(OggSamplesBufferSize, _TargetSampleRate, True);
  Self._LeftOversBuffer := TERRAAudioBuffer.Create(OggMaxLeftovers, _TargetSampleRate, True);
End;

Procedure OggStreamer.Release();
Begin
  Inherited;

  ReleaseObject(_LeftoversBuffer);
  stb_vorbis_close(_Vorbis);
End;

Function OggStreamer.ResampleSound(left, right:PSingle; Offset, Length:Integer):Integer;
Const
  Scale = 32767;
Var
  fs:Single;
  I, J, newsamples:Cardinal;
  index:Cardinal;
  InSample:MixingAudioSample;
  OutSample:PAudioSample;
Begin
  fs := _TargetSampleRate / _Info.sample_rate;

  NewSamples := Trunc(Length * fs);
  Result := NewSamples;

  For j:=0 to pred(newsamples) do
  Begin
    Index := Trunc(j/fs);

    InSample.Left := IncPointer(Left ,+index)^;
    InSample.Right := IncPointer(Right ,+index)^;

    I := Index + Offset;

    If (I<_Buffer.SampleCount) Then
    Begin
      OutSample := _Buffer.GetSampleAt(I, 0);
    End Else
    Begin
      _LeftOversTotal := Succ(I - _Buffer.SampleCount);
      OutSample := _LeftOversBuffer.GetSampleAt(_LeftOversTotal - 1, 0);
    End;

    OutSample^ := Trunc(FloatClamp(InSample.Left, -1, 1.0) * 32767);
    Inc(OutSample);
    OutSample^ := Trunc(FloatClamp(InSample.Right, -1, 1.0) * 32767);
  End;
End;

Function OggStreamer.FillBuffer(Offset:Integer): Integer;
Label retry3;
Var
  Q:Integer;
  outputs:TOutput;
  left, right:PSingle;
  SamplesToRender, ChannelCount:Integer;
  Ofs, Used:Integer;
Begin
  Result := 0;
  q := 1024;
  Ofs := _Source.Position;
  retry3:
    If (q > _Source.Size - _Source.Position) Then
      q := _Source.Size - _Source.Position;

    _Source.Read(@_Temp[0], Q);
    Used := stb_vorbis_decode_frame_pushdata(_Vorbis, @(_Temp[0]), q, ChannelCount, @outputs, SamplesToRender);
    If (Used = 0) Then
    Begin
      If (_Source.Position + q >= _Source.Size) Then
        Exit; // no more data, stop

      If (q < 128) Then
        q := 128;
      q := q * 2;

      If (Q>OggBufferSize) Then
      Begin
        Log(logWarning, 'OGG', 'Buffer too small!');
      End;

      _Source.Seek(Ofs);
      Goto retry3;
    End;

  If (Ofs + Used>= _Source.Position) Then
    _Source.Seek(_BaseOffset)
  Else
    _Source.Seek(Ofs + Used);

  If (SamplesToRender = 0) Then
    Exit; // seek/error recovery

  Left := outputs[0];
  If (ChannelCount > 1) Then
    Right := outputs[1]
  Else
    Right := outputs[0];

  Result := ResampleSound(Left, Right, Offset, SamplesToRender);
End;

Procedure OggStreamer.RequestMoreSamples();
Var
  Ofs, Count, Total:Integer;
Begin
  _CurrentSample := 0;

  Total := _Buffer.SampleCount;

  If (_LeftOversTotal>0) Then
  Begin
    Move(_LeftOversBuffer.Samples^, _Buffer.Samples^, SizeOf(AudioSample) * _LeftOversTotal * 2);
    Dec(Total, _LeftOversTotal);
    Ofs := _LeftOversTotal;
    _LeftOversTotal := 0;
  End Else
    Ofs := 0;

  While Total>0 Do
  Begin
    Count := FillBuffer(Ofs);
    Inc(Ofs, Count);
    Dec(Total, Count);
  End;
End;


Class Function OggStreamer.Validate(Source:TERRAStream):Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Log(logDebug, 'Ogg', 'Got ID: '+ID);

  Result := CompareFileHeader(ID, 'OggS');
End;

{ OGGFormat }
Function OGGFormat.Identify(Source: TERRAStream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Log(logDebug, 'Ogg', 'Got ID: '+ID);

  Result := CompareFileHeader(ID, 'OggS');
End;

Function OGGFormat.LoadFromStream(Target:TERRAObject; Source:TERRAStream): Boolean;
Var
  Mem:Array Of Byte;
  Samples, Channels, SampleRate, Size:Integer;
  Output:pSmallInt;
  Sound:TERRASound;
Begin
  Result := False;

  SetLength(Mem, Source.Size);
  Source.Seek(0);

  Source.Read(@Mem[0], Source.Size);

  Samples := stb_vorbis_decode_memory(@Mem[0], Source.Size, Channels, SampleRate, output);

  If (Samples<0) Then
    Exit;

  Size := Samples * Channels * 2;
  Sound := TERRASound(Target);
  Sound.SamplesFromBuffer(Samples, SampleRate, Channels>1, Output);

  FreeMem(Output);

  Result := True;
End;

Initialization
  Log(logDebug, 'OGG', 'Initializing');
  RegisterSoundStreamFormat(OggStreamer);

  Engine.Formats.Add(OGGFormat.Create(TERRASound, 'ogg'));
  Log(logDebug, 'OGG', 'OGG Sound format ready!');
End.
