Unit alsa;

Interface

Uses unixtype;

const
  AlsaLib = 'libasound.so.2';

type
  Pint16 = ^Word;

  {  PCM generic info container  }
     Psnd_pcm_info_t = pointer;
  {  PCM hardware configuration space container  }
     Psnd_pcm_hw_params_t = pointer;
  {  PCM software configuration container  }
     Psnd_pcm_sw_params_t = pointer;
  {  PCM status container  }
     Psnd_pcm_status_t = pointer;
  {  PCM access types mask  }
     Psnd_pcm_access_mask_t = pointer;
  {  PCM formats mask  }
     Psnd_pcm_format_mask_t = pointer;
  {  PCM subformats mask  }
     Psnd_pcm_subformat_mask_t = pointer;

  Psnd_config_t = pointer;

  Psnd_async_handler_t = pointer;

  Psnd_output_t = Pointer;

  {  PCM class  }
     _snd_pcm_class =  Cardinal;
     snd_pcm_class_t = _snd_pcm_class;
const
     SND_PCM_CLASS_GENERIC = 0;   { standard device  }
     SND_PCM_CLASS_MULTI = 1;     { multichannel device  }
     SND_PCM_CLASS_MODEM = 2;     { software modem device  }
     SND_PCM_CLASS_DIGITIZER = 3; { digitizer device  }
     SND_PCM_CLASS_LAST = SND_PCM_CLASS_DIGITIZER;

type
  {  PCM subclass  }
     _snd_pcm_subclass =  Cardinal;
     snd_pcm_subclass_t = _snd_pcm_subclass;
const
     SND_PCM_SUBCLASS_GENERIC_MIX = 0; { subdevices are mixed together  }
     SND_PCM_SUBCLASS_MULTI_MIX = 1;   { multichannel subdevices are mixed together  }
     SND_PCM_SUBCLASS_LAST = SND_PCM_SUBCLASS_MULTI_MIX;

type
  {  PCM stream (direction)  }
     _snd_pcm_stream =  Cardinal;
     snd_pcm_stream_t = _snd_pcm_stream;
const
     SND_PCM_STREAM_PLAYBACK = 0;    { Playback stream  }
     SND_PCM_STREAM_CAPTURE = 1;     { Capture stream  }
     SND_PCM_STREAM_LAST = SND_PCM_STREAM_CAPTURE;

type
  {  PCM access type  }
     _snd_pcm_access =  Cardinal;
     snd_pcm_access_t = _snd_pcm_access;
const
     SND_PCM_ACCESS_MMAP_INTERLEAVED = 0;     { mmap access with simple interleaved channels }
     SND_PCM_ACCESS_MMAP_NONINTERLEAVED = 1;  { mmap access with simple non interleaved channels }
     SND_PCM_ACCESS_MMAP_COMPLEX = 2;         { mmap access with complex placement }
     SND_PCM_ACCESS_RW_INTERLEAVED = 3;       { snd_pcm_readi/snd_pcm_writei access }
     SND_PCM_ACCESS_RW_NONINTERLEAVED = 4;    { snd_pcm_readn/snd_pcm_writen access }
     SND_PCM_ACCESS_LAST = SND_PCM_ACCESS_RW_NONINTERLEAVED;

type
   { PCM sample format }
     _snd_pcm_format =  Cardinal;
     snd_pcm_format_t = _snd_pcm_format;
const
	{ Unknown }
	SND_PCM_FORMAT_UNKNOWN = -1;
	{ Signed 8 bit }
	SND_PCM_FORMAT_S8 = 0;
	{ Unsigned 8 bit }
	SND_PCM_FORMAT_U8 = 1;
	{ Signed 16 bit Little Endian }
	SND_PCM_FORMAT_S16_LE = 2;
	{ Signed 16 bit Big Endian }
	SND_PCM_FORMAT_S16_BE = 3;
	{ Unsigned 16 bit Little Endian }
	SND_PCM_FORMAT_U16_LE = 4;
	{ Unsigned 16 bit Big Endian }
	SND_PCM_FORMAT_U16_BE = 5;
	{ Signed 24 bit Little Endian }
	SND_PCM_FORMAT_S24_LE = 6;
	{ Signed 24 bit Big Endian }
	SND_PCM_FORMAT_S24_BE = 7;
	{ Unsigned 24 bit Little Endian }
	SND_PCM_FORMAT_U24_LE = 8;
	{ Unsigned 24 bit Big Endian }
	SND_PCM_FORMAT_U24_BE = 9;
	{ Signed 32 bit Little Endian }
	SND_PCM_FORMAT_S32_LE = 10;
	{ Signed 32 bit Big Endian }
	SND_PCM_FORMAT_S32_BE = 11;
	{ Unsigned 32 bit Little Endian }
	SND_PCM_FORMAT_U32_LE = 12;
	{ Unsigned 32 bit Big Endian }
	SND_PCM_FORMAT_U32_BE = 13;
	{ Float 32 bit Little Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT_LE = 14;
	{ Float 32 bit Big Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT_BE = 15;
	{ Float 64 bit Little Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT64_LE = 16;
	{ Float 64 bit Big Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT64_BE = 17;
	{ IEC-958 Little Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME_LE = 18;
	{ IEC-958 Big Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME_BE = 19;
	{ Mu-Law }
	SND_PCM_FORMAT_MU_LAW = 20;
	{ A-Law }
	SND_PCM_FORMAT_A_LAW = 21;
	{ Ima-ADPCM }
	SND_PCM_FORMAT_IMA_ADPCM = 22;
	{ MPEG }
	SND_PCM_FORMAT_MPEG = 23;
	{ GSM }
	SND_PCM_FORMAT_GSM = 24;
	{ Special }
	SND_PCM_FORMAT_SPECIAL = 31;
	{ Signed 24bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S24_3LE = 32;
	{ Signed 24bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S24_3BE = 33;
	{ Unsigned 24bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U24_3LE = 34;
	{ Unsigned 24bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U24_3BE = 35;
	{ Signed 20bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S20_3LE = 36;
	{ Signed 20bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S20_3BE = 37;
	{ Unsigned 20bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U20_3LE = 38;
	{ Unsigned 20bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U20_3BE = 39;
	{ Signed 18bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S18_3LE = 40;
	{ Signed 18bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S18_3BE = 41;
	{ Unsigned 18bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U18_3LE = 42;
	{ Unsigned 18bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U18_3BE = 43;
	SND_PCM_FORMAT_LAST = SND_PCM_FORMAT_U18_3BE;

{$IFDEF ENDIAN_LITTLE}
	{ Signed 16 bit CPU endian }
	SND_PCM_FORMAT_S16 = SND_PCM_FORMAT_S16_LE;
	{ Unsigned 16 bit CPU endian }
	SND_PCM_FORMAT_U16 = SND_PCM_FORMAT_U16_LE;
	{ Signed 24 bit CPU endian }
	SND_PCM_FORMAT_S24 = SND_PCM_FORMAT_S24_LE;
	{ Unsigned 24 bit CPU endian }
	SND_PCM_FORMAT_U24 = SND_PCM_FORMAT_U24_LE;
	{ Signed 32 bit CPU endian }
	SND_PCM_FORMAT_S32 = SND_PCM_FORMAT_S32_LE;
	{ Unsigned 32 bit CPU endian }
	SND_PCM_FORMAT_U32 = SND_PCM_FORMAT_U32_LE;
	{ Float 32 bit CPU endian }
	SND_PCM_FORMAT_FLOAT = SND_PCM_FORMAT_FLOAT_LE;
	{ Float 64 bit CPU endian }
	SND_PCM_FORMAT_FLOAT64 = SND_PCM_FORMAT_FLOAT64_LE;
	{ IEC-958 CPU Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME = SND_PCM_FORMAT_IEC958_SUBFRAME_LE;
{$ENDIF}

{$IFDEF ENDIAN_BIG}
	{ Signed 16 bit CPU endian }
	SND_PCM_FORMAT_S16 = SND_PCM_FORMAT_S16_BE;
	{ Unsigned 16 bit CPU endian }
	SND_PCM_FORMAT_U16 = SND_PCM_FORMAT_U16_BE;
	{ Signed 24 bit CPU endian }
	SND_PCM_FORMAT_S24 = SND_PCM_FORMAT_S24_BE;
	{ Unsigned 24 bit CPU endian }
	SND_PCM_FORMAT_U24 = SND_PCM_FORMAT_U24_BE;
	{ Signed 32 bit CPU endian }
	SND_PCM_FORMAT_S32 = SND_PCM_FORMAT_S32_BE;
	{ Unsigned 32 bit CPU endian }
	SND_PCM_FORMAT_U32 = SND_PCM_FORMAT_U32_BE;
	{ Float 32 bit CPU endian }
	SND_PCM_FORMAT_FLOAT = SND_PCM_FORMAT_FLOAT_BE;
	{ Float 64 bit CPU endian }
	SND_PCM_FORMAT_FLOAT64 = SND_PCM_FORMAT_FLOAT64_BE;
	{ IEC-958 CPU Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME = SND_PCM_FORMAT_IEC958_SUBFRAME_BE;
{$ENDIF}

type
  {  PCM sample subformat  }
     _snd_pcm_subformat =  Cardinal;
     snd_pcm_subformat_t = _snd_pcm_subformat;

const
     SND_PCM_SUBFORMAT_STD = 0; {  Standard  }
     SND_PCM_SUBFORMAT_LAST = SND_PCM_SUBFORMAT_STD;

type
  {  PCM state  }
     _snd_pcm_state =  Cardinal;
     snd_pcm_state_t = _snd_pcm_state;
const
     SND_PCM_STATE_OPEN = 0;                     { Open  }
     SND_PCM_STATE_SETUP = 1;                    { Setup installed  }
     SND_PCM_STATE_PREPARED = 2;                 { Ready to start  }
     SND_PCM_STATE_RUNNING = 3;                  { Running  }
     SND_PCM_STATE_XRUN = 4;                     { Stopped: underrun (playback) or overrun (capture) detected  }
     SND_PCM_STATE_DRAINING = 5;                 { Draining: running (playback) or stopped (capture)  }
     SND_PCM_STATE_PAUSED = 6;                   { Paused  }
     SND_PCM_STATE_SUSPENDED = 7;                { Hardware is suspended  }
     SND_PCM_STATE_LAST = SND_PCM_STATE_SUSPENDED;

type
  {  PCM start mode  }
     _snd_pcm_start =  Cardinal;
     snd_pcm_start_t = _snd_pcm_start;
const
     SND_PCM_START_DATA = 0;                     { Automatic start on data read/write  }
     SND_PCM_START_EXPLICIT = 1;                 { Explicit start  }
     SND_PCM_START_LAST = SND_PCM_START_EXPLICIT;

type
  {  PCM xrun mode  }
     _snd_pcm_xrun =  Cardinal;
     snd_pcm_xrun_t = _snd_pcm_xrun;
const
     SND_PCM_XRUN_NONE = 0;                      { Xrun detection disabled  }
     SND_PCM_XRUN_STOP = 1;                      { Stop on xrun detection  }
     SND_PCM_XRUN_LAST = SND_PCM_XRUN_STOP;

type
  {  PCM timestamp mode  }
     _snd_pcm_tstamp =  Cardinal;
     snd_pcm_tstamp_t = _snd_pcm_tstamp;
const
     SND_PCM_TSTAMP_NONE = 0;                    { No timestamp  }
     SND_PCM_TSTAMP_MMAP = 1;                    { Update mmap'ed timestamp  }
     SND_PCM_TSTAMP_LAST = SND_PCM_TSTAMP_MMAP;

type
  {  Unsigned frames quantity  }
     snd_pcm_uframes_t = LongWord;
     Psnd_pcm_uframes_t = ^snd_pcm_uframes_t;
  {  Signed frames quantity  }
     snd_pcm_sframes_t = Integer;
     Psnd_pcm_sframes_t = ^snd_pcm_sframes_t;
  {  Timestamp  }
     snd_timestamp_t = timeval;
     Psnd_timestamp_t = ^snd_timestamp_t;

const
  {  Non blocking mode (flag for open mode) \hideinitializer  }
     SND_PCM_NONBLOCK = $0001;
  {  Async notification (flag for open mode) \hideinitializer  }
     SND_PCM_ASYNC = $0002;

type
  {  PCM handle  }
     Psnd_pcm_t = pointer;

  {  PCM type  }
     _snd_pcm_type =  Cardinal;
     snd_pcm_type_t = _snd_pcm_type;
const
  {  PCM type  }
       SND_PCM_TYPE_HW = 0;                      {  Kernel level PCM  }
       SND_PCM_TYPE_HOOKS = 1;                   {  Hooked PCM  }
       SND_PCM_TYPE_MULTI = 2;                   {  One ore more linked PCM with exclusive access to selected  channels  }
       SND_PCM_TYPE_FILE = 3;                    {  File writing plugin  }
       SND_PCM_TYPE_NULL = 4;                    {  Null endpoint PCM  }
       SND_PCM_TYPE_SHM = 5;                     {  Shared memory client PCM  }
       SND_PCM_TYPE_INET = 6;                    {  INET client PCM (not yet implemented)  }
       SND_PCM_TYPE_COPY = 7;                    {  Copying plugin  }
       SND_PCM_TYPE_LINEAR = 8;                  {  Linear format conversion PCM  }
       SND_PCM_TYPE_ALAW = 9;                    {  A-Law format conversion PCM  }
       SND_PCM_TYPE_MULAW = 10;                  {  Mu-Law format conversion PCM  }
       SND_PCM_TYPE_ADPCM = 11;                  {  IMA-ADPCM format conversion PCM  }
       SND_PCM_TYPE_RATE = 12;                   {  Rate conversion PCM  }
       SND_PCM_TYPE_ROUTE = 13;                  {  Attenuated static route PCM  }
       SND_PCM_TYPE_PLUG = 14;                   {  Format adjusted PCM  }
       SND_PCM_TYPE_SHARE = 15;                  {  Sharing PCM  }
       SND_PCM_TYPE_METER = 16;                  {  Meter plugin  }
       SND_PCM_TYPE_MIX = 17;                    {  Mixing PCM  }
       SND_PCM_TYPE_DROUTE = 18;                 {  Attenuated dynamic route PCM (not yet implemented)  }
       SND_PCM_TYPE_LBSERVER = 19;               {  Loopback server plugin (not yet implemented)  }
       SND_PCM_TYPE_LINEAR_FLOAT = 20;           {  Linear Integer <-> Linear Float format conversion PCM  }
       SND_PCM_TYPE_LADSPA = 21;                 {  LADSPA integration plugin  }


  type

  {  PCM area specification  }
     _snd_pcm_channel_area = record
          addr : pointer;                        {  base address of channel samples  }
          first : LongWord;                      {  offset to first sample in bits  }
          step : LongWord;                       {  samples distance in bits  }
       end;
     snd_pcm_channel_area_t = _snd_pcm_channel_area;
     Psnd_pcm_channel_area_t = ^snd_pcm_channel_area_t;

  {  PCM synchronization ID  }
     _snd_pcm_sync_id = record
         case Cardinal of
            0 : ( id : array[0..15] of byte );   {  8-bit ID  }
            1 : ( id16 : array[0..7] of word );  {  16-bit ID  }
            2 : ( id32 : array[0..3] of LongWord );  {  32-bit ID  }
         end;
     snd_pcm_sync_id_t = _snd_pcm_sync_id;

  {  #SND_PCM_TYPE_METER scope handle  }
     Psnd_pcm_scope_t = pointer;

type
   snd_async_callback_t = Procedure (pcm_callback:Psnd_async_handler_t); Cdecl;
  snd_pcm_open_t = function(var pcm:Psnd_pcm_t; name:PAnsiChar; stream:snd_pcm_stream_t; mode:Cardinal):Integer;cdecl;
  snd_pcm_open_lconf_t = function(var pcm:Psnd_pcm_t; name:PAnsiChar; stream:snd_pcm_stream_t; mode:Cardinal; lconf:Psnd_config_t):Integer;cdecl;
  snd_pcm_close_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_name_t = function(pcm:Psnd_pcm_t):PAnsiChar;cdecl;
  snd_pcm_type_t_t = function(pcm:Psnd_pcm_t):snd_pcm_type_t;cdecl;
  snd_pcm_stream_t_t = function(pcm:Psnd_pcm_t):snd_pcm_stream_t;cdecl;
  snd_pcm_poll_descriptors_count_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  //snd_pcm_poll_descriptors_t = function(pcm:Psnd_pcm_t; var pfds:pollfd; space:LongWord):Integer;cdecl;
 // snd_pcm_poll_descriptors_revents_t = function(pcm:Psnd_pcm_t; var pfds:pollfd; nfds:LongWord; revents:Pword):Integer;cdecl;
  snd_pcm_set_nonblock_t = function(pcm:Psnd_pcm_t; nonblock:Cardinal):Integer;cdecl;
  snd_async_add_pcm_handler_t = function(var handler:Psnd_async_handler_t; pcm:Psnd_pcm_t; callback:snd_async_callback_t; private_data:pointer):Integer;cdecl;
  snd_async_handler_get_pcm_t = function(handler:Psnd_async_handler_t):Psnd_pcm_t;cdecl;
  snd_pcm_info_t = function(pcm:Psnd_pcm_t; info:Psnd_pcm_info_t):Integer;cdecl;
  snd_pcm_hw_params_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_current_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_free_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_sw_params_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t):Integer;cdecl;
  snd_pcm_sw_params_current_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t):Integer;cdecl;
  snd_pcm_prepare_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_reset_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_status_t = function(pcm:Psnd_pcm_t; status:Psnd_pcm_status_t):Integer;cdecl;
  snd_pcm_start_t_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_drop_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_drain_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_pause_t = function(pcm:Psnd_pcm_t; enable:Cardinal):Integer;cdecl;
  snd_pcm_state_t_t = function(pcm:Psnd_pcm_t):snd_pcm_state_t;cdecl;
  snd_pcm_hwsync_t = function(pcm: Psnd_pcm_t):Integer;cdecl;
  snd_pcm_delay_t = function(pcm:Psnd_pcm_t; delayp:Psnd_pcm_sframes_t):Integer;cdecl;
  snd_pcm_resume_t = function(pcm:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_avail_update_t = function(pcm:Psnd_pcm_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_rewind_t = function(pcm:Psnd_pcm_t; frames:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_writei_t = function(pcm:Psnd_pcm_t; buffer:pointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_readi_t = function(pcm:Psnd_pcm_t; buffer:pointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_writen_t = function(pcm:Psnd_pcm_t; bufs:Ppointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_readn_t = function(pcm:Psnd_pcm_t; bufs:Ppointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_wait_t = function(pcm:Psnd_pcm_t; timeout:Cardinal):Integer;cdecl;
  snd_pcm_link_t = function(pcm1:Psnd_pcm_t; pcm2:Psnd_pcm_t):Integer;cdecl;
  snd_pcm_unlink_t = function(pcm:Psnd_pcm_t):Integer;cdecl;

  snd_pcm_info_sizeof_t = function:size_t;cdecl;
  snd_pcm_info_malloc_t = function(var obj:Psnd_pcm_info_t):Integer;cdecl;
  snd_pcm_info_free_t = procedure(obj:Psnd_pcm_info_t);cdecl;
  snd_pcm_info_copy_t = procedure(dst:Psnd_pcm_info_t; src:Psnd_pcm_info_t);cdecl;
  snd_pcm_info_get_device_t = function(obj:Psnd_pcm_info_t):LongWord;cdecl;
  snd_pcm_info_get_subdevice_t = function(obj:Psnd_pcm_info_t):LongWord;cdecl;
  snd_pcm_info_get_stream_t = function(obj:Psnd_pcm_info_t):snd_pcm_stream_t;cdecl;
  snd_pcm_info_get_card_t = function(obj:Psnd_pcm_info_t):Integer;cdecl;
  snd_pcm_info_get_id_t = function(obj:Psnd_pcm_info_t):PAnsiChar;cdecl;
  snd_pcm_info_get_name_t = function(obj:Psnd_pcm_info_t):PAnsiChar;cdecl;
  snd_pcm_info_get_subdevice_name_t = function(obj:Psnd_pcm_info_t):PAnsiChar;cdecl;
  snd_pcm_info_get_class_t = function(obj:Psnd_pcm_info_t):snd_pcm_class_t;cdecl;
  snd_pcm_info_get_subclass_t = function(obj:Psnd_pcm_info_t):snd_pcm_subclass_t;cdecl;
  snd_pcm_info_get_subdevices_count_t = function(obj:Psnd_pcm_info_t):LongWord;cdecl;
  snd_pcm_info_get_subdevices_avail_t = function(obj:Psnd_pcm_info_t):LongWord;cdecl;
  snd_pcm_info_get_sync_t = function(obj:Psnd_pcm_info_t):snd_pcm_sync_id_t;cdecl;
  snd_pcm_info_set_device_t = procedure(obj:Psnd_pcm_info_t; val:LongWord);cdecl;
  snd_pcm_info_set_subdevice_t = procedure(obj:Psnd_pcm_info_t; val:LongWord);cdecl;
  snd_pcm_info_set_stream_t = procedure(obj:Psnd_pcm_info_t; val:snd_pcm_stream_t);cdecl;
    {
       Hardware Parameters
       See the \ref pcm page for more details.
    }
  snd_pcm_hw_params_any_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_can_mmap_sample_resolution_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_is_double_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_is_batch_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_is_block_transfer_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_can_overrange_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_can_pause_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_can_resume_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_is_half_duplex_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_is_joint_duplex_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_can_sync_start_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_get_rate_numden_t = function(params:Psnd_pcm_hw_params_t; rate_num:PLongWord; rate_den:PLongWord):Integer;cdecl;
  snd_pcm_hw_params_get_sbits_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_get_fifo_size_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;

  snd_pcm_hw_params_sizeof_t = function:size_t;cdecl;
  snd_pcm_hw_params_malloc_t = function(Out obj:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_free_t = procedure(obj:Psnd_pcm_hw_params_t);cdecl;
  snd_pcm_hw_params_copy_t = procedure(dst:Psnd_pcm_hw_params_t; src:Psnd_pcm_hw_params_t);cdecl;
  snd_pcm_hw_params_get_access_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_test_access_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_access_t):Integer;cdecl;
  snd_pcm_hw_params_set_access_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_access_t):Integer;cdecl;
  snd_pcm_hw_params_set_access_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_access_t;cdecl;
  snd_pcm_hw_params_set_access_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_access_t;cdecl;
  snd_pcm_hw_params_set_access_mask_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_access_mask_t):Integer;cdecl;
  snd_pcm_hw_params_get_access_mask_t = procedure(params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_access_mask_t);cdecl;
  snd_pcm_hw_params_get_format_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_test_format_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_hw_params_set_format_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_hw_params_set_format_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_format_t;cdecl;
  snd_pcm_hw_params_set_format_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_format_t;cdecl;
  snd_pcm_hw_params_set_format_mask_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_format_mask_t):Integer;cdecl;
  snd_pcm_hw_params_get_format_mask_t = procedure(params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_format_mask_t);cdecl;
  snd_pcm_hw_params_test_subformat_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_subformat_t):Integer;cdecl;
  snd_pcm_hw_params_get_subformat_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_set_subformat_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_subformat_t):Integer;cdecl;
  snd_pcm_hw_params_set_subformat_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_subformat_t;cdecl;
  snd_pcm_hw_params_set_subformat_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_subformat_t;cdecl;
  snd_pcm_hw_params_set_subformat_mask_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_subformat_mask_t):Integer;cdecl;
  snd_pcm_hw_params_get_subformat_mask_t = procedure(params:Psnd_pcm_hw_params_t; mask:Psnd_pcm_subformat_mask_t);cdecl;
  snd_pcm_hw_params_get_channels_t = function(params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_get_channels_min_t = function(params:Psnd_pcm_hw_params_t):LongWord;cdecl;
  snd_pcm_hw_params_get_channels_max_t = function(params:Psnd_pcm_hw_params_t):LongWord;cdecl;
  snd_pcm_hw_params_test_channels_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord):Integer;cdecl;
  snd_pcm_hw_params_set_channels_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord):Integer;cdecl;
  snd_pcm_hw_params_set_channels_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord):Integer;cdecl;
  snd_pcm_hw_params_set_channels_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord):Integer;cdecl;
  snd_pcm_hw_params_set_channels_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; max:PLongWord):Integer;cdecl;
  snd_pcm_hw_params_set_channels_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord):LongWord;cdecl;
  snd_pcm_hw_params_set_channels_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):LongWord;cdecl;
  snd_pcm_hw_params_set_channels_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):LongWord;cdecl;
  snd_pcm_hw_params_get_rate_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_get_rate_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_rate_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_test_rate_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_rate_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_rate_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_rate_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_rate_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; mindir:PCardinal; max:PLongWord;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_rate_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PCardinal; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_rate_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_rate_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_period_time_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_get_period_time_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_period_time_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_test_period_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_time_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_time_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_time_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; mindir:PCardinal; max:PLongWord;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_time_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_period_time_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_period_time_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_period_size_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):snd_pcm_sframes_t;cdecl;
  snd_pcm_hw_params_get_period_size_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_get_period_size_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_test_period_size_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_uframes_t; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_size_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_uframes_t; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_size_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_size_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_size_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:Psnd_pcm_uframes_t; mindir:PCardinal; max:Psnd_pcm_uframes_t;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_period_size_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t; dir:PCardinal):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_set_period_size_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_set_period_size_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_set_period_size_integer_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_get_periods_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_get_periods_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_periods_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_test_periods_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_periods_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_periods_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_periods_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_periods_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; mindir:PCardinal; max:PLongWord;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_periods_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_periods_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_periods_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_periods_integer_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):Integer;cdecl;
  snd_pcm_hw_params_get_buffer_time_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_get_buffer_time_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_buffer_time_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_test_buffer_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_time_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_time_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_time_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; mindir:PCardinal; max:PLongWord;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_time_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_buffer_time_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_buffer_time_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_buffer_size_t = function(params:Psnd_pcm_hw_params_t; frames:Psnd_pcm_sframes_t):LongWord;cdecl;
  snd_pcm_hw_params_get_buffer_size_min_t = function(params:Psnd_pcm_hw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_get_buffer_size_max_t = function(params:Psnd_pcm_hw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_test_buffer_size_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_size_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_size_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_size_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_size_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:Psnd_pcm_uframes_t; max:Psnd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_hw_params_set_buffer_size_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:Psnd_pcm_uframes_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_set_buffer_size_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_set_buffer_size_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_hw_params_get_tick_time_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_get_tick_time_min_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_get_tick_time_max_t = function(params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_test_tick_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_tick_time_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:Cardinal):Integer;cdecl;
  snd_pcm_hw_params_set_tick_time_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_tick_time_max_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:PLongWord; dir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_tick_time_minmax_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; min:PLongWord; mindir:PCardinal; max:PLongWord;
               maxdir:PCardinal):Integer;cdecl;
  snd_pcm_hw_params_set_tick_time_near_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; val:LongWord; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_tick_time_first_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
  snd_pcm_hw_params_set_tick_time_last_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_hw_params_t; dir:PCardinal):LongWord;cdecl;
    { New function added in alsa-lib-0.9.0-rc5 }
  snd_pcm_hw_params_get_min_align_t = function(params: Psnd_pcm_hw_params_t; var val: snd_pcm_uframes_t):Integer;cdecl;

    {
       Software Parameters
    }
    // function snd_pcm_sw_params_current(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t):Integer;cdecl;external asoundlib_name;
  snd_pcm_sw_params_sizeof_t = function:size_t;cdecl;
  snd_pcm_sw_params_malloc_t = function(Out obj:Psnd_pcm_sw_params_t):Integer;cdecl;
  snd_pcm_sw_params_free_t = procedure(obj:Psnd_pcm_sw_params_t);cdecl;
  snd_pcm_sw_params_copy_t = procedure(dst:Psnd_pcm_sw_params_t; src:Psnd_pcm_sw_params_t);cdecl;
  snd_pcm_sw_params_set_tstamp_mode_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_tstamp_t):Integer;cdecl;
  snd_pcm_sw_params_get_tstamp_mode_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_tstamp_t;cdecl;

  snd_pcm_sw_params_set_sleep_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:LongWord):Integer;cdecl;
  snd_pcm_sw_params_get_sleep_min_t = function(params:Psnd_pcm_sw_params_t):LongWord;cdecl;
  snd_pcm_sw_params_set_avail_min_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_avail_min_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_sw_params_set_xfer_align_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_xfer_align_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_sw_params_set_start_threshold_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_start_threshold_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_sw_params_set_stop_threshold_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_stop_threshold_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;

  snd_pcm_sw_params_set_silence_threshold_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_silence_threshold_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_sw_params_set_silence_size_t = function(pcm:Psnd_pcm_t; params:Psnd_pcm_sw_params_t; val:snd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_sw_params_get_silence_size_t = function(params:Psnd_pcm_sw_params_t):snd_pcm_uframes_t;cdecl;

    {
       Access Mask Functions
    }
  snd_pcm_access_mask_sizeof_t = function:size_t;cdecl;
  snd_pcm_access_mask_malloc_t = function(var obj:Psnd_pcm_access_mask_t):Integer;cdecl;
  snd_pcm_access_mask_free_t = procedure(obj:Psnd_pcm_access_mask_t);cdecl;
  snd_pcm_access_mask_copy_t = procedure(dst:Psnd_pcm_access_mask_t; src:Psnd_pcm_access_mask_t);cdecl;
  snd_pcm_access_mask_none_t = procedure(mask:Psnd_pcm_access_mask_t);cdecl;
  snd_pcm_access_mask_any_t = procedure(mask:Psnd_pcm_access_mask_t);cdecl;
  snd_pcm_access_mask_test_t = function(mask:Psnd_pcm_access_mask_t; val:snd_pcm_access_t):Integer;cdecl;
  snd_pcm_access_mask_empty_t = function(mask:Psnd_pcm_access_mask_t):Integer;cdecl;
  snd_pcm_access_mask_set_t = procedure(mask:Psnd_pcm_access_mask_t; val:snd_pcm_access_t);cdecl;
  snd_pcm_access_mask_reset_t = procedure(mask:Psnd_pcm_access_mask_t; val:snd_pcm_access_t);cdecl;
    {
       Format Mask Functions
    }
  snd_pcm_format_mask_sizeof_t = function:size_t;cdecl;
  snd_pcm_format_mask_malloc_t = function(var obj:Psnd_pcm_format_mask_t):Integer;cdecl;
  snd_pcm_format_mask_free_t = procedure(obj:Psnd_pcm_format_mask_t);cdecl;
  snd_pcm_format_mask_copy_t = procedure(dst:Psnd_pcm_format_mask_t; src:Psnd_pcm_format_mask_t);cdecl;
  snd_pcm_format_mask_none_t = procedure(mask:Psnd_pcm_format_mask_t);cdecl;
  snd_pcm_format_mask_any_t = procedure(mask:Psnd_pcm_format_mask_t);cdecl;
  snd_pcm_format_mask_test_t = function(mask:Psnd_pcm_format_mask_t; val:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_mask_empty_t = function(mask:Psnd_pcm_format_mask_t):Integer;cdecl;
  snd_pcm_format_mask_set_t = procedure(mask:Psnd_pcm_format_mask_t; val:snd_pcm_format_t);cdecl;
  snd_pcm_format_mask_reset_t = procedure(mask:Psnd_pcm_format_mask_t; val:snd_pcm_format_t);cdecl;
    {
       Subformat Mask Functions
    }
  snd_pcm_subformat_mask_sizeof_t = function:size_t;cdecl;
  snd_pcm_subformat_mask_malloc_t = function(var obj:Psnd_pcm_subformat_mask_t):Integer;cdecl;
  snd_pcm_subformat_mask_free_t = procedure(obj:Psnd_pcm_subformat_mask_t);cdecl;
  snd_pcm_subformat_mask_copy_t = procedure(dst:Psnd_pcm_subformat_mask_t; src:Psnd_pcm_subformat_mask_t);cdecl;
  snd_pcm_subformat_mask_none_t = procedure(mask:Psnd_pcm_subformat_mask_t);cdecl;
  snd_pcm_subformat_mask_any_t = procedure(mask:Psnd_pcm_subformat_mask_t);cdecl;
  snd_pcm_subformat_mask_test_t = function(mask:Psnd_pcm_subformat_mask_t; val:snd_pcm_subformat_t):Integer;cdecl;
  snd_pcm_subformat_mask_empty_t = function(mask:Psnd_pcm_subformat_mask_t):Integer;cdecl;
  snd_pcm_subformat_mask_set_t = procedure(mask:Psnd_pcm_subformat_mask_t; val:snd_pcm_subformat_t);cdecl;
  snd_pcm_subformat_mask_reset_t = procedure(mask:Psnd_pcm_subformat_mask_t; val:snd_pcm_subformat_t);cdecl;
    {
       Status Functions
    }
  snd_pcm_status_sizeof_t = function:size_t;cdecl;
  snd_pcm_status_malloc_t = function(var obj:Psnd_pcm_status_t):Integer;cdecl;
  snd_pcm_status_free_t = procedure(obj:Psnd_pcm_status_t);cdecl;
  snd_pcm_status_copy_t = procedure(dst:Psnd_pcm_status_t; src:Psnd_pcm_status_t);cdecl;
  snd_pcm_status_get_state_t = function(obj:Psnd_pcm_status_t):snd_pcm_state_t;cdecl;
  snd_pcm_status_get_trigger_tstamp_t = procedure(obj:Psnd_pcm_status_t; ptr:Psnd_timestamp_t);cdecl;
  snd_pcm_status_get_tstamp_t = procedure(obj:Psnd_pcm_status_t; ptr:Psnd_timestamp_t);cdecl;
  snd_pcm_status_get_delay_t = function(obj:Psnd_pcm_status_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_status_get_avail_t = function(obj:Psnd_pcm_status_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_status_get_avail_max_t = function(obj:Psnd_pcm_status_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_status_get_overrange_t = function(obj:Psnd_pcm_status_t):snd_pcm_uframes_t;cdecl;
    {
       Description Functions
    }
  snd_pcm_stream_name_t = function(stream:snd_pcm_stream_t):PAnsiChar;cdecl;
  snd_pcm_access_name_t = function(_access:snd_pcm_access_t):PAnsiChar;cdecl;
  snd_pcm_format_name_t = function(format:snd_pcm_format_t):PAnsiChar;cdecl;
  snd_pcm_format_description_t = function(format:snd_pcm_format_t):PAnsiChar;cdecl;
  snd_pcm_subformat_name_t = function(subformat:snd_pcm_subformat_t):PAnsiChar;cdecl;
  snd_pcm_subformat_description_t = function(subformat:snd_pcm_subformat_t):PAnsiChar;cdecl;
  snd_pcm_format_value_t = function(name:PAnsiChar):snd_pcm_format_t;cdecl;
  snd_pcm_tstamp_mode_name_t = function(mode:snd_pcm_tstamp_t):PAnsiChar;cdecl;
  snd_pcm_state_name_t = function(state:snd_pcm_state_t):PAnsiChar;cdecl;
    {
       Debug Functions
    }
  snd_pcm_dump_t = function(pcm:Psnd_pcm_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_dump_hw_setup_t = function(pcm:Psnd_pcm_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_dump_sw_setup_t = function(pcm:Psnd_pcm_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_dump_setup_t = function(pcm:Psnd_pcm_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_hw_params_dump_t = function(params:Psnd_pcm_hw_params_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_sw_params_dump_t = function(params:Psnd_pcm_sw_params_t; mout:Psnd_output_t):Integer;cdecl;
  snd_pcm_status_dump_t = function(status:Psnd_pcm_status_t; mout:Psnd_output_t):Integer;cdecl;
    {
       Direct Access (MMAP) Functions
    }
  snd_pcm_mmap_begin_t = function(pcm:Psnd_pcm_t; var areas:Psnd_pcm_channel_area_t; offset:Psnd_pcm_uframes_t; frames:Psnd_pcm_uframes_t):Integer;cdecl;
  snd_pcm_mmap_commit_t = function(pcm:Psnd_pcm_t; offset:snd_pcm_uframes_t; frames:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_mmap_writei_t = function(pcm:Psnd_pcm_t; buffer:pointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_mmap_readi_t = function(pcm:Psnd_pcm_t; buffer:pointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_mmap_writen_t = function(pcm:Psnd_pcm_t; bufs:Ppointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_mmap_readn_t = function(pcm:Psnd_pcm_t; bufs:Ppointer; size:snd_pcm_uframes_t):snd_pcm_sframes_t;cdecl;
    {
       Helper Functions
    }
  snd_pcm_format_signed_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_unsigned_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_linear_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_is_float_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_little_endian_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_big_endian_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_cpu_endian_t = function(format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_format_width_t = function(format:snd_pcm_format_t):Integer;cdecl;
    { in bits  }
  snd_pcm_format_physical_width_t = function(format:snd_pcm_format_t):Integer;cdecl;
    { in bits  }
  snd_pcm_build_linear_format_t = function(width:Cardinal; pwidth:Cardinal; unsignd:Cardinal; big_endian:Cardinal):snd_pcm_format_t;cdecl;
  snd_pcm_format_size_t = function(format:snd_pcm_format_t; samples:size_t):ssize_t;cdecl;
  snd_pcm_format_silence_t = function(format:snd_pcm_format_t):Byte;cdecl;
  snd_pcm_format_silence_16_t = function(format:snd_pcm_format_t):Word;cdecl;
  snd_pcm_format_silence_32_t = function(format:snd_pcm_format_t):Integer;cdecl;
//  snd_pcm_format_silence_64_t = function(format:snd_pcm_format_t):u_int64_t;cdecl;
  snd_pcm_format_set_silence_t = function(format:snd_pcm_format_t; buf:pointer; samples:LongWord):Integer;cdecl;
  snd_pcm_bytes_to_frames_t = function(pcm:Psnd_pcm_t; bytes:ssize_t):snd_pcm_sframes_t;cdecl;
  snd_pcm_frames_to_bytes_t = function(pcm:Psnd_pcm_t; frames:snd_pcm_sframes_t):ssize_t;cdecl;
  snd_pcm_bytes_to_samples_t = function(pcm:Psnd_pcm_t; bytes:ssize_t):Integer;cdecl;
  snd_pcm_samples_to_bytes_t = function(pcm:Psnd_pcm_t; samples:Cardinal):ssize_t;cdecl;
  snd_pcm_area_silence_t = function(dst_channel:Psnd_pcm_channel_area_t; dst_offset:snd_pcm_uframes_t; samples:LongWord; format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_areas_silence_t = function(dst_channels:Psnd_pcm_channel_area_t; dst_offset:snd_pcm_uframes_t; channels:LongWord; frames:snd_pcm_uframes_t; format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_area_copy_t = function(dst_channel:Psnd_pcm_channel_area_t; dst_offset:snd_pcm_uframes_t; src_channel:Psnd_pcm_channel_area_t; src_offset:snd_pcm_uframes_t; samples:LongWord;
               format:snd_pcm_format_t):Integer;cdecl;
  snd_pcm_areas_copy_t = function(dst_channels:Psnd_pcm_channel_area_t; dst_offset:snd_pcm_uframes_t; src_channels:Psnd_pcm_channel_area_t; src_offset:snd_pcm_uframes_t; channels:LongWord;
               frames:snd_pcm_uframes_t; format:snd_pcm_format_t):Integer;cdecl;
    {
       Hook Extension
    }
type
    {  type of pcm hook  }
       _snd_pcm_hook_type =  Cardinal;
       snd_pcm_hook_type_t = _snd_pcm_hook_type;
const
         SND_PCM_HOOK_TYPE_HW_PARAMS = 0;
         SND_PCM_HOOK_TYPE_HW_FREE = 1;
         SND_PCM_HOOK_TYPE_CLOSE = 2;
         SND_PCM_HOOK_TYPE_LAST = SND_PCM_HOOK_TYPE_CLOSE;

type
    {  PCM hook container  }
       Psnd_pcm_hook_t = pointer;
    {  PCM hook callback function  }
       snd_pcm_hook_func_t = function (hook:Psnd_pcm_hook_t):Integer;cdecl;
type

  snd_pcm_hook_get_pcm_t = function(hook:Psnd_pcm_hook_t):Psnd_pcm_t;cdecl;
  snd_pcm_hook_get_private_t = function(hook:Psnd_pcm_hook_t):pointer;cdecl;
  snd_pcm_hook_set_private_t = procedure(hook:Psnd_pcm_hook_t; private_data:pointer);cdecl;
  snd_pcm_hook_add_t = function(var hookp:Psnd_pcm_hook_t; pcm:Psnd_pcm_t; _type:snd_pcm_hook_type_t; func:snd_pcm_hook_func_t; private_data:pointer):Integer;cdecl;
  snd_pcm_hook_remove_t = function(hook:Psnd_pcm_hook_t):Integer;cdecl;
    {
       Scope Plugin Extension
    }
type
    {  #SND_PCM_TYPE_METER scope functions  }
       _snd_pcm_scope_ops = record
            enable : function (scope:Psnd_pcm_scope_t):Integer;cdecl; {  Enable and prepare it using current params  }
            disable : procedure (scope:Psnd_pcm_scope_t);             {  Disable  }
            start : procedure (scope:Psnd_pcm_scope_t);               {  PCM has been started  }
            stop : procedure (scope:Psnd_pcm_scope_t);                {  PCM has been stopped  }
            update : procedure (scope:Psnd_pcm_scope_t);              {  New frames are present  }
            reset : procedure (scope:Psnd_pcm_scope_t);               {  Reset status  }
            close : procedure (scope:Psnd_pcm_scope_t);               {  PCM is closing  }
         end;
       snd_pcm_scope_ops_t = _snd_pcm_scope_ops;
       Psnd_pcm_scope_ops_t = ^snd_pcm_scope_ops_t;

  snd_pcm_meter_get_bufsize_t = function(pcm:Psnd_pcm_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_meter_get_channels_t = function(pcm:Psnd_pcm_t):LongWord;cdecl;
  snd_pcm_meter_get_rate_t = function(pcm:Psnd_pcm_t):LongWord;cdecl;
  snd_pcm_meter_get_now_t = function(pcm:Psnd_pcm_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_meter_get_boundary_t = function(pcm:Psnd_pcm_t):snd_pcm_uframes_t;cdecl;
  snd_pcm_meter_add_scope_t = function(pcm:Psnd_pcm_t; scope:Psnd_pcm_scope_t):Integer;cdecl;
  snd_pcm_meter_search_scope_t = function(pcm:Psnd_pcm_t; name:PAnsiChar):Psnd_pcm_scope_t;cdecl;
  snd_pcm_scope_malloc_t = function(var ptr:Psnd_pcm_scope_t):Integer;cdecl;
  snd_pcm_scope_set_ops_t = procedure(scope:Psnd_pcm_scope_t; val:Psnd_pcm_scope_ops_t);cdecl;
  snd_pcm_scope_set_name_t = procedure(scope:Psnd_pcm_scope_t; val:PAnsiChar);cdecl;
  snd_pcm_scope_get_name_t = function(scope:Psnd_pcm_scope_t):PAnsiChar;cdecl;
  snd_pcm_scope_get_callback_private_t = function(scope:Psnd_pcm_scope_t):pointer;cdecl;
  snd_pcm_scope_set_callback_private_t = procedure(scope:Psnd_pcm_scope_t; val:pointer);cdecl;
  snd_pcm_scope_s16_open_t = function(pcm:Psnd_pcm_t; name:PAnsiChar; var scopep:Psnd_pcm_scope_t):Integer;cdecl;
  snd_pcm_scope_s16_get_channel_buffer_t = function(scope:Psnd_pcm_scope_t; channel:LongWord):Pint16;cdecl;

  snd_strerror_t = Function (errnum:Integer):PAnsiChar; Cdecl;
var

  snd_pcm_close : snd_pcm_close_t;
  snd_pcm_drop : snd_pcm_drop_t;
  snd_pcm_drain : snd_pcm_drain_t;
  snd_pcm_wait: snd_pcm_wait_t;
  snd_pcm_avail_update:snd_pcm_avail_update_t;
  snd_pcm_hw_params_malloc : snd_pcm_hw_params_malloc_t;
  snd_pcm_hw_params_any : snd_pcm_hw_params_any_t;
  snd_pcm_hw_params_get_periods : snd_pcm_hw_params_get_periods_t;
  snd_pcm_hw_params_get_period_size : snd_pcm_hw_params_get_period_size_t;
  snd_pcm_hw_params_get_rate : snd_pcm_hw_params_get_rate_t;
  snd_pcm_hw_params_get_buffer_size: snd_pcm_hw_params_get_buffer_size_t;
  snd_pcm_hw_params_set_access : snd_pcm_hw_params_set_access_t;
  snd_pcm_hw_params_set_buffer_size : snd_pcm_hw_params_set_buffer_size_t;
  snd_pcm_hw_params_set_buffer_size_near : snd_pcm_hw_params_set_buffer_size_near_t;
  snd_pcm_hw_params_set_channels : snd_pcm_hw_params_set_channels_t;
  snd_pcm_hw_params_set_format : snd_pcm_hw_params_set_format_t;
  snd_pcm_hw_params_set_periods:snd_pcm_hw_params_set_periods_t;
  snd_pcm_hw_params_set_period_size_near : snd_pcm_hw_params_set_period_size_near_t;
  snd_pcm_hw_params_set_periods_near : snd_pcm_hw_params_set_periods_near_t;
  snd_pcm_hw_params_set_rate_near : snd_pcm_hw_params_set_rate_near_t;
  snd_pcm_hw_params : snd_pcm_hw_params_t;
  snd_pcm_hw_params_free : snd_pcm_hw_params_free_t;

  snd_pcm_sw_params:snd_pcm_sw_params_t;
  snd_pcm_sw_params_malloc:snd_pcm_sw_params_malloc_t;
  snd_pcm_sw_params_free:snd_pcm_sw_params_free_t;
  snd_pcm_sw_params_current: snd_pcm_sw_params_current_t;
  snd_pcm_sw_params_set_avail_min:snd_pcm_sw_params_set_avail_min_t;
  snd_pcm_sw_params_set_start_threshold:snd_pcm_sw_params_set_start_threshold_t;

  snd_pcm_info_get_device : snd_pcm_info_get_device_t;
  snd_pcm_info_get_name : snd_pcm_info_get_name_t;
  snd_pcm_info_set_device : snd_pcm_info_set_device_t;
  snd_pcm_mmap_readi : snd_pcm_mmap_readi_t;
  snd_pcm_mmap_writei : snd_pcm_mmap_writei_t;
  snd_pcm_open : snd_pcm_open_t;
  snd_pcm_pause : snd_pcm_pause_t;
  snd_pcm_prepare : snd_pcm_prepare_t;
  snd_pcm_readi : snd_pcm_readi_t;
  snd_pcm_reset : snd_pcm_reset_t;
  snd_pcm_resume : snd_pcm_resume_t;
  snd_pcm_state : snd_pcm_state_t_t;
  snd_pcm_stream : snd_pcm_stream_t_t;
  snd_pcm_writei : snd_pcm_writei_t;

  snd_async_add_pcm_handler:snd_async_add_pcm_handler_t;

  snd_strerror:snd_strerror_t;

// Mixer types

type

  psnd_ctl_card_info_t = Pointer;
  ppsnd_ctl_card_info_t = ^psnd_ctl_card_info_t;
  psnd_ctl_t = Pointer;
  ppsnd_ctl_t = ^psnd_ctl_t;


type

  snd_ctl_card_info_sizeof_t = function : Integer; cdecl;
  snd_ctl_card_info_malloc_t = function(ptr : ppsnd_ctl_card_info_t) : Integer; cdecl;
  snd_ctl_card_info_free_t = procedure(obj : psnd_ctl_card_info_t); cdecl;
  snd_ctl_card_info_clear_t = procedure(obj : psnd_ctl_card_info_t); cdecl;
  snd_ctl_card_info_copy_t = procedure(dst, src : psnd_ctl_card_info_t); cdecl;
  snd_ctl_card_info_get_card_t = function(obj : psnd_ctl_card_info_t) : Integer; cdecl;
  snd_ctl_card_info_get_id_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;
  snd_ctl_card_info_get_driver_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;
  snd_ctl_card_info_get_name_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;
  snd_ctl_card_info_get_longname_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;
  snd_ctl_card_info_get_mixername_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;
  snd_ctl_card_info_get_components_t = function(obj : psnd_ctl_card_info_t) : PAnsiChar; cdecl;


Procedure LoadAlsa();

Implementation
Uses dynlibs;

Var
  LibHandle:TLibHandle;

Procedure LoadAlsa();
Begin
  If Libhandle<>0 Then
    Exit;

  Libhandle := LoadLibrary(AlsaLib);
  If Libhandle<>0 Then
  Begin
    snd_pcm_close := GetProcedureAddress(Libhandle, 'snd_pcm_close');
    snd_pcm_drop := GetProcedureAddress(Libhandle, 'snd_pcm_drop');
    snd_pcm_drain := GetProcedureAddress(Libhandle, 'snd_pcm_drain');
    snd_pcm_wait := GetProcedureAddress(Libhandle, 'snd_pcm_wait');
    snd_pcm_avail_update := GetProcedureAddress(Libhandle, 'snd_pcm_avail_update');
    snd_pcm_hw_params_malloc := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_malloc');
    snd_pcm_hw_params_any := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_any');
    snd_pcm_hw_params_get_periods := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_get_periods');
    snd_pcm_hw_params_get_period_size := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_get_period_size');
    snd_pcm_hw_params_get_rate := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_get_rate');
    snd_pcm_hw_params_get_buffer_size := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_get_buffer_size');
    snd_pcm_hw_params_set_access := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_access');
    snd_pcm_hw_params_set_buffer_size := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_buffer_size');
    snd_pcm_hw_params_set_buffer_size_near := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_buffer_size_near');
    snd_pcm_hw_params_set_channels := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_channels');
    snd_pcm_hw_params_set_format := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_format');
    snd_pcm_hw_params_set_period_size_near := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_period_size_near');
    snd_pcm_hw_params_set_periods := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_periods');
    snd_pcm_hw_params_set_periods_near := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_periods_near');
    snd_pcm_hw_params_set_rate_near := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_set_rate_near');
    snd_pcm_hw_params := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params');
    snd_pcm_hw_params_free := GetProcedureAddress(Libhandle, 'snd_pcm_hw_params_free');

    snd_pcm_sw_params := GetProcedureAddress(Libhandle, 'snd_pcm_sw_params');
    snd_pcm_sw_params_malloc  := GetProcedureAddress(Libhandle, 'snd_pcm_sw_params_malloc');
    snd_pcm_sw_params_free  := GetProcedureAddress(Libhandle, 'snd_pcm_sw_params_free');
    snd_pcm_sw_params_current := GetProcedureAddress( Libhandle, 'snd_pcm_sw_params_current');
    snd_pcm_sw_params_set_avail_min := GetProcedureAddress( Libhandle, 'snd_pcm_sw_params_set_avail_min');
    snd_pcm_sw_params_set_start_threshold := GetProcedureAddress( Libhandle, 'snd_pcm_sw_params_set_start_threshold');

    snd_pcm_info_get_device := GetProcedureAddress(Libhandle, 'snd_pcm_info_get_device');
    snd_pcm_info_get_name := GetProcedureAddress(Libhandle, 'snd_pcm_info_get_name');
    snd_pcm_info_set_device := GetProcedureAddress(Libhandle, 'snd_pcm_info_set_device');
    snd_pcm_mmap_readi := GetProcedureAddress(Libhandle, 'snd_pcm_mmap_readi');
    snd_pcm_mmap_writei := GetProcedureAddress(Libhandle, 'snd_pcm_mmap_writei');
    snd_pcm_open := GetProcedureAddress(Libhandle, 'snd_pcm_open');
    snd_pcm_pause := GetProcedureAddress(Libhandle, 'snd_pcm_pause');
    snd_pcm_prepare := GetProcedureAddress(Libhandle, 'snd_pcm_prepare');
    snd_pcm_readi := GetProcedureAddress(Libhandle, 'snd_pcm_readi');
    snd_pcm_reset := GetProcedureAddress(Libhandle, 'snd_pcm_reset');
    snd_pcm_resume := GetProcedureAddress(Libhandle, 'snd_pcm_resume');
    snd_pcm_state := GetProcedureAddress(Libhandle, 'snd_pcm_state');
    snd_pcm_stream := GetProcedureAddress(Libhandle, 'snd_pcm_stream');
    snd_pcm_writei := GetProcedureAddress(Libhandle, 'snd_pcm_writei');

    snd_async_add_pcm_handler := GetProcedureAddress(Libhandle, 'snd_async_add_pcm_handler');

    snd_strerror  := GetProcedureAddress(Libhandle, 'snd_strerror');
  End;
End;

Initialization
Finalization
  If Libhandle<>0 Then
    UnloadLibrary(Libhandle);
End.

