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
 * TERRA_AudioMixer
 * Implements a software audio mixer
 ***********************************************************************************************************************
}
Unit TERRA_AudioMixer;
{$I terra.inc}

Interface

Uses TERRA_Utils, TERRA_ThreadPool, TERRA_Vector3D, TERRA_Matrix;

Const
  HRIR_BITS = 5;
  HRIR_LENGTH  = 1 Shl HRIR_BITS;
  HRIR_MASK = (HRIR_LENGTH-1);
  HRIR_COUNT = 828;
  ELEV_COUNT = 19;

  BUFFERSIZE = 4096;

  MAX_SENDS  = 4;

  SRC_HISTORY_BITS = 6;
  SRC_HISTORY_LENGTH = (1 Shl SRC_HISTORY_BITS);
  SRC_HISTORY_MASK  = (SRC_HISTORY_LENGTH-1);

  FRACTIONBITS = 1;
  FRACTIONONE  = (1 Shl FRACTIONBITS);
  FRACTIONMASK = (FRACTIONONE-1);

  QUADRANT_NUM  = 128;
  LUT_NUM       = (4 * QUADRANT_NUM);

  CHANNEL_FRONT_LEFT    = 0;
  CHANNEL_FRONT_RIGHT   = 1;
  CHANNEL_FRONT_CENTER  = 2;
  CHANNEL_LFE           = 3;
  CHANNEL_BACK_LEFT     = 4;
  CHANNEL_BACK_RIGHT    = 5;
  CHANNEL_BACK_CENTER   = 6;
  CHANNEL_SIDE_LEFT     = 7;
  CHANNEL_SIDE_RIGHT    = 8;
  MAXCHANNELS = 9;

  devcap_Playback = 1;
  devcap_Capture  = 2;


  EAXREVERB     = 0;
  REVERB        = 1;
  ECHO          = 2;
  MODULATOR     = 3;
  DEDICATED     = 4;
  MAX_EFFECTS   = 5;

Type
  PALCdevice=^ALCDevice;
  PALCcontext=^ALCContext;

  UIntMapEntry = Record
    key:Integer;
    value:Pointer;
  End;

  UIntMap = Record
    entries:Array Of UIntMapEntry;
    size:Integer;
    maxsize:Integer;
    limit:Integer;
    //RWLock lock;
  End;

  EffectEntry = Record
    name:AnsiString;
    effecttype:Integer;
    ename:AnsiString;
    val:Integer;
  End;

  Hrtf = Record
    sampleRate:Integer;
    coeffs:Array[0..Pred(HRIR_COUNT), 0..Pred(HRIR_LENGTH)] Of SmallInt; // short
    delays:Array[0..Pred(HRIR_COUNT)] Of Byte;
  End;

  Last_sample = Record
    asis:Double;
    lo:Double;
    hi:Double;
  End;

  Bs2b = Record
    level:Integer;  // Crossfeed level
    srate:Integer;   // Sample rate (Hz)

    // Lowpass IIR filter coefficients
    a0_lo:double;
    b1_lo:double;

    // Highboost IIR filter coefficients
    a0_hi:double;
    a1_hi:double;
    b1_hi:double;

    // Global gain against overloading
    gain:Single;

    // Buffer of last filtered sample.  [0] - first channel, [1] - second channel
    last_sample:Array[0..1] Of last_sample;
  End;

  PALeffectslot = ^ALeffectslot;

  ALeffectState = Class
    Destructor Destroy; Virtual; Abstract;
    Function DeviceUpdate(Device:PALCDevice):Boolean; Virtual; Abstract;
    Procedure Update(Device:PALCdevice; Slot:PALeffectslot); Virtual; Abstract;
    Procedure Process(SamplesToDo:Integer; SamplesIn, SamplesOut:PSingleArray); Virtual; Abstract;
  End;

  ALeffectslot = Record
    effect:Integer;

    Gain:Single;
    AuxSendAuto:Boolean;

    NeedsUpdate:Integer;
    EffectState:ALeffectState;

    WetBuffer:Array[0..Pred(BUFFERSIZE)] Of Single;

    ClickRemoval:Single;
    PendingClicks:Single;

    // Index to itself
    effectslot:Integer;

    next:PALeffectslot;
  End;

  AudioBackEnd = Class
    Protected
      _Name:AnsiString;

    Public
      Destructor Destroy; Virtual; Abstract;

      Function Init():Boolean; Virtual; Abstract;
      Function GetCapabilities():Integer; Virtual; Abstract;

      Function OpenPlayback(Device:PALCdevice; S:AnsiString):Integer; Virtual; Abstract;
      Procedure ClosePlayback(Device:PALCdevice); Virtual; Abstract;
      Function ResetPlayback(Device:PALCdevice):Boolean; Virtual; Abstract;
      Procedure StartPlayback(Device:PALCdevice); Virtual; Abstract;
      Procedure StopPlayback(Device:PALCdevice); Virtual; Abstract;

      Function OpenCapture(Device:PALCdevice; S:AnsiString):Integer; Virtual; Abstract;
      Procedure CloseCapture(Device:PALCdevice); Virtual; Abstract;
      Procedure StartCapture(Device:PALCdevice); Virtual; Abstract;
      Procedure StopCapture(Device:PALCdevice); Virtual; Abstract;
      Function CaptureSamples(Device:PALCdevice; Buffer:Pointer; N:Integer):Integer; Virtual; Abstract;
      Function AvailableSamples(Device:PALCdevice):Integer; Virtual; Abstract;

      Property Name:AnsiString Read _Name;
  End;

  PALlistener = ^ALlistener;
  ALlistener = Record
    Position:Vector3D;
    Velocity:Vector3D;
    Forward:Vector3D;
    Up:Vector3D;
    Matrix:Matrix;
    Gain:Single;
    MetersPerUnit:Single;
  End;

  PALbuffer = ^ALbuffer;
  ALbuffer = Record
    data:Pointer;

    Frequency:Integer;
    Format:Integer;
    SampleLen:Integer;

    FmtChannels:Integer;
    FmtType:Integer;

    OriginalChannels:Integer;
    OriginalType:Integer;
    OriginalSize:Integer;

    LoopStart:Integer;
    LoopEnd:Integer;

    // Index to itself
    buffer:Integer;
  End;

  PALbufferlistitem = ^ALbufferlistitem;
  ALbufferlistitem = Record
    buffer:PALbuffer;
    next:PALbufferlistitem;
    prev:PALbufferlistitem;
  End;

  ALFilter = Class
    // Filter type (AL_FILTER_NULL, ...)
    filtertype:Integer;

    // Index to itself
    filter:Integer;

    Gain:Single;
    GainHF:Single;

    Procedure SetParami(param, val:Integer);
    Procedure SetParamiv(param:Integer;  vals:PInteger);
    Procedure SetParamf(param:Integer; val:Single);
    Procedure SetParamfv(param:Integer; vals:PSingle);

    Procedure GetParami(param:Integer; Var val:Integer);
    Procedure GetParamiv(param:Integer; vals:PIntegerArray);
    Procedure GetParamf(param:Integer; Var val:Single);
    Procedure GetParamfv(param:Integer; vals:PSingleArray);
  End;


  ALSourceSend = Record
    Slot:PALeffectslot;
    WetGain:Single;
    WetGainHF:Single;
  End;

  ALMixerSend = Record
    Slot:PALeffectslot;
    WetGain:Single;
    iirFilter:ALFilter;
    history:Array[0..Pred(MAXCHANNELS)] Of Single;
  End;

  ALMixerParams = Record
    //MixerFunc DoMix;

    Step:Integer;

    HrtfGain:Single;
    HrtfDir:Array[0..2] Of Single;
    HrtfCoeffs:Array[0..Pred(MAXCHANNELS), 0..Pred(HRIR_LENGTH), 0..1] Of Single;
    HrtfDelay:Array[0..Pred(MAXCHANNELS), 0..1] Of Cardinal;
    HrtfCoeffStep:Array[0..Pred(HRIR_LENGTH), 0..1] Of Single;
    HrtfDelayStep:Array[0..1] Of Integer;

    { A mixing matrix. First subscript is the channel number of the input
      data (regardless of channel configuration) and the second is the
      channel target (eg. FRONT_LEFT) }
    DryGains:Array[0..Pred(MAXCHANNELS), 0..Pred(MAXCHANNELS)] Of Single;

    iirFilter:ALFILTER ;
    history:Array[0..Pred(MAXCHANNELS*2)] Of Single;
    Send:Array[0..Pred(MAX_SENDS)] Of ALMixerSend;
  End;

  ALsource = Class
    flPitch:Single;
    flGain:Single;
    flOuterGain:Single;
    flMinGain:Single;
    flMaxGain:Single;
    flInnerAngle:Single;
    flOuterAngle:Single;
    flRefDistance:Single;
    flMaxDistance:Single;
    flRollOffFactor:Single;
    vPosition:Vector3D;
    vVelocity:Vector3D;
    vOrientation:Vector3D;
    bHeadRelative:Boolean;
    bLooping:Boolean;
    DistanceModel:Integer;
    DirectChannels:Boolean;

    Resampler:Integer;

    state:Integer;
    new_state:Integer;
    position:Integer;
    position_fraction:Integer;

    queue:ALbufferlistitem; // Linked list of buffers in queue
    BuffersInQueue:Integer;   // Number of buffers in queue
    BuffersPlayed:Integer;    // Number of buffers played on this loop

    DirectGain:Single;
    DirectGainHF:Single;

    Send:Array[0..Pred(MAX_SENDS)] Of ALSourceSend;

    DryGainHFAuto:Boolean;
    WetGainAuto:Boolean;
    WetGainHFAuto:Boolean;
    OuterGainHF:Single;

    AirAbsorptionFactor:Single;
    RoomRolloffFactor:Single;
    DopplerFactor:Single;

    lOffset:Integer;
    lOffsetType:Integer;

    // Source Type (Static, Streaming, or Undetermined)
    lSourceType:Integer;

    NumChannels:Integer;
    SampleSize:Integer;

    // HRTF info
    HrtfMoving:Boolean;
    HrtfCounter:Cardinal;
    HrtfHistory:Array[0..Pred(MAXCHANNELS), 0.. Pred(SRC_HISTORY_LENGTH)] Of Single;
    HrtfValues:Array[0..Pred(MAXCHANNELS), 0..Pred(HRIR_LENGTH), 0..1] Of Single;
    HrtfOffset:Cardinal;

    // Current target parameters used for mixing
    Params:ALMixerparams;
    NeedsUpdate:Boolean;

    // Index to itself
    source:Integer;

    Constructor Create;
    Procedure Update(); Virtual; Abstract;
  End;

  ALCDevice = Record
    Connected:Boolean;
    DeviceType:Integer;

    Mutex:CriticalSection;

    Frequency:Cardinal;
    UpdateSize:Cardinal;
    NumUpdates:Cardinal;
    FmtChans:Integer;
    FmtType:Integer;

    DeviceName:AnsiString;

    LastError:Integer;

    // Maximum number of sources that can be created
    MaxNoOfSources:Integer;

    // Maximum number of slots that can be created
    AuxiliaryEffectSlotMax:Integer;

    NumMonoSources:Integer;
    NumStereoSources:Integer;
    NumAuxSends:Integer;

    // Map of Buffers for this device
    BufferMap:UIntMap;

    // Map of Effects for this device
    EffectMap:UIntMap ;

    // Map of Filters for this device
    FilterMap:UIntMap ;

    // HRTF filter tables
    Hrtf:^Hrtf;

    // Stereo-to-binaural filter
    bs2b:^Bs2b;
    Bs2bLevel:Integer;

    // Device flags
    Flags:Cardinal;

    // Dry path buffer mix
    DryBuffer:Array[0..Pred(BUFFERSIZE), 0..Pred(MAXCHANNELS)] Of Single;

    DevChannels:Array[0..Pred(MAXCHANNELS)] Of Integer;

    Speaker2Chan:Array[0..Pred(MAXCHANNELS)] Of Integer;
    PanningLUT:Array[0..Pred(LUT_NUM), 0..Pred(MAXCHANNELS)] Of Single;
    NumChan:Integer;

    ClickRemoval:Array[0..Pred(MAXCHANNELS)] Of Single;
    PendingClicks:Array[0..Pred(MAXCHANNELS)] Of Single;

    // Default effect slot
    DefaultSlot:PALeffectslot;

    Funcs:AudioBackend;
    Next:PALCDevice;
  End;

  ALCContext = Record
    Listener:PALlistener;

    SourceMap:UIntMap;
    EffectSlotMap:UIntMap;

    LastError:Integer;

    UpdateSources:Boolean;

    DistanceModel:Integer;
    SourceDistanceModel:Boolean;

    DopplerFactor:Single;
    DopplerVelocity:Single;
    flSpeedOfSound:Single;
    DeferUpdates:Boolean;

    ActiveSources:Array Of Integer;
    ActiveSourceCount:Integer;
    MaxActiveSources:Integer;

    ActiveEffectSlots:Array Of Integer;
    ActiveEffectSlotCount:Integer;
    MaxActiveEffectSlots:Integer;

    Device:ALCdevice;
    ExtensionList:AnsiString;
  End;

Const
  //bad value
  AL_INVALID                                = -1;

  AL_NONE                                   = 0;

  //Boolean False.
  AL_FALSE                                  = 0;

  //Boolean True.
  AL_TRUE                                   = 1;

  //Indicate the type of AL_SOURCE.
  //Sources can be spatialized
  AL_SOURCE_TYPE                            = $200;

  //Indicate source has absolute coordinates.
  AL_SOURCE_ABSOLUTE                       = $201;

  //Indicate Source has relative coordinates.
  AL_SOURCE_RELATIVE                       = $202;

  //Directional source, inner cone angle, in degrees.
  //Range:    [0-360]
  //Default:  360
  AL_CONE_INNER_ANGLE                      = $1001;

  //Directional source, outer cone angle, in degrees.
  //Range:    [0-360]
  //Default:  360
  AL_CONE_OUTER_ANGLE                       = $1002;

  //Specify the pitch to be applied, either at source,
  //or on mixer results, at listener.
  //Range:   [0.5-2.0]
  //Default: 1.0
  AL_PITCH                                  =$1003;

  //Specify the current location in three dimensional space.
  //OpenAL, like OpenGL, uses a right handed coordinate system,
  //where in a frontal default view X (thumb) points right,
  //Y points up (index finger), and Z points towards the
  //viewer/camera (middle finger).
  //To switch from a left handed coordinate system, flip the
  //sign on the Z coordinate.
  //Listener position is always in the world coordinate system.
  AL_POSITION                               =$1004;

  //Specify the current direction.
  AL_DIRECTION                              =$1005;

  // Specify the current velocity in three dimensional space.
  AL_VELOCITY                               =$1006;

  //Indicate whether source is looping.
  //Type: ALboolean?
  //Range:   [AL_TRUE, AL_FALSE]
  //Default: FALSE.
  AL_LOOPING                                =$1007;

  //Indicate the buffer to provide sound samples.
  //Type: ALuint.
  //Range: any valid Buffer id.
  AL_BUFFER                                 =$1009;

  AL_METERS_PER_UNIT                       =$20004;

  //Indicate the gain (volume amplification) applied.
  //Type:   ALfloat.
  //Range:  ]0.0-  ]
  //A value of 1.0 means un-attenuated/unchanged.
  //Each division by 2 equals an attenuation of -6dB.
  //Each multiplicaton with 2 equals an amplification of +6dB.
  //A value of 0.0 is meaningless with respect to a logarithmic
  //scale; it is interpreted as zero volume - the channel
  //is effectively disabled.
  AL_GAIN                                   =$100A;

  //Indicate minimum source attenuation
  //Type: ALfloat
  //Range:  [0.0 - 1.0]
  //Logarthmic
  AL_MIN_GAIN                               =$100D;

  //Indicate maximum source attenuation
  //Type: ALfloat
  //Range:  [0.0 - 1.0]
  //Logarthmic
  AL_MAX_GAIN                               =$100E;

  //Indicate listener orientation.
  //at/up
  AL_ORIENTATION                            =$100F;

  //Specify the channel mask. (Creative)
  //Type:	 ALuint
  //Range:	 [0 - 255]
  AL_CHANNEL_MASK                           =$3000;

  //Source state information.
  AL_SOURCE_STATE                           =$1010;
  AL_INITIAL                                =$1011;
  AL_PLAYING                                =$1012;
  AL_PAUSED                                 =$1013;
  AL_STOPPED                                =$1014;

  //Buffer Queue params
  AL_BUFFERS_QUEUED                         =$1015;
  AL_BUFFERS_PROCESSED                      =$1016;

  //Sound samples: format specifier.
  AL_FORMAT_MONO8                           =$1100;
  AL_FORMAT_MONO16                          =$1101;
  AL_FORMAT_STEREO8                         =$1102;
  AL_FORMAT_STEREO16                        =$1103;

  AL_DEFERRED_UPDATES_SOFT                 = $C002;

  //source specific reference distance
  //Type: ALfloat
  //Range:  0.0 - +inf
  //At 0.0, no distance attenuation occurs.  Default is
  //1.0.
  AL_REFERENCE_DISTANCE                     =$1020;

  //source specific rolloff factor
  //Type: ALfloat
  //Range:  0.0 - +inf
  AL_ROLLOFF_FACTOR                         =$1021;

  //Directional source, outer cone gain.
  //Default:  0.0
  //Range:    [0.0 - 1.0]
  //Logarithmic
  AL_CONE_OUTER_GAIN                        =$1022;

  //Indicate distance above which sources are not
  //attenuated using the inverse clamped distance model.
  //Default: +inf
  //Type: ALfloat
  //Range:  0.0 - +inf
  AL_MAX_DISTANCE                           =$1023;

  //Sound samples: frequency, in units of Hertz [Hz].
  //This is the number of samples per second. Half of the
  //sample frequency marks the maximum significant
  //frequency component.
  AL_FREQUENCY                              =$2001;
  AL_BITS                                   =$2002;
  AL_CHANNELS                               =$2003;
  AL_SIZE                                   =$2004;
  AL_DATA                                   =$2005;

  //Buffer state.
  //Not supported for public use (yet).
  AL_UNUSED                                 =$2010;
  AL_PENDING                                =$2011;
  AL_PROCESSED                              =$2012;

  AL_SOURCE_DISTANCE_MODEL                 = $200;


  //Errors: No Error.
  AL_NO_ERROR                               =AL_FALSE;

  //Invalid Name paramater passed to AL call.
  AL_INVALID_NAME                           =$A001;

  //Invalid parameter passed to AL call.
  AL_ILLEGAL_ENUM                           =$A002;
  AL_INVALID_ENUM                           =$A002;

  //Invalid enum parameter value.
  AL_INVALID_VALUE                          =$A003;

  //Illegal call.
  AL_ILLEGAL_COMMAND                        =$A004;
  AL_INVALID_OPERATION                      =$A004;

  //No mojo.
  AL_OUT_OF_MEMORY                          =$A005;

  // Context strings: Vendor Name.
  AL_VENDOR                                 =$B001;
  AL_VERSION                                =$B002;
  AL_GraphicsManager                               =$B003;
  AL_EXTENSIONS                             =$B004;

  // Global tweakage.

  // Doppler scale.  Default 1.0
  AL_DOPPLER_FACTOR                         =$C000;

  // Tweaks speed of propagation.
  AL_DOPPLER_VELOCITY                       = $C001;

  AL_SPEED_OF_SOUND                         = $C003;

  // Distance models
  //
  // used in conjunction with DistanceModel
  //
  // implicit: NONE, which disances distance attenuation.
  AL_DISTANCE_MODEL                         =$D000;
  AL_INVERSE_DISTANCE                       =$D001;
  AL_INVERSE_DISTANCE_CLAMPED               =$D002;
  AL_LINEAR_DISTANCE                        =$D003;
  AL_LINEAR_DISTANCE_CLAMPED                =$D004;
  AL_EXPONENT_DISTANCE                      =$D005;
  AL_EXPONENT_DISTANCE_CLAMPED              =$D006;


  //bad value
  ALC_INVALID                              =0;

  //Boolean False.
  ALC_FALSE                                =0;

  //Boolean True.
  ALC_TRUE                                 =1;

  //followed by <int> Hz
  ALC_FREQUENCY                            =$1007;

  //followed by <int> Hz
  ALC_REFRESH                              =$1008;

  //followed by AL_TRUE, AL_FALSE
  ALC_SYNC                                 =$1009;

  //errors

  //No error
  ALC_NO_ERROR                             =ALC_FALSE;

  //No device
  ALC_INVALID_DEVICE                       =$A001;

  //invalid context ID
  ALC_INVALID_CONTEXT                      =$A002;

  //bad enum
  ALC_INVALID_ENUM                         =$A003;

  //bad value
  ALC_INVALID_VALUE                        =$A004;

  //Out of memory.
  ALC_OUT_OF_MEMORY                        =$A005;

  //The Specifier string for default device
  ALC_DEFAULT_DEVICE_SPECIFIER             =$1004;
  ALC_DEVICE_SPECIFIER                     =$1005;
  ALC_EXTENSIONS                           =$1006;

  ALC_CAPTURE_DEVICE_SPECIFIER		        = $310;
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER    = $311;
  ALC_CAPTURE_SAMPLES			                = $312;

  ALC_MAJOR_VERSION                        =$1000;
  ALC_MINOR_VERSION                        =$1001;

  ALC_ATTRIBUTES_SIZE                      =$1002;
  ALC_ALL_ATTRIBUTES                       =$1003;

  AL_REVERB_DENSITY                        = $0001;
  AL_REVERB_DIFFUSION                      = $0002;
  AL_REVERB_GAIN                           = $0003;
  AL_REVERB_GAINHF                         = $0004;
  AL_REVERB_DECAY_TIME                     = $0005;
  AL_REVERB_DECAY_HFRATIO                  = $0006;
  AL_REVERB_REFLECTIONS_GAIN               = $0007;
  AL_REVERB_REFLECTIONS_DELAY              = $0008;
  AL_REVERB_LATE_REVERB_GAIN               = $0009;
  AL_REVERB_LATE_REVERB_DELAY              = $000A;
  AL_REVERB_AIR_ABSORPTION_GAINHF          = $000B;
  AL_REVERB_ROOM_ROLLOFF_FACTOR            = $000C;
  AL_REVERB_DECAY_HFLIMIT                  = $000D;

  //Source definitions to be used with alSource functions.
  //These values must be unique and not conflict with other
  //al source values.
  AL_DIRECT_FILTER                         =$20005;
  AL_AUXILIARY_SEND_FILTER                 =$20006;
  AL_AIR_ABSORPTION_FACTOR                 =$20007;
  AL_ROOM_ROLLOFF_FACTOR                   =$20008;
  AL_CONE_OUTER_GAINHF                     =$20009;
  AL_DIRECT_FILTER_GAINHF_AUTO             =$2000A;
  AL_AUXILIARY_SEND_FILTER_GAIN_AUTO       =$2000B;
  AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO     =$2000C;

  // Filter type definitions to be used with AL_FILTER_TYPE.
  AL_FILTER_NULL                           =$0000;  // Can also be used as a Filter Object ID
  AL_FILTER_LOWPASS                        =$0001;
  AL_FILTER_HIGHPASS                       =$0002;
  AL_FILTER_BANDPASS                       =$0003;

  //Auxiliary Slot object definitions to be used with alAuxiliaryEffectSlot functions.
  AL_EFFECTSLOT_EFFECT                     = $0001;
  AL_EFFECTSLOT_GAIN                       = $0002;
  AL_EFFECTSLOT_AUXILIARY_SEND_AUTO        = $0003;

  //Value to be used as an Auxiliary Slot ID to disable a source send..
  AL_EFFECTSLOT_NULL                       = $0000;

  //Effect type definitions to be used with AL_EFFECT_TYPE.
  AL_EFFECT_NULL                           = $0000;  // Can also be used as an Effect Object ID
  AL_EFFECT_REVERB                         = $0001;
  AL_EFFECT_CHORUS                         = $0002;
  AL_EFFECT_DISTORTION                     = $0003;
  AL_EFFECT_ECHO                           = $0004;
  AL_EFFECT_FLANGER                        = $0005;
  AL_EFFECT_FREQUENCY_SHIFTER              = $0006;
  AL_EFFECT_VOCAL_MORPHER                  = $0007;
  AL_EFFECT_PITCH_SHIFTER                  = $0008;
  AL_EFFECT_RING_MODULATOR                 = $0009;
  AL_EFFECT_AUTOWAH                        = $000A;
  AL_EFFECT_COMPRESSOR                     = $000B;
  AL_EFFECT_EQUALIZER                      = $000C;

  //Effect type
  AL_EFFECT_FIRST_PARAMETER                = $0000;
  AL_EFFECT_LAST_PARAMETER                 = $8000;
  AL_EFFECT_TYPE                           = $8001;

  //Effect type definitions to be used with AL_EFFECT_TYPE.
  AL_EFFECT_EAXREVERB                      = $8000;

{  UserFmtByte   = AL_BYTE;
  UserFmtUByte  = AL_UNSIGNED_BYTE_SOFT,
  UserFmtShort  = AL_SHORT_SOFT,
  UserFmtUShort = AL_UNSIGNED_SHORT_SOFT,
  UserFmtInt    = AL_INT_SOFT,
  UserFmtUInt   = AL_UNSIGNED_INT_SOFT,
  UserFmtFloat  = AL_FLOAT_SOFT,
  UserFmtDouble = AL_DOUBLE_SOFT,
  UserFmtByte3  = AL_BYTE3_SOFT,
  UserFmtUByte3 = AL_UNSIGNED_BYTE3_SOFT,
  UserFmtMulaw  = 9997;
  UserFmtAlaw   = 9998;
  UserFmtIMA4   = 9999;

  UserFmtMono   = AL_MONO_SOFT;
  UserFmtStereo = AL_STEREO_SOFT;
  UserFmtRear   = AL_REAR_SOFT;
  UserFmtQuad   = AL_QUAD_SOFT;
  UserFmtX51    = AL_5POINT1_SOFT; /* (WFX order) */
  UserFmtX61    = AL_6POINT1_SOFT; /* (WFX order) */
  UserFmtX71    = AL_7POINT1_SOFT; /* (WFX order) */
}

  //GraphicsManager State management.
  Procedure alEnable(capability: Integer); CDecl;
  Procedure alDisable(capability: Integer); CDecl;
  Function alIsEnabled(capability: Integer):Boolean; CDecl;

  //State retrieval.through return value ( for compatibility )
  Function alGetBoolean(param: Integer): Boolean; CDecl;
  Function alGetInteger(param: Integer): Integer; CDecl;
  Function alGetFloat(param: Integer): Single; CDecl;
  Function alGetDouble(param: Integer): Double; CDecl;

  //State retrieval.
  Procedure alGetBooleanv(param: Integer; data: PBoolean); CDecl;
  Procedure alGetIntegerv(param: Integer; data: PInteger); CDecl;
  Procedure alGetFloatv(param: Integer; data: PSingle); CDecl;
  Procedure alGetDoublev(param: Integer; data: PDouble); CDecl;
  Function alGetString(param: Integer): PAnsiChar; CDecl;

  //ERROR support.

  //Obtain the most recent error generated in the AL state machine.
  Function alGetError:Integer; CDecl;

  //EXTENSION support.

  // Verify is extension is avaliable
  Function alIsExtensionPresent(fname: PAnsiChar): Boolean; CDecl;

  { Obtain the address of a Function (usually an extension)
    with the name fname. All addresses are context-independent.
  }
  Function alGetProcAddress(fname: PAnsiChar): Pointer; CDecl;

  //Obtain the integer value of an enumeration (usually an extension) with the name ename.
  //Function alGetEnumValue(ename: PAnsiChar): Integer; CDecl;

  //LISTENER
  { Listener is the sample position for a given context.
    The multi-channel (usually stereo) output stream generated
    by the mixer is parametrized by this Listener object:
    its position and velocity relative to Sources, within
    occluder and reflector geometry.
  }
  //Listener Environment:  default 0.
  Procedure alListeneri(param: Integer; value: Integer); CDecl;

  //Listener Gain:  default 1.0f.
  Procedure alListenerf(param: Integer; value: Single); CDecl;

  //Listener Position.
  //Listener Velocity.
  Procedure alListener3f(param: Integer; f1: Single; f2: Single; f3: Single); CDecl;

  //Listener Position:        array [0..2] of TSingle
  //Listener Velocity:        array [0..2] of TSingle
  //Listener Orientation:     array [0..5] of TSingle  forward and up vector.
  Procedure alListenerfv(param:Integer; values: PSingle); CDecl;

  //Retrieve listener information
  Procedure alGetListeneriv(param:Integer; values: PInteger); CDecl;
  Procedure alGetListenerfv(param:Integer; values: PSingle); CDecl;


  //SOURCE
  { Source objects are by default localized. Sources
    take the PCM data provided in the specified Buffer,
    apply Source-specific modifications, and then
    submit them to be mixed according to spatial
    arrangement etc.
  }
  //Create Source objects.
  Procedure alGenSources(n:Integer; sources: PCardinal); CDecl;

  //Delete Source objects.
  Procedure alDeleteSources(n:Integer; sources: PCardinal); CDecl;

  //Verify a handle is a valid Source.
  Function alIsSource(id:Integer):Boolean; CDecl;

  //Set an integer parameter for a Source object.
  Procedure alSourcei(source: Cardinal; param:Integer; value: Integer); CDecl;
  //Set a 3 integer parameter for a Source object.
  Procedure alSource3i(source: Cardinal; param:Integer; v1, v2, v3: Integer); CDecl;
  //Set a float parameter for a Source object.
  Procedure alSourcef(source: Cardinal; param:Integer; value: Single); CDecl;
  //Set a 3 float parameter for a Source object.
  Procedure alSource3f(source: Cardinal; param:Integer; v1: Single; v2: Single; v3: Single); CDecl;
  //Set a float vector parameter for a Source object.
  Procedure alSourcefv(source: Cardinal; param:Integer; values: PSingle); CDecl;

  //Get an integer scalar parameter for a Source object.
  Procedure alGetSourcei(source:Cardinal; param:Integer; value: PInteger); CDecl;
  //Get a float scalar parameter for a Source object.
  Procedure alGetSourcef(source:Cardinal; param:Integer; value: PSingle); CDecl;
  //Get three float scalar parameter for a Source object.
  Procedure alGetSource3f(source:Cardinal; param:Integer; v1: PSingle; v2: PSingle; v3: PSingle); CDecl;
  //Get a float vector parameter for a Source object.
  Procedure alGetSourcefv(source:Cardinal; param:Integer; values: PSingle); CDecl;

  //Activate a source, start replay.
  Procedure alSourcePlay(source:Cardinal); CDecl;

  //Pause a source,
  //temporarily remove it from the mixer list.
  Procedure alSourcePause(source:Cardinal); CDecl;

  //Stop a source,
  { temporarily remove it from the mixer list,
    and reset its internal state to pre-Play.
    To remove a Source completely, it has to be
    deleted following Stop, or before Play.
  }
  Procedure alSourceStop(source:Cardinal); CDecl;


  //Rewind a souce.
  { Stopped paused and playing sources,
    resets the offset into the PCM data and sets state to AL_INITIAL.
  }
  Procedure alSourceRewind(source:Cardinal); CDecl;

  //vector forms of those Functions we all love
  Procedure alSourcePlayv(n:Cardinal; sources: PCardinal); CDecl;
  Procedure alSourceStopv(n:Cardinal; sources: PCardinal); CDecl;
  Procedure alSourceRewindv(n:Cardinal; sources: PCardinal); CDecl;
  Procedure alSourcePausev(n:Cardinal; sources: PCardinal); CDecl;

  //BUFFER
  { Buffer objects are storage space for sample data.
    Buffers are referred to by Sources. There can be more than
    one Source using the same Buffer data. If Buffers have
    to be duplicated on a per-Source basis, the driver has to
    take care of allocation, copying, and deallocation as well
    as propagating buffer data changes.
  }

  //Buffer object generation.
  Procedure alGenBuffers(n:Cardinal; buffers: PCardinal); CDecl;
  Procedure alDeleteBuffers(n:Cardinal; buffers: PCardinal); CDecl;
  Function alIsBuffer(buffer:Cardinal):Boolean; CDecl;
  //Specify the data to be filled into a buffer.
  Procedure alBufferData(buffer:Cardinal; format:Integer; data: Pointer; size, freq:Cardinal); CDecl;
  //read parameter for an buffer object
  Procedure alGetBufferi(buffer:Cardinal; param:Integer; value: PInteger); CDecl;
  Procedure alGetBufferf(buffer:Cardinal; param:Integer; value: PSingle); CDecl;

  //Queue stuff
  Procedure alSourceQueueBuffers(source:Cardinal; n:Cardinal; buffers: PCardinal); CDecl;
  Procedure alSourceUnqueueBuffers(source:Cardinal; n:Cardinal; buffers: PCardinal); CDecl;

  //Knobs and dials
  Procedure alDistanceModel(value:Integer); CDecl;
  Procedure alDopplerFactor(value:Single); CDecl;
  Procedure alDopplerVelocity(value:Single); CDecl;

  //alc
  Function alcCreateContext(device: PALCdevice; attrlist: PInteger): PALCcontext; CDecl;

  //There is no current context, as we can mix
  //several active contexts. But al* calls
  //only affect the current context.
  Function alcMakeContextCurrent(context: PALCcontext): Integer; CDecl;

  //Perform processing on a synced context, non-op on a asynchronous
  //context.
  Procedure alcProcessContext(context: PALCcontext); CDecl;

  //Suspend processing on an asynchronous context, non-op on a
  //synced context.
  Procedure alcSuspendContext(context: PALCcontext); CDecl;

  Procedure alcDestroyContext(context: PALCcontext); CDecl;

  Function alcGetError(device: PALCdevice):Integer; CDecl;

  Function alcGetCurrentContext: PALCcontext; CDecl;

  Function alcOpenDevice(deviceName: PAnsiChar): PALCdevice; CDecl;
  Procedure alcCloseDevice(device: PALCdevice); CDecl;

  Function alcIsExtensionPresent(device: PALCdevice; extName: PAnsiChar): Boolean; CDecl;
  Function alcGetProcAddress(device: PALCdevice; funcName: PAnsiChar):Pointer; CDecl;
  Function alcGetEnumValue(device: PALCdevice; enumName: PAnsiChar):Integer; CDecl;

  Function alcGetContextsDevice(context: PALCcontext): PALCdevice; CDecl;

  //Query Functions
  Function alcGetString(device: PALCdevice; param: Integer): PAnsiChar; CDecl;
  Procedure alcGetIntegerv(device: PALCdevice; param:Integer; size: Integer; data: PInteger); CDecl;

  // EFX
  Procedure alGenEffects(n:Integer; effects:PCardinal); CDecl;
  Procedure alDeleteEffects (N:Integer; effects:PCardinal); CDecl;
  Function alIsEffect(effect:Cardinal):Boolean; CDecl;
  Procedure alEffecti(effect:Cardinal; param:Cardinal; iValue:Integer); CDecl;
  Procedure alEffectiv(effect:Cardinal; param:Cardinal; piValues:Integer); CDecl;
  Procedure alEffectf(effect:Cardinal; param:Cardinal; flValue:Single); CDecl;
  Procedure alEffectfv(effect:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
  Procedure alGetEffecti(effect:Cardinal; param:Cardinal; piValue:PInteger); CDecl;
  Procedure alGetEffectiv(effect:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
  Procedure alGetEffectf(effect:Cardinal; param:Cardinal; pflValue:PSingle); CDecl;
  Procedure alGetEffectfv(effect:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;

  Procedure alGenAuxiliaryEffectSlots(n:Integer; effectslots:PCardinal); CDecl;
  Procedure alDeleteAuxiliaryEffectSlots(N:Integer; effectslots:PCardinal); CDecl;
  Procedure alIsAuxiliaryEffectSlot(effectslot:Cardinal); CDecl;
  Procedure alAuxiliaryEffectSloti(effectslot:Cardinal; param:Cardinal; iValue:Integer); CDecl;
  Procedure alAuxiliaryEffectSlotiv(effectslot:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
  Procedure alAuxiliaryEffectSlotf(effectslot:Cardinal; param:Cardinal; flValue:Single); CDecl;
  Procedure alAuxiliaryEffectSlotfv(effectslot:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;
  Procedure alGetAuxiliaryEffectSloti(effectslot:Cardinal; param:Cardinal; piValue:PInteger); CDecl;
  Procedure alGetAuxiliaryEffectSlotiv(effectslot:Cardinal; param:Cardinal; piValues:PInteger); CDecl;
  Procedure alGetAuxiliaryEffectSlotf(effectslot:Cardinal; param:Cardinal; pflValue:PSingle); CDecl;
  Procedure alGetAuxiliaryEffectSlotfv(effectslot:Cardinal; param:Cardinal; pflValues:PSingle); CDecl;

Var
  alEffectsAvaliable:Boolean = False;

Procedure LoadOpenAL;
Procedure InitOpenAL;
Procedure FreeOpenAL;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Application;

Var
  EffectList:Array Of EffectEntry;
  DisabledEffects:Array[0..Pred(MAX_EFFECTS)] Of Boolean;

Function isfinite(S:Single):Boolean;
Begin
  Result := Not IsInfinite(S);
End;

Procedure InitOpenAL;
Begin
End;

Procedure LoadOpenAL;
Begin
End;

Procedure FreeOpenAL;
Begin
End;

Var
  _CurrentContext:PALCcontext;

Procedure alSetError(errorCode:Integer);
Begin
  If Assigned(_CurrentContext) Then
    _CurrentContext.LastError := ErrorCode;

  Log(logError, 'AL', 'Error: '+alGetString(ErrorCode));
End;

Procedure alEnable(capability: Integer); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Capability) Of
  AL_SOURCE_DISTANCE_MODEL:
    Begin
      _CurrentContext.SourceDistanceModel := True;
      _CurrentContext.UpdateSources := True;
    End;

  Else
    Begin
      alSetError(AL_INVALID_ENUM);
    End;
  End;

  //ALCcontext_DecRef(Context);
End;

Procedure alDisable(capability:Integer);
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Capability) Of
  AL_SOURCE_DISTANCE_MODEL:
    Begin
      _CurrentContext.SourceDistanceModel := False;
      _CurrentContext.UpdateSources := True;
    End;

  Else
    Begin
      alSetError(AL_INVALID_ENUM);
    End;
  End;
End;

Function alIsEnabled(capability:Integer):Boolean;
Begin
  Result := False;
  If (_CurrentContext=Nil) Then
    Exit;

  Case (capability) Of
  AL_SOURCE_DISTANCE_MODEL:
    Begin
      Result := _CurrentContext.SourceDistanceModel;
    End;

  Else
    Begin
      alSetError(AL_INVALID_ENUM);
    End;
  End;
End;

Function alGetBoolean(param: Integer): Boolean; CDecl;
Begin
  Result := False;
  If (_CurrentContext=Nil) Then
    Exit;

  Case (param) Of
    AL_DOPPLER_FACTOR:
      Begin
        If (_CurrentContext.DopplerFactor <> 0.0) Then
          Result := True;
      End;

    AL_DOPPLER_VELOCITY:
      Begin
        If (_CurrentContext.DopplerVelocity <> 0.0) Then
          Result := True;
      End;


    AL_DISTANCE_MODEL:
      Begin
        If(_CurrentContext.DistanceModel = AL_INVERSE_DISTANCE_CLAMPED)Then
          Result := True;
      End;

    AL_SPEED_OF_SOUND:
      Begin
        If (_CurrentContext.flSpeedOfSound <> 0.0) Then
          Result := True;
      End;

    AL_DEFERRED_UPDATES_SOFT:
      Begin
        Result := _CurrentContext.DeferUpdates;
      End;

    Else
      Begin
        alSetError(AL_INVALID_ENUM);
      End;
  End;
End;

Function alGetDouble(param: Integer): Double; CDecl;
Begin
  Result := 0.0;
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Param) Of
    AL_DOPPLER_FACTOR:
      Result := _CurrentContext.DopplerFactor;

    AL_DOPPLER_VELOCITY:
      Result := _CurrentContext.DopplerVelocity;

    AL_DISTANCE_MODEL:
      Result := _CurrentContext.DistanceModel;

    AL_SPEED_OF_SOUND:
      Result := _CurrentContext.flSpeedOfSound;

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Function alGetFloat(param: Integer): Single; CDecl;
Begin
  Result := 0.0;
  If (_CurrentContext=Nil) Then
    Exit;

  Case (param) Of
    AL_DOPPLER_FACTOR:
      Result := _CurrentContext
.DopplerFactor;

    AL_DOPPLER_VELOCITY:
      Result := _CurrentContext
.DopplerVelocity;

    AL_DISTANCE_MODEL:
      Result := _CurrentContext
.DistanceModel;

    AL_SPEED_OF_SOUND:
      Result := _CurrentContext
.flSpeedOfSound;

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Function alGetInteger(param: Integer): Integer; CDecl;
Begin
  Result := 0;
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Param) Of
    AL_DOPPLER_FACTOR:
      Result := Trunc(_CurrentContext.DopplerFactor);

    AL_DOPPLER_VELOCITY:
      Result := Trunc(_CurrentContext.DopplerVelocity);

    AL_DISTANCE_MODEL:
      Result := _CurrentContext.DistanceModel;

    AL_SPEED_OF_SOUND:
      Result := Trunc(_CurrentContext.flSpeedOfSound);

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alGetBooleanv(param: Integer; data: PBoolean); CDecl;
Begin
  If (Data = Nil) Then
  Begin
    alSetError(AL_INVALID_VALUE);
    Exit;
  End;

  Case (param) Of
    AL_DOPPLER_FACTOR,
    AL_DOPPLER_VELOCITY,
    AL_DISTANCE_MODEL,
    AL_SPEED_OF_SOUND,
    AL_DEFERRED_UPDATES_SOFT:
      Begin
        PBoolean(data)^ := alGetBoolean(Param);
        Exit;
      End;

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alGetDoublev(param: Integer; data: PDouble); CDecl;
Begin
  If (Data = Nil) Then
  Begin
    alSetError(AL_INVALID_VALUE);
    Exit;
  End;

  Case (Param) Of
    AL_DOPPLER_FACTOR,
    AL_DOPPLER_VELOCITY,
    AL_DISTANCE_MODEL,
    AL_SPEED_OF_SOUND,
    AL_DEFERRED_UPDATES_SOFT:
      Begin
        PDouble(data)^ := alGetDouble(Param);
        Exit;
      End;

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alGetFloatv(param: Integer; data: PSingle); CDecl;
Begin
  If (Data = Nil) Then
  Begin
    alSetError(AL_INVALID_VALUE);
    Exit;
  End;


  Case Param Of
    AL_DOPPLER_FACTOR,
    AL_DOPPLER_VELOCITY,
    AL_DISTANCE_MODEL,
    AL_SPEED_OF_SOUND,
    AL_DEFERRED_UPDATES_SOFT:
      Begin
        PSingle(data)^ := alGetFloat(Param);
        Exit;
      End;
    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alGetIntegerv(param: Integer; data: PInteger); CDecl;
Begin
  If (Data = Nil) Then
  Begin
    alSetError(AL_INVALID_VALUE);
    Exit;
  End;

  Case Param Of
    AL_DOPPLER_FACTOR,
    AL_DOPPLER_VELOCITY,
    AL_DISTANCE_MODEL,
    AL_SPEED_OF_SOUND,
    AL_DEFERRED_UPDATES_SOFT:
      Begin
        PInteger(data)^ := alGetInteger(Param);
        Exit;
      End;
    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Function alGetString(param: Integer): PAnsiChar; CDecl;
Begin
  Result := '';
  If (_CurrentContext=Nil) Then
    Exit;

  Case Param Of
    AL_VENDOR:
      Result := 'TERRA.AL';

    AL_VERSION:
      Result := '0.1';

    AL_GraphicsManager:
      Result := 'TERRA.AL';

    AL_EXTENSIONS:
      Result := PAnsiChar(_CurrentContext.ExtensionList);

    AL_NO_ERROR:
      Result := 'No error';

    AL_INVALID_NAME:
      Result := 'Invalid Name';

    AL_INVALID_ENUM:
      Result := 'Invalid Enum';

    AL_INVALID_VALUE:
      Result := 'Invalid Value';

    AL_INVALID_OPERATION:
      Result := 'Invalid Operation';

    AL_OUT_OF_MEMORY:
      Result := 'Out of Memory';

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alDopplerFactor(value:Single); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  If (value >= 0.0) And (Not IsInfinite(Value)) Then
  Begin
    _CurrentContext.DopplerFactor := value;
    _CurrentContext.UpdateSources := True;
  End Else
    alSetError(AL_INVALID_VALUE);
End;

Procedure alDopplerVelocity(value:Single); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  If (value > 0.0) And (Not IsInfinite(value)) Then
  Begin
    _CurrentContext.DopplerVelocity := Value;
    _CurrentContext.UpdateSources := True;
  End Else
    alSetError(AL_INVALID_VALUE);
End;

Procedure alSpeedOfSound(flSpeedOfSound:Single); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  If (flSpeedOfSound > 0.0) And (Not IsInfinite(flSpeedOfSound)) Then
  Begin
    _CurrentContext.flSpeedOfSound := flSpeedOfSound;
    _CurrentContext.UpdateSources := True;
  End Else
    alSetError(AL_INVALID_VALUE);
End;

Procedure alDistanceModel(value:Integer); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (value) Of
    AL_NONE,
    AL_INVERSE_DISTANCE,
    AL_INVERSE_DISTANCE_CLAMPED,
    AL_LINEAR_DISTANCE,
    AL_LINEAR_DISTANCE_CLAMPED,
    AL_EXPONENT_DISTANCE,
    AL_EXPONENT_DISTANCE_CLAMPED:
      Begin
        _CurrentContext.DistanceModel := Value;
        _CurrentContext.UpdateSources := True;
      End;

    Else
      alSetError(AL_INVALID_VALUE);
  End;
End;

Function alGetError:Integer; CDecl;
Begin
  If (_CurrentContext = Nil) Then
  Begin
    Result := AL_INVALID_OPERATION;
  End Else
  Begin
    Result := _CurrentContext.LastError;
    _CurrentContext.LastError := AL_NO_ERROR;
  End;
End;

Function alIsExtensionPresent(fname: PAnsiChar): Boolean; CDecl;
Begin
  Result := False;

  If (_CurrentContext = Nil) Then
    Exit;

  Result := Pos(fName, _CurrentContext.ExtensionList)>0;
End;

Function alGetProcAddress(fname: PAnsiChar): Pointer; CDecl;
Begin
  Result := Nil;
End;

{Function alGetEnumValue(ename: PAnsiChar): Integer; CDecl;
Var
  I:Integer;
Begin
  Result := 0;

  For I:=0 To Pred(Length(EffectList)) Do
  If (EffectList[I].ename = ename) Then
  Begin
    If (DisabledEffects[EffectList[i].effecttype]) Then
    Exit;
  End;

  i := 0;
  While (enumeration[i].enumName) And (enumeration[i].enumName <> enumName) Do
    Inc(I);

  Result := enumeration[i].value;
End;        }


Procedure alListeneri(param: Integer; value: Integer); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  alSetError( AL_INVALID_ENUM);
End;

Procedure alListenerf(param: Integer; value: Single); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (param) Of
    AL_GAIN:
      Begin
        If (value >= 0.0) And (Not IsInfinite(value)) Then
        Begin
          _CurrentContext.Listener.Gain := Value;
          _CurrentContext.UpdateSources := True;
        End Else
          alSetError( AL_INVALID_VALUE);
      End;

    AL_METERS_PER_UNIT:
      Begin
        If (Value > 0.0) And (Not IsInfinite(Value)) Then
        Begin
          _CurrentContext.Listener.MetersPerUnit := Value;
          _CurrentContext.UpdateSources := True;
        End Else
          alSetError(AL_INVALID_VALUE);
      End;
    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alListener3f(param: Integer; f1: Single; f2: Single; f3: Single); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Param) Of
    AL_POSITION:
      If (isfinite(f1)) And (isfinite(f2)) And (isfinite(f3)) Then
      Begin
        _CurrentContext.Listener.Position := VectorCreate(f1, f2, f3);
        _CurrentContext.UpdateSources := True;
      End Else
        alSetError( AL_INVALID_VALUE);

    AL_VELOCITY:
      If (isfinite(f1)) And (isfinite(f2)) And (isfinite(f3)) Then
      Begin
        _CurrentContext.Listener.Velocity := VectorCreate(f1, f2, f3);
        _CurrentContext.UpdateSources := True;
      End Else
        alSetError( AL_INVALID_VALUE);

    Else
      alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alListenerfv(param:Integer; values: PSingle); CDecl;
Var
  X,Y,Z:Single;
  U,V,N:Vector3D;
  M:Matrix;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  If (values = Nil ) Then
  Begin
    alSetError(AL_INVALID_VALUE);
    Exit;
  End;

  Case (Param) Of
    AL_GAIN,
    AL_METERS_PER_UNIT:
      Begin
        alListenerf(Param, values^);
        Exit;
      End;

    AL_POSITION,
    AL_VELOCITY:
      Begin
        X := Values^; Inc(Values);
        Y := Values^; Inc(Values);
        Z := Values^; Inc(Values);
        alListener3f(Param, X, Y, Z);
        Exit;
      End;

    AL_ORIENTATION:
      Begin
        // AT then UP
        N.X := values^; Inc(Values);
        N.Y := values^; Inc(Values);
        N.Z := values^; Inc(Values);
        _CurrentContext.Listener.Forward := N;
        N.Normalize();

        V.X := values^; Inc(Values);
        V.Y := values^; Inc(Values);
        V.Z := values^; Inc(Values);
        _CurrentContext.Listener.Up := N;
        V.Normalize();

        // Build and normalize right-vector
        U := VectorCross(N, V);
        U.Normalize();

        M.SetValue(0, 0, U.X);
        M.SetValue(0, 1, V.X);
        M.SetValue(0, 2, -N.X);
        M.SetValue(0, 3, 0.0);
        M.SetValue(1, 0, U.Y);
        M.SetValue(1, 1, V.Y);
        M.SetValue(1, 2, -N.Y);
        M.SetValue(1, 3, 0.0);
        M.SetValue(2, 0, U.Z);
        M.SetValue(2, 1, V.Z);
        M.SetValue(2, 2, -N.Z);
        M.SetValue(2, 3, 0.0);
        M.SetValue(3, 0, 0.0);
        M.SetValue(3, 1, 0.0);
        M.SetValue(3, 2, 0.0);
        M.SetValue(3, 3, 1.0);
        _CurrentContext.Listener.Matrix := M;
        _CurrentContext.UpdateSources := True;
      End;
  Else
    alSetError(AL_INVALID_ENUM);
  End;
End;

Procedure alGetListeneriv(param:Integer; values: PInteger); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  alSetError(AL_INVALID_ENUM);
End;

Procedure alGetListenerfv(param:Integer; values: PSingle); CDecl;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  Case (Param) Of
    AL_POSITION:
        PVector3D(Values)^ := _CurrentContext.Listener.Position;

    AL_VELOCITY:
        PVector3D(Values)^ := _CurrentContext.Listener.Velocity;

    AL_ORIENTATION:
      Begin
        // AT then UP
        PSingleArray(Values)[0] := _CurrentContext.Listener.Forward.X;
        PSingleArray(Values)[1] := _CurrentContext.Listener.Forward.Y;
        PSingleArray(Values)[2] := _CurrentContext.Listener.Forward.Z;
        PSingleArray(Values)[3] := _CurrentContext.Listener.Up.X;
        PSingleArray(Values)[4] := _CurrentContext.Listener.Up.Y;
        PSingleArray(Values)[5] := _CurrentContext.Listener.Up.Z;
      End;

  Else
    alSetError(AL_INVALID_ENUM);
  End;
End;

Constructor ALSource.Create();
Begin
  flInnerAngle := 360.0;
  flOuterAngle := 360.0;
  flPitch := 1.0;
  vPosition := VectorZero;
  vOrientation := VectorZero;
  vVelocity := VectorZero;
  flRefDistance := 1.0;
  flMaxDistance := 99999.0;
  flRollOffFactor := 1.0;
  bLooping := False;
  flGain := 1.0;
  flMinGain := 0.0;
  flMaxGain := 1.0;
  flOuterGain := 0.0;
  OuterGainHF := 1.0;

  DryGainHFAuto := True;
  WetGainAuto := True;
  WetGainHFAuto := True;
  AirAbsorptionFactor := 0.0;
  RoomRolloffFactor := 0.0;
  DopplerFactor := 1.0;
  DirectChannels := AL_FALSE;

  DistanceModel := DefaultDistanceModel;

  Resampler := DefaultResampler;

  state := AL_INITIAL;
  new_state := AL_NONE;
  lSourceType := AL_UNDETERMINED;
  lOffset := -1;

  DirectGain := 1.0;
  DirectGainHF := 1.0;
  for(i := 0;i < MAX_SENDS;i++)
    {
        Send[i].WetGain := 1.0;
        Send[i].WetGainHF := 1.0;
    }

    NeedsUpdate := AL_TRUE;

    HrtfMoving := AL_FALSE;
    HrtfCounter := 0;
End;

Procedure alGenSources(n:Integer; sources: PCardinal); CDecl;
Var
  err, i:Integer;
  Source:ALSource;
Begin
  If (_CurrentContext=Nil) Then
    Exit;

  If (n < 0) Then
  Begin
    alSetError( AL_INVALID_VALUE);
    Exit;
  End;

  // Add additional sources to the list
  i := 0;
  While (i < n) Do
  Begin
    source := ALSource.Create();
    InitSourceParams(source);

            err = NewThunkEntry(&source->source);
            if(err == AL_NO_ERROR)
                err = InsertUIntMapEntry(&Context->SourceMap, source->source, source);
            if(err != AL_NO_ERROR)
            {
                FreeThunkEntry(source->source);
                memset(source, 0, sizeof(ALsource));
                free(source);

                alSetError(Context, err);
                alDeleteSources(i, sources);
                break;
            }

            sources[i++] = source->source;
        }
    }

End.
