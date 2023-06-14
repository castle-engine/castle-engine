{%MainUnit castleinternalopenal.pas}

{ translation of AL/altypes.h }

type

  {* OpenAL 8bit bool type. }
  TALboolean = ByteBool;
  PALboolean = ^TALboolean;

  {* OpenAL 8bit signed byte. }
  TALbyte = CChar;
  PALbyte = ^TALbyte;

  {* OpenAL 8bit unsigned byte. }
  TALubyte = CUChar;
  PALubyte = ^TALubyte;

  {* OpenAL 16bit signed short integer type. }
  TALshort = CShort;
  PALshort = ^TALshort;

  {* OpenAL 16bit unsigned short integer type. }
  TALushort = CUShort;
  PALushort = ^TALushort;

  {* OpenAL 32bit unsigned integer type. }
  TALuint = CUInt;
  PALuint = ^TALuint;

  {* OpenAL 32bit signed integer type. }
  TALint = CInt;
  PALint = ^TALint;

  {* OpenAL 32bit floating point type. }
  TALfloat = CFloat;
  PALfloat = ^TALfloat;

  {* OpenAL 64bit double point type. }
  TALdouble = CDouble;
  PALdouble = ^TALdouble;

  {* OpenAL 32bit type. }
  TALsizei = CInt;
  PALsizei = ^TALsizei;

  {* OpenAL void type (for params, not returns). }
  { TALvoid = void;
  PALvoid = ^TALvoid; } { }
  PALvoid = Pointer;

  {* OpenAL enumerations. }
  TALenum = CInt;
  PALenum = ^TALenum;

  {* OpenAL bitfields. }
  TALbitfield = CUInt;
  PALbitfield = ^TALbitfield;

  {* OpenAL clamped float. }
  TALclampf = TALfloat;
  PALclampf = ^TALclampf;

  {* Openal clamped double. }
  TALclampd = TALdouble;
  PALclampd = ^TALclampd;

const

{ Enumerant values begin at column 50. No tabs. }

  { bad value }
  AL_INVALID = -1;

  AL_NONE = 0;

  { Boolean False. }
  AL_FALSE = 0;

  {* Boolean True. }
  AL_TRUE = 1;

  {*
   * Indicate the type of AL_SOURCE.
   * Sources can be spatialized
   }
  // TODO: constant conflict between OpenAL 1.1 and sample implementation (1.0),
  // sample implementation is below.
  //AL_SOURCE_TYPE = $0200;

  {* Indicate Source has relative coordinates. }
  AL_SOURCE_RELATIVE = $0202;

  {*
   * Directional source, inner cone angle, in degrees.
   * Range:    [0-360]
   * Default:  360
   }
  AL_CONE_INNER_ANGLE = $1001;

  {*
   * Directional source, outer cone angle, in degrees.
   * Range:    [0-360]
   * Default:  360
   }
  AL_CONE_OUTER_ANGLE = $1002;

  {*
   * Specify the pitch to be applied, either at source,
   *  or on mixer results, at listener.
   * Range:   [0.5-2.0]
   * Default: 1.0
   }
  AL_PITCH = $1003;

  {*
   * Specify the current location in three dimensional space.
   * OpenAL, like OpenGL, uses a right handed coordinate system,
   *  where in a frontal default view X (thumb) points right,
   *  Y points up (index finger), and Z points towards the
   *  viewer/camera (middle finger).
   * To switch from a left handed coordinate system, flip the
   *  sign on the Z coordinate.
   * Listener position is always in the world coordinate system.
   }
  AL_POSITION = $1004;

  {* Specify the current direction. }
  AL_DIRECTION = $1005;

  {* Specify the current velocity in three dimensional space. }
  AL_VELOCITY = $1006;

  {*
   * Indicate whether source is looping.
   * Type: ALboolean?
   * Range:   [AL_TRUE, AL_FALSE]
   * Default: FALSE.
   }
  AL_LOOPING = $1007;

  {*
   * Indicate whether source is meant to be streaming.
   * Type: ALboolean?
   * Range:   [AL_TRUE, AL_FALSE]
   * Default: FALSE.
   }
  // TODO: constant conflict between OpenAL 1.1 and sample implementation (1.0),
  // sample implementation is below.
  //AL_STREAMING = $1008;

  {*
   * Indicate the buffer to provide sound samples.
   * Type: ALuint.
   * Range: any valid Buffer id.
   }
  AL_BUFFER = $1009;

  {*
   * Indicate the gain (volume amplification) applied.
   * Type:   ALfloat.
   * Range:  ]0.0-  ]
   * A value of 1.0 means un-attenuated/unchanged.
   * Each division by 2 equals an attenuation of -6dB.
   * Each multiplicaton with 2 equals an amplification of +6dB.
   * A value of 0.0 is meaningless with respect to a logarithmic
   *  scale; it is interpreted as zero volume - the channel
   *  is effectively disabled.
   }
  AL_GAIN = $100A;

  { byte offset into source (in canon format).  -1 if source
   * is not playing.  Don't set this, get this.
   *
   * Type:  ALint
   * Range: -1 - +inf
   }
  AL_BYTE_LOKI = $100C;

  {
   * Indicate minimum source attenuation
   * Type: ALfloat
   * Range:  [0.0 - 1.0]
   *
   * Logarthmic
   }
  AL_MIN_GAIN = $100D;

  {*
   * Indicate maximum source attenuation
   * Type: ALfloat
   * Range:  [0.0 - 1.0]
   *
   * Logarthmic
   }
  AL_MAX_GAIN = $100E;

  {*
   * Indicate listener orientation.
   *
   * at/up
   }
  AL_ORIENTATION = $100F;

  {*
   * Source state information.
   }
  AL_SOURCE_STATE = $1010;
  AL_INITIAL = $1011;
  AL_PLAYING = $1012;
  AL_PAUSED = $1013;
  AL_STOPPED = $1014;

  {*
   * Buffer Queue params
   }
  AL_BUFFERS_QUEUED = $1015;
  AL_BUFFERS_PROCESSED = $1016;

  {*
   * Buffer states
   }
  AL_PENDING = $1017;
  AL_PROCESSED = $1018;


  {* Sound samples: format specifier. }
  AL_FORMAT_MONO8 = $1100;
  AL_FORMAT_MONO16 = $1101;
  AL_FORMAT_STEREO8 = $1102;
  AL_FORMAT_STEREO16 = $1103;

  {*
   * source specific reference distance
   * Type: ALfloat
   * Range:  0.0 - +inf
   *
   * At 0.0, no distance attenuation occurs.  Default is
   * 1.0.
   }
  AL_REFERENCE_DISTANCE = $1020;

  {*
   * source specific rolloff factor
   * Type: ALfloat
   * Range:  0.0 - +inf
   *
   }
  AL_ROLLOFF_FACTOR = $1021;

  {*
   * Directional source, outer cone gain.
   *
   * Default:  0.0
   * Range:    [0.0 - 1.0]
   * Logarithmic
   }
  AL_CONE_OUTER_GAIN = $1022;

  {*
   * Indicate distance above which sources are not
   * attenuated using the inverse clamped distance model.
   *
   * Default: +inf
   * Type: ALfloat
   * Range:  0.0 - +inf
   }
  AL_MAX_DISTANCE = $1023;

  {** Source buffer position, in seconds.

    Probably only since OpenAL 1.1. *}
  AL_SEC_OFFSET = $1024;

  {** Source buffer position, in sample frames.

    Probably only since OpenAL 1.1. *}
  AL_SAMPLE_OFFSET = $1025;

  {** Source buffer position, in bytes.

    Probably only since OpenAL 1.1. *}
  AL_BYTE_OFFSET = $1026;

  {**
   * Source type (query only).
   * Type:  ALint
   * Range: [AL_STATIC, AL_STREAMING, AL_UNDETERMINED]
   *
   * A Source is Static if a Buffer has been attached using AL_BUFFER.
   *
   * A Source is Streaming if one or more Buffers have been attached using
   * alSourceQueueBuffers.
   *
   * A Source is Undetermined when it has the NULL buffer attached using
   * AL_BUFFER.
   *}
  // TODO: constant conflict between OpenAL 1.1 and sample implementation,
  // 1.1 is below?
  //AL_SOURCE_TYPE = $1027;

  {** Source type value. *}
  AL_STATIC       = $1028;
  // TODO: constant conflict between OpenAL 1.1 and sample implementation,
  // 1.1 is below?
  //AL_STREAMING    = $1029;
  AL_UNDETERMINED = $1030;

  {*
   * Sound samples: frequency, in units of Hertz [Hz].
   * This is the number of samples per second. Half of the
   *  sample frequency marks the maximum significant
   *  frequency component.
   }
  AL_FREQUENCY = $2001;
  AL_BITS = $2002;
  AL_CHANNELS = $2003;
  AL_SIZE = $2004;

  {*
   * Buffer state.
   *
   * Not supported for public use (yet).
   }
  AL_UNUSED = $2010;
  AL_QUEUED = $2011;
  AL_CURRENT = $2012;

  {* Errors: No Error. }
  AL_NO_ERROR = AL_FALSE;

  {*
   * Invalid Name paramater passed to AL call.
   }
  AL_INVALID_NAME = $A001;

  {*
   * Invalid parameter passed to AL call.
   }
  AL_ILLEGAL_ENUM = $A002;

  {*
   * Invalid enum parameter value.
   }
  AL_INVALID_VALUE = $A003;

  {*
   * Illegal call.
   }
  AL_ILLEGAL_COMMAND = $A004;

  {*
   * No mojo.
   }
  AL_OUT_OF_MEMORY = $A005;


  {* Context strings: Vendor Name. }
  AL_VENDOR = $B001;
  AL_VERSION = $B002;
  AL_RENDERER = $B003;
  AL_EXTENSIONS = $B004;

  {* Global tweakage. }

  {*
   * Doppler scale.  Default 1.0
   }
  AL_DOPPLER_FACTOR = $C000;

  {*
   * Tweaks speed of propagation.
   }
  AL_DOPPLER_VELOCITY = $C001;

  {*
   * Distance scaling
   }
  AL_DISTANCE_SCALE = $C002;

  {*
   * Distance models
   *
   * used in conjunction with DistanceModel
   *
   * implicit: NONE, which disances distance attenuation.
   }
  AL_DISTANCE_MODEL            = $D000;
  AL_INVERSE_DISTANCE          = $D001;
  AL_INVERSE_DISTANCE_CLAMPED  = $D002;
  AL_LINEAR_DISTANCE           = $D003; {< since OpenAL 1.1 }
  AL_LINEAR_DISTANCE_CLAMPED   = $D004; {< since OpenAL 1.1 }
  AL_EXPONENT_DISTANCE         = $D005; {< since OpenAL 1.1 }
  AL_EXPONENT_DISTANCE_CLAMPED = $D006; {< since OpenAL 1.1 }

  {*
   * enables
   }

  { #define AL_SOME_ENABLE                            0xE000 }

  {* IASIG Level 2 Environment. }

  {*
   * Parameter:  IASIG ROOM  blah
   * Type:       intgeger
   * Range:      [-10000, 0]
   * Default:    -10000
   }
  AL_ENV_ROOM_IASIG = $3001;

  {*
   * Parameter:  IASIG ROOM_HIGH_FREQUENCY
   * Type:       integer
   * Range:      [-10000, 0]
   * Default:    0
   }
  AL_ENV_ROOM_HIGH_FREQUENCY_IASIG = $3002;

  {*
   * Parameter:  IASIG ROOM_ROLLOFF_FACTOR
   * Type:       float
   * Range:      [0.0, 10.0]
   * Default:    0.0
   }
  AL_ENV_ROOM_ROLLOFF_FACTOR_IASIG = $3003;

  {*
   * Parameter:  IASIG  DECAY_TIME
   * Type:       float
   * Range:      [0.1, 20.0]
   * Default:    1.0
   }
  AL_ENV_DECAY_TIME_IASIG = $3004;

  {*
   * Parameter:  IASIG DECAY_HIGH_FREQUENCY_RATIO
   * Type:       float
   * Range:      [0.1, 2.0]
   * Default:    0.5
   }
  AL_ENV_DECAY_HIGH_FREQUENCY_RATIO_IASIG = $3005;

  {*
   * Parameter:  IASIG REFLECTIONS
   * Type:       integer
   * Range:      [-10000, 1000]
   * Default:    -10000
   }
  AL_ENV_REFLECTIONS_IASIG = $3006;

  {*
   * Parameter:  IASIG REFLECTIONS_DELAY
   * Type:       float
   * Range:      [0.0, 0.3]
   * Default:    0.02
   }
  AL_ENV_REFLECTIONS_DELAY_IASIG = $3006;

  {*
   * Parameter:  IASIG REVERB
   * Type:       integer
   * Range:      [-10000,2000]
   * Default:    -10000
   }
  AL_ENV_REVERB_IASIG = $3007;

  {*
   * Parameter:  IASIG REVERB_DELAY
   * Type:       float
   * Range:      [0.0, 0.1]
   * Default:    0.04
   }
  AL_ENV_REVERB_DELAY_IASIG = $3008;

  {*
   * Parameter:  IASIG DIFFUSION
   * Type:       float
   * Range:      [0.0, 100.0]
   * Default:    100.0
   }
  AL_ENV_DIFFUSION_IASIG = $3009;

  {*
   * Parameter:  IASIG DENSITY
   * Type:       float
   * Range:      [0.0, 100.0]
   * Default:    100.0
   }
  AL_ENV_DENSITY_IASIG = $300A;

  {*
   * Parameter:  IASIG HIGH_FREQUENCY_REFERENCE
   * Type:       float
   * Range:      [20.0, 20000.0]
   * Default:    5000.0
   }
  AL_ENV_HIGH_FREQUENCY_REFERENCE_IASIG = $300B;

  { MP3 format.
    See http://icculus.org/alextreg/index.php?operation=op_showext&extname=AL_EXT_mp3 }
  AL_FORMAT_MP3_EXT = $10020;

  { Ogg Vorbis format.
    See http://icculus.org/alextreg/index.php?operation=op_showext&extname=AL_EXT_vorbis }
  AL_FORMAT_VORBIS_EXT = $10003;
