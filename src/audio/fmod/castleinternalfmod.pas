{ FMOD API expressed in Pascal, for Castle Game Engine.
  See https://github.com/castle-engine/castle-engine/wiki/FMOD
  about using FMOD with CGE.

  Based on FMOD 2.00.01, fmod.h and fmod_common.h header files.

  Translation to Pascal copyright by Michalis Kamburelis 2019-2019.

  Copyright of the C headers:

  ========================================================================================
  FMOD Core API - C header file.
  Copyright (c), Firelight Technologies Pty, Ltd. 2004-2019.

  Use this header in conjunction with fmod_common.h (which contains all the constants /
  callbacks) to develop using the C interface

  For more detail visit:
  https://fmod.com/resources/documentation-api?version=2.0&page=core-api.html
  ========================================================================================
}
unit CastleInternalFMOD;

interface

{ FMOD enums have xxx_FORCEINT to force them explicitly to be 4 bytes. }
{$PACKENUM 4}

{$PACKRECORDS C}

{$if defined(MSWINDOWS)}
  {$define extdecl_callback := stdcall}
{$else}
  {$define extdecl_callback := cdecl}
{$endif}

{$I castleconf.inc}

uses CTypes;

type
  TFMOD_BOOL = CInt;
  PFMOD_BOOL = ^TFMOD_BOOL;

  TFMOD_PORT_TYPE = CUInt;
  TFMOD_PORT_INDEX = CULongLong;

  { Opaque types in FMOD (only pointers should be passed around) }
  TFMOD_SYSTEM_FmodInternalType = record end;
  TFMOD_SOUND_FmodInternalType = record end;
  TFMOD_CHANNELCONTROL_FmodInternalType = record end;
  TFMOD_CHANNEL_FmodInternalType = record end;
  TFMOD_CHANNELGROUP_FmodInternalType = record end;
  TFMOD_SOUNDGROUP_FmodInternalType = record end;
  TFMOD_REVERB3D_FmodInternalType = record end;
  TFMOD_DSP_FmodInternalType = record end;
  TFMOD_DSPCONNECTION_FmodInternalType = record end;
  TFMOD_POLYGON_FmodInternalType = record end;
  TFMOD_GEOMETRY_FmodInternalType = record end;
  TFMOD_SYNCPOINT_FmodInternalType = record end;
  TFMOD_ASYNCREADINFO_FmodInternalType = record end;

  PFMOD_SYSTEM = ^TFMOD_SYSTEM_FmodInternalType;
  PFMOD_SOUND = ^TFMOD_SOUND_FmodInternalType;
  PFMOD_CHANNELCONTROL = ^TFMOD_CHANNELCONTROL_FmodInternalType;
  PFMOD_CHANNEL = ^TFMOD_CHANNEL_FmodInternalType;
  PFMOD_CHANNELGROUP = ^TFMOD_CHANNELGROUP_FmodInternalType;
  PFMOD_SOUNDGROUP = ^TFMOD_SOUNDGROUP_FmodInternalType;
  PFMOD_REVERB3D = ^TFMOD_REVERB3D_FmodInternalType;
  PFMOD_DSP = ^TFMOD_DSP_FmodInternalType;
  PFMOD_DSPCONNECTION = ^TFMOD_DSPCONNECTION_FmodInternalType;
  PFMOD_POLYGON = ^TFMOD_POLYGON_FmodInternalType;
  PFMOD_GEOMETRY = ^TFMOD_GEOMETRY_FmodInternalType;
  PFMOD_SYNCPOINT = ^TFMOD_SYNCPOINT_FmodInternalType;
  PFMOD_ASYNCREADINFO = ^TFMOD_ASYNCREADINFO_FmodInternalType;

  PPFMOD_SYSTEM = ^PFMOD_SYSTEM;
  PPFMOD_SOUND = ^PFMOD_SOUND;
  PPFMOD_CHANNELCONTROL = ^PFMOD_CHANNELCONTROL;
  PPFMOD_CHANNEL = ^PFMOD_CHANNEL;
  PPFMOD_CHANNELGROUP = ^PFMOD_CHANNELGROUP;
  PPFMOD_SOUNDGROUP = ^PFMOD_SOUNDGROUP;
  PPFMOD_REVERB3D = ^PFMOD_REVERB3D;
  PPFMOD_DSP = ^PFMOD_DSP;
  PPFMOD_DSPCONNECTION = ^PFMOD_DSPCONNECTION;
  PPFMOD_POLYGON = ^PFMOD_POLYGON;
  PPFMOD_GEOMETRY = ^PFMOD_GEOMETRY;
  PPFMOD_SYNCPOINT = ^PFMOD_SYNCPOINT;
  PPFMOD_ASYNCREADINFO = ^PFMOD_ASYNCREADINFO;

const
  FMOD_VERSION = $00020001; {< $aaaabbcc -> aaaa = product version, bb = major version, cc = minor version. }

type
  TFMOD_DEBUG_FLAGS = CUInt;
const
  FMOD_DEBUG_LEVEL_NONE                        = $00000000;
  FMOD_DEBUG_LEVEL_ERROR                       = $00000001;
  FMOD_DEBUG_LEVEL_WARNING                     = $00000002;
  FMOD_DEBUG_LEVEL_LOG                         = $00000004;
  FMOD_DEBUG_TYPE_MEMORY                       = $00000100;
  FMOD_DEBUG_TYPE_FILE                         = $00000200;
  FMOD_DEBUG_TYPE_CODEC                        = $00000400;
  FMOD_DEBUG_TYPE_TRACE                        = $00000800;
  FMOD_DEBUG_DISPLAY_TIMESTAMPS                = $00010000;
  FMOD_DEBUG_DISPLAY_LINENUMBERS               = $00020000;
  FMOD_DEBUG_DISPLAY_THREAD                    = $00040000;

type
  TFMOD_MEMORY_TYPE = CUInt;
const
  FMOD_MEMORY_NORMAL                           = $00000000;
  FMOD_MEMORY_STREAM_FILE                      = $00000001;
  FMOD_MEMORY_STREAM_DECODE                    = $00000002;
  FMOD_MEMORY_SAMPLEDATA                       = $00000004;
  FMOD_MEMORY_DSP_BUFFER                       = $00000008;
  FMOD_MEMORY_PLUGIN                           = $00000010;
  FMOD_MEMORY_PERSISTENT                       = $00200000;
  FMOD_MEMORY_ALL                              = $FFFFFFFF;

type
  TFMOD_INITFLAGS = CUInt;
const
  FMOD_INIT_NORMAL                             = $00000000;
  FMOD_INIT_STREAM_FROM_UPDATE                 = $00000001;
  FMOD_INIT_MIX_FROM_UPDATE                    = $00000002;
  FMOD_INIT_3D_RIGHTHANDED                     = $00000004;
  FMOD_INIT_CHANNEL_LOWPASS                    = $00000100;
  FMOD_INIT_CHANNEL_DISTANCEFILTER             = $00000200;
  FMOD_INIT_PROFILE_ENABLE                     = $00010000;
  FMOD_INIT_VOL0_BECOMES_VIRTUAL               = $00020000;
  FMOD_INIT_GEOMETRY_USECLOSEST                = $00040000;
  FMOD_INIT_PREFER_DOLBY_DOWNMIX               = $00080000;
  FMOD_INIT_THREAD_UNSAFE                      = $00100000;
  FMOD_INIT_PROFILE_METER_ALL                  = $00200000;

type
  TFMOD_DRIVER_STATE = CUInt;
  PFMOD_DRIVER_STATE = ^TFMOD_DRIVER_STATE;
const
  FMOD_DRIVER_STATE_CONNECTED                  = $00000001;
  FMOD_DRIVER_STATE_DEFAULT                    = $00000002;

type
  TFMOD_TIMEUNIT = CUInt;
  PFMOD_TIMEUNIT = ^TFMOD_TIMEUNIT;
const
  FMOD_TIMEUNIT_MS                             = $00000001;
  FMOD_TIMEUNIT_PCM                            = $00000002;
  FMOD_TIMEUNIT_PCMBYTES                       = $00000004;
  FMOD_TIMEUNIT_RAWBYTES                       = $00000008;
  FMOD_TIMEUNIT_PCMFRACTION                    = $00000010;
  FMOD_TIMEUNIT_MODORDER                       = $00000100;
  FMOD_TIMEUNIT_MODROW                         = $00000200;
  FMOD_TIMEUNIT_MODPATTERN                     = $00000400;

type
  TFMOD_SYSTEM_CALLBACK_TYPE = CUInt;
const
  FMOD_SYSTEM_CALLBACK_DEVICELISTCHANGED       = $00000001;
  FMOD_SYSTEM_CALLBACK_DEVICELOST              = $00000002;
  FMOD_SYSTEM_CALLBACK_MEMORYALLOCATIONFAILED  = $00000004;
  FMOD_SYSTEM_CALLBACK_THREADCREATED           = $00000008;
  FMOD_SYSTEM_CALLBACK_BADDSPCONNECTION        = $00000010;
  FMOD_SYSTEM_CALLBACK_PREMIX                  = $00000020;
  FMOD_SYSTEM_CALLBACK_POSTMIX                 = $00000040;
  FMOD_SYSTEM_CALLBACK_ERROR                   = $00000080;
  FMOD_SYSTEM_CALLBACK_MIDMIX                  = $00000100;
  FMOD_SYSTEM_CALLBACK_THREADDESTROYED         = $00000200;
  FMOD_SYSTEM_CALLBACK_PREUPDATE               = $00000400;
  FMOD_SYSTEM_CALLBACK_POSTUPDATE              = $00000800;
  FMOD_SYSTEM_CALLBACK_RECORDLISTCHANGED       = $00001000;
  FMOD_SYSTEM_CALLBACK_ALL                     = $FFFFFFFF;

type
  TFMOD_MODE = CUInt;
  PFMOD_MODE = ^TFMOD_MODE;
const
  FMOD_DEFAULT                                 = $00000000;
  FMOD_LOOP_OFF                                = $00000001;
  FMOD_LOOP_NORMAL                             = $00000002;
  FMOD_LOOP_BIDI                               = $00000004;
  FMOD_2D                                      = $00000008;
  FMOD_3D                                      = $00000010;
  FMOD_CREATESTREAM                            = $00000080;
  FMOD_CREATESAMPLE                            = $00000100;
  FMOD_CREATECOMPRESSEDSAMPLE                  = $00000200;
  FMOD_OPENUSER                                = $00000400;
  FMOD_OPENMEMORY                              = $00000800;
  FMOD_OPENMEMORY_POINT                        = $10000000;
  FMOD_OPENRAW                                 = $00001000;
  FMOD_OPENONLY                                = $00002000;
  FMOD_ACCURATETIME                            = $00004000;
  FMOD_MPEGSEARCH                              = $00008000;
  FMOD_NONBLOCKING                             = $00010000;
  FMOD_UNIQUE                                  = $00020000;
  FMOD_3D_HEADRELATIVE                         = $00040000;
  FMOD_3D_WORLDRELATIVE                        = $00080000;
  FMOD_3D_INVERSEROLLOFF                       = $00100000;
  FMOD_3D_LINEARROLLOFF                        = $00200000;
  FMOD_3D_LINEARSQUAREROLLOFF                  = $00400000;
  FMOD_3D_INVERSETAPEREDROLLOFF                = $00800000;
  FMOD_3D_CUSTOMROLLOFF                        = $04000000;
  FMOD_3D_IGNOREGEOMETRY                       = $40000000;
  FMOD_IGNORETAGS                              = $02000000;
  FMOD_LOWMEM                                  = $08000000;
  FMOD_VIRTUAL_PLAYFROMSTART                   = $80000000;

type
  TFMOD_CHANNELMASK = CUInt;
  PFMOD_CHANNELMASK = ^TFMOD_CHANNELMASK;
const
  FMOD_CHANNELMASK_FRONT_LEFT                  = $00000001;
  FMOD_CHANNELMASK_FRONT_RIGHT                 = $00000002;
  FMOD_CHANNELMASK_FRONT_CENTER                = $00000004;
  FMOD_CHANNELMASK_LOW_FREQUENCY               = $00000008;
  FMOD_CHANNELMASK_SURROUND_LEFT               = $00000010;
  FMOD_CHANNELMASK_SURROUND_RIGHT              = $00000020;
  FMOD_CHANNELMASK_BACK_LEFT                   = $00000040;
  FMOD_CHANNELMASK_BACK_RIGHT                  = $00000080;
  FMOD_CHANNELMASK_BACK_CENTER                 = $00000100;

const
  FMOD_CHANNELMASK_MONO                       = (FMOD_CHANNELMASK_FRONT_LEFT);
  FMOD_CHANNELMASK_STEREO                     = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT);
  FMOD_CHANNELMASK_LRC                        = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER);
  FMOD_CHANNELMASK_QUAD                       = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_SURROUND_LEFT or FMOD_CHANNELMASK_SURROUND_RIGHT);
  FMOD_CHANNELMASK_SURROUND                   = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER  or FMOD_CHANNELMASK_SURROUND_LEFT or FMOD_CHANNELMASK_SURROUND_RIGHT);
  FMOD_CHANNELMASK_5POINT1                    = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER  or FMOD_CHANNELMASK_LOW_FREQUENCY or FMOD_CHANNELMASK_SURROUND_LEFT  or FMOD_CHANNELMASK_SURROUND_RIGHT);
  FMOD_CHANNELMASK_5POINT1_REARS              = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER  or FMOD_CHANNELMASK_LOW_FREQUENCY or FMOD_CHANNELMASK_BACK_LEFT      or FMOD_CHANNELMASK_BACK_RIGHT);
  FMOD_CHANNELMASK_7POINT0                    = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER  or FMOD_CHANNELMASK_SURROUND_LEFT or FMOD_CHANNELMASK_SURROUND_RIGHT or FMOD_CHANNELMASK_BACK_LEFT      or FMOD_CHANNELMASK_BACK_RIGHT);
  FMOD_CHANNELMASK_7POINT1                    = (FMOD_CHANNELMASK_FRONT_LEFT or FMOD_CHANNELMASK_FRONT_RIGHT or FMOD_CHANNELMASK_FRONT_CENTER  or FMOD_CHANNELMASK_LOW_FREQUENCY or FMOD_CHANNELMASK_SURROUND_LEFT  or FMOD_CHANNELMASK_SURROUND_RIGHT or FMOD_CHANNELMASK_BACK_LEFT or FMOD_CHANNELMASK_BACK_RIGHT);

{ Preset for FMOD_REVERB_PROPERTIES }
(* TODO: translate to Pascal
const
  #define FMOD_PRESET_OFF                             {  1000,    7,  11, 5000, 100, 100, 100, 250, 0,    20,  96, -80.0f }
  #define FMOD_PRESET_GENERIC                         {  1500,    7,  11, 5000,  83, 100, 100, 250, 0, 14500,  96,  -8.0f }
  #define FMOD_PRESET_PADDEDCELL                      {   170,    1,   2, 5000,  10, 100, 100, 250, 0,   160,  84,  -7.8f }
  #define FMOD_PRESET_ROOM                            {   400,    2,   3, 5000,  83, 100, 100, 250, 0,  6050,  88,  -9.4f }
  #define FMOD_PRESET_BATHROOM                        {  1500,    7,  11, 5000,  54, 100,  60, 250, 0,  2900,  83,   0.5f }
  #define FMOD_PRESET_LIVINGROOM                      {   500,    3,   4, 5000,  10, 100, 100, 250, 0,   160,  58, -19.0f }
  #define FMOD_PRESET_STONEROOM                       {  2300,   12,  17, 5000,  64, 100, 100, 250, 0,  7800,  71,  -8.5f }
  #define FMOD_PRESET_AUDITORIUM                      {  4300,   20,  30, 5000,  59, 100, 100, 250, 0,  5850,  64, -11.7f }
  #define FMOD_PRESET_CONCERTHALL                     {  3900,   20,  29, 5000,  70, 100, 100, 250, 0,  5650,  80,  -9.8f }
  #define FMOD_PRESET_CAVE                            {  2900,   15,  22, 5000, 100, 100, 100, 250, 0, 20000,  59, -11.3f }
  #define FMOD_PRESET_ARENA                           {  7200,   20,  30, 5000,  33, 100, 100, 250, 0,  4500,  80,  -9.6f }
  #define FMOD_PRESET_HANGAR                          { 10000,   20,  30, 5000,  23, 100, 100, 250, 0,  3400,  72,  -7.4f }
  #define FMOD_PRESET_CARPETTEDHALLWAY                {   300,    2,  30, 5000,  10, 100, 100, 250, 0,   500,  56, -24.0f }
  #define FMOD_PRESET_HALLWAY                         {  1500,    7,  11, 5000,  59, 100, 100, 250, 0,  7800,  87,  -5.5f }
  #define FMOD_PRESET_STONECORRIDOR                   {   270,   13,  20, 5000,  79, 100, 100, 250, 0,  9000,  86,  -6.0f }
  #define FMOD_PRESET_ALLEY                           {  1500,    7,  11, 5000,  86, 100, 100, 250, 0,  8300,  80,  -9.8f }
  #define FMOD_PRESET_FOREST                          {  1500,  162,  88, 5000,  54,  79, 100, 250, 0,   760,  94, -12.3f }
  #define FMOD_PRESET_CITY                            {  1500,    7,  11, 5000,  67,  50, 100, 250, 0,  4050,  66, -26.0f }
  #define FMOD_PRESET_MOUNTAINS                       {  1500,  300, 100, 5000,  21,  27, 100, 250, 0,  1220,  82, -24.0f }
  #define FMOD_PRESET_QUARRY                          {  1500,   61,  25, 5000,  83, 100, 100, 250, 0,  3400, 100,  -5.0f }
  #define FMOD_PRESET_PLAIN                           {  1500,  179, 100, 5000,  50,  21, 100, 250, 0,  1670,  65, -28.0f }
  #define FMOD_PRESET_PARKINGLOT                      {  1700,    8,  12, 5000, 100, 100, 100, 250, 0, 20000,  56, -19.5f }
  #define FMOD_PRESET_SEWERPIPE                       {  2800,   14,  21, 5000,  14,  80,  60, 250, 0,  3400,  66,   1.2f }
  #define FMOD_PRESET_UNDERWATER                      {  1500,    7,  11, 5000,  10, 100, 100, 250, 0,   500,  92,   7.0f }
*)

const
  FMOD_MAX_CHANNEL_WIDTH                      = 32;
  FMOD_MAX_SYSTEMS                            = 8;
  FMOD_MAX_LISTENERS                          = 8;
  FMOD_REVERB_MAXINSTANCES                    = 4;
  FMOD_PORT_INDEX_NONE                        = $FFFFFFFFFFFFFFFF;

type
  TFMOD_RESULT = (
    FMOD_OK,
    FMOD_ERR_BADCOMMAND,
    FMOD_ERR_CHANNEL_ALLOC,
    FMOD_ERR_CHANNEL_STOLEN,
    FMOD_ERR_DMA,
    FMOD_ERR_DSP_CONNECTION,
    FMOD_ERR_DSP_DONTPROCESS,
    FMOD_ERR_DSP_FORMAT,
    FMOD_ERR_DSP_INUSE,
    FMOD_ERR_DSP_NOTFOUND,
    FMOD_ERR_DSP_RESERVED,
    FMOD_ERR_DSP_SILENCE,
    FMOD_ERR_DSP_TYPE,
    FMOD_ERR_FILE_BAD,
    FMOD_ERR_FILE_COULDNOTSEEK,
    FMOD_ERR_FILE_DISKEJECTED,
    FMOD_ERR_FILE_EOF,
    FMOD_ERR_FILE_ENDOFDATA,
    FMOD_ERR_FILE_NOTFOUND,
    FMOD_ERR_FORMAT,
    FMOD_ERR_HEADER_MISMATCH,
    FMOD_ERR_HTTP,
    FMOD_ERR_HTTP_ACCESS,
    FMOD_ERR_HTTP_PROXY_AUTH,
    FMOD_ERR_HTTP_SERVER_ERROR,
    FMOD_ERR_HTTP_TIMEOUT,
    FMOD_ERR_INITIALIZATION,
    FMOD_ERR_INITIALIZED,
    FMOD_ERR_INTERNAL,
    FMOD_ERR_INVALID_FLOAT,
    FMOD_ERR_INVALID_HANDLE,
    FMOD_ERR_INVALID_PARAM,
    FMOD_ERR_INVALID_POSITION,
    FMOD_ERR_INVALID_SPEAKER,
    FMOD_ERR_INVALID_SYNCPOINT,
    FMOD_ERR_INVALID_THREAD,
    FMOD_ERR_INVALID_VECTOR,
    FMOD_ERR_MAXAUDIBLE,
    FMOD_ERR_MEMORY,
    FMOD_ERR_MEMORY_CANTPOINT,
    FMOD_ERR_NEEDS3D,
    FMOD_ERR_NEEDSHARDWARE,
    FMOD_ERR_NET_CONNECT,
    FMOD_ERR_NET_SOCKET_ERROR,
    FMOD_ERR_NET_URL,
    FMOD_ERR_NET_WOULD_BLOCK,
    FMOD_ERR_NOTREADY,
    FMOD_ERR_OUTPUT_ALLOCATED,
    FMOD_ERR_OUTPUT_CREATEBUFFER,
    FMOD_ERR_OUTPUT_DRIVERCALL,
    FMOD_ERR_OUTPUT_FORMAT,
    FMOD_ERR_OUTPUT_INIT,
    FMOD_ERR_OUTPUT_NODRIVERS,
    FMOD_ERR_PLUGIN,
    FMOD_ERR_PLUGIN_MISSING,
    FMOD_ERR_PLUGIN_RESOURCE,
    FMOD_ERR_PLUGIN_VERSION,
    FMOD_ERR_RECORD,
    FMOD_ERR_REVERB_CHANNELGROUP,
    FMOD_ERR_REVERB_INSTANCE,
    FMOD_ERR_SUBSOUNDS,
    FMOD_ERR_SUBSOUND_ALLOCATED,
    FMOD_ERR_SUBSOUND_CANTMOVE,
    FMOD_ERR_TAGNOTFOUND,
    FMOD_ERR_TOOMANYCHANNELS,
    FMOD_ERR_TRUNCATED,
    FMOD_ERR_UNIMPLEMENTED,
    FMOD_ERR_UNINITIALIZED,
    FMOD_ERR_UNSUPPORTED,
    FMOD_ERR_VERSION,
    FMOD_ERR_EVENT_ALREADY_LOADED,
    FMOD_ERR_EVENT_LIVEUPDATE_BUSY,
    FMOD_ERR_EVENT_LIVEUPDATE_MISMATCH,
    FMOD_ERR_EVENT_LIVEUPDATE_TIMEOUT,
    FMOD_ERR_EVENT_NOTFOUND,
    FMOD_ERR_STUDIO_UNINITIALIZED,
    FMOD_ERR_STUDIO_NOT_LOADED,
    FMOD_ERR_INVALID_STRING,
    FMOD_ERR_ALREADY_LOCKED,
    FMOD_ERR_NOT_LOCKED,
    FMOD_ERR_RECORD_DISCONNECTED,
    FMOD_ERR_TOOMANYSAMPLES,

    FMOD_RESULT_FORCEINT := 65536
  );

  TFMOD_CHANNELCONTROL_TYPE = (
    FMOD_CHANNELCONTROL_CHANNEL,
    FMOD_CHANNELCONTROL_CHANNELGROUP,

    FMOD_CHANNELCONTROL_MAX,
    FMOD_CHANNELCONTROL_FORCEINT := 65536
  );

  TFMOD_OUTPUTTYPE = (
    FMOD_OUTPUTTYPE_AUTODETECT,
    FMOD_OUTPUTTYPE_UNKNOWN,
    FMOD_OUTPUTTYPE_NOSOUND,
    FMOD_OUTPUTTYPE_WAVWRITER,
    FMOD_OUTPUTTYPE_NOSOUND_NRT,
    FMOD_OUTPUTTYPE_WAVWRITER_NRT,
    FMOD_OUTPUTTYPE_WASAPI,
    FMOD_OUTPUTTYPE_ASIO,
    FMOD_OUTPUTTYPE_PULSEAUDIO,
    FMOD_OUTPUTTYPE_ALSA,
    FMOD_OUTPUTTYPE_COREAUDIO,
    FMOD_OUTPUTTYPE_AUDIOTRACK,
    FMOD_OUTPUTTYPE_OPENSL,
    FMOD_OUTPUTTYPE_AUDIOOUT,
    FMOD_OUTPUTTYPE_AUDIO3D,
    FMOD_OUTPUTTYPE_WEBAUDIO,
    FMOD_OUTPUTTYPE_NNAUDIO,
    FMOD_OUTPUTTYPE_WINSONIC,

    FMOD_OUTPUTTYPE_MAX,
    FMOD_OUTPUTTYPE_FORCEINT := 65536
  );
  PFMOD_OUTPUTTYPE = ^TFMOD_OUTPUTTYPE;

  TFMOD_DEBUG_MODE = (
    FMOD_DEBUG_MODE_TTY,
    FMOD_DEBUG_MODE_FILE,
    FMOD_DEBUG_MODE_CALLBACK,

    FMOD_DEBUG_MODE_FORCEINT := 65536
  );

  TFMOD_SPEAKERMODE = (
    FMOD_SPEAKERMODE_DEFAULT,
    FMOD_SPEAKERMODE_RAW,
    FMOD_SPEAKERMODE_MONO,
    FMOD_SPEAKERMODE_STEREO,
    FMOD_SPEAKERMODE_QUAD,
    FMOD_SPEAKERMODE_SURROUND,
    FMOD_SPEAKERMODE_5POINT1,
    FMOD_SPEAKERMODE_7POINT1,
    FMOD_SPEAKERMODE_7POINT1POINT4,

    FMOD_SPEAKERMODE_MAX,
    FMOD_SPEAKERMODE_FORCEINT := 65536
  );
  PFMOD_SPEAKERMODE = ^TFMOD_SPEAKERMODE;

  TFMOD_SPEAKER = (
    FMOD_SPEAKER_FRONT_LEFT,
    FMOD_SPEAKER_FRONT_RIGHT,
    FMOD_SPEAKER_FRONT_CENTER,
    FMOD_SPEAKER_LOW_FREQUENCY,
    FMOD_SPEAKER_SURROUND_LEFT,
    FMOD_SPEAKER_SURROUND_RIGHT,
    FMOD_SPEAKER_BACK_LEFT,
    FMOD_SPEAKER_BACK_RIGHT,
    FMOD_SPEAKER_TOP_FRONT_LEFT,
    FMOD_SPEAKER_TOP_FRONT_RIGHT,
    FMOD_SPEAKER_TOP_BACK_LEFT,
    FMOD_SPEAKER_TOP_BACK_RIGHT,

    FMOD_SPEAKER_MAX,
    FMOD_SPEAKER_FORCEINT := 65536
  );
  PFMOD_SPEAKER = ^TFMOD_SPEAKER;

  TFMOD_CHANNELORDER = (
    FMOD_CHANNELORDER_DEFAULT,
    FMOD_CHANNELORDER_WAVEFORMAT,
    FMOD_CHANNELORDER_PROTOOLS,
    FMOD_CHANNELORDER_ALLMONO,
    FMOD_CHANNELORDER_ALLSTEREO,
    FMOD_CHANNELORDER_ALSA,

    FMOD_CHANNELORDER_MAX,
    FMOD_CHANNELORDER_FORCEINT := 65536
  );

  TFMOD_PLUGINTYPE = (
    FMOD_PLUGINTYPE_OUTPUT,
    FMOD_PLUGINTYPE_CODEC,
    FMOD_PLUGINTYPE_DSP,

    FMOD_PLUGINTYPE_MAX,
    FMOD_PLUGINTYPE_FORCEINT := 65536
  );
  PFMOD_PLUGINTYPE = ^TFMOD_PLUGINTYPE;

  TFMOD_SOUND_TYPE = (
    FMOD_SOUND_TYPE_UNKNOWN,
    FMOD_SOUND_TYPE_AIFF,
    FMOD_SOUND_TYPE_ASF,
    FMOD_SOUND_TYPE_DLS,
    FMOD_SOUND_TYPE_FLAC,
    FMOD_SOUND_TYPE_FSB,
    FMOD_SOUND_TYPE_IT,
    FMOD_SOUND_TYPE_MIDI,
    FMOD_SOUND_TYPE_MOD,
    FMOD_SOUND_TYPE_MPEG,
    FMOD_SOUND_TYPE_OGGVORBIS,
    FMOD_SOUND_TYPE_PLAYLIST,
    FMOD_SOUND_TYPE_RAW,
    FMOD_SOUND_TYPE_S3M,
    FMOD_SOUND_TYPE_USER,
    FMOD_SOUND_TYPE_WAV,
    FMOD_SOUND_TYPE_XM,
    FMOD_SOUND_TYPE_XMA,
    FMOD_SOUND_TYPE_AUDIOQUEUE,
    FMOD_SOUND_TYPE_AT9,
    FMOD_SOUND_TYPE_VORBIS,
    FMOD_SOUND_TYPE_MEDIA_FOUNDATION,
    FMOD_SOUND_TYPE_MEDIACODEC,
    FMOD_SOUND_TYPE_FADPCM,

    FMOD_SOUND_TYPE_MAX,
    FMOD_SOUND_TYPE_FORCEINT := 65536
  );
  PFMOD_SOUND_TYPE = ^TFMOD_SOUND_TYPE;

  TFMOD_SOUND_FORMAT = (
    FMOD_SOUND_FORMAT_NONE,
    FMOD_SOUND_FORMAT_PCM8,
    FMOD_SOUND_FORMAT_PCM16,
    FMOD_SOUND_FORMAT_PCM24,
    FMOD_SOUND_FORMAT_PCM32,
    FMOD_SOUND_FORMAT_PCMFLOAT,
    FMOD_SOUND_FORMAT_BITSTREAM,

    FMOD_SOUND_FORMAT_MAX,
    FMOD_SOUND_FORMAT_FORCEINT := 65536
  );
  PFMOD_SOUND_FORMAT = ^TFMOD_SOUND_FORMAT;

  TFMOD_OPENSTATE = (
    FMOD_OPENSTATE_READY,
    FMOD_OPENSTATE_LOADING,
    FMOD_OPENSTATE_ERROR,
    FMOD_OPENSTATE_CONNECTING,
    FMOD_OPENSTATE_BUFFERING,
    FMOD_OPENSTATE_SEEKING,
    FMOD_OPENSTATE_PLAYING,
    FMOD_OPENSTATE_SETPOSITION,

    FMOD_OPENSTATE_MAX,
    FMOD_OPENSTATE_FORCEINT := 65536
  );
  PFMOD_OPENSTATE = ^TFMOD_OPENSTATE;

  TFMOD_SOUNDGROUP_BEHAVIOR = (
    FMOD_SOUNDGROUP_BEHAVIOR_FAIL,
    FMOD_SOUNDGROUP_BEHAVIOR_MUTE,
    FMOD_SOUNDGROUP_BEHAVIOR_STEALLOWEST,

    FMOD_SOUNDGROUP_BEHAVIOR_MAX,
    FMOD_SOUNDGROUP_BEHAVIOR_FORCEINT := 65536
  );
  PFMOD_SOUNDGROUP_BEHAVIOR = ^TFMOD_SOUNDGROUP_BEHAVIOR;

  TFMOD_CHANNELCONTROL_CALLBACK_TYPE = (
    FMOD_CHANNELCONTROL_CALLBACK_END,
    FMOD_CHANNELCONTROL_CALLBACK_VIRTUALVOICE,
    FMOD_CHANNELCONTROL_CALLBACK_SYNCPOINT,
    FMOD_CHANNELCONTROL_CALLBACK_OCCLUSION,

    FMOD_CHANNELCONTROL_CALLBACK_MAX,
    FMOD_CHANNELCONTROL_CALLBACK_FORCEINT := 65536
  );

  TFMOD_CHANNELCONTROL_DSP_INDEX = (
    FMOD_CHANNELCONTROL_DSP_TAIL     := -3,
    FMOD_CHANNELCONTROL_DSP_FADER    := -2,
    FMOD_CHANNELCONTROL_DSP_HEAD     := -1,

    FMOD_CHANNELCONTROL_DSP_FORCEINT := 65536
  );

  TFMOD_ERRORCALLBACK_INSTANCETYPE = (
    FMOD_ERRORCALLBACK_INSTANCETYPE_NONE,
    FMOD_ERRORCALLBACK_INSTANCETYPE_SYSTEM,
    FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNEL,
    FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNELGROUP,
    FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNELCONTROL,
    FMOD_ERRORCALLBACK_INSTANCETYPE_SOUND,
    FMOD_ERRORCALLBACK_INSTANCETYPE_SOUNDGROUP,
    FMOD_ERRORCALLBACK_INSTANCETYPE_DSP,
    FMOD_ERRORCALLBACK_INSTANCETYPE_DSPCONNECTION,
    FMOD_ERRORCALLBACK_INSTANCETYPE_GEOMETRY,
    FMOD_ERRORCALLBACK_INSTANCETYPE_REVERB3D,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_SYSTEM,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_EVENTDESCRIPTION,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_EVENTINSTANCE,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_PARAMETERINSTANCE,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_BUS,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_VCA,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_BANK,
    FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_COMMANDREPLAY,

    FMOD_ERRORCALLBACK_INSTANCETYPE_FORCEINT := 65536
  );

  TFMOD_DSP_RESAMPLER = (
    FMOD_DSP_RESAMPLER_DEFAULT,
    FMOD_DSP_RESAMPLER_NOINTERP,
    FMOD_DSP_RESAMPLER_LINEAR,
    FMOD_DSP_RESAMPLER_CUBIC,
    FMOD_DSP_RESAMPLER_SPLINE,

    FMOD_DSP_RESAMPLER_MAX,
    FMOD_DSP_RESAMPLER_FORCEINT := 65536
  );

  TFMOD_DSPCONNECTION_TYPE = (
    FMOD_DSPCONNECTION_TYPE_STANDARD,
    FMOD_DSPCONNECTION_TYPE_SIDECHAIN,
    FMOD_DSPCONNECTION_TYPE_SEND,
    FMOD_DSPCONNECTION_TYPE_SEND_SIDECHAIN,

    FMOD_DSPCONNECTION_TYPE_MAX,
    FMOD_DSPCONNECTION_TYPE_FORCEINT := 65536
  );

  TFMOD_TAGTYPE = (
    FMOD_TAGTYPE_UNKNOWN,
    FMOD_TAGTYPE_ID3V1,
    FMOD_TAGTYPE_ID3V2,
    FMOD_TAGTYPE_VORBISCOMMENT,
    FMOD_TAGTYPE_SHOUTCAST,
    FMOD_TAGTYPE_ICECAST,
    FMOD_TAGTYPE_ASF,
    FMOD_TAGTYPE_MIDI,
    FMOD_TAGTYPE_PLAYLIST,
    FMOD_TAGTYPE_FMOD,
    FMOD_TAGTYPE_USER,

    FMOD_TAGTYPE_MAX,
    FMOD_TAGTYPE_FORCEINT := 65536
  );

  TFMOD_TAGDATATYPE = (
    FMOD_TAGDATATYPE_BINARY,
    FMOD_TAGDATATYPE_INT,
    FMOD_TAGDATATYPE_FLOAT,
    FMOD_TAGDATATYPE_STRING,
    FMOD_TAGDATATYPE_STRING_UTF16,
    FMOD_TAGDATATYPE_STRING_UTF16BE,
    FMOD_TAGDATATYPE_STRING_UTF8,

    FMOD_TAGDATATYPE_MAX,
    FMOD_TAGDATATYPE_FORCEINT := 65536
  );

  { FMOD callbacks. }
  TFMOD_DEBUG_CALLBACK =            function (flags: TFMOD_DEBUG_FLAGS; file_: PChar; line: CInt; func: PChar; message: PChar): TFMOD_RESULT; extdecl_callback;
  TFMOD_SYSTEM_CALLBACK =           function (system: PFMOD_SYSTEM; type_: TFMOD_SYSTEM_CALLBACK_TYPE; commanddata1: Pointer; commanddata2: Pointer; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_CHANNELCONTROL_CALLBACK =   function (channelcontrol: PFMOD_CHANNELCONTROL; controltype: TFMOD_CHANNELCONTROL_TYPE; callbacktype: TFMOD_CHANNELCONTROL_CALLBACK_TYPE; commanddata1: Pointer; commanddata2: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_SOUND_NONBLOCK_CALLBACK =   function (sound: PFMOD_SOUND; result: TFMOD_RESULT): TFMOD_RESULT; extdecl_callback;
  TFMOD_SOUND_PCMREAD_CALLBACK =    function (sound: PFMOD_SOUND; data: Pointer; datalen: CUInt): TFMOD_RESULT; extdecl_callback;
  TFMOD_SOUND_PCMSETPOS_CALLBACK =  function (sound: PFMOD_SOUND; subsound: CInt; position: CUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_OPEN_CALLBACK =        function (name: PChar; filesize: PCUInt; handle: PPointer; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_CLOSE_CALLBACK =       function (handle: Pointer; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_READ_CALLBACK =        function (handle: Pointer; buffer: Pointer; sizebytes: CUInt; bytesread: PCUInt; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_SEEK_CALLBACK =        function (handle: Pointer; pos: CUInt; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_ASYNCREAD_CALLBACK =   function (info: PFMOD_ASYNCREADINFO; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_ASYNCCANCEL_CALLBACK = function (info: PFMOD_ASYNCREADINFO; userdata: Pointer): TFMOD_RESULT; extdecl_callback;
  TFMOD_FILE_ASYNCDONE_FUNC =       procedure (info: PFMOD_ASYNCREADINFO; result: TFMOD_RESULT); extdecl_callback;
  TFMOD_MEMORY_ALLOC_CALLBACK =     function (size: CUInt; type_: TFMOD_MEMORY_TYPE; sourcestr: PChar): Pointer; extdecl_callback;
  TFMOD_MEMORY_REALLOC_CALLBACK =   function (ptr: Pointer; size: CUInt; type_: TFMOD_MEMORY_TYPE; sourcestr: PChar): Pointer; extdecl_callback;
  TFMOD_MEMORY_FREE_CALLBACK =      procedure (ptr: Pointer; type_: TFMOD_MEMORY_TYPE; sourcestr: PChar); extdecl_callback;
  TFMOD_3D_ROLLOFF_CALLBACK =       function (channelcontrol: PFMOD_CHANNELCONTROL; distance: CFloat): CFloat; extdecl_callback;

  { FMOD structs. }

  TFMOD_ASYNCREADINFO = record
    handle: Pointer;
    offset: CUInt;
    sizebytes: CUInt;
    priority: CInt;
    userdata: Pointer;
    buffer: Pointer;
    bytesread: CUInt;
    done: TFMOD_FILE_ASYNCDONE_FUNC;
  end;

  TFMOD_VECTOR = record
    x: CFloat;
    y: CFloat;
    z: CFloat;
  end;
  PFMOD_VECTOR = ^TFMOD_VECTOR;
  PPFMOD_VECTOR = ^PFMOD_VECTOR;

  TFMOD_3D_ATTRIBUTES = record
    position: TFMOD_VECTOR;
    velocity: TFMOD_VECTOR;
    forward_: TFMOD_VECTOR;
    up: TFMOD_VECTOR;
  end;

  TFMOD_GUID = record
    Data1: CUInt;
    Data2: CUShort;
    Data3: CUShort;
    Data4: array [0..7] of AnsiChar;
  end;
  PFMOD_GUID = ^TFMOD_GUID;

  TFMOD_PLUGINLIST = record
    type_: TFMOD_PLUGINTYPE;
    description: Pointer;
  end;

  TFMOD_ADVANCEDSETTINGS = record
    cbSize: CInt;
    maxMPEGCodecs: CInt;
    maxADPCMCodecs: CInt;
    maxXMACodecs: CInt;
    maxVorbisCodecs: CInt;
    maxAT9Codecs: CInt;
    maxFADPCMCodecs: CInt;
    maxPCMCodecs: CInt;
    ASIONumChannels: CInt;
    ASIOChannelList: PPChar;
    ASIOSpeakerList: PFMOD_SPEAKER;
    vol0virtualvol: CFloat;
    defaultDecodeBufferSize: CUInt;
    profilePort: CUShort;
    geometryMaxFadeTime: CUInt;
    distanceFilterCenterFreq: CFloat;
    reverb3Dinstance: CInt;
    DSPBufferPoolSize: CInt;
    stackSizeStream: CUInt;
    stackSizeNonBlocking: CUInt;
    stackSizeMixer: CUInt;
    resamplerMethod: TFMOD_DSP_RESAMPLER;
    commandQueueSize: CUInt;
    randomSeed: CUInt;
  end;
  PFMOD_ADVANCEDSETTINGS = ^TFMOD_ADVANCEDSETTINGS;

  TFMOD_TAG = record
    type_: TFMOD_TAGTYPE;
    datatype: TFMOD_TAGDATATYPE;
    name: PChar;
    data: Pointer;
    datalen: CUInt;
    updated: TFMOD_BOOL;
  end;
  PFMOD_TAG = ^TFMOD_TAG;

  TFMOD_CREATESOUNDEXINFO = record
    cbsize: CInt;
    length: CUInt;
    fileoffset: CUInt;
    numchannels: CInt;
    defaultfrequency: CInt;
    format: TFMOD_SOUND_FORMAT;
    decodebuffersize: CUInt;
    initialsubsound: CInt;
    numsubsounds: CInt;
    inclusionlist: PCInt;
    inclusionlistnum: CInt;
    pcmreadcallback: TFMOD_SOUND_PCMREAD_CALLBACK;
    pcmsetposcallback: TFMOD_SOUND_PCMSETPOS_CALLBACK;
    nonblockcallback: TFMOD_SOUND_NONBLOCK_CALLBACK;
    dlsname: PChar;
    encryptionkey: PChar;
    maxpolyphony: CInt;
    userdata: Pointer;
    suggestedsoundtype: TFMOD_SOUND_TYPE;
    fileuseropen: TFMOD_FILE_OPEN_CALLBACK;
    fileuserclose: TFMOD_FILE_CLOSE_CALLBACK;
    fileuserread: TFMOD_FILE_READ_CALLBACK;
    fileuserseek: TFMOD_FILE_SEEK_CALLBACK;
    fileuserasyncread: TFMOD_FILE_ASYNCREAD_CALLBACK;
    fileuserasynccancel: TFMOD_FILE_ASYNCCANCEL_CALLBACK;
    fileuserdata: Pointer;
    filebuffersize: CInt;
    channelorder: TFMOD_CHANNELORDER;
    initialsoundgroup: PFMOD_SOUNDGROUP;
    initialseekposition: CUInt;
    initialseekpostype: TFMOD_TIMEUNIT;
    ignoresetfilesystem: CInt;
    audioqueuepolicy: CUInt;
    minmidigranularity: CUInt;
    nonblockthreadid: CInt;
    fsbguid: PFMOD_GUID;
  end;
  PFMOD_CREATESOUNDEXINFO = ^TFMOD_CREATESOUNDEXINFO;

  TFMOD_REVERB_PROPERTIES = record
    DecayTime: CFloat;
    EarlyDelay: CFloat;
    LateDelay: CFloat;
    HFReference: CFloat;
    HFDecayRatio: CFloat;
    Diffusion: CFloat;
    Density: CFloat;
    LowShelfFrequency: CFloat;
    LowShelfGain: CFloat;
    HighCut: CFloat;
    EarlyLateMix: CFloat;
    WetLevel: CFloat;
  end;
  PFMOD_REVERB_PROPERTIES = ^TFMOD_REVERB_PROPERTIES;

  TFMOD_ERRORCALLBACK_INFO = record
    result: TFMOD_RESULT;
    instancetype: TFMOD_ERRORCALLBACK_INSTANCETYPE;
    instance: Pointer;
    functionname: PChar;
    functionparams: PChar;
  end;

{ Load FMOD library and it's entry points.
  This is important in case of dynamic FMOD loading, in case of static
  loading this does nothing.
  You only need to call it explicitly on platforms that don't define
  ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION. }
procedure InitializeFmodLibrary;

{ Did we found dynamic FMOD library in the last InitializeFmodLibrary call.
  In case of static loading this is always @true, since the static library
  must be present at compile-time. }
function FmodLibraryAvailable: Boolean;

{ Surround calls to Fmod library in these routines.
  This protects from freeing the dynamic FMOD library too early.
  Makes everything working correctly even if this unit's "finalization"
  section is run even when FMOD backend is still used (because CastleSoundEngine
  "finalization" didn't finish yet). }
procedure FmodLibraryUsingBegin;
procedure FmodLibraryUsingEnd;

{$ifdef MSWINDOWS} {$define FMOD_DYNAMIC_LINK} {$endif}
{$ifdef LINUX} {$define FMOD_DYNAMIC_LINK} {$endif}
{$ifdef FMOD_DYNAMIC_LINK}
  {$I castleinternalfmod_dynamic.inc}
{$else}
  {$I castleinternalfmod_static.inc}
{$endif}
