{ FMOD API expressed in Pascal.

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

{$ifdef MSWINDOWS}
  { Distrubute with fmod.dll from
    c:/Program Files (x86)/FMOD SoundSystem/FMOD Studio API Windows/api/core/lib/x64/
  }
  {$define extdecl_callback := stdcall}
  {$define extdecl := extdecl_callback; external 'fmod'}
{$else}
  {$define extdecl_callback := cdecl}
  {$define extdecl := extdecl_callback}
{$endif}

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

{ FMOD global system functions (optional). }
function FMOD_Memory_Initialize           (poolmem: Pointer; poollen: CInt; useralloc: TFMOD_MEMORY_ALLOC_CALLBACK; userrealloc: TFMOD_MEMORY_REALLOC_CALLBACK; userfree: TFMOD_MEMORY_FREE_CALLBACK; memtypeflags: TFMOD_MEMORY_TYPE): TFMOD_RESULT; extdecl;
function FMOD_Memory_GetStats             (currentalloced: PCInt; maxalloced: PCInt; blocking: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Debug_Initialize            (flags: TFMOD_DEBUG_FLAGS; mode: TFMOD_DEBUG_MODE; callback: TFMOD_DEBUG_CALLBACK; filename: PChar): TFMOD_RESULT; extdecl;
function FMOD_File_SetDiskBusy            (busy: CInt): TFMOD_RESULT; extdecl;
function FMOD_File_GetDiskBusy            (busy: PCInt): TFMOD_RESULT; extdecl;

{ FMOD System factory functions.
  Use this to create an FMOD System Instance.
  below you will see FMOD_System_Init/Close to get started. }
function FMOD_System_Create               (system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_Release              (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ 'System' API }

{ Setup functions. }
function FMOD_System_SetOutput                 (system: PFMOD_SYSTEM; output: TFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutput                 (system: PFMOD_SYSTEM; output: PFMOD_OUTPUTTYPE): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumDrivers             (system: PFMOD_SYSTEM; numdrivers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDriverInfo             (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetDriver                 (system: PFMOD_SYSTEM; driver: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDriver                 (system: PFMOD_SYSTEM; driver: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetSoftwareChannels       (system: PFMOD_SYSTEM; numsoftwarechannels: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSoftwareChannels       (system: PFMOD_SYSTEM; numsoftwarechannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetSoftwareFormat         (system: PFMOD_SYSTEM; samplerate: CInt; speakermode: TFMOD_SPEAKERMODE; numrawspeakers: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSoftwareFormat         (system: PFMOD_SYSTEM; samplerate: PCInt; speakermode: PFMOD_SPEAKERMODE; numrawspeakers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetDSPBufferSize          (system: PFMOD_SYSTEM; bufferlength: CUInt; numbuffers: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetDSPBufferSize          (system: PFMOD_SYSTEM; bufferlength: PCUInt; numbuffers: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetFileSystem             (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK; userasyncread: TFMOD_FILE_ASYNCREAD_CALLBACK; userasynccancel: TFMOD_FILE_ASYNCCANCEL_CALLBACK; blockalign: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_AttachFileSystem          (system: PFMOD_SYSTEM; useropen: TFMOD_FILE_OPEN_CALLBACK; userclose: TFMOD_FILE_CLOSE_CALLBACK; userread: TFMOD_FILE_READ_CALLBACK; userseek: TFMOD_FILE_SEEK_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_System_SetAdvancedSettings       (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
function FMOD_System_GetAdvancedSettings       (system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): TFMOD_RESULT; extdecl;
function FMOD_System_SetCallback               (system: PFMOD_SYSTEM; callback: TFMOD_SYSTEM_CALLBACK; callbackmask: TFMOD_SYSTEM_CALLBACK_TYPE): TFMOD_RESULT; extdecl;

{ Plug-in support. }
(* TODO: translate
function FMOD_System_SetPluginPath             (system: PFMOD_SYSTEM; path: PChar): TFMOD_RESULT; extdecl;
function FMOD_System_LoadPlugin                (system: PFMOD_SYSTEM; filename: PChar; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_UnloadPlugin              (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumNestedPlugins       (system: PFMOD_SYSTEM; handle: CUInt; count: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNestedPlugin           (system: PFMOD_SYSTEM; handle: CUInt; index: CInt; nestedhandle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNumPlugins             (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; numplugins: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetPluginHandle           (system: PFMOD_SYSTEM; plugintype: TFMOD_PLUGINTYPE; index: CInt; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetPluginInfo             (system: PFMOD_SYSTEM; handle: CUInt; plugintype: PFMOD_PLUGINTYPE; name: PChar; namelen: CInt; version: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetOutputByPlugin         (system: PFMOD_SYSTEM; handle: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutputByPlugin         (system: PFMOD_SYSTEM; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_CreateDSPByPlugin         (system: PFMOD_SYSTEM; handle: CUInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_System_GetDSPInfoByPlugin        (system: PFMOD_SYSTEM; handle: CUInt; description: PPFMOD_DSP_DESCRIPTION): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterCodec             (system: PFMOD_SYSTEM; description: PFMOD_CODEC_DESCRIPTION; handle: PCUInt; priority: CUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterDSP               (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RegisterOutput            (system: PFMOD_SYSTEM; description: PFMOD_OUTPUT_DESCRIPTION; handle: PCUInt): TFMOD_RESULT; extdecl;
*)

{ Init/Close. }
function FMOD_System_Init                      (system: PFMOD_SYSTEM; maxchannels: CInt; flags: TFMOD_INITFLAGS; extradriverdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_System_Close                     (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ General post-init system functions. }
function FMOD_System_Update                    (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_SetSpeakerPosition        (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: CFloat; y: CFloat; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_GetSpeakerPosition        (system: PFMOD_SYSTEM; speaker: TFMOD_SPEAKER; x: PCFloat; y: PCFloat; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_SetStreamBufferSize       (system: PFMOD_SYSTEM; filebuffersize: CUInt; filebuffersizetype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_System_GetStreamBufferSize       (system: PFMOD_SYSTEM; filebuffersize: PCUInt; filebuffersizetype: PFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DSettings             (system: PFMOD_SYSTEM; dopplerscale: CFloat; distancefactor: CFloat; rolloffscale: CFloat): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DSettings             (system: PFMOD_SYSTEM; dopplerscale: PCFloat; distancefactor: PCFloat; rolloffscale: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DNumListeners         (system: PFMOD_SYSTEM; numlisteners: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DNumListeners         (system: PFMOD_SYSTEM; numlisteners: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DListenerAttributes   (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_System_Get3DListenerAttributes   (system: PFMOD_SYSTEM; listener: CInt; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_System_Set3DRolloffCallback      (system: PFMOD_SYSTEM; callback: TFMOD_3D_ROLLOFF_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_System_MixerSuspend              (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_MixerResume               (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_GetDefaultMixMatrix       (system: PFMOD_SYSTEM; sourcespeakermode: TFMOD_SPEAKERMODE; targetspeakermode: TFMOD_SPEAKERMODE; matrix: PCFloat; matrixhop: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetSpeakerModeChannels    (system: PFMOD_SYSTEM; mode: TFMOD_SPEAKERMODE; channels: PCInt): TFMOD_RESULT; extdecl;

{ System information functions. }
function FMOD_System_GetVersion                (system: PFMOD_SYSTEM; version: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetOutputHandle           (system: PFMOD_SYSTEM; handle: PPointer): TFMOD_RESULT; extdecl;
function FMOD_System_GetChannelsPlaying        (system: PFMOD_SYSTEM; channels: PCInt; realchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetCPUUsage               (system: PFMOD_SYSTEM; dsp: PCFloat; stream: PCFloat; geometry: PCFloat; update: PCFloat; total: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_GetFileUsage              (system: PFMOD_SYSTEM; sampleBytesRead: PCLongLong; streamBytesRead: PCLongLong; otherBytesRead: PCLongLong): TFMOD_RESULT; extdecl;

{ Sound/DSP/Channel/FX creation and retrieval. }
function FMOD_System_CreateSound               (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_System_CreateStream              (system: PFMOD_SYSTEM; name_or_data: PChar; mode: TFMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
// TODO function FMOD_System_CreateDSP                 (system: PFMOD_SYSTEM; description: PFMOD_DSP_DESCRIPTION; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
// TODO function FMOD_System_CreateDSPByType           (system: PFMOD_SYSTEM; type_: TFMOD_DSP_TYPE; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateChannelGroup        (system: PFMOD_SYSTEM; name: PChar; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateSoundGroup          (system: PFMOD_SYSTEM; name: PChar; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_CreateReverb3D            (system: PFMOD_SYSTEM; reverb: PPFMOD_REVERB3D): TFMOD_RESULT; extdecl;
function FMOD_System_PlaySound                 (system: PFMOD_SYSTEM; sound: PFMOD_SOUND; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_PlayDSP                   (system: PFMOD_SYSTEM; dsp: PFMOD_DSP; channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_GetChannel                (system: PFMOD_SYSTEM; channelid: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_System_GetMasterChannelGroup     (system: PFMOD_SYSTEM; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_System_GetMasterSoundGroup       (system: PFMOD_SYSTEM; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{ Routing to ports. }
function FMOD_System_AttachChannelGroupToPort  (system: PFMOD_SYSTEM; portType: TFMOD_PORT_TYPE; portIndex: TFMOD_PORT_INDEX; channelgroup: PFMOD_CHANNELGROUP; passThru: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_DetachChannelGroupFromPort(system: PFMOD_SYSTEM; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{ Reverb API. }
function FMOD_System_SetReverbProperties       (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_System_GetReverbProperties       (system: PFMOD_SYSTEM; instance: CInt; prop: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;

{ System level DSP functionality. }
function FMOD_System_LockDSP                   (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;
function FMOD_System_UnlockDSP                 (system: PFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{ Recording API. }
function FMOD_System_GetRecordNumDrivers       (system: PFMOD_SYSTEM; numdrivers: PCInt; numconnected: PCInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetRecordDriverInfo       (system: PFMOD_SYSTEM; id: CInt; name: PChar; namelen: CInt; guid: PFMOD_GUID; systemrate: PCInt; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PCInt; state: PFMOD_DRIVER_STATE): TFMOD_RESULT; extdecl;
function FMOD_System_GetRecordPosition         (system: PFMOD_SYSTEM; id: CInt; position: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_System_RecordStart               (system: PFMOD_SYSTEM; id: CInt; sound: PFMOD_SOUND; loop: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_System_RecordStop                (system: PFMOD_SYSTEM; id: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_IsRecording               (system: PFMOD_SYSTEM; id: CInt; recording: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{ Geometry API. }
function FMOD_System_CreateGeometry            (system: PFMOD_SYSTEM; maxpolygons: CInt; maxvertices: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
function FMOD_System_SetGeometrySettings       (system: PFMOD_SYSTEM; maxworldsize: CFloat): TFMOD_RESULT; extdecl;
function FMOD_System_GetGeometrySettings       (system: PFMOD_SYSTEM; maxworldsize: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_System_LoadGeometry              (system: PFMOD_SYSTEM; data: Pointer; datasize: CInt; geometry: PPFMOD_GEOMETRY): TFMOD_RESULT; extdecl;
function FMOD_System_GetGeometryOcclusion      (system: PFMOD_SYSTEM; listener: PFMOD_VECTOR; source: PFMOD_VECTOR; direct: PCFloat; reverb: PCFloat): TFMOD_RESULT; extdecl;

{ Network functions. }
function FMOD_System_SetNetworkProxy           (system: PFMOD_SYSTEM; proxy: PChar): TFMOD_RESULT; extdecl;
function FMOD_System_GetNetworkProxy           (system: PFMOD_SYSTEM; proxy: PChar; proxylen: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_SetNetworkTimeout         (system: PFMOD_SYSTEM; timeout: CInt): TFMOD_RESULT; extdecl;
function FMOD_System_GetNetworkTimeout         (system: PFMOD_SYSTEM; timeout: PCInt): TFMOD_RESULT; extdecl;

{ Userdata set/get. }
function FMOD_System_SetUserData               (system: PFMOD_SYSTEM; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_System_GetUserData               (system: PFMOD_SYSTEM; userdata: PPointer): TFMOD_RESULT; extdecl;

{ Sound API }

function FMOD_Sound_Release                    (sound: PFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSystemObject            (sound: PFMOD_SOUND; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Standard sound manipulation functions.
}

function FMOD_Sound_Lock                       (sound: PFMOD_SOUND; offset: CUInt; length: CUInt; ptr1: PPointer; ptr2: PPointer; len1: PCUInt; len2: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Unlock                     (sound: PFMOD_SOUND; ptr1: Pointer; ptr2: Pointer; len1: CUInt; len2: CUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetDefaults                (sound: PFMOD_SOUND; frequency: CFloat; priority: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetDefaults                (sound: PFMOD_SOUND; frequency: PCFloat; priority: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DMinMaxDistance        (sound: PFMOD_SOUND; min: CFloat; max: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DMinMaxDistance        (sound: PFMOD_SOUND; min: PCFloat; max: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DConeSettings          (sound: PFMOD_SOUND; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DConeSettings          (sound: PFMOD_SOUND; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_Set3DCustomRolloff         (sound: PFMOD_SOUND; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_Get3DCustomRolloff         (sound: PFMOD_SOUND; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSubSound                (sound: PFMOD_SOUND; index: CInt; subsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSubSoundParent          (sound: PFMOD_SOUND; parentsound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetName                    (sound: PFMOD_SOUND; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLength                  (sound: PFMOD_SOUND; length: PCUInt; lengthtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetFormat                  (sound: PFMOD_SOUND; type_: PFMOD_SOUND_TYPE; format: PFMOD_SOUND_FORMAT; channels: PCInt; bits: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetNumSubSounds            (sound: PFMOD_SOUND; numsubsounds: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetNumTags                 (sound: PFMOD_SOUND; numtags: PCInt; numtagsupdated: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetTag                     (sound: PFMOD_SOUND; name: PChar; index: CInt; tag: PFMOD_TAG): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetOpenState               (sound: PFMOD_SOUND; openstate: PFMOD_OPENSTATE; percentbuffered: PCUInt; starving: PFMOD_BOOL; diskbusy: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Sound_ReadData                   (sound: PFMOD_SOUND; buffer: Pointer; length: CUInt; read: PCUInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SeekData                   (sound: PFMOD_SOUND; pcm: CUInt): TFMOD_RESULT; extdecl;

function FMOD_Sound_SetSoundGroup              (sound: PFMOD_SOUND; soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSoundGroup              (sound: PFMOD_SOUND; soundgroup: PPFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Synchronization poAPI: CInt.  These points can come from markers embedded in wav files; and can also generate channel callbacks.
}

function FMOD_Sound_GetNumSyncPoints           (sound: PFMOD_SOUND; numsyncpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSyncPoint               (sound: PFMOD_SOUND; index: CInt; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetSyncPointInfo           (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT; name: PChar; namelen: CInt; offset: PCUInt; offsettype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_AddSyncPoint               (sound: PFMOD_SOUND; offset: CUInt; offsettype: TFMOD_TIMEUNIT; name: PChar; point: PPFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;
function FMOD_Sound_DeleteSyncPoint            (sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT): TFMOD_RESULT; extdecl;

{
     Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
}

function FMOD_Sound_SetMode                    (sound: PFMOD_SOUND; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMode                    (sound: PFMOD_SOUND; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetLoopCount               (sound: PFMOD_SOUND; loopcount: CInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLoopCount               (sound: PFMOD_SOUND; loopcount: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetLoopPoints              (sound: PFMOD_SOUND; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetLoopPoints              (sound: PFMOD_SOUND; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     For MOD/S3M/XM/IT/MID sequenced formats only.
}

function FMOD_Sound_GetMusicNumChannels        (sound: PFMOD_SOUND; numchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetMusicChannelVolume      (sound: PFMOD_SOUND; channel: CInt; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMusicChannelVolume      (sound: PFMOD_SOUND; channel: CInt; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_SetMusicSpeed              (sound: PFMOD_SOUND; speed: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetMusicSpeed              (sound: PFMOD_SOUND; speed: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Sound_SetUserData                (sound: PFMOD_SOUND; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Sound_GetUserData                (sound: PFMOD_SOUND; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Channel' API
}

function FMOD_Channel_GetSystemObject          (channel: PFMOD_CHANNEL; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

function FMOD_Channel_Stop                     (channel: PFMOD_CHANNEL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPaused                (channel: PFMOD_CHANNEL; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPaused                (channel: PFMOD_CHANNEL; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetVolume                (channel: PFMOD_CHANNEL; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetVolume                (channel: PFMOD_CHANNEL; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetVolumeRamp            (channel: PFMOD_CHANNEL; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetVolumeRamp            (channel: PFMOD_CHANNEL; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetAudibility            (channel: PFMOD_CHANNEL; audibility: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPitch                 (channel: PFMOD_CHANNEL; pitch: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPitch                 (channel: PFMOD_CHANNEL; pitch: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMute                  (channel: PFMOD_CHANNEL; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMute                  (channel: PFMOD_CHANNEL; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetReverbProperties      (channel: PFMOD_CHANNEL; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetReverbProperties      (channel: PFMOD_CHANNEL; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLowPassGain           (channel: PFMOD_CHANNEL; gain: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLowPassGain           (channel: PFMOD_CHANNEL; gain: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMode                  (channel: PFMOD_CHANNEL; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMode                  (channel: PFMOD_CHANNEL; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetCallback              (channel: PFMOD_CHANNEL; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_Channel_IsPlaying                (channel: PFMOD_CHANNEL; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

function FMOD_Channel_SetPan                   (channel: PFMOD_CHANNEL; pan: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixLevelsOutput       (channel: PFMOD_CHANNEL; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixLevelsInput        (channel: PFMOD_CHANNEL; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetMixMatrix             (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetMixMatrix             (channel: PFMOD_CHANNEL; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

function FMOD_Channel_GetDSPClock              (channel: PFMOD_CHANNEL; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetDelay                 (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetDelay                 (channel: PFMOD_CHANNEL; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_AddFadePoint             (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetFadePointRamp         (channel: PFMOD_CHANNEL; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_RemoveFadePoints         (channel: PFMOD_CHANNEL; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetFadePoints            (channel: PFMOD_CHANNEL; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

function FMOD_Channel_GetDSP                   (channel: PFMOD_CHANNEL; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_AddDSP                   (channel: PFMOD_CHANNEL; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_RemoveDSP                (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetNumDSPs               (channel: PFMOD_CHANNEL; numdsps: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetDSPIndex              (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetDSPIndex              (channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

function FMOD_Channel_Set3DAttributes          (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DAttributes          (channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DMinMaxDistance      (channel: PFMOD_CHANNEL; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DMinMaxDistance      (channel: PFMOD_CHANNEL; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DConeSettings        (channel: PFMOD_CHANNEL; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DConeSettings        (channel: PFMOD_CHANNEL; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DConeOrientation     (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DConeOrientation     (channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DCustomRolloff       (channel: PFMOD_CHANNEL; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DCustomRolloff       (channel: PFMOD_CHANNEL; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DOcclusion           (channel: PFMOD_CHANNEL; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DOcclusion           (channel: PFMOD_CHANNEL; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DSpread              (channel: PFMOD_CHANNEL; angle: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DSpread              (channel: PFMOD_CHANNEL; angle: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DLevel               (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DLevel               (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DDopplerLevel        (channel: PFMOD_CHANNEL; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DDopplerLevel        (channel: PFMOD_CHANNEL; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Set3DDistanceFilter      (channel: PFMOD_CHANNEL; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_Get3DDistanceFilter      (channel: PFMOD_CHANNEL; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Channel_SetUserData              (channel: PFMOD_CHANNEL; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetUserData              (channel: PFMOD_CHANNEL; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Channel specific control functionality.
}

function FMOD_Channel_SetFrequency             (channel: PFMOD_CHANNEL; frequency: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetFrequency             (channel: PFMOD_CHANNEL; frequency: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPriority              (channel: PFMOD_CHANNEL; priority: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPriority              (channel: PFMOD_CHANNEL; priority: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetPosition              (channel: PFMOD_CHANNEL; position: CUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetPosition              (channel: PFMOD_CHANNEL; position: PCUInt; postype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetChannelGroup          (channel: PFMOD_CHANNEL; channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetChannelGroup          (channel: PFMOD_CHANNEL; channelgroup: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLoopCount             (channel: PFMOD_CHANNEL; loopcount: CInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLoopCount             (channel: PFMOD_CHANNEL; loopcount: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Channel_SetLoopPoints            (channel: PFMOD_CHANNEL; loopstart: CUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: CUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetLoopPoints            (channel: PFMOD_CHANNEL; loopstart: PCUInt; loopstarttype: TFMOD_TIMEUNIT; loopend: PCUInt; loopendtype: TFMOD_TIMEUNIT): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_Channel_IsVirtual                (channel: PFMOD_CHANNEL; isvirtual: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetCurrentSound          (channel: PFMOD_CHANNEL; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_Channel_GetIndex                 (channel: PFMOD_CHANNEL; index: PCInt): TFMOD_RESULT; extdecl;

{
    'ChannelGroup' API
}

function FMOD_ChannelGroup_GetSystemObject     (channelgroup: PFMOD_CHANNELGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     General control functionality for Channels and ChannelGroups.
}

function FMOD_ChannelGroup_Stop                (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetPaused           (channelgroup: PFMOD_CHANNELGROUP; paused: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetPaused           (channelgroup: PFMOD_CHANNELGROUP; paused: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetVolume           (channelgroup: PFMOD_CHANNELGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetVolume           (channelgroup: PFMOD_CHANNELGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetVolumeRamp       (channelgroup: PFMOD_CHANNELGROUP; ramp: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetVolumeRamp       (channelgroup: PFMOD_CHANNELGROUP; ramp: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetAudibility       (channelgroup: PFMOD_CHANNELGROUP; audibility: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetPitch            (channelgroup: PFMOD_CHANNELGROUP; pitch: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetPitch            (channelgroup: PFMOD_CHANNELGROUP; pitch: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMute             (channelgroup: PFMOD_CHANNELGROUP; mute: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMute             (channelgroup: PFMOD_CHANNELGROUP; mute: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetReverbProperties (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetReverbProperties (channelgroup: PFMOD_CHANNELGROUP; instance: CInt; wet: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetLowPassGain      (channelgroup: PFMOD_CHANNELGROUP; gain: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetLowPassGain      (channelgroup: PFMOD_CHANNELGROUP; gain: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMode             (channelgroup: PFMOD_CHANNELGROUP; mode: TFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMode             (channelgroup: PFMOD_CHANNELGROUP; mode: PFMOD_MODE): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetCallback         (channelgroup: PFMOD_CHANNELGROUP; callback: TFMOD_CHANNELCONTROL_CALLBACK): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_IsPlaying           (channelgroup: PFMOD_CHANNELGROUP; isplaying: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Note all 'set' functions alter a final matrix; this is why the only get function is getMixMatrix; to avoid other get functions returning incorrect/obsolete values.
}

function FMOD_ChannelGroup_SetPan              (channelgroup: PFMOD_CHANNELGROUP; pan: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixLevelsOutput  (channelgroup: PFMOD_CHANNELGROUP; frontleft: CFloat; frontright: CFloat; center: CFloat; lfe: CFloat; surroundleft: CFloat; surroundright: CFloat; backleft: CFloat; backright: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixLevelsInput   (channelgroup: PFMOD_CHANNELGROUP; levels: PCFloat; numlevels: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetMixMatrix        (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetMixMatrix        (channelgroup: PFMOD_CHANNELGROUP; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;

{
     Clock based functionality.
}

function FMOD_ChannelGroup_GetDSPClock         (channelgroup: PFMOD_CHANNELGROUP; dspclock: PCULongLong; parentclock: PCULongLong): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetDelay            (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong; stopchannels: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetDelay            (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: PCULongLong; dspclock_end: PCULongLong; stopchannels: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_AddFadePoint        (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetFadePointRamp    (channelgroup: PFMOD_CHANNELGROUP; dspclock: CULongLong; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_RemoveFadePoints    (channelgroup: PFMOD_CHANNELGROUP; dspclock_start: CULongLong; dspclock_end: CULongLong): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetFadePoints       (channelgroup: PFMOD_CHANNELGROUP; numpoints: PCUInt; point_dspclock: PCULongLong; point_volume: PCFloat): TFMOD_RESULT; extdecl;

{
     DSP effects.
}

function FMOD_ChannelGroup_GetDSP              (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_AddDSP              (channelgroup: PFMOD_CHANNELGROUP; index: CInt; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_RemoveDSP           (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumDSPs          (channelgroup: PFMOD_CHANNELGROUP; numdsps: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_SetDSPIndex         (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetDSPIndex         (channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: PCInt): TFMOD_RESULT; extdecl;

{
     3D functionality.
}

function FMOD_ChannelGroup_Set3DAttributes     (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DAttributes     (channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DMinMaxDistance (channelgroup: PFMOD_CHANNELGROUP; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DMinMaxDistance (channelgroup: PFMOD_CHANNELGROUP; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DConeSettings   (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: CFloat; outsideconeangle: CFloat; outsidevolume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DConeSettings   (channelgroup: PFMOD_CHANNELGROUP; insideconeangle: PCFloat; outsideconeangle: PCFloat; outsidevolume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DConeOrientation(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DConeOrientation(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DCustomRolloff  (channelgroup: PFMOD_CHANNELGROUP; points: PFMOD_VECTOR; numpoints: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DCustomRolloff  (channelgroup: PFMOD_CHANNELGROUP; points: PPFMOD_VECTOR; numpoints: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DOcclusion      (channelgroup: PFMOD_CHANNELGROUP; directocclusion: CFloat; reverbocclusion: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DOcclusion      (channelgroup: PFMOD_CHANNELGROUP; directocclusion: PCFloat; reverbocclusion: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DSpread         (channelgroup: PFMOD_CHANNELGROUP; angle: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DSpread         (channelgroup: PFMOD_CHANNELGROUP; angle: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DLevel          (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DLevel          (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DDopplerLevel   (channelgroup: PFMOD_CHANNELGROUP; level: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DDopplerLevel   (channelgroup: PFMOD_CHANNELGROUP; level: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Set3DDistanceFilter (channelgroup: PFMOD_CHANNELGROUP; custom: TFMOD_BOOL; customLevel: CFloat; centerFreq: CFloat): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_Get3DDistanceFilter (channelgroup: PFMOD_CHANNELGROUP; custom: PFMOD_BOOL; customLevel: PCFloat; centerFreq: PCFloat): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_ChannelGroup_SetUserData         (channelgroup: PFMOD_CHANNELGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetUserData         (channelgroup: PFMOD_CHANNELGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

function FMOD_ChannelGroup_Release             (channelgroup: PFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Nested channel groups.
}

function FMOD_ChannelGroup_AddGroup            (channelgroup: PFMOD_CHANNELGROUP; group: PFMOD_CHANNELGROUP; propagatedspclock: TFMOD_BOOL; connection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumGroups        (channelgroup: PFMOD_CHANNELGROUP; numgroups: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetGroup            (channelgroup: PFMOD_CHANNELGROUP; index: CInt; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetParentGroup      (channelgroup: PFMOD_CHANNELGROUP; group: PPFMOD_CHANNELGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_ChannelGroup_GetName             (channelgroup: PFMOD_CHANNELGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetNumChannels      (channelgroup: PFMOD_CHANNELGROUP; numchannels: PCInt): TFMOD_RESULT; extdecl;
function FMOD_ChannelGroup_GetChannel          (channelgroup: PFMOD_CHANNELGROUP; index: CInt; channel: PPFMOD_CHANNEL): TFMOD_RESULT; extdecl;

{
    'SoundGroup' API
}

function FMOD_SoundGroup_Release               (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetSystemObject       (soundgroup: PFMOD_SOUNDGROUP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     SoundGroup control functions.
}

function FMOD_SoundGroup_SetMaxAudible         (soundgroup: PFMOD_SOUNDGROUP; maxaudible: CInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMaxAudible         (soundgroup: PFMOD_SOUNDGROUP; maxaudible: PCInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetMaxAudibleBehavior (soundgroup: PFMOD_SOUNDGROUP; behavior: TFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMaxAudibleBehavior (soundgroup: PFMOD_SOUNDGROUP; behavior: PFMOD_SOUNDGROUP_BEHAVIOR): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetMuteFadeSpeed      (soundgroup: PFMOD_SOUNDGROUP; speed: CFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetMuteFadeSpeed      (soundgroup: PFMOD_SOUNDGROUP; speed: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_SetVolume             (soundgroup: PFMOD_SOUNDGROUP; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetVolume             (soundgroup: PFMOD_SOUNDGROUP; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_Stop                  (soundgroup: PFMOD_SOUNDGROUP): TFMOD_RESULT; extdecl;

{
     Information only functions.
}

function FMOD_SoundGroup_GetName               (soundgroup: PFMOD_SOUNDGROUP; name: PChar; namelen: CInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetNumSounds          (soundgroup: PFMOD_SOUNDGROUP; numsounds: PCInt): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetSound              (soundgroup: PFMOD_SOUNDGROUP; index: CInt; sound: PPFMOD_SOUND): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetNumPlaying         (soundgroup: PFMOD_SOUNDGROUP; numplaying: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_SoundGroup_SetUserData           (soundgroup: PFMOD_SOUNDGROUP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_SoundGroup_GetUserData           (soundgroup: PFMOD_SOUNDGROUP; userdata: PPointer): TFMOD_RESULT; extdecl;

(* TODO: translate
{
    'DSP' API
}

function FMOD_DSP_Release                      (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetSystemObject              (dsp: PFMOD_DSP; system: PPFMOD_SYSTEM): TFMOD_RESULT; extdecl;

{
     Connection / disconnection / input and output enumeration.
}

function FMOD_DSP_AddInput                     (dsp: PFMOD_DSP; input: PFMOD_DSP; connection: PPFMOD_DSPCONNECTION; type_: TFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;
function FMOD_DSP_DisconnectFrom               (dsp: PFMOD_DSP; target: PFMOD_DSP; connection: PFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_DSP_DisconnectAll                (dsp: PFMOD_DSP; inputs: TFMOD_BOOL; outputs: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumInputs                 (dsp: PFMOD_DSP; numinputs: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumOutputs                (dsp: PFMOD_DSP; numoutputs: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetInput                     (dsp: PFMOD_DSP; index: CInt; input: PPFMOD_DSP; inputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetOutput                    (dsp: PFMOD_DSP; index: CInt; output: PPFMOD_DSP; outputconnection: PPFMOD_DSPCONNECTION): TFMOD_RESULT; extdecl;

{
     DSP unit control.
}

function FMOD_DSP_SetActive                    (dsp: PFMOD_DSP; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetActive                    (dsp: PFMOD_DSP; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetBypass                    (dsp: PFMOD_DSP; bypass: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetBypass                    (dsp: PFMOD_DSP; bypass: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetWetDryMix                 (dsp: PFMOD_DSP; prewet: CFloat; postwet: CFloat; dry: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetWetDryMix                 (dsp: PFMOD_DSP; prewet: PCFloat; postwet: PCFloat; dry: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetChannelFormat             (dsp: PFMOD_DSP; channelmask: TFMOD_CHANNELMASK; numchannels: CInt; source_speakermode: TFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetChannelFormat             (dsp: PFMOD_DSP; channelmask: PFMOD_CHANNELMASK; numchannels: PCInt; source_speakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetOutputChannelFormat       (dsp: PFMOD_DSP; inmask: TFMOD_CHANNELMASK; inchannels: CInt; inspeakermode: TFMOD_SPEAKERMODE; outmask: PFMOD_CHANNELMASK; outchannels: PCInt; outspeakermode: PFMOD_SPEAKERMODE): TFMOD_RESULT; extdecl;
function FMOD_DSP_Reset                        (dsp: PFMOD_DSP): TFMOD_RESULT; extdecl;

{
     DSP parameter control.
}

function FMOD_DSP_SetParameterFloat            (dsp: PFMOD_DSP; index: CInt; value: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterInt              (dsp: PFMOD_DSP; index: CInt; value: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterBool             (dsp: PFMOD_DSP; index: CInt; value: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_SetParameterData             (dsp: PFMOD_DSP; index: CInt; data: Pointer; length: CUInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterFloat            (dsp: PFMOD_DSP; index: CInt; value: PCFloat; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterInt              (dsp: PFMOD_DSP; index: CInt; value: PCInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterBool             (dsp: PFMOD_DSP; index: CInt; value: PFMOD_BOOL; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterData             (dsp: PFMOD_DSP; index: CInt; data: PPointer; length: PCUInt; valuestr: PChar; valuestrlen: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetNumParameters             (dsp: PFMOD_DSP; numparams: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetParameterInfo             (dsp: PFMOD_DSP; index: CInt; desc: PPFMOD_DSP_PARAMETER_DESC): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetDataParameterIndex        (dsp: PFMOD_DSP; datatype: CInt; index: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_ShowConfigDialog             (dsp: PFMOD_DSP; hwnd: Pointer; show: TFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     DSP attributes.
}

function FMOD_DSP_GetInfo                      (dsp: PFMOD_DSP; name: PChar; version: PCUInt; channels: PCInt; configwidth: PCInt; configheight: PCInt): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetType                      (dsp: PFMOD_DSP; type_: PFMOD_DSP_TYPE): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetIdle                      (dsp: PFMOD_DSP; idle: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_DSP_SetUserData                  (dsp: PFMOD_DSP; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetUserData                  (dsp: PFMOD_DSP; userdata: PPointer): TFMOD_RESULT; extdecl;

{
     Metering.
}

function FMOD_DSP_SetMeteringEnabled           (dsp: PFMOD_DSP; inputEnabled: TFMOD_BOOL; outputEnabled: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetMeteringEnabled           (dsp: PFMOD_DSP; inputEnabled: PFMOD_BOOL; outputEnabled: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetMeteringInfo              (dsp: PFMOD_DSP; inputInfo: PFMOD_DSP_METERING_INFO; outputInfo: PFMOD_DSP_METERING_INFO): TFMOD_RESULT; extdecl;
function FMOD_DSP_GetCPUUsage                  (dsp: PFMOD_DSP; exclusive: PCUInt; inclusive: PCUInt): TFMOD_RESULT; extdecl;
*)

{
    'DSPConnection' API
}

function FMOD_DSPConnection_GetInput           (dspconnection: PFMOD_DSPCONNECTION; input: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetOutput          (dspconnection: PFMOD_DSPCONNECTION; output: PPFMOD_DSP): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_SetMix             (dspconnection: PFMOD_DSPCONNECTION; volume: CFloat): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetMix             (dspconnection: PFMOD_DSPCONNECTION; volume: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_SetMixMatrix       (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: CInt; inchannels: CInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetMixMatrix       (dspconnection: PFMOD_DSPCONNECTION; matrix: PCFloat; outchannels: PCInt; inchannels: PCInt; inchannel_hop: CInt): TFMOD_RESULT; extdecl;
// TODO function FMOD_DSPConnection_GetType            (dspconnection: PFMOD_DSPCONNECTION; type_: PFMOD_DSPCONNECTION_TYPE): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_DSPConnection_SetUserData        (dspconnection: PFMOD_DSPCONNECTION; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_DSPConnection_GetUserData        (dspconnection: PFMOD_DSPCONNECTION; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Geometry' API
}

function FMOD_Geometry_Release                 (geometry: PFMOD_GEOMETRY): TFMOD_RESULT; extdecl;

{
     Polygon manipulation.
}

function FMOD_Geometry_AddPolygon              (geometry: PFMOD_GEOMETRY; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL; numvertices: CInt; vertices: PFMOD_VECTOR; polygonindex: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetNumPolygons          (geometry: PFMOD_GEOMETRY; numpolygons: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetMaxPolygons          (geometry: PFMOD_GEOMETRY; maxpolygons: PCInt; maxvertices: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonNumVertices   (geometry: PFMOD_GEOMETRY; index: CInt; numvertices: PCInt): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPolygonVertex        (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonVertex        (geometry: PFMOD_GEOMETRY; index: CInt; vertexindex: CInt; vertex: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPolygonAttributes    (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: CFloat; reverbocclusion: CFloat; doublesided: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPolygonAttributes    (geometry: PFMOD_GEOMETRY; index: CInt; directocclusion: PCFloat; reverbocclusion: PCFloat; doublesided: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Object manipulation.
}

function FMOD_Geometry_SetActive               (geometry: PFMOD_GEOMETRY; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetActive               (geometry: PFMOD_GEOMETRY; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetRotation             (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetRotation             (geometry: PFMOD_GEOMETRY; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetPosition             (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetPosition             (geometry: PFMOD_GEOMETRY; position: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_SetScale                (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetScale                (geometry: PFMOD_GEOMETRY; scale: PFMOD_VECTOR): TFMOD_RESULT; extdecl;
function FMOD_Geometry_Save                    (geometry: PFMOD_GEOMETRY; data: Pointer; datasize: PCInt): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Geometry_SetUserData             (geometry: PFMOD_GEOMETRY; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Geometry_GetUserData             (geometry: PFMOD_GEOMETRY; userdata: PPointer): TFMOD_RESULT; extdecl;

{
    'Reverb3D' API
}

function FMOD_Reverb3D_Release                 (reverb3d: PFMOD_REVERB3D): TFMOD_RESULT; extdecl;

{
     Reverb manipulation.
}

function FMOD_Reverb3D_Set3DAttributes         (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: CFloat; maxdistance: CFloat): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_Get3DAttributes         (reverb3d: PFMOD_REVERB3D; position: PFMOD_VECTOR; mindistance: PCFloat; maxdistance: PCFloat): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_SetProperties           (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetProperties           (reverb3d: PFMOD_REVERB3D; properties: PFMOD_REVERB_PROPERTIES): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_SetActive               (reverb3d: PFMOD_REVERB3D; active: TFMOD_BOOL): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetActive               (reverb3d: PFMOD_REVERB3D; active: PFMOD_BOOL): TFMOD_RESULT; extdecl;

{
     Userdata set/get.
}

function FMOD_Reverb3D_SetUserData             (reverb3d: PFMOD_REVERB3D; userdata: Pointer): TFMOD_RESULT; extdecl;
function FMOD_Reverb3D_GetUserData             (reverb3d: PFMOD_REVERB3D; userdata: PPointer): TFMOD_RESULT; extdecl;

implementation

end.
