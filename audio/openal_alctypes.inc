{ translation of AL/alctypes.h }

type 
  TALCdevice = record end;
  PALCdevice = ^TALCdevice;
  TALCcontext = record end;
  PALCcontext = ^TALCcontext;

type
  TALCenum = LongInt;

{ Enumerant values begin at column 50. No tabs. }

const

  { bad value }
  ALC_INVALID = 0;

  {$ifdef UNIX}
  {*
   * followed by <int> Hz
   }
  ALC_FREQUENCY = $100;

  {*
   * followed by <int> Hz
   }
  ALC_REFRESH = $101;

  {*
   * followed by AL_TRUE, AL_FALSE
   }
  ALC_SYNC = $102;
  {$endif}

  {*
   * errors
   }

  {*
   * No error
   }
  ALC_NO_ERROR = 0;

  {*
   * No device
   }
  ALC_INVALID_DEVICE = $200;

  {*
   * invalid context ID
   }
  ALC_INVALID_CONTEXT = $201;

  {*
   * bad enum
   }
  ALC_INVALID_ENUM = $202;

  {*
   * bad value
   }
  ALC_INVALID_VALUE = $203;

  {*
   * Out of memory.
   }
  ALC_OUT_OF_MEMORY = $204;


  {$ifdef UNIX}
  {*
   * The Specifier string for default device
   }
  ALC_DEFAULT_DEVICE_SPECIFIER = $300;
  ALC_DEVICE_SPECIFIER = $301;
  ALC_EXTENSIONS = $302;

  ALC_MAJOR_VERSION = $303;
  ALC_MINOR_VERSION = $304;

  ALC_ATTRIBUTES_SIZE = $305;
  ALC_ALL_ATTRIBUTES = $306;
  {$endif}

  {$ifdef WIN32}  
  { Kambi: strange, but Windows (Creative) OpenAL implementation has different
    values for these consts. }
  ALC_MAJOR_VERSION = $1000;
  ALC_MINOR_VERSION = $1001;
  ALC_ATTRIBUTES_SIZE = $1002;
  ALC_ALL_ATTRIBUTES = $1003;

  ALC_DEFAULT_DEVICE_SPECIFIER = $1004;
  ALC_DEVICE_SPECIFIER = $1005;
  ALC_EXTENSIONS = $1006;

  ALC_FREQUENCY = $1007;
  ALC_REFRESH = $1008;
  ALC_SYNC = $1009;
  {$endif}

  {*
   * Not sure if the following are conformant
   }
  ALC_FALSE = 0;
  ALC_TRUE = 1; { Kambi: to bylo zapisane w C jako (!(ALC_FALSE)) }
