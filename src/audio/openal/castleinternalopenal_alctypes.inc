{%MainUnit castleinternalopenal.pas}

{ translation of AL/alctypes.h }

type
  TALCdevice = record end;
  PALCdevice = ^TALCdevice;
  TALCcontext = record end;
  PALCcontext = ^TALCcontext;

type
  TALCenum = CInt;

{ Enumerant values begin at column 50. No tabs. }

{ Variables below are officially "constants" for OpenAL.
  In practice, it's horrible and lame but they are different between
  OpenAL implementations: implementations based on Windows Creative
  versions (this also includes OpenAL soft on Unix) have different
  values than OpenAL Sample Implementation.

  So they are declared as variables, and will be initialized
  only when OpenAL is initialized (when ALLibraryAvailable is @true). }

var
  {*
   * followed by <int> Hz
   } { }
  ALC_FREQUENCY: Integer;

  {*
   * followed by <int> Hz
   } { }
  ALC_REFRESH: Integer;

  {*
   * followed by AL_TRUE, AL_FALSE
   } { }
  ALC_SYNC: Integer;

  {
    The Specifier string for default device.
  }
  ALC_DEFAULT_DEVICE_SPECIFIER: Integer;
  ALC_DEVICE_SPECIFIER: Integer;
  ALC_EXTENSIONS: Integer;

  ALC_MAJOR_VERSION: Integer;
  ALC_MINOR_VERSION: Integer;

  ALC_ATTRIBUTES_SIZE: Integer;
  ALC_ALL_ATTRIBUTES: Integer;

const
  { bad value }
  ALC_INVALID = 0;

  {*
   * errors
   }

  {
    No error.
  }
  ALC_NO_ERROR = 0;

  {
    No device.
  }
  ALC_INVALID_DEVICE = $200;

  {
    Invalid context ID.
  }
  ALC_INVALID_CONTEXT = $201;

  {
    Bad enum.
  }
  ALC_INVALID_ENUM = $202;

  {
    Bad value.
  }
  ALC_INVALID_VALUE = $203;

  {
    Out of memory.
  }
  ALC_OUT_OF_MEMORY = $204;

  {*
   * Not sure if the following are conformant
   } { }
  ALC_FALSE = 0;
  ALC_TRUE = 1; { Kambi: to bylo zapisane w C jako (!(ALC_FALSE)) }
