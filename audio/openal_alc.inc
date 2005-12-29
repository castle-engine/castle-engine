{ translation of AL/alc.h }

{ we should include here OpenAL_altypes.inc but they are already included
  with OpenAL_al.inc }

{$I OpenAL_alctypes.inc}

{ #define ALC_VERSION_0_1         1 }

var

  alcCreateContext : function( dev:PALCdevice; attrlist:PALint ):PALCcontext; alc_call

  {*
   * There is no current context, as we can mix
   *  several active contexts. But al* calls
   *  only affect the current context.
   }
  alcMakeContextCurrent : function( alcHandle:PALCcontext ):TALCenum; alc_call

  {*
   * Perform processing on a synced context, non-op on a asynchronous
   * context.
   }
  alcProcessContext : function( alcHandle:PALCcontext ):PALCcontext; alc_call

  {*
   * Suspend processing on an asynchronous context, non-op on a
   * synced context.
   }
  alcSuspendContext : procedure( alcHandle:PALCcontext ); alc_call

  alcDestroyContext : function( alcHandle:PALCcontext ):TALCenum; alc_call

  alcGetError : function( dev:PALCdevice ):TALCenum; alc_call

  alcGetCurrentContext : function():PALCcontext; alc_call

  alcOpenDevice : function( tokstr:PChar ):PALCdevice; alc_call
  alcCloseDevice : procedure( dev:PALCdevice ); alc_call

  alcIsExtensionPresent : function(device:PALCdevice; extName:PChar):TALboolean; alc_call
  alcGetProcAddress : function(device:PALCdevice; funcName:PChar):PALvoid; alc_call
  alcGetEnumValue : function(device:PALCdevice; enumName:PChar):TALenum; alc_call

  alcGetContextsDevice : function(context:PALCcontext):PALCdevice; alc_call

  {*
   * Query functions
   }
  alcGetString : function(deviceHandle:PALCdevice; token:TALenum):PChar; alc_call
  alcGetIntegerv : procedure(deviceHandle:PALCdevice; token:TALenum; size:TALsizei; dest:PALint); alc_call
