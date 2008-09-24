{ translation of AL/alc.h }

{ we should include here OpenAL_altypes.inc but they are already included
  with OpenAL_al.inc }

{$I openal_alctypes.inc}

{ #define ALC_VERSION_0_1         1 }

var
  { }
  alcCreateContext : function( dev:PALCdevice; attrlist:PALint ):PALCcontext; extdecl;

  {*
   * There is no current context, as we can mix
   *  several active contexts. But al* calls
   *  only affect the current context.
   } { }
  alcMakeContextCurrent : function( alcHandle:PALCcontext ):TALCenum; extdecl;

  {
    Perform processing on a synced context, non-op on a asynchronous
    context.
  }
  alcProcessContext : function( alcHandle:PALCcontext ):PALCcontext; extdecl;

  {
    Suspend processing on an asynchronous context, non-op on a
    synced context.
  }
  alcSuspendContext : procedure( alcHandle:PALCcontext ); extdecl;

  alcDestroyContext : function( alcHandle:PALCcontext ):TALCenum; extdecl;

  alcGetError : function( dev:PALCdevice ):TALCenum; extdecl;

  alcGetCurrentContext : function():PALCcontext; extdecl;

  alcOpenDevice : function( tokstr:PChar ):PALCdevice; extdecl;
  alcCloseDevice : procedure( dev:PALCdevice ); extdecl;

  alcIsExtensionPresent : function(device:PALCdevice; extName:PChar):TALboolean; extdecl;
  alcGetProcAddress : function(device:PALCdevice; funcName:PChar):PALvoid; extdecl;
  alcGetEnumValue : function(device:PALCdevice; enumName:PChar):TALenum; extdecl;

  alcGetContextsDevice : function(context:PALCcontext):PALCdevice; extdecl;

  {*
   * Query functions
   } { }
  alcGetString : function(deviceHandle:PALCdevice; token:TALenum):PChar; extdecl;
  alcGetIntegerv : procedure(deviceHandle:PALCdevice; token:TALenum; size:TALsizei; dest:PALint); extdecl;
