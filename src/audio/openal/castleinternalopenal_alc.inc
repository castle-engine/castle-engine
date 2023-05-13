{%MainUnit castleinternalopenal.pas}

{ translation of AL/alc.h }

{ we should include here OpenAL_altypes.inc but they are already included
  with OpenAL_al.inc }

{$I castleinternalopenal_alctypes.inc}

{ #define ALC_VERSION_0_1         1 }

var
  { }
  alcCreateContext : function( dev:PALCdevice; attrlist:PALint ):PALCcontext; cdecl;

  {*
   * There is no current context, as we can mix
   *  several active contexts. But al* calls
   *  only affect the current context.
   } { }
  alcMakeContextCurrent : function( alcHandle:PALCcontext ):TALCenum; cdecl;

  {
    Perform processing on a synced context, non-op on a asynchronous
    context.
  }
  alcProcessContext : function( alcHandle:PALCcontext ):PALCcontext; cdecl;

  {
    Suspend processing on an asynchronous context, non-op on a
    synced context.
  }
  alcSuspendContext : procedure( alcHandle:PALCcontext ); cdecl;

  alcDestroyContext : function( alcHandle:PALCcontext ):TALCenum; cdecl;

  alcGetError : function( dev:PALCdevice ):TALCenum; cdecl;

  alcGetCurrentContext : function():PALCcontext; cdecl;

  alcOpenDevice : function( tokstr:PChar ):PALCdevice; cdecl;
  alcCloseDevice : procedure( dev:PALCdevice ); cdecl;

  alcIsExtensionPresent : function(device:PALCdevice; extName:PChar):TALboolean; cdecl;
  alcGetProcAddress : function(device:PALCdevice; funcName:PChar):PALvoid; cdecl;
  alcGetEnumValue : function(device:PALCdevice; enumName:PChar):TALenum; cdecl;

  alcGetContextsDevice : function(context:PALCcontext):PALCdevice; cdecl;

  {*
   * Query functions
   } { }
  alcGetString : function(deviceHandle:PALCdevice; token:TALenum):PChar; cdecl;
  alcGetIntegerv : procedure(deviceHandle:PALCdevice; token:TALenum; size:TALsizei; dest:PALint); cdecl;
