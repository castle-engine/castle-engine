{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenAL library bindings.
  This is a translation of OpenAL C headers: @code(AL/al.h, AL/alc.h)
  (and included headers: @code(AL/altypes.h, AL/alctypes.h)).

  The library is loaded in unit initialization, and if not found
  we will merely set global ALLibraryAvailable variable to @false.
  This allows you to check at runtime and gracefully work
  if OpenAL is not installed (e.g. you can turn off game sounds.)

  Renamed to CastleInternalOpenAL (from OpenAL) to avoid clash with FPC's OpenAL
  unit. FPC unit doesn't provide above ALLibraryAvailable feature, and it doesn't
  integrate with CastleVectors.

  @exclude (Unit not really ready for PasDoc, with many comments from
  original C headers.) }
unit CastleInternalOpenAL;

{ C->Pascal translation comments:

  @unorderedList(
    @item(We use CTypes.)
    @item(All types start with T, except pointers starting with P.
      This is normal for Pascal.)
    @item(In most cases function parameters and return values
      PALubyte were changed into PChar, as their intention is to handle
      C strings. ABI is the same,
      but using PChar allows easier usage under Pascal compilers.)
    @item(I didn't use h2pas. OpenGL headers are clean enough to handle
      most stuff with some regexps and Emacs macros.)
    @item(All original comments are preserved, sometimes they are useful.)
  )
}

{ If OPENAL_DEPRECATED, we will define and load some functions
  considered deprecated.
  See http://opensource.creative.com/pipermail/openal-devel/2005-April/002985.html . }
{ $define OPENAL_DEPRECATED}

{$i castleconf.inc}

interface

uses SysUtils, Math, CastleVectors, CTypes;

{$ifdef FREEBSD}
  {$linklib pthread}
{$endif}

{$ifndef PASDOC}
{$I castleinternalopenal_al.inc}
{$I castleinternalopenal_alc.inc}
{$endif}

{ ----------------------------------------------------------------------------
  Types to make this unit cooperate nicely with CastleVectors.

  Definitions of types below are connected with definitions in openal_altypes.inc
  --- if you change some type in openal_altypes.inc you have to adjust definitions
  below as well. That's because we use constructions like
    TALVector3f = TVector3
  instead of
    TALVector3f = array [0..2] of TALSingle
  This way we can use all TVector3 functions from CastleVectors
  to operate on OpenAL vectors.
}

type
  { }
  TALVector3f = TVector3;
  TALVector3d = TVector3Double;
  { TwoVectors type is useful for OpenAL's listener ORIENTATION property.
    @groupBegin }
  TALTwoVectors3f = array [0..1] of TALVector3f;
  TALTwoVectors3d = array [0..1] of TALVector3d;
  { @groupEnd }

{ ---------------------------------------------------------------------------- }

{ Is OpenAL library loaded.
  This means that functions alXxx and alcXxx are loaded (not @nil).
  When OpenAL is not available, alXxx and alcXxx are @nil,
  and this variable is @false, and you can gracefully handle it in your programs
  (for example, continue normal game, just with sound turned off). }
function ALLibraryAvailable: boolean;

{ Reset OpenAL library.

  In this unit's initialization, we link to OpenAL library, load all symbols,
  and initialize ALLibraryAvailable. In this unit's finalization,
  we release library handles and set ALLibraryAvailable back to @false.

  What this procedure does?
  It behaves like finalizing this unit (releasing OpenAL library handles),
  then sleeping for some short amount, and initializing this unit again
  (loading OpenAL library symbols).

  When is it needed?

  Unix OpenAL implementation seems to have a problem.
  It's reproduceable in "The Castle" code:
  When I want to switch from one OpenAL device to the other,
  I would like to close OpenAL context and device,
  change my device name, and create OpenAL context and device again.
  But this causes problems under Linux:
  when user selects some non-working device (e.g. when I select
  esd device while Esound daemon is not running), OpenAL (correctly)
  doesn't let me initialize the device (returns nil).
  But then, after such failure, when I try to initialize *any* other device
  (that worked before, like alsa or waveout), the initialization of
  them also fails. I have to restart my program (to restart whole
  OpenAL library state) to be able to initialize any other backend.

  Workaround that I found is to do such OpenALRestart.
  Release OpenAL library, wait some short time
  (I guess that otherwise some resources will still occupy the sound
  device? In any case, this short sleep is needed...), then do
  OpenAL library initialization again. And things work correctly.

  Note that this was possibly only necessary for old Loki implementation?  }
procedure OpenALRestart;

{ Detect OpenAL SI (Sample Implementation) from Loki.
  This was the most common implementation on most Unixes,
  until OpenAL-soft came.

  Use this only when ALLibraryAvailable, since this is just a call to some alcGetString. }
function OpenALSampleImplementation: boolean;

procedure OpenALInitialization;

implementation

uses CastleUtils, CastleLog, CastleDynLib;

function OpenALSampleImplementation: boolean;
begin
  Result := alGetString(AL_VENDOR) = 'J. Valenzuela';
end;

{ unit init/fini ------------------------------------------------------------ }

procedure OpenALFinalization; forward;

procedure OpenALRestart;
begin
  OpenALFinalization;
  Sleep(500);
  OpenALInitialization;
end;

var
  ALLibrary: TDynLib;

function ALLibraryAvailable: boolean;
begin
  Result := ALLibrary <> nil;
end;

procedure OpenALInitialization;
begin
  { Be sure to start with a "clean" state. }
  OpenALFinalization;

  ALLibrary := TDynLib.Load(
    {$ifdef UNIX}
      {$ifdef DARWIN}
        '/System/Library/Frameworks/OpenAL.framework/OpenAL'
      {$else}
        { We are linking dynamically using dlopen/dlsym.
          Library name should not be just 'libopenal.so',
          as that is only available in -dev package of openal. }
        'libopenal.so.0'
      {$endif}
    {$endif}
    {$ifdef MSWINDOWS} 'OpenAL32.dll' {$endif}
    , false);

  {$ifdef UNIX}
  if ALLibrary = nil then
    ALLibrary := TDynLib.Load('libopenal.so.1', false);
  { esp. good for Android with our version in ../../../external_libraries/arm-android/ }
  if ALLibrary = nil then
    ALLibrary := TDynLib.Load('libopenal.so', false);
  {$endif}

  if ALLibrary <> nil then
  begin
    { alXxx functions -------------------------------------------------------- }
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alEnable, 'alEnable');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDisable, 'alDisable');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alIsEnabled, 'alIsEnabled');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBooleanv, 'alGetBooleanv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetIntegerv, 'alGetIntegerv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetFloatv, 'alGetFloatv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetDoublev, 'alGetDoublev');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetString, 'alGetString');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBoolean, 'alGetBoolean');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetInteger, 'alGetInteger');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetFloat, 'alGetFloat');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetDouble, 'alGetDouble');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetError, 'alGetError');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alIsExtensionPresent, 'alIsExtensionPresent');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetProcAddress, 'alGetProcAddress');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetEnumValue, 'alGetEnumValue');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alListenerf, 'alListenerf');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alListeneri, 'alListeneri');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alListener3f, 'alListener3f');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alListenerfv, 'alListenerfv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetListeneri, 'alGetListeneri');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetListenerf, 'alGetListenerf');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetListenerfv, 'alGetListenerfv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetListener3f, 'alGetListener3f');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGenSources, 'alGenSources');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDeleteSources, 'alDeleteSources');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alIsSource, 'alIsSource');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcei, 'alSourcei');

    { alSource3i possibily not present in older < 1.1 OpenAL implementations,
      in particular in Loki versions. Handle gracefully: set to @nil, allowing
      programs that don't use alSource3i to still work. }
    try
      ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSource3i, 'alSource3i');
    except
      on EDynLibError do alSource3i := nil;
    end;

    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcef, 'alSourcef');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSource3f, 'alSource3f');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcefv, 'alSourcefv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetSourcei, 'alGetSourcei');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetSourcef, 'alGetSourcef');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetSourcefv, 'alGetSourcefv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcePlayv, 'alSourcePlayv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceStopv, 'alSourceStopv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceRewindv, 'alSourceRewindv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcePausev, 'alSourcePausev');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcePlay, 'alSourcePlay');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourcePause, 'alSourcePause');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceRewind, 'alSourceRewind');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceStop, 'alSourceStop');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGenBuffers, 'alGenBuffers');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDeleteBuffers, 'alDeleteBuffers');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alIsBuffer, 'alIsBuffer');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alBufferData, 'alBufferData');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBufferi, 'alGetBufferi');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBufferf, 'alGetBufferf');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceQueueBuffers, 'alSourceQueueBuffers');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alSourceUnqueueBuffers, 'alSourceUnqueueBuffers');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDopplerFactor, 'alDopplerFactor');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDopplerVelocity, 'alDopplerVelocity');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alDistanceModel, 'alDistanceModel');

    {$ifdef OPENAL_DEPRECATED}
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBufferiv, 'alGetBufferiv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetBufferfv, 'alGetBufferfv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetSourceiv, 'alGetSourceiv');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alGetListeneriv, 'alGetListeneriv');
    {$endif OPENAL_DEPRECATED}

    { alcXxx functions ---------------------------------------- }
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcCreateContext, 'alcCreateContext');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcMakeContextCurrent, 'alcMakeContextCurrent');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcProcessContext, 'alcProcessContext');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcSuspendContext, 'alcSuspendContext');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcDestroyContext, 'alcDestroyContext');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetError, 'alcGetError');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetCurrentContext, 'alcGetCurrentContext');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcOpenDevice, 'alcOpenDevice');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcCloseDevice, 'alcCloseDevice');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcIsExtensionPresent, 'alcIsExtensionPresent');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetProcAddress, 'alcGetProcAddress');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetEnumValue, 'alcGetEnumValue');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetContextsDevice, 'alcGetContextsDevice');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetString, 'alcGetString');
    ALLibrary.SetSymbol({$ifndef FPC}@{$endif} alcGetIntegerv, 'alcGetIntegerv');

    { --------------------------------------------------------------------------
      ALC_xxx constants depending on Sample Implementation or not }
    if OpenALSampleImplementation then
    begin
      ALC_FREQUENCY := $100;
      ALC_REFRESH := $101;
      ALC_SYNC := $102;

      ALC_DEFAULT_DEVICE_SPECIFIER := $300;
      ALC_DEVICE_SPECIFIER := $301;
      ALC_EXTENSIONS := $302;

      ALC_MAJOR_VERSION := $303;
      ALC_MINOR_VERSION := $304;
      ALC_ATTRIBUTES_SIZE := $305;
      ALC_ALL_ATTRIBUTES := $306;
    end else
    begin
      ALC_MAJOR_VERSION := $1000;
      ALC_MINOR_VERSION := $1001;
      ALC_ATTRIBUTES_SIZE := $1002;
      ALC_ALL_ATTRIBUTES := $1003;

      ALC_DEFAULT_DEVICE_SPECIFIER := $1004;
      ALC_DEVICE_SPECIFIER := $1005;
      ALC_EXTENSIONS := $1006;

      ALC_FREQUENCY := $1007;
      ALC_REFRESH := $1008;
      ALC_SYNC := $1009;
    end;
  end else
    WritelnLog('OpenAL library not found');
end;

procedure OpenALFinalization;
begin
  FreeAndNil(ALLibrary);
end;

initialization
  { Below needs to be done for OpenAL to work.

    Note that the GL unit in FPC already does this,
    but it is still important for applications that don't use OpenGL
    (but use OpenAL), like
      examples/audio/simplest_play_sound/
      examples/audio/audio_player/

    Otherwise simplest_play_sound on Linux/x86_64 always exits with
    An unhandled exception occurred at $00007FC4771DF69B:
    EInvalidOp: Invalid floating point operation
      $00007FC4771DF69B
  }
  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$endif}

  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  OpenALInitialization;
  {$endif}
finalization
  OpenALFinalization;
end.
