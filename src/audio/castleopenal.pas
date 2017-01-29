{
  Copyright 2003-2017 Michalis Kamburelis.

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
  we will merely set global ALInited variable to @false.
  This allows you to check at runtime and gracefully work
  if OpenAL is not installed (e.g. you can turn off game sounds.)

  Renamed to CastleOpenAL (from OpenAL) to avoid clash with FPC's OpenAL
  unit. FPC unit doesn't provide above ALInited feature, and it doesn't
  integrate with CastleVectors.

  @exclude (Unit not really ready for PasDoc, with many comments from
  original C headers.) }
unit CastleOpenAL;

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

uses SysUtils, CastleVectors, CTypes;

{$ifdef FREEBSD}
  {$linklib pthread}
{$endif}

{ Macro below must be named "extdecl", to workaround Lazarus code tools
  known bug http://bugs.freepascal.org/view.php?id=7431 }
{$define extdecl := cdecl}

{$ifndef PASDOC}
{$I openal_al.inc}
{$I openal_alc.inc}
{$endif}

{ ----------------------------------------------------------------------------
  Types to make this unit cooperate nicely with CastleVectors.

  Definitions of types below are connected with definitions in openal_altypes.inc
  --- if you change some type in openal_altypes.inc you have to adjust definitions
  below as well. That's because we use constructions like
    TALVector3f = TVector3Single
  instead of
    TALVector3f = array [0..2] of TALSingle
  This way we can use all TVector3Single functions from CastleVectors
  to operate on OpenAL vectors.
}

type
  { }
  TALVector3f = TVector3Single;
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
function ALInited: boolean;

{ Reset OpenAL library.

  In this unit's initialization, we link to OpenAL library, load all symbols,
  and initialize ALInited. In this unit's finalization,
  we release library handles and set ALInited back to @false.

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

  Use this only when ALInited, since this is just a call to some alcGetString. }
function OpenALSampleImplementation: boolean;

procedure OpenALInitialization;

implementation

uses CastleUtils, CastleDynLib;

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

function ALInited: boolean;
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
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alEnable) := ALLibrary.Symbol('alEnable');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDisable) := ALLibrary.Symbol('alDisable');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alIsEnabled) := ALLibrary.Symbol('alIsEnabled');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBooleanv) := ALLibrary.Symbol('alGetBooleanv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetIntegerv) := ALLibrary.Symbol('alGetIntegerv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetFloatv) := ALLibrary.Symbol('alGetFloatv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetDoublev) := ALLibrary.Symbol('alGetDoublev');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetString) := ALLibrary.Symbol('alGetString');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBoolean) := ALLibrary.Symbol('alGetBoolean');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetInteger) := ALLibrary.Symbol('alGetInteger');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetFloat) := ALLibrary.Symbol('alGetFloat');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetDouble) := ALLibrary.Symbol('alGetDouble');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetError) := ALLibrary.Symbol('alGetError');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alIsExtensionPresent) := ALLibrary.Symbol('alIsExtensionPresent');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetProcAddress) := ALLibrary.Symbol('alGetProcAddress');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetEnumValue) := ALLibrary.Symbol('alGetEnumValue');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alListenerf) := ALLibrary.Symbol('alListenerf');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alListeneri) := ALLibrary.Symbol('alListeneri');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alListener3f) := ALLibrary.Symbol('alListener3f');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alListenerfv) := ALLibrary.Symbol('alListenerfv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListeneri) := ALLibrary.Symbol('alGetListeneri');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListenerf) := ALLibrary.Symbol('alGetListenerf');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListenerfv) := ALLibrary.Symbol('alGetListenerfv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListener3f) := ALLibrary.Symbol('alGetListener3f');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGenSources) := ALLibrary.Symbol('alGenSources');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDeleteSources) := ALLibrary.Symbol('alDeleteSources');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alIsSource) := ALLibrary.Symbol('alIsSource');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcei) := ALLibrary.Symbol('alSourcei');

    { alSource3i possibily not present in older < 1.1 OpenAL implementations,
      in particular in Loki versions. Handle gracefully: set to @nil, allowing
      programs that don't use alSource3i to still work. }
    try
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSource3i) := ALLibrary.Symbol('alSource3i');
    except
      on EDynLibError do alSource3i := nil;
    end;

    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcef) := ALLibrary.Symbol('alSourcef');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSource3f) := ALLibrary.Symbol('alSource3f');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcefv) := ALLibrary.Symbol('alSourcefv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSourcei) := ALLibrary.Symbol('alGetSourcei');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSourcef) := ALLibrary.Symbol('alGetSourcef');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSourcefv) := ALLibrary.Symbol('alGetSourcefv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcePlayv) := ALLibrary.Symbol('alSourcePlayv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceStopv) := ALLibrary.Symbol('alSourceStopv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceRewindv) := ALLibrary.Symbol('alSourceRewindv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcePausev) := ALLibrary.Symbol('alSourcePausev');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcePlay) := ALLibrary.Symbol('alSourcePlay');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourcePause) := ALLibrary.Symbol('alSourcePause');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceRewind) := ALLibrary.Symbol('alSourceRewind');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceStop) := ALLibrary.Symbol('alSourceStop');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGenBuffers) := ALLibrary.Symbol('alGenBuffers');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDeleteBuffers) := ALLibrary.Symbol('alDeleteBuffers');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alIsBuffer) := ALLibrary.Symbol('alIsBuffer');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alBufferData) := ALLibrary.Symbol('alBufferData');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferi) := ALLibrary.Symbol('alGetBufferi');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferf) := ALLibrary.Symbol('alGetBufferf');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceQueueBuffers) := ALLibrary.Symbol('alSourceQueueBuffers');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alSourceUnqueueBuffers) := ALLibrary.Symbol('alSourceUnqueueBuffers');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDopplerFactor) := ALLibrary.Symbol('alDopplerFactor');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDopplerVelocity) := ALLibrary.Symbol('alDopplerVelocity');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alDistanceModel) := ALLibrary.Symbol('alDistanceModel');

    {$ifdef OPENAL_DEPRECATED}
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferiv) := ALLibrary.Symbol('alGetBufferiv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferfv) := ALLibrary.Symbol('alGetBufferfv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSourceiv) := ALLibrary.Symbol('alGetSourceiv');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListeneriv) := ALLibrary.Symbol('alGetListeneriv');
    {$endif OPENAL_DEPRECATED}

    { alcXxx functions ---------------------------------------- }
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcCreateContext) := ALLibrary.Symbol('alcCreateContext');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcMakeContextCurrent) := ALLibrary.Symbol('alcMakeContextCurrent');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcProcessContext) := ALLibrary.Symbol('alcProcessContext');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcSuspendContext) := ALLibrary.Symbol('alcSuspendContext');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcDestroyContext) := ALLibrary.Symbol('alcDestroyContext');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetError) := ALLibrary.Symbol('alcGetError');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetCurrentContext) := ALLibrary.Symbol('alcGetCurrentContext');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcOpenDevice) := ALLibrary.Symbol('alcOpenDevice');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcCloseDevice) := ALLibrary.Symbol('alcCloseDevice');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcIsExtensionPresent) := ALLibrary.Symbol('alcIsExtensionPresent');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetProcAddress) := ALLibrary.Symbol('alcGetProcAddress');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetEnumValue) := ALLibrary.Symbol('alcGetEnumValue');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetContextsDevice) := ALLibrary.Symbol('alcGetContextsDevice');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetString) := ALLibrary.Symbol('alcGetString');
    Pointer({$ifndef FPC_OBJFPC} @ {$endif} alcGetIntegerv) := ALLibrary.Symbol('alcGetIntegerv');

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
  end;
end;

procedure OpenALFinalization;
begin
  FreeAndNil(ALLibrary);
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  OpenALInitialization;
  {$endif}
finalization
  OpenALFinalization;
end.
