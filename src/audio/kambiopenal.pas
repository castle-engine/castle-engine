{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}


{ OpenAL library functions.
  This is a translation of OpenAL C headers: @code(AL/al.h, AL/alc.h, AL/alut.h)
  (and included headers: @code(AL/altypes.h, AL/alctypes.h)).

  Renamed to KambiOpenAL (from OpenAL) to avoid clash with FPC's OpenAL
  unit. In the future, we may drop to using FPC's OpenAL unit, for now
  this unit has at least one advantage over FPC's OpenAL unit: if OpenAL
  library is not present (so, dylib or dll), it will simply set
  ALInited to @false. While FPC's OpenAL unit links to OpenAL using "external"
  and will fail when library is not present at runtime.

  Ogolne strategie tlumaczenia :
  @unorderedList(
    @item(
      wszystkie typy tlumaczylem na typy o ustalonych rozmiarach,
      tzn. int na LongInt zamiast na Integer,
      unsigned int na LongWord zamiast na Cardinal.)

    @item(typy OpenAL'a maja literke T na poczatku, zrobilem tez dla
      kazdego typu wersje PAL<typ> wskazujaca na odpowiedni typ TAL<typ>)

    @item(zazwyczaj parametry funkcji typu PALubyte zamienialem na PChar
      (i typy zwracane przez funkcje jako PALubyte tak samo, np. alGetString
      zwraca PChar);
      to jest to samo dla komputera (wskaznik na cos rozmiaru bajta, zreszta
      tak naprawde - po prostu wskaznik), a podawanie PChar jest duzo latwiejsze
      (oczywiscie, o ile tylko rzeczywiscie chodzi o string; zamienialem
      PALubyte na PChar wlasnie wtedy kiedy wiedzialem ze zawsze _chodzi_
      o string).)

    @item(nie uzywalem h2pas; headery OpenAL'a sa ladnie i konsekwentnie napisane
      i uznalem ze lepiej sobie poradze uzywajac moich makr w EmacsLispie +
      mnostwo zamian na regexpach pisanych z palca + troche recznej "klepaniny".)

    @item(wszystkie komentarze z oryginalnych headerow zostawilem, gdzieniegdzie
      sa naprawde dobre i stanowia zalazek dokumentacji odpowiednich funkcji.
      Wszystkie komentarze dodane ode mnie zawieraja string "Kambi".)
  )

  Funkcje i procedury sa przetlumaczone na wskazniki na funkcje.
  W inicjalizacji tego modulu probujemy je zaladowac z odpowiednich DLLi
  ale _nie ma zadnych gwarancji ze to sie uda_. Kazdy program uzywajacy
  tego modulu musi sie jakos upewnic ze ALInited (przed uzyciem
  dowolnej funkcji alXxx i alcXxx), jesli chce uzywac takze alutXxx
  to musi tez sprawdzic ze ALUTInited. Np. gry ktore powinny
  probowac zainicjowac OpenAL'a ale jednoczesnie powinny moc dzialac jesli
  nie ma OpenAL'a dostepnego (no bo w koncu, jesli nie ma muzyki to trudno -
  - grac mozna i bez muzyki) musza sprawdzac przed kazdym wejsciem w kazdy
  kawalek kodu uzywajacy OpenAL'a czy ALInited (ew. czy ALUTInited).
  Z drugiej strony, programy ktore nie chca komplikowac sobie tak zycia
  i chca dzialac na zasadzie "jesli nie ma dostepnego OpenAL'a to nie bede
  dzialal" moga wywolac proste
    Check(ALInited, "OpenAL is not available. This program requires OpenAL.");
  (albo cos podobnego dla ALUTInited) i potem w dalszej czesci programu
  mozemy swobodnie uzywac OpenAL'owych funkcji. Takie dwie funkcje
  CheckALInited i CheckALUTInited juz sa zreszta gotowe w tym module.

  Wszystkie zmienne w interfejsie tego modulu sa zmiennymi tylko dlatego
  ze to upraszcza mi robote. Nigdy nie moga byc modyfikowane z zewnatrz
  tego modulu - dotyczy to zarowno wskaznikow na wszystkie funkcje/procedury
  jak i zmiennych AL/ALUTInited.
}

unit KambiOpenAL;

{ Zdefiniuj dla danej platformy czego oczekujemy od jej wersji OpenALa
  (o ile w ogole bedzie obecna w danym systemie) :

  Zdefiniuj OLD_OPENAL aby powiedziec ze OpenAL nie zawiera funkcji
  alHint, alGetListeneriv, alGetSourceiv, alGetBufferiv, alGetBufferfv i alQueuei.
  W tym momencie dotyczy to wersji ktora mam na Windowsie rozprowadzanej
  z Creative SDK. 2005-11-12: now it's also for Debian testing OpenAL.
  So I'll just define it always. }
{$define OLD_OPENAL}

{ Zdefiniuj ALUT_IN_AL_LIB aby powiedziec ze funkcje "alutXxx" moga byc dostepne
  i na dodatek sa w tej samej dynlib co zasadnicze funkcje OpenALa.
  W tym momencie jest to jedyny sposob aby uzyskac funkcje alutXxx ale byc
  moze kiedys pojawia sie tez inne sposoby (np. jezeli znajde sie kiedys
  w sytuacji ze na jakims systemie funkcje alutXxx sa w osobnym dllu;
  ale na razie nie znam takiego systemu). }
{$ifdef LINUX} {$define ALUT_IN_AL_LIB} {$endif}
{$ifdef FREEBSD} {$define ALUT_IN_AL_LIB} {$endif}

interface

uses SysUtils, VectorMath;

{$ifdef FREEBSD}
  {$linklib pthread}
{$endif}

{ Macro below must be named "extdecl", to workaround Lazarus code tools
  known bug http://bugs.freepascal.org/view.php?id=7431 }
{$define extdecl := cdecl}

{$ifndef PASDOC}
{$I openal_al.inc}
{$I openal_alc.inc}
{$I openal_alut.inc}
{$endif}

{ ---------------------------------------------------------------------------- }

{ @section(Make this unit cooperate nicely with VectorMath.)

  Definitions of types below are connected with definitions in OpenAL_altypes.inc
  --- if you change some type in OpenAL_altypes.inc you have to adjust definitions
  below as well. (that's because we use constructions like
    TALVector3f = TVector3Single
  instead of
    TALVector3f = array[0..2]of TALSingle.
  We @italic(depend) on the fact that first definition is actually equal to the
  second one. However, we use the first definition because then compiler
  knows that TALVector3f and TVector3Single are the same types and we are
  able to use TALVector3f everywhere where we can use TVector3Single).
}

type
  { }
  TALVector3f = TVector3Single;
  TALVector3d = TVector3Double;
  { TwoVectors type is useful for OpenAL's listener ORIENTATION property.
    @groupBegin }
  TALTwoVectors3f = array[0..1]of TALVector3f;
  TALTwoVectors3d = array[0..1]of TALVector3d;
  { @groupEnd }

{ ---------------------------------------------------------------------------- }

{ @section(Make possible checking of OpenAL availability at runtime
    (not at compile time)).

  This makes possible including sound support
  in your program as an @italic(option), not requirement (and this is usually
  nice thing to do), i.e. program that includes OpenAL unit @italic(can) work
  on systems that do not even have any OpenAL implementation (any OpenAL
  dynamic library installed).
}

var
  { ALInited means that functions alXxx and alcXxx are loaded, i.e. their
    pointers in this module are initialized from appropriate library.
    Jezeli not ALInited to zawsze wszystkie pointery na funkcje
    alXxx i alcXxx beda rowne nil. }
  ALInited: boolean = false;

  { ALUTInited means that functions alutXxx are loaded (their pointers
    are loaded from appropriate library) AND that ALInited
    is true. I.e. it is not posiible to have (ALUTInited = true and ALInited = false).
    This may look like a simplification of a real situation because _if_ ever
    (under some OS) alutXxx functions will be loaded from different library
    than alXxx functions than it _is_ theoretically possible to have alutXxx
    functions available but alXxx functions. But the truth is that alutXxx
    functions are treated by me as COMPLETELY UNUSABLE if alXxx functions
    are not available. So if ALInited = false, we will never even check
    for alutXxx functions - if ALInited = false we will always set ALUTInited
    to false too. }
  ALUTInited: boolean = false;

type
  EOpenALError = class(Exception);
  EOpenALInitError = class(EOpenALError);

{ Check is appropriate variable (ALInited, ALUTInited) @true,
  if not --- raise EOpenALInitError with appropriate message.
  Actually these trivial procedures are implemented only because
  I wanted to place inside standard error messages for missing OpenAL
  functionality.

  @raises EOpenALInitError If appropriate variable is @false. }
procedure CheckALInited;
procedure CheckALUTInited;

const
  OpenALDLL =
    {$ifdef UNIX}
      {$ifdef DARWIN}
        '/System/Library/Frameworks/OpenAL.framework/OpenAL'
      {$else}
        { I'm linking dynamically using dlopen/dlsym, so library name
          below can't be just 'libopenal.so', this is only avail
          in -dev package of openal. }
        'libopenal.so.0'
      {$endif}
    {$endif}
    {$ifdef MSWINDOWS} {TODO: fix for win64?} 'OpenAL32.dll' {$endif};
  OpenALDLLAlt = {$ifdef UNIX} 'libopenal.so.1' {$else} '' {$endif};

{ Reset OpenAL library.

  In this unit's initialization, we link to OpenAL library, load all symbols,
  and initialize ALInited and ALUTInited. In this unit's finalization,
  we release library handles and set ALInited and ALUTInited back to @false.

  What this procedure does ?
  It behaves like finalizing this unit (releasing OpenAL library handles),
  then sleeping for some short amount, and initializing this unit again
  (loading OpenAL library symbols).

  When is it needed ?

  Unix OpenAL implementation seems to have a problem.
  It's reproduceable in "The Castle" code:
  When I want to switch from one OpenAL device to the other,
  I would like to call EndAL (this releases OpenAL context and device),
  change my device name, and call BeginAL (this allocates
  OpenAL context and device). But this causes problems under Linux:
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
  device ? In any case, this short sleep is needed...), then do
  OpenAL library initialization again. And things work correctly. }
procedure OpenALRestart;

{ Detect OpenAL SI (Sample Implementation) from Loki.
  This was the most common implementation on most Unixes,
  until OpenAL-soft came.

  Use this only when ALInited, since this is just a call to some alcGetString. }
function OpenALSampleImplementation: boolean;

implementation

uses KambiUtils, KambiTimeUtils, KambiDynLib;

{ CheckAL*Inited ------------------------------------------------------------ }

procedure CheckALInited;
begin
 if not ALInited then
  raise EOpenALInitError.Create('OpenAL library is not available');
end;

procedure CheckALUTInited;
begin
 if not ALUTInited then
 begin
  if ALInited then
   raise EOpenALInitError.Create(
     'OpenAL library is available but without alutXxx functions') else
   raise EOpenALInitError.Create(
     'OpenAL library with alutXxx functions is required, '+
     'but not even base OpenAL library is available.');
 end;
end;

function OpenALSampleImplementation: boolean;
begin
  Result := alGetString(AL_VENDOR) = 'J. Valenzuela';
end;

{ unit init/fini ------------------------------------------------------------ }

procedure OpenALInitialization; forward;
procedure OpenALFinalization; forward;

procedure OpenALRestart;
begin
  OpenALFinalization;
  Delay(500);
  OpenALInitialization;
end;

var
  ALLibrary: TDynLib;

procedure OpenALInitialization;
begin
 { Just to be sure to start with a "clean" state. }
 OpenALFinalization;

 ALLibrary := TDynLib.Load(OpenALDLL, false);
 if (ALLibrary = nil) and (OpenALDLLAlt <> '') then
   ALLibrary := TDynLib.Load(OpenALDLLAlt, false);
 ALInited := ALLibrary <> nil;
 ALUTInited := false; { I know it is initialized in unit's interface... but just to be sure... }

 if ALInited then
 begin
  { alXxx functions ---------------------------------------- }
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
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSource3f) := ALLibrary.Symbol('alGetSource3f');
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

  {$ifndef OLD_OPENAL}
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alQueuei) := ALLibrary.Symbol('alQueuei');
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferiv) := ALLibrary.Symbol('alGetBufferiv');
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetBufferfv) := ALLibrary.Symbol('alGetBufferfv');
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetSourceiv) := ALLibrary.Symbol('alGetSourceiv');
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alGetListeneriv) := ALLibrary.Symbol('alGetListeneriv');
  Pointer({$ifndef FPC_OBJFPC} @ {$endif} alHint) := ALLibrary.Symbol('alHint');
  {$endif not OLD_OPENAL}

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

  ALUTInited := {$ifdef ALUT_IN_AL_LIB} true {$else} false {$endif};
  if ALUTInited then
  begin
    { alutXxx functions ---------------------------------------- }
    try
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutInit) := ALLibrary.Symbol('alutInit');
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutExit) := ALLibrary.Symbol('alutExit');
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutLoadWAV) := ALLibrary.Symbol('alutLoadWAV');
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutLoadWAVFile) := ALLibrary.Symbol('alutLoadWAVFile');
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutLoadWAVMemory) := ALLibrary.Symbol('alutLoadWAVMemory');
      Pointer({$ifndef FPC_OBJFPC} @ {$endif} alutUnloadWAV) := ALLibrary.Symbol('alutUnloadWAV');
    except
      on EDynLibError do
        { If loading of some alutXxx function will fail, then set
          ALUTInited to false and silence the exception. }
        ALUTInited := false;
    end;
  end;

  {$undef FPC_OBJFPC}
 end;
end;

procedure OpenALFinalization;
begin
 ALInited := false;
 ALUTInited := false;
 FreeAndNil(ALLibrary);
end;

initialization
  OpenALInitialization;
finalization
  OpenALFinalization;
end.
