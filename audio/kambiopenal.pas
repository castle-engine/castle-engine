{
  Copyright 2003-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

{$define al_call := cdecl;}
{$define alc_call := cdecl;}
{$define alut_call := cdecl;}

{$I OpenAL_al.inc}
{$I OpenAL_alc.inc}
{$I OpenAL_alut.inc}

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
 ALInited := ALLibrary <> nil;
 ALUTInited := false; { I know it is initialized in unit's interface... but just to be sure... }

 if ALInited then
 begin
  {$ifdef FPC_OBJFPC}
    {$define ProcVarCast := Pointer}
  {$else}
    {$define ProcVarCast := @}
  {$endif}

  { alXxx functions ---------------------------------------- }
  ProcVarCast(alEnable) := ALLibrary.Symbol('alEnable');
  ProcVarCast(alDisable) := ALLibrary.Symbol('alDisable');
  ProcVarCast(alIsEnabled) := ALLibrary.Symbol('alIsEnabled');
  {$ifndef OLD_OPENAL} ProcVarCast(alHint) := ALLibrary.Symbol('alHint'); {$endif}
  ProcVarCast(alGetBooleanv) := ALLibrary.Symbol('alGetBooleanv');
  ProcVarCast(alGetIntegerv) := ALLibrary.Symbol('alGetIntegerv');
  ProcVarCast(alGetFloatv) := ALLibrary.Symbol('alGetFloatv');
  ProcVarCast(alGetDoublev) := ALLibrary.Symbol('alGetDoublev');
  ProcVarCast(alGetString) := ALLibrary.Symbol('alGetString');
  ProcVarCast(alGetBoolean) := ALLibrary.Symbol('alGetBoolean');
  ProcVarCast(alGetInteger) := ALLibrary.Symbol('alGetInteger');
  ProcVarCast(alGetFloat) := ALLibrary.Symbol('alGetFloat');
  ProcVarCast(alGetDouble) := ALLibrary.Symbol('alGetDouble');
  ProcVarCast(alGetError) := ALLibrary.Symbol('alGetError');
  ProcVarCast(alIsExtensionPresent) := ALLibrary.Symbol('alIsExtensionPresent');
  ProcVarCast(alGetProcAddress) := ALLibrary.Symbol('alGetProcAddress');
  ProcVarCast(alGetEnumValue) := ALLibrary.Symbol('alGetEnumValue');
  ProcVarCast(alListenerf) := ALLibrary.Symbol('alListenerf');
  ProcVarCast(alListeneri) := ALLibrary.Symbol('alListeneri');
  ProcVarCast(alListener3f) := ALLibrary.Symbol('alListener3f');
  ProcVarCast(alListenerfv) := ALLibrary.Symbol('alListenerfv');
  ProcVarCast(alGetListeneri) := ALLibrary.Symbol('alGetListeneri');
  ProcVarCast(alGetListenerf) := ALLibrary.Symbol('alGetListenerf');
  {$ifndef OLD_OPENAL} ProcVarCast(alGetListeneriv) := ALLibrary.Symbol('alGetListeneriv'); {$endif}
  ProcVarCast(alGetListenerfv) := ALLibrary.Symbol('alGetListenerfv');
  ProcVarCast(alGetListener3f) := ALLibrary.Symbol('alGetListener3f');
  ProcVarCast(alGenSources) := ALLibrary.Symbol('alGenSources');
  ProcVarCast(alDeleteSources) := ALLibrary.Symbol('alDeleteSources');
  ProcVarCast(alIsSource) := ALLibrary.Symbol('alIsSource');
  ProcVarCast(alSourcei) := ALLibrary.Symbol('alSourcei');
  ProcVarCast(alSourcef) := ALLibrary.Symbol('alSourcef');
  ProcVarCast(alSource3f) := ALLibrary.Symbol('alSource3f');
  ProcVarCast(alSourcefv) := ALLibrary.Symbol('alSourcefv');
  ProcVarCast(alGetSourcei) := ALLibrary.Symbol('alGetSourcei');
  {$ifndef OLD_OPENAL} ProcVarCast(alGetSourceiv) := ALLibrary.Symbol('alGetSourceiv'); {$endif}
  ProcVarCast(alGetSourcef) := ALLibrary.Symbol('alGetSourcef');
  ProcVarCast(alGetSourcefv) := ALLibrary.Symbol('alGetSourcefv');
  ProcVarCast(alGetSource3f) := ALLibrary.Symbol('alGetSource3f');
  ProcVarCast(alSourcePlayv) := ALLibrary.Symbol('alSourcePlayv');
  ProcVarCast(alSourceStopv) := ALLibrary.Symbol('alSourceStopv');
  ProcVarCast(alSourceRewindv) := ALLibrary.Symbol('alSourceRewindv');
  ProcVarCast(alSourcePausev) := ALLibrary.Symbol('alSourcePausev');
  ProcVarCast(alSourcePlay) := ALLibrary.Symbol('alSourcePlay');
  ProcVarCast(alSourcePause) := ALLibrary.Symbol('alSourcePause');
  ProcVarCast(alSourceRewind) := ALLibrary.Symbol('alSourceRewind');
  ProcVarCast(alSourceStop) := ALLibrary.Symbol('alSourceStop');
  ProcVarCast(alGenBuffers) := ALLibrary.Symbol('alGenBuffers');
  ProcVarCast(alDeleteBuffers) := ALLibrary.Symbol('alDeleteBuffers');
  ProcVarCast(alIsBuffer) := ALLibrary.Symbol('alIsBuffer');
  ProcVarCast(alBufferData) := ALLibrary.Symbol('alBufferData');
  ProcVarCast(alGetBufferi) := ALLibrary.Symbol('alGetBufferi');
  ProcVarCast(alGetBufferf) := ALLibrary.Symbol('alGetBufferf');
  {$ifndef OLD_OPENAL} ProcVarCast(alGetBufferiv) := ALLibrary.Symbol('alGetBufferiv'); {$endif}
  {$ifndef OLD_OPENAL} ProcVarCast(alGetBufferfv) := ALLibrary.Symbol('alGetBufferfv'); {$endif}
  ProcVarCast(alSourceQueueBuffers) := ALLibrary.Symbol('alSourceQueueBuffers');
  ProcVarCast(alSourceUnqueueBuffers) := ALLibrary.Symbol('alSourceUnqueueBuffers');
  {$ifndef OLD_OPENAL} ProcVarCast(alQueuei) := ALLibrary.Symbol('alQueuei'); {$endif}
  ProcVarCast(alDopplerFactor) := ALLibrary.Symbol('alDopplerFactor');
  ProcVarCast(alDopplerVelocity) := ALLibrary.Symbol('alDopplerVelocity');
  ProcVarCast(alDistanceModel) := ALLibrary.Symbol('alDistanceModel');

  { alcXxx functions ---------------------------------------- }
  ProcVarCast(alcCreateContext) := ALLibrary.Symbol('alcCreateContext');
  ProcVarCast(alcMakeContextCurrent) := ALLibrary.Symbol('alcMakeContextCurrent');
  ProcVarCast(alcProcessContext) := ALLibrary.Symbol('alcProcessContext');
  ProcVarCast(alcSuspendContext) := ALLibrary.Symbol('alcSuspendContext');
  ProcVarCast(alcDestroyContext) := ALLibrary.Symbol('alcDestroyContext');
  ProcVarCast(alcGetError) := ALLibrary.Symbol('alcGetError');
  ProcVarCast(alcGetCurrentContext) := ALLibrary.Symbol('alcGetCurrentContext');
  ProcVarCast(alcOpenDevice) := ALLibrary.Symbol('alcOpenDevice');
  ProcVarCast(alcCloseDevice) := ALLibrary.Symbol('alcCloseDevice');
  ProcVarCast(alcIsExtensionPresent) := ALLibrary.Symbol('alcIsExtensionPresent');
  ProcVarCast(alcGetProcAddress) := ALLibrary.Symbol('alcGetProcAddress');
  ProcVarCast(alcGetEnumValue) := ALLibrary.Symbol('alcGetEnumValue');
  ProcVarCast(alcGetContextsDevice) := ALLibrary.Symbol('alcGetContextsDevice');
  ProcVarCast(alcGetString) := ALLibrary.Symbol('alcGetString');
  ProcVarCast(alcGetIntegerv) := ALLibrary.Symbol('alcGetIntegerv');

  ALUTInited := {$ifdef ALUT_IN_AL_LIB} true {$else} false {$endif};
  if ALUTInited then
  begin
    { alutXxx functions ---------------------------------------- }
    try
      ProcVarCast(alutInit) := ALLibrary.Symbol('alutInit');
      ProcVarCast(alutExit) := ALLibrary.Symbol('alutExit');
      ProcVarCast(alutLoadWAV) := ALLibrary.Symbol('alutLoadWAV');
      ProcVarCast(alutLoadWAVFile) := ALLibrary.Symbol('alutLoadWAVFile');
      ProcVarCast(alutLoadWAVMemory) := ALLibrary.Symbol('alutLoadWAVMemory');
      ProcVarCast(alutUnloadWAV) := ALLibrary.Symbol('alutUnloadWAV');
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
