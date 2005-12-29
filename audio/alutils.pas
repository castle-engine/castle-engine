{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's audio Pascal units".

  "Kambi's audio Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's audio Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's audio Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(A set of general utilities for working with OpenAL.)

  This unit provides many comfortable utilities to work with OpenAL.
  Everything is based on my OpenAL binding in unit @link(OpenAL).
  Most important things are BeginAL and EndAL
  procedures for initializing / releasing OpenAL context.
  Also TALSoundWAV class is useful to load WAV files into
  OpenAL buffers.

  This unit does @italic(not) use alut* functions from @link(OpenAL),
  so your programs will not depend on existence of alut*
  functions in some library. You can define symbol USE_ALUT
  and recompile this unit to use alut* functions, but this
  is really useful only for debugging. One can say that this
  unit provides a superset of alut functionality.

  When you're using this unit, you shouldn't use any alc*
  functions and alutInit/alutExit functions from @link(OpenAL).
  The rule is that this unit takes care about such things.
}

unit ALUtils;

{
  TODO: make sure docs look good in pasdoc
  TODO: translate docs to English
}

{ Define symbol USE_ALUT to make Begin/EndAL be implemented using
  alutInit/Exit. BeginAL will then always behave like CheckForAlut = true
  (so user must have proper alut library installed, or alut functions
  must be available in his openal library).
  BeginAL will also ignore ALCDevice (as it's impossible to pass this
  to alutInit). Everything else should work as it should.

  Also, without USE_ALUT the code will do appropriate calls to alc*
  functions and this means that eventual error messages will be more
  detailed and clear.

  Summary: there's actually no good reason to define USE_ALUT.
  Everything works better without USE_ALUT defined.
  So defining USE_ALUT may be useful only to test implementation of this
  unit (if Begin/EndAL doesn't work, you can define USE_ALUT
  and then check if it still fails).
}
{ $define USE_ALUT}

interface

{$define read_interface}

uses SysUtils, KambiUtils, OpenAL, Classes, SoundWAV, ParsingPars;

{ ---------------------------------------------------------------------------- }
{ @section(OpenAL device used by TryBeginAL and BeginAL) }

const
  { @name says what OpenAL device is generally the best.

    Theoretically always the default OpenAL device should be
    considered "the best", so BestALCDevice should be equal to ''.
    And that's indeed the current situation --- BestALCDevice
    is a constant equal to ''. But, in the past there were
    some reasons to set this to some other specific values for
    some OSes... And I want to be able in the future to do such
    tweaking again. }
  BestALCDevice = '';

var
  { This OpenAL device will be used when you will call TryBeginAL or BeginAL.

    I could make it instead a parameter for TryBeginAL / BeginAL procedures,
    but it's more comfortable to have a global variable for this ---
    it's useful e.g. to easily implement handling of @--audio-device option
    in OpenALOptionsParse. TryBeginAL / BeginAL procedures assume anyway
    that your program will require only one OpenAL context at any time. }
  ALCDevice: string = BestALCDevice;

{ This parses parameters in ParStr and interprets and removes
  recognized options. Internally it uses ParsePars with
  ParseOnlyKnownLongOptions = @true. Recognized options :

  @definitionList(
    @itemLabel @--audio-device DEVICE-NAME
    @item Set ALCDevice variable to given argument.

    @itemLabel @--print-audio-devices
    @item(
      Use ALC_ENUMERATION_EXT to print all available OpenAL audio devices
      to stdout. If this extension is not present, write something
      like "Enumerating audio devices not supported by your OpenAL"
      to stdout too.

      Then do ProgramBreak.)
  )

  More user-oriented documentation for the above options is here:
  [http://www.camelot.homedns.org/~michalis/openal_notes.php#section_options] }
procedure OpenALOptionsParse;

{ This is help string for options parsed by OpenALOptionsParse.

  Formatting is consistent with Kambi standards
  (see file @code(../base/README.kambi_command_line_params)).

  If PrintALCDeviceAsDefault then it will also say (near
  the help for option @--audio-device) that "defauls device is ..."
  and will give here current value of ALCDevice.
  This is usually useful, e.g. if you don't intend to modify directly
  ALCDevice (only indirectly via OpenALOptionsParse)
  then you should give here true. }
function OpenALOptionsHelp(PrintALCDeviceAsDefault: boolean): string;

{ ---------------------------------------------------------------------------- }
{ @section(BeginAL, EndAL and related things) }

var
  { @true means that you successfully called [Try]BeginAL and you didn't
    call EndAL after it. In other words, this means that you have
    active OpenAL context (and @link(OpenAL) functions are available,
    i.e. @link(ALInited) is true and maybe even @link(ALUTInited) = true,
    if you passed CheckForAlut = true to BeginAL). }
  ALActive: boolean = false;

  { When @link(BeginAL) fails with @link(EOpenALError) or when
    @link(TryBeginAL) returns with false, they put error message in this variable.
    When you're using @link(BeginAL) this variable is actually useless for you,
    as this is equal to Message property of raised exception.
    But this is of course useful when you're using @link(TryBeginAL),
    as this is the only way to get detailed info why @link(TryBeginAL) failed. }
  ALActivationErrorMessage: string = '';

  { Used by TryBeginAL and BeginAL, see there for description. }
  SetDefaultALState: boolean = true;

{ BeginAL creates and activates OpenAL context for the ALCDevice device.

  You should deactivate and free the context by calling EndAL.
  You should usually use @code(try .. finally .. end) construction for
  this, e.g.

  @longCode(# BeginAL; try ... finally EndAL; end; #)

  @raises(EOpenALError if something will go wrong)

  Detailed description of what BeginAL does:

  @orderedList(
    @item(
      Checks is ALInited = @true, if CheckForAlut then it also checks
      is ALUTInited = @true. If the test will fail, raises EOpenALInitError.)

    @item(
      Init's OpenAL context using appropriate alc* functions
      and using the device specified in ALCDevice variable.

      If this initialization will fail, ALActive will be set to @false,
      ALActivationErrorStr will be set to error message and
      EOpenALError (but not EOpenALInitError) will be raised
      (with Message equal to ALActivationErrorStr value).

      If this initialization will succeed, ALActive will be set to @true.)

    @item(
      If SetDefaultALState then we additionally make OpenAL context state
      more conformant to OpenAL specification.
      That's because Windows Creative Labs' implementation has
      a little different starting state than Linux implementation :
      @unorderedList(
        @item(DopplerVelocity is different than in the specification
          and Linux implementation. So we set it to value given in specification.)
        @item(DistanceModel is different than in the Linux implementation,
          specification does not specify this. So we set it to INVERSE_DISTANCE,
          like Linux implementation.)
      )
    )
  )
}
procedure BeginAL(CheckForAlut: boolean);

{ This is like BeginAL but it doesn't raise EOpenALError.

  So you should check ALActive
  (if @false, error message will be in ALActivationErrorMessage)
  to know whether initialization was successfull. }
function TryBeginAL(CheckForAlut: boolean): boolean;

{ This deactivates and frees the context initialized by last TryBeginAL
  or BeginAL call.

  If not ALActive, then this does nothing. }
procedure EndAL;

{ ---------------------------------------------------------------------------- }
{ @section(ALC querying) }

{$ifndef USE_ALUT}
{ Simple wrapper --- calls alcGetString with the device created
  by last TryBeginAL or BeginAL call (or nil if no
  TryBeginAL or BeginAL was called or the device was freed by
  EndAL). And returns normal Pascal AnsiString (so you don't have
  to worry anymore that alcGetString returns a pointer to some
  internal data in OpenAL library and you can't modify it). }
function GetALCString(enum: TALCenum): string;

{ Calls GetALCString and additionally checks alcGetError.
  If alcGetError returns error, it returns string looking like
  '(detailed error description)' (normal incorrect call to
  alcGetError would return just nil).

  Just like all other functions that somehow check al[c]GetError,
  this function assumes that error state was "clear" before
  calling this function, i.e. al[c]GetError would return AL[C]_NO_ERROR. }
function GetALCStringTrapped(enum: TALCenum): string;
{$endif not USE_ALUT}

{ ---------------------------------------------------------------------------- }
{ @section(Error checking) }

type
  { This is for errors reported by alGetError (using constants AL_xxx) }
  EALError = class(EOpenALError)
  private
    FALErrorNum: TALenum;
  public
    property ALErrorNum: TALenum read FALErrorNum;
    constructor Create(AALErrorNum: TALenum; const AMessage: string);
  end;

{ @raises(EALError if alGetError returned something <> AL_NO_ERROR) }
procedure CheckAL(const situation: string);

{ ---------------------------------------------------------------------------- }
{ @section(TALSoundWAV) }

type
  { }
  TALSoundWAV = class(TSoundWAV)
    { ALDataformat = AL_FORMAT_MONO/STEREO8/16, based on DataFormat }
    function ALDataformat: TALenum;

    { load this wave to the buffer using alBufferData(buffer, ...) where
      "..." is taken from this object's properties. }
    procedure alBufferData(buffer: TALuint);

    class procedure alBufferDataFromFile(buffer: TALuint; const FileName: string);
    class function alCreateBufferDataFromFile(const FileName: string): TALuint;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Query AL state) }

{ @groupBegin

  Simple wrappers for alGetSource*, alGetBuffer*, alGetListener* and
  alGetFloat/Integer/Boolean*. In many cases these should be more comfortable
  (because they are functions) and safer (no need to pass some pointer)
  than directly using related OpenAL functions.

  OpenAL errors are @italic(not) checked by these functions
  (i.e. CheckAL or alGetError is not called).

  No checking does @code(Attribute) really return value of given type is done.
  This means that if you will request value of the wrong type for
  given @code(Attribute), OpenAL may do some convertion, or may set the error
  state. In some cases you may even get nasty access violation errors
  or accidental writes over some random place in memory --- this may
  happen if for given @code(Attribute) OpenAL likes to return an array
  of some values, and you will use the wrong version
  (e.g. using AL_GAIN with a version that returns TALVector3f,
  or using AL_POSITION with a version that returns single TALfloat).
  So @italic(always check carefully that given @code(Attribute)
  supports the requested output value.)
}
function alGetSource1i(SourceName: TALuint; Attribute: TALenum): TALint;
function alGetSource1f(SourceName: TALuint; Attribute: TALenum): TALfloat;
function alGetSource1bool(SourceName: TALuint; Attribute: TALenum): TALboolean;
function alGetSource1ui(SourceName: TALuint; Attribute: TALenum): TALuint;
function alGetSource3f(SourceName: TALuint; Attribute: TALenum): TALVector3f;

function alGetBuffer1sizei(BufferName: TALuint; Attribute: TALenum): TALsizei;
function alGetBuffer1i(BufferName: TALuint; Attribute: TALenum): TALint;
function alGetBuffer1f(BufferName: TALuint; Attribute: TALenum): TALfloat;

function alGetListener1f(Attribute: TALenum): TALfloat;
function alGetListener3f(Attribute: TALenum): TALVector3f;
function alGetListenerOrientation: TALTwoVectors3f;

{ @groupEnd }

{ ---------------------------------------------------------------------------- }
{ @section(Simple wrappers over OpenAL function to pass TALVector* types) }

{ @groupBegin

  These functions are simple wrappers over OpenAL functions
  to allow you to pass TALVector* / TALTwoVectors* types.

  Just like with alGet* wrappers (above in this unit),
  no error checking is done (no CheckAL etc.) and no
  checking does @code(Param) accept the given type of value is done.
}

procedure alSourceVector3f(SourceName: TALuint; Param: TALenum; const Value: TALVector3f);
procedure alListenerVector3f(Param: TALenum; const Value: TALVector3f);
procedure alListenerOrientation(const Dir, Up: TALVector3f); overload;
procedure alListenerOrientation(const Orient: TALTwoVectors3f); overload;

{ @groupEnd }

{ ---------------------------------------------------------------------------- }
{ @section(State setting for compatibility between various OpenAL implementations) }

{ @groupBegin

  Unfortunately current Creative OpenAL Win32 implementation violates
  OpenAL specifitation : default source state (i.e. newly generated
  source state) is not as it is specified by OpenAL implementation :
  attributes MAX_DISTANCE, DIRECTION and CONE_OUTER_GAIN have different
  values.

  So alCreateSources calls alGenSources and then makes sure that all sources
  have state consistent with OpenAL specification (under Win32 it means
  that it sets MAX_DISTANCE, DIRECTION and CONE_OUTER_GAIN attributes
  to their proper values). alCreateBuffers does the same for alGenBuffers
  (which means, @italic(for now), that it simply calls alGenBuffers.)

  (alCreateSources and alCreateBuffers may be extended at some time if
  I discover some other incompatibilities in alGenSources/Buffers,
  maybe in Win32 and maybe in Linux implementation (and maybe in others?)).

  So explaining it simply : always use alCreateSources and alCreateBuffers
  instead alGenSources and alGenBuffers. (warning : no additional CheckAL
  is called in alCreateSources and alCreateBuffers; their intent is to be
  a complete analogy to calling alGenSources and alGenBuffers). }

procedure alCreateSources(n: TALsizei; sources: PALuint);
procedure alCreateBuffers(n: TALsizei; buffers: PALuint);

{ @groupEnd }

{$undef read_interface}

implementation

uses VectorMath;

{$define read_implementation}

{ alc device choosing ------------------------------------------------------------ }

{$ifndef USE_ALUT}
procedure CheckALC(const situation: string); forward;
{$endif}

procedure OpenALOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var DefaultDeviceName: string;
    pDeviceList: PChar;
    DeviceList: TDynStringArray;
    i, DefaultDeviceNum: Integer;
begin
 case OptionNum of
  0: ALCDevice := Argument;
  1: begin
      if not ALInited then
       Writeln('OpenAL is not available - cannot print available audio devices') else
      if not alcIsExtensionPresent(nil, 'ALC_ENUMERATION_EXT') then
       Writeln('Your OpenAL implementation does not support getting the list '+
         'of available audio devices (ALC_ENUMERATION_EXT extension not present).') else
      begin
       DefaultDeviceName := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
       DefaultDeviceNum := -1;
       pDeviceList := alcGetString(nil, ALC_DEVICE_SPECIFIER);
       Assert(pDeviceList <> nil);

       DeviceList := TDynStringArray.Create;
       try
        { parse pDeviceList, evaluate DeviceList and DefaultDeviceNum }
        while pDeviceList^<>#0 do
        begin
         DeviceList.AppendItem(pDeviceList); { automatic conversion PChar -> AnsiString }
         if DeviceList[DeviceList.High] = DefaultDeviceName then
          DefaultDeviceNum := DeviceList.High;
         { advance position of pDeviceList }
         pDeviceList := StrEnd(pDeviceList);
         Inc(pDeviceList);
        end;

        Writeln(DeviceList.Count, ' available audio devices:');
        for i := 0 to DeviceList.Count-1 do
        begin
         Write('  ', DeviceList[i]);
         if i = DefaultDeviceNum then Write(' (default OpenAL device)');
         Writeln;
        end;

        if DefaultDeviceNum = -1 then
         Writeln('Default OpenAL device name is "', DefaultDeviceName, '" but this device '+
           'was not listed as available device. Bug in OpenAL ?');
       finally DeviceList.Free end;
      end;

      ProgramBreak;
     end;
  else raise EInternalError.Create('OpenALOptionProc');
 end;
end;

procedure OpenALOptionsParse;
const
  OpenALOptions: array[0..1] of TOption =
  ( (Short:#0; Long:'audio-device'; Argument: oaRequired),
    (Short:#0; Long:'print-audio-devices'; Argument: oaNone)
  );
begin
 ParsePars(OpenALOptions, OpenALOptionProc, nil, true);
end;

function OpenALOptionsHelp(PrintALCDeviceAsDefault: boolean): string;
begin
 result:=
   '  --audio-device DEVICE-NAME' +nl+
   '                        Choose specific OpenAL audio device';
 if PrintALCDeviceAsDefault then
  result += nl+
    '                        Default audio device for this OS is:' +nl+
    '                        '+ Iff(ALCDevice = '', '(OpenAL default device)', ALCDevice);
 result += nl+
    '  --print-audio-devices' +nl+
    '                        Print available audio devices' +nl+
    '                        (not supported by every OpenAL implementation)';
end;

{ implementation internal alc things  --------------------------------- }

{$ifndef USE_ALUT}
var
  audio_device: PALCdevice;
  audio_context: PALCcontext;

{ dla bledow zglosznych przez alcGetError, stale ALC_xxx }
type
  EALCError = class(EOpenALError)
  private
    FALCErrorNum: TALenum;
  public
    property ALCErrorNum: TALenum read FALCErrorNum;
    constructor Create(AALCErrorNum: TALenum; const AMessage: string);
  end;

constructor EALCError.Create(AALCErrorNum: TALenum; const AMessage: string);
begin
 FALCErrorNum := AALCErrorNum;
 inherited Create(AMessage);
end;

procedure CheckALC(const situation: string);
{ wymaga do dzialania valid audio_device ! }
var err: TALenum;
    alcErrDescription: PChar;
    alcErrDescriptionStr: string;
begin
 err := alcGetError(audio_device);
 if err <> ALC_NO_ERROR then
 begin
  { moznaby tu uproscic zapis eliminujac zmienne alcErrDescription i alcErrDescriptionStr
    i zamiast alcErrDescriptionStr uzyc po prostu alcGetString(audio_device, err).
    Jedynym powodem dla ktorego jednak wprowadzam tu ta mala komplikacje jest fakt
    ze sytuacja ze alcGetError zwroci cos niespodziewanego (bledny kod bledu) niestety
    zdarza sie (implementacja Creative pod win32 nie jest doskonala...).
    W zwiazku z tym chcemy sie nia zajac. }
  alcErrDescription := alcGetString(audio_device, err);
  if alcErrDescription = nil then
   alcErrDescriptionStr := Format('(alc does not recognize this error number : %d)', [err]) else
   alcErrDescriptionStr := alcErrDescription;

  raise EALCError.Create(err,
    'OpenAL error ALC_xxx at '+situation+' : '+alcErrDescriptionStr);
 end;
end;
{$endif}

{ begin/end OpenAL ---------------------------------------------------- }

procedure BeginAL(CheckForAlut: boolean);

  procedure DoSetDefaultALState;
  begin
   {$ifdef WIN32}
   alDopplerVelocity(1.0);
   alDistanceModel(AL_INVERSE_DISTANCE);
   {$endif}
  end;

{$ifdef USE_ALUT}
var argc: Integer;
{$endif}
begin
 {
   Notka (ale jeszcze nie wiem czy ma ona znaczenie) : patrzylem na implementacje
   alutInit/Exit i one nie robia alcProcessContext/alcSuspendContext.

   Notki do nazw devices valid dla alcOpenDevice (oczywiscie w nie-testowych
     wersjach zawsze najlepiej uzywac nil zamiast podawac device name,
     wiec ponizszych rzeczy uzywam tylko dla celow testowania i zabawy OpenALem):
   - Pod implementacja Creative Labs OpenAL 1.0 pod Win32 nastepujace
     nazwy devices sa valid :
       DirectSound3D (rownowazne nil)
       DirectSound
       MMSYSTEM (kazda inna nazwa zostanie po cichu zinterpretowana jako MMSYSTEM)
     (podanie DirectSound3D spowoduje ze sprobuje zainicjowac DirectSound3D.
        Jesli mu sie nie uda, sprobuje zainicjowac DirectSound. Jesli i to
        sie nie uda, sprobuje zainicjowac MMSYSTEM.
      podanie DirectSound powoduje ze sprobuje zainicjowac DirectSound, jesli
        mu sie nie uda - sprobuje MMSYSTEM.
      podanie MMSYSTEM wreszcie powoduje ze sprobuje zainicjowac tylko
        MMSYSTEM.
      podanie nil jest rownowazne podaniu DirectSound3D, co oznacza ze autorom
        implementacji Creative wydawalo sie ze jest to najbardziej funkcjonalny
        device. (no i w razie klopotow z DirectSound3D beda probowane wszystkie
        pozostale devices po kolei)
     ).
 }

 try
  ALActive := false;

  {$ifdef USE_ALUT}
  CheckALUTInited;
  argc := 0;
  alutInit(argc, nil);
  if alcGetCurrentContext() = nil then
   raise EOpenALError.Create('OpenAL could not be initialized');

  {$else}
  if CheckForAlut then
   CheckALUTInited else
   CheckALInited;

  audio_device := alcOpenDevice(PCharOrNil(ALCDevice));
  if (audio_device = nil) then
   raise EOpenALError.Create('OpenAL''s audio device is not available');

  audio_context := alcCreateContext(audio_device, nil);
  CheckALC('initing OpenAL (alcCreateContext)');
  alcMakeContextCurrent(audio_context);
  CheckALC('initing OpenAL (alcMakeContextCurrent)');

  { Is alcProcessContext really needed ? alutExit doesn't do this. }
  alcProcessContext(audio_context);
  CheckALC('initing OpenAL (alcProcessContext)');
  {$endif USE_ALUT}

  ALActive := true;

  if SetDefaultALState then DoSetDefaultALState;
 except
  on E: EOpenALError do
  begin
   ALActivationErrorMessage := E.Message;
   raise;
  end;
 end;
end;

function TryBeginAL(CheckForAlut: boolean): boolean;
begin
 try
  BeginAL(CheckForAlut);
 except
  on E: EOpenALError do ;
 end;
 result := ALActive;
end;

procedure EndAL;
begin
 if not ALActive then Exit;

 {$ifdef USE_ALUT}
 alutExit;

 {$else}
 if audio_context <> nil then
 begin
  { Is alcSuspendContext really needed ? alutExit doesn't do this. }
  alcSuspendContext(audio_context);

  { Is alcMakeContextCurrent(nil) really needed ? alutExit doesn't do this.
    After testing: No, this line is bad,
    it makes Linux lets_take_a_walk hang on exit.
  alcMakeContextCurrent(nil); }

  alcDestroyContext(audio_context);
  audio_context := nil;
  CheckALC('closing OpenAL context');
 end;

 if audio_device <> nil then
 begin
  alcCloseDevice(audio_device);
  { w/g specyfikacji OpenAL generuje teraz error ALC_INVALID_DEVICE jesli
    device bylo nieprawidlowe; ale niby jak mam sprawdzic ten blad ?
    Przeciez zeby sprawdzic alcGetError potrzebuje miec valid device w reku,
    a po wywolaniu alcCloseDevice(device) device jest invalid (bez wzgledu
    na czy przed wywolaniem alcCloseDevice bylo valid) }
  audio_device := nil;
 end;
 {$endif}
end;

{ alc querying ------------------------------------------------------------ }

{$ifndef USE_ALUT}
function GetALCString(enum: TALCenum): string;
begin
 result := alcGetString(audio_device, enum);
end;

function GetALCStringTrapped(enum: TALCenum): string;
begin
 result := GetALCString(enum);
 try
  CheckALC('alcGetString');
 except
  on E: EALCError do result := '('+E.Message+')';
 end;
end;
{$endif USE_ALUT}

{ error checking ------------------------------------------------------- }

constructor EALError.Create(AALErrorNum: TALenum; const AMessage: string);
begin
 FALErrorNum := AALErrorNum;
 inherited Create(AMessage);
end;

procedure CheckAL(const situation: string);
var err: TALenum;
begin
 err := alGetError;
 if err <> AL_NO_ERROR then raise EALError.Create(err,
   'OpenAL error AL_xxx at '+situation+' : '+alGetString(err));
end;

{ TALSoundWAV ------------------------------------------------------------ }

function TALSoundWAV.ALDataFormat: TALenum;
const
  T: array[TSoundDataFormat]of TALenum =
  (AL_FORMAT_MONO8, AL_FORMAT_MONO16, AL_FORMAT_STEREO8, AL_FORMAT_STEREO16);
begin
 result := T[DataFormat];
end;

procedure TALSoundWAV.alBufferData(buffer: TALuint);
begin
 OpenAL.alBufferData(buffer, ALDataFormat, Data, DataSize, Frequency);
end;

class procedure TALSoundWAV.alBufferDataFromFile(buffer: TALuint; const FileName: string);
var wav: TALSoundWAV;
begin
 wav := TALSoundWAV.CreateFromFile(FileName);
 try
  wav.alBufferData(buffer);
 finally wav.Free end;
end;

class function TALSoundWAV.alCreateBufferDataFromFile(const FileName: string): TALuint;
begin
 alCreateBuffers(1, @result);
 try
  alBufferDataFromFile(result, FileName);
 except alDeleteBuffers(1, @result); raise end;
end;

{ query al state -------------------------------------------------------------- }

{ glGetSource }

function alGetSource1i(SourceName: TALuint; Attribute: TALenum): TALint;
begin
 alGetSourcei(SourceName, Attribute, @result);
end;

function alGetSource1f(SourceName: TALuint; Attribute: TALenum): TALfloat;
begin
 alGetSourcef(SourceName, Attribute, @result);
end;

function alGetSource1bool(SourceName: TALuint; Attribute: TALenum): TALboolean;
begin
 result := alGetSource1i(SourceName, Attribute) <> 0;
end;

function alGetSource1ui(SourceName: TALuint; Attribute: TALenum): TALuint;
begin
 { niestety, nie ma normalnej metody na pobranie TALuint. Mozna pobrac tylko
   int (signed). Zgaduje ze nalezy wiec pobrac po prostu wartosc uzywajac
   takiej samej procedury jakbysmy chcieli pobrac int (signed) i olac
   wszelkie checki na zakres. W przypadku ponizej Pascal nie wykona zadnych
   checkow bo przekazujemy ponizej tylko pointer. }
 Assert(SizeOf(TALint) = SizeOf(TALuint));
 alGetSourcei(SourceName, Attribute, @result);
end;

function alGetSource3f(SourceName: TALuint; Attribute: TALenum): TALVector3f;
begin
 alGetSourcefv(SourceName, Attribute, @result);
end;

{ glGetBuffer }

function alGetBuffer1sizei(BufferName: TALuint; Attribute: TALenum): TALsizei;
begin
 { nie ma mozliwosci zapytania GetBuffer o TALsizei. Wiec robimy podobnie jak
   alGetSource1ui : jako PALint podajemy wskaznik na TALsizei. }
 Assert(SizeOf(TALsizei) = SizeOf(TALint));
 alGetBufferi(BufferName, Attribute, @result);
end;

function alGetBuffer1i(BufferName: TALuint; Attribute: TALenum): TALint;
begin
 alGetBufferi(BufferName, Attribute, @result);
end;

function alGetBuffer1f(BufferName: TALuint; Attribute: TALenum): TALfloat;
begin
 alGetBufferf(BufferName, Attribute, @result);
end;

{ alGetListener }

function alGetListener1f(Attribute: TALenum): TALfloat;
begin
 alGetListenerf(Attribute, @result);
end;

function alGetListener3f(Attribute: TALenum): TALVector3f;
begin
 alGetListenerfv(Attribute, @result);
end;

function alGetListenerOrientation: TALTwoVectors3f;
begin
 alGetListenerfv(AL_ORIENTATION, @result);
end;

{ opakowania na funkcje OpenALa aby dac parametry typu TALVector ------------ }

procedure alSourceVector3f(SourceName: TALuint; Param: TALenum; const Value: TALVector3f);
begin
 alSourcefv(SourceName, Param, @Value);
end;

procedure alListenerVector3f(Param: TALenum; const Value: TALVector3f);
begin
 alListenerfv(Param, @Value);
end;

procedure alListenerOrientation(const Dir, Up: TALVector3f);
var Orient: TALTwoVectors3f;
begin
 Orient[0] := Dir;
 Orient[1] := Up;
 alListenerfv(AL_ORIENTATION, @Orient);
end;

procedure alListenerOrientation(const Orient: TALTwoVectors3f);
begin
 alListenerfv(AL_ORIENTATION, @Orient);
end;

{ --------------------------------------------------------------------------
  state setting for compatibility between various OpenAL implementations }

procedure alCreateSources(n: TALsizei; sources: PALuint);
{$ifdef WIN32}
var i: Integer;
{$endif}
begin
 alGenSources(n, sources);

 {$ifdef WIN32}
 for i := 1 to n do
 begin
//sorry  alSourcei(sources^, AL_MAX_DISTANCE, );
  alSourceVector3f(sources^, AL_DIRECTION, ZeroVector3Single);
  alSourcef(sources^, AL_CONE_OUTER_GAIN, 0);
  Inc(sources);
 end;
 {$endif}
end;

procedure alCreateBuffers(n: TALsizei; buffers: PALuint);
begin
 alGenBuffers(n, buffers);
end;

end.
