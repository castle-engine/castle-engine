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

{ Various utilities for working with OpenAL.
  Everything is based on my OpenAL bindings in unit KambiOpenAL.
  Most important things here are BeginAL and EndAL
  procedures for initializing / releasing OpenAL context.
  Also TALSoundFile class is very useful to load files into
  OpenAL buffers.

  Notes about alut (OpenAL utlity library): we don't use it.
  (Although your own programs you can use it's functions,
  as KambiOpenAL exports them.) For testing, you can recompile
  this unit withsymbol USE_ALUT defined to actually use alut,
  but this doesn't provide any real benefit for normal use.

  When you're using this unit, you shouldn't use any alc*
  functions and alutInit/alutExit functions from KambiOpenAL.
  The rule is that this unit takes care about such things.
}

unit ALUtils;

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

uses SysUtils, KambiUtils, KambiOpenAL, Classes, SoundFile, ParseParametersUnit,
  Cameras;

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

{ ---------------------------------------------------------------------------- }
{ @section(OpenAL devices) }

const
  { The suggested best OpenAL device.

    Theoretically always the default OpenAL device should be
    considered "the best", so BestALCDevice should be equal to ''.
    And that's indeed the current situation --- BestALCDevice
    is a constant equal to ''. But, in the past there were
    some reasons to set this to some other specific values for
    some OSes... And I want to be able in the future to do such
    tweaking again. }
  BestALCDevice = '';

var
  { OpenAL device to be used when initializing OpenAL context.
    Used by TryBeginAL or BeginAL.

    I could make it instead a parameter for TryBeginAL / BeginAL procedures,
    but it's more comfortable to have a global variable for this ---
    it's useful e.g. to easily implement handling of @--audio-device option
    in OpenALOptionsParse. TryBeginAL / BeginAL procedures assume anyway
    that your program will require only one OpenAL context at any time. }
  ALCDevice: string = BestALCDevice;

{ Check and use OpenAL enumeration extension.
  If OpenAL supports ALC_ENUMERATION_EXT, then we return @true
  and pDeviceList is initialized to the null-separated list of
  possible OpenAL devices.

  @groupBegin }
function EnumerationExtPresent(out pDeviceList: PChar): boolean; overload;
function EnumerationExtPresent: boolean; overload;
{ @groupEnd }

{ Append to DevicesList a list of available OpenAL devices.
  OpenAL context must be already initialized when calling this
  (ALActive must be @true).

  It tries to use ALC_ENUMERATION_EXT extension, available on all modern
  OpenAL implementations. If it fails, and we're dealing with
  OpenAL "sample implementation" (older OpenAL Unix implementation)
  then we return a hardcoded list of devices known to be supported
  by this implementation.
  This makes it working sensibly under all OpenAL implementations in use
  today.

  If it fails (you have some really weird / old OpenAL implementation)
  it doesn't append anything to the DevicesList.

  Remember that for every OpenAL implementation, there is also an implicit
  OpenAL device named '' (empty string) supported. }
procedure GetOpenALDevices(DevicesList: TStringList);

{ Nice description of given OpenAL device.
  Currently this returns nice string for
  @unorderedList(
    @item(Empty string (means "Default OpenAL device"))
    @item(Various Unix devices looking like @code('(( devices '(DEVICE-NAME) ))))
  ) }
function ALCDeviceToNiceStr(const ALCDevice: string): string;

{ This parses parameters in @link(Parameters) and interprets and removes
  recognized options. Internally it uses ParseParameters with
  ParseOnlyKnownLongOptions = @true. Recognized options :

  @definitionList(
    @itemLabel @--audio-device DEVICE-NAME
    @item Set ALCDevice variable to given argument.

    @itemLabel @--print-audio-devices
    @item(
      Use ALC_ENUMERATION_EXT to print all available OpenAL audio devices
      to stdout (uses InfoWrite, so on Windows when program is GUI, it will
      make a dialog box).
      If this extension is not present, write something
      like "Enumerating audio devices not supported by your OpenAL".

      Then do ProgramBreak.)
  )

  More user-oriented documentation for the above options is here:
  [http://vrmlengine.sourceforge.net/openal_notes.php#section_options] }
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
  { Do we have active OpenAL context. This is @true when you successfully
    called TryBeginAL or BeginAL (and you didn't call EndAL yet).
    This also implies that OpenAL library is loaded, that is ALUTInited = @true. }
  ALActive: boolean = false;

  { Last error when initializing OpenAL context.
    When @link(BeginAL) fails with EOpenALError or when
    @link(TryBeginAL) returns with false, they put error message in this variable.

    When you're using @link(BeginAL) this variable is rather useless,
    as this is equal to Message property of raised exception.
    But when you're using @link(TryBeginAL),
    this is the only way to get detailed info why @link(TryBeginAL) failed. }
  ALActivationErrorMessage: string = '';

  EFXSupported: boolean = false;

{ Create and activate OpenAL context for the ALCDevice device.

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
      If the context will be successfully activated, we will also try to init
      EFX extensions for it by doing EFXSupported := Load_EFX(Device).
      If the context will not be activated for whatever reason,
      EFXSupported will be set to @false.)
  )
}
procedure BeginAL(CheckForAlut: boolean);

{ This is like BeginAL but it doesn't raise EOpenALError.

  So you should check ALActive
  (if @false, error message will be in ALActivationErrorMessage)
  to know whether initialization was successfull. }
function TryBeginAL(CheckForAlut: boolean): boolean;

{ This deactivates and frees the context initialized by last TryBeginAL
  or BeginAL call. ALActive is set to @false.

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
  alcGetString would return just nil).

  Actually, this @italic(also) checks normal al error (alGetError instead
  of alcGetError). Seems that when Darwin (Mac OS X) Apple's OpenAL
  implementation fails to return some alcGetString
  it reports this by setting AL error (instead of ALC one)
  to "invalid value". Although (after fixes to detect OpenALSampleImplementation
  at runtime and change constants values) this shouldn't happen anymore
  it you pass normal consts to this function.

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
{ @section(TALSoundFile) }

type
  { }
  TALSoundFile = class
  private
    FSoundFile: TSoundFile;
    FOwnsSoundFile: boolean;
  public
    { Creates TALSoundFile, with given SoundFile.
      If OwnsSoundFile then SoundFile will be freed on destruction
      of this object. }
    constructor Create(ASoundFile: TSoundFile; AOwnsSoundFile: boolean);
    destructor Destroy; override;

    property SoundFile: TSoundFile read FSoundFile;
    property OwnsSoundFile: boolean read FOwnsSoundFile;

    { Load this file to the buffer using alBufferData(buffer, ...) where
      "..." is taken from SoundFile's properties. }
    procedure alBufferData(buffer: TALuint);

    class procedure alBufferDataFromFile(buffer: TALuint; const FileName: string);
    class function alCreateBufferDataFromFile(const FileName: string): TALuint;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(Query AL state) }

{ Comfortable wrappers for alGet*.
  In many cases these should be more comfortable
  (because they are functions) and safer (no need to pass some pointer)
  than directly using related OpenAL functions.

  OpenAL errors are @italic(not) checked by these functions
  (i.e. CheckAL or alGetError is not called).

  We don't check does @code(Attribute) really return value of given type.
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

  @groupBegin
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

function alcGetInterger1(deviceHandle:PALCdevice; token:TALenum): TALint;

{ @groupEnd }

{ ---------------------------------------------------------------------------- }
{ @section(Simple wrappers over OpenAL function to pass TALVector* types) }

{ Comfortable wrappers over OpenAL functions that take vector types.
  These take TALVector* / TALTwoVectors* types.

  Just like with alGet* wrappers (above in this unit),
  no error checking is done (no CheckAL etc.) and no
  checking does @code(Param) accept the given type of value is done.

  @groupBegin }
procedure alSourceVector3f(SourceName: TALuint; Param: TALenum; const Value: TALVector3f);
procedure alListenerVector3f(Param: TALenum; const Value: TALVector3f);
procedure alListenerOrientation(const Dir, Up: TALVector3f); overload;
procedure alListenerOrientation(const Orient: TALTwoVectors3f); overload;
{ @groupEnd }

{ Set OpenAL listener position and orientation from currrent Camera properties. }
procedure alListenerFromCamera(Camera: TCamera);

{ ---------------------------------------------------------------------------- }
{ @section(State setting for compatibility between various OpenAL implementations) }

{ Allocate OpenAL sources and buffers, making sure their initial state
  conforms to specification.

  Unfortunately current Creative OpenAL Windows implementation violates
  OpenAL specifitation: default source state (i.e. newly generated
  source state) is not as it is specified by OpenAL implementation.
  Attributes MAX_DISTANCE, DIRECTION and CONE_OUTER_GAIN have different
  values.

  So alCreateSources calls alGenSources and then makes sure that all sources
  have state consistent with OpenAL specification (under Windows it means
  that it sets MAX_DISTANCE, DIRECTION and CONE_OUTER_GAIN attributes
  to their proper values). alCreateBuffers does the same for alGenBuffers
  (which means, @italic(for now), that it simply calls alGenBuffers.)

  To be on the safe side, you should always use
  alCreateSources and alCreateBuffers instead alGenSources and alGenBuffers.
  @groupBegin }
procedure alCreateSources(n: TALsizei; sources: PALuint);
procedure alCreateBuffers(n: TALsizei; buffers: PALuint);
{ @groupEnd }

{ @section(Other utils) --------------------------------------------------- }

{ }
function alSourcePlayingOrPaused(ALSource: TALuint): boolean;

const
  BoolToAL: array[boolean] of TALint = (AL_FALSE, AL_TRUE);

{ Pass resource to alDeleteSources or alDeleteBuffers,
  checking and setting it to zero.

  These are trivial wrappers over @code(alDeleteSources(1, @@Source)),
  @code(alDeleteBuffers(1, @@Buffer)). They first check if resource is non-zero,
  and after freeing set it to zero. This makes calling them many times
  (e.g. on already freed resources) harmless.

  alFreeSource also calls alSourceStop first, because we cannot free playing
  sources.

  @groupBegin }
procedure alFreeSource(var Source: TALuint);
procedure alFreeBuffer(var Buffer: TALuint);
{ @groupEnd }

{$undef read_interface}

implementation

uses VectorMath, KambiStringUtils, EFX;

{$define read_implementation}

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

{ alc device choosing ------------------------------------------------------------ }

{$ifndef USE_ALUT}
procedure CheckALC(const situation: string); forward;
{$endif}

function SampleImpALCDeviceName(
  const RealDeviceName: string): string;
begin
  Result := '''(( devices ''(' + RealDeviceName + ') ))';
end;

function EnumerationExtPresent(out pDeviceList: PChar): boolean;
begin
  Result := alcIsExtensionPresent(nil, 'ALC_ENUMERATION_EXT');
  if Result then
  begin
    pDeviceList := alcGetString(nil, ALC_DEVICE_SPECIFIER);
    Assert(pDeviceList <> nil);
  end;
end;

function EnumerationExtPresent: boolean;
begin
  Result := alcIsExtensionPresent(nil, 'ALC_ENUMERATION_EXT');
end;

procedure GetOpenALDevices(DevicesList: TStringList);
var
  pDeviceList: PChar;
begin
  if ALInited and EnumerationExtPresent(pDeviceList) then
  begin
    { parse pDeviceList }
    while pDeviceList^ <> #0 do
    begin
      { automatic conversion PChar -> AnsiString below }
      DevicesList.Append(pDeviceList);

      { advance position of pDeviceList }
      pDeviceList := StrEnd(pDeviceList);
      Inc(pDeviceList);
    end;
  end else
  if ALInited and OpenALSampleImplementation then
  begin
    DevicesList.Append(SampleImpALCDeviceName('native'));
    DevicesList.Append(SampleImpALCDeviceName('sdl'));

    { aRts device is too unstable on my Linux:

      When trying to initialize <tt>arts</tt> backend
      I can bring the OpenAL library (and, consequently, whole program
      using it) to crash with message <i>can't create mcop
      directory</i>. Right after running konqueror, I get also
      crash with message <i>*** glibc detected *** double free or corruption (out):
      0x08538d88 ***</i>.

      This is so unstable, that I think that I do a service
      for users by *not* listing aRts in available OpenAL
      devices. It's listed on [http://vrmlengine.sourceforge.net/openal_notes.php]
      and that's enough.

    DevicesList.Append(UnixALCDeviceName('arts'));
    }

    DevicesList.Append(SampleImpALCDeviceName('esd'));
    DevicesList.Append(SampleImpALCDeviceName('alsa'));
    DevicesList.Append(SampleImpALCDeviceName('waveout'));
    DevicesList.Append(SampleImpALCDeviceName('null'));
  end;
end;

procedure OpenALOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Message, DefaultDeviceName: string;
  DeviceList: TStringList;
  i, DefaultDeviceNum: Integer;
begin
 case OptionNum of
  0: ALCDevice := Argument;
  1: begin
      if not ALInited then
       Message := 'OpenAL is not available - cannot print available audio devices' else
      if not EnumerationExtPresent then
       Message := 'Your OpenAL implementation does not support getting the list '+
         'of available audio devices (ALC_ENUMERATION_EXT extension not present ' +
         '(or not implemented correctly in case of Apple version)).' else
      begin
       DefaultDeviceName := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
       DefaultDeviceNum := -1;

       DeviceList := TStringList.Create;
       try
        GetOpenALDevices(DeviceList);

        DefaultDeviceNum := DeviceList.IndexOf(DefaultDeviceName);

        Message := Format('%d available audio devices:', [DeviceList.Count]) + nl;
        for i := 0 to DeviceList.Count-1 do
        begin
         Message += '  ' + DeviceList[i];
         if i = DefaultDeviceNum then Message += ' (default OpenAL device)';
         Message += nl;
        end;

        if DefaultDeviceNum = -1 then
         Message += 'Default OpenAL device name is "' + DefaultDeviceName +
           '" but this device was not listed as available device. ' +
           'Bug in OpenAL ?';
       finally DeviceList.Free end;
      end;

      InfoWrite(Message);

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
 ParseParameters(OpenALOptions,
   {$ifdef FPC_OBJFPC} @ {$endif} OpenALOptionProc, nil, true);
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

function ALCDeviceToNiceStr(const ALCDevice: string): string;
begin
  if ALCDevice = '' then
    Result := 'Default OpenAL device' else
  if OpenALSampleImplementation then
  begin
    if ALCDevice = SampleImpALCDeviceName('native') then
      Result := 'Operating system native' else
    if ALCDevice = SampleImpALCDeviceName('sdl') then
      Result := 'SDL (Simple DirectMedia Layer)' else
    if ALCDevice = SampleImpALCDeviceName('arts') then
      Result := 'aRts (analog Real time synthesizer)' else
    if ALCDevice = SampleImpALCDeviceName('esd') then
      Result := 'Esound (Enlightened Sound Daemon)' else
    if ALCDevice = SampleImpALCDeviceName('alsa') then
      Result := 'ALSA (Advanced Linux Sound Architecture)' else
    if ALCDevice = SampleImpALCDeviceName('waveout') then
      Result := 'WAVE file output' else
    if ALCDevice = SampleImpALCDeviceName('null') then
      Result := 'Null device (no output)' else
      Result := ALCDevice;
  end else
    Result := ALCDevice;
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
    zdarza sie (implementacja Creative pod Windows nie jest doskonala...).
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
{$ifdef USE_ALUT}
var argc: Integer;
{$endif}
begin
 { We don't do alcProcessContext/alcSuspendContext, no need
   (spec says that context is initially in processing state). }

 try
  ALActive := false;
  EFXSupported := false;

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
   raise EOpenALError.CreateFmt(
     'OpenAL''s audio device "%s" is not available', [ALCDevice]);

  audio_context := alcCreateContext(audio_device, nil);
  CheckALC('initing OpenAL (alcCreateContext)');

  alcMakeContextCurrent(audio_context);
  CheckALC('initing OpenAL (alcMakeContextCurrent)');
  {$endif USE_ALUT}

  ALActive := true;
  EFXSupported := Load_EFX(audio_device);
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

 ALActive := false;
 EFXSupported := false;

 {$ifdef USE_ALUT}
 alutExit;

 {$else}
 { CheckALC first, in case some error is "hanging" not catched yet. }
 CheckALC('right before closing OpenAL context');

 if audio_context <> nil then
 begin
   (* The OpenAL specification says

      "The correct way to destroy a context is to first release
      it using alcMakeCurrent with a NULL context. Applications
      should not attempt to destroy a current context – doing so
      will not work and will result in an ALC_INVALID_OPERATION error."

      (See [http://openal.org/openal_webstf/specs/oal11spec_html/oal11spec6.html])

      However, sample implementation (used on most Unixes,
      before OpenAL soft came) can hang
      on alcMakeContextCurrent(nil) call. Actually, it doesn't hang,
      but it stops for a *very* long time (even a couple of minutes).
      This is a known problem, see
      [http://opensource.creative.com/pipermail/openal-devel/2005-March/002823.html]
      and
      [http://lists.berlios.de/pipermail/warzone-dev/2005-August/000441.html].

      Tremulous code workarounds it like

	if( Q_stricmp((const char* )qalGetString( AL_VENDOR ), "J. Valenzuela" ) ) {
		qalcMakeContextCurrent( NULL );
	}

      ... and this seems a good idea, we do it also here.
      Initially I wanted to do $ifdef UNIX, but checking for Sample implementation
      with alGetString(AL_VENDOR) is more elegant (i.e. affecting more precisely
      the problematic OpenAL implementations, e.g. allowing us to work
      correctly with OpenAL soft too). *)

  if not OpenALSampleImplementation then
    alcMakeContextCurrent(nil);

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
  CheckAL('alcGetString');
 except
  on E: EALCError do result := '('+E.Message+')';
  on E: EALError do result := '('+E.Message+')';
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
 err := alGetError();
 if err <> AL_NO_ERROR then raise EALError.Create(err,
   'OpenAL error AL_xxx at '+situation+' : '+alGetString(err));
end;

{ TALSoundFile ------------------------------------------------------------ }

constructor TALSoundFile.Create(ASoundFile: TSoundFile; AOwnsSoundFile: boolean);
begin
  inherited Create;
  FOwnsSoundFile := AOwnsSoundFile;
  FSoundFile := ASoundFile;
end;

destructor TALSoundFile.Destroy;
begin
  if OwnsSoundFile then
    FreeAndNil(FSoundFile);
end;

procedure TALSoundFile.alBufferData(buffer: TALuint);
begin
  SoundFile.PrepareOpenAL;
  KambiOpenAL.alBufferData(buffer, SoundFile.DataFormat, SoundFile.Data,
    SoundFile.DataSize, SoundFile.Frequency);
end;

class procedure TALSoundFile.alBufferDataFromFile(buffer: TALuint;
  const FileName: string);
var
  F: TSoundFile;
  FAL: TALSoundFile;
begin
  F := TSoundFile.CreateFromFile(FileName);
  try
    FAL := TALSoundFile.Create(F, false);
    try
      FAL.alBufferData(buffer);
    finally FAL.Free end;
  finally F.Free end;
end;

class function TALSoundFile.alCreateBufferDataFromFile(
  const FileName: string): TALuint;
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

function alcGetInterger1(deviceHandle:PALCdevice; token:TALenum): TALint;
begin
  alcGetIntegerv(deviceHandle, token, 1, @Result);
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

procedure alListenerFromCamera(Camera: TCamera);
var
  Pos: TVector3Single;
  { By using Orientation vector, instead of two separate Dir, Up we save
    some time. }
  Orientation: TALTwoVectors3f;
begin
  Camera.GetView(Pos, Orientation[0], Orientation[1]);
  alListenerVector3f(AL_POSITION, Pos);
  alListenerfv(AL_ORIENTATION, @Orientation);
end;

{ --------------------------------------------------------------------------
  state setting for compatibility between various OpenAL implementations }

procedure alCreateSources(n: TALsizei; sources: PALuint);
{$ifdef MSWINDOWS}
var i: Integer;
{$endif}
begin
 alGenSources(n, sources);

 {$ifdef MSWINDOWS}
 for i := 1 to n do
 begin
//TODO:  alSourcei(sources^, AL_MAX_DISTANCE, );
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

{ Other utils ---------------------------------------------------------------- }

function alSourcePlayingOrPaused(ALSource: TALuint): boolean;
var
  SourceState: TALuint;
begin
  SourceState := alGetSource1i(ALSource, AL_SOURCE_STATE);
  Result := (SourceState = AL_PLAYING) or (SourceState = AL_PAUSED);
end;

procedure alFreeSource(var Source: TALuint);
begin
  if Source <> 0 then
  begin
    alSourceStop(Source);
    alDeleteSources(1, @Source);
    Source := 0;
  end;
end;

procedure alFreeBuffer(var Buffer: TALuint);
begin
  if Buffer <> 0 then
  begin
    alDeleteBuffers(1, @Buffer);
    Buffer := 0;
  end;
end;

end.
