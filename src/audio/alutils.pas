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
  For higher-level class that takes care of initializing OpenAL
  and loading and playing sounds, see TALSoundEngine.

  You shouldn't use any alc* functions or alutInit/alutExit
  functions from KambiOpenAL yourself. This unit and ALSoundEngine take care
  about everything needed there.
}

unit ALUtils;

interface

{$define read_interface}

uses SysUtils, KambiUtils, KambiOpenAL, Classes, SoundFile, KambiTimeUtils;

type
  EOpenALError = class(Exception);
  EOpenALInitError = class(EOpenALError);

{ Check is appropriate variable (ALInited, ALUTInited) @true.
  Raises EOpenALInitError with appropriate message if not.

  @raises EOpenALInitError If appropriate variable is @false. }
procedure CheckALInited;
procedure CheckALUTInited;

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

    class procedure alBufferDataFromFile(buffer: TALuint; const FileName: string;
      out Duration: TKamTime);
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

{ ---------------------------------------------------------------------------- }
{ @section(State setting for compatibility between various OpenAL implementations) }

{ Allocate OpenAL sources and buffers, making sure their initial state
  conforms to specification.

  Unfortunately current Creative OpenAL Windows implementation violates
  OpenAL specification: default source state (i.e. newly generated
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

uses VectorMath, KambiStringUtils, KambiLog;

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
  const FileName: string; out Duration: TKamTime);
var
  F: TSoundFile;
  FAL: TALSoundFile;
begin
  F := TSoundFile.CreateFromFile(FileName);
  try
    FAL := TALSoundFile.Create(F, false);
    try
      FAL.alBufferData(buffer);

      { Write to log after PrepareOpenAL, as OggVorbis is potentially decoded
        there (and this way we get to know it's duration). }
      if Log then
        WritelnLog('Sound', Format('Loaded "%s": %s, %s, size: %d, frequency: %d, duration: %f',
          [ FileName, F.ClassName, ALDataFormatToStr(F.DataFormat),
            F.DataSize, F.Frequency, F.Duration ]));

      Duration := F.Duration;
    finally FAL.Free end;
  finally F.Free end;
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
