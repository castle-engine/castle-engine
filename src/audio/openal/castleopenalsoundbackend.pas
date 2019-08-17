{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Full-featured sound engine backend using OpenAL.
  Supports spatial (3D) sound, is cross-platform,
  and underlying OpenAL is free and open-source. }
unit CastleOpenALSoundBackend;

{$I castleconf.inc}

interface

{ Use this to set sound engine backend to OpenAL.

  Note that OpenAL is the default sound backend on most platforms,
  so you don't need to call this procedure in simple applications.
  You only need to call it if you want to switch at runtime between FMOD,
  OpenAL and other backends. }
procedure UseOpenALSoundBackend;

implementation

uses SysUtils, Classes, Math, StrUtils, Generics.Collections,
  CastleInternalOpenAL, CastleVectors, CastleTimeUtils, CastleXMLConfig,
  CastleClassUtils, CastleStringUtils, CastleInternalSoundFile,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine,
  CastleInternalALUtils, CastleInternalEFX, CastleLog, CastleUtils;

{ sound backend classes interface -------------------------------------------- }

type
  {$ifdef CASTLE_SUPPORTS_THREADING}
  TOpenALStreamFeedThread = class;
  {$endif}
  TOpenALSoundSourceBackend = class;
  TOpenALStreamBufferBackend = class;

  TOpenALSoundBufferBackend = class(TSoundBufferBackendFromSoundFile)
  private
    ALBuffer: TALuint;
    function ALVersion11: Boolean;
  public
    procedure ContextOpenFromSoundFile(const SoundFile: TSoundFile); override;
    procedure ContextClose; override;
  end;

  { Manages resources (OpenAL buffers, thread) to play the given
    sound buffer on a given sound source using streaming. }
  TOpenALStreaming = class
  private
    const
      StreamBuffersCount = 4;
    var
      Source: TOpenALSoundSourceBackend;
      Buffer: TOpenALStreamBufferBackend;
      ALBuffers: array [0..StreamBuffersCount - 1] of TALuint;
      StreamedFile: TStreamedSoundFile;
      {$ifdef CASTLE_SUPPORTS_THREADING}
      FeedThread: TOpenALStreamFeedThread;
      {$endif}

    function FillBuffer(const ALBuffer: TALuint): Integer;
  public
    constructor Create(const ASoundSurce: TOpenALSoundSourceBackend;
      const StreamBuffer: TOpenALStreamBufferBackend);
    destructor Destroy; override;
    procedure FeedBuffers;
  end;

  TOpenALStreamBufferBackend = class(TSoundBufferBackendFromStreamedFile)
  end;

  TOpenALStreamFeedThread = class(TThread)
  private
    Streaming: TOpenALStreaming;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreateSuspended: Boolean;
      const AStreaming: TOpenALStreaming); reintroduce;
  end;

  TOpenALSoundSourceBackend = class(TSoundSourceBackend)
  strict private
    Streaming: TOpenALStreaming;
    function ALVersion11: Boolean;
  private
    FBuffer: TSoundBufferBackend;
    ALSource: TALuint;
    FLooping: Boolean;

    { When buffer is stremed, OpenAL source looping need to be off,
      otherwise, one buffer will be looped. This procedure cares about that. }
    procedure AdjustALLooping;
  public
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: boolean; override;
    procedure Play(const BufferChangedRecently: Boolean); override;
    procedure Stop; override;
    procedure SetPosition(const Value: TVector3); override;
    procedure SetVelocity(const Value: TVector3); override;
    procedure SetLooping(const Value: boolean); override;
    procedure SetRelative(const Value: boolean); override;
    procedure SetGain(const Value: Single); override;
    procedure SetMinGain(const Value: Single); override;
    procedure SetMaxGain(const Value: Single); override;
    procedure SetBuffer(const Value: TSoundBufferBackend); override;
    procedure SetPitch(const Value: Single); override;
    procedure SetRolloffFactor(const Value: Single); override;
    procedure SetReferenceDistance(const Value: Single); override;
    procedure SetMaxDistance(const Value: Single); override;
    function GetOffset: Single; override;
    procedure SetOffset(const Value: Single); override;
  end;

  TOpenALSoundEngineBackend = class(TSoundEngineBackend)
  strict private
    FALMajorVersion, FALMinorVersion: Integer;
    ALDevice: PALCdevice;
    ALContext: PALCcontext;
    FEFXSupported: boolean;
    { ContextOpen was already called once with result @true. }
    WasAlreadyOpen: Boolean;
    WasAlreadyOpenDevice: String;

    { Check ALC errors. Requires valid ALDevice. }
    procedure CheckALC(const Situation: string);

    { Wrapper for alcGetString. }
    function GetContextString(const Enum: TALCenum): String;
  private
    ALVersion11: Boolean;
  public
    procedure DetectDevices(const Devices: TSoundDeviceList); override;
    function ContextOpen(const ADevice: String; out Information: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;

    procedure SetGain(const Value: Single); override;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
    procedure SetListener(const Position, Direction, Up: TVector3); override;

    { Is the OpenAL version at least @code(AMajor.AMinor).
      Available only when OpenAL is initialized, that is:
      between @link(TSoundEngine.ContextOpen) and @link(TSoundEngine.ContextClose),
      only when @link(TSoundEngine.IsContextOpenSuccess). }
    function ALVersionAtLeast(const AMajor, AMinor: Integer): boolean;

    { Are OpenAL effects (EFX) extensions supported.
      Meaningful only after ContextOpen,
      when IsContextOpenSuccess, that is it's initialized by . }
    property EFXSupported: boolean read FEFXSupported;
  end;

const
  ALDataFormat: array [TSoundDataFormat] of TALuint = (
    AL_FORMAT_MONO8,
    AL_FORMAT_MONO16,
    AL_FORMAT_STEREO8,
    AL_FORMAT_STEREO16
  );

{ TOpenALStreaming ----------------------------------------- }

constructor TOpenALStreaming.Create(
  const ASoundSurce: TOpenALSoundSourceBackend;
  const StreamBuffer: TOpenALStreamBufferBackend);
var
  I: Integer;
  NecessaryBuffers: Integer;
begin
  inherited Create;
  Source := ASoundSurce;
  Buffer := StreamBuffer;

  { Note: It is tempting to reuse one TStreamedSoundFile instance
    created inside FBuffer (TSoundBufferBackendFromStreamedFile class).
    This one instance could be used to initialize
    TSoundBufferBackendFromStreamedFile config
    (see TSoundBufferBackendFromStreamedFile.ReadStreamConfig)
    and be used by TOpenALStreaming too.

    However, this goes badly with threads.
    Multiple TOpenALStreaming (so also multiple threads)
    could then access the same TStreamedSoundFile instance,
    which is not ready for this.
    Random errors could be observed with examples/audio/play_sounds/ .

    So instead we create new TStreamedSoundFile.Create here.
    We can use it to initialize Buffer.ReadStreamConfig
    (useful to avoid the need to create temporary
    TStreamedSoundFile inside Buffer.ReadStreamConfigFromTemp
    in some situations).
  }
  StreamedFile := TStreamedSoundFile.Create(Buffer.URL);
  Buffer.ReadStreamConfig(StreamedFile);

  alCreateBuffers(StreamBuffersCount, ALBuffers);
  CheckAL('Before filling buffers');

  try
    NecessaryBuffers := 0;
    for I := 0 to StreamBuffersCount - 1 do
    begin
      { FillBuffer may return 0 if sound is non-looping and is too short
        to need StreamBuffersCount buffers.
        Testcase: play_sounds example,
        play castle-data:/sounds/misc_sound-22000Hz-8bit-mono.wav
        with slStreaming . }
      if FillBuffer(ALBuffers[I]) = 0 then
        Break;
      Inc(NecessaryBuffers);
      CheckAL('After filling buffer '+IntToStr(I));
    end;
  except
    alDeleteBuffers(StreamBuffersCount, @ALBuffers);
    raise;
  end;
  CheckAL('After filling all buffers');

  Assert(NecessaryBuffers > 0);
  Assert(NecessaryBuffers <= StreamBuffersCount);
  alSourceQueueBuffers(Source.ALSource, NecessaryBuffers, ALBuffers);
  CheckAL('After queue');
end;

destructor TOpenALStreaming.Destroy;

  procedure StopFeedingBuffers;
  begin
    {$ifdef CASTLE_SUPPORTS_THREADING}
    if FeedThread <> nil then
    begin
      FeedThread.Terminate;
      FeedThread.WaitFor;
      FreeAndNil(FeedThread);
    end;
    {$endif}
  end;

var
  ALBuffersProcessed: TALint;
  ALBuffer: TALuint;
  I:Integer;
begin
  StopFeedingBuffers;
  Source.Stop;

  { Stoping sound source mark all buffers as processed so we can delete them safely. }
  alGetSourcei(Source.ALSource, AL_BUFFERS_PROCESSED, @ALBuffersProcessed);
  for I := 0 to ALBuffersProcessed - 1 do
    alSourceUnqueueBuffers(Source.ALSource, 1, @ALBuffer);

  alDeleteBuffers(StreamBuffersCount, ALBuffers);

  FreeAndNil(StreamedFile);

  { Sound stream can be destroyed before Source, so need to fix dangling
    pointer here. }
  Source.FBuffer := nil;

  inherited;
end;

procedure TOpenALStreaming.FeedBuffers;
begin
  {$ifdef CASTLE_SUPPORTS_THREADING}
  if FeedThread <> nil then
  begin
    FeedThread.Terminate;
    FeedThread.WaitFor;
    FreeAndNil(FeedThread);
  end;

  FeedThread := TOpenALStreamFeedThread.Create(true, Self);
  FeedThread.Start;
  {$endif}
end;

function TOpenALStreaming.FillBuffer(const ALBuffer: TALuint): Integer;
var
  BufferPtr: Pointer;
const
  BufSize = 1024 * 32; // 32kB should be enough
begin
  BufferPtr := GetMem(BufSize);
  try
    Result := StreamedFile.Read(BufferPtr^, BufSize);
    if Result > 0 then
    begin
      alBufferData(ALBuffer, ALDataFormat[StreamedFile.DataFormat], BufferPtr, Result, StreamedFile.Frequency);
    end else
    if Source.FLooping then
    begin
      StreamedFile.Rewind;
      Result := FillBuffer(ALBuffer);
    end;
  finally
    FreeMemNiling(BufferPtr);
  end;
end;

{ TOpenALStreamFeedThread ---------------------------------------------------- }

procedure TOpenALStreamFeedThread.Execute;
var
  ALBuffersProcessed: TALint;
  ALBuffer: TALuint;
  ALSource: TALuint;
begin
  ALBuffersProcessed := 0;
  ALSource := Streaming.Source.ALSource;
  while (not Terminated) do
  begin
    Sleep(10);

    alGetSourcei(ALSource, AL_BUFFERS_PROCESSED, @ALBuffersProcessed);

    while ALBuffersProcessed > 0 do
    begin
      alSourceUnqueueBuffers(ALSource, 1, @ALBuffer);

      if Streaming.FillBuffer(ALBuffer) > 0 then
        alSourceQueueBuffers(ALSource, 1, @ALBuffer)
      else
        Exit;

      Dec(ALBuffersProcessed);
    end;

    if not Streaming.Source.PlayingOrPaused then
    begin
      alSourcePlay(ALSource);
    end;
  end;
end;

constructor TOpenALStreamFeedThread.Create(const CreateSuspended: Boolean;
  const AStreaming: TOpenALStreaming);
begin
  inherited Create(CreateSuspended);
  Streaming := AStreaming;
  Assert(not FreeOnTerminate); // should be default
end;

{ TOpenALSoundBufferBackend -------------------------------------------------- }

function TOpenALSoundBufferBackend.ALVersion11: Boolean;
begin
  Result := (SoundEngine as TOpenALSoundEngineBackend).ALVersion11;
end;

procedure TOpenALSoundBufferBackend.ContextOpenFromSoundFile(const SoundFile: TSoundFile);
begin
  inherited;

  alCreateBuffers(1, @ALBuffer);
  try
    alBufferData(ALBuffer, ALDataFormat[SoundFile.DataFormat],
      SoundFile.Data, SoundFile.DataSize, SoundFile.Frequency);
  except alDeleteBuffers(1, @ALBuffer); raise end;
end;

procedure TOpenALSoundBufferBackend.ContextClose;
begin
  alFreeBuffer(ALBuffer);
  inherited;
end;

{ TOpenALSoundSourceBackend -------------------------------------------------- }

function TOpenALSoundSourceBackend.ALVersion11: Boolean;
begin
  Result := (SoundEngine as TOpenALSoundEngineBackend).ALVersion11;
end;

procedure TOpenALSoundSourceBackend.AdjustALLooping;
begin
  if FBuffer is TOpenALStreamBufferBackend then
    alSourcei(ALSource, AL_LOOPING, BoolToAL[false])
  else
    alSourcei(ALSource, AL_LOOPING, BoolToAL[FLooping]);
end;

procedure TOpenALSoundSourceBackend.ContextOpen;
var
  ErrorCode: TALenum;
begin
  { We have to check alGetError now, because we may need to catch
    (and convert to ENoMoreSources exception) alGetError after
    alCreateSources. So we want to have "clean error state" first. }
  CheckAL('Checking before TOpenALSoundSourceBackend.ContextOpen');
  alCreateSources(1, @ALSource);

  ErrorCode := alGetError();
  if ErrorCode = AL_INVALID_VALUE then
    raise ENoMoreSources.Create('No more sound sources available')
  else
  if ErrorCode <> AL_NO_ERROR then
    raise EALError.Create(ErrorCode,
      'OpenAL error AL_xxx at creation of sound : ' + alGetString(ErrorCode));
end;

procedure TOpenALSoundSourceBackend.ContextClose;
begin
  FreeAndNil(Streaming);
  alDeleteSources(1, @ALSource);
end;

function TOpenALSoundSourceBackend.PlayingOrPaused: boolean;
var
  SourceState: TALuint;
begin
  SourceState := alGetSource1i(ALSource, AL_SOURCE_STATE);
  Result := (SourceState = AL_PLAYING) or (SourceState = AL_PAUSED);
end;

procedure TOpenALSoundSourceBackend.Play(const BufferChangedRecently: Boolean);
var
  CompleteBuffer: TOpenALSoundBufferBackend;
begin
  if FBuffer is TOpenALStreamBufferBackend then
  begin
    CheckAL('PlayStream');
    alSourcePlay(ALSource);

    // start feed buffers thread
    Assert(Streaming <> nil);
    Streaming.FeedBuffers;
  end else

  if FBuffer is TOpenALSoundBufferBackend then
  begin
    CompleteBuffer := TOpenALSoundBufferBackend(FBuffer);

    if BufferChangedRecently and
       (CompleteBuffer <> nil) and
       (CompleteBuffer.ALBuffer <> 0) then
    begin
      { This is a workaround needed on Apple OpenAL implementation
        (although I think that at some time I experienced similar
        problems (that would be cured by this workaround) on Linux
        (Loki OpenAL implementation)).

        The problem: music on some
        levels doesn't play. This happens seemingly random: sometimes
        when you load a level music starts playing, sometimes it's
        silent. Then when you go to another level, then go back to the
        same level, music plays.

        Investigation: I found that sometimes changing the buffer
        of the sound doesn't work immediately. Simple
          Writeln(SoundInfos.List^[Sound].Buffer, ' ',
            alGetSource1ui(FAllocatedSource.ALSource, AL_BUFFER));
        right after alCommonSourceSetup shows this (may output
        two different values). Then if you wait a little, OpenAL
        reports correct buffer. This probably means that OpenAL
        internally finishes some tasks related to loading buffer
        into source. Whatever it is, it seems that it doesn't
        occur (or rather, is not noticeable) on normal game sounds
        that are short --- but it's noticeable delay with larger
        sounds, like typical music.

        So the natural workaround below follows. For OpenAL implementations
        that immediately load the buffer, this will not cause any delay. }

      { We have to do CheckAL first, to catch eventual errors.
        Otherwise the loop could hang. }
      CheckAL('PlaySound');
      while CompleteBuffer.ALBuffer <> alGetSource1ui(ALSource, AL_BUFFER) do
        Sleep(10);
    end;
    alSourcePlay(ALSource);
  end else

    raise EInternalError.CreateFmt('Cannot play buffer class type %s', [FBuffer.ClassName]);
end;

procedure TOpenALSoundSourceBackend.Stop;
begin
  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSource, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSource);
end;

procedure TOpenALSoundSourceBackend.SetPosition(const Value: TVector3);
begin
  alSourceVector3f(ALSource, AL_POSITION, Value);
end;

procedure TOpenALSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  alSourceVector3f(ALSource, AL_VELOCITY, Value);
end;

procedure TOpenALSoundSourceBackend.SetLooping(const Value: boolean);
begin
  { This variable is set from main thread but can be read by 2 threads (main and
    TOpenALStreamFeedThread, but I think this is Boolean and changeing Boolean
    value is atomic so we don't need critical section here.
    Can be changed if that make any issues.
    More info:
    https://stackoverflow.com/questions/5481030/are-delphi-simple-types-thread-safe
    }
  FLooping := Value;
  AdjustALLooping;
end;

procedure TOpenALSoundSourceBackend.SetRelative(const Value: boolean);
begin
  alSourcei(ALSource, AL_SOURCE_RELATIVE, BoolToAL[Value]);
end;

procedure TOpenALSoundSourceBackend.SetGain(const Value: Single);
begin
  alSourcef(ALSource, AL_GAIN, Value);
end;

procedure TOpenALSoundSourceBackend.SetMinGain(const Value: Single);
begin
  alSourcef(ALSource, AL_MIN_GAIN, Value);
end;

procedure TOpenALSoundSourceBackend.SetMaxGain(const Value: Single);
begin
  alSourcef(ALSource, AL_MAX_GAIN, Value);
end;

procedure TOpenALSoundSourceBackend.SetBuffer(const Value: TSoundBufferBackend);
var
  CompleteBuffer: TOpenALSoundBufferBackend;
  StreamBuffer: TOpenALStreamBufferBackend;
begin
  { If some streamed buffer was connected to this source, disconnect it before
    the change. }
  FreeAndNil(Streaming);

  FBuffer := Value;

  if Assigned(FBuffer) then
  begin
    if FBuffer is TOpenALSoundBufferBackend then
    begin
      AdjustALLooping;
      CompleteBuffer := TOpenALSoundBufferBackend(FBuffer);
      { TSoundBuffer is unsigned, while alSourcei is declared as taking signed integer.
        But we know we can pass TSoundBuffer to alSourcei, just typecasting it to
        whatever alSourcei requires. }
      {$I norqcheckbegin.inc}
      alSourcei(ALSource, AL_BUFFER, CompleteBuffer.ALBuffer);
      {$I norqcheckend.inc}
    end else

    if FBuffer is TOpenALStreamBufferBackend then
    begin
      AdjustALLooping;
      StreamBuffer := TOpenALStreamBufferBackend(FBuffer);
      Streaming := TOpenALStreaming.Create(Self, StreamBuffer);
    end else

      raise EInternalError.CreateFmt('Cannot assign buffer class type %s', [FBuffer.ClassName]);
  end else
    alSourcei(ALSource, AL_BUFFER, 0);
end;

procedure TOpenALSoundSourceBackend.SetPitch(const Value: Single);
begin
  alSourcef(ALSource, AL_PITCH, Value);
end;

procedure TOpenALSoundSourceBackend.SetRolloffFactor(const Value: Single);
begin
  alSourcef(ALSource, AL_ROLLOFF_FACTOR, Value);
end;

procedure TOpenALSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
  alSourcef(ALSource, AL_REFERENCE_DISTANCE, Value);
end;

procedure TOpenALSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
  alSourcef(ALSource, AL_MAX_DISTANCE, Value);
end;

function TOpenALSoundSourceBackend.GetOffset: Single;
begin
  if ALVersion11 then
    Result := alGetSource1f(ALSource, AL_SEC_OFFSET)
  else
    Result := 0;
end;

procedure TOpenALSoundSourceBackend.SetOffset(const Value: Single);
var
  ErrorCode: TALenum;
begin
  if ALVersion11 then
  begin
    { We have to check alGetError now, because we need to catch
      AL_INVALID_VALUE later. }
    CheckAL('Checking before TOpenALSoundSourceBackend.SetOffset work');

    alSourcef(ALSource, AL_SEC_OFFSET, Value);

    { capture AL_INVALID_VALUE, otherwise it would be too easy to make mistake
      at setting offset to something like "duration-epsilon". }

    ErrorCode := alGetError();
    if ErrorCode = AL_INVALID_VALUE then
      WritelnWarning('Ignoring TOpenALSoundSourceBackend.SetOffset with offset %f', [Value])
    else
    if ErrorCode <> AL_NO_ERROR then
      raise EALError.Create(ErrorCode,
        'OpenAL error AL_xxx at setting sound offset : ' + alGetString(ErrorCode));
  end;
end;

{ TOpenALSoundEngineBackend -------------------------------------------------- }

procedure TOpenALSoundEngineBackend.DetectDevices(const Devices: TSoundDeviceList);

{ Find available OpenAL devices.

  It tries to use ALC_ENUMERATION_EXT extension, available on all modern
  OpenAL implementations. If it fails, and we're dealing with
  OpenAL "sample implementation" (older OpenAL Unix implementation)
  then we return a hardcoded list of devices known to be supported
  by this implementation.
  This makes it working sensibly under all OpenAL implementations in use
  today. }

  function SampleImpALCDeviceName(const ShortDeviceName: string): string;
  begin
    Result := '''(( devices ''(' + ShortDeviceName + ') ))';
  end;

var
  DeviceListPtr: PChar;
begin
  if not ALLibraryAvailable then
    WritelnWarning('Sound', 'OpenAL is not available, cannot list available audio devices')
  else
  if EnumerationExtPresent(DeviceListPtr) then
  begin
    { parse DeviceListPtr }
    while DeviceListPtr^ <> #0 do
    begin
      { automatic conversion PChar -> AnsiString below }
      Devices.Add(DeviceListPtr, DeviceListPtr);

      { advance position of DeviceListPtr }
      DeviceListPtr := StrEnd(DeviceListPtr);
      Inc(DeviceListPtr);
    end;
  end else
  begin
    WritelnWarning('Sound', 'OpenAL does not support getting the list of available audio devices (missing ALC_ENUMERATION_EXT), probably old OpenAL.');

    if OpenALSampleImplementation then
    begin
      Devices.Add(SampleImpALCDeviceName('native'), 'Operating System Native');
      Devices.Add(SampleImpALCDeviceName('sdl'), 'SDL (Simple DirectMedia Layer)');

      { aRts device is too unstable on my Linux:

        When trying to initialize <tt>arts</tt> backend
        I can bring the OpenAL library (and, consequently, whole program
        using it) to crash with message <i>can't create mcop
        directory</i>. Right after running konqueror, I get also
        crash with message <i>*** glibc detected *** double free or corruption (out):
        0x08538d88 ***</i>.

        This is so unstable, that I think that I do a service
        for users by *not* listing aRts in available OpenAL
        devices. It's listed on [https://castle-engine.io/openal_notes.php]
        and that's enough.

      Devices.Add(SampleImpALCDeviceName('arts'), 'aRts (analog Real time synthesizer)');
      }

      Devices.Add(SampleImpALCDeviceName('esd'), 'Esound (Enlightened Sound Daemon)');
      Devices.Add(SampleImpALCDeviceName('alsa'), 'ALSA (Advanced Linux Sound Architecture)');
      Devices.Add(SampleImpALCDeviceName('waveout'), 'WAVE File Output');
      Devices.Add(SampleImpALCDeviceName('null'), 'Null Device (No Output)');
    end;
  end;
end;

function TOpenALSoundEngineBackend.ALVersionAtLeast(const AMajor, AMinor: Integer): boolean;
begin
  Result :=
      (AMajor < FALMajorVersion) or
    ( (AMajor = FALMajorVersion) and (AMinor <= FALMinorVersion) );
end;

procedure TOpenALSoundEngineBackend.CheckALC(const Situation: string);
var
  ErrCode: TALenum;
  ErrDescription: PChar;
  ErrDescriptionStr: string;
begin
  ErrCode := alcGetError(ALDevice);
  if ErrCode <> ALC_NO_ERROR then
  begin
    { Secure, in case alcGetError returns nil (so the error code is incorrect),
      which happened long time ago on Creative Windows OpenAL implementation. }
    ErrDescription := alcGetString(ALDevice, ErrCode);
    if ErrDescription = nil then
      ErrDescriptionStr := Format('Unknown OpenAL (alc) error number: %d', [ErrCode])
    else
      ErrDescriptionStr := ErrDescription;

    raise EALCError.Create(ErrCode,
      'OpenAL error ALC_xxx at ' + Situation + ' : ' + ErrDescriptionStr);
  end;
end;

function TOpenALSoundEngineBackend.GetContextString(const Enum: TALCenum): String;
begin
  Result := alcGetString(ALDevice, Enum);
  try
    CheckALC('alcGetString');
    { Check also normal al error (alGetError instead
      of alcGetError). Seems that when Darwin (macOS) Apple's OpenAL
      implementation fails to return some alcGetString
      it reports this by setting AL error (instead of ALC one)
      to "invalid value". Although (after fixes to detect OpenALSampleImplementation
      at runtime and change constants values) this shouldn't happen anymore
      if you pass correct consts to this function. }
    CheckAL('alcGetString');
  except
    on E: EALCError do result := '('+E.Message+')';
    on E: EALError do result := '('+E.Message+')';
  end;
end;

function TOpenALSoundEngineBackend.ContextOpen(const ADevice: String;
  out Information: String): Boolean;

  procedure ParseVersion(const Version: string; out Major, Minor: Integer);
  var
    DotP, SpaceP: Integer;
  begin
    { version unknown }
    Major := 0;
    Minor := 0;

    DotP := Pos('.', Version);
    if DotP <> 0 then
    try
      Major := StrToInt(Trim(Copy(Version, 1, DotP - 1)));
      SpaceP := PosEx(' ', Version, DotP + 1);
      if SpaceP <> 0 then
        Minor := StrToInt(Trim(Copy(Version, DotP + 1, SpaceP - DotP))) else
        Minor := StrToInt(Trim(SEnding(Version, DotP + 1)));
    except
      on EConvertError do
      begin
        Major := 0;
        Minor := 0;
      end;
    end;
  end;

  function ALSuccessInformation: string;
  begin
    Result := Format(
      'OpenAL version: %s (major: %d, minor: %d)' +NL+
      'Renderer: %s' +NL+
      'Vendor: %s' +NL+
      'Extensions: %s', [
        alGetString(AL_VERSION),
        FALMajorVersion, FALMinorVersion,
        alGetString(AL_RENDERER),
        alGetString(AL_VENDOR),
        alGetString(AL_EXTENSIONS)
      ]);
  end;

begin
  { Workaround old OpenAL problems when trying to initialize OpenAL context again
    with another device. }
  if WasAlreadyOpen and (WasAlreadyOpenDevice <> ADevice) then
    OpenALRestart;

  { Try to initialize OpenAL.
    Sets Information, EFXSupported. }

  { We don't do alcProcessContext/alcSuspendContext, no need
    (spec says that context is initially in processing state). }

  try
    //raise EOpenALError.Create('Test pretend OpenAL fails');

    Result := false;
    FEFXSupported := false;
    Information := '';
    FALMajorVersion := 0;
    FALMinorVersion := 0;

    if not ALLibraryAvailable then
      raise EOpenALInitError.Create('OpenAL library is not available');

    Assert(Assigned(alcOpenDevice), 'Assigned(alcOpenDevice)');

    ALDevice := alcOpenDevice(PCharOrNil(ADevice));
    if (ALDevice = nil) then
      raise EOpenALError.CreateFmt(
        'OpenAL''s audio device "%s" is not available', [ADevice]);

    ALContext := alcCreateContext(ALDevice, nil);
    CheckALC('initializing OpenAL (alcCreateContext)');

    alcMakeContextCurrent(ALContext);
    CheckALC('initializing OpenAL (alcMakeContextCurrent)');

    FEFXSupported := Load_EFX(ALDevice);

    ParseVersion(alGetString(AL_VERSION), FALMajorVersion, FALMinorVersion);
    ALVersion11 := ALVersionAtLeast(1, 1);

    Result := true;
    Information := ALSuccessInformation;
    WasAlreadyOpen := true;
    WasAlreadyOpenDevice := ADevice;
  except
    on E: EOpenALError do
      Information := E.Message;
  end;
end;

procedure TOpenALSoundEngineBackend.ContextClose;
begin
  FEFXSupported := false;

  { CheckALC first, in case some error is "hanging" not caught yet. }
  CheckALC('right before closing OpenAL context');

  if ALContext <> nil then
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

    alcDestroyContext(ALContext);
    ALContext := nil;
    CheckALC('closing OpenAL context');
  end;

  if ALDevice <> nil then
  begin
    alcCloseDevice(ALDevice);
    { w/g specyfikacji OpenAL generuje teraz error ALC_INVALID_DEVICE jesli
      device bylo nieprawidlowe; ale niby jak mam sprawdzic ten blad ?
      Przeciez zeby sprawdzic alcGetError potrzebuje miec valid device w reku,
      a po wywolaniu alcCloseDevice(device) device jest invalid (bez wzgledu
      na czy przed wywolaniem alcCloseDevice bylo valid) }
    ALDevice := nil;
  end;
end;

procedure TOpenALSoundEngineBackend.SetGain(const Value: Single);
begin
  alListenerf(AL_GAIN, Value);
end;

procedure TOpenALSoundEngineBackend.SetDistanceModel(const Value: TSoundDistanceModel);
const
  ALDistanceModelConsts: array [TSoundDistanceModel] of TALenum = (
    AL_NONE,
    AL_INVERSE_DISTANCE,
    AL_INVERSE_DISTANCE_CLAMPED,
    AL_LINEAR_DISTANCE,
    AL_LINEAR_DISTANCE_CLAMPED,
    AL_EXPONENT_DISTANCE,
    AL_EXPONENT_DISTANCE_CLAMPED
  );
begin
  if (not ALVersion11) and (Value in [dmLinearDistance, dmExponentDistance]) then
    alDistanceModel(AL_INVERSE_DISTANCE)
  else
  if (not ALVersion11) and (Value in [dmLinearDistanceClamped, dmExponentDistanceClamped]) then
    alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED)
  else
    alDistanceModel(ALDistanceModelConsts[Value]);
end;

procedure TOpenALSoundEngineBackend.SetListener(const Position, Direction, Up: TVector3);
begin
  alListenerVector3f(AL_POSITION, Position);
  alListenerOrientation(Direction, Up);
end;

function TOpenALSoundEngineBackend.CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend;
begin
  {$ifdef CASTLE_SUPPORTS_THREADING}
  case SoundLoading of
    slComplete : Result := TOpenALSoundBufferBackend.Create(Self);
    slStreaming: Result := TOpenALStreamBufferBackend.Create(Self);
    else raise EInternalError.Create('TOpenALSoundEngineBackend.CreateBuffer: Invalid SoundLoading');
  end;
  {$else}
  Result := TOpenALSoundBufferBackend.Create(Self);
  {$endif}
end;

function TOpenALSoundEngineBackend.CreateSource: TSoundSourceBackend;
begin
  Result := TOpenALSoundSourceBackend.Create(Self);
end;

{ globals -------------------------------------------------------------------- }

procedure UseOpenALSoundBackend;
begin
  SoundEngine.InternalBackend := TOpenALSoundEngineBackend.Create;
end;

end.
