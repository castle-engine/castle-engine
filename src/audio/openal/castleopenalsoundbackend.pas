{
  Copyright 2010-2022 Michalis Kamburelis.

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
  CastleInternalALUtils, CastleInternalEFX, CastleLog, CastleUtils, CastleURIUtils;

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
  strict private
    const
      StreamBuffersCount = 4;
      HelperBufferSize = 1024 * 32; // 32kB should be enough
    var
      Buffer: TOpenALStreamBufferBackend;
      ALBuffers: array [0..StreamBuffersCount - 1] of TALuint;
      StreamedFile: TStreamedSoundFile;
      {$ifdef CASTLE_SUPPORTS_THREADING}
      FeedThread: TOpenALStreamFeedThread;
      {$endif}
      { Used in each FillBuffer.
        Contents of this between FillBuffer are undefined,
        we keep this field only to avoid wasting time on GetMem/FreeMem in each
        FillBuffer call. }
      HelperBufferPtr: Pointer;
  private
    Source: TOpenALSoundSourceBackend;
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
    FSpatial: Boolean; //< by default true, as this is OpenAL default
    FPosition, FVelocity: TVector3;
    FReferenceDistance, FMaxDistance: Single;
    function ALVersion11: Boolean;
  private
    FBuffer: TSoundBufferBackend;
    ALSource: TALuint;
    FLoop: Boolean;

    { When buffer is stremed, OpenAL source looping need to be off,
      otherwise, one buffer will be looped. This procedure cares about that. }
    procedure AdjustALLooping;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend);
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: boolean; override;
    procedure Play(const BufferChangedRecently: Boolean); override;
    procedure Stop; override;
    procedure SetPosition(const Value: TVector3); override;
    procedure SetVelocity(const Value: TVector3); override;
    procedure SetLoop(const Value: boolean); override;
    procedure SetSpatial(const Value: boolean); override;
    procedure SetVolume(const Value: Single); override;
    procedure SetMinGain(const Value: Single); override;
    procedure SetMaxGain(const Value: Single); override;
    procedure SetBuffer(const Value: TSoundBufferBackend); override;
    procedure SetPitch(const Value: Single); override;
    procedure SetReferenceDistance(const Value: Single); override;
    procedure SetMaxDistance(const Value: Single); override;
    procedure SetPriority(const Value: Single); override;
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
    function ContextOpen(const ADevice: String; out Information, InformationSummary: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;

    procedure SetVolume(const Value: Single); override;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
    procedure SetDopplerFactor(const Value: Single); override;
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

{ Check and report (as warnings) OpenAL errors as often as possible.
  This is useful to localize faulty OpenAL command, that causes OpenAL error. }
{.$define CASTLE_OPENAL_DEBUG}

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

  HelperBufferPtr := GetMem(HelperBufferSize);

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

  alCreateBuffers(StreamBuffersCount, @ALBuffers[Low(ALBuffers)]);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alCreateBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

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
    end;
  except
    alDeleteBuffers(StreamBuffersCount, @ALBuffers);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alDeleteBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
    raise;
  end;

  Assert(NecessaryBuffers > 0);
  Assert(NecessaryBuffers <= StreamBuffersCount);
  alSourceQueueBuffers(Source.ALSource, NecessaryBuffers, @ALBuffers[Low(ALBuffers)]);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceQueueBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
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
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alGetSourcei(.., AL_BUFFERS_PROCESSED, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  for I := 0 to ALBuffersProcessed - 1 do
    alSourceUnqueueBuffers(Source.ALSource, 1, @ALBuffer);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceUnqueueBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

  alDeleteBuffers(StreamBuffersCount, @ALBuffers[Low(ALBuffers)]);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alDeleteBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

  FreeAndNil(StreamedFile);

  { Sound stream can be destroyed before Source, so need to fix dangling
    pointer here. }
  Source.FBuffer := nil;

  FreeMemNiling(HelperBufferPtr);

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
begin
  Result := StreamedFile.Read(HelperBufferPtr^, HelperBufferSize);
  if Result > 0 then
  begin
    alBufferData(ALBuffer, ALDataFormat[StreamedFile.DataFormat],
      HelperBufferPtr, Result, StreamedFile.Frequency);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alBufferData ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end else
  if Source.FLoop then
  begin
    StreamedFile.Rewind;
    Result := FillBuffer(ALBuffer);
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
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alGetSourcei(.., AL_BUFFERS_PROCESSED, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

    while ALBuffersProcessed > 0 do
    begin
      alSourceUnqueueBuffers(ALSource, 1, @ALBuffer);
      {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceUnqueueBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

      if Streaming.FillBuffer(ALBuffer) > 0 then
      begin
        alSourceQueueBuffers(ALSource, 1, @ALBuffer);
        {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceQueueBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
      end else
        Exit;

      Dec(ALBuffersProcessed);
    end;

    if not Streaming.Source.PlayingOrPaused then
    begin
      alSourcePlay(ALSource);
      {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcePlay ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
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
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alCreateBuffers ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  try
    alBufferData(ALBuffer, ALDataFormat[SoundFile.DataFormat],
      SoundFile.Data, SoundFile.DataSize, SoundFile.Frequency);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alBufferData ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  except
    alFreeBuffer(ALBuffer);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alFreeBuffer ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
    raise;
  end;
end;

procedure TOpenALSoundBufferBackend.ContextClose;
begin
  alFreeBuffer(ALBuffer);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alFreeBuffer ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  inherited;
end;

{ TOpenALSoundSourceBackend -------------------------------------------------- }

constructor TOpenALSoundSourceBackend.Create(const ASoundEngine: TSoundEngineBackend);
begin
  inherited;
  FSpatial := true;
  // correspond to OpenAL defaults, https://www.openal.org/documentation/openal-1.1-specification.pdf
  FReferenceDistance := 1;
  FMaxDistance := MaxSingle;
end;

function TOpenALSoundSourceBackend.ALVersion11: Boolean;
begin
  Result := (SoundEngine as TOpenALSoundEngineBackend).ALVersion11;
end;

procedure TOpenALSoundSourceBackend.AdjustALLooping;
begin
  if FBuffer is TOpenALStreamBufferBackend then
    alSourcei(ALSource, AL_LOOPING, BoolToAL[false])
  else
    alSourcei(ALSource, AL_LOOPING, BoolToAL[FLoop]);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcei(.., AL_LOOPING, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
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
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alDeleteSources ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

function TOpenALSoundSourceBackend.PlayingOrPaused: boolean;
var
  SourceState: TALuint;
begin
  SourceState := alGetSource1i(ALSource, AL_SOURCE_STATE);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alGetSource1i(.., AL_SOURCE_STATE) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  Result := (SourceState = AL_PLAYING) or (SourceState = AL_PAUSED);
end;

procedure TOpenALSoundSourceBackend.Play(const BufferChangedRecently: Boolean);
var
  CompleteBuffer: TOpenALSoundBufferBackend;
begin
  // make a clear warning when trying to play stereo sound as spatial
  if FSpatial and
     (FBuffer.DataFormat in [sfStereo8, sfStereo16]) then
    WritelnWarning('Stereo sound files are *never* played as spatial by OpenAL. Convert sound file "%s" to mono (e.g. by Audacity or SOX).', [
      URIDisplay(FBuffer.URL)
    ]);

  if FBuffer is TOpenALStreamBufferBackend then
  begin
    alSourcePlay(ALSource);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcePlay ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

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
          Writeln(SoundInfos.L[Sound].Buffer, ' ',
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
      CheckAL('TOpenALSoundSourceBackend.Play');
      while CompleteBuffer.ALBuffer <> alGetSource1ui(ALSource, AL_BUFFER) do
        Sleep(10);
    end;

    alSourcePlay(ALSource);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcePlay ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end else

  // ignore FBuffer = nil which may mean that streaming sound failed to load
  if FBuffer <> nil then
    raise EInternalError.CreateFmt('Cannot play buffer class type %s', [FBuffer.ClassName]);
end;

procedure TOpenALSoundSourceBackend.Stop;
begin
  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSource, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSource);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceStop ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetPosition(const Value: TVector3);
begin
  FPosition := Value;

  if not FSpatial then Exit; // apply this only if Spatial
  alSourceVector3f(ALSource, AL_POSITION, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceVector3f(.., AL_POSITION, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  FVelocity := Value;

  if not FSpatial then Exit; // apply this only if Spatial
  alSourceVector3f(ALSource, AL_VELOCITY, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceVector3f(.., AL_VELOCITY, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetLoop(const Value: boolean);
begin
  { This variable is set from main thread but can be read by 2 threads (main and
    TOpenALStreamFeedThread, but I think this is Boolean and changeing Boolean
    value is atomic so we don't need critical section here.
    Can be changed if that make any issues.
    More info:
    https://stackoverflow.com/questions/5481030/are-delphi-simple-types-thread-safe
    }
  FLoop := Value;
  AdjustALLooping;
end;

procedure TOpenALSoundSourceBackend.SetSpatial(const Value: boolean);
begin
  FSpatial := Value;

  alSourcei(ALSource, AL_SOURCE_RELATIVE, BoolToAL[not Value]);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcei(.., AL_SOURCE_RELATIVE, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

  if not Value then
  begin
    { No attenuation by distance. }
    alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_ROLLOFF_FACTOR, 0) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

    { Note: source's AL_REFERENCE_DISTANCE , AL_MAX_DISTANCE don't matter in this case. }

    { Although AL_ROLLOFF_FACTOR := 0 turns off
      attenuation by distance, we still have to turn off
      any changes from player's orientation (so that the sound
      is not played on left or right side, but normally). }
    alSourceVector3f(ALSource, AL_POSITION, TVector3.Zero);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceVector3f(.., AL_POSITION, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

    alSourceVector3f(ALSource, AL_VELOCITY, TVector3.Zero);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourceVector3f(.., AL_VELOCITY, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end else
  begin
    { Attenuation by distance. }
    alSourcef(ALSource, AL_ROLLOFF_FACTOR, 1);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_ROLLOFF_FACTOR, 1) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}

    { Set everything that was kept constant when Spatial was false.
      We rely here on the fact that SetXxx here do not ignore "setting to the same value as previous". }
    SetPosition(FPosition);
    SetVelocity(FVelocity);
    SetReferenceDistance(FReferenceDistance);
    SetMaxDistance(FMaxDistance);
  end;
end;

procedure TOpenALSoundSourceBackend.SetVolume(const Value: Single);
begin
  alSourcef(ALSource, AL_GAIN, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_GAIN, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetMinGain(const Value: Single);
begin
  alSourcef(ALSource, AL_MIN_GAIN, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_MIN_GAIN, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetMaxGain(const Value: Single);
begin
  alSourcef(ALSource, AL_MAX_GAIN, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_MAX_GAIN, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetBuffer(const Value: TSoundBufferBackend);

  { Set buffer to 0 on this OpenAL source. }
  procedure ClearALBuffer;
  begin
    alSourcei(ALSource, AL_BUFFER, 0);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcei(.., AL_BUFFER, 0) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end;

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
      {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcei(.., AL_BUFFER, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
      {$I norqcheckend.inc}
    end else

    if FBuffer is TOpenALStreamBufferBackend then
    begin
      AdjustALLooping;
      StreamBuffer := TOpenALStreamBufferBackend(FBuffer);
      try
        Streaming := TOpenALStreaming.Create(Self, StreamBuffer);
      except
        { Catching exceptions in case of loading errors. Because:
          - The programs can generally tolerate missing sound files.
          - Error at OggVorbis reading may be caused by
            EOggVorbisMissingLibraryError, and we try to gracefully react to missing
            libraries.
          See TCastleSound.ReloadBuffer for the same logic and reasons
          for non-streaming sounds. }
        on E: ESoundFileError do
        begin
          WritelnWarning('Sound', Format('Sound file "%s" cannot be loaded (with streaming): %s', [
            // Do not use FBuffer.URL, as TOpenALStreaming.Destroy set FBuffer to nil
            URIDisplay(StreamBuffer.URL),
            E.Message
          ]));

          // continue like no buffer was assigned
          FBuffer := nil; // actually TOpenALStreaming.Destroy just did it
          ClearALBuffer;
        end;
      end;
    end else

      raise EInternalError.CreateFmt('Cannot assign buffer class type %s', [FBuffer.ClassName]);
  end else
    ClearALBuffer;
end;

procedure TOpenALSoundSourceBackend.SetPitch(const Value: Single);
begin
  alSourcef(ALSource, AL_PITCH, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_PITCH, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

(*
procedure TOpenALSoundSourceBackend.SetRolloffFactor(const Value: Single);
begin
  FRolloffFactor := Value;

  if not FSpatial then Exit; // apply this only if Spatial
  alSourcef(ALSource, AL_ROLLOFF_FACTOR, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_ROLLOFF_FACTOR, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;
*)

procedure TOpenALSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;

  if not FSpatial then Exit; // apply this only if Spatial
  alSourcef(ALSource, AL_REFERENCE_DISTANCE, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_REFERENCE_DISTANCE, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;

  if not FSpatial then Exit; // apply this only if Spatial
  alSourcef(ALSource, AL_MAX_DISTANCE, Value);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alSourcef(.., AL_MAX_DISTANCE, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

procedure TOpenALSoundSourceBackend.SetPriority(const Value: Single);
begin
  { Ignored by OpenAL backend,
    as it depends on our manual management of sound sources in TSoundAllocator. }
end;

function TOpenALSoundSourceBackend.GetOffset: Single;
begin
  if ALVersion11 then
  begin
    Result := alGetSource1f(ALSource, AL_SEC_OFFSET);
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alGetSource1f(.., AL_SEC_OFFSET) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end else
    Result := 0;
end;

procedure TOpenALSoundSourceBackend.SetOffset(const Value: Single);
var
  ErrorCode: TALenum;
begin
  if ALVersion11 then
  begin
    { We have to check alGetError now, because we need to catch
      AL_INVALID_VALUE later.
      TODO: only warning now, occurs when switch Stream/URL of TCastleSound. }
    CheckAL('Checking before TOpenALSoundSourceBackend.SetOffset work', true);

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
  out Information, InformationSummary: String): Boolean;

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
    {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alGetString ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  end;

  function ALSuccessInformationSummary: string;
  begin
    Result := Format('OpenAL %d.%d', [
      FALMajorVersion,
      FALMinorVersion
    ]);
  end;

var
  ErrMessage: String;
begin
  { Workaround old OpenAL problems when trying to initialize OpenAL context again
    with another device. }
  if WasAlreadyOpen and (WasAlreadyOpenDevice <> ADevice) then
    OpenALRestart;

  { Try to initialize OpenAL.
    Sets Information, EFXSupported. }

  { We don't do alcProcessContext/alcSuspendContext, no need
    (spec says that context is initially in processing state). }

  Result := false;
  try
    //raise EOpenALError.Create('Test pretend OpenAL fails');

    FEFXSupported := false;
    Information := '';
    InformationSummary := '';
    FALMajorVersion := 0;
    FALMinorVersion := 0;

    if not ALLibraryAvailable then
      raise EOpenALInitError.Create('OpenAL library is not available');

    Assert(Assigned(alcOpenDevice), 'Assigned(alcOpenDevice)');

    ALDevice := alcOpenDevice(PCharOrNil(ADevice));
    if ALDevice = nil then
    begin
      ErrMessage := Format('OpenAL audio device "%s" is not available', [ADevice]);
      {$ifdef MSWINDOWS}
      if ADevice = '' then
        ErrMessage := ErrMessage + '.' + NL + 'Note: It seems that even the default audio device is unavailable. ' + 'Please check that you have all the necessary OpenAL DLL files present (alongside the exe file, or on $PATH). In case of standard Windows OpenAL implementation, you should have OpenAL32.dll and wrap_oal.dll present.';
      {$endif}
      raise EOpenALError.Create(ErrMessage);
    end;

    ALContext := alcCreateContext(ALDevice, nil);
    CheckALC('initializing OpenAL (alcCreateContext)');

    alcMakeContextCurrent(ALContext);
    CheckALC('initializing OpenAL (alcMakeContextCurrent)');

    FEFXSupported := Load_EFX(ALDevice);

    ParseVersion(alGetString(AL_VERSION), FALMajorVersion, FALMinorVersion);
    ALVersion11 := ALVersionAtLeast(1, 1);

    Result := true;
    Information := ALSuccessInformation;
    InformationSummary := ALSuccessInformationSummary;
    WasAlreadyOpen := true;
    WasAlreadyOpenDevice := ADevice;
  except
    on E: EOpenALError do
    begin
      Information := E.Message;
      InformationSummary := Information;
    end;
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

procedure TOpenALSoundEngineBackend.SetVolume(const Value: Single);
begin
  alListenerf(AL_GAIN, Value);
end;

procedure TOpenALSoundEngineBackend.SetDistanceModel(const Value: TSoundDistanceModel);
const
  ALDistanceModelConsts: array [TSoundDistanceModel] of TALenum = (
    AL_INVERSE_DISTANCE_CLAMPED,
    AL_LINEAR_DISTANCE_CLAMPED
  );
begin
  { Note:

    Initially we supported all OpenAL distance models:

      TSoundDistanceModel = (dmNone,
        dmInverseDistance , dmInverseDistanceClamped,
        dmLinearDistance  , dmLinearDistanceClamped,
        dmExponentDistance, dmExponentDistanceClamped);

    mapping to

      const
        ALDistanceModelConsts: array [TSoundDistanceModel] of TALenum = (
          AL_NONE,
          AL_INVERSE_DISTANCE,
          AL_INVERSE_DISTANCE_CLAMPED,
          AL_LINEAR_DISTANCE,              //< only OpenAL >= 1.1
          AL_LINEAR_DISTANCE_CLAMPED,      //< only OpenAL >= 1.1
          AL_EXPONENT_DISTANCE,            //< only OpenAL >= 1.1
          AL_EXPONENT_DISTANCE_CLAMPED     //< only OpenAL >= 1.1
        );

    Now we support only a subset of distance models, that is common to
    OpenAL ( https://www.openal.org/documentation/openal-1.1-specification.pdf ),
    FMOD ( https://www.fmod.com/resources/documentation-api?version=2.01&page=white-papers-3d-sounds.html#inverse ).

    Some models are actually available only since OpenAL 1.1
    version. We eventually may fallback on some other model.
    This is not be a problem in practice, as all modern OS
    versions (Linux distros, Windows OpenAL installers etc.) include OpenAL 1.1. }

(* Old version, for all models:

  if (not ALVersion11) and (Value in [dmLinearDistance, dmExponentDistance]) then
    alDistanceModel(AL_INVERSE_DISTANCE)
  else
  if (not ALVersion11) and (Value in [dmLinearDistanceClamped, dmExponentDistanceClamped]) then
    alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED)
  else
    alDistanceModel(ALDistanceModelConsts[Value]);
*)

  if (not ALVersion11) and (Value = dmLinear) then
    alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED) // OpenAL < 1.1 doesn't support AL_LINEAR_DISTANCE* models.
  else
    alDistanceModel(ALDistanceModelConsts[Value]);
end;

procedure TOpenALSoundEngineBackend.SetDopplerFactor(const Value: Single);
begin
  alDopplerFactor(Value);
end;

procedure TOpenALSoundEngineBackend.SetListener(const Position, Direction, Up: TVector3);
begin
  alListenerVector3f(AL_POSITION, Position);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alListenerVector3f(AL_POSITION, ..) ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
  alListenerOrientation(Direction, Up);
  {$ifdef CASTLE_OPENAL_DEBUG} CheckAL('alListenerOrientation ' + {$include %FILE%} + ':' + {$include %LINE%}, true); {$endif}
end;

function TOpenALSoundEngineBackend.CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend;
begin
  {$ifdef CASTLE_SUPPORTS_THREADING}
  case SoundLoading of
    slComplete : Result := TOpenALSoundBufferBackend.Create(Self);
    slStreaming: Result := TOpenALStreamBufferBackend.Create(Self);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TOpenALSoundEngineBackend.CreateBuffer: Invalid SoundLoading');
    {$endif}
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
