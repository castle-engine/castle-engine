{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3D sound smart allocation (TSoundAllocator). }
unit CastleSoundAllocator;

{$I castleconf.inc}

interface

uses SysUtils, CastleOpenAL, CastleClassUtils, Classes, CastleUtils, CastleVectors,
  CastleXMLConfig, FGL;

type
  TSound = class;

  TSoundBuffer = type TALuint;

  TSoundEvent = procedure (Sender: TSound) of object;

  ENoMoreOpenALSources = class(Exception);

  { Sound (an allocated OpenAL sound source). }
  TSound = class
  private
    FUsed: boolean;
    FOnRelease: TSoundEvent;
    FImportance: Integer;
    FALSource: TALuint;
    { This must be @true for the whole lifetime of this object
      except the situation at the beginning of the constructor,
      and in destructor (if constructor exited with ENoMoreOpenALSources). }
    FALSourceAllocated: boolean;
    FUserData: TObject;
    FPosition, FVelocity: TVector3Single;
    FLooping, FRelative: boolean;
    FGain, FMinGain, FMaxGain, FPitch: Single;
    FBuffer: TSoundBuffer;
    FRolloffFactor, FReferenceDistance, FMaxDistance: Single;
    procedure SetPosition(const Value: TVector3Single);
    procedure SetVelocity(const Value: TVector3Single);
    procedure SetLooping(const Value: boolean);
    procedure SetRelative(const Value: boolean);
    procedure SetGain(const Value: Single);
    procedure SetMinGain(const Value: Single);
    procedure SetMaxGain(const Value: Single);
    procedure SetBuffer(const Value: TSoundBuffer);
    procedure SetPitch(const Value: Single);
    procedure SetRolloffFactor(const Value: Single);
    procedure SetReferenceDistance(const Value: Single);
    procedure SetMaxDistance(const Value: Single);
  public
    { Create sound. This allocates actual OpenAL source.
      @raises(ENoMoreOpenALSources If no more sources available.
        It should be caught and silenced by TSoundAllocator.AllocateSound.) }
    constructor Create;
    destructor Destroy; override;

    property ALSource: TALuint read FALSource;

    { Do we play something.
      Sources that are not Used are simply OpenAL allocated sources
      that are not used right now, and will be used when we will
      need them. }
    property Used: boolean read FUsed default false;

    { The priority of keeping this source, relevant only when @link(Used).

      Higher Importance means that it's more important to keep it.
      (I didn't name this property "Priority" so that it's obvious
      that higher Importance means more important sound). }
    property Importance: Integer read FImportance default 0;

    { Any data comfortable to keep here by the caller of
      TSoundAllocator.AllocateSound. It should be initialized
      after calling TSoundAllocator.AllocateSound, and should
      be finalized in OnRelease. }
    property UserData: TObject read FUserData write FUserData;

    { Called when this OpenAL allocated sound will no longer
      be used. It may stop be used because there are more demanding
      sources (see @link(Importance) and to keep MaxAllocatedSources)
      and we must assign this OpenAL sound slot to something else,
      or it may stop be used because it simply stopped playing.

      But note that we do not make any guarantees that sources that
      stopped playing will be immediately reported to OnRelease.
      In fact, a source may be considered in Used = @true state
      for a long time until it stopped playing. That's not a problem
      for this unit --- TSoundAllocator.AllocateSound is smart,
      and it may actually check (and eventually mark with @link(Release))
      whether some sources are in playing state,
      to avoid allocating unnecessary sources.
      However, if this is a problem for you (because e.g. you do
      some expensive operations to update all used sources every time)
      and you really desire OnRelease to be called quickly after
      sound stoppped playing, you may call TSoundAllocator.Refresh
      from time to time.

      In this event you should make sure to delete all references
      to this sound, because the TSound instance may
      be freed (or reused for other means) after calling OnRelease.
      For the same reason, after calling this, we always clear it
      (set OnRelease to @nil).

      It's guaranteed that when this will be called,
      @link(Used) will be @false and @link(PlayingOrPaused) will be @false. }
    property OnRelease: TSoundEvent read FOnRelease write FOnRelease;

    { Stops playing the source,
      sets Used to @false, and calls OnRelease (if assigned).

      You can call this yourself if you want to stop playing the sound.
      It's preferable to call this (instead of manually calling
      alSourceStop), because this will immediately mark Used property
      as @false and will call OnRelease. Otherwise we would have to
      get source state at some time (they are checked in AllocateSound)
      and check it, then see that it's no longer playing.

      You can call this only when Used = @true. }
    procedure Release; virtual;

    property Position: TVector3Single read FPosition write SetPosition;
    property Velocity: TVector3Single read FVelocity write SetVelocity;
    property Looping: boolean read FLooping write SetLooping;
    property Relative: boolean read FRelative write SetRelative;
    property Gain: Single read FGain write SetGain;
    property MinGain: Single read FMinGain write SetMinGain;
    property MaxGain: Single read FMaxGain write SetMaxGain;
    property Buffer: TSoundBuffer read FBuffer write SetBuffer;
    property Pitch: Single read FPitch write SetPitch;
    property RolloffFactor: Single read FRolloffFactor write SetRolloffFactor;
    property ReferenceDistance: Single read FReferenceDistance write SetReferenceDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { Is the sound playing or paused. This is almost always @true for sounds
      returned by TSoundAllocator.AllocateSound, when it stops being @true
      --- the sound engine will realize it (soon), which will cause @link(Release)
      and OnRelease being automatically called, and this TSound may then
      be reused for playing other sounds. }
    function PlayingOrPaused: boolean;
  end;

  TSoundList = class(specialize TFPGObjectList<TSound>)
  public
    { Sort sounds by Used + Importance, descending.
      First all sounds with Used = @true are placed,
      starting from the sound with largest Importance, and so on
      until the sound with smallest Importance.
      Then all sounds with Used = @false are placed (in any, arbitrary order).

      List must not contain nil values when calling this. }
    procedure SortByImportance;
  end;

  { Manage allocated OpenAL sounds.
    You leave to this class creating and deleting of OpenAL sounds.
    When you need OpenAL sound to do something, just call AllocateSound method.

    This class will manage OpenAL sources in an intelligent manner,
    which means when you need new sound, we may
    @orderedList(
      @item(Reuse already allocated sound that is not used to play anything.)
      @item(Allocate new sound (but we will keep allocated sounds count
        within MaxAllocatedSources sound limit, to not overload OpenAL
        implementation with work).)
      @item(We may simply interrupt already allocated sound, if new
        sound is more important.)
    )

    Our OpenAL resources are created in ALContextOpen, and released
    in ALContextClose.

    The very reason behind this class is to hide from you the fact that
    the number of OpenAL sources are limited. In particular, this
    means that when OpenAL will run out of sources, no OpenAL error
    (alGetError) will be left, and no exception will be raised.
    In the worst case TSoundAllocator.AllocateSound will return nil,
    but in more probable cases some other sources (unused, or with
    less priority) will be reused.

    Note that this means that the code in this unit must
    read in some situations alGetError. That's because reading alGetError
    is the only way to know when OpenAL implementation has run out of sources.
    So the code in this unit may in various places raise EALError if you
    made some error in your OpenAL code, and you didn't check alGetError
    yourself often enough. }
  TSoundAllocator = class
  private
    FAllocatedSources: TSoundList;
    FMinAllocatedSources: Cardinal;
    FMaxAllocatedSources: Cardinal;
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  public
    const
      DefaultMinAllocatedSources = 4;
      DefaultMaxAllocatedSources = 16;

    constructor Create;
    destructor Destroy; override;
    procedure ALContextOpen; virtual;
    procedure ALContextClose; virtual;

    { Allocate sound for playing. You should initialize the OpenAL sound
      properties and start playing the sound (you have
      OpenAL sound identifier in TSound.ALSource).

      Note that if you don't call alSourcePlay, the source may be detected
      as unused (and recycled for another sound) at the next AllocateSound,
      PlaySound, @link(Refresh) and such calls.

      If we can't allocate new OpenAL sound, we return nil.
      This may happen your OpenAL context is not initialized.
      It may also happen if we cannot create more sources (because
      we hit MaxAllocatedSources limit, or OpenAL just refuses to create
      more sources) and all existing sounds are used and their
      Importance is > given here Importance.

      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Importance of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSound(const Importance: Integer): TSound;

    { All allocated (not necessarily used) OpenAL sources.
      Useful only for advanced or debuging tasks, in normal circumstances
      we mange this completely ourselves. This is @nil when ALContextOpen
      was not yet called. }
    property AllocatedSources: TSoundList read FAllocatedSources;

    { Detect unused sound sources. If you rely on your sources receiving
      TSound.OnRelease in a timely manner, be sure to call
      this method often. Otherwise, it's not needed to call this at all
      (unused sounds will be detected automatically on-demand anyway).

      This is automatically called by TCastleSceneManager.

      For every source that is marked as Used, this checks
      whether this source is actually in playing/paused state
      right now. If not, it calls @link(TSound.Release) (thus setting
      Used to @false and triggering OnRelease) for this source. }
    procedure Refresh;

    { Stop all the sources currently playing. Especially useful since
      you have to stop a source before releasing it's associated buffer. }
    procedure StopAllSources;

    { Load and save into the config file sound engine properties.
      For example use with @link(UserConfig) to store sound preferences
      along with other user preferences.
      Everything is loaded / saved under the path "sound/" inside Config.

      TSoundAllocator saves MinAllocatedSources, MaxAllocatedSources.
      Descendant TSoundEngine additionally saves current Device, Enable
      (unless Enable was set by @--no-sound command-line option).
      Descendant TRepoSoundEngine additionally saves sound and music volume.

      @groupBegin }
    procedure LoadFromConfig(const Config: TCastleConfig); virtual;
    procedure SaveToConfig(const Config: TCastleConfig); virtual;
    { @groupEnd }
  published
    { Minimum / maximum number of allocated OpenAL sources.
      Always keep MinAllocatedSources <= MaxAllocatedSources.

      For the sake of speed, we always keep allocated at least
      MinAllocatedSources OpenAL sources. This must be >= 1.
      Setting MinAllocatedSources too large value will raise
      ENoMoreOpenALSources.

      At most MaxAllocatedSources sources may be simultaneously used (played).
      This prevents us from allocating too many sounds,
      which would be bad for OpenAL speed (not to mention that it may
      be impossible under some OpenAL implementations, like Windows one).
      When all MaxAllocatedSources sources are playing, the only way
      to play another sound is to use appropriately high @code(Importance)
      to AllocateSound.

      @groupBegin }
    property MinAllocatedSources: Cardinal
      read FMinAllocatedSources write SetMinAllocatedSources
      default DefaultMinAllocatedSources;

    property MaxAllocatedSources: Cardinal
      read FMaxAllocatedSources write SetMaxAllocatedSources
      default DefaultMaxAllocatedSources;
    { @groupEnd }
  end;

implementation

uses CastleALUtils, CastleConfig;

{ TSound ---------------------------------------------------------- }

constructor TSound.Create;
var
  ErrorCode: TALenum;
begin
  inherited;

  { I must check alGetError now, because I may need to catch
    (and convert to ENoMoreOpenALSources exception) alGetError after
    alCreateSources. So I want to have "clean error state" first. }
  CheckAL('prevention OpenAL check in TSound.Create');

  alCreateSources(1, @FALSource);

  ErrorCode := alGetError();
  if ErrorCode = AL_INVALID_VALUE then
    raise ENoMoreOpenALSources.Create('No more sound sources available') else
  if ErrorCode <> AL_NO_ERROR then
    raise EALError.Create(ErrorCode,
      'OpenAL error AL_xxx at creation of sound : ' + alGetString(ErrorCode));

  { This signals to TSound.Destroy that FALSource contains
    valid source name, that should be deleted by alDeleteSources. }
  FALSourceAllocated := true;
end;

destructor TSound.Destroy;
begin
  if FALSourceAllocated then
    alDeleteSources(1, @FALSource);
  inherited;
end;

procedure TSound.Release;
begin
  FUsed := false;

  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSource, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSource);

  { Detach the buffer from source. Otherwise we couldn't free the buffer
    while it's associated with the source. Also, this would be a problem
    once we implement streaming on some sources: you have to reset
    buffer to 0 before queing buffers on source. }
  Buffer := 0;

  if Assigned(OnRelease) then
  begin
    OnRelease(Self);
    OnRelease := nil;
  end;
end;

procedure TSound.SetPosition(const Value: TVector3Single);
begin
  FPosition := Value;
  alSourceVector3f(ALSource, AL_POSITION, Value);
end;

procedure TSound.SetVelocity(const Value: TVector3Single);
begin
  FVelocity := Value;
  alSourceVector3f(ALSource, AL_VELOCITY, Value);
end;

procedure TSound.SetLooping(const Value: boolean);
begin
  FLooping := Value;
  alSourcei(ALSource, AL_LOOPING, BoolToAL[Value]);
end;

procedure TSound.SetRelative(const Value: boolean);
begin
  FRelative := Value;
  alSourcei(ALSource, AL_SOURCE_RELATIVE, BoolToAL[Value]);
end;

procedure TSound.SetGain(const Value: Single);
begin
  FGain := Value;
  alSourcef(ALSource, AL_GAIN, Value);
end;

procedure TSound.SetMinGain(const Value: Single);
begin
  FMinGain := Value;
  alSourcef(ALSource, AL_MIN_GAIN, Value);
end;

procedure TSound.SetMaxGain(const Value: Single);
begin
  FMaxGain := Value;
  alSourcef(ALSource, AL_MAX_GAIN, Value);
end;

procedure TSound.SetBuffer(const Value: TSoundBuffer);
begin
  FBuffer := Value;
  { TSoundBuffer is unsigned, while alSourcei is declared as taking signed integer.
    But we know we can pass TSoundBuffer to alSourcei, just typecasting it to
    whatever alSourcei requires. }
  {$I norqcheckbegin.inc}
  alSourcei(ALSource, AL_BUFFER, Value);
  {$I norqcheckend.inc}
end;

procedure TSound.SetPitch(const Value: Single);
begin
  FPitch := Value;
  alSourcef(ALSource, AL_PITCH, Value);
end;

procedure TSound.SetRolloffFactor(const Value: Single);
begin
  FRolloffFactor := Value;
  alSourcef(ALSource, AL_ROLLOFF_FACTOR, Value);
end;

procedure TSound.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;
  alSourcef(ALSource, AL_REFERENCE_DISTANCE, Value);
end;

procedure TSound.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  alSourcef(ALSource, AL_MAX_DISTANCE, Value);
end;

function TSound.PlayingOrPaused: boolean;
var
  SourceState: TALuint;
begin
  SourceState := alGetSource1i(ALSource, AL_SOURCE_STATE);
  Result := (SourceState = AL_PLAYING) or (SourceState = AL_PAUSED);
end;

{ TSoundList ----------------------------------------------------- }

function IsSmallerByImportance(const AA, BB: TSound): Integer;
begin
  if (AA.Used and (not BB.Used)) or
     (AA.Used and BB.Used and (AA.Importance > BB.Importance)) then
    Result := -1 else
  if (BB.Used and (not AA.Used)) or
     (BB.Used and AA.Used and (BB.Importance > AA.Importance)) then
    Result :=  1 else
    Result :=  0;
end;

procedure TSoundList.SortByImportance;
begin
  Sort(@IsSmallerByImportance);
end;

{ TSoundAllocator ---------------------------------------------------------- }

constructor TSoundAllocator.Create;
begin
  inherited;
  FMinAllocatedSources := DefaultMinAllocatedSources;
  FMaxAllocatedSources := DefaultMaxAllocatedSources;
  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);
end;

destructor TSoundAllocator.Destroy;
begin
  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;
  inherited;
end;

procedure TSoundAllocator.ALContextOpen;
var
  I: Integer;
begin
  FAllocatedSources := TSoundList.Create(false);
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.Count - 1 do
    FAllocatedSources[I] := TSound.Create;
end;

procedure TSoundAllocator.ALContextClose;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
  begin
    { Stop using and free allocated sounds. }
    for I := 0 to FAllocatedSources.Count - 1 do
      { Although usually we are sure that every FAllocatedSources[I] <> nil,
        in this case we must take into account that maybe our constructor
        raise ENonMoreOpenALSources and so some FAllocatedSources[I] were
        not initialized. }
      if FAllocatedSources[I] <> nil then
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        FPGObjectList_FreeAndNilItem(FAllocatedSources, I);
      end;

    FreeAndNil(FAllocatedSources);
  end;
end;

function TSoundAllocator.AllocateSound(
  const Importance: Integer): TSound;
var
  I: Integer;
  MinImportanceIndex: Integer;
begin
  Result := nil;

  { OpenAL context not initialized yet }
  if FAllocatedSources = nil then Exit;

  { Try: maybe we have already allocated unused sound ?
    If no unused sound will be found, it will calculate
    MinImportanceIndex, this will be useful later. }
  MinImportanceIndex := -1;
  for I := 0 to FAllocatedSources.Count - 1 do
    if not FAllocatedSources[I].Used then
    begin
      Result := FAllocatedSources[I];
      { Breaking here means that MinImportanceIndex will not be calculated
        correctly (because we did not iterate to the end of FAllocatedSources
        list). But that's OK, because if Result <> nil here, then we will
        not need MinImportanceIndex later. }
      Break;
    end else
    begin
      { Update MinImportanceIndex }
      if (MinImportanceIndex = -1) or
         (FAllocatedSources[I].Importance <
          FAllocatedSources[MinImportanceIndex].Importance) then
         MinImportanceIndex := I;
    end;

  { Try: maybe one of the allocated sounds is marked as Used,
    but actually it's not used anymore ? }
  if Result = nil then
  begin
    for I := 0 to FAllocatedSources.Count - 1 do
      if not FAllocatedSources[I].PlayingOrPaused then
      begin
        Result := FAllocatedSources[I];
        Break;
      end;
  end;

  { Try: maybe we can allocate one more sound ? }
  if (Result = nil) and
     (Cardinal(FAllocatedSources.Count) < MaxAllocatedSources) then
  begin
    try
      Result := TSound.Create;
      FAllocatedSources.Add(Result);
    except
      { If TSound.Create raises ENoMoreOpenALSources ---
        then silence the exception and leave Result = nil. }
      on ENoMoreOpenALSources do ;
    end;
  end;

  { Try: maybe we can remove one more sound ?

    If Result = nil then we know that MinImportanceIndex <> -1, because
    all sounds must be used and MinAllocatedSources is always > 0,
    so some sound must be used.

    Note that if FAllocatedSources[MinImportanceIndex].Importance
    is equal to Importance, we *do* interrupt already playing sound.
    The assumption is here that the newer sound is more imoportant. }
  if (Result = nil) and
     (FAllocatedSources[MinImportanceIndex].Importance <= Importance) then
  begin
    Result := FAllocatedSources[MinImportanceIndex];
  end;

  if Result <> nil then
  begin
    { Prepare Result }
    if Result.Used then
      Result.Release;
    Result.FImportance := Importance;
    Result.FUsed := true;
  end;

  CheckAL('allocating sound source (TSoundAllocator.AllocateSound)');
end;

procedure TSoundAllocator.SetMinAllocatedSources(const Value: Cardinal);
var
  I: Integer;
  OldAllocatedSourcesCount: Cardinal;
begin
  if Value <> FMinAllocatedSources then
  begin
    FMinAllocatedSources := Value;
    if (FAllocatedSources <> nil) and
       (Cardinal(FAllocatedSources.Count) < MinAllocatedSources) then
    begin
      OldAllocatedSourcesCount := FAllocatedSources.Count;
      FAllocatedSources.Count := MinAllocatedSources;
      for I := OldAllocatedSourcesCount to FAllocatedSources.Count - 1 do
        FAllocatedSources[I] := TSound.Create;
    end;
  end;
end;

procedure TSoundAllocator.SetMaxAllocatedSources(const Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FMaxAllocatedSources then
  begin
    FMaxAllocatedSources := Value;
    if (FAllocatedSources <> nil) and
       (Cardinal(FAllocatedSources.Count) > MaxAllocatedSources) then
    begin
      { Refresh is useful here, so that we really cut off
        the *currently* unused sources. }
      Refresh;
      FAllocatedSources.SortByImportance;

      for I := MaxAllocatedSources to FAllocatedSources.Count - 1 do
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        FPGObjectList_FreeAndNilItem(FAllocatedSources, I);
      end;
      FAllocatedSources.Count := MaxAllocatedSources;
    end;
  end;
end;

procedure TSoundAllocator.Refresh;
var
  I: Integer;
begin
  CheckAL('before Refresh');

  if FAllocatedSources <> nil then
    for I := 0 to FAllocatedSources.Count - 1 do
      if FAllocatedSources[I].Used and
         (not FAllocatedSources[I].PlayingOrPaused) then
        FAllocatedSources[I].Release;
end;

procedure TSoundAllocator.StopAllSources;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
    for I := 0 to FAllocatedSources.Count - 1 do
      if FAllocatedSources[I].Used then
        FAllocatedSources[I].Release;
end;

procedure TSoundAllocator.LoadFromConfig(const Config: TCastleConfig);
begin
  MinAllocatedSources := Config.GetValue(
    'sound/allocated_sources/min', DefaultMinAllocatedSources);
  MaxAllocatedSources := Config.GetValue(
    'sound/allocated_sources/max', DefaultMaxAllocatedSources);
end;

procedure TSoundAllocator.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('sound/allocated_sources/min',
    MinAllocatedSources, DefaultMinAllocatedSources);
  Config.SetDeleteValue('sound/allocated_sources/max',
    MaxAllocatedSources, DefaultMaxAllocatedSources);
end;

end.
