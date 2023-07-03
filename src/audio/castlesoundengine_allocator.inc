{%MainUnit castlesoundengine.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type // allow type declaration to continue

  { Manager of allocated sounds.

    For efficiency, the pool of available sound sources (things that are actually
    audible at a given time) is limited.
    This limit comes from the backend limits (like OpenAL or APIs underneath OpenAL),
    it may also come from sound hardware limitations,
    and in general you should not have too many playing sources,
    as mixing many sources is time-consuming.
    So you cannot simply allocate new sound source for each of 100 creatures
    you display in the game.

    This class hides this limitation, by managing a pool of sound sources,
    and returning on demand the next unused sound source (or @nil).
    It can prioritize sound sources,
    it can reuse sound sources that finished playing,
    it can interrupt a lower-priority sound when necessary to play
    a higher-priority sound.

    This is automatically used by higher-level comfortable methods to play
    sounds like @link(TSoundEngine.Play). }
  TSoundAllocator = class
  strict private
    FAllocatedSources: TInternalSoundSourceList;
    FMinAllocatedSources: Cardinal;
    FMaxAllocatedSources: Cardinal;
    LastSoundRefresh: TTimerResult;

    procedure Update(Sender: TObject);
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  private
    Backend: TSoundEngineBackend;
    FIsContextOpenSuccess: boolean;
    procedure ContextOpenCore; virtual;
    procedure ContextCloseCore; virtual;

    { Allocate sound for playing. You should initialize the sound source
      properties and start playing the sound.

      Note that if you don't call TInternalSoundSource.Backend.Play immediately, the source may be detected
      as unused (and recycled for another sound) at the next
      sound allocation, play, update etc.

      If we can't allocate new sound source, we return nil.
      This may happen if your sound context is not initialized.
      It may also happen if we cannot create more sources (because
      we hit MaxAllocatedSources limit, or backend (like OpenAL) just refuses to create
      more sources) and all existing sounds are used and their
      Priority is > given here Priority.

      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Priority of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSound(const Priority: Single): TInternalSoundSource;

    { Stop all the sources currently playing. Especially useful since
      you have to stop a source before releasing it's associated buffer. }
    procedure StopAllSources;
  public
    const
      DefaultMinAllocatedSources = 4;
      DefaultMaxAllocatedSources = 16;

    constructor Create;
    destructor Destroy; override;

    { Minimum / maximum number of allocated sources.
      Always keep MinAllocatedSources <= MaxAllocatedSources.

      For the sake of speed, we always keep allocated at least
      MinAllocatedSources sources. This must be >= 1.
      Setting MinAllocatedSources too large value will raise
      ENoMoreSources.

      At most MaxAllocatedSources sources may be simultaneously used (played).
      This prevents us from allocating too many sounds,
      which would be bad for speed (not to mention that it may
      be impossible under some backends, like OpenAL on Windows).
      When all MaxAllocatedSources sources are playing, the only way
      to play another sound is to use appropriately high @code(Priority)
      to AllocateSound.

      @groupBegin }
    property MinAllocatedSources: Cardinal
      read FMinAllocatedSources write SetMinAllocatedSources
      default DefaultMinAllocatedSources;

    property MaxAllocatedSources: Cardinal
      read FMaxAllocatedSources write SetMaxAllocatedSources
      default DefaultMaxAllocatedSources;
    { @groupEnd }

    { All allocated (not necessarily used) sources.
      Accessing this is useful only for debugging tasks,
      in normal circumstances this is internal.
      This is @nil when ContextOpen was not yet called. }
    property InternalAllocatedSources: TInternalSoundSourceList read FAllocatedSources;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSoundAllocator ---------------------------------------------------------- }

constructor TSoundAllocator.Create;
begin
  inherited;
  FMinAllocatedSources := DefaultMinAllocatedSources;
  FMaxAllocatedSources := DefaultMaxAllocatedSources;

  {$ifdef CASTLE_SOUND_BACKEND_DEFAULT_OPENAL} UseOpenALSoundBackend; {$endif}
end;

destructor TSoundAllocator.Destroy;
begin
  FreeAndNil(Backend);
  inherited;
end;

procedure TSoundAllocator.ContextOpenCore;
var
  I: Integer;
begin
  FAllocatedSources := TInternalSoundSourceList.Create(true);
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.Count - 1 do
    FAllocatedSources[I] := TInternalSoundSource.Create(Backend);

  ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif} Update);
end;

procedure TSoundAllocator.ContextCloseCore;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
  begin
    { Stop using and free allocated sounds. }
    for I := 0 to FAllocatedSources.Count - 1 do
      { Although usually we are sure that every FAllocatedSources[I] <> nil,
        in this case we must take into account that maybe our constructor
        raise ENonMoreSources and so some FAllocatedSources[I] were
        not initialized. }
      if FAllocatedSources[I] <> nil then
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        { This will free FAllocatedSources[I], as FAllocatedSources owns children }
        FAllocatedSources[I] := nil;
      end;

    FreeAndNil(FAllocatedSources);
  end;

  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef FPC}@{$endif} Update);
end;

function TSoundAllocator.AllocateSound(
  const Priority: Single): TInternalSoundSource;
var
  I: Integer;
  MinPriorityIndex: Integer;
begin
  Result := nil;

  { Sound context not initialized yet }
  if FAllocatedSources = nil then Exit;

  { Try: maybe we have already allocated unused sound ?
    If no unused sound will be found, it will calculate
    MinPriorityIndex, this will be useful later. }
  MinPriorityIndex := -1;
  for I := 0 to FAllocatedSources.Count - 1 do
    if not FAllocatedSources[I].Used then
    begin
      Result := FAllocatedSources[I];
      { Breaking here means that MinPriorityIndex will not be calculated
        correctly (because we did not iterate to the end of FAllocatedSources
        list). But that's OK, because if Result <> nil here, then we will
        not need MinPriorityIndex later. }
      Break;
    end else
    begin
      { Update MinPriorityIndex }
      if (MinPriorityIndex = -1) or
         (FAllocatedSources[I].Priority <
          FAllocatedSources[MinPriorityIndex].Priority) then
         MinPriorityIndex := I;
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
      Result := TInternalSoundSource.Create(Backend);
      FAllocatedSources.Add(Result);
    except
      { If TInternalSoundSource.Create raises ENoMoreSources ---
        then silence the exception and leave Result = nil. }
      on ENoMoreSources do ;
    end;
  end;

  { Try: maybe we can remove one more sound ?

    If Result = nil then we know that MinPriorityIndex <> -1, because
    all sounds must be used and MinAllocatedSources is always > 0,
    so some sound must be used.

    Note that if FAllocatedSources[MinPriorityIndex].Priority
    is equal to Priority, we *do* interrupt already playing sound.
    The assumption is here that the newer sound is more imoportant. }
  if (Result = nil) and
     (FAllocatedSources[MinPriorityIndex].Priority <= Priority) then
  begin
    Result := FAllocatedSources[MinPriorityIndex];
  end;

  if Result <> nil then
  begin
    { Prepare Result }
    if Result.Used then
      Result.Release;
    Result.FPriority := Priority;
    Result.FUsed := true;
  end;
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
        FAllocatedSources[I] := TInternalSoundSource.Create(Backend);
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
      FAllocatedSources.SortByPriority;

      for I := MaxAllocatedSources to FAllocatedSources.Count - 1 do
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        { This will free FAllocatedSources[I], as FAllocatedSources owns children }
        FAllocatedSources[I] := nil;
      end;
      FAllocatedSources.Count := MaxAllocatedSources;
    end;
  end;
end;

procedure TSoundAllocator.Update(Sender: TObject);

  { Call Update on all sources, and detect unused sound sources. }
  procedure UpdateSounds(const SecondsPassed: TFloatTime);
  var
    I: Integer;
  begin
    if FAllocatedSources <> nil then
      for I := 0 to FAllocatedSources.Count - 1 do
        FAllocatedSources[I].Update(SecondsPassed);
  end;

const
  { Delay between calling DetectUnusedSounds, in seconds. }
  SoundRefreshDelay = 0.1;
var
  TimeNow: TTimerResult;
  SecondsPassed: TFloatTime;
begin
  if FIsContextOpenSuccess then
    Backend.Update;

  { Calling UpdateSounds relatively often is important,
    to call OnRelease for sound sources that finished playing. }
  if FAllocatedSources <> nil then
  begin
    TimeNow := Timer;
    SecondsPassed := TimerSeconds(TimeNow, LastSoundRefresh);
    if SecondsPassed > SoundRefreshDelay then
    begin
      LastSoundRefresh := TimeNow;
      UpdateSounds(SecondsPassed);
    end;
  end;
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

{$endif read_implementation}
