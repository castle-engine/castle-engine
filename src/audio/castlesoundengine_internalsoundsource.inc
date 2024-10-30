{%MainUnit castlesoundengine.pas}
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

{$ifdef read_interface}

//type // allow type declaration to continue

  TInternalSoundSourceEvent = procedure (Sender: TInternalSoundSource) of object;

  { Sound source that is allocated (on the sound backend) and can be immediately played.
    One instance of TInternalSoundSource is is not permanently connected
    to a particular use (like a particular TCastleSound), it is only temporarily allocated
    for given use (e.g. play a particular TCastleSound), and then it becomes "unused"
    (see @link(Used), @link(OnRelease)) and can be used for other purposes.

    This is internal now. Use TCastleSound and TCastleSoundSource
    to manage playback of a sound.

    @exclude
  }
  TInternalSoundSource = class
  private
    FUsed: boolean;
    FOnRelease: TInternalSoundSourceEvent;
    FPriority: Single;
    FUserData: TObject;
    FPosition, FVelocity: TVector3;
    FLoop, FSpatial: boolean;
    FVolume, FMinGain, FMaxGain, FPitch: Single;
    FBuffer: TInternalSoundBuffer;
    FReferenceDistance, FMaxDistance: Single;
    Backend: TSoundSourceBackend;
    BackendIsOpen: Boolean;
    procedure SetPosition(const Value: TVector3);
    procedure SetVelocity(const Value: TVector3);
    procedure SetLoop(const Value: boolean);
    procedure SetSpatial(const Value: boolean);
    procedure SetVolume(const Value: Single);
    procedure SetMinGain(const Value: Single);
    procedure SetMaxGain(const Value: Single);
    procedure SetBuffer(const Value: TInternalSoundBuffer);
    procedure SetPitch(const Value: Single);
    procedure SetReferenceDistance(const Value: Single);
    procedure SetMaxDistance(const Value: Single);
    procedure SetPriority(const Value: Single);
    function GetOffset: Single;
    procedure SetOffset(const Value: Single);

    { Call backend Update on sound source,
      and checks whether the source is still playing (or is paused).
      If not, it calls @link(TInternalSoundSource.Release) (thus setting
      TInternalSoundSource.Used to @false and triggering TSound.OnRelease) for this source. }
    procedure Update(const SecondsPassed: TFloatTime);
  public
    { Create sound.
      This allocates sound source using the sound backend (like OpenAL source).
      @raises(ENoMoreSources If no more sources available.
        It should be caught and silenced by TSoundAllocator.AllocateSound.) }
    constructor Create(const SoundEngineBackend: TSoundEngineBackend);
    destructor Destroy; override;

    { Do we play something.
      Sources that are not Used are still allocated on the sound backend (like
      OpenAL), and will be used when we will need them. }
    property Used: boolean read FUsed default false;

    { The priority of keeping this source, relevant only when @link(Used).
      See TCastleSound.Priority for the exact meaning and range. }
    property Priority: Single read FPriority write SetPriority {$ifdef FPC}default 0.0{$endif};

    { Any data comfortable to keep here by the caller of
      TSoundAllocator.AllocateSound. It should be initialized
      after calling TSoundAllocator.AllocateSound, and should
      be finalized in OnRelease. }
    property UserData: TObject read FUserData write FUserData;

    { Called when this sound will no longer be used.
      This may happen because it simply finished playing,
      or when there are more demanding
      sounds (see @link(Priority) and to keep MaxAllocatedSources)
      and we must use this sound source for something else.

      When this event occurs, you should forget (e.g. set to @nil) all
      your references to this sound instance. That's because this TInternalSoundSource instance
      may be freed (or reused for other sounds) after calling OnRelease.
      For the same reason, right after calling this event, we always clear it
      (set OnRelease to @nil).

      It's guaranteed that when this will be called,
      @link(Used) will be @false and @link(PlayingOrPaused) will be @false.

      Note that we do not guarantee that sources that
      stopped playing will be immediately reported to OnRelease.
      A source may have Used = @true state
      for a short time when it stopped playing (when PlayingOrPaused
      is already @false). }
    property OnRelease: TInternalSoundSourceEvent read FOnRelease write FOnRelease;

    { Stops playing the source,
      sets Used to @false, and calls OnRelease (if assigned).

      You can call this if you want to stop playing the sound.
      This will also immediately set Used property
      to @false and will call OnRelease.

      You can call this only when Used = @true. }
    procedure Release; virtual;

    property Position: TVector3 read FPosition write SetPosition;
    property Velocity: TVector3 read FVelocity write SetVelocity;
    property Loop: boolean read FLoop write SetLoop;
    property Spatial: boolean read FSpatial write SetSpatial;
    property Volume: Single read FVolume write SetVolume;
    property MinGain: Single read FMinGain write SetMinGain;
    property MaxGain: Single read FMaxGain write SetMaxGain;
    property Buffer: TInternalSoundBuffer read FBuffer write SetBuffer;
    property Pitch: Single read FPitch write SetPitch;
    property ReferenceDistance: Single read FReferenceDistance write SetReferenceDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { Playback time of this sound, expressed in seconds.

      This value will loop back to zero for looping sound sources.
      Setting this to something larger than the @italic(sound buffer duration)
      is ignored.

      This offset refers to the sound like it had a @link(Pitch) equal 1.0
      (when the sound is not slowed down or sped up).
      So this offset will vary from 0 to the @italic(sound buffer duration),
      regardless of the current @link(Pitch) value.
      The @italic(actual) seconds passed since the sound started
      playing may be different, if you will change the @link(Pitch)
      to something else than 1.0.

      Setting this on a not-yet playing sound source
      (this is done by @link(TSoundEngine.PlaySound))
      causes the sound to start playing from that offset. }
    property Offset: Single read GetOffset write SetOffset;

    { Is the sound playing or paused. This is almost always @true for sounds
      returned by TSoundAllocator.AllocateSound, when it stops being @true
      --- the sound engine will realize it (soon), which will cause @link(Release)
      and OnRelease being automatically called, and this TInternalSoundSource may then
      be reused for playing other sounds. }
    function PlayingOrPaused: boolean;

    { Make sure that the sound keeps playing, in case it stopped playing.

      This is an alternative approach to play a sound many times,
      like in a loop, but without using the @link(Loop) property.
      The idea is that you leave @link(Loop) set to @false,
      and you keep calling this method from some "update" event
      (like some @link(TCastleUserInterface.Update) implementation).
      Once you stop calling this method, the sound will automatically stop
      (once it finishes the current cycle).

      Note that you still (as always when using TInternalSoundSource) must observe
      the @link(TInternalSoundSource.OnRelease). When it's called, it means that the sound
      engine (TSoundEngine) decided that this sound should be used for other purposes
      (there's also a very small chance that the sound engine "caught"
      the sound as unused, in a short time when it stopped playing but you didn't
      yet call this method).
      In such case, you must stop doing anything with this TInternalSoundSource instance
      (including calling this method, @name, on it).
      You have to start playing the sound again by @link(TSoundEngine.PlaySound)
      instead.

      Note that calling this method is better than observing @link(TInternalSoundSource.OnRelease),
      to start playing a new sound when the previous one stopped.
      That's because @link(TInternalSoundSource.OnRelease) may be called with some small delay
      after the sound actually stopped, and it may be noticeable (e.g. in case
      of using this for a short rhytmic sound, like footsteps). }
    procedure KeepPlaying; deprecated 'use Loop to play looping sound. This method is a hack -- as you shall not operate on stopped TInternalSoundSource, as it may be repurposed.';
  end;

  TSound = TInternalSoundSource deprecated 'play sounds using TCastleSoundSource + TCastleSound';

  TInternalSoundSourceList = class({$ifdef FPC}specialize{$endif} TObjectList<TInternalSoundSource>)
  public
    { Sort sounds by Used + Priority, descending.
      First all sounds with Used = @true are placed,
      starting from the sound with largest Priority, and so on
      until the sound with smallest Priority.
      Then all sounds with Used = @false are placed (in any, arbitrary order).

      List must not contain nil values when calling this. }
    procedure SortByPriority;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TInternalSoundSource ---------------------------------------------------------- }

constructor TInternalSoundSource.Create(const SoundEngineBackend: TSoundEngineBackend);
begin
  inherited Create;

  Backend := SoundEngineBackend.CreateSource;
  { For now, TInternalSoundSource always refers to an open (on backend side) sound source. }
  Backend.ContextOpen;
  { This way, TInternalSoundSource.Destroy will not call Backend.ContextClose
    if Backend.ContextOpen failed.
    (So e.g. OpenAL Backend.ContextClose doesn't try to call
    alDeleteSources on invalid sound source.) }
  BackendIsOpen := true;
end;

destructor TInternalSoundSource.Destroy;
begin
  if (Backend <> nil) and BackendIsOpen then
    Backend.ContextClose;
  FreeAndNil(Backend);
  inherited;
end;

procedure TInternalSoundSource.Release;
var
  SavedOnRelease: TInternalSoundSourceEvent;
begin
  FUsed := false;

  Backend.Stop;

  { Detach the buffer from source. Otherwise we couldn't free the buffer
    while it's associated with the source. Also, this would be a problem
    once we implement streaming on some sources: you have to reset
    buffer to 0 before queing buffers on source. }
  Buffer := nil;

  if Assigned(OnRelease) then
  begin
    { Note that calling OnRelease may do anything, it may even reuse this sound
      and set the OnRelease again. This is possible when using TCastlePlayingSound:
      if in OnStop of one TCastlePlayingSound, you will start playing another
      TCastlePlayingSound, it may reuse the same TInternalSoundSource.

      So *do not* do "OnRelease := nil" after calling OnRelease, do it before.
      That's why we need to keep it in SavedOnRelease.

      Testcase: https://github.com/castle-engine/castle-engine/issues/344 }
    SavedOnRelease := OnRelease;
    OnRelease := nil;
    SavedOnRelease(Self);
  end;
end;

procedure TInternalSoundSource.SetPosition(const Value: TVector3);
begin
  FPosition := Value;
  Backend.SetPosition(Value);
end;

procedure TInternalSoundSource.SetVelocity(const Value: TVector3);
begin
  FVelocity := Value;
  Backend.SetVelocity(Value);
end;

procedure TInternalSoundSource.SetLoop(const Value: boolean);
begin
  FLoop := Value;
  Backend.SetLoop(Value);
end;

procedure TInternalSoundSource.SetSpatial(const Value: boolean);
begin
  FSpatial := Value;
  Backend.SetSpatial(Value);
end;

procedure TInternalSoundSource.SetVolume(const Value: Single);
begin
  FVolume := Value;
  Backend.SetVolume(Value);
end;

procedure TInternalSoundSource.SetMinGain(const Value: Single);
begin
  FMinGain := Value;
  Backend.SetMinGain(Value);
end;

procedure TInternalSoundSource.SetMaxGain(const Value: Single);
begin
  FMaxGain := Value;
  Backend.SetMaxGain(Value);
end;

procedure TInternalSoundSource.SetBuffer(const Value: TInternalSoundBuffer);
begin
  FBuffer := Value;
  if (Value <> nil) and Value.BackendIsOpen then
    Backend.SetBuffer(Value.Backend)
  else
    Backend.SetBuffer(nil);
end;

procedure TInternalSoundSource.SetPitch(const Value: Single);
begin
  FPitch := Value;
  Backend.SetPitch(Value);
end;

procedure TInternalSoundSource.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;
  Backend.SetReferenceDistance(Value);
end;

procedure TInternalSoundSource.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  Backend.SetMaxDistance(Value);
end;

procedure TInternalSoundSource.SetPriority(const Value: Single);
begin
  FPriority := Value;
  Backend.SetPriority(Value);
end;

function TInternalSoundSource.GetOffset: Single;
begin
  Result := Backend.Offset;
end;

procedure TInternalSoundSource.SetOffset(const Value: Single);
begin
  Backend.Offset := Value;
end;

function TInternalSoundSource.PlayingOrPaused: boolean;
begin
  Result := Backend.PlayingOrPaused;
end;

procedure TInternalSoundSource.KeepPlaying;
begin
  if not PlayingOrPaused then
    Backend.Play(false);
end;

procedure TInternalSoundSource.Update(const SecondsPassed: TFloatTime);
begin
  if Used then
  begin
    if not BackendIsOpen then
      Release
    else
    begin
      Backend.Update(SecondsPassed);
      if not PlayingOrPaused then
        Release;
    end;
  end;
end;

{ TInternalSoundSourceList ----------------------------------------------------- }

function IsSmallerByPriority({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AA, BB: TInternalSoundSource): Integer;
begin
  if (AA.Used and (not BB.Used)) or
     (AA.Used and BB.Used and (AA.Priority > BB.Priority)) then
    Result := -1 else
  if (BB.Used and (not AA.Used)) or
     (BB.Used and AA.Used and (BB.Priority > AA.Priority)) then
    Result :=  1 else
    Result :=  0;
end;

procedure TInternalSoundSourceList.SortByPriority;
type
  TInternalSoundSourceComparer = {$ifdef FPC}specialize{$endif} TComparer<TInternalSoundSource>;
begin
  Sort(TInternalSoundSourceComparer.Construct(
    {$ifdef FPC}@{$endif} IsSmallerByPriority));
end;

{$endif read_implementation}
