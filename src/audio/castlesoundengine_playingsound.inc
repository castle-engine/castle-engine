{%MainUnit castlesoundengine.pas}
{
  Copyright 2021-2021 Michalis Kamburelis.

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

  { Controls a sound playback.
    You can pass an instance of it to
    @link(TSoundEngine.Play) or @link(TCastleSoundSource.Play).
    It allows to control sound playback before you start playing (like @link(OnStop))
    and after (like @link(Offset)).

    You can reuse the same instance of TCastlePlayingSound for playback many times,
    but you can start new playback only if the previous one finished.
    IOW, you can pass to @link(TSoundEngine.Play) or @link(TCastleSoundSource.Play)
    only instances when @link(Playing) is @false.
    If you don't want to wait, just create a new instance of TCastlePlayingSound -- they
    are designed to be lightweight, you can create them during runtime.

    Destroying this class always stops the associated sound playback.
    Note that it is a TComponent descendant, so you can make it automatically destroyed
    when some owner TComponent is destroyed.

    You can make your own descendants of this class, to associate any additional information
    with the playback of the sound. }
  TCastlePlayingSound = class(TComponent)
  strict private
    FSound: TCastleSound; //< @nil => not playing (when not playing, it can be nil or non-nil)
    FSource: TInternalSoundSource; //< @nil <=> not playing; we only keep FSource with Used=true
    FFreeOnStop: Boolean;
    FOnStop: TNotifyEvent;
    FLoop: Boolean;
    FInitialOffset: Single;
    FVolume: Single;
    FPitch: Single;
    FPriority: Single;
    procedure SetSound(const Value: TCastleSound);
    function GetOffset: Single;
    procedure SetOffset(const Value: Single);
    procedure SetOnStop(const Value: TNotifyEvent);
    procedure SetFreeOnStop(const Value: Boolean);
    procedure SetLoop(const Value: Boolean);
    procedure SourceRelease(Source: TInternalSoundSource);
    procedure SetVolume(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetPriority(const Value: Single);
    { React to sound parameters changes during playback (when FSource <> nil). }
    procedure SoundChangeDuringPlayback(const Sender: TCastleSound; const Change: TSoundChange);
  private
    { Used internally by TSoundEngine to notify we started playing this, using given ASource. }
    procedure Start(const ASource: TInternalSoundSource);
  protected
    { Do the necessary job after source stopped, due to any reason.
      In this class it handles OnStop and FreeOnStop.

      If you override this: Since FreeOnStop may free the instance,
      you should add your code @italic(before) calling inherited. }
    procedure DoStop; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Associated sound used for playing.
      You have to set it before playing.
      Cannot be changed while playing. }
    property Sound: TCastleSound read FSound write SetSound;

    { Volume, a multiplication of @link(TCastleSound.Volume),
      @link(TCastlePlayingSound.Volume) and @link(TCastleSoundSource.Volume)
      (if the sound is played through @link(TCastleSoundSource)).
      @exclude }
    function InternalFinalVolume: Single; virtual;

    { Pitch, a multiplication of @link(TCastleSound.Pitch),
      @link(TCastlePlayingSound.Pitch) and @link(TCastleSoundSource.Pitch)
      (if the sound is played through @link(TCastleSoundSource)).
      @exclude }
    function InternalFinalPitch: Single; virtual;

    { Priority, a multiplication of @link(TCastleSound.Priority),
      @link(TCastlePlayingSound.Priority) and @link(TCastleSoundSource.Priority)
      (if the sound is played through @link(TCastleSoundSource)).
      @exclude }
    function InternalFinalPriority: Single; virtual;

    { If provided, we will call this event when sound stops playing.

      Note that in some cases the sound cannot be played at all.
      For example when TCastleSound is not loaded (no TCastleSound.URL provided,
      or invalid TCastleSound.URL provided) or when there are no free sound sources
      (these are limited in sound backends; the new sound may "steal" some sound source
      but only if the @link(TCastleSound.Priority) is high enough).
      Then @link(TSoundEngine.Play) or @link(TCastleSoundSource.Play)
      call the OnStop event immediately, from within Play call.

      If the object owning the notification method may be destroyed before
      TCastlePlayingSound is destroyed, be sure to set this callback to @nil.
      Otherwise the callback could try calling method of a non-existent object.

      You can only set this when sound is not playing
      (as otherwise it would be an easy source of bugs --
      sometimes @link(TSoundEngine.Play) wants to immediately stop sound playback). }
    property OnStop: TNotifyEvent read FOnStop write SetOnStop;

    { Automatically free when the playback finishes.
      If the playback is not yet started, it will wait for it to start, and then finish.

      You can only set this when sound is not playing
      (as otherwise it would be an easy source of bugs --
      sometimes @link(TSoundEngine.Play) wants to immediately stop sound playback). }
    property FreeOnStop: Boolean read FFreeOnStop write SetFreeOnStop default false;

    { Is the playback looping.
      You can change this on a playing or stopped sound. }
    property Loop: Boolean read FLoop write SetLoop default false;

    { Is the sound playing now.
      Using @link(TSoundEngine.Play) or @link(TCastleSoundSource.Play)
      makes it @true for some time, then (right before OnStop) it switches back to @false. }
    function Playing: Boolean;

    { Volume (how loud the playing sound is).

      The effective sound volume is a multiplication of @link(TCastleSound.Volume),
      @link(TCastlePlayingSound.Volume) and @link(TCastleSoundSource.Volume)
      (if the sound is played through @link(TCastleSoundSource)).
      It is also affected by spatial calculations (if the sound is played
      through @link(TCastleSoundSource) with @link(TCastleSoundSource.Spatial) = @true)
      and finally by a master @link(TSoundEngine.Volume SoundEngine.Volume).

      Any value > 0 is allowed. }
    property Volume: Single read FVolume write SetVolume {$ifdef FPC}default 1.0{$endif};

    { Sound playing speed.

      The effective sound pitch is a multiplication of @link(TCastleSound.Pitch),
      @link(TCastlePlayingSound.Pitch) and @link(TCastleSoundSource.Pitch)
      (if the sound is played through @link(TCastleSoundSource)).

      Any value > 0 is allowed. }
    property Pitch: Single read FPitch write SetPitch {$ifdef FPC}default 1.0{$endif};

    { How important is the sound, between 0.0 (least important) to 1.0 (most important).

      The effective priority is a multiplication of @link(TCastleSound.Priority),
      @link(TCastlePlayingSound.Priority) and @link(TCastleSoundSource.Priority)
      (if the sound is played through @link(TCastleSoundSource)). }
     property Priority: Single read FPriority write SetPriority {$ifdef FPC}default 1.0{$endif};

    { Playback time of this sound, expressed in seconds.

      This value will loop back to zero for looping sound sources.
      Setting this to something larger than the sound duration
      (which you can read from @link(TCastleSound.Duration)) is ignored.

      This offset refers to the sound like it had a @link(TCastleSound.Pitch Pitch) equal 1.0
      (when the sound is not slowed down or sped up).
      So this offset will vary from 0 to the @link(TCastleSound.Duration),
      regardless of the initial/current @link(TCastleSound.Pitch) value.
      The @italic(actual) seconds passed since the sound started
      playing may be different, if you change the @link(TCastleSound.Pitch)
      to something else than 1.0.

      Using this on a sound that is not playing is allowed.
      Getting this value always returns zero in this case, and setting it is ignored.

      Note: you can adjust InitialOffset to determine the offset for starting new sound.
      It is deliberately a separate property from current @link(Offset),
      otherwise it would be too easy to accidentally start playing in the middle when
      you wanted to play from the start. }
    property Offset: Single read GetOffset write SetOffset;

    { Initial position, in seconds, within the sound when we start playing it.
      You can get/set this property regardless of whether sound is playing. }
    property InitialOffset: Single read FInitialOffset write FInitialOffset;

    { Associated sound source, to change parameters of this sound at runtime.
      @nil if not playing.
      @exclude }
    property InternalSource: TInternalSoundSource read FSource;

    { Stop playing now. Note that you can also free the TCastlePlayingSound instance,
      it will always automatically stop the playback.

      Calling this on a sound that is not playing is allowed, and ignored. }
    procedure Stop;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Design note: The TCastlePlayingSound class is somewhat similar in features
  to TInternalSoundSource.
  In fact we used to expose TInternalSoundSource under deprecated TSound name
  for a very similar use-cases that TCastlePlayingSound now handles.

  However the lifetime of TInternalSoundSource was less under user control.
  It generally existed for a long time, unless you decreased SoundEngine.MaxAllocatedSources.
  TCastlePlayingSoundXxx has clearer lifetime:
  controlled by user, has FreeOnStop, stops automatically when freed. }

constructor TCastlePlayingSound.Create(AOwner: TComponent);
begin
  inherited;
  FVolume := 1.0;
  FPitch := 1.0;
  FPriority := 1.0;
end;

destructor TCastlePlayingSound.Destroy;
begin
  FFreeOnStop := false; // we are already freeing, don't let DoStop call destructor recursively
  Stop;
  inherited;
end;

procedure TCastlePlayingSound.SetSound(const Value: TCastleSound);
begin
  if FSound <> Value then
  begin
    if Playing then
      raise Exception.Create('Cannot change TCastlePlayingSound.Sound during playback');
    FSound := Value;
  end;
end;

function TCastlePlayingSound.GetOffset: Single;
begin
  if FSource <> nil then
    Result := FSource.Offset
  else
    Result := 0;
end;

procedure TCastlePlayingSound.SetOffset(const Value: Single);
begin
  if FSource <> nil then
    FSource.Offset := Value;
end;

function TCastlePlayingSound.Playing: Boolean;
begin
  Result := FSource <> nil;
end;

procedure TCastlePlayingSound.SetOnStop(const Value: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnStop), TMethod(Value)) then
  begin
    if Playing then
      raise Exception.Create('Cannot change TCastlePlayingSound.OnStop during playback');
    FOnStop := Value;
  end;
end;

procedure TCastlePlayingSound.SetFreeOnStop(const Value: Boolean);
begin
  if FFreeOnStop <> Value then
  begin
    if Playing then
      raise Exception.Create('Cannot change TCastlePlayingSound.FreeOnStop during playback');
    FFreeOnStop := Value;
  end;
end;

procedure TCastlePlayingSound.SetLoop(const Value: Boolean);
begin
  if FLoop <> Value then
  begin
    FLoop := Value;
    if FSource <> nil then
      FSource.Loop := Value;
  end;
end;

procedure TCastlePlayingSound.Stop;
begin
  if FSource <> nil then
  begin
    Assert(FSource.Used); // we only keep FSource with Used=true
    FSource.Release; // note: this can destroy our instance (if FreeOnStop)
    { The SourceRelease should already set FSource to nil
      (but we cannnot make Assert below, as we possibly no longer exist after
      FSource.Release, due to FreeOnStop; testcase: castle1 tower level, let ball missile
      explode on player). }
    // Assert(FSource = nil);
  end;
end;

procedure TCastlePlayingSound.Start(const ASource: TInternalSoundSource);
begin
  if FSource <> nil then
    raise Exception.Create('Cannot start new TCastlePlayingSound playback before previous one finished');
  Assert(FSound <> nil);
  Assert(ASource <> nil);
  Assert(ASource.Buffer = FSound.InternalBuffer);
  FSource := ASource;
  FSource.OnRelease := {$ifdef FPC}@{$endif}SourceRelease;

  FSound.InternalAddChangeNotification({$ifdef FPC}@{$endif}SoundChangeDuringPlayback);
end;

procedure TCastlePlayingSound.SourceRelease(Source: TInternalSoundSource);
begin
  // FSound must always exist during playback, since we stop playback when FSound is freed.
  FSound.InternalRemoveChangeNotification({$ifdef FPC}@{$endif}SoundChangeDuringPlayback);

  Assert(Source = FSource); // we should only ever get callback about current FSource
  Source.OnRelease := nil;
  FSource := nil;
  DoStop; // note: this can destroy our instance (if FreeOnStop)
end;

procedure TCastlePlayingSound.DoStop;
begin
  if Assigned(OnStop) then
    OnStop(Self);
  if FreeOnStop then
    Self.Destroy;
end;

procedure TCastlePlayingSound.SoundChangeDuringPlayback(const Sender: TCastleSound; const Change: TSoundChange);
begin
  Assert(FSource <> nil);
  Assert(Sender = FSound);

  case Change of
    scAfterOther:
      begin
        FSource.Volume := InternalFinalVolume;
        FSource.Pitch  := InternalFinalPitch;
        FSource.Priority := InternalFinalPriority;
        {$warnings off} // using unportable symbols knowingly
        FSource.MinGain := FSound.MinGain;
        FSource.MaxGain := FSound.MaxGain;
        {$warnings on}
        FSource.ReferenceDistance := FSound.ReferenceDistance;
        FSource.MaxDistance       := FSound.MaxDistance;
      end;
    scBeforeBufferDestroyed:
      Stop;
    else ;
  end;
end;

function TCastlePlayingSound.InternalFinalVolume: Single;
begin
  Result := FVolume;
  if Sound <> nil then
    Result := Result * Sound.Volume;
end;

function TCastlePlayingSound.InternalFinalPitch: Single;
begin
  Result := FPitch;
  if Sound <> nil then
    Result := Result * Sound.Pitch;
end;

function TCastlePlayingSound.InternalFinalPriority: Single;
begin
  Result := FPriority;
  if Sound <> nil then
    Result := Result * Sound.Priority;
end;

procedure TCastlePlayingSound.SetVolume(const Value: Single);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    // as InternalFinalVolume changed, update playback (InternalSource) properties
    if FSource <> nil then
      SoundChangeDuringPlayback(FSound, scAfterOther);
  end;
end;

procedure TCastlePlayingSound.SetPitch(const Value: Single);
begin
  if FPitch <> Value then
  begin
    FPitch := Value;
    // as InternalFinalPitch changed, update playback (InternalSource) properties
    if FSource <> nil then
      SoundChangeDuringPlayback(FSound, scAfterOther);
  end;
end;

procedure TCastlePlayingSound.SetPriority(const Value: Single);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    // as InternalFinalPriority changed, update playback (InternalSource) properties
    if FSource <> nil then
      SoundChangeDuringPlayback(FSound, scAfterOther);
  end;
end;

{$endif read_implementation}
