{%MainUnit castlesoundengine.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

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

  { @exclude Used with TCastleSound notifications (TCastleSound.InternalAddChangeNotification). }
  TSoundChange = (
    { TCastleSound.Buffer, and maybe other values, changed. }
    scAfterBufferCreated,
    { TCastleSound.Buffer may be destroyed now, stop related sounds. }
    scBeforeBufferDestroyed,
    { TCastleSound values, but not Buffer, changed. }
    scAfterOther
  );

  TCastleSound = class;

  { @exclude Used with TCastleSound notifications (TCastleSound.InternalAddChangeNotification). }
  TCastleSoundChangeEvent = procedure (const Sender: TCastleSound; const Change: TSoundChange) of object;

  { @exclude Used with TCastleSound notifications (TCastleSound.InternalAddChangeNotification). }
  TCastleSoundChangeEventList = class({$ifdef FPC}specialize{$endif} TList<TCastleSoundChangeEvent>)
  public
    procedure ExecuteAll(const Sender: TCastleSound; const Change: TSoundChange);
  end;

  { Sound that can be loaded from URL (possibly reusing a cache,
    possibly using streaming) and played.

    Aside from sound file (also called "sound buffer" or "audio clip" in various APIs)
    this class allows to configue also playback values, like @link(Volume) and @link(Pitch).
    These playback values are intended to reflect preferences specific to this sound file.
    They are multiplied by playback values of the @link(TCastleSoundSource)
    (@link(TCastleSoundSource.Volume), @link(TCastleSoundSource.Pitch))
    and @link(TCastlePlayingSound) (to adjust it per-playback, using
    @link(TCastlePlayingSound.Volume), @link(TCastlePlayingSound.Pitch)).

    This class does not actually play the sound.
    It only defines something that @italic(can) be played.
    To play the sound use @link(TSoundEngine.Play SoundEngine.Play) or @link(TCastleSoundSource).
    For spatial sounds, attach @link(TCastleSoundSource) to a @link(TCastleTransform) within some
    @link(TCastleViewport) and set @link(TCastleSoundSource.Sound)
    or call @link(TCastleSoundSource.Play). }
  TCastleSound = class(TCastleComponent)
  strict private
    FURL: string;
    FBuffer: TInternalSoundBuffer;
    FStream: Boolean;
    FVolume: Single;
    FPitch: Single;
    FMinGain: Single;
    FMaxGain: Single;
    FPriority: Single;
    FReferenceDistance, FMaxDistance: Single;
    FChangeNotifications: TCastleSoundChangeEventList;
    procedure SetStream(const Value: Boolean);
    procedure SetURL(const Value: String);
    procedure SetVolume(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetMinGain(const Value: Single);
    procedure SetMaxGain(const Value: Single);
    procedure SetPriority(const Value: Single);
    procedure SetReferenceDistance(const Value: Single);
    procedure SetMaxDistance(const Value: Single);
    procedure ReloadBuffer;
    procedure DoChange(const Change: TSoundChange);
  protected
    procedure Loaded; override;
  public
    const
      DefaultReferenceDistance = 1.0;
      DefaultMaxDistance = 10000.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { @exclude Add notification when sound property changes. }
    procedure InternalAddChangeNotification(const Notify: TCastleSoundChangeEvent);
    { @exclude Remove notification when sound property occurs. }
    procedure InternalRemoveChangeNotification(const Notify: TCastleSoundChangeEvent);

    { Load sound contents from given TStream.

      @param(AStream Stream to load.
        When loading, we rewind stream positon 0 zero,
        and read the whole stream.)

      @param(MimeType Valid MIME type that indicates a sound file type,
        like @code(audio/x-wav) or @code(audio/ogg).
        See TUrlFile.MimeType implementation for currently supported
        MIME file types.) }
    procedure LoadFromStream(const AStream: TStream; const MimeType: String);

    { Loaded sound buffer, for playing the buffer with the @link(SoundEngine).
      @nil if not loaded.
      @exclude }
    property InternalBuffer: TInternalSoundBuffer read FBuffer;

    { Force a minimum sound loudness, despite what volume would
      be calculated by the spatialization. This can be used to force sound
      to be audible, even when it's far away from the listener.

      It must be in [0, 1] range. By default it is 0. }
    property MinGain: Single read FMinGain write SetMinGain {$ifdef FPC}default 0.0; platform { this feature is only available with OpenAL sound backend }{$endif};

    { Force a maximum sound loudness, despite what volume would
      be calculated by the spatialization. This can be used to limit sound volume,
      regardless of the distance attenuation calculation.

      It must be in [0, 1] range. By default it is 1. }
    property MaxGain: Single read FMaxGain write SetMaxGain {$ifdef FPC}default 1.0; platform { this feature is only available with OpenAL sound backend }{$endif};

    { Duration of the sound, in seconds. -1 if not loaded yet.
      The sound is loaded once you set @link(URL) and don't get any loading exceptions. }
    function Duration: Single;

    { Sound data format (disregarding compression on top of it), for informational purposes.
      Undefined if not loaded yet.
      The sound is loaded once you set @link(URL) and don't get any loading exceptions. }
    function DataFormat: TSoundDataFormat;

    { Frequency (sample rate) of the loaded sound file.
      0 if not loaded yet.
      The sound is loaded once you set @link(URL) and don't get any loading exceptions. }
    function Frequency: LongWord;
  published
    { URL of the sound file.
      Set this to load a new sound buffer, you can set to '' to clear the sound buffer.

      If you plan to change @link(Stream), note that it is best to do it before
      setting @link(URL). Changing @link(Stream) while @link(URL) is already set
      means that the sound is reloaded. }
    property URL: String read FURL write SetURL;

    { Play sound using streaming. This means that the sound is gradually decompressed in memory,
      which means that loading time is much smaller,
      although there may be a small overhead on CPU during playback.
      Streaming is usually a good idea for longer sounds, e.g. music tracks.

      See also @link(TSoundLoading) for details.

      If you plan to change @link(Stream), note that it is best to do it before
      setting @link(URL). Changing @link(Stream) while @link(URL) is already set
      means that the sound is reloaded. }
    property Stream: Boolean read FStream write SetStream default false;

    { Volume (how loud the sound is).

      Use this to indicate that e.g. a plane engine is louder than a mouse squeak
      (when heard from the same distance).
      Note: Do not make the actual sound data (in sound files)
      louder/more silent to express that something is louder/more silent in reality.
      This would usually lower the sound quality.
      Instead, keep your sound file data at max loudness ("normalized"), and use
      this property to adjust the volume associated with the sound.

      The effective sound volume is a multiplication of @link(TCastleSound.Volume),
      @link(TCastlePlayingSound.Volume) and @link(TCastleSoundSource.Volume)
      (if the sound is played through @link(TCastleSoundSource)).
      It is also affected by spatial calculations (if the sound is played
      through @link(TCastleSoundSource) with @link(TCastleSoundSource.Spatial) = @true)
      and finally by a master @link(TSoundEngine.Volume SoundEngine.Volume).

      Any value > 0 is allowed. The default is 1. Note that,
      while values > 1 are allowed,
      but some sound backends (like OpenAL) may clip the resulting sound volume
      (after all volume influences, including spatial calculations are done) to 1.0. }
    property Volume: Single read FVolume write SetVolume {$ifdef FPC}default 1.0{$endif};

    { Sound playing speed.

      The effective sound pitch is a multiplication of @link(TCastleSound.Pitch),
      @link(TCastlePlayingSound.Pitch) and @link(TCastleSoundSource.Pitch)
      (if the sound is played through @link(TCastleSoundSource)).

      Changing this naturally also changes the audible sound.
      Each reduction by 50 percent equals a pitchshift of -12 semitones (one octave reduction).
      Each doubling equals a pitch shift of 12 semitones (one octave increase).

      Any value > 0 is allowed. }
    property Pitch: Single read FPitch write SetPitch {$ifdef FPC}default 1.0{$endif};

    { How important is the sound, between 0.0 (least important) to 1.0 (most important).

      The sound engine sometimes has to interrupt (or even not start at all)
      playback of some non-important sound, to have a good performance of audio mixing.
      In these cases, the sound with higher priority always "wins" over sound with lower
      priority. That is, sound with lower priority will be interrupted (the slot will be "stolen")
      to make room for sound with higher priority.

      Note: Many sound APIs (Unity, FMOD, Wwise, X3D...) have a similar property,
      with inconsistent range (minimum, maximum) and inconsistent meaning (some say "lower is more important",
      some say "higher is more important"). Our treatment of this property is similar to X3D,
      as it satisfies a number of important points: it defines easy to remember min/max (0 and 1),
      it is agnostic to the underlying precision (e.g. it has to map to 257 levels in FMOD,
      100 levels in Wwise). Also, "higher is more important" seems just more natural. }
    property Priority: Single read FPriority write SetPriority {$ifdef FPC}default 0.5{$endif};

    { Spatial sound is fully audible (with @link(Volume)) at this distance.
      See TSoundDistanceModel for a description how distance affects the sound volume. }
    property ReferenceDistance: Single read FReferenceDistance write SetReferenceDistance
      {$ifdef FPC}default DefaultReferenceDistance{$endif};

    { Distance at which sound volume stops attenuating.
      For the linear distance model (@link(TSoundEngine.DistanceModel SoundEngine.DistanceModel) = dmLinear)
      this is also a distance at which sound volume drops to zero (completely silent).
      See TSoundDistanceModel for a description how distance affects the sound volume. }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance
      {$ifdef FPC}default DefaultMaxDistance{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSoundChangeEventList ------------------------------------------------ }

procedure TCastleSoundChangeEventList.ExecuteAll(const Sender: TCastleSound; const Change: TSoundChange);
var
  I: Integer;
begin
  { We do this weird inverted "for" with extra condition, to work even when a notification
    may remove its own listener from the list. This can actually happen:
    in case of scBeforeBufferDestroyed, it notifies TCastlePlayingSound,
    and it will stop the sound, removing itself from notifications list.

    Testcase: examples/audio/game_3d_sound/ and exit while many "kaboom" sounds are playing. }

  for I := Count - 1 downto 0 do
    if I < Count then
       Items[I](Sender, Change);
end;

{ TCastleSound --------------------------------------------------------------- }

constructor TCastleSound.Create(AOwner: TComponent);
begin
  inherited;
  FStream := false;
  FVolume := 1.0;
  FPitch := 1.0;
  FMinGain := 0.0;
  FMaxGain := 1.0;
  FPriority := 0.5;
  FReferenceDistance := DefaultReferenceDistance;
  FMaxDistance := DefaultMaxDistance;
  FChangeNotifications := TCastleSoundChangeEventList.Create;
end;

destructor TCastleSound.Destroy;
begin
  { Just like in ReloadBuffer, we need to call scBeforeBufferDestroyed notification.
    This allows related TCastlePlayingSound to stop playback automatically
    when TCastleSound is destroyed. }
  DoChange(scBeforeBufferDestroyed);

  { Use InternalSoundEngine instead of SoundEngine to avoid creating
    2nd SoundEngine instance, if this would be called at finalization
    (after 1st singleton SoundEngine destroyed). }
  if InternalSoundEngine <> nil then
  begin
    {$warnings off} // using deprecated that should be internal
    InternalSoundEngine.FreeBuffer(FBuffer); // this sets FBuffer = nil
    {$warnings on}
  end;

  FreeAndNil(FChangeNotifications);
  inherited;
end;

function TCastleSound.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'URL') or
     (PropertyName = 'Stream') or
     (PropertyName = 'Volume') or
     (PropertyName = 'Pitch') or
     (PropertyName = 'Priority') or
     (PropertyName = 'ReferenceDistance') or
     (PropertyName = 'MaxDistance') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSound.ReloadBuffer;
var
  SoundLoading: TSoundLoading;
begin
  if FBuffer <> nil then // check nil to avoid accessing SoundEngine if not needed
  begin
    { We first make scBeforeBufferDestroyed notification, so that sound sources playing
      this TCastleSound can be stopped.
      OpenAL makes warning if trying to free buffer used by playing sources. }
    DoChange(scBeforeBufferDestroyed);
    {$warnings off} // using deprecated that should be internal
    SoundEngine.FreeBuffer(FBuffer); // this sets FBuffer = nil
    {$warnings on}
  end;

  if URL <> '' then
  begin
    if Stream then
      SoundLoading := slStreaming
    else
      SoundLoading := slComplete;
    {$warnings off} // using deprecated that should be internal
    { Using ExceptionOnError = false. Because
      - The programs can generally tolerate missing sound files, and continue just fine.
      - Error at OggVorbis reading may be caused by
        EOggVorbisMissingLibraryError, and we try to gracefully react to missing
        libraries, as it is easy to miss installation of some libs on some systems
        (like Linux) or package with appropriate DLLs (like Windows). }
    FBuffer := SoundEngine.LoadBuffer(URL, SoundLoading, false);
    {$warnings on}
    DoChange(scAfterBufferCreated);
  end;
end;

procedure TCastleSound.Loaded;
begin
  inherited;
  ReloadBuffer;
end;

procedure TCastleSound.SetStream(const Value: Boolean);
begin
  if FStream <> Value then
  begin
    FStream := Value;
    if not IsLoading then
      ReloadBuffer;
  end;
end;

procedure TCastleSound.SetURL(const Value: String);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    if not IsLoading then
      ReloadBuffer;
  end;
end;

procedure TCastleSound.LoadFromStream(const AStream: TStream; const MimeType: String);
begin
  // TODO: unoptimal implementation, we should pass TStream downward, not base64-encode it
  URL := StreamToDataUri(AStream, MimeType);
end;

procedure TCastleSound.SetVolume(const Value: Single);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetPitch(const Value: Single);
begin
  if FPitch <> Value then
  begin
    FPitch := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetMinGain(const Value: Single);
begin
  if FMinGain <> Value then
  begin
    FMinGain := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetMaxGain(const Value: Single);
begin
  if FMaxGain <> Value then
  begin
    FMaxGain := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetPriority(const Value: Single);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetReferenceDistance(const Value: Single);
begin
  if FReferenceDistance <> Value then
  begin
    FReferenceDistance := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.SetMaxDistance(const Value: Single);
begin
  if FMaxDistance <> Value then
  begin
    FMaxDistance := Value;
    DoChange(scAfterOther);
  end;
end;

procedure TCastleSound.DoChange(const Change: TSoundChange);
begin
  FChangeNotifications.ExecuteAll(Self, Change);
end;

procedure TCastleSound.InternalAddChangeNotification(const Notify: TCastleSoundChangeEvent);
begin
  FChangeNotifications.Add(Notify);
end;

procedure TCastleSound.InternalRemoveChangeNotification(const Notify: TCastleSoundChangeEvent);
begin
  if FChangeNotifications <> nil then // may be nil when destroying, and we make notification
    FChangeNotifications.Remove(Notify);
end;

function TCastleSound.Duration: Single;
begin
  if FBuffer <> nil then
    Result := FBuffer.Duration
  else
    Result := -1;
end;

function TCastleSound.DataFormat: TSoundDataFormat;
begin
  if FBuffer <> nil then
    Result := FBuffer.DataFormat
  else
    Result := Default(TSoundDataFormat); // whatever, just not memory garbage
end;

function TCastleSound.Frequency: LongWord;
begin
  if FBuffer <> nil then
    Result := FBuffer.Frequency
  else
    Result := 0;
end;

{$endif read_implementation}
