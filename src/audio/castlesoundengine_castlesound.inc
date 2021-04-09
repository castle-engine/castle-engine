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

type
  { Sound file that can be loaded from URL (possibly reusing a cache,
    possibly using streaming) and played.

    Aside from sound file (also called "sound buffer" or "audio clip" in various APIs)
    this class allows to configue also playback values, like @link(Volume) and @link(Pitch).

    This class does not handle actual playing of the sound,
    as it doesn't manage the 3D position of sound in case of spatial sounds,
    looping etc. To play it, use @link(TCastleSoundSource).
    Attach @link(TCastleSoundSource) to a @link(TCastleTransform) within some @link(TCastleViewport).

    This class can be nicely serialized (so you can use this as a SubComponent
    e.g. in TCastleBehavior descendants that want to expose some sound,
    like TCastleMoveAttack). }
  TCastleSound = class(TCastleComponent)
  strict private
    FURL: string;
    FBuffer: TSoundBuffer;
    FStream: Boolean;
    FVolume: Single;
    FPitch: Single;
    FSpatial: Boolean;
    FMinGain: Single;
    FMaxGain: Single;
    FOnChange: TNotifyEvent;
    procedure SetStream(const Value: Boolean);
    procedure SetURL(const Value: String);
    procedure SetVolume(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetSpatial(const Value: Boolean);
    procedure SetMinGain(const Value: Single);
    procedure SetMaxGain(const Value: Single);
    procedure ReloadBuffer;
  protected
    procedure Loaded; override;
    procedure DoChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { @exclude Reserved for parent TCastleSoundSource to listen for changes. }
    property InternalOnChange: TNotifyEvent read FOnChange write FOnChange;

    { Loaded sound buffer, for playing the buffer with the @link(SoundEngine).
      @nil if not loaded. }
    property Buffer: TSoundBuffer read FBuffer;
  published
    { URL of the sound file.
      Set this to load a new sound buffer, you can set to '' to clear the sound buffer.
      Changing this also changes @link(Buffer).

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

      Note: Do not make the actual sound data (in wav, ogg and such files)
      louder/more silent for this purpose. This is usually bad for sound quality.
      Instead, keep your sound data at max loudness (normalized), and use
      this gain property to scale sound.

      It can be anything from 0 to +infinity. The default is 1. Note that values > 1 are allowed,
      but some sound backends (like OpenAL) may clip the resulting sound volume
      (after all spatial calculations are be done) to 1.0. }
    property Volume: Single read FVolume write SetVolume default 1.0;

    { Sound playing speed.

      Changing this naturally also changes the audible sound.
      Each reduction by 50 percent equals a pitchshift of -12 semitones (one octave reduction).
      Each doubling equals a pitch shift of 12 semitones (one octave increase).

      Any value > 0 it allowed. }
    property Pitch: Single read FPitch write SetPitch default 1.0;

    { Is the sound playback spatialized.

      Spatial sounds are louder/more silent depending on their 3D distance to listener
      (this means a distance from TCastleTransform origin to the camera within
      the TCastleViewport).

      Note that some sound backends (like OpenAL) can only spatialize mono sounds,
      and play stereo sounds always as non-spatialized. }
    property Spatial: Boolean read FSpatial write SetSpatial default true;

    { Force a minimum sound loudness, despite what volume would
      be calculated by the spatialization. This can be used to force sound
      to be audible, even when it's far away from the listener.

      It must be in [0, 1] range. By default it is 0. }
    property MinGain: Single read FMinGain write SetMinGain default 0.0;

    { Force a maximum sound loudness, despite what volume would
      be calculated by the spatialization. This can be used to limit sound volume,
      regardless of the distance attenuation calculation.

      It must be in [0, 1] range. By default it is 1. }
    property MaxGain: Single read FMaxGain write SetMaxGain default 1.0;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleSound.Create(AOwner: TComponent);
begin
  inherited;
  FStream := false;
  FVolume := 1.0;
  FPitch := 1.0;
  FSpatial := true;
  FMinGain := 0.0;
  FMaxGain := 1.0;
end;

destructor TCastleSound.Destroy;
begin
  if FBuffer <> nil then // avoid accessing SoundEngine if not needed
    SoundEngine.FreeBuffer(FBuffer); // this sets FBuffer = nil
  inherited;
end;

procedure TCastleSound.ReloadBuffer;
var
  SoundLoading: TSoundLoading;
begin
  if FBuffer <> nil then // avoid accessing SoundEngine if not needed
    SoundEngine.FreeBuffer(FBuffer); // this sets FBuffer = nil

  if URL <> '' then
  begin
    if Stream then
      SoundLoading := slStreaming
    else
      SoundLoading := slComplete;
    FBuffer := SoundEngine.LoadBuffer(URL, SoundLoading);
  end;

  DoChange;
end;

procedure TCastleSound.Loaded;
begin
  ReloadBuffer;
end;

procedure TCastleSound.SetStream(const Value: Boolean);
begin
  if FStream <> Value then
  begin
    FStream := Value;
    if not (csLoading in ComponentState) then
      ReloadBuffer;
  end;
end;

procedure TCastleSound.SetURL(const Value: String);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    if not (csLoading in ComponentState) then
      ReloadBuffer;
  end;
end;

procedure TCastleSound.SetVolume(const Value: Single);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    DoChange;
  end;
end;

procedure TCastleSound.SetPitch(const Value: Single);
begin
  if FPitch <> Value then
  begin
    FPitch := Value;
    DoChange;
  end;
end;

procedure TCastleSound.SetSpatial(const Value: Boolean);
begin
  if FSpatial <> Value then
  begin
    FSpatial := Value;
    DoChange;
  end;
end;

procedure TCastleSound.SetMinGain(const Value: Single);
begin
  if FMinGain <> Value then
  begin
    FMinGain := Value;
    DoChange;
  end;
end;

procedure TCastleSound.SetMaxGain(const Value: Single);
begin
  if FMaxGain <> Value then
  begin
    FMaxGain := Value;
    DoChange;
  end;
end;

procedure TCastleSound.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$endif read_implementation}
