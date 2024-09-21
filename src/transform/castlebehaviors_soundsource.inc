{%MainUnit castlebehaviors.pas}
{
  Copyright 2006-2022 Michalis Kamburelis.

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
  TCastleSoundSource = class;

  { Controls a sound playback initiated by @link(TCastleSoundSource.Play).
    See the ancestor TCastlePlayingSound for most important documentation.
    This class simply extends the ancestor with some properties useful only
    together with the TCastleSoundSource. }
  TCastlePlayingSoundSource = class(TCastlePlayingSound)
  strict private
    FFollow: Boolean;
    FSoundHeight: Single;
    LastPosition: TVector3;
  private
    SourceBehavior: TCastleSoundSource; //< non-nil while TCastleSoundSource exists
    HasLastPosition: Boolean;
    procedure Update(const SecondsPassed: Single);
  protected
    procedure DoStop; override;
  public
    constructor Create(AOwner: TComponent); override;
    function InternalFinalVolume: Single; override;
    function InternalFinalPitch: Single; override;
    function InternalFinalPriority: Single; override;

    { If Follow then the sound position will be updated
      as the parent TCastleTransform moves. Moreover, when TCastleSoundSource
      will be destroyed, the sound will stop.
      An example would be a walking sound of a creature -- the sound moves
      together with the creature.

      If not Follow, then
      the sound will start at the parent's position, and then
      it will continue to be played independent of the parent existence
      or position changes.
      An example would be an explosion sound -- the sound stays
      at the position it happened, regardless if the object moved.

      This matters only for spatial sounds (@link(TCastleSoundSource.Spatial)). }
    property Follow: Boolean read FFollow write FFollow default true;

    { The exact sound position reflects the transformation of the TCastleSoundSource,
      either
      @link(TCastleTransform.Translation Parent.Translation) or
      @link(TCastleTransform.Middle Parent.Middle).
      Use this property to control which one, it makes a linear interpolation
      between them.

      SoundHeight = 0 means to use
      @link(TCastleTransform.Translation Parent.Translation),
      SoundHeight = 1 means @link(TCastleTransform.Middle Parent.Middle)
      other values imply a linear interpolation between the above two values.

      You can conigure @link(TCastleTransform.Middle Parent.Middle)
      using @link(TCastleTransform.MiddleHeight Parent.MiddleHeight). }
    property SoundHeight: Single read FSoundHeight write FSoundHeight
      {$ifdef FPC}default 0.0{$endif};
  end;

  { Behavior to play spatial sounds, that automatically follow
    the parent @link(TCastleTransform) transformation.
    Multiple such behaviors may be attached to a single TCastleTransform
    (if you need to play multiple looping sounds, or want to have multiple
    playback settings like different @link(Volume) control). }
  TCastleSoundSource = class(TCastleBehavior)
  strict private
    FSound: TCastleSound;
    FSpatial: Boolean;
    FVolume: Single;
    FPitch: Single;
    FPriority: Single;
    FSoundPlaying: Boolean;
    procedure SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
    procedure SetSound(const Value: TCastleSound);
    procedure SetSpatial(const Value: Boolean);
    procedure SetVolume(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetPriority(const Value: Single);
    procedure SetSoundPlaying(const Value: Boolean);
    { Start playing current @link(Sound)
      (ignored if Sound = @nil, ignored if SoundPlaying = @false).
      MainPlayingSound must be @nil when calling this, so the sound cannot be already playing. }
    procedure SoundStart;
    { Stop playing current @link(Sound), if currently playing. }
    procedure SoundStop;
  private
    type
      TCastlePlayingSoundSourceList = {$ifdef FPC}specialize{$endif} TObjectList<TCastlePlayingSoundSource>;
    var
      AllPlayingSounds: TCastlePlayingSoundSourceList;
      MainPlayingSound: TCastlePlayingSoundSource; //< TCastlePlayingSoundSource corresponding to main Sound
    function GetSoundPosition(const SoundHeight: Single): TVector3;
  protected
    procedure ParentAfterAttach; override;
    procedure ParentBeforeDetach; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Play given sound once (not looping).
      If the sound source is spatial (@link(TCastleSoundSource.Spatial)),
      it's 3D position will reflect the @link(Parent) (TCastleTransform) position. }
    procedure Play(const ASound: TCastleSound); overload;

    { Play given sound.
      If the sound source is spatial (@link(TCastleSoundSource.Spatial)),
      it's 3D position will reflect the @link(Parent) (TCastleTransform) position.

      Set @link(TCastlePlayingSound.Sound) before passing it to this method.
      Otherwise, if @link(TCastlePlayingSound.Sound) is left @nil, nothing will be played.

      Use the TCastlePlayingSound properties to provide additional information
      about the sound playback.
      E.g. assign @link(TCastlePlayingSound.Loop)
      @link(TCastlePlayingSound.OnStop) or control sound afterwards by
      @link(TCastlePlayingSound.Stop), @link(TCastlePlayingSound.Offset).

      Consider using @link(TCastlePlayingSound.FreeOnStop) if you don't want
      to manage the lifetime of the @link(TCastlePlayingSound) instance. }
    procedure Play(const PlayingSound: TCastlePlayingSoundSource); overload;
  published
    { Sound to play, always looping.
      Control playback of it using @link(SoundPlaying).

      Note that this is not the only sound possible to play using this
      @link(TCastleSoundSource). You can play any other @link(TCastleSound)
      or @link(TCastlePlayingSoundSource) instance (looping or not looping)
      by calling @link(Play) method. }
    property Sound: TCastleSound read FSound write SetSound;

    { Is the @link(Sound) currently playing.
      Setting it to @false and back to @true will restart playing sound from the beginning. }
    property SoundPlaying: Boolean read FSoundPlaying write SetSoundPlaying default true;

    { Is the sound playback spatialized. Affects sounds played by assigning @link(Sound)
      or calling @link(Play) here.

      Spatial sounds are louder/more silent depending on their 3D distance to listener
      (this means a distance from TCastleTransform origin to the camera within
      the TCastleViewport). And their relative position to the listener in 3D
      is reflected by making them louder/more silent in the proper
      (e.g. left or right) speaker.

      Note that some sound backends (like OpenAL) can only spatialize mono sounds,
      and play stereo sounds always as non-spatialized. }
    property Spatial: Boolean read FSpatial write SetSpatial default true;

    { Volume (how loud the playing sound is).

      The effective sound volume is a multiplication of @link(TCastleSound.Volume),
      @link(TCastlePlayingSound.Volume) and @link(TCastleSoundSource.Volume).
      It is also affected by spatial calculations (if the sound is played
      with @link(TCastleSoundSource.Spatial) = @true)
      and finally by a master @link(TSoundEngine.Volume SoundEngine.Volume).

      Any value > 0 is allowed. }
    property Volume: Single read FVolume write SetVolume
      {$ifdef FPC}default 1.0{$endif};

    { Sound playing speed.

      The effective sound pitch is a multiplication of @link(TCastleSound.Pitch),
      @link(TCastlePlayingSound.Pitch) and @link(TCastleSoundSource.Pitch).

      Any value > 0 is allowed. }
    property Pitch: Single read FPitch write SetPitch
      {$ifdef FPC}default 1.0{$endif};

    { How important is the sound, between 0.0 (least important) to 1.0 (most important).

      The effective priority is a multiplication of @link(TCastleSound.Priority),
      @link(TCastlePlayingSound.Priority) and @link(TCastleSoundSource.Priority).

      Any value > 0 is allowed. }
    property Priority: Single read FPriority write SetPriority
      {$ifdef FPC}default 1.0{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastlePlayingSoundSource ---------------------------------------------- }

constructor TCastlePlayingSoundSource.Create(AOwner: TComponent);
begin
  inherited;
  FFollow := true;
end;

procedure TCastlePlayingSoundSource.DoStop;
begin
  // we override DoStop, instead of using OnStop, to allow user code to assign OnStop as needed
  if SourceBehavior <> nil then
  begin
    SourceBehavior.AllPlayingSounds.Remove(Self);
    if SourceBehavior.MainPlayingSound = Self then
      SourceBehavior.MainPlayingSound := nil;
    SourceBehavior := nil;
  end;
  inherited;
end;

procedure TCastlePlayingSoundSource.Update(const SecondsPassed: Single);
const
  MaxVelocityLength = 20;
var
  NewPosition, NewVelocity: TVector3;
begin
  if (SourceBehavior <> nil) and
     (InternalSource <> nil) and
     { If not Spatial, then we don't need to update Source.Position
       (it is ignored by backends). }
     InternalSource.Spatial and
     Follow then
  begin
    NewPosition := SourceBehavior.GetSoundPosition(SoundHeight);
    InternalSource.Position := NewPosition;

    { update InternalSource.Velocity, to hear Doppler effect. }
    if HasLastPosition and (SecondsPassed > SingleEpsilon) then
    begin
      NewVelocity := (NewPosition - LastPosition) / SecondsPassed;
      { Clamp NewVelocity length, to not cause too crazy Doppler because
        of temporary high speed.
        Testcase: examples/audio/doppler_demo, drag with mouse fast. }
      if NewVelocity.LengthSqr > Sqr(MaxVelocityLength) then
        NewVelocity := NewVelocity.AdjustToLength(MaxVelocityLength);
      InternalSource.Velocity := NewVelocity;
    end;
    HasLastPosition := true;
    LastPosition := NewPosition;
  end else
    HasLastPosition := false;
end;

function TCastlePlayingSoundSource.InternalFinalVolume: Single;
begin
  Result := inherited;
  if SourceBehavior <> nil then
    Result := Result * SourceBehavior.Volume;
end;

function TCastlePlayingSoundSource.InternalFinalPitch: Single;
begin
  Result := inherited;
  if SourceBehavior <> nil then
    Result := Result * SourceBehavior.Pitch;
end;

function TCastlePlayingSoundSource.InternalFinalPriority: Single;
begin
  Result := inherited;
  if SourceBehavior <> nil then
    Result := Result * SourceBehavior.Priority;
end;

{ TCastleSoundSource ------------------------------------------------------- }

constructor TCastleSoundSource.Create(AOwner: TComponent);
begin
  inherited;
  AllPlayingSounds := TCastlePlayingSoundSourceList.Create(false);
  FSpatial := true;
  FVolume := 1.0;
  FPitch := 1.0;
  FPriority := 1.0;
  FSoundPlaying := true;
end;

destructor TCastleSoundSource.Destroy;
var
  Playing: TCastlePlayingSoundSource;
begin
  if AllPlayingSounds <> nil then
  begin
    for Playing in AllPlayingSounds do
    begin
      { In case of Playing.Follow = true, this is needed:
        Otherwise Playing.DoStop would call our SoundStop,
        and this would remove it from AllPlayingSounds list, breaking our
        indexing over this list here.

        In case of Playing.Follow = false, this is needed even more:
        the TCastlePlayingSound instance will survive destruction of this TCastleSoundSource,
        but it should not call any of TCastleSoundSource methods. }
      Playing.SourceBehavior := nil;
      if Playing.Follow then
        Playing.Stop;
    end;
    FreeAndNil(AllPlayingSounds);
  end;

  { We now for sure stopped this sound.
    But we cleared SourceBehavior above, so it didn't set itself to nil
    in TCastlePlayingSoundSource.DoStop.
    So we need to nil it manually. }
  MainPlayingSound := nil;

  // detach free notifications
  Sound := nil;

  inherited;
end;

function TCastleSoundSource.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Sound', 'Spatial', 'Volume', 'Pitch', 'Priority', 'SoundPlaying'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSoundSource.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Playing: TCastlePlayingSoundSource;
begin
  inherited;
  for Playing in AllPlayingSounds do
    Playing.Update(SecondsPassed);
end;

function TCastleSoundSource.GetSoundPosition(const SoundHeight: Single): TVector3;

  function LerpLegsMiddle(const A: Single): TVector3;
  begin
    { Check "Parent.World <> nil", otherwise Parent.Middle not available.
      Check also A <> 0, as just optimization -- A = 0 is an often case,
      that avoids the need to calculate Parent.Middle. }
    if (Parent.World <> nil) and (A <> 0) then
      Result := Lerp(A, Parent.Translation, Parent.Middle)
    else
      Result := Parent.Translation;
  end;

begin
  Result := LerpLegsMiddle(SoundHeight);
  if (Parent <> nil) and
     (Parent.Parent <> nil) then // make sound position in world coordinates
    Result := Parent.Parent.LocalToWorld(Result);
end;

procedure TCastleSoundSource.Play(const ASound: TCastleSound);
var
  Playing: TCastlePlayingSoundSource;
begin
  Playing := TCastlePlayingSoundSource.Create(nil);
  Playing.Sound := ASound;
  Playing.FreeOnStop := true;
  Play(Playing);
end;

procedure TCastleSoundSource.Play(const PlayingSound: TCastlePlayingSoundSource);
var
  SoundPosition: TVector3;
begin
  if Parent = nil then
    raise Exception.Create('Do not play sounds from a TCastleSoundSource not attached to any TCastleTransform; use SoundEngine.Play for sound playback without TCastleSoundSource');

  SoundPosition := GetSoundPosition(PlayingSound.SoundHeight);

  { Add to AllPlayingSounds and set PlayingSound.SourceBehavior early,
    before calling SoundEngine.Play, as SoundEngine.Play may immediately stop it
    (e.g. if no free TInternalSoundSource slots). }
  PlayingSound.SourceBehavior := Self;
  AllPlayingSounds.Add(PlayingSound);

  SoundEngine.InternalPlay(PlayingSound, Spatial, SoundPosition);
end;

procedure TCastleSoundSource.SoundStop;
begin
  if MainPlayingSound <> nil then
    MainPlayingSound.Stop;
end;

procedure TCastleSoundSource.SoundStart;
begin
  Assert(MainPlayingSound = nil);
  if (Sound <> nil) and SoundPlaying and (Parent <> nil) then
  begin
    MainPlayingSound := TCastlePlayingSoundSource.Create(nil);
    MainPlayingSound.Sound := Sound;
    MainPlayingSound.FreeOnStop := true;
    { Loop and Follow = true in this case,
      it seems most natural for sounds set as TCastleSoundSource.Sound. }
    MainPlayingSound.Loop := true;
    Play(MainPlayingSound);
  end;
end;

procedure TCastleSoundSource.SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
begin
  case Change of
    scAfterBufferCreated: SoundStart;
    else ;
      // scAfterOther handled by TCastlePlayingSound
      // scBeforeBufferDestroyed also handled by TCastlePlayingSound, it will stop sound making MainPlayingSound -> nil
  end;
end;

procedure TCastleSoundSource.ParentAfterAttach;
var
  Playing: TCastlePlayingSoundSource;
begin
  inherited;

  // start playing, once we have Parent assigned
  if MainPlayingSound = nil then
    SoundStart;

  { Change of parent implies also a sudden change of position,
    so do not use last position for Doppler effect calculation. }
  for Playing in AllPlayingSounds do
    Playing.HasLastPosition := false;
end;

procedure TCastleSoundSource.ParentBeforeDetach;
begin
  // stop playing when Parent detached,
  // we could not realize Follow in this case (GetSoundPosition would crash)
  SoundStop;
  inherited;
end;

procedure TCastleSoundSource.SetSound(const Value: TCastleSound);
begin
  if FSound <> Value then
  begin
    if FSound <> nil then
    begin
      SoundStop;
      FSound.InternalRemoveChangeNotification({$ifdef FPC}@{$endif}SoundChange);
      FSound.RemoveFreeNotification(Self);
    end;
    FSound := Value;
    if FSound <> nil then
    begin
      FSound.InternalAddChangeNotification({$ifdef FPC}@{$endif}SoundChange);
      FSound.FreeNotification(Self);
      SoundStart;
    end;
  end;
end;

procedure TCastleSoundSource.SetSoundPlaying(const Value: Boolean);
begin
  if FSoundPlaying <> Value then
  begin
    FSoundPlaying := Value;
    if Value then
      SoundStart
    else
      SoundStop;
  end;
end;

procedure TCastleSoundSource.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  { set to nil by setter to clean nicely }
  if (Operation = opRemove) and (AComponent = FSound) then
    Sound := nil;
end;

procedure TCastleSoundSource.SetSpatial(const Value: Boolean);
begin
  if FSpatial <> Value then
  begin
    FSpatial := Value;
    if (MainPlayingSound <> nil) and
       (MainPlayingSound.InternalSource <> nil) then
      MainPlayingSound.InternalSource.Spatial := Spatial;
  end;
end;

procedure TCastleSoundSource.SetVolume(const Value: Single);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    { Update because MainPlayingSound.InternalFinalVolume changed,
      just like TCastlePlayingSound.SoundChangeDuringPlayback does. }
    if (MainPlayingSound <> nil) and
       (MainPlayingSound.InternalSource <> nil) then
      MainPlayingSound.InternalSource.Volume := MainPlayingSound.InternalFinalVolume;
  end;
end;

procedure TCastleSoundSource.SetPitch(const Value: Single);
begin
  if FPitch <> Value then
  begin
    FPitch := Value;
    { Update because MainPlayingSound.InternalFinalVolume changed,
      just like TCastlePlayingSound.SoundChangeDuringPlayback does. }
    if (MainPlayingSound <> nil) and
       (MainPlayingSound.InternalSource <> nil) then
      MainPlayingSound.InternalSource.Pitch := MainPlayingSound.InternalFinalPitch;
  end;
end;

procedure TCastleSoundSource.SetPriority(const Value: Single);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    { Update because MainPlayingSound.InternalFinalPriority changed,
      just like TCastlePlayingSound.SoundChangeDuringPlayback does. }
    if (MainPlayingSound <> nil) and
       (MainPlayingSound.InternalSource <> nil) then
      MainPlayingSound.InternalSource.Priority := MainPlayingSound.InternalFinalPriority;
  end;
end;

{$endif read_implementation}
