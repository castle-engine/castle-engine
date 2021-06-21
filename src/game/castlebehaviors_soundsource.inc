{%MainUnit castlebehaviors.pas}
{
  Copyright 2006-2021 Michalis Kamburelis.

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
  private
    SourceBehavior: TCastleSoundSource; //< non-nil while TCastleSoundSource exists
    procedure Update;
  protected
    procedure DoStop; override;
  public
    constructor Create(AOwner: TComponent); override;

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

      This matters only for spatial sounds (TCastleSound.Spatial). }
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
    property SoundHeight: Single read FSoundHeight write FSoundHeight default 0.0;
  end;

  { Behavior to play spatial sounds, that automatically follow
    the parent @link(TCastleTransform) transformation.
    Multiple such behaviors may be attached to a single TCastleTransform
    (if you need to play multiple looping sounds, or want to have multiple
    playback settings like different @link(Volume) control). }
  TCastleSoundSource = class(TCastleBehavior)
  strict private
    var
      FSound: TCastleSound;
      FSpatial: Boolean;
    procedure SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
    procedure SetSound(const Value: TCastleSound);
    procedure SetSpatial(const Value: Boolean);
  private
    type
      TCastlePlayingSoundSourceList = specialize TObjectList<TCastlePlayingSoundSource>;
    var
      AllPlayingSounds: TCastlePlayingSoundSourceList;
      MainPlayingSound: TCastlePlayingSoundSource; //< TCastlePlayingSoundSource corresponding to main Sound
    function GetSoundPosition(const SoundHeight: Single): TVector3;
  protected
    procedure ParentChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Play given sound once (not looping).
      If the sound is spatial (TCastleSound.Spatial),
      it's 3D position will reflect the @link(Parent) (TCastleTransform) position. }
    procedure Play(const ASound: TCastleSound);

    { Play given sound.
      If the sound is spatial (TCastleSound.Spatial),
      it's 3D position will reflect the @link(Parent) (TCastleTransform) position.

      You have to set @link(TCastlePlayingSound.Sound) before passing it to this method.

      Use the TCastlePlayingSound properties to provide additional information
      about the sound playback.
      E.g. assign @link(TCastlePlayingSound.Loop)
      @link(TCastlePlayingSound.OnStop) or control sound afterwards by
      @link(TCastlePlayingSound.Stop), @link(TCastlePlayingSound.Offset).

      Consider using @link(TCastlePlayingSound.FreeOnStop) if you don't want
      to manage the lifetime of the @link(TCastlePlayingSound) instance. }
    procedure Play(const PlayingSound: TCastlePlayingSoundSource);
  published
    { Sound to play, always looping.

      Note that this is not the only sound possible to play using this
      @link(TCastleSoundSource). You can play any other @link(TCastleSound)
      or @link(TCastlePlayingSoundSource) instance (looping or not looping)
      by calling @link(Play) method. }
    property Sound: TCastleSound read FSound write SetSound;

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

procedure TCastlePlayingSoundSource.Update;
begin
  if (SourceBehavior <> nil) and
     (InternalSource <> nil) and
     { If not Spatial, then we don't need to update Source.Position
       (it is ignored by backends). }
     InternalSource.Spatial and
     Follow then
  begin
    InternalSource.Position := SourceBehavior.GetSoundPosition(SoundHeight);
  end;
end;

{ TCastleSoundSource ------------------------------------------------------- }

constructor TCastleSoundSource.Create(AOwner: TComponent);
begin
  inherited;
  AllPlayingSounds := TCastlePlayingSoundSourceList.Create(false);
  FSpatial := true;
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
  case PropertyName of
    'Sound', 'Spatial':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

procedure TCastleSoundSource.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Playing: TCastlePlayingSoundSource;
begin
  inherited;
  for Playing in AllPlayingSounds do
    Playing.Update;
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
     (Parent.UniqueParent <> nil) then // make sound position in world coordinates
    Result := Parent.UniqueParent.LocalToWorld(Result);
end;

procedure TCastleSoundSource.Play(const ASound: TCastleSound);
var
  SoundPlaying: TCastlePlayingSoundSource;
begin
  SoundPlaying := TCastlePlayingSoundSource.Create(nil);
  SoundPlaying.Sound := ASound;
  SoundPlaying.FreeOnStop := true;
  Play(SoundPlaying);
end;

procedure TCastleSoundSource.Play(const PlayingSound: TCastlePlayingSoundSource);
var
  Params: TPlaySoundParameters;
  ASound: TCastleSound;
  SoundPosition: TVector3;
  Source: TInternalSoundSource;
begin
  if Parent = nil then
    raise Exception.Create('Do not play sounds from a TCastleSoundSource not attached to any TCastleTransform; use SoundEngine.Play for sound playback without TCastleSoundSource');

  SoundPosition := GetSoundPosition(PlayingSound.SoundHeight);

  ASound := PlayingSound.Sound;
  Assert(ASound <> nil);

  { TODO:
    - The check of ASound.InternalBuffer,
    - manual calling of PlayingSound.DoStop in case of early stop
    - setting TPlaySoundParameters
    - calling PlayingSound.InternalStart

    ... should all be done only by TSoundEngine.Play.
    Here we really only want to set some params before starting sound playback,
    but after TInternalSoundSource is allocated. }

  if ASound.InternalBuffer = nil then
  begin
    // sound cannot be played, as not loaded
    PlayingSound.DoStop; // apply PlayingSound.OnStop, FreeOnStop
    Exit;
  end;

  Params := TPlaySoundParameters.Create;
  try
    Params.Position   := SoundPosition;
    Params.Buffer     := ASound.InternalBuffer;
    Params.Spatial    := Spatial;
    Params.Loop       := PlayingSound.Loop;
    Params.Priority   := ASound.Priority;
    Params.Pitch      := ASound.Pitch;
    Params.Volume     := ASound.Volume;
    {$warnings off} // using unportable symbols knowingly
    Params.MinGain    := ASound.MinGain;
    Params.MaxGain    := ASound.MaxGain;
    {$warnings on}
    Params.Offset     := PlayingSound.InitialOffset;

    {$warnings off} // using deprecated that should be internal
    Source := SoundEngine.PlaySound(Params);
    {$warnings on}

    if Source <> nil then
    begin
      PlayingSound.Start(Source);
      { TODO:
        SoundEngine.PlayingSounds.Add(PlayingSound);
        ... and see TSoundEngine.Play TODOs.
      }
    end else
    begin
      // sound cannot be played, as no free TInternalSoundSource slot
      PlayingSound.DoStop; // apply PlayingSound.OnStop, FreeOnStop
    end;

    PlayingSound.SourceBehavior := Self;
    AllPlayingSounds.Add(PlayingSound);
  finally FreeAndNil(Params) end;
end;

procedure TCastleSoundSource.SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
begin
  case Change of
    scBeforeBufferDestroyed:
      begin
        if MainPlayingSound <> nil then
          MainPlayingSound.Stop;
        Assert(MainPlayingSound = nil);
      end;

    scAfterBufferCreated:
      begin
        { Loop and Follow = true, it seems most natural for sounds set as TCastleSoundSource.Sound. }
        if (Sound <> nil) and (Parent <> nil) then
        begin
          MainPlayingSound := TCastlePlayingSoundSource.Create(nil);
          MainPlayingSound.Sound := Sound;
          MainPlayingSound.FreeOnStop := true;
          MainPlayingSound.Loop := true;
          Play(MainPlayingSound);
        end;
      end;

    scAfterOther:
      begin
        if MainPlayingSound <> nil then
        begin
          if MainPlayingSound.InternalSource <> nil then
          begin
            // TODO: move this updating to TCastlePlayingSoundSource.Update?
            MainPlayingSound.InternalSource.Volume := Sound.Volume;
            MainPlayingSound.InternalSource.Pitch := Sound.Pitch;
            MainPlayingSound.InternalSource.Spatial := Spatial;
            {$warnings off} // using unportable symbols knowingly
            MainPlayingSound.InternalSource.MinGain := Sound.MinGain;
            MainPlayingSound.InternalSource.MaxGain := Sound.MaxGain;
            {$warnings on}
          end else
            Play(MainPlayingSound); // retry allocating TInternalSoundSource for it
        end;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleSoundSource.SoundChange:Change');
    {$endif}
  end;
end;

procedure TCastleSoundSource.ParentChanged;
begin
  inherited;

  if Parent <> nil then
  begin
    // start playing, once we have Parent assigned
    if (Sound <> nil) and (MainPlayingSound = nil) then
      SoundChange(FSound, scAfterBufferCreated);
  end else
  begin
    // stop playing when Parent detached,
    // we could not realize Follow in this case (GetSoundPosition would crash)
    if MainPlayingSound <> nil then
      MainPlayingSound.Stop;
  end;

  { Change of parent implies also a change of position.
    For now position is updated in Update, for all sounds, so we don't do anything here. }
end;

procedure TCastleSoundSource.SetSound(const Value: TCastleSound);
begin
  if FSound <> Value then
  begin
    if FSound <> nil then
    begin
      SoundChange(FSound, scBeforeBufferDestroyed);
      FSound.InternalRemoveChangeNotification(@SoundChange);
      FSound.RemoveFreeNotification(Self);
    end;
    FSound := Value;
    if FSound <> nil then
    begin
      FSound.InternalAddChangeNotification(@SoundChange);
      FSound.FreeNotification(Self);
      SoundChange(FSound, scAfterBufferCreated);
    end;
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
    if FSound <> nil then
      SoundChange(FSound, scAfterOther);
  end;
end;

{$endif read_implementation}
