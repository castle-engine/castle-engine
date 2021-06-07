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
  { Behavior to play spatial sounds, that automatically follow
    the parent @link(TCastleTransform) transformation. }
  TCastleSoundSource = class(TCastleBehavior)
  strict private
    FSound: TCastleSound;
    UsedSounds: TInternalPlayingSoundList;
    PlayingSound: TInternalPlayingSound; //< TInternalPlayingSound corresponding to main Sound
    procedure SoundRelease(Sender: TInternalPlayingSound);
    function LerpLegsMiddle(const A: Single): TVector3;
    function PlayOnceCore(const ASound: TCastleSound; const Looping, Follow: Boolean;
      const SoundHeight: Single): TInternalPlayingSound;
    procedure SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
    procedure SetSound(const Value: TCastleSound);
  protected
    procedure ParentChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Play given Sound. If Sound has TCastleSound.Spatial = @true,
      it's 3D position will reflect the @link(Parent) (TCastleTransform) position,

      If Follow then the sound position will be updated
      as the parent TCastleTransform will move, and when TCastleSoundSource
      will be destroyed, sound will stop. If not Follow, then
      the sound will simply start at parent's position, and then
      it will continue to be played independent of the parent existence
      or position.

      In case of spatial sounds (TCastleSound.Spatial = @true)
      the exact sound position is between
      @link(TCastleTransform.Translation Parent.Translation) and
      @link(TCastleTransform.Middle Parent.Middle).
      SoundHeight = 0 means to use
      @link(TCastleTransform.Translation Parent.Translation),
      SoundHeight = 1 means @link(TCastleTransform.Middle Parent.Middle),
      other values imply a linear interpolation between the above two values. }
    procedure PlayOnce(const ASound: TCastleSound;
      const Follow: boolean = true; const SoundHeight: Single = 0);
  published
    { Sound to play, looping.
      Note that this is not the only sound possible to play using this
      @link(TCastleSoundSource). You can play any other @link(TCastleSound)
      instance, defined anywhere, by calling @link(PlayOnce) here. }
    property Sound: TCastleSound read FSound write SetSound;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSoundData ----------------------------------------------------------------- }

type
  TSoundData = class
  public
    SoundHeight: Single;
    Spatial, Follow: Boolean;
  end;

{ TCastleSoundSource ------------------------------------------------------- }

constructor TCastleSoundSource.Create(AOwner: TComponent);
begin
  inherited;
  UsedSounds := TInternalPlayingSoundList.Create(false);
end;

destructor TCastleSoundSource.Destroy;
var
  I: Integer;
  SoundData: TSoundData;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      SoundData := TSoundData(UsedSounds[I].UserData);
      UsedSounds[I].UserData := nil;

      { In case of Follow = true, this is needed:
        Otherwise OnRelease would call SoundRelease,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here.

        In case of Follow = false, this is needed even more:
        the TInternalPlayingSound instance will survive destruction of this TCastleSoundSource,
        but it should not call any of TCastleSoundSource methods. }
      UsedSounds[I].OnRelease := nil;

      if SoundData.Follow then
        UsedSounds[I].Release;

      FreeAndNil(SoundData);
    end;
    FreeAndNil(UsedSounds);
  end;

  // detach free notifications
  Sound := nil;

  inherited;
end;

function TCastleSoundSource.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Sound':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

procedure TCastleSoundSource.SoundRelease(Sender: TInternalPlayingSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  UsedSounds.Remove(Sender);
  if Sender = PlayingSound then
    PlayingSound := nil;
end;

procedure TCastleSoundSource.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I: Integer;
  SoundPosition: TVector3;
  SoundData: TSoundData;
begin
  inherited;
  for I := 0 to UsedSounds.Count - 1 do
  begin
    SoundData := TSoundData(UsedSounds[I].UserData);

    { Note that right now, things that have Follow = false are not really
      necessary on UsedSounds: they are not updated here,
      they are not stoppe in destruction.
      But they could be useful in the future, so we add all sounds we made to UsedSounds. }

    { If not Spatial, then we don't need to update UsedSounds[I].Position
      (it is ignored by backends). }

    if SoundData.Spatial and SoundData.Follow then
    begin
      SoundPosition := LerpLegsMiddle(SoundData.SoundHeight);
      if (Parent <> nil) and
         (Parent.UniqueParent <> nil) then // make sound position in world coordinates
        SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);
      UsedSounds[I].Position := SoundPosition;
    end;
  end;
end;

function TCastleSoundSource.LerpLegsMiddle(const A: Single): TVector3;
begin
  { Check "Parent.World <> nil", otherwise Parent.Middle not available.
    Check also A <> 0, as just optimization -- A = 0 is an often case,
    that avoids the need to calculate Parent.Middle. }
  if (Parent.World <> nil) and (A <> 0) then
    Result := Lerp(A, Parent.Translation, Parent.Middle)
  else
    Result := Parent.Translation;
end;

function TCastleSoundSource.PlayOnceCore(const ASound: TCastleSound;
  const Looping, Follow: Boolean; const SoundHeight: Single): TInternalPlayingSound;
var
  Params: TPlaySoundParameters;
  SoundPosition: TVector3;
begin
  Result := nil;

  if (ASound.InternalBuffer <> nil) and (Parent <> nil) then
  begin
    SoundPosition := LerpLegsMiddle(SoundHeight);
    if (Parent <> nil) and
       (Parent.UniqueParent <> nil) then // make sound position in world coordinates
      SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);

    Params := TPlaySoundParameters.Create;
    try
      Params.Position := SoundPosition;
      Params.Buffer := ASound.InternalBuffer;
      Params.Spatial := ASound.Spatial;
      Params.Looping := Looping;
      Params.Importance := DefaultSoundImportance;
      Params.Pitch := ASound.Pitch;
      Params.Volume := ASound.Volume;
      {$warnings off} // using unportable symbols knowingly
      Params.MinGain := ASound.MinGain;
      Params.MaxGain := ASound.MaxGain;
      {$warnings on}

      {$warnings off} // using deprecated that should be internal
      Result := SoundEngine.PlaySound(Params);
      {$warnings on}

      if Result <> nil then
      begin
        UsedSounds.Add(Result);
        Result.OnRelease := @SoundRelease;
        Result.UserData := TSoundData.Create;
        TSoundData(Result.UserData).SoundHeight := SoundHeight;
        TSoundData(Result.UserData).Spatial := ASound.Spatial;
        TSoundData(Result.UserData).Follow := Follow;
      end;
    finally FreeAndNil(Params) end;
  end;
end;

procedure TCastleSoundSource.SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
begin
  case Change of
    scBeforeBufferDestroyed:
      begin
        if PlayingSound <> nil then
          PlayingSound.Release;
        Assert(PlayingSound = nil);
      end;

    scAfterBufferCreated:
      begin
        { Looping and Follow = true, it seems most natural for sounds set as TCastleSoundSource.Sound. }
        if Sound <> nil then
          PlayingSound := PlayOnceCore(Sound, { looping } true, { follow } true, { SoundHeight } 0);
      end;

    scAfterOther:
      begin
        if Sound <> nil then
        begin
          if PlayingSound <> nil then
          begin
            PlayingSound.Volume := Sound.Volume;
            PlayingSound.Pitch := Sound.Pitch;
            PlayingSound.Spatial := Sound.Spatial;
            {$warnings off} // using unportable symbols knowingly
            PlayingSound.MinGain := Sound.MinGain;
            PlayingSound.MaxGain := Sound.MaxGain;
            {$warnings on}
          end else
            { start playing, just like after scAfterBufferCreated }
            PlayingSound := PlayOnceCore(Sound, { looping } true, { follow } true, { SoundHeight } 0);
        end;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleSoundSource.SoundChange:Change');
    {$endif}
  end;
end;

procedure TCastleSoundSource.PlayOnce(const ASound: TCastleSound;
  const Follow: boolean; const SoundHeight: Single);
begin
  PlayOnceCore(ASound, { looping } false, Follow, SoundHeight);
end;

procedure TCastleSoundSource.ParentChanged;
begin
  inherited;

  { Change of parent implies a change of position.
    For now commented out -- position is updated in Update, for all sounds.
    No need to do this here
    (also scAfterOther doesn't update PlayingSound.Position for now for spatial sounds).

  SoundChange(nil, scAfterOther);  }
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

{$endif read_implementation}
