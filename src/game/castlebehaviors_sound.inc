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
  TCastleSound = class(TCastleBehavior)
  strict private
    UsedSounds: TSoundList;
    procedure SoundRelease(Sender: TSound);
    function LerpLegsMiddle(const A: Single): TVector3;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Play SoundType where the parent is.

      The exact position is between
      @link(TCastleTransform.Translation Parent.Translation) and
      @link(TCastleTransform.Middle Parent.Middle).
      SoundHeight = 0 means to use
      @link(TCastleTransform.Translation Parent.Translation),
      SoundHeight = 1 means @link(TCastleTransform.Middle Parent.Middle),
      other values imply a linear interpolation between the above two values.

      If TiedToParent then the sound position will be updated
      as the parent will move, and when we will
      be destroyed, sound will stop. If not TiedToParent, then
      the sound will simply be done at parent's position, but then
      it will continue to be played independent of the parent existence
      or position. }
    procedure PlayOnce(const SoundType: TSoundType; const SoundHeight: Single;
      const TiedToParent: boolean = true);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSoundData ----------------------------------------------------------------- }

type
  TSoundData = class
  public
    SoundHeight: Single;
  end;

{ TCastleSound ------------------------------------------------------- }

constructor TCastleSound.Create(AOwner: TComponent);
begin
  inherited;
  UsedSounds := TSoundList.Create(false);
end;

destructor TCastleSound.Destroy;
var
  I: Integer;
begin
  if UsedSounds <> nil then
  begin
    for I := 0 to UsedSounds.Count - 1 do
    begin
      UsedSounds[I].UserData.Free;
      UsedSounds[I].UserData := nil;

      { Otherwise OnRelease would call SoundRelease,
        and this would remove it from UsedSounds list, breaking our
        indexing over this list here. }
      UsedSounds[I].OnRelease := nil;
      UsedSounds[I].Release;
    end;
    FreeAndNil(UsedSounds);
  end;
  inherited;
end;

procedure TCastleSound.SoundRelease(Sender: TSound);
begin
  Sender.UserData.Free;
  Sender.UserData := nil;
  UsedSounds.Remove(Sender);
end;

procedure TCastleSound.PlayOnce(const SoundType: TSoundType; const SoundHeight: Single;
  const TiedToParent: boolean);
var
  NewSource: TSound;
  SoundPosition: TVector3;
begin
  SoundPosition := LerpLegsMiddle(SoundHeight);
  if Parent.UniqueParent <> nil then // make sound position in world coordinates
    SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);
  NewSource := SoundEngine.Sound3d(SoundType, SoundPosition);
  if TiedToParent and (NewSource <> nil) then
  begin
    UsedSounds.Add(NewSource);
    NewSource.OnRelease := @SoundRelease;
    NewSource.UserData := TSoundData.Create;
  end;
end;

procedure TCastleSound.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I: Integer;
  SoundPosition: TVector3;
begin
  inherited;
  for I := 0 to UsedSounds.Count - 1 do
  begin
    SoundPosition := LerpLegsMiddle(
      TSoundData(UsedSounds[I].UserData).SoundHeight);
    if Parent.UniqueParent <> nil then // make sound position in world coordinates
      SoundPosition := Parent.UniqueParent.LocalToWorld(SoundPosition);
    UsedSounds[I].Position := SoundPosition;
  end;
end;

function TCastleSound.LerpLegsMiddle(const A: Single): TVector3;
begin
  Result := Lerp(A, Parent.Translation, Parent.Middle);
end;

{$endif read_implementation}
