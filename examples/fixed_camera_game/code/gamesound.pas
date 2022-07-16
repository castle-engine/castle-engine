{
  Copyright 2007-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize sounds (TCastleSound instances). }
unit GameSound;

interface

uses Classes, CastleSoundEngine;

type
  TAllSounds = class(TComponent)
    SoundIntroMusic,
      SoundMainMenuMusic,
      SoundMenuClick,
      SoundMenuCurrentItemChanged: TCastleSound;
    constructor Create(AOwner: TComponent); override;
  end;

var
  { Create in InitializeSound, destroyed in finalization. }
  AllSounds: TAllSounds;

procedure InitializeSound;

implementation

uses SysUtils,
  CastleComponentSerialize;

procedure InitializeSound;
begin
  AllSounds := TAllSounds.Create(nil);
end;

constructor TAllSounds.Create(AOwner: TComponent);
begin
  inherited;
  ComponentLoad('castle-data:/sounds/all_sounds.castle-component', Self);
  SoundIntroMusic             := FindRequiredComponent('SoundIntroMusic') as TCastleSound;
  SoundMainMenuMusic          := FindRequiredComponent('SoundMainMenuMusic') as TCastleSound;
  SoundMenuClick              := FindRequiredComponent('SoundMenuClick') as TCastleSound;
  SoundMenuCurrentItemChanged := FindRequiredComponent('SoundMenuCurrentItemChanged') as TCastleSound;
end;

initialization
finalization
  FreeAndNil(AllSounds);
end.
