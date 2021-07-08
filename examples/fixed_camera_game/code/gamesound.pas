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

uses CastleSoundEngine;

var
  SoundIntroMusic,
  SoundMainMenuMusic,
  SoundMenuClick,
  SoundMenuCurrentItemChanged
  : TCastleSound;

procedure InitializeSound;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize;

var
  AllSoundsOwner: TComponent;

procedure InitializeSound;
begin
  AllSoundsOwner := TComponent.Create(nil);
  ComponentLoad('castle-data:/sounds/all_sounds.castle-component', AllSoundsOwner);
  SoundIntroMusic             := AllSoundsOwner.FindRequiredComponent('SoundIntroMusic') as TCastleSound;
  SoundMainMenuMusic          := AllSoundsOwner.FindRequiredComponent('SoundMainMenuMusic') as TCastleSound;
  SoundMenuClick              := AllSoundsOwner.FindRequiredComponent('SoundMenuClick') as TCastleSound;
  SoundMenuCurrentItemChanged := AllSoundsOwner.FindRequiredComponent('SoundMenuCurrentItemChanged') as TCastleSound;
end;

finalization
  FreeAndNil(AllSoundsOwner);
end.
