{
  Copyright 2024-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage the repository of sounds (useful from any view).

  Usage:
  - Configure sounds using CGE editor by editing data/audio/sounds.castle-component ,
  - From code:
    - Call once InitializeSounds (e.g. from ApplicationInitialize)
    - Use SoundXxx variables freely, they should non-nil.
      Treat them as read-only, don't modify them. }
unit GameSounds;

interface

uses CastleSoundEngine;

procedure InitializeSounds;

var
  SoundMusic: TCastleSound;
  SoundShoot: TCastleSound;

implementation

uses Classes,
  CastleWindow, CastleComponentSerialize;

procedure InitializeSounds;
var
  { Owner of components loaded from sounds.castle-component. }
  Sounds: TComponent;
begin
  Sounds := TComponent.Create(Application);
  ComponentLoad('castle-data:/audio/sounds.castle-component', Sounds);
  SoundMusic := Sounds.FindRequiredComponent('SoundMusic') as TCastleSound;
  SoundShoot := Sounds.FindRequiredComponent('SoundShoot') as TCastleSound;
end;

end.
