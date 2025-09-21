{
  Copyright 2023-2023 Andrzej Kilijański, Michalis Kamburelis.

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
  - Configure sounds using CGE editor by editing sounds.castle-component ,
  - From code:
    - Call once InitializeSounds (e.g. from ApplicationInitialize)
    - Use NamedSound to load TCastleSound instances. }
unit GameSound;

interface

uses CastleSoundEngine;

{ Initialize, call before any call to NamedSound. }
procedure InitializeSounds;

{ Returns TCastleSound with the given name from sounds.castle-component design. }
function NamedSound(const Name: String): TCastleSound;

implementation

uses Classes,
  CastleWindow, CastleComponentSerialize;

var
  { Owner of components loaded from sounds.castle-component. }
  Sounds: TComponent;

function NamedSound(const Name: String): TCastleSound;
begin
  Result := Sounds.FindRequiredComponent(Name) as TCastleSound;
end;

procedure InitializeSounds;
begin
  Sounds := TComponent.Create(Application);
  ComponentLoad('castle-data:/sounds.castle-component', Sounds);
end;

end.
