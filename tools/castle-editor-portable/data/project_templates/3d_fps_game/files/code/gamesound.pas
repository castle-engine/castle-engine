{ Manage the repository of sounds (useful from any view).

  Usage:
  - Configure sounds using CGE editor by editing sounds.castle-component ,
  - From code:
    - Call once InitializeSounds (e.g. from ApplicationInitialize)
    - Use NamedSound to load TCastleSound instances.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
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
