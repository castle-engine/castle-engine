{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Component to manage rockets. }
unit GameRocketsManager;

interface

uses Classes,
  CastleVectors, CastleTransform, CastleUiControls, CastleComponentSerialize,
  CastleControls;

type
  { Manage rockets. }
  TRocketsManager = class(TCastleUserInterface)
  private
    RocketFactory: TCastleComponentFactory;
    CannonTimers: array of TCastleTimer;
    procedure CannonTimer(Sender: TObject);
  public
    { Set this to the list of cannons on player's spaceship.
      Call InitializeCannons after setting this. }
    Cannons: array of TCastleTransform;

    { Set this to the parent of all rockets.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside RocketsParent correspond to
      the translations/rotations in the world coordinates of the viewport. }
    RocketsParent: TCastleTransform;

    constructor Create(AOwner: TComponent); override;

    { Call after you set Cannons. }
    procedure InitializeCannons;

    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleRectangles;

constructor TRocketsManager.Create(AOwner: TComponent);
begin
  inherited;
  RocketFactory := TCastleComponentFactory.Create(Self);
  RocketFactory.Url := 'castle-data:/rocket.castle-transform';
end;

procedure TRocketsManager.InitializeCannons;
var
  I: Integer;
begin
  // make sure to clean previous timers, in case you call InitializeCannons many times
  for I := 0 to High(CannonTimers) do
    FreeAndNil(CannonTimers[I]);

  // create TCastleTimer for each cannon
  SetLength(CannonTimers, Length(Cannons));
  for I := 0 to High(Cannons) do
  begin
    CannonTimers[I] := TCastleTimer.Create(Self);
    // for this demo, it seems nice to randomize cannons' shooting times
    CannonTimers[I].IntervalSeconds := RandomFloatRange(0.1, 0.3);
    CannonTimers[I].OnTimer := {$ifdef FPC}@{$endif} CannonTimer;
    CannonTimers[I].Tag := I;
    InsertFront(CannonTimers[I]);
  end;
end;

procedure TRocketsManager.CannonTimer(Sender: TObject);
var
  Rocket, Cannon: TCastleTransform;
  RocketOwner: TComponent;
  RocketRigidBody: TCastleRigidBody;
  RocketDirection: TVector3;
begin
  // create a rocket
  RocketOwner := TComponent.Create(Self);
  Rocket := RocketFactory.ComponentLoad(RocketOwner) as TCastleTransform;

  RocketRigidBody := RocketOwner.FindRequiredComponent('RocketRigidBody') as TCastleRigidBody;

  Cannon := Cannons[(Sender as TCastleTimer).Tag];
  Rocket.Translation := Cannon.WorldTranslation;

  RocketsParent.Add(Rocket);

  { Use Vector3(0, 1, 0) to determine the rocket direction,
    because we happened to arrange Cannon transformations in editor
    such that +Y (green arrow) points in the direction in which the cannon
    should shoot.

    Note: Do not use Cannon.Direction, which assumes typical 3D direction/up
    following glTF orientation, rotated by Cannon.Rotation.
    This is not really useful for us here, despite a tempting sounding name. }
  RocketDirection := Cannon.LocalToWorldDirection(Vector3(0, 1, 0));
  RocketRigidBody.LinearVelocity := RocketDirection * 1000;
end;

procedure TRocketsManager.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  { Rockets are removed when they go far away.
    This rectangle was chosen to be large enough to cover the whole screen.

    Vertically, it covers the screen for 100%:
    Height is 1200, for sure larger than Viewport.Camera.Orthographic.Height = 1000.

    Horizontally, it covers the screen as long as aspect ratio is <= 2:1.

    We *could* adjust it to Viewport.Camera.Orthographic.EffectiveRect
    sizes, and be more precise. But this would mean that game balance works
    a bit differently (rockets reach more or less further) depending on
    the window size aspect ratio. }
  RocketAllowedPositions: TFloatRectangle = (
    Left: -1000; Bottom: -600; Width: 2000; Height: 1200
  );
var
  Rocket: TCastleTransform;
begin
  inherited;
  for Rocket in RocketsParent do
    if not RocketAllowedPositions.Contains(Rocket.WorldTranslation.XY) then
      RocketsParent.RemoveDelayed(Rocket, true);
end;

end.