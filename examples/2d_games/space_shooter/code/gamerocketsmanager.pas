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
    Factory: TCastleComponentFactory;
    CannonTimers: array of TCastleTimer;
    procedure CannonTimer(Sender: TObject);
  public
    { Set this to the list of cannons on player's spaceship.
      Call @link(Initialize) after setting this. }
    Cannons: array of TCastleTransform;

    { Set this to the parent of all rockets.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside RocketsParent correspond to
      the translations/rotations in the world coordinates of the viewport. }
    RocketsParent: TCastleTransform;

    ShootsArmed: Boolean;

    constructor Create(AOwner: TComponent); override;

    { Call after you set Cannons. }
    procedure Initialize;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleRectangles, CastleColors, CastleScene,
  GameBehaviors;

constructor TRocketsManager.Create(AOwner: TComponent);
begin
  inherited;
  Factory := TCastleComponentFactory.Create(Self);
  Factory.Url := 'castle-data:/rocket.castle-transform';
end;

procedure TRocketsManager.Initialize;
var
  I: Integer;
begin
  // make sure to clean previous timers, in case you call Initialize many times
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

type
  TRocketDesign = class(TPersistent)
  published
    RocketRigidBody: TCastleRigidBody;
    RootSphere: TCastleSphere;
  end;

procedure TRocketsManager.CannonTimer(Sender: TObject);
var
  Rocket, Cannon: TCastleTransform;
  RocketDesign: TRocketDesign;
  RocketDirection: TVector3;
  RocketOwner: TComponent;
  RocketBehavior: TRocketBehavior;
  RocketAutoRemoveBehavior: TAutoRemoveBehavior;
begin
  // create a rocket
  RocketDesign := TRocketDesign.Create;
  try
    RocketOwner := TComponent.Create(Self);

    Rocket := Factory.ComponentLoad(RocketOwner, RocketDesign) as TCastleTransform;

    RocketAutoRemoveBehavior := TAutoRemoveBehavior.Create(RocketOwner);
    RocketAutoRemoveBehavior.RemoveOwner := RocketOwner;
    Rocket.AddBehavior(RocketAutoRemoveBehavior);

    RocketBehavior := TRocketBehavior.Create(RocketOwner);
    RocketBehavior.RemoveOwner := RocketOwner;
    RocketBehavior.Armed := ShootsArmed;
    Rocket.AddBehavior(RocketBehavior);

    if ShootsArmed then
      RocketDesign.RootSphere.Color := Red
    else
      RocketDesign.RootSphere.Color := Yellow;

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
    RocketDesign.RocketRigidBody.LinearVelocity := RocketDirection * 1000;
  finally FreeAndNil(RocketDesign) end;
end;

end.