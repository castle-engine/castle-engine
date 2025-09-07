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

{ Component to manage rocks. }
unit GameRocksManager;

interface

uses Classes,
  CastleVectors, CastleTransform, CastleUiControls, CastleComponentSerialize,
  CastleControls, CastleTimeUtils, CastleViewport,
  GameExplosionsManager;

type
  { Manage rocks. }
  TRocksManager = class(TCastleUserInterface)
  private
    Factory: TCastleComponentFactory;
    TimeToNextRock: TFloatTime;
    FRocksDestroyed: Cardinal;
    procedure RockCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  public
    { Set this to the parent of all rocks.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside this correspond to
      the translations/rotations in the world coordinates of the viewport. }
    RocksParent: TCastleTransform;

    { Use this to spawn explosions when rock is destroyed. }
    ExplosionsManager: TExplosionsManager;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    { Number of rocks destroyed. E.g. to show player score. }
    property RocksDestroyed: Cardinal read FRocksDestroyed;

    { Make sure rock resources, like the texture, are initialized.
      Otherwise the first rock creation would cause a noticeable stutter
      when spawning. }
    procedure InitializeResources(const ParentViewport: TCastleViewport);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleRectangles, CastleApplicationProperties,
  GameBehaviors;

constructor TRocksManager.Create(AOwner: TComponent);
begin
  inherited;
  Factory := TCastleComponentFactory.Create(Self);
  Factory.Url := 'castle-data:/rocks/rock.castle-transform';

  TimeToNextRock := RandomFloatRange(0.25, 1.0);
end;

procedure TRocksManager.InitializeResources(const ParentViewport: TCastleViewport);
var
  TestInstance: TCastleTransform;
begin
  TestInstance := Factory.ComponentLoad(Self) as TCastleTransform;
  ParentViewport.PrepareResources(TestInstance);
  // FreeAndNil(TestInstance) // do not free it, let it exist, to hold reference count to the texture
end;

type
  TRockDesign = class(TPersistent)
  published
    SceneToRotate: TCastleTransform;
    RockRigidBody: TCastleRigidBody;
  end;

procedure TRocksManager.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure SpawnRock;
  var
    Rock: TCastleTransform;
    RockDesign: TRockDesign;
    RockOwner: TComponent;
    RockBehavior: TRockBehavior;
    RockAutoRemoveBehavior: TAutoRemoveLeftBehavior;
  begin
    // create a rock
    RockDesign := TRockDesign.Create;
    try
      RockOwner := TComponent.Create(Self);

      Rock := Factory.ComponentLoad(RockOwner, RockDesign) as TCastleTransform;

      RockAutoRemoveBehavior := TAutoRemoveLeftBehavior.Create(RockOwner);
      RockAutoRemoveBehavior.RemoveOwner := RockOwner;
      Rock.AddBehavior(RockAutoRemoveBehavior);

      RockBehavior := TRockBehavior.Create(RockOwner);
      RockBehavior.RemoveOwner := RockOwner;
      Rock.AddBehavior(RockBehavior);

      RockDesign.SceneToRotate.AddBehavior(TRotateBehavior.Create(RockOwner));

      { Note: do not put initial X too far beyond the screen, otherwise
        it could be too often destroyed when off-screen, which isn't impressive
        for the player. }
      Rock.Translation := Vector3(1100, RandomFloatRange(-500, 500), 100);

      RocksParent.Add(Rock);

      RockDesign.RockRigidBody.LinearVelocity := Vector3(-500, 0, 0);
      RockDesign.RockRigidBody.OnCollisionEnter := {$ifdef FPC}@{$endif} RockCollisionEnter;
    finally FreeAndNil(RockDesign) end;
  end;

begin
  inherited;
  TimeToNextRock := TimeToNextRock - SecondsPassed;
  while TimeToNextRock < 0.0 do
  begin
    SpawnRock;
    TimeToNextRock := TimeToNextRock + RandomFloatRange(0.0, 1.0);
  end;
end;

procedure TRocksManager.RockCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
var
  Rock: TRockBehavior;
  Rocket: TRocketBehavior;
begin
  Rock := CollisionDetails.Sender.FindBehavior(TRockBehavior) as TRockBehavior;
  Rocket := CollisionDetails.OtherTransform.FindBehavior(TRocketBehavior) as TRocketBehavior;
  // Abort if this is not a collision between rock and rocket
  if (Rock = nil) or (Rocket = nil) then
    Exit;

  if not Rocket.Armed then
    Exit;

  ExplosionsManager.Spawn(Rock.Parent.Translation);

  { Make the rock and rocket disappear. }
  ApplicationProperties.FreeDelayed(Rock.RemoveOwner);
  ApplicationProperties.FreeDelayed(Rocket.RemoveOwner);

  Inc(FRocksDestroyed);
end;

end.