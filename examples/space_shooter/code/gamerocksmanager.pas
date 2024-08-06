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
  CastleControls, CastleTimeUtils, CastleViewport;

type
  { Manage rocks. }
  TRocksManager = class(TCastleUserInterface)
  private
    RockFactory: TCastleComponentFactory;
    TimeToNextRock: TFloatTime;
    FRocksDestroyed: Cardinal;
    procedure RockCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  public
    { Set this to the parent of all rocks.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside this correspond to
      the translations/rotations in the world coordinates of the viewport. }
    RocksParent: TCastleTransform;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    { Number of rocks destroyed. E.g. to show player score. }
    property RocksDestroyed: Cardinal read FRocksDestroyed;

    { Make sure rock resources, like the texture, is initialized.
      Otherwise the first rock creation would cause a noticeable stutter
      when spawning. }
    procedure InitializeRockResources(const ParentViewport: TCastleViewport);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleRectangles,
  GameBehaviors;

constructor TRocksManager.Create(AOwner: TComponent);
begin
  inherited;
  RockFactory := TCastleComponentFactory.Create(Self);
  RockFactory.Url := 'castle-data:/rocks/rock.castle-transform';

  TimeToNextRock := RandomFloatRange(0.25, 1.0);
end;

procedure TRocksManager.InitializeRockResources(const ParentViewport: TCastleViewport);
var
  TestRock: TCastleTransform;
begin
  TestRock := RockFactory.ComponentLoad(Self) as TCastleTransform;
  ParentViewport.PrepareResources(TestRock);
  // FreeAndNil(TestRock) // do not free it, let it exist, to hold reference count to the texture
end;

procedure TRocksManager.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure SpawnRock;
  var
    Rock: TCastleTransform;
    RockRigidBody: TCastleRigidBody;
    RockOwner: TComponent;
    SceneToRotate: TCastleTransform;
  begin
    // create a rock
    RockOwner := TComponent.Create(Self);
    Rock := RockFactory.ComponentLoad(RockOwner) as TCastleTransform;

    Rock.AddBehavior(TAutoRemoveLeftBehavior.Create(Rock));
    Rock.AddBehavior(TRockBehavior.Create(Rock));

    SceneToRotate := RockOwner.FindRequiredComponent('SceneToRotate') as TCastleTransform;
    SceneToRotate.AddBehavior(TRotateBehavior.Create(SceneToRotate));

    { Note: do not put initial X too far beyond the screen, otherwise
      it could be too often destroyed when off-screen, which isn't impressive
      for the player. }
    Rock.Translation := Vector3(1100, RandomFloatRange(-500, 500), 100);

    RocksParent.Add(Rock);

    RockRigidBody := RockOwner.FindRequiredComponent('RockRigidBody') as TCastleRigidBody;
    RockRigidBody.LinearVelocity := Vector3(-500, 0, 0);
    RockRigidBody.OnCollisionEnter := {$ifdef FPC}@{$endif} RockCollisionEnter;
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
  Rock, Rocket: TCastleTransform;
begin
  // Abort if this is not a collision between rock and rocket
  if CollisionDetails.Transforms[0].FindBehavior(TRockBehavior) = nil then
    Exit;
  if CollisionDetails.Transforms[1].FindBehavior(TRocketBehavior) = nil then
    Exit;

  { TODO: We could do something more interesting here, like play a sound,
    or play pretty rock exploding animation.
    For now, we just make the rock disappear.
    The rocket that hit the rock also disappears. }
  Rock := CollisionDetails.Transforms[0];
  // TODO: workaround occasional crash here, only remove the rock, do not free it
  //Rock.Parent.RemoveDelayed(Rock, true);
  Rock.Parent.RemoveDelayed(Rock);

  Rocket := CollisionDetails.Transforms[1];
  Rocket.Parent.RemoveDelayed(Rocket, true);

  Inc(FRocksDestroyed);
end;

end.