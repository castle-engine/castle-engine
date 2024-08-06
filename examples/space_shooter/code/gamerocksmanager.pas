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
  public
    { Set this to the parent of all rocks.
      Should not be transformed, because (for simplicity) code here assumes
      that translations/rotations inside this correspond to
      the translations/rotations in the world coordinates of the viewport. }
    RocksParent: TCastleTransform;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

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

  TimeToNextRock := RandomFloatRange(1.0, 3.0);
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

    SceneToRotate := RockOwner.FindRequiredComponent('SceneToRotate') as TCastleTransform;
    SceneToRotate.AddBehavior(TRotateBehavior.Create(SceneToRotate));

    { Note: do not put initial X too far beyond the screen, otherwise
      it could be too often destroyed when off-screen, which isn't impressive
      for the player. }
    Rock.Translation := Vector3(1100, RandomFloatRange(-500, 500), 100);

    RocksParent.Add(Rock);

    RockRigidBody := RockOwner.FindRequiredComponent('RockRigidBody') as TCastleRigidBody;
    RockRigidBody.LinearVelocity := Vector3(-500, 0, 0);
  end;

begin
  inherited;
  TimeToNextRock := TimeToNextRock - SecondsPassed;
  while TimeToNextRock < 0.0 do
  begin
    SpawnRock;
    TimeToNextRock := TimeToNextRock + RandomFloatRange(1.0, 3.0);
  end;
end;

end.