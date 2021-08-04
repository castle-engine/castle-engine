{
  Copyright 2021-2021 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Behavior of falling obstacles (TFallingObstacle). }
unit GameFallingObstacle;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  TFallingObstacle = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    { Set to true when player was bellow and started falling }
    IsFalling: Boolean;
    procedure ConfigureFallingObstaclePhysics(const FallingObstacleScene: TCastleScene);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentChanged; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure HitPlayer;

    procedure CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  end;

TFallingObstaclesList = specialize TObjectList<TFallingObstacle>;

implementation

uses GameStatePlay;

{ TFallingObstacle }

procedure TFallingObstacle.ConfigureFallingObstaclePhysics(
  const FallingObstacleScene: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
begin
  RBody := TRigidBody.Create(FallingObstacleScene);
  RBody.Dynamic := true;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.OnCollisionEnter := @CollisionEnter;

  Collider:= TBoxCollider.Create(RBody);
  Collider.Size := Vector3(5, Scene.BoundingBox.SizeY / 3, 30.0);
  Collider.Friction := 0.1;
  Collider.Restitution := 0;

  FallingObstacleScene.RigidBody := RBody;
end;

constructor TFallingObstacle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsFalling := false;
end;

procedure TFallingObstacle.ParentChanged;
begin
  inherited ParentChanged;
  Scene := Parent as TCastleScene;
  ConfigureFallingObstaclePhysics(Scene);
end;

procedure TFallingObstacle.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RayHitThat: TCastleTransform;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if not IsFalling then
  begin
    { Here we wait for player to start falling down }
    RayHitThat := Scene.RigidBody.PhysicsRayCast(Scene.Translation,
      Vector3(0, -1, 0), 300);

    { Check was that a player? }
    if (RayHitThat <> nil) and (Pos('ScenePlayer', RayHitThat.Name) > 0) then
    begin
      { Start falling down }
      Scene.RigidBody.Gravity := true;
      Scene.RigidBody.LinearVelocity := Vector3(0, -500, 0);
      IsFalling := true;
    end;
  end;
end;

procedure TFallingObstacle.HitPlayer;
begin
  StatePlay.HitPlayer;
  Scene.Exists := false;
  //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
  Scene.RigidBody.Exists := false;
end;

procedure TFallingObstacle.CollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if Pos('ScenePlayer', CollisionDetails.OtherTransform.Name) > 0 then
  begin
    HitPlayer;
  end else
  begin
    { Once the spike hits the ground, it should no longer be able to
      hurt the player. }
    Scene.RigidBody.Exists := false;
  end;
end;

end.
