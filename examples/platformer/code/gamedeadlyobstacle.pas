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

{ Behavior of deadly obstacles (TDeadlyObstacle). }
unit GameDeadlyObstacle;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  TDeadlyObstacle = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    { How much time need to pass to deal damage. }
    HitInterval: Single;
    CollidingTime: Single;
    IsPlayerColliding: Boolean;
    procedure ConfigureDeadlyObstaclePhysics(const DeadlyObstacleScene: TCastleScene);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure HitPlayer;

    procedure CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    procedure CollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
  end;

  TDeadlyObstaclesList = {$ifdef FPC}specialize{$endif} TObjectList<TDeadlyObstacle>;

implementation

uses GameStatePlay;

{ TDeadlyObstacle ------------------------------------------------------------ }

procedure TDeadlyObstacle.ConfigureDeadlyObstaclePhysics(
  const DeadlyObstacleScene: TCastleScene);
var
  RBody: TCastleRigidBody;
begin
  { Castle Rigid Body and Collider is added in editor we configure only events
    in code. }

  RBody := DeadlyObstacleScene.RigidBody;
  if RBody <> nil then
  begin
    RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}CollisionEnter;
    RBody.OnCollisionExit := {$ifdef FPC}@{$endif}CollisionExit;
  end;
end;

constructor TDeadlyObstacle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitInterval := 0.7;
  CollidingTime := 0;
  IsPlayerColliding := false;
end;

procedure TDeadlyObstacle.ParentAfterAttach;
begin
  inherited ParentAfterAttach;

  Scene := Parent as TCastleScene;
  ConfigureDeadlyObstaclePhysics(Scene);
end;

procedure TDeadlyObstacle.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);
  if IsPlayerColliding then
    CollidingTime := CollidingTime + SecondsPassed;

  if CollidingTime > HitInterval then
  begin
    CollidingTime := 0;
    HitPlayer;
  end;
end;

procedure TDeadlyObstacle.HitPlayer;
begin
  StatePlay.HitPlayer;
  { When player is dead stop hiting. }
  if StatePlay.IsPlayerDead then
  begin
    IsPlayerColliding := false;
    CollidingTime := 0;
  end;
end;

procedure TDeadlyObstacle.CollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if Pos('ScenePlayer', CollisionDetails.OtherTransform.Name) > 0 then
  begin
    HitPlayer;
    IsPlayerColliding := true;
    CollidingTime := 0;
  end;
end;

procedure TDeadlyObstacle.CollisionExit(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if Pos('ScenePlayer', CollisionDetails.OtherTransform.Name) > 0 then
  begin
    IsPlayerColliding := false;
    CollidingTime := 0;
  end;
end;

end.
