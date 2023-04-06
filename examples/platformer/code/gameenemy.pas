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

{ Enemy behavior. }
unit GameEnemy;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform, CastleSoundEngine;

type
  TEnemy = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    RBody: TCastleRigidBody;
    MoveDirection: Integer; //< Always 1 or -1
    Dead: Boolean;
    DontFallDown: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure HitPlayer;
    procedure TakeDamageFromBullet(const Bullet: TCastleTransform);

    procedure CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  end;

  TEnemyList = {$ifdef FPC}specialize{$endif} TObjectList<TEnemy>;

implementation

uses
  CastleLog,
  GameSound, GameViewPlay;

{ TEnemy --------------------------------------------------------------------- }

constructor TEnemy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MoveDirection := -1;
  DontFallDown := true;
end;

procedure TEnemy.ParentAfterAttach;
begin
  inherited;

  Scene := Parent as TCastleScene; // TEnemy can only be added as behavior to TCastleScene
  Scene.PlayAnimation('walk', true);
  RBody := Scene.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody <> nil then
    RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}CollisionEnter;
  { In editor you can change scale to -1 1 1 to change enemy inital direction }
  if Scene.Scale.X < 0 then
    MoveDirection := 1;
end;

procedure TEnemy.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MovingSpeed = 200;
var
  EnemyOnGround: Boolean;
  NeedTurn: Boolean;
  Vel: TVector3;
  RayMaxDistance: Single;
  ObstacleAhead: TCastleTransform;
begin
  inherited;

  if Dead then
  begin
    if Scene.CurrentAnimation.X3DName <> 'dead' then
      Scene.PlayAnimation('dead', true);
    Exit;
  end;

  if RBody = nil then
    Exit;

  RayMaxDistance := Scene.BoundingBox.SizeY * 0.50 + 5;
  EnemyOnGround := RBody.PhysicsRayCast(Scene.Translation,
    Vector3(0, -1, 0), RayMaxDistance).Hit;

  if not EnemyOnGround then
  begin
    Scene.PlayAnimation('idle', true);
    Exit;
  end else
  begin
    if Scene.CurrentAnimation.X3DName <> 'walk' then
      Scene.PlayAnimation('walk', true);
  end;

  if DontFallDown then
  begin
    NeedTurn := not RBody.PhysicsRayCast(Scene.Translation
      + Vector3(MoveDirection * Scene.BoundingBox.SizeX * 0.50, 0, 0),
      Vector3(0, -1, 0), RayMaxDistance).Hit;
  end else
    NeedTurn := false;

  { Check enemy must turn because he go wall. }
  if not NeedTurn then
  begin
    ObstacleAhead := RBody.PhysicsRayCast(Scene.Translation,
      Vector3(MoveDirection, 0, 0), RayMaxDistance + 5).Transform;

    if ObstacleAhead <> nil then
    begin
      { Enemy should not run away from the player or change direction when
        there is coin }
      if (ObstacleAhead.Name <> 'ScenePlayer') and
         (Pos('GoldCoin', ObstacleAhead.Name) = 0) and
         (Pos('Spider', ObstacleAhead.Name) = 0) then
        NeedTurn := true;
    end;
  end;

  if NeedTurn then
    MoveDirection := - MoveDirection;

  Vel := RBody.LinearVelocity;

  Vel.X := MoveDirection * MovingSpeed;

  Scene.Scale := Vector3(-MoveDirection, 1, 1);

  RBody.LinearVelocity := Vel;
end;

procedure TEnemy.HitPlayer;
begin
  ViewPlay.HitPlayer;
  Dead := true;
  RBody.Exists := false;
end;

procedure TEnemy.TakeDamageFromBullet(const Bullet: TCastleTransform);
begin
  SoundEngine.Play(NamedSound('HitEnemy'));
  Bullet.Exists := false;

  Dead := true;
  RBody.Exists := false;
end;

procedure TEnemy.CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
begin
  if Dead then
    Exit;

  if CollisionDetails.OtherTransform.Name = 'ScenePlayer' then
    HitPlayer
  else
  if CollisionDetails.OtherTransform is TBullet then
    TakeDamageFromBullet(CollisionDetails.OtherTransform);
end;

end.
