{ Enemy behaviour.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameEnemy;

{$mode objfpc}{$H+}

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  TEnemy = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    MoveDirection: Integer; //< Always 1 or -1
    Dead: Boolean;
    DontFallDown: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentChanged; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Hit;

    procedure CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  end;

TEnemyList = specialize TObjectList<TEnemy>;

implementation

{ TEnemy }

constructor TEnemy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MoveDirection := -1;
  DontFallDown := true;
end;

procedure TEnemy.ParentChanged;
begin
  inherited;
  Scene := Parent as TCastleScene; // TEnemy can only be added as behavior to TCastleScene
  Scene.PlayAnimation('walk', true);
  Scene.RigidBody.OnCollisionEnter := @CollisionEnter;
end;

procedure TEnemy.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MovingSpeed = 200;
var
  EnemyOnGround: Boolean;
  Distance: Single;
  NeedTurn: Boolean;
  Vel: TVector3;
  RayMaxDistance: Single;
begin
  inherited;

  if Dead then
  begin
    if Scene.CurrentAnimation.X3DName <> 'dead' then
      Scene.PlayAnimation('dead', true);
    Exit;
  end;

  RayMaxDistance := Scene.BoundingBox.SizeY * 0.50 + 5;

  EnemyOnGround := Scene.RigidBody.PhysicsRayCast(Scene.Translation,
  Vector3(0, -1, 0), RayMaxDistance) <> nil;

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
    NeedTurn := Scene.RigidBody.PhysicsRayCast(Scene.Translation
      + Vector3(MoveDirection * Scene.BoundingBox.SizeX * 0.50, 0, 0),
      Vector3(0, -1, 0), RayMaxDistance) = nil;
  end else
    NeedTurn := false;

  if NeedTurn then
    MoveDirection := - MoveDirection;

  Vel := Scene.RigidBody.LinearVelocity;

  Vel.X := MoveDirection * MovingSpeed;

  Scene.Scale := Vector3(-MoveDirection, 1, 1);

  Scene.RigidBody.LinearVelocity := Vel;
end;

procedure TEnemy.Hit;
begin
  Dead := true;
end;

procedure TEnemy.CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform.Name = 'ScenePlayer' then
  begin
    Hit;
  end;
end;

end.

