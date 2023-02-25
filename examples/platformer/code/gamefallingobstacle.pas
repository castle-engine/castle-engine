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
    RBody: TCastleRigidBody;
    { Set to true when player was bellow and started falling }
    IsFalling: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure HitPlayer;

    procedure CollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
  end;

  TFallingObstaclesList = {$ifdef FPC}specialize{$endif} TObjectList<TFallingObstacle>;

implementation

uses GameViewPlay;

{ TFallingObstacle ----------------------------------------------------------- }

constructor TFallingObstacle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsFalling := false;
end;

procedure TFallingObstacle.ParentAfterAttach;
begin
  inherited ParentAfterAttach;

  Scene := Parent as TCastleScene;

  { TCastleRigidBody was added in the editor, here we configure only the events. }
  RBody := Scene.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody <> nil then
    RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}CollisionEnter;
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

    if RBody <> nil then
    begin
      RayHitThat := RBody.PhysicsRayCast(Scene.Translation, Vector3(0, -1, 0), 300).Transform;

      { Check was that a player? }
      if (RayHitThat <> nil) and (Pos('ScenePlayer', RayHitThat.Name) > 0) then
      begin
        { Start falling down }
        RBody.Gravity := true;
        RBody.LinearVelocity := Vector3(0, -500, 0);
        IsFalling := true;
      end;
    end;
  end;
end;

procedure TFallingObstacle.HitPlayer;
begin
  ViewPlay.HitPlayer;
  Scene.Exists := false;
end;

procedure TFallingObstacle.CollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  // TODO: We should check by instance reference, not by name
  if Pos('ScenePlayer', CollisionDetails.OtherTransform.Name) > 0 then
  begin
    HitPlayer;
  end else
  begin
    { Once the spike hits the ground, it should no longer be able to
      hurt the player. }
    RBody.Exists := false;
  end;
end;

end.
