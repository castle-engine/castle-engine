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

{ Moving Platform. }
unit GameMovingPlatform;

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleScene, CastleTransform;

type
  TMovingPlatform = class(TCastleBehavior)
  strict private
    Scene: TCastleScene;
    RBody: TCastleRigidBody;
    MoveDirection: Integer; // Always 1 or -1
    StartPoint: TVector3;
    StopPoint: TVector3;

    function IsVerticalMove: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TMovingPlatformList = {$ifdef FPC}specialize{$endif} TObjectList<TMovingPlatform>;

implementation

uses
  CastleLog,
  GameViewPlay;

{ TMovingPlatform ------------------------------------------------------------ }

function TMovingPlatform.IsVerticalMove: Boolean;
begin
  Result := (Scene.Tag < 0)
end;

constructor TMovingPlatform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MoveDirection := -1;
end;

procedure TMovingPlatform.ParentAfterAttach;

  procedure ConfigurePhysics;
  var
    Collider: TCastleCollider;
  begin
    RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    Assert(RBody <> nil, 'No TCastleRigidBody in TMovingPlatform Parent');

    RBody.Dynamic := true;
    if Scene.Tag > 0 then
      RBody.LockTranslation := [1, 2]
    else
    if Scene.Tag < 0 then
      RBody.LockTranslation := [0, 2];

    Collider := Scene.FindBehavior(TCastleCollider) as TCastleCollider;

    Assert(Collider <> nil, 'No Castle Collider in Parent');
    Collider.Friction := 100;
    Collider.Restitution := 0;
  end;

var
  Distance: Single;
begin
  inherited;
  Scene := Parent as TCastleScene;

  if Scene.Scale.X < 0 then
    MoveDirection := 1
  else
    MoveDirection := -1;

  StartPoint := Scene.Translation;

  Distance := Abs(Scene.Tag);

  if IsVerticalMove then
    StopPoint := StartPoint + Vector3(0, Distance, 0)  // vertical move
  else
    StopPoint := StartPoint + Vector3(Distance, 0, 0); // horizontal move

  ConfigurePhysics;
end;

procedure TMovingPlatform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MovingSpeed = 100;
var
  Vel: TVector3;
begin
  inherited;

  if IsVerticalMove then
  begin
    if Scene.Translation.Y > StopPoint.Y then
      MoveDirection := - 1
    else
    if Scene.Translation.Y < StartPoint.Y then
      MoveDirection := 1;
  end else
  begin
    if Scene.Translation.X > StopPoint.X then
      MoveDirection := - 1
    else
    if Scene.Translation.X < StartPoint.X then
      MoveDirection := 1;
  end;

  if RBody <> nil then
  begin
    Vel := RBody.LinearVelocity;

    if IsVerticalMove then
      Vel.Y := MoveDirection * MovingSpeed
    else
      Vel.X := MoveDirection * MovingSpeed;

    RBody.LinearVelocity := Vel;
  end;
end;

end.
