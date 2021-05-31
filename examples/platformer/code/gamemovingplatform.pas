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
    MoveDirection: Integer; //< Always 1 or -1
    StartPoint: TVector3;
    StopPoint: TVector3;

    function IsVerticalMove: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentChanged; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TMovingPlatformList = specialize TObjectList<TMovingPlatform>;

implementation

uses
  CastleLog,
  GameStatePlay;

{ TMovingPlatform }

function TMovingPlatform.IsVerticalMove: Boolean;
begin
  Result := (Scene.Tag < 0)
end;

constructor TMovingPlatform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MoveDirection := -1;
end;

procedure TMovingPlatform.ParentChanged;
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
    // vertical move
    StopPoint := StartPoint + Vector3(0, Distance, 0)
  else
    // horizontal move
    StopPoint := StartPoint + Vector3(Distance, 0, 0);

end;

procedure TMovingPlatform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MovingSpeed = 100;
var
  // DeltaX: Single;
  Vel: TVector3;
  // T: TVector3;
begin
  inherited;

  {if IsVerticalMove then
  begin

  end else
  begin
    DeltaX := SecondsPassed * MovingSpeed;
    T := Scene.Translation;
    if MoveDirection > 0 then
    begin
      if Scene.Translation.X + DeltaX > StopPoint.X then
        MoveDirection := MoveDirection * -1
      else
      begin
        T.X := T.X + SecondsPassed * MovingSpeed;
        Scene.Translation := T;
        //Scene.RigidBody
      end;
    end else
    begin
      if Scene.Translation.X - DeltaX < StartPoint.X then
        MoveDirection := MoveDirection * -1
      else
      begin
        T.X := T.X - SecondsPassed * MovingSpeed;
        Scene.Translation := T;
      end;
    end;
  end;}

  {if (IsVerticalMove and ((Scene.Translation.Y > StopPoint.Y)
     or (Scene.Translation.Y < StartPoint.Y))) or ((IsVerticalMove = false) and
     (
     or (Scene.Translation.X < StartPoint.X))) then
     MoveDirection := - MoveDirection;}
  if IsVerticalMove = false then
  begin
    if Scene.Translation.X > StopPoint.X - 20 then
       MoveDirection := - 1
    else
      if Scene.Translation.X < StartPoint.X + 20 then
        MoveDirection := 1;
  end;

  Vel := Scene.RigidBody.LinearVelocity;

  if IsVerticalMove then
    Vel.Y := MoveDirection * MovingSpeed
  else
    Vel.X := MoveDirection * MovingSpeed;

  Scene.RigidBody.LinearVelocity := Vel;
end;

end.

