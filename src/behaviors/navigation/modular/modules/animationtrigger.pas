{
  Copyright 2023-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO docs. }
unit AnimationTrigger;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleClassUtils, CastleScene;

type

  { Animation trigger for TModularMovement. Changes animation by using
    TModularMovementState. It's very simple class in almost all real games
    you need implement your own animation triger or use TCastleTransform.AfterUpdateListener
    and check some modules state like in platformer example. }
  TAnimationTrigger = class(TAbstractMovementModule)
  strict private
    FIdleAnimation: String;
    FWalkAnimation: String;
    FFlyAnimation: String;
    FJumpAnimation: String;
    FFallAnimation: String;

    procedure SetAnimation(const PlayerScene: TCastleScene; const AnimationName: String);

    function IdleAnimationStored: Boolean;
    function WalkAnimationStored: Boolean;
    function FlyAnimationStored: Boolean;
    function JumpAnimationStored: Boolean;
    function FallAnimationStored: Boolean;
  public
    const
      DefaultIdleAnimation = 'idle';
      DefaultWalkAnimation = 'walk';
      DefaultFlyAnimation = 'fly';
      DefaultJumpAnimation = 'jump';
      DefaultFallAnimation = 'fall';

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
    property IdleAnimation: String read FIdleAnimation write FIdleAnimation stored IdleAnimationStored nodefault;
    property WalkAnimation: String read FWalkAnimation write FWalkAnimation stored WalkAnimationStored nodefault;
    property FlyAnimation: String read FFlyAnimation write FFlyAnimation stored FlyAnimationStored nodefault;
    property JumpAnimation: String read FJumpAnimation write FJumpAnimation stored JumpAnimationStored nodefault;
    property FallAnimation: String read FFallAnimation write FFallAnimation stored FallAnimationStored nodefault;
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TAnimationTrigger ------------------------------------------------------------ }

constructor TAnimationTrigger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIdleAnimation := DefaultIdleAnimation;
  FWalkAnimation := DefaultWalkAnimation;
  FFlyAnimation := DefaultFlyAnimation;
  FJumpAnimation := DefaultJumpAnimation;
  FFallAnimation := DefaultFallAnimation;
end;


procedure TAnimationTrigger.UpdateMovement(const MovementState: TModularMovementState);
var
  RBody: TCastleRigidBody;
  ParentScene: TCastleScene;
begin
  if not (Parent is TCastleScene) then
    Exit;

  ParentScene := TCastleScene(Parent);
  RBody := MovementState.RigidBody;

  { When we have gravity, no input direction and velocities are zero play idle
    animation }
  if TVector3.PerfectlyEquals(MovementState.InputDirection, TVector3.Zero) and
    TVector3.PerfectlyEquals(RBody.LinearVelocity, TVector3.Zero) and
    TVector3.PerfectlyEquals(RBody.AngularVelocity, TVector3.Zero) and
    RBody.Gravity
  then
  begin
    SetAnimation(ParentScene, FIdleAnimation);
    Exit;
  end;

  { When no gravity play fly animation }
  if not RBody.Gravity then
  begin
    SetAnimation(ParentScene, FFlyAnimation);
    Exit;
  end;

  { When not on goround and y velocity > 0 play jump animation }
  if (not MovementState.IsPlayerOnGround) and
    (RBody.LinearVelocity.Y > 0.02)
  then
  begin
    SetAnimation(ParentScene, FJumpAnimation);
    Exit;
  end;

  { When not on goround and y velocity < 0 play fall animation }
  if (not MovementState.IsPlayerOnGround) and
    (RBody.LinearVelocity.Y < -0.02)
  then
  begin
    SetAnimation(ParentScene, FFallAnimation);
    Exit;
  end;

  if MovementState.IsPlayerOnGround and
    MovementState.IsMoving
  then
  begin
    SetAnimation(ParentScene, FWalkAnimation);
    Exit;
  end;
end;

procedure TAnimationTrigger.SetAnimation(const PlayerScene: TCastleScene;
  const AnimationName: String);
begin
  if CastleDesignMode then
    Exit;

  if not PlayerScene.HasAnimation(AnimationName) then
  begin
    WritelnWarning(Name + ' no animation with name ' + AnimationName);
    Exit;
  end;

  if (PlayerScene.CurrentAnimation = nil) or (PlayerScene.CurrentAnimation.X3DName <> AnimationName) then
    PlayerScene.PlayAnimation(AnimationName, true)
end;

function TAnimationTrigger.IdleAnimationStored: Boolean;
begin
  Result := FIdleAnimation <> DefaultIdleAnimation;
end;

function TAnimationTrigger.WalkAnimationStored: Boolean;
begin
  Result := FWalkAnimation <> DefaultWalkAnimation;
end;

function TAnimationTrigger.FlyAnimationStored: Boolean;
begin
  Result := FFlyAnimation <> DefaultFlyAnimation;
end;

function TAnimationTrigger.JumpAnimationStored: Boolean;
begin
  Result := FJumpAnimation <> DefaultJumpAnimation;
end;

function TAnimationTrigger.FallAnimationStored: Boolean;
begin
  Result := FFallAnimation <> DefaultFallAnimation;
end;

function TAnimationTrigger.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'IdleAnimation', 'WalkAnimation', 'FlyAnimation', 'JumpAnimation',
      'FallAnimation'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TAnimationTrigger, ['Navigation', 'Modules', 'Animation Trigger']);

end.

