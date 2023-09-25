{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleCameras, CastleTransform,
  CastleViewport, SimplestFpsPlayerMovement, SimpleFpsPlayerMovementWithRotation,
  DirectRotateTransformByKeys, RotateRigidBody, HeadBobbing, FpsCrouch, CastleInputAxis, RotateCamera,
  ModularMovement, StairsSupport, Fly3DSupport, Walk3DSupport,
  AnimationTrigger, FollowingTargetForCamera, DoubleJumpSupport, InAir3DControl;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelFlyWalk: TCastleLabel;
    Viewport: TCastleViewport;

    WalkNavigation: TCastleWalkNavigation;

    Player: TCastleTransform;
    FlySupport: TFly3DSupport;
    WalkSupport: TWalk3DSupport;
    RotateRigidBody: TRotateRigidBody;

    SimplestFpsPlayerMovement: TSimplestFpsPlayerMovement;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  { New modular navigation, that you can expand by implementing
    TAbstractMovementModifier.

    See TWalk3DSupport, TFpsFlySupport, THeadBobbing, TFpsCrouch for example }
  DesignUrl := 'castle-data:/gameviewmain_behaviors_modular_movement.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  if WalkNavigation <> nil then
    WalkNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  procedure AddCollectionOfBoxes;
  var
    Boxes: TCastleTransform;
  begin
    Boxes := TransformLoad('castle-data:/collection_of_boxes.castle-transform', FreeAtStop);
    if Player <> nil then
    begin
      Boxes.Translation := Viewport.Camera.LocalToWorld(Viewport.Camera.Translation) + Vector3(0, 3, 0) + Viewport.Camera.LocalToWorldDirection(Viewport.Camera.Direction) * 20;
      Boxes.Direction := Viewport.Camera.LocalToWorldDirection(Viewport.Camera.Direction);
    end else
    begin
      Boxes.Translation := Viewport.Camera.Translation + Vector3(0, 3, 0) + Viewport.Camera.Direction * 20;
      Boxes.Direction := Viewport.Camera.Direction;
    end;

    Viewport.Items.Add(Boxes);
  end;

  procedure AddBullet;
  var
    BulletOwner: TComponent;
    Bullet: TCastleTransform;
    BulletRigidBody: TCastleRigidBody;
    PlayerCollider: TCastleCollider;
  begin
    { Bullet's owner is BulletOwner, not directly FreeAtStop.
      This way we know that names are local within BulletOwner,
      and we can later look for 'BulletRigidBody' without risking that we
      will find rigid body from some older bullet. }
    BulletOwner := TComponent.Create(FreeAtStop);
    Bullet := TransformLoad('castle-data:/bullet_with_physics.castle-transform', BulletOwner);

    if Player <> nil then
    begin
      PlayerCollider := Player.FindBehavior(TCastleCollider) as TCastleCollider;
      { Code for player that can rotate }
      Bullet.Translation := PlayerCollider.Middle + Vector3(0, 0.75, 0) + Player.Direction.Normalize * 1.5;

      { Camera shows things in oposite to its direction }
      Bullet.Direction := Player.Direction;
      Bullet.Collides := false; // do not collide with player
    end;

    BulletRigidBody := BulletOwner.FindRequiredComponent('BulletRigidBody') as TCastleRigidBody;
    BulletRigidBody.LinearVelocity := Bullet.Direction * 25 + Vector3(0, 2, 0);
    { You can turn off gravity for Bullet to make it easier to shoot high objects
      even when initial LinearVelocity would be low.
      Of course this is non-realistic. }
    //BulletRigidBody.Gravity := false;
    Viewport.Items.Add(Bullet);
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    if mkShift in Event.ModifiersDown then
      AddCollectionOfBoxes
    else
      AddBullet;
    Exit(true);
  end;

  { Start mouse look }
  if Event.IsMouseButton(buttonRight) then
  begin
    Container.StartMouseLook(Viewport);
    WritelnLog('Mouse look started');
  end;

  { Fly/walk support in modular navigation - by change FlySupport/WalkSupport
    existance. }
  if Assigned(FlySupport) and Assigned(WalkSupport) and Assigned(RotateRigidBody) then
  begin
    if Event.IsKey(keyF) then
    begin
      if FlySupport.Exists then
      begin
        FlySupport.Exists := false;
        WalkSupport.Exists := true;
        LabelFlyWalk.Caption := 'Walking';
        RotateRigidBody.HorizontalRotationInput.PositiveKey := keyD;
        RotateRigidBody.HorizontalRotationInput.NegativeKey := keyA;
        RotateRigidBody.RotationHorizontalSpeed := 1.5;
      end else
      begin
        FlySupport.Exists := true;
        WalkSupport.Exists := false;
        LabelFlyWalk.Caption := 'Flying';
        RotateRigidBody.HorizontalRotationInput.PositiveKey := keyD;
        RotateRigidBody.HorizontalRotationInput.NegativeKey := keyA;
        RotateRigidBody.RotationHorizontalSpeed := 0.4;
      end;
    end;
  end;
end;

function TViewMain.Release(const Event: TInputPressRelease): boolean;
begin
  { Show cursor for mouse look }
  if Event.IsMouseButton(buttonRight) then
  begin
    Container.StopMouseLook;
    WritelnLog('Mouse look stoped.');
  end;

  Result := inherited Release(Event);
end;

end.
