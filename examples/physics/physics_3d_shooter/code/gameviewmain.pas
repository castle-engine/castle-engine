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
  ModularMovement, StairsSupport, Fly3DSupport, Walk3DSupport;

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

    See TFpsWalkSupport, TFpsFlySupport, THeadBobbing, TFpsCrouch for example }
  DesignUrl := 'castle-data:/gameviewmain_behaviors_modular_movement.castle-user-interface';

  { Old direct walk navigation }
  //DesignUrl := 'castle-data:/gameviewmain_direct.castle-user-interface';

  { Simplest navigation - rotation only in camera no player rotation. When
    you want something really simple to start experiment. }
  // DesignUrl := 'castle-data:/gameviewmain_behaviors_simplest_inputaxis.castle-user-interface';

  { Horizontal rotation in player physics - rotate rigid body using angular velocity -
    start point for your own navigation if you don't want to use modular version. }
  // DesignUrl := 'castle-data:/gameviewmain_behaviors_simple_with_rotation_physics.castle-user-interface';
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
    CameraDirectionForBullet: TVector3;
  begin
    { Bullet's owner is BulletOwner, not directly FreeAtStop.
      This way we know that names are local within BulletOwner,
      and we can later look for 'BulletRigidBody' without risking that we
      will find rigid body from some older bullet. }
    BulletOwner := TComponent.Create(FreeAtStop);
    Bullet := TransformLoad('castle-data:/bullet_with_physics.castle-transform', BulletOwner);
    //Bullet := TransformLoad('castle-data:/bullet_with_physics_sphere.castle-transform', BulletOwner);
    //Bullet := TransformLoad('castle-data:/bullet_with_physics_capsule.castle-transform', BulletOwner);

    if Player <> nil then
    begin
      if SimplestFpsPlayerMovement = nil then
      begin
        { Code for player that can rotate }
        Bullet.Translation := Viewport.Camera.LocalToWorld(Viewport.Camera.Translation) + Player.Direction.Normalize * 1.5;

        { Camera shows things in oposite to its direction }
        CameraDirectionForBullet := -Viewport.Camera.Direction;
        CameraDirectionForBullet.Y := -CameraDirectionForBullet.Y;

        Bullet.Direction := Viewport.Camera.LocalToWorldDirection(CameraDirectionForBullet);
        Bullet.Collides := false; // do not collide with player
      end else
      begin
        { When you use SimplestFpsPlayerMovement player never rotates so you
          need use camera direction }

        Bullet.Translation := Viewport.Camera.LocalToWorld(Viewport.Camera.Translation) + Viewport.Camera.Direction * 1.5;
        Bullet.Direction := Viewport.Camera.Direction;
        Bullet.Collides := false; // do not collide with player
      end;
    end else
    begin
      { Code for old walk navigation. }
      Bullet.Translation := Viewport.Camera.Translation + Viewport.Camera.Direction * 1.5;
      Bullet.Direction := Viewport.Camera.Direction;
      Bullet.Collides := false; // do not collide with player
    end;

    BulletRigidBody := BulletOwner.FindRequiredComponent('BulletRigidBody') as TCastleRigidBody;
    BulletRigidBody.LinearVelocity := Bullet.Direction * 25;
    { You can turn off gravity for Bullet to make it easier to shoot high objects
      even when initial LinearVelocity would be low.
      Of course this is non-realistic. }
    //BulletRigidBody.Gravity := false;
    Viewport.Items.Add(Bullet);

    { Instead of setting BulletRigidBody.LinearVelocity directly (see line above)
      you can also add force to push the bullet.
      Both ApplyImpulse and AddForce can be used.
      Try it out -- comment out above "BulletRigidBody.LinearVelocity := ..."
      and uncomment one of the lines below. }
    //BulletRigidBody.ApplyImpulse(Viewport.Camera.Direction * 50, Viewport.Camera.Translation);
    //BulletRigidBody.AddForce(Viewport.Camera.Direction * 5000, false);
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    Container.StartMouseDrag;
    WritelnLog('Mouse drag started');

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
        RotateRigidBody.HorizontalRotationInput.PositiveKey := keyArrowRight;
        RotateRigidBody.HorizontalRotationInput.NegativeKey := keyArrowLeft;
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

  if Event.IsMouseButton(buttonLeft) then
  begin
    Container.StopMouseDrag;
    WritelnLog('Mouse drag stoped');
  end;

  Result := inherited Release(Event);
end;

end.
