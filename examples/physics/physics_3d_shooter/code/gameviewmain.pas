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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleCameras,
  CastleViewport, Move3DPlayerDynamic, RotateCameraByMouse,
  SimplestFpsPlayerMovement, RotateCameraByKeys, SimpleFpsPlayerMovementWithRotation,
  DirectRotateTransformByKeys, RotateRigidBodyByKeys, RotateRigidBodyByMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    WalkNavigation: TCastleWalkNavigation;
    Viewport: TCastleViewport;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleTransform;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  { Rotation in camera only no player rotation }
  //DesignUrl := 'castle-data:/gameviewmain_behaviors_simplest.castle-user-interface';
  { Rotation in player direct - rotate transform and synchronize physics - fall out level sometimes  }
  //DesignUrl := 'castle-data:/gameviewmain_behaviors_simple_with_rotation_direct.castle-user-interface';
  { Rotation in player physics - rotate rigid body by mouse and angular velocity }
  //DesignUrl := 'castle-data:/gameviewmain_behaviors_simple_with_rotation_physics_mouse.castle-user-interface';
  { Rotation in player physics - rotate rigid body by keys and angular velocity }
  DesignUrl := 'castle-data:/gameviewmain_behaviors_simple_with_rotation_physics_keys.castle-user-interface';
  //DesignUrl := 'castle-data:/gameviewmain_behaviors_simple_with_rotation_physics.castle-user-interface';

  {Old tests }
  //DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  //DesignUrl := 'castle-data:/gameviewmain_scaled_player_test.castle-user-interface';
  //DesignUrl := 'castle-data:/gameviewmain_behaviors.castle-user-interface';

  //DesignUrl := 'castle-data:/gameviewmain_direct.castle-user-interface';
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
  WalkNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  procedure AddCollectionOfBoxes;
  var
    Boxes: TCastleTransform;
  begin
    Boxes := TransformLoad('castle-data:/collection_of_boxes.castle-transform', FreeAtStop);
    Boxes.Translation := Viewport.Camera.Translation + Vector3(0, 3, 0) + Viewport.Camera.Direction * 20;
    Boxes.Direction := Viewport.Camera.Direction;
    Viewport.Items.Add(Boxes);
  end;

  procedure AddBullet;
  var
    BulletOwner: TComponent;
    Bullet: TCastleTransform;
    BulletRigidBody: TCastleRigidBody;
  begin
    { Bullet's owner is BulletOwner, not directly FreeAtStop.
      This way we know that names are local within BulletOwner,
      and we can later look for 'BulletRigidBody' without risking that we
      will find rigid body from some older bullet. }
    BulletOwner := TComponent.Create(FreeAtStop);
    Bullet := TransformLoad('castle-data:/bullet_with_physics.castle-transform', BulletOwner);
    Bullet.Translation := Viewport.Camera.Parent.Translation + Viewport.Camera.Translation + Viewport.Camera.Direction * 1.5;
    Bullet.Direction := Viewport.Camera.Direction;
    Bullet.Collides := false; // do not collide with player
    BulletRigidBody := BulletOwner.FindRequiredComponent('BulletRigidBody') as TCastleRigidBody;
    BulletRigidBody.LinearVelocity := Viewport.Camera.Direction * 100;
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
    if mkShift in Event.ModifiersDown then
      AddCollectionOfBoxes
    else
      AddBullet;
    Exit(true);
  end;
end;

end.
