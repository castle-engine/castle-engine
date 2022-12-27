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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  CastleScene, CastleViewport;

type
  { Main view, where most of the application logic takes place. }

  TBullet = class(TCastleTransform)
  strict private
    Duration: Single;
  public
    constructor Create(AOwner: TComponent; const BulletSpriteImage: TCastleTransform); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Ship: TCastleTransform;
    ShipRigidBody: TCastleRigidBody;
    Viewport: TCastleViewport;
  strict private
    const
      ThrustForce = 300;
      TorqueValue = 10000;
    var
      BulletSpriteImage: TCastleImageTransform;
    function InputLeft: Boolean;
    function InputRight: Boolean;
    function InputUp: Boolean;
    function InputDown: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleLog, Math;

{ TBullet -------------------------------------------------------------------- }

constructor TBullet.Create(AOwner: TComponent; const BulletSpriteImage: TCastleTransform);
var
  RBody: TCastleRigidBody;
  Collider: TCastleSphereCollider;
begin
  inherited Create(AOwner);

  // BulletSpriteImage is reused for all bullets
  Add(BulletSpriteImage);

  RBody := TCastleRigidBody.Create(Self);
  RBody.Setup2D;
  RBody.Dynamic := true;
  RBody.MaxLinearVelocity := 0;
  RBody.Gravity := false;
  AddBehavior(RBody);

  Collider := TCastleSphereCollider.Create(Self);
  Collider.Mass := 1;

  AddBehavior(Collider);
end;

procedure TBullet.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);

  Duration := Duration + SecondsPassed;
  if Duration > 3 then
    RemoveMe := rtRemoveAndFree;
end;

{ TViewMain ----------------------------------------------------------------- }

function TViewMain.InputLeft: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyA] or
    Container.Pressed.Items[keyArrowLeft];
end;

function TViewMain.InputRight: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyD] or
    Container.Pressed.Items[keyArrowRight];
end;

function TViewMain.InputUp: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyW] or
    Container.Pressed.Items[keyArrowUp];
end;

function TViewMain.InputDown: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyS] or
    Container.Pressed.Items[keyArrowDown];
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  BulletSpriteImage := TCastleImageTransform.Create(FreeAtStop);
  BulletSpriteImage.URL := 'castle-data:/graphics/bullet/bullet.png';
  BulletSpriteImage.Scale := Vector3(2, 2, 2);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  Torque: TVector3;
  Direction: TVector3;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if InputUp then
  begin
    Direction := Vector3(0, 1, 0);
    ShipRigidBody.AddForce(Direction * ThrustForce, true);
  end;

  Torque := Vector3(0, 0, 0);

  if InputLeft then
    Torque := Vector3(0, 0, 1);

  if InputRight then
    Torque := Vector3(0, 0, -1);

  if not Torque.IsPerfectlyZero then
    ShipRigidBody.AddTorque(Torque * TorqueValue);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  Bullet: TBullet;
  BullletBody: TCastleRigidBody;
  Direction: TVector3;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  if Event.IsKey(keySpace) then
  begin
    Bullet := TBullet.Create(FreeAtStop, BulletSpriteImage);
    Bullet.Translation := Ship.LocalToWorld(Vector3(0, Ship.LocalBoundingBox.SizeY / 2 + 5 , 0));
    Viewport.Items.Add(Bullet);

    Direction := Ship.LocalToWorldDirection(Vector3(0,1,0));

    BullletBody := Bullet.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    { Change Bullet.Translation to Vector3(0,1,0) to keep the bullets rotating }
    BullletBody.ApplyImpulse(Direction * 500, {Vector3(0,1,0)} Bullet.Translation);

    Exit(true); // key was handled
  end;
end;

end.
