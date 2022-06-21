{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  CastleScene, CastleViewport;

type
  { Main state, where most of the application logic takes place. }

  TBullet = class(TCastleTransform)
  strict private
    Duration: Single;
  public
    constructor Create(AOwner: TComponent; BulletSpriteScene: TCastleScene); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TStateMain = class(TUIState)
  strict private
    const
      ThrustForce = 300;
      TorqueValue = 10000;

    function InputLeft: Boolean;
    function InputRight: Boolean;
    function InputUp: Boolean;
    function InputDown: Boolean;

  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    Ship: TCastleTransform;
    ShipRigidBody: TCastleRigidBody;
    BulletSpriteScene: TCastleScene;
    Viewport: TCastleViewport;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, CastleLog, Math;

{ TBullet }

constructor TBullet.Create(AOwner: TComponent; BulletSpriteScene: TCastleScene);
var
  RBody: TCastleRigidBody;
  Collider: TCastleSphereCollider;
begin
  inherited Create(AOwner);

  Add(BulletSpriteScene);
  BulletSpriteScene.Visible := true;
  BulletSpriteScene.Translation := Vector3(0, 0, 0);

  RBody := TCastleRigidBody.Create(Self);
  RBody.Setup2D;
  RBody.Dynamic := true;
  RBody.MaximalLinearVelocity := 0;
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

{ TStateMain ----------------------------------------------------------------- }

function TStateMain.InputLeft: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyA] or
    Container.Pressed.Items[keyArrowLeft];
end;

function TStateMain.InputRight: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyD] or
    Container.Pressed.Items[keyArrowRight];
end;

function TStateMain.InputUp: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyW] or
    Container.Pressed.Items[keyArrowUp];
end;

function TStateMain.InputDown: Boolean;
begin
  Result :=
    Container.Pressed.Items[keyS] or
    Container.Pressed.Items[keyArrowDown];
end;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;

  Ship := DesignedComponent('Ship') as TCastleTransform;
  ShipRigidBody := Ship.RigidBody;

  BulletSpriteScene := TCastleScene.Create(FreeAtStop);
  BulletSpriteScene.URL := 'castle-data:/graphics/bullet/bullet.png';
  BulletSpriteScene.Scale := Vector3(2, 2, 2);

  Viewport := DesignedComponent('Viewport') as TCastleViewport;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
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
    ShipRigidBody.AddCentralForce(Direction * ThrustForce);
  end;

  Torque := Vector3(0, 0, 0);

  if InputLeft then
     Torque := Vector3(0, 0, 1);

  if InputRight then
    Torque := Vector3(0, 0, -1);

  if not Torque.IsPerfectlyZero then
    ShipRigidBody.AddTorque(Torque * TorqueValue);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
var
  Bullet: TBullet;
  Direction: TVector3;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  if Event.IsKey(keySpace) then
  begin
    Bullet := TBullet.Create(FreeAtStop, BulletSpriteScene);
    Bullet.Translation := Ship.LocalToWorld(Vector3(0, Ship.LocalBoundingBox.SizeY / 2 + 5 , 0));
    Viewport.Items.Add(Bullet);

    Direction := Ship.LocalToWorldDirection(Vector3(0,1,0));
    { Change Bullet.Translation to Vector3(0,1,0) to keep the bullets rotating }
    Bullet.RigidBody.ApplyImpulse(Direction * 500, {Vector3(0,1,0)} Bullet.Translation);
    Exit(true); // key was handled
  end;
end;

end.
