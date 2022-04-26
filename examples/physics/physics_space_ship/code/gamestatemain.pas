{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
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
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  Torque: TVector3;
  Direction: TVector3;
  Rotation: Single;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if InputUp then
  begin
    Direction := Vector3(0, 1, 0);
    Rotation := Ship.Rotation.W;
    Direction := RotatePointAroundAxisRad(Ship.Rotation.W, Direction, Vector3(0, 1, 0));

    ShipRigidBody.AddCentralForce(Direction * ThrustForce);
    ShipRigidBody.WakeUp;
  end;

  Torque := Vector3(0, 0, 0);

  if InputLeft then
     Torque := Vector3(0, 0, 1);

  if InputRight then
    Torque := Vector3(0, 0, -1);

  if not Torque.IsPerfectlyZero then
  begin
    ShipRigidBody.AddTorque(Torque * TorqueValue);
    ShipRigidBody.WakeUp;
  end;



end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
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
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
