{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place.

  Shows a room with a number of boxes and spheres, following the physics.
  The gravity in this room follows the real gravity,
  measured using the device accelerometer.
  So the objects always fall "down" in the real world, however you rotate
  the device, and you can shake the device to throw them around. }
unit GameViewMain;

interface

uses Classes,
  { Delphi units to access the sensors, available on all platforms. }
  System.Sensors, System.Sensors.Components,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, CastleViewport, CastleTransform;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    RootGroup: TCastleUserInterface;
    LabelFps: TCastleLabel;
    LabelInfo: TCastleLabel;
    MainViewport: TCastleViewport;
    DynamicObjects: TCastleTransform;
  private
    { Delphi component to access the motion sensors, we use it to read accelerometer. }
    MotionSensor: TMotionSensor;
    { Did we find the accelerometer sensor. }
    AccelerometerAvailable: Boolean;
    { Acceleration to use when the accelerometer is not available,
      can be changed by keys, to test on desktops. }
    FakeAcceleration: TVector3;
    procedure MotionSensorChoosing(Sender: TObject;
      const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    { Current acceleration, in device coordinates, in G units
      (so the vector length is 1 when the device is not moving). }
    function GetAcceleration(const SecondsPassed: Single): TVector3;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math;

const
  { Acceleration in meters per second squared equal to 1 G. }
  StandardGravity = 9.81;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  FakeAcceleration := Vector3(0, -1, 0);

  { Create and start the motion sensor.
    A device has multiple motion sensors (accelerometer, gyroscope...),
    in MotionSensorChoosing we choose the one we want. }
  MotionSensor := TMotionSensor.Create(FreeAtStop);
  MotionSensor.OnSensorChoosing := MotionSensorChoosing;
  MotionSensor.Active := true;
end;

procedure TViewMain.Stop;
begin
  MotionSensor.Active := false;
  inherited;
end;

procedure TViewMain.MotionSensorChoosing(Sender: TObject;
  const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I: Integer;
begin
  { There are various accelerometer sensors possible, see
    https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Sensors.TMotionSensorType .
    We want one that gives us 3D acceleration. }
  AccelerometerAvailable := false;
  for I := 0 to High(Sensors) do
    if (Sensors[I] as TCustomMotionSensor).SensorType = TMotionSensorType.Accelerometer3D then
    begin
      ChoseSensorIndex := I;
      AccelerometerAvailable := true;
      Exit;
    end;
end;

function TViewMain.GetAcceleration(const SecondsPassed: Single): TVector3;
const
  { How fast do the keys change the fake acceleration. }
  KeysSpeed = 2;
begin
  if AccelerometerAvailable and (MotionSensor.Sensor <> nil) then
  begin
    { The accelerometer reports acceleration in G units.

      When the device is not moving, this is a vector of length 1,
      pointing towards the ground, in device coordinates.
      When you hold the device in portrait orientation:
      - device X axis is horizontal, pointing to the right of the screen,
      - device Y axis is vertical, pointing to the top of the screen,
      - device Z axis points from the screen towards you.

      So it is (0, -1, 0) when you hold the device vertically
      and (0, 0, -1) when the device lies on the table with screen up.

      When the device is moving (e.g. you shake it), this vector changes
      accordingly. }
    Result := Vector3(
      MotionSensor.Sensor.AccelerationX,
      MotionSensor.Sensor.AccelerationY,
      MotionSensor.Sensor.AccelerationZ
    );
  end else
  begin
    { Allow to change the acceleration using arrow keys,
      to test on desktops without accelerometer. }
    if Container.Pressed[keyArrowLeft] then
      FakeAcceleration.X := FakeAcceleration.X - KeysSpeed * SecondsPassed;
    if Container.Pressed[keyArrowRight] then
      FakeAcceleration.X := FakeAcceleration.X + KeysSpeed * SecondsPassed;
    if Container.Pressed[keyArrowDown] then
      FakeAcceleration.Y := FakeAcceleration.Y - KeysSpeed * SecondsPassed;
    if Container.Pressed[keyArrowUp] then
      FakeAcceleration.Y := FakeAcceleration.Y + KeysSpeed * SecondsPassed;
    Result := FakeAcceleration;
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  Acceleration: TVector3;
  AccelerationLength: Single;
  Child: TCastleTransform;
  RigidBody: TCastleRigidBody;
  SensorInfo: String;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Apply the Container.SafeBorder, to not draw UI over mobile status bars,
    notches, etc. }
  RootGroup.Border.Assign(Container.SafeBorder);

  Acceleration := GetAcceleration(SecondsPassed);
  AccelerationLength := Acceleration.Length;

  { Use the acceleration as the physics gravity.

    The device coordinates (see GetAcceleration) match
    the camera coordinates in our design: X goes to the right,
    Y goes up, Z goes towards the viewer. So we can use the acceleration
    vector directly, without any conversion.

    Physics gravity pulls in the direction -Camera.GravityUp,
    with the strength PhysicsProperties.GravityStrength.

    TODO: The fact that MainViewport.Camera.GravityUp controls the gravity
    direction is a leftover from the old design.
    It's a TODO to remove it (see TODO at TCastleAbstractRootTransform.GravityUp).
    We should expose instead MainViewport.Items.PhysicsProperties.GravityUp
    vector, and then this will be simpler: maybe even just this:

      MainViewport.Items.PhysicsProperties.GravityUp := -Acceleration;
  }
  if AccelerationLength > 0.01 then
  begin
    MainViewport.Camera.GravityUp := -Acceleration / AccelerationLength;
    MainViewport.Items.PhysicsProperties.GravityStrength :=
      StandardGravity * AccelerationLength;
  end else
    MainViewport.Items.PhysicsProperties.GravityStrength := 0;

  { Physics engine puts to "sleep" bodies that don't move for some time.
    Sleeping body would not notice that the gravity changed,
    so make sure all the bodies are awake. }
  for Child in DynamicObjects do
  begin
    RigidBody := Child.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if RigidBody <> nil then
      RigidBody.WakeUp;
  end;

  if AccelerometerAvailable then
    SensorInfo := 'Rotate and shake the device'
  else
    SensorInfo := 'Accelerometer not available, use arrow keys';
  LabelInfo.Caption := Format('%s' + sLineBreak +
    'Acceleration (in G):' + sLineBreak +
    '  X: %f' + sLineBreak +
    '  Y: %f' + sLineBreak +
    '  Z: %f' + sLineBreak +
    '  Length: %f', [
    SensorInfo,
    Acceleration.X,
    Acceleration.Y,
    Acceleration.Z,
    AccelerationLength
  ]);
end;

end.
