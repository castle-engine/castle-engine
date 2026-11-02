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

  Shows a dungeon (maze) with a ball, visible from the top.
  Rotating the device (measured using the gyroscope) tilts the dungeon,
  and the ball rolls following the physics. }
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
    ButtonReset: TCastleButton;
    MainViewport: TCastleViewport;
    Ball: TCastleSphere;
    BallRigidBody: TCastleRigidBody;
  private
    { Delphi component to access the motion sensors, we use it to read gyroscope. }
    MotionSensor: TMotionSensor;
    { Did we find a sensor that reports the rotation speed. }
    GyroscopeAvailable: Boolean;
    { Is the rotation speed reported by the sensor in degrees per second
      (if not, then it is in radians per second). }
    GyroscopeInDegrees: Boolean;

    { Current tilt of the dungeon, in radians.
      TiltX is a rotation around the X axis (horizontal on the screen),
      TiltZ is a rotation around the Z axis (vertical on the screen,
      remember that we look at the dungeon from the top). }
    TiltX, TiltZ: Single;

    { Initial state, to implement reset and to calculate tilted camera. }
    InitialBallTranslation: TVector3;
    InitialCameraTranslation, InitialCameraDirection, InitialCameraUp: TVector3;

    procedure MotionSensorChoosing(Sender: TObject;
      const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure ClickReset(Sender: TObject);
    { Change the tilt based on gyroscope (and keys, to test on desktop). }
    procedure UpdateTilt(const SecondsPassed: Single);
    { Make the camera and gravity reflect current TiltX, TiltZ. }
    procedure ApplyTilt;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils;

const
  { Maximum tilt of the dungeon, in radians. }
  MaxTilt: Single = Pi / 6;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  ButtonReset.OnClick := ClickReset;

  InitialBallTranslation := Ball.Translation;
  MainViewport.Camera.GetWorldView(
    InitialCameraTranslation, InitialCameraDirection, InitialCameraUp);

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
  GyroscopeAvailable := false;
  for I := 0 to High(Sensors) do
    case (Sensors[I] as TCustomMotionSensor).SensorType of
      { On Android, gyroscope is a separate sensor.
        It reports the rotation speed in radians per second. }
      TMotionSensorType.Gyrometer3D:
        begin
          ChoseSensorIndex := I;
          GyroscopeAvailable := true;
          GyroscopeInDegrees := false;
          Exit;
        end;
      { On iOS, gyroscope data is available in the "device motion" sensor.
        It reports the rotation speed in degrees per second. }
      TMotionSensorType.MotionDetector:
        begin
          ChoseSensorIndex := I;
          GyroscopeAvailable := true;
          GyroscopeInDegrees := true;
          Exit;
        end;
    end;
end;

procedure TViewMain.ClickReset(Sender: TObject);
begin
  { Reset the tilt. Use this when you hold the device in a comfortable
    position, this position will be considered "flat" from now on.
    This is also useful because the tilt is calculated by summing gyroscope
    measurements, so the errors accumulate over time. }
  TiltX := 0;
  TiltZ := 0;
  ApplyTilt;

  { Reset the ball. }
  Ball.Translation := InitialBallTranslation;
  BallRigidBody.LinearVelocity := TVector3.Zero;
  BallRigidBody.AngularVelocity := TVector3.Zero;
end;

procedure TViewMain.UpdateTilt(const SecondsPassed: Single);
const
  { Tilt speed when using keys, in radians per second. }
  KeysTiltSpeed = 0.5;
var
  { Rotation speed of the device around device X, Y axes, in radians per second.
    When you hold the device in portrait orientation:
    - device X axis is horizontal, pointing to the right of the screen,
    - device Y axis is vertical, pointing to the top of the screen,
    - device Z axis points from the screen towards you. }
  DeviceRotationX, DeviceRotationY: Single;
begin
  if GyroscopeAvailable and (MotionSensor.Sensor <> nil) then
  begin
    { Note: The properties are called AngleAccelX/Y/Z,
      but for the gyroscope they are just rotation (angular) speed. }
    DeviceRotationX := MotionSensor.Sensor.AngleAccelX;
    DeviceRotationY := MotionSensor.Sensor.AngleAccelY;
    if GyroscopeInDegrees then
    begin
      DeviceRotationX := DegToRad(DeviceRotationX);
      DeviceRotationY := DegToRad(DeviceRotationY);
    end;

    { Gyroscope reports rotation speed, so we multiply it by time
      (SecondsPassed) to know how much did the device rotate since last frame.

      We look at the dungeon from the top:
      - Device X axis corresponds to the world X axis.
      - Device Y axis (pointing to the top of the screen) corresponds to
        the world -Z axis. This is the reason for the minus sign below.

      If the dungeon tilts in the opposite direction than you expect,
      just change the signs below. }
    TiltX := TiltX + DeviceRotationX * SecondsPassed;
    TiltZ := TiltZ - DeviceRotationY * SecondsPassed;
  end;

  { Allow to tilt using arrow keys, to test on desktops without gyroscope. }
  if Container.Pressed[keyArrowUp] then
    TiltX := TiltX - KeysTiltSpeed * SecondsPassed;
  if Container.Pressed[keyArrowDown] then
    TiltX := TiltX + KeysTiltSpeed * SecondsPassed;
  if Container.Pressed[keyArrowLeft] then
    TiltZ := TiltZ + KeysTiltSpeed * SecondsPassed;
  if Container.Pressed[keyArrowRight] then
    TiltZ := TiltZ - KeysTiltSpeed * SecondsPassed;

  TiltX := Clamped(TiltX, -MaxTilt, MaxTilt);
  TiltZ := Clamped(TiltZ, -MaxTilt, MaxTilt);
end;

procedure TViewMain.ApplyTilt;

  { Tilting the dungeon means rotating it: first around X axis by TiltX,
    then around Z axis by TiltZ.

    But we don't actually rotate the dungeon: it has a mesh collider
    (TCastleMeshCollider), which is the best collider for a static level,
    but it cannot be moved or rotated by physics.

    Instead we do something that looks exactly the same:
    we leave the dungeon unchanged, and we apply the reverse rotation
    to everything else: to the camera and to the gravity direction.

    This function applies this reverse rotation to the given vector. }
  function ReverseTilt(const V: TVector3): TVector3;
  begin
    Result := RotatePointAroundAxisRad(-TiltZ, V, Vector3(0, 0, 1));
    Result := RotatePointAroundAxisRad(-TiltX, Result, Vector3(1, 0, 0));
  end;

begin
  MainViewport.Camera.SetWorldView(
    ReverseTilt(InitialCameraTranslation),
    ReverseTilt(InitialCameraDirection),
    ReverseTilt(InitialCameraUp));

  { Physics gravity pulls in the direction of -Camera.GravityUp. }
  MainViewport.Camera.GravityUp := ReverseTilt(Vector3(0, 1, 0));

  { Physics engine puts to "sleep" bodies that don't move for some time.
    Sleeping body would not notice that the gravity direction changed,
    so make sure the ball is awake. }
  BallRigidBody.WakeUp;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  SensorInfo: String;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Apply the Container.SafeBorder, to not draw UI over mobile status bars,
    notches, etc. }
  RootGroup.Border.Assign(Container.SafeBorder);

  UpdateTilt(SecondsPassed);
  ApplyTilt;

  if GyroscopeAvailable then
    SensorInfo := 'Rotate the device to tilt the dungeon'
  else
    SensorInfo := 'Gyroscope not available, use arrow keys';
  LabelInfo.Caption := Format('%s' + sLineBreak + 'Tilt: %f, %f degrees', [
    SensorInfo,
    RadToDeg(TiltX),
    RadToDeg(TiltZ)
  ]);
end;

end.
