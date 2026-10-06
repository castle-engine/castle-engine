unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Sensors, System.Sensors.Components, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation,
  Fmx.CastleControl,
  CastleScene, CastleViewport, CastleControls, CastleUiControls, CastleTransform;

type
  TForm1 = class(TForm)
    LayoutInfo: TLayout;
    LabelStatus: TLabel;
    LabelLatitude: TLabel;
    LabelLongitude: TLabel;
    LabelAltitude: TLabel;
    LabelSpeed: TLabel;
    LabelHeading: TLabel;
    LabelAccuracy: TLabel;
    ButtonTestLocation: TButton;
    CastleControl1: TCastleControl;
    LocationSensor1: TLocationSensor;
    procedure FormCreate(Sender: TObject);
    procedure ButtonTestLocationClick(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
  private
    { Components designed using CGE editor, in data/main.castle-user-interface. }
    MainViewport: TCastleViewport;
    Pin: TCastleTransform;
    LabelFps: TCastleLabel;
    { Did we already move the camera to look at the location. }
    CameraLooksAtLocation: Boolean;
    procedure LocationPermissionsResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    { Show given location (in degrees) on the 3D Earth. }
    procedure ShowLocation(const Latitude, Longitude: Double);
    procedure DoUpdate(const Sender: TCastleUserInterface;
      const SecondsPassed: Single; var HandleInput: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.Math,
  CastleVectors;

{$R *.fmx}

const
  { Android permissions needed to get the location.
    On other platforms, requesting them is harmless
    (they are automatically reported as "granted"). }
  PermissionFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
  PermissionCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';

{ Convert floating-point value to String.
  Location sensor reports NaN for information that is not available. }
function ValueToStr(const Value: Double; const Suffix: String): String;
begin
  if IsNan(Value) then
    Result := 'not available'
  else
    Result := FormatFloat('0.00', Value) + Suffix;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CastleControl1.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Initialize references to components designed using CGE editor,
    saved in data/main.castle-user-interface. }
  MainViewport := CastleControl1.Container.DesignedComponent('MainViewport') as TCastleViewport;
  Pin := CastleControl1.Container.DesignedComponent('Pin') as TCastleTransform;
  LabelFps := CastleControl1.Container.DesignedComponent('LabelFps') as TCastleLabel;

  { Assign event to some OnUpdate, to update FPS display. }
  LabelFps.OnUpdate := DoUpdate;

  { Ask user for the permission to access the location.
    The result (also when the permission is already granted)
    is passed to LocationPermissionsResult. }
  PermissionsService.RequestPermissions(
    [PermissionFineLocation, PermissionCoarseLocation],
    LocationPermissionsResult);
end;

procedure TForm1.LocationPermissionsResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
var
  I: Integer;
  AnyGranted: Boolean;
begin
  { User may grant only the coarse (approximate) location, which is OK for us. }
  AnyGranted := false;
  for I := 0 to High(AGrantResults) do
    if AGrantResults[I] = TPermissionStatus.Granted then
      AnyGranted := true;

  if AnyGranted then
  begin
    { Start the location sensor. It will call LocationSensor1LocationChanged
      when the location is known, and later each time it changes. }
    LocationSensor1.Active := true;
    if LocationSensor1.Active then
      LabelStatus.Text := 'Waiting for location...'
    else
      LabelStatus.Text := 'Location sensor is not available';
  end else
    LabelStatus.Text := 'No permission to access the location';
end;

procedure TForm1.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  Sensor: TCustomLocationSensor;
begin
  LabelStatus.Text := 'Location received at ' + TimeToStr(Now);
  LabelLatitude.Text := 'Latitude: ' + ValueToStr(NewLocation.Latitude, ' deg');
  LabelLongitude.Text := 'Longitude: ' + ValueToStr(NewLocation.Longitude, ' deg');

  { Additional information is available using the Sensor property. }
  Sensor := LocationSensor1.Sensor;
  if Sensor <> nil then
  begin
    LabelAltitude.Text := 'Altitude: ' + ValueToStr(Sensor.Altitude, ' m');
    LabelSpeed.Text := 'Speed: ' + ValueToStr(Sensor.Speed, ' m/s');
    LabelHeading.Text := 'Heading: ' + ValueToStr(Sensor.TrueHeading, ' deg');
    LabelAccuracy.Text := 'Accuracy: ' + ValueToStr(Sensor.ErrorRadius, ' m');
  end;

  if not (IsNan(NewLocation.Latitude) or IsNan(NewLocation.Longitude)) then
    ShowLocation(NewLocation.Latitude, NewLocation.Longitude);
end;

procedure TForm1.ButtonTestLocationClick(Sender: TObject);
const
  WarsawLatitude = 52.23;
  WarsawLongitude = 21.01;
begin
  { Useful to test the 3D display when location sensor is not available,
    e.g. on desktops. }
  LabelStatus.Text := 'Showing test location (Warsaw)';
  LabelLatitude.Text := 'Latitude: ' + ValueToStr(WarsawLatitude, ' deg');
  LabelLongitude.Text := 'Longitude: ' + ValueToStr(WarsawLongitude, ' deg');
  CameraLooksAtLocation := false; // move the camera to see the test location
  ShowLocation(WarsawLatitude, WarsawLongitude);
end;

procedure TForm1.ShowLocation(const Latitude, Longitude: Double);
var
  LatitudeRad, LongitudeRad: Single;
  { Direction from the Earth center to the location, normalized. }
  Dir: TVector3;
  RotationAxis: TVector3;
begin
  LatitudeRad := DegToRad(Latitude);
  LongitudeRad := DegToRad(Longitude);

  { Calculate direction from the Earth center to the given location.

    This matches how the texture (data/earth.jpg, in equirectangular projection)
    is mapped on the sphere (TCastleSphere):
    - North pole is at +Y.
    - Latitude 0 (equator), longitude 0 (Greenwich meridian) is at +Z.
    - Longitude increases to the east, towards +X. }
  Dir := Vector3(
    Cos(LatitudeRad) * Sin(LongitudeRad),
    Sin(LatitudeRad),
    Cos(LatitudeRad) * Cos(LongitudeRad)
  );

  { The pin (see the design) sticks out from the north pole, along +Y.
    Rotate it to make it stick out along the Dir. }
  RotationAxis := TVector3.CrossProduct(Vector3(0, 1, 0), Dir);
  if RotationAxis.IsZero(0.001) then
    { Dir is (almost) at the north or south pole, any axis perpendicular to Y is OK. }
    RotationAxis := Vector3(1, 0, 0);
  { The angle between +Y and Dir is ArcCos(Dir.Y), since both are normalized. }
  Pin.Rotation := Vector4(RotationAxis.Normalize, ArcCos(Dir.Y));
  Pin.Exists := true;

  { Move the camera to look at the location.
    Do this only once, later user can rotate the view freely
    (by dragging, this is handled by TCastleExamineNavigation in the design). }
  if not CameraLooksAtLocation then
  begin
    CameraLooksAtLocation := true;
    MainViewport.Camera.SetWorldView(
      Dir * 3.5, // camera position
      -Dir, // camera direction
      Vector3(0, 1, 0) // camera up, will be automatically adjusted to be orthogonal to direction
    );
  end;
end;

procedure TForm1.DoUpdate(const Sender: TCastleUserInterface;
  const SecondsPassed: Single; var HandleInput: Boolean);
begin
  LabelFps.Caption := 'FPS: ' + CastleControl1.Container.Fps.ToString;
end;

end.
