unit RotateCamera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleRenderOptions, GameInputAxis;

type
  TRotateCamera = class(TCastleBehavior)
  private
    MinAngleFromGravityUp: Single;
    FHorizontalRotationInput: TCastleInputAxis;
    FVerticalRotationInput: TCastleInputAxis;

    const
      DefaultMouseLookHorizontalSensitivity = Pi * 0.2 / 180;
      DefaultMouseLookVerticalSensitivity = Pi * 0.2 / 180;
      DefaultMinAngleFromGravityUp = Pi * 10 / 180;

    function Camera: TCastleCamera;

    procedure RotateAroundGravityUp(const Angle: Single);
    procedure RotateAroundUp(const Angle: Single);
    procedure RotateHorizontal(const Angle: Single);
    procedure RotateVertical(AngleRad: Single);
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  published
    property HorizontalRotationInput: TCastleInputAxis read FHorizontalRotationInput write FHorizontalRotationInput;
    property VerticalRotationInput: TCastleInputAxis read FVerticalRotationInput write FVerticalRotationInput;
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TMouseCameraRotation }

function TRotateCamera.Camera: TCastleCamera;
begin
  Result := Parent as TCastleCamera;
end;

procedure TRotateCamera.RotateAroundGravityUp(const Angle: Single);
var
  GravityAxis,
    OldPosition, OldDirection, OldUp,
    NewPosition, NewDirection, NewUp: TVector3;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, Camera.GravityUp) > Pi/2 then
    GravityAxis := -Camera.GravityUp
  else
    GravityAxis := Camera.GravityUp;

  NewUp        := RotatePointAroundAxisRad(Angle,        OldUp, GravityAxis);
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, GravityAxis);

  NewPosition := OldPosition; //AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Camera.SetWorldView(NewPosition, NewDirection, NewUp);
end;

procedure TRotateCamera.RotateAroundUp(const Angle: Single);
var
  OldPosition, OldDirection, OldUp, NewPosition, NewDirection: TVector3;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We know that RotatePointAroundAxisRad below doesn't change the length
    of the NewDirection (so it will remain normalized, like OldDirection)
    and it will keep NewDirection and OldUp vectors orthogonal. }
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, OldUp);

  NewPosition := OldPosition;//AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Camera.SetWorldView(NewPosition, NewDirection, OldUp);
end;

procedure TRotateCamera.RotateHorizontal(const Angle: Single);
begin
  if true {PreferGravityUpForRotations} then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

procedure TRotateCamera.RotateVertical(AngleRad: Single);
var
  Side: TVector3;
  GravityAxis, OldPosition, OldDirection, OldUp, NewDirection, NewUp: TVector3;
  AngleRadBetween: Single;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, Camera.GravityUp) > Pi/2 then
    GravityAxis := -Camera.GravityUp
  else
    GravityAxis := Camera.GravityUp;

  if true {PreferGravityUpForRotations} then
  begin
    Side := TVector3.CrossProduct(OldDirection, GravityAxis);
    if Side.IsZero then
    begin
      { if OldDirection is parallel to GravityAxis,
        then realizing PreferGravityUpForRotations is not really possible.
        Allow rotation as if PreferGravityUpForRotations = false.
        This is important to do right, to allow in CGE editor to use mouse look
        right after pressing Top (7). }
      Side := TVector3.CrossProduct(OldDirection, OldUp);
    end else
    if MinAngleFromGravityUp <> 0 then
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(OldDirection, GravityAxis);

      if AngleRadBetween - AngleRad < MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleFromGravityUp
      else
      if AngleRadBetween - AngleRad > Pi - MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleFromGravityUp);
    end;
  end else
  begin
    Side := TVector3.CrossProduct(OldDirection, OldUp);
  end;

  { Rotate NewUp around Side }
  NewUp        := RotatePointAroundAxisRad(AngleRad, OldUp       , Side);
  { Rotate NewDirection around Side }
  NewDirection := RotatePointAroundAxisRad(AngleRad, OldDirection, Side);

  Camera.SetWorldView(OldPosition, NewDirection, NewUp);
end;

procedure TRotateCamera.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  HorizontalAxisValue: Single;
  VerticalAxisValue: Single;
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  HorizontalAxisValue := HorizontalRotationInput.Value(FocusedContainer);
  WritelnLog('Hoizontal ' + FloatToStr(HorizontalAxisValue));
  VerticalAxisValue := VerticalRotationInput.Value(FocusedContainer);
  WritelnLog('Vert ' + FloatToStr(VerticalAxisValue));

  RotateHorizontal(HorizontalAxisValue);
  RotateVertical(VerticalAxisValue);

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateCamera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MinAngleFromGravityUp := DefaultMinAngleFromGravityUp;

  FHorizontalRotationInput := TCastleInputAxis.Create(Self);
  FHorizontalRotationInput.SetSubComponent(true);
  FHorizontalRotationInput.MouseLook := true;
  FHorizontalRotationInput.MouseLookAxis := mlaHorizontal;
  FHorizontalRotationInput.Multiplier := DefaultMouseLookHorizontalSensitivity;

  FVerticalRotationInput := TCastleInputAxis.Create(Self);
  FVerticalRotationInput.SetSubComponent(true);
  FVerticalRotationInput.MouseLook := true;
  FVerticalRotationInput.MouseLookAxis := mlaVertical;
  FVerticalRotationInput.Multiplier := DefaultMouseLookVerticalSensitivity;
end;

initialization
  RegisterSerializableComponent(TRotateCamera, ['Physics', 'Camera Rotation']);

end.

