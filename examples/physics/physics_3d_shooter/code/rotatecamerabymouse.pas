unit RotateCameraByMouse;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleRenderOptions;

type
  TRotateCameraByMouse = class(TCastleBehavior)
  private
    FMouseLook: Boolean;
    MinAngleFromGravityUp: Single;
    FLastUpdateMousePosition: TVector2;
    FLastMousePositionIsSet: Boolean;
    FLockRotation: T3DCoords;
    const
      DefaultMouseLookHorizontalSensitivity = Pi * 0.2 / 180;
      DefaultMouseLookVerticalSensitivity = Pi * 0.2 / 180;
      DefaultMinAngleFromGravityUp = Pi * 10 / 180;

    function Camera: TCastleCamera;
    procedure SetMouseLook(const Value: boolean);
    procedure HandleMouseLook;
    procedure ProcessMouseLookDelta(const Delta: TVector2);

    procedure RotateAroundGravityUp(const Angle: Single);
    procedure RotateAroundUp(const Angle: Single);
    procedure RotateHorizontal(const Angle: Single);
    procedure RotateVertical(AngleRad: Single);

    function InternalUsingMouseLook: Boolean;
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  published
    property MouseLook: boolean read FMouseLook write SetMouseLook default false;

    property LockRotation: T3DCoords read FLockRotation write FLockRotation default [];
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TMouseCameraRotation }

function TRotateCameraByMouse.Camera: TCastleCamera;
begin
  Result := Parent as TCastleCamera;
end;

procedure TRotateCameraByMouse.SetMouseLook(const Value: boolean);
begin
  {if FMouseLook <> Value then
  begin
    FMouseLook := Value;
    if InternalUsingMouseLook then
    begin
      Cursor := mcForceNone;
      if Container <> nil then
        Container.MouseLookPress;
    end else
      Cursor := mcDefault;
  end;}
end;

procedure TRotateCameraByMouse.HandleMouseLook;
var
  MouseChange: TVector2;
begin
  MouseChange := (FocusedContainer.MousePosition) - FLastUpdateMousePosition;

  if not MouseChange.IsPerfectlyZero then
  begin
//    if InvertVerticalMouseLook then
//      MouseChange.Y := -MouseChange.Y;
    MouseChange.X := MouseChange.X * DefaultMouseLookHorizontalSensitivity;
    MouseChange.Y := MouseChange.Y * DefaultMouseLookVerticalSensitivity;
    ProcessMouseLookDelta(MouseChange);
  end;
  FLastUpdateMousePosition := FocusedContainer.MousePosition;
end;

procedure TRotateCameraByMouse.ProcessMouseLookDelta(const Delta: TVector2);
begin
  if not (1 in FLockRotation) then
    RotateHorizontal(-Delta.X);
  if not (0 in FLockRotation) then
    RotateVertical(Delta.Y);
end;

procedure TRotateCameraByMouse.RotateAroundGravityUp(const Angle: Single);
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

procedure TRotateCameraByMouse.RotateAroundUp(const Angle: Single);
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

procedure TRotateCameraByMouse.RotateHorizontal(const Angle: Single);
begin
  if true {PreferGravityUpForRotations} then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

procedure TRotateCameraByMouse.RotateVertical(AngleRad: Single);
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

function TRotateCameraByMouse.InternalUsingMouseLook: Boolean;
begin
  Result := MouseLook; //and (niNormal in UsingInput);

  { Note: we used to have here condition "and (not CastleDesignMode)"
    as escaping from MouseLook was impossible, if you enable it in Object Inspector.
    But it is OK now: our TCastleWalkNavigationDesign makes mouse look intuitive to use. }
end;


procedure TRotateCameraByMouse.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);

  procedure MouseLookUpdate;
  begin
    if InternalUsingMouseLook and (FocusedContainer <> nil) then
      FocusedContainer.MouseLookUpdate;
  end;

begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  if buttonRight in FocusedContainer.MousePressed then
  begin
    if not FLastMousePositionIsSet then
    begin
      FLastUpdateMousePosition := FocusedContainer.MousePosition;
      FLastMousePositionIsSet := true;
    end;

    HandleMouseLook;
  end
  else
    FLastMousePositionIsSet := false;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateCameraByMouse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MinAngleFromGravityUp := DefaultMinAngleFromGravityUp;
  FMouseLook := false;
  FLastUpdateMousePosition := Vector2(0, 0);
  FLockRotation := [];
end;

initialization
  RegisterSerializableComponent(TRotateCameraByMouse, ['Physics', 'Mouse Camera Rotation']);

end.

