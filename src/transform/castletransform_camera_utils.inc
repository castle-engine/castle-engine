{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Camera-related global routines. }

{$ifdef read_interface}

const
  { Default camera direction and up vectors, used to define the meaning
    of "camera orientation" for @link(OrientationFromDirectionUp),
    @link(OrientationToDirection), @link(OrientationToUp).
    These match X3D default camera values.
    @groupBegin }
  DefaultCameraDirection: TVector3 = (X: 0; Y: 0; Z: -1);
  DefaultCameraUp       : TVector3 = (X: 0; Y: 1; Z: 0);
  { @groupEnd }

  Default2DProjectionFar = 1000.0 deprecated 'this default is not used; ProjectionFar in orthographic projection is now automatically adjusted to what you display';
  Default2DProjectionNear = - {Default2DProjectionFar}1000.0 deprecated 'this default is not used; ProjectionNear in orthographic projection is now automatically adjusted to what you display';
  Default2DCameraZ = {Default2DProjectionFar}1000.0 / 2;

{ Convert camera direction and up vectors into a rotation (X3D "orientation" vector).

  Orientation vector expresses Direction and Up as a rotation.
  First three components of the resulting vector are the Axis (normalized) and the
  4th component is the Angle (in radians). If you would rotate the standard
  direction and up (see DefaultCameraDirection, DefaultCameraUp) around Axis
  by the Angle, then you would get Direction and Up back.

  There is an overloaded version where you can pass your custom
  DefaultDirection, DefaultUp to be used instead of
  default DefaultCameraDirection, DefaultCameraUp.

  Given here Direction and Up must be orthogonal and non-zero.
  Their lengths are not relevant (that is, you don't need to normalize them
  before passing here).

  @groupBegin }
function OrientationFromDirectionUp(const Direction, Up: TVector3): TVector4; overload;
procedure OrientationFromDirectionUp(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single); overload;
function OrientationFromDirectionUp(const Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TVector4; overload;
{ @groupEnd }

{ Convert rotation (X3D orientation) to a direction vector,
  reversing the @link(OrientationFromDirectionUp). }
function OrientationToDirection(const OrientationRotation: TVector4): TVector3;

{ Convert rotation (X3D orientation) to an up vector,
  reversing the @link(OrientationFromDirectionUp). }
function OrientationToUp(const OrientationRotation: TVector4): TVector3;

function CamDirUp2Orient(const Direction, Up: TVector3): TVector4; overload;
  deprecated 'use OrientationFromDirectionUp';
procedure CamDirUp2Orient(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single); overload;
  deprecated 'use OrientationFromDirectionUp';

{ Convert camera direction and up vectors into a "rotation quaternion".
  Just like OrientationFromDirectionUp, but the result is a quaternion,
  not an axis-angle vector.
  @groupBegin }
function OrientationQuaternionFromDirectionUp(Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TQuaternion; overload;
function OrientationQuaternionFromDirectionUp(const Direction,
  Up: TVector3): TQuaternion; overload;
{ @groupEnd }

function CamDirUp2OrientQuat(const Direction, Up: TVector3): TQuaternion;
  deprecated 'OrientationQuaternionFromDirectionUp';

{ Calculate sensible camera configuration to see the whole Box.

  WantedDirection and WantedUp indicate desired look direction/up axis
  (0, 1 or 2 for X, Y or Z). WantedDirectionPositive and WantedUpPositive
  indicate if we want the positive axis. Obviously look direction and up
  cannot be parallel, so WantedDirection must be different than WantedUp.

  Returned Direction, Up, GravityUp are normalized. }
procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3);

{ Calculate suitable camera to see everything using an orthographic projection.

  Assumes that the camera direction is -Z, and camera up is +Y.
  So the horizontal axis of the world is X,
  vertical axis is Y.
  These are default values of camera set by @link(TCastleViewport.Setup2D).

  The meaning of Origin is the same as @link(TCastleOrthographic.Origin).

  Returns new correct values of
  @link(TCastleOrthographic.Width),
  @link(TCastleOrthographic.Height),
  @link(TCastleCamera.ProjectionFar) and camera position
  (set it like @code(Viewport.Camera.Position := NewPosition;)).
}
procedure CameraOrthoViewpointForWholeScene(const Box: TBox3D;
  const ViewportWidth, ViewportHeight: Single;
  const Origin: TVector2;
  out Position: TVector3;
  out AProjectionWidth, AProjectionHeight, AProjectionFar: Single);

{ TODO: move these consts somewhere more private? }

const
  { Following X3D spec of NavigationType:
    "It is recommended that the near clipping plane be set to one-half
    of the collision radius as specified in the avatarSize field." }
  RadiusToProjectionNear = 0.6;

  { Default Radius, determining also (with RadiusToProjectionNear)
    default TCastleCamera.EffectiveProjectionNear.

    Note: *Do not* base automatic Radius (and, consistently,
    automatic ProjectionNear) on Box size.

    - Major reason: It makes things go bad, as automatic ProjectionNear increases
      and clipping at near plane becomes visible,
      when you have some TCastleTransform in your world that travels forever
      (and thus increases the Viewport.ItemsBoundingBox into infinity).

      E.g. a missile that flies into infinity.
      Or a box that falls down due to gravity into infinity.
      While such things in general should not happen (you should prevent things
      from traveling to infinity), but they may happen esp. in development,
      and it's bad when one problem (flying to infinity) causes another (sudden
      clipping at near plane).

    - Minor reason: It avoids the need to calculate Viewport.ItemsBoundingBox
      at each frame, at least for the purpose of automatic projection
      for ProjectionNear.
      It was never observed in profiler,
      but in theory Viewport.ItemsBoundingBox may be costly when you have a lot of scenes.

    Note: This is smaller, so even safer, than X3D default Navigation.avatarSize[0] (0.25).
  }
  DefaultCameraRadius = 0.1;

  { Multiply radius by this to get sensible "preferred height".

    We need to make "preferred height" much larger than Radius * 2, to allow some
    space to decrease (e.g. by Input_DecreasePreferredHeight).
    Remember that CorrectPreferredHeight
    adds a limit to PreferredHeight, around Radius * 2.

    This determines minimal PreferredHeight, it should be used always like
      Max(DefaultPreferredHeight, Radius * RadiusToPreferredHeightMin)
    This way, in case of models that are small, but still follow the standard "1 unit = 1 meter",
    the PreferredHeight will not get weirdly small, it will be DefaultPreferredHeight.
    Testcase: examples/third_person_navigation/data/level/level-dungeon.gltf open with view3dscene.
  }
  RadiusToPreferredHeightMin = 4.0;

{$endif read_interface}

{$ifdef read_implementation}

function OrientationQuaternionFromDirectionUp(const Direction, Up: TVector3): TQuaternion;
begin
  Result := OrientationQuaternionFromDirectionUp(Direction, Up,
    DefaultCameraDirection, DefaultCameraUp);
end;

function OrientationQuaternionFromDirectionUp(Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TQuaternion;

{ This was initially based on Stephen Chenney's ANSI C code orient.c,
  available still from here: http://vrmlworks.crispen.org/tools.html
  I rewrote it a couple of times, possibly removing and possibly adding
  some bugs :)

  Idea: we want to convert Direction and Up into VRML orientation,
  which is a rotation from DefaultDirection/DefaultUp into Direction/Up.

  1) Take vector orthogonal to standard DefaultDirection and Direction.
     Rotate around it, to match DefaultDirection with Direction.

  2) Now rotate around Direction such that standard up (already rotated
     by 1st transform) matches with Up. We know it's possible,
     since Direction and Up are orthogonal and normalized,
     just like standard DefaultDirection/DefaultUp.

  Combine these two rotations and you have the result.

  How to combine two rotations, such that in the end you get nice
  single rotation? That's where quaternions rule.
}

  function QuatFromAxisAngleCos(const Axis: TVector3;
    const AngleRadCos: Single): TQuaternion;
  begin
    Result := QuatFromAxisAngle(Axis, ArcCos(Clamped(AngleRadCos, -1.0, 1.0)));
  end;

var
  Rot1Axis, Rot2Axis, DefaultUpAfterRot1: TVector3;
  Rot1Quat, Rot2Quat: TQuaternion;
  Rot1CosAngle, Rot2CosAngle: Single;
begin
  Direction := Direction.Normalize;
  Up := Up.Normalize;

  { calculate Rot1Quat }
  Rot1Axis := TVector3.CrossProduct(DefaultDirection, Direction);
  { Rot1Axis may be zero if DefaultDirection and Direction are parallel.
    When they point in the same direction, then it doesn't matter
    (rotation will be by 0 angle anyway), but when they are in opposite
    direction we want to do some rotation, so we need some non-zero
    sensible Rot1Axis. }
  if Rot1Axis.IsZero then
    Rot1Axis := DefaultUp
  else
    { Normalize *after* checking ZeroVector, otherwise normalization
      could change some almost-zero vector into a (practically random)
      vector of length 1. }
    Rot1Axis := Rot1Axis.Normalize;
  Rot1CosAngle := TVector3.DotProduct(DefaultDirection, Direction);
  Rot1Quat := QuatFromAxisAngleCos(Rot1Axis, Rot1CosAngle);

  { calculate Rot2Quat }
  DefaultUpAfterRot1 := Rot1Quat.Rotate(DefaultUp);
  { We know Rot2Axis should be either Direction or -Direction. But how do we know
    which one? (To make the rotation around it in correct direction.)
    Calculating Rot2Axis below is a solution. }
  Rot2Axis := TVector3.CrossProduct(DefaultUpAfterRot1, Up);

  (*We could now do Rot2Axis := Rot2Axis.Normalize,
    after making sure it's not zero. Like

    { we need larger epsilon for ZeroVector below, in case
      DefaultUpAfterRot1 is = -Up.
      testcameras.pas contains testcases that require it. }
    if Rot2Axis.IsZero(0.001) then
      Rot2Axis := Direction else
      { Normalize *after* checking ZeroVector, otherwise normalization
        could change some almost-zero vector into a (practically random)
        vector of length 1. }
      Rot2Axis := Rot2Axis.Normalize;

    And later do

      { epsilon for TVector3.Equals 0.001 is too small }
      Assert( TVector3.Equals(Rot2Axis,  Direction, 0.01) or
              TVector3.Equals(Rot2Axis, -Direction, 0.01),
        Format('OrientationQuaternionFromDirectionUp failed for Direction, Up: (%s), (%s)',
          [ Direction.ToRawString, Up.ToRawString ]));

    However, as can be seen in above comments, this requires some careful
    adjustments of epsilons, so it's somewhat numerically unstable.
    It's better to just use now the knowledge that Rot2Axis
    is either Direction or -Direction, and choose one of them. *)
  if AreParallelVectorsSameDirection(Rot2Axis, Direction) then
    Rot2Axis :=  Direction else
    Rot2Axis := -Direction;

  Rot2CosAngle := TVector3.DotProduct(DefaultUpAfterRot1, Up);
  Rot2Quat := QuatFromAxisAngleCos(Rot2Axis, Rot2CosAngle);

  { calculate Result = combine Rot1 and Rot2 (yes, the order
    for QuatMultiply is reversed) }
  Result := Rot2Quat * Rot1Quat;
end;

procedure OrientationFromDirectionUp(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single);
begin
  { Call OrientationQuaternionFromDirectionUp,
    and extract the axis and angle from the quaternion. }
  OrientationQuaternionFromDirectionUp(Direction, Up).ToAxisAngle(Axis, Angle);
end;

function OrientationFromDirectionUp(const Direction, Up: TVector3): TVector4;
var
  Axis: TVector3;
  Angle: Single;
begin
  OrientationFromDirectionUp(Direction, Up, Axis, Angle);
  Result := Vector4(Axis, Angle);
end;

function OrientationFromDirectionUp(const Direction, Up,
  DefaultDirection, DefaultUp: TVector3): TVector4;
var
  Axis: TVector3;
  Angle: Single;
begin
  OrientationQuaternionFromDirectionUp(Direction, Up, DefaultDirection, DefaultUp).
    ToAxisAngle(Axis, Angle);
  Result := Vector4(Axis, Angle);
end;

function OrientationToDirection(const OrientationRotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(OrientationRotation, DefaultCameraDirection);
end;

function OrientationToUp(const OrientationRotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(OrientationRotation, DefaultCameraUp);
end;

function CamDirUp2Orient(const Direction, Up: TVector3): TVector4;
begin
  Result := OrientationFromDirectionUp(Direction, Up);
end;

procedure CamDirUp2Orient(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single);
begin
  OrientationFromDirectionUp(Direction, Up, Axis, Angle);
end;

function CamDirUp2OrientQuat(const Direction, Up: TVector3): TQuaternion;
begin
  Result := OrientationQuaternionFromDirectionUp(Direction, Up);
end;

procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3);
var
  Offset: Single;
begin
  Direction := TVector3.One[WantedDirection];
  if not WantedDirectionPositive then Direction := -Direction;

  Up := TVector3.One[WantedUp];
  if not WantedUpPositive then Up := -Up;

  if Box.IsEmpty then
  begin
    { If box is empty, choose the default X3D viewpoint.
      This is least surprising, see https://github.com/castle-engine/view3dscene/issues/3 }
    Position  := CastleVectors.Vector3(0, 0, 10); // DefaultX3DCameraPosition[cvVrml2_X3d];
  end else
  begin
    Position := Box.Center;
    Offset := 2 * Box.AverageSize;

    if WantedDirectionPositive then
      Position.Data[WantedDirection] := Box.Data[0].Data[WantedDirection] - Offset
    else
      Position.Data[WantedDirection] := Box.Data[1].Data[WantedDirection] + Offset;
  end;

  { GravityUp is just always equal Up here. }
  GravityUp := Up;
end;

procedure CameraOrthoViewpointForWholeScene(const Box: TBox3D;
  const ViewportWidth, ViewportHeight: Single;
  const Origin: TVector2;
  out Position: TVector3;
  out AProjectionWidth, AProjectionHeight, AProjectionFar: Single);

  { Calculate Position.XY and AProjectionWidth, AProjectionHeight. }
  function PositionXY: TVector2;
  var
    Rect: TFloatRectangle;
    EffectiveProjectionWidth, EffectiveProjectionHeight: Single;
  begin
    if Box.IsEmpty then
    begin
      Result := Vector2(0, 0);
      AProjectionWidth := 0;
      AProjectionHeight := 0;
    end else
    begin
      Rect := Box.RectangleXY;

      if ViewportWidth / ViewportHeight >
         Rect.Width / Rect.Height then
      begin
        AProjectionWidth := 0;
        AProjectionHeight := Rect.Height;
        { Calculate EffectiveProjectionXxx
          the same way that TCastleOrthographic.EffectiveWidth/Height would be calculated. }
        EffectiveProjectionWidth := Rect.Height * ViewportWidth / ViewportHeight;
        EffectiveProjectionHeight := AProjectionHeight;
      end else
      begin
        AProjectionWidth := Rect.Width;
        AProjectionHeight := 0;
        { Calculate EffectiveProjectionXxx
          the same way that TCastleOrthographic.EffectiveWidth/Height would be calculated. }
        EffectiveProjectionWidth := AProjectionWidth;
        EffectiveProjectionHeight := Rect.Width * ViewportHeight / ViewportWidth;
      end;

      // calculate PositionXY
      Result := Rect.Center +
        (Origin - Vector2(0.5, 0.5)) *
        Vector2(EffectiveProjectionWidth, EffectiveProjectionHeight);
    end;
  end;

  { Calculate Position.Z and AProjectionFar. }
  function PositionZ: Single;
  const
    Default2DProjectionFar = 1000.0;
  var
    MinZ, MaxZ: Single;
  begin
    if Box.IsEmpty then
    begin
      Result := Default2DCameraZ;
      AProjectionFar := Default2DProjectionFar;
    end else
    begin
      MinZ := Box.Min.Z;
      MaxZ := Box.Max.Z;
      if (MinZ > - Default2DProjectionFar / 2) and
         (MaxZ < Default2DProjectionFar / 2) then
      begin
        // prefer to use Default2DProjectionFar and Default2DCameraZ, if the scene fits inside
        Result := Default2DCameraZ;
        AProjectionFar := Default2DProjectionFar;
      end else
      begin
        Result := MaxZ + 1;
        AProjectionFar := MaxZ - MinZ;
      end;
    end;
  end;

begin
  Position := Vector3(PositionXY, PositionZ);
end;

{$endif read_implementation}
