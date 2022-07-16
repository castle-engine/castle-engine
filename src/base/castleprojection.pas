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

{ Projection parameters (TProjection). }
unit CastleProjection;

{$I castleconf.inc}

interface

uses CastleVectors, CastleRectangles;

type
  { Projection type, used by @link(TProjection.ProjectionType). }
  TProjectionTypeCore = (ptOrthographic, ptPerspective, ptFrustum);
  TProjectionType = ptOrthographic .. ptPerspective;

  { Projection determines how does the 3D world map onto 2D.
    To change the currently displayed projection,
    you usually want to override the @link(TCastleCamera.InternalProjection). }
  TProjection = record
    ProjectionType: TProjectionTypeCore;

    { If ProjectionType is ptPerspective, this property specifies
      angles of view (horizontal and vertical), in radians.

      Note that when overriding the @link(TCastleViewport.CalculateProjection),
      you are expected to provide both angles calculated, even though some routines
      for now will only use the vertical angle (and automatically adjust the other
      to the aspect ratio).
      Use the AdjustViewAngleRadToAspectRatio to calculate the angles as necessary.
    }
    PerspectiveAnglesRad: TVector2;

    { If ProjectionType is ptOrthographic or ptFrustum, this property specifies
      dimensions of the visible window. }
    Dimensions: TFloatRectangle;

    { Near clipping distance.
      Everything closer to this distance is clipped (invisible). }
    ProjectionNear: Single;

    { Far clipping distance.
      Everything further than this distance is clipped (invisible).
      Note that it have a special value ZFarInfinity, which means that
      no far clipping plane is used. E.g. shadow volumes require this. }
    ProjectionFar: Single;

    { Projection matrix, adjusted to given viewport aspect ratio (width/height). }
    function Matrix(const AspectRatio: Single): TMatrix4;

    { Detect whether any sensible projection values are initialized. }
    function Initialized: Boolean; deprecated 'this should not be necessary anymore';
  end;

{ Calculate second viewing angle for perspective projection.
  Given one viewing angle of the camera (FirstViewAngleDeg) and
  aspect ratio of your window sizes (SecondToFirstRatio),
  calculate second viewing angle of the camera.

  The intention is that when projecting camera view (with given view angles)
  on a screen with given aspect ratio), the image will not be distorted
  (squeezed horizontally or vertically).

  For the "Deg" version both angles (given and returned) are in degress,
  for the "Rad" version both angles and in radians.

  @groupBegin }
function AdjustViewAngleDegToAspectRatio(const FirstViewAngleDeg,
  SecondToFirstRatio: Single): Single; deprecated 'use radians for everything throughout CGE';
function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad,
  SecondToFirstRatio: Single): Single;
{ @groupEnd }

const
  { Special value that you can pass to various perspective-projection functions
    with the intention to set far plane at infinity.
    These functions include
    like @link(FrustumProjectionMatrix), @link(PerspectiveProjectionMatrixDeg),
    @link(PerspectiveProjectionMatrixRad), @link(PerspectiveProjection).
    You can pass ZFarInfinity as their @code(ZFar) parameter value.

    @italic(Implementation note:)
    It would be "cooler" to define this constant as Math.Infinity,
    but operating on Math.Infinity requires unnecessary turning
    off of some compiler checks. The point was only to have some special ZFar
    value, so 0 is as good as Infinity. }
  ZFarInfinity = 0.0;

{ Functions to create 4x4 matrices used in 3D graphics for projection.
  That are equivalent to OpenGL glOrtho, glFrustum, gluPerspective.

  The frustum and pespective generation
  (routines FrustumProjectionMatrix, PerspectiveProjectionMatrixDeg, PerspectiveProjectionMatrixRad)
  accept also a special value ZFarInfinity as the ZFar parameter.
  Then you get perspective projection matrix withour far clipping plane,
  which is very useful for the z-fail shadow volumes technique.
  @groupBegin }
function OrthoProjectionMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4; overload;
function OrthoProjectionMatrix(const left, right, bottom, top, ZNear, ZFar: Single): TMatrix4; overload; deprecated 'use the overloaded version that takes Dimensions as TFloatRectangle';

function Ortho2DProjectionMatrix(const Dimensions: TFloatRectangle): TMatrix4; overload; deprecated 'just use OrthoProjectionMatrix, the 2D optimization is not really worth the maintenance';
function Ortho2DProjectionMatrix(const left, right, bottom, top: Single): TMatrix4; overload; deprecated 'just use OrthoProjectionMatrix, the 2D optimization is not really worth the maintenance';

function FrustumProjectionMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4; overload;
function FrustumProjectionMatrix(const left, right, bottom, top, ZNear, ZFar: Single): TMatrix4; overload; deprecated 'use the overloaded version that takes Dimensions as TFloatRectangle';

function PerspectiveProjectionMatrixDeg(const fovyDeg, aspect, ZNear, ZFar: Single): TMatrix4;
function PerspectiveProjectionMatrixRad(const fovyRad, aspect, ZNear, ZFar: Single): TMatrix4;
{ @groupEnd }

function ProjectionTypeToStr(const ProjectionType: TProjectionTypeCore): String;

implementation

uses Math,
  CastleUtils, CastleLog;

{ TProjection ---------------------------------------------------------------- }

function TProjection.Matrix(const AspectRatio: Single): TMatrix4;
begin
  case ProjectionType of
    ptPerspective:
      Result := PerspectiveProjectionMatrixRad(
        PerspectiveAnglesRad.Y,
        AspectRatio,
        ProjectionNear,
        ProjectionFar);
    ptOrthographic:
      Result := OrthoProjectionMatrix(
        Dimensions,
        ProjectionNear,
        ProjectionFar);
    ptFrustum:
      Result := FrustumProjectionMatrix(
        Dimensions,
        ProjectionNear,
        ProjectionFar);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create(2018081901);
    {$endif}
  end;
end;

function TProjection.Initialized: Boolean;
begin
  Result :=
    { ProjectionNear may remain = 0 in case of orthographic projection,
      see TCastleViewport.CalculateProjection .
      Testcase: glTF-Sample-Models/2.0/Cameras/glTF/Cameras.gltf , switch to ortho viewpoint. }
    // (ProjectionNear <> 0) and
    { ProjectionFar may remain = 0 = ZFarInfinity. }
    // (ProjectionFar <> 0) and
    (Dimensions.Width <> 0) and
    (Dimensions.Height <> 0);
end;

{ global routines ------------------------------------------------------------ }

function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad, SecondToFirstRatio: Single): Single;
begin
  { Ratio on window sizes is the ratio of angle tangeses.
    For two angles (a, b), and window sizes (x, y) we have
      Tan(a/2) = (x/2)/d
      Tan(b/2) = (y/2)/d
    where D is the distance to projection plane. So
      Tan(a/2) / Tan(b/2) = x/y }

  Result := ArcTan( Tan(FirstViewAngleRad/2) * SecondToFirstRatio) * 2;
end;

function AdjustViewAngleDegToAspectRatio(const FirstViewAngleDeg, SecondToFirstRatio: Single): Single;
begin
  Result := RadToDeg( AdjustViewAngleRadToAspectRatio( DegToRad(FirstViewAngleDeg),
    SecondToFirstRatio));
end;

function OrthoProjectionMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left := Left;
  Dimensions.Width := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  Result := OrthoProjectionMatrix(Dimensions, ZNear, ZFar);
end;

function OrthoProjectionMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
var
  Depth: Single;
begin
  Depth := ZFar - ZNear;

  Result := TMatrix4.Zero;
  Result.Data[0, 0] := 2 / Dimensions.Width;
  Result.Data[1, 1] := 2 / Dimensions.Height;
  Result.Data[2, 2] := - 2 / Depth; { negate, because our Z-y are negative when going "deeper inside the screen" }
  Result.Data[3, 0] := - (Dimensions.Right + Dimensions.Left  ) / Dimensions.Width;
  Result.Data[3, 1] := - (Dimensions.Top   + Dimensions.Bottom) / Dimensions.Height;
  Result.Data[3, 2] := - (ZFar + ZNear) / Depth;
  Result.Data[3, 3] := 1;
end;

function Ortho2DProjectionMatrix(const Left, Right, Bottom, Top: Single): TMatrix4;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left   := Left;
  Dimensions.Width  := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  {$warnings off}
  // consciously using deprecated function in another deprecated function
  Result := Ortho2DProjectionMatrix(Dimensions);
  {$warnings on}
end;

function Ortho2DProjectionMatrix(const Dimensions: TFloatRectangle): TMatrix4;
begin
  { simple version: Result := OrthoProjectionMatrix(Dimensions, -1, 1); }
  { optimized version below: }

  Result := TMatrix4.Zero;
  Result.Data[0, 0] := 2 / Dimensions.Width;
  Result.Data[1, 1] := 2 / Dimensions.Height;
  Result.Data[2, 2] := -1;
  Result.Data[3, 0] := - (Dimensions.Right + Dimensions.Left  ) / Dimensions.Width;
  Result.Data[3, 1] := - (Dimensions.Top   + Dimensions.Bottom) / Dimensions.Height;
  Result.Data[3, 3] := 1;
end;

function FrustumProjectionMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left := Left;
  Dimensions.Width := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  Result := FrustumProjectionMatrix(Dimensions, ZNear, ZFar);
end;

function FrustumProjectionMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;

{ This is based on "OpenGL Programming Guide",
  Appendix G "... and Transformation Matrices".
  ZFarInfinity version based on various sources, pretty much every
  article about shadow volumes mentions z-fail and this trick. }

var
  Depth, ZNear2: Single;
begin
  ZNear2 := ZNear * 2;

  Result := TMatrix4.Zero;
  Result.Data[0, 0] := ZNear2                               / Dimensions.Width;
  Result.Data[2, 0] := (Dimensions.Right + Dimensions.Left) / Dimensions.Width;
  Result.Data[1, 1] := ZNear2                               / Dimensions.Height;
  Result.Data[2, 1] := (Dimensions.Top + Dimensions.Bottom) / Dimensions.Height;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result.Data[2, 2] := - (ZFar + ZNear) / Depth;
    Result.Data[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result.Data[2, 2] := -1;
    Result.Data[3, 2] := -ZNear2;
  end;
  Result.Data[2, 3] := -1;
end;

function PerspectiveProjectionMatrixDeg(const FovyDeg, Aspect, ZNear, ZFar: Single): TMatrix4;
begin
  Result := PerspectiveProjectionMatrixRad(DegToRad(FovyDeg), Aspect, ZNear, ZFar);
end;

function PerspectiveProjectionMatrixRad(const FovyRad, Aspect, ZNear, ZFar: Single): TMatrix4;
{ Based on various sources, e.g. sample implementation of
  glu by SGI in Mesa3d sources. }
var
  Depth, ZNear2, Cotangent: Single;
  // R: TFloatRectangle;
  // W, H: Single;
begin
  { You can express PerspectiveProjectionMatrixRad using FrustumProjectionMatrix.
    Adapted from https://stackoverflow.com/a/12943456 }
  // H := Tan(FovyRad / 2) * ZNear;
  // W := H * Aspect;
  // R := FloatRectangle(-W, -H, 2 * W, 2 * H);
  // Result := FrustumProjectionMatrix(R, ZNear, ZFar);
  // WritelnLog('Converted PerspectiveProjectionMatrixRad to a FrustumProjectionMatrix with rectangle %s',
  //   [R.ToString]);

  ZNear2 := ZNear * 2;
  Cotangent := CastleCoTan(FovyRad / 2);

  Result := TMatrix4.Zero;
  Result.Data[0, 0] := Cotangent / Aspect;
  Result.Data[1, 1] := Cotangent;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result.Data[2, 2] := - (ZFar + ZNear) / Depth;
    Result.Data[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result.Data[2, 2] := -1;
    Result.Data[3, 2] := -ZNear2;
  end;

  Result.Data[2, 3] := -1;
end;

function ProjectionTypeToStr(const ProjectionType: TProjectionTypeCore): String;
const
  Names: array[TProjectionTypeCore] of String = ('Orthographic', 'Perspective', 'Frustum');
begin
  Result := Names[ProjectionType];
end;

end.
