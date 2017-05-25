{
  Copyright 2003-2017 Michalis Kamburelis.

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

interface

uses CastleVectors, CastleRectangles;

type
  { Projection type, used by @link(TProjection.ProjectionType). }
  TProjectionType = (ptOrthographic, ptPerspective, ptFrustum);

  { Projection determines how does the 3D world map onto 2D.
    To change the currently displayed projection,
    you usually want to override the @link(TCastleAbstractViewport.CalculateProjection). }
  TProjection = object
    ProjectionType: TProjectionType;

    { If ProjectionType is ptPerspective, this property specifies
      angles of view (horizontal and vertical), in degrees. }
    PerspectiveAngles: TVector2Single;

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

    { Far clipping distance to be used in cases when it cannot be infinite.
      Unlike ProjectionFar, this property cannot have a magical value ZFarInfinity.
      It should be calculated just like ProjectionFar,
      except it's never changed to be ZFarInfinity. }
    ProjectionFarFinite: Single;
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
  SecondToFirstRatio: Single): Single;
function AdjustViewAngleRadToAspectRatio(const FirstViewAngleRad,
  SecondToFirstRatio: Single): Single;
{ @groupEnd }

const
  { Special value that you can pass to various perspective-projection functions
    with the intention to set far plane at infinity.
    These functions include
    like @link(FrustumProjMatrix), @link(PerspectiveProjMatrixDeg),
    @link(PerspectiveProjMatrixRad), @link(PerspectiveProjection).
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
  (routines FrustumProjMatrix, PerspectiveProjMatrixDeg, PerspectiveProjMatrixRad)
  accept also a special value ZFarInfinity as the ZFar parameter.
  Then you get perspective projection matrix withour far clipping plane,
  which is very useful for the z-fail shadow volumes technique.
  @groupBegin }
function OrthoProjMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4Single;
function OrthoProjMatrix(const left, right, bottom, top, ZNear, ZFar: Single): TMatrix4Single; deprecated 'use the overloaded version that takes Dimensions as TFloatRectangle';

function Ortho2dProjMatrix(const Dimensions: TFloatRectangle): TMatrix4Single; deprecated 'just use OrthoProjMatrix, the 2D optimization is not really worth the maintenance';
function Ortho2dProjMatrix(const left, right, bottom, top: Single): TMatrix4Single; deprecated 'just use OrthoProjMatrix, the 2D optimization is not really worth the maintenance';

function FrustumProjMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4Single;
function FrustumProjMatrix(const left, right, bottom, top, ZNear, ZFar: Single): TMatrix4Single; deprecated 'use the overloaded version that takes Dimensions as TFloatRectangle';

function PerspectiveProjMatrixDeg(const fovyDeg, aspect, ZNear, ZFar: Single): TMatrix4Single;
function PerspectiveProjMatrixRad(const fovyRad, aspect, ZNear, ZFar: Single): TMatrix4Single;
{ @groupEnd }

implementation

uses Math,
  CastleUtils;

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

function OrthoProjMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4Single;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left := Left;
  Dimensions.Width := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  Result := OrthoProjMatrix(Dimensions, ZNear, ZFar);
end;

function OrthoProjMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4Single;
var
  Depth: Single;
begin
  Depth := ZFar - ZNear;

  Result := ZeroMatrix4Single;
  Result[0, 0] := 2 / Dimensions.Width;
  Result[1, 1] := 2 / Dimensions.Height;
  Result[2, 2] := - 2 / Depth; { negate, because our Z-y are negative when going "deeper inside the screen" }
  Result[3, 0] := - (Dimensions.Right + Dimensions.Left  ) / Dimensions.Width;
  Result[3, 1] := - (Dimensions.Top   + Dimensions.Bottom) / Dimensions.Height;
  Result[3, 2] := - (ZFar + ZNear) / Depth;
  Result[3, 3] := 1;
end;

function Ortho2dProjMatrix(const Left, Right, Bottom, Top: Single): TMatrix4Single;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left   := Left;
  Dimensions.Width  := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  {$warnings off}
  // consciously using deprecated function in another deprecated function
  Result := Ortho2dProjMatrix(Dimensions);
  {$warnings on}
end;

function Ortho2dProjMatrix(const Dimensions: TFloatRectangle): TMatrix4Single;
begin
  { simple version: Result := OrthoProjMatrix(Dimensions, -1, 1); }
  { optimized version below: }

  Result := ZeroMatrix4Single;
  Result[0, 0] := 2 / Dimensions.Width;
  Result[1, 1] := 2 / Dimensions.Height;
  Result[2, 2] := -1;
  Result[3, 0] := - (Dimensions.Right + Dimensions.Left  ) / Dimensions.Width;
  Result[3, 1] := - (Dimensions.Top   + Dimensions.Bottom) / Dimensions.Height;
  Result[3, 3] := 1;
end;

function FrustumProjMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4Single;
var
  Dimensions: TFloatRectangle;
begin
  Dimensions.Left := Left;
  Dimensions.Width := Right - Left;
  Dimensions.Bottom := Bottom;
  Dimensions.Height := Top - Bottom;
  Result := FrustumProjMatrix(Dimensions, ZNear, ZFar);
end;

function FrustumProjMatrix(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4Single;

{ This is based on "OpenGL Programming Guide",
  Appendix G "... and Transformation Matrices".
  ZFarInfinity version based on various sources, pretty much every
  article about shadow volumes mentions z-fail and this trick. }

var
  Depth, ZNear2: Single;
begin
  ZNear2 := ZNear * 2;

  Result := ZeroMatrix4Single;
  Result[0, 0] := ZNear2                               / Dimensions.Width;
  Result[2, 0] := (Dimensions.Right + Dimensions.Left) / Dimensions.Width;
  Result[1, 1] := ZNear2                               / Dimensions.Height;
  Result[2, 1] := (Dimensions.Top + Dimensions.Bottom) / Dimensions.Height;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result[2, 2] := - (ZFar + ZNear) / Depth;
    Result[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result[2, 2] := -1;
    Result[3, 2] := -ZNear2;
  end;
  Result[2, 3] := -1;
end;

function PerspectiveProjMatrixDeg(const FovyDeg, Aspect, ZNear, ZFar: Single): TMatrix4Single;
begin
  Result := PerspectiveProjMatrixRad(DegToRad(FovyDeg), Aspect, ZNear, ZFar);
end;

function PerspectiveProjMatrixRad(const FovyRad, Aspect, ZNear, ZFar: Single): TMatrix4Single;
{ Based on various sources, e.g. sample implementation of
  glu by SGI in Mesa3d sources. }
var
  Depth, ZNear2, Cotangent: Single;
begin
  ZNear2 := ZNear * 2;
  Cotangent := CastleCoTan(FovyRad / 2);

  Result := ZeroMatrix4Single;
  Result[0, 0] := Cotangent / Aspect;
  Result[1, 1] := Cotangent;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result[2, 2] := - (ZFar + ZNear) / Depth;
    Result[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result[2, 2] := -1;
    Result[3, 2] := -ZNear2;
  end;

  Result[2, 3] := -1;
end;

end.
