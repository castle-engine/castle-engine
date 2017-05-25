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

uses CastleVectors;

type
  { Projection type, used by @link(TProjection.ProjectionType). }
  TProjectionType = (ptOrthographic, ptPerspective);

  { Projection determines how does the 3D world map onto 2D.
    To change the currently displayed projection,
    you usually want to override the @link(TCastleAbstractViewport.CalculateProjection). }
  TProjection = object
    ProjectionType: TProjectionType;

    { Perspective / orthogonal projection properties.

      When ProjectionType = ptPerspective, then PerspectiveAngles

      When ProjectionType = ptOrthographic, then OrthoDimensions
      specify

      @groupBegin }

    { If ProjectionType is ptPerspective, this property specifies
      angles of view (horizontal and vertical), in degrees. }
    PerspectiveAngles: TVector2Single;

    { If ProjectionType is ptOrthographic, this property specifies
      dimensions of the visible window. }
    OrthoDimensions: TVector4Single;

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

implementation

uses Math;

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

end.
