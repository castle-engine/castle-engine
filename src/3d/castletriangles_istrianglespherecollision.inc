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

var
  Intersection: TVector3;
  SphereRadiusSqr: TScalar;
  I, NextI: integer;
  {$ifndef HAS_PRECALC_PLANE} TriPlane: TVector4; {$endif}
  { In case HAS_PRECALC_PLANE is not defined, TriDir is valid only once
    TriPlane is calculated. }
  TriDir: TVector3 absolute TriPlane;
begin
  SphereRadiusSqr := Sqr(SphereRadius);

  (* Is SphereCenter within the radius of one of triangle corners.
     Note that this case is necessary, it is not fully catched by
     "SphereCenter close to one of the triangles' edges" lower. *)
  if (PointsDistanceSqr(Tri[0], SphereCenter) <= SphereRadiusSqr) or
     (PointsDistanceSqr(Tri[1], SphereCenter) <= SphereRadiusSqr) or
     (PointsDistanceSqr(Tri[2], SphereCenter) <= SphereRadiusSqr) then
    Exit(true);

  {$ifndef HAS_PRECALC_PLANE} TriPlane := TrianglePlane(Tri);{$endif}
  (* Is SphereCenter within the triangle. *)
  Intersection := PointOnPlaneClosestToPoint(TriPlane, SphereCenter);
  if (PointsDistanceSqr(SphereCenter, Intersection) <= SphereRadiusSqr) and
     IsPointOnTrianglePlaneWithinTriangle(Intersection, Tri, TriDir) then
    Exit(true);

  (* Is SphereCenter close to one of the triangles' edges. *)
  for I := 0 to 2 do
  begin
    NextI := (I + 1) mod 3;
    Intersection := PointOnLineClosestToPoint(Tri[I],
      VectorSubtract(Tri[NextI], Tri[I]), SphereCenter);
    if IsPointOnSegmentLineWithinSegment(Intersection, Tri[I], Tri[NextI]) and
       (PointsDistanceSqr(SphereCenter, Intersection) <= SphereRadiusSqr) then
      Exit(true);
  end;

  Result := false;
end;
