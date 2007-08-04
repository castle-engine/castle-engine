{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ w zaleznosci od tego czy zdefiniowales HAS_PRECALC_PLANE
  to makro wygeneruje inna implementacje funkcji IsTriangleSphereCollision }
var intersection: TYPE_VECTOR3;
    SphereRadiusSqr: TYPE_SCALAR;
    i: integer;
    {$ifndef HAS_PRECALC_PLANE} TriPlane: TYPE_VECTOR4; {$endif}
    { pamietaj ze TriDir dla ifndef HAS_PRECALC_PLANE jest dobre dopiero
      po obliczeniu TriPlane }
    TriDir: TYPE_VECTOR3 absolute TriPlane;
begin
 SphereRadiusSqr := Sqr(SphereRadius);
 if (PointsDistanceSqr(Tri[0], SphereCenter) <= SphereRadiusSqr) or
    (PointsDistanceSqr(Tri[1], SphereCenter) <= SphereRadiusSqr) or
    (PointsDistanceSqr(Tri[2], SphereCenter) <= SphereRadiusSqr) then exit(true);

 {$ifndef HAS_PRECALC_PLANE} TriPlane := TrianglePlane(Tri);{$endif}
 intersection := PointOnPlaneClosestToPoint(TriPlane, SphereCenter);
 if (PointsDistanceSqr(SphereCenter, intersection) <= SphereRadiusSqr) and
    IsPointOnTrianglePlaneWithinTriangle(intersection, Tri, TriDir) then
  exit(true);

 (* pozostaje nam sprawdzic czy moze punkt SphereCenter jest blisko jednego
    z bokow trojkata (ale nie musimy juz sprawdzac czy jest blisko jakiegos rogu;
    dzieki temu wystarczy ze obliczymy na kazdym boku trojkata punkt najblizszy
    SphereCenter i obliczymy odleglosc tego punktu od SphereCenter (powinna byc
    mniejsza niz SphereRadius)) *)
 for i := 0 to 2 do
 begin
  intersection := PointOnLineClosestToPoint(Tri[i],
    VectorSubtract(Tri[(i+1)mod 3], Tri[i]), SphereCenter);
  if IsPointOnSegmentLineWithinSegment(intersection, Tri[i], Tri[(i+1)mod 3]) and
     (PointsDistanceSqr(SphereCenter, intersection) <= SphereRadiusSqr) then
   exit(true);
 end;

 result := false;
end;
