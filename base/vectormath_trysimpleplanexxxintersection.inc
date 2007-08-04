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

{ Ray ---------------------------------------------------------------------- }

{$define TrySimplePlaneRayIntersection_IMPLEMENT:=
var MaybeT: TYPE_SCALAR;
begin
 result := not IsLineParallelToSimplePlane(RayVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Ray0[PlaneConstCoord]) / RayVector[PlaneConstCoord];
  result := MaybeT >= 0;
  if result then
  begin
   {$ifdef HAS_T}T := MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection := VectorAdd(Ray0, VectorScale(RayVector, MaybeT));{$endif}
  end;
 end;
end;}

function TrySimplePlaneRayIntersection(out Intersection: TYPE_VECTOR3;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Ray0, RayVector: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(
  out Intersection: TYPE_VECTOR3; out T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Ray0, RayVector: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(out T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Ray0, RayVector: TYPE_VECTOR3): boolean;
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneRayIntersection_IMPLEMENT}

{ SegmentDir ------------------------------------------------------------ }

{$define TrySimplePlaneSegmentDirIntersection_IMPLEMENT:=
var MaybeT: TYPE_SCALAR;
begin
 result := not IsLineParallelToSimplePlane(SegmentVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Segment0[PlaneConstCoord]) / SegmentVector[PlaneConstCoord];
  result:=(MaybeT >= 0) and (MaybeT <= 1);
  if result then
  begin
   {$ifdef HAS_T}T := MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));{$endif}
  end;
 end;
end;}

function TrySimplePlaneSegmentDirIntersection(var Intersection: TYPE_VECTOR3;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneSegmentDirIntersection(var Intersection: TYPE_VECTOR3; var T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneSegmentDirIntersection(var T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
{$define HAS_T}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneSegmentDirIntersection_IMPLEMENT}

{ Segment ----------------------------------------------------------------- }

{$define TrySimplePlaneSegmentIntersection_IMPLEMENT:=
var MaybeT: TYPE_SCALAR;
    Segment0: TYPE_VECTOR3 absolute Pos1;
    SegmentVector: TYPE_VECTOR3;
begin
 SegmentVector := VectorSubtract(Pos2, Pos1);

 result := not IsLineParallelToSimplePlane(SegmentVector, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-Segment0[PlaneConstCoord]) / SegmentVector[PlaneConstCoord];
  result:=(MaybeT >= 0) and (MaybeT <= 1);
  if result then
  begin
   {$ifdef HAS_T}T := MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));{$endif}
  end;
 end;
end;}

function TrySimplePlaneSegmentIntersection(out Intersection: TYPE_VECTOR3;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Pos1, Pos2: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneSegmentIntersection(
  out Intersection: TYPE_VECTOR3; out T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Pos1, Pos2: TYPE_VECTOR3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneSegmentIntersection(out T: TYPE_SCALAR;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR;
  const Pos1, Pos2: TYPE_VECTOR3): boolean;
{$define HAS_T}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneSegmentIntersection_IMPLEMENT}

{ eof ------------------------------------------------------------ }