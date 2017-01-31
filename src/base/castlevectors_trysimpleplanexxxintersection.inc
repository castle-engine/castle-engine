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

{ Ray ---------------------------------------------------------------------- }

{$define TrySimplePlaneRayIntersection_IMPLEMENT:=
var MaybeT: TScalar;
begin
 result := not IsLineParallelToSimplePlane(RayDirection, PlaneConstCoord);
 if result then
 begin
  MaybeT:=(PlaneConstValue-RayOrigin[PlaneConstCoord]) / RayDirection[PlaneConstCoord];
  result := MaybeT >= 0;
  if result then
  begin
   {$ifdef HAS_T}T := MaybeT;{$endif}
   {$ifdef HAS_INTR}Intersection := VectorAdd(RayOrigin, VectorScale(RayDirection, MaybeT));{$endif}
  end;
 end;
end;}

function TrySimplePlaneRayIntersection(out Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
{$define HAS_INTR}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(
  out Intersection: TVector3; out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneRayIntersection(out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
{$define HAS_T}
TrySimplePlaneRayIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneRayIntersection_IMPLEMENT}

{ SegmentDir ------------------------------------------------------------ }

{$define TrySimplePlaneSegmentDirIntersection_IMPLEMENT:=
var MaybeT: TScalar;
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

function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean;
{$define HAS_INTR}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3; var T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneSegmentDirIntersection(var T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean;
{$define HAS_T}
TrySimplePlaneSegmentDirIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneSegmentDirIntersection_IMPLEMENT}

{ Segment ----------------------------------------------------------------- }

{$define TrySimplePlaneSegmentIntersection_IMPLEMENT:=
var MaybeT: TScalar;
    Segment0: TVector3 absolute Pos1;
    SegmentVector: TVector3;
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

function TrySimplePlaneSegmentIntersection(out Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean;
{$define HAS_INTR}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_INTR}

function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3; out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean;
{$define HAS_INTR}
{$define HAS_T}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_T}
{$undef HAS_INTR}

function TrySimplePlaneSegmentIntersection(out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean;
{$define HAS_T}
TrySimplePlaneSegmentIntersection_IMPLEMENT
{$undef HAS_T}

{$undef TrySimplePlaneSegmentIntersection_IMPLEMENT}

{ eof ------------------------------------------------------------ }