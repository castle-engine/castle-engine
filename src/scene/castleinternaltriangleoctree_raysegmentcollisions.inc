{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of TTriangleOctreeNode.CommonSegmentLeaf and TTriangleOctreeNode.CommonRayLeaf.
  Uses SEGMENT_COLLISION to differentiate between segment and ray case. }

{$ifdef SEGMENT_COLLISION}
var
  RayOrigin: TVector3 absolute Pos1;
  RayDirection: TVector3;
{$endif}

  function TryTriangleCollision(
    out Intersection: TVector3;
    out IntersectionDistance: Single;
    var Item: TTriangle): boolean;
  begin
    Result:=
      {$ifdef SEGMENT_COLLISION}
      Item.SegmentDirCollision(Intersection, IntersectionDistance,
        Pos1, RayDirection, Tag)
      {$else}
      Item.RayCollision(Intersection, IntersectionDistance,
        RayOrigin, RayDirection, Tag)
      {$endif}
  end;

  function FoundIntersection(
    out Intersection: TVector3;
    out IntersectionDistance: Single;
    const ItemPtr: PTriangle): boolean;
  begin
    Result := TryTriangleCollision(Intersection, IntersectionDistance,  ItemPtr^) and
       ( TriangleToIgnore <> ItemPtr ) and
       ( (not IgnoreMarginAtStart) or
         (not TVector3.Equals(Intersection, RayOrigin)) ) and
       ( (not Assigned(TrianglesToIgnoreFunc)) or
         (not TrianglesToIgnoreFunc(ParentTree, ItemPtr)) );
  end;

var
  Iterate_ItemNumber: integer;
  Iterate_Item: PTriangle;
  Iterate_Intersection: TVector3;
  Iterate_IntersectionDistance: Single;
begin
  {$ifdef SEGMENT_COLLISION}
  RayDirection := Pos2 - Pos1;
  {$endif}

  // Always define to something, to avoid warnings and to be deterministic
  // (although user code should not use it if Result = nil).
  IntersectionDistance := 0;

  if ReturnClosestIntersection then
  begin
    Result := nil;

    for Iterate_ItemNumber := 0 to ItemsCount - 1 do
    begin
      Iterate_Item := Items[Iterate_ItemNumber];
      if FoundIntersection(Iterate_Intersection, Iterate_IntersectionDistance, Iterate_Item) then
      begin
        if (Result = nil) or
           (Iterate_IntersectionDistance < IntersectionDistance) then
        begin
          Result               := Iterate_Item;
          Intersection         := Iterate_Intersection;
          IntersectionDistance := Iterate_IntersectionDistance;
        end;
      end;
    end;
  end else
  begin
    for Iterate_ItemNumber := 0 to ItemsCount - 1 do
    begin
      Iterate_Item := Items[Iterate_ItemNumber];
      if FoundIntersection(Iterate_Intersection, Iterate_IntersectionDistance, Iterate_Item) then
      begin
        Result               := Iterate_Item;
        Intersection         := Iterate_Intersection;
        IntersectionDistance := Iterate_IntersectionDistance;
        Exit;
      end;
    end;
    Exit(nil);
  end;
end;
