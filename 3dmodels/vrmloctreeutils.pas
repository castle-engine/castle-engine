{
  Copyright 2003-2008 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ Helper types and classes for octrees related to VRML models. }
unit VRMLOctreeUtils;

{$I vrmloctreeconf.inc}

interface

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3d, Math,
  KambiOctree;

{$define read_interface}

{ TOctreeItem  ------------------------------------------------------------ }

type
  { }
  TOctreeItemMailboxState = (msEmpty, msRay, msSegmentDir);
  TCollisionCount = Int64;

  { This is a single item of a triangle octree.
    In other words, this is really just a triangle with a lot
    of associated information.

    @bold(Never modify fields of this record directly) ---
    this record should be created only by CreateOctreeItem and
    modified by other routines in this unit. }
  TOctreeItem = record
    Triangle: TTriangle3Single;

    { Calculated TriangleArea(Triangle) }
    TriangleArea: Single;

    State: TVRMLGraphTraverseState;
    GeometryNode: TVRMLGeometryNode;
    MatNum: integer;

    { If this triangle is part of a face created by coordIndex field
      (like all faces in IndexedFaceSet) then these fields indicate where
      in this coordIndex this face is located.

      You should look into GeometryNode, get it's coordIndex field,
      and the relevant indexes are between FaceCoordIndexBegin
      and FaceCoordIndexEnd - 1. Index FaceCoordIndexEnd is either
      non-existing (coordIndex list ended) or is the "-1" (faces separator
      on coordIndex fields).

      If this triangle doesn't come from any coordIndex (e.g. because GeometryNode
      is a TNodeSphere) then both FaceCoordIndex* are -1. }
    FaceCoordIndexBegin, FaceCoordIndexEnd: Integer;

    {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { MailboxSavedTag is a tag of item for which we have saved an
      intersection result. Intersection result is in
      MailboxIsIntersection, MailboxIntersection, MailboxIntersectionDistance.

      To make things correct, we obviously assume that every segment
      and ray have different tags. Also, tag -1 is reserved.
      In practice, we simply initialize MailboxSavedTag to -1
      (in CreateOctreeItem), and each new segment/ray get consecutive tags
      starting from 0.

      @groupBegin }
    MailboxSavedTag: Int64;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    case Integer of
      0: ({ This is a calculated TriangleNormPlane(Triangle),
            that is a 3D plane containing our Triangle, with normalized
            direction vector. }
          TriangleNormalPlane: TVector4Single;);
      1: (TriangleNormal: TVector3Single;);
  end;
  POctreeItem = ^TOctreeItem;

  TDynArrayItem_1 = TOctreeItem;
  PDynArrayItem_1 = POctreeItem;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynOctreeItemsArray = TDynArray_1;

{ Create new TOctreeItem. Given Triangle must satisfy IsValidTriangle. }
function CreateOctreeItem(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer): TOctreeItem;

{ Check collisions between TOctreeItem and ray/segment.

  Always use these routines to check for collisions,
  to use mailboxes if possible. Mailboxes are used only if this was
  compiled with OCTREE_ITEM_USE_MAILBOX defined.

  Increments DirectCollisionTestsCounter if actual test was done
  (that is, if we couldn't use mailbox to get the result quickier).

  @groupBegin }
function TryOctreeItemSegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Odc0, OdcVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;

function TryOctreeItemRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
{ @groupEnd }

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

{ TOctreeItem  ------------------------------------------------------------ }

function CreateOctreeItem(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer): TOctreeItem;
begin
  result.Triangle := Triangle;
  result.TriangleNormalPlane := TriangleNormPlane(Triangle);
  result.TriangleArea := TriangleArea(Triangle);

  result.State := State;
  result.GeometryNode := GeometryNode;
  result.MatNum := MatNum;
  result.FaceCoordIndexBegin := FaceCoordIndexBegin;
  result.FaceCoordIndexEnd := FaceCoordIndexEnd;

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
  result.MailboxSavedTag := -1;
  {$endif}
end;

function TryOctreeItemSegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Odc0, OdcVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
  {$ifdef OCTREE_ITEM_USE_MAILBOX}
  if OctreeItem.MailboxSavedTag = OdcTag then
  begin
    result := OctreeItem.MailboxIsIntersection;
    if result then
    begin
      Intersection         := OctreeItem.MailboxIntersection;
      IntersectionDistance := OctreeItem.MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    Result := TryTriangleSegmentDirCollision(
      Intersection, IntersectionDistance,
      OctreeItem.Triangle, OctreeItem.TriangleNormalPlane,
      Odc0, OdcVector);
    Inc(DirectCollisionTestsCounter);

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { save result to mailbox }
    with OctreeItem do
    begin
      MailboxSavedTag := OdcTag;
      MailboxIsIntersection := result;
      if result then
      begin
        MailboxIntersection         := Intersection;
        MailboxIntersectionDistance := IntersectionDistance;
      end;
    end;
  end;
  {$endif}
end;

function TryOctreeItemRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
  { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
    z jednego Ray0 ale z roznym RayVector (np. w raytracerze). Wiec lepiej
    najpierw porownywac przechowywane w skrzynce RayVector (niz Ray0)
    zeby moc szybciej stwierdzic niezgodnosc. }
  {$ifdef OCTREE_ITEM_USE_MAILBOX}
  if OctreeItem.MailboxSavedTag = RayTag then
  begin
    result := OctreeItem.MailboxIsIntersection;
    if result then
    begin
      Intersection         := OctreeItem.MailboxIntersection;
      IntersectionDistance := OctreeItem.MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    result := TryTriangleRayCollision(
      Intersection, IntersectionDistance,
      OctreeItem.Triangle, OctreeItem.TriangleNormalPlane,
      Ray0, RayVector);
    Inc(DirectCollisionTestsCounter);

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { zapisz wyniki do mailboxa }
    with OctreeItem do
    begin
      MailboxSavedTag := RayTag;
      MailboxIsIntersection := result;
      if result then
      begin
        MailboxIntersection         := Intersection;
        MailboxIntersectionDistance := IntersectionDistance;
      end;
    end;
  end;
  {$endif}
end;

end.
