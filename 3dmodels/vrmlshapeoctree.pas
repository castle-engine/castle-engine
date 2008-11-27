{
  Copyright 2005,2008 Michalis Kamburelis.

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

{ @abstract(@link(TVRMLShapeOctree) --- octree that provides
  spatial structure of all TVRMLShape structures of a given
  @link(TVRMLScene) object.)

  Don't confuse it with @link(TVRMLTriangleOctree) from unit
  @link(VRMLTriangleOctree):
  @link(TVRMLTriangleOctree) is an octree based on scene triangles,
  while @link(TVRMLShapeOctree) is an octree based on scene
  Shapes. A scene usually has much more (e.g. 100 000, but this
  is really only an example) triangles than Shapes (e.g. 100-1000,
  but this is really only an example).

  If you want to work on triangle-by-triangle basis,
  use @link(TVRMLTriangleOctree). But if you want to work with higher-level
  objects, Shapes, use this class, @link(TVRMLShapeOctree).

  This octree is the key structure to do scene culling
  (e.g. to camera frustum) on a Shape-basis, as used
  by @link(TVRMLGLScene). }

unit VRMLShapeOctree;

{$I vrmloctreeconf.inc}

interface

uses SysUtils, Boxes3d, KambiOctree, VRMLShape, VectorMath, KambiUtils,
  VRMLTriangle;

const
  { Kambi private notes: values below found experimetally, many tests on
    /win/3dmodels/3ds/ParkKambi.wrl and /win/3dmodels/lars/scene.wrl }
  { }
  DefShapeOctreeMaxDepth = 5;
  DefShapeOctreeLeafCapacity = 10;

type
  TVRMLShapeOctree = class;

  TVRMLShapeOctreeNode = class(TVRMLBaseTrianglesOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;
  public
    function ParentTree: TVRMLShapeOctree;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function BoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;
  end;

  TVRMLShapeOctree = class(TVRMLBaseTrianglesOctree)
  private
    FShapesList: TVRMLShapesList;
    FOwnsShapesList: boolean;
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(AMaxDepth, ALeafCapacity: Integer;
      const ARootBox: TBox3d; AShapesList: TVRMLShapesList;
      AOwnsShapesList: boolean);

    destructor Destroy; override;

    function TreeRoot: TVRMLShapeOctreeNode;
    property ShapesList: TVRMLShapesList read FShapesList;
    property OwnsShapesList: boolean read FOwnsShapesList;
  end;

implementation

{$I kambioctreemacros.inc}

{ TVRMLShapeOctreeNode ---------------------------------------- }

procedure TVRMLShapeOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(
    Subnode: TOctreeNode; var Stop: boolean);
  begin
   Subnode.AddItem(ItemIndex);
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
begin
 OSIS_Box := ParentTree.ShapesList[ItemIndex].BoundingBox;

 { For safety, I'm enlarging box a little, to be sure.
   This way if BoundingBox will lie exactly on one of
   3 orthogonal planes determined by MiddlePoint then
   this ItemIndex will be said to collide with both sides
   of this plane. }
 BoxExpandTo1st(OSIS_Box, SingleEqualityEpsilon);

 OCTREE_STEP_INTO_SUBNODES
end;

function TVRMLShapeOctreeNode.ParentTree: TVRMLShapeOctree;
begin
 Result := TVRMLShapeOctree(InternalParentTree);
end;

{ TODO: temporarily, to make basic implementation, we don't do actual
  octree traversing during XxxCollision. Instead, we use
  ItemsInNonLeafNodes, and just check every item (so this will
  be done only on TreeRoot).
}

function TVRMLShapeOctreeNode.SphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalBox: TBox3d;
begin
  { TODO: this is bad, as 1. we take box around the sphere,
    and 2. we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := Box3dTransform(BoundingBox3dFromSphere(Pos, Radius),
        Shape.State.InvertedTransform);
      Result := Shape.OctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then
    begin
      Result^.UpdateWorld;
      Exit;
    end;
  end;
end;

function TVRMLShapeOctreeNode.BoxCollision(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalBox: TBox3d;
begin
  { TODO: this is bad, as we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := Box3dTransform(ABox,
        Shape.State.InvertedTransform);
      Result := Shape.OctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then
    begin
      Result^.UpdateWorld;
      Exit;
    end;
  end;
end;

function TVRMLShapeOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalPos1, LocalPos2: TVector3Single;
  ThisIntersection: TVector3Single;
  ThisIntersectionDistance: Single;
  ThisResult: PVRMLTriangle;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      try
        LocalPos1 := MatrixMultPoint(Shape.State.InvertedTransform, Pos1);
        LocalPos2 := MatrixMultPoint(Shape.State.InvertedTransform, Pos2);
        Result := Shape.OctreeTriangles.SegmentCollision(
          Intersection, IntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if Result <> nil then
      begin
        Result^.UpdateWorld;
        Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
        Exit;
      end;
    end;
  end else
  begin
    { To implement ReturnClosestIntersection = true, we use This* variables.
      We only use This* variables if they
      indicate closer intersection than currently known. }

    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      try
        LocalPos1 := MatrixMultPoint(Shape.State.InvertedTransform, Pos1);
        LocalPos2 := MatrixMultPoint(Shape.State.InvertedTransform, Pos2);
        ThisResult := Shape.OctreeTriangles.SegmentCollision(
          ThisIntersection, ThisIntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if (ThisResult <> nil) and
         ( (Result = nil) or
           (ThisIntersectionDistance < IntersectionDistance) ) then
      begin
        Intersection         := ThisIntersection;
        IntersectionDistance := ThisIntersectionDistance;
        Result               := ThisResult;
      end;
    end;

    if Result <> nil then
    begin
      Result^.UpdateWorld;
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
    end;
  end;
end;

function TVRMLShapeOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalRay0, LocalRayVector: TVector3Single;
  ThisIntersection: TVector3Single;
  ThisIntersectionDistance: Single;
  ThisResult: PVRMLTriangle;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      try
        LocalRay0 := MatrixMultPoint(Shape.State.InvertedTransform, Ray0);
        LocalRayVector := MatrixMultDirection(Shape.State.InvertedTransform, RayVector);
        Result := Shape.OctreeTriangles.RayCollision(
          Intersection, IntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if Result <> nil then
      begin
        Result^.UpdateWorld;
        Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
        Exit;
      end;
    end;
  end else
  begin
    { To implement ReturnClosestIntersection = true, we use This* variables.
      We only use This* variables if they
      indicate closer intersection than currently known. }

    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      Assert(Shape.OctreeTriangles <> nil);
      try
        LocalRay0 := MatrixMultPoint(Shape.State.InvertedTransform, Ray0);
        LocalRayVector := MatrixMultDirection(Shape.State.InvertedTransform, RayVector);
        ThisResult := Shape.OctreeTriangles.RayCollision(
          ThisIntersection, ThisIntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if (ThisResult <> nil) and
         ( (Result = nil) or
           (ThisIntersectionDistance < IntersectionDistance) ) then
      begin
        Intersection         := ThisIntersection;
        IntersectionDistance := ThisIntersectionDistance;
        Result               := ThisResult;
      end;
    end;

    if Result <> nil then
    begin
      Result^.UpdateWorld;
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
    end;
  end;
end;

{ TVRMLShapeOctree ------------------------------------------ }

constructor TVRMLShapeOctree.Create(AMaxDepth, ALeafCapacity: Integer;
  const ARootBox: TBox3d; AShapesList: TVRMLShapesList;
  AOwnsShapesList: boolean);
begin
  inherited Create(AMaxDepth, ALeafCapacity, ARootBox,
    TVRMLShapeOctreeNode, true);
  FShapesList := AShapesList;
  FOwnsShapesList := AOwnsShapesList;
end;

destructor TVRMLShapeOctree.Destroy;
begin
  if OwnsShapesList then FreeAndNil(FShapesList);
  inherited;
end;

function TVRMLShapeOctree.TreeRoot: TVRMLShapeOctreeNode;
begin
 Result := TVRMLShapeOctreeNode(InternalTreeRoot);
end;

function TVRMLShapeOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 if ShapesList.Count = 0 then
  Result :=
    '  Empty octree - scene has no Shapes, i.e. no visible nodes.' +nl else
  Result := Format(
    '  %d items (=Shapes) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each Shapes is present in tree about %f times.' +nl,
    [ ShapesList.Count, ItemsCount, ItemsCount / ShapesList.Count] );
end;

end.