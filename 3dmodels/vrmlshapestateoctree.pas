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

{ @abstract(@link(TVRMLShapeStateOctree) --- octree that provides
  hierarchical view of all ShapeState structures of a given
  @link(TVRMLScene) object.)

  Don't confuse it with @link(TVRMLTriangleOctree) from unit
  @link(VRMLTriangleOctree):
  @link(TVRMLTriangleOctree) is an octree based on scene triangles,
  while @link(TVRMLShapeStateOctree) is an octree based on scene
  ShapeStates. A scene usually has much more (e.g. 100 000, but this
  is really only an example) triangles than ShapeStates (e.g. 100-1000,
  but this is really only an example).

  If you want to work of triangle-by-triangle basis,
  use @link(TVRMLTriangleOctree). But if you want to work with higher-level
  objects, ShapeStates, use this class, @link(TVRMLShapeStateOctree).

  This octree is the key structure to do scene culling
  (e.g. to camera frustum) on a ShapeState-basis, as used
  by @link(TVRMLGLScene). }

unit VRMLShapeStateOctree;

{$I vrmloctreeconf.inc}

interface

uses SysUtils, Boxes3d, KambiOctree, VRMLShapeState, VectorMath, KambiUtils,
  VRMLOctreeItems;

const
  { Kambi private notes: values below found experimetally, many tests on
    /win/3dmodels/3ds/ParkKambi.wrl and /win/3dmodels/lars/scene.wrl }
  { }
  DefShapeStateOctreeMaxDepth = 5;
  DefShapeStateOctreeLeafCapacity = 10;

type
  TVRMLShapeStateOctree = class;

  TVRMLShapeStateOctreeNode = class(TVRMLItemsOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;
  public
    function ParentTree: TVRMLShapeStateOctree;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; override;

    function BoxCollision(const ABox: TBox3d;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; override;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; override;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; override;
  end;

  TVRMLShapeStateOctree = class(TVRMLItemsOctree)
  private
    FShapeStatesList: TVRMLShapeStatesList;
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(AMaxDepth, ALeafCapacity: Integer;
      const ARootBox: TBox3d; AShapeStatesList: TVRMLShapeStatesList);
    function TreeRoot: TVRMLShapeStateOctreeNode;
    property ShapeStatesList: TVRMLShapeStatesList read FShapeStatesList;
  end;

implementation

{$I kambioctreemacros.inc}

{ TVRMLShapeStateOctreeNode ---------------------------------------- }

procedure TVRMLShapeStateOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(
    Subnode: TOctreeNode; var Stop: boolean);
  begin
   Subnode.AddItem(ItemIndex);
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
begin
 OSIS_Box := ParentTree.ShapeStatesList[ItemIndex].BoundingBox;

 { For safety, I'm enlarging box a little, to be sure.
   This way if BoundingBox will lie exactly on one of
   3 orthogonal planes determined by MiddlePoint then
   this ItemIndex will be said to collide with both sides
   of this plane. }
 BoxExpandTo1st(OSIS_Box, SingleEqualityEpsilon);

 OCTREE_STEP_INTO_SUBNODES
end;

function TVRMLShapeStateOctreeNode.ParentTree: TVRMLShapeStateOctree;
begin
 Result := TVRMLShapeStateOctree(InternalParentTree);
end;

{ TODO: temporarily, to make basic implementation, we don't do actual
  octree traversing during XxxCollision. Instead, we use
  ItemsInNonLeafNodes, and just check every item (so this will
  be done only on TreeRoot).
}

function TVRMLShapeStateOctreeNode.SphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
var
  I: Integer;
  ShapeState: TVRMLShapeState;
  LocalBox: TBox3d;
begin
  { TODO: this is bad, as 1. we take box around the sphere,
    and 2. we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := Box3dTransform(BoundingBox3dFromSphere(Pos, Radius),
        ShapeState.State.InvertedTransform);
      Result := ShapeState.OctreeTriangles.BoxCollision(
        LocalBox, OctreeItemToIgnore, ItemsToIgnoreFunc);
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

function TVRMLShapeStateOctreeNode.BoxCollision(const ABox: TBox3d;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
var
  I: Integer;
  ShapeState: TVRMLShapeState;
  LocalBox: TBox3d;
begin
  { TODO: this is bad, as we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := Box3dTransform(ABox,
        ShapeState.State.InvertedTransform);
      Result := ShapeState.OctreeTriangles.BoxCollision(
        LocalBox, OctreeItemToIgnore, ItemsToIgnoreFunc);
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

function TVRMLShapeStateOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
var
  I: Integer;
  ShapeState: TVRMLShapeState;
  LocalPos1, LocalPos2: TVector3Single;
  ThisIntersection: TVector3Single;
  ThisIntersectionDistance: Single;
  ThisResult: POctreeItem;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
      try
        LocalPos1 := MatrixMultPoint(ShapeState.State.InvertedTransform, Pos1);
        LocalPos2 := MatrixMultPoint(ShapeState.State.InvertedTransform, Pos2);
        Result := ShapeState.OctreeTriangles.SegmentCollision(
          Intersection, IntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          OctreeItemToIgnore, IgnoreMarginAtStart, ItemsToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if Result <> nil then
      begin
        Result^.UpdateWorld;
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
      ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
      try
        LocalPos1 := MatrixMultPoint(ShapeState.State.InvertedTransform, Pos1);
        LocalPos2 := MatrixMultPoint(ShapeState.State.InvertedTransform, Pos2);
        ThisResult := ShapeState.OctreeTriangles.SegmentCollision(
          ThisIntersection, ThisIntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          OctreeItemToIgnore, IgnoreMarginAtStart, ItemsToIgnoreFunc);
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
      Result^.UpdateWorld;
  end;
end;

function TVRMLShapeStateOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
var
  I: Integer;
  ShapeState: TVRMLShapeState;
  LocalRay0, LocalRayVector: TVector3Single;
  ThisIntersection: TVector3Single;
  ThisIntersectionDistance: Single;
  ThisResult: POctreeItem;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
      try
        LocalRay0 := MatrixMultPoint(ShapeState.State.InvertedTransform, Ray0);
        LocalRayVector := MatrixMultDirection(ShapeState.State.InvertedTransform, RayVector);
        Result := ShapeState.OctreeTriangles.RayCollision(
          Intersection, IntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          OctreeItemToIgnore, IgnoreMarginAtStart, ItemsToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;
      if Result <> nil then
      begin
        Result^.UpdateWorld;
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
      ShapeState := ParentTree.ShapeStatesList.Items[ItemsIndices.Items[I]];
      try
        LocalRay0 := MatrixMultPoint(ShapeState.State.InvertedTransform, Ray0);
        LocalRayVector := MatrixMultDirection(ShapeState.State.InvertedTransform, RayVector);
        ThisResult := ShapeState.OctreeTriangles.RayCollision(
          ThisIntersection, ThisIntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          OctreeItemToIgnore, IgnoreMarginAtStart, ItemsToIgnoreFunc);
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
      Result^.UpdateWorld;
  end;
end;

{ TVRMLShapeStateOctree ------------------------------------------ }

constructor TVRMLShapeStateOctree.Create(AMaxDepth, ALeafCapacity: Integer;
  const ARootBox: TBox3d; AShapeStatesList: TVRMLShapeStatesList);
begin
 inherited Create(AMaxDepth, ALeafCapacity, ARootBox,
   TVRMLShapeStateOctreeNode, true);
 FShapeStatesList := AShapeStatesList;
end;

function TVRMLShapeStateOctree.TreeRoot: TVRMLShapeStateOctreeNode;
begin
 Result := TVRMLShapeStateOctreeNode(InternalTreeRoot);
end;

function TVRMLShapeStateOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 if ShapeStatesList.Count = 0 then
  Result :=
    '  Empty octree - scene has no ShapeStates, i.e. no visible nodes.' +nl else
  Result := Format(
    '  %d items (=ShapeStates) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each ShapeStates is present in tree about %f times.' +nl,
    [ ShapeStatesList.Count, ItemsCount, ItemsCount / ShapeStatesList.Count] );
end;

end.