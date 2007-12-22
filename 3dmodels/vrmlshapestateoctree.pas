{
  Copyright 2005 Michalis Kamburelis.

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

{ @abstract(@link(TVRMLShapeStateOctree) --- octree that provides
  hierarchical view of all ShapeState structures of a given
  @link(TVRMLFlatScene) object.)

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
  by @link(TVRMLFlatSceneGL). }

unit VRMLShapeStateOctree;

interface

uses SysUtils, Boxes3d, KambiOctree, VRMLShapeState, VectorMath, KambiUtils;

const
  { Kambi private notes: values below found experimetally, many tests on
    /win/3dmodels/3ds/ParkKambi.wrl and /win/3dmodels/lars/scene.wrl }
  { }
  DefShapeStateOctreeMaxDepth = 5;
  DefShapeStateOctreeMaxLeafItemsCount = 10;

type
  TVRMLShapeStateOctree = class;

  TVRMLShapeStateOctreeNode = class(TOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;
  public
    function ParentTree: TVRMLShapeStateOctree;
 end;

  TVRMLShapeStateOctree = class(TOctree)
  private
    FShapeStatesList: TVRMLShapeStatesList;
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(AMaxDepth, AMaxLeafItemsCount: Integer;
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

{ TVRMLShapeStateOctree ------------------------------------------ }

constructor TVRMLShapeStateOctree.Create(AMaxDepth, AMaxLeafItemsCount: Integer;
  const ARootBox: TBox3d; AShapeStatesList: TVRMLShapeStatesList);
begin
 inherited Create(AMaxDepth, AMaxLeafItemsCount, ARootBox,
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