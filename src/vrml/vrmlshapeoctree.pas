{
  Copyright 2005-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Octrees for fast 3D seaching in VRML shapes (TVRMLShapeOctree).

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

uses SysUtils, Base3D, Boxes3D, KambiOctree, VRMLShape, VectorMath, KambiUtils,
  VRMLTriangle;

const
  { Kambi private notes: values below found experimetally, many tests on
    /win/3dmodels/3ds/ParkKambi.wrl and /win/3dmodels/lars/scene.wrl }
  { }
  DefShapeOctreeMaxDepth = 5;
  DefShapeOctreeLeafCapacity = 10;
  DefShapeOctreeLimits: TOctreeLimits = (
    MaxDepth: DefShapeOctreeMaxDepth;
    LeafCapacity: DefShapeOctreeLeafCapacity
  );

type
  TVRMLShapeOctree = class;

  TVRMLShapeOctreeNode = class(TVRMLBaseTrianglesOctreeNode)
  private
    { These do the job of their counterparts (without Local prefix),
      except they do not transform resulting triangle and Intersection
      position back to the world coordinates.

      The idea is that non-local versions with IsXxx do not need to do
      this transformation, thus saving a tiny amount of time.
      And the non-local versions without "Is" can just transform
      the result at the end, if non-nil. }
    function LocalSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function LocalBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function LocalSegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function LocalRayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonBoxLeaf(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;
  public
    function ParentTree: TVRMLShapeOctree;
    function ParentNode: TVRMLShapeOctreeNode;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsSegmentCollision(
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

  public
    { For Hierarchical Occlusion Culling }
    LastVisitedFrameId: Cardinal;
    Visible: boolean;
  end;

  TVRMLShapeOctree = class(TVRMLBaseTrianglesOctree)
  private
    FShapesList: TVRMLShapeList;
    FOwnsShapesList: boolean;
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(const ALimits: TOctreeLimits;
      const ARootBox: TBox3D; AShapesList: TVRMLShapeList;
      AOwnsShapesList: boolean);

    destructor Destroy; override;

    function TreeRoot: TVRMLShapeOctreeNode;
    property ShapesList: TVRMLShapeList read FShapesList;
    property OwnsShapesList: boolean read FOwnsShapesList;

    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      override;
    function TrianglesCount: Cardinal; override;
  end;

implementation

{ TVRMLShapeOctreeNode ---------------------------------------- }

procedure TVRMLShapeOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  SubnodesBox: TBox3D;
  B0, B1, B2: boolean;
begin
  SubnodesBox := ParentTree.ShapesList[ItemIndex].BoundingBox;

  { We assume below that SubnodesBox is not empty.
    Only such shapes can be added to TVRMLShapeOctree. }

  { For safety, I'm enlarging box a little, to be sure.
    This way if BoundingBox will lie exactly on one of
    3 orthogonal planes determined by MiddlePoint then
    this ItemIndex will be said to collide with both sides
    of this plane. }
  SubnodesBox.ExpandMe(SingleEqualityEpsilon);

  SubnodesWithBox(SubnodesBox, BoxLo, BoxHi);

  for B0 := BoxLo[0] to BoxHi[0] do
    for B1 := BoxLo[1] to BoxHi[1] do
      for B2 := BoxLo[2] to BoxHi[2] do
        TreeSubNodes[B0, B1, B2].AddItem(ItemIndex);
end;

function TVRMLShapeOctreeNode.ParentTree: TVRMLShapeOctree;
begin
  Result := TVRMLShapeOctree(InternalParentTree);
end;

function TVRMLShapeOctreeNode.ParentNode: TVRMLShapeOctreeNode;
begin
  Result := TVRMLShapeOctreeNode(InternalParentNode);
end;

function TVRMLShapeOctreeNode.CommonSphereLeaf(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalBox: TBox3D;
begin
  { TODO: this is bad, as 1. we take box around the sphere,
    and 2. we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := BoundingBox3DFromSphere(Pos, Radius).Transform(
        Shape.State.InvertedTransform);
      Result := Shape.OctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TVRMLShapeOctreeNode.LocalSphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLShapeOctreeNode.SphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TVRMLShapeOctreeNode.IsSphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TVRMLShapeOctreeNode.CommonBoxLeaf(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
var
  I: Integer;
  Shape: TVRMLShape;
  LocalBox: TBox3D;
begin
  { TODO: this is bad, as we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := ABox.Transform(Shape.State.InvertedTransform);
      Result := Shape.OctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TVRMLShapeOctreeNode.LocalBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLShapeOctreeNode.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TVRMLShapeOctreeNode.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TVRMLShapeOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
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
        Result := Shape.SegmentCollision(Tag,
          Intersection, IntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;

      if Result <> nil then
      begin
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
        ThisResult := Shape.SegmentCollision(Tag,
          ThisIntersection, ThisIntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do ThisResult := nil;
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
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
  end;
end;

function TVRMLShapeOctreeNode.LocalSegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonSegment(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore,
    IgnoreMarginAtStart, TrianglesToIgnoreFunc);
end;

function TVRMLShapeOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := LocalSegmentCollision(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);

  { At one point, I also did here
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
    But it turns out that CommonRay/Segment (for non-leaf  nodes) code
    requires the returned intersection to be in correct (global, for this
    octree) coordinates --- see it's
    Box3DPointInsideTolerant(Intersection, SubNode.Box) test.
    So Intersection must be transformed back already in CommonSegmentLeaf. }
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TVRMLShapeOctreeNode.IsSegmentCollision(
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  Result := LocalSegmentCollision(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    {ReturnClosestIntersection}false, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc) <> nil;
end;

function TVRMLShapeOctreeNode.CommonRayLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
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
        Result := Shape.RayCollision(Tag,
          Intersection, IntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;

      if Result <> nil then
      begin
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
        ThisResult := Shape.RayCollision(Tag,
          ThisIntersection, ThisIntersectionDistance, LocalRay0, LocalRayVector,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do ThisResult := nil;
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
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
  end;
end;

function TVRMLShapeOctreeNode.LocalRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonRay(
    Intersection, IntersectionDistance, Ray0, RayVector,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TVRMLShapeOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := LocalRayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);

  { At one point, I also did here
      Intersection := MatrixMultPoint(Result^.State.Transform, Intersection);
    But it turns out that CommonRay/Segment (for non-leaf  nodes) code
    requires the returned intersection to be in correct (global, for this
    octree) coordinates --- see it's
    Box3DPointInsideTolerant(Intersection, SubNode.Box) test.
    So Intersection must be transformed back already in CommonSegmentLeaf. }
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TVRMLShapeOctreeNode.IsRayCollision(
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  Result := LocalRayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    Tag,
    {ReturnClosestIntersection}false, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc) <> nil;
end;

{ TVRMLShapeOctree ------------------------------------------ }

constructor TVRMLShapeOctree.Create(const ALimits: TOctreeLimits;
  const ARootBox: TBox3D; AShapesList: TVRMLShapeList;
  AOwnsShapesList: boolean);
begin
  inherited Create(ALimits, ARootBox, TVRMLShapeOctreeNode, true);
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
 Result := nl+
   Format(
   '  Octree constructed with limits: max depth %d, leaf capacity %d.',
   [MaxDepth, LeafCapacity]) + nl + nl;

 if ShapesList.Count = 0 then
  Result +=
    '  Empty octree - scene has no Shapes, i.e. no visible nodes.' +nl else
  Result += Format(
    '  %d items (=Shapes) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each shape is present in tree about %f times.' +nl,
    [ ShapesList.Count, ItemsCount, ItemsCount / ShapesList.Count] );
end;

procedure TVRMLShapeOctree.EnumerateTriangles(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
begin
  for I := 0 to ShapesList.Count - 1 do
    ShapesList.Items[I].OctreeTriangles.EnumerateTrianglesUpdateWorld(
      EnumerateTriangleFunc);
end;

function TVRMLShapeOctree.TrianglesCount: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ShapesList.Count - 1 do
    Result += ShapesList.Items[I].OctreeTriangles.TrianglesCount;
end;

end.
