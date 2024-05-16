{
  Copyright 2005-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Octree for fast 3D collision detection in VRML/X3D shapes (TShapeOctree).

  Contrast this with @link(TTriangleOctree):
  @link(TTriangleOctree) is an octree based on scene triangles,
  while @link(TShapeOctree) is an octree based on scene shapes.
  A scene usually has much more triangles than shapes.

  This octree is the key structure to do scene culling on a per-shape-basis,
  e.g. to camera frustum, as used by @link(TCastleScene).
  It can also be used to perform collision detection that starts working per-shape,
  to choose the shapes that potentially collide, and then run per-triangle collision
  detection within the shape, using @link(TShape.InternalOctreeTriangles). }
unit CastleInternalShapeOctree;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses SysUtils, CastleBoxes, CastleInternalOctree, CastleShapes, CastleVectors,
  CastleUtils, CastleInternalBaseTriangleOctree, CastleTriangles;

const
  { }
  DefShapeOctreeMaxDepth = 5;
  DefShapeOctreeLeafCapacity = 10;
  DefShapeOctreeLimits: TOctreeLimits = (
    MaxDepth: DefShapeOctreeMaxDepth;
    LeafCapacity: DefShapeOctreeLeafCapacity
  );

type
  TShapeOctree = class;

  TShapeOctreeNode = class(TBaseTrianglesOctreeNode)
  private
    { These do the job of their counterparts (without Local prefix),
      except they do not transform resulting triangle and Intersection
      position back to the world coordinates.

      The idea is that non-local versions with IsXxx do not need to do
      this transformation, thus saving a tiny amount of time.
      And the non-local versions without "Is" can just transform
      the result at the end, if non-nil. }
    function LocalSphereCollision(const pos: TVector3;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    function LocalSphereCollision2D(const pos: TVector2;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    function LocalPointCollision2D(const Point: TVector2;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    function LocalBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    function LocalSegmentCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    function LocalRayCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
  protected
    function ItemBoundingBox(const ItemIndex: integer): TBox3D; override;
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;

    function CommonSphereLeaf(const pos: TVector3;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function CommonSphere2DLeaf(const pos: TVector2;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function CommonPoint2DLeaf(const Point: TVector2;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function CommonBoxLeaf(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function CommonSegmentLeaf(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function CommonRayLeaf(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;
  public
    function ParentTree: TShapeOctree;
    function ParentNode: TShapeOctreeNode;

    function SphereCollision(const pos: TVector3;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsSphereCollision(const pos: TVector3;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

    function SphereCollision2D(const pos: TVector2;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsSphereCollision2D(const pos: TVector2;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

    function PointCollision2D(const Point: TVector2;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsPointCollision2D(const Point: TVector2;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsSegmentCollision(
      const Pos1, Pos2: TVector3;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsRayCollision(
      const RayOrigin, RayDirection: TVector3;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;

  public
    { For Hierarchical Occlusion Culling }
    LastVisitedFrameId: Cardinal;
    Visible: boolean;
  end;

  TShapeOctree = class(TBaseTrianglesOctree)
  private
    FShapesList: TShapeList;
    FOwnsShapesList: boolean;
  protected
    function StatisticsBonus: string; override;
  public
    { Constructor.
      Given TShapeList contents are copied. }
    constructor Create(const ALimits: TOctreeLimits;
      const ARootBox: TBox3D; AShapesList: TShapeList);
    destructor Destroy; override;
    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      override;
    function TrianglesCount: Cardinal; override;
    function TotalItemsInOctree: Int64; override;

    function TreeRoot: TShapeOctreeNode;
    property ShapesList: TShapeList read FShapesList;
    property OwnsShapesList: boolean read FOwnsShapesList;
  end;

implementation

uses CastleLog;

{ TShapeOctreeNode ---------------------------------------- }

function TShapeOctreeNode.ItemBoundingBox(const ItemIndex: integer): TBox3D;
begin
  Result := ParentTree.ShapesList[ItemIndex].BoundingBox;

  { We assume below that SubnodesBox is not empty.
    Only such shapes can be added to TShapeOctree. }

  { For safety, I'm enlarging box a little, to be sure.
    This way if BoundingBox will lie exactly on one of
    3 orthogonal planes determined by MiddlePoint then
    this ItemIndex will be said to collide with both sides
    of this plane. }
  Result.ExpandMe(SingleEpsilon);
end;

procedure TShapeOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  SubnodesBox: TBox3D;
  B0, B1, B2: boolean;
begin
  SubnodesBox := ItemBoundingBox(ItemIndex);
  SubnodesWithBox(SubnodesBox, BoxLo, BoxHi);

  for B0 := BoxLo[0] to BoxHi[0] do
    for B1 := BoxLo[1] to BoxHi[1] do
      for B2 := BoxLo[2] to BoxHi[2] do
        TreeSubNodes[B0, B1, B2].AddItem(ItemIndex);
end;

function TShapeOctreeNode.ParentTree: TShapeOctree;
begin
  Result := TShapeOctree(InternalParentTree);
end;

function TShapeOctreeNode.ParentNode: TShapeOctreeNode;
begin
  Result := TShapeOctreeNode(InternalParentNode);
end;

function TShapeOctreeNode.CommonSphereLeaf(const Pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  SafeCollisionsSphereAsBox: Boolean;
  LocalBox: TBox3D;
  LocalPos: TVector3;
  LocalRadius: Single;
begin
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      { Using SafeCollisionsSphereAsBox means that sphere is actually transformed to a box.
        This means that it collides as something more than it should.
        OTOH, this accounts for non-uniform scale. }
      SafeCollisionsSphereAsBox := not Shape.State.Transformation.UniformScale;

      if SafeCollisionsSphereAsBox then
      begin
        // too verbose for normal usage
        // WritelnWarning('Non-uniform scale, SphereCollision will not work precisely, approximating sphere with larger box');
        LocalBox := BoundingBox3DFromSphere(Pos, Radius).Transform(
          Shape.State.Transformation.InverseTransform);
        Result := Shape.InternalOctreeTriangles.BoxCollision(
          LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
      end else
      begin
        LocalPos := Shape.State.Transformation.InverseTransform.MultPoint(Pos);
        LocalRadius := Radius / Shape.State.Transformation.Scale;
        Result := Shape.InternalOctreeTriangles.SphereCollision(
          LocalPos, LocalRadius, TriangleToIgnore, TrianglesToIgnoreFunc);
      end;
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalSphereCollision(const Pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SphereCollision(const Pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsSphereCollision(const Pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonSphere2DLeaf(const Pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPos: TVector2;
  LocalRadius: Single;
begin
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalPos := Shape.State.Transformation.InverseTransform.MultPoint(Pos);
      LocalRadius := Radius / Shape.State.Transformation.Scale;
      Result := Shape.InternalOctreeTriangles.SphereCollision2D(
        LocalPos, LocalRadius, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalSphereCollision2D(const Pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere2D(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SphereCollision2D(const Pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalSphereCollision2D(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsSphereCollision2D(const Pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := LocalSphereCollision2D(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonPoint2DLeaf(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPoint: TVector2;
begin
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalPoint := Shape.State.Transformation.InverseTransform.MultPoint(Point);
      Result := Shape.InternalOctreeTriangles.PointCollision2D(
        LocalPoint, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalPointCollision2D(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonPoint2D(Point, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.PointCollision2D(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalPointCollision2D(Point, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsPointCollision2D(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := LocalPointCollision2D(Point, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonBoxLeaf(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalBox: TBox3D;
begin
  { TODO: this is bad, as we transform this box, making larger box.
    This means that collision is done vs something larger than it should be. }
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalBox := ABox.Transform(Shape.State.Transformation.InverseTransform);
      Result := Shape.InternalOctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPos1, LocalPos2: TVector3;
  ThisIntersection: TVector3;
  ThisIntersectionDistance: Single;
  ThisResult: PTriangle;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      try
        LocalPos1 := Shape.State.Transformation.InverseTransform.MultPoint(Pos1);
        LocalPos2 := Shape.State.Transformation.InverseTransform.MultPoint(Pos2);
        Result := Shape.SegmentCollision(Tag,
          Intersection, IntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;

      if Result <> nil then
      begin
        Intersection := Result^.State.Transformation.Transform.MultPoint(Intersection);
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
      ThisResult := nil;
      try
        LocalPos1 := Shape.State.Transformation.InverseTransform.MultPoint(Pos1);
        LocalPos2 := Shape.State.Transformation.InverseTransform.MultPoint(Pos2);
        ThisResult := Shape.SegmentCollision(Tag,
          ThisIntersection, ThisIntersectionDistance, LocalPos1, LocalPos2,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do ;
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
      Intersection := Result^.State.Transformation.Transform.MultPoint(Intersection);
  end;
end;

function TShapeOctreeNode.LocalSegmentCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSegment(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore,
    IgnoreMarginAtStart, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SegmentCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalSegmentCollision(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);

  { At one point, I also did here
      Intersection := Result^.State.Transform.MultPoint(Intersection);
    But it turns out that CommonRay/Segment (for non-leaf  nodes) code
    requires the returned intersection to be in correct (global, for this
    octree) coordinates --- see it's
    Box3DContainsTolerant(Intersection, SubNode.Box) test.
    So Intersection must be transformed back already in CommonSegmentLeaf. }
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsSegmentCollision(
  const Pos1, Pos2: TVector3;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3;
  IntersectionDistance: Single;
begin
  Result := LocalSegmentCollision(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    {ReturnClosestIntersection}false, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonRayLeaf(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalRayOrigin, LocalRayDirection: TVector3;
  ThisIntersection: TVector3;
  ThisIntersectionDistance: Single;
  ThisResult: PTriangle;
begin
  Result := nil;

  if not ReturnClosestIntersection then
  begin
    for I := 0 to ItemsIndices.Count - 1 do
    begin
      Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
      try
        LocalRayOrigin := Shape.State.Transformation.InverseTransform.MultPoint(RayOrigin);
        LocalRayDirection := Shape.State.Transformation.InverseTransform.MultDirection(RayDirection);
        Result := Shape.RayCollision(Tag,
          Intersection, IntersectionDistance, LocalRayOrigin, LocalRayDirection,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do Result := nil;
      end;

      if Result <> nil then
      begin
        Intersection := Result^.State.Transformation.Transform.MultPoint(Intersection);
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
      Assert(Shape.InternalOctreeTriangles <> nil);
      ThisResult := nil;
      try
        LocalRayOrigin := Shape.State.Transformation.InverseTransform.MultPoint(RayOrigin);
        LocalRayDirection := Shape.State.Transformation.InverseTransform.MultDirection(RayDirection);
        ThisResult := Shape.RayCollision(Tag,
          ThisIntersection, ThisIntersectionDistance, LocalRayOrigin, LocalRayDirection,
          ReturnClosestIntersection,
          TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);
      except
        on ETransformedResultInvalid do ;
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
      Intersection := Result^.State.Transformation.Transform.MultPoint(Intersection);
  end;
end;

function TShapeOctreeNode.LocalRayCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonRay(
    Intersection, IntersectionDistance, RayOrigin, RayDirection,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.RayCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalRayCollision(Intersection, IntersectionDistance,
    RayOrigin, RayDirection,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);

  { At one point, I also did here
      Intersection := Result^.State.Transform.MultPoint(Intersection);
    But it turns out that CommonRay/Segment (for non-leaf  nodes) code
    requires the returned intersection to be in correct (global, for this
    octree) coordinates --- see it's
    Box3DContainsTolerant(Intersection, SubNode.Box) test.
    So Intersection must be transformed back already in CommonSegmentLeaf. }
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsRayCollision(
  const RayOrigin, RayDirection: TVector3;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3;
  IntersectionDistance: Single;
begin
  Result := LocalRayCollision(Intersection, IntersectionDistance,
    RayOrigin, RayDirection,
    Tag,
    {ReturnClosestIntersection}false, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc) <> nil;
end;

{ TShapeOctree ------------------------------------------ }

constructor TShapeOctree.Create(const ALimits: TOctreeLimits;
  const ARootBox: TBox3D; AShapesList: TShapeList);
begin
  inherited Create(ALimits, ARootBox, TShapeOctreeNode, true);

  FShapesList := TShapeList.Create(false);
  FShapesList.AddRange(AShapesList);
  FOwnsShapesList := true; // for now always true
end;

destructor TShapeOctree.Destroy;
begin
  if OwnsShapesList then
    FreeAndNil(FShapesList);
  inherited;
end;

function TShapeOctree.TreeRoot: TShapeOctreeNode;
begin
 Result := TShapeOctreeNode(InternalTreeRoot);
end;

function TShapeOctree.StatisticsBonus: string;
begin
  Result := NL;
  if ShapesList.Count = 0 then
    Result := Result + NL + NL +
      '  Empty octree - scene has no Shapes, i.e. no visible nodes.' + NL
  else
    Result := Result + Format(
      '  %d items (=Shapes) defined for octree, %d items in octree''s leafs' + NL +
      '  - so each shape is present in tree about %f times.' + NL +
      '  (Not counting shapes duplicated in internal nodes.)' + NL,
      [ShapesList.Count, TotalItemsInLeafs, TotalItemsInLeafs / ShapesList.Count] );
end;

function TShapeOctree.TotalItemsInOctree: Int64;
begin
  Result := ShapesList.Count;
end;

procedure TShapeOctree.EnumerateTriangles(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
begin
  for I := 0 to ShapesList.Count - 1 do
    ShapesList.Items[I].InternalOctreeTriangles.EnumerateTrianglesUpdateWorld(
      EnumerateTriangleFunc);
end;

function TShapeOctree.TrianglesCount: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ShapesList.Count - 1 do
    Result := Result + ShapesList.Items[I].InternalOctreeTriangles.TrianglesCount;
end;

end.
