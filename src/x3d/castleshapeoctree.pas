{
  Copyright 2005-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Octrees for fast 3D seaching in VRML/X3D shapes (TShapeOctree).

  Don't confuse it with @link(TTriangleOctree):
  @link(TTriangleOctree) is an octree based on scene triangles,
  while @link(TShapeOctree) is an octree based on scene
  Shapes. A scene usually has much more (e.g. 100 000, but this
  is really only an example) triangles than Shapes (e.g. 100-1000,
  but this is really only an example).

  If you want to work on triangle-by-triangle basis,
  use @link(TTriangleOctree). But if you want to work with higher-level
  objects, Shapes, use this class, @link(TShapeOctree).

  This octree is the key structure to do scene culling
  (e.g. to camera frustum) on a Shape-basis, as used
  by @link(TCastleScene). }

unit CastleShapeOctree;

{$I octreeconf.inc}

interface

uses SysUtils, CastleBoxes, CastleOctree, CastleShapes, CastleVectors,
  CastleUtils, X3DTriangles, CastleTriangles;

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
    function LocalSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function LocalSphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function LocalPointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function LocalBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function LocalSegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function LocalRayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function CommonSphere2DLeaf(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function CommonPoint2DLeaf(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function CommonBoxLeaf(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;
  public
    function ParentTree: TShapeOctree;
    function ParentNode: TShapeOctreeNode;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function SphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsSphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function PointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsPointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsSegmentCollision(
      const Pos1, Pos2: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; override;

    function IsRayCollision(
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;

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
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(const ALimits: TOctreeLimits;
      const ARootBox: TBox3D; AShapesList: TShapeList;
      AOwnsShapesList: boolean);

    destructor Destroy; override;

    function TreeRoot: TShapeOctreeNode;
    property ShapesList: TShapeList read FShapesList;
    property OwnsShapesList: boolean read FOwnsShapesList;

    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      override;
    function TrianglesCount: Cardinal; override;
  end;

implementation

{ TShapeOctreeNode ---------------------------------------- }

procedure TShapeOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  SubnodesBox: TBox3D;
  B0, B1, B2: boolean;
begin
  SubnodesBox := ParentTree.ShapesList[ItemIndex].BoundingBox;

  { We assume below that SubnodesBox is not empty.
    Only such shapes can be added to TShapeOctree. }

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

function TShapeOctreeNode.ParentTree: TShapeOctree;
begin
  Result := TShapeOctree(InternalParentTree);
end;

function TShapeOctreeNode.ParentNode: TShapeOctreeNode;
begin
  Result := TShapeOctreeNode(InternalParentNode);
end;

function TShapeOctreeNode.CommonSphereLeaf(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
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
      Result := Shape.InternalOctreeTriangles.BoxCollision(
        LocalBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalSphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsSphereCollision(const Pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalSphereCollision(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonSphere2DLeaf(const Pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPos: TVector2Single;
  LocalRadius: Single;
begin
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalPos := MatrixMultPoint(Shape.State.InvertedTransform, Pos);
      LocalRadius := Radius / Shape.State.TransformScale;
      Result := Shape.InternalOctreeTriangles.SphereCollision2D(
        LocalPos, LocalRadius, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalSphereCollision2D(const Pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere2D(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SphereCollision2D(const Pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalSphereCollision2D(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsSphereCollision2D(const Pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalSphereCollision2D(Pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonPoint2DLeaf(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPoint: TVector2Single;
begin
  Result := nil;
  for I := 0 to ItemsIndices.Count - 1 do
  begin
    Shape := ParentTree.ShapesList.Items[ItemsIndices.Items[I]];
    try
      LocalPoint := MatrixMultPoint(Shape.State.InvertedTransform, Point);
      Result := Shape.InternalOctreeTriangles.PointCollision2D(
        LocalPoint, TriangleToIgnore, TrianglesToIgnoreFunc);
    except
      on ETransformedResultInvalid do Result := nil;
    end;
    if Result <> nil then Exit;
  end;
end;

function TShapeOctreeNode.LocalPointCollision2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonPoint2D(Point, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.PointCollision2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalPointCollision2D(Point, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsPointCollision2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalPointCollision2D(Point, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonBoxLeaf(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
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
      LocalBox := ABox.Transform(Shape.State.InvertedTransform);
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
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc);
  if Result <> nil then
    Result^.UpdateWorld;
end;

function TShapeOctreeNode.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := LocalBoxCollision(ABox, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TShapeOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalPos1, LocalPos2: TVector3Single;
  ThisIntersection: TVector3Single;
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

function TShapeOctreeNode.LocalSegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSegment(
    Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore,
    IgnoreMarginAtStart, TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
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

function TShapeOctreeNode.IsSegmentCollision(
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PTriangle;
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

function TShapeOctreeNode.CommonRayLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  I: Integer;
  Shape: TShape;
  LocalRayOrigin, LocalRayDirection: TVector3Single;
  ThisIntersection: TVector3Single;
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
        LocalRayOrigin := MatrixMultPoint(Shape.State.InvertedTransform, RayOrigin);
        LocalRayDirection := MatrixMultDirection(Shape.State.InvertedTransform, RayDirection);
        Result := Shape.RayCollision(Tag,
          Intersection, IntersectionDistance, LocalRayOrigin, LocalRayDirection,
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
      Assert(Shape.InternalOctreeTriangles <> nil);
      try
        LocalRayOrigin := MatrixMultPoint(Shape.State.InvertedTransform, RayOrigin);
        LocalRayDirection := MatrixMultDirection(Shape.State.InvertedTransform, RayDirection);
        ThisResult := Shape.RayCollision(Tag,
          ThisIntersection, ThisIntersectionDistance, LocalRayOrigin, LocalRayDirection,
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

function TShapeOctreeNode.LocalRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonRay(
    Intersection, IntersectionDistance, RayOrigin, RayDirection,
    Tag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TShapeOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := LocalRayCollision(Intersection, IntersectionDistance,
    RayOrigin, RayDirection,
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

function TShapeOctreeNode.IsRayCollision(
  const RayOrigin, RayDirection: TVector3Single;
  const Tag: TMailboxTag;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3Single;
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
  const ARootBox: TBox3D; AShapesList: TShapeList;
  AOwnsShapesList: boolean);
begin
  inherited Create(ALimits, ARootBox, TShapeOctreeNode, true);
  FShapesList := AShapesList;
  FOwnsShapesList := AOwnsShapesList;
end;

destructor TShapeOctree.Destroy;
begin
  if OwnsShapesList then FreeAndNil(FShapesList);
  inherited;
end;

function TShapeOctree.TreeRoot: TShapeOctreeNode;
begin
 Result := TShapeOctreeNode(InternalTreeRoot);
end;

function TShapeOctree.StatisticsBonus(
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
    Result += ShapesList.Items[I].InternalOctreeTriangles.TrianglesCount;
end;

end.
