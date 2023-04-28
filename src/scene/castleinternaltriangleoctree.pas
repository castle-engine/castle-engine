{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Triangle octrees (TTriangleOctree). }
unit CastleInternalTriangleOctree;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses CastleVectors, SysUtils, CastleUtils, X3DNodes, CastleBoxes,
  CastleInternalOctree, CastleInternalBaseTriangleOctree, CastleTriangles;

{$define read_interface}

const
  { }
  DefTriangleOctreeMaxDepth = 10;
  DefTriangleOctreeLeafCapacity = 20;
  DefTriangleOctreeLimits: TOctreeLimits = (
    MaxDepth: DefTriangleOctreeMaxDepth;
    LeafCapacity: DefTriangleOctreeLeafCapacity
  );

type
  TTriangleOctree = class;

  TTriangleOctreeNode = class(TBaseTrianglesOctreeNode)
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
  strict private
    function GetItems(ItemIndex: integer): PTriangle;
  public
    function ParentTree: TTriangleOctree;

    { Triangles stored in this octree leaf.
      This is a more comfortable way to access ItemsIndices array.
      Given ItemIndex indexes our ItemsIndices, and we return
      @code(ParentTree.Triangles[ItemsIndices[ItemIndex]]). }
    property Items[ItemIndex: integer]: PTriangle read GetItems;

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
      const pos1, pos2: TVector3;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle; override;

    function IsSegmentCollision(
      const pos1, pos2: TVector3;
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
  end;

  { Octree based on triangles. Allows for fast collision-detection
    with a set of triangles. Each triangle is a TTriangle structure,
    that keeps triangle geometry in 3D space, and links to parent
    X3D Shape and such. }
  TTriangleOctree = class(TBaseTrianglesOctree)
  protected
    function StatisticsBonus: string; override;
  public
    { All our triangles.

      By keeping a list of triangles here, and only keeping indexes
      to this table in leafs (in ItemsIndices) we conserve a lot of memory.
      This also allows to use mailboxes and fast TriangleToIgnore
      (because every triangle has a unique index,
      and a pointer too, shared even if this triangle is placed in multiple
      leaves). }
    Triangles: TTriangleList;

    constructor Create(const ARootBox: TBox3D); overload;
    constructor Create(const ALimits: TOctreeLimits; const ARootBox: TBox3D); overload;
    destructor Destroy; override;
    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      override;
    function TrianglesCount: Cardinal; override;
    function TotalItemsInOctree: Int64; override;

    function TreeRoot: TTriangleOctreeNode;

    { Add a single triangle. Automatically checks whether Triangle.IsValid.
      Before adding a lot of triangles, it's suggested to increase
      Triangles.AllowedCapacityCount.  }
    procedure AddItemTriangle(Shape: TObject;
      const Triangle: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);

    { Internal for cooperation with TShapeOctree.
      @exclude }
    procedure EnumerateTrianglesUpdateWorld(
      EnumerateTriangleFunc: TEnumerateTriangleFunc);
  end;

{$undef read_interface}

implementation

uses CastleShapes;

{$define read_implementation}

{ TTriangleOctreeNode -------------------------------------------------------------- }

function TTriangleOctreeNode.ItemBoundingBox(const ItemIndex: integer): TBox3D;
begin
  Result := TriangleBoundingBox(ParentTree.Triangles.L[ItemIndex].Local.Triangle);
end;

procedure TTriangleOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
var
  AddedSomewhere: boolean;
  Triangle: PTriangle3;

  procedure JustAdd(SubNode: TOctreeNode);
  begin
    SubNode.AddItem(ItemIndex);
    AddedSomewhere := true;
  end;

  procedure SecondTestAndAdd(SubNode: TOctreeNode);
  begin
    if SubNode.Box.IsTriangleCollision(Triangle^) then
    begin
      SubNode.AddItem(ItemIndex);
      AddedSomewhere := true;
    end;
  end;

var
  OSIS_b_low, OSIS_b_high: TOctreeSubnodeIndex;
  OSIS_b_0, OSIS_b_1, OSIS_b_2: boolean;
  FullTriangle: PTriangle;
begin
  AddedSomewhere := false;

  FullTriangle := PTriangle(ParentTree.Triangles.Ptr(ItemIndex));
  Triangle := Addr(FullTriangle^.Local.Triangle);

  { First prototype of this just run SecondTestAndAdd 8 times, without
    initial SubnodesWithBox checking. It turns out that it's faster
    to do SubnodesWithBox first, this way we eliminate many calls
    to IsBox3DTriangleCollision.

    Tests on http://www.web3d.org/x3d/content/examples/Basic/HumanoidAnimation/BoxMan.wrl :
    Around 6.20 / 2.41 =~ 2.5 faster.
    Tests on ../../castle/data/levels/gate/gate_final.wrl
    Around 27.8 / 12.5 =~ 2.2 faster.
  }

  SubnodesWithBox(TriangleBoundingBox(Triangle^), OSIS_b_low, OSIS_b_high);

  for OSIS_b_0 := OSIS_b_low[0] to OSIS_b_high[0] do
    for OSIS_b_1 := OSIS_b_low[1] to OSIS_b_high[1] do
      for OSIS_b_2 := OSIS_b_low[2] to OSIS_b_high[2] do
        SecondTestAndAdd(TreeSubNodes[OSIS_b_0, OSIS_b_1, OSIS_b_2]);

  if not AddedSomewhere then
  begin
    { This should not happen. But it happens, and unfortunately
      it seems unavoidable: IsBox3DTriangleCollision tries hard
      to detect that there's no collision. Even with really large
      epsilons inside IsBox3DTriangleCollision,
      it often detects no collision, while in fact triangle
      lies on the boundary of SubNode.Box.

      I tried to make epsilons inside IsBox3DTriangleCollision larger,
      use better calculation (on doubles instead of singles),
      and all of this helped... for some time. For some scenes.
      Sooner or later, I was always able to find another scene,
      so specific that requires even larger epsilon inside
      IsBox3DTriangleCollision... This is unacceptable of course.

      So I detect the cases when IsBox3DTriangleCollision works
      badly, and use more "lazy" approach in this case (that may
      result in inserting the triangle into more nodes than
      necessary --- but that's not a problem (such triangle
      happens seldom, so it doesn't really make octree less optimal)). }

    for OSIS_b_0 := OSIS_b_low[0] to OSIS_b_high[0] do
      for OSIS_b_1 := OSIS_b_low[1] to OSIS_b_high[1] do
        for OSIS_b_2 := OSIS_b_low[2] to OSIS_b_high[2] do
          JustAdd(TreeSubNodes[OSIS_b_0, OSIS_b_1, OSIS_b_2]);
  end;

{  Assert(AddedSomewhere,
    'TTriangleOctreeNode.PutItemIntoSubNodes lost a triangle');}
end;

function TTriangleOctreeNode.ParentTree: TTriangleOctree;
begin
  Result := TTriangleOctree(InternalParentTree);
end;

function TTriangleOctreeNode.GetItems(ItemIndex: integer): PTriangle;
begin
  Result := PTriangle(ParentTree.Triangles.Ptr(ItemsIndices.L[ItemIndex]));
end;

{ TTriangleOctreeNode Collisions ------------------------------------------------------ }

function TTriangleOctreeNode.SphereCollision(const pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonSphereLeaf(const pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.Count - 1 do
  begin
    Inc(TriangleCollisionTestsCounter);
    Result := Items[i];
    if IsTriangleSphereCollision(Result^.Local.Triangle,
      Result^.Local.Plane, pos, Radius) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsSphereCollision(const pos: TVector3;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := SphereCollision(pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.SphereCollision2D(const pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonSphere2D(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonSphere2DLeaf(const pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.Count - 1 do
  begin
    Inc(TriangleCollisionTestsCounter);
    Result := Items[i];
    if IsTriangleSphereCollision2D(Result^.Local.Triangle, pos, Radius) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsSphereCollision2D(const pos: TVector2;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := SphereCollision2D(pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.PointCollision2D(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonPoint2D(Point, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonPoint2DLeaf(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.Count - 1 do
  begin
    Inc(TriangleCollisionTestsCounter);
    Result := Items[i];
    if IsPointWithinTriangle2D(Point, Result^.Local.Triangle) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsPointCollision2D(const Point: TVector2;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := PointCollision2D(Point, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonBoxLeaf(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.Count - 1 do
  begin
    Inc(TriangleCollisionTestsCounter);
    Result := Items[i];
    if ABox.IsTriangleCollision(Result^.Local.Triangle) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := BoxCollision(ABox, TriangleToIgnore, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.SegmentCollision(
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

function TTriangleOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
{$define SEGMENT_COLLISION}
{$I castleinternaltriangleoctree_raysegmentcollisions.inc}
{$undef SEGMENT_COLLISION}

function TTriangleOctreeNode.IsSegmentCollision(
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
  Result := SegmentCollision(Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.RayCollision(
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

function TTriangleOctreeNode.CommonRayLeaf(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
{$I castleinternaltriangleoctree_raysegmentcollisions.inc}

function TTriangleOctreeNode.IsRayCollision(
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
  Result := RayCollision(Intersection, IntersectionDistance,
    RayOrigin, RayDirection,
    Tag,
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

{ TTriangleOctree -------------------------------------------------------- }

constructor TTriangleOctree.Create(const ARootBox: TBox3D);
begin
 Create(DefTriangleOctreeLimits, ARootBox);
end;

constructor TTriangleOctree.Create(const ALimits: TOctreeLimits;
  const ARootBox: TBox3D);
begin
 inherited Create (ALimits, ARootBox, TTriangleOctreeNode, false);
 Triangles := TTriangleList.Create;
end;

destructor TTriangleOctree.Destroy;
begin
 FreeAndNil(Triangles);
 inherited;
end;

function TTriangleOctree.TreeRoot: TTriangleOctreeNode;
begin
 Result := TTriangleOctreeNode(InternalTreeRoot);
end;

function TTriangleOctree.StatisticsBonus: string;
begin
  Result := NL;
  if Triangles.Count = 0 then
    Result := Result +
      '  Empty octree - no triangles defined.' + NL
  else
    Result := Result + Format(
      '  %d items (=triangles) defined for octree, %d items in octree''s leafs' + NL +
      '  - so each triangle is present in tree about %f times.' + NL,
      [ Triangles.Count, TotalItemsInLeafs, TotalItemsInLeafs / Triangles.Count] );
end;

function TTriangleOctree.TotalItemsInOctree: Int64;
begin
  Result := Triangles.Count;
end;

procedure TTriangleOctree.AddItemTriangle(Shape: TObject;
  const Triangle: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  if Triangle.IsValid then
  begin
    Triangles.Add^.Init(Shape, Triangle, Normal, TexCoord, Face);
    TreeRoot.AddItem(Triangles.Count - 1);
  end;
end;

procedure TTriangleOctree.EnumerateTrianglesUpdateWorld(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
  T: PTriangle;
begin
  T := PTriangle(Triangles.L);
  for I := 0 to Triangles.Count - 1 do
  begin
    T^.UpdateWorld;
    EnumerateTriangleFunc(T);
    Inc(T);
  end;
end;

procedure TTriangleOctree.EnumerateTriangles(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
begin
  for I := 0 to Triangles.Count - 1 do
    EnumerateTriangleFunc(PTriangle(Triangles.Ptr(I)));
end;

function TTriangleOctree.TrianglesCount: Cardinal;
begin
  Result := Triangles.Count;
end;

end.
