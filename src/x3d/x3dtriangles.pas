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

{ Triangles in VRML/X3D models (TTriangle) and octrees
  that resolve collisions with such triangles (TBaseTrianglesOctree). }
unit X3DTriangles;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses CastleVectors, SysUtils, CastleUtils, X3DNodes, Castle3D, CastleBoxes,
  CastleInternalOctree, CastleGenericLists, CastleTriangles;

{ TTriangle  ------------------------------------------------------------ }

type
  { }
  TCollisionCount = Int64;
  TMailboxTag = Int64;

  { Triangle in VRML/X3D model. This is the most basic item for our
    VRML/X3D collision detection routines, returned by octrees descending from
    TBaseTrianglesOctree. }
  TTriangle = object(T3DTriangle)
  public
    { Initialize new triangle.
      Given APosition must satisfy IsValidTriangle. }
    constructor Init(AShape: TObject;
      const APosition: TTriangle3Single;
      const ANormal: TTriangle3Single; const ATexCoord: TTriangle4Single;
      const AFace: TFaceIndex);

    procedure UpdateWorld;
  public
    { See TTriangleEvent for the meaning of these fields. }
    Shape: TObject;

    {$ifndef CONSERVE_TRIANGLE_MEMORY}
    Normal: TTriangle3Single;
    TexCoord: TTriangle4Single;
    Face: TFaceIndex;
    {$else}
    function Face: TFaceIndex;
    {$endif not CONSERVE_TRIANGLE_MEMORY}

    {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { Tag of an object (like a ray or a line segment)
      for which we have saved an
      intersection result. Intersection result is in
      MailboxIsIntersection, MailboxIntersection, MailboxIntersectionDistance.

      To make things correct, we obviously assume that every segment
      and ray have different tags. Also, tag -1 is reserved.
      In practice, we simply initialize MailboxSavedTag to -1,
      and each new segment/ray get consecutive tags starting from 0.

      @italic(History): a naive implementation at the beginning
      was not using tags, instead I had MailboxState (empty, ray or segment)
      and I was storing ray/line vectors (2 TVector3Single values).
      This had much larger size (6 * SizeOf(Single) + SizeOf(enum) = 28 bytes)
      than tag, which is important (3D models have easily thousands of
      TTriangle). And it took longer to compare and assign,
      so it was working much slower.

      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { State of this shape, containing various information about 3D shape.
      This is a shortcut of TShape(Shape).State. }
    function State: TX3DGraphTraverseState;

    { Check collisions between TTriangle and ray/segment.

      Always use these routines to check for collisions,
      to use mailboxes if possible. Mailboxes are used only if this was
      compiled with TRIANGLE_OCTREE_USE_MAILBOX defined.

      Increments TriangleCollisionTestsCounter if actual test was done
      (that is, if we couldn't use mailbox to get the result quickier).

      @groupBegin }
    function SegmentDirCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Odc0, OdcVector: TVector3Single;
      const SegmentTag: TMailboxTag): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const RayTag: TMailboxTag): boolean;
    { @groupEnd }

    { X3D shape node of this triangle. May be @nil in case of VRML 1.0. }
    function ShapeNode: TAbstractShapeNode;

    { X3D material node of this triangle. May be @nil in case material is not set,
      or in VRML 1.0. }
    function MaterialNode: TMaterialNode;

    { Create material information instance for material of this triangle.
      See TX3DMaterialInfoAbstract for usage description.

      Returns @nil when no Material node is defined, this can happen
      only for VRML >= 2.0.

      Returned TX3DMaterialInfoAbstract is valid only as long as the Material
      node (for VRML 1.0 or 2.0) on which it was based. }
    function MaterialInfo: TX3DMaterialInfoAbstract;

    { Return transparency of this triangle's material.
      Equivalent to MaterialInfo.Transparency, although a little faster. }
    function Transparency: Single;

    { Returns @true for triangles that are transparent. }
    function IsTransparent: boolean;

    { Returns @true for triangles that should be ignored by shadow rays.
      Returns @true for transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE).

      @seealso TBaseTrianglesOctree.IgnoreForShadowRays }
    function IgnoreForShadowRays: boolean;

    {$ifndef CONSERVE_TRIANGLE_MEMORY}

    { For a given position (in world coordinates), return the texture
      coordinate at this point. It is an interpolated texture coordinate
      from our per-vertex texture coordinates in @link(TexCoord) field.

      This assumes that Position actually lies within the triangle.

      The ITexCoord2D returns the same, but cut to the first 2 texture
      coordinate components. Usable for normal 2D textures.
      @groupBegin }
    function ITexCoord(const Point: TVector3Single): TVector4Single;
    function ITexCoord2D(const Point: TVector3Single): TVector2Single;
    { @groupEnd }

    { For a given position (in world coordinates), return the smooth
      normal vector at this point. It is an interpolated normal
      from our per-vertex normals in @link(Normal) field.
      Like them, it is a normal vector in local coordinates.

      This assumes that Position actally lies within the triangle. }
    function INormal(const Point: TVector3Single): TVector3Single;
    {$endif}
  end;
  PTriangle = ^TTriangle;

  TTriangleList = specialize TGenericStructList<TTriangle>;

{ TBaseTrianglesOctree ----------------------------------------------------------- }

type
  { }
  TBaseTrianglesOctree = class;

  { }
  TBaseTrianglesOctreeNode = class(TOctreeNode)
  protected
    { These realize the common implementation of SphereCollision:
      traversing down the octree nodes. They take care of traversing
      down the non-leaf nodes, you only have to override
      the CommonXxxLeaf versions where you handle the leaves
      (and you have to call CommonXxx from normal Xxx routines). }
    function CommonSphere(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function CommonSphere2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonSphere2DLeaf(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function CommonPoint2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonPoint2DLeaf(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function CommonBox(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonBoxLeaf(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function CommonSegment(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function CommonRay(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;
  public
    { See TBaseTrianglesOctree for documentation of these routines.

      Note that methods here do not try to limit detected intersections
      to their boxes. If you will insert a large triangle into a node,
      that is partially inside and partially outside of this node,
      the collision methods may find an intersection outside of this node.

      This is not be a problem for a root node, since the root node has
      a box such that every triangle is completely inside.
      But it is important to remember when you implement recursive
      *Collision calls in nodes: if you want to query your subnodes
      in some particular order (for example to honour ReturnClosestIntersection
      = @true), then remember that one subnode may detect a collision
      that in fact happened in other subnode.

      @groupBegin }
    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function SphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsSphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function PointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsPointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; virtual; abstract;

    function IsRayCollision(
      const RayOrigin, RayDirection: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;
    { @groupEnd }
  end;

  { Callback for @link(TBaseTrianglesOctree.EnumerateTriangles). }
  TEnumerateTriangleFunc = procedure (const Triangle: PTriangle) of object;

  { Abstract class for octrees that can check and return collisions
    with TTriangle.

    Octree node class used by this must be a TBaseTrianglesOctreeNode descendant.

    In a simple case, this is an ancestor of TTriangleOctree,
    that is just an octree storing TTriangle. But it's also an
    ancestor of TShapeOctree, since each shape has also a
    triangle octree. This way, TShapeOctree can calculate collisions
    with TTriangle, even though it doesn't directly store TTriangle items. }
  TBaseTrianglesOctree = class(TOctree)
  private
    { Return NextFreeTag and increment it (for the future AssignNewTag).

      This guarantees that NextFreeTag is incremented immediately,
      so it will not be reused by some other routine. For example
      if your collision query will cause another collision query
      inside, that calls inside another AssignNewTag, everything will work OK. }
    function AssignNewTag: TMailboxTag;
  public
    { Collision checking using the octree.

      SegmentCollision checks for collision between a line segment and tree items.

      SphereCollision checks for collision with a sphere.

      BoxCollision checks for collision with a box (axis-aligned, TBox3D type).

      RayCollision checks for collision with a ray.

      All there methods return nil if there is no collision, or a pointer
      to colliding item.

      @param(ReturnClosestIntersection

        If @false, then any collision detected is returned.
        For routines that don't have ReturnClosestIntersection parameter
        (SphereCollision, BoxCollision) always any collision is returned.

        If this is @true, then the collision closest to RayOrigin (for RayCollision)
        or Pos1 (for SegmentCollision) is returned. This makes the collision
        somewhat slower (as we have to check all collisions, while
        for ReturnClosestIntersection = @false we can terminate at first
        collision found.)

        The versions that return boolean value (IsXxxCollision) don't
        take this parameter, as they are naturally interested in existence
        of @italic(any) intersection.)

      @param(TriangleToIgnore

        If this is non-nil, then Segment/RayCollision assume that there
        is @italic(never) a collision with this octree item.
        It's never returned as collidable item.

        This is useful for recursive ray-tracer, when you start tracing
        from some existing face (octree item). In this case, you don't
        want to "hit" the starting face. So you can pass this face
        as TriangleToIgnore.

        Note that IgnoreMarginAtStart helps with the same problem,
        although a little differently.)

      @param(TrianglesToIgnoreFunc

        If assigned, then items for which TrianglesToIgnoreFunc returns @true
        will be ignored. This is a more general mechanism than
        TriangleToIgnore, as you can ignore many items, you can also
        make some condition to ignore --- for example, you can ignore
        partially transparent items.)

      @param(IgnoreMarginAtStart

        If @true, then collisions that happen very very close to RayOrigin (or Pos1
        for SegmentCollision) will be ignored.

        This is another thing helpful for recursive ray-tracers:
        you don't want to hit the starting face, or any coplanar faces,
        when tracing reflected/refracted/shadow ray.

        Note that if you know actual pointer of your face, it's better to use
        TriangleToIgnore --- TriangleToIgnore is a 100% guaranteed
        stable solution, while IgnoreMarginAtStart necessarily has some
        "epsilon" constant that determines which items are ignored.
        This epsilon may be too large, or too small, in some cases.

        In practice, recursive ray-tracers should use both
        TriangleToIgnore (to avoid collisions with starting face)
        and IgnoreMarginAtStart = @true (to avoid collisions with faces
        coplanar with starting face).)

      @param(IntersectionDistance
        For RayCollision:
        Returned IntersectionDistance is the distance along the RayDirection:
        smaller IntersectionDistance, closer to RayOrigin.
        IntersectionDistance is always >= 0.
        Intersection is always equal to RayOrigin + RayDirection * IntersectionDistance.

        For SegmentCollision: analogously,
        IntersectionDistance is along Pos2 - Pos1.
        IntersectionDistance is always in 0...1.
        Intersectio is always equal to Pos1 + (Pos2 - Pos1) * IntersectionDistance.
      )

      @groupBegin
    }
    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function SegmentCollision(
      out Intersection: TVector3Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function SegmentCollision(
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function SegmentCollision(
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function SphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function IsSphereCollision2D(const pos: TVector2Single;
      const Radius: Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function PointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function IsPointCollision2D(const Point: TVector2Single;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      const RayOrigin, RayDirection: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function RayCollision(
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle; overload;

    function IsRayCollision(
      const RayOrigin, RayDirection: TVector3Single;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
    { @groupEnd }

    { Check is move allowed. This is the perfect (precise, using triangle mesh,
      and fast) implementation of T3D.MoveCollision interface.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. This can be used to allow
      camera to walk thorugh some surfaces (e.g. through water
      surface, or to allow player to walk through some "fake wall"
      and discover secret room in game etc.). }
    function MoveCollision(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TriangleToIgnore: PTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;
    function MoveCollision(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TriangleToIgnore: PTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;
    { @groupEnd }

    { For given camera position and up vector, calculate camera height
      above the ground. This is comfortable for cooperation with
      TWalkCamera.OnHeight.

      See T3D.Height for specification.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. }
    function HeightCollision(
      const Position, GravityUp: TVector3Single;
      out AboveHeight: Single; out AboveGround: PTriangle;
      const TriangleToIgnore: PTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    { Ignore (return @true) transparent triangles
      (with Material.Transparency > 0).

      This is suitable for T3DTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    class function IgnoreTransparentItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;

    { Ignore (return @true) transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE).

      This is suitable for T3DTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    class function IgnoreForShadowRays(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;

    { Checks whether VRML Light (point or directional) lights at scene point
      LightedPoint.

      "Lights at scene" means that the light is turned on
      (field "on" is @true) and between light source and a LightedPoint
      nothing blocks the light (we check it by querying collisions using
      the octree, ignoring transparent and non-shadow-casting triangles),
      and the light source is on the same side of LightedPointPlane as
      RenderDir.

      TriangleToIgnore and IgnoreMarginAtStart work just like for
      SegmentCollision. You should usually set TriangleToIgnore to the
      triangle containing your LightedPoint and IgnoreMarginAtStart to @true,
      to avoid detecting point as shadowing itself. }
    function LightNotBlocked(const Light: TLightInstance;
      const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean): boolean;

    { Enumerate every triangle of this octree.

      It passes to EnumerateTriangleFunc callback a Triangle.
      Triangle is passed as a pointer (never @nil) --- these are guaranteed
      to be "stable" pointers stored inside octrees' lists (so they will be valid
      as long as octree (and eventual children octrees for TShapeOctree)).

      Every triangle is guaranteed to have it's World coordinates updated
      (to put it simply, when this is used on TShapeOctree, then we
      call UpdateWorld on each triangle). }
    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      virtual; abstract;

    { Number of triangles within the octree. This counts all triangles
      returned by EnumerateTriangles. }
    function TrianglesCount: Cardinal; virtual; abstract;
  end;

  { Simple utility class to easily ignore all transparent, non-shadow-casting
    triangles, and, additionally, one chosen triangle.
    Useful for TrianglesToIgnoreFunc parameters of various
    TBaseTrianglesOctree methods. }
  TOctreeIgnoreForShadowRaysAndOneItem = class
  public
    OneItem: PTriangle;
    function IgnoreItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;
    constructor Create(AOneItem: PTriangle);
  end;

var
  { Counter of collision tests done by TTriangle when the actual collision
    calculation had to be done.
    This counts all calls to TTriangle.SegmentDirCollision and
    TTriangle.RayCollision when the result had to be actually geometrically
    calculated (result was not in the cache aka "mailbox").

    It is especially useful to look at this after using some spatial
    data structure, like an octree. The goal of tree structures is to
    minimize this number.

    It is a global variable, because that's the most comfortable way to use
    it. Triangles are usually wrapped in an octree (like TTriangleOctree),
    or even in an octree of octrees (like TShapeOctree).
    Tracking collisions using the global variable is most comfortable,
    instead of spending time on propagating this (purely debugging) information
    through the octree structures. }
  TriangleCollisionTestsCounter: Cardinal;

implementation

uses CastleStringUtils, CastleShapes;

{ TTriangle  ------------------------------------------------------------- }

constructor TTriangle.Init(AShape: TObject;
  const APosition: TTriangle3Single;
  const ANormal: TTriangle3Single; const ATexCoord: TTriangle4Single;
  const AFace: TFaceIndex);
begin
  inherited Init(APosition);

  Shape := AShape;

  {$ifndef CONSERVE_TRIANGLE_MEMORY}
  Normal := ANormal;
  TexCoord := ATexCoord;
  Face := AFace;
  {$endif not CONSERVE_TRIANGLE_MEMORY}

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

function TTriangle.State: TX3DGraphTraverseState;
begin
  Result := TShape(Shape).State;
end;

procedure TTriangle.UpdateWorld;
begin
  World.Triangle := TriangleTransform(Local.Triangle, State.Transform);
  {$ifndef CONSERVE_TRIANGLE_MEMORY_MORE}
  World.Plane := TriangleNormPlane(World.Triangle);
  World.Area := CastleTriangles.TriangleArea(World.Triangle);
  {$endif}
end;

function TTriangle.SegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Odc0, OdcVector: TVector3Single;
  const SegmentTag: TMailboxTag): boolean;
begin
  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = SegmentTag then
  begin
    result := MailboxIsIntersection;
    if result then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    Result := TryTriangleSegmentDirCollision(
      Intersection, IntersectionDistance,
      Local.Triangle, Local.Plane,
      Odc0, OdcVector);
    Inc(TriangleCollisionTestsCounter);

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { save result to mailbox }
    MailboxSavedTag := SegmentTag;
    MailboxIsIntersection := result;
    if result then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TTriangle.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const RayTag: TMailboxTag): boolean;
begin
  { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
    z jednego RayOrigin ale z roznym RayDirection (np. w raytracerze). Wiec lepiej
    najpierw porownywac przechowywane w skrzynce RayDirection (niz RayOrigin)
    zeby moc szybciej stwierdzic niezgodnosc. }
  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = RayTag then
  begin
    result := MailboxIsIntersection;
    if result then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    result := TryTriangleRayCollision(
      Intersection, IntersectionDistance,
      Local.Triangle, Local.Plane,
      RayOrigin, RayDirection);
    Inc(TriangleCollisionTestsCounter);

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { zapisz wyniki do mailboxa }
    MailboxSavedTag := RayTag;
    MailboxIsIntersection := result;
    if result then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TTriangle.ShapeNode: TAbstractShapeNode;
begin
  Result := State.ShapeNode;
end;

function TTriangle.MaterialNode: TMaterialNode;
var
  S: TAbstractShapeNode;
begin
  S := ShapeNode;
  if S <> nil then
    Result := S.Material
  else
    Result := nil;
end;

function TTriangle.MaterialInfo: TX3DMaterialInfoAbstract;
var
  M2: TMaterialNode;
begin
  if State.ShapeNode <> nil then
  begin
    M2 := State.ShapeNode.Material;
    if M2 <> nil then
      Result := M2.MaterialInfo else
      Result := nil;
  end else
    Result := State.LastNodes.Material.MaterialInfo(0);
end;

function TTriangle.Transparency: Single;
var
  M2: TMaterialNode;
begin
  if State.ShapeNode <> nil then
  begin
    M2 := State.ShapeNode.Material;
    if M2 <> nil then
      Result := M2.FdTransparency.Value else
      Result := 0;
  end else
    Result := State.LastNodes.Material.Transparency(0);
end;

function TTriangle.IsTransparent: boolean;
begin
  Result := Transparency > SingleEqualityEpsilon;
end;

function TTriangle.IgnoreForShadowRays: boolean;

  function NonShadowCaster(State: TX3DGraphTraverseState): boolean;
  var
    Shape: TAbstractShapeNode;
  begin
    Shape := State.ShapeNode;
    Result :=
      (Shape <> nil) and
      (Shape.FdAppearance.Value <> nil) and
      (Shape.FdAppearance.Value is TAppearanceNode) and
      (not TAppearanceNode(Shape.FdAppearance.Value).FdShadowCaster.Value);
  end;

begin
  Result := ({ IsTransparent } Transparency > SingleEqualityEpsilon) or
    NonShadowCaster(State);
end;

{$ifndef CONSERVE_TRIANGLE_MEMORY}
function TTriangle.ITexCoord(const Point: TVector3Single): TVector4Single;
var
  B: TVector3Single;
begin
  B := Barycentric(World.Triangle, Point);
  Result := TexCoord[0] * B[0] +
            TexCoord[1] * B[1] +
            TexCoord[2] * B[2];
end;

function TTriangle.ITexCoord2D(const Point: TVector3Single): TVector2Single;
var
  V: TVector4Single;
begin
  V := ITexCoord(Point);
  Move(V, Result, SizeOf(TVector2Single));
end;

function TTriangle.INormal(const Point: TVector3Single): TVector3Single;
var
  B: TVector3Single;
begin
  B := Barycentric(World.Triangle, Point);
  Result := Normal[0] * B[0] +
            Normal[1] * B[1] +
            Normal[2] * B[2];
end;

{$else}
function TTriangle.Face: TFaceIndex;
begin
  Result := UnknownFaceIndex;
end;
{$endif not CONSERVE_TRIANGLE_MEMORY}

{ TBaseTrianglesOctreeNode -----------------------------------------------

  Common* (non-leaf nodes) implementations }

function TBaseTrianglesOctreeNode.CommonSphere(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  SubnodesBox: TBox3D;
  B0, B1, B2: boolean;
begin
  if not IsLeaf then
  begin
    Result := nil;

    { Visit every subnode containing this sphere, and look for collision there.
      TODO: we take box below, as simply bounding box of the sphere,
      so potentially we visit more nodes than necessary. }
    SubnodesBox.Data[0] := VectorSubtract(pos, Vector3Single(Radius, Radius, Radius) );
    SubnodesBox.Data[1] := VectorAdd(     pos, Vector3Single(Radius, Radius, Radius) );

    SubnodesWithBox(SubnodesBox, BoxLo, BoxHi);

    for B0 := BoxLo[0] to BoxHi[0] do
      for B1 := BoxLo[1] to BoxHi[1] do
        for B2 := BoxLo[2] to BoxHi[2] do
        begin
          Result := TBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            CommonSphere(Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonSphereLeaf(Pos, Radius, TriangleToIgnore,
      TrianglesToIgnoreFunc);
  end;
end;

function TBaseTrianglesOctreeNode.CommonSphere2D(const pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  SubnodesBoxMin, SubnodesBoxMax: TVector2Single;
  B0, B1, B2: boolean;
begin
  if not IsLeaf then
  begin
    Result := nil;

    { Visit every subnode containing this sphere, and look for collision there.
      TODO: we take box below, as simply bounding box of the sphere,
      so potentially we visit more nodes than necessary. }
    SubnodesBoxMin := VectorSubtract(pos, Vector2Single(Radius, Radius) );
    SubnodesBoxMax := VectorAdd(     pos, Vector2Single(Radius, Radius) );

    SubnodesWithBox2D(SubnodesBoxMin, SubnodesBoxMax, BoxLo, BoxHi);

    for B0 := BoxLo[0] to BoxHi[0] do
      for B1 := BoxLo[1] to BoxHi[1] do
        for B2 := BoxLo[2] to BoxHi[2] do
        begin
          Result := TBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            CommonSphere2D(Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonSphere2DLeaf(Pos, Radius, TriangleToIgnore,
      TrianglesToIgnoreFunc);
  end;
end;

function TBaseTrianglesOctreeNode.CommonPoint2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  B0, B1, B2: boolean;
begin
  if not IsLeaf then
  begin
    Result := nil;

    SubnodesWithBox2D(Point, Point, BoxLo, BoxHi);

    for B0 := BoxLo[0] to BoxHi[0] do
      for B1 := BoxLo[1] to BoxHi[1] do
        for B2 := BoxLo[2] to BoxHi[2] do
        begin
          Result := TBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            CommonPoint2D(Point, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonPoint2DLeaf(Point, TriangleToIgnore,
      TrianglesToIgnoreFunc);
  end;
end;

function TBaseTrianglesOctreeNode.CommonBox(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
var
  BoxLo, BoxHi: TOctreeSubnodeIndex;
  B0, B1, B2: boolean;
begin
  if not IsLeaf then
  begin
    Result := nil;

    { Visit every subnode containing this box, and look for collision there. }
    SubnodesWithBox(ABox, BoxLo, BoxHi);

    for B0 := BoxLo[0] to BoxHi[0] do
      for B1 := BoxLo[1] to BoxHi[1] do
        for B2 := BoxLo[2] to BoxHi[2] do
        begin
          Result := TBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            BoxCollision(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonBoxLeaf(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
  end;
end;

function TBaseTrianglesOctreeNode.CommonSegment(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
{$define SEGMENT_COLLISION}
{$I x3dtriangles_raysegment_nonleaf.inc}
{$undef SEGMENT_COLLISION}

function TBaseTrianglesOctreeNode.CommonRay(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
{$I x3dtriangles_raysegment_nonleaf.inc}

{ TBaseTrianglesOctree --------------------------------------------------- }

{$define SegmentCollision_CommonParams :=
  const pos1, pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc
}

{$define SegmentCollision_Implementation :=
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).SegmentCollision(
    Intersection, IntersectionDistance,
    Pos1, Pos2,
    AssignNewTag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;}

  function TBaseTrianglesOctree.SegmentCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): PTriangle;
  SegmentCollision_Implementation

  function TBaseTrianglesOctree.SegmentCollision(
    out Intersection: TVector3Single;
    SegmentCollision_CommonParams): PTriangle;
  var
    IntersectionDistance: Single;
  SegmentCollision_Implementation

  function TBaseTrianglesOctree.SegmentCollision(
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): PTriangle;
  var
    Intersection: TVector3Single;
  SegmentCollision_Implementation

  function TBaseTrianglesOctree.SegmentCollision(
    SegmentCollision_CommonParams): PTriangle;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  SegmentCollision_Implementation

{$undef SegmentCollision_CommonParams}
{$undef SegmentCollision_Implementation}

function TBaseTrianglesOctree.IsSegmentCollision(
  const pos1, pos2: TVector3Single;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsSegmentCollision(
    Pos1, Pos2,
    AssignNewTag,
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).SphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.IsSphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsSphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.SphereCollision2D(const pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).SphereCollision2D(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.IsSphereCollision2D(const pos: TVector2Single;
  const Radius: Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsSphereCollision2D(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.PointCollision2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).PointCollision2D(
    Point, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.IsPointCollision2D(const Point: TVector2Single;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsPointCollision2D(
    Point, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).BoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TBaseTrianglesOctree.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsBoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

{$define RayCollision_CommonParams :=
  const RayOrigin, RayDirection: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc
}

{$define RayCollision_Implementation :=
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).RayCollision(
    Intersection, IntersectionDistance,
    RayOrigin, RayDirection,
    AssignNewTag,
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;}

  function TBaseTrianglesOctree.RayCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    RayCollision_CommonParams): PTriangle;
  RayCollision_Implementation

  function TBaseTrianglesOctree.RayCollision(
    out Intersection: TVector3Single;
    RayCollision_CommonParams): PTriangle;
  var
    IntersectionDistance: Single;
  RayCollision_Implementation

  function TBaseTrianglesOctree.RayCollision(
    out IntersectionDistance: Single;
    RayCollision_CommonParams): PTriangle;
  var
    Intersection: TVector3Single;
  RayCollision_Implementation

  function TBaseTrianglesOctree.RayCollision(
    RayCollision_CommonParams): PTriangle;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  RayCollision_Implementation

{$undef RayCollision_CommonParams}
{$undef RayCollision_Implementation}

function TBaseTrianglesOctree.IsRayCollision(
  const RayOrigin, RayDirection: TVector3Single;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TBaseTrianglesOctreeNode(InternalTreeRoot).IsRayCollision(
    RayOrigin, RayDirection,
    AssignNewTag,
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

{ XxxCollision methods ------------------------------------------------------- }

function TBaseTrianglesOctree.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if IsRadius then
    Result :=
      (not IsSegmentCollision(OldPos, NewPos,
        TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
      (not IsSphereCollision(NewPos, Radius,
        TriangleToIgnore, TrianglesToIgnoreFunc)) else
    Result :=
      (not IsSegmentCollision(OldPos, NewPos,
        TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
      (not IsBoxCollision(NewBox,
        TriangleToIgnore, TrianglesToIgnoreFunc));
end;

function TBaseTrianglesOctree.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

  { $define DEBUG_WALL_SLIDING}

  const
    { For wall-sliding inside MoveAlongTheBlocker implementations,
      we want to position ourselves slightly farther away than
      Radius. (Exactly on Radius would mean that it's
      sensitive to floating point imprecision, and sometimes the sphere
      could be considered colliding with Blocker anyway, instead
      of sliding along it. And final MoveCollision (without wall-sliding) call
      will then fail, making wall-sliding non-working.)

      So this must be something slightly larger than 1.
      And obviously must be close to 1
      (otherwise NewPos will not be sensible). }
    RadiusEnlarge = 1.01;

  { This is the worse version of wall-sliding:
    we don't know the 3D point of intersection with blocker,
    which means we can't really calculate a vector to make
    proper wall-sliding. We do some tricks to still perform wall-sliding
    in many positions, but it's not perfect. }
  function MoveAlongTheBlocker(Blocker: PTriangle): boolean;
  var
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    Plane: TVector4Single;
    {$endif}
    PlanePtr: PVector4Single;
    PlaneNormalPtr: PVector3Single;
    NewPosShift: TVector3Single;
  begin
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    Plane := Blocker^.World.Plane;
    PlanePtr := @Plane;
    {$else}
    PlanePtr := @(Blocker^.World.Plane);
    {$endif}
    PlaneNormalPtr := PVector3Single(PlanePtr);

    { project ProposedNewPos on a plane of blocking object }
    NewPos := PointOnPlaneClosestToPoint(PlanePtr^, ProposedNewPos);

    { now NewPos must be on the same plane side as OldPos is,
      and it must be at the distance slightly larger than Radius from the plane }
    if VectorsSamePlaneDirections(PlaneNormalPtr^,
         VectorSubtract(ProposedNewPos, NewPos), PlanePtr^) then
      NewPosShift := VectorScale(PlaneNormalPtr^,  Radius * RadiusEnlarge) else
      NewPosShift := VectorScale(PlaneNormalPtr^, -Radius * RadiusEnlarge);
    VectorAddVar(NewPos, NewPosShift);

    { Even though I calculated NewPos so that it's not blocked by object
      Blocker, I must check whether it's not blocked by something else
      (e.g. if player is trying to walk into the corner (two walls)).
      I can do it by using my simple MoveCollision. }

    Result := MoveCollision(OldPos, NewPos, IsRadius, Radius, OldBox, NewBox,
      TriangleToIgnore, TrianglesToIgnoreFunc);

    {$ifdef DEBUG_WALL_SLIDING}
    Writeln('Wall-sliding: WORSE version without 3d intersection. Blocker ', PointerToStr(Blocker), '.');
    {$endif}
  end;

  { The better wall-sliding implementation, that can calculate
    nice vector along which to slide.

    It requires as input BlockerIntersection, this is the 3D point
    of intersection between player move line (from OldPos to ProposedNewPos)
    and the Blocker.World.Plane.

    SegmentCollision says whether segment OldPos->ProposedNewPos was detected
    as colliding with Blocker.World.Plane (IOW, ProposedNewPos is on the other
    side of the blocker plane) or not (IOW, ProposedNewPos is on the same
    side of the blocker plane). }
  function MoveAlongTheBlocker(
    const BlockerIntersection: TVector3Single;
    SegmentCollision: boolean;
    Blocker: PTriangle): boolean;
  var
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    Plane: TVector4Single;
    {$endif}
    PlanePtr: PVector4Single;
    Slide, Projected: TVector3Single;
    NewBlocker: PTriangle;
    NewBlockerIntersection: TVector3Single;
  begin
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    Plane := Blocker^.World.Plane;
    PlanePtr := @Plane;
    {$else}
    PlanePtr := @(Blocker^.World.Plane);
    {$endif}

    {$ifdef DEBUG_WALL_SLIDING}
    Write('Wall-sliding: Better version (with 3d intersection). ');
    if SegmentCollision then
      Write('Segment collided. ') else
      Write('Sphere collided . ');
    Writeln('Blocker ', PointerToStr(Blocker), '.');
    {$endif}

    { Project ProposedNewPos or OldPos on Blocker plane.
      The idea is that knowing this projection, and knowing BlockerIntersection,
      we can calculate Slide (= vector that will move us parallel to
      Blocker plane).

      We could always project ProposedNewPos. But for
      SegmentCollision = @false, OldPos is also good to use,
      and it's farther from BlockerIntersection than ProposedNewPos
      --- this is good, as we want Slide vector to be long, to avoid
      floating point imprecision when Slide is very very short vector. }
    if SegmentCollision then
    begin
      Projected := PointOnPlaneClosestToPoint(PlanePtr^, ProposedNewPos);
      Slide := VectorSubtract(Projected, BlockerIntersection);
    end else
    begin
      Projected := PointOnPlaneClosestToPoint(PlanePtr^, OldPos);
      Slide := VectorSubtract(BlockerIntersection, Projected);
    end;

    if not ZeroVector(Slide) then
    begin
      { Move by Slide.

        Length of Slide is taken from the distance between
        OldPos and ProposedNewPos. This is Ok, as we do not try to
        make perfect wall-sliding (that would first move as close to Blocker
        plane as possible, and then move along the blocker).
        Instead we move all the way along the blocker. This is in practice Ok. }

      VectorAdjustToLengthVar(Slide, PointsDistance(OldPos, ProposedNewPos));

      NewPos := VectorAdd(OldPos, Slide);

      { Even though I calculated NewPos so that it's not blocked by object
        Blocker, I must check whether it's not blocked by something else
        (e.g. if player is trying to walk into the corner (two walls)).
        I can do it by using my simple MoveCollision. }

      Result := MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TriangleToIgnore, TrianglesToIgnoreFunc);

      {$ifdef DEBUG_WALL_SLIDING} Writeln('Wall-sliding: Final check of sliding result: ', Result); {$endif}

      if (not Result) and (not SegmentCollision) then
      begin
        { When going through corners, previous code will not necessarily make
          good wall-sliding, because our Blocker may be taken from sphere
          collision. So it's not really a good plane to slide along.
          Let's try harder to to get a better blocker: use RayCollision
          in the previous Slide direction,
          and check is result still within our sphere.

          We preserve below the old value of Blocker (have our own NewBlocker
          and NewBlockerIntersection), but the rest of variables may be
          mercilessly overriden by code below:
          PlanePtr, Projected, Slide helpers.

          Check that it works: e.g. test beginning of castle_hall_final.wrl,
          new_acts.wrl. }

        NewBlocker := RayCollision(
          OldPos, Slide, true { return closest blocker },
          TriangleToIgnore, false, TrianglesToIgnoreFunc);

        if (NewBlocker <> nil) and
           (NewBlocker <> Blocker) and
           IsTriangleSphereCollision(
             NewBlocker^.World.Triangle,
             NewBlocker^.World.Plane,
             ProposedNewPos,
             { NewBlocker is accepted more generously, within 2 * normal radius. }
             Radius * 2) and
           TryPlaneLineIntersection(NewBlockerIntersection,
             NewBlocker^.World.Plane,
             OldPos, VectorSubtract(ProposedNewPos, OldPos)) then
        begin
          {$ifdef DEBUG_WALL_SLIDING} Writeln('Wall-sliding: Better blocker found: ', PointerToStr(NewBlocker), '.'); {$endif}

          { Below we essentially make the wall-sliding computation again.
            We know that we're in sphere collision case
            (checked above that "not SegmentCollision"). }

          {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
          Plane := NewBlocker^.World.Plane;
          PlanePtr := @Plane;
          {$else}
          PlanePtr := @(NewBlocker^.World.Plane);
          {$endif}
          Projected := PointOnPlaneClosestToPoint(PlanePtr^, OldPos);
          Slide := VectorSubtract(NewBlockerIntersection, Projected);

          if not ZeroVector(Slide) then
          begin
            VectorAdjustToLengthVar(Slide, PointsDistance(OldPos, ProposedNewPos));
            NewPos := VectorAdd(OldPos, Slide);
            Result := MoveCollision(OldPos, NewPos,
              IsRadius, Radius, OldBox, NewBox, TriangleToIgnore, TrianglesToIgnoreFunc);

            {$ifdef DEBUG_WALL_SLIDING} Writeln('Wall-sliding: Better blocker final check of sliding result: ', Result); {$endif}
          end;
        end else
        if NewBlocker <> nil then
        begin
          {$ifdef DEBUG_WALL_SLIDING}
          Writeln('Wall-sliding: Better blocker NOT found: ', PointerToStr(NewBlocker), ' ',
            IsTriangleSphereCollision(
              NewBlocker^.World.Triangle,
              NewBlocker^.World.Plane,
              ProposedNewPos, Radius), ' ',
            TryPlaneLineIntersection(NewBlockerIntersection,
              NewBlocker^.World.Plane,
              OldPos, VectorSubtract(ProposedNewPos, OldPos)), '.');
          {$endif}
        end;
      end;
    end else
    begin
      { Fallback to worse wall-sliding version. }
      {$ifdef DEBUG_WALL_SLIDING} Writeln('Wall-sliding: Need to fallback to worse version (Slide = 0)'); {$endif}
      Result := MoveAlongTheBlocker(Blocker);
    end;
  end;

var
  Blocker: PTriangle;
  BlockerIntersection: TVector3Single;
begin
  if not IsRadius then
  begin
    { for IsRadius = false, for now just fallback to simple yes-no check,
      without wall-sliding. We can improve this one day to make wall-sliding
      even in this case (NewBox in this case will be shifted
      like ProposedNewPos->NewPos). }
    Result := MoveCollision(OldPos, ProposedNewPos,
      IsRadius, Radius, OldBox, NewBox, TriangleToIgnore, TrianglesToIgnoreFunc);
    NewPos := ProposedNewPos;
    Exit;
  end;

  Blocker := SegmentCollision(
    BlockerIntersection, OldPos, ProposedNewPos,
    true { return closest blocker },
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  if Blocker = nil then
  begin
    Blocker := SphereCollision(ProposedNewPos, Radius,
      TriangleToIgnore, TrianglesToIgnoreFunc);
    if Blocker = nil then
    begin
      Result := true;
      NewPos := ProposedNewPos;
    end else
    if TryPlaneLineIntersection(BlockerIntersection, Blocker^.World.Plane,
      OldPos, VectorSubtract(ProposedNewPos, OldPos)) then
      Result := MoveAlongTheBlocker(BlockerIntersection, false, Blocker) else
      Result := MoveAlongTheBlocker(Blocker);
  end else
    Result := MoveAlongTheBlocker(BlockerIntersection, true, Blocker);
end;

function TBaseTrianglesOctree.HeightCollision(
  const Position, GravityUp: TVector3Single;
  out AboveHeight: Single; out AboveGround: PTriangle;
  const TriangleToIgnore: PTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  AboveGround := RayCollision(AboveHeight, Position, VectorNegate(GravityUp),
    true, TriangleToIgnore, false, TrianglesToIgnoreFunc);
  Result := AboveGround <> nil;
  if not Result then
    AboveHeight := MaxSingle;
end;

{ Other TBaseTrianglesOctree utils ----------------------------------------------- }

var
  { Next tag that will be allocated for ray/segment and such by AssignNewTag.
    Can be read/written only by AssignNewTag.

    That's right, this is a global variable. Reason: octree instances
    are sometimes freed / recreated (consider e.g. TShapeOctree
    recreated when Trasform changes). New octree may want to immediately
    do some collision checks. However, records about tags from old octree
    may still be remembered somewhere -- for example, TShape
    remembers MailboxSavedTag (given for Shape.RayCollision from
    TShapeOctreeNode.CommonRayLeaf). So, if this would be a field
    of TBaseTrianglesOctree, then the newly created octree could
    create the same tag, and hit the mailbox mechanism of already existing
    shape.

    This actually happened with SphereSensor tests,
    e.g. on unison.x3dv from
    http://www.web3d.org/x3d/content/examples/Conformance/Sensors/SphereSensor/index.html .
    When you rotate the box, it's Transform.rotation changed, causing
    a rebuild of shapes octree. (But actual TShape and it's local triangles
    octree stay unmodified.) Without moving NextFreeTag to global variable,
    the new created octree would have NextFreeTag that was already recorded.
    In effect, @link(Height) (for camera gravity) were returning a result
    for previous mouse ray pick, temporary showing our "height above the ground"
    even though we were not standing on the ground. }
  NextFreeTag: TMailboxTag;

function TBaseTrianglesOctree.AssignNewTag: TMailboxTag;
begin
 result := NextFreeTag;
 Inc(NextFreeTag);
end;

class function TBaseTrianglesOctree.IgnoreTransparentItem(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := PTriangle(Triangle)^.IsTransparent;
end;

class function TBaseTrianglesOctree.IgnoreForShadowRays(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := PTriangle(Triangle)^.IgnoreForShadowRays;
end;

function TBaseTrianglesOctree.LightNotBlocked(const Light: TLightInstance;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean): boolean;
var LightPos: TVector3Single;
begin
 if not Light.Node.FdOn.Value then result := false;

 if Light.Node is TAbstractDirectionalLightNode then
  { Swiatlo directional oznacza ze swiatlo polozone jest tak bardzo
    daleko ze wszystkie promienie od swiatla sa rownolegle.

    Od pozycji LightedPoint odejmujemy wydluzone Direction swiatla.

    3 * Box3DMaxSize(Octree.TreeRoot.Box) na pewno jest odlegloscia
    ktora sprawi ze LightPos bedzie poza Octree.TreeRoot.Box
    (bo gdyby nawet Octree.TreeRoot.Box byl szescianem to jego przekatna
    ma dlugosc tylko Sqrt(2) * Sqrt(2) * Box3DMaxSize(Octree.TreeRoot.Box)
    (= 2 * Box3DMaxSize(Octree.TreeRoot.Box))
    W ten sposob otrzymujemy punkt ktory na pewno lezy POZA TreeRoot.Box
    i jezeli nic nie zaslania drogi od Point do tego punktu to
    znaczy ze swiatlo oswietla Intersection. }
  LightPos := VectorSubtract(LightedPoint,
    VectorAdjustToLength(Light.Direction,
      3 * InternalTreeRoot.Box.MaxSize ) ) else
  LightPos := Light.Location;

 Result := (VectorsSamePlaneDirections(
       VectorSubtract(LightPos, LightedPoint),
       RenderDir,
       LightedPointPlane)) and
   (SegmentCollision(LightedPoint, LightPos,
     false, TriangleToIgnore, IgnoreMarginAtStart, @IgnoreForShadowRays)
     = nil);
end;

{ TOctreeIgnoreForShadowRaysAndOneItem -------------------------------------- }

function TOctreeIgnoreForShadowRaysAndOneItem.IgnoreItem(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := (Triangle = P3DTriangle(OneItem)) or
    PTriangle(Triangle)^.IgnoreForShadowRays;
end;

constructor TOctreeIgnoreForShadowRaysAndOneItem.Create(
  AOneItem: PTriangle);
begin
  inherited Create;
  OneItem := AOneItem;
end;

end.
