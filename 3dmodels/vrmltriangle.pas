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

{ VRML triangles (TVRMLTriangle) and abstract class for octrees
  that resolve collision to triangles (TVRMLBaseTrianglesOctree). }
unit VRMLTriangle;

{$I vrmloctreeconf.inc}

interface

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3d, Math,
  KambiOctree;

{$define read_interface}

{ TVRMLTriangle  ------------------------------------------------------------ }

type
  { }
  TVRMLTriangleMailboxState = (msEmpty, msRay, msSegmentDir);
  TCollisionCount = Int64;

  TVRMLTriangleGeometry = record
    Triangle: TTriangle3Single;

    { Area of the triangle. In other words, just a precalculated for you
      TriangleArea(Triangle). }
    Area: Single;

    case Integer of
      0: ({ This is a calculated TriangleNormPlane(Triangle),
            that is a 3D plane containing our Triangle, with normalized
            direction vector. }
          Plane: TVector4Single;);
      1: (Normal: TVector3Single;);
  end;

  { Triangle of VRML model. This is the most basic item for our
    collision detection routines, returned by octrees descending from
    TVRMLBaseTrianglesOctree.

    This object should always be initialized by @link(Init),
    and updated only by it's methods (never modify fields of
    this object directly).

    I use old-style Pascal "object" to define this,
    since this makes it a little more efficient. This doesn't need
    any virtual methods or such, so (at least for now) it's easier
    and more memory-efficient to keep this as an old-style object.
    And memory efficiency is somewhat important here, since large
    scenes may easily have milions of triangles, and each triangle
    results in one TVRMLTriangle instance. }
  TVRMLTriangle = object
  public
    { Initialize new TVRMLTriangle. Given Triangle must satisfy IsValidTriangle. }
    constructor Init(const ATriangle: TTriangle3Single;
      AState: TVRMLGraphTraverseState; AGeometryNode: TVRMLGeometryNode;
      const AMatNum, AFaceCoordIndexBegin, AFaceCoordIndexEnd: integer);

    { Geometry of this item.
      We need two geometry descriptions:

      @unorderedList(

        @item(Local is based on initial Triangle, given when constructing
          this TVRMLTriangle. It's constant for this TVRMLTriangle. It's used
          by octree collision routines, that is things like
          TVRMLBaseTrianglesOctree.SphereCollision, TVRMLBaseTrianglesOctree.RayCollision
          and such expect parameters in the same coord space.

          This may be local coord space of this shape (this is used
          by TVRMLShape.OctreeTriangles) or world coord space
          (this is used by TVRMLScene.OctreeTriangles).)

        @item(World is the geometry of Local transformed to be in world
          coordinates. Initially, World is just a copy of Local.

          If Local already contains world-space geometry, then World
          can just remain constant, and so is always Local copy.

          If Local ontains local shape-space geometry, then World
          will have to be updated by UpdateWorld whenever some octree item's
          geometry will be needed in world coords. This will have to be
          done e.g. by TVRMLBaseTrianglesOctree.XxxCollision for each returned item.)
      ) }
    Loc, World: TVRMLTriangleGeometry;
    procedure UpdateWorld;

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
      in constructor, and each new segment/ray get consecutive tags
      starting from 0.

      @groupBegin }
    MailboxSavedTag: Int64;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { Check collisions between TVRMLTriangle and ray/segment.

      Always use these routines to check for collisions,
      to use mailboxes if possible. Mailboxes are used only if this was
      compiled with OCTREE_ITEM_USE_MAILBOX defined.

      Increments DirectCollisionTestsCounter if actual test was done
      (that is, if we couldn't use mailbox to get the result quickier).

      @groupBegin }
    function SegmentDirCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Odc0, OdcVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
      var DirectCollisionTestsCounter: TCollisionCount): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
      var DirectCollisionTestsCounter: TCollisionCount): boolean;
    { @groupEnd }
  end;
  PVRMLTriangle = ^TVRMLTriangle;

  TDynArrayItem_1 = TVRMLTriangle;
  PDynArrayItem_1 = PVRMLTriangle;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynVRMLTriangleArray = TDynArray_1;

{ TVRMLBaseTrianglesOctree ----------------------------------------------------------- }

type
  { }
  TVRMLBaseTrianglesOctree = class;

  { Return for given Triangle do we want to ignore collisions with it. }
  TVRMLTriangleIgnoreFunc = function (
    { Actually, this Octree is always TVRMLTriangleOctree, but this cannot
      be declared at this point. }
    const Octree: TVRMLBaseTrianglesOctree;
    const Triangle: PVRMLTriangle): boolean of object;

  { }
  TVRMLBaseTrianglesOctreeNode = class(TOctreeNode)
  protected
    { These realize the common implementation of SphereCollision:
      traversing down the octree nodes. They take care of traversing
      down the non-leaf nodes, you only have to override
      the CommonXxxLeaf versions where you handle the leaves
      (and you have to call CommonXxx from normal Xxx routines). }
    function CommonSphere(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonBox(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function CommonBoxLeaf(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonSegment(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonRay(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;
  public
    { See TVRMLBaseTrianglesOctree for documentation of these routines.

      Dodatkowo nalezy tu dodac ze na skutek tego ze duze trojkaty moge znalezc
      sie w wielu subnode'ach naraz powinienes wiedziec ze ponizsze procedury
      badaja kolizje ze wszystkimi elementami ktore maja w sobie zapamietane,
      nie obcinajac tych elementow do swoich Box'ow.

      W rezultacie np. RayCollision moze wykryc kolizje promienia z trojkatem
      taka ze Intersection nie lezy w Box - z tego prostego powodu ze
      trojkat akurat "wystawal" z Box'a i wlasnie ta wystajaca czesc
      trojkata trafil promien.

      Zazwyczaj nie jest to problem, zwlaszcza nie jest to problem gdy wywolujesz
      po prostu *Collision z glownego TreeRoot node'a, bo jego Box jest tak
      ustawiany zeby zawsze objac w pelni wszystkie elementy drzewa (nic nie
      bedzie wystawac poza TreeRoot). Ale nalezy to wziac pod uwage robiac
      rekurencyjne wywolania w implementacji *Collision dla nie-lisci:
      tam trzeba uwzglednic fakt ze np. jezeli przegladasz subnode'y
      w jakiejs kolejnosci (jak np. w RayCollision gdzie przegladamy node'y
      w kolejnosci ktora ma nam zapewnic poprawna implementacje
      ReturnClosestIntersection) to musisz uwazac zeby jakis subnode nie wykryl
      przypadkiem kolizji ktora de facto zdarzyla sie w innym subnodzie.

      @groupBegin }
    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual; abstract;

    function BoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsBoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual; abstract;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual; abstract;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual; abstract;
    { @groupEnd }
  end;

  { Abstract class for octrees that can check and return collisions
    with TVRMLTriangle.

    Octree node class used by this must be a TVRMLBaseTrianglesOctreeNode descendant.

    In a simple case, this is an ancestor of TVRMLTriangleOctree,
    that is just an octree storing TVRMLTriangle. But it's also an
    ancestor of TVRMLShapeOctree, since each shape has also a
    triangle octree. This way, TVRMLShapeOctree can calculate collisions
    with TVRMLTriangle, even though it doesn't directly store TVRMLTriangle items. }
  TVRMLBaseTrianglesOctree = class(TOctree)
  private
    {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { nastepny wolny tag ktory przydzielimy nastepnemu promieniowi lub
      odcinkowi z ktorym bedziemy chcieli robic test na kolizje z octree.
      Ta zmienna moze byc czytana/pisana tylko przez AssignNewRayOdcTag. }
    NextFreeRayOdcTag: Int64;

    { zwroci NextFreeRayOdcTag i zrobi Inc(NextFreeRayOdcTag).
      Uzywaj tego aby przydzielic nowy tag. Uzywanie tej funkcji przy okazji
      zapobiega potencjalnie blednej sytuacji :
        TreeRoot.SegmentColision(..., NextFreeRayOdcTag, ...)
        Inc(NextFreeRayOdcTag);
      Powyzszy kod bedzie ZAZWYCZAJ dzialal - ale spowoduje on ze nie bedzie
      mozna uzywac Segment/RayCollision na tym samym TVRMLTriangleOctree gdy juz
      jestesmy w trakcie badania kolizji. Np. callbacki w rodzaju
      TVRMLTriangleIgnoreFunc nie beda mogly wywolywac kolizji. Innymi slowy,
      taki zapis uczynilby Segment/RayCollision non-reentrant. A to na dluzsza
      mete zawsze jest klopotliwe. Natomiast robienie Inc(NextFreeRayOdcTag);
      przed faktycznym wejsciem do funkcji TreeRoot.SegmentColision
      usuwa ten blad. Uzywajac funkcji AssignNewRayOdcTag automatycznie
      to robimy. }
    function AssignNewRayOdcTag: Int64;
    {$endif OCTREE_ITEM_USE_MAILBOX}
  public
    { Collision checking using the octree.

      SegmentCollision checks for collision between a line segment and tree items.

      SphereCollision checks for collision with a sphere.

      BoxCollision checks for collision with a box (axis-aligned, TBox3d type).

      RayCollision checks for collision with a ray.

      All there methods return nil if there is no collision, or a pointer
      to colliding item.

      @param(ReturnClosestIntersection

        If @false, then any collision detected is returned.
        For routines that don't have ReturnClosestIntersection parameter
        (SphereCollision, BoxCollision) always any collision is returned.

        If this is @true, then the collision closest to Ray0 (for RayCollision)
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

        If @true, then collisions that happen very very close to Ray0 (or Pos1
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
        Returned IntersectionDistance is the distance along the RayVector:
        smaller IntersectionDistance, closer to Ray0.
        IntersectionDistance is always >= 0.
        Intersection is always equal to Ray0 + RayVector * IntersectionDistance.

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
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      out Intersection: TVector3Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;

    function BoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

    function IsBoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; overload;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
    { @groupEnd }

    { This checks if move between OldPos and ProposedNewPos is possible,
      by checking is segment between OldPos and ProposedNewPos free
      and sphere (with radius CameraRadius) ProposedNewPos is free.

      CameraRadius must obviously be > 0.

      See @link(MoveAllowed) for some more sophisticated way of
      collision detection.

      If KeepWithinRootBox then it will additionally make sure that
      user stays within the whole octree box. That is, moving outside
      of the RootNode.Box will be disallowed.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. This can be used to allow
      camera to walk thorugh some surfaces (e.g. through water
      surface, or to allow player to walk through some "fake wall"
      and discover secret room in game etc.).

      @seealso(TWalkNavigator.DoMoveAllowed
        TWalkNavigator.DoMoveAllowed is some place
        where you can use this function) }
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const KeepWithinRootBox: boolean;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc = nil): boolean;

    { This is like @link(MoveAllowedSimple), but it checks for collision
      around ProposedNewPos using TBox3d instead of a sphere. }
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const KeepWithinRootBox: boolean;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc = nil): boolean;

    { This is like @link(MoveAllowedSimple), but in some cases
      where MoveAllowedSimple would answer "false", this will
      answer "true" and will set NewPos to some other position
      (close to ProposedNewPos) that user is allowed to move into.
      This is used to allow user who is trying to walk "into the wall"
      to "move alongside the wall" (instead of just completely blocking
      his move, like @link(MoveAllowedSimple) would do).

      Always when MoveAllowedSimple would return true, this will also
      answer true and set NewPos to ProposedNewPos.

      CameraRadius must obviously be > 0.

      Note that it sometimes modifies NewPos even when it returns false.
      Such modification has no meaning to you.
      So you should not assume that NewPos is not modified when it returns
      with false. You should assume that when it returns false,
      NewPos is undefined (especiall since NewPos is "out" parameter
      and it may be implicitly modified anyway).

      If KeepWithinRootBox then it will additionally make sure that
      user stays within the whole octree box. That is, moving outside
      of the RootNode.Box will be disallowed.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision.

      @seealso(TWalkNavigator.DoMoveAllowed
        TWalkNavigator.DoMoveAllowed is some place
        where you can use this function) }
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const CameraRadius: Single;
      const KeepWithinRootBox: boolean;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc = nil): boolean;

    { For given camera position and up vector, calculate camera height
      above the ground. This is comfortable for cooperation with
      TWalkNavigator.OnGetCameraHeight.

      This simply checks collision of a ray from
      CameraPos in direction -GravityUp, and sets IsAboveTheGround
      and SqrHeightAboveTheGround as needed.

      Also GroundItemIndex is set to index of octree item immediately
      below the camera (if IsAboveTheGround). This can be handy to detect
      e.g. that player walks on hot lava and he should be wounded,
      or that he walks on concrete/grass ground (to set his footsteps
      sound accordingly). If IsAboveTheGround then for sure GroundItem
      <> nil.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. }
    procedure GetCameraHeight(
      const CameraPos, GravityUp: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc);

    { This is just like GetCameraHeight, but it assumes that
      GravityUp = (0, 0, 1) and it returns the actual
      HeightAboveTheGround (not it's square). Thanks to the fact that
      calculating HeightAboveTheGround doesn't require costly Sqrt operation
      in case of such simple GravityUp. }
    procedure GetCameraHeightZ(
      const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc);

    { This makes transparent triangles (with material
      with Transparency > 0) ignored (i.e. returns @true for them).
      This is a prepared function compatible with TVRMLTriangleIgnoreFunc
      function type. }
    class function IgnoreTransparentItem(
      const Octree: TVRMLBaseTrianglesOctree;
      const Triangle: PVRMLTriangle): boolean;

    { Checks whether VRML Light (point or directional) lights at scene point
      LightedPoint.

      "Dociera do punktu" to znaczy
      1) Light.LightNode jest ON (FdOn.Value = true) (ten check jest zrobiony
         na samym poczatku bo moze przeciez zaoszczedzic sporo czasu)
      2) ze droga pomiedzy Light a LightedPoint jest wolna w Octree
         (za wyjatkiem obiektow pol-przezroczystych ktore sa po prostu ignorowane
         - TODO: to jest uproszczenie, ale w tym momencie te elementy po prostu
         w ogole nie blokuja swiatla)
      3) oraz ze swiatlo jest po tej samej stronie LightedPointPlane co RenderDir.

      Szukanie kolizji w octree uzywa przekazanych TriangleToIgnore i
      IgnoreMarginAtStart - zazwyczaj powinienes je przekazac na element
      w drzewie z ktorego wziales LightedPoint i na true, odpowiednio.

      Jezeli ta funkcja zwroci true to zazwyczaj pozostaje ci obliczenie
      wplywu swiatla na dany punkt z lokalnych rownan oswietlenia (przy czym
      mozesz juz pominac sprawdzanie LightNode.FdOn - chociaz zazwyczaj
      lepiej bedzie nie pomijac, powtorzenie takiego prostego checku nie
      powoduje przeciez zbytniego marnotrawstwa czasu a kod moze wydawac
      sie bardziej spojny w ten sposob).
    }
    function ActiveLightNotBlocked(const Light: TActiveLight;
      const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean): boolean;
  end;

  { Simple utility class to easily ignore all transparent items and,
    additionally, one chosen item.
    Useful for TrianglesToIgnoreFunc parameters of various TVRMLBaseTrianglesOctree
    methods. }
  TVRMLOctreeIgnoreTransparentAndOneItem = class
    OneItem: PVRMLTriangle;
    { Returns @true for partially transparent items (Transparency > 0),
      and for OneItem. }
    function IgnoreItem(
      const Octree: TVRMLBaseTrianglesOctree;
      const Triangle: PVRMLTriangle): boolean;
    constructor Create(AOneItem: PVRMLTriangle);
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

{$I kambioctreemacros.inc}

{ TVRMLTriangle  ------------------------------------------------------------ }

constructor TVRMLTriangle.Init(const ATriangle: TTriangle3Single;
  AState: TVRMLGraphTraverseState; AGeometryNode: TVRMLGeometryNode;
  const AMatNum, AFaceCoordIndexBegin, AFaceCoordIndexEnd: Integer);
begin
  Loc.Triangle := ATriangle;
  Loc.Plane := TriangleNormPlane(ATriangle);
  Loc.Area := TriangleArea(ATriangle);

  World := Loc;

  State := AState;
  GeometryNode := AGeometryNode;
  MatNum := AMatNum;
  FaceCoordIndexBegin := AFaceCoordIndexBegin;
  FaceCoordIndexEnd := AFaceCoordIndexEnd;

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

procedure TVRMLTriangle.UpdateWorld;
begin
  World.Triangle := TriangleTransform(Loc.Triangle, State.Transform);
  World.Plane := TriangleNormPlane(World.Triangle);
  World.Area := VectorMath.TriangleArea(World.Triangle);
end;

function TVRMLTriangle.SegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Odc0, OdcVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
  {$ifdef OCTREE_ITEM_USE_MAILBOX}
  if MailboxSavedTag = OdcTag then
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
      Loc.Triangle, Loc.Plane,
      Odc0, OdcVector);
    Inc(DirectCollisionTestsCounter);

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { save result to mailbox }
    MailboxSavedTag := OdcTag;
    MailboxIsIntersection := result;
    if result then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TVRMLTriangle.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
  { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
    z jednego Ray0 ale z roznym RayVector (np. w raytracerze). Wiec lepiej
    najpierw porownywac przechowywane w skrzynce RayVector (niz Ray0)
    zeby moc szybciej stwierdzic niezgodnosc. }
  {$ifdef OCTREE_ITEM_USE_MAILBOX}
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
      Loc.Triangle, Loc.Plane,
      Ray0, RayVector);
    Inc(DirectCollisionTestsCounter);

  {$ifdef OCTREE_ITEM_USE_MAILBOX}
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

{ TVRMLBaseTrianglesOctreeNode -----------------------------------------------

  Common* (non-leaf nodes) implementations }

function TVRMLBaseTrianglesOctreeNode.CommonSphere(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    result := TVRMLBaseTrianglesOctreeNode(subnode).CommonSphere(
      pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
    Stop := result <> nil;
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
begin
  if not IsLeaf then
  begin
    { TODO: traktujemy tu sfere jako szescian a wiec byc moze wejdziemy w wiecej
      SubNode'ow niz rzeczywiscie musimy. }
    result := nil;
    OSIS_Box[0] := VectorSubtract(pos, Vector3Single(Radius, Radius, Radius) );
    OSIS_Box[1] := VectorAdd(     pos, Vector3Single(Radius, Radius, Radius) );
    OCTREE_STEP_INTO_SUBNODES
  end else
  begin
    Result := CommonSphereLeaf(Pos, Radius, TriangleToIgnore,
      TrianglesToIgnoreFunc);
  end;
end;

function TVRMLBaseTrianglesOctreeNode.CommonBox(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    Result := TVRMLBaseTrianglesOctreeNode(subnode).BoxCollision(ABox,
      TriangleToIgnore, TrianglesToIgnoreFunc);
    Stop := result <> nil;
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
begin
  if not IsLeaf then
  begin
    Result := nil;
    OSIS_Box := ABox;
    OCTREE_STEP_INTO_SUBNODES
  end else
  begin
    Result := CommonBoxLeaf(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
  end;
end;

function TVRMLBaseTrianglesOctreeNode.CommonSegment(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
{$define SEGMENT_COLLISION}
{$I vrmltriangle_raysegment_nonleaf.inc}
{$undef SEGMENT_COLLISION}

function TVRMLBaseTrianglesOctreeNode.CommonRay(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
{$I vrmltriangle_raysegment_nonleaf.inc}

{ TVRMLBaseTrianglesOctree --------------------------------------------------- }

{$define SegmentCollision_CommonParams :=
  const pos1, pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc
}

{$define SegmentCollision_Implementation :=
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).SegmentCollision(
    Intersection, IntersectionDistance,
    Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;}

  function TVRMLBaseTrianglesOctree.SegmentCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): PVRMLTriangle;
  SegmentCollision_Implementation

  function TVRMLBaseTrianglesOctree.SegmentCollision(
    out Intersection: TVector3Single;
    SegmentCollision_CommonParams): PVRMLTriangle;
  var
    IntersectionDistance: Single;
  SegmentCollision_Implementation

  function TVRMLBaseTrianglesOctree.SegmentCollision(
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): PVRMLTriangle;
  var
    Intersection: TVector3Single;
  SegmentCollision_Implementation

  function TVRMLBaseTrianglesOctree.SegmentCollision(
    SegmentCollision_CommonParams): PVRMLTriangle;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  SegmentCollision_Implementation

{$undef SegmentCollision_CommonParams}
{$undef SegmentCollision_Implementation}

function TVRMLBaseTrianglesOctree.IsSegmentCollision(
  const pos1, pos2: TVector3Single;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsSegmentCollision(
    Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).SphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.IsSphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsSphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.BoxCollision(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).BoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.IsBoxCollision(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsBoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

{$define RayCollision_CommonParams :=
  const Ray0, RayVector: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc
}

{$define RayCollision_Implementation :=
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).RayCollision(
    Intersection, IntersectionDistance,
    Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;}

  function TVRMLBaseTrianglesOctree.RayCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    RayCollision_CommonParams): PVRMLTriangle;
  RayCollision_Implementation

  function TVRMLBaseTrianglesOctree.RayCollision(
    out Intersection: TVector3Single;
    RayCollision_CommonParams): PVRMLTriangle;
  var
    IntersectionDistance: Single;
  RayCollision_Implementation

  function TVRMLBaseTrianglesOctree.RayCollision(
    out IntersectionDistance: Single;
    RayCollision_CommonParams): PVRMLTriangle;
  var
    Intersection: TVector3Single;
  RayCollision_Implementation

  function TVRMLBaseTrianglesOctree.RayCollision(
    RayCollision_CommonParams): PVRMLTriangle;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  RayCollision_Implementation

{$undef RayCollision_CommonParams}
{$undef RayCollision_Implementation}

function TVRMLBaseTrianglesOctree.IsRayCollision(
  const Ray0, RayVector: TVector3Single;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsRayCollision(
    Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

{ MoveAllowed / GetCameraHeight methods -------------------------------------- }

function TVRMLBaseTrianglesOctree.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const KeepWithinRootBox: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result :=
    (not IsSegmentCollision(OldPos, ProposedNewPos,
      TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
    (not IsSphereCollision(ProposedNewPos, CameraRadius,
      TriangleToIgnore, TrianglesToIgnoreFunc));

  if Result and
     KeepWithinRootBox then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(ProposedNewPos, InternalTreeRoot.Box);
end;

function TVRMLBaseTrianglesOctree.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const KeepWithinRootBox: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result :=
    (not IsSegmentCollision(OldPos, ProposedNewPos,
      TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
    (not IsBoxCollision(ProposedNewBox,
      TriangleToIgnore, TrianglesToIgnoreFunc));

  if Result and
     KeepWithinRootBox then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(ProposedNewPos, InternalTreeRoot.Box);
end;

function TVRMLBaseTrianglesOctree.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single;
  const CameraRadius: Single;
  const KeepWithinRootBox: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;

  function MoveAlongTheBlocker(Blocker: PVRMLTriangle): boolean;
  const
    { This must be something slightly larger than 1.
      Must be larger than 1 (otherwise MoveAlongTheBlocker will
      never produce NewPos that will be satisfied by final
      MoveAllowedSimple test), and obviously must be close to 1
      (otherwise NewPos will not be sensible). }
    CameraRadiusEnlarge = 1.01;
  var
    PlanePtr: PVector4Single;
    PlaneNormalPtr: PVector3Single;
    NewPosShift: TVector3Single;
  begin
    PlanePtr := @(Blocker^.World.Plane);
    PlaneNormalPtr := PVector3Single(PlanePtr);

    { project ProposedNewPos on a plane of blocking object }
    NewPos := PointOnPlaneClosestToPoint(PlanePtr^, ProposedNewPos);

    { now NewPos must be on the same plane side as OldPos is,
      and it must be at the distance slightly larger than CameraRadius from the plane }
    if VectorsSamePlaneDirections(PlaneNormalPtr^,
         VectorSubtract(ProposedNewPos, NewPos), PlanePtr^) then
      NewPosShift := VectorScale(PlaneNormalPtr^,  CameraRadius * CameraRadiusEnlarge) else
      NewPosShift := VectorScale(PlaneNormalPtr^, -CameraRadius * CameraRadiusEnlarge);
    VectorAddTo1st(NewPos, NewPosShift);

    { Even though I calculated NewPos so that it's not blocked by object
      Blocker, I must check whether it's not blocked by something else
      (e.g. if player is trying to walk into the corner (two walls)).
      I can do it by using my simple MoveAllowedSimple. }

    Result := MoveAllowedSimple(OldPos, NewPos, CameraRadius, false,
      TriangleToIgnore, TrianglesToIgnoreFunc);
  end;

var
  Blocker: PVRMLTriangle;
begin
  { Tests: make MoveAllowed equivalent to MoveAllowedSimple:
  Result := MoveAllowedSimple(OldPos, ProposedNewPos, CameraRadius,
    KeepWithinRootBox, TriangleToIgnore, TrianglesToIgnoreFunc);
  if Result then NewPos := ProposedNewPos;
  Exit;
  }

  Blocker := SegmentCollision(OldPos, ProposedNewPos,
    true { return closest blocker },
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  if Blocker = nil then
  begin
    Blocker := SphereCollision(ProposedNewPos, CameraRadius,
      TriangleToIgnore, TrianglesToIgnoreFunc);
    if Blocker = nil then
    begin
      Result := true;
      NewPos := ProposedNewPos;
    end else
      Result := MoveAlongTheBlocker(Blocker);
  end else
    Result := MoveAlongTheBlocker(Blocker);

  if Result and
     KeepWithinRootBox then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(NewPos, InternalTreeRoot.Box);
end;

procedure TVRMLBaseTrianglesOctree.GetCameraHeight(
  const CameraPos, GravityUp: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc);
var
  GroundIntersection: TVector3Single;
begin
  GroundItem := RayCollision(GroundIntersection,
    CameraPos, VectorNegate(GravityUp), true,
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  IsAboveTheGround := GroundItem <> nil;
  if IsAboveTheGround then
    SqrHeightAboveTheGround := PointsDistanceSqr(CameraPos, GroundIntersection);
end;

procedure TVRMLBaseTrianglesOctree.GetCameraHeightZ(
  const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc);
const
  RayDir: TVector3Single = (0, 0, -1);
var
  GroundIntersection: TVector3Single;
begin
  GroundItem := RayCollision(GroundIntersection,
    CameraPos, RayDir, true,
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  IsAboveTheGround := GroundItem <> nil;
  if IsAboveTheGround then
    { Calculation of HeightAboveTheGround uses the fact that RayDir is so simple. }
    HeightAboveTheGround := CameraPos[2] - GroundIntersection[2];
end;

{ Other TVRMLBaseTrianglesOctree utils ----------------------------------------------- }

{$ifdef OCTREE_ITEM_USE_MAILBOX}
function TVRMLBaseTrianglesOctree.AssignNewRayOdcTag: Int64;
begin
 result := NextFreeRayOdcTag;
 Inc(NextFreeRayOdcTag);
end;
{$endif}

class function TVRMLBaseTrianglesOctree.IgnoreTransparentItem(
  const Octree: TVRMLBaseTrianglesOctree;
  const Triangle: PVRMLTriangle): boolean;
begin
  Result :=
    Triangle^.State.LastNodes.Material.Transparency(Triangle^.MatNum)
      > SingleEqualityEpsilon;
end;

function TVRMLBaseTrianglesOctree.ActiveLightNotBlocked(const Light: TActiveLight;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean): boolean;
var LightPos: TVector3Single;
begin
 if not Light.LightNode.FdOn.Value then result := false;

 if Light.LightNode is TVRMLDirectionalLightNode then
  { Swiatlo directional oznacza ze swiatlo polozone jest tak bardzo
    daleko ze wszystkie promienie od swiatla sa rownolegle.

    Od pozycji LightedPoint odejmujemy wydluzone Direction swiatla.

    3 * Box3dMaxSize(Octree.TreeRoot.Box) na pewno jest odlegloscia
    ktora sprawi ze LightPos bedzie poza Octree.TreeRoot.Box
    (bo gdyby nawet Octree.TreeRoot.Box byl szescianem to jego przekatna
    ma dlugosc tylko Sqrt(2) * Sqrt(2) * Box3dMaxSize(Octree.TreeRoot.Box)
    (= 2 * Box3dMaxSize(Octree.TreeRoot.Box))
    W ten sposob otrzymujemy punkt ktory na pewno lezy POZA TreeRoot.Box
    i jezeli nic nie zaslania drogi od Point do tego punktu to
    znaczy ze swiatlo oswietla Intersection. }
  LightPos := VectorSubtract(LightedPoint,
    VectorAdjustToLength(Light.TransfNormDirection,
      3 * Box3dMaxSize(InternalTreeRoot.Box) ) ) else
  LightPos := Light.TransfLocation;

 Result := (VectorsSamePlaneDirections(
       VectorSubtract(LightPos, LightedPoint),
       RenderDir,
       LightedPointPlane)) and
   (SegmentCollision(LightedPoint, LightPos,
     false, TriangleToIgnore, IgnoreMarginAtStart,
     {$ifdef FPC_OBJFPC} @ {$endif} IgnoreTransparentItem)
     = nil);
end;

{ TVRMLOctreeIgnoreTransparentAndOneItem -------------------------------------- }

function TVRMLOctreeIgnoreTransparentAndOneItem.IgnoreItem(
  const Octree: TVRMLBaseTrianglesOctree;
  const Triangle: PVRMLTriangle): boolean;
begin
  Result := (Triangle = OneItem) or
    (Triangle^.State.LastNodes.Material.Transparency(Triangle^.MatNum)
      > SingleEqualityEpsilon);
end;

constructor TVRMLOctreeIgnoreTransparentAndOneItem.Create(AOneItem: PVRMLTriangle);
begin
  inherited Create;
  OneItem := AOneItem;
end;

end.
