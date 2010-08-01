{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML triangles (TVRMLTriangle) and abstract class for octrees
  that resolve collision to VRML triangles (TVRMLBaseTrianglesOctree). }
unit VRMLTriangle;

{$I vrmloctreeconf.inc}

interface

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Base3D, Boxes3D,
  KambiOctree;

{$define read_interface}

{ TVRMLTriangle  ------------------------------------------------------------ }

type
  { }
  TCollisionCount = Int64;
  TMailboxTag = Int64;

  { Triangle of VRML model. This is the most basic item for our
    VRML collision detection routines, returned by octrees descending from
    TVRMLBaseTrianglesOctree. }
  TVRMLTriangle = object(T3DTriangle)
  public
    { Initialize new triangle of VRML model.
      Given ATriangle must satisfy IsValidTriangle. }
    constructor Init(const ATriangle: TTriangle3Single;
      AState: TVRMLGraphTraverseState; AGeometry: TVRMLGeometryNode;
      const AMatNum, AFaceCoordIndexBegin, AFaceCoordIndexEnd: integer);

    procedure UpdateWorld;
  public
    State: TVRMLGraphTraverseState;
    Geometry: TVRMLGeometryNode;
    MatNum: integer;

    { If this triangle is part of a face created by coordIndex field
      (like all faces in IndexedFaceSet) then these fields indicate where
      in this coordIndex this face is located.

      You should look into Geometry, get it's coordIndex field,
      and the relevant indexes are between FaceCoordIndexBegin
      and FaceCoordIndexEnd - 1. Index FaceCoordIndexEnd is either
      non-existing (coordIndex list ended) or is the "-1" (faces separator
      on coordIndex fields).

      If this triangle doesn't come from any coordIndex (e.g. because Geometry
      is a TNodeSphere) then both FaceCoordIndex* are -1. }
    FaceCoordIndexBegin, FaceCoordIndexEnd: Integer;

    {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { MailboxSavedTag is a tag of object (like ray or line segment)
      for which we have saved an
      intersection result. Intersection result is in
      MailboxIsIntersection, MailboxIntersection, MailboxIntersectionDistance.

      To make things correct, we obviously assume that every segment
      and ray have different tags. Also, tag -1 is reserved.
      In practice, we simply initialize MailboxSavedTag to -1
      in constructor, and each new segment/ray get consecutive tags
      starting from 0.

      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { Check collisions between TVRMLTriangle and ray/segment.

      Always use these routines to check for collisions,
      to use mailboxes if possible. Mailboxes are used only if this was
      compiled with TRIANGLE_OCTREE_USE_MAILBOX defined.

      Increments DirectCollisionTestsCounter if actual test was done
      (that is, if we couldn't use mailbox to get the result quickier).

      @groupBegin }
    function SegmentDirCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Odc0, OdcVector: TVector3Single;
      const SegmentTag: TMailboxTag;
      var DirectCollisionTestsCounter: TCollisionCount): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const RayTag: TMailboxTag;
      var DirectCollisionTestsCounter: TCollisionCount): boolean;
    { @groupEnd }

    { Create material information instance for material of this triangle.
      See TVRMLMaterialInfo for usage description.

      Returns @nil when no Material node is defined, this can happen
      only for VRML 1.0.

      Returned TVRMLMaterialInfo is valid only as long as the Material
      node (for VRML 1.0 or 2.0) on which it was based. }
    function MaterialInfo: TVRMLMaterialInfo;

    { Return transparency of this triangle's material.
      Equivalent to MaterialInfo.Transparency, although a little faster. }
    function Transparency: Single;

    { Returns @true for triangles that are transparent. }
    function IsTransparent: boolean;

    { Returns @true for triangles that should be ignored by shadow rays.
      Returns @true for transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE).

      @seealso TVRMLBaseTrianglesOctree.IgnoreForShadowRays }
    function IgnoreForShadowRays: boolean;
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
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonBox(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function CommonBoxLeaf(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonSegment(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function CommonRay(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;
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
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; virtual; abstract;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      const Tag: TMailboxTag;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual; abstract;
    { @groupEnd }
  end;

  { Callback for @link(TVRMLBaseTrianglesOctree.EnumerateTriangles). }
  TEnumerateTriangleFunc = procedure (const Triangle: PVRMLTriangle) of object;

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
    { nastepny wolny tag ktory przydzielimy nastepnemu promieniowi lub
      odcinkowi z ktorym bedziemy chcieli robic test na kolizje z octree.
      Ta zmienna moze byc czytana/pisana tylko przez AssignNewTag. }
    NextFreeTag: TMailboxTag;

    { zwroci NextFreeTag i zrobi Inc(NextFreeTag).
      Uzywaj tego aby przydzielic nowy tag. Uzywanie tej funkcji przy okazji
      zapobiega potencjalnie blednej sytuacji :
        TreeRoot.SegmentColision(..., NextFreeTag, ...)
        Inc(NextFreeTag);
      Powyzszy kod bedzie ZAZWYCZAJ dzialal - ale spowoduje on ze nie bedzie
      mozna uzywac Segment/RayCollision na tym samym TVRMLTriangleOctree gdy juz
      jestesmy w trakcie badania kolizji. Np. callbacki w rodzaju
      T3DTriangleIgnoreFunc nie beda mogly wywolywac kolizji. Innymi slowy,
      taki zapis uczynilby Segment/RayCollision non-reentrant. A to na dluzsza
      mete zawsze jest klopotliwe. Natomiast robienie Inc(NextFreeTag);
      przed faktycznym wejsciem do funkcji TreeRoot.SegmentColision
      usuwa ten blad. Uzywajac funkcji AssignNewTag automatycznie
      to robimy. }
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
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      out Intersection: TVector3Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function SegmentCollision(
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function BoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    function IsBoxCollision(const ABox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function RayCollision(const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; overload;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
    { @groupEnd }

    { This checks if move between OldPos and ProposedNewPos is possible,
      by checking is segment between OldPos and ProposedNewPos free
      and sphere (with radius CameraRadius) ProposedNewPos is free.

      CameraRadius must obviously be > 0.

      See @link(MoveAllowed) for some more sophisticated way of
      collision detection.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. This can be used to allow
      camera to walk thorugh some surfaces (e.g. through water
      surface, or to allow player to walk through some "fake wall"
      and discover secret room in game etc.).

      @seealso(TWalkCamera.DoMoveAllowed
        TWalkCamera.DoMoveAllowed is some place
        where you can use this function) }
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;

    { This is like @link(MoveAllowedSimple), but it checks for collision
      around ProposedNewPos using TBox3D instead of a sphere. }
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3D;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;

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

      If KeepAboveMinPlane then we will additionally make sure that
      the resulting position is above (or exactly on) the @italic(minimal plane).
      Minimal plane in calculated as minimal plane intersecting
      MinPlaneBox pointed at direction MinPlaneDirection (see
      Box3DMinimumPlane for more precise definition).
      When MinPlaneBox is empty and KeepAboveMinPlane, then
      we will not allow any move.

      KeepAboveMinPlane is specifically useful for handling moves because
      of gravity. Typically, you can pass KeepAboveMinPlane = BecauseOfGravity
      (where BecauseOfGravity comes from callback like Cameras.TMoveAllowedFunc),
      MinPlaneBox = your animation box, MinPlaneDirection = gravity up vector.
      This way, we will not allow player to fall below the lowest plane
      when gravity works --- this is good sometimes, otherwise the player
      could fall infinitely down outside of this box.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision.

      @seealso(TWalkCamera.DoMoveAllowed
        TWalkCamera.DoMoveAllowed is some place
        where you can use this function)

      @groupBegin }
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const CameraRadius: Single;
      const KeepAboveMinPlane: boolean;
      const MinPlaneBox: TBox3D;
      const MinPlaneDirection: TVector3Single;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;

    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TriangleToIgnore: PVRMLTriangle = nil;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc = nil): boolean;
    { @groupEnd }

    { For given camera position and up vector, calculate camera height
      above the ground. This is comfortable for cooperation with
      TWalkCamera.OnGetHeightAbove.

      This simply checks collision of a ray from
      Position in direction -GravityUp, and sets IsAbove,
      AboveHeight and AboveGround as needed. See TBase3D.GetHeightAbove
      for specification.

      TriangleToIgnore and TrianglesToIgnoreFunc meaning
      is just like for RayCollision. }
    procedure GetHeightAbove(
      const Position, GravityUp: TVector3Single;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: PVRMLTriangle;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc);

    { This is just like GetHeightAbove, but it assumes that
      GravityUp = (0, 0, 1). This is faster than GetHeightAbove,
      since calculating AboveHeight doesn't require now costly Sqrt operation. }
    procedure GetHeightAboveZ(
      const Position: TVector3Single;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: PVRMLTriangle;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc);

    { This ignores (that is, returns @true) transparent triangles
      (with Material.Transparency > 0).

      This is suitable for T3DTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    class function IgnoreTransparentItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;

    { This ignores (that is, returns @true) transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE).

      This is suitable for T3DTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    class function IgnoreForShadowRays(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;

    { Checks whether VRML Light (point or directional) lights at scene point
      LightedPoint.

      "Dociera do punktu" to znaczy
      1) Light.LightNode jest ON (FdOn.Value = true) (ten check jest zrobiony
         na samym poczatku bo moze przeciez zaoszczedzic sporo czasu)
      2) ze droga pomiedzy Light a LightedPoint jest wolna w Octree
         (with IgnoreForShadowRays, that is we ignore transparent
         and non-shadow-casting triangles;
         TODO: to jest uproszczenie, for transparent triangles they should
         not block but still should scale the light)
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

    { Enumerate every triangle of this octree.

      It passes to EnumerateTriangleFunc callback a Triangle.
      Triangle is passed as a pointer (never @nil) --- these are guaranteed
      to be "stable" pointers stored inside octrees' lists (so they will be valid
      as long as octree (and eventual children octrees for TVRMLShapeOctree)).

      Every triangle is guaranteed to have it's World coordinates updated
      (to put it simply, when this is used on TVRMLShapeOctree, then we
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
    TVRMLBaseTrianglesOctree methods. }
  TVRMLOctreeIgnoreForShadowRaysAndOneItem = class
  public
    OneItem: PVRMLTriangle;
    function IgnoreItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean;
    constructor Create(AOneItem: PVRMLTriangle);
  end;

{ Check is NewPos above the minimal plane, just like
  TVRMLBaseTrianglesOctree.MoveAllowed does when KeepAboveMinPlane = @true.
  Sometimes it's useful to call this separately. }
function SimpleKeepAboveMinPlane(
  const NewPos: TVector3Single;
  const MinPlaneBox: TBox3D;
  const MinPlaneDirection: TVector3Single): boolean;

{$undef read_interface}

implementation

uses KambiStringUtils;

{$define read_implementation}
{$I dynarray_1.inc}

{ TVRMLTriangle  ------------------------------------------------------------- }

constructor TVRMLTriangle.Init(const ATriangle: TTriangle3Single;
  AState: TVRMLGraphTraverseState; AGeometry: TVRMLGeometryNode;
  const AMatNum, AFaceCoordIndexBegin, AFaceCoordIndexEnd: Integer);
begin
  inherited Init(ATriangle);

  State := AState;
  Geometry := AGeometry;
  MatNum := AMatNum;
  FaceCoordIndexBegin := AFaceCoordIndexBegin;
  FaceCoordIndexEnd := AFaceCoordIndexEnd;

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
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
  const SegmentTag: TMailboxTag;
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
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
      Loc.Triangle, Loc.Plane,
      Odc0, OdcVector);
    Inc(DirectCollisionTestsCounter);

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

function TVRMLTriangle.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const RayTag: TMailboxTag;
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
  { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
    z jednego Ray0 ale z roznym RayVector (np. w raytracerze). Wiec lepiej
    najpierw porownywac przechowywane w skrzynce RayVector (niz Ray0)
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
      Loc.Triangle, Loc.Plane,
      Ray0, RayVector);
    Inc(DirectCollisionTestsCounter);

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

function TVRMLTriangle.MaterialInfo: TVRMLMaterialInfo;
var
  M2: TNodeMaterial_2;
begin
  if State.ShapeNode <> nil then
  begin
    M2 := State.ShapeNode.Material;
    if M2 <> nil then
      Result := M2.MaterialInfo else
      Result := nil;
  end else
    Result := State.LastNodes.Material.MaterialInfo(MatNum);
end;

function TVRMLTriangle.Transparency: Single;
var
  M2: TNodeMaterial_2;
begin
  if State.ShapeNode <> nil then
  begin
    M2 := State.ShapeNode.Material;
    if M2 <> nil then
      Result := M2.FdTransparency.Value else
      Result := 0;
  end else
    Result := State.LastNodes.Material.Transparency(MatNum);
end;

function TVRMLTriangle.IsTransparent: boolean;
begin
  Result := Transparency > SingleEqualityEpsilon;
end;

function TVRMLTriangle.IgnoreForShadowRays: boolean;

  function NonShadowCaster(State: TVRMLGraphTraverseState): boolean;
  var
    Shape: TNodeX3DShapeNode;
  begin
    Shape := State.ShapeNode;
    Result :=
      (Shape <> nil) and
      (Shape.FdAppearance.Value <> nil) and
      (Shape.FdAppearance.Value is TNodeAppearance) and
      (not TNodeAppearance(Shape.FdAppearance.Value).FdShadowCaster.Value);
  end;

begin
  Result := ({ IsTransparent } Transparency > SingleEqualityEpsilon) or
    NonShadowCaster(State);
end;

{ TVRMLBaseTrianglesOctreeNode -----------------------------------------------

  Common* (non-leaf nodes) implementations }

function TVRMLBaseTrianglesOctreeNode.CommonSphere(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
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
    SubnodesBox[0] := VectorSubtract(pos, Vector3Single(Radius, Radius, Radius) );
    SubnodesBox[1] := VectorAdd(     pos, Vector3Single(Radius, Radius, Radius) );

    SubnodesWithBox(SubnodesBox, BoxLo, BoxHi);

    for B0 := BoxLo[0] to BoxHi[0] do
      for B1 := BoxLo[1] to BoxHi[1] do
        for B2 := BoxLo[2] to BoxHi[2] do
        begin
          Result := TVRMLBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            CommonSphere(Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonSphereLeaf(Pos, Radius, TriangleToIgnore,
      TrianglesToIgnoreFunc);
  end;
end;

function TVRMLBaseTrianglesOctreeNode.CommonBox(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
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
          Result := TVRMLBaseTrianglesOctreeNode(TreeSubNodes[B0, B1, B2]).
            BoxCollision(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
          if Result <> nil then Exit;
        end;
  end else
  begin
    Result := CommonBoxLeaf(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
  end;
end;

function TVRMLBaseTrianglesOctreeNode.CommonSegment(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
{$define SEGMENT_COLLISION}
{$I vrmltriangle_raysegment_nonleaf.inc}
{$undef SEGMENT_COLLISION}

function TVRMLBaseTrianglesOctreeNode.CommonRay(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
{$I vrmltriangle_raysegment_nonleaf.inc}

{ TVRMLBaseTrianglesOctree --------------------------------------------------- }

{$define SegmentCollision_CommonParams :=
  const pos1, pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc
}

{$define SegmentCollision_Implementation :=
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).SegmentCollision(
    Intersection, IntersectionDistance,
    Pos1, Pos2,
    AssignNewTag,
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
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsSegmentCollision(
    Pos1, Pos2,
    AssignNewTag,
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).SphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.IsSphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsSphereCollision(
    Pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).BoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TVRMLBaseTrianglesOctree.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsBoxCollision(
    ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

{$define RayCollision_CommonParams :=
  const Ray0, RayVector: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc
}

{$define RayCollision_Implementation :=
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).RayCollision(
    Intersection, IntersectionDistance,
    Ray0, RayVector,
    AssignNewTag,
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
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := TVRMLBaseTrianglesOctreeNode(InternalTreeRoot).IsRayCollision(
    Ray0, RayVector,
    AssignNewTag,
    TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

{ MoveAllowed / GetHeightAbove methods -------------------------------------- }

function SimpleKeepAboveMinPlane(
  const NewPos: TVector3Single;
  const MinPlaneBox: TBox3D;
  const MinPlaneDirection: TVector3Single): boolean;
var
  GravityPlane: TVector4Single;
begin
  if IsEmptyBox3D(MinPlaneBox) then
    Result := false else
  begin
    { TODO: instead of setting Result to false, this should
      actually update NewPos so that it's *exactly* on the GravityPlane.
      In other words, implementation below is Ok for MoveAllowedSimple,
      but for full-features MoveAllowed we can do something slightly better.

      Doesn't seem to matter in practice, it's not noticeable. }

    GravityPlane := Box3DMinimumPlane(MinPlaneBox, MinPlaneDirection);
    Result := GravityPlane[0] * NewPos[0] +
              GravityPlane[1] * NewPos[1] +
              GravityPlane[2] * NewPos[2] +
              GravityPlane[3] >= 0;
  end;
end;

function TVRMLBaseTrianglesOctree.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result :=
    (not IsSegmentCollision(OldPos, ProposedNewPos,
      TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
    (not IsSphereCollision(ProposedNewPos, CameraRadius,
      TriangleToIgnore, TrianglesToIgnoreFunc));
end;

function TVRMLBaseTrianglesOctree.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result :=
    (not IsSegmentCollision(OldPos, ProposedNewPos,
      TriangleToIgnore, false, TrianglesToIgnoreFunc)) and
    (not IsBoxCollision(ProposedNewBox,
      TriangleToIgnore, TrianglesToIgnoreFunc));
end;

function TVRMLBaseTrianglesOctree.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;

  { $define DEBUG_WALL_SLIDING}

  const
    { For wall-sliding inside MoveAlongTheBlocker implementations,
      we want to position ourselves slightly farther away than
      CameraRadius. (Exactly on CameraRadius would mean that it's
      sensitive to floating point imprecision, and sometimes the sphere
      could be considered colliding with Blocker anyway, instead
      of sliding along it. And final MoveAllowedSimple test will
      then fail, making wall-sliding non-working.)

      So this must be something slightly larger than 1.
      And obviously must be close to 1
      (otherwise NewPos will not be sensible). }
    CameraRadiusEnlarge = 1.01;

  { This is the worse version of wall-sliding:
    we don't know the 3D point of intersection with blocker,
    which means we can't really calculate a vector to make
    proper wall-sliding. We do some tricks to still perform wall-sliding
    in many positions, but it's not perfect. }
  function MoveAlongTheBlocker(Blocker: PVRMLTriangle): boolean;
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

    Result := MoveAllowedSimple(OldPos, NewPos, CameraRadius,
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
    Blocker: PVRMLTriangle): boolean;
  var
    PlanePtr: PVector4Single;
    Slide, Projected: TVector3Single;
    NewBlocker: PVRMLTriangle;
    NewBlockerIntersection: TVector3Single;
  begin
    PlanePtr := @(Blocker^.World.Plane);

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

      VectorAdjustToLengthTo1st(Slide, PointsDistance(OldPos, ProposedNewPos));

      NewPos := VectorAdd(OldPos, Slide);

      { Even though I calculated NewPos so that it's not blocked by object
        Blocker, I must check whether it's not blocked by something else
        (e.g. if player is trying to walk into the corner (two walls)).
        I can do it by using my simple MoveAllowedSimple. }

      Result := MoveAllowedSimple(OldPos, NewPos,
        CameraRadius, TriangleToIgnore, TrianglesToIgnoreFunc);

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
             CameraRadius * 2) and
           TryPlaneLineIntersection(NewBlockerIntersection,
             NewBlocker^.World.Plane,
             OldPos, VectorSubtract(ProposedNewPos, OldPos)) then
        begin
          {$ifdef DEBUG_WALL_SLIDING} Writeln('Wall-sliding: Better blocker found: ', PointerToStr(NewBlocker), '.'); {$endif}

          { Below we essentially make the wall-sliding computation again.
            We know that we're in sphere collision case
            (checked above that "not SegmentCollision"). }

          PlanePtr := @(NewBlocker^.World.Plane);
          Projected := PointOnPlaneClosestToPoint(PlanePtr^, OldPos);
          Slide := VectorSubtract(NewBlockerIntersection, Projected);

          if not ZeroVector(Slide) then
          begin
            VectorAdjustToLengthTo1st(Slide, PointsDistance(OldPos, ProposedNewPos));
            NewPos := VectorAdd(OldPos, Slide);
            Result := MoveAllowedSimple(OldPos, NewPos,
              CameraRadius, TriangleToIgnore, TrianglesToIgnoreFunc);

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
              ProposedNewPos, CameraRadius), ' ',
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
  Blocker: PVRMLTriangle;
  BlockerIntersection: TVector3Single;
begin
  { Tests: make MoveAllowed equivalent to MoveAllowedSimple:
  Result := MoveAllowedSimple(OldPos, ProposedNewPos, CameraRadius,
    KeepWithinRootBox, TriangleToIgnore, TrianglesToIgnoreFunc);
  if Result then NewPos := ProposedNewPos;
  Exit;
  }

  Blocker := SegmentCollision(
    BlockerIntersection, OldPos, ProposedNewPos,
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
    if TryPlaneLineIntersection(BlockerIntersection, Blocker^.World.Plane,
      OldPos, VectorSubtract(ProposedNewPos, OldPos)) then
      Result := MoveAlongTheBlocker(BlockerIntersection, false, Blocker) else
      Result := MoveAlongTheBlocker(Blocker);
  end else
    Result := MoveAlongTheBlocker(BlockerIntersection, true, Blocker);
end;

function TVRMLBaseTrianglesOctree.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single;
  const CameraRadius: Single;
  const KeepAboveMinPlane: boolean;
  const MinPlaneBox: TBox3D;
  const MinPlaneDirection: TVector3Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := MoveAllowed(OldPos, ProposedNewPos, NewPos, CameraRadius,
    TriangleToIgnore, TrianglesToIgnoreFunc);
  if Result and KeepAboveMinPlane then
    Result := SimpleKeepAboveMinPlane(NewPos, MinPlaneBox, MinPlaneDirection);
end;

procedure TVRMLBaseTrianglesOctree.GetHeightAbove(
  const Position, GravityUp: TVector3Single;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: PVRMLTriangle;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc);
var
  GroundIntersection: TVector3Single;
begin
  AboveGround := RayCollision(GroundIntersection,
    Position, VectorNegate(GravityUp), true,
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  IsAbove := AboveGround <> nil;
  if IsAbove then
    AboveHeight := PointsDistance(Position, GroundIntersection) else
    AboveHeight := MaxSingle;
end;

procedure TVRMLBaseTrianglesOctree.GetHeightAboveZ(
  const Position: TVector3Single;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: PVRMLTriangle;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc);
const
  RayDir: TVector3Single = (0, 0, -1);
var
  GroundIntersection: TVector3Single;
begin
  AboveGround := RayCollision(GroundIntersection,
    Position, RayDir, true,
    TriangleToIgnore, false, TrianglesToIgnoreFunc);
  IsAbove := AboveGround <> nil;
  if IsAbove then
    { Calculation of AboveHeight uses the fact that RayDir is so simple. }
    AboveHeight := Position[2] - GroundIntersection[2] else
    AboveHeight := MaxSingle;
end;

{ Other TVRMLBaseTrianglesOctree utils ----------------------------------------------- }

function TVRMLBaseTrianglesOctree.AssignNewTag: TMailboxTag;
begin
 result := NextFreeTag;
 Inc(NextFreeTag);
end;

class function TVRMLBaseTrianglesOctree.IgnoreTransparentItem(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := PVRMLTriangle(Triangle)^.IsTransparent;
end;

class function TVRMLBaseTrianglesOctree.IgnoreForShadowRays(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := PVRMLTriangle(Triangle)^.IgnoreForShadowRays;
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

    3 * Box3DMaxSize(Octree.TreeRoot.Box) na pewno jest odlegloscia
    ktora sprawi ze LightPos bedzie poza Octree.TreeRoot.Box
    (bo gdyby nawet Octree.TreeRoot.Box byl szescianem to jego przekatna
    ma dlugosc tylko Sqrt(2) * Sqrt(2) * Box3DMaxSize(Octree.TreeRoot.Box)
    (= 2 * Box3DMaxSize(Octree.TreeRoot.Box))
    W ten sposob otrzymujemy punkt ktory na pewno lezy POZA TreeRoot.Box
    i jezeli nic nie zaslania drogi od Point do tego punktu to
    znaczy ze swiatlo oswietla Intersection. }
  LightPos := VectorSubtract(LightedPoint,
    VectorAdjustToLength(Light.TransfNormDirection,
      3 * Box3DMaxSize(InternalTreeRoot.Box) ) ) else
  LightPos := Light.TransfLocation;

 Result := (VectorsSamePlaneDirections(
       VectorSubtract(LightPos, LightedPoint),
       RenderDir,
       LightedPointPlane)) and
   (SegmentCollision(LightedPoint, LightPos,
     false, TriangleToIgnore, IgnoreMarginAtStart, @IgnoreForShadowRays)
     = nil);
end;

{ TVRMLOctreeIgnoreForShadowRaysAndOneItem -------------------------------------- }

function TVRMLOctreeIgnoreForShadowRaysAndOneItem.IgnoreItem(
  const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := (Triangle = P3DTriangle(OneItem)) or
    PVRMLTriangle(Triangle)^.IgnoreForShadowRays;
end;

constructor TVRMLOctreeIgnoreForShadowRaysAndOneItem.Create(
  AOneItem: PVRMLTriangle);
begin
  inherited Create;
  OneItem := AOneItem;
end;

end.
