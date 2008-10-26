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
}

{ @abstract(@link(TVRMLTriangleOctree) class --- octree that provides
  hierarchical tree of all triangles in a VRML scene.)

  Dokladniej, mozemy
  nawet zbudowac drzewo osemkowe laczac mniejsze kawalki wielu roznych
  scen VRML'a. Elementami ktore bedziemy trzymac w lisciach drzewa sa
  rekordy TOctreeItem - jeden taki rekord reprezentuje jeden trojkat
  w przestrzeni. Razem z kazdym trojkatem zapamietywana jest informacja
  z jakiego State'a i Shape'a on pochodzi. Wiec pamietaj ze po zbudowaniu
  ze sceny octree scena jest praktycznie "zamrozona" - nic nie wolno
  w niej zmieniac.

  Zasadnicza klasa rekurencyjna ktora reprezentuje wezel drzewa
  (lisc = liste indeksow do TOctreeItem lub
   wezel wewnetrzny = 8 podwezlow TOctreeNode) jest TOctreeNode.
  Klasa TVRMLTriangleOctree to proste opakowanie na TreeRoot: TOctreeNode,
  przechowuje miedzy innymi liste OctreeItems (w TOctreeNode mamy tylko
  indeksy do nich) co pozwala nam zaoszczedzic MASE pamieci i umozliwia
  nam zaimplementowanie skrzynek pocztowych podczas sprawdzania przeciec.
}

unit VRMLTriangleOctree;

{
  TODO
  - "po zbudowaniu ze sceny octree scena jest praktycznie "zamrozona" -
    nic nie wolno w niej zmieniac" :
    oczywiscie chcialbym zeby to sie z czasem
    zmienilo, zebym mial w octree przygotowane funkcje ktore potrafiliby
    radzic sobie z (przynajmniej pewnymi) zmianami sceny (tak jak jest teraz
    zmiana sceny oznacza w zasadzie koniecznosc przebudowania octree).
}

{$I vrmloctreeconf.inc}

interface

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3d, Math,
  KambiOctree, VRMLOctreeUtils;

{$define read_interface}

const
  DefTriangleOctreeMaxDepth = 10;
  DefTriangleOctreeLeafCapacity = 20;

{ TOctreeNode ------------------------------------------------------------------}

type
  TVRMLTriangleOctree = class;

  { Return for given OctreeItem do we want to ignore collisions with it. }
  TOctreeItemIgnoreFunc = function (
    const Octree: TVRMLTriangleOctree;
    const OctreeItem: POctreeItem): boolean of object;

  TTriangleOctreeNode = class(TOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;
  private
    function GetItems(ItemIndex: integer): POctreeItem;
  public
    function ParentTree: TVRMLTriangleOctree;

    { Items zapewniaja wygodniejszy (czasami) dostep do tablicy ItemsIndices.
      Podane ItemIndex jest indeksem do tablicy ItemsIndices - wyciagamy z tego
      ParentTree.OctreeItems[ItemsIndices[ItemIndex]] }
    property Items[ItemIndex: integer]: POctreeItem read GetItems;

    { definicja tych proc - patrz metody o tych samych nazwach w
      TVRMLTriangleOctree.

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
      przypadkiem kolizji ktora de facto zdarzyla sie w innym subnodzie. }
    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

    function BoxCollision(const ABox: TBox3d;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;
  end;

  { TVRMLTriangleOctree ------------------------------------------------------------ }

  TVRMLTriangleOctree = class(TOctree)
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
      TOctreeItemIgnoreFunc nie beda mogly wywolywac kolizji. Innymi slowy,
      taki zapis uczynilby Segment/RayCollision non-reentrant. A to na dluzsza
      mete zawsze jest klopotliwe. Natomiast robienie Inc(NextFreeRayOdcTag);
      przed faktycznym wejsciem do funkcji TreeRoot.SegmentColision
      usuwa ten blad. Uzywajac funkcji AssignNewRayOdcTag automatycznie
      to robimy. }
    function AssignNewRayOdcTag: Int64;
    {$endif OCTREE_ITEM_USE_MAILBOX}
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    { tu beda zgromadzone wszystkie OctreeItems jakie mamy w drzewie.
      W lisciach beda tylko ItemsIndices ktore beda indeksami do tej tablicy.
      Zrobilem to 27.04.2003 gdy zobaczylem w drzewie
      z ciasno dobranymi MaxDepth i LeafCapacity jeden trojkat sceny moze
      byc powielony az 50 000 razy ! To powodowalo zzeranie niesamowitych ilosci
      pamieci, bo rekord TOctreeItem jest dosc duzy i z czasem pewnie bede go
      jeszcze rozszerzal. Trzymanie wszystkich elementow w tablicy pozwala
      mi miec w lapie kazdy element tylko raz.
      - ponadto unikajac robienia TOctreeItem jako obiektow unikam fragmentacji
        pamieci
      - umozliwilem sobie zrobienie OCTREE_ITEM_USE_MAILBOX
      - umozliwiam realizowanie OctreeItemToIgnore w RayCollision przez szybkie
        porownywanie of a simple pointer (zamiast np. zawartosci TOctreeItem) }
    OctreeItems: TDynOctreeItemsArray;

    function TreeRoot: TTriangleOctreeNode;

    { Add single OctreeItem. Automatically checks whether IsValidTriangle.
      Przed dodaniem duzej ilosci trojkatow sugerowane jest aby ustalic
      OctreeItems.AllowedCapacityCount na odpowiednio duza wartosc.  }
    procedure AddItemTriangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);

    { Direct collisions counter, to test octree efficiency.

      It is incremented each time XxxCollision make a direct
      collision test, that is when some single triangle is tested for collision
      with a sphere, line segment and such. The very idea of octree is to
      minimize this number.

      0 by default. }
    DirectCollisionTestsCounter: TCollisionCount;

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
        collision found.))

      @param(OctreeItemToIgnore

        If this is non-nil, then Segment/RayCollision assume that there
        is @italic(never) a collision with this octree item.
        It's never returned as collidable item.

        This is useful for recursive ray-tracer, when you start tracing
        from some existing face (octree item). In this case, you don't
        want to "hit" the starting face. So you can pass this face
        as OctreeItemToIgnore.

        Note that IgnoreMarginAtStart helps with the same problem,
        although a little differently.)

      @param(ItemsToIgnoreFunc

        If assigned, then items for which ItemsToIgnoreFunc returns @true
        will be ignored. This is a more general mechanism than
        OctreeItemToIgnore, as you can ignore many items, you can also
        make some condition to ignore --- for example, you can ignore
        partially transparent items.)

      @param(IgnoreMarginAtStart

        If @true, then collisions that happen very very close to Ray0 (or Pos1
        for SegmentCollision) will be ignored.

        This is another thing helpful for recursive ray-tracers:
        you don't want to hit the starting face, or any coplanar faces,
        when tracing reflected/refracted/shadow ray.

        Note that if you know actual pointer of your face, it's better to use
        OctreeItemToIgnore --- OctreeItemToIgnore is a 100% guaranteed
        stable solution, while IgnoreMarginAtStart necessarily has some
        "epsilon" constant that determines which items are ignored.
        This epsilon may be too large, or too small, in some cases.

        In practice, recursive ray-tracers should use both
        OctreeItemToIgnore (to avoid collisions with starting face)
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
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function SegmentCollision(
      out Intersection: TVector3Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function SegmentCollision(
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function SegmentCollision(
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

    function BoxCollision(const ABox: TBox3d;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;

    function RayCollision(const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemToIgnore: POctreeItem;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem; overload;
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

      OctreeItemToIgnore and ItemsToIgnoreFunc meaning
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
      const OctreeItemToIgnore: POctreeItem = nil;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc = nil): boolean;

    { This is like @link(MoveAllowedSimple), but it checks for collision
      around ProposedNewPos using TBox3d instead of a sphere. }
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const KeepWithinRootBox: boolean;
      const OctreeItemToIgnore: POctreeItem = nil;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc = nil): boolean;

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

      OctreeItemToIgnore and ItemsToIgnoreFunc meaning
      is just like for RayCollision.

      @seealso(TWalkNavigator.DoMoveAllowed
        TWalkNavigator.DoMoveAllowed is some place
        where you can use this function) }
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const CameraRadius: Single;
      const KeepWithinRootBox: boolean;
      const OctreeItemToIgnore: POctreeItem = nil;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc = nil): boolean;

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

      OctreeItemToIgnore and ItemsToIgnoreFunc meaning
      is just like for RayCollision. }
    procedure GetCameraHeight(
      const CameraPos, GravityUp: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: POctreeItem;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);

    { This is just like GetCameraHeight, but it assumes that
      GravityUp = (0, 0, 1) and it returns the actual
      HeightAboveTheGround (not it's square). Thanks to the fact that
      calculating HeightAboveTheGround doesn't require costly Sqrt operation
      in case of such simple GravityUp. }
    procedure GetCameraHeightZ(
      const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
      out GroundItem: POctreeItem;
      const OctreeItemToIgnore: POctreeItem;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);

    { This makes transparent triangles (with material
      with Transparency > 0) ignored (i.e. returns @true for them).
      This is a prepared function compatible with TOctreeItemIgnoreFunc
      function type. }
    class function IgnoreTransparentItem(
      const Octree: TVRMLTriangleOctree;
      const OctreeItem: POctreeItem): boolean;

    constructor Create(const ARootBox: TBox3d); overload;
    constructor Create(AMaxDepth, ALeafCapacity: integer;
      const ARootBox: TBox3d); overload;
    destructor Destroy; override;
  end;

  { przygotowane funkcje i klasy dla ItemsToIgnoreFunc: TOctreeItemIgnoreFunc. }

  TOctreeIgnore_Transparent_And_OneItem = class
    OneItem: POctreeItem;
    { IgnoreItem zwraca true dla obiektow ktorych Transparency > 0,
      and for OneItem. }
    function IgnoreItem(
      const Octree: TVRMLTriangleOctree;
      const OctreeItem: POctreeItem): boolean;
    constructor Create(AOneItem: POctreeItem);
  end;

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

  Szukanie kolizji w octree uzywa przekazanych OctreeItemToIgnore i
  IgnoreMarginAtStart - zazwyczaj powinienes je przekazac na element
  w drzewie z ktorego wziales LightedPoint i na true, odpowiednio.

  Jezeli ta funkcja zwroci true to zazwyczaj pozostaje ci obliczenie
  wplywu swiatla na dany punkt z lokalnych rownan oswietlenia (przy czym
  mozesz juz pominac sprawdzanie LightNode.FdOn - chociaz zazwyczaj
  lepiej bedzie nie pomijac, powtorzenie takiego prostego checku nie
  powoduje przeciez zbytniego marnotrawstwa czasu a kod moze wydawac
  sie bardziej spojny w ten sposob).
}
function ActiveLightNotBlocked(Octree: TVRMLTriangleOctree; const Light: TActiveLight;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean): boolean;

{$undef read_interface}

implementation

{$define read_implementation}

{$I kambioctreemacros.inc}

{ TTriangleOctreeNode -------------------------------------------------------------- }

procedure TTriangleOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
var
  AddedSomewhere: boolean;
  Triangle: PTriangle3Single;

  procedure JustAdd(SubNode: TOctreeNode);
  begin
    SubNode.AddItem(ItemIndex);
    AddedSomewhere := true;
  end;

  procedure SecondTestAndAdd(SubNode: TOctreeNode);
  begin
    if IsBox3dTriangleCollision(SubNode.Box, Triangle^) then
    begin
      SubNode.AddItem(ItemIndex);
      AddedSomewhere := true;
    end;
  end;

var
  OSIS_b_low, OSIS_b_high: TOctreeSubnodeIndex;
  OSIS_b_0, OSIS_b_1, OSIS_b_2: boolean;
begin
  AddedSomewhere := false;

  Triangle := @(ParentTree.OctreeItems.Items[ItemIndex].Triangle);

  { First prototype of this just run SecondTestAndAdd 8 times, without
    initial SubnodesWithBox checking. It turns out that it's faster
    to do SubnodesWithBox first, this way we eliminate many calls
    to IsBox3dTriangleCollision.

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
      it seems unavoidable: IsBox3dTriangleCollision tries hard
      to detect that there's no collision. Even with really large
      epsilons inside IsBox3dTriangleCollision,
      it often detects no collision, while in fact triangle
      lies on the boundary of SubNode.Box.

      I tried to make epsilons inside IsBox3dTriangleCollision larger,
      use better calculation (on doubles instead of singles),
      and all of this helped... for some time. For some scenes.
      Sooner or later, I was always able to find another scene,
      so specific that requires even larger epsilon inside
      IsBox3dTriangleCollision... This is unacceptable of course.

      So I detect the cases when IsBox3dTriangleCollision works
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

function TTriangleOctreeNode.ParentTree: TVRMLTriangleOctree;
begin
 Result := TVRMLTriangleOctree(InternalParentTree);
end;

function TTriangleOctreeNode.GetItems(ItemIndex: integer): POctreeItem;
begin
 result := @(ParentTree.OctreeItems.Items[ItemsIndices.Items[ItemIndex]]);
end;

{ TTriangleOctreeNode Collisions ------------------------------------------------------ }

function TTriangleOctreeNode.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    result := TTriangleOctreeNode(subnode).SphereCollision(
      pos, Radius, OctreeItemToIgnore, ItemsToIgnoreFunc);
    Stop := result <> nil;
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
var
  i: integer;
begin
  if IsLeaf then
  begin
    for i := 0 to ItemsIndices.High do
    begin
      Inc(ParentTree.DirectCollisionTestsCounter);
      Result := Items[i];
      if IsTriangleSphereCollision(Result^.Triangle,
        Result^.TriangleNormalPlane, pos, Radius) and
        (OctreeItemToIgnore <> Result) and
        ( (not Assigned(ItemsToIgnoreFunc)) or
          (not ItemsToIgnoreFunc(ParentTree, Result)) ) then
        Exit;
    end;
    Exit(nil);
  end else
  begin
    { TODO: traktujemy tu sfere jako szescian a wiec byc moze wejdziemy w wiecej
      SubNode'ow niz rzeczywiscie musimy. }
    result := nil;
    OSIS_Box[0] := VectorSubtract(pos, Vector3Single(Radius, Radius, Radius) );
    OSIS_Box[1] := VectorAdd(     pos, Vector3Single(Radius, Radius, Radius) );
    OCTREE_STEP_INTO_SUBNODES
  end;
end;

function TTriangleOctreeNode.BoxCollision(const ABox: TBox3d;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    Result := TTriangleOctreeNode(subnode).BoxCollision(ABox,
      OctreeItemToIgnore, ItemsToIgnoreFunc);
    Stop := result <> nil;
  end;

OCTREE_STEP_INTO_SUBNODES_DECLARE
var
  i: integer;
begin
  if IsLeaf then
  begin
    for i := 0 to ItemsIndices.High do
    begin
      Inc(ParentTree.DirectCollisionTestsCounter);
      Result := Items[i];
      if IsBox3dTriangleCollision(ABox, Result^.Triangle) and
        (OctreeItemToIgnore <> Result) and
        ( (not Assigned(ItemsToIgnoreFunc)) or
          (not ItemsToIgnoreFunc(ParentTree, Result)) ) then
        Exit;
    end;
    Exit(nil);
  end else
  begin
    Result := nil;
    OSIS_Box := ABox;
    OCTREE_STEP_INTO_SUBNODES
  end;
end;

function TTriangleOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
{$define SEGMENT_COLLISION}
{$I vrmltriangleoctree_raysegmentcollisions.inc}
{$undef SEGMENT_COLLISION}

function TTriangleOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
{$I vrmltriangleoctree_raysegmentcollisions.inc}

{ TVRMLTriangleOctree -------------------------------------------------------------- }

function TVRMLTriangleOctree.TreeRoot: TTriangleOctreeNode;
begin
 Result := TTriangleOctreeNode(InternalTreeRoot);
end;

function TVRMLTriangleOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 if OctreeItems.Count = 0 then
  Result :=
    '  Empty octree - no triangles defined.' +nl else
  Result := Format(
    '  %d items (=triangles) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each triangle is present in tree about %f times.' +nl,
    [ OctreeItems.Count, ItemsCount, ItemsCount / OctreeItems.Count] );
end;

{$ifdef OCTREE_ITEM_USE_MAILBOX}
function TVRMLTriangleOctree.AssignNewRayOdcTag: Int64;
begin
 result := NextFreeRayOdcTag;
 Inc(NextFreeRayOdcTag);
end;
{$endif}

procedure TVRMLTriangleOctree.AddItemTriangle(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
 if IsValidTriangle(Triangle) then
 begin
  OctreeItems.AppendItem(CreateOctreeItem(Triangle, State, GeometryNode, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd));
  TreeRoot.AddItem(OctreeItems.High);
 end;
end;

{ wszystkie deklaracje *Collision przekazywane do TreeRoot -------------------- }

{$define SegmentCollision_CommonParams :=
  const pos1, pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc
}

{$define SegmentCollision_Implementation :=
begin
  Result := TreeRoot.SegmentCollision(Intersection, IntersectionDistance,
    Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, OctreeItemToIgnore, IgnoreMarginAtStart,
    ItemsToIgnoreFunc);
end;}

  function TVRMLTriangleOctree.SegmentCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): POctreeItem;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    out Intersection: TVector3Single;
    SegmentCollision_CommonParams): POctreeItem;
  var
    IntersectionDistance: Single;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): POctreeItem;
  var
    Intersection: TVector3Single;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    SegmentCollision_CommonParams): POctreeItem;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  SegmentCollision_Implementation

{$undef SegmentCollision_CommonParams}
{$undef SegmentCollision_Implementation}

function TVRMLTriangleOctree.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
begin
  Result := TreeRoot.SphereCollision(pos, Radius,
    OctreeItemToIgnore, ItemsToIgnoreFunc);
end;

function TVRMLTriangleOctree.BoxCollision(const ABox: TBox3d;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): POctreeItem;
begin
  Result := TreeRoot.BoxCollision(ABox,
    OctreeItemToIgnore, ItemsToIgnoreFunc);
end;

{$define RayCollision_CommonParams :=
  const Ray0, RayVector: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc
}

{$define RayCollision_Implementation :=
begin
  Result := TreeRoot.RayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, OctreeItemToIgnore, IgnoreMarginAtStart,
    ItemsToIgnoreFunc);
end;}

  function TVRMLTriangleOctree.RayCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    RayCollision_CommonParams): POctreeItem;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    out Intersection: TVector3Single;
    RayCollision_CommonParams): POctreeItem;
  var
    IntersectionDistance: Single;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    out IntersectionDistance: Single;
    RayCollision_CommonParams): POctreeItem;
  var
    Intersection: TVector3Single;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    RayCollision_CommonParams): POctreeItem;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  RayCollision_Implementation

{$undef RayCollision_CommonParams}
{$undef RayCollision_Implementation}

{ TVRMLTriangleOctree.MoveAllowed methods ---------------------------------------- }

function TVRMLTriangleOctree.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const KeepWithinRootBox: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result :=
    (SegmentCollision(OldPos, ProposedNewPos, false,
      OctreeItemToIgnore, false, ItemsToIgnoreFunc) = nil) and
    (SphereCollision(ProposedNewPos, CameraRadius,
      OctreeItemToIgnore, ItemsToIgnoreFunc) = nil);

  if Result and
     KeepWithinRootBox then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(ProposedNewPos, InternalTreeRoot.Box);
end;

function TVRMLTriangleOctree.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const KeepWithinRootBox: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result :=
    (SegmentCollision(OldPos, ProposedNewPos, false,
      OctreeItemToIgnore, false, ItemsToIgnoreFunc) = nil) and
    (BoxCollision(ProposedNewBox,
      OctreeItemToIgnore, ItemsToIgnoreFunc) = nil);

  if Result and
     KeepWithinRootBox then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(ProposedNewPos, InternalTreeRoot.Box);
end;

function TVRMLTriangleOctree.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single;
  const CameraRadius: Single;
  const KeepWithinRootBox: boolean;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;

  function MoveAlongTheBlocker(Blocker: POctreeItem): boolean;
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
    PlanePtr := @(Blocker^.TriangleNormalPlane);
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
      OctreeItemToIgnore, ItemsToIgnoreFunc);
  end;

var
  Blocker: POctreeItem;
begin
  { Tests: make MoveAllowed equivalent to MoveAllowedSimple:
  Result := MoveAllowedSimple(OldPos, ProposedNewPos, CameraRadius);
  if Result then NewPos := ProposedNewPos;
  Exit; }

  Blocker := SegmentCollision(OldPos, ProposedNewPos,
    true { return closest blocker },
    OctreeItemToIgnore, false, ItemsToIgnoreFunc);
  if Blocker = nil then
  begin
    Blocker := SphereCollision(ProposedNewPos, CameraRadius,
      OctreeItemToIgnore, ItemsToIgnoreFunc);
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

procedure TVRMLTriangleOctree.GetCameraHeight(
  const CameraPos, GravityUp: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: POctreeItem;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);
var
  GroundIntersection: TVector3Single;
begin
  GroundItem := RayCollision(GroundIntersection,
    CameraPos, VectorNegate(GravityUp), true,
    OctreeItemToIgnore, false, ItemsToIgnoreFunc);
  IsAboveTheGround := GroundItem <> nil;
  if IsAboveTheGround then
    SqrHeightAboveTheGround := PointsDistanceSqr(CameraPos, GroundIntersection);
end;

procedure TVRMLTriangleOctree.GetCameraHeightZ(
  const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
  out GroundItem: POctreeItem;
  const OctreeItemToIgnore: POctreeItem;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);
const
  RayDir: TVector3Single = (0, 0, -1);
var
  GroundIntersection: TVector3Single;
begin
  GroundItem := RayCollision(GroundIntersection,
    CameraPos, RayDir, true,
    OctreeItemToIgnore, false, ItemsToIgnoreFunc);
  IsAboveTheGround := GroundItem <> nil;
  if IsAboveTheGround then
    { Calculation of HeightAboveTheGround uses the fact that RayDir is so simple. }
    HeightAboveTheGround := CameraPos[2] - GroundIntersection[2];
end;

{ Create/Destroy ------------------------------------------------------------ }

constructor TVRMLTriangleOctree.Create(const ARootBox: TBox3d);
begin
 Create(DefTriangleOctreeMaxDepth, DefTriangleOctreeLeafCapacity, ARootBox);
end;

constructor TVRMLTriangleOctree.Create(AMaxDepth, ALeafCapacity: integer;
  const ARootBox: TBox3d);
begin
 inherited Create (AMaxDepth, ALeafCapacity, ARootBox,
   TTriangleOctreeNode, false);
 OctreeItems := TDynOctreeItemsArray.Create;
end;

destructor TVRMLTriangleOctree.Destroy;
begin
 FreeAndNil(OctreeItems);
 inherited;
end;

{ TVRMLTriangleOctree.IgnoreTransparentItem ---------------------------------- }

class function TVRMLTriangleOctree.IgnoreTransparentItem(
  const Octree: TVRMLTriangleOctree;
  const OctreeItem: POctreeItem): boolean;
begin
  Result :=
    OctreeItem^.State.LastNodes.Material.Transparency(OctreeItem^.MatNum)
      > SingleEqualityEpsilon;
end;

{ --------------------------------------------------------------------------------
  przygotowane funkcje i klasy dla ItemsToIgnoreFunc: TOctreeItemIgnoreFunc.  }

function TOctreeIgnore_Transparent_And_OneItem.IgnoreItem(
  const Octree: TVRMLTriangleOctree;
  const OctreeItem: POctreeItem): boolean;
begin
  Result := (OctreeItem = OneItem) or
    (OctreeItem^.State.LastNodes.Material.Transparency(OctreeItem^.MatNum)
      > SingleEqualityEpsilon);
end;

constructor TOctreeIgnore_Transparent_And_OneItem.Create(AOneItem: POctreeItem);
begin
  inherited Create;
  OneItem := AOneItem;
end;

{ some global procs ---------------------------------------------------------- }

function ActiveLightNotBlocked(Octree: TVRMLTriangleOctree; const Light: TActiveLight;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
  const OctreeItemToIgnore: POctreeItem;
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
      3 * Box3dMaxSize(Octree.TreeRoot.Box) ) ) else
  LightPos := Light.TransfLocation;

 Result := (VectorsSamePlaneDirections(
       VectorSubtract(LightPos, LightedPoint),
       RenderDir,
       LightedPointPlane)) and
   (Octree.SegmentCollision(LightedPoint, LightPos,
     false, OctreeItemToIgnore, IgnoreMarginAtStart,
     {$ifdef FPC_OBJFPC} @ {$endif} Octree.IgnoreTransparentItem)
     = nil);
end;

end.
