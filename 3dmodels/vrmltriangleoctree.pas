{
  Copyright 2003-2007 Michalis Kamburelis.

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

{ zdefiniuj OCTREE_ITEM_USE_MAILBOX aby w TryOctreeItem*Collision uzywac
  wlasciwosci Mailbox* rekordow TOctreeItem. W ten sposob mozemy uniknac
  wielokrotnego sprawdzania przeciecia tego samego trojkata z tym samym
  promieniem na skutek tego ze jeden trojkat moze trafic do wielu lisci.

  Kiedys zamiast MailboxSavedTag mialem MailboxState (empty, ray lub segment)
  i MailboxVector1, MailboxVector2 (:TVector3Single) ktore razem okreslaly
  co jest zapamietane w Mailboxie za pomoca 6*SizeOf(Single) + SizeOf(enum) =
  = 7*4 = 28 bajtow. Teraz to wszystko zamienilem na MailboxSaveTag: Int64
  a wiec 8 bajtow a wiec istotna oszczednosc pamieci i przede wszystkim
  czasu zarzadzania mailboxem (czyli czasu porownywania "czy mozemy
  uzyc maiboxa" i czasu uaktualniania mailboxa).

  Patrz 3dmodels/rayhunter-demos/new-mailbox/new-mailbox-raport.txt :
  przeciêtny zysk czasowy ze zdefiniowania tego symbolu dla drzew
  o¶emkowych w rodzaju max-depth = 10 i leaf-capacity = 20 wynosi
  1.09 (tzn.  stary czas dzia³ania / nowy czas dzia³ania = 1.09) }
{$define OCTREE_ITEM_USE_MAILBOX}

interface

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3d, Math,
  KambiOctree;

{$define read_interface}

const
  DefTriangleOctreeMaxDepth = 10;
  DefTriangleOctreeLeafCapacity = 20;

{ TOctreeItem  ------------------------------------------------------------ }

type
  TOctreeItemMailboxState = (msEmpty, msRay, msSegmentDir);
  TCollisionCount = Int64;

  { This is a single item of a triangle octree.
    In other words, this is really just a triangle with a lot
    of associated information.

    @bold(Never modify fields of this record directly) ---
    this record should be created only by CreateOctreeItem and
    modified by other routines in this unit. }
  TOctreeItem = record
    Triangle: TTriangle3Single;

    { Calculated TriangleArea(Triangle) }
    TriangleArea: Single;

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
      is a TNodeSphere) than both FaceCoordIndex* are -1. }
    FaceCoordIndexBegin, FaceCoordIndexEnd: Integer;

    {$ifdef OCTREE_ITEM_USE_MAILBOX}
    { MailboxSavedTag okresla tag elementu z ktorym mamy zapamietane przeciecie
      w MailboxIsIntersection i MailboxIntersection i MailboxIntersectionDistance.
      Aby wszystko dzialalo 100%
      poprawnie zakladamy ze kazdy segment i kazdy promien z jakim bedziemy
      testowali kolizje z octree beda mialy inne tagi i zaden z nich nie bedzie
      mial taga = -1. W praktyce oznacza to ze nastepujace zachowanie jest
      poprawne : inicjujemy kazdemu nowemu elementowi OctreeItem MailboxSavedTag
      na -1 a kolejnym odcinkom i promieniom przydzielami kolejne Tagi
      zaczynajac od 0. }
    MailboxSavedTag: Int64;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    {$endif}

    case Integer of
      0: ({ This is calculated TriangleNormPlane(Triangle) czyli pierwsze
            trzy wspolrzedne TriNormPlane daja wektor normalny o dlug. 1 z trojkata.
            W ten sposob mamy tu przeliczony juz i plane trojkata
            i jego wektor normalny. }
          TriangleNormalPlane: TVector4Single;);
      1: (TriangleNormal: TVector3Single;);
  end;
  POctreeItem = ^TOctreeItem;

  TDynArrayItem_1 = TOctreeItem;
  PDynArrayItem_1 = POctreeItem;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynOctreeItemsArray = TDynArray_1;

{ podany Triangle musi byc IsValidTriangle }
function CreateOctreeItem(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer): TOctreeItem;

{ Sprawdzaj przeciecia z elementem octree item tylko przez
  TryOctreeItemRay/Segment Collision zeby umozliwic uzywanie metody
  skrzynek pocztowych do zapamietywania ostatnio badanego przeciecia
  (co moze sprawic ze Ray/SegmentCollision beda dzialac duzo szybciej dla
  scen gdzie jeden octree item bedzie czesto trafial do wielu lisci).

  Zwieksza o jeden DirectCollisionTestsCounter jezeli wykonalismy rzeczywisty
  test na przeciecie, czyli jesli nie moglismy skorzystac z wyniku
  w skrzynce.

  Jezeli nie jest zdefiniowane OCTREE_ITEM_USE_MAILBOX to nigdy nie uzywa
  skrzynki i zawsze inkrementuje DirectCollisionTestsCounter. }
function TryOctreeItemSegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Odc0, OdcVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;

function TryOctreeItemRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;

{ TOctreeNode ------------------------------------------------------------------}

const NoItemIndex = -1;

type
  TVRMLTriangleOctree = class;

  { zwraca dla zadanego indeksu w Octree.OctreeItems czy chcemy zignorowac
    kolizje z nim }
  TOctreeItemIgnoreFunc = function(Octree: TVRMLTriangleOctree;
    OctreeItemIndex: Integer): boolean of object;

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
      const OctreeItemIndexToIgnore: integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;

    function BoxCollision(const ABox: TBox3d;
      const OctreeItemIndexToIgnore: integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): Integer;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;
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
        porownywanie indeksow (zamiast np. zawartosci TOctreeItem) }
    OctreeItems: TDynOctreeItemsArray;

    function TreeRoot: TTriangleOctreeNode;

    { Add single OctreeItem. Automatically checks whether IsValidTriangle.
      Przed dodaniem duzej ilosci trojkatow sugerowane jest aby ustalic
      OctreeItems.AllowedCapacityCount na odpowiednio duza wartosc.  }
    procedure AddItemTriangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);

    { you can use variable below for testing purposes. It is increemented each
      time SphereCollision or SegmentCollision or RayCollision makes a direct
      collision test, that is when some single triangle is tested for collision
      with a sphere, line segment or ray. When we use octree we expect that this
      number will be small. }
    DirectCollisionTestsCounter: TCollisionCount; { = 0 }

    { Ponizej mamy najwazniejsze procedury ktore wykorzystuja cala
      strukture jaka zbudowalismy zeby efektywnie badac kolizje.

      SegmentCollision bada czy segment nie przecina zadnego elementu drzewa

      SphereCollision bada czy sfera nie zawiera w sobie choc czesci jakiegos
      elementu drzewa.

      RayCollision bada przeciecie promienia z drzewem.

      Procedury zwracaja -1 (co preferujemy wyrazac jako NoItemIndex)
      jesli nie ma kolizji lub indeks do tablicy OctreeItems
      na element z ktorym jest kolizja. Pytanie brzmi "ktore przeciecie
      zwrocic jesli jest ich wiele ?". SphereCollision zwraca ktorykolwiek,
      podobnie jak Ray/SegmentCollision gdy (not ReturnClosestIntersection).
      Gdy ReturnClosestIntersection = true to RayCollision zwraca przeciecie
      najblizsze Ray0, SegmentCollision najblizsze pos1.
      (pamietaj ze placisz czasem wykonania
      za przekazanie ReturnClosestIntersection = true, wiec unikaj tego).

      Segment/RayCollision uwzgledniaja ze na pewno NIE MA przeciecia z elementem
      OctreeItemIndexToIgnore, podaj OctreeItemIndexToIgnore = NoItemIndex
      aby uwzglednial wszystkie elementy (przydatne przy rekurencyjnym
      ray-tracingu gdy nie chcesz zeby promien odbity/zalamany/cienia omylkowo trafil na
      powierzchnie z ktorej wlasnie "wychodzisz" - mozna by bylo temu
      zaradzic tez przez nieznacznie przesuwanie Ray0, ale niniejsza metoda
      jest duzo bardziej elegancka). OctreeItemIndexToIgnore to
      naturalnie indeks do tablicy OctreeItems, podobnie jak wynik wszystkich
      tych funkcji *Collision i podobnie jak elementy TTriangleOctreeNode.ItemsIndices[]

      Uwzgledniaja tez ze na pewno nie ma przeciecia z elementami dla
      ktorych ItemsToIgnoreFunc zwroci true (mozesz przekazac nil
      aby nie ignorowac nic na podstawie ItemsToIgnoreFunc).

      Ponadto jesli podasz IgnoreMarginAtStart to beda ignorowac przeciecia ktore
      zdarzyly sie *bardzo* blisko Ray0 (lub Pos1). W ten sposob raytracer
      bedzie w stanie poradzic sobie nawet ze scenami ktore maja nieprawidlowo
      zdefiniowane (czesciowo zachodzace na siebie) polygony. Takie polygony
      normalnie generowalyby zbedne cienie (zaslanialyby sie nawzajem).
      Pozornie podajac IgnoreMarginAtStart = true raytracer moglby czesto
      nie podawac juz OctreeItemIndexToIgnore (tzn. podac je = NoItemIndex),
      bo przeciez po to sie zazwyczaj podaje OctreeItemIndexToIgnore.
      Ale prawda jest taka ze IgnoreMarginAtStart nie daje 100% pewnosci
      ze unikniemy kolizji z elementem od ktorego zaczelismy (bo on przeciez
      tylko unika pewnego *malego* marginesu wokol Ray0). W rezultacie
      i tak nalezy uzywac OctreeItemIndexToIgnore. To raczej podawanie
      IgnoreMarginAtStart = true jest zbedne, ale niestety jest to pozadane
      i daje dobre efekty gdy przychodzi do nieprawidlowo zbudowanych scen.
      A nawet sibenik.3ds i office.mgf.wrl a wiec sceny zrobione niby porzadnie
      ktorych uzywalem do zasadniczych testow na rayhunterze
      maja gdzeniegdzie tak nieprawidlowo zbudowane sciany.

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
    }
    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function SegmentCollision(
      out Intersection: TVector3Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function SegmentCollision(
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function SegmentCollision(
      const pos1, pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function SphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const OctreeItemIndexToIgnore: integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;

    function BoxCollision(const ABox: TBox3d;
      const OctreeItemIndexToIgnore: integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): Integer;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function RayCollision(
      out Intersection: TVector3Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    function RayCollision(const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const OctreeItemIndexToIgnore: integer;
      const IgnoreMarginAtStart: boolean;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer; overload;

    { This checks if move between OldPos and ProposedNewPos is possible,
      by checking is segment between OldPos and ProposedNewPos free
      and sphere (with radius CameraRadius) ProposedNewPos is free.

      CameraRadius must obviously be > 0.

      See @link(MoveAllowed) for some more sophisticated way of
      collision detection.

      If KeepWithinRootBox then it will additionally make sure that
      user stays within the whole octree box. That is, moving outside
      of the RootNode.Box will be disallowed.

      OctreeItemIndexToIgnore and ItemsToIgnoreFunc meaning
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
      const OctreeItemIndexToIgnore: integer = NoItemIndex;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc = nil): boolean;

    { This is like @link(MoveAllowedSimple), but it checks for collision
      around ProposedNewPos using TBox3d instead of a sphere. }
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const KeepWithinRootBox: boolean;
      const OctreeItemIndexToIgnore: integer = NoItemIndex;
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

      OctreeItemIndexToIgnore and ItemsToIgnoreFunc meaning
      is just like for RayCollision.

      @seealso(TWalkNavigator.DoMoveAllowed
        TWalkNavigator.DoMoveAllowed is some place
        where you can use this function) }
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const CameraRadius: Single;
      const KeepWithinRootBox: boolean;
      const OctreeItemIndexToIgnore: integer = NoItemIndex;
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
      sound accordingly). If IsAboveTheGround then for sure GroundItemIndex
      <> NoItemIndex.

      OctreeItemIndexToIgnore and ItemsToIgnoreFunc meaning
      is just like for RayCollision. }
    procedure GetCameraHeight(
      const CameraPos, GravityUp: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItemIndex: Integer;
      const OctreeItemIndexToIgnore: integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);

    { This is just like GetCameraHeight, but it assumes that
      GravityUp = (0, 0, 1) and it returns the actual
      HeightAboveTheGround (not it's square). Thanks to the fact that
      calculating HeightAboveTheGround doesn't require costly Sqrt operation
      in case of such simple GravityUp. }
    procedure GetCameraHeightZ(
      const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
      out GroundItemIndex: Integer;
      const OctreeItemIndexToIgnore: Integer;
      const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);

    { This makes transparent triangles (with material
      with Transparency > 0) ignored (i.e. returns @true for them).
      This is a prepared function compatible with TOctreeItemIgnoreFunc
      function type. }
    class function IgnoreTransparentItem(
      Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean;

    constructor Create(const ARootBox: TBox3d); overload;
    constructor Create(AMaxDepth, ALeafCapacity: integer;
      const ARootBox: TBox3d); overload;
    destructor Destroy; override;
  end;

  { przygotowane funkcje i klasy dla ItemsToIgnoreFunc: TOctreeItemIgnoreFunc. }

  TOctreeIgnore_Transparent_And_OneItem = class
    OneItemIndex: Integer;
    { IgnoreItem zwraca true dla obiektow ktorych Transparency > 0. }
    function IgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean;
    constructor Create(AOneItemIndex: Integer);
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

  Szukanie kolizji w octree uzywa przekazanych OctreeItemIndexToIgnore i
  IgnoreMarginAtStart - zazwyczaj powinienes je przekazac na indeks elementu
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
  OctreeItemIndexToIgnore: Integer; IgnoreMarginAtStart: boolean): boolean;

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

{$I kambioctreemacros.inc}

{ TOctreeItem  ------------------------------------------------------------ }

function CreateOctreeItem(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer): TOctreeItem;
begin
 result.Triangle := Triangle;
 result.TriangleNormalPlane := TriangleNormPlane(Triangle);
 result.TriangleArea := TriangleArea(Triangle);

 result.State := State;
 result.GeometryNode := GeometryNode;
 result.MatNum := MatNum;
 result.FaceCoordIndexBegin := FaceCoordIndexBegin;
 result.FaceCoordIndexEnd := FaceCoordIndexEnd;

 {$ifdef OCTREE_ITEM_USE_MAILBOX}
 result.MailboxSavedTag := -1;
 {$endif}
end;

function TryOctreeItemSegmentDirCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Odc0, OdcVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const OdcTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
 {$ifdef OCTREE_ITEM_USE_MAILBOX}
 if OctreeItem.MailboxSavedTag = OdcTag then
 begin
  result := OctreeItem.MailboxIsIntersection;
  if result then
  begin
   Intersection         := OctreeItem.MailboxIntersection;
   IntersectionDistance := OctreeItem.MailboxIntersectionDistance;
  end;
 end else
 begin
 {$endif}

  Result := TryTriangleSegmentDirCollision(
    Intersection, IntersectionDistance,
    OctreeItem.Triangle, OctreeItem.TriangleNormalPlane,
    Odc0, OdcVector);
  Inc(DirectCollisionTestsCounter);

 {$ifdef OCTREE_ITEM_USE_MAILBOX}
  { zapisz wyniki do mailboxa }
  with OctreeItem do
  begin
   MailboxSavedTag := OdcTag;
   MailboxIsIntersection := result;
   if result then
   begin
    MailboxIntersection         := Intersection;
    MailboxIntersectionDistance := IntersectionDistance;
   end;
  end;
 end;
 {$endif}
end;

function TryOctreeItemRayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  var OctreeItem: TOctreeItem; const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayTag: Int64; {$endif}
  var DirectCollisionTestsCounter: TCollisionCount): boolean;
begin
 { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
   z jednego Ray0 ale z roznym RayVector (np. w raytracerze). Wiec lepiej
   najpierw porownywac przechowywane w skrzynce RayVector (niz Ray0)
   zeby moc szybciej stwierdzic niezgodnosc. }
 {$ifdef OCTREE_ITEM_USE_MAILBOX}
 if OctreeItem.MailboxSavedTag = RayTag then
 begin
  result := OctreeItem.MailboxIsIntersection;
  if result then
  begin
   Intersection         := OctreeItem.MailboxIntersection;
   IntersectionDistance := OctreeItem.MailboxIntersectionDistance;
  end;
 end else
 begin
 {$endif}

  result := TryTriangleRayCollision(
    Intersection, IntersectionDistance,
    OctreeItem.Triangle, OctreeItem.TriangleNormalPlane,
    Ray0, RayVector);
  Inc(DirectCollisionTestsCounter);

 {$ifdef OCTREE_ITEM_USE_MAILBOX}
  { zapisz wyniki do mailboxa }
  with OctreeItem do
  begin
   MailboxSavedTag := RayTag;
   MailboxIsIntersection := result;
   if result then
   begin
    MailboxIntersection         := Intersection;
    MailboxIntersectionDistance := IntersectionDistance;
   end;
  end;
 end;
 {$endif}
end;

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
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    result := TTriangleOctreeNode(subnode).SphereCollision(
      pos, Radius, OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
    Stop := result <> NoItemIndex;
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
      if IsTriangleSphereCollision(Items[i]^.Triangle,
        Items[i]^.TriangleNormalPlane, pos, Radius) and
        (OctreeItemIndexToIgnore <> ItemsIndices[I]) and
        ( (not Assigned(ItemsToIgnoreFunc)) or
          (not ItemsToIgnoreFunc(ParentTree, ItemsIndices[I])) ) then
        Exit(ItemsIndices[i]);
    end;
    Exit(NoItemIndex);
  end else
  begin
    { TODO: traktujemy tu sfere jako szescian a wiec byc moze wejdziemy w wiecej
      SubNode'ow niz rzeczywiscie musimy. }
    result := NoItemIndex;
    OSIS_Box[0] := VectorSubtract(pos, Vector3Single(Radius, Radius, Radius) );
    OSIS_Box[1] := VectorAdd(     pos, Vector3Single(Radius, Radius, Radius) );
    OCTREE_STEP_INTO_SUBNODES
  end;
end;

function TTriangleOctreeNode.BoxCollision(const ABox: TBox3d;
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): Integer;

  procedure OCTREE_STEP_INTO_SUBNODES_PROC(subnode: TOctreeNode; var Stop: boolean);
  begin
    Result := TTriangleOctreeNode(subnode).BoxCollision(ABox,
      OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
    Stop := result <> NoItemIndex;
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
      if IsBox3dTriangleCollision(ABox, Items[i]^.Triangle) and
        (OctreeItemIndexToIgnore <> ItemsIndices[I]) and
        ( (not Assigned(ItemsToIgnoreFunc)) or
          (not ItemsToIgnoreFunc(ParentTree, ItemsIndices[I])) ) then
        Exit(ItemsIndices[i]);
    end;
    Exit(NoItemIndex);
  end else
  begin
    Result := NoItemIndex;
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
  const OctreeItemIndexToIgnore: integer;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;
{$define SEGMENT_COLLISION}
{$I vrmltriangleoctree_raysegmentcollisions.inc}
{$undef SEGMENT_COLLISION}

function TTriangleOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const OctreeItemIndexToIgnore: integer;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;
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
  const OctreeItemIndexToIgnore: integer;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc
}

{$define SegmentCollision_Implementation :=
begin
  Result := TreeRoot.SegmentCollision(Intersection, IntersectionDistance,
    Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, OctreeItemIndexToIgnore, IgnoreMarginAtStart,
    ItemsToIgnoreFunc);
end;}

  function TVRMLTriangleOctree.SegmentCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): integer;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    out Intersection: TVector3Single;
    SegmentCollision_CommonParams): integer;
  var
    IntersectionDistance: Single;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    out IntersectionDistance: Single;
    SegmentCollision_CommonParams): integer;
  var
    Intersection: TVector3Single;
  SegmentCollision_Implementation

  function TVRMLTriangleOctree.SegmentCollision(
    SegmentCollision_CommonParams): integer;
  var
    Intersection: TVector3Single;
    IntersectionDistance: Single;
  SegmentCollision_Implementation

{$undef SegmentCollision_CommonParams}
{$undef SegmentCollision_Implementation}

function TVRMLTriangleOctree.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;
begin
  Result := TreeRoot.SphereCollision(pos, Radius,
    OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
end;

function TVRMLTriangleOctree.BoxCollision(const ABox: TBox3d;
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): integer;
begin
  Result := TreeRoot.BoxCollision(ABox,
    OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
end;

{$define RayCollision_CommonParams :=
  const Ray0, RayVector: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const OctreeItemIndexToIgnore: integer;
  const IgnoreMarginAtStart: boolean;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc
}

{$define RayCollision_Implementation :=
begin
  Result := TreeRoot.RayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} AssignNewRayOdcTag, {$endif}
    ReturnClosestIntersection, OctreeItemIndexToIgnore, IgnoreMarginAtStart,
    ItemsToIgnoreFunc);
end;}

  function TVRMLTriangleOctree.RayCollision(
    out Intersection: TVector3Single;
    out IntersectionDistance: Single;
    RayCollision_CommonParams): integer;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    out Intersection: TVector3Single;
    RayCollision_CommonParams): integer;
  var
    IntersectionDistance: Single;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    out IntersectionDistance: Single;
    RayCollision_CommonParams): integer;
  var
    Intersection: TVector3Single;
  RayCollision_Implementation

  function TVRMLTriangleOctree.RayCollision(
    RayCollision_CommonParams): integer;
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
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result :=
    (SegmentCollision(OldPos, ProposedNewPos, false,
      OctreeItemIndexToIgnore, false, ItemsToIgnoreFunc) = NoItemIndex) and
    (SphereCollision(ProposedNewPos, CameraRadius,
      OctreeItemIndexToIgnore, ItemsToIgnoreFunc) = NoItemIndex);

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
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;
begin
  Result :=
    (SegmentCollision(OldPos, ProposedNewPos, false,
      OctreeItemIndexToIgnore, false, ItemsToIgnoreFunc) = NoItemIndex) and
    (BoxCollision(ProposedNewBox,
      OctreeItemIndexToIgnore, ItemsToIgnoreFunc) = NoItemIndex);

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
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc): boolean;

  function MoveAlongTheBlocker(BlockerIndex: Integer): boolean;
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
   PlanePtr := @OctreeItems.Items[BlockerIndex].TriangleNormalPlane;
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
     BlockerIndex, I must check whether it's not blocked by something else
     (e.g. if player is trying to walk into the corner (two walls)).
     I can do it by using my simple MoveAllowedSimple. }

   Result := MoveAllowedSimple(OldPos, NewPos, CameraRadius, false,
     OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
  end;

var
  BlockerIndex: Integer;
begin
 { Tests: make MoveAllowed equivalent to MoveAllowedSimple:
 Result := MoveAllowedSimple(OldPos, ProposedNewPos, CameraRadius);
 if Result then NewPos := ProposedNewPos;
 Exit; }

 BlockerIndex := SegmentCollision(OldPos, ProposedNewPos,
   true { return closest blocker },
   OctreeItemIndexToIgnore, false, ItemsToIgnoreFunc);
 if BlockerIndex = NoItemIndex then
 begin
  BlockerIndex := SphereCollision(ProposedNewPos, CameraRadius,
    OctreeItemIndexToIgnore, ItemsToIgnoreFunc);
  if BlockerIndex = NoItemIndex then
  begin
   Result := true;
   NewPos := ProposedNewPos;
  end else
   Result := MoveAlongTheBlocker(BlockerIndex);
 end else
  Result := MoveAlongTheBlocker(BlockerIndex);

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
  out GroundItemIndex: Integer;
  const OctreeItemIndexToIgnore: integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);
var
  GroundIntersection: TVector3Single;
begin
  GroundItemIndex := RayCollision(GroundIntersection,
    CameraPos, VectorNegate(GravityUp), true,
    OctreeItemIndexToIgnore, false, ItemsToIgnoreFunc);
  IsAboveTheGround := GroundItemIndex <> NoItemIndex;
  if IsAboveTheGround then
    SqrHeightAboveTheGround := PointsDistanceSqr(CameraPos, GroundIntersection);
end;

procedure TVRMLTriangleOctree.GetCameraHeightZ(
  const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out HeightAboveTheGround: Single;
  out GroundItemIndex: Integer;
  const OctreeItemIndexToIgnore: Integer;
  const ItemsToIgnoreFunc: TOctreeItemIgnoreFunc);
const
  RayDir: TVector3Single = (0, 0, -1);
var
  GroundIntersection: TVector3Single;
begin
  GroundItemIndex := RayCollision(GroundIntersection,
    CameraPos, RayDir, true,
    OctreeItemIndexToIgnore, false, ItemsToIgnoreFunc);
  IsAboveTheGround := GroundItemIndex <> NoItemIndex;
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
  Octree: TVRMLTriangleOctree;
  OctreeItemIndex: Integer): boolean;
var ItemPtr: POctreeItem;
begin
 ItemPtr := @(Octree.OctreeItems.Items[OctreeItemIndex]);
 result := ItemPtr^.State.LastNodes.Material.Transparency(ItemPtr^.MatNum)
   > SingleEqualityEpsilon;
end;

{ --------------------------------------------------------------------------------
  przygotowane funkcje i klasy dla ItemsToIgnoreFunc: TOctreeItemIgnoreFunc.  }

function TOctreeIgnore_Transparent_And_OneItem.IgnoreItem(Octree: TVRMLTriangleOctree; OctreeItemIndex: Integer): boolean;
var ItemPtr: POctreeItem;
begin
 if OctreeItemIndex = OneItemIndex then Exit(true);
 ItemPtr := @(Octree.OctreeItems.Items[OctreeItemIndex]);
 result := ItemPtr^.State.LastNodes.Material.Transparency(ItemPtr^.MatNum)
   > SingleEqualityEpsilon;
end;

constructor TOctreeIgnore_Transparent_And_OneItem.Create(AOneItemIndex: Integer);
begin
 inherited Create;
 OneItemIndex := AOneItemIndex;
end;

{ some global procs ---------------------------------------------------------- }

function ActiveLightNotBlocked(Octree: TVRMLTriangleOctree; const Light: TActiveLight;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3Single;
  OctreeItemIndexToIgnore: Integer; IgnoreMarginAtStart: boolean): boolean;
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
     false, OctreeItemIndexToIgnore, IgnoreMarginAtStart,
     {$ifdef FPC_OBJFPC} @ {$endif} Octree.IgnoreTransparentItem)
     = NoItemIndex);
end;

end.
