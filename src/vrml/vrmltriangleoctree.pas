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

{ @abstract(@link(TVRMLTriangleOctree) class --- octree that provides
  hierarchical tree of all triangles in a VRML scene.)

  Dokladniej, mozemy
  nawet zbudowac drzewo osemkowe laczac mniejsze kawalki wielu roznych
  scen VRML'a. Elementami ktore bedziemy trzymac w lisciach drzewa sa
  rekordy TVRMLTriangle - jeden taki rekord reprezentuje jeden trojkat
  w przestrzeni. Razem z kazdym trojkatem zapamietywana jest informacja
  z jakiego State'a i Shape'a on pochodzi. Wiec pamietaj ze po zbudowaniu
  ze sceny octree scena jest praktycznie "zamrozona" - nic nie wolno
  w niej zmieniac.

  Zasadnicza klasa rekurencyjna ktora reprezentuje wezel drzewa
  (lisc = liste indeksow do TVRMLTriangle lub
   wezel wewnetrzny = 8 podwezlow TOctreeNode) jest TOctreeNode.
  Klasa TVRMLTriangleOctree to proste opakowanie na TreeRoot: TOctreeNode,
  przechowuje miedzy innymi liste Triangles (w TOctreeNode mamy tylko
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

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3D,
  KambiOctree, VRMLTriangle, Base3D;

{$define read_interface}

const
  DefTriangleOctreeMaxDepth = 10;
  DefTriangleOctreeLeafCapacity = 20;
  DefTriangleOctreeLimits: TOctreeLimits = (
    MaxDepth: DefTriangleOctreeMaxDepth;
    LeafCapacity: DefTriangleOctreeLeafCapacity
  );

{ TTriangleOctreeNode ------------------------------------------------------------------}

type
  TVRMLTriangleOctree = class;

  TTriangleOctreeNode = class(TVRMLBaseTrianglesOctreeNode)
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
  private
    function GetItems(ItemIndex: integer): PVRMLTriangle;
  public
    function ParentTree: TVRMLTriangleOctree;

    { Items zapewniaja wygodniejszy (czasami) dostep do tablicy ItemsIndices.
      Podane ItemIndex jest indeksem do tablicy ItemsIndices - wyciagamy z tego
      ParentTree.Triangles[ItemsIndices[ItemIndex]] }
    property Items[ItemIndex: integer]: PVRMLTriangle read GetItems;

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
      const pos1, pos2: TVector3Single;
      const Tag: TMailboxTag;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
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
  end;

{ TVRMLTriangleOctree ------------------------------------------------------------ }

  TVRMLTriangleOctree = class(TVRMLBaseTrianglesOctree)
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
  public
    constructor Create(const ARootBox: TBox3D); overload;
    constructor Create(const ALimits: TOctreeLimits; const ARootBox: TBox3D); overload;
    destructor Destroy; override;
  public
    { tu beda zgromadzone wszystkie Triangles jakie mamy w drzewie.
      W lisciach beda tylko ItemsIndices ktore beda indeksami do tej tablicy.
      Zrobilem to 27.04.2003 gdy zobaczylem w drzewie
      z ciasno dobranymi MaxDepth i LeafCapacity jeden trojkat sceny moze
      byc powielony az 50 000 razy ! To powodowalo zzeranie niesamowitych ilosci
      pamieci, bo rekord TVRMLTriangle jest dosc duzy i z czasem pewnie bede go
      jeszcze rozszerzal. Trzymanie wszystkich elementow w tablicy pozwala
      mi miec w lapie kazdy element tylko raz.
      - ponadto unikajac robienia TVRMLTriangle jako obiektow unikam fragmentacji
        pamieci
      - umozliwilem sobie uzycie mailboxow (for TRIANGLE_OCTREE_USE_MAILBOX)
      - umozliwiam realizowanie TriangleToIgnore w RayCollision przez szybkie
        porownywanie of a simple pointer (zamiast np. zawartosci TVRMLTriangle) }
    Triangles: TDynVRMLTriangleArray;

    function TreeRoot: TTriangleOctreeNode;

    { Add single Triangle. Automatically checks whether IsValidTriangle.
      Przed dodaniem duzej ilosci trojkatow sugerowane jest aby ustalic
      Triangles.AllowedCapacityCount na odpowiednio duza wartosc.  }
    procedure AddItemTriangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      Geometry: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  public
    { Direct collisions counter, to test octree efficiency.

      It is incremented each time XxxCollision make a direct
      collision test, that is when some single triangle is tested for collision
      with a sphere, line segment and such. The very idea of octree is to
      minimize this number.

      0 by default. }
    DirectCollisionTestsCounter: TCollisionCount;

    { Internal for cooperation with TVRMLShapeOctree.
      @exclude }
    procedure EnumerateTrianglesUpdateWorld(
      EnumerateTriangleFunc: TEnumerateTriangleFunc);

    procedure EnumerateTriangles(EnumerateTriangleFunc: TEnumerateTriangleFunc);
      override;

    function TrianglesCount: Cardinal; override;
  end;

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
    if IsBox3DTriangleCollision(SubNode.Box, Triangle^) then
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

  Triangle := @(ParentTree.Triangles.Items[ItemIndex].Loc.Triangle);

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

function TTriangleOctreeNode.ParentTree: TVRMLTriangleOctree;
begin
 Result := TVRMLTriangleOctree(InternalParentTree);
end;

function TTriangleOctreeNode.GetItems(ItemIndex: integer): PVRMLTriangle;
begin
 result := @(ParentTree.Triangles.Items[ItemsIndices.Items[ItemIndex]]);
end;

{ TTriangleOctreeNode Collisions ------------------------------------------------------ }

function TTriangleOctreeNode.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonSphereLeaf(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.High do
  begin
    Inc(ParentTree.DirectCollisionTestsCounter);
    Result := Items[i];
    if IsTriangleSphereCollision(Result^.Loc.Triangle,
      Result^.Loc.Plane, pos, Radius) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsSphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := SphereCollision(pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.BoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonBoxLeaf(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.High do
  begin
    Inc(ParentTree.DirectCollisionTestsCounter);
    Result := Items[i];
    if IsBox3DTriangleCollision(ABox, Result^.Loc.Triangle) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsBoxCollision(const ABox: TBox3D;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := BoxCollision(ABox, TriangleToIgnore, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.SegmentCollision(
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

function TTriangleOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
{$define SEGMENT_COLLISION}
{$I vrmltriangleoctree_raysegmentcollisions.inc}
{$undef SEGMENT_COLLISION}

function TTriangleOctreeNode.IsSegmentCollision(
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
  Result := SegmentCollision(Intersection, IntersectionDistance, Pos1, Pos2,
    Tag,
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.RayCollision(
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

function TTriangleOctreeNode.CommonRayLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const Tag: TMailboxTag;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
{$I vrmltriangleoctree_raysegmentcollisions.inc}

function TTriangleOctreeNode.IsRayCollision(
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
  Result := RayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    Tag,
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

{ TVRMLTriangleOctree -------------------------------------------------------- }

constructor TVRMLTriangleOctree.Create(const ARootBox: TBox3D);
begin
 Create(DefTriangleOctreeLimits, ARootBox);
end;

constructor TVRMLTriangleOctree.Create(const ALimits: TOctreeLimits;
  const ARootBox: TBox3D);
begin
 inherited Create (ALimits, ARootBox, TTriangleOctreeNode, false);
 Triangles := TDynVRMLTriangleArray.Create;
end;

destructor TVRMLTriangleOctree.Destroy;
begin
 FreeAndNil(Triangles);
 inherited;
end;

function TVRMLTriangleOctree.TreeRoot: TTriangleOctreeNode;
begin
 Result := TTriangleOctreeNode(InternalTreeRoot);
end;

function TVRMLTriangleOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 Result := nl+
   Format(
   '  Octree constructed with limits: max depth %d, leaf capacity %d.',
   [MaxDepth, LeafCapacity]) + nl + nl;

 if Triangles.Count = 0 then
  Result +=
    '  Empty octree - no triangles defined.' +nl else
  Result += Format(
    '  %d items (=triangles) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each triangle is present in tree about %f times.' +nl,
    [ Triangles.Count, ItemsCount, ItemsCount / Triangles.Count] );
end;

procedure TVRMLTriangleOctree.AddItemTriangle(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; Geometry: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  if IsValidTriangle(Triangle) then
  begin
    Triangles.Add^.Init(
      Triangle, State, Geometry, MatNum,
      FaceCoordIndexBegin, FaceCoordIndexEnd);
    TreeRoot.AddItem(Triangles.High);
  end;
end;

procedure TVRMLTriangleOctree.EnumerateTrianglesUpdateWorld(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
  T: PVRMLTriangle;
begin
  T := PVRMLTriangle(Triangles.ItemsArray);
  for I := 0 to Triangles.Count - 1 do
  begin
    T^.UpdateWorld;
    EnumerateTriangleFunc(T);
  end;
end;

procedure TVRMLTriangleOctree.EnumerateTriangles(
  EnumerateTriangleFunc: TEnumerateTriangleFunc);
var
  I: Integer;
begin
  for I := 0 to Triangles.Count - 1 do
    EnumerateTriangleFunc(Triangles.Pointers[I]);
end;

function TVRMLTriangleOctree.TrianglesCount: Cardinal;
begin
  Result := Triangles.Count;
end;

end.
