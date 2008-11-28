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

uses VectorMath, SysUtils, KambiUtils, VRMLNodes, Boxes3d, Math,
  KambiOctree, VRMLTriangle;

{$define read_interface}

const
  DefTriangleOctreeMaxDepth = 10;
  DefTriangleOctreeLeafCapacity = 20;

{ TTriangleOctreeNode ------------------------------------------------------------------}

type
  TVRMLTriangleOctree = class;

  TTriangleOctreeNode = class(TVRMLBaseTrianglesOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;

    function CommonSphereLeaf(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonBoxLeaf(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonSegmentLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function CommonRayLeaf(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;
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
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsSphereCollision(const pos: TVector3Single;
      const Radius: Single;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function BoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsBoxCollision(const ABox: TBox3d;
      const TriangleToIgnore: PVRMLTriangle;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function SegmentCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsSegmentCollision(
      const pos1, pos2: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;

    function RayCollision(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle; override;

    function IsRayCollision(
      const Ray0, RayVector: TVector3Single;
      {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
  end;

{ TVRMLTriangleOctree ------------------------------------------------------------ }

  TVRMLTriangleOctree = class(TVRMLBaseTrianglesOctree)
  protected
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string; override;
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
      - umozliwilem sobie zrobienie OCTREE_ITEM_USE_MAILBOX
      - umozliwiam realizowanie TriangleToIgnore w RayCollision przez szybkie
        porownywanie of a simple pointer (zamiast np. zawartosci TVRMLTriangle) }
    Triangles: TDynVRMLTriangleArray;

    function TreeRoot: TTriangleOctreeNode;

    { Add single Triangle. Automatically checks whether IsValidTriangle.
      Przed dodaniem duzej ilosci trojkatow sugerowane jest aby ustalic
      Triangles.AllowedCapacityCount na odpowiednio duza wartosc.  }
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

    constructor Create(const ARootBox: TBox3d); overload;
    constructor Create(AMaxDepth, ALeafCapacity: integer;
      const ARootBox: TBox3d); overload;
    destructor Destroy; override;
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

  Triangle := @(ParentTree.Triangles.Items[ItemIndex].Loc.Triangle);

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

function TTriangleOctreeNode.GetItems(ItemIndex: integer): PVRMLTriangle;
begin
 result := @(ParentTree.Triangles.Items[ItemsIndices.Items[ItemIndex]]);
end;

{ TTriangleOctreeNode Collisions ------------------------------------------------------ }

function TTriangleOctreeNode.SphereCollision(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonSphere(pos, Radius, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonSphereLeaf(const pos: TVector3Single;
  const Radius: Single;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
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
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := SphereCollision(pos, Radius, TriangleToIgnore,
    TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.BoxCollision(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonBox(ABox, TriangleToIgnore, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonBoxLeaf(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
var
  i: integer;
begin
  for i := 0 to ItemsIndices.High do
  begin
    Inc(ParentTree.DirectCollisionTestsCounter);
    Result := Items[i];
    if IsBox3dTriangleCollision(ABox, Result^.Loc.Triangle) and
      (TriangleToIgnore <> Result) and
      ( (not Assigned(TrianglesToIgnoreFunc)) or
        (not TrianglesToIgnoreFunc(ParentTree, Result)) ) then
      Exit;
  end;
  Exit(nil);
end;

function TTriangleOctreeNode.IsBoxCollision(const ABox: TBox3d;
  const TriangleToIgnore: PVRMLTriangle;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := BoxCollision(ABox, TriangleToIgnore, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.SegmentCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonSegment(
    Intersection, IntersectionDistance, Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
    ReturnClosestIntersection, TriangleToIgnore,
    IgnoreMarginAtStart, TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonSegmentLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
{$define SEGMENT_COLLISION}
{$I vrmltriangleoctree_raysegmentcollisions.inc}
{$undef SEGMENT_COLLISION}

function TTriangleOctreeNode.IsSegmentCollision(
  const Pos1, Pos2: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  Result := SegmentCollision(Intersection, IntersectionDistance, Pos1, Pos2,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

function TTriangleOctreeNode.RayCollision(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
begin
  Result := CommonRay(
    Intersection, IntersectionDistance, Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
    ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
    TrianglesToIgnoreFunc);
end;

function TTriangleOctreeNode.CommonRayLeaf(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): PVRMLTriangle;
{$I vrmltriangleoctree_raysegmentcollisions.inc}

function TTriangleOctreeNode.IsRayCollision(
  const Ray0, RayVector: TVector3Single;
  {$ifdef OCTREE_ITEM_USE_MAILBOX} const RayOdcTag: Int64; {$endif}
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  { We just ignore returned Intersection, IntersectionDistance }
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  Result := RayCollision(Intersection, IntersectionDistance,
    Ray0, RayVector,
    {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
    {ReturnClosestIntersection}false,
    TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc) <> nil;
end;

{ TVRMLTriangleOctree -------------------------------------------------------------- }

function TVRMLTriangleOctree.TreeRoot: TTriangleOctreeNode;
begin
 Result := TTriangleOctreeNode(InternalTreeRoot);
end;

function TVRMLTriangleOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 if Triangles.Count = 0 then
  Result :=
    '  Empty octree - no triangles defined.' +nl else
  Result := Format(
    '  %d items (=triangles) defined for octree, %d items in octree''s nodes' +nl+
    '  - so each triangle is present in tree about %f times.' +nl,
    [ Triangles.Count, ItemsCount, ItemsCount / Triangles.Count] );
end;

procedure TVRMLTriangleOctree.AddItemTriangle(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  if IsValidTriangle(Triangle) then
  begin
    Triangles.IncLength;
    Triangles.Items[Triangles.High].Init(
      Triangle, State, GeometryNode, MatNum,
      FaceCoordIndexBegin, FaceCoordIndexEnd);
    TreeRoot.AddItem(Triangles.High);
  end;
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
 Triangles := TDynVRMLTriangleArray.Create;
end;

destructor TVRMLTriangleOctree.Destroy;
begin
 FreeAndNil(Triangles);
 inherited;
end;

end.
