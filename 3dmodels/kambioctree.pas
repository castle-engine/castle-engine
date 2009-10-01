{
  Copyright 2003-2005,2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*@abstract(This unit provides common utilities and classes for making
  octrees, used in @link(VRMLTriangleOctree) and @link(VRMLShapeOctree).)

  It provides class @link(TOctree) that holds some settings "global"
  for whole octree (like MaxDepth etc.) and it holds reference to
  root node of the tree, @link(TOctreeNode), in the protected property
  InternalTreeRoot.

  Each @link(TOctreeNode) is either
  @unorderedList(
    @item(a leaf: then it stores a list
      of Integers in ItemsIndices array --- these integers are usuallly
      indexes to some array, but for the sake of this unit they are
      just some integers that uniquely describe items that you want
      to have in octree leafs.
      The key of this unit is that it doesn't specify what
      items you want to put in your octree leaf.)

    @item(or not a leaf: then it has 8 children stored in TreeSubNodes.)
  )

  Each @link(TOctreeNode) also has some essential properties like
  Box, MiddlePoint and ParentTree.

  Using this unit it's very easy to create real, non-abstract octree
  classes. Typical schema looks like this;
  @longcode(#
type
  TMyOctree = class;

  TMyOctreeNode = class(TOctreeNode)
  protected
    procedure PutItemIntoSubNodes(ItemIndex: integer); override;
  public
    function ParentTree: TMyOctree;
 end;

  TMyOctree = class(TOctree)
  public
    constructor Create(AMaxDepth, ALeafCapacity: Integer;
      const ARootBox: TBox3d);
    function TreeRoot: TMyOctreeNode;
  end;

{ implementation }

procedure TMyOctreeNode.PutItemIntoSubNodes(ItemIndex: integer);
begin
 { See comments at @link(TOctreeNode.PutItemIntoSubNodes)
   to know how you should implement this. }
end;

function TMyOctreeNode.ParentTree: TMyOctree;
begin
 Result := TMyOctree(InternalParentTree);
end;

constructor TMyOctree.Create(AMaxDepth, ALeafCapacity: Integer;
  const ARootBox: TBox3d);
begin
 inherited Create(AMaxDepth, ALeafCapacity, ARootBox,
   TMyOctreeNode);
end;

function TMyOctree.TreeRoot: TMyOctreeNode;
begin
 Result := TMyOctreeNode(InternalTreeRoot);
end;
  #)
*)

unit KambiOctree;

interface

uses SysUtils, VectorMath, Boxes3d, KambiUtils, Frustum, Contnrs;

type
  { }
  TOctree = class;
  TOctreeNode = class;

  TOctreeNodeClass = class of TOctreeNode;

  TOctreeSubnodeIndex = array[0..2]of boolean;

  TEnumerateOctreeItemsFunc = procedure(ItemIndex: Integer;
    CollidesForSure: boolean) of object;

  TOctreeNode = class
  private
    { read-only after the object is created }
    FBox: TBox3d;
    FMiddlePoint: TVector3Single;
    FDepth: Integer;
    FParentTree: TOctree;
    FParentNode: TOctreeNode;
    FBoundingSphereRadiusSqr: Single;
    FBoundingSphereCenter: TVector3Single;

    FItemsIndices: TDynIntegerArray;

    FIsLeaf: boolean;

    { CreateTreeSubNodes inicjuje TreeSubNodes (tworzy je, wywolujac odpowiednie
      konstruktry); CreateLeafItems inicjuje Items.
      Uwaga - zadna z tych proc NIE robi najpierw Items.Free czy Subnodes[].Free,
      musisz najpierw zadbac sam o zrobienie odpowiednich Free (a w zasadzie,
      jezeli sa <> nil, to po co je tworzyc ? Nie powinienes wtedy w ogole
      wywolywac ponizszych Create* }
    procedure CreateTreeSubNodes(AsLeaves: boolean);
    procedure CreateLeafItems;
    procedure FreeAndNilTreeSubNodes;

    procedure SetLeaf(value: boolean);

    procedure EnumerateCollidingOctreeItems(
      const Frustum: TFrustum;
      EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);
  protected
    { This must be overriden in subclasses. This should call
      SubNode.AddItem(ItemIndex) for chosen TreeSubNodes
      (maybe all, maybe none). It depends on what is your definition of
      "an octree item" how you implement it -- you generally
      want to check how given item collides with BoundingBoxes
      of your TreeSubNodes and call SubNode.AddItem(ItemIndex)
      for each SubNode that has (al least part) of your ItemIndex.

      Don't ever call this directly from subclasses of TOctreeNode.
      TOctreeNode internally calls it when it needs to.

      You can assume here that all TreeSubNodes are <> nil.
      But you shouldn't assume here anything about value of IsLeaf
      or ItemIndices <> nil (yes, this means that this function
      may be internally called when the state of this object
      is somewhat invalid). }
    procedure PutItemIntoSubNodes(ItemIndex: integer); virtual; abstract;

    property InternalParentTree: TOctree read FParentTree;

    { Parent node of the octree. @nil for the root node. }
    property InternalParentNode: TOctreeNode read FParentNode;
  public
    { TreeSubNodes sa wszystkie = nil gdy IsLeaf i wszystkie <> nil gdy not IsLeaf.
      znaczenie indeksow : pierwszy indeks mowi czy X sa >= MiddlePoint[0],
      drugi czy Y sa >= MiddlePoint[1] itd.

      Their class is always the same as the class of Self.

      Note that this field is read-only from outside of this unit ! }
    TreeSubNodes: array[boolean, boolean, boolean]of TOctreeNode;

    { When @link(IsLeaf), ItemsIndices is always <> nil
      and contains all items stored in this leaf.

      When not @link(IsLeaf) and not ParentTree.ItemsInNonLeafNodes
      then ItemIndices = nil.
      When not @link(IsLeaf) and ParentTree.ItemsInNonLeafNodes
      then ItemIndices is <> nil and stores indices of all items
      that are somewhere beneath this node. See comments
      at @link(TOctree.ItemsInNonLeafNodes) for more info
      when this may be useful.

      Never put any items in the octree node using simple ItemsIndices.Add !
      Instead you must use @link(AddItem) method.
    }
    property ItemsIndices: TDynIntegerArray read FItemsIndices;

    { Same things as ItemsIndices.Count.
      It is nice name to use with Items[] property defined in subclasses.
      Use this only when you know that @link(ItemsIndices) <> nil. }
    function ItemsCount: integer;

    { This inserts item this octree node.

      It takes takes care of many things that are essential for octree
      construction, like
      -- splitting a leaf node into non-leaf node
         if maximum number of items in leaf is exceeded
         (and Depth < MaxDepth) and
      -- (in case of inserting item into non-leaf node)
         dispatching item to correct children node (from @link(TreeSubNodes)). }
    procedure AddItem(ItemIndex: integer);

    { IsLeaf : zmiana z false na true powoduje zebranie wszystkich elementow
      z SubNode'ow i wrzucenie ich do ItemsIndices. Zmiana z true na false powoduje
      stworzenie SubNode'ow i wrzucenie do nich ItemsIndices zgodnie z MiddlePoint.}
    property IsLeaf: boolean read fIsLeaf write SetLeaf;

    { MiddlePoint to punkt wzgledem ktorego sa rozbite SubNode'y.
      Liscie tez musza miec zdefiniowany MiddlePoint na wypadek gdyby kiedys
      trzeba je bylo rozbic na 8 SubNode'ow.

      Note that MiddlePoint does not need to be exactly in the
      middle of octree node. This is it's default value
      (if you called Create that doesn't let you explicitly
      set MiddlePoint), but it's not a requirement.
      MiddlePoint may be anywhere inside @link(Box).

      This way you can specify MiddlePoint if you know that node
      with such MiddlePoint will yield better hierarchical division
      of your scene.

      Special case: when IsEmptyBox3d(Box), then value of MiddlePoint
      is undefined.  }
    property MiddlePoint: TVector3Single read fMiddlePoint;

    { Box to pudelko w ktorym powinny sie miescic wszystkie elementy pod tym
      OctreeNode. Tzn. jest dopuszczalne zeby jakies elementy "wystawaly" poza
      Box'a (bo nie chcemy dzielic trojkatow gdy bedziemy je wrzucac do drzewa,
      wiec jesli np. jakis trojkat bedzie zajmowal miejsce w dwoch subnode'ach
      to bedziemy go wrzucac do obydwu) ale jest generalnie niezdefiniowane jak
      takie wystajace czesci beda traktowane - czy na pewno zawsze wszystko bedzie
      te wystajace czesci uwzglednialo. Tylko te kawalki trojkatow ktore beda
      sie miescily w Box maja gwarantowana poprawna obsluge.

      Special case: when IsEmptyBox3d(Box), then MiddlePoint has undefined
      value and IsLeaf *must be true*. }
    property Box: TBox3d read fBox;

    { BoundingSphereCenter and BoundingSphereRadius are simply calculated
      as the smallest possible sphere enclosing Box.
      So they are only a bad approximation of bounding Box,
      but they can be sometimes useful in stuations when detecting
      collision versus bounding sphere is much faster than detecting
      them versus bounding box.

      BoundingSphereRadiusSqr = 0 and BoundingSphereCenter is undefined
      if Box is empty. }
    property BoundingSphereCenter: TVector3Single read FBoundingSphereCenter;
    property BoundingSphereRadiusSqr: Single read FBoundingSphereRadiusSqr;

    property Depth: integer read fDepth;

    { This calculates MiddlePoint as middle of ABox,
      or as (0, 0, 0) if ABox is empty.
      Then it simply calls @link(CreateBase) }
    constructor Create(const ABox: TBox3d; AParentTree: TOctree;
      AParentNode: TOctreeNode;
      ADepth: integer; AsLeaf: boolean);

    { This is the virtual constructor that you want to override
      in descendants. This constructor must be virtual,
      since in this unit CreateTreeSubNodes and TOctree.Create
      must be able to construct new instances of class
      using class reference (given by Self.ClassType in CreateTreeSubNodes
      or OctreeNodeFinalClass in TOctree.Create). }
    constructor CreateBase(const ABox: TBox3d; AParentTree: TOctree;
      AParentNode: TOctreeNode;
      ADepth: integer; AsLeaf: boolean;
      const AMiddlePoint: TVector3Single); virtual;

    destructor Destroy; override;

    { W jakim SubNode moglby byc punkt P ? Zdecyduj sie na podstawie wlasnego
      MiddlePoint. Ignoruje w ogole fakt czy jestesmy wezlem czy lisciem
      i ignoruje fakt czy punkt P jest w ogole w naszym Box (to znaczy
      jest w stanie zaklasyfikowac kazdy punkt przestrzeni na podstawie
      MiddlePoint, nie obchodzi go czy rzeczywiscie P inside Box) }
    function SubnodeWithPoint(const P: TVector3Double):
      TOctreeSubnodeIndex; overload;
    function SubnodeWithPoint(const P: TVector3Single):
      TOctreeSubnodeIndex; overload;

    procedure SubnodesWithBox(const ABox: TBox3d;
      out SubnodeLow, SubnodeHigh: TOctreeSubnodeIndex);

    { Simple check for frustum collision. }
    function FrustumCollisionPossible(const Frustum: TFrustum): boolean;

    { Push children nodes (use this only for non-leafs) into the List.
      @groupBegin }
    procedure PushChildrenFrontToBack(List: TOrderedList; const Position: TVector3Single);
    procedure PushChildrenBackToFront(List: TOrderedList; const Position: TVector3Single);
    procedure PushChildren(List: TOrderedList);
    { @groupEnd }
  end;

  { Helper structure to keep octree limits. Useful to implement
    VRML extension
    [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_octree_properties]. }
  TOctreeLimits = record
    MaxDepth: integer;
    LeafCapacity: Integer;
  end;
  POctreeLimits = ^TOctreeLimits;

  TOctree = class
  private
    FTreeRoot: TOctreeNode;
    FOctreeNodeFinalClass: TOctreeNodeClass;
    FItemsInNonLeafNodes: boolean;
    FMaxDepth: integer;
    FLeafCapacity: Integer;
  protected
    property InternalTreeRoot: TOctreeNode read FTreeRoot;

    { This will be appended to output of @link(Statistics).
      In this class this returns ''.
      You can override this if you want, and you can use there
      LeavesCount, ItemsCount, NonLeafNodesCount counts
      (they are already calculated for the sake of Statistics anyway).

      Every line, including the last one, must be terminated by nl. }
    function StatisticsBonus(
      const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
      virtual;
  public

    { MaxDepth = maksymalna glebokosc drzewa. MaxDepth = 0 oznacza ze RootNode
      musi byc lisciem. LeafCapacity to maksymalna pojemnosc lisci,
      tzn. w kazdym lisciu nie moze byc wiecej LeafCapacity elementow
      CHYBA ze ten lisc jest na maksymalnej glebokosci ! Jezeli lisc nie jest
      na maksymalnej glebokosci to przekroczenie w nim LeafCapacity
      powoduje rozbicie liscia na wezel wewnetrzny. Naturalnie LeafCapacity
      zawsze musi byc >= 1, inaczej wszystko nie ma sensu.

      TODO -- comments below are rather specific for TVRMLTriangleOctree,
      adjust them to general octree (so that they also talk about
      TVRMLShapeOctree)

      Gdy MaxDepth = 0
      wszystkie elementy sa na jednym poziomie wiec de facto badajac kolizje
      nie bedziemy miec drzewa osemkowego tylko zwykle liniowe przeszukiwanie
      wsrod wszystkich obiektow. MaxDepth = 0 moze byc przydatne do sprawdzenie
      czy w ogole jest sens robic drzewo osemkowe na danej scenie.
      Ogolnie, LeafCapacity i MaxDepth najlepiej jest dostroic do
      sceny - uzyteczna informacja bedzie tu
      TVRMLScene.CountNodes(TVRMLGeometryNode).

      Note: changing MaxDepth and LeafCapacity after creating
      octree is allowed, you shouldn't change them if you already put
      some items in your octree. Otherwise your octree will be correct
      but may not satisfy current MaxDepth and LeafCapacity
      constraints. In other words, changing MaxDepth and LeafCapacity
      does *not* rebuild your octree to satisfy new values
      of MaxDepth and LeafCapacity.
      Although I may implement it some day -- it's easy to implement,
      but it would be just useless for me now. }
    property MaxDepth: integer read FMaxDepth write FMaxDepth;
    property LeafCapacity: Integer
      read FLeafCapacity write FLeafCapacity;

    { TOctreeNode with IsLeaf = true *always* have ItemIndices,
      since leaves always store items inside.

      If you set this property to true when constructing this object
      then also every TOctreeNode with IsLeaf = false
      (i.e. non-leaf nodes, then have some children nodes)
      will have ItemIndices property that always keeps the sum
      of all ItemIndices stored in it's children.

      Advantages: e.g. consider frustum culling using octree.
      In some situations you know that some OctreeNode is
      completely inside frustum. Then you want to mark all
      items inside this OctreeNode as "visible".
      If you would have to traverse all children of this
      OctreeNode, you could have horribly slow algorithm
      (especially since one item is often stored in many leafs,
      since it collides with many leafs BoundingBoxes).
      So you can set ItemsInNonLeafNodes and simply
      use ItemIndices list of your OctreeNode.
      And you don't have to traverse children of this OctreeNode.

      Disadvantages: if you have many items in your octree
      (as is typical with e.g. @link(TVRMLTriangleOctree))
      then this property can cost you a *lot* of memory. }
    property ItemsInNonLeafNodes: boolean
      read FItemsInNonLeafNodes;

    { You must give value for this property when constructing
      this object.

      This tells what class should have FTreeRoot.
      Since inside TOctreeNode, each children node
      (in TreeSubNodes) always has the same class as it's parent,
      so class OctreeNodeFinalClass determines the TOctreeNode descendant
      that will be used to construct your whole octree. }
    property OctreeNodeFinalClass: TOctreeNodeClass
      read FOctreeNodeFinalClass;

    constructor Create(AMaxDepth, ALeafCapacity: integer;
      const ARootBox: TBox3d; AOctreeNodeFinalClass: TOctreeNodeClass;
      AItemsInNonLeafNodes: boolean);
    constructor Create(const Limits: TOctreeLimits;
      const ARootBox: TBox3d; AOctreeNodeFinalClass: TOctreeNodeClass;
      AItemsInNonLeafNodes: boolean);
    destructor Destroy; override;

    { This traverses octree seeking for nodes that collide
      (or, in some situations, *possibly* collide)
      with given Frustum. Then it returns every item in such nodes
      with EnumerateOctreeItemsFunc.

      It may return the same ItemIndex multiple times.

      It requires that ParentTree.ItemsInNonLeafNodes = true
      (checked by assertion here, so only if you have assertions on).
      I may implement it one day to work with octree with
      ParentTree.ItemsInNonLeafNodes = false, but it will be
      (usually) much slower on such tree anyway.

      It passes to EnumerateOctreeItemsFunc ItemIndex (i.e. one
      of items of ItemIndices[] array). It also passes to
      EnumerateOctreeItemsFunc parameter CollidesForSure.

      If passed CollidesForSure = true then octree node that
      had this item was found to be entirely in the frustum.
      This means that (assuming that octree item that is in some
      octree node always collides with Box of this node,
      and it should always be true (unless you screwed up your
      @link(TOctreeNode.PutItemIntoSubNodes) :))
      given octree item for sure collides with frustum.

      If passed CollidesForSure = false then given octree item
      was found inside some octree leaf that may only partially
      (or not at all) collide with frustum.
      And this function doesn't check whether your *octree item*
      possibly collides with Frustum (since in this generic
      octree unit, KambiOctree, we have no knowledge about
      geometry of your octree items).
      So you may want to check inside EnumerateOctreeItemsFunc handler
      your octree items versus Frustum, if you want to
      have greatest possibility of eliminating octree items
      that are not within your frustum (but beware that too many
      checks for visibility can also cost you too much time...).

      That's where CollidesForSure parameter is useful:
      if it's true then you already know for sure that frustum
      collides with this octree item, so you don't have to waste your
      time on additional checks.

      Internal notes:
      This simply calls TreeRoot.EnumerateCollidingOctreeItems. }
    procedure EnumerateCollidingOctreeItems(
      const Frustum: TFrustum;
      EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);

    { This calculates and returns some multi-line string that describes
      how octree levels look like -- how many leaves, non-leaves,
      items in leaves and on each level and in summary.

      Every line, including the last one, is terminated by nl.

      This appends @link(StatisticsBonus) result, so if you want to
      extend it in descendants you should override @link(StatisticsBonus). }
    function Statistics: string;
  end;

{ small helpers }
function OctreeSubnodeIndexToNiceStr(const SI: TOctreeSubnodeIndex): string;
function OctreeSubnodeIndexesEqual(const SI1, SI2: TOctreeSubnodeIndex): boolean;

implementation

uses KambiStringUtils;

{$I kambioctreemacros.inc}

{ TOctreeNode ------------------------------------------------------------ }

procedure TOctreeNode.CreateTreeSubNodes(AsLeaves: boolean);
var b: TOctreeSubnodeIndex;
    b_0, b_1, b_2: boolean;
    SubBox: TBox3d;
    i: integer;
begin
 for b_0 := Low(boolean) to High(boolean) do
  for b_1 := Low(boolean) to High(boolean) do
   for b_2 := Low(boolean) to High(boolean) do
   begin
    { I can't do "for b[0] := Low(boolean) ... " in FPC 1.9.5,
      so I'm using variables b_0, b_1, b_2. }
    b[0] := b_0;
    b[1] := b_1;
    b[2] := b_2;

    { calculate SubBox }
    SubBox := Box;
    for i := 0 to 2 do
     if b[i] then
      SubBox[0, i] := MiddlePoint[i] else
      SubBox[1, i] := MiddlePoint[i];

    TreeSubNodes[b[0], b[1], b[2]] :=
      TOctreeNodeClass(Self.ClassType).Create(
        SubBox, FParentTree, Self, Depth + 1, AsLeaves);
   end;
end;

procedure TOctreeNode.CreateLeafItems;
begin
 FItemsIndices := TDynIntegerArray.Create;
 ItemsIndices.AllowedCapacityOverflow := Max(FParentTree.LeafCapacity div 4, 4);
end;

procedure TOctreeNode.FreeAndNilTreeSubNodes;
var b1, b2, b3: boolean;
begin
 for b1 := Low(boolean) to High(boolean) do
  for b2 := Low(boolean) to High(boolean) do
   for b3 := Low(boolean) to High(boolean) do
    FreeAndNil(TreeSubNodes[b1, b2, b3]);
end;

procedure TOctreeNode.SetLeaf(value: boolean);
var i: integer;
begin
 if value <> FIsLeaf then
 begin
  if Value then
  begin
   CreateLeafItems;

   { TODO -- gather all items from subNode's to Items }
   raise Exception.Create('TODO -- setting from not-leaf back to leaf not '+
     'implemented yet (because not used anywhere)');

   FreeAndNilTreeSubNodes;
  end else
  begin
   CreateTreeSubNodes(true);
   for i := 0 to ItemsIndices.Length - 1 do
    PutItemIntoSubNodes(ItemsIndices[i]);
   if not FParentTree.ItemsInNonLeafNodes then
    FreeAndNil(FItemsIndices);
  end;
  FIsLeaf := value;
 end;
end;

function TOctreeNode.ItemsCount: integer;
begin
 Result := ItemsIndices.Count;
end;

procedure TOctreeNode.AddItem(ItemIndex: integer);
begin
 if IsLeaf and (ItemsCount = FParentTree.LeafCapacity) and
   (Depth < FParentTree.MaxDepth) then
  IsLeaf := false;

 if not IsLeaf then
 begin
  if FParentTree.ItemsInNonLeafNodes then
   ItemsIndices.AppendItem(ItemIndex);
  PutItemIntoSubNodes(ItemIndex)
 end else
  ItemsIndices.AppendItem(ItemIndex);
end;

constructor TOctreeNode.Create(const ABox: TBox3d; AParentTree: TOctree;
  AParentNode: TOctreeNode;
  ADepth: integer; AsLeaf: boolean);
var AMiddlePoint: TVector3Single;
begin
 if IsEmptyBox3d(ABox) then
 begin
  Check(AsLeaf, 'TOctreeNode.Create error: attempt to create non-leaf'
    +' node with empty bounding box');
  AMiddlePoint := Vector3Single(0, 0, 0);
 end else
  AMiddlePoint := Box3dMiddle(ABox);

 CreateBase(ABox, AParentTree, AParentNode, ADepth, AsLeaf, AMiddlePoint);
end;

constructor TOctreeNode.CreateBase(const ABox: TBox3d;
  AParentTree: TOctree; AParentNode: TOctreeNode;
  ADepth: integer; AsLeaf: boolean; const AMiddlePoint: TVector3Single);
begin
 inherited Create;

 FBox := ABox;
 BoundingSphereFromBox3d(Box, FBoundingSphereCenter, FBoundingSphereRadiusSqr);
 FMiddlePoint := AMiddlePoint;

 FParentTree := AParentTree;
 FParentNode := AParentNode;
 FDepth := ADepth;
 FIsLeaf := AsLeaf;
 if IsLeaf then
  CreateLeafItems else
  CreateTreeSubNodes(true);
end;

destructor TOctreeNode.Destroy;
begin
 FreeAndNilTreeSubNodes;
 FreeAndNil(FItemsIndices);
 inherited;
end;

{$define SubnodeWithPoint_IMPLEMENT:=
begin
 result[0] := P[0] >= MiddlePoint[0];
 result[1] := P[1] >= MiddlePoint[1];
 result[2] := P[2] >= MiddlePoint[2];
end;}

function TOctreeNode.SubnodeWithPoint(const P: TVector3Single): TOctreeSubnodeIndex;
SubnodeWithPoint_IMPLEMENT

function TOctreeNode.SubnodeWithPoint(const P: TVector3Double): TOctreeSubnodeIndex;
SubnodeWithPoint_IMPLEMENT

{$undef SubnodeWithPoint_IMPLEMENT}

procedure TOctreeNode.SubnodesWithBox(const ABox: TBox3d;
  out SubnodeLow, SubnodeHigh: TOctreeSubnodeIndex);
var i: Integer;
begin
 for i := 0 to 2 do
 begin
  SubnodeLow[i] := false;
  SubnodeHigh[i] := true;
  if ABox[0, i] >= MiddlePoint[i] then
   SubnodeLow[i] := true else
  if ABox[1, i] < MiddlePoint[i] then
   SubnodeHigh[i] := false;
 end;
end;

procedure TOctreeNode.EnumerateCollidingOctreeItems(
  const Frustum: TFrustum;
  EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);

  procedure EnumerateAllItems(CollidesForSure: boolean);
  var i: Integer;
  begin
   for i := 0 to ItemsIndices.Count - 1 do
    EnumerateOctreeItemsFunc(ItemsIndices.Items[i], CollidesForSure);
  end;

begin
 { Key to understand thing below is that I have two methods
   to check collision of octree node with frustum:
   using node's BoundingSphere and using node's
   bounding Box.

   Advantage of testing for collision with sphere:
   it's much faster than checking for collision versus box,
   which can be clearly seen comparing implementations of
   Frustum.SphereCollisionPossible and
   FrustumBox3dCollisionPossible.

   Disadvantage: bounding sphere is worse description of
   octree node's geometry (i.e. it's simply always larger then
   node's bounding Box), so such testing is not so effective
   in eliminating Shapes from visibility.

   So there are a couple things to do below.
   1. You can check each octree node versus it's BoundingSphere
   2. You can check each octree node versus it's Box
   3. And one that prooves the most efficient one in practice:
      check for collision versus sphere first, and then,
      it test versus sphere succeeds, check versus box. }

 if Frustum.SphereCollisionPossibleSimple(
      BoundingSphereCenter, BoundingSphereRadiusSqr) then
 begin
  case Frustum.Box3dCollisionPossible(Box) of
   fcNoCollision: ;
   fcSomeCollisionPossible:
     if IsLeaf then
      EnumerateAllItems(false) else
     begin
      { Once I had an idea to do an optimization to code below.
        For now, every recursive call to EnumerateCollidingOctreeItems
        potentially results in a call to FrustumBox3dCollisionPossible.
        But inside this call, the same points are often evaluated
        versus the same Frustum many times
        (because 8 points, on 8 edges of my Box, are shared by 2 children;
         bacause 6 points, on 6 faces of my Box, are shared by 4 children;
         and because 1 point in the middle of my Box is shared by all 8 children).
        Also note that Box[0] point of my Box is the same as Box[0] point
        of my [false, false, false] child and
        Box[1] point of my Box is the same as Box[1] point of my
        [true, true, true] child. So these points need not be evaluated
        versus the frustum ever again.

        1) So first I tried to implement passing the calculations of
           points versus frustum down the recursive calls.
           I.e. EnumerateCollidingOctreeItems gets from it's caller
           8 * 6 boolean results for it's 8 corner point.
           Then before recursive calls below it evaluates
           8 + 6 + 1 points versus frustum and passes them down
           to children. This way instead of evaluating
           8 points versus frustum in each call (for all nodes, leaves or not),
           I would be doing 15 evaluations versus frustum but
           only at non-leaf nodes that collide with frustum as
           fcSomeCollisionPossible.

           a) answer first : this is actually not necessarily faster,
              since now in some children first call to
              Frustum.SphereCollisionPossibleSimple may eliminate
              any need to compare this child's Box versus Frustum.
              But above scheme makes an assumption that every child
              will need to compare it's Box against Frustum.

           b) Honestly, passing down to the recursive calls
              proper corners prooved to be damn hard to implement
              with a clear and conside code.
              I didn't finish it yet. And (because of b))
              I don't think I will ever do, since it's probably not worth
              the trouble.

        2) So second idea is to use memoization on point comparison
           vs frustum planes.
           This would potentially give me the same advantages as 1)
           but without it's disadvantage 1.a).
           I didn't implement it (yet?) simply because I don't
           believe it's worth the trouble. }

      TreeSubNodes[false, false, false].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[false, false, true ].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[false, true , false].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[false, true , true ].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[true , false, false].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[true , false, true ].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[true , true , false].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
      TreeSubNodes[true , true , true ].EnumerateCollidingOctreeItems(Frustum, EnumerateOctreeItemsFunc);
     end;
   { fcInsideFrustum: } else EnumerateAllItems(true);
  end;
 end;
end;

function TOctreeNode.FrustumCollisionPossible(const Frustum: TFrustum): boolean;
begin
  Result :=
    Frustum.SphereCollisionPossibleSimple(
      BoundingSphereCenter, BoundingSphereRadiusSqr) and
    Frustum.Box3dCollisionPossibleSimple(Box);
end;

procedure TOctreeNode.PushChildrenFrontToBack(List: TOrderedList;
  const Position: TVector3Single);
var
  Index: TOctreeSubnodeIndex;
begin
  Index := SubnodeWithPoint(Position);
  List.Push(TreeSubNodes[Index[0], Index[1], Index[2]]);

  { next, push 3 children with one Index bit negated }
  List.Push(TreeSubNodes[not Index[0],     Index[1],     Index[2]]);
  List.Push(TreeSubNodes[    Index[0], not Index[1],     Index[2]]);
  List.Push(TreeSubNodes[    Index[0],     Index[1], not Index[2]]);

  Index[0] := not Index[0];
  Index[1] := not Index[1];
  Index[2] := not Index[2];

  { next, push 3 children with two Index bits negated.
    We just negated all 3 bits, so below we negate one back... }
  List.Push(TreeSubNodes[not Index[0],     Index[1],     Index[2]]);
  List.Push(TreeSubNodes[    Index[0], not Index[1],     Index[2]]);
  List.Push(TreeSubNodes[    Index[0],     Index[1], not Index[2]]);

  List.Push(TreeSubNodes[Index[0], Index[1], Index[2]]);
end;

procedure TOctreeNode.PushChildrenBackToFront(List: TOrderedList;
  const Position: TVector3Single);
var
  FrontIndex, BackIndex: TOctreeSubnodeIndex;
begin
  FrontIndex := SubnodeWithPoint(Position);
  BackIndex[0] := not FrontIndex[0];
  BackIndex[1] := not FrontIndex[1];
  BackIndex[2] := not FrontIndex[2];

  List.Push(TreeSubNodes[BackIndex[0], BackIndex[1], BackIndex[2]]);

  { next, push 3 children with one BackIndex bit negated }
  List.Push(TreeSubNodes[not BackIndex[0],     BackIndex[1],     BackIndex[2]]);
  List.Push(TreeSubNodes[    BackIndex[0], not BackIndex[1],     BackIndex[2]]);
  List.Push(TreeSubNodes[    BackIndex[0],     BackIndex[1], not BackIndex[2]]);


  { next, push 3 children with one FrontIndex bit negated. }
  List.Push(TreeSubNodes[not FrontIndex[0],     FrontIndex[1],     FrontIndex[2]]);
  List.Push(TreeSubNodes[    FrontIndex[0], not FrontIndex[1],     FrontIndex[2]]);
  List.Push(TreeSubNodes[    FrontIndex[0],     FrontIndex[1], not FrontIndex[2]]);

  List.Push(TreeSubNodes[FrontIndex[0], FrontIndex[1], FrontIndex[2]]);
end;

procedure TOctreeNode.PushChildren(List: TOrderedList);
begin
  List.Push(TreeSubNodes[false, false, false]);
  List.Push(TreeSubNodes[false, false, true ]);
  List.Push(TreeSubNodes[false, true , false]);
  List.Push(TreeSubNodes[false, true , true ]);
  List.Push(TreeSubNodes[true , false, false]);
  List.Push(TreeSubNodes[true , false, true ]);
  List.Push(TreeSubNodes[true , true , false]);
  List.Push(TreeSubNodes[true , true , true ]);
end;

{ TOctree ------------------------------------------------------------ }

constructor TOctree.Create(AMaxDepth, ALeafCapacity: integer;
  const ARootBox: TBox3d; AOctreeNodeFinalClass: TOctreeNodeClass;
  AItemsInNonLeafNodes: boolean);
begin
 inherited Create;
 FMaxDepth := AMaxDepth;
 FLeafCapacity := ALeafCapacity;
 FOctreeNodeFinalClass := AOctreeNodeFinalClass;
 FItemsInNonLeafNodes := AItemsInNonLeafNodes;
 FTreeRoot := OctreeNodeFinalClass.Create(ARootBox, Self, nil, 0, true);
end;

constructor TOctree.Create(const Limits: TOctreeLimits;
  const ARootBox: TBox3d; AOctreeNodeFinalClass: TOctreeNodeClass;
  AItemsInNonLeafNodes: boolean);
begin
  Create(Limits.MaxDepth, Limits.LeafCapacity,
    ARootBox, AOctreeNodeFinalClass, AItemsInNonLeafNodes);
end;

destructor TOctree.Destroy;
begin
 FreeAndNil(FTreeRoot);
 inherited;
end;

procedure TOctree.EnumerateCollidingOctreeItems(
  const Frustum: TFrustum;
  EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);
begin
 Assert(ItemsInNonLeafNodes);
 FTreeRoot.EnumerateCollidingOctreeItems(
   Frustum, EnumerateOctreeItemsFunc);
end;

function TOctree.Statistics: string;
var leavesCounts: TDynCardinalArray;
    nonLeafNodesCounts: TDynCardinalArray;
    itemsCounts: TDynCardinalArray;

  procedure StatNode(TreeNode: TOctreeNode);
  var b0, b1, b2: boolean;
  begin
   if TreeNode.IsLeaf then
   begin
    Inc(leavesCounts.Items[TreeNode.Depth]);
    itemsCounts.Items[TreeNode.Depth] += Cardinal(TreeNode.ItemsCount);
   end else
   begin
    Inc(nonLeafNodesCounts.Items[TreeNode.Depth]);
    for b0 := Low(boolean) to High(boolean) do
     for b1 := Low(boolean) to High(boolean) do
      for b2 := Low(boolean) to High(boolean) do
       StatNode(TreeNode.TreeSubNodes[b0, b1, b2]);
   end;
  end;

  procedure WritelnStatLine(const Header: string;
    const LeavesCount, ItemsCount, NonLeafNodesCount: Cardinal);
  begin
   Result += Format(
     '  %-12s%4d leaves (%6d items in leaves), %4d non-leaf nodes.' +nl,
     [Header, LeavesCount, ItemsCount, NonLeafNodesCount]);
  end;

var i: integer;
    LeavesCount, ItemsCount, NonLeafNodesCount: Int64;
begin
 leavesCounts := nil;
 nonLeafNodesCounts := nil;
 itemsCounts := nil;
 try
  leavesCounts := TDynCardinalArray.Create(MaxDepth+1);
  leavesCounts.FillChar(0);
  nonLeafNodesCounts := TDynCardinalArray.Create(MaxDepth+1);
  nonLeafNodesCounts.FillChar(0);
  itemsCounts := TDynCardinalArray.Create(MaxDepth+1);
  itemsCounts.FillChar(0);

  StatNode(FTreeRoot);

  Result := 'Octree statistics :' +nl;
  for i := 0 to MaxDepth do
  begin
   WritelnStatLine('Depth '+Format('%3d',[i])+' :',
     leavesCounts[i], itemsCounts[i], nonLeafNodesCounts[i]);

   if (nonLeafNodesCounts[i] = 0) then
   begin
    if i < MaxDepth then
     Result += Format(
       '  No nodes on lower depths (%d ...  ).' +nl, [i+1, MaxDepth]);
    break;
   end;
  end;

  LeavesCount := leavesCounts.BigSum;
  ItemsCount := itemsCounts.BigSum;
  NonLeafNodesCount := NonLeafNodesCounts.BigSum;

  WritelnStatLine('SUM :', LeavesCount, ItemsCount, NonLeafNodesCount);

  Result += StatisticsBonus(LeavesCount, ItemsCount, NonLeafNodesCount);
 finally
  leavesCounts.Free;
  nonLeafNodesCounts.Free;
  itemsCounts.Free;
 end;
end;

function TOctree.StatisticsBonus(
  const LeavesCount, ItemsCount, NonLeafNodesCount: Int64): string;
begin
 Result := '';
end;

{ some global procs ---------------------------------------------------------- }

function OctreeSubnodeIndexToNiceStr(const SI: TOctreeSubnodeIndex): string;
begin
 result := BoolToStr[SI[0]]+'-'+BoolToStr[SI[1]]+'-'+BoolToStr[SI[2]];
end;

function OctreeSubnodeIndexesEqual(const SI1, SI2: TOctreeSubnodeIndex): boolean;
begin
 result := (SI1[0] = SI2[0]) and
           (SI1[1] = SI2[1]) and
           (SI1[2] = SI2[2]);
end;

end.