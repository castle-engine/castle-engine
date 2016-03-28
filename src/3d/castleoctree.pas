{
  Copyright 2003-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*Base octree classes (TOctreeNode and TOctree) and utilities.
  Used by actual octrees in units like CastleTriangleOctree and CastleShapeOctree.

  Typical way to derive actual (non-abstract) octrees goes like this;
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
      const ARootBox: TBox3D);
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
  const ARootBox: TBox3D);
begin
  inherited Create(AMaxDepth, ALeafCapacity, ARootBox, TMyOctreeNode);
end;

function TMyOctree.TreeRoot: TMyOctreeNode;
begin
  Result := TMyOctreeNode(InternalTreeRoot);
end;
#)
*)
unit CastleOctree;

interface

uses SysUtils, CastleVectors, CastleBoxes, CastleUtils, CastleFrustum, Contnrs;

type
  { }
  TOctree = class;
  TOctreeNode = class;

  TOctreeNodeClass = class of TOctreeNode;

  TOctreeSubnodeIndex = array[0..2]of boolean;

  TEnumerateOctreeItemsFunc = procedure(ItemIndex: Integer;
    CollidesForSure: boolean) of object;

  { Octree node.

    @unorderedList(
      @item(@italic(Leaf nodes) store a list of indexes in ItemsIndices array.
        These are usuallly indexes to some array of items on TOctree.
        For the sake of this unit they are just some integers that
        uniquely describe items that you want to keep in octree leafs.
        The base abstract TOctreeNode class doesn't clarify what
        kind of items are actually kept.)

      @item(@italic(Not leaf (internal) nodes) have 8 children
       nodes in TreeSubNodes.)
    )

    Each @link(TOctreeNode) also has some essential properties like
    Box, MiddlePoint and ParentTree. }
  TOctreeNode = class
  private
    { read-only after the object is created }
    FBox: TBox3D;
    FMiddlePoint: TVector3Single;
    FDepth: Integer;
    FParentTree: TOctree;
    FParentNode: TOctreeNode;
    FBoundingSphereRadiusSqr: Single;
    FBoundingSphereCenter: TVector3Single;

    FItemsIndices: TIntegerList;

    FIsLeaf: boolean;

    { Neither CreateTreeSubNodes nor CreateLeafItems call
      Items.Free or Subnodes[].Free, that is they don't cleanup after their
      previous calls. If you want, you have to cleanup yourself. }
    { Create TreeSubNodes. }
    procedure CreateTreeSubNodes(AsLeaves: boolean);
    { Create ItemsIndices. }
    procedure CreateLeafItems;
    procedure FreeAndNilTreeSubNodes;

    procedure SetLeaf(value: boolean);

    procedure EnumerateCollidingOctreeItems(
      const Frustum: TFrustum;
      EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);
  protected
    { Insert given index into appropriate subnodes.
      This should call SubNode.AddItem(ItemIndex) for chosen TreeSubNodes
      (maybe all, maybe none). It all depends on what is your definition of
      "an octree item" -- you generally want to check how given item
      collides with BoundingBoxes of your TreeSubNodes and call
      SubNode.AddItem(ItemIndex) for each SubNode that contains
      (al least part) of your ItemIndex.

      Don't ever call this directly from subclasses of TOctreeNode.
      TOctreeNode internally calls it when it needs to.

      You can assume here that all TreeSubNodes are <> nil.
      But you shouldn't assume here anything about value of IsLeaf
      or ItemIndices <> nil (yes, this means that this function
      may be internally called when the state of this object
      is partially invalid). }
    procedure PutItemIntoSubNodes(ItemIndex: integer); virtual; abstract;

    property InternalParentTree: TOctree read FParentTree;

    { Parent node of the octree. @nil for the root node. }
    property InternalParentNode: TOctreeNode read FParentNode;
  public
    { Child octree nodes, only if this is an internal node (IsLeaf = @false).
      When this is a leaf (IsLeaf = @true), these are all @nil.

      Indexed by booleans, "true" means that given coordinate
      is >= than corresponding MiddlePoint coordinate.
      For example TreeSubNodes[true, true, true] are coordinates
      between MiddlePoint and Box[1].

      Subnodes class is always the same as our (Self) class.

      This field is read-only from outside of this unit. }
    TreeSubNodes: array [boolean, boolean, boolean] of TOctreeNode;

    { Items stored at the octree node.
      Items are stored here (and ItemsIndices <> nil) only
      when this is a leaf item (IsLeaf = @true) or when
      ParentTree.ItemsInNonLeafNodes is @true. In the latter case,
      we store items even in internal nodes, see TOctree.ItemsInNonLeafNodes.

      Never put any items in the octree node by direct ItemsIndices.Add.
      Instead you must use @link(AddItem) method. }
    property ItemsIndices: TIntegerList read FItemsIndices;

    { Number of items stored here.
      Same thing as ItemsIndices.Count, but has somewhat nicer name
      if you have Items[] property defined in a subclass.
      Use this only when you know that @link(ItemsIndices) <> nil. }
    function ItemsCount: integer;

    { Insert an item into this octree node.

      It takes care of the octree structure properties:

      @unorderedList(
        @item(Splits a leaf node into non-leaf node
         if maximum number of items in leaf is exceeded (and Depth < MaxDepth).)

        @item(And when you insert an item into non-leaf node,
          it correctly puts it into children subnodes too.)
      ) }
    procedure AddItem(ItemIndex: integer);

    { Is this a leaf node.

      Changing this property rearranges items correctly.
      If you change this from @false to @true then all items from subnodes
      are correctly gathered and stored in our ItemsIndices.
      If you change this from @true to @false then subnodes are created
      and we insert to them our items, following the MiddlePoint rule. }
    property IsLeaf: boolean read fIsLeaf write SetLeaf;

    { Middle point of the octree node, determines how our subnodes divide
      the space. This is defined for leaf nodes too, since leaf nodes
      may have to be converted into internal nodes at some point.

      Note that MiddlePoint does not need to be exactly in the
      middle of the octree node. This is it's default value
      (if you called constructor without explicitly
      providing MiddlePoint value), but it's not a requirement.
      MiddlePoint may be anywhere inside @link(Box).

      This way you can explicitly specify some MiddlePoint if you know that
      if will yield better hierarchical division of your scene.

      When @link(Box) is empty, then value of MiddlePoint is undefined.  }
    property MiddlePoint: TVector3Single read fMiddlePoint;

    { Axis-aligned box in the 3D space that contains all items within
      this node. It's allowed that some items are actually larger
      and go (partially) outside of this box too (so you don't have
      to split one triangle into many when it doesn't fit perfectly
      into octree node), but it's undefined if the parts that go outside
      will be detected by collision routines of this node.

      Special case: when this is empty, then MiddlePoint has undefined
      value and IsLeaf must be true. }
    property Box: TBox3D read fBox;

    { Bounding sphere of this box.
      Right now simply calculated
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

    { Simple constructor. Calculates MiddlePoint as a middle of the ABox,
      or as (0, 0, 0) if ABox is empty. }
    constructor Create(const ABox: TBox3D; AParentTree: TOctree;
      AParentNode: TOctreeNode;
      ADepth: integer; AsLeaf: boolean);

    { Virtual constructor, not to be called directly, only to be overridden.
      (But a constructor should be public, not protected.)

      You want to override this in descendants.
      This constructor must be virtual,
      since in this unit CreateTreeSubNodes and TOctree.Create
      must be able to construct new instances of class
      using class reference (given by Self.ClassType in CreateTreeSubNodes
      or OctreeNodeFinalClass in TOctree.Create).

      @exclude }
    constructor CreateBase(const ABox: TBox3D; AParentTree: TOctree;
      AParentNode: TOctreeNode;
      ADepth: integer; AsLeaf: boolean;
      const AMiddlePoint: TVector3Single); virtual;

    destructor Destroy; override;

    { In which subnode does the given point lie.
      Decides using MiddlePoint.

      This is a simple utility, ignores what is our @link(Box) (doesn't check
      is P is inside @link(Box) at all), ignores if we're leaf or not. }
    function SubnodeWithPoint(const P: TVector3Double):
      TOctreeSubnodeIndex; overload;
    function SubnodeWithPoint(const P: TVector3Single):
      TOctreeSubnodeIndex; overload;

    procedure SubnodesWithBox(const ABox: TBox3D;
      out SubnodeLow, SubnodeHigh: TOctreeSubnodeIndex);

    { Ignore Z coords of boxes, like they were infinite in Z. }
    procedure SubnodesWithBox2D(const ABoxMin, ABoxMax: TVector2Single;
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
    VRML/X3D extension
    [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_octree_properties]. }
  TOctreeLimits = record
    MaxDepth: integer;
    LeafCapacity: Integer;
  end;
  POctreeLimits = ^TOctreeLimits;

  { Base abstract octree class.
    Holds some settings common for the whole octree (like MaxDepth)
    and a reference to the root octree node (of @link(TOctreeNode) class)
    in the protected property InternalTreeRoot. }
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
    { Maximum tree depth.

      Set this to zero to force RootNode to be a leaf
      (useful to test whether octree vs a flat list is actually useful).

      Currently, you should not change MaxDepth and LeafCapacity after creating
      the octree, as they will not rebuild the octree to obey the given limits. }
    property MaxDepth: integer read FMaxDepth write FMaxDepth;

    { Maximum number of items inside a leaf, unless this leaf is already
      at maximum depth (MaxDepth). When you add an item to a leaf,
      to keep LeafCapacity correct. Must be >= 1. }
    property LeafCapacity: Integer
      read FLeafCapacity write FLeafCapacity;

    { Does this octree keep items also in internal nodes.
      Leaf nodes always store items (have ItemIndices).

      If you set this property to @true when constructing this object
      then all created TOctreeNode will have ItemIndices property
      that keeps the items inside.

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
      (as is typical with e.g. @link(TTriangleOctree))
      then this property can cost you a @italic(lot) of memory. }
    property ItemsInNonLeafNodes: boolean
      read FItemsInNonLeafNodes;

    { The actual (non-abstact) TOctreeNode descendant class for this octree.
      Set when constructing this octree.

      This tells what class should have FTreeRoot.
      Since inside TOctreeNode, each children node
      (in TreeSubNodes) always has the same class as it's parent,
      so class OctreeNodeFinalClass determines the TOctreeNode descendant
      that will be used to construct your whole octree. }
    property OctreeNodeFinalClass: TOctreeNodeClass
      read FOctreeNodeFinalClass;

    constructor Create(AMaxDepth, ALeafCapacity: integer;
      const ARootBox: TBox3D; AOctreeNodeFinalClass: TOctreeNodeClass;
      AItemsInNonLeafNodes: boolean);
    constructor Create(const Limits: TOctreeLimits;
      const ARootBox: TBox3D; AOctreeNodeFinalClass: TOctreeNodeClass;
      AItemsInNonLeafNodes: boolean);
    destructor Destroy; override;

    { Traverse octree seeking for nodes that (possibly) collide
      with given Frustum. For every such item
      calls given EnumerateOctreeItemsFunc.

      Requires that ParentTree.ItemsInNonLeafNodes = true
      (checked by assertion here, so only if you have assertions on).
      May be implemented one day to work with
      ParentTree.ItemsInNonLeafNodes = false too, but it will be
      (usually) much slower on such tree.

      It gives to EnumerateOctreeItemsFunc an ItemIndex,
      an index to octrees items. This is just taken from ItemIndices array.
      May call EnumerateOctreeItemsFunc with the same ItemIndex multiple times.

      It also gives to EnumerateOctreeItemsFunc parameter CollidesForSure:

      @unorderedList(
        @item(If CollidesForSure = true then octree node that
          had this item was found to be entirely in the frustum.
          This means that (assuming that octree item that is in some
          octree node always collides with Box of this node,
          and it should always be true)
          given octree item for sure collides with frustum.)

        @item(If CollidesForSure = false then given octree item
          was found inside some octree leaf that may only partially
          (or not at all) collide with frustum.
          And this function doesn't check whether your @italic(octree item)
          possibly collides with Frustum (since in this generic
          octree unit, CastleOctree, we have no knowledge about
          geometry of your octree items).
          So you may want to check inside EnumerateOctreeItemsFunc handler
          your octree items versus Frustum, if you want to
          have greatest possibility of eliminating octree items
          that are not within your frustum (but beware that too many
          checks for visibility can also cost you too much time...).

          That why CollidesForSure may be useful:
          if it's true then you already know for sure that frustum
          collides with this octree item, so you don't have to waste your
          time on additional checks.)
      )

      @italic(Notes for implementing descendants of this class:)
      This method simply calls TreeRoot.EnumerateCollidingOctreeItems. }
    procedure EnumerateCollidingOctreeItems(
      const Frustum: TFrustum;
      EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);

    { Multi-line description of how the octree levels look like.
      Describes how many leaves / non-leaves  we have,
      how many items in leaves we have and on each level and in summary.

      Every line, including the last one, is terminated by newline.

      @italic(Notes for implementing descendants of this class:)
      You can override StatisticsBonus, it's appended to the result of this
      method. }
    function Statistics: string;
  end;

function OctreeSubnodeIndexToNiceStr(const SI: TOctreeSubnodeIndex): string;
function OctreeSubnodeIndexesEqual(const SI1, SI2: TOctreeSubnodeIndex): boolean;

implementation

uses CastleStringUtils;

{ TOctreeNode ------------------------------------------------------------ }

procedure TOctreeNode.CreateTreeSubNodes(AsLeaves: boolean);
var b: TOctreeSubnodeIndex;
    b_0, b_1, b_2: boolean;
    SubBox: TBox3D;
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
      SubBox.Data[0, i] := MiddlePoint[i] else
      SubBox.Data[1, i] := MiddlePoint[i];

    TreeSubNodes[b[0], b[1], b[2]] :=
      TOctreeNodeClass(Self.ClassType).Create(
        SubBox, FParentTree, Self, Depth + 1, AsLeaves);
   end;
end;

procedure TOctreeNode.CreateLeafItems;
begin
 FItemsIndices := TIntegerList.Create;
 ItemsIndices.Capacity := Max(FParentTree.LeafCapacity div 4, 4);
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
   for i := 0 to ItemsIndices.Count - 1 do
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
   ItemsIndices.Add(ItemIndex);
  PutItemIntoSubNodes(ItemIndex)
 end else
  ItemsIndices.Add(ItemIndex);
end;

constructor TOctreeNode.Create(const ABox: TBox3D; AParentTree: TOctree;
  AParentNode: TOctreeNode;
  ADepth: integer; AsLeaf: boolean);
var AMiddlePoint: TVector3Single;
begin
 if ABox.IsEmpty then
 begin
  Check(AsLeaf, 'TOctreeNode.Create error: attempt to create non-leaf'
    +' node with empty bounding box');
  AMiddlePoint := Vector3Single(0, 0, 0);
 end else
  AMiddlePoint := ABox.Middle;

 CreateBase(ABox, AParentTree, AParentNode, ADepth, AsLeaf, AMiddlePoint);
end;

constructor TOctreeNode.CreateBase(const ABox: TBox3D;
  AParentTree: TOctree; AParentNode: TOctreeNode;
  ADepth: integer; AsLeaf: boolean; const AMiddlePoint: TVector3Single);
begin
 inherited Create;

 FBox := ABox;
 Box.BoundingSphere(FBoundingSphereCenter, FBoundingSphereRadiusSqr);
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

function TOctreeNode.SubnodeWithPoint(const P: TVector3Single): TOctreeSubnodeIndex;
begin
  result[0] := P[0] >= MiddlePoint[0];
  result[1] := P[1] >= MiddlePoint[1];
  result[2] := P[2] >= MiddlePoint[2];
end;

function TOctreeNode.SubnodeWithPoint(const P: TVector3Double): TOctreeSubnodeIndex;
begin
  result[0] := P[0] >= MiddlePoint[0];
  result[1] := P[1] >= MiddlePoint[1];
  result[2] := P[2] >= MiddlePoint[2];
end;

procedure TOctreeNode.SubnodesWithBox(const ABox: TBox3D;
  out SubnodeLow, SubnodeHigh: TOctreeSubnodeIndex);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    SubnodeLow[i] := false;
    SubnodeHigh[i] := true;
    if ABox.Data[0, i] >= MiddlePoint[i] then
      SubnodeLow[i] := true else
    if ABox.Data[1, i] < MiddlePoint[i] then
      SubnodeHigh[i] := false;
  end;
end;

procedure TOctreeNode.SubnodesWithBox2D(const ABoxMin, ABoxMax: TVector2Single;
  out SubnodeLow, SubnodeHigh: TOctreeSubnodeIndex);
var
  i: Integer;
begin
  for i := 0 to 1 do
  begin
    SubnodeLow[i] := false;
    SubnodeHigh[i] := true;
    if ABoxMin[i] >= MiddlePoint[i] then
      SubnodeLow[i] := true else
    if ABoxMax[i] < MiddlePoint[i] then
      SubnodeHigh[i] := false;
  end;
  SubnodeLow[2] := false;
  SubnodeHigh[2] := true;
end;

procedure TOctreeNode.EnumerateCollidingOctreeItems(
  const Frustum: TFrustum;
  EnumerateOctreeItemsFunc: TEnumerateOctreeItemsFunc);

  procedure EnumerateAllItems(CollidesForSure: boolean);
  var i: Integer;
  begin
   for i := 0 to ItemsIndices.Count - 1 do
    EnumerateOctreeItemsFunc(ItemsIndices[i], CollidesForSure);
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
   FrustumBox3DCollisionPossible.

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
  case Frustum.Box3DCollisionPossible(Box) of
   fcNoCollision: ;
   fcSomeCollisionPossible:
     if IsLeaf then
      EnumerateAllItems(false) else
     begin
      { Once I had an idea to do an optimization to code below.
        For now, every recursive call to EnumerateCollidingOctreeItems
        potentially results in a call to FrustumBox3DCollisionPossible.
        But inside this call, the same points are often calculated
        versus the same Frustum many times
        (because 8 points, on 8 edges of my Box, are shared by 2 children;
         bacause 6 points, on 6 faces of my Box, are shared by 4 children;
         and because 1 point in the middle of my Box is shared by all 8 children).
        Also note that Box[0] point of my Box is the same as Box[0] point
        of my [false, false, false] child and
        Box[1] point of my Box is the same as Box[1] point of my
        [true, true, true] child. So these points need not be calculated
        versus the frustum ever again.

        1) So first I tried to implement passing the calculations of
           points versus frustum down the recursive calls.
           I.e. EnumerateCollidingOctreeItems gets from it's caller
           8 * 6 boolean results for it's 8 corner point.
           Then before recursive calls below it calculates
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
    Frustum.Box3DCollisionPossibleSimple(Box);
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
  const ARootBox: TBox3D; AOctreeNodeFinalClass: TOctreeNodeClass;
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
  const ARootBox: TBox3D; AOctreeNodeFinalClass: TOctreeNodeClass;
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
var leavesCounts: TCardinalList;
    nonLeafNodesCounts: TCardinalList;
    itemsCounts: TCardinalList;

  procedure StatNode(TreeNode: TOctreeNode);
  var b0, b1, b2: boolean;
  begin
   if TreeNode.IsLeaf then
   begin
    Inc(leavesCounts.L[TreeNode.Depth]);
    itemsCounts.L[TreeNode.Depth] += Cardinal(TreeNode.ItemsCount);
   end else
   begin
    Inc(nonLeafNodesCounts.L[TreeNode.Depth]);
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
  leavesCounts := TCardinalList.Create;
  leavesCounts.Count := MaxDepth + 1; { TFPGList initialized everything to 0 }
  nonLeafNodesCounts := TCardinalList.Create;
  nonLeafNodesCounts.Count := MaxDepth + 1; { TFPGList initialized everything to 0 }
  itemsCounts := TCardinalList.Create;
  itemsCounts.Count := MaxDepth + 1; { TFPGList initialized everything to false }

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
