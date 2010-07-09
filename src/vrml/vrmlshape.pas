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

{ VRML shape (TVRMLShape class) and a simple tree of shapes
  (TVRMLShapeTree class). }

unit VRMLShape;

{$I vrmloctreeconf.inc}

interface

uses SysUtils, Classes, VectorMath, Base3D, Boxes3D, VRMLNodes, KambiClassUtils,
  KambiUtils, VRMLTriangleOctree, Frustum, KambiOctree, VRMLTriangle;

{$define read_interface}

const
  DefLocalTriangleOctreeMaxDepth = 10;
  { Default octree leaf capacity for TVRMLShape.OctreeTriangles.

    This is slightly larger than DefTriangleOctreeLeafCapacity, as this
    octree will usually be used interactively for collision detection,
    not by ray-tracer. So octree construction speed is somewhat important,
    and cannot be too large... }
  DefLocalTriangleOctreeLeafCapacity = 32;
  DefLocalTriangleOctreeLimits: TOctreeLimits = (
    MaxDepth: DefLocalTriangleOctreeMaxDepth;
    LeafCapacity: DefLocalTriangleOctreeLeafCapacity
  );

type
  { Internal type for TVRMLShape
    @exclude }
  TVRMLShapeValidities = set of (svLocalBBox, svBBox,
    svVerticesCountNotOver,  svVerticesCountOver,
    svTrianglesCountNotOver, svTrianglesCountOver,
    svBoundingSphere,
    svNormals);

  { Internal type for TVRMLShape
    @exclude }
  TVRMLShapeNormalsCached = (ncSmooth, ncFlat, ncCreaseAngle);

  { Possible spatial structure types that may be managed by TVRMLShape,
    see TVRMLShape.Spatial. }
  TVRMLShapeSpatialStructure = (
    { Create the TVRMLShape.OctreeTriangles.
      This is an octree containing all triangles. }
    ssTriangles);
  TVRMLShapeSpatialStructures = set of TVRMLShapeSpatialStructure;

  TVRMLShape = class;

  TShapeTraverseFunc = procedure (Shape: TVRMLShape) of object;

  { Tree of VRML shapes.

    Although VRML model already provides the tree (graph of VRML nodes),
    it's a little too complicated to be used at each render call.
    It's especially true for VRML <= 1.0 (where properties may "leak out"
    from one node to the next), VRML >= 2.0 cleaned a lot here but still
    some work must be done when traversing (like accumulating transformations).

    So we process VRML tree to this tree, which is much simpler tree with
    all the geometry nodes (TVRMLGeometryNode) along with their state
    (TVRMLGraphTraverseState) as leafs (TVRMLShape). }
  TVRMLShapeTree = class
  private
    FParentScene: TObject;
  public
    constructor Create(AParentScene: TObject);

    { Parent TVRMLScene instance. This cannot be declared here as
      TVRMLScene (this would create circular unit dependency),
      but it always is TVRMLScene. }
    property ParentScene: TObject read FParentScene write FParentScene;

    procedure Traverse(Func: TShapeTraverseFunc;
      OnlyActive: boolean); virtual; abstract;

    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; virtual; abstract;

    { Look for shape with Geometry.NodeName = GeometryNodeName.
      Returns @nil if not found. }
    function FindGeometryNodeName(const GeometryNodeName: string;
      OnlyActive: boolean = false): TVRMLShape;

    { Look for shae with Geometry that has a parent named ParentNodeName.
      Parent is searched by Geometry.TryFindParentNodeByName.
      Returns @nil if not found. }
    function FindShapeWithParentNamed(const ParentNodeName: string;
      OnlyActive: boolean = false): TVRMLShape;

    { Assuming that the model was created by Blender VRML 1 or 2 exporter,
      this searches for a first shape that was created from Blender
      mesh named BlenderMeshName.

      It follows the logic of two Blender exporters.

      If it doesn't find matching node, returns nil. Otherwise, returns
      the matching shape.

      Note that FindBlenderObject would be usually more sensible
      (since there can be only one shape from given Blender object),
      but Blender VRML 1.0 exporter doesn't export anywhere Blender object
      name. So when working with VRML 1.0, you're stuck with looking
      for mesh names. }
    function FindBlenderMesh(const BlenderMeshName: string;
      OnlyActive: boolean = false): TVRMLShape;
  end;

  TEnumerateShapeTexturesFunction = procedure (Shape: TVRMLShape;
    Texture: TNodeX3DTextureNode) of object;

  { Shape is a geometry node @link(Geometry) instance and it's
    @link(State). For VRML >= 2.0, this usually corresponds to
    a single instance of actual VRML @code(Shape) node.
    It allows to perform many operations that need to know both geometry
    and it's current state (parent Shape node, current transformation and such).

    This class caches results of methods LocalBoundingBox, BoundingBox,
    and most others (see TVRMLShapeValidities for hints).
    This means that things work fast, but this also means that
    you must manually call @link(Changed)
    when you changed some properties of Geometry or contents of State.

    But note that you can't change Geometry or State to different
    objects --- they are readonly properties.

    Also note that if you're using @link(TVRMLScene) class
    then you don't have to worry about calling @link(Changed)
    of items in @link(TVRMLScene.Shapes).
    All you have to do is to call appropriate @code(Changed*)
    methods of @link(TVRMLScene). }
  TVRMLShape = class(TVRMLShapeTree)
  private
    FLocalBoundingBox: TBox3D;
    FBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: array [boolean] of Cardinal;
    Validities: TVRMLShapeValidities;
    FGeometry: TVRMLGeometryNode;
    FState: TVRMLGraphTraverseState;
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;

    procedure ValidateBoundingSphere;
  private
    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; Geometry: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
    function CreateTriangleOctree(const ALimits: TOctreeLimits;
      const ProgressTitle: string): TVRMLTriangleOctree;
  private
    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FOctreeTriangles: TVRMLTriangleOctree;

    FSpatial: TVRMLShapeSpatialStructures;
    procedure SetSpatial(const Value: TVRMLShapeSpatialStructures);

    function OverrideOctreeLimits(
      const BaseLimits: TOctreeLimits): TOctreeLimits;
  private
    {$ifdef SHAPE_OCTREE_USE_MAILBOX}
    { Mailbox, for speeding up collision queries.
      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxResult: PVRMLTriangle;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { Meaningful only when svNormals in Validities.
      Normals may be assigned only if svNormals in Validities. }
    FNormalsCached: TVRMLShapeNormalsCached;
    FNormals: TDynVector3SingleArray;
    { Meaningful only when svNormals in Validities and
      NormalsCached = ncCreaseAngle. }
    FNormalsCreaseAngle: Single;

    { Free and nil FOctreeTriangles. Also, makes sure to call
      PointingDeviceClear on ParentScene (since some PVRMLTriangle pointers
      were freed). }
    procedure FreeOctreeTriangles;
  public
    constructor Create(AParentScene: TObject;
      AGeometry: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
    destructor Destroy; override;

    property Geometry: TVRMLGeometryNode read FGeometry;

    { State of this shape.
      This object is owned by TVRMLShape class --- we will do State.Free
      in destructor. }
    property State: TVRMLGraphTraverseState read FState;

    { Calculate bounding box and vertices/triangles count,
      see TVRMLGeometryNode methods.
      @groupBegin }
    function LocalBoundingBox: TBox3D;
    function BoundingBox: TBox3D;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;
    { @groupEnd }

    { Calculates bounding sphere based on BoundingBox.
      In the future this may be changed to use BoundingSphere method
      of @link(TVRMLGeometryNode), when I will implement it.
      For now, BoundingSphere is always worse approximation of bounding
      volume than @link(BoundingBox) (i.e. BoundingSphere is always
      larger) but it may be useful in some cases when
      detecting collision versus bounding sphere is much faster than detecting
      them versus bounding box.

      BoundingSphereRadiusSqr = 0 and BoundingSphereCenter is undefined
      if Box is empty.

      @groupBegin }
    function BoundingSphereCenter: TVector3Single;
    function BoundingSphereRadiusSqr: Single;
    { @groupEnd }

    { Exactly equivalent to getting
      @link(BoundingSphereCenter) and @link(BoundingSphereRadiusSqr)
      and then using @link(TFrustum.SphereCollisionPossible).

      But it may be a little faster since it avoids some small speed problems
      (like copying memory contents when you get values of
      BoundingSphereXxx properties and checking twice are
      BoundingSphereXxx calculated). }
    function FrustumBoundingSphereCollisionPossible(
      const Frustum: TFrustum): TFrustumCollisionPossible;

    { Exactly equivalent to getting
      @link(BoundingSphereCenter) and @link(BoundingSphereRadiusSqr)
      and then using @link(TFrustum.SphereCollisionPossibleSimple).

      But it may be a little faster since it avoids some small speed problems. }
    function FrustumBoundingSphereCollisionPossibleSimple(
      const Frustum: TFrustum): boolean;

    procedure Changed(PossiblyLocalGeometryChanged: boolean); virtual;

    { The dynamic octree containing all triangles.
      It contains only triangles within this shape.

      There is no distinction here between collidable / visible
      (as for TVRMLScene octrees), since the whole shape may be
      visible and/or collidable.

      The triangles are specified in local coordinate system of this shape
      (that is, they are independent from transformation within State.Transform).
      This allows the tree to remain unmodified when transformation of this
      shape changes.

      This is automatically managed (initialized, updated, and used)
      by parent TVRMLScene. You usually don't need to know about this
      octree from outside.

      To initialize this, add ssTriangles to @link(Spatial) property,
      otherwise it's @nil. Parent TVRMLScene will take care of this
      (when parent TVRMLScene.Spatial contains ssDynamicCollisions, then
      all shapes contain ssTriangles within their Spatial).

      Parent TVRMLScene will take care to keep this octree always updated.

      Parent TVRMLScene will also take care of actually using
      this octree: TVRMLScene.OctreeCollisions methods actually use the
      octrees of specific shapes at the bottom. }
    property OctreeTriangles: TVRMLTriangleOctree read FOctreeTriangles;

    { Which spatial structrues (octrees, for now) should be created and managed.
      This works analogous to TVRMLScene.Spatial, but this manages
      octrees within this TVRMLShape. }
    property Spatial: TVRMLShapeSpatialStructures read FSpatial write SetSpatial;

    { Properties of created triangle octrees.
      See VRMLTriangleOctree unit comments for description.

      Default value comes from DefLocalTriangleOctreeLimits.

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      @groupBegin }
    function TriangleOctreeLimits: POctreeLimits;

    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;
    { @groupEnd }

    { Called by parent scene when local geometry changed.

      "Local" means that we're concerned here about changes visible
      in shape local coordinate system. E.g. things that only change our
      transformation (State.Transform) do not cause "local" geometry changes.

      "Geometry" means that we're concerned only about changes to topology
      --- vertexes, edges, faces, how they connect each other.
      Things that affect only appearance (e.g. whole Shape.appearance content
      in stuff for VRML >= 2.0) is not relevant here. E.g. changing
      material color does not cause "local" geometry changes

      For now, this updates octree, if initialized.
      Also removes cached normals. }
    procedure LocalGeometryChanged;

  public
    { Internally used by TVRMLScene. Says if local geometry change is scheduled
      (actual change will be done by TVRMLScene.DoGeometryChanged).

      ScheduledLocalGeometryChangedCoord means that coordinates changed.
      If this is all that changed (ScheduledLocalGeometryChanged = @false),
      then this means that model edges structure remains the same
      (this is helpful e.g. to avoid recalculating Manifold/BorderEdges
      in parent scene).

      @groupBegin }
    ScheduledLocalGeometryChanged: boolean;
    ScheduledLocalGeometryChangedCoord: boolean;
    { @groupEnd }

    { Looking at material, decide if it's opaque or (partially)
      transparent.

      For VRML >= 2.0, shape is transparent if material exists and
      has transparency > 0 (epsilon).

      For VRML <= 1.0, for now shape is transparent if all it's
      transparent values (in VRML 1.0, material node has actually many
      material values) have transparency > 0 (epsilon). }
    function Transparent: boolean;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    { Is shape visible, according to VRML Collision node rules.
      Ths is simply a shortcut (with more obvious name) for
      @code(State.InsideInvisible = 0). }
    function Visible: boolean;

    { Is shape collidable, according to VRML Collision node rules.
      Ths is simply a shortcut (with more obvious name) for
      @code(State.InsideIgnoreCollision = 0). }
    function Collidable: boolean;

    { Equivalent to using OctreeTriangles.RayCollision, except this
      wil use the mailbox. }
    function RayCollision(
      const Tag: TMailboxTag;
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    { Equivalent to using OctreeTriangles.SegmentCollision, except this
      wil use the mailbox. }
    function SegmentCollision(
      const Tag: TMailboxTag;
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PVRMLTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;

    { Create normals suitable for this shape.

      You can call this only for GeometryProxy being coordinate-based
      VRML geometry, implementing Coord and having non-empty coordinates
      (that is, GeometryProxy.Coord returns @true and sets ACoord <> @nil),
      and having GeometryProxy.CoordIndex <> @nil.

      GeometryProxy is the geometry of this shape. In cases when
      our @link(Geometry) field is already a coordinate-based node,
      you can and should simply pass it here. In cases when our Geometry
      is not coordinate-based, but you have a Proxy that should be used
      instead (see TVRMLGeometryNode.Proxy) you should pass here this proxy.
      We assume that geometry of this proxy is directly derived from original
      geometry defined by @link(Geometry) field --- for example,
      normals will be cached until LocalGeometryChanged (or Changed
      with PossiblyLocalGeometryChanged = @true).

      @unorderedList(
        @item(Smooth normals are perfectly smooth.
          They are per-vertex, calculated by CreateSmoothNormalsCoordinateNode.
          You can call this only for VRML coordinate-based
          GeometryProxy implementing TVRMLGeometryNode.CoordPolygons.

          As an exception, you can call this even when coords are currently
          empty (GeometryProxy.Coord returns @true but ACoord is @nil),
          then result is also @nil.)

        @item(Flat normals are per-face.
          Calculated by CreateFlatNormals.)

        @item(Finally NormalsCreaseAngle creates separate
          normal per index (auto-smoothing by CreaseAngle).)
      )

      The normals here are cached. So using these methods makes condiderable
      speedup if the shape will not change (@link(Changed) method) and
      will need normals many times (e.g. will be rendered many times).

      Normals generated always point out from CCW (FromCCW = @true
      is passed to all Create*Normals internally).

      @groupBegin }
    function NormalsSmooth(GeometryProxy: TVRMLGeometryNode): TDynVector3SingleArray;
    function NormalsFlat(GeometryProxy: TVRMLGeometryNode): TDynVector3SingleArray;
    function NormalsCreaseAngle(GeometryProxy: TVRMLGeometryNode;
      const CreaseAngle: Single): TDynVector3SingleArray;
    { @groupEnd }

    { Enumerate to callback all single texture nodes possibly used by this shape.
      This looks into appearance.texture field (and if it's MultiTexture,
      looks into it's children). And looks into shaders textures.
      Also, for VRML 1.0, looks into LastNodes.Texture2. }
    procedure EnumerateShapeTextures(Enumerate: TEnumerateShapeTexturesFunction);

    { Is the texture node Node possibly used by this shape.
      This is equivalent to checking does EnumerateShapeTextures return this shape. }
    function UsesTexture(Node: TNodeX3DTextureNode): boolean;

    { Check is shape a shadow caster. Looks at Shape's
      Appearance.shadowCaster field (see
      http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_caster). }
    function ShadowCaster: boolean;
  end;

  TObjectsListItem_2 = TVRMLShapeTree;
  {$I objectslist_2.inc}
  TVRMLShapeTreesList = TObjectsList_2;

  { Internal (non-leaf) node of the TVRMLShapeTree.
    This is practically just a list of other children
    (other TVRMLShapeTree items).

    All children are considered "active" by this class.

    This class owns it's children TVRMLShapeTree.
    Since TVRMLShapeTree is a simple tree structure, there are no duplicates
    possible, that is given TVRMLShapeTree instance may be within only
    one parent TVRMLShapeTree. (VRML node's parenting mechanism is more
    complicated than this, because of DEF/USE mechanism.) }
  TVRMLShapeTreeGroup = class(TVRMLShapeTree)
  private
    FChildren: TVRMLShapeTreesList;
  public
    constructor Create(AParentScene: TObject);
    destructor Destroy; override;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    property Children: TVRMLShapeTreesList read FChildren;
  end;

  { Node of the TVRMLShapeTree representing an alternative,
    choosing one (or none) child from it's children list as active.

    It's ideal for representing the VRML >= 2.0 Switch node
    (not possible for VRML 1.0 Switch node, as it may affect also other
    nodes after Switch). Actually, it even has a SwitchNode link that is
    used to decide which child to choose (using SwitchNode.FdWhichChoice).  }
  TVRMLShapeTreeSwitch = class(TVRMLShapeTreeGroup)
  private
    FSwitchNode: TNodeSwitch_2;
  public
    property SwitchNode: TNodeSwitch_2 read FSwitchNode write FSwitchNode;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
  end;

  { Node of the TVRMLShapeTree representing the LOD (level of detail) VRML
    concept. It chooses one child from it's children list as active.
    Represents the VRML >= 2.0 LOD node
    (not possible for VRML 1.0 LOD node, as it may affect also other
    nodes after LOD).

    To choose which child is active we need to know the LOD node,
    with it's transformation in VRML graph.
    This information is in LODNode and LODInvertedTransform properties.

    Also, we need to know the current viewer position.
    This is passed as ViewerPosition to CalculateLevel.
    Note that this class doesn't call CalculateLevel by itself, never.
    You have to call CalculateLevel, and use it to set Level property,
    from parent scene to make this LOD work. (Reasoning behind this decision:
    parent scene has LastViewerPosition and such, and parent scene
    knows whether to initiate level_changes event sending.) }
  TVRMLShapeTreeLOD = class(TVRMLShapeTreeGroup)
  private
    FLODNode: TVRMLLODNode;
    FLODInvertedTransform: TMatrix4Single;
    FLevel: Cardinal;
    FWasLevel_ChangedSend: boolean;
  public
    property LODNode: TVRMLLODNode read FLODNode write FLODNode;
    function LODInvertedTransform: PMatrix4Single;

    { Calculate @link(Level). This only calculates level, doesn't
      assign @link(Level) property or send level_changed event. }
    function CalculateLevel(const ViewerPosition: TVector3Single): Cardinal;

    { Current level, that is index of the active child of this LOD node.
      This is always < Children.Count, unless there are no children.
      In this case it's 0.

      Should be calculated by CalculateLevel. By default
      we simply use the first (highest-detail) LOD as active.
      So if you never assign this (e.g. because TVRMLScene.IsLastViewer
      = @false, that is user position is never known) we'll always
      use the highest-detail children. }
    property Level: Cardinal read FLevel write FLevel default 0;

    property WasLevel_ChangedSend: boolean
      read FWasLevel_ChangedSend write FWasLevel_ChangedSend default false;

    procedure Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
  end;

  TVRMLShapesList = class;

  { Iterates over all TVRMLShape items that would be enumerated by
    Tree.Traverse. Sometimes it's easier to write code using this iterator
    than to create callbacks and use TVRMLShapeTree.Traverse. }
  TVRMLShapeTreeIterator = class
  private
    List: TVRMLShapesList;
    CurrentIndex: Integer;
    FCurrent: TVRMLShape;
  public
    constructor Create(Tree: TVRMLShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: TVRMLShape read FCurrent;
  end;

  TObjectsListItem_1 = TVRMLShape;
  {$I objectslist_1.inc}
  TVRMLShapesList = class(TObjectsList_1)
  private
    AddedCount: Integer;
    procedure AddToList(Shape: TVRMLShape);
    procedure AddToListIfVisible(Shape: TVRMLShape);
    procedure AddToListIfCollidable(Shape: TVRMLShape);
    procedure AddToListIfVisibleAndCollidable(Shape: TVRMLShape);
  private
    SortPosition: TVector3Single;
    function IsSmallerFrontToBack(const A, B: TVRMLShape): boolean;
    function IsSmallerBackToFront(const A, B: TVRMLShape): boolean;
  public
    constructor Create;

    { Constructor that initializes list contents by traversing given tree. }
    constructor Create(Tree: TVRMLShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);

    { Sort shapes by distance to given Position point, closest first. }
    procedure SortFrontToBack(const Position: TVector3Single);

    { Sort shapes by distance to given Position point, farthest first. }
    procedure SortBackToFront(const Position: TVector3Single);
  end;

{$undef read_interface}

implementation

uses ProgressUnit, VRMLScene, VRMLErrors, NormalsCalculator, KambiLog,
  KambiStringUtils, VRMLFields;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

{ TVRMLShapeTree ------------------------------------------------------------ }

constructor TVRMLShapeTree.Create(AParentScene: TObject);
begin
  inherited Create;
  FParentScene := AParentScene;
end;

function TVRMLShapeTree.FindGeometryNodeName(
  const GeometryNodeName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.Geometry.NodeName = GeometryNodeName then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

function TVRMLShapeTree.FindShapeWithParentNamed(
  const ParentNodeName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.Geometry.TryFindParentByName(ParentNodeName) <> nil then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

function TVRMLShapeTree.FindBlenderMesh(
  const BlenderMeshName: string; OnlyActive: boolean): TVRMLShape;
var
  SI: TVRMLShapeTreeIterator;
begin
  SI := TVRMLShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;

      if { detect Blender meshes generated by VRML 1 exporter }
         ( (Result.Geometry is TVRMLGeometryNode_1) and
           (Result.Geometry.TryFindDirectParentByName(BlenderMeshName) <> nil) ) or
         { detect Blender meshes generated by VRML 2 (aka 97) exporter }
         ( (Result.State.ParentShape <> nil) and
           (Result.State.ParentShape.TryFindDirectParentByName(
             'ME_' + BlenderMeshName) <> nil) ) then
        Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

{ TVRMLShape -------------------------------------------------------------- }

constructor TVRMLShape.Create(AParentScene: TObject;
  AGeometry: TVRMLGeometryNode; AState: TVRMLGraphTraverseState);
begin
  inherited Create(AParentScene);

  FTriangleOctreeLimits := DefLocalTriangleOctreeLimits;

  FGeometry := AGeometry;
  FState := AState;

  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

destructor TVRMLShape.Destroy;
begin
  FreeAndNil(FNormals);
  FreeAndNil(FState);
  FreeOctreeTriangles;
  inherited;
end;

procedure TVRMLShape.FreeOctreeTriangles;
begin
  { secure against ParentScene = nil, since this may be called from destructor }

  if ParentScene <> nil then
    { some PVRMLTriangles became invalid }
    TVRMLScene(ParentScene).PointingDeviceClear;

  FreeAndNil(FOctreeTriangles);
end;

function TVRMLShape.TriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TVRMLShape.LocalBoundingBox: TBox3D;
begin
  if not (svLocalBBox in Validities) then
  begin
    FLocalBoundingBox := Geometry.LocalBoundingBox(State);
    Include(Validities, svLocalBBox);
  end;
  Result := FLocalBoundingBox;
end;

function TVRMLShape.BoundingBox: TBox3D;
begin
  if not (svBBox in Validities) then
  begin
    FBoundingBox := Geometry.BoundingBox(State);
    Include(Validities, svBBox);
  end;
  Result := FBoundingBox;
end;

function TVRMLShape.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
  begin
    if not (svVerticesCountOver in Validities) then
    begin
      FVerticesCount[OverTriangulate] := Geometry.VerticesCount(State, OverTriangulate);
      Include(Validities, svVerticesCountOver);
    end;
  end else
  begin
    if not (svVerticesCountNotOver in Validities) then
    begin
      FVerticesCount[OverTriangulate] := Geometry.VerticesCount(State, OverTriangulate);
      Include(Validities, svVerticesCountNotOver);
    end;
  end;
  Result := FVerticesCount[OverTriangulate];
end;

function TVRMLShape.TrianglesCount(OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
  begin
    if not (svTrianglesCountOver in Validities) then
    begin
      FTrianglesCount[OverTriangulate] := Geometry.TrianglesCount(State, OverTriangulate);
      Include(Validities, svTrianglesCountOver);
    end;
  end else
  begin
    if not (svTrianglesCountNotOver in Validities) then
    begin
      FTrianglesCount[OverTriangulate] := Geometry.TrianglesCount(State, OverTriangulate);
      Include(Validities, svTrianglesCountNotOver);
    end;
  end;
  Result := FTrianglesCount[OverTriangulate];
end;

procedure TVRMLShape.Changed(PossiblyLocalGeometryChanged: boolean);
begin
  if PossiblyLocalGeometryChanged then
  begin
    Validities := [];
    FreeAndNil(FNormals);
  end else
  begin
    { Leave in Validities things that depend on local geometry.
      Since PossiblyLocalGeometryChanged = false, they didn't change. }
    Validities := Validities * [svLocalBBox,
      svVerticesCountNotOver,  svVerticesCountOver,
      svTrianglesCountNotOver, svTrianglesCountOver,
      svNormals];
  end;
end;

procedure TVRMLShape.ValidateBoundingSphere;
begin
 if not (svBoundingSphere in Validities) then
 begin
  BoundingSphereFromBox3D(BoundingBox, FBoundingSphereCenter,
    FBoundingSphereRadiusSqr);
  Include(Validities, svBoundingSphere);
 end;
end;

function TVRMLShape.BoundingSphereCenter: TVector3Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereCenter;
end;

function TVRMLShape.BoundingSphereRadiusSqr: Single;
begin
 ValidateBoundingSphere;
 Result := FBoundingSphereRadiusSqr;
end;

function TVRMLShape.FrustumBoundingSphereCollisionPossible(
  const Frustum: TFrustum): TFrustumCollisionPossible;
begin
 ValidateBoundingSphere;
 Result := Frustum.SphereCollisionPossible(
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TVRMLShape.FrustumBoundingSphereCollisionPossibleSimple(
  const Frustum: TFrustum): boolean;
begin
 ValidateBoundingSphere;
 Result := Frustum.SphereCollisionPossibleSimple(
   FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TVRMLShape.OverrideOctreeLimits(
  const BaseLimits: TOctreeLimits): TOctreeLimits;
var
  Props: TNodeKambiOctreeProperties;
begin
  Result := BaseLimits;
  if (State.ParentShape <> nil) and
     (State.ParentShape.FdOctreeTriangles.Value <> nil) and
     (State.ParentShape.FdOctreeTriangles.Value is TNodeKambiOctreeProperties) then
  begin
    Props := TNodeKambiOctreeProperties(State.ParentShape.FdOctreeTriangles.Value);
    Props.OverrideLimits(Result);
  end;
end;

procedure TVRMLShape.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; Geometry: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, Geometry, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

function TVRMLShape.CreateTriangleOctree(
  const ALimits: TOctreeLimits;
  const ProgressTitle: string): TVRMLTriangleOctree;
begin
  Result := TVRMLTriangleOctree.Create(ALimits, LocalBoundingBox);
  try
    Result.Triangles.AllowedCapacityOverflow := TrianglesCount(false);
    try
      if (ProgressTitle <> '') and
         (Progress.UserInterface <> nil) and
         (not Progress.Active) then
      begin
        Progress.Init(TrianglesCount(false), ProgressTitle, true);
        try
          TriangleOctreeToAdd := Result;
          Geometry.LocalTriangulate(State, false,  @AddTriangleToOctreeProgress);
        finally Progress.Fini end;
      end else
        Geometry.LocalTriangulate(State, false,  @Result.AddItemTriangle);
    finally
      Result.Triangles.AllowedCapacityOverflow := 4;
    end;
  except Result.Free; raise end;
end;

procedure TVRMLShape.SetSpatial(const Value: TVRMLShapeSpatialStructures);
var
  Old, New: boolean;
begin
  if Value <> Spatial then
  begin
    { Handle OctreeTriangles }

    Old := ssTriangles in Spatial;
    New := ssTriangles in Value;

    if Old and not New then
    begin
      FreeOctreeTriangles;
    end else
    if New and not Old then
    begin
      FOctreeTriangles := CreateTriangleOctree(
        OverrideOctreeLimits(FTriangleOctreeLimits),
        TriangleOctreeProgressTitle);
    end;

    FSpatial := Value;
  end;
end;

procedure TVRMLShape.LocalGeometryChanged;
begin
  { Remember to do FreeAndNil on octrees below.
    Although we will recreate octrees right after rebuilding,
    it's still good to nil them right after freeing.
    Otherwise, when exception will raise from CreateXxxOctree,
    Scene.OctreeXxx will be left as invalid pointer. }

  if OctreeTriangles <> nil then
  begin
    FreeOctreeTriangles;
    FOctreeTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits),
      TriangleOctreeProgressTitle);
  end;

  { Remove cached normals }
  FreeAndNil(FNormals);
  Exclude(Validities, svNormals);

  { Remove from Validities things that depend on geometry.
    Local geometry change means that also global (world-space) geometry changed. }
  Validities := Validities - [svLocalBBox, svBBox,
    svVerticesCountNotOver,  svVerticesCountOver,
    svTrianglesCountNotOver, svTrianglesCountOver,
    svBoundingSphere,
    svNormals];
end;

function TVRMLShape.Transparent: boolean;
var
  M: TNodeMaterial_2;
begin
  { For VRML 1.0, there may be multiple materials on a node.
    Some of them may be transparent, some not --- we arbitrarily
    decide for now that AllMaterialsTransparent decides whether
    blending should be used or not. We may change this in th
    future to AnyMaterialsTransparent, since this will be more
    consistent with X3D ColorRGBA treatment?

    We do not try to split node into multiple instances.
    This is difficult and memory-consuming task, so we just
    depend on VRML author to split his geometry nodes if he
    wants it.

    Obviously, we also drop the idea of splitting the geometry
    into separate triangles and deciding whether to use blending
    for each separate triangle. Or to sort every separate triangle.
    This would obviously get very very slow for models with lots
    of triangles.
  }

  if State.ParentShape <> nil then
  begin
    M := State.ParentShape.Material;
    Result := (M <> nil) and (M.FdTransparency.Value > SingleEqualityEpsilon);
  end else
    Result := State.LastNodes.Material.AllMaterialsTransparent;
end;

procedure TVRMLShape.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
begin
  Func(Self);
end;

function TVRMLShape.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  if ((not OnlyVisible) or Visible) and
     ((not OnlyCollidable) or Collidable) then
    Result := 1 else
    Result := 0;
end;

function TVRMLShape.Visible: boolean;
begin
  Result := State.InsideInvisible = 0;
end;

function TVRMLShape.Collidable: boolean;
begin
  Result := State.InsideIgnoreCollision = 0;
end;

function TVRMLShape.RayCollision(
  const Tag: TMailboxTag;
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = Tag then
  begin
    Result := MailboxResult;
    if Result <> nil then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    Result := OctreeTriangles.RayCollision(
      Intersection, IntersectionDistance, Ray0, RayVector,
      ReturnClosestIntersection,
      TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);

  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
    { save result to mailbox }
    MailboxSavedTag := Tag;
    MailboxResult := Result;
    if Result <> nil then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TVRMLShape.SegmentCollision(
  const Tag: TMailboxTag;
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PVRMLTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PVRMLTriangle;
begin
  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = Tag then
  begin
    Result := MailboxResult;
    if Result <> nil then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    Result := OctreeTriangles.SegmentCollision(
      Intersection, IntersectionDistance, Pos1, Pos2,
      ReturnClosestIntersection,
      TriangleToIgnore, IgnoreMarginAtStart, TrianglesToIgnoreFunc);

  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
    { save result to mailbox }
    MailboxSavedTag := Tag;
    MailboxResult := Result;
    if Result <> nil then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TVRMLShape.NormalsSmooth(GeometryProxy: TVRMLGeometryNode): TDynVector3SingleArray;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncSmooth)) then
  begin
    if Log then
      WritelnLog('Normals', 'Calculating shape smooth normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    FNormals := CreateSmoothNormalsCoordinateNode(GeometryProxy, State, true);
    FNormalsCached := ncSmooth;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TVRMLShape.NormalsFlat(GeometryProxy: TVRMLGeometryNode): TDynVector3SingleArray;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncFlat)) then
  begin
    if Log then
      WritelnLog('Normals', 'Calculating shape flat normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    FNormals := CreateFlatNormals(GeometryProxy.CoordIndex.Items,
      GeometryProxy.Coordinates(State).Items, true);
    FNormalsCached := ncFlat;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TVRMLShape.NormalsCreaseAngle(GeometryProxy: TVRMLGeometryNode;
  const CreaseAngle: Single): TDynVector3SingleArray;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncCreaseAngle) and
          (FNormalsCreaseAngle = CreaseAngle)) then
  begin
    if Log then
      WritelnLog('Normals', 'Calculating shape CreaseAngle normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    FNormals := CreateNormals(GeometryProxy.CoordIndex.Items,
      GeometryProxy.Coordinates(State).Items, CreaseAngle, true);
    FNormalsCached := ncCreaseAngle;
    FNormalsCreaseAngle := CreaseAngle;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

procedure TVRMLShape.EnumerateShapeTextures(Enumerate: TEnumerateShapeTexturesFunction);

  procedure HandleSingleTextureNode(Tex: TVRMLNode);
  begin
    if (Tex <> nil) and
       (Tex is TNodeX3DTextureNode) then
      Enumerate(Self, TNodeX3DTextureNode(Tex));
  end;

  procedure HandleTextureNode(Tex: TVRMLNode);
  var
    I: Integer;
  begin
    if (Tex <> nil) and
       (Tex is TNodeMultiTexture) then
    begin
      for I := 0 to TNodeMultiTexture(Tex).FdTexture.Items.Count - 1 do
        HandleSingleTextureNode(TNodeMultiTexture(Tex).FdTexture.Items.Items[I]);
    end else
      HandleSingleTextureNode(Tex);
  end;

  { Scan IDecls for SFNode and MFNode fields, handling texture nodes inside. }
  procedure HandleShaderFields(IDecls: TVRMLInterfaceDeclarationsList);
  var
    I, J: Integer;
    UniformField: TVRMLField;
  begin
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          HandleTextureNode(TSFNode(UniformField).Value);
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
            HandleTextureNode(TMFNode(UniformField).Items[J]);
        end;
      end;
    end;
  end;

var
  ComposedShader: TNodeComposedShader;
  I: Integer;
begin
  HandleTextureNode(State.LastNodes.Texture2);

  if (State.ParentShape <> nil) and
     (State.ParentShape.Appearance <> nil) then
  begin
    HandleTextureNode(State.ParentShape.Appearance.FdTexture.Value);

    for I := 0 to State.ParentShape.Appearance.FdShaders.Items.Count - 1 do
    begin
      ComposedShader := State.ParentShape.Appearance.GLSLShader(I);
      if ComposedShader <> nil then
        HandleShaderFields(ComposedShader.InterfaceDeclarations);
    end;
  end;
end;

type
  TUsesTextureHelper = class
    Node: TNodeX3DTextureNode;
    procedure HandleTexture(Shape: TVRMLShape; Texture: TNodeX3DTextureNode);
  end;

  BreakUsesTexture = class(TCodeBreaker);

procedure TUsesTextureHelper.HandleTexture(Shape: TVRMLShape;
  Texture: TNodeX3DTextureNode);
begin
  if Texture = Node then
    raise BreakUsesTexture.Create;
end;

function TVRMLShape.UsesTexture(Node: TNodeX3DTextureNode): boolean;
var
  Helper: TUsesTextureHelper;
begin
  Helper := TUsesTextureHelper.Create;
  try
    Helper.Node := Node;
    try
      EnumerateShapeTextures(@Helper.HandleTexture);
      Result := false;
    except
      on BreakUsesTexture do Result := true;
    end;
  finally Helper.Free end;
end;

function TVRMLShape.ShadowCaster: boolean;
var
  S: TNodeX3DShapeNode;
  A: TVRMLNode;
begin
  Result := true;

  S := State.ParentShape;
  if S <> nil then
  begin
    A := S.FdAppearance.Value;
    if (A <> nil) and
       (A is TNodeAppearance) then
      Result := TNodeAppearance(A).FdShadowCaster.Value;
  end;
end;

{ TVRMLShapeTreeGroup -------------------------------------------------------- }

constructor TVRMLShapeTreeGroup.Create(AParentScene: TObject);
begin
  inherited Create(AParentScene);
  FChildren := TVRMLShapeTreesList.Create;
end;

destructor TVRMLShapeTreeGroup.Destroy;
begin
  FreeWithContentsAndNil(FChildren);
  inherited;
end;

procedure TVRMLShapeTreeGroup.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].Traverse(Func, OnlyActive);
end;

function TVRMLShapeTreeGroup.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
var
  I: Integer;
  ResultPart: Cardinal;
begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do
  begin
    { Workaround for http://bugs.freepascal.org/bug_view_page.php?bug_id=14403
      Without using ResultPart to hold partial result, this raises range check error. }
    ResultPart := FChildren.Items[I].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);
    Result += ResultPart;
  end;
end;

{ TVRMLShapeTreeSwitch ------------------------------------------------------- }

procedure TVRMLShapeTreeSwitch.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Children.Items[WhichChoice].Traverse(Func, OnlyActive);
  end else
    inherited;
end;

function TVRMLShapeTreeSwitch.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Result := Children.Items[WhichChoice].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable) else
      Result := 0;
  end else
    Result := inherited;
end;

{ TVRMLShapeTreeLOD ------------------------------------------------------- }

function TVRMLShapeTreeLOD.LODInvertedTransform: PMatrix4Single;
begin
  Result := @FLODInvertedTransform;
end;

function TVRMLShapeTreeLOD.CalculateLevel(const ViewerPosition: TVector3Single): Cardinal;
var
  Viewer: TVector3Single;
  Dummy: Single;
begin
  if (Children.Count = 0) or
     (LODNode.FdRange.Count = 0) then
    Result := 0 else
  begin
    try
      Viewer := MatrixMultPoint(LODInvertedTransform^, ViewerPosition);
      Result := KeyRange(LODNode.FdRange.Items,
        PointsDistance(Viewer, LODNode.FdCenter.Value), Dummy);
      { Now we know Result is between 0..LODNode.FdRange.Count.
        Following X3D spec "Specifying too few levels will result in
        the last level being used repeatedly for the lowest levels of detail",
        so just clamp to last children. }
      MinTo1st(Result, Children.Count - 1);
    except
      on E: ETransformedResultInvalid do
      begin
        VRMLWarning(vwSerious, Format('Cannot transform viewer position %s to LOD node local coordinate space, transformation results in direction (not point): %s',
          [ VectorToRawStr(ViewerPosition), E.Message ]));
        Result := 0;
      end;
    end;
  end;

  Assert(
    ( (Children.Count = 0) and (Result = 0) ) or
    ( (Children.Count > 0) and (Result < Cardinal(Children.Count)) ) );
end;

procedure TVRMLShapeTreeLOD.Traverse(Func: TShapeTraverseFunc; OnlyActive: boolean);
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Children.Items[Level].Traverse(Func, OnlyActive) else
      inherited;
  end;
end;

function TVRMLShapeTreeLOD.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Result := Children.Items[Level].ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable) else
      Result := inherited;
  end else
    Result := 0;
end;

{ TVRMLShapeTreeIterator ----------------------------------------------------- }

{ Current implementation is very simple: just call Tree.Traverse,
  collecting shapes to a list in constructor. Then simply iterate
  over this list.

  I *did* implement once more sophisticated TVRMLShapeTreeIterator version,
  that was traversing one step further in each GetNext.
  It was building a simple stack of items to make efficient push/pop while
  walking down/up the tree of TVRMLShapesTree.

  See SVN (on sourceforge) revision 3839 (local kambi disk:
  ~/sources/vrmlengine/private/old/vrmlshape_test.pas).

  Theoretically, the sophisticated version was supposed to be much better,
  as memory use was much smaller (only the depth of the shapes tree,
  while currently it's the number of all leaves).
  And speed was supposed to be better: O(1) constructor
  and GetNext all the time (while currently constructor is O(leaves count)).

  In practice however, it turned out that this sophisticated version
  was useless. Time measures shown that current "naive" and simple
  version is even very very slightly faster in some cases.
  Time measure is in kambi_vrml_game_engine/tests/testvrmlscene.pas,
  define ITERATOR_SPEED_TEST and test for yourself.

  So in practice good memory allocator in FPC
  (as this is the bottleneck of the naive version, since List is potentially
  resized on adding each new shape) outperforms the sophisticated algorithm.

  So right now we're back to simple version. Maybe the "sophisticated"
  implementation will be restored some day... }

constructor TVRMLShapeTreeIterator.Create(Tree: TVRMLShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  inherited Create;
  List := TVRMLShapesList.Create(Tree, OnlyActive, OnlyVisible, OnlyCollidable);
  CurrentIndex := -1;
end;

destructor TVRMLShapeTreeIterator.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

function TVRMLShapeTreeIterator.GetNext: boolean;
begin
  Inc(CurrentIndex);
  Result := CurrentIndex < List.Count;
  if Result then
    FCurrent := List.Items[CurrentIndex];
end;

{ TVRMLShapesList ------------------------------------------------------- }

constructor TVRMLShapesList.Create;
begin
  inherited;
end;

constructor TVRMLShapesList.Create(Tree: TVRMLShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  Create;

  { We know exactly how many shapes are present. So set Count once,
    calculating by ShapesCount. This will be faster than resizing
    in each AddToList. (Confirmed e.g. by profiling change_vrml_by_code_2). }
  AddedCount := 0;
  Count := Tree.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);

  if OnlyVisible and OnlyCollidable then
    Tree.Traverse(@AddToListIfVisibleAndCollidable, OnlyActive) else
  if OnlyVisible then
    Tree.Traverse(@AddToListIfVisible, OnlyActive) else
  if OnlyCollidable then
    Tree.Traverse(@AddToListIfCollidable, OnlyActive) else
    Tree.Traverse(@AddToList, OnlyActive);

  Assert(AddedCount = Count);
end;

procedure TVRMLShapesList.AddToList(Shape: TVRMLShape);
begin
  Items[AddedCount] := Shape;
  Inc(AddedCount);
end;

procedure TVRMLShapesList.AddToListIfVisible(Shape: TVRMLShape);
begin
  if Shape.Visible then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

procedure TVRMLShapesList.AddToListIfCollidable(Shape: TVRMLShape);
begin
  if Shape.Collidable then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

procedure TVRMLShapesList.AddToListIfVisibleAndCollidable(Shape: TVRMLShape);
begin
  if Shape.Visible and Shape.Collidable then
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;
end;

function TVRMLShapesList.IsSmallerFrontToBack(const A, B: TVRMLShape): boolean;
begin
  { We always treat empty box as closer than non-empty.
    And two empty boxes are always equal.

    Remember that code below must make sure that IsSmaller = always false
    for equal elements (our Sort depends on this). So A < B only when:
    - A empty, and B non-empty
    - both non-empty, and A closer }

  Result := (not IsEmptyBox3D(B.BoundingBox)) and
    ( IsEmptyBox3D(A.BoundingBox) or
      ( PointsDistanceSqr(Box3DMiddle(A.BoundingBox), SortPosition) <
        PointsDistanceSqr(Box3DMiddle(B.BoundingBox), SortPosition)));
end;

function TVRMLShapesList.IsSmallerBackToFront(const A, B: TVRMLShape): boolean;
begin
  Result := (not IsEmptyBox3D(A.BoundingBox)) and
    ( IsEmptyBox3D(B.BoundingBox) or
      ( PointsDistanceSqr(Box3DMiddle(A.BoundingBox), SortPosition) >
        PointsDistanceSqr(Box3DMiddle(B.BoundingBox), SortPosition)));
end;

procedure TVRMLShapesList.SortFrontToBack(const Position: TVector3Single);
begin
  SortPosition := Position;
  Sort(@IsSmallerFrontToBack);
end;

procedure TVRMLShapesList.SortBackToFront(const Position: TVector3Single);
begin
  SortPosition := Position;
  Sort(@IsSmallerBackToFront);
end;

end.
