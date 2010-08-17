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

  TEnumerateShapeTexturesFunction = procedure (Shape: TVRMLShape;
    Texture: TNodeX3DTextureNode) of object;

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

    { Look for shape with Geometry that has a parent named ParentNodeName.
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

      Note that FindBlenderObject would be theoreticall possible too,
      but Blender VRML 1.0 exporter doesn't export anywhere Blender object
      name. So when working with VRML 1.0, you're stuck with looking
      for mesh names. }
    function FindBlenderMesh(const BlenderMeshName: string;
      OnlyActive: boolean = false): TVRMLShape;

    { Enumerate all single texture nodes (possibly) used by the shapes.
      This looks into all shapes (not only active, so e.g. it looks into all
      Switch/LOD children, not only the chosen one).

      This looks into the Appearance.texture field (and if it's MultiTexture,
      looks into it's children). Also it looks into shaders textures.
      Also, for VRML 1.0, looks into LastNodes.Texture2. }
    procedure EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction); virtual; abstract;
  end;

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
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;
    FOriginalGeometry: TVRMLGeometryNode;
    FOriginalState: TVRMLGraphTraverseState;
    { FGeometry[false] should be nil exactly when FState[false] is nil.
      Same for FGeometry[true] and FState[true]. }
    FGeometry: array [boolean] of TVRMLGeometryNode;
    FState: array [boolean] of TVRMLGraphTraverseState;

    FBlenderObjectNode: TVRMLNode;
    FBlenderObjectName: string;
    FBlenderMeshNode: TVRMLNode;
    FBlenderMeshName: string;

    { Just like Geometry() and State(), except return @nil if no proxy available
      (when Geometry would return the same thing as OriginalGeometry).
      @groupBegin }
    function ProxyGeometry(const OverTriangulate: boolean): TVRMLGeometryNode;
    function ProxyState(const OverTriangulate: boolean): TVRMLGraphTraverseState;
    { @groupEnd }

    procedure ValidateBoundingSphere;

    { Make both FGeometry[OverTriangulate] and FState[OverTriangulate] set.
      Uses appropriate Proxy calls to initialize them. }
    procedure ValidateGeometryState(const OverTriangulate: boolean);

    { Make both FGeometry and FState nil (unset),
      freeing eventual instances created by Proxy methods.
      Next Geometry() or State() call will cause Proxy to be recalculated. }
    procedure FreeProxy;
  private
    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      Shape: TObject; State: TVRMLGraphTraverseState;
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
      AOriginalGeometry: TVRMLGeometryNode; AOriginalState: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
    destructor Destroy; override;

    { Original geometry node, that you get from a VRML/X3D graph. }
    property OriginalGeometry: TVRMLGeometryNode read FOriginalGeometry;

    { Original state, that you get from a VRML/X3D graph. }
    property OriginalState: TVRMLGraphTraverseState read FOriginalState;

    { Geometry of this shape.
      This may come from initial VRML/X3D node graph (see OriginalGeometry),
      or it may be processed by @link(TVRMLGeometryNode.Proxy)
      for easier handling. }
    function Geometry(const OverTriangulate: boolean = true): TVRMLGeometryNode;

    { State of this shape.
      This may come from initial VRML/X3D node graph (see OriginalState),
      or it may be processed by @link(TVRMLGeometryNode.Proxy)
      for easier handling.

      Owned by this TVRMLShape class. }
    function State(const OverTriangulate: boolean = true): TVRMLGraphTraverseState;

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

    { Looking at material and color nodes, decide if the shape is opaque
      or (partially) transparent.

      For VRML >= 2.0, shape is transparent if material exists and
      has transparency > 0 (epsilon). It's also transparent if it has
      ColorRGBA node inside "color" field.

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

      You can call this only when Geometry is coordinate-based
      VRML geometry, implementing Coord and having non-empty coordinates
      (that is, Geometry.Coord returns @true and sets ACoord <> @nil),
      and having Geometry.CoordIndex <> @nil.

      @unorderedList(
        @item(Smooth normals are perfectly smooth.
          They are per-vertex, calculated by CreateSmoothNormalsCoordinateNode.
          You can call this only for VRML coordinate-based
          Geometry implementing TVRMLGeometryNode.CoordPolygons.

          As an exception, you can call this even when coords are currently
          empty (Geometry.Coord returns @true but ACoord is @nil),
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

      Note that this always uses Geometry with OverTriangulate = @true.

      @groupBegin }
    function NormalsSmooth: TDynVector3SingleArray;
    function NormalsFlat: TDynVector3SingleArray;
    function NormalsCreaseAngle(const CreaseAngle: Single): TDynVector3SingleArray;
    { @groupEnd }

    procedure EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction); override;

    { Is the texture node Node possibly used by this shape.
      This is equivalent to checking does EnumerateShapeTextures return this shape. }
    function UsesTexture(Node: TNodeX3DTextureNode): boolean;

    { Check is shape a shadow caster. Looks at Shape's
      Appearance.shadowCaster field (see
      http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_caster). }
    function ShadowCaster: boolean;

    { Triangulate shape.

      These are comfortable and efficient wrappers over calling
      TVRMLGeometryNode.Triangulate and TVRMLGeometryNode.LocalTriangulate
      on Geometry.

      @groupBegin }
    procedure Triangulate(OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
    procedure LocalTriangulate(OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
    { @groupEnd }

    { For scenes exported from Blender, get Blender object/mesh names
      and nodes. Works assuming that this VRML scene was created by
      Blender VRML 1.0 or 2.0 exporter.

      This is useful if you e.g. know that your game data is exported
      from Blender, and you want to do some VRML/X3D processing tricks in your
      game (for example, treat some Blender objects specially).
      Of course, this is completely optional, our engine is completely
      independent from Blender and no engine feature depends on it.

      Note that a single BlenderObjectNode and BlenderObjectName may correspond
      to many VRML shapes (for example, VRML 2.0 exporter may split
      one Blender object with many materials into multiple VRML shapes).
      Also mesh may occur many times in the file, as both Blender exporters
      correctly use VRML DEF/USE mechanism to reuse mesh data (just like
      Blender itself does), so the same BlenderMeshNode
      and BlenderMeshName may be found in many shapes.

      Note that Blender VRML 1.0
      exporter doesn't record anywhere object names (only mesh names),
      so BlenderObjectName is always '' for VRML 1.0.
      For VRML 2.0 exporter it's Ok.

      Implementation of this follows the logic of Blender VRML 1.0 and 2.0
      standard exporters, there's no other way to implement this.
      E.g. if you wrote in Python your own Blender exporter for VRML,
      it cannot magically work with the properties below.

      @groupBegin }
    property BlenderObjectNode: TVRMLNode read FBlenderObjectNode;
    property BlenderObjectName: string read FBlenderObjectName;
    property BlenderMeshNode: TVRMLNode read FBlenderMeshNode;
    property BlenderMeshName: string read FBlenderMeshName;
    { @groupEnd }
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

    procedure EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction); override;
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
      if Result.OriginalGeometry.NodeName = GeometryNodeName then Exit;
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
      if Result.OriginalGeometry.TryFindParentByName(ParentNodeName) <> nil then Exit;
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
      if SI.Current.BlenderMeshName = BlenderMeshName then
        Exit(SI.Current);
  finally FreeAndNil(SI) end;
  Result := nil;
end;

{ TVRMLShape -------------------------------------------------------------- }

constructor TVRMLShape.Create(AParentScene: TObject;
  AOriginalGeometry: TVRMLGeometryNode; AOriginalState: TVRMLGraphTraverseState;
  ParentInfo: PTraversingInfo);

  procedure CalculateBlender;
  begin
    if OriginalGeometry is TVRMLGeometryNode_1 then
    begin
      { Shape node generated by Blender VRML 1.0 exporter should have
        one parent, and this is his mesh. This mesh may have may
        parents, and these are his objects. }
      if ParentInfo <> nil then
      begin
        FBlenderMeshNode := ParentInfo^.Node;
        FBlenderMeshName := BlenderMeshNode.NodeName;

        ParentInfo := ParentInfo^.ParentInfo;

        if ParentInfo <> nil then
        begin
          FBlenderObjectNode := ParentInfo^.Node;
          { Unfortunately, this will always be ''. Blender VRML 1.0 exporter
            doesn't write this. }
          FBlenderObjectName := BlenderObjectNode.NodeName;
        end;
      end;
    end else
    if (OriginalState.ShapeNode <> nil) and (ParentInfo <> nil) then
    begin
      { For VRML 2.0 exporter, the situation is actually quite similar, but
        we have to remove ME_ and OB_ prefixes from node names.
        Oh, and VRML 2.0 exporter actually does write object names.

        We look at parent Shape node.
        That's how VRML 2.0 Blender exporter writes. }

      ParentInfo := ParentInfo^.ParentInfo;

      if ParentInfo <> nil then
      begin
        FBlenderMeshNode := ParentInfo^.Node;
        FBlenderMeshName := PrefixRemove('ME_', BlenderMeshNode.NodeName, false);

        ParentInfo := ParentInfo^.ParentInfo;

        if ParentInfo <> nil then
        begin
          FBlenderObjectNode := ParentInfo^.Node;
          FBlenderObjectName := PrefixRemove('OB_', BlenderObjectNode.NodeName, false);
        end;
      end;
    end;
  end;

begin
  inherited Create(AParentScene);

  FTriangleOctreeLimits := DefLocalTriangleOctreeLimits;

  FOriginalGeometry := AOriginalGeometry;
  FOriginalState := AOriginalState;

  CalculateBlender;

  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

destructor TVRMLShape.Destroy;
begin
  FreeProxy;
  FreeAndNil(FNormals);
  FreeAndNil(FOriginalState);
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
    FLocalBoundingBox := OriginalGeometry.LocalBoundingBox(OriginalState,
      ProxyGeometry(false), ProxyState(false));
    Include(Validities, svLocalBBox);
  end;
  Result := FLocalBoundingBox;
end;

function TVRMLShape.BoundingBox: TBox3D;
begin
  if not (svBBox in Validities) then
  begin
    FBoundingBox := OriginalGeometry.BoundingBox(OriginalState,
      ProxyGeometry(false), ProxyState(false));
    Include(Validities, svBBox);
  end;
  Result := FBoundingBox;
end;

function TVRMLShape.VerticesCount(OverTriangulate: boolean): Cardinal;

  procedure Calculate;
  begin
    FVerticesCount[OverTriangulate] := OriginalGeometry.VerticesCount(
      OriginalState, OverTriangulate,
      ProxyGeometry(OverTriangulate),
      ProxyState(OverTriangulate));
  end;

begin
  if OverTriangulate then
  begin
    if not (svVerticesCountOver in Validities) then
    begin
      Calculate;
      Include(Validities, svVerticesCountOver);
    end;
  end else
  begin
    if not (svVerticesCountNotOver in Validities) then
    begin
      Calculate;
      Include(Validities, svVerticesCountNotOver);
    end;
  end;
  Result := FVerticesCount[OverTriangulate];
end;

function TVRMLShape.TrianglesCount(OverTriangulate: boolean): Cardinal;

  procedure Calculate;
  begin
    FTrianglesCount[OverTriangulate] := OriginalGeometry.TrianglesCount(
      OriginalState, OverTriangulate,
      ProxyGeometry(OverTriangulate),
      ProxyState(OverTriangulate));
  end;

begin
  if OverTriangulate then
  begin
    if not (svTrianglesCountOver in Validities) then
    begin
      Calculate;
      Include(Validities, svTrianglesCountOver);
    end;
  end else
  begin
    if not (svTrianglesCountNotOver in Validities) then
    begin
      Calculate;
      Include(Validities, svTrianglesCountNotOver);
    end;
  end;
  Result := FTrianglesCount[OverTriangulate];
end;

procedure TVRMLShape.FreeProxy;
begin
  if FGeometry[false] <> OriginalGeometry then
  begin
    if FGeometry[true] = FGeometry[false] then
      { Then either both FGeometry[] are nil (in which case we do no harm
        by code below) or they are <> nil because
        ProxyUsesOverTriangulate = false. In the 2nd case, we should
        avoid freeing the same instance twice. }
      FGeometry[true] := nil;

    FreeAndNil(FGeometry[false]);
  end else
    FGeometry[false] := nil;

  if FGeometry[true] <> OriginalGeometry then
    FreeAndNil(FGeometry[true]) else
    FGeometry[true] := nil;

  if FState[false] <> OriginalState then
  begin
    if FState[true] = FState[false] then FState[true] := nil;
    FreeAndNil(FState[false]);
  end else
    FState[false] := nil;

  if FState[true] <> OriginalState then
    FreeAndNil(FState[true]) else
    FState[true] := nil;

  Assert(FGeometry[false] = nil);
  Assert(FGeometry[true] = nil);
  Assert(FState[false] = nil);
  Assert(FState[true] = nil);
end;

procedure TVRMLShape.Changed(PossiblyLocalGeometryChanged: boolean);
begin
  FreeProxy;

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
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.FdOctreeTriangles.Value <> nil) and
     (State.ShapeNode.FdOctreeTriangles.Value is TNodeKambiOctreeProperties) then
  begin
    Props := TNodeKambiOctreeProperties(State.ShapeNode.FdOctreeTriangles.Value);
    Props.OverrideLimits(Result);
  end;
end;

procedure TVRMLShape.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  Shape: TObject; State: TVRMLGraphTraverseState;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, Shape, State, MatNum,
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
          LocalTriangulate(false, @AddTriangleToOctreeProgress);
        finally Progress.Fini end;
      end else
        LocalTriangulate(false, @Result.AddItemTriangle);
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
  if State.ShapeNode <> nil then
  begin
    M := State.ShapeNode.Material;
    Result := (M <> nil) and (M.FdTransparency.Value > SingleEqualityEpsilon);
  end else
    { For VRML 1.0, there may be multiple materials on a node.
      Some of them may be transparent, some not --- we arbitrarily
      decide for now that AllMaterialsTransparent decides whether
      blending should be used or not. We may change this in the
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
      of triangles.  }
    Result := State.LastNodes.Material.AllMaterialsTransparent;

  if Geometry.ColorRGBA <> nil then
    Result := true;
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

function TVRMLShape.NormalsSmooth: TDynVector3SingleArray;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncSmooth)) then
  begin
    if Log then
      WritelnLog('Normals', 'Calculating shape smooth normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    FNormals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
    FNormalsCached := ncSmooth;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TVRMLShape.NormalsFlat: TDynVector3SingleArray;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncFlat)) then
  begin
    if Log then
      WritelnLog('Normals', 'Calculating shape flat normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    FNormals := CreateFlatNormals(Geometry.CoordIndex.Items,
      Geometry.Coordinates(State).Items, true);
    FNormalsCached := ncFlat;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TVRMLShape.NormalsCreaseAngle(const CreaseAngle: Single): TDynVector3SingleArray;
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

    FNormals := CreateNormals(Geometry.CoordIndex.Items,
      Geometry.Coordinates(State).Items, CreaseAngle, true);
    FNormalsCached := ncCreaseAngle;
    FNormalsCreaseAngle := CreaseAngle;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

procedure TVRMLShape.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction);

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

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    HandleTextureNode(State.ShapeNode.Appearance.FdTexture.Value);

    for I := 0 to State.ShapeNode.Appearance.FdShaders.Items.Count - 1 do
    begin
      ComposedShader := State.ShapeNode.Appearance.GLSLShader(I);
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
      EnumerateTextures(@Helper.HandleTexture);
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

  S := State.ShapeNode;
  if S <> nil then
  begin
    A := S.FdAppearance.Value;
    if (A <> nil) and
       (A is TNodeAppearance) then
      Result := TNodeAppearance(A).FdShadowCaster.Value;
  end;
end;

procedure TVRMLShape.ValidateGeometryState(const OverTriangulate: boolean);
begin
  if FGeometry[OverTriangulate] = nil then
  begin
    Assert(FState[OverTriangulate] = nil);
    FState[OverTriangulate] := OriginalState;

    FGeometry[OverTriangulate] := OriginalGeometry.Proxy(
      FState[OverTriangulate], OverTriangulate);

    if FGeometry[OverTriangulate] <> nil then
    begin
      { We just used OriginalGeometry.Proxy successfully.
        Let's now check can we fill the over FGeometry/FState[] value for free.
        If ProxyUsesOverTriangulate = false, then we can reuse
        this Proxy. This may save us from unnecessarily calling Proxy
        second time. }
      if (FGeometry[not OverTriangulate] = nil) and
          not OriginalGeometry.ProxyUsesOverTriangulate then
      begin
        Assert(FState[not OverTriangulate] = nil);
        FGeometry[not OverTriangulate] := FGeometry[OverTriangulate];
        FState   [not OverTriangulate] := FState   [OverTriangulate];
      end;
    end else
    begin
      FGeometry[OverTriangulate] := OriginalGeometry;
      FState   [OverTriangulate] := OriginalState;
    end;
  end;
end;

function TVRMLShape.Geometry(const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  ValidateGeometryState(OverTriangulate);
  Result := FGeometry[OverTriangulate];
end;

function TVRMLShape.State(const OverTriangulate: boolean): TVRMLGraphTraverseState;
begin
  ValidateGeometryState(OverTriangulate);
  Result := FState[OverTriangulate];
end;

function TVRMLShape.ProxyGeometry(const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := Geometry(OverTriangulate);
  if Result = OriginalGeometry then Result := nil;
end;

function TVRMLShape.ProxyState(const OverTriangulate: boolean): TVRMLGraphTraverseState;
begin
  if Geometry(OverTriangulate) <> OriginalGeometry then
    Result := State(OverTriangulate) else
    Result := nil;
end;

procedure TVRMLShape.Triangulate(OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { Always pass the same OverTriangulate value to ProxyGeometry/State(),
    and to Triangulate(). This is sensible:

    1. if a node uses Proxy, then the OverTriangulate parameter
       to the ProxyGeometry/State() will already cause using appropriate
       triangulation during convertion to the IndexedFaceSet.
       Such IndexedFaceSet will ignore OverTriangulate parameter
       to Triangulate().

    2. if a node does not use Proxy, then ProxyGeometry/State() parameters
       don't matter. The parameter to Triangulate() then decides
       the triangulation.
  }

  OriginalGeometry.Triangulate(Self, OriginalState, OverTriangulate, NewTriangleProc,
    ProxyGeometry(OverTriangulate),
    ProxyState(OverTriangulate));
end;

procedure TVRMLShape.LocalTriangulate(OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  OriginalGeometry.LocalTriangulate(Self, OriginalState, OverTriangulate, NewTriangleProc,
    ProxyGeometry(OverTriangulate),
    ProxyState(OverTriangulate));
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

procedure TVRMLShapeTreeGroup.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].EnumerateTextures(Enumerate);
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
