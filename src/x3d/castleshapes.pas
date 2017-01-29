{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shape (TShape class) and a simple tree of shapes (TShapeTree class). }
unit CastleShapes;

{$I castleconf.inc}

{ $define SHAPE_ITERATOR_SOPHISTICATED}

{$I octreeconf.inc}

{$modeswitch nestedprocvars}{$H+}

interface

uses SysUtils, Classes, FGL,
  CastleVectors, Castle3D, CastleBoxes, X3DNodes, CastleClassUtils,
  CastleUtils, CastleInternalTriangleOctree, CastleFrustum, CastleInternalOctree,
  X3DTriangles, X3DFields, CastleGeometryArrays, CastleTriangles,
  CastleMaterialProperties, CastleShapeInternalShadowVolumes;

const
  { }
  DefLocalTriangleOctreeMaxDepth = 10;
  { Default octree leaf capacity for TShape.OctreeTriangles.

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
  { Possible spatial structure types that may be managed by TShape,
    see TShape.Spatial. }
  TShapeSpatialStructure = (
    { Create the TShape.OctreeTriangles.
      This is an octree containing all triangles. }
    ssTriangles);
  TShapeSpatialStructures = set of TShapeSpatialStructure;

  TShape = class;

  TShapeTraverseFunc = procedure (Shape: TShape) is nested;

  TEnumerateShapeTexturesFunction = function (Shape: TShape;
    Texture: TAbstractTextureNode): Pointer of object;

  TTestShapeVisibility = function (Shape: TShape): boolean of object;

  { Triangle information, called by TShape.LocalTriangulate and such.

    @param(Shape A shape containing this triangle.
      This is always an instance of TShape class, but due
      to unit dependencies it cannot be declared as such.)

    @param(Normal Normal vectors, for each triangle point.)

    @param(TexCoord Texture coordinates, for each triangle point.

      Each texture coordinate is a 4D vector, since we may have 3D textures
      referenced by 4D (homogeneous) coordinates. For normal 2D textures,
      you can simply take the first 2 components of the vector,
      and ignore the remaining 2 components. The 3th component is always
      0 if was not specified (if model had only 2D texture coords).
      The 4th component is always 1 if was not specified
      (if model had only 2D or 3D texture coords).

      In case of multi-texturing, this describes coordinates
      of the first texture unit.
      In case no texture is defined, this is undefined.)

    @param(Face Describes the indexes of this face, for editing / removing it.
      See TFaceIndex.) }
  TTriangleEvent = procedure (Shape: TObject;
    const Position: TTriangle3Single;
    const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
    const Face: TFaceIndex) of object;

  { Tree of shapes.

    Although VRML/X3D model already provides the tree (graph of VRML/X3D nodes),
    it's a little too complicated to be used at each render call.
    It's especially true for VRML <= 1.0 (where properties may "leak out"
    from one node to the next), VRML/X3D >= 2.0 cleaned a lot here but still
    some work must be done when traversing (like accumulating transformations).

    So we process VRML/X3D tree to this tree, which is much simpler with
    all the geometry nodes (TAbstractGeometryNode) along with their state
    (TX3DGraphTraverseState) as leafs (TShape). }
  TShapeTree = class
  private
    FParentScene: TObject;
  public
    constructor Create(AParentScene: TObject);

    { Parent TCastleSceneCore instance. This cannot be declared here as
      TCastleSceneCore (this would create circular unit dependency),
      but it always is TCastleSceneCore. }
    property ParentScene: TObject read FParentScene write FParentScene;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); virtual; abstract;

    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; virtual; abstract;

    { Look for shape with Geometry.X3DName = GeometryNodeName.
      Returns @nil if not found. }
    function FindGeometryNodeName(const GeometryNodeName: string;
      OnlyActive: boolean = false): TShape;

    { Look for shape with Geometry that has a parent named ParentNodeName.
      Parent is searched by Geometry.TryFindParentNodeByName.
      Returns @nil if not found. }
    function FindShapeWithParentNamed(const ParentNodeName: string;
      OnlyActive: boolean = false): TShape;

    { Enumerate all single texture nodes (possibly) used by the shapes.
      This looks into all shapes (not only active, so e.g. it looks into all
      Switch/LOD children, not only the chosen one).

      This looks into the Appearance.texture field (and if it's MultiTexture,
      looks into it's children). Also it looks into shaders textures.
      Also, for VRML 1.0, looks into LastNodes.Texture2.

      If Enumerate callbacks returns non-nil for some texture, returns it immediately,
      and stops further processing. }
    function EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer; virtual; abstract;

    function DebugInfo(const Indent: string = ''): string; virtual; abstract;
  end;

  { Shape is a geometry node @link(Geometry) instance and it's
    @link(State). For VRML >= 2.0, this usually corresponds to
    a single instance of actual VRML @code(Shape) node.
    It allows to perform many operations that need to know both geometry
    and it's current state (parent Shape node, current transformation and such).

    This class caches results of methods LocalBoundingBox, BoundingBox,
    and most others (see TShapeValidities for hints).
    This means that things work fast, but this also means that
    you must manually call @link(Changed)
    when you changed some properties of Geometry or contents of State.

    But note that you can't change Geometry or State to different
    objects --- they are readonly properties.

    Also note that if you're using @link(TCastleSceneCore) class
    then you don't have to worry about calling @link(Changed)
    of items in @link(TCastleSceneCore.Shapes).
    All you have to do is to call appropriate @code(Changed*)
    methods of @link(TCastleSceneCore). }
  TShape = class(TShapeTree)
  private
  type
    TShapeValidities = set of (svLocalBBox, svBBox,
      svVerticesCountNotOver,  svVerticesCountOver,
      svTrianglesCountNotOver, svTrianglesCountOver,
      svBoundingSphere,
      svNormals);
    TShapeNormalsCached = (ncSmooth, ncFlat, ncCreaseAngle);

    var
    FLocalBoundingBox: TBox3D;
    FBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: array [boolean] of Cardinal;
    Validities: TShapeValidities;
    FBoundingSphereCenter: TVector3Single;
    FBoundingSphereRadiusSqr: Single;
    FOriginalGeometry: TAbstractGeometryNode;
    FOriginalState: TX3DGraphTraverseState;
    { FGeometry[false] should be nil exactly when FState[false] is nil.
      Same for FGeometry[true] and FState[true]. }
    FGeometry: array [boolean] of TAbstractGeometryNode;
    FState: array [boolean] of TX3DGraphTraverseState;

    FGeometryParentNodeName,
    FGeometryGrandParentNodeName,
    FGeometryGrandGrandParentNodeName: string;

    FDynamicGeometry: boolean;

    IsCachedMaterialProperty: boolean;
    CachedMaterialProperty: TMaterialProperty;

    FShadowVolumes: TShapeShadowVolumes;

    { Just like Geometry() and State(), except return @nil if no proxy available
      (when Geometry would return the same thing as OriginalGeometry).
      @groupBegin }
    function ProxyGeometry(const OverTriangulate: boolean): TAbstractGeometryNode;
    function ProxyState(const OverTriangulate: boolean): TX3DGraphTraverseState;
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
    TriangleOctreeToAdd: TTriangleOctree;
    procedure AddTriangleToOctreeProgress(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
    function CreateTriangleOctree(const ALimits: TOctreeLimits;
      const ProgressTitle: string): TTriangleOctree;
  private
    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FOctreeTriangles: TTriangleOctree;

    FSpatial: TShapeSpatialStructures;
    procedure SetSpatial(const Value: TShapeSpatialStructures);

    function OverrideOctreeLimits(
      const BaseLimits: TOctreeLimits): TOctreeLimits;
  private
    {$ifdef SHAPE_OCTREE_USE_MAILBOX}
    { Mailbox, for speeding up collision queries.
      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxResult: PTriangle;
    MailboxIntersection: TVector3Single;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { Meaningful only when svNormals in Validities.
      Normals may be assigned only if svNormals in Validities. }
    FNormalsCached: TShapeNormalsCached;
    FNormals: TVector3SingleList;
    { Meaningful only when svNormals in Validities and
      NormalsCached = ncCreaseAngle. }
    FNormalsCreaseAngle: Single;
    FNormalsOverTriangulate: boolean;

    { Free and nil FOctreeTriangles. Also, makes sure to call
      PointingDeviceClear on ParentScene (since some PTriangle pointers
      were freed). }
    procedure FreeOctreeTriangles;
  public
    { Constructor.
      @param(ParentInfo Resursive information about parents,
        for the geometry node of given shape.
        Note that for VRML 2.0/X3D, the immediate parent
        of geometry node is always TShapeNode.) }
    constructor Create(AParentScene: TObject;
      AOriginalGeometry: TAbstractGeometryNode; AOriginalState: TX3DGraphTraverseState;
      ParentInfo: PTraversingInfo);
    destructor Destroy; override;

    { Original geometry node, that you get from a VRML/X3D graph. }
    property OriginalGeometry: TAbstractGeometryNode read FOriginalGeometry;

    { Original state, that you get from a VRML/X3D graph. }
    property OriginalState: TX3DGraphTraverseState read FOriginalState;

    { Geometry of this shape.
      This may come from initial VRML/X3D node graph (see OriginalGeometry),
      or it may be processed by @link(TAbstractGeometryNode.Proxy)
      for easier handling. }
    function Geometry(const OverTriangulate: boolean = true): TAbstractGeometryNode;

    { State of this shape.
      This may come from initial VRML/X3D node graph (see OriginalState),
      or it may be processed by @link(TAbstractGeometryNode.Proxy)
      for easier handling.

      Owned by this TShape class. }
    function State(const OverTriangulate: boolean = true): TX3DGraphTraverseState;

    { Calculate bounding box and vertices/triangles count,
      see TAbstractGeometryNode methods.
      @groupBegin }
    function LocalBoundingBox: TBox3D;
    function BoundingBox: TBox3D;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;
    { @groupEnd }

    { Decompose the geometry into primitives, with arrays of per-vertex data. }
    function GeometryArrays(OverTriangulate: boolean): TGeometryArrays;

    { Calculates bounding sphere based on BoundingBox.
      In the future this may be changed to use BoundingSphere method
      of @link(TAbstractGeometryNode), when I will implement it.
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
    function BoundingSphereRadius: Single;
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

    { Notify this shape that you changed a field inside one of it's nodes
      (automatically done by TCastleSceneCore).
      This should be called when fields within Shape.Geometry,
      Shape.State.Last*, Shape.State.ShapeNode or such change.

      Pass InactiveOnly = @true is you know that this shape is fully in
      inactive VRML graph part (inactive Switch, LOD etc. children).

      Including chTransform in Changes means something more than
      general chTransform (which means that transformation of children changed,
      which implicates many things --- not only shape changes).
      Here, chTransform in Changes means that only the transformation
      of TShape.State changed (so only on fields ignored by
      EqualsNoTransform). }
    procedure Changed(const InactiveOnly: boolean;
      const Changes: TX3DChanges); virtual;

    { @exclude
      Called when local geometry changed. Internally used to communicate
      between TCastleSceneCore and TShape.

      "Local" means that we're concerned here about changes visible
      in shape local coordinate system. E.g. things that only change our
      transformation (State.Transform) do not cause "local" geometry changes.

      "Geometry" means that we're concerned only about changes to topology
      --- vertexes, edges, faces, how they connect each other.
      Things that affect only appearance (e.g. whole Shape.appearance content
      in stuff for VRML >= 2.0) is not relevant here. E.g. changing
      material color does not cause "local" geometry changes.

      This frees the octree (will be recreated on Octree* call).
      Also removes cached normals.
      Also notifies parent scene about this change (unless CalledFromParentScene). }
    procedure LocalGeometryChanged(const CalledFromParentScene, ChangedOnlyCoord: boolean);

    { The dynamic octree containing all triangles.
      It contains only triangles within this shape.

      There is no distinction here between collidable / visible
      (as for TCastleSceneCore octrees), since only the whole shape may be
      marked as visible and/or collidable, not particular triangles.

      The triangles are specified in local coordinate system of this shape
      (that is, they are independent from transformation within State.Transform).
      This allows the tree to remain unmodified when transformation of this
      shape changes.

      This is automatically managed (initialized, updated, and used)
      by parent TCastleSceneCore. You usually don't need to know about this
      octree from outside.

      To initialize this, add ssTriangles to @link(InternalSpatial) property,
      otherwise it's @nil. Parent TCastleSceneCore will take care of this
      (when parent TCastleSceneCore.Spatial contains ssDynamicCollisions, then
      all shapes contain ssTriangles within their InternalSpatial).

      Parent TCastleSceneCore will take care to keep this octree always updated.

      Parent TCastleSceneCore will also take care of actually using
      this octree: TCastleSceneCore.OctreeCollisions methods actually use the
      octrees of specific shapes at the bottom. }
    function InternalOctreeTriangles: TTriangleOctree;

    { Which spatial structrues (octrees, for now) should be created and managed.
      This works analogous to TCastleSceneCore.Spatial, but this manages
      octrees within this TShape.

      Parent TCastleSceneCore will take care to keep this value updated,
      you should only set TCastleSceneCore.Spatial from the outside. }
    property InternalSpatial: TShapeSpatialStructures read FSpatial write SetSpatial;

    { Properties of created triangle octrees.
      See TriangleOctree unit comments for description.

      Default value comes from DefLocalTriangleOctreeLimits.

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(InternalSpatial) from []
      to something else. }
    function InternalTriangleOctreeLimits: POctreeLimits;

    { If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      (so we avoid starting "progress bar within progress bar"). }
    property InternalTriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;

    { Decide should the shape use alpha blending (partial transparency),
      looking at material, color, texture nodes (including at texture
      images contents).

      This may look at the data of the texture node,
      so it should be called before any calls to TCastleSceneCore.FreeResources.
      It checks AlphaChannel of textures, so assumes that given shape
      textures are already loaded. }
    function Blending: boolean;
    function Transparent: boolean; deprecated 'use Blending';

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
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
      const RayOrigin, RayDirection: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    { Equivalent to using OctreeTriangles.SegmentCollision, except this
      wil use the mailbox. }
    function SegmentCollision(
      const Tag: TMailboxTag;
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3Single;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;

    { Create normals suitable for this shape.

      You can call this only when Geometry is coordinate-based
      VRML geometry, implementing Coord and having non-empty coordinates
      (that is, Geometry.Coord returns @true and sets ACoord <> @nil),
      and having Geometry.CoordIndex <> @nil.

      For NormalsSmooth, also Geometry.CoordIndex = @nil is allowed,
      but make sure that Geometry.CoordPolygons is available.
      See CreateSmoothNormalsCoordinateNode.

      @unorderedList(
        @item(Smooth normals are perfectly smooth, per-vertex.

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

      @groupBegin }
    function NormalsSmooth(OverTriangulate: boolean): TVector3SingleList;
    function NormalsFlat(OverTriangulate: boolean): TVector3SingleList;
    function NormalsCreaseAngle(OverTriangulate: boolean;
      const CreaseAngle: Single): TVector3SingleList;
    { @groupEnd }

    function EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;

    { Is the texture node Node possibly used by this shape.
      This is equivalent to checking does EnumerateShapeTextures return this shape. }
    function UsesTexture(Node: TAbstractTextureNode): boolean;

    { Check is shape a shadow caster. Looks at Shape's
      Appearance.shadowCaster field (see
      http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadow_caster). }
    function ShadowCaster: boolean;

    { Triangulate shape. Calls TriangleEvent callback for each triangle.
      LocalTriangulate returns coordinates in local shape transformation
      (that is, not transformed by State.Transform yet).

      OverTriangulate determines if we should make more triangles for Gouraud
      shading. For example, it makes Cones and Cylinders divided into
      additional stacks.

      @groupBegin }
    procedure Triangulate(OverTriangulate: boolean; TriangleEvent: TTriangleEvent);
    procedure LocalTriangulate(OverTriangulate: boolean; TriangleEvent: TTriangleEvent);
    { @groupEnd }

    function DebugInfo(const Indent: string = ''): string; override;
    function NiceName: string;

    { Local geometry is treated as dynamic (changes very often, like every frame).
      This is automatically detected and set to @true, although you can also
      explicitly set this if you want.

      Dynamic geometry has worse collision detection (using a crude
      approximation) and falls back to rendering method better for
      dynamic geometry (for example, marking VBO data as dynamic for OpenGL
      rendering). }
    property DynamicGeometry: boolean read FDynamicGeometry write FDynamicGeometry;

    { Shape node in VRML/X3D graph.
      This is always present for VRML >= 2 (including X3D).
      For VRML 1.0 and Inventor this is @nil. }
    function Node: TAbstractShapeNode;

    { Node names of parents of the geometry node.
      Note that for X3D/VRML 2.0, GeometryParentNodeName is the same
      as Node.NodeName, because the parent of geometry node is always
      a TShapeNode.
      @groupBegin }
    property GeometryParentNodeName: string read FGeometryParentNodeName;
    property GeometryGrandParentNodeName: string read FGeometryGrandParentNodeName;
    property GeometryGrandGrandParentNodeName: string read FGeometryGrandGrandParentNodeName;
    { @groupEnd }

    { Material property associated with this shape's material/texture. }
    function MaterialProperty: TMaterialProperty;

    { @exclude }
    property InternalShadowVolumes: TShapeShadowVolumes read FShadowVolumes;
  end;

  TShapeTreeList = specialize TFPGObjectList<TShapeTree>;

  { Internal (non-leaf) node of the TShapeTree.
    This is practically just a list of other children
    (other TShapeTree items).

    All children are considered "active" by this class.

    This class owns it's children TShapeTree.
    Since TShapeTree is a simple tree structure, there are no duplicates
    possible, that is given TShapeTree instance may be within only
    one parent TShapeTree. (VRML node's parenting mechanism is more
    complicated than this, because of DEF/USE mechanism.) }
  TShapeTreeGroup = class(TShapeTree)
  private
    FChildren: TShapeTreeList;
  public
    constructor Create(AParentScene: TObject);
    destructor Destroy; override;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    property Children: TShapeTreeList read FChildren;

    function EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;

    {$ifdef SHAPE_ITERATOR_SOPHISTICATED}

    { Start index for TShapeTreeIterator.
      Must be >= -1 (-1 means to start from 0).

      May be >= Children.Count, even IterateBeginIndex + 1 may
      be >= Children.Count, i.e. it's Ok if this is already out of range. }
    function IterateBeginIndex(OnlyActive: boolean): Integer; virtual;

    { End index for TShapeTreeIterator. Valid indexes are < this.
      This must be <= Children.Count. }
    function IterateEndIndex(OnlyActive: boolean): Cardinal; virtual;

    {$endif}

    function DebugInfo(const Indent: string = ''): string; override;
  end;

  { Node of the TShapeTree representing an alternative,
    choosing one (or none) child from it's children list as active.

    It's ideal for representing the VRML >= 2.0 Switch node
    (not possible for VRML 1.0 Switch node, as it may affect also other
    nodes after Switch). Actually, it even has a SwitchNode link that is
    used to decide which child to choose (using SwitchNode.FdWhichChoice).  }
  TShapeTreeSwitch = class(TShapeTreeGroup)
  private
    FSwitchNode: TSwitchNode;
  public
    property SwitchNode: TSwitchNode read FSwitchNode write FSwitchNode;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    {$ifdef SHAPE_ITERATOR_SOPHISTICATED}
    function IterateBeginIndex(OnlyActive: boolean): Integer; override;
    function IterateEndIndex(OnlyActive: boolean): Cardinal; override;
    {$endif}
  end;

  { Node of the TShapeTree transforming it's children.

    It's ideal for handling VRML 2.0 / X3D Transform node,
    and similar nodes (MatrixTransform and some H-Anim nodes also act
    as a transformation node and also may be handled by this). }
  TShapeTreeTransform = class(TShapeTreeGroup)
  private
    FTransformNode: TX3DNode;
    FTransformState: TX3DGraphTraverseState;
  public
    constructor Create(AParentScene: TObject);
    destructor Destroy; override;

    { Internal note: We don't declare TransformNode as ITransformNode interface,
      because we don't want to keep reference to it too long,
      as it's manually freed. That's safer. }
    { Transforming VRML/X3D node. Always assigned, always may be casted
      to ITransformNode interface. }
    property TransformNode: TX3DNode read FTransformNode write FTransformNode;

    { State right before traversing the TransformNode.
      Owned by this TShapeTreeTransform instance. You should assign
      to it when you set TransformNode. }
    property TransformState: TX3DGraphTraverseState read FTransformState;
  end;

  { Node of the TShapeTree representing the LOD (level of detail) alternative.
    It chooses one child from it's children list as active.
    Represents the VRML >= 2.0 LOD node
    (not possible for VRML 1.0 LOD node, as it may affect also other
    nodes after LOD).

    To choose which child is active we need to know the LOD node,
    with it's transformation in VRML graph.
    This information is in LODNode and LODInvertedTransform properties.

    Also, we need to know the current camera position.
    This is passed as CameraPosition to CalculateLevel.
    Note that this class doesn't call CalculateLevel by itself, never.
    You have to call CalculateLevel, and use it to set Level property,
    from parent scene to make this LOD work. (Reasoning behind this decision:
    parent scene has CameraPosition and such, and parent scene
    knows whether to initiate level_changes event sending.) }
  TShapeTreeLOD = class(TShapeTreeGroup)
  private
    FLODNode: TAbstractLODNode;
    FLODInvertedTransform: TMatrix4Single;
    FLevel: Cardinal;
    FWasLevel_ChangedSend: boolean;
  public
    property LODNode: TAbstractLODNode read FLODNode write FLODNode;
    function LODInvertedTransform: PMatrix4Single;

    { Calculate @link(Level). This only calculates level, doesn't
      assign @link(Level) property or send level_changed event. }
    function CalculateLevel(const CameraPosition: TVector3Single): Cardinal;

    { Current level, that is index of the active child of this LOD node.
      This is always < Children.Count, unless there are no children.
      In this case it's 0.

      Should be calculated by CalculateLevel. By default
      we simply use the first (highest-detail) LOD as active.
      So if you never assign this (e.g. because TCastleSceneCore.CameraViewKnown
      = @false, that is user position is never known) we'll always
      use the highest-detail children. }
    property Level: Cardinal read FLevel write FLevel default 0;

    property WasLevel_ChangedSend: boolean
      read FWasLevel_ChangedSend write FWasLevel_ChangedSend default false;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;

    {$ifdef SHAPE_ITERATOR_SOPHISTICATED}
    function IterateBeginIndex(OnlyActive: boolean): Integer; override;
    function IterateEndIndex(OnlyActive: boolean): Cardinal; override;
    {$endif}
  end;

  TProximitySensorInstance = class(TShapeTree)
  private
    FNode: TProximitySensorNode;
  public
    InvertedTransform: TMatrix4Single;
    IsActive: boolean;

    property Node: TProximitySensorNode read FNode write FNode;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
    function EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;
    function DebugInfo(const Indent: string = ''): string; override;
  end;

  TVisibilitySensorInstance = class(TShapeTree)
  private
    FNode: TVisibilitySensorNode;
  public
    { Bounding box of this visibility sensor instance,
      already transformed to global VRML/X3D scene coordinates.
      That is, transformed by parent Transform and similar nodes. }
    Box: TBox3D;
    Transform: TMatrix4Single;

    property Node: TVisibilitySensorNode read FNode write FNode;

    procedure Traverse(Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal; override;
    function EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;
    function DebugInfo(const Indent: string = ''): string; override;
  end;

  TShapeList = class;

  { Iterates over all TShape items that would be enumerated by
    Tree.Traverse. Sometimes it's easier to write code using this iterator
    than to create callbacks and use TShapeTree.Traverse. }
  TShapeTreeIterator = class
  private
    FCurrent: TShape;
    {$ifdef SHAPE_ITERATOR_SOPHISTICATED}
    Info: Pointer;
    SingleShapeRemaining: boolean;
    FOnlyActive, FOnlyVisible, FOnlyCollidable: boolean;
    function CurrentMatches: boolean;
    {$else}
    List: TShapeList;
    CurrentIndex: Integer;
    {$endif}
  public
    constructor Create(Tree: TShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: TShape read FCurrent;
  end;

  TShapeList = class(specialize TFPGObjectList<TShape>)
  public
    constructor Create;

    { Constructor that initializes list contents by traversing given tree. }
    constructor Create(Tree: TShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);

    { Sort shapes by distance to given Position point, closest first. }
    procedure SortFrontToBack(const Position: TVector3Single);

    { Sort shapes by distance to given Position point, farthest first.

      If Distance3D is @true: we use real distance in 3D to sort.
      See the @link(bs3D) at @link(TBlendingSort) documentation.

      If Distance3D is @false: we use only the distance in the Z coordinate
      to sort. This is suitable for
      rendering things that pretend to be 2D, like Spine slots.
      See the @link(bs2D) at @link(TBlendingSort) documentation. }
    procedure SortBackToFront(const Position: TVector3Single;
      const Distance3D: boolean);
  end;

var
  { If nonzero, disables automatic TShape.DynamicGeometry detection
    on every node modification. This is useful if you do some interactive
    editing of the shape, but you don't want the shape octree to be replaced
    by it's approximation. }
  DisableAutoDynamicGeometry: Cardinal;

  { Log various information about shapes. This displays quite a lot of non-critical
    information when opening non-trivial models.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogShapes: boolean = false;

type
  { Detect the 3D placeholder name set in the external modeler,
    like 3D object name set in Blender or 3DS Max.
    Assumes that a specific modeler was used to create and export this 3D model.
    Each TPlaceholderName function is made to follow the logic of a single
    modeler, and they are gathered in PlaceholderNames.

    Returns empty string if none.

    When implementing this, you may find useful the following properties
    of the shape: TShape.OriginalGeometry.X3DName,
    TShape.Node.X3DName, TShape.GeometryParentNodeName,
    TShape.GeometryGrandParentNodeName,
    TShape.GeometryGrandGrandParentNodeName.

    Preferably, the result should be unique, only for this VRML/X3D shape.
    But in practice it's the responsibility of the modeler
    and model author to make it true.
    For example, modelers that allow multiple materials on object (like
    Blender) @italic(must) split a single 3D object into many VRML/X3D shapes
    sometimes.
    So just don't use shapes with multiple materials if this shape
    may be meaningful for a placeholder.

    This is used only by TGameSceneManager.LoadLevel placeholders.
    Ultimately, this should be something that is easy to set when creating
    a 3D model in given external modeler.
    @italic(Nothing else in our engine depends on a particular modeler
    strategy for exporting VRML/X3D models.)

    This should be object name (to allow sharing a single mesh underneath).
    Except when it's not possible (like for old Blender VRML 1.0 exporter,
    when only mesh names are stored in VRML/X3D exported files),
    in which case it can be a mesh name. }
  TPlaceholderName = function (const Shape: TShape): string;
  TPlaceholderNames = specialize TFPGMap<string, TPlaceholderName>;

var
  PlaceholderNames: TPlaceholderNames;

implementation

uses CastleProgress, CastleSceneCore, CastleInternalNormals, CastleLog,
  CastleStringUtils, CastleArraysGenerator, CastleImages, CastleURIUtils;

const
  UnknownTexCoord: TTriangle4Single = (
    (0, 0, 0, 1),
    (0, 0, 0, 1),
    (0, 0, 0, 1) );

{ TShapeTree ------------------------------------------------------------ }

constructor TShapeTree.Create(AParentScene: TObject);
begin
  inherited Create;
  FParentScene := AParentScene;
end;

function TShapeTree.FindGeometryNodeName(
  const GeometryNodeName: string; OnlyActive: boolean): TShape;
var
  SI: TShapeTreeIterator;
begin
  SI := TShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.OriginalGeometry.X3DName = GeometryNodeName then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

function TShapeTree.FindShapeWithParentNamed(
  const ParentNodeName: string; OnlyActive: boolean): TShape;
var
  SI: TShapeTreeIterator;
begin
  SI := TShapeTreeIterator.Create(Self, OnlyActive);
  try
    while SI.GetNext do
    begin
      Result := SI.Current;
      if Result.OriginalGeometry.TryFindParentByName(ParentNodeName) <> nil then Exit;
    end;
  finally FreeAndNil(SI) end;
  Result := nil;
end;

{ TShape -------------------------------------------------------------- }

constructor TShape.Create(AParentScene: TObject;
  AOriginalGeometry: TAbstractGeometryNode; AOriginalState: TX3DGraphTraverseState;
  ParentInfo: PTraversingInfo);
begin
  inherited Create(AParentScene);

  FTriangleOctreeLimits := DefLocalTriangleOctreeLimits;
  FShadowVolumes := TShapeShadowVolumes.Create(Self);

  FOriginalGeometry := AOriginalGeometry;
  FOriginalState := AOriginalState;

  if ParentInfo <> nil then
  begin
    FGeometryParentNodeName := ParentInfo^.Node.X3DName;
    ParentInfo := ParentInfo^.ParentInfo;
    if ParentInfo <> nil then
    begin
      FGeometryGrandParentNodeName := ParentInfo^.Node.X3DName;
      ParentInfo := ParentInfo^.ParentInfo;
      if ParentInfo <> nil then
        FGeometryGrandGrandParentNodeName := ParentInfo^.Node.X3DName;
    end;
  end;

  {$ifdef SHAPE_OCTREE_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

destructor TShape.Destroy;
begin
  FreeAndNil(FShadowVolumes);
  FreeProxy;
  FreeAndNil(FNormals);
  FreeAndNil(FOriginalState);
  FreeOctreeTriangles;
  inherited;
end;

procedure TShape.FreeOctreeTriangles;
begin
  { secure against ParentScene = nil, since this may be called from destructor }

  if ParentScene <> nil then
  begin
    { Some PTriangles will be freed. Make sure to clear
      PointingDeviceOverItem, unless they belong to a different shape. }
    if (TCastleSceneCore(ParentScene).PointingDeviceOverItem <> nil) and
       (TCastleSceneCore(ParentScene).PointingDeviceOverItem^.Shape = Self) then
      TCastleSceneCore(ParentScene).PointingDeviceClear;
  end;

  FreeAndNil(FOctreeTriangles);
end;

function TShape.InternalOctreeTriangles: TTriangleOctree;
begin
  if (ssTriangles in InternalSpatial) and (FOctreeTriangles = nil) then
  begin
    FOctreeTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits),
      InternalTriangleOctreeProgressTitle);
    if Log and LogChanges then
      WritelnLog('X3D changes (octree)', Format(
        'Shape(%s).OctreeTriangles updated', [PointerToStr(Self)]));
  end;

  Result := FOctreeTriangles;
end;

function TShape.InternalTriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TShape.LocalBoundingBox: TBox3D;
begin
  if not (svLocalBBox in Validities) then
  begin
    FLocalBoundingBox := OriginalGeometry.LocalBoundingBox(OriginalState,
      ProxyGeometry(false), ProxyState(false));
    Include(Validities, svLocalBBox);
  end;
  Result := FLocalBoundingBox;
end;

function TShape.BoundingBox: TBox3D;
begin
  if not (svBBox in Validities) then
  begin
    FBoundingBox := OriginalGeometry.BoundingBox(OriginalState,
      ProxyGeometry(false), ProxyState(false));
    Include(Validities, svBBox);
  end;
  Result := FBoundingBox;
end;

function TShape.VerticesCount(OverTriangulate: boolean): Cardinal;

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

function TShape.TrianglesCount(OverTriangulate: boolean): Cardinal;

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

function TShape.GeometryArrays(OverTriangulate: boolean): TGeometryArrays;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;

  function MaterialOpacity: Single;
  begin
    if G is TAbstractGeometryNode_1 then
      Result := S.LastNodes.Material.Opacity(0) else
    if (S.ShapeNode <> nil) and
       (S.ShapeNode.Material <> nil) then
      Result := S.ShapeNode.Material.Opacity else
      Result := 1;
  end;

  function TexCoordsNeeded: Cardinal;
  begin
    if G is TAbstractGeometryNode_1 then
    begin
      { We don't want to actually load the texture here,
        so only check is filename/image set. }
      if (S.LastNodes.Texture2.FdFilename.Value <> '') or
         ((S.LastNodes.Texture2.FdImage.Value <> nil) and
          (not S.LastNodes.Texture2.FdImage.Value.IsEmpty)) then
        Result := 1 else
        Result := 0;
    end else
    if (S.ShapeNode <> nil) and { for correct VRML >= 2, Shape should be assigned, but secure from buggy models }
       (S.ShapeNode.Texture <> nil) then
    begin
      if S.ShapeNode.Texture is TMultiTextureNode then
        Result := TMultiTextureNode(S.ShapeNode.Texture).FdTexture.Count else
        Result := 1;
    end else
      Result := 0;

    if OriginalGeometry.FontTextureNode <> nil then
      Inc(Result);
  end;

  function ArrayForBox(Box: TBox3D): TGeometryArrays;
  begin
    { When there's no TArraysGenerator suitable, then we have an unsupported node.

      For now, we make an array describing a single quad: this shape's
      bounding box in XY plane. The reason for this is historical
      (this was a proper shape to approximate collision with 2D Text nodes;
      but they do not get anymore into this procedure, as they are implemented
      by a proxy that renders them through QuadSet or such node). }

    Result := TGeometryArrays.Create;
    if not Box.IsEmpty then
    begin
      Result.Primitive := gpTriangleFan; // gpQuads; - use triangle fan instead, to work with OpenGLES
      Result.Count := 4;

      Result.Position(0)^ := Vector3Single(Box.Data[0][0], Box.Data[0][1], Box.Data[0][2]);
      Result.Position(1)^ := Vector3Single(Box.Data[1][0], Box.Data[0][1], Box.Data[0][2]);
      Result.Position(2)^ := Vector3Single(Box.Data[1][0], Box.Data[1][1], Box.Data[0][2]);
      Result.Position(3)^ := Vector3Single(Box.Data[0][0], Box.Data[1][1], Box.Data[0][2]);

      Result.Normal(0)^ := UnitVector3Single[2];
      Result.Normal(1)^ := UnitVector3Single[2];
      Result.Normal(2)^ := UnitVector3Single[2];
      Result.Normal(3)^ := UnitVector3Single[2];
    end;
  end;

var
  GeneratorClass: TArraysGeneratorClass;
  Generator: TArraysGenerator;
begin
  G := Geometry(OverTriangulate);
  S := State(OverTriangulate);
  GeneratorClass := GetArraysGenerator(G);
  if GeneratorClass <> nil then
  begin
    Generator := GeneratorClass.Create(Self, OverTriangulate);
    try
      Generator.TexCoordsNeeded := TexCoordsNeeded;
      Generator.MaterialOpacity := MaterialOpacity;
      Generator.FacesNeeded := true;
      { Leave the rest of Generator properties as default }
      Result := Generator.GenerateArrays;
    finally FreeAndNil(Generator) end;
  end else
    Result := ArrayForBox(LocalBoundingBox);
end;

procedure TShape.FreeProxy;
begin
  if Log and LogChanges and
    { OriginalGeometry should always be <> nil, but just in case
      (e.g. running from destructor, or with bad state) check. }
    (OriginalGeometry <> nil) and
    (
    ( (FGeometry[false] <> OriginalGeometry) and (FGeometry[false] <> nil) ) or
    ( (FGeometry[true ] <> OriginalGeometry) and (FGeometry[true ] <> nil) ) or
    ( (FState[false] <> OriginalState) and (FState[false] <> nil) ) or
    ( (FState[true ] <> OriginalState) and (FState[true ] <> nil) )
    ) then
    WritelnLog('X3D changes', 'Releasing the Proxy geometry of ' + OriginalGeometry.ClassName);

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

procedure TShape.Changed(const InactiveOnly: boolean;
  const Changes: TX3DChanges);
begin
  { Remember to code everything here to act only when some stuff
    is included inside Changed value. For example, when
    Changes = [chClipPlane], there's no need to do anything here. }

  { When Proxy needs to be recalculated.
    Include chVisibleVRML1State, since even MaterialBinding may change VRML 1.0
    proxies. }
  if Changes * [chCoordinate, chVisibleVRML1State, chGeometryVRML1State,
    chTextureCoordinate, chGeometry, chWireframe] <> [] then
    FreeProxy;

  { When bounding volumes in global coordinates changed.
    Probably only chTransform is really needed here
    (testcase: upwind_turbine.x3d), as other flags already cause other changes
    that invalidate global bboxes anyway. }
  if Changes * [chTransform, chCoordinate, chGeometry, chGeometryVRML1State,
    chEverything] <> [] then
    Validities := Validities - [svBBox, svBoundingSphere];

  if chCoordinate in Changes then
    { Coordinate changes actual geometry. }
    LocalGeometryChanged(false, true);

  if Changes * [chGeometry, chGeometryVRML1State, chWireframe] <> [] then
    LocalGeometryChanged(false, false);

  if not InactiveOnly then
    TCastleSceneCore(ParentScene).VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
end;

procedure TShape.ValidateBoundingSphere;
begin
  if not (svBoundingSphere in Validities) then
  begin
    BoundingBox.BoundingSphere(FBoundingSphereCenter, FBoundingSphereRadiusSqr);
    Include(Validities, svBoundingSphere);
  end;
end;

function TShape.BoundingSphereCenter: TVector3Single;
begin
  ValidateBoundingSphere;
  Result := FBoundingSphereCenter;
end;

function TShape.BoundingSphereRadiusSqr: Single;
begin
  ValidateBoundingSphere;
  Result := FBoundingSphereRadiusSqr;
end;

function TShape.BoundingSphereRadius: Single;
begin
  Result := Sqrt(BoundingSphereRadiusSqr);
end;

function TShape.FrustumBoundingSphereCollisionPossible(
  const Frustum: TFrustum): TFrustumCollisionPossible;
begin
  ValidateBoundingSphere;
  Result := Frustum.SphereCollisionPossible(
    FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TShape.FrustumBoundingSphereCollisionPossibleSimple(
  const Frustum: TFrustum): boolean;
begin
  ValidateBoundingSphere;
  Result := Frustum.SphereCollisionPossibleSimple(
    FBoundingSphereCenter, FBoundingSphereRadiusSqr);
end;

function TShape.OverrideOctreeLimits(
  const BaseLimits: TOctreeLimits): TOctreeLimits;
var
  Props: TKambiOctreePropertiesNode;
begin
  Result := BaseLimits;
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.FdOctreeTriangles.Value <> nil) and
     (State.ShapeNode.FdOctreeTriangles.Value is TKambiOctreePropertiesNode) then
  begin
    Props := TKambiOctreePropertiesNode(State.ShapeNode.FdOctreeTriangles.Value);
    Props.OverrideLimits(Result);
  end;
end;

procedure TShape.AddTriangleToOctreeProgress(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Shape, Position, Normal, TexCoord, Face);
end;

function TShape.CreateTriangleOctree(
  const ALimits: TOctreeLimits;
  const ProgressTitle: string): TTriangleOctree;

  procedure LocalTriangulateBox(const Box: TBox3D);

    procedure LocalTriangulateRect(constCoord: integer;
      const constCoordValue, x1, y1, x2, y2: Single);
    var
      Position, Normal: TTriangle3Single;
      i, c1, c2: integer;

      procedure TriAssign(TriIndex: integer; c1value, c2value: Single);
      begin
        Position[TriIndex, c1] := c1value;
        Position[TriIndex, c2] := c2value;
      end;

    begin
      RestOf3dCoords(constCoord, c1, c2);

      for I := 0 to 2 do
      begin
        Position[I, ConstCoord] := ConstCoordValue;
        Normal[I, C1] := 0;
        Normal[I, C2] := 0;
        Normal[I, ConstCoord] := 1; { TODO: or -1 }
      end;

      TriAssign(0, x1, y1);
      TriAssign(1, x1, y2);
      TriAssign(2, x2, y2);
      Result.AddItemTriangle(Self, Position, Normal, UnknownTexCoord, UnknownFaceIndex);
      TriAssign(0, x1, y1);
      TriAssign(1, x2, y2);
      TriAssign(2, x2, y1);
      Result.AddItemTriangle(Self, Position, Normal, UnknownTexCoord, UnknownFaceIndex);
    end;

  var
    I, XCoord, YCoord: Integer;
  begin
    for I := 0 to 2 do
    begin
      RestOf3dCoords(I, XCoord, YCoord);
      LocalTriangulateRect(I, Box.Data[0][I], Box.Data[0][XCoord], Box.Data[0][YCoord], Box.Data[1][XCoord], Box.Data[1][YCoord]);
      LocalTriangulateRect(I, Box.Data[1][I], Box.Data[0][XCoord], Box.Data[0][YCoord], Box.Data[1][XCoord], Box.Data[1][YCoord]);
    end;
  end;

begin
  Result := TTriangleOctree.Create(ALimits, LocalBoundingBox);
  try
    if DynamicGeometry then
    begin
      { Add 12 triangles for 6 cube (LocalBoundingBox) sides.
        No point in progress here, as this is always fast. }
      Result.Triangles.Capacity := 12;
      LocalTriangulateBox(LocalBoundingBox);
    end else
    begin
      Result.Triangles.Capacity := TrianglesCount(false);
      if (ProgressTitle <> '') and
         (not Progress.Active) then
      begin
        Progress.Init(TrianglesCount(false), ProgressTitle, true);
        try
          TriangleOctreeToAdd := Result;
          LocalTriangulate(false, @AddTriangleToOctreeProgress);
        finally Progress.Fini end;
      end else
        LocalTriangulate(false, @Result.AddItemTriangle);
    end;
  except Result.Free; raise end;

  { $define CASTLE_DEBUG_OCTREE_DUPLICATION}
  {$ifdef CASTLE_DEBUG_OCTREE_DUPLICATION}
  WritelnLog('Triangles In Shape Octree Stats', '%d items in octree, %d items in octree''s leafs, duplication %f. Size of items in bytes: %d * %d = %d',
    [Result.TotalItemsInOctree,
     Result.TotalItemsInLeafs,
     Result.TotalItemsInLeafs / Result.TotalItemsInOctree,
     Result.Triangles.Count,
     SizeOf(TTriangle),
     Result.Triangles.Count * SizeOf(TTriangle)]);
  {$endif}
end;

procedure TShape.SetSpatial(const Value: TShapeSpatialStructures);
var
  Old, New: boolean;
begin
  if Value <> InternalSpatial then
  begin
    { Handle OctreeTriangles }

    Old := ssTriangles in InternalSpatial;
    New := ssTriangles in Value;

    if Old and not New then
      FreeOctreeTriangles;

    FSpatial := Value;
  end;
end;

procedure TShape.LocalGeometryChanged(
  const CalledFromParentScene, ChangedOnlyCoord: boolean);
begin
  if FOctreeTriangles <> nil then
  begin
    if DisableAutoDynamicGeometry = 0 then
    begin
      if (not DynamicGeometry) and Log then
        WritelnLog('Shape', Format('Shape with geometry %s detected as dynamic, will use more crude collision detection and more suitable rendering',
          [OriginalGeometry.X3DType]));
      DynamicGeometry := true;
    end;
    FreeOctreeTriangles;
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

  { Clear variables after removing fvTrianglesList* }
  FShadowVolumes.InvalidateTrianglesListShadowCasters;

  { Edges topology possibly changed. }
  if not ChangedOnlyCoord then
    { When ChangedOnlyCoord, we don't do InvalidateManifoldAndBorderEdges,
      and this an important optimization (makes mesh deformation cheaper). }
    FShadowVolumes.InvalidateManifoldAndBorderEdges;

  if not CalledFromParentScene then
  begin
    if ChangedOnlyCoord then
      TCastleSceneCore(ParentScene).DoGeometryChanged(gcLocalGeometryChangedCoord, Self) else
      TCastleSceneCore(ParentScene).DoGeometryChanged(gcLocalGeometryChanged, Self);
  end;
end;

function TShape.Transparent: boolean;
begin
  Result := Blending;
end;

function TShape.Blending: boolean;

  { All the "transparency" field values are greater than zero.
    So the blending should be used when rendering.

    Note that when "transparency" field is empty, then we assume
    a default transparency (0) should be used. So AllMaterialsTransparent
    is @false then (contrary to the strict definition of "all",
    which should be true for empty sets). }
  function AllMaterialsTransparent(const Node: TMaterialNode_1): boolean;
  var
    i: Integer;
  begin
    if Node.FdTransparency.Items.Count = 0 then
      result := DefaultMaterialTransparency > SingleEqualityEpsilon else
    begin
      for i := 0 to Node.FdTransparency.Items.Count-1 do
        if Node.FdTransparency.Items.L[i] <= SingleEqualityEpsilon then
          Exit(false);
      result := true;
    end;
  end;

var
  M: TMaterialNode;
  Tex: TAbstractTextureNode;
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
    Result := AllMaterialsTransparent(State.LastNodes.Material);

  if Geometry.ColorRGBA <> nil then
    Result := true;

  { If texture exists with full range alpha channel then use blending.
    Note that State.Texture may be TMultiTextureNode --- that's Ok,
    it has AlphaChannel = atFullRange
    if any child has atFullRange. So it automatically works Ok too. }
  Tex := State.Texture;
  if (Tex <> nil) and (Tex.AlphaChannelFinal = acBlending) then
    Result := true;

  Tex := OriginalGeometry.FontTextureNode;
  if (Tex <> nil) and (Tex.AlphaChannelFinal = acBlending) then
    Result := true;
end;

procedure TShape.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  if ((not OnlyVisible) or Visible) and
     ((not OnlyCollidable) or Collidable) then
    Func(Self);
end;

function TShape.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  if ((not OnlyVisible) or Visible) and
     ((not OnlyCollidable) or Collidable) then
    Result := 1 else
    Result := 0;
end;

function TShape.Visible: boolean;
begin
  Result := State.InsideInvisible = 0;
end;

function TShape.Collidable: boolean;
begin
  Result := State.InsideIgnoreCollision = 0;
end;

function TShape.RayCollision(
  const Tag: TMailboxTag;
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
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

    Result := InternalOctreeTriangles.RayCollision(
      Intersection, IntersectionDistance, RayOrigin, RayDirection,
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

function TShape.SegmentCollision(
  const Tag: TMailboxTag;
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3Single;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): PTriangle;
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

    Result := InternalOctreeTriangles.SegmentCollision(
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

function TShape.NormalsSmooth(OverTriangulate: boolean): TVector3SingleList;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not ((svNormals in Validities) and
          (FNormalsOverTriangulate = OverTriangulate) and
          (FNormalsCached = ncSmooth)) then
  begin
    if Log and LogShapes then
      WritelnLog('Normals', 'Calculating shape smooth normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry(OverTriangulate);
    S := State(OverTriangulate);

    FNormals := CreateSmoothNormalsCoordinateNode(G, S, true);
    FNormalsCached := ncSmooth;
    FNormalsOverTriangulate := OverTriangulate;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.NormalsFlat(OverTriangulate: boolean): TVector3SingleList;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not ((svNormals in Validities) and
          (FNormalsOverTriangulate = OverTriangulate) and
          (FNormalsCached = ncFlat)) then
  begin
    if Log and LogShapes then
      WritelnLog('Normals', 'Calculating shape flat normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry(OverTriangulate);
    S := State(OverTriangulate);

    FNormals := CreateFlatNormals(G.CoordIndex.Items,
      G.Coordinates(S).Items, true, G.Convex);
    FNormalsCached := ncFlat;
    FNormalsOverTriangulate := OverTriangulate;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.NormalsCreaseAngle(OverTriangulate: boolean;
  const CreaseAngle: Single): TVector3SingleList;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not ((svNormals in Validities) and
          (FNormalsCached = ncCreaseAngle) and
          (FNormalsOverTriangulate = OverTriangulate) and
          (FNormalsCreaseAngle = CreaseAngle)) then
  begin
    if Log and LogShapes then
      WritelnLog('Normals', 'Calculating shape CreaseAngle normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry(OverTriangulate);
    S := State(OverTriangulate);

    FNormals := CreateNormals(G.CoordIndex.Items,
      G.Coordinates(S).Items, CreaseAngle, true, G.Convex);
    FNormalsCached := ncCreaseAngle;
    FNormalsOverTriangulate := OverTriangulate;
    FNormalsCreaseAngle := CreaseAngle;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer;

  function HandleSingleTextureNode(Tex: TX3DNode): Pointer;
  begin
    if (Tex <> nil) and
       (Tex is TAbstractTextureNode) then
      Result := Enumerate(Self, TAbstractTextureNode(Tex)) else
      Result := nil;
  end;

  function HandleTextureNode(Tex: TX3DNode): Pointer;
  var
    I: Integer;
  begin
    Result := nil;

    if (Tex <> nil) and
       (Tex is TMultiTextureNode) then
    begin
      Result := Enumerate(Self, TMultiTextureNode(Tex));
      if Result <> nil then Exit;

      for I := 0 to TMultiTextureNode(Tex).FdTexture.Items.Count - 1 do
      begin
        Result := HandleSingleTextureNode(TMultiTextureNode(Tex).FdTexture.Items.Items[I]);
        if Result <> nil then Exit;
      end;
    end else
      Result := HandleSingleTextureNode(Tex);
  end;

  { Scan IDecls for SFNode and MFNode fields, handling texture nodes inside. }
  function HandleShaderFields(IDecls: TX3DInterfaceDeclarationList): Pointer;
  var
    I, J: Integer;
    UniformField: TX3DField;
  begin
    Result := nil;
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          Result := HandleTextureNode(TSFNode(UniformField).Value);
          if Result <> nil then Exit;
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
          begin
            Result := HandleTextureNode(TMFNode(UniformField).Items[J]);
            if Result <> nil then Exit;
          end;
        end;
      end;
    end;
  end;

var
  ComposedShader: TComposedShaderNode;
  I: Integer;
  App: TAppearanceNode;
begin
  Result := HandleTextureNode(State.LastNodes.Texture2);
  if Result <> nil then Exit;

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    App := State.ShapeNode.Appearance;
    Result := HandleTextureNode(App.FdTexture.Value);
    if Result <> nil then Exit;

    for I := 0 to App.FdShaders.Count - 1 do
    begin
      ComposedShader := App.FdShaders.GLSLShader(I);
      if ComposedShader <> nil then
      begin
        Result := HandleShaderFields(ComposedShader.InterfaceDeclarations);
        if Result <> nil then Exit;
      end;
    end;

    for I := 0 to App.FdEffects.Count - 1 do
      if App.FdEffects[I] is TEffectNode then
      begin
        Result := HandleShaderFields(TEffectNode(App.FdEffects[I]).InterfaceDeclarations);
        if Result <> nil then Exit;
      end;
  end;

  Result := HandleTextureNode(OriginalGeometry.FontTextureNode);
  if Result <> nil then Exit;
end;

type
  TUsesTextureHelper = class
    Node: TAbstractTextureNode;
    function HandleTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
  end;

function TUsesTextureHelper.HandleTexture(Shape: TShape;
  Texture: TAbstractTextureNode): Pointer;
begin
  if Texture = Node then
    Result := Texture { anything non-nil } else
    Result := nil;
end;

function TShape.UsesTexture(Node: TAbstractTextureNode): boolean;
var
  Helper: TUsesTextureHelper;
begin
  Helper := TUsesTextureHelper.Create;
  try
    Helper.Node := Node;
    Result := EnumerateTextures(@Helper.HandleTexture) <> nil;
  finally Helper.Free end;
end;

function TShape.ShadowCaster: boolean;
var
  S: TAbstractShapeNode;
  A: TX3DNode;
begin
  Result := true;

  S := State.ShapeNode;
  if S <> nil then
  begin
    A := S.FdAppearance.Value;
    if (A <> nil) and
       (A is TAppearanceNode) then
      Result := TAppearanceNode(A).FdShadowCaster.Value;
  end;
end;

procedure TShape.ValidateGeometryState(const OverTriangulate: boolean);
begin
  if FGeometry[OverTriangulate] = nil then
  begin
    Assert(FState[OverTriangulate] = nil);
    FState[OverTriangulate] := OriginalState;

    try
      FGeometry[OverTriangulate] := OriginalGeometry.Proxy(
        FState[OverTriangulate], OverTriangulate);
    except
      { in case of trouble, remember to keep both
        FGeometry[OverTriangulate] and FState[OverTriangulate] nil.
        Never let one of them be nil, while other it not. }
      FState[OverTriangulate] := nil;
      raise;
    end;

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

function TShape.Geometry(const OverTriangulate: boolean): TAbstractGeometryNode;
begin
  ValidateGeometryState(OverTriangulate);
  Result := FGeometry[OverTriangulate];
end;

function TShape.State(const OverTriangulate: boolean): TX3DGraphTraverseState;
begin
  ValidateGeometryState(OverTriangulate);
  Result := FState[OverTriangulate];
end;

function TShape.ProxyGeometry(const OverTriangulate: boolean): TAbstractGeometryNode;
begin
  Result := Geometry(OverTriangulate);
  if Result = OriginalGeometry then Result := nil;
end;

function TShape.ProxyState(const OverTriangulate: boolean): TX3DGraphTraverseState;
begin
  if Geometry(OverTriangulate) <> OriginalGeometry then
    Result := State(OverTriangulate) else
    Result := nil;
end;

procedure TShape.LocalTriangulate(OverTriangulate: boolean; TriangleEvent: TTriangleEvent);
var
  Arrays: TGeometryArrays;
  RangeBeginIndex: Integer;

  { Call TriangleEvent once. Give indexes to Arrays (Arrays.Indexes,
    if assigned, otherwise direct coordinates), relative to RangeBeginIndex. }
  procedure Triangle(const I1, I2, I3: Cardinal);
  var
    VI1, VI2, VI3: Integer;
    Position, Normal: TTriangle3Single;
    TexCoord: TTriangle4Single;
    Face: TFaceIndex;
  begin
    if Arrays.Indexes <> nil then
    begin
      VI1 := Arrays.Indexes[RangeBeginIndex + I1];
      VI2 := Arrays.Indexes[RangeBeginIndex + I2];
      VI3 := Arrays.Indexes[RangeBeginIndex + I3];
    end else
    begin
      VI1 := RangeBeginIndex + I1;
      VI2 := RangeBeginIndex + I2;
      VI3 := RangeBeginIndex + I3;
    end;
    Position[0] := Arrays.Position(VI1)^;
    Position[1] := Arrays.Position(VI2)^;
    Position[2] := Arrays.Position(VI3)^;
    Normal[0] := Arrays.Normal(VI1)^;
    Normal[1] := Arrays.Normal(VI2)^;
    Normal[2] := Arrays.Normal(VI3)^;

    if (Arrays.TexCoords.Count <> 0) and
       (Arrays.TexCoords[0] <> nil) and
       (Arrays.TexCoords[0].Generation = tgExplicit) then
    begin
      case Arrays.TexCoords[0].Dimensions of
        2: begin
             TexCoord[0] := Vector4Single(Arrays.TexCoord2D(0, VI1)^);
             TexCoord[1] := Vector4Single(Arrays.TexCoord2D(0, VI2)^);
             TexCoord[2] := Vector4Single(Arrays.TexCoord2D(0, VI3)^);
           end;
        3: begin
             TexCoord[0] := Vector4Single(Arrays.TexCoord3D(0, VI1)^);
             TexCoord[1] := Vector4Single(Arrays.TexCoord3D(0, VI2)^);
             TexCoord[2] := Vector4Single(Arrays.TexCoord3D(0, VI3)^);
           end;
        4: begin
             TexCoord[0] := Arrays.TexCoord4D(0, VI1)^;
             TexCoord[1] := Arrays.TexCoord4D(0, VI2)^;
             TexCoord[2] := Arrays.TexCoord4D(0, VI3)^;
           end;
        else raise EInternalError.Create('Arrays.TexCoord[0].Dimensions? at TShape.localtriangulate');
      end;
    end else
      TexCoord := UnknownTexCoord;

    if Arrays.Faces <> nil then
      Face := Arrays.Faces.L[RangeBeginIndex + I1] else
      Face := UnknownFaceIndex;

    TriangleEvent(Self, Position, Normal, TexCoord, Face);
  end;

  { Call NewTriangle, triangulating indexes 0 .. Count - 1. }
  procedure TriangulateRange(const Count: Cardinal);
  var
    I: Cardinal;
    NormalOrder: boolean;
  begin
    case Arrays.Primitive of
      gpTriangles:
        begin
          I := 0;
          while I + 2 < Count do
          begin
            Triangle(I, I + 1, I + 2);
            I += 3;
          end;
        end;
      {$ifndef OpenGLES}
      gpQuads:
        begin
          I := 0;
          while I + 3 < Count do
          begin
            Triangle(I, I + 1, I + 2);
            Triangle(I, I + 2, I + 3);
            I += 4;
          end;
        end;
      {$endif}
      gpTriangleFan:
        begin
          I := 0;
          while I + 2 < Count do
          begin
            Triangle(0, I + 1, I + 2);
            Inc(I);
          end;
        end;
      gpTriangleStrip:
        begin
          I := 0;
          NormalOrder := true;
          while I + 2 < Count do
          begin
            if NormalOrder then
              Triangle(I    , I + 1, I + 2) else
              Triangle(I + 1, I    , I + 2);
            NormalOrder := not NormalOrder;
            Inc(I);
          end;
        end;
      else { gpLineStrip, gpPoints don't make triangles } ;
    end;
  end;

var
  Count: Cardinal;
  I: Integer;
begin
  Arrays := GeometryArrays(OverTriangulate);
  try
    if Arrays.Indexes <> nil then
      Count := Arrays.IndexesCount else
      Count := Arrays.Count;
    RangeBeginIndex := 0;
    if Arrays.Counts = nil then
      TriangulateRange(Count) else
      for I := 0 to Arrays.Counts.Count - 1 do
      begin
        TriangulateRange(Arrays.Counts[I]);
        RangeBeginIndex += Arrays.Counts[I];
      end;
  finally FreeAndNil(Arrays) end;
end;

type
  TTriangulateRedirect = class
    Transform: PMatrix4Single;
    TriangleEvent: TTriangleEvent;
    procedure LocalNewTriangle(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
  end;

procedure TTriangulateRedirect.LocalNewTriangle(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
begin
  TriangleEvent(Shape, TriangleTransform(Position, Transform^), Normal, TexCoord, Face);
end;

procedure TShape.Triangulate(OverTriangulate: boolean; TriangleEvent: TTriangleEvent);
var
  TR: TTriangulateRedirect;
begin
  TR := TTriangulateRedirect.Create;
  try
    TR.Transform := @(State.Transform);
    TR.TriangleEvent := TriangleEvent;
    LocalTriangulate(OverTriangulate, @TR.LocalNewTriangle);
  finally FreeAndNil(TR) end;
end;

function TShape.DebugInfo(const Indent: string): string;
begin
  Result := Indent + NiceName + NL;
end;

function TShape.NiceName: string;
begin
  Result := OriginalGeometry.NiceName;
  if (Node <> nil) and (Node.X3DName <> '') then
    Result := Node.X3DName + ':' + Result;
end;

function TShape.Node: TAbstractShapeNode;
begin
  Result := State.ShapeNode;
end;

function TShape.MaterialProperty: TMaterialProperty;
var
  TextureUrl: string;
begin
  if IsCachedMaterialProperty then
    Exit(CachedMaterialProperty);

  Result := nil;

  if Node <> nil then
  begin
    { VRML 2.0/X3D version: refer to TAppearanceNode }
    if Node.Appearance <> nil then
      Result := Node.Appearance.MaterialProperty;
  end else
  begin
    { VRML 1.0 version: do it directly here }
    TextureUrl := State.LastNodes.Texture2.FdFileName.Value;
    if TextureUrl <> '' then
      Result := MaterialProperties.FindTextureBaseName(
        DeleteURIExt(ExtractURIName(TextureUrl)));
  end;

  IsCachedMaterialProperty := true;
  CachedMaterialProperty := Result;
end;

{ TODO:
class procedure TShape.MaterialPropertyCacheClear;
begin
end; }

{ TShapeTreeGroup -------------------------------------------------------- }

constructor TShapeTreeGroup.Create(AParentScene: TObject);
begin
  inherited Create(AParentScene);
  FChildren := TShapeTreeList.Create(true);
end;

destructor TShapeTreeGroup.Destroy;
begin
  FreeAndNil(FChildren);
  inherited;
end;

procedure TShapeTreeGroup.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].Traverse(Func, OnlyActive, OnlyVisible, OnlyCollidable);
end;

function TShapeTreeGroup.ShapesCount(
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

function TShapeTreeGroup.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FChildren.Count - 1 do
  begin
    Result := FChildren.Items[I].EnumerateTextures(Enumerate);
    if Result <> nil then Exit;
  end;
end;

{$ifdef SHAPE_ITERATOR_SOPHISTICATED}
function TShapeTreeGroup.IterateBeginIndex(OnlyActive: boolean): Integer;
begin
  Result := -1;
end;

function TShapeTreeGroup.IterateEndIndex(OnlyActive: boolean): Cardinal;
begin
  Result := FChildren.Count;
end;
{$endif}

function TShapeTreeGroup.DebugInfo(const Indent: string): string;
var
  I: Integer;
begin
  Result := Indent + ClassName + NL;
  for I := 0 to FChildren.Count - 1 do
    Result += FChildren[I].DebugInfo(Indent + Format('  %3d:', [I]));
end;

{ TShapeTreeSwitch ------------------------------------------------------- }

procedure TShapeTreeSwitch.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Children.Items[WhichChoice].Traverse(Func, OnlyActive, OnlyVisible, OnlyCollidable);
  end else
    inherited;
end;

function TShapeTreeSwitch.ShapesCount(
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

{$ifdef SHAPE_ITERATOR_SOPHISTICATED}
function TShapeTreeSwitch.IterateBeginIndex(OnlyActive: boolean): Integer;
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if WhichChoice >= 0 then
      { It's ok if whichChoice is >= children count,
        iterator will check this. }
      Result := WhichChoice - 1 else
      Result := -1 { whatever; IterateCount will be 0 anyway };
  end else
    Result := inherited;
end;

function TShapeTreeSwitch.IterateEndIndex(OnlyActive: boolean): Cardinal;
var
  WhichChoice: Integer;
begin
  if OnlyActive then
  begin
    WhichChoice := SwitchNode.FdWhichChoice.Value;
    if (WhichChoice >= 0) and
       (WhichChoice < Children.Count) then
      Result := WhichChoice + 1 else
      Result := 0;
  end else
    Result := inherited;
end;
{$endif}

{ TShapeTreeTransform ---------------------------------------------------- }

constructor TShapeTreeTransform.Create(AParentScene: TObject);
begin
  inherited;
  FTransformState := TX3DGraphTraverseState.Create;
end;

destructor TShapeTreeTransform.Destroy;
begin
  FreeAndNil(FTransformState);
  inherited;
end;

{ TShapeTreeLOD ------------------------------------------------------- }

function TShapeTreeLOD.LODInvertedTransform: PMatrix4Single;
begin
  Result := @FLODInvertedTransform;
end;

function TShapeTreeLOD.CalculateLevel(const CameraPosition: TVector3Single): Cardinal;
var
  Camera: TVector3Single;
  Dummy: Single;
begin
  if (Children.Count = 0) or
     (LODNode.FdRange.Count = 0) then
    Result := 0 else
  begin
    try
      Camera := MatrixMultPoint(LODInvertedTransform^, CameraPosition);
      Result := KeyRange(LODNode.FdRange.Items,
        PointsDistance(Camera, LODNode.FdCenter.Value), Dummy);
      { Now we know Result is between 0..LODNode.FdRange.Count.
        Following X3D spec "Specifying too few levels will result in
        the last level being used repeatedly for the lowest levels of detail",
        so just clamp to last children. }
      MinVar(Result, Children.Count - 1);
    except
      on E: ETransformedResultInvalid do
      begin
        WritelnWarning('VRML/X3D', Format('Cannot transform camera position %s to LOD node local coordinate space, transformation results in direction (not point): %s',
          [ VectorToRawStr(CameraPosition), E.Message ]));
        Result := 0;
      end;
    end;
  end;

  Assert(
    ( (Children.Count = 0) and (Result = 0) ) or
    ( (Children.Count > 0) and (Result < Cardinal(Children.Count)) ) );
end;

procedure TShapeTreeLOD.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Children.Items[Level].Traverse(Func, OnlyActive, OnlyVisible, OnlyCollidable) else
      inherited;
  end;
end;

function TShapeTreeLOD.ShapesCount(
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

{$ifdef SHAPE_ITERATOR_SOPHISTICATED}
function TShapeTreeLOD.IterateBeginIndex(OnlyActive: boolean): Integer;
begin
  if (Children.Count > 0) and OnlyActive then
    Result := Level - 1 else
    Result := inherited;
end;

function TShapeTreeLOD.IterateEndIndex(OnlyActive: boolean): Cardinal;
begin
  if (Children.Count > 0) and OnlyActive then
    Result := Level + 1 else
    Result := inherited;
end;
{$endif}

{ TProximitySensorInstance ---------------------------------------------- }

procedure TProximitySensorInstance.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  { Nothing to do: no geometry shapes, no children here }
end;

function TProximitySensorInstance.ShapesCount(const OnlyActive: boolean;
  const OnlyVisible: boolean = false;
  const OnlyCollidable: boolean = false): Cardinal;
begin
  Result := 0;
end;

function TProximitySensorInstance.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer;
begin
  { Nothing to do: no geometry shapes, no children here }
  Result := nil;
end;

function TProximitySensorInstance.DebugInfo(const Indent: string = ''): string;
begin
  Result := Indent + 'ProximitySensor (' + Node.X3DName + ')' + NL;
end;

{ TVisibilitySensorInstance ---------------------------------------------- }

procedure TVisibilitySensorInstance.Traverse(Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  { Nothing to do: no geometry shapes, no children here }
end;

function TVisibilitySensorInstance.ShapesCount(const OnlyActive: boolean;
  const OnlyVisible: boolean = false;
  const OnlyCollidable: boolean = false): Cardinal;
begin
  Result := 0;
end;

function TVisibilitySensorInstance.EnumerateTextures(Enumerate: TEnumerateShapeTexturesFunction): Pointer;
begin
  { Nothing to do: no geometry shapes, no children here }
  Result := nil;
end;

function TVisibilitySensorInstance.DebugInfo(const Indent: string = ''): string;
begin
  Result := Indent + 'VisibilitySensor (' + Node.X3DName + ')' + NL;
end;

{ TShapeTreeIterator ----------------------------------------------------- }

{ When SHAPE_ITERATOR_SOPHISTICATED is defined, we use a complicated
  implementation that has a nice O(1) speed for constructor and all
  GetNext calls (well, actually some calls may have O(depth), but most
  will not). It traverses one step further in each GetNext.
  It's building a simple stack of items to make efficient push/pop while
  walking down/up the tree of TShapesTree.

  When SHAPE_ITERATOR_SOPHISTICATED is not defined, we use a very simple
  implementation: just call Tree.Traverse,
  collecting shapes to a list in constructor. Then simply iterate
  over this list. This makes constructor time large (equal to traversing time,
  so O(leaves count)), although GetNext is lighting fast.

  Theoretically, the sophisticated version was supposed to be much better,
  as speed is always O(1) and memory use is much smaller
  (only the depth of the shapes tree, as opposed to the number of all leaves).

  In practice however, it turned out that the sophisticated version
  was useless. Time measures shown that "naive" and simple
  version is even very very slightly faster in some cases.
  Time measure is in castle_game_engine/tests/testscenecore.pas,
  define ITERATOR_SPEED_TEST and test for yourself.

  So in practice good memory allocator in FPC
  (as this is the bottleneck of the naive version, since List is potentially
  resized on adding each new shape) outperforms the sophisticated algorithm.

  So right now we're back to simple version. Maybe the "sophisticated"
  implementation will be restored some day... Just define
  SHAPE_ITERATOR_SOPHISTICATED. }

{$ifdef SHAPE_ITERATOR_SOPHISTICATED}

type
  { To efficiently implement TShapeTreeIterator, we have to
    use an efficient stack push/pop when entering TShapeTreeGroup
    (this includes TShapeTreeSwitch), and remember current Index
    within current group.

    Note that this follows the logic of implemented Traverse methods.
    There's no way to efficiently (without e.g. first collecting to a list)
    realize iterator with actually calling Traverse methods. }
  PIteratorInfo = ^TIteratorInfo;
  TIteratorInfo = record
    Group: TShapeTreeGroup;
    Index: Integer;
    GroupCount: Cardinal;
    Parent: PIteratorInfo;
  end;

{$define IteratorInfo := PIteratorInfo(Info)}

{ Check Current for FOnlyVisible and FOnlyCollidable flags. }
function TShapeTreeIterator.CurrentMatches: boolean;
begin
  if FOnlyVisible and FOnlyCollidable then
    Result := (Current <> nil) and Current.Visible and Current.Collidable else
  if FOnlyVisible then
    Result := (Current <> nil) and Current.Visible else
  if FOnlyCollidable then
    Result := (Current <> nil) and Current.Collidable else
    Result := (Current <> nil);
end;

constructor TShapeTreeIterator.Create(Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  inherited Create;

  FOnlyActive := OnlyActive;
  FOnlyVisible := OnlyVisible;
  FOnlyCollidable := OnlyCollidable;

  if Tree is TShapeTreeGroup then
  begin
    New(IteratorInfo);
    IteratorInfo^.Group := TShapeTreeGroup(Tree);
    IteratorInfo^.Index := IteratorInfo^.Group.IterateBeginIndex(OnlyActive);
    IteratorInfo^.GroupCount := IteratorInfo^.Group.IterateEndIndex(OnlyActive);
    IteratorInfo^.Parent := nil;
  end else
  begin
    { When the whole tree is one single TShape, this is a special case
      marked by IteratorInfo = nil and using SingleShapeRemaining.
      FCurrent is just constant in this case. }
    Assert(Tree is TShape);
    FCurrent := TShape(Tree);
    IteratorInfo := nil;
    SingleShapeRemaining := true;
  end;
end;

destructor TShapeTreeIterator.Destroy;

  procedure Done(I: PIteratorInfo);
  begin
    if I <> nil then
    begin
      Done(I^.Parent);
      Dispose(I);
    end;
  end;

begin
  Done(IteratorInfo);
  inherited;
end;

function TShapeTreeIterator.GetNext: boolean;
var
  ParentInfo: PIteratorInfo;
  Child: TShapeTree;
begin
  if IteratorInfo <> nil then
  begin
    repeat
      Inc(IteratorInfo^.Index);
      Assert(IteratorInfo^.Index >= 0);
      Assert(IteratorInfo^.Index > IteratorInfo^.Group.IterateBeginIndex(FOnlyActive));

      if Cardinal(IteratorInfo^.Index) < IteratorInfo^.GroupCount then
      begin
        Child := IteratorInfo^.Group.Children.Items[IteratorInfo^.Index];
        if Child is TShape then
        begin
          FCurrent := TShape(Child);

          if CurrentMatches then
            Result := true else
            Result := GetNext;

          Exit;
        end else
        begin
          Assert(Child is TShapeTreeGroup);
          ParentInfo := IteratorInfo;
          New(IteratorInfo);
          IteratorInfo^.Group := TShapeTreeGroup(Child);
          IteratorInfo^.Index := IteratorInfo^.Group.IterateBeginIndex(FOnlyActive);
          IteratorInfo^.GroupCount := IteratorInfo^.Group.IterateEndIndex(FOnlyActive);
          IteratorInfo^.Parent := ParentInfo;
        end;
      end else
      begin
        ParentInfo := IteratorInfo^.Parent;
        if ParentInfo <> nil then
        begin
          Dispose(IteratorInfo);
          IteratorInfo := ParentInfo;
        end else
          Exit(false);
      end;
    until false;
  end else
  begin
    Result := SingleShapeRemaining;
    SingleShapeRemaining := false;
    { FCurrent already set in constructor }

    if Result and (not CurrentMatches) then
      Result := false;
  end;
end;

{$undef IteratorInfo}

{$else SHAPE_ITERATOR_SOPHISTICATED}

constructor TShapeTreeIterator.Create(Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  inherited Create;
  List := TShapeList.Create(Tree, OnlyActive, OnlyVisible, OnlyCollidable);
  CurrentIndex := -1;
end;

destructor TShapeTreeIterator.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

function TShapeTreeIterator.GetNext: boolean;
begin
  Inc(CurrentIndex);
  Result := CurrentIndex < List.Count;
  if Result then
    FCurrent := List.Items[CurrentIndex];
end;

{$endif SHAPE_ITERATOR_SOPHISTICATED}

{ TShapeList ------------------------------------------------------- }

constructor TShapeList.Create;
begin
  inherited Create(false);
end;

constructor TShapeList.Create(Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
var
  AddedCount: Integer;

  procedure AddToList(Shape: TShape);
  begin
    Items[AddedCount] := Shape;
    Inc(AddedCount);
  end;

begin
  Create;

  { We know exactly how many shapes are present. So set Count once,
    calculating by ShapesCount. This will be faster than resizing
    in each AddToList. (Confirmed e.g. by profiling animate_3d_model_by_code_2). }
  AddedCount := 0;
  Count := Tree.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);

  Tree.Traverse(@AddToList, OnlyActive, OnlyVisible, OnlyCollidable);

  Assert(AddedCount = Count);
end;

var
  { Has to be global (not private field in TShapeList),
    since TFPGObjectList.Sort requires normal function (not "of object"). }
  SortPosition: TVector3Single;

function IsSmallerFrontToBack(const A, B: TShape): Integer;
begin
  { To revert the order, we revert the order of A and B as passed to CompareBackToFront3D. }
  Result := TBox3D.CompareBackToFront3D(B.BoundingBox, A.BoundingBox, SortPosition);
end;

function IsSmallerBackToFront3D(const A, B: TShape): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(A.BoundingBox, B.BoundingBox, SortPosition);
end;

function IsSmallerBackToFront2D(const A, B: TShape): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(A.BoundingBox, B.BoundingBox);
end;

procedure TShapeList.SortFrontToBack(const Position: TVector3Single);
begin
  SortPosition := Position;
  Sort(@IsSmallerFrontToBack);
end;

procedure TShapeList.SortBackToFront(const Position: TVector3Single;
  const Distance3D: boolean);
begin
  SortPosition := Position;
  if Distance3D then
    Sort(@IsSmallerBackToFront3D) else
    Sort(@IsSmallerBackToFront2D);
end;

{ TPlaceholderNames ------------------------------------------------------- }

function X3DShapePlaceholder(const Shape: TShape): string;
begin
  { Shape.Node may be nil for old VRML 1.0 or Inventor. }
  if Shape.Node <> nil then
    Result := Shape.Node.X3DName else
    Result := '';
end;

function BlenderPlaceholder(const Shape: TShape): string;
begin
  if Shape.OriginalGeometry is TAbstractGeometryNode_1 then
  begin
    { Geometry node generated by Blender VRML 1.0 exporter has one parent,
      its mesh. The mesh node may have many parents representing its objects
      (unfortunately, the object names are not recorded in exported file,
      so we use mesh name for BlenderPlaceholder. }
    Result := Shape.GeometryParentNodeName;
  end else
  begin
    { For VRML 2.0 and X3D exporter, the situation is quite similar.
      We look at parent of the Shape node (mesh Group)
      and parent of it (object Transform).
      The object names are available.

      For VRML 2.0 we have to remove ME_ and OB_ prefixes from node names.
      Somewhere around/before 2.64a X3D exporter also added _ifs_TRANSFORM suffix,
      remove it.

      Note that we assume X3D exporter from Blender >= 2.57.
      Earlier Blender X3D exporters were a little different (it seems,
      probably because of mesh splitting added in 2.57),
      we don't handle them. }

    // not needed:
    // BlenderMeshName := PrefixRemove('ME_', GeometryGrandParentNodeName, false);

    Result := SuffixRemove('_ifs_TRANSFORM', PrefixRemove('OB_',
      Shape.GeometryGrandGrandParentNodeName, false), false);
  end;
end;

initialization
  PlaceholderNames := TPlaceholderNames.Create;
  PlaceholderNames['x3dshape'] := @X3DShapePlaceholder;
  PlaceholderNames['blender'] := @BlenderPlaceholder;
finalization
  FreeAndNil(PlaceholderNames);
end.
