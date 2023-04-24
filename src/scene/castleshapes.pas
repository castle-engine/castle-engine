{
  Copyright 2003-2023 Michalis Kamburelis.

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

interface

uses SysUtils, Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleBoxes, X3DNodes, CastleClassUtils,
  CastleUtils, CastleInternalTriangleOctree, CastleFrustum, CastleInternalOctree,
  CastleInternalBaseTriangleOctree, X3DFields, CastleInternalGeometryArrays,
  CastleTriangles, CastleImages, CastleInternalMaterialProperties,
  CastleShapeInternalShadowVolumes, CastleRenderOptions;

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
  TShapeList = class;

  TShapeTraverseFunc = procedure (const  Shape: TShape) of object;

  TEnumerateShapeTexturesFunction = function (Shape: TShape;
    Texture: TAbstractTextureNode): Pointer of object;

  TTestShapeVisibility = function (Shape: TShape): boolean of object;

  TShapesHash = QWord;

  { Triangle in a 3D model.
    Helper methods. }
  TTriangleHelper = record helper for TTriangle
    { Shape containing this triangle. }
    function Shape: TShape;

    { State of this shape, containing various information about 3D shape.
      This is a shortcut for @link(Shape).State. }
    function State: TX3DGraphTraverseState;

    { Use State.Transform to update triangle @link(TTriangle.World) geometry
      from triangle @link(TTriangle.Local) geometry. }
    procedure UpdateWorld;

    { X3D shape node of this triangle. May be @nil in case of VRML 1.0. }
    function ShapeNode: TAbstractShapeNode;

    { X3D material node of this triangle. May be @nil in case material is not set
      or has a different class than one-sided Phong TMaterialNode. }
    function Material: TMaterialNode; deprecated 'use MaterialInfo';
    function MaterialNode: TMaterialNode; deprecated 'use MaterialInfo';

    { Material information for the material of this triangle.
      See TMaterialInfo for usage description.
      Returns @nil when no node determines material properties
      (which indicates white unlit look).

      Returned TMaterialInfo is valid only as long as the underlying
      node (TMaterialNode, TPhysicalMaterialNode, TUnlitMaterialNode, TCommonSurfaceShaderNode...) exists.
      Do not free it yourself, it will be automatically freed. }
    function MaterialInfo: TMaterialInfo;

    { Return transparency of this triangle's material.
      Equivalent to MaterialInfo.Transparency, although a little faster. }
    function Transparency: Single;

    { Returns @true for triangles that are transparent. }
    function IsTransparent: boolean;

    { Returns @true for triangles that should be ignored by shadow rays.
      Returns @true for transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE). }
    function IgnoreForShadowRays: boolean;

    {$ifndef CONSERVE_TRIANGLE_MEMORY}
    { For a given position (in world coordinates), return the smooth
      normal vector at this point, with the resulting normal vector
      in world coordinates.

      @seealso TTriangle.INormal }
    function INormalWorldSpace(const Point: TVector3): TVector3;
    {$endif}
  end;

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
  strict private
    FParentScene: TX3DEventsEngine;
    { Cached MaxShapesCount result, or -1 if not in cache yet. }
    FMaxShapesCount: Integer;
  private
    { Used by TShapeTreeIterator. }
    CachedChildrenList: array [
      { OnlyActive } Boolean,
      { OnlyVisible } Boolean,
      { OnlyCollidable } Boolean ] of TShapeList;
    CachedChildrenListHash: array [
      { OnlyActive } Boolean,
      { OnlyVisible } Boolean,
      { OnlyCollidable } Boolean ] of TShapesHash;
    { Automatically set when adding item to TShapeTreeGroup. }
    FParent: TShapeTree;
    function MaxShapesCountCore: Integer; virtual; abstract;
    procedure InvalidateMaxShapesCount;
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean;
      const OnlyCollidable: boolean); virtual; abstract;
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); virtual; abstract;
  public
    constructor Create(const AParentScene: TX3DEventsEngine);
    destructor Destroy; override;

    { Maximum value of ShapesCount.
      This is cached, so it's usually instant, in contrast to ShapesCount. }
    function MaxShapesCount: Integer;

    { Parent TCastleSceneCore instance. This cannot be declared here as
      TCastleSceneCore (this would create circular unit dependency),
      but it always is TCastleSceneCore. }
    property ParentScene: TX3DEventsEngine read FParentScene write FParentScene;

    { Traverse shapes inside. There are a few alternative ways to do this:

      - Use this method, and have a callback called for each TShape.

      - Use TraverseList, and have a list of TShape.

      - Use TShapeTreeIterator.Create, that has it's own GetNext / Current methods to enumerate.
        This is no longer advised -- using TraverseList is simpler and equally fast.

      - Use TShapeList.Create(Tree, OnlyActive, OnlyVisible, OnlyCollidable) overload.
        This is no longer advised -- using TraverseList is simpler and faster.
    }
    procedure Traverse(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);

    { Return TShapeList containing the traversed shapes, just like @link(Traverse).
      Caller should never free the resulting list, it is owned by this TShapeTree instance. }
    function TraverseList(
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): TShapeList;

    { How many shapes would be enumerated by @link(Traverse). }
    function ShapesCount(const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false): Cardinal;

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
      This checks all possible ways how a texture may be used by a shape,
      so it looks at material fields,
      shaders (ComposedShader, CommonSurfaceShader) and more.

      If Enumerate callbacks returns non-nil for some texture, returns it immediately,
      and stops further processing. }
    function EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer; virtual; abstract;

    function DebugInfo(const Indent: string = ''): string; virtual; abstract;

    { Using the TX3DNode.InternalSceneShape field,
      you can associate X3D node with a number of TShapeTree instances.
      This allows to map X3D node -> TShapeTree instances instantly
      (without e.g. searching the shapes tree for it),
      which is great to do some operations very quickly.

      Right now:
      - TShapeTreeTransform is associated with node implementing TTransformFunctionality
        (like TTransformNode, TBillboardNode, TX3DRootNode).

      - TShape is associated with
        TShapeNode,
        TAppearanceNode,
        TAbstractGeometryNode,
        TCoordinateNode (anything that can be inside TAbstractGeometryNode.CoordField),
        TNormalNode (anything that can be inside TAbstractGeometryNode.NormalField),
        TTangentNode (anything that can be inside TAbstractGeometryNode.TangentField),
        TColorNode, TColorRGBANode  (anything that can be inside TAbstractGeometryNode.ColorField),
        TMaterialNode (anything that can be in TShapeNode.Material),
        TTextureCoordinateNode and other stuff that can be inside TAbstractGeometryNode.InternalTexCoord,
        TClipPlaneNode .

      Note that UnAssociateNode should only be called on nodes
      with which we are associated. Trying to call UnAssociateNode
      on a node on which we didn't call AssociateNode will have
      undefined results (for speed).

      It is valid to associate X3D node with TShapeTree multiple times,
      but it must be unassociated the same number of times.

      @param Node The node with possibly associated shapes. Never @nil.
    }
    procedure AssociateNode(const Node: TX3DNode);
    procedure UnAssociateNode(const Node: TX3DNode);
    class function AssociatedShape(const Node: TX3DNode; const Index: Integer): TShapeTree; static;
    class function AssociatedShapesCount(const Node: TX3DNode): Integer; static;

    procedure FastTransformUpdate(var AnythingChanged: Boolean); virtual;
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
  strict private
  type
    TShapeValidities = set of (svLocalBBox, svBBox,
      svVerticesCount,
      svTrianglesCount,
      svBoundingSphere,
      svNormals);
    TNormalsCached = (ncSmooth, ncFlat, ncCreaseAngle);

    var
    FLocalBoundingBox: TBox3D;
    FBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: Cardinal;
    Validities: TShapeValidities;
    FBoundingSphereCenter: TVector3;
    FBoundingSphereRadiusSqr: Single;
    FOriginalGeometry: TAbstractGeometryNode;
    FOriginalState: TX3DGraphTraverseState;
    FGeometry: TAbstractGeometryNode;
    FState: TX3DGraphTraverseState;

    FGeometryParentNode,
    FGeometryGrandParentNode,
    FGeometryGrandGrandParentNode: TX3DNode;

    FLocalGeometryChangedCount: Cardinal;
    FDynamicGeometry: Boolean;

    IsCachedMaterialProperty: boolean;
    CachedMaterialProperty: TMaterialProperty;

    FShadowVolumes: TShapeShadowVolumes;

    { Just like Geometry() and State(), except return @nil if no proxy available
      (when Geometry would return the same thing as OriginalGeometry).
      @groupBegin }
    function ProxyGeometry: TAbstractGeometryNode;
    function ProxyState: TX3DGraphTraverseState;
    { @groupEnd }

    procedure ValidateBoundingSphere;

    { Make both FGeometry and FState set.
      Uses appropriate Proxy calls to initialize them. }
    procedure ValidateGeometryState;

    { Make both FGeometry and FState nil (unset),
      freeing eventual instances created by Proxy methods.
      Next Geometry() or State() call will cause Proxy to be recalculated. }
    procedure FreeProxy;

    procedure AssociateGeometryState(
      const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState);
    procedure UnAssociateGeometryState(
      const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState);
    procedure AssociateGeometryStateNeverProxied(
      const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState);
    procedure UnAssociateGeometryStateNeverProxied(
      const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState);
    procedure AssociateProxyGeometryState;
    procedure UnAssociateProxyGeometryState;
  strict private
    function CreateTriangleOctree(const ALimits: TOctreeLimits): TTriangleOctree;
  strict private
    FTriangleOctreeLimits: TOctreeLimits;

    FOctreeTriangles: TTriangleOctree;

    FSpatial: TShapeSpatialStructures;
    procedure SetSpatial(const Value: TShapeSpatialStructures);
  strict private
    {$ifdef SHAPE_OCTREE_USE_MAILBOX}
    { Mailbox, for speeding up collision queries.
      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxResult: PTriangle;
    MailboxIntersection: TVector3;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    { All fields below are meaningful only when svNormals in Validities.
      Normals may be assigned only if svNormals in Validities.
      Moreover, FNormalsCreaseAngle is meaningful only when
      (svNormals in Validities) and (NormalsCached = ncCreaseAngle). }
    FNormalsCached: TNormalsCached;
    FNormalsCachedCcw: boolean;
    FNormals: TVector3List;
    FNormalsCreaseAngle: Single;

    { Free and nil FOctreeTriangles. Also, makes sure to call
      PointingDeviceClear on ParentScene (since some PTriangle pointers
      were freed). }
    procedure FreeOctreeTriangles;
  private
    function MaxShapesCountCore: Integer; override;
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); override;
  public
    { Constructor.
      @param(ParentInfo Recursive information about parents,
        for the geometry node of given shape.
        Note that for VRML 2.0/X3D, the immediate parent
        of geometry node is always TShapeNode.) }
    constructor Create(const AParentScene: TX3DEventsEngine;
      const AOriginalGeometry: TAbstractGeometryNode;
      const AOriginalState: TX3DGraphTraverseState;
      const ParentInfo: PTraversingInfo);
    destructor Destroy; override;

    { Original geometry node, that you get from a VRML/X3D graph. }
    property OriginalGeometry: TAbstractGeometryNode read FOriginalGeometry;

    { Original state, that you get from a VRML/X3D graph. }
    property OriginalState: TX3DGraphTraverseState read FOriginalState;

    { Geometry of this shape.
      This may come from initial VRML/X3D node graph (see OriginalGeometry),
      or it may be processed by @link(TAbstractGeometryNode.Proxy)
      for easier handling. }
    function Geometry: TAbstractGeometryNode;

    { State of this shape.
      This may come from initial VRML/X3D node graph (see OriginalState),
      or it may be processed by @link(TAbstractGeometryNode.Proxy)
      for easier handling.

      Owned by this TShape class. }
    function State: TX3DGraphTraverseState;

    { Bounding box in local shape coordinates, i.e. disregarging all transformations
      (any TTransformNode, TCastleTransform) done on top of shape. }
    function LocalBoundingBox: TBox3D;

    { Bounding box in scene coordinates, i.e. in the local coordinates of TCastleSceneCore
      that owns this shape.

      The idea is that TCastleSceneCore.LocalBoundingBox is calculated
      just by summing up the Shape.BoundingBox for each shape.
      So transformations done using TTransformNode are accounted here.
      Parent TCastleTransform are not accounted here. }
    function BoundingBox: TBox3D;

    function VerticesCount: Cardinal; overload;
    function TrianglesCount: Cardinal; overload;

    function VerticesCount(const Ignored: Boolean): Cardinal; overload; deprecated 'use VerticesCount without Boolean argument, it is ignored now';
    function TrianglesCount(const Ignored: Boolean): Cardinal; overload; deprecated 'use TrianglesCount without Boolean argument, it is ignored now';

    { Decompose the geometry into primitives, with arrays of per-vertex data. }
    function GeometryArrays: TGeometryArrays;

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
    function BoundingSphereCenter: TVector3;
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
      inactive X3D graph part (inactive Switch, LOD etc. children).

      Including chTransform in Changes means something more than
      general chTransform (which means that transformation of children changed,
      which implicates many things --- not only shape changes).
      Here, chTransform in Changes means that only the transformation
      of TShape.State changed (so only on fields ignored by
      EqualsNoTransform). }
    procedure Changed(const InactiveOnly: boolean;
      const Changes: TX3DChanges); virtual;

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

    { How should the alpha of the resulting calculation be used.
      Should we use alpha blending (partial transparency),
      or alpha test (yes-or-no transparency)
      or none of it (shape is simply opaque).

      This is determined looking at the @link(TAppearanceNode.AlphaChannel) field.
      By default, it is acAuto, which in turn means that the final value
      of this method (which cannot be acAuto) is calculated
      looking at material, color, texture nodes data (including at texture
      images contents). }
    function AlphaChannel: TAlphaChannel;
    function Blending: boolean; deprecated 'use "AlphaChannel = acBlending"';
    function Transparent: boolean; deprecated 'use "AlphaChannel = acBlending"';

    { Is the shape visible.
      Most shapes are visible, except when placed in @link(TCollisionNode.Proxy)
      (which allows to define invisible shapes, only for collision purposes). }
    function Visible: boolean;

    { Is the shape collidable.
      Most shapes are collidable.
      One exception is when @link(TAbstractShapeNode.Collision) is set to scNone,
      which disables collisions.
      Another exception is when the shape is placed inside @link(TCollisionNode) children,
      and then you use @link(TCollisionNode.Enabled) to turn off collisions,
      or @link(TCollisionNode.Proxy) to provide alternative geometry for collisions. }
    function Collidable: boolean;

    { Equivalent to using OctreeTriangles.RayCollision, except this
      wil use the mailbox. }
    function RayCollision(
      const Tag: TMailboxTag;
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    { Equivalent to using OctreeTriangles.SegmentCollision, except this
      wil use the mailbox. }
    function SegmentCollision(
      const Tag: TMailboxTag;
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const Pos1, Pos2: TVector3;
      const ReturnClosestIntersection: boolean;
      const TriangleToIgnore: PTriangle;
      const IgnoreMarginAtStart: boolean;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;

    { Create normals suitable for this shape.

      You can call this only when Geometry is coordinate-based
      X3D geometry, implementing Coord and having non-empty coordinates
      (that is, Geometry.Coord returns @true and sets ACoord <> @nil),
      and having Geometry.CoordIndex <> @nil.

      For NormalsSmooth, also Geometry.CoordIndex = @nil is allowed,
      but make sure that Geometry.InternalCoordPolygons is available.
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

      @groupBegin }
    function NormalsSmooth(const FromCcw: boolean): TVector3List;
    function NormalsFlat(const FromCcw: boolean): TVector3List;
    function NormalsCreaseAngle(const FromCcw: boolean;
      const CreaseAngle: Single): TVector3List;
    { @groupEnd }

    function EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;

    { Is the texture node Node possibly used by this shape.
      This is equivalent to checking does EnumerateShapeTextures return this shape. }
    function UsesTexture(Node: TAbstractTextureNode): boolean;

    { Check is shape a shadow caster. Looks at Shape's
      Appearance.shadowCaster field (see
      https://castle-engine.io/x3d_extensions.php#section_ext_shadow_caster). }
    function ShadowCaster: boolean;

    { Triangulate shape. Calls TriangleEvent callback for each triangle.
      LocalTriangulate returns coordinates in local shape transformation
      (that is, not transformed by State.Transform yet).

      @groupBegin }
    procedure Triangulate(const TriangleEvent: TTriangleEvent);
    procedure LocalTriangulate(const TriangleEvent: TTriangleEvent);
    { @groupEnd }

    function DebugInfo(const Indent: string = ''): string; override;
    function NiceName: string;

    { Local geometry changes very often (like every frame).
      This is automatically detected and set to @true.
      May be used as an optimization hint. }
    property DynamicGeometry: Boolean read FDynamicGeometry;

    { Shape node in X3D graph.
      This is always non-nil for X3D, VRML 2 and all model formats that are imported
      as X3D, like glTF, Spine, sprite sheets and more.
      This is @nil only for old VRML 1.0 and Inventor model formats. }
    function Node: TAbstractShapeNode;

    { Parent, grand-parent, grand-grand-parent nodes determined during traversing
      of the X3D node graph.

      Note that GeometryParentNode is almost always equal to @link(Node),
      for X3D, VRML 2 and all model formats that are imported
      as X3D, like glTF, Spine, sprite sheets and more.
      That's because in VRML 2 and X3D, parent of geometry node must be a TShapeNode.

      Only in old VRML 1.0 and Inventor model formats the GeometryParentNode
      may be different than @link(Node).

      All these nodes (GeometryParentNode, GeometryGrandParentNode and
      GeometryGrandGrandParentNode) may be useful if you
      need to take a different action depending on the parent nodes of this shape.

      @groupBegin }
    property GeometryParentNode: TX3DNode read FGeometryParentNode;
    property GeometryGrandParentNode: TX3DNode read FGeometryGrandParentNode;
    property GeometryGrandGrandParentNode: TX3DNode read FGeometryGrandGrandParentNode;
    { @groupEnd }

    function GeometryParentNodeName: String; deprecated 'use GeometryParentNode.X3DName';
    function GeometryGrandParentNodeName: String; deprecated 'use GeometryGrandParentNode.X3DName';
    function GeometryGrandGrandParentNodeName: String; deprecated 'use GeometryGrandGrandParentNode.X3DName';

    { Material property associated with this shape's material/texture. }
    function InternalMaterialProperty: TMaterialProperty;
    function MaterialProperty: TMaterialProperty; deprecated 'use InternalMaterialProperty, or (better) do not use it at all -- this is internal';

    { @exclude }
    property InternalShadowVolumes: TShapeShadowVolumes read FShadowVolumes;

    { @exclude
      Called internally by the engine when changing some stuff in shapes.
      User code should not use it (as we do not guarantee what is OK
      to change this way, and what should rebuild shapes graph),
      instead user code should change X3D scene graph,
      calling @link(TCastleSceneCore.BeforeNodesFree) earlier,
      and call @link(TX3DField.Changed) after each change. }
    procedure InternalBeforeChange;
    { @exclude }
    procedure InternalAfterChange;
  end;

  TShapeTreeList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeTree>;

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
  strict private
    FChildren: TShapeTreeList;
    procedure ChildrenChanged(Sender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Item: TShapeTree;
      Action: TCollectionNotification);
  private
    function MaxShapesCountCore: Integer; override;
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); override;
  public
    constructor Create(const AParentScene: TX3DEventsEngine);
    destructor Destroy; override;

    property Children: TShapeTreeList read FChildren;

    function EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;

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
  strict private
    FSwitchNode: TSwitchNode;
  private
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
  public
    property SwitchNode: TSwitchNode read FSwitchNode write FSwitchNode;

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
  strict private
    FTransformFunctionality: TTransformFunctionality;
    FTransformState: TX3DGraphTraverseState;
    procedure SetTransformFunctionality(const Value: TTransformFunctionality);
  private
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); override;
  public
    constructor Create(const AParentScene: TX3DEventsEngine);
    destructor Destroy; override;
    procedure FastTransformUpdate(var AnythingChanged: Boolean); override;

    property TransformFunctionality: TTransformFunctionality
      read FTransformFunctionality write SetTransformFunctionality;

    function TransformNode: TX3DNode;

    { State right before traversing the TransformNode.
      Owned by this TShapeTreeTransform instance. You should assign
      to it when you set TransformNode. }
    property TransformState: TX3DGraphTraverseState read FTransformState;

    function DebugInfo(const Indent: string = ''): string; override;
  end;

  { Node of the TShapeTree representing the LOD (level of detail) alternative.
    It chooses one child from it's children list as active.
    Represents the VRML >= 2.0 LOD node
    (not possible for VRML 1.0 LOD node, as it may affect also other
    nodes after LOD).

    To choose which child is active we need to know the LOD node,
    with it's transformation in VRML graph.
    This information is in LODNode and LODInverseTransform properties.

    Also, we need to know the current camera position.
    This is passed as CameraPosition to CalculateLevel.
    Note that this class doesn't call CalculateLevel by itself, never.
    You have to call CalculateLevel, and use it to set Level property,
    from parent scene to make this LOD work. (Reasoning behind this decision:
    parent scene has CameraPosition and such, and parent scene
    knows whether to initiate level_changes event sending.) }
  TShapeTreeLOD = class(TShapeTreeGroup)
  strict private
    FLODNode: TLODNode;
    FLODInverseTransform: TMatrix4;
    FLevel: Cardinal;
    FWasLevel_ChangedSend: boolean;
  private
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
  public
    property LODNode: TLODNode read FLODNode write FLODNode;
    function LODInverseTransform: PMatrix4;

    { Calculate @link(Level). This only calculates level, doesn't
      assign @link(Level) property or send level_changed event. }
    function CalculateLevel(const CameraPosition: TVector3): Cardinal;

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

    {$ifdef SHAPE_ITERATOR_SOPHISTICATED}
    function IterateBeginIndex(OnlyActive: boolean): Integer; override;
    function IterateEndIndex(OnlyActive: boolean): Cardinal; override;
    {$endif}
  end;

  TProximitySensorInstance = class(TShapeTree)
  strict private
    FNode: TProximitySensorNode;
  private
    function MaxShapesCountCore: Integer; override;
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); override;
  public
    InverseTransform: TMatrix4;
    IsActive: boolean;

    property Node: TProximitySensorNode read FNode write FNode;

    function EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;
    function DebugInfo(const Indent: string = ''): string; override;
  end;

  TVisibilitySensorInstance = class(TShapeTree)
  strict private
    FNode: TVisibilitySensorNode;
  private
    function MaxShapesCountCore: Integer; override;
    procedure TraverseCore(const Func: TShapeTraverseFunc;
      const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); override;
    procedure FastTransformUpdateCore(var AnythingChanged: Boolean;
      const ParentTransformation: TTransformation); override;
  public
    { Bounding box of this visibility sensor instance,
      already transformed to global VRML/X3D scene coordinates.
      That is, transformed by parent Transform and similar nodes. }
    Box: TBox3D;
    Transform: TMatrix4;

    property Node: TVisibilitySensorNode read FNode write FNode;

    function EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer; override;
    function DebugInfo(const Indent: string = ''): string; override;
  end;

  { Iterates over all TShape items that would be enumerated by
    Tree.Traverse. Sometimes it's easier to write code using this iterator
    than to create callbacks and use TShapeTree.Traverse. }
  TShapeTreeIterator = class
  strict private
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
    constructor Create(const Tree: TShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false);
    destructor Destroy; override;
    function GetNext: boolean;
    property Current: TShape read FCurrent;
  end deprecated{ 'use Tree.TraverseList(...)'};

  TShapeList = class({$ifdef FPC}specialize{$endif} TObjectList<TShape>)
  strict private
    SortPosition: TVector3;
    function IsSmallerFrontToBack(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
    function IsSmallerBackToFront2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
    function IsSmallerBackToFront3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
    function IsSmallerBackToFront3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
    function IsSmallerBackToFront3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
  private
    { Like regular Add, but parameter is "const" to satisfy TShapeTraverseFunc signature. }
    procedure AddConst(const S: TShape);
  public
    constructor Create; overload;

    { Constructor that initializes list contents by traversing given tree. }
    constructor Create(const Tree: TShapeTree; const OnlyActive: boolean;
      const OnlyVisible: boolean = false;
      const OnlyCollidable: boolean = false); overload; deprecated 'use Tree.TraverseList(...)';

    { Sort shapes by distance to given Position point, closest first. }
    procedure SortFrontToBack(const Position: TVector3);

    { Sort shapes by distance to given Position point, farthest first.

      BlendingSort determines the sorting algorithm.
      See @link(TBlendingSort) documentation. }
    procedure SortBackToFront(const Position: TVector3;
      const BlendingSort: TBlendingSort);
  end;

var
  { If nonzero, disables automatic TShape.DynamicGeometry detection
    on every node modification. This is useful if you do some interactive
    editing of the shape, but you don't want to mark shape as dynamic. }
  DisableAutoDynamicGeometry: Cardinal;

  { Log various information about shapes. This displays quite a lot of non-critical
    information when opening non-trivial models. }
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
    TShape.Node.X3DName, TShape.GeometryParentNode.X3DName,
    TShape.GeometryGrandParentNode.X3DName,
    TShape.GeometryGrandGrandParentNode.X3DName.

    Preferably, the result should be unique, only for this VRML/X3D shape.
    But in practice it's the responsibility of the modeler
    and model author to make it true.
    For example, modelers that allow multiple materials on object (like
    Blender) @italic(must) split a single 3D object into many VRML/X3D shapes
    sometimes.
    So just don't use shapes with multiple materials if this shape
    may be meaningful for a placeholder.

    This is used only by TLevel.Load placeholders.
    Ultimately, this should be something that is easy to set when creating
    a 3D model in given external modeler.
    @italic(Nothing else in our engine depends on a particular modeler
    strategy for exporting VRML/X3D models.)

    This should be object name (to allow sharing a single mesh underneath).
    Except when it's not possible (like for old Blender VRML 1.0 exporter,
    when only mesh names are stored in VRML/X3D exported files),
    in which case it can be a mesh name. }
  TPlaceholderName = function (const Shape: TShape): string;
  TPlaceholderNames = class({$ifdef FPC}specialize{$endif} TDictionary<string, TPlaceholderName>)
  strict private
    function GetItems(const AKey: string): TPlaceholderName;
    procedure SetItems(const AKey: string; const AValue: TPlaceholderName);
  public
    { Access dictionary items.
      Setting this is allowed regardless if the key previously existed or not,
      in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
      that only allows setting when the key already exists. }
    property Items [const AKey: string]: TPlaceholderName read GetItems write SetItems; default;
  end;

var
  PlaceholderNames: TPlaceholderNames;

implementation

uses Generics.Defaults, Math,
  CastleSceneCore, CastleInternalNormals, CastleLog, CastleTimeUtils,
  CastleStringUtils, CastleInternalArraysGenerator, CastleURIUtils;

const
  UnknownTexCoord: TTriangle4 = (Data: (
    (X: 0; Y: 0; Z: 0; W: 1),
    (X: 0; Y: 0; Z: 0; W: 1),
    (X: 0; Y: 0; Z: 0; W: 1)
  ));

{ TTriangleHelper ------------------------------------------------------------ }

function TTriangleHelper.Shape: TShape;
begin
  Assert(InternalShape is TShape); // will be optimized out in RELEASE mode
  Result := TShape(InternalShape);
end;

function TTriangleHelper.State: TX3DGraphTraverseState;
begin
  Result := TShape(InternalShape).State;
end;

procedure TTriangleHelper.UpdateWorld;
begin
  World.Triangle := Local.Triangle.Transform(State.Transformation.Transform);
  {$ifndef CONSERVE_TRIANGLE_MEMORY_MORE}
  World.Plane := World.Triangle.NormalizedPlane;
  World.Area := World.Triangle.Area;
  {$endif}
end;

function TTriangleHelper.ShapeNode: TAbstractShapeNode;
begin
  Result := State.ShapeNode;
end;

function TTriangleHelper.Material: TMaterialNode;
var
  S: TAbstractShapeNode;
begin
  S := ShapeNode;
  if (S <> nil) and (S.Material is TMaterialNode) then
    Result := TMaterialNode(S.Material)
  else
    Result := nil;
end;

function TTriangleHelper.MaterialNode: TMaterialNode;
begin
  {$warnings off} // using deprecated in deprecated
  Result := Material;
  {$warnings on}
end;

function TTriangleHelper.MaterialInfo: TMaterialInfo;
begin
  Result := State.MaterialInfo;
end;

function TTriangleHelper.Transparency: Single;
var
  M: TMaterialInfo;
begin
  M := MaterialInfo;
  if M <> nil then
    Result := M.Transparency
  else
    Result := 0;
end;

function TTriangleHelper.IsTransparent: boolean;
begin
  Result := Transparency > SingleEpsilon;
end;

function TTriangleHelper.IgnoreForShadowRays: boolean;

  function NonShadowCaster(State: TX3DGraphTraverseState): boolean;
  var
    Shape: TAbstractShapeNode;
  begin
    Shape := State.ShapeNode;
    Result :=
      (Shape <> nil) and
      (Shape.FdAppearance.Value <> nil) and
      (Shape.FdAppearance.Value is TAppearanceNode) and
      (not TAppearanceNode(Shape.FdAppearance.Value).FdShadowCaster.Value);
  end;

begin
  Result := ({ IsTransparent } Transparency > SingleEpsilon) or
    NonShadowCaster(State);
end;

{$ifndef CONSERVE_TRIANGLE_MEMORY}
function TTriangleHelper.INormalWorldSpace(const Point: TVector3): TVector3;
begin
  Result := State.Transformation.Transform.MultDirection(INormalCore(Point)).Normalize;
end;
{$endif not CONSERVE_TRIANGLE_MEMORY}

{ TShapeTree ------------------------------------------------------------ }

constructor TShapeTree.Create(const AParentScene: TX3DEventsEngine);
begin
  inherited Create;
  FParentScene := AParentScene;
  FMaxShapesCount := -1;
end;

destructor TShapeTree.Destroy;
var
  OnlyActive: Boolean;
  OnlyVisible: Boolean;
  OnlyCollidable: Boolean;
begin
  for OnlyActive := Low(Boolean) to High(Boolean) do
    for OnlyVisible := Low(Boolean) to High(Boolean) do
      for OnlyCollidable := Low(Boolean) to High(Boolean) do
        FreeAndNil(CachedChildrenList[OnlyActive, OnlyVisible, OnlyCollidable]);
  inherited;
end;

function TShapeTree.MaxShapesCount: Integer;
begin
  if FMaxShapesCount = -1 then
    FMaxShapesCount := MaxShapesCountCore;
  Result := FMaxShapesCount;
end;

function TShapeTree.FindGeometryNodeName(
  const GeometryNodeName: string; OnlyActive: boolean): TShape;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  ShapeList := TraverseList(OnlyActive);
  for Shape in ShapeList do
  begin
    Result := Shape;
    if Result.OriginalGeometry.X3DName = GeometryNodeName then Exit;
  end;
  Result := nil;
end;

function TShapeTree.FindShapeWithParentNamed(
  const ParentNodeName: string; OnlyActive: boolean): TShape;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  ShapeList := TraverseList(OnlyActive);
  for Shape in ShapeList do
  begin
    Result := Shape;
    if Result.OriginalGeometry.TryFindParentByName(ParentNodeName) <> nil then Exit;
  end;
  Result := nil;
end;

procedure TShapeTree.AssociateNode(const Node: TX3DNode);
var
  OldShapeTree: TShapeTree;
begin
  { InternalSceneShape is either nil, TShapeTree or TShapeTreeList.
    Memory usage is important here -- we have a lot of nodes in larger scenes,
    and in many cases a Node is associated with at most one TShapeTree.
    So we optimize this common case. }

  if Node.InternalSceneShape = nil then
    Node.InternalSceneShape := Self
  else
  if Node.InternalSceneShape <> nil then
  begin
    { comparing ClassType is faster than "InternalSceneShape is TShapeTreeList",
      and enough in this case. }
    if Node.InternalSceneShape.ClassType <> TShapeTreeList then
    begin
      // convert Node.InternalSceneShape into list with 1 item
      Assert(Node.InternalSceneShape is TShapeTree);
      OldShapeTree := TShapeTree(Node.InternalSceneShape);
      Node.InternalSceneShape := TShapeTreeList.Create(false);
      TShapeTreeList(Node.InternalSceneShape).Add(OldShapeTree);
    end;
    TShapeTreeList(Node.InternalSceneShape).Add(Self);
  end;
end;

procedure TShapeTree.UnAssociateNode(const Node: TX3DNode);
begin
  if Node.InternalSceneShape = Self then
    Node.InternalSceneShape := nil
  else
  begin
    if Node.InternalSceneShape = nil then
    begin
      WritelnWarning('Calling %s.UnAssociateNode on X3D node that is already not associated with anything: %s. This can happen when you manually change nodes.',
        [ClassName, Node.NiceName]);
      Exit;
    end;
    Assert(Node.InternalSceneShape is TShapeTreeList);
    if TShapeTreeList(Node.InternalSceneShape).Count = 1 then
    begin
      Assert(TShapeTreeList(Node.InternalSceneShape)[0] = Self);
      Node.InternalSceneShape.Free;
      Node.InternalSceneShape := nil;
    end else
      TShapeTreeList(Node.InternalSceneShape).Remove(Self);
  end;
end;

class function TShapeTree.AssociatedShape(const Node: TX3DNode; const Index: Integer): TShapeTree; {$ifdef FPC}static;{$endif}
begin
  if Node.InternalSceneShape.ClassType = TShapeTreeList then
    Result := TShapeTreeList(Node.InternalSceneShape)[Index]
  else
    Result := TShapeTree(Node.InternalSceneShape);
end;

class function TShapeTree.AssociatedShapesCount(const Node: TX3DNode): Integer; {$ifdef FPC}static;{$endif}
begin
  if Node.InternalSceneShape = nil then
    Result := 0
  else
  if Node.InternalSceneShape.ClassType <> TShapeTreeList then
  begin
    Assert(Node.InternalSceneShape is TShapeTree);
    Result := 1;
  end else
    Result := TShapeTreeList(Node.InternalSceneShape).Count;
end;

procedure TShapeTree.InvalidateMaxShapesCount;
begin
  FMaxShapesCount := -1;
  if FParent <> nil then
    FParent.InvalidateMaxShapesCount;
end;

function TShapeTree.TraverseList(const OnlyActive, OnlyVisible, OnlyCollidable: Boolean): TShapeList;
var
  CurrentShapesHash: TShapesHash;

  { Call TraverseCore, gather shapes to a new list. }
  procedure TraverseCoreToList;
  begin
    Result := TShapeList.Create;
    { Set Capacity, to make AddToList calls faster.
      Note that we use MaxShapesCount instead of ShapesCount,
      since MaxShapesCount is usually instant, while ShapesCount...
      now ShapesCount depends on TraverseList, so it would cause infinite loop. }
    Result.Capacity := MaxShapesCount;
    TraverseCore({$ifdef FPC}@{$endif} Result.AddConst, OnlyActive, OnlyVisible, OnlyCollidable);
    CachedChildrenListHash[OnlyActive, OnlyVisible, OnlyCollidable] := CurrentShapesHash;
    CachedChildrenList[OnlyActive, OnlyVisible, OnlyCollidable] := Result;
  end;

begin
  Result := CachedChildrenList[OnlyActive, OnlyVisible, OnlyCollidable];
  if Result <> nil then
  begin
    if FParentScene <> nil then
      CurrentShapesHash := TCastleSceneCore(FParentScene).InternalShapesHash
    else
      CurrentShapesHash := 0;
    if CurrentShapesHash <> CachedChildrenListHash[OnlyActive, OnlyVisible, OnlyCollidable] then
    begin
      FreeAndNil(CachedChildrenList[OnlyActive, OnlyVisible, OnlyCollidable]);
      TraverseCoreToList;
    end;
  end else
  begin
    TraverseCoreToList;
  end;
end;

procedure TShapeTree.Traverse(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
var
  I: Integer;
  List: TShapeList;
begin
  List := TraverseList(OnlyActive, OnlyVisible, OnlyCollidable);
  for I := 0 to List.Count - 1 do
    Func(List[I]);
end;

function TShapeTree.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  // Since TraverseList is optimized now by caching, this method can just call TraverseList
  Result := TraverseList(OnlyActive, OnlyVisible, OnlyCollidable).Count;
end;

procedure TShapeTree.FastTransformUpdate(var AnythingChanged: Boolean);
var
  T: TTransformation;
begin
  T.Init;
  FastTransformUpdateCore(AnythingChanged, T);
end;

{ TShape -------------------------------------------------------------- }

constructor TShape.Create(const AParentScene: TX3DEventsEngine;
  const AOriginalGeometry: TAbstractGeometryNode;
  const AOriginalState: TX3DGraphTraverseState;
  const ParentInfo: PTraversingInfo);
var
  PI: PTraversingInfo;
begin
  inherited Create(AParentScene);

  FTriangleOctreeLimits := DefLocalTriangleOctreeLimits;
  FShadowVolumes := TShapeShadowVolumes.Create(Self);

  FOriginalGeometry := AOriginalGeometry;
  FOriginalState := AOriginalState;

  AssociateGeometryState(FOriginalGeometry, FOriginalState);
  AssociateGeometryStateNeverProxied(FOriginalGeometry, FOriginalState);

  PI := ParentInfo;
  if PI <> nil then
  begin
    FGeometryParentNode := PI^.Node;
    PI := PI^.ParentInfo;
    if PI <> nil then
    begin
      FGeometryGrandParentNode := PI^.Node;
      PI := PI^.ParentInfo;
      if PI <> nil then
        FGeometryGrandGrandParentNode := PI^.Node;
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
  if FOriginalState <> nil then // when exception occurs in constructor, may be nil
  begin
    UnAssociateGeometryState(FOriginalGeometry, FOriginalState);
    UnAssociateGeometryStateNeverProxied(FOriginalGeometry, FOriginalState);
    FreeAndNil(FOriginalState);
  end;
  FreeOctreeTriangles;
  inherited;
end;

function TShape.GeometryParentNodeName: String;
begin
  if GeometryParentNode <> nil then
    Result := GeometryParentNode.X3DName
  else
    Result := '';
end;

function TShape.GeometryGrandParentNodeName: String;
begin
  if GeometryGrandParentNode <> nil then
    Result := GeometryGrandParentNode.X3DName
  else
    Result := '';
end;

function TShape.GeometryGrandGrandParentNodeName: String;
begin
  if GeometryGrandGrandParentNode <> nil then
    Result := GeometryGrandGrandParentNode.X3DName
  else
    Result := '';
end;

procedure TShape.AssociateGeometryState(
  const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState);
begin
  if AGeometry <> nil then
  begin
    AssociateNode(AGeometry);
    if (AGeometry.ColorField <> nil) and
       (AGeometry.ColorField.Value <> nil) then
      AssociateNode(AGeometry.ColorField.Value);
    if (AGeometry.CoordField <> nil) and
       (AGeometry.CoordField.Value <> nil) then
      AssociateNode(AGeometry.CoordField.Value);
    if (AGeometry.NormalField <> nil) and
       (AGeometry.NormalField.Value <> nil) then
      AssociateNode(AGeometry.NormalField.Value);
    if (AGeometry.TangentField <> nil) and
       (AGeometry.TangentField.Value <> nil) then
      AssociateNode(AGeometry.TangentField.Value);
    if (AGeometry.TexCoordField <> nil) and
       (AGeometry.TexCoordField.Value <> nil) and
       { TODO: This workarounds assertion failure in UnAssociateNode
         when using shadow maps on a primitive, like Sphere.
         Reproducible by view3dscene (open and close
         demo-models/shadow_maps/primitives.x3dv )
         and automatic tests (when TTestOpeningAndRendering3D.TestScene
         opens and closes tests/data/warning_when_new_node_as_shadow_map_light.x3dv ).
         The cause is unknown.
         But in any case, associating with TMultiTextureCoordinateNode
         is not really useful (we should associate with children of it). }
       (not (AGeometry.TexCoordField.Value is TMultiTextureCoordinateNode)) then
      AssociateNode(AGeometry.TexCoordField.Value);
  end;
end;

procedure TShape.AssociateGeometryStateNeverProxied(
  const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState);
var
  I: Integer;
begin
  if AState <> nil then
  begin
    if AState.ShapeNode <> nil then
    begin
      AssociateNode(AState.ShapeNode);
      if AState.ShapeNode.Appearance <> nil then
        AssociateNode(AState.ShapeNode.Appearance);
      if AState.ShapeNode.Material <> nil then
        AssociateNode(AState.ShapeNode.Material);
    end;
    if AState.ClipPlanes <> nil then
      for I := 0 to AState.ClipPlanes.Count - 1 do
        AssociateNode(AState.ClipPlanes[I].Node);
  end;
end;

procedure TShape.UnAssociateGeometryState(
  const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState);
begin
  if AGeometry <> nil then
  begin
    UnAssociateNode(AGeometry);
    if (AGeometry.ColorField <> nil) and
       (AGeometry.ColorField.Value <> nil) then
      UnAssociateNode(AGeometry.ColorField.Value);
    if (AGeometry.CoordField <> nil) and
       (AGeometry.CoordField.Value <> nil) then
      UnAssociateNode(AGeometry.CoordField.Value);
    if (AGeometry.NormalField <> nil) and
       (AGeometry.NormalField.Value <> nil) then
      UnAssociateNode(AGeometry.NormalField.Value);
    if (AGeometry.TangentField <> nil) and
       (AGeometry.TangentField.Value <> nil) then
      UnAssociateNode(AGeometry.TangentField.Value);
    if (AGeometry.TexCoordField <> nil) and
       (AGeometry.TexCoordField.Value <> nil) and
       (not (AGeometry.TexCoordField.Value is TMultiTextureCoordinateNode)) then
      UnAssociateNode(AGeometry.TexCoordField.Value);
  end;
end;

procedure TShape.UnAssociateGeometryStateNeverProxied(
  const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState);
var
  I: Integer;
begin
  if AState <> nil then
  begin
    if AState.ShapeNode <> nil then
    begin
      UnAssociateNode(AState.ShapeNode);
      if AState.ShapeNode.Appearance <> nil then
        UnAssociateNode(AState.ShapeNode.Appearance);
      if AState.ShapeNode.Material <> nil then
        UnAssociateNode(AState.ShapeNode.Material);
    end;
    if AState.ClipPlanes <> nil then
      for I := 0 to AState.ClipPlanes.Count - 1 do
        UnAssociateNode(AState.ClipPlanes[I].Node);
  end;
end;

procedure TShape.AssociateProxyGeometryState;
begin
  Assert(FGeometry <> nil);
  Assert(FState    <> nil);

  { For speed, do not even call AssociateGeometryState
    when the proxy is equal to actual nodes.
    This way in an usual case, a geometry node will have only 1 associated shape,
    which is best for memory usage. }

  if (FGeometry <> FOriginalGeometry) or
     (FState    <> FOriginalState) then
  begin
    AssociateGeometryState(FGeometry, FState);
  end;
end;

procedure TShape.UnAssociateProxyGeometryState;
begin
  Assert(FGeometry <> nil);
  Assert(FState    <> nil);

  if (FGeometry <> FOriginalGeometry) or
     (FState    <> FOriginalState) then
  begin
    UnAssociateGeometryState(FGeometry, FState);
  end;
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
var
  TimeStart: TCastleProfilerTime;
  S: String;
begin
  if (ssTriangles in InternalSpatial) and (FOctreeTriangles = nil) then
  begin
    S := 'Creating octree for shape ' + NiceName;
    if LogChanges then
      WritelnLog('X3D changes (octree)', S);
    TimeStart := Profiler.Start(S);
    try
      FOctreeTriangles := CreateTriangleOctree(FTriangleOctreeLimits);
    finally
      Profiler.Stop(TimeStart);
    end;
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
      ProxyGeometry, ProxyState);
    Include(Validities, svLocalBBox);
  end;
  Result := FLocalBoundingBox;
end;

function TShape.BoundingBox: TBox3D;
begin
  if not (svBBox in Validities) then
  begin
    FBoundingBox := OriginalGeometry.BoundingBox(OriginalState,
      ProxyGeometry, ProxyState);
    Include(Validities, svBBox);
  end;
  Result := FBoundingBox;
end;

function TShape.VerticesCount: Cardinal;

  procedure Calculate;
  begin
    FVerticesCount := OriginalGeometry.VerticesCount(
      OriginalState, ProxyGeometry, ProxyState);
  end;

begin
  if not (svVerticesCount in Validities) then
  begin
    Calculate;
    Include(Validities, svVerticesCount);
  end;
  Result := FVerticesCount;
end;

function TShape.TrianglesCount: Cardinal;

  procedure Calculate;
  begin
    FTrianglesCount := OriginalGeometry.TrianglesCount(
      OriginalState, ProxyGeometry, ProxyState);
  end;

begin
  if not (svTrianglesCount in Validities) then
  begin
    Calculate;
    Include(Validities, svTrianglesCount);
  end;
  Result := FTrianglesCount;
end;

function TShape.VerticesCount(const Ignored: Boolean): Cardinal;
begin
  Result := VerticesCount();
end;

function TShape.TrianglesCount(const Ignored: Boolean): Cardinal;
begin
  Result := TrianglesCount();
end;

function TShape.GeometryArrays: TGeometryArrays;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;

  procedure TexCoordsNeededForMapping(var Needed: Cardinal; const Mapping: String);
  var
    Index: Integer;
  begin
    if G.FindTextureMapping(Mapping, Index, false) <> nil then
      MaxVar(Needed, Index + 1);
  end;

  function TexCoordsNeeded: Cardinal;
  var
    Tex: TAbstractTextureNode;
    SurfaceShader: TCommonSurfaceShaderNode;
    MatPhong: TMaterialNode;
    MatPhysical: TPhysicalMaterialNode;
    MatOne: TAbstractOneSidedMaterialNode;
  begin
    Tex := S.MainTexture;

    if Tex is TMultiTextureNode then
      Result := TMultiTextureNode(Tex).FdTexture.Count
    else
    if Tex <> nil then
      Result := 1
    else
      Result := 0;

    { we need at least 1 texture coordinate if some special texture
      (not necessarily diffuse texture) is used }
    if (S.ShapeNode <> nil) and
       (S.ShapeNode.Appearance <> nil) then
    begin
      // CommonSurfaceShader can only be non-nil if Appearance is non-nil
      {$warnings off} // using deprecated to keep backward compatibility
      SurfaceShader := S.ShapeNode.CommonSurfaceShader;
      {$warnings on}
      if SurfaceShader <> nil then
      begin
        if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
          MaxVar(Result, SurfaceShader.DiffuseTextureCoordinatesId + 1); // TODO test X3DOM/InstantReality are they using DiffuseTextureCoordinatesId in this case
        if SurfaceShader.DiffuseTexture <> nil then
          MaxVar(Result, SurfaceShader.DiffuseTextureCoordinatesId + 1);
        if SurfaceShader.NormalTexture <> nil then
          MaxVar(Result, SurfaceShader.NormalTextureCoordinatesId + 1);
        if SurfaceShader.AmbientTexture <> nil then
          MaxVar(Result, SurfaceShader.AmbientTextureCoordinatesId + 1);
        if SurfaceShader.SpecularTexture <> nil then
          MaxVar(Result, SurfaceShader.SpecularTextureCoordinatesId + 1);
        if SurfaceShader.ShininessTexture <> nil then
          MaxVar(Result, SurfaceShader.ShininessTextureCoordinatesId + 1);
      end else
      begin
        if S.ShapeNode.Appearance.NormalMap <> nil then
          MaxVar(Result, 1);
        if S.ShapeNode.Appearance.Material is TAbstractOneSidedMaterialNode then
        begin
          MatOne := TAbstractOneSidedMaterialNode(S.ShapeNode.Appearance.Material);
          if MatOne.EmissiveTexture <> nil then
            TexCoordsNeededForMapping(Result, MatOne.EmissiveTextureMapping);
          if MatOne.NormalTexture <> nil then
            TexCoordsNeededForMapping(Result, MatOne.NormalTextureMapping);
        end;
        if S.ShapeNode.Appearance.Material is TMaterialNode then
        begin
          MatPhong := TMaterialNode(S.ShapeNode.Appearance.Material);
          if MatPhong.AmbientTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhong.AmbientTextureMapping);
          if MatPhong.DiffuseTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhong.DiffuseTextureMapping);
          if MatPhong.ShininessTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhong.ShininessTextureMapping);
          if MatPhong.SpecularTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhong.SpecularTextureMapping);
          if MatPhong.OcclusionTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhong.OcclusionTextureMapping);
        end;
        if S.ShapeNode.Appearance.Material is TPhysicalMaterialNode then
        begin
          MatPhysical := TPhysicalMaterialNode(S.ShapeNode.Appearance.Material);
          if MatPhysical.BaseTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhysical.BaseTextureMapping);
          if MatPhysical.MetallicRoughnessTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhysical.MetallicRoughnessTextureMapping);
          if MatPhysical.OcclusionTexture <> nil then
            TexCoordsNeededForMapping(Result, MatPhysical.OcclusionTextureMapping);
        end;
      end;
    end;

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
      Result.Primitive := gpTriangleFan;
      Result.Count := 4;

      Result.Position(0)^ := Vector3(Box.Data[0][0], Box.Data[0][1], Box.Data[0][2]);
      Result.Position(1)^ := Vector3(Box.Data[1][0], Box.Data[0][1], Box.Data[0][2]);
      Result.Position(2)^ := Vector3(Box.Data[1][0], Box.Data[1][1], Box.Data[0][2]);
      Result.Position(3)^ := Vector3(Box.Data[0][0], Box.Data[1][1], Box.Data[0][2]);

      Result.Normal(0)^ := TVector3.One[2];
      Result.Normal(1)^ := TVector3.One[2];
      Result.Normal(2)^ := TVector3.One[2];
      Result.Normal(3)^ := TVector3.One[2];
    end;
  end;

var
  GeneratorClass: TArraysGeneratorClass;
  Generator: TArraysGenerator;
begin
  G := Geometry;
  S := State;
  GeneratorClass := GetArraysGenerator(G);
  if GeneratorClass <> nil then
  begin
    Generator := GeneratorClass.Create(Self);
    try
      Generator.TexCoordsNeeded := TexCoordsNeeded;
      Generator.FacesNeeded := true;
      { Leave the rest of Generator properties as default }
      Result := Generator.GenerateArrays;
    finally FreeAndNil(Generator) end;
  end else
    Result := ArrayForBox(LocalBoundingBox);
end;

procedure TShape.FreeProxy;
begin
  if LogChanges and
    { OriginalGeometry should always be <> nil, but just in case
      (e.g. running from destructor, or with bad state) check. }
    (OriginalGeometry <> nil) and
    (
    ( (FGeometry <> OriginalGeometry) and (FGeometry <> nil) ) or
    ( (FState    <> OriginalState   ) and (FState    <> nil) )
    ) then
    WritelnLog('X3D changes', 'Releasing the Proxy geometry of ' + OriginalGeometry.ClassName);

  if FGeometry <> nil then
    UnAssociateProxyGeometryState;

  if FGeometry <> OriginalGeometry then
    FreeAndNil(FGeometry)
  else
    FGeometry := nil;

  if FState <> OriginalState then
    FreeAndNil(FState)
  else
    FState := nil;

  Assert(FGeometry = nil);
  Assert(FState = nil);
end;

procedure TShape.Changed(const InactiveOnly: boolean;
  const Changes: TX3DChanges);

  { Called when local geometry changed.

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
    Also notifies parent scene about this change. }
  procedure LocalGeometryChanged(const ChangedOnlyCoord: boolean);
  begin
    if FLocalGeometryChangedCount <> 0 then
    begin
      if DisableAutoDynamicGeometry = 0 then
        FDynamicGeometry := true;
    end else
      Inc(FLocalGeometryChangedCount); // for now, only increase FLocalGeometryChangedCount to 1

    if (FOctreeTriangles <> nil) and
       { Do not recreate octree if it's based only on our bounding box,
         and our bounding box is stored in TShapeNode.Box (so it doesn't change when
         geometry changes).
         This is the case with glTF skinned animation. }
       not (
         (Node <> nil) and
         (Node.Collision in [scBox, scNone]) and
         (not Node.BBox.IsEmpty)
       ) then
      FreeOctreeTriangles;

    { Remove cached normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    { Remove from Validities things that depend on geometry.
      Local geometry change means that also global (world-space) geometry changed. }
    Validities := Validities - [svLocalBBox, svBBox,
      svVerticesCount,
      svTrianglesCount,
      svBoundingSphere,
      svNormals];

    { Clear variables after removing fvTrianglesList* }
    FShadowVolumes.InvalidateTrianglesListShadowCasters;

    { Edges topology possibly changed. }
    if not ChangedOnlyCoord then
      { When ChangedOnlyCoord, we don't do InvalidateManifoldAndBorderEdges,
        and this an important optimization (makes mesh deformation cheaper). }
      FShadowVolumes.InvalidateManifoldAndBorderEdges;

    if ChangedOnlyCoord then
      TCastleSceneCore(ParentScene).DoGeometryChanged(gcLocalGeometryChangedCoord, Self)
    else
      TCastleSceneCore(ParentScene).DoGeometryChanged(gcLocalGeometryChanged, Self);
  end;

begin
  { Remember to code everything here to act only when some stuff
    is included inside Changed value. For example, when
    Changes = [chClipPlane], there's no need to do anything here. }

  { When Proxy needs to be recalculated.
    Include chVisibleVRML1State, since even MaterialBinding may change VRML 1.0
    proxies. }
  if Changes * [
       chCoordinate,
       chNormal,
       chTangent,
       chVisibleVRML1State,
       chGeometryVRML1State,
       chTextureCoordinate,
       chGeometry,
       chGeometryFontChanged,
       chWireframe,
       chFontStyle,
       chFontStyleFontChanged
     ] <> [] then
    FreeProxy;

  { When bounding volumes in global coordinates changed.
    Probably only chTransform is really needed here
    (testcase: upwind_turbine.x3d), as other flags already cause other changes
    that invalidate global bboxes anyway. }
  if Changes * [
       chTransform,
       chCoordinate,
       chGeometry,
       chGeometryFontChanged,
       chGeometryVRML1State,
       chEverything,
       chFontStyle,
       chFontStyleFontChanged
     ] <> [] then
    Validities := Validities - [svBBox, svBoundingSphere];

  { Changes to actual geometry that are limited to Coordinate (topology or other things don't change). }
  if chCoordinate in Changes then
    LocalGeometryChanged(true);

  { Changes to actual geometry (other). }
  if Changes * [
       chGeometry,
       chGeometryFontChanged,
       chGeometryVRML1State,
       chWireframe,
       chFontStyle,
       chFontStyleFontChanged
     ] <> [] then
    LocalGeometryChanged(false);

  if Changes * [chBBox] <> [] then
  begin
    Validities := Validities - [svLocalBBox, svBBox];
    if (Node <> nil) and (Node.Collision in [scBox, scNone]) then
      FreeOctreeTriangles; // bbox changed, so simple octree based on bbox also changed
  end;

  { Recreate TTextNode.FontTextureNode when FontStyle parameters,
    like font family/bold/italic changed. }
  if Changes * [
       chGeometryFontChanged,
       chFontStyleFontChanged
     ] <> [] then
    (FOriginalGeometry as TTextNode).FontChanged;

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

function TShape.BoundingSphereCenter: TVector3;
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

function TShape.CreateTriangleOctree(
  const ALimits: TOctreeLimits): TTriangleOctree;

  procedure LocalTriangulateBox(const Box: TBox3D);

    procedure LocalTriangulateRect(const constCoord: T3DAxis;
      const constCoordValue, x1, y1, x2, y2: Single);
    var
      Position, Normal: TTriangle3;
      i: Integer;
      c1, c2: T3DAxis;

      procedure TriAssign(TriIndex: integer; c1value, c2value: Single);
      begin
        Position.Data[TriIndex].Data[c1] := c1value;
        Position.Data[TriIndex].Data[c2] := c2value;
      end;

    begin
      RestOf3DCoords(constCoord, c1, c2);

      for I := 0 to 2 do
      begin
        Position.Data[I].Data[ConstCoord] := ConstCoordValue;
        {$warnings off} // silence FPC warning about Normal uninitialized
        Normal.Data[I].Data[C1] := 0;
        {$warnings on}
        Normal.Data[I].Data[C2] := 0;
        Normal.Data[I].Data[ConstCoord] := 1; { TODO: or -1 }
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
    I, XCoord, YCoord: T3DAxis;
  begin
    for I := 0 to 2 do
    begin
      RestOf3DCoords(I, XCoord, YCoord);
      LocalTriangulateRect(I, Box.Data[0][I], Box.Data[0][XCoord], Box.Data[0][YCoord], Box.Data[1][XCoord], Box.Data[1][YCoord]);
      LocalTriangulateRect(I, Box.Data[1][I], Box.Data[0][XCoord], Box.Data[0][YCoord], Box.Data[1][XCoord], Box.Data[1][YCoord]);
    end;
    { We have 12 triangles, DefTriangleOctreeLeafCapacity should be at least 12,
      and tree root should remain leaf.
      This should be a simple tree. }
    Assert(Result.TreeRoot.IsLeaf);
  end;

begin
  Result := TTriangleOctree.Create(ALimits, LocalBoundingBox);
  try
    if (Node <> nil) and (Node.Collision in [scBox, scNone]) then
    begin
      { Add 12 triangles for 6 cube (LocalBoundingBox) sides. }
      Result.Triangles.Capacity := 12;
      LocalTriangulateBox(LocalBoundingBox);
    end else
    begin
      Result.Triangles.Capacity := TrianglesCount;
      LocalTriangulate({$ifdef FPC}@{$endif}Result.AddItemTriangle);
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
  if FSpatial <> Value then
  begin
    Old := ssTriangles in InternalSpatial;
    New := ssTriangles in Value;

    FSpatial := Value;

    { Handle OctreeTriangles }

    if Old and not New then
      FreeOctreeTriangles;
    if New and not Old then
      { We do not strictly need to create FOctreeTriangles now,
        we could let it be created on-demand.
        But experience shows that it is better to create it now.
        Otherwise moving around the scene would create sudden stutters
        as we load octrees, for each shape separately, when needed.

        Note: we were loading not-on-demand before 2010, and switched
        to only on-demand in
        https://github.com/castle-engine/castle-engine/commit/d5030af3a3372fccaeb2472e9d0bd390190b5f2a .
        But it was wrong.
        "Lynch" demo https://github.com/michaliskambi/lynch
        clearly shows that on-demand causes bad delays at playing.
        Moreover, TCastleSceneCore.SetSpatial was actually workarounding
        it (calling "Shape.InternalOctreeTriangles" right after setting
        "Shape.InternalSpatial := Value") for some time,
        but it just wasn't perfect,
        because if we did "Scene.Spatial := [ssDynamicCollisions]"
        but later "Scene.URL := ..." then didn't create octree for new shapes.

        Note: InternalOctreeTriangles only does the job if FSpatial is already set to non-empty.
      }
      InternalOctreeTriangles;
  end;
end;

function TShape.Transparent: boolean;
begin
  Result := AlphaChannel = acBlending;
end;

function TShape.Blending: boolean;
begin
  Result := AlphaChannel = acBlending;
end;

function TShape.AlphaChannel: TAlphaChannel;

  function DetectAlphaBlending: boolean;

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
        result := TMaterialInfo.DefaultTransparency > SingleEpsilon else
      begin
        for i := 0 to Node.FdTransparency.Items.Count-1 do
          if Node.FdTransparency.Items.List^[i] <= SingleEpsilon then
            Exit(false);
        result := true;
      end;
    end;

  var
    M: TMaterialInfo;
    Tex: TAbstractTextureNode;
  begin
    if State.ShapeNode <> nil then
    begin
      M := State.MaterialInfo;
      Result := (M <> nil) and (M.Transparency > SingleEpsilon);
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
      Result := AllMaterialsTransparent(State.VRML1State.Material);

    if Geometry.InternalColorRGBA <> nil then
      Result := true;

    { If texture exists with full range alpha channel then use blending.
      Note that State.Texture may be TMultiTextureNode --- that's Ok,
      it has AlphaChannel = atFullRange
      if any child has atFullRange. So it automatically works Ok too. }
    Tex := State.MainTexture;
    if (Tex <> nil) and (Tex.AlphaChannelFinal = acBlending) then
      Result := true;

    Tex := OriginalGeometry.FontTextureNode;
    if (Tex <> nil) and (Tex.AlphaChannelFinal = acBlending) then
      Result := true;
  end;

  function DetectAlphaTest: Boolean;
  var
    TextureNode: TAbstractTextureNode;
    FontTextureNode: TAbstractTexture2DNode;
    TexturesAlphaChannel: TAlphaChannel;
  begin
    TextureNode := State.MainTexture;
    FontTextureNode := OriginalGeometry.FontTextureNode;

    { This works also for TextureNode being TMultiTextureNode,
      since it has smartly calculated AlphaChannel based on children. }
    TexturesAlphaChannel := acNone;
    if TextureNode <> nil then
      AlphaMaxVar(TexturesAlphaChannel, TextureNode.AlphaChannelFinal);
    if FontTextureNode <> nil then
      AlphaMaxVar(TexturesAlphaChannel, FontTextureNode.AlphaChannelFinal);
    Result := TexturesAlphaChannel = acTest;
  end;

  { TODO: DetectAlphaBlending and DetectAlphaTest both look at alpha channel
    of textures. They should share some part of a single implementation. }

const
  AlphaModeToChannel: array [TAlphaMode] of TAutoAlphaChannel = (
    { amAuto -> } acAuto,
    { amOpaque -> } acNone,
    { amMask -> } acTest,
    { amBlend -> } acBlending
  );
begin
  { Check whether Appearance.alphaMode or alphaChannel field is set to something <> "AUTO".
    This is the simplest option, in which we don't need to run our "auto detection"
    below. }
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) and
     (State.ShapeNode.Appearance.AlphaMode <> amAuto) then
    Exit(AlphaModeToChannel[State.ShapeNode.Appearance.AlphaMode]);

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) and
     (State.ShapeNode.Appearance.AlphaChannel <> acAuto) then
    Exit(State.ShapeNode.Appearance.AlphaChannel);

  if DetectAlphaBlending then
    Exit(acBlending)
  else
  if DetectAlphaTest then
    Exit(acTest)
  else
    Exit(acNone);
end;

procedure TShape.TraverseCore(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  if ((not OnlyVisible) or Visible) and
     ((not OnlyCollidable) or Collidable) then
    Func(Self);
end;

procedure TShape.FastTransformUpdateCore(var AnythingChanged: Boolean;
  const ParentTransformation: TTransformation);
begin
  State.Transformation := ParentTransformation;

  // Changed(false, [chTransform]);
  // a bit faster:
  Validities := Validities - [svBBox, svBoundingSphere];

  AnythingChanged := true;
end;

function TShape.MaxShapesCountCore: Integer;
begin
  Result := 1;
end;

function TShape.Visible: boolean;
begin
  Result := State.InsideInvisible = 0;
end;

function TShape.Collidable: boolean;
begin
  Result := State.InsideIgnoreCollision = 0;
  if (Node <> nil) and
     (Node.Collision = scNone) then
    Result := false;
end;

function TShape.RayCollision(
  const Tag: TMailboxTag;
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
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
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Pos1, Pos2: TVector3;
  const ReturnClosestIntersection: boolean;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): PTriangle;
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

function TShape.NormalsSmooth(const FromCcw: boolean): TVector3List;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not (
       (svNormals in Validities) and
       (FNormalsCached = ncSmooth) and
       (FNormalsCachedCcw = FromCcw)
     ) then
  begin
    if LogShapes then
      WritelnLog('Normals', 'Calculating shape smooth normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry;
    S := State;

    FNormals := CreateSmoothNormalsCoordinateNode(G, S, FromCcw);
    FNormalsCached := ncSmooth;
    FNormalsCachedCcw := FromCcw;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.NormalsFlat(const FromCcw: boolean): TVector3List;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not (
       (svNormals in Validities) and
       (FNormalsCached = ncFlat) and
       (FNormalsCachedCcw = FromCcw)
     ) then
  begin
    if LogShapes then
      WritelnLog('Normals', 'Calculating shape flat normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry;
    S := State;

    FNormals := CreateFlatNormals(G.CoordIndexField.Items,
      G.InternalCoordinates(S).Items, FromCcw, G.Convex);
    FNormalsCached := ncFlat;
    FNormalsCachedCcw := FromCcw;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.NormalsCreaseAngle(const FromCcw: boolean;
  const CreaseAngle: Single): TVector3List;
var
  G: TAbstractGeometryNode;
  S: TX3DGraphTraverseState;
begin
  if not (
       (svNormals in Validities) and
       (FNormalsCached = ncCreaseAngle) and
       (FNormalsCachedCcw = FromCcw) and
       (FNormalsCreaseAngle = CreaseAngle)
     ) then
  begin
    if LogShapes then
      WritelnLog('Normals', 'Calculating shape CreaseAngle normals');

    { Free previous normals }
    FreeAndNil(FNormals);
    Exclude(Validities, svNormals);

    G := Geometry;
    S := State;

    FNormals := CreateNormals(G.CoordIndexField.Items,
      G.InternalCoordinates(S).Items, CreaseAngle, FromCcw, G.Convex);
    FNormalsCached := ncCreaseAngle;
    FNormalsCachedCcw := FromCcw;
    FNormalsCreaseAngle := CreaseAngle;
    Include(Validities, svNormals);
  end;

  Result := FNormals;
end;

function TShape.EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer;

  { Handle Tex being any TAbstractSingleTextureNode.
    Ignores Tex = nil. }
  function HandleSingleTextureNode(Tex: TX3DNode): Pointer;
  begin
    if Tex is TAbstractTextureNode then
      Result := Enumerate(Self, TAbstractTextureNode(Tex))
    else
      Result := nil;
  end;

  function HandleIDecls(IDecls: TX3DInterfaceDeclarationList): Pointer; overload; forward;

  function HandleIDecls(Nodes: TMFNode): Pointer; overload;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Nodes.Count - 1 do
    begin
      Result := HandleIDecls(Nodes[I].InterfaceDeclarations);
      if Result <> nil then Exit;
    end;
  end;

  function HandleIDecls(Nodes: TX3DNodeList): Pointer; overload;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Nodes.Count - 1 do
    begin
      Result := HandleIDecls(Nodes[I].InterfaceDeclarations);
      if Result <> nil then Exit;
    end;
  end;

  { Handle Tex being any TAbstractTextureNode.
    Ignores Tex = nil case. }
  function HandleTextureNode(Tex: TX3DNode): Pointer;
  var
    I: Integer;
  begin
    Result := nil;

    if Tex is TAbstractTextureNode then
    begin
      if Tex is TAbstractSingleTextureNode then
      begin
        { Texture node may use more texture nodes through it's "effects" field. }
        Result := HandleIDecls(TAbstractSingleTextureNode(Tex).FdEffects);
        if Result <> nil then Exit;
      end;

      if Tex is TMultiTextureNode then
      begin
        Result := Enumerate(Self, TMultiTextureNode(Tex));
        if Result <> nil then Exit;

        for I := 0 to TMultiTextureNode(Tex).FdTexture.Count - 1 do
        begin
          Result := HandleSingleTextureNode(TMultiTextureNode(Tex).FdTexture[I]);
          if Result <> nil then Exit;
        end;
      end else
        Result := HandleSingleTextureNode(Tex);
    end;
  end;

  procedure HandleEnvironmentLight(const EnvLight: TEnvironmentLightNode);
  begin
    Result := HandleSingleTextureNode(EnvLight.FdDiffuseTexture.Value);
    if Result <> nil then Exit;
    Result := HandleSingleTextureNode(EnvLight.FdSpecularTexture.Value);
    if Result <> nil then Exit;
  end;

  { Scan IDecls for SFNode and MFNode fields, handling texture nodes inside. }
  function HandleIDecls(IDecls: TX3DInterfaceDeclarationList): Pointer;
  var
    I, J: Integer;
    UniformField: TX3DField;
  begin
    Result := nil;
    if IDecls <> nil then
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
              Result := HandleTextureNode(TMFNode(UniformField)[J]);
              if Result <> nil then Exit;
            end;
          end;
        end;
      end;
  end;

  function HandleCommonSurfaceShader(SurfaceShader: TCommonSurfaceShaderNode): Pointer;
  begin
    Result := HandleTextureNode(SurfaceShader.FdAlphaTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdAmbientTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdDiffuseTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdDiffuseDisplacementTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdDisplacementTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdEmissiveTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdEnvironmentTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdMultiDiffuseAlphaTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdMultiEmmisiveAmbientIntensityTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdMultiSpecularShininessTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdMultiVisibilityTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdNormalTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdReflectionTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdShininessTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdSpecularTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(SurfaceShader.FdTransmissionTexture.Value);
    if Result <> nil then Exit;
  end;

  function HandleOneSidedMaterial(const Mat: TAbstractOneSidedMaterialNode): Pointer;
  begin
    Result := HandleTextureNode(Mat.FdEmissiveTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdNormalTexture.Value);
    if Result <> nil then Exit;
  end;

  function HandleMaterial(const Mat: TMaterialNode): Pointer;
  begin
    Result := HandleTextureNode(Mat.FdAmbientTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdDiffuseTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdShininessTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdSpecularTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdOcclusionTexture.Value);
    if Result <> nil then Exit;
  end;

  function HandlePhysicalMaterial(const Mat: TPhysicalMaterialNode): Pointer;
  begin
    Result := HandleTextureNode(Mat.FdBaseTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdMetallicRoughnessTexture.Value);
    if Result <> nil then Exit;
    Result := HandleTextureNode(Mat.FdOcclusionTexture.Value);
    if Result <> nil then Exit;
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
  I: Integer;
  App: TAppearanceNode;
  Lights: TLightInstancesList;
begin
  Result := HandleTextureNode(State.VRML1State.Texture2);
  if Result <> nil then Exit;

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    App := State.ShapeNode.Appearance;

    if App.FdMaterial.Value is TAbstractOneSidedMaterialNode then
    begin
      Result := HandleOneSidedMaterial(TAbstractOneSidedMaterialNode(App.FdMaterial.Value));
      if Result <> nil then Exit;
    end;

    if App.FdMaterial.Value is TMaterialNode then
    begin
      Result := HandleMaterial(TMaterialNode(App.FdMaterial.Value));
      if Result <> nil then Exit;
    end;

    if App.FdMaterial.Value is TPhysicalMaterialNode then
    begin
      Result := HandlePhysicalMaterial(TPhysicalMaterialNode(App.FdMaterial.Value));
      if Result <> nil then Exit;
    end;

    Result := HandleTextureNode(App.FdTexture.Value);
    if Result <> nil then Exit;

    Result := HandleTextureNode(App.FdNormalMap.Value);
    if Result <> nil then Exit;

    HandleIDecls(App.FdShaders);
    HandleIDecls(App.FdEffects);

    { CommonSurfaceShader can be non-nil only when App is non-nil }
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := State.ShapeNode.CommonSurfaceShader;
    {$warnings on}
    if SurfaceShader <> nil then
    begin
      HandleCommonSurfaceShader(SurfaceShader);
      if Result <> nil then Exit;
    end;
  end;

  Lights := State.Lights;
  if Lights <> nil then
    for I := 0 to Lights.Count - 1 do
    begin
      if Lights.List^[I].Node is TEnvironmentLightNode then
      begin
        HandleEnvironmentLight(TEnvironmentLightNode(Lights.List^[I].Node));
        if Result <> nil then Exit;
      end;

      Result := HandleIDecls(Lights.List^[I].Node.FdEffects);
      if Result <> nil then Exit;
    end;

  if State.Effects <> nil then
    HandleIDecls(State.Effects);

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
    Result := EnumerateTextures(
      {$ifdef FPC}@{$endif}Helper.HandleTexture) <> nil;
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

procedure TShape.ValidateGeometryState;
begin
  if FGeometry = nil then
  begin
    Assert(FState = nil);
    FState := OriginalState;

    try
      FGeometry := OriginalGeometry.Proxy(FState);
      if FGeometry <> nil then
        AssociateProxyGeometryState;
    except
      { in case of trouble, remember to keep both
        FGeometry and FState nil.
        Never let one of them be nil, while other it not. }
      FState := nil;
      raise;
    end;

    if FGeometry = nil then
    begin
      FGeometry := OriginalGeometry;
      FState    := OriginalState;
    end;
  end;
end;

function TShape.Geometry: TAbstractGeometryNode;
begin
  ValidateGeometryState;
  Result := FGeometry;
end;

function TShape.State: TX3DGraphTraverseState;
begin
  ValidateGeometryState;
  Result := FState;
end;

function TShape.ProxyGeometry: TAbstractGeometryNode;
begin
  Result := Geometry;
  if Result = OriginalGeometry then Result := nil;
end;

function TShape.ProxyState: TX3DGraphTraverseState;
begin
  if Geometry <> OriginalGeometry then
    Result := State else
    Result := nil;
end;

procedure TShape.LocalTriangulate(const TriangleEvent: TTriangleEvent);
var
  Arrays: TGeometryArrays;
  RangeBeginIndex: Integer;

  { Call TriangleEvent once. Give indexes to Arrays (Arrays.Indexes,
    if assigned, otherwise direct coordinates), relative to RangeBeginIndex. }
  procedure Triangle(const I1, I2, I3: Cardinal);
  var
    VI1, VI2, VI3: Integer;
    Position, Normal: TTriangle3;
    TexCoord: TTriangle4;
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
    Position.Data[0] := Arrays.Position(VI1)^;
    Position.Data[1] := Arrays.Position(VI2)^;
    Position.Data[2] := Arrays.Position(VI3)^;
    Normal.Data[0] := Arrays.Normal(VI1)^;
    Normal.Data[1] := Arrays.Normal(VI2)^;
    Normal.Data[2] := Arrays.Normal(VI3)^;

    if (Arrays.TexCoords.Count <> 0) and
       (Arrays.TexCoords[0] <> nil) and
       (Arrays.TexCoords[0].Generation = tgExplicit) then
    begin
      case Arrays.TexCoords[0].Dimensions of
        2: begin
             TexCoord.Data[0] := Vector4(Arrays.TexCoord2D(0, VI1)^, 0, 1);
             TexCoord.Data[1] := Vector4(Arrays.TexCoord2D(0, VI2)^, 0, 1);
             TexCoord.Data[2] := Vector4(Arrays.TexCoord2D(0, VI3)^, 0, 1);
           end;
        3: begin
             TexCoord.Data[0] := Vector4(Arrays.TexCoord3D(0, VI1)^, 1);
             TexCoord.Data[1] := Vector4(Arrays.TexCoord3D(0, VI2)^, 1);
             TexCoord.Data[2] := Vector4(Arrays.TexCoord3D(0, VI3)^, 1);
           end;
        4: begin
             TexCoord.Data[0] := Arrays.TexCoord4D(0, VI1)^;
             TexCoord.Data[1] := Arrays.TexCoord4D(0, VI2)^;
             TexCoord.Data[2] := Arrays.TexCoord4D(0, VI3)^;
           end;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Arrays.TexCoord[0].Dimensions? at TShape.localtriangulate');
        {$endif}
      end;
    end else
      TexCoord := UnknownTexCoord;

    if Arrays.Faces <> nil then
      Face := Arrays.Faces.List^[RangeBeginIndex + I1]
    else
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
            Inc(I, 3);
          end;
        end;
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
  Arrays := GeometryArrays;
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
        RangeBeginIndex := RangeBeginIndex + Arrays.Counts[I];
      end;
  finally FreeAndNil(Arrays) end;
end;

type
  TTriangulateRedirect = class
    Transform: PMatrix4;
    TriangleEvent: TTriangleEvent;
    procedure LocalNewTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  end;

procedure TTriangulateRedirect.LocalNewTriangle(Shape: TObject;
  const Position: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  TriangleEvent(Shape, Position.Transform(Transform^), Normal, TexCoord, Face);
end;

procedure TShape.Triangulate(const TriangleEvent: TTriangleEvent);
var
  TR: TTriangulateRedirect;
begin
  TR := TTriangulateRedirect.Create;
  try
    TR.Transform := @(State.Transformation.Transform);
    TR.TriangleEvent := TriangleEvent;
    LocalTriangulate({$ifdef FPC}@{$endif}TR.LocalNewTriangle);
  finally FreeAndNil(TR) end;
end;

function TShape.DebugInfo(const Indent: string): string;
begin
  Result := Indent + NiceName + NL;
end;

function TShape.NiceName: string;
begin
  Result := OriginalGeometry.NiceName;

  { Slash / seems like a nice way to show it, since it is like a path, just in X3D tree. }

  if FGeometryParentNode <> nil then
    Result := FGeometryParentNode.X3DName + '/' + Result;

  if FGeometryGrandParentNode <> nil then
    Result := FGeometryGrandParentNode.X3DName + '/' + Result;

  if FGeometryGrandGrandParentNode <> nil then
    Result := FGeometryGrandGrandParentNode.X3DName + '/' + Result;
end;

function TShape.Node: TAbstractShapeNode;
begin
  Result := State.ShapeNode;
end;

function TShape.MaterialProperty: TMaterialProperty;
begin
  Result := InternalMaterialProperty;
end;

procedure TShape.InternalBeforeChange;
begin
  FreeProxy;
  UnAssociateGeometryState(FOriginalGeometry, FOriginalState);
  UnAssociateGeometryStateNeverProxied(FOriginalGeometry, FOriginalState);
end;

procedure TShape.InternalAfterChange;
begin
  AssociateGeometryState(FOriginalGeometry, FOriginalState);
  AssociateGeometryStateNeverProxied(FOriginalGeometry, FOriginalState);
end;

function TShape.InternalMaterialProperty: TMaterialProperty;
var
  TextureUrl: string;
begin
  if IsCachedMaterialProperty then
    Exit(CachedMaterialProperty);

  Result := nil;

  if Node <> nil then
  begin
    { VRML 2.0/X3D version: refer to TAppearanceNode.MaterialProperty }
    if Node.Appearance <> nil then
      Result := Node.Appearance.InternalMaterialProperty;
  end else
  begin
    { VRML 1.0 version: calculate it directly here }
    TextureUrl := State.VRML1State.Texture2.FdFileName.Value;
    if TextureUrl <> '' then
      Result := MaterialProperties.FindTextureBaseName(
        DeleteURIExt(ExtractURIName(TextureUrl)));
  end;

  IsCachedMaterialProperty := true;
  CachedMaterialProperty := Result;
end;

{ TShapeTreeGroup -------------------------------------------------------- }

constructor TShapeTreeGroup.Create(const AParentScene: TX3DEventsEngine);
begin
  inherited;
  FChildren := TShapeTreeList.Create(true);
  FChildren.OnNotify := {$ifdef FPC}@{$endif} ChildrenChanged;
end;

destructor TShapeTreeGroup.Destroy;
begin
  FreeAndNil(FChildren);
  inherited;
end;

procedure TShapeTreeGroup.ChildrenChanged(Sender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Item: TShapeTree;
  Action: TCollectionNotification);
begin
  if Action = cnAdded then
    Item.FParent := Self;
  if Action in [cnExtracted, cnRemoved] then
    Item.FParent := nil;
  InvalidateMaxShapesCount;
  if ParentScene <> nil then
    TCastleSceneCore(ParentScene).InternalIncShapesHash;
end;

procedure TShapeTreeGroup.TraverseCore(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].Traverse(Func, OnlyActive, OnlyVisible, OnlyCollidable);
end;

procedure TShapeTreeGroup.FastTransformUpdateCore(var AnythingChanged: Boolean;
  const ParentTransformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    FChildren.Items[I].FastTransformUpdateCore(AnythingChanged, ParentTransformation);
end;

function TShapeTreeGroup.MaxShapesCountCore: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do
    Result := Result + FChildren.Items[I].MaxShapesCount;
end;

function TShapeTreeGroup.EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer;
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
    Result := Result + FChildren[I].DebugInfo(Indent + Format('  %3d:', [I]));
end;

{ TShapeTreeSwitch ------------------------------------------------------- }

procedure TShapeTreeSwitch.TraverseCore(const Func: TShapeTraverseFunc;
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

constructor TShapeTreeTransform.Create(const AParentScene: TX3DEventsEngine);
begin
  inherited;
  FTransformState := TX3DGraphTraverseState.Create;
end;

destructor TShapeTreeTransform.Destroy;
begin
  if FTransformFunctionality <> nil then
    UnAssociateNode(FTransformFunctionality.Parent);
  FreeAndNil(FTransformState);
  inherited;
end;

procedure TShapeTreeTransform.SetTransformFunctionality(const Value: TTransformFunctionality);
begin
  if FTransformFunctionality <> Value then
  begin
    if FTransformFunctionality <> nil then
      UnAssociateNode(FTransformFunctionality.Parent);
    FTransformFunctionality := Value;
    if FTransformFunctionality <> nil then
      AssociateNode(FTransformFunctionality.Parent);
  end;
end;

procedure TShapeTreeTransform.FastTransformUpdate(var AnythingChanged: Boolean);
begin
  FastTransformUpdateCore(AnythingChanged, FTransformState.Transformation);
end;

procedure TShapeTreeTransform.FastTransformUpdateCore(var AnythingChanged: Boolean;
  const ParentTransformation: TTransformation);
var
  NewTransformation: TTransformation;
begin
  NewTransformation := ParentTransformation;

  { Keep FTransformState up-to-date.
    This is not necessary when OptimizeExtensiveTransformations = true,
    so we don't do it to conserve speed. }
  if not OptimizeExtensiveTransformations then
  begin
    FTransformState.Transformation := ParentTransformation;
  end;

  TransformFunctionality.ApplyTransform(NewTransformation);

  inherited FastTransformUpdateCore(AnythingChanged, NewTransformation);
end;

function TShapeTreeTransform.TransformNode: TX3DNode;
begin
  Result := TransformFunctionality.Parent;
end;

function TShapeTreeTransform.DebugInfo(const Indent: string): string;
var
  I: Integer;
  TransformNodeName: String;
begin
  if TransformFunctionality <> nil then
    TransformNodeName := TransformFunctionality.Parent.NiceName
  else
    TransformNodeName := 'nil';

  Result := Indent + ClassName + ' (' + TransformNodeName + ')' + NL;
  for I := 0 to Children.Count - 1 do
    Result := Result + Children[I].DebugInfo(Indent + Format('  %3d:', [I]));
end;

{ TShapeTreeLOD ------------------------------------------------------- }

function TShapeTreeLOD.LODInverseTransform: PMatrix4;
begin
  Result := @FLODInverseTransform;
end;

function TShapeTreeLOD.CalculateLevel(const CameraPosition: TVector3): Cardinal;
var
  Camera: TVector3;
  Dummy: Single;
begin
  if (Children.Count = 0) or
     (LODNode.FdRange.Count = 0) then
    Result := 0 else
  begin
    try
      Camera := LODInverseTransform^.MultPoint(CameraPosition);
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
          [ CameraPosition.ToRawString, E.Message ]));
        Result := 0;
      end;
    end;
  end;

  Assert(
    ( (Children.Count = 0) and (Result = 0) ) or
    ( (Children.Count > 0) and (Result < Cardinal(Children.Count)) ) );
end;

procedure TShapeTreeLOD.TraverseCore(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  if Children.Count > 0 then
  begin
    if OnlyActive then
      { Now we know that Level < Children.Count, no need to check it. }
      Children.Items[Level].Traverse(Func, OnlyActive, OnlyVisible, OnlyCollidable)
    else
      inherited;
  end;
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

procedure TProximitySensorInstance.TraverseCore(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  { Nothing to do: no geometry shapes, no children here }
end;

function TProximitySensorInstance.MaxShapesCountCore: Integer;
begin
  { This is not a TShape instance, and has no TShape children. }
  Result := 0;
end;

procedure TProximitySensorInstance.FastTransformUpdateCore(var AnythingChanged: Boolean;
  const ParentTransformation: TTransformation);
begin
  { Nothing to do: This is not a TShape instance, and has no TShape children. }
end;

function TProximitySensorInstance.EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer;
begin
  { Nothing to do: no geometry shapes, no children here }
  Result := nil;
end;

function TProximitySensorInstance.DebugInfo(const Indent: string = ''): string;
begin
  Result := Indent + 'ProximitySensor (' + Node.X3DName + ')' + NL;
end;

{ TVisibilitySensorInstance ---------------------------------------------- }

procedure TVisibilitySensorInstance.TraverseCore(const Func: TShapeTraverseFunc;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  { Nothing to do: no geometry shapes, no children here }
end;

procedure TVisibilitySensorInstance.FastTransformUpdateCore(var AnythingChanged: Boolean;
  const ParentTransformation: TTransformation);
begin
  { Nothing to do: This is not a TShape instance, and has no TShape children. }
end;

function TVisibilitySensorInstance.MaxShapesCountCore: Integer;
begin
  { This is not a TShape instance, and has no TShape children. }
  Result := 0;
end;

function TVisibilitySensorInstance.EnumerateTextures(const Enumerate: TEnumerateShapeTexturesFunction): Pointer;
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

  Later: We also improved the unsophisticated performance using CachedChildrenList.

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

constructor TShapeTreeIterator.Create(const Tree: TShapeTree;
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

constructor TShapeTreeIterator.Create(const Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  inherited Create;
  List := Tree.TraverseList(OnlyActive, OnlyVisible, OnlyCollidable);
  CurrentIndex := -1;
end;

destructor TShapeTreeIterator.Destroy;
begin
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

constructor TShapeList.Create(const Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean);
begin
  Create;
  { Set Capacity, to make following operations faster.
    Note that we use MaxShapesCount instead of ShapesCount,
    since MaxShapesCount is usually instant.
    Testcase of speedup: e.g. profiling animate_3d_model_by_code_2. }
  Capacity := Tree.MaxShapesCount;
  { This method uses Tree.Traverse that uses Tree.TraverseList that creates a list,
    iterates over it, and here we add results to another list...
    This is clearly a waste of time. That's why this method is deprecated. }
  Tree.Traverse({$ifdef FPC}@{$endif} AddConst, OnlyActive, OnlyVisible, OnlyCollidable);
end;

procedure TShapeList.AddConst(const S: TShape);
begin
  Add(S);
end;

type
  TShapeComparer = {$ifdef FPC}specialize{$endif} TComparer<TShape>;

function TShapeList.IsSmallerFrontToBack(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
begin
  { To revert the order, we revert the order of A and B as passed to CompareBackToFront3D. }
  Result := TBox3D.CompareBackToFront3D(B.BoundingBox, A.BoundingBox, SortPosition);
end;

function TShapeList.IsSmallerBackToFront2D(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(A.BoundingBox, B.BoundingBox);
end;

function TShapeList.IsSmallerBackToFront3DBox(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(A.BoundingBox, B.BoundingBox, SortPosition);
end;

function TShapeList.IsSmallerBackToFront3DOrigin(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := A.OriginalState.Transformation.Transform.MultPoint(TVector3.Zero);
  PointB := B.OriginalState.Transformation.Transform.MultPoint(TVector3.Zero);
  Result := Sign(
    PointsDistanceSqr(PointB, SortPosition) -
    PointsDistanceSqr(PointA, SortPosition));
end;

function TShapeList.IsSmallerBackToFront3DGround(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TShape): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := A.OriginalState.Transformation.Transform.MultPoint(TVector3.Zero);
  PointB := B.OriginalState.Transformation.Transform.MultPoint(TVector3.Zero);
  PointA.Y := 0;
  PointB.Y := 0;
  Result := Sign(
    PointsDistanceSqr(PointB, SortPosition) -
    PointsDistanceSqr(PointA, SortPosition));
end;

procedure TShapeList.SortFrontToBack(const Position: TVector3);
begin
  SortPosition := Position;
  Sort(TShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerFrontToBack));
end;

procedure TShapeList.SortBackToFront(const Position: TVector3;
  const BlendingSort: TBlendingSort);
begin
  SortPosition := Position;
  case BlendingSort of
    bs2D      : Sort(TShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront2D));
    bs3D      : Sort(TShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DBox));
    bs3DOrigin: Sort(TShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DOrigin));
    bs3DGround: Sort(TShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DGround));
    else ;
  end;
end;

{ TPlaceholderNames ------------------------------------------------------- }

function TPlaceholderNames.GetItems(const AKey: string): TPlaceholderName;
begin
  Result := inherited Items[AKey];
end;

procedure TPlaceholderNames.SetItems(const AKey: string; const AValue: TPlaceholderName);
begin
  AddOrSetValue(AKey, AValue);
end;

function X3DShapePlaceholder(const Shape: TShape): string;
begin
  { Shape.Node may be nil for old VRML 1.0 or Inventor. }
  if Shape.Node <> nil then
    Result := Shape.Node.X3DName
  else
    Result := '';
end;

function BlenderPlaceholder(const Shape: TShape): string;
begin
  Result := '';
  if Shape.OriginalGeometry is TAbstractGeometryNode_1 then
  begin
    { Geometry node generated by Blender VRML 1.0 exporter has one parent,
      its mesh. The mesh node may have many parents representing its objects
      (unfortunately, the object names are not recorded in exported file,
      so we use mesh name for BlenderPlaceholder. }
    if Shape.GeometryParentNode <> nil then
      Result := Shape.GeometryParentNode.X3DName;
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
      we don't handle them.

      This also works for glTF exporter in Blender 2.82a,
      imported to X3D nodes by CGE.
      In this case, the name comes from Blender mesh name (not Blender object name),
      and has no prefixes/suffixes.
      It is still in GeometryGrandGrandParentNode.X3DName, because of how
      X3DLoadInternalGltf organizes data.
    }

    // not needed:
    // if Shape.GeometryGrandParentNode <> nil then
    //   BlenderMeshName := PrefixRemove('ME_', GeometryGrandParentNode.X3DName, false);

    if Shape.GeometryGrandGrandParentNode <> nil then
      Result := SuffixRemove('_ifs_TRANSFORM', PrefixRemove('OB_',
        Shape.GeometryGrandGrandParentNode.X3DName, false), false);
  end;
end;

initialization
  PlaceholderNames := TPlaceholderNames.Create;
  PlaceholderNames['x3dshape'] := @X3DShapePlaceholder;
  PlaceholderNames['blender'] := @BlenderPlaceholder;
finalization
  FreeAndNil(PlaceholderNames);
end.
