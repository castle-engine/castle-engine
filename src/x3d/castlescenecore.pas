{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3D scenes (TCastleSceneCore). }

unit CastleSceneCore;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3D,
  X3DFields, X3DNodes, CastleClassUtils, CastleUtils,
  CastleShapes, TriangleOctree, ProgressUnit, CastleOctree, ShapeOctree,
  KeysMouse, X3DTime, Cameras, Triangle, Contnrs,
  RenderingCameraUnit, Base3D, X3DShadowMaps, FGL, GenericStructList;

const
  DefaultShadowMapsDefaultSize = 256;

type
  TTriangle3SingleList = specialize TGenericStructList<TTriangle3Single>;

  { Internal helper type for TCastleSceneCore.
    @exclude }
  TSceneValidity = (fvBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvTrianglesListShadowCasters,
    fvManifoldAndBorderEdges,
    fvMainLightForShadows,
    fvShapesActiveCount,
    fvShapesActiveVisibleCount);

  { @exclude }
  TSceneValidities = set of TSceneValidity;

  { Scene edge that is between exactly two triangles.
    It's used by @link(TCastleSceneCore.ManifoldEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TManifoldEdge = record
    { Index to get vertexes of this edge.
      The actual edge's vertexes are not recorded here (this would prevent
      using TCastleSceneCore.ShareManifoldAndBorderEdges with various scenes from
      the same animation). You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the first triangle
      (i.e. Triangles[0]). }
    VertexIndex: Cardinal;

    { Indexes to TCastleSceneCore.TrianglesListShadowCasters array }
    Triangles: array [0..1] of Cardinal;

    { These are vertexes at VertexIndex and (VertexIndex+1)mod 3 positions,
      but @italic(only at generation of manifold edges time).
      Like said in VertexIndex, keeping here actual vertex info would prevent
      TCastleSceneCore.ShareManifoldAndBorderEdges. However, using these when generating
      makes a great speed-up when generating manifold edges.

      Memory cost is acceptable: assume we have model with 10 000 faces,
      so 15 000 edges (assuming it's correctly closed manifold), so we waste
      15 000 * 2 * SizeOf(TVector3Single) = 360 000 bytes... that's really nothing
      to worry (we waste much more on other things).

      Checked with "The Castle": indeed, this costs about 1 MB memory
      (out of 218 MB...), with really lot of creatures... On the other hand,
      this causes small speed-up when loading: loading creatures is about
      5 seconds faster (18 with, 23 without this).

      So memory loss is small, speed gain is noticeable (but still small),
      implementation code is a little simplified, so I'm keeping this.
      Also, in the future, maybe it will be sensible
      to use this for actual shadow quad rendering, in cases when we know that
      TCastleSceneCore.ShareManifoldAndBorderEdges was not used to make it. }
    V0, V1: TVector3Single;
  end;
  PManifoldEdge = ^TManifoldEdge;

  TManifoldEdgeList = specialize TGenericStructList<TManifoldEdge>;

  { Scene edge that has one neighbor, i.e. border edge.
    It's used by @link(TCastleSceneCore.BorderEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TBorderEdge = record
    { Index to get vertex of this edge.
      The actual edge's vertexes are not recorded here (this would prevent
      using TCastleSceneCore.ShareManifoldAndBorderEdges with various scenes from
      the same animation). You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the triangle TriangleIndex. }
    VertexIndex: Cardinal;

    { Index to TCastleSceneCore.TrianglesListShadowCasters array. }
    TriangleIndex: Cardinal;
  end;
  PBorderEdge = ^TBorderEdge;

  TBorderEdgeList = specialize TGenericStructList<TBorderEdge>;

  { These are various features that may be freed by
    TCastleSceneCore.FreeResources.

    @italic(Warning): This is for experienced usage of TCastleSceneCore.
    Everything is explained in detail below, but still  --- if you have some
    doubts, or you just don't observe any memory shortage in your program,
    it's probably best to not use TCastleSceneCore.FreeResources.

    @unorderedList(
      @item(For frTextureDataInNodes, frBackgroundImageInNodes and TrianglesList,
        if you will free them unnecessarily
        (i.e. you will use it after you freed it), it will be automatically
        recreated on next use. So everything will work correctly, but you
        will experience unnecessary slowdown if we will need to recreate
        exactly the same resource over and over again.)

      @item(For frTextureDataInNodes and frBackgroundImageInNodes note that
        freeing these resources too eagerly may make texture cache
        (see TextureImages) less effective. In normal circumstances,
        if you will use the same cache instance throughout the program,
        loaded images are reused. If you free frTextureDataInNodes
        too early, you may remove them from the cache too early, and lose
        a chance to reuse them. So you may cause unnecessary slowdown
        of preparing models, e.g. inside PrepareResources.)
    )
  }
  TSceneFreeResource = (
    { Unloads the texture images/videos allocated in texture nodes.

      It's useful if you know that you already prepared everything
      that needed the texture images, and you will not need texture images
      later. For TCastleScene this means that you use Optimization
      method other than roNone,
      and you already did PrepareResources (so textures are already loaded to OpenGL),
      and your code will not access TextureImage / TextureVideo anymore.
      This is commonly @true for various games.

      Then you can call this to free some resources.

      Note that if you made an accident and you will use some TextureImage or
      TextureVideo after FreeResources, then you will get no crash,
      but texture image will be simply reloaded. So you may experience
      slowdown if you inappropriately use this feature. }
    frTextureDataInNodes,

    { Unloads the background images allocated in VRML Background nodes.
      The same comments as for frTextureDataInNodes apply. }
    frBackgroundImageInNodes,

    { Free triangle list created by TrianglesListShadowCasters call.
      This list is also implicitly created by ManifoldEdges or BorderEdges. }
    frTrianglesListShadowCasters,

    { Free edges lists in ManifoldEdges and BorderEdges.

      Frees memory, but next call to ManifoldEdges and BorderEdges will
      need to calculate them again (or you will need to call
      TCastleSceneCore.ShareManifoldAndBorderEdges again).
      Note that using this scene as shadow caster for shadow volumes algorithm
      requires ManifoldEdges and BorderEdges. }
    frManifoldAndBorderEdges);

  TSceneFreeResources = set of TSceneFreeResource;

  TCastleSceneCore = class;

  TSceneNotification = procedure (Scene: TCastleSceneCore) of object;

  { Callback for TCastleSceneCore.OnGeometryChanged.

    SomeLocalGeometryChanged means that octree, triangles, bounding volumes
    local to some shape changed (not just e.g. shape transformation).

    OnlyShapeChanged is meaningful when SomeLocalGeometryChanged = @true.
    If nil, it indicates that only the given shape geometry changed.
    If not nil, assume that every shape's geometry potentially changed. }
  TSceneGeometryChanged = procedure (Scene: TCastleSceneCore;
    const SomeLocalGeometryChanged: boolean;
    OnlyShapeChanged: TShape) of object;

  { Stack of bindable nodes (only the top, bound, node is used for rendering/navigation).
    This keeps a stack of TAbstractBindableNode, with comfortable routines
    to examine top and push/pop from top. The stack is actually stored
    as a list, with the last item being the top one. }
  TX3DBindableStack = class(TX3DBindableStackBasic)
  private
    FParentScene: TCastleSceneCore;
    FOnBoundChanged: TSceneNotification;
    BoundChangedSchedule: Cardinal;
    BoundChangedScheduled: boolean;

    { A useful utility: if the Node is not @nil, send isBound = Value and
      bindTime events to it. }
    procedure SendIsBound(Node: TAbstractBindableNode; const Value: boolean);

    { Add new node to the top.

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged / DoScheduleBoundChanged. }
    procedure Push(Node: TAbstractBindableNode);

    { Remove current top node. Returns removed node, or @nil if no current
      node was present (that is, stack was empty).

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged / DoScheduleBoundChanged. }
    function Pop: TAbstractBindableNode;

    { When we're inside BeginChangesSchedule / EndChangesSchedule,
      then DoScheduleBoundChanged will not immediately call DoBoundChanged.
      Effectively, you can wrap a part of code inside
      BeginChangesSchedule / EndChangesSchedule, to delay calling
      DoBoundChanged (and so OnBoundChanged callback). }
    procedure BeginChangesSchedule;
    procedure EndChangesSchedule;
  protected
    { Notification when the currently bound node, that is
      @link(Top), changed.
      In this class, just calls OnBoundChanged if assigned. }
    procedure DoBoundChanged; virtual;

    { Call DoBoundChanged at the nearest comfortable time.
      Either now (when not inside BeginChangesSchedule / EndChangesSchedule)
      or at a closing EndChangesSchedule. }
    procedure DoScheduleBoundChanged;
  public
    constructor Create(AParentScene: TCastleSceneCore);

    property ParentScene: TCastleSceneCore read FParentScene;

    { Returns top item on this stack, or @nil if not present. }
    function Top: TAbstractBindableNode;

    { Add new node to the top, but only if stack is currently empty.
      If SendEvents, then isBound = true and bindTime events will be
      send to newly bound node. }
    procedure PushIfEmpty(Node: TAbstractBindableNode; SendEvents: boolean);

    { Use when you suspect that some nodes on the stack
      are no longer present in VRML/X3D graph RootNode (they were deleted).
      In this case, they have to be removed from stack.

      If this will change the currently bound node, then the new bound
      node will receive isBound = true and bindTime events (the old node
      will not  receive any set_bind = false or isBound = false events, since it
      may be destroyed by now). }
    procedure CheckForDeletedNodes(const RootNode: TX3DRootNode;
      const SendEvents: boolean);

    { Handle set_bind event send to given Node.
      This always generates appropriate events. }
    procedure Set_Bind(Node: TAbstractBindableNode; const Value: boolean); override;

    { Notification when the currently bound node, that is
      @link(Top), changed. This also includes notification
      when @link(Top) changed to (or from) @nil, that is
      when no node becomes bound or when some node is initially bound. }
    property OnBoundChanged: TSceneNotification
      read FOnBoundChanged write FOnBoundChanged;
  end;

  TViewpointStack = class(TX3DBindableStack)
  protected
    procedure DoBoundChanged; override;
  public
    function Top: TAbstractViewpointNode;
  end;

  TNavigationInfoStack = class(TX3DBindableStack)
  protected
    procedure DoBoundChanged; override;
  public
    function Top: TNavigationInfoNode;
  end;

  { @exclude }
  TGeneratedTexture = record
    { May be only TGeneratedCubeMapTextureNode or TRenderedTextureNode
      or TGeneratedShadowMapNode. }
    TextureNode: TAbstractTextureNode;
    Handler: TGeneratedTextureHandler;
    Shape: TShape;
  end;
  PGeneratedTexture = ^TGeneratedTexture;

  { @exclude
    Internal for TCastleSceneCore: list of generated textures
    (GeneratedCubeMapTexture, RenderedTexture and similar nodes)
    along with their shape. }
  TGeneratedTextureList = class(specialize TGenericStructList<TGeneratedTexture>)
  public
    function IndexOfTextureNode(TextureNode: TX3DNode): Integer;
    function FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
    procedure AddShapeTexture(Shape: TShape; Tex: TAbstractTextureNode);
    procedure UpdateShadowMaps(LightNode: TAbstractLightNode);
  end;

  { List of transform nodes (ITransformNode),
    used to extract TShapeTreeList for this node. }
  TTransformInstancesList = class(TX3DNodeList)
  public
    { Returns existing TShapeTreeList corresponding to given Node.
      If not found, and AutoCreate, then creates new.
      If not found, and not AutoCreate, then return @nil. }
    function Instances(Node: TX3DNode;
      const AutoCreate: boolean): TShapeTreeList;
    procedure FreeShapeTrees;
  end;

  { @exclude }
  TProximitySensorInstanceList = specialize TFPGObjectList<TProximitySensorInstance>;
  { @exclude }
  TVisibilitySensorInstanceList = specialize TFPGObjectList<TVisibilitySensorInstance>;
  { @exclude }
  TVisibilitySensors = class(specialize TGenericStructMap<TVisibilitySensorNode, TVisibilitySensorInstanceList>)
  public
    destructor Destroy; override;
    { Remove everything are released owned stuff.
      We own TVisibilitySensorInstanceList instances on our Data list.
      We do not own TVisibilitySensorNode (our Keys list). }
    procedure Clear;
  end;
  TTimeDependentHandlerList = class(specialize TFPGObjectList<TTimeDependentNodeHandler>)
    procedure AddIfNotExists(const Item: TTimeDependentNodeHandler);
  end;

  TCompiledScriptHandler = procedure (
    Value: TX3DField; const Time: TX3DTime) of object;

  { @exclude }
  TCompiledScriptHandlerInfo = record
    Handler: TCompiledScriptHandler;
    Name: string;
  end;
  PCompiledScriptHandlerInfo = ^TCompiledScriptHandlerInfo;
  TCompiledScriptHandlerInfoList = specialize TGenericStructList<TCompiledScriptHandlerInfo>;

  { Possible spatial structure types that may be managed by TCastleSceneCore,
    see TCastleSceneCore.Spatial. }
  TSceneSpatialStructure = (
    { Create and keep current the TCastleSceneCore.OctreeRendering.
      This is a dynamic octree containing all visible shapes. }
    ssRendering,

    { Create and keep current the TCastleSceneCore.OctreeDynamicCollisions.
      This is a dynamic octree containing all collidable items. }
    ssDynamicCollisions,

    { Create the TCastleSceneCore.OctreeVisibleTriangles.
      This is an octree containing all visible triangles, suitable only
      for scenes that stay static. }
    ssVisibleTriangles,

    { Create the TCastleSceneCore.OctreeCollidableTriangles.
      This is an octree containing all collidable triangles, suitable only
      for scenes that stay static. }
    ssCollidableTriangles);
  TSceneSpatialStructures = set of TSceneSpatialStructure;

  { Triangles array for shadow casting object.

    This guarantees that the whole array has first OpaqueCount opaque triangles,
    then the rest is transparent.
    The precise definition between "opaque"
    and "transparent" is done by TShape.Transparent.
    This is also used by OpenGL rendering to determine which shapes
    need blending.

    This separation into opaque and transparent parts
    (with OpaqueCount marking the border) is useful for shadow volumes
    algorithm, that must treat transparent shadow casters a little
    differently. }
  TTrianglesShadowCastersList = class(TTriangle3SingleList)
  private
    FOpaqueCount: Cardinal;
  public
    { Numer of opaque triangles on this list. Opaque triangles
      are guarenteed to be placed before all transparent triangles
      on this list. }
    property OpaqueCount: Cardinal read FOpaqueCount;
  end;

  TGeometryChange =
  ( { Everything changed. All octrees must be rebuild, old State pointers
      may be invalid.

      Every ChangedAll call does this.
      ChangedAll must take into account that everything could change.
      Note that ChangedAll traverses the VRML graph again,
      recalculating State values... so the old States are not
      correct anymore. You have to rebuild the octree or your pointers
      will be bad.

      When DoGeometryChanged with gcAll is called, we know that ChangedAll
      called this, and every TShape will be (or already is) destroyed
      and created new. }
    gcAll,

    { Transformation of some shape changed.
      @groupBegin }
    gcCollidableTransformChanged,
    gcVisibleTransformChanged,
    { @groupEnd }

    { Local geometry change
      happened (actual octree free is already done by
      TShape.LocalGeometryChanged, octree create will be done at next demand).
      We should update stuff at higher (TCastleSceneCore) level accordingly.

      gcLocalGeometryChangedCoord means that coordinates changed.
      Compared to gcLocalGeometryChanged, this means that model edges
      structure remains the same (this is helpful e.g. to avoid
      recalculating Manifold/BorderEdges in parent scene).

      In this case, DoGeometryChanged parameter LocalGeometryShape is non-nil
      and indicated the (only) shape that changed.

      @groupBegin }
    gcLocalGeometryChanged,
    gcLocalGeometryChangedCoord,
    { @groupEnd }

    { What is considered "active" shapes changed. Like after Switch.whichChoice
      change. }
    gcActiveShapesChanged);

  { 3D scene, 3D model handling (except rendering, for which see TCastleScene).
    Provides a lot of useful functionality. Simple loading of the scene (@link(Load)
    method), calculating various things (like @link(BoundingBox) method).

    The @link(Shapes) tree provides a simple processed scene information,
    alternative to traversing the complicated VRML/X3D nodes graph.
    The basic idea is to have at the same time full VRML/X3D graph
    of the scene (in @link(RootNode)) and a simple view of the same scene
    (in @link(Shapes)).

    VRML scene also takes care of initiating and managing VRML events
    and routes mechanism (see ProcessEvents).

    The actual VRML/X3D nodes graph is stored in the RootNode property.
    Remember that if you directly change the fields/nodes within the RootNode,
    this scene object must be notified about this.
    The simplest way to do this is to use only TX3DField.Send to change
    the fields' values. Or you can call TX3DField.Changed after each change.
    Or you will have to call ChangedField or ChangedAll method
    of this class.
    If the scene is changed by VRML events, all changes are automagically
    acted upon, so you don't have to do anything.

    For more-or-less static scenes,
    many things are cached and work very quickly.
    E.g. methods BoundingBox, VerticesCount, TrianglesCount, @link(Shapes)
    cache their results so after the first call to @link(TrianglesCount)
    next calls to the same method will return instantly (assuming
    that scene did not change much). }
  TCastleSceneCore = class(TX3DEventsEngine)
  private
    FOwnsRootNode: boolean;
    FShapes: TShapeTree;
    FRootNode: TX3DRootNode;
    FOnPointingDeviceSensorsChange: TNotifyEvent;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FFileName: string;
    FStatic: boolean;
    FShadowMaps: boolean;
    FShadowMapsDefaultSize: Cardinal;
    ScheduleHeadlightOnFromNavigationInfoInChangedAll: boolean;
    { All CameraFromViewpoint calls will disable smooth (animated)
      transitions when this is true.
      This is set to true by @link(Load),
      when ChangedAll is scheduled, so that first camera bindings
      (in particular after LoadAnchor, when the already existing camera
      is changed) will have immediate transitions in newly loaded file.
      ChangedAll sets this back to false at the end. }
    ForceTeleportTransitions: boolean;

    { Humanoids on which we should call AnimateSkin.
      We don't do AnimateSkin immediately, as it would force slowdown
      when many joints are changed at once (e.g. many joints, and each one
      animated with it's own OrientationInterpolator). }
    ScheduledHumanoidAnimateSkin: TX3DNodeList;

    { This always holds pointers to all TShapeTreeLOD instances in Shapes
      tree. }
    ShapeLODs: TObjectList;
    { Recalculate and update LODTree.Level (if camera position known).
      Also sends level_changed when needed. }
    procedure UpdateLODLevel(LODTree: TShapeTreeLOD);

    procedure SetFileName(const AValue: string);
    procedure SetStatic(const Value: boolean);
    procedure SetShadowMaps(const Value: boolean);
    procedure SetShadowMapsDefaultSize(const Value: Cardinal);

    { Handle change of transformation of ITransformNode node.
      TransformNode and TransformNode must not be @nil here.
      Changes must include chTransform, may also include other changes
      (this will be passed to shapes affected). }
    procedure TransformationChanged(TransformNode: TX3DNode;
      Instances: TShapeTreeList; const Changes: TX3DChanges);
  private
    { For all ITransformNode, except Billboard nodes }
    TransformInstancesList: TTransformInstancesList;
    { For all Billboard nodes }
    BillboardInstancesList: TTransformInstancesList;

    FGlobalLights: TLightInstancesList;

    FBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: array [boolean] of Cardinal;
    Validities: TSceneValidities;
    function CalculateBoundingBox: TBox3D;
    function CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
    function CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
  private
    FShapesActiveCount: Cardinal;
    FShapesActiveVisibleCount: Cardinal;
    FTrianglesListShadowCasters: TTrianglesShadowCastersList;

    { Removes fvTrianglesListShadowCasters from Validities,
      and clears FTrianglesListShadowCasters variable. }
    procedure InvalidateTrianglesListShadowCasters;

    function GetViewpointCore(
      const OnlyPerspective: boolean;
      out ProjectionType: TProjectionType;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string):
      TAbstractViewpointNode;
  private
    FManifoldEdges: TManifoldEdgeList;
    FBorderEdges: TBorderEdgeList;
    FOwnsManifoldAndBorderEdges: boolean;

    { Removes fvManifoldAndBorderEdges from Validities,
      and clears FManifold/BordEdges variables. }
    procedure InvalidateManifoldAndBorderEdges;

    procedure CalculateIfNeededManifoldAndBorderEdges;

    procedure FreeResources_UnloadTextureData(Node: TX3DNode);
    procedure FreeResources_UnloadTexture3DData(Node: TX3DNode);
    procedure FreeResources_UnloadBackgroundImage(Node: TX3DNode);
  private
    FOnGeometryChanged: TSceneGeometryChanged;
    FOnViewpointsChanged: TSceneNotification;
    FOnBoundViewpointVectorsChanged: TSceneNotification;
    FOnBoundNavigationInfoFieldsChanged: TSceneNotification;

    FProcessEvents: boolean;
    procedure SetProcessEvents(const Value: boolean);
  private
    KeyDeviceSensorNodes: TX3DNodeList;
    TimeDependentHandlers: TTimeDependentHandlerList;
    ProximitySensors: TProximitySensorInstanceList;
    FVisibilitySensors: TVisibilitySensors;

    procedure ChangedAllEnumerateCallback(Node: TX3DNode);
    procedure ScriptsInitializeCallback(Node: TX3DNode);
    procedure ScriptsFinalizeCallback(Node: TX3DNode);
    procedure UnregisterSceneCallback(Node: TX3DNode);

    procedure ScriptsInitialize;
    procedure ScriptsFinalize;
  private
    FTime: TX3DTime;

    { Internal procedure that handles Time changes. }
    procedure InternalSetTime(
      const NewValue: TX3DTime; const TimeIncrease: TFloatTime; const ResetTime: boolean);

    procedure ResetLastEventTime(Node: TX3DNode);
  private
    { Bindable nodes helpers }
    FBackgroundStack: TX3DBindableStack;
    FFogStack: TX3DBindableStack;
    FNavigationInfoStack: TNavigationInfoStack;
    FViewpointStack: TViewpointStack;

    { If @true, then next ChangedAll call will do ProcessShadowMapsReceivers
      at the end.

      ProcessShadowMapsReceivers are correctly called at the
      right moment of ChangedAll, so that they have the necessary
      information (Shapes) ready and their modifications (new GeneratedTextures
      items) are accounted for. }
    ScheduledShadowMapsProcessing: boolean;

    { Mechanism to schedule ChangedAll and GeometryChanged calls. }
    ChangedAllSchedule: Cardinal;
    ChangedAllScheduled: boolean;

    ChangedAllCurrentViewpointIndex: Cardinal;
    FInitialViewpointIndex: Cardinal;
    FInitialViewpointName: string;

    FPointingDeviceOverItem: PTriangle;
    FPointingDeviceOverPoint: TVector3Single;
    FPointingDeviceActive: boolean;
    FPointingDeviceActiveSensors: TX3DNodeList;
  private
    { Call this when the ProximitySensor instance changed (either the box or
      it's transformation) or when camera position changed (by user actions
      or animating the Viewpoint).

      Camera position/dir/up must at this point be stored within Camera*
      properties. }
    procedure ProximitySensorUpdate(const PSI: TProximitySensorInstance);
  private
    FCameraPosition, FCameraDirection, FCameraUp: TVector3Single;
    FCameraViewKnown: boolean;

    FCompiledScriptHandlers: TCompiledScriptHandlerInfoList;

    function OverrideOctreeLimits(
      const BaseLimits: TOctreeLimits;
      const OP: TSceneOctreeProperties): TOctreeLimits;

    { Create octree containing all triangles or shapes from our scene.
      Create octree, inits it with our BoundingBox
      and adds shapes (or all triangles from our Shapes).

      Triangles are generated using calls like
      @code(Shape.Triangulate(false, ...)).
      Note that OverTriangulate parameter for Triangulate call above is @false:
      it shouldn't be needed to have triangle octree with over-triangulate
      (over-triangulate is only for rendering with Gouraud shading).

      If Collidable, then only the collidable, or at least "pickable",
      triangles are generated. Which means that children of
      Collision nodes with collide = FALSE (or proxy <> nil) are not placed here.
      Otherwise, only the visible (not necessarily collidable)
      items are placed in the octree.

      If ProgressTitle <> '' (and progress is not active already,
      so we avoid starting "progress bar within progress bar",
      and progress user interface is initialized)
      then it uses @link(Progress) while building octree.

      Remember that triangle octree has references to Shape nodes
      inside RootNode vrml tree and to State objects inside
      our @link(Shapes) tree.
      And shape octree has references to our @link(Shapes) tree.
      So you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of any
      OctreeXxx property, and the returned octree is not managed
      by this scene.

      Everything in my units is done in the spirit
      that you can create as many octrees as you want for a given scene
      (both octrees based on triangles and based on shapes).
      Also, in some special cases an octree may be constructed in
      some special way (not only using @link(CreateShapeOctree)
      or @link(CreateTriangleOctree)) so that it doesn't contain
      the whole scene from some TCastleSceneCore object, or it contains
      the scene from many TCastleSceneCore objects, or something else.

      What I want to say is that it's generally wrong to think of
      an octree as something that maps 1-1 to some TCastleSceneCore object.
      Octrees, as implemented here, are a lot more flexible.

      @groupBegin }
    function CreateTriangleOctree(const Limits: TOctreeLimits;
      const ProgressTitle: string;
      const Collidable: boolean): TTriangleOctree;
    function CreateShapeOctree(const Limits: TOctreeLimits;
      const ProgressTitle: string;
      const Collidable: boolean): TShapeOctree;
    { @groupEnd }
  private
    TriangleOctreeToAdd: TTriangleOctree;
    procedure AddTriangleToOctreeProgress(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
  private
    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FShapeOctreeLimits: TOctreeLimits;
    FShapeOctreeProgressTitle: string;

    FOctreeRendering: TShapeOctree;
    FOctreeDynamicCollisions: TShapeOctree;
    FOctreeVisibleTriangles: TTriangleOctree;
    FOctreeCollidableTriangles: TTriangleOctree;
    FSpatial: TSceneSpatialStructures;

    procedure SetSpatial(const Value: TSceneSpatialStructures);
  private
    FMainLightForShadowsExists: boolean;
    FMainLightForShadows: TVector4Single;
    FMainLightForShadowsNode: TAbstractLightNode;
    FMainLightForShadowsTransform: TMatrix4Single;
    procedure SearchMainLightForShadows(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    { Based on FMainLightForShadowsNode and FMainLightForShadowsTransform,
      calculate FMainLightForShadows (position). }
    procedure CalculateMainLightForShadowsPosition;
    procedure ValidateMainLightForShadows;
  private
    FHeadlightOn: boolean;
    FOnHeadlightOnChanged: TNotifyEvent;

    procedure RenderingCameraChanged(const RenderingCamera: TRenderingCamera;
      Viewpoint: TAbstractViewpointNode);
    procedure SetHeadlightOn(const Value: boolean);
  protected
    { Value <> 0 means that our state isn't complete (for example,
      we're in the middle of ChangedAll call or octree creation).

      Some callbacks *may* be called during such time: namely,
      the progress call (e.g. done during constructing octrees).
      As these callbacks may try to e.g. render our scene (which should
      not be done on the dirty state), we have to protect ourselves
      using this variable (e.g. Render routines will exit immediately
      when Dirty <> 0). }
    Dirty: Cardinal;

    { List of TScreenEffectNode nodes, collected by ChangedAll. }
    ScreenEffectNodes: TX3DNodeList;

    { Create TShape (or descendant) instance suitable for this
      TCastleSceneCore descendant. In this class, this simply creates new
      TShape instance. If you make a descendant of TCastleSceneCore,
      you may need to store some per-shape information, and then it may
      be useful to have your own TShape descendant to carry this information.
      So you can override this to create your own descendant, and then
      you're sure that all leafs within Shapes tree are created using
      this.

      Example: TCastleScene uses this to create TGLShape. }
    function CreateShape(AGeometry: TAbstractGeometryNode;
      AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape; virtual;

    procedure UpdateHeadlightOnFromNavigationInfo;

    procedure InvalidateBackground; virtual;

    property VisibilitySensors: TVisibilitySensors read FVisibilitySensors;
  protected
    GeneratedTextures: TGeneratedTextureList;

    { Called after PointingDeviceSensors or
      PointingDeviceActiveSensors lists (possibly) changed.

      In this class, DoPointingDeviceSensorsChange updates Cursor and calls
      OnPointingDeviceSensorsChange. }
    procedure DoPointingDeviceSensorsChange; virtual;

    procedure ExecuteCompiledScript(const HandlerName: string; ReceivedValue: TX3DField); override;

    function HeightCollision(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
    function MoveCollision(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveCollision(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; override;
  public
    constructor Create(AOwner: TComponent); override;

    { Load new 3D model (from VRML node tree).
      This replaces RootNode with new value.

      If AResetTime, we will also do ResetTimeAtLoad,
      changing @link(Time) --- this is usually what you want when you
      really load a new world. }
    procedure Load(ARootNode: TX3DRootNode; AOwnsRootNode: boolean;
      const AResetTime: boolean = true);

    { Load the 3D model from given AFileName.

      Model is loaded by Load3D, so this supports all
      3D model formats that Load3D handles
      (VRML, X3D, Wavefront OBJ, 3DS, Collada and more).

      @param(AllowStdIn If AllowStdIn and AFileName = '-' then we will load
        a file from standard input (StdInStream), using GetCurrentDir
        as BaseUrl (to resolve relative URLs from the file).
        Currently, this limits the file to be VRML/X3D.) }
    procedure Load(const AFileName: string; AllowStdIn: boolean = false;
      const AResetTime: boolean = true);

    destructor Destroy; override;

    { Simple (usually very flat) tree of shapes within this VRML scene.

      Contents of this tree are read-only from outside.

      Note that the only place where @link(Shapes) structure is rebuild
      in this class is ChangedAll procedure.
      So e.g. if you want to do something after each change of
      @link(Shapes) tree, you can simply override ChangedAll
      and do your work after calling "inherited". }
    property Shapes: TShapeTree read FShapes;

    { Number of active shapes in the @link(Shapes) tree.
      This is equivalent to Shapes.ShapesCount(true), except that this
      is faster (it's cached and reused in this instance, and automatically
      invalidated only when needed). }
    function ShapesActiveCount: Cardinal;

    { Number of active and visible (TShape.Visible) shapes in the
      @link(Shapes) tree.

      @seealso ShapesActiveCount }
    function ShapesActiveVisibleCount: Cardinal;

    { Calculate bounding box, number of triangls and vertexes of all
      shapa states. For detailed specification of what these functions
      do (and what does OverTriangulate mean) see appropriate
      TAbstractGeometryNode methods. Here, we just sum their results
      for all shapes.
      @groupBegin }
    function BoundingBox: TBox3D; override;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;
    { @groupEnd }

    { Methods to notify this class about changes to the underlying RootNode
      graph. Since this class caches some things, it has to be notified
      when you manually change something within RootNode graph. }

    { Release all internal associations with your VRML nodes.
      In particular, this will release OpenGL resources connected to your nodes.
      You should always call this @italic(before) you (may) free some nodes in
      @link(RootNode).

      @italic(You have to call ChangedAll or Load at sometime
      after BeforeNodesFree, and before you try actual rendering, events etc.)
      Otherwise some stuff may not get recalculated.

      InternalChangedAll is for internal use. This is @true when ChangedAll
      calls it at the beginning of work, and means that nothing is freed,
      and we only require necessary cleanup at the beginning of ChangedAll.
      This way ChangedAll (when it wasn't preceeded by explicit
      BeforeNodesFree(false)) produces events from stacks CheckForDeletedNodes. }
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); override;

    { Call Node.FreeRemovingFromAllParents, making sure that changes
      to our VRML node graph are allowed. This makes sure we call
      BeforeNodesFree befor freeing, and ChangedAll afterwards.

      This avoids a common pitfall with relying on TShape or such
      existence between BeforeNodesFree and ChangedAll.
      BeforeNodesFree may free all our TShape instances, so if you
      want to free TX3DNode from our graph --- you typically want to
      get this TX3DNode instance *before* calling BeforeNodesFree.
      Using this method to free the node ensures this. }
    procedure NodeFreeRemovingFromAllParents(Node: TX3DNode);

    { Remove the geometry of this shape from the scene. }
    procedure RemoveShapeGeometry(Shape: TShape);

    { Notify scene that potentially everything changed
      in the VRML graph. This includes adding/removal of some nodes within
      RootNode graph and changing their fields' values.
      (Before freeing the nodes, remember to also call BeforeNodesFree
      earlier.)

      ChangedAll causes recalculation of all things dependent on RootNode,
      so it's very costly to call this. While you have to call some ChangedXxx
      method after you changed RootNode graph directly, usually you
      can call something more efficient, like ChangedField.

      @italic(Descendant implementors notes:) ChangedAll is virtual,
      when overriding it remember that it's
      called by constructor of this class, so you can put a lot of your
      initialization there (instead of in the constructor).

      ChangedAll calls BeforeNodesFree(true) first, for safety (and TGLShape
      actually depends on it, see implementation comments). }
    procedure ChangedAll; override;

    { Notify scene that you changed the value of given field.

      This does relatively intelligent discovery what could be possibly
      affected by this field, and updates / invalidates
      internal information where needed.

      Every change you do directly to the VRML/X3D nodes inside RootNode
      (using directly the methods of TX3DNode or TX3DField) must be
      reported to scene by calling this method. This includes changes
      to the inactive graph part (e.g. in inactive Switch child),
      because our shapes have to track it also.
      Changes to TShape.State.LastNodes (these nodes may come
      from StateDefaultNodes) should also be reported here.
      In fact, you can even notify this scene about changes to fields
      that don't belong to our RootNode --- nothing bad will happen.
      We always try to intelligently
      detect what this change implicates for this VRML scene. }
    procedure ChangedField(Field: TX3DField); override;

    { Notification when geometry changed.
      "Geometry changed" means that the positions
      of triangles changed. This is not send when merely things like
      material changed.

      It is not guaranteed that octrees are already recalculated
      when this is called. (They may be recalculated only on-demand,
      that is when you actually access them.)
      However, it is guaranteed that shape's transformation
      (like TShape.State.Transform) are already updated. }
    property OnGeometryChanged: TSceneGeometryChanged
      read FOnGeometryChanged write FOnGeometryChanged;

    { Notification when the list of viewpoints in the scene possibly
      changed.

      Note that this doesn't necessarily mean that the current,
      bound viewpoint changed (although it could).
      If you only want to get notified when currently @italic(bound)
      viewpoint changes, then what you seek is rather
      @link(TX3DBindableStack.OnBoundChanged ViewpointStack.OnBoundChanged). }
    property OnViewpointsChanged: TSceneNotification
      read FOnViewpointsChanged write FOnViewpointsChanged;

    { Notification when the currently bound viewpoint's vectors
      (position/orientation and such) changed.

      More precisely, this is called whenever values generated by
      ViewpointStack.Top.GetView changed.

      It cannot be called when ViewpointStack.Top = @nil.
      Note that this also doesn't notify you about changes to
      currently bound viewpoint, for this you rather want to use
      @link(TX3DBindableStack.OnBoundChanged ViewpointStack.OnBoundChanged).
      This is called only when @italic(currently bound viewpoint stays
      the same, only it's vectors change). }
    property OnBoundViewpointVectorsChanged: TSceneNotification
      read FOnBoundViewpointVectorsChanged write FOnBoundViewpointVectorsChanged;

    { Called when geometry changed.
      Does OnGeometryChanged, and does some other stuff necessary
      (mark some octrees for regenerating at next access).

      This is public only for overloading (and for internal TShape
      access). Do not call this yourself --- TShape and TCastleSceneCore
      implementations know when and how to call this. }
    procedure DoGeometryChanged(const Change: TGeometryChange;
      LocalGeometryShape: TShape); virtual;

    { Call OnViewpointsChanged, if assigned. }
    procedure DoViewpointsChanged;

    { Call OnBoundViewpointVectorsChanged, if assigned. }
    procedure DoBoundViewpointVectorsChanged;

    property OnBoundNavigationInfoFieldsChanged: TSceneNotification
      read FOnBoundNavigationInfoFieldsChanged write FOnBoundNavigationInfoFieldsChanged;
    procedure DoBoundNavigationInfoFieldsChanged; virtual;

    { Mechanism to schedule ChangedAll calls.

      Since these calls may be costly (traversing the hierarchy),
      and their results are often
      not immediately needed by TCastleSceneCore or TX3DNode hierarchy,
      it's sometimes not desirable to call them immediately
      when geometry changed / all changed.

      So you can use ScheduleChangedAll instead of ChangedAll.
      All event handlers within TCastleSceneCore already do this.

      When you're within Begin/EndChangesSchedule, then
      ScheduleChangedAll just sets an internal flag and actual ChangedAll
      will be done only once at EndChangesSchedule. Otherwise
      (when not within Begin/EndChangesSchedule), ScheduleChangedAll will
      immediately call ChangedAll.

      @groupBegin }
    procedure ScheduleChangedAll;
    procedure BeginChangesSchedule;
    procedure EndChangesSchedule;
    { @groupEnd }

    { Returns short information about the scene.
      This consists of a few lines, separated by CastleUtils.NL.
      Last line also ends with CastleUtils.NL.

      Note that AManifoldAndBorderEdges = @true will require calculation
      of ManifoldEdges and BorderEdges (if they weren't calculated already).
      If you don't want to actually use them (if you wanted only to
      report them to user), then you may free them (freeing some memory)
      with @code(FreeResources([frManifoldAndBorderEdges])). }
    function Info(
      ATriangleVerticesCounts,
      ABoundingBox,
      AManifoldAndBorderEdges: boolean): string;

    function InfoTriangleVerticesCounts: string;
    function InfoBoundingBox: string;
    function InfoManifoldAndBorderEdges: string;

    { Actual VRML/X3D graph defining this scene.

      It is allowed to change contents of RootNode. Just make sure
      the scene is notified about these changes --- you should assign
      fields using methods like TX3DField.Send or (if you assign field
      values directly, like @code(TSFVec3f.Value := ...))
      call TX3DField.Changed. Eventually, you can call our ChangedField method
      (but it's usually nicer to use TX3DField.Changed).

      It is also allowed to change the value of RootNode
      and even to set RootNode to @nil. Be sure to call ChangedAll after this.
      Changing RootNode allows you to load
      and unload whole new VRML/X3D graph (for example from some 3D file)
      whenever you want, and keep the same TCastleSceneCore instance
      (with the same rendering settings and such).

      Note that there is also a trick to conserve memory use.
      After you've done PrepareResources some things are precalculated here,
      and RootNode is actually not used, unless you use ProcessEvents.
      So you can free RootNode
      (and set it to nil here) @italic(without calling ChangedAll)
      and some things will just continue to work, unaware of the fact
      that the underlying RootNode structure is lost.
      Note that this is still considered a "dirty trick", and you will
      have to be extra-careful then about what methods/properties
      from this class. Generally, use only things that you prepared
      with PrepareResources. So e.g. calling Render or using BoundingBox.
      If all your needs are that simple, then you can use this trick
      to save some memory. This is actually useful when using TCastlePrecalculatedAnimation,
      as it creates a lot of intermediate node structures and TCastleSceneCore
      instances. }
    property RootNode: TX3DRootNode read FRootNode write FRootNode;

    { If @true, RootNode will be freed by destructor of this class. }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode default true;

    { The dynamic octree containing all visible shapes.
      It's useful for "frustum culling", it will be automatically
      used by TCastleScene.RenderFrustum to speed up the rendering.

      This octree will be automatically updated on dynamic scenes
      (when e.g. animation moves some shape by changing it's transformation).

      Add ssRendering to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects.  }
    function OctreeRendering: TShapeOctree;

    { The dynamic octree containing all collidable items.

      This is actually a hierarchy of octrees: scene is partitioned
      first into Shapes (each instance of VRML geometry node),
      and then each Shape has an octree of triangles inside.

      This octree is useful for all kinds of collision detection.
      Compared to OctreeCollidableTriangles, it is (very slightly on typical scenes)
      less efficient, but it can also be updated very fast.
      For example, merely transforming some Shape means that only
      one item needs to be moved in the top-level shape tree.
      So this is the most important structure for collision detection on
      dynamic scenes.

      You can use OctreeCollisions to get either OctreeDynamicCollisions
      or OctreeCollidableTriangles, whichever is available.

      Add ssDynamicCollisions to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(collidable (not necessarily rendered)) objects.

      TODO: Temporarily, this is updated simply by rebuilding.
      This is a work in progress. }
    function OctreeDynamicCollisions: TShapeOctree;

    { The octree containing all visible triangles.
      It's mainly useful for ray-tracers. When rendering using OpenGL,
      this has no use currently.

      This octree is not updated on scene changes. In fact, the scene
      contents cannot change when this octree is created --- as this octree
      keeps pointers to some states that may become invalid in dynamic scenes.

      Add ssVisibleTriangles to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects.  }
    function OctreeVisibleTriangles: TTriangleOctree;

    { The octree containing all collidable triangles.
      This is pretty much unused for now.

      It may be useful if you're absolutely sure that you have a static scene
      (nothing changes, e.g. because ProcessEvents = @false) and
      you want to have collision detection with the scene.

      For dynamic scenes, using this is a bad idea as
      this octree is not updated on scene changes. In fact, the scene
      contents cannot change when this octree is created --- as this octree
      keeps pointers to some states that may become invalid in dynamic scenes.
      Use OctreeDynamicCollisions for dynamic scenes.

      You can use OctreeCollisions to get either OctreeDynamicCollisions
      or OctreeCollidableTriangles, whichever is available.

      Add ssCollidableTriangles to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(collidable (not necessarily rendered)) objects.  }
    function OctreeCollidableTriangles: TTriangleOctree;

    { Octree for collisions. This returns either OctreeCollidableTriangles
      or OctreeDynamicCollisions, whichever is available (or @nil if none).
      Be sure to add ssDynamicCollisions or ssCollidableTriangles to have
      this available. }
    function OctreeCollisions: TBaseTrianglesOctree;

    { Properties of created triangle octrees.
      See TriangleOctree unit comments for description.

      Default value comes from DefTriangleOctreeLimits.

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_octree_properties].

      @groupBegin }
    function TriangleOctreeLimits: POctreeLimits;

    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;
    { @groupEnd }

    { Properties of created shape octrees.
      See ShapeOctree unit comments for description.

      Default value comes from DefShapeOctreeLimits.

      If ShapeOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      (so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_octree_properties].

      @groupBegin }
    function ShapeOctreeLimits: POctreeLimits;

    property ShapeOctreeProgressTitle: string
      read  FShapeOctreeProgressTitle
      write FShapeOctreeProgressTitle;
    { @groupEnd }

    { Viewpoint defined in the 3D file (or some default camera settings
      if no viewpoint is found).

      GetViewpoint seeks for VRML/X3D nodes like
      Viewpoint, OrthoViewpoint (actually, any X3DViewpointNode)
      and VRML 1.0 PerspectiveCamera and OrthographicCamera.
      GetPerspectiveViewpoint seeks only for perspective viewpoints.

      If ViewpointDescription = '', they return the first found viewpoint node.
      Otherwise, they look for X3DViewpointNode with description field mathing
      given string.

      If camera properties were found in some node,
      it returns this node. Otherwise it returns nil.
      This way you can optionally extract some additional info from
      used viewpoint node, or do something special if default values
      were used. Often you will just ignore result of this function
      --- after all, the most important feature of this function
      is that you don't @italic(have) to care about details of
      dealing with camera node.

      Returned CamDir and CamUp and GravityUp are always normalized ---
      reasons the same as for TAbstractViewpointNode.GetView.

      @groupBegin }
    function GetViewpoint(
      out ProjectionType: TProjectionType;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string = ''):
      TAbstractViewpointNode;

    function GetPerspectiveViewpoint(
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string = ''):
      TAbstractViewpointNode;
    { @groupEnd }

    { Currently bound fog for this scene.
      A trivial shortcut for FogStack.Top.
      It returns currently bound Fog node, or @nil if none. }
    function FogNode: TFogNode;

    { Returns an array of triangles that should be shadow casters
      for this scene.

      Additionally, TTrianglesShadowCastersList contains some
      additional information needed for rendering with shadows:
      currently, this means TTrianglesShadowCastersList.OpaqueCount.

      Results of these functions are cached, and are also owned by this object.
      So don't modify it, don't free it. }
    function TrianglesListShadowCasters: TTrianglesShadowCastersList;

    { ManifoldEdges is a list of edges that have exactly @bold(two) neighbor
      triangles, and BorderEdges is a list of edges that have exactly @bold(one)
      neighbor triangle. These are crucial for rendering shadows using shadow
      volumes.

      Edges with more than two neighbors are allowed. If an edge has an odd
      number of neighbors, it will be placed in BorderEdges. Every other pair
      of neighbors will be "paired" and placed as one manifold edge inside
      ManifoldEdges. So actually edge with exactly 1 neighbor (odd number,
      so makes one BorderEdges item) and edge with exactly 2 neighbors
      (even number, one pair of triangles, makes one item in ManifoldEdges)
      --- they are just a special case of a general rule, that allows any
      neighbors number.

      Note that vertexes must be consistently ordered in triangles.
      For two neighboring triangles, if one triangle's edge has
      order V0, V1, then on the neighbor triangle the order must be reversed
      (V1, V0). This is true in almost all situations, for example
      if you have a closed solid object and all outside faces are ordered
      consistently (all CCW or all CW).
      Failure to order consistently will result in edges not being "paired",
      i.e. we will not recognize that some 2 edges are in fact one edge between
      two neighboring triangles --- and this will result in more edges in
      BorderEdges.

      Both of these lists are calculated at once, i.e. when you call ManifoldEdges
      or BorderEdges for the 1st time, actually both ManifoldEdges and
      BorderEdges are calculated at once. If all edges are in ManifoldEdges,
      then the scene is a correct closed manifold, or rather it's composed
      from any number of closed manifolds.

      Results of these functions are cached, and are also owned by this object.
      So don't modify it, don't free it.

      This uses TrianglesListShadowCasters.

      @groupBegin }
    function ManifoldEdges: TManifoldEdgeList;
    function BorderEdges: TBorderEdgeList;
    { @groupEnd }

    { This allows you to "share" @link(ManifoldEdges) and
      @link(BorderEdges) values between TCastleSceneCore instances,
      to conserve memory and preparation time.
      The values set here will be returned by following ManifoldEdges and
      BorderEdges calls. The values passed here will @italic(not
      be owned) by this object --- you gave this, you're responsible for
      freeing it.

      This is handy if you know that this scene has the same
      ManifoldEdges and BorderEdges contents as some other scene. In particular,
      this is extremely handy in cases of animations in TCastlePrecalculatedAnimation,
      where all scenes actually need only a single instance of TManifoldEdgeList
      and TBorderEdgeList,
      this greatly speeds up TCastlePrecalculatedAnimation loading and reduces memory use.

      Note that passing here as values the same references
      that are already returned by ManifoldEdges / BorderEdges is always
      guaranteed to be a harmless operation. If ManifoldEdges  / BorderEdges
      was owned by this object,
      it will remain owned in this case (while in normal sharing situation,
      values set here are assumed to be owned by something else). }
    procedure ShareManifoldAndBorderEdges(
      ManifoldShared: TManifoldEdgeList;
      BorderShared: TBorderEdgeList);

    { Frees some scene resources, to conserve memory.
      See TSceneFreeResources documentation. }
    procedure FreeResources(Resources: TSceneFreeResources);

    { Recursively unset node's TX3DNode.Scene. Useful if you want to remove
      part of a node graph and put it in some other scene.

      @italic(You almost never need to call this method) --- this
      is done automatically for you when TCastleSceneCore is destroyed.
      However, if you process RootNode graph
      and extract some node from it (that is, delete node from our
      RootNode graph, but instead of freeing it you insert it
      into some other VRML graph) you must call it to manually
      "untie" this node (and all it's children) from this TCastleSceneCore instance. }
    procedure UnregisterScene(Node: TX3DNode);

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;

    { Called when pointing device moves.
      This may generate the continously-generated events like
      hitPoint_changed, also it updates PointingDeviceOverItem
      and PointingDeviceOverPoint,
      thus producing isOver and such events.

      To make pointing-device sensors work Ok, make sure you have non-nil
      OctreeCollisions (e.g. include ssDynamicCollisions in @link(Spatial)). }
    function PointingDeviceMove(const Pick: TRayCollisionNode;
      const Distance: Single): boolean; override;

    { Current item over which the pointing device is. @nil if over none.
      For example, you can investigate it's pointing device sensors
      (in PointingDeviceOverItem^.State.PointingDeviceSensors),
      although there's a shortcut for just this in @link(PointingDeviceSensors).
      You can change this by PointingDeviceMove and PointingDeviceClear. }
    property PointingDeviceOverItem: PTriangle
      read FPointingDeviceOverItem write FPointingDeviceOverItem;

    { Current 3D point under the pointing device.
      Only meaningful when PointingDeviceOverItem <> nil,
      otherwise undefined. }
    property PointingDeviceOverPoint: TVector3Single
      read FPointingDeviceOverPoint write FPointingDeviceOverPoint;

    { Pointing-device sensors over which the pointing device is.
      This is just a shortcut for
      PointingDeviceOverItem^.State.PointingDeviceSensors,
      returning @nil if PointingDeviceOverItem = @nil. }
    function PointingDeviceSensors: TPointingDeviceSensorList;

    { Currently active pointing-device sensors.
      Only TAbstractPointingDeviceSensorNode instances.
      Always empty when PointingDeviceActive = @false.
      Read-only from outside of this class.

      Note that sensor specified here doesn't have to be one of the sensors of
      PointingDeviceOverItem. When some sensor is activated, it grabs
      further events until it's deactivated (e.g. when you set
      PointingDeviceActive := false, which means that user released mouse
      button). This means that when user moves the mouse while given
      sensors are active, he can move mouse over other items, even the ones
      where the active sensors aren't listed --- but the sensors remain active. }
    property PointingDeviceActiveSensors: TX3DNodeList
      read FPointingDeviceActiveSensors;

    { Clear any references to OverItem passed previously to PointingDeviceMove.
      This is sometimes useful, because PointingDeviceMove may save
      the last passed OverItem, and PointingDeviceActivate may even
      save some sensors for a longer time.

      You could free it by calling
      @link(PointingDeviceMove) with Collision = @nil,
      but this could cause other X3D events,
      so it's undesirable to call this when you're going to e.g. release
      the scene or it's octree. Also, you would have to deactivate sensor
      first, causing even more events.

      So this method clears any references to saved OverItem and
      PointingDeviceActiveSensors, without calling any VRML/X3D events.
      Note that this still calls DoPointingDeviceSensorsChange
      (making OnPointingDeviceSensorsChange event), if PointingDeviceActiveSensors /
      PointingDeviceSensors possibly changed. }
    procedure PointingDeviceClear;

    { Is pointing device currently active
      (for example, mouse button is pressed down). }
    property PointingDeviceActive: boolean
      read FPointingDeviceActive default false;

    { Event called PointingDeviceSensors or
      PointingDeviceActiveSensors lists (possibly) changed. }
    property OnPointingDeviceSensorsChange: TNotifyEvent
      read FOnPointingDeviceSensorsChange
      write FOnPointingDeviceSensorsChange;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { Change current scene time, setting @link(Time).
      It is crucial that you call this continously to have some VRML/X3D
      time-dependent features working, like TimeSensor and MovieTexture.
      See @link(Time) for details what is affected by this.

      This is automatically taken care of if you added this scene
      to TCastleWindowCustom.Controls or TCastleControlCustom.Controls.
      Then our @link(Idle) takes care of doing the job,
      according to TimePlaying and TimePlayingSpeed.

      This causes time to be passed to appropriate time-dependent nodes,
      events will be called etc.

      SetTime and IncreaseTime do exactly the same, the difference is
      only that for IncreaseTime you specify increase in the time
      (that is, NewTime = @link(Time) + TimeIncrease). Use whichever version
      is more handy.

      Following X3D specification, time should only grow. So NewValue should
      be > @link(Time) (or TimeIncrease > 0).
      Otherwise we ignore this call.
      For resetting the time (when you don't necessarily want to grow @link(Time))
      see ResetTime.

      If a change of Time will produce some visible change in the VRML/X3D model
      (for example, MovieTexture will change, or TimeSensor change will be
      routed by interpolator to coordinates of some visible node) it
      will be reported by usual method, that is VisibleChangeHere.

      @groupBegin }
    procedure SetTime(const NewValue: TFloatTime);
    procedure IncreaseTime(const TimeIncrease: TFloatTime);
    { @groupEnd }

    { Increase @link(Time) by some infinitely small value.
      This simply increments @code(Time.PlusTicks), which may be sometimes
      useful: this allows events to pass through the ROUTEs
      without the fear of being rejected as "recursive (cycle) events". }
    procedure IncreaseTimeTick; override;

    { This is the scene time, that is passed to time-dependent nodes.
      See X3D specification "Time" component about time-dependent nodes.
      In short, this "drives" the time passed to TimeSensor, MovieTexture
      and AudioClip. See SetTime for changing this.

      Default value is 0.0 (zero). }
    property Time: TX3DTime read FTime;
    function GetTime: TX3DTime; override;

    { Set @link(Time) to arbitrary value.

      You should only use this when you loaded new VRML model.

      Unlike SetTime and IncreaseTime, this doesn't require that
      @link(Time) grows. It still does some time-dependent events work,
      although some time-dependent nodes may be just unconditionally reset
      by this to starting value (as they keep local time, and so require
      TimeIncrease notion, without it we can only reset them).

      @groupBegin }
    procedure ResetTime(const NewValue: TFloatTime);
    { @groupEnd }

    { Set @link(Time) to suitable initial value after loading a world.

      This honours VRML specification about VRML time origin,
      and our extension
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_time_origin_at_load]. }
    procedure ResetTimeAtLoad;

    { Binding stack of X3DBackgroundNode nodes.
      All descend from TAbstractBackgroundNode class. }
    property BackgroundStack: TX3DBindableStack read FBackgroundStack;

    { Binding stack of Fog nodes.
      All descend from TFogNode class. }
    property FogStack: TX3DBindableStack read FFogStack;

    { Binding stack of NavigatinInfo nodes.
      All descend from TNavigationInfoNode class. }
    property NavigationInfoStack: TNavigationInfoStack read FNavigationInfoStack;

    { Binding stack of Viewpoint nodes.
      All descend from TAbstractViewpointNode (not necessarily from
      TAbstractX3DViewpointNode, so VRML 1.0 camera nodes are also included in
      this stack.) }
    property ViewpointStack: TViewpointStack read FViewpointStack;

    function GetViewpointStack: TX3DBindableStackBasic; override;
    function GetNavigationInfoStack: TX3DBindableStackBasic; override;
    function GetBackgroundStack: TX3DBindableStackBasic; override;
    function GetFogStack: TX3DBindableStackBasic; override;

    { Camera position/direction/up known for this scene.

      Set by CameraChanged. CameraViewKnown = @false means that
      CameraChanged was never called, and so camera settings are not known,
      and so other Camera* properties have undefined values.

      These are remembered for various reasons, like
      reacting to changes to ProximitySensor box (center, size)
      (or it's transform), or changing LOD node children.

      @groupBegin }
    property CameraPosition: TVector3Single read FCameraPosition;
    property CameraDirection: TVector3Single read FCameraDirection;
    property CameraUp: TVector3Single read FCameraUp;
    property CameraViewKnown: boolean read FCameraViewKnown;
    { @groupEnd }

    { Call when camera position/dir/up changed, to update things depending
      on camera settings. This includes sensors like ProximitySensor,
      LOD nodes, camera settings for next RenderedTexture update and more.
      It automatically does proper VisibleChangeHere (OnVisibleChangeHere).

      @param(Changes describes changes to the scene caused by camera change.
        This is needed if some geometry / light follows the player.

        We'll automatically add here vcCamera, so not need to specify it.

        You should add here vcVisibleNonGeometry if player has a headlight.
        You should add here vcVisibleGeometry if player has a rendered
        avatar.) }
    procedure CameraChanged(ACamera: TCamera; const Changes: TVisibleChanges);

    { List of handlers for VRML Script node with "compiled:" protocol.
      This is read-only, change this only by RegisterCompiledScript. }
    property CompiledScriptHandlers: TCompiledScriptHandlerInfoList
      read FCompiledScriptHandlers;

    { Register compiled script handler, for VRML Script node with
      "compiled:" protocol.
      See [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_script_compiled]. }
    procedure RegisterCompiledScript(const HandlerName: string;
      Handler: TCompiledScriptHandler);

    { Update camera properties based on currently bound NavigationInfo.

      Bound NavigationInfo node is taken from
      NavigationInfoStack.Top. If no NavigationInfo is bound, this is @nil,
      and we will create camera corresponding to default NavigationInfo
      values (this is following VRML/X3D spec), so it will have
      initial type = EXAMINE.

      You can pass ForceNavigationType = 'EXAMINE', 'WALK', 'FLY', 'NONE' etc.
      (see X3D specification about NavigationInfo node, type field,
      on [http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#NavigationInfo],
      although not all values are handled by our engine now).
      This way we will ignore what NavigationInfo.type information
      inside the scene says.

      This initializes a lot of camera properties:
      @unorderedList(
        @item(TCamera.Radius,)
        @item(TCamera.Input,)
        @item(TWalkCamera.Gravity,)
        @item(TWalkCamera.PreferGravityUpForRotations,)
        @item(TWalkCamera.PreferGravityUpForMoving,)
        @item(TWalkCamera.PreferredHeight,)
        @item(TWalkCamera.ClimbHeight,)
        @item(TWalkCamera.HeadBobbing, TWalkCamera.HeadBobbingTime.)
      )

      Box is the expected bounding box of the whole 3D scene.
      Usually, it should be just Scene.BoundingBox, but it may be something
      larger, if this scene is part of a larger world. }
    procedure CameraFromNavigationInfo(Camera: TCamera;
      const Box: TBox3D;
      const ForceNavigationType: string = '';
      const ForceRadius: Single = 0);

    { Update camera to the currently bound VRML/X3D viewpoint.
      When no viewpoint is currently bound, we will go to a suitable
      viewpoint to see the whole scene (based on a scene bounding box).

      The initial camera vectors (TCamera.InitialPosition,
      TCamera.InitialDirection, TCamera.InitialUp, TWalkCamera.GravityUp)
      are set to the current viewpoint. This is done
      regardless of the RelativeCameraTransform value.

      How current camera vectors change depends on RelativeCameraTransform:

      @unorderedList(
        @item(RelativeCameraTransform = @false means that we just
          set current vectors to the initial vectors.
          In other words, camera jumps to the viewpoint.

          In this case, AllowTransitionAnimate determines if we allow
          moving camera by a smooth transition. If this is @true,
          and also NavigationInfo allows it (see CameraTransition),
          then camera will jump smoothy. Otherwise camera will jump
          to viewpoint immediately.)

        @item(RelativeCameraTransform = @true means that we translate/rotate
          the current camera in the same manner as initial camera
          changed. This is suitable when you change transformation, position
          or orientation of the VRML/X3D Viewpoint node: conceptually,
          there exists a "user" camera transformation that is the child
          of the viewpoint. When viewpoint is moved, then the current
          camera moves with it.)
      ) }
    procedure CameraFromViewpoint(ACamera: TCamera;
      const RelativeCameraTransform: boolean = false;
      const AllowTransitionAnimate: boolean = true);

    { Create new camera instance, and bind it to current NavigationInfo
      and Viewpoint. This is only a shortcut for creating
      TUniversalCamera and then using CameraFromNavigationInfo
      and CameraFromViewpoint.

      CameraFromViewpoint here is called with AllowTransitionAnimate = @false,
      because animating camera in this case would be wrong (user does
      not want to see the animation from default camera position). }
    function CreateCamera(AOwner: TComponent;
      const Box: TBox3D;
      const ForceNavigationType: string = ''): TUniversalCamera;

    { @deprecated }
    function CreateCamera(AOwner: TComponent;
      const ForceNavigationType: string = ''): TUniversalCamera; deprecated;

    { Make Camera go to the view given by Position, Direction, Up.

      Honours current NavigationInfo.transitionType and transitionTime.
      If transitionType indicates instanteneous transition, then jumps
      by simple @code(Camera.SetView(Position, Direction, Up)).
      Otherwise makes a smooth animation into new values by
      @code(Camera.AnimateTo(Position, Direction, TransitionTime)).

      @groupBegin }
    procedure CameraTransition(Camera: TCamera; const Position, Direction, Up: TVector3Single);
    procedure CameraTransition(Camera: TCamera; const Position, Direction, Up, GravityUp: TVector3Single);
    { @groupEnd }

    { Detect position/direction of the main light that produces shadows.
      This is useful when you want to make shadows on the scene
      from only a single light, but your scene has many lights.

      The main light is simply one with both @code(kambiShadows) and
      @code(kambiShadowsMain) fields set to @true. See
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows]
      for more info.
      If no light with kambiShadows = kambiShadowsMain = TRUE
      is present then this function returns @false,
      since AMainLightPosition cannot be calculated.

      AMainLightPosition[3] is always set to 1
      (positional light) or 0 (indicates that this is a directional light).

      @seealso TCastleAbstractViewport.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4Single): boolean;

    { Light node that should be used for headlight, or @nil if default
      directional headlight is suitable.

      This never returns @nil. It's not concerned whether the headlight
      should actually be used --- for this, see HeadlightOn. }
    function CustomHeadlight: TAbstractLightNode;

    { Should we use headlight for this scene. Controls if containing TCastleSceneManager
      will use a headlight, if this is the main scene.

      When you load a new model, this is always updated based on this model's
      NavigationInfo.headlight. (If no NavigationInfo node, then default
      is to use the headlight.) When you bind a new NavigationInfo node,
      this is also updated to follow NavigationInfo.headlight.

      You can change the value of this property. If we have a
      NavigationInfo node, then NavigationInfo.headlight field will
      be always updated to correspond to this value.
      (It will be even updated using events mechanism if ProcessEvents,
      so scripts inside the VRML/X3D "know" when
      you turn on/off the headlight and may react to it,
      e.g. spawn a zombie monster when you turn on the flashlight.) }
    property HeadlightOn: boolean
      read FHeadlightOn write SetHeadlightOn;

    property OnHeadlightOnChanged: TNotifyEvent
      read FOnHeadlightOnChanged write FOnHeadlightOnChanged;

    { Notify the scene that camera position/direction changed a lot.
      It may be called when you make a sudden change to the camera,
      like teleporting the player to a completely different scene part.

      This may be used as a hint by some optimizations. It tells that what
      will be visible in the next rendered frame will be probably
      very different from what was visible in the last frame.

      @italic(Current implementation notes:)

      Currently, this is used by TCastleScene if you use
      Attributes.UseOcclusionQuery.
      Normally, occlusion query tries to reuse results from previous
      frame, using the assumption that usually camera changes slowly
      and objects appear progressively in the view. When you make
      a sudden camera jump/change, this assumption breaks, so it's
      better to resign from occlusion query for the very next frame.
      This method will do exactly that. }
    procedure ViewChangedSuddenly; virtual;

    procedure PrepareResources(Options: TPrepareResourcesOptions;
      ProgressStep: boolean; BaseLights: TAbstractLightInstancesList); override;

    function Dragging: boolean; override;

    { Static scene will not be automatically notified about the changes
      to the field values. This means that TX3DField.Send and
      TX3DField.Changed will not notify this scene. This makes a
      small optimization when you know you will not modify scene's VRML graph
      besides loading (or you're prepared to do it by manually calling
      Scene.ChangedField etc.).

      The behavior of events is undefined when scene is static.
      This means that you should always have ProcessEvents = @false
      when Static = @true. Only when Static = false you're allowed
      to freely change ProcessEvents to @true.

      Changing this is expensive when the scene content is already loaded,
      so it's best to adjust this before @link(Load). }
    property Static: boolean read FStatic write SetStatic default false;

    { Nice scene caption. Uses the "title" of WorldInfo
      node inside the VRML/X3D scene. If there is no WorldInfo node
      (or it has empty title) then returns last loaded FileName
      (without directory). }
    function Caption: string;

    { Global lights of this scene. Read-only. May be useful to render
      other 3D objects with lights defined inside this scene. }
    property GlobalLights: TLightInstancesList read FGlobalLights;

    { Simple methods to find a named X3D node (and a field or event
      within this node) in a current node graph. They search all nodes
      (in active or not) graph parts.

      For more flexible and extensive search methods, use RootNode property
      along with TX3DNode.FindNodeByName, TX3DNode.FindNode and other methods.

      @raises(EX3DNotFound If given node (or field/event inside this node)
        could not be found.)
      @groupBegin }
    function Node(const NodeName: string): TX3DNode;
    function Field(const NodeName, FieldName: string): TX3DField;
    function Event(const NodeName, EventName: string): TX3DEvent;
    { @groupEnd }
  published
    { When TimePlaying is @true, the time of our 3D world will keep playing.
      More precisely, our @link(Idle) will take care of increasing @link(Time).
      Our @link(Idle) is usually automatically called (if you added this
      scene to TCastleWindowCustom.Controls or TCastleControlCustom.Controls)
      so you don't have to do anything to make this work. }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;

    { Controls the time speed (if TimePlaying is @true):
      1.0 means that 1 second  of real time equals to 1 unit of world time. }
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;

    { Which spatial structures (octrees, for now) should be created and managed.

      You should set this, based on your expected usage of this model.
      See TSceneSpatialStructure for possible values.
      For usual dynamic scenes rendered with OpenGL,
      you want this to be [ssRendering, ssDynamicCollisions].

      Before setting any value <> [] you may want to adjust
      TriangleOctreeLimits, ShapeOctreeLimits.
      These properties fine-tune how the octrees will be generated
      (although default values should be Ok for typical cases).

      Default value of this property is [], which means that
      no octrees will be created. This has to be the default value,
      to 1. get you chance to change TriangleOctreeLimits and such
      before creating octree 2. otherwise, scenes that not require
      collision detection would unnecessarily create octrees at construction.
      Scenes that do not have any spatial structures use default T3D
      methods for resolving collisions, which means that collisions
      are checked vs BoundingBox of this scene. (Unless @link(GetCollides)
      is @false, in which case collisions are disabled, regardless
      of @name.) }
    property Spatial: TSceneSpatialStructures read FSpatial write SetSpatial;

    { Should the VRML/X3D event mechanism work.

      If @true, then events will be send and received
      through routes, time dependent nodes (X3DTimeDependentNode,
      like TimeSensor) will be activated and updated from @link(Time) time
      property, @link(Press), @link(Release) and other methods will activate
      key/mouse sensor nodes, scripts will be initialized and work, etc.

      In other words, this makes the scene fully animated and interacting
      with the user.

      If @false, this all doesn't work, which makes the scene static. }
    property ProcessEvents: boolean
      read FProcessEvents write SetProcessEvents default false;

    { Currently loaded scene filename. Set this to load a 3D scene
      from the given file, this can load from any known 3D format
      (VRML, X3D, Collada, 3ds, Wavefront, etc.).

      Works just like the @link(Load) method (the overloaded version
      that takes @code(AFileName: string) parameter). And, in fact, using
      directly the @link(Load) method will also change this FileName property.

      The only difference of @code(Scene.FileName := 'blah.x3d') vs
      @code(Scene.Load('blah.x3d')) is that setting the filename will
      @italic(not) reload the scene if you set it to the same value.
      That is, @code(Scene.FileName := Scene.FileName;) will not reload
      the scene (you have to use explicit @link(Load) for this.). }
    property FileName: string read FFileName write SetFileName;

    { At loading, process the scene to support shadow maps.
      This happens at the @link(Load) method call,
      and it makes "receiveShadows" field automatically handled.

      Note that this is not the only way to make shadow maps.
      VRML author can always make shadow maps by using lower-level nodes, see
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadow_maps].
      When using these lower-level nodes, this property does not matter
      This property (and related ones
      like ShadowMapsDefaultSize)
      is relevant only for handling shadows by the "receiveShadows" field. }
    property ShadowMaps: boolean read FShadowMaps write SetShadowMaps default true;

    { Default shadow map texture size.

      Affects how shadow maps are handled for the "receiveShadows"
      field.  This is taken into account at the scene @link(Load) time,
      and only if @link(ShadowMaps) is @true.

      VRML author can always override this by placing a @code(GeneratedShadowMap)
      node inside light's @code(defaultShadowMap) field. In this case,
      @code(GeneratedShadowMap.size) determines shadow map size. }
    property ShadowMapsDefaultSize: Cardinal
      read FShadowMapsDefaultSize write SetShadowMapsDefaultSize
      default DefaultShadowMapsDefaultSize;

    { When loading new model, use this viewpoint index to initialize camera.
      VRML/X3D specification says to use the first (index = 0) viewpoint,
      you can change this property to bind 2nd, 3rd and so on viewpoints.

      This is applied only at loading (actually, at ChangedAll).
      If you later want to bind another viewpoint, just send set_bind := true
      to it. }
    property InitialViewpointIndex: Cardinal
      read FInitialViewpointIndex write FInitialViewpointIndex default 0;

    { When loading new model and looking for initial viewpoint,
      consider only viewpoints with this node name.
      Relevant only if non-empty.

      This may cooperate with InitialViewpointIndex: InitialViewpointIndex
      specifies the index of viewpoint node that satisfies also
      InitialViewpointName condition. For example:

      @unorderedList(
        @item(InitialViewpointIndex = 0 and InitialViewpointName = ''
          means to use the first viewpoint, ignoring nodes' names.
          This is the default behavior, also following VRML/X3D specification.)
        @item(InitialViewpointIndex = 1 and InitialViewpointName = ''
          means to use the 2nd viewpoint. Node name doesn't matter.)
        @item(InitialViewpointIndex = 1 and InitialViewpointName = 'blah'
          means to use the first viewpoint named 'blah'.
          That is, we are only counting nodes named 'blah' for this.)
      ) }
    property InitialViewpointName: string
      read FInitialViewpointName write FInitialViewpointName;
  end;

var
  { Log changes to fields.
    This debugs what and why happens through TCastleSceneCore.ChangedField method
    and friends, which is central to VRML/X3D dynamic changes and events engine.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogChanges: boolean = false;

implementation

uses X3DCameraUtils, CastleStringUtils, CastleLog, DateUtils, CastleWarnings,
  X3DLoad;

{ TX3DBindableStack ----------------------------------------------------- }

constructor TX3DBindableStack.Create(AParentScene: TCastleSceneCore);
begin
  inherited Create(false);
  FParentScene := AParentScene;
end;

procedure TX3DBindableStack.SendIsBound(Node: TAbstractBindableNode;
  const Value: boolean);
begin
  if Node <> nil then
  begin
    Node.EventIsBound.Send(Value, ParentScene.Time);
    Node.EventBindTime.Send(ParentScene.Time.Seconds, ParentScene.Time);
  end;
end;

function TX3DBindableStack.Top: TAbstractBindableNode;
begin
  if Count <> 0 then
    Result := Last as TAbstractBindableNode else
    Result := nil;
end;

procedure TX3DBindableStack.Push(Node: TAbstractBindableNode);
begin
  Add(Node);
end;

procedure TX3DBindableStack.PushIfEmpty(Node: TAbstractBindableNode;
  SendEvents: boolean);
begin
  if Count = 0 then
  begin
    Push(Node);
    if SendEvents then
      SendIsBound(Node, true);
    DoScheduleBoundChanged;
  end;
end;

function TX3DBindableStack.Pop: TAbstractBindableNode;
begin
  if Count <> 0 then
  begin
    Result := Last as TAbstractBindableNode;
    Count := Count - 1;
  end else
    Result := nil;
end;

procedure TX3DBindableStack.CheckForDeletedNodes(
  const RootNode: TX3DRootNode; const SendEvents: boolean);
var
  I: Integer;
  TopChanged: boolean;
begin
  if RootNode = nil then
    { Just empty all, since all are possibly freed }
    Count := 0 else
  begin
    { Remember that pointers on Items may be wrong now.
      So don't call things like Top function now
      (it typecasts to TAbstractBindableNode). }

    TopChanged := false;

    I := 0;
    while I < Count do
    begin
      { Search also inactive part of VRML graph.
        VRML doesn't allow a bound node to be in inactive VRML graph part,
        but not bound (but present on the stack) node in inactive VRML
        graph part is acceptable, as far as I understand. }
      if not RootNode.IsNodePresent(Items[I], false) then
      begin
        TopChanged := I = Count - 1;
        Delete(I);
      end else
        Inc(I);
    end;

    if TopChanged and SendEvents and (Count <> 0) then
      SendIsBound(Top, true);

    if TopChanged then
      DoScheduleBoundChanged;
  end;
end;

procedure TX3DBindableStack.Set_Bind(Node: TAbstractBindableNode;
  const Value: boolean);
var
  NodeIndex: Integer;
begin
  NodeIndex := IndexOf(Node);
  if Value then
  begin
    if NodeIndex = -1 then
    begin
      { Node not on the stack. So add it, as new top node. }
      SendIsBound(Top, false);
      Push(Node);
      SendIsBound(Top, true);
      DoScheduleBoundChanged;
    end else
    if NodeIndex <> Count - 1 then
    begin
      { Node on the stack, but not on top. So move it, as new top node. }
      SendIsBound(Top, false);
      Exchange(NodeIndex, Count - 1);
      SendIsBound(Top, true);
      DoScheduleBoundChanged;
    end;
    { set_bind = true for node already on the top is ignored. }
  end else
  begin
    if NodeIndex <> -1 then
    begin
      if NodeIndex = Count - 1 then
      begin
        SendIsBound(Top, false);
        Delete(NodeIndex);
        SendIsBound(Top, true);
        DoScheduleBoundChanged;
      end else
      begin
        Delete(NodeIndex);
      end;
    end;
    { set_bind = false for node already outside of the stack is ignored. }
  end;
end;

procedure TX3DBindableStack.DoBoundChanged;
begin
  if Assigned(OnBoundChanged) then
    OnBoundChanged(ParentScene);
end;

procedure TX3DBindableStack.BeginChangesSchedule;
begin
  { BoundChangedScheduled = false always when BoundChangedSchedule = 0. }
  Assert((BoundChangedSchedule <> 0) or (not BoundChangedScheduled));

  Inc(BoundChangedSchedule);
end;

procedure TX3DBindableStack.DoScheduleBoundChanged;
begin
  if BoundChangedSchedule = 0 then
    DoBoundChanged else
    BoundChangedScheduled := true;
end;

procedure TX3DBindableStack.EndChangesSchedule;
begin
  Dec(BoundChangedSchedule);
  if (BoundChangedSchedule = 0) and BoundChangedScheduled then
  begin
    BoundChangedScheduled := false;
    DoBoundChanged;
  end;
end;

{ TViewpointStack -------------------------------------------------------- }

procedure TViewpointStack.DoBoundChanged;
begin
  { The new viewpoint may be in some totally different place of the scene,
    so call ViewChangedSuddenly.

    This takes care of all viewpoints switching, like
    - switching to other viewpoint through view3dscene "viewpoints" menu,
    - just getting an event set_bind = true through vrml route. }

  ParentScene.ViewChangedSuddenly;

  inherited;
end;

function TViewpointStack.Top: TAbstractViewpointNode;
begin
  Result := (inherited Top) as TAbstractViewpointNode;
end;

{ TViewpointStack -------------------------------------------------------- }

procedure TNavigationInfoStack.DoBoundChanged;
begin
  ParentScene.UpdateHeadlightOnFromNavigationInfo;
  inherited;
end;

function TNavigationInfoStack.Top: TNavigationInfoNode;
begin
  Result := (inherited Top) as TNavigationInfoNode;
end;

{ TGeneratedTextureList -------------------------------------------------- }

function TGeneratedTextureList.IndexOfTextureNode(TextureNode: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].TextureNode = TextureNode then
      Exit;
  Result := -1;
end;

function TGeneratedTextureList.FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
var
  Index: Integer;
begin
  Index := IndexOfTextureNode(TextureNode);
  if Index <> -1 then
    Result := Addr(L[Index]) else
    Result := nil;
end;

procedure TGeneratedTextureList.AddShapeTexture(Shape: TShape;
  Tex: TAbstractTextureNode);
var
  GenTex: PGeneratedTexture;
begin
  if (Tex is TGeneratedCubeMapTextureNode) or
     (Tex is TGeneratedShadowMapNode) or
     (Tex is TRenderedTextureNode) then
  begin
    GenTex := FindTextureNode(Tex);
    if GenTex <> nil then
    begin
      { The same generated texture node Tex is instantiated more than once.

        - For GeneratedShadowMap, the shape where it's used, and actually
          the whole place where it's used in the VRML graph, doesn't matter.
          (GeneratedShadowMap makes view from it's light).
          Same thing for RenderedTexture (it makes view from specified viewpoint).

          So we can simply ignore duplicates (placing them
          on GeneratedTextures list would make it updated many times
          in UpdateGeneratedTextures, instead of just once).

        - For GeneratedCubeMapTexture, their view depends on the shape
          where they are used. They can be instanced many times only
          within a single shape (possible if you use MultiTexture
          and instance there the same GeneratedCubeMapTexture),
          then duplicates are simply ignored.

          When GeneratedCubeMapTexture is instanced within different shapes,
          make a warning, and reject duplicate. }

      if (Tex is TGeneratedCubeMapTextureNode) and
         (Shape <> GenTex^.Shape) then
        OnWarning(wtMajor, 'VRML/X3D', 'The same GeneratedCubeMapTexture node is used (instanced) within at least two different VRML shapes. This is bad, as we don''t know from which shape should environment be captured');
    end else
    begin
      GenTex := Add;
      GenTex^.TextureNode := Tex;

      if Tex is TGeneratedCubeMapTextureNode then
        GenTex^.Handler := TGeneratedCubeMapTextureNode(Tex).GeneratedTextureHandler else
      if Tex is TGeneratedShadowMapNode then
        GenTex^.Handler := TGeneratedShadowMapNode(Tex).GeneratedTextureHandler else
      if Tex is TRenderedTextureNode then
        GenTex^.Handler := TRenderedTextureNode(Tex).GeneratedTextureHandler else
        raise EInternalError.Create('sf34234');

      { Make sure to reset UpdateNeeded to true, in case it was false because
        it was already generated but now some change caused ChangedAll.
        Testcase: projected_Spotlight.x3dv from Victor Amat. }
      GenTex^.Handler.UpdateNeeded := true;
      GenTex^.Shape := Shape;
    end;
  end;
end;

procedure TGeneratedTextureList.UpdateShadowMaps(LightNode: TAbstractLightNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (L[I].TextureNode is TGeneratedShadowMapNode) and
       (TGeneratedShadowMapNode(L[I].TextureNode).FdLight.Value = LightNode) then
      L[I].Handler.UpdateNeeded := true;
end;

{ TTransformInstancesList ------------------------------------------------- }

function TTransformInstancesList.Instances(Node: TX3DNode;
  const AutoCreate: boolean): TShapeTreeList;
begin
  Result := Node.ShapeTrees as TShapeTreeList;

  if (Result = nil) and AutoCreate then
  begin
    Node.ShapeTrees := TShapeTreeList.Create(false);
    Result := TShapeTreeList(Node.ShapeTrees);
    Add(Node);
  end;
end;

procedure TTransformInstancesList.FreeShapeTrees;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].ShapeTrees.Free;
    Items[I].ShapeTrees := nil;
  end;
  Count := 0;
end;

{ TTimeDependentHandlerList ------------------------------------------------- }

procedure TTimeDependentHandlerList.AddIfNotExists(const Item: TTimeDependentNodeHandler);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

{ TVisibilitySensors --------------------------------------------------------- }

destructor TVisibilitySensors.Destroy;
begin
  Clear;
  inherited;
end;

procedure TVisibilitySensors.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Data[I].Free;
    Data[I] := nil;
  end;
  Count := 0;
end;

{ TCastleSceneCore ----------------------------------------------------------- }

constructor TCastleSceneCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRootNode := nil;
  FOwnsRootNode := true;

  FTriangleOctreeLimits := DefTriangleOctreeLimits;
  FShapeOctreeLimits := DefShapeOctreeLimits;

  FShapes := TShapeTreeGroup.Create(Self);
  ShapeLODs := TObjectList.Create(false);
  FGlobalLights := TLightInstancesList.Create;

  FBackgroundStack := TX3DBindableStack.Create(Self);
  FFogStack := TX3DBindableStack.Create(Self);
  FNavigationInfoStack := TNavigationInfoStack.Create(Self);
  FViewpointStack := TViewpointStack.Create(Self);

  FPointingDeviceActiveSensors := TX3DNodeList.Create(false);

  FCompiledScriptHandlers := TCompiledScriptHandlerInfoList.Create;
  TransformInstancesList := TTransformInstancesList.Create(false);
  BillboardInstancesList := TTransformInstancesList.Create(false);
  GeneratedTextures := TGeneratedTextureList.Create;
  ProximitySensors := TProximitySensorInstanceList.Create(false);
  FVisibilitySensors := TVisibilitySensors.Create;
  ScreenEffectNodes := TX3DNodeList.Create(false);
  ScheduledHumanoidAnimateSkin := TX3DNodeList.Create(false);
  KeyDeviceSensorNodes := TX3DNodeList.Create(false);
  TimeDependentHandlers := TTimeDependentHandlerList.Create(false);

  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;

  FShadowMaps := true;
  FShadowMapsDefaultSize := DefaultShadowMapsDefaultSize;

  { We could call here ScheduleChangedAll (or directly ChangedAll),
    but there should be no need. FRootNode remains nil,
    and our current state should be equal to what ScheduleChangedAll
    would set. This is (potentially) a small time saving,
    as ScheduleChangedAll does a lot of calls (although probably is fast
    anyway when RootNode = nil). }
end;

destructor TCastleSceneCore.Destroy;
begin
  { This also deinitializes script nodes. }
  ProcessEvents := false;

  { free FTrianglesList* variables }
  InvalidateTrianglesListShadowCasters;

  { frees FManifoldEdges, FBorderEdges if needed }
  InvalidateManifoldAndBorderEdges;

  FreeAndNil(ScheduledHumanoidAnimateSkin);
  FreeAndNil(ScreenEffectNodes);
  FreeAndNil(ProximitySensors);
  FreeAndNil(FVisibilitySensors);
  FreeAndNil(GeneratedTextures);
  if TransformInstancesList <> nil then
  begin
    TransformInstancesList.FreeShapeTrees;
    FreeAndNil(TransformInstancesList);
  end;
  if BillboardInstancesList <> nil then
  begin
    BillboardInstancesList.FreeShapeTrees;
    FreeAndNil(BillboardInstancesList);
  end;
  FreeAndNil(FCompiledScriptHandlers);
  FreeAndNil(KeyDeviceSensorNodes);
  FreeAndNil(TimeDependentHandlers);

  FreeAndNil(FBackgroundStack);
  FreeAndNil(FFogStack);
  FreeAndNil(FNavigationInfoStack);
  FreeAndNil(FViewpointStack);

  FreeAndNil(FPointingDeviceActiveSensors);

  FreeAndNil(FShapes);
  FreeAndNil(ShapeLODs);
  FreeAndNil(FGlobalLights);

  FreeAndNil(FOctreeRendering);
  FreeAndNil(FOctreeDynamicCollisions);
  FreeAndNil(FOctreeVisibleTriangles);
  FreeAndNil(FOctreeCollidableTriangles);

  if OwnsRootNode then
    FreeAndNil(FRootNode) else
  begin
    { This will call UnregisterScene(RootNode). }
    Static := true;
    FRootNode := nil;
  end;

  inherited;
end;

procedure TCastleSceneCore.Load(ARootNode: TX3DRootNode; AOwnsRootNode: boolean;
  const AResetTime: boolean);
var
  RestoreProcessEvents: boolean;
begin
  { temporarily turn off events, to later initialize and turn them on }
  RestoreProcessEvents := ProcessEvents;
  ProcessEvents := false;
  BeforeNodesFree;
  PointingDeviceClear;
  if OwnsRootNode then FreeAndNil(FRootNode);

  RootNode := ARootNode;
  OwnsRootNode := AOwnsRootNode;
  ScheduledShadowMapsProcessing := true;

  { We can't call UpdateHeadlightOnFromNavigationInfo here,
    as NavigationInfoStack may contain now already freed nodes
    (testcase: view3dscene anchor_test and click on key_sensor anchor).
    So only schedule it. }
  ScheduleHeadlightOnFromNavigationInfoInChangedAll := true;

  { Disable smooth camera transitions up to the of next ChangedAll.
    This way initial Viewpoint at loading (this will also catch
    a viewpoint given as #viewpoint_name at LoadAnchor, see there for
    comments) will be set immediately. }
  ForceTeleportTransitions := true;

  ScheduleChangedAll;

  if AResetTime then
    ResetTimeAtLoad;

  { restore events processing, initialize new scripts and such }
  ProcessEvents := RestoreProcessEvents;
end;

procedure TCastleSceneCore.Load(const AFileName: string; AllowStdIn: boolean;
  const AResetTime: boolean);
begin
  { Note that if Load3D fails, we will not change the RootNode,
    so currently loaded scene will remain valid. }

  Load(Load3D(AFileName, AllowStdIn), true, AResetTime);

  FFileName := AFileName;
end;

procedure TCastleSceneCore.SetFileName(const AValue: string);
begin
  if AValue <> FFileName then
    Load(AValue);
end;

function TCastleSceneCore.ShapesActiveCount: Cardinal;
begin
  if not (fvShapesActiveCount in Validities) then
  begin
    FShapesActiveCount := Shapes.ShapesCount(true);
    Include(Validities, fvShapesActiveCount);
  end;
  Result := FShapesActiveCount;
end;

function TCastleSceneCore.ShapesActiveVisibleCount: Cardinal;
begin
  if not (fvShapesActiveVisibleCount in Validities) then
  begin
    FShapesActiveVisibleCount := Shapes.ShapesCount(true, true);
    Include(Validities, fvShapesActiveVisibleCount);
  end;
  Result := FShapesActiveVisibleCount;
end;

function TCastleSceneCore.CalculateBoundingBox: TBox3D;
var
  SI: TShapeTreeIterator;
begin
  Result := EmptyBox3D;
  SI := TShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result.Add(SI.Current.BoundingBox);
  finally FreeAndNil(SI) end;
end;

function TCastleSceneCore.CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
var
  SI: TShapeTreeIterator;
begin
  Result := 0;
  SI := TShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result += SI.Current.VerticesCount(OverTriangulate);
  finally FreeAndNil(SI) end;
end;

function TCastleSceneCore.CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
var
  SI: TShapeTreeIterator;
begin
  Result := 0;
  SI := TShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result += SI.Current.TrianglesCount(OverTriangulate);
  finally FreeAndNil(SI) end;
end;

function TCastleSceneCore.BoundingBox: TBox3D;
begin
  if GetExists then
  begin
    if not (fvBoundingBox in Validities) then
    begin
      FBoundingBox := CalculateBoundingBox;
      Include(Validities, fvBoundingBox);
    end;
    Result := FBoundingBox;
  end else
    Result := EmptyBox3D;
end;

function TCastleSceneCore.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
  begin
    if not (fvVerticesCountOver in Validities) then
    begin
      FVerticesCount[OverTriangulate] := CalculateVerticesCount(OverTriangulate);
      Include(Validities, fvVerticesCountOver);
    end;
  end else
  begin
    if not (fvVerticesCountNotOver in Validities) then
    begin
      FVerticesCount[OverTriangulate] := CalculateVerticesCount(OverTriangulate);
      Include(Validities, fvVerticesCountNotOver);
    end;
  end;
  Result := FVerticesCount[OverTriangulate];
end;

function TCastleSceneCore.TrianglesCount(OverTriangulate: boolean): Cardinal;
begin
  if OverTriangulate then
  begin
    if not (fvTrianglesCountOver in Validities) then
    begin
      FTrianglesCount[OverTriangulate] := CalculateTrianglesCount(OverTriangulate);
      Include(Validities, fvTrianglesCountOver);
    end;
  end else
  begin
    if not (fvTrianglesCountNotOver in Validities) then
    begin
      FTrianglesCount[OverTriangulate] := CalculateTrianglesCount(OverTriangulate);
      Include(Validities, fvTrianglesCountNotOver);
    end;
  end;
  Result := FTrianglesCount[OverTriangulate];
end;

function TCastleSceneCore.CreateShape(AGeometry: TAbstractGeometryNode;
  AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape;
begin
  Result := TShape.Create(Self, AGeometry, AState, ParentInfo);
end;

type
  { Traverses on ChangedAll event, single TChangedAllTraverser
    instance traverses into a single ShapesGroup (but may
    recursively create other TChangedAllTraverser instances
    to create recursive groups). }
  TChangedAllTraverser = class
    ParentScene: TCastleSceneCore;
    ShapesGroup: TShapeTreeGroup;
    Active: boolean;
    procedure Traverse(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

procedure TChangedAllTraverser.Traverse(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);

  { Handle ITransformNode node }
  procedure HandleTransform(TransformNode: TX3DNode);
  var
    TransformTree: TShapeTreeTransform;
    Traverser: TChangedAllTraverser;
  begin
    TransformTree := TShapeTreeTransform.Create(ParentScene);
    TransformTree.TransformNode := TransformNode;

    { We want to save at TransformState the state right before traversing
      inside this TransformNode.

      StateStack.Top is bad --- it is already modified by TransformNode
      transformation, we don't want this, the very purpose of TransformState
      is to later restart traversing from a TransformNode that changed
      it's transformation. Clearly, TransformState must not depend on
      current TransformNode transformation.

      So we cheat a little, knowing that internally every ITransformNode
      does StateStack.Push inside BeforeTraverse exactly once and then
      modifies transformation.(This happens for both TAbstractGroupingNode
      and THAnimHumanoidNode. Right now, ITransformNode is always one of those.)
      So we know that previous state lies safely at PreviousTop. }
    TransformTree.TransformState.Assign(StateStack.PreviousTop);

    ShapesGroup.Children.Add(TransformTree);

    { update ParentScene.TransformInstancesList }
    if TransformNode is TBillboardNode then
      ParentScene.BillboardInstancesList.Instances(TransformNode, true).Add(TransformTree) else
      ParentScene.TransformInstancesList.Instances(TransformNode, true).Add(TransformTree);

    Traverser := TChangedAllTraverser.Create;
    try
      Traverser.ParentScene := ParentScene;
      { No need to create another TShapeTreeGroup, like ChildGroup
        for Switch/LOD nodes. We can just add new shapes to our TransformTree.
        Reason: unlike Switch/LOD nodes, we don't care about keeping
        the indexes of children stable. }
      Traverser.ShapesGroup := TransformTree;
      Traverser.Active := Active;

      TransformNode.TraverseIntoChildren(StateStack, TX3DNode,
        @Traverser.Traverse, ParentInfo);
    finally FreeAndNil(Traverser) end;

    TraverseIntoChildren := false;
  end;

  procedure HandleSwitch(SwitchNode: TSwitchNode);
  var
    SwitchTree: TShapeTreeSwitch;
    Traverser: TChangedAllTraverser;
    ChildNode: TX3DNode;
    ChildGroup: TShapeTreeGroup;
    I: Integer;
  begin
    SwitchTree := TShapeTreeSwitch.Create(ParentScene);
    SwitchTree.SwitchNode := SwitchNode;
    ShapesGroup.Children.Add(SwitchTree);

    for I := 0 to SwitchNode.FdChildren.Count - 1 do
    begin
      ChildNode := SwitchNode.FdChildren[I];
      ChildGroup := TShapeTreeGroup.Create(ParentScene);
      SwitchTree.Children.Add(ChildGroup);

      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := ParentScene;
        Traverser.ShapesGroup := ChildGroup;
        Traverser.Active := Active and (I = SwitchNode.FdWhichChoice.Value);
        ChildNode.TraverseInternal(StateStack, TX3DNode, @Traverser.Traverse,
          ParentInfo);
      finally FreeAndNil(Traverser) end;
    end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TAbstractLODNode);
  var
    LODTree: TShapeTreeLOD;
    Traverser: TChangedAllTraverser;
    ChildNode: TX3DNode;
    ChildGroup: TShapeTreeGroup;
    I: Integer;
  begin
    LODTree := TShapeTreeLOD.Create(ParentScene);
    LODTree.LODNode := LODNode;
    LODTree.LODInvertedTransform^ := StateStack.Top.InvertedTransform;
    ShapesGroup.Children.Add(LODTree);
    ParentScene.ShapeLODs.Add(LODTree);

    { First add TShapeTreeGroup.Create as children, as many times
      as there are LODNode.FdChildren. Reason: LODTree.CalculateLevel
      uses this Count. }

    for I := 0 to LODNode.FdChildren.Items.Count - 1 do
      LODTree.Children.Add(TShapeTreeGroup.Create(ParentScene));

    ParentScene.UpdateLODLevel(LODTree);

    for I := 0 to LODNode.FdChildren.Count - 1 do
    begin
      ChildNode := LODNode.FdChildren[I];
      ChildGroup := TShapeTreeGroup(LODTree.Children.Items[I]);

      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := ParentScene;
        Traverser.ShapesGroup := ChildGroup;
        Traverser.Active := Active and (Cardinal(I) = LODTree.Level);
        ChildNode.TraverseInternal(StateStack, TX3DNode, @Traverser.Traverse,
          ParentInfo);
      finally FreeAndNil(Traverser) end;
    end;

    TraverseIntoChildren := false;
  end;

  procedure HandleProximitySensor(const Node: TProximitySensorNode);
  var
    PSI: TProximitySensorInstance;
  begin
    PSI := TProximitySensorInstance.Create(ParentScene);
    PSI.Node := Node;
    PSI.InvertedTransform := StateStack.Top.InvertedTransform;
    PSI.IsActive := false; { IsActive = false initially }

    ShapesGroup.Children.Add(PSI);
    ParentScene.ProximitySensors.Add(PSI);
  end;

  procedure HandleVisibilitySensor(const Node: TVisibilitySensorNode);
  var
    VSI: TVisibilitySensorInstance;
    Instances: TVisibilitySensorInstanceList;
    Index: Integer;
  begin
    VSI := TVisibilitySensorInstance.Create(ParentScene);
    VSI.Node := Node;
    VSI.Transform := StateStack.Top.Transform;
    VSI.Box := Node.Box.Transform(VSI.Transform);

    ShapesGroup.Children.Add(VSI);

    { add to ParentScene.VisibilitySensors map }
    Index := ParentScene.VisibilitySensors.IndexOf(Node);
    if Index = -1 then
    begin
      Instances := TVisibilitySensorInstanceList.Create(false);
      ParentScene.VisibilitySensors.Add(Node, Instances);
    end else
      Instances := ParentScene.VisibilitySensors.Data[Index];
    Instances.Add(VSI);
  end;

var
  Shape: TShape;
begin
  if Node is TAbstractGeometryNode then
  begin
    { Add shape to Shapes }
    Shape := ParentScene.CreateShape(Node as TAbstractGeometryNode,
      TX3DGraphTraverseState.CreateCopy(StateStack.Top), ParentInfo);
    ShapesGroup.Children.Add(Shape);

    { When Spatial contain ssDynamicCollisions, then each collidable
      shape must have octree created. Normally, this is watched over by
      SetSpatial. In this case, we just created new Shape, so we have
      to set it's Spatial property correctly. }
    if (ssDynamicCollisions in ParentScene.Spatial) and
       Shape.Collidable then
    begin
      Shape.TriangleOctreeProgressTitle := ParentScene.TriangleOctreeProgressTitle;
      Shape.Spatial := [ssTriangles];
    end;
  end else

  if Node is TAbstractLightNode then
  begin
    (*Global lights within inactive Switch child are not active.
      Although I didn't find where specification explicitly says this,
      but it's natural. Allows e.g. to do

      Switch {
        DEF SomeGroup1 { .. some lights and shapes .. }
        DEF SomeGroup2 { .. some lights and shapes .. }
      }
      USE SomeGroup1

      which is a trick used e.g. by our Collada importer.
      Without this rule, SomeGroup1 would be affected be it's own lights
      *two* times (and in addition by SomeGroup2 lights).

      TODO: Changing things like Switch.FdWhichChoice should update
      global lights set. *)

    { Add lights to GlobalLights }
    if Active and (TAbstractLightNode(Node).Scope = lsGlobal) then
      ParentScene.GlobalLights.Add(
        TAbstractLightNode(Node).CreateLightInstance(StateStack.Top));
  end else

  if Node is TSwitchNode then
  begin
    HandleSwitch(TSwitchNode(Node));
  end else
  if Node is TAbstractLODNode then
  begin
    HandleLOD(TAbstractLODNode(Node));
  end else
  if Supports(Node, ITransformNode) then
  begin
    HandleTransform(Node);
  end else

  if (Node is TAbstractBindableNode) and
     { Do not look for first bindable node within inlined content,
       this is following VRML spec. }
     (StateStack.Top.InsideInline = 0) and
     { Do not look for first bindable node within inactive content.
       TODO: Actually, where does spec precisely say that first bindable node
       must be in active part? Although it seems most sensible... }
     Active then
  begin
    { If some bindable stack is empty, push the node to it.
      This way, upon reading VRML file, we will bind the first found
      node for each stack. }
    if Node is TAbstractBackgroundNode then
      ParentScene.BackgroundStack.PushIfEmpty( TAbstractBackgroundNode(Node), true) else
    if Node is TFogNode then
      ParentScene.FogStack.PushIfEmpty( TFogNode(Node), true) else
    if Node is TNavigationInfoNode then
      ParentScene.NavigationInfoStack.PushIfEmpty( TNavigationInfoNode(Node), true) else
    if Node is TAbstractViewpointNode then
    begin
      { before binding viewpoint, check InitialViewpoint* conditions }
      if (ParentScene.InitialViewpointName = '') or
         (ParentScene.InitialViewpointName = Node.NodeName) then
      begin
        if (ParentScene.InitialViewpointIndex =
            ParentScene.ChangedAllCurrentViewpointIndex) then
          ParentScene.ViewpointStack.PushIfEmpty( TAbstractViewpointNode(Node), true);
        Inc(ParentScene.ChangedAllCurrentViewpointIndex);
      end;
    end;
  end else
  if Node is TProximitySensorNode then
    HandleProximitySensor(Node as TProximitySensorNode) else
  if Node is TVisibilitySensorNode then
    HandleVisibilitySensor(Node as TVisibilitySensorNode) else
  if Node is TScreenEffectNode then
  begin
    TScreenEffectNode(Node).StateForShaderPrepare.Assign(StateStack.Top);
    ParentScene.ScreenEffectNodes.Add(Node);
  end;
end;

procedure TCastleSceneCore.UpdateLODLevel(LODTree: TShapeTreeLOD);
var
  OldLevel, NewLevel: Cardinal;
begin
  if CameraViewKnown then
  begin
    OldLevel := LODTree.Level;
    NewLevel := LODTree.CalculateLevel(CameraPosition);
    LODTree.Level := NewLevel;

    if ProcessEvents and
       ( (OldLevel <> NewLevel) or
         (not LODTree.WasLevel_ChangedSend) ) and
       { If LODTree.Children.Count = 0, then Level = 0, but we don't
         want to send Level_Changed since the children 0 doesn't really exist. }
       (LODTree.Children.Count <> 0) then
    begin
      LODTree.WasLevel_ChangedSend := true;
      LODTree.LODNode.EventLevel_Changed.Send(LongInt(NewLevel), Time);
    end;

    if OldLevel <> NewLevel then
    begin
      { This means that active shapes changed, so we have to change things
        depending on them. Just like after Switch.whichChoice change. }

      Validities := Validities - [
        { Calculation traverses over active shapes. }
        fvShapesActiveCount,
        fvShapesActiveVisibleCount,
        { Calculation traverses over active nodes (uses RootNode.Traverse). }
        fvMainLightForShadows];

      DoGeometryChanged(gcActiveShapesChanged, nil);
    end;
  end;
end;

procedure TCastleSceneCore.BeforeNodesFree(const InternalChangedAll: boolean);
begin
  { Stuff that will be recalculated by ChangedAll }
  TransformInstancesList.FreeShapeTrees;
  BillboardInstancesList.FreeShapeTrees;
  GeneratedTextures.Count := 0;
  ProximitySensors.Count := 0;
  VisibilitySensors.Clear;
  ScreenEffectNodes.Count := 0;
  ScheduledHumanoidAnimateSkin.Count := 0;
  KeyDeviceSensorNodes.Clear;
  TimeDependentHandlers.Clear;

  if not InternalChangedAll then
  begin
    { Clean Shapes, ShapeLODs }
    FreeAndNil(FShapes);
    FShapes := TShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;
  end;
end;

procedure TCastleSceneCore.NodeFreeRemovingFromAllParents(Node: TX3DNode);
begin
  BeforeNodesFree;
  Node.FreeRemovingFromAllParents;
  ChangedAll;
end;

procedure TCastleSceneCore.RemoveShapeGeometry(Shape: TShape);
begin
  { Do not use Shape.Geometry here, as it may be a temporary result
    of OriginalGeometry.Proxy.

    When the shape is freed (which happens in BeforeNodesFree called
    at the beginning of  NodeFreeRemovingFromAllParents),
    the proxy result is freed too. So using here Shape.Geometry
    would not only not free what you think, it would also cause segfault. }

  NodeFreeRemovingFromAllParents(Shape.OriginalGeometry);
end;

procedure TCastleSceneCore.ChangedAllEnumerateCallback(Node: TX3DNode);
begin
  if not Static then
    Node.Scene := Self;

  { We're using AddIfNotExists, not simple Add, below:

    - for time-dependent nodes (TimeSensor, MovieTexture etc.),
      duplicates would cause time to be then incremented many times
      during single SetTime, so their local time would grow too fast.

    - for other sensors, events would be passed twice.
  }

  if Node is TAbstractKeyDeviceSensorNode then
    KeyDeviceSensorNodes.AddIfNotExists(Node) else
  if Supports(Node, IAbstractTimeDependentNode) then
    TimeDependentHandlers.AddIfNotExists(
      (Node as IAbstractTimeDependentNode).TimeDependentNodeHandler);
end;

procedure TCastleSceneCore.ChangedAll;

  { Add where necessary lights with scope = global. }
  procedure AddGlobalLights;

    procedure AddLightEverywhere(const L: TLightInstance);
    var
      SI: TShapeTreeIterator;
    begin
      SI := TShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          SI.Current.State.AddLight(L);
      finally FreeAndNil(SI) end;
    end;

    { Add L everywhere within given Radius from Location.
      Note that this will calculate BoundingBox of every Shape
      (but that's simply unavoidable if you have scene with VRML 2.0
      positional lights). }
    procedure AddLightRadius(const L: TLightInstance;
      const Location: TVector3Single; const Radius: Single);
    var
      SI: TShapeTreeIterator;
    begin
      SI := TShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if SI.Current.BoundingBox.SphereCollision(Location, Radius) then
            SI.Current.State.AddLight(L);
      finally FreeAndNil(SI) end;
    end;

  var
    I: Integer;
    L: PLightInstance;
    LNode: TAbstractLightNode;
  begin
    { Here we only deal with light scope = lsGlobal case.
      Other scopes are handled during traversing. }

    for I := 0 to GlobalLights.Count - 1 do
    begin
      L := Addr(GlobalLights.L[I]);
      LNode := L^.Node;

      { TODO: for spot lights, it would be an optimization to also limit
        LightInstances by spot cone size. }

      if (LNode is TAbstractPositionalLightNode) and
         TAbstractPositionalLightNode(LNode).HasRadius then
        AddLightRadius(L^, L^.Location, L^.Radius) else
        AddLightEverywhere(L^);
    end;
  end;

  { Assigns nodes TX3DNode.Scene, and adds nodes to KeyDeviceSensorNodes
    and TimeDependentHandlers lists. }
  procedure ChangedAllEnumerate;
  begin
    if RootNode <> nil then
      RootNode.EnumerateNodes(TX3DNode, @ChangedAllEnumerateCallback, false);
  end;

var
  Traverser: TChangedAllTraverser;
begin
  { We really need to use Dirty here, to forbid rendering during this.

    For example, ProcessShadowMapsReceivers work assumes this:
    otherwise, RootNode.Traverse may cause some progress Step call
    which may call Render which may prepare GLSL shadow map shader
    that will be freed by the following ProcessShadowMapsReceivers call.
    Testcase: view3dscene open simple_shadow_map_teapots.x3dv, turn off
    shadow maps "receiveShadows" handling, then turn it back on
    --- will crash without "Dirty" variable safety. }
  Inc(Dirty);
  try

  if Log and LogChanges then
    WritelnLog('VRML changes', 'ChangedAll');

  BeforeNodesFree(true);

  { Call DoGeometryChanged already here, as our old shapes already
    stopped to exist. }
  DoGeometryChanged(gcAll, nil);

  { Delay calling OnBoundChanged from bindable stack changes.

    Reason: CheckForDeletedNodes below, or PushIfEmpty during traversing,
    should not cause immediate OnBoundChanged, as a receiver may expect
    us to be in initialized state (and e.g. use BoundingBox function).
    And before traversing finished, our Shapes tree is not calculated,
    so we're not ready to answer e.g. BoundingBox correctly.

    Besides, this was often causing two OnBoundChanged calls,
    while only one was needed. E.g. if your previous scene has
    a Viewpoint node, and newly loaded scene also has Viewpoint,
    then we would call OnBoundChanged twice (1st time by CheckForDeletedNodes). }
  BackgroundStack.BeginChangesSchedule;
  FogStack.BeginChangesSchedule;
  NavigationInfoStack.BeginChangesSchedule;
  ViewpointStack.BeginChangesSchedule;

  try
    BackgroundStack.CheckForDeletedNodes(RootNode, true);
    FogStack.CheckForDeletedNodes(RootNode, true);
    NavigationInfoStack.CheckForDeletedNodes(RootNode, true);
    ViewpointStack.CheckForDeletedNodes(RootNode, true);

    Validities := [];

    { Clear variables after removing fvTrianglesList* from Validities }
    InvalidateTrianglesListShadowCasters;

    { Clear variables after removing fvManifoldAndBorderEdges from Validities }
    InvalidateManifoldAndBorderEdges;

    { Clean Shapes and other stuff initialized by traversing }
    FreeAndNil(FShapes);
    FShapes := TShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;
    GlobalLights.Clear;

    if RootNode <> nil then
    begin
      ChangedAllCurrentViewpointIndex := 0;
      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := Self;
        { We just created FShapes as TShapeTreeGroup, so this cast
          is safe }
        Traverser.ShapesGroup := TShapeTreeGroup(FShapes);
        Traverser.Active := true;
        RootNode.Traverse(TX3DNode, @Traverser.Traverse);
      finally FreeAndNil(Traverser) end;

      AddGlobalLights;

      ChangedAllEnumerate;
    end;

    { Call DoGeometryChanged here, as our new shapes are added.
      Probably, only one DoGeometryChanged(gcAll) is needed, but for safety
      --- we can call it twice, it's ultra-fast right now. }
    DoGeometryChanged(gcAll, nil);

    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    DoViewpointsChanged;

  finally
    BackgroundStack.EndChangesSchedule;
    FogStack.EndChangesSchedule;
    NavigationInfoStack.EndChangesSchedule;
    ViewpointStack.EndChangesSchedule;
  end;

  if ScheduledShadowMapsProcessing then
  begin
    ProcessShadowMapsReceivers(RootNode, Shapes, ShadowMaps,
      ShadowMapsDefaultSize);
    ScheduledShadowMapsProcessing := false;
  end;

  { Add items to GeneratedTextures for this Shape, if it has any
    generated textures.

    Do this *after* ProcessShadowMapsReceivers was run
    (before ProcessShadowMapsReceivers we may have old GeneratedShadowMap nodes
    that will be freed during ProcessShadowMapsReceivers,
    and new nodes added by ProcessShadowMapsReceivers would be missing.)

    Note that clearing GeneratedTextures was already done at the beginning
    of ChangedAll (as part of BeforeNodesFree(true) call). }
  Shapes.EnumerateTextures(@GeneratedTextures.AddShapeTexture);

  if ScheduleHeadlightOnFromNavigationInfoInChangedAll then
  begin
    ScheduleHeadlightOnFromNavigationInfoInChangedAll := false;
    UpdateHeadlightOnFromNavigationInfo;
  end;

  ForceTeleportTransitions := false;

  if Log and LogShapes then
    WriteLogMultiline('Shapes tree', Shapes.DebugInfo);

  finally Dec(Dirty) end;
end;

type
  BreakTransformChangeFailed = class(TCodeBreaker)
    Reason: string;
    constructor Create(const AReason: string);
  end;
  BreakTransformChangeSuccess = class(TCodeBreaker);

constructor BreakTransformChangeFailed.Create(const AReason: string);
begin
  Reason := AReason;
end;

type
  { When Transform changes, we have to traverse Shapes tree simultaneously
    with traversing VRML graph. So we have to know at each point
    the TShapeTree we're on. To record this, we'll manage a linked list
    of PShapesParentInfo records.

    We will traverse Shapes tree knowing
    it's structure, since it must have been created by ChangedAll code. }

  PShapesParentInfo = ^TShapesParentInfo;
  TShapesParentInfo = record
    Group: TShapeTreeGroup;
    Index: Integer;
  end;

  { We need a separate class to keep various helpful state
    during traversal when Transform changed.
    Changing VRML >= 2.0 Transform fields must be highly optimized.

    Reason: one change of Transform may cause, during traversal,
    another change of Transform and another traversal.
    Consider e.g. proximity_sensor_transformed.x3dv: ProximitySensorUpdate
    causes sending of Position_Changed, which may be routed to something
    like Transfom.set_translation.

    So many TransformChange traversals may run at once, so they must
    have different state variables. }
  TTransformChangeHelper = class
    ParentScene: TCastleSceneCore;
    Shapes: PShapesParentInfo;
    ChangingNode: TX3DNode; {< must be also ITransformNode }
    AnythingChanged: boolean;
    Inside: boolean;
    { If = 0, we're in active or inactive graph part (we don't know).
      If > 0, we're in inactive graph part (TransformChangeTraverse
      may enter there, since our changing Transform node (or some of it's
      children) may be inactive; but we have to update all shapes,
      active or not). }
    Inactive: Cardinal;
    Changes: TX3DChanges;
    procedure TransformChangeTraverse(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

procedure TTransformChangeHelper.TransformChangeTraverse(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);

  { Handle ITransformNode }
  procedure HandleTransform(TransformNode: TX3DNode);
  var
    ShapeTransform: TShapeTreeTransform;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    if TransformNode = ChangingNode then
    begin
      if Inside and Log then
        WritelnLog('VRML transform', 'Cycle in VRML/X3D graph detected: transform node is a child of itself');
      Inside := true;
      { Nothing to do, in particular: do not enter inside.
        Our Shapes^.Group and Shapes^.Index is already correctly set
        at the inside of this transform by our HandleChangeTransform. }
      Exit;
    end;

    { get Shape and increase Shapes^.Index }
    ShapeTransform := Shapes^.Group.Children[Shapes^.Index] as TShapeTreeTransform;
    Inc(Shapes^.Index);

    { update transformation inside Transform nodes that are *within*
      the modified Transform node }
    ShapeTransform.TransformState.AssignTransform(StateStack.PreviousTop);

    OldShapes := Shapes;
    try
      { NewShapes group is just our ShapeTransform. Transform children do not
        have addition TShapeTreeGroup, unlike Switch/LOD nodes. }
      NewShapes.Group := ShapeTransform;
      NewShapes.Index := 0;
      Shapes := @NewShapes;

      TransformNode.TraverseIntoChildren(
        StateStack, TX3DNode, @TransformChangeTraverse, ParentInfo);
    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

  procedure HandleSwitch(SwitchNode: TSwitchNode);
  var
    I: Integer;
    ChildInactive: boolean;
    ShapeSwitch: TShapeTreeSwitch;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    { get Shape and increase Shapes^.Index }
    ShapeSwitch := Shapes^.Group.Children[Shapes^.Index] as TShapeTreeSwitch;
    Inc(Shapes^.Index);

    OldShapes := Shapes;
    try
      { We have to enter *every* Switch child (while normal
        Traverse would enter only active child), because changing Transform
        node may be in inactive graph parts. }

      for I := 0 to SwitchNode.FdChildren.Items.Count - 1 do
      begin
        NewShapes.Group := ShapeSwitch.Children[I] as TShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        ChildInactive := I <> SwitchNode.FdWhichChoice.Value;
        if ChildInactive then Inc(Inactive);

        SwitchNode.FdChildren.Items[I].TraverseInternal(
          StateStack, TX3DNode, @TransformChangeTraverse, ParentInfo);

        if ChildInactive then Dec(Inactive);
      end;

    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TAbstractLODNode);
  var
    I: Integer;
    ShapeLOD: TShapeTreeLOD;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    { get Shape and increase Shapes^.Index }
    ShapeLOD := Shapes^.Group.Children[Shapes^.Index] as TShapeTreeLOD;
    Inc(Shapes^.Index);

    { by the way, update LODInvertedTransform, since it changed }
    if Inside then
    begin
      ShapeLOD.LODInvertedTransform^ := StateStack.Top.InvertedTransform;
      ParentScene.UpdateLODLevel(ShapeLOD);
    end;

    OldShapes := Shapes;
    try
      { Just like in HandleSwitch: we have to enter *every* LOD child
        (while normal Traverse would enter only active child),
        because changing Transform  node may be in inactive graph parts. }

      for I := 0 to LODNode.FdChildren.Items.Count - 1 do
      begin
        NewShapes.Group := ShapeLOD.Children[I] as TShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        if Cardinal(I) <> ShapeLOD.Level then Inc(Inactive);

        LODNode.FdChildren.Items[I].TraverseInternal(
          StateStack, TX3DNode, @TransformChangeTraverse, ParentInfo);

        if Cardinal(I) <> ShapeLOD.Level then Dec(Inactive);
      end;

    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLight(LightNode: TAbstractLightNode);
  { When the transformation of light node changes, we should update every
    TLightInstance record of this light in every shape.

    TODO: code below updates too much, if the light was instantiated
    many times then only some occurrences should be updated, not all.

    TODO: for global lights, limited by radius field,
    we should also add / remove this light from some TX3DGraphTraverseState.Lights. }

    procedure HandleLightsList(List: TLightInstancesList);
    var
      I: Integer;
    begin
      if List <> nil then
        for I := 0 to List.Count - 1 do
          if List.L[I].Node = LightNode then
            LightNode.UpdateLightInstanceState(List.L[I], StateStack.Top);
    end;

  var
    SI: TShapeTreeIterator;
    Current: TShape;
  begin
    SI := TShapeTreeIterator.Create(ParentScene.Shapes, false);
    try
      while SI.GetNext do
      begin
        Current := SI.Current;
        HandleLightsList(Current.OriginalState.Lights);
        if Current.State(true) <> Current.OriginalState then
          HandleLightsList(Current.State(true).Lights);
        if Current.State(false) <> Current.OriginalState then
          HandleLightsList(Current.State(false).Lights);
        ParentScene.VisibleChangeHere([vcVisibleNonGeometry]);
      end;
    finally FreeAndNil(SI) end;

    { force update of GeneratedShadowMap textures that used this light }
    ParentScene.GeneratedTextures.UpdateShadowMaps(LightNode);
  end;

  procedure HandleProximitySensor(Node: TProximitySensorNode);
  var
    Instance: TProximitySensorInstance;
  begin
    Check(Shapes^.Index < Shapes^.Group.Children.Count,
      'Missing shape in Shapes tree');
    Instance := Shapes^.Group.Children[Shapes^.Index] as TProximitySensorInstance;
    Inc(Shapes^.Index);

    Instance.InvertedTransform := StateStack.Top.InvertedTransform;

    { We only care about ProximitySensor in active graph parts.

      TODO: (Inactive = 0) does not guarantee that we're in active part,
      it only says we're *possibly* in an active part, and we cannot fix it
      (without sacrificing transform optimization).
      This is bad, it means we make ProximitySensor events also for
      sensors in inactive graph parts. Although, should we really
      look at this? Maybe ProximitySensor ignore active/inactive,
      and we should just remove the test for "(Inactive = 0)" and that's it? }
    if Inactive = 0 then
    begin
      { Call ProximitySensorUpdate, since the sensor's box is transformed,
        so possibly it should be activated/deactivated,
        position/orientation_changed called etc. }
      if ParentScene.CameraViewKnown then
        ParentScene.ProximitySensorUpdate(Instance);
    end;
  end;

  procedure HandleVisibilitySensor(Node: TVisibilitySensorNode);
  var
    Instance: TVisibilitySensorInstance;
  begin
    Check(Shapes^.Index < Shapes^.Group.Children.Count,
      'Missing shape in Shapes tree');
    Instance := Shapes^.Group.Children[Shapes^.Index] as TVisibilitySensorInstance;
    Inc(Shapes^.Index);
    Instance.Transform := StateStack.Top.Transform;
    Instance.Box := Node.Box.Transform(Instance.Transform);
  end;

var
  Shape: TShape;
begin
  case Node.TransformationChange of
    ntcNone: ;
    ntcSwitch: HandleSwitch(TSwitchNode(Node));
    ntcLOD: HandleLOD(TAbstractLODNode(Node));
    ntcTransform: HandleTransform(Node);
    ntcGeometry:
      begin
        { get Shape and increase Shapes^.Index }
        Check(Shapes^.Index < Shapes^.Group.Children.Count,
          'Missing shape in Shapes tree');
        Shape := Shapes^.Group.Children[Shapes^.Index] as TShape;
        Inc(Shapes^.Index);

        Shape.State.AssignTransform(StateStack.Top);
        { Changes = [chTransform] here, good for roShapeDisplayList
          optimization. }
        Shape.Changed(Inactive <> 0, Changes);

        if Inactive = 0 then
        begin
          if Shape.Visible then
            ParentScene.DoGeometryChanged(gcVisibleTransformChanged, nil);

          if Shape.Collidable then
            ParentScene.DoGeometryChanged(gcCollidableTransformChanged, nil);

          AnythingChanged := true;
        end;
      end;
    ntcBackground:
      begin
        { Just make redraw. It will redraw the background with
          new transform correctly. }
        AnythingChanged := true;
      end;
    ntcFog:
      begin
        { There's no need to do anything more here.
          Fog node TransformScale was already updated by
          TAbstractBindableNode.BeforeTraverse.
          Renderer in TCastleScene will detect that TransformScale changed,
          and eventually destroy display lists and such when rendering next time. }
        if Inactive = 0 then
          ParentScene.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
      end;
    ntcViewpoint:
      begin
        if Node = ParentScene.ViewpointStack.Top then
          ParentScene.DoBoundViewpointVectorsChanged;

        { TODO: Transformation of viewpoint should also affect NavigationInfo,
          according to spec: "The speed, avatarSize and visibilityLimit values
          are all scaled by the transformation being applied to
          the currently bound X3DViewpointNode node."
          When this will be implemented, then also when transformation
          of viewpoint changes here we'll have to do something. }
      end;
    ntcLight: HandleLight(TAbstractLightNode(Node));
    ntcProximitySensor: HandleProximitySensor(TProximitySensorNode(Node));
    ntcVisibilitySensor: HandleVisibilitySensor(TVisibilitySensorNode(Node));
    else raise EInternalError.Create('HandleTransform: NodeTransformationChange?');
  end;
end;

procedure TCastleSceneCore.TransformationChanged(TransformNode: TX3DNode;
  Instances: TShapeTreeList; const Changes: TX3DChanges);
var
  TransformChangeHelper: TTransformChangeHelper;
  TransformShapesParentInfo: TShapesParentInfo;
  TraverseStack: TX3DGraphTraverseStateStack;
  I: Integer;
  TransformShapeTree: TShapeTreeTransform;
  DoVisibleChanged: boolean;
begin
  { This is the optimization for changing VRML >= 2.0 transformation
    (most fields of Transform node like translation, scale, center etc.,
    also some HAnim nodes like Joint and Humanoid have this behavior.).

    In the simple cases, Transform node simply changes
    TX3DGraphTraverseState.Transform for children nodes.

    So we have to re-traverse from this Transform node, and change
    states of affected children. Our TransformNodesInfo gives us
    a list of TShapeTreeTransform corresponding to this transform node,
    so we know we can traverse from this point.

    In more difficult cases, children of this Transform node may
    be affected in other ways by transformation. For example,
    Fog and Background nodes are affected by their parents transform.
    Currently, we cannot account for this, and just raise
    BreakTransformChangeFailed in this case --- we have to do ChangedAll
    in such cases.
  }

  if Log and LogChanges then
    WritelnLog('VRML changes', Format('Transform node %s change: %d instances',
      [TransformNode.NodeTypeName, Instances.Count]));

  try
    DoVisibleChanged := false;

    TraverseStack := nil;
    TransformChangeHelper := nil;
    try
      TraverseStack := TX3DGraphTraverseStateStack.Create;

      { initialize TransformChangeHelper, set before the loop properties
        that cannot change }
      TransformChangeHelper := TTransformChangeHelper.Create;
      TransformChangeHelper.ParentScene := Self;
      TransformChangeHelper.ChangingNode := TransformNode;
      TransformChangeHelper.Changes := Changes;

      for I := 0 to Instances.Count - 1 do
      begin
        TransformShapeTree := Instances[I] as TShapeTreeTransform;
        TraverseStack.Clear;
        TraverseStack.Push(TransformShapeTree.TransformState);

        TransformShapesParentInfo.Group := TransformShapeTree;
        TransformShapesParentInfo.Index := 0;

        { initialize TransformChangeHelper properties that may be changed
          during Node.Traverse later }
        TransformChangeHelper.Shapes := @TransformShapesParentInfo;
        TransformChangeHelper.AnythingChanged := false;
        TransformChangeHelper.Inside := false;
        TransformChangeHelper.Inactive := 0;

        try
          TransformNode.TraverseInternal(TraverseStack, TX3DNode,
            @TransformChangeHelper.TransformChangeTraverse, nil);
        except
          on BreakTransformChangeSuccess do
            { BreakTransformChangeSuccess is equivalent with normal finish
              of Traverse. So do nothing, just silence exception. }
        end;

        if TransformChangeHelper.AnythingChanged then
          DoVisibleChanged := true;

        { take care of calling THAnimHumanoidNode.AnimateSkin when joint is
          animated. Secure from Humanoid = nil (may happen if Joint
          is outside Humanoid node, see VRML 97 test
          ~/3dmodels/vrmlx3d/hanim/tecfa.unige.ch/vrml/objects/avatars/blaxxun/kambi_hanim_10_test.wrl)  }
        if (TransformNode is THAnimJointNode) and
           (TransformShapeTree.TransformState.Humanoid <> nil) then
          ScheduledHumanoidAnimateSkin.AddIfNotExists(
            TransformShapeTree.TransformState.Humanoid);
      end;
    finally
      FreeAndNil(TraverseStack);
      FreeAndNil(TransformChangeHelper);
    end;

    if DoVisibleChanged then
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  except
    on B: BreakTransformChangeFailed do
    begin
      if Log and LogChanges then
        WritelnLog('VRML changes', 'Transform change (because of child: ' + B.Reason + ') causes ChangedAll (no optimized action)');
      ScheduleChangedAll;
      Exit;
    end;
  end;
end;

procedure TCastleSceneCore.ChangedField(Field: TX3DField);
var
  ANode: TX3DNode;
  Changes: TX3DChanges;

  procedure DoLogChanges(const Additional: string = '');
  var
    S: string;
  begin
    S := 'ChangedField: ' + X3DChangesToStr(Changes) +
      Format(', node: %s (%s %s) at %s',
      [ ANode.NodeName, ANode.NodeTypeName, ANode.ClassName, PointerToStr(ANode) ]);
    if Field <> nil then
      S += Format(', field %s (%s)', [ Field.Name, Field.TypeName ]);
    if Additional <> '' then
      S += '. ' + Additional;
    WritelnLog('VRML changes', S);
  end;

  { Handle VRML >= 2.0 transformation changes. }
  procedure HandleChangeTransform;
  var
    Instances: TShapeTreeList;
  begin
    Check(Supports(ANode, ITransformNode),
      'chTransform flag may be set only for ITransformNode');

    Instances := TransformInstancesList.Instances(ANode, false);
    if Instances = nil then
    begin
      if Log and LogChanges then
        WritelnLog('VRML changes', Format('Transform node "%s" has no information, assuming does not exist in our VRML graph',
          [ANode.NodeTypeName]));
      Exit;
    end;

    TransformationChanged(ANode, Instances, Changes);
  end;

  procedure HandleChangeCoordinate;
  var
    Coord: TMFVec3f;
    SI: TShapeTreeIterator;
  begin
    { TCoordinateNode is special, although it's part of VRML 1.0 state,
      it can also occur within coordinate-based nodes of VRML >= 2.0.
      So it affects coordinate-based nodes with this node.

      In fact, code below takes into account both VRML 1.0 and 2.0 situation.
      That's why chCoordinate should not be used with chVisibleVRML1State
      --- chVisibleVRML1State handling is not needed after this. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.Geometry.Coord(SI.Current.State, Coord) and
           (Coord = Field) then
          SI.Current.Changed(false, Changes);

      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    finally FreeAndNil(SI) end;
  end;

  { Good for both chVisibleVRML1State and chGeometryVRML1State
    (TShape.Changed actually cares about the difference between these two.) }
  procedure HandleVRML1State;
  var
    VRML1StateNode: TVRML1StateNode;
    SI: TShapeTreeIterator;
  begin
    if ANode.VRML1StateNode(VRML1StateNode) then
    begin
      { ANode is part of VRML 1.0 state, so it affects Shapes where
        it's present on State.LastNodes list. }
      SI := TShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if (SI.Current.State.LastNodes.Nodes[VRML1StateNode] = ANode) or
             (SI.Current.OriginalState.LastNodes.Nodes[VRML1StateNode] = ANode) then
            SI.Current.Changed(false, Changes);
      finally FreeAndNil(SI) end;
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeMaterial;
  var
    SI: TShapeTreeIterator;
  begin
    { VRML 2.0 Material affects only shapes where it's
      placed inside Appearance.material field. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.State.ShapeNode.Material = ANode then
          SI.Current.Changed(false, Changes);
    finally FreeAndNil(SI) end;
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeLightInstanceProperty;
  var
    J: integer;
    SI: TShapeTreeIterator;
    LightInstance: PLightInstance;
    LightNode: TAbstractLightNode;
  begin
    LightNode := ANode as TAbstractLightNode;

    { Update all TLightInstance records with LightNode = this ANode.

      TODO: what if some TX3DGraphTraverseState.Lights need to be updated?
      Code below fails for this.

      To be fixed (at the same time taking into account that in X3D
      "global" is exposed field and so may change during execution) by
      constructing TX3DGraphTraverseState.Lights always with global = TRUE assumption.
      RenderShapeLights will have to take care of eventual "radius"
      constraints. Or not --- this will hurt performance, global = FALSE
      is a good optimization for local lights, we don't want long lights list. }

    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.State.Lights <> nil then
          for J := 0 to SI.Current.State.Lights.Count - 1 do
          begin
            LightInstance := Addr(SI.Current.State.Lights.L[J]);
            if LightInstance^.Node = LightNode then
            begin
              LightNode.UpdateLightInstance(LightInstance^);
              VisibleChangeHere([vcVisibleNonGeometry]);
            end;
          end;
    finally FreeAndNil(SI) end;

    GeneratedTextures.UpdateShadowMaps(LightNode);
  end;

  procedure HandleChangeLightForShadowVolumes;
  begin
    { When some kambiShadows or kambiShadowsMain field changes,
      then MainLightForShadows must be recalculated. }
    Exclude(Validities, fvMainLightForShadows);
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeLightLocationDirection;
  begin
    { If we had calculated MainLightForShadows, and this ANode is the
      main light for shadows, then update FMainLightForShadows.
      Thanks to varius FMainLightForShadows* properties, we can check
      and recalculate it very fast --- this is good for scenes where main
      shadow light location is moving. }

    if (fvMainLightForShadows in Validities) and
       FMainLightForShadowsExists and
       (FMainLightForShadowsNode = ANode) then
    begin
      CalculateMainLightForShadowsPosition;
      VisibleChangeHere([vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeSwitch2;
  begin
    { Changing Switch.whichChoice changes the shapes that are considered
      active (in VRML graph, and in Shapes tree).
      This means that things calculated based
      on traverse/iterator over Shapes with "OnlyActive = true"
      are invalid now.

      DoGeometryChanged (scheduled by ScheduleGeometryChanged)
      actually does most of this work, it invalidates already most
      of the needed things when ScheduledGeometryActiveShapesChanged:

      fvBoundingBox,
      fvVerticesCountNotOver, fvVerticesCountOver,
      fvTrianglesCountNotOver, fvTrianglesCountOver,
      fvTrianglesListShadowCasters,
      fvManifoldAndBorderEdges
    }

    Validities := Validities - [
      { Calculation traverses over active shapes. }
      fvShapesActiveCount,
      fvShapesActiveVisibleCount,
      { Calculation traverses over active nodes (uses RootNode.Traverse). }
      fvMainLightForShadows];

    DoGeometryChanged(gcActiveShapesChanged, nil);

    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeColorNode;
  var
    SI: TShapeTreeIterator;
  begin
    { Affects all geometry nodes with "color" field referencing this node.

      Note: also TParticleSystemNode may have color in FdcolorRamp field.
      This is not detected for now, and doesn't matter (we do not handle
      particle systems at all now). }

    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if ((SI.Current.Geometry is TAbstractComposedGeometryNode) and (TAbstractComposedGeometryNode(SI.Current.Geometry).FdColor.Value = ANode)) or
           ((SI.Current.Geometry is TIndexedLineSetNode          ) and (TIndexedLineSetNode          (SI.Current.Geometry).FdColor.Value = ANode)) or
           ((SI.Current.Geometry is TLineSetNode                 ) and (TLineSetNode                 (SI.Current.Geometry).FdColor.Value = ANode)) or
           ((SI.Current.Geometry is TPointSetNode                ) and (TPointSetNode                (SI.Current.Geometry).FdColor.Value = ANode)) or
           ((SI.Current.Geometry is TElevationGridNode           ) and (TElevationGridNode           (SI.Current.Geometry).FdColor.Value = ANode)) then
          SI.Current.Changed(false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeTextureCoordinate;
  var
    SI: TShapeTreeIterator;
    TexCoord: TX3DNode;
  begin
    { VRML 2.0 TextureCoordinate affects only shapes where it's
      placed inside texCoord field. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.Geometry.TexCoord(SI.Current.State, TexCoord) and
           (TexCoord = ANode) then
          SI.Current.Changed(false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeTextureTransform;

    { If Appearance.TextureTransform contains TextureTransform
      (which should be some TextureTransform* VRML/X3D node,
      but not nil and not TMultiTextureTransformNode). }
    function AppearanceUsesTextureTransform(Appearance: TAppearanceNode;
      TextureTransform: TX3DNode): boolean;
    var
      MultiTrans: TMFNode;
      I: Integer;
    begin
      Result := Appearance.FdTextureTransform.Value = TextureTransform;
      if (not Result) and
         (Appearance.FdTextureTransform.Value <> nil) and
         (Appearance.FdTextureTransform.Value is TMultiTextureTransformNode) then
      begin
        MultiTrans := TMultiTextureTransformNode(
          Appearance.FdTextureTransform.Value).FdTextureTransform;
        for I := 0 to MultiTrans.Count - 1 do
          if MultiTrans[I] = TextureTransform then
            Exit(true);
      end;
    end;

  var
    SI: TShapeTreeIterator;
  begin
    { VRML 2.0 / X3D TextureTransform* affects only shapes where it's
      placed inside textureTransform field. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if (SI.Current.State.ShapeNode <> nil) and
           (SI.Current.State.ShapeNode.FdAppearance.Value <> nil) and
           (SI.Current.State.ShapeNode.FdAppearance.Value is TAppearanceNode) and
           AppearanceUsesTextureTransform(
             TAppearanceNode(SI.Current.State.ShapeNode.FdAppearance.Value), ANode) then
          SI.Current.Changed(false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeGeometry;
  var
    SI: TShapeTreeIterator;
  begin
    { Geometry nodes, affect only shapes that use them. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if (SI.Current.Geometry = ANode) or
           (SI.Current.OriginalGeometry = ANode) then
          SI.Current.Changed(false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeEnvironmentalSensorBounds;
  var
    I: Integer;
    VSInstances: TVisibilitySensorInstanceList;
    VS: TVisibilitySensorNode;
  begin
    if ANode is TProximitySensorNode then
    begin
      { Update state for this ProximitySensor node. }
      if CameraViewKnown then
        for I := 0 to ProximitySensors.Count - 1 do
        begin
          if ProximitySensors[I].Node = ANode then
            ProximitySensorUpdate(ProximitySensors[I]);
        end;
    end else
    if ANode is TVisibilitySensorNode then
    begin
      VS := TVisibilitySensorNode(ANode);
      { local Box of this node changed,
        so update transformed Box in all TVisibilitySensorInstance for this node }
      I := VisibilitySensors.IndexOf(VS);
      if I <> -1 then
      begin
        VSInstances := VisibilitySensors.Data[I];
        for I := 0 to VSInstances.Count - 1 do
          VSInstances[I].Box := VS.Box.Transform(VSInstances[I].Transform);
      end;
    end;
  end;

  procedure HandleChangeTimeStopStart;

    function GetTimeDependentNodeHandler(ANode: TX3DNode): TTimeDependentNodeHandler;
    begin
      if Supports(ANode, IAbstractTimeDependentNode) then
        Result := (ANode as IAbstractTimeDependentNode).TimeDependentNodeHandler else
        Result := nil;
    end;

  var
    Handler: TTimeDependentNodeHandler;
  begin
    Handler := GetTimeDependentNodeHandler(ANode);
    if Handler = nil then Exit; {< ANode not time-dependent. }

    { Although (de)activation of time-dependent nodes will be also caught
      by the nearest IncreaseTime run, it's good to explicitly
      call SetTime to catch it here.

      Reason: if cycleInterval is very small, and not looping, then
      we could "miss" the fact that node should be activated (if it
      was inactive, and next IncreaseTime will happen after
      cycleInterval time already passed).

      Code below will make sure that no matter how small cycleInterval,
      no matter how seldom IncreaseTime occur, the node will get
      activated when doing something like startTime := Time.

      Note that we don't reset time here (ResetTime = false), otherwise
      we would mistakenly interpret resuming (from paused state) just like
      activation (from stopped state), testcase time_sensor_3.x3dv.  }

    Handler.SetTime(Time, Time, 0, false);

    { No need to do VisibleChangeHere.
      Redisplay will be done by next IncreaseTime run, if active now. }
  end;

  procedure HandleChangeViewpointVectors;
  begin
    if ANode = ViewpointStack.Top then
      DoBoundViewpointVectorsChanged;
      { Nothing needs to be done if
        - non-bound viewpoint changed,
        - or a field of bound viewpoint that doesn't affect it's vectors. }
  end;

  { Handle chTextureImage, chTextureRendererProperties }
  procedure HandleChangeTextureImageOrRenderer;
  var
    SI: TShapeTreeIterator;
  begin
    if chTextureImage in Changes then
    begin
      { On change of TAbstractTexture2DNode field that changes the result of
        TAbstractTexture2DNode.LoadTextureData, we have to explicitly release
        old texture (otherwise, LoadTextureData will not be called
        to reload the texture). }
      if ANode is TAbstractTexture2DNode then
        TAbstractTexture2DNode(ANode).IsTextureLoaded := false;
      if ANode is TAbstractTexture3DNode then
        TAbstractTexture3DNode(ANode).TextureLoaded := false;
    end;

    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if SI.Current.UsesTexture(TAbstractTextureNode(ANode)) then
          SI.Current.Changed(false, Changes);
      end;
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeShadowCasters;
  begin
    { When Appearance.shadowCaster field changed, then
      TrianglesListShadowCasters and Manifold/BorderEdges change. }
    InvalidateTrianglesListShadowCasters;
    InvalidateManifoldAndBorderEdges;
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeGeneratedTextureUpdateNeeded;
  var
    Handler: TGeneratedTextureHandler;
  begin
    if ANode is TGeneratedCubeMapTextureNode then
      Handler := TGeneratedCubeMapTextureNode(ANode).GeneratedTextureHandler else
    if ANode is TGeneratedShadowMapNode then
      Handler := TGeneratedShadowMapNode(ANode).GeneratedTextureHandler else
    if ANode is TRenderedTextureNode then
      Handler := TRenderedTextureNode(ANode).GeneratedTextureHandler else
      Exit;

    Handler.UpdateNeeded := true;
    VisibleChangeHere([]);
  end;

  procedure HandleChangeHeadLightOn;
  begin
    { Recalculate HeadlightOn based on NavigationInfo.headlight. }
    UpdateHeadlightOnFromNavigationInfo;
    VisibleChangeHere([]);
  end;

  procedure HandleChangeClipPlane;
  var
    SI: TShapeTreeIterator;
  begin
    Assert(ANode is TClipPlaneNode);

    { Call TShape.Changed for all shapes using this ClipPlane node. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if (SI.Current.State.ClipPlanes <> nil) and
           (SI.Current.State.ClipPlanes.IndexOfNode(TClipPlaneNode(ANode)) <> -1) then
          SI.Current.Changed(false, Changes);
      end;
    finally FreeAndNil(SI) end;
    VisibleChangeHere([vcVisibleGeometry]);
  end;

  procedure HandleChangeEverything;
  begin
    { An arbitrary change occurred. }
    ScheduleChangedAll;
  end;

  { Handle all four flags chVisibleGeometry, chVisibleNonGeometry,
    chCamera, chRedisplay. }
  procedure HandleVisibleChange;
  var
    VisibleChanges: TVisibleChanges;
  begin
    VisibleChanges := [];
    if chVisibleGeometry    in Changes then Include(VisibleChanges, vcVisibleGeometry);
    if chVisibleNonGeometry in Changes then Include(VisibleChanges, vcVisibleNonGeometry);
    if chCamera             in Changes then Include(VisibleChanges, vcCamera);
    VisibleChangeHere(VisibleChanges);
  end;

  procedure HandleChangeDragSensorEnabled;
  var
    Enabled: boolean;
    DragSensor: TAbstractDragSensorNode;
  begin
    Enabled := (Field as TSFBool).Value;
    DragSensor := ANode as TAbstractDragSensorNode;

    { When we disable an active drag sensor, specification says to
      deactivate it. This cannot be handled fully by TAbstractDragSensorNode
      implementation, because we have to remove it from our property
      PointingDeviceActiveSensors. }

    if (not Enabled) and (PointingDeviceActiveSensors.IndexOf(DragSensor) <> -1) then
    begin
      DragSensor.Deactivate(Time);
      FPointingDeviceActiveSensors.Remove(DragSensor);
      DoPointingDeviceSensorsChange;
    end;
  end;

  procedure HandleChangeNavigationInfo;
  begin
    if ANode = NavigationInfoStack.Top then
      DoBoundNavigationInfoFieldsChanged;
  end;

  procedure HandleChangeScreenEffectEnabled;
  var
    SE: TScreenEffectNode;
  begin
    SE := ANode as TScreenEffectNode;
    { Just like TCastleScene.CloseGLScreenEffect: no need to even
      communicate with renderer, just reset ShaderLoaded and Shader.
      At the nearest time, it will be recalculated. }
    SE.ShaderLoaded := false;
    SE.Shader := nil;
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeBackground;
  begin
    InvalidateBackground;
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeShadowMaps;
  begin
    if ShadowMaps then { if not ShadowMaps, then no need to reprocess }
    begin
      ScheduledShadowMapsProcessing := true;
      ScheduleChangedAll;
    end;
  end;

begin
  ANode := TX3DNode(Field.ParentNode);
  Assert(ANode <> nil);

  { We used to check here RootNode.IsNodePresent, to eliminate
    changes to nodes not in our graph. This is not done now, because:

    1. This check is not usually needed, and usually it wastes quite
       some time (for example, profile animate_3d_model_by_code_2 example).

       In most cases, when modifying graph by code, and always when
       modifying graph by VRML/X3D events, the Node is known to be inside
       our VRML/X3D graph...

    2. Also, there are nodes that affect our graph but are outside
       of it: StateDefaultNodes, and also all the nodes created
       by Proxy methods (geometry and new state nodes).
  }

  Changes := Field.Changes;

  if Log and LogChanges then
    DoLogChanges;

  { Optimize Changes = [] case: no need even for Begin/EndChangesSchedule }
  if Changes = [] then Exit;

  BeginChangesSchedule;
  try
    if chTransform in Changes then HandleChangeTransform;
    if chCoordinate in Changes then HandleChangeCoordinate;
    if Changes * [chVisibleVRML1State, chGeometryVRML1State] <> [] then
      HandleVRML1State;
    if chMaterial2 in Changes then HandleChangeMaterial;
    if chLightInstanceProperty  in Changes then HandleChangeLightInstanceProperty;
    if chLightForShadowVolumes  in Changes then HandleChangeLightForShadowVolumes;
    if chLightLocationDirection in Changes then HandleChangeLightLocationDirection;
    if chSwitch2 in Changes then HandleChangeSwitch2;
    if chColorNode in Changes then HandleChangeColorNode;
    if chTextureCoordinate in Changes then HandleChangeTextureCoordinate;
    if chTextureTransform in Changes then HandleChangeTextureTransform;
    if chGeometry in Changes then HandleChangeGeometry;
    if chEnvironmentalSensorBounds in Changes then HandleChangeEnvironmentalSensorBounds;
    if chTimeStopStart in Changes then HandleChangeTimeStopStart;
    if chViewpointVectors in Changes then HandleChangeViewpointVectors;
    { TODO: if chViewpointProjection then HandleChangeViewpointProjection }
    if Changes * [chTextureImage, chTextureRendererProperties] <> [] then
      HandleChangeTextureImageOrRenderer;
    { TODO: chTexturePropertiesNode }
    if chShadowCasters in Changes then HandleChangeShadowCasters;
    if chGeneratedTextureUpdateNeeded in Changes then HandleChangeGeneratedTextureUpdateNeeded;
    { TODO: chFontStyle. Fortunately, FontStyle fields are not exposed,
      so this isn't a bug in vrml/x3d browser. }
    if chHeadLightOn in Changes then HandleChangeHeadLightOn;
    if chClipPlane in Changes then HandleChangeClipPlane;
    if chDragSensorEnabled in Changes then HandleChangeDragSensorEnabled;
    if chNavigationInfo in Changes then HandleChangeNavigationInfo;
    if chScreenEffectEnabled in Changes then HandleChangeScreenEffectEnabled;
    if chBackground in Changes then HandleChangeBackground;
    if chEverything in Changes then HandleChangeEverything;
    if chShadowMaps in Changes then HandleChangeShadowMaps;

    if Changes * [chVisibleGeometry, chVisibleNonGeometry,
      chCamera, chRedisplay] <> [] then
      HandleVisibleChange;
  finally EndChangesSchedule end;
end;

procedure TCastleSceneCore.DoGeometryChanged(const Change: TGeometryChange;
  LocalGeometryShape: TShape);
var
  SomeLocalGeometryChanged: boolean;
  EdgesStructureChanged: boolean;
begin
  Validities := Validities - [fvBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvTrianglesListShadowCasters];

  { Clear variables after removing fvTrianglesList* }
  InvalidateTrianglesListShadowCasters;

  { First, call LocalGeometryChanged(true, ...) on shapes when needed.

    By the way, also calculate SomeLocalGeometryChanged (= if any
    LocalGeometryChanged was called, which means that octree and
    bounding box/sphere of some shape changed).

    Note that this also creates implication ScheduledGeometryChangedAll
    => SomeLocalGeometryChanged. In later code, I sometimes check
    for SomeLocalGeometryChanged, knowing that this also checks for
    ScheduledGeometryChangedAll.

    By the way, also calculate EdgesStructureChanged. }

  if Change = gcAll then
  begin
    SomeLocalGeometryChanged := true;
    EdgesStructureChanged := true;

    { No need to do here LocalGeometryChanged on all shapes:
      we know that ChangedAll already did (or will, soon) recreate
      all the shapes. So their geometry stuff will be correctly reinitialized
      anyway. }
  end else
  begin
    { Note that if
      ScheduledLocalGeometryChangedCoord = true, but
      ScheduledLocalGeometryChanged = false, then
      EdgesStructureChanged may remain false. This is the very reason
      for     ScheduledLocalGeometryChangedCoord separation from
      regular ScheduledLocalGeometryChanged. }

    SomeLocalGeometryChanged := Change in
      [gcLocalGeometryChanged, gcLocalGeometryChangedCoord];
    EdgesStructureChanged := Change in
      [gcActiveShapesChanged, gcLocalGeometryChanged];
  end;

  { Use EdgesStructureChanged to decide should be invalidate
    ManifoldAndBorderEdges. }
  if EdgesStructureChanged then
    InvalidateManifoldAndBorderEdges;

  if (FOctreeRendering <> nil) and
     ((Change in [gcVisibleTransformChanged, gcActiveShapesChanged]) or
      SomeLocalGeometryChanged) then
    FreeAndNil(FOctreeRendering);

  if (FOctreeDynamicCollisions <> nil) and
     ((Change in [gcCollidableTransformChanged, gcActiveShapesChanged]) or
      SomeLocalGeometryChanged) then
    FreeAndNil(FOctreeDynamicCollisions);

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self, SomeLocalGeometryChanged,
      { We know LocalGeometryShape is nil now for Change not in
        gcLocalGeometryChanged*. }
      LocalGeometryShape);
end;

procedure TCastleSceneCore.DoViewpointsChanged;
begin
  if Assigned(OnViewpointsChanged) then
    OnViewpointsChanged(Self);
end;

procedure TCastleSceneCore.DoBoundViewpointVectorsChanged;
begin
  if Assigned(OnBoundViewpointVectorsChanged) then
    OnBoundViewpointVectorsChanged(Self);
end;

procedure TCastleSceneCore.DoBoundNavigationInfoFieldsChanged;
begin
  if Assigned(OnBoundNavigationInfoFieldsChanged) then
    OnBoundNavigationInfoFieldsChanged(Self);
end;

resourcestring
  SSceneInfoTriVertCounts_Same = 'Scene contains %d triangles and %d ' +
    'vertices (with and without over-triangulating).';
  SSceneInfoTriVertCounts_1 =
    'When we don''t use over-triangulating (e.g. when we do collision '+
    'detection or ray tracing) scene has %d triangles and %d vertices.';
  SSceneInfoTriVertCounts_2 =
    'When we use over-triangulating (e.g. when we do OpenGL rendering) '+
    'scene has %d triangles and %d vertices.';

function TCastleSceneCore.InfoTriangleVerticesCounts: string;
begin
  if (VerticesCount(false) = VerticesCount(true)) and
     (TrianglesCount(false) = TrianglesCount(true)) then
    Result := Format(SSceneInfoTriVertCounts_Same,
      [TrianglesCount(false), VerticesCount(false)]) + NL else
  begin
    Result :=
      Format(SSceneInfoTriVertCounts_1,
        [TrianglesCount(false), VerticesCount(false)]) + NL +
      Format(SSceneInfoTriVertCounts_2,
        [TrianglesCount(true), VerticesCount(true)]) + NL;
  end;
end;

function TCastleSceneCore.InfoBoundingBox: string;
var
  BBox: TBox3D;
begin
  BBox := BoundingBox;
  Result := 'Bounding box : ' + BBox.ToNiceStr;
  if not BBox.IsEmpty then
  begin
    Result += ', average size : ' + FloatToNiceStr(BBox.AverageSize);
  end;
  Result += NL;
end;

function TCastleSceneCore.InfoManifoldAndBorderEdges: string;
begin
  Result := Format('Edges detection: all edges split into %d manifold edges and %d border edges. Remember that for shadow volumes perfect manifold (that is, zero border edges) is required, otherwise the scene will not cast shadows.',
    [ ManifoldEdges.Count,
      BorderEdges.Count ]) + NL;
end;

function TCastleSceneCore.Info(
  ATriangleVerticesCounts,
  ABoundingBox,
  AManifoldAndBorderEdges: boolean): string;
begin
  Result := '';

  if ATriangleVerticesCounts then
  begin
    Result += InfoTriangleVerticesCounts;
  end;

  if ABoundingBox then
  begin
    if Result <> '' then Result += NL;
    Result += InfoBoundingBox;
  end;

  if AManifoldAndBorderEdges then
  begin
    if Result <> '' then Result += NL;
    Result += InfoManifoldAndBorderEdges;
  end;
end;

{ octrees -------------------------------------------------------------------- }

function TCastleSceneCore.OverrideOctreeLimits(
  const BaseLimits: TOctreeLimits;
  const OP: TSceneOctreeProperties): TOctreeLimits;
var
  Props: TKambiOctreePropertiesNode;
begin
  Result := BaseLimits;
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TKambiNavigationInfoNode) then
  begin
    Props := TKambiNavigationInfoNode(NavigationInfoStack.Top).OctreeProperties(OP);
    if Props <> nil then
      Props.OverrideLimits(Result);
  end;
end;

function TCastleSceneCore.TriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TCastleSceneCore.ShapeOctreeLimits: POctreeLimits;
begin
  Result := @FShapeOctreeLimits;
end;

procedure TCastleSceneCore.AddTriangleToOctreeProgress(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Shape, Position, Normal, TexCoord, Face);
end;

procedure TCastleSceneCore.SetSpatial(const Value: TSceneSpatialStructures);

  procedure SetShapeSpatial(const Value: TShapeSpatialStructures;
    OnlyCollidable: boolean);
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do

        { When Value <> [], we honor OnlyCollidable. }
        if (Value = []) or
           (not OnlyCollidable) or
           (SI.Current.Collidable) then
        begin
          { Note: do not change here
              SI.Current.TriangleOctreeLimits :=
            Our own TriangleOctreeLimits properties may be *not* suitable
            for this (as our properties are for global octrees).

            Just let programmer change per-shape properties if he wants,
            or user to change this per-shape by
            [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_octree_properties].
          }

          SI.Current.TriangleOctreeProgressTitle := TriangleOctreeProgressTitle;
          SI.Current.Spatial := Value;
          { prepare OctreeTriangles. Not really needed, but otherwise
            shape's octrees would be updated (even on static scenes!)
            when the model runs. }
          SI.Current.OctreeTriangles;
        end;

    finally FreeAndNil(SI) end;
  end;

var
  Old, New: boolean;
begin
  if Value <> Spatial then
  begin
    { Handle OctreeRendering }

    Old := ssRendering in Spatial;
    New := ssRendering in Value;

    if Old and not New then
      FreeAndNil(FOctreeRendering);

    { Handle OctreeDynamicCollisions and Shapes[I].Spatial }

    Old := ssDynamicCollisions in Spatial;
    New := ssDynamicCollisions in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeDynamicCollisions);
      SetShapeSpatial([], true);
    end else
    if New and not Old then
    begin
      { SetShapeSpatial cannot be done by the way of doing CreateShapeOctree,
        since in CreateShapeOctree we iterate over OnlyActive shapes,
        but SetShapeSpatial must iterate over all shapes. }
      SetShapeSpatial([ssTriangles], true);
    end;

    { Handle OctreeVisibleTriangles }

    Old := ssVisibleTriangles in Spatial;
    New := ssVisibleTriangles in Value;

    if Old and not New then
      FreeAndNil(FOctreeVisibleTriangles);

    { Handle OctreeCollidableTriangles }

    Old := ssCollidableTriangles in Spatial;
    New := ssCollidableTriangles in Value;

    if Old and not New then
      FreeAndNil(FOctreeCollidableTriangles);

    FSpatial := Value;
  end;
end;

function TCastleSceneCore.OctreeRendering: TShapeOctree;
begin
  if (ssRendering in Spatial) and (FOctreeRendering = nil) then
  begin
    FOctreeRendering := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opRendering),
      ShapeOctreeProgressTitle,
      false);
    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeRendering updated');
  end;

  Result := FOctreeRendering;
end;

function TCastleSceneCore.OctreeDynamicCollisions: TShapeOctree;
begin
  if (ssDynamicCollisions in Spatial) and (FOctreeDynamicCollisions = nil) then
  begin
    FOctreeDynamicCollisions := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opDynamicCollisions),
      ShapeOctreeProgressTitle,
      true);
    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeDynamicCollisions updated');
  end;

  Result := FOctreeDynamicCollisions;
end;

function TCastleSceneCore.OctreeVisibleTriangles: TTriangleOctree;
begin
  if (ssVisibleTriangles in Spatial) and (FOctreeVisibleTriangles = nil) then
    FOctreeVisibleTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opVisibleTriangles),
      TriangleOctreeProgressTitle,
      false);
  Result := FOctreeVisibleTriangles;
end;

function TCastleSceneCore.OctreeCollidableTriangles: TTriangleOctree;
begin
  if (ssCollidableTriangles in Spatial) and (FOctreeCollidableTriangles = nil) then
    FOctreeCollidableTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opCollidableTriangles),
      TriangleOctreeProgressTitle,
      true);
  Result := FOctreeCollidableTriangles;
end;

function TCastleSceneCore.OctreeCollisions: TBaseTrianglesOctree;
begin
  if OctreeCollidableTriangles <> nil then
    Result := OctreeCollidableTriangles else
  if OctreeDynamicCollisions <> nil then
    Result := OctreeDynamicCollisions else
    Result := nil;
end;

function TCastleSceneCore.CreateTriangleOctree(
  const Limits: TOctreeLimits;
  const ProgressTitle: string;
  const Collidable: boolean): TTriangleOctree;

  procedure FillOctree(TriangleEvent: TTriangleEvent);
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, true);
    try
      while SI.GetNext do
        if (Collidable and SI.Current.Collidable) or
           ((not Collidable) and SI.Current.Visible) then
          SI.Current.Triangulate(false, TriangleEvent);
    finally FreeAndNil(SI) end;
  end;

begin
  Inc(Dirty);
  try

  Result := TTriangleOctree.Create(Limits, BoundingBox);
  try
    Result.Triangles.Capacity := TrianglesCount(false);
    if (ProgressTitle <> '') and
       (Progress.UserInterface <> nil) and
       (not Progress.Active) then
    begin
      Progress.Init(TrianglesCount(false), ProgressTitle, true);
      try
        TriangleOctreeToAdd := Result;
        FillOctree({$ifdef FPC_OBJFPC} @ {$endif} AddTriangleToOctreeProgress);
      finally Progress.Fini end;
    end else
      FillOctree({$ifdef FPC_OBJFPC} @ {$endif} Result.AddItemTriangle);
  except Result.Free; raise end;

  finally Dec(Dirty) end;
end;

function TCastleSceneCore.CreateShapeOctree(
  const Limits: TOctreeLimits;
  const ProgressTitle: string;
  const Collidable: boolean): TShapeOctree;
var
  I: Integer;
  ShapesList: TShapeList;
begin
  Inc(Dirty);
  try

  if Collidable then
    { Add only active and collidable shapes }
    ShapesList := TShapeList.Create(Shapes, true, false, true) else
    { Add only active and visible shapes }
    ShapesList := TShapeList.Create(Shapes, true, true, false);

  Result := TShapeOctree.Create(Limits, BoundingBox, ShapesList, true);
  try
    if (ProgressTitle <> '') and
       (Progress.UserInterface <> nil) and
       (not Progress.Active) then
    begin
      Progress.Init(Result.ShapesList.Count, ProgressTitle, true);
      try
        for I := 0 to Result.ShapesList.Count - 1 do
          if not Result.ShapesList[I].BoundingBox.IsEmpty then
          begin
            Result.TreeRoot.AddItem(I);
            Progress.Step;
          end;
      finally Progress.Fini end;
    end else
    begin
      for I := 0 to Result.ShapesList.Count - 1 do
        if not Result.ShapesList[I].BoundingBox.IsEmpty then
          Result.TreeRoot.AddItem(I);
    end;
  except Result.Free; raise end;

  finally Dec(Dirty) end;
end;

{ viewpoints ----------------------------------------------------------------- }

type
  BreakFirstViewpointFound = class(TCodeBreaker);

  TFirstViewpointSeeker = class
    OnlyPerspective: boolean;
    ViewpointDescription: string;
    FoundNode: TAbstractViewpointNode;
    procedure Seek(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

  procedure TFirstViewpointSeeker.Seek(
    Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  var
    V: TAbstractViewpointNode;
  begin
    V := Node as TAbstractViewpointNode;
    if ( (not OnlyPerspective) or
         (V.ProjectionType = ptPerspective) ) and
       ( (ViewpointDescription = '') or
         ( (Node is TAbstractX3DViewpointNode) and
           (TAbstractX3DViewpointNode(Node).FdDescription.Value = ViewpointDescription) ) ) then
    begin
      FoundNode := V;
      raise BreakFirstViewpointFound.Create;
    end;
  end;

function TCastleSceneCore.GetViewpointCore(
  const OnlyPerspective: boolean;
  out ProjectionType: TProjectionType;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TAbstractViewpointNode;
var
  Seeker: TFirstViewpointSeeker;
begin
  Result := nil;

  if RootNode <> nil then
  begin
    Seeker := TFirstViewpointSeeker.Create;
    try
      Seeker.OnlyPerspective := OnlyPerspective;
      Seeker.ViewpointDescription := ViewpointDescription;

      try
        RootNode.Traverse(TAbstractViewpointNode, @Seeker.Seek);
      except
        on BreakFirstViewpointFound do
        begin
          Result := Seeker.FoundNode;
        end;
      end;
    finally FreeAndNil(Seeker) end;
  end;

  if Result <> nil then
  begin
    Result.GetView(CamPos, CamDir, CamUp, GravityUp);
    ProjectionType := Result.ProjectionType;
  end else
  begin
    { use default camera settings }
    CamPos := DefaultX3DCameraPosition[cvVrml2_X3d];
    CamDir := DefaultX3DCameraDirection;
    CamUp := DefaultX3DCameraUp;
    GravityUp := DefaultX3DGravityUp;
    ProjectionType := ptPerspective;
  end;
end;

function TCastleSceneCore.GetViewpoint(
  out ProjectionType: TProjectionType;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TAbstractViewpointNode;
begin
  Result := GetViewpointCore(false, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
end;

function TCastleSceneCore.GetPerspectiveViewpoint(
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TAbstractViewpointNode;
var
  ProjectionType: TProjectionType;
begin
  Result := GetViewpointCore(true, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
  Assert(ProjectionType = ptPerspective);
end;

{ fog ---------------------------------------------------------------------- }

function TCastleSceneCore.FogNode: TFogNode;
begin
  Result := FogStack.Top as TFogNode;
end;

{ triangles list ------------------------------------------------------------- }

type
  TTriangleAdder = class
    TriangleList: TTriangle3SingleList;
    procedure AddTriangle(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
  end;

procedure TTriangleAdder.AddTriangle(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
begin
  if IsValidTriangle(Position) then
    TriangleList.Add(Position);
end;

function TCastleSceneCore.TrianglesListShadowCasters: TTrianglesShadowCastersList;

  function CreateTrianglesListShadowCasters: TTrianglesShadowCastersList;

    function ShadowCaster(AShape: TShape): boolean;
    var
      Shape: TAbstractShapeNode;
    begin
      Shape := AShape.State.ShapeNode;
      Result := not (
        (Shape <> nil) and
        (Shape.FdAppearance.Value <> nil) and
        (Shape.FdAppearance.Value is TAppearanceNode) and
        (not TAppearanceNode(Shape.FdAppearance.Value).FdShadowCaster.Value));
    end;

  var
    SI: TShapeTreeIterator;
    TriangleAdder: TTriangleAdder;
    WasSomeTransparentShadowCaster: boolean;
  begin
    Result := TTrianglesShadowCastersList.Create;
    try
      Result.Capacity := TrianglesCount(false);
      TriangleAdder := TTriangleAdder.Create;
      try
        TriangleAdder.TriangleList := Result;

        { This variable allows a small optimization: if there are
          no transparent triangles for shadow casters,
          then there's no need to iterate over Shapes
          second time. }
        WasSomeTransparentShadowCaster := false;

        { Add all opaque triangles }
        SI := TShapeTreeIterator.Create(Shapes, true);
        try
          while SI.GetNext do
            if ShadowCaster(SI.Current) then
            begin
              if not SI.Current.Transparent then
                SI.Current.Triangulate(false, @TriangleAdder.AddTriangle) else
                WasSomeTransparentShadowCaster := true;
            end;
        finally FreeAndNil(SI) end;

        { Mark OpaqueCount border }
        Result.FOpaqueCount := Result.Count;

        { Add all transparent triangles }
        if WasSomeTransparentShadowCaster then
        begin
          SI := TShapeTreeIterator.Create(Shapes, true);
          try
            while SI.GetNext do
              if ShadowCaster(SI.Current) and
                 SI.Current.Transparent then
                SI.Current.Triangulate(false, @TriangleAdder.AddTriangle);
          finally FreeAndNil(SI) end;
        end;

        if Log and LogShadowVolumes then
          WritelnLog('Shadow volumes', Format('Shadows casters triangles: %d opaque, %d total',
            [Result.OpaqueCount, Result.Count]));

      finally FreeAndNil(TriangleAdder) end;
    except Result.Free; raise end;
  end;

begin
  if not (fvTrianglesListShadowCasters in Validities) then
  begin
    FreeAndNil(FTrianglesListShadowCasters);
    FTrianglesListShadowCasters := CreateTrianglesListShadowCasters;
    Include(Validities, fvTrianglesListShadowCasters);
  end;

  Result := FTrianglesListShadowCasters;
end;

procedure TCastleSceneCore.InvalidateTrianglesListShadowCasters;
begin
  Exclude(Validities, fvTrianglesListShadowCasters);
  FreeAndNil(FTrianglesListShadowCasters);
end;

{ edges lists ------------------------------------------------------------- }

procedure TCastleSceneCore.CalculateIfNeededManifoldAndBorderEdges;

  { Sets FManifoldEdges and FBorderEdges. Assumes that FManifoldEdges and
    FBorderEdges are @nil on enter. }
  procedure CalculateManifoldAndBorderEdges;

    { If the counterpart of this edge (edge from neighbor) exists in
      EdgesSingle, then it adds this edge (along with it's counterpart)
      to FManifoldEdges.

      Otherwise, it just adds the edge to EdgesSingle. This can happen
      if it's the 1st time this edge occurs, or maybe the 3d one, 5th...
      all odd occurrences, assuming that ordering of faces is consistent,
      so that counterpart edges are properly detected. }
    procedure AddEdgeCheckManifold(
      EdgesSingle: TManifoldEdgeList;
      const TriangleIndex: Cardinal;
      const V0: TVector3Single;
      const V1: TVector3Single;
      const VertexIndex: Cardinal;
      Triangles: TTriangle3SingleList);
    var
      I: Integer;
      EdgePtr: PManifoldEdge;
    begin
      if EdgesSingle.Count <> 0 then
      begin
        EdgePtr := PManifoldEdge(EdgesSingle.List);
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          { It would also be possible to get EdgePtr^.V0/1 by code like

            TrianglePtr := @Triangles.L[EdgePtr^.Triangles[0]];
            EdgeV0 := @TrianglePtr^[EdgePtr^.VertexIndex];
            EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + 1) mod 3];

            But, see TManifoldEdge.V0/1 comments --- current version is
            a little faster.
          }

          { Triangles must be consistently ordered on a manifold,
            so the second time an edge is present, we know it must
            be in different order. So we compare V0 with EdgeV1
            (and V1 with EdgeV0), no need to compare V1 with EdgeV1. }
          if VectorsPerfectlyEqual(V0, EdgePtr^.V1) and
             VectorsPerfectlyEqual(V1, EdgePtr^.V0) then
          begin
            EdgePtr^.Triangles[1] := TriangleIndex;

            { Move edge to FManifoldEdges: it has 2 neighboring triangles now. }
            FManifoldEdges.Add^ := EdgePtr^;

            { Remove this from EdgesSingle.
              Note that we delete from EdgesSingle fast, using assignment and
              deleting only from the end (normal Delete would want to shift
              EdgesSingle contents in memory, to preserve order of items;
              but we don't care about order). }
            EdgePtr^ := EdgesSingle.L[EdgesSingle.Count - 1];
            EdgesSingle.Count := EdgesSingle.Count - 1;

            Exit;
          end;
          Inc(EdgePtr);
        end;
      end;

      { New edge: add new item to EdgesSingle }
      EdgePtr := EdgesSingle.Add;
      EdgePtr^.VertexIndex := VertexIndex;
      EdgePtr^.Triangles[0] := TriangleIndex;
      EdgePtr^.V0 := V0;
      EdgePtr^.V1 := V1;
    end;

  var
    I: Integer;
    Triangles: TTriangle3SingleList;
    TrianglePtr: PTriangle3Single;
    EdgesSingle: TManifoldEdgeList;
  begin
    Assert(FManifoldEdges = nil);
    Assert(FBorderEdges = nil);

    { It's important here that TrianglesListShadowCasters guarentees that only valid
      triangles are included. Otherwise degenerate triangles could make
      shadow volumes rendering result bad. }
    Triangles := TrianglesListShadowCasters;

    FManifoldEdges := TManifoldEdgeList.Create;
    { There is a precise relation between number of edges and number of faces
      on a closed manifold: E = T * 3 / 2. }
    FManifoldEdges.Capacity := Triangles.Count * 3 div 2;

    { EdgesSingle are edges that have no neighbor,
      i.e. have only one adjacent triangle. At the end, what's left here
      will be simply copied to BorderEdges. }
    EdgesSingle := TManifoldEdgeList.Create;
    try
      EdgesSingle.Capacity := Triangles.Count * 3 div 2;

      TrianglePtr := PTriangle3Single(Triangles.List);
      for I := 0 to Triangles.Count - 1 do
      begin
        { TrianglePtr points to Triangles[I] now }
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[0], TrianglePtr^[1], 0, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[1], TrianglePtr^[2], 1, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[2], TrianglePtr^[0], 2, Triangles);
        Inc(TrianglePtr);
      end;

      FBorderEdges := TBorderEdgeList.Create;

      if EdgesSingle.Count <> 0 then
      begin
        { scene not a perfect manifold: less than 2 faces for some edges
          (the case with more than 2 is already eliminated above).
          So we copy EdgesSingle to BorderEdges. }
        FBorderEdges.Count := EdgesSingle.Count;
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          FBorderEdges.L[I].VertexIndex := EdgesSingle.L[I].VertexIndex;
          FBorderEdges.L[I].TriangleIndex := EdgesSingle.L[I].Triangles[0];
        end;
      end;
    finally FreeAndNil(EdgesSingle); end;

    if Log and LogShadowVolumes then
      WritelnLog('Shadow volumes', Format(
        'Edges: %d manifold, %d border',
        [FManifoldEdges.Count, FBorderEdges.Count] ));
  end;

begin
  if not (fvManifoldAndBorderEdges in Validities) then
  begin
    FOwnsManifoldAndBorderEdges := true;
    CalculateManifoldAndBorderEdges;
    Include(Validities, fvManifoldAndBorderEdges);
  end;
end;

function TCastleSceneCore.ManifoldEdges: TManifoldEdgeList;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FManifoldEdges;
end;

function TCastleSceneCore.BorderEdges: TBorderEdgeList;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FBorderEdges;
end;

procedure TCastleSceneCore.ShareManifoldAndBorderEdges(
  ManifoldShared: TManifoldEdgeList;
  BorderShared: TBorderEdgeList);
begin
  Assert(
    (ManifoldShared = FManifoldEdges) =
    (BorderShared = FBorderEdges),
    'For ShareManifoldAndBorderEdges, either both ManifoldShared and ' +
    'BorderShared should be the same as already owned, or both should ' +
    'be different. If you have a good reason to break this, report, ' +
    'implementation of ShareManifoldAndBorderEdges may be improved ' +
    'if it''s needed');

  if (fvManifoldAndBorderEdges in Validities) and
    (ManifoldShared = FManifoldEdges) then
    { No need to do anything in this case.

      If ManifoldShared = FManifoldEdges = nil, then we may leave
      FOwnsManifoldAndBorderEdges = true
      while it could be = false (if we let this procedure continue),
      but this doesn't matter (since FOwnsManifoldAndBorderEdges doesn't matter
      when FManifoldEdges = nil).

      If ManifoldShared <> nil and old FOwnsManifoldAndBorderEdges is false
      then this doesn't change anything.

      Finally, the important case: If ManifoldShared <> nil and old
      FOwnsManifoldAndBorderEdges is true. Then it would be very very bad
      to continue this method, as we would free FManifoldEdges pointer
      and right away set FManifoldEdges to the same pointer (that would
      be invalid now). }
    Exit;

  if FOwnsManifoldAndBorderEdges then
  begin
    FreeAndNil(FManifoldEdges);
    FreeAndNil(FBorderEdges);
  end;

  FManifoldEdges := ManifoldShared;
  FBorderEdges := BorderShared;
  FOwnsManifoldAndBorderEdges := false;
  Include(Validities, fvManifoldAndBorderEdges);
end;

procedure TCastleSceneCore.InvalidateManifoldAndBorderEdges;
begin
  Exclude(Validities, fvManifoldAndBorderEdges);

  { Clear variables after removing fvManifoldAndBorderEdges }
  if FOwnsManifoldAndBorderEdges then
  begin
    FreeAndNil(FManifoldEdges);
    FreeAndNil(FBorderEdges);
  end else
  begin
    FManifoldEdges := nil;
    FBorderEdges := nil;
  end;
end;

{ freeing resources ---------------------------------------------------------- }

procedure TCastleSceneCore.FreeResources_UnloadTextureData(Node: TX3DNode);
begin
  (Node as TAbstractTexture2DNode).IsTextureLoaded := false;
end;

procedure TCastleSceneCore.FreeResources_UnloadTexture3DData(Node: TX3DNode);
begin
  (Node as TAbstractTexture3DNode).TextureLoaded := false;
end;

procedure TCastleSceneCore.FreeResources_UnloadBackgroundImage(Node: TX3DNode);
begin
  (Node as TBackgroundNode).BgImagesLoaded := false;
end;

procedure TCastleSceneCore.FreeResources(Resources: TSceneFreeResources);
begin
  if (frTextureDataInNodes in Resources) and (RootNode <> nil) then
  begin
    RootNode.EnumerateNodes(TAbstractTexture2DNode,
      @FreeResources_UnloadTextureData, false);
    RootNode.EnumerateNodes(TAbstractTexture3DNode,
      @FreeResources_UnloadTexture3DData, false);
  end;

  if (frBackgroundImageInNodes in Resources) and (RootNode <> nil) then
    RootNode.EnumerateNodes(TBackgroundNode,
      @FreeResources_UnloadBackgroundImage, false);

  if frTrianglesListShadowCasters in Resources then
    InvalidateTrianglesListShadowCasters;

  if frManifoldAndBorderEdges in Resources then
    InvalidateManifoldAndBorderEdges;
end;

{ events --------------------------------------------------------------------- }

procedure TCastleSceneCore.ScriptsInitializeCallback(Node: TX3DNode);
begin
  TScriptNode(Node).Initialized := true;
end;

procedure TCastleSceneCore.ScriptsInitialize;
begin
  if RootNode <> nil then
  begin
    BeginChangesSchedule;
    try
      { We have to initialize scripts only after all other initialization
        is done, in particular after Node.Scene was set by ChangedAll.
        Reason: scripts initialize() methods may already cause some events,
        that should notify us appropriately.
        This is also why Begin/EndChangesSchedule around is useful. }
      RootNode.EnumerateNodes(TScriptNode, @ScriptsInitializeCallback, false);
    finally EndChangesSchedule end;
  end;
end;

procedure TCastleSceneCore.ScriptsFinalizeCallback(Node: TX3DNode);
begin
  TScriptNode(Node).Initialized := false;
end;

procedure TCastleSceneCore.ScriptsFinalize;
begin
  if RootNode <> nil then
  begin
    BeginChangesSchedule;
    try
      { We have to deinitialize scripts before any other deinitialization
        is done. Just like for ScriptsInitialize. }
      RootNode.EnumerateNodes(TScriptNode, @ScriptsFinalizeCallback, false);
    finally EndChangesSchedule end;
  end;
end;

procedure TCastleSceneCore.SetProcessEvents(const Value: boolean);

  { When ProcessEvents is set to @true, you want to call initial
    position/orientation_changed events.

    Implementation below essentially is like CameraChanged,
    except it checks CameraViewKnown (instead of setting it always to @true). }
  procedure InitialProximitySensorsEvents;
  var
    I: Integer;
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      if CameraViewKnown then
      begin
        for I := 0 to ProximitySensors.Count - 1 do
          ProximitySensorUpdate(ProximitySensors[I]);
      end;
    finally EndChangesSchedule end;
  end;

begin
  if FProcessEvents <> Value then
  begin
    if Value then
    begin
      FProcessEvents := Value;

      ScriptsInitialize;
      InitialProximitySensorsEvents;

      RenderingCamera.OnChanged.Add(@RenderingCameraChanged);
    end else
    begin
      ScriptsFinalize;
      PointingDeviceClear;

      FProcessEvents := Value;

      { ProcessEvents := false may get called from destructor,
        after RenderingCameraUnit finalization }
      if RenderingCamera <> nil then
        RenderingCamera.OnChanged.Remove(@RenderingCameraChanged);
    end;
  end;
end;

procedure TCastleSceneCore.SetStatic(const Value: boolean);
begin
  if FStatic <> Value then
  begin
    FStatic := Value;
    if Static then
    begin
      { Clear TX3DNode.Scene for all nodes }
      if RootNode <> nil then
        UnregisterScene(RootNode);
    end else
      { Set TX3DNode.Scene for all nodes.
        This is done as part of ChangedAll when Static = true. }
      ScheduleChangedAll;
  end;
end;

{ key sensors handling ------------------------------------------------------- }

function TCastleSceneCore.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itKey) then Exit;

  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TAbstractKeyDeviceSensorNode).
          KeyDown(Event.Key, Event.KeyCharacter, FTime);
    finally EndChangesSchedule; end;

    { Do not treat it as handled (returning ExclusiveEvents),
      even if some X3DKeyDeviceSensorNode was found and did something.
      This would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;
end;

function TCastleSceneCore.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) or (Event.EventType <> itKey) then Exit;

  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TAbstractKeyDeviceSensorNode).
          KeyUp(Event.Key, Event.KeyCharacter, FTime);
    finally EndChangesSchedule; end;

    { Do not treat it as handled (returning ExclusiveEvents),
      even if some X3DKeyDeviceSensorNode was found and did something.
      This would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;
end;

{ pointing device handling --------------------------------------------------- }

function TCastleSceneCore.PointingDeviceMove(const Pick: TRayCollisionNode;
  const Distance: Single): boolean;
var
  TouchSensor: TTouchSensorNode;
  ActiveSensor: TAbstractPointingDeviceSensorNode;
  OldIsOver, NewIsOver: boolean;
  OldSensors: TX3DNodeList;
  NewSensors: TX3DNodeList;
  I: Integer;
  OverItem: PTriangle;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  OverItem := PTriangle(Pick.Triangle);

  { Never treat it as handled here (returning ExclusiveEvents),
    this would disable too much (like Camera usually under Scene on Controls).
  Result := false; }

  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
    { Note that using Begin/EndChangesSchedule is not only for efficiency
      here. It's also sometimes needed to keep the code correct: note
      that ChangedAll changes everything, including State pointers.
      So OverItem.State becomes invalid. (Once octree will be rebuild, also
      OverItem will be invalid.) Obviously, we can't let this happen
      in the middle of PointingDeviceMove. }
    BeginChangesSchedule;
    try
      { Handle isOver events }

      if PointingDeviceOverItem <> OverItem then
      begin
        if PointingDeviceActiveSensors.Count <> 0 then
        begin
          Assert(PointingDeviceActive);

          { This is quite special situation, as it means that
            PointingDeviceActiveSensors grabbed all events.
            isOver (either TRUE or FALSE) may be generated for active
            sensors, but no other sensors should receive any events. }

          for I := 0 to PointingDeviceActiveSensors.Count - 1 do
          begin
            ActiveSensor := PointingDeviceActiveSensors.Items[I] as
              TAbstractPointingDeviceSensorNode;

            if ActiveSensor.FdEnabled.Value then
            begin
              OldIsOver := (PointingDeviceOverItem <> nil) and
                (PointingDeviceOverItem^.State.PointingDeviceSensors.
                  IndexOf(ActiveSensor) <> -1);

              NewIsOver := (OverItem <> nil) and
                (OverItem^.State.PointingDeviceSensors.
                  IndexOf(ActiveSensor) <> -1);

              if OldIsOver <> NewIsOver then
                ActiveSensor.EventIsOver.Send(NewIsOver, Time);
            end;
          end;
        end else
        if (PointingDeviceOverItem <> nil) and
           (OverItem <> nil) then
        begin
          OldSensors := PointingDeviceOverItem^.State.PointingDeviceSensors;
          NewSensors := OverItem^.State.PointingDeviceSensors;

          { X3D spec says about isOver events that
            "Events are not generated if the geometry itself is
            animating and moving underneath the pointing device."

            I understand that they mean that you don't have to call
            PointingDeviceMove continously, you can only check this on
            actual mouse move. That is, isOver state changes are not
            intended to be caught immediately if they happened because
            the geometry is animating. But, still, all isOver changes *are*
            reported. I hope they don't mean that isOver changes because
            of underlying geometry animating should not be reported at all.

            The latter interpretation:
            - Would be unhandy for implementation, as I would have to call
              raycollision twice for each MouseMove (to get
              OverItemBeforeMove and OverItem).
            - Would be unhandy for users. You want
              to catch isOver changes eventually, right? Otherwise,
              user may be forced to move mouse out and then back in to generate
              isOver event = TRUE. Worse, user has to move mouse inside and
              then back outside to generate isOver = FALSE event.
          }

          for I := 0 to OldSensors.Count - 1 do
            if NewSensors.IndexOf(OldSensors[I]) = -1 then
            begin
              if (OldSensors[I] is TAbstractPointingDeviceSensorNode) and
                TAbstractPointingDeviceSensorNode(OldSensors[I]).FdEnabled.Value then
                TAbstractPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, Time);
            end;

          for I := 0 to NewSensors.Count - 1 do
            if OldSensors.IndexOf(NewSensors[I]) = -1 then
            begin
              if (NewSensors[I] is TAbstractPointingDeviceSensorNode) and
                TAbstractPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
                TAbstractPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, Time);
            end;
        end else
        if PointingDeviceOverItem <> nil then
        begin
          { So we previously pointed as something, and now at nothing.
            So simply call isOver = FALSE for all. }

          OldSensors := PointingDeviceOverItem^.State.PointingDeviceSensors;

          for I := 0 to OldSensors.Count - 1 do
            if (OldSensors[I] is TAbstractPointingDeviceSensorNode) and
              TAbstractPointingDeviceSensorNode(OldSensors[I]).FdEnabled.Value then
              TAbstractPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, Time);
        end else
        begin
          Assert(OverItem <> nil);

          { So we previously pointed as nothing, and now at something.
            So simply call isOver = TRUE for all. }

          NewSensors := OverItem^.State.PointingDeviceSensors;

          for I := 0 to NewSensors.Count - 1 do
            if (NewSensors[I] is TAbstractPointingDeviceSensorNode) and
              TAbstractPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
              TAbstractPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, Time);
        end;

        FPointingDeviceOverItem := OverItem;

        DoPointingDeviceSensorsChange;
      end;

      { When OverItem <> nil => Pick.Point is meaningful.
        Right now OverItem = FPointingDeviceOverItem,
        so take care to make PointingDeviceOverPoint also meaningful. }
      FPointingDeviceOverPoint := Pick.Point;

      { Handle hitXxx_changed events }

      if OverItem <> nil then
      begin
        NewSensors := OverItem^.State.PointingDeviceSensors;

        for I := 0 to NewSensors.Count - 1 do
          if NewSensors[I] is TTouchSensorNode then
          begin
            TouchSensor := TTouchSensorNode(NewSensors[I]);
            if TouchSensor.FdEnabled.Value then
            begin
              TouchSensor.EventHitPoint_Changed.Send(
                { hitPoint_changed event wants a point in local coords,
                  we can get this by InverseTransform. }
                MatrixMultPoint(OverItem^.State.InvertedTransform, Pick.Point), Time);

              if TouchSensor.EventHitNormal_Changed.SendNeeded then
                TouchSensor.EventHitNormal_Changed.Send(
                  OverItem^.INormal(Pick.Point), Time);

              if TouchSensor.EventHitTexCoord_Changed.SendNeeded then
                TouchSensor.EventHitTexCoord_Changed.Send(
                  OverItem^.ITexCoord2D(Pick.Point), Time);
            end;
          end;
      end;

      { Call Drag on active drag sensors }
      for I := 0 to PointingDeviceActiveSensors.Count - 1 do
      begin
        ActiveSensor := PointingDeviceActiveSensors.Items[I] as
          TAbstractPointingDeviceSensorNode;
        if ActiveSensor is TAbstractDragSensorNode then
          TAbstractDragSensorNode(ActiveSensor).Drag(
            Time, Pick.RayOrigin, Pick.RayDirection);
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TCastleSceneCore.DoPointingDeviceSensorsChange;
begin
  { I want to keep assertion that Cursor = mcHand when
    we're over or keeping active some pointing-device sensors. }
  if ((PointingDeviceSensors <> nil) and
      (PointingDeviceSensors.EnabledCount <> 0)) or
     (PointingDeviceActiveSensors.Count <> 0) then
    Cursor := mcHand else
    Cursor := mcDefault;

  if Assigned(OnPointingDeviceSensorsChange) then
    OnPointingDeviceSensorsChange(Self);
end;

procedure TCastleSceneCore.PointingDeviceClear;
var
  SensorsChanged: boolean;
begin
  SensorsChanged :=
    (FPointingDeviceOverItem <> nil) or
    { This may be called from destructor (through
      TShape.FreeOctreeTriangles when freeing shapes), so prepare for
      the FPointingDeviceActiveSensors = nil case. }
    ( (FPointingDeviceActiveSensors <> nil) and
      (FPointingDeviceActiveSensors.Count <> 0) );

  FPointingDeviceOverItem := nil;
  { PointingDeviceOverPoint may be left undefined now, but let's set it
    to something deterministic to ease debugging. }
  FPointingDeviceOverPoint := ZeroVector3Single;
  FPointingDeviceActive := false;
  if FPointingDeviceActiveSensors <> nil then
    FPointingDeviceActiveSensors.Count := 0;

  if SensorsChanged then
    DoPointingDeviceSensorsChange;
end;

function TCastleSceneCore.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;

  function AnchorActivate(Anchor: TAnchorNode): boolean;
  var
    NewRootNode: TX3DRootNode;
    NewViewpoint: TAbstractViewpointNode;
  begin
    Result := Anchor.LoadAnchor(NewRootNode, NewViewpoint, RootNode);
    if Result then
    begin
      { activating Anchor clears other sensors, since Anchor
        loads completely different scene. }
      FPointingDeviceActiveSensors.Count := 0;

      if NewRootNode <> nil then
        Load(NewRootNode, true, { do not reset Time } false);

      { When NewRootNode <> nil, it's important here that we know
        we're inside BeginChangesSchedule.

        This means that ForceTeleportTransitions (set to true by Load)
        is still true during the following Set_Bind := true call.
        That's because ChangedAll (that resets ForceTeleportTransitions
        to false) was not called yet. }

      if NewViewpoint <> nil then
        NewViewpoint.EventSet_Bind.Send(true, Time);
    end;
  end;

var
  Sensors: TPointingDeviceSensorList;

  function PDSensorActivate(Sensor: TAbstractPointingDeviceSensorNode): boolean;
  begin
    Result := Sensor.FdEnabled.Value and
      { Send isActive = true and make DoPointingDeviceSensorsChange
        only if FPointingDeviceActiveSensor changes. }
      (PointingDeviceActiveSensors.IndexOf(Sensor) = -1);
    if Result then
    begin
      PointingDeviceActiveSensors.Add(Sensor);
      { We do this only when PointingDeviceOverItem <> nil,
        so we know that PointingDeviceOverPoint is meaningful. }
      Sensor.Activate(Time, Sensors.Transform, Sensors.InvertedTransform,
        PointingDeviceOverPoint);
    end;
  end;

var
  I: Integer;
  ToActivate: TX3DNode;
  ActiveChanged: boolean;
  ActiveSensor: TAbstractPointingDeviceSensorNode;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if ProcessEvents and (FPointingDeviceActive <> Active) then
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      ActiveChanged := false;
      FPointingDeviceActive := Active;
      if Active then
      begin
        if PointingDeviceOverItem <> nil then
        begin
          Sensors := PointingDeviceOverItem^.State.PointingDeviceSensors;
          for I := 0 to Sensors.Count - 1 do
          begin
            { Activate all the enabled sensors. Spec says to activate
              simultaneouly all Sensors (tied for this mouse down). }
            ToActivate := Sensors[I];
            if ToActivate is TAbstractPointingDeviceSensorNode then
            begin
              if PDSensorActivate(TAbstractPointingDeviceSensorNode(ToActivate)) then
                ActiveChanged := true;
            end else
            if ToActivate is TAnchorNode then
            begin
              if AnchorActivate(TAnchorNode(ToActivate)) then
              begin
                ActiveChanged := true;
                Break;
              end;
            end;
          end;
        end;
      end else
      begin
        { Deactivate all PointingDeviceActiveSensors (if any) }
        if PointingDeviceActiveSensors.Count <> 0 then
        begin
          for I := 0 to PointingDeviceActiveSensors.Count -1 do
          begin
            ActiveSensor := PointingDeviceActiveSensors.Items[I]
              as TAbstractPointingDeviceSensorNode;
            ActiveSensor.Deactivate(Time);
            { If we're still over the sensor, generate touchTime for TouchSensor }
            if (PointingDeviceOverItem <> nil) and
               (PointingDeviceOverItem^.State.PointingDeviceSensors.
                 IndexOf(ActiveSensor) <> -1) and
               (ActiveSensor is TTouchSensorNode) then
            begin
              TTouchSensorNode(ActiveSensor).
                EventTouchTime.Send(Time.Seconds, Time);
            end;
          end;
          FPointingDeviceActiveSensors.Count := 0;
          ActiveChanged := true;
        end;
      end;

      if ActiveChanged then DoPointingDeviceSensorsChange;

      { We try hard to leave Result as false when nothing happened.
        This is important for TCastleSceneManager, that wants to retry
        activation around if ApproximateActivation, and for other 3D objects
        along the same TRayCollision list. So we really must set
        Result := false if nothing happened, to enable other objects
        to have a better chance of catching activation.
        At the same time, we really must set Result := true if something
        (possibly) happened. Otherwise, simultaneously two objects may be activated,
        and TCastleSceneManager.PointingDeviceActivateFailed may do a sound
        warning that activation was unsuccessful.

        Fortunately, our ActiveChanged right now precisely tells us
        when something happened. Whe not ActiveChanged, nothing happened,
        no X3D event was send. }
      Result := ActiveChanged;
    finally
      EndChangesSchedule;
    end;
  end;
end;

function TCastleSceneCore.PointingDeviceSensors: TPointingDeviceSensorList;
begin
  if PointingDeviceOverItem <> nil then
    Result := PointingDeviceOverItem^.State.PointingDeviceSensors else
    Result := nil;
end;

function TCastleSceneCore.Dragging: boolean;

  function ActiveDraggingSensor: boolean;
  var
    I: Integer;
  begin
    Result := false;
    for I := 0 to PointingDeviceActiveSensors.Count - 1 do
      if PointingDeviceActiveSensors.Items[I] is TAbstractDragSensorNode then
        Exit(true);
   end;

begin
  Result := (inherited Dragging) or
    ((PointingDeviceActiveSensors.Count <> 0) and
      { minor optimization, do not even call ActiveDraggingSensor if
        PointingDeviceActiveSensors.Count = 0 (most usual case) }
      ActiveDraggingSensor and
      ProcessEvents);
end;

{ Time stuff ------------------------------------------------------------ }

function TCastleSceneCore.GetTime: TX3DTime;
begin
  Result := FTime;
end;

procedure TCastleSceneCore.InternalSetTime(
  const NewValue: TX3DTime; const TimeIncrease: TFloatTime; const ResetTime: boolean);
var
  SomethingVisibleChanged: boolean;
  I: Integer;
  ChangedSkin: TMFVec3f;
begin
  if ProcessEvents then
  begin
    BeginChangesSchedule;
    try
      SomethingVisibleChanged := false;

      for I := 0 to TimeDependentHandlers.Count - 1 do
      begin
        if TimeDependentHandlers[I].SetTime(Time, NewValue, TimeIncrease, ResetTime) and
          (TimeDependentHandlers[I].Node is TMovieTextureNode) then
          SomethingVisibleChanged := true;
      end;

      { If SomethingVisibleChanged (on MovieTexture nodes),
        then we have to redisplay. }
      if SomethingVisibleChanged then
        VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);

      { call humanoids AnimateSkin now.
        This could actually be done anywhere, as long as it gets called
        fairly soon after every HAnimJoint animation. }
      for I := 0 to ScheduledHumanoidAnimateSkin.Count - 1 do
      begin
        ChangedSkin := (ScheduledHumanoidAnimateSkin.Items[I]
          as THAnimHumanoidNode).AnimateSkin;
        if ChangedSkin <> nil then
          ChangedSkin.Changed;
      end;
      ScheduledHumanoidAnimateSkin.Count := 0;
    finally
      EndChangesSchedule;
    end;
  end;

  FTime := NewValue;
end;

procedure TCastleSceneCore.SetTime(const NewValue: TFloatTime);
var
  TimeIncrease: TFloatTime;
  NewCompleteValue: TX3DTime;
begin
  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  TimeIncrease := NewValue - FTime.Seconds;
  if TimeIncrease > 0 then
    InternalSetTime(NewCompleteValue, TimeIncrease, false);
end;

procedure TCastleSceneCore.IncreaseTime(const TimeIncrease: TFloatTime);
var
  NewCompleteValue: TX3DTime;
begin
  NewCompleteValue.Seconds := FTime.Seconds + TimeIncrease;
  NewCompleteValue.PlusTicks := 0;
  if TimeIncrease > 0 then
    InternalSetTime(NewCompleteValue, TimeIncrease, false);
end;

procedure TCastleSceneCore.ResetLastEventTime(Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to Node.Routes.Count - 1 do
    Node.Routes[I].ResetLastEventTime;
  if Node is TAbstractScriptNode then
    TAbstractScriptNode(Node).ResetLastEventTimes;
end;

procedure TCastleSceneCore.ResetTime(const NewValue: TFloatTime);
var
  NewCompleteValue: TX3DTime;
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes(@ResetLastEventTime, false);

  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  InternalSetTime(NewCompleteValue, 0, true);
end;

procedure TCastleSceneCore.ResetTimeAtLoad;
var
  TimeAtLoad: TFloatTime;
begin
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TKambiNavigationInfoNode) and
     TKambiNavigationInfoNode(NavigationInfoStack.Top).FdTimeOriginAtLoad.Value
    then
    TimeAtLoad := 0.0 else
    TimeAtLoad := DateTimeToUnix(Now);
  ResetTime(TimeAtLoad);
end;

procedure TCastleSceneCore.IncreaseTimeTick;
begin
  Inc(FTime.PlusTicks);
end;

procedure TCastleSceneCore.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if not GetExists then Exit;

  { Ignore Idle calls when CompSpeed is precisely zero
    (this may happen, and is correct, see TCastleWindowBase.IgnoreNextIdleSpeed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway. }
  if TimePlaying and (CompSpeed <> 0) then
    IncreaseTime(TimePlayingSpeed * CompSpeed);
end;

{ changes schedule ----------------------------------------------------------- }

procedure TCastleSceneCore.BeginChangesSchedule;
begin
  { ChangedAllScheduled = false always when ChangedAllSchedule = 0. }
  Assert((ChangedAllSchedule <> 0) or (not ChangedAllScheduled));

  Inc(ChangedAllSchedule);
end;

procedure TCastleSceneCore.ScheduleChangedAll;
begin
  if ChangedAllSchedule = 0 then
    ChangedAll else
    ChangedAllScheduled := true;
end;

procedure TCastleSceneCore.EndChangesSchedule;
begin
  Dec(ChangedAllSchedule);
  if (ChangedAllSchedule = 0) and ChangedAllScheduled then
  begin
    ChangedAllScheduled := false;

    { ChangedAll calls ChangedAllEnumerate, which in turn calls
      Begin/EndChangesSchedule. Which means that
      ChangedAllSchedule/ChangedAllScheduled must be Ok for this.
      That's why ChangedAllScheduled := false must be done previously. }

    ChangedAll;
  end;
end;

{ proximity sensor ----------------------------------------------------------- }

procedure TCastleSceneCore.ProximitySensorUpdate(const PSI: TProximitySensorInstance);
var
  Position, Direction, Up: TVector3Single;
  ProxNode: TProximitySensorNode;
  NewIsActive: boolean;
begin
  Assert(CameraViewKnown);
  if ProcessEvents then
  begin
    BeginChangesSchedule;
    try
      ProxNode := PSI.Node;
      if not ProxNode.FdEnabled.Value then Exit;

      { In each ProximitySensorUpdate we transform CameraPosition to
        ProximitySensor coordinate-space. This allows us to check
        whether the camera is inside ProximitySensor precisely
        (otherwise transforming ProximitySensor box could make a larger
        box, as we do not support oriented bounding boxes, only
        axis-aligned).

        This is also needed to generate position_changed value,
        as it has to be in ProximitySensor coordinate-space.

        Also, since we don't store precalculated box of ProximitySensor,
        we can gracefully react in ChangedField to changes:
        - changes to ProximitySensor center and size must only produce
          new ProximitySensorUpdate to eventually activate/deactivate ProximitySensor
        - changes to transforms affecting ProximitySensor must only update
          it's InvertedTransform and call ProximitySensorUpdate.
      }

      Position := MatrixMultPoint(PSI.InvertedTransform, CameraPosition);

      NewIsActive :=
        (Position[0] >= ProxNode.FdCenter.Value[0] - ProxNode.FdSize.Value[0] / 2) and
        (Position[0] <= ProxNode.FdCenter.Value[0] + ProxNode.FdSize.Value[0] / 2) and
        (Position[1] >= ProxNode.FdCenter.Value[1] - ProxNode.FdSize.Value[1] / 2) and
        (Position[1] <= ProxNode.FdCenter.Value[1] + ProxNode.FdSize.Value[1] / 2) and
        (Position[2] >= ProxNode.FdCenter.Value[2] - ProxNode.FdSize.Value[2] / 2) and
        (Position[2] <= ProxNode.FdCenter.Value[2] + ProxNode.FdSize.Value[2] / 2) and
        { ... and the box is not empty, which for ProximitySensor
          is signalled by any size <= 0 (yes, equal 0 also means empty).
          We check this at the end, as this is the least common situation? }
        (ProxNode.FdSize.Value[0] > 0) and
        (ProxNode.FdSize.Value[1] > 0) and
        (ProxNode.FdSize.Value[2] > 0);

      if NewIsActive <> PSI.IsActive then
      begin
        PSI.IsActive := NewIsActive;
        ProxNode.EventIsActive.Send(NewIsActive, Time);
        if NewIsActive then
          ProxNode.EventEnterTime.Send(Time.Seconds, Time) else
          ProxNode.EventExitTime.Send(Time.Seconds, Time);
      end;

      { Call position_changed, orientation_changed, even if this is just
        the first time NewIsActive = true (that is, even when it was
        NewIsActive <> PSI.IsActive). Reasoning: this allows to activate
        ProximitySensor when world starts (before player moves),
        which is a wanted feature. }

      if NewIsActive then
      begin
        ProxNode.EventPosition_Changed.Send(Position, Time);
        if ProxNode.EventOrientation_Changed.SendNeeded then
        begin
          Direction := MatrixMultDirection(PSI.InvertedTransform, CameraDirection);
          Up        := MatrixMultDirection(PSI.InvertedTransform, CameraUp);
          ProxNode.EventOrientation_Changed.Send(
            CamDirUp2Orient(Direction, Up), Time);
        end;
        { TODO: centerOfRotation_changed }
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TCastleSceneCore.CameraChanged(ACamera: TCamera;
  const Changes: TVisibleChanges);
var
  I: Integer;
begin
  ACamera.GetView(FCameraPosition, FCameraDirection, FCameraUp);
  FCameraViewKnown := true;

  BeginChangesSchedule;
  try
    for I := 0 to ShapeLODs.Count - 1 do
      UpdateLODLevel(TShapeTreeLOD(ShapeLODs.Items[I]));

    if ProcessEvents then
    begin
      Inc(FTime.PlusTicks);
      for I := 0 to ProximitySensors.Count - 1 do
        ProximitySensorUpdate(ProximitySensors[I]);

      Inc(FTime.PlusTicks);
      { Update camera information on all Billboard nodes,
        and retraverse scene from Billboard nodes. So we treat Billboard nodes
        much like Transform nodes, except that their transformation animation
        is caused by camera changes, not by changes to field values.

        TODO: If one Billboard is under transformation of another Billboard,
        this will be a little wasteful. We should update first all camera
        information, and then update only Billboard nodes that do not have
        any parent Billboard nodes. }
      for I := 0 to BillboardInstancesList.Count - 1 do
      begin
        (BillboardInstancesList[I] as TBillboardNode).CameraChanged(
          FCameraPosition, FCameraDirection, FCameraUp);
        TransformationChanged(BillboardInstancesList[I],
          BillboardInstancesList[I].ShapeTrees as TShapeTreeList,
          [chTransform]);
      end;
    end;
  finally EndChangesSchedule end;

  VisibleChangeHere(Changes + [vcCamera]);
end;

{ compiled scripts ----------------------------------------------------------- }

procedure TCastleSceneCore.RegisterCompiledScript(const HandlerName: string;
  Handler: TCompiledScriptHandler);
var
  HandlerInfo: PCompiledScriptHandlerInfo;
begin
  HandlerInfo := CompiledScriptHandlers.Add;
  HandlerInfo^.Handler := Handler;
  HandlerInfo^.Name := HandlerName;
end;

procedure TCastleSceneCore.ExecuteCompiledScript(const HandlerName: string;
  ReceivedValue: TX3DField);
var
  I: Integer;
begin
  for I := 0 to CompiledScriptHandlers.Count - 1 do
    if CompiledScriptHandlers.L[I].Name = HandlerName then
    begin
      CompiledScriptHandlers.L[I].Handler(ReceivedValue, Time);
      Break;
    end;
end;

{ camera ------------------------------------------------------------------ }

procedure TCastleSceneCore.CameraFromNavigationInfo(
  Camera: TCamera; const Box: TBox3D;
  const ForceNavigationType: string;
  const ForceRadius: Single);
var
  NavigationTypeInitialized: boolean;

  { Use to set TWalkCamera-specific properties.
    This is Camera (if it's TWalkCamera),
    or Camera.Walk (if it's TUniversalCamera), or nil. }
  Walk: TWalkCamera;
  Examine: TExamineCamera;
  Universal: TUniversalCamera;

  { Initialize stuff determined by NavigationType
    (treating it like NavigationInfo.type value).
    Sets NavigationTypeInitialized to true if navigation type
    recognized (and not 'ANY'). }
  procedure InitializeNavigationType(const NavigationType: string);
  begin
    if NavigationType = 'WALK' then
    begin
      NavigationTypeInitialized := true;
      if Universal <> nil then Universal.NavigationClass := ncWalk;
      if Walk <> nil then Walk.PreferGravityUpForRotations := true;
      if Walk <> nil then Walk.PreferGravityUpForMoving := true;
      if Walk <> nil then Walk.Gravity := true;
    end else
    if NavigationType = 'FLY' then
    begin
      NavigationTypeInitialized := true;
      if Universal <> nil then Universal.NavigationClass := ncWalk;
      if Walk <> nil then Walk.PreferGravityUpForRotations := true;
      if Walk <> nil then Walk.PreferGravityUpForMoving := false;
      if Walk <> nil then Walk.Gravity := false;
    end else
    if NavigationType = 'NONE' then
    begin
      NavigationTypeInitialized := true;
      Camera.Input := [];
    end else
    if (NavigationType = 'EXAMINE') or (NavigationType = 'LOOKAT') then
    begin
      if NavigationType = 'LOOKAT' then
        OnWarning(wtMinor, 'VRML/X3D', 'TODO: Navigation type "LOOKAT" is not yet supported, treating like "EXAMINE"');
      NavigationTypeInitialized := true;
      if Universal <> nil then Universal.NavigationClass := ncExamine;
      if Examine <> nil then Examine.ArchitectureMode := false;
    end else
    if NavigationType = 'ARCHITECTURE' then
    begin
      NavigationTypeInitialized := true;
      if Universal <> nil then Universal.NavigationClass := ncExamine;
      if Examine <> nil then Examine.ArchitectureMode := true;
    end else
    if NavigationType = 'ANY' then
    begin
      { Do nothing, also do not report this NavigationInfo.type as unknown. }
    end else
      OnWarning(wtMajor, 'VRML/X3D', Format('Unknown NavigationInfo.type "%s"',
        [NavigationType]));
  end;

var
  NavigationNode: TNavigationInfoNode;
  I: Integer;
  Radius: Single;
begin
  { calculate Walk, Examine and Universal first.
    Makes handling various cameras later much easier. }
  if Camera is TUniversalCamera then
    Walk := TUniversalCamera(Camera).Walk else
  if Camera is TWalkCamera then
    Walk := TWalkCamera(Camera) else
    Walk := nil;

  if Camera is TUniversalCamera then
    Examine := TUniversalCamera(Camera).Examine else
  if Camera is TExamineCamera then
    Examine := TExamineCamera(Camera) else
    Examine := nil;

  if Camera is TUniversalCamera then
    Universal := TUniversalCamera(Camera) else
    Universal := nil;

  NavigationTypeInitialized := false;
  NavigationNode := NavigationInfoStack.Top;

  { Reset Camera properties, this way InitializeNavigationType may
    assume these are already set. }
  if Universal <> nil then Universal.NavigationClass := ncWalk;
  if Walk <> nil then Walk.PreferGravityUpForRotations := true;
  if Walk <> nil then Walk.PreferGravityUpForMoving := true;
  if Walk <> nil then Walk.Gravity := false;
  if Examine <> nil then Examine.ArchitectureMode := false;
  Camera.Input := DefaultCameraInput;

  if ForceNavigationType <> '' then
    InitializeNavigationType(ForceNavigationType);

  if not NavigationTypeInitialized then
  begin
    if NavigationNode <> nil then
      for I := 0 to NavigationNode.FdType.Count - 1 do
      begin
        InitializeNavigationType(NavigationNode.FdType.Items[I]);
        if NavigationTypeInitialized then
          Break;
      end;
  end;

  if not NavigationTypeInitialized then
    { No recognized "type" found, so use default type EXAMINE. }
    InitializeNavigationType('EXAMINE');

  { calculate Radius }
  Radius := ForceRadius;
  if Radius <= 0 then
  begin
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 1) then
      Radius := NavigationNode.FdAvatarSize.Items[0];
    { if avatarSize doesn't specify Radius, or specifies invalid <= 0,
      calculate something suitable based on Box. }
    if Radius <= 0 then
      Radius := Box.AverageSize(false, 1.0) * 0.005;
  end;

  Camera.Radius := Radius;

  if Walk <> nil then
  begin
    { calculate Walk.PreferredHeight }
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 2) then
      Walk.PreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      { Make it something >> Radius * 2, to allow some
        space to decrease (e.g. by Input_DecreasePreferredHeight
        in view3dscene). Remember that CorrectPreferredHeight
        adds a limit to PreferredHeight, around Radius * 2. }
      Walk.PreferredHeight := Radius * 4;

    Walk.CorrectPreferredHeight;

    { calculate Walk.ClimbHeight }
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 3) then
      Walk.ClimbHeight := Max(NavigationNode.FdAvatarSize.Items[2], 0.0) else
      Walk.ClimbHeight := 0;

    { calculate Walk.HeadBobbing* }
    if (NavigationNode <> nil) and
       (NavigationNode is TKambiNavigationInfoNode) then
    begin
      Walk.HeadBobbing := TKambiNavigationInfoNode(NavigationNode).FdHeadBobbing.Value;
      Walk.HeadBobbingTime := TKambiNavigationInfoNode(NavigationNode).FdHeadBobbingTime.Value;
    end else
    begin
      Walk.HeadBobbing := DefaultHeadBobbing;
      Walk.HeadBobbingTime := DefaultHeadBobbingTime;
    end;

    { calculate Walk.MoveSpeed }
    if NavigationNode = nil then
      { Since we don't have NavigationNode.speed, we just calculate some
        speed that should "feel sensible". We base it on Radius,
        that was set above. }
      Walk.MoveSpeed := Camera.Radius * 20 else
      { This is OK, also for NavigationNode.FdSpeed.Value = 0 case. }
      Walk.MoveSpeed := NavigationNode.FdSpeed.Value;
  end;

  if Examine <> nil then Examine.ModelBox := Box;

  { No point in calling Walk.Init here: this method,
    together with CameraFromViewpoint (with RelativeCameraTransform = false),
    together initialize everything that TWalkCamera.Init does.

    Also, no point in calling Examine.Init, for the same reason. }
end;

procedure TCastleSceneCore.CameraFromViewpoint(ACamera: TCamera;
  const RelativeCameraTransform, AllowTransitionAnimate: boolean);
var
  Position: TVector3Single;
  Direction: TVector3Single;
  Up: TVector3Single;
  GravityUp: TVector3Single;
  WalkCamera: TWalkCamera;
begin
  if ViewpointStack.Top <> nil then
  begin
    (ViewpointStack.Top as TAbstractViewpointNode).GetView(
      Position, Direction, Up, GravityUp);
  end else
  begin
    { Suitable viewpoint,
      with dir -Z and up +Y (like standard VRML/X3D viewpoint) }
    CameraViewpointForWholeScene(BoundingBox, 2, 1, false, true,
      Position, Direction, Up, GravityUp);
  end;

  if ACamera is TWalkCamera then
    WalkCamera := TWalkCamera(ACamera) else
  if ACamera is TUniversalCamera then
    WalkCamera := TUniversalCamera(ACamera).Walk else
    WalkCamera := nil;

  if WalkCamera <> nil then
    WalkCamera.GravityUp := GravityUp;

  { If RelativeCameraTransform, then we will move relative to
    initial camera changes. Else, we will jump to new initial camera vectors. }
  ACamera.SetInitialView(Position, Direction, Up, RelativeCameraTransform);
  if not RelativeCameraTransform then
  begin
    if AllowTransitionAnimate and (not ForceTeleportTransitions) then
      CameraTransition(ACamera, Position, Direction, Up) else
      ACamera.SetView(Position, Direction, Up);
  end;
end;

function TCastleSceneCore.CreateCamera(AOwner: TComponent;
  const Box: TBox3D;
  const ForceNavigationType: string = ''): TUniversalCamera;
begin
  Result := TUniversalCamera.Create(AOwner);
  CameraFromNavigationInfo(Result, Box, ForceNavigationType);
  CameraFromViewpoint(Result, false, false);
end;

function TCastleSceneCore.CreateCamera(AOwner: TComponent;
  const ForceNavigationType: string = ''): TUniversalCamera;
begin
  Result := CreateCamera(AOwner, BoundingBox, ForceNavigationType);
end;

procedure TCastleSceneCore.CameraTransition(Camera: TCamera;
  const Position, Direction, Up: TVector3Single);
var
  NavigationNode: TNavigationInfoNode;
  TransitionAnimate: boolean;
  TransitionTime: TFloatTime;
  TransitionType: string;
  I: Integer;
begin
  NavigationNode := NavigationInfoStack.Top;

  TransitionAnimate := true;

  { check NavigationInfo.transitionType, update TransitionAnimate.
    If we have LINEAR or ANIMATE or only unknown transition types,
    spec says to use animation. }
  if NavigationNode <> nil then
    for I := 0 to NavigationNode.FdTransitionType.Count - 1 do
    begin
      TransitionType := NavigationNode.FdTransitionType.Items[I];
      if TransitionType = 'TELEPORT' then
      begin
        TransitionAnimate := false;
        Break;
      end else
      if (TransitionType = 'LINEAR') or (TransitionType = 'ANIMATE') then
        { Leave TransitionAnimate as true }
        Break else
        OnWarning(wtMinor, 'VRML/X3D', Format('Unrecognized transitionType "%s"', [TransitionType]));
    end;

  { calculate TransitionTime }
  if NavigationNode <> nil then
    TransitionTime := NavigationNode.FdTransitionTime.Value else
    TransitionTime := 1;

  { correct TransitionAnimate in case TransitionTime invalid }
  if TransitionTime <= 0 then
    TransitionAnimate := false;

  if TransitionAnimate then
    Camera.AnimateTo(Position, Direction, Up, TransitionTime) else
    Camera.SetView(Position, Direction, Up);
end;

procedure TCastleSceneCore.CameraTransition(Camera: TCamera;
  const Position, Direction, Up, GravityUp: TVector3Single);
begin
  if Camera is TWalkCamera then
    TWalkCamera(Camera).GravityUp := GravityUp else
  if Camera is TUniversalCamera then
    TUniversalCamera(Camera).Walk.GravityUp := GravityUp;
    { Else ignore GravityUp }

  CameraTransition(Camera, Position, Direction, Up);
end;

{ misc ----------------------------------------------------------------------- }

function TCastleSceneCore.GetViewpointStack: TX3DBindableStackBasic;
begin
  Result := FViewpointStack;
end;

function TCastleSceneCore.GetNavigationInfoStack: TX3DBindableStackBasic;
begin
  Result := FNavigationInfoStack;
end;

function TCastleSceneCore.GetBackgroundStack: TX3DBindableStackBasic;
begin
  Result := FBackgroundStack;
end;

function TCastleSceneCore.GetFogStack: TX3DBindableStackBasic;
begin
  Result := FFogStack;
end;

type
  BreakMainLightForShadows = class(TCodeBreaker);

procedure TCastleSceneCore.CalculateMainLightForShadowsPosition;
begin
  if FMainLightForShadowsNode is TAbstractPositionalLightNode then
    FMainLightForShadows := Vector4Single(
      MatrixMultPoint(
        FMainLightForShadowsTransform,
        TAbstractPositionalLightNode(FMainLightForShadowsNode).FdLocation.Value), 1) else
  if FMainLightForShadowsNode is TAbstractDirectionalLightNode then
    FMainLightForShadows := Vector4Single( Normalized(
      MatrixMultDirection(
        FMainLightForShadowsTransform,
        TAbstractDirectionalLightNode(FMainLightForShadowsNode).FdDirection.Value) ), 0) else
    raise Exception.CreateFmt('TCastleSceneCore.MainLightForShadows: ' +
      'light node "%s" cannot be used to cast shadows, it has no position ' +
      'and no direction', [FMainLightForShadowsNode.NodeTypeName]);
end;

procedure TCastleSceneCore.SearchMainLightForShadows(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
var
  L: TAbstractLightNode absolute Node;
begin
  if L.FdKambiShadows.Value and
     L.FdKambiShadowsMain.Value then
  begin
    FMainLightForShadowsNode := L;
    FMainLightForShadowsTransform := StateStack.Top.Transform;
    FMainLightForShadowsExists := true;
    CalculateMainLightForShadowsPosition;
    raise BreakMainLightForShadows.Create;
  end;
end;

procedure TCastleSceneCore.ValidateMainLightForShadows;

  procedure CalculateMainLightForShadows;
  begin
    FMainLightForShadowsExists := false;
    if RootNode <> nil then
    try
      RootNode.Traverse(TAbstractLightNode, @SearchMainLightForShadows);
    except on BreakMainLightForShadows do ; end;
  end;

begin
  if not (fvMainLightForShadows in Validities) then
  begin
    CalculateMainLightForShadows;
    Include(Validities, fvMainLightForShadows);
  end;
end;

function TCastleSceneCore.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
begin
  ValidateMainLightForShadows;
  Result := FMainLightForShadowsExists;
  if Result then
    AMainLightPosition := FMainLightForShadows;
end;

procedure TCastleSceneCore.SetHeadlightOn(const Value: boolean);
begin
  if FHeadlightOn <> Value then
  begin
    FHeadlightOn := Value;
    if Assigned(OnHeadlightOnChanged) then OnHeadlightOnChanged(Self);
    if NavigationInfoStack.Top <> nil then
      NavigationInfoStack.Top.FdHeadlight.Send(HeadlightOn);
  end;
end;

function TCastleSceneCore.CustomHeadlight: TAbstractLightNode;
var
  MaybeResult: TX3DNode;
begin
  Result := nil;
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TKambiNavigationInfoNode) then
  begin
    MaybeResult := TKambiNavigationInfoNode(NavigationInfoStack.Top).FdheadlightNode.Value;
    if MaybeResult is TAbstractLightNode then
      Result := TAbstractLightNode(MaybeResult);
  end;
end;

procedure TCastleSceneCore.UpdateHeadlightOnFromNavigationInfo;
begin
  if NavigationInfoStack.Top <> nil then
    HeadlightOn := NavigationInfoStack.Top.FdHeadlight.Value else
    HeadlightOn := DefaultNavigationInfoHeadlight;
end;

procedure TCastleSceneCore.ViewChangedSuddenly;
begin
  { Nothing meaningful to do in this class }
end;

procedure TCastleSceneCore.RenderingCameraChanged(
  const RenderingCamera: TRenderingCamera;
  Viewpoint: TAbstractViewpointNode);
begin
  { Although we register this callback only when ProcessEvents,
    so we could assume here that ProcessEvents is already true...
    But, just in case, check ProcessEvents again (in case in the future
    there will be some queue of events and they could arrive with delay). }

  if (Viewpoint = nil) and
     (ViewpointStack.Top <> nil) and
     (ViewpointStack.Top is TAbstractViewpointNode) then
    Viewpoint := TAbstractViewpointNode(ViewpointStack.Top);

  if ProcessEvents and
     (Viewpoint <> nil) and
     ( (RenderingCamera.Target = rtScreen) or
       Viewpoint.FdcameraMatrixSendAlsoOnOffscreenRendering.Value ) then
  begin
    BeginChangesSchedule;
    try
      Inc(FTime.PlusTicks);

      if Viewpoint.EventCameraMatrix.SendNeeded then
        Viewpoint.EventCameraMatrix.Send(RenderingCamera.Matrix, Time);

      if Viewpoint.EventCameraInverseMatrix.SendNeeded then
      begin
        RenderingCamera.InverseMatrixNeeded;
        Viewpoint.EventCameraInverseMatrix.Send(RenderingCamera.InverseMatrix, Time);
      end;

      if Viewpoint.EventCameraRotationMatrix.SendNeeded then
        Viewpoint.EventCameraRotationMatrix.Send(RenderingCamera.RotationMatrix3, Time);

      if Viewpoint.EventCameraRotationInverseMatrix.SendNeeded then
      begin
        RenderingCamera.RotationInverseMatrixNeeded;
        Viewpoint.EventCameraRotationInverseMatrix.Send(RenderingCamera.RotationInverseMatrix3, Time);
      end;
    finally EndChangesSchedule end;
  end;
end;

procedure TCastleSceneCore.PrepareResources(Options: TPrepareResourcesOptions;
  ProgressStep: boolean; BaseLights: TAbstractLightInstancesList);

  procedure PrepareShapesOctrees;
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do SI.Current.OctreeTriangles;
    finally FreeAndNil(SI) end;
  end;

begin
  inherited;

  if prBoundingBox in Options then
    BoundingBox { ignore the result };

  if prTrianglesListShadowCasters in Options then
    TrianglesListShadowCasters;

  if prManifoldAndBorderEdges in Options then
    ManifoldEdges;

  if prSpatial in Options then
  begin
    OctreeRendering;
    OctreeDynamicCollisions;
    OctreeVisibleTriangles;
    OctreeCollidableTriangles;
    PrepareShapesOctrees;
  end;
end;

function TCastleSceneCore.HeightCollision(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  if OctreeCollisions <> nil then
  begin
    Result := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;

    if GetCollides then
    begin
      Result := OctreeCollisions.HeightCollision(Position, GravityUp,
        AboveHeight, PTriangle(AboveGround), nil, TrianglesToIgnoreFunc);
    end;
  end else
    Result := inherited HeightCollision(Position, GravityUp,
      TrianglesToIgnoreFunc, AboveHeight, AboveGround);
end;

function TCastleSceneCore.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if OctreeCollisions <> nil then
  begin
    if GetCollides then
    begin
      Result := OctreeCollisions.MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := true;
      NewPos := ProposedNewPos;
    end;
  end else
    Result := inherited MoveCollision(OldPos, ProposedNewPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
end;

function TCastleSceneCore.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if OctreeCollisions <> nil then
  begin
    Result := (not GetCollides) or
      OctreeCollisions.MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
  end else
    Result := inherited MoveCollision(OldPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
end;

function TCastleSceneCore.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  if OctreeCollisions <> nil then
    Result := (GetCollides or (ALineOfSight and GetExists)) and
      OctreeCollisions.IsSegmentCollision(
        Pos1, Pos2,
        nil, false, TrianglesToIgnoreFunc) else
    Result := inherited SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
end;

function TCastleSceneCore.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if OctreeCollisions <> nil then
    Result := GetCollides and
      OctreeCollisions.IsSphereCollision(
        Pos, Radius, nil, TrianglesToIgnoreFunc) else
    Result := inherited SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
end;

function TCastleSceneCore.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if OctreeCollisions <> nil then
    Result := GetCollides and
      OctreeCollisions.IsBoxCollision(
        Box,  nil, TrianglesToIgnoreFunc) else
    Result := inherited BoxCollision(Box, TrianglesToIgnoreFunc);
end;

function TCastleSceneCore.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  Triangle: PTriangle;
  Intersection: TVector3Single;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
begin
  if OctreeCollisions <> nil then
  begin
    Result := nil;
    if GetExists then
    begin
      Triangle := OctreeCollisions.RayCollision(
        Intersection, IntersectionDistance, RayOrigin, RayDirection,
        { ReturnClosestIntersection } true,
        { TriangleToIgnore } nil,
        { IgnoreMarginAtStart } false, TrianglesToIgnoreFunc);
      if Triangle <> nil then
      begin
        Result := TRayCollision.Create;
        Result.Distance := IntersectionDistance;
        NewNode := Result.Add;
        NewNode^.Item := Self;
        NewNode^.Point := Intersection;
        NewNode^.Triangle := Triangle;
        NewNode^.RayOrigin := RayOrigin;
        NewNode^.RayDirection := RayDirection;
      end;
    end;
  end else
    Result := inherited RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc);
end;

procedure TCastleSceneCore.SetShadowMaps(const Value: boolean);
begin
  if FShadowMaps <> Value then
  begin
    FShadowMaps := Value;

    ScheduledShadowMapsProcessing := true;
    ScheduleChangedAll;
  end;
end;

procedure TCastleSceneCore.SetShadowMapsDefaultSize(const Value: Cardinal);
begin
  if FShadowMapsDefaultSize <> Value then
  begin
    FShadowMapsDefaultSize := Value;

    if ShadowMaps then { if not ShadowMaps, then no need to reprocess }
    begin
      ScheduledShadowMapsProcessing := true;
      ScheduleChangedAll;
    end;
  end;
end;

function TCastleSceneCore.Caption: string;
var
  WorldInfoNode: TWorldInfoNode;
begin
  WorldInfoNode := RootNode.TryFindNode(TWorldInfoNode, true) as TWorldInfoNode;
  if (WorldInfoNode <> nil) and
     (WorldInfoNode.FdTitle.Value <> '') then
    Result := WorldInfoNode.FdTitle.Value else
    Result := ExtractFileName(FileName);
end;

function TCastleSceneCore.Node(const NodeName: string): TX3DNode;
begin
  if RootNode = nil then
    raise EX3DNotFound.CreateFmt('Cannot find node "%s"', [NodeName]) else
    Result := RootNode.FindNodeByName(TX3DNode, NodeName, false);
end;

function TCastleSceneCore.Field(const NodeName, FieldName: string): TX3DField;
begin
  if RootNode = nil then
    raise EX3DNotFound.CreateFmt('Cannot find node "%s"', [NodeName]) else
    Result := RootNode.FindNodeByName(TX3DNode, NodeName, false).
      Fields.ByName[FieldName];
end;

function TCastleSceneCore.Event(const NodeName, EventName: string): TX3DEvent;
begin
  if RootNode = nil then
    raise EX3DNotFound.CreateFmt('Cannot find node "%s"', [NodeName]) else
    Result := RootNode.FindNodeByName(TX3DNode, NodeName, false).
      Events.ByName[EventName];
end;

procedure TCastleSceneCore.InvalidateBackground;
begin
end;

procedure TCastleSceneCore.UnregisterSceneCallback(Node: TX3DNode);
begin
  Node.Scene := nil;
end;

procedure TCastleSceneCore.UnregisterScene(Node: TX3DNode);
begin
  Node.EnumerateNodes(TX3DNode, @UnregisterSceneCallback, false);
end;

end.
