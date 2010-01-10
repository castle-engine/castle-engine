{
  Copyright 2003-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(VRML scene as @link(TVRMLScene) class.) }

unit VRMLScene;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3d,
  VRMLFields, VRMLNodes, KambiClassUtils, KambiUtils,
  VRMLShape, VRMLTriangleOctree, ProgressUnit, KambiOctree, VRMLShapeOctree,
  KeysMouse, VRMLTime, Navigation, VRMLTriangle, Contnrs, VRMLHeadLight,
  RenderStateUnit, UIControls;

{$define read_interface}

type
  { }
  TDynArrayItem_1 = TTriangle3Single;
  PDynArrayItem_1 = PTriangle3Single;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Triangle3Single = TInfiniteArray_1;
  PArray_Triangle3Single = PInfiniteArray_1;
  TDynTriangle3SingleArray = TDynArray_1;

  { Internal helper type for TVRMLScene.
    @exclude }
  TVRMLSceneValidity = (fvBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    { fvFog is not used for now, since FogNode is not cached now
      (doesn't have to be, as it's simple shortcut for FogStack.Top). }
    fvFog,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvTrianglesListShadowCasters,
    fvManifoldAndBorderEdges,
    fvMainLightForShadows,
    fvShapesActiveCount,
    fvShapesActiveVisibleCount);

  { @exclude }
  TVRMLSceneValidities = set of TVRMLSceneValidity;

  { Scene edge that is between exactly two triangles.
    It's used by @link(TVRMLScene.ManifoldEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TManifoldEdge = record
    { Index to get vertexes of this edge.
      The actual edge's vertexes are not recorded here (this would prevent
      using TVRMLScene.ShareManifoldAndBorderEdges with various scenes from
      the same animation). You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the first triangle
      (i.e. Triangles[0]). }
    VertexIndex: Cardinal;

    { Indexes to TVRMLScene.TrianglesListShadowCasters array }
    Triangles: array [0..1] of Cardinal;

    { These are vertexes at VertexIndex and (VertexIndex+1)mod 3 positions,
      but @italic(only at generation of manifold edges time).
      Like said in VertexIndex, keeping here actual vertex info would prevent
      TVRMLScene.ShareManifoldAndBorderEdges. However, using these when generating
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
      TVRMLScene.ShareManifoldAndBorderEdges was not used to make it. }
    V0, V1: TVector3Single;
  end;
  PManifoldEdge = ^TManifoldEdge;

  TDynArrayItem_2 = TManifoldEdge;
  PDynArrayItem_2 = PManifoldEdge;
  {$define DYNARRAY_2_IS_STRUCT}
  {$I dynarray_2.inc}

  TDynManifoldEdgeArray = class(TDynArray_2)
  private
  end;

  { Scene edge that has one neighbor, i.e. border edge.
    It's used by @link(TVRMLScene.BorderEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TBorderEdge = record
    { Index to get vertex of this edge.
      The actual edge's vertexes are not recorded here (this would prevent
      using TVRMLScene.ShareManifoldAndBorderEdges with various scenes from
      the same animation). You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the triangle TriangleIndex. }
    VertexIndex: Cardinal;

    { Index to TVRMLScene.TrianglesListShadowCasters array. }
    TriangleIndex: Cardinal;
  end;
  PBorderEdge = ^TBorderEdge;

  TDynArrayItem_3 = TBorderEdge;
  PDynArrayItem_3 = PBorderEdge;
  {$define DYNARRAY_3_IS_STRUCT}
  {$I dynarray_3.inc}
  TDynBorderEdgeArray = TDynArray_3;

  { These are various features that may be freed by
    TVRMLScene.FreeResources.

    @italic(Warning): This is for experienced usage of TVRMLScene.
    Everything is explained in detail below, but still  --- if you have some
    doubts, or you just don't observe any memory shortage in your program,
    it's probably best to not use TVRMLScene.FreeResources.

    @unorderedList(
      @item(For frRootNode, you @italic(may) get nasty effects including crashes
        if you will use this in a wrong way.)

      @item(For frTextureDataInNodes, frBackgroundImageInNodes and TrianglesList,
        if you will free them unnecessarily
        (i.e. you will use it after you freed it), it will be automatically
        recreated on next use. So everything will work correctly, but you
        will experience unnecessary slowdown if we will need to recreate
        exactly the same resource over and over again.)

      @item(For frTextureDataInNodes, frBackgroundImageInNodes and frRootNode, note that
        freeing these resources too eagerly may make image cache
        (see ImagesCache) less effective. In normal circumstances,
        if you will use the same cache instance throughout the program,
        loaded images are reused. If you free frTextureDataInNodes or frRootNode
        too early, you may remove them from the cache too early, and lose
        a chance to reuse them. So you may cause unnecessary slowdown
        of preparing models, e.g. inside PrepareRender.)
    )
  }
  TVRMLSceneFreeResource = (
    { Free (and set to nil) RootNode of the scene. Works only if
      TVRMLScene.OwnsRootNode is @true (the general assertion is that
      TVRMLScene will @italic(never) free RootNode when OwnsRootNode is
      @false).

      frRootNode allows you to save some memory, but may be quite dangerous.
      You have to be careful then about what methods from the scene you use.
      Usually, you will prepare appropriate things first (usually by
      TVRMLSceneGL.PrepareRender), and after that call FreeResources
      with frRootNode.

      Note that event processing is impossible without RootNode nodes and
      fields and routes, so don't ever use this if you want to set
      TVRMLScene.ProcessEvents to @true.

      Note that if you will try to use a resource that was already freed
      by frRootNode, you may even get segfault (access violation).
      So be really careful, be sure to prepare everything first by
      TVRMLSceneGL.PrepareRender or such. }
    frRootNode,

    { Unloads the texture images/videos allocated in VRML texture nodes.

      It's useful if you know that you already prepared everything
      that needed the texture images, and you will not need texture images
      later. For TVRMLGLScene this means that you use Optimization
      method other than roNone,
      and you already did PrepareRender (so textures are already loaded to OpenGL),
      and your code will not access TextureImage / TextureVideo anymore.
      This is commonly @true for various games.

      Then you can call this to free some resources.

      Note that if you made an accident and you will use some TextureImage or
      TextureVideo after FreeResources, then you will get no crash,
      but texture image will be simply reloaded. So you may experience
      slowdown if you inappropriately use this feature.

      Oh, and note that if frRootNode and OwnsRootNode, then this is not
      necessary (as freeing RootNode also frees texture nodes, along with
      their texture). }
    frTextureDataInNodes,

    { Unloads the background images allocated in VRML Background nodes.
      The same comments as for frTextureDataInNodes apply. }
    frBackgroundImageInNodes,

    { Free triangle list created by TrianglesListShadowCasters call.
      This list is also implicitly created by ManifoldEdges or BorderEdges. }
    frTrianglesListShadowCasters,

    { Free triangle list created by TrianglesList(false) call.
      This list is also implicitly created by constructing triangle octree.

      Note that if you made an accident and you will use TrianglesList(false)
      after FreeResources, then you will get no crash,
      but TrianglesList(false) will be regenerated. So you may experience
      slowdown if you inappropriately use this feature. }
    frTrianglesListNotOverTriangulate,

    { Free triangle list created by TrianglesList(true) call.
      Analogous to frTrianglesListNotOverTriangulate.

      Note that if you made an accident and you will use TrianglesList(true)
      after FreeResources, then you will get no crash,
      but TrianglesList(true) will be regenerated. So you may experience
      slowdown if you inappropriately use this feature. }
    frTrianglesListOverTriangulate,

    { Free edges lists in ManifoldEdges and BorderEdges.

      Frees memory, but next call to ManifoldEdges and BorderEdges will
      need to calculate them again (or you will need to call
      TVRMLScene.ShareManifoldAndBorderEdges again).
      Note that using this scene as shadow caster for shadow volumes algorithm
      requires ManifoldEdges and BorderEdges. }
    frManifoldAndBorderEdges);

  TVRMLSceneFreeResources = set of TVRMLSceneFreeResource;

  TVRMLScene = class;

  TVRMLSceneNotification = procedure (Scene: TVRMLScene) of object;
  TVRMLSceneGeometryChanged = procedure (Scene: TVRMLScene;
    const SomeLocalGeometryChanged: boolean) of object;

  { VRML bindable nodes stack.
    This keeps a stack of TNodeX3DBindableNode, with comfortable routines
    to examine top and push/pop from top. The stack is actually stored
    as a list, with the last item being the top one. }
  TVRMLBindableStack = class(TVRMLNodesList)
  private
    FParentScene: TVRMLScene;
    { A useful utility: if the Node is not @nil, send isBound = Value and
      bindTime events to it. }
    procedure SendIsBound(Node: TNodeX3DBindableNode; const Value: boolean);
  private
    FOnBoundChanged: TVRMLSceneNotification;

    { Add new node to the top.

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged. }
    procedure Push(Node: TNodeX3DBindableNode);

    { Remove current top node. Returns removed node, or @nil if no current
      node was present (that is, stack was empty).

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged. }
    function Pop: TNodeX3DBindableNode;
  protected
    { Notification when the currently bound node, that is
      @link(Top), changed. This also includes notification
      when @link(Top) changed to (or from) @nil, that is
      when no node becomes bound or when some node is initially bound.

      In this class, just calls OnBoundChanged if assigned. }
    procedure DoBoundChanged; virtual;
  public
    constructor Create(AParentScene: TVRMLScene);

    property ParentScene: TVRMLScene read FParentScene;

    { Returns top item on this stack, or @nil if not present. }
    function Top: TNodeX3DBindableNode;

    { Add new node to the top, but only if stack is currently empty.
      If SendEvents, then isBound = true and bindTime events will be
      send to newly bound node. }
    procedure PushIfEmpty(Node: TNodeX3DBindableNode; SendEvents: boolean);

    { This should be used when you suspect that some nodes on the stack
      are no longer present in current VRML graph (they were deleted).
      In this case, they have to be removed from stack.

      RootNode may be nil, in which case all nodes are considered removed.
      This is only a special case for FreeResources(frRootNode).

      If this will change the currently bound node, then the new bound
      node will receive isBound = true and bindTime events (the old node
      will not  receive any set_bind = false or isBound = false events, since it
      may be destroyed by now). }
    procedure CheckForDeletedNodes(RootNode: TVRMLNode; SendEvents: boolean);

    { Handle set_bind event send to given Node.
      This always generates appropriate events. }
    procedure Set_Bind(Node: TNodeX3DBindableNode; const Value: boolean);

    { Notification when the currently bound node, that is
      @link(Top), changed.
      @seealso DoBoundChanged }
    property OnBoundChanged: TVRMLSceneNotification
      read FOnBoundChanged write FOnBoundChanged;
  end;

  TVRMLViewpointStack = class(TVRMLBindableStack)
  protected
    procedure DoBoundChanged; override;
  end;

  { @exclude }
  TProximitySensorInstance = record
    Node: TNodeProximitySensor;
    InvertedTransform: TMatrix4Single;
    IsActive: boolean;
  end;
  { @exclude }
  PProximitySensorInstance = ^TProximitySensorInstance;
  { @exclude }
  TDynArrayItem_4 = TProximitySensorInstance;
  { @exclude }
  PDynArrayItem_4 = PProximitySensorInstance;
  {$define DYNARRAY_4_IS_STRUCT}
  { @exclude }
  {$I dynarray_4.inc}
  { @exclude

    Internal for TVRMLScene: list of proximity sensors (one for each
    instance in the file, so when one ProximitySensor is instantiated
    more than once --- it will produce more than one entry in this array). }
  TDynProximitySensorInstanceArray = TDynArray_4;

  { @exclude }
  TGeneratedTexture = record
    { May be only TNodeGeneratedCubeMapTexture or TNodeRenderedTexture
      or TNodeGeneratedShadowMap. }
    TextureNode: TVRMLNode;
    Handler: TGeneratedTextureHandler;
    Shape: TVRMLShape;
  end;
  { @exclude }
  PGeneratedTexture = ^TGeneratedTexture;
  { @exclude }
  TDynArrayItem_7 = TGeneratedTexture;
  { @exclude }
  PDynArrayItem_7 = PGeneratedTexture;
  {$define DYNARRAY_7_IS_STRUCT}
  { @exclude }
  {$I dynarray_7.inc}
  { @exclude
    Internal for TVRMLScene: list of generated textures
    (GeneratedCubeMapTexture, RenderedTexture and similar nodes)
    along with their shape. }
  TDynGeneratedTextureArray = class(TDynArray_7)
  public
    function IndexOfTextureNode(TextureNode: TVRMLNode): Integer;
    function FindTextureNode(TextureNode: TVRMLNode): PGeneratedTexture;
    procedure AddShapeTexture(Shape: TVRMLShape; Tex: TNodeX3DTextureNode);
  end;

  { Internal helper for TVRMLScene, gathers information for transform nodes.

    Transform nodes are all nodes that have any field with Transform = true,
    currently:
      TNodeTransform_2
      TNodeMatrixTransform_2
      TNodeHAnimHumanoid
      TNodeHAnimJoint
      TNodeHAnimSite

    @exclude }
  TTransformNodeInfo = record
    Node: TVRMLNode;
    Occurences: Cardinal;
  end;

  { @exclude }
  PTransformNodeInfo = ^TTransformNodeInfo;
  { @exclude }
  TDynArrayItem_6 = TTransformNodeInfo;
  { @exclude }
  PDynArrayItem_6 = PTransformNodeInfo;
  {$define DYNARRAY_6_IS_STRUCT}
  { @exclude }
  {$I dynarray_6.inc}
  { @exclude }
  TDynTransformNodeInfoArray = class(TDynArray_6)
  public
    function NodeInfo(Node: TVRMLNode): PTransformNodeInfo;
  end;

  TCompiledScriptHandler = procedure (
    Value: TVRMLField; const Time: TVRMLTime) of object;

  { @exclude }
  TCompiledScriptHandlerInfo = record
    Handler: TCompiledScriptHandler;
    Name: string;
  end;
  { @exclude }
  PCompiledScriptHandlerInfo = ^TCompiledScriptHandlerInfo;
  { @exclude }
  TDynArrayItem_5 = TCompiledScriptHandlerInfo;
  { @exclude }
  PDynArrayItem_5 = PCompiledScriptHandlerInfo;
  {$define DYNARRAY_5_IS_STRUCT}
  {$define DYNARRAY_5_IS_INIT_FINI_TYPE}
  { @exclude }
  {$I dynarray_5.inc}
  { @exclude }
  TDynCompiledScriptHandlerInfoArray = TDynArray_5;

  { Possible spatial structure types that may be managed by TVRMLScene,
    see TVRMLScene.Spatial. }
  TVRMLSceneSpatialStructure = (
    { Create and keep current the TVRMLScene.OctreeRendering.
      This is a dynamic octree containing all visible shapes. }
    ssRendering,

    { Create and keep current the TVRMLScene.OctreeDynamicCollisions.
      This is a dynamic octree containing all collidable items. }
    ssDynamicCollisions,

    { Create the TVRMLScene.OctreeVisibleTriangles.
      This is an octree containing all visible triangles, suitable only
      for scenes that stay static. }
    ssVisibleTriangles,

    { Create the TVRMLScene.OctreeCollidableTriangles.
      This is an octree containing all collidable triangles, suitable only
      for scenes that stay static. }
    ssCollidableTriangles);
  TVRMLSceneSpatialStructures = set of TVRMLSceneSpatialStructure;

  { Triangles array for shadow casting object.

    This guarantees that the whole array has first OpaqueCount opaque triangles,
    then the rest is transparent.
    The precise definition between "opaque"
    and "transparent" is done by TVRMLShape.Transparent.
    This is also used by OpenGL rendering to determine which shapes
    need blending.

    This separation into opaque and transparent parts
    (with OpaqueCount marking the border) is useful for shadow volumes
    algorithm, that must treat transparent shadow casters a little
    differently. }
  TDynTrianglesShadowCastersArray = class(TDynTriangle3SingleArray)
  private
    FOpaqueCount: Cardinal;
  public
    { Numer of opaque triangles on this list. Opaque triangles
      are guarenteed to be placed before all transparent triangles
      on this list. }
    property OpaqueCount: Cardinal read FOpaqueCount;
  end;

  TVisibleSceneChange = (
    { Something visible in the scene geometry changed.
      "Geometry" means that this is applicable only to actual 3D shape
      changes. (Think about "does depth buffer from some point in space
      changes" --- this is actually why we have separate prVisibleSceneGeometry
      and prVisibleSceneNonGeometry for now, as GeneratedShadowMap
      does need to be updated only on geometry changes.) So it's not applicable
      when only light conditions, materials, textures and such change. }
    prVisibleSceneGeometry,
    { Something visible,  but not geometry, in the scene changed. }
    prVisibleSceneNonGeometry,
    { Viewer (the settings passed to ViewerChanged) changed. }
    prViewer);
  TVisibleSceneChanges = set of TVisibleSceneChange;

  { VRML scene, a final class to handle VRML models
    (with the exception of rendering, which is delegated to descendants,
    like TVRMLGLScene for OpenGL).

    VRML scene works with a graph of VRML nodes
    rooted in RootNode. It also deconstructs this graph to a very simple tree
    of @link(TVRMLShape) objects.
    The basic idea is to have at the same time full hierarchical
    view of the scene (in @link(RootNode)) and a simple view of the same scene
    (in @link(Shapes) tree).

    VRML scene also takes care of initiating and managing VRML events
    and routes mechanism (see ProcessEvents).

    Note that when you use this class and you directly
    change the scene within RootNode, you'll have to use our
    @code(Changed*) methods to notify this class about changes.
    Although whole @link(TVRMLNode) class works very nicely and you
    can freely change any part of it, add, delete and move nodes around
    and change their properties, this class has to be manually (for now)
    notified about such changes. Basically you can just call @link(ChangedAll)
    after changing anything inside @link(RootNode), but you should
    also take a look at other @code(Changed*) methods defined here.

    If the scene is changed by VRML events, all changes are automagically
    acted upon, so you don't have to do anything. In other words,
    this class takes care to automatically internally call appropriate @code(Changed*)
    methods when events change field values and such.

    This class provides many functionality.
    For more-or-less static scenes, many things are cached and work very
    quickly.
    E.g. methods LocalBoundingBox, BoundingBox, VerticesCount, TrianglesCount
    cache their results so after the first call to @link(TrianglesCount)
    next calls to the same method will return instantly (assuming
    that scene did not change much). And the @link(Shapes) tree
    is the main trick for various processing of the scene, most importantly
    it's the main trick to write a flexible OpenGL renderer of the VRML scene.

    Also, VRML2ActiveLights are magically updated for all states in
    @link(Shapes) tree. This is crucial for lights rendering in VRML >= 2.0. }
  TVRMLScene = class(TUIControl)
  private
    FOwnsRootNode: boolean;
    FShapes: TVRMLShapeTree;
    FRootNode: TVRMLNode;
    FOnPointingDeviceSensorsChange: TNotifyEvent;
    FNavigator: TNavigator;
    FAngleOfViewX: Single;
    FAngleOfViewY: Single;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;

    { This always holds pointers to all TVRMLShapeTreeLOD instances in Shapes
      tree. }
    ShapeLODs: TObjectList;
    { Recalculate and update LODTree.Level (if viewer position known).
      Also sends level_changed when needed. }
    procedure UpdateLODLevel(LODTree: TVRMLShapeTreeLOD);
  private
    TransformNodesInfo: TDynTransformNodeInfoArray;

    ChangedAll_TraversedLights: TDynActiveLightArray;

    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLSceneValidities;
    function CalculateBoundingBox: TBox3d;
    function CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
    function CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
  private
    FShapesActiveCount: Cardinal;
    FShapesActiveVisibleCount: Cardinal;

    { If appropriate fvXxx is not in Validities, then
      - free if needed appropriate FTrianglesList[] item
      - calculate appropriate FTrianglesList[] item
      - add appropriate fvXxx to Validities. }
    procedure ValidateTrianglesList(OverTriangulate: boolean);
  private
    FTrianglesList:
      array[boolean { OverTriangulate ?}] of TDynTriangle3SingleArray;
  private
    FTrianglesListShadowCasters: TDynTrianglesShadowCastersArray;

    { Removes fvTrianglesListShadowCasters from Validities,
      and clears FTrianglesListShadowCasters variable. }
    procedure InvalidateTrianglesListShadowCasters;

    { Removes fvTrianglesList[Not]OverTriangulate from Validities,
      and clears FTrianglesList[] variable. }
    procedure InvalidateTrianglesList(const OverTriangulate: boolean);

    function GetViewpointCore(
      const OnlyPerspective: boolean;
      out CamKind: TVRMLCameraKind;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string):
      TVRMLViewpointNode;
  private
    FManifoldEdges: TDynManifoldEdgeArray;
    FBorderEdges: TDynBorderEdgeArray;
    FOwnsManifoldAndBorderEdges: boolean;

    { Removes fvManifoldAndBorderEdges from Validities,
      and clears FManifold/BordEdges variables. }
    procedure InvalidateManifoldAndBorderEdges;

    procedure CalculateIfNeededManifoldAndBorderEdges;

    procedure FreeResources_UnloadTextureData(Node: TVRMLNode);
    procedure FreeResources_UnloadTexture3DData(Node: TVRMLNode);
    procedure FreeResources_UnloadBackgroundImage(Node: TVRMLNode);
  private
    FOnGeometryChanged: TVRMLSceneGeometryChanged;
    FOnViewpointsChanged: TVRMLSceneNotification;
    FOnBoundViewpointVectorsChanged: TVRMLSceneNotification;

    FProcessEvents: boolean;
    procedure SetProcessEvents(const Value: boolean);
  private
    { This is collected by CollectNodesForEvents. @nil if not ProcessEvents. }
    KeySensorNodes, TimeSensorNodes, MovieTextureNodes: TVRMLNodesList;
    ProximitySensorInstances: TDynProximitySensorInstanceArray;

    procedure CollectNodesForEvents;
    procedure TraverseForEvents(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    procedure CollectNodeForEvents(Node: TVRMLNode);
    procedure UnCollectForEvents(Node: TVRMLNode);

    procedure ScriptsInitialize(Node: TVRMLNode);
    procedure ScriptsDeInitialize(Node: TVRMLNode);
  private
    FWorldTime: TVRMLTime;

    { Internal procedure that handles WorldTime changes. }
    procedure InternalSetWorldTime(
      const NewValue: TVRMLTime; const TimeIncrease: TKamTime);

    procedure ResetLastEventTime(Node: TVRMLNode);
  private
    { Bindable nodes helpers }
    FBackgroundStack: TVRMLBindableStack;
    FFogStack: TVRMLBindableStack;
    FNavigationInfoStack: TVRMLBindableStack;
    FViewpointStack: TVRMLViewpointStack;

    { Mechanism to schedule geometry changed calls.

      Since DoGeometryChanged call may be costly (updating some octrees),
      and it's not immediately needed by TVRMLScene or TVRMLNode hierarchy,
      it's sometimes not desirable to call DoGeometryChanged immediately
      when geometry changed.

      This mechanism allows to defer calling DoGeometryChanged.
      Idea: BeginGeometryChangedSchedule increases internal GeometrySchedule
      counter, EndGeometryChangedSchedule decreases it and calls
      actual DoGeometryChanged if counter is zero and some
      ScheduleGeometryChanged was called in between.

      When ScheduleGeometryChanged is called when counter is zero,
      DoGeometryChanged is called immediately, so it's safe to always
      use ScheduleGeometryChanged instead of direct DoGeometryChanged
      in this class. }
    GeometrySchedule: Cardinal;
    GeometryChangedScheduled: boolean;
    procedure BeginGeometryChangedSchedule;
    procedure ScheduleGeometryChanged;
    procedure EndGeometryChangedSchedule;
  private
    { Everything changed. All octrees must be rebuild, old State pointers
      may be invalid.

      Every ChangedAll call does this.
      ChangedAll must take into account that everything could change.
      Note that ChangedAll traverses the VRML graph again,
      recalculating State values... so the old States are not
      correct anymore. You have to rebuild the octree or your pointers
      will be bad. }
    ScheduledGeometryChangedAll: boolean;

    { Transformation of some shape changed. }
    ScheduledGeometrySomeCollidableTransformChanged: boolean;
    ScheduledGeometrySomeVisibleTransformChanged: boolean;

    { What is considered "active" shapes changed. Like after Switch.whichChoice
      change. }
    ScheduledGeometryActiveShapesChanged: boolean;

    { Mechanism to schedule ChangedAll and GeometryChanged calls. }
    ChangedAllSchedule: Cardinal;
    ChangedAllScheduled: boolean;

    FPointingDeviceOverItem: PVRMLTriangle;
    FPointingDeviceActive: boolean;
    FPointingDeviceActiveSensor: TNodeX3DPointingDeviceSensorNode;
    procedure SetPointingDeviceActive(const Value: boolean);
  private
    FLogChanges: boolean;

    { Call this when ProximitySensorInstance changed (either the box or
      it's transformation) or when viewer position changed
      (in the future, this will include also implicit changes to viewer position
      by changing transformation of viewer's Viewpoint --- not interesting
      for now, since transforming Viewpoint does nothing for now).

      Viewer position/dir/up must at this point be stored within
      LastViewerXxx. }
    procedure ProximitySensorUpdate(var PSI: TProximitySensorInstance);
  private
    FLastViewerPosition, FLastViewerDirection, FLastViewerUp: TVector3Single;
    FIsLastViewer: boolean;

    FCompiledScriptHandlers: TDynCompiledScriptHandlerInfoArray;

    function OverrideOctreeLimits(
      const BaseLimits: TOctreeLimits;
      const OP: TSceneOctreeProperties): TOctreeLimits;

    { Create octree containing all triangles or shapes from our scene.
      Create octree, inits it with our BoundingBox
      and adds shapes (or all triangles from our Shapes).

      Triangles are generated using calls like
      @code(Geometry.Triangulate(State, false, ...)).
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
      the whole scene from some TVRMLScene object, or it contains
      the scene from many TVRMLScene objects, or something else.

      What I want to say is that it's generally wrong to think of
      an octree as something that maps 1-1 to some TVRMLScene object.
      Octrees, as implemented here, are a lot more flexible.

      @groupBegin }
    function CreateTriangleOctree(const Limits: TOctreeLimits;
      const ProgressTitle: string;
      const Collidable: boolean): TVRMLTriangleOctree;
    function CreateShapeOctree(const Limits: TOctreeLimits;
      const ProgressTitle: string;
      const Collidable: boolean): TVRMLShapeOctree;
    { @groupEnd }
  private
    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; Geometry: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  private
    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FShapeOctreeLimits: TOctreeLimits;
    FShapeOctreeProgressTitle: string;

    FOctreeRendering: TVRMLShapeOctree;
    FOctreeDynamicCollisions: TVRMLShapeOctree;
    FOctreeVisibleTriangles: TVRMLTriangleOctree;
    FOctreeCollidableTriangles: TVRMLTriangleOctree;

    FSpatial: TVRMLSceneSpatialStructures;
    procedure SetSpatial(const Value: TVRMLSceneSpatialStructures);
  private
    FMainLightForShadowsExists: boolean;
    FMainLightForShadows: TVector4Single;
    FMainLightForShadowsNode: TVRMLLightNode;
    FMainLightForShadowsTransform: TMatrix4Single;
    procedure SearchMainLightForShadows(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    { Based on FMainLightForShadowsNode and FMainLightForShadowsTransform,
      calculate FMainLightForShadows (position). }
    procedure CalculateMainLightForShadowsPosition;
    procedure ValidateMainLightForShadows;
  private
    FHeadlight: TVRMLHeadlight;
    FHeadlightInitialized: boolean;
    procedure SetHeadlightInitialized(const Value: boolean);
    property HeadlightInitialized: boolean
      read FHeadlightInitialized
      write SetHeadlightInitialized default false;

    procedure CameraChanged(RenderState: TRenderState);
  protected
    { Called when LightNode fields changed, while LightNode is in
      active part of VRML graph. }
    procedure ChangedActiveLightNode(LightNode: TVRMLLightNode;
      Field: TVRMLField); virtual;

    { Notify scene that you changed only given Shape.
      This means that you changed only fields within Shape.Geometry,
      Shape.State.Last*, Shape.State.ParentShape. And you're sure that
      these nodes are not shared by other shapes using VRML DEF/USE
      mechanism.

      You can call this only if given Shape is in our @link(Shapes) tree.

      Set TransformOnly = @true if you know that you changed
      only the State parts of the associatated TVRMLShape, and only
      on Transform-related fields (see EqualsNoTransform).
      Setting TransformOnly = @true is very beneficial if you
      use TVRMLGLScene with roSeparateShapesNoTransform.

      Pass TransformOnly = @false if unsure, this is safer.

      Pass InactiveOnly = @true is you know that shape is fully in inactive
      VRML graph part (inactive Switch, LOD etc. children). }
    procedure ChangedShapeFields(Shape: TVRMLShape;
      const TransformOnly, InactiveOnly, TextureImageChanged, PossiblyLocalGeometryChanged: boolean); virtual;

    { Create TVRMLShape (or descendant) instance suitable for this
      TVRMLScene descendant. In this class, this simply creates new
      TVRMLShape instance. If you make a descendant of TVRMLScene,
      you may need to store some per-shape information, and then it may
      be useful to have your own TVRMLShape descendant to carry this information.
      So you can override this to create your own descendant, and then
      you're sure that all leafs within Shapes tree are created using
      this.

      Example: TVRMLGLScene uses this to create TVRMLGLShape. }
    function CreateShape(AGeometry: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState): TVRMLShape; virtual;

    { Create TVRMLHeadLight instance suitable for this TVRMLScene descendant.
      In this class, this simply creates new TVRMLHeadLight instance.
      You can override it in descendants to create something more specialized. }
    function CreateHeadLightInstance
      (HeadLightNode: TNodeKambiHeadLight): TVRMLHeadLight; virtual;
  protected
    GeneratedTextures: TDynGeneratedTextureArray;

    { Called after PointingDeviceSensors list (possibly) changed,
      or when PointingDeviceActiveSensor (possibly) changed.
      In this class, DoPointingDeviceSensorsChange just calls
      OnPointingDeviceSensorsChange. }
    procedure DoPointingDeviceSensorsChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
    constructor Create(const SceneFileName: string);
    destructor Destroy; override;

    { Simple (usually very flat) tree of shapes within this VRML scene.

      Contents of this tree are read-only from outside.

      Note that the only place where @link(Shapes) structure is rebuild
      in this class is ChangedAll procedure.
      So e.g. if you want to do something after each change of
      @link(Shapes) tree, you can simply override ChangedAll
      and do your work after calling "inherited". }
    property Shapes: TVRMLShapeTree read FShapes;

    { Number of active shapes in the @link(Shapes) tree.
      This is equivalent to Shapes.ShapesCount(true), except that this
      is faster (it's cached and reused in this instance, and automatically
      invalidated only when needed). }
    function ShapesActiveCount: Cardinal;

    { Number of active and visible (TVRMLShape.Visible) shapes in the
      @link(Shapes) tree.

      @seealso ShapesActiveCount }
    function ShapesActiveVisibleCount: Cardinal;

    { Calculate bounding box, number of triangls and vertexes of all
      shapa states. For detailed specification of what these functions
      do (and what does OverTriangulate mean) see appropriate
      VRMLNodes.TNodeGenaralShape methods. Here, we just sum results
      of TNodeGenaralShape methods for all shapes.
      @groupBegin }
    function BoundingBox: TBox3d;
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;
    { @groupEnd }

    { Methods to notify this class about changes to the underlying RootNode
      graph. Since this class caches some things, it has to be notified
      when you manually change something within RootNode graph. }

    { Notify scene that potentially everything changed
      in the VRML graph. This includes adding/removal of some nodes within
      RootNode graph and changing their fields' values.

      ChangedAll causes recalculation of all things dependent on RootNode,
      so it's very costly to call this. While you have to call some ChangedXxx
      method after you changed RootNode graph directly, usually you
      can call something more efficient, like ChangedFields.

      @italic(Descendant implementors notes:) ChangedAll and
      ChangedShapeFields are virtual, so of course you can override them
      (remember to always call @code(inherited)). ChangedAll is also
      called by constructor of this class, so you can put a lot of your
      initialization there (instead of in the constructor). }
    procedure ChangedAll; virtual;

    { Notify scene that you changed field values of given Node.
      This does relatively intelligent discovery of what could be possibly
      affected by this node's fields, and updates/invalidates
      internal cache only where needed.

      If you changed exactly one field, you should pass
      the field's instance to ChangedFields. This often allows
      for even more optimizations (as then we know exactly what changed).
      Or you can pass field's eventIn or eventOut that you used to change.
      For ChangedFields(Node, FieldOrEvent) call,
      FieldOrEvent must belong to the given Node.
      It's usually more comfortable to use ChangedField(Field) if
      only one field was changed.

      Pass FieldOrEvent = @nil if you don't know this, or if many fields changed.

      It's acceptable to pass here a Node that
      isn't in our VRML graph part (isn't reachable from RootNode).
      Note that changes to inactive nodes (within RootNode graph,
      but inactive, i.e. not reachable by RootNode.Traverse --- for example,
      not chosen children of Switch node) even @italic(must) be reported
      here, just like changes to active parts. That's because our Shapes
      tree may contain some precalculated values even for inactive parts,
      see TVRMLShapeTreeSwitch for examples.
      Node passed here can also be one of StateDefaultNodes that you took
      from one of State.LastNodes[]. So we really handle all cases here,
      and passing any node is fine here, and we'll try to intelligently
      detect what this change implicates for this VRML scene. }
    procedure ChangedFields(Node: TVRMLNode; FieldOrEvent: TVRMLFieldOrEvent);

    { Notify scene that you changed only the value of given field.

      This is actually just a shortcut for ChangedFields(Field.ParentNode, Field),
      so don't expect to get more optimizations than ChangedFields.
      It's just shorter to type in many circumstances. }
    procedure ChangedField(Field: TVRMLField);

    { Notification when geometry changed.
      "Geometry changed" means that the positions
      of triangles changed. This is not send when merely things like
      material changed.

      What exactly changed can be checked by looking at
      ScheduledGeometryChangedAll,
      ScheduledGeometrySomeCollidableTransformChanged,
      ScheduledGeometrySomeVisbibleTransformChanged
      ScheduledGeometryActiveShapesChanged,
      TVRMLShape.ScheduledLocalGeometryChangedCoord (in all Shapes),
      TVRMLShape.ScheduledLocalGeometryChanged (in all Shapes).
      Something from there must be @true. The sum of these
      ScheduledGeometryXxx flags describe the change.

      When OnGeometryChanged, we're right after DoGeometryChanged did
      octree updating (so octrees are already updated), and right before
      we cleared the ScheduledGeometryXxx flags. So you can
      check ScheduledGeometryXxx flags, and do whatever you need.

      This is particularly useful when using ProcessEvents = @true,
      since then you don't have control over when ChangedXxx are called.
      (When ProcessEvents = @false, you always call ChangedXxx explicitly
      and in most cases you already know when the geometry did and when
      it did not change.) }
    property OnGeometryChanged: TVRMLSceneGeometryChanged
      read FOnGeometryChanged write FOnGeometryChanged;

    { Notification when the list of viewpoints in the scene possibly
      changed.

      Note that this doesn't necessarily mean that the current,
      bound viewpoint changed (although it could).
      If you only want to get notified when currently @italic(bound)
      viewpoint changes, then what you seek is rather
      @link(TVRMLBindableStack.OnBoundChanged ViewpointStack.OnBoundChanged). }
    property OnViewpointsChanged: TVRMLSceneNotification
      read FOnViewpointsChanged write FOnViewpointsChanged;

    { Notification when the currently bound viewpoint's vectors
      (position/orientation and such) changed.

      More precisely, this is called whenever values generated by
      ViewpointStack.Top.GetCameraVectors changed.

      It cannot be called when ViewpointStack.Top = @nil.
      Note that this also doesn't notify you about changes to
      currently bound viewpoint, for this you rather want to use
      @link(TVRMLBindableStack.OnBoundChanged ViewpointStack.OnBoundChanged).
      This is called only when @italic(currently bound viewpoint stays
      the same, only it's vectors change). }
    property OnBoundViewpointVectorsChanged: TVRMLSceneNotification
      read FOnBoundViewpointVectorsChanged write FOnBoundViewpointVectorsChanged;

    { Something visible changed.

      In this class, just calls VisibleChange, that calls OnVisibleChange
      if assigned.

      Changes is a set describing what changes occured that caused this
      redisplay. It can be [], meaning "something else", we'll
      still make OnVisibleChange then. See TVisibleSceneChange
      docs for possible values. It must specify all parts that possibly
      changed.

      When you want to call OnVisibleChange from your own code,
      and there's a chance that Changes <> [],
      it's important that you call this (not directly call OnVisibleChange).
      That's because descendant TVRMLGLScene does important updates for
      generated textures (potentially instructing them to regenerate
      next frame) when Changes <> []. }
    procedure VisibleSceneChange(const Changes: TVisibleSceneChanges); virtual;

    { Call OnGeometryChanged, if assigned. }
    procedure DoGeometryChanged; virtual;

    { Call OnViewpointsChanged, if assigned. }
    procedure DoViewpointsChanged;

    { Call OnBoundViewpointVectorsChanged, if assigned. }
    procedure DoBoundViewpointVectorsChanged;

    { Mechanism to schedule ChangedAll and GeometryChanged calls.

      Since these calls may be costly (updating some octrees,
      traversing the hierarchy), and their results are often
      not immediately needed by TVRMLScene or TVRMLNode hierarchy,
      it's sometimes not desirable to call them immediately
      when geometry changed / all changed.

      So you can use ScheduleChangedAll instead of ChangedAll.
      All event handlers within TVRMLScene already do this.

      When you're within Begin/EndChangesSchedule, then
      ScheduleChangedAll just sets an internal flag and actual ChangedAll
      will be done only once at EndChangesSchedule. Otherwise
      (when not within Begin/EndChangesSchedule), ScheduleChangedAll will
      immediately call ChangedAll.

      Begin/EndChangesSchedule set up ChangedAll schedule,
      and also do Begin/EndGeometryChangedSchedule. So the analogous mechanism
      will be used to avoid calling GeometryChanged too often.

      @groupBegin }
    procedure ScheduleChangedAll;
    procedure BeginChangesSchedule;
    procedure EndChangesSchedule;
    { @groupEnd }

    { Returns short information about the scene.
      This consists of a few lines, separated by KambiUtils.NL.
      Last line also ends with KambiUtils.NL.

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

    { Write contents of all VRML "Info" nodes.
      Also write how many Info nodes there are in the scene. }
    procedure WritelnInfoNodes;

    { Actual VRML graph defining this VRML scene.
      This class can be viewed as a wrapper around specified RootNode.

      As such it is allowed to change contents of RootNode
      (however, this object requires notification of such changes
      via ChangedXxx methods, see their docs).
      Moreover it is allowed to change value of RootNode
      and even to set RootNode to nil for some time.

      This is useful for programs like view3dscene, that want to
      have one TVRMLScene for all the lifetime and only
      replace RootNode value from time to time.
      This is useful because it allows to change viewed model
      (by changing RootNode) while preserving values of things
      like Attributes properties in subclass @link(TVRMLGLScene).
      This is also used internally when user clicks on Anchor node,
      if you have ProcessEvents.

      That's why it is possible to change RootNode and it is even
      possible to set it to nil. And when When RootNode = nil everything
      should work -- you can query such scene (with RootNode = nil)
      for Vertices/TrianglesCount (answer will be 0),
      for BoundingBox (answer will be EmptyBox3d),
      you can render such scene (nothing will be rendered) etc.
      Scene RootNode = nil will act quite like a Scene with
      e.g. no TVRMLGeometryNode nodes.

      Always call ChangedAll when you changed RootNode.

      Note that there is also a trick to conserve memory use.
      After you've done PrepareRender some things are precalculated here,
      and RootNode is actually not used, unless you use ProcessEvent.
      So you can free RootNode
      (and set it to nil here) @italic(without calling ChangedAll)
      and some things will just continue to work, unaware of the fact
      that the underlying RootNode structure is lost.
      Note that this is still considered a "dirty trick", and you will
      have to be extra-careful then about what methods/properties
      from this class. Generally, use only things that you prepared
      with PrepareRender. So e.g. calling Render or using BoundingBox.
      If all your needs are that simple, then you can use this trick
      to save some memory. This is actually useful when using TVRMLGLAnimation,
      as it creates a lot of intermediate node structures and TVRMLScene
      instances. }
    property RootNode: TVRMLNode read FRootNode write FRootNode;

    { If @true, RootNode will be freed by destructor of this class. }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode;

    { The dynamic octree containing all visible shapes.
      It's useful for "frustum culling", it will be automatically
      used by TVRMLGLScene.RenderFrustum to speed up the rendering.

      This octree will be automatically updated on dynamic scenes
      (when e.g. animation moves some shape by changing it's transformation).

      Add ssRendering to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects.  }
    property OctreeRendering: TVRMLShapeOctree read FOctreeRendering;

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
    property OctreeDynamicCollisions: TVRMLShapeOctree read FOctreeDynamicCollisions;

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
    property OctreeVisibleTriangles: TVRMLTriangleOctree read FOctreeVisibleTriangles;

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
    property OctreeCollidableTriangles: TVRMLTriangleOctree read FOctreeCollidableTriangles;

    { Octree for collisions. This returns either OctreeCollidableTriangles
      or OctreeDynamicCollisions, whichever is available (or @nil if none).
      Be sure to add ssDynamicCollisions or ssCollidableTriangles to have
      this available. }
    function OctreeCollisions: TVRMLBaseTrianglesOctree;

    { Which spatial structures (octrees, for now) should be created and managed.

      You should set this, based on your expected usage of octrees.
      See TVRMLSceneSpatialStructure for possible values.
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
      collision detection would unnecessarily create octrees at construction. }
    property Spatial: TVRMLSceneSpatialStructures read FSpatial write SetSpatial;

    { Properties of created triangle octrees.
      See VRMLTriangleOctree unit comments for description.

      Default value comes from DefTriangleOctreeLimits.

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_octree_properties].

      @groupBegin }
    function TriangleOctreeLimits: POctreeLimits;

    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;
    { @groupEnd }

    { Properties of created shape octrees.
      See VRMLShapeOctree unit comments for description.

      Default value comes from DefShapeOctreeLimits.

      If ShapeOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      (so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_octree_properties].

      @groupBegin }
    function ShapeOctreeLimits: POctreeLimits;

    property ShapeOctreeProgressTitle: string
      read  FShapeOctreeProgressTitle
      write FShapeOctreeProgressTitle;
    { @groupEnd }

    { GetViewpoint and GetPerspectiveViewpoint return the properties
      of the defined Viewpoint in VRML file, or some default viewpoint
      properties if no viewpoint is found in RootNode graph. They seek for
      nodes Viewpoint (for VRML 97) or OrthoViewpoint (any X3DViewpointNode
      actually, for X3D), PerspectiveCamera and OrthographicCamera
      (for VRML 1.0) in the scene graph.

      GetPerspectiveViewpoint omits OrthographicCamera andy OrthoViewpoint.

      If ViewpointDescription = '', they return the first found viewpoint node.
      Otherwise, they look for Viewpoint or OrthoViewpoint (any X3DViewpointNode
      actually) with description field mathing given string.

      If some viewpoint will be found (in the active VRML graph
      under RootNode), then "out" parameters
      will be calculated to this viewpoint position, direction etc.
      If no such thing is found in the VRML graph, then returns
      default viewpoint properties from VRML spec (CamPos = (0, 0, 1),
      CamDir = (0, 0, -1), CamUp = GravityUp = (0, 1, 0), CamType = ctPerspective).

      If camera properties were found in some node,
      it returns this node. Otherwise it returns nil.
      This way you can optionally extract some additional info from
      used viewpoint node, or do something special if default values
      were used. Often you will just ignore result of this function
      --- after all, the most important feature of this function
      is that you don't @italic(have) to care about details of
      dealing with camera node.

      Returned CamDir and CamUp and GravityUp are always normalized ---
      reasons the same as for TVRMLViewpointNode.GetCameraVectors.

      @groupBegin }
    function GetViewpoint(
      out CamKind: TVRMLCameraKind;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string = ''):
      TVRMLViewpointNode;

    function GetPerspectiveViewpoint(
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string = ''):
      TVRMLViewpointNode;
    { @groupEnd }

    { Currently bound fog for this scene.

      FogNode is just a trivial shortcut for FogStack.Top.
      It returns currently bound Fog node, or @nil if none.

      FogDistanceScaling returns scaling of this FogNode, taken
      from the transformation of FogNode in VRML graph.
      You should always multiply FogNode.FdVisibilityRange.Value
      by FogDistanceScaling when applying (rendering etc.) this fog node.
      Value of FogDistanceScaling is undefined if FogNode = nil.

      Currently, FogDistanceScaling is just a trivial shortcut
      for FogNode.AverageScaleTransform. So it's fast.

      @groupBegin }
    function FogNode: TNodeFog;
    function FogDistanceScaling: Single;
    { @groupEnd }

    { This returns an array of all triangles of this scene.
      I.e. it triangulates the scene, adding all non-degenerated
      (see IsValidTriangles) triangles to the list.

      It's your responsibility what to do with resulting
      object, when to free it etc. }
    function CreateTrianglesList(OverTriangulate: boolean):
      TDynTriangle3SingleArray;

    { Just like CreateTrianglesList, but results of this function are cached,
      and returned object is read-only for you. Don't modify it, don't free it. }
    function TrianglesList(OverTriangulate: boolean):
      TDynTriangle3SingleArray;

    { Returns an array of triangles that should be shadow casters
      for this scene.

      Additionally, TDynTrianglesShadowCastersArray contains some
      additional information needed for rendering with shadows:
      currently, this means TDynTrianglesShadowCastersArray.OpaqueCount.

      Results of these functions are cached, and are also owned by this object.
      So don't modify it, don't free it. }
    function TrianglesListShadowCasters: TDynTrianglesShadowCastersArray;

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
    function ManifoldEdges: TDynManifoldEdgeArray;
    function BorderEdges: TDynBorderEdgeArray;
    { @groupEnd }

    { This allows you to "share" @link(ManifoldEdges) and
      @link(BorderEdges) values between TVRMLScene instances,
      to conserve memory and preparation time.
      The values set here will be returned by following ManifoldEdges and
      BorderEdges calls. The values passed here will @italic(not
      be owned) by this object --- you gave this, you're responsible for
      freeing it.

      This is handy if you know that this scene has the same
      ManifoldEdges and BorderEdges contents as some other scene. In particular,
      this is extremely handy in cases of animations in TVRMLGLAnimation,
      where all scenes actually need only a single instance of TDynManifoldEdgeArray
      and TDynBorderEdgeArray,
      this greatly speeds up TVRMLGLAnimation loading and reduces memory use.

      Note that passing here as values the same references
      that are already returned by ManifoldEdges / BorderEdges is always
      guaranteed to be a harmless operation. If ManifoldEdges  / BorderEdges
      was owned by this object,
      it will remain owned in this case (while in normal sharing situation,
      values set here are assumed to be owned by something else). }
    procedure ShareManifoldAndBorderEdges(
      ManifoldShared: TDynManifoldEdgeArray;
      BorderShared: TDynBorderEdgeArray);

    { Frees some scene resources, to conserve memory.
      See TVRMLSceneFreeResources documentation. }
    procedure FreeResources(Resources: TVRMLSceneFreeResources);

    { Should the VRML event mechanism work.

      If @true, then we will implement whole VRML event mechanism here,
      as expected from a VRML browser. Events will be send and received
      through routes, time dependent nodes (X3DTimeDependentNode,
      like TimeSensor) will be activated and updated from WorldTime time
      property, KeyDown, KeyUp and other methods will activate
      key/mouse sensor nodes, scripts will be initialized and work, etc.

      Appropriate ChangedXxx, like ChangedAll, will be automatically called
      when necessary.

      In other words, this makes the scene fully animated and interacting
      with the user (provided you will call KeyDown etc. methods when
      necessary).

      If @false, this all doesn't work, which makes the scene static. }
    property ProcessEvents: boolean
      read FProcessEvents write SetProcessEvents default false;

    { Internally unregister a node from our events processing.

      @italic(You almost never need to call this method) --- this
      is done automatically for you when ProcessEvents is changed
      between @true and @false, including when TVRMLScene is destroyed.
      However, if you process RootNode graph
      and extract some node from it (that is, delete node from our
      RootNode graph, but instead of freeing it you insert it
      into some other VRML graph) you must call it to manually
      "untie" this node (and all it's children) from this TVRMLScene instance.

      Reason: when ProcessEvents is activated,
      we set Self as node's EventsProcessor to get event
      changes notifications to ChangedXxx methods.
      When ProcessEvents is deactivated (including when this TVRMLScene
      instance is released) we revert this. If you add new node
      to VRML graph, we make sure (in ChangedAll) that EventsProcessor
      is set correctly for all new nodes. Buf if you deleted a node
      from our graph, we have no chance to remove this...
      You have to call UnregisterProcessEvents then manually,
      if you don't want the Node to be "tied" to our TVRMLScene.

      In really really corner cases, when you're not sure whether you
      this Node with all it's children, you should make sure to call
      this before calling ChangedAll. This way, this will unregister
      this Node and all it's children from our event processing,
      and ChangedAll will re-register again these children which are
      detected to still be part of our graph. }
    procedure UnregisterProcessEvents(Node: TVRMLNode);

    function KeyDown(Key: TKey; C: char): boolean; override;
    function KeyUp(Key: TKey; C: char): boolean; override;

    { Call this to when pointing-device moves.
      This may generate the continously-generated events like
      hitPoint_changed, also it updates PointingDeviceOverItem,
      thus producing isOver and such events.

      OverItem may be @nil to indicate we're not over any item.
      In this case, OverPoint is ignored. }
    procedure PointingDeviceMove(const OverPoint: TVector3Single;
      const OverItem: PVRMLTriangle);

    { Current item over which the pointing device is. @nil if over none.
      For example, you can investigate it's pointing device sensors
      (in PointingDeviceOverItem^.State.PointingDeviceSensors),
      although there's a shortcut for just this in @link(PointingDeviceSensors).
      You can change this by PointingDeviceMove and PointingDeviceClear. }
    property PointingDeviceOverItem: PVRMLTriangle
      read FPointingDeviceOverItem write FPointingDeviceOverItem;

    { Pointing-device sensors over which the pointing device is.
      This is just a shortcut for
      PointingDeviceOverItem^.State.PointingDeviceSensors,
      returning @nil if PointingDeviceOverItem = @nil. }
    function PointingDeviceSensors: TPointingDeviceSensorsList;

    { Currently active sensor. @nil if none.
      Always @nil when PointingDeviceActive = @false.

      Note that sensor specified here doesn't have to be one of the sensors of
      PointingDeviceOverItem. When some sensor is activated, it grabs
      further events until it's deactivated (like you set
      PointingDeviceActive := false, which means that user released mouse
      button). This means that when user moves the mouse while given
      sensor is active, he can move mouse over other items, even the ones
      where the sensor isn't listen --- but the sensor remains active. }
    property PointingDeviceActiveSensor: TNodeX3DPointingDeviceSensorNode
      read FPointingDeviceActiveSensor;

    { Clear any references to OverItem passed previously to PointingDeviceMove.
      This is sometimes useful, because PointingDeviceMove may save
      the last passed OverItem, and SetPointingDeviceActive may even
      save some sensor of it for a longer time.

      You could free it by calling
      @code(PointingDeviceMove(nil)), but this could cause other VRML events,
      so it's undesirable to call this when you're going to e.g. release
      the scene or it's octree. Also, you would have to deactivate sensor
      first, causing even more events.

      So this method clears any references to saved OverItem and
      PointingDeviceActiveSensor, without calling any events. }
    procedure PointingDeviceClear;

    { Change this to indicate whether pointing device is currently active
      (for example, mouse button is pressed down) or not. }
    property PointingDeviceActive: boolean
      read FPointingDeviceActive
      write SetPointingDeviceActive default false;

    { Event called after PointingDeviceSensors list (possibly) changed,
      or when PointingDeviceActiveSensor (possibly) changed.
      @seeAlso DoPointingDeviceSensorsChange }
    property OnPointingDeviceSensorsChange: TNotifyEvent
      read FOnPointingDeviceSensorsChange
      write FOnPointingDeviceSensorsChange;

    { Call mouse down / up / move to have PointiDeviceXxx stuff automatically
      handled.

      To make mouse move actually working (so that VRML touch sensors work Ok),
      make sure you have non-nil OctreeCollisions (e.g. include
      ssDynamicCollisions in @link(Spatial)).


      @groupBegin }
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    { @groupEnd }

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { Overridden in TVRMLScene to catch events regardless of mouse position. }
    function PositionInside(const X, Y: Integer): boolean; override;

    { Navigator (camera) in this scene. May be @nil if not known / not used.

      Your navigator must be inside some container
      (i.e. on TGLUIWindow.Controls or TKamOpenGLControl.Controls list),
      at least at the time of Mouse* methods call
      (that is, when container passed mouse events to this scene).

      This is for now used only with Mouse* methods, to convert mouse position
      (MouseX, MouseY) to a picked ray in 3D space. }
    property Navigator: TNavigator read FNavigator write FNavigator;

    { Camera angles of view, in degrees.
      Automatically set by every TVRMLGLScene.GLProjection call.
      But you can also change them manually, if you need more control
      for some reason.

      These are for now used only with Mouse* methods, to convert mouse position
      (MouseX, MouseY) to a picked ray in 3D space.

      @groupBegin }
    property AngleOfViewX: Single read FAngleOfViewX write FAngleOfViewX;
    property AngleOfViewY: Single read FAngleOfViewY write FAngleOfViewY;
    { @groupEnd }

    { These change world time, see WorldTime.
      It is crucial that you call this continously to have some VRML
      time-dependent features working, like TimeSensor and MovieTexture.
      See WorldTime for details what is affected by this.

      This is automatically taken care of if you added this scene
      to TGLUIWindow.Controls or TKamOpenGLControl.Controls.
      Then our @link(Idle) takes care of doing the job,
      according to TimePlaying and TimePlayingSpeed.

      This causes time to be passed to appropriate time-dependent nodes,
      events will be called etc.

      SetWorldTime and IncreaseWorldTime do exactly the same, the difference is
      only that for IncreaseWorldTime you specify increase in the time
      (that is, NewTime = WorldTime + TimeIncrease). Use whichever version
      is more handy.

      Following X3D specification, time should only grow. So NewValue should
      be > WorldTime (or TimeIncrease > 0).
      Otherwise we ignore this call.
      For resetting the time (when you don't necessarily want to grow WorldTime)
      see ResetWorldTime.

      If a change of WorldTime will produce some visible change in VRML model
      (for example, MovieTexture will change, or TimeSensor change will be
      routed by interpolator to coordinates of some visible node) this
      will be notified by usual method, that is OnVisibleChange.

      @groupBegin }
    procedure SetWorldTime(const NewValue: TKamTime);
    procedure IncreaseWorldTime(const TimeIncrease: TKamTime);
    { @groupEnd }

    { This is the world time, that is passed to time-dependent nodes.
      See X3D specification "Time" component about time-dependent nodes.
      In short, this "drives" the time passed to TimeSensor, MovieTexture
      and AudioClip. See SetWorldTime for changing this.

      Default value is 0.0 (zero). }
    property WorldTime: TVRMLTime read FWorldTime;

    { Set WorldTime to arbitrary value.

      You should only use this when you loaded new VRML model.

      Unlike SetWorldTime and IncreaseWorldTime, this doesn't require that
      WorldTime grows. It still does some time-dependent events work,
      although some time-dependent nodes may be just unconditionally reset
      by this to starting value (as they keep local time, and so require
      TimeIncrease notion, without it we can only reset them).

      @groupBegin }
    procedure ResetWorldTime(const NewValue: TKamTime);
    { @groupEnd }

    { Set WorldTime to initial value after loading a world.

      This honours VRML specification about VRML time origin,
      and our extension
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_time_origin_at_load]. }
    procedure ResetWorldTimeAtLoad;

    { Binding stack of X3DBackgroundNode nodes.
      All descend from TNodeX3DBackgroundNode class. }
    property BackgroundStack: TVRMLBindableStack read FBackgroundStack;

    { Binding stack of Fog nodes.
      All descend from TNodeFog class. }
    property FogStack: TVRMLBindableStack read FFogStack;

    { Binding stack of NavigatinInfo nodes.
      All descend from TNodeNavigationInfo class. }
    property NavigationInfoStack: TVRMLBindableStack read FNavigationInfoStack;

    { Binding stack of Viewpoint nodes.
      All descend from TVRMLViewpointNode (not necessarily from
      TNodeX3DViewpointNode, so VRML 1.0 camera nodes are also included in
      this stack.) }
    property ViewpointStack: TVRMLViewpointStack read FViewpointStack;

    { If true, and also KambiLog.Log is true, then we will log
      ChangedFields and ChangedAll occurences. Useful only for debugging
      and optimizing VRML events engine. }
    property LogChanges: boolean
      read FLogChanges write FLogChanges default false;

    { Last known viewer (camera) position/direction/up.

      Set by ViewerChanged. IsLastViewer = @false means that
      ViewerChanged was never called, and so viewer settings are not known,
      and so other LastViewer* properties have undefined values.

      These are remembered for various reasons, like
      reacting to changes to ProximitySensor box (center, size)
      (or it's transform), or changing LOD node children.

      @groupBegin }
    property LastViewerPosition: TVector3Single read FLastViewerPosition;
    property LastViewerDirection: TVector3Single read FLastViewerDirection;
    property LastViewerUp: TVector3Single read FLastViewerUp;
    property IsLastViewer: boolean read FIsLastViewer;
    { @groupEnd }

    { Call when viewer position/dir/up changed, to update things depending
      on viewer settings. This includes sensors like ProximitySensor,
      LOD nodes, camera settings for next RenderedTexture update and more.
      It automatically does proper VisibleSceneChange (VisibleChange, OnVisibleChange).

      @param(Changes describes changes to the scene caused by viewer change.
        This is needed if some geometry / light follows the player.

        We'll automatically add here "prViewer", so not need to specify it.

        You should add here prVisibleSceneNonGeometry if player has a headlight.
        You should add here prVisibleSceneGeometry if player has a rendered
        avatar.) }
    procedure ViewerChanged(ANavigator: TNavigator;
      const Changes: TVisibleSceneChanges);

    { List of handlers for VRML Script node with "compiled:" protocol.
      This is read-only, change this only by RegisterCompiledScript. }
    property CompiledScriptHandlers: TDynCompiledScriptHandlerInfoArray
      read FCompiledScriptHandlers;

    { Register compiled script handler, for VRML Script node with
      "compiled:" protocol.
      See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_script_compiled]. }
    procedure RegisterCompiledScript(const HandlerName: string;
      Handler: TCompiledScriptHandler);

    { Create and initialize TNavigator instance based on currently
      bound NavigationInfo and Viewpoint node.

      Bound NavigationInfo node is just
      NavigationInfoStack.Top. If no NavigationInfo is bound, this is @nil,
      and we will create navigator corresponding to default NavigationInfo
      values (this is following VRML spec), so it will have type = EXAMINE.

      Sets always suitable @link(TNavigator.CameraRadius).

      This initializes many TWalkNavigator properties, if this is determined
      to be proper result class:
      @unorderedList(
        @item(TWalkNavigator.Gravity,)
        @item(TWalkNavigator.PreferGravityUpForRotations,)
        @item(TWalkNavigator.PreferGravityUpForMoving,)
        @item(TWalkNavigator.IgnoreAllInputs,)
        @item(TWalkNavigator.CameraPreferredHeight.)
      )

      This also calls NavigatorBindToViewpoint at the end,
      so navigator is bound to current vewpoint. }
    function CreateNavigator(AOwner: TComponent): TNavigator;

    { Update navigator when currently bound viewpoint changes.
      When no viewpoint is currently bound, we will go to standard (initial)
      VRML viewpoint position.

      This moves the navigator to starting point of the viewpoint,
      updating navigator's initial and current vectors
      ([Initial]CameraPos, [Initial]CameraDir, [Initial]CameraUp, GravityUp).

      Currently bound NavigationInfo.speed is also taken into account here.
      Navigator's MoveHorizontalSpeed and MoveVerticalSpeed are updated. }
    procedure NavigatorBindToViewpoint(Nav: TNavigator;
      const OnlyViewpointVectorsChanged: boolean);

    { Detect position/direction of the main light that produces shadows.
      This is useful when you want to make shadows on the scene
      from only a single light, but your scene has many lights.

      The main light is simply one with both @code(kambiShadows) and
      @code(kambiShadowsMain) fields set to @true. See
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows]
      for more info.
      If no light with kambiShadows = kambiShadowsMain = TRUE
      is present then MainLightForShadowsExists returns @false,
      and you cannot call MainLightForShadows (since it
      cannot be calculated).

      MainLightPosition[3] is always set to 1
      (positional light) or 0 (indicates that this is a directional light).

      @seealso TVRMLLightSet.MainLightForShadows

      @groupBegin }
    function MainLightForShadowsExists: boolean;
    function MainLightForShadows: TVector4Single;
    { @groupEnd }

    { Creates a headlight, using (if present) KambiHeadLight node defined
      in this VRML file. You're responsible for freeing this node.

      Note that this is @italic(not) concerned whether you
      actually should use this headlight (this information usually comes from
      NavigationInfo.headlight value).

      If you're looking for more comfortable alternative to this,
      that uses all VRML information, see @link(Headlight) property.

      @seealso Headlight }
    function CreateHeadLight: TVRMLHeadLight;

    { Headlight that should be used for this scene,
      or @nil if no headlight should be used.

      This uses (if present) NavigationInfo.headlight and KambiHeadLight
      node defined in this VRML file.

      This object is automatically managed inside this class.
      Calling this method automatically initializes it.

      If you want, it's a little dirty but allowed to directly change
      this light's properties after it's initialized. }
    function Headlight: TVRMLHeadlight;

    { Notify the scene that viewer position/direction changed a lot.
      It may be called when you make a sudden change to the camera,
      like teleporting the player to a completely different scene part.

      This may be used as a hint by some optimizations. It tells that what
      will be visible in the next rendered frame will be probably
      very different from what was visible in the last frame.

      @italic(Current implementation notes:)

      Currently, this is used by TVRMLGLScene if you use
      Attributes.UseOcclusionQuery.
      Normally, occlusion query tries to reuse results from previous
      frame, using the assumption that usually camera changes slowly
      and objects appear progressively in the view. When you make
      a sudden camera jump/change, this assumption breaks, so it's
      better to resign from occlusion query for the very next frame.
      This method will do exactly that. }
    procedure ViewChangedSuddenly; virtual;
  published
    { When TimePlaying is @true, the time of our 3D world will keep playing.
      More precisely, our @link(Idle) will take care of increasing WorldTime.
      Our @link(Idle) is usually automatically called (if you added this
      scene to TGLUIWindow.Controls or TKamOpenGLControl.Controls)
      so you don't have to do anything to make this work. }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;

    { Controls the time speed (if TimePlaying is @true):
      1.0 means that 1 second  of real time equals to 1 unit of world time. }
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;
  end;

{$undef read_interface}

implementation

uses VRMLCameraUtils, KambiStringUtils, KambiLog, VRMLErrors, DateUtils,
  Object3dAsVRML;

{$define read_implementation}
{$I macprecalcvaluereturn.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}
{$I dynarray_5.inc}
{$I dynarray_6.inc}
{$I dynarray_7.inc}

{ TVRMLBindableStack ----------------------------------------------------- }

constructor TVRMLBindableStack.Create(AParentScene: TVRMLScene);
begin
  inherited Create;
  FParentScene := AParentScene;
end;

procedure TVRMLBindableStack.SendIsBound(Node: TNodeX3DBindableNode;
  const Value: boolean);
begin
  if Node <> nil then
  begin
    Node.EventIsBound.Send(Value, ParentScene.WorldTime);
    Node.EventBindTime.Send(ParentScene.WorldTime.Seconds, ParentScene.WorldTime);
  end;
end;

function TVRMLBindableStack.Top: TNodeX3DBindableNode;
begin
  if Count <> 0 then
    Result := Items[High] as TNodeX3DBindableNode else
    Result := nil;
end;

procedure TVRMLBindableStack.Push(Node: TNodeX3DBindableNode);
begin
  Add(Node);
end;

procedure TVRMLBindableStack.PushIfEmpty(Node: TNodeX3DBindableNode;
  SendEvents: boolean);
begin
  if Count = 0 then
  begin
    Push(Node);
    if SendEvents then
      SendIsBound(Node, true);
    DoBoundChanged;
  end;
end;

function TVRMLBindableStack.Pop: TNodeX3DBindableNode;
begin
  if Count <> 0 then
  begin
    Result := Items[High] as TNodeX3DBindableNode;
    Count := Count - 1;
  end else
    Result := nil;
end;

procedure TVRMLBindableStack.CheckForDeletedNodes(RootNode: TVRMLNode;
  SendEvents: boolean);
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
      (it typecasts to TNodeX3DBindableNode). }

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
        TopChanged := I = High;
        Delete(I);
      end else
        Inc(I);
    end;

    if TopChanged and SendEvents and (Count <> 0) then
      SendIsBound(Top, true);

    if TopChanged then
      DoBoundChanged;
  end;
end;

procedure TVRMLBindableStack.Set_Bind(Node: TNodeX3DBindableNode;
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
      DoBoundChanged;
    end else
    if NodeIndex <> High then
    begin
      { Node on the stack, but not on top. So move it, as new top node. }
      SendIsBound(Top, false);
      Exchange(NodeIndex, High);
      SendIsBound(Top, true);
      DoBoundChanged;
    end;
    { set_bind = true for node already on the top is ignored. }
  end else
  begin
    if NodeIndex <> -1 then
    begin
      if NodeIndex = High then
      begin
        SendIsBound(Top, false);
        Delete(NodeIndex);
        SendIsBound(Top, true);
        DoBoundChanged;
      end else
      begin
        Delete(NodeIndex);
      end;
    end;
    { set_bind = false for node already outside of the stack is ignored. }
  end;
end;

procedure TVRMLBindableStack.DoBoundChanged;
begin
  if Assigned(OnBoundChanged) then
    OnBoundChanged(ParentScene);
end;

{ TVRMLViewpointStack -------------------------------------------------------- }

procedure TVRMLViewpointStack.DoBoundChanged;
begin
  { The new viewpoint may be in some totally different place of the scene,
    so call ViewChangedSuddenly.

    This takes care of all viewpoints switching, like
    - switching to other viewpoint through view3dscene "viewpoints" menu,
    - just getting an event set_bind = true through vrml route. }

  ParentScene.ViewChangedSuddenly;

  inherited;
end;

{ TDynGeneratedTextureArray -------------------------------------------------- }

function TDynGeneratedTextureArray.IndexOfTextureNode(TextureNode: TVRMLNode): Integer;
begin
  for Result := 0 to High do
    if Items[Result].TextureNode = TextureNode then
      Exit;
  Result := -1;
end;

function TDynGeneratedTextureArray.FindTextureNode(TextureNode: TVRMLNode): PGeneratedTexture;
var
  Index: Integer;
begin
  Index := IndexOfTextureNode(TextureNode);
  if Index <> -1 then
    Result := @(Items[Index]) else
    Result := nil;
end;

procedure TDynGeneratedTextureArray.AddShapeTexture(Shape: TVRMLShape;
  Tex: TNodeX3DTextureNode);
var
  GenTex: PGeneratedTexture;
begin
  if (Tex is TNodeGeneratedCubeMapTexture) or
     (Tex is TNodeGeneratedShadowMap) or
     (Tex is TNodeRenderedTexture) then
  begin
    GenTex := FindTextureNode(Tex);
    if GenTex <> nil then
    begin
      { The same generated texture node Tex is instantiated more than once.

        - For GeneratedShadowMap, the shape where it's used, and actually
          the whole place where it's used in the VRML graph, doesn't matter.
          (GeneratedShadowMap makes view from it's projectedLight).
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

      if (Tex is TNodeGeneratedCubeMapTexture) and
         (Shape <> GenTex^.Shape) then
        VRMLWarning(vwSerious, 'The same GeneratedCubeMapTexture node is used (instanced) within at least two different VRML shapes. This is bad, as we don''t know from which shape should environment be captured');
    end else
    begin
      GenTex := Add;
      GenTex^.TextureNode := Tex;

      if Tex is TNodeGeneratedCubeMapTexture then
        GenTex^.Handler := TNodeGeneratedCubeMapTexture(Tex).GeneratedTextureHandler else
      if Tex is TNodeGeneratedShadowMap then
        GenTex^.Handler := TNodeGeneratedShadowMap(Tex).GeneratedTextureHandler else
      if Tex is TNodeRenderedTexture then
        GenTex^.Handler := TNodeRenderedTexture(Tex).GeneratedTextureHandler else
        raise EInternalError.Create('sf34234');

      GenTex^.Shape := Shape;
    end;
  end;
end;

{ TDynTransformNodeInfoArray ------------------------------------------------- }

function TDynTransformNodeInfoArray.NodeInfo(Node: TVRMLNode): PTransformNodeInfo;
var
  I: Integer;
begin
  for I := 0 to High do
    if Node = Items[I].Node then
    begin
      Result := Pointers[I];
      Exit;
    end;
  Result := nil;
end;

{ TVRMLScene ----------------------------------------------------------- }

constructor TVRMLScene.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 { Leave FRootNode, FOwnsRootNode as they were.
   If coming from Create(ARootNode, AOwnsRootNode),
   we'll have them set to whatever caller wanted.
   Otherwise, default FRootNode is nil, which is Ok,
   and FOwnsRootNode = is false, which doesn't matter (since FRootNode is nil). }

 FTriangleOctreeLimits := DefTriangleOctreeLimits;
 FShapeOctreeLimits := DefShapeOctreeLimits;

 FShapes := TVRMLShapeTreeGroup.Create(Self);
 ShapeLODs := TObjectList.Create(false);

 FBackgroundStack := TVRMLBindableStack.Create(Self);
 FFogStack := TVRMLBindableStack.Create(Self);
 FNavigationInfoStack := TVRMLBindableStack.Create(Self);
 FViewpointStack := TVRMLViewpointStack.Create(Self);

 FCompiledScriptHandlers := TDynCompiledScriptHandlerInfoArray.Create;
 TransformNodesInfo := TDynTransformNodeInfoArray.Create;
 GeneratedTextures := TDynGeneratedTextureArray.Create;

 FTimePlaying := true;
 FTimePlayingSpeed := 1.0;

 ScheduleChangedAll;
end;

constructor TVRMLScene.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
  { Set FRootNode, FOwnsRootNode first.
    This way ScheduleChangedAll in Create will already take care of initializing
    it properly. }

  FRootNode := ARootNode;
  FOwnsRootNode := AOwnsRootNode;

  Create(nil);
end;

constructor TVRMLScene.Create(const SceneFileName: string);
begin
  Create(LoadAsVRML(SceneFileName, false), true);
end;

destructor TVRMLScene.Destroy;
begin
  { This also frees related lists, like KeySensorNodes,
    and does UnregisterProcessEvents(RootNode). }
  ProcessEvents := false;

  HeadlightInitialized := false;

 { free FTrianglesList* variables }
 InvalidateTrianglesList(false);
 InvalidateTrianglesList(true);
 InvalidateTrianglesListShadowCasters;

 { frees FManifoldEdges, FBorderEdges if needed }
 InvalidateManifoldAndBorderEdges;

 FreeAndNil(GeneratedTextures);
 FreeAndNil(TransformNodesInfo);
 FreeAndNil(FCompiledScriptHandlers);

 FreeAndNil(FBackgroundStack);
 FreeAndNil(FFogStack);
 FreeAndNil(FNavigationInfoStack);
 FreeAndNil(FViewpointStack);

 FreeAndNil(FShapes);
 FreeAndNil(ShapeLODs);

 FreeAndNil(FOctreeRendering);
 FreeAndNil(FOctreeDynamicCollisions);
 FreeAndNil(FOctreeVisibleTriangles);
 FreeAndNil(FOctreeCollidableTriangles);

 if OwnsRootNode then FreeAndNil(FRootNode);
 inherited;
end;

function TVRMLScene.ShapesActiveCount: Cardinal;
begin
  if not (fvShapesActiveCount in Validities) then
  begin
    FShapesActiveCount := Shapes.ShapesCount(true);
    Include(Validities, fvShapesActiveCount);
  end;
  Result := FShapesActiveCount;
end;

function TVRMLScene.ShapesActiveVisibleCount: Cardinal;
begin
  if not (fvShapesActiveVisibleCount in Validities) then
  begin
    FShapesActiveVisibleCount := Shapes.ShapesCount(true, true);
    Include(Validities, fvShapesActiveVisibleCount);
  end;
  Result := FShapesActiveVisibleCount;
end;

function TVRMLScene.CalculateBoundingBox: TBox3d;
var
  SI: TVRMLShapeTreeIterator;
begin
  Result := EmptyBox3d;
  SI := TVRMLShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Box3dSumTo1st(Result, SI.Current.BoundingBox);
  finally FreeAndNil(SI) end;
end;

function TVRMLScene.CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
var
  SI: TVRMLShapeTreeIterator;
begin
  Result := 0;
  SI := TVRMLShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result += SI.Current.VerticesCount(OverTriangulate);
  finally FreeAndNil(SI) end;
end;

function TVRMLScene.CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
var
  SI: TVRMLShapeTreeIterator;
begin
  Result := 0;
  SI := TVRMLShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result += SI.Current.TrianglesCount(OverTriangulate);
  finally FreeAndNil(SI) end;
end;

function TVRMLScene.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := fvBoundingBox}
{$define PRECALC_VALUE := FBoundingBox}
{$define PRECALC_VALUE_CALCULATE := CalculateBoundingBox}
PRECALC_VALUE_RETURN

function TVRMLScene.VerticesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := CalculateVerticesCount(OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := fvVerticesCountOver}
  {$define PRECALC_VALUE := FVerticesCountOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := fvVerticesCountNotOver}
  {$define PRECALC_VALUE := FVerticesCountNotOver}
  PRECALC_VALUE_RETURN
 end;
end;

function TVRMLScene.TrianglesCount(OverTriangulate: boolean): Cardinal;
begin
 {$define PRECALC_VALUE_CALCULATE := CalculateTrianglesCount(OverTriangulate)}
 if OverTriangulate then
 begin
  {$define PRECALC_VALUE_ENUM := fvTrianglesCountOver}
  {$define PRECALC_VALUE := FTrianglesCountOver}
  PRECALC_VALUE_RETURN
 end else
 begin
  {$define PRECALC_VALUE_ENUM := fvTrianglesCountNotOver}
  {$define PRECALC_VALUE := FTrianglesCountNotOver}
  PRECALC_VALUE_RETURN
 end;
end;

function TVRMLScene.CreateShape(AGeometry: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState): TVRMLShape;
begin
  Result := TVRMLShape.Create(Self, AGeometry, AState);
end;

type
  { Traverses on ChangedAll event, single TChangedAllTraverser
    instance traverses into a single ShapesGroup (but may
    recursively create other TChangedAllTraverser instances
    to create recursive groups). }
  TChangedAllTraverser = class
    ParentScene: TVRMLScene;
    ShapesGroup: TVRMLShapeTreeGroup;
    Active: boolean;
    procedure Traverse(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

procedure TChangedAllTraverser.Traverse(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);

  procedure HandleSwitch(SwitchNode: TNodeSwitch_2);
  var
    SwitchTree: TVRMLShapeTreeSwitch;
    Traverser: TChangedAllTraverser;
    ChildNode: TVRMLNode;
    ChildGroup: TVRMLShapeTreeGroup;
    I: Integer;
  begin
    SwitchTree := TVRMLShapeTreeSwitch.Create(ParentScene);
    SwitchTree.SwitchNode := SwitchNode;
    ShapesGroup.Children.Add(SwitchTree);

    for I := 0 to SwitchNode.FdChildren.Items.Count - 1 do
    begin
      ChildNode := SwitchNode.FdChildren.Items[I];
      ChildGroup := TVRMLShapeTreeGroup.Create(ParentScene);
      SwitchTree.Children.Add(ChildGroup);

      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := ParentScene;
        Traverser.ShapesGroup := ChildGroup;
        Traverser.Active := I = SwitchNode.FdWhichChoice.Value;
        ChildNode.TraverseInternal(StateStack, TVRMLNode, @Traverser.Traverse,
          nil, ParentInfo);
      finally FreeAndNil(Traverser) end;
    end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TVRMLLODNode);
  var
    LODTree: TVRMLShapeTreeLOD;
    Traverser: TChangedAllTraverser;
    ChildNode: TVRMLNode;
    ChildGroup: TVRMLShapeTreeGroup;
    I: Integer;
  begin
    LODTree := TVRMLShapeTreeLOD.Create(ParentScene);
    LODTree.LODNode := LODNode;
    LODTree.LODInvertedTransform^ := StateStack.Top.InvertedTransform;
    ShapesGroup.Children.Add(LODTree);
    ParentScene.ShapeLODs.Add(LODTree);

    { First add TVRMLShapeTreeGroup.Create as children, as many times
      as there are LODNode.FdChildren. Reason: LODTree.CalculateLevel
      uses this Count. }

    for I := 0 to LODNode.FdChildren.Items.Count - 1 do
      LODTree.Children.Add(TVRMLShapeTreeGroup.Create(ParentScene));

    ParentScene.UpdateLODLevel(LODTree);

    for I := 0 to LODNode.FdChildren.Items.Count - 1 do
    begin
      ChildNode := LODNode.FdChildren.Items[I];
      ChildGroup := TVRMLShapeTreeGroup(LODTree.Children.Items[I]);

      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := ParentScene;
        Traverser.ShapesGroup := ChildGroup;
        Traverser.Active := Cardinal(I) = LODTree.Level;
        ChildNode.TraverseInternal(StateStack, TVRMLNode, @Traverser.Traverse,
          nil, ParentInfo);
      finally FreeAndNil(Traverser) end;
    end;

    TraverseIntoChildren := false;
  end;

var
  Shape: TVRMLShape;
  Info: PTransformNodeInfo;
begin
  if Node is TVRMLGeometryNode then
  begin
    { Add shape to Shapes }
    Shape := ParentScene.CreateShape(Node as TVRMLGeometryNode,
      TVRMLGraphTraverseState.CreateCopy(StateStack.Top));
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

    { Add items to GeneratedTextures for this Shape, if it has any
      generated textures. }
    Shape.EnumerateShapeTextures(@ParentScene.GeneratedTextures.AddShapeTexture);
  end else

  if Node is TVRMLLightNode then
  begin
    { TODO: currently this means that lights within inactive Switch child
      work as active, just like other lights.
      Changing this (adding "if Active" test here) would mean that changing
      Switch.FdWhichChoice needs to update this too...

      Hm, actually, where VRML spec says precisely that lights
      are affected by active/inactive state? Anyway, if lights from
      inactive state *should* be taken into account, then gathering
      of lights should not be done by Traverse at all (but by some
      EnumarateNodes call). }

    { Add lights to ChangedAll_TraversedLights }
    ParentScene.ChangedAll_TraversedLights.Add(
      (Node as TVRMLLightNode).CreateActiveLight(StateStack.Top));
  end else

  if Node is TNodeSwitch_2 then
  begin
    HandleSwitch(TNodeSwitch_2(Node));
  end else
  if Node is TVRMLLODNode then
  begin
    HandleLOD(TVRMLLODNode(Node));
  end else

  if (Node is TNodeX3DBindableNode) and
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
    if Node is TNodeX3DBackgroundNode then
      ParentScene.BackgroundStack.PushIfEmpty( TNodeX3DBackgroundNode(Node), true) else
    if Node is TNodeFog then
      ParentScene.FogStack.PushIfEmpty( TNodeFog(Node), true) else
    if Node is TNodeNavigationInfo then
      ParentScene.NavigationInfoStack.PushIfEmpty( TNodeNavigationInfo(Node), true) else
    if Node is TVRMLViewpointNode then
      ParentScene.ViewpointStack.PushIfEmpty( TVRMLViewpointNode(Node), true);
  end else

  if (Node is TNodeTransform_2) or
     (Node is TNodeMatrixTransform_2) or
     (Node is TNodeHAnimHumanoid) or
     (Node is TNodeHAnimJoint) or
     (Node is TNodeHAnimSite) then
  begin
    Info := ParentScene.TransformNodesInfo.NodeInfo(Node);
    if Info = nil then
    begin
      Info := ParentScene.TransformNodesInfo.Add;
      Info^.Node := Node;
      Info^.Occurences := 1;
    end else
      Inc(Info^.Occurences);
  end;
end;

procedure TVRMLScene.UpdateLODLevel(LODTree: TVRMLShapeTreeLOD);
var
  OldLevel, NewLevel: Cardinal;
begin
  if IsLastViewer then
  begin
    OldLevel := LODTree.Level;
    NewLevel := LODTree.CalculateLevel(LastViewerPosition);
    LODTree.Level := NewLevel;

    if ProcessEvents and
       ( (OldLevel <> NewLevel) or
         (not LODTree.WasLevel_ChangedSend) ) and
       { If LODTree.Children.Count = 0, then Level = 0, but we don't
         want to send Level_Changed since the children 0 doesn't really exist. }
       (LODTree.Children.Count <> 0) then
    begin
      LODTree.WasLevel_ChangedSend := true;
      LODTree.LODNode.EventLevel_Changed.Send(LongInt(NewLevel), WorldTime);
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

      ScheduledGeometryActiveShapesChanged := true;
      ScheduleGeometryChanged;
    end;
  end;
end;

procedure TVRMLScene.ChangedAll;

  procedure UpdateVRML2ActiveLights;

    procedure AddLightEverywhere(const L: TActiveLight);
    var
      SI: TVRMLShapeTreeIterator;
    begin
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          SI.Current.State.VRML2ActiveLights.Add(L);
      finally FreeAndNil(SI) end;
    end;

    { Add L everywhere within given Radius from Location.
      Note that this will calculate BoundingBox of every Shape
      (but that's simply unavoidable if you have scene with VRML 2.0
      positional lights). }
    procedure AddLightRadius(const L: TActiveLight;
      const Location: TVector3Single; const Radius: Single);
    var
      SI: TVRMLShapeTreeIterator;
    begin
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if Box3dSphereCollision(SI.Current.BoundingBox, Location, Radius) then
            SI.Current.State.VRML2ActiveLights.Add(L);
      finally FreeAndNil(SI) end;
    end;

  var
    I: Integer;
    L: PActiveLight;
    LNode: TVRMLLightNode;
  begin
    for I := 0 to ChangedAll_TraversedLights.High do
    begin
      { TODO: for spot lights, it would be an optimization to also limit
        ActiveLights by spot cone size. }
      L := ChangedAll_TraversedLights.Pointers[I];
      LNode := L^.LightNode;
      if (LNode is TNodePointLight_1) or
         (LNode is TNodeSpotLight_1) then
        AddLightEverywhere(L^) else
      if LNode is TNodePointLight_2 then
        AddLightRadius(L^, L^.TransfLocation, L^.TransfRadius) else
      if LNode is TNodeSpotLight_2 then
        AddLightRadius(L^, L^.TransfLocation, L^.TransfRadius);
      { Other light types (directional) should be handled by
        TVRMLGroupingNode.BeforeTraverse }
    end;
  end;

var
  Traverser: TChangedAllTraverser;
begin
  if Log and LogChanges then
    WritelnLog('VRML changes', 'ChangedAll');

  { TransformNodesInfo will be recalculated by ChangedAll }
  TransformNodesInfo.Count := 0;

  { GeneratedTextures will be recalculated by ChangedAll }
  GeneratedTextures.Count := 0;

  BackgroundStack.CheckForDeletedNodes(RootNode, true);
  FogStack.CheckForDeletedNodes(RootNode, true);
  NavigationInfoStack.CheckForDeletedNodes(RootNode, true);
  ViewpointStack.CheckForDeletedNodes(RootNode, true);

  Validities := [];

  { Clear variables after removing fvTrianglesList* from Validities }
  InvalidateTrianglesList(false);
  InvalidateTrianglesList(true);
  InvalidateTrianglesListShadowCasters;

  { Clear variables after removing fvManifoldAndBorderEdges from Validities }
  InvalidateManifoldAndBorderEdges;

  ChangedAll_TraversedLights := TDynActiveLightArray.Create;
  try
    { Clean Shapes, ShapeLODs }
    FreeAndNil(FShapes);
    FShapes := TVRMLShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;

    if RootNode <> nil then
    begin
      Traverser := TChangedAllTraverser.Create;
      try
        Traverser.ParentScene := Self;
        { We just created FShapes as TVRMLShapeTreeGroup, so this cast
          is safe }
        Traverser.ShapesGroup := TVRMLShapeTreeGroup(FShapes);
        Traverser.Active := true;
        RootNode.Traverse(TVRMLNode, @Traverser.Traverse);
      finally FreeAndNil(Traverser) end;

      UpdateVRML2ActiveLights;

      if ProcessEvents then
        CollectNodesForEvents;
    end;
  finally FreeAndNil(ChangedAll_TraversedLights) end;

  ScheduledGeometryChangedAll := true;
  ScheduleGeometryChanged;

  VisibleSceneChange([prVisibleSceneGeometry, prVisibleSceneNonGeometry]);
  DoViewpointsChanged;
end;

procedure TVRMLScene.ChangedShapeFields(Shape: TVRMLShape;
  const TransformOnly, InactiveOnly, TextureImageChanged, PossiblyLocalGeometryChanged: boolean);
begin
  { Eventual clearing of Validities items because shape changed
    can be done by DoGeometryChanged, where more specific info
    about what changed is passed. No reason to do it here.
    Reason: Validities items
      fvBoundingBox, fvVerticesCountNotOver, fvVerticesCountOver,
      fvTrianglesCountNotOver, fvTrianglesCountOver,
      fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
      fvTrianglesListShadowCasters,
      fvManifoldAndBorderEdges
    are all related to geometry changes. }

  Shape.Changed(PossiblyLocalGeometryChanged);

  if not InactiveOnly then
    VisibleSceneChange([prVisibleSceneGeometry, prVisibleSceneNonGeometry]);
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
    the TVRMLShapeTree we're on. To record this, we'll manage a linked list
    of PShapesParentInfo records.

    We will traverse Shapes tree knowing
    it's structure, since it must have been created by ChangedAll code. }

  PShapesParentInfo = ^TShapesParentInfo;
  TShapesParentInfo = record
    Group: TVRMLShapeTreeGroup;
    Index: Integer;
  end;

type
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
    Shapes: PShapesParentInfo;
    ProximitySensorNum: Cardinal;
    ChangingNode: TVRMLNode;
    ChangingNodeOccurences: Integer; //< -1 if not known
    ChangingNodeFoundCount: Cardinal;
    AnythingChanged: boolean;
    Inside: boolean;
    ParentScene: TVRMLScene;
    { If = 0, we're in active graph part.
      If > 0, we're in inactive graph part (TransformChangeTraverse
      may enter there, since our changing Transform node (or some of it's
      children) may be inactive; but we have to update all shapes,
      active or not). }
    Inactive: Cardinal;
    procedure TransformChangeTraverse(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    procedure TransformChangeTraverseAfter(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo);
  end;

procedure TTransformChangeHelper.TransformChangeTraverse(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);

  procedure HandleSwitch(SwitchNode: TNodeSwitch_2);
  var
    I: Integer;
    ChildInactive: boolean;
    ShapeSwitch: TVRMLShapeTreeSwitch;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    { get Shape and increase Shapes^.Index }
    ShapeSwitch := Shapes^.Group.Children[Shapes^.Index] as TVRMLShapeTreeSwitch;
    Inc(Shapes^.Index);

    OldShapes := Shapes;
    try
      { We have to enter *every* Switch child (while normal
        Traverse would enter only active child), because changing Transform
        node may be in inactive graph parts. }

      for I := 0 to SwitchNode.FdChildren.Items.Count - 1 do
      begin
        NewShapes.Group := ShapeSwitch.Children[I] as TVRMLShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        ChildInactive := I <> SwitchNode.FdWhichChoice.Value;
        if ChildInactive then Inc(Inactive);

        SwitchNode.FdChildren.Items[I].TraverseInternal(
          StateStack, TVRMLNode,
          @TransformChangeTraverse,
          @TransformChangeTraverseAfter,
          ParentInfo);

        if ChildInactive then Dec(Inactive);
      end;

    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TVRMLLODNode);
  var
    I: Integer;
    ShapeLOD: TVRMLShapeTreeLOD;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    { get Shape and increase Shapes^.Index }
    ShapeLOD := Shapes^.Group.Children[Shapes^.Index] as TVRMLShapeTreeLOD;
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
        NewShapes.Group := ShapeLOD.Children[I] as TVRMLShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        if Cardinal(I) <> ShapeLOD.Level then Inc(Inactive);

        LODNode.FdChildren.Items[I].TraverseInternal(
          StateStack, TVRMLNode,
          @TransformChangeTraverse,
          @TransformChangeTraverseAfter,
          ParentInfo);

        if Cardinal(I) <> ShapeLOD.Level then Dec(Inactive);
      end;

    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

var
  Shape: TVRMLShape;
begin
  if Node is TNodeSwitch_2 then
  begin
    HandleSwitch(TNodeSwitch_2(Node));
  end else
  if Node is TVRMLLODNode then
  begin
    HandleLOD(TVRMLLODNode(Node));
  end else

  if Inside then
  begin
    if Node is TVRMLGeometryNode then
    begin
      { get Shape and increase Shapes^.Index }
      Check(Shapes^.Index < Shapes^.Group.Children.Count,
        'Missing shape in Shapes tree');
      Shape := Shapes^.Group.Children[Shapes^.Index] as TVRMLShape;
      Inc(Shapes^.Index);

      Shape.State.AssignTransform(StateStack.Top);
      { TransformOnly = @true, suitable for roSeparateShapesNoTransform,
        they don't have Transform compiled in display list. }
      ParentScene.ChangedShapeFields(Shape, true, Inactive <> 0, false, false);

      if Inactive = 0 then
      begin
        if Shape.Visible then
          ParentScene.ScheduledGeometrySomeVisibleTransformChanged := true;

        if Shape.Collidable then
          ParentScene.ScheduledGeometrySomeCollidableTransformChanged := true;

        ParentScene.ScheduleGeometryChanged;

        AnythingChanged := true;
      end;
    end else
    if Node is TNodeX3DBackgroundNode then
    begin
      { TODO: make this work to actually change displayed background }
      if Node = ParentScene.BackgroundStack.Top then
        raise BreakTransformChangeFailed.Create('bound ' + Node.NodeTypeName);
    end else
    if Node is TNodeFog then
    begin
      { There's no need to do anything more here, as FogDistanceScaling
        automatically changed (as this is just a shortcut to
        AverageScaleTransform, that is automatically stored within TNodeFog
        during traversing it).

        Renderer in TVRMLGLScene should detect this, and eventually
        destroy display lists and such when rendering next time.
      }
      if Inactive = 0 then
        ParentScene.VisibleSceneChange([prVisibleSceneGeometry, prVisibleSceneNonGeometry]);
    end else
    if Node is TNodeNavigationInfo then
    begin
      { TODO: make this work to actually change displayed NavigationInfo.
        For now, this does nothing, since TVRMLScene doesn't deal with
        NavigationInfo. }
      { if Node = NavigationInfoStack.Top then
        raise BreakTransformChangeFailed.Create; }
    end else
    if Node is TVRMLViewpointNode then
    begin
      if Node = ParentScene.ViewpointStack.Top then
        ParentScene.DoBoundViewpointVectorsChanged;
    end else
    if Node is TVRMLLightNode then
    begin
      { TODO: when light's transform changed, this could be more optimized:
        - update all TActiveLight records when this light node was present
          (by UpdateActiveLightState)
        - hmm, eventually ChangedAll may be needed to update
          CurrentActiveLights? }
      raise BreakTransformChangeFailed.Create(Node.NodeTypeName);
    end else
    if (Node is TNodeProximitySensor) and
       { We only care about ProximitySensor in active graph parts. }
       (Inactive = 0) then
    begin
      ParentScene.ProximitySensorInstances.Items[ProximitySensorNum].
        InvertedTransform := StateStack.Top.InvertedTransform;
      { Call ProximitySensorUpdate, since the sensor's box is transformed,
        so possibly it should be activated/deactivated,
        position/orientation_changed called etc. }
      if ParentScene.IsLastViewer then
        ParentScene.ProximitySensorUpdate(
          ParentScene.ProximitySensorInstances.Items[ProximitySensorNum]);
      Inc(ProximitySensorNum);
    end;
  end else
  begin
    if Node is TVRMLGeometryNode then
    begin
      { increase Shapes^.Index }
      Check(Shapes^.Index < Shapes^.Group.Children.Count,
        'Missing shape in Shapes tree');
      Inc(Shapes^.Index);
    end else
    if (Node is TNodeProximitySensor) and
       { We only care about ProximitySensor in active graph parts. }
       (Inactive = 0) then
      Inc(ProximitySensorNum) else
    if Node = ChangingNode then
      Inside := true;
  end;
end;

procedure TTransformChangeHelper.TransformChangeTraverseAfter(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo);
begin
  if Node = ChangingNode then
  begin
    Inside := false;

    { ChangingNodeFoundCount and BreakTransformChangeSuccess are
      useful to optimize TTransformChangeHelper: if we already
      encountered all possible instances of ChangingNode in VRML graph,
      we can terminate. This means that we don't have to waste time
      to traverse remaining nodes and shapes. }

    Inc(ChangingNodeFoundCount);
    if (ChangingNodeOccurences <> -1) and
       (ChangingNodeFoundCount >= Cardinal(ChangingNodeOccurences)) then
      raise BreakTransformChangeSuccess.Create;
  end;
end;

procedure TVRMLScene.ChangedFields(Node: TVRMLNode;
  FieldOrEvent: TVRMLFieldOrEvent);
var
  Field: TVRMLField;

  procedure ChangedTimeDependentNode(Handler: TTimeDependentNodeHandler);
  var
    DummySomethingChanged: boolean;
  begin
    { Although (de)activation of time-dependent nodes will be also catched
      by the nearest IncreaseWorldTime run, it's good to explicitly
      call SetWorldTime to catch it here.

      Reason: if cycleInterval is very small, and not looping, then
      we could "miss" the fact that node should be activated (if it
      was inactive, and next IncreaseWorldTime will happen after
      cycleInterval time already passed).

      Code below will make sure that no matter how small cycleInterval,
      no matter how seldom IncreaseWorldTime occur, the node will get
      activated when doing something like startTime := WorldTime. }

    if (Field = nil) or
       (Field = Handler.FdPauseTime) or
       (Field = Handler.FdresumeTime) or
       (Field = Handler.FdstartTime) or
       (Field = Handler.FdstopTime) then
    begin
      Handler.SetWorldTime(WorldTime, WorldTime, 0, DummySomethingChanged);

      { DummySomethingChanged is simply ignored here (we do not have to report
        changes anywhere). }
    end;
  end;

  procedure DoLogChanges(const Additional: string = '');
  var
    S: string;
  begin
    S := Format('ChangedFields: Node %s (%s %s) at %s',
      [ Node.NodeName, Node.NodeTypeName, Node.ClassName, PointerToStr(Node) ]);
    if Field <> nil then
      S += Format('. Field %s (%s)', [ Field.Name, Field.VRMLTypeName ]);
    if Additional <> '' then
      S += '. ' + Additional;
    WritelnLog('VRML changes', S);
  end;

  { If Appearance.TextureTransform contains TextureTransform
    (which should be some TextureTransform* VRML/X3D node,
    but not nil and not TNodeMultiTextureTransform). }
  function AppearanceUsesTextureTransform(Appearance: TNodeAppearance;
    TextureTransform: TVRMLNode): boolean;
  var
    MultiTrans: TMFNode;
    I: Integer;
  begin
    Result := Appearance.FdTextureTransform.Value = TextureTransform;
    if (not Result) and
       (Appearance.FdTextureTransform.Value <> nil) and
       (Appearance.FdTextureTransform.Value is TNodeMultiTextureTransform) then
    begin
      MultiTrans := TNodeMultiTextureTransform(
        Appearance.FdTextureTransform.Value).FdTextureTransform;
      for I := 0 to MultiTrans.Count - 1 do
        if MultiTrans.Items[I] = TextureTransform then
          Exit(true);
    end;
  end;

var
  NodeLastNodesIndex, I: integer;
  Coord: TMFVec3f;
  TransformChangeHelper: TTransformChangeHelper;
  SI: TVRMLShapeTreeIterator;
  NodeInfo: PTransformNodeInfo;
  TransformShapesParentInfo: TShapesParentInfo;
begin
  NodeLastNodesIndex := Node.TraverseStateLastNodesIndex;

  { calculate Field: either FieldOrEvent, or (if FieldOrEvent is an
    exposed event) undelying FieldOrEvent.ParentField, or @nil if not possible. }
  if FieldOrEvent <> nil then
  begin
    if FieldOrEvent is TVRMLEvent then
      Field := TVRMLEvent(FieldOrEvent).ParentExposedField else
      Field := FieldOrEvent as TVRMLField;
  end else
    Field := nil;

  if Log and LogChanges then
    DoLogChanges;

  { Ignore this ChangedFields call if you're changing
    something that doesn't *directly* affect actual content:
    - metadata field
    - metadata or WorldInfo nodes
    - and others, see comments...

    Do this first (before RootNode.IsNodePresent checks for ignoring),
    as this is much faster. This way e.g. changing TouchSensor.enabled
    or some other things works instantly fast.
  }

  if (Node is TNodeX3DNode) and
     (TNodeX3DNode(Node).FdMetadata = Field) then
    Exit;

  if (Node is TNodeMetadataDouble) or
     (Node is TNodeMetadataFloat) or
     (Node is TNodeMetadataInteger) or
     (Node is TNodeMetadataSet) or
     (Node is TNodeMetadataString) or
     (Node is TNodeWorldInfo) or
     { sensors (they don't affect actual content directly --- only when
       they are routed somewhere, and this will be eventually
       detected in another ChangedFields call) }
     (Node is TNodeX3DPointingDeviceSensorNode) or
     { script (like for sensors; script nodes take care themselves
        to react to events send to them) }
     (Node is TNodeScript) or
     { X3D event utilities nodes }
     (Node is TNodeX3DSequencerNode) or
     (Node is TNodeX3DTriggerNode) or
     (Node is TNodeBooleanFilter) or
     (Node is TNodeBooleanToggle) or
     { interpolators }
     (Node is TNodeX3DInterpolatorNode) then
    Exit;

  { We used to check here RootNode.IsNodePresent, to eliminate
    changes to nodes not in our graph. This is not done now,
    as in fact this check is not needed, and usually it wastes quite
    some time (for example, profile
    ../vrml/opengl/examples/change_vrml_by_code_2.pasprogram
    when doing ChangedFields (not ChangedAll)).

    In most cases, when modifying graph by code, and always when
    modifying graph by VRML events, the Node is known to be inside
    our VRML graph... }
  { $define CHECK_NODE_PRESENCE}
  {$ifdef CHECK_NODE_PRESENCE}

  { Ignore this ChangedFields call if node is not in our VRML graph.
    Exception is for StateDefaultNodes nodes (they are not present in RootNode
    graph, but influence us).

    At some point, we ignored here changes to the inactive part of VRML graph,
    by passing OnlyActive = true to IsNodePresent. (And assuming
    that changing the node's field cannot change it's active state,
    otherwise we would have to call IsNodePresent before and after
    field's change.)

    But now we have to allow processing nodes even in inactive part.
    Reason? Our Shapes tree contains things from inactive part
    (inactive Switch children), and they must be kept current just
    like active parts. }

  if (RootNode = nil) or
     ( (not RootNode.IsNodePresent(Node, false)) and
       ((NodeLastNodesIndex = -1) or
         (StateDefaultNodes.Nodes[NodeLastNodesIndex] <> Node))
     ) then
    Exit;
  {$endif CHECK_NODE_PRESENCE}

  { Test other changes: }

  BeginChangesSchedule;
  try
    if Node is TNodeComposedShader then
    begin
      { Do nothing here, as TVRMLOpenGLRenderer registers and takes care
        to update shaders' uniform variables. We don't have to do
        anything here, no need to rebuild/recalculate anything.
        The only thing that needs to be called is redisplay, by VisibleSceneChange
        at the end of ChangedFields. }
    end else
    if Node is TNodeCoordinate then
    begin
      { TNodeCoordinate is special, although it's part of VRML 1.0 state,
        it can also occur within coordinate-based nodes of VRML >= 2.0.
        So it affects coordinate-based nodes with this node.

        In fact, code below takes into account both VRML 1.0 and 2.0 situation,
        that's why it's before "if NodeLastNodesIndex <> -1 then" branch. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if SI.Current.Geometry.Coord(SI.Current.State, Coord) and
             (Coord.ParentNode = Node) then
          begin
            ChangedShapeFields(SI.Current, false, false, false, false
              { We pass PossiblyLocalGeometryChanged = false,
                as we'll do ScheduledLocalGeometryChangedCoord := true
                later that will take care of it anyway.
                For now, this may be slightly more optimal. });

            { Another special thing about Coordinate node: it changes
              actual geometry. }

            SI.Current.ScheduledLocalGeometryChangedCoord := true;
            ScheduleGeometryChanged;
          end;
      finally FreeAndNil(SI) end;
    end else
    if NodeLastNodesIndex <> -1 then
    begin
      { Node is part of VRML 1.0 state, so it affects Shapes where
        it's present on State.LastNodes list. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if SI.Current.State.LastNodes.Nodes[NodeLastNodesIndex] = Node then
            ChangedShapeFields(SI.Current, false, false, false, true);
      finally FreeAndNil(SI) end;
    end else
    if Node is TNodeX3DColorNode then
    begin
      { Affects all geometry nodes with "color" field referencing this node.

        Note: also TNodeParticleSystem may have color in FdcolorRamp field.
        This is not detected for now, and doesn't matter (we do not handle
        particle systems at all now). }

      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if ((SI.Current.Geometry is TNodeX3DComposedGeometryNode) and (TNodeX3DComposedGeometryNode(SI.Current.Geometry).FdColor.Value = Node)) or
	     ((SI.Current.Geometry is TNodeIndexedLineSet_2       ) and (TNodeIndexedLineSet_2       (SI.Current.Geometry).FdColor.Value = Node)) or
	     ((SI.Current.Geometry is TNodeLineSet                ) and (TNodeLineSet                (SI.Current.Geometry).FdColor.Value = Node)) or
	     ((SI.Current.Geometry is TNodePointSet_2             ) and (TNodePointSet_2             (SI.Current.Geometry).FdColor.Value = Node)) or
	     ((SI.Current.Geometry is TNodeElevationGrid          ) and (TNodeElevationGrid          (SI.Current.Geometry).FdColor.Value = Node)) then
            ChangedShapeFields(SI.Current, false, false, false, false);
      finally FreeAndNil(SI) end;
    end else
    if Node is TNodeMaterial_2 then
    begin
      { VRML 2.0 Material affects only shapes where it's
        placed inside Appearance.material field. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if SI.Current.State.ParentShape.Material = Node then
            ChangedShapeFields(SI.Current, false, false, false, false);
      finally FreeAndNil(SI) end;
    end else
    if Node is TNodeX3DTextureCoordinateNode then
    begin
      { VRML 2.0 TextureCoordinate affects only shapes where it's
        placed inside texCoord field. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if (SI.Current.Geometry is TNodeX3DComposedGeometryNode) and
             (TNodeX3DComposedGeometryNode(SI.Current.Geometry).
               FdTexCoord.Value = Node) then
            ChangedShapeFields(SI.Current, false, false, false, false);
      finally FreeAndNil(SI) end;
    end else
    if (Node is TNodeTextureTransform) or
       (Node is TNodeTextureTransformMatrix3D) or
       (Node is TNodeTextureTransform3D) then
    begin
      { VRML 2.0 / X3D TextureTransform* affects only shapes where it's
        placed inside textureTransform field. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if (SI.Current.State.ParentShape <> nil) and
             (SI.Current.State.ParentShape.FdAppearance.Value <> nil) and
             (SI.Current.State.ParentShape.FdAppearance.Value is TNodeAppearance) and
             AppearanceUsesTextureTransform(
               TNodeAppearance(SI.Current.State.ParentShape.FdAppearance.Value), Node) then
            ChangedShapeFields(SI.Current, false, false, false, false);
      finally FreeAndNil(SI) end;
    end else
    if Node is TVRMLLightNode then
    begin
      ChangedActiveLightNode(TVRMLLightNode(Node), Field);
    end else
    if Node is TVRMLGeometryNode then
    begin
      { node jest Shape'm. Wiec wplynal tylko na Shapes gdzie wystepuje jako
        Geometry. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if SI.Current.Geometry = Node then
          begin
            ChangedShapeFields(SI.Current, false, false, false, false
              { We pass PossiblyLocalGeometryChanged = false,
                as we'll do ScheduledLocalGeometryChangedCoord := true
                later that will take care of it anyway.
                For now, this may be slightly more optimal. });

            SI.Current.ScheduledLocalGeometryChanged := true;
            ScheduleGeometryChanged;
          end;
      finally FreeAndNil(SI) end;
    end else
    if (Field <> nil) and Field.Transform then
    begin
      { This is the optimization for changing VRML >= 2.0 transformation
        (most fields of Transform node like translation, scale, center etc.,
        also some HAnim nodes like Joint and Humanoid have this behavior.).

        In the simple cases, Transform node simply changes
        TVRMLGraphTraverseState.Transform for children nodes.

        So we have to re-traverse from this Transform node, and change
        states of affected children. Also, first we have to traverse
        *to* this Transform node, as we don't know it's initial State...
        Moreover, Node may be instantiated many times in VRML graph,
        so we have to Traverse to all it's occurences.

        Besides, while traversing to current transform node, we have to count
        Shapes, to know which Shapes will be affected by what
        state change (as we cannot deduce it from the mere Geometry
        reference, since node may be instantiated many times under
        different transformation, many times within our changing Transform
        node, and many times outside of the changing Transform group).

        In more difficult cases, children of this Transform node may
        be affected in other ways by transformation. For example,
        Fog and Background nodes are affected by their parents transform.
        Currently, we cannot account for this, and just raise
        BreakTransformChangeFailed in this case --- we have to do ChangedAll
        in such cases.
      }
      try
        TransformShapesParentInfo.Group := Shapes as TVRMLShapeTreeGroup;
        TransformShapesParentInfo.Index := 0;

        TransformChangeHelper := TTransformChangeHelper.Create;
        try
          TransformChangeHelper.ParentScene := Self;
          TransformChangeHelper.Shapes := @TransformShapesParentInfo;
          TransformChangeHelper.ProximitySensorNum := 0;
          TransformChangeHelper.ChangingNode := Node;
          TransformChangeHelper.AnythingChanged := false;
          TransformChangeHelper.Inside := false;
          TransformChangeHelper.Inactive := 0;
          TransformChangeHelper.ChangingNodeFoundCount := 0;

          NodeInfo := TransformNodesInfo.NodeInfo(Node);
          if NodeInfo <> nil then
          begin
            TransformChangeHelper.ChangingNodeOccurences := NodeInfo^.Occurences;
          end else
          begin
            if Log and LogChanges then
              WritelnLog('VRML changes', Format('Occurences number for node "%s" not recorded, changing transformation of this node will not be optimized',
                [Node.NodeTypeName]));
            TransformChangeHelper.ChangingNodeOccurences := -1;
          end;

          try
            RootNode.Traverse(TVRMLNode,
              @TransformChangeHelper.TransformChangeTraverse,
              @TransformChangeHelper.TransformChangeTraverseAfter);
          except
            on BreakTransformChangeSuccess do
              { BreakTransformChangeSuccess is equivalent with normal finish
                of Traverse. So do nothing, just silence exception. }
          end;

          if not TransformChangeHelper.AnythingChanged then
            { No need to even VisibleSceneChange at the end. }
            Exit;
        finally
          FreeAndNil(TransformChangeHelper);
        end;
      except
        on B: BreakTransformChangeFailed do
        begin
          if Log and LogChanges then
            DoLogChanges('-> this Transform change (because of child: ' + B.Reason + ') causes ChangedAll (no optimized action)');
          ScheduleChangedAll;
          Exit;
        end;
      end;
    end else
    if Node is TNodeProximitySensor then
    begin
      if (Field = TNodeProximitySensor(Node).FdCenter) or
         (Field = TNodeProximitySensor(Node).FdSize) then
      begin
        { Update state for this ProximitySensor node. }
        if IsLastViewer then
          for I := 0 to ProximitySensorInstances.Count - 1 do
          begin
            if ProximitySensorInstances.Items[I].Node = Node then
              ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
          end;
      end else
      begin
        { Other changes to TNodeProximitySensor (enabled, metadata)
          can safely be ignored, not require any action. }
        Exit;
      end;
    end else
    if Node is TNodeMovieTexture then
    begin
      ChangedTimeDependentNode(TNodeMovieTexture(Node).TimeDependentNodeHandler);
      { No need to do VisibleSceneChange.
        Redisplay will be done by next IncreaseWorldTime run, if active now. }
      Exit;
    end else
    if Node is TNodeTimeSensor then
    begin
      ChangedTimeDependentNode(TNodeTimeSensor(Node).TimeDependentNodeHandler);
      { VisibleSceneChange not needed --- this is only a sensor, it's state
        is not directly visible. }
      Exit;
    end else
    if Node is TVRMLViewpointNode then
    begin
      if (Node = ViewpointStack.Top) and
         ( (Field = nil) or
           (TVRMLViewpointNode(Node).FdOrientation = Field) or
           (TVRMLViewpointNode(Node).FdDirection   = Field) or
           (TVRMLViewpointNode(Node).FdUp          = Field) or
           (TVRMLViewpointNode(Node).FdGravityUp   = Field) or
           (TVRMLViewpointNode(Node).Position      = Field) ) then
        DoBoundViewpointVectorsChanged else
        { Nothing needs to be done if
          - non-bound viewpoint changed,
          - or a field of bound viewpoint that doesn't affect it's vectors. }
        Exit;
    end else
    if ( (Node is TNodePixelTexture) and
         ( (Field = nil) or
           (TNodePixelTexture(Node).FdImage = Field) ) ) or
       ( (Node is TNodeImageTexture) and
         ( (Field = nil) or
           (TNodeImageTexture(Node).FdUrl = Field) ) ) or
       ( (Node is TNodeMovieTexture) and
         ( (Field = nil) or
           { TODO: we will not get here, as previous
             "if Node is TNodeMovieTexture" hides this }
           (TNodeMovieTexture(Node).FdUrl = Field) ) ) or
       ( (Node is TNodeTexture2) and
         ( (Field = nil) or
           (TNodeTexture2(Node).FdFilename = Field) or
           (TNodeTexture2(Node).FdImage = Field) ) ) then
    begin
      { On change of TVRMLTextureNode field that changes the result of
        TVRMLTextureNode.LoadTextureData, we have to explicitly release
        old texture (otherwise, LoadTextureData will not be called
        to reload the texture). }
      TVRMLTextureNode(Node).IsTextureLoaded := false;

      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
        begin
          if SI.Current.UsesTexture(TVRMLTextureNode(Node)) then
            ChangedShapeFields(SI.Current, false, false, true, false);
        end;
      finally FreeAndNil(SI) end;
    end else
    if (Node is TNodeKambiAppearance) and
       (TNodeKambiAppearance(Node).FdShadowCaster = Field) then
    begin
      { When KambiAppearance.shadowCaster field changed, then
        TrianglesListShadowCasters and Manifold/BorderEdges change. }
      InvalidateTrianglesListShadowCasters;
      InvalidateManifoldAndBorderEdges;
    end else
    if (Node is TNodeSwitch_2) and
       (TNodeSwitch_2(Node).FdWhichChoice = Field) then
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
        fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
        fvTrianglesListShadowCasters,
        fvManifoldAndBorderEdges
      }

      Validities := Validities - [
        { Calculation traverses over active shapes. }
        fvShapesActiveCount,
        fvShapesActiveVisibleCount,
        { Calculation traverses over active nodes (uses RootNode.Traverse). }
        fvMainLightForShadows];

      ScheduledGeometryActiveShapesChanged := true;
      ScheduleGeometryChanged;
    end else
    if Node is TNodeGeneratedCubeMapTexture then
    begin
      { For generated textures nodes "update" fields:
        changes will be handled automatically
        at next UpdateGeneratedTextures call.
        So just make VisibleSceneChange and nothing else is needed.

        Note we pass Changes = [] to VisibleSceneChange.
        That's logical --- only the change of "update" doesn't visibly
        change anything on the scene. This means that if you change "update"
        to "ALWAYS", but no visible change was registered since last update
        of the texture, the texture will not be actually immediately
        regenerated --- correct optimization!

        For other fields, even VisibleSceneChange isn't needed. }

      if (Field = nil) or
         (Field = TNodeGeneratedCubeMapTexture(Node).FdUpdate) then
        VisibleSceneChange([]);
      Exit;
    end else
    if Node is TNodeRenderedTexture then
    begin
      if (Field = nil) or
         (Field = TNodeRenderedTexture(Node).FdDimensions) or
         (Field = TNodeRenderedTexture(Node).FdViewpoint) or
         (Field = TNodeRenderedTexture(Node).FdDepthMap) then
        { Call with prVisibleSceneGeometry, to regenerate even if UpdateNeeded = false }
        VisibleSceneChange([prVisibleSceneGeometry]) else
      if (Field = TNodeRenderedTexture(Node).FdUpdate) then
        VisibleSceneChange([]);
      Exit;
    end else
    if Node is TNodeGeneratedShadowMap then
    begin
      if (Field = nil) or
         (Field = TNodeGeneratedShadowMap(Node).FdScale) or
         (Field = TNodeGeneratedShadowMap(Node).FdBias) or
         (Field = TNodeGeneratedShadowMap(Node).FdLight) then
        { Call with prVisibleSceneGeometry, to regenerate even if UpdateNeeded = false }
        VisibleSceneChange([prVisibleSceneGeometry]) else
      if (Field = TNodeGeneratedShadowMap(Node).FdUpdate) then
        VisibleSceneChange([]);
      Exit;
    end else
    begin
      { Node is something else. So we must assume that an arbitrary change
        occured, possibly changing State of following and/or children
        nodes of this Node. }

      if Log and LogChanges then
        DoLogChanges('-> causes ChangedAll (no optimized action)');

      ScheduleChangedAll;
      Exit;
    end;

    VisibleSceneChange([prVisibleSceneGeometry, prVisibleSceneNonGeometry]);
  finally EndChangesSchedule end;
end;

procedure TVRMLScene.ChangedField(Field: TVRMLField);
begin
  if Field.ParentNode <> nil then
    ChangedFields(Field.ParentNode as TVRMLNode, Field) else
  begin
    if Log and LogChanges then
    begin
      { It's useful to warn about this if LogChanges, as this can
        potentially be much optimized by fixing ParentNode. }
      WritelnLog('VRML changes', Format('WARNING: Field %s (%s) changed, but has no ParentNode assigned, falling back on (slow) ChangedAll', [ Field.Name, Field.VRMLTypeName ]));
    end;

    ScheduleChangedAll;
  end;
end;

procedure TVRMLScene.ChangedActiveLightNode(LightNode: TVRMLLightNode;
  Field: TVRMLField);
var
  J: integer;
  SI: TVRMLShapeTreeIterator;
  ActiveLight: PActiveLight;
begin
  { For light properties not reflected in TActiveLight,
    there's just no need to do anything right now. }

  if (Field = nil) or Field.ProcessedInActiveLight then
  begin
    { Update all TActiveLight records with LightNode = this Node.

      TODO: what if some CurrentActiveLights need to be updated?
      Code below fails for this.

      To be fixed (at the same time taking into account that in X3D
      "global" is exposed field and so may change during execution) by
      constructing CurrentActiveLights always with global = TRUE assumption.
      RenderShapeLights will have to take care of eventual "radius"
      constraints. }

    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        for J := 0 to SI.Current.State.CurrentActiveLights.Count - 1 do
        begin
          ActiveLight := @(SI.Current.State.CurrentActiveLights.Items[J]);
          if ActiveLight^.LightNode = LightNode then
            LightNode.UpdateActiveLight(ActiveLight^);
        end;
    finally FreeAndNil(SI) end;
  end;

  { When some kambiShadows or kambiShadowsMain field changes,
    then MainLightForShadows must be recalculated. }

  if (Field = nil) or
     (Field = LightNode.FdKambiShadows) or
     (Field = LightNode.FdKambiShadowsMain) then
    Exclude(Validities, fvMainLightForShadows);

  { If we had calculated MainLightForShadows, and this LightNode is the
    main light for shadows, then update FMainLightForShadows.
    Thanks to varius FMainLightForShadows* properties, we can check
    and recalculate it very fast --- this is good for scenes where main
    shadow light location is moving. }

  if (fvMainLightForShadows in Validities) and
     FMainLightForShadowsExists and
     (FMainLightForShadowsNode = LightNode) then
    CalculateMainLightForShadowsPosition;
end;

procedure TVRMLScene.VisibleSceneChange(const Changes: TVisibleSceneChanges);
begin
  VisibleChange;
end;

procedure TVRMLScene.DoGeometryChanged;
var
  SomeLocalGeometryChanged: boolean;
  EdgesStructureChanged: boolean;
  SI: TVRMLShapeTreeIterator;
begin
  Validities := Validities - [fvBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvTrianglesListShadowCasters];

  { Clear variables after removing fvTrianglesList* }
  InvalidateTrianglesList(false);
  InvalidateTrianglesList(true);
  InvalidateTrianglesListShadowCasters;

  { First, call LocalGeometryChanged on shapes when needed.

    By the way, also calculate SomeLocalGeometryChanged (= if any
    LocalGeometryChanged was called, which means that octree and
    bounding box/sphere of some shape changed).

    Note that this also creates implication ScheduledGeometryChangedAll
    => SomeLocalGeometryChanged. In later code, I sometimes check
    for SomeLocalGeometryChanged, knowing that this also checks for
    ScheduledGeometryChangedAll.

    By the way, also calculate EdgesStructureChanged. }

  if ScheduledGeometryChangedAll then
  begin
    SomeLocalGeometryChanged := true;

    EdgesStructureChanged := true;

    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        SI.Current.LocalGeometryChanged;
    finally FreeAndNil(SI) end;

    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'All TVRMLShape.OctreeTriangles updated');
  end else
  begin
    SomeLocalGeometryChanged := false;
    EdgesStructureChanged := ScheduledGeometryActiveShapesChanged;

    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if SI.Current.ScheduledLocalGeometryChanged or
           SI.Current.ScheduledLocalGeometryChangedCoord then
        begin
          SomeLocalGeometryChanged := true;
          SI.Current.LocalGeometryChanged;
          if Log and LogChanges and
             (SI.Current.OctreeTriangles <> nil) then
            WritelnLog('VRML changes (octree)', Format(
              'Shape(%s).OctreeTriangles updated', [PointerToStr(SI.Current)]));
        end;

        { Note that if
          ScheduledLocalGeometryChangedCoord = true, but
          ScheduledLocalGeometryChanged = false, then
          EdgesStructureChanged may remain false. This is the very reason
          for     ScheduledLocalGeometryChangedCoord separation from
          regular ScheduledLocalGeometryChanged. }

        if SI.Current.ScheduledLocalGeometryChanged then
          EdgesStructureChanged := true;
      end;
    finally FreeAndNil(SI) end;
  end;

  { Use EdgesStructureChanged to decide should be invalidate
    ManifoldAndBorderEdges. }
  if EdgesStructureChanged then
    InvalidateManifoldAndBorderEdges;

  if (OctreeRendering <> nil) and
     (ScheduledGeometrySomeVisibleTransformChanged or
      ScheduledGeometryActiveShapesChanged or
      SomeLocalGeometryChanged) then
  begin
    { Remember to do FreeAndNil on octrees below.
      Although we will recreate octrees right after rebuilding,
      it's still good to nil them right after freeing.
      Otherwise, when exception will raise from CreateXxxOctree,
      Scene.OctreeXxx will be left as invalid pointer. }

    FreeAndNil(FOctreeRendering);
    FOctreeRendering := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opRendering),
      ShapeOctreeProgressTitle,
      false);

    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeRendering updated');
  end;

  if (OctreeDynamicCollisions <> nil) and
     (ScheduledGeometrySomeCollidableTransformChanged or
      ScheduledGeometryActiveShapesChanged or
      SomeLocalGeometryChanged) then
  begin
    FreeAndNil(FOctreeDynamicCollisions);
    FOctreeDynamicCollisions := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opDynamicCollisions),
      ShapeOctreeProgressTitle,
      true);

    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeDynamicCollisions updated');
  end;

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self, SomeLocalGeometryChanged);

  { clear ScheduledGeometryXxx flags now }
  ScheduledGeometryChangedAll := false;
  ScheduledGeometrySomeVisibleTransformChanged := false;
  ScheduledGeometrySomeCollidableTransformChanged := false;
  ScheduledGeometryActiveShapesChanged := false;

  SI := TVRMLShapeTreeIterator.Create(Shapes, false);
  try
    while SI.GetNext do
    begin
      SI.Current.ScheduledLocalGeometryChanged := false;
      SI.Current.ScheduledLocalGeometryChangedCoord := false;
    end;
  finally FreeAndNil(SI) end;
end;

procedure TVRMLScene.DoViewpointsChanged;
begin
  if Assigned(OnViewpointsChanged) then
    OnViewpointsChanged(Self);
end;

procedure TVRMLScene.DoBoundViewpointVectorsChanged;
begin
  if Assigned(OnBoundViewpointVectorsChanged) then
    OnBoundViewpointVectorsChanged(Self);
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

function TVRMLScene.InfoTriangleVerticesCounts: string;
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

function TVRMLScene.InfoBoundingBox: string;
var
  BBox: TBox3d;
begin
  BBox := BoundingBox;
  Result := 'Bounding box : ' + Box3dToNiceStr(BBox);
  if not IsEmptyBox3d(BBox) then
  begin
    Result += ', average size : ' + FloatToNiceStr(Box3dAvgSize(BBox));
  end;
  Result += NL;
end;

function TVRMLScene.InfoManifoldAndBorderEdges: string;
begin
  Result := Format('Edges detection: all edges split into %d manifold edges and %d border edges. Note that for some algorithms, like shadow volumes, perfect manifold (that is, no border edges) works best.',
    [ ManifoldEdges.Count,
      BorderEdges.Count ]) + NL;
end;

function TVRMLScene.Info(
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

type
  TInfoNodeWriter = class
    Count: Cardinal;
    procedure WriteNode(node: TVRMLNode);
  end;
  procedure TInfoNodeWriter.WriteNode(node: TVRMLNode);
  begin
   Inc(Count);
   Writeln('Info node : "',(Node as TNodeInfo).FdString.Value, '"')
  end;

procedure TVRMLScene.WritelnInfoNodes;
var W: TInfoNodeWriter;
begin
 if RootNode = nil then Exit;

 W := TInfoNodeWriter.Create;
 try
  RootNode.EnumerateNodes(TNodeInfo,
    {$ifdef FPC_OBJFPC} @ {$endif} W.WriteNode, false);
  Writeln(W.Count, ' Info nodes in the scene.');
 finally W.Free end;
end;

{ octrees -------------------------------------------------------------------- }

function TVRMLScene.OverrideOctreeLimits(
  const BaseLimits: TOctreeLimits;
  const OP: TSceneOctreeProperties): TOctreeLimits;
var
  Props: TNodeKambiOctreeProperties;
begin
  Result := BaseLimits;
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TNodeKambiNavigationInfo) then
  begin
    Props := TNodeKambiNavigationInfo(NavigationInfoStack.Top).OctreeProperties(OP);
    if Props <> nil then
      Props.OverrideLimits(Result);
  end;
end;

function TVRMLScene.TriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TVRMLScene.ShapeOctreeLimits: POctreeLimits;
begin
  Result := @FShapeOctreeLimits;
end;

procedure TVRMLScene.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; Geometry: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, Geometry, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

procedure TVRMLScene.SetSpatial(const Value: TVRMLSceneSpatialStructures);

  procedure SetShapeSpatial(const Value: TVRMLShapeSpatialStructures;
    OnlyCollidable: boolean);
  var
    SI: TVRMLShapeTreeIterator;
  begin
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
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
            [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_octree_properties].
          }

          SI.Current.TriangleOctreeProgressTitle := TriangleOctreeProgressTitle;
          SI.Current.Spatial := Value;
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
    begin
      FreeAndNil(FOctreeRendering);
    end else
    if New and not Old then
    begin
      FOctreeRendering := CreateShapeOctree(
        OverrideOctreeLimits(FShapeOctreeLimits, opRendering),
        ShapeOctreeProgressTitle,
        false);
    end;

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
      FOctreeDynamicCollisions := CreateShapeOctree(
        OverrideOctreeLimits(FShapeOctreeLimits, opDynamicCollisions),
        ShapeOctreeProgressTitle,
        true);

      { SetShapeSpatial cannot be done by the way of doing CreateShapeOctree,
        since in CreateShapeOctree we iterate over OnlyActive shapes,
        but SetShapeSpatial must iterate over all shapes. }
      SetShapeSpatial([ssTriangles], true);
    end;

    { Handle OctreeVisibleTriangles }

    Old := ssVisibleTriangles in Spatial;
    New := ssVisibleTriangles in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeVisibleTriangles);
    end else
    if New and not Old then
    begin
      FOctreeVisibleTriangles := CreateTriangleOctree(
        OverrideOctreeLimits(FTriangleOctreeLimits, opVisibleTriangles),
        TriangleOctreeProgressTitle,
        false);
    end;

    { Handle OctreeCollidableTriangles }

    Old := ssCollidableTriangles in Spatial;
    New := ssCollidableTriangles in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeCollidableTriangles);
    end else
    if New and not Old then
    begin
      FOctreeCollidableTriangles := CreateTriangleOctree(
        OverrideOctreeLimits(FTriangleOctreeLimits, opCollidableTriangles),
        TriangleOctreeProgressTitle,
        true);
    end;

    FSpatial := Value;
  end;
end;

function TVRMLScene.OctreeCollisions: TVRMLBaseTrianglesOctree;
begin
  if OctreeCollidableTriangles <> nil then
    Result := OctreeCollidableTriangles else
  if OctreeDynamicCollisions <> nil then
    Result := OctreeDynamicCollisions else
    Result := nil;
end;

function TVRMLScene.CreateTriangleOctree(
  const Limits: TOctreeLimits;
  const ProgressTitle: string;
  const Collidable: boolean): TVRMLTriangleOctree;

  procedure FillOctree(AddTriProc: TNewTriangleProc);
  var
    SI: TVRMLShapeTreeIterator;
  begin
    SI := TVRMLShapeTreeIterator.Create(Shapes, true);
    try
      while SI.GetNext do
        if (Collidable and SI.Current.Collidable) or
           ((not Collidable) and SI.Current.Visible) then
          SI.Current.Geometry.Triangulate(
            SI.Current.State, false, AddTriProc);
    finally FreeAndNil(SI) end;
  end;

begin
  Result := TVRMLTriangleOctree.Create(Limits, BoundingBox);
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
          FillOctree({$ifdef FPC_OBJFPC} @ {$endif} AddTriangleToOctreeProgress);
        finally Progress.Fini end;
      end else
        FillOctree({$ifdef FPC_OBJFPC} @ {$endif} Result.AddItemTriangle);
    finally
      Result.Triangles.AllowedCapacityOverflow := 4;
    end;
  except Result.Free; raise end;
end;

function TVRMLScene.CreateShapeOctree(
  const Limits: TOctreeLimits;
  const ProgressTitle: string;
  const Collidable: boolean): TVRMLShapeOctree;
var
  I: Integer;
  ShapesList: TVRMLShapesList;
begin
  if Collidable then
    { Add only active and collidable shapes }
    ShapesList := TVRMLShapesList.Create(Shapes, true, false, true) else
    { Add only active and visible shapes }
    ShapesList := TVRMLShapesList.Create(Shapes, true, true, false);

  Result := TVRMLShapeOctree.Create(Limits, BoundingBox, ShapesList, true);
  try
    if (ProgressTitle <> '') and
       (Progress.UserInterface <> nil) and
       (not Progress.Active) then
    begin
      Progress.Init(Result.ShapesList.Count, ProgressTitle, true);
      try
        for I := 0 to Result.ShapesList.Count - 1 do
        begin
          Result.TreeRoot.AddItem(I);
          Progress.Step;
        end;
      finally Progress.Fini end;
    end else
    begin
      for I := 0 to Result.ShapesList.Count - 1 do
        Result.TreeRoot.AddItem(I);
    end;
  except Result.Free; raise end;
end;

{ viewpoints ----------------------------------------------------------------- }

type
  BreakFirstViewpointFound = class(TCodeBreaker);

  TFirstViewpointSeeker = class
    OnlyPerspective: boolean;
    ViewpointDescription: string;
    FoundNode: TVRMLViewpointNode;
    procedure Seek(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  end;

  procedure TFirstViewpointSeeker.Seek(
    Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
    ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  var
    V: TVRMLViewpointNode;
  begin
    V := Node as TVRMLViewpointNode;
    if ( (not OnlyPerspective) or
         (V.CameraKind = ckPerspective) ) and
       ( (ViewpointDescription = '') or
         ( (Node is TNodeX3DViewpointNode) and
           (TNodeX3DViewpointNode(Node).FdDescription.Value = ViewpointDescription) ) ) then
    begin
      FoundNode := V;
      raise BreakFirstViewpointFound.Create;
    end;
  end;

function TVRMLScene.GetViewpointCore(
  const OnlyPerspective: boolean;
  out CamKind: TVRMLCameraKind;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
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
        RootNode.Traverse(TVRMLViewpointNode, @Seeker.Seek);
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
    Result.GetCameraVectors(CamPos, CamDir, CamUp, GravityUp);
    CamKind := Result.CameraKind;
  end else
  begin
    { use default camera settings }
    CamPos := StdVRMLCamPos[1];
    CamDir := StdVRMLCamDir;
    CamUp := StdVRMLCamUp;
    GravityUp := StdVRMLGravityUp;
    CamKind := ckPerspective;
  end;
end;

function TVRMLScene.GetViewpoint(
  out CamKind: TVRMLCameraKind;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
begin
  Result := GetViewpointCore(false, CamKind, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
end;

function TVRMLScene.GetPerspectiveViewpoint(
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
var
  CamKind: TVRMLCameraKind;
begin
  Result := GetViewpointCore(true, CamKind, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
  Assert(CamKind = ckPerspective);
end;

{ fog ---------------------------------------------------------------------- }

function TVRMLScene.FogNode: TNodeFog;
begin
  Result := FogStack.Top as TNodeFog;
end;

function TVRMLScene.FogDistanceScaling: Single;
var
  Fog: TNodeFog;
begin
  { TODO: Using FogAverageScaleTransform is a simplification here.
    If we have non-uniform scaling, then FogAverageScaleTransform (and
    FogDistanceScaling) shouldn't  be used at all.

    Zamiast FFogDistanceScaling powinnismy
    sobie tutaj jakos wyliczac transformacje odwrotna do FogTransform.
    Potem kazdy element ktory bedziemy rysowac najpierw zrzutujemy
    do coordinate space node'u mgly, podobnie jak pozycje kamery,
    obliczymy odleglosci tych zrzutowanych punktow i to te odleglosci
    bedziemy porownywac z FogNode.VisibilityRange. To jest poprawna metoda.
    I w ten sposob np. mozemy zrobic mgle bardziej gesta w jednym kierunku
    a mniej w drugim. Fajne.

    Zupelnie nie wiem jak to zrobic w OpenGLu - jego GL_FOG_END (chyba)
    nie przechodzi takiej transformacji. Wiec w OpenGLu nie zrobie
    przy pomocy glFog takiej mgly (a przeciez samemu robic mgle nie bedzie
    mi sie chcialo, nie mowiac juz o tym ze zalezy mi na szybkosci a strace
    ja jesli bede implementowal rzeczy ktore juz sa w OpenGLu).
  }

  Fog := FogNode;
  if Fog <> nil then
    Result := Fog.AverageScaleTransform else
    { Result doesn't matter in this case, but should be deterministic,
      to help caching and comparing fog properties }
    Result := 0;
end;

{ triangles list ------------------------------------------------------------- }

procedure TVRMLScene.ValidateTrianglesList(OverTriangulate: boolean);
var
  ValidityValue: TVRMLSceneValidity;
begin
  if OverTriangulate then
    ValidityValue := fvTrianglesListOverTriangulate else
    ValidityValue := fvTrianglesListNotOverTriangulate;
  if not (ValidityValue in Validities) then
  begin
    FreeAndNil(FTrianglesList[OverTriangulate]);
    FTrianglesList[OverTriangulate] := CreateTrianglesList(OverTriangulate);
    Include(Validities, ValidityValue);
  end;
end;

type
  TTriangleAdder = class
    TriangleList: TDynTriangle3SingleArray;
    procedure AddTriangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      Geometry: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangleAdder.AddTriangle(const Triangle: TTriangle3Single;
    State: TVRMLGraphTraverseState;
    Geometry: TVRMLGeometryNode;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  begin
    if IsValidTriangle(Triangle) then
      TriangleList.Add(Triangle);
  end;

function TVRMLScene.CreateTrianglesList(OverTriangulate: boolean):
  TDynTriangle3SingleArray;
var
  SI: TVRMLShapeTreeIterator;
  TriangleAdder: TTriangleAdder;
begin
  Result := TDynTriangle3SingleArray.Create;
  try
    Result.AllowedCapacityOverflow := TrianglesCount(false);
    try
      TriangleAdder := TTriangleAdder.Create;
      try
        TriangleAdder.TriangleList := Result;

        SI := TVRMLShapeTreeIterator.Create(Shapes, true);
        try
          while SI.GetNext do
            SI.Current.Geometry.Triangulate(
              SI.Current.State, OverTriangulate,
              @TriangleAdder.AddTriangle);
        finally FreeAndNil(SI) end;

      finally FreeAndNil(TriangleAdder) end;
    finally Result.AllowedCapacityOverflow := 4 end;
  except Result.Free; raise end;
end;

function TVRMLScene.TrianglesList(OverTriangulate: boolean):
  TDynTriangle3SingleArray;
begin
  ValidateTrianglesList(OverTriangulate);
  Result := FTrianglesList[OverTriangulate];
end;

procedure TVRMLScene.InvalidateTrianglesList(const OverTriangulate: boolean);
var
  ValidityValue: TVRMLSceneValidity;
begin
  if OverTriangulate then
    ValidityValue := fvTrianglesListOverTriangulate else
    ValidityValue := fvTrianglesListNotOverTriangulate;
  Exclude(Validities, ValidityValue);
  FreeAndNil(FTrianglesList[OverTriangulate]);
end;

function TVRMLScene.TrianglesListShadowCasters: TDynTrianglesShadowCastersArray;

  function CreateTrianglesListShadowCasters: TDynTrianglesShadowCastersArray;

    function ShadowCaster(AShape: TVRMLShape): boolean;
    var
      Shape: TNodeX3DShapeNode;
    begin
      Shape := AShape.State.ParentShape;
      Result := not (
        (Shape <> nil) and
        (Shape.FdAppearance.Value <> nil) and
        (Shape.FdAppearance.Value is TNodeKambiAppearance) and
        (not TNodeKambiAppearance(Shape.FdAppearance.Value).FdShadowCaster.Value));
    end;

  var
    SI: TVRMLShapeTreeIterator;
    TriangleAdder: TTriangleAdder;
    WasSomeTransparentShadowCaster: boolean;
  begin
    Result := TDynTrianglesShadowCastersArray.Create;
    try
      Result.AllowedCapacityOverflow := TrianglesCount(false);
      try
        TriangleAdder := TTriangleAdder.Create;
        try
          TriangleAdder.TriangleList := Result;

          { This variable allows a small optimization: if there are
            no transparent triangles for shadow casters,
            then there's no need to iterate over Shapes
            second time. }
          WasSomeTransparentShadowCaster := false;

          { Add all opaque triangles }
          SI := TVRMLShapeTreeIterator.Create(Shapes, true);
          try
            while SI.GetNext do
              if ShadowCaster(SI.Current) then
              begin
                if not SI.Current.Transparent then
                  SI.Current.Geometry.Triangulate(
                    SI.Current.State, false, @TriangleAdder.AddTriangle) else
                  WasSomeTransparentShadowCaster := true;
              end;
          finally FreeAndNil(SI) end;

          { Mark OpaqueCount border }
          Result.FOpaqueCount := Result.Count;

          { Add all transparent triangles }
          if WasSomeTransparentShadowCaster then
          begin
            SI := TVRMLShapeTreeIterator.Create(Shapes, true);
            try
              while SI.GetNext do
                if ShadowCaster(SI.Current) and
                   SI.Current.Transparent then
                  SI.Current.Geometry.Triangulate(
                    SI.Current.State, false, @TriangleAdder.AddTriangle);
            finally FreeAndNil(SI) end;
          end;

          if Log then
            WritelnLog('Shadows', Format('Shadows casters triangles: %d opaque, %d total',
              [Result.OpaqueCount, Result.Count]));

        finally FreeAndNil(TriangleAdder) end;
      finally Result.AllowedCapacityOverflow := 4 end;
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

procedure TVRMLScene.InvalidateTrianglesListShadowCasters;
begin
  Exclude(Validities, fvTrianglesListShadowCasters);
  FreeAndNil(FTrianglesListShadowCasters);
end;

{ edges lists ------------------------------------------------------------- }

procedure TVRMLScene.CalculateIfNeededManifoldAndBorderEdges;

  { Sets FManifoldEdges and FBorderEdges. Assumes that FManifoldEdges and
    FBorderEdges are @nil on enter. }
  procedure CalculateManifoldAndBorderEdges;

    { If the counterpart of this edge (edge from neighbor) exists in
      EdgesSingle, then it adds this edge (along with it's counterpart)
      to FManifoldEdges.

      Otherwise, it just adds the edge to EdgesSingle. This can happen
      if it's the 1st time this edge occurs, or maybe the 3d one, 5th...
      all odd occurences, assuming that ordering of faces is consistent,
      so that counterpart edges are properly detected. }
    procedure AddEdgeCheckManifold(
      EdgesSingle: TDynManifoldEdgeArray;
      const TriangleIndex: Cardinal;
      const V0: TVector3Single;
      const V1: TVector3Single;
      const VertexIndex: Cardinal;
      Triangles: TDynTriangle3SingleArray);
    var
      I: Integer;
      EdgePtr: PManifoldEdge;
    begin
      if EdgesSingle.Count <> 0 then
      begin
        EdgePtr := EdgesSingle.Pointers[0];
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          { It would also be possible to get EdgePtr^.V0/1 by code like

            TrianglePtr := @Triangles.Items[EdgePtr^.Triangles[0]];
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
            FManifoldEdges.Add(EdgePtr^);

            { Remove this from EdgesSingle.
              Note that we delete from EdgesSingle fast, using assignment and
              deleting only from the end (normal Delete would want to shift
              EdgesSingle contents in memory, to preserve order of items;
              but we don't care about order). }
            EdgePtr^ := EdgesSingle.Items[EdgesSingle.Count - 1];
            EdgesSingle.DecLength;

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
    Triangles: TDynTriangle3SingleArray;
    TrianglePtr: PTriangle3Single;
    EdgesSingle: TDynManifoldEdgeArray;
  begin
    Assert(FManifoldEdges = nil);
    Assert(FBorderEdges = nil);

    { It's important here that TrianglesListShadowCasters guarentees that only valid
      triangles are included. Otherwise degenerate triangles could make
      shadow volumes rendering result bad. }
    Triangles := TrianglesListShadowCasters;

    FManifoldEdges := TDynManifoldEdgeArray.Create;
    { There is a precise relation between number of edges and number of faces
      on a closed manifold: E = T * 3 / 2. }
    FManifoldEdges.AllowedCapacityOverflow := Triangles.Count * 3 div 2;

    { EdgesSingle are edges that have no neighbor,
      i.e. have only one adjacent triangle. At the end, what's left here
      will be simply copied to BorderEdges. }
    EdgesSingle := TDynManifoldEdgeArray.Create;
    try
      EdgesSingle.AllowedCapacityOverflow := Triangles.Count * 3 div 2;

      TrianglePtr := Triangles.Pointers[0];
      for I := 0 to Triangles.Count - 1 do
      begin
        { TrianglePtr points to Triangles[I] now }
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[0], TrianglePtr^[1], 0, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[1], TrianglePtr^[2], 1, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^[2], TrianglePtr^[0], 2, Triangles);
        Inc(TrianglePtr);
      end;

      FBorderEdges := TDynBorderEdgeArray.Create;

      if EdgesSingle.Count <> 0 then
      begin
        { scene not a perfect manifold: less than 2 faces for some edges
          (the case with more than 2 is already eliminated above).
          So we copy EdgesSingle to BorderEdges. }
        FBorderEdges.Count := EdgesSingle.Count;
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          FBorderEdges.Items[I].VertexIndex := EdgesSingle.Items[I].VertexIndex;
          FBorderEdges.Items[I].TriangleIndex := EdgesSingle.Items[I].Triangles[0];
        end;
      end;
    finally FreeAndNil(EdgesSingle); end;

    if Log then
      WritelnLog('Shadows', Format(
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

function TVRMLScene.ManifoldEdges: TDynManifoldEdgeArray;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FManifoldEdges;
end;

function TVRMLScene.BorderEdges: TDynBorderEdgeArray;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FBorderEdges;
end;

procedure TVRMLScene.ShareManifoldAndBorderEdges(
  ManifoldShared: TDynManifoldEdgeArray;
  BorderShared: TDynBorderEdgeArray);
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

procedure TVRMLScene.InvalidateManifoldAndBorderEdges;
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

procedure TVRMLScene.FreeResources_UnloadTextureData(Node: TVRMLNode);
begin
  (Node as TVRMLTextureNode).IsTextureLoaded := false;
end;

procedure TVRMLScene.FreeResources_UnloadTexture3DData(Node: TVRMLNode);
begin
  (Node as TNodeX3DTexture3DNode).TextureLoaded := false;
end;

procedure TVRMLScene.FreeResources_UnloadBackgroundImage(Node: TVRMLNode);
begin
  (Node as TNodeBackground).BgImagesLoaded := false;
end;

procedure TVRMLScene.FreeResources(Resources: TVRMLSceneFreeResources);
begin
  if (frRootNode in Resources) and OwnsRootNode then
  begin
    RootNode.Free;
    RootNode := nil;
  end;

  if (frTextureDataInNodes in Resources) and (RootNode <> nil) then
  begin
    RootNode.EnumerateNodes(TVRMLTextureNode,
      @FreeResources_UnloadTextureData, false);
    RootNode.EnumerateNodes(TNodeX3DTexture3DNode,
      @FreeResources_UnloadTexture3DData, false);
  end;

  if (frBackgroundImageInNodes in Resources) and (RootNode <> nil) then
    RootNode.EnumerateNodes(TNodeBackground,
      @FreeResources_UnloadBackgroundImage, false);

  if frTrianglesListNotOverTriangulate in Resources then
    InvalidateTrianglesList(false);

  if frTrianglesListOverTriangulate in Resources then
    InvalidateTrianglesList(true);

  if frTrianglesListShadowCasters in Resources then
    InvalidateTrianglesListShadowCasters;

  if frManifoldAndBorderEdges in Resources then
    InvalidateManifoldAndBorderEdges;
end;

{ events --------------------------------------------------------------------- }

{ We're using AddIfNotExists, not simple Add, in Collect_ routines
  below:

  - for time-dependent nodes (TimeSensor, MovieTexture etc.),
    duplicates would cause time to be then incremented many times
    during single SetWorldTime, so their local time would grow too fast.

  - for other sensors, events would be passed twice.
}

procedure TVRMLScene.CollectNodeForEvents(Node: TVRMLNode);
begin
  Node.ParentEventsProcessor := Self;

  if Node is TNodeKeySensor then
    KeySensorNodes.AddIfNotExists(Node) else
  if Node is TNodeTimeSensor then
    TimeSensorNodes.AddIfNotExists(Node) else
  if Node is TNodeMovieTexture then
    MovieTextureNodes.AddIfNotExists(Node);
end;

procedure TVRMLScene.ScriptsInitialize(Node: TVRMLNode);
begin
  TNodeScript(Node).Initialized := true;
end;

procedure TVRMLScene.TraverseForEvents(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
var
  PSI: PProximitySensorInstance;
begin
  PSI := ProximitySensorInstances.Add;
  PSI^.Node := Node as TNodeProximitySensor;
  PSI^.InvertedTransform := StateStack.Top.InvertedTransform;
  PSI^.IsActive := false; { IsActive = false initially }
end;

procedure TVRMLScene.CollectNodesForEvents;
begin
  KeySensorNodes.Clear;
  TimeSensorNodes.Clear;
  MovieTextureNodes.Clear;
  ProximitySensorInstances.Count := 0;

  RootNode.EnumerateNodes(TVRMLNode, @CollectNodeForEvents, false);

  { Proximity sensors collecting requires a State.Transform, so
    we must do full Traverse, EnumerateNodes is not enough.
    Also, this means that ProximitySensor are detected only in active
    VRML graph part. }
  RootNode.Traverse(TNodeProximitySensor, @TraverseForEvents);

  BeginChangesSchedule;
  try
    { We have to initialize scripts only after all other initialization
      is done, in particular after CollectNodeForEvents was called
      for all and set their ParentEventsProcessor. Reason: scripts
      initialize() methods may already cause some events, that should
      notify us appropriately.

      This is also why Begin/EndChangesSchedule around is useful. }
    RootNode.EnumerateNodes(TNodeScript, @ScriptsInitialize, false);
  finally EndChangesSchedule end;
end;

procedure TVRMLScene.UnCollectForEvents(Node: TVRMLNode);
begin
  Node.ParentEventsProcessor := nil;
end;

procedure TVRMLScene.ScriptsDeInitialize(Node: TVRMLNode);
begin
  TNodeScript(Node).Initialized := false;
end;

procedure TVRMLScene.UnregisterProcessEvents(Node: TVRMLNode);
begin
  BeginChangesSchedule;
  try
    { We have to deinitialize scripts before any other deinitialization
      is done. Just like for ScriptsInitialize. }
    Node.EnumerateNodes(TNodeScript, @ScriptsDeInitialize, false);
  finally EndChangesSchedule end;

  Node.EnumerateNodes(TVRMLNode, @UnCollectForEvents, false);
end;

procedure TVRMLScene.SetProcessEvents(const Value: boolean);

  { When ProcessEvents is set to @true, you want to call initial
    position/orientation_changed events.

    Implementation below essentially is like ViewerChanged,
    except it checks IsLastViewer (instead of setting it always to @true). }
  procedure InitialProximitySensorsEvents;
  var
    I: Integer;
  begin
    Inc(FWorldTime.PlusTicks);
    BeginChangesSchedule;
    try
      if IsLastViewer then
      begin
        for I := 0 to ProximitySensorInstances.Count - 1 do
          ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
      end;
    finally EndChangesSchedule end;
  end;

begin
  if FProcessEvents <> Value then
  begin
    if Value then
    begin
      KeySensorNodes := TVRMLNodesList.Create;
      TimeSensorNodes := TVRMLNodesList.Create;
      MovieTextureNodes := TVRMLNodesList.Create;
      ProximitySensorInstances := TDynProximitySensorInstanceArray.Create;

      FProcessEvents := Value;

      CollectNodesForEvents;
      InitialProximitySensorsEvents;

      RenderState.OnCameraChanged.Add(@CameraChanged);
    end else
    begin
      UnregisterProcessEvents(RootNode);
      FreeAndNil(KeySensorNodes);
      FreeAndNil(TimeSensorNodes);
      FreeAndNil(MovieTextureNodes);
      FreeAndNil(ProximitySensorInstances);
      PointingDeviceClear;

      FProcessEvents := Value;

      RenderState.OnCameraChanged.Remove(@CameraChanged);
    end;
  end;
end;

{ key sensors handling ------------------------------------------------------- }

{ Convert TKey to VRML "action" key code.
  As defined by X3D KeySensor node specification. }
function KeyToActionKey(Key: TKey; out ActionKey: Integer): boolean;
begin
  Result := true;
  case Key of
    K_F1 .. K_F12 : ActionKey := Key - K_F1 + 1;
    K_Home     : ActionKey := 13;
    K_End      : ActionKey := 14;
    K_PageUp   : ActionKey := 15;
    K_PageDown : ActionKey := 16;
    K_Up       : ActionKey := 17;
    K_Down     : ActionKey := 18;
    K_Left     : ActionKey := 19;
    K_Right    : ActionKey := 20;
    else Result := false;
  end;
end;

function TVRMLScene.KeyDown(Key: TKey; C: char): boolean;
var
  I: Integer;
  KeySensor: TNodeKeySensor;
  ActionKey: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if ProcessEvents then
  begin
    Inc(FWorldTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeySensorNodes.Count - 1 do
      begin
        KeySensor := KeySensorNodes.Items[I] as TNodeKeySensor;
        if KeySensor.FdEnabled.Value then
        begin
          { Do not treat it as handled (returning ExclusiveEvents),
            this would disable too much (like Navigator usually under Scene on Controls).
          Result := false; }
          KeySensor.EventIsActive.Send(true, WorldTime);
          if KeyToActionKey(Key, ActionKey) then
            KeySensor.EventActionKeyPress.Send(ActionKey, WorldTime);
          if C <> #0 then
            KeySensor.EventKeyPress.Send(C, WorldTime);
          case Key of
            K_Alt: KeySensor.EventAltKey.Send(true, WorldTime);
            K_Ctrl: KeySensor.EventControlKey.Send(true, WorldTime);
            K_Shift: KeySensor.EventShiftKey.Send(true, WorldTime);
          end;
        end;
      end;
    finally EndChangesSchedule; end;
  end;
end;

function TVRMLScene.KeyUp(Key: TKey; C: char): boolean;
var
  I: Integer;
  KeySensor: TNodeKeySensor;
  ActionKey: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if ProcessEvents then
  begin
    Inc(FWorldTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeySensorNodes.Count - 1 do
      begin
        KeySensor := KeySensorNodes.Items[I] as TNodeKeySensor;
        if KeySensor.FdEnabled.Value then
        begin
          { Do not treat it as handled (returning ExclusiveEvents),
            this would disable too much (like Navigator usually under Scene on Controls).
          Result := false; }
          KeySensor.EventIsActive.Send(false, WorldTime);
          if KeyToActionKey(Key, ActionKey) then
            KeySensor.EventActionKeyRelease.Send(ActionKey, WorldTime);
          if C <> #0 then
            KeySensor.EventKeyRelease.Send(C, WorldTime);
          case Key of
            K_Alt: KeySensor.EventAltKey.Send(false, WorldTime);
            K_Ctrl: KeySensor.EventControlKey.Send(false, WorldTime);
            K_Shift: KeySensor.EventShiftKey.Send(false, WorldTime);
          end;
        end;
      end;
    finally EndChangesSchedule; end;
  end;
end;

{ pointing device handling --------------------------------------------------- }

procedure TVRMLScene.PointingDeviceMove(const OverPoint: TVector3Single;
  const OverItem: PVRMLTriangle);
var
  TouchSensor: TNodeTouchSensor;
  OldIsOver, NewIsOver: boolean;
  OldSensors: TVRMLNodesList;
  NewSensors: TVRMLNodesList;
  I: Integer;
begin
  if ProcessEvents then
  begin
    Inc(FWorldTime.PlusTicks);
    { Note that using Begin/EndChangesSchedule is not only for efficiency
      here. It's also sometimes needed to keep the code correct: note
      that ChangedAll changes everything, including State pointers.
      So OverPoint.State becomes invalid. (Once octree will be rebuild, also
      OverPoint will be invalid.) Obviously, we can't let this happen
      in the middle of PointingDeviceMove. }
    BeginChangesSchedule;
    try
      { Handle isOver events }

      if PointingDeviceOverItem <> OverItem then
      begin
        if PointingDeviceActiveSensor <> nil then
        begin
          Assert(PointingDeviceActive);

          { This is quite special situation, as it means that
            PointingDeviceActiveSensor has grabbed all events.
            isOver (either TRUE or FALSE) may be generated for active
            sensor, but no other sensors should receive any events. }

          if (PointingDeviceActiveSensor is TNodeX3DPointingDeviceSensorNode) and
            TNodeX3DPointingDeviceSensorNode(PointingDeviceActiveSensor).FdEnabled.Value then
          begin
            OldIsOver := (PointingDeviceOverItem <> nil) and
              (PointingDeviceOverItem^.State.PointingDeviceSensors.
                IndexOf(PointingDeviceActiveSensor) <> -1);

            NewIsOver := (OverItem <> nil) and
              (OverItem^.State.PointingDeviceSensors.
                IndexOf(PointingDeviceActiveSensor) <> -1);

            if OldIsOver <> NewIsOver then
            begin
              PointingDeviceActiveSensor.EventIsOver.Send(NewIsOver, WorldTime);
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
            intended to be catched immediately if they happened because
            the geometry is animating. But, still, all isOver changes *are*
            reported. I hope they don't mean that isOver changes because
            of underlying geometry animating should not be reported at all.

            The latter interpretation:
            - Would be unhandy for implementation, as I would have to call
              raycollision twice for each MouseMove (to get
              OverPointBeforeMove and OverPoint).
            - Would be unhandy for users. You want
              to catch isOver changes eventually, right? Otherwise,
              user may be forced to move mouse out and then back in to generate
              isOver event = TRUE. Worse, user has to move mouse inside and
              then back outside to generate isOver = FALSE event.
          }

          for I := 0 to OldSensors.Count - 1 do
            if NewSensors.IndexOf(OldSensors[I]) = -1 then
            begin
              if (OldSensors[I] is TNodeX3DPointingDeviceSensorNode) and
                TNodeX3DPointingDeviceSensorNode(OldSensors[I]).FdEnabled.Value then
                TNodeX3DPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, WorldTime);
            end;

          for I := 0 to NewSensors.Count - 1 do
            if OldSensors.IndexOf(NewSensors[I]) = -1 then
            begin
              if (NewSensors[I] is TNodeX3DPointingDeviceSensorNode) and
                TNodeX3DPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
                TNodeX3DPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, WorldTime);
            end;
        end else
        if PointingDeviceOverItem <> nil then
        begin
          { So we previously pointed as something, and now at nothing.
            So simply call isOver = FALSE for all. }

          OldSensors := PointingDeviceOverItem^.State.PointingDeviceSensors;

          for I := 0 to OldSensors.Count - 1 do
            if (OldSensors[I] is TNodeX3DPointingDeviceSensorNode) and
              TNodeX3DPointingDeviceSensorNode(OldSensors[I]).FdEnabled.Value then
              TNodeX3DPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, WorldTime);
        end else
        begin
          Assert(OverItem <> nil);

          { So we previously pointed as nothing, and now at something.
            So simply call isOver = TRUE for all. }

          NewSensors := OverItem^.State.PointingDeviceSensors;

          for I := 0 to NewSensors.Count - 1 do
            if (NewSensors[I] is TNodeX3DPointingDeviceSensorNode) and
              TNodeX3DPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
              TNodeX3DPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, WorldTime);
        end;

        FPointingDeviceOverItem := OverItem;

        DoPointingDeviceSensorsChange;
      end;

      { Handle hitXxx_changed events }

      if OverItem <> nil then
      begin
        NewSensors := OverItem^.State.PointingDeviceSensors;

        for I := 0 to NewSensors.Count - 1 do
          if NewSensors[I] is TNodeTouchSensor then
          begin
            TouchSensor := TNodeTouchSensor(NewSensors[I]);
            if TouchSensor.FdEnabled.Value then
            begin
              TouchSensor.EventHitPoint_Changed.Send(
                { hitPoint_changed event wants a point in local coords,
                  we can get this by InverseTransform. }
                MatrixMultPoint(OverItem^.State.InvertedTransform, OverPoint), WorldTime);

              { The best normal I can generate for now is flat normal
                for the hit triangle. }
              TouchSensor.EventHitNormal_Changed.Send(
                OverItem^.World.Normal, WorldTime);

              { TODO: hitTexCoord_changed generation should also be done
                here, but honestly I just cannot do this with current
                information. Triangulation must be much improved to
                provide this. }
            end;
          end;
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TVRMLScene.DoPointingDeviceSensorsChange;
begin
  if Assigned(OnPointingDeviceSensorsChange) then
    OnPointingDeviceSensorsChange(Self);
end;

procedure TVRMLScene.PointingDeviceClear;
begin
  FPointingDeviceOverItem := nil;
  FPointingDeviceActive := false;
  FPointingDeviceActiveSensor := nil;
end;

procedure TVRMLScene.SetPointingDeviceActive(const Value: boolean);

  procedure AnchorActivate(Anchor: TNodeAnchor);
  var
    NewRootNode: TVRMLNode;
    NewViewpoint: TVRMLViewpointNode;
  begin
    if Anchor.LoadAnchor(NewRootNode, NewViewpoint, RootNode) then
    begin
      if NewRootNode <> nil then
      begin
        PointingDeviceClear;
        if OwnsRootNode then FreeAndNil(FRootNode);
        RootNode := NewRootNode;
        OwnsRootNode := true;
        ScheduleChangedAll;
      end;
      if NewViewpoint <> nil then
        NewViewpoint.EventSet_Bind.Send(true, WorldTime);
    end;
  end;

var
  I: Integer;
  ToActivate: TVRMLNode;
  Sensors: TVRMLNodesList;
begin
  if ProcessEvents and (FPointingDeviceActive <> Value) then
  begin
    Inc(FWorldTime.PlusTicks);
    BeginChangesSchedule;
    try
      FPointingDeviceActive := Value;
      if Value then
      begin
        if PointingDeviceOverItem <> nil then
        begin
          Sensors := PointingDeviceOverItem^.State.PointingDeviceSensors;
          for I := 0 to Sensors.Count - 1 do
          begin
            { Activate the first enabled sensor.
              TODO: this is actually bad, spec says to activate
              simultaneouly all sensors on Sensors list (tied for this node). }
            ToActivate := Sensors[I];
            if (ToActivate is TNodeX3DPointingDeviceSensorNode) and
               (TNodeX3DPointingDeviceSensorNode(ToActivate).FdEnabled.Value) then
            begin
              { Send isActive = true and make DoPointingDeviceSensorsChange
                only if FPointingDeviceActiveSensor changes. }
              if FPointingDeviceActiveSensor <> ToActivate then
              begin
                FPointingDeviceActiveSensor :=
                  TNodeX3DPointingDeviceSensorNode(ToActivate);
                PointingDeviceActiveSensor.EventIsActive.Send(true, WorldTime);
                DoPointingDeviceSensorsChange;
              end;
              Break;
            end else
            if ToActivate is TNodeAnchor then
            begin
              AnchorActivate(TNodeAnchor(ToActivate));
              DoPointingDeviceSensorsChange;
              Break;
            end;
          end;
        end;
      end else
      begin
        { Deactivate PointingDeviceActiveSensor (if any) }
        if PointingDeviceActiveSensor <> nil then
        begin
          PointingDeviceActiveSensor.EventIsActive.Send(false, WorldTime);
          { If we're still over the sensor, generate touchTime for TouchSensor }
          if (PointingDeviceOverItem <> nil) and
             (PointingDeviceOverItem^.State.PointingDeviceSensors.
               IndexOf(PointingDeviceActiveSensor) <> -1) and
             (PointingDeviceActiveSensor is TNodeTouchSensor) then
          begin
            TNodeTouchSensor(PointingDeviceActiveSensor).
              EventTouchTime.Send(WorldTime.Seconds, WorldTime);
          end;
          FPointingDeviceActiveSensor := nil;
          DoPointingDeviceSensorsChange;
        end;
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

function TVRMLScene.PointingDeviceSensors: TPointingDeviceSensorsList;
begin
  if PointingDeviceOverItem <> nil then
    Result := PointingDeviceOverItem^.State.PointingDeviceSensors else
    Result := nil;
end;

function TVRMLScene.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Button = mbLeft then
  begin
    PointingDeviceActive := true;
    { Do not treat it as handled (returning ExclusiveEvents),
      this would disable too much (like Navigator usually under Scene on Controls).
    Result := false; }
  end;
end;

function TVRMLScene.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Button = mbLeft then
  begin
    PointingDeviceActive := false;
    { Do not treat it as handled (returning ExclusiveEvents),
      this would disable too much (like Navigator usually under Scene on Controls).
    Result := false; }
  end;
end;

function TVRMLScene.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  Ray0, RayVector: TVector3Single;
  OverPoint: TVector3Single;
  Item: PVRMLTriangle;
begin
  Result := inherited;
  if Result then Exit;

  if OctreeCollisions <> nil then
  begin
    Navigator.Ray(NewX, NewY, AngleOfViewX, AngleOfViewY, Ray0, RayVector);

    Item := OctreeCollisions.RayCollision(
      OverPoint, Ray0, RayVector, true, nil, false, nil);

    PointingDeviceMove(OverPoint, Item);

    { Do not treat it as handled (returning ExclusiveEvents),
      this would disable too much (like Navigator usually under Scene on Controls).
    Result := false; }
  end;
end;

function TVRMLScene.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

{ WorldTime stuff ------------------------------------------------------------ }

procedure TVRMLScene.InternalSetWorldTime(
  const NewValue: TVRMLTime; const TimeIncrease: TKamTime);
var
  SomethingChanged: boolean;
  I: Integer;
begin
  if ProcessEvents then
  begin
    BeginChangesSchedule;
    try
      SomethingChanged := false;

      for I := 0 to MovieTextureNodes.Count - 1 do
        (MovieTextureNodes.Items[I] as TNodeMovieTexture).
          TimeDependentNodeHandler.SetWorldTime(
            WorldTime, NewValue, TimeIncrease, SomethingChanged);

      { If SomethingChanged on MovieTexture nodes, then we have to redisplay.
        Note that this is not needed for other time-dependent nodes
        (like TimeSensor), as they are not visible (and so can influence
        other nodes only by events, and this will change EventChanged
        to catch it). }
      if SomethingChanged then
        VisibleSceneChange([prVisibleSceneGeometry, prVisibleSceneNonGeometry]);

      for I := 0 to TimeSensorNodes.Count - 1 do
        (TimeSensorNodes.Items[I] as TNodeTimeSensor).
          TimeDependentNodeHandler.SetWorldTime(
            WorldTime, NewValue, TimeIncrease, SomethingChanged);
    finally
      EndChangesSchedule;
    end;
  end;

  FWorldTime := NewValue;
end;

procedure TVRMLScene.SetWorldTime(const NewValue: TKamTime);
var
  TimeIncrease: TKamTime;
  NewCompleteValue: TVRMLTime;
begin
  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  TimeIncrease := NewValue - FWorldTime.Seconds;
  if TimeIncrease > 0 then
    InternalSetWorldTime(NewCompleteValue, TimeIncrease);
end;

procedure TVRMLScene.IncreaseWorldTime(const TimeIncrease: TKamTime);
var
  NewCompleteValue: TVRMLTime;
begin
  NewCompleteValue.Seconds := FWorldTime.Seconds + TimeIncrease;
  NewCompleteValue.PlusTicks := 0;
  if TimeIncrease > 0 then
    InternalSetWorldTime(NewCompleteValue, TimeIncrease);
end;

procedure TVRMLScene.ResetLastEventTime(Node: TVRMLNode);
var
  I: Integer;
begin
  for I := 0 to Node.Routes.Count - 1 do
    Node.Routes[I].ResetLastEventTime;
  if Node is TNodeX3DScriptNode then
    TNodeX3DScriptNode(Node).ResetLastEventTimes;
end;

procedure TVRMLScene.ResetWorldTime(const NewValue: TKamTime);
var
  NewCompleteValue: TVRMLTime;
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes(@ResetLastEventTime, false);

  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  InternalSetWorldTime(NewCompleteValue, 0);
end;

procedure TVRMLScene.ResetWorldTimeAtLoad;
var
  WorldTimeAtLoad: TKamTime;
begin
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TNodeKambiNavigationInfo) and
     TNodeKambiNavigationInfo(NavigationInfoStack.Top).FdTimeOriginAtLoad.Value
    then
    WorldTimeAtLoad := 0.0 else
    WorldTimeAtLoad := DateTimeToUnix(Now);
  ResetWorldTime(WorldTimeAtLoad);
end;

procedure TVRMLScene.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  { Ignore Idle calls when CompSpeed is precisely zero
    (this may happen, and is correct, see TGLWindow.IgnoreNextIdleSpeed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway. }
  if TimePlaying and (CompSpeed <> 0) then
    IncreaseWorldTime(TimePlayingSpeed * CompSpeed);

  { Even if mouse is over the scene, still allow others (like a Navigator
    underneath) to always handle mouse and keys in their Idle. }
  LetOthersHandleMouseAndKeys := true;
end;

{ geometry changes schedule -------------------------------------------------- }

procedure TVRMLScene.BeginGeometryChangedSchedule;
begin
  { GeometryChangedScheduled = false always when GeometrySchedule = 0. }
  Assert((GeometrySchedule <> 0) or (not GeometryChangedScheduled));

  Inc(GeometrySchedule);
end;

procedure TVRMLScene.ScheduleGeometryChanged;
begin
  if GeometrySchedule = 0 then
    DoGeometryChanged else
    GeometryChangedScheduled := true;
end;

procedure TVRMLScene.EndGeometryChangedSchedule;
begin
  Dec(GeometrySchedule);
  if (GeometrySchedule = 0) and GeometryChangedScheduled then
  begin
    { Set GeometryChangedScheduled before DoGeometryChanged,
      to be in consistent state (pass assertion in BeginGeometryChangedSchedule).
      Note that DoGeometryChanged may call many things,
      e.g. it may want to show a progress bar while constructing the octree,
      that will want to render the screen for background of progress bar,
      that will want to update some generated texture nodes,
      that will call CameraChanged to eventually make camera events,
      that will secure itself by BeginGeometryChangedSchedule... }

    GeometryChangedScheduled := false;

    DoGeometryChanged;
  end;
end;

{ changes schedule ----------------------------------------------------------- }

procedure TVRMLScene.BeginChangesSchedule;
begin
  BeginGeometryChangedSchedule;

  { ChangedAllScheduled = false always when ChangedAllSchedule = 0. }
  Assert((ChangedAllSchedule <> 0) or (not ChangedAllScheduled));

  Inc(ChangedAllSchedule);
end;

procedure TVRMLScene.ScheduleChangedAll;
begin
  if ChangedAllSchedule = 0 then
    ChangedAll else
    ChangedAllScheduled := true;
end;

procedure TVRMLScene.EndChangesSchedule;
begin
  Dec(ChangedAllSchedule);
  if (ChangedAllSchedule = 0) and ChangedAllScheduled then
  begin
    ChangedAllScheduled := false;

    { ChangedAll calls CollectNodesForEvents, which in turn calls
      Begin/EndChangesSchedule. Which means that
      ChangedAllSchedule/ChangedAllScheduled must be Ok for this.
      That's why ChangedAllScheduled := false must be done previously. }

    ChangedAll;
  end;

  { Call EndGeometryChangedSchedule after finalizing ChangeAll schedule.
    Reason: ChangedAll may (in fact, it always does, currently) call
    geometry changed (so it will always be scheduled and performed only below).
    But GeometryChanged cannot call ChangedAll. So all is Ok.

    Doing it the other way around (first EndGeometryChangedSchedule,
    then finalize ChangedAll) would mean that GeometryChanged is possibly
    done twice (once at EndGeometryChangedSchedule, then from ChangedAll). }

  EndGeometryChangedSchedule;
end;

{ proximity sensor ----------------------------------------------------------- }

procedure TVRMLScene.ProximitySensorUpdate(var PSI: TProximitySensorInstance);
var
  Position, Direction, Up: TVector3Single;
  Node: TNodeProximitySensor;
  NewIsActive: boolean;
begin
  Assert(IsLastViewer);
  if ProcessEvents then
  begin
    BeginChangesSchedule;
    try
      Node := PSI.Node;
      if not Node.FdEnabled.Value then Exit;

      { In each ProximitySensorUpdate we transform ViewerPosition to
        ProximitySensor coordinate-space. This allows us to check
        whether the viewer is inside ProximitySensor precisely
        (otherwise transforming ProximitySensor box could make a larger
        box, as we do not support oriented bounding boxes, only
        axis-aligned).

        This is also needed to generate position_changed value,
        as it has to be in ProximitySensor coordinate-space.

        Also, since we don't store precalculated box of ProximitySensor,
        we can gracefully react in ChangedFields to changes:
        - changes to ProximitySensor center and size must only produce
          new ProximitySensorUpdate to eventually activate/deactivate ProximitySensor
        - changes to transforms affecting ProximitySensor must only update
          it's InvertedTransform and call ProximitySensorUpdate.
      }

      Position := MatrixMultPoint(PSI.InvertedTransform, LastViewerPosition);

      NewIsActive :=
        (Position[0] >= Node.FdCenter.Value[0] - Node.FdSize.Value[0] / 2) and
        (Position[0] <= Node.FdCenter.Value[0] + Node.FdSize.Value[0] / 2) and
        (Position[1] >= Node.FdCenter.Value[1] - Node.FdSize.Value[1] / 2) and
        (Position[1] <= Node.FdCenter.Value[1] + Node.FdSize.Value[1] / 2) and
        (Position[2] >= Node.FdCenter.Value[2] - Node.FdSize.Value[2] / 2) and
        (Position[2] <= Node.FdCenter.Value[2] + Node.FdSize.Value[2] / 2) and
        { ... and the box is not empty, which for ProximitySensor
          is signalled by any size <= 0 (yes, equal 0 also means empty).
          We check this at the end, as this is the least common situation? }
        (Node.FdSize.Value[0] > 0) and
        (Node.FdSize.Value[1] > 0) and
        (Node.FdSize.Value[2] > 0);

      if NewIsActive <> PSI.IsActive then
      begin
        PSI.IsActive := NewIsActive;
        Node.EventIsActive.Send(NewIsActive, WorldTime);
        if NewIsActive then
          Node.EventEnterTime.Send(WorldTime.Seconds, WorldTime) else
          Node.EventExitTime.Send(WorldTime.Seconds, WorldTime);
      end;

      { Call position_changed, orientation_changed, even if this is just
        the first time NewIsActive = true (that is, even when it was
        NewIsActive <> PSI.IsActive). Reasoning: this allows to activate
        ProximitySensor when world starts (before player moves),
        which is a wanted feature. }

      if NewIsActive then
      begin
        Node.EventPosition_Changed.Send(Position, WorldTime);
        if Node.EventOrientation_Changed.SendNeeded then
        begin
          Direction := MatrixMultDirection(PSI.InvertedTransform, LastViewerDirection);
          Up        := MatrixMultDirection(PSI.InvertedTransform, LastViewerUp);
          Node.EventOrientation_Changed.Send(
            CamDirUp2Orient(Direction, Up), WorldTime);
        end;
        { TODO: centerOfRotation_changed }
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TVRMLScene.ViewerChanged(ANavigator: TNavigator;
  const Changes: TVisibleSceneChanges);
var
  I: Integer;
begin
  ANavigator.GetCameraVectors(
    FLastViewerPosition, FLastViewerDirection, FLastViewerUp);
  FIsLastViewer := true;

  BeginChangesSchedule;
  try
    for I := 0 to ShapeLODs.Count - 1 do
      UpdateLODLevel(TVRMLShapeTreeLOD(ShapeLODs.Items[I]));

    if ProcessEvents then
    begin
      Inc(FWorldTime.PlusTicks);
      for I := 0 to ProximitySensorInstances.Count - 1 do
        ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
    end;
  finally EndChangesSchedule end;

  VisibleSceneChange(Changes + [prViewer]);
end;

{ compiled scripts ----------------------------------------------------------- }

procedure TVRMLScene.RegisterCompiledScript(const HandlerName: string;
  Handler: TCompiledScriptHandler);
var
  HandlerInfo: PCompiledScriptHandlerInfo;
begin
  HandlerInfo := CompiledScriptHandlers.Add;
  HandlerInfo^.Handler := Handler;
  HandlerInfo^.Name := HandlerName;
end;

{ navigator ------------------------------------------------------------------ }

function TVRMLScene.CreateNavigator(AOwner: TComponent): TNavigator;
var
  NavigationNode: TNodeNavigationInfo;
  I: Integer;
  CameraPreferredHeight, CameraRadius: Single;
begin
  NavigationNode := NavigationInfoStack.Top as TNodeNavigationInfo;

  Result := nil;

  if NavigationNode <> nil then
    for I := 0 to NavigationNode.FdType.Count - 1 do
      if NavigationNode.FdType.Items[I] = 'WALK' then
      begin
        Result := TWalkNavigator.Create(AOwner);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := true;
        TWalkNavigator(Result).Gravity := true;
        TWalkNavigator(Result).IgnoreAllInputs := false;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'FLY' then
      begin
        Result := TWalkNavigator.Create(AOwner);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := false;
        TWalkNavigator(Result).Gravity := false;
        TWalkNavigator(Result).IgnoreAllInputs := false;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'NONE' then
      begin
        Result := TWalkNavigator.Create(AOwner);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := true; { doesn't matter }
        TWalkNavigator(Result).Gravity := false;
        TWalkNavigator(Result).IgnoreAllInputs := true;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'EXAMINE' then
      begin
        Result := TExamineNavigator.Create(AOwner);
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'ANY' then
      begin
        { Do nothing, also do not report this NavigationInfo.type as unknown. }
      end else
        VRMLWarning(vwSerious, Format('Unknown NavigationInfo.type "%s"',
          [NavigationNode.FdType.Items[I]]));

  if Result = nil then
    { No recognized "type" found, so use default type EXAMINE. }
    Result := TExamineNavigator.Create(AOwner);

  { calculate CameraRadius }
  CameraRadius := 0;
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 1) then
    CameraRadius := NavigationNode.FdAvatarSize.Items[0];
  { if avatarSize doesn't specify CameraRadius, or specifies invalid <= 0,
    calculate something suitable based on Scene.BoundingBox. }
  if CameraRadius <= 0 then
    CameraRadius := Box3dAvgSize(BoundingBox, 1.0) * 0.005;

  Result.CameraRadius := CameraRadius;

  if Result is TWalkNavigator then
  begin
    { For NavigationNode = nil, always Examine is created. }
    Assert(NavigationNode <> nil);

    { calculate CameraPreferredHeight }
    if NavigationNode.FdAvatarSize.Count >= 2 then
      CameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      { Make it something >> CameraRadius * 2, to allow some
        space to decrease (e.g. by Input_DecreaseCameraPreferredHeight
        in view3dscene). Remember that CorrectCameraPreferredHeight
        adds a limit to CameraPreferredHeight, around CameraRadius * 2. }
      CameraPreferredHeight := CameraRadius * 4;

    TWalkNavigator(Result).CameraPreferredHeight := CameraPreferredHeight;
    TWalkNavigator(Result).CorrectCameraPreferredHeight;
  end else
  if Result is TExamineNavigator then
  begin
    TExamineNavigator(Result).Init(BoundingBox, CameraRadius);
  end;

  NavigatorBindToViewpoint(Result, false);
end;

procedure TVRMLScene.NavigatorBindToViewpoint(Nav: TNavigator;
  const OnlyViewpointVectorsChanged: boolean);
var
  CameraPos: TVector3Single;
  CameraDir: TVector3Single;
  CameraUp: TVector3Single;
  GravityUp: TVector3Single;
  NavigationNode: TNodeNavigationInfo;
  WalkNav: TWalkNavigator;
begin
  { Currently we can set viewpoint only to TWalkNavigator.
    This is supposed to be fixed one day (as currently VRML author
    has no control over ExamineNavigator). }
  if not (Nav is TWalkNavigator) then Exit;
  WalkNav := TWalkNavigator(Nav);

  if ViewpointStack.Top <> nil then
  begin
    (ViewpointStack.Top as TVRMLViewpointNode).GetCameraVectors(
      CameraPos, CameraDir, CameraUp, GravityUp);
  end else
  begin
    CameraPos := StdVRMLCamPos[1];
    CameraDir := StdVRMLCamDir;
    CameraUp := StdVRMLCamUp;
    GravityUp := StdVRMLGravityUp;
  end;

  NavigationNode := NavigationInfoStack.Top as TNodeNavigationInfo;

  { Change CameraDir length, to adjust speed.
    Also set MoveHorizontal/VerticalSpeed. }

  if NavigationNode = nil then
  begin
    { Since we don't have NavigationNode.speed, we just calculate some
      speed that should "feel sensible". We base it on CameraRadius.
      CameraRadius in turn was calculated based on
      Box3dAvgSize(SceneAnimation.BoundingBoxSum). }
    VectorAdjustToLengthTo1st(CameraDir, Nav.CameraRadius * 0.4);
    WalkNav.MoveHorizontalSpeed := 1;
    WalkNav.MoveVerticalSpeed := 1;
  end else
  if NavigationNode.FdSpeed.Value = 0 then
  begin
    { Then user is not allowed to move at all.

      CameraDir must be non-zero (we normalize it just to satisfy
      requirement that "length of CameraDir doesn't matter" here,
      in case user will later increase move speed by menu anyway.

      So we do this is by setting MoveHorizontal/VerticalSpeed to zero.
      This is also the reason why other SetViewpointCore must change
      MoveHorizontal/VerticalSpeed to something different than zero
      (otherwise, user would be stuck with speed = 0). }
    NormalizeTo1st(CameraDir);
    WalkNav.MoveHorizontalSpeed := 0;
    WalkNav.MoveVerticalSpeed := 0;
  end else
  begin
    VectorAdjustToLengthTo1st(CameraDir, NavigationNode.FdSpeed.Value / 50.0);
    WalkNav.MoveHorizontalSpeed := 1;
    WalkNav.MoveVerticalSpeed := 1;
  end;

  { If OnlyViewpointVectorsChanged, then we will move relative to
    initial camera changes. Else, we will jump to new initial camera vectors.

    Below, we do some work normally done by TWalkNavigator.Init.
    But we know we already have CameraPreferredHeight set (by CreateNavigator),
    and we take into account OnlyViewpointVectorsChanged case. }

  WalkNav.SetInitialCameraLookDir(CameraPos, CameraDir, CameraUp,
    OnlyViewpointVectorsChanged);
  WalkNav.GravityUp := GravityUp;
  if not OnlyViewpointVectorsChanged then
    WalkNav.Home;
end;

{ misc ----------------------------------------------------------------------- }

type
  BreakMainLightForShadows = class(TCodeBreaker);

procedure TVRMLScene.CalculateMainLightForShadowsPosition;
begin
  if FMainLightForShadowsNode is TVRMLPositionalLightNode then
    FMainLightForShadows := Vector4Single(
      MatrixMultPoint(
        FMainLightForShadowsTransform,
        TVRMLPositionalLightNode(FMainLightForShadowsNode).FdLocation.Value), 1) else
  if FMainLightForShadowsNode is TVRMLDirectionalLightNode then
    FMainLightForShadows := Vector4Single( Normalized(
      MatrixMultDirection(
        FMainLightForShadowsTransform,
        TVRMLDirectionalLightNode(FMainLightForShadowsNode).FdDirection.Value) ), 0) else
    raise Exception.CreateFmt('TVRMLScene.MainLightForShadows: ' +
      'light node "%s" cannot be used to cast shadows, it has no position ' +
      'and no direction', [FMainLightForShadowsNode.NodeTypeName]);
end;

procedure TVRMLScene.SearchMainLightForShadows(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
var
  L: TVRMLLightNode absolute Node;
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

procedure TVRMLScene.ValidateMainLightForShadows;

  procedure CalculateMainLightForShadows;
  begin
    FMainLightForShadowsExists := false;
    if RootNode <> nil then
    try
      RootNode.Traverse(TVRMLLightNode, @SearchMainLightForShadows);
    except on BreakMainLightForShadows do ; end;
  end;

begin
  if not (fvMainLightForShadows in Validities) then
  begin
    CalculateMainLightForShadows;
    Include(Validities, fvMainLightForShadows);
  end;
end;

function TVRMLScene.MainLightForShadows: TVector4Single;
begin
  ValidateMainLightForShadows;
  Assert(MainLightForShadowsExists, 'MainLightForShadows position is available only when MainLightForShadowsExists');
  Result := FMainLightForShadows;
end;

function TVRMLScene.MainLightForShadowsExists: boolean;
begin
  ValidateMainLightForShadows;
  Result := FMainLightForShadowsExists;
end;

function TVRMLScene.CreateHeadLightInstance
  (HeadLightNode: TNodeKambiHeadLight): TVRMLHeadLight;
begin
  Result := TVRMLHeadLight.Create(HeadLightNode);
end;

function TVRMLScene.CreateHeadLight: TVRMLHeadLight;
var
  HeadLightNode: TNodeKambiHeadLight;
begin
  HeadLightNode := nil;
  if RootNode <> nil then
    HeadLightNode := RootNode.TryFindNode(TNodeKambiHeadLight, true) as
      TNodeKambiHeadLight;
  Result := CreateHeadLightInstance(HeadLightNode);
end;

procedure TVRMLScene.SetHeadlightInitialized(const Value: boolean);
var
  UseHeadlight: boolean;
begin
  if FHeadlightInitialized <> Value then
  begin
    FHeadlightInitialized := Value;
    if Value then
    begin
      if NavigationInfoStack.Top <> nil then
        UseHeadlight := (NavigationInfoStack.Top as TNodeNavigationInfo).FdHeadlight.Value else
        UseHeadlight := DefaultNavigationInfoHeadlight;

      if UseHeadlight then
        FHeadlight := CreateHeadlight else
        FHeadlight := nil;
    end else
    begin
      FreeAndNil(FHeadlight);
    end;
  end;
end;

function TVRMLScene.Headlight: TVRMLHeadlight;
begin
  HeadlightInitialized := true;
  Result := FHeadlight;
end;

procedure TVRMLScene.ViewChangedSuddenly;
begin
  if Log then
    WritelnLog('Scene', 'Optimizer received hint: View changed suddenly');

  { Nothing meaningful to do in this class }
end;

procedure TVRMLScene.CameraChanged(RenderState: TRenderState);
var
  V: TVRMLViewpointNode;
begin
  { Although we register this callback only when ProcessEvents,
    so we could assume here that ProcessEvents is already true...
    But, just in case, check ProcessEvents again (in case in the future
    there will be some queue of events and they could arrive with delay). }

  if ProcessEvents and
     (ViewpointStack.Top <> nil) and
     (ViewpointStack.Top is TVRMLViewpointNode) and
     ( (RenderState.Target = rtScreen) or
       TVRMLViewpointNode(ViewpointStack.Top).FdcameraMatrixSendAlsoOnOffscreenRendering.Value ) then
  begin
    V := TVRMLViewpointNode(ViewpointStack.Top);

    BeginChangesSchedule;
    try
      Inc(FWorldTime.PlusTicks);

      if V.EventCameraMatrix.SendNeeded then
        V.EventCameraMatrix.Send(RenderState.CameraMatrix, WorldTime);

      if V.EventCameraInverseMatrix.SendNeeded then
      begin
        RenderState.CameraInverseMatrixNeeded;
        V.EventCameraInverseMatrix.Send(RenderState.CameraInverseMatrix, WorldTime);
      end;

      if V.EventCameraRotationMatrix.SendNeeded then
        V.EventCameraRotationMatrix.Send(RenderState.CameraRotationMatrix3, WorldTime);

      if V.EventCameraRotationInverseMatrix.SendNeeded then
      begin
        RenderState.CameraRotationInverseMatrixNeeded;
        V.EventCameraRotationInverseMatrix.Send(RenderState.CameraRotationInverseMatrix3, WorldTime);
      end;
    finally EndChangesSchedule end;
  end;
end;

end.
