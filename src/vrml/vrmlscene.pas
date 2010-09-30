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

{ @abstract(VRML scene as @link(TVRMLScene) class.) }

unit VRMLScene;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3D,
  VRMLFields, VRMLNodes, KambiClassUtils, KambiUtils,
  VRMLShape, VRMLTriangleOctree, ProgressUnit, KambiOctree, VRMLShapeOctree,
  KeysMouse, VRMLTime, Cameras, VRMLTriangle, Contnrs, VRMLHeadLight,
  RenderStateUnit, Base3D, VRMLShadowMaps;

{$define read_interface}

const
  DefaultShadowMapsDefaultSize = 256;

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
        of preparing models, e.g. inside PrepareResources.)
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
      TVRMLGLScene.PrepareResources), and after that call FreeResources
      with frRootNode.

      Note that event processing is impossible without RootNode nodes and
      fields and routes, so don't ever use this if you want to set
      TVRMLScene.ProcessEvents to @true.

      Note that if you will try to use a resource that was already freed
      by frRootNode, you may even get segfault (access violation).
      So be really careful, be sure to prepare everything first by
      TVRMLGLScene.PrepareResources or such. }
    frRootNode,

    { Unloads the texture images/videos allocated in VRML texture nodes.

      It's useful if you know that you already prepared everything
      that needed the texture images, and you will not need texture images
      later. For TVRMLGLScene this means that you use Optimization
      method other than roNone,
      and you already did PrepareResources (so textures are already loaded to OpenGL),
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
  TVRMLBindableStack = class(TVRMLBindableStackBasic)
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
    procedure Set_Bind(Node: TNodeX3DBindableNode; const Value: boolean); override;

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
    TextureNode: TNodeX3DTextureNode;
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
      called this, and every TVRMLShape will be (or already is) destroyed
      and created new. }
    gcAll,

    { Transformation of some shape changed.
      @groupBegin }
    gcCollidableTransformChanged,
    gcVisibleTransformChanged,
    { @groupEnd }

    { Local geometry change
      happened (actual octree free is already done by
      TVRMLShape.LocalGeometryChanged, octree create will be done at next demand).
      We should update stuff at higher (TVRMLScene) level accordingly.

      gcLocalGeometryChangedCoord means that coordinates changed.
      Compared to gcLocalGeometryChanged, this means that model edges
      structure remains the same (this is helpful e.g. to avoid
      recalculating Manifold/BorderEdges in parent scene).

      @groupBegin }
    gcLocalGeometryChanged,
    gcLocalGeometryChangedCoord,
    { @groupEnd }

    { What is considered "active" shapes changed. Like after Switch.whichChoice
      change. }
    gcActiveShapesChanged);

  { VRML scene, a final class to handle VRML models
    (with the exception of rendering, which is delegated to descendants,
    like TVRMLGLScene for OpenGL).

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
    The simplest way to do this is to use only TVRMLField.Send to change
    the fields' values. Or you can call TVRMLField.Changed after each change.
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
  TVRMLScene = class(TVRMLEventsEngine)
  private
    FOwnsRootNode: boolean;
    FShapes: TVRMLShapeTree;
    FRootNode: TVRMLNode;
    FOnPointingDeviceSensorsChange: TNotifyEvent;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FFileName: string;
    FInput_PointingDeviceActivate: TInputShortcut;
    FOwnsInput_PointingDeviceActivate: boolean;
    FStatic: boolean;
    FShadowMaps: boolean;
    FShadowMapsPCF: TPercentageCloserFiltering;
    FShadowMapsVisualizeDepth: boolean;
    FShadowMapsDefaultSize: Cardinal;

    { This always holds pointers to all TVRMLShapeTreeLOD instances in Shapes
      tree. }
    ShapeLODs: TObjectList;
    { Recalculate and update LODTree.Level (if viewer position known).
      Also sends level_changed when needed. }
    procedure UpdateLODLevel(LODTree: TVRMLShapeTreeLOD);

    procedure SetFileName(const AValue: string);
    procedure SetInput_PointingDeviceActivate(const Value: TInputShortcut);
    procedure SetStatic(const Value: boolean);
    procedure SetShadowMaps(const Value: boolean);
    procedure SetShadowMapsPCF(const Value: TPercentageCloserFiltering);
    procedure SetShadowMapsVisualizeDepth(const Value: boolean);
    procedure SetShadowMapsDefaultSize(const Value: Cardinal);
  private
    TransformNodesInfo: TDynTransformNodeInfoArray;

    ChangedAll_TraversedLights: TDynActiveLightArray;

    FBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: array [boolean] of Cardinal;
    Validities: TVRMLSceneValidities;
    function CalculateBoundingBox: TBox3D;
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
      out ProjectionType: TProjectionType;
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
    KeyDeviceSensorNodes, TimeSensorNodes, MovieTextureNodes: TVRMLNodesList;
    ProximitySensorInstances: TDynProximitySensorInstanceArray;

    procedure ClearCollectedNodesForEvents;
    { CollectNodesForEvents also calls ClearCollectedNodesForEvents
      at the beginning. But for safety (e.g. if some nodes may no longer
      be valid) you may want to call ClearCollectedNodesForEvents explicitly
      earlier. }
    procedure CollectNodesForEvents;
    procedure TraverseForEvents(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
    procedure CollectNodeForEvents(Node: TVRMLNode);
    procedure UnCollectForEvents(Node: TVRMLNode);

    procedure ScriptsInitialize(Node: TVRMLNode);
    procedure ScriptsDeInitialize(Node: TVRMLNode);
  private
    FTime: TVRMLTime;

    { Internal procedure that handles Time changes. }
    procedure InternalSetTime(
      const NewValue: TVRMLTime; const TimeIncrease: TKamTime; const ResetTime: boolean);

    procedure ResetLastEventTime(Node: TVRMLNode);
  private
    { Bindable nodes helpers }
    FBackgroundStack: TVRMLBindableStack;
    FFogStack: TVRMLBindableStack;
    FNavigationInfoStack: TVRMLBindableStack;
    FViewpointStack: TVRMLViewpointStack;
  private
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

    FPointingDeviceOverItem: PVRMLTriangle;
    FPointingDeviceOverPoint: TVector3Single;
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
      Shape: TObject;
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
    { Value <> 0 means that our state isn't complete (for example,
      we're in the middle of ChangedAll call).

      Some callbacks *may* be called during such time: namely,
      the progress call (e.g. done during constructing octrees).
      As these callbacks may try to e.g. render our scene (which should
      not be done on the dirty state), we have to protect ourselves
      using this variable (e.g. Render routines will exit immediately
      when Dirty <> 0). }
    Dirty: Cardinal;

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
      AState: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo): TVRMLShape; virtual;

    { Create TVRMLHeadLight instance suitable for this TVRMLScene descendant.
      In this class, this simply creates new TVRMLHeadLight instance.
      You can override it in descendants to create something more specialized. }
    function CreateHeadLightInstance
      (HeadLightNode: TNodeKambiHeadLight): TVRMLHeadLight; virtual;
  protected
    GeneratedTextures: TDynGeneratedTextureArray;

    { Called after PointingDeviceSensors list (possibly) changed,
      or when PointingDeviceActiveSensor (possibly) changed.

      In this class, DoPointingDeviceSensorsChange updates Cursor and calls
      OnPointingDeviceSensorsChange. }
    procedure DoPointingDeviceSensorsChange; virtual;

    procedure ExecuteCompiledScript(const HandlerName: string; ReceivedValue: TVRMLField); override;
  public
    constructor Create(AOwner: TComponent); override;

    { Load new 3D model (from VRML node tree).
      This replaces RootNode with new value.

      If AResetTime, we will also do ResetTimeAtLoad,
      changing @link(Time) --- this is usually what you want when you
      really load a new world. }
    procedure Load(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      const AResetTime: boolean = true);

    { Load the 3D model from given AFileName.

      Model is loaded by LoadVRML, so this supports all
      3D model formats that LoadVRML handles
      (VRML, X3D, Wavefront OBJ, 3DS, Collada and more).

      @param(AllowStdIn If AllowStdIn and AFileName = '-' then we will load
        a file from standard input (StdInStream), using GetCurrentDir
        as WWWBasePath (to resolve relative URLs from the file).
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

      This avoids a common pitfall with relying on TVRMLShape or such
      existence between BeforeNodesFree and ChangedAll.
      BeforeNodesFree may free all our TVRMLShape instances, so if you
      want to free TVRMLNode from our graph --- you typically want to
      get this TVRMLNode instance *before* calling BeforeNodesFree.
      Using this method to free the node ensures this. }
    procedure NodeFreeRemovingFromAllParents(Node: TVRMLNode);

    { Remove the geometry of this shape from the scene. }
    procedure RemoveShapeGeometry(Shape: TVRMLShape);

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

      ChangedAll calls BeforeNodesFree(true) first, for safety (and TVRMLGLShape
      actually depends on it, see implementation comments). }
    procedure ChangedAll; override;

    { @italic(Deprecated) way to notify scene that you changed given Field value.

      @italic(Deprecated, use ChangedField instead.)
      Node @italic(must) be equal to Field.ParentNode.
      Field must not be @nil.

      @deprecated }
    procedure ChangedFields(Node: TVRMLNode; Field: TVRMLField);

    { Notify scene that you changed the value of given field.

      This does relatively intelligent discovery what could be possibly
      affected by this field, and updates / invalidates
      internal information where needed.

      Every change you do directly to the VRML/X3D nodes inside RootNode
      (using directly the methods of TVRMLNode or TVRMLField) must be
      reported to scene by calling this method. This includes changes
      to the inactive graph part (e.g. in inactive Switch child),
      because our shapes have to track it also.
      Changes to TVRMLShape.State.LastNodes (these nodes may come
      from StateDefaultNodes) should also be reported here.
      In fact, you can even notify this scene about changes to fields
      that don't belong to our RootNode --- nothing bad will happen.
      We always try to intelligently
      detect what this change implicates for this VRML scene. }
    procedure ChangedField(Field: TVRMLField); override;

    { Notification when geometry changed.
      "Geometry changed" means that the positions
      of triangles changed. This is not send when merely things like
      material changed.

      It is not guaranteed that octrees are already recalculated
      when this is called. (They may be recalculated only on-demand,
      that is when you actually access them.)
      However, it is guaranteed that shape's transformation
      (like TVRMLShape.State.Transform) are already updated. }
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

    { Called when geometry changed.
      Does OnGeometryChanged, and does some other stuff necessary
      (mark some octrees for regenerating at next access).

      This is public only for overloading (and for internal TVRMLShape
      access). Do not call this yourself --- TVRMLShape and TVRMLScene
      implementations know when and how to call this. }
    procedure DoGeometryChanged(const Change: TGeometryChange); virtual;

    { Call OnViewpointsChanged, if assigned. }
    procedure DoViewpointsChanged;

    { Call OnBoundViewpointVectorsChanged, if assigned. }
    procedure DoBoundViewpointVectorsChanged;

    { Mechanism to schedule ChangedAll calls.

      Since these calls may be costly (traversing the hierarchy),
      and their results are often
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
      for BoundingBox (answer will be EmptyBox3D),
      you can render such scene (nothing will be rendered) etc.
      Scene RootNode = nil will act quite like a Scene with
      e.g. no TVRMLGeometryNode nodes.

      Always call ChangedAll when you changed RootNode.

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
      to save some memory. This is actually useful when using TVRMLGLAnimation,
      as it creates a lot of intermediate node structures and TVRMLScene
      instances. }
    property RootNode: TVRMLNode read FRootNode write FRootNode;

    { If @true, RootNode will be freed by destructor of this class. }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode default true;

    { The dynamic octree containing all visible shapes.
      It's useful for "frustum culling", it will be automatically
      used by TVRMLGLScene.RenderFrustum to speed up the rendering.

      This octree will be automatically updated on dynamic scenes
      (when e.g. animation moves some shape by changing it's transformation).

      Add ssRendering to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects.  }
    function OctreeRendering: TVRMLShapeOctree;

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
    function OctreeDynamicCollisions: TVRMLShapeOctree;

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
    function OctreeVisibleTriangles: TVRMLTriangleOctree;

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
    function OctreeCollidableTriangles: TVRMLTriangleOctree;

    { Octree for collisions. This returns either OctreeCollidableTriangles
      or OctreeDynamicCollisions, whichever is available (or @nil if none).
      Be sure to add ssDynamicCollisions or ssCollidableTriangles to have
      this available. }
    function OctreeCollisions: TVRMLBaseTrianglesOctree;

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
      out ProjectionType: TProjectionType;
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
      we set Self as node's EventsEngine to get event
      changes notifications to ChangedXxx methods.
      When ProcessEvents is deactivated (including when this TVRMLScene
      instance is released) we revert this. If you add new node
      to VRML graph, we make sure (in ChangedAll) that EventsEngine
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
      hitPoint_changed, also it updates PointingDeviceOverItem
      and PointingDeviceOverPoint,
      thus producing isOver and such events.

      OverItem may be @nil to indicate we're not over any item.
      In this case, OverPoint is ignored. }
    procedure PointingDeviceMove(
      const RayOrigin, RayDirection: TVector3Single;
      const OverPoint: TVector3Single; const OverItem: PVRMLTriangle);

    { Current item over which the pointing device is. @nil if over none.
      For example, you can investigate it's pointing device sensors
      (in PointingDeviceOverItem^.State.PointingDeviceSensors),
      although there's a shortcut for just this in @link(PointingDeviceSensors).
      You can change this by PointingDeviceMove and PointingDeviceClear. }
    property PointingDeviceOverItem: PVRMLTriangle
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
    function PointingDeviceSensors: TPointingDeviceSensorsList;

    { Currently active sensor. @nil if none.
      Always @nil when PointingDeviceActive = @false.

      Note that sensor specified here doesn't have to be one of the sensors of
      PointingDeviceOverItem. When some sensor is activated, it grabs
      further events until it's deactivated (like you set
      PointingDeviceActive := false, which means that user released mouse
      button). This means that when user moves the mouse while given
      sensor is active, he can move mouse over other items, even the ones
      where the sensor isn't listed --- but the sensor remains active. }
    property PointingDeviceActiveSensor: TNodeX3DPointingDeviceSensorNode
      read FPointingDeviceActiveSensor;

    { Clear any references to OverItem passed previously to PointingDeviceMove.
      This is sometimes useful, because PointingDeviceMove may save
      the last passed OverItem, and SetPointingDeviceActive may even
      save some sensor of it for a longer time.

      You could free it by calling
      @link(PointingDeviceMove) with OverItem = @nil,
      but this could cause other VRML events,
      so it's undesirable to call this when you're going to e.g. release
      the scene or it's octree. Also, you would have to deactivate sensor
      first, causing even more events.

      So this method clears any references to saved OverItem and
      PointingDeviceActiveSensor, without calling any VRML/X3D events.
      Note that this still calls DoPointingDeviceSensorsChange
      (making OnPointingDeviceSensorsChange event), if PointingDeviceActiveSensor /
      PointingDeviceSensors possibly changed. }
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

      To make mouse move actually working (so that VRML pointing-device sensors work Ok),
      make sure you have non-nil OctreeCollisions (e.g. include
      ssDynamicCollisions in @link(Spatial)).

      @groupBegin }
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const RayOrigin, RayDirection: TVector3Single;
      RayHit: T3DCollision): boolean; override;
    { @groupEnd }

    { Input (mouse / key combination) to make pointing device active
      (that is, to activate VRML/X3D pointing-device sensors like TouchSensor).
      By default this requires left mouse button click.

      You can change it to any other mouse button or even to key combination.
      You can simply change properties of existing
      Input_PointingDeviceActivate value (like TInputShortcut.Key1
      or TInputShortcut.MouseButtonUse).

      Or you can even assign here your own TInputShortcut instance.
      Then you're responsible for freeing it yourself.
      This may be comfortable e.g. to share your own TInputShortcut instance
      among many TVRMLScene instances. }
    property Input_PointingDeviceActivate: TInputShortcut
      read FInput_PointingDeviceActivate write SetInput_PointingDeviceActivate;

    procedure Idle(const CompSpeed: Single); override;

    { These change current scene time, setting @link(Time).
      It is crucial that you call this continously to have some VRML
      time-dependent features working, like TimeSensor and MovieTexture.
      See @link(Time) for details what is affected by this.

      This is automatically taken care of if you added this scene
      to TGLUIWindow.Controls or TKamOpenGLControl.Controls.
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

      If a change of Time will produce some visible change in VRML model
      (for example, MovieTexture will change, or TimeSensor change will be
      routed by interpolator to coordinates of some visible node) this
      will be notified by usual method, that is VisibleChangeHere.

      @groupBegin }
    procedure SetTime(const NewValue: TKamTime);
    procedure IncreaseTime(const TimeIncrease: TKamTime);
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
    property Time: TVRMLTime read FTime;
    function GetTime: TVRMLTime; override;

    { Set @link(Time) to arbitrary value.

      You should only use this when you loaded new VRML model.

      Unlike SetTime and IncreaseTime, this doesn't require that
      @link(Time) grows. It still does some time-dependent events work,
      although some time-dependent nodes may be just unconditionally reset
      by this to starting value (as they keep local time, and so require
      TimeIncrease notion, without it we can only reset them).

      @groupBegin }
    procedure ResetTime(const NewValue: TKamTime);
    { @groupEnd }

    { Set @link(Time) to suitable initial value after loading a world.

      This honours VRML specification about VRML time origin,
      and our extension
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_time_origin_at_load]. }
    procedure ResetTimeAtLoad;

    { @deprecated Deprecated name for ResetTime. }
    procedure ResetWorldTime(const NewValue: TKamTime);
    { @deprecated Deprecated name for Time. }
    function WorldTime: TVRMLTime;

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

    function GetViewpointStack: TVRMLBindableStackBasic; override;
    function GetNavigationInfoStack: TVRMLBindableStackBasic; override;
    function GetBackgroundStack: TVRMLBindableStackBasic; override;
    function GetFogStack: TVRMLBindableStackBasic; override;

    { If true, and also KambiLog.Log is true, then we will log
      ChangedField and ChangedAll occurences. Useful only for debugging
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
      It automatically does proper VisibleChangeHere (OnVisibleChangeHere).

      @param(Changes describes changes to the scene caused by viewer change.
        This is needed if some geometry / light follows the player.

        We'll automatically add here "prViewer", so not need to specify it.

        You should add here vcVisibleNonGeometry if player has a headlight.
        You should add here vcVisibleGeometry if player has a rendered
        avatar.) }
    procedure ViewerChanged(ACamera: TCamera;
      const Changes: TVisibleChanges);

    { List of handlers for VRML Script node with "compiled:" protocol.
      This is read-only, change this only by RegisterCompiledScript. }
    property CompiledScriptHandlers: TDynCompiledScriptHandlerInfoArray
      read FCompiledScriptHandlers;

    { Register compiled script handler, for VRML Script node with
      "compiled:" protocol.
      See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_script_compiled]. }
    procedure RegisterCompiledScript(const HandlerName: string;
      Handler: TCompiledScriptHandler);

    { Set TUniversalCamera properties based on currently
      bound NavigationInfo and Viewpoint node.

      Bound NavigationInfo node is taken from
      NavigationInfoStack.Top. If no NavigationInfo is bound, this is @nil,
      and we will create camera corresponding to default NavigationInfo
      values (this is following VRML spec), so it will have
      initial type = EXAMINE.

      You can pass ForceNavigationType = 'EXAMINE', 'WALK', 'FLY', 'NONE' etc.
      (see X3D specification about NavigationInfo node, type field,
      on [http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#NavigationInfo],
      although not all values are handled by our engine now).
      This way we will ignore what NavigationInfo.type information
      inside the scene says.

      This initializes a lot of camera properties:
      @unorderedList(
        @item(TCamera.CameraRadius,)
        @item(TCamera.IgnoreAllInputs,)
        @item(TWalkCamera.Gravity,)
        @item(TWalkCamera.PreferGravityUpForRotations,)
        @item(TWalkCamera.PreferGravityUpForMoving,)
        @item(TWalkCamera.CameraPreferredHeight,)
        @item(TWalkCamera.HeadBobbing, TWalkCamera.HeadBobbingTime.)
      )

      This also calls CameraBindToViewpoint at the end,
      so camera is bound to the current viewpoint.

      Box is the expected bounding box of the whole 3D scene.
      Usually, it should be just Scene.BoundingBox, but it may be something
      larger, if this scene is part of a larger world. }
    procedure CameraBindToNavigationAndViewpoint(Camera: TUniversalCamera;
      const Box: TBox3D;
      const ForceNavigationType: string = '';
      const ForceCameraRadius: Single = 0);

    { Create new camera instance, and bind it to current NavigationInfo
      and Viewpoint. This is only a deprecated shortcut for creating
      TUniversalCamera and then using CameraBindToNavigationAndViewpoint.
      @deprecated }
    function CreateCamera(AOwner: TComponent;
      const Box: TBox3D;
      const ForceNavigationType: string = ''): TUniversalCamera;

    { @deprecated }
    function CreateCamera(AOwner: TComponent;
      const ForceNavigationType: string = ''): TUniversalCamera;

    { Update camera when currently bound viewpoint changes.
      When no viewpoint is currently bound, we will go to a suitable
      viewpoint to see the whole scene (based on a scene bounding box).

      This moves the camera to starting point of the viewpoint,
      updating camera's initial and current vectors
      ([Initial]Position, [Initial]Direction, [Initial]Up, GravityUp).

      If not OnlyViewpointVectorsChanged, then we also adjust MoveSpeed
      to match currently bound NavigationInfo.speed. }
    procedure CameraBindToViewpoint(ACamera: TCamera;
      const OnlyViewpointVectorsChanged: boolean);

    { Detect position/direction of the main light that produces shadows.
      This is useful when you want to make shadows on the scene
      from only a single light, but your scene has many lights.

      The main light is simply one with both @code(kambiShadows) and
      @code(kambiShadowsMain) fields set to @true. See
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows]
      for more info.
      If no light with kambiShadows = kambiShadowsMain = TRUE
      is present then this function returns @false,
      since AMainLightPosition cannot be calculated.

      AMainLightPosition[3] is always set to 1
      (positional light) or 0 (indicates that this is a directional light).

      @seealso TVRMLLightSet.MainLightForShadows
      @seealso TKamAbstractViewport.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4Single): boolean;

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

    procedure PrepareResources(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareResourcesOptions;
      ProgressStep: boolean); override;

    procedure GetHeightAbove(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: P3DTriangle); override;
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): T3DCollision; override;
    function Dragging: boolean; override;

    { Static scene will not be automatically notified about the changes
      to the field values. This means that TVRMLField.Send and
      TVRMLField.Changed will not notify this scene. This makes a
      small optimization when you know you will not modify scene's VRML graph
      besides loading (or you're prepared to do it by manually calling
      Scene.ChangedField etc.).

      Note that when the ProcessEvents is @true, the scene will be
      notified about changes to it's nodes anyway, regardless of
      @name value. }
    property Static: boolean read FStatic write SetStatic default false;
  published
    { When TimePlaying is @true, the time of our 3D world will keep playing.
      More precisely, our @link(Idle) will take care of increasing @link(Time).
      Our @link(Idle) is usually automatically called (if you added this
      scene to TGLUIWindow.Controls or TKamOpenGLControl.Controls)
      so you don't have to do anything to make this work. }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;

    { Controls the time speed (if TimePlaying is @true):
      1.0 means that 1 second  of real time equals to 1 unit of world time. }
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;

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

    { Should the VRML event mechanism work.

      If @true, then we will implement whole VRML event mechanism here,
      as expected from a VRML browser. Events will be send and received
      through routes, time dependent nodes (X3DTimeDependentNode,
      like TimeSensor) will be activated and updated from @link(Time) time
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
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_maps].
      When using these lower-level nodes, this property does not matter
      This property (and related ones
      like ShadowMapsPCF, ShadowMapsVisualizeDepth, ShadowMapsDefaultSize)
      is relevant only for handling shadows by the "receiveShadows" field. }
    property ShadowMaps: boolean read FShadowMaps write SetShadowMaps default true;

    { Use Percentage Closer Filtering to improve shadow maps look.

      Affects how shadow maps are handled for the "receiveShadows"
      field.  This is taken into account at the scene @link(Load) time,
      and only if @link(ShadowMaps) is @true. }
    property ShadowMapsPCF: TPercentageCloserFiltering
      read FShadowMapsPCF write SetShadowMapsPCF default pcf16;

    { Visualize depths stored in the shadow maps, instead of using them to
      actually make shadow.

      Affects how shadow maps are handled for the "receiveShadows"
      field.  This is taken into account at the scene @link(Load) time,
      and only if @link(ShadowMaps) is @true.

      Even without turning this on, VRML author can always activate it
      explicitly for specific lights. For this, you have to use
      @code(X3DLightNode.defaultShadowMap) field,
      and place a GeneratedShadowMap node there. If the
      @code(GeneratedShadowMap.compareMode) is set to @code('NONE'),
      we will always visualize depths of this shadow map. }
    property ShadowMapsVisualizeDepth: boolean
     read FShadowMapsVisualizeDepth write SetShadowMapsVisualizeDepth default false;

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
  end;

{$undef read_interface}

implementation

uses VRMLCameraUtils, KambiStringUtils, KambiLog, VRMLErrors, DateUtils,
  Object3DAsVRML;

{$define read_implementation}
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
    Node.EventIsBound.Send(Value, ParentScene.Time);
    Node.EventBindTime.Send(ParentScene.Time.Seconds, ParentScene.Time);
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

  FRootNode := nil;
  FOwnsRootNode := true;

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

  FInput_PointingDeviceActivate := TInputShortcut.Create(K_None, K_None, #0, true, mbLeft);
  FOwnsInput_PointingDeviceActivate := true;

  FShadowMaps := true;
  FShadowMapsPCF := pcf16;
  FShadowMapsVisualizeDepth := false;
  FShadowMapsDefaultSize := DefaultShadowMapsDefaultSize;

  { We could call here ScheduleChangedAll (or directly ChangedAll),
    but there should be no need. FRootNode remains nil,
    and our current state should be equal to what ScheduleChangedAll
    would set. This is (potentially) a small time saving,
    as ScheduleChangedAll does a lot of calls (although probably is fast
    anyway when RootNode = nil). }

  ChangeListeners.Add(Self); { by default FStatic is false }
end;

destructor TVRMLScene.Destroy;
begin
  if ChangeListeners <> nil then
    ChangeListeners.Remove(Self);

  { This also frees related lists, like KeyDeviceSensorNodes,
    and does UnregisterProcessEvents(RootNode). }
  ProcessEvents := false;

  HeadlightInitialized := false;

  { free FTrianglesList* variables }
  InvalidateTrianglesList(false);
  InvalidateTrianglesList(true);
  InvalidateTrianglesListShadowCasters;

  { frees FManifoldEdges, FBorderEdges if needed }
  InvalidateManifoldAndBorderEdges;

  if FOwnsInput_PointingDeviceActivate then
    FreeAndNil(FInput_PointingDeviceActivate) else
    FInput_PointingDeviceActivate := nil;

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

procedure TVRMLScene.Load(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  const AResetTime: boolean);
begin
  BeforeNodesFree;
  PointingDeviceClear;
  if OwnsRootNode then FreeAndNil(FRootNode);

  RootNode := ARootNode;
  OwnsRootNode := AOwnsRootNode;
  ScheduledShadowMapsProcessing := true;

  ScheduleChangedAll;

  if AResetTime then
    ResetTimeAtLoad;
end;

procedure TVRMLScene.Load(const AFileName: string; AllowStdIn: boolean;
  const AResetTime: boolean);
begin
  { Note that if LoadVRML fails, we will not change the RootNode,
    so currently loaded scene will remain valid. }

  Load(LoadVRML(AFileName, AllowStdIn), true, AResetTime);

  FFileName := AFileName;
end;

procedure TVRMLScene.SetFileName(const AValue: string);
begin
  if AValue <> FFileName then
    Load(AValue);
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

function TVRMLScene.CalculateBoundingBox: TBox3D;
var
  SI: TVRMLShapeTreeIterator;
begin
  Result := EmptyBox3D;
  SI := TVRMLShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Box3DSumTo1st(Result, SI.Current.BoundingBox);
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

function TVRMLScene.BoundingBox: TBox3D;
begin
  if Exists then
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

function TVRMLScene.VerticesCount(OverTriangulate: boolean): Cardinal;
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

function TVRMLScene.TrianglesCount(OverTriangulate: boolean): Cardinal;
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

function TVRMLScene.CreateShape(AGeometry: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo): TVRMLShape;
begin
  Result := TVRMLShape.Create(Self, AGeometry, AState, ParentInfo);
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
      TVRMLGraphTraverseState.CreateCopy(StateStack.Top), ParentInfo);
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

      DoGeometryChanged(gcActiveShapesChanged);
    end;
  end;
end;

procedure TVRMLScene.BeforeNodesFree(const InternalChangedAll: boolean);
begin
  { TransformNodesInfo will be recalculated by ChangedAll }
  TransformNodesInfo.Count := 0;

  { GeneratedTextures will be recalculated by ChangedAll }
  GeneratedTextures.Count := 0;

  { clear nodes before doing CheckForDeletedNodes below, as CheckForDeletedNodes
    may send some events so no invalid pointers (e.g. on ProximitySensorInstance
    list) must exist. }
  if ProcessEvents then
    ClearCollectedNodesForEvents;

  if not InternalChangedAll then
  begin
    { Clean Shapes, ShapeLODs }
    FreeAndNil(FShapes);
    FShapes := TVRMLShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;
  end;
end;

procedure TVRMLScene.NodeFreeRemovingFromAllParents(Node: TVRMLNode);
begin
  BeforeNodesFree;
  Node.FreeRemovingFromAllParents;
  ChangedAll;
end;

procedure TVRMLScene.RemoveShapeGeometry(Shape: TVRMLShape);
begin
  { Do not use Shape.Geometry here, as it may be a temporary result
    of OriginalGeometry.Proxy.

    When the shape is freed (which happens in BeforeNodesFree called
    at the beginning of  NodeFreeRemovingFromAllParents),
    the proxy result is freed too. So using here Shape.Geometry
    would not only not free what you think, it would also cause segfault. }

  NodeFreeRemovingFromAllParents(Shape.OriginalGeometry);
end;

procedure TVRMLScene.ChangedAll;

  { VRML2ActiveLights are magically updated for all states in
    @link(Shapes) tree. This is crucial for lights rendering in VRML >= 2.0. }
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
          if Box3DSphereCollision(SI.Current.BoundingBox, Location, Radius) then
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
  DoGeometryChanged(gcAll);

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

  { Call DoGeometryChanged here, as our new shapes are added.
    Probably, only one DoGeometryChanged(gcAll) is needed, but for safety
    --- we can call it twice, it's ultra-fast right now. }
  DoGeometryChanged(gcAll);

  VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  DoViewpointsChanged;

  if ScheduledShadowMapsProcessing then
  begin
    ProcessShadowMapsReceivers(RootNode, Shapes, ShadowMaps,
      ShadowMapsDefaultSize, ShadowMapsVisualizeDepth, ShadowMapsPCF);
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
    ChangingField: TVRMLField;
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
    Changes: TVRMLChanges;
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
      { Changes = [chTransform] here, good for roSeparateShapesNoTransform
        optimization. }
      Shape.Changed(ChangingField, Inactive <> 0, Changes);

      if Inactive = 0 then
      begin
        if Shape.Visible then
          ParentScene.DoGeometryChanged(gcVisibleTransformChanged);

        if Shape.Collidable then
          ParentScene.DoGeometryChanged(gcCollidableTransformChanged);

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
        ParentScene.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
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

procedure TVRMLScene.ChangedFields(Node: TVRMLNode; Field: TVRMLField);
begin
  Assert(Field <> nil);
  Assert(Field.ParentNode = Node);
  ChangedField(Field);
end;

procedure TVRMLScene.ChangedField(Field: TVRMLField);
var
  Node: TVRMLNode;
  Changes: TVRMLChanges;

  procedure DoLogChanges(const Additional: string = '');
  var
    S: string;
  begin
    S := 'ChangedField: ' + VRMLChangesToStr(Changes) +
      Format(', node: %s (%s %s) at %s',
      [ Node.NodeName, Node.NodeTypeName, Node.ClassName, PointerToStr(Node) ]);
    if Field <> nil then
      S += Format(', field %s (%s)', [ Field.Name, Field.VRMLTypeName ]);
    if Additional <> '' then
      S += '. ' + Additional;
    WritelnLog('VRML changes', S);
  end;

  { Handle VRML >= 2.0 transformation changes. }
  procedure HandleChangeTransform;
  var
    TransformChangeHelper: TTransformChangeHelper;
    NodeInfo: PTransformNodeInfo;
    TransformShapesParentInfo: TShapesParentInfo;
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

      Besides, while traversing to the current transform node, we have to count
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
        TransformChangeHelper.ChangingField := Field;
        TransformChangeHelper.AnythingChanged := false;
        TransformChangeHelper.Inside := false;
        TransformChangeHelper.Inactive := 0;
        TransformChangeHelper.Changes := Changes;
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
          { No need to even VisibleChangeHere at the end. }
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

    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeCoordinate;
  var
    Coord: TMFVec3f;
    SI: TVRMLShapeTreeIterator;
  begin
    { TNodeCoordinate is special, although it's part of VRML 1.0 state,
      it can also occur within coordinate-based nodes of VRML >= 2.0.
      So it affects coordinate-based nodes with this node.

      In fact, code below takes into account both VRML 1.0 and 2.0 situation.
      That's why chCoordinate should not be used with chVisibleVRML1State
      --- chVisibleVRML1State handling is not needed after this. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.Geometry.Coord(SI.Current.State, Coord) and
           (Coord = Field) then
          SI.Current.Changed(Field, false, Changes);

      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    finally FreeAndNil(SI) end;
  end;

  { Good for both chVisibleVRML1State and chGeometryVRML1State
    (TVRMLShape.Changed actually cares about the difference between these two.) }
  procedure HandleVRML1State;
  var
    VRML1StateNode: TVRML1StateNode;
    SI: TVRMLShapeTreeIterator;
  begin
    if Node.VRML1StateNode(VRML1StateNode) then
    begin
      { Node is part of VRML 1.0 state, so it affects Shapes where
        it's present on State.LastNodes list. }
      SI := TVRMLShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if (SI.Current.State.LastNodes.Nodes[VRML1StateNode] = Node) or
             (SI.Current.OriginalState.LastNodes.Nodes[VRML1StateNode] = Node) then
            SI.Current.Changed(Field, false, Changes);
      finally FreeAndNil(SI) end;
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeMaterial;
  var
    SI: TVRMLShapeTreeIterator;
  begin
    { VRML 2.0 Material affects only shapes where it's
      placed inside Appearance.material field. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.State.ShapeNode.Material = Node then
          SI.Current.Changed(Field, false, Changes);
    finally FreeAndNil(SI) end;
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeLightActiveProperty;
  var
    J: integer;
    SI: TVRMLShapeTreeIterator;
    ActiveLight: PActiveLight;
    LightNode: TVRMLLightNode;
  begin
    LightNode := Node as TVRMLLightNode;

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
          begin
            LightNode.UpdateActiveLight(ActiveLight^);
            VisibleChangeHere([vcVisibleNonGeometry]);
          end;
        end;
    finally FreeAndNil(SI) end;
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
    { If we had calculated MainLightForShadows, and this Node is the
      main light for shadows, then update FMainLightForShadows.
      Thanks to varius FMainLightForShadows* properties, we can check
      and recalculate it very fast --- this is good for scenes where main
      shadow light location is moving. }

    if (fvMainLightForShadows in Validities) and
       FMainLightForShadowsExists and
       (FMainLightForShadowsNode = Node) then
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

    DoGeometryChanged(gcActiveShapesChanged);

    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  procedure HandleChangeColorNode;
  var
    SI: TVRMLShapeTreeIterator;
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
          SI.Current.Changed(Field, false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeTextureCoordinate;
  var
    SI: TVRMLShapeTreeIterator;
    TexCoord: TVRMLNode;
  begin
    { VRML 2.0 TextureCoordinate affects only shapes where it's
      placed inside texCoord field. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if SI.Current.Geometry.TexCoord(SI.Current.State, TexCoord) and
           (TexCoord = Node) then
          SI.Current.Changed(Field, false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeTextureTransform;

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
    SI: TVRMLShapeTreeIterator;
  begin
    { VRML 2.0 / X3D TextureTransform* affects only shapes where it's
      placed inside textureTransform field. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if (SI.Current.State.ShapeNode <> nil) and
           (SI.Current.State.ShapeNode.FdAppearance.Value <> nil) and
           (SI.Current.State.ShapeNode.FdAppearance.Value is TNodeAppearance) and
           AppearanceUsesTextureTransform(
             TNodeAppearance(SI.Current.State.ShapeNode.FdAppearance.Value), Node) then
          SI.Current.Changed(Field, false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeGeometry;
  var
    SI: TVRMLShapeTreeIterator;
  begin
    { Geometry nodes, affect only shapes that use them. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        if (SI.Current.Geometry = Node) or
           (SI.Current.OriginalGeometry = Node) then
          SI.Current.Changed(Field, false, Changes);
    finally FreeAndNil(SI) end;
  end;

  procedure HandleChangeEnvironmentalSensorBounds;
  var
   I: Integer;
  begin
    { For now, we're only interested here in ProximitySensor changes.
      In the future, we may need to do something for other
      X3DEnvironmentalSensorNode too. }
    if not (Node is TNodeProximitySensor) then Exit;

    { Update state for this ProximitySensor node. }
    if IsLastViewer then
      for I := 0 to ProximitySensorInstances.Count - 1 do
      begin
        if ProximitySensorInstances.Items[I].Node = Node then
          ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
      end;
  end;

  procedure HandleChangeTimeStopStart;
  var
    DummySomethingChanged: boolean;
    Handler: TTimeDependentNodeHandler;
  begin
    if Field.ParentNode is TNodeMovieTexture then
      Handler := TNodeMovieTexture(Field.ParentNode).TimeDependentNodeHandler else
    if Field.ParentNode is TNodeTimeSensor then
      Handler := TNodeTimeSensor(Field.ParentNode).TimeDependentNodeHandler else
      { Node not really time-dependent.
        TODO: probably an interface method INodeX3DTimeDependentNode
        to get handler would be a good idea. }
      Exit;

    { Although (de)activation of time-dependent nodes will be also catched
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

    Handler.SetTime(Time, Time, 0, false, DummySomethingChanged);

    { DummySomethingChanged is simply ignored here (we do not have to report
      changes anywhere). }

    { No need to do VisibleChangeHere.
      Redisplay will be done by next IncreaseTime run, if active now. }
  end;

  procedure HandleChangeViewpointVectors;
  begin
    if Field.ParentNode = ViewpointStack.Top then
      DoBoundViewpointVectorsChanged;
      { Nothing needs to be done if
        - non-bound viewpoint changed,
        - or a field of bound viewpoint that doesn't affect it's vectors. }
  end;

  { Handle chTextureImage, chTextureRendererProperties }
  procedure HandleChangeTextureImageOrRenderer;
  var
    SI: TVRMLShapeTreeIterator;
  begin
    if chTextureImage in Changes then
    begin
      { On change of TVRML2DTextureNode field that changes the result of
        TVRML2DTextureNode.LoadTextureData, we have to explicitly release
        old texture (otherwise, LoadTextureData will not be called
        to reload the texture). }
      if Node is TVRML2DTextureNode then
        TVRML2DTextureNode(Node).IsTextureLoaded := false;
      if Node is TNodeX3DTexture3DNode then
        TNodeX3DTexture3DNode(Node).TextureLoaded := false;
    end;

    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if SI.Current.UsesTexture(TNodeX3DTextureNode(Node)) then
          SI.Current.Changed(Field, false, Changes);
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
    if Node is TNodeGeneratedCubeMapTexture then
      Handler := TNodeGeneratedCubeMapTexture(Node).GeneratedTextureHandler else
    if Node is TNodeGeneratedShadowMap then
      Handler := TNodeGeneratedShadowMap(Node).GeneratedTextureHandler else
    if Node is TNodeRenderedTexture then
      Handler := TNodeRenderedTexture(Node).GeneratedTextureHandler else
      Exit;

    Handler.UpdateNeeded := true;
    VisibleChangeHere([]);
  end;

  procedure HandleChangeHeadLight;
  begin
    { It will be automatically reinitalized when needed (on next display) }
    HeadlightInitialized := false;
    VisibleChangeHere([]);
  end;

  procedure HandleChangeClipPlane;
  var
    SI: TVRMLShapeTreeIterator;
  begin
    Assert(Node is TNodeClipPlane);

    { Call TVRMLShape.Changed for all shapes using this ClipPlane node. }
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if SI.Current.State.ClipPlanes.IndexOfNode(TNodeClipPlane(Node)) <> -1 then
          SI.Current.Changed(Field, false, Changes);
      end;
    finally FreeAndNil(SI) end;
    VisibleChangeHere([vcVisibleGeometry]);
  end;

  procedure HandleChangeEverything;
  begin
    { An arbitrary change occured. }
    ScheduleChangedAll;
  end;

  { Handle all four flags chVisibleGeometry, chVisibleNonGeometry,
    chViewer, chRedisplay. }
  procedure HandleVisibleChange;
  var
    VisibleChanges: TVisibleChanges;
  begin
    VisibleChanges := [];
    if chVisibleGeometry    in Changes then Include(VisibleChanges, vcVisibleGeometry);
    if chVisibleNonGeometry in Changes then Include(VisibleChanges, vcVisibleNonGeometry);
    if chViewer             in Changes then Include(VisibleChanges, vcViewer);
    VisibleChangeHere(VisibleChanges);
  end;

  procedure HandleChangeDragSensorEnabled;
  var
    Enabled: boolean;
    DragSensor: TNodeX3DDragSensorNode;
  begin
    Enabled := (Field as TSFBool).Value;
    DragSensor := Node as TNodeX3DDragSensorNode;

    { When we disable an active drag sensor, specification says to
      deactivate it. This cannot be handled fully by TNodeX3DDragSensorNode
      implementation, because we have to set to nil our property
      PointingDeviceActiveSensor. }

    if (not Enabled) and (DragSensor = PointingDeviceActiveSensor) then
    begin
      DragSensor.Deactivate(Time);
      FPointingDeviceActiveSensor := nil;
      DoPointingDeviceSensorsChange;
    end;
  end;

begin
  Node := TVRMLNode(Field.ParentNode);
  Assert(Node <> nil);

  { We used to check here RootNode.IsNodePresent, to eliminate
    changes to nodes not in our graph. This is not done now, because:

    1. This check is not usually needed, and usually it wastes quite
       some time (for example, profile
       ../vrml/opengl/examples/change_vrml_by_code_2.lpr
       when doing ChangedField (not ChangedAll)).

       In most cases, when modifying graph by code, and always when
       modifying graph by VRML events, the Node is known to be inside
       our VRML graph...

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
    if chLightActiveProperty    in Changes then HandleChangeLightActiveProperty;
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
    if chHeadLight in Changes then HandleChangeHeadLight;
    if chClipPlane in Changes then HandleChangeClipPlane;
    if chDragSensorEnabled in Changes then HandleChangeDragSensorEnabled;
    if chEverything in Changes then HandleChangeEverything;

    if Changes * [chVisibleGeometry, chVisibleNonGeometry,
      chViewer, chRedisplay] <> [] then
      HandleVisibleChange;
  finally EndChangesSchedule end;
end;

procedure TVRMLScene.DoGeometryChanged(const Change: TGeometryChange);
var
  SomeLocalGeometryChanged: boolean;
  EdgesStructureChanged: boolean;
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
    OnGeometryChanged(Self, SomeLocalGeometryChanged);
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
  BBox: TBox3D;
begin
  BBox := BoundingBox;
  Result := 'Bounding box : ' + Box3DToNiceStr(BBox);
  if not IsEmptyBox3D(BBox) then
  begin
    Result += ', average size : ' + FloatToNiceStr(Box3DAvgSize(BBox));
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
  Shape: TObject;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, Shape, MatNum,
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

function TVRMLScene.OctreeRendering: TVRMLShapeOctree;
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

function TVRMLScene.OctreeDynamicCollisions: TVRMLShapeOctree;
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

function TVRMLScene.OctreeVisibleTriangles: TVRMLTriangleOctree;
begin
  if (ssVisibleTriangles in Spatial) and (FOctreeVisibleTriangles = nil) then
    FOctreeVisibleTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opVisibleTriangles),
      TriangleOctreeProgressTitle,
      false);
  Result := FOctreeVisibleTriangles;
end;

function TVRMLScene.OctreeCollidableTriangles: TVRMLTriangleOctree;
begin
  if (ssCollidableTriangles in Spatial) and (FOctreeCollidableTriangles = nil) then
    FOctreeCollidableTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opCollidableTriangles),
      TriangleOctreeProgressTitle,
      true);
  Result := FOctreeCollidableTriangles;
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
          SI.Current.Triangulate(false, AddTriProc);
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
         (V.ProjectionType = ptPerspective) ) and
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
  out ProjectionType: TProjectionType;
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
    ProjectionType := Result.ProjectionType;
  end else
  begin
    { use default camera settings }
    CamPos := DefaultVRMLCameraPosition[1];
    CamDir := DefaultVRMLCameraDirection;
    CamUp := DefaultVRMLCameraUp;
    GravityUp := DefaultVRMLGravityUp;
    ProjectionType := ptPerspective;
  end;
end;

function TVRMLScene.GetViewpoint(
  out ProjectionType: TProjectionType;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
begin
  Result := GetViewpointCore(false, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
end;

function TVRMLScene.GetPerspectiveViewpoint(
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
var
  ProjectionType: TProjectionType;
begin
  Result := GetViewpointCore(true, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
  Assert(ProjectionType = ptPerspective);
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
      Shape: TObject;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangleAdder.AddTriangle(const Triangle: TTriangle3Single;
    Shape: TObject;
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
            SI.Current.Triangulate(OverTriangulate, @TriangleAdder.AddTriangle);
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
      Shape := AShape.State.ShapeNode;
      Result := not (
        (Shape <> nil) and
        (Shape.FdAppearance.Value <> nil) and
        (Shape.FdAppearance.Value is TNodeAppearance) and
        (not TNodeAppearance(Shape.FdAppearance.Value).FdShadowCaster.Value));
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
                  SI.Current.Triangulate(false, @TriangleAdder.AddTriangle) else
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
                  SI.Current.Triangulate(false, @TriangleAdder.AddTriangle);
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
  (Node as TVRML2DTextureNode).IsTextureLoaded := false;
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
    RootNode.EnumerateNodes(TVRML2DTextureNode,
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
    during single SetTime, so their local time would grow too fast.

  - for other sensors, events would be passed twice.
}

procedure TVRMLScene.CollectNodeForEvents(Node: TVRMLNode);
begin
  Node.EventsEngine := Self;

  if Node is TNodeX3DKeyDeviceSensorNode then
    KeyDeviceSensorNodes.AddIfNotExists(Node) else
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

procedure TVRMLScene.ClearCollectedNodesForEvents;
begin
  KeyDeviceSensorNodes.Clear;
  TimeSensorNodes.Clear;
  MovieTextureNodes.Clear;
  ProximitySensorInstances.Count := 0;
end;

procedure TVRMLScene.CollectNodesForEvents;
begin
  ClearCollectedNodesForEvents;

  if RootNode <> nil then
  begin
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
        for all and set their EventsEngine. Reason: scripts
        initialize() methods may already cause some events, that should
        notify us appropriately.

        This is also why Begin/EndChangesSchedule around is useful. }
      RootNode.EnumerateNodes(TNodeScript, @ScriptsInitialize, false);
    finally EndChangesSchedule end;
  end;
end;

procedure TVRMLScene.UnCollectForEvents(Node: TVRMLNode);
begin
  Node.EventsEngine := nil;
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
    Inc(FTime.PlusTicks);
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
      KeyDeviceSensorNodes := TVRMLNodesList.Create;
      TimeSensorNodes := TVRMLNodesList.Create;
      MovieTextureNodes := TVRMLNodesList.Create;
      ProximitySensorInstances := TDynProximitySensorInstanceArray.Create;

      FProcessEvents := Value;

      CollectNodesForEvents;
      InitialProximitySensorsEvents;

      RenderState.OnCameraChanged.Add(@CameraChanged);
    end else
    begin
      if RootNode <> nil then UnregisterProcessEvents(RootNode);
      FreeAndNil(KeyDeviceSensorNodes);
      FreeAndNil(TimeSensorNodes);
      FreeAndNil(MovieTextureNodes);
      FreeAndNil(ProximitySensorInstances);
      PointingDeviceClear;

      FProcessEvents := Value;

      { ProcessEvents := false may get called from destructor,
        after RenderStateUnit finalization }
      if RenderState <> nil then
        RenderState.OnCameraChanged.Remove(@CameraChanged);
    end;
  end;
end;

procedure TVRMLScene.SetStatic(const Value: boolean);
begin
  if FStatic <> Value then
  begin
    FStatic := Value;
    if Value then
      ChangeListeners.Remove(Self) else
      ChangeListeners.Add(Self);
  end;
end;

{ key sensors handling ------------------------------------------------------- }

function TVRMLScene.KeyDown(Key: TKey; C: char): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TNodeX3DKeyDeviceSensorNode).KeyDown(Key, C, FTime);
    finally EndChangesSchedule; end;

    { Do not treat it as handled (returning ExclusiveEvents),
      even if some X3DKeyDeviceSensorNode was found and did something.
      This would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;

  if Input_PointingDeviceActivate.IsKey(Key, C) then
    PointingDeviceActive := true;
end;

function TVRMLScene.KeyUp(Key: TKey; C: char): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TNodeX3DKeyDeviceSensorNode).KeyUp(Key, C, FTime);
    finally EndChangesSchedule; end;

    { Do not treat it as handled (returning ExclusiveEvents),
      even if some X3DKeyDeviceSensorNode was found and did something.
      This would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;

  if Input_PointingDeviceActivate.IsKey(Key, C) then
    PointingDeviceActive := false;
end;

{ pointing device handling --------------------------------------------------- }

procedure TVRMLScene.PointingDeviceMove(
  const RayOrigin, RayDirection: TVector3Single;
  const OverPoint: TVector3Single; const OverItem: PVRMLTriangle);
var
  TouchSensor: TNodeTouchSensor;
  OldIsOver, NewIsOver: boolean;
  OldSensors: TVRMLNodesList;
  NewSensors: TVRMLNodesList;
  I: Integer;
begin
  if ProcessEvents then
  begin
    Inc(FTime.PlusTicks);
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
              PointingDeviceActiveSensor.EventIsOver.Send(NewIsOver, Time);
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
                TNodeX3DPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, Time);
            end;

          for I := 0 to NewSensors.Count - 1 do
            if OldSensors.IndexOf(NewSensors[I]) = -1 then
            begin
              if (NewSensors[I] is TNodeX3DPointingDeviceSensorNode) and
                TNodeX3DPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
                TNodeX3DPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, Time);
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
              TNodeX3DPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, Time);
        end else
        begin
          Assert(OverItem <> nil);

          { So we previously pointed as nothing, and now at something.
            So simply call isOver = TRUE for all. }

          NewSensors := OverItem^.State.PointingDeviceSensors;

          for I := 0 to NewSensors.Count - 1 do
            if (NewSensors[I] is TNodeX3DPointingDeviceSensorNode) and
              TNodeX3DPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
              TNodeX3DPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, Time);
        end;

        FPointingDeviceOverItem := OverItem;

        DoPointingDeviceSensorsChange;
      end;

      { When OverItem <> nil => OverPoint is meaningful.
        Right now OverItem = FPointingDeviceOverItem,
        so take care to make PointingDeviceOverPoint also meaningful. }
      FPointingDeviceOverPoint := OverPoint;

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
                MatrixMultPoint(OverItem^.State.InvertedTransform, OverPoint), Time);

              { The best normal I can generate for now is flat normal
                for the hit triangle. }
              TouchSensor.EventHitNormal_Changed.Send(
                OverItem^.World.Normal, Time);

              { TODO: hitTexCoord_changed generation should also be done
                here, but honestly I just cannot do this with current
                information. Triangulation must be much improved to
                provide this. }
            end;
          end;
      end;

      if (PointingDeviceActiveSensor <> nil) and
         (PointingDeviceActiveSensor is TNodeX3DDragSensorNode) then
        TNodeX3DDragSensorNode(PointingDeviceActiveSensor).Drag(
          Time, RayOrigin, RayDirection);
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TVRMLScene.DoPointingDeviceSensorsChange;
begin
  { I want to keep assertion that Cursor = mcHand when
    we're over or keeping active some pointing-device sensors. }
  if ((PointingDeviceSensors <> nil) and
      (PointingDeviceSensors.EnabledCount <> 0)) or
     (PointingDeviceActiveSensor <> nil) then
    Cursor := mcHand else
    Cursor := mcDefault;

  if Assigned(OnPointingDeviceSensorsChange) then
    OnPointingDeviceSensorsChange(Self);
end;

procedure TVRMLScene.PointingDeviceClear;
var
  SensorsChanged: boolean;
begin
  SensorsChanged :=
    (FPointingDeviceOverItem <> nil) or
    (FPointingDeviceActiveSensor <> nil);

  FPointingDeviceOverItem := nil;
  { PointingDeviceOverPoint may be left undefined now, but let's set it
    to something deterministic to ease debugging. }
  FPointingDeviceOverPoint := ZeroVector3Single;
  FPointingDeviceActive := false;
  FPointingDeviceActiveSensor := nil;

  if SensorsChanged then
    DoPointingDeviceSensorsChange;
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
        Load(NewRootNode, true, { do not reset Time } false);
      if NewViewpoint <> nil then
        NewViewpoint.EventSet_Bind.Send(true, Time);
    end;
  end;

var
  I: Integer;
  ToActivate: TVRMLNode;
  Sensors: TPointingDeviceSensorsList;
begin
  if ProcessEvents and (FPointingDeviceActive <> Value) then
  begin
    Inc(FTime.PlusTicks);
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
                { We do this only when PointingDeviceOverItem <> nil,
                  so we know that PointingDeviceOverPoint is meaningful. }
                PointingDeviceActiveSensor.Activate(Time,
                  Sensors.Transform, Sensors.InvertedTransform, PointingDeviceOverPoint);
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
          PointingDeviceActiveSensor.Deactivate(Time);
          { If we're still over the sensor, generate touchTime for TouchSensor }
          if (PointingDeviceOverItem <> nil) and
             (PointingDeviceOverItem^.State.PointingDeviceSensors.
               IndexOf(PointingDeviceActiveSensor) <> -1) and
             (PointingDeviceActiveSensor is TNodeTouchSensor) then
          begin
            TNodeTouchSensor(PointingDeviceActiveSensor).
              EventTouchTime.Send(Time.Seconds, Time);
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

procedure TVRMLScene.SetInput_PointingDeviceActivate(const Value: TInputShortcut);
begin
  if FInput_PointingDeviceActivate <> Value then
  begin
    { first, free the old one }
    if FOwnsInput_PointingDeviceActivate then
      FreeAndNil(FInput_PointingDeviceActivate);

    FInput_PointingDeviceActivate := Value;
    FOwnsInput_PointingDeviceActivate := false;
  end;
end;

function TVRMLScene.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Input_PointingDeviceActivate.IsMouseButton(Button) then
  begin
    PointingDeviceActive := true;
    { Do not treat it as handled (returning ExclusiveEvents),
      this would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;
end;

function TVRMLScene.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Input_PointingDeviceActivate.IsMouseButton(Button) then
  begin
    PointingDeviceActive := false;
    { Do not treat it as handled (returning ExclusiveEvents),
      this would disable too much (like Camera usually under Scene on Controls).
    Result := false; }
  end;
end;

function TVRMLScene.MouseMove(const RayOrigin, RayDirection: TVector3Single;
  RayHit: T3DCollision): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (RayHit = nil) or (not RayHit.Hierarchy.IsLast(Self)) then
    { If ray hit outside this scene (other 3D object, or empty space)
      then mouse is no longer over any part of *this* scene. }
    PointingDeviceMove(RayOrigin, RayDirection, ZeroVector3Single, nil) else
    PointingDeviceMove(RayOrigin, RayDirection, RayHit.Point, PVRMLTriangle(RayHit.Triangle));

  { Do not treat it as handled (returning ExclusiveEvents),
    this would disable too much (like Camera usually under Scene on Controls).
  Result := false; }
end;

function TVRMLScene.Dragging: boolean;
begin
  Result := (inherited Dragging) or
    ((PointingDeviceActiveSensor <> nil) and ProcessEvents);
end;

{ Time stuff ------------------------------------------------------------ }

function TVRMLScene.GetTime: TVRMLTime;
begin
  Result := FTime;
end;

procedure TVRMLScene.InternalSetTime(
  const NewValue: TVRMLTime; const TimeIncrease: TKamTime; const ResetTime: boolean);
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
          TimeDependentNodeHandler.SetTime(
            Time, NewValue, TimeIncrease, ResetTime, SomethingChanged);

      { If SomethingChanged on MovieTexture nodes, then we have to redisplay.
        Note that this is not needed for other time-dependent nodes
        (like TimeSensor), as they are not visible (and so can influence
        other nodes only by events, and this will change EventChanged
        to catch it). }
      if SomethingChanged then
        VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);

      for I := 0 to TimeSensorNodes.Count - 1 do
        (TimeSensorNodes.Items[I] as TNodeTimeSensor).
          TimeDependentNodeHandler.SetTime(
            Time, NewValue, TimeIncrease, ResetTime, SomethingChanged);
    finally
      EndChangesSchedule;
    end;
  end;

  FTime := NewValue;
end;

procedure TVRMLScene.SetTime(const NewValue: TKamTime);
var
  TimeIncrease: TKamTime;
  NewCompleteValue: TVRMLTime;
begin
  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  TimeIncrease := NewValue - FTime.Seconds;
  if TimeIncrease > 0 then
    InternalSetTime(NewCompleteValue, TimeIncrease, false);
end;

procedure TVRMLScene.IncreaseTime(const TimeIncrease: TKamTime);
var
  NewCompleteValue: TVRMLTime;
begin
  NewCompleteValue.Seconds := FTime.Seconds + TimeIncrease;
  NewCompleteValue.PlusTicks := 0;
  if TimeIncrease > 0 then
    InternalSetTime(NewCompleteValue, TimeIncrease, false);
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

procedure TVRMLScene.ResetTime(const NewValue: TKamTime);
var
  NewCompleteValue: TVRMLTime;
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes(@ResetLastEventTime, false);

  NewCompleteValue.Seconds := NewValue;
  NewCompleteValue.PlusTicks := 0;
  InternalSetTime(NewCompleteValue, 0, true);
end;

procedure TVRMLScene.ResetTimeAtLoad;
var
  TimeAtLoad: TKamTime;
begin
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TNodeKambiNavigationInfo) and
     TNodeKambiNavigationInfo(NavigationInfoStack.Top).FdTimeOriginAtLoad.Value
    then
    TimeAtLoad := 0.0 else
    TimeAtLoad := DateTimeToUnix(Now);
  ResetTime(TimeAtLoad);
end;

procedure TVRMLScene.IncreaseTimeTick;
begin
  Inc(FTime.PlusTicks);
end;

procedure TVRMLScene.Idle(const CompSpeed: Single);
begin
  inherited;

  { Ignore Idle calls when CompSpeed is precisely zero
    (this may happen, and is correct, see TGLWindow.IgnoreNextIdleSpeed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway. }
  if TimePlaying and (CompSpeed <> 0) then
    IncreaseTime(TimePlayingSpeed * CompSpeed);
end;

procedure TVRMLScene.ResetWorldTime(const NewValue: TKamTime);
begin
  ResetTime(NewValue);
end;

function TVRMLScene.WorldTime: TVRMLTime;
begin
  Result := Time;
end;

{ changes schedule ----------------------------------------------------------- }

procedure TVRMLScene.BeginChangesSchedule;
begin
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
        we can gracefully react in ChangedField to changes:
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
        Node.EventIsActive.Send(NewIsActive, Time);
        if NewIsActive then
          Node.EventEnterTime.Send(Time.Seconds, Time) else
          Node.EventExitTime.Send(Time.Seconds, Time);
      end;

      { Call position_changed, orientation_changed, even if this is just
        the first time NewIsActive = true (that is, even when it was
        NewIsActive <> PSI.IsActive). Reasoning: this allows to activate
        ProximitySensor when world starts (before player moves),
        which is a wanted feature. }

      if NewIsActive then
      begin
        Node.EventPosition_Changed.Send(Position, Time);
        if Node.EventOrientation_Changed.SendNeeded then
        begin
          Direction := MatrixMultDirection(PSI.InvertedTransform, LastViewerDirection);
          Up        := MatrixMultDirection(PSI.InvertedTransform, LastViewerUp);
          Node.EventOrientation_Changed.Send(
            CamDirUp2Orient(Direction, Up), Time);
        end;
        { TODO: centerOfRotation_changed }
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TVRMLScene.ViewerChanged(ACamera: TCamera;
  const Changes: TVisibleChanges);
var
  I: Integer;
begin
  ACamera.GetCameraVectors(
    FLastViewerPosition, FLastViewerDirection, FLastViewerUp);
  FIsLastViewer := true;

  BeginChangesSchedule;
  try
    for I := 0 to ShapeLODs.Count - 1 do
      UpdateLODLevel(TVRMLShapeTreeLOD(ShapeLODs.Items[I]));

    if ProcessEvents then
    begin
      Inc(FTime.PlusTicks);
      for I := 0 to ProximitySensorInstances.Count - 1 do
        ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
    end;
  finally EndChangesSchedule end;

  VisibleChangeHere(Changes + [vcViewer]);
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

procedure TVRMLScene.ExecuteCompiledScript(const HandlerName: string;
  ReceivedValue: TVRMLField);
var
  I: Integer;
begin
  for I := 0 to CompiledScriptHandlers.Count - 1 do
    if CompiledScriptHandlers.Items[I].Name = HandlerName then
    begin
      CompiledScriptHandlers.Items[I].Handler(ReceivedValue, Time);
      Break;
    end;
end;

{ camera ------------------------------------------------------------------ }

procedure TVRMLScene.CameraBindToNavigationAndViewpoint(
  Camera: TUniversalCamera; const Box: TBox3D;
  const ForceNavigationType: string;
  const ForceCameraRadius: Single);
var
  NavigationTypeInitialized: boolean;

  { Initialize stuff determined by NavigationType
    (treating it like NavigationInfo.type value).
    Sets NavigationTypeInitialized to true if navigation type
    recognized (and not 'ANY'). }
  procedure InitializeNavigationType(const NavigationType: string);
  begin
    if NavigationType = 'WALK' then
    begin
      NavigationTypeInitialized := true;
      Camera.NavigationType := ntWalk;
      Camera.Walk.PreferGravityUpForRotations := true;
      Camera.Walk.PreferGravityUpForMoving := true;
      Camera.Walk.Gravity := true;
    end else
    if NavigationType = 'FLY' then
    begin
      NavigationTypeInitialized := true;
      Camera.NavigationType := ntWalk;
      Camera.Walk.PreferGravityUpForRotations := true;
      Camera.Walk.PreferGravityUpForMoving := false;
      Camera.Walk.Gravity := false;
    end else
    if NavigationType = 'NONE' then
    begin
      NavigationTypeInitialized := true;
      Camera.IgnoreAllInputs := true;
    end else
    if (NavigationType = 'EXAMINE') or (NavigationType = 'LOOKAT') then
    begin
      if NavigationType = 'LOOKAT' then
        VRMLWarning(vwIgnorable, 'TODO: Navigation type "LOOKAT" is not yet supported, treating like "EXAMINE"');
      NavigationTypeInitialized := true;
      Camera.NavigationType := ntExamine;
    end else
    if NavigationType = 'ANY' then
    begin
      { Do nothing, also do not report this NavigationInfo.type as unknown. }
    end else
      VRMLWarning(vwSerious, Format('Unknown NavigationInfo.type "%s"',
        [NavigationType]));
  end;

var
  NavigationNode: TNodeNavigationInfo;
  I: Integer;
  CameraRadius: Single;
begin
  NavigationTypeInitialized := false;
  NavigationNode := NavigationInfoStack.Top as TNodeNavigationInfo;

  { Reset Camera properties, this way InitializeNavigationType may
    assume these are already set. }
  Camera.NavigationType := ntWalk;
  Camera.Walk.PreferGravityUpForRotations := true;
  Camera.Walk.PreferGravityUpForMoving := true;
  Camera.Walk.Gravity := false;
  Camera.IgnoreAllInputs := false;

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

  { calculate CameraRadius }
  CameraRadius := ForceCameraRadius;
  if CameraRadius <= 0 then
  begin
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 1) then
      CameraRadius := NavigationNode.FdAvatarSize.Items[0];
    { if avatarSize doesn't specify CameraRadius, or specifies invalid <= 0,
      calculate something suitable based on Box. }
    if CameraRadius <= 0 then
      CameraRadius := Box3DAvgSize(Box, false, 1.0) * 0.005;
  end;

  Camera.CameraRadius := CameraRadius;

  { calculate Camera.Walk.CameraPreferredHeight }
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 2) then
    Camera.Walk.CameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
    { Make it something >> CameraRadius * 2, to allow some
      space to decrease (e.g. by Input_DecreaseCameraPreferredHeight
      in view3dscene). Remember that CorrectCameraPreferredHeight
      adds a limit to CameraPreferredHeight, around CameraRadius * 2. }
    Camera.Walk.CameraPreferredHeight := CameraRadius * 4;

  Camera.Walk.CorrectCameraPreferredHeight;

  Camera.Examine.Init(Box, CameraRadius);

  { calculate HeadBobbing* }
  if (NavigationNode <> nil) and
     (NavigationNode is TNodeKambiNavigationInfo) then
  begin
    Camera.Walk.HeadBobbing := TNodeKambiNavigationInfo(NavigationNode).FdHeadBobbing.Value;
    Camera.Walk.HeadBobbingTime := TNodeKambiNavigationInfo(NavigationNode).FdHeadBobbingTime.Value;
  end else
  begin
    Camera.Walk.HeadBobbing := DefaultHeadBobbing;
    Camera.Walk.HeadBobbingTime := DefaultHeadBobbingTime;
  end;

  CameraBindToViewpoint(Camera, false);
end;

function TVRMLScene.CreateCamera(AOwner: TComponent;
  const Box: TBox3D;
  const ForceNavigationType: string = ''): TUniversalCamera;
begin
  Result := TUniversalCamera.Create(AOwner);
  CameraBindToNavigationAndViewpoint(Result, Box, ForceNavigationType);
end;

function TVRMLScene.CreateCamera(AOwner: TComponent;
  const ForceNavigationType: string = ''): TUniversalCamera;
begin
  Result := CreateCamera(AOwner, BoundingBox, ForceNavigationType);
end;

procedure TVRMLScene.CameraBindToViewpoint(ACamera: TCamera;
  const OnlyViewpointVectorsChanged: boolean);
var
  Position: TVector3Single;
  Direction: TVector3Single;
  Up: TVector3Single;
  GravityUp: TVector3Single;
  NavigationNode: TNodeNavigationInfo;
  WalkCamera: TWalkCamera;
begin
  if ViewpointStack.Top <> nil then
  begin
    (ViewpointStack.Top as TVRMLViewpointNode).GetCameraVectors(
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
  begin
    WalkCamera.GravityUp := GravityUp;

    { update WalkCamera.MoveSpeed }
    if not OnlyViewpointVectorsChanged then
    begin
      NavigationNode := NavigationInfoStack.Top as TNodeNavigationInfo;

      { Change MoveSpeed. }

      if NavigationNode = nil then
      begin
        { Since we don't have NavigationNode.speed, we just calculate some
          speed that should "feel sensible". We base it on CameraRadius.
          CameraRadius in turn was calculated based on
          Box3DAvgSize(SceneAnimation.BoundingBox). }
        WalkCamera.MoveSpeed := ACamera.CameraRadius * 20;
      end else
      if NavigationNode.FdSpeed.Value = 0 then
      begin
        { Then user is not allowed to move at all.

          So we do this is by setting MoveSpeed.
          This is also the reason why other SetViewpointCore branches must change
          MoveSpeed to something different than zero
          (otherwise, user would be stuck with speed = 0). }
        WalkCamera.MoveSpeed := 0;
      end else
      begin
        WalkCamera.MoveSpeed := NavigationNode.FdSpeed.Value;
      end;
    end;
  end;

  { If OnlyViewpointVectorsChanged, then we will move relative to
    initial camera changes. Else, we will jump to new initial camera vectors. }
  ACamera.SetInitialCameraVectors(Position, Direction, Up, OnlyViewpointVectorsChanged);
  if not OnlyViewpointVectorsChanged then
    ACamera.SetCameraVectors(Position, Direction, Up);
end;

{ misc ----------------------------------------------------------------------- }

function TVRMLScene.GetViewpointStack: TVRMLBindableStackBasic;
begin
  Result := FViewpointStack;
end;

function TVRMLScene.GetNavigationInfoStack: TVRMLBindableStackBasic;
begin
  Result := FNavigationInfoStack;
end;

function TVRMLScene.GetBackgroundStack: TVRMLBindableStackBasic;
begin
  Result := FBackgroundStack;
end;

function TVRMLScene.GetFogStack: TVRMLBindableStackBasic;
begin
  Result := FFogStack;
end;

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

function TVRMLScene.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
begin
  ValidateMainLightForShadows;
  Result := FMainLightForShadowsExists;
  if Result then
    AMainLightPosition := FMainLightForShadows;
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
      Inc(FTime.PlusTicks);

      if V.EventCameraMatrix.SendNeeded then
        V.EventCameraMatrix.Send(RenderState.CameraMatrix, Time);

      if V.EventCameraInverseMatrix.SendNeeded then
      begin
        RenderState.CameraInverseMatrixNeeded;
        V.EventCameraInverseMatrix.Send(RenderState.CameraInverseMatrix, Time);
      end;

      if V.EventCameraRotationMatrix.SendNeeded then
        V.EventCameraRotationMatrix.Send(RenderState.CameraRotationMatrix3, Time);

      if V.EventCameraRotationInverseMatrix.SendNeeded then
      begin
        RenderState.CameraRotationInverseMatrixNeeded;
        V.EventCameraRotationInverseMatrix.Send(RenderState.CameraRotationInverseMatrix3, Time);
      end;
    finally EndChangesSchedule end;
  end;
end;

procedure TVRMLScene.PrepareResources(
  TransparentGroups: TTransparentGroups;
  Options: TPrepareResourcesOptions;
  ProgressStep: boolean);

  procedure PrepareShapesOctrees;
  var
    SI: TVRMLShapeTreeIterator;
  begin
    SI := TVRMLShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do SI.Current.OctreeTriangles;
    finally FreeAndNil(SI) end;
  end;

begin
  inherited;

  if prBoundingBox in Options then
    BoundingBox { ignore the result };

  if prTrianglesListNotOverTriangulate in Options then
    TrianglesList(false);

  if prTrianglesListOverTriangulate in Options then
    TrianglesList(true);

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

procedure TVRMLScene.GetHeightAbove(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: P3DTriangle);
begin
  if Exists and Collides and (OctreeCollisions <> nil) then
  begin
    OctreeCollisions.GetHeightAbove(
      Position, GravityUp,
      IsAbove, AboveHeight, PVRMLTriangle(AboveGround),
      nil, TrianglesToIgnoreFunc);
  end else
    inherited;
end;

function TVRMLScene.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if Exists and Collides and (OctreeCollisions <> nil) then
  begin
    Result := OctreeCollisions.MoveAllowed(
      OldPos, ProposedNewPos, NewPos,
      CameraRadius, nil, TrianglesToIgnoreFunc);
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

function TVRMLScene.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or (OctreeCollisions = nil) or
    OctreeCollisions.MoveAllowedSimple(
      OldPos, ProposedNewPos,
      CameraRadius, nil, TrianglesToIgnoreFunc);
end;

function TVRMLScene.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := (not Exists) or (not Collides) or (OctreeCollisions = nil) or
    OctreeCollisions.MoveBoxAllowedSimple(
      OldPos, ProposedNewPos, ProposedNewBox,
      nil, TrianglesToIgnoreFunc);
end;

function TVRMLScene.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and (OctreeCollisions <> nil) and
    OctreeCollisions.IsSegmentCollision(
      Pos1, Pos2,
      nil, false, TrianglesToIgnoreFunc);
end;

function TVRMLScene.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and (OctreeCollisions <> nil) and
    OctreeCollisions.IsSphereCollision(
      Pos, Radius,  nil, TrianglesToIgnoreFunc);
end;

function TVRMLScene.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides and (OctreeCollisions <> nil) and
    OctreeCollisions.IsBoxCollision(
      Box,  nil, TrianglesToIgnoreFunc);
end;

function TVRMLScene.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): T3DCollision;
var
  Triangle: PVRMLTriangle;
  Intersection: TVector3Single;
begin
  Result := nil;
  if Exists and Collides and (OctreeCollisions <> nil) then
  begin
    Triangle := OctreeCollisions.RayCollision(
      Intersection, IntersectionDistance, Ray0, RayVector,
      { ReturnClosestIntersection } true,
      { TriangleToIgnore } nil,
      { IgnoreMarginAtStart } false, TrianglesToIgnoreFunc);
    if Triangle <> nil then
    begin
      Result := T3DCollision.Create;
      Result.Triangle := Triangle;
      Result.Point := Intersection;
      Result.Hierarchy.Add(Self);
    end;
  end;
end;

procedure TVRMLScene.SetShadowMaps(const Value: boolean);
begin
  if FShadowMaps <> Value then
  begin
    FShadowMaps := Value;

    ScheduledShadowMapsProcessing := true;
    ScheduleChangedAll;
  end;
end;

procedure TVRMLScene.SetShadowMapsPCF(const Value: TPercentageCloserFiltering);
begin
  if FShadowMapsPCF <> Value then
  begin
    FShadowMapsPCF := Value;

    if ShadowMaps then { if not ShadowMaps, then no need to reprocess }
    begin
      ScheduledShadowMapsProcessing := true;
      ScheduleChangedAll;
    end;
  end;
end;

procedure TVRMLScene.SetShadowMapsVisualizeDepth(const Value: boolean);
begin
  if FShadowMapsVisualizeDepth <> Value then
  begin
    FShadowMapsVisualizeDepth := Value;

    if ShadowMaps then { if not ShadowMaps, then no need to reprocess }
    begin
      ScheduledShadowMapsProcessing := true;
      ScheduleChangedAll;
    end;
  end;
end;

procedure TVRMLScene.SetShadowMapsDefaultSize(const Value: Cardinal);
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

end.
