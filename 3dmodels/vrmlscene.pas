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
}

{ @abstract(VRML scene as @link(TVRMLScene) class.) }

unit VRMLScene;

interface

uses
  SysUtils, Classes, VectorMath, Boxes3d,
  VRMLFields, VRMLNodes, KambiClassUtils, KambiUtils,
  VRMLShapeState, VRMLTriangleOctree, ProgressUnit, VRMLShapeStateOctree,
  Keys, VRMLTime, Navigation, VRMLOctreeItems;

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
  TVRMLSceneValidity = (fvBBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    { fvFog is not used for now, since FogNode is not cached now
      (doesn't have to be, as it's simple shortcut for FogStack.Top). }
    fvFog,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvManifoldAndBorderEdges,
    fvMainLightForShadows);

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

    { Indexes to TVRMLScene.Triangles(false) array }
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

    { Index to TVRMLScene.Triangles(false) array. }
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

    { Free triangle list created by TrianglesList(false) call.
      This list is also implicitly created by constructing triangle octree
      or ManifoldEdges or BorderEdges.

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

    FOnBoundChanged: TVRMLSceneNotification;

    { Call OnBoundChanged if assigned. }
    procedure DoBoundChanged;

    { Add new node to the top.

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged. }
    procedure Push(Node: TNodeX3DBindableNode);

    { Remove current top node. Returns removed node, or @nil if no current
      node was present (that is, stack was empty).

      This is internal, note that it doesn't send any events
      and doesn't produce DoBoundChanged. }
    function Pop: TNodeX3DBindableNode;
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
      @link(Top), changed. This also includes notification
      when @link(Top) changed to (or from) @nil, that is
      when no node becomes bound or when some node is initially bound. }
    property OnBoundChanged: TVRMLSceneNotification
      read FOnBoundChanged write FOnBoundChanged;
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

  { VRML scene, a final class to handle VRML models
    (with the exception of rendering, which is delegated to descendants,
    like TVRMLGLScene for OpenGL).

    VRML scene works with a graph of VRML nodes
    rooted in RootNode. It also deconstructs this graph to a flat list
    of @link(TVRMLShapeState)
    objects. The basic idea is to "have" at the same time hierarchical
    view of the scene (in @link(RootNode)) and a flattened view of the same scene
    (in @link(ShapeStates) list).

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
    that scene did not changed much). And the @link(ShapeStates) list
    is the main trick for various processing of the scene, most importantly
    it's the main trick to write a flexible OpenGL renderer of the VRML scene.

    Also, VRML2ActiveLights are magically updated for all states in
    ShapeStates list. This is crucial for lights rendering in VRML >= 2.0. }
  TVRMLScene = class
  private
    FOwnsRootNode: boolean;
    FShapeStates: TVRMLShapeStatesList;
    FRootNode: TVRMLNode;

    ChangedAll_TraversedLights: TDynActiveLightArray;
    procedure ChangedAll_Traverse(Node: TVRMLNode; State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);

    FBoundingBox: TBox3d;
    FVerticesCountNotOver, FVerticesCountOver,
    FTrianglesCountNotOver, FTrianglesCountOver: Cardinal;
    Validities: TVRMLSceneValidities;
    function CalculateBoundingBox: TBox3d;
    function CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
    function CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;

    { If appropriate fvXxx is not in Validities, then
      - free if needed appropriate FTrianglesList[] item
      - calculate appropriate FTrianglesList[] item
      - add appropriate fvXxx to Validities. }
    procedure ValidateTrianglesList(OverTriangulate: boolean);
    FTrianglesList:
      array[boolean { OverTriangulate ?}] of TDynTriangle3SingleArray;

    function GetViewpointCore(
      const OnlyPerspective: boolean;
      out CamKind: TVRMLCameraKind;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
      const ViewpointDescription: string):
      TVRMLViewpointNode;

    FManifoldEdges: TDynManifoldEdgeArray;
    FBorderEdges: TDynBorderEdgeArray;
    FOwnsManifoldAndBorderEdges: boolean;

    procedure CalculateIfNeededManifoldAndBorderEdges;

    procedure FreeResources_UnloadTextureData(Node: TVRMLNode);
    procedure FreeResources_UnloadBackgroundImage(Node: TVRMLNode);

    FOnGeometryChanged: TVRMLSceneNotification;
    FOnPostRedisplay: TVRMLSceneNotification;
    FOnViewpointsChanged: TVRMLSceneNotification;
    FOnBoundViewpointVectorsChanged: TVRMLSceneNotification;

    FProcessEvents: boolean;
    procedure SetProcessEvents(const Value: boolean);

    { This is collected by CollectNodesForEvents. @nil if not ProcessEvents. }
    KeySensorNodes, TimeSensorNodes, MovieTextureNodes: TVRMLNodesList;
    ProximitySensorInstances: TDynProximitySensorInstanceArray;

    procedure CollectNodesForEvents;
    procedure TraverseForEvents(Node: TVRMLNode;
      State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
    procedure CollectNodeForEvents(Node: TVRMLNode);
    procedure UnCollectForEvents(Node: TVRMLNode);

    procedure ScriptsInitialize(Node: TVRMLNode);
    procedure ScriptsDeInitialize(Node: TVRMLNode);

    FWorldTime: TVRMLTime;

    { Internal procedure that handles WorldTime changes. }
    procedure InternalSetWorldTime(
      const NewValue: TVRMLTime; const TimeIncrease: TKamTime);

    procedure ResetLastEventTime(Node: TVRMLNode);

    { Bindable nodes helpers }
    FBackgroundStack: TVRMLBindableStack;
    FFogStack: TVRMLBindableStack;
    FNavigationInfoStack: TVRMLBindableStack;
    FViewpointStack: TVRMLBindableStack;

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

    { Everything changed. All octrees must be rebuild, old State pointers
      may be invalid.

      Every ChangedAll call does this.
      ChangedAll must take into account that everything could change.
      Note that ChangedAll traverses the VRML graph again,
      recalculating State values... so the old States are not
      correct anymore. You have to rebuild the octree or your pointers
      will be bad. }
    ScheduledGeometryChangedAll: boolean;

    { Transformation of some shapestates changed. }
    ScheduledGeometrySomeTransformChanged: boolean;

    { Mechanism to schedule ChangedAll and GeometryChanged calls. }
    ChangedAllSchedule: Cardinal;
    ChangedAllScheduled: boolean;

    FPointingDeviceOverItem: POctreeItem;
    FPointingDeviceActive: boolean;
    FPointingDeviceActiveSensor: TNodeX3DPointingDeviceSensorNode;
    procedure SetPointingDeviceActive(const Value: boolean);

    FLogChanges: boolean;

    { Call this when ProximitySensorInstance changed (either the box or
      it's transformation) or when viewer position changed
      (in the future, this will include also implicit changes to viewer position
      by changing transformation of viewer's Viewpoint --- not interesting
      for now, since transforming Viewpoint does nothing for now).
      Viewer position must be at this point be stored within
      LastViewerPosition. }
    procedure ProximitySensorUpdate(var PSI: TProximitySensorInstance);

    { LastViewerPosition is remembered for reacting to changes to
      ProximitySensor box (center, size) (or it's transform) }
    LastViewerPosition: TVector3Single;
    IsLastViewerPosition: boolean;

    FCompiledScriptHandlers: TDynCompiledScriptHandlerInfoArray;

    { Create octree containing all triangles or shape+states from our scene.
      Create octree, inits it with our BoundingBox
      and adds shapestates (or all triangles from our ShapeStates).

      Triangles are generated using calls like
      @code(GeometryNode.Triangulate(State, false, ...)).
      Note that OverTriangulate parameter for Triangulate call above is @false:
      it shouldn't be needed to have triangle octree with over-triangulate
      (over-triangulate is only for rendering with Gouraud shading).

      If Collidable, then only the collidable, or at least "pickable",
      triangles are generated. Which means that children of
      Collision nodes with collide = FALSE are not placed here.
      TODO: update this comment for Collision.proxy handling.
      Otherwise, only the visible (not necessarily collidable)
      items are placed in the octree.

      If ProgressTitle <> '' (and progress is not active already,
      so we avoid starting "progress bar within progress bar",
      and progress user interface is initialized)
      then it uses @link(Progress) while building octree.

      Remember that triangle octree has references to Shape nodes
      inside RootNode vrml tree and to State objects inside
      our ShapeStates list.
      And shapestate octree has references to our ShapeStates list.
      So you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of any
      OctreeXxx property, and the returned octree is not managed
      by this scene.

      Everything in my units is done in the spirit
      that you can create as many octrees as you want for a given scene
      (both octrees based on triangles and based on shapestates).
      Also, in some special cases an octree may be constructed in
      some special way (not only using @link(CreateShapeStateOctree)
      or @link(CreateTriangleOctree)) so that it doesn't contain
      the whole scene from some TVRMLScene object, or it contains
      the scene from many TVRMLScene objects, or something else.

      What I want to say is that it's generally wrong to think of
      an octree as something that maps 1-1 to some TVRMLScene object.
      Octrees, as implemented here, are a lot more flexible.

      @groupBegin }
    function CreateTriangleOctree(AMaxDepth, ALeafCapacity: integer;
      const ProgressTitle: string;
      const Collidable: boolean): TVRMLTriangleOctree;
    function CreateShapeStateOctree(AMaxDepth, ALeafCapacity: integer;
      const ProgressTitle: string;
      const Collidable: boolean;
      const SetShapeSpatial: boolean = false): TVRMLShapeStateOctree;
    { @groupEnd }

    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);

    FTriangleOctreeMaxDepth: Integer;
    FTriangleOctreeLeafCapacity: Integer;
    FTriangleOctreeProgressTitle: string;

    FShapeStateOctreeMaxDepth: Integer;
    FShapeStateOctreeLeafCapacity: Integer;
    FShapeStateOctreeProgressTitle: string;

    FOctreeRendering: TVRMLShapeStateOctree;
    FOctreeDynamicCollisions: TVRMLShapeStateOctree;
    FOctreeVisibleTriangles: TVRMLTriangleOctree;
    FOctreeCollidableTriangles: TVRMLTriangleOctree;

    FSpatial: TVRMLSceneSpatialStructures;
    procedure SetSpatial(const Value: TVRMLSceneSpatialStructures);

    FMainLightForShadowsExists: boolean;
    FMainLightForShadows: TVector4Single;
    procedure SearchMainLightForShadows(Node: TVRMLNode;
      State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
  protected
    { Called when LightNode fields changed, while LightNode is in
      active part of VRML graph. }
    procedure ChangedActiveLightNode(LightNode: TVRMLLightNode;
      Field: TVRMLField); virtual;
  public
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
    destructor Destroy; override;

    { List of shape+states within this VRML scene.

      ShapeStates contents are read-only from outside.

      Note that the only place where ShapeStates length is changed
      in this class is ChangedAll procedure.
      So e.g. if you want to do something after each change of
      ShapeStates length, you can simply override ChangedAll
      and do your work after calling "inherited". }
    property ShapeStates: TVRMLShapeStatesList read FShapeStates;

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
      when you manually change something within RootNode graph.

      Call ChangedAll to notify that "potentially everything changed",
      including adding/removal of some nodes within RootNode and changing their
      fields' values.
      This causes recalculation of all things dependent on RootNode.
      It's more optimal to use one of the other Changed* methods, when possible.

      Call ChangedShapeStateFields(ShapeStateNum, TransformOnly, TextureImageChanged)
      if you only changed
      values of fields within ShapeList[ShapeStateNum].GeometryNode,
      ShapeList[ShapeStateNum].State.Last*. (And you're sure that
      these nodes are not shared by other shape+states using VRML DEF/USE
      mechanism.)

      Set TransformOnly = @true if you know that you changed
      only the State parts of the associatated ShapeState, and only
      on Transform-related fields (see EqualsNoTransform).
      Setting TransformOnly = @true is very beneficial if you
      use TVRMLGLScene with roSeparateShapeStatesNoTransform.

      Pass TransformOnly = @false if unsure, this is safer.

      Call ChangedFields when you only changed field values of given Node.
      This does relatively intelligent discovery of what could be possibly
      affected by changing this node, and updates/invalidates
      cache only where needed. It's acceptable to pass here a Node that
      isn't in the active VRML graph part (that is not reached by Traverse
      method), or even that isn't in the RootNode
      graph at all. Node can also be one of StateDefaultNodes that you took
      from one of State.LastNodes[]. So we really handle all cases here,
      and passing any node is fine here, and we'll try to intelligently
      detect what this change implicates for this VRML scene.

      You can also pass to ChangedFields a field's instance,
      or field's eventIn or eventOut
      that you used to change --- this way we know only this field changed.
      This must belong then to the given Node.
      Pass FieldOrEvent = @nil if you don't know this, or if many fields changed.

      Call ChangedField(Field) if you only changed exactly this one field.
      This is actually just a shortcut for ChangedFields(Field.ParentNode, Field),
      so don't expect to get more optimizations than ChangedFields.
      It's just shorter to type in many circumstances.

      @italic(Descendant implementors notes:) ChangedAll and
      ChangedShapeStateFields are virtual, so of course you can override them
      (remember to always call @code(inherited)). ChangedAll is also
      called by constructor of this class, so you can put a lot of your
      initialization there (instead of in the constructor).

      @groupBegin }
    procedure ChangedAll; virtual;
    procedure ChangedShapeStateFields(ShapeStateNum: Integer;
      const TransformOnly, TextureImageChanged: boolean); virtual;
    procedure ChangedFields(Node: TVRMLNode; FieldOrEvent: TVRMLFieldOrEvent);
    procedure ChangedField(Field: TVRMLField);
    { @groupEnd }

    { Notification when geometry changed.
      "Geometry changed" means that the positions
      of triangles changed. This is not send when merely things like
      material changed.

      What exactly changed can be checked by looking at
      ScheduledGeometryChangedAll,
      ScheduledGeometrySomeTransformChanged,
      TVRMLShapeState.ScheduledLocalGeometryChanged (in all ShapeStates).
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
    property OnGeometryChanged: TVRMLSceneNotification
      read FOnGeometryChanged write FOnGeometryChanged;

    { Notification when anything changed needing redisplay. }
    property OnPostRedisplay: TVRMLSceneNotification
      read FOnPostRedisplay write FOnPostRedisplay;

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
      This is called only when @italic(currentl bound viewpoint stays
      the same, only it's vectors change). }
    property OnBoundViewpointVectorsChanged: TVRMLSceneNotification
      read FOnBoundViewpointVectorsChanged write FOnBoundViewpointVectorsChanged;

    { Call OnPostRedisplay, if assigned. }
    procedure DoPostRedisplay;

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
      (when e.g. animation moves some shapestate by changing it's transformation).

      Add ssRendering to @link(Spatial) property to have this available,
      otherwise it's @nil.

      Note that when VRML scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects.  }
    property OctreeRendering: TVRMLShapeStateOctree read FOctreeRendering;

    { The dynamic octree containing all collidable items.

      This is actually a hierarchy of octrees: scene is partitioned
      first into ShapeStates (each instance of VRML geometry node),
      and then each ShapeState has an octree of triangles inside.

      This octree is useful for all kinds of collision detection.
      Compared to OctreeCollidableTriangles, it is (very slightly on typical scenes)
      less efficient, but it can also be updated very fast.
      For example, merely transforming some ShapeState means that only
      one item needs to be moved in the top-level shapestate tree.
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
    property OctreeDynamicCollisions: TVRMLShapeStateOctree read FOctreeDynamicCollisions;

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
    function OctreeCollisions: TVRMLItemsOctree;

    { Which spatial structures (octrees, for now) should be created and managed.

      You should set this, based on your expected usage of octrees.
      See TVRMLSceneSpatialStructure for possible values.
      For usual dynamic scenes rendered with OpenGL,
      you want this to be [ssRendering, ssDynamicCollisions].

      Before setting any value <> [] you may want to adjust
      TriangleOctreeMaxDepth, TriangleOctreeLeafCapacity,
      ShapeStateOctreeMaxDepth, ShapeStateOctreeLeafCapacity.
      These properties fine-tune how the octrees will be generated
      (although default values should be Ok for typical cases).

      Default value of this property is [], which means that
      no octrees will be created. This has to be the default value,
      to 1. get you chance to change TriangleOctreeMaxDepth and such
      before creating octree 2. otherwise, scenes that not require
      collision detection would unnecessarily create octrees at construction. }
    property Spatial: TVRMLSceneSpatialStructures read FSpatial write SetSpatial;

    { Properties of created triangle octrees.
      See VRMLTriangleOctree unit comments for description.

      If TriangleOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      @groupBegin }
    property     TriangleOctreeMaxDepth: Integer
      read      FTriangleOctreeMaxDepth
      write     FTriangleOctreeMaxDepth
      default DefTriangleOctreeMaxDepth;

    property     TriangleOctreeLeafCapacity: Integer
       read     FTriangleOctreeLeafCapacity
      write     FTriangleOctreeLeafCapacity
      default DefTriangleOctreeLeafCapacity;

    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;
    { @groupEnd }

    { Properties of created shapestate octrees.
      See VRMLShapeStateOctree unit comments for description.

      If ShapeStateOctreeProgressTitle <> '', it will be shown during
      octree creation (through TProgress.Title). Will be shown only
      if progress is not active already
      ( so we avoid starting "progress bar within progress bar").

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      @groupBegin }
    property     ShapeStateOctreeMaxDepth: Integer
      read      FShapeStateOctreeMaxDepth
      write     FShapeStateOctreeMaxDepth
      default DefShapeStateOctreeMaxDepth;

    property     ShapeStateOctreeLeafCapacity: Integer
       read     FShapeStateOctreeLeafCapacity
      write     FShapeStateOctreeLeafCapacity
      default DefShapeStateOctreeLeafCapacity;

    property ShapeStateOctreeProgressTitle: string
      read  FShapeStateOctreeProgressTitle
      write FShapeStateOctreeProgressTitle;
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

      This uses TrianglesList(false).

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

    procedure KeyDown(Key: TKey; C: char; KeysDown: PKeysBooleans);
    procedure KeyUp(Key: TKey; C: char);

    { Call this to when pointing-device moves.
      This may generate the continously-generated events like
      hitPoint_changed, also it updates PointingDeviceOverItem,
      thus producing isOver and such events.

      OverItem may be @nil to indicate we're not over any item.
      In this case, OverPoint is ignored. }
    procedure PointingDeviceMove(const OverPoint: TVector3Single;
      const OverItem: POctreeItem);

    { Current item over which the pointing device is. @nil if over none.
      For example, you can investigate it's pointing device sensors
      (in PointingDeviceOverItem^.State.PointingDeviceSensors),
      although there's a shortcut for just this in @link(PointingDeviceSensors).
      You can change this by PointingDeviceMove and PointingDeviceClear. }
    property PointingDeviceOverItem: POctreeItem
      read FPointingDeviceOverItem write FPointingDeviceOverItem;

    { Pointing-device sensors over which the pointing device is.
      This is just a shortcut for
      PointingDeviceOverItem^.State.PointingDeviceSensors,
      returning @nil if PointingDeviceOverItem = @nil. }
    function PointingDeviceSensors: TVRMLNodesList;

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

    { These change world time, see WorldTime.
      It is crucial that you call this continously to have some VRML
      time-dependent features working, like TimeSensor and MovieTexture.
      See WorldTime for details what is affected by this.

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
      will be notified by usual method, that is OnPostRedisplay.

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
    property ViewpointStack: TVRMLBindableStack read FViewpointStack;

    { If true, and also KambiLog.Log is true, then we will log
      ChangedFields and ChangedAll occurences. Useful only for debugging
      and optimizing VRML events engine. }
    property LogChanges: boolean
      read FLogChanges write FLogChanges default false;

    { Call when viewer position changed, to update sensors. }
    procedure ViewerPositionChanged(const ViewerPosition: TVector3Single);

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
      Calculates also CameraRadius for you.

      Bound NavigationInfo node is just
      NavigationInfoStack.Top. If no NavigationInfo is bound, this is @nil,
      and we will create navigator corresponding to default NavigationInfo
      values (this is following VRML spec), so it will have type = EXAMINE.

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
    function CreateNavigator(out CameraRadius: Single): TNavigator;

    { Update navigator when currently bound viewpoint changes.
      When no viewpoint is currently bound, we will go to standard (initial)
      VRML viewpoint position.

      This moves the navigator to starting point of the viewpoint,
      updating navigator's initial and current vectors
      ([Initial]CameraPos, [Initial]CameraDir, [Initial]CameraUp, GravityUp).

      Currently bound NavigationInfo.speed is also taken into account here.
      Navigator's MoveHorizontalSpeed and MoveVerticalSpeed are updated. }
    procedure NavigatorBindToViewpoint(Nav: TNavigator;
      const CameraRadius: Single; const OnlyViewpointVectorsChanged: boolean);

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

      @seealso TVRMLLightSet.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4Single): boolean;
  end;

{$undef read_interface}

implementation

uses VRMLCameraUtils, KambiStringUtils, KambiLog, VRMLErrors, DateUtils;

{$define read_implementation}
{$I macprecalcvaluereturn.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}
{$I dynarray_5.inc}

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

{ TVRMLScene ----------------------------------------------------------- }

constructor TVRMLScene.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
 inherited Create;
 FRootNode := ARootNode;
 FOwnsRootNode := AOwnsRootNode;

 FTriangleOctreeMaxDepth := DefTriangleOctreeMaxDepth;
 FTriangleOctreeLeafCapacity := DefTriangleOctreeLeafCapacity;
 FShapeStateOctreeMaxDepth := DefShapeStateOctreeMaxDepth;
 FShapeStateOctreeLeafCapacity := DefShapeStateOctreeLeafCapacity;

 FShapeStates := TVRMLShapeStatesList.Create;

 FBackgroundStack := TVRMLBindableStack.Create(Self);
 FFogStack := TVRMLBindableStack.Create(Self);
 FNavigationInfoStack := TVRMLBindableStack.Create(Self);
 FViewpointStack := TVRMLBindableStack.Create(Self);

 FCompiledScriptHandlers := TDynCompiledScriptHandlerInfoArray.Create;

 ScheduleChangedAll;
end;

destructor TVRMLScene.Destroy;
begin
  { This also frees related lists, like KeySensorNodes,
    and does UnregisterProcessEvents(RootNode). }
  ProcessEvents := false;

 FreeAndNil(FTrianglesList[false]);
 FreeAndNil(FTrianglesList[true]);

 if FOwnsManifoldAndBorderEdges then
 begin
   FreeAndNil(FManifoldEdges);
   FreeAndNil(FBorderEdges);
 end;

 FreeAndNil(FCompiledScriptHandlers);

 FreeAndNil(FBackgroundStack);
 FreeAndNil(FFogStack);
 FreeAndNil(FNavigationInfoStack);
 FreeAndNil(FViewpointStack);

 ShapeStates.FreeWithContents;

 FreeAndNil(FOctreeRendering);
 FreeAndNil(FOctreeDynamicCollisions);
 FreeAndNil(FOctreeVisibleTriangles);
 FreeAndNil(FOctreeCollidableTriangles);

 if OwnsRootNode then FreeAndNil(FRootNode);
 inherited;
end;

function TVRMLScene.CalculateBoundingBox: TBox3d;
var i: integer;
begin
 Result := EmptyBox3d;
 for i := 0 to ShapeStates.Count-1 do
  Box3dSumTo1st(Result, ShapeStates[i].BoundingBox);
end;

function TVRMLScene.CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
var i: integer;
begin
 Result := 0;
 for i := 0 to ShapeStates.Count-1 do
  Result += ShapeStates[i].VerticesCount(OverTriangulate);
end;

function TVRMLScene.CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
var i: integer;
begin
 Result := 0;
 for i := 0 to ShapeStates.Count-1 do
  Result += ShapeStates[i].TrianglesCount(OverTriangulate);
end;

function TVRMLScene.BoundingBox: TBox3d;
{$define PRECALC_VALUE_ENUM := fvBBox}
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

procedure TVRMLScene.ChangedAll_Traverse(
  Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
var
  Shape: TVRMLShapeState;
begin
  if Node is TVRMLGeometryNode then
  begin
    { Add shape to ShapeStates }
    Shape := TVRMLShapeState.Create(Node as TVRMLGeometryNode,
      TVRMLGraphTraverseState.CreateCopy(State));
    ShapeStates.Add(Shape);

    { TODO: this has to be improved to handle collidable but not visible
      geometry (Collision.proxy). }

    { When Spatial contain ssDynamicCollisions, then each collidable
      shape must hav octree created. Normally, this is watched over by
      SetSpatial. In this case, we just created new Shape, so we have
      to set it's Spatial property correctly. }
    if (ssDynamicCollisions in Spatial) and
       (State.InsideIgnoreCollision = 0) then
    begin
      Shape.TriangleOctreeProgressTitle := TriangleOctreeProgressTitle;
      Shape.Spatial := [ssTriangles];
    end;

  end else
  if Node is TVRMLLightNode then
  begin
    { Add lights to ChangedAll_TraversedLights }
    ChangedAll_TraversedLights.AppendItem(
      (Node as TVRMLLightNode).CreateActiveLight(State));
  end else
  { Do not look for first bindable node within inlined content,
    this is following VRML spec. }
  if State.InsideInline = 0 then
  begin
    { If some bindable stack is empty, push the node to it.
      This way, upon reading VRML file, we will bind the first found
      node for each stack. }
    if Node is TNodeX3DBackgroundNode then
      BackgroundStack.PushIfEmpty( TNodeX3DBackgroundNode(Node), true) else
    if Node is TNodeFog then
      FogStack.PushIfEmpty( TNodeFog(Node), true) else
    if Node is TNodeNavigationInfo then
      NavigationInfoStack.PushIfEmpty( TNodeNavigationInfo(Node), true) else
    if Node is TVRMLViewpointNode then
      ViewpointStack.PushIfEmpty( TVRMLViewpointNode(Node), true);
  end;
end;

procedure TVRMLScene.ChangedAll;

  procedure UpdateVRML2ActiveLights;

    procedure AddLightEverywhere(const L: TActiveLight);
    var
      J: Integer;
    begin
      for J := 0 to ShapeStates.Count - 1 do
        ShapeStates[J].State.VRML2ActiveLights.AppendItem(L);
    end;

    { Add L everywhere within given Radius from Location.
      Note that this will calculate BoundingBox of every ShapeState
      (but that's simply unavoidable if you have scene with VRML 2.0
      positional lights). }
    procedure AddLightRadius(const L: TActiveLight;
      const Location: TVector3Single; const Radius: Single);
    var
      J: Integer;
    begin
      for J := 0 to ShapeStates.Count - 1 do
        if Box3dSphereCollision(ShapeStates[J].BoundingBox,
             Location, Radius) then
          ShapeStates[J].State.VRML2ActiveLights.AppendItem(L);
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

begin
  if Log and LogChanges then
    WritelnLog('VRML changes', 'ChangedAll');

  { TODO: FManifoldEdges and FBorderEdges and triangles lists should be freed
    (and removed from Validities) on any ChangedXxx call. }

  BackgroundStack.CheckForDeletedNodes(RootNode, true);
  FogStack.CheckForDeletedNodes(RootNode, true);
  NavigationInfoStack.CheckForDeletedNodes(RootNode, true);
  ViewpointStack.CheckForDeletedNodes(RootNode, true);

  ChangedAll_TraversedLights := TDynActiveLightArray.Create;
  try
    ShapeStates.FreeContents;
    Validities := [];

    if RootNode <> nil then
    begin
      RootNode.Traverse(TVRMLNode, @ChangedAll_Traverse);

      UpdateVRML2ActiveLights;

      if ProcessEvents then
        CollectNodesForEvents;
    end;
  finally FreeAndNil(ChangedAll_TraversedLights) end;

  ScheduledGeometryChangedAll := true;
  ScheduleGeometryChanged;

  DoPostRedisplay;
  DoViewpointsChanged;
end;

procedure TVRMLScene.ChangedShapeStateFields(ShapeStateNum: integer;
  const TransformOnly, TextureImageChanged: boolean);
begin
  Validities := [];
  ShapeStates[ShapeStateNum].Changed;
  DoPostRedisplay;
end;

type
  BreakTransformChangeFailed = class(TCodeBreaker);

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
    ShapeStateNum: Cardinal;
    ProximitySensorNum: Cardinal;
    ChangingNode: TVRMLNode;
    AnythingChanged: boolean;
    Inside: boolean;
    ParentScene: TVRMLScene;
    procedure TransformChangeTraverse(
      Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
    procedure TransformChangeTraverseAfter(
      Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
  end;

procedure TTransformChangeHelper.TransformChangeTraverse(
  Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
begin
  if Inside then
  begin
    if Node is TVRMLGeometryNode then
    begin
      ParentScene.ShapeStates[ShapeStateNum].State.AssignTransform(State);
      { TransformOnly = @true, suitable for roSeparateShapeStatesNoTransform,
        they don't have Transform compiled in display list. }
      ParentScene.ChangedShapeStateFields(ShapeStateNum, true, false);
      Inc(ShapeStateNum);
      AnythingChanged := true;
    end else
    if Node is TNodeX3DBackgroundNode then
    begin
      { TODO: make this work to actually change displayed background }
      if Node = ParentScene.BackgroundStack.Top then
        raise BreakTransformChangeFailed.Create;
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
      ParentScene.DoPostRedisplay;
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
      raise BreakTransformChangeFailed.Create;
    end else
    if Node is TNodeProximitySensor then
    begin
      ParentScene.ProximitySensorInstances.Items[ProximitySensorNum].
        InvertedTransform := State.InvertedTransform;
      { Call ProximitySensorUpdate, since the sensor's box moved,
        so possibly it should be activated/deactivated, position_changed
        called etc. }
      if ParentScene.IsLastViewerPosition then
        ParentScene.ProximitySensorUpdate(
          ParentScene.ProximitySensorInstances.Items[ProximitySensorNum]);
      Inc(ProximitySensorNum);
    end;
  end else
  begin
    if Node is TVRMLGeometryNode then
      Inc(ShapeStateNum) else
    if Node is TNodeProximitySensor then
      Inc(ProximitySensorNum) else
    if Node = ChangingNode then
      Inside := true;
  end;
end;

procedure TTransformChangeHelper.TransformChangeTraverseAfter(
  Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
begin
  if Node = ChangingNode then
  begin
    Inside := false;
  end;
end;

procedure TVRMLScene.ChangedFields(Node: TVRMLNode;
  FieldOrEvent: TVRMLFieldOrEvent);
var
  Field: TVRMLField;

  procedure DoLogChanges;
  var
    S: string;
  begin
    S := Format('ChangedFields: Node %s (%s %s) at %s',
      [ Node.NodeName, Node.NodeTypeName, Node.ClassName, PointerToStr(Node) ]);
    if Field <> nil then
      S += Format('. Field %s (%s)', [ Field.Name, Field.VRMLTypeName ]);
    WritelnLog('VRML changes', S);
  end;

var
  NodeLastNodesIndex, I: integer;
  Coord: TMFVec3f;
  TransformChangeHelper: TTransformChangeHelper;
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

  { Ignore this ChangedFields call if node is not in our VRML graph.
    Or is the inactive part. The definition of "active" part
    (see e.g. TVRMLNode.Traverse) is exactly such that we can ignore
    non-active parts here.

    Exception is for StateDefaultNodes nodes (they are not present in RootNode
    graph, but influence us).

    Zakladamy tutaj ze IsNodePresent(,true) zwraca stan Node'a zarowno
    przed modyfkacja pola jak i po - innymi slowy, zakladamy tu ze zmiana
    pola node'a nie mogla zmienic jego wlasnego stanu active/inactive. }

  if (RootNode = nil) or
     ( (not RootNode.IsNodePresent(Node, true)) and
       ((NodeLastNodesIndex = -1) or
         (StateDefaultNodes.Nodes[NodeLastNodesIndex] <> Node))
     ) then
    Exit;

  { Ignore this ChangedFields call if you're changing
    something that doesn't *directly* affect actual content:
    - metadata field
    - metadata or WorldInfo nodes
    - sensors (they don't affect actual content directly --- only when
      they are routed somewhere, and this will be eventually
      detected in another ChangedFields call)
    - script (like for sensors; script nodes take care themselves
      to react to events send to them)
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
     (Node is TNodeX3DPointingDeviceSensorNode) or
     (Node is TNodeTimeSensor) or
     (Node is TNodeScript) then
    Exit;

  { Test other changes: }

  if Node is TNodeComposedShader then
  begin
    { Do nothing here, as TVRMLOpenGLRenderer registers and takes care
      to update shaders' uniform variables. We don't have to do
      anything here, no need to rebuild/recalculate anything.
      The only thing that needs to be called is redisplay, by DoPostRedisplay
      at the end of ChangedFields. }
  end else
  if Node is TNodeCoordinate then
  begin
    { TNodeCoordinate is special, although it's part of VRML 1.0 state,
      it can also occur within coordinate-based nodes of VRML >= 2.0.
      So it affects coordinate-based nodes with this node.

      In fact, code below takes into account both VRML 1.0 and 2.0 situation,
      that's why it's before "if NodeLastNodesIndex <> -1 then" branch. }
    for I := 0 to ShapeStates.Count - 1 do
      if ShapeStates[I].GeometryNode.Coord(ShapeStates[I].State, Coord) and
         (Coord.ParentNode = Node) then
      begin
        ChangedShapeStateFields(I, false, false);

        { Another special thing about Coordinate node: it changes
          actual geometry. }

        ShapeStates[I].ScheduledLocalGeometryChanged := true;
        ScheduleGeometryChanged;
      end;
  end else
  if NodeLastNodesIndex <> -1 then
  begin
    { Node is part of VRML 1.0 state, so it affects ShapeStates where
      it's present on State.LastNodes list. }
    for i := 0 to ShapeStates.Count - 1 do
      if ShapeStates[i].State.LastNodes.Nodes[NodeLastNodesIndex] = Node then
        ChangedShapeStateFields(i, false, false);
  end else
  if Node is TNodeMaterial_2 then
  begin
    { VRML 2.0 Material affects only shapes where it's
      placed inside Appearance.material field. }
    for I := 0 to ShapeStates.Count - 1 do
      if ShapeStates[I].State.ParentShape.Material = Node then
        ChangedShapeStateFields(I, false, false);
  end else
  if Node is TNodeX3DTextureCoordinateNode then
  begin
    { VRML 2.0 TextureCoordinate affects only shapes where it's
      placed inside texCoord field. }
    for I := 0 to ShapeStates.Count - 1 do
      if (ShapeStates[I].GeometryNode is TNodeX3DComposedGeometryNode) and
         (TNodeX3DComposedGeometryNode(ShapeStates[I].GeometryNode).
           FdTexCoord.Value = Node) then
        ChangedShapeStateFields(I, false, false);
  end else
  if Node is TVRMLLightNode then
  begin
    ChangedActiveLightNode(TVRMLLightNode(Node), Field);
  end else
  if Node is TVRMLGeometryNode then
  begin
    { node jest Shape'm. Wiec wplynal tylko na ShapeStates gdzie wystepuje jako
      GeometryNode. }
    for i := 0 to ShapeStates.Count-1 do
      if ShapeStates[i].GeometryNode = Node then
      begin
        ChangedShapeStateFields(i, false, false);

        ShapeStates[I].ScheduledLocalGeometryChanged := true;
        ScheduleGeometryChanged;
      end;
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
      ShapeStates, to know which ShapeStates will be affected by what
      state change (as we cannot deduce it from the mere GeometryNode
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
      TransformChangeHelper := TTransformChangeHelper.Create;
      try
        TransformChangeHelper.ParentScene := Self;
        TransformChangeHelper.ShapeStateNum := 0;
        TransformChangeHelper.ProximitySensorNum := 0;
        TransformChangeHelper.ChangingNode := Node;
        TransformChangeHelper.AnythingChanged := false;
        TransformChangeHelper.Inside := false;

        RootNode.Traverse(TVRMLNode,
          @TransformChangeHelper.TransformChangeTraverse,
          @TransformChangeHelper.TransformChangeTraverseAfter);

        if not TransformChangeHelper.AnythingChanged then
          { No need to do ScheduleGeometryChanged, not even DoPostRedisplay
            at the end. }
          Exit;

      finally FreeAndNil(TransformChangeHelper) end;

      ScheduledGeometrySomeTransformChanged := true;
      ScheduleGeometryChanged;
    except
      on BreakTransformChangeFailed do
      begin
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
      if IsLastViewerPosition then
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
  if (Node is TNodeMovieTexture) or
     (Node is TNodeTimeSensor) then
  begin
    { No need to do anything.
      Everything updated in time-dependent nodes will be catched by next
      IncreaseWorldTime run. }
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

    for I := 0 to ShapeStates.Count - 1 do
      if ShapeStates[i].State.Texture = Node then
        ChangedShapeStateFields(I, false, true);
  end else
  begin
    { node jest czyms innym; wiec musimy zalozyc ze zmiana jego pol wplynela
      jakos na State nastepujacych po nim node'ow (a moze nawet wplynela na to
      co znajduje sie w aktywnej czesci grafu VRMLa pod niniejszym node'm -
      tak sie moglo stac gdy zmienilismy pole Switch.whichChild. ) }
    ScheduleChangedAll;
    Exit;
  end;

  DoPostRedisplay;
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
  I, J: integer;
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
      RenderShapeStateLights will have to take care of eventual "radius"
      constraints. }

    for I := 0 to ShapeStates.Count - 1 do
    begin
      for J := 0 to ShapeStates[I].State.CurrentActiveLights.Count - 1 do
      begin
        ActiveLight := @(ShapeStates[I].State.CurrentActiveLights.Items[J]);
        if ActiveLight^.LightNode = LightNode then
          LightNode.UpdateActiveLight(ActiveLight^);
      end;
    end;
  end;

  { When some kambiShadows or kambiShadowsMain field changes,
    then MainLightForShadows must be recalculated. }

  if (Field = nil) or
     (Field = LightNode.FdKambiShadows) or
     (Field = LightNode.FdKambiShadowsMain) then
    Exclude(Validities, fvMainLightForShadows);
end;

procedure TVRMLScene.DoPostRedisplay;
begin
  if Assigned(OnPostRedisplay) then
    OnPostRedisplay(Self);
end;

procedure TVRMLScene.DoGeometryChanged;
var
  I: Integer;
  SomeLocalGeometryChanged: boolean;
begin
  Validities := Validities - [fvBBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvManifoldAndBorderEdges];

  { Clear variables after removing fvTrianglesList* }
  FreeAndNil(FTrianglesList[false]);
  FreeAndNil(FTrianglesList[true]);

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

  { First, call LocalGeometryChanged on shapes when needed.
    By the way, also calculate SomeLocalGeometryChanged (= if any
    LocalGeometryChanged was called, which means that octree and
    bounding box/sphere of some shape changed).

    Note that this also creates implication ScheduledGeometryChangedAll
    => SomeLocalGeometryChanged. In later code, I sometimes check
    for SomeLocalGeometryChanged, knowing that this also checks for
    ScheduledGeometryChangedAll. }

  if ScheduledGeometryChangedAll then
  begin
    SomeLocalGeometryChanged := true;
    for I := 0 to ShapeStates.Count - 1 do
      ShapeStates[I].LocalGeometryChanged;
    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'All TVRMLShapeState.OctreeTriangles updated');
  end else
  begin
    SomeLocalGeometryChanged := false;
    for I := 0 to ShapeStates.Count - 1 do
    begin
      if ShapeStates[I].ScheduledLocalGeometryChanged then
      begin
        SomeLocalGeometryChanged := true;
        ShapeStates[I].LocalGeometryChanged;
        if Log and LogChanges and
           (ShapeStates[I].OctreeTriangles <> nil) then
          WritelnLog('VRML changes (octree)', Format('ShapeState[%d].OctreeTriangles updated', [I]));
      end;
    end;
  end;

  if (OctreeRendering <> nil) and
     (ScheduledGeometrySomeTransformChanged or
      SomeLocalGeometryChanged) then
  begin
    { Remember to do FreeAndNil on octrees below.
      Although we will recreate octrees right after rebuilding,
      it's still good to nil them right after freeing.
      Otherwise, when exception will raise from CreateXxxOctree,
      Scene.OctreeXxx will be left as invalid pointer. }

    FreeAndNil(FOctreeRendering);
    FOctreeRendering := CreateShapeStateOctree(
      ShapeStateOctreeMaxDepth,
      ShapeStateOctreeLeafCapacity,
      ShapeStateOctreeProgressTitle,
      false);

    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeRendering updated');
  end;

  if (OctreeDynamicCollisions <> nil) and
     (ScheduledGeometrySomeTransformChanged or
      SomeLocalGeometryChanged) then
  begin
    FreeAndNil(FOctreeDynamicCollisions);
    FOctreeDynamicCollisions := CreateShapeStateOctree(
      TriangleOctreeMaxDepth,
      TriangleOctreeLeafCapacity,
      TriangleOctreeProgressTitle,
      true);

    if Log and LogChanges then
      WritelnLog('VRML changes (octree)', 'OctreeDynamicCollisions updated');
  end;

  if SomeLocalGeometryChanged then
    { Some POctreeItems became invalid. }
    PointingDeviceClear;

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self);

  { clear ScheduledGeometryXxx flags now }
  ScheduledGeometryChangedAll := false;
  ScheduledGeometrySomeTransformChanged := false;
  for I := 0 to ShapeStates.Count - 1 do
    ShapeStates[I].ScheduledLocalGeometryChanged := false;
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

procedure TVRMLScene.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, GeometryNode, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

procedure TVRMLScene.SetSpatial(const Value: TVRMLSceneSpatialStructures);

  procedure SetShapeStateSpatial(const Value: TVRMLShapeSpatialStructures);
  var
    I: Integer;
  begin
    for I := 0 to ShapeStates.Count - 1 do
      ShapeStates[I].Spatial := Value;
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
      FOctreeRendering := CreateShapeStateOctree(
        ShapeStateOctreeMaxDepth,
        ShapeStateOctreeLeafCapacity,
        ShapeStateOctreeProgressTitle,
        false);
    end;

    { Handle OctreeDynamicCollisions and ShapeStates[I].Spatial }

    Old := ssDynamicCollisions in Spatial;
    New := ssDynamicCollisions in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeDynamicCollisions);
      SetShapeStateSpatial([]);
    end else
    if New and not Old then
    begin
      FOctreeDynamicCollisions := CreateShapeStateOctree(
        TriangleOctreeMaxDepth,
        TriangleOctreeLeafCapacity,
        TriangleOctreeProgressTitle,
        true,
        { During this, set also ShapeStates[I].Spatial := [ssTriangles].
          This way one progress bar displays progress of creating
          both FOctreeDynamicCollisions and specific octrees for items. }
        true);
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
        TriangleOctreeMaxDepth,
        TriangleOctreeLeafCapacity,
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
        TriangleOctreeMaxDepth,
        TriangleOctreeLeafCapacity,
        TriangleOctreeProgressTitle,
        true);
    end;

    FSpatial := Value;
  end;
end;

function TVRMLScene.OctreeCollisions: TVRMLItemsOctree;
begin
  if OctreeCollidableTriangles <> nil then
    Result := OctreeCollidableTriangles else
  if OctreeDynamicCollisions <> nil then
    Result := OctreeDynamicCollisions else
    Result := nil;
end;

function TVRMLScene.CreateTriangleOctree(
  AMaxDepth, ALeafCapacity: integer;
  const ProgressTitle: string;
  const Collidable: boolean): TVRMLTriangleOctree;

  procedure FillOctree(AddTriProc: TNewTriangleProc);
  var
    I: Integer;
  begin
    for I := 0 to ShapeStates.Count - 1 do
      { TODO: this has to be improved to handle collidable but not visible
        geometry (Collision.proxy). }
      if (not Collidable) or
         (ShapeStates[I].State.InsideIgnoreCollision = 0) then
        ShapeStates[I].GeometryNode.Triangulate(
          ShapeStates[I].State, false, AddTriProc);
  end;

begin
  Result := TVRMLTriangleOctree.Create(AMaxDepth, ALeafCapacity, BoundingBox);
  try
    Result.OctreeItems.AllowedCapacityOverflow := TrianglesCount(false);
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
      Result.OctreeItems.AllowedCapacityOverflow := 4;
    end;
  except Result.Free; raise end;
end;

function TVRMLScene.CreateShapeStateOctree(
  AMaxDepth, ALeafCapacity: integer;
  const ProgressTitle: string;
  const Collidable: boolean;
  const SetShapeSpatial: boolean): TVRMLShapeStateOctree;

  procedure AddShapeState(const I: Integer);
  begin
    { TODO: this has to be improved to handle collidable but not visible
      geometry (Collision.proxy). }
    if (not Collidable) or
       (ShapeStates[I].State.InsideIgnoreCollision = 0) then
    begin
      Result.TreeRoot.AddItem(I);

      if SetShapeSpatial then
      begin
        {
          ShapeStates[I].TriangleOctreeMaxDepth :=
          ShapeStates[I].TriangleOctreeLeafCapacity :=

          Leave them at defaults? We shouldn't just use
          our TriangleOctreeMaxDepth properties, they may be unsuitable.
        }
        ShapeStates[I].TriangleOctreeProgressTitle := TriangleOctreeProgressTitle;
        ShapeStates[I].Spatial := [ssTriangles];
      end;
    end;
  end;

var
  I: Integer;
begin
  Result := TVRMLShapeStateOctree.Create(AMaxDepth, ALeafCapacity,
    BoundingBox, ShapeStates);
  try
    if (ProgressTitle <> '') and
       (Progress.UserInterface <> nil) and
       (not Progress.Active) then
    begin
      Progress.Init(ShapeStates.Count, ProgressTitle, true);
      try
        for I := 0 to ShapeStates.Count - 1 do
        begin
          AddShapeState(I);
          Progress.Step;
        end;
      finally Progress.Fini end;
    end else
    begin
      for I := 0 to ShapeStates.Count - 1 do
        AddShapeState(I);
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
    procedure Seek(Node: TVRMLNode;
      State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
  end;

  procedure TFirstViewpointSeeker.Seek(
    Node: TVRMLNode;
    State: TVRMLGraphTraverseState;
    ParentInfo: PTraversingInfo);
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
      GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangleAdder.AddTriangle(const Triangle: TTriangle3Single;
    State: TVRMLGraphTraverseState;
    GeometryNode: TVRMLGeometryNode;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  begin
    if IsValidTriangle(Triangle) then
      TriangleList.AppendItem(Triangle);
  end;

function TVRMLScene.CreateTrianglesList(OverTriangulate: boolean):
  TDynTriangle3SingleArray;
var
  I: Integer;
  TriangleAdder: TTriangleAdder;
begin
  Result := TDynTriangle3SingleArray.Create;
  try
    Result.AllowedCapacityOverflow := TrianglesCount(false);
    try
      TriangleAdder := TTriangleAdder.Create;
      try
        TriangleAdder.TriangleList := Result;
        for I := 0 to ShapeStates.Count - 1 do
          ShapeStates[I].GeometryNode.Triangulate(
            ShapeStates[I].State, OverTriangulate,
            {$ifdef FPC_OBJFPC} @ {$endif} TriangleAdder.AddTriangle);
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
            FManifoldEdges.AppendItem(EdgePtr^);

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
      EdgesSingle.IncLength;
      EdgePtr := EdgesSingle.Pointers[EdgesSingle.High];
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

    { It's important here that TrianglesList guarentees that only valid
      triangles are included. Otherwise degenerate triangles could make
      shadow volumes rendering result bad. }
    Triangles := TrianglesList(false);

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

{ freeing resources ---------------------------------------------------------- }

procedure TVRMLScene.FreeResources_UnloadTextureData(Node: TVRMLNode);
begin
  (Node as TVRMLTextureNode).IsTextureLoaded := false;
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
    RootNode.EnumerateNodes(TVRMLTextureNode,
      @FreeResources_UnloadTextureData, false);

  if (frBackgroundImageInNodes in Resources) and (RootNode <> nil) then
    RootNode.EnumerateNodes(TNodeBackground,
      @FreeResources_UnloadBackgroundImage, false);

  if frTrianglesListNotOverTriangulate in Resources then
  begin
    Exclude(Validities, fvTrianglesListNotOverTriangulate);
    FreeAndNil(FTrianglesList[false]);
  end;

  if frTrianglesListOverTriangulate in Resources then
  begin
    Exclude(Validities, fvTrianglesListOverTriangulate);
    FreeAndNil(FTrianglesList[true]);
  end;

  if frManifoldAndBorderEdges in Resources then
  begin
    Exclude(Validities, fvManifoldAndBorderEdges);
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

procedure TVRMLScene.TraverseForEvents(Node: TVRMLNode;
  State: TVRMLGraphTraverseState;
  ParentInfo: PTraversingInfo);
var
  PSI: PProximitySensorInstance;
begin
  ProximitySensorInstances.IncLength;
  PSI := ProximitySensorInstances.Pointers[ProximitySensorInstances.High];
  PSI^.Node := Node as TNodeProximitySensor;
  PSI^.InvertedTransform := State.InvertedTransform;
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
begin
  if FProcessEvents <> Value then
  begin
    if Value then
    begin
      IsLastViewerPosition := false;
      KeySensorNodes := TVRMLNodesList.Create;
      TimeSensorNodes := TVRMLNodesList.Create;
      MovieTextureNodes := TVRMLNodesList.Create;
      ProximitySensorInstances := TDynProximitySensorInstanceArray.Create;
      CollectNodesForEvents;
    end else
    begin
      UnregisterProcessEvents(RootNode);
      FreeAndNil(KeySensorNodes);
      FreeAndNil(TimeSensorNodes);
      FreeAndNil(MovieTextureNodes);
      FreeAndNil(ProximitySensorInstances);
      PointingDeviceClear;
    end;
    FProcessEvents := Value;
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

procedure TVRMLScene.KeyDown(Key: TKey; C: char; KeysDown: PKeysBooleans);
var
  I: Integer;
  KeySensor: TNodeKeySensor;
  ActionKey: Integer;
begin
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

procedure TVRMLScene.KeyUp(Key: TKey; C: char);
var
  I: Integer;
  KeySensor: TNodeKeySensor;
  ActionKey: Integer;
begin
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
  const OverItem: POctreeItem);
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
              FPointingDeviceActiveSensor :=
                TNodeX3DPointingDeviceSensorNode(ToActivate);
              PointingDeviceActiveSensor.EventIsActive.Send(true, WorldTime);
              Break;
            end else
            if ToActivate is TNodeAnchor then
            begin
              AnchorActivate(TNodeAnchor(ToActivate));
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
        end;
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

function TVRMLScene.PointingDeviceSensors: TVRMLNodesList;
begin
  if PointingDeviceOverItem <> nil then
    Result := PointingDeviceOverItem^.State.PointingDeviceSensors else
    Result := nil;
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
        DoPostRedisplay;

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
    DoGeometryChanged;
    GeometryChangedScheduled := false;
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
  Position: TVector3Single;
  Node: TNodeProximitySensor;
  NewIsActive: boolean;
begin
  Assert(IsLastViewerPosition);
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
      end else
      if NewIsActive then
      begin
        Node.EventPosition_Changed.Send(Position, WorldTime);
        { TODO: centerOfRotation_changed, orientation_changed }
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TVRMLScene.ViewerPositionChanged(const ViewerPosition: TVector3Single);
var
  I: Integer;
begin
  if ProcessEvents then
  begin
    Inc(FWorldTime.PlusTicks);
    BeginChangesSchedule;
    try
      LastViewerPosition := ViewerPosition;
      IsLastViewerPosition := true;

      for I := 0 to ProximitySensorInstances.Count - 1 do
        ProximitySensorUpdate(ProximitySensorInstances.Items[I]);
    finally
      EndChangesSchedule;
    end;
  end;
end;

{ compiled scripts ----------------------------------------------------------- }

procedure TVRMLScene.RegisterCompiledScript(const HandlerName: string;
  Handler: TCompiledScriptHandler);
var
  HandlerInfo: PCompiledScriptHandlerInfo;
begin
  CompiledScriptHandlers.IncLength;
  HandlerInfo := CompiledScriptHandlers.Pointers[CompiledScriptHandlers.High];
  HandlerInfo^.Handler := Handler;
  HandlerInfo^.Name := HandlerName;
end;

{ navigator ------------------------------------------------------------------ }

function TVRMLScene.CreateNavigator(out CameraRadius: Single): TNavigator;
var
  NavigationNode: TNodeNavigationInfo;
  I: Integer;
  CameraPreferredHeight: Single;
begin
  NavigationNode := NavigationInfoStack.Top as TNodeNavigationInfo;

  Result := nil;

  if NavigationNode <> nil then
    for I := 0 to NavigationNode.FdType.Count - 1 do
      if NavigationNode.FdType.Items[I] = 'WALK' then
      begin
        Result := TWalkNavigator.Create(nil);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := true;
        TWalkNavigator(Result).Gravity := true;
        TWalkNavigator(Result).IgnoreAllInputs := false;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'FLY' then
      begin
        Result := TWalkNavigator.Create(nil);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := false;
        TWalkNavigator(Result).Gravity := false;
        TWalkNavigator(Result).IgnoreAllInputs := false;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'NONE' then
      begin
        Result := TWalkNavigator.Create(nil);
        TWalkNavigator(Result).PreferGravityUpForRotations := true;
        TWalkNavigator(Result).PreferGravityUpForMoving := true; { doesn't matter }
        TWalkNavigator(Result).Gravity := false;
        TWalkNavigator(Result).IgnoreAllInputs := true;
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'EXAMINE' then
      begin
        Result := TExamineNavigator.Create(nil);
        Break;
      end else
      if NavigationNode.FdType.Items[I] = 'ANY' then
      begin
        { Do nothing, also do not report this NavigationInfo.type as unknown. }
      end else
        VRMLNonFatalError(Format('Unknown NavigationInfo.type "%s"',
          [NavigationNode.FdType.Items[I]]));

  if Result = nil then
    { No recognized "type" found, so use default type EXAMINE. }
    Result := TExamineNavigator.Create(nil);

  { calculate CameraRadius }
  CameraRadius := 0;
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 1) then
    CameraRadius := NavigationNode.FdAvatarSize.Items[0];
  { if avatarSize doesn't specify CameraRadius, or specifies invalid <= 0,
    calculate something suitable based on Scene.BoundingBox. }
  if CameraRadius <= 0 then
    CameraRadius := Box3dAvgSize(BoundingBox, 1.0) * 0.01;

  if Result is TWalkNavigator then
  begin
    { For NavigationNode = nil, always Examine is created. }
    Assert(NavigationNode <> nil);

    { calculate CameraPreferredHeight }
    if NavigationNode.FdAvatarSize.Count >= 2 then
      CameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      CameraPreferredHeight := CameraRadius * 2;

    TWalkNavigator(Result).CameraPreferredHeight := CameraPreferredHeight;
    TWalkNavigator(Result).CorrectCameraPreferredHeight(CameraRadius);
  end else
  if Result is TExamineNavigator then
  begin
    TExamineNavigator(Result).Init(BoundingBox);
  end;

  NavigatorBindToViewpoint(Result, CameraRadius, false);
end;

procedure TVRMLScene.NavigatorBindToViewpoint(Nav: TNavigator;
  const CameraRadius: Single; const OnlyViewpointVectorsChanged: boolean);
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
    VectorAdjustToLengthTo1st(CameraDir, CameraRadius * 0.4);
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

procedure TVRMLScene.SearchMainLightForShadows(Node: TVRMLNode;
  State: TVRMLGraphTraverseState;
  ParentInfo: PTraversingInfo);
var
  L: TVRMLLightNode absolute Node;
begin
  if L.FdKambiShadows.Value and
     L.FdKambiShadowsMain.Value then
  begin
    if L is TVRMLPositionalLightNode then
      FMainLightForShadows := Vector4Single(
        MatrixMultPoint(State.Transform,
          TVRMLPositionalLightNode(L).FdLocation.Value), 1) else
    if L is TVRMLDirectionalLightNode then
      FMainLightForShadows := Vector4Single( Normalized(
        MatrixMultDirection(State.Transform,
          TVRMLDirectionalLightNode(L).FdDirection.Value) ), 0) else
      raise Exception.CreateFmt('TVRMLScene.MainLightForShadows: ' +
        'light node "%s" cannot be used to cast shadows, it has no position ' +
        'and no direction', [L.NodeTypeName]);
    FMainLightForShadowsExists := true;
    raise BreakMainLightForShadows.Create;
  end;
end;

function TVRMLScene.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;

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

  Result := FMainLightForShadowsExists;
  if Result then
    AMainLightPosition := FMainLightForShadows;
end;

end.
