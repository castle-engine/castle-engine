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
  Keys, KambiTimeUtils;

{$define read_interface}

type
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
    fvManifoldAndBorderEdges);

  { @exclude }
  TVRMLSceneValidities = set of TVRMLSceneValidity;

  TViewpointFunction = procedure (Node: TVRMLViewpointNode;
    const Transform: TMatrix4Single) of object;

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
  public
    constructor Create(AParentScene: TVRMLScene);

    property ParentScene: TVRMLScene read FParentScene;

    { Returns top item on this stack, or @nil if not present. }
    function Top: TNodeX3DBindableNode;

    { Add new node to the top. }
    procedure Push(Node: TNodeX3DBindableNode);

    { Add new node to the top, but only if stack is currently empty.
      If SendEvents, then isBound = true and bindTime events will be
      send to newly bound node. }
    procedure PushIfEmpty(Node: TNodeX3DBindableNode; SendEvents: boolean);

    { Remove current top node. Returns removed node, or @nil if no current
      node was present (that is, stack was empty). }
    function Pop: TNodeX3DBindableNode;

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
  end;

  TVRMLSceneNotification = procedure (Scene: TVRMLScene) of object;

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

    TriangleOctreeToAdd: TVRMLTriangleOctree;
    procedure AddTriangleToOctreeProgress(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);

    FDefaultShapeStateOctree: TVRMLShapeStateOctree;
    FDefaultTriangleOctree: TVRMLTriangleOctree;
    FOwnsDefaultTriangleOctree: boolean;
    FOwnsDefaultShapeStateOctree: boolean;

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

    FProcessEvents: boolean;
    procedure SetProcessEvents(const Value: boolean);

    { This is collected by CollectNodesForEvents. @nil if not ProcessEvents. }
    KeySensorNodes, TimeSensorNodes, MovieTextureNodes: TVRMLNodesList;

    procedure CollectNodesForEvents;
    procedure Collect_KeySensor(Node: TVRMLNode);
    procedure Collect_TimeSensor(Node: TVRMLNode);
    procedure Collect_MovieTexture(Node: TVRMLNode);
    procedure Collect_ChangedFields(Node: TVRMLNode);
    procedure UnCollect_ChangedFields(Node: TVRMLNode);

    FWorldTime: TKamTime;

    { Internal procedure that handles WorldTime changes. }
    procedure InternalSetWorldTime(
      const NewValue, TimeIncrease: TKamTime);

    procedure ResetRoutesLastEventTime(Node: TVRMLNode);

    { Helpers for the case when VRML >= 2.0 Transform changed it's fields. }
    TransformChange_ShapeStateNum: Cardinal;
    TransformChange_ChangingNode: TVRMLNode;
    TransformChange_AnythingChanged: boolean;
    TransformChange_Inside: boolean;
    procedure TransformChangeTraverse(
      Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
    procedure TransformChangeTraverseAfter(
      Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);

    { Bindable nodes helpers }
    FBackgroundStack: TVRMLBindableStack;
    FFogStack: TVRMLBindableStack;
    FNavigationInfoStack: TVRMLBindableStack;
    FViewpointStack: TVRMLBindableStack;

    { Mechanism to schedule geometry changed calls.

      Since DoGeometryChanged call may be costly (rebuilding the octree),
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

      Call ChangedShapeStateFields(ShapeStateNum, TransformOnly) if you only changed
      values of fields within ShapeList[ShapeStateNum].GeometryNode,
      ShapeList[ShapeStateNum].State.Last* and
      ShapeList[ShapeStateNum].State.Active*. (And you're sure that
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

      @italic(Descendant implementors notes:) ChangedAll and
      ChangedShapeStateFields are virtual, so of course you can override them
      (remember to always call @code(inherited)). ChangedAll is also
      called by constructor of this class, so you can put a lot of your
      initialization there (instead of in the constructor).

      @groupBegin }
    procedure ChangedAll; virtual;
    procedure ChangedShapeStateFields(ShapeStateNum: Integer;
      const TransformOnly: boolean); virtual;
    procedure ChangedFields(Node: TVRMLNode; FieldOrEvent: TVRMLFieldOrEvent);
    { @groupEnd }

    { Notification when geometry changed, so you may need to rebuild
      things like an octree. "Geometry changed" means that the positions
      of triangles changed. This is not send when merely things like
      material changed.

      This is particularly useful when using ProcessEvents = @true,
      since then you don't have control over when ChangedXxx are called.
      (When ProcessEvents = @false, you always call ChangedXxx explicitly
      and in most cases you already know when the geometry did and when
      it did not change.)

      In particular, note that every ChangedAll call is guaranteed to send this.
      ChangedAll must take into account that everything could change.
      Note that ChangedAll traverses the VRML graph again,
      recalculating State values... so the old States are not
      correct anymore. You have to rebuild the octree or your pointers
      will be bad. (So if you want to ignore OnGeometryChanged calls
      and keep a temporarily-invalid octree, note that in these cases
      octree will be really invalid: not only it will not correspond
      to current geometry, but also the pointers will be invalid.) }
    property OnGeometryChanged: TVRMLSceneNotification
      read FOnGeometryChanged write FOnGeometryChanged;

    { Notification when anything changed needing redisplay. }
    property OnPostRedisplay: TVRMLSceneNotification
      read FOnPostRedisplay write FOnPostRedisplay;

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

      That's why it is possible to change RootNode and it is even
      possible to set it to nil. And when When RootNode = nil everything
      should work -- you can query such scene (with RootNode = nil)
      for Vecrtices/TrianglesCount (answer will be 0),
      for BoundingBox (answer will be EmptyBox3d),
      you can render such scene (nothing will be rendered) etc.
      Scene RootNode = nil will act quite like a Scene with
      e.g. no TVRMLGeometryNode nodes.

      Always call ChangedAll when you changed RootNode.

      Note that there is also a trick to conserve memory use.
      After you've done PrepareRender some things are precalculated here,
      and RootNode is actually not used. So you can free RootNode
      (and set it to nil here) @italic(without calling ChangedAll)
      and some things will just continue to work, unaware of the fact
      that the underlying RootNode structure is lost.
      Note that this is still considered a "trick", and you will
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

    { Create octree containing all triangles from our scene.
      Creates triangle-based octree, inits it with our BoundingBox
      and adds all triangles from our ShapeStates.
      Triangles are generated using calls like
      @code(GeometryNode.Triangulate(State, false, ...)).
      Note that OverTriangulate parameter for Triangulate call above is @false:
      it shouldn't be needed to have octree with over-triangulate
      (over-triangulate is for rendering with Gouraud shading).

      If ProgressTitle <> '' (and progress is not active already,
      so we avoid starting "progress bar within progress bar")
      then it uses @link(Progress) while building octree.

      Remember that such octree has a reference to Shape nodes
      inside RootNode vrml tree and to State objects inside
      our ShapeStates list.
      So you must not use this octree after freeing this object.
      Also you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of property
      @link(DefaultTriangleOctree). But of course you can use it
      (and you often will) in code like

      @longCode(#  Scene.DefaultTriangleOctree := Scene.CreateTriangleOctree(...) #)

      @groupBegin }
    function CreateTriangleOctree(const ProgressTitle: string):
      TVRMLTriangleOctree; overload;
    function CreateTriangleOctree(AMaxDepth, AMaxLeafItemsCount: integer;
      const ProgressTitle: string): TVRMLTriangleOctree; overload;
    { @groupEnd }

    { Create octree containing all shape+states from our scene.
      Creates shape+state octree and inits it with our BoundingBox
      and adds all our ShapeStates.

      If ProgressTitle <> '' (and progress is not active already,
      so we avoid starting "progress bar within progress bar")
      then it uses @link(Progress) while building octree.

      Remember that such octree has a reference to our ShapeStates list.
      So you must not use this octree after freeing this object.
      Also you must rebuild such octree when this object changes.

      Note: remember that this is a function and it returns
      created octree object. It does *not* set value of property
      @link(DefaultShapeStateOctree). But of course you can use it
      (and you often will) in code like

      @longCode(#  Scene.DefaultShapeStateOctree := Scene.CreateShapeStateOctree(...) #)

      @groupBegin }
    function CreateShapeStateOctree(const ProgressTitle: string):
      TVRMLShapeStateOctree; overload;
    function CreateShapeStateOctree(AMaxDepth, AMaxLeafItemsCount: integer;
      const ProgressTitle: string): TVRMLShapeStateOctree; overload;
    { @groupEnd }

    { Notes for both DefaultTriangleOctree and
      DefaultShapeStateOctree:

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

      That said, it's very often the case that you actually want to
      create exactly one octree of each kind (one TVRMLTriangleOctree
      and one TVRMLShapeStateOctree) for each scene.
      Properties below make it easier for you.
      Basically you can use them however you like.
      They can simply serve for you as some variables inside
      TVRMLGLScene that you can use however you like,
      so that you don't have to declare two additional variables
      like SceneTriangleOctree and SceneShapeStateOctree
      each time you define variable Scene: TVRMLScene.

      Also, some methods in this class that take an octree from you
      as a parameter may have some overloaded versions that
      implicitly use octree objects stored in properties below,
      e.g. see @link(TVRMLGLScene.RenderFrustumOctree).

      This class modifies these properties in *only* one case:
      if OwnsDefaultTriangleOctree is true (default value)
      then at destruction this class calls FreeAndNil(DefaultTriangleOctree).
      Similar for DefaultShapeStateOctree.

      In any case, these properties are not managed by this object.
      E.g. these octrees are not automaticaly rebuild when you
      call ChangedAll.

      @groupBegin }
    property DefaultTriangleOctree: TVRMLTriangleOctree
      read FDefaultTriangleOctree write FDefaultTriangleOctree;

    property DefaultShapeStateOctree: TVRMLShapeStateOctree
      read FDefaultShapeStateOctree write FDefaultShapeStateOctree;

    property OwnsDefaultTriangleOctree: boolean
      read FOwnsDefaultTriangleOctree write FOwnsDefaultTriangleOctree
      default true;

    property OwnsDefaultShapeStateOctree: boolean
      read FOwnsDefaultShapeStateOctree write FOwnsDefaultShapeStateOctree
      default true;
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

    { This enumerates all viewpoint nodes (any X3DViewpointNode
      for VRML >= 2.0, PerspectiveCamera and OrthographicCamera for VRML 1.0)
      in the scene graph.
      For each such node, it calls ViewpointFunction.

      Essentially, this is a trivial wrapper over RootNode.Traverse
      that returns only TVRMLViewpointNode. }
    procedure EnumerateViewpoints(ViewpointFunction: TViewpointFunction);

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
      key/mouse sensor nodes, in the future scripts
      will also work (TODO), etc.

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
    procedure KeyUp(Key: TKey);

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
    property WorldTime: TKamTime read FWorldTime write SetWorldTime;

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

    { Call OnPostRedisplay, if assigned. }
    procedure DoPostRedisplay;

    { Call OnGeometryChanged, if assigned. }
    procedure DoGeometryChanged; virtual;

    { Binding stack of X3DBackgroundNode nodes.
      All descend from TNodeX3DBackgroundNode class. }
    property BackgroundStack: TVRMLBindableStack read FBackgroundStack;

    { Binding stack of Fog nodes.
      All descend from TNodeFog class. }
    property FogStack: TVRMLBindableStack read FFogStack;

    { Binding stack of NavigatinInfo nodes.
      All descend from TNodeNavigationInfo class. }
    property NavigationInfoStack: TVRMLBindableStack read FNavigationInfoStack;

    { Binding stack of Viewpoint  nodes.
      All descend from TVRMLViewpointNode (not necessarily from
      TNodeX3DViewpointNode, so VRML 1.0 camera nodes are also included in
      this stack.) }
    property ViewpointStack: TVRMLBindableStack read FViewpointStack;
  end;

{$undef read_interface}

implementation

uses VRMLCameraUtils, KambiStringUtils;

{$define read_implementation}
{$I macprecalcvaluereturn.inc}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}

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
    Node.EventBindTime.Send(ParentScene.WorldTime, ParentScene.WorldTime);
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
    end else
    if NodeIndex <> High then
    begin
      { Node on the stack, but not on top. So move it, as new top node. }
      SendIsBound(Top, false);
      Exchange(NodeIndex, High);
      SendIsBound(Top, true);
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
      end else
      begin
        Delete(NodeIndex);
      end;
    end;
    { set_bind = false for node already outside of the stack is ignored. }
  end;
end;

{ TVRMLScene ----------------------------------------------------------- }

constructor TVRMLScene.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
 inherited Create;
 FRootNode := ARootNode;
 FOwnsRootNode := AOwnsRootNode;

 FOwnsDefaultTriangleOctree := true;
 FOwnsDefaultShapeStateOctree := true;

 FShapeStates := TVRMLShapeStatesList.Create;

 FBackgroundStack := TVRMLBindableStack.Create(Self);
 FFogStack := TVRMLBindableStack.Create(Self);
 FNavigationInfoStack := TVRMLBindableStack.Create(Self);
 FViewpointStack := TVRMLBindableStack.Create(Self);

 ChangedAll;
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

 FreeAndNil(FBackgroundStack);
 FreeAndNil(FFogStack);
 FreeAndNil(FNavigationInfoStack);
 FreeAndNil(FViewpointStack);

 ShapeStates.FreeWithContents;

 if OwnsDefaultTriangleOctree then FreeAndNil(FDefaultTriangleOctree);
 if OwnsDefaultShapeStateOctree then FreeAndNil(FDefaultShapeStateOctree);
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
const
  { TODO: Bindable nodes should not be collected from inlined content.
    TODO: we should also omit nodes created by
    createX3DFromString() or createX3DFromURL, when scripting will be
    implemented. }
  InsideInline = false;
begin
  if Node is TVRMLGeometryNode then
  begin
    { Add shape to ShapeStates }
    ShapeStates.Add(
      TVRMLShapeState.Create(Node as TVRMLGeometryNode,
      TVRMLGraphTraverseState.CreateCopy(State)));
  end else
  if Node is TVRMLLightNode then
  begin
    { Add lights to ChangedAll_TraversedLights }
    ChangedAll_TraversedLights.AppendItem(
      (Node as TVRMLLightNode).CreateActiveLight(State));
  end else
  if not InsideInline then
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

var
  InitialState: TVRMLGraphTraverseState;
begin
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
      InitialState := TVRMLGraphTraverseState.Create;
      try
        RootNode.Traverse(InitialState, TVRMLNode,
          {$ifdef FPC_OBJFPC} @ {$endif} ChangedAll_Traverse);
      finally InitialState.Free end;

      UpdateVRML2ActiveLights;

      if ProcessEvents then
        CollectNodesForEvents;
    end;
  finally FreeAndNil(ChangedAll_TraversedLights) end;

  ScheduleGeometryChanged;
  DoPostRedisplay;
end;

procedure TVRMLScene.ChangedShapeStateFields(ShapeStateNum: integer;
  const TransformOnly: boolean);
begin
  Validities := [];
  ShapeStates[ShapeStateNum].Changed;
  DoPostRedisplay;
end;

type
  BreakTransformChangeFailed = class(TCodeBreaker);

procedure TVRMLScene.TransformChangeTraverse(
  Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
begin
  if TransformChange_Inside then
  begin
    if Node is TVRMLGeometryNode then
    begin
      ShapeStates[TransformChange_ShapeStateNum].State.AssignTransform(State);
      { TransformOnly = @true, suitable for roSeparateShapeStatesNoTransform,
        they don't have Transform compiled in display list. }
      ChangedShapeStateFields(TransformChange_ShapeStateNum, true);
      Inc(TransformChange_ShapeStateNum);
      TransformChange_AnythingChanged := true;
    end else
    if Node is TNodeX3DBackgroundNode then
    begin
      { TODO: make this work to actually change displayed background }
      if Node = BackgroundStack.Top then
        raise BreakTransformChangeFailed.Create;
    end else
    if Node is TNodeFog then
    begin
      { TODO: make this work to actually change displayed fog }
      if Node = FogStack.Top then
        raise BreakTransformChangeFailed.Create;
    end else
    if Node is TNodeNavigationInfo then
    begin
      { TODO: make this work to actually change displayed NavigationInfo.
        For now, this does nothing, since TVRMLScene doesn't deal with
        NavigationInfo. }
      { if Node = NavigationInfoStack.Top then
        raise BreakTransformChangeFailed.Create; }
    end else
    if Node is TNodeViewpoint then
    begin
      { TODO: make this work to actually change displayed Viewpoint.
        For now, this does nothing, since TVRMLScene doesn't deal with
        Viewpoint. }
      { if Node = ViewpointStack.Top then
        raise BreakTransformChangeFailed.Create; }
    end else
    if Node is TVRMLLightNode then
    begin
      raise BreakTransformChangeFailed.Create;
    end;
  end else
  begin
    if Node is TVRMLGeometryNode then
    begin
      Inc(TransformChange_ShapeStateNum);
    end else
    if Node = TransformChange_ChangingNode then
    begin
      TransformChange_Inside := true;
    end;
  end;
end;

procedure TVRMLScene.TransformChangeTraverseAfter(
  Node: TVRMLNode; State: TVRMLGraphTraverseState; ParentInfo: PTraversingInfo);
begin
  if Node = TransformChange_ChangingNode then
  begin
    TransformChange_Inside := false;
  end;
end;

procedure TVRMLScene.ChangedFields(Node: TVRMLNode;
  FieldOrEvent: TVRMLFieldOrEvent);
var
  NodeLastNodesIndex, i: integer;
  Coord: TMFVec3f;
  Field: TVRMLField;
  InitialState: TVRMLGraphTraverseState;
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

  { $define Changed_Fields_Log}
  {$ifdef Changed_Fields_Log}
  begin
    Write(Format('ChangedFields: Node %s (%s %s) at %s',
      [ Node.NodeName, Node.NodeTypeName, Node.ClassName, PointerToStr(Node) ]));
    if Field <> nil then
      Write(Format('. Field %s (%s)', [ Field.Name, Field.VRMLTypeName ]));
    Writeln;
  end;
  {$endif Changed_Fields_Log}

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

  { Ignore this ChangedFields call if you're changing some metadata
    or WorldInfo nodes, since they don't affect actual content. }

  if (Node is TNodeX3DNode) and
     (TNodeX3DNode(Node).FdMetadata = Field) then
    Exit;

  if (Node is TNodeMetadataDouble) or
     (Node is TNodeMetadataFloat) or
     (Node is TNodeMetadataInteger) or
     (Node is TNodeMetadataSet) or
     (Node is TNodeMetadataString) or
     (Node is TNodeWorldInfo) then
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
        ChangedShapeStateFields(I, false);

    { Another special thing about Coordinate node is that it changes
      actual geometry. }
    ScheduleGeometryChanged;
  end else
  if NodeLastNodesIndex <> -1 then
  begin
    { Node is part of VRML 1.0 state, so it affects ShapeStates where
      it's present on State.LastNodes list. }
    for i := 0 to ShapeStates.Count - 1 do
      if ShapeStates[i].State.LastNodes.Nodes[NodeLastNodesIndex] = Node then
        ChangedShapeStateFields(i, false);
  end else
  if Node is TNodeMaterial_2 then
  begin
    { VRML 2.0 Material affects only shapes where it's
      placed inside Appearance.material field. }
    for I := 0 to ShapeStates.Count - 1 do
      if ShapeStates[I].State.ParentShape.Material = Node then
        ChangedShapeStateFields(I, false);
  end else
  if Node is TNodeX3DTextureCoordinateNode then
  begin
    { VRML 2.0 TextureCoordinate affects only shapes where it's
      placed inside texCoord field. }
    for I := 0 to ShapeStates.Count - 1 do
      if (ShapeStates[I].GeometryNode is TNodeX3DComposedGeometryNode) and
         (TNodeX3DComposedGeometryNode(ShapeStates[I].GeometryNode).
           FdTexCoord.Value = Node) then
        ChangedShapeStateFields(I, false);
  end else
  if Node is TVRMLLightNode then
  begin
    { node jest jednym z node'ow Active*. Wiec wplynal tylko na ShapeStates
      gdzie wystepuje jako Active.

      We use CurrentActiveLights, so possibly VRML2ActiveLights here ---
      that's OK, they are valid because UpdateVRML2ActiveLights was called
      when construcing ShapeStates list. }
    for i := 0 to ShapeStates.Count-1 do
      if ShapeStates[i].State.CurrentActiveLights.
           IndexOfLightNode(TVRMLLightNode(Node)) >= 0 then
        { TransformOnly = @true, suitable for roSeparateShapeStatesNoTransform,
          they don't have lights compiled in display list. }
        ChangedShapeStateFields(i, true);
  end else
  if Node is TVRMLGeometryNode then
  begin
    { node jest Shape'm. Wiec wplynal tylko na ShapeStates gdzie wystepuje jako
      GeometryNode. }
    for i := 0 to ShapeStates.Count-1 do
      if ShapeStates[i].GeometryNode = Node then
        ChangedShapeStateFields(i, false);
    ScheduleGeometryChanged;
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
      InitialState := TVRMLGraphTraverseState.Create;
      try
        TransformChange_ShapeStateNum := 0;
        TransformChange_ChangingNode := Node;
        TransformChange_AnythingChanged := false;
        TransformChange_Inside := false;

        RootNode.Traverse(InitialState, TVRMLNode, @TransformChangeTraverse,
          nil, @TransformChangeTraverseAfter);

        if not TransformChange_AnythingChanged then
          { No need to do ScheduleGeometryChanged, not even DoPostRedisplay
            at the end. }
          Exit;

        ScheduleGeometryChanged;
      finally InitialState.Free end;
    except
      on BreakTransformChangeFailed do
      begin
        ChangedAll;
        Exit;
      end;
    end;
  end else
  begin
    { node jest czyms innym; wiec musimy zalozyc ze zmiana jego pol wplynela
      jakos na State nastepujacych po nim node'ow (a moze nawet wplynela na to
      co znajduje sie w aktywnej czesci grafu VRMLa pod niniejszym node'm -
      tak sie moglo stac gdy zmienilismy pole Switch.whichChild. ) }
    ChangedAll;
    Exit;
  end;

  DoPostRedisplay;
end;

procedure TVRMLScene.DoPostRedisplay;
begin
  if Assigned(OnPostRedisplay) then
    OnPostRedisplay(Self);
end;

procedure TVRMLScene.DoGeometryChanged;
begin
  Validities := Validities - [fvBBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvTrianglesListNotOverTriangulate, fvTrianglesListOverTriangulate,
    fvManifoldAndBorderEdges];

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self);
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

{ using triangle octree -------------------------------------------------- }

procedure TVRMLScene.AddTriangleToOctreeProgress(
  const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Triangle, State, GeometryNode, MatNum,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

function TVRMLScene.CreateTriangleOctree(const ProgressTitle: string):
  TVRMLTriangleOctree;
begin
 result := CreateTriangleOctree(
   DefTriangleOctreeMaxDepth,
   DefTriangleOctreeMaxLeafItemsCount,
   ProgressTitle);
end;

function TVRMLScene.CreateTriangleOctree(
  AMaxDepth, AMaxLeafItemsCount: integer;
  const ProgressTitle: string): TVRMLTriangleOctree;

  procedure FillOctree(AddTriProc: TNewTriangleProc);
  var i: integer;
  begin
   for i := 0 to ShapeStates.Count-1 do
    ShapeStates[i].GeometryNode.Triangulate(ShapeStates[i].State, false, AddTriProc);
  end;

begin
 result := TVRMLTriangleOctree.Create(AMaxDepth, AMaxLeafItemsCount, BoundingBox);
 try
  result.OctreeItems.AllowedCapacityOverflow := TrianglesCount(false);
  try
   if (ProgressTitle <> '') and (not Progress.Active) then
   begin
    Progress.Init(TrianglesCount(false), ProgressTitle, true);
    try
     TriangleOctreeToAdd := result;
     FillOctree({$ifdef FPC_OBJFPC} @ {$endif} AddTriangleToOctreeProgress);
    finally Progress.Fini end;
   end else
    FillOctree({$ifdef FPC_OBJFPC} @ {$endif} Result.AddItemTriangle);
  finally
   result.OctreeItems.AllowedCapacityOverflow := 4;
  end;
 except result.Free; raise end;
end;

{ using shapestate octree ---------------------------------------- }

function TVRMLScene.CreateShapeStateOctree(const ProgressTitle: string):
  TVRMLShapeStateOctree;
begin
 Result := CreateShapeStateOctree(
   DefShapeStateOctreeMaxDepth,
   DefShapeStateOctreeMaxLeafItemsCount,
   ProgressTitle);
end;

function TVRMLScene.CreateShapeStateOctree(
  AMaxDepth, AMaxLeafItemsCount: integer;
  const ProgressTitle: string): TVRMLShapeStateOctree;
var i: Integer;
begin
 Result := TVRMLShapeStateOctree.Create(AMaxDepth, AMaxLeafItemsCount,
   BoundingBox, ShapeStates);
 try

  if (ProgressTitle <> '') and (not Progress.Active) then
  begin
   Progress.Init(ShapeStates.Count, ProgressTitle, true);
   try
    for i := 0 to ShapeStates.Count - 1 do
    begin
     Result.TreeRoot.AddItem(i);
     Progress.Step;
    end;
   finally Progress.Fini end;
  end else
  begin
   for i := 0 to ShapeStates.Count - 1 do
    Result.TreeRoot.AddItem(i);
  end;

 except Result.Free; raise end;
end;

{ viewpoints ----------------------------------------------------------------- }

type
  TViewpointsSeeker = class
    ViewpointFunction: TViewpointFunction;
    procedure Seek(ANode: TVRMLNode; AState: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo);
  end;

  procedure TViewpointsSeeker.Seek(
    ANode: TVRMLNode; AState: TVRMLGraphTraverseState;
    ParentInfo: PTraversingInfo);
  begin
    ViewpointFunction(
      TVRMLViewpointNode(ANode),
      AState.Transform);
  end;

procedure TVRMLScene.EnumerateViewpoints(
  ViewpointFunction: TViewpointFunction);
var
  InitialState: TVRMLGraphTraverseState;
  Seeker: TViewpointsSeeker;
begin
  if RootNode <> nil then
  begin
    InitialState := TVRMLGraphTraverseState.Create;
    try
      Seeker := TViewpointsSeeker.Create;
      try
        Seeker.ViewpointFunction := ViewpointFunction;
        RootNode.Traverse(InitialState, TVRMLViewpointNode,
          {$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek);
      finally FreeAndNil(Seeker) end;
    finally FreeAndNil(InitialState) end;
  end;
end;

type
  BreakFirstViewpointFound = class(TCodeBreaker);

  TFirstViewpointSeeker = class
    OnlyPerspective: boolean;
    ViewpointDescription: string;
    FoundNode: TVRMLViewpointNode;
    FoundTransform: PMatrix4Single;
    procedure Seek(Node: TVRMLViewpointNode;
      const Transform: TMatrix4Single);
  end;

  procedure TFirstViewpointSeeker.Seek(
    Node: TVRMLViewpointNode;
    const Transform: TMatrix4Single);
  begin
    if ( (not OnlyPerspective) or
         (Node.CameraKind = ckPerspective) ) and
       ( (ViewpointDescription = '') or
         ( (Node is TNodeX3DViewpointNode) and
           (TNodeX3DViewpointNode(Node).FdDescription.Value = ViewpointDescription) ) ) then
    begin
      FoundTransform^ := Transform;
      FoundNode := Node;
      raise BreakFirstViewpointFound.Create;
    end;
  end;

function TVRMLScene.GetViewpointCore(
  const OnlyPerspective: boolean;
  out CamKind: TVRMLCameraKind;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  const ViewpointDescription: string): TVRMLViewpointNode;
var
  CamTransform: TMatrix4Single;
  Seeker: TFirstViewpointSeeker;
begin
  Result := nil;
  Seeker := TFirstViewpointSeeker.Create;
  try
    Seeker.OnlyPerspective := OnlyPerspective;
    Seeker.ViewpointDescription := ViewpointDescription;
    Seeker.FoundTransform := @CamTransform;
    try
      EnumerateViewpoints({$ifdef FPC_OBJFPC} @ {$endif} Seeker.Seek);
    except
      on BreakFirstViewpointFound do
      begin
        Result := Seeker.FoundNode;
      end;
    end;
  finally FreeAndNil(Seeker) end;

  if Result <> nil then
  begin
    Result.GetCameraVectors(CamTransform, CamPos, CamDir, CamUp, GravityUp);
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

procedure TVRMLScene.Collect_KeySensor(Node: TVRMLNode);
begin
  Assert(Node is TNodeKeySensor);
  KeySensorNodes.AddIfNotExists(Node);
end;

procedure TVRMLScene.Collect_TimeSensor(Node: TVRMLNode);
begin
  Assert(Node is TNodeTimeSensor);
  TimeSensorNodes.AddIfNotExists(Node);
end;

procedure TVRMLScene.Collect_MovieTexture(Node: TVRMLNode);
begin
  Assert(Node is TNodeMovieTexture);
  MovieTextureNodes.AddIfNotExists(Node);
end;

procedure TVRMLScene.Collect_ChangedFields(Node: TVRMLNode);
begin
  Node.ParentEventsProcessor := Self;
end;

procedure TVRMLScene.UnCollect_ChangedFields(Node: TVRMLNode);
begin
  Node.ParentEventsProcessor := nil;
end;

procedure TVRMLScene.CollectNodesForEvents;
begin
  KeySensorNodes.Clear;
  RootNode.EnumerateNodes(TNodeKeySensor, @Collect_KeySensor, false);

  TimeSensorNodes.Clear;
  RootNode.EnumerateNodes(TNodeTimeSensor, @Collect_TimeSensor, false);

  MovieTextureNodes.Clear;
  RootNode.EnumerateNodes(TNodeMovieTexture, @Collect_MovieTexture, false);

  RootNode.EnumerateNodes(TVRMLNode, @Collect_ChangedFields, false);
end;

procedure TVRMLScene.UnregisterProcessEvents(Node: TVRMLNode);
begin
  Node.EnumerateNodes(TVRMLNode, @UnCollect_ChangedFields, false);
end;

procedure TVRMLScene.SetProcessEvents(const Value: boolean);
begin
  if FProcessEvents <> Value then
  begin
    if Value then
    begin
      KeySensorNodes := TVRMLNodesList.Create;
      TimeSensorNodes := TVRMLNodesList.Create;
      MovieTextureNodes := TVRMLNodesList.Create;
      CollectNodesForEvents;
    end else
    begin
      FreeAndNil(KeySensorNodes);
      FreeAndNil(TimeSensorNodes);
      FreeAndNil(MovieTextureNodes);
      UnregisterProcessEvents(RootNode);
    end;
    FProcessEvents := Value;
  end;
end;

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
    for I := 0 to KeySensorNodes.Count - 1 do
    begin
      KeySensor := KeySensorNodes.Items[I] as TNodeKeySensor;
      if KeySensor.FdEnabled.Value then
      begin
        KeySensor.EventIsActive.Send(true, WorldTime);
        if KeyToActionKey(Key, ActionKey) then
          KeySensor.EventActionKeyPress.Send(ActionKey, WorldTime);
        KeySensor.EventKeyPress.Send(C, WorldTime);
        case Key of
          K_Alt: KeySensor.EventAltKey.Send(true, WorldTime);
          K_Ctrl: KeySensor.EventControlKey.Send(true, WorldTime);
          K_Shift: KeySensor.EventShiftKey.Send(true, WorldTime);
        end;
      end;
    end;
  end;
end;

procedure TVRMLScene.KeyUp(Key: TKey);
var
  I: Integer;
  KeySensor: TNodeKeySensor;
  ActionKey: Integer;
begin
  if ProcessEvents then
  begin
    for I := 0 to KeySensorNodes.Count - 1 do
    begin
      KeySensor := KeySensorNodes.Items[I] as TNodeKeySensor;
      if KeySensor.FdEnabled.Value then
      begin
        KeySensor.EventIsActive.Send(false, WorldTime);
        if KeyToActionKey(Key, ActionKey) then
          KeySensor.EventActionKeyRelease.Send(ActionKey, WorldTime);
        { TODO: KeySensor.EventKeyRelease.Send(C, WorldTime) would be nice here }
        case Key of
          K_Alt: KeySensor.EventAltKey.Send(false, WorldTime);
          K_Ctrl: KeySensor.EventControlKey.Send(false, WorldTime);
          K_Shift: KeySensor.EventShiftKey.Send(false, WorldTime);
        end;
      end;
    end;
  end;
end;

{ WorldTime stuff ------------------------------------------------------------ }

procedure TVRMLScene.InternalSetWorldTime(
  const NewValue, TimeIncrease: TKamTime);
var
  SomethingChanged: boolean;
  I: Integer;
begin
  if ProcessEvents then
  begin
    BeginGeometryChangedSchedule;
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
      EndGeometryChangedSchedule;
    end;
  end;

  FWorldTime := NewValue;
end;

procedure TVRMLScene.SetWorldTime(const NewValue: TKamTime);
var
  TimeIncrease: TKamTime;
begin
  TimeIncrease := NewValue - FWorldTime;
  if TimeIncrease > 0 then
    InternalSetWorldTime(NewValue, TimeIncrease);
end;

procedure TVRMLScene.IncreaseWorldTime(const TimeIncrease: TKamTime);
begin
  if TimeIncrease > 0 then
    InternalSetWorldTime(FWorldTime + TimeIncrease, TimeIncrease);
end;

procedure TVRMLScene.ResetRoutesLastEventTime(Node: TVRMLNode);
var
  I: Integer;
begin
  for I := 0 to Node.Routes.Count - 1 do
    Node.Routes[I].ResetLastEventTime;
end;

procedure TVRMLScene.ResetWorldTime(const NewValue: TKamTime);
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes(@ResetRoutesLastEventTime, false);
  InternalSetWorldTime(NewValue, 0);
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

end.
