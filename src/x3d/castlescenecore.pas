{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and processing of scenes (TCastleSceneCore). }
unit CastleSceneCore;

{$I castleconf.inc}
{$I octreeconf.inc}
{$modeswitch nestedprocvars}{$H+}

interface

uses SysUtils, Classes, Generics.Collections, Contnrs, Kraft,
  CastleVectors, CastleBoxes, CastleTriangles, X3DFields, X3DNodes,
  CastleClassUtils, CastleUtils, CastleShapes, CastleInternalTriangleOctree,
  CastleProgress, CastleInternalOctree, CastleInternalShapeOctree,
  CastleKeysMouse, X3DTime, CastleCameras, X3DTriangles,
  CastleTransform, CastleInternalShadowMaps, CastleProjection;

type
  { Internal helper type for TCastleSceneCore.
    @exclude }
  TSceneValidity = (fvLocalBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver,
    fvMainLightForShadows,
    fvShapesActiveCount,
    fvShapesActiveVisibleCount);

  { @exclude }
  TSceneValidities = set of TSceneValidity;

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

    { Unloads the background images allocated in VRML/X3D Background nodes.
      The same comments as for frTextureDataInNodes apply. }
    frBackgroundImageInNodes,

    { Free data created for shadow volumes processing.
      This frees some memory, but trying to render shadow volumes will
      need to recreate this data again (at least for all active shapes).
      So freeing this is useful only if you have used shadow volumes,
      but you will not need to render with shadow volumes anymore
      (for some time). }
    frShadowVolume
  );

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

  TBackgroundStack = class(TX3DBindableStack)
  public
    function Top: TAbstractBackgroundNode;
    procedure PushIfEmpty(Node: TAbstractBackgroundNode; SendEvents: boolean);
  end;

  TFogStack = class(TX3DBindableStack)
  public
    function Top: TFogNode;
    procedure PushIfEmpty(Node: TFogNode; SendEvents: boolean);
  end;

  TNavigationInfoStack = class(TX3DBindableStack)
  protected
    procedure DoBoundChanged; override;
  public
    function Top: TNavigationInfoNode;
    procedure PushIfEmpty(Node: TNavigationInfoNode; SendEvents: boolean);
  end;

  TViewpointStack = class(TX3DBindableStack)
  protected
    procedure DoBoundChanged; override;
  public
    function Top: TAbstractViewpointNode;
    procedure PushIfEmpty(Node: TAbstractViewpointNode; SendEvents: boolean);
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
  TGeneratedTextureList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TGeneratedTexture>)
  public
    function IndexOfTextureNode(TextureNode: TX3DNode): Integer;
    function FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
    function AddShapeTexture(Shape: TShape; Tex: TAbstractTextureNode): Pointer;
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
  TProximitySensorInstanceList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TProximitySensorInstance>;
  { @exclude }
  TVisibilitySensorInstanceList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TVisibilitySensorInstance>;
  { @exclude }
  TVisibilitySensors = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TVisibilitySensorNode, TVisibilitySensorInstanceList>)
  public
    destructor Destroy; override;
    { Remove everything are released owned stuff.
      We own TVisibilitySensorInstanceList instances on our Data list.
      We do not own TVisibilitySensorNode (our Keys list). }
    procedure Clear; reintroduce;
  end;
  TTimeDependentHandlerList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TInternalTimeDependentHandler>)
    procedure AddIfNotExists(const Item: TInternalTimeDependentHandler);
  end;

  TCompiledScriptHandler = procedure (
    Value: TX3DField; const Time: TX3DTime) of object;

  { @exclude }
  TCompiledScriptHandlerInfo = record
    Handler: TCompiledScriptHandler;
    Name: string;
  end;
  PCompiledScriptHandlerInfo = ^TCompiledScriptHandlerInfo;
  TCompiledScriptHandlerInfoList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TCompiledScriptHandlerInfo>;

  { Possible spatial structures that may be managed by TCastleSceneCore,
    see @link(TCastleSceneCore.Spatial). }
  TSceneSpatialStructure = (
    { Create @italic(and keep up-to-date) a spatial structure
      containing all visible shapes.
      It's useful for "frustum culling", it will be automatically
      used by TCastleScene rendering to speed it up.

      This octree will be automatically updated on dynamic scenes
      (when e.g. animation moves some shape by changing it's transformation). }
    ssRendering,

    { Create @italic(and keep up-to-date) a spatial structure
      containing all collidable shapes (and then reaching
      into collidable triangles for a specifc shape).
      It is automatically used by the XxxCollision methods in this class.

      This is actually a hierarchy of octrees: scene is partitioned
      first into Shapes (each instance of VRML/X3D geometry node),
      and then each Shape has an octree of triangles inside.

      This octree is useful for all kinds of collision detection.
      Compared to ssStaticCollisions, it is (very slightly on typical scenes)
      less efficient, but it can also be updated very fast.
      For example, merely transforming some Shape means that only
      one item needs to be moved in the top-level shape tree.
      So this is the most important structure for collision detection on
      dynamic scenes. }
    ssDynamicCollisions,

    { Create a spatial structure containing all visible triangles, suitable only
      for scenes that stay static.

      It's primarily use is for ray-tracers, that make a lot of collision queries
      to the same scene in the same time. When rendering using OpenGL,
      this has no use currently.

      This structure is not updated on scene changes. In fact, the scene
      contents cannot change when this octree is created --- as this octree
      keeps pointers to some states that may become invalid in dynamic scenes. }
    ssVisibleTriangles,

    { Create a spatial structure containing all collidable triangles,
      @bold(only for scenes that never change).

      It may be useful if you're absolutely sure that you have a static scene
      (nothing changes, i.e. ProcessEvents = @false and you never
      make any change to X3D nodes from code) and
      you want to have collision detection with the scene.

      For dynamic scenes, using this is a bad idea as
      this octree is not updated on scene changes. In fact, the scene
      contents cannot change when this octree is created --- as this octree
      keeps pointers to some states that may become invalid in dynamic scenes.
      Use ssDynamicCollisions for dynamic scenes. }
    ssStaticCollisions);
  TSceneSpatialStructures = set of TSceneSpatialStructure;

  TGeometryChange =
  ( { Everything changed. All octrees must be rebuild, old State pointers
      may be invalid.

      Every ChangedAll call does this.
      ChangedAll must take into account that everything could change.
      Note that ChangedAll traverses the VRML/X3D graph again,
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
      recalculating Manifold/BorderEdges).

      In this case, DoGeometryChanged parameter LocalGeometryShape is non-nil
      and indicated the (only) shape that changed.

      @groupBegin }
    gcLocalGeometryChanged,
    gcLocalGeometryChangedCoord,
    { @groupEnd }

    { What is considered "active" shapes changed. Like after Switch.whichChoice
      change. }
    gcActiveShapesChanged);

  { Looping mode to use with TCastleSceneCore.PlayAnimation. }
  TPlayAnimationLooping = (
    { Use current TimeSensor.Loop value to determine whether animation
      should loop. Suitable when X3D model already has sensible "TimeSensor.loop"
      values. }
    paDefault,
    { Set TimeSensor.Loop to be @true, to force looping. }
    paLooping,
    { Set TimeSensor.Loop to be @false, to force not looping. }
    paNotLooping
  ) deprecated 'use PlayAnimation with "Loop: boolean" parameter instead of TPlayAnimationLooping';

  TStopAnimationEvent = procedure (const Scene: TCastleSceneCore;
    const Animation: TTimeSensorNode) of object;

  { Parameters to use when playing animation,
    see @link(TCastleSceneCore.PlayAnimation). }
  TPlayAnimationParameters = class
    { Animation name.
      You have to set at least this field, otherwise calling
      @link(TCastleSceneCore.PlayAnimation) with this is useless. }
    Name: string;

    { Should we play in a loop, default @false which means to play just once. }
    Loop: boolean;

    { Does animation play forward, default @true. }
    Forward: boolean;

    { Notification when the animation finished playing,
      which happens if the animation was non-looping and it reached the end,
      or another animation was started by @link(TCastleSceneCore.PlayAnimation).

      This will never fire if the animation did not exist
      (@link(TCastleSceneCore.PlayAnimation) returned @false).
      It also may never fire if the scene was destroyed or rebuilt
      (@link(TCastleSceneCore.ChangedAll)) during the animation playing.

      Listening on this notification is usually simpler
      and more reliable then explicitly adding a notification
      to TTimeSensorNode.EventIsActive, e.g. by
      @code(CurrentAnimation.EventIsActive.AddNotification).
      It avoids corner cases when PlayAnimation restarts playing the current
      animation. This notification will happen when the animation that you
      caused (by the call to @link(TCastleSceneCore.PlayAnimation)) stops,
      not at other times. }
    StopNotification: TStopAnimationEvent;

    { Time, in seconds, when this animation fades-in (and the previous
      animation, if any, fades-out).
      See https://castle-engine.io/wp/2018/03/21/animation-blending/ }
    TransitionDuration: TFloatTime;

    { Start new animation at given moment in animation.
      Allows to start animation from the middle, not necessarily from the start.

      Note that animation blending (TransitionDuration) starts
      from the moment you called the PlayAnimation, i.e. from InitialTime,
      not from 0. In other words, animation will enter smoothly (cross-fade),
      regardless of InitialTime. }
    InitialTime: TFloatTime;

    constructor Create;
  end;

  { Loading and processing of a scene.
    Almost everything visible in your game will be an instance of
    @link(TCastleScene), which is a descendant of this class that adds rendering
    capabilities.

    This class provides a lot of functionality, like loading of the scene
    (@link(Load) method), animating it, performing collisions with the scene,
    and calculating things (like @link(LocalBoundingBox)).

    The actual scene information (visible and collidable things) is inside
    X3D nodes graph contained within the @link(RootNode).
    During the lifetime of the scene, this X3D graph can change
    (e.g. because of animations), and you can always change it by code
    too. E.g. you can freely change @link(TTransformNode.Translation)
    or add children by @link(TAbstractX3DGroupingNode.AddChildren RootNode.AddChildren).
    The X3D nodes graph works like a DOM tree for rendering HTML documents:
    it's typically initialized from a file (3D model), but during
    the game execution it is dynamic, always changing.

    This class takes care of performing the X3D events
    and routes mechanism (if @link(ProcessEvents) is @true).
    This is what allows X3D graph to be animated, or even be interactive
    (respond to user actions).
    For a simple way to play animations, use the @link(PlayAnimation) method.

    This class maintains a @link(Shapes) tree is
    always synchronized with the X3D nodes tree in @link(RootNode).
    The @link(Shapes) tree provides a little simple view at the scene,
    sometimes easier (or faster) to iterate over.

    Many results are cached, so querying them multiple times is fast,
    if the scene does not change (in a significant way) between each query.
    So you can query information like @link(LocalBoundingBox),
    @link(BoundingBox), @link(VerticesCount)... as often as you want to. }
  TCastleSceneCore = class(TX3DEventsEngine)
  private
    FOwnsRootNode: boolean;
    FShapes: TShapeTree;
    FRootNode: TX3DRootNode;
    FOnPointingDeviceSensorsChange: TNotifyEvent;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FURL: string;
    FStatic: boolean;
    FShadowMaps: boolean;
    FShadowMapsDefaultSize: Cardinal;
    ScheduleHeadlightOnFromNavigationInfoInChangedAll: boolean;
    LastUpdateFrameId: Int64;
    { All CameraFromViewpoint calls will disable smooth (animated)
      transitions when this is true.
      This is set to true by @link(Load),
      when ChangedAll is scheduled, so that first camera bindings
      (in particular after LoadAnchor, when the already existing camera
      is changed) will have immediate transitions in newly loaded file.
      ChangedAll sets this back to false at the end. }
    ForceTeleportTransitions: boolean;

    WatchForTransitionComplete: boolean;

    { Humanoids on which we should call AnimateSkin.
      We don't do AnimateSkin immediately, as it would force slowdown
      when many joints are changed at once (e.g. many joints, and each one
      animated with it's own OrientationInterpolator). }
    ScheduledHumanoidAnimateSkin: TX3DNodeList;

    { NewPlayingAnimationXxx describe scheduled animation to change.
      They are set by PlayAnimation, and used by UpdateNewPlayingAnimation. }
    NewPlayingAnimationUse: boolean;
    NewPlayingAnimationNode: TTimeSensorNode;
    NewPlayingAnimationLoop: boolean;
    NewPlayingAnimationForward: boolean;
    NewPlayingAnimationStopNotification: TStopAnimationEvent;
    NewPlayingAnimationTransitionDuration: TFloatTime;
    NewPlayingAnimationInitialTime: TFloatTime;

    { PlayingAnimationXxx are changed in UpdateNewPlayingAnimation,
      when the new animation actually starts
      (the X3D TimeSensor gets event to start). }
    PlayingAnimationNode: TTimeSensorNode;
    PlayingAnimationStartTime: TFloatTime;
    PlayingAnimationStopNotification: TStopAnimationEvent;
    PlayingAnimationTransitionDuration: TFloatTime;

    PreviousPlayingAnimation: TTimeSensorNode;
    PreviousPlayingAnimationLoop: boolean;
    PreviousPlayingAnimationForward: boolean;
    PreviousPlayingAnimationTimeInAnimation: TFloatTime;

    PreviousPartialAffectedFields: TX3DFieldList;

    FCurrentAnimation: TTimeSensorNode;
    FAnimationPrefix: string;
    FAnimationsList: TStrings;
    FTimeAtLoad: TFloatTime;
    ForceImmediateProcessing: Integer;

    { When this is non-empty, then the transformation change happened,
      and should be processed (for the whole X3D graph inside RootNode).
      This must include then chTransform field, may also include other changes
      (this will be passed to shapes affected).
      Used only when OptimizeExtensiveTransformations. }
    TransformationDirty: TX3DChanges;

    { This always holds pointers to all TShapeTreeLOD instances in Shapes
      tree. }
    ShapeLODs: TObjectList;

    { Perform animation fade-in and fade-out by initializing
      TInternalTimeDependentHandler.PartialSend before
      TInternalTimeDependentHandler.SetTime. }
    procedure PartialSendBegin(const TimeHandler: TInternalTimeDependentHandler);

    { Finalize what PartialSendBegin started,
      after TInternalTimeDependentHandler.SetTime was called. }
    procedure PartialSendEnd(const TimeHandler: TInternalTimeDependentHandler);

    { Call this if during this frame, TimeHandler.PartialSend always remained @nil. }
    procedure NoPartialSend;

    { Recalculate and update LODTree.Level.
      Also sends level_changed when needed.
      Call only when ProcessEvents. }
    procedure UpdateLODLevel(const LODTree: TShapeTreeLOD;
      const CameraLocalPosition: TVector3);

    procedure SetURL(const AValue: string);
    procedure SetStatic(const Value: boolean);
    procedure SetShadowMaps(const Value: boolean);
    procedure SetShadowMapsDefaultSize(const Value: Cardinal);

    { Handle change of transformation of ITransformNode node.
      TransformNode must not be @nil here.
      Changes must include chTransform, may also include other changes
      (this will be passed to shapes affected). }
    procedure TransformationChanged(TransformNode: TX3DNode;
      Instances: TShapeTreeList; const Changes: TX3DChanges);
    { Like TransformationChanged, but specialized for TransformNode = RootNode. }
    procedure RootTransformationChanged(const Changes: TX3DChanges);

    function LocalBoundingVolumeMoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D): boolean;

    procedure SetRootNode(const Value: TX3DRootNode);

    { Always assigned to PlayingAnimationNode.EventIsActive. }
    procedure PlayingAnimationIsActive(
      Event: TX3DEvent; Value: TX3DField; const ATime: TX3DTime);
  private
    { For all ITransformNode, except Billboard nodes }
    TransformInstancesList: TTransformInstancesList;
    { For all Billboard nodes }
    BillboardInstancesList: TTransformInstancesList;

    FGlobalLights: TLightInstancesList;

    FLocalBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: array [boolean] of Cardinal;
    Validities: TSceneValidities;
    function CalculateLocalBoundingBox: TBox3D;
    function CalculateVerticesCount(OverTriangulate: boolean): Cardinal;
    function CalculateTrianglesCount(OverTriangulate: boolean): Cardinal;
  private
  type
    TAbstractViewpointNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TAbstractViewpointNode>;
  var
    FShapesActiveCount: Cardinal;
    FShapesActiveVisibleCount: Cardinal;
    { For easier access to list of viewpoints in the scene }
    FViewpointsArray: TAbstractViewpointNodeList;

    function GetViewpointCore(
      const OnlyPerspective: boolean;
      out ProjectionType: TProjectionType;
      out CamPos, CamDir, CamUp, GravityUp: TVector3;
      const ViewpointDescription: string):
      TAbstractViewpointNode;
  private
    procedure FreeResources_UnloadTextureData(Node: TX3DNode);
    procedure FreeResources_UnloadTexture3DData(Node: TX3DNode);
    procedure FontChanged_TextNode(Node: TX3DNode);
    procedure FontChanged_AsciiTextNode_1(Node: TX3DNode);
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
    FTimeNow: TX3DTime;

    { Internal procedure that handles Time changes. }
    procedure InternalSetTime(
      const NewValue: TFloatTime; const TimeIncrease: TFloatTime; const ResetTime: boolean);

    procedure ResetLastEventTime(Node: TX3DNode);
  private
    { Bindable nodes helpers }
    FBackgroundStack: TBackgroundStack;
    FFogStack: TFogStack;
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
    FPointingDeviceOverPoint: TVector3;
    FPointingDeviceActive: boolean;
    FPointingDeviceActiveSensors: TX3DNodeList;
  private
    { Call this when the ProximitySensor instance changed (either the box or
      it's transformation) or when camera position changed (by user actions
      or animating the Viewpoint). }
    procedure ProximitySensorUpdate(const PSI: TProximitySensorInstance;
      const CameraVectors: TCameraVectors);
  private
    FCompiledScriptHandlers: TCompiledScriptHandlerInfoList;

    function OverrideOctreeLimits(
      const BaseLimits: TOctreeLimits;
      const OP: TSceneOctreeProperties): TOctreeLimits;

    { Create octree containing all triangles or shapes from our scene.
      Create octree, inits it with our LocalBoundingBox
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
      inside RootNode VRML/X3D tree and to State objects inside
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
      const APosition: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  private
    FTriangleOctreeLimits: TOctreeLimits;
    FTriangleOctreeProgressTitle: string;

    FShapeOctreeLimits: TOctreeLimits;
    FShapeOctreeProgressTitle: string;

    FOctreeRendering: TShapeOctree;
    FOctreeDynamicCollisions: TShapeOctree;
    FOctreeVisibleTriangles: TTriangleOctree;
    FOctreeStaticCollisions: TTriangleOctree;
    FSpatial: TSceneSpatialStructures;

    { Properties of created triangle octrees.
      See TriangleOctree unit comments for description.

      Default value comes from DefTriangleOctreeLimits.

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [https://castle-engine.io/x3d_extensions.php#section_ext_octree_properties].

      @groupBegin }
    function TriangleOctreeLimits: POctreeLimits;

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
      [https://castle-engine.io/x3d_extensions.php#section_ext_octree_properties]. }
    function ShapeOctreeLimits: POctreeLimits;

    procedure SetSpatial(const Value: TSceneSpatialStructures);
  private
    FMainLightForShadowsExists: boolean;
    FMainLightForShadows: TVector4;
    FMainLightForShadowsNode: TAbstractLightNode;
    FMainLightForShadowsTransform: TMatrix4;
    function SearchMainLightForShadows(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
    { Based on FMainLightForShadowsNode and FMainLightForShadowsTransform,
      calculate FMainLightForShadows (position). }
    procedure CalculateMainLightForShadowsPosition;
    procedure ValidateMainLightForShadows;
  private
    FHeadlightOn: boolean;
    FOnHeadlightOnChanged: TNotifyEvent;
    FAnimateOnlyWhenVisible: boolean;
    FAnimateGatheredTime: TFloatTime;
    FAnimateSkipTicks: Cardinal;
    AnimateSkipNextTicks: Cardinal;

    procedure SetAnimateSkipTicks(const Value: Cardinal);

    procedure SetHeadlightOn(const Value: boolean);

    { Get camera position in current scene local coordinates.
      This is calculated every time now (in the future it may be optimized
      to recalculate only when WorldTransform changed, e.g. using
      FWorldTransformAndInverseId). }
    function GetCameraLocal(out CameraVectors: TCameraVectors): boolean;
    function GetCameraLocal(out CameraLocalPosition: TVector3): boolean;

    { Update various X3D nodes using the camera position/direction/up,
      like LOD, Billboard, ProximitySensor. }
    procedure UpdateCameraEvents;
  protected
    { List of TScreenEffectNode nodes, collected by ChangedAll. }
    ScreenEffectNodes: TX3DNodeList;

    { Is the scene visible currently. Descendants may set this to @true
      during @link(TCastleTransform.LocalRender). }
    IsVisibleNow: boolean;

    GeneratedTextures: TGeneratedTextureList;

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

    procedure ChangedTransform; override;
    procedure ChangeWorld(const Value: TSceneManagerWorld); override;

    { Called after PointingDeviceSensors or
      PointingDeviceActiveSensors lists (possibly) changed.

      In this class, DoPointingDeviceSensorsChange updates Cursor and calls
      OnPointingDeviceSensorsChange. }
    procedure DoPointingDeviceSensorsChange; virtual;

    procedure ExecuteCompiledScript(const HandlerName: string; ReceivedValue: TX3DField); override;

    function LocalHeightCollision(const APosition, GravityUp: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean; override;
    function LocalMoveCollision(
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;
    function LocalMoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;
    function LocalSegmentCollision(const Pos1, Pos2: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; override;
    function LocalSphereCollision(const Pos: TVector3; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;
    function LocalSphereCollision2D(const Pos: TVector2; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const Details: TCollisionDetails): boolean; override;
    function LocalPointCollision2D(const Point: TVector2;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;
    function LocalBoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; override;
    function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;

    procedure LocalRender(const Params: TRenderParams); override;
  public
    { Nonzero value prevents rendering of this scene,
      and generally means that our state isn't complete.
      This is useful if we're in the middle of some operation
      (like ChangedAll call or octree creation).

      Some callbacks *may* be called during such time: namely,
      the progress call (e.g. done during constructing octrees).
      As these callbacks may try to e.g. render our scene (which should
      not be done on the dirty state), we have to protect ourselves
      using this variable (e.g. Render routines will exit immediately
      when InternalDirty <> 0).

      Note: in the future, we could replace this by just Enable/Disable
      feature on T3D. But it's not so trivial now, as Enable/Disable
      makes even *too much* things non-existing, e.g. GetCollides
      may return false, LocalBoundingBox may be empty etc.

      @exclude }
    InternalDirty: Cardinal;

    const
      DefaultShadowMapsDefaultSize = 256;

    constructor Create(AOwner: TComponent); override;

    { Load new 3D model (from VRML/X3D node tree).
      This replaces RootNode with new value.

      If AResetTime, we will also do ResetTimeAtLoad,
      changing @link(Time) --- this is usually what you want when you
      really load a new world. }
    procedure Load(ARootNode: TX3DRootNode; AOwnsRootNode: boolean;
      const AResetTime: boolean = true);

    { Load the 3D model from given URL.

      Model is loaded by Load3D, so this supports all
      3D model formats that Load3D handles
      (VRML, X3D, Wavefront OBJ, 3DS, Collada and more).

      URL is downloaded using CastleDownload unit.
      If you all you care about is loading normal files, then just pass
      a normal filename (absolute or relative to the current directory)
      as the URL parameter.

      @param(AllowStdIn If AllowStdIn and AURL = '-' then we will load
        a file from standard input (StdInStream), using current working directory
        as BaseUrl (to resolve relative URLs from the file).
        Currently, this limits the file to be VRML/X3D.) }
    procedure Load(const AURL: string; AllowStdIn: boolean = false;
      const AResetTime: boolean = true);

    { Save the current 3D model (X3D nodes graph) to the given file (URL).

      The X3D encoding is automatically guessed from the URL extension.
      By default it is XML, and we suggest using the @code(.x3d) extension.
      If you specify an extension indicating "classic encoding",
      we will use such encoding (use @code(.x3dv) for X3D
      in classic encoding, or @code(.wrl) for older VRML content).

      The file may also be automatically gzip compressed if extension indicates it.
      Use @code(.x3d.gz) to indicated XML compressed with gzip,
      or use @code(.x3dv.gz) or @code(.wrl.gz) to indicate classic encoding
      compressed with gzip.

      The @link(URL) property is also changed. }
    procedure Save(const AURL: string);

    destructor Destroy; override;

    { Simple (usually very flat) tree of shapes within this VRML/X3D scene.

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

    { Calculate the number of triangls and vertexes of all
      shapa states. For detailed specification of what these functions
      do (and what does OverTriangulate mean) see appropriate
      TAbstractGeometryNode methods. Here, we just sum their results
      for all shapes.
      @groupBegin }
    function VerticesCount(OverTriangulate: boolean): Cardinal;
    function TrianglesCount(OverTriangulate: boolean): Cardinal;
    { @groupEnd }

    { Helper functions for accessing viewpoints defined in the scene.
      @groupBegin }
    function ViewpointsCount: Cardinal;
    function GetViewpointName(Idx: integer): string;
    procedure MoveToViewpoint(Idx: integer; Animated: boolean = true);
    procedure AddViewpointFromCamera(ACamera: TCamera; AName: string);
    { @groupEnd }

    { Methods to notify this class about changes to the underlying RootNode
      graph. Since this class caches some things, it has to be notified
      when you manually change something within RootNode graph. }

    { Release all internal associations with your VRML/X3D nodes.
      In particular, this will release OpenGL resources connected to your nodes.
      You should always call this @italic(before) you (may) free some nodes in
      @link(RootNode).

      @italic(You have to call ChangedAll or Load at sometime
      after BeforeNodesFree, and before you try actual rendering, events etc.)
      Otherwise some stuff may not get recalculated.

      The InternalChangedAll parameter is for internal use.
      It is set to @true when ChangedAll
      calls this method at the beginning of it's work, and means that nothing is freed,
      and we only require necessary cleanup at the beginning of ChangedAll.
      This way ChangedAll (when it wasn't preceeded by explicit
      BeforeNodesFree(false)) produces events from stacks CheckForDeletedNodes. }
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); override;

    { Call Node.FreeRemovingFromAllParents, making sure that changes
      to our VRML/X3D node graph are allowed. This makes sure we call
      BeforeNodesFree befor freeing, and ChangedAll afterwards.

      This avoids a common pitfall with relying on TShape or such
      existence between BeforeNodesFree and ChangedAll.
      BeforeNodesFree may free all our TShape instances, so if you
      want to free TX3DNode from our graph --- you typically want to
      get this TX3DNode instance *before* calling BeforeNodesFree.
      Using this method to free the node ensures this. }
    procedure NodeFreeRemovingFromAllParents(Node: TX3DNode);

    { Remove the shape node from the scene.
      In case of VRML 1.0 / Inventor, when the Shape doesn't have a node,
      we remove the geometry node. }
    procedure RemoveShape(Shape: TShape);

    { Notify scene that potentially everything changed
      in the VRML/X3D graph. This includes adding/removal of some nodes within
      RootNode graph and changing their fields' values.
      (Before freeing the nodes, remember to also call BeforeNodesFree
      earlier.)

      @bold(You usually never need to call this method explicitly.
      It's called by engine when necesssary.)
      However, you need to call it yourself if you change the X3D graph directly
      through nodes' @code(FdXxx) fields,
      and you don't want to call for some reason @link(TX3DField FdXxx.Changed).

      ChangedAll causes recalculation of all things dependent on RootNode,
      so it's very costly to call this. Avoid calling this.
      When you change a simple field, like by @code(FdXxx.Value := ...),
      you should rather call @code(FdXxx.Changed), and it may do something
      much faster than rebuilding the scene.

      @italic(Descendant implementors notes:) ChangedAll is virtual,
      when overriding it remember that it's
      called by constructor of this class, so you can put a lot of your
      initialization there (instead of in the constructor).

      ChangedAll calls BeforeNodesFree(true) first, for safety (and TGLShape
      actually depends on it, see implementation comments). }
    procedure ChangedAll; override;

    { Notify scene that you changed the value of given field.
      @bold(This method is internal, it should be used only by TX3DField
      implementation.)

      This does relatively intelligent discovery what could be possibly
      affected by this field, and updates / invalidates
      internal information where needed.

      Every change you do directly to the VRML/X3D nodes inside RootNode
      (using directly the methods of TX3DNode or TX3DField) must be
      reported to scene by calling this method. This includes changes
      to the inactive graph part (e.g. in inactive Switch child),
      because our shapes have to track it also.
      Changes to TShape.State.VRML1State (these nodes may come
      from VRML1DefaultState) should also be reported here.
      In fact, you can even notify this scene about changes to fields
      that don't belong to our RootNode --- nothing bad will happen.
      We always try to intelligently
      detect what this change implicates for this VRML/X3D scene.

      @exclude }
    procedure InternalChangedField(Field: TX3DField); override;

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
      This consists of a few lines, separated by newlines.
      Last line also ends with CastleUtils.NL. }
    function Info(
      ATriangleVerticesCounts,
      ABoundingBox: boolean;
      AManifoldAndBorderEdges: boolean): string; deprecated 'do not use this, better to construct a summary string yourself';

    function InfoTriangleVerticesCounts: string;
      deprecated 'better to construct a string yourself, use TrianglesCount, VerticesCount';
    function InfoBoundingBox: string;
      deprecated 'better to construct a string yourself, use BoundingBox.ToString';
    function InfoManifoldAndBorderEdges: string;
      deprecated 'better to construct a string yourself, use EdgesCount';

    { Edges count in the scene, for information purposes. }
    procedure EdgesCount(out ManifoldEdges, BorderEdges: Cardinal);

    { Actual VRML/X3D graph defining this scene.

      It is allowed to change contents of RootNode. Just make sure
      the scene is notified about these changes.
      The simplest option is to use simple properties of nodes
      (not the ones starting with @code(Fd...) prefix), when possible.
      Then everything is notified automatically.
      If you must use fields through the @code(FdXxx) properties,
      then assign them using @link(TX3DField.Send),
      or (if you need to assign field
      values directly, like @code(TSFVec3f.Value := ...))
      call @link(TX3DField.Changed).

      It is also allowed to change the value of RootNode
      and even to set RootNode to @nil.
      Changing RootNode allows you to load
      and unload whole new VRML/X3D graph (for example from some 3D file)
      whenever you want, and keep the same TCastleSceneCore instance
      (with the same rendering settings and such). }
    property RootNode: TX3DRootNode read FRootNode write SetRootNode;

    { If @true, RootNode will be freed by destructor of this class. }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode default true;

    { A spatial structure containing all visible shapes.
      Add ssRendering to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use SceneManager
      (like @link(TCastleSceneManager) or @link(TCastle2DSceneManager))
      and then use @code(SceneManager.Items.WorldXxxCollision) methods like
      @link(TSceneManagerWorld.WorldRay SceneManager.Items.WorldRay) or
      @link(TSceneManagerWorld.WorldSphereCollision SceneManager.Items.WorldSphereCollision).)

      Note that when VRML/X3D scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects. }
    function InternalOctreeRendering: TShapeOctree;

    { A spatial structure containing all collidable shapes.
      Add ssDynamicCollisions to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use SceneManager
      (like @link(TCastleSceneManager) or @link(TCastle2DSceneManager))
      and then use @code(SceneManager.Items.WorldXxxCollision) methods like
      @link(TSceneManagerWorld.WorldRay SceneManager.Items.WorldRay) or
      @link(TSceneManagerWorld.WorldSphereCollision SceneManager.Items.WorldSphereCollision).)

      You can use @link(InternalOctreeCollisions) to get either
      @link(InternalOctreeDynamicCollisions) or
      @link(InternalOctreeStaticCollisions), whichever is available.

      Note that when VRML/X3D scene contains Collision nodes, this octree
      contains the @italic(collidable (not necessarily rendered)) objects.

      TODO: Temporarily, this is updated simply by rebuilding. }
    function InternalOctreeDynamicCollisions: TShapeOctree;

    { A spatial structure containing all visible triangles, suitable only
      for scenes that stay static.
      Add ssVisibleTriangles to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use SceneManager
      (like @link(TCastleSceneManager) or @link(TCastle2DSceneManager))
      and then use @code(SceneManager.Items.WorldXxxCollision) methods like
      @link(TSceneManagerWorld.WorldRay SceneManager.Items.WorldRay) or
      @link(TSceneManagerWorld.WorldSphereCollision SceneManager.Items.WorldSphereCollision).)

      Note that when VRML/X3D scene contains X3D Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects. }
    function InternalOctreeVisibleTriangles: TTriangleOctree;

    { A spatial structure containing all collidable triangles.
      Add ssStaticCollisions to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use SceneManager
      (like @link(TCastleSceneManager) or @link(TCastle2DSceneManager))
      and then use @code(SceneManager.Items.WorldXxxCollision) methods like
      @link(TSceneManagerWorld.WorldRay SceneManager.Items.WorldRay) or
      @link(TSceneManagerWorld.WorldSphereCollision SceneManager.Items.WorldSphereCollision).)

      It is automatically used by the XxxCollision methods in this class,
      if exists, unless OctreeDynamicCollisions exists.

      Note that you can use @link(InternalOctreeCollisions) to get either
      @link(InternalOctreeDynamicCollisions)
      or @link(InternalOctreeStaticCollisions), whichever is available. }
    function InternalOctreeStaticCollisions: TTriangleOctree;

    { Octree for collisions. This returns either
      @link(InternalOctreeStaticCollisions) or
      @link(InternalOctreeDynamicCollisions), whichever is available (or @nil if none).
      Be sure to add ssDynamicCollisions or ssStaticCollisions to have
      this available.

      @bold(You should not usually use this directly.
      Instead use SceneManager
      (like @link(TCastleSceneManager) or @link(TCastle2DSceneManager))
      and then use @code(SceneManager.Items.WorldXxxCollision) methods like
      @link(TSceneManagerWorld.WorldRay SceneManager.Items.WorldRay) or
      @link(TSceneManagerWorld.WorldSphereCollision SceneManager.Items.WorldSphereCollision).) }
    function InternalOctreeCollisions: TBaseTrianglesOctree;

    function UseInternalOctreeCollisions: boolean;

    { Progress title shown during spatial structure creation
      (through TProgress.Title). Uses only when not empty,
      and only if progress was not active already
      (so we avoid starting "progress bar within a progress bar"). }
    property TriangleOctreeProgressTitle: string
      read  FTriangleOctreeProgressTitle
      write FTriangleOctreeProgressTitle;

    { Progress title shown during spatial structure creation
      (through TProgress.Title). Uses only when not empty,
      and only if progress was not active already
      (so we avoid starting "progress bar within a progress bar"). }
    property ShapeOctreeProgressTitle: string
      read  FShapeOctreeProgressTitle
      write FShapeOctreeProgressTitle;

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
      out CamPos, CamDir, CamUp, GravityUp: TVector3;
      const ViewpointDescription: string = ''):
      TAbstractViewpointNode;

    function GetPerspectiveViewpoint(
      out CamPos, CamDir, CamUp, GravityUp: TVector3;
      const ViewpointDescription: string = ''):
      TAbstractViewpointNode;
    { @groupEnd }

    { Frees some scene resources, to conserve memory.
      See TSceneFreeResources documentation. }
    procedure FreeResources(Resources: TSceneFreeResources); virtual;

    { Recursively unset node's TX3DNode.Scene. Useful if you want to remove
      part of a node graph and put it in some other scene.

      @italic(You almost never need to call this method) --- this
      is done automatically for you when TCastleSceneCore is destroyed.
      However, if you process RootNode graph
      and extract some node from it (that is, delete node from our
      RootNode graph, but instead of freeing it you insert it
      into some other VRML/X3D graph) you must call it to manually
      "untie" this node (and all it's children) from this TCastleSceneCore instance. }
    procedure UnregisterScene(Node: TX3DNode);

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single; const CancelAction: boolean = false): boolean; override;

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
    property PointingDeviceOverPoint: TVector3
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

    function LocalBoundingBox: TBox3D; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Change current scene time, setting @link(Time).
      It is crucial that you call this continously to have some VRML/X3D
      time-dependent features working, like TimeSensor and MovieTexture.
      See @link(Time) for details what is affected by this.

      This is automatically taken care of if you added this scene
      to TCastleWindowCustom.Controls or TCastleControlCustom.Controls.
      Then our @link(Update) takes care of doing the job,
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

    procedure IncreaseTimeTick;
      deprecated 'it should not be necessary to call this, ever; using TX3DEvent.Send(...) or TX3DEvent.Send(..., NextEventTime) will automatically behave Ok.';

    { The time within this scene, in seconds.
      Increasing this "drives" the animations (by increasing
      time of time-dependent nodes like X3D TimeSensor, which in turn
      drive the rest of the animation).

      You can use @link(SetTime) or @link(IncreaseTime) to move time
      forward manually. But usually there's no need for it:
      our @link(Update) method takes care of it automatically,
      you only need to place the scene inside @link(TCastleSceneManager.Items).

      You can start/stop time progress by @link(TimePlaying)
      and scale it by @link(TimePlayingSpeed). These properties
      affect how the time is updated by the @link(Update) method
      (so if you use @link(SetTime) or @link(IncreaseTime) methods,
      you're working around these properties).

      Default time value is 0.0 (zero). However it will be reset
      at load to a current time (seconds since Unix epoch ---
      that's what X3D standard says to use, although you can change
      it by KambiNavigationInfo.timeOriginAtLoad,
      see https://castle-engine.io/x3d_implementation_navigation_extensions.php#section_ext_time_origin_at_load ).
      You can perform this "time reset" yourself by @link(ResetTimeAtLoad)
      or @link(ResetTime). }
    function Time: TFloatTime; override;

    { Time that should be used for next event.
      You usually don't need to call this directly, this is automatically
      used by TX3DEvent.Send when you don't specify explicit time. }
    function NextEventTime: TX3DTime; override;

    { Set @link(Time) to arbitrary value.

      You should only use this when you loaded new VRML/X3D model.

      Unlike SetTime and IncreaseTime, this doesn't require that
      @link(Time) grows. It still does some time-dependent events work,
      although some time-dependent nodes may be just unconditionally reset
      by this to starting value (as they keep local time, and so require
      TimeIncrease notion, without it we can only reset them).

      @groupBegin }
    procedure ResetTime(const NewValue: TFloatTime);
    { @groupEnd }

    { Set @link(Time) to suitable initial value after loading a world.

      This honours VRML/X3D specification about VRML/X3D time origin,
      and our extension
      [https://castle-engine.io/x3d_extensions.php#section_ext_time_origin_at_load]. }
    procedure ResetTimeAtLoad;

    { Initial world time, set by the last ResetTimeAtLoad call. }
    property TimeAtLoad: TFloatTime read FTimeAtLoad;

    { Stack of background nodes. The node at the top is the current background.
      All nodes on this stack must descend from TAbstractBackgroundNode class. }
    property BackgroundStack: TBackgroundStack read FBackgroundStack;

    { Stack of fog nodes. The node at the top is the current fog.
      All nodes on this stack must descend from TFogNode class. }
    property FogStack: TFogStack read FFogStack;

    { Stack of NavigatinInfo nodes. The node at the top is the current NavigatinInfo.
      All nodes on this stack must descend from TNavigationInfoNode class. }
    property NavigationInfoStack: TNavigationInfoStack read FNavigationInfoStack;

    { Stack of viewpoint nodes. The node at the top is the current Viewpoint.
      All nodes on this stack must descend from TAbstractViewpointNode.
      Note that this includes also VRML 1.0/Inventor nodes. }
    property ViewpointStack: TViewpointStack read FViewpointStack;

    function GetBackgroundStack: TX3DBindableStackBasic; override;
    function GetFogStack: TX3DBindableStackBasic; override;
    function GetNavigationInfoStack: TX3DBindableStackBasic; override;
    function GetViewpointStack: TX3DBindableStackBasic; override;

    function CameraPosition: TVector3; deprecated 'do not access camera properties this way, instead use e.g. SceneManager.Camera.Position';
    function CameraDirection: TVector3; deprecated 'do not access camera properties this way, instead use e.g. SceneManager.Camera.GetView';
    function CameraUp: TVector3; deprecated 'do not access camera properties this way, instead use e.g. SceneManager.Camera.GetView';
    function CameraViewKnown: boolean; deprecated 'do not access camera properties this way, instead use e.g. SceneManager.Camera';

    { Call when camera position/dir/up changed, to update things depending
      on camera settings. This includes sensors like ProximitySensor,
      LOD nodes, camera settings for next RenderedTexture update and more.

      @bold(There should be no need to call this method explicitly.
      The scene is notified about camera changes automatically,
      by the @link(TCastleSceneManager). This method may be renamed / removed
      in future releases.) }
    procedure CameraChanged(ACamera: TCamera); override;

    { List of handlers for VRML/X3D Script node with "compiled:" protocol.
      This is read-only, change this only by RegisterCompiledScript. }
    property CompiledScriptHandlers: TCompiledScriptHandlerInfoList
      read FCompiledScriptHandlers;

    { Register compiled script handler, for VRML/X3D Script node with
      "compiled:" protocol.
      See [https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled]. }
    procedure RegisterCompiledScript(const HandlerName: string;
      Handler: TCompiledScriptHandler);

    { TNavigationType value determined by current NavigationInfo node. }
    function NavigationTypeFromNavigationInfo: TNavigationType;

    { Update camera properties based on currently bound NavigationInfo.

      Bound NavigationInfo node is taken from
      NavigationInfoStack.Top. If no NavigationInfo is bound, this is @nil,
      and we will create camera corresponding to default NavigationInfo
      values (this is following VRML/X3D spec), so it will have
      initial type = EXAMINE.

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
      Usually, it should be SceneManager.Items.BoundingBox.
      In simple cases (if this scene is the only TCastleScene instance
      in your world, and it's not transformed) it may be equal to just
      @link(BoundingBox) of this scene. }
    procedure CameraFromNavigationInfo(Camera: TCamera; const WorldBox: TBox3D);

    { Update camera to the currently bound VRML/X3D viewpoint.
      When no viewpoint is currently bound, we will go to a suitable
      viewpoint to see the whole scene (based on the Camera.ModelBox,
      which should be set in CameraFromNavigationInfo to the world bounding box).

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

    { Make Camera go to the view given by APosition, ADirection, AUp.

      Honours current NavigationInfo.transitionType and transitionTime.
      If transitionType indicates instanteneous transition, then jumps
      by simple @code(Camera.SetView(APosition, ADirection, AUp)).
      Otherwise makes a smooth animation into new values by
      @code(Camera.AnimateTo(APosition, ADirection, AUp, TransitionTime)).

      Will generate NavigationInfo.transitionComplete when transition ends.

      @groupBegin }
    procedure CameraTransition(Camera: TCamera; const APosition, ADirection, AUp: TVector3);
    procedure CameraTransition(Camera: TCamera; const APosition, ADirection, AUp, GravityUp: TVector3);
    { @groupEnd }

    { Detect position/direction of the main light that produces shadows.
      This is useful when you want to make shadows on the scene
      from only a single light, but your scene has many lights.

      The main light is simply one with both @code(shadowVolumes) and
      @code(shadowVolumesMain) fields set to @true. See
      [https://castle-engine.io/x3d_extensions.php#section_ext_shadows]
      for more info.
      If no light with shadowVolumes = shadowVolumesMain = TRUE
      is present then this function returns @false,
      since AMainLightPosition cannot be calculated.

      AMainLightPosition[3] is always set to 1
      (positional light) or 0 (indicates that this is a directional light).

      @seealso TCastleAbstractViewport.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4): boolean;

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

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const ProgressStep: boolean; const Params: TPrepareParams); override;

    function Dragging: boolean; override;

    { Static scene will not be automatically notified about the changes
      to the field values. This means that TX3DField.Send and
      TX3DField.Changed will not notify this scene. This makes a
      small optimization when you know you will not modify scene's VRML/X3D graph
      besides loading (or you're prepared to do it by manually calling
      Scene.InternalChangedField, but this should not be used anymore, it's really
      dirty).

      The behavior of events is undefined when scene is static.
      This means that you should always have ProcessEvents = @false
      when Static = @true. Only when Static = false you're allowed
      to freely change ProcessEvents to @true.

      Changing this is expensive when the scene content is already loaded,
      so it's best to adjust this before @link(Load). }
    property Static: boolean read FStatic write SetStatic default false;
      deprecated 'do not use this; optimization done by this is really negligible; leave ProcessEvents=false for static scenes';

    { Nice scene caption. Uses the "title" of WorldInfo
      node inside the VRML/X3D scene. If there is no WorldInfo node
      (or it has empty title) then result is based on loaded URL. }
    function Caption: string;

    { Global lights of this scene. Read-only. May be useful to render
      other 3D objects with lights defined inside this scene. }
    property GlobalLights: TLightInstancesList read FGlobalLights;

    { Find a named X3D node (and a field or event within this node)
      in the current node graph. They search all nodes
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

    { List the names of available animations in this file.
      Animations are detected in VRML/X3D models as simply TimeSensor nodes
      (if you set @link(AnimationPrefix) property, we additionally
      filter them to show only the names starting with given prefix).
      You can even get the time sensor node directly by AnimationTimeSensor.

      The resulting TStringList instance is owned by this object,
      do not free it.

      Note that the list of animations may change if you rebuild the underlying
      X3D nodes graph, for example if you start to delete / add some TimeSensor
      nodes.  }
    property AnimationsList: TStrings read FAnimationsList;

    { Does named animation with given name exist.
      @seealso AnimationsList
      @seealso PlayAnimation }
    function HasAnimation(const AnimationName: string): boolean;

    { TimeSensor of this animation. @nil if this name not found.
      See e.g. examples/3d_rendering_processing/listen_on_x3d_events.lpr
      for an example of using this. }
    function AnimationTimeSensor(const AnimationName: string): TTimeSensorNode;

    { TimeSensor of this animation, by animation index (index
      on AnimationsList). @nil if this index not found. }
    function AnimationTimeSensor(const Index: Integer): TTimeSensorNode;

    function Animations: TStringList; deprecated 'use AnimationsList (and do not free it''s result)';

    { Forcefully, immediately, set 3D pose from given animation,
      with given time in animation.

      This avoids the normal passage of time in X3D scenes,
      it ignores the @link(ProcessEvents) and @link(AnimateOnlyWhenVisible)
      properties, it ignores the current animation set by @link(PlayAnimation),
      and forces the current time on TimeSensors by @link(TTimeSensorNode.FakeTime). }
    function ForceAnimationPose(const AnimationName: string;
      const TimeInAnimation: TFloatTime;
      const Loop: boolean;
      const Forward: boolean = true): boolean; overload;
    function ForceAnimationPose(const AnimationName: string;
      const TimeInAnimation: TFloatTime;
      const Looping: TPlayAnimationLooping;
      const Forward: boolean = true): boolean; overload;
      deprecated 'use ForceAnimationPose overload with "Loop: boolean" parameter';

    { Play a named animation.
      Calling this method also stops previously playing named animation, if any.
      Returns boolean whether such animation name was found.
      To get the list of available animations, see @link(AnimationsList).

      This is the simplest way to play animations using Castle Game Engine.
      For a nice overview about using PlayAnimation, see the manual
      https://castle-engine.io/manual_scene.php , section "Play animation".

      Playing an already-playing animation is guaranteed to start it from
      the beginning.

      You can specify whether the animation should loop,
      whether to play it forward or backward,
      whether to do animation blending and some other options:
      see @link(TPlayAnimationParameters).

      If you use an overloaded version with the TPlayAnimationParameters,
      note that you can (and usually should) free the TPlayAnimationParameters
      instance right after calling this method. We do not keep reference to
      the TPlayAnimationParameters instance, and we do not free it ourselves.

      Some obscure notes (you usually @italic(do not need to know the details
      below)):

      @unorderedList(
        @item(Calling this method @italic(does not change the scene immediately).
          There may be a delay between calling PlayAnimation and actually
          changing the scene to reflect the state at the beginning of the indicated
          animation. This delay is usually 1 frame (that is, the scene is updated
          at the next @link(Update) call), but it may be larger if you use
          the optimization @link(AnimateSkipTicks).

          This is often a desirable optimization. There's often no "rush" to
          visually change the animation @italic(right now), and doing it at
          the nearest @link(Update) call is acceptable.
          It also means that calling @link(PlayAnimation) multiple times
          before a single @link(Update) call is not expensive.

          But, sometimes you need to change the scene immediately (for example,
          because you don't want to show user the initial scene state).
          To do this, simply call @link(ForceAnimationPose) with @code(TimeInAnimation)
          parameter = 0 and the same animation. This will change the scene immediately,
          to show the beginning of this animation.
          You can call @link(ForceAnimationPose) before or after @link(PlayAnimation),
          it doesn't matter.

          Or you can call @link(ForceInitialAnimationPose) right after @link(PlayAnimation).
        )

        @item(Internally, @italic(the animation is performed using TTimeSensorNode
          that instructs other nodes to change). For exampe, TTimeSensorNode
          may instruct a TCoordinateInterpolatorNode to update the TIndexedFaceSetNode coordinates,
          and in effect the animation can deform a mesh.
          Or TTimeSensorNode may instruct a TPositionInterpolatorNode to update
          @link(TTransformNode.Translation),
          and in effect the animation can move something.

          So the animation means that the X3D nodes graph within @link(RootNode)
          is being changed. The exact subset of the nodes inside @link(RootNode)
          that change depends on your animation.

          This means that internally our mechanism is very flexible.
          E.g. you can have another animation running (another TTimeSensorNode running)
          in parallel to the animation run by this method.
          You can also change parts of the node graph by your own code
          (accessing @link(RootNode))
          in parallel to the animation by this method, as long as you don't touch
          the same nodes.
          @italic(Such tricks are possible, but require manually designing a proper X3D file).

          If you load a model from a castle-anim-frames or MD3 format, note that it
          is animated using a special "node interpolator" algorithm. In this case,
          you cannot really change the model inside @link(RootNode) anymore
          --- any modifications may be overwritten by the "node interpolator"
          at some point (the exact overwrite moment depends on the "merge nodes"
          optimization done by the "node interpolator").
        )
      )
    }
    function PlayAnimation(const Parameters: TPlayAnimationParameters): boolean; overload;
    function PlayAnimation(const AnimationName: string;
      const Loop: boolean; const Forward: boolean = true): boolean; overload;
    function PlayAnimation(const AnimationName: string;
      const Looping: TPlayAnimationLooping;
      const Forward: boolean = true): boolean; overload;
      deprecated 'use another overloaded version of PlayAnimation, like simple PlayAnimation(AnimationName: string, Loop: boolean)';

    { Call right after calling @link(PlayAnimation) (before any update event
      took place) to force setting initial animation frame @italic(now).
      This sets first animation frame,
      unless you used TPlayAnimationParameters.InitialTime <> 0. }
    procedure ForceInitialAnimationPose;

    { Duration, in seconds, of the named animation
      (named animations are detected by @link(AnimationsList) method).
      For a looping animation, this is the duration of a single cycle.
      0 if not found. }
    function AnimationDuration(const AnimationName: string): TFloatTime;

    { The prefix of an X3D TimeSensor node name to treat it as a "named animation".
      Named animation are used by methods @link(AnimationsList), @link(PlayAnimation),
      and @link(AnimationDuration), @link(HasAnimation).
      By default this is empty, which means we consider all TimeSensor nodes
      a "named animation".

      You can set this to something like 'Anim_' or 'Animation_'
      or whatever your 3D export software produces. Only the TimeSensor
      nodes with names starting with this prefix will be available
      on @link(AnimationsList), and this prefix will be stripped from
      the names you use with methods like @link(PlayAnimation). }
    property AnimationPrefix: string
      read FAnimationPrefix write FAnimationPrefix;

    { Currently played animation by @link(PlayAnimation), or @nil.
      Note that in X3D world, you can have multiple active animations
      (TimeSensor nodes) at the same time. This property only describes
      the animation controlled by the @link(PlayAnimation) method.

      Note that the animation may be started by PlayAnimation with a 1-frame
      delay, but this property hides it from you, it is changed
      immediately by the PlayAnimation call. }
    property CurrentAnimation: TTimeSensorNode read FCurrentAnimation;

    { Force recalculating the text shapes when font changed.
      For now, we don't detect font changes (when TFontStyleNode.OnFont
      returns something different) ourselves.
      This calls @link(TTextNode.FontChanged) and @link(TAsciiTextNode_1.FontChanged)
      on all appropriate nodes. }
    procedure FontChanged;

    { Create a scene with the same contents (X3D scene graph) as this one.
      The created scene has exactly the same class as this one
      (we use ClassType.Create to call a virtual constructor).

      Note that this @bold(does not copy other scene attributes),
      like @link(ProcessEvents) or @link(Spatial) or rendering attributes
      in @link(TCastleScene.Attributes). }
    function Clone(const AOwner: TComponent): TCastleSceneCore;

    { @deprecated Deprecated name for @link(URL). }
    property FileName: string read FURL write SetURL; deprecated;
  published
    { When TimePlaying is @true, the time of our 3D world will keep playing.
      More precisely, our @link(Update) will take care of increasing @link(Time).
      Our @link(Update) is usually automatically called (if you added this
      scene to TCastleWindowCustom.Controls or TCastleControlCustom.Controls)
      so you don't have to do anything to make this work. }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;

    { Controls the time speed (if TimePlaying is @true):
      1.0 means that 1 second  of real time equals to 1 unit of world time. }
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;

    { Which spatial structures (octrees) should be created and used.

      Using "spatial structures" allows to achieve various things:

      @unorderedList(
        @item(@bold(ssDynamicCollisions) or @bold(ssStaticCollisions):

          Using any of these flags allows to resolve collisions with
          the (collidable) triangles of the model.
          By default, every shape is collidable, but you can use the
          @link(TCollisionNode) to turn collisions off for some shapes
          (or replace them with simpler objects
          for the collision-detection purposes).

          As for the distinction between these two flags,
          ssDynamicCollisions and ssStaticCollisions:
          Almost always you should use ssDynamicCollisions.
          The speedup of ssStaticCollisions is very small,
          and sometimes it costs memory usage (as shapes cannot be reused),
          and ssStaticCollisions may cause crashes if the model
          accidentally changes.

          If you use neither @bold(ssDynamicCollisions) nor @bold(ssStaticCollisions),
          then the collisions are resolved using the whole scene bounding
          box. That is, treating the whole scene as a giant cube.

          You can always toggle @link(Collides) to quickly make
          the scene not collidable.)

        @item(@bold(ssRendering):

          Using this adds an additional optimization during rendering.
          This allows to use frustum culling with an octree.

          Using this is adviced, if your camera usually only sees
          a small portion of the scene. For example,
          a typical level / location in a game usually qualifies.)

        @item(@bold(ssVisibleTriangles):

          Using this allows to resolve collisions with visible triangles
          quickly. This mostly useful only for ray-tracers.)
      )

      See @link(TSceneSpatialStructure) for more details about
      the possible values. For usual dynamic scenes rendered in real-time,
      you set this to @code([ssRendering, ssDynamicCollisions]).

      By default, the value of this property is empty, which means that
      no octrees will be created. This has to be the default value,
      to:

      @orderedList(
        @item(Not create octrees by default (e.g. at construction).
          Creating them takes time (and memory).)
        @item(Allow developer to adjust TriangleOctreeLimits
          before creating the octree.)
      ) }
    property Spatial: TSceneSpatialStructures read FSpatial write SetSpatial default [];

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

    { Currently loaded scene URL. Set this to load a 3D scene
      from the given URL, we can load from any known 3D format
      (VRML, X3D, Collada, 3ds, Wavefront, etc.).

      Works just like the @link(Load) method (the overloaded version
      that takes @code(AURL: string) parameter). And, in fact, using
      directly the @link(Load) method will also change this URL property.

      The only difference of @code(Scene.URL := 'blah.x3d') vs
      @code(Scene.Load('blah.x3d')) is that setting the URL will
      @italic(not) reload the scene if you set it to the same value.
      That is, @code(Scene.URL := Scene.URL;) will not reload
      the scene (you have to use explicit @link(Load) for this.). }
    property URL: string read FURL write SetURL;

    { At loading, process the scene to support shadow maps.
      This happens at the @link(Load) method call,
      and it makes "receiveShadows" field automatically handled.

      Note that this is not the only way to make shadow maps.
      VRML/X3D author can always make shadow maps by using lower-level nodes, see
      [https://castle-engine.io/x3d_extensions.php#section_ext_shadow_maps].
      When using these lower-level nodes, this property does not matter
      This property (and related ones
      like ShadowMapsDefaultSize)
      is relevant only for handling shadows by the "receiveShadows" field. }
    property ShadowMaps: boolean read FShadowMaps write SetShadowMaps default true;

    { Default shadow map texture size.

      Affects how shadow maps are handled for the "receiveShadows"
      field.  This is taken into account at the scene @link(Load) time,
      and only if @link(ShadowMaps) is @true.

      VRML/X3D author can always override this by placing a @code(GeneratedShadowMap)
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

    { When @true, we animate (more precisely: process time pass in @link(Update))
      only when the model is visible. This is a powerful optimization,
      but be careful if you depend on your animations
      for something else than just visual effect. }
    property AnimateOnlyWhenVisible: boolean
      read FAnimateOnlyWhenVisible write FAnimateOnlyWhenVisible default false;

    { Non-zero values optimize the animation processing, by not updating
      the animation every frame. After updating the animation in one
      @link(Update) call, the next AnimateSkipTicks number of @link(Update)
      calls will go very quickly, as they will not actually change the scene
      at all.

      This is an effective optimization if the scene is usually not large on
      the screen.

      @unorderedList(
        @item(The animation is less smooth. For example, if AnimateSkipTicks = 1,
          then every other @link(Update) call does not change the scene.
          For example, if you have 60 FPS, and @link(Update) is called 60 times
          per second, then we will actually change the scene only 30 times per second.

          The "skip" within every scene has a little random shift,
          to avoid synchronizing this skip across many created scenes.
          This makes this a little harder to notice.)

        @item(In exchange, the speedup is substantial.
          For example, if AnimateSkipTicks = 1, then the animation on CPU effectively
          costs 2x less. In general, AnimateSkipTicks = N means that the cost
          drops to @code(1 / (1 + N)).)
      ) }
    property AnimateSkipTicks: Cardinal read FAnimateSkipTicks write SetAnimateSkipTicks
      default 0;
  end;

  {$define read_interface}
  {$I castlescenecore_physics.inc}
  {$undef read_interface}

var
  { Log changes to fields.
    This debugs what and why happens through TCastleSceneCore.InternalChangedField method
    and friends, which is central to VRML/X3D dynamic changes and events engine.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogChanges: boolean = false;

  { Set this to optimize animating transformations for scenes where you
    have many transformations (many Transform nodes), and many of them
    are animated at the same time. Often particularly effective for
    skeletal animations of characters, 3D and 2D (e.g. from Spine). }
  OptimizeExtensiveTransformations: boolean = false;

const
  // Old name for paLooping.
  paForceLooping    = paLooping;
  // Old name for paNotLooping.
  paForceNotLooping = paNotLooping;

  ssCollidableTriangles = ssStaticCollisions deprecated 'use ssStaticCollisions instead';

implementation

uses Math,
  X3DCameraUtils, CastleStringUtils, CastleLog, DateUtils,
  X3DLoad, CastleURIUtils, CastleTimeUtils;

{$define read_implementation}
{$I castlescenecore_physics.inc}
{$I castlescenecore_collisions.inc}
{$undef read_implementation}

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
    Node.EventIsBound.Send(Value, ParentScene.NextEventTime);
    Node.EventBindTime.Send(ParentScene.Time, ParentScene.NextEventTime);
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

{ TBackgroundStack ----------------------------------------------------------- }

function TBackgroundStack.Top: TAbstractBackgroundNode;
begin
  Result := (inherited Top) as TAbstractBackgroundNode;
end;

procedure TBackgroundStack.PushIfEmpty(Node: TAbstractBackgroundNode; SendEvents: boolean);
begin
  inherited PushIfEmpty(Node, SendEvents);
end;

{ TFogStack ------------------------------------------------------------------ }

function TFogStack.Top: TFogNode;
begin
  Result := (inherited Top) as TFogNode;
end;

procedure TFogStack.PushIfEmpty(Node: TFogNode; SendEvents: boolean);
begin
  inherited PushIfEmpty(Node, SendEvents);
end;

{ TNavigationInfoStack ------------------------------------------------------- }

procedure TNavigationInfoStack.DoBoundChanged;
begin
  ParentScene.UpdateHeadlightOnFromNavigationInfo;
  inherited;
end;

function TNavigationInfoStack.Top: TNavigationInfoNode;
begin
  Result := (inherited Top) as TNavigationInfoNode;
end;

procedure TNavigationInfoStack.PushIfEmpty(Node: TNavigationInfoNode; SendEvents: boolean);
begin
  inherited PushIfEmpty(Node, SendEvents);
end;

{ TViewpointStack ------------------------------------------------------------ }

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

procedure TViewpointStack.PushIfEmpty(Node: TAbstractViewpointNode; SendEvents: boolean);
begin
  inherited PushIfEmpty(Node, SendEvents);
end;

{ TGeneratedTextureList -------------------------------------------------- }

function TGeneratedTextureList.IndexOfTextureNode(TextureNode: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].TextureNode = TextureNode then
      Exit;
  Result := -1;
end;

function TGeneratedTextureList.FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
var
  Index: Integer;
begin
  Index := IndexOfTextureNode(TextureNode);
  if Index <> -1 then
    Result := Ptr(Index) else
    Result := nil;
end;

function TGeneratedTextureList.AddShapeTexture(Shape: TShape;
  Tex: TAbstractTextureNode): Pointer;
var
  GenTex: PGeneratedTexture;
begin
  Result := nil;

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
        WritelnWarning('VRML/X3D', 'The same GeneratedCubeMapTexture node is used (instanced) within at least two different VRML shapes. This is bad, as we don''t know from which shape should environment be captured');
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
    if (List^[I].TextureNode is TGeneratedShadowMapNode) and
       (TGeneratedShadowMapNode(List^[I].TextureNode).FdLight.Value = LightNode) then
      List^[I].Handler.UpdateNeeded := true;
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

procedure TTimeDependentHandlerList.AddIfNotExists(const Item: TInternalTimeDependentHandler);
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
  V: TVisibilitySensorInstanceList;
begin
  for V in Values do
    V.Free;
  inherited Clear;
end;

{ TPlayAnimationParameters --------------------------------------------------- }

constructor TPlayAnimationParameters.Create;
begin
  inherited;
  Loop := false;
  Forward := true;
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

  FBackgroundStack := TBackgroundStack.Create(Self);
  FFogStack := TFogStack.Create(Self);
  FNavigationInfoStack := TNavigationInfoStack.Create(Self);
  FViewpointStack := TViewpointStack.Create(Self);
  FViewpointsArray := TAbstractViewpointNodeList.Create(false);

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
  FAnimationsList := TStringList.Create;
  TStringList(FAnimationsList).CaseSensitive := true; // X3D node names are case-sensitive

  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;

  FShadowMaps := true;
  FShadowMapsDefaultSize := DefaultShadowMapsDefaultSize;
  FAnimationPrefix := DefaultAnimationPrefix;

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
  FreeAndNil(FViewpointsArray);

  FreeAndNil(FPointingDeviceActiveSensors);

  FreeAndNil(FShapes);
  FreeAndNil(ShapeLODs);
  FreeAndNil(FGlobalLights);

  FreeAndNil(FOctreeRendering);
  FreeAndNil(FOctreeDynamicCollisions);
  FreeAndNil(FOctreeVisibleTriangles);
  FreeAndNil(FOctreeStaticCollisions);
  FreeAndNil(FAnimationsList);
  FreeAndNil(PreviousPartialAffectedFields);

  if OwnsRootNode then
    FreeAndNil(FRootNode) else
  begin
    { This will call UnregisterScene(RootNode). }
    {$warnings off}
    { consciously using deprecated feature; in the future,
      we will just explicitly call
        if RootNode <> nil then UnregisterScene(RootNode);
      here. }
    Static := true;
    {$warnings on}
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

  FRootNode := ARootNode; // set using FRootNode, we will call ChangedAll later
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

procedure TCastleSceneCore.Load(const AURL: string; AllowStdIn: boolean;
  const AResetTime: boolean);
begin
  { Note that if Load3D fails, we will not change the RootNode,
    so currently loaded scene will remain valid. }

  Load(Load3D(AURL, AllowStdIn), true, AResetTime);

  FURL := AURL;
end;

procedure TCastleSceneCore.SetRootNode(const Value: TX3DRootNode);
begin
  if FRootNode <> Value then
  begin
    if OwnsRootNode then
    begin
      BeforeNodesFree;
      PointingDeviceClear;
      FreeAndNil(FRootNode);
    end;
    FRootNode := Value;
    ScheduleChangedAll;
  end;
end;

procedure TCastleSceneCore.Save(const AURL: string);
begin
  if RootNode <> nil then
    Save3D(RootNode, AURL, ApplicationName);
  FURL := AURL;
end;

procedure TCastleSceneCore.SetURL(const AValue: string);
begin
  if AValue <> FURL then
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

function TCastleSceneCore.CalculateLocalBoundingBox: TBox3D;
var
  SI: TShapeTreeIterator;
begin
  Result := TBox3D.Empty;
  SI := TShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
      Result.Include(SI.Current.BoundingBox);
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
      Result := Result + SI.Current.VerticesCount(OverTriangulate);
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
      Result := Result + SI.Current.TrianglesCount(OverTriangulate);
  finally FreeAndNil(SI) end;
end;

function TCastleSceneCore.LocalBoundingBox: TBox3D;
begin
  if GetExists then
  begin
    if not (fvLocalBoundingBox in Validities) then
    begin
      FLocalBoundingBox := CalculateLocalBoundingBox;
      Include(Validities, fvLocalBoundingBox);
    end;
    Result := FLocalBoundingBox;
  end else
    Result := TBox3D.Empty;

  Result.Include(inherited LocalBoundingBox);
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
    function Traverse(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
  end;

function TChangedAllTraverser.Traverse(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;

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
    CameraLocalPosition: TVector3;
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

    if ParentScene.ProcessEvents and
       ParentScene.GetCameraLocal(CameraLocalPosition) then
      ParentScene.UpdateLODLevel(LODTree, CameraLocalPosition);

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
  begin
    VSI := TVisibilitySensorInstance.Create(ParentScene);
    VSI.Node := Node;
    VSI.Transform := StateStack.Top.Transform;
    VSI.Box := Node.Box.Transform(VSI.Transform);

    ShapesGroup.Children.Add(VSI);

    { add to ParentScene.VisibilitySensors map }
    if not ParentScene.VisibilitySensors.TryGetValue(Node, Instances) then
    begin
      Instances := TVisibilitySensorInstanceList.Create(false);
      ParentScene.VisibilitySensors.Add(Node, Instances);
    end;
    Instances.Add(VSI);
  end;

var
  Shape: TShape;
begin
  Result := nil;

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
      Shape.InternalTriangleOctreeProgressTitle := ParentScene.TriangleOctreeProgressTitle;
      Shape.InternalSpatial := [ssTriangles];
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
         (ParentScene.InitialViewpointName = Node.X3DName) then
      begin
        if (ParentScene.InitialViewpointIndex =
            ParentScene.ChangedAllCurrentViewpointIndex) then
          ParentScene.ViewpointStack.PushIfEmpty( TAbstractViewpointNode(Node), true);
        Inc(ParentScene.ChangedAllCurrentViewpointIndex);
      end;
      ParentScene.FViewpointsArray.Add(TAbstractViewpointNode(Node));
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

procedure TCastleSceneCore.UpdateLODLevel(const LODTree: TShapeTreeLOD;
  const CameraLocalPosition: TVector3);
var
  OldLevel, NewLevel: Cardinal;
begin
  Assert(ProcessEvents);

  OldLevel := LODTree.Level;
  NewLevel := LODTree.CalculateLevel(CameraLocalPosition);
  LODTree.Level := NewLevel;

  if ( (OldLevel <> NewLevel) or
       (not LODTree.WasLevel_ChangedSend) ) and
     { If LODTree.Children.Count = 0, then Level = 0, but we don't
       want to send Level_Changed since the children 0 doesn't really exist. }
     (LODTree.Children.Count <> 0) then
  begin
    LODTree.WasLevel_ChangedSend := true;
    LODTree.LODNode.EventLevel_Changed.Send(LongInt(NewLevel), NextEventTime);
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

  { clear animation stuff, since any TTimeSensorNode may be freed soon }
  if PlayingAnimationNode <> nil then
    PlayingAnimationNode.EventIsActive.RemoveNotification(@PlayingAnimationIsActive);
  PlayingAnimationNode := nil;
  PlayingAnimationStopNotification := nil;
  FCurrentAnimation := nil;
  NewPlayingAnimationNode := nil;
  NewPlayingAnimationUse := false;
  PreviousPlayingAnimation := nil;
  FreeAndNil(PreviousPartialAffectedFields);

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

procedure TCastleSceneCore.RemoveShape(Shape: TShape);
begin
  if Shape.Node <> nil then
    NodeFreeRemovingFromAllParents(Shape.Node) else
    { Do not use Shape.Geometry here, as it may be a temporary result
      of OriginalGeometry.Proxy.

      When the shape is freed (which happens in BeforeNodesFree called
      at the beginning of NodeFreeRemovingFromAllParents),
      the proxy result is freed too. So using here Shape.Geometry
      would not only not free what you think, it would also cause segfault. }
    NodeFreeRemovingFromAllParents(Shape.OriginalGeometry);
end;

procedure TCastleSceneCore.ChangedAllEnumerateCallback(Node: TX3DNode);
begin
  if not FStatic then
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
      (Node as IAbstractTimeDependentNode).InternalTimeDependentHandler);
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
      const Location: TVector3; const Radius: Single);
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
      L := GlobalLights.Ptr(I);
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
  { We really need to use InternalDirty here, to forbid rendering during this.

    For example, ProcessShadowMapsReceivers work assumes this:
    otherwise, RootNode.Traverse may cause some progress Step call
    which may call Render which may prepare GLSL shadow map shader
    that will be freed by the following ProcessShadowMapsReceivers call.
    Testcase: view3dscene open simple_shadow_map_teapots.x3dv, turn off
    shadow maps "receiveShadows" handling, then turn it back on
    --- will crash without "InternalDirty" variable safety. }
  Inc(InternalDirty);
  try

  if Log and LogChanges then
    WritelnLog('X3D changes', 'ChangedAll');

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

    { Clean Shapes and other stuff initialized by traversing }
    FreeAndNil(FShapes);
    FShapes := TShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;
    GlobalLights.Clear;
    FViewpointsArray.Clear;
    FAnimationsList.Clear;

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

    { recreate FAnimationsList now }
    FreeAndNil(FAnimationsList);
    {$warnings off}
    FAnimationsList := Animations;
    {$warnings on}
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
    WritelnLogMultiline('Shapes tree', Shapes.DebugInfo);

  finally Dec(InternalDirty) end;
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
    function TransformChangeTraverse(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
  end;

function TTransformChangeHelper.TransformChangeTraverse(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;

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
        StateStack, TX3DNode, @Self.TransformChangeTraverse, ParentInfo);
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
          StateStack, TX3DNode, @Self.TransformChangeTraverse, ParentInfo);

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
    CameraLocalPosition: TVector3;
  begin
    { get Shape and increase Shapes^.Index }
    ShapeLOD := Shapes^.Group.Children[Shapes^.Index] as TShapeTreeLOD;
    Inc(Shapes^.Index);

    { by the way, update LODInvertedTransform, since it changed }
    if Inside then
    begin
      ShapeLOD.LODInvertedTransform^ := StateStack.Top.InvertedTransform;
      if ParentScene.ProcessEvents and
         ParentScene.GetCameraLocal(CameraLocalPosition) then
        ParentScene.UpdateLODLevel(ShapeLOD, CameraLocalPosition);
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
          StateStack, TX3DNode, @Self.TransformChangeTraverse, ParentInfo);

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
          if List.List^[I].Node = LightNode then
            LightNode.UpdateLightInstanceState(List.List^[I], StateStack.Top);
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
    CameraVectors: TCameraVectors;
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
      if ParentScene.GetCameraLocal(CameraVectors) then
        ParentScene.ProximitySensorUpdate(Instance, CameraVectors);
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
  Result := nil;

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

    In some cases, children of this Transform node may
    be affected in other ways by transformation. For example,
    Fog and Background nodes are affected by their parents transform.
  }

  if Log and LogChanges then
    WritelnLog('X3D changes', Format('Transform node %s change: %d instances',
      [TransformNode.X3DType, Instances.Count]));

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

      TransformNode.TraverseInternal(TraverseStack, TX3DNode,
        @TransformChangeHelper.TransformChangeTraverse, nil);

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
end;

procedure TCastleSceneCore.RootTransformationChanged(const Changes: TX3DChanges);
var
  TransformChangeHelper: TTransformChangeHelper;
  TransformShapesParentInfo: TShapesParentInfo;
  TraverseStack: TX3DGraphTraverseStateStack;
  DoVisibleChanged: boolean;
begin
  if Log and LogChanges then
    WritelnLog('X3D changes', 'Transform root node change');

  if RootNode = nil then Exit;
  if not (Shapes is TShapeTreeGroup) then
  begin
    WritelnWarning('X3D changes', 'Root node did not create corresponding TShapeTreeGroup, transformation will not be applied correctly. Submit a bug');
    Exit;
  end;

  DoVisibleChanged := false;

  TraverseStack := nil;
  TransformChangeHelper := nil;
  try
    TraverseStack := TX3DGraphTraverseStateStack.Create;

    { initialize TransformChangeHelper, set before the loop properties
      that cannot change }
    TransformChangeHelper := TTransformChangeHelper.Create;
    TransformChangeHelper.ParentScene := Self;
    TransformChangeHelper.ChangingNode := RootNode;
    TransformChangeHelper.Changes := Changes;

    TransformShapesParentInfo.Group := Shapes as TShapeTreeGroup;
    TransformShapesParentInfo.Index := 0;

    { initialize TransformChangeHelper properties that may be changed
      during Node.Traverse later }
    TransformChangeHelper.Shapes := @TransformShapesParentInfo;
    TransformChangeHelper.AnythingChanged := false;
    TransformChangeHelper.Inside := false;
    TransformChangeHelper.Inactive := 0;

    RootNode.Traverse(TX3DNode, @TransformChangeHelper.TransformChangeTraverse);

    if TransformChangeHelper.AnythingChanged then
      DoVisibleChanged := true;

    { take care of calling THAnimHumanoidNode.AnimateSkin when joint is
      animated. Secure from Humanoid = nil (may happen if Joint
      is outside Humanoid node, see VRML 97 test
      ~/3dmodels/vrmlx3d/hanim/tecfa.unige.ch/vrml/objects/avatars/blaxxun/kambi_hanim_10_test.wrl)  }
    { TODO:
    if (RootNode is THAnimJointNode) and
       (TransformShapeTree.TransformState.Humanoid <> nil) then
      ScheduledHumanoidAnimateSkin.AddIfNotExists(
        TransformShapeTree.TransformState.Humanoid);
    }
  finally
    FreeAndNil(TraverseStack);
    FreeAndNil(TransformChangeHelper);
  end;

  if DoVisibleChanged then
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
end;

procedure TCastleSceneCore.InternalChangedField(Field: TX3DField);
var
  ANode: TX3DNode;
  Changes: TX3DChanges;

  procedure DoLogChanges(const Additional: string = '');
  var
    S: string;
  begin
    S := 'InternalChangedField: ' + X3DChangesToStr(Changes) +
      Format(', node: %s (%s %s) at %s',
      [ ANode.X3DName, ANode.X3DType, ANode.ClassName, PointerToStr(ANode) ]);
    if Field <> nil then
      S := S + Format(', field %s (%s)', [ Field.X3DName, Field.X3DType ]);
    if Additional <> '' then
      S := S + '. ' + Additional;
    WritelnLog('X3D changes', S);
  end;

  { Handle VRML >= 2.0 transformation changes. }
  procedure HandleChangeTransform;
  var
    Instances: TShapeTreeList;
  begin
    { the OptimizeExtensiveTransformations only works for scene with ProcessEvents,
      otherwise TransformationDirty would never be processed }
    if OptimizeExtensiveTransformations and
       ProcessEvents and
       (ForceImmediateProcessing = 0) then
      TransformationDirty := TransformationDirty + Changes else
    begin
      Check(Supports(ANode, ITransformNode),
        'chTransform flag may be set only for ITransformNode');

      Instances := TransformInstancesList.Instances(ANode, false);
      if Instances = nil then
      begin
        if Log and LogChanges then
          WritelnLog('X3D changes', Format('Transform node "%s" has no information, assuming does not exist in our VRML graph',
            [ANode.X3DType]));
        Exit;
      end;

      TransformationChanged(ANode, Instances, Changes);
    end;
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
        if (SI.Current.Geometry.InternalCoord(SI.Current.State, Coord) and
            (Coord = Field)) or
           { Change to OriginalGeometry.Coord should also be reported,
             since it may cause FreeProxy for shape. This is necessary
             for animation of NURBS controlPoint to work. }
           (SI.Current.OriginalGeometry.InternalCoord(SI.Current.State, Coord) and
            (Coord = Field)) then
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
        it's present on State.VRML1State list. }
      SI := TShapeTreeIterator.Create(Shapes, false);
      try
        while SI.GetNext do
          if (SI.Current.State.VRML1State.Nodes[VRML1StateNode] = ANode) or
             (SI.Current.OriginalState.VRML1State.Nodes[VRML1StateNode] = ANode) then
            SI.Current.Changed(false, Changes);
      finally FreeAndNil(SI) end;
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeMaterial;

    procedure HandleShape(Shape: TShape);
    begin
      if (Shape.State.ShapeNode <> nil) and
         (Shape.State.ShapeNode.Material = ANode) then
        Shape.Changed(false, Changes);
    end;

  begin
    { VRML 2.0 Material affects only shapes where it's
      placed inside Appearance.material field. }
    Shapes.Traverse(@HandleShape, false);
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
            LightInstance := SI.Current.State.Lights.Ptr(J);
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
    { When some shadowVolumes or shadowVolumesMain field changes,
      then MainLightForShadows must be recalculated. }
    Exclude(Validities, fvMainLightForShadows);
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeLightLocationDirection;
  var
    L: PLightInstance;
    I: Integer;
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

    { Change light instance on GlobalLights list, if any.
      This way other 3D scenes, using our lights by
      @link(TCastleAbstractViewport.UseGlobalLights) feature,
      also have updated light location/direction.
      See https://sourceforge.net/p/castle-engine/discussion/general/thread/0bbaaf38/
      for a testcase. }
    for I := 0 to GlobalLights.Count - 1 do
    begin
      L := GlobalLights.Ptr(I);
      if L^.Node = ANode then
        L^.Node.UpdateLightInstance(L^);
    end;
  end;

  procedure HandleChangeSwitch2;
  begin
    { Changing Switch.whichChoice changes the shapes that are considered
      active (in VRML/X3D graph, and in Shapes tree).
      This means that things calculated based
      on traverse/iterator over Shapes with "OnlyActive = true"
      are invalid now.

      DoGeometryChanged (scheduled by ScheduleGeometryChanged)
      actually does most of this work, it invalidates already most
      of the needed things when ScheduledGeometryActiveShapesChanged:

      fvLocalBoundingBox,
      fvVerticesCountNotOver, fvVerticesCountOver,
      fvTrianglesCountNotOver, fvTrianglesCountOver,
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
        if (SI.Current.Geometry.ColorField <> nil) and
           (SI.Current.Geometry.ColorField.Value = ANode) then
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
        if SI.Current.Geometry.InternalTexCoord(SI.Current.State, TexCoord) and
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
    CameraVectors: TCameraVectors;
  begin
    if ANode is TProximitySensorNode then
    begin
      { Update state for this ProximitySensor node. }
      if GetCameraLocal(CameraVectors) then
        for I := 0 to ProximitySensors.Count - 1 do
        begin
          if ProximitySensors[I].Node = ANode then
            ProximitySensorUpdate(ProximitySensors[I], CameraVectors);
        end;
    end else
    if ANode is TVisibilitySensorNode then
    begin
      VS := TVisibilitySensorNode(ANode);
      { local Box of this node changed,
        so update transformed Box in all TVisibilitySensorInstance for this node }
      if VisibilitySensors.TryGetValue(VS, VSInstances) then
      begin
        for I := 0 to VSInstances.Count - 1 do
          VSInstances[I].Box := VS.Box.Transform(VSInstances[I].Transform);
      end;
    end;
  end;

  procedure HandleChangeTimeStopStart;

    function GetInternalTimeDependentHandler(ANode: TX3DNode): TInternalTimeDependentHandler;
    begin
      if Supports(ANode, IAbstractTimeDependentNode) then
        Result := (ANode as IAbstractTimeDependentNode).InternalTimeDependentHandler else
        Result := nil;
    end;

  var
    Handler: TInternalTimeDependentHandler;
  begin
    Handler := GetInternalTimeDependentHandler(ANode);
    if Handler = nil then Exit; {< ANode not time-dependent. }

    { Why do we want to call Handler.SetTime now?

      Although (de)activation of time-dependent nodes will be also caught
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
      activation (from stopped state), testcase time_sensor_3.x3dv. }

    Handler.SetTime(Time, 0, false);

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
      TrianglesListShadowCasters and Manifold/BorderEdges changed in shapes. }
    FreeResources([frShadowVolume]);
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

  { Handle flags chVisibleGeometry, chVisibleNonGeometry, chRedisplay. }
  procedure HandleVisibleChange;
  var
    VisibleChanges: TVisibleChanges;
  begin
    VisibleChanges := [];
    if chVisibleGeometry    in Changes then Include(VisibleChanges, vcVisibleGeometry);
    if chVisibleNonGeometry in Changes then Include(VisibleChanges, vcVisibleNonGeometry);
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
      DragSensor.Deactivate(NextEventTime);
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

  procedure HandleChangeWireframe;
  var
    SI: TShapeTreeIterator;
  begin
    Assert(ANode is TShapeNode);

    { Call TShape.Changed for all shapes using this Shape node. }
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
      begin
        if SI.Current.State.ShapeNode = ANode then
          SI.Current.Changed(false, Changes);
      end;
    finally FreeAndNil(SI) end;
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
       of it: VRML1DefaultState, and also all the nodes created
       by Proxy methods (geometry and new state nodes).
  }

  Changes := Field.ExecuteChanges;

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
    if chWireframe in Changes then HandleChangeWireframe;

    if Changes * [chVisibleGeometry, chVisibleNonGeometry, chRedisplay] <> [] then
      HandleVisibleChange;
  finally EndChangesSchedule end;
end;

procedure TCastleSceneCore.DoGeometryChanged(const Change: TGeometryChange;
  LocalGeometryShape: TShape);
var
  SomeLocalGeometryChanged: boolean;
begin
  Validities := Validities - [fvLocalBoundingBox,
    fvVerticesCountNotOver, fvVerticesCountOver,
    fvTrianglesCountNotOver, fvTrianglesCountOver];

  { Calculate SomeLocalGeometryChanged (= if any
    LocalGeometryChanged was called, which means that octree and
    bounding box/sphere of some shape changed). }
  SomeLocalGeometryChanged := (Change = gcAll) or
    (Change in [gcLocalGeometryChanged, gcLocalGeometryChangedCoord]);

  if (FOctreeRendering <> nil) and
     ((Change in [gcVisibleTransformChanged, gcActiveShapesChanged]) or
      SomeLocalGeometryChanged) then
    FreeAndNil(FOctreeRendering);

  if (FOctreeDynamicCollisions <> nil) and
     ((Change in [gcCollidableTransformChanged, gcActiveShapesChanged]) or
      SomeLocalGeometryChanged) then
    FreeAndNil(FOctreeDynamicCollisions);

  if FOctreeStaticCollisions <> nil then
  begin
    WritelnWarning('ssStaticCollisions used on scene "' + Name + '" but the geometry changed. Freeing the spatial structure. You should use ssDynamicCollisions for this scene');
    FreeAndNil(FOctreeStaticCollisions);
  end;

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self, SomeLocalGeometryChanged,
      { We know LocalGeometryShape is nil now if Change does not contain
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
  Result := 'Bounding box : ' + BBox.ToString;
  if not BBox.IsEmpty then
  begin
    Result := Result + Format(', average size : %f', [BBox.AverageSize]);
  end;
  Result := Result + NL;
end;

procedure TCastleSceneCore.EdgesCount(out ManifoldEdges, BorderEdges: Cardinal);
var
  SI: TShapeTreeIterator;
begin
  ManifoldEdges := 0;
  BorderEdges := 0;
  SI := TShapeTreeIterator.Create(Shapes, true);
  try
    while SI.GetNext do
    begin
      ManifoldEdges := ManifoldEdges + SI.Current.InternalShadowVolumes.ManifoldEdges.Count;
      BorderEdges := BorderEdges + SI.Current.InternalShadowVolumes.BorderEdges.Count;
    end;
  finally FreeAndNil(SI) end;
end;

function TCastleSceneCore.InfoManifoldAndBorderEdges: string;
var
  ManifoldEdges, BorderEdges: Cardinal;
begin
  EdgesCount(ManifoldEdges, BorderEdges);
  Result := Format('Edges detection: all edges split into %d manifold edges and %d border edges. Remember that for shadow volumes, only the shapes that are perfect manifold (have zero border edges) can cast shadows.',
    [ManifoldEdges, BorderEdges]) + NL;
end;

function TCastleSceneCore.Info(
  ATriangleVerticesCounts,
  ABoundingBox,
  AManifoldAndBorderEdges: boolean): string;
begin
  Result := '';

  if ATriangleVerticesCounts then
  begin
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result := Result + InfoTriangleVerticesCounts;
    {$warnings on}
  end;

  if ABoundingBox then
  begin
    if Result <> '' then Result := Result + NL;
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result := Result + InfoBoundingBox;
    {$warnings on}
  end;

  if AManifoldAndBorderEdges then
  begin
    if Result <> '' then Result := Result + NL;
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result := Result + InfoManifoldAndBorderEdges;
    {$warnings on}
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
  const APosition: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  Progress.Step;
  TriangleOctreeToAdd.AddItemTriangle(Shape, APosition, Normal, TexCoord, Face);
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
            [https://castle-engine.io/x3d_extensions.php#section_ext_octree_properties].
          }

          SI.Current.InternalTriangleOctreeProgressTitle := TriangleOctreeProgressTitle;
          SI.Current.InternalSpatial := Value;
          { prepare OctreeTriangles. Not really needed, but otherwise
            shape's octrees would be updated (even on static scenes!)
            when the model runs. }
          SI.Current.InternalOctreeTriangles;
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

    { Handle OctreeStaticCollisions }

    Old := ssStaticCollisions in Spatial;
    New := ssStaticCollisions in Value;

    if Old and not New then
      FreeAndNil(FOctreeStaticCollisions);

    FSpatial := Value;
  end;
end;

function TCastleSceneCore.InternalOctreeRendering: TShapeOctree;
begin
  if (ssRendering in Spatial) and (FOctreeRendering = nil) then
  begin
    FOctreeRendering := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opRendering),
      ShapeOctreeProgressTitle,
      false);
    if Log and LogChanges then
      WritelnLog('X3D changes (octree)', 'OctreeRendering updated');
  end;

  Result := FOctreeRendering;
end;

function TCastleSceneCore.InternalOctreeDynamicCollisions: TShapeOctree;
begin
  if (ssDynamicCollisions in Spatial) and (FOctreeDynamicCollisions = nil) then
  begin
    FOctreeDynamicCollisions := CreateShapeOctree(
      OverrideOctreeLimits(FShapeOctreeLimits, opDynamicCollisions),
      ShapeOctreeProgressTitle,
      true);
    if Log and LogChanges then
      WritelnLog('X3D changes (octree)', 'OctreeDynamicCollisions updated');
  end;

  Result := FOctreeDynamicCollisions;
end;

function TCastleSceneCore.InternalOctreeVisibleTriangles: TTriangleOctree;
begin
  if (ssVisibleTriangles in Spatial) and (FOctreeVisibleTriangles = nil) then
    FOctreeVisibleTriangles := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opVisibleTriangles),
      TriangleOctreeProgressTitle,
      false);
  Result := FOctreeVisibleTriangles;
end;

function TCastleSceneCore.InternalOctreeStaticCollisions: TTriangleOctree;
begin
  if (ssStaticCollisions in Spatial) and (FOctreeStaticCollisions = nil) then
    FOctreeStaticCollisions := CreateTriangleOctree(
      OverrideOctreeLimits(FTriangleOctreeLimits, opStaticCollisions),
      TriangleOctreeProgressTitle,
      true);
  Result := FOctreeStaticCollisions;
end;

function TCastleSceneCore.InternalOctreeCollisions: TBaseTrianglesOctree;
begin
  if InternalOctreeStaticCollisions <> nil then
    Result := InternalOctreeStaticCollisions else
  if InternalOctreeDynamicCollisions <> nil then
    Result := InternalOctreeDynamicCollisions else
    Result := nil;
end;

function TCastleSceneCore.UseInternalOctreeCollisions: boolean;
begin
  Result := Spatial * [ssStaticCollisions, ssDynamicCollisions] <> [];
  Assert((not Result) or (InternalOctreeCollisions <> nil));

  { We check whether to use InternalOctreeCollisions
    not by checking InternalOctreeCollisions <> nil,
    but by checking Spatial.

    If you switch Spatial from non-empty to empty,
    then the octree in InternalOctreeCollisions remains assigned
    as long as there's no need to rebuild it.
    This is nice, in case you change Spatial again
    (e.g. by switching "Collisions" in view3dscene),
    the octree is immediately available.

    But we don't want to use this octree.
    When Spatial = [], you can *expect* that collisions revert to simpler
    mechanism (using bounding boxes).
    This is important only if you may have Collides = true with Spatial empty. }
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
  Inc(InternalDirty);
  try

  Result := TTriangleOctree.Create(Limits, LocalBoundingBox);
  try
    Result.Triangles.Capacity := TrianglesCount(false);
    if (ProgressTitle <> '') and
       (not Progress.Active) then
    begin
      Progress.Init(TrianglesCount(false), ProgressTitle, true);
      try
        TriangleOctreeToAdd := Result;
        FillOctree({$ifdef CASTLE_OBJFPC} @ {$endif} AddTriangleToOctreeProgress);
      finally Progress.Fini end;
    end else
      FillOctree({$ifdef CASTLE_OBJFPC} @ {$endif} Result.AddItemTriangle);
  except Result.Free; raise end;

  finally Dec(InternalDirty) end;

  { $define CASTLE_DEBUG_OCTREE_DUPLICATION}
  {$ifdef CASTLE_DEBUG_OCTREE_DUPLICATION}
  WritelnLog('Triangles Octree Stats', '%d items in octree, %d items in octree''s leafs, duplication %f',
    [Result.TotalItemsInOctree,
     Result.TotalItemsInLeafs,
     Result.TotalItemsInLeafs / Result.TotalItemsInOctree]);
  {$endif}
end;

function TCastleSceneCore.CreateShapeOctree(
  const Limits: TOctreeLimits;
  const ProgressTitle: string;
  const Collidable: boolean): TShapeOctree;
var
  I: Integer;
  ShapesList: TShapeList;
begin
  Inc(InternalDirty);
  try

  if Collidable then
    { Add only active and collidable shapes }
    ShapesList := TShapeList.Create(Shapes, true, false, true) else
    { Add only active and visible shapes }
    ShapesList := TShapeList.Create(Shapes, true, true, false);

  Result := TShapeOctree.Create(Limits, LocalBoundingBox, ShapesList, true);
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

  finally Dec(InternalDirty) end;

  {$ifdef CASTLE_DEBUG_OCTREE_DUPLICATION}
  WritelnLog('Shapes Octree Stats', '%d items in octree, %d items in octree''s leafs, duplication %f',
    [Result.TotalItemsInOctree,
     Result.TotalItemsInLeafs,
     Result.TotalItemsInLeafs / Result.TotalItemsInOctree]);
  {$endif}
end;

{ viewpoints ----------------------------------------------------------------- }

type
  TFirstViewpointSeeker = class
    OnlyPerspective: boolean;
    ViewpointDescription: string;
    function Seek(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
  end;

  function TFirstViewpointSeeker.Seek(
    Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
  var
    V: TAbstractViewpointNode;
  begin
    V := Node as TAbstractViewpointNode;
    if ( (not OnlyPerspective) or
         (V.ProjectionType = ptPerspective) ) and
       ( (ViewpointDescription = '') or
         ( (Node is TAbstractX3DViewpointNode) and
           (TAbstractX3DViewpointNode(Node).FdDescription.Value = ViewpointDescription) ) ) then
      Result := V else
      Result := nil;
  end;

function TCastleSceneCore.GetViewpointCore(
  const OnlyPerspective: boolean;
  out ProjectionType: TProjectionType;
  out CamPos, CamDir, CamUp, GravityUp: TVector3;
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
      Result := TAbstractViewpointNode(
        RootNode.Traverse(TAbstractViewpointNode, @Seeker.Seek));
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
  out CamPos, CamDir, CamUp, GravityUp: TVector3;
  const ViewpointDescription: string): TAbstractViewpointNode;
begin
  Result := GetViewpointCore(false, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
end;

function TCastleSceneCore.GetPerspectiveViewpoint(
  out CamPos, CamDir, CamUp, GravityUp: TVector3;
  const ViewpointDescription: string): TAbstractViewpointNode;
var
  ProjectionType: TProjectionType;
begin
  Result := GetViewpointCore(true, ProjectionType, CamPos, CamDir, CamUp, GravityUp,
    ViewpointDescription);
  Assert(ProjectionType = ptPerspective);
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

procedure TCastleSceneCore.FreeResources(Resources: TSceneFreeResources);

  procedure FreeShadowVolumes;
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        SI.Current.InternalShadowVolumes.FreeResources;
    finally FreeAndNil(SI) end;
  end;

begin
  if (frTextureDataInNodes in Resources) and (RootNode <> nil) then
  begin
    RootNode.EnumerateNodes(TAbstractTexture2DNode,
      @FreeResources_UnloadTextureData, false);
    RootNode.EnumerateNodes(TAbstractTexture3DNode,
      @FreeResources_UnloadTexture3DData, false);
  end;

  if frShadowVolume in Resources then
    FreeShadowVolumes;
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
begin
  if FProcessEvents <> Value then
  begin
    if Value then
    begin
      FProcessEvents := Value;

      ScriptsInitialize;

      { When ProcessEvents is set to @true, you want to initialize stuff
        for things depending on camera (and X3D events):
        - position/orientation_changed events on ProximitySensors,
        - update camera information on all Billboard nodes,
        - LOD nodes. }
      UpdateCameraEvents;
    end else
    begin
      ScriptsFinalize;
      PointingDeviceClear;

      FProcessEvents := Value;
    end;
  end;
end;

procedure TCastleSceneCore.SetStatic(const Value: boolean);
begin
  if FStatic <> Value then
  begin
    FStatic := Value;
    if FStatic then
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
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TAbstractKeyDeviceSensorNode).
          KeyDown(Event.Key, Event.KeyCharacter, NextEventTime);
    finally EndChangesSchedule; end;

    { Never treat the event as handled here,
      even if some X3DKeyDeviceSensorNode was found and did something.
      That is because we don't get enough information
      from VRML/X3D events (which may come down to calling some scripts in VRML/X3D)
      to be sure that the event is handled (and should not be passed to others,
      like Camera processed by TCastleSceneManager after Items.Press).
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
    BeginChangesSchedule;
    try
      for I := 0 to KeyDeviceSensorNodes.Count - 1 do
        (KeyDeviceSensorNodes.Items[I] as TAbstractKeyDeviceSensorNode).
          KeyUp(Event.Key, Event.KeyCharacter, NextEventTime);
    finally EndChangesSchedule; end;

    { Never treat the event as handled here,
      even if some X3DKeyDeviceSensorNode was found and did something.
      That is because we don't get enough information
      from VRML/X3D events (which may come down to calling some scripts in VRML/X3D)
      to be sure that the event is handled (and should not be passed to others,
      like Camera processed by TCastleSceneManager after Items.Release).
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

  OverItem := Pick.Triangle;

  { Never treat the event as handled here, as we don't get enough information
    from VRML/X3D events (which may come down to calling some scripts in VRML/X3D)
    to be sure that the event is handled (and should not be passed to others).
  Result := false; }

  if ProcessEvents then
  begin
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
                (OverItem^.State.PointingDeviceSensors.IndexOf(ActiveSensor) <> -1);

              if OldIsOver <> NewIsOver then
                ActiveSensor.EventIsOver.Send(NewIsOver, NextEventTime);
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
              raycollision twice for each Motion (to get
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
                TAbstractPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, NextEventTime);
            end;

          for I := 0 to NewSensors.Count - 1 do
            if OldSensors.IndexOf(NewSensors[I]) = -1 then
            begin
              if (NewSensors[I] is TAbstractPointingDeviceSensorNode) and
                TAbstractPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
                TAbstractPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, NextEventTime);
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
              TAbstractPointingDeviceSensorNode(OldSensors[I]).EventIsOver.Send(false, NextEventTime);
        end else
        begin
          Assert(OverItem <> nil);

          { So we previously pointed as nothing, and now at something.
            So simply call isOver = TRUE for all. }

          NewSensors := OverItem^.State.PointingDeviceSensors;

          for I := 0 to NewSensors.Count - 1 do
            if (NewSensors[I] is TAbstractPointingDeviceSensorNode) and
              TAbstractPointingDeviceSensorNode(NewSensors[I]).FdEnabled.Value then
              TAbstractPointingDeviceSensorNode(NewSensors[I]).EventIsOver.Send(true, NextEventTime);
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
                  we can get this by InvertedTransform. }
                OverItem^.State.InvertedTransform.MultPoint(Pick.Point), NextEventTime);

              {$ifndef CONSERVE_TRIANGLE_MEMORY}
              if TouchSensor.EventHitNormal_Changed.SendNeeded then
                TouchSensor.EventHitNormal_Changed.Send(
                  OverItem^.INormal(Pick.Point), NextEventTime);

              if TouchSensor.EventHitTexCoord_Changed.SendNeeded then
                TouchSensor.EventHitTexCoord_Changed.Send(
                  OverItem^.ITexCoord2D(Pick.Point), NextEventTime);
              {$endif not CONSERVE_TRIANGLE_MEMORY}
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
            NextEventTime, Pick.RayOrigin, Pick.RayDirection);
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
  FPointingDeviceOverPoint := TVector3.Zero;
  FPointingDeviceActive := false;
  if FPointingDeviceActiveSensors <> nil then
    FPointingDeviceActiveSensors.Count := 0;

  if SensorsChanged then
    DoPointingDeviceSensorsChange;
end;

function TCastleSceneCore.PointingDeviceActivate(const Active: boolean;
  const Distance: Single; const CancelAction: boolean): boolean;

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
        NewViewpoint.Bound := true;
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
      Sensor.Activate(NextEventTime, Sensors.Transform, Sensors.InvertedTransform,
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
            ActiveSensor.Deactivate(NextEventTime);
            { If we're still over the sensor, generate touchTime for TouchSensor }
            if (not CancelAction) and
               (PointingDeviceOverItem <> nil) and
               (PointingDeviceOverItem^.State.PointingDeviceSensors.
                 IndexOf(ActiveSensor) <> -1) and
               (ActiveSensor is TTouchSensorNode) then
            begin
              TTouchSensorNode(ActiveSensor).
                EventTouchTime.Send(Time, NextEventTime);
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

function TCastleSceneCore.Time: TFloatTime;
begin
  Result := FTimeNow.Seconds;
end;

function TCastleSceneCore.NextEventTime: TX3DTime;
begin
  { Increase @link(FTime) by some infinitely small value.
    This allows events to pass through the ROUTEs
    without the fear of being rejected as "recursive (cycle) events"
    from previous events. }

  Inc(FTimeNow.PlusTicks);
  Result := FTimeNow;
end;

procedure TCastleSceneCore.SetAnimateSkipTicks(const Value: Cardinal);
begin
  if FAnimateSkipTicks <> Value then
  begin
    FAnimateSkipTicks := Value;
    { randomizing it now desynchronizes the skipped frames across many scenes
      created in a single frame }
    AnimateSkipNextTicks := Random(FAnimateSkipTicks + 1);
  end;
end;

procedure TCastleSceneCore.InternalSetTime(
  const NewValue: TFloatTime; const TimeIncrease: TFloatTime; const ResetTime: boolean);

  { Apply NewPlayingAnimation* stuff.

    Call outside of AnimateOnlyWhenVisible check, to do it even when object
    is not visible.  Otherwise stop/start time would be delayed too until
    it becomes visible, which is not perfect (although it would be Ok
    in practice too?) }
  procedure UpdateNewPlayingAnimation;
  begin
    if NewPlayingAnimationUse then
    begin
      NewPlayingAnimationUse := false;
      if PlayingAnimationNode <> nil then
      begin
        { We want to stop old PlayingAnimationNode from sending any further
          elapsedTime events (elapsedTime causes also fraction_changed).
          Sending of further elapsedTime means that two animations try to play
          at the same time, and modify the same transforms.

          One way to stop animation is to set

            PlayingAnimationNode.StopTime := Time;

          While it works in practice (no reproducible bug with it found),
          it has 2 problems:

          - There will be 1 additional frame when TimeSensor "finishes of"
            the animation, by remaining "TimeIncrease - (NewTime - FdStopTime.Value)"
            duration. This isn't a problem in practice, since it only happens for 1 frame
            (then the old TimeSensor becomes inactive), but it stil feels dirty/wasteful.

          - In case you will move time backwards, e.g. by ResetTime or ResetTimeAtLoad,
            you may get into trouble.
            You may accidentally activate a time sensor that is
            inactive now, but was active long time ago.

          To overcome the 2nd problem, we tried the trick below.
          It will work assuming you never reset time to something < 1.
          (If you do reset time to something very small, then typical TimeSensors will
          have problems anyway,
          see https://castle-engine.io/x3d_time_origin_considered_uncomfortable.php ).

            PlayingAnimationNode.StartTime := 0;
            PlayingAnimationNode.StopTime := 1;
            PlayingAnimationNode.Loop := false;

          But this:
          - Still has the "1 additional frame when TimeSensor finishes of" dirtyness.
          - Also, setting startTime while time-dependent node is active is ignored,
            X3D spec requires this, see our TSFTimeIgnoreWhenActive implementation.
            (bug reproduction: escape_universe, meteorite_1 dying).
            Although we remove this problem by NeverIgnore hack.
          - (Obsolete argument:) Also, it's bad to unconditionally set "Loop" value.
            If user is using paDefault for animation, (s)he expects
            that PlayAnimation doesn't change it ever.

          So it's simpest and reliable to just set enabled=false on old TimeSensor.
          TInternalTimeDependentHandler.SetTime then guarantees it will immediately stop
          sending elapsedTime events. }

        PlayingAnimationNode.Enabled := false;

        if Assigned(PlayingAnimationStopNotification) then
        begin
          PlayingAnimationStopNotification(Self, PlayingAnimationNode);
          PlayingAnimationStopNotification := nil;
        end;
        PlayingAnimationNode.EventIsActive.RemoveNotification(@PlayingAnimationIsActive);
      end;
      Assert(PlayingAnimationStopNotification = nil);

      { If calling PlayAnimation on already-playing node,
        we have to make sure it actually starts playing from the start.
        For the StartTime below to be correctly applied, we have to make
        sure the node actually *stops* temporarily (otherwise the
        TInternalTimeDependentHandler.SetTime never "sees" the node
        in the Enabled = false state). }
      if PlayingAnimationNode = NewPlayingAnimationNode then
        PlayingAnimationNode.InternalTimeDependentHandler.SetTime(Time, 0, false);

      { set PreviousPlayingAnimationXxx }
      PreviousPlayingAnimation := PlayingAnimationNode;
      if PreviousPlayingAnimation <> nil then
      begin
        PreviousPlayingAnimationLoop := PreviousPlayingAnimation.Loop;
        PreviousPlayingAnimationForward := PreviousPlayingAnimation.FractionIncreasing;
        PreviousPlayingAnimationTimeInAnimation :=
          PreviousPlayingAnimation.InternalTimeDependentHandler.ElapsedTime;
      end;

      PlayingAnimationNode := NewPlayingAnimationNode;

      if PlayingAnimationNode <> nil then
      begin
        PlayingAnimationNode.Loop := NewPlayingAnimationLoop;
        PlayingAnimationNode.FractionIncreasing := NewPlayingAnimationForward;
        PlayingAnimationNode.Enabled := true;

        { Assign these before PartialSendBegin/End during StartTime assignment. }
        PlayingAnimationTransitionDuration := NewPlayingAnimationTransitionDuration;
        { save current Time to own variable,
          do not trust PlayingAnimationNode.StartTime to remain reliable,
          it can be changed by user later. }
        PlayingAnimationStartTime := Time;

        { Disable the "ignore" mechanism, otherwise
          setting startTime on a running TimeSensor would be ignored.
          Testcase: e.g. mana animation on dark_dragon and dragon_squash. }
        Inc(PlayingAnimationNode.FdStopTime.NeverIgnore);
        Inc(PlayingAnimationNode.FdStartTime.NeverIgnore);

        { Assign StopTime and StartTime. }
        PlayingAnimationNode.StopTime := 0;
        PlayingAnimationNode.StartTime := Time - NewPlayingAnimationInitialTime;

        { Enable the "ignore" mechanism again, to follow X3D spec. }
        Dec(PlayingAnimationNode.FdStopTime.NeverIgnore);
        Dec(PlayingAnimationNode.FdStartTime.NeverIgnore);

        PlayingAnimationNode.EventIsActive.AddNotification(@PlayingAnimationIsActive);
        PlayingAnimationStopNotification := NewPlayingAnimationStopNotification;
      end;
    end;
  end;

  { Call SetTime on all TimeDependentHandlers. }
  procedure UpdateTimeDependentHandlers(const ExtraTimeIncrease: TFloatTime);
  var
    SomethingVisibleChanged, SomePartialSend: boolean;
    I: Integer;
    T: TFloatTime;
    TimeHandler: TInternalTimeDependentHandler;
  begin
    SomethingVisibleChanged := false;
    T := Time;
    SomePartialSend := false;

    for I := 0 to TimeDependentHandlers.Count - 1 do
    begin
      TimeHandler := TimeDependentHandlers[I];

      PartialSendBegin(TimeHandler);
      if TimeHandler.PartialSend <> nil then
        SomePartialSend := true;

      if TimeHandler.SetTime(T, TimeIncrease + ExtraTimeIncrease, ResetTime) and
        (TimeHandler.Node is TMovieTextureNode) then
        SomethingVisibleChanged := true;

      PartialSendEnd(TimeHandler);
    end;

    if not SomePartialSend then
      NoPartialSend;

    { If SomethingVisibleChanged (in MovieTexture nodes), we have to redisplay. }
    if SomethingVisibleChanged then
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;

  { Call UpdateTimeDependentHandlers, but only if AnimateOnlyWhenVisible logic agrees.
    Note that ResetTime = true forces always an UpdateTimeDependentHandlers(0.0) call. }
  procedure UpdateTimeDependentHandlersIfVisible;
  begin
    if FAnimateOnlyWhenVisible and (not IsVisibleNow) and (not ResetTime) then
      FAnimateGatheredTime := FAnimateGatheredTime + TimeIncrease else
    if (AnimateSkipNextTicks <> 0) and (not ResetTime) then
    begin
      Dec(AnimateSkipNextTicks);
      FAnimateGatheredTime := FAnimateGatheredTime + TimeIncrease;
    end else
    begin
      if ResetTime then
        FAnimateGatheredTime := 0;
      UpdateTimeDependentHandlers(FAnimateGatheredTime);
      AnimateSkipNextTicks := AnimateSkipTicks;
      FAnimateGatheredTime := 0;
    end;
    IsVisibleNow := false;
  end;

  { Call humanoids AnimateSkin.
    This could actually be done from anywhere, as long as it gets called
    fairly soon after every HAnimJoint animation. }
  procedure UpdateHumanoidSkin;
  var
    I: Integer;
    ChangedSkin: TMFVec3f;
  begin
    for I := 0 to ScheduledHumanoidAnimateSkin.Count - 1 do
    begin
      ChangedSkin := (ScheduledHumanoidAnimateSkin.Items[I]
        as THAnimHumanoidNode).AnimateSkin;
      if ChangedSkin <> nil then
        ChangedSkin.Changed;
    end;
    ScheduledHumanoidAnimateSkin.Count := 0;
  end;

  { Process TransformationDirty at the end of increasing time, to apply scheduled
    TransformationDirty in the same Update, as soon as possible
    (useful e.g. for mana shot animation in dragon_squash). }
  procedure UpdateTransformationDirty;
  begin
    if TransformationDirty <> [] then
    begin
      RootTransformationChanged(TransformationDirty);
      TransformationDirty := [];
    end;
  end;

begin
  FTimeNow.Seconds := NewValue;
  FTimeNow.PlusTicks := 0; // using InternalSetTime always resets PlusTicks

  if ProcessEvents then
  begin
    BeginChangesSchedule;
    try
      UpdateNewPlayingAnimation;
      UpdateTimeDependentHandlersIfVisible;
      UpdateHumanoidSkin;
      UpdateTransformationDirty;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TCastleSceneCore.SetTime(const NewValue: TFloatTime);
var
  TimeIncrease: TFloatTime;
begin
  TimeIncrease := NewValue - FTimeNow.Seconds;
  if TimeIncrease > 0 then
    InternalSetTime(NewValue, TimeIncrease, false);
end;

procedure TCastleSceneCore.IncreaseTime(const TimeIncrease: TFloatTime);
begin
  if TimeIncrease > 0 then
    InternalSetTime(FTimeNow.Seconds + TimeIncrease, TimeIncrease, false);
end;

procedure TCastleSceneCore.ResetLastEventTime(Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to Node.RoutesCount - 1 do
    Node.Routes[I].ResetLastEventTime;
  if Node is TAbstractScriptNode then
    TAbstractScriptNode(Node).ResetLastEventTimes;
end;

procedure TCastleSceneCore.ResetTime(const NewValue: TFloatTime);
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes(@ResetLastEventTime, false);
  InternalSetTime(NewValue, 0, true);
end;

procedure TCastleSceneCore.ResetTimeAtLoad;
begin
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top is TKambiNavigationInfoNode) and
     TKambiNavigationInfoNode(NavigationInfoStack.Top).TimeOriginAtLoad then
    FTimeAtLoad := 0.0 else
    FTimeAtLoad := DateTimeToUnix(Now);
  ResetTime(TimeAtLoad);
end;

procedure TCastleSceneCore.IncreaseTimeTick;
begin
  Inc(FTimeNow.PlusTicks);
end;

procedure TCastleSceneCore.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  SP: Single;
begin
  inherited;
  if not GetExists then Exit;

  { in case the same scene is present many times on SceneManager.Items list,
    do not process it's Update() many times (would cause time to move too fast). }
  if LastUpdateFrameId = TFramesPerSecond.FrameId then Exit;
  LastUpdateFrameId := TFramesPerSecond.FrameId;

  { Most of the "update" job happens inside InternalSetTime.
    Reasons are partially historiec: TCastlePrecalculatedAnimation
    called only SetTime, not Update. }

  { Ignore calls when SecondsPassed is precisely zero
    (this may happen, and is correct, see TFramesPerSecond.ZeroNextSecondsPassed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway.

    (Well, time dependent nodes like TimeSensor could "realize" that startTime
    happened *now*, and send initial events to start animation.
    But actually we take care of it in HandleChangeTimeStopStart.) }
  SP := TimePlayingSpeed * SecondsPassed;
  if TimePlaying and (SP <> 0) then
    IncreaseTime(SP);
end;

procedure TCastleSceneCore.PartialSendBegin(const TimeHandler: TInternalTimeDependentHandler);
var
  T: TFloatTime;
  PartialSend: TPartialSend;
begin
  T := Time;

  if (PlayingAnimationTransitionDuration <> 0) { often early exit } and
     (TimeHandler.Node = PlayingAnimationNode) and
     (T >= PlayingAnimationStartTime) { only sanity check } and
     (T < PlayingAnimationStartTime + PlayingAnimationTransitionDuration) then
  begin
    PartialSend := TPartialSend.Create;
    TimeHandler.PartialSend := PartialSend;

    if PreviousPlayingAnimation <> nil then
    begin
      PartialSend.Partial := 1 - (T - PlayingAnimationStartTime) /
        PlayingAnimationTransitionDuration;

      { First fade-out previous animation.
        Note that
        - PreviousPlayingAnimation may be equal to PlayingAnimationNode
          (in case when PlayAnimation starts the same animation from beginning)
          and we still must work correctly.
        - We do not use FadingOutAnimation.InternalTimeDependentHandler.SetTime,
          as we do not want to activate/deactivate PreviousPlayingAnimation
          time sensor,
          and we do not want to look at enabled state of time sensor.
      }
      PreviousPlayingAnimation.FakeTime(PreviousPlayingAnimationTimeInAnimation
        + (T - PlayingAnimationStartTime),
        PreviousPlayingAnimationLoop,
        PreviousPlayingAnimationForward,
        NextEventTime,
        PartialSend);
    end;

    { calculate Result.Partial that should be passed
      to PlayingAnimationNode }
    PartialSend.Partial := (T - PlayingAnimationStartTime) /
      PlayingAnimationTransitionDuration;
  end else
    TimeHandler.PartialSend := nil;
end;

procedure TCastleSceneCore.PartialSendEnd(const TimeHandler: TInternalTimeDependentHandler);

  function FieldsEqual(const L1, L2: TX3DFieldList): boolean;
  var
    I: Integer;
  begin
    Result := L1.Count = L2.Count;
    if Result then
      for I := 0 to L1.Count - 1 do
        if L1[I] <> L2[I] then
          Exit(false);
  end;

var
  F: TX3DField;
  AffectedFields: TX3DFieldList;
begin
  if TimeHandler.PartialSend <> nil then
  begin
    AffectedFields := TimeHandler.PartialSend.AffectedFields;
    TimeHandler.PartialSend.AffectedFields := nil; // do not free by TPartialSend.Destroy
    FreeAndNil(TimeHandler.PartialSend);

    { call InternalAffectedPartial on all AffectedFields, to correctly update
      their values. }
    for F in AffectedFields do
      F.InternalAffectedPartial;

    { call InternalRemovePartial on all fields affected in previous frame,
      but not affected this frame. }
    if PreviousPartialAffectedFields <> nil then
    begin
      { optimize the most common case, when PreviousPartialAffectedFields
        is equal to AffectedFields. }
      if not FieldsEqual(PreviousPartialAffectedFields, AffectedFields) then
        for F in PreviousPartialAffectedFields do
          if not AffectedFields.Contains(F) then
            F.InternalRemovePartial;
      FreeAndNil(PreviousPartialAffectedFields);
    end;

    PreviousPartialAffectedFields := AffectedFields;
  end;
end;

procedure TCastleSceneCore.NoPartialSend;
var
  F: TX3DField;
begin
  if PreviousPartialAffectedFields <> nil then
  begin
    for F in PreviousPartialAffectedFields do
      F.InternalRemovePartial;
    FreeAndNil(PreviousPartialAffectedFields);
  end;
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

procedure TCastleSceneCore.ProximitySensorUpdate(const PSI: TProximitySensorInstance;
  const CameraVectors: TCameraVectors);
var
  APosition, ADirection, AUp: TVector3;
  ProxNode: TProximitySensorNode;
  NewIsActive: boolean;
begin
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
        we can gracefully react in InternalChangedField to changes:
        - changes to ProximitySensor center and size must only produce
          new ProximitySensorUpdate to eventually activate/deactivate ProximitySensor
        - changes to transforms affecting ProximitySensor must only update
          it's InvertedTransform and call ProximitySensorUpdate.
      }

      APosition := PSI.InvertedTransform.MultPoint(CameraVectors.Position);

      NewIsActive :=
        (APosition[0] >= ProxNode.FdCenter.Value[0] - ProxNode.FdSize.Value[0] / 2) and
        (APosition[0] <= ProxNode.FdCenter.Value[0] + ProxNode.FdSize.Value[0] / 2) and
        (APosition[1] >= ProxNode.FdCenter.Value[1] - ProxNode.FdSize.Value[1] / 2) and
        (APosition[1] <= ProxNode.FdCenter.Value[1] + ProxNode.FdSize.Value[1] / 2) and
        (APosition[2] >= ProxNode.FdCenter.Value[2] - ProxNode.FdSize.Value[2] / 2) and
        (APosition[2] <= ProxNode.FdCenter.Value[2] + ProxNode.FdSize.Value[2] / 2) and
        { ... and the box is not empty, which for ProximitySensor
          is signalled by any size <= 0 (yes, equal 0 also means empty).
          We check this at the end, as this is the least common situation? }
        (ProxNode.FdSize.Value[0] > 0) and
        (ProxNode.FdSize.Value[1] > 0) and
        (ProxNode.FdSize.Value[2] > 0);

      if NewIsActive <> PSI.IsActive then
      begin
        PSI.IsActive := NewIsActive;
        ProxNode.EventIsActive.Send(NewIsActive, NextEventTime);
        if NewIsActive then
          ProxNode.EventEnterTime.Send(Time, NextEventTime) else
          ProxNode.EventExitTime.Send(Time, NextEventTime);
      end;

      { Call position_changed, orientation_changed, even if this is just
        the first time NewIsActive = true (that is, even when it was
        NewIsActive <> PSI.IsActive). Reasoning: this allows to activate
        ProximitySensor when world starts (before player moves),
        which is a wanted feature. }

      if NewIsActive then
      begin
        ProxNode.EventPosition_Changed.Send(APosition, NextEventTime);
        if ProxNode.EventOrientation_Changed.SendNeeded then
        begin
          ADirection := PSI.InvertedTransform.MultDirection(CameraVectors.Direction);
          AUp        := PSI.InvertedTransform.MultDirection(CameraVectors.Up);
          ProxNode.EventOrientation_Changed.Send(
            OrientationFromDirectionUp(ADirection, AUp), NextEventTime);
        end;
        { TODO: centerOfRotation_changed }
      end;
    finally
      EndChangesSchedule;
    end;
  end;
end;

{ camera --------------------------------------------------------------------- }

function TCastleSceneCore.GetCameraLocal(
  out CameraVectors: TCameraVectors): boolean;
begin
  // note that HasWorldTransform implies also World <> nil
  Result := HasWorldTransform and World.CameraKnown;
  if Result then
  begin
    CameraVectors.Position  := WorldInverseTransform.MultPoint    (World.CameraPosition);
    CameraVectors.Direction := WorldInverseTransform.MultDirection(World.CameraDirection);
    CameraVectors.Up        := WorldInverseTransform.MultDirection(World.CameraUp);
  end;
end;

function TCastleSceneCore.GetCameraLocal(
  out CameraLocalPosition: TVector3): boolean;
begin
  // note that HasWorldTransform implies also World <> nil
  Result := HasWorldTransform and World.CameraKnown;
  if Result then
    CameraLocalPosition := WorldInverseTransform.MultPoint(World.CameraPosition);
end;

procedure TCastleSceneCore.ChangedTransform;
begin
  inherited;
  { WorldInverseTransform changed, so update things depending on camera view.
    TODO: This way doesn't make a notification when WorldInverseTransform
    changed because parent transform changed.
    We should look at FWorldTransformAndInverseId changes to detect this? }
  UpdateCameraEvents;
end;

procedure TCastleSceneCore.ChangeWorld(const Value: TSceneManagerWorld);
begin
  inherited;
  { World changed, so update things depending on camera view }
  if Value <> nil then
    UpdateCameraEvents;
end;

function TCastleSceneCore.CameraPosition: TVector3;
begin
  Result := World.CameraPosition;
end;

function TCastleSceneCore.CameraDirection: TVector3;
begin
  Result := World.CameraDirection;
end;

function TCastleSceneCore.CameraUp: TVector3;
begin
  Result := World.CameraUp;
end;

function TCastleSceneCore.CameraViewKnown: boolean;
begin
  Result := (World <> nil) and World.CameraKnown;
end;

procedure TCastleSceneCore.CameraChanged(ACamera: TCamera);
begin
  inherited;
  UpdateCameraEvents;

  { handle WatchForTransitionComplete, looking at ACamera.Animation }
  if ProcessEvents and WatchForTransitionComplete and not ACamera.Animation then
  begin
    BeginChangesSchedule;
    try
      WatchForTransitionComplete := false;
      if NavigationInfoStack.Top <> nil then
        NavigationInfoStack.Top.EventTransitionComplete.Send(true, NextEventTime);
    finally EndChangesSchedule end;
  end;
end;

procedure TCastleSceneCore.UpdateCameraEvents;

  { Does CameraProcessing needs to be called (does it actually do anything). }
  function CameraProcessingNeeded: boolean;
  begin
    Result :=
      (ShapeLODs.Count <> 0) or
      (ProximitySensors.Count <> 0) or
      (BillboardInstancesList.Count <> 0);
  end;

  { Update things depending on camera information and X3D events.
    Call it only when ProcessEvents. }
  procedure CameraProcessing(const CameraVectors: TCameraVectors);
  var
    I: Integer;
  begin
    Assert(ProcessEvents);

    for I := 0 to ShapeLODs.Count - 1 do
      UpdateLODLevel(TShapeTreeLOD(ShapeLODs.Items[I]), CameraVectors.Position);

    for I := 0 to ProximitySensors.Count - 1 do
      ProximitySensorUpdate(ProximitySensors[I], CameraVectors);

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
      (BillboardInstancesList[I] as TBillboardNode).CameraChanged(CameraVectors);
      { TODO: use OptimizeExtensiveTransformations? }
      TransformationChanged(BillboardInstancesList[I],
        BillboardInstancesList[I].ShapeTrees as TShapeTreeList,
        [chTransform]);
    end;
  end;

var
  CameraVectors: TCameraVectors;
begin
  if ProcessEvents and
     CameraProcessingNeeded and
     { call GetCameraLocal only when necessary, as it does some calculations }
     GetCameraLocal(CameraVectors) then
  begin
    BeginChangesSchedule;
    try
      CameraProcessing(CameraVectors);
    finally EndChangesSchedule end;
  end;
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
    if CompiledScriptHandlers.List^[I].Name = HandlerName then
    begin
      CompiledScriptHandlers.List^[I].Handler(ReceivedValue, NextEventTime);
      Break;
    end;
end;

{ camera ------------------------------------------------------------------ }

function TCastleSceneCore.NavigationTypeFromNavigationInfo: TNavigationType;

  function StringToNavigationType(const S: string;
    out NavigationType: TNavigationType): boolean;
  begin
    Result := false;
    if S = 'WALK' then
    begin
      Result := true;
      NavigationType := ntWalk;
    end else
    if S = 'FLY' then
    begin
      Result := true;
      NavigationType := ntFly
    end else
    if S = 'NONE' then
    begin
      Result := true;
      NavigationType := ntNone
    end else
    if (S = 'EXAMINE') or
       (S = 'LOOKAT') then
    begin
      if S = 'LOOKAT' then
        WritelnWarning('VRML/X3D', 'TODO: Navigation type "LOOKAT" is not yet supported, treating like "EXAMINE"');
      Result := true;
      NavigationType := ntExamine;
    end else
    if (S = 'ARCHITECTURE') or
       (S = 'TURNTABLE') then
    begin
      Result := true;
      NavigationType := ntTurntable
    end else
    if S = 'ANY' then
    begin
      { Do nothing, also do not report this NavigationInfo.type as unknown. }
    end else
      WritelnWarning('VRML/X3D', 'Unknown NavigationInfo.type "%s"', [S]);
  end;

var
  I: Integer;
  NavigationNode: TNavigationInfoNode;
begin
  NavigationNode := NavigationInfoStack.Top;
  if NavigationNode <> nil then
    for I := 0 to NavigationNode.FdType.Count - 1 do
      if StringToNavigationType(NavigationNode.FdType.Items[I], Result) then
        Exit;

  { No recognized "type" found, so use default type EXAMINE. }
  Result := ntExamine;
end;

procedure TCastleSceneCore.CameraFromNavigationInfo(
  Camera: TCamera; const WorldBox: TBox3D);
var
  NavigationNode: TNavigationInfoNode;
  Radius: Single;
begin
  NavigationNode := NavigationInfoStack.Top;

  { calculate Radius }
  Radius := 0;
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 1) then
    Radius := NavigationNode.FdAvatarSize.Items[0];
  { if avatarSize doesn't specify Radius, or specifies invalid <= 0,
    calculate something suitable based on Box. }
  if Radius <= 0 then
    Radius := WorldBox.AverageSize(false, 1.0) * 0.005;
  Camera.Radius := Radius;

  { Note that we cannot here conditionally set some properties
    e.g. only if "Camera is TWalkCamera".
    This would mean that e.g. value of PreferredHeight
    (from NavigationInfo.avatarSize) is lost when NavigationInfo.type="EXAMINE",
    even if developer later switches NavigationType to Walk. }

  { calculate Camera.PreferredHeight }
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 2) then
    Camera.PreferredHeight := NavigationNode.FdAvatarSize.Items[1]
  else
    { Make it something >> Radius * 2, to allow some
      space to decrease (e.g. by Input_DecreasePreferredHeight
      in view3dscene). Remember that CorrectPreferredHeight
      adds a limit to PreferredHeight, around Radius * 2. }
    Camera.PreferredHeight := Radius * 4;

  Camera.CorrectPreferredHeight;

  { calculate Camera.ClimbHeight }
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 3) then
    Camera.ClimbHeight := Max(NavigationNode.FdAvatarSize.Items[2], 0.0)
  else
    Camera.ClimbHeight := 0;

  { calculate Camera.HeadBobbing* }
  if (NavigationNode <> nil) and
     (NavigationNode is TKambiNavigationInfoNode) then
  begin
    Camera.HeadBobbing := TKambiNavigationInfoNode(NavigationNode).FdHeadBobbing.Value;
    Camera.HeadBobbingTime := TKambiNavigationInfoNode(NavigationNode).FdHeadBobbingTime.Value;
  end else
  begin
    Camera.HeadBobbing := TWalkCamera.DefaultHeadBobbing;
    Camera.HeadBobbingTime := TWalkCamera.DefaultHeadBobbingTime;
  end;

  { calculate Camera.MoveSpeed }
  if NavigationNode = nil then
    { Since we don't have NavigationNode.speed, we just calculate some
      speed that should "feel sensible". We base it on Radius,
      that was set above. }
    Camera.MoveSpeed := Camera.Radius * 20
  else
    { This is OK, also for NavigationNode.FdSpeed.Value = 0 case. }
    Camera.MoveSpeed := NavigationNode.FdSpeed.Value;

  Camera.ModelBox := WorldBox;

  { No point in calling TWalkCamera.Init or TExamineCamera.Init here:
    this method, together with CameraFromViewpoint
    (with RelativeCameraTransform = false),
    together initialize everything that Init does. }
end;

procedure TCastleSceneCore.CameraFromViewpoint(ACamera: TCamera;
  const RelativeCameraTransform, AllowTransitionAnimate: boolean);
var
  APosition: TVector3;
  ADirection: TVector3;
  AUp: TVector3;
  GravityUp: TVector3;
begin
  if ViewpointStack.Top <> nil then
    ViewpointStack.Top.GetView(APosition, ADirection, AUp, GravityUp) else
  begin
    { Suitable viewpoint,
      with dir -Z and up +Y (like standard VRML/X3D viewpoint) }
    CameraViewpointForWholeScene(ACamera.ModelBox, 2, 1, false, true,
      APosition, ADirection, AUp, GravityUp);
  end;

  ACamera.GravityUp := GravityUp;

  { If RelativeCameraTransform, then we will move relative to
    initial camera changes. Else, we will jump to new initial camera vectors. }
  ACamera.SetInitialView(APosition, ADirection, AUp, RelativeCameraTransform);
  if not RelativeCameraTransform then
  begin
    if AllowTransitionAnimate and (not ForceTeleportTransitions) then
      CameraTransition(ACamera, APosition, ADirection, AUp)
    else
      ACamera.SetView(APosition, ADirection, AUp);
  end;
end;

procedure TCastleSceneCore.CameraTransition(Camera: TCamera;
  const APosition, ADirection, AUp: TVector3);
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
        WritelnWarning('VRML/X3D', Format('Unrecognized transitionType "%s"', [TransitionType]));
    end;

  { calculate TransitionTime }
  if NavigationNode <> nil then
    TransitionTime := NavigationNode.FdTransitionTime.Value else
    TransitionTime := 1;

  { correct TransitionAnimate in case TransitionTime invalid }
  if TransitionTime <= 0 then
    TransitionAnimate := false;

  if TransitionAnimate then
  begin
    Camera.AnimateTo(APosition, ADirection, AUp, TransitionTime);
    WatchForTransitionComplete := true;
  end else
  begin
    Camera.SetView(APosition, ADirection, AUp);
    if NavigationInfoStack.Top <> nil then
      NavigationInfoStack.Top.EventTransitionComplete.Send(true, NextEventTime);
  end;
end;

procedure TCastleSceneCore.CameraTransition(Camera: TCamera;
  const APosition, ADirection, AUp, GravityUp: TVector3);
begin
  Camera.GravityUp := GravityUp;
  CameraTransition(Camera, APosition, ADirection, AUp);
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

procedure TCastleSceneCore.CalculateMainLightForShadowsPosition;
begin
  if FMainLightForShadowsNode is TAbstractPositionalLightNode then
    FMainLightForShadows := Vector4(
      FMainLightForShadowsTransform.MultPoint(
        TAbstractPositionalLightNode(FMainLightForShadowsNode).FdLocation.Value), 1) else
  if FMainLightForShadowsNode is TAbstractDirectionalLightNode then
    FMainLightForShadows := Vector4(
      FMainLightForShadowsTransform.MultDirection(
        TAbstractDirectionalLightNode(FMainLightForShadowsNode).FdDirection.Value).Normalize, 0) else
    raise Exception.CreateFmt('TCastleSceneCore.MainLightForShadows: ' +
      'light node "%s" cannot be used to cast shadows, it has no position ' +
      'and no direction', [FMainLightForShadowsNode.X3DType]);
end;

function TCastleSceneCore.SearchMainLightForShadows(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
var
  L: TAbstractLightNode absolute Node;
begin
  if L.FdShadowVolumes.Value and
     L.FdShadowVolumesMain.Value then
  begin
    FMainLightForShadowsNode := L;
    FMainLightForShadowsTransform := StateStack.Top.Transform;
    FMainLightForShadowsExists := true;
    CalculateMainLightForShadowsPosition;
    Result := Node; // anything non-nil to break traversing
  end else
    Result := nil;
end;

procedure TCastleSceneCore.ValidateMainLightForShadows;

  procedure CalculateMainLightForShadows;
  begin
    FMainLightForShadowsExists := false;
    if RootNode <> nil then
      RootNode.Traverse(TAbstractLightNode, @SearchMainLightForShadows);
  end;

begin
  if not (fvMainLightForShadows in Validities) then
  begin
    CalculateMainLightForShadows;
    Include(Validities, fvMainLightForShadows);
  end;
end;

function TCastleSceneCore.MainLightForShadows(
  out AMainLightPosition: TVector4): boolean;
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

procedure TCastleSceneCore.PrepareResources(const Options: TPrepareResourcesOptions;
  const ProgressStep: boolean; const Params: TPrepareParams);

  { PrepareShapesOctrees and PrepareShadowVolumes could be optimized
    into one run }

  procedure PrepareShapesOctrees;
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        SI.Current.InternalOctreeTriangles;
    finally FreeAndNil(SI) end;
  end;

  procedure PrepareShadowVolumes;
  var
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false);
    try
      while SI.GetNext do
        SI.Current.InternalShadowVolumes.PrepareResources;
    finally FreeAndNil(SI) end;
  end;

begin
  inherited;

  if prBoundingBox in Options then
    LocalBoundingBox { ignore the result };

  if prShadowVolume in Options then
    PrepareShadowVolumes;

  if prSpatial in Options then
  begin
    InternalOctreeRendering;
    InternalOctreeDynamicCollisions;
    InternalOctreeVisibleTriangles;
    InternalOctreeStaticCollisions;
    PrepareShapesOctrees;
  end;
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
    Result := URICaption(URL);
end;

function TCastleSceneCore.Node(const NodeName: string): TX3DNode;
begin
  if RootNode = nil then
    raise EX3DNotFound.CreateFmt('Cannot find node "%s"', [NodeName]) else
    Result := RootNode.FindNodeByName(TX3DNode, NodeName, false);
end;

function TCastleSceneCore.Field(const NodeName, FieldName: string): TX3DField;
begin
  Result := Node(NodeName).Field(FieldName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Field name "%s" not found', [FieldName]);
end;

function TCastleSceneCore.Event(const NodeName, EventName: string): TX3DEvent;
begin
  Result := Node(NodeName).AnyEvent(EventName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Event name "%s" not found', [EventName]);
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

function TCastleSceneCore.ViewpointsCount: Cardinal;
begin
  Result := FViewpointsArray.Count;
end;

function TCastleSceneCore.GetViewpointName(Idx: integer): string;
begin
  if Between(Idx, 0, FViewpointsArray.Count - 1) then
    Result := FViewpointsArray[Idx].SmartDescription else
    Result := '';
end;

procedure TCastleSceneCore.MoveToViewpoint(Idx: integer; Animated: boolean);
var
  OldForceTeleport: boolean;
begin
  if Between(Idx, 0, FViewpointsArray.Count - 1) then
  begin
    if not Animated then
    begin
      OldForceTeleport := ForceTeleportTransitions;
      ForceTeleportTransitions := true;
    end;

    if FViewpointsArray[Idx] = FViewpointStack.Top then
      FViewpointsArray[Idx].Bound := false;
    FViewpointsArray[Idx].Bound := true;

    if not Animated then
      ForceTeleportTransitions := OldForceTeleport;
  end;
end;

procedure TCastleSceneCore.AddViewpointFromCamera(ACamera: TCamera; AName: string);
var
  APosition: TVector3;
  ADirection: TVector3;
  AUp: TVector3;
  GravityUp: TVector3;
  Version: TX3DCameraVersion;
  NewViewNode: TAbstractChildNode;
  NewViewpointNode: TAbstractViewpointNode;
  NavigationType: string;
  Walk: TWalkCamera;
  Examine: TExamineCamera;
  WalkSpeed, VisibilityLimit: Single;
  AvatarSize: TVector3;
  NewNavigationNode: TNavigationInfoNode;
  NewGroupNode: TGroupNode;
  NewRoute: TX3DRoute;
begin
  if RootNode = nil then
    raise Exception.Create('You have to initialize RootNode, usually just by loading some scene to TCastleSceneCore.Load, before adding viewpoints');

  ACamera.GetView(APosition, ADirection, AUp, GravityUp);

  if RootNode.HasForceVersion and (RootNode.ForceVersion.Major <= 1) then
    Version := cvVrml1_Inventor else
    Version := cvVrml2_X3d;
  NewViewNode := MakeCameraNode(Version, '', APosition, ADirection, AUp, GravityUp,
    NewViewpointNode);
  NewViewpointNode.FdDescription.Value := AName;
  NewViewpointNode.X3DName := 'Viewpoint' + IntToStr(Random(10000));
  NewViewpointNode.Scene := self;

  { Create NavigationInfo node }
  Walk := nil;
  Examine := nil;
  if ACamera is TWalkCamera then
  begin
    Walk := ACamera as TWalkCamera;
    if Walk.Gravity then
      NavigationType := 'WALK'
    else
      NavigationType := 'FLY';
  end else
  if ACamera is TExamineCamera then
  begin
    Examine := ACamera as TExamineCamera;
    if Examine.Turntable then
      NavigationType := 'TURNTABLE'
    else
      NavigationType := 'EXAMINE';
  end;

  AvatarSize[0] := ACamera.Radius;
  if Walk <> nil then begin
    WalkSpeed := Walk.MoveSpeed;
    AvatarSize[1] := Walk.PreferredHeight;
    AvatarSize[2] := Walk.ClimbHeight;
  end
  else begin
    WalkSpeed := 0;
    AvatarSize[1] := 0;
    AvatarSize[2] := 0;
  end;
  VisibilityLimit := 0;

  NewNavigationNode := MakeCameraNavNode(Version, '', NavigationType, WalkSpeed,
    VisibilityLimit, AvatarSize, HeadlightOn);
  NewNavigationNode.X3DName := 'NavInfo' + IntToStr(Random(10000));
  NewNavigationNode.Scene := self;

  // Connect viewpoint with navigation info
  NewRoute := TX3DRoute.Create;
  NewRoute.SetSourceDirectly(NewViewpointNode.EventIsBound);
  NewRoute.SetDestinationDirectly(NewNavigationNode.EventSet_Bind);

  // Add both nodes to the scene
  NewGroupNode := TGroupNode.Create;
  NewGroupNode.AddChildren(NewViewNode);
  NewGroupNode.AddChildren(NewNavigationNode);
  NewGroupNode.AddRoute(NewRoute);

  RootNode.AddChildren(NewGroupNode);
  { The 100% safe version would now call RootNode.FdChildren.Changed,
    and it would automatically refresh FViewpointsArray.
    But RootNode.FdChildren.Changed is a little costly, it processes the whole
    scene again, so it's faster to directly add to FViewpointsArray. }
  FViewpointsArray.Add(NewViewpointNode);
end;

type
  TAnimationsEnumerator = class
    //Parent: TCastleSceneCore;
    AnimationPrefix: string;
    List: TStringList;
    procedure EnumerateWithAlias(const Node: TX3DNode; const NodeName: string;
      const Overwrite: boolean);
    procedure Enumerate(Node: TX3DNode);
  end;

procedure TAnimationsEnumerator.Enumerate(Node: TX3DNode);
begin
  EnumerateWithAlias(Node, Node.X3DName, false);
end;

procedure TAnimationsEnumerator.EnumerateWithAlias(const Node: TX3DNode;
  const NodeName: string; const Overwrite: boolean);
var
  AnimationName: string;
  ExistingIndex: Integer;
begin
  if IsPrefix(AnimationPrefix, NodeName, false) then
  begin
    AnimationName := PrefixRemove(AnimationPrefix, NodeName, false);
    if AnimationName = '' then
    begin
      if AnimationPrefix <> '' then // this is normal with AnimationPrefix = '' on many scenes
        WritelnWarning('Named Animations', Format('TimeSensor node name is exactly "%s", this indicates named animation with empty name, ignoring',
          [AnimationName]));
      Exit;
    end;
    ExistingIndex := List.IndexOf(AnimationName);
    if ExistingIndex <> -1 then
    begin
      if (not Overwrite) and (AnimationPrefix <> '') then
        { Warn in case of multiple animation (TimeSensor) names
          only when AnimationPrefix set.
          Bacause this is normal on some models, and is valid X3D.
          - You can repeat the same node name multiple times in X3D (although
            it's discouraged).
          - You can use PROTO and DEF with TimeSensor inside, and use it multiple
            times. Both these usage scenarios result in multiple instances
            of the animation in scene.
          - Even our own mechanism for renaming castle-anim-frames animations from
            https://github.com/castle-engine/demo-models/blob/master/castle-anim-frames/simple/two_animations.x3dv
            results in multiple "Animation" names in scene. }
        WritelnWarning('Named Animations', Format('Animation name "%s" occurs multiple times in scene',
          [AnimationName]));
      List.Objects[ExistingIndex] := Node;
    end else
    begin
      List.AddObject(AnimationName, Node);
    end;
  end;
end;

function TCastleSceneCore.Animations: TStringList;
var
  Enum: TAnimationsEnumerator;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := true; // X3D node names are case-sensitive
  if RootNode <> nil then
  begin
    Enum := TAnimationsEnumerator.Create;
    try
      //Enum.Parent := Self;
      Enum.List := Result;
      Enum.AnimationPrefix := AnimationPrefix;
      RootNode.EnumerateNodes(TTimeSensorNode, @Enum.Enumerate, true);

      { recognize named animations also from IMPORTed node names.
        This alllows to import and rename animations, which is useful. }
      if RootNode.ImportedNames <> nil then
        for I := 0 to RootNode.ImportedNames.Count - 1 do
          if RootNode.ImportedNames[I].Node is TTimeSensorNode then
            Enum.EnumerateWithAlias(
              RootNode.ImportedNames[I].Node,
              RootNode.ImportedNames[I].Name, true);
    finally FreeAndNil(Enum) end;
  end;
end;

function TCastleSceneCore.HasAnimation(const AnimationName: string): boolean;
begin
  Result := FAnimationsList.IndexOf(AnimationName) <> -1;
end;

function TCastleSceneCore.AnimationTimeSensor(const AnimationName: string): TTimeSensorNode;
begin
  Result := AnimationTimeSensor(FAnimationsList.IndexOf(AnimationName));
end;

function TCastleSceneCore.AnimationTimeSensor(const Index: Integer): TTimeSensorNode;
begin
  if Between(Index, 0, FAnimationsList.Count - 1) then
    Result := FAnimationsList.Objects[Index] as TTimeSensorNode else
    Result := nil;
end;

function TCastleSceneCore.ForceAnimationPose(const AnimationName: string;
  const TimeInAnimation: TFloatTime;
  const Looping: TPlayAnimationLooping;
  const Forward: boolean): boolean;
var
  Loop: boolean;
  TimeNode: TTimeSensorNode;
begin
  // calculate Loop
  case Looping of
    paLooping   : Loop := true;
    paNotLooping: Loop := false;
    else
    begin
      TimeNode := AnimationTimeSensor(AnimationName);
      Loop := (TimeNode <> nil) and TimeNode.Loop;
    end;
  end;

  Result := ForceAnimationPose(AnimationName, TimeInAnimation, Loop, Forward);
end;

function TCastleSceneCore.ForceAnimationPose(const AnimationName: string;
  const TimeInAnimation: TFloatTime;
  const Loop: boolean;
  const Forward: boolean): boolean;
var
  Index: Integer;
  TimeNode: TTimeSensorNode;
begin
  Index := FAnimationsList.IndexOf(AnimationName);
  Result := Index <> -1;
  if Result then
  begin
    TimeNode := FAnimationsList.Objects[Index] as TTimeSensorNode;
    Inc(ForceImmediateProcessing);
    try
      TimeNode.FakeTime(TimeInAnimation, Loop, Forward, NextEventTime);
    finally
      Dec(ForceImmediateProcessing);
    end;
  end;
end;

procedure TCastleSceneCore.ForceInitialAnimationPose;
begin
  if NewPlayingAnimationUse then
  begin
    Inc(ForceImmediateProcessing);
    try
      NewPlayingAnimationNode.FakeTime(
        NewPlayingAnimationInitialTime,
        NewPlayingAnimationLoop,
        NewPlayingAnimationForward,
        NextEventTime);
    finally
      Dec(ForceImmediateProcessing);
    end;
  end;
end;

function TCastleSceneCore.PlayAnimation(const AnimationName: string;
  const Looping: TPlayAnimationLooping;
  const Forward: boolean): boolean;
var
  Params: TPlayAnimationParameters;
  Loop: boolean;
  TimeNode: TTimeSensorNode;
begin
  // calculate Loop
  case Looping of
    paLooping   : Loop := true;
    paNotLooping: Loop := false;
    else
    begin
      TimeNode := AnimationTimeSensor(AnimationName);
      Loop := (TimeNode <> nil) and TimeNode.Loop;
    end;
  end;

  Params := TPlayAnimationParameters.Create;
  try
    Params.Name := AnimationName;
    Params.Loop := Loop;
    Params.Forward := Forward;
    Result := PlayAnimation(Params);
  finally FreeAndNil(Params) end;
end;

function TCastleSceneCore.PlayAnimation(const AnimationName: string;
  const Loop: boolean; const Forward: boolean): boolean;
var
  Params: TPlayAnimationParameters;
begin
  Params := TPlayAnimationParameters.Create;
  try
    Params.Name := AnimationName;
    Params.Loop := Loop;
    Params.Forward := Forward;
    Result := PlayAnimation(Params);
  finally FreeAndNil(Params) end;
end;

function TCastleSceneCore.PlayAnimation(
  const Parameters: TPlayAnimationParameters): boolean;
var
  Index: Integer;
begin
  Index := FAnimationsList.IndexOf(Parameters.Name);
  Result := Index <> -1;
  if Result then
  begin
    { We defer actual sending of stopTime and startTime to Update method.
      This way multiple calls to PlayAnimation within the same frame
      behave Ok, only last one matters.

      Otherwise we're left
      with TimeSensor having equal stopTime and startTime on animations
      executed by non-last PlayAnimation call within the same frame,
      and so these animations play (while they should not), simultaneously
      with desired animations.
      (reproduction: escape_universe, boss flying + flying_left/right + dying
      animations.)

      Don't even set TimeNode.Loop here --- setting Loop property
      on a node, and then not controlling it's startTime / stopTime,
      means that it will play infinitely (because the default values
      mean that stopTime is ignored).
    }
    FCurrentAnimation := FAnimationsList.Objects[Index] as TTimeSensorNode;
    NewPlayingAnimationNode := FCurrentAnimation;
    NewPlayingAnimationLoop := Parameters.Loop;
    NewPlayingAnimationForward := Parameters.Forward;
    NewPlayingAnimationStopNotification := Parameters.StopNotification;
    NewPlayingAnimationTransitionDuration := Parameters.TransitionDuration;
    NewPlayingAnimationInitialTime := Parameters.InitialTime;
    NewPlayingAnimationUse := true;
  end;
end;

procedure TCastleSceneCore.PlayingAnimationIsActive(
  Event: TX3DEvent; Value: TX3DField; const ATime: TX3DTime);
var
  Val: boolean;
begin
  Val := (Value as TSFBool).Value;
  if (not Val) and Assigned(PlayingAnimationStopNotification) then
  begin
    PlayingAnimationStopNotification(Self, PlayingAnimationNode);
    { Always after calling PlayingAnimationStopNotification,
      make it nil, to not call it 2nd time when PlayAnimation plays another
      animation. }
    PlayingAnimationStopNotification := nil;
  end;
end;

function TCastleSceneCore.AnimationDuration(const AnimationName: string): TFloatTime;
var
  Index: Integer;
  TimeNode: TTimeSensorNode;
begin
  Index := FAnimationsList.IndexOf(AnimationName);
  if Index <> -1 then
  begin
    TimeNode := FAnimationsList.Objects[Index] as TTimeSensorNode;
    Result := TimeNode.CycleInterval;
  end else
    Result := 0;
end;

procedure TCastleSceneCore.FontChanged_TextNode(Node: TX3DNode);
begin
  (Node as TTextNode).FontChanged;
end;

procedure TCastleSceneCore.FontChanged_AsciiTextNode_1(Node: TX3DNode);
begin
  (Node as TAsciiTextNode_1).FontChanged;
end;

procedure TCastleSceneCore.FontChanged;
begin
  if RootNode <> nil then
  begin
    GLContextClose; // force TGLRenderer.Prepare on shapes
    { Free and recalculate all proxy nodes...
      TODO: this could be done much more efficiently,
      we only need to free proxies on text nodes. }
    ChangedAll;
    RootNode.EnumerateNodes(TTextNode, @FontChanged_TextNode, false);
    RootNode.EnumerateNodes(TAsciiTextNode_1, @FontChanged_AsciiTextNode_1, false);
  end;
end;

function TCastleSceneCore.Clone(const AOwner: TComponent): TCastleSceneCore;
begin
  Result := TComponentClass(ClassType).Create(AOwner) as TCastleSceneCore;
  if RootNode <> nil then
    Result.Load(RootNode.DeepCopy as TX3DRootNode, true);
end;

procedure TCastleSceneCore.LocalRender(const Params: TRenderParams);

  procedure RenderingCameraChanged(const RenderingCamera: TRenderingCamera);
  var
    Viewpoint: TAbstractViewpointNode;
  begin
    Viewpoint := ViewpointStack.Top;

    if ProcessEvents and
       (Viewpoint <> nil) and
       ( (RenderingCamera.Target = rtScreen) or
         Viewpoint.FdcameraMatrixSendAlsoOnOffscreenRendering.Value ) then
    begin
      BeginChangesSchedule;
      try
        if Viewpoint.EventCameraMatrix.SendNeeded then
          Viewpoint.EventCameraMatrix.Send(RenderingCamera.Matrix, NextEventTime);

        if Viewpoint.EventCameraInverseMatrix.SendNeeded then
        begin
          RenderingCamera.InverseMatrixNeeded;
          Viewpoint.EventCameraInverseMatrix.Send(RenderingCamera.InverseMatrix, NextEventTime);
        end;

        if Viewpoint.EventCameraRotationMatrix.SendNeeded then
          Viewpoint.EventCameraRotationMatrix.Send(RenderingCamera.RotationMatrix3, NextEventTime);

        if Viewpoint.EventCameraRotationInverseMatrix.SendNeeded then
        begin
          RenderingCamera.RotationInverseMatrixNeeded;
          Viewpoint.EventCameraRotationInverseMatrix.Send(RenderingCamera.RotationInverseMatrix3, NextEventTime);
        end;
      finally EndChangesSchedule end;
    end;
  end;

begin
  inherited;
  RenderingCameraChanged(Params.RenderingCamera);
end;

end.
