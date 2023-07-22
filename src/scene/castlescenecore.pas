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

{ Loading and processing of scenes (TCastleSceneCore). }
unit CastleSceneCore;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses SysUtils, Classes, Generics.Collections, Contnrs, Kraft,
  CastleVectors, CastleBoxes, CastleTriangles, X3DFields, X3DNodes,
  CastleClassUtils, CastleUtils, CastleShapes, CastleInternalTriangleOctree,
  CastleInternalOctree, CastleInternalShapeOctree,
  CastleKeysMouse, X3DTime, CastleCameras, CastleInternalBaseTriangleOctree,
  CastleTimeUtils, CastleTransform, CastleInternalShadowMaps, CastleProjection,
  CastleComponentSerialize;

type
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
    FOnBoundChanged: TNotifyEvent;
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
    property OnBoundChanged: TNotifyEvent read FOnBoundChanged write FOnBoundChanged;
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
  public
    function Top: TAbstractViewpointNode;
    procedure PushIfEmpty(Node: TAbstractViewpointNode; SendEvents: boolean);
  end;

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
    see @link(TCastleSceneCore.PlayAnimation).

    Design note: This is a class, not e.g. an advanced record.
    This way is has always sensible defaults.
    You will usually create and quickly destroy it around
    the @link(TCastleSceneCore.PlayAnimation) call.
    Don't worry, time of creation/destruction of it really doesn't matter
    in practice, thanks to fast FPC memory allocator. }
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
      not at other times.

      Example usage:
      @includeCode(../../examples/short_api_samples/animation_stop_notification/animation_stop_notification.dpr) }
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

  { Possible options for @link(TCastleSceneCore.Load). }
  TSceneLoadOption = (
    slDisableResetTime
  );
  TSceneLoadOptions = set of TSceneLoadOption;

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
    or add children by @link(TAbstractGroupingNode.AddChildren RootNode.AddChildren).
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
    type
      TSceneValidity = (fvLocalBoundingBox,
        fvVerticesCount,
        fvTrianglesCount,
        fvMainLightForShadows,
        fvShapesActiveCount,
        fvShapesActiveVisibleCount);

      TSceneValidities = set of TSceneValidity;

      TGeneratedTexture = record
        { May be only TGeneratedCubeMapTextureNode or TRenderedTextureNode
          or TGeneratedShadowMapNode. }
        TextureNode: TAbstractTextureNode;
        Functionality: TGeneratedTextureFunctionality;
        Shape: TShape;
      end;
      PGeneratedTexture = ^TGeneratedTexture;

      { @exclude
        Internal for TCastleSceneCore: list of generated textures
        (GeneratedCubeMapTexture, RenderedTexture and similar nodes)
        along with their shape. }
      TGeneratedTextureList = class({$ifdef FPC}specialize{$endif} TStructList<TGeneratedTexture>)
      public
        function IndexOfTextureNode(TextureNode: TX3DNode): Integer;
        function FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
        function AddShapeTexture(Shape: TShape; Tex: TAbstractTextureNode): Pointer;
        procedure UpdateShadowMaps(LightNode: TAbstractLightNode);
      end;

      TProximitySensorInstanceList = {$ifdef FPC}specialize{$endif} TObjectList<TProximitySensorInstance>;
      TTimeDependentList = class({$ifdef FPC}specialize{$endif} TObjectList<TTimeDependentFunctionality>)
        procedure AddIfNotExists(const Item: TTimeDependentFunctionality);
      end;

      TCompiledScriptHandler = procedure (
        Value: TX3DField; const Time: TX3DTime) of object;

      TCompiledScriptHandlerInfo = record
        Handler: TCompiledScriptHandler;
        Name: string;
      end;
      PCompiledScriptHandlerInfo = ^TCompiledScriptHandlerInfo;
      TCompiledScriptHandlerInfoList = {$ifdef FPC}specialize{$endif} TStructList<TCompiledScriptHandlerInfo>;

      TExposedTransform = class
      strict private
        ChildObserver: TFreeNotificationObserver;
        WarningDone: Boolean;
        FNode: TTransformNode;
        FTransformFunctionality: TTransformFunctionality;
        FChild: TCastleTransform;
        FParentScene: TCastleSceneCore;
        procedure ChildFreeNotification(const Sender: TFreeNotificationObserver);
      public
        constructor Create(const AParentScene: TCastleSceneCore;
          const ANode: TTransformNode; const AChild: TCastleTransform);
        destructor Destroy; override;
        property Node: TTransformNode read FNode;
        property Child: TCastleTransform read FChild;
        property ParentScene: TCastleSceneCore read FParentScene;
        procedure Synchronize;
        class function X3dNameToPascal(const Prefix, S: String): String; static;
      end;
      TExposedTransformList = {$ifdef FPC}specialize{$endif} TObjectList<TExposedTransform>;

  protected
    type
      TVisibilitySensorInstanceList = {$ifdef FPC}specialize{$endif} TObjectList<TVisibilitySensorInstance>;
      TVisibilitySensors = class({$ifdef FPC}specialize{$endif} TDictionary<TVisibilitySensorNode, TVisibilitySensorInstanceList>)
      public
        destructor Destroy; override;
        { Remove everything are released owned stuff.
          We own TVisibilitySensorInstanceList instances on our Data list.
          We do not own TVisibilitySensorNode (our Keys list). }
        procedure Clear; reintroduce;
      end;

  private
    FShapes: TShapeTree;
    FRootNode, FRootNodeCacheOrigin: TX3DRootNode;
    FOwnsRootNode: Boolean;
    FPendingSetUrl: String;
    FOnPointingDeviceSensorsChange: TNotifyEvent;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FURL: string;
    FStatic: boolean;
    FShadowMaps: boolean;
    FShadowMapsDefaultSize: Cardinal;
    ScheduleHeadlightOnFromNavigationInfoInChangedAll: boolean;
    LastUpdateFrameId: TFrameId;
    LastCameraStateId: TFrameId;
    FDefaultAnimationTransition: Single;
    FCache: Boolean;

    { All InternalUpdateCamera calls will disable smooth (animated)
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
      They are set by PlayAnimation, and used by UpdateNewPlayingAnimation.
      Note that NewPlayingAnimationUse does *not* imply that
      NewPlayingAnimationNode is <> nil,
      it can be nil (if we want to change animation to nil). }
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

    { Some TimeSensor with DetectAffectedFields exists on TimeDependentList . }
    NeedsDetectAffectedFields: Boolean;
    AnimationAffectedFields: TX3DFieldList;

    { The transformation change happened,
      and should be processed (for the whole X3D graph inside RootNode).
      Used only when OptimizeExtensiveTransformations. }
    TransformationDirty: Boolean;

    { This always holds pointers to all TShapeTreeLOD instances in Shapes
      tree. }
    ShapeLODs: TObjectList;

    { Increased when something changed that could affect the results
      of Shapes tree traversal, i.e. different TShape instances returned
      by Shapes.Traverse.
      Never zero. }
    FShapesHash: TShapesHash;

    FExposeTransforms: TStrings;
    FExposedTransforms: TExposedTransformList;
    FExposeTransformsPrefix: String;

    { Perform animation fade-in and fade-out by initializing
      TTimeDependentFunctionality.PartialSend before
      TTimeDependentFunctionality.SetTime. }
    procedure PartialSendBegin(const TimeFunctionality: TTimeDependentFunctionality);

    { Finalize what PartialSendBegin started,
      after TTimeDependentFunctionality.SetTime was called. }
    procedure PartialSendEnd(const TimeFunctionality: TTimeDependentFunctionality);

    { Call this if during this frame, TTimeDependentFunctionality.PartialSend always remained @nil. }
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

    { Handle change of transformation of a node with TTransformFunctionality.
      TransformFunctionality.Parent must be associated only with TShapeTreeTransform,
      which must be true for all nodes implementing TTransformFunctionality.
      Changes must include chTransform, may also include other changes
      (this will be passed to shapes affected).

      Do not ever call this when OptimizeExtensiveTransformations.
      It would be buggy when both OptimizeExtensiveTransformations
      and InternalFastTransformUpdate are set, because in this case,
      TShapeTreeTransform.FastTransformUpdateCore leaves
      TShapeTreeTransform.Transform invalid,
      which will cause TransformationChanged result invalid.
    }
    procedure TransformationChanged(const TransformFunctionality: TTransformFunctionality);
    { Like TransformationChanged, but specialized for TransformNode = RootNode. }
    procedure RootTransformationChanged;

    function LocalBoundingVolumeMoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D): boolean;

    procedure SetRootNode(const Value: TX3DRootNode);

    { Always assigned to PlayingAnimationNode.EventIsActive. }
    procedure PlayingAnimationIsActive(
      const Event: TX3DEvent; const Value: TX3DField; const ATime: TX3DTime);

    { If we have NewPlayingAnimationUse, apply it
      (actually start playing it using X3D nodes, calling UpdateNewPlayingAnimation).
      This may be necessary to call in some circumstances.

      Returns false if we had NewPlayingAnimationUse, applied it,
      but we still have unapplied NewPlayingAnimationUse (which means that stopping
      previous animation started yet another animation). }
    function ApplyNewPlayingAnimation: Boolean;

    { Apply NewPlayingAnimation* stuff.

      Call outside of AnimateOnlyWhenVisible check, to do it even when object
      is not visible.  Otherwise stop/start time would be delayed too until
      it becomes visible, which is not perfect (although it would be Ok
      in practice too?) }
    procedure UpdateNewPlayingAnimation(out NeedsUpdateTimeDependent: Boolean);

    { Call SetTime on all things in TimeDependentList. }
    procedure UpdateTimeDependentList(const TimeIncrease: TFloatTime; const ResetTime: boolean);

    function SensibleCameraRadius(out RadiusAutoCalculated: Boolean): Single;

    { Apply TransformationDirty effect
      (necessary to finalize OptimizeExtensiveTransformations,
      after TTransformNode values changed).
      Always call this after doing something that could change
      TTransformNode values. }
    procedure FinishTransformationChanges;

    procedure SetExposeTransforms(const Value: TStrings);
    procedure ExposeTransformsChange(Sender: TObject);
    procedure SetExposeTransformsPrefix(const Value: String);
  private
    FGlobalLights: TLightInstancesList;

    FLocalBoundingBox: TBox3D;
    FVerticesCount, FTrianglesCount: Cardinal;
    Validities: TSceneValidities;
    function CalculateLocalBoundingBox: TBox3D;
    function CalculateVerticesCount: Cardinal;
    function CalculateTrianglesCount: Cardinal;
  private
  type
    TAbstractViewpointNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TAbstractViewpointNode>;
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
    FOnViewpointsChanged: TNotifyEvent;
    FOnBoundViewpointVectorsChanged: TNotifyEvent;
    FOnBoundNavigationInfoFieldsChanged: TNotifyEvent;

    FProcessEvents: boolean;
    procedure SetProcessEvents(const Value: boolean);
  private
    KeyDeviceSensorNodes: TX3DNodeList;
    TimeDependentList: TTimeDependentList;
    ProximitySensors: TProximitySensorInstanceList;
    FVisibilitySensors: TVisibilitySensors;
    BillboardNodes: TX3DNodeList;

    procedure ChangedAllEnumerateCallback(Node: TX3DNode);
    procedure ScriptsInitializeCallback(Node: TX3DNode);
    procedure ScriptsFinalizeCallback(Node: TX3DNode);

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
    ChangedAllScheduled: Boolean;
    ChangedAllScheduledOnlyAdditions: Boolean;

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
      const CameraVectors: TViewVectors);
  private
    FCompiledScriptHandlers: TCompiledScriptHandlerInfoList;

    { Create octree containing all triangles or shapes from our scene.
      Create octree, inits it with our LocalBoundingBox
      and adds shapes (or all triangles from our Shapes).

      Triangles are generated using calls like
      @code(Shape.Triangulate(...)).

      If Collidable, then only the collidable, or at least "pickable",
      triangles are generated. Which means that children of
      Collision nodes with collide = FALSE (or proxy <> nil) are not placed here.
      Otherwise, only the visible (not necessarily collidable)
      items are placed in the octree.

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
      const Collidable: boolean): TTriangleOctree;
    function CreateShapeOctree(const Limits: TOctreeLimits;
      const Collidable: boolean): TShapeOctree;
    { @groupEnd }
  private
    FTriangleOctreeLimits: TOctreeLimits;

    FShapeOctreeLimits: TOctreeLimits;

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

      They are used only when the octree is created, so usually you
      want to set them right before changing @link(Spatial) from []
      to something else.

      Note that particular models may override this by
      [https://castle-engine.io/x3d_extensions.php#section_ext_octree_properties]. }
    function ShapeOctreeLimits: POctreeLimits;

    procedure SetSpatial(const Value: TSceneSpatialStructures);
    function GetPreciseCollisions: Boolean;
    procedure SetPreciseCollisions(const Value: Boolean);
  private
    FMainLightForShadowsExists: boolean;
    FMainLightForShadows: TVector4;
    FMainLightForShadowsNode: TAbstractPunctualLightNode;
    FMainLightForShadowsTransform: TMatrix4;
    function SearchMainLightForShadows(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
    { Based on FMainLightForShadowsNode and FMainLightForShadowsTransform,
      calculate FMainLightForShadows (position). }
    procedure CalculateMainLightForShadowsPosition;
    procedure ValidateMainLightForShadows;
  private
    FAutoAnimation: String;
    FAutoAnimationLoop: Boolean;
    FHeadlightOn: boolean;
    FOnHeadlightOnChanged: TNotifyEvent;
    FAnimateOnlyWhenVisible: boolean;
    FAnimateGatheredTime: TFloatTime;
    FAnimateSkipTicks: Cardinal;
    AnimateSkipNextTicks: Cardinal;

    procedure SetAnimateSkipTicks(const Value: Cardinal);
    procedure SetHeadlightOn(const Value: boolean);
    procedure SetAutoAnimation(const Value: String);
    procedure SetAutoAnimationLoop(const Value: Boolean);
    procedure UpdateAutoAnimation(const StopIfPlaying: Boolean);

    { Get camera position in current scene local coordinates.
      This is calculated every time now (in the future it may be optimized
      to recalculate only when WorldTransform changed, e.g. using
      FWorldTransformAndInverseId). }
    function GetCameraLocal(out CameraVectors: TViewVectors): boolean; overload;
    function GetCameraLocal(out CameraLocalPosition: TVector3): boolean; overload;

    function PointingDevicePressRelease(const DoPress: boolean;
      const Distance: Single; const CancelAction: boolean): boolean;

    { Like Load but with additional ARootNodeCacheOrigin parameter. }
    procedure LoadCore(const ARootNode: TX3DRootNode;
      const ARootNodeCacheOrigin: TX3DRootNode;
      const AOwnsRootNode: boolean;
      const AOptions: TSceneLoadOptions);
    { Free FRootNode and FRootNodeCacheOrigin and set it to @nil.
      Always use this to free FRootNode. }
    procedure FreeRootNode;
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
    function CreateShape(const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState;
      const ParentInfo: PTraversingInfo): TShape; virtual;

    procedure UpdateHeadlightOnFromNavigationInfo;

    { Camera changed.
      In this class this updates various X3D nodes using the camera position/direction/up,
      like LOD, Billboard, ProximitySensor.
      @exclude }
    procedure InternalCameraChanged; virtual;

    { Background node changed. @exclude }
    procedure InternalInvalidateBackgroundRenderer; virtual;

    property VisibilitySensors: TVisibilitySensors read FVisibilitySensors;

    procedure ChangedTransform; override;
    procedure ChangeWorld(const Value: TCastleAbstractRootTransform); override;

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
    procedure Loaded; override;

    { Called before changing one node into another,
      when old node may have beeen associated with a shape using TShapeTree.AssociateNode.

      Both OldNode and NewNode may be @nil, this method can handle it.

      It is allowed to call this even when OldNode = NewNode, which means that nothing
      really changes. }
    procedure InternalMoveShapeAssociations(
      const OldNode, NewNode: TX3DNode; const ContainingShapes: TObject); override;

    { Local (not affected by our @link(Translation), @link(Rotation), @link(Scale)) bounding box.
      Takes into account loaded scene (in @link(URL))
      but not children TCastleTransform bounding volumes. }
    function LocalBoundingBoxNoChildren: TBox3D;
  public
    var
      { Nonzero value prevents rendering of this scene,
        and generally means that our state isn't complete.
        This is useful if we're in the middle of some operation
        (like ChangedAll call or octree creation).

        Some callbacks *may* be called during such time.
        (No good example now; old example was: callbacks caused by progress steps
        done during constructing octrees).
        As these callbacks may try to e.g. render our scene (which should
        not be done on the dirty state), we have to protect ourselves
        using this variable (e.g. Render routines will exit immediately
        when InternalDirty <> 0).

        @exclude }
      InternalDirty: Cardinal;

      { @exclude }
      InternalNodeSharing: Boolean;

    const
      DefaultShadowMapsDefaultSize = 256;

  public // repeat "public" is necessary for FPC to parse Node<NodeType> method declaration later with FPC 3.2.2

    { }
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Load the model given as a X3D nodes graph.
      This replaces RootNode with new value.

      @param(ARootNode The model to load.
        This will become a new value of our @link(RootNode) property.)

      @param(AOwnsRootNode Should the scene take care of freeing
        this root node when it is no longer used. If @false,
        you are expected to free it yourself, but only after the scene
        using it was freed.)

      @param(AResetTime If @true then we will reset time at loading
        (using @link(ResetTimeAtLoad)),
        changing the @link(Time). This is usually what you want when you
        load a new world.)

      Note that you should never load the same @link(TX3DRootNode) instance
      into multiple @link(TCastleScene) instances.

      @longCode(#
        // DON'T DO THIS!
        Node := LoadNode(URL);
        Scene1 := TCastleScene.Create(Application);
        Scene1.Load(Node, false);
        Scene2 := TCastleScene.Create(Application);
        Scene2.Load(Node, false);
      #)

      If you need to load the same model into multiple scenes,
      it is best to use the @link(Clone) method:

      @longCode(#
        SceneTemplate := TCastleScene.Create(Application);
        SceneTemplate.Load(URL);
        Scene1 := SceneTemplate.Clone(Application);
        Scene2 := SceneTemplate.Clone(Application);
      #)

      Using the @link(Clone) makes a copy of the underlying X3D graph,
      so it is roughly like doing:

      @longCode(#
        Node := LoadNode(URL);
        Scene1 := TCastleScene.Create(Application);
        Scene1.Load(Node.DeepCopy as TX3DRootNode, false);
        Scene2 := TCastleScene.Create(Application);
        Scene2.Load(Node.DeepCopy as TX3DRootNode, false);
      #)

      Note that sometimes you don't need to create multiple scenes
      to show the same model many times.
      You can simply insert the same TCastleScene instance multiple
      times to TCastleViewport.Items (TCastleRootTransform).
      See the manual:
      https://castle-engine.io/manual_scene.php#section_many_instances
    }
    procedure Load(const ARootNode: TX3DRootNode; const AOwnsRootNode: boolean;
      const AOptions: TSceneLoadOptions = []); overload;

    { Load the 3D model from given URL.

      We load a number of 3D model formats (X3D, VRML, Collada, Wavefront OBJ...)
      and some 2D model formats (Spine JSON).
      See https://castle-engine.io/creating_data_model_formats.php
      for the complete list.

      URL is downloaded using the CastleDownload unit,
      so it supports files, http resources and more.
      See https://castle-engine.io/manual_network.php
      about supported URL schemes.
      If you all you care about is loading normal files, then just pass
      a normal filename (absolute or relative to the current directory)
      as the URL parameter. }
    procedure Load(const AURL: string; const AOptions: TSceneLoadOptions = []); overload;
    procedure Load(const AURL: string; const AllowStdIn: boolean;
      const AResetTime: boolean = true); overload; deprecated 'use Load with (AURL: string, AOptions: TSceneLoadOptions) parameters. AllowStdIn is not implemented anymore.';

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

    procedure BeforeDestruction; override;
    destructor Destroy; override;

    { Tree of shapes in the scene, acting as a simplfied mirror
      of the X3D node graph.
      Contents of this tree are read-only from outside. }
    property Shapes: TShapeTree read FShapes;

    // { Bounding box of all occurrences of the given X3D Shape node. }
    // function ShapeBoundingBox(const Node: TShapeNode): TBox3D;

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
      do see appropriate TAbstractGeometryNode methods.
      Here, we just sum their results for all shapes.
      @groupBegin }
    function VerticesCount: Cardinal; overload;
    function TrianglesCount: Cardinal; overload;
    { @groupEnd }

    function VerticesCount(const Ignored: Boolean): Cardinal; overload; deprecated 'use VerticesCount without Boolean argument, it is ignored now';
    function TrianglesCount(const Ignored: Boolean): Cardinal; overload; deprecated 'use TrianglesCount without Boolean argument, it is ignored now';

    { Helper functions for accessing viewpoints defined in the scene.
      @groupBegin }
    function ViewpointsCount: Cardinal;
    function GetViewpointName(Idx: integer): string;
    procedure MoveToViewpoint(Idx: integer; Animated: boolean = true);
    procedure AddViewpointFromNavigation(const Navigation: TCastleNavigation;
      const AName: string);
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
    procedure ChangedAll(const OnlyAdditions: Boolean = false); override;

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
    procedure InternalChangedField(const Field: TX3DField; const Change: TX3DChange); override;

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
    property OnViewpointsChanged: TNotifyEvent
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
    property OnBoundViewpointVectorsChanged: TNotifyEvent
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

    property OnBoundNavigationInfoFieldsChanged: TNotifyEvent
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
    procedure ScheduleChangedAll(const OnlyAdditions: Boolean = false);
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
      {$ifdef FPC} deprecated 'set OwnsRootNode only at loading, do not depend on this property'; {$endif}

    { A spatial structure containing all visible shapes.
      Add ssRendering to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not use this directly.
      Instead use TCastleViewport
      and then use @code(Viewport.Items.WorldXxxCollision) methods like
      @link(TCastleAbstractRootTransform.WorldRay Viewport.Items.WorldRay) or
      @link(TCastleAbstractRootTransform.WorldSphereCollision Viewport.Items.WorldSphereCollision).)

      Note that when VRML/X3D scene contains Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects. }
    function InternalOctreeRendering: TShapeOctree;

    { A spatial structure containing all collidable shapes.
      Add ssDynamicCollisions to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use TCastleViewport
      and then use @code(Viewport.Items.WorldXxxCollision) methods like
      @link(TCastleAbstractRootTransform.WorldRay Viewport.Items.WorldRay) or
      @link(TCastleAbstractRootTransform.WorldSphereCollision Viewport.Items.WorldSphereCollision).)

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
      Instead use TCastleViewport
      and then use @code(Viewport.Items.WorldXxxCollision) methods like
      @link(TCastleAbstractRootTransform.WorldRay Viewport.Items.WorldRay) or
      @link(TCastleAbstractRootTransform.WorldSphereCollision Viewport.Items.WorldSphereCollision).)

      Note that when VRML/X3D scene contains X3D Collision nodes, this octree
      contains the @italic(visible (not necessarily collidable)) objects. }
    function InternalOctreeVisibleTriangles: TTriangleOctree;

    { A spatial structure containing all collidable triangles.
      Add ssStaticCollisions to @link(Spatial) property, otherwise it's @nil.

      @bold(You should not usually use this directly.
      Instead use TCastleViewport
      and then use @code(Viewport.Items.WorldXxxCollision) methods like
      @link(TCastleAbstractRootTransform.WorldRay Viewport.Items.WorldRay) or
      @link(TCastleAbstractRootTransform.WorldSphereCollision Viewport.Items.WorldSphereCollision).)

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
      Instead use TCastleViewport
      and then use @code(Viewport.Items.WorldXxxCollision) methods like
      @link(TCastleAbstractRootTransform.WorldRay Viewport.Items.WorldRay) or
      @link(TCastleAbstractRootTransform.WorldSphereCollision Viewport.Items.WorldSphereCollision).) }
    function InternalOctreeCollisions: TBaseTrianglesOctree;

    function UseInternalOctreeCollisions: boolean;

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

    { Recursively unset node's TX3DNode.Scene. }
    procedure UnregisterScene(Node: TX3DNode);
      deprecated 'use Node.UnregisterScene';

    { Scene processes X3D key sensor nodes here, https://castle-engine.io/x3d_implementation_keydevicesensor.php .
      Remember to set ListenPressRelease to process the key sensor nodes. }
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
    function PointingDeviceRelease(const Pick: TRayCollisionNode;
      const Distance: Single; const CancelAction: Boolean): Boolean; override;

    { Called when pointing device moves.
      This may generate the continuously-generated events like
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

    { Clear any references to PTriangle passed previously in TRayCollisionNode argument to
      PointingDevicePress, PointingDeviceRelease, PointingDeviceMove.
      This is internally used in case some of past seen PTriangle may become invalid,
      so any memory about them should be cleared.

      This doesn't cause any X3D events
      (contrary to e.g. calling PointingDeviceMove with Pick.Triangle = @nil).

      This method clears any references to saved PTriangle and
      PointingDeviceActiveSensors.
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
      It is crucial that you call this continuously to have some VRML/X3D
      time-dependent features working, like TimeSensor and MovieTexture.
      See @link(Time) for details what is affected by this.

      This is automatically taken care of if you added this scene
      to TCastleWindow.Controls or TCastleControl.Controls.
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
      you only need to place the scene inside @link(TCastleViewport.Items).

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

    { List of handlers for VRML/X3D Script node with "compiled:" protocol.
      This is read-only, change this only by RegisterCompiledScript. }
    property CompiledScriptHandlers: TCompiledScriptHandlerInfoList
      read FCompiledScriptHandlers;

    { Register compiled script handler, for VRML/X3D Script node with
      "compiled:" protocol.
      See [https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled]. }
    procedure RegisterCompiledScript(const HandlerName: string;
      Handler: TCompiledScriptHandler);

    { Update TCastleNavigation properties based on currently bound TNavigationInfoNode.

      Bound TNavigationInfoNode is taken from
      NavigationInfoStack.Top. If no such node is bound (NavigationInfoStack.Top is @nil)
      then we behave as if TNavigationInfoNode with all fields at default was active.

      Note that, since some fields are only in some descendants (e.g. MoveSpeed
      is only at TCastleWalkNavigation) then some TNavigationInfoNode settings
      are just not transferred to all Navigation instances like
      TCastleExamineNavigation }
    procedure InternalUpdateNavigation(
      const Navigation: TCastleNavigation);

    { Update TCastleCamera properties based on the current X3D nodes
      (currently bound X3D Viewpoint and NavigationInfo nodes).
      When no viewpoint is currently bound, we will go to a suitable
      viewpoint to see the whole world (based on the WorldBox).

      The initial camera vectors position, direction, up, gravity up
      are set to the current viewpoint. This is done
      regardless of the RelativeCameraTransform value.

      TODO: below is no longer valid, RelativeCameraTransform has no meaning now.

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
          or orientation of the X3D Viewpoint node: conceptually,
          there exists a "user" camera transformation that is the child
          of the viewpoint. When viewpoint is moved, then the current
          camera moves with it.)
      )

      @exclude }
    procedure InternalUpdateCamera(const ACamera: TCastleCamera;
      const WorldBox: TBox3D;
      const RelativeCameraTransform: boolean;
      const AllowTransitionAnimate: boolean);

    { Make Camera go to the view given by (world coordinates) APosition, ADirection, AUp.

      Honours current NavigationInfo.transitionType and transitionTime.
      If transitionType indicates instanteneous transition, then jumps
      by simple @code(Camera.SetWorldView(APosition, ADirection, AUp)).
      Otherwise makes a smooth animation into new values by
      @code(Camera.AnimateTo(APosition, ADirection, AUp, TransitionTime)).

      Will generate NavigationInfo.transitionComplete when transition ends.

      @groupBegin }
    procedure CameraTransition(const Camera: TCastleCamera; const APosition, ADirection, AUp: TVector3); overload;
      deprecated 'use Camera.AnimateTo(APosition, ADirection, AUp)';
    procedure CameraTransition(const Camera: TCastleCamera; const APosition, ADirection, AUp, GravityUp: TVector3); overload;
      deprecated 'use Camera.AnimateTo(APosition, ADirection, AUp)';
    procedure CameraTransition(const Navigation: TCastleNavigation; const APosition, ADirection, AUp: TVector3); overload;
      deprecated 'use Camera.AnimateTo(APosition, ADirection, AUp)';
    procedure CameraTransition(const Navigation: TCastleNavigation; const APosition, ADirection, AUp, GravityUp: TVector3); overload;
      deprecated 'use Camera.AnimateTo(APosition, ADirection, AUp)';
    { @groupEnd }

    { Detect position/direction of the main light that produces shadow volumes.
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
      Returned value is local to this transformation (i.e. it is in this scene's space,
      not world space).

      @exclude
      Should only be used internally by TCastleViewport. }
    function InternalMainLightForShadowVolumes(out AMainLightPosition: TVector4): boolean;

    { Light node that should be used for headlight, or @nil if default
      directional headlight is suitable.

      This never returns @nil. It's not concerned whether the headlight
      should actually be used --- for this, see HeadlightOn. }
    function CustomHeadlight: TAbstractLightNode;

    { Should we use headlight for this scene. Controls if containing TCastleViewport
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

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const Params: TPrepareParams); override;

    {$ifdef FPC}
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
    {$endif}

    { Nice scene caption. Uses the "title" of WorldInfo
      node inside the VRML/X3D scene. If there is no WorldInfo node
      (or it has empty title) then result is based on loaded URL. }
    function Caption: string;

    { Global lights of this scene. Read-only.
      Useful to shine these lights on other scenes, if TCastleScene.CastGlobalLights. }
    property InternalGlobalLights: TLightInstancesList read FGlobalLights;

    { Find a named X3D node in the current node graph.

      By default it searches both active and inactive graph parts.
      Add fnOnlyActive to search only in active parts.

      It searches only within nodes of given type (NodeClass).
      Specifying NodeClass helps to make search unambiguous
      (as name clashes are possible, some authoring tools may write models with duplicate
      names for materials, meshes etc. and both glTF and X3D allow it).
      This allows to safely write code like:

      @longCode(#
        MyMaterial := Scene.Node(TNodePhysicalMaterial, 'MyMaterial') as TNodePhysicalMaterial;
      #)

      TODO: An even better version, "MyMaterial := Scene.Node<TNodePhysicalMaterial>('MyMaterial')",
      may be available in the future, if FPC support for generic methods will improve.
      Define GENERIC_METHODS to try it out now, but heed the warnings in castleconf.inc .

      @raises(EX3DNotFound When node is not found.
        Unless fnNilOnMissing in Options, then it returns @nil on missing node,
        and EX3DNotFound is never raised.)
    }
    function Node(const NodeClass: TX3DNodeClass; const NodeName: string;
      const Options: TFindNodeOptions = []): TX3DNode; overload;
    function Node(const NodeName: string): TX3DNode; overload;
      { deprecated 'use Node(NodeClass, NodeName)';

        Do not deprecate. It doesn't offer that big benefit over using version
        without NodeClass, when you know that node name is unique.
        The "as" with perform check in case types differ. }

    { Find a named field within an X3D node in the current node graph.

      Like @link(Node), this searches all nodes (in active or not) graph parts.

      @raises(EX3DNotFound If given node or field could not be found.) }
    function Field(const NodeName, FieldName: string): TX3DField; overload;
      {$ifdef GENERIC_METHODS} deprecated 'use Field<NodeType>(NodeName, FieldName)'; {$endif}

    { Find a named event within an X3D node in the current node graph.

      Like @link(Node), this searches all nodes (in active or not) graph parts.

      @raises(EX3DNotFound If given node or event could not be found.) }
    function Event(const NodeName, EventName: string): TX3DEvent; overload;
      {$ifdef GENERIC_METHODS} deprecated 'use Event<NodeType>(NodeName, FieldName)'; {$endif}

    {$ifdef GENERIC_METHODS}
    (*Find a named X3D node in the current node graph.
      See non-generic Node for description of Options.

      It searches only within nodes of given type (T) and also
      returns the appropriate type. Use it like this:

      @longCode(#
      var
        Material: TPhysicalMaterialNode;
      begin
        Material := MyScene.{$ifdef FPC}specialize{$endif} Node<TPhysicalMaterialNode>('MyMaterialName');
      end;
      #)

      You can simplify the code if you only care about FPC or Delphi compiler.

      TODO: The extra "specialize" looks ugly, it is excessive. It is even uglier when compounded
      if "ifdef FPC", if you need to support both FPC and Delphi.

      TODO: FPC 3.2.2. unfortunately makes internal error if you forget
      "specialize", not a proper error message.
      And sometimes it compiles something weird, that always crashes. *)
    {$ifdef FPC}generic{$endif} function Node<T: TX3DNode>(const NodeName: string;
      const Options: TFindNodeOptions = []): T; overload;
    {$ifdef FPC}generic{$endif} function Field<T: TX3DNode>(const NodeName, FieldName: string): TX3DField; overload;
    {$ifdef FPC}generic{$endif} function Event<T: TX3DNode>(const NodeName, EventName: string): TX3DEvent; overload;
    {$endif}

    { List the names of available animations in current scene.
      Animations are detected looking for TimeSensor nodes.
      See https://castle-engine.io/x3d_implementation_interpolation.php
      for an overview how nodes are used to create animations.

      Note that if you set @link(AnimationPrefix) property, we additionally
      filter TimeSensor nodes to show only the names starting with given prefix.
      In this case, not all TTimeSensorNode are reflected in this list.

      You can get the corresponding TTimeSensorNode by AnimationTimeSensor.
      Note that the same TTimeSensorNode may occur multiple times on this list,
      in case X3D IMPORT mechanism was used to rename the imported animation.

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

    { TimeSensor of this animation. @nil if this name not found. }
    function AnimationTimeSensor(const AnimationName: string): TTimeSensorNode; overload;

    { TimeSensor of this animation, by animation index (index
      on AnimationsList). @nil if this index not found. }
    function AnimationTimeSensor(const Index: Integer): TTimeSensorNode; overload;

    function Animations: TStringList; deprecated 'use AnimationsList (and do not free it''s result)';

    { Forcefully, immediately, set pose from given animation,
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

    { Play an animation specified by name.

      You can specify the animation name, whether it should loop,
      and whether to play it forward or backward using parameters to this method.

      Or you can specify even more parameters using TPlayAnimationParameters instance.
      TPlayAnimationParameters can additionally specify a stop notification,
      initial time and more.
      It is OK to create a short-lived TPlayAnimationParameters instance and destroy
      it right after calling this method.
      This method doesn't store the TPlayAnimationParameters instance reference.

      To get the list of available animations, see @link(AnimationsList).

      This is one of the simplest way to play animations using Castle Game Engine.
      Alternative (that calls PlayAnimation under the hood) is to set AutoAnimation
      and AutoAnimationLoop.
      See https://castle-engine.io/viewport_3d#_play_animation .

      Playing an already-playing animation is guaranteed to restart it from
      the beginning (more precisely: from TPlayAnimationParameters.InitialTime,
      if you pass TPlayAnimationParameters).

      If the given animation name exists,
      then we stop previously playing animation (if any),
      calling it's stop notification (see TPlayAnimationParameters.StopNotification),
      start playing the new animation, and return @true.
      If the animation name does not exist then we return @false,
      make a warning and the current animation is unchanged.

      The change from previous to new animation can be smooth (using animation
      cross-fading) if you use TPlayAnimationParameters with @link(TPlayAnimationParameters.TransitionDuration)
      or if you set DefaultAnimationTransition to something non-zero.

      This automatically turns on @link(ProcessEvents),
      if it wasn't turned on already.
      Processing events is necessary for playing animations.

      More details about how this works:

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

          If you really need to change the scene immediately (for example,
          because you don't want to show user the initial scene state),
          simply call @link(ForceInitialAnimationPose) right after @link(PlayAnimation).
        )

        @item(@italic(The animation is performed using X3D TTimeSensorNode).

          For example, TTimeSensorNode
          may instruct a TCoordinateInterpolatorNode to update the TIndexedFaceSetNode coordinates,
          and in effect the animation can deform a mesh.
          Or TTimeSensorNode may instruct a TPositionInterpolatorNode to update
          @link(TTransformNode.Translation),
          and in effect the animation can move something.
          See https://castle-engine.io/x3d_implementation_interpolation.php
          for a thorough description how the animation works in X3D.

          This means that during the animation,
          the X3D nodes graph (within @link(RootNode)) is being changed.

          If you load a model from a castle-anim-frames or MD3 format, note that it
          is animated using a special "node interpolator".
          This also works using a special (internal) X3D node that
          switches which node is visible.)

        @item(Starting an animation effectively cancels any effect from
          any previous animation. That is, even if the new animation doesn't
          modify some transformations, these transformations are @italic(not)
          left "as is", instead they are reset to the default model state using
          @link(ResetAnimationState). This is usually the desired effect, corresponding
          to how the animations should intuitively behave, e.g. if you have "walk"
          and "idle" animations.

          If you want to simultaneously play multiple animations
          or if you want to simultaneously modify scene by code (while playing some animation)
          then you can control each time sensor using @link(TTimeSensorNode.Start)
          and @link(TTimeSensorNode.Stop). See also the example
          in examples/animations/simultaneous_animations_one_scene .
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

    { Force the model to look like the initial animation frame @italic(now).

      Use this after calling @link(PlayAnimation).
      Calling this is simply ignored if no @link(PlayAnimation) was called
      earlier, of if the model already looks following the
      animation requested by @link(PlayAnimation).

      If you don't use this method, there may be a 1-frame delay
      between calling @link(PlayAnimation) and actually updating the rendered
      scene look. If during that time a rendering will happen,
      the user will see a scene in previous pose (not in the first
      pose of animation you requested by @link(PlayAnimation)).
      To avoid this, simply call this method right after @link(PlayAnimation).

      This sets first animation frame,
      unless you used TPlayAnimationParameters.InitialTime <> 0. }
    procedure ForceInitialAnimationPose;

    { Duration, in seconds, of the named animation
      (named animations are detected by @link(AnimationsList) method).
      For a looping animation, this is the duration of a single cycle.
      0 if not found. }
    function AnimationDuration(const AnimationName: string): TFloatTime;

    {$ifdef FPC}
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
      deprecated 'this property did not prove to be of much use; report if you need it, otherwise it may be removed one day';
    {$endif}

    { Currently played animation by @link(PlayAnimation), or @nil.

      Note that, in a general case, you can have multiple active animations
      (multiple TimeSensor X3D nodes may be active) at the same time.
      Calling @link(TTimeSensorNode.Start) directly on multiple nodes is
      one way to make it happen.
      This property simply describes
      the animation controlled by the last @link(PlayAnimation) method.

      Note that the animation may be started by PlayAnimation with a 1-frame
      delay, but this property hides it from you, it is changed
      immediately by the PlayAnimation call. }
    property CurrentAnimation: TTimeSensorNode read FCurrentAnimation;

    { Stop the @link(CurrentAnimation), started by last @link(PlayAnimation) call.
      Note that this leaves the model in a state in the middle of the last animation.

      You can use @link(ResetAnimationState) to reset the state afterwards.
      Calling @link(ForceInitialAnimationPose) also will do it.

      Note that it is not necessary to stop the previous animation
      before starting a new one (by @link(PlayAnimation)).
      @link(PlayAnimation) will automatically stop the previous animation.
      Moreover, @link(PlayAnimation) may do animation blending (cross-fade)
      between old and new animation (if you use @link(TPlayAnimationParameters.TransitionDuration)),
      which only works if you @italic(did not) call StopAnimation.

      When animation is stopped (by this or any other means),
      by default it's @link(TPlayAnimationParameters.StopNotification) is called.
      You can use DisableStopNotification to avoid it,
      which should be used only in exceptional situations. }
    procedure StopAnimation(const DisableStopNotification: Boolean = false);

    { Reset all the fields affected by animations.
      See TimeSensor.detectAffectedFields documentation
      (https://castle-engine.io/x3d_implementation_time_extensions.php#section_detect_affected_fields)
      for details how these fields are detected, and when this is useful.
      By default this is enabled, for all TimeSensor nodes.

      If IgnoreAffectedBy <> nil, the fields affected by the given TimeSensor
      may not be reset. This is an optimization useful in case this TimeSensor
      will modify the scene very soon, so resetting its affected fields
      is a waste of time.
      Do not depend on 100% that the fields affected by given TimeSensor
      are left untouched (in the current implementation, this field is ignored). }
    procedure ResetAnimationState(const IgnoreAffectedBy: TTimeSensorNode = nil);

    { Force recalculating the text shapes when font changed.
      For now, we don't detect font changes (e.g. when TFontStyleNode.CustomFont changed)
      automatically.
      This calls @link(TTextNode.FontChanged) and @link(TAsciiTextNode_1.FontChanged)
      on all appropriate nodes. }
    procedure FontChanged;

    { Create a scene with the same contents (X3D scene graph) as this one.
      The created scene has exactly the same class as this one
      (we use ClassType.Create to call a virtual constructor).

      Note that this @bold(does not copy other scene attributes),
      like @link(ProcessEvents) or @link(Spatial) or rendering attributes
      in @link(TCastleScene.RenderOptions).
      It only copies the scene graph (RootNode) and also sets
      target URL based on source URL (for logging purposes, e.g.
      TCastleProfilerTime use this URL to report loading and preparation times). }
    function Clone(const AOwner: TComponent): TCastleSceneCore;

    {$ifdef FPC}
    { @deprecated Deprecated name for @link(URL). }
    property FileName: string read FURL write SetURL; deprecated;
    {$endif}

    procedure InternalIncShapesHash;
    property InternalShapesHash: TShapesHash read FShapesHash;

    { Load again the model from current URL.
      This makes sense to be used when underlying file on disk
      changed, and you want to reload it.

      TODO: If the file is cached using @link(Cache), then it will not reload
      the version in cache, so effectively it will not load new version from
      disk. This will be fixed at some point. }
    procedure ReloadUrl;
  published
    { When using @link(PlayAnimation) without TPlayAnimationParameters,
      this value is used as the duration (in seconds) of animation cross-fade
      (blending of animations).
      Zero (default value) disables the smooth transition, animations
      will change without any transition by default.

      This default transition is only used if some previous animation
      was playing. Otherwise, it would be applied even when starting
      the initial animation. }
    property DefaultAnimationTransition: Single
      read FDefaultAnimationTransition write FDefaultAnimationTransition {$ifdef FPC}default 0.0{$endif};

    { When TimePlaying is @true, the time of our 3D world will keep playing.
      More precisely, our @link(Update) will take care of increasing @link(Time).
      Our @link(Update) is usually automatically called (if you added this
      scene to TCastleWindow.Controls or TCastleControl.Controls)
      so you don't have to do anything to make this work. }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;

    { Controls the time speed (if TimePlaying is @true):
      1.0 means that 1 second  of real time equals to 1 unit of world time. }
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed {$ifdef FPC}default 1.0{$endif};

    { In most cases you should get / set simpler @link(PreciseCollisions) property, not this.
      Which spatial structures (octrees) should be created and used.

      Using "spatial structures" allows to achieve various things:

      @unorderedList(
        @item(@bold(ssDynamicCollisions) or @bold(ssStaticCollisions):

          Using one of these two flags allows to resolve collisions with
          the (collidable) triangles of the model.
          By default, every X3D Shape is collidable using it's exact mesh.
          You can use the X3D @link(TCollisionNode) to turn collisions
          off for some shapes, or replace some shapes with simpler objects
          for the collision-detection purposes.

          If you use neither @bold(ssDynamicCollisions) nor @bold(ssStaticCollisions),
          then the collisions are resolved using the whole scene bounding
          box. That is, treating the whole scene as a giant cube.

          You can always toggle @link(Collides) to quickly make
          the scene not collidable. In summary:

          @unorderedList(
            @item(@link(Collides) = @false: the scene does not collide.)

            @item(@link(Collides) = @true and Spatial is empty:
              the scene collides as it's bounding box (LocalBoundingBoxNoChildren to be precise,
              so the box of @link(URL) model is taken into account,
              but not children).
              This is the default situation after constructing TCastleScene.)

            @item(@link(Collides) = @true and
              Spatial contains @bold(ssDynamicCollisions) or @bold(ssStaticCollisions):
              the scene collides as a set of triangles.
              The triangles are derived from information in X3D Shape and Collision
              nodes.)
          )

          The ssStaticCollisions can be used instead of ssDynamicCollisions
          when the scene is guaranteed to @italic(absolutely never) change,
          and @italic(only when the speed is absolutely crucial).
          The collision structure created by ssStaticCollisions
          is a bit faster (although may use significantly more memory).
          The @italic(practical advice is to almost always use ssDynamicCollisions
          instead of ssStaticCollisions): the speed gains
          from ssStaticCollisions are usually impossible to measure,
          and ssStaticCollisions sometimes uses significantly more memory,
          and if you by accident modify the model (animate it etc.)
          with ssStaticCollisions -> then results are undefined,
          even crashes are possible.
        )

        @item(@bold(ssRendering):

          Using this flag adds an additional optimization during rendering.
          It allows to use frustum culling with an octree.
          Whether the frustum culling is actually used depends
          on @link(TCastleScene.ShapeFrustumCulling) value (by default: yes).

          Without this flag, we can still use frustum culling,
          but it's less effective as it considers each shape separately.
          Whether the frustum culling is actually used also depends
          on @link(TCastleScene.ShapeFrustumCulling) value.

          Using frustum culling (preferably with ssRendering flag)
          is highly adviced if your camera usually only sees
          only a part of the scene. For example, it a noticeable optimization
          if you have a camera walking/flying inside a typical game
          level/location.)

        @item(@bold(ssVisibleTriangles):

          Using this flag allows to resolve collisions with visible
          (not only collidable) triangles quickly.

          This is in practice useful only for ray-tracers,
          normal applications should not use this.
          Normal applications should avoid collision detection with the
          @italic(visible) version of the model.
          Normal applications should instead
          perform collision detection with the @italic(collidable)
          version of the model, since this is much better optimized
          (both by the engine code, and by an artist creating the model,
          using X3D Collision nodes etc.)
        )
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
    property Spatial: TSceneSpatialStructures read FSpatial write SetSpatial
      stored false default [];
      {$ifdef FPC}deprecated 'use PreciseCollisions';{$endif}

    { Resolve collisions precisely with the scene triangles.

      When this is @false we will only consider the bounding box of this scene
      for collisions. We look at bounding box of model loaded in @link(URL),
      not at children (TCastleTransform) bounding boxes.

      Internal notes:
      When @true, this sets @link(TCastleSceneCore.Spatial) to [ssRendering, ssDynamicCollisions].
      This is a good setting for scenes that may be dynamic.
      When @false, this sets @link(TCastleSceneCore.Spatial) to [].
      When reading, any @link(TCastleSceneCore.Spatial) <> [] means "precise collisions". }
    property PreciseCollisions: Boolean read GetPreciseCollisions write SetPreciseCollisions default false;

    { Should the event mechanism (a basic of animations and interactions) work.

      If @true, then events will be send and received
      through X3D routes, time dependent nodes (TTimeDependentFunctionality,
      like TTimeSensorNode) will be activated and updated from @link(Time) time
      property, @link(Press), @link(Release) and other methods will activate
      key/mouse sensor nodes, scripts will be initialized and work, etc.

      In other words, this makes the scene fully animated and interacting
      with the user.

      If @false, this all doesn't work, which makes the scene static. }
    property ProcessEvents: boolean
      read FProcessEvents write SetProcessEvents default false;

    { Currently loaded scene URL. Change this property to load a scene
      from the given URL. We support many 3D and 2D scene formats,
      like X3D and glTF,
      see https://castle-engine.io/creating_data_model_formats.php .

      Setting this property works just like using the @link(Load) method with a new URL.
      In fact, using directly the @link(Load) method will also change this URL property.

      The only difference between @code(Scene.URL := 'blah.x3d') and
      @code(Scene.Load('blah.x3d')) is that setting the URL will
      @italic(not) reload the scene if you set it to the same value.
      That is, @code(Scene.URL := Scene.URL;) will not reload
      the scene (you have to use explicit @link(Load) for this.).

      Pass URL = '' to load an empty scene, this sets @link(RootNode) to @nil.
    }
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


    { If AutoAnimation is set, this animation will be automatically played.
      It is useful to determine the initial animation, played once the model
      is loaded (each time URL changes). You can also change AutoAnimation
      at any other moment at runtime (set it to something non-empty to change to a new animation;
      set it to '' to stop any animation).

      Note: Using @link(AutoAnimation) will under the hood call methods like @link(PlayAnimation),
      @link(StopAnimation) and update @link(CurrentAnimation).
      The reverse is not true: calling @link(PlayAnimation) doesn't change @link(AutoAnimation).
      So you can think of @link(AutoAnimation) as "an initial animation, activated each time
      we load the model, even if later we can change it to something else using @link(PlayAnimation)".

      @seealso AutoAnimationLoop }
    property AutoAnimation: String
      read FAutoAnimation write SetAutoAnimation;

    { Does the animation indicated by AutoAnimation loops. }
    property AutoAnimationLoop: Boolean
      read FAutoAnimationLoop write SetAutoAnimationLoop default true;

    { Transformation nodes inside the model
      that are synchronized with automatically-created children TCastleTransform.

      This allows to expose transformation nodes from glTF, X3D and other model formats
      as TCastleTransform.
      These transformation nodes include animated bones from skeletons (armatures).
      Such "exposed transformation" results in a creation of TCastleTransform child,
      with the same name and synchronized transformation (translation, rotation, scale).

      This allows to expose e.g. "hand that may hold a weapon",
      or "slot of a vehicle where to attach a camera", as TCastleTransform.
      And this allows, in turn, to attach various things to (possibly animated) transformations,
      e.g. attach model of a weapon to a hand, or attach TCastleCamera to some bone.
      The attached things will be automatically animated when the scene skeleton animates.

      Setting this property merely copies the contents using TStrings.Assign,
      as is usual for published TStrings properties.
      In CGE editor, there's a nice GUI editor to pick the transfomation nodes,
      click on "..." at this property.

      Note: the owner of auto-created children is equal to this scene's Owner.
      This is most natural when you edit this in CGE editor,
      or load using @link(TransformLoad).

      See the engine example in: examples/animations/expose_transformations_to_animate_children/ .

      @seealso ExposeTransformsPrefix }
    property ExposeTransforms: TStrings read FExposeTransforms write SetExposeTransforms;

    { Name prefix for all children created by ExposeTransforms.
      Useful to keep names unique in the scene. }
    property ExposeTransformsPrefix: String read FExposeTransformsPrefix write SetExposeTransformsPrefix;

    { Use cache to load given scene.
      This makes sense when you have multiple TCastleScene instances using the same URL,
      loading them will be much faster if they share the cache,
      so set @name to @true for all of them.

      If you have only one scene using given URL, using cache is not necessary
      and in fact it will result in a bit of wasted memory.
      So better to keep @name at @false then. }
    property Cache: Boolean read FCache write FCache default false;
  end;

  {$define read_interface}
  {$I castlescenecore_physics_deprecated.inc}
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
    skeletal animations of characters, 3D and 2D (e.g. from Spine or glTF). }
  OptimizeExtensiveTransformations: boolean = false;

  { Experimental optimization of Transform animation.
    It assumes that Transform nodes affect only geometry, i.e. their only effect
    is moving/rotating etc. X3D shapes.
    This is *usually*, but not always, true.
    In X3D, Transform node can also affect lights, Background, Fog, cameras...

    TODO: Extend it to include all cases, and use always. }
  InternalFastTransformUpdate: Boolean = false;

const
  // Old name for paLooping.
  paForceLooping    = paLooping;
  // Old name for paNotLooping.
  paForceNotLooping = paNotLooping;

  ssCollidableTriangles = ssStaticCollisions deprecated 'use ssStaticCollisions instead';

var
  InternalEnableAnimation: Boolean = true;

implementation

uses Math, DateUtils,
  X3DCameraUtils, CastleStringUtils, CastleLog,
  X3DLoad, CastleURIUtils, CastleQuaternions;

{$define read_implementation}
{$I castlescenecore_physics_deprecated.inc}
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
    {$ifndef CASTLE_SLIM_NODES}
    Node.EventBindTime.Send(ParentScene.Time, ParentScene.NextEventTime);
    {$endif}
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

function TViewpointStack.Top: TAbstractViewpointNode;
begin
  Result := (inherited Top) as TAbstractViewpointNode;
end;

procedure TViewpointStack.PushIfEmpty(Node: TAbstractViewpointNode; SendEvents: boolean);
begin
  inherited PushIfEmpty(Node, SendEvents);
end;

{ TGeneratedTextureList -------------------------------------------------- }

function TCastleSceneCore.TGeneratedTextureList.IndexOfTextureNode(TextureNode: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].TextureNode = TextureNode then
      Exit;
  Result := -1;
end;

function TCastleSceneCore.TGeneratedTextureList.FindTextureNode(TextureNode: TX3DNode): PGeneratedTexture;
var
  Index: Integer;
begin
  Index := IndexOfTextureNode(TextureNode);
  if Index <> -1 then
    Result := PGeneratedTexture(Ptr(Index)) else
    Result := nil;
end;

function TCastleSceneCore.TGeneratedTextureList.AddShapeTexture(Shape: TShape;
  Tex: TAbstractTextureNode): Pointer;
var
  GenTex: PGeneratedTexture;
  GenTexFunctionality: TGeneratedTextureFunctionality;
begin
  Result := nil;
  GenTexFunctionality := Tex.GenTexFunctionality;
  if GenTexFunctionality <> nil then
  begin
    GenTex := FindTextureNode(Tex);
    if GenTex <> nil then
    begin
      { The same generated texture node Tex is instantiated more than once.

        - For GeneratedShadowMap, the shape where it's used, and actually
          the whole place where it's used in the X3D graph, doesn't matter.
          (GeneratedShadowMap makes view from it's light/viewpoint,
          referenced explicitly in the node).
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
        WritelnWarning('X3D', 'The same GeneratedCubeMapTexture node is used (instanced) within at least two different X3D shapes. This is incorrect, as we don''t know from which shape should environment be captured');
    end else
    begin
      GenTex := PGeneratedTexture(Add);
      GenTex^.TextureNode := Tex;
      GenTex^.Functionality := GenTexFunctionality;
      { Make sure to reset InternalUpdateNeeded to true, in case it was false because
        it was already generated but now some change caused ChangedAll.
        Testcase: projected_Spotlight.x3dv from Victor Amat. }
      GenTex^.Functionality.InternalUpdateNeeded := true;
      GenTex^.Shape := Shape;
    end;
  end;
end;

procedure TCastleSceneCore.TGeneratedTextureList.UpdateShadowMaps(LightNode: TAbstractLightNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (L[I].TextureNode is TGeneratedShadowMapNode) and
       (TGeneratedShadowMapNode(L[I].TextureNode).FdLight.Value = LightNode) then
      L[I].Functionality.InternalUpdateNeeded := true;
end;

{ TTimeDependentList ------------------------------------------------- }

procedure TCastleSceneCore.TTimeDependentList.AddIfNotExists(const Item: TTimeDependentFunctionality);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

{ TVisibilitySensors --------------------------------------------------------- }

destructor TCastleSceneCore.TVisibilitySensors.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCastleSceneCore.TVisibilitySensors.Clear;
var
  V: TVisibilitySensorInstanceList;
begin
  for V in Values do
    V.Free;
  inherited Clear;
end;

{ TCastleSceneCore.TExposedTransform ----------------------------------------- }

constructor TCastleSceneCore.TExposedTransform.Create(const AParentScene: TCastleSceneCore;
  const ANode: TTransformNode; const AChild: TCastleTransform);
begin
  inherited Create;
  FParentScene := AParentScene;
  FNode := ANode;
  FTransformFunctionality := Node.TransformFunctionality;
  Assert(FTransformFunctionality <> nil);
  FChild := AChild;

  // create ChildObserver
  ChildObserver := TFreeNotificationObserver.Create(nil);
  ChildObserver.OnFreeNotification := {$ifdef FPC}@{$endif}ChildFreeNotification;
  ChildObserver.Observed := Child;
end;

destructor TCastleSceneCore.TExposedTransform.Destroy;
begin
  FreeAndNil(ChildObserver);
  inherited;
end;

procedure TCastleSceneCore.TExposedTransform.ChildFreeNotification(const Sender: TFreeNotificationObserver);
begin
  // free this TExposedTransform instance, removing it from FExposeTransforms
  ParentScene.FExposedTransforms.Remove(Self);
end;

procedure TCastleSceneCore.TExposedTransform.Synchronize;
var
  ShapeTransform: TShapeTreeTransform;
  C: Integer;
  Translation, Scale: TVector3;
  Rotation: TVector4;
  T: TTransformation;
begin
  C := TShapeTree.AssociatedShapesCount(Node);
  if C = 0 then
  begin
    if not WarningDone then
    begin
      WarningDone := true;
      WritelnWarning('Exposed transformation (bone) "%s" is not present in the shapes tree (which usually means it has no shapes), transformation is not updated', [
        Node.X3DName
      ]);
    end;
    Exit;
  end;

  if C > 1 then
  begin
    if not WarningDone then
    begin
      WarningDone := true;
      WritelnWarning('Exposed transformation (bone) "%s" is present mutliple times in the shapes tree, it has no single transformation', [
        Node.X3DName
      ]);
    end;
    Exit;
  end;

  Assert(C = 1);
  ShapeTransform := TShapeTree.AssociatedShape(Node, 0) as TShapeTreeTransform;
  T :=  ShapeTransform.TransformState.Transformation;
  FTransformFunctionality.ApplyTransform(T);
  MatrixDecompose(T.Transform, Translation, Rotation, Scale);

  { Apply to Child the accumulated transformation within this TTransformNode,
    taking into account TTransformNode hierarchy. }
  Child.Translation := Translation;
  Child.Rotation := Rotation;
  Child.Scale := Scale;

  { This would be incorrect, as we would not apply parent TTransformNode values.
  Child.Translation := Node.Translation;
  Child.Rotation := Node.Rotation;
  Child.Scale := Node.Scale;
  }
end;

class function TCastleSceneCore.TExposedTransform.X3dNameToPascal(const Prefix, S: String): String; {$ifdef FPC}static;{$endif}
const
  AllowedChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  AllowedCharsFirst = AllowedChars - ['0'..'9'];
begin
  Result := Prefix + SReplaceChars(S, AllChars - AllowedChars, '_');
  if not SCharIs(Result, 1, AllowedCharsFirst) then
    Result := '_' + Result;
end;

{ TPlayAnimationParameters --------------------------------------------------- }

constructor TPlayAnimationParameters.Create;
begin
  inherited;
  Loop := false;
  Forward := true;
end;

{ TDetectAffectedFields ------------------------------------------------------ }

type
  TDetectAffectedFields = class
  strict private
    ParentScene: TCastleSceneCore;
    AllRoutes: TX3DRouteList;
    AffectedInterpolators, AffectedTriggers: TX3DNodeList;
    procedure FindRoutesAndInterpolatorsEnumerate(Node: TX3DNode);
    function IsInterpolator(const Node: TX3DNode): Boolean;
  public
    constructor Create(const AParentScene: TCastleSceneCore);
    destructor Destroy; override;
    procedure FindRoutesAndInterpolators;
    procedure FindAnimationAffectedFields;
  end;

constructor TDetectAffectedFields.Create(const AParentScene: TCastleSceneCore);
begin
  inherited Create;
  ParentScene := AParentScene;
  AllRoutes := TX3DRouteList.Create(false);
  AffectedInterpolators := TX3DNodeList.Create(false);
  AffectedTriggers := TX3DNodeList.Create(false);
end;

destructor TDetectAffectedFields.Destroy;
begin
  FreeAndNil(AffectedInterpolators);
  FreeAndNil(AffectedTriggers);
  FreeAndNil(AllRoutes);
  inherited;
end;

procedure TDetectAffectedFields.FindRoutesAndInterpolators;
begin
  if ParentScene.RootNode <> nil then
    ParentScene.RootNode.EnumerateNodes(TX3DNode,
      {$ifdef FPC}@{$endif}FindRoutesAndInterpolatorsEnumerate,
      false);
end;

procedure TDetectAffectedFields.FindRoutesAndInterpolatorsEnumerate(Node: TX3DNode);
var
  Route: TX3DRoute;
  I: Integer;
begin
  for I := 0 to Node.RoutesCount - 1 do
  begin
    Route := Node.Routes[I];
    { Note that it's OK to add the same route instance multiple times
      to AllRoutes list below. }
    AllRoutes.Add(Route);

    { Found route from TimeSensor, and check if TimeSensor has detectAffectedFields. }
    if (Route.SourceNode is TTimeSensorNode) and
       TTimeSensorNode(Route.SourceNode).DetectAffectedFields then
    begin
      { Found route from some TimeSensor.fraction_changed to some interpolator/sequencer.set_fraction. }
      if (Route.SourceEvent = TTimeSensorNode(Route.SourceNode).EventFraction_Changed) then
      begin
        if IsInterpolator(Route.DestinationNode) and
           (Route.DestinationEvent.X3DName = 'set_fraction') then
        begin
          { Note that it's OK to add the same interpolator instance multiple times below. }
          AffectedInterpolators.Add(Route.DestinationNode);
        end;
      end else

      { Found route from some TimeSensor.isActive to some trigger input. }
      if (Route.SourceEvent = TTimeSensorNode(Route.SourceNode).EventIsActive) then
      begin
        if ( (Route.DestinationNode is TValueTriggerNode) and
             (Route.DestinationEvent = TValueTriggerNode(Route.DestinationNode).EventTrigger) ) or
           ( (Route.DestinationNode is TIntegerTriggerNode) and
             (Route.DestinationEvent = TIntegerTriggerNode(Route.DestinationNode).EventSet_boolean) ) or
           ( (Route.DestinationNode is TTimeTriggerNode) and
             (Route.DestinationEvent = TTimeTriggerNode(Route.DestinationNode).EventSet_boolean) ) then
        begin
          { Note that it's OK to add the same trigger instance multiple times below. }
          AffectedTriggers.Add(Route.DestinationNode);
        end;
      end;
    end;
  end;
end;

function TDetectAffectedFields.IsInterpolator(const Node: TX3DNode): Boolean;
begin
  Result :=
    (Node is TAbstractInterpolatorNode) or
    (Node is TAbstractSequencerNode);
end;

procedure TDetectAffectedFields.FindAnimationAffectedFields;

  { Does this event come from an exposed field interface declaration?
    This is used to detect ValueTrigger fields that send information.
    See TValueTriggerNode.EventTriggerReceive how TValueTriggerNode does it. }
  function ExposedCustomField(const Node: TX3DNode; const Event: TX3DEvent): Boolean;
  var
    Field: TX3DField;
  begin
    Field := Node.FieldSendingEvent(Event);
    Result :=
      (Field <> nil) and
      (Field.ParentInterfaceDeclaration <> nil);
  end;

  { Add to the "affected" list the field indicated by given route destination
    (as Node and Event of this node).
    Node = nil is allowed here (Route.DestinationNode may be nil if node was freed,
    e.g. delete shape in view3dscene). }
  procedure RouteDestinationAffectsField(const Node: TX3DNode; const Event: TX3DEvent);
  var
    Field: TX3DField;
  begin
    if Node <> nil then
    begin
      Field := Node.FieldSetByEvent(Event);
      if (Field <> nil) and
         (not ParentScene.AnimationAffectedFields.Contains(Field)) then
      begin
        // WritelnLog('Found affected field %s', [Field.NiceName]);
        Field.InternalSaveResetValue;
        ParentScene.AnimationAffectedFields.Add(Field);
      end;
    end;
  end;

var
  Route: TX3DRoute;
begin
  for Route in AllRoutes do
    if IsInterpolator(Route.SourceNode) then
    begin
      if (Route.SourceEvent.X3DName = 'value_changed') and
         (AffectedInterpolators.IndexOf(Route.SourceNode) <> -1) then
        RouteDestinationAffectsField(Route.DestinationNode, Route.DestinationEvent);
    end else
    if Route.SourceNode is TValueTriggerNode then
    begin
      if ExposedCustomField(Route.SourceNode, Route.SourceEvent) and
         (AffectedTriggers.IndexOf(Route.SourceNode) <> -1) then
        RouteDestinationAffectsField(Route.DestinationNode, Route.DestinationEvent);
    end else
    if Route.SourceNode is TIntegerTriggerNode then
    begin
      if (Route.SourceEvent = TIntegerTriggerNode(Route.SourceNode).EventTriggerValue) and
         (AffectedTriggers.IndexOf(Route.SourceNode) <> -1) then
        RouteDestinationAffectsField(Route.DestinationNode, Route.DestinationEvent);
    end else
    if Route.SourceNode is TTimeTriggerNode then
    begin
      if (Route.SourceEvent = TTimeTriggerNode(Route.SourceNode).EventTriggerTime) and
         (AffectedTriggers.IndexOf(Route.SourceNode) <> -1) then
        RouteDestinationAffectsField(Route.DestinationNode, Route.DestinationEvent);
    end;
end;

{ TCastleSceneCore ----------------------------------------------------------- }

constructor TCastleSceneCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRootNode := nil;
  FOwnsRootNode := true;
  FShapesHash := 1;

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
  GeneratedTextures := TGeneratedTextureList.Create;
  ProximitySensors := TProximitySensorInstanceList.Create(false);
  FVisibilitySensors := TVisibilitySensors.Create;
  ScreenEffectNodes := TX3DNodeList.Create(false);
  ScheduledHumanoidAnimateSkin := TX3DNodeList.Create(false);
  KeyDeviceSensorNodes := TX3DNodeList.Create(false);
  BillboardNodes := TX3DNodeList.Create(false);
  TimeDependentList := TTimeDependentList.Create(false);
  FAnimationsList := TStringList.Create;
  TStringList(FAnimationsList).CaseSensitive := true; // X3D node names are case-sensitive
  AnimationAffectedFields := TX3DFieldList.Create(false);

  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;

  FShadowMaps := true;
  FShadowMapsDefaultSize := DefaultShadowMapsDefaultSize;
  FAnimationPrefix := DefaultAnimationPrefix;

  FAutoAnimationLoop := true;

  FExposeTransforms := TStringList.Create;
  TStringList(FExposeTransforms).OnChange := {$ifdef FPC}@{$endif} ExposeTransformsChange;
  FExposedTransforms := TExposedTransformList.Create(true);

  { We could call here ScheduleChangedAll (or directly ChangedAll),
    but there should be no need. FRootNode remains nil,
    and our current state should be equal to what ScheduleChangedAll
    would set. This is (potentially) a small time saving,
    as ScheduleChangedAll does a lot of calls (although probably is fast
    anyway when RootNode = nil). }
end;

procedure TCastleSceneCore.BeforeDestruction;
begin
  FreeRootNode;
  inherited;
end;

destructor TCastleSceneCore.Destroy;
begin
  { This also deinitializes script nodes. }
  ProcessEvents := false;

  FreeAndNil(FExposeTransforms);
  FreeAndNil(FExposedTransforms);
  FreeAndNil(ScheduledHumanoidAnimateSkin);
  FreeAndNil(ScreenEffectNodes);
  FreeAndNil(ProximitySensors);
  FreeAndNil(FVisibilitySensors);
  FreeAndNil(GeneratedTextures);
  FreeAndNil(FCompiledScriptHandlers);
  FreeAndNil(KeyDeviceSensorNodes);
  FreeAndNil(BillboardNodes);
  FreeAndNil(TimeDependentList);

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
  FreeAndNil(AnimationAffectedFields);
  FreeAndNil(PreviousPartialAffectedFields);

  inherited;
end;

procedure TCastleSceneCore.FreeRootNode;
begin
  { These must be done always before freeing }
  BeforeNodesFree;
  PointingDeviceClear;

  if FRootNodeCacheOrigin <> nil then
    X3DCache.FreeNode(FRootNodeCacheOrigin);
  Assert(FRootNodeCacheOrigin = nil);

  { Not calling UnregisterScene when FOwnsRootNode=true is just an optimization.
    There's no point in calling recursive UnregisterScene if the FRootNode
    will be freed right afterwards.

    TODO: This optimization is actually not without a problem.
    If the RootNode has some child that has other parents (outside of this
    scene, or with KeepExistingBegin) then they will remain existing,
    and will have a dangling reference to this Scene.
    This happened with internal TAppearanceNode in CastleTerrain at one point.

    In general, we do not guarantee detaching invalid Scene reference now.
    E.g. if a node will stop being part of a scene,
    because we do

      Appearance.Material := OldMaterial;
      // add Appearance to Scene in any way
      Appearance.Material := NewMaterial;
      FreeAndNil(Scene);

    ... then OldMaterial will have invalid reference in OldMaterial.Scene.
  }
  if (FRootNode <> nil) and (not FOwnsRootNode) then
    FRootNode.UnregisterScene;

  if FOwnsRootNode then
    FreeAndNil(FRootNode)
  else
    FRootNode := nil;
  Assert(FRootNode = nil);
end;

procedure TCastleSceneCore.Load(const ARootNode: TX3DRootNode; const AOwnsRootNode: Boolean;
  const AOptions: TSceneLoadOptions);
begin
  LoadCore(ARootNode, nil, AOwnsRootNode, AOptions);
end;

procedure TCastleSceneCore.LoadCore(const ARootNode: TX3DRootNode;
  const ARootNodeCacheOrigin: TX3DRootNode;
  const AOwnsRootNode: boolean;
  const AOptions: TSceneLoadOptions);
var
  RestoreProcessEvents: boolean;
begin
  { ARootNodeCacheOrigin can be non-nil only if ARootNode is non-nil. }
  Assert(not ((ARootNodeCacheOrigin <> nil) and (ARootNode = nil)));

  { temporarily turn off events, to later initialize and turn them on }
  RestoreProcessEvents := ProcessEvents;
  ProcessEvents := false;

  FreeRootNode;

  FRootNode := ARootNode; // set using FRootNode, we will call ChangedAll later
  FRootNodeCacheOrigin := ARootNodeCacheOrigin;
  FOwnsRootNode := AOwnsRootNode;
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

  { Call ChangedAll instead of ScheduleChangedAll.
    That's because below we potentially do SetProcessEvents(true)
    (if loading a scene when ProcessEvents already enabled),
    and it may require that ChangedAll already run (e.g. it may
    initialize Script nodes, that require Node.Scene to be set,
    see https://github.com/castle-engine/view3dscene/issues/16 ). }
  ChangedAll;

  if not (slDisableResetTime in AOptions) then
    ResetTimeAtLoad;

  { restore events processing, initialize new scripts and such }
  ProcessEvents := RestoreProcessEvents;

  UpdateAutoAnimation(false);
end;

procedure TCastleSceneCore.Load(const AURL: string; const AllowStdIn: boolean;
  const AResetTime: boolean);
var
  Options: TSceneLoadOptions;
begin
  Options := [];
  if not AResetTime then
    Include(Options, slDisableResetTime);
  Load(AURL, Options);
end;

procedure TCastleSceneCore.Load(const AURL: string; const AOptions: TSceneLoadOptions);
var
  TimeStart: TCastleProfilerTime;
  NewRoot, NewRootCacheOrigin: TX3DRootNode;
  C: TCastleCollider;
begin
  TimeStart := Profiler.Start('Loading "' + URIDisplay(AURL) + '" (TCastleSceneCore)');
  try
    NewRoot := nil; // set this as RootNode when AURL is ''
    NewRootCacheOrigin := nil;

    if AURL <> '' then
    begin
      { If LoadNode fails:

        - When CastleDesignMode is false:
          We make an exception (just pass the exception from LoadNode),
          and we do not change the RootNode or URL.
          So currently loaded scene will remain 100% valid.

        - When CastleDesignMode is true:
          We *do* change the RootNode (to empty) and URL
          (to allow user to comfortably correct the URL in editor),
          and we make a warning.
          This way the design with invalid URL will still load nicely in the editor.
      }

      try
        if Cache then
        begin
          NewRootCacheOrigin := X3DCache.LoadNode(AURL);
          NewRoot := NewRootCacheOrigin.DeepCopy as TX3DRootNode;
        end else
        begin
            { Load using cache, in case the scene was cached using
            <warmup_cache> or Cache. This way we use cache (without incrementing
            reference count in cache) even when Cache=false. }
          NewRoot := X3DCache.TryCopyNode(AURL);
          if NewRoot = nil then
            NewRoot := LoadNode(AURL);
        end;
      except
        on E: Exception do
        begin
          if CastleDesignMode then
          begin
            WritelnWarning('TCastleSceneCore', 'Failed to load scene "%s": %s',
              [URIDisplay(AURL), ExceptMessage(E)]);
            NewRoot := nil;
            NewRootCacheOrigin := nil;
          end else
            raise;
        end;
      end;
    end;

    { Set FURL before calling Load below.
      This way eventual warning from Load (like "animation not found",
      in case AutoAnimation is used) will mention the new URL, not the old one. }
    FURL := AURL;

    LoadCore(NewRoot, NewRootCacheOrigin, true, AOptions);

    { After loading a new model we need to
      - update sizes calculated by AutoSize for simple colliders
      - update triangles used by TCastleMeshCollider (note that this code
        will only update TCastleMeshCollider that is our behavior,
        it doesn't notify TCastleMeshCollider instances elsewhere that may refer to us). }
    C := FindBehavior(TCastleCollider) as TCastleCollider;
    if C <> nil then
      C.InternalTransformChanged(Self);
  finally Profiler.Stop(TimeStart) end;
end;

procedure TCastleSceneCore.UpdateAutoAnimation(const StopIfPlaying: Boolean);
begin
  // when IsLoading, delay this to Loaded
  if not IsLoading then
  begin
    if AutoAnimation <> '' then
    begin
      if PlayAnimation(AutoAnimation, AutoAnimationLoop) then
        { call ForceInitialAnimationPose, to avoid blinking with "setup pose"
          right after loading the UI design from file. }
        ForceInitialAnimationPose;
    end else
    if StopIfPlaying then
    begin
      StopAnimation;
      ResetAnimationState;
    end;
  end;
end;

procedure TCastleSceneCore.Loaded;
begin
  inherited;
  if FPendingSetUrl <> '' then
  begin
    Url := FPendingSetUrl;
    FPendingSetUrl := '';
  end;
  UpdateAutoAnimation(false);
  ExposeTransformsChange(nil);
end;

procedure TCastleSceneCore.SetAutoAnimationLoop(const Value: Boolean);
begin
  if FAutoAnimationLoop <> Value then
  begin
    FAutoAnimationLoop := Value;
    UpdateAutoAnimation(true);
  end;
end;

procedure TCastleSceneCore.SetAutoAnimation(const Value: String);
begin
  if FAutoAnimation <> Value then
  begin
    FAutoAnimation := Value;
    UpdateAutoAnimation(true);
  end;
end;

procedure TCastleSceneCore.SetRootNode(const Value: TX3DRootNode);
begin
  if FRootNode <> Value then
  begin
    FreeRootNode;
    FRootNode := Value;
    ScheduleChangedAll;
  end;
end;

procedure TCastleSceneCore.Save(const AURL: string);
begin
  if RootNode <> nil then
    SaveNode(RootNode, AURL, ApplicationName);
  FURL := AURL;
end;

procedure TCastleSceneCore.SetURL(const AValue: string);
begin
  if AValue <> FURL then
  begin
    if IsLoading and (AValue <> '') then
      { Defer actually loading URL to later, when Loading is called, to use proper Cache value. }
      FPendingSetUrl := AValue
    else
      Load(AValue);
  end;
end;

procedure TCastleSceneCore.ReloadUrl;
begin
  { Naive implementation:
      TempUrl := Url;
      Url := '';
      Url := TempUrl;
    But this would make warning in case of non-empty AutoAnimation,
    that such animation doesn't exist in empty scene. }
  Load(Url);
end;

(* This is working, and ultra-fast thanks to TShapeTree.AssociatedShape,
   but in practice it's unused now.

function TCastleSceneCore.ShapeBoundingBox(const Node: TShapeNode): TBox3D;
var
  I: Integer;
begin
  C := TShapeTree.AssociatedShapesCount(Node);
  case C of
    0: Result := TBox3D.Empty;
    1: Result := TShape(TShapeTree.AssociatedShape(Node, 0)).BoundingBox;
    else
      begin
        Result := TBox3D.Empty;
        for I := 0 to C - 1 do
          Result.Include(TShape(TShapeTree.AssociatedShape(Node, I)).BoundingBox);
      end;
  end;
end;
*)

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
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Result := TBox3D.Empty;
  ShapeList := Shapes.TraverseList(true);
  for Shape in ShapeList do
    Result.Include(Shape.BoundingBox);
end;

function TCastleSceneCore.CalculateVerticesCount: Cardinal;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Result := 0;
  ShapeList := Shapes.TraverseList(true);
  for Shape in ShapeList do
    Result := Result + Shape.VerticesCount;
end;

function TCastleSceneCore.CalculateTrianglesCount: Cardinal;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Result := 0;
  ShapeList := Shapes.TraverseList(true);
  for Shape in ShapeList do
    Result := Result + Shape.TrianglesCount;
end;

function TCastleSceneCore.LocalBoundingBoxNoChildren: TBox3D;
begin
  if Exists then
  begin
    if not (fvLocalBoundingBox in Validities) then
    begin
      FLocalBoundingBox := CalculateLocalBoundingBox;
      Include(Validities, fvLocalBoundingBox);
    end;
    Result := FLocalBoundingBox;
  end else
    Result := TBox3D.Empty;
end;

function TCastleSceneCore.LocalBoundingBox: TBox3D;
begin
  Result := LocalBoundingBoxNoChildren;
  Result.Include(inherited LocalBoundingBox);
end;

function TCastleSceneCore.VerticesCount: Cardinal;
begin
  if not (fvVerticesCount in Validities) then
  begin
    FVerticesCount := CalculateVerticesCount;
    Include(Validities, fvVerticesCount);
  end;
  Result := FVerticesCount;
end;

function TCastleSceneCore.TrianglesCount: Cardinal;
begin
  if not (fvTrianglesCount in Validities) then
  begin
    FTrianglesCount := CalculateTrianglesCount;
    Include(Validities, fvTrianglesCount);
  end;
  Result := FTrianglesCount;
end;

function TCastleSceneCore.VerticesCount(const Ignored: Boolean): Cardinal;
begin
  Result := VerticesCount();
end;

function TCastleSceneCore.TrianglesCount(const Ignored: Boolean): Cardinal;
begin
  Result := TrianglesCount();
end;

function TCastleSceneCore.CreateShape(const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState; const ParentInfo: PTraversingInfo): TShape;
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

  { Handle node with TTransformFunctionality }
  procedure HandleTransform(const TransformFunctionality: TTransformFunctionality);
  var
    TransformNode: TX3DNode;
    TransformTree: TShapeTreeTransform;
    Traverser: TChangedAllTraverser;
  begin
    TransformNode := TransformFunctionality.Parent;

    TransformTree := TShapeTreeTransform.Create(ParentScene);
    TransformTree.TransformFunctionality := TransformFunctionality;

    { We want to save at TransformState the state right before traversing
      inside this TransformNode.

      StateStack.Top is bad --- it is already modified by TransformNode
      transformation, we don't want this, the very purpose of TransformState
      is to later restart traversing from a TransformNode that changed
      it's transformation. Clearly, TransformState must not depend on
      current TransformNode transformation.

      So we cheat a little, knowing that internally every node implementing TTransformFunctionality
      does StateStack.Push inside BeforeTraverse exactly once and then
      modifies transformation.
      (This happens for both TAbstractInternalGroupingNode and THAnimHumanoidNode.
      Right now, node with TTransformFunctionality is always one of those.)
      So we know that previous state lies safely at PreviousTop.

      Exception: As TX3DRootNode.SeparateGroup is false now,
      it needs special case to look at just StateStack.Top.
      TODO: This is wrong though, i.e. the StateStack.Top has the TX3DRootNode
      already applied. There's no state without the TX3DRootNode applied... }
    if TransformNode is TX3DRootNode then
      TransformTree.TransformState.Assign(StateStack.Top)
    else
      TransformTree.TransformState.Assign(StateStack.PreviousTop);

    ShapesGroup.Children.Add(TransformTree);

    { update ParentScene.BillboardNodes }
    if TransformNode is TBillboardNode then
      ParentScene.BillboardNodes.Add(TransformNode);

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
        {$ifdef FPC}@{$endif}Traverser.Traverse, ParentInfo);
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
        ChildNode.TraverseInternal(StateStack, TX3DNode,
          {$ifdef FPC}@{$endif}Traverser.Traverse, ParentInfo);
      finally FreeAndNil(Traverser) end;
    end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TLODNode);
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
    LODTree.LODInverseTransform^ := StateStack.Top.Transformation.InverseTransform;
    ShapesGroup.Children.Add(LODTree);
    ParentScene.ShapeLODs.Add(LODTree);

    { First add TShapeTreeGroup.Create as children, as many times
      as there are LODNode.FdChildren. Reason: LODTree.CalculateLevel
      uses this Count. }

    for I := 0 to LODNode.FdChildren.Count - 1 do
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
        ChildNode.TraverseInternal(StateStack, TX3DNode,
          {$ifdef FPC}@{$endif}Traverser.Traverse, ParentInfo);
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
    PSI.InverseTransform := StateStack.Top.Transformation.InverseTransform;
    PSI.IsActive := false; { IsActive = false initially }

    ShapesGroup.Children.Add(PSI);
    ParentScene.ProximitySensors.Add(PSI);
  end;

  procedure HandleVisibilitySensor(const Node: TVisibilitySensorNode);
  var
    VSI: TVisibilitySensorInstance;
    Instances: TCastleSceneCore.TVisibilitySensorInstanceList;
  begin
    VSI := TVisibilitySensorInstance.Create(ParentScene);
    VSI.Node := Node;
    VSI.Transform := StateStack.Top.Transformation.Transform;
    VSI.Box := Node.Box.Transform(VSI.Transform);

    ShapesGroup.Children.Add(VSI);

    { add to ParentScene.VisibilitySensors map }
    if not ParentScene.VisibilitySensors.TryGetValue(Node, Instances) then
    begin
      Instances := TCastleSceneCore.TVisibilitySensorInstanceList.Create(false);
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
    if (ssDynamicCollisions in ParentScene.FSpatial) and
      Shape.Collidable then
    begin
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
      ParentScene.InternalGlobalLights.Add(
        TAbstractLightNode(Node).CreateLightInstance(StateStack.Top));
  end else

  if Node is TSwitchNode then
  begin
    HandleSwitch(TSwitchNode(Node));
  end else
  if Node is TLODNode then
  begin
    HandleLOD(TLODNode(Node));
  end else
  if Node.TransformFunctionality <> nil then
  begin
    HandleTransform(Node.TransformFunctionality);
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

    InternalIncShapesHash; // TShapeTreeLOD returns now different things
  end;
end;

procedure TCastleSceneCore.BeforeNodesFree(const InternalChangedAll: boolean);
begin
  { Stuff that will be recalculated by ChangedAll }
  BillboardNodes.Count := 0;
  GeneratedTextures.Count := 0;
  ProximitySensors.Count := 0;
  VisibilitySensors.Clear;
  ScreenEffectNodes.Count := 0;
  ScheduledHumanoidAnimateSkin.Count := 0;
  KeyDeviceSensorNodes.Clear;
  TimeDependentList.Clear;
  FExposedTransforms.Clear;

  { clear animation stuff, since any TTimeSensorNode may be freed soon }
  if PlayingAnimationNode <> nil then
    PlayingAnimationNode.EventIsActive.RemoveNotification(
      {$ifdef FPC}@{$endif}PlayingAnimationIsActive);
  PlayingAnimationNode := nil;
  PlayingAnimationStopNotification := nil;
  FCurrentAnimation := nil;
  NewPlayingAnimationNode := nil;
  NewPlayingAnimationUse := false;
  PreviousPlayingAnimation := nil;
  FreeAndNil(PreviousPartialAffectedFields);

  if not InternalChangedAll then
  begin
    { Clean Shapes, ShapeLODs.
      TShapeTree and descendants keep references to Nodes. }
    FreeAndNil(FShapes);
    FShapes := TShapeTreeGroup.Create(Self);
    ShapeLODs.Clear;
    InternalIncShapesHash;
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
  begin
    if (Node.Scene <> nil) and
       (Node.Scene <> Self) and
       (not InternalNodeSharing) then
      WritelnWarning('X3D node %s is already part of another TCastleScene instance.' + ' You cannot use the same X3D node in multiple instances of TCastleScene. Instead you must copy the node, using "Node.DeepCopy". It is usually most comfortable to copy the entire scene, using "TCastleScene.Clone".',
        [Node.NiceName]);
    Node.Scene := Self;
  end;

  { We're using AddIfNotExists, not simple Add, below:

    - for time-dependent nodes (TimeSensor, MovieTexture etc.),
      duplicates would cause time to be then incremented many times
      during single SetTime, so their local time would grow too fast.

    - for other sensors, events would be passed twice.
  }

  if Node is TAbstractKeyDeviceSensorNode then
    KeyDeviceSensorNodes.AddIfNotExists(Node)
  else
  if Node.TimeFunctionality <> nil then
  begin
    TimeDependentList.AddIfNotExists(Node.TimeFunctionality);

    // update NeedsDetectAffectedFields if necessary
    if (Node is TTimeSensorNode) and TTimeSensorNode(Node).DetectAffectedFields then
      NeedsDetectAffectedFields := true;
  end;
end;

procedure TCastleSceneCore.ChangedAll(const OnlyAdditions: Boolean);

  { Add where necessary lights with scope = global. }
  procedure AddGlobalLights;

    procedure AddLightEverywhere(const L: TLightInstance);
    var
      ShapeList: TShapeList;
      Shape: TShape;
    begin
      ShapeList := Shapes.TraverseList(false);
      for Shape in ShapeList do
        Shape.State.AddLight(L);
    end;

  var
    I: Integer;
    L: PLightInstance;
  begin
    { Here we only deal with light scope = lsGlobal case.
      Other scopes are handled during traversing. }
    for I := 0 to InternalGlobalLights.Count - 1 do
    begin
      L := PLightInstance(InternalGlobalLights.Ptr(I));
      AddLightEverywhere(L^);
    end;
  end;

  { Assigns nodes TX3DNode.Scene, and adds nodes to KeyDeviceSensorNodes
    and TimeDependentList lists. }
  procedure EnumerateNodes;
  begin
    if RootNode <> nil then
      RootNode.EnumerateNodes(TX3DNode,
        {$ifdef FPC}@{$endif}ChangedAllEnumerateCallback, false);
  end;

  procedure DetectAffectedFields;
  var
    Detector: TDetectAffectedFields;
  begin
    Detector := TDetectAffectedFields.Create(Self);
    try
      Detector.FindRoutesAndInterpolators;
      Detector.FindAnimationAffectedFields;
    finally FreeAndNil(Detector) end;
  end;

var
  Traverser: TChangedAllTraverser;
  TimeStart: TCastleProfilerTime;
begin
  { Whether this is called by EndChangesSchedule, or from other places,
    we can always reset the ChangedAllScheduled flag.

    Note that ChangedAll calls ChangedAllEnumerate inside,
    which in turn calls Begin/EndChangesSchedule.
    So ChangedAllScheduled flag must be ready for this,
    so it should be reset early, at the very beginning of ChangedAll implementation. }
  ChangedAllScheduled := false;

  TimeStart := Profiler.Start('ChangedAll for ' + Name + ' from ' + URIDisplay(URL));
  try

  { We really need to use InternalDirty here, to forbid rendering during this.

    For example, ProcessShadowMapsReceivers work assumes this:
    otherwise, RootNode.Traverse may cause some progress Step call
    (note: this is old comment, progress is not possible now)
    which may call Render which may prepare GLSL shadow map shader
    that will be freed by the following ProcessShadowMapsReceivers call.
    Testcase: view3dscene open simple_shadow_map_teapots.x3dv, turn off
    shadow maps "receiveShadows" handling, then turn it back on
    --- will crash without "InternalDirty" variable safety. }
  Inc(InternalDirty);
  try

  if LogChanges then
    WritelnLog('X3D changes', 'ChangedAll (OnlyAdditions: %s)',
      [BoolToStr(OnlyAdditions, true)]);

  { TODO:
    We ignore OnlyAdditions now.
    We have to even do BeforeNodesFree always, even when OnlyAdditions,
    the code below (may) assume it is always done, e.g. all lists are cleared in BeforeNodesFree. }

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
    InternalGlobalLights.Clear;
    FViewpointsArray.Clear;
    FAnimationsList.Clear;
    AnimationAffectedFields.Clear;
    NeedsDetectAffectedFields := false;
    InternalIncShapesHash;

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
        RootNode.Traverse(TX3DNode,
          {$ifdef FPC}@{$endif}Traverser.Traverse);
      finally FreeAndNil(Traverser) end;

      AddGlobalLights;

      EnumerateNodes;

      if NeedsDetectAffectedFields then
        DetectAffectedFields;
    end;

    { Wait until loading finished before calling ExposeTransformsChange.
      Otherwise we would create new TCastleTransform children in ExposeTransformsChange,
      instead of reusing existing ones. }
    if not IsLoading then
      ExposeTransformsChange(nil);

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
  Shapes.EnumerateTextures(
    {$ifdef FPC}@{$endif}GeneratedTextures.AddShapeTexture);

  if ScheduleHeadlightOnFromNavigationInfoInChangedAll then
  begin
    ScheduleHeadlightOnFromNavigationInfoInChangedAll := false;
    UpdateHeadlightOnFromNavigationInfo;
  end;

  ForceTeleportTransitions := false;

  if LogShapes then
    WritelnLogMultiline('Shapes tree', Shapes.DebugInfo);

  finally Dec(InternalDirty) end;

  finally Profiler.Stop(TimeStart, true, true) end;
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
    like Transform.set_translation.

    So many TransformChange traversals may run at once, so they must
    have different state variables. }
  TTransformChangeHelper = class
    ParentScene: TCastleSceneCore;
    Shapes: PShapesParentInfo;
    ChangingNode: TX3DNode;
    AnythingChanged: boolean;
    Inside: boolean;
    { If = 0, we're in active or inactive graph part (we don't know).
      If > 0, we're in inactive graph part (TransformChangeTraverse
      may enter there, since our changing Transform node (or some of it's
      children) may be inactive; but we have to update all shapes,
      active or not). }
    Inactive: Cardinal;
    function TransformChangeTraverse(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
  end;

function TTransformChangeHelper.TransformChangeTraverse(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;

  { Handle node with TTransformFunctionality }
  procedure HandleTransform(const TransformNode: TX3DNode);
  var
    ShapeTransform: TShapeTreeTransform;
    OldShapes: PShapesParentInfo;
    NewShapes: TShapesParentInfo;
  begin
    if TransformNode = ChangingNode then
    begin
      if Inside then
        WritelnLog('X3D transform', 'Cycle in X3D graph detected: transform node is a child of itself');
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
      the modified Transform node.

      As in TChangedAllTraverser.Traverse, we apply special treatment
      for TX3DRootNode.
      TODO: As in TChangedAllTraverser.Traverse, this StateStack.Top is actually
      not correct in case TX3DRootNode does any scaling. StateStack.Top has the scaling
      applied, we want transformation before. }
    if TransformNode is TX3DRootNode then
      ShapeTransform.TransformState.AssignTransform(StateStack.Top)
    else
      ShapeTransform.TransformState.AssignTransform(StateStack.PreviousTop);

    OldShapes := Shapes;
    try
      { NewShapes group is just our ShapeTransform. Transform children do not
        have addition TShapeTreeGroup, unlike Switch/LOD nodes. }
      NewShapes.Group := ShapeTransform;
      NewShapes.Index := 0;
      Shapes := @NewShapes;

      TransformNode.TraverseIntoChildren(
        StateStack, TX3DNode,
        {$ifdef FPC}@{$endif}Self.TransformChangeTraverse, ParentInfo);
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

      for I := 0 to SwitchNode.FdChildren.Count - 1 do
      begin
        NewShapes.Group := ShapeSwitch.Children[I] as TShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        ChildInactive := I <> SwitchNode.FdWhichChoice.Value;
        if ChildInactive then Inc(Inactive);

        SwitchNode.FdChildren[I].TraverseInternal(
          StateStack, TX3DNode,
          {$ifdef FPC}@{$endif}Self.TransformChangeTraverse,
          ParentInfo);

        if ChildInactive then Dec(Inactive);
      end;

    finally Shapes := OldShapes end;

    TraverseIntoChildren := false;
  end;

  procedure HandleLOD(LODNode: TLODNode);
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

    { by the way, update LODInverseTransform, since it changed }
    if Inside then
    begin
      ShapeLOD.LODInverseTransform^ := StateStack.Top.Transformation.InverseTransform;
      if ParentScene.ProcessEvents and
         ParentScene.GetCameraLocal(CameraLocalPosition) then
        ParentScene.UpdateLODLevel(ShapeLOD, CameraLocalPosition);
    end;

    OldShapes := Shapes;
    try
      { Just like in HandleSwitch: we have to enter *every* LOD child
        (while normal Traverse would enter only active child),
        because changing Transform  node may be in inactive graph parts. }

      for I := 0 to LODNode.FdChildren.Count - 1 do
      begin
        NewShapes.Group := ShapeLOD.Children[I] as TShapeTreeGroup;
        NewShapes.Index := 0;
        Shapes := @NewShapes;

        if Cardinal(I) <> ShapeLOD.Level then Inc(Inactive);

        LODNode.FdChildren[I].TraverseInternal(
          StateStack, TX3DNode,
          {$ifdef FPC}@{$endif}Self.TransformChangeTraverse,
          ParentInfo);

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
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    // TODO: Optimize using TShapeTree.AssociatedShape

    ShapeList := ParentScene.Shapes.TraverseList(false);
    for Shape in ShapeList do
    begin
      HandleLightsList(Shape.OriginalState.Lights);
      if Shape.State <> Shape.OriginalState then
        HandleLightsList(Shape.State.Lights);
    end;

    { Update also light state on GlobalLights list, in case other scenes
      depend on this light. Testcase: planets-demo. }
    HandleLightsList(ParentScene.InternalGlobalLights);

    { force update of GeneratedShadowMap textures that used this light }
    ParentScene.GeneratedTextures.UpdateShadowMaps(LightNode);

    ParentScene.VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleProximitySensor(Node: TProximitySensorNode);
  var
    Instance: TProximitySensorInstance;
    CameraVectors: TViewVectors;
  begin
    Check(Shapes^.Index < Shapes^.Group.Children.Count,
      'Missing shape in Shapes tree');
    Instance := Shapes^.Group.Children[Shapes^.Index] as TProximitySensorInstance;
    Inc(Shapes^.Index);

    Instance.InverseTransform := StateStack.Top.Transformation.InverseTransform;

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
    Instance.Transform := StateStack.Top.Transformation.Transform;
    Instance.Box := Node.Box.Transform(Instance.Transform);
  end;

var
  Shape: TShape;
begin
  Result := nil;

  case Node.TransformationChange of
    ntcNone: ;
    ntcSwitch: HandleSwitch(TSwitchNode(Node));
    ntcLOD: HandleLOD(TLODNode(Node));
    ntcTransform: HandleTransform(Node);
    ntcGeometry:
      begin
        { get Shape and increase Shapes^.Index }
        Check(Shapes^.Index < Shapes^.Group.Children.Count,
          'Missing shape in Shapes tree');
        Shape := Shapes^.Group.Children[Shapes^.Index] as TShape;
        Inc(Shapes^.Index);

        Shape.State.AssignTransform(StateStack.Top);
        Shape.Changed(Inactive <> 0, [chTransform]);

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
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('HandleTransform: NodeTransformationChange?');
    {$endif}
  end;
end;

procedure TCastleSceneCore.TransformationChanged(const TransformFunctionality: TTransformFunctionality);
var
  TransformChangeHelper: TTransformChangeHelper;
  TransformShapesParentInfo: TShapesParentInfo;
  TraverseStack: TX3DGraphTraverseStateStack;
  C, I: Integer;
  TransformShapeTree: TShapeTreeTransform;
  DoVisibleChanged: boolean;
  TransformNode: TX3DNode;
begin
  TransformNode := TransformFunctionality.Parent;

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

  Assert(not OptimizeExtensiveTransformations);

  C := TShapeTree.AssociatedShapesCount(TransformNode);

  if LogChanges then
    WritelnLog('X3D changes', Format('Transform node %s change: present %d times in the scene graph',
      [TransformNode.X3DType, C]));

  DoVisibleChanged := false;

  if InternalFastTransformUpdate then
  begin
    for I := 0 to C - 1 do
      TShapeTree.AssociatedShape(TransformNode, I).FastTransformUpdate(DoVisibleChanged);
    if DoVisibleChanged then
      Validities := Validities - [fvLocalBoundingBox];
  end else
  begin
    TraverseStack := nil;
    TransformChangeHelper := nil;
    try
      TraverseStack := TX3DGraphTraverseStateStack.Create;

      { initialize TransformChangeHelper, set before the loop properties
        that cannot change }
      TransformChangeHelper := TTransformChangeHelper.Create;
      TransformChangeHelper.ParentScene := Self;
      TransformChangeHelper.ChangingNode := TransformNode;

      for I := 0 to C - 1 do
      begin
        TransformShapeTree := TShapeTree.AssociatedShape(TransformNode, I) as TShapeTreeTransform;
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
          {$ifdef FPC}@{$endif}TransformChangeHelper.TransformChangeTraverse,
          nil);

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
  end;

  if DoVisibleChanged then
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
end;

procedure TCastleSceneCore.RootTransformationChanged;
var
  TransformChangeHelper: TTransformChangeHelper;
  TransformShapesParentInfo: TShapesParentInfo;
  TraverseStack: TX3DGraphTraverseStateStack;
  DoVisibleChanged: boolean;
begin
  if LogChanges then
    WritelnLog('X3D changes', 'Transform root node change');

  if RootNode = nil then Exit;
  if not (Shapes is TShapeTreeGroup) then
  begin
    WritelnWarning('X3D changes', 'Root node did not create corresponding TShapeTreeGroup, transformation will not be applied correctly. Submit a bug');
    Exit;
  end;

  DoVisibleChanged := false;

  if InternalFastTransformUpdate then
  begin
    Shapes.FastTransformUpdate(DoVisibleChanged);
    { Manually adjust Validities, because FastTransformUpdate doesn't call DoGeometryChanged.

      TODO: If uncommenting Changed(false, [chTransform]) in TShape.FastTransformUpdateCore,
      it should call DoGeometryChanged, but it still doesn't? Why?
      In any case, it doesn't matter, it's faster to fix Validities manually below. }
    if DoVisibleChanged then
      Validities := Validities - [fvLocalBoundingBox];
  end else
  begin
    TraverseStack := nil;
    TransformChangeHelper := nil;
    try
      TraverseStack := TX3DGraphTraverseStateStack.Create;

      { initialize TransformChangeHelper, set before the loop properties
        that cannot change }
      TransformChangeHelper := TTransformChangeHelper.Create;
      TransformChangeHelper.ParentScene := Self;
      TransformChangeHelper.ChangingNode := RootNode;

      TransformShapesParentInfo.Group := Shapes as TShapeTreeGroup;
      TransformShapesParentInfo.Index := 0;

      { initialize TransformChangeHelper properties that may be changed
        during Node.Traverse later }
      TransformChangeHelper.Shapes := @TransformShapesParentInfo;
      TransformChangeHelper.AnythingChanged := false;
      TransformChangeHelper.Inside := false;
      TransformChangeHelper.Inactive := 0;

      RootNode.Traverse(TX3DNode,
        {$ifdef FPC}@{$endif}TransformChangeHelper.TransformChangeTraverse);

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
  end;

  if DoVisibleChanged then
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
end;

procedure TCastleSceneCore.InternalChangedField(const Field: TX3DField; const Change: TX3DChange);
var
  ANode: TX3DNode;

  procedure DoLogChanges(const Additional: string = '');
  var
    S: string;
  begin
    S := 'InternalChangedField: ' + X3DChangeToStr[Change] +
      Format(', node: %s (%s %s) at %s',
      [ ANode.X3DName, ANode.X3DType, ANode.ClassName, PointerToStr(ANode) ]);
    if Field <> nil then
      S := S + Format(', field %s (%s)', [ Field.X3DName, Field.X3DType ]);
    if Additional <> '' then
      S := S + '. ' + Additional;
    WritelnLog('X3D change', S);
  end;

  { Handle VRML >= 2.0 transformation changes (node with TTransformFunctionality). }
  procedure HandleChangeTransform;
  begin
    if OptimizeExtensiveTransformations then
    begin
      TransformationDirty := true
    end else
    begin
      Check(ANode.TransformFunctionality <> nil, 'chTransform flag may be set only for node with TTransformFunctionality');
      TransformationChanged(ANode.TransformFunctionality);
    end;

    if not ProcessEvents then
      { Otherwise FinishTransformationChanges would not be called in nearest Update,
        leaving transformation effects unapplied to the Shapes tree. }
      FinishTransformationChanges;
  end;

  procedure HandleChangeCoordinate;
  var
    C, I: Integer;
  begin
    { TCoordinateNode is special, although it's part of VRML 1.0 state,
      it can also occur within coordinate-based nodes of VRML >= 2.0.
      So it affects coordinate-based nodes with this node.

      In fact, code below takes into account both VRML 1.0 and 2.0 situation.
      That's why chCoordinate should not be used with chVisibleVRML1State
      --- chVisibleVRML1State handling is not needed after this. }

    { Note that both shape's OriginalGeometry, and actualy Geometry,
      have associated shapes.
      This is good, as a change to OriginalGeometry.Coord should also be handled,
      since it may cause FreeProxy for shape. This is necessary
      for animation of NURBS controlPoint to work. }

    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeNormalTangent;
  var
    C, I: Integer;
  begin
    { Similar to chCoordinate, this takes into account both VRML 1.0 and VRML 2.0/X3D.
      So performing chVisibleVRML1State after this is not necessary. }
    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
      VisibleChangeHere([vcVisibleNonGeometry]);
    end;
  end;

  { Good for both chVisibleVRML1State and chGeometryVRML1State
    (TShape.Changed actually cares about the difference between these two.) }
  procedure HandleVRML1State;
  var
    VRML1StateNode: TVRML1StateNode;
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    if ANode.VRML1StateNode(VRML1StateNode) then
    begin
      { ANode is part of VRML 1.0 state, so it affects Shapes where
        it's present on State.VRML1State list. }
      ShapeList := Shapes.TraverseList(false);
      for Shape in ShapeList do
        if (Shape.State.VRML1State.Nodes[VRML1StateNode] = ANode) or
           (Shape.OriginalState.VRML1State.Nodes[VRML1StateNode] = ANode) then
          Shape.Changed(false, [Change]);
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
    end;
  end;

  procedure HandleChangeLightInstanceProperty;
  var
    J: integer;
    ShapeList: TShapeList;
    Shape: TShape;
    LightInstance: PLightInstance;
    LightNode: TAbstractLightNode;
  begin
    // TODO: Optimize using TShapeTree.AssociatedShape

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

    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      if Shape.State.Lights <> nil then
        for J := 0 to Shape.State.Lights.Count - 1 do
        begin
          LightInstance := PLightInstance(Shape.State.Lights.Ptr(J));
          if LightInstance^.Node = LightNode then
          begin
            LightNode.UpdateLightInstance(LightInstance^);
            VisibleChangeHere([vcVisibleNonGeometry]);
          end;
        end;

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
      This way other scenes, using our lights by
      @link(TCastleScene.CastGlobalLights) feature,
      also have updated light location/direction.
      See https://sourceforge.net/p/castle-engine/discussion/general/thread/0bbaaf38/
      for a testcase. }
    for I := 0 to InternalGlobalLights.Count - 1 do
    begin
      L := PLightInstance(InternalGlobalLights.Ptr(I));
      if L^.Node = ANode then
        L^.Node.UpdateLightInstance(L^);
    end;

    { changing Location/Direction implies also changing "any light source property" }
    HandleChangeLightInstanceProperty;
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
      fvVerticesCount,
      fvTrianglesCount
    }

    Validities := Validities - [
      { Calculation traverses over active shapes. }
      fvShapesActiveCount,
      fvShapesActiveVisibleCount,
      { Calculation traverses over active nodes (uses RootNode.Traverse). }
      fvMainLightForShadows];

    DoGeometryChanged(gcActiveShapesChanged, nil);

    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);

    InternalIncShapesHash; // TShapeTreeSwitch returns now different things
  end;

  procedure HandleChangeColorNode;
  var
    C, I: Integer;
  begin
    { Affects all geometry nodes with "color" field referencing this node.

      Note: also TParticleSystemNode may have color in FdcolorRamp field.
      This is not detected for now, and doesn't matter (we do not handle
      particle systems at all now). }

    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
    end;
  end;

  procedure HandleChangeTextureCoordinate;
  var
    C, I: Integer;
  begin
    C := TShapeTree.AssociatedShapesCount(ANode);
    for I := 0 to C - 1 do
      TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
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
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    // TODO: Optimize and simplify to use TShapeTree.AssociatedShape

    { VRML 2.0 / X3D TextureTransform* affects only shapes where it's
      placed inside textureTransform field. }
    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      if (Shape.State.ShapeNode <> nil) and
         (Shape.State.ShapeNode.FdAppearance.Value <> nil) and
         (Shape.State.ShapeNode.FdAppearance.Value is TAppearanceNode) and
         AppearanceUsesTextureTransform(
           TAppearanceNode(Shape.State.ShapeNode.FdAppearance.Value), ANode) then
        Shape.Changed(false, [Change]);
  end;

  procedure HandleChangeGeometry;
  var
    C, I: Integer;
  begin
    C := TShapeTree.AssociatedShapesCount(ANode);
    for I := 0 to C - 1 do
      TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
  end;

  procedure HandleChangeEnvironmentalSensorBounds;
  var
    I: Integer;
    VSInstances: TVisibilitySensorInstanceList;
    VS: TVisibilitySensorNode;
    CameraVectors: TViewVectors;
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
  begin
    if ANode.TimeFunctionality = nil then Exit; {< ANode not time-dependent. }

    { Why do we want to call TimeFunctionality.SetTime now?

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

    ANode.TimeFunctionality.SetTime(Time, 0, false);

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
  procedure HandleChangeTextureImageOrRenderer(const ANode: TX3DNode; const Change: TX3DChange);
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    // TODO: Optimize using TShapeTree.AssociatedShape
    if Change = chTextureImage then
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

    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
    begin
      if Shape.UsesTexture(TAbstractTextureNode(ANode)) then
        Shape.Changed(false, [Change]);
    end;
  end;

  { React to change of TTexturePropertiesNode fields.

    Testcase that this is needed: create new TCastleImageTransform
    and change TCastleImageTransform.RepeatImage between (0.1, 0.1) and (10, 10),
    effectively changing the TextureProperties.BoundaryModeS/T under the hood between
    clamp and repeat. }
  procedure HandleChangeTextureProperties;
  var
    ParentField: TX3DField;
    TextureNode: TX3DNode;
    I: Integer;
  begin
    Assert(ANode is TTexturePropertiesNode, 'Only TTexturePropertiesNode should send chTexturePropertiesNode');
    for I := 0 to ANode.ParentFieldsCount - 1 do
    begin
      ParentField := ANode.ParentFields[I];
      if not (ParentField is TSFNode) then
      begin
        WritelnWarning('TTexturePropertiesNode change', 'ParentField is not TSFNode. This should not happen in normal usage of TTexturePropertiesNode, submit a bug');
        Continue;
      end;

      TextureNode := TSFNode(ParentField).ParentNode;
      if TextureNode = nil then
      begin
        WritelnWarning('TTexturePropertiesNode change', 'ParentField.Node is nil. This should not happen in usual usage of TCastleScene, submit a bug');
        Continue;
      end;

      { Make the same effect as when texture node's repeatS/T/R field changes }
      HandleChangeTextureImageOrRenderer(TextureNode, chTextureRendererProperties);
    end;
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
    GenTexFunctionality: TGeneratedTextureFunctionality;
  begin
    GenTexFunctionality := ANode.GenTexFunctionality;
    if GenTexFunctionality = nil then
      Exit;
    GenTexFunctionality.InternalUpdateNeeded := true;
    VisibleChangeHere([]);
  end;

  procedure HandleFontStyle;
  (* Naive version to seek all shapes. Left only for educational purposes.
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList({ OnlyActive } false);
    for Shape in ShapeList do
      if (Shape.OriginalGeometry is TTextNode) and
         (TTextNode(Shape.OriginalGeometry).FontStyle = ANode) then
        Shape.Changed(false, [Change]);
  end;
  *)
  var
    ParentField: TX3DField;
    TextNode: TX3DNode;
    C, I, J: Integer;
  begin
    { ANode is TFontStyleNode, child of (maybe many) TTextNodes.
      For each TTextNode, we want to call Changed on all TShape instances associated it.
      This accounts for various complicated X3D setups:
      TFontStyleNode may be referenced multiple times,
      TTextNode may be referenced multiple times. }

    Assert(ANode is TFontStyleNode, 'Only TFontStyleNode should send chFontStyle');
    for I := 0 to ANode.ParentFieldsCount - 1 do
    begin
      ParentField := ANode.ParentFields[I];
      if not (ParentField is TSFNode) then
      begin
        WritelnWarning('TFontStyleNode change', 'ParentField is not TSFNode. This should not happen in normal usage of TFontStyleNode, submit a bug');
        Continue;
      end;

      TextNode := TSFNode(ParentField).ParentNode;
      if TextNode = nil then
      begin
        WritelnWarning('TFontStyleNode change', 'ParentField.Node is nil. This should not happen in usual usage of TCastleScene, submit a bug');
        Continue;
      end;
      if not (TextNode is TTextNode) then
      begin
        WritelnWarning('TFontStyleNode change', 'ParentField.Node is not TTextNode. This should not happen in normal usage of TFontStyleNode, submit a bug');
        Continue;
      end;

      C := TShapeTree.AssociatedShapesCount(TextNode);
      for J := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(TextNode, J)).Changed(false, [Change]);
    end;
  end;

  procedure HandleChangeHeadLightOn;
  begin
    { Recalculate HeadlightOn based on NavigationInfo.headlight. }
    UpdateHeadlightOnFromNavigationInfo;
    VisibleChangeHere([]);
  end;

  procedure HandleChangeClipPlane;
  var
    C, I: Integer;
  begin
    Assert(ANode is TClipPlaneNode);

    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
      VisibleChangeHere([vcVisibleGeometry]);
    end;
  end;

  procedure HandleChangeEverything(const OnlyAdditions: Boolean = false);
  begin
    { An arbitrary change occurred. }
    ScheduleChangedAll(OnlyAdditions);
  end;

  { Handle Change in [chVisibleGeometry, chVisibleNonGeometry, chRedisplay] }
  procedure HandleVisibleChange;
  var
    VisibleChanges: TVisibleChanges;
  begin
    VisibleChanges := [];
    if Change = chVisibleGeometry    then Include(VisibleChanges, vcVisibleGeometry);
    if Change = chVisibleNonGeometry then Include(VisibleChanges, vcVisibleNonGeometry);
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
  begin
    (ANode as TScreenEffectNode).InternalRendererResourceFree;
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeBackground;
  begin
    InternalInvalidateBackgroundRenderer;
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
    C, I: Integer;
  begin
    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);
    end;
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;

  procedure HandleChangeChildren(const OnlyAdditions: Boolean);
  begin
    HandleChangeEverything(OnlyAdditions);
  end;

  procedure HandleChangeBBox;
  var
    C, I: Integer;
  begin
    C := TShapeTree.AssociatedShapesCount(ANode);
    if C <> 0 then
    begin
      for I := 0 to C - 1 do
        TShape(TShapeTree.AssociatedShape(ANode, I)).Changed(false, [Change]);

      { Bounding box of the scene changed, and rendering octree changed,
        because bbox of shape changed.
        Testcase: knight.gltf (from examples/fps_game/ ) animations or
        lizardman.gltf (from demo-models/bump_mapping/ ) animations.
        We deliberately pass Shape=nil, to cause MaybeBoundingBoxChanged=true
        inside DoGeometryChanged. }
      DoGeometryChanged(gcLocalGeometryChanged, nil);
    end;
  end;

begin
  ANode := TX3DNode(Field.ParentNode);
  Assert(ANode <> nil);

  { We used to check here RootNode.IsNodePresent(ANode), to eliminate
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

  if LogChanges then
    DoLogChanges;

  { Optimize Change = chNone case: no need even for Begin/EndChangesSchedule }
  if Change = chNone then
    Exit;

  BeginChangesSchedule;
  try
    case Change of
      chTransform: HandleChangeTransform;
      chCoordinate: HandleChangeCoordinate;
      chNormal, chTangent: HandleChangeNormalTangent;
      chVisibleVRML1State, chGeometryVRML1State: HandleVRML1State;
      chLightInstanceProperty: HandleChangeLightInstanceProperty;
      chLightForShadowVolumes: HandleChangeLightForShadowVolumes;
      chLightLocationDirection: HandleChangeLightLocationDirection;
      chSwitch2: HandleChangeSwitch2;
      chColorNode: HandleChangeColorNode;
      chTextureCoordinate: HandleChangeTextureCoordinate;
      chTextureTransform: HandleChangeTextureTransform;
      chGeometry, chGeometryFontChanged: HandleChangeGeometry;
      chEnvironmentalSensorBounds: HandleChangeEnvironmentalSensorBounds;
      chTimeStopStart: HandleChangeTimeStopStart;
      chViewpointVectors: HandleChangeViewpointVectors;
      // TODO:  chViewpointProjection: HandleChangeViewpointProjection
      chTextureImage, chTextureRendererProperties: HandleChangeTextureImageOrRenderer(ANode, Change);
      chTexturePropertiesNode: HandleChangeTextureProperties;
      chShadowCasters: HandleChangeShadowCasters;
      chGeneratedTextureUpdateNeeded: HandleChangeGeneratedTextureUpdateNeeded;
      { The HandleFontStyle implementation matches
        both chFontStyle, chFontStyleFontChanged.
        Onlt the TShape.Changed processing handles
        chFontStyle, chFontStyleFontChanged differently. }
      chFontStyle, chFontStyleFontChanged: HandleFontStyle;
      chHeadLightOn: HandleChangeHeadLightOn;
      chClipPlane: HandleChangeClipPlane;
      chDragSensorEnabled: HandleChangeDragSensorEnabled;
      chNavigationInfo: HandleChangeNavigationInfo;
      chScreenEffectEnabled: HandleChangeScreenEffectEnabled;
      chBackground: HandleChangeBackground;
      chEverything: HandleChangeEverything;
      chShadowMaps: HandleChangeShadowMaps;
      chWireframe: HandleChangeWireframe;
      chGroupChildren, chGroupChildrenAdd: HandleChangeChildren(Change = chGroupChildrenAdd);
      chBBox: HandleChangeBBox;
      chVisibleGeometry, chVisibleNonGeometry, chRedisplay: HandleVisibleChange;
      else ;
    end;
  finally EndChangesSchedule end;
end;

procedure TCastleSceneCore.DoGeometryChanged(const Change: TGeometryChange;
  LocalGeometryShape: TShape);
var
  MaybeBoundingBoxChanged: boolean;
const
  { Whether LocalGeometryChanged was called, which means that octree and/or
    bounding box/sphere of some shape changed. }
  SomeLocalGeometryChanged = [gcAll, gcLocalGeometryChanged, gcLocalGeometryChangedCoord];
begin
  MaybeBoundingBoxChanged := not (
    { Box stayed the same if we changed shape geometry,
      but TShapeNode provides explicit bbox information,
      so this shape has still the same bbox.
      This is the case with glTF skinned animation. }
    (Change in [gcLocalGeometryChanged, gcLocalGeometryChangedCoord]) and
    (LocalGeometryShape <> nil) and
    (LocalGeometryShape.Node <> nil) and
    (not LocalGeometryShape.Node.BBox.IsEmpty)
  );

  Validities := Validities - [
    fvVerticesCount,
    fvTrianglesCount
  ];

  if MaybeBoundingBoxChanged then
  begin
    Validities := Validities - [fvLocalBoundingBox];

    if (FOctreeRendering <> nil) and
       (Change in [gcVisibleTransformChanged, gcActiveShapesChanged] + SomeLocalGeometryChanged) then
      FreeAndNil(FOctreeRendering);

    if (FOctreeDynamicCollisions <> nil) and
       (Change in [gcCollidableTransformChanged, gcActiveShapesChanged] + SomeLocalGeometryChanged) then
    begin
      FreeAndNil(FOctreeDynamicCollisions);
      // PointingDeviceClear; // do not free PTriangle records, the per-shape octrees remain valid. Testcase: Unholy clicking
    end;
  end;

  if FOctreeStaticCollisions <> nil then
  begin
    WritelnWarning('ssStaticCollisions used on scene "' + Name + '" but the geometry changed. Freeing the spatial structure. You should use ssDynamicCollisions for this scene');
    FreeAndNil(FOctreeStaticCollisions);
    PointingDeviceClear; // remove any reference to (no longer valid) PTriangle records
  end;

  if Assigned(OnGeometryChanged) then
    OnGeometryChanged(Self, Change in SomeLocalGeometryChanged,
      { We know LocalGeometryShape is nil now if Change does not contain
        gcLocalGeometryChanged*. }
      LocalGeometryShape);
end;

procedure TCastleSceneCore.InternalMoveShapeAssociations(const OldNode, NewNode: TX3DNode; const ContainingShapes: TObject);
var
  S: TShapeTree;
  L: TShapeTreeList;
  I: Integer;
begin
  if (OldNode <> NewNode) and (ContainingShapes <> nil) then
  begin
    { For shapes on ContainingShapes, they should be removed from OldNode,
      and added to NewNode.

      Otherwise we have no chance to clean them from OldNode, as change notification
      reaches TCastleSceneCore / TShapeTree after the old value changed into new. }

    if ContainingShapes.ClassType = TShapeTreeList then
    begin
      // ContainingShapes is a list
      L := TShapeTreeList(ContainingShapes);

      { Using downto just for future safety,
        in case S.UnAssociateNode would remove from L -- but it is not the case now,
        L is from containing node InternalSceneShape (e.g. from Appearance containing this Material,
        or Shape containing this Appearance), and its contents don't change during this loop. }
      for I := L.Count - 1 downto 0 do
      begin
        S := L[I];
        if OldNode <> nil then
          S.UnAssociateNode(OldNode);
        if NewNode <> nil then
          S.AssociateNode(NewNode);
      end;
    end else
    begin
      // ContainingShapes is a single TShapeTree
      S := TShapeTree(ContainingShapes);
      if OldNode <> nil then
        S.UnAssociateNode(OldNode);
      if NewNode <> nil then
        S.AssociateNode(NewNode);
    end;
  end;
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

function TCastleSceneCore.InfoTriangleVerticesCounts: string;
begin
  Result := Format('Scene contains %d triangles and %d vertices.',
    [TrianglesCount, VerticesCount]) + NL;
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
  ShapeList: TShapeList;
  Shape: TShape;
begin
  ManifoldEdges := 0;
  BorderEdges := 0;
  ShapeList := Shapes.TraverseList(true);
  for Shape in ShapeList do
  begin
    ManifoldEdges := ManifoldEdges + Shape.InternalShadowVolumes.ManifoldEdges.Count;
    BorderEdges := BorderEdges + Shape.InternalShadowVolumes.BorderEdges.Count;
  end;
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

function TCastleSceneCore.TriangleOctreeLimits: POctreeLimits;
begin
  Result := @FTriangleOctreeLimits;
end;

function TCastleSceneCore.ShapeOctreeLimits: POctreeLimits;
begin
  Result := @FShapeOctreeLimits;
end;

procedure TCastleSceneCore.SetSpatial(const Value: TSceneSpatialStructures);

  procedure SetShapeSpatial(const Value: TShapeSpatialStructures;
    OnlyCollidable: boolean);
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      { When Value <> [], we honor OnlyCollidable. }
      if (Value = []) or
         (not OnlyCollidable) or
         (Shape.Collidable) then
      begin
        { Note: do not change here
            Shape.TriangleOctreeLimits :=
          Our own TriangleOctreeLimits properties may be *not* suitable
          for this (as our properties are for global octrees).
          Just let programmer change per-shape properties if (s)he wants. }

        Shape.InternalSpatial := Value;
      end;
  end;

var
  Old, New: Boolean;
  TimeStart: TCastleProfilerTime;
begin
  if Value <> FSpatial then
  begin
    if ( (Value <> []) and
         (Value <> [ssRendering, ssDynamicCollisions]) and
         (Value <> [ssDynamicCollisions])
       ) then
      WritelnWarning('%s: Spatial values different than [], [ssRendering,ssDynamicCollisions], [ssDynamicCollisions] may not be allowed in future engine versions. We advise to use TCastleScene.PreciseCollisions instead of TCastleScene.Spatial.', [
        Name
      ]);

    { Handle OctreeRendering }

    Old := ssRendering in FSpatial;
    New := ssRendering in Value;

    if Old and not New then
      FreeAndNil(FOctreeRendering);

    { Handle OctreeDynamicCollisions and Shapes[I].Spatial }

    Old := ssDynamicCollisions in FSpatial;
    New := ssDynamicCollisions in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeDynamicCollisions);
      // PointingDeviceClear; // do not free PTriangle records, the per-shape octrees remain valid. Testcase: Unholy clicking
      SetShapeSpatial([], true);
    end else
    if New and not Old then
    begin
      TimeStart := Profiler.Start('Creating octrees for all shapes from Scene.Spatial := [...]');
      try
        { SetShapeSpatial cannot be done by the way of doing CreateShapeOctree,
          since in CreateShapeOctree we iterate over OnlyActive shapes,
          but SetShapeSpatial must iterate over all shapes. }
        SetShapeSpatial([ssTriangles], true);
      finally
        Profiler.Stop(TimeStart, true, true);
      end;
    end;

    { Handle OctreeVisibleTriangles }

    Old := ssVisibleTriangles in FSpatial;
    New := ssVisibleTriangles in Value;

    if Old and not New then
      FreeAndNil(FOctreeVisibleTriangles);

    { Handle OctreeStaticCollisions }

    Old := ssStaticCollisions in FSpatial;
    New := ssStaticCollisions in Value;

    if Old and not New then
    begin
      FreeAndNil(FOctreeStaticCollisions);
      PointingDeviceClear; // remove any reference to (no longer valid) PTriangle records
    end;

    FSpatial := Value;
  end;
end;

function TCastleSceneCore.GetPreciseCollisions: Boolean;
begin
  {$warnings off} // this uses deprecated Spatial, which should be Internal at some point
  Result := Spatial <> [];
  {$warnings on}
end;

procedure TCastleSceneCore.SetPreciseCollisions(const Value: Boolean);
begin
  {$warnings off} // this uses deprecated Spatial, which should be Internal at some point
  if Value then
    Spatial := [ssRendering, ssDynamicCollisions]
  else
    Spatial := [];
  {$warnings on}
end;

function TCastleSceneCore.InternalOctreeRendering: TShapeOctree;
begin
  if (ssRendering in FSpatial) and (FOctreeRendering = nil) then
  begin
    FOctreeRendering := CreateShapeOctree(
      FShapeOctreeLimits,
      false);
    if LogChanges then
      WritelnLog('X3D changes (octree)', 'OctreeRendering updated');
  end;

  Result := FOctreeRendering;
end;

function TCastleSceneCore.InternalOctreeDynamicCollisions: TShapeOctree;
begin
  if (ssDynamicCollisions in FSpatial) and (FOctreeDynamicCollisions = nil) then
  begin
    FOctreeDynamicCollisions := CreateShapeOctree(
      FShapeOctreeLimits,
      true);
    if LogChanges then
      WritelnLog('X3D changes (octree)', 'OctreeDynamicCollisions updated');
  end;

  Result := FOctreeDynamicCollisions;
end;

function TCastleSceneCore.InternalOctreeVisibleTriangles: TTriangleOctree;
begin
  if (ssVisibleTriangles in FSpatial) and (FOctreeVisibleTriangles = nil) then
    FOctreeVisibleTriangles := CreateTriangleOctree(
      FTriangleOctreeLimits,
      false);
  Result := FOctreeVisibleTriangles;
end;

function TCastleSceneCore.InternalOctreeStaticCollisions: TTriangleOctree;
begin
  if (ssStaticCollisions in FSpatial) and (FOctreeStaticCollisions = nil) then
    FOctreeStaticCollisions := CreateTriangleOctree(
      FTriangleOctreeLimits,
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
  Result := FSpatial * [ssStaticCollisions, ssDynamicCollisions] <> [];
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
  const Collidable: boolean): TTriangleOctree;

  procedure FillOctree(TriangleEvent: TTriangleEvent);
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(true);
    for Shape in ShapeList do
      if (Collidable and Shape.Collidable) or
         ((not Collidable) and Shape.Visible) then
        Shape.Triangulate(TriangleEvent);
  end;

begin
  Inc(InternalDirty);
  try

  Result := TTriangleOctree.Create(Limits, LocalBoundingBoxNoChildren);
  try
    Result.Triangles.Capacity := TrianglesCount;
    FillOctree({$ifdef FPC} @ {$endif} Result.AddItemTriangle);
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
  const Collidable: boolean): TShapeOctree;
var
  I: Integer;
  ShapesList: TShapeList;
begin
  Inc(InternalDirty);
  try

  if Collidable then
    { Add only active and collidable shapes }
    ShapesList := Shapes.TraverseList(true, false, true) else
    { Add only active and visible shapes }
    ShapesList := Shapes.TraverseList(true, true, false);

  Result := TShapeOctree.Create(Limits, LocalBoundingBoxNoChildren, ShapesList, false);
  try
    for I := 0 to Result.ShapesList.Count - 1 do
      if not Result.ShapesList[I].BoundingBox.IsEmpty then
        Result.TreeRoot.AddItem(I);
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
         (V.Description = ViewpointDescription) ) then
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
        RootNode.Traverse(TAbstractViewpointNode,
        {$ifdef FPC}@{$endif}Seeker.Seek));
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
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      Shape.InternalShadowVolumes.FreeResources;
  end;

begin
  if (frTextureDataInNodes in Resources) and (RootNode <> nil) then
  begin
    RootNode.EnumerateNodes(TAbstractTexture2DNode,
      {$ifdef FPC}@{$endif}FreeResources_UnloadTextureData, false);
    RootNode.EnumerateNodes(TAbstractTexture3DNode,
      {$ifdef FPC}@{$endif}FreeResources_UnloadTexture3DData, false);
  end;

  if frShadowVolume in Resources then
    FreeShadowVolumes;
end;

{ events --------------------------------------------------------------------- }

procedure TCastleSceneCore.ScriptsInitializeCallback(Node: TX3DNode);
begin
  // TODO: Delphi Support
  {$ifdef FPC}
  // Node.Scene must be set in order for TScriptNode.SetInitialized to work
  Assert(Node.Scene = Self);
  TScriptNode(Node).Initialized := true;
  {$endif}
end;

procedure TCastleSceneCore.ScriptsInitialize;
begin
  // TODO: Delphi Support
  {$ifdef FPC}
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
  {$endif}
end;

procedure TCastleSceneCore.ScriptsFinalizeCallback(Node: TX3DNode);
begin
  // TODO: Delphi Support
  {$ifdef FPC}
  TScriptNode(Node).Initialized := false;
  {$endif}
end;

procedure TCastleSceneCore.ScriptsFinalize;
begin
  // TODO: Delphi Support
  {$ifdef FPC}
  if RootNode <> nil then
  begin
    BeginChangesSchedule;
    try
      { We have to deinitialize scripts before any other deinitialization
        is done. Just like for ScriptsInitialize. }
      RootNode.EnumerateNodes(TScriptNode, @ScriptsFinalizeCallback, false);
    finally EndChangesSchedule end;
  end;
  {$endif}
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
      InternalCameraChanged;
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
        RootNode.UnregisterScene;
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
  if Result or (not Exists) or (Event.EventType <> itKey) then Exit;

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
      to be sure that the event is handled (and should not be passed to others).
    Result := false; }
  end;
end;

function TCastleSceneCore.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or (not Exists) or (Event.EventType <> itKey) then Exit;

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
      to be sure that the event is handled (and should not be passed to others).
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
  if Result or (not Exists) then Exit;

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
            PointingDeviceMove continuously, you can only check this on
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
                  we can get this by InverseTransform. }
                OverItem^.State.Transformation.InverseTransform.MultPoint(Pick.Point), NextEventTime);

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

function TCastleSceneCore.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  { If the Scene was just added to Viewport (and no mouse move occurred),
    or if the Scene was modified by ChangedAll, e.g. by removing some items
    (and no mouse move occurred),
    then PointingDevicePress would not fire for sensors (like TouchSensor)
    under mouse, because PointingDeviceOverItem is not set.
    See https://github.com/castle-engine/castle-engine/issues/227 .
    Using PointingDeviceMove sets PointingDeviceOverItem. }
  PointingDeviceMove(Pick, Distance);

  Result := PointingDevicePressRelease(true, Distance, { ignored } false);
end;

function TCastleSceneCore.PointingDeviceRelease(const Pick: TRayCollisionNode;
  const Distance: Single; const CancelAction: Boolean): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  Result := PointingDevicePressRelease(false, Distance, CancelAction);
end;

function TCastleSceneCore.PointingDevicePressRelease(const DoPress: boolean;
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
        Load(NewRootNode, true, [slDisableResetTime]);

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
      Sensor.Activate(NextEventTime,
        Sensors.Transformation.Transform,
        Sensors.Transformation.InverseTransform,
        PointingDeviceOverPoint);
    end;
  end;

var
  I: Integer;
  ToActivate: TX3DNode;
  ActiveChanged: boolean;
  ActiveSensor: TAbstractPointingDeviceSensorNode;
begin
  Result := false;

  if ProcessEvents and (FPointingDeviceActive <> DoPress) then
  begin
    BeginChangesSchedule;
    try
      ActiveChanged := false;
      FPointingDeviceActive := DoPress;
      if DoPress then
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
              TTouchSensorNode(ActiveSensor).EventTouchTime.Send(Time, NextEventTime);
            end;
          end;
          FPointingDeviceActiveSensors.Count := 0;
          ActiveChanged := true;
        end;
      end;

      if ActiveChanged then DoPointingDeviceSensorsChange;

      { We try hard to leave Result as false when nothing happened.
        This is important for TCastleViewport, that wants to retry
        press around if ApproximateActivation, and for other TCastleTransform objects
        along the same TRayCollision list. So we really must set
        Result := false if nothing happened, to enable other objects
        to have a better chance of catching press/release event.
        At the same time, we really must set Result := true if something
        (possibly) happened. Otherwise, simultaneously two objects may be activated,
        and TCastleViewport.PointingDevicePressFailed may do a sound
        warning that activation was unsuccessful.

        Fortunately, our ActiveChanged right now precisely tells us
        when something happened. When not ActiveChanged, nothing happened,
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

(*
// Not needed now
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
*)

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

procedure TCastleSceneCore.UpdateNewPlayingAnimation(out NeedsUpdateTimeDependent: Boolean);
begin
  NeedsUpdateTimeDependent := false;

  if NewPlayingAnimationUse then
  begin
    NewPlayingAnimationUse := false;
    if PlayingAnimationNode <> nil then
    begin
      PlayingAnimationNode.Stop;
      if Assigned(PlayingAnimationStopNotification) then
      begin
        PlayingAnimationStopNotification(Self, PlayingAnimationNode);
        PlayingAnimationStopNotification := nil;
      end;
      PlayingAnimationNode.EventIsActive.RemoveNotification(
        {$ifdef FPC}@{$endif}PlayingAnimationIsActive);
    end;
    Assert({$ifndef FPC}@{$endif}PlayingAnimationStopNotification = nil);

    { If calling PlayAnimation on already-playing node,
      we have to make sure it actually starts playing from the start.
      For the StartTime below to be correctly applied, we have to make
      sure the node actually *stops* temporarily (otherwise the
      TTimeDependentFunctionality.SetTime never "sees" the node
      in the Enabled = false state). }
    if (PlayingAnimationNode = NewPlayingAnimationNode) and
       (PlayingAnimationNode <> nil) then
      PlayingAnimationNode.TimeFunctionality.SetTime(Time, 0, false);

    { set PreviousPlayingAnimationXxx }
    PreviousPlayingAnimation := PlayingAnimationNode;
    if PreviousPlayingAnimation <> nil then
    begin
      PreviousPlayingAnimationLoop := PreviousPlayingAnimation.Loop;
      PreviousPlayingAnimationForward := PreviousPlayingAnimation.FractionIncreasing;
      PreviousPlayingAnimationTimeInAnimation := PreviousPlayingAnimation.TimeFunctionality.ElapsedTime;
    end;

    { If current animation node changes (to non-nil) call ResetAnimatedFields,
      to restore fields *not* affected by new animation to their "reset" state.

      Note: This must be called before we pass time to a new TimeSensor
      (since otherwise, ResetAnimationState would override some state set by new TimeSensor)
      and after the old TimeSensor is stopped.
      In particular, this must be called before PlayingAnimationNode.Start done lower
      (after PlayingAnimationNode:=NewPlayingAnimationNode) since TTimeSensorNode.Start
      sets StartTime and this calls HandleChangeTimeStopStart which applies
      the new TimeSensor.

      How does this work with blending (cross-fading) animations (using TransitionDuration)?

      For each field X, there are 4 cases possible:

      1. Neither old nor new animation affects the field X.

        Then everything trivially works.
        Old animation doesn't change X,
        ResetAnimationState doesn't change X
        (maybe even it doesn't touch X, if X is not affected by *any* animation),
        new animation doesn't change X.

      2. Old animation doesn't affect field X, new animation affects field X.

        Then ResetAnimationState on X doesn't do anything
        (since X already has the same value as ResetAnimationState sets).

        In TX3DField.InternalAffectedPartial
        we will blend (fade-in) new animation with FResetValue recorded
        in the field.

      3. Old animation affects field X, new animation doesn't affect X.

        We will blend (fade-out) old animation value with the FResetValue
        in the field.

      4. Both old and new animations touch field X.

        In this case field's FResetValue value is not useful
        (we don't want to it use since neither old nor new animation
        corresponds to the "reset" state).

        Luckily, in this case FResetValue will be ignored!
        The field X will receive two values (during cross-fade):
        old one (let's call it's strength Alpha), and new one (with strength 1-Alpha).
        Since their strengths sum to 1.0, in TX3DField.InternalAffectedPartial
        we will have PartialReceived.Partial = 1.0,
        which means that FResetValue will be ignored.

      To test it all in a simple case,
      open the Spine JSON file
      from https://github.com/castle-engine/demo-models/tree/master/animation/spine_animation_blending_test/exported
      with view3dscene and run animations with TransitionDuration > 0.

      Note that above assumes that the field X supports lerp (TX3DField.CanAssignLerp).
      Otherwise the AD 3 case is broken (new animation would not correctly "reset"
      the field X, value of old animation would override it if TransitionDuration > 0).
      Testcase: TransitionDuration > 0 with Roseanne, switch between
      die and attack_monster.
      To fix this, we use IgnoreIfCannotLerp.
    }

    if (PlayingAnimationNode <> NewPlayingAnimationNode) and
       (NewPlayingAnimationNode <> nil) then
    begin
      ResetAnimationState(NewPlayingAnimationNode);
      { As soon as possible, we need to process TimeSensors, otherwise user would
        see a default animation pose. }
      NeedsUpdateTimeDependent := true;
    end;

    PlayingAnimationNode := NewPlayingAnimationNode;

    if PlayingAnimationNode <> nil then
    begin
      { Assign these before PartialSendBegin/End }
      PlayingAnimationTransitionDuration := NewPlayingAnimationTransitionDuration;
      { save current Time to own variable,
        do not trust PlayingAnimationNode.StartTime to remain reliable,
        it can be changed by user later. }
      PlayingAnimationStartTime := Time;

      PlayingAnimationNode.Start(
        NewPlayingAnimationLoop,
        NewPlayingAnimationForward,
        NewPlayingAnimationInitialTime);

      PlayingAnimationNode.EventIsActive.AddNotification(
        {$ifdef FPC}@{$endif}PlayingAnimationIsActive);
      PlayingAnimationStopNotification := NewPlayingAnimationStopNotification;
    end;
  end;
end;

procedure TCastleSceneCore.UpdateTimeDependentList(
  const TimeIncrease: TFloatTime; const ResetTime: boolean);
var
  SomethingVisibleChanged, SomePartialSend: boolean;
  T: TFloatTime;
  TimeFunctionality: TTimeDependentFunctionality;
begin
  SomethingVisibleChanged := false;
  T := Time;
  SomePartialSend := false;

  for TimeFunctionality in TimeDependentList do
  begin
    PartialSendBegin(TimeFunctionality);
    if TimeFunctionality.PartialSend <> nil then
      SomePartialSend := true;

    if TimeFunctionality.SetTime(T, TimeIncrease, ResetTime) and
      (TimeFunctionality.Parent is TMovieTextureNode) then
      SomethingVisibleChanged := true;

    PartialSendEnd(TimeFunctionality);
  end;

  if not SomePartialSend then
    NoPartialSend;

  { If SomethingVisibleChanged (in MovieTexture nodes), we have to redisplay. }
  if SomethingVisibleChanged then
    VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
end;

procedure TCastleSceneCore.InternalSetTime(
  const NewValue: TFloatTime; const TimeIncrease: TFloatTime; const ResetTime: boolean);

  { Call UpdateTimeDependentList, but only if AnimateOnlyWhenVisible logic agrees.
    When ResetTime, we force always an UpdateTimeDependentList(0.0) call.
    @param NeedsUpdate Forces always *some* UpdateTimeDependentList call. }
  procedure UpdateTimeDependentListIfVisible(const NeedsUpdate: Boolean);
  begin
    if ResetTime then
    begin
      { Call UpdateTimeDependentList and reset FAnimateGatheredTime.
        However, do not reset AnimateSkipNextTicks (this would synchronize
        all skipping ticks if multiple animations are reset at the beginning
        of game, making randomization in SetAnimateSkipTicks pointless). }
      FAnimateGatheredTime := 0;
      UpdateTimeDependentList(TimeIncrease + FAnimateGatheredTime, ResetTime);
    end else
    if NeedsUpdate then
    begin
      { Call UpdateTimeDependentList regardless of visibility and
        of AnimateSkipNextTicks. Note that we do not reset AnimateSkipNextTicks
        (to avoid synchronizing AnimateSkipNextTicks of multiple animations
        started in the sam frame). }
      UpdateTimeDependentList(TimeIncrease + FAnimateGatheredTime, ResetTime);
      FAnimateGatheredTime := 0;
    end else
    if FAnimateOnlyWhenVisible and (not IsVisibleNow) then
    begin
      FAnimateGatheredTime := FAnimateGatheredTime + TimeIncrease;
    end else
    if AnimateSkipNextTicks <> 0 then
    begin
      Dec(AnimateSkipNextTicks);
      FAnimateGatheredTime := FAnimateGatheredTime + TimeIncrease;
    end else
    begin
      UpdateTimeDependentList(TimeIncrease + FAnimateGatheredTime, ResetTime);
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

var
  NeedsUpdateTimeDependent: Boolean;
begin
  FTimeNow.Seconds := NewValue;
  FTimeNow.PlusTicks := 0; // using InternalSetTime always resets PlusTicks

  if ProcessEvents and InternalEnableAnimation then
  begin
    BeginChangesSchedule;
    try
      { UpdateNewPlayingAnimation must be called before UpdateTimeDependentListIfVisible.
        This way if we called StopAnimation, then UpdateNewPlayingAnimation
        first stops it (preventing from sending any events),
        so a stopped animation will *not* send any events after StopAnimation
        (making it useful to call ResetAnimationState right after StopAnimation call). }
      UpdateNewPlayingAnimation(NeedsUpdateTimeDependent);
      UpdateTimeDependentListIfVisible(NeedsUpdateTimeDependent);
      UpdateHumanoidSkin;
      { Process TransformationDirty at the end of increasing time, to apply scheduled
        TransformationDirty in the same Update, as soon as possible
        (useful e.g. for mana shot animation in dragon_squash). }
      FinishTransformationChanges;
    finally
      EndChangesSchedule;
    end;
  end;
end;

procedure TCastleSceneCore.FinishTransformationChanges;
var
  E: TExposedTransform;
begin
  if TransformationDirty then
  begin
    RootTransformationChanged;
    TransformationDirty := false;
  end;

  for E in FExposedTransforms do
    E.Synchronize;
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
  // TODO: Delphi Support
  {$ifdef FPC}
  if Node is TAbstractScriptNode then
    TAbstractScriptNode(Node).ResetLastEventTimes;
  {$endif}
end;

procedure TCastleSceneCore.ResetTime(const NewValue: TFloatTime);
begin
  if RootNode <> nil then
    RootNode.EnumerateNodes({$ifdef FPC}@{$endif}ResetLastEventTime,
     false);
  InternalSetTime(NewValue, 0, true);
end;

procedure TCastleSceneCore.ResetTimeAtLoad;
begin
  if (NavigationInfoStack.Top <> nil) and
     NavigationInfoStack.Top.TimeOriginAtLoad then
    FTimeAtLoad := 0.0
  else
    FTimeAtLoad := DateTimeToUnix(CastleNow);
  ResetTime(TimeAtLoad);
end;

procedure TCastleSceneCore.IncreaseTimeTick;
begin
  Inc(FTimeNow.PlusTicks);
end;

procedure TCastleSceneCore.InternalCameraChanged;

  { Does CameraProcessing needs to be called (does it actually do anything). }
  function CameraProcessingNeeded: boolean;
  begin
    Result :=
      (ShapeLODs.Count <> 0) or
      (ProximitySensors.Count <> 0) or
      (BillboardNodes.Count <> 0);
  end;

  { Update things depending on camera information and X3D events.
    Call it only when ProcessEvents. }
  procedure CameraProcessing(const CameraVectors: TViewVectors);
  var
    I: Integer;
  begin
    Assert(ProcessEvents);

    for I := 0 to ShapeLODs.Count - 1 do
      UpdateLODLevel(TShapeTreeLOD(ShapeLODs.Items[I]), CameraVectors.Translation);

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
    for I := 0 to BillboardNodes.Count - 1 do
    begin
      (BillboardNodes[I] as TBillboardNode).InternalCameraChanged(CameraVectors);
      { Apply transformation change to Shapes tree.
        Note that we should never call TransformationChanged when
        OptimizeExtensiveTransformations. }
      if OptimizeExtensiveTransformations then
        TransformationDirty := true
      else
        TransformationChanged(BillboardNodes[I].TransformFunctionality);
    end;
  end;

var
  CameraVectors: TViewVectors;
begin
  if World <> nil then // may be called from SetProcessEvents when World may be nil
    LastCameraStateId := World.InternalMainCameraStateId;

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

  { handle WatchForTransitionComplete, looking at ACamera.Animation }
  (* TODO: we don't camera TCamera reference now,
    - we do not generate EventTransitionComplete now,
    - we never update WatchForTransitionComplete to false.
    This is not a critical feature, for X3D authors or Pascal developers.

  if ProcessEvents and WatchForTransitionComplete and not ACamera.Animation then
  begin
    BeginChangesSchedule;
    try
      WatchForTransitionComplete := false;
      if NavigationInfoStack.Top <> nil then
        NavigationInfoStack.Top.EventTransitionComplete.Send(true, NextEventTime);
    finally EndChangesSchedule end;
  end;
  *)
end;

procedure TCastleSceneCore.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  SP: Single;
begin
  inherited;
  if not Exists then Exit;

  { in case the same scene is present many times on Viewport.Items list,
    do not process it's Update() many times (would cause time to move too fast). }
  if LastUpdateFrameId = TFramesPerSecond.FrameId then Exit;
  LastUpdateFrameId := TFramesPerSecond.FrameId;

  FrameProfiler.Start(fmUpdateScene);

  if (World <> nil) and
     (LastCameraStateId <> World.InternalMainCameraStateId) then
    InternalCameraChanged; // sets LastCameraStateId

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

  FrameProfiler.Stop(fmUpdateScene);
end;

procedure TCastleSceneCore.PartialSendBegin(const TimeFunctionality: TTimeDependentFunctionality);
var
  T: TFloatTime;
  PartialSend: TPartialSend;
begin
  T := Time;

  if (PlayingAnimationTransitionDuration <> 0) { often early exit } and
     (TimeFunctionality.Parent = PlayingAnimationNode) and
     (T >= PlayingAnimationStartTime) { only sanity check } and
     (T < PlayingAnimationStartTime + PlayingAnimationTransitionDuration) then
  begin
    PartialSend := TPartialSend.Create;
    TimeFunctionality.PartialSend := PartialSend;

    if PreviousPlayingAnimation <> nil then
    begin
      PartialSend.Partial := 1 - (T - PlayingAnimationStartTime) /
        PlayingAnimationTransitionDuration;
      PartialSend.IgnoreIfCannotLerp := true;

      { First fade-out previous animation.
        Note that
        - PreviousPlayingAnimation may be equal to PlayingAnimationNode
          (in case when PlayAnimation starts the same animation from beginning)
          and we still must work correctly.
        - We do not use FadingOutAnimation.TimeFunctionality.SetTime,
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
    PartialSend.IgnoreIfCannotLerp := false;
  end else
    TimeFunctionality.PartialSend := nil;
end;

procedure TCastleSceneCore.PartialSendEnd(const TimeFunctionality: TTimeDependentFunctionality);

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
  if TimeFunctionality.PartialSend <> nil then
  begin
    AffectedFields := TimeFunctionality.PartialSend.AffectedFields;
    TimeFunctionality.PartialSend.AffectedFields := nil; // do not free by TPartialSend.Destroy
    FreeAndNil(TimeFunctionality.PartialSend);

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

  ChangedAllScheduledOnlyAdditions := true; // may be changed to false by ScheduleChangedAll

  Inc(ChangedAllSchedule);
end;

procedure TCastleSceneCore.ScheduleChangedAll(const OnlyAdditions: Boolean);
begin
  if ChangedAllSchedule = 0 then
    ChangedAll
  else
  begin
    ChangedAllScheduled := true;
    ChangedAllScheduledOnlyAdditions := ChangedAllScheduledOnlyAdditions and OnlyAdditions;
  end;
end;

procedure TCastleSceneCore.EndChangesSchedule;
begin
  Dec(ChangedAllSchedule);
  if (ChangedAllSchedule = 0) and ChangedAllScheduled then
  begin
    { Note that ChangedAll will set ChangedAllScheduled to false. }
    ChangedAll(ChangedAllScheduledOnlyAdditions);
  end;
end;

{ proximity sensor ----------------------------------------------------------- }

procedure TCastleSceneCore.ProximitySensorUpdate(const PSI: TProximitySensorInstance;
  const CameraVectors: TViewVectors);
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
          it's InverseTransform and call ProximitySensorUpdate.
      }

      APosition := PSI.InverseTransform.MultPoint(CameraVectors.Translation);

      NewIsActive :=
        (APosition.X >= ProxNode.FdCenter.Value.X - ProxNode.FdSize.Value.X / 2) and
        (APosition.X <= ProxNode.FdCenter.Value.X + ProxNode.FdSize.Value.X / 2) and
        (APosition.Y >= ProxNode.FdCenter.Value.Y - ProxNode.FdSize.Value.Y / 2) and
        (APosition.Y <= ProxNode.FdCenter.Value.Y + ProxNode.FdSize.Value.Y / 2) and
        (APosition.Z >= ProxNode.FdCenter.Value.Z - ProxNode.FdSize.Value.Z / 2) and
        (APosition.Z <= ProxNode.FdCenter.Value.Z + ProxNode.FdSize.Value.Z / 2) and
        { ... and the box is not empty, which for ProximitySensor
          is signalled by any size <= 0 (yes, equal 0 also means empty).
          We check this at the end, as this is the least common situation? }
        (ProxNode.FdSize.Value.X > 0) and
        (ProxNode.FdSize.Value.Y > 0) and
        (ProxNode.FdSize.Value.Z > 0);

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
          ADirection := PSI.InverseTransform.MultDirection(CameraVectors.Direction);
          AUp        := PSI.InverseTransform.MultDirection(CameraVectors.Up);
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
  out CameraVectors: TViewVectors): boolean;
begin
  // note that HasWorldTransform implies also World <> nil
  Result := HasWorldTransform and (World.MainCamera <> nil);
  if Result then
  begin
    World.MainCamera.GetWorldView(
      CameraVectors.Translation,
      CameraVectors.Direction,
      CameraVectors.Up);
    CameraVectors.Translation := WorldInverseTransform.MultPoint    (CameraVectors.Translation);
    CameraVectors.Direction   := WorldInverseTransform.MultDirection(CameraVectors.Direction);
    CameraVectors.Up          := WorldInverseTransform.MultDirection(CameraVectors.Up);
  end;
end;

function TCastleSceneCore.GetCameraLocal(
  out CameraLocalPosition: TVector3): boolean;
begin
  // note that HasWorldTransform implies also World <> nil
  Result := HasWorldTransform and (World.MainCamera <> nil);
  if Result then
    CameraLocalPosition := WorldInverseTransform.MultPoint(World.MainCamera.WorldTranslation);
end;

procedure TCastleSceneCore.ChangedTransform;
begin
  inherited;
  { WorldInverseTransform changed, so update things depending on camera view.
    TODO: This way doesn't make a notification when WorldInverseTransform
    changed because parent transform changed.
    We should look at FWorldTransformAndInverseId changes to detect this? }
  InternalCameraChanged;
end;

procedure TCastleSceneCore.ChangeWorld(const Value: TCastleAbstractRootTransform);
begin
  inherited;

  { World changed, so update things depending on camera view.
    This is important to make ProximitySensors, Billboard etc. work immediately
    when item is part of the world (and knows the camera),
    and has ProcessEvents = true.

    Testcase:

    - 1st frame rendering not using BlendingSort for non-MainScene scenes:
      trees_blending/CW_demo.lpr testcase from Eugene.
    - Billboards test from Kagamma:
      https://sourceforge.net/p/castle-engine/discussion/general/thread/882ca037/
      https://gist.github.com/michaliskambi/9520282717870d3be1511412754958e9

  }
  if Value <> nil then
    InternalCameraChanged;
end;

{ compiled scripts ----------------------------------------------------------- }

procedure TCastleSceneCore.RegisterCompiledScript(const HandlerName: string;
  Handler: TCompiledScriptHandler);
var
  HandlerInfo: PCompiledScriptHandlerInfo;
begin
  HandlerInfo := PCompiledScriptHandlerInfo(CompiledScriptHandlers.Add);
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
      CompiledScriptHandlers.L[I].Handler(ReceivedValue, NextEventTime);
      Break;
    end;
end;

{ camera ------------------------------------------------------------------ }

function TCastleSceneCore.SensibleCameraRadius(out RadiusAutoCalculated: Boolean): Single;
var
  NavigationNode: TNavigationInfoNode;
begin
  Result := 0;
  RadiusAutoCalculated := false;

  NavigationNode := NavigationInfoStack.Top;
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 1) then
    Result := NavigationNode.FdAvatarSize.Items[0];

  { if avatarSize doesn't specify Radius, or specifies invalid <= 0,
    use DefaultCameraRadius. }
  if Result <= 0 then
  begin
    Result := DefaultCameraRadius;
    RadiusAutoCalculated := true;
  end;
end;

procedure TCastleSceneCore.InternalUpdateNavigation(
  const Navigation: TCastleNavigation);
var
  NavigationNode: TNavigationInfoNode;
  ViewpointNode: TAbstractViewpointNode;
  Radius: Single;
  RadiusAutoCalculated: Boolean;

  procedure UpdateWalkNavigation(const Navigation: TCastleWalkNavigation);
  begin
    { calculate Navigation.PreferredHeight }
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 2) then
      Navigation.PreferredHeight := NavigationNode.FdAvatarSize.Items[1]
    else
      Navigation.PreferredHeight := Max(TCastleNavigation.DefaultPreferredHeight, Radius * RadiusToPreferredHeightMin);

    Navigation.CorrectPreferredHeight;

    { calculate Navigation.ClimbHeight }
    if (NavigationNode <> nil) and
       (NavigationNode.FdAvatarSize.Count >= 3) then
      Navigation.ClimbHeight := Max(NavigationNode.FdAvatarSize.Items[2], 0.0)
    else
      Navigation.ClimbHeight := 0;

    { calculate Navigation.HeadBobbing* }
    if NavigationNode <> nil then
    begin
      Navigation.HeadBobbing := NavigationNode.HeadBobbing;
      Navigation.HeadBobbingTime := NavigationNode.HeadBobbingTime;
    end else
    begin
      Navigation.HeadBobbing := TCastleWalkNavigation.DefaultHeadBobbing;
      Navigation.HeadBobbingTime := TCastleWalkNavigation.DefaultHeadBobbingTime;
    end;

    { calculate Navigation.MoveSpeed }
    if NavigationNode = nil then
      { Since we don't have NavigationNode.speed, we just calculate some
        speed that should "feel sensible". We base it on Radius,
        that was set above. }
      Navigation.MoveSpeed := Navigation.Radius * 20
    else
      { This is OK, also for NavigationNode.FdSpeed.Value = 0 case. }
      Navigation.MoveSpeed := NavigationNode.FdSpeed.Value;
  end;

  procedure UpdateExamineNavigation(const Navigation: TCastleExamineNavigation);
  begin
    if ViewpointNode <> nil then
    begin
      Navigation.AutoCenterOfRotation := ViewpointNode.AutoCenterOfRotation;
      Navigation.CenterOfRotation := ViewpointNode.Transform.MultPoint(
        ViewpointNode.CenterOfRotation);
    end else
    begin
      Navigation.AutoCenterOfRotation := true;
      Navigation.CenterOfRotation := TVector3.Zero; // remove previous customization of this property
    end;
  end;

begin
  NavigationNode := NavigationInfoStack.Top;
  ViewpointNode := ViewpointStack.Top;

  { calculate Radius }
  Radius := SensibleCameraRadius(RadiusAutoCalculated);
  Navigation.Radius := Radius;

  if Navigation is TCastleWalkNavigation then
    UpdateWalkNavigation(TCastleWalkNavigation(Navigation));
  if Navigation is TCastleExamineNavigation then
    UpdateExamineNavigation(TCastleExamineNavigation(Navigation));
end;

procedure TCastleSceneCore.InternalUpdateCamera(const ACamera: TCastleCamera;
  const WorldBox: TBox3D;
  const RelativeCameraTransform, AllowTransitionAnimate: boolean);
var
  APosition: TVector3;
  ADirection: TVector3;
  AUp: TVector3;
  GravityUp: TVector3;
  Radius, OriginX, OriginY: Single;
  RadiusAutoCalculated: Boolean;
  ViewpointNode: TAbstractViewpointNode;
  NavigationNode: TNavigationInfoNode;
  FieldOfView: TSingleList;
begin
  Radius := SensibleCameraRadius(RadiusAutoCalculated);
  if RadiusAutoCalculated then
    { Set ProjectionNear to zero, this way we avoid serializing value
      when it is not necessary to serialize it
      (because it can be calculated by each TCastleViewport.CalculateProjection). }
    ACamera.ProjectionNear := 0
  else
    ACamera.ProjectionNear := Radius * RadiusToProjectionNear;

  { Default projection parameters.
    Reset here anything that is determined by TCastleSceneCore.InternalUpdateCamera. }
  ACamera.ProjectionType := ptPerspective;
  ACamera.ProjectionFar := 0;
  ACamera.Perspective.FieldOfView := TCastlePerspective.DefaultFieldOfView;
  ACamera.Perspective.FieldOfViewAxis := TCastlePerspective.DefaultFieldOfViewAxis;
  ACamera.Orthographic.Width := 0;
  ACamera.Orthographic.Height := 0;
  ACamera.Orthographic.Origin := TVector2.Zero;
  {$warnings off} // using deprecated to keep it working
  ACamera.Orthographic.Stretch := false;
  {$warnings on}

  ViewpointNode := ViewpointStack.Top;
  NavigationNode := NavigationInfoStack.Top;

  if ViewpointNode <> nil then
  begin
    ViewpointNode.GetView(APosition, ADirection, AUp, GravityUp);

    { Transform into world coordinates, as camera is set in world coordinates.
      As an exception for backward compatibility: when HasWorldTransform = false,
      we just assume that this scene will be added to world without any transformations.

      This is possible when you load a scene to Viewport.Items.MainScene,
      but MainScene is not yet part of Viewport.Items, like

        Viewport.Items.MainScene := TCastleScene.Create(Self);
        Viewport.Items.MainScene.Load(...);
        Viewport.Items.Insert(0, Items.MainScene);

      ... which is even done by src/deprecated_units/castlelevels.pas.
      Then MainScene.Load causes InternalUpdateCamera, with MainScene being used.
      In the long-term future, some day MainScene will be removed and this
      complication should disappear.

      Testcase: mountains-of-fire. }
    if HasWorldTransform then
    begin
      APosition  := WorldTransform.MultPoint    (APosition);
      ADirection := WorldTransform.MultDirection(ADirection);
      AUp        := WorldTransform.MultDirection(AUp);
      GravityUp  := WorldTransform.MultDirection(GravityUp);
    end;

    if ViewpointNode is TViewpointNode then
    begin
      ACamera.Perspective.FieldOfView :=
        TViewpointNode(ViewpointNode).FieldOfView;
      if TViewpointNode(ViewpointNode).FieldOfViewForceVertical then
        ACamera.Perspective.FieldOfViewAxis := faVertical;
    end else
    if ViewpointNode is TPerspectiveCameraNode_1 then
    begin
      ACamera.Perspective.FieldOfView :=
        TPerspectiveCameraNode_1(ViewpointNode).FdHeightAngle.Value;
    end else
    if ViewpointNode is TOrthoViewpointNode then
    begin
      ACamera.ProjectionType := ptOrthographic;

      { default Dimensions, for default OrthoViewpoint.fieldOfView }
      ACamera.Orthographic.Width := 2;
      ACamera.Orthographic.Height := 2;
      OriginX := 0.5;
      OriginY := 0.5;

      FieldOfView := TOrthoViewpointNode(ViewpointNode).FdFieldOfView.Items;

      if FieldOfView.Count > 2 then ACamera.Orthographic.Width  := FieldOfView.Items[2] - FieldOfView.Items[0];
      if FieldOfView.Count > 3 then ACamera.Orthographic.Height := FieldOfView.Items[3] - FieldOfView.Items[1];

      if FieldOfView.Count > 0 then OriginX := - FieldOfView.Items[0] / ACamera.Orthographic.Width;
      if FieldOfView.Count > 1 then OriginY := - FieldOfView.Items[1] / ACamera.Orthographic.Height;

      ACamera.Orthographic.Origin := Vector2(OriginX, OriginY);
    end else
    if ViewpointNode is TOrthographicCameraNode_1 then
    begin
      ACamera.ProjectionType := ptOrthographic;
      ACamera.Orthographic.Width := TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value;
      ACamera.Orthographic.Height := TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value;
      ACamera.Orthographic.Origin := Vector2(0.5, 0.5);
    end;
  end else
  begin
    { Suitable viewpoint,
      with dir -Z and up +Y (like standard VRML/X3D viewpoint) }
    CameraViewpointForWholeScene(WorldBox, 2, 1, false, true,
      APosition, ADirection, AUp, GravityUp);
  end;

  if NavigationNode <> nil then
    ACamera.ProjectionFar := NavigationNode.VisibilityLimit;

  ACamera.GravityUp := GravityUp;

  { TODO:
    If RelativeCameraTransform, then we should move relative to initial camera changes,
    to honor X3D dictated Viewpoint animation behavior.
    Right now, we always behave like RelativeCameraTransform=false. }

  if AllowTransitionAnimate and (not ForceTeleportTransitions) then
    {$warnings off} // using deprecated, it really should be internal - camera transition following X3D events
    CameraTransition(ACamera, APosition, ADirection, AUp)
    {$warnings on}
  else
    ACamera.SetWorldView(APosition, ADirection, AUp);
end;

procedure TCastleSceneCore.CameraTransition(const Camera: TCastleCamera;
  const APosition, ADirection, AUp: TVector3);
var
  TransitionAnimate: boolean;
  TransitionTime: TFloatTime;
  NavigationNode: TNavigationInfoNode;
  TransitionType: string;
  I: Integer;
begin
  NavigationNode := NavigationInfoStack.Top;

  TransitionAnimate := true;
  TransitionTime := 1;

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
        WritelnWarning('X3D', Format('Unrecognized transitionType "%s"', [TransitionType]));
    end;

  { calculate TransitionTime }
  if NavigationNode <> nil then
  begin
    TransitionTime := NavigationNode.FdTransitionTime.Value;
    { correct TransitionAnimate in case TransitionTime invalid }
    if TransitionTime <= 0 then
      TransitionAnimate := false;
  end;

  if TransitionAnimate then
  begin
    Camera.AnimateTo(APosition, ADirection, AUp, TransitionTime);
    WatchForTransitionComplete := true;
  end else
  begin
    Camera.SetWorldView(APosition, ADirection, AUp);
    if NavigationInfoStack.Top <> nil then
      NavigationInfoStack.Top.EventTransitionComplete.Send(true, NextEventTime);
  end;
end;

procedure TCastleSceneCore.CameraTransition(const Camera: TCastleCamera;
  const APosition, ADirection, AUp, GravityUp: TVector3);
begin
  Camera.GravityUp := GravityUp;
  {$warnings off} // using deprecated in deprecated
  CameraTransition(Camera, APosition, ADirection, AUp);
  {$warnings on}
end;

procedure TCastleSceneCore.CameraTransition(const Navigation: TCastleNavigation;
  const APosition, ADirection, AUp: TVector3);
begin
  {$warnings off} // using deprecated in deprecated
  CameraTransition(Navigation.Camera, APosition, ADirection, AUp);
  {$warnings on}
end;

procedure TCastleSceneCore.CameraTransition(const Navigation: TCastleNavigation;
  const APosition, ADirection, AUp, GravityUp: TVector3);
begin
  {$warnings off} // using deprecated in deprecated
  CameraTransition(Navigation.Camera, APosition, ADirection, AUp, GravityUp);
  {$warnings on}
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
  L: TAbstractPunctualLightNode absolute Node;
begin
  if L.FdShadowVolumes.Value and
     L.FdShadowVolumesMain.Value then
  begin
    FMainLightForShadowsNode := L;
    FMainLightForShadowsTransform := StateStack.Top.Transformation.Transform;
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
      RootNode.Traverse(TAbstractPunctualLightNode,
        {$ifdef FPC}@{$endif}SearchMainLightForShadows);
  end;

begin
  if not (fvMainLightForShadows in Validities) then
  begin
    CalculateMainLightForShadows;
    Include(Validities, fvMainLightForShadows);
  end;
end;

function TCastleSceneCore.InternalMainLightForShadowVolumes(
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
begin
  if NavigationInfoStack.Top <> nil then
    Result := NavigationInfoStack.Top.HeadlightNode
  else
    Result := nil;
end;

procedure TCastleSceneCore.UpdateHeadlightOnFromNavigationInfo;
begin
  if NavigationInfoStack.Top <> nil then
    HeadlightOn := NavigationInfoStack.Top.Headlight
  else
    HeadlightOn := DefaultNavigationInfoHeadlight;
end;

procedure TCastleSceneCore.PrepareResources(const Options: TPrepareResourcesOptions;
  const Params: TPrepareParams);

  { PrepareShapesOctrees and PrepareShadowVolumes could be optimized
    into one run }

  procedure PrepareShapesOctrees;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      Shape.InternalOctreeTriangles;
  end;

  procedure PrepareShadowVolumes;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(false);
    for Shape in ShapeList do
      Shape.InternalShadowVolumes.PrepareResources;
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
  if RootNode <> nil then
    {$warnings off} // using deprecated, as for now there's no way to search for unnamed nodes
    WorldInfoNode := RootNode.TryFindNode(TWorldInfoNode, true) as TWorldInfoNode
    {$warnings on}
  else
    WorldInfoNode := nil;

  if (WorldInfoNode <> nil) and
     (WorldInfoNode.FdTitle.Value <> '') then
    Result := WorldInfoNode.FdTitle.Value else
    Result := URICaption(URL);
end;

function TCastleSceneCore.Node(const NodeClass: TX3DNodeClass; const NodeName: string;
  const Options: TFindNodeOptions): TX3DNode;
begin
  if RootNode = nil then
  begin
    if fnNilOnMissing in Options then
      Result := nil
    else
      raise EX3DNotFound.CreateFmt('Cannot find node "%s" because no model is loaded', [NodeName]);
  end else
    Result := RootNode.FindNode(NodeClass, NodeName, Options);
end;

function TCastleSceneCore.Node(const NodeName: string): TX3DNode;
begin
  Result := Node(TX3DNode, NodeName);
end;

function TCastleSceneCore.Field(const NodeName, FieldName: string): TX3DField;
begin
  Result := Node(TX3DNode, NodeName).Field(FieldName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Field name "%s" not found', [FieldName]);
end;

function TCastleSceneCore.Event(const NodeName, EventName: string): TX3DEvent;
begin
  Result := Node(TX3DNode, NodeName).AnyEvent(EventName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Event name "%s" not found', [EventName]);
end;

{$ifdef GENERIC_METHODS}

{$ifdef FPC}generic{$endif} function TCastleSceneCore.Node<T>(const NodeName: string;
  const Options: TFindNodeOptions): T;
begin
  if RootNode = nil then
    raise EX3DNotFound.CreateFmt('Cannot find node "%s" because no model is loaded', [NodeName])
  else
    Result := RootNode.{$ifdef FPC}specialize{$endif} Find<T>(NodeName, Options);
end;

{$ifdef FPC}generic{$endif} function TCastleSceneCore.Field<T>(const NodeName, FieldName: string): TX3DField;
begin
  Result := {$ifdef FPC}specialize{$endif} Node<T>(NodeName).Field(FieldName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Field name "%s" not found', [FieldName]);
end;

{$ifdef FPC}generic{$endif} function TCastleSceneCore.Event<T>(const NodeName, EventName: string): TX3DEvent;
begin
  Result := {$ifdef FPC}specialize{$endif} Node<T>(NodeName).AnyEvent(EventName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Event name "%s" not found', [EventName]);
end;

{$endif}

procedure TCastleSceneCore.InternalInvalidateBackgroundRenderer;
begin
end;

procedure TCastleSceneCore.UnregisterScene(Node: TX3DNode);
begin
  Node.UnregisterScene;
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
    end else
      OldForceTeleport := false; // silence warning

    if FViewpointsArray[Idx] = FViewpointStack.Top then
      FViewpointsArray[Idx].Bound := false;
    FViewpointsArray[Idx].Bound := true;

    if not Animated then
      ForceTeleportTransitions := OldForceTeleport;
  end;
end;

procedure TCastleSceneCore.AddViewpointFromNavigation(
  const Navigation: TCastleNavigation; const AName: string);
var
  APosition: TVector3;
  ADirection: TVector3;
  AUp: TVector3;
  GravityUp: TVector3;
  Version: TX3DCameraVersion;
  NewViewNodeMake: TMakeX3DViewpoint;
  NewViewNode: TAbstractChildNode;
  NewViewpointNode: TAbstractViewpointNode;
  NavigationType: string;
  Walk: TCastleWalkNavigation;
  Examine: TCastleExamineNavigation;
  WalkSpeed, VisibilityLimit: Single;
  AvatarSize: TVector3;
  NewNavigationNode: TNavigationInfoNode;
  NewGroupNode: TGroupNode;
  NewRoute: TX3DRoute;
begin
  if RootNode = nil then
    raise Exception.Create('You have to initialize RootNode, usually just by loading some scene to TCastleSceneCore.Load, before adding viewpoints');
  if Navigation.Camera <> nil then
    raise Exception.Create('Navigation must be part of some Viewport before using AddViewpointFromNavigation');

  Navigation.Camera.GetWorldView(APosition, ADirection, AUp);
  GravityUp := Navigation.Camera.GravityUp;

  if RootNode.HasForceVersion and (RootNode.ForceVersion.Major <= 1) then
    Version := cvVrml1_Inventor
  else
    Version := cvVrml2_X3d;
  NewViewNodeMake := TMakeX3DViewpoint.Create;
  try
    NewViewNodeMake.Version := Version;
    NewViewNodeMake.Position := APosition;
    NewViewNodeMake.Direction := ADirection;
    NewViewNodeMake.Up := AUp;
    NewViewNodeMake.GravityUp := GravityUp;

    if Navigation is TCastleExamineNavigation then
    begin
      NewViewNodeMake.AutoCenterOfRotation := TCastleExamineNavigation(Navigation).AutoCenterOfRotation;
      NewViewNodeMake.CenterOfRotation := TCastleExamineNavigation(Navigation).CenterOfRotation;
    end;

    NewViewNode := NewViewNodeMake.ToNode(NewViewpointNode);
  finally FreeAndNil(NewViewNodeMake) end;

  NewViewpointNode.FdDescription.Value := AName;
  NewViewpointNode.X3DName := 'Viewpoint' + IntToStr(Random(10000));
  NewViewpointNode.Scene := Self;

  { Create NavigationInfo node }
  Walk := nil;
  Examine := nil;
  if Navigation is TCastleWalkNavigation then
  begin
    Walk := Navigation as TCastleWalkNavigation;
    if Walk.Gravity then
      NavigationType := 'WALK'
    else
      NavigationType := 'FLY';
  end else
  if Navigation is TCastleExamineNavigation then
  begin
    Examine := Navigation as TCastleExamineNavigation;
    if Examine.Turntable then
      NavigationType := 'TURNTABLE'
    else
      NavigationType := 'EXAMINE';
  end;

  AvatarSize.X := Navigation.Radius;
  if Walk <> nil then begin
    WalkSpeed := Walk.MoveSpeed;
    AvatarSize.Y := Walk.PreferredHeight;
    AvatarSize.Z := Walk.ClimbHeight;
  end
  else begin
    WalkSpeed := 0;
    AvatarSize.Y := 0;
    AvatarSize.Z := 0;
  end;
  VisibilityLimit := 0;

  NewNavigationNode := MakeCameraNavNode(Version, '', NavigationType, WalkSpeed,
    VisibilityLimit, AvatarSize, HeadlightOn);
  NewNavigationNode.X3DName := 'NavInfo' + IntToStr(Random(10000));
  NewNavigationNode.Scene := Self;

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
      {$ifdef FPC}
      {$warnings off} // knowingly using deprecated, to keep it working
      Enum.AnimationPrefix := AnimationPrefix;
      {$warnings on}
      {$endif}
      { OnlyActive = false, to also find animations under inactive Switch clauses.
        Since the Switch.WhichChoice may change at runtime,
        while the animations list should stay constant. }
      RootNode.EnumerateNodes(TTimeSensorNode,
        {$ifdef FPC}@{$endif}Enum.Enumerate, false);

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
    Result := FAnimationsList.Objects[Index] as TTimeSensorNode
  else
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
    ResetAnimationState(TimeNode);
    TimeNode.FakeTime(TimeInAnimation, Loop, Forward, NextEventTime);
    FinishTransformationChanges;
  end;
end;

procedure TCastleSceneCore.ForceInitialAnimationPose;
begin
  if NewPlayingAnimationUse then
  begin
    ResetAnimationState(NewPlayingAnimationNode);
    { After StopAnimation,
      we may have NewPlayingAnimationUse and (NewPlayingAnimationNode = nil),
      this is a valid state. }
    if NewPlayingAnimationNode <> nil then
    begin
      NewPlayingAnimationNode.FakeTime(
        NewPlayingAnimationInitialTime,
        NewPlayingAnimationLoop,
        NewPlayingAnimationForward,
        NextEventTime);
      FinishTransformationChanges;
    end;
  end;
end;

function TCastleSceneCore.PlayAnimation(const AnimationName: string;
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

  Result := PlayAnimation(AnimationName, Loop, Forward);
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
    if PlayingAnimationNode <> nil then
      Params.TransitionDuration := DefaultAnimationTransition;
    Result := PlayAnimation(Params);
  finally FreeAndNil(Params) end;
end;

function TCastleSceneCore.PlayAnimation(
  const Parameters: TPlayAnimationParameters): boolean;
var
  Index: Integer;
begin
  ProcessEvents := true;

  Index := FAnimationsList.IndexOf(Parameters.Name);
  Result := Index <> -1;
  if Result then
  begin
    { If we already have NewPlayingAnimationUse scheduled
      (another PlayAnimation was done right before this),
      then Parameters.TransitionDuration indicates to do blending from
      NewPlayingAnimationNode to Parameters.Name animation.
      In this case, we need to "apply" NewPlayingAnimationNode
      (make it actually present in X3D nodes),
      otherwise we would accidentally do blending with *even older* animation.

      Testcase in Unholy:
      - going out of sewers with keys or mouse
        ('going_up_on_ladder' finishes, changes to 'idle', which changes to 'stealth_idle' immediately)
      - going up the ladder to warehouse, only when using mouse
        ('going_up_on_ladder' finishes, changes to 'idle', which changes to 'walking' immediately then)
    }
    if NewPlayingAnimationUse and (Parameters.TransitionDuration <> 0) then
      ApplyNewPlayingAnimation;

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
  end else
  begin
    WritelnWarning('Animation "%s" not found on scene %s (loaded from %s)', [
      Parameters.Name,
      Name,
      URIDisplay(URL)
    ]);
  end;
end;

procedure TCastleSceneCore.PlayingAnimationIsActive(
  const Event: TX3DEvent; const Value: TX3DField; const ATime: TX3DTime);
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

function TCastleSceneCore.ApplyNewPlayingAnimation: Boolean;
var
  NeedsUpdateTimeDependent: Boolean;
begin
  Result := true;
  if NewPlayingAnimationUse then
  begin
    UpdateNewPlayingAnimation(NeedsUpdateTimeDependent);

    { The case of NeedsUpdateTimeDependent = true is normal,
      and we should handle it.
      It can easily occur if the first thing you do with Scene is call Scene.PlayAnimation(...),
      then Scene.StopAnimation within the same frame.
      This will cause ApplyNewPlayingAnimation that will initialize the animation,
      only to have it stopped immediately.
      Such ApplyNewPlayingAnimation cannot Exit(false), as it would make StopAnimation to fail,
      see https://github.com/castle-engine/castle-engine/issues/273 .

      It is tempting to do nothing and ignore NeedsUpdateTimeDependent here.
      It could be often OK, since we stop the animation?
      But doing UpdateTimeDependentList is what is consistent with normal code flow,
      so safest. }
    if NeedsUpdateTimeDependent then
      UpdateTimeDependentList(0, false); // update TimeSensors

    if NewPlayingAnimationUse then
    begin
      WritelnWarning('StopNotification callback of an old animation initialized another animation, bypassing the CurrentAnimation. ' + 'The StopAnimation will not actually stop the animation, and the StopNotification callback of the CurrentAnimation is now overridden so we cannot call it anymore.');
      Exit(false);
    end;
  end;
end;

procedure TCastleSceneCore.StopAnimation(const DisableStopNotification: Boolean = false);
begin
  if DisableStopNotification then
  begin
    PlayingAnimationStopNotification := nil;
  end else
  begin
    { If new animation was requested, but not yet processed by UpdateNewPlayingAnimation:
      Make it start, to early call the "stop notification" callback
      for the previous animation (before calling the "stop notification"
      callback for the new animation).
      This also makes all behavior consistent with
      "what if the new animation was actually applied already", i.e. the same calls
      to TTimeSensorNode.Stop and TTimeSensorNode.Start will be done. }
    if not ApplyNewPlayingAnimation then
      Exit;
  end;

  { Stop animation by setting NewPlayingAnimationNode to nil,
    this way the "stop notification" callback
    for new animation will be correctly called.

    The next Update (with UpdateNewPlayingAnimation) will set PlayingAnimationNode
    to nil this way. }
  FCurrentAnimation := nil;
  NewPlayingAnimationNode := FCurrentAnimation;
  NewPlayingAnimationUse := true;
  { No need to set other NewPlayingAnimationXxx,
    like NewPlayingAnimationStopNotification,
    they will be ignored when NewPlayingAnimationNode = nil. }
end;

procedure TCastleSceneCore.ResetAnimationState(const IgnoreAffectedBy: TTimeSensorNode);
var
  F: TX3DField;
begin
  { set fields in AnimationAffectedFields to their reset values }
  for F in AnimationAffectedFields do
    F.InternalRestoreSaveValue;
  FinishTransformationChanges;
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
    GLContextClose; // force TRenderer.Prepare on shapes
    { Free and recalculate all proxy nodes...
      TODO: this could be done much more efficiently,
      we only need to free proxies on text nodes. }
    ChangedAll;
    RootNode.EnumerateNodes(TTextNode,
      {$ifdef FPC}@{$endif}FontChanged_TextNode, false);
    RootNode.EnumerateNodes(TAsciiTextNode_1,
      {$ifdef FPC}@{$endif}FontChanged_AsciiTextNode_1, false);
  end;
end;

function TCastleSceneCore.Clone(const AOwner: TComponent): TCastleSceneCore;
begin
  Result := TComponentClass(ClassType).Create(AOwner) as TCastleSceneCore;
  Result.FURL := FURL + '[Clone]';
  if RootNode <> nil then
    Result.Load(RootNode.DeepCopy as TX3DRootNode, true);
end;

function TCastleSceneCore.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'URL', 'ProcessEvents', 'AutoAnimation', 'AutoAnimationLoop',
       'DefaultAnimationTransition', 'PreciseCollisions', 'ExposeTransforms',
       'TimePlaying', 'TimePlayingSpeed', 'Cache'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
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

procedure TCastleSceneCore.InternalIncShapesHash;
begin
  // WritelnLog('Scene %s shapes hash changed to %d', [Name, FShapesHash]);
  if FShapesHash = High(FShapesHash) then
  begin
    WritelnWarning('Scene %s state hash reached the highest number of 64-bit value. Resetting', [Name]);
    FShapesHash := 1;
  end else
    Inc(FShapesHash);
end;

procedure TCastleSceneCore.SetExposeTransforms(const Value: TStrings);
begin
  FExposeTransforms.Assign(Value);
end;

procedure TCastleSceneCore.ExposeTransformsChange(Sender: TObject);
var
  TransformName, TransformNamePascal: String;
  TransformNode: TTransformNode;
  TransformChild: TCastleTransform;
  E: TExposedTransform;
  I: Integer;
begin
  FExposedTransforms.Clear;
  if RootNode = nil then // needed for subsequent RootNode.TryFindNodeByName
    Exit;

  for TransformName in FExposeTransforms do
  begin
    if TransformName = '' then // ignore empty lines on FExposeTransforms list
      Continue;

    // calculate TransformNode
    TransformNode := RootNode.FindNode(TTransformNode, TransformName, [fnNilOnMissing]) as TTransformNode;
    if TransformNode = nil then
    begin
      WritelnWarning('No TTransformNode (bone) named "%s" found in model', [TransformName]);
      Continue;
    end;

    for E in FExposedTransforms do
      if E.Node = TransformNode then
        Continue; // ignore duplicates on FExposeTransforms list

    TransformNamePascal := TExposedTransform.X3dNameToPascal(ExposeTransformsPrefix, TransformName);

    // calculate TransformChild
    TransformChild := nil;
    for I := 0 to Count - 1 do
      if Items[I].Name = TransformNamePascal then
      begin
        TransformChild := Items[I];
        Break;
      end;
    if TransformChild = nil then
    begin
      { Create new TCastleTransform.
        Note
        - We connect to it only by Name, not any other internal reference.
          This is important, as we need to restore the connection
          e.g. after deserializing it from design file.
        - We use current Owner as the owner, not Self.
          This makes it work in case of editor or TransformLoad nicely.
          Intuitively, we don't want to manage the lifetime of these scenes
          here: the lifetime of them should be up to our owner.
      }
      TransformChild := TCastleTransform.Create(Owner);
      TransformChild.Name := TransformNamePascal;
      Add(TransformChild);

      // otherwise changing TCastleSceneCore.ExposeTransforms would not update CGE editor hierarchy view
      if not (csTransient in ComponentStyle) then
        InternalCastleDesignInvalidate := true;
    end;

    // create TExposedTransform
    E := TExposedTransform.Create(Self, TransformNode, TransformChild);
    E.Synchronize;
    FExposedTransforms.Add(E);
  end;
end;

procedure TCastleSceneCore.SetExposeTransformsPrefix(const Value: String);
var
  E: TExposedTransform;
begin
  if FExposeTransformsPrefix <> Value then
  begin
    FExposeTransformsPrefix := Value;
    // rename existing children
    for E in FExposedTransforms do
    begin
      E.Child.Name := TExposedTransform.X3dNameToPascal(ExposeTransformsPrefix, E.Node.X3DName);
      InternalCastleDesignInvalidate :=  true;
    end;
  end;
end;

end.
