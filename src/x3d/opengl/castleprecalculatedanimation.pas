{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A precalculated 3D animation rendered in OpenGL (TCastlePrecalculatedAnimation). }
unit CastlePrecalculatedAnimation;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FGL,
  X3DNodes, CastleRenderer, CastleSceneCore, CastleScene,
  CastleUtils, CastleBoxes, CastleClassUtils,
  CastleKeysMouse, CastleTimeUtils, CastleFrustum, CastleVectors, Castle3D, X3DTriangles,
  CastleTriangles, CastleRectangles, CastleCameras,
  CastleInternalNodeInterpolator, X3DLoad;

type
  { A "precalculated" animation done by
    interpolating between a number of 3D model states.

    After constructing an object of this class, you must actually
    load it's animation by calling Load or LoadFromFile or LoadFromEvents
    etc.

    When loading you must provide one or more X3D models with
    their associated times. Animation will show a transition from the first
    model to the last. If models are "structurally equal" then the transition
    between two successive models will be smooth, otherwise a sudden change
    will be shown. "Structurally equal" means
    the same nodes hierarchy, the same names of nodes,
    the same values of all fields (with the exception of fields
    that are floating-point based and so can be interpolated, for example
    SFFloat, SFVec3f and equivalent MFXxx fields).
    For multi-valued fields (MFXxx) that can be interpolated: note that values
    of items may differ, but still the counts of items must be equal.

    This creates a list of @link(Scenes) such that
    @unorderedList(
      @itemSpacing Compact
      @item the first scene on the list is exactly the 1st object
      @item the last scene on the list is exactly the last object
      @item(intermediate scenes are accordingly interpolated between
        the two surrounding "predefined" by you scenes)
    )

    For example, first object may be a small sphere with blue color, the other
    object may be a larger sphere with white color, and the simplest
    times are 0.0 for the 1st scene and 1.0 for the 2nd scene.
    The animation will show the blue sphere growing larger
    and fading into the white
    color. Of course, any kind of models is allowed --- e.g. it can
    be a walking man at various stages, so in effect you get an animation
    of walking man.

    A special case when you pass only one scene to this class is allowed
    (it may be handy in some situations). This will obviously produce
    just a still result, i.e. resulting TCastlePrecalculatedAnimation will be just
    a wrapper around single TCastleScene instance.

    For more information see our engine documentation on
    [http://castle-engine.sourceforge.net/engine_doc.php].
    Specifically the section
    "Non-interactive precalculated animation: TCastlePrecalculatedAnimation",
    [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.animation_precalculated.html]. }
  TCastlePrecalculatedAnimation = class(T3D)
  private
    FScenes: TCastleSceneList;
    function GetScenes(I: Integer): TCastleScene;
  private
    Renderer: TGLRenderer;
    FCache: TGLRendererContextCache;
    FTimeBegin, FTimeEnd: Single;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FOwnsFirstRootNode: boolean;
    FLoaded: boolean;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FTimeAtLoad: TFloatTime;
    FTime: TFloatTime;
    FShadowMaps: boolean;
    FShadowMapsDefaultSize: Cardinal;
    FTryFirstSceneDynamic: boolean;

    ValidBoundingBox: boolean;
    FBoundingBox: TBox3D;
    FCollisionUseLastScene: boolean;
    FInitialViewpointIndex: Cardinal;
    FInitialViewpointName: string;

    procedure SetShadowMaps(const Value: boolean);
    procedure SetShadowMapsDefaultSize(const Value: Cardinal);

    function InfoBoundingBox: string;
  private
    { Helpers for Load implementation. }
    Load_KeyNodes: TX3DNodeList;
    Load_KeyTimes: TSingleList;
    procedure Load_GetKeyNodeWithTime(const Index: Cardinal;
      out KeyNode: TX3DRootNode; out Time: Single);
  private
    { Helpers for LoadFromEvents implementation. }
    LoadFromEvents_TimeBegin: Single;
    LoadFromEvents_Scene: TCastleSceneCore;
    LoadFromEvents_ScenesPerTime: Cardinal;
    procedure LoadFromEvents_GetKeyNodeWithTime(const Index: Cardinal;
      out KeyNode: TX3DRootNode; out Time: Single);
    procedure LoadFromEvents_GetKeyNodeWithTime_Progress(
      const Index: Cardinal;
      out KeyNode: TX3DRootNode; out Time: Single);

    procedure SetOwnsFirstRootNode(const Value: boolean);
  protected
    procedure SetWorld(const Value: T3DWorld); override;

    { Internal version of @link(Load) routines, feasible to load
      from both ready KeyNodes array and to automatically generate KeyNodes
      on the fly.

      GetKeyNodeWithTime will be called with indexes from 0 to KeyNodesCount - 1.
      It's guaranteed that it will be called in this order (from 0 upwards to
      KeyNodesCount - 1) and will be called exactly once for each index.
      So it's safe to e.g. create RootNode with some costly operation there.

      Note that RootNode passed to GetKeyNodeWithTime becomes owned by
      this class. Well, you can get control over only the first one,
      by AOwnsFirstRootNode, but you cannot free it anyway while this is loaded.

      See @link(Load) for more information, including the meaning of
      EqualityEpsilon. }
    procedure LoadCore(
      GetKeyNodeWithTime: TGetKeyNodeWithTime;
      KeyNodesCount: Cardinal;
      AOwnsFirstRootNode: boolean;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single);

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

    { Constructor that allows you to pass your own Cache instance. }
    constructor CreateCustomCache(AOwner: TComponent;
      ACache: TGLRendererContextCache);

    destructor Destroy; override;

    { Load the animation scenes.
      Must be called (this or some other loading routine like LoadFromFile)
      before you do almost anything with this object.
      @link(Loaded) changes to @true after calling this.

      @param(KeyNodes
        Models describing the "predefined" frames of animation.
        They must descend from TX3DRootNode.

        For all nodes except the first: They are @italic(always)
        owned by this class --- that's needed,
        because actually we may do some operations on these models when
        building animation (including even freeing some KeyNodes,
        if we will find that they are equivalent to some other KeyNodes).
        They all must point to different objects.

        You must supply at least one item here (you cannot make an animation
        from 0 items).)

      @param(Times Array specifying the point of time for each "predefined"
        frame. Length of this array must equal to length of KeyNodes array.)

      @param(ScenesPerTime
        This says how many scenes will be used for period of time equal to 1.0.
        This will determine Scenes.Count.
        KeyNodes[0] always takes Scenes[0] and KeyNodes[High(KeyNodes)]
        always takes Scenes[Scenes.High].

        Note that if we will find that some nodes along the way are
        exactly equal, we may drop scenes count between --- because if they are
        both equal, we can simply render the same scene for some period
        of time. This is an optimization, and you shouldn't notice it at all,
        since rendeting will be the same (but less memory-consuming).

        Special value ScenesPerTime = 0 means that you want to have only the
        KeyNodes you explicitly passed in the scene, not more.
        No more intermediate scenes will ever be created.
        This creates a trivial animation that suddenly jumps from
        one RootNode to the next at specified times. It may be useful if you
        already have generated a lot of KeyNodes, densely distributed
        over time, and you don't need TCastlePrecalculatedAnimation to insert any more
        scenes.)

      @param(EqualityEpsilon
        This will be used for comparing fields, to decide if two fields
        (and, consequently, nodes) are equal. It will be simply
        passed to TX3DField.Equals.

        You can pass here 0 to use exact comparison, but it's
        advised to use here something > 0. Otherwise we could waste
        display list memory (and loading time) for many frames of the
        same node that are in fact equal.)
    }
    procedure Load(
      KeyNodes: TX3DNodeList;
      AOwnsFirstRootNode: boolean;
      AKeyTimes: TSingleList;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single);

    { Load precalculated animation by playing a single VRML/X3D file with
      events (interpolators, TimeSensor and such working).
      Conceptually, this "records" interactive animation stored in VRML/X3D file
      into TCastlePrecalculatedAnimation precalculated animation.

      ATimeBegin, ATimeEnd tell what time slice should be recorded.
      They will also set @link(TimeBegin) and @link(TimeEnd) properties.

      @param(ScenesPerTime
        tells with what density should the animation be recorded.
        See @link(Load) for ScenesPerTime, EqualityEpsilon precise documentation.
        Note that special value ScenesPerTime = 0 is interpreted here as
        "record only one, initial frame".)

      @param(ProgressTitle When <> '' we will use Progress.Init, Step, Fini
        to display nice progress of operation.) }
    procedure LoadFromEvents(
      RootNode: TX3DRootNode;
      AOwnsRootNode: boolean;
      const ATimeBegin, ATimeEnd: Single;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single;
      const ProgressTitle: string);

    { Load a dumb animation that consists of only one frame (so actually
      there's  no animation, everything is static).

      This just calls @link(Load) with parameters such that
      @orderedList(
        @item(KeyNodes list contains one specified node)
        @item(Times contain only one item 0.0)
        @item(ScenesPerTime and EqualityEpsilon have some unimportant
          values --- they are not meaningfull when you have only one scene)
      )

      This is usefull when you know that you have a static scene,
      but still you want to treat it as TCastlePrecalculatedAnimation. }
    procedure LoadStatic(RootNode: TX3DNode; AOwnsRootNode: boolean);

    { Load animation parameters (models to use, times to use and such)
      from given file.

      Various file formats are possible, everything that can be handled by
      Load3DSequence, in particular simple 3D model files, MD3,
      castle-anim-frames (described on
      [http://castle-engine.sourceforge.net/castle_animation_frames.php]).

      If you need more control over loading, for example you want to
      change some parameters at loading (for example, ScenesPerTime
      and EqualityEpsilon of castle-anim-frames files), you should use
      more flexible (and less comfortable to use)
      LoadFromFileToVars class procedure (specialized for castle-anim-frames files)
      or Load3DSequence (if you want to handle any files).

      @link(Loaded) property changes to @true after calling this.

      @param(AllowStdIn If @true, then URL = '-' is understood
        as "standard input".)

      @param(LoadTime If @true then loading changes
        current TimeLoop and TimeBackwards properties.
        Sometimes this is sensible (you want to allow control over them
        from the file), sometimes not (e.g. you set suitable values for them
        by code).

        Note that, independent of this, you can always change TimeLoop
        and TimeBackwards properties later,
        since these properties are writeable at any time.)

      @groupBegin }
    procedure LoadFromFile(const URL: string;
      const AllowStdIn: boolean; const LoadTime: boolean);
    { @groupEnd }

    { This releases all resources allocared by Load (or LoadFromFile).
      @link(Loaded) property changes to @false after calling this.

      It's safe to call this even if @link(Loaded) is already @false --- then
      this will do nothing. }
    procedure Close;

    property Loaded: boolean read FLoaded;

    { Is the RootNode in first scene owned by this TCastlePrecalculatedAnimation instance?
      If yes, it will be freed at closing the animation.
      Otherwise, you are responsible for freeing it yourself
      (but you cannot do this while animation is loaded, anyway). }
    property OwnsFirstRootNode: boolean
      read FOwnsFirstRootNode write SetOwnsFirstRootNode;

    { You can read anything from Scenes below. But you cannot set some
      things: don't set their scenes Attributes properties.
      Use only our @link(Attributes).

      The scenes here have TCastleSceneCore.Static set to @true, which means
      we assume you will not modify their VRML nodes graph (by TX3DField.Send
      and such). Note that this doesn't prevent you from enabling
      TCastleSceneCore.ProcessEvents on the first scene (TCastleSceneCore.ProcessEvents
      will be property handled regardless of TCastleSceneCore.Static value). }
    property Scenes[I: Integer]: TCastleScene read GetScenes;
    function ScenesCount: Integer;

    { Just a shortcut for Scenes[0]. }
    function FirstScene: TCastleScene;

    { Just a shortcut for Scenes[ScenesCount - 1]. }
    function LastScene: TCastleScene;

    { Prepare all scenes for rendering. Basically, this calls
      PrepareResources(...) for all Scenes.

      There's also a special memory (and prepare time) optimization used
      for prManifoldAndBorderEdges: we use the fact that animation scenes are
      "structurally equal", and so prepare and share one manifold edges
      information for all scenes.

      ProgressStep = @true is especially useful with this: we'll call
      Progress.Step then after preparing each scene.
      For portability, always check PrepareResourcesSteps, but for now this
      is just always equal ScenesCount. }
    procedure PrepareResources(Options: TPrepareResourcesOptions;
      ProgressStep: boolean; BaseLights: TAbstractLightInstancesList); override;
    function PrepareResourcesSteps: Cardinal; override;

    { Free resources for all scenes, it's useful if you know
      that you will not need some allocated resources anymore and you
      want to conserve memory use.

      See TCastleSceneCore.FreeResource documentation for a description of what
      are possible resources to free. }
    procedure FreeResources(Resources: TSceneFreeResources);

    { Close anything associated with current OpenGL context in this class.
      This calls GLContextClose on every Scenes[], and additionally may close
      some other internal things here. }
    procedure GLContextClose; override;

    { Just a shortcut for TimeEnd - TimeBegin. }
    function TimeDuration: Single;

    { This is TimeDuration * 2 if TimeBackwards, otherwise it's just
      TimeDuration. In other words, this is the time of the one "full"
      (forward + backward) animation. }
    function TimeDurationWithBack: Single;

    { First and last time that you passed to Load (or that were read
      from file by LoadFromFile).
      In other words, Times[0] and Times[High(Times)].
      @groupBegin }
    property TimeBegin: Single read FTimeBegin;
    property TimeEnd: Single read FTimeEnd;
    { @groupEnd }

    { Appropriate scene from @link(Scenes) based on given Time.
      If Time is between given TimeBegin and TimeEnd,
      then this will be appropriate scene in the middle.

      For Time outside the range TimeBegin .. TimeEnd
      behavior depends on TimeLoop and TimeBackwards properties:

      @unorderedList(
        @item(When not TimeLoop and not TimeBackwards then:

          If Time is < TimeBegin, always the first scene will
          be returned. If Time is > TimeEnd, always the last scene will
          be returned.

          So there will no real animation outside
          TimeBegin .. TimeEnd timeline.)

        @item(When not TimeLoop and TimeBackwards then:

          If Time is < TimeBegin, always the first scene will
          be returned. If Time is between TimeEnd and
          TimeEnd + TimeDuration, then the animation
          will be played backwards. When Time is > TimeEnd + TimeDuration,
          again always the first scene will be returned.

          So between TimeEnd and TimeEnd + TimeDuration
          animation will be played backwards, and
          there will no real animation outside
          TimeBegin .. TimeEnd + TimeDuration timeline.)

        @item(When TimeLoop and not TimeBackwards then:

          Outside TimeBegin .. TimeEnd, animation will cycle.
          This means that e.g. between TimeEnd and TimeEnd + TimeDuration
          animation will be played just like between TimeBegin and TimeEnd.)

        @item(When TimeLoop and TimeBackwardsm then:

          Outside TimeBegin .. TimeEnd, animation will cycle.
          Cycle between TimeEnd and TimeEnd + TimeDuration will
          go backwards. Cycle between TimeEnd + TimeDuration
          and TimeEnd + TimeDuration * 2 will again go forward.
          And so on.)
      )

      Overloaded version with explicit Loop parameter ignores the TimeLoop
      property. This way you can force looping (or force not looping),
      regardless of the TimeLoop property, so also regardless
      of loop setting in castle-anim-frames file.

      @groupBegin }
    function Scene(const Time: Single): TCastleScene;
    function Scene(const Time: Single; const Loop: boolean): TCastleScene;
    { @groupEnd }

    { Appropriate scene from @link(Scenes) based on current @link(Time).
      This is just a shortcut for Scene(@link(Time)),
      useful if you track animation time in our @link(Time) property. }
    function CurrentScene: TCastleScene;

    { Attributes controlling rendering.
      See TSceneRenderingAttributes and TRenderingAttributes
      for documentation of properties.

      You can change properties of this
      object at any time, but beware that some changes may force
      time-consuming regeneration of some things (like OpenGL display lists)
      in the nearest Render of the scenes.
      So explicitly calling PrepareResources may be useful after
      changing these Attributes.

      Note that Attributes may be accessed and even changed when the scene
      is not loaded (e.g. before calling Load / LoadFromFile).
      Also, Attributes are preserved between various animations loaded. }
    function Attributes: TSceneRenderingAttributes;

    { The sum of bounding boxes of all animation frames.

      Result of this function is cached, which means that it usually returns
      very fast. But you have to call ChangedAll when you changed something
      inside Scenes[] using some direct Scenes[].RootNode operations,
      to force recalculation of this box. }
    function BoundingBox: TBox3D; override;

    { Call this before directly freeing some VRML nodes in animation scenes. }
    procedure BeforeNodesFree;

    { Call this when you changed something
      inside Scenes[] using some direct Scenes[].RootNode operations.
      This calls TCastleScene.ChangedAll on all Scenes[]
      and invalidates some cached things inside this class. }
    procedure ChangedAll;

    { Returns some textual info about this animation.
      Similar to TCastleScene.Info. }
    function Info(
      ATriangleVerticesCounts,
      ABoundingBox,
      AManifoldAndBorderEdges: boolean): string;
      deprecated 'do not use this, better to construct a summary string yourself';

    { Handling key and mouse events.

      We pass key and mouse events only if there's exactly one scene
      (ScenesCount = 1), as there's no sensible way of activating
      VRML/X3D events when TCastlePrecalculatedAnimation contains
      more than one scene.
      (Precalculated animation of this class, and interactive
      animation by TCastleSceneCore.ProcessEvents do not mix sensibly.)

      So when ScenesCount = 1, we simply pass key and mouse events to
      the only Scene[0]. Be sure to turn on @code(Scene[0].ProcessEvents := true)
      if you want to make actual use of it.

      @groupBegin }
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    { @groupEnd }

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Initial world time, set by the ResetTimeAtLoad call.
      This can be useful for showing user
      time like @code("Animation Time: LoadTime + %f") on status bar.

      0 means that starting @link(Time) was TimeBegin of the animation
      (0.0 in case of normal VRML files, usually 0.0 in case of castle-anim-frames).
      Note that even when TimeBegin <> 0 (for castle-anim-frames), we still set
      TimeAtLoad to 0, this is nicer to show to user.

      Other value means that we used current real time as time origin,
      following VRML/X3D specification.
      See also [http://castle-engine.sourceforge.net/x3d_time_origin_considered_uncomfortable.php] }
    property TimeAtLoad: TFloatTime read FTimeAtLoad;

    { Current time of the animation. Although you do not have to use it:
      you can always acccess any point in time of the animation by @link(Scene).
      But sometimes tracking the current time here is most natural
      and comfortable.

      When we have exactly one scene in Scenes, our methods (ResetTime,
      ResetTimeAtLoad and Update) will synchronize Scenes[0].Time
      always to the same value as our own @link(Time).
      This makes time-dependent nodes (like TimeSensor,
      MovieTexture etc.) inside this scene work Ok. }
    property Time: TFloatTime read FTime;

    { Set @link(Time) to initial value after loading a world. }
    procedure ResetTimeAtLoad(const ForceTimeOrigin: boolean = false);

    { Set @link(Time) to arbitrary value. }
    procedure ResetTime(const NewValue: TFloatTime);

    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    procedure UpdateGeneratedTextures(
      const RenderFunc: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const OriginalViewport: TRectangle); override;
    procedure VisibleChangeNotification(const Changes: TVisibleChanges); override;
    procedure CameraChanged(ACamera: TCamera); override;
    function Dragging: boolean; override;

    property Cache: TGLRendererContextCache read FCache;

    { Turn this on to treat specially the case when a single scene (Scenes.Count = 1)
      is loaded: we will set this scene's Static = @false.
      This allows you to enable VRML/X3D events and dynamically change the scene
      in this very special case.
      The normal behavior, when we load many scenes (or when this property is @false),
      is to set all children scenes Static = @true.

      Practically, this is useful only for tools like view3dscene, that want
      to have full VRML/X3D events when possible, and at the same time they want
      to load everything as TCastlePrecalculatedAnimation, for ease of coding.

      To put it simply, just don't use this in normal programs -- it's a hack.

      Although Static can be later changed, but changing it (after loading) to @false
      is expensive (needs ChangedAll, that also recalculates shape tree, forces
      shape octree and other recalculations). That's why this property is needed,
      it sets Static correctly before loading the contents. }
    property TryFirstSceneDynamic: boolean
      read FTryFirstSceneDynamic write FTryFirstSceneDynamic default false;
  published
    { Is the animation time playing, and how fast.

      For exact meaning of our TimePlaying, TimePlayingSpeed, see
      TCastleSceneCore.TimePlaying, TCastleSceneCore.TimePlayingSpeed.
      Like in TCastleSceneCore, these are realized by our @link(Update) method,
      so Time is automatically increased in @link(Update) which is called
      automatically if you added this to some TCastleWindowCustom.Controls or
      TCastleControlCustom.Controls.

      Note that Scenes[0].TimePlaying, Scenes[0].TimePlayingSpeed do not matter
      when you're operating on the TCastlePrecalculatedAnimation level.
      They will not affect our @link(Time), or even Scenes[0].Time,
      and they will not be synchronized with our values.

      @groupBegin }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;
    { @groupEnd }

    { See @link(Scene) for precise description what this property does. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop default true;

    { See @link(Scene) for precise description what this property does. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards default false;

    { Should collision checking check also last animation frame.

      Regardless of this value, we always check collision with the
      first animation frame (FirstScene), of course only when
      FirstScene.OctreeCollisions is initialized, and only if
      @link(GetCollides) (which includes @link(GetExists)).

      When CollisionUseLastScene is @true, we will also check collision
      with the last animation frame's octree, i.e. LastScene.OctreeCollisions.
      (Of course, only if it's initialized, e.g. by adding
      ssDynamicCollisions to the LastScene.Spatial property.)
      So when CollisionUseLastScene, collision checking sees the animation
      as a sum of first and last frames geometry.
      CollisionUseLastScene
      is useful if the object is moving, but the move is very slight,
      so that the sum of first and last scenes geometry is good enough
      approximation of the whole geometry at any point of the animation.

      Although it seems like a totally dumb way to check for collisions,
      it's suitable for many purposes (see e.g. uses on "castle hall" level),
      it's simple and not memory-consuming, and you don't have to take
      any action when animation frame changes (because @link(Time) changes
      don't change the colliding geometry, so the animation is static from
      the point of view of collision checking routines).

      TODO: In the future other collision methods may be available.
      First of all, checking with sum of all bounding boxes, or with particular
      scene time box, should be available. }
    property CollisionUseLastScene: boolean
      read FCollisionUseLastScene
      write FCollisionUseLastScene default false;

    { At loading, process the animation to support shadow maps.
      See TCastleSceneCore.ShadowMaps and related properties for documentation.
      @groupBegin }
    property ShadowMaps: boolean read FShadowMaps write SetShadowMaps default true;
    property ShadowMapsDefaultSize: Cardinal
      read FShadowMapsDefaultSize write SetShadowMapsDefaultSize
      default TCastleSceneCore.DefaultShadowMapsDefaultSize;
    { @groupEnd }

    property InitialViewpointIndex: Cardinal
      read FInitialViewpointIndex write FInitialViewpointIndex;

    property InitialViewpointName: string
      read FInitialViewpointName write FInitialViewpointName;
  end deprecated 'instead of TCastlePrecalculatedAnimation, use TCastleScene to load animations in any format (X3D, castle-anim-frames...) and run them using methods like PlayAnimation';

const
  DefaultAnimationSmoothness = DefaultBakedAnimationSmoothness
    deprecated 'instead of this, use DefaultBakedAnimationSmoothness from X3DLoad unit';

function InternalGetAnimationSmoothness: Single;
procedure InternalSetAnimationSmoothness(const Value: Single);

{ @deprecated
  Use BakedAnimationSmoothness from X3DLoad unit instead of this. }
property AnimationSmoothness: Single
  read InternalGetAnimationSmoothness
  write InternalSetAnimationSmoothness;

procedure Register;

implementation

uses Math, X3DFields, CastleProgress, CastleLog, DateUtils,
  CastleShapes, CastleConfig;

procedure Register;
begin
  {$warnings off}
  RegisterComponents('Castle', [TCastlePrecalculatedAnimation]);
  {$warnings on}
end;

function InternalGetAnimationSmoothness: Single;
begin
  Result := BakedAnimationSmoothness;
end;

procedure InternalSetAnimationSmoothness(const Value: Single);
begin
  BakedAnimationSmoothness := Value;
end;

{ TAnimationScene ------------------------------------------------------ }

type
  TAnimationScene = class(TCastleScene)
  private
    {$warnings off}
    FParentAnimation: TCastlePrecalculatedAnimation;
    {$warnings on}
  public
    constructor CreateForAnimation(
      ARootNode: TX3DRootNode; AOwnsRootNode: boolean;
      ACustomRenderer: TGLRenderer;
      AParentAnimation: TCastlePrecalculatedAnimation;
      AStatic: boolean);
    property ParentAnimation: TCastlePrecalculatedAnimation read FParentAnimation;
    procedure DoGeometryChanged(const Change: TGeometryChange;
      LocalGeometryShape: TShape); override;
    function Shared: TCastleScene; override;
  end;

constructor TAnimationScene.CreateForAnimation(
  ARootNode: TX3DRootNode; AOwnsRootNode: boolean;
  ACustomRenderer: TGLRenderer;
  AParentAnimation: TCastlePrecalculatedAnimation;
  AStatic: boolean);
begin
  { ParentAnimation is used by DoGeometryChanged, which is virtual and
    *may* be called by ChangedAll, which *may* called by inherited constructor.
    So ParentAnimation must be set even before inherited constructor. }
  FParentAnimation := AParentAnimation;

  inherited CreateCustomRenderer(nil, ACustomRenderer);

  ShadowMaps := FParentAnimation.ShadowMaps;
  ShadowMapsDefaultSize := FParentAnimation.ShadowMapsDefaultSize;
  InitialViewpointIndex := FParentAnimation.InitialViewpointIndex;
  InitialViewpointName := FParentAnimation.InitialViewpointName;
  SetWorld(FParentAnimation.World);

  {$warnings off}
  { consciously using deprecated feature in a deprecated class }
  Static := AStatic;
  {$warnings on}

  Load(ARootNode, AOwnsRootNode);
end;

function TAnimationScene.Shared: TCastleScene;
begin
  Result := FParentAnimation.FirstScene;
end;

procedure TAnimationScene.DoGeometryChanged(const Change: TGeometryChange;
  LocalGeometryShape: TShape);
begin
  inherited;
  ParentAnimation.ValidBoundingBox := false;
end;

{ TCastlePrecalculatedAnimation ------------------------------------------------------------ }

{ About Create and CreateCustomCache relationship:

  Note that Create cannot call CreateCustomCache and depend that
  CreateCustomCache calls "inherited Create". This wouldn't be nice
  for descendants: If some TCastlePrecalculatedAnimation descendant would override "Create"
  to do his initialization, and we would create this descendant by
  CreateCustomCache --- we would miss executing descendant's constructor code.

  So only our Create may call "inherited Create".
  CreateCustomCache should always call just "Create" (virtual). }

constructor TCastlePrecalculatedAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if Cache = nil then
    FCache := GLContextCache;

  Renderer := TGLRenderer.Create(TSceneRenderingAttributes, Cache);

  FTimeLoop := true;
  FTimeBackwards := false;
  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;
  FShadowMaps := true;
  FShadowMapsDefaultSize := TCastleSceneCore.DefaultShadowMapsDefaultSize;
end;

constructor TCastlePrecalculatedAnimation.CreateCustomCache(AOwner: TComponent;
  ACache: TGLRendererContextCache);
begin
  FCache := ACache;
  Create(AOwner);
end;

destructor TCastlePrecalculatedAnimation.Destroy;
begin
  Close;
  FreeAndNil(Renderer);
  FCache := nil; // just to be safe
  inherited;
end;

procedure TCastlePrecalculatedAnimation.SetWorld(const Value: T3DWorld);
var
  I: Integer;
begin
  if World <> Value then
  begin
    inherited;
    if FScenes <> nil then
      for I := 0 to FScenes.Count - 1 do
        TAnimationScene(FScenes[I]).SetWorld(Value);
  end;
end;

procedure TCastlePrecalculatedAnimation.LoadCore(
  GetKeyNodeWithTime: TGetKeyNodeWithTime;
  KeyNodesCount: Cardinal;
  AOwnsFirstRootNode: boolean;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single);
var
  SceneStatic: boolean;
  BakedAnimation: TNodeInterpolator.TBakedAnimation;
  I: Integer;
begin
  Close;

  FOwnsFirstRootNode := AOwnsFirstRootNode;

  { We want all the scenes to be dynamic only when
    (TryFirstSceneDynamic and (FScenes.Count = 1)).
    We don't know yet FScenes.Count, but FScenes.Count = 1 is quite special:
    it only (if and only if) occurs if KeyNodesCount = 1. }
  SceneStatic := not (TryFirstSceneDynamic and (KeyNodesCount = 1));

  BakedAnimation := TNodeInterpolator.BakeToSequence(GetKeyNodeWithTime, KeyNodesCount,
    ScenesPerTime, EqualityEpsilon);
  try
    FTimeBegin := BakedAnimation.TimeBegin;
    FTimeEnd := BakedAnimation.TimeEnd;

    { calculate FScenes }
    FScenes := TCastleSceneList.Create(false);
    FScenes.Count := BakedAnimation.Nodes.Count;
    for I := 0 to FScenes.Count - 1 do
    begin
      if (I > 0) and (BakedAnimation.Nodes[I] = BakedAnimation.Nodes[I - 1]) then
        { In this case don't waste memory, only reuse
          LastSceneRootNode. Actually, just copy last scene.
          This way we have a series of the same instances of TCastleScene
          along the FScenes list. When freeing FScenes, we will be smart and
          avoid deallocating the same pointer twice. }
        FScenes[I] := FScenes[I - 1] else
        FScenes[I] := TAnimationScene.CreateForAnimation(
          BakedAnimation.Nodes[I] as TX3DRootNode,
          (I <> 0) or OwnsFirstRootNode, Renderer, Self, SceneStatic);
    end;

  finally FreeAndNil(BakedAnimation) end;

  FLoaded := true;
end;

procedure TCastlePrecalculatedAnimation.Load_GetKeyNodeWithTime(const Index: Cardinal;
  out KeyNode: TX3DRootNode; out Time: Single);
begin
  KeyNode := Load_KeyNodes[Index] as TX3DRootNode;
  Time := Load_KeyTimes[Index];
end;

procedure TCastlePrecalculatedAnimation.Load(
  KeyNodes: TX3DNodeList;
  AOwnsFirstRootNode: boolean;
  AKeyTimes: TSingleList;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single);
begin
  Assert(KeyNodes.Count = AKeyTimes.Count);
  Load_KeyNodes := KeyNodes;
  Load_KeyTimes := AKeyTimes;

  LoadCore(@Load_GetKeyNodeWithTime, KeyNodes.Count,
    AOwnsFirstRootNode, ScenesPerTime, EqualityEpsilon);
end;

procedure TCastlePrecalculatedAnimation.LoadFromEvents_GetKeyNodeWithTime(
  const Index: Cardinal;
  out KeyNode: TX3DRootNode; out Time: Single);
begin
  Time := LoadFromEvents_TimeBegin;
  if LoadFromEvents_ScenesPerTime <> 0 then
    Time += Index / LoadFromEvents_ScenesPerTime;

  if Index = 0 then
    LoadFromEvents_Scene.ResetTime(Time) else
    LoadFromEvents_Scene.SetTime(Time);

  KeyNode := LoadFromEvents_Scene.RootNode.DeepCopy as TX3DRootNode;
end;

procedure TCastlePrecalculatedAnimation.LoadFromEvents_GetKeyNodeWithTime_Progress(
  const Index: Cardinal;
  out KeyNode: TX3DRootNode; out Time: Single);
begin
  LoadFromEvents_GetKeyNodeWithTime(Index, KeyNode, Time);
  Progress.Step;
end;

procedure TCastlePrecalculatedAnimation.LoadFromEvents(
  RootNode: TX3DRootNode;
  AOwnsRootNode: boolean;
  const ATimeBegin, ATimeEnd: Single;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  const ProgressTitle: string);
var
  Count: Cardinal;
begin
  LoadFromEvents_ScenesPerTime := ScenesPerTime;
  LoadFromEvents_TimeBegin := ATimeBegin;
  LoadFromEvents_Scene := TCastleSceneCore.Create(nil);
  try
    LoadFromEvents_Scene.Load(RootNode, AOwnsRootNode);

    Count := Max(1, Round((ATimeEnd - ATimeBegin) * ScenesPerTime));

    LoadFromEvents_Scene.ProcessEvents := true;

    if ProgressTitle <> '' then
    begin
      Progress.Init(Count, ProgressTitle);
      try
        LoadCore(@LoadFromEvents_GetKeyNodeWithTime_Progress, Count,
          true, 0, EqualityEpsilon);
      finally
        Progress.Fini;
      end;
    end else
    begin
      LoadCore(@LoadFromEvents_GetKeyNodeWithTime, Count,
        true, 0, EqualityEpsilon);
    end;

    { Although LoadCore sets FTimeEnd already, it may be a little
      smaller than ATimeEnd if ScenesPerTime is very small.
      Last scene generated by LoadFromEvents_GetKeyNodeWithTime
      will not necessarily "hit" exactly TimeEnd.
      In particular, when ScenesPerTime = 0, LoadCore will just set
      FTimeEnd to TimeBegin...

      Since we guarantee in the interface that FTimeEnd will be exactly
      equal to ATimeEnd after LoadFromEvents, we fix it here. }

    FTimeEnd := ATimeEnd;
  finally FreeAndNil(LoadFromEvents_Scene) end;
end;

procedure TCastlePrecalculatedAnimation.LoadStatic(
  RootNode: TX3DNode;
  AOwnsRootNode: boolean);
var
  KeyNodes: TX3DNodeList;
  AKeyTimes: TSingleList;
begin
  KeyNodes := TX3DNodeList.Create(false);
  try
    AKeyTimes := TSingleList.Create;
    try
      KeyNodes.Add(RootNode);
      AKeyTimes.Add(0);
      Load(KeyNodes, AOwnsRootNode, AKeyTimes, 1, 0.0);
    finally FreeAndNil(AKeyTimes) end;
  finally FreeAndNil(KeyNodes) end;
end;

procedure TCastlePrecalculatedAnimation.LoadFromFile(const URL: string;
  const AllowStdIn, LoadTime: boolean);
var
  Times: TSingleList;
  KeyNodes: TX3DNodeList;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  NewTimeLoop, NewTimeBackwards: boolean;
begin
  Times := TSingleList.Create;
  KeyNodes := TX3DNodeList.Create(false);
  try
    {$warnings off}
    { deliberately using deprecated function in a deprecated unit }
    Load3DSequence(URL, AllowStdIn,
      KeyNodes, Times, ScenesPerTime, EqualityEpsilon,
      NewTimeLoop, NewTimeBackwards);
    {$warnings on}

    Load(KeyNodes, true, Times, ScenesPerTime, EqualityEpsilon);

    if LoadTime then
    begin
      TimeLoop := NewTimeLoop;
      TimeBackwards := NewTimeBackwards;
    end;

    WritelnLog('PrecalculatedAnimation', 'Loaded %s: %d precalculated scenes',
      [URL, ScenesCount]);
  finally
    FreeAndNil(Times);
    FreeAndNil(KeyNodes);
  end;
end;

procedure TCastlePrecalculatedAnimation.Close;
var
  I: Integer;
begin
  { This is called from destructor, so this must always check whether
    things are <> nil before trying to free them. }

  GLContextClose;

  if FScenes <> nil then
  begin
    { Although FScenes.Count should always be > 0 when FScenes allocated,
      this is a destructor so we must handle various abnormal situations
      if we exit with exception. }
    if FScenes.Count <> 0 then
    begin
      { Now we must note that we may have a sequences of the same scenes
        on FScenes. So we must deallocate smartly, to avoid deallocating
        the same pointer more than once. }
      for I := 0 to FScenes.Count - 2 do
      begin
        if FScenes[I] = FScenes[I+1] then
          FScenes[I] := nil { set to nil, just for safety } else
          FPGObjectList_FreeAndNilItem(FScenes, I);
      end;
      FPGObjectList_FreeAndNilItem(FScenes, FScenes.Count - 1);
    end;

    FreeAndNil(FScenes);
  end;

  ValidBoundingBox := false;

  FLoaded := false;
end;

function TCastlePrecalculatedAnimation.GetScenes(I: Integer): TCastleScene;
begin
  Result := FScenes[I];
end;

function TCastlePrecalculatedAnimation.ScenesCount: Integer;
begin
  if Loaded then
    Result := FScenes.Count else
    Result := 0;
end;

function TCastlePrecalculatedAnimation.FirstScene: TCastleScene;
begin
  Result := FScenes.First;
end;

function TCastlePrecalculatedAnimation.LastScene: TCastleScene;
begin
  Result := FScenes.Last;
end;

procedure TCastlePrecalculatedAnimation.PrepareResources(Options: TPrepareResourcesOptions;
  ProgressStep: boolean; BaseLights: TAbstractLightInstancesList);
var
  I: Integer;
begin
  if not Loaded then Exit;

  for I := 0 to FScenes.Count - 1 do
  begin
    FScenes[I].PrepareResources(Options, false, BaseLights);
    if ProgressStep then
      Progress.Step;
  end;
end;

function TCastlePrecalculatedAnimation.PrepareResourcesSteps: Cardinal;
begin
  Result := ScenesCount;
end;

procedure TCastlePrecalculatedAnimation.FreeResources(Resources: TSceneFreeResources);
var
  I: Integer;
begin
  for I := 0 to FScenes.Count - 1 do
    FScenes[I].FreeResources(Resources);
end;

procedure TCastlePrecalculatedAnimation.GLContextClose;
{ Note that this is called from destructor, so we must be extra careful
  here and check is everything <> nil before freeing it. }
begin
  if FScenes <> nil then
    FScenes.GLContextClose;

  if Renderer <> nil then
    Renderer.UnprepareAll;
end;

function TCastlePrecalculatedAnimation.TimeDuration: Single;
begin
  Result := TimeEnd - TimeBegin;
end;

function TCastlePrecalculatedAnimation.TimeDurationWithBack: Single;
begin
  Result := TimeDuration;
  if TimeBackwards then
    Result *= 2;
end;

function TCastlePrecalculatedAnimation.Scene(const Time: Single): TCastleScene;
begin
  Result := Scene(Time, TimeLoop);
end;

function TCastlePrecalculatedAnimation.Scene(const Time: Single;
  const Loop: boolean): TCastleScene;
var
  SceneNumber: Integer;
  DivResult: SmallInt;
  ModResult: Word;
begin
  if FScenes.Count = 1 then
  begin
    { In this case TimeBegin = TimeEnd, so it's better to not perform
      any MapRange(Time, TimeBegin, TimeEnd, ...) calculation here
      and just treat this as a special case. }
    SceneNumber := 0;
  end else
  begin
    { I use FScenes.Count, not FScenes.Count - 1 as the highest range value.
      This is critical. On the short range (not looping), it may seem
      that FScenes.Count - 1 is more appropriate, since the last scene
      corresponds exactly to TimeEnd. But that's not good for looping:
      in effect float range TimeDuration would contain one scene less,
      and so when looking at large Time values, the scenes are slightly shifted
      within time.

      This causes problems when code relies on the meaning of some time
      values. E.g. if TimeBegin = 0, you expect that Time = k * TimeEnd,
      for any k, will result in the LastScene generated (assuming
      backwards is @true). This is needed for tricks like smooth animations
      concatenation, see "the rift" in RiftCreatures unit.

      When using FScenes.Count - 1, we would break this, as scenes are shifted
      by one in each range. }
    SceneNumber := Floor(MapRange(Time, TimeBegin, TimeEnd, 0, FScenes.Count));

    DivUnsignedMod(SceneNumber, FScenes.Count, DivResult, ModResult);

    if Loop then
    begin
      if TimeBackwards and Odd(DivResult) then
        SceneNumber := FScenes.Count - 1 - ModResult else
        SceneNumber := ModResult;
    end else
    begin
      if TimeBackwards then
      begin
        if (DivResult < 0) or (DivResult > 1) then
          SceneNumber := 0 else
        if DivResult = 1 then
          SceneNumber := FScenes.Count - 1 - ModResult;
          { else DivResult = 0, so SceneNumber is already correct }
      end else
      begin
        if DivResult < 0 then
          SceneNumber := 0 else
        if DivResult > 0 then
          SceneNumber := FScenes.Count - 1;
      end;
    end;
  end;

  Result := FScenes[SceneNumber];
end;

function TCastlePrecalculatedAnimation.Attributes: TSceneRenderingAttributes;
begin
  Result := TSceneRenderingAttributes(Renderer.Attributes);
end;

function TCastlePrecalculatedAnimation.BoundingBox: TBox3D;

  procedure ValidateBoundingBox;
  var
    I: Integer;
  begin
    FBoundingBox := FScenes[0].BoundingBox;
    for I := 1 to FScenes.Count - 1 do
      FBoundingBox.Add(FScenes[I].BoundingBox);
    ValidBoundingBox := true;
  end;

begin
  if Loaded and GetExists then
  begin
    if not ValidBoundingBox then
      ValidateBoundingBox;
    Result := FBoundingBox;
  end else
    Result := EmptyBox3D;
end;

procedure TCastlePrecalculatedAnimation.BeforeNodesFree;
var
  I: Integer;
begin
  for I := 0 to FScenes.Count - 1 do
    FScenes[I].BeforeNodesFree;
end;

procedure TCastlePrecalculatedAnimation.ChangedAll;
var
  I: Integer;
begin
  for I := 0 to FScenes.Count - 1 do
    FScenes[I].ChangedAll;
  ValidBoundingBox := false;
end;

function TCastlePrecalculatedAnimation.InfoBoundingBox: string;
var
  BBox: TBox3D;
begin
  BBox := BoundingBox;
  Result := 'Bounding box (of the whole animation) : ' + BBox.ToNiceStr;
  if not BBox.IsEmpty then
  begin
    Result += ', average size : ' + FloatToNiceStr(BBox.AverageSize);
  end;
  Result += NL;
end;

function TCastlePrecalculatedAnimation.Info(
  ATriangleVerticesCounts,
  ABoundingBox,
  AManifoldAndBorderEdges: boolean): string;
begin
  Result := '';

  if ATriangleVerticesCounts then
  begin
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result += FirstScene.InfoTriangleVerticesCounts;
    {$warnings on}
  end;

  if ABoundingBox then
  begin
    if Result <> '' then Result += NL;
    { We do not call FirstScene.InfoBoundingBox here, instead we want
      to get full bounding box of the animation. }
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result += InfoBoundingBox;
    {$warnings on}
  end;

  if AManifoldAndBorderEdges then
  begin
    if Result <> '' then Result += NL;
    {$warnings off}
    { deliberately using deprecated function in another deprecated function }
    Result += FirstScene.InfoManifoldAndBorderEdges;
    {$warnings on}
  end;
end;

procedure TCastlePrecalculatedAnimation.SetOwnsFirstRootNode(const Value: boolean);
var
  I: Integer;
begin
  if Value <> FOwnsFirstRootNode then
  begin
    FOwnsFirstRootNode := Value;
    if FScenes <> nil then
    begin
      Assert(FScenes.Count > 0, 'When the Scenes <> nil, anim is loaded so should always have at least one scene');

      { OwnsFirstRootNode corresponds to FScenes[0].OwnsRootNode.
        But note that it's allowed to have duplicates of scenes
        in one consecutive range of FScenes. So we have to iterate over
        the first FScenes (while they are equal to FScenes[0]). }

      for I := 0 to FScenes.Count - 1 do
      begin
        if FScenes[I] = FScenes[0] then
          FScenes[I].OwnsRootNode := Value else
          Break;
      end;
    end;
  end;
end;

function TCastlePrecalculatedAnimation.Press(const Event: TInputPressRelease): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].Press(Event) else
    Result := false;
end;

function TCastlePrecalculatedAnimation.Release(const Event: TInputPressRelease): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].Release(Event) else
    Result := false;
end;

procedure TCastlePrecalculatedAnimation.ResetTimeAtLoad(const ForceTimeOrigin: boolean = false);

  function TimeOriginAtLoad: boolean;
  var
    N: TNavigationInfoNode;
  begin
    Result := false;

    if Loaded then
    begin
      N := Scenes[0].NavigationInfoStack.Top;
      if (N <> nil) and
         (N is TKambiNavigationInfoNode) then
        Result := TKambiNavigationInfoNode(N).FdTimeOriginAtLoad.Value;
    end;
  end;

begin
  if (ScenesCount > 1) or ForceTimeOrigin or TimeOriginAtLoad then
  begin
    FTimeAtLoad := 0.0;
    ResetTime(TimeBegin);
  end else
  begin
    FTimeAtLoad := DateTimeToUnix(Now);
    ResetTime(TimeAtLoad);
  end;
end;

procedure TCastlePrecalculatedAnimation.ResetTime(const NewValue: TFloatTime);
begin
  FTime := NewValue;

  { Ignored when SceneAnimation.ScenesCount <> 1, as scenes' ProcessEvents
    is always false then and Time wouldn't have much sense anyway. }
  if ScenesCount = 1 then
    Scenes[0].ResetTime(NewValue);
end;

procedure TCastlePrecalculatedAnimation.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  OldTime: TFloatTime;
begin
  inherited;

  { Ignore Update calls when SecondsPassed is precisely zero
    (this may happen, and is good, see TFramesPerSecond.ZeroNextSecondsPassed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway. }

  if Loaded and TimePlaying and (SecondsPassed <> 0) then
  begin
    OldTime := FTime;
    FTime += TimePlayingSpeed * SecondsPassed;

    { When ScenesCount = 1, it's sensible for single scene to receive
      events, to increase it's time. Note that TCastleSceneCore.SetTime
      will signal when redisplay will be needed (something visible changed),
      we don't have to worry about it.

      We call Scenes[0].SetTime direcly, instead of calling Scenes[0].Update.
      This way we do not have to worry to set scene's initial time, TimePlaying,
      TimePlayingSpeed to our values. }
    if ScenesCount = 1 then
      Scenes[0].SetTime(Time);

    { Call VisibleChangeHere only if the displayed animation frame actually changed.
      This way, we avoid wasting CPU cycles if the loaded scene is actually
      still, or if the animation stopped running. }
    if (Scene(OldTime) <> Scene(Time)) then
      VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);
  end;
end;

function TCastlePrecalculatedAnimation.CurrentScene: TCastleScene;
begin
  Result := Scene(Time);
end;

procedure TCastlePrecalculatedAnimation.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if Loaded and GetExists then
    CurrentScene.Render(Frustum, Params);
end;

procedure TCastlePrecalculatedAnimation.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
  if Loaded and GetExists and CastShadowVolumes then
    CurrentScene.RenderShadowVolume(ShadowVolumeRenderer,
      ParentTransformIsIdentity, ParentTransform);
end;

{ We have to typecast TAnimationScene to get access to it's protected methods.
  Instead of macros, this could be solved by making TAnimationScene an internal
  class of TCastlePrecalculatedAnimation, but only for new FPC versions. }
{$define FirstAnimScene := TAnimationScene(FirstScene)}
{$define LastAnimScene := TAnimationScene(LastScene)}

function TCastlePrecalculatedAnimation.HeightCollision(
  const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;

  procedure MakeScene(Scene: TAnimationScene);
  var
    NewResult: boolean;
    NewAboveHeight: Single;
    NewAboveGround: PTriangle;
  begin
    NewResult := Scene.HeightCollision(
      Position, GravityUp, TrianglesToIgnoreFunc, NewAboveHeight, NewAboveGround);

    if NewAboveHeight < AboveHeight then
    begin
      Result := NewResult;
      AboveHeight := NewAboveHeight;
      AboveGround := NewAboveGround;
    end;
  end;

begin
  Result := false;
  AboveHeight := MaxSingle;
  AboveGround := nil;

  if Loaded and GetCollides then
  begin
    MakeScene(FirstAnimScene);
    if CollisionUseLastScene then
      MakeScene(LastAnimScene);
  end;
end;

function TCastlePrecalculatedAnimation.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  if Loaded and GetCollides then
  begin
    Result := FirstAnimScene.MoveCollision(OldPos, ProposedNewPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);

    { On the LastScene use MoveCollision without wall sliding.
      Reason: see T3DList.MoveCollision implementation. }

    if Result and CollisionUseLastScene then
    begin
      Result := LastAnimScene.MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
    end;
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

function TCastlePrecalculatedAnimation.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := (not Loaded) or (not GetCollides) or
    (FirstAnimScene.MoveCollision(OldPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc) and
       ( (not CollisionUseLastScene) or
         LastAnimScene.MoveCollision(OldPos, NewPos,
           IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc) ));
end;

function TCastlePrecalculatedAnimation.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  Result := Loaded and
    (GetCollides or (ALineOfSight and GetExists)) and
    ( FirstAnimScene.SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight) or
      (CollisionUseLastScene and
        (LastAnimScene.SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight)))
    );
end;

function TCastlePrecalculatedAnimation.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Loaded and GetCollides and
    ( FirstAnimScene.SphereCollision(Pos, Radius, TrianglesToIgnoreFunc) or
      (CollisionUseLastScene and
        (LastAnimScene.SphereCollision(Pos, Radius, TrianglesToIgnoreFunc)))
    );
end;

function TCastlePrecalculatedAnimation.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Loaded and GetCollides and
    ( FirstAnimScene.BoxCollision(Box, TrianglesToIgnoreFunc) or
      (CollisionUseLastScene and
        (LastAnimScene.BoxCollision(Box, TrianglesToIgnoreFunc)))
    );
end;

function TCastlePrecalculatedAnimation.RayCollision(
  const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  NewResult: TRayCollision;
  NewNode, PreviousNode: PRayCollisionNode;
begin
  Result := nil;

  if Loaded and GetExists then
  begin
    Result := FirstAnimScene.RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc);

    if CollisionUseLastScene then
    begin
      { try the same thing on LastScene }
      NewResult := LastAnimScene.RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc);

      if NewResult <> nil then
      begin
        if (Result = nil) or (NewResult.Distance < Result.Distance) then
        begin
          SysUtils.FreeAndNil(Result);
          Result := NewResult;
        end else
          FreeAndNil(NewResult);
      end;
    end;

    if Result <> nil then
    begin
      NewNode := Result.Add;
      PreviousNode := @(Result.List^[Result.Count - 2]);
      NewNode^.Item := Self;
      NewNode^.Point := PreviousNode^.Point;
      NewNode^.Triangle := nil;
      NewNode^.RayOrigin := PreviousNode^.RayOrigin;
      NewNode^.RayDirection := PreviousNode^.RayDirection;
    end;
  end;
end;

procedure TCastlePrecalculatedAnimation.UpdateGeneratedTextures(
  const RenderFunc: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const OriginalViewport: TRectangle);
begin
  inherited;
  if Loaded then
    CurrentScene.UpdateGeneratedTextures(RenderFunc, ProjectionNear, ProjectionFar,
      OriginalViewport);
end;

procedure TCastlePrecalculatedAnimation.VisibleChangeNotification(const Changes: TVisibleChanges);
begin
  inherited;
  if Loaded then
    CurrentScene.VisibleChangeNotification(Changes);
end;

procedure TCastlePrecalculatedAnimation.CameraChanged(ACamera: TCamera);
begin
  inherited;
  if Loaded then
    CurrentScene.CameraChanged(ACamera);
end;

function TCastlePrecalculatedAnimation.Dragging: boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Loaded then
    Result := CurrentScene.Dragging;
end;

procedure TCastlePrecalculatedAnimation.SetShadowMaps(const Value: boolean);
var
  I: Integer;
begin
  if Value <> FShadowMaps then
  begin
    FShadowMaps := Value;
    if FScenes <> nil then
    begin
      for I := 0 to FScenes.Count - 1 do
        FScenes[I].ShadowMaps := Value;
    end;
  end;
end;

procedure TCastlePrecalculatedAnimation.SetShadowMapsDefaultSize(const Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FShadowMapsDefaultSize then
  begin
    FShadowMapsDefaultSize := Value;
    if FScenes <> nil then
    begin
      for I := 0 to FScenes.Count - 1 do
        FScenes[I].ShadowMapsDefaultSize := Value;
    end;
  end;
end;

end.
