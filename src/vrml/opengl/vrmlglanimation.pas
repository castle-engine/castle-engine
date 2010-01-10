{
  Copyright 2006-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ TVRMLGLAnimation class. }
unit VRMLGLAnimation;

interface

uses SysUtils, Classes, VRMLNodes, VRMLOpenGLRenderer, VRMLScene, VRMLGLScene,
  KambiUtils, Boxes3d, KambiClassUtils, VRMLAnimation, KeysMouse, Navigation,
  KambiTimeUtils;

{$define read_interface}

type
  TAnimationChangeLoadParametersFunc = procedure (
    var ScenesPerTime: Cardinal;
    var Optimization: TGLRendererOptimization;
    var EqualityEpsilon: Single) of object;

  TGetRootNodeWithTime = procedure (const Index: Cardinal;
    out RootNode: TVRMLNode; out Time: Single) of object;

  { This is a "precalculated" animation of VRML model done by
    interpolating between any number of model states.

    After constructing an object of this class, you must actually
    load it's animation by calling Load or LoadFromFile or LoadFromVRMLEvents
    etc.

    When loading you must provide one or more VRML models with
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
    just a still result, i.e. resulting TVRMLGLAnimation will be just
    a wrapper around single TVRMLGLScene instance.

    For more information see the "VRML engine documentation",
    [http://vrmlengine.sourceforge.net/vrml_engine_doc/].
    Specifically the section
    "Non-interactive precalculated animation: TVRMLGLAnimation",
    [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.animation_precalculated.html]. }
  TVRMLGLAnimation = class(TVRMLAnimation)
  private
    FScenes: TVRMLGLScenesList;
    function GetScenes(I: Integer): TVRMLGLScene;
  private
    Renderer: TVRMLOpenGLRenderer;
    FTimeBegin, FTimeEnd: Single;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FOwnsFirstRootNode: boolean;
    FLoaded: boolean;
    FTimePlaying: boolean;
    FTimePlayingSpeed: Single;
    FWorldTimeAtLoad: TKamTime;
    FWorldTime: TKamTime;

    ValidBoundingBoxSum: boolean;
    FBoundingBoxSum: TBox3d;
    FOptimization: TGLRendererOptimization;
    FNavigator: TNavigator;

    function GetBackgroundSkySphereRadius: Single;
    procedure SetBackgroundSkySphereRadius(const Value: Single);
    function GetAngleOfViewX: Single;
    procedure SetAngleOfViewX(const Value: Single);
    function GetAngleOfViewY: Single;
    procedure SetAngleOfViewY(const Value: Single);
    procedure SetNavigator(const Value: TNavigator);
    procedure SetOptimization(const Value: TGLRendererOptimization);

    function InfoBoundingBoxSum: string;
  private
    FWalkProjectionNear: Single;
    FWalkProjectionFar : Single;

    { Helpers for Load implementation. }
    Load_RootNodes: TVRMLNodesList;
    Load_Times: TDynSingleArray;
    procedure Load_GetRootNodeWithTime(const Index: Cardinal;
      out RootNode: TVRMLNode; out Time: Single);
  private
    { Helpers for LoadFromVRMLEvents implementation. }
    LoadFromVRMLEvents_TimeBegin: Single;
    LoadFromVRMLEvents_Scene: TVRMLScene;
    LoadFromVRMLEvents_ScenesPerTime: Cardinal;
    procedure LoadFromVRMLEvents_GetRootNodeWithTime(const Index: Cardinal;
      out RootNode: TVRMLNode; out Time: Single);
    procedure LoadFromVRMLEvents_GetRootNodeWithTime_Progress(
      const Index: Cardinal;
      out RootNode: TVRMLNode; out Time: Single);

    procedure SetOwnsFirstRootNode(const Value: boolean);
  protected
    { Internal version of @link(Load) routines, feasible to load
      from both ready RootNodes array and to automatically generate RootNodes
      on the fly.

      GetRootNodeWithTime will be called with indexes from 0 to RootNodesCount - 1.
      It's guaranteed that it will be called in this order (from 0 upwards to
      RootNodesCount - 1) and will be called exactly once for each index.
      So it's safe to e.g. create RootNode with some costly operation there.

      Note that RootNode passed to GetRootNodeWithTime becomes owned by
      this class. Well, you can get control over only the first one,
      by AOwnsFirstRootNode, but you cannot free it anyway while this is loaded.

      See @link(Load) for more information, including the meaning of
      EqualityEpsilon. }
    procedure LoadCore(
      GetRootNodeWithTime: TGetRootNodeWithTime;
      RootNodesCount: Cardinal;
      AOwnsFirstRootNode: boolean;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single);
  public
    constructor Create(AOwner: TComponent); override;

    { Constructor that allows you to pass your own Cache instance. }
    constructor CreateCustomCache(AOwner: TComponent;
      ACache: TVRMLOpenGLRendererContextCache);

    destructor Destroy; override;

    { This actually loads the animation scenes.
      You must call this (or some other loading routine like LoadFromFile)
      before you do almost anything with this object.
      Loaded changes to @true after calling this.

      Animation is loaded with current @link(Optimization) value.

      @param(RootNodes
        Models describing the "predefined" frames of animation.

        For all nodes except the first: They are @italic(always)
        owned by this class --- that's needed,
        because actually we may do some operations on these models when
        building animation (including even freeing some RootNodes,
        if we will find that they are equivalent to some other RootNodes).
        They all must point to different objects.

        You must supply at least one item here (you cannot make an animation
        from 0 items).)

      @param(Times Array specifying the point of time for each "predefined"
        frame. Length of this array must equal to length of RootNodes array.)

      @param(ScenesPerTime
        This says how many scenes will be used for period of time equal to 1.0.
        This will determine Scenes.Count.
        RootNodes[0] always takes Scenes[0] and RootNodes[High(RootNodes)]
        always takes Scenes[Scenes.High].

        Note that if we will find that some nodes along the way are
        exactly equal, we may drop scenes count between --- because if they are
        both equal, we can simply render the same scene for some period
        of time. This is an optimization, and you shouldn't notice it at all,
        since rendeting will be the same (but less memory-consuming).

        Special value ScenesPerTime = 0 means that you want to have only the
        RootNodes you explicitly passed in the scene, not more.
        No more intermediate scenes will ever be created.
        This creates a trivial animation that suddenly jumps from
        one RootNode to the next at specified times. It may be useful if you
        already have generated a lot of RootNodes, densely distributed
        over time, and you don't need TVRMLGLAnimation to insert any more
        scenes.)

      @param(EqualityEpsilon
        This will be used for comparing fields, to decide if two fields
        (and, consequently, nodes) are equal. It will be simply
        passed to TVRMLField.Equals.

        You can pass here 0 to use exact comparison, but it's
        adviced to use here something > 0. Otherwise we could waste
        display list memory (and loading time) for many frames of the
        same node that are in fact equal.)
    }
    procedure Load(
      RootNodes: TVRMLNodesList;
      AOwnsFirstRootNode: boolean;
      ATimes: TDynSingleArray;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single);

    { Load precalculated animation by playing a single VRML file with
      events (interpolators, TimeSensor and such working).
      Conceptually, this "records" interactive animation stored in VRML file
      into TVRMLGLAnimation precalculated animation.

      ATimeBegin, ATimeEnd tell what time slice should be recorded.
      They will also set @link(TimeBegin) and @link(TimeEnd) properties.

      @param(ScenesPerTime
        tells with what density should the animation be recorded.
        See @link(Load) for ScenesPerTime, EqualityEpsilon precise documentation.
        Note that special value ScenesPerTime = 0 is interpreted here as
        "record only one, initial frame".)

      @param(ProgressTitle When <> '' we will use Progress.Init, Step, Fini
        to display nice progress of operation.) }
    procedure LoadFromVRMLEvents(
      RootNode: TVRMLNode;
      AOwnsRootNode: boolean;
      const ATimeBegin, ATimeEnd: Single;
      ScenesPerTime: Cardinal;
      const EqualityEpsilon: Single;
      const ProgressTitle: string);

    { Load a dumb animation that consists of only one frame (so actually
      there's  no animation, everything is static).

      This just calls @link(Load) with parameters such that
      @orderedList(
        @item(RootNodes list contains one specified node)
        @item(Times contain only one item 0.0)
        @item(ScenesPerTime and EqualityEpsilon have some unimportant
          values --- they are not meaningfull when you have only one scene)
      )

      This is usefull when you know that you have a static scene,
      but still you want to treat it as TVRMLGLAnimation. }
    procedure LoadStatic(
      RootNode: TVRMLNode;
      AOwnsRootNode: boolean);

    { This loads TVRMLGLAnimation by loading it's parameters
      (models to use, times to use etc.) from given file.
      File format is described on
      [http://vrmlengine.sourceforge.net/kanim_format.php].

      This changes Optimization, TimeLoop and such,
      based on their values in Kanim file.

      You can change some of the loaded parameters by providing ChangeLoadParameters
      callback (you can provide @nil, if you don't want to change them).
      ChangeLoadParameters callback is the only way to change some animation rendering
      parameters, like ScenesPerTime --- after LoadFromFile finished,
      animation is fully loaded and
      these parameters cannot be changed anymore.
      (Optimization may be changed after loading, but still it's more efficient
      to change it before loading.)

      If you need more control than simple ChangeLoadParameters callback,
      you should use something more flexible (and less comfortable to use)
      like LoadFromFileToVars class procedure.

      Note that you can always change TimeLoop and TimeBackwards --- since these
      properties are writeable at any time.

      @link(Loaded) property changes to @true after calling this. }
    procedure LoadFromFile(const FileName: string;
      ChangeLoadParameters: TAnimationChangeLoadParametersFunc = nil);

    { This releases all resources allocared by Load (or LoadFromFile).
      @link(Loaded) property changes to @false after calling this.

      It's safe to call this even if @link(Loaded) is already @false --- then
      this will do nothing. }
    procedure Close;

    property Loaded: boolean read FLoaded;

    { Is the RootNode in first scene owned by this TVRMLGLAnimation instance?
      If yes, it will be freed at closing the animation.
      Otherwise, you are responsible for freeing it yourself
      (but you cannot do this while animation is loaded, anyway). }
    property OwnsFirstRootNode: boolean
      read FOwnsFirstRootNode write SetOwnsFirstRootNode;

    { You can read anything from Scenes below. But you cannot set some
      things: don't set their scenes Attributes properties.
      Use only our @link(Attributes). }
    property Scenes[I: Integer]: TVRMLGLScene read GetScenes;
    function ScenesCount: Integer;

    { Just a shortcut for Scenes[0]. }
    function FirstScene: TVRMLGLScene;

    { Just a shortcut for Scenes[ScenesCount - 1]. }
    function LastScene: TVRMLGLScene;

    { Prepare all scenes for rendering. This just calls
      PrepareRender(...) for all Scenes.

      If ProgressStep then it will additionally call Progress.Step after
      preparing each scene (it will call it ScenesCount times).

      If prManifoldAndBorderEdges is included, then actually a special memory
      (and prepare time) optimization will be used: only the first scene will
      have actually prepared prManifoldAndBorderEdges. The other scenes will
      just share the same ManifoldEdges and BorderEdges instances, by
      TVRMLScene.ShareManifoldAndBorderEdges method. }
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions;
      ProgressStep: boolean);

    { This calls FreeResources for all scenes, it's useful if you know
      that you will not need some allocated resources anymore and you
      want to conserve memory use.

      See TVRMLSceneFreeResource documentation for a description of what
      are possible resources to free.

      Note in case you pass frRootNode: the first scene has OwnsRootNode
      set to what you passed as OwnsFirstRootNode. Which means that
      if you passed OwnsFirstRootNode = @true, then frRootNode will @bold(not
      free) the initial RootNodes[0]. }
    procedure FreeResources(Resources: TVRMLSceneFreeResources);

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
    }
    function SceneFromTime(const Time: Single): TVRMLGLScene;

    { Appropriate scene from @link(Scenes) based on given WorldTime.
      This is just a shortcut for SceneFromTime(WorldTime),
      useful if you track animation time in our WorldTime property. }
    function CurrentScene: TVRMLGLScene;

    { Attributes controlling rendering.
      See TVRMLSceneRenderingAttributes and TVRMLRenderingAttributes
      for documentation of properties.

      You can change properties of this
      object at any time, but beware that some changes may force
      time-consuming regeneration of some things (like OpenGL display lists)
      in the nearest Render of the scenes.
      So explicitly calling PrepareRender may be useful after
      changing these Attributes.

      Note that Attributes may be accessed and even changed when the scene
      is not loaded (e.g. before calling Load / LoadFromFile).
      Also, Attributes are preserved between various animations loaded. }
    function Attributes: TVRMLSceneRenderingAttributes;

    { This simply returns FirstScene.ManifoldEdges.
      Since all scenes in the animation must have exactly the same
      structure, we know that this ManifoldEdges is actually good
      for all scenes within this animation. }
    function ManifoldEdges: TDynManifoldEdgeArray;

    { This simply returns FirstScene.BorderEdges.
      Like ManifoldEdges: all scenes in the animation must have exactly the same
      structure, we know that this BorderEdges is actually good
      for all scenes within this animation. }
    function BorderEdges: TDynBorderEdgeArray;

    { Calls ShareManifoldAndBoderEdges on all scenes within this
      animation. This is useful if you already have ManifoldEdges and BorderEdges,
      and you somehow know that it's good also for this scene. }
    procedure ShareManifoldAndBorderEdges(
      ManifoldShared: TDynManifoldEdgeArray;
      BorderShared: TDynBorderEdgeArray);

    { The sum of bounding boxes of all animation frames.

      Result of this function is cached, which means that it usually returns
      very fast. But you have to call ChangedAll when you changed something
      inside Scenes[] using some direct Scenes[].RootNode operations,
      to force recalculation of this box. }
    function BoundingBoxSum: TBox3d;

    { Call this when you changed something
      inside Scenes[] using some direct Scenes[].RootNode operations.
      This calls TVRMLGLScene.ChangedAll on all Scenes[]
      and invalidates some cached things inside this class. }
    procedure ChangedAll;

    { Returns some textual info about this animation.
      Similar to TVRMLGLScene.Info. }
    function Info(
      ATriangleVerticesCounts,
      ABoundingBox,
      AManifoldAndBorderEdges: boolean): string;

    { Write contents of all VRML "Info" nodes.
      Also write how many Info nodes there are in the scene.

      Actually, this just calls WritelnInfoNodes on the FirstScene
      --- thanks to the knowledge that all info nodes in all scenes
      must be equal, since strings cannot be interpolated. }
    procedure WritelnInfoNodes;

    { Common BackgroundSkySphereRadius of all scenes,
      see TVRMLGLScene.BackgroundSkySphereRadius.

      This reads simply FirstScene.BackgroundSkySphereRadius
      and sets BackgroundSkySphereRadius for all scenes.
      So when reading this, it only makes sense if you always
      set all BackgroundSkySphereRadius to all (e.g. you set only
      by this property).

      Note that there is doesn't really exist a requirement that all
      animation frames have the same BackgroundSkySphereRadius...
      But it's most common. In fact, it all depends on your needs, see
      TVRMLGLScene.BackgroundSkySphereRadius used by
      TVRMLGLScene.Background. }
    property BackgroundSkySphereRadius: Single
      read GetBackgroundSkySphereRadius
      write SetBackgroundSkySphereRadius;

    { Common camera angle of view, for all scenes of this animation.
      See TVRMLScene.AngleOfViewX and TVRMLScene.AngleOfViewY.

      Reading these reads FirstScene.AngleOfViewX/Y values,
      and setting these sets the value for all scenes within this animation.
      In other words, if you use only this (and our GLProjection),
      then all the scenes of your animation will always have equal
      AngleOfViewX/Y values.

      @groupBegin }
    property AngleOfViewX: Single read GetAngleOfViewX write SetAngleOfViewX;
    property AngleOfViewY: Single read GetAngleOfViewY write SetAngleOfViewY;
    { @groupEnd }

    { Set OpenGL projection, based on currently
      bound Viewpoint, NavigationInfo (using FirstScene) and used navigator.

      You should use this instead of directly calling Scenes[0].GLProjection,
      because this takes care to update our WalkProjectionNear,
      WalkProjectionFar, BackgroundSkySphereRadius, AngleOfViewX,
      AngleOfViewY properties. Moreover, last three of these properties
      are also set as values for all the other scenes in an animation.
      That is, we take care to keep BackgroundSkySphereRadius and
      AngleOfView* @italic(equal for all scenes of this animation,
      and the TVRMLGLAnimation itself).

      @seealso TVRMLGLScene.GLProjection }
    procedure GLProjection(Nav: TNavigator;
      const Box: TBox3d;
      const WindowWidth, WindowHeight: Cardinal;
      const ForceZFarInfinity: boolean = false);

    property WalkProjectionNear: Single read FWalkProjectionNear;
    property WalkProjectionFar : Single read FWalkProjectionFar ;

    procedure ViewChangedSuddenly;

    { Handling key and mouse events, overriding TUIControl methods.

      We pass key and mouse events only if there's exactly one scene
      (ScenesCount = 1), as there's no sensible way of activating
      VRML events when TVRMLGLAnimation contains more than one scene.
      (Precalculated animation of this class, and interactive
      animation by TVRMLScene.ProcessEvents do not mix sensibly.)

      So when ScenesCount = 1, we simply pass key and mouse events to
      the only Scene[0]. Be sure to turn on @code(Scene[0].ProcessEvents := true)
      if you want to make actual use of it.

      @groupBegin }
    function KeyDown(Key: TKey; C: char): boolean; override;
    function KeyUp(Key: TKey; C: char): boolean; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    { @groupEnd }

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function PositionInside(const X, Y: Integer): boolean; override;

    { Initial world time, set by the ResetWorldTimeAtLoad call.
      This can be useful for showing user
      time like "WorldTime: LoadTime + %f" on status bar.

      0 means that starting WorldTime was TimeBegin of the animation
      (0.0 in case of normal VRML files, usually 0.0 in case of Kanim).
      Note that even when TimeBegin <> 0 (for Kanim), we still set
      WorldTimeAtLoad to 0, this is nicer to show to user.

      Other value means that we used current real time as time origin,
      following VRML/X3D specification.
      See also [http://vrmlengine.sourceforge.net/vrml_time_origin_considered_uncomfortable.php] }
    property WorldTimeAtLoad: TKamTime read FWorldTimeAtLoad;

    { Current time of the animation. Although you do not have to use it:
      you can always acccess any point in time of the animtion by SceneFromTime.
      But sometimes tracking the current time here is most natural
      and comfortable.

      When we have exactly one scene in Scenes, our methods (ResetWorldTime,
      ResetWorldTimeAtLoad and Idle) will synchronize Scenes[0].WorldTime
      always to the same value as our own WorldTime.
      This makes time-dependent nodes (like TimeSensor,
      MovieTexture etc.) inside this scene work Ok. }
    property WorldTime: TKamTime read FWorldTime;

    { Set WorldTime to initial value after loading a world. }
    procedure ResetWorldTimeAtLoad(const ForceTimeOrigin: boolean = false);

    { Set WorldTime to arbitrary value. }
    procedure ResetWorldTime(const NewValue: TKamTime);
  published
    { Is the animation time playing, and how fast.

      For exact meaning of our TimePlaying, TimePlayingSpeed, see
      TVRMLScene.TimePlaying, TVRMLScene.TimePlayingSpeed.
      Like in TVRMLScene, these are realized by our @link(Idle) method,
      so WorldTime is automatically increased in @link(Idle) which is called
      automatically if you added this to some TGLUIWindow.Controls or
      TKamOpenGLControl.Controls.

      Note that Scenes[0].TimePlaying, Scenes[0].TimePlayingSpeed do not matter
      when you're operating on the TVRMLGLAnimation level.
      They will not affect our WorldTime, or even Scenes[0].WorldTime,
      and they will not be synchronized with our values.

      @groupBegin }
    property TimePlaying: boolean read FTimePlaying write FTimePlaying default true;
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;
    { @groupEnd }

    { See SceneFromTime for description what this property does. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop default true;

    { See SceneFromTime for description what this property does. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards default false;

    { Common Navigator, for all scenes of this animation.
      See TVRMLScene.Navigator.

      Setting this sets the Navigator for all scenes within this animation.
      In other words, if you use only this,
      then all the scenes of your animation will always have equal
      Navigator values. }
    property Navigator: TNavigator read FNavigator write SetNavigator;

    { Optimization of the animation. See TVRMLGLScene.Optimization.

      When animation is @link(Loaded), this is equal to
      TVRMLGLScene.Optimization of all loaded scenes.
      That is, all loaded scenes should have the same Optimization, always.

      You can access this even when animation is not @link(Loaded).
      Note that changing this when animation is @link(Loaded) may be
      a costly operation, see TVRMLGLScene.Optimization. So don't do it
      e.g. every frame. And when loading, it's best to set Optimization
      as desired @italic(before) calling @link(Load) for fasters loading.

      Note that this class should generally use roSeparateShapesNoTransform
      or roSeparateShapes for Optimization, to conserve memory
      in some common cases. See docs at TGLRendererOptimization type. }
    property Optimization: TGLRendererOptimization
      read FOptimization write SetOptimization
      default roSeparateShapesNoTransform;
  end;

  TObjectsListItem_1 = TVRMLGLAnimation;
  {$I objectslist_1.inc}
  TVRMLGLAnimationsList = TObjectsList_1;

procedure Register;

{$undef read_interface}

implementation

uses Math, VectorMath, VRMLFields,
  ProgressUnit, Object3dAsVRML, KambiLog, DateUtils;

{$define read_implementation}
{$I objectslist_1.inc}

procedure Register;
begin
  RegisterComponents('Kambi', [TVRMLGLAnimation]);
end;

{ TVRMLGLAnimationScene ------------------------------------------------------ }

type
  TVRMLGLAnimationScene = class(TVRMLGLScene)
  private
    FParentAnimation: TVRMLGLAnimation;
  public
    constructor CreateForAnimation(
      ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AProvidedRenderer: TVRMLOpenGLRenderer;
      AParentAnimation: TVRMLGLAnimation);
    property ParentAnimation: TVRMLGLAnimation read FParentAnimation;
    procedure DoGeometryChanged; override;
    procedure VisibleChange; override;
  end;

constructor TVRMLGLAnimationScene.CreateForAnimation(
  ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AProvidedRenderer: TVRMLOpenGLRenderer;
  AParentAnimation: TVRMLGLAnimation);
begin
  { ParentAnimation is used by DoGeometryChanged, which is virtual and
    *may* called by ChangedAll, which *may* called by inherited constructor...
    So ParentAnimation must be set even before inherited constructor. }
  FParentAnimation := AParentAnimation;

  inherited CreateProvidedRenderer(nil, AProvidedRenderer);

  Optimization := FParentAnimation.Optimization;
  Navigator := FParentAnimation.Navigator;

  Load(ARootNode, AOwnsRootNode);
end;

procedure TVRMLGLAnimationScene.DoGeometryChanged;
begin
  inherited;
  ParentAnimation.ValidBoundingBoxSum := false;
end;

procedure TVRMLGLAnimationScene.VisibleChange;
begin
  inherited;
  ParentAnimation.VisibleChange;
end;

{ EModelsStructureDifferent -------------------------------------------------- }

type
  EModelsStructureDifferent = class(Exception)
    constructor CreateFmt(const S: string; const Args: array of const);
  end;

constructor EModelsStructureDifferent.CreateFmt(const S: string;
  const Args: array of const);
begin
  inherited CreateFmt('Models are structurally different: ' + S, Args);
end;

{ TVRMLGLAnimation ------------------------------------------------------------ }

{ About Create and CreateCustomCache relationship:

  Note that Create cannot call CreateCustomCache and depend that
  CreateCustomCache calls "inherited Create". This wouldn't be nice
  for descendants: If some TVRMLGLAnimation descendant would override "Create"
  to do his initialization, and we would create this descendant by
  CreateCustomCache --- we would miss executing descendant's constructor code.

  So only our Create may call "inherited Create".

  CreateCustomCache should always call just "Create" (virtual).
  To do this (since CreateCustomCache must initialize Renderer)
  we initialize Renderer in CreateCustomCache, and in Create check
  Renderer = nil before setting default renderer. }

constructor TVRMLGLAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Renderer = nil then
    Renderer := TVRMLOpenGLRenderer.Create(TVRMLSceneRenderingAttributes, nil);
  FOptimization := roSeparateShapesNoTransform;
  FTimeLoop := true;
  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;
end;

constructor TVRMLGLAnimation.CreateCustomCache(AOwner: TComponent;
  ACache: TVRMLOpenGLRendererContextCache);
begin
  Renderer := TVRMLOpenGLRenderer.Create(TVRMLSceneRenderingAttributes, ACache);
  Create(AOwner);
end;

destructor TVRMLGLAnimation.Destroy;
begin
  Close;
  FreeAndNil(Renderer);
  inherited;
end;

procedure TVRMLGLAnimation.LoadCore(
  GetRootNodeWithTime: TGetRootNodeWithTime;
  RootNodesCount: Cardinal;
  AOwnsFirstRootNode: boolean;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single);

  { This will check that Model1 and Model2 are exactly equal,
    or that at least interpolating (see VRMLModelLerp) is possible.

    If models are structurally different (which means that even
    interpolating between Model1 and Model2 is not possible),
    it will raise EModelsStructureDifferent. }
  procedure CheckVRMLModelsStructurallyEqual(Model1, Model2: TVRMLNode);

    procedure CheckSFNodesStructurallyEqual(Field1, Field2: TSFNode);
    begin
      if (Field1.Value <> nil) and (Field2.Value <> nil) then
      begin
        CheckVRMLModelsStructurallyEqual(Field1.Value, Field2.Value);
      end else
      if not ((Field1.Value = nil) and (Field2.Value = nil)) then
        raise EModelsStructureDifferent.CreateFmt('Field "%s" of type SFNode ' +
          'is once NULL and once not-NULL', [Field1.Name]);
    end;

    procedure CheckMFNodesStructurallyEqual(Field1, Field2: TMFNode);
    var
      I: Integer;
    begin
      if Field1.Items.Count <> Field2.Items.Count then
        raise EModelsStructureDifferent.CreateFmt(
          'Different number of children in MFNode fields: "%d" and "%d"',
          [Model1.ChildrenCount, Model2.ChildrenCount]);

      for I := 0 to Field1.Items.Count - 1 do
        CheckVRMLModelsStructurallyEqual(Field1.Items.Items[I],
                                         Field2.Items.Items[I]);
    end;

  var
    I: Integer;
  begin
    { Yes, Model1 and Model2 must have *exactly* the same classes. }
    if Model1.ClassType <> Model2.ClassType then
      raise EModelsStructureDifferent.CreateFmt(
        'Different nodes classes: "%s" and "%s"',
        [Model1.ClassName, Model2.ClassName]);

    {$ifndef VER_THAT_SUPPORTS_INTERFACES_WITHOUT_BUGS}

    { This ugly version (without using interfaces) is only to support
      compilation with FPC 2.0.4, it will be removed at some point.

      Later: arrghh. This was under "ifdef VER2_0", and the cleaner version
      seemed to work with FPC 2.2.0...
      well, but it doesn't, crashed with access violation when Model1,2 are
      of TNodeComposedShader (for example, try on
      kambi_vrml_test_suite/kanim/specular_demo_phong_shading/specular_demo.kanim).
      That's because TNodeComposedShader has some interfaces...
      Screw this, don't use interfaces for now. Ugly hack below will be used. }

    if Model1 is TNodeWWWInline then
    begin
      TNodeWWWInline(Model1).LoadInlined(false);
      TNodeWWWInline(Model2).LoadInlined(false);
    end else
    if Model1 is TNodeInline then
    begin
      TNodeInline(Model1).LoadInlined(false);
      TNodeInline(Model2).LoadInlined(false);
    end else
    if Model1 is TNodeInlineLoadControl then
    begin
      TNodeInlineLoadControl(Model1).LoadInlined(false);
      TNodeInlineLoadControl(Model2).LoadInlined(false);
    end;
    {$else}
    if Supports(Model1, IVRMLInlineNode) and
       Supports(Model2, IVRMLInlineNode) then
      begin
        { Make sure that *Inline content is loaded now. }
        (Model1 as IVRMLInlineNode).LoadInlined(false);
        (Model2 as IVRMLInlineNode).LoadInlined(false);
      end;
    {$endif}

    if Model1.NodeName <> Model2.NodeName then
      raise EModelsStructureDifferent.CreateFmt(
        'Different names of nodes: "%s" and "%s"',
        [Model1.NodeName, Model2.NodeName]);

    if Model1.WWWBasePath <> Model2.WWWBasePath then
      raise EModelsStructureDifferent.CreateFmt(
        'Different WWWBasePath of nodes: "%s" and "%s"',
        [Model1.WWWBasePath, Model2.WWWBasePath]);

    if Model1.ChildrenCount <> Model2.ChildrenCount then
      raise EModelsStructureDifferent.CreateFmt(
        'Different number of children in nodes: "%d" and "%d"',
        [Model1.ChildrenCount, Model2.ChildrenCount]);

    for I := 0 to Model1.ChildrenCount - 1 do
      CheckVRMLModelsStructurallyEqual(Model1.Children[I], Model2.Children[I]);

    { Yes, the situation below can happen. *Usually* when we know
      that Model1 and Model2 are equal classes then we know that
      they have the same number of fields of the same type.
      However, for TVRMLUnknownNode, it's not that easy. Two different instances
      of TVRMLUnknownNode class may have completely different fields,
      so we must safeguard against this. }
    if Model1.Fields.Count <> Model2.Fields.Count then
      raise EModelsStructureDifferent.CreateFmt(
        'Different number of fields in nodes: "%d" and "%d"',
        [Model1.Fields.Count, Model2.Fields.Count]);

    for I := 0 to Model1.Fields.Count - 1 do
    begin
      if Model1.Fields[I].ClassType <> Model2.Fields[I].ClassType then
        raise EModelsStructureDifferent.CreateFmt(
          'Different type of field number %d in nodes: "%s" and "%s"',
          [I, Model1.Fields[I].ClassName, Model2.Fields[I].ClassName]);

      if Model1.Fields[I] is TSFNode then
        CheckSFNodesStructurallyEqual(
          TSFNode(Model1.Fields[I]), TSFNode(Model2.Fields[I])) else
      if Model1.Fields[I] is TMFNode then
        CheckMFNodesStructurallyEqual(
          TMFNode(Model1.Fields[I]), TMFNode(Model2.Fields[I])) else
      if Model1.Fields[I].CanAssignLerp then
      begin
        if Model1.Fields[I] is TVRMLMultField then
        begin
          try
            (Model1.Fields[I] as TVRMLMultField).CheckCountEqual
              (Model2.Fields[I] as TVRMLMultField);
          except
            (* Translate EVRMLMultFieldDifferentCount exception
               (may be raised by TVRMLMultField.CheckCountEqual above)
               to EModelsStructureDifferent. *)
            on E: EVRMLMultFieldDifferentCount do
              raise EModelsStructureDifferent.CreateFmt('%s', [E.Message]);
          end;
        end;
        { Else we have single-value field that can lerp.
          No need to check anything in this case,
          it's ready to go (that is, to lerp). }
      end else
      begin
        { Check fields for equality.

          Some special fields like TNodeWWWInline.FdName do not
          have to be equal, as they don't have any role for the
          "real" meaning of the model. I mean, if TNodeWWWInline
          children (loaded from pointed file) have the same structure,
          then we're happy. And it's handy to allow this --- see e.g.
          examples/models/gus_1_final.wrl and
          examples/models/gus_2_final.wrl trick. }

        if not (
           ( (Model1 is TNodeWWWInline)         and (Model1.Fields[I].Name = 'name') ) or
           ( (Model2 is TNodeInline)            and (Model1.Fields[I].Name = 'url') ) or
           ( (Model1 is TNodeInlineLoadControl) and (Model1.Fields[I].Name = 'url') ) or
           Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon)
           ) then
          raise EModelsStructureDifferent.CreateFmt(
            'Fields "%s" (class "%s") are not equal',
            [Model1.Fields[I].Name, Model1.Fields[I].ClassName]);
      end;
    end;
  end;

  { This will merge equal children of Model1 and Model2,
    and check that Model1 and Model2 are exactly equal.

    It assumes that models are structurally equal, i.e. that you
    already did run CheckVRMLModelsStructurallyEqual over them.

    It works recursively: first it checks for every children
    are they equal. For each pair that is equal, it replaces
    given children in Model2 with appropriate children of Model1.
    At the end, if every children pair was equal and additionally
    if all fields are equal, then it returns true.

    Such copying of references is useful, because then we simply copy given
    node's reference instead of duplicating this object.
    This way Model1 and Model2 and all models interpolated along the way
    can share the same object reference. This is very good, because:

    1. If nodes are equal then creating new object each
       time would mean that I create a lot of objects with exactly the
       same contents. So memory is wasted, without any good reason.

    2. For nodes like Texture2, this is good because then the image
       is loaded from the file only once. This means that memory is saved,
       once again. This also means that in case when texture file doesn't
       exist, user gets only 1 warning/error message (instead of getting
       warning/error message for each duplicated TNodeTexture2 instance).

    3. Also for nodes like Texture2, this means that if we use the same
       VRMLOpenGLRenderer to render every model of the animation,
       then VRMLOpenGLRenderer will recognize this and given texture
       will be loaded only once for OpenGL. So loading time and
       memory are saved *once again*  (otherwise OpenGL would allocate
       internal copy of texture for each duplicated node, once again
       wasting a lot of memory).

    4. And later the Shape cache of TVRMLOpenGLRenderer can speed
       up loading time and conserve memory use, if it sees the same
       reference to given GeometryNode twice. }
  function VRMLModelsMerge(Model1, Model2: TVRMLNode): boolean;

    function SFNodesMerge(Field1, Field2: TSFNode): boolean;
    begin
      Result := true;

      { Equality was already checked by CheckVRMLModelsStructurallyEqual,
        so now if one SFNode value is not nil, we know that the other
        one is not nil too. }
      if Field1.Value <> nil then
      begin
        if VRMLModelsMerge(Field1.Value, Field2.Value) then
          Field1.Value := Field2.Value else
          Result := false;
      end;
    end;

    function MFNodesMerge(Field1, Field2: TMFNode): boolean;
    var
      I: Integer;
    begin
      Result := true;

      { Note that we already know that Counts are equals,
        checked already by CheckVRMLModelsStructurallyEqual. }
      Assert(Field1.Items.Count = Field2.Items.Count);
      for I := 0 to Field1.Items.Count - 1 do
      begin
        if VRMLModelsMerge(Field1.Items.Items[I],
                           Field2.Items.Items[I]) then
        begin
          { Think of this as
              Field1.Items.Items[I] := Field2.Items.Items[I]
            but I can't call this directly, I must use Field1.ReplaceChild
            to not mess reference counts. }
          Field1.ReplaceItem(I, Field2.Items.Items[I]);
        end else
          Result := false;
      end;
    end;

  var
    I: Integer;
  begin
    Result := true;

    { Note that this loop will iterate over every Children,
      even if somewhere along the way we will already set Result to false.
      Even if we already know that Result is false, we stil want to
      merge Model1 and Model2 children as much as we can. }
    for I := 0 to Model1.ChildrenCount - 1 do
    begin
      if VRMLModelsMerge(Model1.Children[I], Model2.Children[I]) then
      begin
        { Tests: Writeln('merged child ', I, ' of class ',
          Model1.Children[I].NodeTypeName); }
        Model1.Children[I] := Model2.Children[I];
      end else
        Result := false;
    end;

    if not Result then Exit;

    for I := 0 to Model1.Fields.Count - 1 do
    begin
      if Model1.Fields[I] is TSFNode then
      begin
        if not SFNodesMerge(TSFNode(Model1.Fields[I]),
                            TSFNode(Model2.Fields[I])) then
          Result := false;
      end else
      if Model1.Fields[I] is TMFNode then
      begin
        if not MFNodesMerge(TMFNode(Model1.Fields[I]),
                            TMFNode(Model2.Fields[I])) then
          Result := false;
      end else
      if Model1.Fields[I].CanAssignLerp then
      begin
        if not Model1.Fields[I].Equals(Model2.Fields[I], EqualityEpsilon) then
          Result := false;
      end;

      { Other fields were already checked by CheckVRMLModelsStructurallyEqual }
    end;
  end;

  { Linear interpolation between Model1 and Model2.
    A = 0 means Model1, A = 1 means Model2, A between 0 and 1 is lerp
    between Model1 and Model2.

    If Model1 and Model2 are the same object (the same references),
    then this will return just Model1. This way it keeps memory optimization
    described by VRMLModelsMerge. This is also true if both Model1 and Model2
    are nil: then you can safely call this and it will return also nil. }
  function VRMLModelLerp(const A: Single; Model1, Model2: TVRMLNode): TVRMLNode;

    procedure SFNodeLerp(Target, Field1, Field2: TSFNode);
    begin
      Target.Value := VRMLModelLerp(A, Field1.Value, Field2.Value);
    end;

    procedure MFNodeLerp(Target, Field1, Field2: TMFNode);
    var
      I: Integer;
    begin
      for I := 0 to Field1.Items.Count - 1 do
        Target.AddItem(VRMLModelLerp(A, Field1.Items.Items[I],
                                        Field2.Items.Items[I]));
    end;

  var
    I: Integer;
  begin
    if Model1 = Model2 then
      Exit(Model1);

    Result := TVRMLNodeClass(Model1.ClassType).Create(Model1.NodeName,
      Model1.WWWBasePath);
    try
      { We already loaded all inlines (in CheckVRMLModelsStructurallyEqual).
        We have to mark it now, by setting Loaded := true field as necessary
        inside inline nodes --- otherwise, they could be loaded again
        (adding content to already existing nodes, making content loaded
        more than once). }
      if Result is TNodeWWWInline then
      begin
        TNodeWWWInline(Result).LoadedInlineDirectly;
      end else
      if Result is TNodeInline then
      begin
        TNodeInline(Result).LoadedInlineDirectly;
      end else
      if Result is TNodeInlineLoadControl then
      begin
        TNodeInline(Result).LoadedInlineDirectly;
      end;

      { TODO: the code below doesn't deal efficiently with the situation when single
        TVRMLNode is used as a child many times in one of the nodes.
        (through VRML "USE" keyword). Code below will then unnecessarily
        create copies of such things (wasting construction time and memory),
        instead of reusing the same object reference. }
      for I := 0 to Model1.ChildrenCount - 1 do
        Result.AddChild(VRMLModelLerp(A, Model1.Children[I], Model2.Children[I]));

      { TODO: for TVRMLUnknownNode, we should fill here Result.Fields.
        Also for TVRMLPrototypeNode. }

      for I := 0 to Model1.Fields.Count - 1 do
      begin
        if Model1.Fields[I] is TSFNode then
        begin
          SFNodeLerp(
            (Result.Fields[I] as TSFNode),
            (Model1.Fields[I] as TSFNode),
            (Model2.Fields[I] as TSFNode));
        end else
        if Model1.Fields[I] is TMFNode then
        begin
          MFNodeLerp(
            (Result.Fields[I] as TMFNode),
            (Model1.Fields[I] as TMFNode),
            (Model2.Fields[I] as TMFNode));
        end else
        if Model1.Fields[I].CanAssignLerp then
        begin
          Result.Fields[I].AssignLerp(A, Model1.Fields[I], Model2.Fields[I]);
        end else
        begin
          { These fields cannot be interpolated.
            So just copy to Result.Fields[I]. }
          Result.Fields[I].Assign(Model1.Fields[I]);
        end;
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;

  function CreateOneScene(Node: TVRMLNode;
    OwnsRootNode: boolean): TVRMLGLAnimationScene;
  begin
    Result := TVRMLGLAnimationScene.CreateForAnimation(
      Node, OwnsRootNode, Renderer, Self);
  end;

var
  I: Integer;
  StructurallyEqual, RootNodesEqual: boolean;
  LastSceneIndex: Integer;
  LastSceneRootNode, NewRootNode: TVRMLNode;
  LastTime, NewTime: Single;
  SceneIndex: Integer;
begin
  Close;

  FOwnsFirstRootNode := AOwnsFirstRootNode;

  FScenes := TVRMLGLScenesList.Create;

  FTimeLoop := true;
  FTimeBackwards := false;

  { calculate FScenes contents now }

  { RootNodes[0] goes to FScenes[0], that's easy }
  GetRootNodeWithTime(0, NewRootNode, NewTime);

  FScenes.Count := 1;
  FScenes[0] := CreateOneScene(NewRootNode, OwnsFirstRootNode);
  LastSceneIndex := 0;
  LastTime := NewTime;
  LastSceneRootNode := NewRootNode;

  { calculate TimeBegin at this point }
  FTimeBegin := NewTime;

  for I := 1 to RootNodesCount - 1 do
  begin
    { Now add RootNodes[I] }
    GetRootNodeWithTime(I, NewRootNode, NewTime);

    StructurallyEqual := false;

    try
      CheckVRMLModelsStructurallyEqual(LastSceneRootNode, NewRootNode);
      StructurallyEqual := true;
    except
      on E: EModelsStructureDifferent do
      begin
        if Log then
          WritelnLog('VRMLGLAnimation', Format(
            'Nodes %d and %d structurally different, so animation will not be smoothed between them: ',
            [I - 1, I]) + E.Message);
      end;
    end;

    FScenes.Count := FScenes.Count +
      Max(1, Round((NewTime - LastTime) * ScenesPerTime));

    if StructurallyEqual then
    begin
      { Try to merge it with LastSceneRootNode.
        Then initialize FScenes[LastSceneIndex + 1 to FScenes.High]. }
      RootNodesEqual := VRMLModelsMerge(NewRootNode, LastSceneRootNode);
      if RootNodesEqual then
      begin
        { In this case I don't waste memory, and I'm simply reusing
          LastSceneRootNode. Actually, I'm just copying FScenes[LastSceneIndex].
          This way I have a series of the same instances of TVRMLGLScene
          along the way. When freeing FScenes, we will be smart and
          avoid deallocating the same pointer twice. }
        FreeAndNil(NewRootNode);
        for SceneIndex := LastSceneIndex + 1 to FScenes.High do
          FScenes[SceneIndex] := FScenes[LastSceneIndex];
      end else
      begin
        for SceneIndex := LastSceneIndex + 1 to FScenes.High - 1 do
          FScenes[SceneIndex] := CreateOneScene(VRMLModelLerp(
            MapRange(SceneIndex, LastSceneIndex, FScenes.High, 0.0, 1.0),
            LastSceneRootNode, NewRootNode), true);
        FScenes.Last := CreateOneScene(NewRootNode, true);
        LastSceneRootNode := NewRootNode;
      end;
    end else
    begin
      { We cannot interpolate between last and new node.
        So just duplicate last node until FScenes.High - 1,
        and at FScenes.High insert new node. }
      for SceneIndex := LastSceneIndex + 1 to FScenes.High - 1 do
        FScenes[SceneIndex] := FScenes[LastSceneIndex];
      FScenes.Last := CreateOneScene(NewRootNode, true);
      LastSceneRootNode := NewRootNode;
    end;

    LastTime := NewTime;
    LastSceneIndex := FScenes.High;
  end;

  { calculate TimeEnd at this point }
  FTimeEnd := NewTime;

  FLoaded := true;
end;

procedure TVRMLGLAnimation.Load_GetRootNodeWithTime(const Index: Cardinal;
  out RootNode: TVRMLNode; out Time: Single);
begin
  RootNode := Load_RootNodes[Index];
  Time := Load_Times[Index];
end;

procedure TVRMLGLAnimation.Load(
  RootNodes: TVRMLNodesList;
  AOwnsFirstRootNode: boolean;
  ATimes: TDynSingleArray;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single);
begin
  Assert(RootNodes.Count = ATimes.Count);
  Load_RootNodes := RootNodes;
  Load_Times := ATimes;

  LoadCore(@Load_GetRootNodeWithTime, RootNodes.Count,
    AOwnsFirstRootNode, ScenesPerTime, EqualityEpsilon);
end;

procedure TVRMLGLAnimation.LoadFromVRMLEvents_GetRootNodeWithTime(
  const Index: Cardinal;
  out RootNode: TVRMLNode; out Time: Single);
begin
  Time := LoadFromVRMLEvents_TimeBegin;
  if LoadFromVRMLEvents_ScenesPerTime <> 0 then
    Time += Index / LoadFromVRMLEvents_ScenesPerTime;

  if Index = 0 then
    LoadFromVRMLEvents_Scene.ResetWorldTime(Time) else
    LoadFromVRMLEvents_Scene.SetWorldTime(Time);

  RootNode := LoadFromVRMLEvents_Scene.RootNode.DeepCopy;
end;

procedure TVRMLGLAnimation.LoadFromVRMLEvents_GetRootNodeWithTime_Progress(
  const Index: Cardinal;
  out RootNode: TVRMLNode; out Time: Single);
begin
  LoadFromVRMLEvents_GetRootNodeWithTime(Index, RootNode, Time);
  Progress.Step;
end;

procedure TVRMLGLAnimation.LoadFromVRMLEvents(
  RootNode: TVRMLNode;
  AOwnsRootNode: boolean;
  const ATimeBegin, ATimeEnd: Single;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  const ProgressTitle: string);
var
  Count: Cardinal;
begin
  LoadFromVRMLEvents_ScenesPerTime := ScenesPerTime;
  LoadFromVRMLEvents_TimeBegin := ATimeBegin;
  LoadFromVRMLEvents_Scene := TVRMLScene.Create(nil);
  try
    LoadFromVRMLEvents_Scene.Load(RootNode, AOwnsRootNode);

    Count := Max(1, Round((ATimeEnd - ATimeBegin) * ScenesPerTime));

    LoadFromVRMLEvents_Scene.ProcessEvents := true;

    if ProgressTitle <> '' then
    begin
      Progress.Init(Count, ProgressTitle);
      try
        LoadCore(@LoadFromVRMLEvents_GetRootNodeWithTime_Progress, Count,
          true, 0, EqualityEpsilon);
      finally
        Progress.Fini;
      end;
    end else
    begin
      LoadCore(@LoadFromVRMLEvents_GetRootNodeWithTime, Count,
        true, 0, EqualityEpsilon);
    end;

    { Although LoadCore sets FTimeEnd already, it may be a little
      smaller than ATimeEnd if ScenesPerTime is very small.
      Last scene generated by LoadFromVRMLEvents_GetRootNodeWithTime
      will not necessarily "hit" exactly TimeEnd.
      In particular, when ScenesPerTime = 0, LoadCore will just set
      FTimeEnd to TimeBegin...

      Since we guarantee in the interface that FTimeEnd will be exactly
      equal to ATimeEnd after LoadFromVRMLEvents, we fix it here. }

    FTimeEnd := ATimeEnd;
  finally FreeAndNil(LoadFromVRMLEvents_Scene) end;
end;

procedure TVRMLGLAnimation.LoadStatic(
  RootNode: TVRMLNode;
  AOwnsRootNode: boolean);
var
  RootNodes: TVRMLNodesList;
  ATimes: TDynSingleArray;
begin
  RootNodes := TVRMLNodesList.Create;
  try
    ATimes := TDynSingleArray.Create;
    try
      RootNodes.Add(RootNode);
      ATimes.Add(0);
      Load(RootNodes, AOwnsRootNode, ATimes, 1, 0.0);
    finally FreeAndNil(ATimes) end;
  finally FreeAndNil(RootNodes) end;
end;

procedure TVRMLGLAnimation.LoadFromFile(const FileName: string;
  ChangeLoadParameters: TAnimationChangeLoadParametersFunc);
var
  { Vars from LoadFromFileToVars }
  ModelFileNames: TDynStringArray;
  Times: TDynSingleArray;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  ATimeLoop, ATimeBackwards: boolean;

  RootNodes: TVRMLNodesList;
  I, J: Integer;
begin
  ModelFileNames := nil;
  Times := nil;
  RootNodes := nil;

  try
    ModelFileNames := TDynStringArray.Create;
    Times := TDynSingleArray.Create;

    LoadFromFileToVars(FileName, ModelFileNames, Times,
      ScenesPerTime, FOptimization, EqualityEpsilon, ATimeLoop, ATimeBackwards);

    if Assigned(ChangeLoadParameters) then
      { Since the scene is not loaded now, FOptimization can just be changed
        directly. So we simply pass it as "var" param to ChangeLoadParameters. }
      ChangeLoadParameters(ScenesPerTime, FOptimization, EqualityEpsilon);

    Assert(ModelFileNames.Length = Times.Length);
    Assert(ModelFileNames.Length >= 1);

    RootNodes := TVRMLNodesList.Create;
    RootNodes.Count := ModelFileNames.Count;

    for I := 0 to ModelFileNames.High do
    try
      RootNodes[I] := LoadAsVRML(ModelFileNames[I]);
    except
      for J := 0 to I - 1 do
        RootNodes.FreeAndNil(J);
      raise;
    end;

    Load(RootNodes, true, Times, ScenesPerTime, EqualityEpsilon);
    TimeLoop := ATimeLoop;
    TimeBackwards := ATimeBackwards;

  finally
    FreeAndNil(ModelFileNames);
    FreeAndNil(Times);
    FreeAndNil(RootNodes);
  end;
end;

procedure TVRMLGLAnimation.Close;
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
      for I := 0 to FScenes.High - 1 do
      begin
        if FScenes[I] = FScenes[I+1] then
          FScenes[I] := nil { set to nil, just for safety } else
          FScenes.FreeAndNil(I);
      end;
      FScenes.FreeAndNil(FScenes.High);
    end;

    FreeAndNil(FScenes);
  end;

  ValidBoundingBoxSum := false;

  FLoaded := false;
end;

function TVRMLGLAnimation.GetScenes(I: Integer): TVRMLGLScene;
begin
  Result := FScenes[I];
end;

function TVRMLGLAnimation.ScenesCount: Integer;
begin
  if Loaded then
    Result := FScenes.Count else
    Result := 0;
end;

function TVRMLGLAnimation.FirstScene: TVRMLGLScene;
begin
  Result := FScenes.First;
end;

function TVRMLGLAnimation.LastScene: TVRMLGLScene;
begin
  Result := FScenes.Last;
end;

procedure TVRMLGLAnimation.PrepareRender(
  TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions;
  ProgressStep: boolean);
var
  I: Integer;
  SceneOptions: TPrepareRenderOptions;
begin
  for I := 0 to FScenes.High do
  begin
    { For I <> 0, we don't want to pass prManifoldAndBorderEdges to scenes. }
    SceneOptions := Options;
    if I <> 0 then
      Exclude(SceneOptions, prManifoldAndBorderEdges);

    FScenes[I].PrepareRender(TransparentGroups, SceneOptions);

    if (prManifoldAndBorderEdges in Options) and (I <> 0) then
      FScenes[I].ShareManifoldAndBorderEdges(
        FScenes[0].ManifoldEdges, FScenes[0].BorderEdges);

    if ProgressStep then
      Progress.Step;
  end;
end;

procedure TVRMLGLAnimation.FreeResources(Resources: TVRMLSceneFreeResources);
var
  I: Integer;
begin
  for I := 0 to FScenes.High do
    FScenes[I].FreeResources(Resources);
end;

procedure TVRMLGLAnimation.GLContextClose;
{ Note that this is called from destructor, so we must be extra careful
  here and check is everything <> nil before freeing it. }
begin
  if FScenes <> nil then
    FScenes.GLContextClose;

  if Renderer <> nil then
    Renderer.UnprepareAll;
end;

function TVRMLGLAnimation.TimeDuration: Single;
begin
  Result := TimeEnd - TimeBegin;
end;

function TVRMLGLAnimation.TimeDurationWithBack: Single;
begin
  Result := TimeDuration;
  if TimeBackwards then
    Result *= 2;
end;

function TVRMLGLAnimation.SceneFromTime(const Time: Single): TVRMLGLScene;
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
    { I use FScenes.Count, not FScenes.High as the highest range value.
      This is critical. On the short range (not looping), it may seem
      that FScenes.High is more appropriate, since the last scene
      corresponds exactly to TimeEnd. But that's not good for looping:
      in effect float range TimeDuration would contain one scene less,
      and so when looking at large Time values, the scenes are slightly shifted
      within time.

      This causes problems when code relies on the meaning of some time
      values. E.g. if TimeBegin = 0, you expect that Time = k * TimeEnd,
      for any k, will result in the LastScene generated (assuming
      backwards is @true). This is needed for tricks like smooth animations
      concatenation, see "the rift" in RiftCreatures unit.

      When using FScenes.High, we would break this, as scenes are shifted
      by one in each range. }
    SceneNumber := Floor(MapRange(Time, TimeBegin, TimeEnd, 0, FScenes.Count));

    DivUnsignedMod(SceneNumber, FScenes.Count, DivResult, ModResult);

    if TimeLoop then
    begin
      if TimeBackwards and Odd(DivResult) then
        SceneNumber := FScenes.High - ModResult else
        SceneNumber := ModResult;
    end else
    begin
      if TimeBackwards then
      begin
        if (DivResult < 0) or (DivResult > 1) then
          SceneNumber := 0 else
        if DivResult = 1 then
          SceneNumber := FScenes.High - ModResult;
          { else DivResult = 0, so SceneNumber is already correct }
      end else
      begin
        if DivResult < 0 then
          SceneNumber := 0 else
        if DivResult > 0 then
          SceneNumber := FScenes.High;
      end;
    end;
  end;

  Result := FScenes[SceneNumber];
end;

function TVRMLGLAnimation.Attributes: TVRMLSceneRenderingAttributes;
begin
  Result := TVRMLSceneRenderingAttributes(Renderer.Attributes);
end;

function TVRMLGLAnimation.ManifoldEdges: TDynManifoldEdgeArray;
begin
  Result := FirstScene.ManifoldEdges;
end;

function TVRMLGLAnimation.BorderEdges: TDynBorderEdgeArray;
begin
  Result := FirstScene.BorderEdges;
end;

procedure TVRMLGLAnimation.ShareManifoldAndBorderEdges(
  ManifoldShared: TDynManifoldEdgeArray;
  BorderShared: TDynBorderEdgeArray);
var
  I: Integer;
begin
  for I := 0 to FScenes.High do
    FScenes[I].ShareManifoldAndBorderEdges(ManifoldShared, BorderShared);
end;

function TVRMLGLAnimation.BoundingBoxSum: TBox3d;

  procedure ValidateBoundingBoxSum;
  var
    I: Integer;
  begin
    FBoundingBoxSum := FScenes[0].BoundingBox;
    for I := 1 to FScenes.High do
      Box3dSumTo1st(FBoundingBoxSum, FScenes[I].BoundingBox);
    ValidBoundingBoxSum := true;
  end;

begin
  if not ValidBoundingBoxSum then
    ValidateBoundingBoxSum;
  Result := FBoundingBoxSum;
end;

procedure TVRMLGLAnimation.ChangedAll;
var
  I: Integer;
begin
  for I := 0 to FScenes.High do
    FScenes[I].ChangedAll;
  ValidBoundingBoxSum := false;
end;

function TVRMLGLAnimation.InfoBoundingBoxSum: string;
var
  BBox: TBox3d;
begin
  BBox := BoundingBoxSum;
  Result := 'Bounding box (of the whole animation) : ' + Box3dToNiceStr(BBox);
  if not IsEmptyBox3d(BBox) then
  begin
    Result += ', average size : ' + FloatToNiceStr(Box3dAvgSize(BBox));
  end;
  Result += NL;
end;

function TVRMLGLAnimation.Info(
  ATriangleVerticesCounts,
  ABoundingBox,
  AManifoldAndBorderEdges: boolean): string;
begin
  Result := '';

  if ATriangleVerticesCounts then
  begin
    Result += FirstScene.InfoTriangleVerticesCounts;
  end;

  if ABoundingBox then
  begin
    if Result <> '' then Result += NL;
    { We do not call FirstScene.InfoBoundingBox here, instead we want
      to get full bounding box of the animation. }
    Result += InfoBoundingBoxSum;
  end;

  if AManifoldAndBorderEdges then
  begin
    if Result <> '' then Result += NL;
    Result += FirstScene.InfoManifoldAndBorderEdges;
  end;
end;

procedure TVRMLGLAnimation.WritelnInfoNodes;
begin
  FirstScene.WritelnInfoNodes;
end;

function TVRMLGLAnimation.GetBackgroundSkySphereRadius: Single;
begin
  Result := FirstScene.BackgroundSkySphereRadius;
end;

procedure TVRMLGLAnimation.SetBackgroundSkySphereRadius(const Value: Single);
var
  I: Integer;
begin
  { Note: GLProjection implementation depends on the fact that we always
    assing here Value to all scenes, without checking is new
    Value <> old GetBackgroundSkySphereRadius. }

  for I := 0 to FScenes.High do
    FScenes[I].BackgroundSkySphereRadius := Value;
end;

function TVRMLGLAnimation.GetAngleOfViewX: Single;
begin
  Result := FirstScene.AngleOfViewX;
end;

procedure TVRMLGLAnimation.SetAngleOfViewX(const Value: Single);
var
  I: Integer;
begin
  { Note: GLProjection implementation depends on the fact that we always
    assing here Value to all scenes, without checking is new
    Value <> old GetAngleOfViewX. }

  for I := 0 to FScenes.High do
    FScenes[I].AngleOfViewX := Value;
end;

function TVRMLGLAnimation.GetAngleOfViewY: Single;
begin
  Result := FirstScene.AngleOfViewY;
end;

procedure TVRMLGLAnimation.SetAngleOfViewY(const Value: Single);
var
  I: Integer;
begin
  { Note: GLProjection implementation depends on the fact that we always
    assing here Value to all scenes, without checking is new
    Value <> old GetAngleOfViewY. }

  for I := 0 to FScenes.High do
    FScenes[I].AngleOfViewY := Value;
end;

procedure TVRMLGLAnimation.SetNavigator(const Value: TNavigator);
var
  I: Integer;
begin
  { We use our own FNavigator field (that is, not implementing
    GetNavigator as Result := FirstScene.Navigator), to make Navigator
    property work even when the animation is currently not loaded. }

  if Value <> FNavigator then
  begin
    FNavigator := Value;
    if FScenes <> nil then
    begin
      for I := 0 to FScenes.High do
        FScenes[I].Navigator := Value;
    end;
  end;
end;

procedure TVRMLGLAnimation.SetOptimization(const Value: TGLRendererOptimization);
var
  I: Integer;
begin
  { Although TVRMLGLScene.SetOptimization already compares new value
    with previous (and ignores setting the same value), we also compare
    it to avoid potentially length iteration (when FScenes.High,
    merely iterating may take a (small) time). }

  if Value <> FOptimization then
  begin
    FOptimization := Value;
    if FScenes <> nil then
    begin
      for I := 0 to FScenes.High do
        FScenes[I].Optimization := Value;
    end;
  end;
end;

procedure TVRMLGLAnimation.SetOwnsFirstRootNode(const Value: boolean);
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

      for I := 0 to FScenes.High do
      begin
        if FScenes[I] = FScenes[0] then
          FScenes[I].OwnsRootNode := Value else
          Break;
      end;
    end;
  end;
end;

procedure TVRMLGLAnimation.GLProjection(Nav: TNavigator;
  const Box: TBox3d;
  const WindowWidth, WindowHeight: Cardinal;
  const ForceZFarInfinity: boolean);
begin
  FirstScene.GLProjection(Nav, Box, WindowWidth, WindowHeight,
    ForceZFarInfinity);

  { Setting these will also update their values in all scenes. }
  BackgroundSkySphereRadius := FirstScene.BackgroundSkySphereRadius;
  AngleOfViewX := FirstScene.AngleOfViewX;
  AngleOfViewY := FirstScene.AngleOfViewY;

  FWalkProjectionNear := FirstScene.WalkProjectionNear;
  FWalkProjectionFar  := FirstScene.WalkProjectionFar ;
end;

procedure TVRMLGLAnimation.ViewChangedSuddenly;
var
  I: Integer;
begin
  if FScenes <> nil then
  begin
    for I := 0 to FScenes.High do
      FScenes[I].ViewChangedSuddenly;
  end;
end;

function TVRMLGLAnimation.KeyDown(Key: TKey; C: char): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].KeyDown(Key, C) else
    Result := false;
end;

function TVRMLGLAnimation.KeyUp(Key: TKey; C: char): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].KeyUp(Key, C) else
    Result := false;
end;

function TVRMLGLAnimation.MouseDown(const Button: TMouseButton): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].MouseDown(Button) else
    Result := false;
end;

function TVRMLGLAnimation.MouseUp(const Button: TMouseButton): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].MouseUp(Button) else
    Result := false;
end;

function TVRMLGLAnimation.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  if ScenesCount = 1 then
    Result := Scenes[0].MouseMove(OldX, OldY, NewX, NewY) else
    Result := false;
end;

procedure TVRMLGLAnimation.ResetWorldTimeAtLoad(const ForceTimeOrigin: boolean = false);

  function TimeOriginAtLoad: boolean;
  var
    N: TNodeNavigationInfo;
  begin
    Result := false;

    if Loaded then
    begin
      N := Scenes[0].NavigationInfoStack.Top as TNodeNavigationInfo;
      if (N <> nil) and
         (N is TNodeKambiNavigationInfo) then
        Result := TNodeKambiNavigationInfo(N).FdTimeOriginAtLoad.Value;
    end;
  end;

begin
  if (ScenesCount > 1) or ForceTimeOrigin or TimeOriginAtLoad then
  begin
    FWorldTimeAtLoad := 0.0;
    ResetWorldTime(TimeBegin);
  end else
  begin
    FWorldTimeAtLoad := DateTimeToUnix(Now);
    ResetWorldTime(WorldTimeAtLoad);
  end;
end;

procedure TVRMLGLAnimation.ResetWorldTime(const NewValue: TKamTime);
begin
  FWorldTime := NewValue;

  { Ignored when SceneAnimation.ScenesCount <> 1, as scenes' ProcessEvents
    is always false then and WorldTime wouldn't have much sense anyway. }
  if ScenesCount = 1 then
    Scenes[0].ResetWorldTime(NewValue);
end;

procedure TVRMLGLAnimation.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
var
  OldWorldTime: TKamTime;
begin
  inherited;

  { Ignore Idle calls when CompSpeed is precisely zero
    (this may happen, and is good, see TGLWindow.IgnoreNextIdleSpeed).
    In this case, time increase will be zero so the whole code
    will not do anything anyway. }

  if TimePlaying and (CompSpeed <> 0) then
  begin
    OldWorldTime := FWorldTime;
    FWorldTime += TimePlayingSpeed * CompSpeed;

    { When ScenesCount = 1, it's sensible for single scene to receive
      events, to increase it's time. Note that TVRMLScene.SetWorldTime
      will signal when redisplay will be needed (something visible changed),
      we don't have to worry about it.

      We call Scenes[0].SetWorldTime direcly, instead of calling Scenes[0].Idle.
      This way we do not have to worry to set scene's initial time, TimePlaying,
      TimePlayingSpeed to our values. }
    if ScenesCount = 1 then
      Scenes[0].SetWorldTime(WorldTime);

    { Call VisibleChange only if the displayed animation frame actually changed.
      This way, we avoid wasting CPU cycles if the loaded scene is actually
      still, or if the animation stopped running. }
    if (SceneFromTime(OldWorldTime) <>
        SceneFromTime(WorldTime)) then
      VisibleChange;
  end;

  { Even if mouse is over the scene, still allow others (like a Navigator
    underneath) to always handle mouse and keys in their Idle. }
  LetOthersHandleMouseAndKeys := true;
end;

function TVRMLGLAnimation.CurrentScene: TVRMLGLScene;
begin
  Result := SceneFromTime(WorldTime);
end;

function TVRMLGLAnimation.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

end.
