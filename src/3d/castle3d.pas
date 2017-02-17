{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base 3D objects (T3D, T3DList, T3DTransform, T3DOrient, T3DMoving). }
unit Castle3D;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, CastleVectors, CastleFrustum,
  CastleBoxes, CastleClassUtils, CastleKeysMouse, CastleRectangles,
  CastleUtils, FGL, CastleGenericLists, CastleTimeUtils,
  CastleSoundEngine, CastleSectors, CastleCameras, CastleTriangles;

type
  T3D = class;
  T3DList = class;
  T3DWorld = class;
  T3DOrient = class;
  T3DAlive = class;

  TRenderFromViewFunction = procedure of object;

  { Describe what visible thing changed for T3D.VisibleChangeHere. }
  TVisibleChange = (
    { Visible geometry (actual 3D shape) changed.
      It's not used when only light conditions, materials, textures
      and such changed.

      In practice, this means that the depth buffer from some point in space
      changed. Which means that shadow maps should be regenerated. }
    vcVisibleGeometry,

    { Something visible, but not the geometry shape, changes.
      For example, material or texture on a visible surface changed. }
    vcVisibleNonGeometry);
  TVisibleChanges = set of TVisibleChange;

  TVisibleChangeEvent = procedure (const Sender: T3D; const Changes: TVisibleChanges) of object;

  { Various things that T3D.PrepareResources may prepare. }
  TPrepareResourcesOption = (prRender, prBackground, prBoundingBox,
    prShadowVolume,
    { Prepare octrees (determined by things like TCastleSceneCore.Spatial). }
    prSpatial,
    prScreenEffects);
  TPrepareResourcesOptions = set of TPrepareResourcesOption;

  { Shadow volumes helper, not depending on OpenGL. }
  TBaseShadowVolumeRenderer = class
  end;

  T3DListCore = class;

  { Information about ray collision with a single 3D object.
    Everything (Point, RayOrigin, RayDirection) is expressed in the
    local coordinates of given 3D object (in @link(Item)). }
  TRayCollisionNode = object
  public
    { Colliding 3D object. }
    Item: T3D;

    { Position, in local coordinate system of this 3D object,
      of the picked 3D point.

      If the ray hit empty space, this is undefined.
      Note that only MainScene is informed about pointing device events
      when the ray hit empty space. }
    Point: TVector3Single;

    { Triangle that was hit. This triangle is always a part of @link(Item).

      If the ray hit empty space, this is @nil.
      Note that only TCastleSceneManager.MainScene is informed about pointing
      device events when the ray hit empty space, so this is an unusual case.

      May also be @nil if RayCollision implementation for the 3D object
      simply left it @nil. Right now, only TCastleScene sets this field. }
    Triangle: P3DTriangle;

    { Ray used to cause the collision,
      in local coordinate system of this 3D object. }
    RayOrigin, RayDirection: TVector3Single;
  end;
  PRayCollisionNode = ^TRayCollisionNode;

  { Represents a @bold(ray) collision with a 3D objects tree.
    Just access the @code(First) item for the collision information
    with the final 3D object. The rest of items are containers of this 3D
    object (a path within @link(TCastleSceneManager.Items) hierarchy tree,
    usually).

    This list is a path in the 3D objects tree leading from the
    final colliding 3D object to the root of the tree.

    For example, your 3D tree may be a list (like T3DList), and within
    this list is a transformed list (T3DTransform),
    and within is your final colliding object (like TCastleScene).
    We will contain in this case these three items, in reverse order
    (TCastleScene, T3DTransform, T3DList).
    This allows you to track the containers that contain given collision.

    This is never an empty list when returned by RayCollision. }
  TRayCollision = class(specialize TGenericStructList<TRayCollisionNode>)
  public
    { Distance, in world coordinate system, from the current
      camera to the picked point. The suggested usage is to decide if player
      is close enough to reach the 3D object --- for example, you may not
      want to allow player to open a door by clicking on it from a far distance.

      If the ray hit empty space, the distance is MaxSingle.
      Note that only MainScene is informed about pointing device events
      when the ray hit empty space. }
    Distance: Single;

    { Index of node with given Item. }
    function IndexOfItem(const Item: T3D): Integer;
  end;

  { Detailed information about collision with a single 3D object. }
  TCollisionDetailsItem = object
  public
    { Colliding 3D object. }
    Item: T3D;

    { Triangle that was hit. This triangle is always a part of @link(Item).

      May be @nil if the given collision implementation cannot determine
      this (not all 3D objects must have a simple array of triangles in memory).
      In practice, only TCastleSceneCore sets this now, and containers
      that eventually proxy the collisions to underlying TCastleSceneCore instances.

      If the ray hit empty space, this is @nil.
      Note that only TCastleSceneManager.MainScene is informed about pointing
      device events when the ray hit empty space, so this is an unusual case. }
    // Triangle: P3DTriangle;
  end;
  PCollisionDetailsItem = ^TCollisionDetailsItem;

  { Represents a collision with a 3D objects tree.
    Just access the @code(First) item for the collision information
    with the final 3D object. The rest of items are containers of this 3D
    object (a path within @link(TCastleSceneManager.Items) hierarchy tree,
    usually).

    This list is a path in the 3D objects tree leading from the
    final colliding 3D object to the root of the tree.

    For example, your 3D tree may be a list (like T3DList), and within
    this list is a transformed list (T3DTransform),
    and within is your final colliding object (like TCastleScene).
    We will contain in this case these three items, in reverse order
    (TCastleScene, T3DTransform, T3DList).
    This allows you to track the containers that contain given collision.

    This is never an empty list when returned by XxxCollision method. }
  TCollisionDetails = class(specialize TGenericStructList<TCollisionDetailsItem>)
  public
    { Index of node with given Item. }
    function IndexOfItem(const Item: T3D): Integer;
    procedure Add(const Item: T3D);
  end;

  { Statistics about what was rendered during last frame.
    You will usually access this by scene manager property,
    see @link(TCastleAbstractViewport.Statistics). }
  TRenderStatistics = record
    { How many shapes were rendered (send to OpenGL)
      versus all shapes that were potentially visible.
      Potentially visible shapes are the ones with
      TShape.Visible inside a 3D object with T3D.GetExists.

      When ShapesRendered is much smaller than ShapesVisible,
      it means that the algorithm for removing invisible scene parts
      works good. This includes frustum culling (automatically
      used by TCastleScene), or occlusion culling (see
      TSceneRenderingAttributes.UseOcclusionQuery),
      or any custom algorithm you implement by using TTestShapeVisibility
      callback with @link(TCastleScene.Render). }
    ShapesRendered, ShapesVisible: Cardinal;

    { The number of shapes that were not rendered,
      but their bounding box was rendered to check with occlusion query.
      This is always zero when not using occlusion query (see
      TSceneRenderingAttributes.UseOcclusionQuery).
      Basically, this measures the "invisible overhead" of occlusion query. }
    BoxesOcclusionQueriedCount: Cardinal;
  end;

  { List of lights. Always TLightInstancesList, but we cannot declare it here
    as such. }
  TAbstractLightInstancesList = TFPSList;

  TRenderingPass = 0..1;

  { Information that 3D object needs to render.
    Read-only for T3D.Render (except Statistics, which should be updated
    by T3D.Render). }
  TRenderParams = class
    { Which parts should be rendered: opaque (@false) or transparent (@true). }
    Transparent: boolean;

    { Should we render parts that may receive shadow volumes, or ones that don't.
      During rendering, simply check does it match TCastleScene.ReceiveShadowVolumes. }
    ShadowVolumesReceivers: boolean;

    { If @true, means that we're using multi-pass
      shadowing technique (like shadow volumes),
      and currently doing the "shadowed" pass.

      Which means that most lights (ones with shadowVolumes = TRUE)
      should be turned off, see [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows].) }
    InShadow: boolean;

    { Value > 0 means we're inside some stencil test (like for
      InShadow = @false pass of shadow volumes). }
    StencilTest: Cardinal;

    { Rendering pass number, for multi-pass rendering, like for shadow volumes. }
    Pass: TRenderingPass;

    { Transformation that should be applied to the rendered result.
      If RenderTransformIdentity, then RenderTransform is always identity.
      @groupBegin }
    RenderTransform: TMatrix4Single;
    RenderTransformIdentity: boolean;
    { @groupEnd }

    Statistics: TRenderStatistics;

    constructor Create;

    { Lights that shine on given 3D object. }
    function BaseLights(Scene: T3D): TAbstractLightInstancesList; virtual; abstract;
  end;

  TRemoveType = (rtNone, rtRemove, rtRemoveAndFree);

  { Base 3D object, that can be managed by TCastleSceneManager.
    All 3D objects should descend from this, this way we can easily
    insert them into the TCastleSceneManager.

    Default implementations of collision methods in this class work
    with our BoundingBox:

    @unorderedList(
      @item(Wall-sliding MoveCollision version simply calls
        non-wall-sliding version (without separate ProposedNewPos
        and NewPos).)
      @item(Non-wall-sliding MoveCollision version,
        SegmentCollision, SphereCollision, SphereCollision2D, PointCollision2D,
        BoxCollision, RayCollision, and HeightCollision
        check for collisions with our BoundingBox,
        using TBox3D methods:
        @link(TBox3D.TryRayClosestIntersection),
        @link(TBox3D.TryRayEntrance),
        @link(TBox3D.SegmentCollision),
        @link(TBox3D.SphereCollision) and
        @link(TBox3D.Collision).)
    )

    The idea is that by default everything simply uses BoundingBox,
    and that is the only method that you really @italic(have) to override.
    You do not have to (in fact, usually you should not) call "inherited"
    when overriding collision methods mentioned above. }
  T3D = class(TComponent)
  private
    FCastShadowVolumes: boolean;
    FExists: boolean;
    FCollides: boolean;
    FPickable: boolean;
    FCursor: TMouseCursor;
    FCollidesWithMoving: boolean;
    Disabled: Cardinal;
    FExcludeFromGlobalLights, FExcludeFromStatistics: boolean;
    FWorld: T3DWorld;
    procedure SetCursor(const Value: TMouseCursor);
  protected
    { Make this 3D object assigned to given 3D world.
      Each 3D object can only be part of one T3DWorld at a time.
      Always remove 3D object from previous world (scene manager)
      before adding it to new one. }
    procedure SetWorld(const Value: T3DWorld); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Height of a point above the 3D model.
      This checks ray collision, from Position along the negated GravityUp vector.
      Measures distance to the nearest scene item (called "ground" here).

      @returns(If the 3D scene is hit.
        @false means that Position floats above an empty space.
        That is, if you turn gravity on, it will fall down forever,
        as far as this 3D scene is concerned.)

      @param(AboveHeight Height above the ground.
        @italic(One height unit equals one GravityUp vector).
        Always use normalized GravityUp vector if you expect
        to receive here a normal distance.

        AboveHeight is always set to MaxSingle when returned result is @false
        (this guarantee simplifies some code).)

      @param(AboveGround Pointer to P3DTriangle representing the ground.
        Must be @nil if returned result is @false.
        @bold(May) be @nil even if we returned @true (not all 3D
        objects may be able to generate P3DTriangle information about collision).

        This may be useful for example to make a footsteps sound dependent
        on texture of the ground.
        Or to decrease player life points for walking on hot lava.
        See "The Castle" game for examples.)
    }
    function HeightCollision(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; virtual;

    { Can other 3D object (maybe a player) move without colliding with this object.

      If IsRadius, then you should prefer to perform exact collision with sphere
      of given radius (must be > 0).
      At the very least, this checks that the line segment
      between OldPos and NewPos doesn't collide,
      @bold(and) that sphere with given Radius centered around NewPos
      doesn't collide.

      If not IsRadius, or if checking for collisions with sphere is not possible
      for some reasons, then you can check for collisions with boxes.
      OldBox should usually be ignored (it can be useful when collision-checking
      has to be approximate in some corner cases, see TCreature.MoveCollision).
      NewBox plays the same role as "sphere centered around NewPos" in paragraph
      above.

      Overloaded version with separate ProposedNewPos and NewPos parameters
      allows you to accept the move, but for NewPos (that should be some slightly
      modified version of ProposedNewPos). This allows to implement wall-sliding:
      when camera tries to walk into the wall, we will change movement
      to move alongside the wall (instead of just completely blocking the move).
      When this version returns @false, it's undefined what is the NewPos.

      @groupBegin }
    function MoveCollision(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual;
    function MoveCollision(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual;
    { @groupEnd }

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; virtual;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual;

    { Check collision with a sphere in 2D (a circle, extruded to infinity along the Z axis).

      Note that PointCollision2D and SphereCollision2D @italic(do not work
      reliably on objects that have 3D rotations). See @link(PointCollision2D)
      for details.

      @param(Details If non-nil, these are automatically filled with the details
        about the collision.
        If the result is @false, the Details contents are untouched.
        If the result is @true, the Details contents are set to describe
        the 3D objects hierarchy that caused this collision.) }
    function SphereCollision2D(const Pos: TVector2Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const Details: TCollisionDetails = nil): boolean; virtual;

    { Check collision with a point in 2D (which is an infinite line along the Z axis
      in 3D).

      Note that PointCollision2D and SphereCollision2D @italic(do not work
      reliably on objects that have 3D rotations), that is: rotations that change
      the direction of Z axis! This applies to all ways of rotating --
      using the T3DCustomTransform descendants (like T3DTransform)
      or using the X3D node TTransformNode (within a TCastleSce).

      @orderedList(
        @item(@italic(The reason): we transform the point (or sphere center)
          to the local coordinates, and we should also transform the Z axis to
          the local coordinates, to be always correct. Right now, we don't do
          the latter.)

        @item(And we don't want to do it (at least not in all cases)!
          The simple 2D point collision check would then actually perform
          a 3D line collision check, thus PointCollision2D would lose all the speed
          benefits over LineCollision. PointCollision2D would become
          a simple shortcut to perform @italic(LineCollision with
          a line parallel to Z axis).

          And in case of 2D games, or mostly 2D games, this speed loss
          would not be justified. Often you @italic(know) that your objects
          have no 3D rotations, for example if your animations are done in Spine.)

        @item(In the future, we may overcome this limitation.
          To do this, we will detect whether the transformation is "only 2D"
          (actually this part is easy, you can detect it by looking at the matrix
          even, so check whether appropriate numbers are zero).
          And then PointCollision2D will change to LineCollision,
          and SphereCollision2D will change to something like ExtrudedCirleCollision,
          @italic(only when necessary).)
      )
    }
    function PointCollision2D(const Point: TVector2Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual;

    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; virtual;

    { Check collision with a ray, building a TRayCollision result.
      Returns a collision as TRayCollision instance, or @nil if no collision.
      Caller is responsible for freeing the returned TRayCollision instance.

      Contrary to other collision routines, this should @italic(ignore
      the @link(Collides) property). The @link(Collides) property
      specifies whether item collides with camera. And this method is used
      for picking (pointing) 3D stuff --- everything visible can be picked,
      collidable or not. Instead, this looks at @link(Pickable) property
      (actually, at @link(GetPickable) method result).

      This always returns the first collision with the 3D world, that is
      the one with smallest TRayCollision.Distance. For example, when
      implemented in T3DList, this checks collisions for all list items,
      and chooses the closest one. }
    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Does item really exist, see @link(Exists) and @link(Enable),
      @link(Disable).
      It T3D class, returns @true if @link(Exists) and not disabled.
      May be modified in subclasses, to return something more complicated. }
    function GetExists: boolean; virtual;

    { Does item really collide, see @link(Collides).
      It T3D class, returns @link(Collides) and @link(GetExists).
      May be modified in subclasses, to return something more complicated. }
    function GetCollides: boolean; virtual;

    { Is item really pickable, see @link(Pickable).
      It T3D class, returns @link(Pickable) and @link(GetExists).
      May be modified in subclasses, to return something more complicated. }
    function GetPickable: boolean; virtual;

    { Is this object visible and colliding.

      Setting this to @false pretty much turns everything of this 3D object
      to "off". This is useful for objects that disappear completely from
      the level when something happens. You could just as well remove
      this object from @link(TCastleSceneManager.Items) tree, but sometimes it's more
      comfortable to simply turn this property to @false.

      Descendants may also override GetExists method.

      @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { Items that are at least once disabled are treated like not existing.
      Every @link(Disable) call should always be paired with @link(Enable) call
      (usually using @code(try ... finally .... end) block).
      Internally, we keep a counter of how many times the object is disabled,
      and if this counter is <> 0 then GetExists returns @false.
      Using this is useful for taming collisions, especially to avoid self-collisions
      (when a creature moves, it doesn't want to collide with other creatures,
      but obviously it doesn't collide with it's own bounding volume).
      @groupBegin }
    procedure Disable;
    procedure Enable;
    { @groupEnd }

    { Should this 3D object participate in collision detection.
      You can turn this off, useful to make e.g. "fake" walls
      (to some secret places on level).

      This describes collision resolution with almost everything --- camera,
      player (in third-person perspective, camera may differ from player),
      other creatures. That is because everything
      resolves collisions through our methods MoveCollision and HeightCollision
      (high-level) or SegmentCollision, SphereCollision, SphereCollision2D,
      PointCollision2D, BoxCollision (low-level).

      (Note that RayCollision is excluded from this,
      it exceptionally ignores Collides value, as it's primarily used for picking.
      Same for SegmentCollision with LineOfSight=true.)

      The only exception are the collisions with T3DMoving instances
      (movable world parts like elevators and doors) that have their own
      detection routines and look at CollidesWithMoving property of other objects.
      That is, the T3DMoving instance itself must still have Collides = @true,
      but it interacts with @italic(other) objects if and only if they have
      CollidesWithMoving = @true (ignoring their Collides value).
      This allows items to be moved by elevators, but still player and creatures
      can pass through them.

      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection).

      Descendants may also override GetCollides method. Sometimes it's more
      comfortable than changing the property value.

      @noAutoLinkHere }
    property Collides: boolean read FCollides write FCollides default true;

    { Is item pickable by @link(RayCollision) method.
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects are never pickable).
      This is independent from @link(Collides), as @link(RayCollision)
      does not look at @link(Collides), it only looks at @link(Pickable).

      Descendants may also override GetPickable method. Sometimes it's more
      comfortable than changing the property value.

      @noAutoLinkHere }
    property Pickable: boolean read FPickable write FPickable default true;

    { Bounding box of the 3D object.

      Should take into account both collidable and visible objects.
      For example, invisible walls (not visible) and fake walls
      (not collidable) should all be accounted here.

      It's a @italic(bounding) volume, it should be as large as necessary
      to include the object inside. At the same time, it should be
      as "tight" as it can, to make various optimizations work best. }
    function BoundingBox: TBox3D; virtual; abstract;

    { Render given object.
      Should check and immediately exit when @link(Exists) is @false.
      Should render only parts with matching Params.Transparency
      and Params.ShadowVolumesReceivers values (it may be called
      more than once to render frame).

      @param(Frustum May be used to optimize rendering, to not
        render the parts outside the Frustum.)

      @param(Params Other parameters helpful for rendering.)
    }
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); virtual;

    { Does the 3D object cast shadows by shadow volumes.
      See also TCastleScene.ReceiveShadowVolumes. }
    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes default true;

    { Render shadow quads for all the things rendered by @link(Render).
      This is done only if @link(Exists) and @link(CastShadowVolumes).

      It does shadow volumes culling inside (so ShadowVolumeRenderer should
      have FrustumCullingInit already initialized).

      ParentTransform and ParentTransformIsIdentity describe the transformation
      of this object in the 3D world.
      T3D objects may be organized in a hierarchy when
      parent transforms it's children. When ParentTransformIsIdentity,
      ParentTransform must be IdentityMatrix4Single (it's not guaranteed
      that when ParentTransformIsIdentity = @true, Transform value will be
      ignored !).

      @italic(Implementation note:) In @link(Render), it is usually possible
      to implement ParentTransform* by glPush/PopMatrix and Frustum.Move tricks.
      But RenderShadowVolume needs actual transformation explicitly:
      ShadowMaybeVisible needs actual box position in world coordinates,
      so bounding box has to be transformed by ParentTransform.
      And TCastleScene.RenderShadowVolumeCore needs explicit ParentTransform
      to correctly detect front/back sides (for silhouette edges and
      volume capping). }
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); virtual;

    { Prepare resources, making various methods (like rendering and such)
      to execute fast.

      This requires OpenGL to be initailized for most 3D objects.
      If not, some parts of preparations will be aborted.

      This makes sure that appropriate methods execute as fast as possible.
      It's never required to call this method
      --- everything will be prepared "as needed" anyway.
      But if you allow everything to be prepared "as needed",
      then e.g. the first @link(Render) call may take a long time because it may
      have to prepare resources that will be reused in next @link(Render) calls.
      This is bad, as your program will seem very slow at the beginning
      (when rendering resources are prepared, so a first frame,
      or a couple of first frames, if it's something
      like a precalculated animation). To avoid this, call this method,
      showing the user something like "now we're preparing
      the resources --- please wait".

      For OpenGL rendered objects, this method ties this object
      to the current OpenGL context.
      But it doesn't change any OpenGL state or buffers contents
      (at most, it allocates some texture and display list names).

      @param(Options What features should be prepared to execute fast.
        See TPrepareResourcesOption,
        the names should be self-explanatory (they refer to appropriate
        methods of T3D, TCastleSceneCore or TCastleScene).)

      @param(ProgressStep Says that we should make this many Progress.Step calls
        during preparation.
        Useful to show progress bar to the user during long preparation.

        TODO: for now, do not include prSpatial if you use ProgressStep.
        Reason: octree preparations have a separate mechanism
        that may want to show progress.)

      @param(BaseLights Used if Options contains prRender.
        A list of base lights (always TLightInstancesList, although
        cannot be declated as such) used for rendering.
        May be @nil (equivalent to empty).) }
    procedure PrepareResources(
      Options: TPrepareResourcesOptions;
      ProgressStep: boolean;
      BaseLights: TAbstractLightInstancesList); virtual;

    { How many times PrepareResources will call Progress.Step.
      Useful only if you want to pass ProgressStep = @true to PrepareResources.
      In the base class T3D this just returns 0.  }
    function PrepareResourcesSteps: Cardinal; virtual;

    { Press and release events of key and mouse. Return @true if you handled them.
      See also TUIControl analogous events.
      @groupBegin }
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
    { @groupEnd }

    { Pointing device (usually mouse) events.
      Return @true if you handled the event.

      @unorderedList(
        @item(PointingDeviceActivate signals that the picking button (usually,
          left mouse button) is pressed or released (depending on Active parameter).

          Note that the exact key or mouse responsible for this is configurable
          in our engine by Input_Interact. By default it's the left mouse button,
          as is usual for VRML/X3D browsers. But it can be configured to be other
          mouse button or a key, for example most 3D games use "e" key to interact.)

        @item(PointingDeviceMove signals that pointer moves over this 3D object.)
      )

      PointingDeviceMove receives Pick information about what exactly is hit
      by the 3D ray corresponding to the current mouse position.
      It contains the detailed information about 3D point, triangle
      and ray (all in local coordinate system) that are indicated by the mouse.
      PointingDeviceActivate does not receive this information now
      (because it may happen in obscure situations when ray direction is not known;
      this is all related to our "fallback to MainScene" mechanism).

      They also receive Distance to the collision,
      in world coordinates. See TRayCollision.Distance.

      The pointing device event (activation,
      deactivation or move) is send first to the innermost 3D object.
      That is, we first send this event to the first item on
      TRayCollision list corresponding to the current ray.
      This way, the innermost ("most local") 3D object has the chance
      to handle this event first. If the event is not handled, it is passed
      to other 3D objects (we simply iterate over the TRayCollision list).
      If nothing on TRayCollision list
      handled the item, it is eventually passed to main 3D scene
      (TCastleSceneManager.MainScene), if it wasn't already present on
      TRayCollision list.

      Note that when passing this event to TCastleSceneManager.MainScene,
      it is possible that 3D ray simply didn't hit anything (mouse pointer
      is over the background). In this case, TRayCollisionNode.Point
      is undefined, TRayCollisionNode.Triangle is @nil
      and Distance is MaxSingle.

      This event should be handled only if GetExists.
      Usually, 3D objects with GetExists = @false will not be returned
      by RayCollision, so they will not receive this event anyway.
      However, if 3D object may be equal to TCastleSceneManager.MainScene,
      then it should be secured and check for GetExists
      inside PointingDeviceActivate and PointingDeviceMove.

      @groupBegin }
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; virtual;
    function PointingDeviceMove(const Pick: TRayCollisionNode;
      const Distance: Single): boolean; virtual;
    { @groupEnd }

    { Continously occuring event, for various tasks.
      @param(RemoveMe Set this to rtRemove or rtRemoveAndFree to remove
        this item from 3D world (parent list) after Update finished.
        rtRemoveAndFree additionally will free this item.
        Initially it's rtNone when this method is called.) }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); virtual;

    { Something visible changed inside @italic(this) 3D object.
      This is usually called by implementation of this 3D object,
      to notify others that it changed.

      Changes is a set describing what changes occurred.
      See TVisibleChange docs for more information.
      It must specify all things that possibly changed.

      Changes can be [], meaning "something tells us to redraw, but no visible change
      happened yet, maybe something will happen during a redraw"
      (this is used when e.g. possibly LOD level changed).
      We still broadcast VisibleChangeNotification, even when Changes=[].

      The information about visibility changed is passed upward,
      to the Parent, and eventually to the TCastleSceneManager,
      that broadcasts this to all 3D objects
      by VisibleChangeNotification. If you want to @italic(react) to visibility
      changes, you should override VisibleChangeNotification,
      not this method.

      Be careful when handling this, various changes may cause this,
      so be prepared to handle this at every time. }
    procedure VisibleChangeHere(const Changes: TVisibleChanges); virtual;

    { World containing this 3D object. In other words, the root of 3D objects
      tree containing this object. In practice, the world instance
      is always 1-1 corresponding to a particular TCastleSceneManager instance
      (each scene manager has it's world instance in @link(TCastleSceneManager.Items)).

      @nil if we are not part of a hierarchy rooted in T3DWorld.
      In pratice, this happens if we're not yet part of a @link(TCastleSceneManager.Items)
      hierarchy. }
    property World: T3DWorld read FWorld;

    { Something visible changed in the 3D world.
      This is usually called by our container (like TCastleSceneManager),
      to allow this 3D object to react (e.g. by regenerating mirror textures)
      to changes in the 3D world (not necessarily in this 3D object,
      maybe in some other T3D instance).

      If you want to @italic(react) to visibility
      changes, you should override this. }
    procedure VisibleChangeNotification(const Changes: TVisibleChanges); virtual;

    { Main camera observing this 3D object changed.
      This is usually called by our container (like TCastleSceneManager)
      to notify that camera changed. }
    procedure CameraChanged(ACamera: TCamera); virtual;

    { Mouse cursor over this object. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Called when OpenGL context of the window is destroyed.
      This will be also automatically called from destructor.

      Control should clear here any resources that are tied to the GL context. }
    procedure GLContextClose; virtual;

    procedure UpdateGeneratedTextures(
      const RenderFunc: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const OriginalViewport: TRectangle); virtual;

    { Are we in the middle of dragging something by moving the mouse.

      This should be set to @true to disable camera navigation
      methods that also use mouse move. In practice, to disable TExamineCamera
      view rotation/movement by moving the mouse, as it makes (comfortable)
      dragging practically impossible (at each mouse move, view changes...).

      In particular, when you operate on active X3D pointing-device sensors
      (like drag sensors, e.g. PlaneSensor, but also TouchSensor may
      use it). }
    function Dragging: boolean; virtual;

    { Middle point, usually "eye point", of the 3D model.
      This is used for sphere center (if overriden Sphere returns @true)
      and is the central point from which collisions of this object
      are checked (Move, MoveAllowed, Height, LineOfSight).
      For 3D things like level scene this is mostly useless (as you will leave
      Sphere at default @false then, and the scene itself doesn't move),
      but it's crucial for dynamic 3D things like player and moving creatures.

      In short, it's usually most comfortable to think about this as
      a position of the eye, or the middle of the creature's head.

      In an ideal situation, it should not be based on anything dynamic.
      For example, when this is based on the current bounding box of the animation,
      there is a risk that a large and sudden change in animation
      box could make the Middle point to jump to the other side of
      the wall (breaking collisions, as it changes Middle without a chance
      to check for collisions by MoveAllowed).
      Ideally, it should remain constant even when the shape of the object changes,
      and be possible to change only when MoveAllowed is checked
      (so only when T3DOrient.Position or T3DTransform.Translation can change).

      In this class this is simply zero. In the descendant
      T3DCustomTransform (ancestor of T3DTransform, T3DOrient
      that in turn are ancestors of normal creatures, items etc.)
      this is overriden to return something sensible above the bottom
      of the box. See T3DCustomTransform.MiddleHeight. }
    function Middle: TVector3Single; virtual;

    { Sector where the middle of this 3D object is.
      Used for AI. @nil if none (maybe because we're not part of any world,
      maybe because sectors of the world were not initialized,
      or maybe simply because we're outside of all sectors). }
    function Sector: TSector;

    { Can the approximate sphere (around Middle point)
      be used for some collision-detection
      tasks. If @true then Radius (and Middle point) determine the approximate
      sphere surrounding the 3D object (it does not have to be a perfect
      bounding sphere around the object), and it may be used for some
      collisions instead of BoundingBox.
      See @link(CollidesWithMoving) and @link(MoveAllowed) for when it may happen.

      Must return @false when not GetExists (because we can't express
      "empty sphere" by @link(Sphere) method for now, but BoundingBox can express
      EmptyBox3D).

      By default, in T3D class, this always returns @false
      and @link(Sphere) is undefined.

      The advantages of using a sphere, that does not have to be a perfect
      bounding sphere (it may be smaller than necessary, and only
      account e.g. for upper body part of the creature), are:

      @unorderedList(
        @item(It can have constant radius, even though the actual
          creature animates. This allows us to perfectly, reliably guarantee
          that sphere absolutely never collides with level and such.

          In case of a tight bounding volume (box or sphere) that animates,
          this guarantee is not really possible. Simply increasing time changes
          the animation to the next frame, which may be slightly larger
          in one dimension because e.g. creature moves a hand in this direction.
          This means that simply increasing time may change the non-collidable
          creature into a collidable one, if creature stands close to a wall/other
          creature and such. And we cannot simply stop/reverse an arbitrary animation
          at an arbitrary time (to avoid collision), this would look weird
          for some animations and would require some additional work
          at preparing animations and designing AI (as then "every action can
          be interrupted").

          Also using a bounding volume large enough to account for all
          possible positions is not doable, as it would be too large.
          Consider that for humanoid creatures, walking animation usually
          has tall and thin bounding box (creature stands) but dead/lying animation
          usually has flat and wide bounding box.

          So, only a bounding volume (like a sphere) that
          @italic(may be smaller than bounding volume) can remain constant
          and easily guarantee the assertion "it never collides".

          This means that using such sphere results in simpler collision
          detection routines, as they may assume that collision doesn't occur.
          In contrast, detection routines looking at our (possibly animated)
          BoundingBox must take into account that collision may already be happening,
          and they must incorporate code to allow creatures/players to "get unstruck".)

        @item(Using smaller sphere also allows to naturally ascend the stairs
          and upward slopes. Sphere can move forward slightly, and then creature
          may raise up, to reach it's preferred height. Then sphere can move
          further forward, and so on. This alllows to allow stair climbing
          for creatures without any extra effort in the code.

          The downside is that creature legs will temporarily "sink into the floor"
          when climbing up the stairs. But it's not noticeable if "growing up"
          mechanism works fast enough.)
      )

      Sphere disadvantage:

      @unorderedList(
        @item(Sphere is far from perfect as a bounding volume --- it's too small,
          sometimes also too large, sometimes both at the same time...

          Since the Sphere radius remains always the same, it must be good
          for many creature animation frames. In cases where the sphere
          isn't suitable, and you don't need advantages above --- you can
          make @name return @false.
          E.g. a dead creature may be stuck in a wall,
          and it doesn't have to climb stairs. So you don't really need
          sphere advantages listed above, and @name may return @false
          when creature is in dying state.

          But still it may be a problem sometimes, if some creature states
          have entirely different animations and bounding boxes. Then you
          will be forced to choose one universal Radius for all creature
          states. And you need constant radius to keep the advantage above
          of "guarantee".

          1. Obviously you can't set radius too small, because if it's much smaller
          than actual creature's geometry then the creature will noticeably collide
          with level geometry and other creatures.

          2. On the other hand, you can't set radius too large
          (or move sphere center, Middle, much lower).
          This would block stair climbing.
        )
      ) }
    function Sphere(out Radius: Single): boolean; virtual;

    { Can this object be pushed by (or block movement of) doors, elevators
      and other moving level parts (T3DMoving instances).

      Some 3D moving objects may try to avoid crushing this item.
      Like an automatic door that stops it's closing animation
      to not crush things standing in the doorway.

      Some other 3D moving objects may push this object.
      Like elevators (vertical, or horizontal moving platforms).
      We may use sphere (see @link(T3D.Sphere)) for checking
      collisions, or bounding box (@link(T3D.BoundingBox)), depending on need.
      The item is moved using @link(T3DCustomTransform.Translate), so make sure it
      actually does something (for example, by descending from T3DTransform,
      that provides natural @link(T3DCustomTransform.Translate) implementation). }
    property CollidesWithMoving: boolean read FCollidesWithMoving write FCollidesWithMoving default false;

    { Get height of my point above the rest of the 3D world.

      This ignores the geometry of this 3D object (to not accidentaly collide
      with your own geometry), and checks collisions with the rest of the world.
      @groupBegin }
    function Height(const MyPosition: TVector3Single;
      out AboveHeight: Single): boolean;
    function Height(const MyPosition: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
    { @groupEnd }

    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;

    { Is the move from OldPos to ProposedNewPos possible for me.
      Returns true and sets NewPos if some move is allowed.
      Overloaded version without ProposedNewPos doesn't do wall-sliding,
      and only answers if exactly this move is allowed.

      If this 3D object allows to use sphere as the bounding volume (see @link(Sphere)),
      then this sphere must be centered around OldPos, not some other point.
      That is, we assume that @link(Sphere) returns Center that is equal to OldPos.

      This ignores the geometry of this 3D object (to not accidentaly collide
      with your own geometry), and checks collisions with the rest of the world.
      @groupBegin }
    function MoveAllowed(const OldPos, ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    function MoveAllowed(const OldPos, NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
    { @groupEnd }

    { Cast a ray from myself to the world, see what is hit.

      This ignores the geometry of this 3D object (to not accidentaly collide
      with your own geometry), and checks collisions with the rest of the world. }
    function Ray(const RayOrigin, RayDirection: TVector3Single): TRayCollision;

    { In case this scene shares lights with other scenes,
      this is the source scene. In usual circumstances, this method
      simply returns @code(Self), which means "no sharing".
      In case of scenes that are children of TCastlePrecalculatedAnimation,
      their Shared methods all point to the 1st animation scene. }
    function Shared: T3D; virtual;
  published
    { If this 3D object is rendered as part of TCastleSceneManager,
      and TCastleSceneManager.UseGlobalLights is @true, then this property allows
      to make an exception for this 3D object: even though TCastleSceneManager.UseGlobalLights is @true,
      do not use global lights @italic(for this 3D object).

      Note that this is not applied recursively. Instead, it is checked at each T3D instance
      that checks TRenderParams.BaseLights. In practice, it is only checked at TCastleScene,
      unless you do custom rendering on your own. }
    property ExcludeFromGlobalLights: boolean
      read FExcludeFromGlobalLights write FExcludeFromGlobalLights default false;

    { Exclude from rendering statistics in
      @link(TCastleAbstractViewport.Statistics). }
    property ExcludeFromStatistics: boolean
      read FExcludeFromStatistics write FExcludeFromStatistics default false;
  end;

  { List of 3D objects (T3D instances).

    This inherits from TCastleObjectList, getting many
    features like TList notification mechanism (useful in some situations).
    Usually you want to use T3DList instead, which is a wrapper around
    this class. }
  T3DListCore = class(TCastleObjectList)
  private
    FOwner: T3DList;

    function GetItem(const I: Integer): T3D;
    procedure SetItem(const I: Integer; const Item: T3D);
  public
    constructor Create(const FreeObjects: boolean; const AOwner: T3DList);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Items[I: Integer]: T3D read GetItem write SetItem; default;

    function First: T3D;
    function Last: T3D;

    { T3DList instance that owns this list.
      May be @nil, for example when this list is used by TRayCollision. }
    property Owner: T3DList read FOwner;
  end;

  { List of 3D objects (T3D instances), that can be treated like another,
    larger 3D object.

    It inherits from T3D class, so this list is itself
    a 3D object, representing a sum of all it's children 3D objects.
    This allows you to group many 3D objects, and treat them as one T3D
    descendant. }
  T3DList = class(T3D)
  private
    FList: T3DListCore;
    function GetItem(const I: Integer): T3D;
    procedure SetItem(const I: Integer; const Item: T3D);
  protected
    procedure SetWorld(const Value: T3DWorld); override;

    { Additional child inside the list, always processed before all children
      on the @link(Items) list. By default this method returns @nil,
      indicating no additional child exists.
      The presence of this child can be calculated in overriden
      method using any condition, which is sometimes more comfortable
      than adding item to Items.

      This item cannot be removed by methods like @link(T3DList.Remove)
      or by setting RemoveMe in it's @link(T3D.Update) implementation.
      Presence of this item is completely determined by GetChild implementation. }
    function GetChild: T3D; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    function SphereCollision2D(const Pos: TVector2Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const Details: TCollisionDetails = nil): boolean; override;
    function PointCollision2D(const Point: TVector2Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Operate on 3D objects contained in the list.
      You can also operate directly on @link(List) instance.
      @groupBegin }
    procedure Add(const Item: T3D);
    procedure Insert(const Index: Integer; const Item: T3D);
    procedure Remove(const Item: T3D);
    property Items[I: Integer]: T3D read GetItem write SetItem; default;
    function Count: Integer;
    procedure Clear;
    procedure Exchange(const Index1, Index2: Integer);
    { @groupEnd }

    { Sort objects back-to-front @italic(right now)
      following one of the blending sorting algorithms.
      Only the immediate list items are reordered,
      looking at their bounding boxes.

      Calling this method makes sense if you have a list
      of objects, and some of them are partially-transparent and may
      be visible at the same place on the screen.
      It may even make sense to call this method every frame (like in every
      @link(TCastleWindowCustom.OnUpdate)),
      if you move or otherwise change the objects (changing their bounding boxes),
      or if the CameraPosition may change (note that CameraPosition is only
      relevant if BlendingSort = bs3D).

      Sorting partially-transparent objects avoids artifacts when rendering.

      Note that this doesn't take care of sorting the shapes
      within the scenes. For this, you should set
      @link(TSceneRenderingAttributes.BlendingSort Scene.Attributes.BlendingSort)
      to a value like bs3D, to keep it sorted.
      It is actually the default now.

      See the TBlendingSort documentation for the exact specification
      of sorting algorithms. Using BlendingSort = bsNone does nothing. }
    procedure SortBackToFront(const BlendingSort: TBlendingSort;
      const CameraPosition: TVector3Single);

    { Sort objects back-to-front @italic(right now)
      following the 2D blending sorting algorithm.
      See @link(SortBackToFront) for documentation, this method
      is only a shortcut for @code(SortBackToFront(bs2D, ZeroVector3Single)). }
    procedure SortBackToFront2D;

    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;
    procedure PrepareResources(
      Options: TPrepareResourcesOptions;
      ProgressStep: boolean;
      BaseLights: TAbstractLightInstancesList); override;
    function PrepareResourcesSteps: Cardinal; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure GLContextClose; override;
    procedure UpdateGeneratedTextures(
      const RenderFunc: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const OriginalViewport: TRectangle); override;
    procedure VisibleChangeNotification(const Changes: TVisibleChanges); override;
    procedure CameraChanged(ACamera: TCamera); override;
    function Dragging: boolean; override;
  published
    { 3D objects inside.
      Freeing these items automatically removes them from this list. }
    property List: T3DListCore read FList;
  end;

  { 3D world. List of 3D objects, with some central properties. }
  T3DWorld = class(T3DList)
  public
    OnCursorChange: TNotifyEvent;
    OnVisibleChange: TVisibleChangeEvent;

    constructor Create(AOwner: TComponent); override;

    { See TCastleSceneManager.CollisionIgnoreItem. }
    function CollisionIgnoreItem(const Sender: TObject;
      const Triangle: P3DTriangle): boolean; virtual; abstract;
    { Up vector, according to gravity. Gravity force pulls in -GravityUp direction. }
    function GravityUp: TVector3Single; virtual; abstract;
    { The major axis of gravity vector: 0, 1 or 2.
      This is derived from GravityUp value. It can only truly express
      GravityUp vector values (1,0,0) or (0,1,0) or (0,0,1),
      although in practice this is enough for normal games (normal 3D scenes
      use up either +Y or +Z).

      We try to avoid using it in
      the engine, and use full GravityUp vector wherever possible.
      Full GravityUp vector may allow for more fun with weird gravity
      in future games. }
    function GravityCoordinate: Integer;
    { Player, see TCastleSceneManager.Player. }
    function Player: T3DAlive; virtual; abstract;
    { Base lights, see TCastleSceneManager.BaseLights. }
    function BaseLights: TAbstractLightInstancesList; virtual; abstract;
    { Sectors in the world, for AI. See TCastleSceneManager.Sectors. }
    function Sectors: TSectorList; virtual; abstract;
    { Water volume. See TCastleSceneManager.Water. }
    function Water: TBox3D; virtual; abstract;

    { Collisions with world. They call corresponding methods without the World
      prefix, automatically taking into account some knowledge about this
      3D world.

      Calling these methods to check collisions makes sense if your
      collision query is not initiated by any existing T3D instance.

      If your query originates from some existing T3D instance,
      you usually do not want to call these WorldXxx methods.
      Instead call T3D.MoveAllowed, T3D.Height methods.
      Underneath, they still call @code(World.WorldMoveAllowed) and
      @code(World.WorldHeight),
      additionally making sure that the 3D object does not collide with itself.
      @groupBegin }
    function WorldMoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; virtual; abstract;
    function WorldMoveAllowed(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; virtual; abstract;
    function WorldHeight(const Position: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; virtual; abstract;
    function WorldLineOfSight(const Pos1, Pos2: TVector3Single): boolean; virtual; abstract;
    function WorldRay(const RayOrigin, RayDirection: TVector3Single): TRayCollision; virtual; abstract;
    function WorldSphereCollision(const Pos: TVector3Single; const Radius: Single): boolean;
    function WorldSphereCollision2D(const Pos: TVector2Single; const Radius: Single;
      const Details: TCollisionDetails = nil): boolean;
    function WorldPointCollision2D(const Point: TVector2Single): boolean;
    { @groupEnd }
  end;

  { Transform (move, rotate, scale) other T3D objects.
    Descends from T3DList, transforming all it's children.
    Also adds gravity and related features.

    T3DCustomTransform is an abstract class, that doesn't define
    how the transformation is stored and accessed.
    Descendants define it by overriding protected virtual
    methods like GetTranslation and GetRotation (in this class they return zeros).
    Use T3DTransform to have simple T3DTransform.Translation and such properties.
    Use T3DOrient to have camera-like transformation vectors. }
  T3DCustomTransform = class(T3DList)
  private
    FGravity: boolean;
    FFallingStartMiddle: TVector3Single;
    FFalling: boolean;
    FFallSpeed: Single;
    FGrowSpeed: Single;
    FMiddleHeight: Single;
  protected
    { Workaround for descendants where BoundingBox may suddenly change
      but their logic depends on stable (not suddenly changing) Middle.
      If MiddleForceBox then we will use given MiddleForceBoxValue
      instead of LocalBoundingBox for Middle and PreferredHeight
      calculation. Descendants that deal with this should usually have
      some timeout when they restore MiddleForceBox to false.

      This is quite internal hack and you should not use this in your own programs.
      This is used only by TWalkAttackCreature.
      @exclude
      @groupBegin }
    MiddleForceBox: boolean;
    { @exclude }
    MiddleForceBoxValue: TBox3D;
    { @groupEnd }

    { The GetXxx methods below determine the transformation returned
      by default TransformMatricesMult implementation in this class.
      Simple descendants need only to override these, and OnlyTranslation,
      and the TransformMatricesMult will automatically work correctly already.

      More complicated descendants may override TransformMatricesMult,
      and then GetCenter, GetRotation etc. methods can be ignored
      (if your TransformMatricesMult will not use it, then GetCenter, GetRotation
      will not be used at all and there's no point in overriding them).
      You still need to override

      @unorderedList(
        @item OnlyTranslation
        @item(GetTranslation (it's used by default Middle implementation,
          and it's also used in case OnlyTranslation returns @true),)
        @item(And make sure AverageScale is correct
          (if you want it to be <> 1, that is: if your transformation
          may make some scale, then you need to override GetScale).)
      )

      @groupBegin }
    function GetTranslation: TVector3Single; virtual;
    function GetCenter: TVector3Single; virtual;
    function GetRotation: TVector4Single; virtual;
    function GetScale: TVector3Single; virtual;
    function GetScaleOrientation: TVector4Single; virtual;
    { @groupEnd }

    { Get translation in 2D (uses GetTranslation, ignores Z coord). }
    function GetTranslation2D: TVector2Single;

    { Can we use simple GetTranslation instead of full TransformMatricesMult.
      Returning @true allows optimization in some cases. }
    function OnlyTranslation: boolean; virtual;
    function Transform: TMatrix4Single;
    function TransformInverse: TMatrix4Single;

    { Transformation matrix.
      You can override this to derive transformation using anything,
      not necessarily GetTranslation / GetCenter etc. methods.

      This method must produce matrices that preserve points as points
      and directions as directions in homegeneous space.
      In other words, using MatrixMultPoint or MatrixMultDirection
      with these matrices must never raise ETransformedResultInvalid.
      For example, a combination of translations, rotations, scaling is Ok. }
    procedure TransformMatricesMult(var M, MInverse: TMatrix4Single); virtual;
    procedure TransformMatrices(out M, MInverse: TMatrix4Single);
    function AverageScale: Single;

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
    function SphereCollision2D(const Pos: TVector2Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      const Details: TCollisionDetails): boolean; override;
    function PointCollision2D(const Point: TVector2Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; override;

    { Called when fall ended. You can use FallHeight to decrease creature
      life or such. }
    procedure Fall(const FallHeight: Single); virtual;

    { Untransformed bounding box value. }
    function LocalBoundingBox: TBox3D;
  public
    const
      DefaultMiddleHeight = 0.5;

    constructor Create(AOwner: TComponent); override;
    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;
    function Middle: TVector3Single; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Convert position between local and outside coordinate system.
      This is called OutsideToLocal, not WorldToLocal, because it only handles transformation
      defined in this item --- it does not recursively apply all transform on the way to root
      @groupBegin. }
    function OutsideToLocal(const Pos: TVector3Single): TVector3Single;
    function LocalToOutside(const Pos: TVector3Single): TVector3Single;
    { @groupEnd }

    { Gravity may make this object fall down (see FallSpeed)
      or grow up (see GrowSpeed). See also PreferredHeight.

      Special notes for TPlayer: player doesn't use this (TPlayer.Gravity
      should remain @false), instead player relies on
      TPlayer.Camera.Gravity = @true, that does a similar thing (with some
      extras, to make camera effects). This will change in the future,
      to merge these two gravity implementations.
      Although the TPlayer.Fall method still works as expected
      (it's linked to TWalkCamera.OnFall in this case). }
    property Gravity: boolean read FGravity write FGravity default false;

    { Falling speed, in units per second, for @link(Gravity).
      TODO: this will be replaced with more physically-based approach.

      This is relevant only if @link(Gravity) and PreferredHeight <> 0.
      0 means no falling. }
    property FallSpeed: Single read FFallSpeed write FFallSpeed default 0;

    { Growing (raising from crouching to normal standing position)
      speed, in units per second.
      This is used by non-flying creatures when climbing up stairs,
      in which case GetTranslation ("legs positon") may be sometimes under
      the ground while Middle ("eyes position") will be always above the ground
      and will try to grow to be at PreferredHeight above the ground.

      This is relevant only if @link(Gravity) and PreferredHeight <> 0.
      0 means no growing. }
    property GrowSpeed: Single read FGrowSpeed write FGrowSpeed default 0;

    { The preferred height of the object @link(Middle) above the ground,
      when the object is standing on the ground firmly.
      This is used by objects affected by gravity (like non-flying creatures
      and items) to know how far they should fall down or grow up.

      The default implementation in this class looks at MiddleHeight property,
      see the algorithm described there.
      This may be dynamic (may change during creature lifetime,
      so you can make the creature duck or grow if you want). }
    function PreferredHeight: Single; virtual;

    { How high are creature eyes in the model.
      Value 0 means that eyes are at the bottom of the model,
      0.5 means the middle, 1 means top.

      The @italic(top) is always considered to be at the top of the bounding box.

      Definition of @italic(bottom) depends on @link(Gravity):

      @unorderedList(
        @item(
          When Gravity is @true, then the @italic(bottom) is considered to be
          the plane where World.GravityCoordinate (like Z or Y axis) is zero.
          The actual bottom (lowest point) of the bounding box doesn't matter.
          This means that things placed below zero plane (like a creature tentacle
          or leg) will sink into the ground, instead of causing whole creature
          to move up. It also means that the creature can easily float above
          the ground, just model it a little above the zero plane.

          In other words, this allows you to model the creature
          with respect to the ground (zero plane), which is comfortable.

          Note that setting MiddleHeight to exact 0 means that gravity will not work,
          as it means that the PreferredHeight above the ground
          is to be stuck right at the ground level.

          For gravity to work right, the MiddleHeight should be large enough
          to cause PreferredHeight to be > @link(Sphere) radius,
          for all possible animation states (for all possible bounding box values).
        )

        @item(
          When Gravity is @false, then the @italic(bottom) is considered
          at the bottom of the bounding box.

          This way it works regardless of where (0,0,0) is in your model
          (regardless if (0,0,0) represents legs, or middle of your creature),
          since we adjust to the BoundingBox position.
        )
      )

      This property determines how the T3DCustomTransform
      handles the @link(Middle) implementation
      (this is the point used for various collision detection routines)
      and @link(PreferredHeight) (this is the preferred height of @link(Middle)
      above the ground). You can override these two methods to use a different
      approach, and then ignore MiddleHeight completely. }
    property MiddleHeight: Single read FMiddleHeight write FMiddleHeight
      default DefaultMiddleHeight;

    { Unconditionally move this 3D object by given vector.
      You usually don't want to use this directly, instead use @link(Move)
      method to move checking collisions (and with optional wall sliding). }
    procedure Translate(const T: TVector3Single); virtual; abstract;

    { Move, if possible (no collisions). This is the simplest way to move
      a 3D object, and a basic building block for artificial intelligence
      of creatures.

      Checks move possibility by MoveAllowed, using @link(Middle) point.
      Actual move is done using @link(Translate). }
    function Move(const Translation: TVector3Single;
      const BecauseOfGravity: boolean;
      const EnableWallSliding: boolean = true): boolean;
  end;

  { Transform (move, rotate, scale) children T3D objects.
    Transformation is a combined 1. @link(Translation),
    2. and @link(Rotation) around @link(Center) point,
    3. and @link(Scale) around @link(Center) and with orientation given by
    @link(ScaleOrientation).

    For precise order of the translation/rotation/scale operations,
    see the X3D Transform node specification.

    Default values of all fields indicate "no transformation".
    So everything is zero, except Scale is (1,1,1).

    This descends from T3DList, and it transforms all it's children. }
  T3DTransform = class(T3DCustomTransform)
  private
    FCenter: TVector3Single;
    FRotation: TVector4Single;
    FScale: TVector3Single;
    FScaleOrientation: TVector4Single;
    FTranslation: TVector3Single;
    FOnlyTranslation: boolean;
  protected
    procedure SetCenter(const Value: TVector3Single);
    procedure SetRotation(const Value: TVector4Single);
    procedure SetScale(const Value: TVector3Single);
    procedure SetScaleOrientation(const Value: TVector4Single);
    procedure SetTranslation(const Value: TVector3Single);

    function OnlyTranslation: boolean; override;

    function GetCenter: TVector3Single; override;
    function GetRotation: TVector4Single; override;
    function GetScale: TVector3Single; override;
    function GetScaleOrientation: TVector4Single; override;
    function GetTranslation: TVector3Single; override;
  public
    constructor Create(AOwner: TComponent); override;

    { Translation (move) the children. Zero by default. }
    property Translation: TVector3Single read FTranslation write SetTranslation;

    { Center point around which the @link(Rotation) and @link(Scale) is performed. }
    property Center: TVector3Single read FCenter write SetCenter;

    { Rotation in 3D, around a specified axis.
      Rotation is expressed as a 4D vector, in which the first 3 components
      specify the rotation axis (does not need to be normalized, but must be non-zero),
      and the last component is the rotation angle @italic(in radians).

      Rotation is done around @link(Center). }
    property Rotation: TVector4Single read FRotation write SetRotation;

    { Scale in 3D. Scaling is done around @link(Center)
      and with orientation given by @link(ScaleOrientation).

      We do the best we can to work with @italic(any) scale value,
      even negative or zero. But usually, it's best to keep the scale
      positive. More information:

      @orderedList(
        @item(If you can, keep the scale uniform, that is scale equal amount
          in X, Y and Z. For example set scale = @code((3.0, 3.0, 3.0))
          to scale 3x times, and avoid scale like @code((3.0, 1.0, 1.0))
          that scales more in one direction.

          Non-uniform scale works, but some collisions are not perfectly
          calculated then. (For example, an ideal sphere is no longer a sphere
          when scaled in non-uniform fashion, and not everywhere do we
          account for that.) Although it works Ok on meshes.
          @link(ScaleOrientation) matters in case of non-uniform scale.
        )

        @item(All scale components should > 0 if you want 3D lighting
          to work corrrectly. That is, avoid negative scale, that flips
          the orientation of faces (CCW becomes CW), or standard
          lighting may not work Ok.

          For unlit stuff, or custom lighting, negative scale may be Ok.
          For many 2D games that use no lighting/custom lighting,
          negative scale is Ok.)

        @item(At least, keep all scale components non-zero.
          Otherwise the scaling operation is not invertible,
          and generally collisions will not work correctly.

          If you really need to set zero scale, at least consider
          using @link(Collides) = @false.)
      )
    }
    property Scale: TVector3Single read FScale write SetScale;

    { Orientation in which 3D @link(Scale) is performed. }
    property ScaleOrientation: TVector4Single read FScaleOrientation write SetScaleOrientation;

    procedure Translate(const T: TVector3Single); override;

    { Make the transform do nothing --- zero @link(Translation), zero @link(Rotation),
      @link(Scale) to one. Also resets @link(ScaleOrientation). }
    procedure Identity;
  end;

  TOrientationType = (
    { Sensible for worlds oriented around Y axis.
      That is when gravity pulls in -Y and GravityUp vector is +Y.
      Transformation makes -Z and +Y match (respectively) Direction and Up.

      This matches default direction/up of OpenGL and VRML/X3D cameras.

      For example, using this value for T3DOrient.Orientation (or even
      T3DOrient.DefaultOrientation) is sensible if you use default
      Blender X3D exporter, and you let the exporter to make
      a transformation (to make +Z up into +Y up). This is the default setting.
      Then you can follow the standard Blender view names
      ("front", "top" and such) when modelling, and Blender tools like
      "X-axis mirror" will work best. }
    otUpYDirectionMinusZ,

    { Sensible for worlds oriented around Z axis.
      Transformation makes -Y and +Z match (respectively) Direction and Up.

      Using this value for T3DOrient.Orientation (or even
      T3DOrient.DefaultOrientation) is sensible if you export your models
      from Blender @italic(without transforming them during export).
      Note that @italic(this is not the default Blender X3D exporter behavior).
      But you can configure the exporter to work like this (not transform),
      and then you can follow the standard Blender view names
      ("front", "top" and such) when modelling. }
    otUpZDirectionMinusY,

    { @deprecated Up in +Z (like otUpZDirectionMinusY) and direction
      in +X. Should not be used in new models. }
    otUpZDirectionX
  );

  { Transform other 3D objects by changing their orientation.

    The rotation of objects depends on given Direction and Up vectors,
    see @link(Orientation) for details.
    The translation of objects is just taken from @link(Position),
    and works just like normal T3DTransform.Translation.
    There is no scaling of 3D objects, ever. }
  T3DOrient = class(T3DCustomTransform)
  private
    FCamera: TWalkCamera;
    FOrientation: TOrientationType;
    function GetPosition: TVector3Single;
    function GetDirection: TVector3Single;
    function GetUp: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
    procedure SetDirection(const Value: TVector3Single);
    procedure SetUp(const Value: TVector3Single);
  protected
    procedure TransformMatricesMult(var M, MInverse: TMatrix4Single); override;
    function OnlyTranslation: boolean; override;
    { T3DOrient overrides GetTranslation to return Position, this will be used
      by T3DCustomTransform.Middle. }
    function GetTranslation: TVector3Single; override;
  public
    { Default value of T3DOrient.Orientation, for new instances of T3DOrient
      (creatures, items, player etc.). }
    DefaultOrientation: TOrientationType; static;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Position (translation) of this 3D object. }
    property Position: TVector3Single read GetPosition write SetPosition;

    { Direction the creature is facing, and up vector.

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). When setting them by these properties, we will normalize
      them automatically.

      They must also always be orthogonal.
      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      Initially, they follow VRML/X3D standard vectors suitable for gravity along
      the Y axis. So direction is -Z (DefaultCameraDirection),
      up is +Y (DefaultCameraUp).

      @groupBegin }
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Up: TVector3Single read GetUp write SetUp;
    { @groupEnd }

    { Set at once vectors: position, direction, up.

      ADir and AUp given here do not have to be normalized
      (they will be normalized if needed).
      They will be automatically fixed to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const APos, ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true);
    procedure SetView(const ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true);

    { Change up vector, keeping the direction unchanged.
      If necessary, the up vector provided here will be fixed to be orthogonal
      to direction.

      This is similar to assigning @link(Up) vector using it's property setter,
      but different behavior happens when we need to fix vectors to have
      direction orthogonal to up (which must be always true).
      In case of assigning @link(Up) by property setter,
      the @link(Direction) vector is changed (if necessary, to be orthogonal to up).
      In case of this method, the up vector is changed (if necessary,
      to be orthogonal to direction).

      It's good to use this if you have a preferred up vector for creatures,
      but still preserving the direction vector has the highest priority. }
    procedure UpPrefer(const AUp: TVector3Single);

    procedure Translate(const T: TVector3Single); override;

    { How the direction and up vectors determine transformation.
      See TOrientationType for values documentation.

      The default value of this is determined by static variable
      DefaultOrientation, this is usually comfortable (because almost
      always you use the same Orientation throughout your game).
      By default it's otUpYDirectionMinusZ (matching default cameras
      of OpenGL and VRML/X3D).

      This value determines how you should model your 3D models,
      like the creatures, the items, and the player weapons.
      Generally, it applies to every 3D model that is used as
      a child of this T3DOrient instance. }
    property Orientation: TOrientationType read FOrientation write FOrientation;

    { Camera, with view vectors (position, direction and up)
      always synchronized with this T3DOrient instance.
      You should not set Camera vectors (by TWalkCamera.Position,
      TWalkCamera.SetView and such) directly, instead use this
      object's properties (T3DOrient.Position, T3DOrient.SetView),
      as we will call proper VisibleChangeHere method.

      We don't deal with any other camera properties in T3DOrient.
      If you want, you can ignore this camera (you will probably do this
      if you use T3DOrient for creature like TCastleCreature;
      although camera may still have a fun usage then, for observing world from
      a creature view).
      Or you can use this camera, taking care of all it's settings,
      even asssigning this camera to TCastleSceneManager.Camera
      to allow user to directly control it (you will probably
      do this if you use T3DOrient for player like TPlayer;
      in fact, TGameSceneManager.LoadLevel does this automatically for you). }
    property Camera: TWalkCamera read FCamera;
  end;

  { Deprecated name for T3DCustomTransform. @deprecated @exclude }
  T3DCustomTranslated = T3DCustomTransform deprecated;

  { Deprecated name for T3DTransform. @deprecated @exclude }
  T3DTranslated = T3DTransform deprecated;

  { 3D object moving and potentially pushing other 3D objects.
    Good for elevators, doors and such.

    Other 3D objects may be pushed, if @link(Pushes).
    There are two methods of pushing available, see @link(PushesEverythingInside).
    Only the 3D objects with @link(T3D.CollidesWithMoving) are ever pushed by
    this object (the rest of 3D world is treated as static, does not interact with
    elevators / doors or such).

    You can also stop/reverse the move to prevent some collisions
    from occuring at all. This way you can e.g. prevent the door
    from automatically closing, if someone/something blocks the way.
    You do this by overriding BeforeTimeIncrease.
    See TDoomLevelDoor.BeforeTimeIncrease in "The Castle" for example how to
    do this. }
  T3DMoving = class(T3DCustomTransform)
  private
    FPushes: boolean;
    FPushesEverythingInside: boolean;
    FAnimationTime: TFloatTime;
  protected
    { Local object time, always increasing, used to track animations. }
    property AnimationTime: TFloatTime read FAnimationTime;

    { Implements T3D.GetTranslation by always calling
      GetTranslationFromTime(AnimationTime).
      Descendants should only override GetTranslationFromTime. }
    function GetTranslation: TVector3Single; override;
    function OnlyTranslation: boolean; override;

    function GetTranslationFromTime(const AnAnimationTime: TFloatTime):
      TVector3Single; virtual; abstract;

    { Do something right before animation progresses.
      Called at the beginning of our @link(Update),
      @italic(right before) AnimationTime changes to NewAnimationTime.

      Useful for taking care of collision detection issues,
      as our assumption always is that "nothing collides". Which means
      that if you don't want your T3DMoving to collide
      with e.g. player or creatures or items, then you should
      prevent the collision @italic(before it happens).
      This is the place to do it. }
    procedure BeforeTimeIncrease(const NewAnimationTime: TFloatTime); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Translate(const T: TVector3Single); override;
  published
    { Are other 3D objects pushed when this object moves.
      Only the 3D objects with @link(T3D.CollidesWithMoving) are ever pushed by this object
      (the rest of 3D world is treated as static, does not interact with
      elevators / doors or such).

      Only relevant if GetCollides. Non-colliding objects never push others. }
    property Pushes: boolean read FPushes write FPushes default true;

    { If @link(Pushes) is @true, this determines how pushing actually works.
      There two methods:

      @orderedList(
        @item(PushesEverythingInside = @true: We move every
          3D object that is inside our bounding box and has CollidesWithMoving=@true.
          This is sensible if we can reasonably assume that things
          inside our box are standing. For example if this is
          a (vertical or horizontal) elevator, then creatures/items
          are usually standing/lying inside, and naturally move with
          the same speed (and direction) as the elevator.)

        @item(When PushesEverythingInside = @false: We check precise
          collision between 3D objects with CollidesWithMoving=@true
          and our triangle mesh.
          Actually, we use T3DList.BoxCollision / T3DList.SphereCollsion,
          that will use children's T3D.BoxCollision / T3D.SphereCollsion;
          they check collisions with triangle mesh in case of TCastleScene
          with Spatial containing e.g. ssDynamicCollisions.)
      )

      Neither method is really perfect.

      PushesEverythingInside = @false seems like a more precise check,
      as it actually compares the triangle mesh, taking into account
      the interior of (this) moving 3D object. PushesEverythingInside = @true
      just approximates the moving 3D object by it's bounding box.

      On the other hand, PushesEverythingInside = @true makes the elevator
      more "sticky". With PushesEverythingInside = @false,
      when player hits the floor, it takes them some time to raise up.
      This creates a "bouncing camera" effect when the elevator goes up
      quickly: player constantly falls to the ground, tries to get up,
      but elevator moves up and player falls to it's ground again.
      When the elevator goes down, the player/creature constantly falls
      down on it because of gravity, which again causes artifacts
      as gravity may work significantly slower/faster than elavator moving speed.
      When the elevator is a horizontal moving platform, it will "slip"
      from under the player/creature, leaving the poor fella suddenly hanging
      in the air, and falling down because of gravity in the next second.

      In practice: PushesEverythingInside should be @true for small
      containers, when you can reasonably assume that things (creatures,
      player, items) stand inside, and when you intend to use it for transport
      of 3D stuff. For very large moving stuff, that possibly
      interacts with flying players/creatures in some creative way,
      PushesEverythingInside may be @false. }
    property PushesEverythingInside: boolean
      read FPushesEverythingInside write FPushesEverythingInside default true;
  end;

  { 3D moving with constant speed between 2 points.
    Moves with a constant speed from (0, 0, 0) to TranslationEnd.
    They are called @italic(begin position) and @italic(end position).

    This is a simplified, more comfortable descendant of T3DMoving.
    You get easy to use GoBeginPosition, GoEndPosition
    properties, you can easily set sounds by SoundGoBeginPosition and
    SoundGoEndPosition and such. }
  T3DLinearMoving = class(T3DMoving)
  private
    FEndPosition: boolean;
    FEndPositionStateChangeTime: Single;

    FSoundGoBeginPosition: TSoundType;
    FSoundGoEndPosition: TSoundType;
    FSoundGoBeginPositionLooping: boolean;
    FSoundGoEndPositionLooping: boolean;
    FSoundTracksCurrentPosition: boolean;

    UsedSound: TSound;
    procedure SoundRelease(Sender: TSound);
    function SoundPosition: TVector3Single;
    procedure PlaySound(SoundType: TSoundType; Looping: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Is this object in @italic(end position), or going to it.
      If @false, then this object is in @italic(begin position)
      or going to it. See also CompletelyEndPosion and CompletelyBeginPosition.

      Initially this is @false, and EndPositionStateChangeTime is set such that
      we're sure that we're in CompletelyBeginPosion, }
    property EndPosition: boolean read FEndPosition;

    { Last time EndPosition changed. }
    property EndPositionStateChangeTime: Single read FEndPositionStateChangeTime;

    function CompletelyEndPosition: boolean;
    function CompletelyBeginPosition: boolean;

    { Start going to @italic(begin position), assuming that
      currently we're in @italic(end position) (i.e. CompletelyEndPosion). }
    procedure GoBeginPosition;

    { Start going to @italic(end position), assuming that
      currently we're in @italic(begin position) (i.e. CompletelyBeginPosion). }
    procedure GoEndPosition;

    { Stop going from @italic(end position) to @italic(begin position)
      and go back to @italic(end position). Call this only when currently
      EndPosition is @false and we were in the middle of going to
      @italic(begin position).

      As an example, this is what happens when door on DOOM level gets blocked.
      In the middle of closing (which ig going to @italic(begin position))
      it will realize that something blocks it, and open back
      (go back to @italic(end position)).  }
    procedure RevertGoEndPosition;

    { Just like RevertGoEndPosition, but this should be used in the middle
      of the move from @italic(begin position) to @italic(end position),
      to go back to @italic(begin position). }
    procedure RevertGoBeginPosition;

    { This goes to the @italic(other) position.
      Which means that if we're completely in @italic(end position)
      or in the middle of move to @italic(end position), this goes
      back to @italic(begin position). And if we're in @italic(begin position),
      this goes back to @italic(end position). }
    procedure GoOtherPosition;

    property SoundGoBeginPosition: TSoundType
      read FSoundGoBeginPosition write FSoundGoBeginPosition;
    property SoundGoEndPosition: TSoundType
      read FSoundGoEndPosition write FSoundGoEndPosition;

    property SoundGoBeginPositionLooping: boolean
      read FSoundGoBeginPositionLooping write FSoundGoBeginPositionLooping
      default false;
    property SoundGoEndPositionLooping: boolean
      read FSoundGoEndPositionLooping write FSoundGoEndPositionLooping
      default false;

    { If @true then the sound (set by SoundGoBeginPosition or
      SoundGoEndPosition) 3D position changes as the 3D position of the object
      changes.

      Otherwise (default) sound is initially made at initial
      3D position of this object, and then the sound position doesn't change
      (even if the position of the object changes). }
    property SoundTracksCurrentPosition: boolean
      read FSoundTracksCurrentPosition write FSoundTracksCurrentPosition
      default false;
  public
    MoveTime: Single;
    TranslationEnd: TVector3Single;

    function GetTranslationFromTime(const AnAnimationTime: TFloatTime):
      TVector3Single; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Alive, oriented 3D object. Basis for players, creatures and everything
    else that has some position, direction and that can be killed.

    Note that the T3DAlive doesn't remove dead objects, doesn't make any
    dead animations or such. T3DAlive class merely keeps track of
    @link(Life), @link(Dead) and such properties,
    and allows you to call @link(Hurt) doing eventual knockback.
    If your own code doesn't call @link(Hurt),
    or even doesn't look at @link(Life) value, then they have no implication
    for given 3D object, so it may be indestructible just like other 3D objects. }
  T3DAlive = class(T3DOrient)
  private
    FLifeTime: Single;
    FDieTime: Single;
    FLife: Single;
    FMaxLife: Single;
    { FKnockbackDistance <= 0 means "no knockback currently" }
    FKnockbackDistance: Single;
    FLastHurtDirection: TVector3Single;
    { Same as LastHurtDirection but (for things with Gravity) flattened
      to be orthogonal to World.Gravity. This prevents from "pushing" creatures
      into the floor by hitting them in downward direction, which is often
      too easy for non-flying creatures that have Sphere with Middle point high. }
    FLastHurtDirectionGround: TVector3Single;
    FKnockBackSpeed: Single;
  protected
    procedure SetLife(const Value: Single); virtual;
    procedure CancelKnockback;
  public
    const
      DefaultKnockBackSpeed = 1.0;

    constructor Create(AOwner: TComponent); override;

    { Shortcut for checking Life <= 0. }
    function Dead: boolean;

    { Hurt given creature, decreasing it's life by LifeLoss,
      setting last attack direction (used by knockback and some other effects),
      optionally doing a knockback.
      If all you want to do is to decrease Life, you can also just set @link(Life)
      property. Unless your code depends on LastHurtDirection being always updated
      (only TCreature in CastleCreatures unit depends on it now).

      HurtDirection should be a normalized vector indicating direction
      in which the attack came.

      AKnockbackDistance, if non-zero, indicates to push creature by given
      length in the direction given by HurtDirection.
      Ignored if HurtDirection is zero.

      Attacker is the other alive creature that caused this damage. It may be @nil
      if no other T3DAlive is directly responsible for this damage. This may
      be useful for various purposes, for example the victim may become aware
      of attacker presence when it's attacked. }
    procedure Hurt(const LifeLoss: Single;
      const HurtDirection: TVector3Single;
      const AKnockbackDistance: Single; const Attacker: T3DAlive); virtual;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Direction from where the attack came.
      Zero if there was no specific direction of last attack,
      otherwise a normalized (length 1) vector. }
    property LastHurtDirection: TVector3Single read FLastHurtDirection;

    property LifeTime: Single read FLifeTime;

    { Time of death, only valid if @link(Dead), taken from LifeTime. }
    property DieTime: Single read FDieTime;
  published
    { Current Life. We're dead when this is <= 0. }
    property Life: Single read FLife write SetLife;

    { Maximum amount of life. Used as default value for Life when sensible.
      Can be also used for information (to display on player HUDs and such).

      It's not really a limit, that is you can set Life
      to something larger than MaxLife if you want. It's normal in some games,
      where you can get some "magic life boost" that makes your health temporarily
      larger than normal. Whether it's sensible in your game (and whether your
      HUD will display it sensibly) is up to you. }
    property MaxLife: Single read FMaxLife write FMaxLife;

    { Scales how far the knockback effect pushes this creature/player. }
    property KnockBackSpeed: Single read FKnockBackSpeed write FKnockBackSpeed
      default DefaultKnockBackSpeed;
  end;

  T3DExistsEvent = function(const Item: T3D): boolean of object;

const
  MaxSingle = Math.MaxSingle;

  { Default values common to TPlayer and TCreature classes.

    Note that FallMinHeightToSound is usually better to be larger for player,
    to avoid making "fall" sound when player merely jumps or walks down a steep
    hill. No such need for creature.

    @groupBegin }
  DefaultFallMinHeightToDamage = 5.0;
  DefaultFallDamageScaleMin = 0.8;
  DefaultFallDamageScaleMax = 1.2;
  DefaultCreatureFallMinHeightToSound = 1.0;
  DefaultPlayerFallMinHeightToSound = 4.0;
  DefaultCreatureFallSoundName = 'creature_fall';
  DefaultPlayerFallSoundName = 'player_fall';
  { @groupEnd }

  DirectionFromOrientation: array [TOrientationType] of TVector3Single =
  ( (0, 0, -1),
    (0, -1, 0),
    (1, 0, 0) );
  UpFromOrientation: array [TOrientationType] of TVector3Single =
  ( (0, 1, 0),
    (0, 0, 1),
    (0, 0, 1) );

{ Apply transformation to a matrix.
  Calculates at the same time transformation matrix, and it's inverse,
  and multiplies given Transform, TransformInverse appropriately.
  The precise meaning of Center, Translation and such parameters
  follows exactly the X3D Transform node definition (see
  http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/group.html#Transform ).

  @param(Rotation Rotation is expressed as a 4D vector,
    in which the first 3 components
    specify the rotation axis (does not need to be normalized, but must be non-zero),
    and the last component is the rotation angle @italic(in radians).)
}
procedure TransformMatricesMult(var Transform, TransformInverse: TMatrix4Single;
  const Center: TVector3Single;
  const Rotation: TVector4Single;
  const Scale: TVector3Single;
  const ScaleOrientation: TVector4Single;
  const Translation: TVector3Single);

var
  { Creatures, items and possibly other 3D stuff may look at these variables
    to display additional features of 3D objects, helpful to debug collisions,
    AI and other things.
    @groupBegin }
  RenderDebug3D: boolean = false;
  RenderDebugCaptions: boolean = false;
  { @groupEnd }

  { Log shadow volume information.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogShadowVolumes: boolean = false;

implementation

uses CastleLog;

{ TRayCollision --------------------------------------------------------------- }

function TRayCollision.IndexOfItem(const Item: T3D): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Item = Item then Exit;
  Result := -1;
end;

{ TCollisionDetails ------------------------------------------------------------ }

function TCollisionDetails.IndexOfItem(const Item: T3D): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Item = Item then Exit;
  Result := -1;
end;

procedure TCollisionDetails.Add(const Item: T3D);
var
  NewItem: PCollisionDetailsItem;
begin
  NewItem := inherited Add();
  NewItem^.Item := Item;
end;

{ TRenderParams -------------------------------------------------------------- }

constructor TRenderParams.Create;
begin
  inherited;
  RenderTransform := IdentityMatrix4Single;
  RenderTransformIdentity := true;
end;

{ T3D -------------------------------------------------------------------- }

constructor T3D.Create(AOwner: TComponent);
begin
  inherited;
  FCastShadowVolumes := true;
  FExists := true;
  FCollides := true;
  FPickable := true;
  FCursor := mcDefault;
end;

destructor T3D.Destroy;
begin
  { set to nil, to detach free notification }
  SetWorld(nil);
  GLContextClose;
  inherited;
end;

procedure T3D.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
end;

procedure T3D.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
end;

procedure T3D.PrepareResources(Options: TPrepareResourcesOptions;
  ProgressStep: boolean; BaseLights: TAbstractLightInstancesList);
begin
end;

function T3D.PrepareResourcesSteps: Cardinal;
begin
  Result := 0;
end;

function T3D.Press(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function T3D.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function T3D.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := false;
end;

function T3D.PointingDeviceMove(const Pick: TRayCollisionNode;
  const Distance: Single): boolean;
begin
  Result := false;
end;

procedure T3D.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
end;

procedure T3D.VisibleChangeHere(const Changes: TVisibleChanges);
begin
  if (World <> nil) and Assigned(World.OnVisibleChange) then
    World.OnVisibleChange(Self, Changes);
end;

procedure T3D.VisibleChangeNotification(const Changes: TVisibleChanges);
begin
end;

procedure T3D.CameraChanged(ACamera: TCamera);
begin
end;

procedure T3D.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if (World <> nil) and Assigned(World.OnCursorChange) then
      World.OnCursorChange(Self);
  end;
end;

procedure T3D.GLContextClose;
begin
end;

function T3D.HeightCollision(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
var
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  AboveHeight := MaxSingle;
  AboveGround := nil;

  Result := GetCollides and
    { Using TryRayEntrance here would also be sensible, but sometimes too eager:
      In case creature walks over an item, it would cause the item to go upward
      (because the creature is collidable (item is not), so item's gravity
      would cause it to grow). Sometimes also the creatures would too easily
      climb on top of each other.
      It may be changed in the future back into TryRayEntrance? Item problems
      could be solved by using GrowSpeed = 0 for items. }
    BoundingBox.TryRayClosestIntersection(Intersection, IntersectionDistance, Position, -GravityUp);
  if Result then
    AboveHeight := IntersectionDistance;
end;

function T3D.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  { A simple implementation, just don't do wall-sliding. }
  Result := MoveCollision(OldPos, ProposedNewPos, IsRadius, Radius, OldBox, NewBox,
    TrianglesToIgnoreFunc);
  if Result then
    NewPos := ProposedNewPos;
end;

function T3D.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  MyBox: TBox3D;

  { P1 is closer to our middle than P2. }
  function CloserToMiddle(const P1, P2: TVector3Single): boolean;
  var
    M: TVector3Single;
  begin
    M := Middle;
    Result := PointsDistanceSqr(M, P1) < PointsDistanceSqr(M, P2);
  end;

var
  OldCollision, NewCollision: boolean;
begin
  { check collision with our bounding box.

    We do not look here at our own sphere. When other objects move,
    it's better to treat ourself as larger (not smaller), to prevent
    collisions rather then allow them in case of uncertainty.
    So we ignore Self.Sphere method.

    But we do take into account that other (moving) object may prefer to
    be treated as a sphere, so we take into account IsRadius, Radius parameters.
    This allows a player to climb on top of dead corpses (with flat
    bbox), since player's sphere is slightly above the ground.
    And it allows the missiles (like arrow) to use their spheres
    for determining what is hit, which is good because e.g. arrow
    has a very large bbox, sphere is much better (otherwise it may be too easy
    to hit with arrow). }

  Result := true;

  if GetCollides then
  begin
    MyBox := BoundingBox;

    if IsRadius then
    begin
      OldCollision := MyBox.SphereCollision(OldPos, Radius);
      NewCollision := MyBox.SphereCollision(NewPos, Radius);
    end else
    begin
      OldCollision := MyBox.Collision(OldBox);
      NewCollision := MyBox.Collision(NewBox);
    end;

    if NewCollision then
    begin
      { We now know that we have a collision with new position.
        Strictly thinking, move should be disallowed
        (we should exit with false). But it's not that simple.

        There is a weakness in collision checking with dynamic objects,
        like creatures, because when LifeTime changes then effectively
        BoundingBox changes, and there is no way how I can prevent collisions
        from occuring (we cannot stop/reverse an arbitrary animation,
        this would look bad and require AI preparations, see @link(Sphere) comments).

        So we must allow some moves, to allow player/creature that is already
        stuck (already collidable with Self) to get out of the collision.
        To do this, we are going to enable a move, only if *old position
        was already collidable (so the other object is stuck with us already)
        and new position is further from us (so the other object tries
        to get unstuck)". }
      if (not OldCollision) or CloserToMiddle(NewPos, OldPos) then
        Exit(false);
    end else
    if (not OldCollision) and
       { new and old positions are Ok (not collidable), so check also
         line segment. Otherwise fast moving player could run through slim
         creature. }
       MyBox.SegmentCollision(OldPos, NewPos) then
      Exit(false);
  end;

{ Simpler implementation that doesn't allow others to become "unstuck".
  It's also slightly less optimal, as internally BoundingBox and GetCollides
  will be calculated many times (although they should be lighting-fast,
  still their time matters, as this is the basis of our AI and may be called
  many times per frame).
  OTOH, this simpler version is a little cleaner: it delegates work
  to other methods, they may use BoundingBox or something else.

  if IsRadius then
    Result := not ( GetCollides and
      ( SegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        SphereCollision(ProposedNewPos, Radius, TrianglesToIgnoreFunc) ) ) else
    Result := not ( GetCollides and
      ( SegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        BoxCollision(NewBox, TrianglesToIgnoreFunc) ) );
}
end;

function T3D.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  Result := (GetCollides or (ALineOfSight and GetExists)) and
    BoundingBox.SegmentCollision(Pos1, Pos2);
end;

function T3D.SphereCollision(const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetCollides and BoundingBox.SphereCollision(Pos, Radius);
end;

function T3D.SphereCollision2D(const Pos: TVector2Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
begin
  Result := GetCollides and BoundingBox.SphereCollision2D(Pos, Radius);

  if Result and (Details <> nil) then
  begin
    Details.Clear;
    Details.Add(Self);
  end;
end;

function T3D.PointCollision2D(const Point: TVector2Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetCollides and BoundingBox.PointInside2D(Point);
end;

function T3D.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetCollides and BoundingBox.Collision(Box);
end;

function T3D.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  Intersection: TVector3Single;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
begin
  if GetPickable and
    BoundingBox.TryRayEntrance(Intersection, IntersectionDistance, RayOrigin, RayDirection) then
  begin
    Result := TRayCollision.Create;
    Result.Distance := IntersectionDistance;

    NewNode := Result.Add;
    NewNode^.Item := Self;
    NewNode^.Point := Intersection;
    { better T3D implementation could assign here something nice to NewNode^.Triangle,
      to inform T3D.PointingDeviceMove/Activate about the intersected material. }
    NewNode^.Triangle := nil;
    NewNode^.RayOrigin := RayOrigin;
    NewNode^.RayDirection := RayDirection;
  end else
    Result := nil;
end;

procedure T3D.UpdateGeneratedTextures(
  const RenderFunc: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const OriginalViewport: TRectangle);
begin
end;

function T3D.Dragging: boolean;
begin
  Result := false;
end;

function T3D.GetExists: boolean;
begin
  Result := FExists and (Disabled = 0);
end;

function T3D.GetCollides: boolean;
begin
  Result := FCollides and GetExists;
end;

function T3D.GetPickable: boolean;
begin
  Result := FPickable and GetExists;
end;

function T3D.Middle: TVector3Single;
begin
  Result := ZeroVector3Single;
end;

function T3D.Sector: TSector;
begin
  if (World <> nil) and (World.Sectors <> nil) then
    Result := World.Sectors.SectorWithPoint(Middle) else
    Result := nil;
end;

function T3D.Sphere(out Radius: Single): boolean;
begin
  Result := false;
  Radius := 0;
end;

procedure T3D.Disable;
begin
  Inc(Disabled);
end;

procedure T3D.Enable;
begin
  Dec(Disabled);
end;

procedure T3D.SetWorld(const Value: T3DWorld);
begin
  if FWorld <> Value then
  begin
    if FWorld <> nil then
    begin
      { Do not call RemoveFreeNotification when FWorld is also our list owner,
        this would prevent getting notification in T3DList.Notification. }
      if (FWorld.List = nil) or (FWorld.List.IndexOf(Self) = -1) then
        FWorld.RemoveFreeNotification(Self);
    end;
    FWorld := Value;
    if FWorld <> nil then
      // Ignore FWorld = Self case, when this is done by T3DWorld.Create? No need to.
      //and (FWorld <> Self) then
      FWorld.FreeNotification(Self);
  end;
end;

procedure T3D.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  { make sure to nil FWorld reference }
  if (Operation = opRemove) and (AComponent = FWorld) then
    SetWorld(nil);
end;

function T3D.Height(const MyPosition: TVector3Single;
  out AboveHeight: Single): boolean;
var
  AboveGroundIgnored: P3DTriangle;
begin
  Result := Height(MyPosition, AboveHeight, AboveGroundIgnored);
end;

function T3D.Height(const MyPosition: TVector3Single;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  Disable;
  try
    Result := World.WorldHeight(MyPosition, AboveHeight, AboveGround);
  finally Enable end;
end;

function T3D.LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
begin
  Disable;
  try
    Result := World.WorldLineOfSight(Pos1, Pos2);
  finally Enable end;
end;

function T3D.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
var
  Sp: boolean;
  SpRadius: Single;
  OldBox, NewBox: TBox3D;
begin
  { save bounding volume information before calling Disable, as Disable makes
    bounding volume empty }
  Sp := Sphere(SpRadius);
  if not Sp then
    SpRadius := 0; { something predictable, for safety }
  OldBox := BoundingBox;
  NewBox := OldBox.Translate(ProposedNewPos - OldPos);

  Disable;
  try
    Result := World.WorldMoveAllowed(OldPos, ProposedNewPos, NewPos,
      Sp, SpRadius, OldBox, NewBox, BecauseOfGravity);
  finally Enable end;
end;

function T3D.MoveAllowed(
  const OldPos, NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
var
  Sp: boolean;
  SpRadius: Single;
  OldBox, NewBox: TBox3D;
begin
  { save bounding volume information before calling Disable, as Disable makes
    bounding volume empty }
  Sp := Sphere(SpRadius);
  if not Sp then
    SpRadius := 0; { something predictable, for safety }
  OldBox := BoundingBox;
  NewBox := OldBox.Translate(NewPos - OldPos);

  Disable;
  try
    Result := World.WorldMoveAllowed(OldPos, NewPos,
      Sp, SpRadius, OldBox, NewBox, BecauseOfGravity);
  finally Enable end;
end;

function T3D.Ray(
  const RayOrigin, RayDirection: TVector3Single): TRayCollision;
begin
  Disable;
  try
    Result := World.WorldRay(RayOrigin, RayDirection);
  finally Enable end;
end;

function T3D.Shared: T3D;
begin
  Result := Self;
end;

{ T3DListCore ------------------------------------------------------------ }

constructor T3DListCore.Create(const FreeObjects: boolean; const AOwner: T3DList);
begin
  inherited Create(FreeObjects);
  FOwner := AOwner;
end;

procedure T3DListCore.Notify(Ptr: Pointer; Action: TListNotification);
var
  B: T3D;
begin
  inherited;

  if Owner <> nil then
  begin
    B := T3D(Ptr);

    case Action of
      lnAdded:
        begin
          B.SetWorld(Owner.World);
          { Register Owner to be notified of item destruction. }
          B.FreeNotification(Owner);
        end;
      lnExtracted, lnDeleted:
        begin
          { We don't change B.World here (we don't set it to
            "previous World", or nil, or anything).
            The 3D object may be present in the same World multiple times,
            so it's better to leave World unchanged here.

            This way T3D.World points to "last world you were part of",
            instead of "current world you are part in", but that's not
            a problem in practice -- as the World is only used to call
            World.OnXxx notifications, and it's harmless to call them
            when the object is not actually part of world. }

          { Do not call RemoveFreeNotification when Owner is equal to B.World,
            this would prevent getting notification when to nil FWorld in
            T3D.Notification. }
          if B.World <> Owner then
            B.RemoveFreeNotification(Owner);
        end;
      else raise EInternalError.Create('T3DListCore.Notify action?');
    end;

    if (Owner.World <> nil) and Assigned(Owner.World.OnCursorChange) then
      Owner.World.OnCursorChange(Owner);
  end;
end;

function T3DListCore.GetItem(const I: Integer): T3D;
begin
  Result := T3D(inherited Items[I]);
end;

procedure T3DListCore.SetItem(const I: Integer; const Item: T3D);
begin
  inherited Items[I] := Item;
end;

function T3DListCore.First: T3D;
begin
  Result := (inherited First) as T3D;
end;

function T3DListCore.Last: T3D;
begin
  Result := (inherited Last) as T3D;
end;

{ T3DList ---------------------------------------------------------------- }

constructor T3DList.Create(AOwner: TComponent);
begin
  inherited;
  FList := T3DListCore.Create(false, Self);
end;

destructor T3DList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure T3DList.SetWorld(const Value: T3DWorld);
var
  I: Integer;
begin
  if FWorld <> Value then
  begin
    inherited;
    if GetChild <> nil then
      GetChild.SetWorld(Value);
    if FList <> nil then // when one list is within another, this may be called during own destruction by T3DListCore.Notify
      for I := 0 to List.Count - 1 do
        List[I].SetWorld(Value);
  end;
end;

function T3DList.GetChild: T3D;
begin
  Result := nil;
end;

procedure T3DList.Add(const Item: T3D);
begin
  List.Add(Item);
end;

procedure T3DList.Insert(const Index: Integer; const Item: T3D);
begin
  List.Insert(Index, Item);
end;

function T3DList.GetItem(const I: Integer): T3D;
begin
  Result := List[I];
end;

procedure T3DList.SetItem(const I: Integer; const Item: T3D);
begin
  List[I] := Item;
end;

function T3DList.Count: Integer;
begin
  Result := List.Count;
end;

procedure T3DList.Remove(const Item: T3D);
begin
  List.Remove(Item);
end;

procedure T3DList.Clear;
begin
  List.Clear;
end;

procedure T3DList.Exchange(const Index1, Index2: Integer);
begin
  List.Exchange(Index1, Index2);
end;

function CompareBackToFront2D(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(T3D(A).BoundingBox, T3D(B).BoundingBox);
end;

var
  { Has to be global, since TFPGObjectList.Sort
    requires normal function (not "of object"). }
  SortCameraPosition: TVector3Single;

function CompareBackToFront3D(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(T3D(A).BoundingBox, T3D(B).BoundingBox,
    SortCameraPosition);
end;

procedure T3DList.SortBackToFront(const BlendingSort: TBlendingSort;
  const CameraPosition: TVector3Single);
begin
  case BlendingSort of
    bs2D: List.Sort(@CompareBackToFront2D);
    bs3D:
      begin
        SortCameraPosition := CameraPosition;
        List.Sort(@CompareBackToFront3D);
      end;
  end;
end;

procedure T3DList.SortBackToFront2D;
begin
  SortBackToFront(bs2D, ZeroVector3Single);
end;

function T3DList.BoundingBox: TBox3D;
var
  I: Integer;
begin
  Result := EmptyBox3D;
  if GetExists then
  begin
    if GetChild <> nil then
      Result.Add(GetChild.BoundingBox);
    for I := 0 to List.Count - 1 do
      Result.Add(List[I].BoundingBox);
  end;
end;

procedure T3DList.Render(const Frustum: TFrustum; const Params: TRenderParams);
var
  I: Integer;
begin
  inherited;
  if GetExists then
  begin
    if GetChild <> nil then
      GetChild.Render(Frustum, Params);
    for I := 0 to List.Count - 1 do
      List[I].Render(Frustum, Params);
  end;
end;

procedure T3DList.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  I: Integer;
begin
  inherited;
  if GetExists and CastShadowVolumes then
  begin
    if GetChild <> nil then
      GetChild.RenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform);
    for I := 0 to List.Count - 1 do
      List[I].RenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform);
  end;
end;

procedure T3DList.PrepareResources(Options: TPrepareResourcesOptions;
  ProgressStep: boolean; BaseLights: TAbstractLightInstancesList);
var
  I: Integer;
begin
  inherited;
  if GetChild <> nil then
    GetChild.PrepareResources(Options, ProgressStep, BaseLights);
  for I := 0 to List.Count - 1 do
    List[I].PrepareResources(Options, ProgressStep, BaseLights);
end;

function T3DList.PrepareResourcesSteps: Cardinal;
var
  I: Integer;
begin
  Result := inherited;
  if GetChild <> nil then
    Result += GetChild.PrepareResourcesSteps;
  for I := 0 to List.Count - 1 do
    Result += List[I].PrepareResourcesSteps;
end;

function T3DList.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if GetChild <> nil then
    if GetChild.Press(Event) then Exit(true);
  for I := 0 to List.Count - 1 do
    if List[I].Press(Event) then Exit(true);
end;

function T3DList.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if GetChild <> nil then
    if GetChild.Release(Event) then Exit(true);
  for I := 0 to List.Count - 1 do
    if List[I].Release(Event) then Exit(true);
end;

procedure T3DList.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I: Integer;
  Item: T3D;
  RemoveItem: TRemoveType;
begin
  inherited;
  if GetExists then
  begin
    if GetChild <> nil then
    begin
      RemoveItem := rtNone;
      GetChild.Update(SecondsPassed, RemoveItem);
      { resulting RemoveItem is ignored, GetChild cannot be removed }
    end;

    I := 0;
    while I < List.Count do
    begin
      Item := List[I];
      RemoveItem := rtNone;
      Item.Update(SecondsPassed, RemoveItem);
      if RemoveItem in [rtRemove, rtRemoveAndFree] then
      begin
        List.Delete(I);
        if RemoveItem = rtRemoveAndFree then
          FreeAndNil(Item);
      end else
        Inc(I);
    end;
  end;
end;

procedure T3DList.GLContextClose;
var
  I: Integer;
begin
  if GetChild <> nil then
    GetChild.GLContextClose;
  { this is called from inherited destructor, so check <> nil carefully }
  if FList <> nil then
  begin
    for I := 0 to List.Count - 1 do
      List[I].GLContextClose;
  end;

  inherited;
end;

procedure T3DList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { We have to remove a reference to the object from the List.
    This is crucial: T3DListCore.Notify,
    and e.g. GLContextClose call, assume that all objects on
    the List are always valid objects (no invalid references,
    even for a short time). }

  { About List <> nil check:

    How situation with List = nil may happen? When our List is destroyed,
    it calls B.FreeNotification on all it's items, so it (falsely) seems we will
    not get any more notifications.

    It turns out that we may get notifications,
    and they are notifications about our own destruction (AComponent = Self).
    That is because TComponent.Notification passes down the notification to
    all it's FComponents, that is rtl/objpas/classes/compon.inc
    (in FPC sources) contains code

    Procedure TComponent.Notification(AComponent: TComponent; Operation: TOperation);
    begin
      ...
      For Runner:=0 To FComponents.Count-1 do
        TComponent(FComponents.Items[Runner]).Notification(AComponent,Operation);
    end;

    And FComponents contain all components that are owned.
    So we are informed when something is removed from the owner,
    including about our own removal. (And in this case, we are a T3D descendant
    ourselves, just like our children; so check "AComponent is T3D" doesn't
    protect us.)
    Practical situation when it happens is in testcases
    TTestCastle3D.TestNotifications and TTestCastle3D.TestNotificationsSceneManager. }

  if (Operation = opRemove) and (AComponent is T3D) and (List <> nil) then
    List.RemoveAll(AComponent);
end;

function T3DList.HeightCollision(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
var
  I: Integer;
  NewResult: boolean;
  NewAboveHeight: Single;
  NewAboveGround: P3DTriangle;
begin
  Result := false;
  AboveHeight := MaxSingle;
  AboveGround := nil;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      NewResult := GetChild.HeightCollision(Position, GravityUp, TrianglesToIgnoreFunc,
        NewAboveHeight, NewAboveGround);

      if NewAboveHeight < AboveHeight then
      begin
        Result := NewResult;
        AboveHeight := NewAboveHeight;
        AboveGround := NewAboveGround;
      end;
    end;

    for I := 0 to List.Count - 1 do
    begin
      NewResult := List[I].HeightCollision(Position, GravityUp, TrianglesToIgnoreFunc,
        NewAboveHeight, NewAboveGround);

      if NewAboveHeight < AboveHeight then
      begin
        Result := NewResult;
        AboveHeight := NewAboveHeight;
        AboveGround := NewAboveGround;
      end;
    end;
  end;
end;

function T3DList.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  if GetCollides then
  begin
    { We call MoveCollision with separate ProposedNewPos and NewPos
      only on the first scene (or GetChild, if exists).
      This means that only first scene collisions provide wall sliding.
      Collisions with other 3D objects will simply block the player.

      Otherwise, various MoveCollision could modify NewPos
      making it colliding with other items, already checked. This would
      be wrong.

      TODO: this could be improved, to call MoveCollision
      with separate ProposedNewPos and NewPos
      on the first scene
      where the simple move is not allowed. This would make it more general,
      although also slower. Is there any way to make it as fast and
      more general? }

    if GetChild <> nil then
    begin
      Result := GetChild.MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;

      for I := 0 to List.Count - 1 do
      begin
        Result := List[I].MoveCollision(OldPos, NewPos,
          IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
        if not Result then Exit;
      end;
    end else
    if List.Count <> 0 then
    begin
      Result := List[0].MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;

      for I := 1 to List.Count - 1 do
      begin
        Result := List[I].MoveCollision(OldPos, NewPos,
          IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
        if not Result then Exit;
      end;
    end;
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

function T3DList.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;
  end;
end;

function T3DList.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
var
  I: Integer;
begin
  Result := false;

  if GetCollides or (ALineOfSight and GetExists) then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
      if Result then Exit;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
      if Result then Exit;
    end;
  end;
end;

function T3DList.SphereCollision(const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function T3DList.SphereCollision2D(const Pos: TVector2Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
var
  I: Integer;
begin
  Result := false;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.SphereCollision2D(Pos, Radius, TrianglesToIgnoreFunc, Details);
      if Result then
      begin
        if Details <> nil then
          Details.Add(Self);
        Exit;
      end;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SphereCollision2D(Pos, Radius, TrianglesToIgnoreFunc, Details);
      if Result then
      begin
        if Details <> nil then
          Details.Add(Self);
        Exit;
      end;
    end;
  end;
end;

function T3DList.PointCollision2D(const Point: TVector2Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.PointCollision2D(Point, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].PointCollision2D(Point, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function T3DList.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if GetCollides then
  begin
    if GetChild <> nil then
    begin
      Result := GetChild.BoxCollision(Box, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;

    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].BoxCollision(Box, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function T3DList.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;

  procedure AddNewResult(NewResult: TRayCollision);
  begin
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

var
  I: Integer;

  NewNode, PreviousNode: PRayCollisionNode;
begin
  Result := nil;

  if GetPickable then
  begin
    if GetChild <> nil then
      AddNewResult(GetChild.RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc));
    for I := 0 to List.Count - 1 do
      AddNewResult(List[I].RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc));

    if Result <> nil then
    begin
      NewNode := Result.Add;
      PreviousNode := @(Result.List^[Result.Count - 2]);
      NewNode^.Item := Self;
      NewNode^.Point := PreviousNode^.Point;
      NewNode^.Triangle := nil;
      NewNode^.RayOrigin := RayOrigin;
      NewNode^.RayDirection := RayDirection;
    end;
  end;
end;

procedure T3DList.UpdateGeneratedTextures(
  const RenderFunc: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const OriginalViewport: TRectangle);
var
  I: Integer;
begin
  inherited;
  if GetChild <> nil then
    GetChild.UpdateGeneratedTextures(RenderFunc, ProjectionNear, ProjectionFar,
      OriginalViewport);
  for I := 0 to List.Count - 1 do
    List[I].UpdateGeneratedTextures(RenderFunc, ProjectionNear, ProjectionFar,
      OriginalViewport);
end;

procedure T3DList.VisibleChangeNotification(const Changes: TVisibleChanges);
var
  I: Integer;
begin
  inherited;
  if GetChild <> nil then
    GetChild.VisibleChangeNotification(Changes);
  for I := 0 to List.Count - 1 do
    List[I].VisibleChangeNotification(Changes);
end;

procedure T3DList.CameraChanged(ACamera: TCamera);
var
  I: Integer;
begin
  inherited;
  if GetChild <> nil then
    GetChild.CameraChanged(ACamera);
  for I := 0 to List.Count - 1 do
    List[I].CameraChanged(ACamera);
end;

function T3DList.Dragging: boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if GetChild <> nil then
  begin
    Result := GetChild.Dragging;
    if Result then Exit;
  end;

  for I := 0 to List.Count - 1 do
  begin
    Result := List[I].Dragging;
    if Result then Exit;
  end;
end;

{ TransformMatricesMult ------------------------------------------------------ }

procedure TransformMatricesMult(var Transform, TransformInverse: TMatrix4Single;
  const Center: TVector3Single;
  const Rotation: TVector4Single;
  const Scale: TVector3Single;
  const ScaleOrientation: TVector4Single;
  const Translation: TVector3Single);
var
  M, IM: TMatrix4Single;
  MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  { To make TransformInverse, we multiply inverted matrices in inverted order
    below. }

  MultMatricesTranslation(Transform, TransformInverse,
    VectorAdd(Translation, Center));

  { We avoid using RotationMatricesRad when angle = 0, since this
    is often the case, and it makes TransformState much faster
    (which is important --- TransformState is important for traversing state). }
  if Rotation[3] <> 0 then
  begin
    { Note that even rotation Axis = zero is OK, both M and IM will be
      identity in this case. }
    RotationMatricesRad(Rotation, M, IM);
    Transform := MatrixMult(Transform, M);
    TransformInverse := MatrixMult(IM, TransformInverse);
  end;

  if (Scale[0] <> 1) or
     (Scale[1] <> 1) or
     (Scale[2] <> 1) then
  begin
    if ScaleOrientation[3] <> 0 then
    begin
      RotationMatricesRad(ScaleOrientation, MRotateScaleOrient, IMRotateScaleOrient);
      Transform := MatrixMult(Transform, MRotateScaleOrient);
      TransformInverse := MatrixMult(IMRotateScaleOrient, TransformInverse);
    end;

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity (the 2nd param to ScalingMatrices
      is "true"). That's because X3D allows
      scaling factor to have 0 components (we need TransformInverse only
      for special tricks). }

    ScalingMatrices(Scale, true, M, IM);
    Transform := MatrixMult(Transform, M);
    TransformInverse := MatrixMult(IM, TransformInverse);

    if ScaleOrientation[3] <> 0 then
    begin
      { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
        matrices below. Since we want to reverse them now, so normal
        Transform is multiplied by IM and TransformInverse is multiplied by M. }
      Transform := MatrixMult(Transform, IMRotateScaleOrient);
      TransformInverse := MatrixMult(MRotateScaleOrient, TransformInverse);
    end;
  end;

  MultMatricesTranslation(Transform, TransformInverse, VectorNegate(Center));
end;

{ T3DWorld ------------------------------------------------------------------- }

constructor T3DWorld.Create(AOwner: TComponent);
begin
  inherited;
  { everything inside is part of this world }
  SetWorld(Self);
end;

function T3DWorld.GravityCoordinate: Integer;
begin
  Result := MaxAbsVectorCoord(GravityUp);
end;

function T3DWorld.WorldSphereCollision(const Pos: TVector3Single;
  const Radius: Single): boolean;
begin
  Result := SphereCollision(Pos, Radius, nil);
end;

function T3DWorld.WorldSphereCollision2D(const Pos: TVector2Single;
  const Radius: Single;
  const Details: TCollisionDetails): boolean;
begin
  Result := SphereCollision2D(Pos, Radius, nil, Details);
end;

function T3DWorld.WorldPointCollision2D(const Point: TVector2Single): boolean;
begin
  Result := PointCollision2D(Point, nil);
end;

{ T3DCustomTransform -------------------------------------------------------- }

constructor T3DCustomTransform.Create(AOwner: TComponent);
begin
  inherited;
  FMiddleHeight := DefaultMiddleHeight;
end;

function T3DCustomTransform.GetTranslation: TVector3Single;
begin
  Result := ZeroVector3Single;
end;

function T3DCustomTransform.GetTranslation2D: TVector2Single;
var
  T: TVector3Single;
begin
  T := GetTranslation;
  Result[0] := T[0];
  Result[1] := T[1];
end;

function T3DCustomTransform.GetCenter: TVector3Single;
begin
  Result := ZeroVector3Single;
end;

function T3DCustomTransform.GetRotation: TVector4Single;
begin
  Result := ZeroVector4Single;
end;

const
  NoScale: TVector3Single = (1, 1, 1);

function T3DCustomTransform.GetScale: TVector3Single;
begin
  Result := NoScale;
end;

function T3DCustomTransform.GetScaleOrientation: TVector4Single;
begin
  Result := ZeroVector4Single;
end;

function T3DCustomTransform.OnlyTranslation: boolean;
begin
  Result := false; { safer but slower default }
end;

function T3DCustomTransform.Transform: TMatrix4Single;
var
  Dummy: TMatrix4Single;
begin
  TransformMatrices(Result, Dummy); // TODO: optimize, if needed?
end;

function T3DCustomTransform.TransformInverse: TMatrix4Single;
var
  Dummy: TMatrix4Single;
begin
  TransformMatrices(Dummy, Result); // TODO: optimize, if needed?
end;

procedure T3DCustomTransform.TransformMatricesMult(
  var M, MInverse: TMatrix4Single);
begin
  Castle3D.TransformMatricesMult(M, MInverse,
    GetCenter, GetRotation, GetScale, GetScaleOrientation, GetTranslation);
end;

procedure T3DCustomTransform.TransformMatrices(
  out M, MInverse: TMatrix4Single);
begin
  M := IdentityMatrix4Single;
  MInverse := IdentityMatrix4Single;
  TransformMatricesMult(M, MInverse); // TODO: optimize, if needed?
end;

function T3DCustomTransform.AverageScale: Single;
begin
  Result := Approximate3DScale(GetScale);
end;

{ We assume in all methods below that OnlyTranslation is the most common case,
  and then that GetTranslation = 0,0,0 is the most common case.
  This is true for many 3D objects. And for only translation,
  we can calculate result much faster (and for translation = zero,
  we don't have to do anything besides calling inherited).

  For some simplest operations, we do not check for GetTranslation = 0,0,0
  case --- if applying GetTranslation is very fast, then checking for
  zero translation would be a waste of time. }

function T3DCustomTransform.BoundingBox: TBox3D;
begin
  if OnlyTranslation then
    Result := LocalBoundingBox.Translate(GetTranslation) else
    Result := LocalBoundingBox.Transform(Transform);
end;

procedure T3DCustomTransform.Render(const Frustum: TFrustum; const Params: TRenderParams);
var
  T: TVector3Single;
  OldRenderTransform, Inverse: TMatrix4Single;
  OldRenderTransformIdentity: boolean;
begin
  T := GetTranslation;
  if OnlyTranslation and ZeroVector(T) then
    inherited Render(Frustum, Params) else
    begin
      { inherited Render expects Frustum in local coordinates (without
        transformation), so we subtract transformation here. }

      OldRenderTransform         := Params.RenderTransform;
      OldRenderTransformIdentity := Params.RenderTransformIdentity;
      Params.RenderTransformIdentity := false;

      if OnlyTranslation then
      begin
        MultMatrixTranslation(Params.RenderTransform, T);
        inherited Render(Frustum.Move(-T), Params);
      end else
      begin
        Inverse := IdentityMatrix4Single;
        TransformMatricesMult(Params.RenderTransform, Inverse);
        if IsNan(Inverse[0][0]) then
          {$ifndef VER3_1}
          WritelnWarning('Transform', Format(
            'Inverse transform matrix has NaN value inside:' + NL +
            '%s' + NL +
            '  Matrix source: Center %s, Rotation %s, Scale %s, ScaleOrientation %s, Translation %s',
            [MatrixToNiceStr(Inverse, '  '),
             VectorToNiceStr(GetCenter),
             VectorToNiceStr(GetRotation),
             VectorToNiceStr(GetScale),
             VectorToNiceStr(GetScaleOrientation),
             VectorToNiceStr(GetTranslation)
            ]));
          {$else}
          { Workaround FPC 3.1.1 Internal error 200211262 when compiling above }
          WritelnWarning('Transform', 'Inverse transform matrix has NaN value inside');
          {$endif}
        inherited Render(Frustum.Transform(Inverse), Params);
      end;

      Params.RenderTransform         := OldRenderTransform;
      Params.RenderTransformIdentity := OldRenderTransformIdentity;
    end;
end;

procedure T3DCustomTransform.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  T: TVector3Single;
begin
  if OnlyTranslation then
  begin
    T := GetTranslation;
    if ZeroVector(T) then
      inherited RenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform) else
      inherited RenderShadowVolume(ShadowVolumeRenderer,
        false, MatrixMult(TranslationMatrix(T), ParentTransform));
  end else
    inherited RenderShadowVolume(ShadowVolumeRenderer,
      false, MatrixMult(Transform, ParentTransform));
end;

function T3DCustomTransform.HeightCollision(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
var
  MInverse: TMatrix4Single;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then
  begin
    Result := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;
    Exit;
  end;

  if OnlyTranslation then
    Result := inherited HeightCollision(
      Position - GetTranslation, GravityUp, TrianglesToIgnoreFunc,
      AboveHeight, AboveGround) else
  begin
    MInverse := TransformInverse;
    Result := inherited HeightCollision(
      MatrixMultPoint(MInverse, Position),
      MatrixMultDirection(MInverse, GravityUp), TrianglesToIgnoreFunc,
        AboveHeight, AboveGround);
    { Note that we should not scale resulting AboveHeight by AverageScale.
      That is because AboveHeight is relative to GravityUp length,
      so it's automatically correct. }
  end;
end;

function T3DCustomTransform.MoveCollision(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
  M, MInverse: TMatrix4Single;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then
  begin
    NewPos := ProposedNewPos;
    Exit(true);
  end;

  if OnlyTranslation then
  begin
    T := GetTranslation;
    Result := inherited MoveCollision(
      OldPos         - T,
      ProposedNewPos - T, NewPos,
      IsRadius, Radius,
      OldBox.AntiTranslate(T),
      NewBox.AntiTranslate(T), TrianglesToIgnoreFunc);
    { translate calculated NewPos back }
    if Result then
      NewPos += T;
  end else
  begin
    TransformMatrices(M, MInverse);
    Result := inherited MoveCollision(
      MatrixMultPoint(MInverse, OldPos),
      MatrixMultPoint(MInverse, ProposedNewPos), NewPos,
      IsRadius, Radius / AverageScale,
      OldBox.Transform(MInverse),
      NewBox.Transform(MInverse), TrianglesToIgnoreFunc);
    { transform calculated NewPos back }
    if Result then
      NewPos := MatrixMultPoint(M, NewPos);
  end;
end;

function T3DCustomTransform.MoveCollision(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
  MInverse: TMatrix4Single;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then Exit(true);

  if OnlyTranslation then
  begin
    { I have to check collision between
        Items + Translation and (OldPos, NewPos).
      So it's equivalent to checking for collision between
        Items and (OldPos, NewPos) - Translation
      And this way I can use inherited MoveCollision. }
    T := GetTranslation;
    Result := inherited MoveCollision(
      OldPos - T,
      NewPos - T,
      IsRadius, Radius,
      OldBox.AntiTranslate(T),
      NewBox.AntiTranslate(T), TrianglesToIgnoreFunc);
  end else
  begin
    MInverse := TransformInverse;
    Result := inherited MoveCollision(
      MatrixMultPoint(MInverse, OldPos),
      MatrixMultPoint(MInverse, NewPos),
      IsRadius, Radius / AverageScale,
      OldBox.Transform(MInverse),
      NewBox.Transform(MInverse), TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTransform.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
var
  T: TVector3Single;
  MInverse: TMatrix4Single;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not (GetCollides or (ALineOfSight and GetExists)) then Exit(false);

  if OnlyTranslation then
  begin
    T := GetTranslation;
    Result := inherited SegmentCollision(Pos1 - T, Pos2 - T, TrianglesToIgnoreFunc, ALineOfSight);
  end else
  begin
    MInverse := TransformInverse;
    Result := inherited SegmentCollision(
      MatrixMultPoint(MInverse, Pos1),
      MatrixMultPoint(MInverse, Pos2), TrianglesToIgnoreFunc, ALineOfSight);
  end;
end;

function T3DCustomTransform.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then Exit(false);

  if OnlyTranslation then
    Result := inherited SphereCollision(
      Pos - GetTranslation, Radius, TrianglesToIgnoreFunc) else
    Result := inherited SphereCollision(
      MatrixMultPoint(TransformInverse, Pos), Radius / AverageScale, TrianglesToIgnoreFunc);
end;

function T3DCustomTransform.SphereCollision2D(
  const Pos: TVector2Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then Exit(false);

  if OnlyTranslation then
    Result := inherited SphereCollision2D(
      Pos - GetTranslation2D, Radius, TrianglesToIgnoreFunc, Details) else
    Result := inherited SphereCollision2D(
      MatrixMultPoint(TransformInverse, Pos), Radius / AverageScale, TrianglesToIgnoreFunc, Details);
end;

function T3DCustomTransform.PointCollision2D(
  const Point: TVector2Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then Exit(false);

  if OnlyTranslation then
    Result := inherited PointCollision2D(
      Point - GetTranslation2D, TrianglesToIgnoreFunc) else
    Result := inherited PointCollision2D(
      MatrixMultPoint(TransformInverse, Point), TrianglesToIgnoreFunc);
end;

function T3DCustomTransform.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetCollides then Exit(false);

  if OnlyTranslation then
    Result := inherited BoxCollision(
      Box.AntiTranslate(GetTranslation), TrianglesToIgnoreFunc) else
    Result := inherited BoxCollision(
      Box.Transform(TransformInverse), TrianglesToIgnoreFunc);
end;

function T3DCustomTransform.OutsideToLocal(const Pos: TVector3Single): TVector3Single;
begin
  if OnlyTranslation then
    Result := Pos - GetTranslation else
    Result := MatrixMultPoint(TransformInverse, Pos);
end;

function T3DCustomTransform.LocalToOutside(const Pos: TVector3Single): TVector3Single;
begin
  if OnlyTranslation then
    Result := Pos + GetTranslation else
    Result := MatrixMultPoint(Transform, Pos);
end;

function T3DCustomTransform.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  T: TVector3Single;
  M, MInverse: TMatrix4Single;
  LastNode: PRayCollisionNode;
begin
  { inherited will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not GetPickable then Exit(nil);

  if OnlyTranslation then
  begin
    T := GetTranslation;
    Result := inherited RayCollision(RayOrigin - T, RayDirection, TrianglesToIgnoreFunc);
    if Result <> nil then
    begin
      LastNode := @(Result.List^[Result.Count - 1]);
      LastNode^.Point += T;
      { untransform the ray }
      LastNode^.RayOrigin := RayOrigin;
      LastNode^.RayDirection := RayDirection;
    end;
  end else
  begin
    TransformMatrices(M, MInverse);
    Result := inherited RayCollision(
      MatrixMultPoint(MInverse, RayOrigin),
      MatrixMultDirection(MInverse, RayDirection), TrianglesToIgnoreFunc);
    if Result <> nil then
    begin
      LastNode := @(Result.List^[Result.Count - 1]);
      LastNode^.Point := MatrixMultPoint(M, LastNode^.Point);
      { untransform the ray }
      LastNode^.RayOrigin := RayOrigin;
      LastNode^.RayDirection := RayDirection;

      { Note that we should not scale Result.Distance by AverageScale.
        That is because Result.Distance is relative to RayDirection length,
        so it's automatically correct. }
    end;
  end;
end;

function Bottom(const Gravity: boolean; const GravityCoordinate: Integer;
  const BoundingBox: TBox3D): Single;
begin
  if Gravity then
    Result := 0 else
    Result := BoundingBox.Data[0, GravityCoordinate];
end;

function T3DCustomTransform.Middle: TVector3Single;
var
  GC: Integer;
  B: TBox3D;
begin
  GC := World.GravityCoordinate;
  if MiddleForceBox then
    B := MiddleForceBoxValue else
    B := LocalBoundingBox;

  { More correct version would be to take B bottom point, add PreferredHeight,
    and transform this point just like T3DCustomTransform transforms everything
    else. Optimized implementation below assumes that instead
    of transforming we can add GetTranslation, so we assume that
    transformations do not change this middle point
    (which is Ok if you think e.g. about a non-flying creature that,
    besides moving, only rotates around it's own up axis). }

  Result := GetTranslation;
  Result[GC] += Bottom(Gravity, GC, B) + PreferredHeight;
end;

function T3DCustomTransform.PreferredHeight: Single;
var
  GC: Integer;
  B: TBox3D;
  { $define CHECK_HEIGHT_VS_RADIUS}
  {$ifdef CHECK_HEIGHT_VS_RADIUS} R: Single; {$endif}
begin
  GC := World.GravityCoordinate;
  if MiddleForceBox then
    B := MiddleForceBoxValue else
    B := LocalBoundingBox;
  Result := MiddleHeight * (B.Data[1, GC] - Bottom(Gravity, GC, B));

  {$ifdef CHECK_HEIGHT_VS_RADIUS}
  if Sphere(R) and (R > Result) then
  begin
    WritelnWarning('3D Radius',
      Format('PreferredHeight %f is smaller than radius %f. Gravity may work weird for this 3D object.',
      [Result, R]));
  end;
  {$endif}
end;

procedure T3DCustomTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure DoGravity(const PreferredHeight: Single);
  var
    GravityUp: TVector3Single;

    { TODO: this is a duplicate of similar TWalkCamera method }
    procedure DoFall;
    var
      BeginPos, EndPos, FallVector: TVector3Single;
    begin
      { Project Middle and FFallingStartMiddle
        onto GravityUp vector to calculate fall height. }
      BeginPos := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, FFallingStartMiddle);
      EndPos   := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, Middle);
      FallVector := BeginPos - EndPos;

      { Because of various growing and jumping effects (imagine you jump up
        onto a taller pillar) it may turn out that we're higher at the end
        at the end of fall. Do not report it to Fall event in this case. }
      if VectorDotProduct(GravityUp, Normalized(FallVector)) <= 0 then
        Exit;

      Fall(VectorLen(FallVector));
    end;

  const
    { HeightMargin is used to safeguard against floating point inaccuracy.
      Without this, creature would too often be considered "falling down"
      or "growing up". }
    HeightMargin = 1.01;
  var
    IsAbove: boolean;
    AboveHeight, RadiusIgnored: Single;
    OldFalling: boolean;
    FallingDistance, MaximumFallingDistance: Single;
  begin
    { calculate and save GravityUp once, it's used quite often in this procedure }
    GravityUp := World.GravityUp;

    OldFalling := FFalling;

    IsAbove := Height(Middle, AboveHeight);

    if (FallSpeed <> 0) and
       (AboveHeight > PreferredHeight * HeightMargin) then
    begin
      { Fall down }
      if not FFalling then
        FFallingStartMiddle := Middle;

      FFalling := true;

      FallingDistance := FallSpeed * SecondsPassed;
      if IsAbove then
      begin
        MaximumFallingDistance := AboveHeight - PreferredHeight;

        { If you will fall down by exactly
          AboveHeight - PreferredHeight,
          then you will get exatly into collision with the ground.
          So actually this is too large MaximumFallingDistance.

          But actually it's OK when Sphere is used, because then wall-sliding
          in MoveCollision can correct new position,
          so actually it will be slightly above the ground. So falling
          down will work.

          But when Sphere is not used, the situation is worse,
          because then MoveCollision doesn't do wall-sliding.
          And it will always simply reject such move
          with MaximumFallingDistance.
          If FPS is low (so we would like to fall down at once
          by large distance), this is noticeable: in such case, instead
          of falling down, creature hangs over the ground,
          because MoveCollision simply doesn't allow it fall
          exactly by AboveHeight - PreferredHeight.
          So MaximumFallingDistance has to be a little smaller in this case.
          In particular, this was noticeable for the initially dead alien
          creature on "Doom" level, when shadows were on (when shadows were on,
          FPS is low, that's why the bug was noticeable only with shadows = on).

          TODO: the better version would be to improve
          MoveCollision for Sphere=false case, instead of
          workarounding it here with this epsilon.
          See TBaseTrianglesOctree.MoveCollision. }
        if not Sphere(RadiusIgnored) then
          MaximumFallingDistance -= 0.01;
        MinVar(FallingDistance, MaximumFallingDistance);
      end;

      if not Move(GravityUp * -FallingDistance, true) then
        FFalling := false;
    end else
    begin
      FFalling := false;

      if (GrowSpeed <> 0) and
         (AboveHeight < PreferredHeight / HeightMargin) then
      begin
        { Growing up }
        Move(GravityUp * Min(GrowSpeed * SecondsPassed,
          PreferredHeight - AboveHeight), false);
      end;
    end;

    if OldFalling and (not FFalling) then
      DoFall;
  end;

var
  PH: Single;
begin
  inherited;

  if GetExists and Gravity then
  begin
    PH := PreferredHeight;
    if (PH <> 0) and
       ((FallSpeed <> 0) or (GrowSpeed <> 0)) then
      { calculate and save PreferredHeight once,
        as it's used quite often in the DoGravity procedure }
      DoGravity(PH);
  end;
end;

procedure T3DCustomTransform.Fall(const FallHeight: Single);
begin
  { Nothing to do in this class }
end;

function T3DCustomTransform.LocalBoundingBox: TBox3D;
begin
  Result := inherited BoundingBox;
end;

function T3DCustomTransform.Move(const Translation: TVector3Single;
  const BecauseOfGravity, EnableWallSliding: boolean): boolean;
var
  OldMiddle, ProposedNewMiddle, NewMiddle: TVector3Single;
begin
  OldMiddle := Middle;

  if EnableWallSliding then
  begin
    ProposedNewMiddle := OldMiddle + Translation;
    Result := MoveAllowed(OldMiddle, ProposedNewMiddle, NewMiddle, BecauseOfGravity);
  end else
  begin
    NewMiddle := OldMiddle + Translation;
    Result := MoveAllowed(OldMiddle, NewMiddle, BecauseOfGravity);
  end;

  if Result then
    Translate(NewMiddle - OldMiddle);
end;

{ T3DTransform -------------------------------------------------------------- }

constructor T3DTransform.Create(AOwner: TComponent);
begin
  inherited;
  FOnlyTranslation := true;
  FScale := NoScale;
end;

function T3DTransform.GetCenter: TVector3Single;
begin
  Result := FCenter;
end;

function T3DTransform.GetRotation: TVector4Single;
begin
  Result := FRotation;
end;

function T3DTransform.GetScale: TVector3Single;
begin
  Result := FScale;
end;

function T3DTransform.GetScaleOrientation: TVector4Single;
begin
  Result := FScaleOrientation;
end;

function T3DTransform.GetTranslation: TVector3Single;
begin
  Result := FTranslation;
end;

{ We try hard to keep OnlyTranslation return fast, and return with true.
  This will allow T3DCustomTransform to be optimized and accurate
  for often case of pure translation. }

procedure T3DTransform.SetCenter(const Value: TVector3Single);
begin
  FCenter := Value;
  FOnlyTranslation := FOnlyTranslation and
    (Value[0] = 0) and (Value[1] = 0) and (Value[2] = 0);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DTransform.SetRotation(const Value: TVector4Single);
begin
  FRotation := Value;
  FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DTransform.SetScale(const Value: TVector3Single);
begin
  FScale := Value;
  FOnlyTranslation := FOnlyTranslation and
    (Value[0] = 1) and (Value[1] = 1) and (Value[2] = 1);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DTransform.SetScaleOrientation(const Value: TVector4Single);
begin
  FScaleOrientation := Value;
  FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DTransform.SetTranslation(const Value: TVector3Single);
begin
  FTranslation := Value;
  VisibleChangeHere([vcVisibleGeometry]);
end;

function T3DTransform.OnlyTranslation: boolean;
begin
  Result := FOnlyTranslation;
end;

procedure T3DTransform.Translate(const T: TVector3Single);
begin
  Translation := Translation + T;
end;

procedure T3DTransform.Identity;
begin
  Center := ZeroVector3Single;
  Rotation := ZeroVector4Single;
  Scale := NoScale;
  ScaleOrientation := ZeroVector4Single;
  Translation := ZeroVector3Single;
end;

{ T3DOrient ------------------------------------------------------------------ }

constructor T3DOrient.Create(AOwner: TComponent);
begin
  inherited;
  FCamera := TWalkCamera.Create(nil);
  FOrientation := DefaultOrientation;
end;

destructor T3DOrient.Destroy;
begin
  FreeAndNil(FCamera);
  inherited;
end;

procedure T3DOrient.TransformMatricesMult(var M, MInverse: TMatrix4Single);
var
  NewM, NewMInverse: TMatrix4Single;
var
  P, D, U, Side: TVector3Single;
begin
  { Note that actually I could do here TransformToCoordsNoScaleMatrix,
    as obviously I don't want any scaling. But in this case I know
    that Direction and Up lengths = 1 (so their product
    length is also = 1), so no need to do
    TransformToCoordsNoScaleMatrix here (and I can avoid wasting my time
    on Sqrts needed inside TransformToCoordsNoScaleMatrix). }

  Camera.GetView(P, D, U);

  case Orientation of
    otUpYDirectionMinusZ:
      begin
        Side := VectorProduct(U, -D);
        NewM := TransformToCoordsMatrix         (P, Side, U, -D);
        NewMInverse := TransformFromCoordsMatrix(P, Side, U, -D);
      end;
    otUpZDirectionMinusY:
      begin
        Side := VectorProduct(-D, U);
        NewM := TransformToCoordsMatrix         (P, Side, -D, U);
        NewMInverse := TransformFromCoordsMatrix(P, Side, -D, U);
      end;
    otUpZDirectionX:
      begin
        Side := VectorProduct(U, D);
        NewM := TransformToCoordsMatrix         (P, D, Side, U);
        NewMInverse := TransformFromCoordsMatrix(P, D, Side, U);
      end;
    else raise EInternalError.Create('T3DOrient.TransformMatricesMult Orientation?');
  end;

  M := M * NewM;
  MInverse := NewMInverse * MInverse;
end;

function T3DOrient.OnlyTranslation: boolean;
begin
  Result := false;
end;

function T3DOrient.GetPosition: TVector3Single;
begin
  Result := Camera.Position;
end;

function T3DOrient.GetDirection: TVector3Single;
begin
  Result := Camera.Direction;
end;

function T3DOrient.GetUp: TVector3Single;
begin
  Result := Camera.Up;
end;

procedure T3DOrient.SetPosition(const Value: TVector3Single);
begin
  Camera.Position := Value;
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.SetDirection(const Value: TVector3Single);
begin
  Camera.Direction := Value;
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.SetUp(const Value: TVector3Single);
begin
  Camera.Up := Value;
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.UpPrefer(const AUp: TVector3Single);
begin
  Camera.UpPrefer(AUp);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.SetView(const APos, ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
begin
  Camera.SetView(APos, ADir, AUp, AdjustUp);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.SetView(const ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
begin
  Camera.SetView(ADir, AUp, AdjustUp);
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure T3DOrient.Translate(const T: TVector3Single);
begin
  Camera.Position := Camera.Position + T;
  VisibleChangeHere([vcVisibleGeometry]);
end;

function T3DOrient.GetTranslation: TVector3Single;
begin
  Result := Camera.Position;
end;

{ T3DMoving --------------------------------------------------------- }

{ TODO: this browses World list, doesn't take into acount CollidesWithMoving items
  that may be inside a sublist. }

constructor T3DMoving.Create(AOwner: TComponent);
begin
  inherited;
  FPushes := true;
  FPushesEverythingInside := true;
  FAnimationTime := 0;
end;

function T3DMoving.GetTranslation: TVector3Single;
begin
  Result := GetTranslationFromTime(AnimationTime);
end;

function T3DMoving.OnlyTranslation: boolean;
begin
  Result := true; { T3DMoving always uses only translation }
end;

{ Note: When pushing the creature/player/item, right now
  we don't check whether the creature/player/item will not be
  pushed into collision with something else.

  For now, design your level to minimize the chance that it will ever happen.
  Although in theory you cannot design your level to guarantee
  that it will never happen (because e.g. a creature may be pushed
  into collision with other creature, and since creatures move
  on their own they can arrange themselves (in theory) in all manners of
  funny configurations...). But in practice it's not so difficult,
  just make sure that there is enough space on the way of move.
}

procedure T3DMoving.BeforeTimeIncrease(
  const NewAnimationTime: TFloatTime);

  function BoundingBoxAssumeTranslation(
    const AssumeTranslation: TVector3Single): TBox3D;
  begin
    if GetCollides then
      Result := (inherited BoundingBox).Translate(AssumeTranslation) else
      Result := EmptyBox3D;
  end;

  function SphereCollisionAssumeTranslation(
    const AssumeTranslation: TVector3Single;
    const Pos: TVector3Single; const Radius: Single;
    const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
  begin
    Result := GetCollides;
    if Result then
    begin
      { We use the same trick as in T3DCustomTransform.MoveCollision to
        use "inherited SphereCollsion" with Translation. }

      Result := inherited SphereCollision(
        Pos - AssumeTranslation, Radius, TrianglesToIgnoreFunc);
    end;
  end;

  function BoxCollisionAssumeTranslation(
    const AssumeTranslation: TVector3Single;
    const Box: TBox3D;
    const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
  begin
    Result := GetCollides;
    if Result then
    begin
      { We use the same trick as in T3DCustomTransform.MoveCollision to
        use "inherited BoxCollision" with Translation. }

      Result := inherited BoxCollision(
        Box.AntiTranslate(AssumeTranslation), TrianglesToIgnoreFunc);
    end;
  end;

var
  CurrentBox, NewBox, Box: TBox3D;
  I: Integer;
  Translation: TVector3Single;
  CurrentTranslation, NewTranslation: TVector3Single;
  SphereRadius: Single;
  Item: T3D;
begin
  if GetCollides and Pushes then
  begin
    CurrentTranslation := GetTranslationFromTime(AnimationTime);
    NewTranslation := GetTranslationFromTime(NewAnimationTime);

    { It often happens that T3DMoving doesn't move at all,
      and then Translation doesn't change at all
      (even when compared precisely, without usual epsilon used to compare
      floats). So the check below may be worth the time, we expect
      it will avoid doing actual work. }

    if not VectorsPerfectlyEqual(CurrentTranslation, NewTranslation) then
    begin
      Translation := NewTranslation - CurrentTranslation;

      { TODO: it may be sensible to add a pushing method when we compare
        other object's bounding box (never a sphere, and be sure to use
        the "tall" box for player, including it's legs) with octree
        (that is, using inherited BoxCollision).
        This can have the advantages of both PushesEverythingInside=true
        (reacts more sticky, more eager to move colliding stuff with
        the same speed as elevator)
        and PushesEverythingInside=false (takes into account triangle mesh,
        not just our bounding volume). }

      if PushesEverythingInside then
      begin
        CurrentBox := BoundingBox;
        NewBox := BoundingBoxAssumeTranslation(NewTranslation);
        for I := 0 to World.Count - 1 do
        begin
          Item := World[I];
          if (Item is T3DCustomTransform) and Item.CollidesWithMoving then
          begin
            { This case doesn't really use Item.Sphere. But it's not really
              terribly important design decision, we may use Item.Sphere
              one day here. It's most comfortable to just use
              here Item.BoundingBox, as we perform collisions with our box. }
            Box := Item.BoundingBox;
            if Box.Collision(NewBox) or
               Box.Collision(CurrentBox) then
              T3DCustomTransform(Item).Translate(Translation);
          end;
        end;
      end else
      begin
        for I := 0 to World.Count - 1 do
        begin
          Item := World[I];
          if (Item is T3DCustomTransform) and Item.CollidesWithMoving then
            if Item.Sphere(SphereRadius) then
            begin
              if SphereCollisionAssumeTranslation(NewTranslation,
                Item.Middle, SphereRadius,
                @World.CollisionIgnoreItem) then
                T3DCustomTransform(Item).Translate(Translation);
            end else
            begin
              if BoxCollisionAssumeTranslation(NewTranslation,
                Item.BoundingBox,
                @World.CollisionIgnoreItem) then
                T3DCustomTransform(Item).Translate(Translation);
            end;
        end;
      end;
    end;
  end;
end;

procedure T3DMoving.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  NewAnimationTime: TFloatTime;
begin
  inherited;

  NewAnimationTime := AnimationTime + SecondsPassed;
  BeforeTimeIncrease(NewAnimationTime);
  FAnimationTime := NewAnimationTime;
end;

procedure T3DMoving.Translate(const T: TVector3Single);
begin
  { ignore Translate calls, our translation is determined by current time
    and other properties. }
end;

{ T3DLinearMoving --------------------------------------------------- }

constructor T3DLinearMoving.Create(AOwner: TComponent);
begin
  inherited;

  FSoundGoEndPosition := stNone;
  FSoundGoBeginPosition := stNone;

  FEndPosition := false;

  { We set FEndPositionStateChangeTime to a past time, to be sure
    that we don't treat the door as "closing right now". }
  FEndPositionStateChangeTime := -1000.0; { TODO: should be implemented better... }

  UsedSound := nil;
end;

destructor T3DLinearMoving.Destroy;
begin
  { Otherwise, if you exit from the game while some sound was played,
    and the sound was e.g. looping (like the elevator on "Tower" level),
    the sound will never get stopped. }
  if UsedSound <> nil then
    UsedSound.Release;

  inherited;
end;

procedure T3DLinearMoving.SoundRelease(Sender: TSound);
begin
  Assert(Sender = UsedSound);
  UsedSound := nil;
end;

function T3DLinearMoving.SoundPosition: TVector3Single;
begin
  Result := BoundingBox.Center;
end;

procedure T3DLinearMoving.PlaySound(SoundType: TSoundType;
  Looping: boolean);
begin
  { The object can play only one sound (going to begin or end position)
    at a time. }
  if UsedSound <> nil then
    UsedSound.Release;
  UsedSound := SoundEngine.Sound3d(SoundType, SoundPosition, Looping);

  if UsedSound <> nil then
    UsedSound.OnRelease := @SoundRelease;
end;

procedure T3DLinearMoving.GoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure T3DLinearMoving.GoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := AnimationTime;
  PlaySound(SoundGoBeginPosition, SoundGoBeginPositionLooping);
end;

procedure T3DLinearMoving.RevertGoEndPosition;
begin
  FEndPosition := true;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoEndPositionLooping);
end;

procedure T3DLinearMoving.RevertGoBeginPosition;
begin
  FEndPosition := false;
  FEndPositionStateChangeTime := { AnimationTime -
    (MoveTime - (AnimationTime - EndPositionStateChangeTime)) }
    { simplified : }
    2 * AnimationTime - MoveTime - EndPositionStateChangeTime;
  PlaySound(SoundGoEndPosition, SoundGoBeginPositionLooping);
end;

procedure T3DLinearMoving.GoOtherPosition;
begin
  if CompletelyEndPosition then
    GoBeginPosition else
  if CompletelyBeginPosition then
    GoEndPosition else
  begin
    if EndPosition then
      RevertGoBeginPosition else
      RevertGoEndPosition;
  end;
end;

function T3DLinearMoving.GetTranslationFromTime(
  const AnAnimationTime: TFloatTime): TVector3Single;
begin
  if not EndPosition then
  begin
    if AnAnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely closed. }
      Result := ZeroVector3Single else
      { During closing. }
      Result := TranslationEnd *
        (1 - (AnAnimationTime - EndPositionStateChangeTime) / MoveTime);
  end else
  begin
    if AnAnimationTime - EndPositionStateChangeTime > MoveTime then
      { Completely open. }
      Result := TranslationEnd else
      { During opening. }
      Result := TranslationEnd *
        ((AnAnimationTime - EndPositionStateChangeTime) / MoveTime);
  end;
end;

function T3DLinearMoving.CompletelyEndPosition: boolean;
begin
  Result := EndPosition and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

function T3DLinearMoving.CompletelyBeginPosition: boolean;
begin
  Result := (not EndPosition) and
    (AnimationTime - EndPositionStateChangeTime > MoveTime);
end;

procedure T3DLinearMoving.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  { Update sound position when object is moving }
  if (UsedSound <> nil) and SoundTracksCurrentPosition then
    UsedSound.Position := SoundPosition;

  { If the SoundGoBegin/EndPosition is longer than the MoveTime
    (or it's looping),
    stop this sound once we're completely in Begin/EndPosition. }
  if (AnimationTime - EndPositionStateChangeTime > MoveTime) and
    (UsedSound <> nil) then
    UsedSound.Release;
end;

{ T3DAlive ------------------------------------------------------------------- }

constructor T3DAlive.Create(AOwner: TComponent);
begin
  inherited;
  KnockBackSpeed := 1.0;
  { at the beginning we are Dead (Life = 0) and DieTime = LifeTime,
    so everything is already in the correct state. }
end;

procedure T3DAlive.SetLife(const Value: Single);
begin
  if (FLife > 0) and (Value <= 0) then
    FDieTime := LifeTime;
  FLife := Value;
end;

function T3DAlive.Dead: boolean;
begin
  Result := Life <= 0;
end;

procedure T3DAlive.Hurt(const LifeLoss: Single;
  const HurtDirection: TVector3Single;
  const AKnockbackDistance: Single; const Attacker: T3DAlive);
begin
  Life := Life - LifeLoss;
  FKnockbackDistance := AKnockbackDistance;
  FLastHurtDirection := HurtDirection;

  { calculate FLastHurtDirectionGround }
  FLastHurtDirectionGround := FLastHurtDirection;
  if Gravity then
    MakeVectorsOrthoOnTheirPlane(FLastHurtDirectionGround, World.GravityUp);
end;

procedure T3DAlive.CancelKnockback;
begin
  FKnockbackDistance := 0;
end;

procedure T3DAlive.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
{ Do the knockback effect, if it's currently active, by pushing
  creature along last attack direction. }
var
  CurrentKnockBackDistance: Single;
begin
  inherited;
  if not GetExists then Exit;

  FLifeTime += SecondsPassed;

  if FKnockbackDistance > 0 then
  begin
    { Calculate CurrentKnockBackDistance, update FKnockbackDistance }
    CurrentKnockBackDistance := KnockBackSpeed * SecondsPassed;
    if FKnockbackDistance < CurrentKnockBackDistance then
    begin
      CurrentKnockBackDistance := FKnockbackDistance;
      FKnockbackDistance := 0;
    end else
      FKnockbackDistance -= CurrentKnockBackDistance;

    Move(FLastHurtDirectionGround * CurrentKnockBackDistance, false);
  end;
end;

end.
