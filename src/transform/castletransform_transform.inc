{%MainUnit castletransform.pas}
{
  Copyright 2017-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type
  { Group and transform (move, rotate, scale) children objects.

    Add and remove children using the @link(Add), @link(Remove) and similar methods.
    A child can be any @link(TCastleTransform) instance,
    in particular it can be a @link(TCastleScene) instance
    (which allows to load and render any 3D or 2D model).

    Control the transformation using these properties:

    @orderedList(
      @item(Move using @link(Translation).)

      @item(Rotate using @link(Rotation).
        The rotation is performed around the @link(Center) point.
        The rotation may be alternatively controlled using the
        @link(Direction) and @link(Up) vectors.)

      @item(Change size using @link(Scale).
        Scale is done around @link(Center) and with orientation given by
        @link(ScaleOrientation).)
    )

    This class is the base object that is managed by the @link(TCastleViewport).
    You insert instances of this class into @link(TCastleViewport.Items),
    which is actually an instance of @link(TCastleTransform) too.

    This class implements also optional gravity and physics.
    See the @link(Gravity) property for a simple unrealistic gravity model.
    See the @link(RigidBody) for a proper rigid-bidy simulation,
    with correct gravity model and collisions with other rigid bodies. }
  TCastleTransform = class(TCastleComponent)
  strict private
    type
      TEnumerator = record
      strict private
        FList: TCastleTransformList;
        FPosition: Integer;
        function GetCurrent: TCastleTransform; inline;
      public
        constructor Create(const AList: TCastleTransformList);
        function MoveNext: Boolean; inline;
        property Current: TCastleTransform read GetCurrent;
      end;

      { Used by @link(TCastleTransform.BehaviorsEnumerate).
        Do not use this type explicitly, it should only be used by for..in
        construction like "for B in MyTranform.BehaviorsEnumerate do ...".
        @exclude }
      TCastleBehaviorEnumerator = record
      strict private
        FParent: TCastleTransform;
        FPosition: Integer;
        function GetCurrent: TCastleBehavior; inline;
      public
        constructor Create(const AParent: TCastleTransform);
        function MoveNext: Boolean; inline;
        property Current: TCastleBehavior read GetCurrent;
        function GetEnumerator: TCastleBehaviorEnumerator;
      end;

    class var
      NextTransformId: Cardinal;
    var
      FCastShadows: Boolean;
      FExists: Boolean;
      FParentExistsInRoot: Boolean;
      FExistsInRoot: Boolean;
      FLastExistsChangeFrameId: TFrameId;
      FCollides: Boolean;
      FPickable: Boolean;
      FVisible: Boolean;
      FCursor: TMouseCursor;
      FCollidesWithMoving: Boolean;
      FExcludeFromStatistics, FInternalExcludeFromParentBoundingVolume: Boolean;
      FWorld: TCastleAbstractRootTransform;
      { How many times this is a child in the FWorld, counting all possible paths in transformation tree. }
      FWorldReferences: Cardinal;
      FList: TCastleTransformList;
      FParent: TCastleTransform;
      FCollisionSphereRadius: Single;
      FListenPressRelease: Boolean;
      RegisteredGLContextCloseListener: Boolean;
      FPendingToRemove: TComponentList;
      FPendingToRemoveAndFree: TComponentList;
      FRenderLayer: TRenderLayer;

      FBlendingSort: TBlendingSort;
      FSortedList: TCastleTransformList;
      FOutsideList: TCastleTransformList;

      // transformation
      FCenter: TVector3;
      FRotation: TVector4;
      FScale: TVector3;
      FScaleOrientation: TVector4;
      FTranslation: TVector3;

      // transformation extras
      FGravity: Boolean;
      FFallingStartMiddle: TVector3;
      FFalling: Boolean;
      FFallSpeed: Single;
      FGrowSpeed: Single;
      FMiddleHeight: Single;
      FOrientation: TOrientationType;
      FOnlyTranslation: Boolean;

      FTransformation: TTransformation;
      FTransformationValid: Boolean;

      FWorldTransformation: TTransformation;
      FWorldTransformationId: Cardinal;
      FWorldTransformationValid: Boolean;

      FLastParentWorldTransformation: TTransformation;
      FLastParentWorldTransformationId: Cardinal;

      FOnAddedBehaviorNotification: TCastleComponentNotification;
      FBeforeRemoveBehaviorNotification: TCastleComponentNotification;

    procedure SerializeChildrenAdd(const C: TComponent);
    procedure SerializeChildrenClear;
    procedure SerializeChildrenEnumerate(const Proc: TGetChildProc);
    procedure SerializeBehaviorsAdd(const C: TComponent);
    procedure SerializeBehaviorsClear;
    procedure SerializeBehaviorsEnumerate(const Proc: TGetChildProc);
    procedure SetCursor(const Value: TMouseCursor);
    procedure SetCenter(const Value: TVector3);
    procedure SetRotation(const Value: TVector4);
    procedure SetScale(const Value: TVector3);
    procedure SetScaleOrientation(const Value: TVector4);
    procedure SetTranslation(const Value: TVector3);
    function GetTranslationXY: TVector2;
    procedure SetTranslationXY(const Value: TVector2);
    procedure SetRigidBody(const Value: TCastleRigidBody);
    function GetRigidBody: TCastleRigidBody;
    function GetDirection: TVector3;
    function GetUp: TVector3;
    procedure SetDirection(const Value: TVector3);
    procedure SetUp(const Value: TVector3);
    function RotationFromDirectionUp(const D, U: TVector3): TVector4;
    function RotationToDirection(const ARotation: TVector4): TVector3;
    function RotationToUp(const ARotation: TVector4): TVector3;
    procedure UpdateSimpleGravity(const SecondsPassed: Single);
    function GetItem(const I: Integer): TCastleTransform;
    procedure SetItem(const I: Integer; const Item: TCastleTransform);
    procedure InternalTransformationMult(var T: TTransformation);
    procedure InternalOutsideTransformationMult(var T: TTransformation);
    procedure InternalTransformation(out T: TTransformation);
    procedure InternalOutsideTransformation(out T: TTransformation);
    { Update our FWorldTransformation, FWorldTransformationId. }
    procedure UpdateWorldTransformation;
    procedure WarningMatrixNan(const NewParamsInverseTransformValue: TMatrix4);
    procedure RemoveBehaviorIndex(const BehaviorIndex: Integer);
    procedure SetListenPressRelease(const Value: Boolean);
    function GetBehaviors(const Index: Integer): TCastleBehavior;
    procedure GLContextCloseEvent(Sender: TObject);
    procedure PendingRemovals;
    procedure SetExists(const Value: Boolean);
    procedure SetParentExistsInRoot(const Value: Boolean);
    { Value of FParent.ParentExistsInRoot, if FParent is non-nil.
      Always true, if FParent = nil.
      Intuitively this means "all our parents exist" -- Parent, Parent.Parent etc.,
      as far as our parent hierarchy goes. }
    property ParentExistsInRoot: Boolean read FParentExistsInRoot write SetParentExistsInRoot default true;
    { Call when ParentExistsInRoot or Exists changed. }
    procedure UpdateExistsInRoot;
    { Never call ChangedTransform directly, always call this. }
    procedure ChangedTransformCore(const UpdatePhysicsTransform: Boolean = true);
  private
    FYSortOffset: Single;
    { "Meaningful" parent is a parent that competes to be a value of Parent.
      This allows to avoid TCastleTransformReference from being considered
      a meaningful parent, so that transforms under TCastleTransformReference
      can still have useful Parent, WorldTransform and more
      (so that they work e.g. in editor nicely, with physics nicely etc.)

      This can change only after construction, before any children are actually added to this. }
    MeaningfulParent: Boolean;

    FBehaviors: TComponentList;

    { Some behaviors need know when castle transform is in world or not or
      when it changes. They can register to those notifications. When list
      is not created that means no behaviors need those notifications. }
    FBehaviorsWorldChangeNotificationList: TCastleBehaviorList;

    { Update FWorld, FWorldReferences.
      This is recursively called on all children as well.
      Value must not be @nil. }
    procedure AddToWorld(const Value: TCastleAbstractRootTransform);

    { Update FWorld, FWorldReferences.
      This is recursively called on all children as well.
      Value must not be @nil. }
    procedure RemoveFromWorld(const Value: TCastleAbstractRootTransform);

    { Add a new parent.
      Updates FParent, FWorld, FWorldReferences.
      This is not recursively called on all children -- only the AddToWorld, a subset of this, is recursive.
      NewParent must not be @nil. }
    procedure AddParent(const NewParent: TCastleTransform);
    { Remove parent, previously added by AddParent.
      Updates FParent, FWorld, FWorldReferences.
      This is not recursively called on all children -- only the RemoveFromWorld, a subset of this, is recursive.
      OldParent must not be @nil. }
    procedure RemoveParent(const OldParent: TCastleTransform);

    { Return non-nil parent, making sure it's valid.
      It's equivalent to @link(Parent), except it makes exception instead of returning @nil. }
    function CheckParent: TCastleTransform;

    procedure NotifyWorldAfterAttach;
    procedure NotifyWorldBeforeDetach;

    // TODO: Do we need so much new API?

    { Ability to add a callback (object of procedure have to be TComponent)
      to be informed when a behavior is added to this CastleTransform. }
    procedure AddNotificationOnBehaviorAdded(const AEvent: TComponentEvent); overload;
    { Ability to add a callback (object of procedure have to be TComponent)
      to be informed when a behavior is added to this CastleTransform with
      object class filtering. }
    procedure AddNotificationOnBehaviorAdded(const AEvent: TComponentEvent;
      const ClassFilter: TComponentClass); overload;
    { Calls all behavior add callbacks with ABehavior as parameter value. }
    procedure NotifyBehaviorAdded(const ABehavior: TCastleBehavior);
    { Remove all occurrences of callback (object of procedure have to be TComponent)
      from "OnBehaviorAdded" list. }
    procedure RemoveNotificationOnBehaviorAdded(const AEvent: TComponentEvent); overload;
    { Remove a callback (object of procedure have to be TComponent)
      from "OnBehaviorAdded" calbacks list with object class filtering. }
    procedure RemoveNotificationOnBehaviorAdded(const AEvent: TComponentEvent;
      const ClassFilter: TComponentClass); overload;

    { Ability to add a callback (object of procedure have to be TComponent)
      to be informed when a behavior is added to this CastleTransform }
    procedure AddNotificationBeforeBehaviorRemove(const AEvent: TComponentEvent); overload;
    { Ability to add a callback (object of procedure have to be TComponent)
      to be informed when a behevior is added to this CastleTransform with
      object class filtering. }
    procedure AddNotificationBeforeBehaviorRemove(const AEvent: TComponentEvent;
      const ClassFilter: TComponentClass); overload;
    { Calls all behavior remove callbacks with ABehavior as parameter value. }
    procedure NotifyBeforeBehaviorRemove(const ABehavior: TCastleBehavior);
    { Remove all occurrences of callback (object of procedure have to be TComponent)
      from "BeforeBehaviorRemove" list. }
    procedure RemoveNotificationBeforeBehaviorRemove(const AEvent: TComponentEvent); overload;
    { Remove a callback (object of procedure have to be TComponent)
      from "BeforeBehaviorRemove" calbacks list with object class filtering. }
    procedure RemoveNotificationBeforeBehaviorRemove(const AEvent: TComponentEvent;
      const ClassFilter: TComponentClass); overload;

    { Call ParentBeforeDetach and WorldBeforeDetach on a behavior that will soon be freed. }
    procedure BeforeBehaviorDestroy(const Behavior: TCastleBehavior);

    { Call UpdateWorldScale on all colliders here and in children. }
    procedure UpdateCollidersWorldScale;
  protected
    { Returns list of TCastleTransform to render. By default, it just returns @link(List). }
    function GetRenderList(const Params: TRenderParams): TCastleTransformList; virtual;

    procedure UpdateOutsideList;

    { Call this when doing anything that allocates GL resources.
      This will make sure GLContextClose will get called. }
    procedure RegisterGLContextClose;

    { Called when the current @link(World) that contains this object changes.
      In the usual case, @link(World) corresponds to a @link(TCastleViewport.Items)
      instance, and when this method is called it means that object
      is added/removed from a viewport.

      You can ignore this when called with Value equal to current @link(World).

      Note that each TCastleTransform instance can only be part of one world
      (TCastleAbstractRootTransform) at a time.
      Although we may be present many times within the same world.
      Always remove the TCastleTransform from previous world
      before adding it to a new one. }
    procedure ChangeWorld(const Value: TCastleAbstractRootTransform); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Height of a point above the 3D model.
      This checks ray collision, from APosition along the negated GravityUp vector.
      Measures distance to the nearest scene item (called "ground" here).

      @returns(If the 3D scene is hit.
        @false means that APosition floats above an empty space.
        That is, if you turn gravity on, it will fall down forever,
        as far as this 3D scene is concerned.)

      @param(AboveHeight Height above the ground.
        @italic(One height unit equals one GravityUp vector).
        Always use normalized GravityUp vector if you expect
        to receive here a normal distance.

        AboveHeight is always set to MaxSingle when returned result is @false
        (this guarantee simplifies some code).)

      @param(AboveGround Pointer to PTriangle representing the ground.
        Must be @nil if returned result is @false.
        @bold(May) be @nil even if we returned @true (not all 3D
        objects may be able to generate PTriangle information about collision).

        This may be useful for example to make a footsteps sound dependent
        on texture of the ground.
        Or to decrease player life points for walking on hot lava.
        See "The Castle" game for examples.)
    }
    function HeightCollision(const APosition, GravityUp: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean;

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
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; overload;
    function MoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; overload;
    { @groupEnd }

    { Check collision with a line segment, that is: a line between 2 points in 3D. }
    function SegmentCollision(const Pos1, Pos2: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean;

    { Check collision with a 3D sphere.

      This works precisely when transformation hierarchy has uniform scaling,
      i.e. scale is the same in all X, Y, Z axes.
      In case of non-uniform scaling, this is an approximation. }
    function SphereCollision(const Pos: TVector3; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;

    { Check collision with a sphere in 2D (a circle, extruded to infinity along the Z axis).

      Note that PointCollision2D and SphereCollision2D @italic(do not work
      reliably on objects that have 3D rotations). See @link(PointCollision2D)
      for details.

      This works precisely when transformation hierarchy has uniform scaling,
      i.e. scale is the same in all X, Y, Z axes.
      In case of non-uniform scaling, this is an approximation.

      @param(Details If non-nil, these are automatically filled with the details
        about the collision.
        If the result is @false, the Details contents are untouched.
        If the result is @true, the Details contents are set to describe
        the 3D objects hierarchy that caused this collision.) }
    function SphereCollision2D(const Pos: TVector2; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const Details: TCollisionDetails = nil): boolean;

    { Check collision with a point in 2D (which is an infinite line along the Z axis
      in 3D).

      Note that PointCollision2D and SphereCollision2D @italic(do not work
      reliably on objects that have 3D rotations), that is: rotations that change
      the direction of Z axis! This applies to all ways of rotating --
      using the @link(TCastleTransform)
      or using the X3D node @link(TTransformNode) (within a TCastleSce).

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
          and SphereCollision2D will change to something like ExtrudedCircleCollision,
          @italic(only when necessary).)
      )
    }
    function PointCollision2D(const Point: TVector2;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;

    { Check collision with axis-aligned box in 3D. }
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;

    { Check collision with a ray, building a TRayCollision result.
      Returns a collision as TRayCollision instance, or @nil if no collision.
      Caller is responsible for freeing the returned TRayCollision instance.

      Contrary to other collision routines, this should @italic(ignore
      the @link(Collides) property). The @link(Collides) property
      specifies whether item collides with camera. And this method is used
      for picking (pointing) 3D stuff --- everything visible can be picked,
      collidable or not. Instead, this looks at @link(Pickable) and @link(Exists) properties.

      This always returns the first collision with the world, that is
      the one with smallest TRayCollision.Distance. For example, when
      implemented in TCastleTransform, this checks collisions for all list items,
      and chooses the closest one. }
    function RayCollision(const RayOrigin, RayDirection: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;

    function LocalHeightCollision(const APosition, GravityUp: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean; virtual;
    function LocalMoveCollision(
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; overload; virtual;
    function LocalMoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; overload; virtual;
    function LocalSegmentCollision(const Pos1, Pos2: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const ALineOfSight: boolean): boolean; virtual;
    function LocalSphereCollision(const Pos: TVector3; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; virtual;
    function LocalSphereCollision2D(const Pos: TVector2; const Radius: Single;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
      const Details: TCollisionDetails = nil): boolean; virtual;
    function LocalPointCollision2D(const Point: TVector2;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; virtual;
    function LocalBoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; virtual;
    function LocalRayCollision(const RayOrigin, RayDirection: TVector3;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; virtual;

    { Render with given Params (includes a full transformation of this scene).

      This is @bold(mostly an internal method). You should not need to override
      it during normal engine usage. Instead, you should render everything using
      TCastleScene, which allows to load or build (by code) nodes to display
      meshes, light and everything else.
      But overriding this may be useful for special customized rendering. }
    procedure LocalRender(const Params: TRenderParams); virtual;

    { Render shadow volumes (with a full transformation of this scene).

      This is @bold(mostly an internal method). You should not need to override
      it during normal engine usage. If you render everything using TCastleScene,
      then rendering shadow volumes is also automatically handled by TCastleScene. }
    procedure LocalRenderShadowVolume(const Params: TRenderParams;
      const ShadowVolumeRenderer: TBaseShadowVolumeRenderer); virtual;

    { Can we use simple @link(Translation) to express our whole transformation
      (IOW, we have no scaling, no rotation).
      Returning @true allows optimization in some cases.
      @bold(Internal. Protected instead of private only for testing.)
      @exclude }
    function OnlyTranslation: boolean;

    { Get translation in 2D (uses @link(Translation), ignores Z coord). }
    function Translation2D: TVector2; deprecated 'use TranslationXY';

    { Transformation matrices, like @link(Transform) and @link(InverseTransform). }
    procedure TransformMatricesMult(var M, MInverse: TMatrix4); deprecated 'do not use this directly, instead use Transform and InverseTransform methods';
    procedure TransformMatrices(out M, MInverse: TMatrix4); deprecated 'do not use this directly, instead use Transform and InverseTransform methods';

    { Average value of 3D scale in @link(Scale).
      It is not calculated as a simple average, it's a little smarter
      to prevent from weird results sometimes, see @link(Approximate3DScale). }
    function AverageScale: Single;

    { Average value of 2D scale, from XY components of @link(Scale).
      It is not calculated as a simple average, it's a little smarter
      to prevent from weird results sometimes, see @link(Approximate2DScale). }
    function AverageScale2D: Single;

    { Called when fall ended. You can use FallHeight to decrease creature
      life or such. }
    procedure Fall(const FallHeight: Single); virtual;

    { Override to be notified about every transformation change.
      By default, this calls VisibleChangeHere, which causes the window to redraw. }
    procedure ChangedTransform; virtual;

    { Override to be notified after ExistsInRoot value changed. }
    procedure ExistsInRootChanged; virtual;
  public
    const
      DefaultMiddleHeight = 0.5;
      DefaultDirection: array [TOrientationType] of TVector3 = (
        (X: 0; Y: 0; Z: -1),
        (X: 0; Y: 0; Z: +1),
        (X: 0; Y: -1; Z: 0),
        (X: 1; Y: 0; Z: 0)
      );
      DefaultUp: array [TOrientationType] of TVector3 = (
        (X: 0; Y: 1; Z: 0),
        (X: 0; Y: 1; Z: 0),
        (X: 0; Y: 0; Z: 1),
        (X: 0; Y: 0; Z: 1)
      );

      { Default TCastleCamera.Orientation. }
      DefaultCameraOrientation = otUpYDirectionMinusZ;

    class var
      { Default value of @link(TCastleTransform.Orientation) for new TCastleTransform instances,
        except for cameras (cameras use special DefaultCameraOrientation).

        This is @link(otUpYDirectionZ) by default.
        This matches glTF orientation (as exported from Blender and other software). }
      DefaultOrientation: TOrientationType;

    var
      { Workaround for descendants where BoundingBox may suddenly change
        but their logic depends on stable (not suddenly changing) Middle.
        If InternalMiddleForceBox then we will use given InternalMiddleForceBoxValue
        instead of LocalBoundingBox for Middle and PreferredHeight
        calculation. Descendants that deal with this should usually have
        some timeout when they restore InternalMiddleForceBox to false.

        This is quite internal hack and you should not use this in your own programs.
        This is used only by TWalkAttackCreature.
        @exclude }
      InternalMiddleForceBox: boolean;
      { @exclude }
      InternalMiddleForceBoxValue: TBox3D;

      { Make the transform rendered and (TODO) processed only within parents
        that are MeaningfulParent, which means: do not enter TCastleTransformReference.Reference.
        This is useful for debug transforms to visualize selection/gizmos to transform
        in CGE editor.
        It is not a complete implementation (right now it only really blocks rendering)
        and it's generally a hack, in the future visualizing selection/gizmos
        in CGE editor should probably not be put as parents of non-debug transforms.

        This is quite internal hack for CGE editor and you should not use this
        in your own programs.
        It may (and probably will) disappear at some point.
        @exclude }
      InternalExistsOnlyInMeaningfulParents: Boolean;

      { Scale of Parent and other higher TCastleTransform does not affect this.
        Useful for various debug visualizations.
        This is internal, and implementation is a bit hacky -- we avoid parent scale
        where needed, not in all relevant places.
        Also resulting rendering frustum is not correct.
        @exclude }
      InternalIgnoreParentScale: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeforeDestruction;override;

    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function GetEnumerator: TEnumerator;

    {$ifdef FPC}
    property GetExists: Boolean read FExists; deprecated 'use Exists';
    {$endif}

    { Does this transform have a collision mesh that TCastleMeshCollider can use.
      Default implementaion returns @false. }
    function HasColliderMesh: Boolean; virtual;

    { Enumerate triangles for a collision mesh that TCastleMeshCollider can use. }
    procedure ColliderMesh(const TriangleEvent: TTriangleEvent); virtual;

    { Does the item really collide. Trivial shortcut for checking @link(Collides) and @link(Exists). }
    function CheckCollides: Boolean;
    function GetCollides: boolean; virtual; deprecated 'use CheckCollides';

    { Is the item really pickable. Trivial shortcut for checking @link(Pickable) and @link(Exists). }
    function CheckPickable: boolean;
    function GetPickable: boolean; virtual; deprecated 'use CheckPickable';

    { Is the item really visible. Trivial shortcut for checking @link(Visible) and @link(Exists). }
    function CheckVisible: boolean;
    function GetVisible: boolean; virtual; deprecated 'use CheckVisible';

    { Add a child.

      Note that adding the same child multiple times is allowed,
      in general you can use the same TCastleTransform instance multiple times in one hierarchy
      of TCastleRootTransform. }
    procedure Add(const Item: TCastleTransform);

    { Insert a child at a specific position on the children list.

      Usually the order of children doesn't matter, so there's not much point
      in using this method. You can as well use @link(Add).
      In particular, for rendering opaque (3D or 2D objects) the front/back is decided by
      the actual translation of the object in 3D, i.e. what is closer to the camera.
      (In a typical 2D setup, larger Z values mean "closer to the camera".)
      The position on the children list doesn't matter in this case.

      However, the position matters in case of blending.
      See https://castle-engine.io/blending .
      For correct blending, when you have multiple partially-transparent scenes,
      the partially-transparent objects must be sorted from back to front on the list.
      So you should add them in the proper order, or insert on proper positions,
      or use @link(SortBackToFront2D) on parent transformation. }
    procedure Insert(const Index: Integer; const Item: TCastleTransform);

    { Remove a child.

      Note that adding and removing from the TCastleTransform hierarchy is guaranteed
      to be fast, so you can do it even in the middle of the game.
      In particular calling @link(Remove) doesn't free rendering reasources
      of the removed scene,
      so removing scene only to add it later to another TCastleViewport.Items is fast. }
    procedure Remove(const Item: TCastleTransform);

    { Use this to remove (and possibly free) the child, but not immediately.

      The actual removal (and freeing, if FreeItem) will be done at some crucial points:

      @unorderedList(
        @itemSpacing Compact
        @item Right before we render children.
        @item Right before we loop over children in the @link(Update) method.
        @item Right after we loop over children in the @link(Update) method.
        @item From our destructor.
      )

      This allows to use this method safely even in the middle of iteration
      over TCastleTransform children. For example you can use it from @link(Update) method,
      from any TCastleTransform.Update or TCastleUserInterface.Update (including TCastleView.Update).

      Note: It is not a problem if the child scheduled to be removed
      will also be removed (or even freed) directly e.g. by just calling @code(Free) on it.
      We ignore removal of items that are no longer
      our children, and we automatically remove the child from "pending to remove" list
      if it is freed. You only need to be careful in case you add the same TCastleTransform
      instance multiple times to the same parent
      (see https://castle-engine.io/viewport_and_scenes_from_code#_multiple_instances_of_the_same_scene )
      as then each removal removes one instance (not all) of the child from parent. }
    procedure RemoveDelayed(const Item: TCastleTransform; const FreeItem: Boolean = false);

    { Remove a child, by index on the @link(Items) list. }
    procedure Delete(const Index: Integer);

    { List of current children. }
    property Items[const I: Integer]: TCastleTransform read GetItem write SetItem; default;

    { Count of current children, to iterate over @link(Items) from 0 to Count-1. }
    function Count: Integer;

    { Remove all children TCastleTransform instances. }
    procedure Clear;

    { Exchange position of 2 children TCastleTransform instances. }
    procedure Exchange(const Index1, Index2: Integer);

    { Return parent TCastleTransform.
      It ignores (never returns) parents caused by referencing transform in TCastleTransformReference.
      If there is (or was, at some point) more than 1 parent (not counting parents
      caused by TCastleTransformReference) then it may return nil, or any of the existing parents
      -- it is undefined which exactly.

      In simple cases, one TCastleTransform is just a child of one other
      TCastleTransform, and then this property returns this obvios parent.

      In general, one TCastleTransform instance may be used
      multiple times as a child of other TCastleTransform instances
      (as long as they are all within the same TCastleViewport).
      The parent TCastleTransform instances may include TCastleTransformReference
      (this allows to create multiple references to one TCastleTransform from editor)
      but in general any parents are allowed
      (when you use code, you can just insert TCastleTransform as a child of other TCastleTransform,
      without the need for TCastleTransformReference).

      This property indicates the one parent that is @italic(not) TCastleTransformReference.
      If there are multiple such parents (this is impossible at design-time in editor,
      but it is possible at runtime if you add/remove TCastleTransform children by code)
      then this returns @nil or any of existing parents.

      It is @nil if this is just the root transform, not added to any parent. }
    property Parent: TCastleTransform read FParent;

    {$ifdef FPC}property UniqueParent: TCastleTransform read FParent; deprecated 'use Parent';{$endif}

    { Sort objects back-to-front @italic(right now)
      following one of the blending sorting algorithms.
      Only the immediate list items are reordered,
      looking at their bounding boxes (the sorting is not recursive).

      Calling this method makes sense if you use
      @url(https://castle-engine.io/blending blending)
      and multiple partially-transparent objects may
      be visible at the same place on the screen.
      Sorting avoids artifacts when rendering.

      @unorderedList(
        @item(
          In general, you should call this method whenever the correct back-to-front order
          of objects (with respect to the current camera) changes.
        )
        @item(
          In 3D, it may make sense to call this method even every frame
          (like in every @link(TCastleWindow.OnUpdate)).
          Call it if you move or otherwise change the objects (changing their bounding boxes),
          or if the CameraPosition may change. Note that CameraPosition is only
          relevant if BlendingSort is one of the bs3D* options,
          like bs3D, bs3DOrigin, bs3DGround.
        )
        @item(
          In 2D, it is a bit simpler.
          You typically need to call this only when some objects' Z value changed
          (making this object move behind / in front of some other object),
          or when new object is added.
          You don't need to call this method when camera changes or when object's XY position
          changes, as they don't affect the order.
        )
      )

      Note that this doesn't take care of sorting the shapes
      within the scenes. For this, you should set
      @link(TCastleRenderOptions.BlendingSort Scene.RenderOptions.BlendingSort)
      to a value like bs3D or bs3DOrigin or bs3DGround, to keep it sorted.
      The bs3D is the default now (see @link(TCastleRenderOptions.DefaultBlendingSort)),
      so you typically don't need to worry about it.

      See the TBlendingSort documentation for the exact specification
      of sorting algorithms. Using BlendingSort = bsNone does nothing.

      @seealso SortBackToFront2D }
    procedure SortBackToFront(const BlendingSort: TBlendingSort;
      const CameraPosition: TVector3);

    { Sort objects back-to-front @italic(right now)
      following the 2D blending sorting algorithm.
      See @link(SortBackToFront) for documentation, this method
      is only a shortcut for @code(SortBackToFront(bs2D, TVector3.Zero)).

      @seealso SortBackToFront }
    procedure SortBackToFront2D;

    { Bounding box of this object, in the coordinate system of the parent transformation.
      This method takes into account current transformation
      (like @link(Translation), @link(Rotation))
      but not parent TCastleTransform transformations.
      Use @link(WorldBoundingBox) instead to know bounding box that accounts for all
      TCastleTransform transformations.
      Use @link(LocalBoundingBox) instead to know bounding box that does not account for
      any parent or this TCastleTransform transformations.

      Takes into account both collidable and visible objects.
      For example, invisible walls (not visible) and fake walls
      (not collidable) should all be accounted here.

      It's a @italic(bounding) volume, it should be as large as necessary
      to include the object inside. At the same time, it should be
      as "tight" as it can, to make various optimizations work best.

      @seealso WorldBoundingBox
      @seealso LocalBoundingBox }
    function BoundingBox: TBox3D;

    { Bounding box of this object, ignoring the transformations of this scene and parents.

      @seealso BoundingBox
      @seealso WorldBoundingBox }
    function LocalBoundingBox: TBox3D; virtual;

    { Bounding box of this object, taking into account
      all transformations of this and parents.

      @seealso BoundingBox
      @seealso LocalBoundingBox }
    function WorldBoundingBox: TBox3D;

    { Render given object.
      Should check and immediately exit when @link(CheckVisible) is @false.

      The rendering transformation, frustum, and filtering
      is specified inside TRenderParams class.
      This method should only update @code(TRenderParams.Statistics). }
    procedure Render(const Params: TRenderParams); overload; virtual;

    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); overload;
      deprecated 'use Render method without an explicit Frustum parameter, it is in Params.Frustum now';

    { Does the 3D object cast shadows by shadow volumes.
      See also TCastleScene.ReceiveShadowVolumes. }
    property CastShadowVolumes: boolean
      read FCastShadows write FCastShadows default true;
      {$ifdef FPC}deprecated 'use CastShadows';{$endif}

    { Render shadow quads for all the things rendered by @link(Render).
      This is done only if @link(Exists) and @link(CastShadows).

      It does shadow volumes culling inside (so ShadowVolumeRenderer should
      have FrustumCullingInit already initialized).

      Params.Transform,InverseTransform,TransformIdentity describe the transformation,
      just like for @link(Render). }
    procedure RenderShadowVolume(const Params: TRenderParams;
      const ShadowVolumeRenderer: TBaseShadowVolumeRenderer);

    { Prepare resources, making various methods (like rendering and such)
      to execute fast.

      It is usually simpler to call @link(TCastleViewport.PrepareResources)
      then this method. Calling @code(Viewport.PrepareResources(MyScene))
      will automatically call @code(MyScene.PrepareResources(...)) underneath,
      with proper parameters.

      It is best to call this when the rendering context is initailized,
      e.g. at Application.OnInitialize or later.
      Calling this method before the rendering context is initialized
      (e.g. from initializaton section of some unit)
      will have to skip some preparations, thus reducing the effectiveness of this method.

      This makes sure that appropriate methods execute as fast as possible.
      It's never required to call this method
      --- everything will be prepared "as needed" anyway.
      But if you allow everything to be prepared "as needed",
      then e.g. the first @link(Render) call may take a long time because it may
      have to prepare resources that will be reused in next @link(Render) calls.
      This may make your program seem slow at the beginning
      (when rendering resources are being prepared, so at the first frame,
      or a couple of first animation frames). To avoid this, call this method,
      showing the user something like "now we're preparing
      the resources --- please wait".

      @param(Options What features should be prepared to execute fast.
        See TPrepareResourcesOption.)

      @param(Params
        Rendering parameters to prepare for.
        It is used only if Options contains prRenderSelf or prRenderClones.
      )
    }
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const Params: TPrepareParams); overload; virtual;

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const Ignored: Boolean;
      const Params: TPrepareParams); overload;
      deprecated 'use PrepareResources(Options,Params) overload (remove the 2nd Boolean parameter)';

    { Press and release events of key and mouse, passed only to instances that set ListenPressRelease.
      Return @true if you handled the event, and it should not be passed to other objects.
      See also TCastleUserInterface analogous events.
      @groupBegin }
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
    { @groupEnd }

    { Are @link(Press) and @link(Release) virtual methods called. }
    property ListenPressRelease: Boolean
      read FListenPressRelease write SetListenPressRelease default false;

    { Pointing device (mouse or touch) events, you can override these to handle pointing device events.
      These methods are automatically called by the TCastleViewport.
      They are exposed here only to allow overriding them.
      Return @true if you handled the event.

      @unorderedList(
        @item(PointingDevicePress signals that the picking button (usually,
          left mouse button) was pressed.

          Note that the exact key or mouse responsible for this is configurable
          in our engine by Input_Interact. By default it's the left mouse button,
          as is usual for VRML/X3D browsers. But it can be configured to be other
          mouse button or a key, for example most 3D games use "e" key to interact.
        )

        @item(PointingDeviceRelease signals that the picking button is released.

          An extra parameter CancelAction indicates
          whether this pointing device "press and release" sequence may be
          considered a "click". When CancelAction = @true, then you should not make
          a "click" event (e.g. TouchSensor should not send touchTime event etc.).
        )

        @item(PointingDeviceMove signals that pointer moves over this object.)
      )

      They receive Pick information (TRayCollisionNode) about what exactly is hit
      by the 3D ray corresponding to the current pointing device position.
      It contains the detailed information about the point, triangle
      and ray (all in local coordinate system of this TCastleTransform) that are indicated
      by the pointing device.
      TRayCollisionNode.Triangle is @nil when it was not possible to determine,
      and TRayCollisionNode.Point is undefined in this case.

      They also receive Distance to the collision point,
      in world coordinates. See TRayCollision.Distance.
      The Distance may be MaxSingle when it was not possible to determine.

      There is a concept of a TCastleTransform that is currently "capturing"
      the pointing device events. Once TCastleTransform
      handles TCastleTransform.PointingDevicePress (returns @true for it),
      it captures the following PointingDeviceMove and PointingDeviceRelease events,
      regardless if ray still hits this TCastleTransform instance.
      The "capturing" instance of TCastleTransform is informed first about
      pointing device move/release.

      After the "capturing" instance,
      every pointing device event (press, release or move) is send to the leaf
      in TCastleTransform hierarchy (usually a TCastleScene) that is under the mouse/touch
      position.
      If the event is not handled, it is passed to other objects under the mouse/touch
      position.

      The PointingDeviceMove event is also always passed to @link(TCastleRootTransform.MainScene)
      at the end (if it wasn't already the "capturing" transform,
      or under the mouse/touch position).
      This way @link(TCastleRootTransform.MainScene) is always informed about pointing device movement.

      These methods are called only if the object Exists.
      There's no need to check this condition inside the method implementation.

      @groupBegin }
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; virtual;
    function PointingDeviceRelease(const Pick: TRayCollisionNode;
      const Distance: Single; const CancelAction: Boolean): Boolean; virtual;
    function PointingDeviceMove(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; virtual;
    { @groupEnd }

    { Continuously occuring event, for various tasks.

      Executed only on instances that have @link(Exists) (no need to check this
      in overridden Update implementation).

      @param(RemoveMe Set this to rtRemove or rtRemoveAndFree to remove
        this item from parent after this call finished.
        rtRemoveAndFree additionally will free this item.
        Initially it's rtNone when this method is called.

        You can also use RemoveDelayed from parent for similar effect.
      ) }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); virtual;

    { Something visible changed inside @italic(this) object.
      This is usually called by implementation of this object,
      to notify others that it changed.

      Changes is a set describing what changes occurred.
      See TVisibleChange docs for more information.
      It must specify all things that possibly changed.

      Changes can be [], meaning "something tells us to redraw, but no visible change
      happened yet, maybe something will happen during a redraw"
      (this is used when e.g. possibly LOD level changed).
      We still increase TCastleAbstractRootTransform.InternalVisibleStateId
      even when Changes=[].

      The information about visibility changed is passed to TCastleAbstractRootTransform
      in @link(World).
      It increases @link(TCastleAbstractRootTransform.InternalVisibleStateId),
      @link(TCastleAbstractRootTransform.InternalVisibleGeometryStateId),
      @link(TCastleAbstractRootTransform.InternalVisibleNonGeometryStateId).
      If you want to @italic(react) to visibility changes,
      you should not override this method, instead watch above "state id" variables
      and react when they change. }
    procedure VisibleChangeHere(const Changes: TVisibleChanges); virtual;

    { Root transformation (TCastleAbstractRootTransform) containing us.
      @nil if we are not (yet) part of some hierarchy rooted in TCastleAbstractRootTransform. }
    property World: TCastleAbstractRootTransform read FWorld;

    { Mouse cursor over this object. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Called when rendering context is destroyed.
      This will be also automatically called from destructor.
      Object should clear here any resources that are connected to the rendering context. }
    procedure GLContextClose; virtual;

    { Middle point, usually "eye point", of the 3D model.
      This is used for sphere center
      (if @link(CollisionSphereRadius) is non-zero or @link(Sphere) returns @true)
      and is the central point from which collisions of this object
      are checked (Move, MoveAllowed, Height, LineOfSight).
      It's useful for dynamic objects like player and moving creatures,
      which rely on @link(MoveAllowed) and gravity.

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
      (so only when @link(TCastleTransform.Translation) can change).

      In this class this returns something sensible above the bottom
      of the box. See @link(TCastleTransform.MiddleHeight).

      This is expressed in the parent coordinate system
      (so it is close to the @link(Translation) value, but moved up, following GravityUp).
      It ignores parent transformations (using @code(Transform.Parent.LocalToWorld(Transform.Middle))
      to convert this to world coordinates. }
    function Middle: TVector3; virtual;

    { Can the approximate sphere (around Middle point)
      be used for some collision-detection
      tasks. If @true then Radius (and Middle point) determine the approximate
      sphere surrounding the 3D object (it does not have to be a perfect
      bounding sphere around the object), and it may be used for some
      collisions instead of BoundingBox.
      See @link(CollidesWithMoving) and @link(MoveAllowed) for when it may happen.

      Must return @false when not Exists (because we can't express
      "empty sphere" by @link(Sphere) method for now, but BoundingBox can express
      TBox3D.Empty).

      By default, in TCastleTransform class, this returns @true if @link(CollisionSphereRadius)
      is non-zero.

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
      and other moving level parts (TCastleMoving instances).

      Some 3D moving objects may try to avoid crushing this item.
      Like an automatic door that stops it's closing animation
      to not crush things standing in the doorway.

      Some other 3D moving objects may push this object.
      Like elevators (vertical, or horizontal moving platforms).
      We may use sphere (see @link(CollisionSphereRadius) and @link(Sphere)) for checking
      collisions, or bounding box (@link(TCastleTransform.BoundingBox)), depending on need. }
    property CollidesWithMoving: boolean read FCollidesWithMoving write FCollidesWithMoving default false;

    { Get height of my point above the rest of the world.

      The given MyPosition, and returned AboveHeight, are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.Height(MyTransform.Translation, ...)).

      This ignores the geometry of this 3D object (to not accidentally collide
      with your own geometry), and checks collisions with the rest of the world.
      @groupBegin }
    function Height(const MyPosition: TVector3;
      out AboveHeight: Single): boolean; overload;
    function Height(const MyPosition: TVector3;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean; overload;
    { @groupEnd }

    { Whether there is line of sight (the line segment does not collide with anything
      opaque) between these 2 points.

      The given Pos1, Pos2 are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.LineOfSight(MyTransform.Translation, MyTransform.Translation + MyTransform.Direction * 10)).

      This ignores the geometry of this 3D object (to not accidentally collide
      with your own geometry), and checks collisions with the rest of the world. }
    function LineOfSight(const Pos1, Pos2: TVector3): boolean;

    { Is the move from OldPos to ProposedNewPos possible for this object.
      Returns true and sets NewPos if some move is allowed.

      The NewPos may be different than ProposedNewPos,
      which allows to perform wall-sliding.
      Wall sliding will only work if collision sphere is defined,
      which should be configured by setting CollisionSphereRadius to something non-zero.
      Otherwise, the move uses only box collisions, and wall sliding doesn't happen.

      When checking collisions, it avoids colliding with itself,
      so it only checks collisions with the rest of the world (things outside of this TCastleTransform).

      The given OldPos, ProposedNewPos, NewPos are in the parent
      coordinate system of this TCastleTransform.
      Intuitively, you are asking @italic("Can I change @link(Translation)
      from OldPos to NewPos").
      So this method is consistent with @link(Move), @link(Translate). }
    function MoveAllowed(const OldPos, ProposedNewPos: TVector3;
      out NewPos: TVector3;
      const BecauseOfGravity: boolean): boolean; overload;

    { Is the move from OldPos to NewPos possible for this object.

      This overloaded version of MoveAllowed doesn't do wall-sliding
      (use the version with ProposedNewPos for wall-sliding).

      If this object allows to use sphere for collisions
      (see @link(CollisionSphereRadius) and @link(Sphere)) then sphere will be used.
      Otherwise, it will collide as a bounding box.

      When checking collisions, it avoids colliding with itself,
      so it only checks collisions with the rest of the world (things outside of this TCastleTransform).

      The given OldPos, NewPos are in the parent
      coordinate system of this TCastleTransform.
      Intuitively, you are asking @italic("Can I change @link(Translation)
      from OldPos to NewPos").
      So this method is consistent with @link(Move), @link(Translate). }
    function MoveAllowed(const OldPos, NewPos: TVector3;
      const BecauseOfGravity: boolean): boolean; overload;

    { Check for ray collisions within this TCastleTransform and children.
      @exclude }
    function InternalRayCollision(const RayOrigin, RayDirection: TVector3): TRayCollision;

    { Cast a ray, see what is hit (checks the whole world, not just this TCastleTransform hierarchy).

      The given RayOrigin, RayDirection are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.Ray(MyTransform.Translation, MyTransform.Direction)).

      This ignores the geometry of this object (to not accidentally collide
      with your own geometry), and checks collisions with the rest of the world. }
    function Ray(const RayOrigin, RayDirection: TVector3): TRayCollision;

    { Cast a ray, see what is hit (checks the whole world, not just this TCastleTransform hierarchy).

      The given RayOrigin, RayDirection are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.RayCast(MyTransform.Translation, MyTransform.Direction)).
      In case of the overloaded version with Distance parameter,
      the Distance is consistently in the same, parent coordinate system.

      This ignores the geometry of this object (to not accidentally collide
      with your own geometry), and checks collisions with the rest of the world.

      This returns the TCastleTransform that is hit (this is the "leaf" TCastleTransform
      in the TCastleTransform tree that is hit)
      and a distance from RayOrigin to the hit point.
      Returns @nil (Distance is undefined in this case) if nothing was hit.
      Use @link(Ray) for a more advanced version of this, with more complicated result.
      @groupBegin }
    function RayCast(const RayOrigin,
      RayDirection: TVector3): TCastleTransform; overload;
    function RayCast(const RayOrigin, RayDirection: TVector3;
      out Distance: Single): TCastleTransform; overload;
    { @groupEnd }

    { Is this object's bounding volume (@link(BoundingBox))
      included in parent bounding volume.
      This should be always @true for non-debug scenes.
      Violating this may cause rendering artifacts, things could
      disappear when they should not.
      Using this is reasonable only if you attach a debug geometry
      to your scene, and you don't want to enlarge your bounding volume
      (e.g. because this debug geometry visualizes something determined
      by the bounding volume, and it would create a "feedback loop"
      if the visualization itself would enlarge the bounding box). }
    property InternalExcludeFromParentBoundingVolume: boolean
      read FInternalExcludeFromParentBoundingVolume
      write FInternalExcludeFromParentBoundingVolume;

    { Convert position between local and outside coordinate system.
      This is called OutsideToLocal, not WorldToLocal, because it only handles transformation
      defined in this item --- it does not recursively apply all transform on the way to root.
      @groupBegin }
    function OutsideToLocal(const Pos: TVector3): TVector3;
    function LocalToOutside(const Pos: TVector3): TVector3;
    { @groupEnd }

    { Convert position between local and world coordinate system.
      This applies all the transformations on the way to root,
      so it looks at this object as well as all parents' transformations.
      @groupBegin }
    function WorldToLocal(const Pos: TVector3): TVector3;
    function LocalToWorld(const Pos: TVector3): TVector3;
    { @groupEnd }

    { Convert direction between local and world coordinate system.
      This applies all the transformations on the way to root,
      so it looks at this object as well as all parents' transformations.
      @groupBegin }
    function WorldToLocalDirection(const Dir: TVector3): TVector3;
    function LocalToWorldDirection(const Dir: TVector3): TVector3;
    { @groupEnd }

    { Convert distance, like sphere radius, between local and world coordinate system.
      This applies all the scaling on the way to root,
      so it looks at this object as well as all parents' transformations.
      @groupBegin }
    function LocalToWorldDistance(const Distance: Single): Single;
    function WorldToLocalDistance(const Distance: Single): Single;
    { @groupEnd }

    { Gravity may make this object fall down (see FallSpeed)
      or grow up (see GrowSpeed). See also PreferredHeight.

      Special notes for TPlayer: player doesn't use this (TPlayer.Gravity
      should remain @false), instead player relies on
      TPlayer.Navigation.Gravity = @true, that does a similar thing (with some
      extras, to make camera effects). This will change in the future,
      to merge these two gravity implementations.
      Although the TPlayer.Fall method still works as expected
      (it's linked to TCastleWalkNavigation.OnFall in this case). }
    property Gravity: boolean read FGravity write FGravity default false;
      {$ifdef FPC}deprecated 'use physics (TCastleRigidBody, TCastleCollider) to make things affected by gravity';{$endif}

    { Falling speed, in units per second, for @link(Gravity).

      This is relevant only if @link(Gravity) and PreferredHeight <> 0.
      0 means no falling. }
    property FallSpeed: Single read FFallSpeed write FFallSpeed {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use physics (TCastleRigidBody, TCastleCollider) to make things affected by gravity';{$endif}

    { Growing (raising from crouching to normal standing position)
      speed, in units per second.
      This is used by non-flying creatures when climbing up stairs,
      in which case @link(Translation) ("legs positon") may be sometimes under
      the ground while Middle ("eyes position") will be always above the ground
      and will try to grow to be at PreferredHeight above the ground.

      This is relevant only if @link(Gravity) and PreferredHeight <> 0.
      0 means no growing. }
    property GrowSpeed: Single read FGrowSpeed write FGrowSpeed {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use physics (TCastleRigidBody, TCastleCollider) to make things affected by gravity';{$endif}

    { The preferred height of the object @link(Middle) above the ground,
      when the object is standing on the ground firmly.
      This is used by objects affected by gravity (like non-flying creatures
      and items) to know how far they should fall down or grow up.

      The default implementation in this class looks at MiddleHeight property,
      see the algorithm described there.
      This may be dynamic (may change during creature lifetime,
      so you can make the creature duck or grow if you want). }
    function PreferredHeight: Single; virtual;
      {$ifdef FPC}deprecated 'use physics (TCastleRigidBody, TCastleCollider) to make things affected by gravity';{$endif}

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

      This property determines how the TCastleTransform
      handles the @link(Middle) implementation
      (this is the point used for various collision detection routines)
      and @link(PreferredHeight) (this is the preferred height of @link(Middle)
      above the ground). You can override these two methods to use a different
      approach, and then ignore MiddleHeight completely. }
    property MiddleHeight: Single read FMiddleHeight write FMiddleHeight
      {$ifdef FPC}default DefaultMiddleHeight{$endif};

    { Transformation (from local to outside) as a matrix.
      This matrix represents a concise version of properties like @link(Translation),
      @link(Rotation), @link(Scale). It does not take into account the
      transformation of parent TCastleTransform (for this, use @link(WorldTransform)). }
    function Transform: TMatrix4;

    function OutsideTransform: TMatrix4;

    { Inverse transformation as a matrix, thus transforming from outside to local
      coordinate system. This is an inverse of @link(Transform). }
    function InverseTransform: TMatrix4;

    function InverseOutsideTransform: TMatrix4;

    { All conditions are satisfied to have @link(WorldTransform).
      When this returns @true, you know that @link(WorldTransform)
      and @link(WorldInverseTransform) will not raise @link(ETransformParentUndefined). }
    function HasWorldTransform: boolean;

    { Transformation (from local to world) as a matrix.
      This accumulates the transformation of this instance
      (derived from properties like @link(Translation), @link(Rotation), @link(Scale))
      with the transformation of parent @link(TCastleTransform) instances,
      all the way up to and including the root transformation
      (@link(TCastleAbstractRootTransform)).
      Thus, this is a transformation to the world known to the
      @link(TCastleViewport) instance.

      Two conditions are necessary to make this available:

      @unorderedList(
        @item(
          This instance must be part of some @link(World).
          So it must be added to @link(TCastleViewport.Items Viewport.Items),
          or to some other @link(TCastleTransform) that is part of @link(World).

          Otherwise reading this raises @link(ENotAddedToWorld).
        )

        @item(
          This instance must not be present multiple times inside the @link(World).
          Ever (even in the past).

          In general, it is allowed to have multiple references to the same TCastleTransform
          within the same @link(World). So you can add
          the same TCastleTransform or TCastleScene many times to
          @link(TCastleViewport.Items Viewport.Items).
          This is a useful optimization (sharing), and is explicitly allowed.
          But if you do this --- you cannot rely on WorldTransform property.

          Otherwise reading this raises @link(EMultipleReferencesInWorld)
          or @link(ETransformParentUndefined).
        )
      )

      You can check HasWorldTransform before calling this method,
      to avoid catching an exception.

      Note that the WorldTransform is not updated in @link(Update),
      it is smartly updated on-demand.
      So you do not have to wait for @link(Update) or other method to be called
      before accessing the @link(WorldTransform).

      @raises(ENotAddedToWorld
        When this instance is not yet part of @link(World).)
      @raises(EMultipleReferencesInWorld
        When this instance is added multiple times to @link(World).)
      @raises(ETransformParentUndefined
        When this instance was once added multiple times to @link(World),
        or for some other reason cannot be calculated.
        This is an ancestor of ENotAddedToWorld and EMultipleReferencesInWorld
        too.)
    }
    function WorldTransform: TMatrix4;

    { Inverse transformation of @link(WorldTransform),
      thus transforming from world to local coordinate system.

      See @link(WorldTransform) for more details how this works.

      @raises(ENotAddedToWorld
        When this instance is not yet part of @link(World).)
      @raises(EMultipleReferencesInWorld
        When this instance is added multiple times to @link(World).)
      @raises(EMultipleReferencesInWorld
        When this instance was once added multiple times to @link(World),
        or for some other reason cannot be calculated.
        This is an ancestor of ENotAddedToWorld and EMultipleReferencesInWorld
        too.)
    }
    function WorldInverseTransform: TMatrix4;

    { Unconditionally move this object by a given vector.

      To move and check collisions, use @link(Move) instead of this method.

      The provided TranslationChange should be a direction in the parent
      coordinate system of this TCastleTransform.
      Using this routine is exactly equivalent to
      @code(Translation := Translation + TranslationChange). }
    procedure Translate(const TranslationChange: TVector3);

    { Move, if possible (checking collisions with other objects in world).
      This is the simplest way to move an object,
      and a basic building block for artificial intelligence of creatures.

      Checks move possibility by MoveAllowed, using @link(Middle) point.
      Actual move is done using @link(Translate).

      Note that wall sliding will only work if collision sphere is defined,
      which should be configured by setting CollisionSphereRadius to something non-zero.
      Otherwise, the move uses only box collisions, and wall sliding doesn't happen.

      The provided TranslationChange should be a direction in the parent
      coordinate system of this TCastleTransform,
      so using this routine is consistent with doing
      @code(Translation := Translation + TranslationChange)
      however this checks collisions. }
    function Move(const TranslationChange: TVector3;
      const BecauseOfGravity: boolean;
      const EnableWallSliding: boolean = true): boolean;

    function RenderOutside: boolean;

    { Translation (move) the children. Zero by default.

      Set it like this:
      @longCode(#
        Scene.Translation := Vector3(1, 2, 3);
        Scene.Translation := Scene.Translation + Vector3(4, 5, 6);
      #) }
    property Translation: TVector3 read FTranslation write SetTranslation;

    { Get or set the XY components of the translation.
      Useful for 2D games. }
    property TranslationXY: TVector2 read GetTranslationXY write SetTranslationXY;

    { Center point around which the @link(Rotation) and @link(Scale) is performed. }
    property Center: TVector3 read FCenter write SetCenter;

    { Rotation in 3D, around a specified axis.
      Rotation is expressed as a 4D vector, in which the first 3 components
      specify the rotation axis (does not need to be normalized, but must be non-zero)
      and the last component is the rotation angle @italic(in radians).

      Note: the Rotation axis (first 3 components) must not be a zero vector,
      however as a special case the rotation value @code(TVector4.Zero) (so all components,
      axis and angle, are zero) is also allowed.

      Rotation is done around @link(Center).

      Set it like this:
      @longCode(#
        Scene.Rotation := Vector4(0, 0, 1, Pi / 10);
        Scene.Rotation := Scene.Rotation + Vector4(0, 0, 0, Pi / 10);
      #) }
    property Rotation: TVector4 read FRotation write SetRotation;

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
          calculated then:

          @unorderedList(
            @item(Sphere collision routines
              @link(TCastleAbstractRootTransform.WorldSphereCollision),
              @link(TCastleAbstractRootTransform.WorldSphereCollision2D)
              only do an approximation in case of non-uniform scale.)

            @item(The physics engine also doesn't support non-uniform scale.
              Descendants of @link(TCastleCollider) may assume that scale
              is uniform, and use an approximation (average) of the actual 3D
              scale when necessary.)
          )

          Note that @link(ScaleOrientation) matters only in case of non-uniform scale.
        )

        @item(All scale components should > 0 if you want 3D lighting
          to work corrrectly. That is, avoid negative scale, that changes
          the normals and orientation of faces (counter-clockwise becomes clockwise,
          if all scale components are -1), or standard
          lighting may not work Ok.

          For unlit stuff, or custom lighting, negative scale may be Ok.
          For many 2D games that use no lighting/custom lighting,
          negative scale is Ok.

          If you really need to use negative scale and lighting,
          enable @link(TCastleRenderOptions.RobustNegativeScale).)

        @item(At least, keep all scale components non-zero.
          Otherwise the scaling operation is not invertible,
          and generally collisions will not work correctly.

          If you really need to set zero scale, the object should not be collidable then.
          Set @link(Collides) = @false and don't add @link(TCastleCollider) as a behavior.)
      )

      Set it like this:
      @longCode(#
        Scene.Scale := Vector3(5, 5, 5);
        Scene.Scale := Scene.Scale * 1.5;
      #) }
    property Scale: TVector3 read FScale write SetScale;

    { Orientation in which 3D @link(Scale) is performed. }
    property ScaleOrientation: TVector4 read FScaleOrientation write SetScaleOrientation;

    { Make the transform do nothing --- zero @link(Translation), zero @link(Rotation),
      @link(Scale) to one. Also resets @link(ScaleOrientation). }
    procedure Identity;

    { Participate in the physics simulation.
      This makes this object collidable with other rigid bodies
      and it allows to move and rotate because of gravity
      or because of collisions with other objects
      (if @link(TCastleRigidBody.Dynamic) is @true).

      Setting this property makes this object
      a single rigid body for the physics engine.
      Underneath, it just adds TCastleRigidBody to behaviors.

      If this property is assigned and the @link(TCastleRigidBody.Dynamic) is @true
      (and @link(TCastleRigidBody.Dynamic) is @true by default)
      then this object is moved and rotated using the physics engine.
      It will move because of gravity (if @link(TCastleRigidBody.Gravity),
      also @true by default),
      and because of collisions with other objects.

      If you use deprecated TCollider:
      The @link(TCastleRigidBody.Collider) property must be initialized
      @italic(before) assigning the @link(TRigidBody) instance here.
      So you must create a @link(TCollider) descendant, specyfying the given
      @link(TRigidBody) as a parent.

      Note that an object can only be present once in the @link(World)
      to be a rigid body. Breaking this rule will cause the
      @link(EMultipleReferencesInWorld) exception at an undefined time.
    }
    property RigidBody: TCastleRigidBody read GetRigidBody write SetRigidBody;
      {$ifdef FPC}deprecated 'use AddBehavior (to set)/FindBehavior (to get)';{$endif}

    { @deprecated Deprecated name for Translation. }
    property Position: TVector3 read FTranslation write SetTranslation;
      {$ifdef FPC}deprecated 'use Translation';{$endif}

    { Direction that this transformation (like creature, space ship, missile, a camera...) is facing.
      The property @link(Up) contains an associated up vector.
      Together, the @link(Direction) and @link(Up) properties provide an alternative way
      to get and set the @link(Rotation) of the transformation.

      Thinking in terms of "direction" and "up" is more natural than thinking
      about @link(Rotation) for various game objects.
      For example, you can move a missile forward by this simple operation:

      @longCode(#
        MyMissile.Translation := MyMissile.Translation +
          MyMissile.Direction * SecondsPassed * MissileMoveSpeed;
      #)

      The @link(Orientation) determines what is the direction and up you get
      when the @link(Rotation) is zero.
      For example @link(Orientation) = otUpYDirectionZ (default now) means that:

      @orderedList(
        @item(When @link(Rotation) is zero,
          then @link(Up) will be +Y axis @code((0,1,0)) and
          @link(Direction) will be +Z axis @code((0,0,1)).)

        @item(Setting the @link(Direction) and @link(Up) to other values will be properly converted.)

        @item(So when @link(Orientation) = otUpYDirectionZ, and you set

          @longCode(#
          MyMissile.Direction := Vector3(0, 0, 1);
          MyMissile.Up := Vector3(0, 1, 0);
          #)

          then you effectively have set @link(Rotation MyMissile.Rotation) to zero (no rotation).
          That is, you could have as well done just this:

          @longCode(#
          MyMissile.Rotation := TVector4.Zero;
          #)

          Note that you can also set direction and up more efficiently by one @link(SetView) call.
          So this is one more way to do it, also equivalent:

          @longCode(#
          MyMissile.SetView(Vector3(0, 0, 1), Vector3(0, 1, 0));
          #)
        )
      )

      Note that changing @link(Orientation) property does not perform @italic(any) transformation
      of the model, it will look the same as it was, no matter what @link(Orientation) you set.
      Only changing the @link(Direction) or @link(Up) actually change the @link(Rotation),
      which in turn actually change how the model looks. This also implies that
      @link(Orientation) value completely doesn't matter if you never get or set
      @link(Direction) or @link(Up). All engine rendering and processing just looks at @link(Rotation).

      See @link(Orientation) for documentation of its default value.

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). But when setting them by these properties, we will normalize
      them automatically, so you don't need to worry about that.

      The direction and up vectors must always be orthogonal.
      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      @groupBegin }
    property Direction: TVector3 read GetDirection write SetDirection;
    property Up: TVector3 read GetUp write SetUp;
    { @groupEnd }

    { Get at once vectors: position, direction, up.

      Just like @link(Translation) and @link(Rotation),
      these vectors reflect the transformation defined locally
      in this TCastleTransform, disregarding the parent transformations.
      Use GetWorldView to account for parent transformations. }
    procedure GetView(out APos, ADir, AUp: TVector3);

    { Get position, direction, up. Similar to @link(GetView), but in world coordinates.

      Returned ADir and AUp are always orthogonal.
      Returned ADir and AUp are always normalized. }
    procedure GetWorldView(out APos, ADir, AUp: TVector3);

    { Get position in world coordinates.

      The returned position is the same as by @link(GetWorldView).
      This method just doesn't calculation anything else, thus it is faster
      than @link(GetWorldView) if you don't need direction/up. }
    function WorldTranslation: TVector3;

    function OutsideTranslation: TVector3;
    function OutsideYSortTranslation: TVector3;

    function OutsideRoot: TCastleTransform;

    { Set at once vectors: position, direction, up.

      ADir and AUp given here do not have to be normalized
      (they will be normalized if needed).
      They will be automatically fixed to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value).

      Just like @link(Translation) and @link(Rotation),
      these vectors reflect the transformation defined locally
      in this TCastleTransform, disregarding the parent transformations.
      Use SetWorldView to account for parent transformations. }
    procedure SetView(const APos, ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;
    procedure SetView(const ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;

    { Set position, direction, up. Similar to @link(SetView), but in world coordinates.

      Given ADir, AUp do not have to be normalized,
      we will normalize them internally if necessary.
      But make sure they are non-zero.

      We will automatically fix ADir and AUp to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetWorldView(const APos, ADir, AUp: TVector3;
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
    procedure UpPrefer(const AUp: TVector3);

    { How the direction and up vectors determine transformation.
      See TOrientationType for values documentation.

      The default value of this is:

      @unorderedList(
        @item(For cameras (TCastleCamera):

          DefaultCameraOrientation, which is just constant @link(otUpYDirectionMinusZ).
          This means direction is -Z (DefaultCameraDirection), up is +Y (DefaultCameraUp).
        )

        @item(For all other transformations (not cameras):

          DefaultOrientation, static variable, by default @link(otUpYDirectionZ).
          This means direction is +Z and up is +Y (DefaultCameraUp).
          This matches default orientation of glTF models exported from Blender.

          You can adjust the Orientation property of particular models.
          Or you can adjust DefaultOrientation to set it for all (non-camera) transforms
          throughout your application.
        )
      )

      This value determines how you should model your 3D models,
      like the creatures, the items, and the player weapons. }
    property Orientation: TOrientationType read FOrientation write FOrientation;

    { Transformation objects inside.
      Freeing these items automatically removes them from this list. }
    property List: TCastleTransformList read FList;

    { Add a TCastleBehavior to this TCastleTransform.
      In effect, the virtual methods of TCastleBehavior, like @link(TCastleBehavior.Update),
      will be automatically called.
      Also the @link(TCastleBehavior.Parent) gets assigned.
      If the TCastleBehavior was part of another TCastleTransform, it is removed from it.

      @seealso FindBehavior
      @seealso BehaviorsEnumerate }
    procedure AddBehavior(const Behavior: TCastleBehavior);

    { Remove TCastleBehavior from this TCastleTransform.
      In effect, the virtual methods of TCastleBehavior, like @link(TCastleBehavior.Update),
      will no longer be automatically called.
      The @link(TCastleBehavior.Parent) is set to @nil. }
    procedure RemoveBehavior(const Behavior: TCastleBehavior);

    { Find the first behavior of the given class, @nil if none. }
    function FindBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;

    { Find all behaviors of given class, in this transform and children (up to any depth).
      The behaviors are added to the list, disregaring the Exists or ExistsInRoot property of parents.
      Returned class @italic(does not) own children.
      The caller is responsible for freeing the returned list. }
    function FindAllBehaviors(const BehaviorClass: TCastleBehaviorClass): TCastleBehaviorList;

    { Find the first behavior of the given class, or create and add a new one if necessary.
      Never returns @nil. }
    function FindRequiredBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;

    { Count of behaviors.
      @seealso AddBehavior
      @seealso RemoveBehavior }
    function BehaviorsCount: Integer;

    { Enumerate current behaviors.
      @seealso AddBehavior
      @seealso RemoveBehavior }
    property Behaviors [const Index: Integer]: TCastleBehavior read GetBehaviors;

    { You can enumerate current behaviors using loop like
      @code(for B in MyTransform.BehaviorsEnumerate do ...).
      Do not call this method in other contexts, it is only useful for "for..in" construction. }
    function BehaviorsEnumerate: TCastleBehaviorEnumerator;

    { Adds a behavior to the list that needs to be informed about the world
      change. Adding multiple times the same behavior is harmless and has no effect. }
    procedure AddWorldChangeNotification(const Behavior: TCastleBehavior);

    { Removes a behavior from the list that needs to be informed about the world
      change. }
    procedure RemoveWorldChangeNotification(const Behavior: TCastleBehavior);

    { Does this object, and all it's parents up to the root, exist.
      This has undefined value in case you place this TCastleTransform instance
      (or any parent) many times within the viewport root,
      and some parents exist while some not.

      This property doesn't check whether this TCastleTransform is actually
      part of some @link(World). So it will be @true if this item doesn't have
      any parent. It only checks that "all parents, going up the tree, have @link(Exists)
      true".

      @noAutoLinkHere }
    property ExistsInRoot: Boolean read FExistsInRoot;
  published
    { Is this object visible and colliding.
      Setting this to @false makes this object non-existing,
      just as if removing the object from parent would do.

      Non-existing object implies also that all children are non-existing.
      Behaviors on non-existing objects should also stop working,
      e.g. physics engine should ignore bodies / colliders attached to non-existing
      objects, and (TODO) TCastleSoundsource should not play when attached to non-existing
      object.

      @noAutoLinkHere }
    property Exists: Boolean read FExists write SetExists default true;

    { Should this collide when
      @url(https://castle-engine.io/physics#_old_system_for_collisions_and_gravity simple physics) is used.
      This setting is honored e.g. by current collisions done by FPS walk/fly navigation
      using TCastleWalkNavigation.
      This setting is ignored by the physics engine
      (for physics, control if something collides just by adding / removing a TCastleCollider component).

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

      The only exception are the collisions with TCastleMoving instances
      (movable world parts like elevators and doors) that have their own
      detection routines and look at CollidesWithMoving property of other objects.
      That is, the TCastleMoving instance itself must still have Collides = @true,
      but it interacts with @italic(other) objects if and only if they have
      CollidesWithMoving = @true (ignoring their Collides value).
      This allows items to be moved by elevators, but still player and creatures
      can pass through them.

      Note that if not @link(Exists) then this doesn't matter
      (non-existing objects never participate in collision detection).

      @noAutoLinkHere }
    property Collides: boolean read FCollides write FCollides default true;

    { Is item pickable by @link(RayCollision) method.
      Note that if not @link(Exists) then this doesn't matter
      (non-existing objects are never pickable).

      This affects ray collisions only for some methods that are part of old
      @url(https://castle-engine.io/physics#_old_system_for_collisions_and_gravity simple physics).
      This setting is ignored by the physics engine.

      This is independent from @link(Collides), as @link(RayCollision)
      does not look at @link(Collides), it only looks at @link(Pickable).
      @noAutoLinkHere }
    property Pickable: boolean read FPickable write FPickable default true;

    { Is item visible.
      Note that if not @link(Exists) then this doesn't matter
      (non-existing objects are never visible).

      This is independent from @link(Collides) or @link(Pickable).
      @noAutoLinkHere }
    property Visible: boolean read FVisible write FVisible default true;

    { Exclude from rendering statistics in
      @link(TCastleViewport.Statistics). }
    property ExcludeFromStatistics: boolean
      read FExcludeFromStatistics write FExcludeFromStatistics default false;

    { When non-zero, we can approximate collisions with this object using a sphere
      in certain situations (@link(MoveAllowed), @link(Gravity)).
      This usually makes dynamic objects, like player and creatures, collide better. }
    property CollisionSphereRadius: Single read FCollisionSphereRadius write FCollisionSphereRadius;

    { Does the object cast shadows.

      TODO: Right now this only affects casting shadows by shadow volumes.
      It will be extended to account for shadow maps. }
    property CastShadows: boolean
      read FCastShadows write FCastShadows default true;

    { Objects on front render layer are rendered in front of objects on back render layer,
      regardless of their actual position (depth) relative to camera.

      rlParent means that object follows the parent layer, which is by default back.

      The typical use-case is to force some object to be in front, by setting rlFront. }
    property RenderLayer: TRenderLayer read FRenderLayer write FRenderLayer default rlParent;

    { When rendering, sort all the children,
      making blending correct when there are multiple partially-transparent objects
      in the world.

      This is a simple way to achieve correct blending order when rendering multiple objects.
      You need this when multiple objects (like TCastleScene and TCastleImageTransform)
      use blending (partial transparency) within the world.
      Note that inside each scene (TCastleScene) the partially-transparent
      shapes are also sorted, following TCastleRenderOptions.BlendingSort,
      with each TCastleScene having own TCastleRenderOptions instance.

      TODO: This is a simple approach to have correct blending.
      But it is not the most efficient approach, it is also not the most capable one.
      However, it is really simple to use and likely is enough in many practical cases.

      Future: We plan to have a rendering that gathers all partially-transparent shapes
      from all the objects, and sorts them.
      This will make the sorting process more efficient (it will only sort partially-transparent shapes)
      and more capable (each shape will be ordered).

      Drawbacks of current approach:

      @unorderedList(

        @item(
          The sorting is repeated at every render call.
          This is not optimal in case of 2D, where it would be more efficient
          to only call @link(SortBackToFront2D) when necessary, e.g. when adding/removing
          objects from the world.

          To avoid this overhead, just leave this property at bsNone
          and call @link(SortBackToFront2D) when necessary.
        )

        @item(
          The sorting accounts for all objects (not only partially transparent).
          This is not optimal (but we cannot be more efficient because each scene
          may contain something partially transparent).)

        @item(
          The sorting puts each TCastleScene completely in front or completely behind
          other TCastleScene. This is not good enough when multiple TCastleScene instances
          have multiple partially-transparent shapes and the correct order would mix their
          shapes.)

        @item(
          The sorting is not recursive (to avoid making it really not efficient, in particular
          to recalculate camera for each transform and to sort in each tree TCastleTransform).

          This means it is not capable of dealing with some cases. It assumes that all things
          where the order matters are direct children of TCastleAbstractRootTransform.)
      )
    }
    property BlendingSort: TBlendingSort read FBlendingSort write FBlendingSort default bsNone;

    property YSortOffset: Single read FYSortOffset write FYSortOffset {$IFDEF FPC}default 0{$ENDIF};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTransform.TEnumerator ------------------------------------------------- }

function TCastleTransform.TEnumerator.GetCurrent: TCastleTransform;
begin
  Result := FList[FPosition];
end;

constructor TCastleTransform.TEnumerator.Create(const AList: TCastleTransformList);
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TCastleTransform.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TCastleTransform ---------------------------------------------------------------- }

constructor TCastleTransform.Create(AOwner: TComponent);
begin
  inherited;
  FCastShadows := true;
  FCollides := true;
  FPickable := true;
  FVisible := true;
  FCursor := mcDefault;
  FList := TCastleTransformList.Create(false);
  FList.FOwner := Self; // get notified about add/release, assign children World, Parent
  FBehaviors := TComponentList.Create(false);
  FBehaviorsWorldChangeNotificationList := nil;

  FMiddleHeight := DefaultMiddleHeight;
  FOnlyTranslation := true;
  FScale := NoScale;
  FOrientation := DefaultOrientation;
  MeaningfulParent := true;
  FExists := true;
  { at the beginning FParent = nil, which implies "all parents exist" }
  FParentExistsInRoot := true;
  FExistsInRoot := true; // FParentExistsInRoot and FExists;, UpdateExistsInRoot would also set this

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleTransform.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  FreeAndNil(FPendingToRemove);
  FreeAndNil(FPendingToRemoveAndFree);

  if RegisteredGLContextCloseListener then
  begin
    ApplicationProperties.OnGLContextCloseObject.Remove({$ifdef FPC}@{$endif}GLContextCloseEvent);
    RegisteredGLContextCloseListener := false;
  end;
  GLContextClose;

  FreeAndNil(FList);
  FreeAndNil(FSortedList);
  FreeAndNil(FOutsideList);
  FreeAndNil(FBehaviors);
  FreeAndNil(FBehaviorsWorldChangeNotificationList);
  inherited;
end;

procedure TCastleTransform.BeforeDestruction;
var
  I: Integer;
  PreviousCount: SizeInt;
begin
  { Remove Self from children, so that in each child we

    - decrease FWorldReferences
    - set FParent to nil (if it matched Self)
    - set FWorld to nil (if it matched Self.FWorld)

    Note that this has to be done before "ChangeWorld(nil)",
    as RemoveParent in child compares "World = OldParent.World".
    That is also why we don't depend on TCastleTransformList.Notify
    to call RemoveParent on all children when parent is freed,
    as it would be too late, when our World is already nil. }
  while FList.Count <> 0 do
  begin
    PreviousCount := FList.Count;
    Remove(FList[0]);
    if FList.Count >= PreviousCount then
    begin
      WritelnWarning('During TCastleTransform.BeforeDestruction, the number of children does not decrease -- this means that some TCastleTransform destructor adds new TCastleTransform instances. ' +
        'We break infinite loop by aborting normal destruction, but expect further problems. To avoid this warning, make sure your TCastleTransform destructor does not create new TCastleTransform instances.');
      Break;
    end;
  end;

  { set to nil, to detach free notification }
  ChangeWorld(nil);

  PendingRemovals;

  { Before we start destroying the transform we need to remove all behaviors from
    it, because destroying behavior calls on it events like BeforeParentDetach,
    ParentChanged.

    And we want Parent (this TCastleTransform) state to be valid
    then, e.g. FBehaviors should still be good.

    In case of descendants, like TCastleScene, also their state has to be valid
    in case behavior's implementation accesses them, e.g. doing
    "(Parent as TCastleScene).StopAnimation" from BeforeParentDetach implementation.
    That's why we do this from BeforeDestruction, before e.g. TCastleScene.Destroy starts
    destroying the instance. }

  for I := FBehaviors.Count - 1 downto 0 do
    RemoveBehaviorIndex(I);

  inherited BeforeDestruction;
end;

procedure TCastleTransform.VisibleChangeHere(const Changes: TVisibleChanges);
begin
  if World <> nil then
  begin
    Inc(World.InternalVisibleStateId);
    if vcVisibleGeometry in Changes then
      Inc(World.InternalVisibleGeometryStateId);
    if vcVisibleNonGeometry in Changes then
      Inc(World.InternalVisibleNonGeometryStateId);
  end;
end;

procedure TCastleTransform.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if (World <> nil) and Assigned(World.OnCursorChange) then
      World.OnCursorChange(Self);
  end;
end;

function TCastleTransform.CheckCollides: Boolean;
begin
  {$warnings off} // using deprecated to keep it working
  Result := GetCollides;
  {$warnings on}
end;

function TCastleTransform.GetCollides: boolean;
begin
  Result := FCollides and Exists;
end;

function TCastleTransform.CheckPickable: boolean;
begin
  {$warnings off} // using deprecated to keep it working
  Result := GetPickable;
  {$warnings on}
end;

function TCastleTransform.GetPickable: boolean;
begin
  Result := FPickable and Exists;
end;

function TCastleTransform.CheckVisible: boolean;
begin
  {$warnings off} // using deprecated to keep it working
  Result := GetVisible;
  {$warnings on}
end;

function TCastleTransform.GetVisible: boolean;
begin
  Result := FVisible and Exists;
end;

function TCastleTransform.Sphere(out Radius: Single): boolean;
begin
  Radius := CollisionSphereRadius;
  Result := Radius <> 0;
end;

procedure TCastleTransform.AddToWorld(const Value: TCastleAbstractRootTransform);
var
  I: Integer;
begin
  Assert(Value <> nil);
  if FWorld <> Value then
  begin
    if FWorld <> nil then
      raise ECannotAddToAnotherWorld.Create('Cannot add object existing in one TCastleRootTransform to another. ' + 'This usually means that your object is part of "Viewport1.Items", and you are adding it to "Viewport2.Items". You have to remove it from "Viewport1.Items" first, or set both "Viewport1.Items" and "Viewport2.Items" to be equal.');
    ChangeWorld(Value);
  end else
    Inc(FWorldReferences);

  // list stuff
  if FList <> nil then // when one list is within another, this may be called during own destruction by TCastleTransformList.Notify
    for I := 0 to List.Count - 1 do
      List[I].AddToWorld(Value);
end;

procedure TCastleTransform.RemoveFromWorld(const Value: TCastleAbstractRootTransform);
var
  I: Integer;
begin
  Assert(Value <> nil);
  Assert(FWorldReferences > 0);
  if FWorld <> Value then
    WritelnWarning('TCastleTransform.RemoveFromWorld: Removing from World you were not part of. ' + 'This probably means that you added one TCastleTransform instance to multiple TCastleRootTransform trees, which is not allowed. Always remove TCastleTransform from previous viewport (e.g. by "Viewport1.Items.Remove(xxx)") before adding to the new viewport.');

  Dec(FWorldReferences);
  if FWorldReferences = 0 then
    ChangeWorld(nil);

  // list stuff
  if FList <> nil then // when one list is within another, this may be called during own destruction by TCastleTransformList.Notify
    for I := 0 to List.Count - 1 do
      List[I].RemoveFromWorld(Value);
end;

procedure TCastleTransform.AddParent(const NewParent: TCastleTransform);
begin
  { Assign FParent before calling AddToWorld
    (that may cause PhysicsChangeWorldAttach that may use Parent).
    We check MeaningfulParent, so that adding children to TCastleTransformReference
    will not change parent. }
  if NewParent.MeaningfulParent then
  begin
    FParent := NewParent;
    { Keep our ParentExistsInRoot synchronized with FParent, and thus keep our ExistsInRoot correct }
    ParentExistsInRoot := FParent.ExistsInRoot;
  end;

  if NewParent.World <> nil then
    AddToWorld(NewParent.World);

  FreeNotification(NewParent);
end;

procedure TCastleTransform.RemoveParent(const OldParent: TCastleTransform);
begin
  if (OldParent.World <> nil) and
     { It is possible that "World <> OldParent.World" when Self is getting
       freed and has World set to nil in constructor already. }
     (World = OldParent.World) then
    RemoveFromWorld(OldParent.World);

  { Clear FParent only if previous parent matched OldParent.
    This way removing from TCastleTransformReference will not change FParent. }
  if FParent = OldParent then
  begin
    FParent := nil;
    { non-existing parent means "all parents exist" }
    ParentExistsInRoot := true;
  end;

  { Do not call RemoveFreeNotification when OldParent is equal to World,
    this would prevent getting notification when FWorld is set to nil in
    TCastleTransform.Notification. }
  if World <> OldParent then
    RemoveFreeNotification(OldParent);
end;

procedure TCastleTransform.ChangeWorld(const Value: TCastleAbstractRootTransform);
begin
  if FWorld <> Value then
  begin
    if FWorld <> nil then
    begin
      if ListenPressRelease then
        FWorld.UnregisterPressRelease(Self);

      NotifyWorldBeforeDetach;

      { Do not call RemoveFreeNotification when FWorld is also our list owner,
        this would prevent getting notification in TCastleTransform.Notification. }
      if (FWorld.List = nil) or (FWorld.List.IndexOf(Self) = -1) then
        FWorld.RemoveFreeNotification(Self);
    end;

    FWorld := Value;
    FWorldReferences := Iff(Value <> nil, 1, 0);

    if FWorld <> nil then
    begin
      // Ignore FWorld = Self case, when this is done by TCastleAbstractRootTransform.Create? No need to.
      //if FWorld <> Self then
      FWorld.FreeNotification(Self);

      NotifyWorldAfterAttach;

      if ListenPressRelease then
        FWorld.RegisterPressRelease(Self);
    end;
  end;
end;

procedure TCastleTransform.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { make sure to nil FWorld reference }
  if (Operation = opRemove) and (AComponent = FWorld) then
    ChangeWorld(nil);

  { We have to remove a reference to the object from the List.
    This is crucial: TCastleTransformList.Notify,
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
    including about our own removal. (And in this case, we are a TCastleTransform descendant
    ourselves, just like our children; so check "AComponent is TCastleTransform" doesn't
    protect us.)
    Practical situation when it happens is in testcases
    TTestCastleTransform.TestNotifications and TTestCastleTransform.TestNotificationsSceneManager. }

  if (Operation = opRemove) and (AComponent is TCastleTransform) and (List <> nil) then
    List.RemoveAll(AComponent);
end;

function TCastleTransform.Height(const MyPosition: TVector3;
  out AboveHeight: Single): boolean;
var
  AboveGroundIgnored: PTriangle;
begin
  Result := Height(MyPosition, AboveHeight, AboveGroundIgnored);
end;

function TCastleTransform.Height(const MyPosition: TVector3;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
var
  MyPositionWorld: TVector3;
  SavedExists: Boolean;
begin
  MyPositionWorld := Parent.LocalToWorld(MyPosition);
  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldHeight(MyPositionWorld, AboveHeight, AboveGround);
    if Result then
      AboveHeight := Parent.WorldToLocalDistance(AboveHeight);
  finally Exists := SavedExists end;
end;

function TCastleTransform.LineOfSight(const Pos1, Pos2: TVector3): boolean;
var
  Pos1World, Pos2World: TVector3;
  SavedExists: Boolean;
begin
  Pos1World := Parent.LocalToWorld(Pos1);
  Pos2World := Parent.LocalToWorld(Pos2);
  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldLineOfSight(Pos1World, Pos2World);
  finally Exists := SavedExists end;
end;

function TCastleTransform.MoveAllowed(const OldPos, ProposedNewPos: TVector3;
  out NewPos: TVector3;
  const BecauseOfGravity: boolean): boolean;
var
  Sp: boolean;
  SpRadius, SpRadiusWorld: Single;
  OldBox, NewBox: TBox3D;
  OldPosWorld, ProposedNewPosWorld, NewPosWorld: TVector3;
  SavedExists: Boolean;
begin
  Assert(Parent <> nil, 'Need to know world transformation before MoveAllowed');

  OldPosWorld := Parent.LocalToWorld(OldPos);
  ProposedNewPosWorld := Parent.LocalToWorld(ProposedNewPos);

  { Save bounding volume information before calling Disable, as Disable makes
    bounding volume empty }
  Sp := Sphere(SpRadius);
  if not Sp then
    SpRadius := 0; { something predictable, for safety }
  OldBox := WorldBoundingBox;
  NewBox := OldBox.Translate(ProposedNewPosWorld - OldPosWorld);

  SpRadiusWorld := Parent.LocalToWorldDistance(SpRadius);

  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldMoveAllowed(OldPosWorld, ProposedNewPosWorld, NewPosWorld,
      Sp, SpRadiusWorld, OldBox, NewBox, BecauseOfGravity);
  finally Exists := SavedExists end;

  if Result then
    NewPos := Parent.WorldToLocal(NewPosWorld);
end;

function TCastleTransform.MoveAllowed(const OldPos, NewPos: TVector3;
  const BecauseOfGravity: boolean): boolean;
var
  Sp: boolean;
  SpRadius: Single;
  OldBox, NewBox: TBox3D;
  SavedExists: Boolean;
begin
  { save bounding volume information before calling Disable, as Disable makes
    bounding volume empty }
  Sp := Sphere(SpRadius);
  if not Sp then
    SpRadius := 0; { something predictable, for safety }
  OldBox := WorldBoundingBox;
  NewBox := OldBox.Translate(NewPos - OldPos);

  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldMoveAllowed(OldPos, NewPos,
      Sp, SpRadius, OldBox, NewBox, BecauseOfGravity);
  finally Exists := SavedExists end;
end;

function TCastleTransform.Ray(
  const RayOrigin, RayDirection: TVector3): TRayCollision;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
  SavedExists: Boolean;
begin
  RayOriginWorld := Parent.LocalToWorld(RayOrigin);
  RayDirectionWorld := Parent.LocalToWorldDirection(RayDirection);
  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldRay(RayOriginWorld, RayDirectionWorld);
  finally Exists := SavedExists end;
end;

function TCastleTransform.RayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
  SavedExists: Boolean;
begin
  RayOriginWorld := Parent.LocalToWorld(RayOrigin);
  RayDirectionWorld := Parent.LocalToWorldDirection(RayDirection);
  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldRayCast(RayOriginWorld, RayDirectionWorld);
  finally Exists := SavedExists end;
end;

function TCastleTransform.RayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
  SavedExists: Boolean;
begin
  RayOriginWorld := Parent.LocalToWorld(RayOrigin);
  RayDirectionWorld := Parent.LocalToWorldDirection(RayDirection);
  SavedExists := Exists;
  Exists := false;
  try
    Result := World.WorldRayCast(RayOriginWorld, RayDirectionWorld, Distance);
    if Result <> nil then
      Distance := Parent.WorldToLocalDistance(Distance);
  finally Exists := SavedExists end;
end;

{ list stuff ---------------------------------------------------------------- }

procedure TCastleTransform.Add(const Item: TCastleTransform);
begin
  List.Add(Item);
end;

procedure TCastleTransform.Insert(const Index: Integer; const Item: TCastleTransform);
begin
  List.Insert(Index, Item);
end;

function TCastleTransform.GetItem(const I: Integer): TCastleTransform;
begin
  Result := List[I];
end;

procedure TCastleTransform.SetItem(const I: Integer; const Item: TCastleTransform);
begin
  List[I] := Item;
end;

function TCastleTransform.Count: Integer;
begin
  { Free notifications may want to iterate over some children from a TCastleTransform
    that is being freed, and has List = nil now.
    This happens for lights in designs, when we remove the light or close the design.
    Behave as if we don't have children then. }
  if List <> nil then
    Result := List.Count
  else
    Result := 0;
end;

procedure TCastleTransform.Remove(const Item: TCastleTransform);
begin
  { Free notifications of various components (like TVisualizeTransform)
    may want to remove some children from a TCastleTransform that is being
    freed, and has List = nil now.
    Ignore it -- removing from a nil list doesn't require doing anything. }
  if List <> nil then
    List.Remove(Item);
end;

procedure TCastleTransform.Delete(const Index: Integer);
begin
  { As in Remove, check "List <> nil" }
  if List <> nil then
    List.Delete(Index);
end;

procedure TCastleTransform.Clear;
begin
  List.Clear;
end;

procedure TCastleTransform.Exchange(const Index1, Index2: Integer);
begin
  List.Exchange(Index1, Index2);
end;

procedure TCastleTransform.SortBackToFront(const BlendingSort: TBlendingSort;
  const CameraPosition: TVector3);
begin
  List.SortBackToFront(BlendingSort, CameraPosition);
end;

procedure TCastleTransform.SortBackToFront2D;
begin
  SortBackToFront(bs2D, TVector3.Zero);
end;

function TCastleTransform.LocalBoundingBox: TBox3D;
var
  I: Integer;
begin
  Result := TBox3D.Empty;
  if Exists then
  begin
    for I := 0 to List.Count - 1 do
      { Ignoring InternalExcludeFromParentBoundingVolume when
        InternalGizmoBoundingBox <> 0 is a hack,
        so that when TCastleViewport.ItemsWithGizmosBoundingBox is called,
        then TGizmoScene box is taken into account.
        Otherwise 2D projection in editor could cut off the TGizmoScene.

        Testcase:
        - 2d viewport
        - add a few TCastleImageTransform with some image loaded with Z = -1000.
        - add TCastleTransform with translation 0,0,0, select it
        - -> the gizmo of it has to be visible, even though everything visible
          (excluding gizmos) is at Z = -1000.
      }
      if not (List[I].InternalExcludeFromParentBoundingVolume and
              (InternalGizmoBoundingBox = 0)) then
        Result.Include(List[I].BoundingBox);
  end;
end;

function TCastleTransform.GetRenderList(const Params: TRenderParams): TCastleTransformList;

  procedure GetRecursionChildRenderList(const AParent:TCastleTransform; const AList: TCastleTransformList);
  var
    T: TCastleTransform;
    P: Pointer;
  begin
    for P in AParent.List do
    begin
      T := TCastleTransform(P);
      if T.BlendingSort <> bsYSort then Continue;

      AList.AddList(T.List);
      GetRecursionChildRenderList(T, AList);
    end;
  end;

begin
  if BlendingSort <> bsNone then
  begin
    if FSortedList = nil then // create on-demand
      FSortedList := TCastleTransformList.Create(false);

    if (BlendingSort = bsYSort) and Assigned(FParent) and (FParent.BlendingSort = bsYSort) then
    begin
      FSortedList.Clear;
    end else
    begin
      FSortedList.Assign(List);
      if BlendingSort = bsYSort then
        GetRecursionChildRenderList(Self, FSortedList);
      FSortedList.SortBackToFront(BlendingSort, Params.RenderingCamera.Position);
    end;
    Result := FSortedList;
  end else
    Result := List;
end;

procedure TCastleTransform.UpdateOutsideList;
var
  Tf: TCastleTransform;
begin
  if not Assigned(FOutsideList) then
  begin
    FOutsideList := TCastleTransformList.Create(False);
    FOutsideList.Capacity := 10;
  end;

  FOutsideList.Clear;
  if not RenderOutside then Exit;

  Tf := Self.Parent.Parent;
  FOutsideList.Add(Self.Parent);
  FOutsideList.Add(Self.Parent.Parent);
  while (Tf <> World) and Assigned(Tf) and (Tf.BlendingSort = bsYSort)  do
  begin
    if Assigned(Tf.Parent) and (Tf.Parent.BlendingSort = bsYSort) then
    begin
      Tf := Tf.Parent;
      FOutsideList.Add(Tf);
    end
    else Break;
  end;
end;

procedure TCastleTransform.LocalRender(const Params: TRenderParams);
var
  I: Integer;
  RenderList: TCastleTransformList;
begin
  if CheckVisible then
  begin
    PendingRemovals;
    RenderList := GetRenderList(Params);
    for I := 0 to RenderList.Count - 1 do
      RenderList[I].Render(Params);
  end;
end;

procedure TCastleTransform.LocalRenderShadowVolume(const Params: TRenderParams;
  const ShadowVolumeRenderer: TBaseShadowVolumeRenderer);
var
  I: Integer;
begin
  if Exists and CastShadows then
  begin
    for I := 0 to List.Count - 1 do
      List[I].RenderShadowVolume(Params, ShadowVolumeRenderer);
  end;
end;

procedure TCastleTransform.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

procedure TCastleTransform.RegisterGLContextClose;
begin
  if not RegisteredGLContextCloseListener then
  begin
    RegisteredGLContextCloseListener := true;
    ApplicationProperties.OnGLContextCloseObject.Add({$ifdef FPC}@{$endif}GLContextCloseEvent);
  end;
end;

procedure TCastleTransform.PrepareResources(const Options: TPrepareResourcesOptions;
  const Ignored: Boolean;
  const Params: TPrepareParams);
begin
  PrepareResources(Options, Params);
end;

procedure TCastleTransform.PrepareResources(const Options: TPrepareResourcesOptions;
  const Params: TPrepareParams);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].PrepareResources(Options, Params);
  RegisterGLContextClose;
end;

function TCastleTransform.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := false;
  if not Exists then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].Press(Event) then Exit(true);
end;

function TCastleTransform.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := false;
  if not Exists then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].Release(Event) then Exit(true);
end;

function TCastleTransform.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  { This event is not automatically passed to all children on List,
    instead the TCastleViewport has special logic which
    TCastleTransform instances receive the PointingDevicePress call. }
  Result := false;
end;

function TCastleTransform.PointingDeviceRelease(const Pick: TRayCollisionNode;
  const Distance: Single; const CancelAction: Boolean): Boolean;
begin
  { This event is not automatically passed to all children on List,
    instead the TCastleViewport has special logic which
    TCastleTransform instances receive the PointingDeviceRelease call. }
  Result := false;
end;

function TCastleTransform.PointingDeviceMove(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  { This event is not automatically passed to all children on List,
    instead the TCastleViewport has special logic which
    TCastleTransform instances receive the PointingDeviceMove call. }
  Result := false;
end;

procedure TCastleTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure UpdateBehaviors;
  var
    I: Integer;
    Beh: TCastleBehavior;
    RemoveItem: TRemoveType;
  begin
    I := 0;
    while I < FBehaviors.Count do
    begin
      Beh := FBehaviors[I] as TCastleBehavior;
      RemoveItem := rtNone;
      Beh.Update(SecondsPassed, RemoveItem);
      if RemoveItem in [rtRemove, rtRemoveAndFree] then
      begin
        RemoveBehaviorIndex(I);
        if RemoveItem = rtRemoveAndFree then
          FreeAndNil(Beh);
      end else
        Inc(I);
    end;
  end;

  procedure UpdateChildren;
  var
    I: Integer;
    Item: TCastleTransform;
    RemoveItem: TRemoveType;
  begin
    PendingRemovals;

    I := 0;
    while I < List.Count do
    begin
      Item := List[I];
      if Item.Exists then
      begin
        RemoveItem := rtNone;
        Item.Update(SecondsPassed, RemoveItem);
        if RemoveItem in [rtRemove, rtRemoveAndFree] then
        begin
          List.Delete(I);
          if RemoveItem = rtRemoveAndFree then
            FreeAndNil(Item);
        end else
          Inc(I);
      end else
        Inc(I);
    end;

    PendingRemovals;
  end;

begin
  Assert(Exists); // this should never be called on TCastleTransform that does not exist

  UpdateBehaviors;
  UpdateChildren;

  { Disable physics and gravity in design mode (in the future we may add optional way to enable them) }
  if not CastleDesignMode then
  begin
    UpdateSimpleGravity(SecondsPassed);
  end;
end;

procedure TCastleTransform.GLContextClose;
begin
  { Does nothing now.
    In particular, this does not call GLContextClose on children.
    This way it keeps their OpenGL resources prepared,
    to be able to quickly readd them to another transform (without time-consuming
    initial PrepareResources). }
end;
(*
var
  I: Integer;
begin
  { this is called from inherited destructor, so check <> nil carefully }
  if FList <> nil then
  begin
    for I := 0 to List.Count - 1 do
      List[I].GLContextClose;
  end;
end;
*)

{ transform stuff -------------------------------------------------------- }

function TCastleTransform.Translation2D: TVector2;
begin
  Result := Translation.XY;
end;

function TCastleTransform.Transform: TMatrix4;
begin
  if not FTransformationValid then
  begin
    InternalTransformation(FTransformation);
    FTransformationValid := true;
  end;
  Result := FTransformation.Transform;
end;

function TCastleTransform.OutsideTransform: TMatrix4;
var
  AOutsideRoot: TCastleTransform;
begin
  AOutsideRoot := OutsideRoot;
  if Assigned(AOutsideRoot) then
    Result := AOutsideRoot.WorldInverseTransform * WorldTransform
  else
    Result := Transform;
end;

function TCastleTransform.InverseTransform: TMatrix4;
begin
  if not FTransformationValid then
  begin
    InternalTransformation(FTransformation);
    FTransformationValid := true;
  end;
  Result := FTransformation.InverseTransform;
end;

function TCastleTransform.InverseOutsideTransform: TMatrix4;
var
  AOutsideRoot: TCastleTransform;
begin
  AOutsideRoot := OutsideRoot;
  if Assigned(AOutsideRoot) then
    Result := WorldInverseTransform * AOutsideRoot.WorldTransform
  else
    Result := InverseTransform;
end;

function TCastleTransform.WorldTransform: TMatrix4;
begin
  UpdateWorldTransformation;
  Result := FWorldTransformation.Transform;
end;

function TCastleTransform.WorldInverseTransform: TMatrix4;
begin
  UpdateWorldTransformation;
  Result := FWorldTransformation.InverseTransform;
end;

function TCastleTransform.CheckParent: TCastleTransform;
begin
  { We no longer check here FWorldReferences = 1.
    As FWorldReferences > 1 even when we're child of 1 normal transform
    and multiple TCastleTransformReference.

    Also, this makes Parent a simple property that just returns FParent
    without checking for nil.
    Otherwise, Parent would have to be a function that returns FParent or nil,
    depending on FWorldReferences, making things more complicated. }

  if FParent = nil then
  begin
    if Self = World then
      raise ETransformParentUndefined.CreateFmt('Parent not available: This is the root node (%s %s).', [
        Name,
        ClassName
      ])
    else
      raise ETransformParentUndefined.CreateFmt('Parent (and WorldTransform) not available for %s %s.' + NL +
        NL +
        'This instance is not part of TCastleRootTransform (Viewport.Items).' + NL +
        'Or it was (at some point in time) added multiple times to TCastleRootTransform (Viewport.Items). References through TCastleTransformReference are not counted here.' + NL +
        NL +
        'To have a Parent (and WorldTransform), add it exactly once to TCastleRootTransform (Viewport.Items). You can still use TCastleTransformReference to have more instances.',
      [
        Name,
        ClassName
      ]);
  end;
  Result := FParent;
end;

procedure TCastleTransform.NotifyWorldAfterAttach;
var
  I: Integer;
begin
  if FBehaviorsWorldChangeNotificationList = nil then
    Exit;

  for I := 0 to FBehaviorsWorldChangeNotificationList.Count - 1 do
  begin
    FBehaviorsWorldChangeNotificationList[I].WorldAfterAttach;
  end;
end;

procedure TCastleTransform.NotifyWorldBeforeDetach;
var
  I: Integer;
begin
  if FBehaviorsWorldChangeNotificationList = nil then
    Exit;

  for I := 0 to FBehaviorsWorldChangeNotificationList.Count - 1 do
  begin
    FBehaviorsWorldChangeNotificationList[I].WorldBeforeDetach;
  end;
end;

procedure TCastleTransform.AddNotificationOnBehaviorAdded(const AEvent: TComponentEvent);
begin
  AddNotificationOnBehaviorAdded(AEvent, TCastleComponent);
end;

procedure TCastleTransform.AddNotificationOnBehaviorAdded(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
begin
  if FOnAddedBehaviorNotification = nil then
    FOnAddedBehaviorNotification := TCastleComponentNotification.Create(Self);

  FOnAddedBehaviorNotification.AddNotification(AEvent, ClassFilter);
end;

procedure TCastleTransform.NotifyBehaviorAdded(const ABehavior: TCastleBehavior);
begin
  if FOnAddedBehaviorNotification = nil then
    Exit;

  FOnAddedBehaviorNotification.Notify(ABehavior);
end;

procedure TCastleTransform.RemoveNotificationOnBehaviorAdded(
  const AEvent: TComponentEvent);
begin
  if FOnAddedBehaviorNotification = nil then
    Exit;

  FOnAddedBehaviorNotification.RemoveNotification(AEvent);
end;

procedure TCastleTransform.RemoveNotificationOnBehaviorAdded(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
begin
  if FOnAddedBehaviorNotification = nil then
    Exit;

  FOnAddedBehaviorNotification.RemoveNotification(AEvent, ClassFilter);
end;

procedure TCastleTransform.AddNotificationBeforeBehaviorRemove(
  const AEvent: TComponentEvent);
begin
  AddNotificationBeforeBehaviorRemove(AEvent, TCastleComponent);
end;

procedure TCastleTransform.AddNotificationBeforeBehaviorRemove(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
begin
  if FBeforeRemoveBehaviorNotification = nil then
    FBeforeRemoveBehaviorNotification := TCastleComponentNotification.Create(Self);

  FBeforeRemoveBehaviorNotification.AddNotification(AEvent, ClassFilter);
end;

procedure TCastleTransform.NotifyBeforeBehaviorRemove(
  const ABehavior: TCastleBehavior);
begin
  if FBeforeRemoveBehaviorNotification = nil then
    Exit;

  FBeforeRemoveBehaviorNotification.Notify(ABehavior);
end;

procedure TCastleTransform.RemoveNotificationBeforeBehaviorRemove(
  const AEvent: TComponentEvent);
begin
  if FBeforeRemoveBehaviorNotification = nil then
    Exit;

  FBeforeRemoveBehaviorNotification.RemoveNotification(AEvent);
end;

procedure TCastleTransform.RemoveNotificationBeforeBehaviorRemove(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
begin
  if FBeforeRemoveBehaviorNotification = nil then
    Exit;

  FBeforeRemoveBehaviorNotification.RemoveNotification(AEvent, ClassFilter);
end;

procedure TCastleTransform.BeforeBehaviorDestroy(const Behavior: TCastleBehavior);
begin
  if Behavior.Parent <> nil then
  begin
    NotifyBeforeBehaviorRemove(Behavior);

    if Behavior.ListenWorldChange and (FWorld <> nil) then
      Behavior.WorldBeforeDetach;

    Behavior.ParentBeforeDetach;

    Behavior.FParent := nil;

    { Could be called but a lot of exsiting code doesn't check Parent = nil.
      For backward compatibility of this deprecated method, don't call it. }
    // Behavior.ParentChanged;

    RemoveWorldChangeNotification(Behavior);
  end;
end;

procedure TCastleTransform.UpdateCollidersWorldScale;
var
  Collider: TCastleCollider;
  T: TCastleTransform;
begin
  Collider := FindBehavior(TCastleCollider) as TCastleCollider;
  if Collider <> nil then
    Collider.UpdateWorldScale;

  for T in Self do
    T.UpdateCollidersWorldScale;
end;

function TCastleTransform.HasWorldTransform: boolean;
begin
  { Check for "FWorld <> nil" is still necessary, otherwise a TCastleTransform
    could satisfy this without having World assigned:
    when it has a FParent, but FParent is not yet part of any World.

    This matters, e.g. we activate FScene.ProcessEvents:=true
    in TCastleAbstractLight.Create, at which point FScene has a parent
    but the parent (TCastleAbstractLight instance) is not yet part of world.
    SetProcessEvents calls GetCameraVectors that checks HasWorldTransform,
    assuming that HasWorldTransform => implies World <> nil.

    TODO: maybe just "FWorld <> nil" should be enough? }
  Result := (FWorld <> nil) and ((Self = World) or (FParent <> nil));
end;

procedure TCastleTransform.UpdateWorldTransformation;
var
  Par: TCastleTransform;
  IsWorld: boolean;
begin
  { The main feature that enables to optimize this (usually reuse previously
    calculated FWorldTransformation) is that we keep
    FWorldTransformationId.
    This id changes every time the FWorldTransform or FWorldInverseTransform changes.
    So a child can save this id, and know that as long as FWorldTransformationId
    didn't change, so also FWorldTransformation didn't change.

    The FWorldTransformationId is never zero after UpdateWorldTransformation.
    So you can safely use FLastParentWorldTransformationId = 0
    as a value that "will never be considered equal". }

  IsWorld := Self = World;

  if not IsWorld then
  begin
    // first, update FLastParentWorldTransform*
    Par := CheckParent;
    Par.UpdateWorldTransformation;
    if FLastParentWorldTransformationId <> Par.FWorldTransformationId then
    begin
      FLastParentWorldTransformationId := Par.FWorldTransformationId;
      FLastParentWorldTransformation   := Par.FWorldTransformation;
      // need to recalculate our FWorldTransformation
      FWorldTransformationValid := false;
    end;
  end;

  { Note that FWorldTransformationValid is set to false
    - when Par.WorldTransform changes or
    - when our Transform changes
    Both situations imply that our WorldTransform should change.
  }

  if not FWorldTransformationValid then
  begin
    // update NextTransformId
    if NextTransformId = High(NextTransformId) then
    begin
      WritelnLog('Watch out, UpdateWorldTransformation overflows the NextTransformId');
      NextTransformId := 1; // skip over 0
    end else
      Inc(NextTransformId);

    FWorldTransformationId := NextTransformId;

    // actually calculate World[Inverse]Transform here
    if IsWorld then
    begin
      FWorldTransformation.Transform := Transform;
      FWorldTransformation.InverseTransform := InverseTransform;
    end else
    begin
      FWorldTransformation.Transform := FLastParentWorldTransformation.Transform * Transform;
      FWorldTransformation.InverseTransform := InverseTransform * FLastParentWorldTransformation.InverseTransform;
    end;
    FWorldTransformationValid := true;
  end;
end;

procedure TCastleTransform.TransformMatricesMult(var M, MInverse: TMatrix4);
var
  T: TTransformation;
begin
  // call InternalTransformationMult (non-deprecated)
  T.Transform := M;
  T.InverseTransform := MInverse;
  // T.Scale := 1; // doesn't matter
  InternalTransformationMult(T);
  M := T.Transform;
  MInverse := T.InverseTransform;
end;

procedure TCastleTransform.TransformMatrices(out M, MInverse: TMatrix4);
var
  T: TTransformation;
begin
  // call InternalTransformation (non-deprecated)
  InternalTransformation(T);
  M := T.Transform;
  MInverse := T.InverseTransform;
end;

procedure TCastleTransform.InternalTransformationMult(var T: TTransformation);

{$ifdef COMPILER_BUGGY_PARAMETERS}
  type
    TTransformData = record
      Transform, InverseTransform: TMatrix4;
      Center: TVector3;
      Rotation: TVector4;
      Scale: TVector3;
      ScaleOrientation: TVector4;
      Translation: TVector3;
    end;

  procedure MultiplyWorkaround(var T: TTransformation; const TransformData: TTransformData);
  begin
    T.Multiply(
      TransformData.Center,
      TransformData.Rotation,
      TransformData.Scale,
      TransformData.ScaleOrientation,
      TransformData.Translation);
  end;

var
  TransformData: TTransformData;
begin
  TransformData.Center := FCenter;
  TransformData.Rotation := FRotation;
  TransformData.Scale := FScale;
  TransformData.ScaleOrientation := FScaleOrientation;
  TransformData.Translation := FTranslation;
  MultiplyWorkaround(T, TransformData);
{$else}
begin
  T.Multiply(
    FCenter,
    FRotation,
    FScale,
    FScaleOrientation,
    FTranslation);
{$endif}
end;

procedure TCastleTransform.InternalOutsideTransformationMult(
  var T: TTransformation);
var
  i: Integer;
begin
  UpdateOutsideList;
  if (FOutsideList.Count < 2) then
    Raise Exception.Create('InternalOutsideTransformationMult: No need to call this.');

  { The last item in FOutsideList is OutsideRoot, so exclude it.}
  for i := FOutsideList.Count - 2 downto 0 do FOutsideList[i].InternalTransformationMult(T);
  InternalTransformationMult(T);
end;

procedure TCastleTransform.InternalTransformation(out T: TTransformation);
begin
  T.Init;
  InternalTransformationMult(T);
end;

procedure TCastleTransform.InternalOutsideTransformation(out T: TTransformation);
begin
  T.Init;
  InternalOutsideTransformationMult(T);
end;

function TCastleTransform.AverageScale: Single;
begin
  Result := Approximate3DScale(Scale);
end;

function TCastleTransform.AverageScale2D: Single;
begin
  Result := Approximate2DScale(Scale.XY);
end;

{ We assume in all methods below that FOnlyTranslation is the most common case,
  and then that Translation = 0,0,0 is the most common case.
  This is true for many 3D objects. And for only translation,
  we can calculate result much faster (and for translation = zero,
  we don't have to do anything besides calling inherited).

  For some simplest operations, we do not check for Translation = 0,0,0
  case --- if applying Translation is very fast, then checking for
  zero translation would be a waste of time. }

function TCastleTransform.BoundingBox: TBox3D;
begin
  if FOnlyTranslation then
    Result := LocalBoundingBox.Translate(Translation)
  else
    Result := LocalBoundingBox.Transform(Transform);
end;

function TCastleTransform.WorldBoundingBox: TBox3D;
begin
  Result := LocalBoundingBox.Transform(WorldTransform);
end;

procedure TCastleTransform.Render(const Frustum: TFrustum; const Params: TRenderParams);
var
  OldFrustum: PFrustum;
begin
  OldFrustum := Params.Frustum;
  Params.Frustum := @Frustum;
  try
    Render(Params);
  finally Params.Frustum := OldFrustum end;
end;

procedure TCastleTransform.WarningMatrixNan(const NewParamsInverseTransformValue: TMatrix4);
begin
  WritelnWarning('Transform', Format(
    'Inverse transform of "%s" (%s) matrix has NaN value inside:' + NL +
    '%s' + NL +
    '  Matrix source: Center %s, Rotation %s, Scale %s, ScaleOrientation %s, Translation %s', [
    Name,
    ClassName,
    NewParamsInverseTransformValue.ToString('  '),
    FCenter.ToString,
    FRotation.ToString,
    FScale.ToString,
    FScaleOrientation.ToString,
    FTranslation.ToString
  ]));
end;

procedure TCastleTransform.Render(const Params: TRenderParams);
var
  T: TVector3;
  OldParamsTransform, OldParamsInverseTransform: PMatrix4;
  NewParamsTransformation: TTransformation;
  OldParamsTransformIdentity, OldInMeaningfulParent: Boolean;
  OldFrustum: PFrustum;
  NewFrustumValue: TFrustum;
  HasSavedDepthRange, HasSavedDepthFunc: Boolean;
  SavedDepthRange: TDepthRange;
  SavedDepthFunc: TDepthFunction;
  ARenderOutside: Boolean;
begin
  HasSavedDepthFunc := BlendingSort = bsYSort;
  if HasSavedDepthFunc then
  begin
    SavedDepthFunc := RenderContext.DepthFunc;
    RenderContext.DepthFunc := dfAlways;
  end;

  OldInMeaningfulParent := Params.InMeaningfulParent;
  Params.InMeaningfulParent := Params.InMeaningfulParent and
    { We look at Parent.MeaningfulParent, not Self.MeaningfulParent,
      because CGE editor visualizations on TCastleTransformReference.Reference
      should still be rendered. }
    //((Parent = nil) or Parent.MeaningfulParent);
    MeaningfulParent;

  HasSavedDepthRange := (RenderLayer <> rlParent) and
    { Shadow maps require normal DepthRange,
      don't mess with RenderContext.DepthRange in case when we render shadow maps. }
    (not (Params.RenderingCamera.Target in [rtShadowMap, rtVarianceShadowMap]));
  if HasSavedDepthRange then
  begin
    { The idea of using DepthRange for render layers is nice:
      - you don't have to split rendering layers in passes,
        you can render all objects in one pass,
        just switching DepthRange as necessary.
      - you can set DepthRange for 3D objects inside TCastleTransform,
        like here TCastleTransform will just affect every child underneath.
    }
    SavedDepthRange := RenderContext.DepthRange;
    case RenderLayer of
      rlFront: RenderContext.DepthRange := drNear;
      rlBack : RenderContext.DepthRange := drFar;
      else ;
    end;
  end else
    SavedDepthRange := Low(SavedDepthRange); // avoid Delphi warnings about SavedDepthRange uninitialized

  if (not InternalExistsOnlyInMeaningfulParents) or Params.InMeaningfulParent then
  begin
    { TODO: It is not fully dealt with here. }
    T := Translation;
    ARenderOutside := RenderOutside;
    if not ARenderOutside and FOnlyTranslation and T.IsZero and (not InternalIgnoreParentScale) then
    begin
      LocalRender(Params);
    end else
    begin
      //FrameProfiler.Start(fmRenderTransform);

      OldParamsTransformIdentity := Params.TransformIdentity;
      OldParamsTransform         := Params.Transform;
      OldParamsInverseTransform  := Params.InverseTransform;
      OldFrustum                 := Params.Frustum;
      Assert(OldFrustum <> nil);

      NewParamsTransformation.Transform        := OldParamsTransform^;
      NewParamsTransformation.InverseTransform := OldParamsInverseTransform^;
      NewFrustumValue                          := OldFrustum^;

      Params.TransformIdentity := false;
      Params.Transform        := @NewParamsTransformation.Transform;
      Params.InverseTransform := @NewParamsTransformation.InverseTransform;
      Params.Frustum          := @NewFrustumValue;

      if InternalIgnoreParentScale then
        NewParamsTransformation.ScaleToIdentity;

      { Update NewXxx to apply the transformation defined by this TCastleTransform.
        LocalRender expects Frustum in local coordinates (without transformation),
        so we subtract transformation below. }
      if ARenderOutside then
      begin
        InternalOutsideTransformationMult(NewParamsTransformation);
        if IsNan(NewParamsTransformation.Transform.Data[0, 0]) then
          WarningMatrixNan(NewParamsTransformation.Transform);
        NewFrustumValue := NewFrustumValue.TransformByInverse(OutsideTransform);
      end else if FOnlyTranslation then
      begin
        NewParamsTransformation.Translate(T);
        NewFrustumValue.MoveVar(-T);
      end else
      begin
        InternalTransformationMult(NewParamsTransformation);
        if IsNan(NewParamsTransformation.Transform.Data[0, 0]) then
          WarningMatrixNan(NewParamsTransformation.Transform);
        NewFrustumValue := NewFrustumValue.TransformByInverse(Transform);
        // 2x slower: NewFrustumValue := NewFrustumValue.Transform(InverseTransform);
      end;

      //FrameProfiler.Stop(fmRenderTransform);
      LocalRender(Params);
      //FrameProfiler.Start(fmRenderTransform);

      { Restore OldXxx values.
        They can be restored fast, thanks to using pointers to matrix/frustum. }
      Params.TransformIdentity := OldParamsTransformIdentity;
      Params.Transform         := OldParamsTransform;
      Params.InverseTransform  := OldParamsInverseTransform;
      Params.Frustum           := OldFrustum;
      //FrameProfiler.Stop(fmRenderTransform);
    end;
  end;

  Params.InMeaningfulParent := OldInMeaningfulParent;

  if HasSavedDepthRange then
    RenderContext.DepthRange := SavedDepthRange;
  if HasSavedDepthFunc then
    RenderContext.DepthFunc := SavedDepthFunc;
end;

procedure TCastleTransform.RenderShadowVolume(const Params: TRenderParams;
  const ShadowVolumeRenderer: TBaseShadowVolumeRenderer);
var
  T: TVector3;
  OldParamsTransform, OldParamsInverseTransform: PMatrix4;
  NewParamsTransformation: TTransformation;
  OldParamsTransformIdentity: Boolean;
  OldFrustum: PFrustum;
  NewFrustumValue: TFrustum;
begin
  T := Translation;
  if FOnlyTranslation and T.IsZero then
  begin
    LocalRenderShadowVolume(Params, ShadowVolumeRenderer);
  end else
  begin
    { TODO: This is a copy-paste of transformation algorithm from TCastleTransform.Render. }

    OldParamsTransformIdentity := Params.TransformIdentity;
    OldParamsTransform         := Params.Transform;
    OldParamsInverseTransform  := Params.InverseTransform;
    OldFrustum                 := Params.Frustum;
    Assert(OldFrustum <> nil);

    NewParamsTransformation.Transform        := OldParamsTransform^;
    NewParamsTransformation.InverseTransform := OldParamsInverseTransform^;
    NewFrustumValue                          := OldFrustum^;

    Params.TransformIdentity := false;
    Params.Transform        := @NewParamsTransformation.Transform;
    Params.InverseTransform := @NewParamsTransformation.InverseTransform;
    Params.Frustum          := @NewFrustumValue;

    { Update NewXxx to apply the transformation defined by this TCastleTransform.
      LocalRender expects Frustum in local coordinates (without transformation),
      so we subtract transformation below. }
    if FOnlyTranslation then
    begin
      NewParamsTransformation.Translate(T);
      NewFrustumValue.MoveVar(-T);
    end else
    begin
      InternalTransformationMult(NewParamsTransformation);
      if IsNan(NewParamsTransformation.Transform.Data[0, 0]) then
        WarningMatrixNan(NewParamsTransformation.Transform);
      NewFrustumValue := NewFrustumValue.TransformByInverse(Transform);
      // 2x slower: NewFrustumValue := NewFrustumValue.Transform(InverseTransform);
    end;

    LocalRenderShadowVolume(Params, ShadowVolumeRenderer);

    { Restore OldXxx values.
      They can be restored fast, thanks to using pointers to matrix/frustum. }
    Params.TransformIdentity := OldParamsTransformIdentity;
    Params.Transform         := OldParamsTransform;
    Params.InverseTransform  := OldParamsInverseTransform;
    Params.Frustum           := OldFrustum;
  end;
end;

function Bottom(const Gravity: boolean; const GravityCoordinate: Integer;
  const BoundingBox: TBox3D): Single;
begin
  if Gravity then
    Result := 0 else
    Result := BoundingBox.Data[0][GravityCoordinate];
end;

function TCastleTransform.Middle: TVector3;
var
  GC: Integer;
  B: TBox3D;
begin
  GC := World.GravityCoordinate;
  if InternalMiddleForceBox then
    B := InternalMiddleForceBoxValue
  else
    B := LocalBoundingBox;

  { More correct version would be to take B bottom point, add PreferredHeight,
    and transform this point just like TCastleTransform transforms everything
    else. Optimized implementation below assumes that instead
    of transforming we can add Translation, so we assume that
    transformations do not change this middle point
    (which is Ok if you think e.g. about a non-flying creature that,
    besides moving, only rotates around it's own up axis). }

  Result := Translation;
  { Using deprecated in deprecated.
    This should be simpler at some point:
      Result.InternalData[GC] := Lerp(MiddleHeight, B.Data[0][GC], B.Data[1][GC]);
  }
  {$warnings off}
  Result.InternalData[GC] := Result.InternalData[GC] + (Bottom(Gravity, GC, B) + PreferredHeight);
  {$warnings on}
end;

function TCastleTransform.PreferredHeight: Single;
var
  GC: Integer;
  B: TBox3D;
  { $define CHECK_HEIGHT_VS_RADIUS}
  {$ifdef CHECK_HEIGHT_VS_RADIUS} R: Single; {$endif}
begin
  GC := World.GravityCoordinate;
  if InternalMiddleForceBox then
    B := InternalMiddleForceBoxValue
  else
    B := LocalBoundingBox;
  if B.IsEmpty then
    Result := 0
  else
    {$warnings off} // using deprecated in deprecated
    Result := MiddleHeight * (B.Data[1][GC] - Bottom(Gravity, GC, B));
    {$warnings on}

  {$ifdef CHECK_HEIGHT_VS_RADIUS}
  if Sphere(R) and (R > Result) then
  begin
    WritelnWarning('3D Radius',
      Format('PreferredHeight %f is smaller than radius %f. Gravity may work weird for this 3D object.',
      [Result, R]));
  end;
  {$endif}
end;

{$warnings off} // keep deprecated working
procedure TCastleTransform.UpdateSimpleGravity(const SecondsPassed: Single);

  procedure DoGravity(const PreferredHeight: Single);
  var
    GravityUp: TVector3;

    { TODO: this is a duplicate of similar TCastleWalkNavigation method }
    procedure DoFall;
    var
      BeginPos, EndPos, FallVector: TVector3;
    begin
      { Project Middle and FFallingStartMiddle
        onto GravityUp vector to calculate fall height. }
      BeginPos := PointOnLineClosestToPoint(TVector3.Zero, GravityUp, FFallingStartMiddle);
      EndPos   := PointOnLineClosestToPoint(TVector3.Zero, GravityUp, Middle);
      FallVector := BeginPos - EndPos;

      { Because of various growing and jumping effects (imagine you jump up
        onto a taller pillar) it may turn out that we're higher at the end
        at the end of fall. Do not report it to Fall event in this case. }
      if TVector3.DotProduct(GravityUp, FallVector.Normalize) <= 0 then
        Exit;

      Fall(FallVector.Length);
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
    if (World = nil) or
       (World.MainCamera = nil) then
      Exit;

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
          MaximumFallingDistance := MaximumFallingDistance - 0.01;
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
  if Gravity then
  begin
    PH := PreferredHeight;
    if (PH <> 0) and
       ((FallSpeed <> 0) or (GrowSpeed <> 0)) then
      { calculate and save PreferredHeight once,
        as it's used quite often in the DoGravity procedure }
      DoGravity(PH);
  end;
end;
{$warnings on}

procedure TCastleTransform.Fall(const FallHeight: Single);
begin
  { Nothing to do in this class }
end;

function TCastleTransform.Move(const TranslationChange: TVector3;
  const BecauseOfGravity: boolean; const EnableWallSliding: boolean): boolean;
var
  OldMiddle, ProposedNewMiddle, NewMiddle: TVector3;
begin
  OldMiddle := Middle;

  if EnableWallSliding then
  begin
    ProposedNewMiddle := OldMiddle + TranslationChange;
    Result := MoveAllowed(OldMiddle, ProposedNewMiddle, NewMiddle, BecauseOfGravity);
  end else
  begin
    NewMiddle := OldMiddle + TranslationChange;
    Result := MoveAllowed(OldMiddle, NewMiddle, BecauseOfGravity);
  end;

  if Result then
    Translate(NewMiddle - OldMiddle);
end;

function TCastleTransform.RenderOutside: boolean;
begin
  Result := Assigned(FParent) and (FParent.BlendingSort = bsYSort)
    and Assigned(FParent.Parent) and (FParent.Parent.BlendingSort = bsYSort);
end;

procedure TCastleTransform.ChangedTransformCore(const UpdatePhysicsTransform: Boolean);
var
  RBody: TCastleRigidBody;
begin
  FTransformationValid := false;
  FWorldTransformationValid := false;
  VisibleChangeHere([vcVisibleGeometry]);

  if (FWorld <> nil) and (FWorld.EnablePhysics) and UpdatePhysicsTransform then
  begin
    RBody := FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if (RBody <> nil) and
       (not RBody.DuringSettingTransformationFromPhysicsEngine) then
      RBody.SetWorldTransformation(WorldTransform);
  end;

  ChangedTransform;
end;

procedure TCastleTransform.ChangedTransform;
begin
end;

procedure TCastleTransform.CustomSerialization(const SerializationProcess: TSerializationProcess);
begin
  inherited;
  SerializationProcess.ReadWriteList('Children',
    {$ifdef FPC}@{$endif}SerializeChildrenEnumerate,
    {$ifdef FPC}@{$endif}SerializeChildrenAdd,
    {$ifdef FPC}@{$endif}SerializeChildrenClear);
  SerializationProcess.ReadWriteList('Behaviors',
    {$ifdef FPC}@{$endif}SerializeBehaviorsEnumerate,
    {$ifdef FPC}@{$endif}SerializeBehaviorsAdd,
    {$ifdef FPC}@{$endif}SerializeBehaviorsClear);
end;

procedure TCastleTransform.SerializeChildrenEnumerate(const Proc: TGetChildProc);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    if List[I].ComponentStyle * [csSubComponent, csTransient] = [] then
      Proc(List[I]);
end;

procedure TCastleTransform.SerializeChildrenAdd(const C: TComponent);
begin
  Add(C as TCastleTransform);
end;

procedure TCastleTransform.SerializeChildrenClear;
var
  I: Integer;
begin
  for I := List.Count - 1 downto 0 do // downto, as list may shrink during loop
    if List[I].ComponentStyle * [csSubComponent, csTransient] = [] then
      List[I].Free; // will remove itself from children list
end;

procedure TCastleTransform.SerializeBehaviorsEnumerate(const Proc: TGetChildProc);
var
  I: Integer;
begin
  for I := 0 to BehaviorsCount - 1 do
    if Behaviors[I].ComponentStyle * [csSubComponent, csTransient] = [] then
      Proc(Behaviors[I]);
end;

procedure TCastleTransform.SerializeBehaviorsAdd(const C: TComponent);
begin
  AddBehavior(C as TCastleBehavior);
end;

procedure TCastleTransform.SerializeBehaviorsClear;
var
  I: Integer;
begin
  for I := BehaviorsCount - 1 downto 0 do // downto, as list may shrink during loop
    if Behaviors[I].ComponentStyle * [csSubComponent, csTransient] = [] then
      Behaviors[I].Free; // will remove itself from Behaviors list
end;

function TCastleTransform.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Exists', 'CastShadows', 'RenderLayer', 'BlendingSort'
     ]) then
      Result := [psBasic]
  else
  if ArrayContainsString(PropertyName, [
       'CenterPersistent', 'ScaleOrientationPersistent'
     ]) then
    Result := [psLayout]
  else
  if ArrayContainsString(PropertyName, [
       'RotationPersistent', 'ScalePersistent', 'TranslationPersistent'
     ]) then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ We try hard to keep FOnlyTranslation return fast, and return with true.
  This allows TCastleTransform to be optimized and accurate
  for the (often) case of pure translation. }

function TCastleTransform.OnlyTranslation: boolean;
begin
  Result := FOnlyTranslation;
end;

procedure TCastleTransform.SetCenter(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FCenter, Value) then
  begin
    FCenter := Value;
    FOnlyTranslation := FOnlyTranslation and Value.IsPerfectlyZero;
    ChangedTransformCore;
  end;
end;

procedure TCastleTransform.SetRotation(const Value: TVector4);
begin
  if not TVector4.PerfectlyEquals(FRotation, Value) then
  begin
    FRotation := Value;
    FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
    ChangedTransformCore;
  end;
end;

procedure TCastleTransform.SetScale(const Value: TVector3);
var
  UpdatePhysicsTransform: Boolean;
begin
  if not TVector3.PerfectlyEquals(FScale, Value) then
  begin
    { No point in doing RBody.SetWorldTransformation when scale only changed
      e.g. from (-1,1,1) to (1,1,1).
      This avoids causing noticeable delays when moving in platformer,
      that updates scale often. }
    UpdatePhysicsTransform := not TVector3.Equals(FScale.Abs, Value.Abs);

    FScale := Value;
    FOnlyTranslation := FOnlyTranslation and
      (Value[0] = 1) and (Value[1] = 1) and (Value[2] = 1);
    ChangedTransformCore(UpdatePhysicsTransform);

    if (FWorld <> nil) and
       FWorld.EnablePhysics then
      UpdateCollidersWorldScale;
  end;
end;

procedure TCastleTransform.SetScaleOrientation(const Value: TVector4);
begin
  if not TVector4.PerfectlyEquals(FScaleOrientation, Value) then
  begin
    FScaleOrientation := Value;
    FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
    ChangedTransformCore;
  end;
end;

procedure TCastleTransform.SetTranslation(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FTranslation, Value) then
  begin
    FTranslation := Value;
    ChangedTransformCore;
  end;
end;

function TCastleTransform.GetTranslationXY: TVector2;
begin
  Result := Translation.XY;
end;

procedure TCastleTransform.SetTranslationXY(const Value: TVector2);
begin
  Translation := Vector3(Value, Translation.Z);
end;

procedure TCastleTransform.Translate(const TranslationChange: TVector3);
begin
  Translation := Translation + TranslationChange;
end;

procedure TCastleTransform.Identity;
begin
  Center := TVector3.Zero;
  Rotation := TVector4.Zero;
  Scale := NoScale;
  ScaleOrientation := TVector4.Zero;
  Translation := TVector3.Zero;
end;

function TCastleTransform.RotationFromDirectionUp(const D, U: TVector3): TVector4;
begin
  Result := OrientationFromDirectionUp(D, U,
    DefaultDirection[Orientation],
    DefaultUp[Orientation]
  );
end;

function TCastleTransform.RotationToDirection(const ARotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(ARotation, DefaultDirection[Orientation]);
end;

function TCastleTransform.RotationToUp(const ARotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(ARotation, DefaultUp[Orientation]);
end;

function TCastleTransform.GetDirection: TVector3;
begin
  Result := RotationToDirection(Rotation);
end;

function TCastleTransform.GetUp: TVector3;
begin
  Result := RotationToUp(Rotation);
end;

procedure TCastleTransform.SetDirection(const Value: TVector3);
var
  D, U: TVector3;
begin
  D := Value; // no need to normalize here
  U := GetUp;
  MakeVectorsOrthoOnTheirPlane(U, D);
  Rotation := RotationFromDirectionUp(D, U);
end;

procedure TCastleTransform.SetUp(const Value: TVector3);
var
  D, U: TVector3;
begin
  U := Value; // no need to normalize here
  D := GetDirection;
  MakeVectorsOrthoOnTheirPlane(D, U);
  Rotation := RotationFromDirectionUp(D, U);
end;

procedure TCastleTransform.UpPrefer(const AUp: TVector3);
var
  D, U: TVector3;
begin
  U := AUp; // no need to normalize here
  D := GetDirection;
  MakeVectorsOrthoOnTheirPlane(U, D);
  Rotation := RotationFromDirectionUp(D, U);
end;

procedure TCastleTransform.GetView(out APos, ADir, AUp: TVector3);
begin
  APos := Translation;
  ADir := Direction;
  AUp  := Up;
end;

procedure TCastleTransform.GetWorldView(out APos, ADir, AUp: TVector3);
var
  T, S: TVector3;
  R: TVector4;
begin
  MatrixDecompose(WorldTransform, T, R, S);
  APos := T;
  ADir := RotationToDirection(R);
  AUp := RotationToUp(R);
end;

function TCastleTransform.WorldTranslation: TVector3;
begin
  Result := TranslationFromMatrix(WorldTransform);
end;

function TCastleTransform.OutsideTranslation: TVector3;
var
  AOutsideRoot: TCastleTransform;
begin
  AOutsideRoot := OutsideRoot;
  if Assigned(AOutsideRoot) then
    Result := TranslationFromMatrix(AOutsideRoot.WorldInverseTransform * WorldTransform)
  else
    Result := Translation;
end;

function TCastleTransform.OutsideYSortTranslation: TVector3;
var
  AOutsideRoot: TCastleTransform;
  T: TTransformation;
begin
  T.Init;
  T.Translate(Vector3(0, YSortOffset, 0));

  AOutsideRoot := OutsideRoot;
  if Assigned(AOutsideRoot) then
    Result := TranslationFromMatrix(AOutsideRoot.WorldInverseTransform * WorldTransform * T.Transform)
  else
    Result := TranslationFromMatrix(Transform * T.Transform);
end;

function TCastleTransform.OutsideRoot: TCastleTransform;
begin
  if not RenderOutside then Exit(nil);

  Result := Self.Parent.Parent;
  while (Result <> World) and Assigned(Result) and (Result.BlendingSort = bsYSort)  do
  begin
    if Assigned(Result.Parent) and (Result.Parent.BlendingSort = bsYSort) then Result := Result.Parent
    else Break;
  end;
end;

procedure TCastleTransform.SetView(const APos, ADir, AUp: TVector3;
  const AdjustUp: boolean);
begin
  Translation := APos;
  SetView(ADir, AUp, AdjustUp);
end;

procedure TCastleTransform.SetView(const ADir, AUp: TVector3;
  const AdjustUp: boolean);
var
  D, U: TVector3;
begin
  D := ADir; // no need to normalize here
  U := AUp; // no need to normalize here
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(U, D)
  else
    MakeVectorsOrthoOnTheirPlane(D, U);
  Rotation := RotationFromDirectionUp(D, U);
end;

procedure TCastleTransform.SetWorldView(const APos, ADir, AUp: TVector3;
  const AdjustUp: boolean = true);
var
  D, U, T, S: TVector3;
  R: TVector4;
  DesiredWorldTransform, ParentWorldTransform, LocalTransform: TMatrix4;
  ParentWorldTransformDeterminant: Single;
begin
  if Parent = nil then
  begin
    SetView(APos, ADir, AUp, AdjustUp);
    Exit;
  end;

  D := ADir; // no need to normalize here
  U := AUp; // no need to normalize here
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(U, D)
  else
    MakeVectorsOrthoOnTheirPlane(D, U);
  DesiredWorldTransform := TranslationMatrix(APos) * RotationMatrix(RotationFromDirectionUp(D, U));

  { Note: This would be wrong:
      DesiredWorldTransform := LookDirMatrix(APos, D, U);
    as LookDirMatrix already assumes some things about camera, e.g. that it looks along -Z.
    But SetWorldView and GetWorldView are independent from cameras,
    so think only in terms of "rotations" (just like GetWorldView does),
    and only convert from/to direction/up using RotationFromDirectionUp/RotationTo...
  }

  ParentWorldTransform := Parent.WorldTransform;
  ParentWorldTransformDeterminant := ParentWorldTransform.Determinant;
  if IsZero(ParentWorldTransformDeterminant) then
  begin
    WritelnWarning('Camera under non-reversible world transformation, cannot apply camera vectors reliably.');
    SetView(APos, ADir, AUp, AdjustUp);
    Exit;
  end;

  { We want LocalTransform such that
    ParentWorldTransform * LocalTransform = DesiredWorldTransform }
  LocalTransform := ParentWorldTransform.Inverse(ParentWorldTransformDeterminant) * DesiredWorldTransform;
  MatrixDecompose(LocalTransform, T, R, S);
  Translation := T;
  Rotation := R;
  Scale := S;
end;

function TCastleTransform.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FList);
end;

function TCastleTransform.HasColliderMesh: Boolean;
begin
  Result := false;
end;

procedure TCastleTransform.ColliderMesh(const TriangleEvent: TTriangleEvent);
begin
  // No triangles returned by this
end;

function TCastleTransform.BehaviorsEnumerate: TCastleBehaviorEnumerator;
begin
  Result := TCastleBehaviorEnumerator.Create(Self);
end;

procedure TCastleTransform.AddWorldChangeNotification(
  const Behavior: TCastleBehavior);
begin
  if FBehaviorsWorldChangeNotificationList = nil then
    FBehaviorsWorldChangeNotificationList := TCastleBehaviorList.Create(false);

  if FBehaviorsWorldChangeNotificationList.IndexOf(Behavior) = -1 then
    FBehaviorsWorldChangeNotificationList.Add(Behavior);
end;

procedure TCastleTransform.RemoveWorldChangeNotification(
  const Behavior: TCastleBehavior);
begin
  if FBehaviorsWorldChangeNotificationList = nil then
    Exit;

  FBehaviorsWorldChangeNotificationList.Remove(Behavior);
end;

procedure TCastleTransform.SetListenPressRelease(const Value: Boolean);
begin
  if FListenPressRelease <> Value then
  begin
    FListenPressRelease := Value;
    if World <> nil then
    begin
      if Value then
        FWorld.RegisterPressRelease(Self)
      else
        FWorld.UnregisterPressRelease(Self);
    end;
  end;
end;

function TCastleTransform.BehaviorsCount: Integer;
begin
  { This can be called when TCastleTransform is in the middle of being
    destroyed, and TDesignFrame.SetTransformForGizmos tries to iterate
    over behaviors. So prepare to handle FBehaviors = nil case,
    which should not be possible during normal lifetime. }
  if FBehaviors = nil then
    Exit(0);

  Result := FBehaviors.Count;
end;

function TCastleTransform.GetBehaviors(const Index: Integer): TCastleBehavior;
begin
  Result := TCastleBehavior(FBehaviors[Index]);
end;

procedure TCastleTransform.AddBehavior(const Behavior: TCastleBehavior);
var
  ReasonWhyCannot: String;
begin
  if Behavior.FParent <> Self then
  begin
    if not Behavior.CanAttachToParent(Self, ReasonWhyCannot) then
      raise Exception.CreateFmt('Cannot add behavior %s (%s) to %s (%s) because: %s', [
        Behavior.Name,
        Behavior.ClassName,
        Name,
        ClassName,
        ReasonWhyCannot
      ]);

    { As a first action (after the check CanAttachToParent has passed) remove
      from previous parent. This will also cause necessary virtual method calls,
      like ParentBeforeDetach, WorldBeforeDetach. }
    if Behavior.FParent <> nil then
      Behavior.FParent.RemoveBehavior(Behavior);
    Assert(Behavior.FParent = nil);

    { Add to new parent }

    FBehaviors.Add(Behavior);
    Behavior.FParent := Self;
    Behavior.ParentAfterAttach;
    {$warnings off}
    Behavior.ParentChanged; // call at end, when state is consistent
    {$warnings on}

    { Call WorldAfterAttach }
    if Behavior.ListenWorldChange then
    begin
      AddWorldChangeNotification(Behavior);
      if FWorld <> nil then
        Behavior.WorldAfterAttach;
    end;
    NotifyBehaviorAdded(Behavior);
  end;
end;

procedure TCastleTransform.RemoveBehaviorIndex(const BehaviorIndex: Integer);
var
  Beh: TCastleBehavior;
begin
  Beh := FBehaviors[BehaviorIndex] as TCastleBehavior;

  NotifyBeforeBehaviorRemove(Beh);

  { Call WorldBeforeDetach }
  if Beh.ListenWorldChange and (FWorld <> nil) then
    Beh.WorldBeforeDetach;

  Beh.ParentBeforeDetach;

  Beh.FParent := nil;
  FBehaviors.Delete(BehaviorIndex);
  {$warnings off}
  Beh.ParentChanged; // call at end, when state is consistent
  {$warnings on}
  RemoveWorldChangeNotification(Beh);
end;

procedure TCastleTransform.RemoveBehavior(const Behavior: TCastleBehavior);
var
  I: Integer;
begin
  if Behavior.FParent = Self then
  begin
    I := FBehaviors.IndexOf(Behavior);
    if I = -1 then
    begin
      WritelnWarning('Internal Error: Behavior %s %s has our parent, but is not present on FBehaviors list', [
        Behavior.Name,
        Behavior.ClassType
      ]);
      Exit;
    end;
    RemoveBehaviorIndex(I);
  end;
end;

function TCastleTransform.FindBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;
var
  I: Integer;
begin
  for I := 0 to FBehaviors.Count - 1 do
    if FBehaviors[I] is BehaviorClass then
      Exit(TCastleBehavior(FBehaviors[I])); // since it's BehaviorClass, casting to TCastleBehavior must be safe
  Result := nil;
end;

function TCastleTransform.FindAllBehaviors(const BehaviorClass: TCastleBehaviorClass): TCastleBehaviorList;

  procedure AddTransform(const T: TCastleTransform);
  var
    I: Integer;
    TChild: TCastleTransform;
  begin
    for I := 0 to T.FBehaviors.Count - 1 do
      if T.FBehaviors[I] is BehaviorClass then
        Result.Add(TCastleBehavior(T.FBehaviors[I]));
    for TChild in T do
      AddTransform(TChild);
  end;

begin
  Result := TCastleBehaviorList.Create(false);
  AddTransform(Self);
end;

function TCastleTransform.FindRequiredBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;
begin
  Result := FindBehavior(BehaviorClass);
  if Result = nil then
  begin
    Result := BehaviorClass.Create(Self);
    AddBehavior(Result);
  end;
end;

procedure TCastleTransform.PendingRemovals;
var
  I: Integer;
  Item: TCastleTransform;
begin
  { We iterate downto, and still check that index is valid.
    That is because removing/freeing may call some virtual methods,
    and may in turn cause removal/free of other items,
    even items on the pending list.
    They may also add new items to this list. }

  if FPendingToRemove <> nil then
    for I := FPendingToRemove.Count - 1 downto 0 do
      if I < FPendingToRemove.Count then
      begin
        Item := FPendingToRemove[I] as TCastleTransform;
        Remove(Item);
        FPendingToRemove.Remove(Item);
      end;

  if FPendingToRemoveAndFree <> nil then
    for I := FPendingToRemoveAndFree.Count - 1 downto 0 do
      if I < FPendingToRemoveAndFree.Count then
        FPendingToRemoveAndFree[I].Free; // this will remove it from children, and from FPendingToRemoveAndFree
end;

procedure TCastleTransform.RemoveDelayed(const Item: TCastleTransform; const FreeItem: Boolean = false);
begin
  if FreeItem then
  begin
    if FPendingToRemoveAndFree = nil then
      FPendingToRemoveAndFree := TComponentList.Create(false);
    FPendingToRemoveAndFree.Add(Item);
  end else
  begin
    if FPendingToRemove = nil then
      FPendingToRemove := TComponentList.Create(false);
    FPendingToRemove.Add(Item);
  end;
end;

procedure TCastleTransform.SetExists(const Value: Boolean);
var
  RBody: TCastleRigidBody;
begin
  if FExists <> Value then
  begin
    FExists := Value;
    UpdateExistsInRoot;

    RBody := FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if (RBody <> nil) and RBody.Exists then
    begin
      if FLastExistsChangeFrameId = TFramesPerSecond.FrameId then
        WritelnWarning('%s:%s: The existence of this TCastleTransform changed multiple times in a single frame and it uses physics (has TCastleRigidBody and TCastleCollider behaviors). ' +
          'This is not good for performance and right now can even cause physics simulation crashes, esp. when TCastleCollider.AutoSize=true and animation may change LocalBoundingBox. ' +
          'Avoid changing the Exists of physical objects multiple times in a single Update.', [
          Name,
          ClassName
        ]);
      FLastExistsChangeFrameId := TFramesPerSecond.FrameId;
    end;
  end;
end;

procedure TCastleTransform.SetParentExistsInRoot(const Value: Boolean);
begin
  if FParentExistsInRoot <> Value then
  begin
    FParentExistsInRoot := Value;
    UpdateExistsInRoot;
  end;
end;

procedure TCastleTransform.UpdateExistsInRoot;
var
  NewExistsInRoot: Boolean;
  I: Integer;
begin
  NewExistsInRoot := ParentExistsInRoot and Exists;
  if FExistsInRoot <> NewExistsInRoot then
  begin
    FExistsInRoot := NewExistsInRoot;
    for I := Count - 1 downto 0 do
      Items[I].ParentExistsInRoot := FExistsInRoot; // use child SetParentExistsInRoot, so it can update it's ExistsInRoot
    ExistsInRootChanged; // virtual, called at end to be called when all children are valid too
  end;
end;

procedure TCastleTransform.ExistsInRootChanged;
var
  RBody: TCastleRigidBody;
begin
  { Update rigid body existence when ExistsInRoot changes }
  if (FWorld <> nil) and (FWorld.FKraftEngine <> nil) then
  begin
    RBody := FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if RBody <> nil then
      RBody.UpdateExist;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
