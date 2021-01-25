{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Group and transform scenes (TCastleTransform). }
unit CastleTransform;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, Generics.Collections, Contnrs, Kraft,
  CastleVectors, CastleFrustum, CastleBoxes, CastleClassUtils, CastleKeysMouse,
  CastleRectangles, CastleUtils, CastleTimeUtils,
  CastleSoundEngine, CastleCameras, CastleTriangles, CastleRenderOptions;

type
  TCastleAbstractRootTransform = class;
  TCastleTransform = class;
  TRenderingCamera = class;

  TCastleTransformClass = class of TCastleTransform;

  ECannotAddToAnotherWorld = class(Exception);
  ETransformParentUndefined = class(Exception);
  EMultipleReferencesInWorld = class(ETransformParentUndefined);
  ENotAddedToWorld = class(ETransformParentUndefined);
  EPhysicsError = class(Exception);

  TRenderFromViewFunction = procedure (const RenderingCamera: TRenderingCamera) of object;

  { Describe what visible thing changed for TCastleTransform.VisibleChangeHere. }
  TVisibleChange = (
    { Visible geometry (actual 3D shape) changed.
      It's not used when only light conditions, materials, textures
      and such changed.

      In practice, this means that the depth buffer from some point in space
      changed. Which means that shadow maps should be regenerated. }
    vcVisibleGeometry,

    { Something visible, but not the geometry shape, changes.
      For example, material or texture on a visible surface changed. }
    vcVisibleNonGeometry
  );
  TVisibleChanges = set of TVisibleChange;

  TVisibleChangeEvent = procedure (const Sender: TCastleTransform; const Changes: TVisibleChanges) of object;

  { Various things that TCastleTransform.PrepareResources may prepare. }
  TPrepareResourcesOption = (
    { Prepare resources for rendering *this* scene
      (on which TCastleTransform.PrepareResources is called). }
    prRenderSelf,
    { Prepare resources for rendering clones of the scene.
      E.g. textures, which are shared by clones using the cache. }
    prRenderClones,
    prBackground,
    prBoundingBox,
    prShadowVolume,
    { Prepare octrees (determined by things like TCastleSceneCore.Spatial). }
    prSpatial,
    prScreenEffects);
  TPrepareResourcesOptions = set of TPrepareResourcesOption;

  { Shadow volumes helper, not depending on OpenGL. }
  TBaseShadowVolumeRenderer = class
  end;

  TCastleTransformList = class;

  { Information about ray collision with a single 3D object.
    Everything (Point, RayOrigin, RayDirection) is expressed
    in the parent coordinate system of this TCastleTransform (in @link(Item)). }
  TRayCollisionNode = object
  public
    { Colliding object. }
    Item: TCastleTransform;

    { Position, in local coordinate system of this 3D object,
      of the picked 3D point.

      If the ray hit empty space (@link(Triangle) field is @nil),
      then this is undefined.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance. }
    Point: TVector3;

    { Triangle that was hit. This triangle is always a part of @link(Item).

      If the ray hit empty space then this is @nil.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance.

      May also be @nil if RayCollision implementation for the 3D object
      simply left it @nil. For example,

      @unorderedList(
        @item(If the collision occured with a TCastleSceneCore with
          @link(TCastleSceneCore.Spatial) that includes a collidable
          structure (ssDynamicCollisions or ssStaticCollisions),
          then this triangle is set.)

        @item(If the collision occured merely with a TCastleTransform bounding box,
          the triangle is left as @nil.)
      )
    }
    Triangle: PTriangle;

    { Ray used to cause the collision,
      in the parent coordinate system of this TCastleTransform.
      RayDirection is @italic(not) necessarily normalized! }
    RayOrigin, RayDirection: TVector3;
  end;
  PRayCollisionNode = ^TRayCollisionNode;

  { Represents a @bold(ray) collision with a 3D objects tree.
    Just access the @code(First) item for the collision information
    with the final 3D object. The rest of items are containers of this 3D
    object (a path within @link(TCastleViewport.Items) hierarchy tree,
    usually).

    This list is a path in the TCastleTransform tree leading from the
    final colliding object (usually TCastleScene)
    to the root of the TCastleTransform tree.
    This allows you to track the TCastleTransform
    containers that contain given collision.

    This is never an empty list when returned by RayCollision. }
  TRayCollision = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TRayCollisionNode>)
  public
    { Distance, in world coordinate system, from the current
      camera to the picked point. The suggested usage is to decide if player
      is close enough to reach the 3D object --- for example, you may not
      want to allow player to open a door by clicking on it from a far distance.

      If the ray hit empty space, the distance is MaxSingle.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance. }
    Distance: Single;

    { Index of node with given Item, -1 if none. }
    function IndexOfItem(const Item: TCastleTransform): Integer;

    { Index of node with given ItemClass, -1 if none. }
    function IndexOfItem(const ItemClass: TCastleTransformClass): Integer;
  end;

  { Detailed information about collision with a single 3D object. }
  TCollisionDetailsItem = object
  public
    { Colliding 3D object. }
    Item: TCastleTransform;
  end;
  PCollisionDetailsItem = ^TCollisionDetailsItem;

  { Represents a collision with a 3D objects tree.
    Just access the @code(First) item for the collision information
    with the final 3D object. The rest of items are containers of this 3D
    object (a path within @link(TCastleViewport.Items) hierarchy tree,
    usually).

    This list is a path in the TCastleTransform tree leading from the
    final colliding object to the root of the TCastleTransform tree.
    This allows you to track the TCastleTransform
    containers that contain given collision.

    This is never an empty list when returned by XxxCollision method. }
  TCollisionDetails = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TCollisionDetailsItem>)
  public
    { Index of node with given Item. }
    function IndexOfItem(const Item: TCastleTransform): Integer;
    procedure Add(const Item: TCastleTransform); reintroduce;
  end;

  TPhysicsCollisionDetails = record
  public
    Transforms: array[0..1] of TCastleTransform;
    OtherTransform: TCastleTransform;
  end;

  TRemoveType = (rtNone, rtRemove, rtRemoveAndFree);

  {$define read_interface}
  {$I castletransform_renderparams.inc}
  {$I castletransform_behavior.inc}
  {$undef read_interface}

  { Information that a TCastleTransform object needs to prepare rendering.

    This is @bold(mostly an internal class). You should not need to create it,
    you should not need to read anything inside or deal with this class otherwise.

    The only official usage allowed is to pass an instance of this class
    taken from @link(TCastleViewport.PrepareParams)
    as a parameter to @link(TCastleTransform.PrepareResources) and friends
    like @link(T3DResource.Prepare). You should treat this class as a "black box"
    in normal applications. }
  TPrepareParams = class
    { Include a headlight, or global lights that shine on all
      scenes (see @link(TCastleViewport.UseGlobalLights)).

      It is not necessary to define this (it may be @nil).
      And all the lighting is dynamic, so of course you can always
      turn on / off things like a headlight during the game.
      However, passing here the appropriate lights will mean that the shaders
      are immediately prepared for the current lighting.

      @exclude }
    InternalBaseLights: TAbstractLightInstancesList;

    { World fog, in any, to prepare for.
      @exclude }
    InternalGlobalFog: TAbstractFogNode;
  end;

  { List of TCastleTransform instances.
    This inherits from TCastleObjectList, getting many
    features like TList notification mechanism. }
  TCastleTransformList = class(TCastleObjectList)
  private
    { Note: Using this class with FOwner <> nil is only for internal purposes. }
    FOwner: TCastleTransform;
    function GetItem(const I: Integer): TCastleTransform;
    procedure SetItem(const I: Integer; const Item: TCastleTransform);
    { TCastleTransform instance that owns this list.
      May be @nil, for example when this list is used by TRayCollision. }
    property Owner: TCastleTransform read FOwner;
  public
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Items[I: Integer]: TCastleTransform read GetItem write SetItem; default;

    function First: TCastleTransform;
    function Last: TCastleTransform;
  end;

  { Orientation of the model is 3D world, determining where is
    the conceptual "up" direction of the model,
    and where is it facing. Used by the @link(TCastleTransform.Orientation)
    and @link(TCastleTransform.DefaultOrientation). }
  TOrientationType = (
    { Orientation sensible for models oriented around Y axis,
      using default export from Blender to X3D.

      Gravity pulls in -Y and GravityUp vector is +Y.
      Transformation makes -Z and +Y match (respectively) Direction and Up.

      This matches default direction/up of OpenGL and VRML/X3D cameras.

      For example, using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you use default
      Blender X3D exporter, and you let the exporter to make
      a default transformation (to make +Z up into +Y up). This is the default setting.
      Then you can follow the standard Blender view names
      ("front", "top" and such) when modelling, and Blender tools like
      "X-axis mirror" will work best. }
    otUpYDirectionMinusZ,

    { Orientation sensible for models oriented around Y axis,
      using default export from Blender to glTF or Wavefront OBJ.
      This matches glTF specification that explicitly says "The front of a glTF asset faces +Z".

      Gravity pulls in -Y and GravityUp vector is +Y.
      Transformation makes +Z and +Y match (respectively) Direction and Up.

      This does not match default direction/up of OpenGL and VRML/X3D cameras.
      When viewed from the default direction/up of OpenGL and VRML/X3D cameras,
      you will see the front of the model,
      which means that the model's direction is the opposite of the camera direction.

      For example, using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you use default
      Blender glTF or OBJ exporter, and you let the exporter to make
      a default transformation (to make +Z up into +Y up). This is the default setting.
      Then you can follow the standard Blender view names
      ("front", "top" and such) when modelling, and Blender tools like
      "X-axis mirror" will work best. }
    otUpYDirectionZ,

    { Orientation sensible for models oriented around Z axis.
      Transformation makes -Y and +Z match (respectively) Direction and Up.

      Using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you export your models
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

  TRigidBody = class;

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
  private
    type
      TEnumerator = class
      private
        FList: TCastleTransformList;
        FPosition: Integer;
        function GetCurrent: TCastleTransform;
      public
        constructor Create(AList: TCastleTransformList);
        function MoveNext: Boolean;
        property Current: TCastleTransform read GetCurrent;
      end;
    class var
      NextTransformId: Cardinal;
    var
      FCastShadowVolumes: boolean;
      FExists: boolean;
      FCollides: boolean;
      FPickable: boolean;
      FVisible: boolean;
      FCursor: TMouseCursor;
      FCollidesWithMoving: boolean;
      Disabled: Cardinal;
      FExcludeFromGlobalLights, FExcludeFromStatistics,
        FInternalExcludeFromParentBoundingVolume: boolean;
      FWorld: TCastleAbstractRootTransform;
      FWorldReferences: Cardinal;
      FList: TCastleTransformList;
      FBehaviors: TComponentList;
      FParent: TCastleTransform;
      FCollisionSphereRadius: Single;
      FListenPressRelease: Boolean;

      // transformation
      FCenter: TVector3;
      FRotation: TVector4;
      FScale: TVector3;
      FScaleOrientation: TVector4;
      FTranslation: TVector3;

      // transformation extras
      FGravity: boolean;
      FFallingStartMiddle: TVector3;
      FFalling: boolean;
      FFallSpeed: Single;
      FGrowSpeed: Single;
      FMiddleHeight: Single;
      FRigidBody: TRigidBody;
      FOrientation: TOrientationType;
      FOnlyTranslation: boolean;

      FTransformation: TTransformation;
      FTransformationValid: boolean;

      FWorldTransformation: TTransformation;
      FWorldTransformationId: Cardinal;
      FWorldTransformationValid: boolean;

      FLastParentWorldTransformation: TTransformation;
      FLastParentWorldTransformationId: Cardinal;

    procedure SetCursor(const Value: TMouseCursor);
    procedure SetCenter(const Value: TVector3);
    procedure SetRotation(const Value: TVector4);
    procedure SetScale(const Value: TVector3);
    procedure SetScaleOrientation(const Value: TVector4);
    procedure SetTranslation(const Value: TVector3);
    function GetTranslationXY: TVector2;
    procedure SetTranslationXY(const Value: TVector2);
    procedure SetRigidBody(const Value: TRigidBody);
    function GetDirection: TVector3;
    function GetUp: TVector3;
    procedure SetDirection(const Value: TVector3);
    procedure SetUp(const Value: TVector3);
    function RotationFromDirectionUp(const D, U: TVector3): TVector4;
    function RotationToDirection(const ARotation: TVector4): TVector3;
    function RotationToUp(const ARotation: TVector4): TVector3;
    procedure UpdateSimpleGravity(const SecondsPassed: Single);
    procedure UpdatePhysicsEngine(const SecondsPassed: Single);
    function GetItem(const I: Integer): TCastleTransform;
    procedure SetItem(const I: Integer; const Item: TCastleTransform);
    procedure PhysicsDestroy;
    procedure PhysicsNotification(AComponent: TComponent; Operation: TOperation);
    procedure PhysicsChangeWorldDetach;
    procedure PhysicsChangeWorldAttach;
    procedure InternalTransformationMult(var T: TTransformation);
    procedure InternalTransformation(out T: TTransformation);
    { Update our FWorldTransformation, FWorldTransformationId. }
    procedure UpdateWorldTransformation;
    { Return non-nil parent, making sure it's valid. }
    function Parent: TCastleTransform;
    procedure WarningMatrixNan(const NewParamsInverseTransformValue: TMatrix4);
    { Change to new world, or (if not needed) just increase FWorldReferences.
      Value must not be @nil. }
    procedure AddToWorld(const Value: TCastleAbstractRootTransform);
    { Decrease FWorldReferences, then (if needed) change world to @nil.
      Value must not be @nil. }
    procedure RemoveFromWorld(const Value: TCastleAbstractRootTransform);
    procedure RemoveBehaviorIndex(const BehaviorIndex: Integer);
    procedure SetListenPressRelease(const Value: Boolean);
  protected
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
      collidable or not. Instead, this looks at @link(Pickable) property
      (actually, at @link(GetPickable) method result).

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
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; virtual;
    function LocalMoveCollision(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean; virtual;
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
    procedure LocalRenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4); virtual;

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

    { Override this to be notified about every transformation change.
      By default, this calls VisibleChangeHere, which causes the window to redraw. }
    procedure ChangedTransform; virtual;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    const
      DefaultMiddleHeight = 0.5;
      DefaultDirection: array [TOrientationType] of TVector3 = (
        (Data: (0,  0, -1)),
        (Data: (0,  0, +1)),
        (Data: (0, -1,  0)),
        (Data: (1,  0,  0))
      );
      DefaultUp: array [TOrientationType] of TVector3 = (
        (Data: (0, 1, 0)),
        (Data: (0, 1, 0)),
        (Data: (0, 0, 1)),
        (Data: (0, 0, 1))
      );

    var
      { Default value of @link(TCastleTransform.Orientation) for new instances.
        By default @link(otUpYDirectionZ), matching glTF orientation
        (as exported from Blender and other software). }
      DefaultOrientation: TOrientationType; static;

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

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InternalAddChild(const C: TComponent); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;
    function GetEnumerator: TEnumerator;

    { Does item really exist, see @link(Exists) and @link(Enable),
      @link(Disable).
      It TCastleTransform class, returns @true if @link(Exists) and not disabled.
      May be modified in subclasses, to return something more complicated. }
    function GetExists: boolean; virtual;

    { Does item really collide, see @link(Collides).
      It TCastleTransform class, returns @link(Collides) and @link(GetExists).
      May be modified in subclasses, to return something more complicated. }
    function GetCollides: boolean; virtual;

    { Is item really pickable, see @link(Pickable).
      It TCastleTransform class, returns @link(Pickable) and @link(GetExists).
      May be modified in subclasses, to return something more complicated. }
    function GetPickable: boolean; virtual;

    { Is item really visible, see @link(Visible).
      It v class, returns @link(Visible) and @link(GetExists).
      May be modified in subclasses, to return something more complicated. }
    function GetVisible: boolean; virtual;

    { Items that are at least once disabled are treated like not existing.
      Every @link(Disable) call should always be paired with @link(Enable) call
      (usually using @code(try ... finally .... end) block).
      Internally, we keep a counter of how many times the object is disabled,
      and if this counter is <> 0 then GetExists returns @false.
      Using this is useful for taming collisions, especially to avoid self-collisions
      (when a creature moves, it checks for collision with other creatures,
      but it doesn't want to check for collisions with it's own bounding volume).
      @groupBegin }
    procedure Disable;
    procedure Enable;
    { @groupEnd }

    { Operate on 3D objects contained in the list.
      You can also operate directly on @link(List) instance.

      Note that adding and removing from this hierarchy is guaranteed to be fast,
      so you can do it even in the middle of the game.
      In particular calling @link(Remove) doesn't free rendering reasources
      of the removed scene,
      so removing scene only to add it later to another TCastleViewport.Items is blazingly fast.
      @groupBegin }
    procedure Add(const Item: TCastleTransform);
    procedure Insert(const Index: Integer; const Item: TCastleTransform);
    procedure Remove(const Item: TCastleTransform);
    property Items[I: Integer]: TCastleTransform read GetItem write SetItem; default;
    function Count: Integer;
    procedure Clear;
    procedure Exchange(const Index1, Index2: Integer);
    { @groupEnd }

    { Return parent TCastleTransform, but only if this TCastleTransform
      is a child of exactly one other TCastleTransform.

      In general, the same TCastleTransform instance may be used
      multiple times as a child of other TCastleTransform instances
      (as long as they are all within the same TCastleViewport),
      in which case this property is @nil.

      It is also @nil if this is the root transform, not added to any parent. }
    property UniqueParent: TCastleTransform read FParent;

    { Sort objects back-to-front @italic(right now)
      following one of the blending sorting algorithms.
      Only the immediate list items are reordered,
      looking at their bounding boxes.

      Calling this method makes sense if you have a list
      of objects, and some of them are partially-transparent and may
      be visible at the same place on the screen.
      It may even make sense to call this method every frame (like in every
      @link(TCastleWindowBase.OnUpdate)),
      if you move or otherwise change the objects (changing their bounding boxes),
      or if the CameraPosition may change (note that CameraPosition is only
      relevant if BlendingSort = bs3D).

      Sorting partially-transparent objects avoids artifacts when rendering.

      Note that this doesn't take care of sorting the shapes
      within the scenes. For this, you should set
      @link(TCastleRenderOptions.BlendingSort Scene.RenderOptions.BlendingSort)
      to a value like bs3D, to keep it sorted.
      It is actually the default now.

      See the TBlendingSort documentation for the exact specification
      of sorting algorithms. Using BlendingSort = bsNone does nothing. }
    procedure SortBackToFront(const BlendingSort: TBlendingSort;
      const CameraPosition: TVector3);

    { Sort objects back-to-front @italic(right now)
      following the 2D blending sorting algorithm.
      See @link(SortBackToFront) for documentation, this method
      is only a shortcut for @code(SortBackToFront(bs2D, TVector3.Zero)). }
    procedure SortBackToFront2D;

    { Bounding box of this object, taking into account current transformation
      (like @link(Translation), @link(Rotation))
      although not parent transformations (for this, see @link(WorldBoundingBox)).

      Takes into account both collidable and visible objects.
      For example, invisible walls (not visible) and fake walls
      (not collidable) should all be accounted here.

      It's a @italic(bounding) volume, it should be as large as necessary
      to include the object inside. At the same time, it should be
      as "tight" as it can, to make various optimizations work best. }
    function BoundingBox: TBox3D;

    { Bounding box of this object, ignoring the transformations of this scene and parents. }
    function LocalBoundingBox: TBox3D; virtual;

    { Bounding box of this object, taking into account
      all transformations of this and parents. }
    function WorldBoundingBox: TBox3D;

    { Render given object.
      Should check and immediately exit when @link(GetVisible) is @false.

      The rendering transformation, frustum, and filtering
      is specified inside TRenderParams class.
      This method should only update @code(TRenderParams.Statistics). }
    procedure Render(const Params: TRenderParams); virtual; overload;

    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); overload;
      deprecated 'use Render method without an explicit Frustum parameter, it is in Params.Frustum now';

    { Does the 3D object cast shadows by shadow volumes.
      See also TCastleScene.ReceiveShadowVolumes. }
    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes default true;

    { Render shadow quads for all the things rendered by @link(Render).
      This is done only if @link(Exists) and @link(CastShadowVolumes).

      It does shadow volumes culling inside (so ShadowVolumeRenderer should
      have FrustumCullingInit already initialized).

      ParentTransform and ParentTransformIsIdentity describe the transformation
      of this object in the world.
      TCastleTransform objects may be organized in a hierarchy when
      parent transforms it's children. When ParentTransformIsIdentity,
      ParentTransform must be TMatrix4.Identity (it's not guaranteed
      that when ParentTransformIsIdentity = @true, Transform value will be
      ignored !). }
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4);

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

      @param(ProgressStep Says that we should call Progress.Step.
        It will be called PrepareResourcesSteps times.
        Useful to show progress bar to the user during long preparation.)

      @param(Params
        Rendering parameters to prepare for.
        It is used only if Options contains prRenderSelf or prRenderClones.
      )
    }
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const ProgressStep: boolean; const Params: TPrepareParams); virtual;

    { How many times PrepareResources will call Progress.Step.
      Useful only if you want to pass ProgressStep = @true to PrepareResources.
      In the base class TCastleTransform this just returns 0.  }
    function PrepareResourcesSteps: Cardinal; virtual;

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

      These methods are called only if the object GetExists.
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
      @param(RemoveMe Set this to rtRemove or rtRemoveAndFree to remove
        this item from parent after this call finished.
        rtRemoveAndFree additionally will free this item.
        Initially it's rtNone when this method is called.) }
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
    procedure VisibleChangeHere(const Changes: TVisibleChanges);

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
      It ignores parent transformations (using @code(Transform.UniqueParent.LocalToWorld(Transform.Middle))
      to convert this to world coordinates. }
    function Middle: TVector3; virtual;

    { Can the approximate sphere (around Middle point)
      be used for some collision-detection
      tasks. If @true then Radius (and Middle point) determine the approximate
      sphere surrounding the 3D object (it does not have to be a perfect
      bounding sphere around the object), and it may be used for some
      collisions instead of BoundingBox.
      See @link(CollidesWithMoving) and @link(MoveAllowed) for when it may happen.

      Must return @false when not GetExists (because we can't express
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

      This ignores the geometry of this 3D object (to not accidentaly collide
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

      This ignores the geometry of this 3D object (to not accidentaly collide
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

    { Cast a ray, see what is hit.

      The given RayOrigin, RayDirection are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.Ray(MyTransform.Translation, MyTransform.Direction)).

      This ignores the geometry of this object (to not accidentaly collide
      with your own geometry), and checks collisions with the rest of the world. }
    function Ray(const RayOrigin, RayDirection: TVector3): TRayCollision;

    { Cast a ray, see what is hit.

      The given RayOrigin, RayDirection are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.RayCast(MyTransform.Translation, MyTransform.Direction)).
      In case of the overloaded version with Distance parameter,
      the Distance is consistently in the same, parent coordinate system.

      This ignores the geometry of this object (to not accidentaly collide
      with your own geometry), and checks collisions with the rest of the world.

      This returns the TCastleTransform that is hit (this is the "leaf" TCastleTransform
      in the TCastleTransform tree that is hit)
      and a distance from RayOrigin to the hit point.
      Returns @nil (Distance is undefined in this case) if nothing was hit.
      Use @link(Ray) for a more advanced version of this, with more complicated result.
      @groupBegin }
    function RayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
    function RayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
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
      (it's linked to TCastleWalkNavigation.OnFall in this case).

      TODO: This will be deprecated at some point, and you will be adviced
      to always use physics, through @link(TCastleTransform.RigidBody),
      to have realistic gravity. }
    property Gravity: boolean read FGravity write FGravity default false;

    { Falling speed, in units per second, for @link(Gravity).

      This is relevant only if @link(Gravity) and PreferredHeight <> 0.
      0 means no falling.

      TODO: In CGE 7.x this will be deprecated, and you will be adviced
      to always use physics, through @link(TCastleTransform.RigidBody),
      to have realistic gravity. }
    property FallSpeed: Single read FFallSpeed write FFallSpeed default 0;

    { Growing (raising from crouching to normal standing position)
      speed, in units per second.
      This is used by non-flying creatures when climbing up stairs,
      in which case @link(Translation) ("legs positon") may be sometimes under
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

      This property determines how the TCastleTransform
      handles the @link(Middle) implementation
      (this is the point used for various collision detection routines)
      and @link(PreferredHeight) (this is the preferred height of @link(Middle)
      above the ground). You can override these two methods to use a different
      approach, and then ignore MiddleHeight completely. }
    property MiddleHeight: Single read FMiddleHeight write FMiddleHeight
      default DefaultMiddleHeight;

    { Transformation (from local to outside) as a matrix.
      This matrix represents a concise version of properties like @link(Translation),
      @link(Rotation), @link(Scale). It does not take into account the
      transformation of parent TCastleTransform (for this, use @link(WorldTransform)). }
    function Transform: TMatrix4;

    { Inverse transformation as a matrix, thus transforming from outside to local
      coordinate system. This is an inverse of @link(Transform). }
    function InverseTransform: TMatrix4;

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

    { Translation (move) the children. Zero by default. }
    property Translation: TVector3 read FTranslation write SetTranslation;

    { Get or set the XY components of the translation.
      Useful for 2D games. }
    property TranslationXY: TVector2 read GetTranslationXY write SetTranslationXY;

    function GetTranslation: TVector3; deprecated 'use Translation';

    { Center point around which the @link(Rotation) and @link(Scale) is performed. }
    property Center: TVector3 read FCenter write SetCenter;

    { Rotation in 3D, around a specified axis.
      Rotation is expressed as a 4D vector, in which the first 3 components
      specify the rotation axis (does not need to be normalized, but must be non-zero),
      and the last component is the rotation angle @italic(in radians).

      Rotation is done around @link(Center). }
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
          calculated then. In particular sphere collision routines
          (@link(TCastleAbstractRootTransform.WorldSphereCollision),
          @link(TCastleAbstractRootTransform.WorldSphereCollision2D))
          only do an approximation in case of non-uniform scale.

          Note that @link(ScaleOrientation) matters only in case of non-uniform scale.
        )

        @item(All scale components should > 0 if you want 3D lighting
          to work corrrectly. That is, avoid negative scale, that changes
          the normals and orientation of faces (counter-clockwise becomes clockwise,
          if all scale components are -1), or standard
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
    property Scale: TVector3 read FScale write SetScale;

    { Orientation in which 3D @link(Scale) is performed. }
    property ScaleOrientation: TVector4 read FScaleOrientation write SetScaleOrientation;

    { Make the transform do nothing --- zero @link(Translation), zero @link(Rotation),
      @link(Scale) to one. Also resets @link(ScaleOrientation). }
    procedure Identity;

    { Participate in rigid body physics simulation.
      This makes this object collidable with other rigid bodies
      (if @link(Collides))
      and it allows to move and rotate because of gravity
      or because of collisions with other objects
      (if @link(TRigidBody.Dynamic) is @true).

      Setting this property makes this 3D object
      a single rigid body for the physics engine.

      If this property is assigned and the @link(TRigidBody.Dynamic) is @true
      (and @link(TRigidBody.Dynamic) is @true by default)
      then this object is moved and rotated using the physics engine.
      It will move because of gravity (if @link(TRigidBody.Gravity),
      also @true by default),
      and because of collisions with other objects.

      The @link(TRigidBody.Collider) property must be initialized
      @italic(before) assigning the @link(TRigidBody) instance here.
      So you must create a @link(TCollider) descendant, specyfying the given
      @link(TRigidBody) as a parent.
      A rigid body without a collider would in theory not collide with anything,
      and (if @link(TRigidBody.Dynamic)) would simply fall down because of gravity.
      In practice, a rigid body without a collider is simply not allowed.
      If you really need this, assign anything to @link(TRigidBody.Collider)
      and just set @link(Collides) to @false.

      @bold(Our engine (for now) also has an internal, simple physics simulation,
      used to perform collisions with player, creatures, and optional
      (unrealistic) gravity.)
      So we have two independent physics systems,
      but they try to not get into each others way (they each perform
      a different task).
      In the future there will be an option to use
      the full-featured physics engine for all simulations,
      also for player and creatures, at which point our "internal physics
      simulation" will become deprecated.
      For now, be aware of these:

      @unorderedList(
        @item(@link(TCastleTransform.Collides) property affects @italic(both our
          "simple physics simulation" and the "full-featured physics engine").
          It determines whether the object is collidable.
        )

        @item(@link(TCastleTransform.Gravity) property @italic(only affects the
          "simple physics simulation"). It is by default @false.

          It has no effect on the "full-featured physics engine" behavior,
          that has an independent property @link(TRigidBody.Gravity),
          which is @true by default.

          You should not set both @link(TCastleTransform.Gravity) to @true
          and @link(TRigidBody.Gravity) to @true, it would mean that
          @italic(gravity simulation is performed twice).
          In the future, @link(TCastleTransform.Gravity) may be removed
          (or merged with @link(TRigidBody.Gravity), but then old games
          may by surprised by a new default @true.)
        )

        @item(The collider used by our internal, simple physics is independent
          from the @link(TRigidBody.Collider).
          The internal, simple physics uses colliders implicitly implemented
          in the overridden @link(TCastleTransform.HeightCollision),
          @link(TCastleTransform.MoveCollision) and friends.
          In case of @link(TCastleSceneCore), the rule is simple:

          @unorderedList(
            @item(If the @link(TCastleSceneCore.Spatial) contains
              ssDynamicCollisions or ssStaticCollisions, then the object collides
              as a mesh. This makes precise collisions with all the triangles.
              Any mesh, convex or concave, is correctly solved.)
            @item(Otherwise, the object collides as it's axis-aligned bounding box.)
          )
        )
      )

      Note that an object can only be present once in the @link(World)
      to be a rigid body. Breaking this rule will cause the
      @link(EMultipleReferencesInWorld) exception at an undefined time.
    }
    property RigidBody: TRigidBody read FRigidBody write SetRigidBody;

    property Position: TVector3 read FTranslation write SetTranslation;
      deprecated 'use Translation';

    { Direction the creature is facing, and up vector.
      These properties provide an alternative way to get and set the @link(Rotation)
      of the transformation.
      Thinking in terms of "direction" and "up" is often more natural
      when transforming creatures and player.

      The @link(Orientation) determines what is your default direction and up
      (when the @link(Rotation) is zero).
      By default we follow X3D standard vectors suitable for gravity along
      the Y axis. So direction is -Z (DefaultCameraDirection),
      up is +Y (DefaultCameraUp).

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). When setting them by these properties, we will normalize
      them automatically.

      They must also always be orthogonal.
      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      @groupBegin }
    property Direction: TVector3 read GetDirection write SetDirection;
    property Up: TVector3 read GetUp write SetUp;
    { @groupEnd }

    { Get at once vectors: position, direction, up. }
    procedure GetView(out APos, ADir, AUp: TVector3);

    { Set at once vectors: position, direction, up.

      ADir and AUp given here do not have to be normalized
      (they will be normalized if needed).
      They will be automatically fixed to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const APos, ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;
    procedure SetView(const ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;

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

      The default value of this is determined by static variable
      DefaultOrientation, this is usually comfortable (because almost
      always you use the same Orientation throughout your game).
      By default it's otUpYDirectionMinusZ (matching default cameras
      of OpenGL and VRML/X3D).

      This value determines how you should model your 3D models,
      like the creatures, the items, and the player weapons.
      Generally, it applies to every 3D model that is used as
      a child of this TCastleTransform instance. }
    property Orientation: TOrientationType read FOrientation write FOrientation;

    { Transformation objects inside.
      Freeing these items automatically removes them from this list. }
    property List: TCastleTransformList read FList;

    { Add a TCastleBehavior to this TCastleTransform.
      In effect, the virtual methods of TCastleBehavior, like @link(TCastleBehavior.Update),
      will be automatically called.
      Also the @link(TCastleBehavior.Parent) gets assigned.
      If the TCastleBehavior was part of another TCastleTransform, it is removed from it. }
    procedure AddBehavior(const Behavior: TCastleBehavior);

    { Remove TCastleBehavior from this TCastleTransform.
      In effect, the virtual methods of TCastleBehavior, like @link(TCastleBehavior.Update),
      will no longer be automatically called.
      The @link(TCastleBehavior.Parent) is set to @nil. }
    procedure RemoveBehavior(const Behavior: TCastleBehavior);

    { Find the first behavior of the given class, @nil if none. }
    function FindBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;

    { Find the first behavior of the given class, or create and add a new one if necessary.
      Never returns @nil. }
    function FindRequiredBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;
  published
    { Is this object visible and colliding.

      Setting this to @false pretty much turns everything of this 3D object
      to "off". This is useful for objects that disappear completely from
      the level when something happens. You could just as well remove
      this object from @link(TCastleViewport.Items) tree, but sometimes it's more
      comfortable to simply turn this property to @false.

      Descendants may also override GetExists method.

      @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

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

      The only exception are the collisions with TCastleMoving instances
      (movable world parts like elevators and doors) that have their own
      detection routines and look at CollidesWithMoving property of other objects.
      That is, the TCastleMoving instance itself must still have Collides = @true,
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

    { Is item visible.
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects are never visible).
      This is independent from @link(Collides) or @link(Pickable).

      Descendants may also override GetVisible method. Sometimes it's more
      comfortable than changing the property value.

      @noAutoLinkHere }
    property Visible: boolean read FVisible write FVisible default true;

    { If this 3D object is rendered as part of TCastleViewport,
      and @link(TCastleViewport.UseGlobalLights) is @true, then this property allows
      to make an exception for this 3D object: even though
      @link(TCastleViewport.UseGlobalLights) is @true,
      do not use global lights @italic(for this 3D object).

      Note that this is not applied recursively. Instead, it is checked at each TCastleTransform instance
      that checks TRenderParams.BaseLights. In practice, it is only checked at TCastleScene,
      unless you do custom rendering on your own. }
    property ExcludeFromGlobalLights: boolean
      read FExcludeFromGlobalLights write FExcludeFromGlobalLights default false;

    { Exclude from rendering statistics in
      @link(TCastleViewport.Statistics). }
    property ExcludeFromStatistics: boolean
      read FExcludeFromStatistics write FExcludeFromStatistics default false;

    { When non-zero, we can approximate collisions with this object using a sphere
      in certain situations (@link(MoveAllowed), @link(Gravity)).
      This usually makes dynamic objects, like player and creatures, collide better. }
    property CollisionSphereRadius: Single read FCollisionSphereRadius write FCollisionSphereRadius;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TPhysicsProperties = class;

  TSceneManagerWorld = TCastleAbstractRootTransform deprecated 'use TCastleRootTransform';

  { Root of transformations and scenes (tree of TCastleTransform and TCastleScene).
    This is the base abstract class, the non-abstract descendant is @link(TCastleRootTransform). }
  TCastleAbstractRootTransform = class(TCastleTransform)
  strict private
    WasPhysicsStep: boolean;
    TimeAccumulator: TFloatTime;
    FEnablePhysics: boolean;
    FMoveLimit: TBox3D;
    FPhysicsProperties: TPhysicsProperties;
    UpdateFrameId: TFrameId;
    FTimeScale: Single;
    FPaused: boolean;
    FMainCamera: TCastleCamera;
    FMainCameraObserver: TFreeNotificationObserver;
    FInternalPressReleaseListeners: TCastleTransformList;
    procedure SetPaused(const Value: boolean);
    procedure SetMainCamera(const Value: TCastleCamera);
    procedure MainCameraFreeNotification(const Sender: TFreeNotificationObserver);
  private
    FKraftEngine: TKraft;
    { Create FKraftEngine, if not assigned yet. }
    procedure InitializePhysicsEngine;
    { Destroy everything related to physics, if present. }
    procedure DestroyPhysicsEngine;
    procedure RegisterPressRelease(const T: TCastleTransform);
    procedure UnregisterPressRelease(const T: TCastleTransform);
  public
    OnCursorChange: TNotifyEvent;

    { Event to render whole world.
      Used by generated textures to update their contents.
      @exclude }
    InternalRenderEverythingEvent: TRenderFromViewFunction;

    { Projection near/far used. May be used when updating generated textures,
      to determine their projection parameters.
      @exclude }
    InternalProjectionNear, InternalProjectionFar: Single;

    { Changes every time MainCamera vectors change.
      Allows to track camera changes in scenes.
      @exclude }
    InternalMainCameraStateId: TFrameId;

    { Latest frame when VisibleChangeNotification was called (with any params, even empty). }
    InternalVisibleStateId: TFrameId;
    { Latest frame when VisibleChangeNotification was called (with vcVisibleGeometry). }
    InternalVisibleGeometryStateId: TFrameId;
    { Latest frame when VisibleChangeNotification was called (with vcVisibleNonGeometry). }
    InternalVisibleNonGeometryStateId: TFrameId;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { @exclude
      TCastleTransform instances that listen on press/release.
      May be @nil, equivalent to empty. }
    property InternalPressReleaseListeners: TCastleTransformList read FInternalPressReleaseListeners;

    { The major axis of gravity vector: 0, 1 or 2.
      This is trivially derived from the known camera
      GravityUp value. It can only truly express
      GravityUp vector values (1,0,0) or (0,1,0) or (0,0,1),
      although in practice this is enough for normal games (normal 3D scenes
      use up either +Y or +Z).

      We try to avoid using it in
      the engine, and use full GravityUp vector wherever possible.
      Full GravityUp vector may allow for more fun with weird gravity
      in future games. }
    function GravityCoordinate: Integer;

    function GravityUp: TVector3;
      // TODO: I would like to deprecate it,
      // but it is so often useful, and the alternative MainCamera.GravityUp looks convoluted.
      // Leave it be for now.
      // TODO: deprecated 'use MainCamera.GravityUp if MainCamera <> nil';

    { Is the move from OldPos to ProposedNewPos possible.
      Returns true and sets NewPos if some move is allowed, thus allows for wall-sliding.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself".

      @seealso TCastleTransform.MoveAllowed }
    function WorldMoveAllowed(
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; overload;

    { Is the move from OldPos to NewPos possible.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself".

      @seealso TCastleTransform.MoveAllowed }
    function WorldMoveAllowed(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; overload;

    { Get height of point APosition above the world.
      Looks at current @link(MainCamera) to know the gravity direction.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself". }
    function WorldHeight(const APosition: TVector3;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean;

    { Check that the line segment between 2 points that not collide with anything
      (that has opaque material).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldLineOfSight(const Pos1, Pos2: TVector3): boolean;

    { What is hit by this ray.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldRay(const RayOrigin, RayDirection: TVector3): TRayCollision;

    { What is hit by this ray.
      Returns the TCastleTransform that is hit (this is the "leaf" TCastleTransform
      in the TCastleTransform tree that is hit)
      and a distance from RayOrigin to the hit point.
      Returns @nil (Distance is undefined in this case) if nothing was hit.
      Use @link(WorldRay) for a more advanced version of this, with more complicated result.
      @groupBegin }
    function WorldRayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
    function WorldRayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
    { @groupEnd }

    { Check whether something collides with axis-aligned box in 3D.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldBoxCollision(const Box: TBox3D): boolean;

    { Check whether something collides with a line segment.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldSegmentCollision(const Pos1, Pos2: TVector3): boolean;

    { Check whether something collides with a sphere.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldSphereCollision(const Pos: TVector3; const Radius: Single): boolean;

    { Check whether something collides with a sphere in 2D
      (a circle, extruded to infinity along the Z axis).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldSphereCollision2D(const Pos: TVector2; const Radius: Single;
      const Details: TCollisionDetails = nil): boolean;

    { Check whether something collides with a point in 2D
      (which is an infinite line along the Z axis in 3D).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true.
      To be more exact, it checks @link(GetCollides) virtual method,
      which by default returns @link(Collides) property and @link(GetExists),
      and @link(GetExists) in turn by default checks @link(Exists) property
      and whether the object is not between @link(Disable) and @link(Enable). }
    function WorldPointCollision2D(const Point: TVector2): boolean;

    { Yoo can temporarily disable physics (no transformations will be updated
      by the physics engine) by setting this property to @false. }
    property EnablePhysics: boolean read FEnablePhysics write FEnablePhysics
      default true;

    { Limit the movement allowed by @link(WorldMoveAllowed).
      Ignored when empty (default).

      This property allows to easily limit the possible places
      where player and creatures go.
      Player is honoring this if it uses @link(WorldMoveAllowed),
      in particular our @link(TCastleWalkNavigation) navigation honors it.
      Creatures honor it if they use @link(WorldMoveAllowed)
      for their decision,
      in particular all creatures in @link(CastleCreatures) use it.

      Note that the @link(TLevel.Load) always
      assigns this property to be non-empty.
      It either determines it by CasMoveLimit placeholder
      in the level 3D model, or by calculating
      to include level bounding box + some space for flying.
    }
    property MoveLimit: TBox3D read FMoveLimit write FMoveLimit;

    { Central camera, that controls the features that require
      a single "main" camera (features that do not make sense
      with multiple cameras from multiple viewports).

      This camera controls:

      - the X3D nodes that "sense" camera like ProximitySensor, Billboard, LOD.
      - an audio listener (controlling the spatial sound).
      - the headlight position/direction.
      - when X3D nodes change Viewport/NavigationInfo,
        they apply these changes to this camera.

      Note that it means that "headlight" is assigned to one camera
      in case of multiple viewports looking at the same world.
      You cannot have a different "headlight" in each viewport,
      this would cause subtle problems since it's not how it would work in reality
      (where every light is visible in all viewports),
      e.g. mirror textures (like GeneratedCubeMapTexture)
      would need different contents in different viewpoints.

      Note that features like LOD or Billboard or ProximitySensor
      need to transform this camera to scene local space.
      Which is not possible if the same scene instance
      is used multiple times (e.g. under many different TCastleTransform parents).
      If you need to use these features of TCastleScene,
      then simply do not use the same scene reference multiple times
      (instead clone the scene by @link(TCastleScene.Clone)).

      By default this is set to @link(TCastleViewport.Camera) of the @link(TCastleViewport)
      that created this @link(TCastleAbstractRootTransform) instance.
      So in simple cases (when you just create one @link(TCastleViewport)
      and add your scenes to it's already-created @link(TCastleViewport.Items))
      you don't have to do anything, it just works.
      In general, you can change this to any camera of any associated @link(TCastleViewport),
      or @nil (in case no camera should be that "central" camera). }
    property MainCamera: TCastleCamera read FMainCamera write SetMainCamera;

    function CameraPosition: TVector3; deprecated 'use MainCamera.Position if MainCamera <> nil';
    function CameraDirection: TVector3; deprecated 'use MainCamera.Direction if MainCamera <> nil';
    function CameraUp: TVector3; deprecated 'use MainCamera.Up if MainCamera <> nil';
    function CameraGravityUp: TVector3; deprecated 'use MainCamera.GravityUp if MainCamera <> nil';
    function CameraKnown: Boolean; deprecated 'use MainCamera <> nil';
  published
    { Adjust physics properties. }
    property PhysicsProperties: TPhysicsProperties read FPhysicsProperties;

    { Time scale used when not @link(Paused). }
    property TimeScale: Single read FTimeScale write FTimeScale default 1;

    { Pausing means that no events (key, mouse, update) are processed.
      So time doesn't move, and input is not processed.

      Navigation also doesn't work (this part is implemented by TCastleViewport
      and each TCastleNavigation).

      This is useful if you want to unconditionally make your world temporary
      still (for example, useful when entering some modal dialog box
      and you want the world to behave as a still background).

      @italic(See also): For other pausing methods,
      there are other methods of pausing / disabling
      some events processing for the world:

      @unorderedList(
        @item(You can set TCastleScene.TimePlaying to @false.
          This is roughly equivalent to not running their @link(Update) methods.
          This means that time will "stand still" for them,
          so their animations will not play. Although they may
          still react and change in response to mouse clicks / key presses,
          if TCastleScene.ProcessEvents.)

        @item(You can set TCastleScene.ProcessEvents to @false.
          This means that scene will not receive and process any
          key / mouse and other events (through VRML/X3D sensors).
          Some animations (not depending on VRML/X3D events processing)
          may still run, for example MovieTexture will still animate,
          if only TCastleScene.TimePlaying.)

        @item(For navigation, you can set @code(TCastleNavigation.Input := []) to ignore
          key / mouse clicks.

          Or you can set @code(TCastleNavigation.Exists) to @false,
          this is actually equivalent to what pausing does now for TCastleNavigation.
        )
      ) }
    property Paused: boolean read FPaused write SetPaused default false;
  end;

  {$define read_interface}
  {$I castletransform_physics.inc}
  {$undef read_interface}

procedure TransformMatricesMult(var Transform, InverseTransform: TMatrix4;
  const Center: TVector3;
  const Rotation: TVector4;
  const Scale: TVector3;
  const ScaleOrientation: TVector4;
  const Translation: TVector3);
  deprecated 'use TTransformation.Multiply';

const
  rfOffScreen = rfRenderedTexture deprecated 'use rfRenderedTexture';

function StrToOrientationType(const S: String): TOrientationType;

implementation

uses CastleLog, CastleQuaternions, CastleComponentSerialize, X3DTriangles;

{$define read_implementation}
{$I castletransform_physics.inc}
{$I castletransform_collisions.inc}
{$I castletransform_renderparams.inc}
{$I castletransform_behavior.inc}
{$undef read_implementation}

{ TransformMatricesMult ------------------------------------------------------ }

{ Workaround FPC bug on Darwin for AArch64 (not on other platforms),
  causes "Fatal: Internal error 2014121702".
  Occurs with 3.0.4 and with 3.3.1 (r44333 from 2020/03/22). }
{$if defined(DARWIN) and defined(CPUAARCH64)}
  {$define COMPILER_BUGGY_PARAMETERS}
{$endif}

procedure TransformMatricesMult(var Transform, InverseTransform: TMatrix4;
  const Center: TVector3;
  const Rotation: TVector4;
  const Scale: TVector3;
  const ScaleOrientation: TVector4;
  const Translation: TVector3);

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
{$endif COMPILER_BUGGY_PARAMETERS}

var
  T: TTransformation;
begin
  T.Transform := Transform;
  T.InverseTransform := InverseTransform;
  // T.Scale := 1; // doesn't matter
{$ifdef COMPILER_BUGGY_PARAMETERS}
  TransformData.Center := Center;
  TransformData.Rotation := Rotation;
  TransformData.Scale := Scale;
  TransformData.ScaleOrientation := ScaleOrientation;
  TransformData.Translation := Translation;
  MultiplyWorkaround(T, TransformData);
{$else}
  T.Multiply(
    Center,
    Rotation,
    Scale,
    ScaleOrientation,
    Translation);
{$endif}
  Transform := T.Transform;
  InverseTransform := T.InverseTransform;
end;

{ TRayCollision --------------------------------------------------------------- }

function TRayCollision.IndexOfItem(const Item: TCastleTransform): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].Item = Item then Exit;
  Result := -1;
end;

function TRayCollision.IndexOfItem(const ItemClass: TCastleTransformClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].Item is ItemClass then Exit;
  Result := -1;
end;

{ TCollisionDetails ------------------------------------------------------------ }

function TCollisionDetails.IndexOfItem(const Item: TCastleTransform): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].Item = Item then Exit;
  Result := -1;
end;

procedure TCollisionDetails.Add(const Item: TCastleTransform);
var
  NewItem: PCollisionDetailsItem;
begin
  NewItem := inherited Add();
  NewItem^.Item := Item;
end;

{ TCastleTransformList ------------------------------------------------------------ }

procedure TCastleTransformList.Notify(Ptr: Pointer; Action: TListNotification);
var
  B: TCastleTransform;
begin
  inherited;

  if Owner <> nil then
  begin
    B := TCastleTransform(Ptr);

    case Action of
      lnAdded:
        begin
          { assign FParent before calling AddToWorld
            (that may cause PhysicsChangeWorldAttach that may use Parent). }
          B.FParent := Owner;
          if Owner.World <> nil then
            B.AddToWorld(Owner.World);
          { Register Owner to be notified of item destruction. }
          B.FreeNotification(Owner);
        end;
      lnExtracted, lnDeleted:
        begin
          if (Owner.World <> nil) and
             { It is possible that "B.World <> Owner.World" when B is getting
               freed and has World set to nil in constructor already. }
             (B.World = Owner.World) then
            B.RemoveFromWorld(Owner.World);
          { It may seem that setting Parent to nil is not really needed,
            nothing should use it when FWorldReferences <> 1.
            But what if FWorldReferences was > 1
            but then it goes back FWorldReferences = 1, we don't want to suddenly
            use FParent that was not watched (it may be invalid reference by now). }
          B.FParent := nil;
          { Do not call RemoveFreeNotification when Owner is equal to B.World,
            this would prevent getting notification when to nil FWorld in
            TCastleTransform.Notification. }
          if B.World <> Owner then
            B.RemoveFreeNotification(Owner);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTransformList.Notify action?');
      {$endif}
    end;

    if (Owner.World <> nil) and Assigned(Owner.World.OnCursorChange) then
      Owner.World.OnCursorChange(Owner);
  end;
end;

function TCastleTransformList.GetItem(const I: Integer): TCastleTransform;
begin
  Result := TCastleTransform(inherited Items[I]);
end;

procedure TCastleTransformList.SetItem(const I: Integer; const Item: TCastleTransform);
begin
  inherited Items[I] := Item;
end;

function TCastleTransformList.First: TCastleTransform;
begin
  Result := (inherited First) as TCastleTransform;
end;

function TCastleTransformList.Last: TCastleTransform;
begin
  Result := (inherited Last) as TCastleTransform;
end;

{ TCastleTransform.TEnumerator ------------------------------------------------- }

function TCastleTransform.TEnumerator.GetCurrent: TCastleTransform;
begin
  Result := FList[FPosition];
end;

constructor TCastleTransform.TEnumerator.Create(AList: TCastleTransformList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TCastleTransform.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TCastleTransform ---------------------------------------------------------------- }

const
  NoScale: TVector3 = (Data: (1, 1, 1));

constructor TCastleTransform.Create(AOwner: TComponent);
begin
  inherited;
  FCastShadowVolumes := true;
  FExists := true;
  FCollides := true;
  FPickable := true;
  FVisible := true;
  FCursor := mcDefault;
  FList := TCastleTransformList.Create(false);
  FList.FOwner := Self; // get notified about add/release, assign children World, Parent
  FBehaviors := TComponentList.Create(false);
  FMiddleHeight := DefaultMiddleHeight;
  FOnlyTranslation := true;
  FScale := NoScale;
  FOrientation := DefaultOrientation;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleTransform.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  PhysicsDestroy;
  FreeAndNil(FList);
  FreeAndNil(FBehaviors);
  { set to nil, to detach free notification }
  ChangeWorld(nil);
  GLContextClose;
  inherited;
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

function TCastleTransform.GetExists: boolean;
begin
  Result := FExists and (Disabled = 0);
end;

function TCastleTransform.GetCollides: boolean;
begin
  Result := FCollides and GetExists;
end;

function TCastleTransform.GetPickable: boolean;
begin
  Result := FPickable and GetExists;
end;

function TCastleTransform.GetVisible: boolean;
begin
  Result := FVisible and GetExists;
end;

function TCastleTransform.Sphere(out Radius: Single): boolean;
begin
  Radius := CollisionSphereRadius;
  Result := Radius <> 0;
end;

procedure TCastleTransform.Disable;
begin
  Inc(Disabled);
end;

procedure TCastleTransform.Enable;
begin
  Dec(Disabled);
end;

procedure TCastleTransform.AddToWorld(const Value: TCastleAbstractRootTransform);
var
  I: Integer;
begin
  Assert(Value <> nil);
  if FWorld <> Value then
  begin
    if FWorld <> nil then
      raise ECannotAddToAnotherWorld.Create('Cannot add object existing in one TCastleRootTransform to another. This usually means that your object is part of "Viewport1.Items", and you are adding it to "Viewport2.Items". You have to remove it from "Viewport1.Items" first, or set both "Viewport1.Items" and "Viewport2.Items" to be equal.');
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
    WritelnWarning('TCastleTransform.RemoveFromWorld: Removing from World you were not part of. This probably means that you added one TCastleTransform instance to multiple TCastleRootTransform trees, which is not allowed. Always remove TCastleTransform from previous viewport (e.g. by "Viewport1.Items.Remove(xxx)") before adding to the new viewport.');

  Dec(FWorldReferences);
  if FWorldReferences = 0 then
    ChangeWorld(nil);

  // list stuff
  if FList <> nil then // when one list is within another, this may be called during own destruction by TCastleTransformList.Notify
    for I := 0 to List.Count - 1 do
      List[I].RemoveFromWorld(Value);
end;

procedure TCastleTransform.ChangeWorld(const Value: TCastleAbstractRootTransform);
begin
  if FWorld <> Value then
  begin
    if FWorld <> nil then
    begin
      if ListenPressRelease then
        FWorld.UnregisterPressRelease(Self);

      PhysicsChangeWorldDetach;

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

      PhysicsChangeWorldAttach;

      if ListenPressRelease then
        FWorld.RegisterPressRelease(Self);
    end;

    // otherwise changing TCastleSceneCore.ExposeTransforms would not update CGE editor hierarchy view
    if not (csTransient in ComponentStyle) then
      InternalCastleDesignInvalidate := true;
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

  PhysicsNotification(AComponent, Operation);
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
begin
  MyPositionWorld := UniqueParent.LocalToWorld(MyPosition);
  Disable;
  try
    Result := World.WorldHeight(MyPositionWorld, AboveHeight, AboveGround);
    if Result then
      AboveHeight := UniqueParent.WorldToLocalDistance(AboveHeight);
  finally Enable end;
end;

function TCastleTransform.LineOfSight(const Pos1, Pos2: TVector3): boolean;
var
  Pos1World, Pos2World: TVector3;
begin
  Pos1World := UniqueParent.LocalToWorld(Pos1);
  Pos2World := UniqueParent.LocalToWorld(Pos2);
  Disable;
  try
    Result := World.WorldLineOfSight(Pos1World, Pos2World);
  finally Enable end;
end;

function TCastleTransform.MoveAllowed(const OldPos, ProposedNewPos: TVector3;
  out NewPos: TVector3;
  const BecauseOfGravity: boolean): boolean;
var
  Sp: boolean;
  SpRadius, SpRadiusWorld: Single;
  OldBox, NewBox: TBox3D;
  OldPosWorld, ProposedNewPosWorld, NewPosWorld: TVector3;
begin
  Assert(UniqueParent <> nil, 'Need to know world transformation before MoveAllowed');

  OldPosWorld := UniqueParent.LocalToWorld(OldPos);
  ProposedNewPosWorld := UniqueParent.LocalToWorld(ProposedNewPos);

  { Save bounding volume information before calling Disable, as Disable makes
    bounding volume empty }
  Sp := Sphere(SpRadius);
  if not Sp then
    SpRadius := 0; { something predictable, for safety }
  OldBox := WorldBoundingBox;
  NewBox := OldBox.Translate(ProposedNewPosWorld - OldPosWorld);

  SpRadiusWorld := UniqueParent.LocalToWorldDistance(SpRadius);

  Disable;
  try
    Result := World.WorldMoveAllowed(OldPosWorld, ProposedNewPosWorld, NewPosWorld,
      Sp, SpRadiusWorld, OldBox, NewBox, BecauseOfGravity);
  finally Enable end;

  if Result then
    NewPos := UniqueParent.WorldToLocal(NewPosWorld);
end;

function TCastleTransform.MoveAllowed(const OldPos, NewPos: TVector3;
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
  OldBox := WorldBoundingBox;
  NewBox := OldBox.Translate(NewPos - OldPos);

  Disable;
  try
    Result := World.WorldMoveAllowed(OldPos, NewPos,
      Sp, SpRadius, OldBox, NewBox, BecauseOfGravity);
  finally Enable end;
end;

function TCastleTransform.Ray(
  const RayOrigin, RayDirection: TVector3): TRayCollision;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
begin
  RayOriginWorld := UniqueParent.LocalToWorld(RayOrigin);
  RayDirectionWorld := UniqueParent.LocalToWorldDirection(RayDirection);
  Disable;
  try
    Result := World.WorldRay(RayOriginWorld, RayDirectionWorld);
  finally Enable end;
end;

function TCastleTransform.RayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
begin
  RayOriginWorld := UniqueParent.LocalToWorld(RayOrigin);
  RayDirectionWorld := UniqueParent.LocalToWorldDirection(RayDirection);
  Disable;
  try
    Result := World.WorldRayCast(RayOriginWorld, RayDirectionWorld);
  finally Enable end;
end;

function TCastleTransform.RayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
begin
  RayOriginWorld := UniqueParent.LocalToWorld(RayOrigin);
  RayDirectionWorld := UniqueParent.LocalToWorldDirection(RayDirection);
  Disable;
  try
    Result := World.WorldRayCast(RayOriginWorld, RayDirectionWorld, Distance);
    if Result <> nil then
      Distance := UniqueParent.WorldToLocalDistance(Distance);
  finally Enable end;
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
  Result := List.Count;
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

procedure TCastleTransform.Clear;
begin
  List.Clear;
end;

procedure TCastleTransform.Exchange(const Index1, Index2: Integer);
begin
  List.Exchange(Index1, Index2);
end;

function CompareBackToFront2D(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(TCastleTransform(A).BoundingBox, TCastleTransform(B).BoundingBox);
end;

var
  { Has to be global, since TObjectList.Sort
    requires normal function (not "of object"). }
  SortCameraPosition: TVector3;

function CompareBackToFront3D(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(TCastleTransform(A).BoundingBox, TCastleTransform(B).BoundingBox,
    SortCameraPosition);
end;

procedure TCastleTransform.SortBackToFront(const BlendingSort: TBlendingSort;
  const CameraPosition: TVector3);
begin
  case BlendingSort of
    bs2D: List.Sort(@CompareBackToFront2D);
    bs3D:
      begin
        SortCameraPosition := CameraPosition;
        List.Sort(@CompareBackToFront3D);
      end;
    else ;
  end;
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
  if GetExists then
  begin
    for I := 0 to List.Count - 1 do
      if not List[I].InternalExcludeFromParentBoundingVolume then
        Result.Include(List[I].BoundingBox);
  end;
end;

procedure TCastleTransform.LocalRender(const Params: TRenderParams);
var
  I: Integer;
begin
  if GetVisible then
  begin
    for I := 0 to List.Count - 1 do
      List[I].Render(Params);
  end;
end;

procedure TCastleTransform.LocalRenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4);
var
  I: Integer;
begin
  if GetExists and CastShadowVolumes then
  begin
    for I := 0 to List.Count - 1 do
      List[I].RenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform);
  end;
end;

procedure TCastleTransform.PrepareResources(const Options: TPrepareResourcesOptions;
  const ProgressStep: boolean; const Params: TPrepareParams);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].PrepareResources(Options, ProgressStep, Params);
end;

function TCastleTransform.PrepareResourcesSteps: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to List.Count - 1 do
    Result := Result + List[I].PrepareResourcesSteps;
end;

function TCastleTransform.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := false;
  if not GetExists then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].Press(Event) then Exit(true);
end;

function TCastleTransform.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := false;
  if not GetExists then Exit;

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
  const Distance: Single): boolean;
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

begin
  if GetExists then
  begin
    UpdateBehaviors;
    UpdateChildren;

    { Disable physics and gravity in design mode (in the future we may add optional way to enable them) }
    if not CastleDesignMode then
    begin
      UpdateSimpleGravity(SecondsPassed);
      UpdatePhysicsEngine(SecondsPassed);
    end;
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

function TCastleTransform.InverseTransform: TMatrix4;
begin
  if not FTransformationValid then
  begin
    InternalTransformation(FTransformation);
    FTransformationValid := true;
  end;
  Result := FTransformation.InverseTransform;
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

function TCastleTransform.Parent: TCastleTransform;
begin
  if FWorldReferences <> 1 then
  begin
    if FWorldReferences = 0 then
      raise ENotAddedToWorld.Create('Parent (and WorldTransform) not available: This instance is not yet added to Viewport.Items')
    else
      raise EMultipleReferencesInWorld.Create('Parent (and WorldTransform) not available: This instance is added multiple times to Viewport.Items, it does not have a single Parent value');
  end;
  if FParent = nil then
  begin
    if Self = World then
      raise ETransformParentUndefined.Create('Parent not available: This is the root node (World)')
    else
      raise ETransformParentUndefined.Create('Parent (and WorldTransform) not available: This instance was once added multiple times to Viewport.Items (it lost link to Parent)');
  end;
  Result := FParent;
end;

function TCastleTransform.HasWorldTransform: boolean;
begin
  Result := (Self = World) or ((FWorldReferences = 1) and (FParent <> nil));
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
    Par := Parent;
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

procedure TCastleTransform.InternalTransformation(out T: TTransformation);
begin
  T.Init;
  InternalTransformationMult(T);
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
    'Inverse transform matrix has NaN value inside:' + NL +
    '%s' + NL +
    '  Matrix source: Center %s, Rotation %s, Scale %s, ScaleOrientation %s, Translation %s',
    [NewParamsInverseTransformValue.ToString('  '),
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
  OldParamsTransformIdentity: boolean;
  OldFrustum: PFrustum;
  NewFrustumValue: TFrustum;
begin
  T := Translation;
  if FOnlyTranslation and T.IsZero then
    LocalRender(Params)
  else
  begin
    FrameProfiler.Start(fmRenderTransform);

    OldParamsTransformIdentity := Params.TransformIdentity;
    OldParamsTransform         := Params.Transform;
    OldParamsInverseTransform  := Params.InverseTransform;
    OldFrustum                 := Params.Frustum;

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

    FrameProfiler.Stop(fmRenderTransform);

    LocalRender(Params);

    FrameProfiler.Start(fmRenderTransform);
    { Restore OldXxx values.
      They can be restored fast, thanks to using pointers to matrix/frustum. }
    Params.TransformIdentity := OldParamsTransformIdentity;
    Params.Transform         := OldParamsTransform;
    Params.InverseTransform  := OldParamsInverseTransform;
    Params.Frustum           := OldFrustum;
    FrameProfiler.Stop(fmRenderTransform);
  end;
end;

procedure TCastleTransform.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4);
var
  T: TVector3;
begin
  if FOnlyTranslation then
  begin
    T := Translation;
    if T.IsZero then
      LocalRenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform)
    else
      LocalRenderShadowVolume(ShadowVolumeRenderer,
        false, TranslationMatrix(T) * ParentTransform);
  end else
    LocalRenderShadowVolume(ShadowVolumeRenderer,
      false, Transform * ParentTransform);
end;

function Bottom(const Gravity: boolean; const GravityCoordinate: Integer;
  const BoundingBox: TBox3D): Single;
begin
  if Gravity then
    Result := 0 else
    Result := BoundingBox.Data[0].Data[GravityCoordinate];
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
  Result.Data[GC] := Result.Data[GC] + (Bottom(Gravity, GC, B) + PreferredHeight);
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
    Result := MiddleHeight * (B.Data[1].Data[GC] - Bottom(Gravity, GC, B));

  {$ifdef CHECK_HEIGHT_VS_RADIUS}
  if Sphere(R) and (R > Result) then
  begin
    WritelnWarning('3D Radius',
      Format('PreferredHeight %f is smaller than radius %f. Gravity may work weird for this 3D object.',
      [Result, R]));
  end;
  {$endif}
end;

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

procedure TCastleTransform.ChangedTransform;
begin
  FTransformationValid := false;
  FWorldTransformationValid := false;
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure TCastleTransform.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.Count - 1 do
    if [csSubComponent, csTransient] * List[I].ComponentStyle = [] then
      Proc(List[I]);
end;

procedure TCastleTransform.InternalAddChild(const C: TComponent);
begin
  // matches TCastleTransform.GetChildren implementation
  Add(C as TCastleTransform)
end;

function TCastleTransform.PropertySection(const PropertyName: String
  ): TPropertySection;
begin
  case PropertyName of
    'Exists':
      Result := psBasic;
    'CenterPersistent',
    'RotationPersistent',
    'ScalePersistent',
    'ScaleOrientationPersistent',
    'TranslationPersistent':
      Result := psLayout;
    else
      Result := inherited PropertySection(PropertyName);
  end;
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
  FCenter := Value;
  FOnlyTranslation := FOnlyTranslation and Value.IsPerfectlyZero;
  ChangedTransform;
end;

procedure TCastleTransform.SetRotation(const Value: TVector4);
begin
  FRotation := Value;
  FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
  ChangedTransform;
end;

procedure TCastleTransform.SetScale(const Value: TVector3);
begin
  FScale := Value;
  FOnlyTranslation := FOnlyTranslation and
    (Value[0] = 1) and (Value[1] = 1) and (Value[2] = 1);
  ChangedTransform;
end;

procedure TCastleTransform.SetScaleOrientation(const Value: TVector4);
begin
  FScaleOrientation := Value;
  FOnlyTranslation := FOnlyTranslation and (Value[3] = 0);
  ChangedTransform;
end;

function TCastleTransform.GetTranslation: TVector3;
begin
  Result := FTranslation;
end;

procedure TCastleTransform.SetTranslation(const Value: TVector3);
begin
  FTranslation := Value;
  ChangedTransform;
end;

function TCastleTransform.GetTranslationXY: TVector2;
begin
  Result := Translation.XY;
end;

procedure TCastleTransform.SetTranslationXY(const Value: TVector2);
begin
  Translation := Vector3(Value, Translation.Data[2]);
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

function TCastleTransform.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FList);
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

procedure TCastleTransform.AddBehavior(const Behavior: TCastleBehavior);
begin
  if Behavior.FParent <> Self then
  begin
    FBehaviors.Add(Behavior);
    Behavior.FParent := Self;
    Behavior.ParentChanged; // call at end, when state is consistent
  end;
end;

procedure TCastleTransform.RemoveBehaviorIndex(const BehaviorIndex: Integer);
var
  Beh: TCastleBehavior;
begin
  Beh := FBehaviors[BehaviorIndex] as TCastleBehavior;
  Beh.FParent := nil;
  FBehaviors.Delete(BehaviorIndex);
  Beh.ParentChanged; // call at end, when state is consistent
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

function TCastleTransform.FindRequiredBehavior(const BehaviorClass: TCastleBehaviorClass): TCastleBehavior;
begin
  Result := FindBehavior(BehaviorClass);
  if Result = nil then
  begin
    Result := BehaviorClass.Create(Self);
    AddBehavior(Result);
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastletransform_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleAbstractRootTransform ------------------------------------------------------------------- }

constructor TCastleAbstractRootTransform.Create(AOwner: TComponent);
begin
  inherited;

  FPhysicsProperties := TPhysicsProperties.Create(Self);
  FPhysicsProperties.SetSubComponent(true);
  FPhysicsProperties.Name := 'PhysicsProperties';
  FPhysicsProperties.RootTransform := Self;

  FMainCameraObserver := TFreeNotificationObserver.Create(Self);
  FMainCameraObserver.OnFreeNotification := @MainCameraFreeNotification;

  FTimeScale := 1;
  FMoveLimit := TBox3D.Empty;
  FEnablePhysics := true;

  { everything inside is part of this world }
  AddToWorld(Self);
end;

destructor TCastleAbstractRootTransform.Destroy;
begin
  DestroyPhysicsEngine;
  FreeAndNil(FInternalPressReleaseListeners);
  inherited;
end;

function TCastleAbstractRootTransform.GravityCoordinate: Integer;
begin
  Result := MaxAbsVectorCoord(GravityUp);
end;

function TCastleAbstractRootTransform.WorldBoxCollision(const Box: TBox3D): boolean;
begin
  Result := BoxCollision(Box, nil);
end;

function TCastleAbstractRootTransform.WorldSegmentCollision(const Pos1, Pos2: TVector3): boolean;
begin
  Result := SegmentCollision(Pos1, Pos2, nil, false);
end;

function TCastleAbstractRootTransform.WorldSphereCollision(const Pos: TVector3;
  const Radius: Single): boolean;
begin
  Result := SphereCollision(Pos, Radius, nil);
end;

function TCastleAbstractRootTransform.WorldSphereCollision2D(const Pos: TVector2;
  const Radius: Single;
  const Details: TCollisionDetails): boolean;
begin
  Result := SphereCollision2D(Pos, Radius, nil, Details);
end;

function TCastleAbstractRootTransform.WorldPointCollision2D(const Point: TVector2): boolean;
begin
  Result := PointCollision2D(Point, nil);
end;

function TCastleAbstractRootTransform.WorldHeight(const APosition: TVector3;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
begin
  Result := HeightCollision(APosition, GravityUp, nil,
    AboveHeight, AboveGround);
end;

function TCastleAbstractRootTransform.WorldLineOfSight(const Pos1, Pos2: TVector3): boolean;
begin
  Result := not SegmentCollision(Pos1, Pos2,
    { Ignore transparent materials, this means that creatures can see through
      glass --- even though they can't walk through it. }
    @TBaseTrianglesOctree(nil).IgnoreTransparentItem,
    true);
end;

function TCastleAbstractRootTransform.WorldRay(
  const RayOrigin, RayDirection: TVector3): TRayCollision;
begin
  Result := RayCollision(RayOrigin, RayDirection, nil);
end;

function TCastleAbstractRootTransform.WorldRayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
var
  RayColl: TRayCollision;
begin
  Result := nil;
  Distance := 0; // just to make it defined

  RayColl := WorldRay(RayOrigin, RayDirection);
  if RayColl <> nil then
  try
    if RayColl.Count <> 0 then
    begin
      Result := RayColl.First.Item;
      Distance := RayColl.Distance;
    end;
  finally FreeAndNil(RayColl) end;
end;

function TCastleAbstractRootTransform.WorldRayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
var
  IgnoredDistance: Single;
begin
  Result := WorldRayCast(RayOrigin, RayDirection, IgnoredDistance);
end;

function TCastleAbstractRootTransform.WorldMoveAllowed(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, ProposedNewPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, nil);
  if Result then
    Result := MoveLimit.IsEmpty or MoveLimit.Contains(NewPos);
end;

function TCastleAbstractRootTransform.WorldMoveAllowed(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, nil);
  if Result then
    Result := MoveLimit.IsEmpty or MoveLimit.Contains(NewPos);
end;

procedure TCastleAbstractRootTransform.SetPaused(const Value: boolean);
begin
  if FPaused <> Value then
  begin
    FPaused := Value;
    { TODO: update the viewport cursor when Paused changed. }
    // RecalculateCursor(Self);
  end;
end;

procedure TCastleAbstractRootTransform.SetMainCamera(const Value: TCastleCamera);
begin
  if FMainCamera <> Value then
  begin
    FMainCameraObserver.Observed := Value;
    FMainCamera := Value;
    VisibleChangeHere([]);
  end;
end;

procedure TCastleAbstractRootTransform.MainCameraFreeNotification(const Sender: TFreeNotificationObserver);
begin
  MainCamera := nil;
end;

function TCastleAbstractRootTransform.CameraPosition: TVector3;
begin
  if MainCamera = nil then
    Result := TVector3.Zero
  else
    Result := MainCamera.Position;
end;

function TCastleAbstractRootTransform.CameraDirection: TVector3;
begin
  if MainCamera = nil then
    Result := DefaultCameraDirection
  else
    Result := MainCamera.Direction;
end;

function TCastleAbstractRootTransform.CameraUp: TVector3;
begin
  if MainCamera = nil then
    Result := DefaultCameraUp
  else
    Result := MainCamera.Up;
end;

function TCastleAbstractRootTransform.CameraGravityUp: TVector3;
begin
  if MainCamera = nil then
    Result := DefaultCameraUp
  else
    Result := MainCamera.GravityUp;
end;

function TCastleAbstractRootTransform.GravityUp: TVector3;
begin
  if MainCamera = nil then
    { This is only to keep deprecated GravityUp/GravityCoordinate
      sensible, even for old code that doesn't check MainCamera <> nil. }
    Result := DefaultCameraUp
  else
    Result := MainCamera.GravityUp;
end;

function TCastleAbstractRootTransform.CameraKnown: Boolean;
begin
  Result := MainCamera <> nil;
end;

procedure TCastleAbstractRootTransform.RegisterPressRelease(const T: TCastleTransform);
begin
  if FInternalPressReleaseListeners = nil then
    FInternalPressReleaseListeners := TCastleTransformList.Create(false);
  FInternalPressReleaseListeners.Add(T);
end;

procedure TCastleAbstractRootTransform.UnregisterPressRelease(const T: TCastleTransform);
var
  I: Integer;
begin
  if FInternalPressReleaseListeners = nil then
    I := -1
  else
    I := FInternalPressReleaseListeners.IndexOf(T);

  if I = -1 then
  begin
    WritelnWarning('Transformation called UnregisterPressRelease, but it was not listening to Press/Release');
    Exit;
  end;

  FInternalPressReleaseListeners.Delete(I);
end;

{ global routines ------------------------------------------------------------ }

const
  OrientationNames: array [TOrientationType] of String =  (
    'up:y,direction:-z',
    'up:y,direction:z',
    'up:z,direction:-y',
    'up:z,direction:x'
  );

function StrToOrientationType(const S: String): TOrientationType;
begin
  if S = 'default' then
    Exit(TCastleTransform.DefaultOrientation);
  for Result in TOrientationType do
    if OrientationNames[Result] = S then
      Exit;
  raise Exception.CreateFmt('Invalid orientation name "%s"', [S]);
end;

initialization
  TCastleTransform.DefaultOrientation := otUpYDirectionZ;
  GlobalIdentityMatrix := TMatrix4.Identity;
  RegisterSerializableComponent(TCastleTransform, 'Transform');
end.
