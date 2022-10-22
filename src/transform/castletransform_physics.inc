{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Physics. Integrates TCastleTransform with Kraft physics engine. }

{$ifdef read_interface}

  { Information send along with TCollisionEvent event, like
    TCastleRigidBody.OnCollisionEnter, TCastleRigidBody.OnCollisionExit. }
  TPhysicsCollisionDetails = record
  public
    Transforms: array[0..1] of TCastleTransform;
    function OtherTransform: TCastleTransform;
  end;

  { Configure physics simulation calculation. }
  TPhysicsProperties = class(TComponent)
  strict private
    FAngularVelocityRK4Integration: Boolean;
    FLinearVelocityRK4Integration: Boolean;
    FFrequency: Single;
    FGravityStrength: Single;
    FMaxPhysicsTicksPerUpdate: Cardinal;
    FUpdateCollidersScaleAtRuntime: Boolean;
    FContinuousCollisionDetection: Boolean;
    procedure SetAngularVelocityRK4Integration(const AValue: Boolean);
    procedure SetLinearVelocityRK4Integration(const AValue: Boolean);
    procedure SetFrequency(const AValue: Single);
    procedure SetUpdateCollidersScaleAtRuntime(const AValue: Boolean);
    procedure SetContinuousCollisionDetection(const AValue: Boolean);
  private
    { Buffer to not count physics step time per frame }
    FPhysicsTimeStep: TFloatTime;
    RootTransform: TCastleAbstractRootTransform;
  public
    const
      DefaultAngularVelocityRK4Integration = false;
      DefaultLinearVelocityRK4Integration = false;
      DefaultFrequency = 60.0;
      DefaultMaxPhysicsTicksPerUpdate = 5;
      DefaultGravityStrength = 9.81;
      DefaultUpdateCollidersScaleAtRuntime = true;
      DefaultContinuousCollisionDetection = true;
    constructor Create(AOwner: TComponent); override;
  published
    { Use a more precise (but also more expensive) method for simulating angular velocity.
      The "RK4" refers to Runge–Kutta method. }
    property AngularVelocityRK4Integration: Boolean read FAngularVelocityRK4Integration write SetAngularVelocityRK4Integration default DefaultAngularVelocityRK4Integration;

    { Use a more precise (but also more expensive) method for simulating linear velocity.
      The "RK4" refers to Runge–Kutta method. }
    property LinearVelocityRK4Integration: Boolean read FLinearVelocityRK4Integration write SetLinearVelocityRK4Integration default DefaultLinearVelocityRK4Integration;

    { How often should the physics simulation run in a second.
      Larger values increase accuracy (up to a certain point), but also increase the CPU overhead. }
    property Frequency: Single read FFrequency write SetFrequency {$ifdef FPC}default DefaultFrequency{$endif};

    { Physics gravity is set using the -GravityUp vector (from main camera)
      multiplied with GravityStrength. }
    property GravityStrength: Single read FGravityStrength write FGravityStrength {$ifdef FPC}default DefaultGravityStrength{$endif};

    { Non-zero value avoids the "spiral of death" when the physics takes
      a long time to calculate.

      When the value is zero, physics makes always as many steps as necessary,
      to advance the time by @code(1 / @link(Frequency)) steps.
      This means that if physics takes a long time to calculate,
      next time it will take even longer time to calculate (it will need more steps),
      thus we have a "spiral of death" that leads to lower and lower FPS. }
    property MaxPhysicsTicksPerUpdate: Cardinal read FMaxPhysicsTicksPerUpdate write FMaxPhysicsTicksPerUpdate default DefaultMaxPhysicsTicksPerUpdate;

    { When set to true it works like in editor - when you change scale,
      it changes colliders size, otherwise scale is updated only after attaching
      collider to world. }
    property UpdateCollidersScaleAtRuntime: Boolean read FUpdateCollidersScaleAtRuntime write SetUpdateCollidersScaleAtRuntime default DefaultUpdateCollidersScaleAtRuntime;

    { Should engine use continous collision detection (CCD). }
    property ContinuousCollisionDetection: Boolean read FContinuousCollisionDetection write SetContinuousCollisionDetection default DefaultContinuousCollisionDetection;
  end;

  { Abstract collider class, ancestor of @link(TCastleSphereCollider), @link(TCastleBoxCollider)
    and others. }
  TCastleCollider = class(TCastleBehavior)
  strict private
    { Implementation note:
      This doesn't store any references to Kraft objects.
      This only describes shape.
      TCastleRigidBody takes care of initializing Kraft. }
    FDensity, FMass, FRestitution, FFriction: Single;
    { When @false, behave as if Translation/Rotation/Scale/FLocalTransform/FLocalInverseTransform
      were all "identity", i.e. "no transformation".
      This is an often case, so we want to optimize it. }
    FTranslation: TVector3;
    FRotation: TVector4;
    FAutoSize: Boolean;
    FCalculatingSize: Boolean; //< are we inside CalculateSize or InternalAutoSize
  private
    FKraftShape: TKraftShape;  // owned by TKraftRigidBody
    FUseLocalTransform: Boolean;
    FLocalTransform: TMatrix4;
    FLocalInverseTransform: TMatrix4;

    { Updates FLocalTransformation and FUseLocalTransformation after changing
      Translation, Scale, Rotation and in InitializeKraft. }
    procedure UpdateLocalTransform;

    { Initializes Kraft shape returns false if initialization fails }
    function InitializeKraft(const APhysics: TKraft;
      const ACastleRigidBody: TCastleRigidBody): Boolean;

    procedure SetDensity(const AValue: Single);
    procedure SetMass(const AValue: Single);
    procedure SetRestitution(const AValue: Single);
    procedure SetFriction(const AValue: Single);
    procedure SetTranslation(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector4);
    procedure SetScale(const AValue: TVector3);
    procedure SetAutoSize(const AValue: Boolean);
    function StoredWithAutoSize: Boolean;
    function GetRigidBody: TCastleRigidBody;
  protected
    FLastUsedWorldScale: TVector3; // Last world scale used to change collider size
    FScale: TVector3; // Collider scale

    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;

    { Notify CastleRigidBody that kraft shape need be recreated }
    procedure ShapeChangedNotifyRigidBody;
    function ForceStaticBody: Boolean; virtual;

    procedure WorldAfterAttach; override;
    procedure WorldBeforeDetach; override;

    { Only one TCastleCollider behavior can be added to a given
      TCastleTransform so we check that here. }
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;

    { Updates scale after for example adding collider to world
      This procedure only updates FLastUsedWorldScale and calls
      ShapeChangedNotifyRigidBody. You should multiply collider size in
      your descendant CreateKraftShape(). }
    procedure UpdateScale(ForceUpdate: Boolean); overload;

    { Sets AutoSize = @false, unless this is called
      inside InternalAutoSize or at loading.
      Call this when something changes collider size explicitly,
      e.g. TCastleSphereCollider.Radius changes. }
    procedure DisableAutoSize;

    { Recalculate collider size.
      Do not be concerned about AutoSize here, just update collider properties.
      The default implementation resets Translation, Rotation, Scale. }
    procedure CalculateSizeCore; virtual;
  public
    const
      { Default for @link(Density). }
      DefaultDensity = 1.0;
      { Default for @link(Mass), zero means "use @link(Density) instead". }
      DefaultMass = 0.0;
      { Default for @link(Restitution). }
      DefaultRestitution = 0.0;
      { Default for @link(Friction). }
      DefaultFriction = 0.5;

    class var
      { Minimal collider thickness used by autosize in default 3d mode.

        AutoSizeMinimalThickness change only affects new size recalculations.}
      AutoSizeMinimalThickness: Single;
      { Minimal collider thickness used by autosize in 2d mode.

        AutoSizeMinimalThickness change only affects new size recalculations.}
      AutoSizeMinimalThickness2D: Single;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Recalculate collider size using parent bounding box, only if @link(AutoSize) says we should recalculate.
      Ignored if not @link(AutoSize). }
    procedure InternalAutoSize;

    { Calculate collider size to reflect parent bounding box now.
      This works regardless of @link(AutoSize) value,
      and doesn't change @link(AutoSize) value.
      The idea is that you can call it when

      @orderedList(
        @item(You have @link(AutoSize) = @false and want to adjust size to bounding box
          at specific moment.)
        @item(You have @link(AutoSize) = @true but still want to force recalculation right now,
          because @link(AutoSize) doesn't do it too eagerly (see AutoSize description).
        )
      )

      @seealso AutoSize }
    procedure CalculateSize;

    { Local collider translation, relative to parent TCastleTransform.
      Note that CalculateSize and AutoSize mechanisms set it automatically. }
    property Translation: TVector3 read FTranslation write SetTranslation;

    { Local collider rotation, relative to parent TCastleTransform.
      Note that CalculateSize and AutoSize mechanisms set it automatically. }
    property Rotation: TVector4 read FRotation write SetRotation;

    { Local collider scale, relative to parent TCastleTransform.
      Note that CalculateSize and AutoSize mechanisms set it automatically. }
    property Scale: TVector3 read FScale write SetScale;
  published
    { Automatically calculate collider properties that determine collider size
      based on the parent transformation bounding box.

      When this is set, properties like
      @link(TCastleSphereCollider.Radius) or @link(TCastleBoxCollider.Size)
      are automatically updated to the best values to match collider size
      with the parent @link(TCastleTransform.LocalBoundingBox).

      Note that even when this is @true, the collider size recalculation is not done
      too "eagerly", i.e. the collider size is not recalculated on every possible change.
      For example, if you animate a TCastleScene, then bounding box may change every frame
      (note: it depends on the technique; e.g. glTF models with skinned animation
      have a complete bounding box of the animation precalculated and constant;
      but X3D models morphed using CoordinateInterpolator have bounding box
      changing every frame).

      But it would be bad to change the collider size (even at design-time) every frame.
      That's because changing the collider size may imply (for the underlying physics engine)
      recreation of internal srtuctures and reinitialization of the collision state.
      So in the ideal case, the collider size should just stay constant once the object
      has been added to some TCastleViewport and it may interact with other physics bodies.

      You can always manually force recalculation of the collider size using the
      @link(CalculateSize) method. }
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;

    { Density (per volume) in kg, this implicitly determines mass
      (volume is automatically calculated by the physics engine).
      If you instead prefer to explicitly set mass, set @link(Mass)
      property to something non-zero. }
    property Density: Single read FDensity write SetDensity
      {$ifdef FPC}default DefaultDensity{$endif};

    { Mass in kg. When non-zero, this overrides the automatically calculated
      mass from the volume and @link(Density). }
    property Mass: Single read FMass write SetMass
      {$ifdef FPC}default DefaultMass{$endif};

    { Larger restitution means that on collision, this object behaves more elastic.
      Reasonable values are between 0 (inelastic) and 1 (elastic).
      See https://en.wikipedia.org/wiki/Coefficient_of_restitution . }
    property Restitution: Single read FRestitution write SetRestitution
      {$ifdef FPC}default DefaultRestitution{$endif};

    { Larger friction means that it is harder for body to slide alongside another object. }
    property Friction: Single read FFriction write SetFriction
      {$ifdef FPC}default DefaultFriction{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Collide as an infinite plane.
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies.

    In the current implementation, this collider supports only static rigid bodies.
    The associated TCastleRigidBody instance (with the same TCastleTransform parent)
    will behave as if @link(TCastleRigidBody.Dynamic) and @link(TCastleRigidBody.Animated) are @false. }
  TCastlePlaneCollider = class(TCastleCollider)
  strict private
    FNormal: TVector3;
    FDistance: Single;
    procedure SetNormal(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    function ForceStaticBody: Boolean; override;
    procedure CalculateSizeCore; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Normal vector of the plane.
      By default this is +Y (if not AutoSize).
      This makes the default plane by constant in Y axis, and span to infinity in X and Z axis.
      This makes it nice as a ground/floor in 3D. }
    property Normal: TVector3 read FNormal write SetNormal stored StoredWithAutoSize;
  published
    { Distance from (0,0,0) to the point.
      Default 0 (if not AutoSize). }
    property Distance: Single read FDistance write SetDistance stored StoredWithAutoSize;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Collide as a box.
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies. }
  TCastleBoxCollider = class(TCastleCollider)
  strict private
    FMode2D: Boolean;
    FSize: TVector3; // calculated size without scale
    FVisualisation: TCastleTransform;
    procedure SetSize(const AValue: TVector3);
    procedure SetMode2D(const AValue: Boolean);
  private
    class procedure CreateComponent2D(Sender: TObject);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure DesigningBegin; override;
    procedure DesigningEnd; override;
    procedure BeforeDestruction; override;

    { Box size.
      Default is (2,2,2) (if not AutoSize), consistent with TCastleBox. }
    property Size: TVector3 read FSize write SetSize;
  published
    { If @true, we calculate automatic size looking only at sizes in XY. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Collide as a sphere.
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies. }
  TCastleSphereCollider = class(TCastleCollider)
  strict private
    FRadius: Single;
    FMode2D: Boolean;
    FVisualisation: TCastleTransform;
    procedure SetRadius(const AValue: Single);
    procedure SetMode2D(const AValue: Boolean);
  private
    class procedure CreateComponent2D(Sender: TObject);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure DesigningBegin; override;
    procedure DesigningEnd; override;
    procedure BeforeDestruction; override;
  published
    { Sphere radius.
      Default is 1 (if not AutoSize), consistent with TCastleSphere. }
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;

    { If @true, we calculate automatic size looking only at sizes in XY. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;
  end;

  { Collide as a capsule.
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies. }
  TCastleCapsuleCollider = class(TCastleCollider)
  strict private
    FRadius: Single;
    FHeight: Single;
    FMode2D: Boolean;
    procedure SetRadius(const AValue: Single);
    procedure SetHeight(const AValue: Single);
    procedure SetMode2D(const AValue: Boolean);
  private
    class procedure CreateComponent2D(Sender: TObject);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Sphere (at both top and bottom of capsule) radius.
      Default is 1 (if not AutoSize), consistent with TCastleSphere. }
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;

    { Sphere (at both top and bottom of capsule) radius.
      Default is 2 (if not AutoSize), consistent with TCastleCylinder. }
    property Height: Single read FHeight write SetHeight stored StoredWithAutoSize;

    { If @true, we calculate automatic size looking only at sizes in XY. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;
  end;

  { Collide as a set of triangles determined by @link(Mesh).
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies.

    Make sure to set the @link(Mesh) property.

    In the current implementation, this collider supports only static rigid bodies.
    The associated TCastleRigidBody instance (with the same TCastleTransform parent)
    will behave as if @link(TCastleRigidBody.Dynamic) and @link(TCastleRigidBody.Animated) are @false. }
  TCastleMeshCollider = class(TCastleCollider)
  strict private
    FKraftMesh: TKraftMesh;
    FMesh: TCastleTransform;
    FDoubleSided: Boolean;
    procedure AddTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
    procedure SetDoubleSided(const Value: Boolean);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure SetMesh(const AValue: TCastleTransform);
    { In case of mesh collider only remove Translation, Rotation and Scale }
    function ForceStaticBody: Boolean; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Mesh which is used to collide.
      It can be a TCastleScene, TCastleBox or other instance of TCastleTransform
      with @link(TCastleTransform.HasColliderMesh) = @true.

      It does not have to be convex.

      Note: The values of @link(TCastleTransform.Collides Mesh.Collides) and
      @link(TCastleTransform.GetCollides Mesh.GetCollides) of the assigned instance
      do not matter.

      Note: Using the TCollisionNode X3D nodes inside the assigned instance
      is still useful. This way the scene may collide
      as something different than it's visible, see
      https://castle-engine.io/x3d_implementation_navigation.php . }
    property Mesh: TCastleTransform read FMesh write SetMesh;

    { Treat both faces sides as collidable.
      When this is @false: Depending on physics engine and situation,
      the back faces (where the normal doesn't point) may be ignored e.g.
      by @link(TCastleRigidBody.PhysicsRayCast). }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;
  end;

  TCollisionEvent = procedure (const CollisionDetails: TPhysicsCollisionDetails) of object;

  TCastleRigidBodyList = {$ifdef FPC}specialize{$endif} TList<TCastleRigidBody>;

  TCollisionDetection = (
    cdDiscrete,
    cdContinuous
  );

  TCollider = class;

  { Use this behavior to be affected by physics collisions and forces.
    Add a TCastleRigidBody and some collider (TCastleCollider descendant)
    as behaviors of TCastleTransform to make the given TCastleTransform
    be affected by physics and collide with other physics bodies. }
  TCastleRigidBody = class(TCastleBehavior)
  strict private
    type
      TState = (
        stateNone, //< nothing is done
        stateMissingCollider, //< needs initialize collider
        stateMissingFinalization, //< needs FinishInitialization
        stateReady //< ready to work
      );
    var
      { Current rigid body state. }
      FState: TState;
      { Castle collider behavior that was used to initialize rigid body or nil
        if physics engine collider was not initialized }
      FCastleCollider: TCastleCollider;

      { Transform for which we initialized the physics objects or nil }
      FTransform: TCastleTransform;
      FDuringSettingTransformationFromPhysicsEngine: Boolean;

      { List of collisions from previous step. }
      FPrevCollisions: TCastleRigidBodyList;
      { List of collisions from current step. }
      FCurrentCollisions: TCastleRigidBodyList;

      FOnCollisionEnter: TCollisionEvent;
      FOnCollisionStay: TCollisionEvent;
      FOnCollisionExit: TCollisionEvent;
      FNotifyOnInitialized: TCastleComponentNotification;
      FNotifyBeforeDeinitialized: TCastleComponentNotification;

    procedure PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
    { Assign or unassign PhysicsPostStep in TKraftBody.OnPostStep when needed. }
    procedure CheckPhysicsPostStepNeeded;

    { Makes easier to get appropriate Kraft shape (from the old collider or
      the new behavior. Will be removed after removing the old physics support.}
    function GetKraftShape: TKraftShape;

    { Returns TCastleCollider bahavior or nil when no collider behavior in
      Parent or Parent = nil. In most cases you should use FCastleCollider
      reference to behavior that was used to initialize rigid body }
    function GetCastleColliderFromParent: TCastleCollider;

    { Updates Kraft rigid body type krbtDynamic, krbtKinematic, krbtStatic
      based on Dynamic and Animated properties. Used in SetAnimated, SetDynamic,
      InitializeEngineRigidBody.}
    procedure UpdateKraftRigidBodyType;

    { Changes Kraft GravityScale based on Gravity property. Used in SetGravity
      and InitializeEngineRigidBody. }
    procedure UpdateKraftGravity;
  private
    FKraftBody: TKraftRigidBody;
    {$warnings off}
    FCollider: TCollider;
    {$warnings on}
    FGravity: Boolean;
    FDynamic: Boolean;
    FAnimated: Boolean;
    FTrigger: Boolean;
    FCollisionDetection: TCollisionDetection;
    FExists: Boolean;
    FLockTranslation: T3DCoords;
    FLockRotation: T3DCoords;
    FAngularVelocity: TVector3;
    FAngularVelocityDamp: Single;
    FMaximalAngularVelocity: Single;
    FLinearVelocity: TVector3;
    FLinearVelocityDamp: Single;
    FMaximalLinearVelocity: Single;

    FCollisionList: TCastleTransformList;

    { Updates engine collision groups to old Transform.Collides property value. }
    procedure UpdateCollides(const Transform: TCastleTransform);
    { Updates existence state of all physics engine objects - body and collider. }
    procedure UpdateExist;
    { Updates existence state of physics engine body. }
    procedure UpdateExistBody;
    { Updates colision detection type (discrete, continous) of physics engine body. }
    procedure UpdateCollisionDetection;
    { Updates axes that have translation lock and call Finish if needed }
    procedure UpdateLockTranslation;
    { Updates axes that have rotation lock and call Finish if needed }
    procedure UpdateLockRotation;

    { Sets all values from Kraft to CGE private fields. Currently angular and
      linear velocity. }
    procedure SynchronizeFromKraft;

    procedure SetWorldTransformation(const WorldTransform: TMatrix4);
    procedure SetLockTranslation(const AValue: T3DCoords);
    procedure SetLockRotation(const AValue: T3DCoords);

    procedure SetAngularVelocity(const AVelocity: TVector3);
    procedure SetAngularVelocityDamp(const AValue: Single);
    procedure SetMaximalAngularVelocity(const AValue: Single);

    procedure SetLinearVelocity(const LVelocity: TVector3);
    procedure SetLinearVelocityDamp(const AValue: Single);
    procedure SetMaximalLinearVelocity(const AValue: Single);

    procedure SetOnCollisionEnter(const AValue: TCollisionEvent);
    procedure SetOnCollisionStay(const AValue: TCollisionEvent);
    procedure SetOnCollisionExit(const AValue: TCollisionEvent);

    procedure SetCollisionDetection(const AValue: TCollisionDetection);

    procedure SetDynamic(const AValue: Boolean);
    procedure SetAnimated(const AValue: Boolean);
    procedure SetGravity(const AValue: Boolean);

    procedure SetExists(const Value: Boolean);
    procedure SetTrigger(const Value: Boolean);

    { Creates physics engine object and connects the rigid body with
      @link(TCastleTransform Parent) and @link(TCastleTransform Parent).World.

      Initialization is done in three steps:
      1. Creation of Kraft rigid body in InitializeEngineRigidBody
      2. Creation of Kraft shape based on TCollder or TCastleCollider
      3. Finishing initialization when creation of Kraft shape succeeded }
    procedure InitializePhysicsEngineObjects;
    { Frees physics objects in three steps:
      1. Deinitialize colliders
      2. Removes self from other rigid bodies collision lists
      3. Frees the physics engine rigid body object }
    procedure DeinitializePhysicsEngineObjects;

    { Initialize rigid body objects from physics engine }
    procedure InitializeEngineRigidBody;
    { Initialize collider from physics engine }
    function TryInitializeColliders: Boolean;
    { Things to initialize after collider initialization }
    procedure FinishInitialization;
    { Used by TCastleCollider when it's parent changes to handle case when
      rigid body is added before collider behavior }
    procedure CheckInitialization(const ACastleCollider: TCastleCollider);

    { Deinitializes physics engine collider object }
    procedure DeinitializeColliders;

    { Called by TCastleCollider when it's shape changes }
    procedure ReinitializeCastleCollider;

    class procedure CreateComponent2D(Sender: TObject);

    procedure AddNotificationOnInitialized(
      const AEvent: TComponentEvent); overload;
    { Calls all callbacks added by AddNotificationOnInitialized }
    procedure NotifyOnInitialized;
    procedure RemoveNotificationOnInitialized(
      const AEvent: TComponentEvent); overload;

    procedure AddNotificationBeforeDeinitialized(
      const AEvent: TComponentEvent); overload;
    { Calls all callbacks added by AddNotificationOnInitialized }
    procedure NotifyBeforeDeinitialized;
    procedure RemoveNotificationBeforeDeinitialized(
      const AEvent: TComponentEvent); overload;

    { Is rigid body full initialized? }
    function IsInitialized: Boolean;

    property DuringSettingTransformationFromPhysicsEngine: Boolean
      read FDuringSettingTransformationFromPhysicsEngine;
  protected
    { Initializes physics engine objects when TCastleRigidBody is attached
      to world. }
    procedure WorldAfterAttach; override;

    { Deinitializes physics engine objects when TCastleRigidBody is attached
      to world. }
    procedure WorldBeforeDetach; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Only one TCastleRigidBody behavior can be added to a given
      TCastleTransform so we check that here. }
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Utility function to set common values for physics in 2D games.
      Locks moving along the Z axis,
      locks rotating along the X and Y axes. }
    procedure Setup2D;

    { Transformations that we collide with currently. }
    function GetCollidingTransforms: TCastleTransformList;

    { Cast a ray using physics engine, see what is hit.

      The given RayOrigin, RayDirection are in the parent
      coordinate system of this TCastleTransform.
      So for example query like this works naturally:
      @code(MyTransform.RayCast(MyTransform.Translation, MyTransform.Direction, MaxDistance)).
      In case of the overloaded version with Distance parameter,
      the Distance is consistently in the same, parent coordinate system.

      This ignores the collider of this rigid body (to not accidentally collide
      with your own collider), and checks collisions with the rest of the world in
      given max distance.
      Only collisions with the physics colliders (TCastleCollider and TCollider))
      are considered.

      The @link(TCastleTransform.Pickable) property is ignored by this method,
      i.e. it considers all colliders regardless of their @link(TCastleTransform.Pickable) value.
      This is in contrast to @link(TCastleTransform.RayCast).
      In the future this method will become the basis of a new @link(TCastleTransform.RayCast)
      implementation and the concept of "pickable" flag will change into
      collision layers.

      This returns the TCastleTransform that is hit and a distance from RayOrigin
      to the hit point.
      Returns @nil (Distance is undefined in this case) if nothing was hit.
      @groupBegin }
    function PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
      const MaxDistance: Single): TCastleTransform; overload;
    function PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
      const MaxDistance: Single; out Distance: Single): TCastleTransform; overload;
    { @groupEnd }

    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Push this rigid body.
      The body is pushed from a Position and along the direction of Force.
      Longer length of Force makes a stronger push.

      The forces affecting each body are reset every frame
      (only the body velocity is preserved across frames).
      If you want to keep applying this force (e.g. it is a wind that continues pushing in given direction)
      you should call this method every frame.
      It automatically accounts for the delta time (because the force will actually
      be applied later, with delta time, to velocity) so no need to multiply the arguments with SecondsPassed.
      That is, adding force means @code(Velocity += Force * TimeDelta / Mass).

      By changing Position from which you push,
      you can change the angular velocity,
      i.e. different Position makes the body spin in different ways.
      Note that Position too distant from the body center may result in unrealistically strong push.
      If Position is exactly at body center, then angular velocity will not be affected.

      Force and position is in world coordinate system.

      TODO: rename params and invert order to Position, Direction?

      TODO: add parameter that determines coordinate system, make it consistent with friend methods. }
    procedure AddForce(const Force, Position: TVector3);

    { Push this rigid body.
      The body is pushed along the direction of Force.
      Longer length of Force makes a stronger push.

      This is similar to @link(AddForce), but the Force is in local coordinate system
      (use @link(TCastleTransform.WorldToLocal) to convert coordinates).
      Moreover it doesn't take as parameter Position: the force always acts from the origin
      (0,0,0 - TODO check) of this body.

      Just like with @link(AddForce),
      the forces affecting each body are reset every frame
      (only the body velocity is preserved across frames).
      If you want to keep applying this force (e.g. it is a wind that continues pushing in given direction)
      you should call this method every frame.
      It automatically accounts for the delta time (because the force will actually
      be applied later, with delta time, to velocity) so no need to multiply the arguments with SecondsPassed.

      TODO: rename param to Direction?

      TODO: add parameter that determines coordinate system, make it consistent with friend methods. }
    procedure AddCentralForce(const Force: TVector3);

    { Rotate this rigid body with physics.
      Longer length of Torque makes a stronger push.
      Torque is in world coordinates.

      Just like with @link(AddForce),
      the forces affecting each body are reset every frame
      (only the body velocity is preserved across frames).
      If you want to keep applying this force (e.g. it is a whirlwind that continues rotating)
      you should call this method every frame.
      It automatically accounts for the delta time (because the force will actually
      be applied later, with delta time, to velocity) so no need to multiply the arguments with SecondsPassed.

      TODO: explain better, why is this not necessary to be called every frame?
      Why AddForce, if called once, doesn't act as "impulse"?

      TODO: params: Direction?

      TODO: add parameter that determines coordinate system, make it consistent with friend methods. }
    procedure AddTorque(const Torque: TVector3);

    { Apply a one-time force, that pushes the body (increasing velocity).
      Impulse is a direction along which to push, Point is the position from which you push.
      Longer length of Impulse makes a stronger push.

      Adding impulse means @code(Velocity += Force / Mass).
      To be more precise, adding impulse directly increases @link(LinearVelocity) and @link(AngularVelocity).
      Unlike AddForce, the velocity increase is not multiplied by the delta of time,
      so it is not expected you will call this every frame.
      (If you do, you should take care to multiply Force by TimeDelta.)

      Same notes as for Position of @link(AddForce):
      By changing Point from which you push,
      you can change the angular velocity,
      i.e. different Point makes the body spin in different ways.
      Note that Point too distant from the body center may result in unrealistically strong push.
      If Point is exactly at body center, then angular velocity will not be affected.

      TODO: Is the difference from AddForce really that simple?
      And if so, do we even need a separate method for it -- user could multiply
      by SecondsPassed whenever needed.
      And if user does it, it will work better with Viewport/Scene.TimeScale.

      TODO: params: Position and Direction, in this order?

      TODO: in local or world coordinates? check.

      TODO: add parameter that determines coordinate system, make it consistent with friend methods. }
    procedure ApplyImpulse(const Impulse, Point: TVector3);

    { Returns true when rigid body behavior exists, and is added to Castle
      Transform, and this transform ExistsInRoot = true. }
    function ExistsInRoot: Boolean;

    { If TCastleTransform did not exist in root at initialization or
      TCastleScene URL changes it may be wrong size when it is
      determined with autosize. Which can cause e.g. the torque to be incorrect. }
    procedure UpdateColliderAutosize;

    { For performance, physics engine sometimes puts some body to "sleep",
      which means it will not move anymore,
      because it didn't move (much) in the recent frames.
      In general, the body should be "woken up" automatically when something collides
      with it, so the mechanism should be invisible to you... but sometimes it is necessary
      to wake up a body explicitly. }
    procedure WakeUp;

    //procedure Sleep;

    { Current rotating velocity of this body.
      You can query it (as it changes, affected by forces) and set it at any point.
      In world coordinates.

      See https://en.wikipedia.org/wiki/Angular_velocity
      about the exact meaning of angular velocity. The vector direction
      (AngularVelocity.Normalize) determines the axis around which we rotate.
      The vector length (AngularVelocity.Length) determines the speed (in radians/second)
      with which we rotate. }
    property AngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;

    { Current moving velocity of this body.
      You can query it (as it changes, affected by forces) and set it at any point.
      In world coordinates. }
    property LinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;

    {$ifdef FPC}
    property InitialAngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;
      deprecated 'use AngularVelocity';
    property InitialLinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;
      deprecated 'use LinearVelocity';
    {$endif}

    { Occurs when TCastleRigidBody starts colliding with another TCastleRigidBody.

      It can occur repeatedly for the same body (in the same time instant)
      if the collision has many points of contact.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleUserInterface.OnUpdate)). }
    property OnCollisionEnter: TCollisionEvent read FOnCollisionEnter write SetOnCollisionEnter;

    { Occurs when TCastleRigidBody stops colliding with another TCastleRigidBody.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleUserInterface.OnUpdate)). }
    property OnCollisionExit: TCollisionEvent read FOnCollisionExit write SetOnCollisionExit;

    { Occurs when TCastleRigidBody still collides with another TCastleRigidBody.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleUserInterface.OnUpdate)). }
    property OnCollisionStay: TCollisionEvent read FOnCollisionStay write SetOnCollisionStay;

  published
    { Does the physics simulation move and rotate this object
      (because of gravity, or because it collides with others).

      @definitionList(
        @itemLabel true (dynamic object, the default)
        @item(
          In this case you cannot
          change the @link(TCastleTransform.Translation) and other
          transformation properties of the related TCastleTransform
          after assigning @link(TCastleTransform.RigidBody).
          They are under the control of the physics engine.
          You can still reliably read them.

          Keep @link(Animated) as @false in this case.
          Right now, @link(Animated) doesn't actually matter when @name is @true,
          but don't depend on it.
          In the future, we may enable to set @link(Animated)
          to @true, to allow you to explicitly transform objects that are still
          under control of physics.
        )

        @itemLabel false
        @item(
          In this case object is not transformed by the physics simulation.
          It still collides with other physical objects.

          You can change the @link(TCastleTransform.Translation) and other
          transformation properties by your own code.
          But if you plan to change them @italic(very often)
          (e.g. every frame), then set the @link(Animated) property to @true.
        )
      )
    }
    property Dynamic: Boolean read FDynamic write SetDynamic default true;

    { Is the transformation of this object updated often
      (relevant only when @link(Dynamic) = @false).

      This property is taken into account only if @link(Dynamic) = @false.
      When @link(Dynamic) = @false,
      you can change the @link(TCastleTransform.Translation) and other
      transformation properties of the related TCastleTransform.
      This property is an optimization hint to the physics engine,
      it says @italic(how often) you will change the transformation.

      @definitionList(
        @itemLabel true (animated object, also known as "kinematic")
        @item(
          The physics engine is prepared that this object can move often,
          e.g. every frame, so you can animate the related @link(TCastleTransform)
          using any technique.
        )

        @itemLabel false (static object)
        @item(
          Changing the transformation properties is costly,
          it may even be like destroying this object
          and creating a new one from the point of view of the physics engine,
          @italic(so do not change them often, e.g. every frame).

          The upside is that collisions with static objects may be much more
          optimized, since the physics engine can assume that all the static
          objects form a completely frozen unmovable geometry in the world.
        )
      )
    }
    property Animated: Boolean read FAnimated write SetAnimated default false;

    { Triggers report when other object collides with them,
      but still allow the other object to pass through.
      In other words, colliding with a trigger will not cause the collider to stop or
      "bounce off" the trigger.

      They are useful as sensors. E.g. a trigger may be a coin (like in "Mario")
      that the player can "consume" by colliding with it.

      Triggers report collisions through the same events as other rigid bodies:
      (@link(TCastleRigidBody.OnCollisionEnter), @link(TCastleRigidBody.OnCollisionStay),
      @link(TCastleRigidBody.OnCollisionExit)). }
    property Trigger: Boolean read FTrigger write SetTrigger default false;

    { Shape used for collisions with this object.
      You cannot assign this property directly,
      it is automatically assigned when you create a @link(TCollider)
      specifying this @link(TRigidBody) as it's parent. }
    property Collider: TCollider read FCollider;

    { Sets CollisionDetectionMode in rigid body to Discrete or ContinuousSweep. }
    property CollisionDetection: TCollisionDetection read FCollisionDetection
      write SetCollisionDetection default cdDiscrete;

    { Is this object affected by gravity. }
    property Gravity: Boolean read FGravity write SetGravity default true;

    { Disable motion (@link(TCastleTransform.Translation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable motion along the Z axis.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockTranslation: T3DCoords read FLockTranslation write SetLockTranslation;

    { Disable rotation (@link(TCastleTransform.Rotation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable rotation along the X and Y axes.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockRotation: T3DCoords read FLockRotation write SetLockRotation;

    { Controls whether the rigid body is actually processed by the physics engine.
      When this is @false, the rigid body is not updated by the physics engine,
      and it does not cause collisions with other rigid bodies. }
    property Exists: Boolean read FExists write SetExists default true;

    property AngularVelocityDamp: Single read FAngularVelocityDamp write SetAngularVelocityDamp {$ifdef FPC}default 0.1{$endif};
    property MaximalAngularVelocity: Single read FMaximalAngularVelocity write SetMaximalAngularVelocity;

    property LinearVelocityDamp: Single read FLinearVelocityDamp write SetLinearVelocityDamp {$ifdef FPC}default 0.1{$endif};
    property MaximalLinearVelocity: Single read FMaximalLinearVelocity write SetMaximalLinearVelocity;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlerigidbody_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ utilities ------------------------------------------------------------------ }

function VectorToKraft(const V: TVector3): TKraftVector3;
begin
  // simple implementation
  // Result.X := V.X;
  // Result.Y := V.Y;
  // Result.Z := V.Z;

  // optimized implementation
  Assert(SizeOf(TKraftScalar) = SizeOf(Single));
  Move(V, Result, SizeOf(V));
end;

function VectorFromKraft(const V: TKraftVector3): TVector3;
begin
  // optimized implementation
  Assert(SizeOf(TKraftScalar) = SizeOf(Single));
  Move(V, Result, SizeOf(V));
end;

function MatrixToKraft(const M: TMatrix4): TKraftMatrix4x4;
begin
  Assert(SizeOf(M) = SizeOf(Result));
  // simply copy the contents,
  // memory layout is the same (column-major, Single precision)
  Move(M, Result, SizeOf(M));
end;

function MatrixFromKraft(const M: TKraftMatrix4x4): TMatrix4;
begin
  Assert(SizeOf(M) = SizeOf(Result));
  Move(M, Result, SizeOf(M));
end;

{ TPhysicsCollisionDetails --------------------------------------------------- }

function TPhysicsCollisionDetails.OtherTransform: TCastleTransform;
begin
  Result := Transforms[1];
end;

{ TPhysicsProperties --------------------------------------------------------- }

procedure TPhysicsProperties.SetFrequency(const AValue: Single);
begin
  if FFrequency = AValue then
    Exit;

  FFrequency := AValue;
  FPhysicsTimeStep := 1.0 / FFrequency;

  if Assigned(RootTransform.FKraftEngine) then
    RootTransform.FKraftEngine.SetFrequency(AValue);
end;

procedure TPhysicsProperties.SetUpdateCollidersScaleAtRuntime(
  const AValue: Boolean);
begin
  if FUpdateCollidersScaleAtRuntime <> AValue then
  begin
    FUpdateCollidersScaleAtRuntime := AValue;

    if FUpdateCollidersScaleAtRuntime and RootTransform.EnablePhysics then
      RootTransform.RecursivelyUpdateCollidersScale(RootTransform);
  end;
end;

procedure TPhysicsProperties.SetContinuousCollisionDetection(
  const AValue: Boolean);
begin
  if FContinuousCollisionDetection = AValue then
    Exit;

  FContinuousCollisionDetection := AValue;
  if Assigned(RootTransform.FKraftEngine) then
    if AValue then
      RootTransform.FKraftEngine.ContinuousMode := kcmTimeOfImpactSubSteps
    else
      RootTransform.FKraftEngine.ContinuousMode := kcmNone;
end;

procedure TPhysicsProperties.SetLinearVelocityRK4Integration(const AValue: Boolean);
begin
  if FLinearVelocityRK4Integration = AValue then
    Exit;

  FLinearVelocityRK4Integration := AValue;

  if Assigned(RootTransform.FKraftEngine) then
    RootTransform.FKraftEngine.LinearVelocityRK4Integration := AValue;
end;

procedure TPhysicsProperties.SetAngularVelocityRK4Integration(const AValue: Boolean);
begin
  if FAngularVelocityRK4Integration = AValue then
    Exit;

  FAngularVelocityRK4Integration := AValue;

  if Assigned(RootTransform.FKraftEngine) then
    RootTransform.FKraftEngine.AngularVelocityRK4Integration := AValue;
end;

constructor TPhysicsProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngularVelocityRK4Integration := DefaultAngularVelocityRK4Integration;
  FLinearVelocityRK4Integration := DefaultLinearVelocityRK4Integration;
  FFrequency := DefaultFrequency;
  FGravityStrength := DefaultGravityStrength;
  FPhysicsTimeStep := 1.0 / FFrequency;
  FMaxPhysicsTicksPerUpdate := DefaultMaxPhysicsTicksPerUpdate;
  FUpdateCollidersScaleAtRuntime := DefaultUpdateCollidersScaleAtRuntime;
  FContinuousCollisionDetection := DefaultContinuousCollisionDetection;
end;

{ TCastleCollider ------------------------------------------------------------ }

function TCastleCollider.ForceStaticBody: Boolean;
begin
  Result := false;
end;

procedure TCastleCollider.UpdateLocalTransform;
var
  T: TTransformation;
begin
  { Initialize FLocalTransform, FLocalInverseTransform and pass transform to FKraftShape }
  T.Init;
  T.Multiply(Rotation, NoScale, Translation * FLastUsedWorldScale * FScale);
  FLocalTransform := T.Transform;
  FLocalInverseTransform := T.InverseTransform;
  if FKraftShape <> nil then
  begin
    FKraftShape.LocalTransform := MatrixToKraft(FLocalTransform);
    Parent.RigidBody.FKraftBody.SynchronizeTransformIncludingShapes;
    FKraftShape.StoreWorldTransform;
  end;

  { After setting the appropriate transformation, we can optimize the matrix
    multiplication when rotation = 0, translation = 0,0,0 scale is not
    checked here because it must to be 1,1,1 in kraft and it's added in
    collider shape creation }
  FUseLocalTransform := not (
    FTranslation.IsPerfectlyZero and
    (FRotation.W = 0)
  );
end;

function TCastleCollider.InitializeKraft(const APhysics: TKraft;
  const ACastleRigidBody: TCastleRigidBody): Boolean;
begin
  // when using old Collider and new one
  if ACastleRigidBody.Collider <> nil then
    raise Exception.Create('Trying to use old TCollider and TCastleCollider');

  Assert(ACastleRigidBody.FKraftBody <> nil,
    'Trying initialize kraft shape when rigid body not initialized!');

  ACastleRigidBody.DeinitializeColliders;
  { If shape count is > 0 we need delete previous shape. }
  {if ACastleRigidBody.FKraftBody.ShapeCount > 0 then
  begin
    Assert(ACastleRigidBody.FKraftBody.ShapeCount = 1, 'More than one shape in rigid body.');
    Assert(ACastleRigidBody.FKraftBody.ShapeFirst <> FKraftShape, 'Mixed kraft shapes');
    FreeAndNil(FKraftShape);
  end;}

  FKraftShape := CreateKraftShape(APhysics, ACastleRigidBody.FKraftBody);

  if FKraftShape = nil then
    Exit(false);
  // assign common TCollider properties to Kraft shape
  FKraftShape.Density := Density;
  FKraftShape.Restitution := Restitution;
  FKraftShape.Friction := Friction;
  if not ACastleRigidBody.ExistsInRoot then
    FKraftShape.Flags := FKraftShape.Flags - [ksfCollision, ksfRayCastable];

  UpdateLocalTransform;

  Result := true;
end;

function TCastleCollider.GetRigidBody: TCastleRigidBody;
begin
  if Parent <> nil then
    Result := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody
  else
    Result := nil;
end;

procedure TCastleCollider.SetDensity(const AValue: Single);
var
  RigidBody: TCastleRigidBody;
begin
  if FDensity <> AValue then
  begin
    FDensity := AValue;

    if FKraftShape <> nil then
    begin
      FKraftShape.Density := FDensity;

      // Similar as in SetMass, we need to call FKraftBody.Finish
      RigidBody := GetRigidBody;
      if (RigidBody <> nil) and (RigidBody.FKraftBody <> nil) then
        RigidBody.FKraftBody.Finish;
    end;
  end;
end;

procedure TCastleCollider.SetMass(const AValue: Single);
var
  RigidBody: TCastleRigidBody;
begin
  if FMass <> AValue then
  begin
    FMass := AValue;

    RigidBody := GetRigidBody;
    if (RigidBody <> nil) and (RigidBody.FKraftBody <> nil) then
    begin
      RigidBody.FKraftBody.ForcedMass := FMass;

      { Without FKraftBody.Finish,
        merely updating FKraftBody.ForcedMass doesn't have an effect,
        Kraft still uses old mass.

        Testcase:
        - in editor, set Mass of some bullet to 10.
        - Play physics.
        - change Mass to 10000.
        - Play physics -- without FKraftBody.Finish,
          it will behave as if Mass was still 10.
        - Stopping and playing physics again will update it OK (because at this point,
          stopping physics reloads the design, initializing everything).
        So the buggy effect is that you need to "play physics twice"
        to actually see the new Mass.
      }
      RigidBody.FKraftBody.Finish;
    end;
  end;
end;

procedure TCastleCollider.SetRestitution(const AValue: Single);
begin
  FRestitution := AValue;
  if FKraftShape <> nil then
    FKraftShape.Restitution := FRestitution;
end;

procedure TCastleCollider.SetFriction(const AValue: Single);
begin
  FFriction := AValue;
  if FKraftShape <> nil then
    FKraftShape.Friction := FFriction;
end;

procedure TCastleCollider.SetTranslation(const AValue: TVector3);
begin
  if AValue.PerfectlyEquals(AValue, FTranslation) then
    Exit;

  FTranslation := AValue;
  DisableAutoSize;
  UpdateLocalTransform;
end;

procedure TCastleCollider.SetRotation(const AValue: TVector4);
begin
  if AValue.PerfectlyEquals(AValue, FRotation) then
    Exit;

  FRotation := AValue;
  DisableAutoSize;
  UpdateLocalTransform;
end;

procedure TCastleCollider.SetScale(const AValue: TVector3);
begin
  if AValue.PerfectlyEquals(AValue, FScale) then
    Exit;

  FScale := AValue;
  // Scale is added to collider on shape creation not by transform
  ShapeChangedNotifyRigidBody;
end;

procedure TCastleCollider.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize = AValue then
    Exit;

  FAutoSize := AValue;
  InternalAutoSize;
end;

procedure TCastleCollider.ShapeChangedNotifyRigidBody;
var
  CastleRBody: TCastleRigidBody;
begin
  if Parent <> nil then
  begin
    CastleRBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if CastleRBody <> nil then
      CastleRBody.ReinitializeCastleCollider;
  end;
end;

procedure TCastleCollider.WorldAfterAttach;
var
  CastleRigidBody: TCastleRigidBody;
begin
  { Check there is a rigid body and we should try initialize collider.
    This is the case when rigid body was added first to behaviors list }
  CastleRigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if CastleRigidBody <> nil then
    CastleRigidBody.CheckInitialization(Self);

  InternalAutoSize;

  UpdateScale(false);

  inherited WorldAfterAttach;
end;

procedure TCastleCollider.WorldBeforeDetach;
var
  CastleRBody: TCastleRigidBody;
begin
  CastleRBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if CastleRBody <> nil then
  begin
    // deinitialize collider
    CastleRBody.DeinitializeColliders;
  end;

  inherited WorldBeforeDetach;
end;

function TCastleCollider.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleCollider) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleCollider behavior can be added to a given TCastleTransform';
    Result := false;
  end;
end;

procedure TCastleCollider.DisableAutoSize;
begin
  if FCalculatingSize then
    Exit;

  if IsLoading then
    Exit;

  AutoSize := false;
end;

procedure TCastleCollider.CalculateSizeCore;
begin
  Translation := TVector3.Zero;
  Rotation := TVector4.Zero;
  Scale := NoScale;
end;

procedure TCastleCollider.CalculateSize;
begin
  FCalculatingSize := true;
  try
    CalculateSizeCore;
  finally FCalculatingSize := false end;
end;

procedure TCastleCollider.UpdateScale(ForceUpdate: Boolean);
var
  NewScale: TVector3;
begin
  if World <> nil then
  begin
    ScaleFromMatrix(Parent.WorldTransform, NewScale);
  end else
    NewScale := NoScale * Parent.Scale;

  { Never use a negative scale for colliders }
  NewScale := NewScale.Abs;

  if ForceUpdate or not FLastUsedWorldScale.PerfectlyEquals(FLastUsedWorldScale, NewScale) then
  begin
    FLastUsedWorldScale := NewScale;
    { Scale is added in CreateCraftShape so we only need notify
      RigidBody to recreate it }
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleCollider.StoredWithAutoSize: Boolean;
begin
  Result := not AutoSize;
end;

constructor TCastleCollider.Create(AOwner: TComponent);
begin
  inherited;
  // properties defaults
  FDensity := DefaultDensity;
  FMass := DefaultMass;
  FRestitution := DefaultRestitution;
  FFriction := DefaultFriction;
  FScale := NoScale;
  FLastUsedWorldScale := NoScale;
  FTranslation := TVector3.Zero;
  FRotation := TVector4.Zero;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_implementation_constructor}

  { Will be updated in TCastleCollider.InitializeKraft to reflect FKraftShape.LocalTransform }
  FUseLocalTransform := false;
  FLocalTransform := TMatrix4.Identity;
  FLocalInverseTransform := TMatrix4.Identity;
  FAutoSize := true;

  NeedWorldChangeNotification := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleCollider.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'Density', 'Friction', 'Mass', 'Restitution'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCollider.InternalAutoSize;
begin
  if AutoSize then
    CalculateSize;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastlePlaneCollider ------------------------------------------------------- }

procedure TCastlePlaneCollider.SetNormal(const AValue: TVector3);
begin
  if not FNormal.Equals(FNormal, AValue) then
  begin
    FNormal := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastlePlaneCollider.SetDistance(const AValue: Single);
begin
  if not SameValue(FDistance, AValue) then
  begin
    FDistance := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastlePlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  if ARigidBody.RigidBodyType <> krbtStatic then
  begin
    WritelnWarning('TCastlePlaneCollider is available only for static rigid bodies.');
    Result := nil;
    Exit;
  end;

  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(FNormal)),
    Distance * FLastUsedWorldScale.Max * FScale.Max));
end;

procedure TCastlePlaneCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
begin
  inherited; // resets Translation, Rotation, Scale

  { TODO: We could calculate Distance better looking at LocalBoundingBox,
    for now we just always set it to 0. }
  Distance := 0;

  { Default, in case we don't have any better value }
  Normal := TVector3.One[1];

  if Parent = nil then
    Exit;
  LocalBoundingBox := Parent.LocalBoundingBox;
  if LocalBoundingBox.IsEmptyOrZero then
    Exit;

  Normal := TVector3.One[MinAbsVectorCoord(LocalBoundingBox.Size)];
end;

constructor TCastlePlaneCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Note: assign using Fxxx, not property setter, to avoid changing AutoSize to @false
  FNormal := TVector3.One[1];

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastlePlaneCollider.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastlePlaneCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AutoSize', 'NormalPersistent', 'Distance'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastlePlaneCollider.ForceStaticBody: Boolean;
begin
  Result := true;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleBoxCollider --------------------------------------------------------- }

procedure TCastleBoxCollider.SetSize(const AValue: TVector3);
begin
  if not TVector3.Equals(FSize, AValue) then
  begin
    FSize := AValue;
    if FVisualisation <> nil then
      TCastleBox(FVisualisation).Size := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleBoxCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    InternalAutoSize;
  end;
end;

class procedure TCastleBoxCollider.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleBoxCollider).Mode2D := true;
end;

function TCastleBoxCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
var
  ScaledSize: TVector3;
begin
  ScaledSize := FSize * FLastUsedWorldScale * FScale;

  { Check it ourselves, otherwise Kraft will crash on such invalid box with segfault.
    First Kraft raises nice
      EKraftDegeneratedConvexHull.Create('Degenerated convex hull');
    but then makes SIGSEGV at
      fPhysics.fStaticAABBTree.DestroyProxy(fStaticAABBTreeProxy)
    in TKraftShape.Destroy. }

  if (ScaledSize[0] <= 0) or
     (ScaledSize[1] <= 0) or
     (ScaledSize[2] <= 0) then
    raise EPhysicsError.Create('You must assign positive Size to TBoxCollider');
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(ScaledSize / 2));
end;

procedure TCastleBoxCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  TempSize: TVector3;
  MinimalThickness: Single;
begin
  inherited; // resets Translation, Rotation, Scale

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Size := CastleVectors.Vector3(MinimalThickness, MinimalThickness, MinimalThickness);
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;
  TempSize := LocalBoundingBox.Size;

  if TempSize.X < MinimalThickness then
    TempSize.X := MinimalThickness;

  if TempSize.Y < MinimalThickness then
    TempSize.Y := MinimalThickness;

  if TempSize.Z < MinimalThickness then
    TempSize.Z := MinimalThickness;

  Size := TempSize;
  Translation := LocalBoundingBox.Center;
end;

constructor TCastleBoxCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Note: assign using Fxxx, not property setter, to avoid changing AutoSize to @false
  FSize := CastleVectors.Vector3(2, 2, 2);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleBoxCollider.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleBoxCollider.DesigningBegin;
begin
  inherited;

  if FVisualisation = nil then
  begin
    FVisualisation := TCastleBox.Create(nil);
    FVisualisation.SetTransient;
    TCastleBox(FVisualisation).UseInternalGlobalRenderOptions := false; // never change rendering to global
    TCastleBox(FVisualisation).Color := White;
    TCastleBox(FVisualisation).Material := pmUnlit;
    TCastleBox(FVisualisation).RenderOptions.WireframeColor := WhiteRGB;
    TCastleBox(FVisualisation).RenderOptions.WireframeEffect := weWireframeOnly;
    TCastleBox(FVisualisation).Size := FSize;
    Parent.Add(FVisualisation);
  end;
end;

procedure TCastleBoxCollider.DesigningEnd;
begin
  FreeAndNil(FVisualisation);
  inherited;
end;

procedure TCastleBoxCollider.BeforeDestruction;
begin
  FreeAndNil(FVisualisation);
  inherited;
end;

function TCastleBoxCollider.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AutoSize', 'Mode2D', 'SizePersistent'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleSphereCollider ------------------------------------------------------ }

constructor TCastleSphereCollider.Create(AOwner: TComponent);
begin
  inherited;

  // Note: assign using Fxxx, not property setter, to avoid changing AutoSize to @false
  FRadius := 1;
end;

procedure TCastleSphereCollider.SetRadius(const AValue: Single);
begin
  if not SameValue(FRadius, AValue) then
  begin
    FRadius := AValue;
    if FVisualisation <> nil then
      TCastleSphere(FVisualisation).Radius := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  { Scale support: There is no good way to do it, so I took the Unity solution,
    we take the largest value. }

  if FMode2D then
    Result := TKraftShapeSphere.Create(APhysics, ARigidBody,
      FRadius * Max(FLastUsedWorldScale.X, FLastUsedWorldScale.Y) *
      Max(FScale.X, FScale.Y))
  else
    Result := TKraftShapeSphere.Create(APhysics, ARigidBody,
      FRadius * FLastUsedWorldScale.Max * FScale.Max);
end;

class procedure TCastleSphereCollider.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleSphereCollider).Mode2D := true;
end;

procedure TCastleSphereCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  MinimalThickness: Single;
begin
  inherited; // resets Translation, Rotation, Scale

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinimalThickness;
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;
  if FMode2D then
    Radius := LocalBoundingBox.AverageSize2D(false, 2, 2) / 2
  else
    Radius := LocalBoundingBox.AverageSize(false, 2) / 2;

  { Check minimal size }
  Radius := Max(Radius, MinimalThickness);
  Translation := LocalBoundingBox.Center;
end;

function TCastleSphereCollider.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AutoSize', 'Mode2D', 'Radius'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSphereCollider.DesigningBegin;
begin
  inherited;
  if FVisualisation = nil then
  begin
    FVisualisation := TCastleSphere.Create(nil);
    FVisualisation.SetTransient;
    TCastleSphere(FVisualisation).UseInternalGlobalRenderOptions := false; // never change rendering to global
    TCastleSphere(FVisualisation).Color := White;
    TCastleSphere(FVisualisation).Material := pmUnlit;
    TCastleSphere(FVisualisation).RenderOptions.WireframeColor := WhiteRGB;
    TCastleSphere(FVisualisation).RenderOptions.WireframeEffect := weWireframeOnly;
    TCastleSphere(FVisualisation).Radius := FRadius;
    Parent.Add(FVisualisation);
  end;
end;

procedure TCastleSphereCollider.DesigningEnd;
begin
  FreeAndNil(FVisualisation);
  inherited;
end;

procedure TCastleSphereCollider.BeforeDestruction;
begin
  FreeAndNil(FVisualisation);
  inherited BeforeDestruction;
end;

procedure TCastleSphereCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    InternalAutoSize;
  end;
end;

{ TCastleCapsuleCollider ----------------------------------------------------- }

constructor TCastleCapsuleCollider.Create(AOwner: TComponent);
begin
  inherited;

  // Note: assign using Fxxx, not property setter, to avoid changing AutoSize to @false
  FRadius := 1;
  FHeight := 2;
end;

procedure TCastleCapsuleCollider.SetRadius(const AValue: Single);
begin
  if not SameValue(FRadius, AValue) then
  begin
    FRadius := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleCapsuleCollider.SetHeight(const AValue: Single);
begin
  if not SameValue(FHeight, AValue) then
  begin
    FHeight := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
  end;
end;

class procedure TCastleCapsuleCollider.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleCapsuleCollider).Mode2D := true;
end;

function TCastleCapsuleCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  if FMode2D then
    Result := TKraftShapeCapsule.Create(APhysics, ARigidBody,
      FRadius * FLastUsedWorldScale.X * FScale.X,
      FHeight * FLastUsedWorldScale.Y * FScale.Y)
  else
    Result := TKraftShapeCapsule.Create(APhysics, ARigidBody,
      FRadius * Max(FLastUsedWorldScale.X, FLastUsedWorldScale.Z) *
      Max(FScale.X, FScale.Y),
      FHeight * FLastUsedWorldScale.Y * FScale.Y);
end;

procedure TCastleCapsuleCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  MinimalThickness: Single;
begin
  inherited; // resets Translation, Rotation, Scale

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinimalThickness;
    Height := MinimalThickness;
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;

  if FMode2D then
    Radius := LocalBoundingBox.SizeX * 0.50
  else
    Radius := ((LocalBoundingBox.SizeX * 0.50) + (LocalBoundingBox.SizeZ * 0.50)) / 2;

  { If radius * 2 is bigger than height make it smaller. }
  if Radius * 2 > LocalBoundingBox.SizeY then
    Radius := LocalBoundingBox.SizeY / 2 - 0.001;
  { Radius must be at least MinimalThickness }
  Radius := Max(Radius, MinimalThickness);

  Height := Max(LocalBoundingBox.SizeY - Radius * 2, 0.001);

  Translation := LocalBoundingBox.Center;
end;

function TCastleCapsuleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AutoSize', 'Mode2D', 'Height', 'Radius'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCapsuleCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    InternalAutoSize;
  end;
end;

{ TCastleMeshCollider -------------------------------------------------------- }

procedure TCastleMeshCollider.AddTriangle(Shape: TObject;
  const Position: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  { TODO: it's probably not optimal to call AddTriangle
    for each triangle, we should instead call Load with a list. }

  FKraftMesh.AddTriangle(
    FKraftMesh.AddVertex(VectorToKraft(Position.Data[0])),
    FKraftMesh.AddVertex(VectorToKraft(Position.Data[1])),
    FKraftMesh.AddVertex(VectorToKraft(Position.Data[2])),
    FKraftMesh.AddNormal(VectorToKraft(Normal.Data[0])),
    FKraftMesh.AddNormal(VectorToKraft(Normal.Data[1])),
    FKraftMesh.AddNormal(VectorToKraft(Normal.Data[2]))
  );

  if DoubleSided then
  begin
    { We also add triangles for the other side.
      Kraft RayCast does not check inverted triangles. }
    FKraftMesh.AddTriangle(
      FKraftMesh.AddVertex(VectorToKraft(Position.Data[2])),
      FKraftMesh.AddVertex(VectorToKraft(Position.Data[1])),
      FKraftMesh.AddVertex(VectorToKraft(Position.Data[0])),
      FKraftMesh.AddNormal(VectorToKraft(-Normal.Data[2])),
      FKraftMesh.AddNormal(VectorToKraft(-Normal.Data[1])),
      FKraftMesh.AddNormal(VectorToKraft(-Normal.Data[0]))
    );
  end;
end;

function TCastleMeshCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  if FMesh = nil then
  begin
    Result := nil;
    Exit;
  end;

  if ARigidBody.RigidBodyType <> krbtStatic then
  begin
    WritelnWarning('TCastleMeshCollider "%s" is available only for static rigid bodies.', [
      Name
    ]);
    Result := nil;
    Exit;
  end;

  FKraftMesh := TKraftMesh.Create(APhysics);

  if not FMesh.HasColliderMesh then
  begin
    WritelnWarning('TCastleMeshCollider "%s": Mesh "%s:%s" does not define any collidable triangles (HasColliderMesh is false). Use a different class for the TCastleMeshCollider.Mesh, like TCastleScene.', [
      Name,
      FMesh.Name,
      FMesh.ClassName
    ]);
  end else
  begin
    FMesh.ColliderMesh({$ifdef FPC}@{$endif}AddTriangle);
    if FKraftMesh.CountVertices = 0 then
      WritelnWarning('TCastleMeshCollider "%s": Mesh "%s:%s" is empty (has no collidable triangles)', [
        Name,
        FMesh.Name,
        FMesh.ClassName
      ]);
  end;

  FKraftMesh.Finish;

  Result := TKraftShapeMesh.Create(APhysics, ARigidBody, FKraftMesh);
end;

procedure TCastleMeshCollider.SetMesh(const AValue: TCastleTransform);
begin
  if FMesh <> AValue then
  begin
    { During loading, because of how references are resolved, we may have SetMesh
      temporarily called with a dummy empty TCastleTransform instance.
      Ignore it (do not make warning from TCastleMeshCollider.CreateKraftShape,
      actually don't even set FMesh to don't treat shape as ready to be initialized),
      as this is normal. }
    if IsLoading and
       (AValue.ClassType = TCastleTransform) and
       (AValue.Name = '') then
      Exit;

    FMesh := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleMeshCollider.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleMeshCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AutoSize', 'Mesh', 'DoubleSided'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleMeshCollider.ForceStaticBody: Boolean;
begin
  Result := true;
end;

{ TCastleRigidBody ----------------------------------------------------------- }

constructor TCastleRigidBody.Create(AOwner: TComponent);
begin
  inherited;
  { This behavior needs WorldAttached() WorldDetached() }
  NeedWorldChangeNotification := true;

  FDuringSettingTransformationFromPhysicsEngine := false;

  // properties defaults
  FGravity := true;
  FDynamic := true;
  FExists := true;
  // default damp values from Kraft
  FLinearVelocityDamp := 0.1;
  FAngularVelocityDamp := 0.1;

  FKraftBody := nil;
  FCollisionList := TCastleTransformList.Create(false);
  FPrevCollisions := TCastleRigidBodyList.Create;
  FCurrentCollisions := TCastleRigidBodyList.Create;

  FOnCollisionEnter := nil;
  FOnCollisionExit := nil;
  FTransform := nil;
  FState := stateNone;
  FLockRotation := [];
  FLockTranslation := [];

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlerigidbody_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleRigidBody.Destroy;
begin
  { The FKraftBody will be freed now if you free TCastleRigidBody
    instance explicitly. In most other cases, DeinitializePhysicsEngineObjects
    already freed FKraftBody and set it nil. }

  FreeAndNil(FKraftBody);
  FreeAndNil(FCollider);
  FreeAndNil(FCollisionList);
  FreeAndNil(FPrevCollisions);
  FreeAndNil(FCurrentCollisions);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlerigidbody_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleRigidBody.Setup2D;
begin
  LockTranslation := [2];
  LockRotation := [0, 1];
end;

function ZeroLockedComponents(const V: TVector3;
  const Locked: T3DCoords): TVector3;
begin
  Result := V;
  if 0 in Locked then Result.X := 0;
  if 1 in Locked then Result.Y := 0;
  if 2 in Locked then Result.Z := 0;
end;

procedure TCastleRigidBody.InitializePhysicsEngineObjects;

  procedure RecreateKraftInstance;
  begin
    InitializeEngineRigidBody;

    if TryInitializeColliders then
      FinishInitialization;
  end;

begin
  Assert(FKraftBody = nil, 'Kraft body is initialized!');

  if Parent = nil then
    raise Exception.Create('Cannot initialize physics in behavior not added to transform.');

  if Parent.Parent = nil then
    raise EMultipleReferencesInWorld.Create('Cannot use physics with TCastleTransform instances inserted multiple times into the Viewport.Items. Use TCastleTransformReference to refer to the TCastleTransform multiple times, or clone the instance instead of referencing it multiple times.');

  FTransform := Parent;
  RecreateKraftInstance;
end;

procedure TCastleRigidBody.DeinitializePhysicsEngineObjects;
begin
  Assert(FKraftBody <> nil, 'Second deinitialization!');

  Assert(Parent = FTransform,
    'Parent should be the transform for which physics engine objects was initiated.');

  Assert(Parent.World <> nil,
    'Transform.World should be assigned at the time of TCastleRigidBody.DeinitializePhysicsEngineObjects call');

  Assert(not ((Parent.World.FKraftEngine = nil) and (FKraftBody <> nil)),
    'KraftBody should not live longer than KraftEngine!');

  DeinitializeColliders;

  FreeAndNil(FKraftBody);

  FTransform := nil;
  FState := stateNone;
end;

function TCastleRigidBody.GetCastleColliderFromParent: TCastleCollider;
begin
  if Parent <> nil then
    Result := Parent.FindBehavior(TCastleCollider) as TCastleCollider
  else
    Result := nil;
end;

procedure TCastleRigidBody.UpdateKraftRigidBodyType;

  function ColliderForceStaticBody: Boolean;
  var
    C: TCastleCollider;
  begin
    { We use GetCastleColliderFromParent, not FCastleCollider,
      because FCastleCollider is not assigned yet by caller(s). }
    C := GetCastleColliderFromParent;
    Result := (C <> nil) and C.ForceStaticBody;
  end;

begin
  if FState in [stateNone] then
  begin
    if FKraftBody <> nil then
    begin
      if ColliderForceStaticBody then
        { Some colliders only support static, in which case we use krbtStatic
          regardless of Dynamic/Animated.
          TODO: Are these colliders (mesh and plane) also disable Animated?
          For now we assume yes, but maybe it is no, and we should still honor Animated? }
        FKraftBody.SetRigidBodyType(krbtStatic)
      else
      if FDynamic then
        FKraftBody.SetRigidBodyType(krbtDynamic)
      else
      if FAnimated then
        FKraftBody.SetRigidBodyType(krbtKinematic)
      else
        FKraftBody.SetRigidBodyType(krbtStatic);
    end;
  end else
  begin
    { Bug in Kraft means you can't change body type when it
      was once created so we need to recreate rigid body physics objects. }
    DeinitializePhysicsEngineObjects;
    InitializePhysicsEngineObjects;
  end;
end;

procedure TCastleRigidBody.UpdateKraftGravity;
begin
  if FKraftBody <> nil then
    FKraftBody.GravityScale := Iff(Gravity, 1.0, 0.0);
end;

procedure TCastleRigidBody.InitializeEngineRigidBody;
begin
  World.InitializePhysicsEngine;

  FKraftBody := TKraftRigidBody.Create(World.FKraftEngine);
  FKraftBody.UserData := Self;

  UpdateKraftRigidBodyType;

  FKraftBody.GravityScale := Iff(Gravity, 1.0, 0.0);
  UpdateLockRotation;
  UpdateLockTranslation;

  UpdateExistBody;
  UpdateCollisionDetection;

  if FTrigger then
    FKraftBody.Flags := FKraftBody.Flags + [krbfSensor];

  FState := stateMissingCollider;
end;

function TCastleRigidBody.TryInitializeColliders: Boolean;
var
  CastleColliderFromTransform: TCastleCollider;
begin
  if FState = stateNone then
    raise Exception.Create(
      'TryInitializeColliders: Trying to add collider but rigid body not initialized.');

  CastleColliderFromTransform := GetCastleColliderFromParent;
  if (Collider <> nil) and (CastleColliderFromTransform <> nil) then
    raise Exception.Create(
      'You can''t use TCollider and TCastleCollider in the same rigid body. ' + NL +
      'Remove deprecated RigidBody.Collider.');

  if Collider <> nil then
  begin
    Collider.InitializeKraft(Parent.World.FKraftEngine, FKraftBody);
    FState := stateMissingFinalization;
    Exit(true);
  end;

  if (FCastleCollider <> CastleColliderFromTransform) and (FCastleCollider <> nil) then
  begin
    // Trying to initialize rigid body with another collider without deinitialization
    // of the currrent one - this is a bug in our physics if will occurr
    raise Exception.Create('Reinitialize with new collider but old one not deinitialized.');
  end;

  if CastleColliderFromTransform = nil then
  begin
    FState := stateMissingCollider;
    Exit(false);
  end;

  if not CastleColliderFromTransform.InitializeKraft(Parent.World.FKraftEngine, Self) then
  begin
    FState := stateMissingCollider;
    Exit(false);
  end;

  FCastleCollider := CastleColliderFromTransform;
  FState := stateMissingFinalization;
  Result := true;
end;

procedure TCastleRigidBody.FinishInitialization;
var
  V: TVector3;
begin
  if FState <> stateMissingFinalization then
    raise Exception.Create('Trying finalize rigid body initialization');

  if Collider <> nil then
    FKraftBody.ForcedMass := Collider.Mass
  else
    FKraftBody.ForcedMass := FCastleCollider.Mass;

  FKraftBody.Finish;

  UpdateCollides(FTransform);

  if (not FAngularVelocity.IsPerfectlyZero) or
     (not FLinearVelocity.IsPerfectlyZero) then
  begin
    { The behavior is more natural when we zero the XxxVelocity
      components that are locked. (Otherwise testing e.g. Setup2D in a 3D
      world makes a little unexpected motions). }

    V := ZeroLockedComponents(FAngularVelocity, FLockRotation);
    FKraftBody.AngularVelocity := VectorToKraft(V);

    V := ZeroLockedComponents(FLinearVelocity, FLockTranslation);
    FKraftBody.LinearVelocity := VectorToKraft(V);

    FKraftBody.SetToAwake;
  end;

  FKraftBody.LinearVelocityDamp := FLinearVelocityDamp;
  FKraftBody.MaximalLinearVelocity := FMaximalLinearVelocity / FTransform.World.FKraftEngine.WorldFrequency;

  FKraftBody.AngularVelocityDamp := FAngularVelocityDamp;
  FKraftBody.MaximalAngularVelocity := FMaximalAngularVelocity / FTransform.World.FKraftEngine.WorldFrequency;

  { Set initial transformation }
  SetWorldTransformation(FTransform.WorldTransform);

  CheckPhysicsPostStepNeeded;
  FState := stateReady;
  NotifyOnInitialized;
end;

procedure TCastleRigidBody.CheckInitialization(const ACastleCollider: TCastleCollider);
begin
  if Collider <> nil then
    raise Exception.Create(
      'You can''t use TCollider and TCastleCollider in the same rigid body. ' + NL +
      'Remove deprecated RigidBody.Collider.');

  { FCastleCollider has pointer to castle collider behavior that was used to
    initialize rigid body so we didn't need do anything. }
  if FCastleCollider = ACastleCollider then
    Exit;

  if (FState = stateMissingCollider) then
  begin
    if FCastleCollider = nil then
    begin
      if TryInitializeColliders then
        FinishInitialization;
    end else
    if FCastleCollider <> ACastleCollider then
    begin
      { Trying to initialize rigid body with another collider without deinitialization
        of the currrent one - this is a bug in our physics if will occurr }
      raise Exception.Create('Reinitialize with new collider but old one not deinitialized.');
    end;
  end;
end;

procedure TCastleRigidBody.DeinitializeColliders;

  procedure RemoveSelfFromOtherRigidBodiesCollisionLists;
  var
    OtherRigidBody: TCastleRigidBody;
    CollisionDetails: TPhysicsCollisionDetails;
    ContactPairEdge: PKraftContactPairEdge;
  begin
    if FKraftBody = nil then
      Exit;

    { We don't use here FPrevCollisions list because it can be empty when
      this rigid body don't assign any FOnCollisionXXX event. }
    ContactPairEdge := FKraftBody.ContactPairEdgeFirst;
    while Assigned(ContactPairEdge) do
    begin
      if kcfColliding in ContactPairEdge^.ContactPair^.Flags then
      begin
        OtherRigidBody := TCastleRigidBody(ContactPairEdge^.OtherRigidBody.UserData);

        { We need check that only when other body has OnCollisionExit or
          OnCollisionStay event assigned. }
        if Assigned(OtherRigidBody.FOnCollisionExit) or
           Assigned(OtherRigidBody.FOnCollisionStay) then
        begin
          { First we need send OnCollisionExit event if needed. }
          if Assigned(OtherRigidBody.FOnCollisionExit) then
          begin
            CollisionDetails.Transforms[0] := OtherRigidBody.FTransform;
            CollisionDetails.Transforms[1] := FTransform;
            OtherRigidBody.FOnCollisionExit(CollisionDetails);
          end;

          { Now we can simply remove Self from other rigid body FPrevCollisions list }
          OtherRigidBody.FPrevCollisions.Remove(Self);
          { Currently not needed because it's not used directly from other functions
            than PhysicsPostStep() but this can change in the future. }
          OtherRigidBody.FCurrentCollisions.Remove(Self);
        end;
      end;
      ContactPairEdge := ContactPairEdge^.Next;
    end;
  end;

begin
  if FState = stateReady then
    NotifyBeforeDeinitialized;

  { Deinitialize rigid body when it's in collision with other rigid body make
    this body will be dangling pointer on it's FPrevCollisions list.
    So other rigid body can crash when it try to send FOnCollisionExit event.
    Collisons must be handled before CastleCollider deinitialization because
    kraft resets collisons when you remove collider. }
  if (Collider <> nil) or (FCastleCollider <> nil) then
    RemoveSelfFromOtherRigidBodiesCollisionLists;

  { Collider.FKraftShape is owned by FKraftBody, it was automatically freed already }
  if Collider <> nil then
  begin

    Collider.FKraftShape := nil;
    FState := stateMissingCollider;
  end else
  if FCastleCollider <> nil then
  begin
    if FKraftBody.ShapeCount > 0 then
    begin
      Assert(FKraftBody.ShapeCount = 1, 'More than one shape in rigid body.');
      Assert(FKraftBody.ShapeFirst = FCastleCollider.FKraftShape, 'Mixed kraft shapes');
      FreeAndNil(FCastleCollider.FKraftShape);
    end;
    Assert(FCastleCollider.FKraftShape = nil, 'Mixed kraft shapes');
    FCastleCollider := nil;
    FState := stateMissingCollider;
  end;
end;

procedure TCastleRigidBody.ReinitializeCastleCollider;
begin
  { Don't try reinitialization when TCastleRigidBody is stateNone }
  if FState = stateNone then
    Exit;

  if TryInitializeColliders then
    FinishInitialization;
end;

class procedure TCastleRigidBody.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleRigidBody).Setup2D;
end;

procedure TCastleRigidBody.AddNotificationOnInitialized(
  const AEvent: TComponentEvent);
begin
  if FNotifyOnInitialized = nil then
    FNotifyOnInitialized := TCastleComponentNotification.Create(Self);

  FNotifyOnInitialized.AddNotification(AEvent);
end;

procedure TCastleRigidBody.NotifyOnInitialized;
begin
  if FNotifyOnInitialized = nil then
    Exit;

  FNotifyOnInitialized.Notify(Self);
end;

procedure TCastleRigidBody.RemoveNotificationOnInitialized(
  const AEvent: TComponentEvent);
begin
  if FNotifyOnInitialized = nil then
    Exit;

  FNotifyOnInitialized.RemoveNotification(AEvent);
end;

procedure TCastleRigidBody.AddNotificationBeforeDeinitialized(
  const AEvent: TComponentEvent);
begin
  if FNotifyBeforeDeinitialized = nil then
    FNotifyBeforeDeinitialized := TCastleComponentNotification.Create(Self);

  FNotifyBeforeDeinitialized.AddNotification(AEvent);
end;

procedure TCastleRigidBody.NotifyBeforeDeinitialized;
begin
  if FNotifyBeforeDeinitialized = nil then
    Exit;

  FNotifyBeforeDeinitialized.Notify(Self);
end;

procedure TCastleRigidBody.RemoveNotificationBeforeDeinitialized(
  const AEvent: TComponentEvent);
begin
  if FNotifyBeforeDeinitialized = nil then
    Exit;

  FNotifyBeforeDeinitialized.RemoveNotification(AEvent);
end;

function TCastleRigidBody.IsInitialized: Boolean;
begin
  Result := (FState = stateReady);
end;

procedure TCastleRigidBody.PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
var
  ContactPairEdge: PKraftContactPairEdge;
  RBody: TCastleRigidBody;
  CollisionDetails: TPhysicsCollisionDetails;
  I: Integer;
begin
  FCurrentCollisions.Clear;
  ContactPairEdge := FKraftBody.ContactPairEdgeFirst;
  while Assigned(ContactPairEdge) do
  begin
    { Without this check, OnCollisonEnter reports fake collisons when
      one of colliding body has non regural shape (for example when
      TBoxCollider is rotated).
      kcfColliding = From kraft source comment: "Set when contact
      collides during a step" }
    if kcfColliding in ContactPairEdge^.ContactPair^.Flags then
    begin
      RBody := TCastleRigidBody(ContactPairEdge^.OtherRigidBody.UserData);

      // Do not send events for next collision points with the same body.
      if FCurrentCollisions.IndexOf(RBody) = - 1 then
      begin
        // Add to current collisions.
        FCurrentCollisions.Add(RBody);

        // Prepare collision data.
        CollisionDetails.Transforms[0] := FTransform;
        CollisionDetails.Transforms[1] := RBody.FTransform;

        // New and ongoing collisions.
        if FPrevCollisions.IndexOf(RBody) = -1 then
        begin
          // New collision.
          if Assigned(FOnCollisionEnter) then
            FOnCollisionEnter(CollisionDetails);
        end else
        begin
          // Still in collision.
          if Assigned(FOnCollisionStay) then
            FOnCollisionStay(CollisionDetails);

          // Remove used collision.
          FPrevCollisions.Remove(RBody);
        end;
      end;
    end;

    ContactPairEdge := ContactPairEdge^.Next;
  end;

  // check collision exit
  if Assigned(FOnCollisionExit) then
  begin
    CollisionDetails.Transforms[0] := FTransform;

    for I := 0  to FPrevCollisions.Count - 1 do
    begin
      CollisionDetails.Transforms[1] := TCastleRigidBody(FPrevCollisions[I]).FTransform;
      FOnCollisionExit(CollisionDetails);
    end;
  end;

  // Make previous list from current list.
  FPrevCollisions.Clear;
  FPrevCollisions.AddRange(FCurrentCollisions);
end;

procedure TCastleRigidBody.CheckPhysicsPostStepNeeded;
begin
  if not Assigned(FKraftBody) then
    Exit;

  if Assigned(FOnCollisionEnter) or Assigned(FOnCollisionStay) or Assigned(FOnCollisionExit) then
    FKraftBody.OnPostStep := {$ifdef FPC}@{$endif}PhysicsPostStep
  else
    FKraftBody.OnPostStep := nil;
end;

function TCastleRigidBody.GetKraftShape: TKraftShape;
begin
  if Collider <> nil then
    Result := Collider.FKraftShape
  else
  if FCastleCollider <> nil then
    Result := FCastleCollider.FKraftShape
  else
    Result := nil;
end;

procedure TCastleRigidBody.UpdateCollides(const Transform: TCastleTransform);
const
  AllCollisionGroups = [
    Low (TKraftRigidBodyCollisionGroup)..
    High(TKraftRigidBodyCollisionGroup)];
begin
  if Transform.Collides then
  begin
    FKraftBody.CollideWithCollisionGroups := AllCollisionGroups;
    FKraftBody.CollisionGroups := [0]; // TODO: make this configurable
  end else
  begin
    FKraftBody.CollideWithCollisionGroups := [];
    FKraftBody.CollisionGroups := [];
  end;
end;

procedure TCastleRigidBody.UpdateExist;
var
  KraftShape: TKraftShape;
begin
  UpdateExistBody;

  KraftShape := GetKraftShape;

  if Assigned(KraftShape) then
  begin
    { Note: ksfRayCastable flag determines whether body is detected by PhysicsRayCast. }
    if ExistsInRoot then
      KraftShape.Flags := KraftShape.Flags + [ksfCollision,
        ksfRayCastable]
    else
      KraftShape.Flags := KraftShape.Flags - [ksfCollision,
        ksfRayCastable];
  end;

  UpdateColliderAutosize;
end;

procedure TCastleRigidBody.UpdateExistBody;
begin
  if Assigned(FKraftBody) then
  begin
    if ExistsInRoot then
      FKraftBody.Flags := FKraftBody.Flags + [krbfActive]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfActive];
  end;
end;

procedure TCastleRigidBody.UpdateCollisionDetection;
begin
  if Assigned(FKraftBody) then
  begin
    { We set here krbfContinuous, krbfContinuousAgainstDynamics because
      continuous works only when:
      - one of bodies has krbfContinuous and they are dynamic bodies
      - ContinuousAgainstDynamics is set to true in FKraftEngine
        and one of bodies has krbfContinuousAgainstDynamics flag }
    case FCollisionDetection of
      cdDiscrete:
        FKraftBody.Flags := FKraftBody.Flags - [krbfContinuous, krbfContinuousAgainstDynamics];
      cdContinuous:
        FKraftBody.Flags := FKraftBody.Flags + [krbfContinuous, krbfContinuousAgainstDynamics];
    end;
  end;
end;

procedure TCastleRigidBody.UpdateLockTranslation;
begin
  if Assigned(FKraftBody) then
  begin
    if 0 in FLockTranslation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisX]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockTranslationAxisX];
    if 1 in FLockTranslation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisY]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockTranslationAxisY];
    if 2 in FLockTranslation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisZ]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockTranslationAxisZ];

    { Changing translation lock needs to call Finish again we do it again
      only when rigid body is ready now. }
    if FState = stateReady then
      FKraftBody.Finish;
  end;
end;

procedure TCastleRigidBody.UpdateLockRotation;
begin
  if Assigned(FKraftBody) then
  begin
    if 0 in FLockRotation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisX]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockRotationAxisX];
    if 1 in FLockRotation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisY]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockRotationAxisY];
    if 2 in FLockRotation then
      FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisZ]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfLockRotationAxisZ];

    { Changing rotation lock needs to call Finish again we do it again only when
      rigid body is ready now }
    if FState = stateReady then
      FKraftBody.Finish;
  end;
end;

procedure TCastleRigidBody.SetMaximalAngularVelocity(const AValue: Single);
begin
  FMaximalAngularVelocity := AValue;
  { Kraft uses max velocity for delta time which is physics update frequency. }
  if FKraftBody <> nil then
    FKraftBody.MaximalAngularVelocity := AValue / FTransform.World.FKraftEngine.WorldFrequency;
end;

procedure TCastleRigidBody.SetMaximalLinearVelocity(const AValue: Single);
begin
  FMaximalLinearVelocity := AValue;
  { Kraft uses max velocity for delta time which is physics update frequency. }
  if FKraftBody <> nil then
    FKraftBody.MaximalLinearVelocity := AValue / FTransform.World.FKraftEngine.WorldFrequency;
end;

procedure TCastleRigidBody.SetOnCollisionEnter(const AValue: TCollisionEvent);
begin
  FOnCollisionEnter := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TCastleRigidBody.SetOnCollisionStay(const AValue: TCollisionEvent);
begin
  FOnCollisionStay := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TCastleRigidBody.SetOnCollisionExit(const AValue: TCollisionEvent);
begin
  FOnCollisionExit := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TCastleRigidBody.SetCollisionDetection(const AValue: TCollisionDetection);
begin
  if FCollisionDetection = AValue then
    Exit;

  FCollisionDetection := AValue;
  UpdateCollisionDetection;
end;

procedure TCastleRigidBody.SetDynamic(const AValue: Boolean);
begin
  if FDynamic <> AValue then
  begin
    FDynamic := AValue;
    UpdateKraftRigidBodyType;
  end;
end;

procedure TCastleRigidBody.SetAnimated(const AValue: Boolean);
begin
  if FAnimated <> AValue then
  begin
    FAnimated := AValue;
    UpdateKraftRigidBodyType;
  end;
end;

procedure TCastleRigidBody.SetGravity(const AValue: Boolean);
begin
  if FGravity <> AValue then
  begin
    FGravity := AValue;
    UpdateKraftGravity;
  end;
end;

procedure TCastleRigidBody.SetAngularVelocityDamp(const AValue: Single);
begin
  FAngularVelocityDamp := AValue;
  if FKraftBody <> nil then
    FKraftBody.AngularVelocityDamp := AValue;
end;

procedure TCastleRigidBody.SetLinearVelocityDamp(const AValue: Single);
begin
  FLinearVelocityDamp := AValue;
  if FKraftBody <> nil then
    FKraftBody.LinearVelocityDamp := AValue;
end;

procedure TCastleRigidBody.SynchronizeFromKraft;
begin
  if FKraftBody = nil then
    Exit;

  FLinearVelocity := VectorFromKraft(FKraftBody.LinearVelocity);
  FAngularVelocity := VectorFromKraft(FKraftBody.AngularVelocity);
end;

procedure TCastleRigidBody.SetWorldTransformation(const WorldTransform: TMatrix4
  );
var
  Translation, Scale: TVector3;
  Rotation: TVector4;
  T: TTransformation;
begin
  { If we just set transformation from physics engine this is a guard
    to not update rigid body transformation. See TransformationFromKraft }
  if FDuringSettingTransformationFromPhysicsEngine then
    Exit;

  if not (FState in [stateMissingFinalization, stateReady]) then
    Exit;

  { Remove scale from world transfrom because using scale in kraft
    leads to an undefined state. }
  MatrixDecompose(WorldTransform, Translation, Rotation, Scale);

  T.Init;
  T.Multiply(Rotation, NoScale, Translation);

  FKraftBody.SetWorldTransformation(MatrixToKraft(T.Transform));

  { Synchronize Kraft rigid body and shapes (colliders) transform to make
    collider position correct. Without the SynchronizeTransformIncludingShapes,
    first (before physics step) TransformationFromKraft
    would set weird TCastleTransform position.

    Testcase: run physics_2d_collisions, press space (pause) and R (restart).
    The plane should jump to predictable initial position (TPlane.Create).
    Without the SynchronizeTransformIncludingShapes call, it jumps to
    an incorrect position.

    Also, because we use shape InterpolatedWorldTransform we need to store
    it for correct result. }
  FKraftBody.SynchronizeTransformIncludingShapes;
  GetKraftShape.StoreWorldTransform;
end;

procedure TCastleRigidBody.SetLockTranslation(const AValue: T3DCoords);
begin
  if FLockTranslation = AValue then
    Exit;

  FLockTranslation := AValue;
  UpdateLockTranslation;
end;

procedure TCastleRigidBody.SetLockRotation(const AValue: T3DCoords);
begin
  if FLockRotation = AValue then
    Exit;

  FLockRotation := AValue;
  UpdateLockRotation;
end;

procedure TCastleRigidBody.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  // function TranslationFromMatrix(const M: TMatrix4): TVector3;
  // begin
  //   Result := PVector3(@M.Data[3])^;
  // end;

  { Update current transformation from Kraft rigid body parameters. }
  procedure TransformationFromKraft;
  var
    // Q: TKraftQuaternion;
    // Axis: TKraftVector3;
    // Angle: TKraftScalar;
    Shape: TKraftShape;
    LocalTransform: TMatrix4;
    Translation, Scale: TVector3;
    Rotation: TVector4;
  begin
    FDuringSettingTransformationFromPhysicsEngine := true;
    try
      Shape := GetKraftShape;

      if Shape = nil then
        Exit;

      LocalTransform := Parent.CheckParent.WorldInverseTransform *
        MatrixFromKraft(Shape.InterpolatedWorldTransform);
      if (FCollider = nil) and (FCastleCollider <> nil) and
         (FCastleCollider.FUseLocalTransform) then
        LocalTransform := LocalTransform * FCastleCollider.FLocalInverseTransform;

      { Don't use Scale form kraft it's always 1.
        See SetWorldTransformation() for more information. }
      MatrixDecompose(LocalTransform, Translation, Rotation, Scale);
      Parent.Rotation := Rotation;
      Parent.Translation := Translation;

      { This is an alternative version using Kraft to do matrix decomposition. }
      // Q := QuaternionFromMatrix4x4(MatrixToKraft(LocalTransform));
      // QuaternionToAxisAngle(Q, Axis, Angle);
      // Transform.Rotation := Vector4(Axis.X, Axis.Y, Axis.Z, Angle);

      // Transform.Translation := TranslationFromMatrix(LocalTransform);
    finally
      FDuringSettingTransformationFromPhysicsEngine := false;
    end;
  end;

begin
  inherited;

  { Disable updating physics in design mode }
  if CastleDesignMode and (CastleDesignPhysicsMode in [pmStopped, pmPaused]) then
    Exit;

  if Parent = nil then
    Exit;

  if (Parent.World <> nil) and (not Parent.World.EnablePhysics) then
    Exit;

  UpdateCollides(Parent);
  if Dynamic then
  begin
    { This is not called with Animated because Kraft Kinematic Bodies are never
      updated by physics engine. }
    TransformationFromKraft;
    SynchronizeFromKraft;
  end

  { Animated: I think we don't need do anything here because when
      TCastleTransform transformation changes SetWorldTransformation() is
      called in TCastleTransform.ChangedTransform. }
end;

function TCastleRigidBody.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleRigidBody) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleRigidBody behavior can be added to a given TCastleTransform';
    Result := false;
  end;
end;

procedure TCastleRigidBody.WorldAfterAttach;
begin
  inherited;

  if World <> nil then
    InitializePhysicsEngineObjects;
end;

procedure TCastleRigidBody.WorldBeforeDetach;
begin
  if FState <> stateNone then
  begin
    Assert(Parent = FTransform, 'Parent and FTransform should be the same.');
    DeinitializePhysicsEngineObjects;
  end;

  inherited;
end;

function TCastleRigidBody.GetCollidingTransforms: TCastleTransformList;
var
  ContactPairEdge: PKraftContactPairEdge;
  CastleTransform:TCastleTransform;
begin
  Result := FCollisionList;
  FCollisionList.Clear;
  ContactPairEdge := FKraftBody.ContactPairEdgeFirst;
  while Assigned(ContactPairEdge) do
  begin
    if not (kcfColliding in ContactPairEdge^.ContactPair^.Flags) then
    begin
      ContactPairEdge := ContactPairEdge^.Next;
      continue;
    end;

    CastleTransform := (TCastleRigidBody(ContactPairEdge^.OtherRigidBody.UserData)).FTransform;
    if FCollisionList.IndexOf(CastleTransform) = -1 then
      FCollisionList.Add(CastleTransform);
    ContactPairEdge := ContactPairEdge^.Next;
  end;
end;

function TCastleRigidBody.PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
  const MaxDistance: Single): TCastleTransform;
var
  IgnoredDistance: Single;
begin
  Result := PhysicsRayCast(RayOrigin, RayDirection, MaxDistance, IgnoredDistance);
end;

function TCastleRigidBody.PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
  const MaxDistance: Single; out Distance: Single): TCastleTransform;
var
  Shape, RayShape: TKraftShape;
  ResultingDistance: TKraftScalar;
  Point: TKraftVector3;
  Normal: TKraftVector3;
  OldShapeFlags: TKraftShapeFlags;
  RayOriginWorld, RayDirectionWorld: TVector3;
  RayMaxDistanceWorld: Single;
  Hit: Boolean;
begin
  if FTransform = nil then
  begin
    WritelnWarning(
      'Attempt to cast a ray from TCastleRigidBody not connected to TCastleTransform. Maybe you forgot to assign to TCastleTransform.RigidBody?');
    Exit(nil);
  end;

  Shape := GetKraftShape;

  RayOriginWorld := FTransform.Parent.LocalToWorld(RayOrigin);
  RayDirectionWorld := FTransform.Parent.LocalToWorldDirection(RayDirection);
  RayMaxDistanceWorld := FTransform.Parent.LocalToWorldDistance(MaxDistance);

  { We use ksfRayCastable flag to not hit to caster shape. }
  if Shape <> nil then
    OldShapeFlags := Shape.Flags;
  try
    if Shape <> nil then
      Shape.Flags := Shape.Flags - [ksfRayCastable];
    { Note: In Kraft, the distance parameters are called "time"
      (MaxTime, Time instead of more natural MaxDistance, Distance).
      But all research shows that it is actually "distance" and that is also how
      other physics engines call it.
      TODO: Check time depends on physics frequency? }
    Hit := FTransform.World.FKraftEngine.RayCast(VectorToKraft(RayOriginWorld),
      VectorToKraft(RayDirectionWorld), RayMaxDistanceWorld, RayShape, ResultingDistance, Point, Normal);

    if Hit then
    begin
      Distance := FTransform.Parent.WorldToLocalDistance(ResultingDistance);
      Result := TCastleRigidBody(RayShape.RigidBody.UserData).FTransform;
    end else
      Result := nil;
  finally
    if Shape <> nil then
      Shape.Flags := OldShapeFlags;
  end;
end;

function TCastleRigidBody.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'Dynamic', 'Animated', 'Trigger', 'Gravity', 'LockTranslation', 'Exists',
     'AngularVelocityPersistent', 'AngularVelocityDamp', 'MaximalAngularVelocity',
     'LinearVelocityPersistent', 'LinearVelocityDamp', 'MaximalLinearVelocity',
     'CollisionDetection', 'LockRotation'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleRigidBody.AddForce(const Force, Position: TVector3);
begin
  if FKraftBody <> nil then
  begin
    if ExistsInRoot then
    begin
      FKraftBody.AddForceAtPosition(VectorToKraft(Force), VectorToKraft(Position));
      WakeUp;
    end;
  end else
    WritelnWarning('Attempting to add force before physics engine initialization.');
end;

procedure TCastleRigidBody.AddCentralForce(const Force: TVector3);
begin
  if FKraftBody <> nil then
  begin
    if ExistsInRoot then
    begin
      FKraftBody.AddBodyForce(VectorToKraft(Force));
      WakeUp;
    end;
  end else
    WritelnWarning('Attempting to add force before physics engine initialization.');
end;

procedure TCastleRigidBody.AddTorque(const Torque: TVector3);
begin
  if FKraftBody <> nil then
  begin
    if ExistsInRoot then
    begin
      FKraftBody.AddWorldTorque(VectorToKraft(Torque));
      WakeUp;
    end;
  end else
    WritelnWarning('Attempting to add torque before physics engine initialization.');
end;

procedure TCastleRigidBody.ApplyImpulse(const Impulse, Point: TVector3);
begin
  if FKraftBody <> nil then
  begin
    if ExistsInRoot then
    begin
      FKraftBody.ApplyImpulseAtPosition(VectorToKraft(Point), VectorToKraft(Impulse));
      WakeUp;
    end;
  end else
    WritelnWarning('Attempting to apply impulse before physics engine initialization.');
end;

function TCastleRigidBody.ExistsInRoot: Boolean;
begin
  if Exists = false then
    Exit(false);

  if Parent = nil then
    Exit(false);

  Result := Parent.ExistsInRoot;
end;

procedure TCastleRigidBody.UpdateColliderAutosize;
begin
  if ExistsInRoot and (FCastleCollider <> nil) then
  begin
    FCastleCollider.InternalAutoSize;
  end;
end;

procedure TCastleRigidBody.WakeUp;
begin
  FKraftBody.SetToAwake;
end;

{ // commented out as should never be needed by actual user code

procedure TCastleRigidBody.Sleep;
begin
  FKraftBody.SetToSleep;
end;
}

procedure TCastleRigidBody.SetLinearVelocity(const LVelocity: TVector3);
begin
  FLinearVelocity := LVelocity;
  if FKraftBody <> nil then
  begin
    FKraftBody.LinearVelocity := VectorToKraft(ZeroLockedComponents(LVelocity, FLockTranslation));
    if not LVelocity.IsPerfectlyZero then
      FKraftBody.SetToAwake;
  end;
end;

procedure TCastleRigidBody.SetAngularVelocity(const AVelocity: TVector3);
begin
  FAngularVelocity := AVelocity;
  if FKraftBody <> nil then
  begin
    FKraftBody.AngularVelocity := VectorToKraft(ZeroLockedComponents(AVelocity, FLockTranslation));
    if not AVelocity.IsPerfectlyZero then
      FKraftBody.SetToAwake;
  end;
end;

procedure TCastleRigidBody.SetExists(const Value: Boolean);
begin
  if FExists = Value then
    Exit;

  FExists := Value;

  if FTransform <> nil then
    UpdateExist;
end;

procedure TCastleRigidBody.SetTrigger(const Value: Boolean);
begin
  if FTrigger = Value then
    Exit;

  FTrigger := Value;

  if Assigned(FKraftBody) then
  begin
    if FTrigger then
      FKraftBody.Flags := FKraftBody.Flags + [krbfSensor]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfSensor];
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlerigidbody_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleTransform ---------------------------------------------------------- }

procedure TCastleTransform.SetRigidBody(const Value: TCastleRigidBody);
var
  OldRigidBody: TCastleRigidBody;
begin
  OldRigidBody := GetRigidBody;
  if OldRigidBody = Value then
     Exit;

  if OldRigidBody <> nil then
  begin
    RemoveBehavior(OldRigidBody);
    FreeAndNil(OldRigidBody);
  end;

  if Value <> nil then
    AddBehavior(Value);
end;

function TCastleTransform.GetRigidBody: TCastleRigidBody;
begin
  Result := FindBehavior(TCastleRigidBody) as TCastleRigidBody;
end;

{ TCastleAbstractRootTransform ------------------------------------------------------------------- }

procedure TCastleAbstractRootTransform.InitializePhysicsEngine;
begin
  if FKraftEngine = nil then
  begin
    FKraftEngine := TKraft.Create(-1);
    { Kraft sets MaximalLinearVelocity in TKraft Constructor to 2.
      With this limit can't make velocity greater than about 120
      (2 * engine step frequency = 2 * 60 = 120). That makes physics
      very slow, so we need remove this limitation. }
    FKraftEngine.MaximalLinearVelocity := 0;
    FKraftEngine.SetFrequency(PhysicsProperties.Frequency);
    FKraftEngine.AngularVelocityRK4Integration := PhysicsProperties.AngularVelocityRK4Integration;
    FKraftEngine.LinearVelocityRK4Integration := PhysicsProperties.LinearVelocityRK4Integration;
    if PhysicsProperties.ContinuousCollisionDetection then
    begin
      { Andrzej says: I tested all options:
         - kcmSpeculativeContacts - its fake so sometime we have fake collisons
         - kcmTimeOfImpactSubSteps - bullets in platformer sometimes pass
                                     through the walls, all TimeOfImpactAlgorithm
                                     algorithm have this issue
         - kcmMotionClamping - works best I think (but may lead to a momentary
                               loss of time in a collision)
         Maybe it can be worth to give user ability to choose what he need }
      FKraftEngine.ContinuousMode := kcmMotionClamping;
      { We need set this because continuous works only when:
        - one of bodies has krbfContinuous and they are dynamic
        - ContinuousAgainstDynamics is set to true in
        FKraftEngine and one of bodies has krbfContinuousAgainstDynamics flag }
      FKraftEngine.ContinuousAgainstDynamics := true;
      { Only for kcmTimeOfImpactSubSteps mode }
      // FKraftEngine.TimeOfImpactAlgorithm := ktoiaConservativeAdvancement;
      // FKraftEngine.MaximalSubSteps := 16;
    end else
    begin
      FKraftEngine.ContinuousMode := kcmNone;
      FKraftEngine.ContinuousAgainstDynamics := false;
    end;
  end;
end;

procedure TCastleAbstractRootTransform.DestroyPhysicsEngine;
var
  CastleRigidBody: TCastleRigidBody;
  KraftRigidBody: TKraftRigidBody;
  NextKraftRigidBody: TKraftRigidBody;
begin
  if FKraftEngine <> nil then
  begin
    KraftRigidBody := FKraftEngine.RigidBodyFirst;

    while Assigned(KraftRigidBody) do
    begin
      NextKraftRigidBody := KraftRigidBody.RigidBodyNext;
      if Assigned(KraftRigidBody.UserData) then
      begin
        CastleRigidBody := TCastleRigidBody(KraftRigidBody.UserData);
        CastleRigidBody.DeinitializePhysicsEngineObjects;
      end;
      KraftRigidBody := NextKraftRigidBody;
    end;

    FreeAndNil(FKraftEngine);
  end;
end;

procedure TCastleAbstractRootTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  KraftGravity: TVector3;
  PhysicsTicksCount:Integer;
  OldTimeAccumulator: TFloatTime;
begin
  if not Exists then Exit;

  { Avoid doing this two times within the same FrameId.
    Important if the same TCastleAbstractRootTransform is present in multiple viewports. }
  if UpdateFrameId = TFramesPerSecond.FrameId then
    Exit;
  UpdateFrameId := TFramesPerSecond.FrameId;

  if IsPhysicsRunning then
  begin
    FrameProfiler.Start(fmUpdatePhysics);

    // update FKraftEngine.Gravity
    // TODO: do we really need to be prepared that it changes each frame?
    KraftGravity := -GravityUp * PhysicsProperties.GravityStrength;
    FKraftEngine.Gravity.Vector := VectorToKraft(KraftGravity);

    PhysicsTicksCount := 0;

    if not WasPhysicsStep then
    begin
      FKraftEngine.StoreWorldTransforms;
      FKraftEngine.InterpolateWorldTransforms(0.0);
      WasPhysicsStep := true;
    end else
    begin
      TimeAccumulator := TimeAccumulator + SecondsPassed;
      while TimeAccumulator >= PhysicsProperties.FPhysicsTimeStep do
      begin
        TimeAccumulator := TimeAccumulator - PhysicsProperties.FPhysicsTimeStep;
        FKraftEngine.StoreWorldTransforms;
        FKraftEngine.Step(PhysicsProperties.FPhysicsTimeStep);

        Inc(PhysicsTicksCount);

        { To avoid the spiral of death, we ignore some accumulated time
          (we will not account for this time in the physics simulation,
          so physics simulation may be slower than time perceived by user,
          than non-physics animations etc.).
          An alternative approach would be to prolong the simulation step
          sometimes, but this could lead to unreliable collision detection.
          See description of this in
          https://github.com/castle-engine/castle-engine/pull/144#issuecomment-562850820 }
        if (TimeAccumulator >= PhysicsProperties.FPhysicsTimeStep) and (PhysicsProperties.MaxPhysicsTicksPerUpdate <> 0) and
           (PhysicsTicksCount >= PhysicsProperties.MaxPhysicsTicksPerUpdate) then
        begin
          OldTimeAccumulator := TimeAccumulator;
          TimeAccumulator := TimeAccumulator - (PhysicsProperties.FPhysicsTimeStep * Floor(TimeAccumulator / PhysicsProperties.FPhysicsTimeStep));

          // This log is valid, but too spammy now.
          // WritelnLog('Max physics ticks in TCastleAbstractRootTransform.Update() exceeded ('
          //   + IntToStr(PhysicsTicksCount) + '). TimeAccumulator reduced from '
          //   + FloatToStrDot(OldTimeAccumulator) + ' to ' + FloatToStrDot(TimeAccumulator));

          break;
        end;
      end;

      { One can wonder why we do interpolate below between
        - previous-to-last calculated physics state
        - and the last calculated physics state

        It seems that we should interpolate instead between
        - the last calculated physics state
        - and the "future" physics state, so we should make one more
          "FKraftEngine.Step" call (and remember to make one less
          "FKraftEngine.Step" call in the next loop run).

        This contains an explanation:
        http://web.archive.org/web/20160205035208/http://gafferongames.com/game-physics/fix-your-timestep/

        """
        You’re actually delaying the simulation by one frame
        and then doing the interpolation to ensure smoothness.
        """

        (The original https://gafferongames.com/post/fix_your_timestep/ no longer
        has comments.)
      }
      FKraftEngine.InterpolateWorldTransforms(TimeAccumulator / PhysicsProperties.FPhysicsTimeStep);
    end;
    FrameProfiler.Stop(fmUpdatePhysics);
  end;

  { call inherited at the end,
    to update transformation of all items in their TCastleRigidBody.Update
    called from TCastleTransform.Update }
  inherited;
end;

{$endif read_implementation}
