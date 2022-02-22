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

{ Physics integration code between CastleTransform and Kraft physics engine. }

{$ifdef read_interface}

  { Information send along with TCollisionEvent event, like
    TRigidBody.OnCollisionEnter, TRigidBody.OnCollisionExit. }
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

    procedure SetAngularVelocityRK4Integration(const AValue: Boolean);
    procedure SetLinearVelocityRK4Integration(const AValue: Boolean);
    procedure SetFrequency(const AValue: Single);

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
  end;

  { Don't have any Kraft objects only describe shape }

  { TCastleCollider }

  TCastleCollider = class(TCastleBehavior)
  strict private
    FDensity, FMass, FRestitution, FFriction: Single;
    { When @false, behave as if Translation/Rotation/Scale/FLocalTransform/FLocalInverseTransform
      were all "identity", i.e. "no transformation".
      This is an often case, so we want to optimize it. }
    FTranslation: TVector3;
    FRotation: TVector4;
    FScale: TVector3;
  private
    FKraftShape: TKraftShape;  // owned by TKraftRigidBody
    FUseLocalTransform: Boolean;
    FLocalTransform: TMatrix4;
    FLocalInverseTransform: TMatrix4;

    { Updates FLocalTransformation and FUseLocalTransformation after changing
      Translation, Scale, Rotation and in InitializeKraft. }
    procedure UpdateLocalTransform;

    procedure InitializeKraft(const APhysics: TKraft;
      const ACastleRigidBody: TCastleRigidBody);

    procedure SetDensity(const AValue: Single);
    procedure SetMass(const AValue: Single);
    procedure SetRestitution(const AValue: Single);
    procedure SetFriction(const AValue: Single);
    procedure SetTranslation(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector4);
    procedure SetScale(const AValue: TVector3);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;

    { Notify CastleRigidBody that kraft shape need be recreated }
    procedure ShapeChangedNotifyRigidBody;
  protected
    procedure ParentAfterAttach; override;
    procedure ParentBeforeDetach; override;
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

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Translation: TVector3 read FTranslation write SetTranslation;
    property Rotation: TVector4 read FRotation write SetRotation;
    property Scale: TVector3 read FScale write SetScale;
  published
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

    property Restitution: Single read FRestitution write SetRestitution
      {$ifdef FPC}default DefaultRestitution{$endif};
    property Friction: Single read FFriction write SetFriction
      {$ifdef FPC}default DefaultFriction{$endif};
  end;

  { Shape used for collision detection of a rigid body @link(TRigidBody). }
  TCollider = class
  private
    FKraftShape: TKraftShape;
    FParent: TRigidBody;
    FDensity, FMass, FRestitution, FFriction: Single;
    procedure InitializeKraft(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;
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

    { Create a collider, and set the parent rigid body.
      The collider instance is @bold(owned) by the @link(TRigidBody) instance,
      so will be freed when the @link(TRigidBody) is freed.
      This automatically sets the
      @link(TRigidBody.Collider) property and
      @link(TCollider.Parent) property. }
    constructor Create(const AParent: TRigidBody);
    destructor Destroy; override;

    { Parent rigid body, which in turn refers to this collider
      by @link(TRigidBody.Collider). }
    property Parent: TRigidBody read FParent;

    { Density (per volume) in kg, this implicitly determines mass
      (volume is automatically calculated by the physics engine).
      If you instead prefer to explicitly set mass, set @link(Mass)
      property to something non-zero. }
    property Density: Single read FDensity write FDensity
      {$ifdef FPC}default DefaultDensity{$endif};

    { Mass in kg. When non-zero, this overrides the automatically calculated
      mass from the volume and @link(Density). }
    property Mass: Single read FMass write FMass
      {$ifdef FPC}default DefaultMass{$endif};

    property Restitution: Single read FRestitution write FRestitution
      {$ifdef FPC}default DefaultRestitution{$endif};
    property Friction: Single read FFriction write FFriction
      {$ifdef FPC}default DefaultFriction{$endif};
  end deprecated 'use TCastleCollider';


  { Collide as an infinite plane.
    Place this inside @link(TRigidBody.Collider) property. }
  TPlaneCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Normal: TVector3;
    Distance: Single;
  end deprecated 'use TCastlePlaneCollider';

  { Collide as an infinite plane.
    Add this to transform behavior list. }

  { TCastlePlaneCollider }

  TCastlePlaneCollider = class(TCastleCollider)
  strict private
    FNormal: TVector3;
    FDistance: Single;

    procedure SetNormal(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Normal: TVector3 read FNormal write SetNormal;
  published
    property Distance: Single read FDistance write SetDistance;
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  { Collide as a box.
    Place this inside @link(TRigidBody.Collider) property. }
  TBoxCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Size: TVector3;
  end deprecated 'use TCastleBoxCollider';

  { Collide as a box.
    Add this to transform behavior list. }

  { TCastleBoxCollider }

  TCastleBoxCollider = class(TCastleCollider)
  strict private
    FSize: TVector3;

    procedure SetSize(const AValue: TVector3);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Size: TVector3 read FSize write SetSize;
  published
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
    {$undef read_interface_class}
  end;


  { Collide as a sphere.
    Place this inside @link(TRigidBody.Collider) property. }

  { TSphereCollider }

  TSphereCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
  end deprecated 'use TCastleSphereCollider';

  { Collide as a sphere.
    Add this to transform behavior list.}

  { TCastleSphereCollider }

  TCastleSphereCollider = class(TCastleCollider)
  strict private
    FRadius: Single;

    procedure SetRadius(const AValue: Single);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Radius: Single read FRadius write SetRadius;
  end;

  { Collide as a capsule.
    Place this inside @link(TRigidBody.Collider) property. }
  TCapsuleCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
    Height: Single;
  end deprecated 'use TCastleCapsuleCollider';

  { Collide as a capsule.
    Add this to transform behavior list.}

  { TCastleCapsuleCollider }

  TCastleCapsuleCollider = class(TCastleCollider)
  strict private
    FRadius: Single;
    FHeight: Single;

    procedure SetRadius(const AValue: Single);
    procedure SetHeight(const AValue: Single);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Radius: Single read FRadius write SetRadius;
    property Height: Single read FHeight write SetHeight;
  end;

  T3DCoord = 0..2;
  T3DCoords = set of T3DCoord;

  TCollisionEvent = procedure (const CollisionDetails: TPhysicsCollisionDetails) of object;
  TOnCollision = TCollisionEvent deprecated 'use TCollisionEvent';

  TRigidBodyList = {$ifdef FPC}specialize{$endif} TList<TRigidBody>;
  TCastleRigidBodyList = {$ifdef FPC}specialize{$endif} TList<TCastleRigidBody>;

  { Rigid body behavior for the physics engine,
    see @link(TCastleTransform.RigidBody). }

  { TCastleRigidBody }

  TCastleRigidBody = class(TCastleBehavior)
  strict private
    type
      TCastleRigidBodyState = (
        crbsNotInitialized, // nothing is done
        crbsNoCollider, // need initialize collider
        crbsNoFinalization, // need FinishInitialization
        crbsInitialized // ready to work
        );
    var
      { Current rigid body state. }
      FState: TCastleRigidBodyState;
      { Castle collider behavior that was used to initialize rigid body }
      FCastleCollider: TCastleCollider;

      { List of collisions from previous step. }
      FPrevCollisions: TCastleRigidBodyList;
      { List of collisions from current step. }
      FCurrentCollisions: TCastleRigidBodyList;

      FOnCollisionEnter: TCollisionEvent;
      FOnCollisionStay: TCollisionEvent;
      FOnCollisionExit: TCollisionEvent;

    procedure PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
    { Assign or unassign PhysicsPostStep in TKraftBody.OnPostStep when needed. }
    procedure CheckPhysicsPostStepNeeded;

    function GetKraftShape: TKraftShape;

    { Returns TCastleCollider bahavior or nil when no collider behavior in
      Parent or Parent = nil. In most cases you should use FCastleCollider
      reference to behavior that was used to initialize rigid body }
    function GetCastleColliderFromParent: TCastleCollider;
  private

    FKraftBody: TKraftRigidBody;
    FCollider: TCollider;
    FGravity: Boolean;
    FDynamic: Boolean;
    FAnimated: Boolean;
    FTrigger: Boolean;
    FExists: Boolean;
    FLockTranslation: T3DCoords;
    FLockRotation: T3DCoords;
    FAngularVelocity: TVector3;
    FAngularVelocityDamp: Single;
    FMaximalAngularVelocity: Single;
    FLinearVelocity: TVector3;
    FLinearVelocityDamp: Single;
    FMaximalLinearVelocity: Single;
    FTransform: TCastleTransform;
    FCollisionList: TCastleTransformList;

    procedure UpdateCollides(const Transform: TCastleTransform);

    { Sets all values from Kraft to CGE private fields. }
    procedure SynchronizeFromKraft;

    procedure SetAngularVelocity(const AVelocity: TVector3);
    procedure SetAngularVelocityDamp(const AValue: Single);
    procedure SetMaximalAngularVelocity(const AValue: Single);

    procedure SetLinearVelocity(const LVelocity: TVector3);
    procedure SetLinearVelocityDamp(const AValue: Single);
    procedure SetMaximalLinearVelocity(const AValue: Single);

    procedure SetOnCollisionEnter(const AValue: TCollisionEvent);
    procedure SetOnCollisionStay(const AValue: TCollisionEvent);
    procedure SetOnCollisionExit(const AValue: TCollisionEvent);

    procedure SetExists(const Value: Boolean);
    procedure SetTrigger(const Value: Boolean);

    { Connect this rigid body with Transform and Transform.World. }
    procedure InitializeTransform(const Transform: TCastleTransform);
    { Disconnect from Transform.World, if was connected. }
    procedure DeinitializeTransform(const Transform: TCastleTransform);

    { Initialize rigid body objects from physics engine }
    procedure InitializeEngineRigidBody(const Transform: TCastleTransform);
    { Initialize collider from physics engine }
    function TryInitializeColliders: Boolean;
    { Things to initialize after collider initialization }
    procedure FinishInitialization(const Transform: TCastleTransform);
    { Used by TCastleCollider when it's parent changes to handle case when
      rigid body is added before collider behavior }
    procedure CheckInitialization(const ACastleCollider: TCastleCollider);

    procedure DeinitializeColliders;

    { Called by Collider when it's shape changes }
    procedure ReinitializeCastleCollider;

    class procedure CreateComponent2D(Sender: TObject);
  protected
    procedure WorldAfterAttach; override;

    procedure WorldBeforeDetach; override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
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
      Only collisions with the physics colliders (defined inside @link(TRigidBody.Collider))
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

    property AngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;
    property AngularVelocityDamp: Single read FAngularVelocityDamp write SetAngularVelocityDamp;
    property MaximalAngularVelocity: Single read FMaximalAngularVelocity write SetMaximalAngularVelocity;

    property LinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;
    property LinearVelocityDamp: Single read FLinearVelocityDamp write SetLinearVelocityDamp;
    property MaximalLinearVelocity: Single read FMaximalLinearVelocity write SetMaximalLinearVelocity;

    {$ifdef FPC}
    property InitialAngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;
      deprecated 'use AngularVelocity';
    property InitialLinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;
      deprecated 'use LinearVelocity';
    {$endif}

    { Occurs when TRigidBody starts colliding with another TRigidBody.

      It can occur repeatedly for the same body (in the same time instant)
      if the collision has many points of contact.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleUserInterface.OnUpdate)). }
    property OnCollisionEnter: TCollisionEvent read FOnCollisionEnter write SetOnCollisionEnter;

    { Occurs when TRigidBody stops colliding with another TRigidBody.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleUserInterface.OnUpdate)). }
    property OnCollisionExit: TCollisionEvent read FOnCollisionExit write SetOnCollisionExit;

    { Occurs when TRigidBody still collides with another TRigidBody.

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
    property Dynamic: Boolean read FDynamic write FDynamic default true;

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
    property Animated: Boolean read FAnimated write FAnimated default false;

    { Triggers report when other object collides with them,
      but still allow the other object to pass through.
      In other words, colliding with a trigger will not cause the collider to stop or
      "bounce off" the trigger.

      They are useful as sensors. E.g. a trigger may be a coin (like in "Mario")
      that the player can "consume" by colliding with it.

      Triggers report collisions through the same events as other rigid bodies:
      (@link(TRigidBody.OnCollisionEnter), @link(TRigidBody.OnCollisionStay),
      @link(TRigidBody.OnCollisionExit)). }
    property Trigger: Boolean read FTrigger write SetTrigger default false;

    { Shape used for collisions with this object.
      You cannot assign this property directly,
      it is automatically assigned when you create a @link(TCollider)
      specifying this @link(TRigidBody) as it's parent. }
    property Collider: TCollider read FCollider;

    { Is this object affected by gravity. }
    property Gravity: Boolean read FGravity write FGravity default true;

    { Disable motion (@link(TCastleTransform.Translation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable motion along the Z axis.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockTranslation: T3DCoords read FLockTranslation write FLockTranslation;

    { Disable rotation (@link(TCastleTransform.Rotation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable rotation along the X and Y axes.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockRotation: T3DCoords read FLockRotation write FLockRotation;

    { Controls whether the rigid body is actually processed by the physics engine.
      When this is @false, the rigid body is not updated by the physics engine,
      and it does not cause collisions with other rigid bodies. }
    property Exists: Boolean read FExists write SetExists default true;
  end;

  TRigidBody = class(TCastleRigidBody)
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

{ TCapsuleCollider ----------------------------------------------------------- }

function TCapsuleCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeCapsule.Create(APhysics, ARigidBody, Radius, Height);
end;

{ TCastleCapsuleCollider ----------------------------------------------------- }

procedure TCastleCapsuleCollider.SetRadius(const AValue: Single);
begin
  if not SameValue(FRadius, AValue) then
  begin
    FRadius := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleCapsuleCollider.SetHeight(const AValue: Single);
begin
  if not SameValue(FHeight, AValue) then
  begin
    FHeight := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleCapsuleCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeCapsule.Create(APhysics, ARigidBody, FRadius, FHeight);
end;

function TCastleCapsuleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Radius') or
     (PropertyName = 'Height') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
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
end;

{ TCollider ------------------------------------------------------------------ }

constructor TCollider.Create(const AParent: TRigidBody);
begin
  inherited Create;

  // properties defaults
  FDensity := DefaultDensity;
  FMass := DefaultMass;
  FRestitution := DefaultRestitution;
  FFriction := DefaultFriction;

  FParent := AParent;
  FreeAndNil(FParent.FCollider);
  FParent.FCollider := Self;
end;

procedure TCollider.InitializeKraft(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody);
begin
  // FreeAndNil(FKraftShape); // do not free here, TKraftShape is owned by TKraftRigidBody
  FKraftShape := CreateKraftShape(APhysics, ARigidBody);
  // assign common TCollider properties to Kraft shape
  FKraftShape.Density := Density;
  FKraftShape.Restitution := Restitution;
  FKraftShape.Friction := Friction;
  if not FParent.Exists then
    FKraftShape.Flags := FKraftShape.Flags - [ksfCollision];
end;

destructor TCollider.Destroy;
begin
  // FreeAndNil(FKraftShape); // do not free here, TKraftShape is owned by TKraftRigidBody
  inherited;
end;

{ TCastleCollider ------------------------------------------------------------ }

procedure TCastleCollider.UpdateLocalTransform;
var
  T: TTransformation;
begin
  { initialize FLocalTransform, FLocalInverseTransform and pass transform to FKraftShape }
  T.Init;
  T.Multiply(Rotation, Scale, Translation);
  FLocalTransform := T.Transform;
  FLocalInverseTransform := T.InverseTransform;
  if FKraftShape <> nil then
    FKraftShape.LocalTransform := MatrixToKraft(FLocalTransform);

  { After setting the appropriate transformation, we can optimize the matrix
    multiplication when rotation = 0, translation = 0,0,0, scale = 1,1,1 }
  FUseLocalTransform := not (
    FTranslation.IsPerfectlyZero and
    (FRotation.W = 0) and
    TVector3.PerfectlyEquals(FScale, NoScale)
  );
end;

procedure TCastleCollider.InitializeKraft(const APhysics: TKraft;
  const ACastleRigidBody: TCastleRigidBody);
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
  // assign common TCollider properties to Kraft shape
  FKraftShape.Density := Density;
  FKraftShape.Restitution := Restitution;
  FKraftShape.Friction := Friction;
  if not Parent.RigidBody.Exists then //
    FKraftShape.Flags := FKraftShape.Flags - [ksfCollision];

  UpdateLocalTransform;
end;

procedure TCastleCollider.SetDensity(const AValue: Single);
begin
  FDensity := AValue;
  if FKraftShape <> nil then
    FKraftShape.Density := FDensity;
end;

procedure TCastleCollider.SetMass(const AValue: Single);
var
  RigidBody: TCastleRigidBody;
begin
  FMass := AValue;
  if Parent <> nil then
  begin
    RigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if (RigidBody <> nil) and (RigidBody.FKraftBody <> nil) then
    begin
      RigidBody.FKraftBody.ForcedMass := FMass;
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
  UpdateLocalTransform;
end;

procedure TCastleCollider.SetRotation(const AValue: TVector4);
begin
  if AValue.PerfectlyEquals(AValue, FRotation) then
    Exit;

  FRotation := AValue;
  UpdateLocalTransform;
end;

procedure TCastleCollider.SetScale(const AValue: TVector3);
begin
  if AValue.PerfectlyEquals(AValue, FScale) then
    Exit;

  FScale := AValue;
  UpdateLocalTransform;
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

procedure TCastleCollider.ParentAfterAttach;
var
  CastleRigidBody: TCastleRigidBody;
begin
  inherited;
  { Check there is a rigid body and we should try initialize collider.
    This is the case when rigid body was added first to behaviors list }
  if Parent <> nil then
  begin
    CastleRigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if CastleRigidBody <> nil then
      CastleRigidBody.CheckInitialization(Self);
  end;
end;

procedure TCastleCollider.ParentBeforeDetach;
var
  CastleRBody: TCastleRigidBody;
begin
  inherited;

  CastleRBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if CastleRBody <> nil then
  begin
    // deinitialize collider
    CastleRBody.DeinitializeColliders;
  end;
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
  FTranslation := Vector3(0, 0, 0);
  FRotation := Vector4(0, 1, 0, 0);

  { Will be updated in TCastleCollider.InitializeKraft to reflect FKraftShape.LocalTransform }
  FUseLocalTransform := false;
  FLocalTransform := TMatrix4.Identity;
  FLocalInverseTransform := TMatrix4.Identity;
end;

function TCastleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Density') or
     (PropertyName = 'Friction') or
     (PropertyName = 'Mass') or
     (PropertyName = 'Restitution') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TPlaneCollider ------------------------------------------------------------- }

function TPlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(Normal)), Distance));
end;

{ TCastlePlaneCollider ------------------------------------------------------- }

procedure TCastlePlaneCollider.SetNormal(const AValue: TVector3);
begin
  if not FNormal.Equals(FNormal, AValue) then
  begin
    FNormal := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastlePlaneCollider.SetDistance(const AValue: Single);
begin
  if not SameValue(FDistance, AValue) then
  begin
    FDistance := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastlePlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(FNormal)), Distance));
end;

constructor TCastlePlaneCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  if (PropertyName = 'NormalPersistent') or
     (PropertyName = 'Distance') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TBoxCollider --------------------------------------------------------------- }

function TBoxCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  { Check it ourselves, otherwise Kraft will crash on such invalid box with segfault.
    First Kraft raises nice
      EKraftDegeneratedConvexHull.Create('Degenerated convex hull');
    but then makes SIGSEGV at
      fPhysics.fStaticAABBTree.DestroyProxy(fStaticAABBTreeProxy)
    in TKraftShape.Destroy. }

  if (Size.X <= 0) or
     (Size.Y <= 0) or
     (Size.Z <= 0) then
    raise EPhysicsError.Create('You must assign positive Size to TBoxCollider');
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(Size / 2));
end;

{ TCastleBoxCollider --------------------------------------------------------- }

procedure TCastleBoxCollider.SetSize(const AValue: TVector3);
begin
  if not TVector3.Equals(FSize, AValue) then
  begin
    FSize := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleBoxCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  { Check it ourselves, otherwise Kraft will crash on such invalid box with segfault.
    First Kraft raises nice
      EKraftDegeneratedConvexHull.Create('Degenerated convex hull');
    but then makes SIGSEGV at
      fPhysics.fStaticAABBTree.DestroyProxy(fStaticAABBTreeProxy)
    in TKraftShape.Destroy. }

  if (FSize[0] <= 0) or
     (FSize[1] <= 0) or
     (FSize[2] <= 0) then
    raise EPhysicsError.Create('You must assign positive Size to TBoxCollider');
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(FSize / 2));
end;

constructor TCastleBoxCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

function TCastleBoxCollider.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if PropertyName = 'SizePersistent' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TSphereCollider ------------------------------------------------------------ }

function TSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, Radius);
end;

{ TCastleSphereCollider ------------------------------------------------------ }

procedure TCastleSphereCollider.SetRadius(const AValue: Single);
begin
  if not SameValue(FRadius, AValue) then
  begin
    FRadius := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, FRadius);
end;

function TCastleSphereCollider.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if PropertyName = 'Radius' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TRigidBody ----------------------------------------------------------------- }

constructor TCastleRigidBody.Create(AOwner: TComponent);
begin
  inherited;
  { This behavior needs WorldAttached() WorldDetached() }
  NeedWorldChangeNotification := true;

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
  FState := crbsNotInitialized;
end;

destructor TCastleRigidBody.Destroy;
begin
  { The FKraftBody will be freed now if you free TRigidBody
    instance explicitly. In most other cases, DeinitializeTransform
    already freed FKraftBody and set it nil. }

  FreeAndNil(FKraftBody);
  FreeAndNil(FCollider);
  FreeAndNil(FCollisionList);
  FreeAndNil(FPrevCollisions);
  FreeAndNil(FCurrentCollisions);
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

procedure TCastleRigidBody.InitializeTransform(const Transform: TCastleTransform);

  procedure RecreateKraftInstance;
  begin
    InitializeEngineRigidBody(Transform);

    if TryInitializeColliders then
      FinishInitialization(Transform);
  end;

begin
  Assert(FKraftBody = nil, 'Kraft body is initialized!');

  if Transform.Parent = nil then
    raise EMultipleReferencesInWorld.Create('Cannot use physics with TCastleTransform instances inserted multiple times into the Viewport.Items. Use TCastleTransformReference to refer to the TCastleTransform multiple times, or clone the instance instead of referencing it multiple times.');

  FTransform := Transform;
  RecreateKraftInstance;
end;

procedure TCastleRigidBody.DeinitializeTransform(const Transform: TCastleTransform);

  procedure RemoveSelfFromOtherRigidBodiesCollisionLists;
  var
    OtherRigidBody: TRigidBody;
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
        OtherRigidBody := TRigidBody(ContactPairEdge^.OtherRigidBody.UserData);

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
  Assert(FKraftBody <> nil, 'Second deinitialization!');
  Assert(Transform.World <> nil, 'Transform.World should be assigned at the time of TRigidBody.DeinitializeTransform call');

  Assert(not ((Transform.World.FKraftEngine = nil) and (FKraftBody <> nil)), 'KraftBody should not live longer than KraftEngine!');

  DeinitializeColliders;

  { Deinitialize rigid body when it's in collision with other rigid body make
    this body will be dangling pointer on it's FPrevCollisions list.
    So other rigid body can crash when it try to send FOnCollisionExit event. }
  RemoveSelfFromOtherRigidBodiesCollisionLists;

  FreeAndNil(FKraftBody);

  FTransform := nil;
  FState := crbsNotInitialized;
end;

function TCastleRigidBody.GetCastleColliderFromParent: TCastleCollider;
begin
  if Parent <> nil then
    Result := Parent.FindBehavior(TCastleCollider) as TCastleCollider
  else
    Result := nil;
end;

procedure TCastleRigidBody.InitializeEngineRigidBody(
  const Transform: TCastleTransform);
begin
  World.InitializePhysicsEngine;

  FKraftBody := TKraftRigidBody.Create(World.FKraftEngine);
  FKraftBody.UserData := Self;

  if Dynamic then
    FKraftBody.SetRigidBodyType(krbtDynamic)
  else
  if Animated then
    FKraftBody.SetRigidBodyType(krbtKinematic)
  else
    FKraftBody.SetRigidBodyType(krbtStatic);

  FKraftBody.GravityScale := Iff(Gravity, 1.0, 0.0);
  if 0 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisX];
  if 1 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisY];
  if 2 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisZ];
  if 0 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisX];
  if 1 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisY];
  if 2 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisZ];
  if not FExists then
    FKraftBody.Flags := FKraftBody.Flags - [krbfActive];

  if FTrigger then
    FKraftBody.Flags := FKraftBody.Flags + [krbfSensor];

  FState := crbsNoCollider;
end;

function TCastleRigidBody.TryInitializeColliders: Boolean;
var
  CastleColliderFromTransform: TCastleCollider;
begin
  if FState = crbsNotInitialized then
    raise Exception.Create(
      'TryInitializeColliders: Trying to add collider but rigid body not initilized');

  CastleColliderFromTransform := GetCastleColliderFromParent;
  if (Collider <> nil) and (CastleColliderFromTransform <> nil) then
    raise Exception.Create(
      'You can''t use TCollider and TCastleCollider in the same rigid body. ' +
      NL + 'Remove deprecated RigidBody.Collider.');

  if Collider <> nil then
  begin
    Collider.InitializeKraft(Parent.World.FKraftEngine, FKraftBody);
    FState := crbsNoFinalization;
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
    FState := crbsNoCollider;
    Exit(false);
  end;

  CastleColliderFromTransform.InitializeKraft(Parent.World.FKraftEngine, Self);

  FCastleCollider := CastleColliderFromTransform;
  FState := crbsNoFinalization;
  Result := true;
end;

procedure TCastleRigidBody.FinishInitialization(const Transform: TCastleTransform);
var
  V: TVector3;
begin
  if FState <> crbsNoFinalization then
    raise Exception.Create('Trying finalize rigid body initialization');

  if Collider <> nil then
    FKraftBody.ForcedMass := Collider.Mass
  else
    FKraftBody.ForcedMass := FCastleCollider.Mass;

  FKraftBody.Finish;

  UpdateCollides(Transform);

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
  FKraftBody.MaximalLinearVelocity := FMaximalLinearVelocity / Transform.World.FKraftEngine.WorldFrequency;

  FKraftBody.AngularVelocityDamp := FAngularVelocityDamp;
  FKraftBody.MaximalAngularVelocity := FMaximalAngularVelocity / Transform.World.FKraftEngine.WorldFrequency;

  // set initial transformation
  FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.WorldTransform));

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

  CheckPhysicsPostStepNeeded;
  FState := crbsInitialized;
end;

procedure TCastleRigidBody.CheckInitialization(const ACastleCollider: TCastleCollider);
begin
  if Collider <> nil then
    raise Exception.Create(
      'You can''t use TCollider and TCastleCollider in the same rigid body. ' +
      NL + 'Remove deprecated RigidBody.Collider.');

  { FCastleCollider has pointer to castle collider behavior that was used to
    initialize rigid body so we didn't need do anything. }
  if FCastleCollider = ACastleCollider then
    Exit;

  if (FState = crbsNoCollider) then
  begin

    if FCastleCollider = nil then
    begin
      if TryInitializeColliders then
        FinishInitialization(Parent);
    end;

    if FCastleCollider <> ACastleCollider then
    begin
      { Trying to initialize rigid body with another collider without deinitialization
        of the currrent one - this is a bug in our physics if will occurr }
      raise Exception.Create('Reinitialize with new collider but old one not deinitialized.');
    end;
  end;
end;

procedure TCastleRigidBody.DeinitializeColliders;
begin
  { Collider.FKraftShape is owned by FKraftBody, it was automatically freed already }
  if Collider <> nil then
  begin
    Collider.FKraftShape := nil;
    FState := crbsNoCollider;
  end else
  if FCastleCollider <> nil then
  begin
    if FKraftBody.ShapeCount > 0 then
    begin
      Assert(FKraftBody.ShapeCount = 1, 'More than one shape in rigid body.');
      Assert(FKraftBody.ShapeFirst <> FCastleCollider.FKraftShape, 'Mixed kraft shapes');
      FreeAndNil(FCastleCollider.FKraftShape);
    end;
    Assert(FCastleCollider.FKraftShape = nil, 'Mixed kraft shapes');
    FCastleCollider := nil;
    FState := crbsNoCollider;
  end;
end;

procedure TCastleRigidBody.ReinitializeCastleCollider;
begin
  { Don't try reinitialization when TCastleRigidBody is crbsNotInitialized }
  if FState = crbsNotInitialized then
    Exit;

  if TryInitializeColliders then
    FinishInitialization(FTransform);
end;

class procedure TCastleRigidBody.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleRigidBody).Setup2D;
end;

procedure TCastleRigidBody.PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
var
  ContactPairEdge: PKraftContactPairEdge;
  RBody: TRigidBody;
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
      RBody := TRigidBody(ContactPairEdge^.OtherRigidBody.UserData);

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
      CollisionDetails.Transforms[1] := TRigidBody(FPrevCollisions[I]).FTransform;
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
  if Transform.ExistsInRoot and Transform.Collides then
  begin
    FKraftBody.CollideWithCollisionGroups := AllCollisionGroups;
    FKraftBody.CollisionGroups := [0]; // TODO: make this configurable
  end else
  begin
    FKraftBody.CollideWithCollisionGroups := [];
    FKraftBody.CollisionGroups := [];
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
    Shape := GetKraftShape;

    if Shape = nil then
      Exit;

    LocalTransform := Parent.CheckParent.WorldInverseTransform *
      MatrixFromKraft(Shape.InterpolatedWorldTransform);
    if (FCollider = nil) and (FCastleCollider <> nil) and
       (FCastleCollider.FUseLocalTransform) then
      LocalTransform := LocalTransform * FCastleCollider.FLocalInverseTransform;

    MatrixDecompose(LocalTransform, Translation, Rotation, Scale);
    Parent.Rotation := Rotation;
    Parent.Translation := Translation;

    { This is an alternative version using Kraft to do matrix decomposition. }
    // Q := QuaternionFromMatrix4x4(MatrixToKraft(LocalTransform));
    // QuaternionToAxisAngle(Q, Axis, Angle);
    // Transform.Rotation := Vector4(Axis.X, Axis.Y, Axis.Z, Angle);

    // Transform.Translation := TranslationFromMatrix(LocalTransform);
  end;

begin
  inherited;

  { Disable physics in design mode (in the future we may add optional way to enable them) }
  if CastleDesignMode then
    Exit;

  if Parent = nil then
    Exit;

  if (Parent.World <> nil) and (not Parent.World.EnablePhysics) then
    Exit;

  UpdateCollides(Parent);
  if Dynamic then
  begin
    TransformationFromKraft;
    SynchronizeFromKraft; // I think its only required for dynamic bodies.
  end else
  if Animated then
  begin
    // TODO: check "if TransformChanged then" or such, don't do this every frame
    FKraftBody.SetWorldTransformation(MatrixToKraft(Parent.WorldTransform))

    // TODO: do above also for rbStatic, once "if TransformChanged then" implemented
    // or will this require recreating rigid body?
  end;
end;

procedure TCastleRigidBody.WorldAfterAttach;
begin
  inherited;

  if World <> nil then
    InitializeTransform(Parent);
end;

procedure TCastleRigidBody.WorldBeforeDetach;
begin
  inherited;

  if Parent <> nil then
  begin
    Assert(Parent = FTransform, 'FTransform should be Parent');
    DeinitializeTransform(Parent);
  end;
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

    CastleTransform := (TRigidBody(ContactPairEdge^.OtherRigidBody.UserData)).FTransform;
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
      Result := TRigidBody(RayShape.RigidBody.UserData).FTransform;
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
  if (PropertyName = 'Dynamic') or
     (PropertyName = 'Animated') or
     (PropertyName = 'Trigger') or
     (PropertyName = 'Gravity') or
     (PropertyName = 'LockTranslation') or
     (PropertyName = 'Exists') or
     (PropertyName = 'LockRotation') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

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
var
  KraftShape: TKraftShape;
begin
  if FExists = Value then
    Exit;

  FExists := Value;

  if Assigned(FKraftBody) then
  begin
    if FExists then
      FKraftBody.Flags := FKraftBody.Flags + [krbfActive]
    else
      FKraftBody.Flags := FKraftBody.Flags - [krbfActive];
  end;

  KraftShape := GetKraftShape;

  if Assigned(KraftShape) then
  begin
    { Note: ksfRayCastable flag determines whether body is detected by PhysicsRayCast. }
    if FExists then
      KraftShape.Flags := KraftShape.Flags + [ksfCollision,
        ksfRayCastable]
    else
      KraftShape.Flags := KraftShape.Flags - [ksfCollision,
        ksfRayCastable];
  end;
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
        CastleRigidBody.DeinitializeTransform(CastleRigidBody.FTransform);
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

  if EnablePhysics and (FKraftEngine <> nil) then
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

          WritelnLog('Max physics ticks in TCastleAbstractRootTransform.Update() exceeded ('
            + IntToStr(PhysicsTicksCount) + '). TimeAccumulator reduced from '
            + FloatToStrDot(OldTimeAccumulator) + ' to ' + FloatToStrDot(TimeAccumulator));

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
    to update transformation of all items in their TRigidBody.Update
    called from TCastleTransform.Update }
  inherited;
end;

{$endif read_implementation}
