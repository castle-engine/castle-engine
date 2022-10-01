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
    FContinuousCD: Boolean;

    procedure SetAngularVelocityRK4Integration(const AValue: Boolean);
    procedure SetLinearVelocityRK4Integration(const AValue: Boolean);
    procedure SetFrequency(const AValue: Single);
    procedure SetUpdateCollidersScaleAtRuntime(const AValue: Boolean);
    procedure SetContinuousCD(const AValue: Boolean);

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
      DefaultContinuousCD = true;
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
    property ContinuousCD: Boolean read FContinuousCD write SetContinuousCD default DefaultContinuousCD;
  end;

  { Don't have any Kraft objects only describe shape }
  TCastleCollider = class(TCastleBehavior)
  strict private
    FDensity, FMass, FRestitution, FFriction: Single;
    { When @false, behave as if Translation/Rotation/Scale/FLocalTransform/FLocalInverseTransform
      were all "identity", i.e. "no transformation".
      This is an often case, so we want to optimize it. }
    FTranslation: TVector3;
    FRotation: TVector4;
    FAutoSize: Boolean;
    FAutoSizing: Boolean; // are we calculating auto size?
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
  strict protected
    FLastUsedWorldScale: TVector3; // Last world scale used to change collider size
    FScale: TVector3; // Collider scale

    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;

    { Notify CastleRigidBody that kraft shape need be recreated }
    procedure ShapeChangedNotifyRigidBody;
    procedure CalculateAutoSize; virtual;

    function StoredWithAutoSize: Boolean;
  protected
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

    { Removes AutoSize = true when someone changes any collider size }
    procedure UpdateAutoSizeProperty;
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
    { Recalculates current size using parent Bounding Box }
    procedure DoAutoSize;

    property Translation: TVector3 read FTranslation write SetTranslation;
    property Rotation: TVector4 read FRotation write SetRotation;
    property Scale: TVector3 read FScale write SetScale;
  published
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

    property Restitution: Single read FRestitution write SetRestitution
      {$ifdef FPC}default DefaultRestitution{$endif};
    property Friction: Single read FFriction write SetFriction
      {$ifdef FPC}default DefaultFriction{$endif};
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
    {$undef read_interface_class}
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
  TCastlePlaneCollider = class(TCastleCollider)
  strict private
    FNormal: TVector3;
    FDistance: Single;

    procedure SetNormal(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;

    procedure CalculateAutoSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Normal: TVector3 read FNormal write SetNormal stored StoredWithAutoSize;
  published
    property Distance: Single read FDistance write SetDistance stored StoredWithAutoSize;
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
  TCastleBoxCollider = class(TCastleCollider)
  strict private
    FMode2D: Boolean;
    FSize: TVector3; // calculated size without scale

    procedure SetSize(const AValue: TVector3);
    procedure SetMode2D(const AValue: Boolean);

  private
    class procedure CreateComponent2D(Sender: TObject);

  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateAutoSize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Size: TVector3 read FSize write SetSize;
  published
    { Determines how to treat collider, currently change the way auto size
      and scale is calculated. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;

    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tcastleboxcollider_persistent_vectors.inc}
    {$undef read_interface_class}
  end;


  { Collide as a sphere.
    Place this inside @link(TRigidBody.Collider) property. }
  TSphereCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
  end deprecated 'use TCastleSphereCollider';

  { Collide as a sphere.
    Add this to transform behavior list.}
  TCastleSphereCollider = class(TCastleCollider)
  strict private
    FRadius: Single;
    FMode2D: Boolean;

    procedure SetRadius(const AValue: Single);
    procedure SetMode2D(const AValue: Boolean);

  private
    class procedure CreateComponent2D(Sender: TObject);

  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;

  protected
    procedure CalculateAutoSize; override;

  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;

    { Determines how to treat collider, currently change the way auto size
      and scale is calculated. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;
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

  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  protected
    procedure CalculateAutoSize; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;
    property Height: Single read FHeight write SetHeight stored StoredWithAutoSize;

    { Determines how to treat collider, currently change the way auto size
      and scale is calculated. }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;
  end;

  { Collide as a mesh, constructed from the given scene shapes.
    Add this behavior to your Scene }
  TCastleMeshCollider = class(TCastleCollider)
  strict private
    Mesh: TKraftMesh;
    FScene: TCastleTransform;
    procedure AddTriangle(Shape: TObject;
      const Position: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure SetScene(const AValue: TCastleTransform);
    { In case of mesh collider only remove Translation, Rotation and Scale }
    procedure CalculateAutoSize; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Mesh which is used to collide.
      It does not have to be convex.

      The @link(TCastleTransform.Collides Scene.Collides) and
      @link(TCastleTransform.GetCollides Scene.GetCollides) do not matter here.
      Using the TCollisionNode inside Scene matters (so the scene may collide
      as something different than it's visible, see
      https://castle-engine.io/x3d_implementation_navigation.php ). }
    property Scene: TCastleTransform read FScene write SetScene;
  end;


  TCollisionEvent = procedure (const CollisionDetails: TPhysicsCollisionDetails) of object;
  TOnCollision = TCollisionEvent deprecated 'use TCollisionEvent';

  TRigidBodyList = {$ifdef FPC}specialize{$endif} TList<TRigidBody>;
  TCastleRigidBodyList = {$ifdef FPC}specialize{$endif} TList<TCastleRigidBody>;

  TCollisionDetectionType = (
    cdtDiscrete,
    cdtContinuous
  );

  { Rigid body behavior for the physics engine }
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
    FCollisionDetectionType: TCollisionDetectionType;
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
    procedure UpdateCollisionDetectionType;
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

    procedure SetCollisionDetectionType(const AValue: TCollisionDetectionType);

    procedure SetDynamic(const AValue: Boolean);
    procedure SetAnimated(const AValue: Boolean);
    procedure SetGravity(const AValue: Boolean);

    procedure SetExists(const Value: Boolean);
    procedure SetTrigger(const Value: Boolean);

    { Creates physics engine object and connects the rigid body with
      Transform and Transform.World.

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

    property DuringSettingTransformationFromPhysicsEngine: Boolean read
      FDuringSettingTransformationFromPhysicsEngine;

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

    procedure WakeUp;

    procedure Sleep;

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
    property CollisionDetectionType: TCollisionDetectionType read FCollisionDetectionType
      write SetCollisionDetectionType default cdtDiscrete;

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

  TRigidBody = class(TCastleRigidBody)
  end;

  TAbstractJoint = class(TCastleBehavior)
  strict protected
    { When joint is added to world we need found parent rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we done that }
    FParentRigidBody: TCastleRigidBody;
    FBreakThresholdForce: Single;
    FBreakThresholdTorque: Single;
    FBreakable: Boolean;

    { Create kraft joint and set it specialized properties }
    procedure CreateKraftJoint; virtual; abstract;
    { Function to get kraft joint }
    function GetKraftJoint: TKraftConstraintJoint; virtual; abstract;
    { Tries to deinitialize and initialize kraft object }
    procedure RecreateKraftJoint;
    procedure InitializeKraftJoint;
    procedure DeinitializeKraftJoint; virtual; abstract;
    { Means that we can create joint object }
    function AllNeededKraftObjectsInitialized: Boolean; virtual;

    { Event callback when TCastleRigidBody is added to parent transform }
    procedure OnParentTransformAddBehavior(const Component: TComponent); virtual;
    { Event callback when TCastleRigidBody is removed from parent transform }
    procedure OnBeforeParentTransfromRemoveBehavior(const Component: TComponent); virtual;
    { Event callback when TCastleRigidBody behavior is initialized }
    procedure OnParentRigidBodyInitialized(const Component: TComponent); virtual;
    { Event callback when TCastleRigidBody behavior is deinitialized }
    procedure OnBeforeParentRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets TCastleRigidBody behavior initialize/deinitialize callbacks }
    procedure SetParentRigidBodyEvents;
    { Removes TCastleRigidBody behavior initialize/deinitialize callbacks }
    procedure RemoveParentRigidBodyEvents;

    procedure UpdateBreakThresholdForceInKraft;
    procedure UpdateBreakThresholdTorqueInKraft;
    procedure UpdateBreakableInKraft;

    procedure SetBreakThresholdForce(const AValue: Single);
    procedure SetBreakThresholdTorque(const AValue: Single);
    procedure SetBreakable(const AValue: Boolean);
  public
    destructor Destroy; override;
    procedure WorldAfterAttach; override;
    procedure WorldBeforeDetach; override;

    { Removes anchors and adds transforms to synchronize to TransformsToSynchronize list. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); virtual;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Breakable: Boolean read FBreakable write SetBreakable;
    property BreakThresholdForce:Single read FBreakThresholdForce write SetBreakThresholdForce;
    property BreakThresholdTorque:Single read FBreakThresholdTorque write SetBreakThresholdTorque;
  end;

  TAbstractOneBodyJoint = class(TAbstractJoint)

  end;

  { Abstract class for joint that connects two TCastleTransforms }
  TAbstractTwoBodiesJoint = class (TAbstractJoint)
  private
    { TAbstractTwoBodiesJoint means that joint connects two transforms so this is
      the second one transform pointer }
    FConnectedTransform: TCastleTransform;
    { When connected transform is assigned we need found second rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we done that }
    FConnectedRigidBody: TCastleRigidBody;
    { Stores valu should connected rigid bodies collide or not }
    FConnectedCollide: Boolean;
    { We need observe second connected transform and remove it form
      FConnectedTransform when second transform is freed }
    FConnectedTransfromFreeObserver: TFreeNotificationObserver;

  strict protected
    procedure SetConnectedTransform(const AConnectedTransform: TCastleTransform); virtual;
    procedure SetConnectedCollide(const AValue: Boolean); virtual;

    { Means that we can create joint object }
    function AllNeededKraftObjectsInitialized: Boolean; override;

    procedure OnConnectedTransformFree(const Sender: TFreeNotificationObserver); virtual;
    { Event callback when TCastleRigidBody is added to connected transform }
    procedure OnConnectedTransformAddBehavior(const Component: TComponent); virtual;
    { Event callback when TCastleRigidBody is removed from connected transform }
    procedure OnConnectedTransfromRemoveBehavior(const Component: TComponent); virtual;
    { Event callback when connected transform TCastleRigidBody behavior is initialized }
    procedure OnConnectedRigidBodyInitialized(const Component: TComponent); virtual;
    { Event callback when connected transform TCastleRigidBody behavior is deinitialized }
    procedure OnBeforeConnectedRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets connected transform add/remmove behavior callbacks and free
      notification observer }
    procedure SetConnectedTransformEvents;
    { Removes connected transform add/remmove behavior callbacks and free
      notification observer }
    procedure RemoveConnectedTransformEvents;
    { Sets connected TCastleRigidBody behavior initialize/deinitialize callbacks }
    procedure SetConnectedRigidBodyEvents;
    { Removes connected TCastleRigidBody behavior initialize/deinitialize
      callbacks }
    procedure RemoveConnectedRigidBodyEvents;

  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { We need to check if sometimes the connected castle transform is not
      the parent node and we need change the connected node to nil then }
    procedure ParentAfterAttach; override;
    { We don't need check WorldAfterAttach and WorldBeforeDetach because all
      things are done in TAbstractJoint }
  published
    { Second transform of physics joint - can't be the Parent of joint }
    property ConnectedTransform: TCastleTransform read FConnectedTransform
      write SetConnectedTransform;
    { Should connected transforms (ConnectedTransform and Parent)
      collide - default no }
    property ConnectedCollide: Boolean read FConnectedCollide
      write SetConnectedCollide default false;
  end;

  EConnectedTransformTheSameAsParent = class (Exception);

  TTemporaryTransform = class(TCastleTransform)
    strict private
      FColor: TCastleColor;
      FJoint: TAbstractJoint;

      procedure SetColor(const Value: TCastleColor);
      procedure ApplyWireframeEffectToParent;
      procedure RemoveWireframeEffectFromParent;
    private
      class var RenderOptionsForParentScene: TCastleRenderOptions;
    protected
      FSphere: TCastleTransform;

      function EstimateSphereRadius: Single;

      procedure SetValue(const AValue: TVector3);
      procedure SetObservedValue(const AValue: TVector3); virtual; abstract;
      function GetObservedValue: TVector3; virtual; abstract;

      procedure CheckTransformInsideParent;
      procedure ChangedTransform; override;
    public
      constructor Create(AOwner: TComponent;
        const AJoint: TAbstractJoint); reintroduce; virtual;
      destructor Destroy; override;

      procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

      property Value: TVector3 read GetObservedValue write SetValue;
      property Joint: TAbstractJoint read FJoint;
      property Color: TCastleColor read FColor write SetColor;
  end;

  TTemporaryAnchor = class(TTemporaryTransform)
    protected
      procedure SetObservedValue(const AValue: TVector3); override;
      function GetObservedValue: TVector3; override;
  end;

  TTemporaryConnectedAnchor = class(TTemporaryTransform)
    protected
      procedure SetObservedValue(const AValue: TVector3); override;
      function GetObservedValue: TVector3; override;

    public
      constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryWorldPoint = class(TTemporaryTransform)
    protected
      procedure SetObservedValue(const AValue: TVector3); override;
      function GetObservedValue: TVector3; override;

    public
      constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;


  TTemporaryWorldAnchor = class(TTemporaryTransform)
    protected
      procedure SetObservedValue(const AValue: TVector3); override;
      function GetObservedValue: TVector3; override;

    public
      constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryConnectedWorldAnchor = class(TTemporaryTransform)
    protected
      procedure SetObservedValue(const AValue: TVector3); override;
      function GetObservedValue: TVector3; override;

    public
      constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;


  TTemporaryWorldGroundAnchor = class(TTemporaryTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;

  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;

  TTemporaryConnectedWorldGroundAnchor = class(TTemporaryTransform)
  protected
    procedure SetObservedValue(const AValue: TVector3); override;
    function GetObservedValue: TVector3; override;

  public
    constructor Create(AOwner: TComponent; const AJoint: TAbstractJoint); override;
  end;


  TJointHinge = class(TAbstractTwoBodiesJoint)
    private
      FKraftJointHinge: TKraftConstraintJointHinge;
      FMotorSpeed: Single;
      FRotationAxis: TVector3;
      FAnchor: TVector3;
      FAnchorTransform: TTemporaryAnchor;
      FMinAngleLimit: Single;
      FMaxAngleLimit: Single;
      FEnableLimits: Boolean;
      FEnableMotor: Boolean;
      FMaxMotorTorque: Single;

      procedure SetAnchor(const AValue: TVector3);
      procedure SetRotationAxis(const AValue: TVector3);
      procedure SetEnableLimits(const AValue: Boolean);
      procedure SetMinAngleLimit(const AValue: Single);
      procedure SetMaxAngleLimit(const AValue: Single);
      procedure SetEnableMotor(const AValue: Boolean);
      procedure SetMotorSpeed(const AValue: Single);
      function GetMotorSpeed: Single;
      procedure SetMaxMotorTorque(const AValue: Single);

    strict protected
      function GetKraftJoint: TKraftConstraintJoint; override;
      procedure CreateKraftJoint; override;
      procedure DeinitializeKraftJoint; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure WorldBeforeDetach; override;

      { Called by editor when behavior is selected, adds anchor handle transform. }
      procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
      { Called by editor to remove anchor handle transform. }
      procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

      function PropertySections(const PropertyName: String): TPropertySections; override;

      // TODO: This is local point should be world?
      property Anchor: TVector3 read FAnchor write SetAnchor;
      // TODO: currently this is local and in kraft is world maybe we should
      // change to world or to castle default?
      property RotationAxis: TVector3 read FRotationAxis write SetRotationAxis;
    published
      property EnableLimits: Boolean read FEnableLimits write SetEnableLimits;
      property MinAngleLimit: Single read FMinAngleLimit write SetMinAngleLimit;
      property MaxAngleLimit: Single read FMaxAngleLimit write SetMaxAngleLimit;
      property EnableMotor: Boolean read FEnableMotor write SetEnableMotor;
      property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
      property MaxMotorTorque: Single read FMaxMotorTorque
        write SetMaxMotorTorque;
      {$define read_interface_class}
      {$I auto_generated_persistent_vectors/tjointhinge_persistent_vectors.inc}
      {$undef read_interface_class}
  end;

  TJointRope = class(TAbstractTwoBodiesJoint)
    private
      FKraftJointRope: TKraftConstraintJointRope;
      FAnchor: TVector3;
      FConnectedAnchor: TVector3;
      FDistance: Single;

      FAnchorTransform: TTemporaryAnchor;
      FConnectedAnchorTransform: TTemporaryConnectedAnchor;

      procedure SetAnchor(const AValue: TVector3);
      procedure SetConnectedAnchor(const AValue: TVector3);
      procedure SetDistance(const AValue: Single);
    strict protected
      function GetKraftJoint: TKraftConstraintJoint; override;
      procedure CreateKraftJoint; override;
      procedure DeinitializeKraftJoint; override;

      procedure OnConnectedTransformFree(const Sender: TFreeNotificationObserver); override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure WorldBeforeDetach; override;

      { Called by editor when behavior is selected, adds anchor and connected anchor handle transform. }
      procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
      { Called by editor to remove anchor and connected anchor handle transform. }
      procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

      function PropertySections(const PropertyName: String): TPropertySections; override;

      // TODO: This is local point should be world?
      property Anchor: TVector3 read FAnchor write SetAnchor;
      property ConnectedAnchor: TVector3 read FConnectedAnchor
        write SetConnectedAnchor;

    published
      property Distance: Single read FDistance write SetDistance;
      {$define read_interface_class}
      {$I auto_generated_persistent_vectors/tjointrope_persistent_vectors.inc}
      {$undef read_interface_class}
  end;

  TJointFixed = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointFixed: TKraftConstraintJointFixed;
    FAnchor: TVector3;

    FAnchorTransform: TTemporaryAnchor;

    procedure SetAnchor(const AValue: TVector3);
  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds anchor handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove anchor handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
  published
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tjointfixed_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  TJointBall = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointBall: TKraftConstraintJointBallSocket;
    FAnchor: TVector3;

    FAnchorTransform: TTemporaryAnchor;

    procedure SetAnchor(const AValue: TVector3);
  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds anchor handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove anchor handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
  published
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tjointball_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  TJointDistance = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointDistance: TKraftConstraintJointDistance;
    FAnchor: TVector3;
    FConnectedAnchor: TVector3;
    FFrequencyHz: Single;
    FDampingRatio: Single;

    FAnchorTransform: TTemporaryAnchor;
    FConnectedAnchorTransform: TTemporaryConnectedAnchor;

    procedure SetAnchor(const AValue: TVector3);
    procedure SetConnectedAnchor(const AValue: TVector3);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds anchor and connected anchor handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove anchor and connected anchor handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
    // TODO: This is local point should be world?
    property ConnectedAnchor: TVector3 read FConnectedAnchor
      write SetConnectedAnchor;

  published
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz;
    property DampingRatio: Single read FDampingRatio write SetDampingRatio;

    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tjointdistance_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  TJointGrab = class (TAbstractOneBodyJoint)
  private
    FDampingRatio: Single;
    FFrequencyHz: Single;
    FKraftJointGrab: TKraftConstraintJointGrab;
    FMaximalForce: Single;
    FWorldPoint: TVector3;

    FWorldPointTransform: TTemporaryWorldPoint;

    procedure SetWorldPoint(const AValue: TVector3);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure SetMaximalForce(const AValue: Single);

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds world point handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove world point handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is world point should be local? Should be synchronized per update?
    property WorldPoint: TVector3 read FWorldPoint write SetWorldPoint;
  published
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz {$ifdef FPC}default 5{$endif};
    property DampingRatio: Single read FDampingRatio write SetDampingRatio {$ifdef FPC}default 0.7{$endif};
    property MaximalForce: Single read FMaximalForce write SetMaximalForce {$ifdef FPC}default MaxSingle{$endif};

    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tjointgrab_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  TJointPulley = class(TAbstractTwoBodiesJoint)
    private
      FKraftJointPulley: TKraftConstraintJointPulley;
      FWorldAnchor: TVector3;
      FWorldGroundAnchor: TVector3;
      FConnectedWorldAnchor: TVector3;
      FConnectedWorldGroundAnchor: TVector3;
      FRatio: Single;

      FWorldAnchorTransform: TTemporaryWorldAnchor;
      FConnectedWorldAnchorTransform: TTemporaryConnectedWorldAnchor;
      FWorldGroundAnchorTransform: TTemporaryWorldGroundAnchor;
      FConnectedWorldGroundAnchorTransform: TTemporaryConnectedWorldGroundAnchor;

      procedure SetWorldAnchor(const AValue: TVector3);
      procedure SetConnectedWorldAnchor(const AValue: TVector3);
      procedure SetWorldGroundAnchor(const AValue: TVector3);
      procedure SetConnectedWorldGroundAnchor(const AValue: TVector3);
      procedure SetRatio(const AValue: Single);
    strict protected
      function GetKraftJoint: TKraftConstraintJoint; override;
      procedure CreateKraftJoint; override;
      procedure DeinitializeKraftJoint; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure WorldBeforeDetach; override;

      { Called by editor when behavior is selected, adds anchor handle transform. }
      procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
      { Called by editor to remove anchor handle transform. }
      procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

      function PropertySections(const PropertyName: String): TPropertySections; override;

      property WorldAnchor: TVector3 read FWorldAnchor write SetWorldAnchor;
      property ConnectedWorldAnchor: TVector3 read FConnectedWorldAnchor
        write SetConnectedWorldAnchor;
      property WorldGroundAnchor: TVector3 read FWorldGroundAnchor
        write SetWorldGroundAnchor;
      property ConnectedWorldGroundAnchor: TVector3
        read FConnectedWorldGroundAnchor write SetConnectedWorldGroundAnchor;
    published
      property Ratio: Single read FRatio write SetRatio {$ifdef FPC}default 1.0{$endif};

      {$define read_interface_class}
      {$I auto_generated_persistent_vectors/tjointpulley_persistent_vectors.inc}
      {$undef read_interface_class}
  end;

  TJointWorldPlaneDistance = class (TAbstractOneBodyJoint)
  private
    FKraftJointWorldPlaneDistance: TKraftConstraintJointWorldPlaneDistance;
    FAnchor: TVector3;
    FPlaneNormal: TVector3;
    FPlaneDistance: Single;
    FWorldDistance: Single;
    FDoubleSidedWorldPlane: Boolean;
    FFrequencyHz: Single;
    FDampingRatio: Single;

    FAnchorTransform: TTemporaryAnchor;

    procedure SetAnchor(const AValue: TVector3);
    procedure SetPlaneNormal(const AValue: TVector3);
    procedure SetPlaneDistance(const AValue: Single);
    procedure SetWorldDistance(const AValue: Single);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure DoubleSidedWorldPlane(const AValue: Boolean);

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds anchor handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove anchor handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Anchor: TVector3 read FAnchor write SetAnchor;
    property PlaneNormal: TVector3 read FPlaneNormal write SetPlaneNormal;
  published
    property PlaneDistance: Single read FPlaneDistance write SetPlaneDistance;
    property WorldDistance: Single read FWorldDistance write SetWorldDistance;
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz;
    property DampingRatio: Single read FDampingRatio write SetDampingRatio;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tjointworldplanedistance_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TJointSlider = class (TAbstractTwoBodiesJoint)
  private
    FKraftJointSlider: TKraftConstraintJointSlider;

    FWorldSliderAxis: TVector3;

    FWorldAnchor: TVector3;
    FWorldAnchorTransform: TTemporaryWorldAnchor;

    FEnableLimits: Boolean;
    FMinTranslationLimit: Single;
    FMaxTranslationLimit: Single;

    FEnableMotor: Boolean;
    FMaxMotorForce: Single;
    FMotorSpeed: Single;

    procedure SetWorldAnchor(const AValue: TVector3);
    procedure SetWorldSliderAxis(const AValue: TVector3);
    procedure SetEnableLimits(const AValue: Boolean);
    procedure SetMinTranslationLimit(const AValue: Single);
    procedure SetMaxTranslationLimit(const AValue: Single);
    procedure SetEnableMotor(const AValue: Boolean);
    procedure SetMotorSpeed(const AValue: Single);
    function GetMotorSpeed: Single;
    procedure SetMaxMotorForce(const AValue: Single);

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;

    { Called by editor when behavior is selected, adds world anchor handle transform. }
    procedure InternalSelectionStart(TransformsToSynchronize: TCastleTransformList); override;
    { Called by editor to remove world anchor handle transform. }
    procedure RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    property WorldAnchor: TVector3 read FWorldAnchor write SetWorldAnchor;
    property WorldSliderAxis: TVector3 read FWorldSliderAxis write SetWorldSliderAxis;
  published
    property EnableLimits: Boolean read FEnableLimits write SetEnableLimits;
    property MinTranslationLimit: Single read FMinTranslationLimit write SetMinTranslationLimit;
    property MaxTranslationLimit: Single read FMaxTranslationLimit write SetMaxTranslationLimit;
    property EnableMotor: Boolean read FEnableMotor write SetEnableMotor;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorForce: Single read FMaxMotorForce write SetMaxMotorForce;
    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tjointslider_persistent_vectors.inc}
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
    UpdateAutoSizeProperty;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleCapsuleCollider.SetHeight(const AValue: Single);
begin
  if not SameValue(FHeight, AValue) then
  begin
    FHeight := AValue;
    UpdateAutoSizeProperty;
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

procedure TCastleCapsuleCollider.CalculateAutoSize;
var
  LocalBoundingBox: TBox3D;
  MinimalThickness: Single;
begin
  inherited;

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinimalThickness;
    Height := MinimalThickness;
    Translation := CastleVectors.Vector3(0, 0, 0);
    Rotation := Vector4(0, 0, 0, 0);
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
  Rotation := Vector4(0, 1, 0, 0);
end;

function TCastleCapsuleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AutoSize') or
     (PropertyName = 'Mode2D') or
     (PropertyName = 'Height') or
     (PropertyName = 'Radius') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCapsuleCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    if AutoSize then
      DoAutoSize;
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

  Mesh.AddTriangle(
    Mesh.AddVertex(VectorToKraft(Position.Data[0])),
    Mesh.AddVertex(VectorToKraft(Position.Data[1])),
    Mesh.AddVertex(VectorToKraft(Position.Data[2])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[0])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[1])),
    Mesh.AddNormal(VectorToKraft(Normal.Data[2]))
  );


  { Kraft RayCast does not check inverted triangles. }
  Mesh.AddTriangle(
      Mesh.AddVertex(VectorToKraft(Position.Data[2])),
      Mesh.AddVertex(VectorToKraft(Position.Data[1])),
      Mesh.AddVertex(VectorToKraft(Position.Data[0])),
      Mesh.AddNormal(VectorToKraft(-Normal.Data[2])),
      Mesh.AddNormal(VectorToKraft(-Normal.Data[1])),
      Mesh.AddNormal(VectorToKraft(-Normal.Data[0]))
    );
end;

function TCastleMeshCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  if FScene = nil then
  begin
    Result := nil;
    Exit;
  end;

  if ARigidBody.RigidBodyType <> krbtStatic then
  begin
    WritelnWarning('TCastleMeshCollider is available only for static rigid bodies.');
    Result := nil;
    Exit;
  end;

  Mesh := TKraftMesh.Create(APhysics);

  FScene.ColliderMesh({$ifdef FPC}@{$endif}AddTriangle);

  if Mesh.CountVertices = 0 then
    WritelnWarning('The scene assigned to TCastleMeshCollider.Scene is empty (no collidable triangles)');
  Mesh.Finish;

  Result := TKraftShapeMesh.Create(APhysics, ARigidBody, Mesh);
end;

procedure TCastleMeshCollider.SetScene(const AValue: TCastleTransform);
begin
  if FScene <> AValue then
  begin
    FScene := AValue;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleMeshCollider.CalculateAutoSize;
begin
  inherited;
  Rotation := Vector4(0, 0, 0, 0);
  Scale := CastleVectors.Vector3(1, 1, 1);
  Translation := CastleVectors.Vector3(0, 0, 0);
end;

function TCastleMeshCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AutoSize') or
     (PropertyName = 'Scene') then
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

procedure TPhysicsProperties.SetContinuousCD(
  const AValue: Boolean);
begin
  if FContinuousCD = AValue then
    Exit;

  FContinuousCD := AValue;
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
  FContinuousCD := DefaultContinuousCD;
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
  if not FParent.ExistsInRoot then
    FKraftShape.Flags := FKraftShape.Flags - [ksfCollision, ksfRayCastable];
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
  UpdateAutoSizeProperty;
  UpdateLocalTransform;
end;

procedure TCastleCollider.SetRotation(const AValue: TVector4);
begin
  if AValue.PerfectlyEquals(AValue, FRotation) then
    Exit;

  FRotation := AValue;
  UpdateAutoSizeProperty;
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
  if FAutoSize then
    DoAutoSize;
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

  if AutoSize then
    DoAutoSize;

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

procedure TCastleCollider.UpdateAutoSizeProperty;
begin
  if AutoSize = false then
    Exit;

  if FAutoSizing then
    Exit;

  if csLoading in ComponentState then
    Exit;

  AutoSize := false;
end;

procedure TCastleCollider.CalculateAutoSize;
begin

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
  FTranslation := CastleVectors.Vector3(0, 0, 0);
  FRotation := Vector4(0, 1, 0, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_implementation_constructor}

  { Will be updated in TCastleCollider.InitializeKraft to reflect FKraftShape.LocalTransform }
  FUseLocalTransform := false;
  FLocalTransform := TMatrix4.Identity;
  FLocalInverseTransform := TMatrix4.Identity;
  FAutoSize := true;

  NeedWorldChangeNotification := true;
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
  if (PropertyName = 'Density') or
     (PropertyName = 'Friction') or
     (PropertyName = 'Mass') or
     (PropertyName = 'Restitution') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCollider.DoAutoSize;
begin
  FAutoSizing := true;
  try
    CalculateAutoSize;
  finally
    FAutoSizing := false;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

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
    UpdateAutoSizeProperty;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastlePlaneCollider.SetDistance(const AValue: Single);
begin
  if not SameValue(FDistance, AValue) then
  begin
    FDistance := AValue;
    UpdateAutoSizeProperty;
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

procedure TCastlePlaneCollider.CalculateAutoSize;
var
  WorldBoundingBox: TBox3D;
begin
  inherited;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Normal := CastleVectors.Vector3(0, 1, 0);
    Translation := CastleVectors.Vector3(0, 0, 0);
    Rotation := Vector4(0, 0, 0, 0);
    Exit;
  end;

  Distance := 0;
  WorldBoundingBox := Parent.WorldBoundingBox;
  if WorldBoundingBox.SizeX > WorldBoundingBox.SizeY then
  begin
    if WorldBoundingBox.SizeY > WorldBoundingBox.SizeZ then
      Normal := CastleVectors.Vector3(0, 0, 1)
    else
      Normal := CastleVectors.Vector3(0, 1, 0);
  end
  else
    begin
      if WorldBoundingBox.SizeX > WorldBoundingBox.SizeZ then
        Normal := CastleVectors.Vector3(0, 0, 1)
      else
        Normal := CastleVectors.Vector3(1, 0, 0);
    end;

  Translation := Parent.LocalBoundingBox.Center;
  Rotation := Vector4(0, 0, 0, 0);
end;

constructor TCastlePlaneCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNormal := CastleVectors.Vector3(0, 1, 0);
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
  if (PropertyName = 'AutoSize') or
     (PropertyName = 'NormalPersistent') or
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
    UpdateAutoSizeProperty;
    ShapeChangedNotifyRigidBody;
  end;
end;

procedure TCastleBoxCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    if AutoSize then
      DoAutoSize;
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

procedure TCastleBoxCollider.CalculateAutoSize;
var
  LocalBoundingBox: TBox3D;
  TempSize: TVector3;
  MinimalThickness: Single;
begin
  inherited;

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Size := CastleVectors.Vector3(MinimalThickness, MinimalThickness, MinimalThickness);
    Translation :=  CastleVectors.Vector3(0, 0, 0);
    Rotation := Vector4(0, 0, 0, 0);
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
  Rotation := Vector4(0, 0, 0, 0);
end;

constructor TCastleBoxCollider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := CastleVectors.Vector3(1.0, 1.0, 1.0);
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
  if (PropertyName = 'AutoSize') or
     (PropertyName = 'Mode2D') or
     (PropertyName = 'SizePersistent') then
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
    UpdateAutoSizeProperty;
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

procedure TCastleSphereCollider.CalculateAutoSize;
var
  LocalBoundingBox: TBox3D;
  MinimalThickness: Single;
begin
  inherited;

  if FMode2D then
    MinimalThickness := AutoSizeMinimalThickness2D
  else
    MinimalThickness := AutoSizeMinimalThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinimalThickness;
    Translation := CastleVectors.Vector3(0, 0, 0);
    Rotation := Vector4(0, 0, 0, 0);
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
  Rotation := Vector4(0, 0, 0, 0);
end;

function TCastleSphereCollider.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AutoSize') or
     (PropertyName = 'Mode2D') or
     (PropertyName = 'Radius') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSphereCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    if AutoSize then
      DoAutoSize;
  end;
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
  FState := crbsNotInitialized;
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
  FState := crbsNotInitialized;
end;

function TCastleRigidBody.GetCastleColliderFromParent: TCastleCollider;
begin
  if Parent <> nil then
    Result := Parent.FindBehavior(TCastleCollider) as TCastleCollider
  else
    Result := nil;
end;

procedure TCastleRigidBody.UpdateKraftRigidBodyType;
begin
  if FState in [crbsNotInitialized] then
  begin
    if FKraftBody <> nil then
    begin
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
    { There is a bug in Kraft that makes you can't change body type when it
      was onece selected so we need recreate rigid body physics objects. }
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
  UpdateCollisionDetectionType;

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
      'TryInitializeColliders: Trying to add collider but rigid body not initialized.');

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

  if not CastleColliderFromTransform.InitializeKraft(Parent.World.FKraftEngine, Self) then
  begin
    FState := crbsNoCollider;
    Exit(false);
  end;

  FCastleCollider := CastleColliderFromTransform;
  FState := crbsNoFinalization;
  Result := true;
end;

procedure TCastleRigidBody.FinishInitialization;
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
  FState := crbsInitialized;
  NotifyOnInitialized;
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
  if FState = crbsInitialized then
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
    FState := crbsNoCollider;
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
    FState := crbsNoCollider;
  end;
end;

procedure TCastleRigidBody.ReinitializeCastleCollider;
begin
  { Don't try reinitialization when TCastleRigidBody is crbsNotInitialized }
  if FState = crbsNotInitialized then
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
  Result := (FState = crbsInitialized);
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

procedure TCastleRigidBody.UpdateCollisionDetectionType;
begin
  if Assigned(FKraftBody) then
  begin
    { We set here krbfContinuous, krbfContinuousAgainstDynamics because
      continuous works only when:
      - one of bodies has krbfContinuous and they are dynamic bodies
      - ContinuousAgainstDynamics is set to true in FKraftEngine
        and one of bodies has krbfContinuousAgainstDynamics flag }
    case FCollisionDetectionType of
      cdtDiscrete:
        FKraftBody.Flags := FKraftBody.Flags - [krbfContinuous, krbfContinuousAgainstDynamics];
      cdtContinuous:
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
    if FState = crbsInitialized then
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
    if FState = crbsInitialized then
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

procedure TCastleRigidBody.SetCollisionDetectionType(const AValue: TCollisionDetectionType);
begin
  if FCollisionDetectionType = AValue then
    Exit;

  FCollisionDetectionType := AValue;
  UpdateCollisionDetectionType;
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

  if not (FState in [crbsNoFinalization, crbsInitialized]) then
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
  if FState <> crbsNotInitialized then
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
  if (PropertyName = 'Dynamic') or
     (PropertyName = 'Animated') or
     (PropertyName = 'Trigger') or
     (PropertyName = 'Gravity') or
     (PropertyName = 'LockTranslation') or
     (PropertyName = 'Exists') or
     (PropertyName = 'AngularVelocityPersistent') or
     (PropertyName = 'AngularVelocityDamp') or
     (PropertyName = 'MaximalAngularVelocity') or
     (PropertyName = 'LinearVelocityPersistent') or
     (PropertyName = 'LinearVelocityDamp') or
     (PropertyName = 'MaximalLinearVelocity') or
     (PropertyName = 'CollisionDetectionType') or
     (PropertyName = 'LockRotation') then
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
    if FCastleCollider.AutoSize then
      FCastleCollider.DoAutoSize;
  end;
end;

procedure TCastleRigidBody.WakeUp;
begin
  FKraftBody.SetToAwake;
end;

procedure TCastleRigidBody.Sleep;
begin
  FKraftBody.SetToSleep;
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

{ TAbstractTwoBodiesJoint ---------------------------------------------------- }

procedure TAbstractTwoBodiesJoint.SetConnectedCollide(const AValue: Boolean);
begin
  if FConnectedCollide = AValue then
    Exit;
  FConnectedCollide := AValue;
  RecreateKraftJoint;
end;

function TAbstractTwoBodiesJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (inherited AllNeededKraftObjectsInitialized) and
    (FConnectedRigidBody <> nil) and (FConnectedRigidBody.IsInitialized);
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransformFree(
  const Sender: TFreeNotificationObserver);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  RemoveConnectedTransformEvents;
  FConnectedRigidBody := nil;
  FConnectedTransform := nil;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransformAddBehavior(
  const Component: TComponent);
begin
  FConnectedRigidBody := Component as TCastleRigidBody;
  SetConnectedRigidBodyEvents;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  FConnectedRigidBody := nil;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TAbstractTwoBodiesJoint.OnBeforeConnectedRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedTransformEvents;
begin
  if FConnectedTransform = nil then
    Exit;

  FConnectedTransform.AddNotificationOnBehaviorAdded(
    @OnConnectedTransformAddBehavior, TCastleRigidBody);

  FConnectedTransform.AddNotificationBeforeBehaviorRemove(
    @OnConnectedTransfromRemoveBehavior, TCastleRigidBody);

  if FConnectedTransfromFreeObserver = nil then
  begin
    FConnectedTransfromFreeObserver := TFreeNotificationObserver.Create(Self);
    FConnectedTransfromFreeObserver.OnFreeNotification := {$ifdef FPC}@{$endif}OnConnectedTransformFree;
  end;

  FConnectedTransfromFreeObserver.Observed := FConnectedTransform;
end;

procedure TAbstractTwoBodiesJoint.RemoveConnectedTransformEvents;
begin
  if FConnectedTransform <> nil then
  begin
    { Remove events from previous connected castle transform }
    FConnectedTransform.AddNotificationOnBehaviorAdded(
      @OnConnectedTransformAddBehavior, TCastleRigidBody);

    FConnectedTransform.AddNotificationBeforeBehaviorRemove(
      @OnConnectedTransfromRemoveBehavior, TCastleRigidBody);
  end;

  { Stop watching this transform }
  if FConnectedTransfromFreeObserver <> nil then
    FConnectedTransfromFreeObserver.Observed := nil;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.AddNotificationOnInitialized(@OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.AddNotificationBeforeDeinitialized(@OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

procedure TAbstractTwoBodiesJoint.RemoveConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.RemoveNotificationOnInitialized(@OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.RemoveNotificationBeforeDeinitialized(@OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

function TAbstractTwoBodiesJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'ConnectedTransform') or
     (PropertyName = 'ConnectedCollide') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TAbstractTwoBodiesJoint.ParentAfterAttach;
begin
  { Remove connected transform if new parent is the same transform }
  if Parent = FConnectedTransform then
  begin
    WritelnWarning('I disconnect the connected transform because it is the same transform as parent.');
    RemoveConnectedRigidBodyEvents;
    FConnectedRigidBody := nil;
    RemoveConnectedTransformEvents;
    FConnectedTransform := nil;
  end;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedTransform(
  const AConnectedTransform: TCastleTransform);
begin
  if (AConnectedTransform <> nil) and (AConnectedTransform = Parent) then
    raise EConnectedTransformTheSameAsParent.Create(
      'Can''t set ConnectedTransform to behavior Parent.');

  if FConnectedTransform <> AConnectedTransform then
  begin
    if FConnectedTransform <> nil then
    begin
      { Remove events from previous connected castle transform }
      RemoveConnectedTransformEvents;

      { Remove events from Rigidbody of old connected transform }
      RemoveConnectedRigidBodyEvents;
    end;

    FConnectedTransform := AConnectedTransform;
    if FConnectedTransform <> nil then
    begin
      { If FConnectedTransform is not nil set all events and try
        recreate joint }
      SetConnectedTransformEvents;

      FConnectedRigidBody := FConnectedTransform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
      if FConnectedRigidBody <> nil then
      begin
        SetConnectedRigidBodyEvents;
        RecreateKraftJoint;
      end;
    end;
  end;
end;

{ TAbstractJoint ------------------------------------------------------------- }

procedure TAbstractJoint.RecreateKraftJoint;
begin
  DeinitializeKraftJoint;
  if AllNeededKraftObjectsInitialized then
    InitializeKraftJoint;
end;

procedure TAbstractJoint.InitializeKraftJoint;
begin
  CreateKraftJoint;
  UpdateBreakableInKraft;
  UpdateBreakThresholdForceInKraft;
  UpdateBreakThresholdTorqueInKraft;
end;

function TAbstractJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (FParentRigidBody <> nil) and FParentRigidBody.IsInitialized;
end;

procedure TAbstractJoint.OnParentTransformAddBehavior(
  const Component: TComponent);
begin
  FParentRigidBody := Component as TCastleRigidBody;
  SetParentRigidBodyEvents;
end;

procedure TAbstractJoint.OnBeforeParentTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveParentRigidBodyEvents;
  FParentRigidBody := nil;
end;

procedure TAbstractJoint.OnParentRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TAbstractJoint.OnBeforeParentRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TAbstractJoint.SetParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.AddNotificationOnInitialized(@OnParentRigidBodyInitialized);
    FParentRigidBody.AddNotificationBeforeDeinitialized(@OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TAbstractJoint.RemoveParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.RemoveNotificationOnInitialized(@OnParentRigidBodyInitialized);
    FParentRigidBody.RemoveNotificationBeforeDeinitialized(@OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TAbstractJoint.UpdateBreakThresholdForceInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdForce := FBreakThresholdForce;
end;

procedure TAbstractJoint.UpdateBreakThresholdTorqueInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdTorque := FBreakThresholdTorque;
end;

procedure TAbstractJoint.UpdateBreakableInKraft;
begin
  if GetKraftJoint <> nil then
  begin
    if FBreakable then
      GetKraftJoint.Flags := GetKraftJoint.Flags + [kcfBreakable]
    else
      GetKraftJoint.Flags := GetKraftJoint.Flags - [kcfBreakable];
  end;
end;

procedure TAbstractJoint.SetBreakThresholdForce(const AValue: Single);
begin
  FBreakThresholdForce := AValue;
  UpdateBreakThresholdForceInKraft;
end;

procedure TAbstractJoint.SetBreakThresholdTorque(const AValue: Single);
begin
  FBreakThresholdTorque := AValue;
  UpdateBreakThresholdTorqueInKraft;
end;

procedure TAbstractJoint.SetBreakable(const AValue: Boolean);
begin
  FBreakable := AValue;
  UpdateBreakableInKraft;
end;

destructor TAbstractJoint.Destroy;
begin
  DeinitializeKraftJoint;
  inherited Destroy;
end;

procedure TAbstractJoint.WorldAfterAttach;
begin
  inherited WorldAfterAttach;

  { This means we are added to world so we need to get parent and check
    Parent has rigid body if has we should try recreate kraft joint. }

  Parent.AddNotificationOnBehaviorAdded(
    @OnParentTransformAddBehavior, TCastleRigidBody);

  Parent.AddNotificationBeforeBehaviorRemove(
    @OnBeforeParentTransfromRemoveBehavior, TCastleRigidBody);

  FParentRigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if FParentRigidBody <> nil then
  begin
    SetParentRigidBodyEvents;
    RecreateKraftJoint;
  end;
end;

procedure TAbstractJoint.WorldBeforeDetach;
begin
  { This means we are before detach from world so we need to get parent
  remove all callbacks also rigid body callbacks }

  Parent.RemoveNotificationOnBehaviorAdded(
    @OnParentTransformAddBehavior, TCastleRigidBody);

  Parent.RemoveNotificationBeforeBehaviorRemove(
    @OnBeforeParentTransfromRemoveBehavior, TCastleRigidBody);

  RemoveParentRigidBodyEvents;
  inherited ParentBeforeDetach;
end;

procedure TAbstractJoint.RemoveAuxiliaryEditorUi(const TransformsToSynchronize:
  TCastleTransformList);
begin
  // default implementation does nothing
end;

function TAbstractJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'Breakable') or
     (PropertyName = 'BreakThresholdForce') or
     (PropertyName = 'BreakThresholdTorque') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TTemporaryTransform ------------------------------------------------------- }

procedure TTemporaryTransform.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    if FSphere <> nil then
      TCastleSphere(FSphere).Color := FColor;
  end;
end;

procedure TTemporaryTransform.ApplyWireframeEffectToParent;
begin
  if RenderOptionsForParentScene = nil then
  begin
    RenderOptionsForParentScene := TCastleRenderOptions.Create(nil);
    RenderOptionsForParentScene.WireframeEffect := weWireframeOnly;
    RenderOptionsForParentScene.WireframeColor := Vector3(1.0, 0, 1.0);
  end;

  if Parent is TCastleScene then
    TCastleScene(Parent).InternalRenderOptions := RenderOptionsForParentScene
  else
  if Parent is TCastleAbstractPrimitive then
    TCastleAbstractPrimitive(Parent).InternalRenderOptions := RenderOptionsForParentScene;
end;

procedure TTemporaryTransform.RemoveWireframeEffectFromParent;
begin
  if Parent is TCastleScene then
    TCastleScene(Parent).InternalRenderOptions := nil
  else
  if Parent is TCastleAbstractPrimitive then
    TCastleAbstractPrimitive(Parent).InternalRenderOptions := nil;
end;

function TTemporaryTransform.EstimateSphereRadius: Single;
var
  Viewport: TCastleViewport;
  SphereWorldPos1, SphereWorldPos2: TVector3;
  SphereViewportPos1, SphereViewportPos2: TVector2;
  DistanceOnViewport: Single;
begin
  if (Parent <> nil) and (Parent.World <> nil) and (Parent.World.Owner <> nil) and
     (Parent.World.Owner is TCastleViewport) then
  begin
    Viewport := Parent.World.Owner as TCastleViewport;
    SphereWorldPos1 := FSphere.LocalToWorld(Vector3(0, 0, 0));
    SphereViewportPos1 := Viewport.PositionFromWorld(SphereWorldPos1);

    SphereWorldPos2 := SphereWorldPos1 + Vector3(0, 1, 0);
    SphereViewportPos2 := Viewport.PositionFromWorld(SphereWorldPos2);
    DistanceOnViewport := PointsDistance(SphereViewportPos1, SphereViewportPos2);

    { Make size about 10 pixels }
    Result := 10 / DistanceOnViewport;
  end
  else
    Result := 1;
end;

procedure TTemporaryTransform.SetValue(const AValue: TVector3);
begin
  SetObservedValue(AValue);
  if not TVector3.PerfectlyEquals(Translation, AValue) then
    Translation := AValue;
end;

procedure TTemporaryTransform.CheckTransformInsideParent;
begin
  if Parent = nil then
    Exit;

  if Parent.LocalBoundingBox.Contains(Translation) then
    ApplyWireframeEffectToParent
  else
    RemoveWireframeEffectFromParent;
end;

procedure TTemporaryTransform.ChangedTransform;
begin
  inherited ChangedTransform;
  if not TVector3.PerfectlyEquals(Translation, Value) then
    Value := Translation;
end;

constructor TTemporaryTransform.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner);

  FColor := Red;
  FJoint := AJoint;
  SetTransient;

  FSphere := TCastleSphere.Create(nil);
  FSphere.SetTransient;
  TCastleSphere(FSphere).UseInternalGlobalRenderOptions := false; // never change rendering to global
  TCastleSphere(FSphere).Color := FColor;
  TCastleSphere(FSphere).Material := pmUnlit;
  TCastleSphere(FSphere).Radius := EstimateSphereRadius;
  Add(FSphere);
end;

destructor TTemporaryTransform.Destroy;
begin
  RemoveWireframeEffectFromParent;
  FreeAndNil(FSphere);
  inherited Destroy;
end;

procedure TTemporaryTransform.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  NewRadius: Single;
begin
  inherited Update(SecondsPassed, RemoveMe);

  if FSphere <> nil then
  begin
    NewRadius := EstimateSphereRadius;

    if not SameValue(NewRadius, TCastleSphere(FSphere).Radius) then
    begin
      TCastleSphere(FSphere).Radius := NewRadius;
    end;
  end;

  CheckTransformInsideParent;
end;

{ TTemporaryAnchor ----------------------------------------------------------- }

procedure TTemporaryAnchor.SetObservedValue(const AValue: TVector3);
begin
  // TODO: move anchor to abstract anchor class or virtual function?

  if Joint is TJointHinge then
  begin
    if not TVector3.PerfectlyEquals(TJointHinge(Joint).Anchor, AValue) then
      TJointHinge(Joint).Anchor := AValue;
  end;

  if Joint is TJointRope then
  begin
    if not TVector3.PerfectlyEquals(TJointRope(Joint).Anchor, AValue) then
      TJointRope(Joint).Anchor := AValue;
  end;

  if Joint is TJointFixed then
  begin
    if not TVector3.PerfectlyEquals(TJointFixed(Joint).Anchor, AValue) then
      TJointFixed(Joint).Anchor := AValue;
  end;

  if Joint is TJointBall then
  begin
    if not TVector3.PerfectlyEquals(TJointBall(Joint).Anchor, AValue) then
      TJointBall(Joint).Anchor := AValue;
  end;

  if Joint is TJointDistance then
  begin
    if not TVector3.PerfectlyEquals(TJointDistance(Joint).Anchor, AValue) then
      TJointDistance(Joint).Anchor := AValue;
  end;

  if Joint is TJointWorldPlaneDistance then
  begin
    if not TVector3.PerfectlyEquals(TJointWorldPlaneDistance(Joint).Anchor, AValue) then
      TJointWorldPlaneDistance(Joint).Anchor := AValue;
  end;

end;

function TTemporaryAnchor.GetObservedValue: TVector3;
begin
  // TODO: move anchor to abstract anchor class or virtual function?

  if Joint is TJointHinge then
    Exit(TJointHinge(Joint).Anchor);

  if Joint is TJointRope then
    Exit(TJointRope(Joint).Anchor);

  if Joint is TJointFixed then
    Exit(TJointFixed(Joint).Anchor);

  if Joint is TJointBall then
    Exit(TJointBall(Joint).Anchor);

  if Joint is TJointDistance then
    Exit(TJointDistance(Joint).Anchor);

  if Joint is TJointWorldPlaneDistance then
    Exit(TJointWorldPlaneDistance(Joint).Anchor);
end;

{ TTemporaryConnectedAnchor -------------------------------------------------- }

procedure TTemporaryConnectedAnchor.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TJointRope then
  begin
    if not TVector3.PerfectlyEquals(TJointRope(Joint).ConnectedAnchor, AValue) then
      TJointRope(Joint).ConnectedAnchor := AValue;
  end;

  if Joint is TJointDistance then
  begin
    if not TVector3.PerfectlyEquals(TJointDistance(Joint).ConnectedAnchor, AValue) then
      TJointDistance(Joint).ConnectedAnchor := AValue;
  end;
end;

function TTemporaryConnectedAnchor.GetObservedValue: TVector3;
begin
  if Joint is TJointRope then
    Exit(TJointRope(Joint).ConnectedAnchor);
end;

constructor TTemporaryConnectedAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;

{ TTemporaryWorldPoint ------------------------------------------------------- }

procedure TTemporaryWorldPoint.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TJointGrab then
  begin
    if not TVector3.PerfectlyEquals(TJointGrab(Joint).WorldPoint, AValue) then
      TJointGrab(Joint).WorldPoint := AValue;
  end;
end;

function TTemporaryWorldPoint.GetObservedValue: TVector3;
begin
  if Joint is TJointGrab then
    Exit(TJointGrab(Joint).WorldPoint);
end;

constructor TTemporaryWorldPoint.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Blue;
end;


{ TTemporaryWorldAnchor ------------------------------------------------------ }

procedure TTemporaryWorldAnchor.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TJointPulley then
  begin
    if not TVector3.PerfectlyEquals(TJointPulley(Joint).WorldAnchor, AValue) then
      TJointPulley(Joint).WorldAnchor := AValue;
  end;

  if Joint is TJointSlider then
  begin
    if not TVector3.PerfectlyEquals(TJointSlider(Joint).WorldAnchor, AValue) then
      TJointSlider(Joint).WorldAnchor := AValue;
  end;
end;

function TTemporaryWorldAnchor.GetObservedValue: TVector3;
begin
  if Joint is TJointPulley then
    Exit(TJointPulley(Joint).WorldAnchor);

  if Joint is TJointSlider then
    Exit(TJointSlider(Joint).WorldAnchor);
end;

constructor TTemporaryWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Red;
end;

{ TTemporaryConnectedWorldAnchor --------------------------------------------- }

procedure TTemporaryConnectedWorldAnchor.SetObservedValue(const AValue: TVector3
  );
begin
  if Joint is TJointPulley then
  begin
    if not TVector3.PerfectlyEquals(TJointPulley(Joint).ConnectedWorldAnchor, AValue) then
      TJointPulley(Joint).ConnectedWorldAnchor := AValue;
  end;
end;

function TTemporaryConnectedWorldAnchor.GetObservedValue: TVector3;
begin
  if Joint is TJointPulley then
    Exit(TJointPulley(Joint).ConnectedWorldAnchor);
end;

constructor TTemporaryConnectedWorldAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Green;
end;


{ TTemporaryWorldGroundAnchor ------------------------------------------------ }

procedure TTemporaryWorldGroundAnchor.SetObservedValue(const AValue: TVector3);
begin
  if Joint is TJointPulley then
  begin
    if not TVector3.PerfectlyEquals(TJointPulley(Joint).WorldGroundAnchor, AValue) then
      TJointPulley(Joint).WorldGroundAnchor := AValue;
  end;
end;

function TTemporaryWorldGroundAnchor.GetObservedValue: TVector3;
begin
  if Joint is TJointPulley then
    Exit(TJointPulley(Joint).WorldGroundAnchor);
end;

constructor TTemporaryWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Orange;
end;

{ TTemporaryConnectedWorldGroundAnchor --------------------------------------- }

procedure TTemporaryConnectedWorldGroundAnchor.SetObservedValue(
  const AValue: TVector3);
begin
  if Joint is TJointPulley then
  begin
    if not TVector3.PerfectlyEquals(TJointPulley(Joint).ConnectedWorldGroundAnchor, AValue) then
      TJointPulley(Joint).ConnectedWorldGroundAnchor := AValue;
  end;
end;

function TTemporaryConnectedWorldGroundAnchor.GetObservedValue: TVector3;
begin
  if Joint is TJointPulley then
    Exit(TJointPulley(Joint).ConnectedWorldGroundAnchor);
end;

constructor TTemporaryConnectedWorldGroundAnchor.Create(AOwner: TComponent;
  const AJoint: TAbstractJoint);
begin
  inherited Create(AOwner, AJoint);
  Color := Teal;
end;

{ TJointHinge ---------------------------------------------------------------- }

procedure TJointHinge.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    FAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TJointHinge.SetRotationAxis(const AValue: TVector3);
begin
  FRotationAxis := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetWorldRotationAxis(
      VectorToKraft(Parent.LocalToWorldDirection(FRotationAxis)));
end;

procedure TJointHinge.SetEnableLimits(const AValue: Boolean);
begin
  if FEnableLimits = AValue then
    Exit;

  FEnableLimits := AValue;

  if FKraftJointHinge <> nil then
    FKraftJointHinge.EnableLimit(AValue);
end;

procedure TJointHinge.SetMinAngleLimit(const AValue: Single);
begin
  FMinAngleLimit := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetMinimumAngleLimit(FMinAngleLimit);
end;

procedure TJointHinge.SetMaxAngleLimit(const AValue: Single);
begin
  FMaxAngleLimit := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetMaximumAngleLimit(FMaxAngleLimit);
end;

procedure TJointHinge.SetEnableMotor(const AValue: Boolean);
begin
  if FEnableMotor = AValue then
    Exit;

  FEnableMotor := AValue;

  if FKraftJointHinge <> nil then
    FKraftJointHinge.EnableMotor(AValue);
end;

procedure TJointHinge.SetMotorSpeed(const AValue: Single);
begin
  FMotorSpeed := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetMotorSpeed(AValue);
end;

function TJointHinge.GetMotorSpeed: Single;
begin
  if FKraftJointHinge <> nil then
    FMotorSpeed := FKraftJointHinge.GetMotorSpeed;

  Result := FMotorSpeed;
end;

procedure TJointHinge.SetMaxMotorTorque(const AValue: Single);
begin
  if SameValue(FMaxMotorTorque, AValue) then
    Exit;

  FMaxMotorTorque := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetMaximalMotorTorque(FMaxMotorTorque);
end;

function TJointHinge.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointHinge;
end;

procedure TJointHinge.CreateKraftJoint;
begin
  FKraftJointHinge := TKraftConstraintJointHinge.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    VectorToKraft(Parent.LocalToWorldDirection(FRotationAxis)),
    FEnableLimits,
    FEnableMotor,
    FMinAngleLimit,
    FMaxAngleLimit,
    FMotorSpeed,
    FMaxMotorTorque,
    ConnectedCollide
  );
end;

procedure TJointHinge.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointHinge);
end;

constructor TJointHinge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FRotationAxis:= Vector3(0, 0, 1);
  FAnchor := Vector3(0, 0, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointhinge_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointHinge.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointhinge_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointHinge.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointHinge.InternalSelectionStart(TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Value := Anchor;
  end;
end;

procedure TJointHinge.RemoveAuxiliaryEditorUi(const TransformsToSynchronize: TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointHinge.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'EnableLimits') or
     (PropertyName = 'MinAngleLimit') or
     (PropertyName = 'MaxAngleLimit') or
     (PropertyName = 'EnableMotor') or
     (PropertyName = 'MotorSpeed') or
     (PropertyName = 'MaxMotorTorque') or
     (PropertyName = 'RotationAxisPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointhinge_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointRope --------------------------------------------------------------- }

procedure TJointRope.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    FAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TJointRope.SetConnectedAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FConnectedAnchor, AValue) then
    Exit;

  FConnectedAnchor := AValue;
  if FConnectedAnchorTransform <> nil then
    FConnectedAnchorTransform.Value := FConnectedAnchor;
  RecreateKraftJoint;
end;

procedure TJointRope.SetDistance(const AValue: Single);
begin
  if SameValue(FDistance, AValue) then
    Exit;
  FDistance := AValue;
  RecreateKraftJoint;
end;

function TJointRope.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointRope;
end;

procedure TJointRope.CreateKraftJoint;
begin
  FKraftJointRope := TKraftConstraintJointRope.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    VectorToKraft(FConnectedAnchor),
    FDistance,
    ConnectedCollide
  );
end;

procedure TJointRope.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointRope);
end;

procedure TJointRope.OnConnectedTransformFree(const Sender: TFreeNotificationObserver);
begin
  { Remove connected transform anchor visualisation because
    its parent is freed }
  FreeAndNil(FConnectedAnchorTransform);

  inherited OnConnectedTransformFree(Sender);
end;

constructor TJointRope.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := Vector3(0, 0, 0);
  FConnectedAnchor := Vector3(0, 0, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointrope_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointRope.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointrope_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointRope.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointRope.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Translation := Anchor;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TTemporaryConnectedAnchor.Create(nil, Self);
    FConnectedAnchorTransform.Name := 'ConnectedAnchorHandle';
    FConnectedAnchorTransform.Translation := ConnectedAnchor;
    TransformsToSynchronize.Add(FConnectedRigidBody.Parent);
    FConnectedRigidBody.Parent.Add(FConnectedAnchorTransform);
  end;
end;

function TJointRope.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'ConnectedAnchorPersistent') or
     (PropertyName = 'Distance') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TJointRope.RemoveAuxiliaryEditorUi(const TransformsToSynchronize:
  TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  if FConnectedAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FConnectedAnchorTransform.Parent);
    FreeAndNil(FConnectedAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointrope_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointFixed --------------------------------------------------------------- }

procedure TJointFixed.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    FAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

function TJointFixed.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointFixed;
end;

procedure TJointFixed.CreateKraftJoint;
begin
  FKraftJointFixed := TKraftConstraintJointFixed.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    ConnectedCollide
  );
end;

procedure TJointFixed.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointFixed);
end;

constructor TJointFixed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := Vector3(0, 0, 0);
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointfixed_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointFixed.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointfixed_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointFixed.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointFixed.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Value := Anchor;
  end;
end;

procedure TJointFixed.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointFixed.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointfixed_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointBall --------------------------------------------------------------- }

procedure TJointBall.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    FAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

function TJointBall.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointBall;
end;

procedure TJointBall.CreateKraftJoint;
begin
  FKraftJointBall := TKraftConstraintJointBallSocket.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    ConnectedCollide
  );
end;

procedure TJointBall.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointBall);
end;

constructor TJointBall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := Vector3(0, 0, 0);
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointball_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointBall.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointball_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointBall.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointBall.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Value := Anchor;
  end;
end;

procedure TJointBall.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointBall.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointball_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointDistance ------------------------------------------------------------- }

procedure TJointDistance.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    FAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TJointDistance.SetConnectedAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FConnectedAnchor, AValue) then
    Exit;

  FConnectedAnchor := AValue;
  if FConnectedAnchorTransform <> nil then
    FConnectedAnchorTransform.Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TJointDistance.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TJointDistance.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
end;

function TJointDistance.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointDistance;
end;

procedure TJointDistance.CreateKraftJoint;
begin
  Assert(FKraftJointDistance = nil, 'Second joint initialization');
  FKraftJointDistance := TKraftConstraintJointDistance.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    VectorToKraft(FConnectedAnchor),
    FFrequencyHz,
    FDampingRatio,
    ConnectedCollide
  );
end;

procedure TJointDistance.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointDistance);
end;

constructor TJointDistance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedAnchor := CastleVectors.Vector3(0, 0, 0);
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointdistance_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointDistance.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointdistance_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointDistance.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointDistance.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Translation := Anchor;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TTemporaryConnectedAnchor.Create(nil, Self);
    FConnectedAnchorTransform.Name := 'ConnectedAnchorHandle';
    FConnectedAnchorTransform.Translation := ConnectedAnchor;
    TransformsToSynchronize.Add(FConnectedRigidBody.Parent);
    FConnectedRigidBody.Parent.Add(FConnectedAnchorTransform);
  end;
end;

procedure TJointDistance.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  if FConnectedAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FConnectedAnchorTransform.Parent);
    FreeAndNil(FConnectedAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointDistance.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'ConnectedAnchorPersistent') or
     (PropertyName = 'DampingRatio') or
     (PropertyName = 'FrequencyHz') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointdistance_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointGrab ----------------------------------------------------------------- }

procedure TJointGrab.SetWorldPoint(const AValue: TVector3);
begin
  FWorldPoint := AValue;

  if FWorldPointTransform <> nil then
    FWorldPointTransform.Value := FWorldPoint;

  if FKraftJointGrab <> nil then
    FKraftJointGrab.SetWorldPoint(VectorToKraft(FWorldPoint));
end;

procedure TJointGrab.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TJointGrab.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
end;

procedure TJointGrab.SetMaximalForce(const AValue: Single);
begin
  if SameValue(FMaximalForce, AValue) then
    Exit;

  FMaximalForce := AValue;
  if FKraftJointGrab <> nil then
    FKraftJointGrab.SetMaximalForce(AValue);
end;

function TJointGrab.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointGrab;
end;

procedure TJointGrab.CreateKraftJoint;
begin
  Assert(FKraftJointGrab = nil, 'Second joint initialization');
  FKraftJointGrab := TKraftConstraintJointGrab.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    VectorToKraft(FWorldPoint),
    FFrequencyHz,
    FDampingRatio,
    FMaximalForce
  );
end;

procedure TJointGrab.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointGrab);
end;

constructor TJointGrab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FWorldPoint := CastleVectors.Vector3(0, 0, 0);
  FMaximalForce := MaxSingle;
  FFrequencyHz := 5.0;
  FDampingRatio := 0.7;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointgrab_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointGrab.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointgrab_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointGrab.WorldBeforeDetach;
begin
  FreeAndNil(FWorldPointTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointGrab.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FWorldPointTransform = nil then
  begin
    FWorldPointTransform := TTemporaryWorldPoint.Create(nil, Self);
    FWorldPointTransform.Name := 'WorldPointHandle';
    Parent.World.Add(FWorldPointTransform);
    TransformsToSynchronize.Add(Parent.World);
    FWorldPointTransform.Translation := WorldPoint;
  end;
end;

procedure TJointGrab.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);

  if FWorldPointTransform <> nil then
  begin
    TransformsToSynchronize.Add(FWorldPointTransform.Parent);
    FreeAndNil(FWorldPointTransform);
  end;
end;

function TJointGrab.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldPointPersistent') or
     (PropertyName = 'DampingRatio') or
     (PropertyName = 'FrequencyHz') or
     (PropertyName = 'MaximalForce') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointgrab_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointPulley ---------------------------------------------------------------- }

procedure TJointPulley.SetWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldAnchor, AValue) then
    Exit;

  FWorldAnchor := AValue;

  if FWorldAnchorTransform <> nil then
    FWorldAnchorTransform.Value := AValue;

  RecreateKraftJoint;
end;

procedure TJointPulley.SetConnectedWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FConnectedWorldAnchor, AValue) then
    Exit;

  FConnectedWorldAnchor := AValue;

  if FConnectedWorldAnchorTransform <> nil then
    FConnectedWorldAnchorTransform.Value := AValue;

  RecreateKraftJoint;
end;

procedure TJointPulley.SetWorldGroundAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldGroundAnchor, AValue) then
    Exit;

  FWorldGroundAnchor := AValue;

  if FWorldGroundAnchorTransform <> nil then
    FWorldGroundAnchorTransform.Value := AValue;

  RecreateKraftJoint;
end;

procedure TJointPulley.SetConnectedWorldGroundAnchor(const AValue: TVector3
  );
begin
  if TVector3.PerfectlyEquals(FConnectedWorldGroundAnchor, AValue) then
    Exit;

  FConnectedWorldGroundAnchor := AValue;

  if FConnectedWorldGroundAnchorTransform <> nil then
    FConnectedWorldGroundAnchorTransform.Value := AValue;

  RecreateKraftJoint;
end;

procedure TJointPulley.SetRatio(const AValue: Single);
begin
  if SameValue(FRatio, AValue) then
    Exit;

  FRatio := AValue;
  RecreateKraftJoint;
end;

function TJointPulley.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointPulley;
end;

procedure TJointPulley.CreateKraftJoint;
begin
  Assert(FKraftJointPulley = nil, 'Second joint initialization');
  FKraftJointPulley := TKraftConstraintJointPulley.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FWorldGroundAnchor),
    VectorToKraft(FConnectedWorldGroundAnchor),
    VectorToKraft(FWorldAnchor),
    VectorToKraft(FConnectedWorldAnchor),
    FRatio,
    ConnectedCollide
  );
end;

procedure TJointPulley.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointPulley);
end;

constructor TJointPulley.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;

  {FWorldAnchor := CastleVectors.Vector3(0, 0, 0);
  FWorldGroundAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedWorldAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedWorldGroundAnchor := CastleVectors.Vector3(0, 0, 0);}

  FWorldAnchor := CastleVectors.Vector3(-1, -1, 0);
  FWorldGroundAnchor := CastleVectors.Vector3(-1, 0, 0);
  FConnectedWorldAnchor := CastleVectors.Vector3(1, -1, 0);
  FConnectedWorldGroundAnchor := CastleVectors.Vector3(1, 0, 0);

  FRatio := 1.0;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointpulley_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointPulley.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointpulley_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointPulley.WorldBeforeDetach;
begin
  FreeAndNil(FWorldAnchorTransform);
  FreeAndNil(FConnectedWorldAnchorTransform);
  FreeAndNil(FWorldGroundAnchorTransform);
  FreeAndNil(FConnectedWorldGroundAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointPulley.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FWorldAnchorTransform = nil then
  begin
    FWorldAnchorTransform := TTemporaryWorldAnchor.Create(nil, Self);
    FWorldAnchorTransform.Name := 'WorldAnchorHandle';
    Parent.World.Add(FWorldAnchorTransform);
    TransformsToSynchronize.Add(Parent.World);
    FWorldAnchorTransform.Translation := WorldAnchor;
  end;

  if FConnectedWorldAnchorTransform = nil then
  begin
    FConnectedWorldAnchorTransform := TTemporaryConnectedWorldAnchor.Create(nil, Self);
    FConnectedWorldAnchorTransform.Name := 'ConnectedWorldAnchorHandle';
    Parent.World.Add(FConnectedWorldAnchorTransform);
    TransformsToSynchronize.Add(Parent.World);
    FConnectedWorldAnchorTransform.Translation := ConnectedWorldAnchor;
  end;

  if FWorldGroundAnchorTransform = nil then
  begin
    FWorldGroundAnchorTransform := TTemporaryWorldGroundAnchor.Create(nil, Self);
    FWorldGroundAnchorTransform.Name := 'WorldGroundAnchorHandle';
    Parent.World.Add(FWorldGroundAnchorTransform);
    TransformsToSynchronize.Add(Parent.World);
    FWorldGroundAnchorTransform.Translation := WorldGroundAnchor;
  end;

  if FConnectedWorldGroundAnchorTransform = nil then
  begin
    FConnectedWorldGroundAnchorTransform := TTemporaryConnectedWorldGroundAnchor.Create(nil, Self);
    FConnectedWorldGroundAnchorTransform.Name := 'ConnectedWorldGroundAnchorHandle';
    Parent.World.Add(FConnectedWorldGroundAnchorTransform);
    TransformsToSynchronize.Add(Parent.World);
    FConnectedWorldGroundAnchorTransform.Translation := ConnectedWorldGroundAnchor;
  end;
end;

procedure TJointPulley.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  if FWorldAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FWorldAnchorTransform.Parent);
    FreeAndNil(FWorldAnchorTransform);
  end;

  if FConnectedWorldAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FConnectedWorldAnchorTransform.Parent);
    FreeAndNil(FConnectedWorldAnchorTransform);
  end;

  if FWorldGroundAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FWorldGroundAnchorTransform.Parent);
    FreeAndNil(FWorldGroundAnchorTransform);
  end;

  if FConnectedWorldGroundAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FConnectedWorldGroundAnchorTransform.Parent);
    FreeAndNil(FConnectedWorldGroundAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointPulley.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldAnchorPersistent') or
     (PropertyName = 'ConnectedWorldAnchorPersistent') or
     (PropertyName = 'WorldGroundAnchorPersistent') or
     (PropertyName = 'ConnectedWorldGroundAnchorPersistent') or
     (PropertyName = 'Ratio') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointpulley_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TJointWorldPlaneDistance --------------------------------------------------- }

procedure TJointWorldPlaneDistance.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  RecreateKraftJoint;
end;

procedure TJointWorldPlaneDistance.SetPlaneNormal(const AValue: TVector3);
begin
  if TVector3.Equals(FPlaneNormal, AValue) then
    Exit;

  FPlaneNormal := AValue;
  RecreateKraftJoint;
end;

procedure TJointWorldPlaneDistance.SetPlaneDistance(const AValue: Single);
begin
  if SameValue(FPlaneDistance, AValue) then
    Exit;

  FPlaneDistance := AValue;
  RecreateKraftJoint;
end;

procedure TJointWorldPlaneDistance.SetWorldDistance(const AValue: Single);
begin
  if SameValue(FWorldDistance, AValue) then
    Exit;

  FWorldDistance := AValue;
  if FKraftJointWorldPlaneDistance <> nil then
    FKraftJointWorldPlaneDistance.SetWorldDistance(FWorldDistance);
end;

procedure TJointWorldPlaneDistance.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TJointWorldPlaneDistance.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
end;

procedure TJointWorldPlaneDistance.DoubleSidedWorldPlane(const AValue: Boolean);
begin
  if FDoubleSidedWorldPlane = AValue then
    Exit;

  FDoubleSidedWorldPlane := AValue;
  RecreateKraftJoint;
end;

function TJointWorldPlaneDistance.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointWorldPlaneDistance;
end;

procedure TJointWorldPlaneDistance.CreateKraftJoint;
begin
  Assert(FKraftJointWorldPlaneDistance = nil, 'Second joint initialization');
  FKraftJointWorldPlaneDistance := TKraftConstraintJointWorldPlaneDistance.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    Plane(Vector3Norm(VectorToKraft(FPlaneNormal)), FPlaneDistance),
    FDoubleSidedWorldPlane,
    FWorldDistance,
    kclbLimitDistance,
    //kclbLimitMinimumDistance,
    //kclbLimitMaximumDistance,
    FFrequencyHz,
    FDampingRatio
  );
end;

procedure TJointWorldPlaneDistance.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointWorldPlaneDistance);
end;

constructor TJointWorldPlaneDistance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  FPlaneNormal := CastleVectors.Vector3(0, 1, 0);
  NeedWorldChangeNotification := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointworldplanedistance_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointWorldPlaneDistance.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointworldplanedistance_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointWorldPlaneDistance.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointWorldPlaneDistance.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TTemporaryAnchor.Create(nil, Self);
    FAnchorTransform.Name := 'AnchorHandle';
    Parent.Add(FAnchorTransform);
    TransformsToSynchronize.Add(Parent);
    FAnchorTransform.Value := Anchor;
  end;
end;

procedure TJointWorldPlaneDistance.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  if FAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FAnchorTransform.Parent);
    FreeAndNil(FAnchorTransform);
  end;

  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);
end;

function TJointWorldPlaneDistance.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'PlaneNormalPersistent') or
     (PropertyName = 'PlaneDistance') or
     (PropertyName = 'WorldDistance') or
     (PropertyName = 'FrequencyHz') or
     (PropertyName = 'DampingRatio') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointworldplanedistance_persistent_vectors.inc}
{$undef read_implementation_methods}


{ TJointSlider }

procedure TJointSlider.SetWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldAnchor, AValue) then
    Exit;

  FWorldAnchor := AValue;
  if FWorldAnchorTransform <> nil then
    FWorldAnchorTransform.Value := FWorldAnchor;
  RecreateKraftJoint;
end;

procedure TJointSlider.SetWorldSliderAxis(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldSliderAxis, AValue) then
    Exit;

  FWorldSliderAxis := AValue;
  RecreateKraftJoint;
end;

procedure TJointSlider.SetEnableLimits(const AValue: Boolean);
begin
  if FEnableLimits = AValue then
    Exit;

  FEnableLimits := AValue;

  if FKraftJointSlider <> nil then
    FKraftJointSlider.EnableLimit(AValue);
end;

procedure TJointSlider.SetMinTranslationLimit(const AValue: Single);
begin
  FMinTranslationLimit := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMinimumTranslationLimit(FMinTranslationLimit);
end;

procedure TJointSlider.SetMaxTranslationLimit(const AValue: Single);
begin
  FMaxTranslationLimit := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMaximumTranslationLimit(FMaxTranslationLimit);
end;

procedure TJointSlider.SetEnableMotor(const AValue: Boolean);
begin
  if FEnableMotor = AValue then
    Exit;

  FEnableMotor := AValue;

  if FKraftJointSlider <> nil then
    FKraftJointSlider.EnableMotor(AValue);
end;

procedure TJointSlider.SetMotorSpeed(const AValue: Single);
begin
  FMotorSpeed := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMotorSpeed(AValue);
end;

function TJointSlider.GetMotorSpeed: Single;
begin
  if FKraftJointSlider <> nil then
    FMotorSpeed := FKraftJointSlider.GetMotorSpeed;

  Result := FMotorSpeed;
end;

procedure TJointSlider.SetMaxMotorForce(const AValue: Single);
begin
  if SameValue(FMaxMotorForce, AValue) then
    Exit;

  FMaxMotorForce := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMaximalMotorForce(FMaxMotorForce);
end;

function TJointSlider.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointSlider;
end;

procedure TJointSlider.CreateKraftJoint;
begin
  FKraftJointSlider := TKraftConstraintJointSlider.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FWorldAnchor),
    VectorToKraft(FWorldSliderAxis),
    FEnableLimits,
    FEnableMotor,
    FMinTranslationLimit,
    FMaxTranslationLimit,
    GetMotorSpeed,
    FMaxMotorForce,
    ConnectedCollide
  );
end;

procedure TJointSlider.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointSlider);
end;

constructor TJointSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FWorldSliderAxis := Vector3(0, 1, 0);
  FWorldAnchor := Vector3(0, 0, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tjointslider_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TJointSlider.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tjointslider_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TJointSlider.WorldBeforeDetach;
begin
  FreeAndNil(FWorldAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TJointSlider.InternalSelectionStart(
  TransformsToSynchronize: TCastleTransformList);
begin
  inherited InternalSelectionStart(TransformsToSynchronize);

  if FWorldAnchorTransform = nil then
  begin
    FWorldAnchorTransform := TTemporaryWorldAnchor.Create(nil, Self);
    FWorldAnchorTransform.Name := 'WorldAnchorHandle';
    Parent.World.Add(FWorldAnchorTransform);
    TransformsToSynchronize.Add(Parent.World);
    FWorldAnchorTransform.Value := WorldAnchor;
  end;
end;

procedure TJointSlider.RemoveAuxiliaryEditorUi(
  const TransformsToSynchronize: TCastleTransformList);
begin
  inherited RemoveAuxiliaryEditorUi(TransformsToSynchronize);

  if FWorldAnchorTransform <> nil then
  begin
    TransformsToSynchronize.Add(FWorldAnchorTransform.Parent);
    FreeAndNil(FWorldAnchorTransform);
  end;
end;

function TJointSlider.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldAnchorPersistent') or
     (PropertyName = 'EnableLimits') or
     (PropertyName = 'MinTranslationLimit') or
     (PropertyName = 'MaxTranslationLimit') or
     (PropertyName = 'EnableMotor') or
     (PropertyName = 'MotorSpeed') or
     (PropertyName = 'MaxMotorForce') or
     (PropertyName = 'WorldSliderAxisPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tjointslider_persistent_vectors.inc}
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
    FKraftEngine.AllowSleep := false;
    if PhysicsProperties.ContinuousCD then
    begin
      { I tested all options:
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
    to update transformation of all items in their TCastleRigidBody.Update
    called from TCastleTransform.Update }
  inherited;
end;

{$endif read_implementation}
