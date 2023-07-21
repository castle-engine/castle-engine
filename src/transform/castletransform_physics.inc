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

{ Physics. Integrates TCastleTransform with Kraft physics engine. }

{$ifdef read_interface}

  { Information send along with TCollisionEvent event, like
    TCastleRigidBody.OnCollisionEnter, TCastleRigidBody.OnCollisionExit. }
  TPhysicsCollisionDetails = record
  public
    Transforms: array[0..1] of TCastleTransform;
    function OtherTransform: TCastleTransform;
  end;

  TPhysicsLayer = 0..19;
  TPhysicsLayers = set of TPhysicsLayer;

  TCastleLayerCollisions = class(TCastleComponent)
  strict private
    function GetCollides(const Layer1, Layer2: TPhysicsLayer): Boolean;
    procedure SetCollides(const Layer1, Layer2: TPhysicsLayer; const AValue: Boolean);
    procedure UpdateRigidBodies;
    function DefaultCollides(const Layer: TPhysicsLayer): TPhysicsLayers;
  private
    FCollides: array [TPhysicsLayer] of TPhysicsLayers;
  public
    constructor Create(AOwner: TComponent); override;

    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { Do physical objects from indicated 2 layers collide with each other. }
    property Collides[const Layer1, Layer2: TPhysicsLayer]: Boolean read GetCollides write SetCollides;
  end;

  TCastleLayerNames = class(TCastleComponent)
  strict private
    FLayerNames: array[TPhysicsLayer] of String;
    FLayerDescriptions: array[TPhysicsLayer] of String;
    function GetLayerDescriptions(const Layer: TPhysicsLayer): String;
    function GetLayerNames(const Layer: TPhysicsLayer): String;
    procedure SetLayerNames(const Layer: TPhysicsLayer; const AValue: String);
    procedure SetLayerDescriptions(const Layer: TPhysicsLayer; const AValue: String);
  public
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { Name of each layer. This information is just for developer convenience,
      to display layer names in the editor. }
    property Names[const Layer: TPhysicsLayer]: String read GetLayerNames write SetLayerNames;

    { Longer (possibly multiline) description about each layer.
      This information is just for developer convenience,
      to document layer purpose in the editor. }
    property Descriptions[const Layer: TPhysicsLayer]: String read GetLayerDescriptions write SetLayerDescriptions;
  end;

  { Configure physics simulation calculation. }
  TPhysicsProperties = class(TComponent)
  strict private
    FAngularVelocityRK4Integration: Boolean;
    FLinearVelocityRK4Integration: Boolean;
    FFrequency: Single;
    FGravityStrength: Single;
    FMaxPhysicsTicksPerUpdate: Cardinal;
    FContinuousCollisionDetection: Boolean;
    FLayerCollisions: TCastleLayerCollisions;
    FLayerNames: TCastleLayerNames;
    procedure SetAngularVelocityRK4Integration(const AValue: Boolean);
    procedure SetLinearVelocityRK4Integration(const AValue: Boolean);
    procedure SetFrequency(const AValue: Single);
    procedure SetContinuousCollisionDetection(const AValue: Boolean);
    // unused
    //procedure SetLayerNames(const AValue: TStrings);
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
      DefaultContinuousCollisionDetection = true;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

    { Should engine use continous collision detection (CCD). }
    property ContinuousCollisionDetection: Boolean read FContinuousCollisionDetection write SetContinuousCollisionDetection default DefaultContinuousCollisionDetection;

     { Which layers collide with each other. }
    property LayerCollisions: TCastleLayerCollisions read FLayerCollisions write FLayerCollisions;

    { Name and a descriptions of layers. }
    property LayerNames: TCastleLayerNames read FLayerNames write FLayerNames;
  end;

  { Abstract collider that determines the shape used to determine collisions with physics bodies. }
  TCastleCollider = class(TCastleBehavior)
  strict private
    { Implementation note:
      This doesn't store any references to Kraft objects.
      This only describes shape.
      TCastleRigidBody takes care of initializing Kraft. }
    FDensity, FMass, FRestitution, FFriction: Single;
    FTranslation: TVector3;
    FRotation: TVector4;
    FAutoSize: Boolean;
    FCalculatingSize: Boolean; //< are we inside CalculateSize or InternalAutoSize
    { Accumulated scale of all parent TCastleTransform.
      Does not include @link(SizeScale).
      This is always converted to a single scalar either using Approximate2DScale
      or Approximate3DScale, depending on Mode2D. }
    FWorldScale: TVector3;
    FSizeScale: Single;
    FMode2D: Boolean;
    WarningNonUniformScaleDone: Boolean;

    { Updates FLocalTransformation and FUseLocalTransformation after changing
      Translation, FSizeScale, FWorldScale, Rotation and in InitializeKraft. }
    procedure UpdateLocalTransform;

    procedure SetDensity(const AValue: Single);
    procedure SetMass(const AValue: Single);
    procedure SetRestitution(const AValue: Single);
    procedure SetFriction(const AValue: Single);
    procedure SetTranslation(const AValue: TVector3);
    procedure SetRotation(const AValue: TVector4);
    procedure SetSizeScale(const AValue: Single);
    procedure SetAutoSize(const AValue: Boolean);
    function GetRigidBody: TCastleRigidBody;
    procedure SetMode2D(const AValue: Boolean);
  private
    FKraftShape: TKraftShape;  // owned by TKraftRigidBody
    { When @false, behave as if Translation/Rotation/FLocalTransform/FLocalInverseTransform
      were all "identity", i.e. "no transformation".
      This is an often case, so we want to optimize it. }
    FUseLocalTransform: Boolean;
    FLocalTransform: TMatrix4;
    FLocalInverseTransform: TMatrix4;

    { Visualizes Translation, Rotation }
    FVisualizationParent: TCastleTransform;

    { Initializes FKraftShape. }
    procedure InitializeKraft(const APhysics: TKraft; const ACastleRigidBody: TCastleRigidBody);

    { Updates FWorldScale, needed e.g. after adding collider to world.
      Calls ShapeChangedNotifyRigidBody if it changed.

      Descendants implementors:
      You should multiply your descendant sizes with TotalScale in CreateKraftShape.
      TotalScale includes FWorldScale (updated by this method) and
      FSizeScale (set explicitly by user to tweak size). }
    procedure UpdateWorldScale;

    function StoredWithAutoSize: Boolean;

    function WorldScaleScalar: Single;

    { Configure Sender (TCastleCollider) to have Mode2D. }
    class procedure CreateComponent2D(Sender: TObject);
  protected
    { Total scale, accumulated from all parents TCastleTransform.Scale (converted to scalar
      following Mode2D) and collider SizeScale. }
    function TotalScale: Single;

    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;

    { Notify associated TCastleRigidBody that Kraft shape needs to be recreated.
      This may in turn call TCastleCollider.InitializeKraft
      and TCastleCollider.CreateKraftShape. }
    procedure ShapeChangedNotifyRigidBody;

    { Called when the collider size has changed and needs to be updated. }
    procedure UpdateVisualization; virtual;
    procedure WorldAfterAttach; override;
    procedure WorldBeforeDetach; override;

    { Only one TCastleCollider behavior can be added to a given
      TCastleTransform so we check that here. }
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;

    { Sets AutoSize = @false, unless this is called
      inside InternalAutoSize or at loading.
      Call this when something changes collider size explicitly,
      e.g. TCastleSphereCollider.Radius changes. }
    procedure DisableAutoSize;

    { Recalculate collider size.
      Do not be concerned about AutoSize here, just update collider properties.
      The default implementation resets Translation and Rotation. }
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
      { Minimum collider thickness used by @link(AutoSize) in 3D mode (when @link(Mode2D) = @false).
        Changing this only affects new size calculations.}
      AutoSizeMinThickness: Single;

      { Minimum collider thickness used by @link(AutoSize) in 2D mode (when @link(Mode2D) = @true).
        This is used for axis other than Z (like X, Y, or collider radiuses that affect size
        in a few axis).
        Changing this only affects new size calculations.}
      AutoSizeMinThickness2D: Single;

      { Minimum collider thickness used by @link(AutoSize) in 2D mode (when @link(Mode2D) = @true) for Z.
        This is used only by TCastleBoxCollider now, as only that collider has specific size in Z.
        Changing this only affects new size calculations.}
      AutoSizeMinThickness2DDepth: Single;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { Notify this collider that given TCastleTransform changed.
      It can provoke AutoSize recalculation (if Parent changed)
      or other reactions (e.g. mesh update, if TransformChanged corresponds to Mesh). }
    procedure InternalTransformChanged(const TransformChanged: TCastleTransform); virtual;

    { Create collider visualization. }
    procedure InternalDesigningBegin; virtual;
    { Destroy collider visualization. }
    procedure InternalDesigningEnd; virtual;

    procedure ParentBeforeDetach; override;

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

    { Local collider size scale.

      This is particularly useful if @link(AutoSize) mechanism calculated too large box.
      For collision purposes, you sometimes want a collider that is a bit smaller
      than the bounding box/sphere around the object.
      Maybe it should even be too small at certain places, but then it will make
      a better fit overall.

      You can achieve this (and still keep using @link(AutoSize)) by adjusting
      this property e.g. to 0.9 to have collider be 90% of what
      @link(AutoSize) calculated.

      This is @bold(not) reset by CalculateSize and AutoSize mechanisms
      (in contrast to Translation, Rotation) because the idea is that user adjusts
      the auto-calculated size this way. }
    property SizeScale: Single read FSizeScale write SetSizeScale {$ifdef FPC}default 1.0{$endif};

    { Make various features (in particular automatic size calculation caused by @link(AutoSize))
      better adjusted to 2D models, where the user-visible shape is in XY
      and the Z coordinate (position in Z, thickness in Z) should have little or no effect on the result.

      More precisely, when this is @true:

      @orderedList(
        @item(@link(AutoSize) estimates collider 3D sizes (because internally Kraft colliders are always in 3D)
          based on geometry sizes only in XY dimensions.
          So it ignores how thin or thick is you geometry along the Z axis.)

        @item(@link(AutoSize) assumes a bit larger "minimum thickness" for 2D,
          because typical 2D worlds use larger units.
          Moreover, it assumes even larger thickness in Z, to avoid too thin boxes
          (that would not be detected as collidable).
          See @link(AutoSizeMinThickness), @link(AutoSizeMinThickness2D), @link(AutoSizeMinThickness2DDepth).)

        @item(Non-uniform scale (coming from accumulated TCastleTransform.Scale on all our parents)
          is handled differently. When Mode2D = @true, the non-uniform scale is converted
          into a scalar using Approximate2DScale, so it only looks at XY coordinates.
          When Mode2D = @false, the non-uniform scale is converted using Approximate3DScale,
          so it looks at all coordinates.)

        @item(In the future, esp. if we integrate with other physics engines like Box2D,
          it is possible that some colliders in 2D mode will have a shape much more suitable
          in 2D. "Suitable in 2D" ideally means "Z coordinate is ignored".
          This means that a 2D collider should (ideally) behave as if it is infinite in Z.

          For example a 2D sphere collider may effectively be an infinite cylinder.
          That is: it should be a circle in XY, but extruded into infinity in -Z and +Z.
          This is not @italic(right now) how the TCastleSphereCollider behaves with Mode2D = @true,
          but it may be in the future.
        )
      )
    }
    property Mode2D: Boolean read FMode2D write SetMode2D default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Push everything to be above the given static plane, like a floor.

    Half of 3D space (below the plane) is effectively collidable,
    and everything is pushed to be above the floor.
    Note: This is more than just "collide as an infinite plane", because
    even if you place a body completely below the plane, it will be pushed up.

    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies.

    This collider supports only static rigid bodies.
    The associated TCastleRigidBody instance (with the same TCastleTransform parent)
    will behave as if @link(TCastleRigidBody.Dynamic) and @link(TCastleRigidBody.Animated) are @false. }
  TCastlePlaneCollider = class(TCastleCollider)
  strict private
    FNormal: TVector3;
    FDistance: Single;
    FVisualization: TCastleTransform;
    procedure SetNormal(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
    function CalculateScaledDistance: Single;
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
    procedure UpdateVisualization; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InternalDesigningBegin; override;
    procedure InternalDesigningEnd; override;

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
    FVisualization: TCastleTransform;
    FSize: TVector3; //< this size doesn't include FSizeScale or FWorldScale
    procedure SetSize(const AValue: TVector3);
    { Returns scaled size, because scale is added to size directly. }
    function CalculateScaledSize: CastleVectors.TVector3;
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
    procedure UpdateVisualization; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure InternalDesigningBegin; override;
    procedure InternalDesigningEnd; override;

    { Box size.
      Default is (2,2,2) (if not AutoSize), consistent with TCastleBox. }
    property Size: TVector3 read FSize write SetSize;
  published
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
    FVisualization: TCastleTransform;
    procedure SetRadius(const AValue: Single);
    { Returns scaled Radius, because scale is added to size directly. }
    function CalculateScaledRadius: Single;
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
    procedure UpdateVisualization; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure InternalDesigningBegin; override;
    procedure InternalDesigningEnd; override;
  published
    { Sphere radius.
      Default is 1 (if not AutoSize), consistent with TCastleSphere. }
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;
  end;

  { Collide as a capsule.
    Add this collider to TCastleTransform behaviors, along with a TCastleRigidBody,
    to make the given TCastleTransform be affected by physics and collide with other physics bodies. }
  TCastleCapsuleCollider = class(TCastleCollider)
  strict private
    FRadius: Single;
    FHeight: Single;

    FVisualization: TCastleTransform;
    FVisualizationTop: TCastleTransform;
    FVisualizationBottom: TCastleTransform;
    FVisualizationCylinder: TCastleTransform;

    procedure SetRadius(const AValue: Single);
    procedure SetHeight(const AValue: Single);
    function CalculateScaledHeight: Single;
    function CalculateScaledRadius: Single;
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
    procedure CalculateSizeCore; override;
    procedure UpdateVisualization; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure InternalDesigningBegin; override;
    procedure InternalDesigningEnd; override;
  published
    { Sphere (at both top and bottom of capsule) radius.
      Default is 1 (if not AutoSize), consistent with TCastleSphere. }
    property Radius: Single read FRadius write SetRadius stored StoredWithAutoSize;

    { Sphere (at both top and bottom of capsule) radius.
      Default is 2 (if not AutoSize), consistent with TCastleCylinder. }
    property Height: Single read FHeight write SetHeight stored StoredWithAutoSize;
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
    procedure SetMesh(const AValue: TCastleTransform);
  protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure InternalTransformChanged(const TransformChanged: TCastleTransform); override;

    { Update the collider triangles to reflect current @link(Mesh) shape.
      This is necessary for physics engine to reflect the @link(Mesh).

      We do it automatically in some circumstances (like when you change
      collider properties, like @link(Mesh) or @link(DoubleSided),
      or when you change @link(TCastleSceneCore.Url)) but we don't try to keep
      the physics mesh follow the triangles every frame (as you really shouldn't
      change physics mesh so often, it's supposed to be a static collider). }
    procedure MeshChanged;
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
        stateReady //< ready to work
      );
    var
      { Current rigid body state. }
      FState: TState;
      { Castle collider behavior that was used to initialize rigid body or nil
        if physics engine collider was not initialized }
      FCastleCollider: TCastleCollider;

      FDuringSettingTransformationFromPhysicsEngine: Boolean;

      FLayer: TPhysicsLayer;

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

    { Updates Kraft rigid body (FKraftBody) type krbtDynamic, krbtKinematic, krbtStatic
      based on Dynamic and Animated properties.

      Note that some colliders force the body type to krbtStatic if they only support static colliders
      by doing "ARigidBody.RigidBodyType := krbtStatic" in CreateKraftShape,
      which is after this call. }
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
    FMaxAngularVelocity: Single;
    FLinearVelocity: TVector3;
    FLinearVelocityDamp: Single;
    FMaxLinearVelocity: Single;

    FCollisionList: TCastleTransformList;

    { Transform for which we initialized the physics objects or nil }
    FTransform: TCastleTransform;

    { Get appropriate TKraftShape (from the deprecated TCollider or the new TCastleCollider).
      This may be removed after removing deprecated TCollider, as it will be a bit pointless then. }
    function GetKraftShape: TKraftShape;

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
    { Updates the collision layer on which the body is located }
    procedure UpdateLayer;

    { Sets all values from Kraft to CGE private fields. Currently angular and
      linear velocity. }
    procedure SynchronizeFromKraft;

    { Calls FKraftBody.Finish, if the body is already initialized (in stateReady).
      This is required after changing various properties in Kraft body/collider. }
    procedure KraftForceUpdatingReadyBody;

    procedure SetWorldTransformation(const WorldTransform: TMatrix4);
    procedure SetLockTranslation(const AValue: T3DCoords);
    procedure SetLockRotation(const AValue: T3DCoords);

    procedure SetAngularVelocity(const AVelocity: TVector3);
    procedure SetAngularVelocityDamp(const AValue: Single);
    procedure SetMaxAngularVelocity(const AValue: Single);

    procedure SetLinearVelocity(const LVelocity: TVector3);
    procedure SetLinearVelocityDamp(const AValue: Single);
    procedure SetMaxLinearVelocity(const AValue: Single);

    procedure SetOnCollisionEnter(const AValue: TCollisionEvent);
    procedure SetOnCollisionStay(const AValue: TCollisionEvent);
    procedure SetOnCollisionExit(const AValue: TCollisionEvent);

    procedure SetCollisionDetection(const AValue: TCollisionDetection);

    procedure SetLayer(const AValue: TPhysicsLayer);

    procedure SetDynamic(const AValue: Boolean);
    procedure SetAnimated(const AValue: Boolean);
    procedure SetGravity(const AValue: Boolean);

    procedure SetExists(const Value: Boolean);
    procedure SetTrigger(const Value: Boolean);

    { Creates physics engine object and connects the rigid body with
      @link(TCastleTransform Parent) and @link(TCastleTransform Parent).World.

      Initialization is done in three steps:
      1. Creation of Kraft rigid body in InitializeEngineRigidBody
      2. Creation of Kraft shape based on TCollider (deprecated) or TCastleCollider
      3. Finishing initialization when creation of Kraft shape succeeded }
    procedure InitializePhysicsEngineObjects;

    { Frees physics objects in three steps:
      1. Deinitialize colliders
      2. Removes self from other rigid bodies collision lists
      3. Frees the physics engine rigid body object }
    procedure DeinitializePhysicsEngineObjects;

    { Initialize rigid body objects from physics engine }
    procedure InitializeEngineRigidBody;

    { Initialize collider from physics engine.
      If some collider is set, this moves state to stateReady or raises an exception.
      If no collider is set, this silently exits without updating state. }
    procedure InitializeCollider;

    { Used by TCastleCollider when it's parent changes to handle case when
      rigid body is added before collider behavior }
    procedure CheckInitialization(const ACastleCollider: TCastleCollider);

    { Deinitializes physics engine collider object. }
    procedure DeinitializeCollider;

    { Called by TCastleCollider when it's shape changes }
    procedure ReinitializeCastleCollider;

    { Configure Sender (TCastleRigidBody) using Setup2D. }
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

      Returns the TPhysicsRayCastResult record.
      Read TPhysicsRayCastResult.Hit (boolean), TPhysicsRayCastResult.Transform
      (guaranteed to be @nil when TPhysicsRayCastResult.Hit is @false) and other fields
      to know what was hit.

      The given RayOrigin, RayDirection, MaxDistance are in the parent
      coordinate system of associated TCastleTransform (@link(TCastleBehavior.Parent)).
      So for example query like this works naturally:

      @longCode(#
        RBody := MyTransform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
        Hit := RBody.RayCast(MyTransform.Translation, MyTransform.Direction, MaxDistance);
      #)

      It returns TPhysicsRayCastResult, with information there
      consistently in the same coordinate system as inputs.
      So TPhysicsRayCastResult.Distance, TPhysicsRayCastResult.Point, TPhysicsRayCastResult.Normal
      are in parent coordinate system of associated TCastleTransform (@link(TCastleBehavior.Parent)).

      This ignores the collider of this rigid body (to not accidentally collide
      with your own collider), and checks collisions with the rest of the world in
      given max distance.
      Only collisions with the physics colliders (TCastleCollider and TCollider)
      are considered.

      Note: The @link(TCastleTransform.Pickable) property is ignored by this method,
      i.e. it considers all colliders regardless of their @link(TCastleTransform.Pickable) value.
      The @link(TCastleTransform.Pickable) is only taken into account by the routines
      of the old physics system
      (see https://castle-engine.io/physics#_old_system_for_collisions_and_gravity ).
      In the new system, a similar role will be filled by layers. }
    function PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
      const MaxDistance: Single = MaxSingle): TPhysicsRayCastResult;

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
      That is, adding force means @code(Velocity += Force * SecondsPassed / Mass).

      By changing Position from which you push,
      you can change the angular velocity,
      i.e. different Position makes the body spin in different ways.
      Note that Position too distant from the body center may result in unrealistically strong push.
      If Position is exactly at body center, then angular velocity will not be affected.

      Force and Position are in the world coordinate system. }
    procedure AddForceAtPosition(const Force, Position: TVector3);

    { Push this rigid body.
      The body is pushed along the direction of Force.
      Longer length of Force makes a stronger push.

      This is similar to @link(AddForceAtPosition), but the Force (direction)
      can be in local or global coordinate system
      (depending on ForceInLocalCoordinates).
      Moreover it doesn't take as parameter any Position:
      the force always acts from the origin of this body.

      Just like with @link(AddForceAtPosition),
      the forces affecting each body are reset every frame
      (only the body velocity is preserved across frames).
      If you want to keep applying this force (e.g. it is a wind that continues pushing in given direction)
      you should call this method every frame.
      It automatically accounts for the delta time (because the force will actually
      be applied later, with delta time, to velocity) so no need to multiply the arguments with SecondsPassed. }
    procedure AddForce(const Force: TVector3; const ForceInLocalCoordinates: Boolean);

    { Rotate this rigid body with physics.
      Longer length of Torque makes a stronger push.
      Torque is in world coordinates.

      Just like with @link(AddForceAtPosition),
      the forces affecting each body are reset every frame
      (only the body velocity is preserved across frames).
      If you want to keep applying this force (e.g. it is a whirlwind that continues rotating)
      you should call this method every frame.
      It automatically accounts for the delta time (because the force will actually
      be applied later, with delta time, to velocity) so no need to multiply the arguments with SecondsPassed. }
    procedure AddTorque(const Torque: TVector3);

    { Apply a one-time instant increase in speed, that pushes the body (increasing velocity).
      Impulse is a direction along which to push, Point is the position from which you push.
      Longer length of Impulse makes a stronger push.

      Impulse and Point are both in world coordinates.

      Adding impulse means @code(Velocity += Force / Mass).
      To be more precise, adding impulse directly increases @link(LinearVelocity) and @link(AngularVelocity).
      Unlike @link(AddForceAtPosition), the velocity increase is not multiplied by the delta of time,
      so it is not expected you will call this every frame.
      (If you do, you should take care to multiply Impulse by SecondsPassed.)

      Same notes as for Position of @link(AddForceAtPosition):
      By changing Point from which you push,
      you can change the angular velocity,
      i.e. different Point makes the body spin in different ways.
      Note that Point too distant from the body center may result in unrealistically strong push.
      If Point is exactly at body center, then angular velocity will not be affected. }
    procedure ApplyImpulse(const Impulse, Point: TVector3);

    { Returns true when rigid body behavior exists, and is added to Castle
      Transform, and this transform ExistsInRoot = true. }
    function ExistsInRoot: Boolean;

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

      When @false the object is not transformed by the physics simulation.
      It still collides with other physical objects.
      So it's a static collidable body that will not move on any collision / force. }
    property Dynamic: Boolean read FDynamic write SetDynamic default true;

    { Is the transformation of this object updated often
      (relevant only when @link(Dynamic) = @false).

      This property is taken into account only if @link(Dynamic) = @false.

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
          @italic(so do not change the transformation often, e.g. every frame).

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
      specifying this @link(TRigidBody) as it's parent.

      This is deprecated, using deprecated TCollider class.

      @deprecated }
    property Collider: TCollider read FCollider;

    { Sets CollisionDetectionMode in rigid body to Discrete or ContinuousSweep. }
    property CollisionDetection: TCollisionDetection read FCollisionDetection
      write SetCollisionDetection default cdDiscrete;

    { Physics layer of the rigid body determines with which other bodies it may collide.

      The way layers interact is configured in the
      @link(TPhysicsProperties.LayerCollisions MyViewport.Items.PhysicsProperties.LayerCollisions).

      Each layer can also have name and description. You can set
      them using the
      @link(TPhysicsProperties.LayerNames MyViewport.Items.PhysicsProperties.LayerNames)
      property. }
    property Layer: TPhysicsLayer read FLayer write SetLayer default 0;

    { Is this object affected by gravity. }
    property Gravity: Boolean read FGravity write SetGravity default true;

    { Disable motion (@link(TCastleTransform.Translation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable motion along the Z axis.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockTranslation: T3DCoords read FLockTranslation write SetLockTranslation default [];

    { Disable rotation (@link(TCastleTransform.Rotation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable rotation along the X and Y axes.
      Instead of directly changing this property,
      you can achieve this by calling @link(Setup2D). }
    property LockRotation: T3DCoords read FLockRotation write SetLockRotation default [];

    { Controls whether the rigid body is actually processed by the physics engine.
      When this is @false, the rigid body is not updated by the physics engine,
      and it does not cause collisions with other rigid bodies. }
    property Exists: Boolean read FExists write SetExists default true;

    { Damping means that the velocity will decrease over time.
      AngularVelocityDamp causes AngularVelocity to decrease over time.
      For the exact equations how it works, see @link(LinearVelocityDamp). }
    property AngularVelocityDamp: Single read FAngularVelocityDamp write SetAngularVelocityDamp {$ifdef FPC}default 0.1{$endif};
    property MaxAngularVelocity: Single read FMaxAngularVelocity write SetMaxAngularVelocity;

    { Damping means that the velocity will decrease over time.
      LinearVelocityDamp causes LinearVelocity to decrease over time,
      similar AngularVelocityDamp causes AngularVelocity to decrease over time.

      Values from 0.0 up to infinity make sense.

      Exact interpretation:

      The damping is applied every "physics frame" following this equation:

      @code(LinearVelocity := LinearVelocity * 1 / (1 + LinearVelocityDamp * 1/PhysicsProperties.Frequency))

      Where @code(PhysicsProperties.Frequency) is by default 60,
      and can be customized for each viewport using @link(TPhysicsProperties.Frequency
      Viewport.Items.PhysicsProperties.Frequency). The frequency also determines how
      often do "physics frames" occur.

      In effect, damping = 0 means that no damping occurs.

      Damping values > 0 means that LinearVelocity decreases with time,
      larger LinearVelocityDamp -> the faster LinearVelocity will decrease.

      The meaning of damping = 1.0 depends on @link(TPhysicsProperties.Frequency
      Viewport.Items.PhysicsProperties.Frequency):

      @unorderedList(
        @item(With TPhysicsProperties.Frequency = 60 (default), the XxxVelocityDamp = 1.0 implies that
          the velocity drops to ~0.37 of original after 1 second.)

        @item(With TPhysicsProperties.Frequency = 1
          (crazy small, just giving it as example),
          the XxxVelocityDamp = 1.0 implies that the velocity drops to half of original after 1 second.)

        @item(With TPhysicsProperties.Frequency = 10000
          (crazy large, just giving it as example),
          the XxxVelocityDamp = 1.0 implies that the velocity drops to ~0.36 of original after 1 second.)
      )
    }
    property LinearVelocityDamp: Single read FLinearVelocityDamp write SetLinearVelocityDamp {$ifdef FPC}default 0.1{$endif};
    property MaxLinearVelocity: Single read FMaxLinearVelocity write SetMaxLinearVelocity;

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

procedure ColliderVisualizationConfigBase(const Transform: TCastleTransform);
begin
  Transform.SetTransient;

  Transform.Collides := false;
  Transform.Pickable := false;
  Transform.CastShadows := false;
  Transform.InternalExcludeFromParentBoundingVolume := true;;
end;

procedure ColliderVisualizationConfig(const Primitive: TCastleAbstractPrimitive);
begin
  ColliderVisualizationConfigBase(Primitive);
  Primitive.Color := Vector4(HexToColorRGB('FC9C2D'), 0.8);
  Primitive.Material := pmUnlit;
  Primitive.RenderOptions.LineWidth := 3;
  Primitive.RenderOptions.WireframeColor := HexToColorRGB('FC9C2D');
  Primitive.RenderOptions.WireframeEffect := weWireframeOnly;
end;

{ TCastleLayerCollisions ----------------------------------------------------- }

function TCastleLayerCollisions.GetCollides(const Layer1, Layer2: TPhysicsLayer): Boolean;
begin
  Result := Layer2 in FCollides[Layer1];
  Assert(Result = (Layer1 in FCollides[Layer2]));
end;

procedure TCastleLayerCollisions.SetCollides(const Layer1,
  Layer2: TPhysicsLayer; const AValue: Boolean);
begin
  if AValue then
  begin
    Include(FCollides[Layer1], Layer2);
    Include(FCollides[Layer2], Layer1);
  end else
  begin
    Exclude(FCollides[Layer1], Layer2);
    Exclude(FCollides[Layer2], Layer1);
  end;
  UpdateRigidBodies;
end;

procedure TCastleLayerCollisions.UpdateRigidBodies;
var
  RigidBodyList: TCastleBehaviorList;
  RootTransform: TCastleAbstractRootTransform;
  Behavior: TCastleBehavior;
begin
  if not (Owner is TPhysicsProperties) then
    Exit;

  if Owner.Owner is TCastleAbstractRootTransform then
  begin
    RootTransform := Owner.Owner as TCastleAbstractRootTransform;
    RigidBodyList := RootTransform.FindAllBehaviors(TCastleRigidBody);
    try
      for Behavior in RigidBodyList do
        TCastleRigidBody(Behavior).UpdateLayer;
    finally
      FreeAndNil(RigidBodyList);
    end;
  end;
end;

constructor TCastleLayerCollisions.Create(AOwner: TComponent);
var
  L: TPhysicsLayer;
begin
  inherited Create(AOwner);

  // assign default state for FCollides
  for L := Low(L) to High(L) do
    FCollides[L] := DefaultCollides(L);
  UpdateRigidBodies;
end;

function TCastleLayerCollisions.DefaultCollides(const Layer: TPhysicsLayer): TPhysicsLayers;
begin
  // By default each layer collides with itself
  Result := [Layer];
end;

procedure TCastleLayerCollisions.CustomSerialization(
  const SerializationProcess: TSerializationProcess);

  procedure FixBirectionalCollisions(var NeedsUpdate: Boolean);
  var
    L1, L2: TPhysicsLayer;
  begin
    for L1 := Low(TPhysicsLayer) to High(TPhysicsLayer) do
      for L2 := Low(TPhysicsLayer) to High(TPhysicsLayer) do
        if ((L2 in FCollides[L1]) <> (L1 in FCollides[L2])) then
        begin
          // if there's a mismatch, assume the right state is "collides"
          Include(FCollides[L1], L2);
          Include(FCollides[L2], L1);
          NeedsUpdate := true;
        end;
  end;

var
  I: TPhysicsLayer;
  OldCollidesWithInt, NewCollidesWithInt: Int32;
  NeedsUpdate: Boolean;
begin
  inherited;

  Assert(SizeOf(Int32) = SizeOf(TPhysicsLayers));

  NeedsUpdate := false;
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    Move(FCollides[I], OldCollidesWithInt, SizeOf(Int32));
    NewCollidesWithInt := OldCollidesWithInt;

    SerializationProcess.ReadWriteInteger('Collides' + IntToStr(I),
      NewCollidesWithInt, FCollides[I] <> DefaultCollides(I));

    if OldCollidesWithInt <> NewCollidesWithInt then
    begin
      NeedsUpdate := true;
      Move(NewCollidesWithInt, FCollides[I], SizeOf(Int32));
    end;
  end;

  FixBirectionalCollisions(NeedsUpdate);

  if NeedsUpdate then
    UpdateRigidBodies;
end;

{ TCastleLayerNames --------------------------------------------------------- }

function TCastleLayerNames.GetLayerNames(const Layer: TPhysicsLayer): String;
begin
  Result := FLayerNames[Layer];
end;

procedure TCastleLayerNames.SetLayerNames(const Layer: TPhysicsLayer;
  const AValue: String);
begin
  FLayerNames[Layer] := AValue;
end;

function TCastleLayerNames.GetLayerDescriptions(
  const Layer: TPhysicsLayer): String;
begin
  Result := FLayerDescriptions[Layer];
end;

procedure TCastleLayerNames.SetLayerDescriptions(const Layer: TPhysicsLayer;
  const AValue: String);
begin
  FLayerDescriptions[Layer] := AValue;
end;

procedure TCastleLayerNames.CustomSerialization(
  const SerializationProcess: TSerializationProcess);
var
  I: TPhysicsLayer;
  Value: String;
begin
  inherited;

  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
  begin
    Value := FLayerNames[I];
    SerializationProcess.ReadWriteString('Name' + IntToStr(I), Value, Value <> '');
    FLayerNames[I] := Value;

    Value := FLayerDescriptions[I];
    SerializationProcess.ReadWriteString('Description' + IntToStr(I), Value, Value <> '');
    FLayerDescriptions[I] := Value;
  end;
end;

{ TPhysicsCollisionDetails --------------------------------------------------- }

function TPhysicsCollisionDetails.OtherTransform: TCastleTransform;
begin
  Result := Transforms[1];
end;

{ TPhysicsProperties --------------------------------------------------------- }

constructor TPhysicsProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngularVelocityRK4Integration := DefaultAngularVelocityRK4Integration;
  FLinearVelocityRK4Integration := DefaultLinearVelocityRK4Integration;
  FFrequency := DefaultFrequency;
  FGravityStrength := DefaultGravityStrength;
  FPhysicsTimeStep := 1.0 / FFrequency;
  FMaxPhysicsTicksPerUpdate := DefaultMaxPhysicsTicksPerUpdate;
  FContinuousCollisionDetection := DefaultContinuousCollisionDetection;
  FLayerNames := TCastleLayerNames.Create(Self);
  FLayerNames.Name := 'LayerNames';
  FLayerNames.SetSubComponent(true);
  FLayerCollisions := TCastleLayerCollisions.Create(Self);
  FLayerCollisions.Name := 'LayerCollisions';
  FLayerCollisions.SetSubComponent(true);
end;

destructor TPhysicsProperties.Destroy;
begin
  FreeAndNil(FLayerNames);
  FreeAndNil(FLayerCollisions);
  inherited Destroy;
end;

procedure TPhysicsProperties.SetAngularVelocityRK4Integration(const AValue: Boolean);
begin
  if FAngularVelocityRK4Integration <> AValue then
  begin
    FAngularVelocityRK4Integration := AValue;
    if Assigned(RootTransform.FKraftEngine) then
      RootTransform.FKraftEngine.AngularVelocityRK4Integration := AValue;
  end;
end;

procedure TPhysicsProperties.SetFrequency(const AValue: Single);
begin
  if FFrequency <> AValue then
  begin
    FFrequency := AValue;
    FPhysicsTimeStep := 1.0 / FFrequency;

    if Assigned(RootTransform.FKraftEngine) then
      RootTransform.FKraftEngine.SetFrequency(AValue);
  end;
end;

procedure TPhysicsProperties.SetContinuousCollisionDetection(
  const AValue: Boolean);
begin
  if FContinuousCollisionDetection <> AValue then
  begin
    FContinuousCollisionDetection := AValue;
    if Assigned(RootTransform.FKraftEngine) then
      if AValue then
        RootTransform.FKraftEngine.ContinuousMode := kcmTimeOfImpactSubSteps
      else
        RootTransform.FKraftEngine.ContinuousMode := kcmNone;
  end;
end;

{
procedure TPhysicsProperties.SetLayerNames(const AValue: TStrings);
begin
  FLayerNames.Assign(AValue);
end;
}

procedure TPhysicsProperties.SetLinearVelocityRK4Integration(const AValue: Boolean);
begin
  if FLinearVelocityRK4Integration <> AValue then
  begin
    FLinearVelocityRK4Integration := AValue;
    if Assigned(RootTransform.FKraftEngine) then
      RootTransform.FKraftEngine.LinearVelocityRK4Integration := AValue;
  end;
end;

{ TCastleCollider ------------------------------------------------------------ }

constructor TCastleCollider.Create(AOwner: TComponent);
begin
  inherited;
  // properties defaults
  FDensity := DefaultDensity;
  FMass := DefaultMass;
  FRestitution := DefaultRestitution;
  FFriction := DefaultFriction;
  FSizeScale := 1.0;
  FWorldScale := NoScale;
  FTranslation := TVector3.Zero;
  FRotation := TVector4.Zero;

  { Will be updated in TCastleCollider.InitializeKraft to reflect FKraftShape.LocalTransform }
  FUseLocalTransform := false;
  FLocalTransform := TMatrix4.Identity;
  FLocalInverseTransform := TMatrix4.Identity;
  FAutoSize := true;

  ListenWorldChange := true;

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

procedure TCastleCollider.UpdateVisualization;
begin
  if FVisualizationParent <> nil then
  begin
    { We need to multiply Translation * TotalScale,
      testcases:
      - "Show Colliders" on examples/physics/physics_test_scale_colliders/
      - "Show Colliders" on TCastleBoxCollider with Size = (0.05 0.05 0.02)
        and Translation = (0.00 0.02 0.00) }
    FVisualizationParent.Translation := Translation * TotalScale;
    FVisualizationParent.Rotation := Rotation;
  end;
end;

procedure TCastleCollider.InternalDesigningBegin;
begin
  inherited;
  if FVisualizationParent = nil then
  begin
    FVisualizationParent := TCastleTransform.Create(nil);
    FVisualizationParent.InternalIgnoreParentScale := true;
    ColliderVisualizationConfigBase(FVisualizationParent);
    Parent.Add(FVisualizationParent);
    // descendants will call UpdateVisualization
  end;
end;

procedure TCastleCollider.InternalDesigningEnd;
begin
  FreeAndNil(FVisualizationParent);
  inherited;
end;

procedure TCastleCollider.ParentBeforeDetach;
begin
  InternalDesigningEnd;
  inherited;
end;

procedure TCastleCollider.UpdateLocalTransform;
var
  T: TTransformation;
  RBody: TCastleRigidBody;
begin
  { Initialize FLocalTransform, FLocalInverseTransform and pass transform to FKraftShape }
  T.Init;

  { TCastleCollider.Translation below is multiplied by WorldScaleScalar
    and not by TotalScale. Because:
    - Conceptually, all distances are scaled by
      WorldScaleScalar, including TCastleCollider.Translation.
      E.g. TCastleCollider.Translation = (20,0,0)
      under TCastleTransform with Scale = 10
      actually moves by 10.
    - But TCastleCollider.SizeScale is more local, it doesn't affect distance
      by TCastleCollider.Translation.
    - The end result is that TCastleCollider.Translation / Rotation / SizeScale
      should behave analogous to local TCastleTransform transformation,
      and be affected by parent TCastleTransform transformations in the same way.
    - Testcase: physics_test_scale_colliders
  }

  T.Multiply(Rotation, NoScale, Translation * WorldScaleScalar);
  FLocalTransform := T.Transform;
  FLocalInverseTransform := T.InverseTransform;
  if FKraftShape <> nil then
  begin
    Assert(Parent <> nil);
    RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    Assert(RBody <> nil);

    FKraftShape.LocalTransform := MatrixToKraft(FLocalTransform);
    RBody.FKraftBody.SynchronizeTransformIncludingShapes;
    FKraftShape.StoreWorldTransform;
  end;

  { After setting the appropriate transformation, we can optimize the matrix
    multiplication when rotation = 0, translation = 0,0,0 .
    TotalScale (FWorldScale, FSizeScale) is not checked here because it must be
    1,1,1 in Kraft and it's added later at the creation of collider shape. }
  FUseLocalTransform := not (
    FTranslation.IsPerfectlyZero and
    (FRotation.W = 0)
  );
  UpdateVisualization;
end;

procedure TCastleCollider.InitializeKraft(const APhysics: TKraft;
  const ACastleRigidBody: TCastleRigidBody);
begin
  // when using old Collider and new one
  if ACastleRigidBody.Collider <> nil then
    raise Exception.Create('Trying to use old TCollider and TCastleCollider');

  Assert(ACastleRigidBody.FKraftBody <> nil,
    'Trying initialize kraft shape when rigid body not initialized!');

  ACastleRigidBody.DeinitializeCollider;
  { If shape count is > 0 we need delete previous shape. }
  {if ACastleRigidBody.FKraftBody.ShapeCount > 0 then
  begin
    Assert(ACastleRigidBody.FKraftBody.ShapeCount = 1, 'More than one shape in rigid body.');
    Assert(ACastleRigidBody.FKraftBody.ShapeFirst <> FKraftShape, 'Mixed kraft shapes');
    FreeAndNil(FKraftShape);
  end;}

  FKraftShape := CreateKraftShape(APhysics, ACastleRigidBody.FKraftBody);
  Assert(FKraftShape <> nil);

  // assign common TCollider properties to Kraft shape
  FKraftShape.Density := Density;
  FKraftShape.Restitution := Restitution;
  FKraftShape.Friction := Friction;
  if not ACastleRigidBody.ExistsInRoot then
    FKraftShape.Flags := FKraftShape.Flags - [ksfCollision, ksfRayCastable];

  UpdateLocalTransform;
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

      // Similar as in SetMass, we need to call KraftForceUpdatingReadyBody
      RigidBody := GetRigidBody;
      if RigidBody <> nil then
       RigidBody.KraftForceUpdatingReadyBody;
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

      { Without KraftForceUpdatingReadyBody (FKraftBody.Finish),
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
      RigidBody.KraftForceUpdatingReadyBody;
    end;
  end;
end;

procedure TCastleCollider.SetRestitution(const AValue: Single);
begin
  if FRestitution <> AValue then
  begin
    FRestitution := AValue;
    if FKraftShape <> nil then
      FKraftShape.Restitution := FRestitution;
  end;
end;

procedure TCastleCollider.SetFriction(const AValue: Single);
begin
  if FFriction <> AValue then
  begin
    FFriction := AValue;
    if FKraftShape <> nil then
      FKraftShape.Friction := FFriction;
  end;
end;

procedure TCastleCollider.SetTranslation(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(AValue, FTranslation) then
  begin
    FTranslation := AValue;
    DisableAutoSize;
    UpdateLocalTransform;
  end;
end;

procedure TCastleCollider.SetRotation(const AValue: TVector4);
begin
  if not TVector4.PerfectlyEquals(AValue, FRotation) then
  begin
    FRotation := AValue;
    DisableAutoSize;
    UpdateLocalTransform;
    UpdateVisualization;
  end;
end;

procedure TCastleCollider.SetSizeScale(const AValue: Single);
begin
  if FSizeScale <> AValue then
  begin
    FSizeScale := AValue;
    // FSizeScale is used by collider at the shape creation
    ShapeChangedNotifyRigidBody;
    UpdateVisualization;
  end;
end;

procedure TCastleCollider.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize <> AValue then
  begin
    FAutoSize := AValue;
    InternalAutoSize;
  end;
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

  UpdateWorldScale;

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
    CastleRBody.DeinitializeCollider;
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
end;

procedure TCastleCollider.CalculateSize;
begin
  FCalculatingSize := true;
  try
    CalculateSizeCore;
  finally FCalculatingSize := false end;
end;

function TCastleCollider.WorldScaleScalar: Single;
begin
  if Mode2D then
  begin
    if (not WarningNonUniformScaleDone) and
       (not SameValue(FWorldScale.X, FWorldScale.Y)) then
    begin
      WritelnWarning('Using non-uniform 2D scale %s for physics body (collider %s) is not fully supported', [
        FWorldScale.XY.ToString,
        Name
      ]);
      WarningNonUniformScaleDone := true;
    end;
    Result := Approximate2DScale(FWorldScale.XY)
  end else
  begin
    if (not WarningNonUniformScaleDone) and
       ( (not SameValue(FWorldScale.X, FWorldScale.Y)) or
         (not SameValue(FWorldScale.X, FWorldScale.Z))
       ) then
    begin
      WritelnWarning('Using non-uniform scale %s for physics body (collider %s) is not fully supported', [
        FWorldScale.ToString,
        Name
      ]);
      WarningNonUniformScaleDone := true;
    end;
    Result := Approximate3DScale(FWorldScale);
  end;
end;

function TCastleCollider.TotalScale: Single;
begin
  Result := WorldScaleScalar * FSizeScale;
end;

procedure TCastleCollider.UpdateWorldScale;
var
  NewScale: TVector3;
begin
  if Parent.HasWorldTransform then
    NewScale := ScaleFromMatrix(Parent.WorldTransform)
  else
    NewScale := Parent.Scale;

  { Never use a negative scale for colliders }
  NewScale := NewScale.Abs;

  if not TVector3.PerfectlyEquals(FWorldScale, NewScale) then
  begin
    FWorldScale := NewScale;
    { FWorldScale is used in CreateCraftShape so we need to notify
      RigidBody to recreate it. }
    ShapeChangedNotifyRigidBody;
  end;
end;

function TCastleCollider.StoredWithAutoSize: Boolean;
begin
  Result := not AutoSize;
end;

function TCastleCollider.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'Density', 'Friction', 'Mass', 'Restitution', 'SizeScale', 'LayerCollisions',
     'LayerNames', 'RotationPersistent', 'TranslationPersistent'
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

procedure TCastleCollider.InternalTransformChanged(const TransformChanged: TCastleTransform);
begin
  Assert(TransformChanged <> nil);
  if TransformChanged = Parent then
    InternalAutoSize;
end;

procedure TCastleCollider.CustomSerialization(const SerializationProcess: TSerializationProcess);

  procedure UpgradeOldScale(const OldScaleVectorName: String);
  var
    OldScale: TVector3;
    TempMode2D: Boolean;
  begin
    OldScale := NoScale;
    SerializationProcess.ReadWriteVector(OldScaleVectorName, OldScale, NoScale, false);
    if not TVector3.PerfectlyEquals(OldScale, NoScale) then
    begin
      // read Mode2D, to work independently of whether it was initialized by normal deserialization already
      SerializationProcess.ReadWriteBoolean('Mode2D', TempMode2D, false);
      WritelnWarning('Design refers to TCastleCollider.%s which is renamed now to SizeScale (and of Single type). Save the design to upgrade it.', [
        OldScaleVectorName
      ]);
      if TempMode2D then
        SizeScale := Approximate2DScale(OldScale.XY)
      else
        SizeScale := Approximate3DScale(OldScale);
    end;
  end;

begin
  inherited;
  { Upgrade old ideas (they only existed on physics branch, so this upgrade code
    can be really removed from master branch at some point. }
  UpgradeOldScale('ScalePersistent');
  UpgradeOldScale('SizeScalePersistent');
end;

procedure TCastleCollider.SetMode2D(const AValue: Boolean);
begin
  if FMode2D <> AValue then
  begin
    FMode2D := AValue;
    InternalAutoSize;
    // even when AutoSize=false, changing Mode2D changes how WorldScale is interpreted
    ShapeChangedNotifyRigidBody;
  end;
end;

class procedure TCastleCollider.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleCollider).Mode2D := true;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastlePlaneCollider ------------------------------------------------------- }

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

procedure TCastlePlaneCollider.SetNormal(const AValue: TVector3);
begin
  if not FNormal.Equals(FNormal, AValue) then
  begin
    FNormal := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
    UpdateVisualization;
  end;
end;

procedure TCastlePlaneCollider.SetDistance(const AValue: Single);
begin
  if FDistance <> AValue then
  begin
    FDistance := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
    UpdateVisualization;
  end;
end;

function TCastlePlaneCollider.CalculateScaledDistance: Single;
begin
  Result := Distance * TotalScale;
end;

procedure TCastlePlaneCollider.UpdateVisualization;
begin
  inherited;
  if FVisualization <> nil then
  begin
    TCastlePlane(FVisualization).Size := Vector2(2, 2);
    FVisualization.Translation := CastleVectors.Vector3(0, CalculateScaledDistance, 0);
    FVisualization.Rotation := Vector4(CastleVectors.Vector3(0, 0, -1), // todo check why -1
      AngleRadBetweenVectors(CastleVectors.Vector3(0, 1, 0), FNormal.Normalize));
  end;
end;

function TCastlePlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  // TCastlePlaneCollider is available only for static rigid bodies
  // TODO: We force krbtStatic now. Can we allow krbtKinematic too?
  ARigidBody.RigidBodyType := krbtStatic;

  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(FNormal)),
    CalculateScaledDistance));
end;

procedure TCastlePlaneCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  NormalCoord: T3DCoord;
begin
  inherited; // resets Translation, Rotation

  { Defaults, in case we don't have any better value }
  Normal := TVector3.One[1];
  Distance := 0;

  if Parent = nil then
    Exit;
  LocalBoundingBox := Parent.LocalBoundingBox;
  if LocalBoundingBox.IsEmptyOrZero then
    Exit;

  NormalCoord := MinAbsVectorCoord(LocalBoundingBox.Size);
  Normal := TVector3.One[NormalCoord];
  Distance :=
    LocalBoundingBox.Max.AsArray[NormalCoord] -
    LocalBoundingBox.Min.AsArray[NormalCoord];
end;

procedure TCastlePlaneCollider.InternalDesigningBegin;
begin
  inherited;
  if FVisualization = nil then
  begin
    FVisualization := TCastlePlane.Create(nil);
    ColliderVisualizationConfig(FVisualization as TCastleAbstractPrimitive);
    UpdateVisualization;
    FVisualizationParent.Add(FVisualization);
  end;
end;

procedure TCastlePlaneCollider.InternalDesigningEnd;
begin
  FreeAndNil(FVisualization);
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

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleplanecollider_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleBoxCollider --------------------------------------------------------- }

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

procedure TCastleBoxCollider.SetSize(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FSize, AValue) then
  begin
    FSize := AValue;
    DisableAutoSize;
    ShapeChangedNotifyRigidBody;
    UpdateVisualization;
  end;
end;

function TCastleBoxCollider.CalculateScaledSize: TVector3;
begin
  Result := FSize * TotalScale;
end;

function TCastleBoxCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
var
  ScaledSize: TVector3;
begin
  ScaledSize := CalculateScaledSize;

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
  MinThickness: TVector3;
begin
  inherited; // resets Translation, Rotation

  if Mode2D then
    MinThickness := CastleVectors.Vector3(AutoSizeMinThickness2D, AutoSizeMinThickness2D, AutoSizeMinThickness2DDepth)
  else
    MinThickness := CastleVectors.Vector3(AutoSizeMinThickness, AutoSizeMinThickness, AutoSizeMinThickness);

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Size := MinThickness;
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;
  TempSize := LocalBoundingBox.Size;

  if TempSize.X < MinThickness.X then
    TempSize.X := MinThickness.X;

  if TempSize.Y < MinThickness.Y then
    TempSize.Y := MinThickness.Y;

  if TempSize.Z < MinThickness.Z then
    TempSize.Z := MinThickness.Z;

  Size := TempSize;
  Translation := LocalBoundingBox.Center;
end;

procedure TCastleBoxCollider.UpdateVisualization;
begin
  inherited;
  if FVisualization <> nil then
  begin
    TCastleBox(FVisualization).Size := CalculateScaledSize;
  end;
end;

procedure TCastleBoxCollider.InternalDesigningBegin;
begin
  inherited;
  if FVisualization = nil then
  begin
    FVisualization := TCastleBox.Create(nil);
    ColliderVisualizationConfig(FVisualization as TCastleAbstractPrimitive);
    UpdateVisualization;
    FVisualizationParent.Add(FVisualization);
  end;
end;

procedure TCastleBoxCollider.InternalDesigningEnd;
begin
  FreeAndNil(FVisualization);
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
var
  BigChange: Boolean;
begin
  if FRadius <> AValue then
  begin
    BigChange := not SameValue(FRadius, AValue);
    FRadius := AValue;
    DisableAutoSize;
    if BigChange then // do not rebuild collider without real need, as it is costly
    begin
      ShapeChangedNotifyRigidBody;
      UpdateVisualization;
    end;
  end;
end;

function TCastleSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, CalculateScaledRadius);
end;

procedure TCastleSphereCollider.UpdateVisualization;
begin
  inherited;
  if FVisualization <> nil then
  begin
    TCastleSphere(FVisualization).Radius := CalculateScaledRadius;
  end;
end;

procedure TCastleSphereCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  MinThickness: Single;
begin
  inherited; // resets Translation, Rotation

  if Mode2D then
    MinThickness := AutoSizeMinThickness2D
  else
    MinThickness := AutoSizeMinThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinThickness;
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;
  if Mode2D then
    Radius := LocalBoundingBox.AverageSize2D(false, 2, 2) / 2
  else
    Radius := LocalBoundingBox.AverageSize(false, 2) / 2;

  { Check minimal size }
  Radius := Max(Radius, MinThickness);
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

procedure TCastleSphereCollider.InternalDesigningBegin;
begin
  inherited;
  if FVisualization = nil then
  begin
    FVisualization := TCastleSphere.Create(nil);
    ColliderVisualizationConfig(FVisualization as TCastleAbstractPrimitive);
    UpdateVisualization;
    FVisualizationParent.Add(FVisualization);
  end;
end;

procedure TCastleSphereCollider.InternalDesigningEnd;
begin
  FreeAndNil(FVisualization);
  inherited;
end;

function TCastleSphereCollider.CalculateScaledRadius: Single;
begin
  Result := FRadius * TotalScale;
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
var
  BigChange: Boolean;
begin
  if FRadius <> AValue then
  begin
    BigChange := not SameValue(FRadius, AValue);
    FRadius := AValue;
    DisableAutoSize;
    if BigChange then // do not rebuild collider without real need, as it is costly
    begin
      ShapeChangedNotifyRigidBody;
      UpdateVisualization;
    end;
  end;
end;

procedure TCastleCapsuleCollider.SetHeight(const AValue: Single);
var
  BigChange: Boolean;
begin
  if FHeight <> AValue then
  begin
    BigChange := not SameValue(FHeight, AValue);
    FHeight := AValue;
    DisableAutoSize;
    if BigChange then // do not rebuild collider without real need, as it is costly
    begin
      ShapeChangedNotifyRigidBody;
      UpdateVisualization;
    end;
  end;
end;

function TCastleCapsuleCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeCapsule.Create(APhysics, ARigidBody, CalculateScaledRadius,
    CalculateScaledHeight);
end;

procedure TCastleCapsuleCollider.UpdateVisualization;
var
  VRadius: Single;
  VHeight: Single;
begin
  inherited;
  if FVisualization <> nil then
  begin
    VRadius := CalculateScaledRadius;
    VHeight := CalculateScaledHeight;
    TCastleSphere(FVisualizationTop).Radius := VRadius;
    TCastleSphere(FVisualizationTop).Translation := CastleVectors.Vector3(0, VHeight / 2, 0);
    TCastleSphere(FVisualizationBottom).Radius := VRadius;
    TCastleSphere(FVisualizationBottom).Translation := CastleVectors.Vector3(0, - VHeight / 2, 0);
    TCastleCylinder(FVisualizationCylinder).Height := VHeight;
    TCastleCylinder(FVisualizationCylinder).Radius := VRadius;
  end;
end;

procedure TCastleCapsuleCollider.InternalDesigningBegin;
begin
  inherited;

  if FVisualization = nil then
  begin
    FVisualization := TCastleTransform.Create(nil);
    FVisualization.SetTransient;

    FVisualizationCylinder := TCastleCylinder.Create(FVisualization);
    ColliderVisualizationConfig(FVisualizationCylinder as TCastleAbstractPrimitive);
    FVisualization.Add(FVisualizationCylinder);

    FVisualizationTop := TCastleSphere.Create(FVisualization);
    ColliderVisualizationConfig(FVisualizationTop as TCastleAbstractPrimitive);
    FVisualization.Add(FVisualizationTop);

    FVisualizationBottom := TCastleSphere.Create(FVisualization);
    ColliderVisualizationConfig(FVisualizationBottom as TCastleAbstractPrimitive);
    FVisualization.Add(FVisualizationBottom);

    UpdateVisualization;
    FVisualizationParent.Add(FVisualization);
  end;
end;

procedure TCastleCapsuleCollider.InternalDesigningEnd;
begin
  FreeAndNil(FVisualization);
  inherited;
end;

function TCastleCapsuleCollider.CalculateScaledHeight: Single;
begin
  Result := FHeight * TotalScale;
end;

function TCastleCapsuleCollider.CalculateScaledRadius: Single;
begin
  Result := FRadius * TotalScale;
end;

procedure TCastleCapsuleCollider.CalculateSizeCore;
var
  LocalBoundingBox: TBox3D;
  MinThickness: Single;
begin
  inherited; // resets Translation, Rotation

  if Mode2D then
    MinThickness := AutoSizeMinThickness2D
  else
    MinThickness := AutoSizeMinThickness;

  if (Parent = nil) or (Parent.BoundingBox.IsEmptyOrZero) then
  begin
    Radius := MinThickness;
    Height := MinThickness;
    Exit;
  end;

  LocalBoundingBox := Parent.LocalBoundingBox;

  if Mode2D then
    Radius := LocalBoundingBox.SizeX * 0.50
  else
    Radius := ((LocalBoundingBox.SizeX * 0.50) + (LocalBoundingBox.SizeZ * 0.50)) / 2;

  { If radius * 2 is bigger than height make it smaller. }
  if Radius * 2 > LocalBoundingBox.SizeY then
    Radius := LocalBoundingBox.SizeY / 2 - 0.001;
  { Radius must be at least MinThickness }
  Radius := Max(Radius, MinThickness);

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
  { TODO: We force krbtStatic now. Can we allow krbtKinematic too? }
  ARigidBody.RigidBodyType := krbtStatic;

  FKraftMesh := TKraftMesh.Create(APhysics);

  if FMesh = nil then
  begin
    { Do not warn about it now, as it would be too eager during loading.
      Mesh may remain nil, and it may even remain nil at Loaded (when IsLoading stops
      being true) if it refers to some component defined later in design.
      Testcase: tests/data/designs/test_mesh_collider_ref.castle-user-interface .

      To really make a warning about it, we should have some method like
      CompleteDesignLoaded.
      But maybe we should just tolerate it, as valid state?

    WritelnWarning('TCastleMeshCollider "%s": Mesh not set, so it will not collide.', [
      Name
    ]);
    }
  end else
  if not FMesh.HasColliderMesh then
  begin
    WritelnWarning('TCastleMeshCollider "%s": Mesh "%s:%s" does not define any collidable triangles (HasColliderMesh is false). Use a different class for the TCastleMeshCollider.Mesh, like TCastleScene.', [
      Name,
      FMesh.Name,
      FMesh.ClassName
    ]);
  end else
  begin
    FMesh.ColliderMesh({$ifdef FPC}@{$endif} AddTriangle);
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

procedure TCastleMeshCollider.MeshChanged;
begin
  ShapeChangedNotifyRigidBody;
end;

procedure TCastleMeshCollider.InternalTransformChanged(const TransformChanged: TCastleTransform);
begin
  inherited;
  if TransformChanged = Mesh then
    MeshChanged;
end;

{ TCastleRigidBody ----------------------------------------------------------- }

constructor TCastleRigidBody.Create(AOwner: TComponent);
begin
  inherited;
  { We need WorldAfterAttach, WorldBeforeDetach }
  ListenWorldChange := true;

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
  FLayer := 0;

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
begin
  Assert(FKraftBody = nil, 'Kraft body is initialized!');

  if Parent = nil then
    raise Exception.Create('Cannot initialize physics in behavior not added to transform.');

  { We don't really need to access Parent.Parent, but

    - We need WorldTransform (so unique Parent.Parent or Parent.Parent = nil)

    - We do not sensibly work on Viewport.Items root --
      such body movement would affect also all children.

    So in practice we need Parent.Parent <> nil. }
  if Parent.Parent = nil then
    raise EMultipleReferencesInWorld.CreateFmt('Cannot use physics on %s. Reason is either:' + NL +
      NL +
      '1. It is the root transformation (Viewport.Items). You should instead: move the rigid body and collider behaviors to some child TCastleTransform.' + NL +
      NL +
      '2  It is inserted multiple times into the Viewport.Items. You should instead: Duplicate (clone) the instance. Or use TCastleTransformReference to refer to the TCastleTransform multiple times (and attach collider/rigid body to TCastleTransformReference).', [
        Parent.Name
      ]);

  FTransform := Parent;
  InitializeEngineRigidBody;
  InitializeCollider;
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

  DeinitializeCollider;

  FreeAndNil(FKraftBody);

  FTransform := nil;
  FState := stateNone;
end;

procedure TCastleRigidBody.UpdateKraftRigidBodyType;
begin
  if FState = stateNone then
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
    { In Kraft you can't change body type when it was once created.
      So we recreate rigid body physics objects. }
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
  UpdateLayer;

  if FTrigger then
    FKraftBody.Flags := FKraftBody.Flags + [krbfSensor];

  FState := stateMissingCollider;
end;

procedure TCastleRigidBody.InitializeCollider;

  { Returns TCastleCollider bahavior or @nil (when no TCastleCollider (behavior) is present in
    the Parent or Parent = @nil). }
  function GetCastleColliderFromParent: TCastleCollider;
  begin
    if Parent <> nil then
      Result := Parent.FindBehavior(TCastleCollider) as TCastleCollider
    else
      Result := nil;
  end;

  procedure FinishInitialization;
  const
    AllCollisionGroups = [
      Low (TKraftRigidBodyCollisionGroup)..
      High(TKraftRigidBodyCollisionGroup)];
  var
    V: TVector3;
  begin
    if Collider <> nil then
      FKraftBody.ForcedMass := Collider.Mass
    else
      FKraftBody.ForcedMass := FCastleCollider.Mass;

    FKraftBody.Finish;

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
    FKraftBody.MaximalLinearVelocity := FMaxLinearVelocity / FTransform.World.FKraftEngine.WorldFrequency;

    FKraftBody.AngularVelocityDamp := FAngularVelocityDamp;
    FKraftBody.MaximalAngularVelocity := FMaxAngularVelocity / FTransform.World.FKraftEngine.WorldFrequency;

    FState := stateReady; // before SetWorldTransformation that checks it

    { Set initial transformation }
    SetWorldTransformation(FTransform.WorldTransform);

    CheckPhysicsPostStepNeeded;
    NotifyOnInitialized;
  end;

var
  CastleColliderFromTransform: TCastleCollider;
begin
  if FState = stateNone then
    raise Exception.Create(
      'InitializeCollider: Trying to add collider but rigid body not initialized.');

  CastleColliderFromTransform := GetCastleColliderFromParent;
  if (Collider <> nil) and (CastleColliderFromTransform <> nil) then
    raise Exception.Create(
      'You can''t use TCollider and TCastleCollider in the same rigid body. ' + NL +
      'Remove deprecated RigidBody.Collider.');

  if Collider <> nil then
  begin
    Collider.InitializeKraft(Parent.World.FKraftEngine, FKraftBody);
    FinishInitialization;
    Exit;
  end;

  if (FCastleCollider <> CastleColliderFromTransform) and (FCastleCollider <> nil) then
  begin
    { Trying to initialize rigid body with another collider without deinitialization
      of the previous one. This should never occur. }
    raise Exception.Create('Reinitialize with new collider but old one not deinitialized.');
  end;

  if CastleColliderFromTransform <> nil then
  begin
    CastleColliderFromTransform.InitializeKraft(Parent.World.FKraftEngine, Self);
    FCastleCollider := CastleColliderFromTransform;
    FinishInitialization;
    Exit;
  end;
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
    // TODO: doesn't this miss FCastleCollider := ACastleCollider ?
    if FCastleCollider = nil then
    begin
      InitializeCollider;
    end else
    if FCastleCollider <> ACastleCollider then
    begin
      { Trying to initialize rigid body with another collider without deinitialization
        of the previous one. This should never occur. }
      raise Exception.Create('Reinitialize with new collider but old one not deinitialized.');
    end;
  end;
end;

procedure TCastleRigidBody.DeinitializeCollider;

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

  InitializeCollider;
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

procedure TCastleRigidBody.UpdateExist;

  { Call TCastleCollider.InternalAutoSize if we use the collider now.

    This is necessary to be called when size changed, and automatic size
    (determined by TCastleCollider.AutoSize) should be recalculated.
    Examples when it needs to be called:
    If TCastleTransform did not exist in root at initialization or
    TCastleScene URL changes.

    TODO: It is not sure whether its really needed to be called here, from UpdateExist. }
  procedure UpdateColliderAutoSize;
  begin
    if ExistsInRoot and (FCastleCollider <> nil) then
    begin
      FCastleCollider.InternalAutoSize;
    end;
  end;

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

  UpdateColliderAutoSize;
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

procedure TCastleRigidBody.KraftForceUpdatingReadyBody;
begin
  if FState = stateReady then
  begin
    Assert(FKraftBody <> nil); // FState = stateReady already implies this
    FKraftBody.Finish;
  end;
end;

procedure TCastleRigidBody.UpdateLockTranslation;
const
  LockTranslationFlag: array [T3DCoord] of TKraftRigidBodyFlag = (
    krbfLockTranslationAxisX,
    krbfLockTranslationAxisY,
    krbfLockTranslationAxisZ
  );
var
  I: T3DCoord;
begin
  if Assigned(FKraftBody) then
  begin
    for I := Low(I) to High(I) do
      if I in FLockTranslation then
        FKraftBody.Flags := FKraftBody.Flags + [LockTranslationFlag[I]]
      else
        FKraftBody.Flags := FKraftBody.Flags - [LockTranslationFlag[I]];
    KraftForceUpdatingReadyBody;
  end;
end;

procedure TCastleRigidBody.UpdateLockRotation;
const
  LockRotationFlag: array [T3DCoord] of TKraftRigidBodyFlag = (
    krbfLockRotationAxisX,
    krbfLockRotationAxisY,
    krbfLockRotationAxisZ
  );
var
  I: T3DCoord;
begin
  if Assigned(FKraftBody) then
  begin
    for I := Low(I) to High(I) do
      if I in FLockRotation then
        FKraftBody.Flags := FKraftBody.Flags + [LockRotationFlag[I]]
      else
        FKraftBody.Flags := FKraftBody.Flags - [LockRotationFlag[I]];
    KraftForceUpdatingReadyBody;
  end;
end;

procedure TCastleRigidBody.UpdateLayer;
var
  CollideWith: TPhysicsLayer;
  KraftCollideWith: TKraftRigidBodyCollisionGroups;
begin
  if FKraftBody <> nil then
  begin
    FKraftBody.CollisionGroups := [FLayer];

    // calculate KraftCollideWith and pass to Kraft
    KraftCollideWith := [];
    for CollideWith in FTransform.World.PhysicsProperties.LayerCollisions.FCollides[FLayer] do
      Include(KraftCollideWith, CollideWith);
    FKraftBody.CollideWithCollisionGroups := KraftCollideWith;
  end;
end;

procedure TCastleRigidBody.SetMaxAngularVelocity(const AValue: Single);
begin
  FMaxAngularVelocity := AValue;
  { Kraft uses max velocity for delta time which is physics update frequency. }
  if FKraftBody <> nil then
    FKraftBody.MaximalAngularVelocity := AValue / FTransform.World.FKraftEngine.WorldFrequency;
end;

procedure TCastleRigidBody.SetMaxLinearVelocity(const AValue: Single);
begin
  FMaxLinearVelocity := AValue;
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

procedure TCastleRigidBody.SetLayer(const AValue: TPhysicsLayer);
begin
  if FLayer <> AValue then
  begin
    FLayer := AValue;
    UpdateLayer;
  end;
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

procedure TCastleRigidBody.SetWorldTransformation(const WorldTransform: TMatrix4);
var
  Translation, Scale: TVector3;
  Rotation: TVector4;
  T: TTransformation;
begin
  { If we just set transformation from physics engine this is a guard
    to not update rigid body transformation. See TransformationFromKraft }
  if FDuringSettingTransformationFromPhysicsEngine then
    Exit;

  if FState <> stateReady then
    Exit;

  { Remove scale from world transform because using scale in kraft
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

      { Don't use Scale form Kraft.
        It should be always 1. We set CGE scale by changing Kraft collider sizes.
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
  if CastleApplicationMode in [appDesign, appSimulationPaused] then
    Exit;

  if Parent = nil then
    Exit;

  if (Parent.World <> nil) and (not Parent.World.EnablePhysics) then
    Exit;

  if Dynamic then
  begin
    TransformationFromKraft;
    SynchronizeFromKraft;
  end

  { Note: We don't do anything special for Animated here.
    And we allow changing TCastleTransform from code regardless of whether
    Animated = true or false.
    So Animated is really just an "optimization hint" passed to Kraft
    (determines if we should set body as krbtKinematic or krbtStatic, when Dynamic = false).
    Nothing more. }
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
  const MaxDistance: Single): TPhysicsRayCastResult;
var
  RayOriginWorld, RayDirectionWorld: TVector3;
  MaxDistanceWorld: Single;
begin
  if FTransform = nil then
  begin
    WritelnWarning('Attempt to cast a ray from TCastleRigidBody not connected to any TCastleTransform. Possibly you forgot to add it using TCastleTransform.AddBehavior.');
    FillChar(Result, SizeOf(Result), 0); // this in particular sets Result.Hit = false
    Exit;
  end;

  if FTransform.World = nil then
  begin
    WritelnWarning('Attempt to cast a ray from TCastleRigidBody not connected to any World. Possibly you forgot to add the TCastleTransform to TCastleViewport.Items.');
    FillChar(Result, SizeOf(Result), 0); // this in particular sets Result.Hit = false
    Exit;
  end;

  if FTransform.Parent <> nil then
  begin
    RayOriginWorld := FTransform.Parent.LocalToWorld(RayOrigin);
    RayDirectionWorld := FTransform.Parent.LocalToWorldDirection(RayDirection);
    MaxDistanceWorld := FTransform.Parent.LocalToWorldDistance(MaxDistance);
  end else
  begin
    RayOriginWorld := RayOrigin;
    RayDirectionWorld := RayDirection;
    MaxDistanceWorld := MaxDistance;
  end;

  Result := FTransform.World.PhysicsRayCast(RayOriginWorld, RayDirectionWorld,
    MaxDistanceWorld, Self);

  if Result.Hit and (FTransform.Parent <> nil) then
  begin
    Result.Distance := FTransform.Parent.WorldToLocalDistance(Result.Distance);
    Result.Point := FTransform.Parent.WorldToLocal(Result.Point);
    Result.Normal := FTransform.Parent.WorldToLocalDirection(Result.Normal);
  end;
end;

function TCastleRigidBody.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'Dynamic', 'Animated', 'Trigger', 'Gravity', 'LockTranslation', 'Exists',
     'AngularVelocityPersistent', 'AngularVelocityDamp', 'MaxAngularVelocity',
     'LinearVelocityPersistent', 'LinearVelocityDamp', 'MaxLinearVelocity',
     'CollisionDetection', 'LockRotation', 'Layer'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleRigidBody.AddForceAtPosition(const Force, Position: TVector3);
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

procedure TCastleRigidBody.AddForce(const Force: TVector3; const ForceInLocalCoordinates: Boolean);
begin
  if FKraftBody <> nil then
  begin
    if ExistsInRoot then
    begin
      if ForceInLocalCoordinates then
        FKraftBody.AddBodyForce(VectorToKraft(Force))
      else
        FKraftBody.AddWorldForce(VectorToKraft(Force));
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
    FKraftBody.AngularVelocity := VectorToKraft(ZeroLockedComponents(AVelocity, FLockRotation));
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

        - kcmSpeculativeContacts:
          it's fake so sometime we have fake collisons

        - kcmTimeOfImpactSubSteps:
          bullets in platformer sometimes pass through the walls,
          all TimeOfImpactAlgorithm algorithms have this issue.

        - kcmMotionClamping:
          works best I think (but may lead to a momentary loss of time in a collision).

        Future: Maybe give user ability to choose ContinuousMode. }
      FKraftEngine.ContinuousMode := kcmMotionClamping;
      { We need to set this because continuous works only when:
        - one of the bodies has krbfContinuous and they are dynamic
        - ContinuousAgainstDynamics is set to true in
          FKraftEngine and one of bodies has krbfContinuousAgainstDynamics flag }
      FKraftEngine.ContinuousAgainstDynamics := true;
      { Below are useful only for kcmTimeOfImpactSubSteps mode }
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
  {$ifdef CASTLE_LOG_PHYSICS_DROPS_FRAMES}
  OldTimeAccumulator: TFloatTime;
  {$endif}
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
          {$ifdef CASTLE_LOG_PHYSICS_DROPS_FRAMES}
          OldTimeAccumulator := TimeAccumulator;
          {$endif}

          TimeAccumulator := TimeAccumulator - (PhysicsProperties.FPhysicsTimeStep * Floor(TimeAccumulator / PhysicsProperties.FPhysicsTimeStep));

          // This log is valid, but too spammy now.
          {$ifdef CASTLE_LOG_PHYSICS_DROPS_FRAMES}
          WritelnLog('Max physics ticks in TCastleAbstractRootTransform.Update() exceeded ('
            + IntToStr(PhysicsTicksCount) + '). TimeAccumulator reduced from '
            + FloatToStrDot(OldTimeAccumulator) + ' to ' + FloatToStrDot(TimeAccumulator));
          {$endif}

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

function TCastleAbstractRootTransform.PhysicsRayCast(
  const RayOrigin, RayDirection: TVector3;
  const MaxDistance: Single; const IgnoreBody: TCastleRigidBody): TPhysicsRayCastResult;

  procedure Core;
  var
    KraftShape: TKraftShape;
    KraftDistance: TKraftScalar;
    KraftPoint: TKraftVector3;
    KraftNormal: TKraftVector3;
  begin
    { make sure everything is zero when no hit }
    FillChar(Result, SizeOf(Result), 0);

    { Note: In Kraft, the distance parameters are called "time"
      (MaxTime, Time instead of more natural MaxDistance, Distance).
      But all research shows that it is actually "distance" and that is also how
      other physics engines call it.

      TODO: Check time does not depend on physics frequency? }

    if FKraftEngine.RayCast(
      // inputs
      VectorToKraft(RayOrigin), VectorToKraft(RayDirection), MaxDistance,
      // outputs
      KraftShape, KraftDistance, KraftPoint, KraftNormal) then
    begin
      Result.Hit := true;
      Result.Transform := TCastleRigidBody(KraftShape.RigidBody.UserData).FTransform;
      Result.Point := VectorFromKraft(KraftPoint);
      Result.Normal := VectorFromKraft(KraftNormal);
      Result.Distance := KraftDistance;
    end;
  end;

var
  OldIgnoreShapeFlags: TKraftShapeFlags;
  IgnoreShape: TKraftShape;
begin
  // calculate IgnoreShape
  if IgnoreBody <> nil then
    IgnoreShape := IgnoreBody.GetKraftShape
  else
    IgnoreShape := nil;

  // protect IgnoreShape from being hit, use Core
  if IgnoreShape <> nil then
  begin
    OldIgnoreShapeFlags := IgnoreShape.Flags;
    try
      // We use ksfRayCastable flag to not hit the IgnoreBody
      IgnoreShape.Flags := IgnoreShape.Flags - [ksfRayCastable];
      Core;
    finally
      IgnoreShape.Flags := OldIgnoreShapeFlags;
    end;
  end else
    Core;
end;

{$endif read_implementation}
