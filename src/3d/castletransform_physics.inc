{
  Copyright 2017-2018 Michalis Kamburelis.

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
    property Density: Single read FDensity write FDensity default DefaultDensity;

    { Mass in kg. When non-zero, this overrides the automatically calculated
      mass from the volume and @link(Density). }
    property Mass: Single read FMass write FMass default DefaultMass;

    property Restitution: Single read FRestitution write FRestitution default DefaultRestitution;
    property Friction: Single read FFriction write FFriction default DefaultFriction;
  end;

  { Collide as an infinite plane.
    Place this inside @link(TRigidBody.Collider) property. }
  TPlaneCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Normal: TVector3;
    Distance: Single;
  end;

  { Collide as a box.
    Place this inside @link(TRigidBody.Collider) property. }
  TBoxCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Size: TVector3;
  end;

  { Collide as a sphere.
    Place this inside @link(TRigidBody.Collider) property. }
  TSphereCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
  end;

  T3DCoord = 0..2;
  T3DCoords = set of T3DCoord;

  TOnCollision = procedure (const CollisionDetails: TPhysicsCollisionDetails) of object;

  { Rigid body properties for the physics engine,
    see @link(TCastleTransform.RigidBody).

    TODO: For now all the properties of this class,
    including the Collider and all properties of Collider,
    must be assigned before setting TCastleTransform.RigidBody . }
  TRigidBody = class(TComponent)
  strict private
    { List of collisions from previous step. }
    FPrevCollisions: TList;
    { List of collisions from current step. }
    FCurrentCollisions: TList;

    FOnCollisionEnter: TOnCollision;
    FOnCollisionStay: TOnCollision;
    FOnCollisionExit: TOnCollision;

    procedure PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
    { Assign or unassign PhysicsPostStep in TKraftBody.OnPostStep when needed. }
    procedure CheckPhysicsPostStepNeeded;
  private
    FKraftBody: TKraftRigidBody;
    FCollider: TCollider;
    FGravity: Boolean;
    FDynamic: Boolean;
    FAnimated: Boolean;
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

    procedure SetOnCollisionEnter(const AValue: TOnCollision);
    procedure SetOnCollisionStay(const AValue: TOnCollision);
    procedure SetOnCollisionExit(const AValue: TOnCollision);

    procedure SetExists(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Utility function to set common values for physics in 2D games.
      Locks moving along the Z axis,
      locks rotating along the X and Y axes. }
    procedure Setup2D;

    { Connect this rigid body with Transform and Transform.World. }
    procedure InitializeTransform(const Transform: TCastleTransform);
    { Disconnect from Transform.World, if was connected. }
    procedure DeinitializeTransform(const Transform: TCastleTransform);
    procedure Update(const Transform: TCastleTransform; const SecondsPassed: Single);

    { Transformations that we collide with currently. }
    function GetCollidingTransforms: TCastleTransformList;

    property AngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;
    property AngularVelocityDamp: Single read FAngularVelocityDamp write SetAngularVelocityDamp;
    property MaximalAngularVelocity: Single read FMaximalAngularVelocity write SetMaximalAngularVelocity;

    property LinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;
    property LinearVelocityDamp: Single read FLinearVelocityDamp write SetLinearVelocityDamp;
    property MaximalLinearVelocity: Single read FMaximalLinearVelocity write SetMaximalLinearVelocity;

    property InitialAngularVelocity: TVector3 read FAngularVelocity write SetAngularVelocity;
      deprecated 'use AngularVelocity';
    property InitialLinearVelocity: TVector3 read FLinearVelocity write SetLinearVelocity;
      deprecated 'use LinearVelocity';

    { Occurs when TRigidBody starts colliding with another TRigidBody.

      It can occur repeatedly for the same body (in the same time instant)
      if the collision has many points of contact.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleTransform.OnUpdate)). }
    property OnCollisionEnter: TOnCollision read FOnCollisionEnter write SetOnCollisionEnter;

    { Occurs when TRigidBody stops colliding with another TRigidBody.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleTransform.OnUpdate)). }
    property OnCollisionExit: TOnCollision read FOnCollisionExit write SetOnCollisionExit;

    { Occurs when TRigidBody still colliding with another TRigidBody.

      @italic(Warning:) Do not free the @link(TCastleTransform) instances
      that collide during this event.
      It would free the rigid body instance, which will crash the physics engine for now.
      Instead, you can set @link(TCastleTransform.Exists) to @false
      (you can free it later, e.g. during @link(TCastleTransform.OnUpdate)). }
    property OnCollisionStay: TOnCollision read FOnCollisionStay write SetOnCollisionStay;

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

          Also, the value @link(Animated) is ignored in this case.
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
    property Dynamic: boolean read FDynamic write FDynamic default true;

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
    property Animated: boolean read FAnimated write FAnimated default false;

    { Shape used for collisions with this object.
      You cannot assign this property directly,
      it is automatically assigned when you create a @link(TCollider)
      specifying this @link(TRigidBody) as it's parent. }
    property Collider: TCollider read FCollider;

    { Is this object affected by gravity. }
    property Gravity: boolean read FGravity write FGravity default true;

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

{ TPlaneCollider ------------------------------------------------------------- }

function TPlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(Normal)), Distance));
end;

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

  if (Size[0] <= 0) or
     (Size[1] <= 0) or
     (Size[2] <= 0) then
    raise EPhysicsError.Create('You must assign positive Size to TBoxCollider');
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(Size / 2));
end;

{ TSphereCollider ------------------------------------------------------------ }

function TSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, Radius);
end;

{ TRigidBody ----------------------------------------------------------------- }

constructor TRigidBody.Create(AOwner: TComponent);
begin
  inherited;

  // properties defaults
  FGravity := true;
  FDynamic := true;
  FExists := true;
  // default damp values from Kraft
  FLinearVelocityDamp := 0.1;
  FAngularVelocityDamp := 0.1;

  FKraftBody := nil;
  FCollisionList := TCastleTransformList.Create(false, nil);
  FPrevCollisions := TList.Create;
  FCurrentCollisions := TList.Create;

  FOnCollisionEnter := nil;
  FOnCollisionExit := nil;
  FTransform := nil;
end;

destructor TRigidBody.Destroy;
begin
  { In case TRigidBody is destroyed before the connected FTransform is destroyed
    (testcase: physics_3d_demo exit), it is necessary to clear FTransform.FRigidBody.
    This avoids having a dangling pointer in FTransform.FRigidBody to half-destroyed
    instance.

    Otherwise:
    - "inherited" below calls TComponent free notification,
    - which calls TCastleTransform.PhysicsNotification,
    - which would (if FTransform.FRigidBody is not cleared)
      call DeinitializeTransform on our instance, with FKraftBody = nil.
  }
  if FTransform <> nil then
    FTransform.FRigidBody := nil;

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

procedure TRigidBody.Setup2D;
begin
  LockTranslation := [2];
  LockRotation := [0, 1];
end;

function ZeroLockedComponents(const V: TVector3;
  const Locked: T3DCoords): TVector3;
begin
  Result := V;
  if 0 in Locked then Result[0] := 0;
  if 1 in Locked then Result[1] := 0;
  if 2 in Locked then Result[2] := 0;
end;

procedure TRigidBody.InitializeTransform(const Transform: TCastleTransform);

  procedure RecreateKraftInstance;
  var
    World: TSceneManagerWorld;
    V: TVector3;
  begin
    World := Transform.World;
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
    FKraftBody.ForcedMass := Collider.Mass;
    if 0 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisX];
    if 1 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisY];
    if 2 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisZ];
    if 0 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisX];
    if 1 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisY];
    if 2 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisZ];
    if not FExists then
      FKraftBody.Flags := FKraftBody.Flags - [krbfActive];

    // initialize Kraft shape
    if Collider = nil then
      raise EPhysicsError.Create('You must assign TRigidBody.Collider');
    Collider.InitializeKraft(World.FKraftEngine, FKraftBody);

    FKraftBody.Finish;

    UpdateCollides(Transform);

    if (not FAngularVelocity.IsPerfectlyZero) or
       (not FLinearVelocity.IsPerfectlyZero) then
    begin
      { The behaviour is more natural when we zero the XxxVelocity
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
    Collider.FKraftShape.StoreWorldTransform;

    CheckPhysicsPostStepNeeded;
  end;

begin
  Assert(FKraftBody = nil, 'Kraft body is initialized!');

  if Transform.FWorldReferences > 1 then
    raise EMultipleReferencesInWorld.Create('Do not use RigidBody physics with objects (TCastleScene or TCastleTransform) inserted multiple times into the SceneManager.Items');

  FTransform := Transform;
  RecreateKraftInstance;
end;

procedure TRigidBody.DeinitializeTransform(const Transform: TCastleTransform);
begin
  Assert(FKraftBody <> nil, 'Second deinitialization!');
  Assert(Transform.World <> nil, 'Transform.World should be assigned at the time of TRigidBody.DeinitializeTransform call');

  Assert(not ((Transform.World.FKraftEngine = nil) and (FKraftBody <> nil)), 'KraftBody should not live longer than KraftEngine!');

  FreeAndNil(FKraftBody);

  { Collider.FKraftShape is owned by FKraftBody, it was automatically freed already }
  if Collider <> nil then
    Collider.FKraftShape := nil;

  FTransform := nil;
end;

procedure TRigidBody.PhysicsPostStep(const RigidBody: TKraftRigidBody; const TimeStep: TKraftTimeStep);
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
    if not (kcfColliding in ContactPairEdge^.ContactPair^.Flags) then
    begin
      ContactPairEdge := ContactPairEdge^.Next;
      continue;
    end;

    RBody := TRigidBody(ContactPairEdge^.OtherRigidBody.UserData);

    // Omit next collision point with the same body.
    if FCurrentCollisions.IndexOf(RBody) <> - 1 then
    begin
      ContactPairEdge := ContactPairEdge^.Next;
      continue;
    end;

    // Add to current collisons.
    FCurrentCollisions.Add(RBody);

    // Prepare collision data.
    CollisionDetails.OtherTransform := RBody.FTransform;
    CollisionDetails.Transforms[0] := FTransform;
    CollisionDetails.Transforms[1] := CollisionDetails.OtherTransform;

    // New and ongoing collisons.
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

      // Remove used collison.
      FPrevCollisions.Remove(RBody);
    end;

    ContactPairEdge := ContactPairEdge^.Next;
  end;

  // check collision exit
  if Assigned(FOnCollisionExit) then
  begin
    CollisionDetails.Transforms[0] := FTransform;

    for I := 0  to FPrevCollisions.Count - 1 do
    begin
      CollisionDetails.OtherTransform := TRigidBody(FPrevCollisions[I]).FTransform;
      CollisionDetails.Transforms[1] := CollisionDetails.OtherTransform;

      FOnCollisionExit(CollisionDetails);
    end;
  end;

  // Make previous list from current list.
  FPrevCollisions.Clear;
  FPrevCollisions.Assign(FCurrentCollisions);
end;

procedure TRigidBody.CheckPhysicsPostStepNeeded;
begin
  if not Assigned(FKraftBody) then
    Exit;

  if Assigned(FOnCollisionEnter) or Assigned(FOnCollisionStay) or Assigned(FOnCollisionExit) then
    FKraftBody.OnPostStep := @PhysicsPostStep
  else
    FKraftBody.OnPostStep := nil;
end;

procedure TRigidBody.UpdateCollides(const Transform: TCastleTransform);
const
  AllCollisionGroups = [
    Low (TKraftRigidBodyCollisionGroup)..
    High(TKraftRigidBodyCollisionGroup)];
begin
  // TODO: this looks at my GetCollides, not at parents GetCollides
  if Transform.GetCollides then
  begin
    FKraftBody.CollideWithCollisionGroups := AllCollisionGroups;
    FKraftBody.CollisionGroups := [0]; // TODO: make this configurable
  end else
  begin
    FKraftBody.CollideWithCollisionGroups := [];
    FKraftBody.CollisionGroups := [];
  end;
end;

procedure TRigidBody.SetMaximalAngularVelocity(const AValue: Single);
begin
  FMaximalAngularVelocity := AValue;
  { Kraft uses max velocity for delta time which is physics update frequency. }
  if FKraftBody <> nil then
    FKraftBody.MaximalAngularVelocity := AValue / FTransform.World.FKraftEngine.WorldFrequency;
end;

procedure TRigidBody.SetMaximalLinearVelocity(const AValue: Single);
begin
  FMaximalLinearVelocity := AValue;
  { Kraft uses max velocity for delta time which is physics update frequency. }
  if FKraftBody <> nil then
    FKraftBody.MaximalLinearVelocity := AValue / FTransform.World.FKraftEngine.WorldFrequency;
end;

procedure TRigidBody.SetOnCollisionEnter(const AValue: TOnCollision);
begin
  FOnCollisionEnter := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TRigidBody.SetOnCollisionStay(const AValue: TOnCollision);
begin
  FOnCollisionStay := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TRigidBody.SetOnCollisionExit(const AValue: TOnCollision);
begin
  FOnCollisionExit := AValue;
  CheckPhysicsPostStepNeeded;
end;

procedure TRigidBody.SetAngularVelocityDamp(const AValue: Single);
begin
  FAngularVelocityDamp := AValue;
  if FKraftBody <> nil then
    FKraftBody.AngularVelocityDamp := AValue;
end;

procedure TRigidBody.SetLinearVelocityDamp(const AValue: Single);
begin
  FLinearVelocityDamp := AValue;
  if FKraftBody <> nil then
    FKraftBody.LinearVelocityDamp := AValue;
end;

procedure TRigidBody.SynchronizeFromKraft;
begin
  if FKraftBody = nil then
    Exit;

  FLinearVelocity := VectorFromKraft(FKraftBody.LinearVelocity);
  FAngularVelocity := VectorFromKraft(FKraftBody.AngularVelocity);
end;

procedure TRigidBody.Update(const Transform: TCastleTransform; const SecondsPassed: Single);

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
    Shape := FCollider.FKraftShape;

    LocalTransform := Transform.Parent.WorldInverseTransform *
      MatrixFromKraft(Shape.InterpolatedWorldTransform);

    MatrixDecompose(LocalTransform, Translation, Rotation, Scale);
    Transform.Rotation := Rotation;
    Transform.Translation := Translation;

    { This is an alternative version using Kraft to do matrix decomposition. }
    // Q := QuaternionFromMatrix4x4(MatrixToKraft(LocalTransform));
    // QuaternionToAxisAngle(Q, Axis, Angle);
    // Transform.Rotation := Vector4(Axis.X, Axis.Y, Axis.Z, Angle);

    // Transform.Translation := TranslationFromMatrix(LocalTransform);
  end;

begin
  if (Transform.World <> nil) and (not Transform.World.EnablePhysics) then
    Exit;

  UpdateCollides(Transform);
  if Dynamic then
  begin
    TransformationFromKraft;
    SynchronizeFromKraft; // I think its only required for dynamic bodies.
  end else
  if Animated then
  begin
    // TODO: check "if TransformChanged then" or such, don't do this every frame
    FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.WorldTransform))

    // TODO: do above also for rbStatic, once "if TransformChanged then" implemented
    // or will this require recreating rigid body?
  end;
end;

function TRigidBody.GetCollidingTransforms: TCastleTransformList;
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

procedure TRigidBody.SetLinearVelocity(const LVelocity: TVector3);
begin
  FLinearVelocity := LVelocity;
  if FKraftBody <> nil then
  begin
    FKraftBody.LinearVelocity := VectorToKraft(ZeroLockedComponents(LVelocity, FLockTranslation));
    if not LVelocity.IsPerfectlyZero then
      FKraftBody.SetToAwake;
  end;
end;

procedure TRigidBody.SetAngularVelocity(const AVelocity: TVector3);
begin
  FAngularVelocity := AVelocity;
  if FKraftBody <> nil then
  begin
    FKraftBody.AngularVelocity := VectorToKraft(ZeroLockedComponents(AVelocity, FLockTranslation));
    if not AVelocity.IsPerfectlyZero then
      FKraftBody.SetToAwake;
  end;
end;

procedure TRigidBody.SetExists(const Value: Boolean);
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

  if Assigned(Collider) and Assigned(Collider.FKraftShape) then
  begin
    if FExists then
      Collider.FKraftShape.Flags := Collider.FKraftShape.Flags + [ksfCollision]
    else
      Collider.FKraftShape.Flags := Collider.FKraftShape.Flags - [ksfCollision];
  end;
end;

{ TCastleTransform ---------------------------------------------------------- }

procedure TCastleTransform.SetRigidBody(const Value: TRigidBody);
begin
  if FRigidBody <> Value then
  begin
    if FRigidBody <> nil then
    begin
      FRigidBody.RemoveFreeNotification(Self);
      if World <> nil then
        RigidBody.DeinitializeTransform(Self);
    end;
    FRigidBody := Value;
    if FRigidBody <> nil then
    begin
      FRigidBody.FreeNotification(Self);
      { For the same reason that TCastleTransform.PhysicsChangeWorld calls this,
        also here call InitializeTransform, in case ChangeWorld happened
        before SetRigidBody. }
      if World <> nil then
        RigidBody.InitializeTransform(Self);
    end;
  end;
end;

procedure TCastleTransform.PhysicsDestroy;
begin
  { set to nil by SetRigidBody, to detach free notification }
  RigidBody := nil;
end;

procedure TCastleTransform.UpdatePhysicsEngine(const SecondsPassed: Single);
begin
  if FRigidBody <> nil then
    FRigidBody.Update(Self, SecondsPassed);
end;

procedure TCastleTransform.PhysicsNotification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FRigidBody) then
    { set to nil by SetRigidBody to clean nicely }
    RigidBody := nil;
end;

procedure TCastleTransform.PhysicsChangeWorldDetach;
begin
  { When removing 3D object from world, remove it also from physics engine world. }
  if (RigidBody <> nil) and (RigidBody.FKraftBody <> nil) then
    RigidBody.DeinitializeTransform(Self);
end;

procedure TCastleTransform.PhysicsChangeWorldAttach;
begin
  { Calling TRigidBody.InitializeTransform explicitly now
    (instead of letting it happen in TRigidBody.Update)
    makes sure that when TSceneManagerWorld.Update will happen,
    the physics engine will know
    the initial position of this object during 1st simulation tick.

    TODO: it's not really nice.
    - If you change a transformation from now to TSceneManagerWorld.Update,
      then 1st simulation tick will have invalid transformation anyway.
    - If you change some other rigid body parameter between now and
      next FRigidBody.Update, then next FRigidBody would recreate Kraft
      resources for this object (once it's implemented, by setting
      FRecreateKraftInstance on every change),
      which is needless (we create Kraft resourcest
      for this transform 2 times in this case, instead of once). }
  if RigidBody <> nil then
    RigidBody.InitializeTransform(Self);
end;

{ TSceneManagerWorld ------------------------------------------------------------------- }

procedure TSceneManagerWorld.InitializePhysicsEngine;
begin
  if FKraftEngine = nil then
  begin
    FKraftEngine := TKraft.Create(-1);
    { Kraft sets MaximalLinearVelocity in TKraft Constructor to 2.
      With this limit can't make velocity greater than about 120
      (2 * engine step frequency = 2 * 60 = 120). That makes physics
      very slow, so we need remove this limitation. }
    FKraftEngine.MaximalLinearVelocity := 0;
    //FKraftEngine.SetFrequency(120.0); // default is 60
  end;
end;

destructor TSceneManagerWorld.Destroy;
var
  CastleRigidBody: TRigidBody;
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
        CastleRigidBody := TRigidBody(KraftRigidBody.UserData);
        CastleRigidBody.DeinitializeTransform(CastleRigidBody.FTransform);
      end;
      KraftRigidBody := NextKraftRigidBody;
    end;
  end;

  FreeAndNil(FKraftEngine);
  inherited;
end;

procedure TSceneManagerWorld.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  PhysicsTimeStep: TFloatTime;
  KraftGravity: TVector3;
begin
  if EnablePhysics and (FKraftEngine <> nil) then
  begin
    // update FKraftEngine.Gravity
    // TODO: do we really need to be prepared that it changes each frame?
    KraftGravity := -GravityUp * 9.81;
    FKraftEngine.Gravity.Vector := VectorToKraft(KraftGravity);

    if not WasPhysicsStep then
    begin
      FKraftEngine.StoreWorldTransforms;
      FKraftEngine.InterpolateWorldTransforms(0.0);
      WasPhysicsStep := true;
    end else
    begin
      PhysicsTimeStep := 1.0 / FKraftEngine.WorldFrequency;
      TimeAccumulator := TimeAccumulator + SecondsPassed;
      while TimeAccumulator >= PhysicsTimeStep do
      begin
        TimeAccumulator := TimeAccumulator - PhysicsTimeStep;
        FKraftEngine.StoreWorldTransforms;
        FKraftEngine.Step(PhysicsTimeStep);
      end;

      { One can wonder why we do interpolate below between
        - previous-to-last calculated physics state
        - and the last calculated physics state

        It seems that we should interpolate instead between
        - the last calculated physics state
        - and the "future" physics state, so we should make one more
          "FKraftEngine.Step" call (and remember to make one less
          "FKraftEngine.Step" call in the next loop run).

        The contains an explanation:
        http://web.archive.org/web/20160205035208/http://gafferongames.com/game-physics/fix-your-timestep/

        """
        You’re actually delaying the simulation by one frame
        and then doing the interpolation to ensure smoothness.
        """

        (The original https://gafferongames.com/post/fix_your_timestep/ no longer
        has comments.)
      }

      FKraftEngine.InterpolateWorldTransforms(TimeAccumulator / PhysicsTimeStep);
    end;
  end;

  { call inherited at the end,
    to update transformation of all items in their TRigidBody.Update
    called from TCastleTransform.Update }
  inherited;
end;

{$endif read_implementation}
