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

  { Rigid body properties for the physics engine,
    see @link(TCastleTransform.RigidBody).

    TODO: For now all the properties of this class,
    including the Collider and all properties of Collider,
    must be assigned before setting TCastleTransform.RigidBody . }
  TRigidBody = class(TComponent)
  private
    FKraftBody: TKraftRigidBody;
    FCollider: TCollider;
    FGravity: boolean;
    FDynamic: boolean;
    FAnimated: boolean;
    FLockTranslation: T3DCoords;
    FLockRotation: T3DCoords;
    FInitialAngularVelocity: TVector3;
    FInitialLinearVelocity: TVector3;
    FRecreateKraftInstance: boolean;
    procedure UpdateCollides(const Transform: TCastleTransform);
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

    property InitialAngularVelocity: TVector3 read FInitialAngularVelocity write FInitialAngularVelocity;
    property InitialLinearVelocity: TVector3 read FInitialLinearVelocity write FInitialLinearVelocity;
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

  FRecreateKraftInstance := true;
end;

destructor TRigidBody.Destroy;
begin
  { The FKraftBody will be freed now if you free TRigidBody
    instance explicitly. In most other cases, DeinitializeTransform
    already freed FKraftBody and set it nil. }

  FreeAndNil(FKraftBody);
  FreeAndNil(FCollider);
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

    FreeAndNil(FKraftBody);
    FKraftBody := TKraftRigidBody.Create(World.FKraftEngine);

    if Dynamic then
      FKraftBody.SetRigidBodyType(krbtDynamic)
    else
    if Animated then
      FKraftBody.SetRigidBodyType(krbtKinematic)
    else
      FKraftBody.SetRigidBodyType(krbtStatic);

    // initialize Kraft shape
    if Collider = nil then
      raise EPhysicsError.Create('You must assign TRigidBody.Collider');
    Collider.InitializeKraft(World.FKraftEngine, FKraftBody);

    FKraftBody.GravityScale := Iff(Gravity, 1.0, 0.0);
    FKraftBody.ForcedMass := Collider.Mass;
    if 0 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisX];
    if 1 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisY];
    if 2 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockRotationAxisZ];
    if 0 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisX];
    if 1 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisY];
    if 2 in FLockTranslation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockTranslationAxisZ];
    FKraftBody.Finish;

    UpdateCollides(Transform);

    if (not InitialAngularVelocity.IsPerfectlyZero) or
       (not InitialLinearVelocity.IsPerfectlyZero) then
    begin
      { The behaviour is more natural when we zero the InitialXxxVelocity
        components that are locked. (Otherwise testing e.g. Setup2D in a 3D
        world makes a little unexpected motions). }

      V := ZeroLockedComponents(InitialAngularVelocity, FLockRotation);
      FKraftBody.AngularVelocity := VectorToKraft(V);

      V := ZeroLockedComponents(InitialLinearVelocity, FLockTranslation);
      FKraftBody.LinearVelocity := VectorToKraft(V);

      FKraftBody.SetToAwake;
    end;

    // set initial transformation
    FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.WorldTransform));
  end;

begin
  if Transform.FWorldReferences > 1 then
    raise EMultipleReferencesInWorld.Create('Do not use RigidBody physics with objects (TCastleScene or TCastleTransform) inserted multiple times into the SceneManager.Items');

  if FRecreateKraftInstance then
  begin
    FRecreateKraftInstance := false;
    RecreateKraftInstance;
  end;
end;

procedure TRigidBody.DeinitializeTransform(const Transform: TCastleTransform);
begin
  Assert(Transform.World <> nil, 'Transform.World should be assigned at the time of TRigidBody.DeinitializeTransform call');

  if Transform.World.FKraftEngine <> nil then
    FreeAndNil(FKraftBody)
  else
    { FKraftBody is owned by FKraftEngine.
      If FKraftEngine = nil then FKraftBody was already freed.
      This can happen because TSceneManagerWorld.Destroy frees FKraftEngine,
      but the TCastleTransform instances (with TRigidBody)
      may be detached later, see TTestCastleTransform.TestPhysicsWorldOwner. }
    FKraftBody := nil;

  { Collider.FKraftShape is owned by FKraftBody, it was automatically freed already }
  if Collider <> nil then
    Collider.FKraftShape := nil;

  { we in unintialized state, so initialize at nearest occasion }
  FRecreateKraftInstance := true;
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

  InitializeTransform(Transform);
  UpdateCollides(Transform);
  if Dynamic then
    TransformationFromKraft
  else
  if Animated then
  begin
    // TODO: check "if TransformChanged then" or such, don't do this every frame
    FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.WorldTransform))

    // TODO: do above also for rbStatic, once "if TransformChanged then" implemented
    // or will this require recreating rigid body?
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
  if RigidBody <> nil then
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
    //KraftEngine.SetFrequency(120.0); // default is 60
  end;
end;

destructor TSceneManagerWorld.Destroy;
begin
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
