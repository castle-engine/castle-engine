{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Physics integration code between Castle3D and Kraft physics engine. }

{$ifdef read_interface}

  { Shape used for collision detection of a rigid body,
    placed in @link(TRigidBody.Collider) property. }
  TCollider = class(TComponent)
  private
    FKraftShape: TKraftShape;
    FParent: TRigidBody;
    procedure SetParent(const Value: TRigidBody);
    procedure InitializeKraft(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;

    { Parent rigid body, which in turn refers to this collider
      by @link(TRigidBody.Collider).
      You can always assign this property instead of assigning
      the @link(TRigidBody.Collider). }
    property Parent: TRigidBody read FParent write SetParent;
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
    see @link(T3DTransform.RigidBody).

    TODO: For now all the properties of this class,
    including the Collider and all properties of Collider,
    must be assigned before setting T3DTransform.RigidBody . }
  TRigidBody = class(TComponent)
  private
    FKraftBody: TKraftRigidBody;
    FCollider: TCollider;
    FGravity: boolean;
    FDynamic: boolean;
    FAnimated: boolean;
    //FMass: Single;
    FLockPosition: T3DCoords;
    FLockRotation: T3DCoords;
    FInitialAngularVelocity: TVector3;
    FInitialLinearVelocity: TVector3;
    FRecreateKraftInstance: boolean;
    procedure InitializeKraft(const APhysics: TKraft);
    procedure SetCollider(const Value: TCollider);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Utility function to set common values for physics in 2D games.
      Locks moving along the Z axis,
      locks rotating along the X and Y axes. }
    procedure Setup2D;

    { Connect this rigid body with Transform and Transform.World. }
    procedure InitializeTransform(const Transform: T3DTransform);
    { Disconnect from Transform.World, if was connected. }
    procedure DeinitializeTransform(const Transform: T3DTransform);
    procedure Update(const Transform: T3DTransform; const SecondsPassed: Single);

    property InitialAngularVelocity: TVector3 read FInitialAngularVelocity write FInitialAngularVelocity;
    property InitialLinearVelocity: TVector3 read FInitialLinearVelocity write FInitialLinearVelocity;
  published
    { Does the physics simulation move and rotate this object
      (because of gravity, or because it collides with others).

      @definitionList(
        @itemLabel true (dynamic object, the default)
        @item(
          In this case you cannot
          change the @link(T3DTransform.Translation) and other
          transformation properties of the related T3DTransform
          after assigning @link(T3DTransform.RigidBody).
          They are under the control of the physics engine.
          You can still reliably read them.

          Also, the value @link(Animated) is ignored in this case.
        )

        @itemLabel false
        @item(
          In this case object is not transformed by the physics simulation.
          It still collides with other physical objects.

          You can change the @link(T3DTransform.Translation) and other
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
      you can change the @link(T3DTransform.Translation) and other
      transformation properties of the related T3DTransform.
      This property is an optimization hint to the physics engine,
      it says @italic(how often) you will change the transformation.

      @definitionList(
        @itemLabel true (animated object, also known as "kinematic")
        @item(
          The physics engine is prepared that this object can move often,
          e.g. every frame, so you can animate the related @link(T3DTransform)
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

    { Shape used for collisions with this object. }
    property Collider: TCollider read FCollider write SetCollider;

    { Is this object affected by gravity. }
    property Gravity: boolean read FGravity write FGravity default true;

    { Mass in kg.
      TODO: should have non-zero default?
      TODO: better to use Density, Kraft does this?
      TODO: not applied yet to Kraft. }
    // property Mass: Single read FMass write FMass;

    { Disable motion (@link(T3DTransform.Translation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable motion along the Z axis.
      You can do this comfortably by calling @link(Setup2D). }
    property LockPosition: T3DCoords read FLockPosition write FLockPosition;

    { Disable rotation (@link(T3DTransform.Rotation) change) along
      the particular (world) axis.
      You can do this comfortably by calling @link(Setup2D). }
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

{ TCollider ------------------------------------------------------------------ }

procedure TCollider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParent) then
    { set to nil by SetParent to clean nicely }
    Parent := nil;
end;

procedure TCollider.SetParent(const Value: TRigidBody);
var
  OldParent: TRigidBody;
begin
  if FParent <> Value then
  begin
    OldParent := FParent;
    FParent := Value;
    { update OldParent.Collider after actually changing FParent,
      this way we avoid infinite loop when both TCollider and TRigidBody
      try to set each other to nil. }
    if OldParent <> nil then
    begin
      if OldParent.Collider = Self then
        OldParent.Collider := nil;
      OldParent.RemoveFreeNotification(Self);
    end;
    if FParent <> nil then
    begin
      FParent.FreeNotification(Self);
      FParent.Collider := Self;
    end;
  end;
end;

procedure TCollider.InitializeKraft(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody);
begin
  // FreeAndNil(FKraftShape); // do not free here, TKraftShape is owned by TKraftRigidBody
  FKraftShape := CreateKraftShape(APhysics, ARigidBody);
end;

destructor TCollider.Destroy;
begin
  { set to nil by SetParent, to detach free notification }
  Parent := nil;

  // FreeAndNil(FKraftShape); // do not free here, TKraftShape is owned by TKraftRigidBody
  inherited;
end;

{ TPlaneCollider ------------------------------------------------------------- }

function TPlaneCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapePlane.Create(APhysics, ARigidBody,
    Plane(Vector3Norm(VectorToKraft(Normal)), Distance));
  // TODO: make configurable
  Result.Restitution := 0.3;
end;

{ TBoxCollider --------------------------------------------------------------- }

function TBoxCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(Size / 2));
  // TODO: make configurable
  Result.Restitution := 0.3;
  Result.Density := 100.0;
end;

{ TSphereCollider ------------------------------------------------------------ }

function TSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, Radius);
  // TODO: make configurable
  Result.Friction := 0.4;
  Result.Restitution := 0.2;
  Result.Density := 20.0;
end;

{ TRigidBody ----------------------------------------------------------------- }

constructor TRigidBody.Create(AOwner: TComponent);
begin
  inherited;
  FGravity := true;
  FDynamic := true;
  FRecreateKraftInstance := true;
end;

destructor TRigidBody.Destroy;
begin
  FreeAndNil(FKraftBody);

  { set to nil by SetCollider, to detach free notification }
  Collider := nil;

  inherited;
end;

procedure TRigidBody.InitializeKraft(const APhysics: TKraft);
begin
  FreeAndNil(FKraftBody);
  FKraftBody := TKraftRigidBody.Create(APhysics);

  if Dynamic then
    FKraftBody.SetRigidBodyType(krbtDynamic)
  else
  if Animated then
    FKraftBody.SetRigidBodyType(krbtKinematic)
  else
    FKraftBody.SetRigidBodyType(krbtStatic);

  if Collider = nil then
    raise Exception.Create('You must assign TRigidBody.Collider');
  Collider.InitializeKraft(APhysics, FKraftBody);
  FKraftBody.GravityScale := Iff(Gravity, 1.0, 0.0);
  if 0 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockAxisX];
  if 1 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockAxisY];
  if 2 in FLockRotation then FKraftBody.Flags := FKraftBody.Flags + [krbfLockAxisZ];
  FKraftBody.Finish;

  FKraftBody.CollisionGroups := [0]; // TODO: make this configurable
  if (not InitialAngularVelocity.IsPerfectlyZero) or
     (not InitialLinearVelocity.IsPerfectlyZero) then
  begin
    FKraftBody.AngularVelocity := VectorToKraft(InitialAngularVelocity);
    FKraftBody.LinearVelocity := VectorToKraft(InitialLinearVelocity);
    FKraftBody.SetToAwake;
  end;
end;

procedure TRigidBody.Setup2D;
begin
  LockPosition := [2];
  LockRotation := [0, 1];
end;

procedure TRigidBody.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCollider) then
    { set to nil by SetCollider to clean nicely }
    Collider := nil;
end;

procedure TRigidBody.SetCollider(const Value: TCollider);
var
  OldCollider: TCollider;
begin
  if FCollider <> Value then
  begin
    OldCollider := FCollider;
    FCollider := Value;
    { update OldCollider.Parent after actually changing FCollider,
      this way we avoid infinite loop when both TCollider and TRigidBody
      try to set each other to nil. }
    if OldCollider <> nil then
    begin
      if OldCollider.Parent = Self then
        OldCollider.Parent := nil;
      OldCollider.RemoveFreeNotification(Self);
    end;
    if FCollider <> nil then
    begin
      FCollider.FreeNotification(Self);
      FCollider.Parent := Self;
    end;
  end;
end;

procedure TRigidBody.InitializeTransform(const Transform: T3DTransform);

  procedure RecreateKraftInstance;
  var
    World: T3DWorld;
  begin
    World := Transform.World;
    World.InitializePhysicsEngine;
    InitializeKraft(World.FKraftEngine);
    // TODO: this assumes that this object is not further transformed by parents
    FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.Transform));
  end;

begin
  if FRecreateKraftInstance then
  begin
    FRecreateKraftInstance := false;
    RecreateKraftInstance;
  end;
end;

procedure TRigidBody.DeinitializeTransform(const Transform: T3DTransform);
begin
  FreeAndNil(FKraftBody);
  { Collider.FKraftShape is owned by FKraftBody, it was automatically freed now }
  if Collider <> nil then
    Collider.FKraftShape := nil;
  { we in unintialized state, so initialize at nearest occasion }
  FRecreateKraftInstance := true;
end;

procedure TRigidBody.Update(const Transform: T3DTransform; const SecondsPassed: Single);

  { Update current transformation from Kraft rigid body parameters. }
  procedure TransformationFromKraft;
  var
    Q: TKraftQuaternion;
    Axis: TKraftVector3;
    Angle: TKraftScalar;
    NewPos: TKraftVector3;
    Body: TKraftRigidBody;
    Shape: TKraftShape;
  begin
    Body := FKraftBody;
    Shape := FCollider.FKraftShape;

    Q := QuaternionFromMatrix4x4(Shape.InterpolatedWorldTransform);
    QuaternionToAxisAngle(Q, Axis, Angle);
    Transform.Rotation := Vector4(Axis.X, Axis.Y, Axis.Z, Angle);

    NewPos := Shape.GetCenter(Shape.InterpolatedWorldTransform);
    if LockPosition <> [] then
    begin
      { TODO: Kraft feature request for [LockPositionX / Y / Z]? }
      // apply LockPosition to fix some NewPos coords
      if 0 in LockPosition then NewPos.X := Transform.Translation[0];
      if 1 in LockPosition then NewPos.Y := Transform.Translation[1];
      if 2 in LockPosition then NewPos.Z := Transform.Translation[2];
    end;
    Transform.Translation := VectorFromKraft(NewPos);
    if LockPosition <> [] then
      // fix also position at Kraft side after fixing by LockPosition
      Body.SetWorldTransformation(MatrixToKraft(Transform.Transform));
  end;

begin
  InitializeTransform(Transform);

  if Dynamic then
    TransformationFromKraft
  else
  if Animated then
  begin
    // TODO: check "if TransformChanged then" or such, don't do this every frame
    // TODO: this assumes that this object is not further transformed by parents
    FKraftBody.SetWorldTransformation(MatrixToKraft(Transform.Transform))

    // TODO: do above also for rbStatic, once "if TransformChanged then" implemented
    // or will this require recreating rigid body?
  end;
end;

{ T3DTransform ---------------------------------------------------------- }

procedure T3DTransform.SetRigidBody(const Value: TRigidBody);
begin
  if FRigidBody <> Value then
  begin
    if FRigidBody <> nil then
      FRigidBody.RemoveFreeNotification(Self);
    FRigidBody := Value;
    if FRigidBody <> nil then
    begin
      FRigidBody.FreeNotification(Self);
      { For the same reason that T3DTransform.SetWorld calls this,
        also here call InitializeTransform, in case SetWorld happened
        before SetRigidBody. }
      if World <> nil then
        RigidBody.InitializeTransform(Self);
    end;
  end;
end;

destructor T3DTransform.Destroy;
begin
  { set to nil by SetRigidBody, to detach free notification }
  RigidBody := nil;

  inherited;
end;

procedure T3DTransform.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited;
  if FRigidBody <> nil then
    FRigidBody.Update(Self, SecondsPassed);
end;

procedure T3DTransform.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRigidBody) then
    { set to nil by SetRigidBody to clean nicely }
    RigidBody := nil;
end;

procedure T3DTransform.SetWorld(const Value: T3DWorld);
begin
  if FWorld <> Value then
  begin
    { When removing 3D object from world, remove it also from physics engine world. }
    if (FWorld <> nil) and (RigidBody <> nil) then
      RigidBody.DeinitializeTransform(Self);

    inherited;

    { Calling TRigidBody.InitializeTransform explicitly now
      (instead of letting it happen in TRigidBody.Update)
      makes sure that when T3DWorld.Update will happen,
      the physics engine will know
      the initial position of this object during 1st simulation tick.

      TODO: it's not really nice.
      - If you change a transformation from now to T3DWorld.Update,
        then 1st simulation tick will have invalid transformation anyway.
      - If you change some other rigid body parameter between now and
        next FRigidBody.Update, then next FRigidBody will recreate Kraft
        resources for this object, which is needless (we create Kraft resources
        for this transform 2 times in this case, instead of once). }
    if (FWorld <> nil) and (RigidBody <> nil) then
      RigidBody.InitializeTransform(Self);
  end;
end;

{ T3DWorld ------------------------------------------------------------------- }

procedure T3DWorld.InitializePhysicsEngine;
begin
  if FKraftEngine = nil then
  begin
    FKraftEngine := TKraft.Create(-1);
    //KraftEngine.SetFrequency(120.0); // default is 60
  end;
end;

destructor T3DWorld.Destroy;
begin
  FreeAndNil(FKraftEngine);
  inherited;
end;

procedure T3DWorld.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  PhysicsTimeStep: TFloatTime;
  KraftGravity: TVector3;
begin
  if FKraftEngine <> nil then
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

      { Kraft demo (sandbox) doesn't do this, but I think it's sensible:
        the later line InterpolateWorldTransforms assumes that we calculated
        "one time too many".
        TODO: ask Kraft author about this. }
      FKraftEngine.StoreWorldTransforms;
      FKraftEngine.Step(PhysicsTimeStep);

      FKraftEngine.InterpolateWorldTransforms(TimeAccumulator / PhysicsTimeStep);
    end;
  end;

  { call inherited at the end,
    to update transformation of all items in their TRigidBody.Update
    called from T3DTransform.Update }
  inherited;
end;

{$endif read_implementation}
