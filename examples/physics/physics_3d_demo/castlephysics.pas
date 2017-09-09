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

{ Physics classes. }
unit CastlePhysics;

interface

uses Classes, Generics.Collections, Kraft,
  CastleVectors, Castle3D, CastleSceneManager, CastleTimeUtils;

type
  { Shape used for collision detection of a rigid body,
    placed in @link(TRigidBody.Collider) property. }
  TCollider = class(TComponent)
  private
    FKraft: TKraftShape;
    procedure InitializeKraft(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody);
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; virtual; abstract;
  public
    destructor Destroy; override;
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

  { Is the object moved/rotated by the physics engine, or is it only
    a static collider. See @link(TRigidBody.RigidBodyType). }
  TRigidBodyType = (
    { Physics simulation moves and rotates this object
      (because of gravity, or because it collides with others).

      Do not change the @link(T3DTransform.Translation) and other
      transformation properties of the related TPhysicsTransform
      after assigning @link(TPhysicsTransform.RigidBody) that is dynamic.
      They are under the control of the physics
      engine. You can still reliably read them.
    }
    rbDynamic,

    { This object is not transformed by the physics simulation,
      but it still collides with other physical objects.

      You can change the @link(T3DTransform.Translation) and other
      transformation properties of the related TPhysicsTransform.
      But it is like destroying it and creating a new one for the physics engine,
      @italic(so do change them often, e.g. every frame).
      Use rbKinematic if you animate some object, and it should still be a collider
      for the physics engine. }
    rbStatic,

    { This object is not transformed by the physics simulation,
      but it still collides with other physical objects.

      You can change the @link(T3DTransform.Translation) and other
      transformation properties of the related TPhysicsTransform.
      The physics engine is prepared that this object can move often,
      e.g. every frame, so you can animate the related @link(TPhysicsTransform)
      using any technique. }
    rbKinematic
  );

  T3DCoord = 0..2;
  T3DCoords = set of T3DCoord;

  { Rigid body properties for the physics engine,
    see @link(TPhysicsTransform.RigidBody).

    TODO: For now all the properties of this class,
    including the Collider and all properties of Collider,
    must be assigned before setting TPhysicsTransform.RigidBody . }
  TRigidBody = class(TComponent)
  private
    FKraft: TKraftRigidBody;
    FRigidBodyType: TRigidBodyType;
    FCollider: TCollider;
    FGravity: boolean;
    FMass: Single;
    FLockPosition: T3DCoords;
    FLockRotation: T3DCoords;
    FAngularVelocity: TVector3;
    FLinearVelocity: TVector3;
    procedure InitializeKraft(const APhysics: TKraft);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Utility function to set common values for physics in 2D games.
      Locks moving along the Z axis,
      locks rotating along the X and Y axes. }
    procedure Setup2D;

    property AngularVelocity: TVector3 read FAngularVelocity write FAngularVelocity;
    property LinearVelocity: TVector3 read FLinearVelocity write FLinearVelocity;
  published
    { Is the object moved/rotated by the physics engine, or is it only
      a static collider. }
    property RigidBodyType: TRigidBodyType
      read FRigidBodyType write FRigidBodyType default rbDynamic;

    { Shape used for collisions with this object. }
    property Collider: TCollider read FCollider write FCollider;

    { Is this object affected by gravity. }
    property Gravity: boolean read FGravity write FGravity default true;

    { Mass in kg.
      TODO: should have non-zero default?
      TODO: better to use Density, Kraft does this?
      TODO: not applied yet to Kraft yet. }
    property Mass: Single read FMass write FMass;

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

  { T3DTransform that may be controlled by physics,
    or at least collide with other physical objects.
    See @link(RigidBody). }
  TPhysicsTransform = class(T3DTransform)
  strict private
    FRigidBody: TRigidBody;
    procedure SetRigidBody(const Value: TRigidBody);
  private
    { TODO: FKraftPhysics should be part of T3DWorld, not static here. }
    FKraftPhysics: TKraft; static;
  public
    { Participate in rigid body physics simulation.
      This makes this object collidable with other rigid bodies
      (if @link(TRigidBody.Collider) is assigned)
      and it allows to move and rotate because of gravity
      or because of collisions with other objects
      (if @link(TRigidBody.Static) is @false).

      Setting this makes this object a single rigid body for the physics engine.

      If this property is assigned and the @link(TRigidBody.Static) is @false
      then this object is moved and rotated using the physics engine.
      It will move because of gravity (if @link(TRigidBody.Gravity)),
      and because of collisions with other objects.

      Usually you want to also assign a @link(TRigidBody.Collider) instance.
      Otherwise, the rigid body will not collide with anything,
      and (if not @link(TRigidBody.Static)) it will simply fall down because of gravity.

      @bold(This is an experimental API, subject to change in future releases.)

      @bold(Our engine (for now) also has internal, simple physics simulation,
      used to perform collisions with player, creatures, and optional gravity.)
      These two physics systems are independent, and use separate properties for now.

      @unorderedList(
        @item(@link(T3D.Collides) property has no effect on whether this
          object is collidable for the physics engine.
          The @link(T3D.Collides) only determines whether
          the object is collidable for our internal collision detection
          (that doesn't use physics engine) which is used for collisions with
          player (camera), creatures, and for our internal gravity (@link(T3DTransform.Gravity)).
        )

        @item(@link(T3DTransform.Gravity) property has no effect on whether this
          object is affected by gravity simulation of the physics engine.
          The physics engine is controlled by independent @link(TRigidBody.Gravity) property,
          which is @true by default (while the @link(T3DTransform.Gravity) is @false
          by default).

          It doesn't make sense to set @link(T3DTransform.Gravity) to @true
          if also use @link(TRigidBody.Gravity), it would mean that
          @italic(gravity simulation is performed twice).
          In the future, @link(T3DTransform.Gravity) may be removed
          (or merged with @link(TRigidBody.Gravity), but then old games
          may by surprised by a new default @true.)
        )

        @item(The collider used by our internal, simple physics is independent
          from the @link(TRigidBody.Collider).
          The internal, simple physics uses colliders implicitly implemented
          in the overridden @link(T3D.HeightCollision),
          @link(T3D.MoveCollision) and friends.
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
    }
    property RigidBody: TRigidBody read FRigidBody write SetRigidBody;

    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TPhysicsTransformList = specialize TObjectList<TPhysicsTransform>;

  TPhysicsSceneManager = class(TCastleSceneManager)
  private
    WasPhysicsStep: boolean;
    TimeAccumulator: TFloatTime;
  public
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
  end;

implementation

uses SysUtils, Math,
  CastleUtils;

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

procedure TCollider.InitializeKraft(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody);
begin
  // FreeAndNil(FKraft); // do not free here, TKraftShape is owned by TKraftRigidBody
  FKraft := CreateKraftShape(APhysics, ARigidBody);
end;

destructor TCollider.Destroy;
begin
  // FreeAndNil(FKraft); // do not free here, TKraftShape is owned by TKraftRigidBody
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
end;

destructor TRigidBody.Destroy;
begin
  FreeAndNil(FKraft);
  FreeAndNil(FCollider);
  inherited;
end;

procedure TRigidBody.InitializeKraft(const APhysics: TKraft);
begin
  FreeAndNil(FKraft);
  FKraft := TKraftRigidBody.Create(APhysics);
  case RigidBodyType of
    rbDynamic  : FKraft.SetRigidBodyType(krbtDynamic);
    rbStatic   : FKraft.SetRigidBodyType(krbtStatic);
    rbKinematic: FKraft.SetRigidBodyType(krbtKinematic);
    else raise EInternalError.Create('TRigidBody.InitializeKraft:RigidBodyType?');
  end;
  Collider.InitializeKraft(APhysics, FKraft);
  FKraft.GravityScale := Iff(Gravity, 1.0, 0.0);
  if 0 in FLockRotation then FKraft.Flags := FKraft.Flags + [krbfLockAxisX];
  if 1 in FLockRotation then FKraft.Flags := FKraft.Flags + [krbfLockAxisY];
  if 2 in FLockRotation then FKraft.Flags := FKraft.Flags + [krbfLockAxisZ];
  FKraft.Finish;

  FKraft.CollisionGroups:=[0]; // TODO: make this configurable
  if (not AngularVelocity.IsPerfectlyZero) or
     (not LinearVelocity.IsPerfectlyZero) then
  begin
    FKraft.AngularVelocity := VectorToKraft(AngularVelocity);
    FKraft.LinearVelocity := VectorToKraft(LinearVelocity);
    FKraft.SetToAwake;
  end;
end;

procedure TRigidBody.Setup2D;
begin
  LockPosition := [2];
  LockRotation := [0, 1];
end;

{ TPhysicsTransform ---------------------------------------------------------- }

procedure TPhysicsTransform.SetRigidBody(const Value: TRigidBody);

  { TODO: initalizing FKraftPhysics should not be done here,
    but in T3DWorld or TCastleSceneManager. }
  procedure CreatePhysics;
  begin
    FKraftPhysics := TKraft.Create(-1);
    //KraftPhysics.SetFrequency(120.0); // default is 60
  end;

begin
  if FRigidBody <> Value then
  begin
    FRigidBody := Value;
    if FKraftPhysics = nil then
      CreatePhysics;
    FRigidBody.InitializeKraft(FKraftPhysics);
    // TODO: this assumes that this object is not further transformed by parents
    FRigidBody.FKraft.SetWorldTransformation(MatrixToKraft(Transform));
  end;
end;

destructor TPhysicsTransform.Destroy;
begin
  FreeAndNil(FRigidBody);
  inherited;
end;

procedure TPhysicsTransform.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);

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
    Body := FRigidBody.FKraft;
    Shape := FRigidBody.FCollider.FKraft;

    Q := QuaternionFromMatrix4x4(Shape.InterpolatedWorldTransform);
    QuaternionToAxisAngle(Q, Axis, Angle);
    Rotation := Vector4(Axis.X, Axis.Y, Axis.Z, Angle);

    NewPos := Shape.GetCenter(Shape.InterpolatedWorldTransform);
    if FRigidBody.LockPosition <> [] then
    begin
      { TODO: Kraft feature request for [LockPositionX / Y / Z]? }
      // apply LockPosition to fix some NewPos coords
      if 0 in FRigidBody.LockPosition then NewPos.X := Translation[0];
      if 1 in FRigidBody.LockPosition then NewPos.Y := Translation[1];
      if 2 in FRigidBody.LockPosition then NewPos.Z := Translation[2];
    end;
    Translation := VectorFromKraft(NewPos);
    if FRigidBody.LockPosition <> [] then
      // fix also position at Kraft side after fixing by LockPosition
      Body.SetWorldTransformation(MatrixToKraft(Transform));
  end;

begin
  inherited;

  if FRigidBody <> nil then
  begin
    case FRigidBody.RigidBodyType of
      rbDynamic: TransformationFromKraft;
      rbKinematic:
        begin
          // TODO: check "if TransformChanged then" or such, don't do this every frame
          // TODO: this assumes that this object is not further transformed by parents
          FRigidBody.FKraft.SetWorldTransformation(MatrixToKraft(Transform));
        end;
      // TODO: do above also for rbStatic, once "if TransformChanged then" implemented
    end;
  end;
end;

{ TPhysicsSceneManager ------------------------------------------------------- }

procedure TPhysicsSceneManager.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  KraftPhysics: TKraft;
  PhysicsTimeStep: TFloatTime;
  KraftGravity: TVector3;
begin
  if (not Paused) and GetExists then
  begin
    // TODO: KraftPhysics will not be stored in TPhysicsTransform
    KraftPhysics := TPhysicsTransform.FKraftPhysics;

    if KraftPhysics <> nil then
    begin
      // update KraftPhysics.Gravity
      // (hm, do we really need to be prepared that it changes each frame?)
      KraftGravity := -GravityUp * 9.81;
      KraftPhysics.Gravity.Vector := VectorToKraft(KraftGravity);

      if not WasPhysicsStep then
      begin
        KraftPhysics.StoreWorldTransforms;
        KraftPhysics.InterpolateWorldTransforms(0.0);
        WasPhysicsStep := true;
      end else
      begin
        PhysicsTimeStep := 1.0 / KraftPhysics.WorldFrequency;
        TimeAccumulator := TimeAccumulator + SecondsPassed * TimeScale;
        while TimeAccumulator>=PhysicsTimeStep do
        begin
          TimeAccumulator := TimeAccumulator - PhysicsTimeStep;
          KraftPhysics.StoreWorldTransforms;
          KraftPhysics.Step(PhysicsTimeStep);
        end;

        { Kraft demo (sandbox) doesn't do this, but I think it's sensible:
          the later line InterpolateWorldTransforms assumes that we calculated
          "one time too many".
          TODO: ask Kraft author about this. }
        KraftPhysics.StoreWorldTransforms;
        KraftPhysics.Step(PhysicsTimeStep);

        KraftPhysics.InterpolateWorldTransforms(TimeAccumulator / PhysicsTimeStep);
      end;
    end;
  end;

  { call inherited at the end,
    to update transformation of all items in their TPhysicsTransform.Update }
  inherited;
end;

destructor TPhysicsSceneManager.Destroy;
begin
  FreeAndNil(TPhysicsTransform.FKraftPhysics);
  inherited;
end;

end.
