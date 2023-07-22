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

{ Deprecated physics. }

{$ifdef read_interface}
  { Deprecated abstract collider used with deprecated rigid body @link(TRigidBody). }
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
      @link(TCastleRigidBody.Collider) property and
      @link(TCollider.Parent) property. }
    constructor Create(const AParent: TRigidBody);
    destructor Destroy; override;

    { Parent rigid body, which in turn refers to this collider
      by @link(TCastleRigidBody.Collider). }
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

    { How elastic are collisions with this body,
      that is how much collider "bounces off" this body.

      It generally ranges from 0 (perfectly non-elastic) to 1
      (perfectly elastic). It is almost always less than 1,
      since the initial kinetic energy gets lost at collision.
      See https://en.wikipedia.org/wiki/Coefficient_of_restitution
      for more discussion. }
    property Restitution: Single read FRestitution write FRestitution
      {$ifdef FPC}default DefaultRestitution{$endif};

    property Friction: Single read FFriction write FFriction
      {$ifdef FPC}default DefaultFriction{$endif};
  end deprecated 'use TCastleCollider';

  { Collide as an infinite plane.
    Place this inside @link(TCastleRigidBody.Collider) property. }
  TPlaneCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Normal: TVector3;
    Distance: Single;
  end deprecated 'use TCastlePlaneCollider';

  { Collide as a box.
    Place this inside @link(TCastleRigidBody.Collider) property. }
  TBoxCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Size: TVector3;
  end deprecated 'use TCastleBoxCollider';

  { Collide as a sphere.
    Place this inside @link(TCastleRigidBody.Collider) property. }
  TSphereCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
  end deprecated 'use TCastleSphereCollider';

  { Collide as a capsule.
    Place this inside @link(TCastleRigidBody.Collider) property. }
  TCapsuleCollider = class(TCollider)
  strict protected
    function CreateKraftShape(const APhysics: TKraft;
      const ARigidBody: TKraftRigidBody): TKraftShape; override;
  public
    Radius: Single;
    Height: Single;
  end deprecated 'use TCastleCapsuleCollider';

  TRigidBody = class(TCastleRigidBody)
  end deprecated 'use TCastleRigidBody';

  TOnCollision = TCollisionEvent deprecated 'use TCollisionEvent';

{$endif read_interface}

{$ifdef read_implementation}

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

  if (Size.X <= 0) or
     (Size.Y <= 0) or
     (Size.Z <= 0) then
    raise EPhysicsError.Create('You must assign positive Size to TBoxCollider');
  Result := TKraftShapeBox.Create(APhysics, ARigidBody, VectorToKraft(Size / 2));
end;

{ TSphereCollider ------------------------------------------------------------ }

function TSphereCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeSphere.Create(APhysics, ARigidBody, Radius);
end;

{ TCapsuleCollider ----------------------------------------------------------- }

function TCapsuleCollider.CreateKraftShape(const APhysics: TKraft;
  const ARigidBody: TKraftRigidBody): TKraftShape;
begin
  Result := TKraftShapeCapsule.Create(APhysics, ARigidBody, Radius, Height);
end;

{$endif read_implementation}
