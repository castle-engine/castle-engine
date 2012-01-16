{
  Copyright 2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestBase3D;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestBase3D = class(TTestCase)
  published
    procedure TestMy3D;
    procedure TestMy3DNotExists;
    procedure TestMy3DNotCollides;
    procedure Test3DTransform;
    procedure Test3DTransformNotExists;
    procedure Test3DTransformNotCollides;
    procedure Test3DTransformReal;
    procedure TestNotifications;
    procedure TestNotificationsSceneManager;
    procedure TestList;
  end;

implementation

uses VectorMath, Boxes3D, Base3D, CastleSceneManager, Contnrs, CastleClassUtils;

{ TMy3D ---------------------------------------------------------------------- }

type
  { Simple invisible 3D axis-aligned box.
    Probably the simplest possible complete T3D descendant implementation,
    to test various T3D methods. }
  TMy3D = class(T3D)
  private
    MyBox: TBox3D;
  public
    constructor Create(AOwner: TComponent; const AMyBox: TBox3D); reintroduce;

    function BoundingBox: TBox3D; override;
    procedure GetHeightAbove(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: P3DTriangle); override;
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(const RayOrigin, RayDirection: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision; override;
  end;

constructor TMy3D.Create(AOwner: TComponent; const AMyBox: TBox3D);
begin
  inherited Create(AOwner);
  MyBox := AMyBox;
end;

function TMy3D.BoundingBox: TBox3D;
begin
  { Note that we don't check Collides property in BoundingBox method.
    BoundingBox must take into account also non-collidable
    (but visible) 3D object parts. (Well, in the case of this trivial
    3D object, it's actually never visible, so it's a little pointless...
    But in general, the rule is "BoundingBox looks at GetExists,
    not at Collides property".) }
  if GetExists then
    Result := MyBox else
    Result := EmptyBox3D;
end;

procedure TMy3D.GetHeightAbove(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: P3DTriangle);
var
  Intersection: TVector3Single;
  IntersectionDistance: Single;
begin
  inherited;
  if GetExists and Collides and
    MyBox.TryRayEntrance(Intersection, IntersectionDistance, Position, -GravityUp) then
  begin
    IsAbove := true;
    AboveHeight := IntersectionDistance;
    // AboveGround := ... leave it as nil for this simple 3D object
  end;
end;

function TMy3D.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  { A simple implementation of MoveAllowed is just to call MoveAllowedSimple.
    This means that we don't do wall-sliding. }
  Result := MoveAllowedSimple(OldPos, ProposedNewPos, CameraRadius,
    TrianglesToIgnoreFunc);
  if Result then
    NewPos := ProposedNewPos;
end;

function TMy3D.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := not (
    GetExists and
    Collides and
    ( MyBox.IsSegmentCollision(OldPos, ProposedNewPos) or
      MyBox.SphereCollision(ProposedNewPos, CameraRadius) )
  );
end;

function TMy3D.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := not (
    GetExists and
    Collides and
    ( MyBox.IsSegmentCollision(OldPos, ProposedNewPos) or
      MyBox.Collision(ProposedNewBox) )
  );
end;

function TMy3D.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetExists and Collides and MyBox.IsSegmentCollision(Pos1, Pos2);
end;

function TMy3D.SphereCollision(const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetExists and Collides and MyBox.SphereCollision(Pos, Radius);
end;

function TMy3D.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := GetExists and Collides and MyBox.Collision(Box);
end;

function TMy3D.RayCollision(const RayOrigin, RayDirection: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  Intersection: TVector3Single;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
begin
  if GetExists and
    MyBox.TryRayEntrance(Intersection, IntersectionDistance, RayOrigin, RayDirection) then
  begin
    Result := TRayCollision.Create;
    Result.Distance := IntersectionDistance;

    NewNode := Result.Add;
    NewNode^.Item := Self;
    NewNode^.Point := Intersection;
    NewNode^.RayOrigin := RayOrigin;
    NewNode^.RayDirection := RayDirection;
    { real T3D implementation could assign here something nice to NewNode^.Triangle,
      to inform T3D.PointingDeviceMove/Activate about the intersected material. }
    NewNode^.Triangle := nil;
  end else
    Result := nil;
end;

{ Helper box values ---------------------------------------------------------- }

const
  Box0: TBox3D = (Data: ((-1, -1, -1), (1, 1, 1)));
  Box20: TBox3D = (Data: ((19, -1, -1), (21, 1, 1)));

{ TTestBase3D ---------------------------------------------------------------- }

procedure TTestBase3D.TestMy3D;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    Assert(M.BoundingBox.Equal(Box0));

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(not M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(not M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));
    Assert(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    Assert(not M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(not M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision <> nil);
    Assert(FloatsEqual(Collision.Distance, 9));
    Assert(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.TestMy3DNotExists;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    M.Exists := false;

    Assert(M.BoundingBox.IsEmpty);

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.TestMy3DNotCollides;
var
  M: TMy3D;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3D.Create(nil, Box0);
  try
    M.Collides := false;

    Assert(M.BoundingBox.Equal(Box0));

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision <> nil);
    Assert(FloatsEqual(Collision.Distance, 9));
    Assert(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

type
  { Define my own T3DTransform descendant, only to expose OnlyTranslation
    value for testing. }
  TMy3DTransform = class(T3DTransform)
  end;

procedure TTestBase3D.Test3DTransform;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.Equal(Box0));

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(not M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(not M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));
    Assert(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    Assert(not M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(not M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision <> nil);
    Assert(FloatsEqual(Collision.Distance, 9));
    Assert(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.Test3DTransformNotExists;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Exists := false;
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.IsEmpty);

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.Test3DTransformNotCollides;
var
  M: TMy3DTransform;
  IsAbove: boolean;
  AboveHeight: Single;
  AboveGround: P3DTriangle;
  NewPos: TVector3Single;
  Collision: TRayCollision;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Collides := false;
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.Equal(Box0));

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      0.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      0.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      0.5, nil));
    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      0.5, nil));

    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    Assert(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision <> nil);
    Assert(FloatsEqual(Collision.Distance, 9));
    Assert(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.Test3DTransformReal;

  { Perform test on M, assuming that it results in box Box20. }
  procedure DoTests(M: TMy3DTransform);
  var
    IsAbove: boolean;
    AboveHeight: Single;
    AboveGround: P3DTriangle;
    NewPos: TVector3Single;
    Collision: TRayCollision;
  begin
    Assert(M.BoundingBox.Equal(Box20));

    M.GetHeightAbove(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    M.GetHeightAbove(Vector3Single(20.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, IsAbove, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    Assert(not M.MoveAllowed(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0), NewPos,
      0.5, nil));
    Assert(not M.MoveAllowed(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      1.5, nil));
    Assert(M.MoveAllowed(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      0.5, nil));
    Assert(VectorsEqual(Vector3Single(18, -1.5, 0), NewPos));

    Assert(not M.MoveAllowedSimple(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      0.5, nil));
    Assert(not M.MoveAllowedSimple(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      1.5, nil));

    Assert(M.MoveAllowedSimple(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      0.5, nil));

    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));
    Assert(not M.MoveBoxAllowedSimple(
      Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      Box3DAroundPoint(Vector3Single(22, 2, 0), 3.0), nil));
    Assert(M.MoveBoxAllowedSimple(
      Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(40, 20, 20), nil));
    Assert(M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(10, -10, -10), nil));

    Assert(not M.SphereCollision(Vector3Single(22, 2, 2), 0.3, nil));
    Assert(M.SphereCollision(Vector3Single(22, 2, 2), 3, nil));
    { below radius values chosen specifically to test that "/ AverageScale"
      inside T3DCustomTransform.SphereCollision works Ok }
    Assert(not M.SphereCollision(Vector3Single(22, 0, 0), 0.9, nil));
    Assert(M.SphereCollision(Vector3Single(22, 0, 0), 1.1, nil));

    Assert(not M.BoxCollision(Box3DAroundPoint(Vector3Single(22, 2, 2), 0.6), nil));
    Assert(M.BoxCollision(Box3DAroundPoint(Vector3Single(22, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(30, 10, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(30, 0, 0), Vector3Single(-1, 0, 0), nil);
    Assert(Collision <> nil);
    Assert(FloatsEqual(Collision.Distance, 9));
    Assert(VectorsEqual(Collision.Last.Point, Vector3Single(21, 0, 0), 0.001));
    FreeAndNil(Collision);
  end;

var
  M, M2: TMy3DTransform;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3Single(20, 0, 0);
    Assert(M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;

  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3Single(20, 0, 0);
    M.Rotation := Vector4Single(0, 1, 0, Pi / 2);
    Assert(not M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;

  M := nil;
  M2 := nil;
  try
    M := TMy3DTransform.Create(nil);
    M2 := TMy3DTransform.Create(nil);

    { test rotation and translation in different order.
      This requires connecting 2 T3DTransform instances, and using Center. }
    M.Add(M2);
    M.Rotation := Vector4Single(0, 1, 0, Pi / 2);
    M.Center := Vector3Single(20, 0, 0);
    Assert(not M.OnlyTranslation);
    M2.Add(TMy3D.Create(M, Box0));
    M2.Translation := Vector3Single(20, 0, 0);
    Assert(M2.OnlyTranslation);
    DoTests(M);
  finally
    FreeAndNil(M);
    FreeAndNil(M2);
  end;

  M := TMy3DTransform.Create(nil);
  try
    { use scaling in T3DTransform }
    M.Add(TMy3D.Create(M, Box3D(
      Vector3Single(-0.25, -0.25, -0.25),
      Vector3Single( 0.25,  0.25,  0.25))));
    M.Translation := Vector3Single(20, 0, 0);
    M.Scale := Vector3Single(4, 4, 4);
    Assert(not M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;
end;

procedure TTestBase3D.TestNotifications;
var
  ListParent, List: T3DList;
  ItemOnList: T3DTransform;
begin
  ListParent := T3DList.Create(nil);
  try
    List := T3DList.Create(ListParent);
    try
      ListParent.Add(List);

      ItemOnList := T3DTransform.Create(ListParent);
      List.Add(ItemOnList);

      { now this will cause T3DList.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(ListParent) end;

  ListParent := T3DList.Create(nil);
  try
    List := T3DList.Create(ListParent);
    try
      ListParent.Add(List);

      ItemOnList := T3DTransform.Create(List);
      List.Add(ItemOnList);

      { now this will cause T3DList.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(ListParent) end;
end;

procedure TTestBase3D.TestNotificationsSceneManager;
var
  SceneManager: TCastleSceneManager;
  List: T3DList;
  ItemOnList: T3DTransform;
begin
  SceneManager := TCastleSceneManager.Create(nil);
  try
    List := T3DList.Create(SceneManager);
    try
      SceneManager.Items.Add(List);

      ItemOnList := T3DTransform.Create(SceneManager);
      List.Add(ItemOnList);

      { now this will cause T3DList.Notification with Owner=Self, and List=nil }
      FreeAndNil(ItemOnList);

    finally FreeAndNil(List) end;
  finally FreeAndNil(SceneManager) end;
end;

procedure TTestBase3D.TestList;
var
  My, My2: T3D;
  List: T3DList;
  OList: TCastleObjectList;
begin
  My  := TMy3D.Create(nil, Box0);
  My2 := TMy3D.Create(nil, Box0);
  try
    OList := TCastleObjectList.Create(false);
    try
      OList.Add(My);
      OList[0] := My2;
      Assert(OList.Count = 1);
      Assert(OList[0] = My2);
    finally FreeAndNil(OList) end;

    { Test that our T3DListCore avoids this bug:
      http://bugs.freepascal.org/view.php?id=21087 }

    List := T3DList.Create(nil);
    try
      List.Add(My);
      List[0] := My2;
      Assert(List.Count = 1);
      Assert(List[0] = My2);
    finally FreeAndNil(List) end;
  finally
    FreeAndNil(My);
    FreeAndNil(My2);
  end;
end;

initialization
  RegisterTest(TTestBase3D);
end.
