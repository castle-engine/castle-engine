{
  Copyright 2012-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastle3D;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastle3D = class(TCastleBaseTestCase)
  strict private
    procedure DoTestWorld(const PrematureFree: boolean);
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
    procedure TestViewVectorsOrthogonal1;
    procedure TestViewVectorsOrthogonal2;
    procedure TestListNotification;
    procedure TestWorldFull;
    procedure TestWorldPrematureFree;
    procedure TestWorldFreeBeforeItem;
  end;

implementation

uses CastleVectors, CastleBoxes, Castle3D, CastleSceneManager, Contnrs, CastleClassUtils,
  CastleTriangles;

{ TMy3D ---------------------------------------------------------------------- }

type
  { Simple invisible 3D axis-aligned box.
    Probably the simplest possible complete T3D descendant implementation,
    to test various T3D methods.
    Default T3D methods implementation takes care of collisions with BoundingBox,
    so all we need to do is to override the BoundingBox method. }
  TMy3D = class(T3D)
  private
    MyBox: TBox3D;
  public
    constructor Create(AOwner: TComponent; const AMyBox: TBox3D); reintroduce;
    function BoundingBox: TBox3D; override;
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

{ Helper box values ---------------------------------------------------------- }

const
  Box0: TBox3D = (Data: ((-1, -1, -1), (1, 1, 1)));
  Box20: TBox3D = (Data: ((19, -1, -1), (21, 1, 1)));

{ TTestCastle3D ---------------------------------------------------------------- }

procedure TTestCastle3D.TestMy3D;
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
    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertFloatsEqual(1, AboveHeight);

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertFloatsEqual(9, Collision.Distance);
    AssertTrue(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.TestMy3DNotExists;
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

    AssertTrue(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.TestMy3DNotCollides;
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

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertFloatsEqual(9, Collision.Distance);
    AssertTrue(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

type
  { Define my own T3DTransform descendant, only to expose OnlyTranslation
    value for testing. }
  TMy3DTransform = class(T3DTransform)
  end;

procedure TTestCastle3D.Test3DTransform;
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
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertFloatsEqual(1, AboveHeight);

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertFloatsEqual(9, Collision.Distance);
    AssertTrue(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.Test3DTransformNotExists;
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
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.Test3DTransformNotCollides;
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
    AssertTrue(M.OnlyTranslation);

    AssertTrue(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    AssertTrue(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 0.3, nil));
    AssertTrue(not M.SphereCollision(Vector3Single(2, 2, 2), 3, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 0.6), nil));
    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(2, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(10, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(10, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertFloatsEqual(9, Collision.Distance);
    AssertTrue(VectorsEqual(Collision.Last.Point, Vector3Single(1, 0, 0)));
    FreeAndNil(Collision);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.Test3DTransformReal;

  { Perform test on M, assuming that it results in box Box20. }
  procedure DoTests(M: TMy3DTransform);
  var
    IsAbove: boolean;
    AboveHeight: Single;
    AboveGround: P3DTriangle;
    NewPos: TVector3Single;
    Collision: TRayCollision;
  begin
    AssertTrue(M.BoundingBox.Equal(Box20));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(not IsAbove);
    AssertTrue(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(20.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    AssertTrue(IsAbove);
    AssertFloatsEqual(1, AboveHeight);

    { wall-sliding with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(VectorsEqual(Vector3Single(18, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    AssertTrue(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));
    AssertTrue(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 3.0), nil));
    AssertTrue(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));

    AssertTrue(not M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(40, 20, 20), nil, false));
    AssertTrue(M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(10, -10, -10), nil, false));

    AssertTrue(not M.SphereCollision(Vector3Single(22, 2, 2), 0.3, nil));
    AssertTrue(M.SphereCollision(Vector3Single(22, 2, 2), 3, nil));
    { below radius values chosen specifically to test that "/ AverageScale"
      inside T3DCustomTransform.SphereCollision works Ok }
    AssertTrue(not M.SphereCollision(Vector3Single(22, 0, 0), 0.9, nil));
    AssertTrue(M.SphereCollision(Vector3Single(22, 0, 0), 1.1, nil));

    AssertTrue(not M.BoxCollision(Box3DAroundPoint(Vector3Single(22, 2, 2), 0.6), nil));
    AssertTrue(M.BoxCollision(Box3DAroundPoint(Vector3Single(22, 2, 2), 6), nil));

    Collision := M.RayCollision(
      Vector3Single(30, 10, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision = nil);

    Collision := M.RayCollision(
      Vector3Single(30, 0, 0), Vector3Single(-1, 0, 0), nil);
    AssertTrue(Collision <> nil);
    AssertFloatsEqual(9, Collision.Distance);
    AssertTrue(VectorsEqual(Collision.Last.Point, Vector3Single(21, 0, 0), 0.001));
    FreeAndNil(Collision);
  end;

var
  M, M2: TMy3DTransform;
begin
  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3Single(20, 0, 0);
    AssertTrue(M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;

  M := TMy3DTransform.Create(nil);
  try
    M.Add(TMy3D.Create(M, Box0));
    M.Translation := Vector3Single(20, 0, 0);
    M.Rotation := Vector4Single(0, 1, 0, Pi / 2);
    AssertTrue(not M.OnlyTranslation);
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
    AssertTrue(not M.OnlyTranslation);
    M2.Add(TMy3D.Create(M, Box0));
    M2.Translation := Vector3Single(20, 0, 0);
    AssertTrue(M2.OnlyTranslation);
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
    AssertTrue(not M.OnlyTranslation);
    DoTests(M);
  finally FreeAndNil(M) end;
end;

procedure TTestCastle3D.TestNotifications;
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

procedure TTestCastle3D.TestNotificationsSceneManager;
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

procedure TTestCastle3D.TestList;
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
      AssertTrue(OList.Count = 1);
      AssertTrue(OList[0] = My2);
    finally FreeAndNil(OList) end;

    { Test that our T3DListCore avoids this bug:
      http://bugs.freepascal.org/view.php?id=21087 }

    List := T3DList.Create(nil);
    try
      List.Add(My);
      List[0] := My2;
      AssertTrue(List.Count = 1);
      AssertTrue(List[0] = My2);
    finally FreeAndNil(List) end;
  finally
    FreeAndNil(My);
    FreeAndNil(My2);
  end;
end;

procedure TTestCastle3D.TestViewVectorsOrthogonal1;
{ Test forcing Direction/Up orthogonal by various T3DOrient routines
  (that actually implement it by TWalkCamera routines).
  This tests doesn't pass dir/up parallel. }
var
  O: T3DOrient;
begin
  O := T3DOrient.Create(nil);

  { no need to change direction/up angle, only normalize them }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(0, 0, 1), O.Up, 0.1);

  O.SetView(Vector3Single(0, 0, 0), Vector3Single(10, 0, 0), Vector3Single(0, 0, 10));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(0, 0, 1), O.Up, 0.1);

  { SetView corrects up vector angle }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(10, 0, 0), Vector3Single(10, 0, 10));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(0, 0, 1), O.Up, 0.1);

  { SetView with AdjustUp = false corrects direction vector angle }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(10, 0, 0), Vector3Single(10, 0, 10), false);
  AssertVectorsEqual(Normalized(Vector3Single(Sqrt(2), 0, -Sqrt(2))), O.Direction, 0.1);
  AssertVectorsEqual(Normalized(Vector3Single(Sqrt(2), 0, Sqrt(2))), O.Up, 0.1);

  { Setting direction corrects up vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.Direction := Vector3Single(10, 0, 10);
  AssertVectorsEqual(Normalized(Vector3Single(Sqrt(2), 0, Sqrt(2))), O.Direction, 0.1);
  AssertVectorsEqual(Normalized(Vector3Single(-Sqrt(2), 0, Sqrt(2))), O.Up, 0.1);

  { Setting up corrects direction vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.Up := Vector3Single(10, 0, 10);
  AssertVectorsEqual(Normalized(Vector3Single(Sqrt(2), 0, -Sqrt(2))), O.Direction, 0.1);
  AssertVectorsEqual(Normalized(Vector3Single(Sqrt(2), 0, Sqrt(2))), O.Up, 0.1);

  { UpPrefer corrects up vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.UpPrefer(Vector3Single(10, 0, 10));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(0, 0, 1), O.Up, 0.1);

  FreeAndNil(O);
end;

procedure TTestCastle3D.TestViewVectorsOrthogonal2;
{ Test forcing Direction/Up orthogonal by various T3DOrient routines
  (that actually implement it by TWalkCamera routines).
  This tests does pass dir/up parallel. }
var
  O: T3DOrient;
begin
  O := T3DOrient.Create(nil);

  { SetView corrects up vector angle }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(10, 0, 0), Vector3Single(10, 0, 0));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(AnyOrthogonalVector(Vector3Single(1, 0, 0)), O.Up, 0.1);

  { SetView with AdjustUp = false corrects direction vector angle }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(10, 0, 0), Vector3Single(10, 0, 0), false);
  AssertVectorsEqual(AnyOrthogonalVector(Vector3Single(1, 0, 0)), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Up, 0.1);

  { Setting direction corrects up vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.Direction := Vector3Single(0, 0, 10);
  AssertVectorsEqual(Vector3Single(0, 0, 1), O.Direction, 0.1);
  AssertVectorsEqual(AnyOrthogonalVector(Vector3Single(0, 0, 1)), O.Up, 0.1);

  { Setting up corrects direction vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.Up := Vector3Single(10, 0, 0);
  AssertVectorsEqual(AnyOrthogonalVector(Vector3Single(1, 0, 0)), O.Direction, 0.1);
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Up, 0.1);

  { UpPrefer corrects up vector }
  O.SetView(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0), Vector3Single(0, 0, 1));
  O.UpPrefer(Vector3Single(10, 0, 0));
  AssertVectorsEqual(Vector3Single(1, 0, 0), O.Direction, 0.1);
  AssertVectorsEqual(AnyOrthogonalVector(Vector3Single(1, 0, 0)), O.Up, 0.1);

  FreeAndNil(O);
end;

procedure TTestCastle3D.TestListNotification;
var
  O1List: T3DList;
  O1: T3D;
begin
  {$warnings off} { don't warn about creating with abstract methods here }
  O1 := T3D.Create(nil); O1.Name := 'O1';
  {$warnings on}
  O1List := T3DList.Create(nil); O1List.Name := 'O1List';

  AssertTrue(O1List.Count = 0);
  O1List.Add(O1);
  AssertTrue(O1List.Count = 1);
  FreeAndNil(O1); // freeing O1 should also remove it from O1List automatically
  AssertTrue(O1List.Count = 0);
  FreeAndNil(O1List);
end;

procedure TTestCastle3D.TestWorldFull;
begin
  DoTestWorld(false);
end;

procedure TTestCastle3D.TestWorldPrematureFree;
begin
  DoTestWorld(true);
end;

procedure TTestCastle3D.DoTestWorld(const PrematureFree: boolean);
var
  World1, World2: T3DWorld;
  O1List, O2List: T3DList;
  O1, O2: T3D;
begin
  World1 := nil;
  World2 := nil;
  try
    {$warnings off} { don't warn about creating with abstract methods here }
    World1 := T3DWorld.Create(nil); World1.Name := 'World1';
    World2 := T3DWorld.Create(nil); World2.Name := 'World2';
    O1 := T3D.Create(World1); O1.Name := 'O1';
    O2 := T3D.Create(World1); O2.Name := 'O2';
    {$warnings on}

    O1List := T3DList.Create(World1); O1List.Name := 'O1List';
    O2List := T3DList.Create(World1); O2List.Name := 'O2List';

    AssertTrue(World1 = World1.World);
    AssertTrue(World2 = World2.World);
    AssertTrue(nil = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(nil = O1List.World);
    AssertTrue(nil = O2List.World);

    World1.Add(O1List);
    World1.Add(O1); // now O1 is present in World1 1 time
    AssertTrue(World1.Count = 2);

    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    O1List.Add(O1); // now O1 is present in World1 2 times

    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    O1List.Add(O1); // now O1 is present in World1 3 times

    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    O1List.Remove(O1); // now O1 is present in World1 2 times

    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    if PrematureFree then
      Exit; // test freeing now

    World1.Remove(O1); // now O1 is present in World1 1 time
    AssertTrue(World1.Count = 1);

    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    O1List.Remove(O1); // now O1 is not present in World1

    //AssertTrue(nil = O1.World); // for now, we don't unassign O1.World yet
    AssertTrue(World1 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    World2.Add(O1);
    AssertTrue(World2.Count = 1);

    AssertTrue(World2 = O1.World);
    AssertTrue(nil = O2.World);
    AssertTrue(World1 = O1List.World);
    AssertTrue(nil = O2List.World);

    { these are incorrect (you should remove object from previous world first),
      but are harmless in practice now. }

    // try
      World1.Add(O1);
      AssertTrue(World1.Count = 2);
    //   raise Exception.Create('Adding T3D to different World should not be possible');
    // except on E: ECannotAddToAnotherWorld do ; end;

    // try
      World2.Add(O1List);
      AssertTrue(World2.Count = 2);
    //   raise Exception.Create('Adding T3D to different World should not be possible');
    // except on E: ECannotAddToAnotherWorld do ; end;
  finally
    FreeAndNil(World1);
    { freeing World1 also frees the things it owned -- which includes
      O1 and O1List within World2. }
    AssertTrue('World2.Count = 0 at the end', World2.Count = 0);
    FreeAndNil(World2);
  end;
end;

procedure TTestCastle3D.TestWorldFreeBeforeItem;
var
  World1: T3DWorld;
  O1List: T3DList;
  O1: T3D;
begin
  {$warnings off} { don't warn about creating with abstract methods here }
  World1 := T3DWorld.Create(nil); World1.Name := 'World1';
  O1 := T3D.Create(nil); O1.Name := 'O1';
  {$warnings on}
  O1List := T3DList.Create(nil); O1List.Name := 'O1List';

  Assert(World1 = World1.World);
  Assert(nil = O1.World);
  Assert(nil = O1List.World);

  World1.Add(O1List);
  O1List.Add(O1);

  Assert(World1 = World1.World);
  Assert(World1 = O1.World);
  Assert(World1 = O1List.World);
  Assert(World1.Count = 1);
  Assert(O1List.Count = 1);

  World1.Add(O1); // now O1 is present in World1 2 times

  Assert(World1 = World1.World);
  Assert(World1 = O1.World);
  Assert(World1 = O1List.World);
  Assert(World1.Count = 2);
  Assert(O1List.Count = 1);

  FreeAndNil(World1);

  Assert(nil = O1.World);
  Assert(nil = O1List.World);
  Assert(O1List.Count = 1);

  FreeAndNil(O1);

  Assert(nil = O1List.World);
  Assert(O1List.Count = 0);

  FreeAndNil(O1List);
end;

initialization
  RegisterTest(TTestCastle3D);
end.
