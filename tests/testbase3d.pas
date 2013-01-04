{
  Copyright 2012-2012 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestCastle3D = class(TTestCase)
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
    Assert(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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

    Assert(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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

    Assert(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(VectorsEqual(Vector3Single(-2, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(not M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.IsEmpty);

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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
    Assert(M.OnlyTranslation);

    Assert(M.BoundingBox.Equal(Box0));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    { wall-sliding with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { no wall-sliding, with sphere }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(2, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(-2, -2, 0), Vector3Single(-2, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(2, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(20, 20, 20), nil, false));
    Assert(not M.SegmentCollision(Vector3Single(10, 10, 10), Vector3Single(-10, -10, -10), nil, false));

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
    Assert(M.BoundingBox.Equal(Box20));

    IsAbove := M.HeightCollision(Vector3Single(0.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(10.5, 10.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(not IsAbove);
    Assert(AboveHeight = Single(MaxSingle));

    IsAbove := M.HeightCollision(Vector3Single(20.5, 0.5, 2), Vector3Single(0, 0, 1),
      nil, AboveHeight, AboveGround);
    Assert(IsAbove);
    Assert(FloatsEqual(AboveHeight, 1));

    { wall-sliding with sphere }
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0), NewPos,
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(VectorsEqual(Vector3Single(18, -1.5, 0), NewPos));

    { no wall-sliding, with sphere }
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      true, 1.5, EmptyBox3D, EmptyBox3D, nil));
    Assert(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      true, 0.5, EmptyBox3D, EmptyBox3D, nil));

    { with box }
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(22, 2, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));
    Assert(not M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 3.0), nil));
    Assert(M.MoveCollision(Vector3Single(18, -2, 0), Vector3Single(18, -1.5, 0),
      false, 0, EmptyBox3D, Box3DAroundPoint(Vector3Single(22, 2, 0), 1.0), nil));

    Assert(not M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(40, 20, 20), nil, false));
    Assert(M.SegmentCollision(Vector3Single(30, 10, 10), Vector3Single(10, -10, -10), nil, false));

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
  RegisterTest(TTestCastle3D);
end.
